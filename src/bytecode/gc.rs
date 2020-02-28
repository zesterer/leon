use std::{
    rc::Rc,
    collections::{HashMap, HashSet},
};

pub trait Trace: Sized {
    fn trace(&self, tracer: &mut Tracer<Self>);
}

pub struct Tracer<'a, T: Trace> {
    new_sweep: usize,
    item_sweeps: &'a mut HashMap<*mut T, usize>,
    items: &'a HashSet<*mut T>,
}

impl<'a, T: Trace> Tracer<'a, T> {
    pub fn mark(&mut self, handle: Handle<T>) {
        let sweep = self.item_sweeps.get_mut(&handle.ptr).unwrap();
        if *sweep != self.new_sweep {
            *sweep = self.new_sweep;
            unsafe { &*handle.ptr }.trace(self);
        }
    }
}

#[derive(Default)]
pub struct Heap<T> {
    last_sweep: usize,
    item_sweeps: HashMap<*mut T, usize>,
    items: HashSet<*mut T>,
    rooted: HashSet<(Rc<()>, *mut T)>,
}

impl<T: Trace> Heap<T> {
    pub fn insert(&mut self, item: T) -> Rooted<T> {
        let ptr = Box::into_raw(Box::new(item));

        self.item_sweeps.insert(ptr, self.last_sweep);
        self.items.insert(ptr);

        let rc = Rc::new(());
        self.rooted.insert((rc.clone(), ptr));

        Rooted {
            rc,
            handle: Handle { ptr },
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    // We can hand out immutable references as much as we like
    pub fn get(&self, handle: Handle<T>) -> Option<&T> {
        assert!(self.items.contains(&handle.ptr));
        if self.items.contains(&handle.ptr) {
            Some(unsafe { &*handle.ptr })
        } else {
            None
        }
    }

    // Undefined behaviour occurs when the handle does not originating in this heap.
    pub fn get_unchecked(&self, handle: Handle<T>) -> &T {
        debug_assert!(self.items.contains(&handle.ptr));
        unsafe { &*handle.ptr }
    }

    // Because it's impossible to mutably (or immutably) access this `GcHeap` from within the
    // closure, it's safe for this to mutate a single item since accessing the `GcHeap` is the
    // only way to turn a handle to an item into an actual reference.
    pub fn mutate<R, F: FnOnce(&mut T) -> R>(&mut self, handle: Handle<T>, f: F) -> Option<R> {
        if self.items.contains(&handle.ptr) {
            Some(f(unsafe { &mut *handle.ptr }))
        } else {
            None
        }
    }

    // Undefined behaviour occurs when the handle does not originating in this heap.
    pub fn mutate_unchecked<R, F: FnOnce(&mut T) -> R>(&mut self, handle: Handle<T>, f: F) -> R {
        debug_assert!(self.items.contains(&handle.ptr));
        f(unsafe { &mut *handle.ptr })
    }

    pub fn clean(&mut self) {
        let new_sweep = self.last_sweep + 1;
        let mut tracer = Tracer {
            new_sweep,
            item_sweeps: &mut self.item_sweeps,
            items: &self.items,
        };

        // Mark
        self.rooted
            .retain(|(rc, ptr)| {
                if Rc::strong_count(rc) > 1 {
                    unsafe { &**ptr }.trace(&mut tracer);
                    true
                } else {
                    false
                }
            });

        // Sweep
        let items = &mut self.items;
        self.item_sweeps
            .retain(|ptr, sweep| {
                if *sweep == new_sweep {
                    true
                } else {
                    items.remove(ptr);
                    drop(unsafe { Box::from_raw(*ptr) });
                    false
                }
            });

        self.last_sweep = new_sweep;
    }
}

impl<T> Drop for Heap<T> {
    fn drop(&mut self) {
        for ptr in &self.items {
            drop(unsafe { Box::from_raw(*ptr) });
        }
    }
}

pub struct Handle<T> {
    ptr: *mut T,
}

impl<T> Copy for Handle<T> {}
impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self { ptr: self.ptr }
    }
}

#[derive(Clone)]
pub struct Rooted<T> {
    rc: Rc<()>,
    handle: Handle<T>,
}

impl<T> Rooted<T> {
    pub fn handle(&self) -> Handle<T> {
        self.handle
    }
}
