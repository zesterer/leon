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

pub struct Heap<T> {
    last_sweep: usize,
    item_sweeps: HashMap<*mut T, usize>,
    items: HashSet<*mut T>,
    rooted: HashSet<(Rc<()>, *mut T)>,
}

impl<T> Default for Heap<T> {
    fn default() -> Self {
        Self {
            last_sweep: 0,
            item_sweeps: HashMap::default(),
            items: HashSet::default(),
            rooted: HashSet::default(),
        }
    }
}

impl<T: Trace> Heap<T> {
    /// Insert a new item that will be cleared upon the next garbage collection, if not attached
    /// to the item tree.
    pub fn insert_temp(&mut self, item: T) -> Handle<T> {
        let ptr = Box::into_raw(Box::new(item));

        self.item_sweeps.insert(ptr, self.last_sweep);
        self.items.insert(ptr);

        Handle { ptr }
    }

    /// Insert a new item that will not be cleared by garbage collection until dropped.
    pub fn insert(&mut self, item: T) -> Rooted<T> {
        let handle = self.insert_temp(item);

        let rc = Rc::new(());
        self.rooted.insert((rc.clone(), handle.ptr));

        Rooted {
            rc,
            handle,
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn contains(&self, handle: impl AsRef<Handle<T>>) -> bool {
        let handle = handle.as_ref();
        self.items.contains(&handle.ptr)
    }

    // We can hand out immutable references as much as we like
    pub fn get(&self, handle: impl AsRef<Handle<T>>) -> Option<&T> {
        let handle = handle.as_ref();
        if self.contains(handle) {
            Some(unsafe { &*handle.ptr })
        } else {
            None
        }
    }

    // Undefined behaviour occurs when the handle does not originating in this heap.
    pub fn get_unchecked(&self, handle: impl AsRef<Handle<T>>) -> &T {
        let handle = handle.as_ref();
        debug_assert!(self.contains(handle));
        unsafe { &*handle.ptr }
    }

    // Because it's impossible to mutably (or immutably) access this `GcHeap` from within the
    // closure, it's safe for this to mutate a single item since accessing the `GcHeap` is the
    // only way to turn a handle to an item into an actual reference.
    pub fn mutate<R, F: FnOnce(&mut T) -> R>(&mut self, handle: impl AsRef<Handle<T>>, f: F) -> Option<R> {
        let handle = handle.as_ref();
        if self.contains(handle) {
            Some(f(unsafe { &mut *handle.ptr }))
        } else {
            None
        }
    }

    // Undefined behaviour occurs when the handle does not originating in this heap.
    pub fn mutate_unchecked<R, F: FnOnce(&mut T) -> R>(&mut self, handle: impl AsRef<Handle<T>>, f: F) -> R {
        let handle = handle.as_ref();
        debug_assert!(self.contains(handle));
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
                    tracer.mark(Handle { ptr: *ptr });
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

impl<T> AsRef<Handle<T>> for Handle<T> {
    fn as_ref(&self) -> &Handle<T> {
        self
    }
}

#[derive(Clone)]
pub struct Rooted<T> {
    rc: Rc<()>,
    handle: Handle<T>,
}

impl<T> AsRef<Handle<T>> for Rooted<T> {
    fn as_ref(&self) -> &Handle<T> {
        &self.handle
    }
}

impl<T> Rooted<T> {
    pub fn into_handle(self) -> Handle<T> {
        self.handle
    }

    pub fn handle(&self) -> Handle<T> {
        self.handle
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    enum Value {
        Base,
        Refs(Handle<Value>, Handle<Value>),
    }

    impl Trace for Value {
        fn trace(&self, tracer: &mut Tracer<Self>) {
            match self {
                Value::Base => {},
                Value::Refs(a, b) => {
                    tracer.mark(*a);
                    tracer.mark(*b);
                },
            }
        }
    }

    #[test]
    fn basic() {
        let mut heap = Heap::default();

        let a = heap.insert(Value::Base);

        heap.clean();

        assert_eq!(heap.contains(&a), true);

        let a = a.into_handle();

        heap.clean();

        assert_eq!(heap.contains(&a), false);
    }

    #[test]
    fn ownership() {
        let mut heap = Heap::default();

        let a = heap.insert(Value::Base).handle();
        let b = heap.insert(Value::Base).handle();
        let c = heap.insert(Value::Base).handle();
        let d = heap.insert(Value::Refs(a, c));
        let e = heap.insert(Value::Base).handle();

        heap.clean();

        assert_eq!(heap.contains(&a), true);
        assert_eq!(heap.contains(&b), false);
        assert_eq!(heap.contains(&c), true);
        assert_eq!(heap.contains(&d), true);
        assert_eq!(heap.contains(&e), false);
    }

    #[test]
    fn recursive() {
        let mut heap = Heap::default();

        let a = heap.insert(Value::Base);
        let b = heap.insert(Value::Base);

        heap.mutate(&a, |a_val| *a_val = Value::Refs(a.handle(), b.handle()));

        heap.clean();

        assert_eq!(heap.contains(&a), true);
        assert_eq!(heap.contains(&b), true);

        let a = a.into_handle();

        heap.clean();

        assert_eq!(heap.contains(&a), false);
        assert_eq!(heap.contains(&b), true);
    }
}
