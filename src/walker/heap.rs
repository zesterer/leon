use std::cell::Cell;

const BIN_LEN_LOG2: usize = 8;

#[derive(Copy, Clone)]
pub struct HeapRef(u32);

impl HeapRef {
    fn new(bin: usize, offset: usize) -> Self {
        Self((bin << BIN_LEN_LOG2 | offset) as u32)
    }

    fn bin(&self) -> usize {
        (self.0 as usize) >> BIN_LEN_LOG2
    }

    fn offset(&self) -> usize {
        (self.0 as usize) & (1 << BIN_LEN_LOG2 - 1)
    }
}

pub trait HeapValue {
    type ChildIter: Iterator<Item=HeapRef>;

    fn children(&self) -> Self::ChildIter;
}

struct Bin<T> {
    access: Cell<u8>,
    items: Vec<T>,
}

impl<T> Bin<T> {
    fn has_space(&self) -> bool {
        self.items.len() < (1 << BIN_LEN_LOG2)
    }

    fn insert(&mut self, item: T) -> usize {
        let idx = self.items.len();
        self.items.push(item);
        idx
    }
}

pub struct Heap<T> {
    bins: Vec<Bin<T>>,
    last_access: u8,
}

impl<T: HeapValue> Heap<T> {
    pub fn new() -> Self {
        Self {
            bins: Vec::with_capacity(16),
            last_access: 0,
        }
    }

    fn find_space_mut(&mut self) -> Option<usize> {
        if let Some((bin, _)) = self.bins
            .iter_mut()
            .enumerate()
            .find(|(_, bin)| bin.has_space())
        {
            Some(bin)
        } else {
            None
        }
    }

    fn make_space_mut(&mut self) -> (usize, &mut Bin<T>) {
        if let Some(bin) = self.find_space_mut() {
            return (bin, &mut self.bins[bin]);
        }

        self.bins.push(Bin {
            access: Cell::new(self.last_access),
            items: Vec::with_capacity(1 << BIN_LEN_LOG2),
        });
        (self.bins.len() - 1, self.bins.last_mut().unwrap())
    }

    pub fn insert(&mut self, item: T) -> HeapRef {
        let (bin_idx, bin) = self.make_space_mut();
        let offset = bin.insert(item);
        HeapRef::new(bin_idx, offset)
    }

    pub fn get(&self, r: HeapRef) -> &T {
        &self.bins[r.bin()].items[r.offset()]
    }

    pub fn get_mut(&mut self, r: HeapRef) -> &mut T {
        &mut self.bins[r.bin()].items[r.offset()]
    }

    pub fn clean(&mut self, roots: impl Iterator<Item=HeapRef>) {
        fn mark<T: HeapValue>(heap: &Heap<T>, r: HeapRef, access: u8) {
            if heap.bins[r.bin()].access.get() != access {
                heap.bins[r.bin()].access.set(access);
                for child in heap.get(r).children() {
                    mark(heap, child, access);
                }
            }
        }

        let new_access = if self.last_access == 0 { 1 } else { 0 };

        for root in roots {
            mark(self, root, new_access);
        }

        // Sweep
        for bin in &mut self.bins {
            if bin.access.get() != new_access {
                bin.access.set(new_access);
                bin.items.clear();
            }
        }
    }

    pub fn stats(&self) -> Stats {
        Stats {
            total_bins: self.bins.len(),
            empty_bins: self.bins
                .iter()
                .filter(|bin| bin.items.len() == 0)
                .count(),
            full_bins: self.bins
                .iter()
                .filter(|bin| !bin.has_space())
                .count(),
            total_values: self.bins
                .iter()
                .map(|bin| bin.items.len())
                .sum(),
            capacity: self.bins.len() * (1 << BIN_LEN_LOG2),
        }
    }
}

pub struct Stats {
    pub total_bins: usize,
    pub empty_bins: usize,
    pub full_bins: usize,
    pub total_values: usize,
    pub capacity: usize,
}
