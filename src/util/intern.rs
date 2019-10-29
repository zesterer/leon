use std::{cmp::PartialEq, collections::HashMap, marker::PhantomData};

#[cfg_attr(test, derive(PartialEq, Debug))]
pub struct Interned<T>(usize, PhantomData<T>);
impl<T> Copy for Interned<T> {}
impl<T> Clone for Interned<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

#[derive(Default)]
#[cfg_attr(test, derive(PartialEq, Debug))]
pub struct InternTable<T: Eq> {
    items: Vec<T>,
}

impl<T: Eq> InternTable<T> {
    pub fn get(&self, key: Interned<T>) -> &T {
        self.items.get(key.0).expect("Cannot locate interned item")
    }

    pub fn intern(&mut self, item: impl Into<T>) -> Interned<T> {
        let item = item.into();

        let idx = self
            .items
            .iter()
            .enumerate()
            .find(|(_, x)| x == &&item)
            .map(|(i, _)| i);

        if let Some(idx) = idx {
            Interned(idx, PhantomData)
        } else {
            self.items.push(item.into());
            Interned(self.items.len() - 1, PhantomData)
        }
    }
}
