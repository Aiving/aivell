use super::BytesSpan;
use std::{
    collections::HashMap,
    hash,
    ops::{Bound, RangeBounds},
    str::{self, FromStr, Utf8Error},
};

#[must_use]
pub fn concat_slice_vec(c: &[u8], done: &[u8]) -> Vec<u8> {
    let mut new_vec = c.to_vec();

    new_vec.extend(done);

    new_vec
}

pub fn convert_vec_utf8(v: &[u8]) -> Result<String, Utf8Error> {
    str::from_utf8(v).map(ToOwned::to_owned)
}

pub fn complete_byte_slice_str_from_utf8(c: BytesSpan) -> Result<&str, Utf8Error> {
    str::from_utf8(c.fragment())
}

pub fn complete_str_from_str<F: FromStr>(c: &str) -> Result<F, F::Err> {
    FromStr::from_str(c)
}

pub fn get_by_index<T>(elements: Vec<T>, index: usize, default_value: T) -> T {
    elements.into_iter().nth(index).unwrap_or(default_value)
}

pub trait VecExt<T> {
    fn next(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
    fn multipeek(&self, count: usize) -> Option<Vec<&T>>;

    fn slice<R: RangeBounds<usize>>(&self, range: R) -> Option<Vec<&T>>;

    fn includes<P>(&self, predicate: P) -> bool
    where
        P: Fn(&T) -> bool;
}

pub trait VecMapExt<K, T> {
    fn get_by_key(&self, key: &K) -> Option<&T>;
    fn get_by_key_unchecked(&self, key: &K) -> &T;
}

impl<K: PartialEq, T> VecMapExt<K, T> for Vec<(K, T)> {
    fn get_by_key(&self, key: &K) -> Option<&T> {
        self.iter()
            .find(|(element, _)| element == key)
            .map(|(_, value)| value)
    }

    fn get_by_key_unchecked(&self, key: &K) -> &T {
        self.iter()
            .find(|(element, _)| element == key)
            .map(|(_, value)| value)
            .unwrap()
    }
}

impl<T: PartialEq> VecExt<T> for Vec<T> {
    fn includes<P>(&self, predicate: P) -> bool
    where
        P: Fn(&T) -> bool,
    {
        for element in self {
            if predicate(element) {
                return true;
            }
        }

        false
    }

    fn next(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            Some(self.remove(0))
        }
    }

    fn peek(&self) -> Option<&T> {
        self.first()
    }

    fn multipeek(&self, count: usize) -> Option<Vec<&T>> {
        if self.len() < count {
            None
        } else {
            self.slice(0..=count)
        }
    }

    fn slice<R: RangeBounds<usize>>(&self, range: R) -> Option<Vec<&T>> {
        let mut elements = vec![];

        let start = match range.start_bound() {
            Bound::Included(index) => *index,
            Bound::Unbounded => 0,
            Bound::Excluded(_) => unreachable!(),
        };

        let end = match range.end_bound() {
            Bound::Included(index) => index + 1,
            Bound::Excluded(index) => *index,
            Bound::Unbounded => self.len(),
        };

        for i in start..end {
            elements.push(self.get(i)?);
        }

        Some(elements)
    }
}

pub trait MapExt<K, V> {
    fn find<P>(&self, predicate: P) -> Option<(&K, &V)>
    where
        P: Fn((&K, &V)) -> bool;

    fn find_mut<P>(&mut self, predicate: P) -> Option<(&K, &mut V)>
    where
        P: FnMut((&K, &V)) -> bool;
}

pub trait BoolExt {
    fn map<T>(&self, trueness: T, falseness: T) -> T;
}

impl BoolExt for bool {
    fn map<T>(&self, trueness: T, falseness: T) -> T {
        if self.eq(&true) {
            trueness
        } else {
            falseness
        }
    }
}

impl<K: Clone, V: Clone, S: hash::BuildHasher> MapExt<K, V> for HashMap<K, V, S> {
    fn find<P>(&self, predicate: P) -> Option<(&K, &V)>
    where
        P: Fn((&K, &V)) -> bool,
    {
        let mut result = None;

        for element in self {
            if predicate(element) {
                result = Some(element);

                break;
            }
        }

        result
    }

    fn find_mut<P>(&mut self, mut predicate: P) -> Option<(&K, &mut V)>
    where
        P: FnMut((&K, &V)) -> bool,
    {
        let mut result = None;

        for element in &mut *self {
            if predicate((element.0, element.1)) {
                result = Some(element);

                break;
            }
        }

        result
    }
}
