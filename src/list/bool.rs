use core::marker::PhantomData;

use crate::{
    bool::{Bool, False, True},
    nat::{Nat, Succ, Zero},
};

/// Represents a list of boolean values. It represents a finite with appended with an infinity
/// number of `False` values.
pub trait BoolList: private::Sealed {
    type Head: Bool;
    type Tail: BoolList;
    type Sum: Nat;
}

pub enum Nil {}
impl BoolList for Nil {
    type Head = False;
    type Tail = Nil;
    type Sum = Zero;
}

pub struct Cons<B: Bool, T: BoolList>(PhantomData<(B, T)>);
impl<T: BoolList> BoolList for Cons<False, T> {
    type Head = False;
    type Tail = T;
    type Sum = T::Sum;
}
impl<T: BoolList> BoolList for Cons<True, T> {
    type Head = True;
    type Tail = T;
    type Sum = Succ<T::Sum>;
}

mod private {
    use super::*;

    pub trait Sealed {}
    impl Sealed for Nil {}
    impl<B: Bool, T: BoolList> Sealed for Cons<B, T> {}
}

#[macro_export]
macro_rules! to_bool_list {
    () => {
        $crate::list::bool::Nil
    };
    (0) => {
        $crate::list::bool::Cons<$crate::bool::False, $crate::list::bool::Nil>
    };
    (1) => {
        $crate::list::bool::Cons<$crate::bool::True, $crate::list::bool::Nil>
    };
    (0, $($xs:tt),+) => {
        $crate::list::bool::Cons<$crate::bool::False, $crate::to_bool_list!($($xs),+)>
    };
    (1, $($xs:tt),+) => {
        $crate::list::bool::Cons<$crate::bool::True, $crate::to_bool_list!($($xs),+)>
    };
}
