use crate::{
    bool::{Bool, False},
    uninhabited::PhantomUninhabited,
};

/// Represents a finite list of boolean values appended with an infinite
/// number of `False` values.
pub trait BoolList: private::Sealed {
    type Head: Bool;
    type Tail: BoolList;
}

pub enum Nil {}
impl BoolList for Nil {
    type Head = False;
    type Tail = Nil;
}

pub struct Cons<B: Bool, T: BoolList>(PhantomUninhabited<(B, T)>);
impl<B: Bool, T: BoolList> BoolList for Cons<B, T> {
    type Head = B;
    type Tail = T;
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
