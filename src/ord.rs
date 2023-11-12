use core::{cmp::Ordering as StdOrdering, marker::PhantomData};

use crate::bool::{BoolT, False, True};

/// The result of comparing two values.
pub trait Ordering: OrderingT + private_ord::Sealed {
    const VALUE: StdOrdering;
}

/// A type representing the comparision of two values.
pub trait OrderingT {
    type Type: Ordering;
}

/// Represents an ordering where a compared value is less than another.
pub enum Less {}
impl Ordering for Less {
    const VALUE: StdOrdering = StdOrdering::Less;
}
impl OrderingT for Less {
    type Type = Self;
}

/// Represents an ordering where a compared value is equal to another.
pub enum Equal {}
impl Ordering for Equal {
    const VALUE: StdOrdering = StdOrdering::Equal;
}
impl OrderingT for Equal {
    type Type = Self;
}

/// Represents an ordering where a compared value is greater than another.
pub enum Greater {}
impl Ordering for Greater {
    const VALUE: StdOrdering = StdOrdering::Greater;
}
impl OrderingT for Greater {
    type Type = Self;
}

/// Reprensents the boolean value `a == b`.
pub struct OrderingEqBase<A: Ordering, B: Ordering>(PhantomData<(A, B)>);
impl BoolT for OrderingEqBase<Less, Less> {
    type Type = True;
}
impl BoolT for OrderingEqBase<Equal, Equal> {
    type Type = True;
}
impl BoolT for OrderingEqBase<Greater, Greater> {
    type Type = True;
}
impl BoolT for OrderingEqBase<Less, Equal> {
    type Type = False;
}
impl BoolT for OrderingEqBase<Less, Greater> {
    type Type = False;
}
impl BoolT for OrderingEqBase<Equal, Less> {
    type Type = False;
}
impl BoolT for OrderingEqBase<Equal, Greater> {
    type Type = False;
}
impl BoolT for OrderingEqBase<Greater, Less> {
    type Type = False;
}
impl BoolT for OrderingEqBase<Greater, Equal> {
    type Type = False;
}

pub struct OrderingEq<A: OrderingT, B: OrderingT>(PhantomData<(A, B)>);
impl<A: OrderingT, B: OrderingT> BoolT for OrderingEq<A, B>
where
    OrderingEqBase<A::Type, B::Type>: BoolT,
{
    type Type = <OrderingEqBase<A::Type, B::Type> as BoolT>::Type;
}

mod private_ord {
    use super::*;

    pub trait Sealed {}
    impl Sealed for Less {}
    impl Sealed for Equal {}
    impl Sealed for Greater {}
}
