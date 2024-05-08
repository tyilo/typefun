use core::marker::PhantomData;

use crate::{
    nat,
    nat::{Nat, NatT},
};

/// An integer.
pub trait Int: IntT + private_int::IntSealed {
    const VALUE: isize;
}

/// A type representing an integer
pub trait IntT {
    type Type: Int;
}

/// Represent the integer `0`.
pub enum Zero {}
impl Int for Zero {
    const VALUE: isize = 0;
}
impl IntT for Zero {
    type Type = Self;
}

/// Represent the positive number `T + 1`.
pub struct Pos<T: Nat>(PhantomData<T>);
impl<T: Nat> Int for Pos<T> {
    const VALUE: isize = T::VALUE as isize + 1;
}
impl<T: Nat> IntT for Pos<T> {
    type Type = Self;
}

/// Represent the negative number `-T - 1`.
pub struct Neg<T: Nat>(PhantomData<T>);
impl<T: Nat> Int for Neg<T> {
    const VALUE: isize = -(T::VALUE as isize) - 1;
}
impl<T: Nat> IntT for Neg<T> {
    type Type = Self;
}

mod private_int {
    use super::*;

    pub trait IntSealed {}
    impl IntSealed for Zero {}
    impl<T: Nat> IntSealed for Pos<T> {}
    impl<T: Nat> IntSealed for Neg<T> {}
}

trait NatToInt {
    type Type: Int;
}

impl NatToInt for nat::Zero {
    type Type = Zero;
}

impl<T: Nat> NatToInt for nat::Succ<T> {
    type Type = Pos<T>;
}

pub struct Succ<A: Int>(PhantomData<A>);
impl<A: Nat> IntT for Succ<Neg<nat::Succ<A>>> {
    type Type = Neg<A>;
}
impl IntT for Succ<Neg<nat::Zero>> {
    type Type = Zero;
}
impl IntT for Succ<Zero> {
    type Type = Pos<nat::Zero>;
}
impl<A: Nat> IntT for Succ<Pos<A>> {
    type Type = Pos<nat::Succ<A>>;
}

pub struct Pred<A: Int>(PhantomData<A>);
impl<A: Nat> IntT for Pred<Neg<A>> {
    type Type = Neg<nat::Succ<A>>;
}
impl IntT for Pred<Zero> {
    type Type = Neg<nat::Zero>;
}
impl IntT for Pred<Pos<nat::Zero>> {
    type Type = Zero;
}
impl<A: Nat> IntT for Pred<Pos<nat::Succ<A>>> {
    type Type = Pos<A>;
}

#[doc(hidden)]
pub struct PlusNat<A: Int, B: Nat>(PhantomData<(A, B)>);
impl<A: Int> IntT for PlusNat<A, nat::Zero> {
    type Type = A;
}
impl<B: Nat> IntT for PlusNat<Zero, nat::Succ<B>> {
    type Type = Pos<B>;
}
impl<A: Nat, B: Nat> IntT for PlusNat<Pos<A>, nat::Succ<B>>
where
    nat::PlusBase<A, B>: NatT,
{
    type Type = Pos<Succ<<nat::PlusBase<A, B> as NatT>::Type>>;
}
impl IntT for PlusNat<Neg<nat::Zero>, nat::Succ<nat::Zero>> {
    type Type = Zero;
}
impl<B: Nat> IntT for PlusNat<Neg<nat::Zero>, nat::Succ<nat::Succ<B>>> {
    type Type = Pos<B>;
}
/*
impl<A: Nat> IntT for PlusNat<Neg<nat::Succ<A>>, nat::Succ<nat::Zero>>
{
    type Type = Neg<A>;
}
*/
// -(A + 2) + B + 2 = -(A + 1) + B + 1
impl<A: Nat, B: Nat> IntT for PlusNat<Neg<nat::Succ<A>>, nat::Succ<B>>
where
    PlusNat<Neg<A>, B>: IntT,
{
    type Type = <PlusNat<Neg<A>, B> as IntT>::Type;
}

/*
#[doc(hidden)]
pub struct PlusBase<A: Int, B: Int>(PhantomData<(A, B)>);
impl<A: Int> IntT for PlusBase<A, Zero> {
    type Type = A;
}
impl<A: Int, B: Nat> IntT for PlusBase<A, Pos<B>>
where
    PlusBase<A, Pos<B>>: IntT,
    Pred<Pos<B>>: IntT,
    PlusBase<A, <Pred<Pos<B>> as IntT>::Type>: IntT,
{
    type Type = Succ<<PlusBase<A, <Pred<Pos<B>> as IntT>::Type> as IntT>::Type>;
}
*/

mod test {
    use super::*;
    use crate::types::assert_same_type;

    const _: () = assert_same_type::<(<nat::Zero as NatToInt>::Type, Zero)>();
    const _: () = assert_same_type::<(<nat::consts::_1 as NatToInt>::Type, Pos<nat::Zero>)>();
    const _: () = assert_same_type::<(<nat::consts::_2 as NatToInt>::Type, Pos<nat::consts::_1>)>();
}
