use crate::{
    bool::{BoolT, False, Not},
    ord::{Equal, Greater, Less, OrderingEq, OrderingT},
    uninhabited::PhantomUninhabited,
};

/// A natural number.
/// The only implementations are `Zero` or `Succ<T: Nat>`.
pub trait Nat: NatT + private_nat::NatSealed {
    const VALUE: usize;
}

/// A type representing a natural number.
pub trait NatT {
    type Type: Nat;
}

/// Represent the natural number `0`.
pub enum Zero {}
impl Nat for Zero {
    const VALUE: usize = 0;
}
impl NatT for Zero {
    type Type = Self;
}

/// Represent the natural number `succ(T) = T + 1`.
pub struct Succ<T: Nat>(PhantomUninhabited<T>);
impl<T: Nat> Nat for Succ<T> {
    const VALUE: usize = T::VALUE + 1;
}
impl<T: Nat> NatT for Succ<T> {
    type Type = Self;
}

mod private_nat {
    use super::*;

    pub trait NatSealed {}
    impl NatSealed for Zero {}
    impl<T: Nat> NatSealed for Succ<T> {}
}

#[doc(hidden)]
pub struct NatCmpBase<A: Nat, B: Nat>(PhantomUninhabited<(A, B)>);
impl OrderingT for NatCmpBase<Zero, Zero> {
    type Type = Equal;
}

impl<A: Nat> OrderingT for NatCmpBase<Zero, Succ<A>> {
    type Type = Less;
}

impl<A: Nat> OrderingT for NatCmpBase<Succ<A>, Zero> {
    type Type = Greater;
}

impl<A: Nat, B: Nat> OrderingT for NatCmpBase<Succ<A>, Succ<B>>
where
    NatCmpBase<A, B>: OrderingT,
{
    type Type = <NatCmpBase<A, B> as OrderingT>::Type;
}

/// Represent the comparison of `a` and `b`.
pub struct NatCmp<A: NatT, B: NatT>(PhantomUninhabited<(A, B)>);
impl<A: NatT, B: NatT> OrderingT for NatCmp<A, B>
where
    NatCmpBase<A::Type, B::Type>: OrderingT,
{
    type Type = <NatCmpBase<A::Type, B::Type> as OrderingT>::Type;
}

/// Represent the boolean value `a == b`.
pub struct NatEq<A: NatT, B: NatT>(PhantomUninhabited<(A, B)>);
impl<A: NatT, B: NatT> BoolT for NatEq<A, B>
where
    NatCmp<A, B>: OrderingT,
    OrderingEq<NatCmp<A, B>, Equal>: BoolT,
{
    type Type = <OrderingEq<NatCmp<A, B>, Equal> as BoolT>::Type;
}

/// Represent the boolean value `a != b`.
pub struct NatNe<A: NatT, B: NatT>(PhantomUninhabited<(A, B)>);
impl<A: NatT, B: NatT> BoolT for NatNe<A, B>
where
    NatEq<A, B>: BoolT,
    Not<NatEq<A, B>>: BoolT,
{
    type Type = <Not<NatEq<A, B>> as BoolT>::Type;
}

/// Represent the boolean value `a < b`.
pub struct NatLt<A: NatT, B: NatT>(PhantomUninhabited<(A, B)>);
impl<A: NatT, B: NatT> BoolT for NatLt<A, B>
where
    NatCmp<A, B>: OrderingT,
    OrderingEq<NatCmp<A, B>, Less>: BoolT,
{
    type Type = <OrderingEq<NatCmp<A, B>, Less> as BoolT>::Type;
}

/// Represent the boolean value `a > b`.
pub struct NatGt<A: NatT, B: NatT>(PhantomUninhabited<(A, B)>);
impl<A: NatT, B: NatT> BoolT for NatGt<A, B>
where
    NatCmp<A, B>: OrderingT,
    OrderingEq<NatCmp<A, B>, Greater>: BoolT,
{
    type Type = <OrderingEq<NatCmp<A, B>, Greater> as BoolT>::Type;
}

/// Represent the boolean value `a <= b`.
pub struct NatLe<A: NatT, B: NatT>(PhantomUninhabited<(A, B)>);
impl<A: NatT, B: NatT> BoolT for NatLe<A, B>
where
    NatGt<A, B>: BoolT,
    Not<NatGt<A, B>>: BoolT,
{
    type Type = <Not<NatGt<A, B>> as BoolT>::Type;
}

/// Represent the boolean value `a >= b`.
pub struct NatGe<A: NatT, B: NatT>(PhantomUninhabited<(A, B)>);
impl<A: NatT, B: NatT> BoolT for NatGe<A, B>
where
    NatLt<A, B>: BoolT,
    Not<NatLt<A, B>>: BoolT,
{
    type Type = <Not<NatLt<A, B>> as BoolT>::Type;
}

#[doc(hidden)]
pub struct PlusBase<A: Nat, B: Nat>(PhantomUninhabited<(A, B)>);
impl<A: Nat> NatT for PlusBase<A, Zero> {
    type Type = A;
}
impl<A: Nat, B: Nat> NatT for PlusBase<A, Succ<B>>
where
    PlusBase<A, B>: NatT,
{
    type Type = Succ<<PlusBase<A, B> as NatT>::Type>;
}

/// Represent the natural number `a + b`.
pub struct Plus<A: NatT, B: NatT>(PhantomUninhabited<(A, B)>);
impl<A: NatT, B: NatT> NatT for Plus<A, B>
where
    PlusBase<A::Type, B::Type>: NatT,
{
    type Type = <PlusBase<A::Type, B::Type> as NatT>::Type;
}

#[doc(hidden)]
pub struct MulBase<A: Nat, B: Nat>(PhantomUninhabited<(A, B)>);
impl<A: Nat> NatT for MulBase<A, Zero> {
    type Type = Zero;
}

impl<A: Nat, B: Nat> NatT for MulBase<A, Succ<B>>
where
    MulBase<A, B>: NatT,
    Plus<MulBase<A, B>, A>: NatT,
{
    type Type = <Plus<MulBase<A, B>, A> as NatT>::Type;
}

/// Represent the natural number `a * b`.
pub struct Mul<A: NatT, B: NatT>(PhantomUninhabited<(A, B)>);
impl<A: NatT, B: NatT> NatT for Mul<A, B>
where
    MulBase<A::Type, B::Type>: NatT,
{
    type Type = <MulBase<A::Type, B::Type> as NatT>::Type;
}

/// Only implemented for two types `A: NatT` and `B: NatT` where `A` and `B` represents the same
/// natural number.
pub trait NatIsEq: private_eq::NatIsEqSealed {}
impl<T: private_eq::NatIsEqSealed> NatIsEq for T {}

/// Only implemented for two types `A: NatT` and `B: NatT` where `A` and `B` represents different
/// natural numbers.
pub trait NatNotEq: private_eq::NatNotEqSealed {}
impl<T: private_eq::NatNotEqSealed> NatNotEq for T {}

mod private_eq {
    use super::*;
    pub trait NatIsEqSealed {}
    impl<V: Nat, A: NatT<Type = V>, B: NatT<Type = V>> NatIsEqSealed for (A, B) {}

    pub trait NatNotEqSealed {}
    impl<A, B> NatNotEqSealed for (A, B)
    where
        A: NatT,
        B: NatT,
        NatEq<A, B>: BoolT<Type = False>,
    {
    }
}

pub const fn assert_nat_eq<T: NatIsEq>() {}
pub const fn assert_nat_neq<T: NatNotEq>() {}

#[macro_export]
#[doc(hidden)]
macro_rules! __nat_reverse {
    (0) => {
        $crate::nat::consts::_0
    };
    (1) => {
        $crate::nat::consts::_1
    };
    (2) => {
        $crate::nat::consts::_2
    };
    (3) => {
        $crate::nat::consts::_3
    };
    (4) => {
        $crate::nat::consts::_4
    };
    (5) => {
        $crate::nat::consts::_5
    };
    (6) => {
        $crate::nat::consts::_6
    };
    (7) => {
        $crate::nat::consts::_7
    };
    (8) => {
        $crate::nat::consts::_8
    };
    (9) => {
        $crate::nat::consts::_9
    };
    ($d:tt, $($ds:tt),+) => {
        <$crate::nat::Plus<
            $crate::nat::Mul<
                $crate::__nat_reverse!($($ds),+),
                $crate::nat::consts::_10>,
            $crate::__nat_reverse!($d)>
        as $crate::nat::NatT>::Type
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __nat_aux {
    ([] [$($ys:tt,)*]) => {
        $crate::__nat_reverse!($($ys),*)
    };
    ([$x:tt, $($xs:tt,)*] [$($ys:tt,)*]) => {
        $crate::__nat_aux!([$($xs,)*] [$x, $($ys,)*])
    };
}

#[macro_export]
macro_rules! nat {
    ($($ds:tt),+ $(,)?) => {
        $crate::__nat_aux!([$($ds,)*] [])
    };
}

pub mod consts {
    use super::{Mul, Succ, Zero};

    pub type _0 = Zero;
    pub type _1 = Succ<_0>;
    pub type _2 = Succ<_1>;
    pub type _3 = Succ<_2>;
    pub type _4 = Succ<_3>;
    pub type _5 = Succ<_4>;
    pub type _6 = Succ<_5>;
    pub type _7 = Succ<_6>;
    pub type _8 = Succ<_7>;
    pub type _9 = Succ<_8>;
    pub type _10 = Succ<_9>;
    pub type _11 = Succ<_10>;
    pub type _12 = Succ<_11>;
    pub type _13 = Succ<_12>;
    pub type _14 = Succ<_13>;
    pub type _15 = Succ<_14>;
    pub type _16 = Succ<_15>;
    pub type _17 = Succ<_16>;
    pub type _18 = Succ<_17>;
    pub type _19 = Succ<_18>;
    pub type _20 = Succ<_19>;
    pub type _21 = Succ<_20>;
    pub type _22 = Succ<_21>;
    pub type _23 = Succ<_22>;
    pub type _24 = Succ<_23>;
    pub type _25 = Succ<_24>;
    pub type _26 = Succ<_25>;
    pub type _27 = Succ<_26>;
    pub type _28 = Succ<_27>;
    pub type _29 = Succ<_28>;
    pub type _30 = Succ<_29>;
    pub type _31 = Succ<_30>;
    pub type _32 = Succ<_31>;

    pub type _100 = Mul<_10, _10>;
    pub type _1000 = Mul<_100, _10>;
}

mod test {
    use super::{consts::*, *};
    use crate::bool::{assert_false, assert_true};

    const _: () = assert_nat_eq::<(_0, _0)>();
    const _: () = assert_nat_neq::<(_0, _1)>();
    const _: () = assert_nat_neq::<(_1, _0)>();
    const _: () = assert_nat_eq::<(_1, _1)>();

    const _: () = assert_nat_neq::<(_2, _3)>();
    const _: () = assert_nat_neq::<(_3, _2)>();
    const _: () = assert_nat_eq::<(_3, _3)>();

    const _: () = assert_nat_eq::<(Plus<_0, _0>, _0)>();
    const _: () = assert_nat_eq::<(Plus<_1, _0>, _1)>();
    const _: () = assert_nat_eq::<(Plus<_1, _1>, _2)>();

    const _: () = assert_nat_eq::<(Plus<_2, _1>, Plus<_1, _2>)>();
    const _: () = assert_nat_eq::<(Plus<Plus<_1, _1>, _1>, Plus<_1, Plus<_1, _1>>)>();

    const _: () = assert_nat_eq::<(Mul<_0, _0>, _0)>();
    const _: () = assert_nat_eq::<(Mul<_0, _10>, _0)>();
    const _: () = assert_nat_eq::<(Mul<_10, _0>, _0)>();
    const _: () = assert_nat_eq::<(Mul<_2, _5>, _10)>();
    const _: () = assert_nat_eq::<(Mul<_3, _3>, _9)>();

    const _: () = assert_false::<NatEq<_7, _8>>();
    const _: () = assert_true::<NatNe<_7, _8>>();
    const _: () = assert_true::<NatLt<_7, _8>>();
    const _: () = assert_false::<NatGt<_7, _8>>();
    const _: () = assert_true::<NatLe<_7, _8>>();
    const _: () = assert_false::<NatGe<_7, _8>>();

    const _: () = assert_true::<NatEq<_8, _8>>();
    const _: () = assert_false::<NatNe<_8, _8>>();
    const _: () = assert_false::<NatLt<_8, _8>>();
    const _: () = assert_false::<NatGt<_8, _8>>();
    const _: () = assert_true::<NatLe<_8, _8>>();
    const _: () = assert_true::<NatGe<_8, _8>>();

    const _: () = assert_false::<NatEq<_9, _8>>();
    const _: () = assert_true::<NatNe<_9, _8>>();
    const _: () = assert_false::<NatLt<_9, _8>>();
    const _: () = assert_true::<NatGt<_9, _8>>();
    const _: () = assert_false::<NatLe<_9, _8>>();
    const _: () = assert_true::<NatGe<_9, _8>>();
}
