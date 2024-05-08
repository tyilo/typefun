use crate::uninhabited::PhantomUninhabited;

/// A boolean value.
/// The only implementations are `False` or `True`.
pub trait Bool: BoolT + private_bool::Sealed {
    const VALUE: bool;
}

/// A type representing a boolean value.
pub trait BoolT {
    type Type: Bool;
}

/// Represent the boolean value `false`.
pub enum False {}
impl Bool for False {
    const VALUE: bool = false;
}
impl BoolT for False {
    type Type = Self;
}

/// Represent the boolean value `true`.
pub enum True {}
impl Bool for True {
    const VALUE: bool = true;
}
impl BoolT for True {
    type Type = Self;
}

mod private_bool {
    use super::*;

    pub trait Sealed {}
    impl Sealed for False {}
    impl Sealed for True {}
}

pub struct NandBase<A: Bool, B: Bool>(PhantomUninhabited<(A, B)>);
impl BoolT for NandBase<False, False> {
    type Type = True;
}
impl BoolT for NandBase<False, True> {
    type Type = True;
}
impl BoolT for NandBase<True, False> {
    type Type = True;
}
impl BoolT for NandBase<True, True> {
    type Type = False;
}

/// Represents `A nand B`.
pub struct Nand<A: BoolT, B: BoolT>(PhantomUninhabited<(A, B)>);
impl<A: BoolT, B: BoolT> BoolT for Nand<A, B>
where
    NandBase<A::Type, B::Type>: BoolT,
{
    type Type = <NandBase<A::Type, B::Type> as BoolT>::Type;
}

/// Represents `not A`.
pub struct Not<A: BoolT>(PhantomUninhabited<A>);
impl<A: BoolT> BoolT for Not<A>
where
    Nand<A, A>: BoolT,
{
    type Type = <Nand<A, A> as BoolT>::Type;
}

/// Represents `A and B`.
pub struct And<A: BoolT, B: BoolT>(PhantomUninhabited<(A, B)>);
impl<A: BoolT, B: BoolT> BoolT for And<A, B>
where
    Nand<A, B>: BoolT,
    Not<<Nand<A, B> as BoolT>::Type>: BoolT,
{
    type Type = <Not<<Nand<A, B> as BoolT>::Type> as BoolT>::Type;
}

/// Represents `A or B`.
pub struct Or<A: BoolT, B: BoolT>(PhantomUninhabited<(A, B)>);
impl<A: BoolT, B: BoolT> BoolT for Or<A, B>
where
    Not<A>: BoolT,
    Not<B>: BoolT,
    Nand<<Not<A> as BoolT>::Type, <Not<B> as BoolT>::Type>: BoolT,
{
    type Type = <Nand<<Not<A> as BoolT>::Type, <Not<B> as BoolT>::Type> as BoolT>::Type;
}

/// Represents `A nor B`.
pub struct Nor<A: BoolT, B: BoolT>(PhantomUninhabited<(A, B)>);
impl<A: BoolT, B: BoolT> BoolT for Nor<A, B>
where
    Or<A, B>: BoolT,
    Not<<Or<A, B> as BoolT>::Type>: BoolT,
{
    type Type = <Not<<Or<A, B> as BoolT>::Type> as BoolT>::Type;
}

/// Represents `A xnor B`.
pub struct Xnor<A: BoolT, B: BoolT>(PhantomUninhabited<(A, B)>);
impl<A: BoolT, B: BoolT> BoolT for Xnor<A, B>
where
    Nand<A, B>: BoolT,
    And<A, B>: BoolT,
    Or<<Nand<A, B> as BoolT>::Type, <And<A, B> as BoolT>::Type>: BoolT,
{
    type Type = <Or<<Nand<A, B> as BoolT>::Type, <And<A, B> as BoolT>::Type> as BoolT>::Type;
}

/// Represents `A xor B`.
pub struct Xor<A: BoolT, B: BoolT>(PhantomUninhabited<(A, B)>);
impl<A: BoolT, B: BoolT> BoolT for Xor<A, B>
where
    Xnor<A, B>: BoolT,
    Not<<Xnor<A, B> as BoolT>::Type>: BoolT,
{
    type Type = <Not<<Xnor<A, B> as BoolT>::Type> as BoolT>::Type;
}

pub type BoolEq<A, B> = Xnor<A, B>;

pub const fn assert_false<T: BoolT<Type = False>>() {}
pub const fn assert_true<T: BoolT<Type = True>>() {}

/// Only implemented for two types `A: BoolT` and `B: BoolT` where `A` and `B` represents the same
/// boolean value.
pub trait BoolIsEq: private_eq::BoolIsEqSealed {}
impl<T: private_eq::BoolIsEqSealed> BoolIsEq for T {}

/// Only implemented for two types `A: BoolT` and `B: BoolT` where `A` and `B` represents different
/// boolean values.
pub trait BoolNotEq: private_eq::BoolNotEqSealed {}
impl<T: private_eq::BoolNotEqSealed> BoolNotEq for T {}

mod private_eq {
    use super::*;
    pub trait BoolIsEqSealed {}
    impl<V: Bool, A: BoolT<Type = V>, B: BoolT<Type = V>> BoolIsEqSealed for (A, B) {}

    pub trait BoolNotEqSealed {}
    impl<A, B> BoolNotEqSealed for (A, B)
    where
        A: BoolT,
        B: BoolT,
        (A, Not<B>): BoolIsEq,
    {
    }
}

pub const fn assert_bool_eq<T1: BoolT, T2: BoolT>()
where
    (T1, T2): BoolIsEq,
{
}

mod test {
    use super::*;

    const _: () = assert_false::<False>();
    const _: () = assert_true::<True>();

    const _: () = assert_true::<Not<False>>();
    const _: () = assert_false::<Not<True>>();

    const _: () = assert_false::<And<False, False>>();
    const _: () = assert_false::<And<False, True>>();
    const _: () = assert_false::<And<True, False>>();
    const _: () = assert_true::<And<True, True>>();

    const _: () = assert_true::<Nand<False, False>>();
    const _: () = assert_true::<Nand<False, True>>();
    const _: () = assert_false::<Nand<True, True>>();
    const _: () = assert_true::<Nand<True, False>>();

    const _: () = assert_false::<Or<False, False>>();
    const _: () = assert_true::<Or<False, True>>();
    const _: () = assert_true::<Or<True, False>>();
    const _: () = assert_true::<Or<True, True>>();

    const _: () = assert_true::<Nor<False, False>>();
    const _: () = assert_false::<Nor<False, True>>();
    const _: () = assert_false::<Nor<True, False>>();
    const _: () = assert_false::<Nor<True, True>>();
}
