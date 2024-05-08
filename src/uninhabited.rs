use core::marker::PhantomData;

enum Uninhabited {}
pub(crate) struct PhantomUninhabited<T>(Uninhabited, PhantomData<T>);
