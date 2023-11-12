pub trait SameType: private::Sealed {}
impl<T> SameType for (T, T) {}

mod private {
    pub trait Sealed {}
    impl<T> Sealed for (T, T) {}
}

pub const fn assert_same_type<T: SameType>() {}
