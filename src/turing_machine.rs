//! Simulate Turing machines on Rust's type system.
//!
//! Example:
//! ```
//! use typefun::{to_bool_list, turing_machine};
//! use typefun::turing_machine::{HaltConfiguration, Run, RunOnBlank};
//! use typefun::bool::True;
//! use typefun::nat::consts::_6;
//! use typefun::types::assert_same_type;
//! // Two-state busy beaver
//!
//! // Declare the states and the transitions:
//! turing_machine! {
//!     TM;
//!
//!     [A B];
//!
//!     (0, A) => (1, R, B);
//!     (1, A) => (1, L, B);
//!
//!     (0, B) => (1, L, A);
//!     (1, B) => (1, R, Z);
//! }
//!
//! #[allow(dead_code)]
//! type Result = RunOnBlank<A>;
//!
//! const _: () = assert_same_type::<(
//!     <Result as Run<TM>>::FinalConfiguration,
//!     HaltConfiguration<
//!         to_bool_list!(1, 1),
//!         True,
//!         to_bool_list!(1),
//!     >,
//! )>();
//!
//! const _: () = assert_same_type::<(<Result as Run<TM>>::Steps, _6)>();
//! ```
//!
//! Expanding the macros this desugars to:
//! ```
//! use typefun::bool::{False, True};
//! use typefun::turing_machine::{Run, TuringMachine, Step, State, NonHaltState, NonHaltConfiguration, NonHaltStep, HaltStep, WriteAndRight, WriteAndLeft, RunOnBlank, HaltConfiguration};
//! use typefun::list::bool::{BoolList, Cons, Nil};
//! use typefun::nat::consts::_6;
//! use typefun::types::assert_same_type;
//!
//! struct TM;
//! impl TuringMachine for TM {}
//!
//! struct A;
//! impl State for A {}
//! impl NonHaltState for A {}
//!
//! struct B;
//! impl State for B {}
//! impl NonHaltState for B {}
//!
//! impl<Left: BoolList, Right: BoolList> Step<TM> for NonHaltConfiguration<Left, False, Right, A> {
//!     #[allow(unused_parens)]
//!     type Next = NonHaltStep<WriteAndRight<Left, Right, True>, B>;
//! }
//!
//! impl<Left: BoolList, Right: BoolList> Step<TM> for NonHaltConfiguration<Left, True, Right, A> {
//!     #[allow(unused_parens)]
//!     type Next = NonHaltStep<WriteAndLeft<Left, Right, True>, B>;
//! }
//!
//! impl<Left: BoolList, Right: BoolList> Step<TM> for NonHaltConfiguration<Left, False, Right, B> {
//!     #[allow(unused_parens)]
//!     type Next = NonHaltStep<WriteAndLeft<Left, Right, True>, A>;
//! }
//!
//! impl<Left: BoolList, Right: BoolList> Step<TM> for NonHaltConfiguration<Left, True, Right, B> {
//!     #[allow(unused_parens)]
//!     type Next = HaltStep<WriteAndRight<Left, Right, True>>;
//! }
//!
//! #[allow(dead_code)]
//! type Result = RunOnBlank<A>;
//!
//! const _: () = assert_same_type::<(
//!     <Result as Run<TM>>::FinalConfiguration,
//!     HaltConfiguration<
//!         Cons<True, Cons<True, Nil>>,
//!         True,
//!         Cons<True, Nil>,
//!     >,
//! )>();
//!
//! const _: () = assert_same_type::<(<Result as Run<TM>>::Steps, _6)>();
//! ```

use core::marker::PhantomData;

use crate::{
    bool::{Bool, False, True},
    list::bool::{BoolList, Cons, Nil},
    nat::{Nat, Succ, Zero},
    types::assert_same_type,
};

pub struct Tape<Left: BoolList, Head: Bool, Right: BoolList>(PhantomData<(Left, Head, Right)>);
pub trait TapeT: private::Sealed {
    type Left: BoolList;
    type Head: Bool;
    type Right: BoolList;
}
impl<Left: BoolList, Head: Bool, Right: BoolList> TapeT for Tape<Left, Head, Right> {
    type Left = Left;
    type Head = Head;
    type Right = Right;
}

mod private {
    use super::*;
    pub trait Sealed {}
    impl<Left: BoolList, Head: Bool, Right: BoolList> Sealed for Tape<Left, Head, Right> {}
}

pub type BlankTape = Tape<Nil, False, Nil>;

pub type WriteAndLeft<Left, Right, Write> =
    Tape<<Left as BoolList>::Tail, <Left as BoolList>::Head, Cons<Write, Right>>;
pub type WriteAndRight<Left, Right, Write> =
    Tape<Cons<Write, Left>, <Right as BoolList>::Head, <Right as BoolList>::Tail>;

/// A Turing machine.
pub trait TuringMachine {}

/// A Turing machine state.
pub trait State {}

/// The halting state.
pub enum Halt {}
impl State for Halt {}

/// A configuration of a Turing machine.
pub trait Configuration {
    type Tape;
    type State: State;
}

pub trait Step<TM: TuringMachine> {
    type Next: Configuration;
}

pub trait Run<TM: TuringMachine> {
    type FinalConfiguration: Configuration;
    type Steps: Nat;
}

pub struct HaltConfiguration<Left: BoolList, Head: Bool, Right: BoolList>(
    PhantomData<(Left, Head, Right)>,
);
impl<Left: BoolList, Head: Bool, Right: BoolList> Configuration
    for HaltConfiguration<Left, Head, Right>
{
    type Tape = Tape<Left, Head, Right>;
    type State = Halt;
}

impl<TM: TuringMachine, Left: BoolList, Head: Bool, Right: BoolList> Run<TM>
    for HaltConfiguration<Left, Head, Right>
{
    type FinalConfiguration = Self;
    type Steps = Zero;
}

pub trait NonHaltState: State {}

pub struct NonHaltConfiguration<Left: BoolList, Head: Bool, Right: BoolList, State: NonHaltState>(
    PhantomData<(Left, Head, Right, State)>,
);
impl<Left: BoolList, Head: Bool, Right: BoolList, State: NonHaltState> Configuration
    for NonHaltConfiguration<Left, Head, Right, State>
{
    type Tape = Tape<Left, Head, Right>;
    type State = State;
}

impl<TM: TuringMachine, Left: BoolList, Head: Bool, Right: BoolList, State: NonHaltState> Run<TM>
    for NonHaltConfiguration<Left, Head, Right, State>
where
    NonHaltConfiguration<Left, Head, Right, State>: Step<TM>,
    <NonHaltConfiguration<Left, Head, Right, State> as Step<TM>>::Next: Run<TM>,
{
    type FinalConfiguration =
        <<NonHaltConfiguration<Left, Head, Right, State> as Step<TM>>::Next as Run<TM>>::FinalConfiguration;
    type Steps = Succ<
        <<NonHaltConfiguration<Left, Head, Right, State> as Step<TM>>::Next as Run<TM>>::Steps,
    >;
}

pub type RunOn<State, Tape> = NonHaltConfiguration<
    <Tape as TapeT>::Left,
    <Tape as TapeT>::Head,
    <Tape as TapeT>::Right,
    State,
>;
pub type RunOnBlank<State> = RunOn<State, BlankTape>;

pub type HaltStep<Tape> =
    HaltConfiguration<<Tape as TapeT>::Left, <Tape as TapeT>::Head, <Tape as TapeT>::Right>;
pub type NonHaltStep<Tape, State> = NonHaltConfiguration<
    <Tape as TapeT>::Left,
    <Tape as TapeT>::Head,
    <Tape as TapeT>::Right,
    State,
>;

/// Macros that make it easier to work with
mod macros {
    #[macro_export]
    macro_rules! state {
        ($name:ident) => {
            enum $name {}
            impl $crate::turing_machine::State for $name {}
            impl $crate::turing_machine::NonHaltState for $name {}
        };
    }

    #[macro_export]
    #[doc(hidden)]
    macro_rules! __transition_aux {
        ($tm:ty: ($head:ty, $state:ty) => $next:tt) => {
            impl<Left: $crate::list::bool::BoolList, Right: $crate::list::bool::BoolList>
                $crate::turing_machine::Step<$tm>
                for $crate::turing_machine::NonHaltConfiguration<Left, $head, Right, $state>
            {
                #[allow(unused_parens)]
                type Next = $next;
            }
        };
    }

    #[macro_export]
    #[doc(hidden)]
    macro_rules! __transition_non_halt_aux {
        ($tm:ty: ($head:ty, $state:ty) => ($write:ty, $move:ident, $new_state:ty)) => {
            $crate::__transition_aux!($tm: ($head, $state) =>
                ($crate::turing_machine::NonHaltStep<
                    $crate::turing_machine::$move<Left, Right, $write>,
                    $new_state,
                >));
        };
    }

    #[macro_export]
    #[doc(hidden)]
    macro_rules! __transition_halt_aux {
        ($tm:ty: ($head:ty, $state:ty) => ($write:ty, $move:ident)) => {
            $crate::__transition_aux!($tm: ($head, $state) =>
                ($crate::turing_machine::HaltStep<
                    $crate::turing_machine::$move<Left, Right, $write>,
                >));
        };
    }

    #[macro_export]
    #[doc(hidden)]
    macro_rules! __to_bool {
        (0) => {
            $crate::bool::False
        };
        (1) => {
            $crate::bool::True
        };
    }

    #[macro_export]
    macro_rules! transition {
        ($tm:ty: ($head:tt, $state:ty) => ($write:tt, R, Z)) => {
            $crate::__transition_halt_aux!($tm: ($crate::__to_bool!($head), $state) => ($crate::__to_bool!($write), WriteAndRight));
        };
        ($tm:ty: ($head:tt, $state:ty) => ($write:tt, L, Z)) => {
            $crate::__transition_halt_aux!($tm: ($crate::__to_bool!($head), $state) => ($crate::__to_bool!($write), WriteAndLeft));
        };
        ($tm:ty: ($head:tt, $state:ty) => ($write:tt, R, $new_state:ty)) => {
            $crate::__transition_non_halt_aux!($tm: ($crate::__to_bool!($head), $state) => ($crate::__to_bool!($write), WriteAndRight, $new_state));
        };
        ($tm:ty: ($head:tt, $state:ty) => ($write:tt, L, $new_state:ty)) => {
            $crate::__transition_non_halt_aux!($tm: ($crate::__to_bool!($head), $state) => ($crate::__to_bool!($write), WriteAndLeft, $new_state));
        };
    }

    #[macro_export]
    macro_rules! turing_machine {
        ($tm:ident; [];) => {
            struct $tm;
            impl $crate::turing_machine::TuringMachine for $tm {}
        };
        ($tm:ident; []; $from:tt => $to:tt; $($froms:tt => $tos:tt;)*) => {
            $crate::transition!($tm: $from => $to);
            $crate::turing_machine! {
                $tm;
                [];
                $($froms => $tos;)*
            }
        };
        ($tm:ident; [$state:ident $($states:ident)*]; $($froms:tt => $tos:tt;)*) => {
            $crate::state!($state);
            $crate::turing_machine! {
                $tm;
                [$($states)*];
                $($froms => $tos;)*
            }
        };
    }
}

mod test {
    use super::*;
    use crate::{nat, nat::consts::*, to_bool_list, turing_machine};

    mod one_state_busy_beaver {
        use super::*;

        turing_machine! {
            TM;

            [A];

            (0, A) => (1, R, Z);
        }

        #[allow(dead_code)]
        type Result = RunOnBlank<A>;

        const _: () = assert_same_type::<(
            <Result as Run<TM>>::FinalConfiguration,
            HaltConfiguration<Cons<True, Nil>, False, Nil>,
        )>();

        const _: () = assert_same_type::<(<Result as Run<TM>>::Steps, _1)>();
    }

    mod two_state_busy_beaver {
        use super::*;

        turing_machine! {
            TM;

            [A B];

            (0, A) => (1, R, B);
            (1, A) => (1, L, B);

            (0, B) => (1, L, A);
            (1, B) => (1, R, Z);
        }

        #[allow(dead_code)]
        type Result = RunOnBlank<A>;

        const _: () = assert_same_type::<(
            <Result as Run<TM>>::FinalConfiguration,
            HaltConfiguration<to_bool_list!(1, 1), True, to_bool_list!(1)>,
        )>();

        const _: () = assert_same_type::<(<Result as Run<TM>>::Steps, _6)>();
    }

    mod three_state_busy_beaver {
        use super::*;

        turing_machine! {
            TM;

            [A B C];

            (0, A) => (1, R, B);
            (1, A) => (1, R, Z);

            (0, B) => (0, R, C);
            (1, B) => (1, R, B);

            (0, C) => (1, L, C);
            (1, C) => (1, L, A);
        }

        #[allow(dead_code)]
        type Result = RunOnBlank<A>;

        const _: () = assert_same_type::<(
            <Result as Run<TM>>::FinalConfiguration,
            HaltConfiguration<to_bool_list!(1, 1, 1), True, to_bool_list!(1, 1)>,
        )>();

        const _: () = assert_same_type::<(<Result as Run<TM>>::Steps, _14)>();
    }

    mod four_state_busy_beaver {
        use super::*;

        turing_machine! {
            TM;

            [A B C D];

            (0, A) => (1, R, B);
            (1, A) => (1, L, B);

            (0, B) => (1, L, A);
            (1, B) => (0, L, C);

            (0, C) => (1, R, Z);
            (1, C) => (1, L, D);

            (0, D) => (1, R, D);
            (1, D) => (0, R, A);
        }

        #[allow(dead_code)]
        type Result = RunOnBlank<A>;

        const _: () = assert_same_type::<(
            <Result as Run<TM>>::FinalConfiguration,
            HaltConfiguration<
                to_bool_list!(1),
                False,
                to_bool_list!(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
            >,
        )>();

        const _: () = assert_same_type::<(<Result as Run<TM>>::Steps, nat!(1, 0, 7))>();
    }

    mod adder {
        use super::*;

        // Input:  1 1 1 0 1 1 0 ...
        // A:      1 1 1 X 1 1 0 ...
        // B:      1 1 1 1 1 X 0 ...
        // C:      1 1 1 1 X 0 0 ...
        // D:    X 1 1 1 1 1 0 0 ...
        // Output: 1 1 1 1 1 0 0 ...

        turing_machine! {
            TM;

            [A B C D];

            (1, A) => (1, R, A);
            (0, A) => (1, R, B);

            (1, B) => (1, R, B);
            (0, B) => (0, L, C);

            (1, C) => (0, L, D);

            (1, D) => (1, L, D);
            (0, D) => (0, R, Z);
        }

        #[allow(dead_code)]
        type Result = RunOn<A, Tape<Nil, True, to_bool_list!(1, 1, 0, 1, 1)>>;

        const _: () = assert_same_type::<(
            <Result as Run<TM>>::FinalConfiguration,
            HaltConfiguration<to_bool_list!(0), True, to_bool_list!(1, 1, 1, 1, 0, 0)>,
        )>();
    }
}
