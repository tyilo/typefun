//! Simulate Turing machines on Rust's type system.
//! Only binary tapes are supported.
//!
//! Example:
//! ```
//! use typefun::{
//!     bool::True,
//!     nat::consts::_6,
//!     tape, turing_machine,
//!     turing_machine::{HaltConfiguration, Run, RunOnBlank},
//!     types::assert_same_type,
//! };
//!
//! // Two-state busy beaver
//! // Declare the states and the transitions:
//! turing_machine! {
//!     TM; // Name of the type implementing `TuringMachine`.
//!
//!     [A B]; // Name of non-halting states.
//!
//!     // Transitions:
//!     (0, A) =>            // When in state `A` and reading a `0`:
//!               (1, R, B); // write a `1`, move to the Right and change the state to `B`.
//!     (1, A) => (1, L, B);
//!
//!     (0, B) => (1, L, A);
//!     (1, B) => (1, R, Z); // `Z` is the halting state.
//! }
//!
//! #[allow(dead_code)]
//! type Result = RunOnBlank<A>;
//!
//! const _: () = assert_same_type::<<Result as Run<TM>>::FinalTape, tape!([1 1] 1 [1])>();
//!
//! const _: () = assert_same_type::<<Result as Run<TM>>::Steps, _6>();
//! ```
//!
//! Expanding the macros this desugars to:
//! ```
//! use typefun::{
//!     bool::{False, True},
//!     list::bool::{BoolList, Cons, Nil},
//!     nat::consts::_6,
//!     turing_machine::{
//!         HaltConfiguration, HaltStep, NonHaltConfiguration, NonHaltState, NonHaltStep, Run,
//!         RunOnBlank, State, Step, Tape, TuringMachine, WriteAndLeft, WriteAndRight,
//!     },
//!     types::assert_same_type,
//! };
//!
//! enum TM {}
//! impl TuringMachine for TM {}
//!
//! enum A {}
//! impl State for A {}
//! impl NonHaltState for A {}
//!
//! enum B {}
//! impl State for B {}
//! impl NonHaltState for B {}
//!
//! impl<Left: BoolList, Right: BoolList> Step<TM> for NonHaltConfiguration<Left, False, Right, A> {
//!     type Next = NonHaltStep<WriteAndRight<Left, Right, True>, B>;
//! }
//!
//! impl<Left: BoolList, Right: BoolList> Step<TM> for NonHaltConfiguration<Left, True, Right, A> {
//!     type Next = NonHaltStep<WriteAndLeft<Left, Right, True>, B>;
//! }
//!
//! impl<Left: BoolList, Right: BoolList> Step<TM> for NonHaltConfiguration<Left, False, Right, B> {
//!     type Next = NonHaltStep<WriteAndLeft<Left, Right, True>, A>;
//! }
//!
//! impl<Left: BoolList, Right: BoolList> Step<TM> for NonHaltConfiguration<Left, True, Right, B> {
//!     type Next = HaltStep<WriteAndRight<Left, Right, True>>;
//! }
//!
//! #[allow(dead_code)]
//! type Result = RunOnBlank<A>;
//!
//! const _: () = assert_same_type::<
//!     <Result as Run<TM>>::FinalTape,
//!     Tape<Cons<True, Cons<True, Nil>>, True, Cons<True, Nil>>,
//! >();
//!
//! const _: () = assert_same_type::<<Result as Run<TM>>::Steps, _6>();
//! ```
//!
//! If you create a TM that doesn't halt, you will get a compile error when trying to use the
//! final tape or the total steps:
//! ```compile_fail
//! use typefun::nat::Nat;
//! use typefun::turing_machine;
//! use typefun::turing_machine::{Run, RunOnBlank};
//!
//! turing_machine! {
//!     NoHalt;
//!
//!     [A];
//!
//!     (0, A) => (1, R, A);
//! }
//!
//! #[allow(dead_code)]
//! type Result = RunOnBlank<A>;
//!
//! #[allow(dead_code)]
//! type Steps = <Result as Run<NoHalt>>::Steps;
//!
//! // Error: overflow evaluating the requirement `Succ<Succ<_>>: Nat`
//! const STEPS: usize = Steps::VALUE;
//! ```
//!
//! Instead you can run the TM for a finite number of steps:
//! ```
//! # use typefun::nat::Nat;
//! # use typefun::turing_machine;
//! # use typefun::turing_machine::{Run, RunOnBlank};
//! #
//! # turing_machine! {
//! #    NoHalt;
//! #
//! #    [A];
//! #
//! #    (0, A) => (1, R, A);
//! # }
//! #
//! # #[allow(dead_code)]
//! # type Result = RunOnBlank<A>;
//! use typefun::{
//!     nat::consts::_5,
//!     tape,
//!     turing_machine::{Configuration, StepN},
//!     types::assert_same_type,
//! };
//!
//! #[allow(dead_code)]
//! type After5 = <Result as StepN<_5, NoHalt>>::Configuration;
//!
//! const _: () = assert_same_type::<<After5 as Configuration>::Tape, tape!([1 1 1 1 1] 0 [])>();
//! const _: () = assert_same_type::<<After5 as Configuration>::State, A>();
//! ```
//!
//! You will also get a compilation error if the simulation encounters an undefined transition:
//! ```compile_fail
//! use typefun::nat::Nat;
//! use typefun::turing_machine;
//! use typefun::turing_machine::{Run, RunOnBlank};
//!
//! turing_machine! {
//!     UndefinedTransition;
//!
//!     [A];
//!
//!     (1, A) => (0, R, Z);
//! }
//!
//! #[allow(dead_code)]
//! type Result = RunOnBlank<A>;
//!
//! #[allow(dead_code)]
//! type Steps = <Result as Run<UndefinedTransition>>::Steps;
//!
//! // Error: the trait bound `NonHaltConfiguration<Nil, False, Nil, A>: Step<UndefinedTransition>` is not satisfied
//! const STEPS: usize = Steps::VALUE;
//! ```

use crate::{
    bool::{Bool, False},
    list::bool::{BoolList, Cons, Nil},
    nat::{Nat, Succ, Zero},
    types::assert_same_type,
    uninhabited::PhantomUninhabited,
};

pub struct Tape<Left: BoolList, Head: Bool, Right: BoolList>(
    PhantomUninhabited<(Left, Head, Right)>,
);
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
    type Tape: TapeT;
    type State: State;
}

/// Perform a single step on a Turing machine.
pub trait Step<TM: TuringMachine> {
    type Next: Configuration;
}

/// Run a Turing machine until it halts.
pub trait Run<TM: TuringMachine> {
    type FinalTape: TapeT;
    type Steps: Nat;
}

/// Configuration of a Turing machine that has halted.
pub struct HaltConfiguration<Left: BoolList, Head: Bool, Right: BoolList>(
    PhantomUninhabited<(Left, Head, Right)>,
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
    type FinalTape = <Self as Configuration>::Tape;
    type Steps = Zero;
}

/// A state that is not the halting state.
pub trait NonHaltState: State {}

/// Configuration of a Turing machine that has not halted yet.
pub struct NonHaltConfiguration<Left: BoolList, Head: Bool, Right: BoolList, State: NonHaltState>(
    PhantomUninhabited<(Left, Head, Right, State)>,
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
    Self: Step<TM>,
    <Self as Step<TM>>::Next: Run<TM>,
{
    type FinalTape = <<Self as Step<TM>>::Next as Run<TM>>::FinalTape;
    type Steps = Succ<<<Self as Step<TM>>::Next as Run<TM>>::Steps>;
}

/// Helper type to run a Turing machine starting in state `State` and on the tape `Tape`.
pub type RunOn<State, Tape> = NonHaltConfiguration<
    <Tape as TapeT>::Left,
    <Tape as TapeT>::Head,
    <Tape as TapeT>::Right,
    State,
>;
/// Helper type to run a Turing machine starting in state `State` and a blank tape.
pub type RunOnBlank<State> = RunOn<State, BlankTape>;

pub type HaltStep<Tape> =
    HaltConfiguration<<Tape as TapeT>::Left, <Tape as TapeT>::Head, <Tape as TapeT>::Right>;
pub type NonHaltStep<Tape, State> = NonHaltConfiguration<
    <Tape as TapeT>::Left,
    <Tape as TapeT>::Head,
    <Tape as TapeT>::Right,
    State,
>;

/// Perform a finite number of steps on a Turing machine.
pub trait StepN<N: Nat, TM: TuringMachine> {
    type Configuration: Configuration;
}

impl<N: Nat, TM: TuringMachine, Left: BoolList, Head: Bool, Right: BoolList> StepN<N, TM>
    for HaltConfiguration<Left, Head, Right>
{
    type Configuration = Self;
}

impl<TM: TuringMachine, Left: BoolList, Head: Bool, Right: BoolList, State: NonHaltState>
    StepN<Zero, TM> for NonHaltConfiguration<Left, Head, Right, State>
{
    type Configuration = Self;
}
impl<
        N: Nat,
        TM: TuringMachine,
        Left: BoolList,
        Head: Bool,
        Right: BoolList,
        State: NonHaltState,
    > StepN<Succ<N>, TM> for NonHaltConfiguration<Left, Head, Right, State>
where
    Self: Step<TM>,
    <Self as Step<TM>>::Next: StepN<N, TM>,
{
    type Configuration = <<Self as Step<TM>>::Next as StepN<N, TM>>::Configuration;
}

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
            enum $tm {}
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

    #[macro_export]
    macro_rules! tape {
        ([$($left:tt)*] $head:tt [$($right:tt)*]) => {
            $crate::turing_machine::Tape<
                $crate::to_bool_list!($($left),*),
                $crate::__to_bool!($head),
                $crate::to_bool_list!($($right),*)
            >
        };
    }
}

mod test {
    use super::*;
    use crate::{nat, nat::consts::*, tape, turing_machine};

    mod one_state_busy_beaver {
        use super::*;

        turing_machine! {
            TM;

            [A];

            (0, A) => (1, R, Z);
        }

        #[allow(dead_code)]
        type Result = RunOnBlank<A>;

        const _: () = assert_same_type::<<Result as Run<TM>>::FinalTape, tape!([1] 0 [])>();
        const _: () = assert_same_type::<<Result as Run<TM>>::Steps, _1>();
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

        const _: () = assert_same_type::<<Result as Run<TM>>::FinalTape, tape!([1 1] 1 [1])>();
        const _: () = assert_same_type::<<Result as Run<TM>>::Steps, _6>();
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

        const _: () = assert_same_type::<<Result as Run<TM>>::FinalTape, tape!([1 1 1] 1 [1 1])>();

        const _: () = assert_same_type::<<Result as Run<TM>>::Steps, _14>();
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

        const _: () = assert_same_type::<
            <Result as Run<TM>>::FinalTape,
            tape!([1] 0 [1 1 1 1 1 1 1 1 1 1 1 1]),
        >();

        const _: () = assert_same_type::<<Result as Run<TM>>::Steps, nat!(1, 0, 7)>();
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
        type Result = RunOn<A, tape!([] 1 [1 1 0 1 1])>;

        const _: () =
            assert_same_type::<<Result as Run<TM>>::FinalTape, tape!([0] 1 [1 1 1 1 0 0])>();
    }

    mod step_n {
        use super::*;

        turing_machine! {
            TM;

            [A];

            (0, A) => (1, R, A);
        }

        #[allow(dead_code)]
        type Result = RunOnBlank<A>;

        const _: () = assert_same_type::<
            <<Result as StepN<_0, TM>>::Configuration as Configuration>::Tape,
            tape!([] 0 []),
        >();

        const _: () = assert_same_type::<
            <<Result as StepN<_1, TM>>::Configuration as Configuration>::Tape,
            tape!([1] 0 []),
        >();

        const _: () = assert_same_type::<
            <<Result as StepN<_2, TM>>::Configuration as Configuration>::Tape,
            tape!([1 1] 0 []),
        >();

        const _: () = assert_same_type::<
            <<Result as StepN<_10, TM>>::Configuration as Configuration>::Tape,
            tape!([1 1 1 1 1 1 1 1 1 1] 0 []),
        >();
    }
}
