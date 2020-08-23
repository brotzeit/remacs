//! Generic Lisp eval functions

use std::unreachable;

use remacs_macros::lisp_fn;

use crate::{lisp::LispObject, remacs_sys::signal_or_quit};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   NOTE!!! Every function that can call EVAL must protect its args   *
 *   and temporaries from garbage collection while it needs them.      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/// Signal an error.  Args are ERROR-SYMBOL and associated DATA. This
/// function does not return.
///
/// An error symbol is a symbol with an `error-conditions' property
/// that is a list of condition names.  A handler for any of those
/// names will get to handle this signal.  The symbol `error' should
/// normally be one of them.
///
/// DATA should be a list.  Its elements are printed as part of the
/// error message.  See Info anchor `(elisp)Definition of signal' for
/// some details on how this error message is constructed.
/// If the signal is handled, DATA is made available to the handler.
/// See also the function `condition-case'.
#[lisp_fn]
pub fn signal_rust(error_symbol: LispObject, data: LispObject) -> ! {
    #[cfg(test)]
    {
        panic!("Fsignal called during tests.");
    }
    #[cfg(not(test))]
    {
        unsafe { signal_or_quit(error_symbol, data, false) };
        unreachable!();
    }
}

include!(concat!(env!("OUT_DIR"), "/eval_exports.rs"));
