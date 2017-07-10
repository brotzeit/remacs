//! Operations on characters.

use lisp::LispObject;
use multibyte::{MAX_CHAR, make_char_multibyte};
use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, error};

/// Return the character of the maximum code.
#[lisp_fn]
fn max_char() -> LispObject {
    LispObject::from_fixnum(MAX_CHAR as EmacsInt)
}

/// Return non-nil if OBJECT is a character.
/// In Emacs Lisp, characters are represented by character codes, which
/// are non-negative integers.  The function `max-char' returns the
/// maximum character code.
/// usage: (fn OBJECT)
#[lisp_fn(min = "1")]
fn characterp(object: LispObject, _ignore: LispObject) -> LispObject {
    LispObject::from_bool(object.is_character())
}

/// Return t if OBJECT is a character or a string.
#[lisp_fn]
fn char_or_string_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_character() || object.is_string())
}

/// Convert the byte CH to multibyte character.
#[lisp_fn]
fn unibyte_char_to_multibyte(ch: LispObject) -> LispObject {
    let c = ch.as_character_or_error();
    if c >= 0x100 {
        unsafe {
            error("Not a unibyte character: %d\0".as_ptr(), c);
        }
    }
    LispObject::from_fixnum(make_char_multibyte(c) as EmacsInt)
}
