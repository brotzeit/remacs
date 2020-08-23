//! Threading code.

use std::mem;

use crate::{
    buffers::LispBufferRef,
    lisp::ExternalPtr,
    remacs_sys::{current_thread as current_thread_pointer, thread_state},
};

pub type ThreadStateRef = ExternalPtr<thread_state>;

pub struct ThreadState {}

impl ThreadState {
    pub fn current_buffer_unchecked() -> LispBufferRef {
        unsafe { mem::transmute((*current_thread_pointer).m_current_buffer) }
    }

        pub fn current_thread() -> ThreadStateRef {
            unsafe { mem::transmute(current_thread_pointer) }
        }
}

include!(concat!(env!("OUT_DIR"), "/threads_exports.rs"));
