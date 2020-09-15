// SPDX-License-Identifier: BlueOak-1.0.0
// This program is free to use under the terms of the Blue Oak Model License 1.0.0.
// See: https://blueoakcouncil.org/license/1.0.0

/// This should really be in the Rust's core/std.
pub trait AndThen {
    fn and_then<U, F: FnOnce() -> Option<U>>(self, f: F) -> Option<U>;
}

impl AndThen for bool {
    fn and_then<U, F>(self, f: F) -> Option<U> where F: FnOnce() -> Option<U> {
        if self { f() } else { None }
    }
}
