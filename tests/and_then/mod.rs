/// This should really be in the Rust's core/std.
pub trait AndThen {
    fn and_then<U, F: FnOnce() -> Option<U>>(self, f: F) -> Option<U>;
}

impl AndThen for bool {
    fn and_then<U, F>(self, f: F) -> Option<U> where F: FnOnce() -> Option<U> {
        if self { f() } else { None }
    }
}
