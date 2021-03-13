pub trait SliceExt<T> {
    fn get_clone(&self, idx: usize) -> T;
}

impl<T: Clone> SliceExt<T> for &[T] {
    fn get_clone(&self, idx: usize) -> T {
        self[idx].clone()
    }
}
