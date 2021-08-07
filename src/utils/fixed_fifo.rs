#![allow(non_upper_case_globals)] // I like "Size" more than Size

/// A FIFO queue that is array-backed (i.e. it has a fixed size) and
/// allocates on the stack.
pub struct FixedFifo<T, const Size: usize> {
    start: usize,
    length: usize,
    inner: [T; Size],
}

impl <T, const Size: usize> FixedFifo<T, Size> 
    where T: Default,
          [T; Size]: Default
{
    /// Create a new instance of `FixedFifo<T, Size>`.
    pub fn new() -> Self {
        Self {
            start: 0,
            length: 0,
            inner: Default::default(),
        }
    }

    /// Tries to push the value into the back of the queue, returning `Result::Err(T)` 
    /// if the queue is full, or `Ok(())` if not.
    pub fn push(&mut self, value: T) -> Result<(), T> {
        let end = self.start + self.length;
        let index = end % Size;

        if self.length < Size {
            self.inner[index] = value;
            self.length += 1;
            Ok(())
        } else {
            Err(value)
        }
    }

    /// Tries to pop a value out of the front of the queue, returning None if the
    /// queue is empty.
    pub fn pop(&mut self) -> Option<T> {
        if self.length > 0 {
            let index = self.start % Size;
            let value = std::mem::replace(&mut self.inner[index], T::default());

            self.start = (self.start + 1) % Size;
            self.length -= 1;

            Some(value)
        } else {
            None
        }
    }

    /// Returns a reference to the first element in the queue.
    pub fn first(&self) -> Option<&T> {
        if self.length > 0 {
            let index = self.start % Size;
            Some(&self.inner[index])
        } else {
            None
        }
    }

    /// Returns a reference to the last element in the queue.
    pub fn last(&self) -> Option<&T> {
        if self.length > 0 {
            let index = (self.start + self.length - 1) % Size;
            Some(&self.inner[index])
        } else {
            None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.length
    }
}
