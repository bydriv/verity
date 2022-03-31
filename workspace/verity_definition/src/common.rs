#[derive(Debug)]
pub struct Constituent<T> {
    location: (usize, usize),
    inner: std::rc::Rc<T>,
}

pub type Lexeme = Constituent<String>;

impl<T> Constituent<T> {
    pub fn new(location: (usize, usize), inner: T) -> Self {
        Self {
            location,
            inner: std::rc::Rc::new(inner),
        }
    }

    pub fn location(&self) -> (usize, usize) {
        self.location
    }

    pub fn relocate(self, location: (usize, usize)) -> Self {
        Self {
            location,
            inner: self.inner,
        }
    }
}

impl<T> Clone for Constituent<T> {
    fn clone(&self) -> Self {
        Self {
            location: self.location,
            inner: self.inner.clone(),
        }
    }
}

impl<T> std::ops::Deref for Constituent<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        std::ops::Deref::deref(&self.inner)
    }
}
