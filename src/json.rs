use std::fmt::Write;

/// Result type alias
pub type Result = std::fmt::Result;

/// All values that JSON writer can save need to implement that trait
pub trait Value {
    fn write(&self, w: &mut dyn Write) -> Result;
}

impl Value for &str {
    fn write(&self, w: &mut dyn std::fmt::Write) -> Result {
        write!(w, "\"{}\"", self)
    }
}

impl Value for i64 {
    fn write(&self, w: &mut dyn std::fmt::Write) -> Result {
        write!(w, "{}", self)
    }
}

impl Value for bool {
    fn write(&self, w: &mut dyn std::fmt::Write) -> Result {
        write!(w, "{}", self)
    }
}

impl Key for &str {
    fn as_str(&self) -> &str {
        self
    }
}

/// All key types that JSON writer can save need to implement that trait
pub trait Key {
    fn as_str(&self) -> &str;
}

/// JSON write helper.
///
/// It's nothing more than a handy JSON DSL to not get lost in
/// all kinds og open/closed brackets and quotation symbols.
///
/// ```rust
/// use query2json::json::Writer;
///
/// let mut s = String::new();
/// let mut w = Writer::new(&mut s);
///
/// let _ = w.object(|write| {
///     write.object("foo", |write| write.field("bar", "baz"))?;
///     write.array("a", |write| {
///         write.elem("foo")?;
///         write.elem(1)?;
///         write.elem(true)
///     })
/// });
///
/// assert_eq!(s, r#"{"foo":{"bar":"baz"},"a":["foo",1,true]}"#);
/// ```
pub struct Writer<'a> {
    out: &'a mut dyn Write,
}

impl<'a> Writer<'a> {
    pub fn new(out: &'a mut dyn Write) -> Self {
        Self { out }
    }

    pub fn value<V>(&mut self, value: V) -> Result
    where
        V: Value,
    {
        value.write(self.out)
    }

    pub fn object<F>(&mut self, f: F) -> Result
    where
        F: FnOnce(&mut ObjectWriter) -> Result,
    {
        write!(self.out, "{{")?;
        let mut w = ObjectWriter::new(self.out);
        f(&mut w)?;
        write!(self.out, "}}")
    }

    pub fn array<F>(&mut self, f: F) -> Result
    where
        F: FnOnce(&mut ArrayWriter) -> Result,
    {
        write!(self.out, "[")?;
        let mut w = ArrayWriter::new(self.out);
        f(&mut w)?;
        write!(self.out, "]")
    }
}

// ArrayWriter is a JSON write helper
pub struct ArrayWriter<'a> {
    empty: bool,
    out: &'a mut dyn Write,
}

impl<'a> ArrayWriter<'a> {
    pub fn new(out: &'a mut dyn Write) -> Self {
        Self { out, empty: true }
    }

    fn write_sep(&mut self) -> Result {
        if !self.empty {
            self.out.write_char(',')?;
        }
        self.empty = false;
        Ok(())
    }

    pub fn elem<V>(&mut self, value: V) -> Result
    where
        V: Value,
    {
        self.write_sep()?;
        value.write(self.out)
    }

    pub fn object<F>(&mut self, f: F) -> Result
    where
        F: FnOnce(&mut ObjectWriter) -> Result,
    {
        self.write_sep()?;
        let mut w = Writer::new(self.out);
        w.object(f)
    }

    pub fn array<F>(&mut self, f: F) -> Result
    where
        F: FnOnce(&mut ArrayWriter) -> Result,
    {
        self.write_sep()?;
        let mut w = Writer::new(self.out);
        w.array(f)
    }
}

// ObjectWriter is a JSON write helper
pub struct ObjectWriter<'a> {
    empty: bool,
    out: &'a mut dyn Write,
}

impl<'a> ObjectWriter<'a> {
    fn new(out: &'a mut dyn Write) -> Self {
        Self { out, empty: true }
    }

    fn write_sep(&mut self) -> Result {
        if !self.empty {
            self.out.write_char(',')?;
        }
        self.empty = false;
        Ok(())
    }

    pub fn object<K, F>(&mut self, name: K, f: F) -> Result
    where
        K: Key,
        F: FnOnce(&mut ObjectWriter) -> Result,
    {
        self.write_sep()?;
        write!(self.out, "\"{}\":", name.as_str())?;
        let mut w = Writer::new(self.out);
        w.object(f)
    }

    pub fn array<K, F>(&mut self, name: K, f: F) -> Result
    where
        K: Key,
        F: FnOnce(&mut ArrayWriter) -> Result,
    {
        self.write_sep()?;
        write!(self.out, "\"{}\":", name.as_str())?;
        let mut w = Writer::new(self.out);
        w.array(f)
    }

    pub fn field<K, V>(&mut self, name: K, value: V) -> Result
    where
        K: Key,
        V: Value,
    {
        self.write_sep()?;
        write!(self.out, "\"{}\":", name.as_str())?;
        value.write(self.out)
    }
}

#[cfg(test)]
mod tests {
    use crate::json::Writer;

    #[test]
    fn test_object_writer() {
        let mut s = String::new();
        let mut w = Writer::new(&mut s);

        let _ = w.object(|write| {
            write.object("foo", |write| write.field("bar", "baz"))?;
            write.array("a", |write| {
                write.elem("foo")?;
                write.elem(1)?;
                write.elem(true)
            })
        });

        assert_eq!(s, r#"{"foo":{"bar":"baz"},"a":["foo",1,true]}"#);
    }
}
