#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Pos {
    pub row: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(row: usize, col: usize) -> Pos {
        Pos { row, col }
    }

    pub fn advance(&mut self, c: char) {
        self.col += 1;
        if c == '\n' {
            self.row += 1;
            self.col = 1;
        }
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl Default for Pos {
    fn default() -> Self {
        Pos::new(1, 1)
    }
}
