use tlc_lexer::{Lexer, token::Token};

pub struct CheckPoint {
    pub(self) offset: usize,
}

pub struct TokenStream<'src> {
    lexer: Lexer<'src>,
    cache: Vec<Token<'src>>,
    offset: usize,
}

impl<'src> TokenStream<'src> {
    /// Create a new token stream.
    /// None if lexer produced no tokens.
    pub fn new(lexer: Lexer<'src>) -> Option<Self> {
        let mut lexer = lexer;
        let token = lexer.next()?;
        Some(Self {
            lexer,
            cache: vec![token],
            offset: 0,
        })
    }

    /// Peek nth token from current position. 0 for current token.
    pub fn peek(&mut self, n: usize) -> Option<Token<'src>> {
        if self.offset + n < self.cache.len() {
            Some(self.cache[self.offset + n])
        } else {
            for _ in 0..=n {
                match self.lexer.next() {
                    Some(token) => self.cache.push(token),
                    None => return None,
                }
            }
            Some(self.cache[self.offset + n])
        }
    }

    /// Acquire last token of this stream.
    pub fn last(&mut self) -> Token<'src> {
        // Ensure cache holds all tokens.
        for token in self.lexer.by_ref() {
            self.cache.push(token);
        }

        // We have at least one token in cache, so unwrap always success.
        self.cache.last().copied().unwrap()
    }

    /// Acquire current token, advance position.
    pub fn advance(&mut self) -> Option<Token<'src>> {
        if self.offset + 1 < self.cache.len() {
            let result = self.cache[self.offset];
            self.offset += 1;
            Some(result)
        } else if self.offset < self.cache.len() {
            if let Some(token) = self.lexer.next() {
                self.cache.push(token);
            }

            let result = self.cache[self.offset];
            self.offset += 1;
            Some(result)
        } else {
            None
        }
    }

    /// Retreat position
    pub fn retreat(&mut self) {
        self.offset = self.offset.saturating_sub(1);
    }

    /// Save current state
    pub fn save(&mut self) -> CheckPoint {
        CheckPoint {
            offset: self.offset,
        }
    }

    /// Load state
    pub fn load(&mut self, checkpoint: CheckPoint) {
        self.offset = checkpoint.offset;
    }
}
