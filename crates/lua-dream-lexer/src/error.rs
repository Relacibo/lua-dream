use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Io Error")]
    Io(#[from] std::io::Error),
}
