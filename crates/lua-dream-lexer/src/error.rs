use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Io Error")]
    Io(#[from] std::io::Error),
}
