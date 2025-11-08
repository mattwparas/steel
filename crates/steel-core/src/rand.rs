//! Random helpers that centralize std/no_std handling.
use core::convert::{TryFrom, TryInto};
use core::fmt;
use core::ops::Range;

/// Errors that can occur when producing random data.
#[derive(Debug)]
pub enum RandomError {
    /// The operating system could not supply randomness.
    Unavailable(getrandom::Error),
    /// The requested range was invalid.
    InvalidRange,
}

impl fmt::Display for RandomError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RandomError::Unavailable(err) => write!(f, "randomness unavailable: {err}"),
            RandomError::InvalidRange => f.write_str("invalid random range"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for RandomError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            RandomError::Unavailable(err) => Some(err),
            RandomError::InvalidRange => None,
        }
    }
}

impl From<getrandom::Error> for RandomError {
    fn from(value: getrandom::Error) -> Self {
        Self::Unavailable(value)
    }
}

/// Fill the provided buffer with random bytes.
#[inline]
pub fn fill_bytes(bytes: &mut [u8]) -> Result<(), RandomError> {
    getrandom::fill(bytes).map_err(RandomError::Unavailable)
}

/// Produce a random `u64`.
#[inline]
pub fn try_random_u64() -> Result<u64, RandomError> {
    getrandom::u64().map_err(RandomError::Unavailable)
}

/// Produce a random `u64`, panicking if randomness is unavailable.
#[inline]
pub fn random_u64() -> u64 {
    try_random_u64().expect("random number generation is unavailable")
}

/// Produce a random `i64` in the provided half-open range.
#[inline]
pub fn try_random_range(range: Range<i64>) -> Result<i64, RandomError> {
    if range.start >= range.end {
        return Err(RandomError::InvalidRange);
    }

    let span = i128::from(range.end) - i128::from(range.start);
    let span = u64::try_from(span).map_err(|_| RandomError::InvalidRange)?;
    let offset = try_random_u64_below(span)?;

    (i128::from(range.start) + i128::from(offset))
        .try_into()
        .map_err(|_| RandomError::InvalidRange)
}

/// Produce a random `i64` in the provided half-open range, panicking on failure.
#[inline]
pub fn random_range(range: Range<i64>) -> i64 {
    try_random_range(range).expect("random range generation failed")
}

fn try_random_u64_below(bound: u64) -> Result<u64, RandomError> {
    if bound == 0 {
        return Err(RandomError::InvalidRange);
    }

    let zone = u64::MAX - (u64::MAX % bound);

    loop {
        let value = try_random_u64()?;
        if value < zone {
            return Ok(value % bound);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{random_range, try_random_u64};

    #[test]
    fn random_u64_smoke() {
        // Ensure we can request multiple values without panicking.
        for _ in 0..32 {
            let _ = try_random_u64().unwrap();
        }
    }

    #[test]
    fn respects_bounds() {
        for _ in 0..32 {
            let value = random_range(1..4);
            assert!(value >= 1 && value < 4);
        }
    }
}
