use serde::{Deserialize, Serialize};

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug, Ord, PartialOrd,
)]
pub struct SourceId(pub(crate) usize);
