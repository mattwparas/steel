use serde::{Deserialize, Serialize};

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug, Ord, PartialOrd,
)]
#[repr(C)]
pub struct SourceId(pub usize);
