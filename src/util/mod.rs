pub mod intern;
pub mod src;

pub use self::{
    intern::{InternTable, Interned},
    src::{SrcLoc, SrcRegion},
};
