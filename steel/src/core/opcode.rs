use serde::{Deserialize, Serialize};

#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Serialize, Deserialize)]
pub enum OpCode {
    VOID = 0,
    PUSH = 1,
    LOOKUP = 2,
    IF = 3,
    JMP = 4,
    FUNC = 5,
    SCLOSURE = 6,
    ECLOSURE = 7,
    STRUCT = 8,
    POP = 9,
    BIND = 10,
    SDEF = 11,
    EDEF = 12,
    PASS = 13,
    PUSHCONST = 14,
    NDEFS = 15,
    EVAL = 16,
    PANIC = 17,
    CLEAR = 18,
    TAILCALL = 19,
    APPLY,
    SET,
    COLLECT,
    TRANSDUCE,
    READ,
    COLLECTTO,
    METALOOKUP,
    CALLCC,
}
