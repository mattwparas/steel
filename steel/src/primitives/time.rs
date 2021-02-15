use crate::gc::Gc;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::convert::TryFrom;
use std::fmt::Display;
use std::time::SystemTime;
// use chrono::

// TODO fix this noise

pub struct TimeOperations {}
impl TimeOperations {
    pub fn time_clock() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 0 {
                stop!(ArityMismatch => "time.clock takes no arguments")
            }

            // let date_time = SystemTime::now().to_string();

            let date_time = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH);

            Ok(Gc::new(SteelVal::StringV(date_time)))
        })
    }

    pub fn time_elapsed() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "time.elapsed only takes one argument")
            }
            if let SteelVal::StringV(old_time) = &args[0].as_ref() {
                // let old_time = Instant::from(old_time);

                // Ok(Gc::new(SteelVal::StringV(old_time.to_string())))

                unimplemented!();
            } else {
                stop!(TypeMismatch => "time.elapsed expected a time-stamp")
            }
        })
    }
}
