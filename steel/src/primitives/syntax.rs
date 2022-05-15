use crate::{stop, SteelVal};

use crate::rvals::Result;

pub fn syntax_loc() -> SteelVal {
    SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
        todo!()
        // if args.len() != 0 {
        //     stop!(ArityMismatch => "enumerating takes no arguments");
        // }

        // let mut transducer = Transducer::new();
        // transducer.push(Transducers::Enumerating);
        // Ok(SteelVal::IterV(Gc::new(transducer)))
    })
}
