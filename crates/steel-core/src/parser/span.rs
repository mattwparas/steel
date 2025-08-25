use crate::{list, rvals::FromSteelVal, rvals::IntoSteelVal};

use crate::rvals::SteelVal;

use super::parser::SourceId;

pub use steel_parser::span::Span;

impl IntoSteelVal for Span {
    fn into_steelval(self) -> crate::rvals::Result<crate::SteelVal> {
        Ok(list![self.start, self.end, self.source_id])
    }
}

impl FromSteelVal for Span {
    fn from_steelval(val: &crate::SteelVal) -> crate::rvals::Result<Self> {
        if let SteelVal::ListV(l) = val {
            if l.len() != 3 {
                stop!(ConversionError => "cannot convert to a span object: {}", val);
            }

            Ok(Span {
                start: u32::from_steelval(l.get(0).unwrap())?,
                end: u32::from_steelval(l.get(1).unwrap())?,
                source_id: l
                    .get(2)
                    .map(Option::<u32>::from_steelval)
                    .and_then(|x| x.transpose())
                    .transpose()?
                    .map(SourceId),
            })
        } else {
            stop!(ConversionError => "cannot convert to a span object: {}", val)
        }
    }
}
