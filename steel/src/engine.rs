use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;

pub trait Engine {
    fn evaluate(&mut self, expr: &str) -> Result<Vec<SteelVal>, SteelErr>;
    fn register(&mut self, name: &str, value: SteelVal);
    fn reset(&mut self);
    fn extract_value(&self, name: &str);
    fn evaluate_and_extract_last(&self, expr: &str, name: &str);
    fn compile(&mut self, expr: &str);
    fn set_max_memory(&mut self, max: usize);
    fn set_permissions(&mut self);
}
