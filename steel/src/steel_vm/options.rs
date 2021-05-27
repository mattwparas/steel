pub trait ApplyContracts: Copy {
    fn enforce_contracts(&self) -> bool;
}

#[derive(Clone, Copy)]
pub struct ApplyContract;

impl ApplyContracts for ApplyContract {
    #[inline(always)]
    fn enforce_contracts(&self) -> bool {
        true
    }
}

#[derive(Clone, Copy)]
pub struct DoNotApplyContracts;

impl ApplyContracts for DoNotApplyContracts {
    #[inline(always)]
    fn enforce_contracts(&self) -> bool {
        false
    }
}

pub trait UseCallbacks: Copy {
    fn use_callbacks(&self) -> bool;
}

#[derive(Clone, Copy)]
pub struct UseCallback;

impl UseCallbacks for UseCallback {
    #[inline(always)]
    fn use_callbacks(&self) -> bool {
        true
    }
}

#[derive(Clone, Copy)]
pub struct DoNotUseCallback;

impl UseCallbacks for DoNotUseCallback {
    #[inline(always)]
    fn use_callbacks(&self) -> bool {
        false
    }
}
