pub trait ApplyContract {
    fn enforce_contracts(&self) -> bool;
}

pub struct ApplyContracts;

impl ApplyContract for ApplyContracts {
    #[inline(always)]
    fn enforce_contracts(&self) -> bool {
        true
    }
}

pub struct DoNotApplyContracts;

impl ApplyContract for DoNotApplyContracts {
    #[inline(always)]
    fn enforce_contracts(&self) -> bool {
        false
    }
}

pub trait UseCallbacks {
    fn use_callbacks(&self) -> bool;
}

pub struct UseCallback;

impl UseCallbacks for UseCallback {
    #[inline(always)]
    fn use_callbacks(&self) -> bool {
        true
    }
}

pub struct DoNotUseCallback;

impl UseCallbacks for DoNotUseCallback {
    #[inline(always)]
    fn use_callbacks(&self) -> bool {
        false
    }
}
