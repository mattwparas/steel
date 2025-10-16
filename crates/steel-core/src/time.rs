#![cfg_attr(not(feature = "std"), allow(dead_code))]

#[cfg(feature = "std")]
pub use std::time::{Duration, Instant, SystemTime, SystemTimeError, UNIX_EPOCH};

#[cfg(not(feature = "std"))]
mod imp {
    use core::{
        cell::SyncUnsafeCell,
        fmt,
        ops::{Add, AddAssign, Sub, SubAssign},
        sync::atomic::{AtomicBool, Ordering as AtomicOrdering},
    };
    #[cfg(target_arch = "wasm32")]
    use js_sys::Date;

    type NowFn = fn() -> u128;

    #[derive(Default)]
    struct ProviderCell {
        func: SyncUnsafeCell<NowFn>,
        is_custom: AtomicBool,
    }

    unsafe impl Sync for ProviderCell {}

    const fn default_now() -> u128 {
        0
    }

    static TIME_PROVIDER: ProviderCell = ProviderCell {
        func: SyncUnsafeCell::new(default_now),
        is_custom: AtomicBool::new(false),
    };

    fn now_fn() -> NowFn {
        unsafe { *TIME_PROVIDER.func.get() }
    }

    /// Configure the global time provider used by `Instant::now` and `SystemTime::now`.
    ///
    /// The provided function is expected to return the number of nanoseconds elapsed since an
    /// arbitrary monotonic epoch. Consumers running in `no_std` environments should set this hook
    /// during start-up to integrate with their platform specific timer source.
    pub fn set_time_provider(provider: NowFn) {
        unsafe { *TIME_PROVIDER.func.get() = provider };
        TIME_PROVIDER.is_custom.store(true, AtomicOrdering::Release);
    }

    #[cfg(target_arch = "wasm32")]
    fn wasm_now_provider() -> u128 {
        let millis = Date::now().max(0.0);
        (millis * 1_000_000.0) as u128
    }

    fn load_ticks() -> u128 {
        #[cfg(target_arch = "wasm32")]
        {
            if !TIME_PROVIDER.is_custom.load(AtomicOrdering::Acquire) {
                set_time_provider(wasm_now_provider);
            }
        }

        (now_fn())()
    }

    #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
    pub struct Duration {
        nanos: u128,
    }

    impl Duration {
        pub const ZERO: Self = Self { nanos: 0 };

        pub const fn from_nanos(nanos: u64) -> Self {
            Self {
                nanos: nanos as u128,
            }
        }

        pub fn from_micros(micros: u64) -> Self {
            Self::from_total_nanos(
                (micros as u128)
                    .checked_mul(1_000)
                    .expect("overflow in Duration::from_micros"),
            )
        }

        pub fn from_millis(millis: u64) -> Self {
            Self::from_total_nanos(
                (millis as u128)
                    .checked_mul(1_000_000)
                    .expect("overflow in Duration::from_millis"),
            )
        }

        pub fn from_secs(secs: u64) -> Self {
            Self::from_total_nanos(
                (secs as u128)
                    .checked_mul(1_000_000_000)
                    .expect("overflow in Duration::from_secs"),
            )
        }

        pub fn as_secs(&self) -> u64 {
            (self.nanos / 1_000_000_000).min(u64::MAX as u128) as u64
        }

        pub fn as_secs_f64(&self) -> f64 {
            self.nanos as f64 / 1_000_000_000.0
        }

        pub fn as_millis(&self) -> u128 {
            self.nanos / 1_000_000
        }

        pub fn as_micros(&self) -> u128 {
            self.nanos / 1_000
        }

        pub fn as_nanos(&self) -> u128 {
            self.nanos
        }

        pub fn checked_add(self, rhs: Self) -> Option<Self> {
            self.nanos
                .checked_add(rhs.nanos)
                .map(Self::from_total_nanos)
        }

        pub fn checked_sub(self, rhs: Self) -> Option<Self> {
            self.nanos
                .checked_sub(rhs.nanos)
                .map(Self::from_total_nanos)
        }

        const fn from_total_nanos(nanos: u128) -> Self {
            Self { nanos }
        }
    }

    impl fmt::Debug for Duration {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Duration")
                .field("secs", &self.as_secs())
                .field("nanos", &(self.nanos % 1_000_000_000))
                .finish()
        }
    }

    impl Add for Duration {
        type Output = Self;

        fn add(self, rhs: Self) -> Self::Output {
            self.checked_add(rhs)
                .expect("overflow when adding Durations")
        }
    }

    impl Sub for Duration {
        type Output = Self;

        fn sub(self, rhs: Self) -> Self::Output {
            self.checked_sub(rhs)
                .expect("overflow when subtracting Durations")
        }
    }

    impl AddAssign for Duration {
        fn add_assign(&mut self, rhs: Self) {
            *self = (*self) + rhs;
        }
    }

    impl SubAssign for Duration {
        fn sub_assign(&mut self, rhs: Self) {
            *self = (*self) - rhs;
        }
    }

    #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
    pub struct Instant {
        nanos_since_epoch: u128,
    }

    impl Instant {
        pub fn now() -> Self {
            Self {
                nanos_since_epoch: load_ticks(),
            }
        }

        pub fn checked_duration_since(&self, earlier: Self) -> Option<Duration> {
            self.nanos_since_epoch
                .checked_sub(earlier.nanos_since_epoch)
                .map(Duration::from_total_nanos)
        }

        pub fn duration_since(&self, earlier: Self) -> Duration {
            self.checked_duration_since(earlier)
                .expect("earlier instant is later than Self")
        }

        pub fn elapsed(&self) -> Duration {
            Self::now().duration_since(*self)
        }

        pub fn checked_add(&self, duration: Duration) -> Option<Self> {
            self.nanos_since_epoch
                .checked_add(duration.nanos)
                .map(|nanos| Self {
                    nanos_since_epoch: nanos,
                })
        }

        pub fn checked_sub(&self, duration: Duration) -> Option<Self> {
            self.nanos_since_epoch
                .checked_sub(duration.nanos)
                .map(|nanos| Self {
                    nanos_since_epoch: nanos,
                })
        }
    }

    impl fmt::Debug for Instant {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_tuple("Instant")
                .field(&self.nanos_since_epoch)
                .finish()
        }
    }

    impl Add<Duration> for Instant {
        type Output = Self;

        fn add(self, rhs: Duration) -> Self::Output {
            self.checked_add(rhs)
                .expect("overflow when adding Duration to Instant")
        }
    }

    impl Sub<Duration> for Instant {
        type Output = Self;

        fn sub(self, rhs: Duration) -> Self::Output {
            self.checked_sub(rhs)
                .expect("overflow when subtracting Duration from Instant")
        }
    }

    impl Sub for Instant {
        type Output = Duration;

        fn sub(self, rhs: Self) -> Self::Output {
            self.duration_since(rhs)
        }
    }

    #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
    pub struct SystemTime {
        nanos_since_unix_epoch: u128,
    }

    pub const UNIX_EPOCH: SystemTime = SystemTime {
        nanos_since_unix_epoch: 0,
    };

    impl SystemTime {
        pub fn now() -> Self {
            Self {
                nanos_since_unix_epoch: load_ticks(),
            }
        }

        pub fn duration_since(&self, earlier: Self) -> Result<Duration, SystemTimeError> {
            match self
                .nanos_since_unix_epoch
                .checked_sub(earlier.nanos_since_unix_epoch)
            {
                Some(diff) => Ok(Duration::from_total_nanos(diff)),
                None => Err(SystemTimeError(Duration::from_total_nanos(
                    earlier
                        .nanos_since_unix_epoch
                        .checked_sub(self.nanos_since_unix_epoch)
                        .unwrap(),
                ))),
            }
        }

        pub fn elapsed(&self) -> Result<Duration, SystemTimeError> {
            self.duration_since(UNIX_EPOCH)
        }

        pub fn checked_add(&self, duration: Duration) -> Option<Self> {
            self.nanos_since_unix_epoch
                .checked_add(duration.nanos)
                .map(|nanos| Self {
                    nanos_since_unix_epoch: nanos,
                })
        }

        pub fn checked_sub(&self, duration: Duration) -> Option<Self> {
            self.nanos_since_unix_epoch
                .checked_sub(duration.nanos)
                .map(|nanos| Self {
                    nanos_since_unix_epoch: nanos,
                })
        }
    }

    impl fmt::Debug for SystemTime {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_tuple("SystemTime")
                .field(&self.nanos_since_unix_epoch)
                .finish()
        }
    }

    #[derive(Copy, Clone, Eq, PartialEq)]
    pub struct SystemTimeError(Duration);

    impl SystemTimeError {
        pub fn duration(&self) -> Duration {
            self.0
        }
    }

    impl fmt::Debug for SystemTimeError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_tuple("SystemTimeError").field(&self.0).finish()
        }
    }

    impl fmt::Display for SystemTimeError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "second time provided was later than self")
        }
    }

    #[cfg(feature = "std")]
    impl std::error::Error for SystemTimeError {}
}

#[cfg(not(feature = "std"))]
pub use imp::{set_time_provider, Duration, Instant, SystemTime, SystemTimeError, UNIX_EPOCH};
