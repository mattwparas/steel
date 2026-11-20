//! Compiled functions kept on disk (`STEEL_JIT_CODE_CACHE`, experimental):
//! Cranelift's incremental cache, keyed by a function's IR and the target, a
//! file per function in `<boot image dir>/jit`.
//!
//! Pruned like Wasmtime's cache: past a soft limit on the number of files or
//! their total size, the least recently used are deleted until both are at 70%
//! of their limits. A hit touches its file, so modification times say what was
//! used last. A cleanup runs after a write, at most once per interval, on a
//! thread of its own; a marker file keeps the time the last one finished and a
//! lock file (holding the pid) keeps two processes from running one at once.

use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

const LAST_CLEANUP: &str = ".last-cleanup";
const CLEANING: &str = ".cleaning";
// Of a limit, what a cleanup leaves
const KEEP_PERCENT: u64 = 70;
// A lock older than this was left by a process that stopped (one whose process
// is gone is stale at once, where that can be told)
const STALE_LOCK: Duration = Duration::from_secs(600);

/// The cache directory's store
pub(crate) struct CodeCacheStore(PathBuf);

/// The soft limits
#[derive(Clone, Copy, Debug)]
pub(crate) struct Limits {
    pub(crate) files: u64,
    pub(crate) bytes: u64,
}

impl Limits {
    fn from_config() -> Self {
        let config = crate::config::config();
        Limits {
            files: config.jit_code_cache_max_files,
            bytes: config.jit_code_cache_max_mib.saturating_mul(1 << 20),
        }
    }
}

impl CodeCacheStore {
    // Looked up each time: the program can say where the cache goes after the
    // first compile
    pub(crate) fn open() -> Option<Self> {
        Some(CodeCacheStore(crate::steel_vm::engine::boot_image::cache_dir()?.join("jit")))
    }

    fn file(&self, key: &[u8]) -> PathBuf {
        let name: String = key.iter().map(|b| format!("{b:02x}")).collect();
        self.0.join(name)
    }
}

impl cranelift_codegen::incremental_cache::CacheKvStore for CodeCacheStore {
    fn get(&self, key: &[u8]) -> Option<std::borrow::Cow<'_, [u8]>> {
        let file = self.file(key);
        let bytes = std::fs::read(&file).ok()?;
        // Used now, for the cleanup's order
        touch(&file);
        Some(std::borrow::Cow::Owned(bytes))
    }

    fn insert(&mut self, key: &[u8], value: Vec<u8>) {
        static WRITES: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        if std::fs::create_dir_all(&self.0).is_err() {
            return;
        }
        let file = self.file(key);
        let temporary = file.with_extension(format!(
            "{}.{}.tmp",
            std::process::id(),
            WRITES.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
        ));
        if std::fs::write(&temporary, value).and_then(|()| std::fs::rename(&temporary, &file)).is_err() {
            let _ = std::fs::remove_file(&temporary);
        }
        let interval = Duration::from_secs(crate::config::config().jit_code_cache_cleanup_secs);
        maybe_clean(&self.0, interval, Limits::from_config());
    }
}

fn touch(file: &Path) {
    if let Ok(f) = std::fs::OpenOptions::new().append(true).open(file) {
        let _ = f.set_modified(SystemTime::now());
    }
}

fn modified(path: &Path) -> Option<SystemTime> {
    std::fs::metadata(path).and_then(|m| m.modified()).ok()
}

// Starts a cleanup on a thread of its own if the interval has passed since the
// last one finished, here or in another process. One at a time in a process;
// the lock in `clean` keeps processes apart.
fn maybe_clean(dir: &Path, interval: Duration, limits: Limits) {
    static RUNNING: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    let marker = dir.join(LAST_CLEANUP);
    let due = modified(&marker).is_none_or(|last| last.elapsed().is_ok_and(|e| e >= interval));
    if !due || RUNNING.swap(true, std::sync::atomic::Ordering::AcqRel) {
        return;
    }
    let dir = dir.to_path_buf();
    let spawned = std::thread::Builder::new()
        .name("steel-jit-cache-cleanup".into())
        .spawn(move || {
            if let Some(removed) = clean(&dir, limits) {
                // Done: a cleanup cut short by the process exiting doesn't count
                let _ = std::fs::write(dir.join(LAST_CLEANUP), b"");
                if crate::config::config().jit_code_cache_log {
                    eprintln!("jit-cache: cleanup removed {removed} files");
                }
            }
            RUNNING.store(false, std::sync::atomic::Ordering::Release);
        });
    if spawned.is_err() {
        RUNNING.store(false, std::sync::atomic::Ordering::Release);
    }
}

// Whether the lock is one nobody holds any more: old, or (where processes can
// be looked up) its process is gone
fn stale(lock: &Path) -> bool {
    let Some(at) = modified(lock) else {
        return false;
    };
    if at.elapsed().is_ok_and(|e| e >= STALE_LOCK) {
        return true;
    }
    let holder = std::fs::read_to_string(lock).ok().and_then(|s| s.trim().parse::<u32>().ok());
    match holder {
        Some(pid) if Path::new("/proc/self").exists() => !Path::new("/proc").join(pid.to_string()).exists(),
        _ => false,
    }
}

// Takes the lock, first taking over one nobody holds. Moving a stale lock
// aside is atomic, so of two processes doing it only one succeeds, and nobody
// removes a lock someone else just took.
fn lock(dir: &Path) -> bool {
    let lock = dir.join(CLEANING);
    if stale(&lock) {
        let aside = dir.join(format!("{CLEANING}.{}.stale", std::process::id()));
        if std::fs::rename(&lock, &aside).is_ok() {
            let _ = std::fs::remove_file(&aside);
        }
    }
    match std::fs::OpenOptions::new().write(true).create_new(true).open(&lock) {
        Ok(mut f) => {
            use std::io::Write;
            let _ = write!(f, "{}", std::process::id());
            true
        }
        Err(_) => false,
    }
}

/// Deletes the least recently used entries past the limits, until both the
/// number of files and their size are at 70% of them, and temporary files a
/// writer left. Gives how many files it removed, or None when another
/// cleanup holds the lock.
pub(crate) fn clean(dir: &Path, limits: Limits) -> Option<usize> {
    if !lock(dir) {
        return None;
    }
    let lock = dir.join(CLEANING);

    let mut removed = 0;
    let mut entries: Vec<(SystemTime, u64, PathBuf)> = Vec::new();
    for entry in std::fs::read_dir(dir).into_iter().flatten().flatten() {
        let path = entry.path();
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        let Ok(meta) = entry.metadata() else { continue };
        let time = meta.modified().unwrap_or(SystemTime::UNIX_EPOCH);
        if name.starts_with('.') || !meta.is_file() {
            continue;
        }
        if name.ends_with(".tmp") {
            // A write that didn't finish
            if time.elapsed().is_ok_and(|e| e > STALE_LOCK) && std::fs::remove_file(&path).is_ok() {
                removed += 1;
            }
            continue;
        }
        entries.push((time, meta.len(), path));
    }

    let (mut files, mut bytes) = (entries.len() as u64, entries.iter().map(|e| e.1).sum::<u64>());
    if files > limits.files || bytes > limits.bytes {
        let keep = |limit: u64| (limit as u128 * KEEP_PERCENT as u128 / 100) as u64;
        let (keep_files, keep_bytes) = (keep(limits.files), keep(limits.bytes));
        // Oldest first
        entries.sort_by_key(|e| e.0);
        for (_, size, path) in entries {
            if files <= keep_files && bytes <= keep_bytes {
                break;
            }
            if std::fs::remove_file(&path).is_ok() {
                removed += 1;
                files -= 1;
                bytes -= size;
            }
        }
    }
    let _ = std::fs::remove_file(&lock);
    Some(removed)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(dir: &Path, name: &str, size: usize, age_secs: u64) {
        let path = dir.join(name);
        std::fs::write(&path, vec![0u8; size]).unwrap();
        let f = std::fs::OpenOptions::new().append(true).open(&path).unwrap();
        f.set_modified(SystemTime::now() - Duration::from_secs(age_secs)).unwrap();
    }

    fn names(dir: &Path) -> Vec<String> {
        let mut names: Vec<String> = std::fs::read_dir(dir)
            .unwrap()
            .flatten()
            .map(|e| e.file_name().to_string_lossy().into_owned())
            .filter(|n| !n.starts_with('.'))
            .collect();
        names.sort();
        names
    }

    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("steel-jit-cache-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    // Past the file limit, the least recently used go until 70% of it is left
    #[test]
    fn the_oldest_entries_go_past_the_file_limit() {
        let dir = scratch("files");
        for i in 0..10 {
            entry(&dir, &format!("e{i}"), 10, 100 - i);
        }
        let removed = clean(&dir, Limits { files: 8, bytes: u64::MAX });
        // 10 files, limit 8: down to 70% of 8 = 5
        assert_eq!(removed, Some(5));
        assert_eq!(names(&dir), ["e5", "e6", "e7", "e8", "e9"]);
        // Under the limits nothing goes
        assert_eq!(clean(&dir, Limits { files: 8, bytes: u64::MAX }), Some(0));
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn the_oldest_entries_go_past_the_size_limit() {
        let dir = scratch("bytes");
        for i in 0..4 {
            entry(&dir, &format!("e{i}"), 100, 100 - i);
        }
        // 400 bytes, limit 300: down to 210
        assert_eq!(clean(&dir, Limits { files: u64::MAX, bytes: 300 }), Some(2));
        assert_eq!(names(&dir), ["e2", "e3"]);
        let _ = std::fs::remove_dir_all(&dir);
    }

    // A hit counts as a use: the entry used last is the one kept
    #[test]
    fn a_hit_keeps_an_entry() {
        use cranelift_codegen::incremental_cache::CacheKvStore;
        let dir = scratch("hit");
        let store = CodeCacheStore(dir.clone());
        let used = store.file(b"used");
        std::fs::write(&used, b"code").unwrap();
        let f = std::fs::OpenOptions::new().append(true).open(&used).unwrap();
        f.set_modified(SystemTime::now() - Duration::from_secs(1000)).unwrap();
        entry(&dir, "a", 4, 10);
        entry(&dir, "b", 4, 10);
        assert!(store.get(b"used").is_some());
        // 3 files, limit 2: down to 1
        assert_eq!(clean(&dir, Limits { files: 2, bytes: u64::MAX }), Some(2));
        assert_eq!(names(&dir), [used.file_name().unwrap().to_string_lossy().into_owned()]);
        let _ = std::fs::remove_dir_all(&dir);
    }

    // Unfinished writes go once they're old; another cleanup's lock stops this one
    #[test]
    fn temporary_files_and_locks() {
        let dir = scratch("tmp");
        entry(&dir, "x.1.2.tmp", 10, 3600);
        entry(&dir, "y.1.3.tmp", 10, 1);
        entry(&dir, "keep", 10, 1);
        assert_eq!(clean(&dir, Limits { files: 100, bytes: u64::MAX }), Some(1));
        assert_eq!(names(&dir), ["keep", "y.1.3.tmp"]);
        // Held by this (running) process: not taken
        std::fs::write(dir.join(CLEANING), std::process::id().to_string()).unwrap();
        entry(&dir, "z.1.4.tmp", 10, 3600);
        assert_eq!(clean(&dir, Limits { files: 100, bytes: u64::MAX }), None, "another cleanup is running");
        // Held by a process that's gone: taken over
        std::fs::write(dir.join(CLEANING), "4294967295").unwrap();
        assert_eq!(clean(&dir, Limits { files: 100, bytes: u64::MAX }), Some(1));
        assert!(!dir.join(CLEANING).exists(), "the lock is let go");
        let _ = std::fs::remove_dir_all(&dir);
    }

    // A cleanup is due once the interval has passed since the last one
    #[test]
    fn cleanups_wait_for_the_interval() {
        let dir = scratch("interval");
        for i in 0..4 {
            entry(&dir, &format!("e{i}"), 10, 100 - i);
        }
        let limits = Limits { files: 2, bytes: u64::MAX };
        // Just cleaned: not due
        std::fs::write(dir.join(LAST_CLEANUP), b"").unwrap();
        maybe_clean(&dir, Duration::from_secs(3600), limits);
        std::thread::sleep(Duration::from_millis(200));
        assert_eq!(names(&dir).len(), 4);
        // Due
        maybe_clean(&dir, Duration::from_secs(0), limits);
        for _ in 0..50 {
            if names(&dir).len() == 1 {
                break;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        assert_eq!(names(&dir), ["e3"]);
        // Done: the marker says when it finished
        for _ in 0..50 {
            if modified(&dir.join(LAST_CLEANUP)).is_some_and(|t| t.elapsed().is_ok_and(|e| e.as_secs() < 5)) {
                break;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        assert!(modified(&dir.join(LAST_CLEANUP)).is_some());
        let _ = std::fs::remove_dir_all(&dir);
    }
}
