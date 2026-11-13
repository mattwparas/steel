//! The platform layer under the line editor: raw mode, reading single bytes
//! with a timeout, the terminal's size, display widths and Ctrl-C. Everything
//! above this (decoding keys, editing, drawing) is meant to be Steel code.
//!
//! Backends: unix (termios through libc), Windows (console modes), and a
//! fallback for everything else, Hermit included, that only uses std. On the
//! fallback raw mode is a no-op, which is right for a serial console.
//!
//! Nothing in here depends on Steel; `#%private/steel/terminal` (in
//! repl.rs) is the Steel face of it.

use std::io;
use std::time::Duration;

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

pub enum Byte {
    Byte(u8),
    Timeout,
    Eof,
}

/// Whether stdin and stdout are both terminals
pub fn is_terminal() -> bool {
    use std::io::IsTerminal;

    io::stdin().is_terminal() && io::stdout().is_terminal()
}

/// Puts the terminal in raw mode: no echo, no line buffering, Ctrl-C and
/// Ctrl-Z arrive as bytes. Output processing stays on, so `\n` still starts a
/// new line. Calling it again while raw does nothing; the mode from before
/// the first call is what [`disable_raw_mode`] goes back to, and it's also
/// restored when the process exits.
pub fn enable_raw_mode() -> io::Result<()> {
    sys::enable_raw_mode()
}

pub fn disable_raw_mode() -> io::Result<()> {
    sys::disable_raw_mode()
}

pub fn is_raw_mode() -> bool {
    sys::is_raw_mode()
}

/// Reads one byte from stdin, waiting at most `timeout` (forever with
/// `None`). This bypasses std's buffered stdin, so don't mix the two while
/// an editor is reading.
pub fn read_byte(timeout: Option<Duration>) -> io::Result<Byte> {
    sys::read_byte(timeout)
}

/// (columns, rows), when the platform can tell
pub fn size() -> Option<(u16, u16)> {
    sys::size().filter(|&(columns, rows)| columns > 0 && rows > 0)
}

/// Columns `text` takes up on a terminal. `text` must not contain escape
/// sequences.
pub fn display_width(text: &str) -> usize {
    text.width()
}

/// `text` split into what the user sees as characters
pub fn graphemes(text: &str) -> impl Iterator<Item = &str> {
    text.graphemes(true)
}

/// Runs `callback` on a thread of its own whenever the user presses Ctrl-C
/// outside raw mode (SIGINT on unix, the console's Ctrl-C event on
/// Windows). Calling it again replaces the callback. Fails with
/// `Unsupported` where there are no such events, Hermit included.
pub fn install_interrupt_handler(callback: impl Fn() + Send + Sync + 'static) -> io::Result<()> {
    sys::install_interrupt_handler(Box::new(callback))
}

type Callback = Box<dyn Fn() + Send + Sync>;

#[cfg(any(unix, windows))]
static INTERRUPT_CALLBACK: std::sync::Mutex<Option<Callback>> = std::sync::Mutex::new(None);

#[cfg(any(unix, windows))]
fn run_interrupt_callback() {
    if let Some(callback) = INTERRUPT_CALLBACK.lock().unwrap().as_ref() {
        callback();
    }
}

#[cfg(unix)]
mod sys {
    use super::Byte;

    use std::io;
    use std::mem::MaybeUninit;
    use std::sync::atomic::{AtomicI32, Ordering};
    use std::sync::{Mutex, Once};
    use std::time::Duration;

    const STDIN: libc::c_int = libc::STDIN_FILENO;

    /// The mode from before raw mode, while raw
    static ORIGINAL: Mutex<Option<libc::termios>> = Mutex::new(None);

    fn check(result: libc::c_int) -> io::Result<libc::c_int> {
        if result == -1 {
            Err(io::Error::last_os_error())
        } else {
            Ok(result)
        }
    }

    extern "C" fn restore_at_exit() {
        let _ = disable_raw_mode();
    }

    pub fn enable_raw_mode() -> io::Result<()> {
        let mut original = ORIGINAL.lock().unwrap();
        if original.is_some() {
            return Ok(());
        }

        let mut termios = MaybeUninit::<libc::termios>::uninit();
        check(unsafe { libc::tcgetattr(STDIN, termios.as_mut_ptr()) })?;
        let saved = unsafe { termios.assume_init() };

        let mut raw = saved;
        raw.c_iflag &= !(libc::BRKINT | libc::ICRNL | libc::INPCK | libc::ISTRIP | libc::IXON);
        raw.c_cflag |= libc::CS8;
        raw.c_lflag &= !(libc::ECHO | libc::ICANON | libc::IEXTEN | libc::ISIG);
        raw.c_cc[libc::VMIN] = 1;
        raw.c_cc[libc::VTIME] = 0;
        check(unsafe { libc::tcsetattr(STDIN, libc::TCSADRAIN, &raw) })?;

        *original = Some(saved);

        static AT_EXIT: Once = Once::new();
        AT_EXIT.call_once(|| unsafe {
            libc::atexit(restore_at_exit);
        });

        Ok(())
    }

    pub fn disable_raw_mode() -> io::Result<()> {
        let mut original = ORIGINAL
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some(saved) = original.take() {
            check(unsafe { libc::tcsetattr(STDIN, libc::TCSADRAIN, &saved) })?;
        }
        Ok(())
    }

    pub fn is_raw_mode() -> bool {
        ORIGINAL.lock().unwrap().is_some()
    }

    pub fn read_byte(timeout: Option<Duration>) -> io::Result<Byte> {
        loop {
            if let Some(timeout) = timeout {
                let mut fd = libc::pollfd {
                    fd: STDIN,
                    events: libc::POLLIN,
                    revents: 0,
                };
                let ms = timeout.as_millis().min(libc::c_int::MAX as u128) as libc::c_int;
                match unsafe { libc::poll(&mut fd, 1, ms) } {
                    0 => return Ok(Byte::Timeout),
                    -1 => {
                        let err = io::Error::last_os_error();
                        if err.kind() == io::ErrorKind::Interrupted {
                            continue;
                        }
                        return Err(err);
                    }
                    _ => {}
                }
            }

            let mut byte = 0u8;
            match unsafe { libc::read(STDIN, (&mut byte as *mut u8).cast(), 1) } {
                1 => return Ok(Byte::Byte(byte)),
                0 => return Ok(Byte::Eof),
                _ => {
                    let err = io::Error::last_os_error();
                    if err.kind() == io::ErrorKind::Interrupted {
                        continue;
                    }
                    return Err(err);
                }
            }
        }
    }

    pub fn size() -> Option<(u16, u16)> {
        for fd in [libc::STDOUT_FILENO, STDIN] {
            let mut size = MaybeUninit::<libc::winsize>::zeroed();
            if unsafe { libc::ioctl(fd, libc::TIOCGWINSZ, size.as_mut_ptr()) } == 0 {
                let size = unsafe { size.assume_init() };
                return Some((size.ws_col, size.ws_row));
            }
        }
        None
    }

    /// Write end of the pipe the SIGINT handler pokes
    static INTERRUPT_PIPE: AtomicI32 = AtomicI32::new(-1);

    // Only async-signal-safe calls in here. The pipe is non-blocking, so a
    // burst of Ctrl-Cs can't wedge the handler.
    extern "C" fn on_sigint(_: libc::c_int) {
        let fd = INTERRUPT_PIPE.load(Ordering::Relaxed);
        if fd >= 0 {
            let byte = 1u8;
            unsafe {
                libc::write(fd, (&byte as *const u8).cast(), 1);
            }
        }
    }

    pub fn install_interrupt_handler(callback: super::Callback) -> io::Result<()> {
        *super::INTERRUPT_CALLBACK.lock().unwrap() = Some(callback);

        static INSTALL: Mutex<bool> = Mutex::new(false);
        let mut installed = INSTALL.lock().unwrap();
        if *installed {
            return Ok(());
        }

        let mut fds = [0 as libc::c_int; 2];
        check(unsafe { libc::pipe(fds.as_mut_ptr()) })?;
        let [read, write] = fds;
        for fd in fds {
            unsafe {
                let flags = libc::fcntl(fd, libc::F_GETFD);
                libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC);
            }
        }
        unsafe {
            let flags = libc::fcntl(write, libc::F_GETFL);
            libc::fcntl(write, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }

        std::thread::Builder::new()
            .name("steel-interrupt".to_owned())
            .spawn(move || loop {
                let mut byte = 0u8;
                match unsafe { libc::read(read, (&mut byte as *mut u8).cast(), 1) } {
                    1 => super::run_interrupt_callback(),
                    -1 if io::Error::last_os_error().kind() == io::ErrorKind::Interrupted => {}
                    _ => return,
                }
            })?;

        INTERRUPT_PIPE.store(write, Ordering::Relaxed);

        unsafe {
            let mut action: libc::sigaction = std::mem::zeroed();
            action.sa_sigaction = on_sigint as *const () as libc::sighandler_t;
            action.sa_flags = libc::SA_RESTART;
            libc::sigemptyset(&mut action.sa_mask);
            check(libc::sigaction(libc::SIGINT, &action, std::ptr::null_mut()))?;
        }

        *installed = true;
        Ok(())
    }
}

#[cfg(windows)]
mod sys {
    use super::Byte;

    use std::io;
    use std::sync::Mutex;
    use std::time::Duration;

    use windows_sys::Win32::Foundation::{BOOL, FALSE, HANDLE, INVALID_HANDLE_VALUE, TRUE};
    use windows_sys::Win32::System::Console::{
        GetConsoleMode, GetConsoleScreenBufferInfo, GetStdHandle, SetConsoleCtrlHandler,
        SetConsoleMode, CONSOLE_MODE, CONSOLE_SCREEN_BUFFER_INFO, CTRL_C_EVENT, ENABLE_ECHO_INPUT,
        ENABLE_LINE_INPUT, ENABLE_PROCESSED_INPUT, ENABLE_VIRTUAL_TERMINAL_INPUT,
        ENABLE_VIRTUAL_TERMINAL_PROCESSING, STD_INPUT_HANDLE, STD_OUTPUT_HANDLE,
    };

    /// (input mode, output mode) from before raw mode, while raw
    static ORIGINAL: Mutex<Option<(CONSOLE_MODE, CONSOLE_MODE)>> = Mutex::new(None);

    fn handle(which: u32) -> io::Result<HANDLE> {
        let handle = unsafe { GetStdHandle(which) };
        if handle == INVALID_HANDLE_VALUE || handle.is_null() {
            Err(io::Error::last_os_error())
        } else {
            Ok(handle)
        }
    }

    fn mode(handle: HANDLE) -> io::Result<CONSOLE_MODE> {
        let mut mode = 0;
        if unsafe { GetConsoleMode(handle, &mut mode) } == FALSE {
            return Err(io::Error::last_os_error());
        }
        Ok(mode)
    }

    fn set_mode(handle: HANDLE, mode: CONSOLE_MODE) -> io::Result<()> {
        if unsafe { SetConsoleMode(handle, mode) } == FALSE {
            return Err(io::Error::last_os_error());
        }
        Ok(())
    }

    pub fn enable_raw_mode() -> io::Result<()> {
        let mut original = ORIGINAL.lock().unwrap();
        if original.is_some() {
            return Ok(());
        }

        let input = handle(STD_INPUT_HANDLE)?;
        let output = handle(STD_OUTPUT_HANDLE)?;
        let saved = (mode(input)?, mode(output)?);

        // Keys come in as VT sequences, like on unix
        let raw_input = (saved.0
            & !(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT))
            | ENABLE_VIRTUAL_TERMINAL_INPUT;
        set_mode(input, raw_input)?;
        set_mode(output, saved.1 | ENABLE_VIRTUAL_TERMINAL_PROCESSING)?;

        *original = Some(saved);
        Ok(())
    }

    pub fn disable_raw_mode() -> io::Result<()> {
        let mut original = ORIGINAL
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some((input, output)) = original.take() {
            set_mode(handle(STD_INPUT_HANDLE)?, input)?;
            set_mode(handle(STD_OUTPUT_HANDLE)?, output)?;
        }
        Ok(())
    }

    pub fn is_raw_mode() -> bool {
        ORIGINAL.lock().unwrap().is_some()
    }

    pub fn read_byte(timeout: Option<Duration>) -> io::Result<Byte> {
        super::reader::read_byte(timeout)
    }

    pub fn size() -> Option<(u16, u16)> {
        let output = handle(STD_OUTPUT_HANDLE).ok()?;
        let mut info: CONSOLE_SCREEN_BUFFER_INFO = unsafe { std::mem::zeroed() };
        if unsafe { GetConsoleScreenBufferInfo(output, &mut info) } == FALSE {
            return None;
        }
        let window = info.srWindow;
        Some((
            (window.Right - window.Left + 1) as u16,
            (window.Bottom - window.Top + 1) as u16,
        ))
    }

    // Windows calls this on a thread of its own
    unsafe extern "system" fn on_ctrl(kind: u32) -> BOOL {
        if kind == CTRL_C_EVENT {
            super::run_interrupt_callback();
            TRUE
        } else {
            FALSE
        }
    }

    pub fn install_interrupt_handler(callback: super::Callback) -> io::Result<()> {
        *super::INTERRUPT_CALLBACK.lock().unwrap() = Some(callback);

        static INSTALL: Mutex<bool> = Mutex::new(false);
        let mut installed = INSTALL.lock().unwrap();
        if !*installed {
            if unsafe { SetConsoleCtrlHandler(Some(on_ctrl), TRUE) } == FALSE {
                return Err(io::Error::last_os_error());
            }
            *installed = true;
        }
        Ok(())
    }
}

#[cfg(not(any(unix, windows)))]
mod sys {
    use super::Byte;

    use std::io;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::time::Duration;

    // A serial console has no modes to switch, it's already raw
    static RAW: AtomicBool = AtomicBool::new(false);

    pub fn enable_raw_mode() -> io::Result<()> {
        RAW.store(true, Ordering::Relaxed);
        Ok(())
    }

    pub fn disable_raw_mode() -> io::Result<()> {
        RAW.store(false, Ordering::Relaxed);
        Ok(())
    }

    pub fn is_raw_mode() -> bool {
        RAW.load(Ordering::Relaxed)
    }

    pub fn read_byte(timeout: Option<Duration>) -> io::Result<Byte> {
        super::reader::read_byte(timeout)
    }

    pub fn size() -> Option<(u16, u16)> {
        None
    }

    pub fn install_interrupt_handler(_callback: super::Callback) -> io::Result<()> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "no interrupt events on this platform",
        ))
    }
}

/// Timed reads where std's stdin is all there is: a thread does the
/// blocking reads, one at a time and only when asked, so it never takes
/// input nobody wanted. After a timeout the read stays outstanding and the
/// next call picks up its byte.
#[cfg(not(unix))]
mod reader {
    use super::Byte;

    use std::io::{self, Read};
    use std::sync::mpsc::{channel, Receiver, RecvTimeoutError, Sender};
    use std::sync::Mutex;
    use std::time::Duration;

    struct Reader {
        requests: Sender<()>,
        bytes: Receiver<io::Result<Byte>>,
        pending: bool,
    }

    static READER: Mutex<Option<Reader>> = Mutex::new(None);

    fn spawn() -> io::Result<Reader> {
        let (requests, wanted) = channel::<()>();
        let (send, bytes) = channel();
        std::thread::Builder::new()
            .name("steel-stdin".to_owned())
            .spawn(move || {
                while wanted.recv().is_ok() {
                    let mut byte = [0u8];
                    let read = match io::stdin().lock().read(&mut byte) {
                        Ok(0) => Ok(Byte::Eof),
                        Ok(_) => Ok(Byte::Byte(byte[0])),
                        Err(err) => Err(err),
                    };
                    if send.send(read).is_err() {
                        return;
                    }
                }
            })?;
        Ok(Reader {
            requests,
            bytes,
            pending: false,
        })
    }

    pub fn read_byte(timeout: Option<Duration>) -> io::Result<Byte> {
        let mut guard = READER.lock().unwrap();
        if guard.is_none() {
            *guard = Some(spawn()?);
        }
        let reader = guard.as_mut().unwrap();

        if !reader.pending {
            reader
                .requests
                .send(())
                .map_err(|_| io::Error::other("stdin reader stopped"))?;
            reader.pending = true;
        }

        let received = match timeout {
            None => reader
                .bytes
                .recv()
                .map_err(|_| RecvTimeoutError::Disconnected),
            Some(timeout) => reader.bytes.recv_timeout(timeout),
        };

        match received {
            Ok(read) => {
                reader.pending = false;
                read
            }
            Err(RecvTimeoutError::Timeout) => Ok(Byte::Timeout),
            Err(RecvTimeoutError::Disconnected) => Err(io::Error::other("stdin reader stopped")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{display_width, graphemes};

    #[test]
    fn widths() {
        assert_eq!(display_width("hello"), 5);
        assert_eq!(display_width("λ > "), 4);
        // Wide characters take two columns, combining marks none
        assert_eq!(display_width("日本"), 4);
        assert_eq!(display_width("e\u{301}"), 1);
        assert_eq!(display_width(""), 0);
    }

    #[test]
    fn grapheme_split() {
        let text = "e\u{301}x👍🏽";
        let split: Vec<_> = graphemes(text).collect();
        assert_eq!(split, ["e\u{301}", "x", "👍🏽"]);
    }
}
