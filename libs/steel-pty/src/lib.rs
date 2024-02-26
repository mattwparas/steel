//! This is a conceptually simple example that spawns the `whoami` program
//! to print your username.  It is made more complex because there are multiple
//! pipes involved and it is easy to get blocked/deadlocked if care and attention
//! is not paid to those pipes!
use abi_stable::std_types::{
    RBoxError,
    RResult::{self},
    RVec,
};
// use ansi_parser::AnsiParser;
// use ansitok::{parse_ansi, AnsiIterator, Element};
use portable_pty::{Child, CommandBuilder, NativePtySystem, PtyPair, PtySize, PtySystem};
use std::{
    io::Read,
    sync::{
        mpsc::{channel, Sender},
        Arc,
    },
    thread::JoinHandle,
};
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{FFIModule, FFIValue, FfiFuture, FfiFutureExt, IntoFFIVal, RegisterFFIFn},
};
use termwiz::escape::{parser::Parser, Action};

use tokio::sync::mpsc;

struct PtyProcess {
    cancellation_token_sender: Sender<()>,
    command_sender: Sender<String>,
    async_receiver: Arc<Mutex<mpsc::UnboundedReceiver<String>>>,
    _pty_system: PtyPair,
    child: Box<dyn Child + Send + Sync>,
    listener: Option<JoinHandle<()>>,
}

use futures::{lock::Mutex, FutureExt};

struct AnsiEscapeParser(termwiz::escape::parser::Parser);

#[derive(Clone)]
struct TermAction(Action);

impl Custom for TermAction {}

pub fn action_to_ffi_value(action: Action) -> FFIValue {
    match action {
        Action::Print(c) => FFIValue::CharV { c },
        Action::PrintString(s) => FFIValue::StringV(s.into()),
        Action::Control(c) => match c {
            // termwiz::escape::ControlCode::Null => todo!(),
            // termwiz::escape::ControlCode::StartOfHeading => todo!(),
            // termwiz::escape::ControlCode::StartOfText => todo!(),
            // termwiz::escape::ControlCode::EndOfText => todo!(),
            // termwiz::escape::ControlCode::EndOfTransmission => todo!(),
            // termwiz::escape::ControlCode::Enquiry => todo!(),
            // termwiz::escape::ControlCode::Acknowledge => todo!(),
            // termwiz::escape::ControlCode::Bell => todo!(),
            // termwiz::escape::ControlCode::Backspace => todo!(),
            // termwiz::escape::ControlCode::HorizontalTab => todo!(),
            termwiz::escape::ControlCode::LineFeed => FFIValue::CharV { c: '\n' },
            // termwiz::escape::ControlCode::VerticalTab => todo!(),
            // termwiz::escape::ControlCode::FormFeed => todo!(),
            termwiz::escape::ControlCode::CarriageReturn => FFIValue::CharV { c: '\r' },
            // termwiz::escape::ControlCode::ShiftOut => todo!(),
            // termwiz::escape::ControlCode::ShiftIn => todo!(),
            // termwiz::escape::ControlCode::DataLinkEscape => todo!(),
            // termwiz::escape::ControlCode::DeviceControlOne => todo!(),
            // termwiz::escape::ControlCode::DeviceControlTwo => todo!(),
            // termwiz::escape::ControlCode::DeviceControlThree => todo!(),
            // termwiz::escape::ControlCode::DeviceControlFour => todo!(),
            // termwiz::escape::ControlCode::NegativeAcknowledge => todo!(),
            // termwiz::escape::ControlCode::SynchronousIdle => todo!(),
            // termwiz::escape::ControlCode::EndOfTransmissionBlock => todo!(),
            // termwiz::escape::ControlCode::Cancel => todo!(),
            // termwiz::escape::ControlCode::EndOfMedium => todo!(),
            // termwiz::escape::ControlCode::Substitute => todo!(),
            // termwiz::escape::ControlCode::Escape => todo!(),
            // termwiz::escape::ControlCode::FileSeparator => todo!(),
            // termwiz::escape::ControlCode::GroupSeparator => todo!(),
            // termwiz::escape::ControlCode::RecordSeparator => todo!(),
            // termwiz::escape::ControlCode::UnitSeparator => todo!(),
            // termwiz::escape::ControlCode::BPH => todo!(),
            // termwiz::escape::ControlCode::NBH => todo!(),
            // termwiz::escape::ControlCode::IND => todo!(),
            // termwiz::escape::ControlCode::NEL => todo!(),
            // termwiz::escape::ControlCode::SSA => todo!(),
            // termwiz::escape::ControlCode::ESA => todo!(),
            // termwiz::escape::ControlCode::HTS => todo!(),
            // termwiz::escape::ControlCode::HTJ => todo!(),
            // termwiz::escape::ControlCode::VTS => todo!(),
            // termwiz::escape::ControlCode::PLD => todo!(),
            // termwiz::escape::ControlCode::PLU => todo!(),
            // termwiz::escape::ControlCode::RI => todo!(),
            // termwiz::escape::ControlCode::SS2 => todo!(),
            // termwiz::escape::ControlCode::SS3 => todo!(),
            // termwiz::escape::ControlCode::DCS => todo!(),
            // termwiz::escape::ControlCode::PU1 => todo!(),
            // termwiz::escape::ControlCode::PU2 => todo!(),
            // termwiz::escape::ControlCode::STS => todo!(),
            // termwiz::escape::ControlCode::CCH => todo!(),
            // termwiz::escape::ControlCode::MW => todo!(),
            // termwiz::escape::ControlCode::SPA => todo!(),
            // termwiz::escape::ControlCode::EPA => todo!(),
            // termwiz::escape::ControlCode::SOS => todo!(),
            // termwiz::escape::ControlCode::SCI => todo!(),
            // termwiz::escape::ControlCode::CSI => todo!(),
            // termwiz::escape::ControlCode::ST => todo!(),
            // termwiz::escape::ControlCode::OSC => todo!(),
            // termwiz::escape::ControlCode::PM => todo!(),
            // termwiz::escape::ControlCode::APC => todo!(),
            _ => TermAction(action).into_ffi_val().unwrap(),
        },
        // Action::DeviceControl(_) => todo!(),
        // Action::OperatingSystemCommand(_) => todo!(),
        Action::CSI(csi) => match &csi {
            // termwiz::escape::CSI::Sgr(_) => todo!(),
            termwiz::escape::CSI::Cursor(c) => match c {
                // termwiz::escape::csi::Cursor::BackwardTabulation(_) => todo!(),
                // termwiz::escape::csi::Cursor::TabulationClear(_) => todo!(),
                // termwiz::escape::csi::Cursor::CharacterAbsolute(_) => todo!(),
                // termwiz::escape::csi::Cursor::CharacterPositionAbsolute(_) => todo!(),
                // termwiz::escape::csi::Cursor::CharacterPositionBackward(_) => FFIValue::IntV(),
                // termwiz::escape::csi::Cursor::CharacterPositionForward(_) => todo!(),
                // termwiz::escape::csi::Cursor::CharacterAndLinePosition { line, col } => todo!(),
                // termwiz::escape::csi::Cursor::LinePositionAbsolute(_) => todo!(),
                // termwiz::escape::csi::Cursor::LinePositionBackward(_) => todo!(),
                // termwiz::escape::csi::Cursor::LinePositionForward(_) => todo!(),
                // termwiz::escape::csi::Cursor::ForwardTabulation(_) => todo!(),
                // termwiz::escape::csi::Cursor::NextLine(_) => todo!(),
                // termwiz::escape::csi::Cursor::PrecedingLine(_) => todo!(),
                // termwiz::escape::csi::Cursor::ActivePositionReport { line, col } => todo!(),
                // termwiz::escape::csi::Cursor::RequestActivePositionReport => todo!(),
                // termwiz::escape::csi::Cursor::SaveCursor => todo!(),
                // termwiz::escape::csi::Cursor::RestoreCursor => todo!(),
                // termwiz::escape::csi::Cursor::TabulationControl(_) => todo!(),
                // termwiz::escape::csi::Cursor::Left(_) => todo!(),
                // termwiz::escape::csi::Cursor::Down(_) => todo!(),
                // termwiz::escape::csi::Cursor::Right(_) => todo!(),
                // termwiz::escape::csi::Cursor::Position { line, col } => todo!(),
                // termwiz::escape::csi::Cursor::Up(_) => todo!(),
                // termwiz::escape::csi::Cursor::LineTabulation(_) => todo!(),
                // termwiz::escape::csi::Cursor::SetTopAndBottomMargins { top, bottom } => todo!(),
                // termwiz::escape::csi::Cursor::SetLeftAndRightMargins { left, right } => todo!(),
                // termwiz::escape::csi::Cursor::CursorStyle(_) => todo!(),
                _ => TermAction(Action::CSI(csi)).into_ffi_val().unwrap(),
            },
            termwiz::escape::CSI::Edit(e) => match &e {
                // termwiz::escape::csi::Edit::DeleteCharacter(_) => todo!(),
                // termwiz::escape::csi::Edit::DeleteLine(_) => todo!(),
                // termwiz::escape::csi::Edit::EraseCharacter(_) => todo!(),
                termwiz::escape::csi::Edit::EraseInLine(e) => match e {
                    termwiz::escape::csi::EraseInLine::EraseToEndOfLine => FFIValue::IntV(0),
                    termwiz::escape::csi::EraseInLine::EraseToStartOfLine => FFIValue::IntV(1),
                    termwiz::escape::csi::EraseInLine::EraseLine => FFIValue::IntV(2),
                },
                // termwiz::escape::csi::Edit::InsertCharacter(_) => todo!(),
                // termwiz::escape::csi::Edit::InsertLine(_) => todo!(),
                // termwiz::escape::csi::Edit::ScrollDown(_) => todo!(),
                // termwiz::escape::csi::Edit::ScrollUp(_) => todo!(),
                // termwiz::escape::csi::Edit::EraseInDisplay(_) => todo!(),
                // termwiz::escape::csi::Edit::Repeat(_) => todo!(),
                _ => TermAction(Action::CSI(csi)).into_ffi_val().unwrap(),
            },
            // termwiz::escape::CSI::Mode(_) => todo!(),
            // termwiz::escape::CSI::Device(_) => todo!(),
            // termwiz::escape::CSI::Mouse(_) => todo!(),
            // termwiz::escape::CSI::Window(_) => todo!(),
            // termwiz::escape::CSI::Keyboard(_) => todo!(),
            // termwiz::escape::CSI::SelectCharacterPath(_, _) => todo!(),
            // termwiz::escape::CSI::Unspecified(_) => todo!(),
            _ => TermAction(Action::CSI(csi)).into_ffi_val().unwrap(),
        },
        Action::Esc(esc) => match esc {
            termwiz::escape::Esc::Unspecified {
                intermediate: _,
                control: _,
            } => FFIValue::Void,
            termwiz::escape::Esc::Code(c) => match c {
                // termwiz::escape::EscCode::FullReset => todo!(),
                // termwiz::escape::EscCode::Index => todo!(),
                termwiz::escape::EscCode::NextLine => FFIValue::CharV { c: '\n' },
                // termwiz::escape::EscCode::CursorPositionLowerLeft => todo!(),
                // termwiz::escape::EscCode::HorizontalTabSet => todo!(),
                // termwiz::escape::EscCode::ReverseIndex => todo!(),
                // termwiz::escape::EscCode::SingleShiftG2 => todo!(),
                // termwiz::escape::EscCode::SingleShiftG3 => todo!(),
                // termwiz::escape::EscCode::StartOfGuardedArea => todo!(),
                // termwiz::escape::EscCode::EndOfGuardedArea => todo!(),
                // termwiz::escape::EscCode::StartOfString => todo!(),
                // termwiz::escape::EscCode::ReturnTerminalId => todo!(),
                // termwiz::escape::EscCode::StringTerminator => todo!(),
                // termwiz::escape::EscCode::PrivacyMessage => todo!(),
                // termwiz::escape::EscCode::ApplicationProgramCommand => todo!(),
                // termwiz::escape::EscCode::TmuxTitle => todo!(),
                // termwiz::escape::EscCode::DecBackIndex => todo!(),
                // termwiz::escape::EscCode::DecSaveCursorPosition => todo!(),
                // termwiz::escape::EscCode::DecRestoreCursorPosition => todo!(),
                // termwiz::escape::EscCode::DecApplicationKeyPad => todo!(),
                // termwiz::escape::EscCode::DecNormalKeyPad => todo!(),
                // termwiz::escape::EscCode::DecLineDrawingG0 => todo!(),
                // termwiz::escape::EscCode::UkCharacterSetG0 => todo!(),
                // termwiz::escape::EscCode::AsciiCharacterSetG0 => todo!(),
                // termwiz::escape::EscCode::DecLineDrawingG1 => todo!(),
                // termwiz::escape::EscCode::UkCharacterSetG1 => todo!(),
                // termwiz::escape::EscCode::AsciiCharacterSetG1 => todo!(),
                // termwiz::escape::EscCode::DecScreenAlignmentDisplay => todo!(),
                // termwiz::escape::EscCode::DecDoubleHeightTopHalfLine => todo!(),
                // termwiz::escape::EscCode::DecDoubleHeightBottomHalfLine => todo!(),
                // termwiz::escape::EscCode::DecSingleWidthLine => todo!(),
                // termwiz::escape::EscCode::DecDoubleWidthLine => todo!(),
                // termwiz::escape::EscCode::ApplicationModeArrowUpPress => todo!(),
                // termwiz::escape::EscCode::ApplicationModeArrowDownPress => todo!(),
                // termwiz::escape::EscCode::ApplicationModeArrowRightPress => todo!(),
                // termwiz::escape::EscCode::ApplicationModeArrowLeftPress => todo!(),
                // termwiz::escape::EscCode::ApplicationModeHomePress => todo!(),
                // termwiz::escape::EscCode::ApplicationModeEndPress => todo!(),
                // termwiz::escape::EscCode::F1Press => todo!(),
                // termwiz::escape::EscCode::F2Press => todo!(),
                // termwiz::escape::EscCode::F3Press => todo!(),
                // termwiz::escape::EscCode::F4Press => todo!(),
                _ => TermAction(Action::Esc(esc)).into_ffi_val().unwrap(),
            },
        },
        // Action::Sixel(_) => todo!(),
        // Action::XtGetTcap(_) => todo!(),
        // Action::KittyImage(_) => todo!(),
        _ => TermAction(action).into_ffi_val().unwrap(),
    }
}

impl AnsiEscapeParser {
    pub fn new() -> Self {
        Self(Parser::new())
    }

    pub fn tokenize_ansi(&mut self, text: String) -> RVec<FFIValue> {
        let mut result = RVec::new();

        self.0.parse(text.as_bytes(), |action| {
            result.push(action_to_ffi_value(action))
        });

        result
    }
}

impl Custom for AnsiEscapeParser {}

impl PtyProcess {
    pub fn kill(&mut self) {
        self.child.kill().ok();
        self.cancellation_token_sender.send(()).ok();
    }

    pub fn send_command(&mut self, command: String) {
        self.command_sender.send(command).unwrap();
    }

    pub fn async_try_read_line(&mut self) -> FfiFuture<RResult<FFIValue, RBoxError>> {
        let ar = Arc::clone(&self.async_receiver);

        async move {
            ar.lock()
                .await
                .recv()
                .map(|x| {
                    if let Some(message) = x {
                        RResult::ROk(FFIValue::StringV(message.into()))
                    } else {
                        RResult::ROk(FFIValue::BoolV(false))
                    }
                })
                .await
        }
        .into_ffi()
    }
}

impl Custom for PtyProcess {}

impl Drop for PtyProcess {
    fn drop(self: &mut PtyProcess) {
        self.kill();
    }
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/pty-process");

    module
        .register_fn("create-native-pty-system!", create_native_pty_system)
        .register_fn("kill-pty-process!", PtyProcess::kill)
        .register_fn("pty-process-send-command", PtyProcess::send_command)
        // .register_fn("pty-process-try-read-line", PtyProcess::try_read_line)
        .register_fn("async-try-read-line", PtyProcess::async_try_read_line)
        .register_fn("make-ansi-tokenizer", AnsiEscapeParser::new)
        .register_fn("tokenize-line", AnsiEscapeParser::tokenize_ansi)
        .register_fn("action->string", |action: &TermAction| {
            format!("{:?}", action.0)
        });

    module
}

fn create_native_pty_system() -> PtyProcess {
    let pty_system = NativePtySystem::default();

    let pair = pty_system
        .openpty(PtySize {
            rows: 24,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let cmd = CommandBuilder::new("/bin/bash");
    let child = pair.slave.spawn_command(cmd).unwrap();

    // Release any handles owned by the slave: we don't need it now
    // that we've spawned the child.
    // drop(pair.slave);

    // Read the output in another thread.
    // This is important because it is easy to encounter a situation
    // where read/write buffers fill and block either your process
    // or the spawned process.
    // let (tx, rx) = channel();
    let (async_sender, async_receiver) = mpsc::unbounded_channel();

    let (cancellation_token_sender, cancellation_token_receiver) = channel::<()>();

    let mut reader = pair.master.try_clone_reader().unwrap();
    let mut writer = pair.master.take_writer().unwrap();

    let listener = std::thread::spawn(move || {
        // Consume the output from the child

        // let mut bufreader = BufReader::new(reader);

        let mut read_buffer = [0; 65536];

        loop {
            // println!("Reading stuff");

            if let Ok(size) = reader.read(&mut read_buffer) {
                if size != 0 {
                    // if let Ok(escaped) = strip_ansi_escapes::strip(&read_buffer[..size]) {
                    // if let Ok(back) = String::from_utf8_lossy(&read_buffer[..size]) {
                    let r = async_sender.send(String::from_utf8_lossy(&read_buffer[..size]).into());

                    if r.is_err() {
                        break;
                    }
                }
            } else {
                break;
            }

            match cancellation_token_receiver.try_recv() {
                Ok(_) => break,
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(std::sync::mpsc::TryRecvError::Disconnected) => break,
            }
        }

        // println!("Finished");
    });

    let (command_sender, command_receiver) = channel::<String>();

    {
        // Obtain the writer.
        // When the writer is dropped, EOF will be sent to
        // the program that was spawned.
        // It is important to take the writer even if you don't
        // send anything to its stdin so that EOF can be
        // generated, otherwise you risk deadlocking yourself.
        // let mut writer = pair.master.take_writer().unwrap();

        if cfg!(target_os = "macos") {
            // macOS quirk: the child and reader must be started and
            // allowed a brief grace period to run before we allow
            // the writer to drop. Otherwise, the data we send to
            // the kernel to trigger EOF is interleaved with the
            // data read by the reader! WTF!?
            // This appears to be a race condition for very short
            // lived processes on macOS.
            // I'd love to find a more deterministic solution to
            // this than sleeping.
            std::thread::sleep(std::time::Duration::from_millis(20));
        }

        // This example doesn't need to write anything, but if you
        // want to send data to the child, you'd set `to_write` to
        // that data and do it like this:
        // let to_write = "ls -l";
        // if !to_write.is_empty() {
        // To avoid deadlock, wrt. reading and waiting, we send
        // data to the stdin of the child in a different thread.
        std::thread::spawn(move || loop {
            while let Ok(command) = command_receiver.recv() {
                writer.write_all(command.as_bytes()).unwrap();
            }

            break;
        });
        // }
    }

    // Take care to drop the master after our processes are
    // done, as some platforms get unhappy if it is dropped
    // sooner than that.
    // drop(pair.master);

    PtyProcess {
        cancellation_token_sender,
        command_sender,
        // output_receiver: rx,
        async_receiver: Arc::new(Mutex::new(async_receiver)),
        _pty_system: pair,
        child,
        listener: Some(listener),
    }

    // Now wait for the xxxxxx to be read by our reader thread
    // let output = rx.recv().unwrap();

    // for line in rx {
    //     print!("{}", line);
    // }

    // We print with escapes escaped because the windows conpty
    // implementation synthesizes title change escape sequences
    // in the output stream and it can be confusing to see those
    // printed out raw in another terminal.
    // print!("output: ");
    // TODO: Include this back when rendering to the built in terminal
    // for c in output.escape_debug() {
    //     print!("{}", c);
    // }

    // print!("{}", output);

    // for c in output {
    // print!("{}", c);
    // }
}
