use abi_stable::{std_types::RVec, DynTrait};
use steel::{
    rvals::Custom,
    steel_vm::ffi::{DynReader, DynWriter, FFIModule, RegisterFFIFn},
};

use std::sync::Mutex;
use std::time::Duration;
use std::{io::Cursor, sync::Arc};

use rustls::RootCertStore;

struct RustlsClientConnection(Option<rustls::ClientConnection>);
impl Custom for RustlsClientConnection {}

#[derive(Clone)]
struct RustlsStream(Arc<Mutex<rustls::StreamOwned<rustls::ClientConnection, TcpStream>>>);
impl Custom for RustlsStream {}

impl std::io::Write for RustlsStream {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.lock().unwrap().flush()
    }
}

impl std::io::Read for RustlsStream {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().read(buf)
    }
}

impl RustlsClientConnection {
    pub fn new(server_name: String) -> Self {
        let root_store = RootCertStore {
            roots: webpki_roots::TLS_SERVER_ROOTS.into(),
        };
        // The provider is named rather than inferred from the crate features:
        // `ClientConfig::builder` looks at which of rustls' `aws-lc-rs` and
        // `ring` features are on and panics when both are, which happens as soon
        // as anything else in the build turns on the other - `ureq` does.
        let provider = Arc::new(rustls::crypto::aws_lc_rs::default_provider());
        let mut config = rustls::ClientConfig::builder_with_provider(provider)
            .with_safe_default_protocol_versions()
            .unwrap()
            .with_root_certificates(root_store)
            .with_no_client_auth();

        // Allow using SSLKEYLOGFILE.
        config.key_log = Arc::new(rustls::KeyLogFile::new());

        RustlsClientConnection(Some(
            rustls::ClientConnection::new(Arc::new(config), server_name.try_into().unwrap())
                .unwrap(),
        ))
    }
}

impl RustlsStream {
    // Pass the new stream in...
    pub fn new(client_connection: &mut RustlsClientConnection, stream: RustlsTcpStream) -> Self {
        Self(Arc::new(Mutex::new(rustls::StreamOwned::new(
            client_connection.0.take().unwrap(),
            stream.0,
        ))))
    }
}

steel::declare_module!(build_module);

use std::net::TcpStream;

struct RustlsTcpStream(TcpStream);
impl Custom for RustlsTcpStream {}

impl Clone for RustlsTcpStream {
    fn clone(&self) -> Self {
        RustlsTcpStream(self.0.try_clone().unwrap())
    }
}

enum RustlsError {
    Io(std::io::Error),
    Argument(String),
}

// Written out rather than derived because the ffi boundary formats an error with
// `{:?}`, so this is the form that reaches a steel program.
impl std::fmt::Debug for RustlsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RustlsError::Io(err) => write!(f, "rustls: {}", err),
            RustlsError::Argument(message) => write!(f, "{}", message),
        }
    }
}

impl std::fmt::Display for RustlsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for RustlsError {}

impl Custom for RustlsError {}

fn connect(addr: &str) -> Result<RustlsTcpStream, RustlsError> {
    TcpStream::connect(addr)
        .map(RustlsTcpStream)
        .map_err(RustlsError::Io)
}

// The deadline goes on the socket rather than on the tls stream above it, so it
// covers the handshake: that is where a server which accepts the connection and
// then says nothing would otherwise hold the calling thread. `tls-stream` moves
// this socket in, and a deadline belongs to the socket, so it stays in force.
fn set_read_timeout(stream: &mut RustlsTcpStream, ms: Option<isize>) -> Result<(), RustlsError> {
    stream
        .0
        .set_read_timeout(read_timeout(ms)?)
        .map_err(RustlsError::Io)
}

fn read_timeout(ms: Option<isize>) -> Result<Option<Duration>, RustlsError> {
    match ms {
        None => Ok(None),
        // A zero timeout means "no timeout at all" to the os, so it is refused
        // rather than turned into waiting forever.
        Some(ms) if ms > 0 => Ok(Some(Duration::from_millis(ms as u64))),
        Some(ms) => Err(RustlsError::Argument(format!(
            "tcp-set-read-timeout!: expected a positive number of milliseconds, found {}",
            ms
        ))),
    }
}

fn tcp_reader(stream: &RustlsTcpStream) -> DynReader {
    DynReader {
        reader: DynTrait::from_value(stream.0.try_clone().unwrap()),
    }
}

fn tcp_writer(stream: &RustlsTcpStream) -> DynWriter {
    DynWriter {
        writer: DynTrait::from_value(stream.0.try_clone().unwrap()),
    }
}

fn tls_tcp_reader(stream: &RustlsStream) -> DynReader {
    DynReader {
        reader: DynTrait::from_value(stream.clone()),
    }
}

fn tls_tcp_writer(stream: &RustlsStream) -> DynWriter {
    DynWriter {
        writer: DynTrait::from_value(stream.clone()),
    }
}

fn gz_decoder(buf: RVec<u8>) -> DynReader {
    DynReader {
        reader: DynTrait::from_value(flate2::read::GzDecoder::new(Cursor::new(buf))),
    }
}

pub fn build_module() -> FFIModule {
    let mut module = FFIModule::new("steel/rustls");

    module
        .register_fn("tcp-connect", connect)
        .register_fn("tcp-set-read-timeout!", set_read_timeout)
        .register_fn("tcp-reader", tcp_reader)
        .register_fn("tcp-writer", tcp_writer)
        .register_fn("client-connection", RustlsClientConnection::new)
        .register_fn("tls-stream", RustlsStream::new)
        .register_fn("tls-reader", tls_tcp_reader)
        .register_fn("tls-writer", tls_tcp_writer)
        .register_fn("gz-decode", gz_decoder);

    module
}

#[cfg(test)]
mod rustls_tests {
    use super::*;
    use std::net::TcpListener;

    fn connected() -> (RustlsTcpStream, TcpStream) {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let client = TcpStream::connect(listener.local_addr().unwrap()).unwrap();
        let (server, _) = listener.accept().unwrap();

        (RustlsTcpStream(server), client)
    }

    #[test]
    fn the_module_hands_out_the_read_timeout() {
        assert!(build_module()
            .bindings()
            .iter()
            .any(|name| name == "tcp-set-read-timeout!"));
    }

    #[test]
    fn a_timeout_in_milliseconds_reaches_the_socket() {
        let (mut stream, _client) = connected();

        set_read_timeout(&mut stream, Some(250)).unwrap();

        assert_eq!(
            stream.0.read_timeout().unwrap(),
            Some(Duration::from_millis(250))
        );
    }

    #[test]
    fn a_timeout_survives_being_moved_into_a_tls_stream() {
        let (mut stream, _client) = connected();

        set_read_timeout(&mut stream, Some(250)).unwrap();

        let mut connection = RustlsClientConnection::new("example.com".to_string());
        let tls = RustlsStream::new(&mut connection, stream);

        assert_eq!(
            tls.0.lock().unwrap().sock.read_timeout().unwrap(),
            Some(Duration::from_millis(250)),
            "the deadline did not follow the socket into the tls stream"
        );
    }

    #[test]
    fn a_false_timeout_goes_back_to_waiting_forever() {
        let (mut stream, _client) = connected();

        set_read_timeout(&mut stream, Some(250)).unwrap();
        set_read_timeout(&mut stream, None).unwrap();

        assert_eq!(stream.0.read_timeout().unwrap(), None);
    }

    #[test]
    fn a_timeout_that_is_not_a_wait_is_refused() {
        let (mut stream, _client) = connected();

        for ms in [0, -1] {
            assert!(
                set_read_timeout(&mut stream, Some(ms)).is_err(),
                "{ms} was taken as a read timeout"
            );
        }

        assert_eq!(
            stream.0.read_timeout().unwrap(),
            None,
            "a refused timeout still reached the socket"
        );
    }

    #[test]
    fn an_error_reads_as_a_sentence_rather_than_a_struct() {
        let err = set_read_timeout(&mut connected().0, Some(0)).unwrap_err();

        assert!(
            format!("{:?}", err).starts_with("tcp-set-read-timeout!:"),
            "the ffi boundary would show a steel program {:?}",
            err
        );

        assert_eq!(
            format!("{:?}", RustlsError::Io(std::io::Error::other("no route"))),
            "rustls: no route"
        );
    }
}
