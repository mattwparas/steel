use abi_stable::{std_types::RVec, DynTrait};
use steel::{
    rvals::Custom,
    steel_vm::ffi::{DynReader, DynWriter, FFIModule, RegisterFFIFn},
};

use std::sync::Mutex;
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
        let mut config = rustls::ClientConfig::builder()
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

#[derive(Debug)]
enum RustlsError {
    Io(std::io::Error),
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
        .register_fn("tcp-reader", tcp_reader)
        .register_fn("tcp-writer", tcp_writer)
        .register_fn("client-connection", RustlsClientConnection::new)
        .register_fn("tls-stream", RustlsStream::new)
        .register_fn("tls-reader", tls_tcp_reader)
        .register_fn("tls-writer", tls_tcp_writer)
        .register_fn("gz-decode", gz_decoder);

    module
}
