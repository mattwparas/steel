use crate::{
    gc::{Gc, ShareableMut},
    rerrs::ErrorKind,
    rvals::{AsRefSteelVal, Custom, IntoSteelVal, SteelByteVector, SteelHashMap, SteelString},
    steel_vm::builtin::BuiltInModule,
    SteelErr, SteelVal,
};

use crate::rvals::Result;

pub struct Header {
    pub name: String,
    pub value: Vec<u8>,
}

pub struct SteelRequest {
    method: SteelString,
    path: SteelString,
    version: SteelString,
    // Offset into the buffer where the body starts
    body_offset: usize,
    // Probably just... push down directly into a hashmap?
    // or keep it some kind of key value pair store?
    headers: Vec<Header>,
}

impl Custom for SteelRequest {}

pub struct SteelResponse {
    pub version: u8,
    /// The response code, such as `200`.
    pub code: u16,
    /// The response reason-phrase, such as `OK`.
    ///
    /// Contains an empty string if the reason-phrase was missing or contained invalid characters.
    pub reason: String,
    /// The response headers.
    pub headers: Vec<Header>,

    pub body_offset: usize,
}

impl Custom for SteelResponse {}

/// Returns the HTTP method (for example `"GET"` or `"POST"`) of a parsed
/// HTTP request.
///
/// (http-request-method request) -> string?
///
/// * request : http-request?
#[steel_derive::function(name = "http-request-method")]
pub fn method(value: &SteelVal) -> Result<SteelVal> {
    SteelRequest::as_ref(value)
        .map(|x| x.method.clone())
        .map(SteelVal::StringV)
}

/// Returns the request path of a parsed HTTP request.
///
/// (http-request-path request) -> string?
///
/// * request : http-request?
#[steel_derive::function(name = "http-request-path")]
pub fn path(value: &SteelVal) -> Result<SteelVal> {
    SteelRequest::as_ref(value)
        .map(|x| x.path.clone())
        .map(SteelVal::StringV)
}

/// Returns the HTTP version of a parsed HTTP request.
///
/// (http-request-version request) -> string?
///
/// * request : http-request?
#[steel_derive::function(name = "http-request-version")]
pub fn version(value: &SteelVal) -> Result<SteelVal> {
    SteelRequest::as_ref(value)
        .map(|x| x.version.clone())
        .map(SteelVal::StringV)
}

/// Download file from a URL
#[steel_derive::function(name = "download-file!")]
fn download_file(_url: &SteelString, _file: &SteelString) -> Result<SteelVal> {
    #[cfg(not(feature = "ureq"))]
    {
        Err(SteelErr::new(
            ErrorKind::BadSyntax,
            "download-file! is not implemented".to_string(),
        ))
    }

    #[cfg(feature = "ureq")]
    {
        use std::{fs, path::PathBuf};

        let url = _url.as_str();
        let file = PathBuf::from(_file.as_str());
        let contents = ureq::get(url)
            .call()
            .map_err(|err| {
                SteelErr::new(ErrorKind::Io, format!("failed to call http method: {err}"))
            })?
            .body_mut()
            .read_to_vec()
            .map_err(|err| SteelErr::new(ErrorKind::Io, format!("http request failed: {err}")))?;

        fs::write(file, contents)
            .map_err(|err| SteelErr::new(ErrorKind::Io, format!("failed to write: {err}")))?;

        Ok(().into())
    }
}

/// Returns the byte offset into the original buffer at which the body of a
/// parsed HTTP request begins.
///
/// (http-request-body-offset request) -> int?
///
/// * request : http-request?
#[steel_derive::function(name = "http-request-body-offset")]
pub fn body_offset(value: &SteelVal) -> Result<SteelVal> {
    SteelRequest::as_ref(value)
        .map(|x| x.body_offset as isize)
        .map(SteelVal::IntV)
}

/// Returns the headers of a parsed HTTP request as a hashmap mapping header
/// names (strings) to their values (byte vectors).
///
/// (http-request-headers request) -> hash?
///
/// * request : http-request?
#[steel_derive::function(name = "http-request-headers")]
pub fn headers(value: &SteelVal) -> Result<SteelVal> {
    let req = SteelRequest::as_ref(value)?;

    Ok(SteelVal::HashMapV(SteelHashMap(Gc::new(
        req.headers
            .iter()
            .map(|x| {
                (
                    SteelVal::StringV(x.name.clone().into()),
                    SteelVal::ByteVector(SteelByteVector::new(x.value.clone())),
                )
            })
            .collect::<crate::values::HashMap<_, _>>(),
    ))))
}

/// Returns the headers of a parsed HTTP response as a hashmap mapping header
/// names (strings) to their values (byte vectors).
///
/// (http-response-headers response) -> hash?
///
/// * response : http-response?
#[steel_derive::function(name = "http-response-headers")]
pub fn resp_headers(value: &SteelVal) -> Result<SteelVal> {
    let resp = SteelResponse::as_ref(value)?;

    Ok(SteelVal::HashMapV(SteelHashMap(Gc::new(
        resp.headers
            .iter()
            .map(|x| {
                (
                    SteelVal::StringV(x.name.clone().into()),
                    SteelVal::ByteVector(SteelByteVector::new(x.value.clone())),
                )
            })
            .collect::<crate::values::HashMap<_, _>>(),
    ))))
}

// If not complete, try again?
fn parse_request(buf: &[u8]) -> Result<SteelVal> {
    // Pull more bytes from the stream?
    let mut headers = [httparse::EMPTY_HEADER; 16];
    let mut req = httparse::Request::new(&mut headers);
    let res = req.parse(buf).unwrap();
    if res.is_complete() {
        let request = SteelRequest {
            method: req.method.unwrap().to_string().into(),
            path: req.path.unwrap().to_string().into(),
            version: req.version.unwrap().to_string().into(),
            body_offset: res.unwrap(),
            headers: headers
                .iter()
                .filter_map(|x| {
                    if *x != httparse::EMPTY_HEADER {
                        Some(Header {
                            name: x.name.to_string(),
                            value: x.value.to_vec(),
                        })
                    } else {
                        None
                    }
                })
                .collect(),
        };

        request.into_steelval()
    } else {
        Ok(SteelVal::BoolV(false))
    }
}

fn parse_response(buf: &[u8]) -> Result<SteelVal> {
    // Pull more bytes from the stream?
    let mut headers = [httparse::EMPTY_HEADER; 64];
    let mut req = httparse::Response::new(&mut headers);
    let res = req.parse(buf).unwrap();
    if res.is_complete() {
        let request = SteelResponse {
            version: req.version.unwrap(),
            code: req.code.unwrap(),
            reason: req.reason.unwrap().to_string(),
            body_offset: res.unwrap(),
            headers: headers
                .iter()
                .filter_map(|x| {
                    if *x != httparse::EMPTY_HEADER {
                        Some(Header {
                            name: x.name.to_string(),
                            value: x.value.to_vec(),
                        })
                    } else {
                        None
                    }
                })
                .collect(),
        };

        request.into_steelval()
    } else {
        Ok(SteelVal::BoolV(false))
    }
}

/// Parses an HTTP request out of the given byte vector. Returns a request
/// object on success, or `#false` if the buffer does not yet contain a
/// complete request.
///
/// (http-parse-request buf) -> (or http-request? #false)
///
/// * buf : bytes?
#[steel_derive::function(name = "http-parse-request")]
pub fn parse_http_request(vector: &SteelByteVector) -> Result<SteelVal> {
    parse_request(&vector.vec.read())
}

/// Parses an HTTP response out of the given byte vector. Returns a response
/// object on success, or `#false` if the buffer does not yet contain a
/// complete response.
///
/// (http-parse-response buf) -> (or http-response? #false)
///
/// * buf : bytes?
#[steel_derive::function(name = "http-parse-response")]
pub fn parse_http_response(vector: &SteelByteVector) -> Result<SteelVal> {
    parse_response(&vector.vec.read())
}

pub fn http_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/http".to_string());

    module
        .register_native_fn_definition(PARSE_HTTP_REQUEST_DEFINITION)
        .register_native_fn_definition(METHOD_DEFINITION)
        .register_native_fn_definition(VERSION_DEFINITION)
        .register_native_fn_definition(PATH_DEFINITION)
        .register_native_fn_definition(BODY_OFFSET_DEFINITION)
        .register_native_fn_definition(HEADERS_DEFINITION)
        .register_native_fn_definition(RESP_HEADERS_DEFINITION)
        .register_native_fn_definition(PARSE_HTTP_RESPONSE_DEFINITION)
        .register_native_fn_definition(DOWNLOAD_FILE_DEFINITION);

    // module
    //     .register_native_fn_definition(TCP_CONNECT_DEFINITION)
    //     .register_native_fn_definition(TCP_INPUT_PORT_DEFINITION)
    //     .register_native_fn_definition(TCP_OUTPUT_PORT_DEFINITION)
    //     .register_native_fn_definition(TCP_BUFFERED_OUTPUT_PORT_DEFINITION)
    //     .register_native_fn_definition(TCP_LISTEN_DEFINITION)
    //     .register_native_fn_definition(TCP_ACCEPT_DEFINITION);

    module
}
