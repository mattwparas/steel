use abi_stable::std_types::{RBoxError, RResult};
use http::Request;
use isahc::{AsyncBody, AsyncReadResponseExt, RequestExt};
use steel::rvals::Custom;
use steel::steel_vm::ffi::{
    as_underlying_ffi_type, CustomRef, FFIArg, FFIValue, FfiFuture, FfiFutureExt, IntoFFIVal,
};
use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

use base64::prelude::*;

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/web/async/requests".to_string());

    module
        .register_fn("client/new", Client::new)
        .register_fn("client/send-async", Client::send_async)
        .register_fn("get", get)
        .register_fn("build-get", AsyncRequest::get)
        .register_fn("build-put", AsyncRequest::put)
        .register_fn("build-post", AsyncRequest::post)
        .register_fn("build-patch", AsyncRequest::patch)
        .register_fn("build-head", AsyncRequest::head)
        .register_fn("build-delete", AsyncRequest::delete)
        .register_fn("url-encode", |uri: &str| {
            urlencoding::encode(uri).to_string()
        })
        .register_fn("base-64-encode", |arg: &str| {
            BASE64_STANDARD.encode(arg.as_bytes())
        })
        .register_fn("url-safe-base-64-encode", |arg: &str| {
            base64::engine::general_purpose::URL_SAFE.encode(arg.as_bytes())
        })
        .register_fn("set-header!", AsyncRequest::set)
        .register_fn("body", AsyncRequest::body)
        .register_fn("response-header-ref", SteelResponse::get_header)
        .register_fn(
            "call-finalized",
            |request: &mut FinalizedAsyncRequest| -> FfiFuture<RResult<FFIValue, RBoxError>> {
                let inner = std::mem::take(&mut request.0);
                async move {
                    match inner {
                        Some(request) => {
                            let response = request.send_async().await;
                            match response {
                                Ok(response) => SteelResponse::from(response).into_ffi_val(),
                                Err(e) => RResult::RErr(RBoxError::new(e)),
                            }
                        }
                        None => RResult::RErr(RBoxError::new(AsyncError::RequestAlreadyUsed)),
                    }
                }
                .into_ffi()
            },
        )
        .register_fn(
            "call",
            |request: &mut AsyncRequest| -> FfiFuture<RResult<FFIValue, RBoxError>> {
                let inner = std::mem::take(&mut request.0);

                async move {
                    match inner.map(|x| x.body(String::new())) {
                        Some(Ok(request)) => {
                            let response = request.send_async().await;
                            match response {
                                Ok(response) => SteelResponse::from(response).into_ffi_val(),
                                Err(e) => RResult::RErr(RBoxError::new(e)),
                            }
                        }
                        Some(Err(e)) => RResult::RErr(RBoxError::new(e)),
                        None => RResult::RErr(RBoxError::new(AsyncError::RequestAlreadyUsed)),
                    }
                }
                .into_ffi()
            },
        )
        .register_fn("response->text", SteelResponse::into_text);

    module
}

struct AsyncRequest(Option<http::request::Builder>);
struct FinalizedAsyncRequest(Option<http::request::Request<String>>);
struct AsyncResponse(isahc::Response<String>);

#[derive(Clone)]
struct Client(isahc::HttpClient);

#[derive(Debug)]
pub enum AsyncError {
    Async(isahc::Error),
    RequestAlreadyUsed,
    ResponseAlreadyUsed,
    TypeMismatch(String),
}

impl std::fmt::Display for AsyncError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for AsyncError {}

impl Custom for AsyncRequest {}
impl Custom for FinalizedAsyncRequest {}
impl Custom for AsyncResponse {}
impl Custom for AsyncError {}
impl Custom for Client {}

impl Client {
    fn new() -> RResult<FFIValue, RBoxError> {
        match isahc::HttpClient::new() {
            Ok(client) => Self(client).into_ffi_val(),
            Err(e) => RResult::RErr(RBoxError::new(e)),
        }
    }

    fn send_async(&self, value: FFIArg) -> FfiFuture<RResult<FFIValue, RBoxError>> {
        if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = value {
            if let Some(request) = as_underlying_ffi_type::<FinalizedAsyncRequest>(custom.get_mut())
            {
                let request = request.0.take();
                // This can be cloned efficiently - it is just wrapping an Arc
                let longer_lived = self.0.clone();

                async move {
                    match request {
                        Some(request) => {
                            let response = longer_lived.send_async(request).await;

                            match response {
                                Ok(response) => SteelResponse::from(response).into_ffi_val(),
                                Err(e) => RResult::RErr(RBoxError::new(e)),
                            }
                        }
                        None => RResult::RErr(RBoxError::new(AsyncError::RequestAlreadyUsed)),
                    }
                }
                .into_ffi()
            } else {
                let message =
                    "Expected a value of type finalized request, found another opaque object"
                        .to_string();
                async move { RResult::RErr(RBoxError::new(AsyncError::TypeMismatch(message))) }
                    .into_ffi()
            }
        } else {
            async move { RResult::RErr(RBoxError::new(AsyncError::RequestAlreadyUsed)) }.into_ffi()
        }
    }
}

fn get(url: String) -> FfiFuture<RResult<FFIValue, RBoxError>> {
    async move {
        let result = isahc::get_async(&url).await;

        match result {
            Ok(response) => SteelResponse::from(response).into_ffi_val(),
            Err(e) => RResult::RErr(RBoxError::new(e)),
        }
    }
    .into_ffi()
}

impl AsyncRequest {
    fn set(&mut self, header: &str, value: &str) -> RResult<FFIValue, RBoxError> {
        let inner = self.0.take();

        match inner {
            Some(req) => {
                self.0 = Some(req.header(header, value));
                RResult::ROk(FFIValue::Void)
            }
            None => RResult::RErr(RBoxError::new(AsyncError::RequestAlreadyUsed)),
        }
    }

    fn get(url: &str) -> AsyncRequest {
        AsyncRequest(Some(Request::get(url)))
    }

    fn post(url: &str) -> AsyncRequest {
        AsyncRequest(Some(Request::post(url)))
    }

    fn put(url: &str) -> AsyncRequest {
        AsyncRequest(Some(Request::put(url)))
    }

    fn patch(url: &str) -> AsyncRequest {
        AsyncRequest(Some(Request::patch(url)))
    }

    fn head(url: &str) -> AsyncRequest {
        AsyncRequest(Some(Request::head(url)))
    }

    fn delete(url: &str) -> AsyncRequest {
        AsyncRequest(Some(Request::delete(url)))
    }

    fn body(&mut self, body: String) -> RResult<FFIValue, RBoxError> {
        let inner = self.0.take().map(|x| x.body(body));

        match inner {
            Some(Ok(inner)) => FinalizedAsyncRequest(Some(inner)).into_ffi_val(),
            Some(Err(e)) => RResult::RErr(RBoxError::new(e)),
            None => RResult::RErr(RBoxError::new(AsyncError::RequestAlreadyUsed)),
        }
    }
}

struct SteelResponse {
    response: Option<isahc::Response<AsyncBody>>,
}

impl SteelResponse {
    fn get_header(&mut self, header: &str) -> RResult<FFIValue, RBoxError> {
        if let Some(inner) = &self.response {
            if let Some(value) = inner.headers().get(header) {
                match value.to_str() {
                    Ok(v) => v.to_string().into_ffi_val(),
                    Err(e) => RResult::RErr(RBoxError::new(e)),
                }
            } else {
                false.into_ffi_val()
            }
        } else {
            false.into_ffi_val()
        }
    }

    fn into_text(&mut self) -> FfiFuture<RResult<FFIValue, RBoxError>> {
        let resp = self.response.take();

        async move {
            if let Some(mut resp) = resp {
                let text = resp.text().await;

                match text {
                    Ok(text) => text.into_ffi_val(),
                    Err(e) => RResult::RErr(RBoxError::new(e)),
                }
            } else {
                RResult::RErr(RBoxError::new(AsyncError::ResponseAlreadyUsed))
            }
        }
        .into_ffi()
    }
}

impl From<isahc::Response<AsyncBody>> for SteelResponse {
    fn from(value: isahc::Response<AsyncBody>) -> Self {
        SteelResponse {
            response: Some(value),
        }
    }
}

impl Custom for SteelResponse {}
