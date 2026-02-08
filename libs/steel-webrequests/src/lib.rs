use steel::rvals::Custom;
use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

use ureq::Request;

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/web/blocking/requests".to_string());

    module
        .register_fn("client/new", Client::new)
        .register_fn("client/get", Client::get)
        .register_fn("client/post", Client::post)
        .register_fn("client/put", Client::put)
        .register_fn("client/patch", Client::patch)
        .register_fn("client/delete", Client::delete)
        .register_fn("client/head", Client::head)
        .register_fn("get", get)
        .register_fn("post", post)
        .register_fn("put", put)
        .register_fn("delete", delete)
        .register_fn("patch", patch)
        .register_fn("head", head)
        .register_fn("set-header!", BlockingRequest::set)
        .register_fn("set-query-parameter!", BlockingRequest::query)
        .register_fn("set-timeout/ms!", BlockingRequest::timeout_ms)
        .register_fn(
            "call",
            |request: &mut BlockingRequest| -> Result<SteelResponse, BlockingError> {
                Request::call(std::mem::take(&mut request.0).unwrap())
                    .map(|x| x.into())
                    .map_err(BlockingError::Ureq)
            },
        )
        .register_fn("call-with-json", BlockingRequest::call_with_json)
        .register_fn("response->text", SteelResponse::into_text);

    module
}

#[derive(Clone)]
struct BlockingRequest(Option<Request>);

#[derive(Clone)]
struct Client(ureq::Agent);

#[derive(Debug)]
enum BlockingError {
    Ureq(ureq::Error),
    ResponseAlreadyUsed,
}

impl Custom for BlockingRequest {}
impl Custom for BlockingError {}
impl Custom for Client {}

impl std::fmt::Display for BlockingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockingError::Ureq(err) => write!(f, "{err}"),
            BlockingError::ResponseAlreadyUsed => write!(f, "response already used"),
        }
    }
}

impl std::error::Error for BlockingError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            BlockingError::Ureq(err) => Some(err),
            BlockingError::ResponseAlreadyUsed => None,
        }
    }
}

impl Client {
    fn new() -> Self {
        Self(ureq::agent())
    }

    fn get(&self, url: &str) -> BlockingRequest {
        BlockingRequest(Some(self.0.get(url)))
    }

    fn post(&self, url: &str) -> BlockingRequest {
        BlockingRequest(Some(self.0.post(url)))
    }

    fn put(&self, url: &str) -> BlockingRequest {
        BlockingRequest(Some(self.0.put(url)))
    }

    fn patch(&self, url: &str) -> BlockingRequest {
        BlockingRequest(Some(self.0.patch(url)))
    }

    fn delete(&self, url: &str) -> BlockingRequest {
        BlockingRequest(Some(self.0.delete(url)))
    }

    fn head(&self, url: &str) -> BlockingRequest {
        BlockingRequest(Some(self.0.head(url)))
    }
}

fn get(url: String) -> BlockingRequest {
    BlockingRequest(Some(ureq::get(&url)))
}

fn post(url: String) -> BlockingRequest {
    BlockingRequest(Some(ureq::post(&url)))
}

fn put(url: String) -> BlockingRequest {
    BlockingRequest(Some(ureq::put(&url)))
}

fn delete(url: String) -> BlockingRequest {
    BlockingRequest(Some(ureq::delete(&url)))
}

fn patch(url: String) -> BlockingRequest {
    BlockingRequest(Some(ureq::patch(&url)))
}

fn head(url: String) -> BlockingRequest {
    BlockingRequest(Some(ureq::head(&url)))
}

impl BlockingRequest {
    fn query(&mut self, parameter: String, value: String) {
        self.0 = Some(self.0.take().unwrap().query(&parameter, &value));
    }

    fn set(&mut self, header: String, value: String) {
        self.0 = Some(self.0.take().unwrap().set(&header, &value));
    }

    // TODO: Add FFI conversion form u64 as well
    fn timeout_ms(&mut self, time_in_ms: usize) {
        self.0 = Some(
            self.0
                .take()
                .unwrap()
                .timeout(std::time::Duration::from_millis(time_in_ms as u64)),
        );
    }

    fn call_with_json(&mut self, json: String) -> Result<SteelResponse, BlockingError> {
        Request::send_string(self.0.clone().unwrap(), &json)
            .map(|x| x.into())
            .map_err(BlockingError::Ureq)
    }
}

struct SteelResponse {
    response: Option<ureq::Response>,
}

impl SteelResponse {
    fn into_text(&mut self) -> Result<String, BlockingError> {
        let resp = self.response.take();

        if let Some(resp) = resp {
            resp.into_string()
                .map_err(|x| BlockingError::Ureq(x.into()))
        } else {
            Err(BlockingError::ResponseAlreadyUsed)
        }
    }
}

impl From<ureq::Response> for SteelResponse {
    fn from(value: ureq::Response) -> Self {
        SteelResponse {
            response: Some(value),
        }
    }
}

impl Custom for SteelResponse {}
