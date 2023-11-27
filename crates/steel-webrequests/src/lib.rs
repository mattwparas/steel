use steel::rvals::Custom;
use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

use ureq::{Request, Response};

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/web/blocking/requests".to_string());

    module
        .register_fn("get", get)
        .register_fn(
            "call",
            |request: Request| -> Result<SteelResponse, ureq::Error> {
                Request::call(request).map(|x| x.into())
            },
        )
        .register_fn("response->text", SteelResponse::into_text);

    module
}

struct BlockingRequest(Request);
struct BlockingResponse(Response);

enum BlockingError {
    Ureq(ureq::Error),
    ResponseAlreadyUsed,
}

impl Custom for BlockingRequest {}
impl Custom for BlockingResponse {}
impl Custom for BlockingError {}

fn get(url: String) -> Request {
    ureq::get(&url)
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
