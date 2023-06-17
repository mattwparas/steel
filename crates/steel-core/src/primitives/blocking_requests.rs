// use reqwest::{
//     blocking::{get, Client, RequestBuilder, Response},
//     StatusCode,
// };

use crate::{
    rvals::Custom,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};

use ureq::{Request, Response};

impl Custom for Request {}
impl Custom for Response {}
impl Custom for ureq::Error {}

fn get(url: String) -> Request {
    ureq::get(&url)
}

impl Custom for std::io::Error {}

struct SteelResponse {
    response: Option<ureq::Response>,
}

impl SteelResponse {
    fn into_text(&mut self) -> crate::rvals::Result<String> {
        let resp = self.response.take();

        if let Some(resp) = resp {
            Ok(resp.into_string()?)
        } else {
            crate::stop!(Generic => "Response already consumed, can only consume the response one time!")
        }
    }

    fn into_json(&mut self) -> crate::rvals::Result<serde_json::Value> {
        let resp = self.response.take();

        if let Some(resp) = resp {
            Ok(resp.into_json()?)

            // fn json(&mut self) -> Option<reqwest::Result<Value>> {
            //     self.response.take().map(|x| x.json::<Value>())
            // }
        } else {
            crate::stop!(Generic => "Response already consumed, can only consume the response one time!")
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

pub fn blocking_requests_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/web/blocking/requests".to_string());

    module
        .register_fn("get", get)
        .register_fn(
            "call",
            |request: Request| -> Result<SteelResponse, ureq::Error> {
                Request::call(request).map(|x| x.into())
            },
        )
        .register_fn("response->text", SteelResponse::into_text)
        .register_fn("response->json", SteelResponse::into_json);

    module
}
