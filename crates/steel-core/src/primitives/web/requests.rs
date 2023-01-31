use reqwest::{
    blocking::{get, Client, RequestBuilder, Response},
    StatusCode,
};

use crate::{
    rvals::Custom,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};

use serde_json::Value;

impl Custom for Client {}

impl Custom for SteelRequestBuilder {}

impl Custom for StatusCode {}

#[derive(Debug)]
struct SteelRequestBuilder {
    builder: RequestBuilder,
}

impl SteelRequestBuilder {
    fn new(builder: RequestBuilder) -> Self {
        Self { builder }
    }

    fn json(self, value: Value) -> Self {
        self.builder.json(&value).into()
    }

    fn query(self, value: Value) -> Self {
        self.builder.query(&value).into()
    }

    fn send(self) -> reqwest::Result<SteelResponse> {
        self.builder.send().map(SteelResponse::from)
    }

    fn header(self, key: String, value: String) -> Self {
        self.builder.header(key, value).into()
    }

    fn basic_auth(self, username: String, password: Option<String>) -> Self {
        self.builder.basic_auth(username, password).into()
    }

    fn bearer_auth(self, token: String) -> Self {
        self.builder.bearer_auth(token).into()
    }
}

impl From<RequestBuilder> for SteelRequestBuilder {
    fn from(value: RequestBuilder) -> Self {
        Self::new(value)
    }
}

impl Clone for SteelRequestBuilder {
    fn clone(&self) -> Self {
        Self {
            builder: self.builder.try_clone().expect("Internal steel error: it should not be possible to pass a stream as a body, which is the only way for this clone operation to fail")
        }
    }
}

fn post_wrapper(client: &Client, url: String) -> SteelRequestBuilder {
    client.post(url).into()
}

fn get_wrapper(client: &Client, url: String) -> SteelRequestBuilder {
    client.get(url).into()
}

fn basic_get_wrapper(url: String) -> reqwest::Result<SteelResponse> {
    get(url).map(|x| x.into())
}

fn status_code_to_int(status_code: &StatusCode) -> usize {
    status_code.as_u16() as usize
}

struct SteelResponse {
    response: Option<Response>,
}

impl From<Response> for SteelResponse {
    fn from(value: Response) -> Self {
        SteelResponse {
            response: Some(value),
        }
    }
}

impl SteelResponse {
    fn status(&self) -> Option<StatusCode> {
        self.response.as_ref().map(|x| x.status())
    }

    fn identity(&self) -> bool {
        true
    }

    fn json(&mut self) -> Option<reqwest::Result<Value>> {
        self.response.take().map(|x| x.json::<Value>())
    }

    fn text(&mut self) -> Option<reqwest::Result<String>> {
        self.response.take().map(|x| x.text())
    }
}

impl Custom for SteelResponse {}

pub fn requests_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/web/requests".to_string());

    module
        .register_fn("request/client", Client::new)
        .register_fn("get", basic_get_wrapper)
        .register_fn("client/post", post_wrapper)
        .register_fn("client/get", get_wrapper)
        .register_fn("request/json", SteelRequestBuilder::json)
        .register_fn("request/query", SteelRequestBuilder::query)
        .register_fn("request/send", SteelRequestBuilder::send)
        .register_fn("request/header", SteelRequestBuilder::header)
        .register_fn("request/basic-auth", SteelRequestBuilder::basic_auth)
        .register_fn("request/bearer-auth", SteelRequestBuilder::bearer_auth)
        .register_fn("response/status", SteelResponse::status)
        .register_fn("status-code->int", status_code_to_int)
        .register_fn("status-code/success?", StatusCode::is_success)
        .register_fn("status-code/redirect?", StatusCode::is_redirection)
        .register_fn("status-code/client-error?", StatusCode::is_client_error)
        .register_fn("status-code/server-error?", StatusCode::is_server_error)
        .register_type::<SteelRequestBuilder>("request/builder?")
        .register_type::<StatusCode>("status-code?")
        .register_type::<Client>("request/client?")
        .register_fn("response?", SteelResponse::identity)
        .register_fn("response->json", SteelResponse::json)
        .register_fn("response->text", SteelResponse::text);

    module
}
