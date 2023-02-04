use rusqlite::{Connection, Result, ToSql};

use crate::{
    rvals::{Custom, SteelString},
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    SteelVal,
};

impl Custom for Connection {}
impl Custom for rusqlite::Error {}

impl ToSql for SteelVal {
    fn to_sql(&self) -> Result<rusqlite::types::ToSqlOutput<'_>> {
        todo!()
    }
}

fn connection_wrapper(
    connection: &Connection,
    sql: SteelString,
    params: im_lists::list::List<SteelVal>,
) -> Result<usize> {
    let opaque_params = params.iter().map(|x| x as &dyn ToSql).collect::<Vec<_>>();

    connection.execute(&sql, opaque_params.as_slice())
}

pub fn sqlite_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/sqlite".to_string());

    module
        .register_fn("connection/open-in-memory", Connection::open_in_memory)
        .register_fn("connection/execute!", connection_wrapper);

    // module
    //     .register_fn("ws/message-ping?", Message::is_ping)
    //     .register_fn("ws/message-pog?", Message::is_pong)
    //     .register_fn("ws/message-text?", Message::is_text)
    //     .register_fn("ws/message-text", Message::Text)
    //     .register_fn("ws/message-ping->pong", ping_to_pong)
    //     .register_fn("ws/message->text-payload", text_payload)
    //     .register_fn("ws/connect", connect::<String>)
    //     .register_fn("ws/read-message!", SteelWebSocket::read_message)
    //     .register_fn("ws/write-message!", SteelWebSocket::write_message);

    module
}
