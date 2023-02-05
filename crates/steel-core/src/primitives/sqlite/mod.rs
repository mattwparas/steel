use rusqlite::{
    types::{ToSqlOutput, Value},
    Connection, Result, Statement, ToSql,
};

use crate::{
    rvals::{Custom, SteelString},
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    SteelVal,
};

impl Custom for Connection {}
impl Custom for rusqlite::Error {}

impl Custom for Statement<'static> {}

impl ToSql for SteelVal {
    fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
        match self {
            Self::IntV(b) => Ok(ToSqlOutput::Owned(Value::Integer((*b).try_into().unwrap()))),
            Self::StringV(s) => Ok(ToSqlOutput::Owned(Value::Text(s.to_string()))),
            Self::NumV(n) => Ok(ToSqlOutput::Owned(Value::Real(*n))),
            Self::Void => Ok(ToSqlOutput::Owned(Value::Null)),
            _ => {
                todo!("Implement serialization for other types")
            }
        }
    }
}

fn statement_wrapper(
    connection: &'static Connection,
    sql: SteelString,
) -> Result<Statement<'static>> {
    connection.prepare(sql.as_str())
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
    // .register_fn("connection/prepare", statement_wrapper);

    module
}
