use crate::values::lists::SteelList as List;
use rusqlite::{
    params_from_iter,
    types::{FromSql, ToSqlOutput, Value},
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
                todo!("Implement serialization for other types: {:?}", self)
            }
        }
    }
}

impl FromSql for SteelVal {
    fn column_result(value: rusqlite::types::ValueRef<'_>) -> rusqlite::types::FromSqlResult<Self> {
        match value {
            rusqlite::types::ValueRef::Null => Ok(SteelVal::Void),
            rusqlite::types::ValueRef::Integer(i) => Ok(SteelVal::IntV(i as isize)),
            rusqlite::types::ValueRef::Real(f) => Ok(SteelVal::NumV(f)),
            rusqlite::types::ValueRef::Text(t) => Ok(SteelVal::StringV(
                std::str::from_utf8(t)
                    .expect("Unable to decode text from sqlite")
                    .into(),
            )),
            rusqlite::types::ValueRef::Blob(_) => {
                todo!("Implement deserialization for other types")
            }
        }
    }
}

fn prepare_and_execute(
    connection: &Connection,
    sql: SteelString,
    params: List<List<SteelVal>>,
) -> Result<usize> {
    let mut statement = connection.prepare(sql.as_str())?;

    let mut count = 0;

    for group in params {
        count += statement.execute(params_from_iter(group))?;
    }

    Ok(count)
}

// Consider returning a struct directly...
fn prepare_and_query(
    connection: &Connection,
    sql: SteelString,
    params: List<SteelVal>,
) -> Result<List<List<SteelVal>>> {
    let mut statement = connection.prepare(sql.as_str())?;

    let mut rows = statement.query(params_from_iter(params))?;

    let mut results = Vec::new();

    let mut width: Option<usize> = None;

    while let Some(row) = rows.next()? {
        if let Some(width) = width {
            // TODO: Save the row length for the next iteration, so that we can pre allocate
            // the row width
            let mut computed_row: Vec<SteelVal> = Vec::with_capacity(width);

            for i in 0..width {
                computed_row.push(row.get(i).unwrap())
            }

            results.push(List::from(computed_row));
        } else {
            // TODO: Save the row length for the next iteration, so that we can pre allocate
            // the row width
            let mut computed_row: Vec<SteelVal> = Vec::new();
            let mut i = 0;

            while let Ok(value) = row.get(i) {
                computed_row.push(value);
                i += 1;
            }

            width = Some(i);

            results.push(List::from(computed_row));
        }
    }

    // todo!()

    Ok(List::from(results))
}

// Consider returning a struct directly...
fn _query_to_struct_list(
    _connection: &Connection,
    _sql: SteelString,
    _params: List<SteelVal>,
    _struct_descriptor: SteelVal,
) -> Result<List<List<SteelVal>>> {
    todo!("Add pushing a struct constructor directly on to the values returned for one less allocation")
}

fn connection_wrapper(
    connection: &Connection,
    sql: SteelString,
    params: List<SteelVal>,
) -> Result<usize> {
    connection.execute(&sql, params_from_iter(params))
}

pub fn sqlite_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/sqlite".to_string());

    module
        .register_fn("connection/open-in-memory", Connection::open_in_memory)
        .register_fn("connection/execute!", connection_wrapper)
        .register_fn("connection/prepare-and-execute!", prepare_and_execute)
        .register_fn("connection/prepare-and-query!", prepare_and_query);

    module
}
