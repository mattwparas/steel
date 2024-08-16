use std::{
    rc::Rc,
    sync::{Arc, Mutex},
};

use abi_stable::std_types::{RString, RVec};
use rusqlite::{
    params_from_iter,
    types::{FromSql, FromSqlError, ToSqlOutput, Value},
    Connection, Statement, ToSql, Transaction,
};
use steel::{
    rvals::Custom,
    steel_vm::ffi::{is_opaque_type, FFIArg, FFIModule, FFIValue, RegisterFFIFn},
};

struct SqliteConnection {
    connection: Rc<Connection>,
}

struct TestSqliteConnection {
    connection: Arc<Mutex<Connection>>,
}

// Share this with multiple threads?
fn test_spawn() {
    let connection = TestSqliteConnection {
        connection: Arc::new(Mutex::new(Connection::open_in_memory().unwrap())),
    };

    std::thread::spawn(move || connection);
}

impl SqliteConnection {
    fn open_in_memory() -> Result<Self, SqliteError> {
        Ok(Self {
            connection: Rc::new(Connection::open_in_memory()?),
        })
    }

    fn open(string: String) -> Result<Self, SqliteError> {
        Ok(Self {
            connection: Rc::new(Connection::open(string)?),
        })
    }

    fn begin_transaction(&self) -> Result<SqliteTransaction, SqliteError> {
        Ok(SqliteTransaction {
            _connection: Rc::clone(&self.connection),
            token: Rc::new(()),
            transaction: Some(unsafe {
                std::mem::transmute::<Transaction<'_>, Transaction<'static>>(
                    self.connection.unchecked_transaction()?,
                )
            }),
        })
    }
}

struct SqlitePreparedStatement {
    _connection: Rc<Connection>,
    _sql: Rc<String>,
    prepared_statement: Option<Statement<'static>>,
}

impl Drop for SqlitePreparedStatement {
    fn drop(&mut self) {
        // Manual drop since we've erased the lifetimes
        if Rc::strong_count(&self._connection) == 1 {
            self.prepared_statement.take().unwrap().finalize().ok();
        }
    }
}

struct SqliteTransaction {
    _connection: Rc<Connection>,
    token: Rc<()>,
    transaction: Option<Transaction<'static>>,
}

impl Custom for SqliteTransaction {}

impl SqliteTransaction {
    fn try_finish(&mut self) -> Result<(), SqliteError> {
        if Rc::strong_count(&self.token) == 1 {
            if let Some(transaction) = self.transaction.take() {
                // Finish the transaction, hiding what the errors are
                return Ok(transaction.finish()?);
            }
        }

        Ok(())
    }

    fn finish(&mut self) -> Result<(), SqliteError> {
        if Rc::strong_count(&self.token) == 1 {
            let transaction = self
                .transaction
                .take()
                .ok_or_else(|| SqliteError::TransactionAlreadyCompleted)?;
            // Finish the transaction, hiding what the errors are
            return Ok(transaction.finish()?);
        }

        Ok(())
    }

    fn commit(&mut self) -> Result<(), SqliteError> {
        let transaction = self
            .transaction
            .take()
            .ok_or_else(|| SqliteError::TransactionAlreadyCompleted)?;

        Ok(transaction.commit()?)
    }

    fn try_commit(&mut self) -> Result<(), SqliteError> {
        if let Some(transaction) = self.transaction.take() {
            return Ok(transaction.commit()?);
        }

        Ok(())
    }

    fn rollback(&mut self) -> Result<(), SqliteError> {
        let transaction = self
            .transaction
            .take()
            .ok_or_else(|| SqliteError::TransactionAlreadyCompleted)?;

        Ok(transaction.rollback()?)
    }
}

impl Drop for SqliteTransaction {
    fn drop(&mut self) {
        if Rc::strong_count(&self.token) == 1 {
            if let Some(inner) = self.transaction.take() {
                // Finish the transaction, hiding what the errors are
                inner.finish().ok();
            }
        }
    }
}

enum SqliteError {
    TransactionAlreadyCompleted,
    Generic(rusqlite::Error),
}

impl Custom for SqliteError {}

impl From<rusqlite::Error> for SqliteError {
    fn from(value: rusqlite::Error) -> Self {
        Self::Generic(value)
    }
}

impl SqlitePreparedStatement {
    fn execute(&mut self, params: Vec<Vec<FFIArg>>) -> Result<usize, SqliteError> {
        let mut count = 0;

        if params.is_empty() {
            count += self.prepared_statement.as_mut().unwrap().execute([])?;
        } else {
            for group in params {
                count += self
                    .prepared_statement
                    .as_mut()
                    .unwrap()
                    .execute(params_from_iter(group.into_iter().map(FFIWrapper)))?;
            }
        }

        Ok(count)
    }

    // This is doing... lots of copying. Probably need to profile and figure out
    // a better interaction at the FFI boundary that doesn't require copying
    // the vector repeatedly
    fn query(&mut self, params: Vec<FFIArg>) -> Result<FFIValue, SqliteError> {
        let mut rows = self
            .prepared_statement
            .as_mut()
            .unwrap()
            .query(params_from_iter(params.into_iter().map(FFIWrapper)))?;

        let mut results = RVec::new();

        let mut width: Option<usize> = None;

        while let Some(row) = rows.next()? {
            if let Some(width) = width {
                // TODO: Save the row length for the next iteration, so that we can pre allocate
                // the row width
                let mut computed_row: RVec<FFIValue> = RVec::with_capacity(width);

                for i in 0..width {
                    computed_row.push(row.get(i).map(|x: FFIReturn| x.0).unwrap())
                }

                results.push(FFIValue::Vector(computed_row));
            } else {
                // TODO: Save the row length for the next iteration, so that we can pre allocate
                // the row width
                // let mut computed_row: Vec<SteelVal> = Vec::new();
                let mut i = 0;
                let mut computed_row: RVec<FFIValue> = RVec::new();

                while let Ok(value) = row.get::<_, FFIReturn>(i) {
                    computed_row.push(value.0);
                    i += 1;
                }

                results.push(FFIValue::Vector(computed_row));

                width = Some(i);
            }
        }

        Ok(FFIValue::Vector(results))
    }
}

impl Custom for SqlitePreparedStatement {}

impl SqliteConnection {
    fn prepare(&self, sql: String) -> Result<SqlitePreparedStatement, SqliteError> {
        let sql = Rc::new(sql);

        let statement = self.connection.prepare(&sql)?;

        Ok(SqlitePreparedStatement {
            _connection: Rc::clone(&self.connection),
            _sql: sql,
            prepared_statement: Some(unsafe {
                std::mem::transmute::<Statement<'_>, Statement<'static>>(statement)
            }),
        })
    }
}

struct FFIWrapper<'a>(FFIArg<'a>);
struct FFIReturn(FFIValue);

#[derive(Debug)]
struct SqliteConversionError(String);

impl std::fmt::Display for SqliteConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::error::Error for SqliteConversionError {}
impl Custom for SqliteConnection {}

impl<'a> ToSql for FFIWrapper<'a> {
    fn to_sql(&self) -> rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
        match &self.0 {
            // FFIValue::BoxedFunction(_) => todo!(),
            // FFIValue::BoolV(b) => Ok(rusqlite::types::ToSqlOutput::Owned(Value::Bo)),
            FFIArg::NumV(f) => Ok(ToSqlOutput::Owned(Value::Real(*f))),
            FFIArg::IntV(i) => Ok(ToSqlOutput::Owned(Value::Integer(*i as i64))),
            FFIArg::Void => Ok(ToSqlOutput::Owned(Value::Null)),
            FFIArg::StringV(s) => Ok(ToSqlOutput::Owned(Value::Text(s.to_string()))),
            // FFIValue::Vector(_) => todo!(),
            // FFIValue::CharV { c } => todo!(),
            // FFIValue::Custom { custom } => todo!(),
            // FFIValue::HashMap(_) => todo!(),
            _ => Err(rusqlite::Error::ToSqlConversionFailure(Box::new(
                SqliteConversionError(format!(
                    "Unable to convert value to a sql value: {:?}",
                    &self.0
                )),
            ))),
        }
    }
}

impl FromSql for FFIReturn {
    fn column_result(value: rusqlite::types::ValueRef<'_>) -> rusqlite::types::FromSqlResult<Self> {
        match value {
            rusqlite::types::ValueRef::Null => Ok(FFIReturn(FFIValue::Void)),
            rusqlite::types::ValueRef::Integer(i) => Ok(FFIReturn(FFIValue::IntV(i as isize))),
            rusqlite::types::ValueRef::Real(f) => Ok(FFIReturn(FFIValue::NumV(f))),
            rusqlite::types::ValueRef::Text(t) => Ok(FFIReturn(FFIValue::StringV(
                RString::from_utf8(t).map_err(|e| FromSqlError::Other(Box::new(e)))?,
            ))),
            rusqlite::types::ValueRef::Blob(_) => Err(rusqlite::types::FromSqlError::Other(
                Box::new(SqliteConversionError(format!(
                    "Unable to convert value to a sql value: {:?}",
                    &value
                ))),
            )),
        }
    }
}

steel::declare_module!(build_module);

pub fn build_module() -> FFIModule {
    let mut module = FFIModule::new("dylib/steel/sqlite");

    module
        .register_fn("prepare", SqliteConnection::prepare)
        .register_fn("SqliteConnection?", is_opaque_type::<SqliteConnection>)
        .register_fn("SqliteTransaction?", is_opaque_type::<SqliteTransaction>)
        .register_fn(
            "SqlitePreparedStatement?",
            is_opaque_type::<SqlitePreparedStatement>,
        )
        .register_fn("open-in-memory", SqliteConnection::open_in_memory)
        .register_fn("open", SqliteConnection::open)
        .register_fn("execute", SqlitePreparedStatement::execute)
        .register_fn("query", SqlitePreparedStatement::query)
        .register_fn("begin/transaction", SqliteConnection::begin_transaction)
        .register_fn("transaction/finish", SqliteTransaction::finish)
        .register_fn("transaction/commit", SqliteTransaction::commit)
        .register_fn("transaction/try-commit", SqliteTransaction::try_commit)
        .register_fn("transaction/rollback", SqliteTransaction::rollback)
        .register_fn("transaction/try-finish", SqliteTransaction::try_finish);

    module
}
