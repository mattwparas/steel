use std::sync::Mutex;

use abi_stable::std_types::{RString, RVec};
use rusqlite::{
    params_from_iter,
    types::{FromSql, FromSqlError, ToSqlOutput, Value},
    Connection, Statement, ToSql, Transaction,
};
use steel::{
    gc::Shared,
    rvals::Custom,
    steel_vm::ffi::{is_opaque_type, FFIArg, FFIModule, FFIValue, RegisterFFIFn},
};

// Just... use arc mutex then?
struct SqliteConnection {
    connection: Shared<Mutex<Connection>>,
}

impl SqliteConnection {
    fn open_in_memory() -> Result<Self, SqliteError> {
        Ok(Self {
            connection: Shared::new(Mutex::new(Connection::open_in_memory()?)),
        })
    }

    fn open(string: String) -> Result<Self, SqliteError> {
        Ok(Self {
            connection: Shared::new(Mutex::new(Connection::open(string)?)),
        })
    }

    fn begin_transaction(&self) -> Result<SqliteTransaction, SqliteError> {
        Ok(SqliteTransaction {
            _connection: Shared::clone(&self.connection),
            token: Shared::new(()),
            transaction: Some(Mutex::new(unsafe {
                std::mem::transmute::<Transaction<'_>, Transaction<'static>>(
                    self.connection.lock().unwrap().unchecked_transaction()?,
                )
            })),
        })
    }
}

unsafe impl Send for SqliteTransaction {}
unsafe impl Sync for SqliteTransaction {}

struct SqlitePreparedStatement {
    _connection: Shared<Mutex<Connection>>,
    _sql: Shared<String>,
    prepared_statement: Option<Mutex<Statement<'static>>>,
}

// TODO: See if this is really necessary
unsafe impl Send for SqlitePreparedStatement {}
unsafe impl Sync for SqlitePreparedStatement {}

impl Drop for SqlitePreparedStatement {
    fn drop(&mut self) {
        // Manual drop since we've erased the lifetimes
        if Shared::strong_count(&self._connection) == 1 {
            self.prepared_statement
                .take()
                .unwrap()
                .into_inner()
                .unwrap()
                .finalize()
                .ok();
        }
    }
}

struct SqliteTransaction {
    _connection: Shared<Mutex<Connection>>,
    token: Shared<()>,
    transaction: Option<Mutex<Transaction<'static>>>,
}

impl Custom for SqliteTransaction {}

impl SqliteTransaction {
    fn try_finish(&mut self) -> Result<(), SqliteError> {
        if Shared::strong_count(&self.token) == 1 {
            if let Some(transaction) = self.transaction.take() {
                // Finish the transaction, hiding what the errors are
                return Ok(transaction.into_inner().unwrap().finish()?);
            }
        }

        Ok(())
    }

    fn finish(&mut self) -> Result<(), SqliteError> {
        if Shared::strong_count(&self.token) == 1 {
            let transaction = self
                .transaction
                .take()
                .ok_or_else(|| SqliteError::TransactionAlreadyCompleted)?;
            // Finish the transaction, hiding what the errors are
            return Ok(transaction.into_inner().unwrap().finish()?);
        }

        Ok(())
    }

    fn commit(&mut self) -> Result<(), SqliteError> {
        let transaction = self
            .transaction
            .take()
            .ok_or_else(|| SqliteError::TransactionAlreadyCompleted)?;

        Ok(transaction.into_inner().unwrap().commit()?)
    }

    fn try_commit(&mut self) -> Result<(), SqliteError> {
        if let Some(transaction) = self.transaction.take() {
            return Ok(transaction.into_inner().unwrap().commit()?);
        }

        Ok(())
    }

    fn rollback(&mut self) -> Result<(), SqliteError> {
        let transaction = self
            .transaction
            .take()
            .ok_or_else(|| SqliteError::TransactionAlreadyCompleted)?;

        Ok(transaction.into_inner().unwrap().rollback()?)
    }
}

impl Drop for SqliteTransaction {
    fn drop(&mut self) {
        if Shared::strong_count(&self.token) == 1 {
            if let Some(inner) = self.transaction.take() {
                // Finish the transaction, hiding what the errors are
                inner.into_inner().unwrap().finish().ok();
            }
        }
    }
}

#[allow(unused)]
#[derive(Debug)]
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
            count += self
                .prepared_statement
                .as_mut()
                .unwrap()
                .lock()
                .unwrap()
                .execute([])?;
        } else {
            for group in params {
                count += self
                    .prepared_statement
                    .as_mut()
                    .unwrap()
                    .lock()
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
        let mut guard = self.prepared_statement.as_mut().unwrap().lock().unwrap();

        let mut rows = guard.query(params_from_iter(params.into_iter().map(FFIWrapper)))?;

        let mut results = RVec::new();

        let mut width: Option<usize> = None;

        while let Some(row) = rows.next()? {
            if let Some(width) = width {
                let mut computed_row: RVec<FFIValue> = RVec::with_capacity(width);

                for i in 0..width {
                    computed_row.push(row.get(i).map(|x: FFIReturn| x.0).unwrap())
                }

                results.push(FFIValue::Vector(computed_row));
            } else {
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
        let sql = Shared::new(sql);

        let guard = self.connection.lock().unwrap();

        let statement = guard.prepare(&sql)?;

        Ok(SqlitePreparedStatement {
            _connection: Shared::clone(&self.connection),
            _sql: sql,
            prepared_statement: Some(Mutex::new(unsafe {
                std::mem::transmute::<Statement<'_>, Statement<'static>>(statement)
            })),
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
            FFIArg::BoolV(b) => Ok(ToSqlOutput::Owned(Value::Integer(if *b { 1 } else { 0 }))),
            FFIArg::NumV(f) => Ok(ToSqlOutput::Owned(Value::Real(*f))),
            FFIArg::IntV(i) => Ok(ToSqlOutput::Owned(Value::Integer(*i as i64))),
            FFIArg::Void => Ok(ToSqlOutput::Owned(Value::Null)),
            FFIArg::StringV(s) => Ok(ToSqlOutput::Owned(Value::Text(s.to_string()))),
            FFIArg::StringRef(s) => Ok(ToSqlOutput::Borrowed(rusqlite::types::ValueRef::Text(
                s.as_bytes(),
            ))),
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
