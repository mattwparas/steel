import sqlite3
from fastapi import FastAPI

app = FastAPI()

con = sqlite3.connect(":memory:", check_same_thread=False)

cur = con.cursor()

cur.execute(
    """
    CREATE TABLE person (
        id   INTEGER PRIMARY KEY,
        name TEXT NOT NULL,
        data TEXT
    )"""
)

cur.executemany(
    "INSERT INTO person (name, data) VALUES (?1, ?2)",
    [("Steven", "likes to eat"), ("Alex", "likes biking"), ("Matt", "likes running")],
)


def get_people():
    return list(cur.execute("SELECT id, name, data FROM person"))


# print(get_people())


@app.get("/people")
async def people():
    return get_people()
