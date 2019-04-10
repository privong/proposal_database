# Proposals Database

Tools to create and manipulate a sqlite3 database with information on telescope and funding proposals.

Before using, copy `config.rkt.example` to `config.rkt` and edit it with your desired database location.
Then run `create_database.rkt` to create the sqlite3 file and create the `proposals` table.
Then use `update_database.rkt` to add new proposal entries.
