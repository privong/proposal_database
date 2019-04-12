# Proposals Database

Tools to create and manipulate a sqlite3 database with information on telescope and funding proposals.

Before using, copy `config.rkt.example` to `config.rkt` and edit it with your desired database location.
Then run `create_database.rkt` to create the sqlite3 file and create the `proposals` table.
Run `update_database.rkt help` for information on how to add/update entries.

## Requirements

* [Racket](https://racket-lang.org/) (tested with Racket 7.2)
* [`db` library](https://docs.racket-lang.org/db/index.html) and sqlite3 native library.

