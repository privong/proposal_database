# Proposals Database

Tools to create and manipulate a sqlite3 database with information on telescope and funding proposals.

Before using, copy `config.rkt.example` to `config.rkt` and edit it with your desired database location and, optionally, your last name.
Then run `racket proposal_database.rkt create-database` to create the sqlite3 file and create the `proposals` table.
Run `racket proposal_database.rkt help` for information on how to add/update entries.

## Requirements

* [Racket](https://racket-lang.org/) (tested with Racket >=7.5)
* [`db` library](https://docs.racket-lang.org/db/index.html) and sqlite3 native library.

