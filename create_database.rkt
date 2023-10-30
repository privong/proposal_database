#lang racket/base

;; This program creates a sqlite3 database and then creates an empty table
;; for information on proposals.

(require db
         "config.rkt") ; load configuration file

; create the database and create the table
(define (createdb dbloc)
  (write-string (string-append "Creating database " dbloc "\n"))
  (define conn (sqlite3-connect #:database dbloc
                                #:mode 'create))
  (query-exec conn "CREATE TABLE proposals (ID INTEGER PRIMARY KEY,
type TEXT NOT NULL,
organization TEXT NOT NULL,
solicitation TEXT NOT NULL,
telescope TEXT DEFAULT '',
orgpropID TEXT NOT NULL,
PI TEXT NOT NULL,
title TEXT NOT NULL,
CoI TEXT NOT NULL,
status TEXT NOT NULL,
submitteddate TEXT NOT NULL,
resultdate TEXT DEFAULT '')")
  (disconnect conn)
  (write-string (string-append "Database created at " dbloc "\n")))


; make sure we can use the sqlite3 connection
(cond (not (sqlite3-available?))
      (error "Sqlite3 library not available."))

; create the database and add the `proposals` table if it doesn't exist
(if (not (file-exists? dbloc))
    (createdb dbloc)
    (write-string "Database exists. Exiting."))
