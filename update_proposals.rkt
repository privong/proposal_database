#lang racket/base

;; This program updates an entry in a proposals database.

(require racket/cmdline
         racket/date
         db
        "config.rkt") ; load configuration file

(define progname "update_proposals.rkt")


; give us the date in YYYY-MM-DD format
(date-display-format 'iso-8601)

; set up command line arguments
(define mode (command-line
              #:program "update_proposals"
              #:args ([updatetype "help"]) ; (add, update, list-open, list-closed, help)
              updatetype))

; print some help
(define (printhelp)
  (displayln (string-append "Usage: "
                               progname " MODE"))
  (newline)
  (displayln "Where MODE is one of:")
  (displayln " add\t\t - add new proposal to database.")
  (displayln " update\t\t - update a proposal with results.")
  (displayln " list-open\t - Show all submitted (but not resolved) proposals.")
  (displayln " list-closed\t - Show all resolved proposals.")
  (displayln " help\t\t - Show this help message.")
  (newline)
  (displayln "Copyright 2019-2020 George C. Privon"))

; set up a condensed prompt for getting information
(define (getinput prompt)
  (write-string prompt)
  (write-string ": ")
  (read-line))

; take an input result from the SQL search and write it out nicely
(define (printentry entry)
  (displayln (string-append
                 (number->string (vector-ref entry 0))
                 ": "
                 (vector-ref entry 1)
                 "("
                 (vector-ref entry 2)
                 "; PI: "
                 (vector-ref entry 4)
                 ") \""
                 (vector-ref entry 3)
                 "\"")))

; add a new proposal to the database
(define (addnew)
  (displayln "Adding new proposal to database.")
  ; user inputs proposal data
  (define proptype (getinput "Proposal type"))
  (define org (getinput "Submitting organization"))
  (define solic (getinput "Solitation/call"))
  (define tele (getinput "Telescope"))
  (define title (getinput "Proposal title"))
  (define pi (getinput "PI"))
  (define coi (getinput "CoIs"))
  ; assume all these proposals are submitted, don't ask the user
  (define status "submitted")
  (define submitdate (getinput "Submit date (YYYY-MM-DD)"))
  (define oID (getinput "Organization's proposal ID"))

  ; do the INSERT into the Sqlite database
  (query-exec conn "INSERT INTO proposals (type, organization, solicitation, telescope, PI, title, CoI, status, submitdate, orgpropID) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
              proptype org solic tele pi title coi status submitdate oID))

; update an entry with new status (accepted, rejected, etc.)
(define (update ID)
  (displayln (string-append "Updating entry " (number->string ID)))
  (define entry (query-maybe-row conn "SELECT * FROM proposals WHERE ID=?" ID))
  (cond
    [(eq? #f entry) (error "Invalid ID. Row not found")])
  (displayln (string-append "Current status is: "
                               (vector-ref entry 9)
                               " ("
                               (vector-ref entry 10)
                               ")"))
  (write-string "Please enter new status: ")
  (define newstatus (read-line))
  ;(write-string "Please enter date of updated status (leave blank to use current date): ")
  ;(define resdate (read-line))
  (define resdate (date->string (seconds->date (current-seconds))))
  ; now update that entry
  (query-exec conn "UPDATE proposals SET status=?, resultdate=? WHERE ID=?"
              newstatus
              resdate
              ID)
  (displayln "Entry updated."))

; retrieve and print proposals based on status
(define (printprop #:submitted issub)
  (define selclause (if issub
                        "status='submitted'"
                        "status!='submitted'"))
  (define props (query-rows conn (string-append "SELECT ID,telescope,solicitation,title,PI FROM proposals WHERE "
                                                selclause)))
  (display (string-append (number->string (length props))))
  (if issub
      (displayln " pending proposals found.")
      (displayln " resolved proposals found."))
  (newline)
  ; print all the unresolved proposals to the screen
  (map printentry props))

; find proposals waiting for updates
(define (findpending)
  (write-string "Updating proposals")
  (printprop #:submitted #t)
  (write-string "Please enter a proposal number to edit (enter 0 or nothing to exit): ")
  (define upID (read-line))
  (cond
    [(eq? (string->number upID) 0) (exit)]
    [(string->number upID) (update (string->number upID))]
    [else (exit)]))

; make sure we can use the sqlite3 connection
(cond (not (sqlite3-available?))
    (error "Sqlite3 library not available."))

; open the database file
(define conn (sqlite3-connect #:database dbloc))

; determine which mode we're in
(cond
  [(regexp-match "help" mode) (printhelp)]
  [(regexp-match "add" mode) (addnew)]
  [(regexp-match "update" mode) (findpending)]
  [(regexp-match "list-open" mode) (printprop #:submitted #t)]
  [(regexp-match "list-closed" mode) (printprop #:submitted #f)]
  [else (error (string-append "Unknown mode. Try " progname " help\n\n"))])

; close the databse
(disconnect conn)
