#lang racket/base

;; This program updates an entry in a proposals database.

(require racket/cmdline)
(require racket/date)
(require db)

(define progname "update_proposals.rkt")

; load configuration file
(require (file "config.rkt"))

; give us the date in YYYY-MM-DD format
(date-display-format 'iso-8601)

; set up command line arguments
(define mode (command-line
              #:program "update_proposals"
              #:args ([updatetype "help"]) ; (add, update, list-open help)
              updatetype))

; print some help
(define (printhelp)
  (write-string (string-append "Usage: "
                               progname " MODE\n\n"))

  (write-string "Where MODE is one of:\n")
  (write-string " add\t\t - add new proposal to database.\n")
  (write-string " update\t\t - update a proposal with results.\n")
  (write-string " list-open\t - Show all submitted (but not resolved) proposals.\n")
  (write-string " help\t\t - Show this help message.\n")
  (write-string "\nCopyright 2019 George C. Privon\n"))

; set up a condensed prompt for getting information
(define (getinput prompt)
  (write-string prompt)
  (write-string ": ")
  (read-line))

; take an input result from the SQL search and write it out nicely
(define (printentry entry)
  (write-string (string-append
                 (number->string (vector-ref entry 0))
                 ": "
                 (vector-ref entry 1)
                 "("
                 (vector-ref entry 2)
                 "; PI: "
                 (vector-ref entry 4)
                 ") \""
                 (vector-ref entry 3)
                 "\"\n")))

; add a new proposal to the database
(define (addnew)
  (write-string "Adding new proposal to database.\n")
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
  (write-string (string-append "Updating entry " (number->string ID) "\n"))
  (define entry (query-maybe-row conn "SELECT * FROM proposals WHERE ID=?" ID))
  (cond
    [((eq? #f entry) (error "Invalid ID. Row not found"))])
  (write-string (string-append "Current status is: "
                               (vector-ref entry 9)
                               " ("
                               (vector-ref entry 10)
                               ")\n"))
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
  (write-string "Entry updated.\n"))

; retrieve and print the proposals whose status is still listed as "submitted"
(define (printopen)
   ; retrieve all proposals wh
  (define unfinished (query-rows conn "SELECT ID,telescope,solicitation,title,PI FROM proposals WHERE status='submitted'"))
  (write-string (string-append (make-string (length unfinished)) " pending proposals found:\n"))
  ; print all the unresolved proposals to the screen
  (map printentry unfinished))

; find proposals waiting for updates
(define (findpending)
  (write-string "Updating proposals")
  (printopen)
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
  [(regexp-match "list-open" mode) (printopen)]
  [else (error(string-append "Unknown mode. Try " progname " help\n\n"))])

; close the databse
(disconnect conn)
