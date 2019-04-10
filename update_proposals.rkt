#lang racket

(require db)

; load configuration file
(require (file "config.rkt"))

; set up command line arguments
(define mode (command-line
              #:program "update_proposals"
              #:args (updatetype) ; (add, update)
              updatetype))

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
                 ") "
                 (vector-ref entry 3)
                 ".\n"))
  )

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
  (define submitdate (getinput "Submit date"))
  (define oID (getinput "Organization's proposal ID"))

  ; do the INSERT into the Sqlite database
  (query-exec conn "INSERT INTO proposals (type, organization, solicitation, telescope, PI, title, CoI, status, submitdate, orgpropID) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
              proptype org solic tele pi title coi status submitdate oID))

; update an entry
(define (update)
  (write-string "Updating proposals")
  ; retrieve all proposals whose status is still listed as "submitted"
  (define unfinished (query-rows conn "SELECT ID,telescope,solicitation,title FROM proposals WHERE status='submitted'"))
  (write-string (string-append (make-string (length unfinished)) " pending proposals found:\n"))
  (map printentry unfinished)
  (write-string "Please enter a proposal number to edit (enter 0 or nothing to exit): ")
  (define upID (read-line))

  )

; make sure we can use the sqlite3 connection
(cond (not (sqlite3-available?))
    (error "Sqlite3 library not available."))

; open the database file
(define conn (sqlite3-connect #:database dbloc))

; determine which mode we're in
(cond
  [(regexp-match "add" mode) (addnew)]
  [(regexp-match "update" mode) (update)])

; close the databse
(disconnect conn)
