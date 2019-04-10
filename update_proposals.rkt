#lang racket

(require db)

; set up a condensed prompt for getting information
(define (getinput prompt)
  (write-string prompt)
  (write-string ": ")
  (read-line))

; make sure we can use the sqlite3 connection
(if sqlite3-available?
    "Adding new proposal to the database."
    (error "Sqlite3 library not available."))

; open the database file
(define conn (sqlite3-connect #:database
              "/home/george/work/proposals/proposals.sqlite"))

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
            proptype org solic tele pi title coi status submitdate oID)

; close the databse
(disconnect conn)