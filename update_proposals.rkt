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
  (displayln " stats\t\t - print summary statistics.")
  (displayln " list-open\t - Show all submitted (but not resolved) proposals.")
  (displayln " list-closed\t - Show all resolved (accepted and rejected) proposals.")
  (displayln " list-accepted\t - Show accepted proposals.")
  (displayln " list-rejected\t - Show rejected proposals.")
  (displayln " help\t\t - Show this help message.")
  (newline)
  (displayln "Copyright 2019-2020, 2022 George C. Privon"))

; set up a condensed prompt for getting information
(define (getinput prompt)
  (write-string prompt)
  (write-string ": ")
  (read-line))

; take an input result from the SQL search and write it out nicely
(define (printentry entry issub)
  (displayln (string-append
              (number->string (vector-ref entry 0))
              ": "
              (vector-ref entry 1)
              "("
              (vector-ref entry 2)
              "; PI: "
              (vector-ref entry 4)
              (if (not issub)
                  (string-append "; "
                                 (vector-ref entry 5))
                  "")
              ") \""
              (vector-ref entry 3)
              "\"")))

; add a new proposal to the database
(define (addnew conn)
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
(define (update conn ID)
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
(define (printprop conn
                   #:submitted issub
                   #:accepted [isaccept #f]
                   #:rejected [isrej #f])
  (define selclause (string-append
                     (if issub
                         "status='submitted'"
                         "status!='submitted'")
                     ; find things that are "accepted" or "funded"
                     (if isaccept
                         " AND status LIKE '%Accepted%' OR status LIKE '%Funded%'"
                         "")
                     ; find things that are "rejected"
                     (if isrej
                         " AND status LIKE '%Rejected%'"
                         "")))
  (define props (query-rows conn (string-append "SELECT ID,telescope,solicitation,title,PI,status FROM proposals WHERE "
                                                selclause)))
  (display (string-append (number->string (length props))))
  (if issub
      (displayln " pending proposals found.")
      (displayln " resolved proposals found."))
  (newline)
  ; print all the unresolved proposals to the screen
  (map (lambda (prop)
         (printentry prop issub))
       props))

; find proposals waiting for updates
(define (findpending conn)
  (write-string "Updating proposals. ")
  (printprop conn #:submitted #t)
  (write-string "Please enter a proposal number to edit (enter 0 or nothing to exit): ")
  (define upID (read-line))
  (cond
    [(eq? (string->number upID) 0) (exit)]
    [(string->number upID) (update conn (string->number upID))]
    [else (exit)]))

; compute and print some statistics about proposals:
; - total number of proposals (since earliest date)
; - number of pending proposals
; - number of successful proposals and corresponding fraction of the total that are not pending
; - number of rejected proposals and corresponding fraction of the total that are not pending
; - do the above two for all proposals and for proposals that I PI'ed. (TODO: PI'ed separation not yet implemented)
(define (proposal-stats conn)
  (displayln "Proposal statistics to date.\n")

  ; do statistics for all proposals
  (displayln "\tAll proposals")
  (let-values ([(Nprop Npending Nrejected) (get-stats conn)])
    (print-stats Nprop Npending Nrejected))

  ; do statistics for proposals as PI
  (displayln (string-append "\n\tPI'ed Proposals (by "
                            PIname
                            ")"))
  (let-values ([(Nprop Npending Nrejected) (get-stats conn #:selclause (string-append "PI LIKE '%"
                                                                                      PIname
                                                                                      "%'"))])
    (print-stats Nprop Npending Nrejected))

)

; given numbers, format somewhat pretty output of proposal statistics
(define (print-stats Nprop Npending Nrejected)
  (display (number->string Nprop))
  (display  "\ttotal proposals entered (")
  (display (number->string (- Nprop Npending)))
  (display " proposals resolved; ")
  (display (number->string Npending))
  (displayln " proposals pending).")
  (define Naccepted (- Nprop Npending Nrejected))
  (display (number->string Naccepted))
  (display "\tproposals accepted (f=")
  (display (number->string (/ Naccepted
                              (- Nprop Npending))))
  (displayln " of resolved proposals).")
  (display (number->string Nrejected))
  (display "\tproposals rejected (f=")
  (display (number->string (/ Nrejected
                              (- Nprop Npending))))
  (displayln " of resolved proposals)."))


; retrieve proposal numbers from the database, for statistics
(define (get-stats conn #:selclause [extrasel ""])
  (define mysel (if (eq? 0 (string-length extrasel))
                    ""
                    (string-append " AND "
                                   extrasel)))
  (define mysel-one (if (eq? 0 (string-length extrasel))
                        ""
                        (string-append " WHERE "
                                       extrasel)))
  (values
   ; total number of proposals
   (length (query-rows conn
                       (string-append "SELECT ID FROM proposals"
                                      mysel-one)))
   ; Number of pending proposals
   (length (query-rows conn
                       (string-append "SELECT ID FROM proposals WHERE status='submitted'"
                                      mysel)))
   ; Number of rejected proposals
   (length (query-rows conn
                       (string-append "SELECT ID FROM proposals WHERE status LIKE '%rejected%'"
                                      mysel)))))

; make sure we can use the sqlite3 connection
(define checkdblib
  (cond (not (sqlite3-available?))
        (error "Sqlite3 library not available.")))

; catch-all routine for when we need to access the database
(define (querysys mode)
  ; first see if we need write access or if we can use read only
  (define dbmode (if (or (regexp-match "add" mode)
                         (regexp-match "update" mode))
                     'read/write
                     'read-only))
  ; open the database with the specified mode
  (define conn (sqlite3-connect #:database dbloc
                                #:mode dbmode))
  ; now handle the user's request
  (cond
    [(regexp-match "add" mode) (addnew conn)]
    [(regexp-match "update" mode) (findpending conn)]
    [(regexp-match "stats" mode) (proposal-stats conn)]
    [(regexp-match "list-open" mode) (printprop conn #:submitted #t)]
    [(regexp-match "list-closed" mode) (printprop conn #:submitted #f)]
    [(regexp-match "list-accepted" mode) (printprop conn #:submitted #f #:accepted #t)]
    [(regexp-match "list-rejected" mode) (printprop conn #:submitted #f #:rejected #t)]
    [else (error (string-append "Unknown mode. Try " progname " help\n\n"))])

  ; close the databse
  (disconnect conn))


; First see if the user wants help or if we need to pass to one of the other
; procedures
(cond
  [(regexp-match "help" mode) (printhelp)]
  [else (querysys mode)])

