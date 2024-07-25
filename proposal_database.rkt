#lang racket/base

;; This program updates an entry in a proposals database.

(require racket/cmdline
         racket/date
         racket/list
         db
         "config.rkt") ; load configuration file

(define progname "proposal_database.rkt")


; give us the date in YYYY-MM-DD format
(date-display-format 'iso-8601)

; parameters
; start and end date to sub-select proposals within a given range
(define start-date (make-parameter #f))
(define end-date (make-parameter #f))
; if #t, use proposal type, submitting organiation, solicitation/call, and
; telescope name from the most recently submitted (i.e., highest ID) proposal
(define reuse-params (make-parameter #f))

; set up command line arguments
(define mode (command-line
              #:program "proposal_database"
              #:once-each
              [("-s" "--start-date") sd "Start of date range (YYYY-MM-DD)"
                                     (start-date sd)]
              [("-e" "--end-date") ed "End of date range (YYYY-MM-DD)"
                                   (end-date ed)]
              [("-r" "--reuse-parameters") "Reuse/auto-fill proposal type, submitting organization, solicitation/call and telescope name from the most recently added proposal."
                                           (reuse-params #t)]
              #:args ([updatetype "help"]) ; (add, update, list-open, list-closed, help)
              updatetype))

; print some help
(define (printhelp)
  (displayln (string-append "Usage: "
                            progname " MODE"))
  (newline)
  (displayln "Where MODE is one of:")
  (displayln " create-database - initialize the proposal database.")
  (displayln " add\t\t - add new proposal to database.")
  (displayln " update\t\t - update a proposal with results.")
  (displayln " stats\t\t - print summary statistics.")
  (displayln " list-open\t - Show all submitted (but not resolved) proposals.")
  (displayln " list-closed\t - Show all resolved (accepted and rejected) proposals.")
  (displayln " list-accepted\t - Show accepted proposals.")
  (displayln " list-rejected\t - Show rejected proposals.")
  (displayln " help\t\t - Show this help message.")
  (newline)
  (displayln "Copyright 2019-2020, 2022-2024 George C. Privon"))

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

(define (get-last-proposal-call conn)
  (displayln "Adopting proposal information from last submission")
  (last-proposal-call conn))

; get information from the most recent proposal submission
(define (last-proposal-call conn)
  (vector->list (query-row conn "SELECT type, organization, solicitation, telescope FROM proposals ORDER BY id DESC LIMIT 1")))

; create the database and create the table
(define (createdb dbloc)
  ; make sure we can use the sqlite3 connection
  (cond [(not (sqlite3-available?)) (error "Sqlite3 library not available.")])

  ; create the database and add the `proposals` table if it doesn't exist
  (cond [(file-exists? dbloc) (error "Database exists. Exiting.")])
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
submitdate TEXT NOT NULL,
resultdate TEXT DEFAULT '')")
  (disconnect conn)
  (write-string (string-append "Database created at " dbloc "\n")))

; check to see if we can access the database
(define (checkdb conn)
  (cond [(connected? conn) (write-string "Database created successfully.")]
        [else (write-string "Could not connect to database.")]))


; add a new proposal to the database
(define (addnew conn)
  ; full list of input fileds that we will need (these will be the prompts
  ; to the user)
  (define input-fields (list "Proposal type"
                             "Submitting Organization"
                             "Solicitation/Call"
                             "Telescope"
                             "Proposal Title"
                             "PI"
                             "CoIs"
                             "Submit date (YYYY-MM-DD)"
                             "Organization's propsal ID"))
  (displayln "Adding new proposal to database.")
  ; assume all these proposals are submitted, don't ask the user
  (define status "submitted")

  ; get the proposal information
  (define propinfo
    (cond
      ; if we're re-using parameters, get info from the most recent submission
      ; and append the user input for the remaining fields
      [(reuse-params) (append (get-last-proposal-call conn)
                              (map getinput (list-tail input-fields 4)))]
      ; if not using previous information, ask the user for all inputs
      [else (map getinput input-fields)]))

  ; do the INSERT into the Sqlite database
  (let* ([add-proposal-info
           (prepare conn "INSERT INTO proposals (type, organization, solicitation, telescope, title, PI, CoI, submitdate, orgpropID, status) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")])
         (query-exec conn (bind-prepared-statement add-proposal-info
                                                   (flatten (list propinfo status))))))

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

; if the user selects a date range we need to decide which date to filter on
; If they're looking at submitted (i.e., open) proposals, use the submitted
; date.
; If they're looking at closed/resolved proposals, use the dates proposals
; were resolved.
(define (date-for-selection submitted)
  (if submitted
    "submitdate"
    "resultdate"))

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
                         " AND (status LIKE '%Accepted%' OR status LIKE '%Funded%')"
                         "")
                     ; find things that are "rejected"
                     (if isrej
                         " AND status LIKE '%Rejected%'"
                         "")))
  ; generate a selection clause if the user requested a restricted range
  (define dateclause (string-append
                       (if (or (start-date) (end-date))
                         " AND "
                         "")
                       (if (start-date)
                         (string-append
                           " DATE("
                           (date-for-selection issub)
                           ") >= DATE('"
                           (start-date)
                           "') ")
                         "")
                       (if (and (start-date) (end-date))
                         " AND "
                         "")
                       (if (end-date)
                         (string-append
                           " DATE("
                           (date-for-selection issub)
                           ") <= DATE('"
                           (end-date)
                           "') ")
                         "")))
  (define props (query-rows conn (string-append "SELECT ID,telescope,solicitation,title,PI,status FROM proposals WHERE "
                                                selclause
                                                dateclause)))
  (display (string-append (number->string (length props))))
  (if issub
      (displayln " pending proposals found.")
      (cond
        [isaccept (displayln " accepted proposals found.")]
        [isrej (displayln " rejected proposals found.")]))
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
  ; check if the user would like to create the database or not
  (cond
    [(regexp-match "create-database" mode) (createdb dbloc)])
  ; see if we need write access or if we can use read only
  (define dbmode (if (or (regexp-match "add" mode)
                         (regexp-match "update" mode))
                     'read/write
                     'read-only))
  ; open the database with the specified mode
  (define conn (sqlite3-connect #:database dbloc
                                #:mode dbmode))
  ; now handle the user's request
  (cond
    [(regexp-match "create-database" mode) (checkdb conn)]
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
