# CHANGELOG

## v0.3.x

### v0.3.0 (20 April 2025)

- Add option to specify date ranges or limits using `--start-date` and `--end-date`. This applies to `list-*` and `stats` command options.
- Add `--reuse-parameters`/`-r` for `add`. This will auto-fill the proposal type, submiting organzation, solicitation, and telescope from the most recent submission.
- bugfix in database creation
- combine creation and update scripts into a single file/tool
- Modes are now specified as switches rather than as a free-form string. This change enables a more coherent/consistent method of parsing the arguments and getting program argument help (i.e., with `--help`)
- Add `--list-open-calls` to show call information for unresolved (i.e., "submitted") proposals. This shows the number of proposals pending for each call, separated into PI'ed and Co-I'ed proposals.
- Print fractions of proposals (in `--stats`) as decimals rather than exact fractions.
- Various internal refactoring for simplicity and off-loading of work to SQLite engine.

## v0.2.x

### v0.2.0 (25 July 2022)

- Improved output formatting
- Add option to print only closed/resolved proposals with `list-closed` (i.e., all accepted/rejected)
- Add option to print only accepted (`list-accepted`) or rejected (`list-rejected`) proposals
- Rudimentary statistics on proposal success rates
    - total
    - only those led by the PI name specified in the config file
- Various bugfixes

## v0.1.x

### v0.1.0 (20 April 2019)

In the beginning was the simple proposal tool.
