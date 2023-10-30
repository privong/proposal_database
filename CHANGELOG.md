# CHANGELOG

## v0.3.x

### v0.3.0 (in progress)

- Add option to specify date ranges or limits using `--start-date` and `--end-date`. This only applies to `list-*` command options.
- Add `--reuse-parameters`/`-r` for `add`. This will auto-fill the proposal type, submiting organzation, solicitation, and telescope from the most recent submission.

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
