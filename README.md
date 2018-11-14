# Strette

Strette is an emacs library for monitoring log files during development.
Strette can be used to watch log files in order to parse, filter and/or transform their content.

Strette configuration (log parsing/filtering/transformation) is done through elisp. While this requires learning a bit of elisp, this make Strette configuration arbitrary flexible, easily reproducible and does not require learning yet another keymap / Domain Specific Language.

Strette is designed to be dynamic - Strette configuration can be live reloaded during development.

Strette is designed to be fast - The number of logs to display can be customized in order to be able to process arbitrary large files without the need for external mode such as VLF or the need to disable features such as font-lock.

## Installation

Strette requires emacs 25+. Strette uses the wc (word count) and tail commands and thus requires them to be installed and available.

Add Strette to your emacs package archives:

```elisp
(add-to-list 'package-archives '("replique" . "https://raw.githubusercontent.com/EwenG/strette.el/master/packages/") t)
```

## API

### `strette-start`

Arglists: `(f-name buffer-name log-regexp log-keys log-formatter log-filter &optional logs-limit)`

Watch the log file F-NAME and display its content in a buffer named BUFFER-NAME.

If BUFFER-NAME already exists and is already a strette buffer, then it is cleared and reused. If BUFFER-NAME already exists and is not a strette buffer, an error is thrown. If BUFFER-NAME does not exist, it is created.

Each line of the F-NAME file is matched against the LOG-REGEXP regexp and matching lines are reified into an association list with the following format:
```elisp
`((key1 . match-group1)
  (key2 . match-group2)
  ...
  (keyN . match-groupN)
  (:message . message-text))
```
where the keys are the elements of the LOG-KEYS list and the match-groups are the groups defined in the LOG-REGEXP. The reified log always contains a :message entry which value is the text that didn't match the regexp, including the lines found between two regexp match.

LOG-FORMATTER is a function which only argument is a log. LOG-FORMATTER must return a string which is the serialized representation of the log.

LOG-FILTER is a function which only argument is a log. LOG-FILTER must return a log which will be output into the BUFFER-NAME buffer. LOG-FILTER can return nil to exclude a log from the output buffer. LOG-FILTER is allowed to modify the values associated with the log keys.

LOGS-LIMIT is an optional parameter representing the maximum number of logs written to the output buffer. Its default value is 100. Passing nil as a LOGS-LIMIT will limit the number of logs to 100. The LOGS-LIMIT can be disabled by passing the value :no-limit.

Additional considerations:
- Every log entry in the F-NAME log file is expected to start after a newline character.
- The LOG-FILTER can be called multiple times with a same log parameter and as such, must be free of side effects. The LOG-FILTER function can be called with a log parameter which :message value is truncated but the LOG-FILTER is guaranteed to be called at least once with its full :message value. Strette will take care of writing a same log only once in the output buffer.

### `strette-alist-get`

Return the value associated with KEY in ALIST. If KEY is not found in ALIST, return DEFAULT. keys are compared using `equal`.

### `strette-alist-set`

Non destructively associate the value V with the key K in the alist ALIST. If ALIST already contains K, then its value (the first one if there are multiple K) is changed to V. If ALIST does not contain a key K, then the pair (K . V) is cons-ed to ALIST. Keys are compared using `equal`.
 
## Usage

See the [sample files](https://github.com/EwenG/strette.el/blob/master/sample/) for an example of usage.

# License

Copyright 2018 Ewen Grosjean.

Distributed under the GNU General Public License, version 3.