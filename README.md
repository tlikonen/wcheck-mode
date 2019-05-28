Wcheck Mode
===========

**General-purpose text-checker interface for Emacs text editor**


Introduction
------------

Wcheck mode is a general-purpose text-checker interface for [Emacs][]
text editor. Wcheck mode is a minor mode which provides an on-the-fly
text checker. It checks the visible text area, as you type, and possibly
highlights some parts of it. What is checked and how are all
configurable.

Wcheck mode can use external programs or Emacs Lisp functions for
checking text. For example, Wcheck mode can be used with spell-checker
programs such as Ispell, Enchant and Hunspell, but actually any tool
that can receive text from standard input stream and send text to
standard output can be used. Wcheck mode sends parts of buffer's content
to an external program or an Emacs Lisp function and, relying on their
output, decides if some parts of text should be marked in the buffer.

[Emacs]: http://www.gnu.org/software/emacs/


Features
--------

In Wcheck mode's configuration different configuration units are called
_languages_. In terms of a spelling checker it is natural to think of
them as different human languages. Wcheck mode is not limited to that,
though. Language is just a configuration unit for a specific text
checking purpose.

Each language can use its own checker engine (external program or a
function), command-line arguments and other settings, such as the
regular expressions and syntax table that are used to match words (or
other text elements) in Emacs buffer. User can choose which _face_ is
used to mark text elements in buffer.

User can create language-specific and major mode specific settings
defining which _faces_ to read or skip in buffers. A typical use for
this feature is to spell-check only those areas in buffer which are
written in the target language. For example, in email messages usually
the message body and Subject header are important enough to spell-check.
In programming modes user could spell-check only documentation strings
and comments (or the opposite if you want to use Wcheck mode to check
keywords and syntax of the programming language itself).

Wcheck mode can also be configured to offer any kind of actions for
marked text. Actions are presented to user through a menu which is
activated either by (1) clicking the right mouse button on a marked text
or (2) executing interactive command `wcheck-actions` while the cursor
(the point) is on a marked text.

If you use Wcheck mode as a spelling checker then it's natural to
configure an action menu that offers spelling suggestions for misspelled
words. The action menu could also have an option to add marked word to
spell-checker's dictionary, so that the word is recognized in the
future. That's only one application for Wcheck mode, though. Wcheck mode
can be configured to find almost any kind of text elements from buffer,
mark them, and offer any kind of actions for marked text.


How does it compare to other spell-checkers?
--------------------------------------------

The open design makes Wcheck mode (internally) quite different from
spell-checkers like [Flyspell][] mode and [Speck][] mode. They are
specific tools for spell-checking through Ispell or compatible program
and are therefore very much tied to Ispell's features and command-line
interface. This can be useful if you want to use Ispell or fully
compatible program for spell-checking natural languages. However, not
all human languages can be supported through Ispell and there can also
be other kind of text-checking needs.

The motivation behind Wcheck mode is to offer more general-purpose and
configurable interface for text checking. It can be configured to work
with almost anything: user's custom shell, Awk or Perl scripts, Lisp
functions or other checkers and text filters. Even if you only need a
spelling checker for human languages Wcheck mode can be a good choice.
It has more configuration possibilities than other spell-checkers and
the on-the-fly checker performs very well. It's a true real-time
checker.

[Flyspell]: http://www.emacswiki.org/emacs/FlySpell
[Speck]:    http://www.emacswiki.org/SpeckMode


Install
-------

You can install Wcheck mode through [GNU Elpa][Elpa] or [Melpa][]
package archives. Alternatively you can put `wcheck-mode.el` file to
some directory in your Emacs's `load-path` and add the following lines
to Emacs's initialization file (`~/.emacs` or `~/.emacs.d/init.el`):

    (autoload 'wcheck-mode "wcheck-mode"
      "Toggle wcheck-mode." t)
    (autoload 'wcheck-change-language "wcheck-mode"
      "Switch wcheck-mode languages." t)
    (autoload 'wcheck-actions "wcheck-mode"
      "Open actions menu." t)
    (autoload 'wcheck-jump-forward "wcheck-mode"
      "Move point forward to next marked text area." t)
    (autoload 'wcheck-jump-backward "wcheck-mode"
      "Move point backward to previous marked text area." t)

[Elpa]:  https://elpa.gnu.org/
[Melpa]: https://melpa.org/


Configuration and basic usage
-----------------------------

The internal documentation of variable `wcheck-language-data` has a
complete description on how to configure Wcheck mode language data. For
easy configuration you can use the options in the customize group named
_wcheck_ (`M-x customize-group RET wcheck RET`).

It might be convenient to bind Wcheck mode commands to some easily
accessible keys, for example:

    (global-set-key (kbd "C-c s") 'wcheck-mode)
    (global-set-key (kbd "C-c l") 'wcheck-change-language)
    (global-set-key (kbd "C-c c") 'wcheck-actions)
    (global-set-key (kbd "C-c n") 'wcheck-jump-forward)
    (global-set-key (kbd "C-c p") 'wcheck-jump-backward)

Interactive command `wcheck-mode` toggles the text-checker minor mode
for the current buffer. Command `wcheck-change-language` is used to
switch languages and command `wcheck-actions` (or the right mouse
button) opens an actions menu for marked text. Commands
`wcheck-jump-forward` and `wcheck-jump-backward` jump to next or
previous marked text area.

A note for Emacs Lisp programmers: Emacs Lisp function
`wcheck-marked-text-at` returns information about marked text at a
buffer position. Programmers can use it to perform any kind of actions
for marked text. Function `wcheck-query-language-data` can be used for
querying effective configuration data for any language.


Examples
--------

Here are some examples on how you can fill the `wcheck-language-data`
variable. The value is a list of language configurations:

    (setq wcheck-language-data
          '(("language"
             ...)
            ("another language"
             ...)))

Perhaps the most common use for Wcheck mode is to spell-check human
languages with Ispell (or compatible) spelling checker. Let's start with
examples on how to configure that.

The following settings configure two languages which are named "British
English" and "Finnish". The former language uses Ispell program as the
spell-checker engine. The latter uses Enchant which has an
Ispell-compatible command-line interface. Both languages use Wcheck
mode's actions feature to offer spelling suggestions for misspelled
words. Since both spelling checkers print spelling suggestions in the
Ispell format we use built-in function
`wcheck-parser-ispell-suggestions` to parse the output and populate the
actions (spelling suggestions) menu for user.

    ("British English"
     (program . "/usr/bin/ispell")
     (args "-l" "-d" "british")
     (action-program . "/usr/bin/ispell")
     (action-args "-a" "-d" "british")
     (action-parser . wcheck-parser-ispell-suggestions))

    ("Finnish"
     (program . "/usr/bin/enchant")
     (args "-l" "-d" "fi")
     (syntax . my-finnish-syntax-table)
     (action-program . "/usr/bin/enchant")
     (action-args "-a" "-d" "fi")
     (action-parser . wcheck-parser-ispell-suggestions))

The "Finnish" language above used a special syntax table called
`my-finnish-syntax-table`. It could be defined like this:

    (defvar my-finnish-syntax-table
      (copy-syntax-table text-mode-syntax-table))

    (modify-syntax-entry ?- "w" my-finnish-syntax-table)

It copies `text-mode-syntax-table` (which Wcheck mode uses by default)
and sets the syntactic meaning of the ASCII hyphen character (-) to a
word character ("w"). Wcheck mode and its regular expression search will
use that syntax table when scanning buffers' content in that language.

Below is an example on how to add an "Add to dictionary" feature to the
actions menu, among spelling suggestions. First, there's the language
configuration. The example below is similar to the "British English"
configuration above except that Enchant spell-checker is used and
`action-parser` is a custom function (which will be defined later).

    ("British English"
     (program . "/usr/bin/enchant")
     (args "-l" "-d" "en_GB")
     (action-program . "/usr/bin/enchant")
     (action-args "-a" "-d" "en_GB")
     (action-parser . enchant-suggestions-menu))

The action parser is custom function `enchant-suggestions-menu`. It will
call `wcheck-parser-ispell-suggestions` and then add "Add to dictionary"
option in the front of the spelling suggestions list. Choosing that
option from the actions menu will call function
`enchant-add-to-dictionary` (will be defined later).

    (defun enchant-suggestions-menu (marked-text)
      (cons (cons "[Add to dictionary]" 'enchant-add-to-dictionary)
            (wcheck-parser-ispell-suggestions)))

Now we need to define the function `enchant-add-to-dictionary`. Below is
an example that works in GNU/Linux systems with Enchant spell-checker.
With small modifications it should work with other spelling checkers and
operating systems.

For British English language the user dictionary file is
`~/.config/enchant/en_GB.dic`. The language code is extracted
automatically from `wcheck-language-data` variable, so the function
works with any Enchant language. Note that adding a word to a dictionary
file doesn't have effect on the current spell-checking session. The
Enchant program must be restarted.

    (defvar enchant-dictionaries-dir "~/.config/enchant")

    (defun enchant-add-to-dictionary (marked-text)
      (let* ((word (aref marked-text 0))
             (language (aref marked-text 4))
             (file (let ((code (nth 1 (member "-d" (wcheck-query-language-data
                                                    language 'action-args)))))
                     (when (stringp code)
                       (concat (file-name-as-directory enchant-dictionaries-dir)
                               code ".dic")))))

        (when (and file (file-writable-p file))
          (with-temp-buffer
            (insert word) (newline)
            (append-to-file (point-min) (point-max) file)
            (message "Added word \"%s\" to the %s dictionary"
                     word language)))))

Spell-checking human languages is not the only application for Wcheck
mode. The following configuration adds language called "Trailing
whitespace" which finds and marks all trailing whitespace characters
(spaces and tabs) on buffer's lines. It uses regular expressions to
match the whitespace. The checker program is the Emacs Lisp function
`identity` which just returns its argument unchanged. The
`action-program` option and feature is used to build an action menu with
just one option: remove the whitespace. It replaces the original
whitespace string with empty string.

    ("Trailing whitespace"
     (program . identity)
     (action-program . (lambda (marked-text)
                         (list (cons "Remove whitespace" ""))))
     (face . highlight)
     (regexp-start . "")
     (regexp-body . "[ \t]+")
     (regexp-end . "$")
     (regexp-discard . "")
     (read-or-skip-faces
      (nil)))

Sometimes it's useful to highlight only a small number of keywords in
buffer. The following example adds a language called "Highlight FIXMEs"
which marks only "FIXME" words. FIXME is some programmers' convention to
put reminders in source code that some parts are not complete yet and
will be fixed or completed later. In source code files such keywords are
written in program's comments only, not in the actual code, so we use
`read-or-skip-faces` feature to scan only the comments. This example
configures it for `emacs-lisp-mode` and `c-mode`. In all other major
modes FIXMEs are marked everywhere.

    ("Highlight FIXMEs"
     (program . (lambda (strings)
                  (when (member "FIXME" strings)
                    (list "FIXME"))))
     (face . highlight)
     (read-or-skip-faces
      ((emacs-lisp-mode c-mode) read font-lock-comment-face)
      (nil)))

The following example shows American English, which checks comments and stings.

    (setq wcheck-language-data
      '(("American English"
          (program . "/usr/bin/enchant-2")
          (args "-l" "-d" "en_US")
          (action-program . "/usr/bin/enchant-2")
          (action-args "-a" "-d" "en_US")
          (action-parser . enchant-suggestions-menu)
          (read-or-skip-faces
            ((emacs-lisp-mode c-mode)
              read font-lock-comment-face
              read font-lock-string-face)
            (nil)))))
    (setq wcheck-language "American English")

The following example adds a language "email" for highlighting email
addresses in buffer and creating an action menu which has option to
start composing mail to that address. Here's the language configuration:

    ("email"
     (program . email-address-detect)
     (face . highlight)
     (case-fold . t)
     (regexp-start . "\\<")
     (regexp-body . "\\S-+@\\S-+")
     (regexp-end . "\\>")
     (regexp-discard . "")
     (action-program . email-action-menu)
     (read-or-skip-faces
      (nil)))

This example 

    (defun email-address-detect (strings)
      (let (addresses)
        (dolist (string strings addresses)
          (when (string-match "\\<[a-z.-]+\\>@\\<[a-z.-]+\\>" string)
            (push (match-string-no-properties 0 string) addresses)))))

    (defun email-action-menu (marked-text)
      (list (cons (concat "Mail to <" (aref marked-text 0) ">")
                  (lambda (marked-text)
                    (compose-mail (aref marked-text 0))))))

Note that detecting all valid email addresses is difficult and a much
more advanced parser is needed for that. Feel free to replace the
detection function with a better one.


The source code repository
--------------------------

GitHub repository URL: <https://github.com/tlikonen/wcheck-mode>

The branch named _master_ is the release branch and it should always be
safe to use. New features and experimental code are developed in other
branches and possibly merged to _master_ when they are ready.


Copyright and license
---------------------

Copyright (C) 2009-2016 Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

The license text: <http://www.gnu.org/licenses/gpl-3.0.html>

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
