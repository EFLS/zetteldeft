;;; zetteldeft.el --- Turn deft into a zettelkasten system -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  EFLS

;; Author: EFLS <Elias Storms>
;; URL: https://efls.github.io/zetteldeft/
;; Keywords: deft zettelkasten zetteldeft wp files
;; Version: 0.3
;; Package-Requires: ((emacs "25.1") (deft "0.8") (ace-window "0.7.0"))

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Zetteldeft is an extension of the deft package for Emacs.
;; It generates unique IDs to create stable links between notes, which
;; allows the user to make an interconnected system of notes.
;; Zetteldeft uses deft to find and follow links to notes.
;; For more information, see zetteldeft.org
;; or https://efls.github.io/zetteldeft

;; Note: this file is tangled from zetteldeft.org.
;; The .org contains documentation and notes on usage of the package.

;;; Code:

(require 'deft)

(unless (require 'avy nil 'no-error)
  (user-error "Avy not installed, required for zetteldeft-avy-* functions"))

(require 'thingatpt)

(require 'ace-window)

(require 'seq)

(declare-function avy-jump "avy")
(unless (fboundp 'avy-jump)
  (display-warning 'zetteldeft
    "Function `avy-jump' not available. Please update `avy'"))

(defgroup zetteldeft nil
  "A zettelkasten on top of deft."
  :group 'deft
  :link '(url-link "https://efls.github.io/zetteldeft"))

;;;###autoload
(defun zetteldeft-search-at-point ()
  "Search via `deft' with `thing-at-point' as filter.
Thing can be a double-bracketed link, a hashtag, or a word."
  (interactive)
  (let ((string (zetteldeft--get-thing-at-point)))
   (if string
       (zetteldeft--search-global string t)
     (user-error "No search term at point"))))

;;;###autoload
(defun zetteldeft-search-current-id ()
  "Search deft with the id of the current file as filter.
Open if there is only one result."
  (interactive)
  (zetteldeft--search-global
    (zetteldeft--current-id) t))

(defun zetteldeft--get-thing-at-point ()
  "Return the thing at point.
This can be
 - a link: a string between [[ brackets ]],
 - a tag matching `zetteldeft-tag-regex',
 - a link matching `zetteldeft-link-indicator',
    `zetteldeft-id-regex' and `zetteldeft-link-suffix',
 - or a word."
 (let* ((link-brackets-re "\\[\\[\\([^]]+\\)\\]\\]")
        (link-id-re (zetteldeft--link-regex))
        (htag-re zetteldeft-tag-regex))
   (cond
    ((thing-at-point-looking-at link-brackets-re)
      (match-string-no-properties 1))
    ((thing-at-point-looking-at link-id-re)
      (match-string-no-properties 0))
    ((thing-at-point-looking-at htag-re)
      (match-string-no-properties 0))
    (t (thing-at-point 'word t)))))

(defun zetteldeft--search-global (str &optional dntOpn)
  "Search deft with STR as filter.
If there is only one result, open that file (unless DNTOPN is true)."
  ;; Sanitize the filter string
  (setq str (replace-regexp-in-string "[[:space:]\n]+" " " str))
  ;; Switch to Deft window if buffer is currently visible
  (when (deft-buffer-visible-p)
    (select-window (deft-buffer-visible-p)))
  ;; Call deft search on the filter string
  (let ((deft-incremental-search t))
   (deft)
   (deft-filter str t))
  ;; If there is a single match, open the file
  (unless dntOpn
   (when (eq (length deft-current-files) 1)
     (deft-open-file (car deft-current-files)))))

(defun zetteldeft--search-filename (thisStr &optional otherWindow)
  "Search for deft files with string THISSTR in filename.
Open if there is only one result (in another window if OTHERWINDOW is non-nil)."
  ;; Sanitize the filter string
  (setq thisStr (replace-regexp-in-string "[[:space:]\n]+" " " thisStr))
  ;; Call deft search on the filter string
  (let ((deft-filter-only-filenames t))
   (deft-filter thisStr t))
  ;; If there is a single match, open the file
  (when (eq (length deft-current-files) 1)
    (deft-open-file (car deft-current-files) otherWindow)))

(defun zetteldeft--get-file-list (srch)
  "Return a list of files with the search item SRCH."
  (let ((deft-current-sort-method 'title))
    (deft-filter srch t)
    deft-current-files))

;;;###autoload
(defun zetteldeft-search-tag ()
  "Prompt interactively for Zetteldeft tag and launch Deft search"
  (interactive)
  (let* ((tags (zetteldeft--get-all-sorted-tags))
         (search-term (completing-read "Tag to search for: " tags)))
    (zetteldeft--search-global search-term t)))

(defun zetteldeft--id-font-lock-setup (var val)
  "Add font-lock highlighting for zetteldeft links.
Called when `zetteldeft-link-indicator' or
`zetteldeft-id-regex' are customized."
  (when (and (boundp 'zetteldeft-link-indicator)
             (boundp 'zetteldeft-id-regex)
             (boundp 'zetteldeft-link-suffix))
     (font-lock-remove-keywords 'org-mode
        `((,(concat zetteldeft-link-indicator
                    zetteldeft-id-regex
                    zetteldeft-link-suffix)
           . font-lock-warning-face))))
  (set-default var val)
  (when (and (boundp 'zetteldeft-id-regex)
             (boundp 'zetteldeft-link-indicator)
             (boundp 'zetteldeft-link-suffix))
     (font-lock-add-keywords 'org-mode
        `((,(concat zetteldeft-link-indicator
                    zetteldeft-id-regex
                    zetteldeft-link-suffix)
           . font-lock-warning-face)))))

(defcustom zetteldeft-id-format "%Y-%m-%d-%H%M"
  "Format used when generating time-based zetteldeft IDs.

Be warned: the regexp to find IDs is set separately.
If you change this value, set `zetteldeft-id-regex' so that
the IDs can be found.

Check the documentation of the `format-time-string'
function to see which placeholders can be used."
  :type 'string
  :group 'zetteldeft)

(setq deft-new-file-format zetteldeft-id-format)

(defun zetteldeft-generate-id (title &optional filename)
  "Generate and return a Zetteldeft ID.
The ID is created using `zetteldeft-id-format', unless
`zetteldeft-custom-id-function' is bound to a function, in which case
that function is used and TITLE and FILENAME are passed to it."
  (let ((id
          (if-let ((f zetteldeft-custom-id-function))
              (funcall f title filename)
            (format-time-string zetteldeft-id-format))))
    (if (zetteldeft--id-available-p id)
        id
      (error "Generated ID %s is not unique." id))))

(defun zetteldeft--id-available-p (str)
  "Return t only if provided string STR is unique among Zetteldeft filenames."
  (let ((deft-filter-only-filenames t))
    (deft-filter str t))
  (eq 0 (length deft-current-files)))

(defcustom zetteldeft-custom-id-function nil
  "User-defined function to generate an ID.
The specified function must accept arguments for note `TITLE'
and &optional `FILENAME'. The returned ID must be a string."
  :type 'function
  :group 'zetteldeft)

(defcustom zetteldeft-id-regex "[0-9]\\{4\\}\\(-[0-9]\\{2,\\}\\)\\{3\\}"
  "The regular expression used to search for zetteldeft IDs.
Set it so that it matches strings generated with
`zetteldeft-id-format'."
  :type 'string
  :group 'zetteldeft
  :set 'zetteldeft--id-font-lock-setup)

(defcustom zetteldeft-link-indicator "§"
  "String to indicate zetteldeft links.
String prepended to IDs to easily identify them as links to zetteldeft notes.
This variable should be a string containing only one character."
  :type 'string
  :group 'zetteldeft
  :set 'zetteldeft--id-font-lock-setup)

(defcustom zetteldeft-link-suffix ""
  "String to append to zetteldeft links.
To disable, set to empty string rather than to nil."
  :type 'string
  :group 'zetteldeft
  :set 'zetteldeft--id-font-lock-setup)

(defun zetteldeft--link-regex ()
  "Return regex for a Zetteldeft link.
Concat link indicator, id-regex, and link suffix."
  (concat zetteldeft-link-indicator
          zetteldeft-id-regex
          zetteldeft-link-suffix))

(defun zetteldeft--lift-id (str)
  "Extract zetteldeft ID from STR.
This is done with the regular expression stored in
`zetteldeft-id-regex'."
  (with-temp-buffer
    (insert str)
    (when (re-search-forward zetteldeft-id-regex nil t -1)
      (match-string 0))))

(defun zetteldeft--insert-link (id &optional title)
  "Insert a link to Zetteldeft note ID.
If TITLE is included, use it as link text. To customize how inserted
links are formatted, change the `zetteldeft-insert-link-function'
variable."
  (interactive)
  (funcall zetteldeft-insert-link-function id title))

(defcustom zetteldeft-insert-link-function
           #'zetteldeft-insert-link-zd-style
  "The function to use when inserting note links.

Use either
 - `zetteldeft-insert-link-zd-style' for Zetteldeft type links
 - `zetteldeft-insert-link-org-style' for Org-mode zdlink: links
 - A custom function that takes two arguments: an ID and an optional title."
  :type 'function
  :options '(zetteldeft-insert-link-zd-style
             zetteldeft-insert-link-org-style)
  :group 'zetteldeft)

(defun zetteldeft-insert-link-zd-style (id &optional title)
  "Insert a Zetteldeft link to note with provided ID."
  (insert zetteldeft-link-indicator
          id
          zetteldeft-link-suffix)
  (when title (insert " " title)))

(defun zetteldeft-insert-link-org-style (id &optional title)
  "Insert a Zetteldeft link in Org-mode format as zdlink: type."
  (if title
      (insert "[[zdlink:" id "][" title "]]")
    (insert "[[zdlink:" id "]]")))

;;;###autoload
(defun zetteldeft-find-file (file)
  "Open deft file FILE."
  (interactive
    (list (completing-read "Deft find file: "
            (deft-find-all-files-no-prefix))))
  (deft-find-file file))

(defvar zetteldeft-home-id nil
  "String with ID of home note, used by `zetteldeft-go-home'.")

(defun zetteldeft-go-home ()
  "Move to a designated home note.
Set `zetteldeft-home-id' to an ID string of your home note."
  (interactive)
  (if (stringp zetteldeft-home-id)
      (zetteldeft-find-file
        (zetteldeft--id-to-full-path zetteldeft-home-id))
    (message "No home set. Provide a string to zetteldeft-home-id.")))

;;;###autoload
(defun zetteldeft-find-file-id-insert (file)
  "Find deft file FILE and insert a link."
  (interactive (list
    (completing-read "File to insert id from: "
      (deft-find-all-files-no-prefix))))
  (zetteldeft--insert-link (zetteldeft--lift-id file)))

(defcustom zetteldeft-backlink-prefix "# Backlink: "
  "Prefix string included before a back link.
Formatted as `org-mode' comment by default."
  :type 'string
  :group 'zetteldeft)

(defcustom zetteldeft-backlink-location-function
           #'zetteldeft-backlink-get-location
 "Function to get location for new backlinks.
The function should return a position in the current buffer.")

(defun zetteldeft-backlink-get-location ()
  "Default function that returns where a backlink should be added.

This is the line below whichever is found first:
 - existing backlink
 - tag line
 - title
 - at top of file
"
  (interactive)
  (save-excursion
    (goto-char (point-min))
      (cond
        ((re-search-forward (regexp-quote zetteldeft-backlink-prefix) nil t)
          (forward-line)
          (point))
        ((re-search-forward (regexp-quote zetteldeft-tag-line-prefix) nil t)
          (forward-line)
          (newline)
          (point))
        ((re-search-forward (regexp-quote zetteldeft-title-prefix) nil t)
          (forward-line)
          (newline)
          (point))
        (t (point-min)))))

;;;###autoload
(defun zetteldeft-backlink-add (file)
  "Find deft file FILE and insert a backlink to it.
Finds the title line, and adds `backlink-prefix' with
ID and title on a new line."
  (interactive (list
    (completing-read "File to add backlink to: "
      (deft-find-all-files-no-prefix))))
  (save-excursion
    (goto-char (funcall zetteldeft-backlink-location-function))
    (insert zetteldeft-backlink-prefix)
    (zetteldeft--insert-link
      (zetteldeft--lift-id file)
      (zetteldeft--lift-file-title (concat deft-directory file)))
    (insert "\n"))
  (message "Backlink added."))

;;;###autoload
(defun zetteldeft-find-file-full-title-insert (file)
  "Find deft file FILE and insert a link with title."
  (interactive (list
    (completing-read "File to insert full title from: "
      (deft-find-all-files-no-prefix))))
  (zetteldeft--insert-link
    (zetteldeft--lift-id file)
    (zetteldeft--lift-file-title (concat deft-directory file))))

(defcustom zetteldeft-id-filename-separator " "
  "String to separate zetteldeft ID from filename."
  :type 'string
  :group 'zetteldeft)

(declare-function evil-insert-state "evil")

(defcustom zetteldeft-new-filename-to-kill-ring nil
  "Add new filename to kill ring?"
  :type 'boolean
  :group 'zetteldeft)

;;;###autoload
(defun zetteldeft-new-file (str &optional id)
  "Create a new deft file.

The filename is a Zetteldeft ID, appended by STR. The ID will be
generated, unless ID is provided. A file title will be inserted in the
newly created file wrapped in `zetteldeft-title-prefix' and
`zetteldeft-title-suffix'. When `zetteldeft-new-filename-to-kill-ring'
is non-nil, the filename (without extension) is added to the kill
ring. When `evil' is loaded, change to insert state."
  (interactive (list (read-string "Note title: ")))
  (let* ((deft-use-filename-as-title t)
         (zdId (or id
                   (zetteldeft-generate-id str)))
         (zdName (concat zdId zetteldeft-id-filename-separator str)))
  (deft-new-file-named zdName)
  (when zetteldeft-new-filename-to-kill-ring
    (kill-new zdName))
  (zetteldeft--insert-title str)
  (save-buffer)
  (when (featurep 'evil) (evil-insert-state))))

;;;###autoload
(defun zetteldeft-new-file-and-link (str)
  "Create a new note and insert a link to it.
Similar to `zetteldeft-new-file', but insert a link to the new file."
  (interactive (list (read-string "Note title: ")))
  (let ((zdId (zetteldeft-generate-id str)))
    (zetteldeft--insert-link zdId str)
    (zetteldeft-new-file str zdId)))

;;;###autoload
(defun zetteldeft-new-file-and-backlink (str)
  "Create a new note and insert link and backlink."
  (interactive (list (read-string "Note title: ")))
  (let ((ogId (zetteldeft--current-id))
        (zdId (zetteldeft-generate-id str)))
    (zetteldeft--insert-link zdId str)
    (zetteldeft-new-file str zdId)
    (newline)
    (zetteldeft--insert-link ogId (zetteldeft--id-to-title ogId))))

(defun zetteldeft-extract-region-to-note (title)
  "Extract the marked region to a new note with TITLE."
  (interactive (list (if (not (use-region-p))
                         (user-error "No region active.")
                     (read-string "Note title: "))))
  (let* ((id (zetteldeft-generate-id title))
         (text (kill-region (region-beginning) (region-end))))
    (save-excursion
      (zetteldeft-new-file title id)
      (yank)
      (save-buffer))
    (zetteldeft--insert-link id title)))

;;;###autoload
(defun zetteldeft-follow-link ()
  "Follows zetteldeft link to a file if point is on a link.
Prompts for a link to follow with `zetteldeft-avy-file-search' if it isn't."
  (interactive)
  (if (and zetteldeft-follow-at-point
           (thing-at-point-looking-at (zetteldeft--link-regex)))
      (zetteldeft--search-filename
        (zetteldeft--lift-id (zetteldeft--get-thing-at-point)))
    (zetteldeft-avy-file-search)))

(defcustom zetteldeft-follow-at-point t
  "Should `zetteldeft-follow-link' open link at point?
When t, open note at point if point is on a link.
When nil, always use avy."
  :type 'boolean
  :group 'zetteldeft)

;;;###autoload
(defun zetteldeft-browse ()
  "Browse your notes with avy.
Keep calling `zetteldeft-avy-file-search' in a loop."
  (interactive)
  (let ((avy-single-candidate-jump nil))
    (while (zetteldeft-avy-file-search)
      (message "Browsing in Zetteldeft!"))))

;;;###autoload
(defun zetteldeft-avy-tag-search ()
  "Call on avy to jump to a tag.
Tags are filtered with `zetteldeft-tag-regex'."
  (interactive)
  (save-excursion
    (let ((avy-all-windows nil))
    (when (consp (avy-jump zetteldeft-tag-regex))
      (zetteldeft-search-at-point)))))

;;;###autoload
(defun zetteldeft-avy-file-search (&optional otherWindow)
 "Use `avy' to follow a zetteldeft link.
Links are found via `zetteldeft-link-indicator' and `zetteldeft-id-regex'.
Open that file (in another window if OTHERWINDOW)."
  (interactive)
  (save-excursion
    (when (consp (avy-jump (zetteldeft--link-regex)))
      (zetteldeft--search-filename
        (zetteldeft--lift-id (zetteldeft--get-thing-at-point)) otherWindow))))

(declare-function aw-select "ace-window")

;;;###autoload
(defun zetteldeft-avy-file-search-ace-window ()
  "Use `avy' to follow a zetteldeft link in another window.
Similar to `zetteldeft-avy-file-search', but with window selection.
When only one window is active, split it first.
When more windows are active, select one via `ace-window'."
  (interactive)
  (save-excursion
    (when (consp (avy-jump (zetteldeft--link-regex)))
      (let ((ID (zetteldeft--lift-id (zetteldeft--get-thing-at-point))))
        (when (eq 1 (length (window-list))) (split-window))
        (select-window (aw-select "Select window..."))
        (zetteldeft--search-filename ID)))))

;;;###autoload
(defun zetteldeft-avy-link-search ()
  "Use `avy' to perform a deft search on a zetteldeft link.
Similar to `zetteldeft-avy-file-search' but performs a full
text search for the link ID instead of filenames only.
Opens immediately if there is only one result."
  (interactive)
  (save-excursion
    (when (consp (avy-jump (zetteldeft--link-regex)))
      (zetteldeft--search-global
        (zetteldeft--lift-id (zetteldeft--get-thing-at-point))))))

(defun zetteldeft--list-dead-links ()
  "Return a list with IDs in Zetteldeft notes that have no corresponding note."
  (let ((dead-links '())
        (deft-filter-only-filenames t))
    (dolist (link (zetteldeft--list-all-links))
      (deft-filter link t)
      (when (eq 0 (length deft-current-files))
        (unless (member link dead-links)
           (push link dead-links))))
   dead-links))

(defconst zetteldeft--dead-links-buffer-name "*zetteldeft-dead-links*")

(defun zetteldeft-dead-links-buffer ()
  "Show a buffer with all dead links in Zetteldeft."
  (interactive)
  (switch-to-buffer zetteldeft--dead-links-buffer-name)
  (erase-buffer)
  (message "Finding all dead Zetteldeft links...")
  (let ((dead-links (zetteldeft--list-dead-links)))
    (insert (format "# Found %d dead links\n" (length dead-links)))
    (dolist (link dead-links)
      (insert (format " - %s in: " link))
      (deft-filter link t)
      (dolist (source (deft-current-files))
        (zetteldeft--insert-link (zetteldeft--lift-id source)))
      (insert "\n")))
  (unless (eq major-mode 'org-mode) (org-mode)))

;;;###autoload
(defun zetteldeft-deft-new-search ()
  "Launch deft, clear filter and enter insert state."
  (interactive)
  (deft)
  (deft-filter-clear)
  (when (featurep 'evil) (evil-insert-state)))

(defun zetteldeft--check ()
  "Check if the currently visited file is in `zetteldeft' territory:
whether it has `deft-directory' somewhere in its path."
  (unless (buffer-file-name)
    (user-error "Buffer not visiting a file"))
  (unless (string-match-p
            (regexp-quote (file-truename deft-directory))
            (file-truename (buffer-file-name)))
    (user-error "Not in zetteldeft territory")))

(defun zetteldeft--current-id ()
  "Retrieve ID from current file."
  (zetteldeft--check)
  (zetteldeft--lift-id
    (file-name-base (buffer-file-name))))

(defcustom zetteldeft-title-prefix "#+TITLE: "
  "Prefix string included when `zetteldeft--insert-title' is called.
Formatted for `org-mode' by default.
Don't forget to include a space."
  :type 'string
  :group 'zetteldeft)

(defcustom zetteldeft-title-suffix ""
  "String inserted below title when `zetteldeft--insert-title' is called.
Empty by default.
Don't forget to add `\\n' at the beginning to start a new line."
  :type 'string
  :group 'zetteldeft)

(defun zetteldeft--insert-title (title)
  "Insert TITLE as title in file.
Prepended by `zetteldeft-title-prefix' and appended by `zetteldeft-title-suffix'."
  (zetteldeft--check)
  (insert
    zetteldeft-title-prefix
    title
    zetteldeft-title-suffix))

(defun zetteldeft--lift-file-title (zdFile)
  "Return the title of a zetteldeft note.
ZDFILE should be a full path to a note."
  (let ((deft-use-filename-as-title nil))
    (deft-parse-title
      zdFile
      (with-temp-buffer
        (insert-file-contents zdFile)
        (buffer-string)))))

;;;###autoload
(defun zetteldeft-file-rename ()
  "Change current file's title, and use the new title to rename the file.
Use this on files in the `deft-directory'.
When the file has no Zetteldeft ID, one is generated and included in the new name."
  (interactive)
  (zetteldeft--check)
  (let ((old-filename (buffer-file-name)))
    (when old-filename
      (let* ((old-title (zetteldeft--lift-file-title old-filename))
             (prompt-text (concat "Change " old-title " to: "))
             (new-title (read-string prompt-text old-title))
             (id (or (zetteldeft--lift-id (file-name-base old-filename))
                     (zetteldeft-generate-id new-title old-filename)))
             (new-filename
               (deft-absolute-filename
                 (concat id zetteldeft-id-filename-separator new-title))))
        (rename-file old-filename new-filename)
        (deft-update-visiting-buffers old-filename new-filename)
        (zetteldeft-update-title-in-file new-title)
        (deft-refresh)))))

(defcustom zetteldeft-always-insert-title t
  "When renaming a note, insert title if not already present."
  :type 'boolean
  :group 'zetteldeft)

(defun zetteldeft-update-title-in-file (title)
  "Update the title in the current note buffer to TITLE.
This searches the buffer for `zetteldeft-title-prefix' and updates the current
title, if present. If not present and `zetteldeft-always-insert-title' is set,
this inserts a title line at the beginning of the buffer. Otherwise, no change
is made."
  (save-excursion
    (let ((zetteldeft-title-suffix ""))
      (goto-char (point-min))
      (if (re-search-forward (regexp-quote zetteldeft-title-prefix) nil t)
          (progn (delete-region (line-beginning-position) (line-end-position))
                 (zetteldeft--insert-title title))
        (when zetteldeft-always-insert-title
          (zetteldeft--insert-title title)
          (newline))))))

;;;###autoload
(defun zetteldeft-count-words ()
  "Prints total number of words and notes in the minibuffer."
  (interactive)
  (let ((numWords 0))
    (dolist (deftFile deft-all-files)
      (with-temp-buffer
        (insert-file-contents deftFile)
        (setq numWords (+ numWords (count-words (point-min) (point-max))))))
    (message
      "Your zettelkasten contains %s notes with %s words in total."
      (length deft-all-files) numWords)))

;;;###autoload
(defun zetteldeft-copy-id-current-file ()
  "Copy current ID.
Add the id from the filename the buffer is currently visiting to the
kill ring."
  (interactive)
  (zetteldeft--check)
  (let ((ID (concat zetteldeft-link-indicator
                    (zetteldeft--lift-id (file-name-base (buffer-file-name)))
                    zetteldeft-link-suffix)))
    (kill-new ID)
    (message "%s" ID)))

(defun zetteldeft--extract-links (deftFile)
  "Find all links in DEFTFILE and return a list."
  (let ((zdLinks (list)))
    (with-temp-buffer
      (insert-file-contents deftFile)
      (while (re-search-forward zetteldeft-id-regex nil t)
        (let ((foundTag (replace-regexp-in-string " " "" (match-string 0))))
          ;; Add found tag to zdLinks if it isn't there already
          (unless (member foundTag zdLinks)
            (push foundTag zdLinks)))
        ;; Remove found tag from buffer
        (delete-region (point) (re-search-backward zetteldeft-id-regex))))
   zdLinks))

(defun zetteldeft--list-all-links ()
  "Return a list with all IDs that appear in notes."
  (let ((all-links '()))
    (dolist (file deft-all-files)
      (dolist (link (zetteldeft--extract-links file))
        (unless (member link all-links)
          (push link all-links))))
    all-links))

(defun zetteldeft--id-to-full-path (zdID)
  "Return full path from given zetteldeft ID ZDID.
Returns nil when no files are found.
Throws an error when multiple files are found."
  (let ((deft-filter-only-filenames t))
    (deft-filter zdID t))
  (when (> (length deft-current-files) 1)
    (user-error "ID Error. Multiple zetteldeft files found with ID %s" zdID))
  (car deft-current-files))

(defun zetteldeft--id-to-title (zdId)
  "Turn a Zetteldeft ID into the title."
  (zetteldeft--lift-file-title
    (zetteldeft--id-to-full-path zdId)))

(defcustom zetteldeft-tag-regex "[#@][[:alnum:]_-]+"
  "Regular expression for finding Zetteldeft tags."
  :type 'string
  :group 'zetteldeft)

(defcustom zetteldeft-tag-prefix "#"
  "String prefix used when inserting new Zetteldeft tags."
  :type 'string
  :group 'zetteldeft)

(defcustom zetteldeft-tag-line-prefix "# Tags"
  "String used to find the line where tags in Zetteldeft files should go."
  :type 'string
  :group 'zetteldeft)

;;;###autoload
(defun zetteldeft-tag-insert-at-point (tag)
  "Insert TAG at point. Interactively, select an existing tag or provide new one."
  (interactive (list (completing-read
                        "Tag to insert: "
                        (zetteldeft--get-all-sorted-tags))))
  (unless (string-prefix-p zetteldeft-tag-prefix tag)
    (insert zetteldeft-tag-prefix))
  (insert tag))

;;;###autoload
(defun zetteldeft-tag-insert ()
  "Select existing tag or enter new one to insert in current Zetteldeft note.

The tag is appended to the first line starting with `zetteldeft-tag-line-prefix'.
If this variable is nil, or tag line is not found, insert tag at point."
  (interactive)
  (zetteldeft--check)
  (let ((dest (when zetteldeft-tag-line-prefix
                (save-excursion
                  (goto-char (point-min))
                  (re-search-forward zetteldeft-tag-line-prefix nil t)))))
    (if dest
        (save-excursion 
          (goto-char dest)
          (end-of-line)
          (insert " ")
          (call-interactively 'zetteldeft-tag-insert-at-point))
      (call-interactively 'zetteldeft-tag-insert-at-point))))

(defun zetteldeft-tag-remove ()
  "Prompt for a tag to remove from the current Zetteldeft note.
Only the first instance of the selected tag is removed."
  (interactive)
  (zetteldeft--check)
  ; Extract tags of current file into `zetteldeft--tag-list'
  (setq zetteldeft--tag-list (list))
  (save-buffer)
  (zetteldeft--extract-tags (buffer-file-name))
  ; Select a tag from that list
  (let* ((tag (completing-read
                "Tag to remove: "
                (seq-filter 'stringp zetteldeft--tag-list))))
    ; Find and remove first instance of that tag
    (save-excursion
      (goto-char (point-min))
      (re-search-forward tag nil t)
      (delete-region (point) (re-search-backward tag nil t))
      ; remove potential empty space before tag
      (backward-char)
      (when (looking-at " ") (delete-char 1)))))

(defconst zetteldeft--tag-buffer-name "*zetteldeft-tag-buffer*")

;;;###autoload
(defun zetteldeft-tag-buffer ()
  "Switch to the `zetteldeft-tag-buffer' and list tags."
  (interactive)
  (switch-to-buffer zetteldeft--tag-buffer-name)
  (erase-buffer)
  (let ((tagList (zetteldeft--get-all-tags)))
    (dolist (zdTag tagList)
      (when (stringp zdTag)
        (insert (format "%s (%d) \n"
                        zdTag
                        (lax-plist-get tagList zdTag)))))
    (unless (eq major-mode 'org-mode) (org-mode))
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-min))))

(defvar zetteldeft--tag-list nil
  "A temporary property list to store all tags.")

(defun zetteldeft--get-all-tags ()
  "Return a plist of all the tags found in zetteldeft files."
  (setq zetteldeft--tag-list (list))
  (dolist (deftFile deft-all-files)
    (zetteldeft--extract-tags deftFile))
  zetteldeft--tag-list)

(defun zetteldeft--get-all-sorted-tags ()
  "Return a sorted plist of all the tags found in zetteldeft files."
  (seq-sort 'string-lessp
            (seq-filter 'stringp
                        (zetteldeft--get-all-tags))))

(defun zetteldeft--tag-format ()
  "Adjust `zetteldeft-tag-regex' for more accurate results."
  (concat "\\(^\\|\s\\)" zetteldeft-tag-regex))

(defun zetteldeft--extract-tags (deftFile)
  "Find all tags in DEFTFILE and add them to `zetteldeft--tag-list'.
Increase counters as we go."
  (with-temp-buffer
    (insert-file-contents deftFile)
    (while (re-search-forward (zetteldeft--tag-format) nil t)
      (let ((foundTag (replace-regexp-in-string " " "" (match-string 0))))
        ;; Add found tag to zetteldeft--tag-list if it isn't there already
        (zetteldeft--tag-count foundTag))
      ;; Remove found tag from buffer
      (delete-region (point) (re-search-backward (zetteldeft--tag-format))))))

(defun zetteldeft--tag-count (zdTag)
  (let ((tagCount (lax-plist-get zetteldeft--tag-list zdTag)))
    (if tagCount
        ; if the tag was there already, inc by 1
        (setq zetteldeft--tag-list
          (lax-plist-put zetteldeft--tag-list zdTag (1+ tagCount)))
      ; if tag was not there yet, add & set to 1
      (setq zetteldeft--tag-list
        (lax-plist-put zetteldeft--tag-list zdTag 1)))))

(defcustom zetteldeft-export-tmp-dir
  (expand-file-name "zetteldeft/tmp/" user-emacs-directory)
  "Temporary directory for Zetteldeft export")

(defun zetteldeft--export-prepare-tmp-notes (&optional ignored)
  "Copy Zetteldeft files and prepare for export."
  (delete-directory zetteldeft-export-tmp-dir t t)
  (make-directory zetteldeft-export-tmp-dir t)
  (deft-refresh)
  (message
    "Zetteldeft preparing notes for export at %s"
    zetteldeft-export-tmp-dir)
  (dolist (file (deft-find-all-files))
    (zetteldeft--export-prepare-file file))
  (message "Zetteldeft notes copy finished."))

(defun zetteldeft--export-prepare-file (zdFile)
  "Prepare ZDFILE for export.
Copy its contents to `zetteldeftd-export-tmp-dir' and replace links with Org
file links. ZDFILE should be the path to the file."
  (with-temp-file (expand-file-name
                    (file-name-nondirectory zdFile)
                    zetteldeft-export-tmp-dir)
    (insert-file-contents zdFile)
    (while (re-search-forward (zetteldeft--link-regex) nil t)
      (let ((zdLink (match-string 0)))
	(delete-region (point)
		       (re-search-backward (zetteldeft--link-regex)))
  (let ((filePath (or (zetteldeft--id-to-full-path
                        (zetteldeft--lift-id zdLink))
                      ; When ID doesn't return a file (a dead link)
                      ;  use empty string
                      "")))
	(insert
	  (org-make-link-string
      (format "./%s" (file-name-nondirectory filePath))
	    zdLink)))))))

;;;###autoload
(defun zetteldeft-insert-list-links (zdSrch)
  "Search for ZDSRCH and insert a list of zetteldeft links to all results."
  (interactive (list (read-string "search string: ")))
  (let ((zdResults (zetteldeft--get-file-list zdSrch))
        (zdThisNote (buffer-file-name)))
    (when zdThisNote (setq zdResults (delete zdThisNote zdResults)))
    (dolist (zdFile zdResults)
      (zetteldeft--list-entry-file-link zdFile))))

(defcustom zetteldeft-list-links-missing-message
  "   No missing links with search term =%s= found\n"
  "Message to insert when no missing links are found.
This is used by `zetteldeft-insert-list-links-missing'.
%s will be replaced by the search term provided to
this function."
  :type 'string
  :group 'zetteldeft)

;;;###autoload
(defun zetteldeft-insert-list-links-missing (zdSrch)
  "Insert a list of links to all deft files with a search string ZDSRCH.
In contrast to `zetteldeft-insert-list-links' only include links not
yet present in the current file. Can only be called from a file in the
zetteldeft directory."
  (interactive (list (read-string "search string: ")))
  (zetteldeft--check)
  (let (zdThisID zdCurrentIDs zdFoundIDs zdFinalIDs)
    (setq zdCurrentIDs (zetteldeft--extract-links (buffer-file-name)))
    ;; filter IDs from search results
    (dolist (zdFile (zetteldeft--get-file-list zdSrch))
      (push (zetteldeft--lift-id zdFile) zdFoundIDs))
    ;; create new list with unique ids
    (dolist (zdID zdFoundIDs)
      (unless (member zdID zdCurrentIDs)
        (push zdID zdFinalIDs)))
    ;; remove the ID of the current buffer from said list
    (setq zdThisID (zetteldeft--lift-id (file-name-base (buffer-file-name))))
    (setq zdFinalIDs (delete zdThisID zdFinalIDs))
    ;; finally find full title for each ID and insert it
    (if zdFinalIDs
        (dolist (zdID zdFinalIDs)
          (insert " - ")
          (zetteldeft--insert-link zdID (zetteldeft--id-to-title zdID))
          (insert "\n"))
      ;; unless the list is empty, then insert a message
      (insert (format zetteldeft-list-links-missing-message zdSrch)))))

(defcustom zetteldeft-list-prefix " - "
  "Prefix for lists created with `zetteldeft-insert-list-links'
and `zetteldeft-insert-list-links-missing'."
  :type 'string
  :group 'zetteldeft)

(defun zetteldeft--list-entry-file-link (zdFile)
  "Insert ZDFILE as list entry."
  (let ((id (zetteldeft--lift-id (file-name-base zdFile))))
    (insert zetteldeft-list-prefix)
    (when id
          (insert zetteldeft-link-indicator
                  id
                  zetteldeft-link-suffix
                  " "))
    (insert (zetteldeft--lift-file-title zdFile)
            "\n")))

;;;###autoload
(defun zetteldeft-org-search-include (zdSrch)
  "Insert `org-mode' syntax to include all files containing ZDSRCH.
Prompt for search string when called interactively."
  (interactive (list (read-string "tag (include the #): ")))
  (dolist (zdFile (zetteldeft--get-file-list zdSrch))
    (zetteldeft--org-include-file zdFile)))

;;;###autoload
(defun zetteldeft-org-search-insert (zdSrch)
  "Insert the contents of all files containing ZDSRCH.
Files are separated by `org-mode' headers with corresponding titles.
Prompt for search string when called interactively."
  (interactive (list (read-string "Search term: ")))
  (dolist (zdFile (zetteldeft--get-file-list zdSrch))
    (zetteldeft--org-insert-file zdFile)))

(defun zetteldeft--file-contents (zdFile &optional removeLines)
  "Insert file contents of a zetteldeft note.
ZDFILE should be a full path to a note.

Optional: leave out first REMOVELINES lines."
  (with-temp-buffer
    (insert-file-contents zdFile)
    (when removeLines
      (kill-whole-line removeLines))
    (buffer-string)))

(defun zetteldeft--org-include-file (zdFile)
  "Insert code to include org file ZDFILE."
  (insert
    ;; Insert org-mode title
    "* " (zetteldeft--lift-file-title zdFile) "\n"
    ;; Insert #+INCLUDE: "file.org" :lines 2-
    "#+INCLUDE: \"" zdFile "\" :lines \"2-\"\n\n"))

(defun zetteldeft--org-insert-file (zdFile)
  "Insert title and contents of ZDFILE."
  (insert
    ;; Insert org-mode title
    "\n* " (zetteldeft--lift-file-title zdFile) "\n\n"
    ;; Insert file contents (without the first 3 lines)
    (zetteldeft--file-contents zdFile 3)))

(defcustom zetteldeft-graph-syntax-begin
  "#+BEGIN_SRC dot :file ./graph.pdf :cmdline -Kfdp -Tpdf
  \n graph {\n"
  "Syntax to be included at the start of the zetteldeft graph."
  :type 'string
  :group 'zetteldeft)

(defcustom zetteldeft-graph-syntax-end
  "} \n#+END_SRC\n"
  "Syntax to be included at the end of the zetteldeft graph."
  :type 'string
  :group 'zetteldeft)

(defvar zetteldeft--graph-links)

;;;###autoload
(defun zetteldeft-org-graph-search (str)
  "Insert org source block for graph with zd search results.
STR should be the search the resulting notes of which should be included in the graph."
  (interactive (list (read-string "search string: ")))
  (setq zetteldeft--graph-links (list))
  (let ((zdList (zetteldeft--get-file-list str)))
    (insert zetteldeft-graph-syntax-begin)
    (insert "\n  // links\n")
    (dolist (oneFile zdList)
      (insert "\n")
      (zetteldeft--graph-insert-links oneFile))
    (zetteldeft--graph-insert-all-titles))
  (insert zetteldeft-graph-syntax-end))

;;;###autoload
(defun zetteldeft-org-graph-note (deftFile)
  "Create a graph starting from note DEFTFILE."
  (interactive (list
    (completing-read "Note to start graph from: "
      (deft-find-all-files))))
  (setq zetteldeft--graph-links (list))
  (insert zetteldeft-graph-syntax-begin)
  (insert "\n  // base note and links \n")
  (zetteldeft--graph-insert-links deftFile)
  (zetteldeft--graph-insert-additional-links)
  (zetteldeft--graph-insert-all-titles)
  (insert zetteldeft-graph-syntax-end))

(defun zetteldeft--graph-insert-links (deftFile)
  "Insert links in DEFTFILE in dot graph syntax on a single line.
Any inserted ID is also stored in `zetteldeft--graph-links'."
  (let ((zdId (zetteldeft--lift-id deftFile)))
    (when zdId
      (insert "  \"" zdId "\" -- {")
      (dolist (oneLink (zetteldeft--extract-links deftFile))
        (zetteldeft--graph-store-link oneLink t)
        (insert "\"" oneLink "\" "))
      (insert "}\n")
      (zetteldeft--graph-store-link deftFile))))

(defun zetteldeft--graph-insert-title (deftFile)
  "Insert the DEFTFILE title definition in a one line dot graph format."
  (let ((zdTitle
          (replace-regexp-in-string "\"" ""
            (zetteldeft--lift-file-title deftFile)))
        (zdId    (zetteldeft--lift-id deftFile)))
    (when zdId
      (insert "  \"" zdId "\""
              " [label = \"" zdTitle " ("
              zetteldeft-link-indicator zdId zetteldeft-link-suffix ")\"")
      (insert "]" "\n"))
    (zetteldeft--graph-store-link deftFile)))

(defun zetteldeft--graph-store-link (deftFile &optional idToFile)
  "Push DEFTFILE to zetteldeft--graph-links unless it's already there.
When IDTOFILE is non-nil, DEFTFILE is considered an id
and the the function first looks for the corresponding file."
  (when idToFile
    (let ((deft-filter-only-filenames t))
      (progn
        (deft-filter deftFile t)
        (setq deftFile (car deft-current-files)))))
  (unless (member deftFile zetteldeft--graph-links)
    (push deftFile zetteldeft--graph-links)))

(defun zetteldeft--graph-insert-additional-links ()
  "Insert rest of `zetteldeft--graph-links'."
  (setq zetteldeft--graph-links (cdr zetteldeft--graph-links))
  (dolist (oneFile zetteldeft--graph-links)
    (zetteldeft--graph-insert-links oneFile)))

(defun zetteldeft--graph-insert-all-titles ()
  "Insert graphviz title lines.
Does this for all links stored in `zetteldeft--graph-links'."
  (insert "\n  // titles \n")
  (dolist (oneLink zetteldeft--graph-links)
    ;; Sometimes, a 'nil' list item is present. Ignore those.
    (when oneLink
      (zetteldeft--graph-insert-title oneLink))))

;;;###autoload
(defun zetteldeft-set-classic-keybindings ()
  "Sets global keybindings for `zetteldeft'."
  (interactive)
  (define-prefix-command 'zetteldeft-prefix)
  (global-set-key (kbd "C-c d") 'zetteldeft-prefix)
  (global-set-key (kbd "C-c d d") 'deft)
  (global-set-key (kbd "C-c d D") 'zetteldeft-deft-new-search)
  (global-set-key (kbd "C-c d R") 'deft-refresh)
  (global-set-key (kbd "C-c d s") 'zetteldeft-search-at-point)
  (global-set-key (kbd "C-c d c") 'zetteldeft-search-current-id)
  (global-set-key (kbd "C-c d f") 'zetteldeft-follow-link)
  (global-set-key (kbd "C-c d F") 'zetteldeft-avy-file-search-ace-window)
  (global-set-key (kbd "C-c d .") 'zetteldeft-browse)
  (global-set-key (kbd "C-c d h") 'zetteldeft-go-home)
  (global-set-key (kbd "C-c d l") 'zetteldeft-avy-link-search)
  (global-set-key (kbd "C-c d t") 'zetteldeft-avy-tag-search)
  (global-set-key (kbd "C-c d T") 'zetteldeft-tag-buffer)
  (global-set-key (kbd "C-c d /") 'zetteldeft-search-tag)
  (global-set-key (kbd "C-c d i") 'zetteldeft-find-file-id-insert)
  (global-set-key (kbd "C-c d I") 'zetteldeft-find-file-full-title-insert)
  (global-set-key (kbd "C-c d o") 'zetteldeft-find-file)
  (global-set-key (kbd "C-c d n") 'zetteldeft-new-file)
  (global-set-key (kbd "C-c d N") 'zetteldeft-new-file-and-link)
  (global-set-key (kbd "C-c d B") 'zetteldeft-new-file-and-backlink)
  (global-set-key (kbd "C-c d b") 'zetteldeft-backlink-add)
  (global-set-key (kbd "C-c d r") 'zetteldeft-file-rename)
  (global-set-key (kbd "C-c d x") 'zetteldeft-count-words)
  (global-set-key (kbd "C-c d #") 'zetteldeft-tag-insert)
  (global-set-key (kbd "C-c d $") 'zetteldeft-tag-remove))

(provide 'zetteldeft)
;;; zetteldeft.el ends here
