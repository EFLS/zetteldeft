;;; zetteldeft.el --- Turn deft into a zettelkasten system -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  EFLS

;; Author: EFLS <Elias Storms>
;; URL: https://efls.github.io/zetteldeft/
;; Keywords: deft zettelkasten zetteldeft
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (deft "0.8"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Zetteldeft is an extension of the deft package for emacs.
;; It generates unique IDs to create stable links between notes, which
;; allows the user to make an interconnected system of notes.
;; Zetteldeft uses deft to find and follow links to notes.

;;; Code:

(require 'deft)

(unless (require 'avy nil 'no-error)
  (user-error "Avy not installed, required for zd-avy-* functions"))

(unless (fboundp 'avy-jump)
  (display-warning 'zetteldeft
    "Function `avy-jump' not available. Please update `avy'"))

(defgroup zetteldeft nil
  "A zettelkasten on top of deft.")

(defun zd-get-thing-at-point ()
  "Return the thing at point, which can be a link, tag or word."
  (require 'thingatpt)
  (let* ((link-re "\\[\\[\\([^]]+\\)\\]\\]")
         (htag-re "\\([§#@][[:alnum:]_-]+\\)"))
   (cond
    ((thing-at-point-looking-at link-re)
      (match-string-no-properties 1))
     ((thing-at-point-looking-at htag-re)
      (match-string-no-properties 1))
     (t (thing-at-point 'word t)))))

(defun zd-search-at-point ()
  "Search deft with `thing-at-point' as filter.
Thing can be a double-bracketed link, a hashtag, or a word."
  (interactive)
  (let ((string (zd-get-thing-at-point)))
   (if string
       (zd-search-global string t)
     (user-error "No search term at point"))))

(defun zd-search-global (str &optional dntOpn)
  "Search deft with STR as filter.
If there is only one result, open that file (unless DNTOPN is true)."
  ;; Sanitize the filter string
  (setq str (replace-regexp-in-string "[[:space:]\n]+" " " str))
  ;; Call deft search on the filter string
  (let ((deft-incremental-search t))
   (deft)
   (deft-filter str t))
  ;; If there is a single match, open the file
  (unless dntOpn
   (when (eq (length deft-current-files) 1)
     (deft-open-file (car deft-current-files)))))

(defun zd-search-filename (thisStr &optional otherWindow)
  "Search for deft files with string THISSTR in filename.
Open if there is only one result (in another window if OTHERWINDOW is non-nill)."
  ;; Sanitize the filter string
  (setq thisStr (replace-regexp-in-string "[[:space:]\n]+" " " thisStr))
  ;; Call deft search on the filter string
  (let ((deft-filter-only-filenames t))
   (deft-filter thisStr t))
  ;; If there is a single match, open the file
  (when (eq (length deft-current-files) 1)
    (deft-open-file (car deft-current-files) otherWindow)))

(defun zd-search-current-id ()
  "Search deft with the id of the current file as filter.
Open if there is only one result."
  (interactive)
  (zd--check)
  (zd-search-global (zd-lift-id (file-name-base (buffer-file-name))) t))

(defun zd-get-file-list (srch)
  "Return a list of files with the search item SRCH."
  (let ((deft-current-sort-method 'title))
    (deft-filter srch t)
    deft-current-files))

(defcustom zd-id-format "%Y-%m-%d-%H%M"
  "Format used when generating zetteldeft IDs.

Be warned: the regexp to find IDs is set separately.
If you change this value, set `zd-id-regex' so that
the IDs can be found.

Check the documentation of the `format-time-string'
function to see which placeholders can be used."
  :type 'string
  :group 'zetteldeft)

(setq deft-new-file-format zd-id-format)

(defun zd-generate-id ()
  "Generate an ID in the format of `zd-id-format'."
  (format-time-string zd-id-format))

(defcustom zd-id-regex "[0-9]\\{4\\}\\(-[0-9]\\{2,\\}\\)\\{3\\}"
  "The regular expression used to search for zetteldeft IDs.
Set it so that it matches strings generated with
`zd-id-format'."
  :type 'string
  :group 'zetteldeft)

(defcustom zd-link-indicator "§"
  "String to indicate zetteldeft links.
String prepended to IDs to easily identify them as links to zetteldeft notes.
This variable should be a string containing only one character."
  :type 'string
  :group 'zetteldeft)

(defun zd-lift-id (str)
  "Extract the zetteldeft ID from STR with the regular expression stored in `zd-id-regex'."
  (with-temp-buffer
    (insert str)
    (when (re-search-forward zd-id-regex nil t -1)
      (match-string 0))))

(defcustom zd-tag-regex "[#@][a-z-]+"
  "Regular expression for zetteldeft tags."
  :type 'string
  :group 'zetteldeft)

(defun zd-find-file (file)
  "Open deft file FILE."
  (interactive
    (list (completing-read "Deft find file: "
           (deft-find-all-files-no-prefix))))
  (deft-find-file file))

(defun zd-find-file-id-insert (file)
  "Find deft file FILE and insert a link."
  (interactive (list
    (completing-read "File to insert id from: "
      (deft-find-all-files-no-prefix))))
  (insert (concat zd-link-indicator (zd-lift-id file))))

(defun zd-find-file-full-title-insert (file)
  "Find deft file FILE and insert a link with title."
  (interactive (list
    (completing-read "File to insert full title from: "
      (deft-find-all-files-no-prefix))))
  (insert (concat zd-link-indicator (file-name-base file))))

(defun zd-new-file (str &optional empty)
  "Create a new deft file.
Filename is `zd-id-format' appended by STR.
No file extension needed.

The title is inserted in `org-mode' format (unless EMPTY is true)
and the file name (without extension) is added to the kill ring.
When `evil' is loaded, enter instert state."
  (interactive (list (read-string "name: ")))
  (let* ((zdId (zd-generate-id))
         (zdName (concat zdId " " str)))
  (deft-new-file-named zdName)
  (kill-new zdName)
  (unless empty (zd-insert-title))
  (save-buffer)
  (when (featurep 'evil) (evil-insert-state))))

(defun zd-new-file-and-link (str)
  "Insert generated id with `zd-id-format' appended with STR.
Creates new deft file with id and STR as name."
  (interactive (list (read-string "name: ")))
  (insert zd-link-indicator (zd-generate-id) " " str)
  (zd-new-file str))

(defun zd-follow-link ()
  "Follows zetteldeft link to a file if point is on a link.
Prompts for a link to follow with `zd-avy-file-search' if it isn't."
  (interactive)
  (if (thing-at-point-looking-at (concat zd-link-indicator zd-id-regex))
      (zd-search-filename (zd-lift-id (zd-get-thing-at-point)))
    (zd-avy-file-search)))

(defun zd-avy-tag-search ()
  "Call on avy to jump to a tag.
Tags are filtered with `zd-tag-regex'."
  (interactive)
  (save-excursion
    (avy-jump zd-tag-regex)
    (zd-search-at-point)))

(defun zd-avy-link-search ()
  "Use `avy' to perform a deft search on a zetteldeft link.
Links are found via `zd-link-indicator'.
Opens immediately if there is only one result."
  (interactive)
  (unless zd-link-indicator
    (user-error "Zetteldeft avy functions won't work when `zd-link-indicator' is nil"))
  (save-excursion
    (avy-goto-char (string-to-char zd-link-indicator))
    (zd-search-global (zd-lift-id (zd-get-thing-at-point)))))

(defun zd-avy-file-search (&optional otherWindow)
 "Use `avy' to follow a zetteldeft link.
Links are found via `zd-link-indicator'
Open that file (in another window if OTHERWINDOW)."
  (interactive)
  (unless zd-link-indicator
    (user-error "Zetteldeft avy functions won't work when `zd-link-indicator' is nil"))
  (save-excursion
    (avy-goto-char (string-to-char zd-link-indicator))
    (zd-search-filename (zd-lift-id (zd-get-thing-at-point)) otherWindow)))

(defun zd-avy-file-search-ace-window ()
  "Use `avy' to follow a zetteldeft link in another window.
When there is only one search result, as there should be,
open that file in a window selected through `ace-window'."
  (interactive)
  (unless zd-link-indicator
    (user-error "Zetteldeft avy functions won't work when `zd-link-indicator' is nil"))
  (require 'ace-window)
  (save-excursion
    (avy-goto-char (string-to-char zd-link-indicator))
    (let ((ID (zd-lift-id (zd-get-thing-at-point))))
      (select-window (aw-select "Select window..."))
      (zd-search-filename ID))))

(defun zd-deft-new-search ()
  "Launch deft, clear filter and enter insert state."
  (interactive)
  (deft)
  (deft-filter-clear)
  (when (featurep 'evil) (evil-insert-state)))

(defun zd--check ()
  "Check if the currently visited file is in `zetteldeft' territory:
whether it has `deft-directory' somewhere in its path."
  (unless (buffer-file-name)
    (user-error "Buffer not visiting a file"))
  (unless (string-match-p
            (regexp-quote (file-truename deft-directory))
            (buffer-file-name))
    (user-error "Not in zetteldeft territory")))

(defun zd-file-rename ()
  "Rename the current file via the deft function. Use this on files in the deft-directory."
  (interactive)
  (zd--check)
    (let ((old-filename (buffer-file-name))
          (deft-dir (file-name-as-directory deft-directory))
          new-filename old-name new-name)
      (when old-filename
        (setq old-name (deft-base-filename old-filename))
        (setq new-name (read-string
                        (concat "Rename " old-name " to (without extension): ")
                        old-name))
        (setq new-filename
              (concat deft-dir new-name "." deft-default-extension))
        (rename-file old-filename new-filename)
        (deft-update-visiting-buffers old-filename new-filename)
        (zd-update-title-in-file)
        (deft-refresh))))

(defun zd-update-title-in-file ()
  "Update the title of the current file, if present.
Does so by looking for `zd-title-prefix'."
  (save-excursion
    (let ((zd-title-suffix ""))
      (goto-char (point-min))
      (when (re-search-forward (regexp-quote zd-title-prefix) nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (zd-insert-title)))))

(defun zd-lift-file-title (zdFile)
  "Return the title of a zetteldeft note.
ZDFILE should be a full path to a note."
  (let ((baseName (file-name-base zdFile)))
    (replace-regexp-in-string
     "[0-9]\\{2,\\}-[0-9-]+[[:space:]]"
     "" baseName)))

(defun zd-insert-title ()
  "Insert filename of current zd note, stripped from its ID.
Prepended by `zd-title-prefix' and appended by `zd-title-suffix'."
  (interactive)
  (zd--check)
  (insert
    zd-title-prefix
    (zd-lift-file-title (file-name-base (buffer-file-name)))
    zd-title-suffix))

(defcustom zd-title-prefix "#+TITLE: "
  "Prefix string included when `zd-insert-title' is called.
Formatted for `org-mode' by default.
Don't forget to include a space."
  :type 'string
  :group 'zetteldeft)

(defcustom zd-title-suffix ""
  "String inserted below title when `zd-insert-title' is called.
Empty by default.
Don't forget to add `\\n' at the beginning to start a new line."
  :type 'string
  :group 'zetteldeft)

(defun zd-count-words ()
  "Prints total number of words and notes in the minibuffer."
  (interactive)
  (let ((numWords 0))
    (dolist (deftFile deft-all-files)
      (with-temp-buffer
        (insert-file-contents deftFile)
        (setq numWords (+ numWords (count-words (point-min) (point-max))))))
    (message "Your zettelkasten contains %s notes with %s words in total." (length deft-all-files) numWords)))

(defun zd-copy-id-current-file ()
  "Add the id from the filename the buffer is currently visiting to the kill ring."
  (interactive)
  (zd--check)
  (let ((ID (concat zd-link-indicator
                    (zd-lift-id (file-name-base (buffer-file-name))))))
    (kill-new ID)
    (message "%s" ID)))

(defun zd-id-to-full-title (zdID)
  "Return full title from given zetteldeft ID ZDID.
Throws an error when either none or multiple files are found."
  (let ((deft-filter-only-filenames t))
    (deft-filter zdID t))
  (unless (eq (length deft-current-files) 1)
    (user-error "ID Error. Either no or multiple zetteldeft files found with ID %s" zdID))
  (file-name-base (car deft-current-files)))

(defun zd-all-tags ()
  "Return a list of all the tags found in zetteldeft files."
  (setq zd-tag-list (list))
  (dolist (deftFile deft-all-files)
    (zd-extract-tags deftFile))
  zd-tag-list)

(setq zd-tag-buffer-name "*zd-tag-buffer*")

(defun zd-tag-buffer ()
  "Switch to the *zd-tag-buffer* and list tags."
  (interactive)
  (switch-to-buffer zd-tag-buffer-name)
  (erase-buffer)
  (dolist (zdTag (zd-all-tags))
    (insert (format "%s \n" zdTag)))
  (unless (eq major-mode 'org-mode) (org-mode))
  (sort-lines nil (point-min) (point-max)))

(setq zd-tag-format (concat "\\(^\\|\s\\)" zd-tag-regex))

(defun zd-extract-tags (deftFile)
  "Find all tags in DEFTFILE and add them to `zd-tag-list'."
  (with-temp-buffer
    (insert-file-contents deftFile)
    (while (re-search-forward zd-tag-format nil t)
      (let ((foundTag (replace-regexp-in-string " " "" (match-string 0))))
        ;; Add found tag to zd-tag-list if it isn't there already
        (unless (member foundTag zd-tag-list)
          (push foundTag zd-tag-list)))
      ;; Remove found tag from buffer
      (delete-region (point) (re-search-backward zd-tag-format)))))

(defun zd-insert-list-links (zdSrch)
  "Search for ZDSRCH and insert a list of zetteldeft links to all results."
  (interactive (list (read-string "search string: ")))
  (let ((zdResults (zd-get-file-list zdSrch))
        (zdThisNote (buffer-file-name)))
    (when zdThisNote (setq zdResults (delete zdThisNote zdResults)))
    (dolist (zdFile zdResults)
      (zd-list-entry-file-link zdFile))))

(defun zd-insert-list-links-missing (zdSrch)
  "Insert a list of links to all deft files with a search string ZDSRCH.
In contrast to `zd-insert-list-links' only include links not yet present
in the current file.
Can only be called from a file in the zetteldeft directory."
  (interactive (list (read-string "search string: ")))
  (zd--check)
  (let (zdThisID zdCurrentIDs zdFoundIDs zdFinalIDs)
    (setq zdCurrentIDs (zd-extract-links (buffer-file-name)))
    ; filter IDs from search results
    (dolist (zdFile (zd-get-file-list zdSrch))
      (push (zd-lift-id zdFile) zdFoundIDs))
    ; create new list with unique ids
    (dolist (zdID zdFoundIDs)
      (unless (member zdID zdCurrentIDs)
        (push zdID zdFinalIDs)))
    ; remove the ID of the current buffer from said list
    (setq zdThisID (zd-lift-id (file-name-base (buffer-file-name))))
    (setq zdFinalIDs (delete zdThisID zdFinalIDs))
    ; finally find full title for each ID and insert it
    (if zdFinalIDs
        (dolist (zdID zdFinalIDs)
          (setq zdID (zd-id-to-full-title zdID))
          (insert " - " (concat zd-link-indicator zdID "\n")))
      ; unless the list is empty, then insert a message
      (insert (format zd-list-links-missing-message zdSrch)))))

(defcustom zd-list-links-missing-message
  "   No missing links with search term =%s= found\n"
  "Message to insert when no missing links are found by `zd-insert-list-links-missing'.
%s will be replaced by the search term provided to
that function."
  :type 'string
  :group 'zetteldeft)

(defun zd-list-entry-file-link (zdFile)
  "Insert ZDFILE as list entry."
  (insert " - " (concat zd-link-indicator (file-name-base zdFile)) "\n"))

(defun zd-org-search-include (zdSrch)
  "Insert `org-mode' syntax to include all files containing ZDSRCH.
Prompt for search string when called interactively."
  (interactive (list (read-string "tag (include the #): ")))
  (dolist (zdFile (zd-get-file-list zdSrch))
    (zd-org-include-file zdFile)))

(defun zd-org-search-insert (zdSrch)
  "Insert the contents of all files containing ZDSRCH.
Files are separated by `org-mode' headers with corresponding titles.
Prompt for search string when called interactively."
  (interactive (list (read-string "Search term: ")))
  (dolist (zdFile (zd-get-file-list zdSrch))
    (zd-org-insert-file zdFile)))

(defun zd-file-contents (zdFile &optional removeLines)
  "Insert file contents of a zetteldeft note.
ZDFILE should be a full path to a note.

Optional: leave out first REMOVELINES lines."
  (with-temp-buffer
    (insert-file-contents zdFile)
    (when removeLines
      (kill-whole-line removeLines))
    (buffer-string)))

(defun zd-org-include-file (zdFile)
  "Insert code to include org file ZDFILE."
  (insert
    ;; Insert org-mode title
    "* " (zd-lift-file-title zdFile) "\n"
    ;; Insert #+INCLUDE: "file.org" :lines 2-
    "#+INCLUDE: \"" zdFile "\" :lines \"2-\"\n\n"))

(defun zd-org-insert-file (zdFile)
  "Insert title and contents of ZDFILE."
  (insert
    ;; Insert org-mode title
    "\n* " (zd-lift-file-title zdFile) "\n\n"
    ;; Insert file contents (without the first 3 lines)
    (zd-file-contents zdFile 3)))

(defun zd-org-graph-search (str)
  "Insert org source block for graph with zd search results.
STR should be the search the resulting notes of which should be included in the graph."
  (interactive (list (read-string "search string: ")))
  (setq zd-graph--links (list))
  (let ((zdList (zd-get-file-list str)))
    (insert zd-graph-syntax-begin)
    (insert "\n  // links\n")
    (dolist (oneFile zdList)
      (insert "\n")
      (zd-graph-insert-links oneFile))
    (zd-graph-insert-all-titles))
  (insert zd-graph-syntax-end))

(defun zd-org-graph-note (deftFile)
  "Create a graph starting from note DEFTFILE."
  (interactive (list
    (completing-read "Note to start graph from: "
      (deft-find-all-files))))
  (setq zd-graph--links (list))
  (insert zd-graph-syntax-begin)
  (insert "\n  // base note and links \n")
  (zd-graph-insert-links deftFile)
  (zd-graph-insert-additional-links)
  (zd-graph-insert-all-titles)
  (insert zd-graph-syntax-end))

(defcustom zd-graph-syntax-begin
  "#+BEGIN_SRC dot :file ./graph.pdf :cmdline -Kfdp -Tpdf
  \n graph {\n"
  "Syntax to be included at the start of the zetteldeft graph.")

(defcustom zd-graph-syntax-end
  "} \n#+END_SRC\n"
  "Syntax to be included at the end of the zetteldeft graph.")

(defun zd-extract-links (deftFile)
  "Find all links in DEFTFILE and return a list."
  (let ((zdLinks (list)))
    (with-temp-buffer
      (insert-file-contents deftFile)
      (while (re-search-forward zd-id-regex nil t)
        (let ((foundTag (replace-regexp-in-string " " "" (match-string 0))))
          ;; Add found tag to zdLinks if it isn't there already
          (unless (member foundTag zdLinks)
            (push foundTag zdLinks)))
        ;; Remove found tag from buffer
        (delete-region (point) (re-search-backward zd-id-regex))))
   zdLinks))

(defun zd-graph-insert-links (deftFile)
  "Insert links in DEFTFILE in dot graph syntax on a single line.
Any inserted ID is also stored in `zd-graph--links'."
  (insert "  \""
          (zd-lift-id deftFile)
          "\" -- {")
  (dolist (oneLink (zd-extract-links deftFile))
    (zd-graph-store-link oneLink t)
    (insert "\"" oneLink "\" "))
  (insert "}\n")
  (zd-graph-store-link deftFile))

(defun zd-graph-insert-title (deftFile)
  "Insert the DEFTFILE title definition in a one line dot graph format."
  (let ((zdTitle (replace-regexp-in-string "\"" "" (zd-lift-file-title deftFile)))
        (zdId    (zd-lift-id deftFile)))
    (insert "  \"" zdId "\""
            " [label = \"" zdTitle " (" zd-link-indicator zdId ")\"")
    (insert "]" "\n"))
  (zd-graph-store-link deftFile))

(defun zd-graph-store-link (deftFile &optional idToFile)
  "Push DEFTFILE to zd-graph--links unless it's already there.
When IDTOFILE is non-nil, DEFTFILE is considered an id
and the the function first looks for the corresponding file."
  (when idToFile
    (let ((deft-filter-only-filenames t))
      (progn
        (deft-filter deftFile t)
        (setq deftFile (car deft-current-files)))))
  (unless (member deftFile zd-graph--links)
    (push deftFile zd-graph--links)))

(defun zd-graph-insert-additional-links ()
  "Insert rest of `zd-graph--links'."
  (setq zd-graph--links (cdr zd-graph--links))
  (dolist (oneFile zd-graph--links)
    (zd-graph-insert-links oneFile)))

(defun zd-graph-insert-all-titles ()
  "Insert all graphviz title lines for all links stored in `zd-graph--links'."
  (insert "\n  // titles \n")
  (dolist (oneLink zd-graph--links)
    ;; Sometimes, a 'nil' list item is present. Ignore those.
    (when oneLink
      (zd-graph-insert-title oneLink))))

(font-lock-add-keywords 'org-mode
  `((,(concat zd-link-indicator zd-id-regex) . font-lock-warning-face)))

(provide 'zetteldeft)
;;; zetteldeft.el ends here
