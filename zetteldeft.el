;;; zetteldeft.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  EFLS

;; Author: EFLS <email>
;; Keywords: deft zettelkasten zetteldeft
;; Version: 0.0.1

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

;; Zetteldeft: turn deft into a zettelkasten writing system to create linked notes.

;;; Code:

(require 'deft)

(unless (package-installed-p 'avy)
  (user-error 'zetteldeft "Avy not installed, required for zd-avy-* functions."))
(require 'avy)

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
  "Search deft with thing-at-point as filter.
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
Open if there is only one result (in another window if otherWindow is non-nill)."
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
  "Returns a list of files with the search item SRCH."
  (let ((deft-current-sort-method 'title))
    (deft-filter srch t)
    deft-current-files))

(defcustom zd-id-format "%Y-%m-%d-%H%M"
  "Format used when generating zetteldeft IDs.
Be warned: the regexp to find these IDs is set separately."
  :type 'string
  :group 'zetteldeft)

(setq deft-new-file-format zd-id-format)

(defun zd-generate-id ()
  "Generates an id in `zd-id-format'."
  (format-time-string zd-id-format))

(defcustom zd-id-regex "[0-9]\\{4\\}\\(-[0-9]\\{2,\\}\\)\\{3\\}"
  "The regex used to search for zetteldeft IDs."
  :type 'string
  :group 'zetteldeft)

(defun zd-lift-id (str)
  "Extract the zetteldeft ID from STR with the regular expression stored in `zd-id-regex'."
  (with-temp-buffer
    (insert str)
    (when (re-search-forward zd-id-regex nil t -1)
      (match-string 0))))

(defun zd-find-file (file)
  "Open deft file FILE."
  (interactive
    (list (completing-read "Deft find file: "
           (deft-find-all-files-no-prefix))))
  (deft-find-file file))

(defun zd-find-file-id-insert (file)
  "Find deft file FILE and insert its link id, prepended by §."
  (interactive (list
    (completing-read "File to insert id from: "
      (deft-find-all-files-no-prefix))))
  (insert (concat "§" (zd-lift-id file))))

(defun zd-find-file-full-title-insert (file)
  "Find deft file FILE and insert its link id with title, prepended by §."
  (interactive (list
    (completing-read "File to insert full title from: "
      (deft-find-all-files-no-prefix))))
  (insert (concat "§" (file-name-base file))))

(defun zd-new-file (str &optional empty)
  "Create a new deft file. Filename is `zd-id-format' appended by STR. No extension needed.

After creating, the title is inserted in org-mode format (unless EMPTY is true) and the full file name is added to the kill ring."
  (interactive (list (read-string "name: ")))
  (let* ((zdId (zd-generate-id))
         (zdName (concat zdId " " str)))
  (deft-new-file-named zdName)
  (kill-new zdName)
  (unless empty (zd-insert-org-title))
  (when (featurep 'evil) (evil-insert-state))))

(defun zd-new-file-and-link (str)
  "Inserts generated id with `zd-id-format' appended with STR.
Creates new deft file with id and STR as name."
  (interactive (list (read-string "name: ")))
  (insert "§" (zd-generate-id) " " str)
  (zd-new-file str))

(defun zd-avy-tag-search ()
  "Call on avy to jump and search tags indicated with #."
  (interactive)
  (save-excursion
   (avy-goto-char ?#)
   (zd-search-at-point)))

(defun zd-avy-link-search ()
  "Call on avy to jump and search link ids indicated with §.
Opens immediately if there is only one result."
  (interactive)
  (save-excursion
    (avy-goto-char ?§)
    (zd-search-global (zd-lift-id (zd-get-thing-at-point)))))

(defun zd-avy-file-search (&optional otherWindow)
 "Call on avy to jump to link ids indicated with § and use it to search for filenames.
Open that file (when it is the only search result, and in another window if OTHERWINDOW)."
  (interactive)
  (save-excursion
    (avy-goto-char ?§)
    (zd-search-filename (zd-lift-id (zd-get-thing-at-point)) otherWindow)))

(defun zd-avy-file-search-ace-window ()
  "Call on avy to jump to link ids indicated with § and use it to search for filenames.
When there is only one search result, as there should be, open that file in a window selected through `ace-window'."
  (interactive)
  (require 'ace-window)
  (save-excursion
    (avy-goto-char ?§)
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
  "Checks if the currently visited file is in `zetteldeft' territory: whether it has `deft-directory' somewhere in its path."
  (unless (buffer-file-name)
    (user-error "Buffer not visiting a file."))
  (unless (string-match-p
            (regexp-quote deft-directory)
            (file-name-directory (buffer-file-name)))
    (user-error "Not in zetteldeft territory.")))

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
  "Update the #+TITLE in the current file, if present."
  (save-excursion
    (let ((zd-string-after-title ""))
      (goto-char (point-min))
      (when (search-forward "#+title:" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (zd-insert-org-title)))))

(defun zd-lift-file-title (zdFile)
  "Returns the title of a zetteldeft note.
ZDFILE should be a full path to a note."
  (let ((baseName (file-name-base zdFile)))
    (replace-regexp-in-string
     "[0-9]\\{2,\\}-[0-9-]+[[:space:]]"
     "" baseName)))

(defun zd-insert-org-title ()
  "Insert filename of current file as org syntax."
  (interactive)
  (zd--check)
  (insert
    "#+title: "
    (zd-lift-file-title (file-name-base (buffer-file-name)))
    zd-string-after-title))

(defcustom zd-string-after-title ""
  "String inserted below title when `zd-insert-org-title' is called.
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
  (let ((ID (concat "§" (zd-lift-id (file-name-base (buffer-file-name))))))
    (kill-new ID)
    (message "%s" ID)))

(defun zd-id-to-full-title (zdID)
  "Return full title from given zetteldeft ID.
Throws an error when either none or multiple files with said ID are found."
  (let ((deft-filter-only-filenames t))
    (deft-filter zdID t))
  (unless (eq (length deft-current-files) 1)
    (user-error "ID Error. Either no or multiple zetteldeft files found with ID %s." zdID))
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

(defcustom zd-tag-format "\\(^\\|\s\\)[#@][a-z-]+"
  "Regular expression used to filter out tags."
  :type 'string
  :group 'zetteldeft)

(defun zd-extract-tags (deftFile)
  "Find all tags in DEFTFILE and add them to zd-tag-list"
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
  "Inserts at point a list of links to all deft files with a search string ZDSRCH.
When searching for a tag, include # manually in the search."
  (interactive (list (read-string "search string: ")))
  (dolist (zdFile (zd-get-file-list zdSrch))
    (zd-list-entry-file-link zdFile)))

(defun zd-insert-list-links-missing (zdSrch)
  "Inserst a list of links to all deft files with a search string ZDSRCH, yet in contrast to `zd-insert-list-links' only includes links that are not yet present in the current file.
Can only be called from a file in the zetteldeft directory."
  (interactive (list (read-string "search string: ")))
  (zd--check)
  (let (zdCurrentIDs zdFoundIDs zdFinalIDs)
    (setq zdCurrentIDs (zd-extract-links (buffer-file-name)))
    ; filter IDs from search results
    (dolist (zdFile (zd-get-file-list zdSrch))
      (push (zd-lift-id zdFile) zdFoundIDs))
    ; create new list with unique ids
    (dolist (zdID zdFoundIDs)
      (unless (member zdID zdCurrentIDs)
        (push zdID zdFinalIDs)))
    ; finally find full title for each ID and insert it
    (dolist (zdID zdFinalIDs)
      (setq zdID (zd-id-to-full-title zdID))
      (insert " - " (concat "§" zdID "\n")))))

(defun zd-list-entry-file-link (zdFile)
  "Insert ZDFILE as list entry."
  (insert " - " (concat "§" (file-name-base zdFile)) "\n"))

(defun zd-org-search-include (zdSrch)
  "Inserts at point org-mode code to include all files with the selected tag. Include the # manually in the prompt."
  (interactive (list (read-string "tag (include the #): ")))
  (dolist (zdFile (zd-get-file-list zdSrch))
    (zd-org-include-file zdFile)))

(defun zd-org-search-insert (zdSrch)
  "Inserts at point all the content of the files with ZDSRCH. When looking for zetteldeft tags, include the # manually in the search."
  (interactive (list (read-string "Search term: ")))
  (dolist (zdFile (zd-get-file-list zdSrch))
    (zd-org-insert-file zdFile)))

(defun zd-file-contents (zdFile &optional removeLines)
  "Inserts file contents of a zetteldeft note.
ZDFILE should be a full path to a note.

Optional: leave out first REMOVELINES lines."
  (with-temp-buffer
    (insert-file-contents zdFile)
    (when removeLines
      (kill-whole-line removeLines))
    (buffer-string)))

(defun zd-org-include-file (zdFile)
  "Insert code to include org-file zdFile."
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
  "Insert org source block for graph with zd search results. STR should be the search the resulting notes of which should be included in the graph."
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
  (interactive)
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
  "Inserts a file's links in a one line dot graph format.
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
  "Inserts the DEFTFILE title definition in a one line dot graph format."
  (let ((zdTitle (replace-regexp-in-string "\"" "" (zd-lift-file-title deftFile)))
        (zdId    (zd-lift-id deftFile)))
    (insert "  \"" zdId "\""
            " [label = \"" zdTitle " (§" zdId ")\"")
    (insert "]" "\n"))
  (zd-graph-store-link deftFile))

(defun zd-graph-store-link (deftFile &optional idToFile)
  "Push DEFTFILE to zd-graph--links unless it's already there.
When IDTOFILE is non-nil, DEFTFILE is considered an id and the the function first looks for the corresponding file."
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

(font-lock-add-keywords 'org-mode '(
  ("§[0-9]\\{2,\\}-[0-9-]+" . font-lock-warning-face)))

(provide 'zetteldeft)
;;; zetteldeft.el ends here
