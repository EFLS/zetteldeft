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

(defun zd-search-filename (str)
"Search for deft files with string STR in filename.
Open if there is only one result."
  ;; Sanitize the filter string
  (setq str (replace-regexp-in-string "[[:space:]\n]+" " " str))
  ;; Call deft search on the filter string
  (let ((deft-filter-only-filenames t))
   (deft)
   (deft-filter str t))
  ;; If there is a single match, open the file
  (when (eq (length deft-current-files) 1)
    (deft-open-file (car deft-current-files))))

(defun zd-search-current-id ()
"Search deft with the id of the current file as filter.
Open if there is only one result."
 (interactive)
 (zd-search-global (zd-id-current-file) t))

(defcustom zd-id-format "%Y-%m-%d-%H%M"
  "Format used when generating zetteldeft IDs.
Be warned, changing this won't alter the regexp to filter IDs and find your files. This is WIP."
  :type 'string
  :group 'zetteldeft)

(setq deft-new-file-format zd-id-format)

(defun zd-generate-id ()
  "Generates an id in `zd-id-format'."
  (format-time-string zd-id-format))

(defun zd-id-insert ()
 (interactive)
 "Inserts an id in `zd-id-format'."
 (insert (zd-generate-id) " "))

(defun zd-id-sanitized (str)
"Strip STRING from everything that is not a number or a dash."
 (replace-regexp-in-string "[^(0-9)-]+" "" str))

(defun zd-file-id-stripped (file)
"Returns file id stripped from given filename FILE."
 (let ((file (substring file 0 16)))
   (zd-id-sanitized file)))

(defun zd-id-current-file ()
"Return the id from the filename the buffer is currently visiting."
 (zd-file-id-stripped (file-name-base (buffer-file-name))))

(defun zd-copy-id-current-file ()
"Add the id from the filename the buffer is currently visiting to the kill ring."
(interactive)
 (kill-new (zd-id-current-file)))

(defun zd-find-file (file)
"Open deft file FILE."
 (interactive
  (list (completing-read "Deft find file: "
        (deft-find-all-files-no-prefix))))
 (deft-find-file file))

(defun zd-find-file-id-copy (file)
"Find deft file FILE and add its id to the kill ring."
 (interactive (list
        (completing-read "File to copy id from: "
        (deft-find-all-files-no-prefix))))
  (kill-new (concat "§" (zd-file-id-stripped file))))

(defun zd-find-file-id-insert (file)
"Find deft file FILE and insert its link id, prepended by §."
 (interactive (list
        (completing-read "File to insert id from: "
        (deft-find-all-files-no-prefix))))
  (insert (concat "§" (zd-file-id-stripped file))))

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
  (zd-search-global (zd-id-sanitized (zd-get-thing-at-point)))))

(defun zd-avy-file-search ()
"Call on avy to jump to link ids indicated with § and use it to search for filenames."
 (interactive)
 (save-excursion
  (avy-goto-char ?§)
  (zd-search-filename (zd-id-sanitized (zd-get-thing-at-point)))))

(defun zd-deft-new-search ()
"Launch deft, clear filter and enter insert state."
 (interactive)
 (deft)
 (deft-filter-clear)
 (when (featurep 'evil) (evil-insert-state)))

(defun zd-file-rename ()
"Rename the current file via the deft function. Use this on files in the deft-directory."
 (interactive)
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

(defun zd-insert-org-title ()
 "Insert filename of current file as org syntax."
 (interactive)
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
  "Prints total number of words in all deft files in the minibuffer."
  (interactive)
  (let ((numWords 0))
    (dolist (deftFile deft-all-files)
      (with-temp-buffer
        (insert-file-contents deftFile)
        (setq numWords (+ numWords (count-words (point-min) (point-max))))))
    (message "Total words accross all notes: %s" numWords)))

(defun zd-insert-list-links (zdSrch)
"Inserts at point a list of links to all deft files with a search string ZDSRCH.
When searching for a tag, include # manually in the search."
 (interactive (list (read-string "search string: ")))
 (dolist (zdFile (zd-get-file-list zdSrch))
  (zd-list-entry-file-link zdFile)))

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

(defun zd-get-file-list (srch)
"Returns a list of files with the search item SRCH."
  (let ((deft-current-sort-method 'title))
   (deft-filter srch t)
   deft-current-files))

(defun zd-lift-file-title (zdFile)
  "Returns the title of a zetteldeft note.
ZDFILE should be a full path to a note."
 (let ((baseName (file-name-base zdFile)))
   (replace-regexp-in-string
    "[0-9]\\{2,\\}-[0-9-]+[[:space:]]"
    "" baseName)))

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
   "\n* " (zd-lift-file-title zdFile) "\n"
   ;; Insert #+INCLUDE: "file.org" :lines 2-
   "#+INCLUDE: \"" zdFile "\" :lines \"2-\"\n"))

(defun zd-org-insert-file (zdFile)
"Insert title and contents of ZDFILE."
  (insert
    ;; Insert org-mode title
    "\n* " (zd-lift-file-title zdFile) "\n\n"
    ;; Insert file contents (without the first 3 lines)
    (zd-file-contents zdFile 3)))

(font-lock-add-keywords 'org-mode '(
  ("§[0-9]\\{2,\\}-[0-9-]+" . font-lock-warning-face)))

(provide 'zetteldeft)
;;; zetteldeft.el ends here

(general-define-key
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
  :states '(normal visual motion emacs)
  :keymaps 'override
  "d"  '(nil :wk "deft")
  "dd" '(deft :wk "deft")
  "dD" '(zd-deft-new-search :wk "new search")
  "dR" '(deft-refresh :wk "refresh")
  "ds" '(zd-search-at-point :wk "search at point")
  "dc" '(zd-search-current-id :wk "search current id")
  "df" '(zd-avy-file-search :wk "avy file search")
  "dl" '(zd-avy-link-search :wk "avy link search")
  "dt" '(zd-avy-tag-search :wk "avy tag search")
  "di" '(zd-find-file-id-insert :wk "insert id")
  "dI" '(zd-find-file-full-title-insert :wk "insert full title")
  "do" '(zd-find-file :wk "find file")
  "dn" '(zd-new-file :wk "new file")
  "dN" '(zd-new-file-and-link :wk "new file & link")
  "dr" '(zd-file-rename :wk "rename"))
