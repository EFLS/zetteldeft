(setq deft-directory (concat org-directory "/notes/zetteldeft"))
(setq deft-recursive t)

(setq deft-extensions '("org" "md" "txt"))

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
     (t (thing-at-point 'word t)))
  ))

(defun zd-search-at-point ()
"Search deft with thing-at-point as filter.
Thing can be a double-bracketed link, a hashtag, or a word.
Open if there is only one result"
  (interactive)
  (let ((string (zd-get-thing-at-point)))
   (if string
       (zd-search-global string)
     (user-error "No search term at point")))
  )

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
 (zd-search-global (zd-id-current-file) t)
)

(defvar zd-id-format "%Y-%m-%d-%H%M"
"Format used when generating zetteldeft IDs."
)

(setq deft-new-file-format zd-id-format)

(defun zd-generate-id ()
 "Generates an id in `zd-id-format'."
 (format-time-string zd-id-format)
)

(defun zd-id-sanitized (str)
"Strip STRING from everything that is not a number or a dash."
 (replace-regexp-in-string "[^(0-9)-]+" "" str)
)

(defun zd-file-id-stripped (file)
"Returns file id stripped from given filename FILE."
 (let ((file (substring file 0 15)))
   (zd-id-sanitized file)
))

(defun zd-id-current-file ()
"Return the id from the filename the buffer is currently visiting."
 (zd-file-id-stripped (file-name-base (buffer-file-name)))
)

(defun zd-copy-id-current-file ()
"Add the id from the filename the buffer is currently visiting to the kill ring."
(interactive)
 (kill-new (zd-id-current-file))
)

(defun zd-find-file (file)
"Open deft file FILE."
 (interactive
  (list (completing-read "Deft find file: "
        (deft-find-all-files-no-prefix))))
 (deft-find-file file)
)

(defun zd-find-file-id-copy (file)
"Find deft file FILE and add its id to the kill ring."
 (interactive (list
        (completing-read "File to copy id from: "
        (deft-find-all-files-no-prefix))))
  (kill-new (concat "§" (zd-file-id-stripped file)))
)

(defun zd-find-file-id-insert (file)
"Find deft file FILE and insert its link id, prepended by §."
 (interactive (list
        (completing-read "File to insert id from: "
        (deft-find-all-files-no-prefix))))
  (insert (concat "§" (zd-file-id-stripped file)))
)

(defun zd-new-file (str)
"Create a new deft file. Filename is `zd-id-format' appended by STR. No extension needed."
 (interactive (list (read-string "name: ")))
 (let* ((zdId (zd-generate-id))
        (zdName (concat zdId " " str)))
 (deft-new-file-named zdName)
 (kill-new zdName)
))

(defun zd-new-link-and-file (str)
"Inserts generated id with `zd-id-format' appended with STR.
Creates new deft file with id and STR as name."
 (interactive (list (read-string "name: ")))
 (insert "§" (zd-generate-id) " " str)
 (zd-new-file str)
)

(defun zd-avy-tag-search ()
"Call on avy to jump and search tags indicated with #."
 (interactive)
 (save-excursion
  (avy-goto-char ?#)
  (zd-search-at-point)
))

(defun zd-avy-link-search ()
"Call on avy to jump and search link ids indicated with §."
 (interactive)
 (save-excursion
  (avy-goto-char ?§)
  (zd-search-global (zd-id-sanitized (zd-get-thing-at-point)))
))

(defun zd-avy-file-search ()
"Call on avy to jump to link ids indicated with § and use it to search for filenames."
 (interactive)
 (save-excursion
  (avy-goto-char ?§)
  (zd-search-filename (zd-id-sanitized (zd-get-thing-at-point)))
))

(with-eval-after-load 'deft
  (define-key spacemacs-deft-mode-map-prefix
    "o" 'efls/deft-open)
 )

(defun efls/deft-open ()
 (interactive)
 (deft-open-file-other-window t)
)

(with-eval-after-load 'deft
  (define-key deft-mode-map
    (kbd "s-j") 'evil-next-line)
  (define-key deft-mode-map
    (kbd "s-k") 'evil-previous-line)
  (define-key deft-mode-map
    (kbd "s-i") 'efls/deft-open)
)

;; Prefix
(spacemacs/declare-prefix "d" "deft")
;; Launch deft
(spacemacs/set-leader-keys "dd" 'deft)
;; SEARCH
 ; Search thing at point
   (spacemacs/set-leader-keys "ds" 'zd-search-at-point)
 ; Search current file id
   (spacemacs/set-leader-keys "dc" 'zd-search-current-id)
 ; Jump & search with avy 
 ;  search link as filename
    (spacemacs/set-leader-keys "df" 'zd-avy-file-search)
 ;  search link as contents
    (spacemacs/set-leader-keys "dl" 'zd-avy-link-search)
 ;  search tag as contents
    (spacemacs/set-leader-keys "dt" 'zd-avy-tag-search)
;; LINKS
 ; Insert link from filename
   (spacemacs/set-leader-keys "di" 'zd-find-file-id-insert)
;; FILES
 ; Create new file
   (spacemacs/set-leader-keys "dn" 'zd-new-file)
   (spacemacs/set-leader-keys "dN" 'zd-new-file-and-link)
;; UTILITIES
(spacemacs/set-leader-keys "dR" 'deft-refresh)
