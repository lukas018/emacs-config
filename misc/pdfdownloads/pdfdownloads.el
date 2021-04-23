;;; pdfdownloads.el -*- lexical-binding: t; -*-

(require 'org-ref-arxiv)
(require 'bibtex)
(require 'url)

(defcustom luklun/pdf-download-dir nil "Directory where to download")

(defun luklun/download-arxiv (entry target-path)
  "Try to download files from arxiv"
  (let*
      ((key (cdr (assoc "&key" entry)))
       (target-path (concat target-path key ".pdf"))
       (arxiv-num (cdr (assoc "eprint" entry))))
    (message "Arxiv Entry %s" key)
    (when (and (not (file-exists-p target-path)) arxiv-num)
      (arxiv-get-pdf arxiv-num target-path))))

(defun luklun/download-doi (entry target-path)
  "Try to download files from scipy"
  (let* ((key (cdr (assoc "&key" entry)))
         (target-path (concat target-path key ".pdf"))
         (doi (cdr (assoc "doi" entry)))
         (command (format "python3 ~/Repos/scihub.py/scihub/scihub.py -d %s -o %s" doi target-path)))
    (message "DOI Entry %s" key)
    (when (and (not (file-exists-p target-path)) doi)
      (message command)
      (shell-command command))))

(defun luklun/download-open-review (entry target-path)
  "Try to download from open-review"
  (let* ((key (cdr (assoc "&key" entry)))
         (target-path (concat target-path key ".pdf"))
         (url (cdr (assoc "url" entry))))

    (message "Open Review %s" key)
    (string-match "\\(id=\\)\\(.+$\\)" url)
    (let* ((id (match-string 2 url))
           (target-url (format "https://openreview.net/pdf?id=%s" id)))
      (when (and (not (file-exists-p target-path)) id)
        (url-copy-file target-url target-path)))))

(defun luklun/download-url (entry target-path)
  "Ask the user to provide a url for download"
  (interactive)
  (let* ((key (cdr (assoc "&key" entry)))
         (title (cdr (assoc "title" entry)))
         (target-path (concat target-path key ".pdf"))
         (url (cdr (assoc "url" entry))))
    (message "URL Entry %s" key)
    (message "targe-path %s" target-path)
    (when (and (not (file-exists-p target-path)) url)
      (if (yes-or-no-p (format "Open url for missing entry %s" title))
          (progn (browse-url url)
                 (let ((pdf-url (read-string (format "Enter URL for %s: " title))))
                   (condition-case nil
                       (url-copy-file pdf-url target-path)
                     (error (progn (message "Something went wrong") nil)))))))))

(defun luklun/bibtex-get-keys (entries key)
  "Extract specific keys from a list of bibtex entries"
  (mapcar (lambda (x) (cdr (assoc key x))) entries))

(defun luklun/download-bib (&optional target)
  "Download as many pdfs as possible from the bibliography

The method will try to determine from where a pdf can be downloaded
based on the type of entry (e.g arxiv-id, DOI etc.). If everything
else fails the user will be asked to provide the urls to the relevant
pdfs themselves.

If target is nil the function defaults to using luklun/download-dir as save dir.
"
  (interactive)
  (let* ((keys (luklun/bibtex-get-keys (bibtex-completion-candidates) "=key="))
         (entries (mapcar (lambda (x) (reftex-parse-bibtex-entry (bibtex-completion-make-bibtex x))) keys))
         (arxiv-entries (seq-filter (lambda (x) (rassoc "arXiv" x)) entries))
         (doi-entries (seq-filter (lambda (x) (assoc "doi" x)) entries))
         (open-review-entries (seq-filter (lambda (x) (string= "OpenReview.net" (cdr (assoc "publisher" x)))) entries))
         (remainder (cl-set-difference entries (append arxiv-entries doi-entries open-review-entries)))
         (remainder-with-url (seq-filter (lambda (x) (assoc "url" x)) remainder))
         (final-remainders (luklun/bibtex-get-keys (cl-set-difference remainder remainder-with-url) "&key"))
         (target (or target luklun/pdf-download-dir)))

    (dolist (entry arxiv-entries)
      (luklun/download-arxiv entry target))
    (dolist (entry open-review-entries)
      (luklun/download-open-review entry target))
    (dolist (entry doi-entries)
      (luklun/download-doi entry target))
    (dolist (entry remainder-with-url)
      (luklun/download-url entry target))
    final-remainders))

(setq luklun/pdf-download-dir biblio-download-directory)

(provide 'pdfdownloads)
