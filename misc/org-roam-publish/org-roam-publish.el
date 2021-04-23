;;; misc/org-roam-publish/org-roam-publish.el -*- lexical-binding: t; -*-

(require 'org)
(require 'org-roam)
(require 'bibtex)

(defcustom org-roam-publish--wiki-path nil "Target path for the wiki")

(defun org-roam-publish--compile-cite-keys (file)
  "Extract all the org-ref cite-keys in file"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char 1)
    (let ((keys '()))
      (org-element-map (org-ref-parse-buffer) 'link
        (lambda (link)
          (let ((plist (nth 1 link)))
            (when (-contains? org-ref-cite-types
                              (plist-get plist :type))
              (cl-dolist (key (org-ref-split-and-strip-string
                               (plist-get plist :path)))
                (push key keys))))))
      keys)))

(defun org-roam-publish--compile-bibtex-note-keys(files)
  "Extract the files that have names corresponding to bibtex entries"
  (let* ((file-names (mapcar (lambda (x) (file-name-sans-extension (file-name-nondirectory  x))) files))
         (keys (seq-filter 'bibtex-completion-get-entry file-names)))
    keys))

(defun org-roam-publish--compile-citations (files)
  "Compile a list of all org-ref citations in files"
  (let* ((keys (apply 'append (mapcar 'org-roam-publish--compile-cite-keys files)))
         (file-names (org-roam-publish--compile-bibtex-note-keys files))
         (keys (append keys file-names)))
    (message "%s" keys)
    (message "The valid keys %s" (seq-filter 'bibtex-completion-get-entry keys))
    (message "The compiled note %s " (org-roam-publish--compile-bibtex-note-keys files))
    (message "No dups %s" (delete-dups keys))
    (cl-sort (delete-dups keys)
             'string-lessp
             :key 'downcase)))

(defun org-roam-publish--create-bibtex (keys)
  "Create bibtex string from a list of cite-keys"
  (message "Creating bibtex")
  (let* ((keys (seq-filter 'bibtex-completion-get-entry keys)))
    (s-join "\n" (--map (bibtex-completion-make-bibtex it) keys))))


(defun org-roam-publish--export-bibtex (keys target-path)
  "Export the relevant bibtex entries references in the list of files to target-path"
  (write-region (org-roam-publish--create-bibtex keys) nil (concat target-path "references.bib")))

(defun org-roam-publish--export-pdfs (keys target-path)
  "Export the relevant pdf files"
  (dolist (key keys)
    (let ((source-file (concat org-ref-pdf-directory key ".pdf"))
          (target-file (concat target-path key ".pdf")))
      (when (file-exists-p source-file)
        (copy-file source-file target-file t)))))

(defun org-roam-publish--export-org-files (files target-path)
  "Export the relevant org files to the target folder"
  (dolist (file files)
    (message file)
    (copy-file file (concat target-path (file-name-nondirectory file)) t)))

(defun org-roam-publish--img-links-in-file (file)
  "Compile a list of all image files linked in the document"
  (let ((image-files '()))
    (with-temp-buffer
      (insert-file-contents file)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let ((link-path (org-element-property :path link)))
            (when (string= (file-name-extension link-path) "png")
              (push (expand-file-name link-path (file-name-directory file)) image-files)
              )))))
    image-files))

(defun org-roam-publish--compile-image-links (files)
  "List all org image links in org files"
  (apply 'append (mapcar #'org-roam-publish--img-links-in-file files)))


(defun org-roam-publish (&optional file target-path)
  "Export a connected components of org-roam notes to a separate folder.

Exports all interconnected roam notes which are connected to ~file~ to ~target-path~.
Including relevant images and citations to a separate folder"

  (interactive
   (list (or (unless (equal current-prefix-arg '(16)) (buffer-file-name))
             (read-string "Start file: "))
         (if (or (equal current-prefix-arg '(4))
                 (equal current-prefix-arg '(16)))
             (read-string "Save files to: ")
           nil)))

  (let* ((files (seq-filter
                 (lambda (x) (string= (file-name-extension x) "org"))
                 (seq-filter 'file-exists-p (org-roam-db--connected-component file))))
         (images (org-roam-publish--compile-image-links files))
         (keys (org-roam-publish--compile-citations files))
         (target-path (file-name-as-directory (or target-path org-roam-publish--wiki-path)))
         ;; (pdf-path (concat target-path "pdfs/"))
         (image-path (concat target-path "img/")))

    (make-directory target-path t)
    (org-roam-publish--export-org-files files target-path)
    (rename-file (concat target-path (file-name-nondirectory file)) (concat target-path "README.org") 't)
    (org-roam-publish--export-bibtex keys target-path)
    ;; (make-directory pdf-path t)
    ;; (org-roam-publish--export-pdfs keys pdf-path)
    (make-directory image-path t)
    (org-roam-publish--export-org-files images image-path)))

;; (setq org-roam-publish--wiki-path "~/git/meta-learning-scrapbook/")
;; (org-roam-publish--export-wiki "/home/lukas/org/roam/few-shot_overview.org")

(provide 'org-roam-publish)
