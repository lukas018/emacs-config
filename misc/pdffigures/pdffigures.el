;;; misc/pdffigures/pdffigures.el -*- lexical-binding: t; -*-
;;; pdffiguree.el --- automated pdf-figure cropping from pdf-tools

;;; Commentary:
;;; This package provides a wrapper around the automated pdf-figure cropping tool pdf-figures
;;
;; pdffigures crops the figures in either the current page of the entire pdf
;; then prompting the user for which to save
;;
;;

(require 'pdf-macs)
;; Small macro for opening a temporary directory for saving
;; cropped images
(defmacro with-temp-dir (temp-dir &rest body)
  `(let ((,temp-dir (make-temp-file "" t)))
     (unwind-protect
         (progn
           ,@body)
       (delete-directory ,temp-dir t))))

(defcustom pdffigures-output-dir nil
  "Default output directory for pdf figures")

(defcustom pdffigures-jar-path  nil
  "The path to the jar file containing pdffigures2 assembly")

(defcustom pdffigures-kill-saved-images nil
  "If non-nil saved images are also added to the kill ring")

(defcustom pdffigures-image-output-dpi 300
  "The dpi resolution for cropped figures")

(defun pdffigures--directory-images (dir)
  "Extract all png files within the *dir*"
  (let ((image-files (seq-filter
                      (lambda (x) (string= (file-name-extension x) "png"))
                      (directory-files dir))))
    (mapcar (lambda (x) (concat (file-name-as-directory dir) x)) image-files)))

(defun pdffigures--iterate-figures (temp-dir save-dir)
  "Creates a temporary buffer and iterate over all images in *temp-dir*, asking the user which to save"
  (let* ((images (pdffigures--directory-images temp-dir)))
    (if (> (length images) 0)
    (dolist (image images)
      (with-temp-buffer
        (switch-to-buffer-other-window (current-buffer))
        (insert-file-contents image)
        (image-mode)
        (let ((target-file (concat (file-name-as-directory save-dir)
                                   (file-name-nondirectory image))))
          (when (yes-or-no-p "Keep this file: ")
            (write-file target-file)
            (when pdffigures-kill-saved-images (kill-new target-file))))))
    (message "No images detected or cropped"))))

(defun pdffigures--extract-figures (pdf-file output)
  "Extracts the figures from =pdf-files= and saves them in output"
  (when (not pdffigures-jar-path)
    (error "pdffigures-jar-path must be set"))
  (when (not (file-exists-p pdffigures-jar-path))
    (error (format "pdffigures-jar-path: file %s doesn't exist" pdffigures-jar-path)))

  (let* ((jar-path pdffigures-jar-path)
         (main-command "org.allenai.pdffigures2.FigureExtractorBatchCli")
         (command-string (format "java -cp %s %s --dpi %d -m %s %s" jar-path main-command pdffigures-image-output-dpi (file-name-as-directory output) pdf-file)))
    (message command-string)
    (shell-command command-string)))

(defun pdffigures--verify ()
  "Verify there is no common configuration misstake"
  (unless pdffigures-jar-path (error "pdffigures-jar-path is not set"))
  (unless (file-exists-p pdffigures-jar-path) (error "pdffigures-jag-path does not exist"))
  (unless (executable-find "pdfjam") (error "pdfjam could not be found on the path")))

(defun pdffigures-crop (save-dir &optional all-pages)
  "Extracts the figures in the current pdf-page and saves them in save-dir

If =all-pages= is non-nill all figures in all paths will be extracted. Otherwise, only the
figures in the current page are extracted.

Using prefix argument C-u will prompt the for a custom output dir on save.
Using prefix argument C-u C-u will prompt the user if they want to extract all pages"

  (interactive
   (list (or
          (if (and pdffigures-output-dir
                   (not (equal current-prefix-arg '(16))))
              pdffigures-output-dir)
          (read-string "Save-dir: "))
         (if (or (equal current-prefix-arg '(4))
                 (equal current-prefix-arg '(16)))
             (yes-or-no-p "Process all pages?: ")
           nil)))

  (pdffigures--verify)
  (save-excursion
    (with-temp-dir temp-dir
                   (let* ((curr-page (pdf-view-current-page))
                          (org-buffer-name (buffer-name))
                          (src-pdf (buffer-file-name))
                          (temp-pdf (concat (file-name-as-directory temp-dir) (file-name-nondirectory src-pdf)))
                          (arguments (list "pdfjam" (buffer-file-name) (or (unless all-pages (number-to-string curr-page))) "-o" temp-pdf))
                          (split-pdf-command (string-join arguments " ")))

                     ;; (message "tmp-pdf %s" temp-pdf)
                     ;; (message "save dir %s" save-dir)
                     (message split-pdf-command)
                     (shell-command split-pdf-command) ;; Call pdfjam and extract relevant pages
                     (pdffigures--extract-figures temp-pdf temp-dir) ;; Extract figures
                     (pdffigures--iterate-figures temp-dir save-dir)))))

(provide 'pdffigures)
