;;; misc/pdffigures/residuals.el -*- lexical-binding: t; -*-

(defhydra hydra--pdffigures (
                             :post (kill-buffer)
                             nil global-key-map)
  "Process current figure"
  ("s" (lambda () (pdffigures--save-current-image current-image)) "save image")
  ("S" (lambda () (pdffigures--save-current-image current-image t)) "save image as")
  ("n" (lambda () (interactive)
         (setq current-idx (min (- (length images) 1) (+ current-idx 1)))
         (pdffigures--update-image)) "next image")
  ("p" (lambda () (interactive)
         (setq pdffigures-curr-idx (max 0 (- current-idx 1)))
         (pdffigures--update-image)) "previous image")
  ("q" nil "quit"))

(defun pdffigures--iterate-over-images (image-dir)
  (interactive)
  (setq images (pdffigures--directory-images image-dir)
        current-idx 0)
  (if (> (length images) 0)
      (progn
        (switch-to-buffer "tmp")
        (pdffigures--update-image)
        (hydra--pdffigures/body)
        (message "hello")) nil))
