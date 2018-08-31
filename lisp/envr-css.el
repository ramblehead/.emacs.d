;;; envr-css.el --- ramblehead's tide configuration

(defun rh-skewer-css-clear-all ()
  (interactive)
  (skewer-css-clear-all)
  (message "All skewer CSS modifications are cleared"))

(defun rh-skewer-css-eval-current-declaration ()
  (interactive)
  (if (not (looking-back "}[\n\r\t\s]*"))
      (skewer-css-eval-current-declaration)
    (skewer-css-eval-current-rule)))

;; (defun rh-skewer-css-eval-current-declaration ()
;;   (interactive)
;;   (if (not (looking-back "}[\n\r\t\s]*"))
;;       (message "looking at declaration")
;;     (message "looking at rule")))

(defun rh-setup-css-skewer ()
  (interactive)
  (skewer-css-mode 1)
  (bind-keys :map skewer-css-mode-map
             ("<f6>" . rh-skewer-css-clear-all)
             ("<f5>" . rh-skewer-css-eval-current-declaration)))

(provide 'envr-css)
