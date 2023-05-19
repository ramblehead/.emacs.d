;;; ramblehead's color-theme-sanityinc-tomorrow configuration

(defun color-theme-sanityinc-tomorrow-blue:after ()
  (load-theme 'rh-sanityinc-tomorrow-blue t))

(defun load-theme-sanityinc-tomorrow-blue:after (_theme &rest _args)
  (when (member 'sanityinc-tomorrow-blue custom-enabled-themes)
    (when (not (member 'rh-sanityinc-tomorrow-blue custom-enabled-themes))
      (load-theme 'rh-sanityinc-tomorrow-blue t))))

(advice-add 'load-theme :after
            #'load-theme-sanityinc-tomorrow-blue:after)

(advice-add 'enable-theme :after
            #'load-theme-sanityinc-tomorrow-blue:after)

(advice-add 'color-theme-sanityinc-tomorrow-blue :after
            #'color-theme-sanityinc-tomorrow-blue:after)

(provide 'config-color-theme-sanityinc-tomorrow)
