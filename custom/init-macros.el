(defmacro not-if (test-form then-form &rest else-forms)
  (declare (indent 2) (debug t))
  `(if (not ,test-form) ,then-form ,@else-forms))

(provide 'init-macros)
;; init-macros.el ends here
