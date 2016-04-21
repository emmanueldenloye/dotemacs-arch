;; "Int -> Double -> (Int -> Double) -> Double"

;; (defun haskell-count-function-args (text)
;;   "Count the number of arguments in the function signature.
;; This is not so way meant to be complete or robust."
;;   (with-temp-buffer
;;     (insert text)
;;     (- (count-matches "->" (point-min) (point-max) nil)
;;        (count-matches "(.*->.*)" (point-min) (point-max) nil))))

;; (count-matches "[A-Z]?[\.]?[a-zA-Z\s-\.]+ ->" (point-min) (point-max) nil)
;; (count-matches "(.*) ->" (point-min) (point-max) nil)

;; (haskell-count-function-args "(a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a")

;; (string-match "(.*)")

;; (count-if (string-match-p "\(\<\w+\>\|(.*)\)"))
