;; magic.toss.funny-coin.bit
;; A whimsical coin toss engine for ALN/BitBot environments
;; Author: XboxTeeJay & ImmersiveNPC-Lab

(defpackage :magic-toss
  (:use :cl)
  (:export :toss-coin))

(in-package :magic-toss)

(defparameter *coin-outcomes*
  '((:heads . "✨ The coin lands on HEADS — a rainbow portal opens above the town square!")
    (:tails . "💫 The coin lands on TAILS — a shower of golden confetti rains down on the marketplace!")
    (:edge  . "🪙 The coin lands on its EDGE — time freezes for 3 seconds, then resumes with a giggle.")))

(defun toss-coin ()
  "Tosses a magical coin and triggers a random event."
  (let* ((roll (random 100))
         (result (cond ((< roll 48) :heads)
                       ((< roll 96) :tails)
                       (t :edge)))
         (message (cdr (assoc result *coin-outcomes*))))
    (format t "[MAGIC.COIN] Toss result: ~A~%" result)
    (format t "[EVENT] ~A~%" message)
    result))

;; Optional: auto-run when loaded
#+sbcl
(toss-coin)
