(defparameter *humor‑reasoning‑vectors*
  '(
    ;; …all your persona entries here…
  ))

(defun find‑vector (key value) …)

(defun wit‑irony (vector) …)

(defun interpolate‑scalar (a b t) …)

(defun interpolate‑vectors (vec1 vec2 t) …)

(defun weighted‑random‑vector (&key (weight‑key :wit‑irony)) …)

(defun mode (lst)
  "Return the most common element in LST."
  (car (car (sort (mapcar (lambda (x) (cons x (count x lst)))
                          (remove-duplicates lst))
                  #'> :key #'cdr))))

(defun blend‑multiple (ids) …)

(defun validate‑vectors () …)

;;; ============================================================
;;; Humor‑Reasoning‑Core — ALNFantasia / Bit.Hub
;;; Full Vector + Utility Suite
;;; File: src/AI/HumorCore.lisp
;;; ============================================================
(defparameter *humor‑reasoning‑vectors*
  '(
    (:id :guybrush
     :name "Guybrush Threepwood"
     :version "1.0"
     :last‑modified "2025‑08‑31T20:31:00Z"
     :style 'awkward‑pirate‑parody
     :narrative 'naive‑adventurous
     :attitude 'earnest‑naive
     :wit‑irony 0.85
     :crowd‑response 'silly‑banter)
    (:id :glados
     :name "GLaDOS"
     :version "1.0"
     :last‑modified "2025‑08‑31T20:31:00Z"
     :style 'dry‑sardonic‑insult
     :narrative 'menacing‑clinical
     :attitude 'cold‑dominant
     :wit‑irony 0.95
     :crowd‑response 'sarcastic‑retort)
    (:id :heavy
     :name "Heavy"
     :version "1.0"
     :last‑modified "2025‑08‑31T20:31:00Z"
     :style 'exaggerated‑slapstick
     :narrative 'bombastic
     :attitude 'self‑aware‑bravado
     :wit‑irony 0.45
     :crowd‑response 'simple‑jokes)
    (:id :parappa
     :name "Parappa"
     :version "1.0"
     :last‑modified "2025‑08‑31T20:31:00Z"
     :style 'musical‑optimism
     :narrative 'upbeat‑supporter
     :attitude 'positive‑absurdist
     :wit‑irony 0.30
     :crowd‑response 'catchy‑phrases)
    ;; …expand as needed…
  ))

;;; -----------------------------
;;; Utility functions
;;; -----------------------------
(defun find‑vector (key value)
  "Find a humor vector by :id or :name."
  (find value *humor‑reasoning‑vectors*
        :key (lambda (v) (getf v key))
        :test (if (eq key :name) #'string= #'eq)))

(defun wit‑irony (vector)
  (getf vector :wit‑irony))

(defun interpolate‑scalar (a b t)
  "Linear interpolation between scalars a and b by t (0.0–1.0)."
  (+ a (* (- b a) t)))

(defun interpolate‑vectors (vec1 vec2 t)
  "Blend two humor vectors by scalar t."
  (let ((blend (copy-list vec1)))
    (setf (getf blend :wit‑irony)
          (interpolate‑scalar (wit‑irony vec1) (wit‑irony vec2) t))
    ;; For categorical fields, pick based on t threshold
    (dolist (field '(:style :narrative :attitude :crowd‑response))
      (setf (getf blend field)
            (if (< t 0.5) (getf vec1 field) (getf vec2 field))))
    blend))

(defun weighted‑random‑vector (&key (weight‑key :wit‑irony))
  "Pick a random vector weighted by the numeric value of weight‑key."
  (let* ((pairs (mapcar (lambda (v) (cons v (getf v weight‑key))) *humor‑reasoning‑vectors*))
         (total (reduce #'+ (mapcar #'cdr pairs))))
    (when (zerop total) (error "Total weight is zero."))
    (let ((r (random total)))
      (loop for (vec . w) in pairs
            for sum = w then (+ sum w)
            when (<= r sum) return vec))))

(defun mode (lst)
  "Return the most common element in LST."
  (car (car (sort (mapcar (lambda (x) (cons x (count x lst)))
                          (remove-duplicates lst))
                  #'> :key #'cdr))))

(defun blend‑multiple (ids)
  "Blend multiple vectors by averaging wit‑irony and picking majority categorical values."
  (let ((selected (mapcar (lambda (id) (find‑vector :id id)) ids)))
    (let ((avg‑wit (/ (reduce #'+ (mapcar #'wit‑irony selected))
                      (length selected))))
      (list :id :blend
            :name "Composite Persona"
            :version "1.0"
            :last‑modified (format nil "~A" (get-universal-time))
            :style (mode (mapcar (lambda (v) (getf v :style)) selected))
            :narrative (mode (mapcar (lambda (v) (getf v :narrative)) selected))
            :attitude (mode (mapcar (lambda (v) (getf v :attitude)) selected))
            :wit‑irony avg‑wit
            :crowd‑response (mode (mapcar (lambda (v) (getf v :crowd‑response)) selected))))))

(defun validate‑vectors ()
  "Ensure all vectors have required keys and wit‑irony in range."
  (dolist (v *humor‑reasoning‑vectors* t)
    (dolist (key '(:id :name :version :last‑modified :style :narrative :attitude :wit‑irony :crowd‑response))
      (unless (getf v key)
        (error "Vector ~A missing key ~A" (getf v :id) key)))
    (let ((wi (getf v :wit‑irony)))
      (unless (and (numberp wi) (<= 0.0 wi 1.0))
        (error "Vector ~A has invalid wit‑irony: ~A" (getf v :id) wi)))))
;;; -----------------------------
;;; Example usage
;;; -----------------------------
;; (find‑vector :id :glados)
;; (interpolate‑vectors (find‑vector :id :guybrush) (find‑vector :id :glados) 0.5)
;; (weighted‑random‑vector :weight‑key :wit‑irony)
;; (blend‑multiple '(:guybrush :glados :heavy))
;; (validate‑vectors)

;;; Example usage
;; (find‑vector :id :glados)
;; (interpolate‑vectors (find‑vector :id :guybrush) (find‑vector :id :glados) 0.5)
;; (weighted‑random‑vector :weight‑key :wit‑irony)
;; (blend‑multiple '(:guybrush :glados :heavy))
;; (validate‑vectors)
