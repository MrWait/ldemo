(defun our-third (x)
  (car (cdr (cdr x))))

(defun sum-greater (x y z)
  (> (+ x y) z))

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))


(defun askem (string)
  (format t "~A" string)
  (read))

(defun ask-number ()
  (format t "Please enter a number~%")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares-other (i end)
  (if (> i end)
      'done
      (progn
        (format t "~A ~A~%" i (* i i))
        (show-squares-other (+ i 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-length-r (lst)
  (if (null lst)
      0
      (+ (our-length-r (cdr lst)) 1)))

(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))
