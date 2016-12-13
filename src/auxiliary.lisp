;;; Copyright (c) 2016, Fereshta Yazdani <yazdani@cs.uni-bremen.de>
;;; All rights reserved.
;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Institute for Artificial Intelligence/
;;;       Universitaet Bremen nor the names of its contributors may be used to 
;;;       endorse or promote products derived from this software without 
;;;       specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :hmi-cram)

(defun get-elem-by-type (name)
 (let*((type NIL)
       (sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
       (new-hash (copy-hash-table sem-hash))
       (sem-keys (hash-table-keys sem-hash)))
       (dotimes(i (length sem-keys))
         (if(string-equal name (nth i sem-keys))
            (cond ((search "tree" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                   (setf type "tree"))
                  ((search "rock" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                   (setf type "rock"))
                  ((search "pylon" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                   (setf type "pylon"))
                  ((search "house" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                   (setf type "house"))
                  (t (setf type (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))))))
   type))

(defun get-elem-by-bboxsize (objname)
   (let*((sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
         (new-hash (copy-hash-table sem-hash))
         (dim (slot-value (gethash objname new-hash) 'sem-map-utils:dimensions))
         (dim-x (cl-transforms:x dim))
         (dim-y (cl-transforms:y dim))
         (dim-z (cl-transforms:z dim)))
     ;;(format t "size ~a~%"  (+ dim-x dim-y dim-z))
   (+ dim-x dim-y dim-z)))

(defun get-elem-by-pose (objname)
 (let*((pose NIL)
       (sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
       (new-hash (copy-hash-table sem-hash))
       (sem-keys (hash-table-keys sem-hash)))
       (dotimes (i (length sem-keys))
         do(if (string-equal objname (nth i sem-keys))
               (setf pose (slot-value (gethash objname new-hash) 'sem-map-utils:pose))
               (format t "")))
   pose))

(defun direction-symbol (sym)
  (intern (string-upcase sym) "KEYWORD"))

;;;
;;; 
;;;
(defun get-next-elem-depend-on-prev-elem (typ spatial name)
  (format t "typ ~a~% spatial ~a~% name ~a~%" typ spatial name)
  (let*((liste (get-elems-of-semmap-by-type typ))
        (resultlist '())
        (result NIL))
   (format t "liste ~a~%" liste)
    (format t "typ ~a~% spatial ~a~% name ~a~%" typ spatial name)
   (dotimes (index (length liste))
       (format t "hieer ~%")
     (if (and (not (null (checker-elems-by-relation->get-elems-by-tf
                   (nth index liste) name spatial)))
              (> 5 (get-distance (get-elem-by-pose name) (get-elem-by-pose (nth index liste)))))
         (setf resultlist (append resultlist (list 
                                                   (format NIL "~a:~a"(nth index liste)
                                                           (get-distance
                                                            (get-elem-by-pose
                                                             (nth index liste))
                                                            (get-elem-by-pose name))))))))
    (format t "resultlist ~a~%" resultlist)
    (if (null resultlist)
        (setf result NIL)
        (setf result  (sort-list resultlist)))
    ;;(format t "elem result ~a~%" result)
    result))

(defun get-next-elem-depend-on-prev-elem-no-con (typ spatial name)
 ;; (format t "typ ~a~% spatial ~a~% name ~a~%" typ spatial name)
  (let*((liste (get-elems-of-semmap-by-type typ))
        (resultlist '())
        (result NIL))
   ;; (format t "liste ~a~%" liste)
   ;; (format t "typ ~a~% spatial ~a~% name ~a~%" typ spatial name)
   (dotimes (index (length liste))
       
     (if (not (null (checker-elems-by-relation->get-elems-by-tf
                   (nth index liste) name spatial)))
         (setf resultlist (append resultlist (list 
                                              (format NIL "~a:~a"(nth index liste)
                                                      (get-distance
                                                       (get-elem-by-pose
                                                        (nth index liste))
                                                       (get-elem-by-pose name))))))))
    (setf result (sort-list resultlist))
    (format t "elem result ~a~%" result)
    result))

(defun get-prev-elem-depend-on-next-elem-no-con (typ spatial name)
 ;; (format t "typ ~a~% spatial ~a~% name ~a~%" typ spatial name)
  (let*((liste (get-elems-of-semmap-by-type typ))
        (resultlist '())
        (result NIL))
   (dotimes (index (length liste))
     (if (not (null (checker-elems-by-relation->get-elems-by-tf
                    name (nth index liste) spatial)))
         (setf resultlist (append resultlist (list 
                                              (format NIL "~a:~a"(nth index liste)
                                                      (get-distance
                                                       (get-elem-by-pose
                                                        (nth index liste))
                                                       (get-elem-by-pose name))))))))
    (setf result (sort-list resultlist))
    (format t "elem result ~a~%" result)
    result))

(defun get-prev-elem-depend-on-next-elem (typ spatial name)
  (format t "get-next-elem ~a~%"name)
 (let*((liste (get-elems-of-semmap-by-type typ))
       (resultlist '()))
   (dotimes (index (length liste))
     (format t "get-next-elem ~a~%" (nth index liste))
        (if  (and (not (null (checker-elems-by-relation->get-elems-by-tf
                   name (nth index liste) spatial)))
                  (> 5 (get-distance (get-elem-by-pose name) (get-elem-by-pose (nth index liste)))))
         (setf resultlist (append resultlist (list 
                                                   (format NIL "~a:~a"(nth index liste)
                                                           (get-distance
                                                            (get-elem-by-pose
                                                             (nth index liste))
                                                            (get-elem-by-pose name))))))))
   (format t "neue luste ~a~%" resultlist)
   (first (split-sequence:split-sequence #\: (first (sort-list resultlist))))))

(defun get-elems-of-semmap-by-type (type)
  (format t "get-elems-of-semmap-by-type ~a~%" type)
  (let*((sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
       (sem-keys (hash-table-keys sem-hash))
       (types '()))
    (dotimes (index (length sem-keys))
      (if (string-equal type (get-elem-by-type (nth index sem-keys)))
          (setf types (append types
                               (list (nth index sem-keys))))))
    (format t "ttype ~a~%" types)
    types))

(defun get-elem-by-type->get-elems-by-type (type)
;;  (format t "get-elem-by-type->get-elems-by-type ~a~%" type)
 (first (split-sequence:split-sequence #\: (first (get-elems-by-type (get-elems-agent-front-by-dist) type)))))

(defun get-elem-by-bboxsize->get-elems-agent-front-by-dist (objtype shape)
 (let*((liste (get-elems-agent-front-by-dist))
       (objtypliste '())
       (objdistliste '())
       (result NIL))
     (dotimes (index (length liste))
       (cond((string-equal objtype
                           (get-elem-by-type (first (split-sequence:split-sequence #\: (nth index liste)))))
             (setf objtypliste (append objtypliste (list (nth index liste))))
             (cond ((> 3 (length objdistliste))
                    (format t "index ~a~%"  (nth index liste))
                     (setf objdistliste (append objdistliste
                                               (list (get-elem-by-bboxsize (first (split-sequence:split-sequence #\: (nth index liste))))))))))))
   (if(string-equal "small" shape)
       (if (> (first objdistliste)
              (second objdistliste))
              (setf result (first (split-sequence:split-sequence #\: (second objtypliste))))
              (setf result (first (split-sequence:split-sequence #\: (first objtypliste)))))
       (if (> (first objdistliste)
              (second objdistliste))
              (setf result (first (split-sequence:split-sequence #\: (first objtypliste))))
              (setf result (first (split-sequence:split-sequence #\: (second objtypliste))))))
   result))

(defun get-elems-by-type (liste type)
  (let((types '()))
       (dotimes (index (length liste))
         (if(string-equal type (get-elem-by-type (first (split-sequence:split-sequence #\: (nth index liste)))))
         (setf types (cons (nth index liste) types))))
       (reverse types)))
           
 (defun get-elem-by-range->get-elems-by-type (type range)
   (let* ((types (get-elems-agent-front-by-type  type))
          (result NIL))
   (dotimes (index (length types))
     (cond ((string-equal "one" range)
            (setf result (first (split-sequence:split-sequence #\: (first types)))))
           ((string-equal "two" range)
            (setf result (first (split-sequence:split-sequence #\: (second types)))))
           ((string-equal "three" range)
            (setf result (first (split-sequence:split-sequence #\: (third types)))))))
     result))

(defun tf-human-to-map ()
  (let ((var (cl-transforms:transform->pose (cl-tf:lookup-transform *tf* "map" "human"))))
    (publish-body var)
    var))

 (defun sort-list (liste)
   (dotimes (index (length liste))
                     (setf liste (sorted-lists liste)))
                   liste)


(defun sorted-lists (liste)
  (let ((sortlist '())
        (tmp  (first liste)))
    (loop for index from 1 to (- (length liste) 1)
          do
            (let((tmpnum (read-from-string
                          (second (split-sequence:split-sequence #\: tmp))))
                 (num (read-from-string
                            (second (split-sequence:split-sequence #\: (nth index liste)))))
                 (value (nth index liste)))
             (cond ((> tmpnum  num)
                    (setf sortlist (cons value sortlist)))
                   (t
                    (setf sortlist (cons tmp sortlist))
                    (setf tmp value)
                    (setf tmpnum (read-from-string
                                  (second (split-sequence:split-sequence #\: tmp))))))))
    (setf sortlist (cons tmp sortlist))
    (reverse sortlist)))


(defun checker-elems-by-relation->get-elems-by-tf (objname1 objname2 property)
  (let*((sem-hash (get-elems-by-tf))
        (obj1-pose (gethash objname1 sem-hash))
        (obj2-pose (gethash objname2 sem-hash))
        (tmp NIL))
    (cond ((string-equal property "behind")
         (setf tmp (and (> (cl-transforms:x (cl-transforms:origin obj1-pose))
                       (cl-transforms:x (cl-transforms:origin obj2-pose)))
                        (plusp (cl-transforms:x (cl-transforms:origin obj1-pose))))))
          ((string-equal property "in-front-of")
         (setf tmp (and (< (cl-transforms:x (cl-transforms:origin obj1-pose))
                           (cl-transforms:x (cl-transforms:origin obj2-pose)))
                        (plusp (cl-transforms:x (cl-transforms:origin obj2-pose))))))
        ((string-equal property "right")
         (setf tmp (< (cl-transforms:y (cl-transforms:origin obj1-pose))
                      (cl-transforms:y (cl-transforms:origin obj2-pose)))))
        ((string-equal property "left")
         (setf tmp (> (cl-transforms:y (cl-transforms:origin obj1-pose))
                      (cl-transforms:y (cl-transforms:origin obj2-pose)))))
        ((string-equal property "close-to")
         (if (>= 4 (get-distance obj1-pose obj2-pose))
             (setf tmp T)
             (setf tmp NIL)))
        ((or (string-equal property "to")
              (string-equal property "around")
              (string-equal property "next"))
         (if (>= 20 (get-distance obj1-pose obj2-pose))
             (setf tmp T)
             (setf tmp NIL))))
    tmp))

(defun get-elems-by-tf ()
  (let* ((sem-map (sem-map-utils:get-semantic-map))
        (sem-hash (slot-value sem-map 'sem-map-utils:parts))
         (sem-keys (hash-table-keys sem-hash))
       ;;  (semm-hash (copy-hash-table sem-hash))
         (new-hash (make-hash-table))(name NIL)
         (obj-pose NIL))
    (dotimes (index (length sem-keys))
      (let*((pose (get-elem-pose (nth index sem-keys)))
            (pub (cl-tf:set-transform *tf* (cl-transforms-stamped:make-transform-stamped "map" (nth index sem-keys) (roslisp:ros-time) (cl-transforms:origin pose) (cl-transforms:orientation pose))))
            (obj-pose (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* "human" (nth index sem-keys)))))
      (setf (gethash (nth index sem-keys) new-hash) obj-pose)))
(copy-hash-table new-hash)))

(defun get-elem-by-small-dist (liste)
  (let*((checker 1000)
        (elem NIL))
    (dotimes (index (length liste))
      (cond ((<= (parse-integer (second (split-sequence:split-sequence #\: (nth index liste)))) checker)
             (setf checker (parse-integer (second (split-sequence:split-sequence #\: (nth index liste)))))
             (setf elem(nth index liste)))))
    elem))

(defun get-elem-by-big-dist (liste)
  (let*((checker 0)
        (elem NIL))
    (dotimes (index (length liste))
      (cond ((>= (parse-integer (second (split-sequence:split-sequence #\: (nth index liste)))) checker)
             (setf checker (parse-integer (second (split-sequence:split-sequence #\: (nth index liste)))))
             (setf elem(nth index liste)))))
    elem))
