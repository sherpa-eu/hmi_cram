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

;;
;; Get the position of the element out of semantic map
;; @objname: object name of the element
;;
(defun get-pose-by-elem (objname)
 (let*((pose NIL)
       (sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
       (new-hash (copy-hash-table sem-hash))
       (sem-keys (hash-table-keys sem-hash)))
       (dotimes (i (length sem-keys))
         do(if (string-equal objname (nth i sem-keys))
               (setf pose (slot-value (gethash objname new-hash) 'sem-map-utils:pose))
               (format t "")))
   pose))

;;
;; Get the bounding box size of an element that is in front of the agent
;; Calculating all objects in front of the agent, checking the type and
;; picking out the object by name and computing the bounding box
;; @objtype: object type of the calculated object
;; @shape: shape of the calculated object e.g. big or small
;;
(defun get-bboxsize-by-elem->get-elems-agent-front-by-dist (objtype shape)
 (let*((liste (get-elems-agent-front-by-dist))
       (objtypliste '())
       (objdistliste '())
       (result NIL))
     (dotimes (index (length liste))
       (cond((string-equal
              objtype
              (get-elem-by-type (first (split-sequence:split-sequence #\: (nth index liste)))))
             (setf objtypliste (append objtypliste (list (nth index liste))))
             (cond ((> 3 (length objdistliste))
                    (format t "index ~a~%"  (nth index liste))
                     (setf objdistliste (append objdistliste
                                               (list (get-bboxsize-by-elem (first (split-sequence:split-sequence #\: (nth index liste))))))))))))
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

;;
;; Get the specific element by range "first", "second", "third"
;; @type: the type of the element
;; @range; the range of the element
;;
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
;;
;; Get elements in front of the agent by specific type
;; @type: type of the object
;;
(defun get-elems-agent-front-by-type (type)
  (let*((liste (get-elems-agent-front-by-dist))
        (resultlist '()))
    (dotimes (index (length liste))
      (if(string-equal type
                       (get-elem-by-type
                        (first (split-sequence:split-sequence #\: (nth index liste)))))
         (setf resultlist (append resultlist (list (first (split-sequence:split-sequence #\: (nth index liste))))))))
    resultlist))

;;
;; Get next element based on previous element without
;; any conditions
;; @typ: element type
;; @spatial: spatial relation of the elements
;; @name: element name
;;
(defun get-next-elem-depend-on-prev-elem-no-con (typ spatial name)
  (let*((liste (get-elems-of-semmap-by-type typ))
        (resultlist '())
        (result NIL))
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
    result))


;;
;; Getting the first element based on "name", "type", "shape"
;; "spatial relation"
;; @name: current object name
;; @type: specific object type
;; @shape: object shape
;; @spatial: relation of the object
;;
(defun get-first-elem-by-name-type-shape-spatial (name type shape spatial)
  (let ((liste (get-elems-agent-front-by-type type))
        (resultlist '())
        (result NIL)
        (tmplist NIL))
    (format t "liste ~a~%" liste)
    (dotimes (index (length liste))
      (if (not (null (checker-elems-by-relation->get-elems-by-tf
                      name (nth index liste) spatial)))
          (setf resultlist (append resultlist (list 
                                                   (format NIL "~a:~a"(nth index liste)
                                                           (get-distance
                                                            (get-elem-by-pose
                                                             (nth index liste))
                                                            (get-elem-by-pose name))))))))
    (setf resultlist (sort-list resultlist))
    (dotimes (jndex (length resultlist))
        (format t "tmplist1 ~a~%" tmplist)
      (setf tmplist (append tmplist (list (get-elem-by-bboxsize
                             (first (split-sequence:split-sequence #\: (nth jndex resultlist))))))))
    (if(string-equal "big" shape)
       (cond((> (first tmplist) (second tmplist))
             (setf result (first resultlist)))
            ((< (first tmplist) (second tmplist))
             (setf result (second resultlist)))
            (t (if(> (second tmplist) (third tmplist))
                  (setf result (second resultlist))
                  (setf result (third resultlist)))))
       (cond((< (first tmplist) (second tmplist))
             (setf result (first resultlist)))
            ((> (first tmplist) (second tmplist))
             (setf result (second resultlist)))
            (t (if(< (second tmplist) (third tmplist))
                  (setf result (second resultlist))
                  (setf result (third resultlist))))))
    result))

;;
;; Get previous element depending on next element
;; without any conditions
;; @typ: element type
;; @spatial: spatial relation of the element
;; @name: element name
;;
(defun get-prev-elem-depend-on-next-elem-no-con (typ spatial name)
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
    result))


;;
;; Getting next element depending on previous element
;; @typ: object type
;; @spatial: spatial relation
;; @name: object name
;;
(defun get-next-elem-depend-on-prev-elem (typ spatial name)
  (let*((liste (get-elems-of-semmap-by-type typ))
        (resultlist '())
        (result NIL))
   (dotimes (index (length liste))
     (if (and (not (null (checker-elems-by-relation->get-elems-by-tf
                          (nth index liste) name spatial)))
              (> 5 (get-distance (get-elem-by-pose name) (get-elem-by-pose (nth index liste)))))
         (setf resultlist (append resultlist (list 
                                              (format NIL "~a:~a"(nth index liste)                                                           (get-distance
                                                                                                                                              (get-pose-by-elem
                                                                                                                                               (nth index liste))                                                          (get-elem-by-pose name))))))))
    (if (null resultlist)
        (setf result NIL)
        (setf result  (sort-list resultlist)))
    result))
