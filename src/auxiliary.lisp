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
;; Get elements in front of the agent by specific type
;; @type: type of the object
;; 
(defun get-front-elems-of-agent-by-type (type &optional (viewpoint "busy-genius"))
  (let*((liste (get-front-elems-of-agent viewpoint))
        (resultlist '()))
    (dotimes (index (length liste))
      (if(string-equal type
                       (get-type-by-elem (nth index liste)))
         (setf resultlist (append resultlist (list (nth index liste))))))
    resultlist))

                      
;; Checking the relation of the objects. See if obj1 satisfy the
;; property towards obj2 or so... 
(defun check-elems-by-relation->get-elems-in-tf (objname1 objname2 property)
  (let*((sem-hash (get-elems-in-tf-agent))
        (obj1-pose (gethash objname1 sem-hash))
        (obj2-pose (gethash objname2 sem-hash))
        (tmp NIL))
    (publish-pose obj1-pose :id 72827)
    (publish-pose obj2-pose :id 272827)
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

(defun get-elems-in-tf (&optional (viewpoint "busy-genius"))
  (let* ((sem-map (sem-map-utils:get-semantic-map))
        (sem-hash (slot-value sem-map 'sem-map-utils:parts))
         (sem-keys (hash-table-keys sem-hash))
       ;;  (semm-hash (copy-hash-table sem-hash))
         (new-hash (make-hash-table))(name NIL)
         (obj-pose NIL))
    (dotimes (index (length sem-keys))
      (let*((pose (get-pose-by-elem (nth index sem-keys)))
            (pub (cl-tf:set-transform *tf* (cl-transforms-stamped:make-transform-stamped "map" (nth index sem-keys) (roslisp:ros-time) (cl-transforms:origin pose) (cl-transforms:orientation pose))))
            (obj-pose (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* viewpoint (nth index sem-keys)))))
      (setf (gethash (nth index sem-keys) new-hash) obj-pose)))
(copy-hash-table new-hash)))

(defun get-elems-in-tf-agent (&optional (viewpoint "busy-genius"))
  (let* ((sem-map (sem-map-utils:get-semantic-map))
         (sem-hash (slot-value sem-map 'sem-map-utils:parts))
         (sem-keys (hash-table-keys sem-hash))
       ;;  (semm-hash (copy-hash-table sem-hash))
         (new-hash (make-hash-table))(name NIL)
         (obj-pose NIL))
    (dotimes (index (length sem-keys))
      (let*((pose (get-pose-by-elem (nth index sem-keys)))
            (human-pose (cl-transforms:transform->pose (cl-tf:lookup-transform *tf* "map" viewpoint)))
            (obj-pose (cl-transforms:make-pose (cl-transforms:origin pose)
                                               (cl-transforms:orientation human-pose))))
      (setf (gethash (nth index sem-keys) new-hash) obj-pose)))
(copy-hash-table new-hash)))

(defun get-type-by-elem (name)
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
                  (t (setf type (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))))))
   type))

;;
;; Get next element based on previous element without
;; any conditions
;; @typ: element type
;; @spatial: spatial relation of the elements
;; @name: element name
;;
(defun get-next-elem-by-prev-elem (type spatial name)
  (let*((liste (get-front-elems-of-agent-by-type type))
        (resultlist '()))
  ;;  (format t "~a~%" liste)
    (dotimes (index (length liste))
      (if (not (null (check-elems-by-relation->get-elems-in-tf
                      name (nth index liste) spatial)))
          (setf resultlist (append resultlist (list 
                                               (format NIL "~a:~a"(nth index liste)                                                       (get-distance
                                                        (get-pose-by-elem
                                                         (nth index liste))
                                                        (get-pose-by-elem name))))))))
    (sort-list resultlist)))

(defun set-keyword (string)
  (intern (string-upcase string) "KEYWORD"))


(defun check-distance (point01 point02)
  (let*((pose02 (cl-transforms:make-pose point02 (cl-transforms:make-identity-rotation)))
        (pose01 (cl-transforms:make-pose point01 (cl-transforms:make-identity-rotation)))
        (fsec (sqrt (+ (square (- (cl-transforms:x (cl-transforms:origin pose02))
                                   (cl-transforms:x (cl-transforms:origin pose01))))
                        (square (- (cl-transforms:y (cl-transforms:origin pose02))
                                   (cl-transforms:y (cl-transforms:origin pose01))))
                        (square (- (cl-transforms:z (cl-transforms:origin pose02))
                                   (cl-transforms:z (cl-transforms:origin pose01)))))))
        (forw2 (sqrt (+ (square (- (cl-transforms:x (cl-transforms:origin pose02))
                                   (+ (cl-transforms:x (cl-transforms:origin pose01)) 1)))
                        (square (- (cl-transforms:y (cl-transforms:origin pose02))
                                   (cl-transforms:y (cl-transforms:origin pose01))))
                        (square (- (cl-transforms:z (cl-transforms:origin pose02))
                                   (cl-transforms:z (cl-transforms:origin pose01)))))))
        (backw2 (sqrt (+ (square (- (cl-transforms:x (cl-transforms:origin pose02))
                                   (- (cl-transforms:x (cl-transforms:origin pose01)) 1)))
                        (square (- (cl-transforms:y (cl-transforms:origin pose02))
                                   (cl-transforms:y (cl-transforms:origin pose01))))
                        (square (- (cl-transforms:z (cl-transforms:origin pose02))
                                   (cl-transforms:z (cl-transforms:origin pose01)))))))
        (right2 (sqrt (+ (square (- (cl-transforms:x (cl-transforms:origin pose02))
                                    (cl-transforms:x (cl-transforms:origin pose01))))
                         (square (- (cl-transforms:y (cl-transforms:origin pose02))
                                    (-  (cl-transforms:y (cl-transforms:origin pose01)) 1)))
                        (square (- (cl-transforms:z (cl-transforms:origin pose02))
                                   (cl-transforms:z (cl-transforms:origin pose01)))))))
        (left2 (sqrt (+ (square (- (cl-transforms:x (cl-transforms:origin pose02))
                                    (cl-transforms:x (cl-transforms:origin pose01))))
                         (square (- (cl-transforms:y (cl-transforms:origin pose02))
                                    (+  (cl-transforms:y (cl-transforms:origin pose01)) 1)))
                        (square (- (cl-transforms:z (cl-transforms:origin pose02))
                                   (cl-transforms:z (cl-transforms:origin pose01)))))))
        (up2 (sqrt (+ (square (- (cl-transforms:x (cl-transforms:origin pose02))
                                    (cl-transforms:x (cl-transforms:origin pose01))))
                         (square (- (cl-transforms:y (cl-transforms:origin pose02))
                                    (cl-transforms:y (cl-transforms:origin pose01))))
                        (square (- (cl-transforms:z (cl-transforms:origin pose02))
                                   (+ (cl-transforms:z (cl-transforms:origin pose01)) 2))))))
        (down2 (sqrt (+ (square (- (cl-transforms:x (cl-transforms:origin pose02))
                                 (cl-transforms:x (cl-transforms:origin pose01))))
                      (square (- (cl-transforms:y (cl-transforms:origin pose02))
                                 (cl-transforms:y (cl-transforms:origin pose01))))
                      (square (- (cl-transforms:z (cl-transforms:origin pose02))
                                 (+ (cl-transforms:z (cl-transforms:origin pose01)) 2))))))
        (all (append (append (append (append (append (append (append '() (list fsec)) (list forw2)) (list backw2)) (list right2)) (list left2)) (list up2)) (list down2)))
        (value NIL))
    (dotimes(index (length all))
      (cond ((and (>= 2.0 (nth index all))
                  (null value))
             (setf value T))))
  value))


;;;
;;;
;;;Sorting the lists by using two functions
;;;
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


;;;
;;; Calculates the distance of two poses
;;;
(defun get-distance (pose1 pose2)
(let*((vector (cl-transforms:origin pose1))
        (x-vec (cl-transforms:x vector))
        (y-vec (cl-transforms:y vector))
        (z-vec (cl-transforms:z vector))
        (ge-vector (cl-transforms:origin pose2))
        (x-ge (cl-transforms:x ge-vector))
        (y-ge (cl-transforms:y ge-vector))
        (z-ge (cl-transforms:z ge-vector)))
    (round (sqrt (+ (square (- x-vec x-ge))
             (square (- y-vec y-ge))
             (square (- z-vec z-ge)))))))

(defun get-front-elems-of-agent-with-dist (viewpoint)
  ;;(format t "get-elems-agent-front-by-dist~%")
  (setf *sem-map* (sem-map-utils:get-semantic-map))
  (let*((sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
         (sem-keys (hash-table-keys sem-hash))
         (poses '()))
    (dotimes (index (length sem-keys))
      (let*((pose (get-pose-by-elem (nth index sem-keys)))
            (pub (cl-tf:set-transform *tf* (cl-transforms-stamped:make-transform-stamped "map" (nth index sem-keys) (roslisp:ros-time) (cl-transforms:origin pose) (cl-transforms:orientation pose))))
            (obj-pose2 (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* "map" (nth index sem-keys))))
            (obj-pose (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* viewpoint (nth index sem-keys))))
            (agentpose (cl-transforms:transform->pose  (cl-tf:lookup-transform *tf*  "map" viewpoint)))
            (dist (get-distance agentpose obj-pose2)))
      (if (and (>= 1000 dist)
               (plusp (cl-transforms:x (cl-transforms:origin obj-pose))))
               (setf poses (append (list (format NIL"~a:~a" (nth index sem-keys) dist)) poses)))))
       (sort-list poses)))

(defun get-front-elems-of-agent (&optional (viewpoint "busy-genius"))
  ;;(format t "get-elems-agent-front-by-dist~%")
  (setf *sem-map* (sem-map-utils:get-semantic-map))
  (let*((sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
        (sem-keys (hash-table-keys sem-hash))
        (poses '())
        (result '()))
    (dotimes (index (length sem-keys))
      (let*((pose (get-pose-by-elem (nth index sem-keys)))
            (pub (cl-tf:set-transform *tf* (cl-transforms-stamped:make-transform-stamped "map" (nth index sem-keys) (roslisp:ros-time) (cl-transforms:origin pose) (cl-transforms:orientation pose))))
            (obj-pose2 (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* "map" (nth index sem-keys))))
            (obj-pose (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* viewpoint (nth index sem-keys))))
            (agentpose (cl-transforms:transform->pose  (cl-tf:lookup-transform *tf*  "map" viewpoint)))
            (dist (get-distance agentpose obj-pose2)))
      (if (and (>= 500 dist)
               (plusp (cl-transforms:x (cl-transforms:origin obj-pose))))
               (setf poses (append (list (format NIL"~a:~a" (nth index sem-keys) dist)) poses)))))
       (setf poses (sort-list poses))
    (dotimes (index (length poses))
      (setf result (append (list (first (split-sequence:split-sequence #\: (nth index poses)))) result)))
    (reverse result)))

(defun get-dist-of-elem-by-agent (name &optional (viewpoint "busy-genius"))
  (format NIL "~a:~a" name (get-distance (get-pose-by-elem name) (cl-transforms:transform->pose (cl-tf:lookup-transform *tf* "map" viewpoint)))))

(defun get-elems-of-type (type)
  (let*((sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
        (new-hash (copy-hash-table sem-hash))
        (sem-keys (hash-table-keys new-hash))
        (liste '()))
    (dotimes(index (length sem-keys))
      (if (string-equal type (get-type-by-elem (nth index sem-keys)))
          (setf liste (append (list (nth index sem-keys)) liste))))
    (format t "liste ~a~%"liste)
    (reverse liste)))
      
