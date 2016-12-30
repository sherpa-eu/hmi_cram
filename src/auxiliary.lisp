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
  (let*((liste (get-front-elems-of-agent-with-dist))
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
                                               (format NIL "~a:~a"(nth index liste)                                                       (get-distance
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
                  (t (setf type (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))))))
   type))

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



(defun set-keyword (string)
  (intern (string-upcase string) "KEYWORD"))

(defun give-pointed-direction (object pose)
  (let ((result NIL)
        (liste (calculate-points pose))) 
    (cond ((string-equal "null" object)
        ())
        (t (

  )))
    result))

(defun calculate-poses (pose)
  (let((poslist'()))
       (publish-pose pose :id 1100)
    (loop for index from 3 to 100
          do (cl-tf:set-transform *tf* (cl-transforms-stamped:make-transform-stamped
                                        "map" "gesture"
                                        (roslisp:ros-time)
                                        (cl-transforms:origin pose)
                                        (cl-transforms:orientation pose)))
             (setf posexyz (cl-transforms-stamped:make-pose-stamped
                            "gesture" 0.0
                            (cl-transforms:make-3d-vector index 1 1)
                            (cl-transforms:make-identity-rotation)))
             (setf posex-yz (cl-transforms-stamped:make-pose-stamped
                             "gesture" 0.0
                             (cl-transforms:make-3d-vector index -1 1)
                             (cl-transforms:make-identity-rotation)))
             (setf posex-y-z (cl-transforms-stamped:make-pose-stamped
                              "gesture" 0.0
                              (cl-transforms:make-3d-vector index -1 -1)
                              (cl-transforms:make-identity-rotation)))
             (setf posexy-z (cl-transforms-stamped:make-pose-stamped
                             "gesture" 0.0
                             (cl-transforms:make-3d-vector index 1 -1)
                             (cl-transforms:make-identity-rotation)))
             (setf posex (cl-transforms-stamped:make-pose-stamped
                          "gesture" 0.0
                          (cl-transforms:make-3d-vector index 0 0)
                          (cl-transforms:make-identity-rotation)))
             (setf posexz (cl-transforms-stamped:make-pose-stamped
                         "gesture" 0.0
                         (cl-transforms:make-3d-vector index 0 1)
                         (cl-transforms:make-identity-rotation)))
             (setf posex-z (cl-transforms-stamped:make-pose-stamped
                            "gesture" 0.0
                            (cl-transforms:make-3d-vector index 0 -1)
                            (cl-transforms:make-identity-rotation)))
             (setf posexy (cl-transforms-stamped:make-pose-stamped
                           "gesture" 0.0
                           (cl-transforms:make-3d-vector index 1 0)
                           (cl-transforms:make-identity-rotation)))
             (setf posex-y (cl-transforms-stamped:make-pose-stamped
                            "gesture" 0.0
                            (cl-transforms:make-3d-vector index -1 0)
                            (cl-transforms:make-identity-rotation)))  
             (setf xyz  (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose *tf* :pose posexyz :target-frame "map")))
             (setf x-yz (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose *tf* :pose posex-yz :target-frame "map")))
             (setf x-y-z  (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose *tf* :pose posex-y-z :target-frame "map")))
             (setf xy-z  (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose *tf* :pose posexy-z :target-frame "map")))
             (setf x (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose *tf* :pose posex :target-frame "map")))
             (setf xz (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose *tf* :pose posexz :target-frame "map")))
             (setf x-z (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose *tf* :pose posex-z :target-frame "map")))
             (setf xy (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose *tf* :pose posexy :target-frame "map")))
             (setf x-y (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose *tf* :pose posex-y :target-frame "map")))     
             (setf poslist (append (list  (cl-transforms-stamped:pose-stamped->pose xyz)) poslist))
             (setf poslist (append (list  (cl-transforms-stamped:pose-stamped->pose x-yz)) poslist))
             (setf poslist (append (list  (cl-transforms-stamped:pose-stamped->pose x-y-z)) poslist))
             (setf poslist (append (list  (cl-transforms-stamped:pose-stamped->pose xy-z)) poslist))
             (setf poslist (append (list  (cl-transforms-stamped:pose-stamped->pose x)) poslist))
             (setf poslist (append (list  (cl-transforms-stamped:pose-stamped->pose xz)) poslist))
             (setf poslist (append (list  (cl-transforms-stamped:pose-stamped->pose x-z)) poslist))
             (setf poslist (append (list  (cl-transforms-stamped:pose-stamped->pose xy)) poslist))
             (setf poslist (append (list  (cl-transforms-stamped:pose-stamped->pose x-y)) poslist)))
  (setf poslist (reverse poslist))
  (dotimes(test (length poslist))
    do  (format t "test4 ~a~%" (nth test poslist))
    (publish-pose (nth test poslist) :id (+  test 100)))
    poslist))

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
      (if (and (>= 3 (nth index all))
               (null value))
          (setf value T)))
  value))

(defun give-pointed-at-not-bboxes (point)
  (let*((sem-map  (sem-map:get-semantic-map))
        (elem NIL)
        (num (make-list 100))
        (sem-hash (slot-value sem-map 'sem-map-utils:parts))
        (sem-keys (hash-table-keys sem-hash))
    (dotimes (jindex (length liste-tr))
      do (dotimes(jo (length sem-keys))
          do(let* ((pose (cl-transforms:origin (slot-value (gethash (nth jo sem-keys) sem-hash) 'sem-map-utils:pose)))
                   (npoint (cl-transforms:origin (nth jindex liste-tr)))
                   (upoint (cl-transforms:origin (nth jindex liste-up)))
                   (dpoint (cl-transforms:origin (nth jindex liste-down)))
                   (rpoint (cl-transforms:origin (nth jindex liste-right)))
                   (lpoint (cl-transforms:origin (nth jindex liste-left)))
                   (fpoint (cl-transforms:origin (nth jindex liste-front)))
                   (bpoint (cl-transforms:origin (nth jindex liste-back)))
                   (value (checker-at-distance pose npoint))
                   (uvalue (checker-at-distance pose upoint))
                   (dvalue (checker-at-distance pose dpoint))
                   (rvalue (checker-at-distance pose rpoint))
                   (lvalue (checker-at-distance pose lpoint))
                   (fvalue (checker-at-distance pose fpoint))
                   (bvalue (checker-at-distance pose  bpoint)))
              ;;(format t "pose is ~a~%" pose)
                  (cond ((and (or (equal value T)
                                  (equal uvalue T)
                                  (equal dvalue T)
                                  (equal rvalue T)
                                  (equal lvalue T)
                                  (equal fvalue T)
                                  (equal bvalue T))
                             (not (equal (nth jo sem-keys)
                                         (find (nth jo sem-keys)
                                           elem :test #'equal))))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-tr)) :id (+ (+ jo jindex) 1000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-right)) :id (+ (+ jo jindex) 2000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-left)) :id (+ (+ jo jindex) 3000))
                         (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-front)) :id (+ (+ jo jindex) 4000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-back )) :id (+ (+ jo jindex) 5000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-up)) :id (+ (+ jo jindex) 6000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-down)) :id (+ (+ jo jindex) 7000))
                        (setf elem (append (list (nth jo sem-keys)) elem)))
                        
                       (t
                         (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-tr)) :id (+ (+ jo jindex) 11000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-right)) :id (+ (+ jo jindex) 22000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-left)) :id (+ (+ jo jindex) 33000))
                         (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-front)) :id (+ (+ jo jindex) 44000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-back )) :id (+ (+ jo jindex) 55000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-up)) :id (+ (+ jo jindex) 66000))
                        (location-costmap:publish-point  (cl-transforms:origin (nth jindex liste-down)) :id (+ (+ jo jindex) 77000))                      
                          )))))
             (reverse elem))) 


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

(defun get-front-elems-of-agent (viewpoint)
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

(defun get-pointed-elem-by-pose (pose type &optional (viewpoint "human"))
  (let ((pointed-liste (calculate-poses pose))
        (agent-elem (get-front-elems-of-agent viewpoint))
        (elem '()))
    (dotimes (index (length pointed-liste))
      (dotimes(in (length agent-elem))
        (cond ((and (equal T (check-distance (nth in agent-elem) (nth index pointed-liste)))
                       (string-equal (get-elem-by-type (nth in agent-elem))
                                     type))
                  (setf elem (append (list (nth in agent-elem)) elem))
                  (return)))))
    (car elem)))
                  
    
    
        
