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
(defvar *sem-map* NIL)
(defvar *tf* NIL)
(defvar *pub* NIL)

(defun tf-busy-genius-to-map ()
  (let ((var (cl-transforms:transform->pose (cl-tf:lookup-transform *tf* "map" "busy_genius"))))
    var))

(defun tf-agent-to-map (name)
  (let* ((var (concatenate 'string name "/base_link"))
        (pose (cl-transforms:transform->pose (cl-tf:lookup-transform *tf* "map" var))))
    pose))

(defun get-elem-pose (name)
  (cram-sherpa-spatial-relations::json-call-pose name))


;;
;; Get the position of the element out of semantic map
;; @objname: object name of the element
;;
(defun get-pose-by-elem (objname)
  (if (null *sem-map*)
      (setf *sem-map* (sem-map-utils:get-semantic-map)))
 (let*((pose NIL)
       (sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
       (new-hash (copy-hash-table sem-hash))
       (sem-keys (hash-table-keys sem-hash)))
       (dotimes (i (length sem-keys))
         do(if (string-equal objname (nth i sem-keys))
               (setf pose (slot-value (gethash objname new-hash) 'sem-map-utils:pose))
               (format t "")))
   pose))

(defun get-bbox-by-elem (objname)
 (let*((bbox NIL)
       (sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
       (new-hash (copy-hash-table sem-hash))
       (sem-keys (hash-table-keys sem-hash)))
       (dotimes (i (length sem-keys))
         do(if (string-equal objname (nth i sem-keys))
               (setf bbox (slot-value (gethash objname new-hash) 'sem-map-utils:dimensions))))
      bbox))
;;
;; Get elements in front of the agent by specific type
;; @type: type of the object
;; 
;; (defun get-front-elems-of-agent-by-type (type &optional (viewpoint "busy_genius"))  
;;   (format t "get-front-elems-by-type~%")
;;   (let*((liste (get-front-elems-of-agent viewpoint))
;;         (resultlist '()))
;;     (dotimes (index (length liste))
;;       (if(string-equal type
;;                        (get-type-by-elem (nth index liste)))
;;          (setf resultlist (append resultlist (list (nth index liste))))))
;;     (dotimes (jndex (length resultlist))
;;       (publish-pose (get-pose-by-elem (nth jndex resultlist)) :id (+ jndex 9239874398274) :zet 0.0))
;;     (format t "~a~%" resultlist)
;;     resultlist))
 ;; (defun get-all-elems-front-agent-by-type (type viewpoint)
 ;;   (if (null *sem-map*)
 ;;       (setf *sem-map* (sem-map-utils:get-semantic-map)))
 ;;   (let*((sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
 ;;         (sem-keys (hash-table-keys sem-hash))
 ;;         (new-hash (make-hash-table))
 ;;         (poses '())
 ;;         (human-pose (tf-busy-genius-to-map)))
 ;;     (dotimes (index (length sem-keys))
 ;;       (cond((and (string-equal type (get-type-by-elem (nth index sem-keys)))
 ;;                  (check-elems-infront-agent (nth index sem-keys) viewpoint))
 ;;           (setf poses (append (list (format NIL "~a:~a" (nth index sem-keys)
 ;;                                             (get-distance human-pose
 ;;                                                           (get-pose-by-elem
 ;;                                                            (nth index sem-keys)))))
 ;;                                                                             poses)))))
 ;;     (setf poses (sort-list poses))
 ;;  ;;  (format t "~a~%" poses)
 ;;    (list (first  (split-sequence:split-sequence #\: (car poses))))))

(defun get-all-elems-by-type (type)
 (let((actlist '())
       (poslist '()))
       (if (json-prolog:check-connection)
           (let*((human-pose (tf-busy-genius-to-map))
                 (liste NIL)(liste1 NIL)(liste2 NIL)
                 (unrealtype NIL))
             (cond((string-equal type "tree")
                   (setf liste1
                         (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs "http://knowrob.org/kb/knowrob.owl#SnowTree"))))
                   (setf liste2
                         (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs "http://knowrob.org/kb/knowrob.owl#PineTree"))))
                   (setf liste (append liste1 liste2)))
                  ((string-equal type "house")
                   (setf liste
                         (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs "http://knowrob.org/kb/knowrob.owl#Cottage")))))
                ((string-equal type "lake")
                 (setf liste
                         (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs "http://knowrob.org/kb/knowrob.owl#FrozenLake")))))
                ((string-equal type "helipad")
                 (setf liste
                         (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs "http://knowrob.org/kb/knowrob.owl#Helipad")))))
                (t
                 (setf unrealtype (concatenate 'string "http://knowrob.org/kb/knowrob.owl#"
                                               (string-capitalize type)))
                 (setf liste (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs ,unrealtype))))))
             (dotimes (index (length liste))
               (let ((obj (second
                           (split-sequence:split-sequence #\# 
                                                          (remove #\' (symbol-name (cdar  (nth index liste))))))))
                 (setf actlist (append actlist (list
                                                (format NIL "~a:~a"
                                                        obj
                                                        (get-distance human-pose
                                                                       (get-elem-pose
                                                                        obj))))))))))
    (setf actlist (remove-duplicates (sort-list actlist) :test #'equal))
    (dotimes (jndex (length actlist))
      (setf poslist (append poslist (list (first (split-sequence:split-sequence #\: (nth jndex actlist)))))))
    poslist))
  
(defun get-all-elems-front-agent-by-type (type viewpoint)
  (let((actlist '())
       (poslist '()))
       (if (json-prolog:check-connection)
           (let*((human-pose (tf-busy-genius-to-map))
                 (liste NIL)(liste1 NIL)(liste2 NIL)
                 (unrealtype NIL))
             (cond((string-equal type "tree")
                   (setf liste1
                         (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs "http://knowrob.org/kb/knowrob.owl#SnowTree"))))
                   (setf liste2
                         (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs "http://knowrob.org/kb/knowrob.owl#PineTree"))))
                   (setf liste (append liste1 liste2)))
                  ((string-equal type "house")
                   (setf liste
                         (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs "http://knowrob.org/kb/knowrob.owl#Cottage")))))
                ((string-equal type "lake")
                 (setf liste
                         (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs "http://knowrob.org/kb/knowrob.owl#FrozenLake")))))
                (t
                 (setf unrealtype (concatenate 'string "http://knowrob.org/kb/knowrob.owl#"
                                               (string-capitalize type)))
                 (setf liste (force-ll
                         (json-prolog:prolog `("map_object_type" ?objs ,unrealtype))))))
             (dotimes (index (length liste))
               (let ((obj (second
                           (split-sequence:split-sequence #\# 
                                                          (remove #\' (symbol-name (cdar  (nth index liste))))))))
               (if(and (check-elems-infront-agent obj viewpoint)
                       (not (find obj actlist :test #'equal))) 
                  (setf actlist (append actlist (list
                                                 (format NIL "~a:~a"
                                                         obj
                                                         (get-distance human-pose
                                                                       (get-elem-pose
                                                                        obj)))))))))))
    (setf actlist (remove-duplicates (sort-list actlist) :test #'equal))
    (dotimes (jndex (length actlist))
      (setf poslist (append poslist (list (first (split-sequence:split-sequence #\: (nth jndex actlist)))))))
    poslist))


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
              (string-equal property "next-to")
              (string-equal property "next"))
         
         (if (>= 20 (get-distance obj1-pose obj2-pose))
             (setf tmp T)
             (setf tmp NIL))))        
    tmp))

(defun get-elems-in-tf (&optional (viewpoint "busy_genius"))
  (if (null *sem-map*)
      (setf *sem-map* (sem-map-utils:get-semantic-map)))
  (let* ((sem-map *sem-map*)
        (sem-hash (slot-value sem-map 'sem-map-utils:parts))
         (sem-keys (hash-table-keys sem-hash))
       ;;  (semm-hash (copy-hash-table sem-hash))
         (new-hash (make-hash-table))(name NIL)
         (obj-pose NIL))
     (if (not (string-equal viewpoint "busy_genius"))
      (setf viewpoint (format NIL "~a/base_link" viewpoint)))
    (dotimes (index (length sem-keys))
      (let*((pose (get-pose-by-elem (nth index sem-keys)))
            (pub (cl-tf:set-transform *tf* (cl-transforms-stamped:make-transform-stamped "map" (nth index sem-keys) (roslisp:ros-time) (cl-transforms:origin pose) (cl-transforms:orientation pose))))
            (obj-pose (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* viewpoint (nth index sem-keys)))))
      (setf (gethash (nth index sem-keys) new-hash) obj-pose)))
(copy-hash-table new-hash)))

(defun get-elems-in-tf-agent (&optional (viewpoint "busy_genius"))
  (let* ((sem-map (sem-map-utils:get-semantic-map))
         (sem-hash (slot-value sem-map 'sem-map-utils:parts))
         (sem-keys (hash-table-keys sem-hash))
       ;;  (semm-hash (copy-hash-table sem-hash))
         (new-hash (make-hash-table))(name NIL)
         (obj-pose NIL))
     (if (not (string-equal viewpoint "busy_genius"))
      (setf viewpoint (format NIL "~a/base_link" viewpoint)))
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
            (cond ((or (search "tree" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                       (search "Tree" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type)))
                   (setf type "Tree"))
                  ((or (search "rock" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                       (search "Rock" (slot-value (gethash name new-hash)
                                                  'cram-semantic-map-utils::type)))
                   (setf type "Rock"))
                  ((or (search "lake" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                       (search "Lake" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type)))
                   (setf type "Lake"))
                  ((or (search "tunnel" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                       (search "Tunnel" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type)))
                   (setf type "Tunnel"))
                  ((or (search "Cottage" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                       (search "Cottage" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type)))
                   (setf type "Cottage"))
                  ((or (search "bridge" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))
                       (search "Bridge" (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type)))
                   (setf type "Bridge"))
                  ((search "pylon" (slot-value (gethash name new-hash)
                                               'cram-semantic-map-utils::type))
                   (setf type "Pylon"))
                  (t (setf type (slot-value (gethash name new-hash)
                                                        'cram-semantic-map-utils::type))))))
   type))

;; Get next element based on previous element without
;; any conditions
;; @typ: element type
;; @spatial: spatial relation of the elements
;; @name: element name
;;
(defun get-prev-elem-based-on-next-elem (type spatial name viewpoint)
  (let*((liste (get-all-elems-front-agent-by-type type viewpoint))
        (resultlist '()))
    (dotimes (index (length liste))
      (if  (not (null (check-elems-by-relation->get-elems-in-tf
                           (nth index liste) name  spatial)))
            (setf resultlist (append resultlist (list 
                                               (format NIL "~a:~a"(nth index liste)                                                       (get-distance
                                                        (get-elem-pose
                                                         (nth index liste))
                                                        (get-elem-pose name))))))))
    (if (null resultlist)
        (setf resultlist (get-elems-around liste "around" name)))
    (sort-list resultlist)))

;; Get next element based on previous element without
;; any conditions
;; @typ: element type
;; @spatial: spatial relation of the elements
;; @name: element name
;;
(defun get-next-elem-based-on-prev-elem (type spatial name viewpoint)
  (let*((liste (get-all-elems-front-agent-by-type type viewpoint))
        (resultlist '()))
    (dotimes (index (length liste))
      (if  (not (null (check-elems-by-relation->get-elems-in-tf
                           name (nth index liste)  spatial)))
            (setf resultlist (append resultlist (list 
                                               (format NIL "~a:~a"(nth index liste)                                                       (get-distance
                                                        (get-elem-pose 
                                                         (nth index liste))
                                                        (get-elem-pose name))))))))
    (if (null resultlist)
        (setf resultlist (get-elems-around liste "around" name)))
    (sort-list resultlist)))

(defun get-elems-around (liste spatial name)
  (let((resultlist '()))
    (dotimes (index (length liste))
      (if  (not (null (check-elems-by-relation->get-elems-in-tf
                           name (nth index liste) spatial)))
            (setf resultlist (append resultlist (list 
                                               (format NIL "~a:~a"(nth index liste)                                                       (get-distance
                                                        (get-elem-pose
                                                         (nth index liste))
                                                        (get-elem-pose name))))))))
    resultlist))

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
      (cond ((and (>= 50.0 (nth index all))
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
 (if (not (string-equal viewpoint "busy_genius"))
      (setf viewpoint (format NIL "~a/base_link" viewpoint)))
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

(defun check-elems-infront-agent (elem viewpoint)
  (if (not (string-equal viewpoint "busy_genius"))
      (setf viewpoint (format NIL "~a/base_link" viewpoint)))
  (let*((poses NIL))
       (let*((pose (get-elem-pose elem));;pose-by-elem elem))
             (pub (cl-tf:set-transform *tf* (cl-transforms-stamped:make-transform-stamped "map" elem (roslisp:ros-time) (cl-transforms:origin pose) (cl-transforms:orientation pose))))
             (obj-pose2 (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* "map" elem)))
             (obj-pose (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* viewpoint elem)))
             (agentpose (cl-transforms:transform->pose  (cl-tf:lookup-transform *tf*  "map" viewpoint)))
             (dist (get-distance agentpose obj-pose2)))
      (if (and (>= 500 dist)
               (plusp (cl-transforms:x (cl-transforms:origin obj-pose))))
               (setf poses T)))
    poses))

(defun get-front-elems-of-agent (&optional (viewpoint "busy_genius"))
   (if (not (string-equal viewpoint "busy_genius"))
      (setf viewpoint (format NIL "~a/base_link" viewpoint)))
  (let*((sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
        (sem-keys (hash-table-keys sem-hash))
        (poses '())
        (result '()))
    (dotimes (index (length sem-keys))
     (cond ((not (search "MountainRoad" (nth index sem-keys)))
      (let*((pose (get-pose-by-elem (nth index sem-keys)))
            (pub (cl-tf:set-transform *tf* (cl-transforms-stamped:make-transform-stamped "map" (nth index sem-keys) (roslisp:ros-time) (cl-transforms:origin pose) (cl-transforms:orientation pose))))
            (obj-pose2 (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* "map" (nth index sem-keys))))
            (obj-pose (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* viewpoint (nth index sem-keys))))
            (agentpose (cl-transforms:transform->pose  (cl-tf:lookup-transform *tf*  "map" viewpoint)))
            (dist (get-distance agentpose obj-pose2)))
      (if (and (>= 500 dist)
               (plusp (cl-transforms:x (cl-transforms:origin obj-pose))))
               (setf poses (append (list (format NIL"~a:~a" (nth index sem-keys) dist)) poses)))))))
       (setf poses (sort-list poses))
    (dotimes (index (length poses))
      (setf result (append (list (first (split-sequence:split-sequence #\: (nth index poses)))) result)))
    (reverse result)))

(defun get-dist-of-elem-by-agent (name &optional (viewpoint "busy_genius"))
   (if (not (string-equal viewpoint "busy_genius"))
      (setf viewpoint (format NIL "~a/base_link" viewpoint)))
  (format NIL "~a:~a" name (get-distance (get-pose-by-elem name) (cl-transforms:transform->pose (cl-tf:lookup-transform *tf* "map" viewpoint)))))

(defun get-elems-of-type (type)
  (let*((sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
        (new-hash (copy-hash-table sem-hash))
        (sem-keys (hash-table-keys new-hash))
        (liste '()))
    (dotimes(index (length sem-keys))
      (if (string-equal type (get-type-by-elem (nth index sem-keys)))
          (setf liste (append (list (nth index sem-keys)) liste))))
    (reverse liste)))

(defun publish-body (pose)
  (setf *marker-publisher*
        (roslisp:advertise "~location_marker" "visualization_msgs/Marker"))
  (let* ((point (cl-transforms:origin pose))
         (rot (cl-transforms:orientation pose)))
    (when *marker-publisher*
      (publish-pose-color pose (cl-transforms:make-3d-vector 1 1 0))
      (roslisp:publish *marker-publisher*
               (roslisp:make-message "visualization_msgs/Marker"
                             (stamp header) (roslisp:ros-time)
                             (frame_id header)
                             (typecase pose
                               (cl-transforms-stamped:pose-stamped (cl-transforms-stamped:frame-id  pose))
                               (t cram-tf:*fixed-frame*))

                             ns "kipla_locations"
                             id 200000
                             type (roslisp:symbol-code 
                                   'visualization_msgs-msg:<marker> :cylinder)
                             action (roslisp:symbol-code
                                     'visualization_msgs-msg:<marker> :add)
                             (x position pose) (cl-transforms:x point)
                             (y position pose) (cl-transforms:y point)
                             (z position pose) 1.0
                             (x orientation pose) (cl-transforms:x rot)
                             (y orientation pose) (cl-transforms:y rot)
                             (z orientation pose) (cl-transforms:z rot)
                             (w orientation pose) (cl-transforms:w rot)
                             (x scale) 0.3
                             (y scale) 0.3
                             (z scale) 2
                             (r color) 1.0
                             (g color) 0.0
                             (b color) 0.0
                             (a color) 1.0))
      (roslisp:publish *marker-publisher*
               (roslisp:make-message "visualization_msgs/Marker"
                             (stamp header) (roslisp:ros-time)
                             (frame_id header) cram-tf:*fixed-frame*
                             ns "kipla_locations"
                             id 100000
                             type (roslisp:symbol-code 
                                   'visualization_msgs-msg:<marker> :sphere)
                             action (roslisp:symbol-code
                                     'visualization_msgs-msg:<marker> :add)
                             (x position pose) (cl-transforms:x point)
                             (y position pose) (cl-transforms:y point)
                             (z position pose) (+ 2 (cl-transforms:z point))
                             (w orientation pose) 1.0
                             (x scale) 0.5
                             (y scale) 0.5
                             (z scale) 0.5
                             (r color) 1.0 ; (random 1.0)
                             (g color) 0.0 ; (random 1.0)
                             (b color) 0.0 ; (random 1.0)
                             (a color) 1.0)))))

(defun publish-pose-color (pose vec)
  (setf *marker-publisher*
        (roslisp:advertise "~location_marker" "visualization_msgs/Marker"))
    (let ((point (cl-transforms:origin pose))
          (rot (cl-transforms:orientation pose)))
    (when *marker-publisher*
      (roslisp:publish *marker-publisher*
                       (roslisp:make-message "visualization_msgs/Marker"
                                             (stamp header) (roslisp:ros-time)
                                             (frame_id header)
                             (typecase pose
                               (cl-transforms-stamped:pose-stamped (cl-transforms-stamped:frame-id  pose))
                               (t cram-tf:*fixed-frame*))
                             ns "kipla_locations"
                             id 500000
                             type (roslisp:symbol-code 
                                   'visualization_msgs-msg:<marker> :arrow)
                             action (roslisp:symbol-code 
                                     'visualization_msgs-msg:<marker> :add)
                             (x position pose) (cl-transforms:x point)
                             (y position pose) (cl-transforms:y point)
                             (z position pose) (+ (cl-transforms:z point) 2)
                             (x orientation pose) (cl-transforms:x rot)
                             (y orientation pose) (cl-transforms:y rot)
                             (z orientation pose) (cl-transforms:z rot)
                             (w orientation pose) (cl-transforms:w rot)
                             (x scale) 0.40
                             (y scale) 0.15
                             (z scale) 0.15
                             (r color) 1;;(cl-transforms:x vec) ; (random 1.0)
                             (g color) 0;;(cl-transforms:y vec) ; (random 1.0)
                             (b color) 0;;(cl-transforms:z vec) ; (random 1.0)
                             (a color) 1.0)))))


(defun publish-box (pose vector &key id)
  (setf *marker-publisher*
        (roslisp:advertise "~location_marker" "visualization_msgs/Marker"))
   (let ((point (cl-transforms:origin pose))
        (rot (cl-transforms:orientation pose))
        (current-index 0))
    (when *marker-publisher*
      (roslisp:publish *marker-publisher*
               (roslisp:make-message "visualization_msgs/Marker"
                             (stamp header) (roslisp:ros-time)
                             (frame_id header)
                             (typecase pose
                               (cl-transforms-stamped:pose-stamped (cl-transforms-stamped:frame-id pose))
                               (t cram-tf:*fixed-frame*))
                             ns "kipla_locations"
                             id (or id (incf current-index))
                             type (roslisp:symbol-code
                                   'visualization_msgs-msg:<marker> :cube)
                             action (roslisp:symbol-code
                                     'visualization_msgs-msg:<marker> :add)
                             (x position pose) (cl-transforms:x point)
                             (y position pose) (cl-transforms:y point)
                             (z position pose) (cl-transforms:z point)
                             (x orientation pose) (cl-transforms:x rot)
                             (y orientation pose) (cl-transforms:y rot)
                             (z orientation pose) (cl-transforms:z rot)
                             (w orientation pose) (cl-transforms:w rot)
                             (x scale) (cl-transforms:x vector)
                             (y scale) (cl-transforms:y vector)
                             (z scale) (cl-transforms:z vector)
                             (r color) 1.0
                             (g color) 1.0
                             (b color) 0.0
                             (a color) 1.0)))))

(defun calculate-relation-by-agent-pose (viewpoint relation)
   (if (not (string-equal viewpoint "busy_genius"))
      (setf viewpoint (format NIL "~a/base_link" viewpoint)))
  (let* ((pose (cl-transforms:transform->pose  (cl-tf:lookup-transform *tf* "map" viewpoint)))
         (result NIL))
    (cl-tf:set-transform *tf*
                         (cl-transforms-stamped:make-transform-stamped
                          "map" "relation"
                          (roslisp:ros-time)
                          (cl-transforms:origin pose)
                          (cl-transforms:orientation pose)))
    (cond((string-equal "right" relation)
          (setf pose (cl-transforms-stamped:make-pose-stamped "relation" 0.0
                                                          (cl-transforms:make-3d-vector 0 -20 0)
                                                          (cl-transforms:make-identity-rotation)))
          (setf result (cl-tf:transform-pose *tf* :pose pose :target-frame "map")))
         ((string-equal "left" relation)
          (setf pose (cl-transforms-stamped:make-pose-stamped "relation" 0.0
                                                          (cl-transforms:make-3d-vector 0 20 0)
                                                          (cl-transforms:make-identity-rotation)))
          (setf result (cl-tf:transform-pose *tf* :pose pose :target-frame "map")))
         ((string-equal "straight" relation)
          (setf pose (cl-transforms-stamped:make-pose-stamped "relation" 0.0
                                                          (cl-transforms:make-3d-vector 20 0 0)
                                                          (cl-transforms:make-identity-rotation)))
          (setf result (cl-tf:transform-pose *tf* :pose pose :target-frame "map"))))
    (publish-pose (cl-transforms-stamped:pose-stamped->pose result) :id 100)
    result))

(defun get-specific-elem (name viewpoint)
  (format t "name ~a~%" name)
   (if (not (string-equal viewpoint "busy_genius"))
      (setf viewpoint (format NIL "~a/base_link" viewpoint)))
  (let((elem-list (get-all-elems-front-agent-by-type name viewpoint))
       (poses '())
       (pose NIL)(obj-pose NIL))
    (dotimes (index (length elem-list))
      (setf pose (get-elem-pose (nth index elem-list)))
      (cl-tf:set-transform *tf* (cl-transforms-stamped:make-transform-stamped "map" (nth index elem-list) (roslisp:ros-time) (cl-transforms:origin pose) (cl-transforms:orientation pose)))
      (setf obj-pose (cl-transforms-stamped:transform->pose (cl-tf:lookup-transform *tf* viewpoint (nth index elem-list))))
      (if (plusp (cl-transforms:x (cl-transforms:origin obj-pose)))
          (setf poses (append poses (list (nth index elem-list))))))
    (first poses)))


(defun check-all-designators (desigs)
  (let((var NIL))
    (dotimes (index (length desigs))
      (cond ((or (string-equal (desig-prop-value (nth index desigs) :to) "take-picture")
                 (string-equal (desig-prop-value (nth index desigs) :to) "stop")
                 (string-equal (desig-prop-value (nth index desigs) :to) "land")
                 (string-equal (desig-prop-value (nth index desigs) :to) "show-picture"))
             (setf var T))
            ((and (string-equal (desig-prop-value (nth index desigs) :to) "mount")
                  (not (null (desig-prop-value (nth index desigs) :agent))))
             (setf var T))
            ((and (string-equal (desig-prop-value (nth index desigs) :to) "scan")
                  (not (null (desig-prop-value (nth index desigs) :agent))))
             (setf var T))
            ((and (string-equal (desig-prop-value (nth index desigs) :to) "look-for")
                  (not (null (desig-prop-value (nth index desigs) :object))))
             (setf var T))
            ((and (string-equal (desig-prop-value (nth index desigs) :to) "come-back")
                  (not (null (desig-prop-value (nth index desigs) :pose))))
             (setf var T))
            ((string-equal (desig-prop-value (nth index desigs) :to) "go")
             (let*((loc (desig-prop-value (nth index desigs) :destination))
                  (prop (desig:properties loc)))
               (cond((and (assoc :pose prop)
                          (not (null (desig-prop-value loc :pose))))
                     (setf var T))
                    ((or (assoc :next-to prop)
                         (assoc :right prop)
                         (assoc :left prop)
                         (assoc :ontop prop)
                         (assoc :behind prop))
                     (if (or (not (null (desig-prop-value loc :next-to)))
                             (not (null (desig-prop-value loc :right)))
                             (not (null (desig-prop-value loc :left)))
                             (not (null (desig-prop-value loc :ontop)))
                             (not (null (desig-prop-value loc :left))))
                         (setf var T))))))))
    var))
                  
