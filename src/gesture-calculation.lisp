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

(defun get-pointed-elem-by-voice-type (pose type &optional (viewpoint "busy_genius"))
  (if(and (= 0.0d0 (cl-transforms:x (cl-transforms:origin pose)))
          (= 0.0d0 (cl-transforms:y (cl-transforms:origin pose)))
          (= 0.0d0 (cl-transforms:z (cl-transforms:origin pose))))
     (setf pose (give-pointing-related-to-human)))
  (let* ((poses-liste (calculate-ray pose))
        (elem1 (get-all-elems-front-agent-by-type type viewpoint))
        (elem2 elem1)
        (elem '()))
    (dotimes (index (length poses-liste))
      (cond ((string-equal (car elem1) (car elem2))
             (dotimes(in (length elem1))
               (publish-pose  (nth index poses-liste) :id (+ 3000 index))
               (cond ((equal T (check-distance (cl-transforms:origin (get-elem-pose (nth in elem1)))
                                               (cl-transforms:origin (nth index poses-liste))))
                      (setf elem (append elem (list (nth in elem1))))))))
            (t (dotimes (in (length elem2))
                 (cond ((equal T (check-distance (cl-transforms:origin (get-elem-pose (nth in elem2)))
                                                 (cl-transforms:origin (nth index poses-liste))))
                        (setf elem (append elem (list (nth in elem2))))))))))
    (if (null elem)
        (if (null elem1)
            (setf elem (list (car elem2)))
            (setf elem (list (car elem1)))))
    (first (remove-duplicates elem))))

(defun give-pointed-direction (pose)
  (let ((liste (calculate-ray (cl-transforms:make-pose
                               (cl-transforms:make-3d-vector (cl-transforms:x
                                                              (cl-transforms:origin pose))
                                                             (cl-transforms:y
                                                              (cl-transforms:origin pose))
                                                             (cl-transforms:z
                                                              (cl-transforms:origin pose)))
                               (cl-transforms:orientation pose)))))
    (car (last liste))))

(defun square (n)
  (* n n))

(defun give-pointing-related-to-human()
  (let((pose (tf-busy-genius-to-map))
       (gest NIL))
   (cl-tf:set-transform cram-tf::*transformer* (cl-transforms-stamped:make-transform-stamped
                                                               "map" "gesture"
                                                               (roslisp:ros-time)
                                                               (cl-transforms:origin pose)
                                                               (cl-transforms:orientation pose)))
   (setf gest (cl-transforms-stamped:make-pose-stamped "gesture" 0.0
                                                   (cl-transforms:make-3d-vector 100 0 0)
                                                   (cl-transforms:make-identity-rotation)))
    (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose gest :target-frame "map"))))

(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table
             :test 'equal
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
            using (hash-value value)
          do (setf (gethash key ht) value)
          finally (return ht))))

(defun hash-table-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) hash-table)
    keys))


(defun calculate-ray (pose)
  (let((poslist'())(posexyz NIL)(posex-yz NIL)(posex-y-z NIL)
       (posexy-z NIL)(posex NIL)(posexz NIL)(posex-z NIL)
       (posexy NIL)(posex-y NIL)(xyz NIL)(x-yz NIL)(xy-z NIL)
       (x-y-z NIL)(x NIL)(xz NIL)(x-z NIL)(xy NIL)(x-y NIL))
    (publish-pose pose :id 1100)
    (loop for index from 3 to 1000
          do (cl-tf:set-transform cram-tf::*transformer*  (cl-transforms-stamped:make-transform-stamped
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
             (setf xyz  (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose posexyz :target-frame "map")))
             (setf x-yz (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose posex-yz :target-frame "map")))
             (setf x-y-z  (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose posex-y-z :target-frame "map")))
             (setf xy-z  (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose posexy-z :target-frame "map")))
             (setf x (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose posex :target-frame "map")))
             (setf xz (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose posexz :target-frame "map")))
             (setf x-z (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose posex-z :target-frame "map")))
             (setf xy (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose posexy :target-frame "map")))
             (setf x-y (cl-transforms-stamped:pose-stamped->pose (cl-tf:transform-pose cram-tf::*transformer* :pose posex-y :target-frame "map")))     
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
      do(publish-pose (nth test poslist) :id (+  test 100))) poslist)) 
