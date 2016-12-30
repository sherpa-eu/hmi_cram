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

(defun give-pointed-obj-based-on-language (obj vec)
  ;;(publish-pose-color (get-gesture->relative-world  vec (tf-human-to-map)) (cl-transforms:make-3d-vector 1 1 0))  
  (let((liste (give-pointed-at-not-bboxes vec))
       (elem NIL))
    (dotimes (index (length liste))
      (if (and (equal elem NIL)
           (string-equal (get-elem-by-type (nth index liste)) obj))
          (setf elem  (nth index liste))))
    (cond((equal elem NIL)
          (let ((new-liste (get-element-based-on-ground-with-gesture vec)))
            (dotimes (jindex (length new-liste))
                (if (string-equal (get-elem-type (nth jindex new-liste)) obj)
                    (setf elem  (nth jindex liste)))))))
    (if (equal NIL elem)
        (setf elem (get-specific-elem-closeto-human obj)))
    elem))

(defun get-element-based-on-ground-with-gesture (vec)
  (let* ((sem-map  (sem-map:get-semantic-map))
         (elem NIL)
         (sem-hash (slot-value sem-map 'sem-map-utils:parts))
         (sem-keys (hash-table-keys sem-hash))
         (incrementer 0)
         (num (make-list 150))
         (valuable (list-the-values num vec)))
   ;; (format t "test~%")
    (let*((liste (calculated-five-down-levels valuable)))
       (dotimes (mo (length liste))
         do (let*((new-point (nth mo liste))
                  (smarter (+ (* 10 incrementer) 2)))
   (dotimes (jndex (length sem-keys))
               do(let* ((elem1 (first (get-bbox-as-aabb (nth jndex sem-keys) sem-hash)))
                        (elem2 (second (get-bbox-as-aabb (nth jndex sem-keys) sem-hash))))
                   (setf value
                         (semantic-map-costmap::inside-aabb elem1 elem2  (cl-transforms:origin new-point)))
                   (cond ((equal value T)
                            ;;(location-costmap::publish-point (cl-transforms:origin new-point) :id smarter)
                          (setf elem (append (list (nth jndex sem-keys)) elem))
                          (remove-duplicates elem))
                         (t   ;;(location-costmap::publish-point (cl-transforms:origin new-point) :id smarter)
                           )))
                (setf incrementer (+ incrementer 2))))))
              (reverse (remove-duplicates elem)))) 



;;###################################################################################################################################################################################################################################################################################################################GROUND FOR POINTING GESTURE#############################################################################################################################################;;
;; GROUND FOR GESTURE


;;
;;Getting the min and max values of a bounding box
;;
(defun get-bbox-as-aabb (name sem-hash)
(let*((dim-x (cl-transforms:x (slot-value (gethash name sem-hash) 'sem-map-utils:dimensions)))
      (dim-y (cl-transforms:y (slot-value (gethash name sem-hash) 'sem-map-utils:dimensions)))
      (dim-z (cl-transforms:z (slot-value (gethash name sem-hash) 'sem-map-utils:dimensions)))
      (pose-x (cl-transforms:x (cl-transforms:origin  (slot-value (gethash name sem-hash) 'sem-map-utils:pose))))
       (pose-y (cl-transforms:y (cl-transforms:origin  (slot-value (gethash name sem-hash) 'sem-map-utils:pose))))
      (min-vec (cl-transforms:make-3d-vector (- pose-x (/ dim-x 2))
                                             (- pose-y (/ dim-y 2))
                                             0))
      (max-vec (cl-transforms:make-3d-vector (+ pose-x (/ dim-x 2))
                                             (+ pose-y (/ dim-y 2))
                                             dim-z)))
  (cram-semantic-map-costmap::get-aabb min-vec max-vec)))

(defun square (n)
  (* n n))



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

