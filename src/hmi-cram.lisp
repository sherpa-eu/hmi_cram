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
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF U

(in-package :hmi-cram)

(defvar *sem-map* NIL)
(defvar *tf* NIL)
(defvar *pub* NIL)


;; ROSSERVICE FOR CALLING HMI-CRAM
(defun hmi-cram ()
  (hmi-cram-call))

(defun hmi-cram-call ()
  (roslisp-utilities:startup-ros :name "hmi_cram_service")
  (roslisp:register-service "service_hmi_cram" 'hmi_interpreter-srv:HMIDesig)
  (setf *sem-map* (sem-map-utils:get-semantic-map))
  (roslisp:ros-info (basics-system) "start hmi_cram_service")
  (roslisp:spin-until nil 1000))

(roslisp:def-service-callback hmi_interpreter-srv::HMIDesig (desigs)
 
  (tf-busy-genius-to-map)
 ;;(display-semantic-map)
 (let (;;(id  (beliefstate:start-node "INTERPRET-INSTRUCTION-DESIGNATOR" NIL 2))
     ;;  (newliste '())
       (create_desig (create-desig-based-on-hmi-call desigs))
       (semantic_desig '()))
    (tf-busy-genius-to-map)
  ;; (format t "after agent-pose~%")
   (dotimes (index (length create_desig))
   ;;    (format t "deisgnator~%")
     (setf semantic_desig (append (list (add-semantic-to-desigs (desig-prop-value (nth index create_desig) :viewpoint) (nth index create_desig))) semantic_desig)))
   (setf semantic_desig (reverse semantic_desig))
    (format t "[(CRAM-REASON-DESIG) INFO] DESIG: ~a~%" semantic_desig)
(roslisp:make-response :result "Done!")))

(defun talker ()
 ;; (sleep 3.0)
  (format t "talker~%")
    (let((pub (roslisp:advertise  "/speaker_on" "std_msgs/String")))
      (roslisp:publish-msg pub :data (format nil "~%"))))

(defun init-tf ()
  (setf *tf* (make-instance 'cl-tf:transform-listener))
  (setf *pub* (cl-tf:make-transform-broadcaster)))

(roslisp-utilities:register-ros-init-function init-tf)

(defun display-semantic-map ()
  (if (null *sem-map*)
      (setf *sem-map* (sem-map-utils:get-semantic-map)))
 ;; (format t "display function~%")
  (let*((sem-hash (slot-value *sem-map* 'sem-map-utils:parts))
        (sem-keys (hash-table-keys sem-hash)))
    (dotimes (index (length sem-keys))
    ;;  (publish-box (cl-transforms:make-identity-pose) :id 200)
      (cond((and (not (search "MountainRoad" (nth index sem-keys)))
                 (<= index 1)) 
            ;;(format t "pose ~a and keys ~a~%" (get-pose-by-elem (nth index sem-keys))(nth index sem-keys))
      (publish-box (get-pose-by-elem (nth index sem-keys)) (get-bbox-by-elem (nth index sem-keys)) :id (+ 10000 index)))))))
