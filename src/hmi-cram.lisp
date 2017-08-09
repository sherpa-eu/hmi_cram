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
(defvar *cmd-pub* nil)
(defvar *gesture* nil)
(defvar *id-logger* nil)
;; ROSSERVICE FOR CALLING HMI-CRAM
(defun hmi-main ()
  (hmi-cram-call))

(defun hmi-cram-call ()
  (roslisp-utilities:startup-ros :name "hmi_cram_service")
  (roslisp:register-service "service_hmi_cram" 'hmi_interpreter-srv:HMIDesig)
  (setf *cmd-pub* (roslisp:advertise "check_cmd_collector" "std_msgs/String"))
  (setf *gesture* (roslisp:advertise "genius_gesture" "geometry_msgs/PoseStamped"))
  (setf *id-logger* "cram")
  (roslisp:ros-info (basics-system) "start hmi_cram_service")
  (roslisp:spin-until nil 1000))

(roslisp:def-service-callback hmi_interpreter-srv::HMIDesig (desigs)
  (let ((create_desig (create-desig-based-on-hmi-call desigs))
        (semantic_desig '())(tmp NIL)(id NIL))
    (if (string-equal *id-logger* "cram")
        (setf id  (beliefstate:start-node "logging-designator" NIL 2))
        (setf id  (beliefstate:start-node "logging-proactive-designator" NIL 2)))
    (publish-humanpose  (tf-busy-genius-to-map) 2981384847289346)
    (dotimes (index (length create_desig))
      (beliefstate:add-designator-to-active-node (nth index create_desig))
      (setf semantic_desig
            (append semantic_desig (list (add-semantic-to-desigs
                                          (nth index create_desig))))))

    (setf tmp (check-all-designators semantic_desig))
    (setf robots-common::*logging-enabled* t)
    (format t "logging-enabled ~a~%" robots-common::*logging-enabled*)
    (cond((null tmp)
          (format t "[(CRAM-REASON-DESIG) INFO] Did not work for DESIG: ~a~%" semantic_desig)
          (reset-all-services)
          (beliefstate:stop-node id)
          (if (string-equal *id-logger* "cram")
              (beliefstate:extract-files :name "logging-designator")
              (beliefstate:extract-files :name "logging-proactive-designator"))
          (roslisp:make-response :result "Done!"))
         (t
          (setf semantic_desig (check-resolve-designators semantic_desig))
          (beliefstate:add-designator-to-active-node (first semantic_desig))
          (roslisp:publish *cmd-pub*
                           (roslisp:make-message "std_msgs/String" :data "checked"))
       ;;   (setf semantic_desig (check-resolve-desigs-pose semantic_desig))
          (beliefstate:add-designator-to-active-node (first semantic_desig))
          (format t "[(CRAM-REASON-DESIG) INFO] DESIG: ~a~%" semantic_desig)
         (setf robots-common::*logging-enabled* t)
         (commander:human-command (first semantic_desig))
          (reset-all-services)
          (beliefstate:stop-node id)
          (if (string-equal *id-logger* "cram")
              (beliefstate:extract-files :name "logging-designator")
              (beliefstate:extract-files :name "logging-proactive-designator"))
          (roslisp:make-response :result "Done!")))))


(defun proactive-main()
  (proactive-main-call))

(defun proactive-main-call ()
  (roslisp-utilities:startup-ros :name "proactive_behavior_service")
  (roslisp:register-service "service_proactivity" 'hmi_interpreter-srv:HMIDesig)
  (setf *cmd-pub* (roslisp:advertise "check_cmd_collector" "std_msgs/String"))
  (setf *id-logger* "proactive")
  (roslisp:ros-info (basics-system) "start proactive_behavior_service")
  (roslisp:spin-until nil 1000))


;; (defun openease-main()
;;   (openease-main-call))

;; (defun openease-main-call ()
;;   (roslisp-utilities:startup-ros :name "openease_transform_service")
;;   (roslisp:register-service "openease_cram_transform" 'hmi_interpreter-srv:StringArray)
;;   (roslisp:ros-info (basic-system) "start openease_transform_service")
;;   (roslisp:spin-until nil 1000))
