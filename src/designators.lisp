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

(defun create-desig-based-on-hmi-call (desigs)
  (let ((action-list '()))
    (loop for index being the elements of desigs
	  do(let ((property-list NIL)
		  (loc_desig NIL)
		  (action (std_msgs-msg:data
                           (hmi_interpreter-msg:action_type index)))
		  (actor (std_msgs-msg:data
			  (hmi_interpreter-msg:actor index)))
		  (operator (std_msgs-msg:data
			     (hmi_interpreter-msg:instructor index)))
		  (viewpoint (std_msgs-msg:data
                              (hmi_interpreter-msg:viewpoint index)))
		  (propkeys (hmi_interpreter-msg:propkeys index)))
	      (loop for jndex being the elements of propkeys
		    do(let((pose NIL)
			   (obj NIL)
			   (spatial
                             (std_msgs-msg:data
                              (hmi_interpreter-msg::object_relation jndex)))
			    (object
                               (std_msgs-msg:data
                              (hmi_interpreter-msg::object jndex)))
			   (color
                               (std_msgs-msg:data
                              (hmi_interpreter-msg::object_color jndex)))
                           (size
                             (std_msgs-msg:data
                              (hmi_interpreter-msg::object_size jndex)))
			   (num
                             (std_msgs-msg:data
                              (hmi_interpreter-msg::object_num jndex)))
			   (flag
                             (std_msgs-msg:data
                              (hmi_interpreter-msg::flag jndex))))
			(if (and (string-equal spatial "null")
               (not (string-equal "null" object)))
			    (setf spatial "ontop"))
			(cond((string-equal "true" flag)
            (format t "flag> ~a~%" flag)
			      (setf pose (cl-transforms:make-3d-vector
					  (geometry_msgs-msg:x
					   (hmi_interpreter-msg:pointing_gesture jndex))
					  (geometry_msgs-msg:y
					   (hmi_interpreter-msg:pointing_gesture jndex))
					  (geometry_msgs-msg:z
					   (hmi_interpreter-msg:pointing_gesture jndex))))
			      (setf obj (give-pointed-obj-based-on-language object pose)))
                             (t (setf obj NIL)))
                        (if (null obj)
                            (setf obj object))
                        (setf property-list (append (list  (list (list (direction-symbol spatial) obj)
                                                                 (list :color color)
                                                                 (list :size size)
                                                                 (list :num num))) property-list))))
        (format t "property-list: ~a~%" (reverse property-list)) 
	      (setf loc_desig (make-designator :location (reverse property-list)))
	      (setf action-list (append action-list (list (make-designator :action `((:type ,action)
										     (:actor ,actor)
										     (:operator ,operator)
										     (:viewpoint ,viewpoint)
										     (:goal ,loc_desig))))))))
    action-list))
			
