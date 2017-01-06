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
          do(format t "tetetetetete ~a~%"(std_msgs-msg:data
                               (hmi_interpreter-msg:action_type index))
                              "go-there")
              

             (if (string-equal (std_msgs-msg:data
                               (hmi_interpreter-msg:action_type index))
                              "go-there")
                (let((obj NIL)
                     (loc_desig NIL)
                     (action "go")
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
                               (spatial :to)
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
                            (format t "tatatatata2 ~a~%" (geometry_msgs-msg:w
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))
                            (format t "tatatatata2 ~a~%" (geometry_msgs-msg:x
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))
                            (format t "tatatatata2 ~a~%" (geometry_msgs-msg:y
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))
                            (format t "tatatatata2 ~a~%" (geometry_msgs-msg:z
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))
                            (format t "tatatatata2 ~a~%" (geometry_msgs-msg:x
                                                (geometry_msgs-msg:position
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))
                            (format t "tatatatata2 ~a~%" (geometry_msgs-msg:y
                                                (geometry_msgs-msg:position
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))
                            (format t "tatatatata2 ~a~%" (geometry_msgs-msg:z
                                                (geometry_msgs-msg:position
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))
                            (setf pose (cl-transforms:make-pose 
                                              (cl-transforms:make-3d-vector
                                               (geometry_msgs-msg:x
                                                (geometry_msgs-msg:position
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:y
                                                (geometry_msgs-msg:position
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:z
                                                (geometry_msgs-msg:position
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))
                                              (cl-transforms:make-quaternion
                                               (geometry_msgs-msg:x
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:y
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:z
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:w
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))))
                            (format t "tatatatata~%")
                            (setf obj (give-pointed-direction pose))
                  (setf loc_desig (make-designator :location `(((:to ,obj)))))
                  (setf action-list (append action-list
                                            (list (make-designator :action `((:type ,action)
                                                                             (:actor ,actor)
                                                                             (:operator ,operator)
                                                                             (:viewpoint ,viewpoint)
                                                                             (:goal ,loc_desig)))))))))
                (let ((property-list NIL)
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
                            (format t "flag> ~a~%" flag)
                            (cond((string-equal "true" flag)
                                  (setf pose (cl-transforms:make-pose 
                                              (cl-transforms:make-3d-vector
                                               (geometry_msgs-msg:x
                                                (geometry_msgs-msg:position
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:y
                                                (geometry_msgs-msg:position
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:z
                                                (geometry_msgs-msg:position
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))
                                              (cl-transforms:make-quaternion
                                               (geometry_msgs-msg:x
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:y
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:z
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex)))
                                               (geometry_msgs-msg:w
                                                (geometry_msgs-msg:orientation
                                                 (hmi_interpreter-msg:pointing_gesture jndex))))))
                                  (setf obj (get-pointed-elem-by-voice-type pose object viewpoint)))
                                 (t (setf obj NIL)))
                            (if (null obj)
                                (setf obj object))
                            (setf property-list (append (list  (list (list (set-keyword spatial) obj)
                                                                     (list :color color)
                                                                     (list :size size)
                                                                     (list :num num))) property-list))))
                  ;;(format t "property-list: ~a~%" (reverse property-list)) 
                  (setf loc_desig (make-designator :location (reverse property-list)))
                  (setf action-list (append action-list (list (make-designator :action `((:type ,action)
                                                                                         (:actor ,actor)
                                                                                         (:operator ,operator)
                                                                                         (:viewpoint ,viewpoint)
                                                                                         (:goal ,loc_desig)))))))))
    action-list))

(defun add-semantic-to-desigs (viewpoint desig)
  (format t "add to desigs semantic desig ~a~%"desig)
  (let* ((goal (desig-prop-value desig :goal))(felem NIL)(tmpproplist '())
         (proplist (desig:properties goal)))
    (cond ((= 1 (length proplist))
           (if (equal (type-of (second (first (first (last proplist))))) 'cl-transforms:pose)
               (setf tmpproplist (append tmpproplist (list (list (first (first (first (last proplist))))
                                                            (second (first (first (last proplist))))))))      
               (cond ((not (null (get-pose-by-elem (second (first (first (last proplist))))))) ;;name
                      (format t "teeeeeeeest123------------~%")
                      (setf felem (second (first (first (last proplist)))))
                      (setf tmpproplist (append tmpproplist (list (list (first (first (first (last proplist))))
                                                                        felem)))))
                     ((and (null (get-pose-by-elem (second (first (first proplist))))) ;;;not-name
                                   (and (or (not (null (second (first (first proplist))))) ;;not NIL
                                            (not (string-equal "null" (second (first (first proplist))))))
                                        (and (not (null (second (first (first proplist))))) ;;not NIL
                                             (not (string-equal "null" (second (first (first proplist))))))))
                      (format t "teeeeeeeest345------------~%")
                      (setf felem  (first (get-front-elems-of-agent-by-type
                                           (second (first (first proplist))) viewpoint)))
                      (setf tmpproplist (append tmpproplist (list (list (first (first (first (last proplist))))
                                                                        felem)))))                 (t (format t "teeeeeeeest----------910911--~%") (setf tmpproplist (append tmpproplist (list (list (first (first (first (last proplist))))
                                                                    felem))))))))
          ((= 2 (length proplist))
           (setf tmpproplist (add-semantics-two-desigs proplist viewpoint))))
    (format t "tetetetetetest ~a~%" (desig-prop-value desig :type))
    (make-designator :action `((:action_type ,(desig-prop-value desig :type))
                               (:actor ,(desig-prop-value desig :actor))
                               (:operator ,(desig-prop-value desig :operator))
                               (:viewpoint ,(desig-prop-value desig :viewpoint))
                               (:goal ,(make-designator :location tmpproplist))))))

(defun add-semantics-two-desigs  (proplist viewpoint)
  (let*((list2 (first (last proplist)))
        (list1 (first proplist))
        (typelist1 (get-front-elems-of-agent-by-type (second (first list1)) viewpoint))
        (tmpproplist '())
        (dist 10)
        (selem NIL)(felem NIL))
          (cond((and (null (get-pose-by-elem (second (first list1)))) ;;type
                     (null (get-pose-by-elem (second (first list2)))))
             ;;   (format t "~a~% ~a~%" (second (first list2)) (first (first list1)))
                (dotimes (index (length typelist1))
                   (let((tmp (get-next-elem-by-prev-elem 
                                   (second (first list2))
                                   (first (first list1))
                                   (nth index typelist1))))
                     (dotimes (in (length tmp))
                          (if (not (null (nth in tmp)))
                              (cond((and (> 20 (read-from-string (second (split-sequence:split-sequence #\: (nth in tmp)))))
                                         (>= dist (read-from-string (second (split-sequence:split-sequence #\: (nth in tmp))))))
                                    (setf dist (read-from-string (second (split-sequence:split-sequence #\: (nth in tmp)))))
                                 (setf felem (nth index typelist1))
                                 (setf selem (first (split-sequence:split-sequence #\: (nth index tmp))))))))))))
                (setf tmpproplist (append tmpproplist (list (list (first (first list1))
                                                                   felem)
                                                             (list (first (first list2))
                                                                   selem))))                    
            tmpproplist))
    


