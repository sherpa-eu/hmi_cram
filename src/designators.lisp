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


(defun go-there-function (index)
  (let((action-list '())
       (loc_desig NIL)(obj NIL)
       (actor (std_msgs-msg:data
               (hmi_interpreter-msg:actor index)))
       (operator (std_msgs-msg:data
                  (hmi_interpreter-msg:instructor index)))
       (viewpoint (std_msgs-msg:data
                   (hmi_interpreter-msg:viewpoint index)))
       (propkeys (hmi_interpreter-msg:propkeys index)))
    (if (roslisp:wait-for-service "add_openEase_object" 10)
        (setf obj (slot-value (roslisp:call-service "add_openEase_object"
                                                    'hmi_interpreter-srv:text_parser :goal "get")
                          'hmi_interpreter-srv:result)))
    (loop for jndex being the elements of propkeys
          do(let((pose NIL)(dir NIL)
                 (pointed-pose NIL))
              (cond((and (= 0.0d0 (geometry_msgs-msg:x
                            (geometry_msgs-msg:position
                             (hmi_interpreter-msg:pointing_gesture jndex))))
                         (= 0.0d0 (geometry_msgs-msg:y
                                   (geometry_msgs-msg:position
                                    (hmi_interpreter-msg:pointing_gesture jndex))))
                         (= 0.0d0 (geometry_msgs-msg:z
                                   (geometry_msgs-msg:position
                                    (hmi_interpreter-msg:pointing_gesture jndex)))))
                    (cond((not (string-equal "none" obj))
                          (setf pointed-pose (get-elem-pose obj))
                          (call-gesture-logging obj (cl-transforms:make-identity-pose)))
                         (t (setf pointed-pose (give-pointing-related-to-human))
                            (setf dir (give-pointing-direction))
                             (call-gesture-logging "human" dir))) )
                  ;;  (go-into-genius-gesture pointed-pose))
                   (t
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
                    (cond((string-equal "none" obj)
                        (setf pointed-pose (give-pointed-direction pose))
                          )
                     (t (setf pointed-pose (get-elem-pose obj))
                        (call-gesture-logging obj (cl-transforms:make-identity-pose))
                        ))
                    ))
              (setf loc_desig (make-designator :location `((:viewpoint ,viewpoint)
                                                           (:pose ,pointed-pose))))
              (if (string-equal "robot" actor)
                  (setf actor NIL))
              (setf action-list (append action-list
                                        (list (make-designator :action `((:to ,(set-keyword "go")) 
                                                                         (:actor ,actor)
                                                                         (:operator ,(set-keyword operator))
                                                                         (:destination ,loc_desig))))))))
    action-list))

(defun make-designator-with-adapted-actions (action actor operator objname loc_desig)
  (let((desig NIL)(test1 NIL)(test2 NIL)(elem-name NIL))
    (cond((string-equal "scan-lake-area" action)
          (setf test1
                (first (get-all-elems-front-agent-by-type "lake" (second (assoc :viewpoint (desig:properties loc_desig))))))
          (setf test2 (first (get-all-elems-by-type "lake")))
          (if (string-equal test1 test2)
              (setf elem-name test1)
              (setf elem-name test2))
          ;;(call-service-logging elem-name (get-elem-type elem-name) "next")
          (setf desig (list (make-designator :action `((:to ,(set-keyword "scan"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:area ,elem-name))))))
         ((or (string-equal "scan-lake" action)
              (string-equal "scan-area" action))
          ;;(call-service-logging objname (get-elem-type objname) "next")
          (setf desig (list (make-designator :action `((:to ,(set-keyword "scan"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:area ,objname))))))
         ((or (string-equal "take-picture" action)
              (string-equal "show-picture" action))
          (if (or (string-equal "kite" objname)
                  (string-equal "victim" objname))
              (setf desig (list (make-designator :action `((:to ,(set-keyword action))
                                                           (:actor ,actor)
                                                           (:operator ,(set-keyword operator))
                                                           (:destination ,(make-designator :location `((:viewpoint ,(desig-prop-value loc_desig :viewpoint))
                                                                                                       (:of ,(set-keyword (desig-prop-value loc_desig :tmp))))))))))
          (setf desig (list (make-designator :action `((:to ,(set-keyword action))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))))))))
         ((string-equal "come-back" action)
          (setf desig (list (make-designator :action `((:to ,(set-keyword "go"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:destination ,(make-designator :location `((:viewpoint ,(desig-prop-value (desig-prop-value loc_desig :destination) :viewpoint))
                                                                                                   (:pose ,(tf-busy-genius-to-map))))))))))
         ((string-equal "charge" action)
          (setf desig (list (make-designator :action `((:to ,(set-keyword "charge"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator)))))))
         ((string-equal "land" action)
          (cond((or (search "donkey" objname)
                    (search "wasp" objname)
                    (search "hawk" objname))
                ;;(call-service-logging objname (get-elem-type objname) "next")
                (setf desig (list (make-designator :action `((:to ,(set-keyword "land"))
                                                             (:actor ,actor)
                                                             (:operator ,(set-keyword operator))
                                                             (:destination ,(make-designator :location `((:viewpoint ,(desig-prop-value (desig-prop-value loc_desig :destination) :viewpoint))                                                                            (:reachable-for ,objname)))))))))
               (t

                (if (or (null objname)
                        (string-equal "null" objname))
                    (setf desig  (list (make-designator :action `((:to ,(set-keyword "land"))
                                                                  (:actor ,actor)
                                                                  (:operator ,(set-keyword operator))))))
                    (setf desig (list (make-designator :action `((:to ,(set-keyword "land"))
                                                                 (:actor ,actor)
                                                                 (:operator ,(set-keyword operator))
                                                                 (:destination ,loc_desig)))))))))
          ((string-equal "search" action)
           (if (get-elem-pose objname)
               (publish-elempose (get-elem-pose objname) 9876545678923))
           ;;(call-service-logging objname (get-elem-type objname) "next")
           (setf desig (list (make-designator :action `((:to ,(set-keyword "search"))
                                                        (:actor ,actor)
                                                        (:operator ,(set-keyword operator))
                                                        (:object ,(set-keyword (second (second (desig:properties loc_desig)))))
                                                        (:area ,objname))))))
          ((string-equal "look-for" action)
           ;;(call-service-logging objname (get-elem-type objname) "next")
           (setf desig (list (make-designator :action `((:to ,(set-keyword "look-for"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:object ,(set-keyword objname)))))))
        
          ((string-equal "search-for" action)
           ;;(call-service-logging objname (get-elem-type objname) "next")
           (setf desig (list (make-designator :action `((:to ,(set-keyword "search-for"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:object ,(set-keyword objname)))))))
         ((string-equal "take-off" action)
          (setf desig (list (make-designator :action `((:to ,(set-keyword "take-off"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator)))))))
         ((string-equal "mount" action)
          (setf desig (list (make-designator :action `((:to ,(set-keyword action))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:agent ,objname))))))
          ((string-equal "unmount" action)
          (setf desig (list (make-designator :action `((:to ,(set-keyword action))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:agent ,objname))))))
         ((string-equal "stop" action)
          (setf desig (list (make-designator :action `((:to ,(set-keyword action))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator)))))))
         (t (setf desig (list (make-designator :action `((:to ,(set-keyword action))
                                                         (:actor ,actor)
                                                         (:operator ,(set-keyword operator))
                                                         (:destination ,loc_desig)))))))
    desig)) 

(defun create-desig-based-on-hmi-call (messages)
  (format t "[(HMI-TO-DESIG) INFO] HMI: ~a~%" messages)
  (let ((act-list '())
        (action-list '())
        (actor NIL) (obj NIL)(elemname NIL)
        (flist NIL))
    (loop for index being the elements of messages
          do (if (string-equal "go-there" (std_msgs-msg:data
                                           (hmi_interpreter-msg:action_type index)))
                 (setf action-list (append action-list (go-there-function index)))
                 (setf action-list (append action-list (various-commands-function index)))))
    (dotimes (index (length action-list))
      (let((pose NIL))
        (beliefstate:add-designator-to-active-node (nth index action-list))
        (cond ((and (string-equal "go" (desig-prop-value (nth index action-list) :to))
                    (desig-prop-value (nth index action-list) :destination)
                    (desig-prop-value (desig-prop-value (nth index action-list) :destination) :of))
               (setf obj (desig-prop-value (desig-prop-value (nth index action-list) :destination) :of))
               (if (or (search "wasp" obj)
                       (search "donkey" obj)
                       (search "hawk" obj))
                   (setf elemname obj)
                   (setf elemname (set-keyword obj)))
               (setf act-list (append act-list (list (make-designator :action `((:to ,(desig-prop-value (nth index action-list) :to))
                                                                                (:actor ,(desig-prop-value (nth index action-list) :actor))
                                                                                (:operator ,(desig-prop-value (nth index action-list) :operator))
                                                                                (:destination
                                                                                 ,(make-designator :location `((:viewpoint ,(desig-prop-value (desig-prop-value (nth index action-list) :destination) :viewpoint))
                                                                                                               (:of ,elemname))))))))))         
              ((and (desig-prop-value (nth index action-list) :destination)
                    (null (desig-prop-value (desig-prop-value (nth index action-list) :destination) :pose)))
               (setf flist (first (desig:properties (desig-prop-value (nth index action-list) :destination))))
               (cond((string-equal (second flist) "null")
                     (setf pose (calculate-relation-by-agent-pose (desig-prop-value (desig-prop-value (nth index action-list) :destination) :viewpoint)
                                                                  (first flist)))
                     (if (or (not (string-equal "robot" (desig-prop-value (nth index action-list) :actor)))
                             (not (string-equal "null" (desig-prop-value (nth index action-list) :actor)))
                             (desig-prop-value (nth index action-list) :actor))
                         (setf actor (desig-prop-value (nth index action-list) :actor)))
                     (setf act-list (append act-list
                                            (list
                                             (make-designator :action `((:to ,(desig-prop-value (nth index action-list) :to))
                                                                        (:actor ,actor)
                                                                        (:operator ,(desig-prop-value (nth index action-list) :operator))
                                                                        (:destination
                                                                         ,(make-designator :location `((:viewpoint ,(desig-prop-value (nth index action-list) :viewpoint))
                                                                                                       (:pose ,pose))))))))))
                    (t 
                     (setf act-list (append act-list (list (nth index action-list)))))))
              (t (setf act-list (append act-list (list (nth index action-list))))))))
  ;;  (format t "act-list ~a~%" act-list)
    act-list)) 

(defun various-commands-function (index)
  (let ((action-list '())
        (property-list '())
        (loc_desig NIL)
        (action (std_msgs-msg:data
                 (hmi_interpreter-msg:action_type index)))
        (actor (std_msgs-msg:data
                (hmi_interpreter-msg:actor index)))
        (operator (std_msgs-msg:data
                   (hmi_interpreter-msg:instructor index)))
        (viewpoint (std_msgs-msg:data
                    (hmi_interpreter-msg:viewpoint index)))
        (obj NIL)
        (oe-object NIL)
        (propkeys (hmi_interpreter-msg:propkeys index)))
    (cond ((roslisp:wait-for-service "add_openEase_object" 10)
           (setf oe-object (slot-value
                            (roslisp:call-service "add_openEase_object"
                                                  'hmi_interpreter-srv:text_parser :goal "get")
                            'hmi_interpreter-srv:result))
        ;;  (beliefstate:add-designator-to-active-node (make-designator :object `((:name ,oe-object))))
           ;; (if (not (string-equal "none" oe-object)) 
           ;;          (call-service-logging oe-object (get-elem-type oe-object) "final"))
           ))
    (loop for jndex being the elements of propkeys
          do(let((pose NIL)
                 (spatial
                   (std_msgs-msg:data
                    (hmi_interpreter-msg::object_relation jndex)))
                 (object
                   (std_msgs-msg:data
                    (hmi_interpreter-msg::object jndex)))
                 (flag
                   (std_msgs-msg:data
                    (hmi_interpreter-msg::flag jndex))))
              (cond((and (string-equal "true" flag)
                         (null (get-elem-pose object))
                         (string-equal oe-object "none"))
                    (cond((and (= 0.0d0 (geometry_msgs-msg:x
                                         (geometry_msgs-msg:position
                                          (hmi_interpreter-msg:pointing_gesture jndex))))
                          (= 0.0d0 (geometry_msgs-msg:y
                                         (geometry_msgs-msg:position
                                          (hmi_interpreter-msg:pointing_gesture jndex)))) 
                          (= 0.0d0 (geometry_msgs-msg:z
                                         (geometry_msgs-msg:position
                                          (hmi_interpreter-msg:pointing_gesture jndex)))))
                          (setf obj (get-pointed-elem-by-voice-type (cl-transforms:make-identity-pose ) object viewpoint)))
                          (t  
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
                           (setf obj (get-pointed-elem-by-voice-type pose object viewpoint)))))
                   ((and (not (string-equal "mount" action))
                         (string-equal "true" flag)
                         (not (string-equal oe-object "none")))
                    (setf obj oe-object))
                   (t (setf obj NIL)))
              (if (null obj)
                  (setf obj object))
              (cond((string-equal spatial "to")
                    (cond((or (search "ridge" object)
                              (search "elipad" object))
                          (setf spatial "ontop"))
                         ((or (string-equal "victim" object)
                              (string-equal "kite" object))
                          (setf spatial "of"))
                         ((or (search "donkey" object)
                              (search "hawk" object)
                              (search "red_wasp" object)
                              (search "blue_wasp" object))
                          (setf spatial "of")))
                    (if (string-equal spatial "to")
                        (setf spatial "next-to")))
                   ((string-equal spatial "at")
                    (cond ((or (search "elipad" object)
                               (search "ridge" object))
                           (setf spatial "ontop"))))
                   ((string-equal spatial "on")
                    (setf spatial "ontop"))
                   ((or (null spatial)
                        (string-equal "null" spatial))
                    (setf spatial "tmp")))
              (setf property-list (append property-list (list (list (set-keyword spatial) obj))))))
    (setf property-list (append (list (list :viewpoint viewpoint)) property-list))
        (setf loc_desig (make-designator :location property-list))
    (if (string-equal "robot" actor)
        (setf actor NIL))
    (setf action-list (append action-list (make-designator-with-adapted-actions action
                                                                                actor
                                                                                operator
                                                                                obj
                                                                                loc_desig)))
    action-list)) 


(defun reset-all-services()
  (if (roslisp:wait-for-service "add_costmap_name" 10)
      (roslisp:call-service "add_costmap_name"
                            'hmi_interpreter-srv:text_parser :goal "none"))
  (if (roslisp:wait-for-service "add_openEase_object" 10)
      (roslisp:call-service "add_openEase_object"
                            'hmi_interpreter-srv:text_parser :goal "none")))



(defun add-semantic-to-desigs (desig)
  (format t "DESIGS ~a~%" desig)
  (let((actor NIL)
       (posy NIL)(test1 NIL)(test2 NIL)
       (pose NIL))
    (cond((and (null (desig-prop-value desig :destination))
               (null (desig-prop-value desig :area))
               (null (desig-prop-value desig :object))
               (null (desig-prop-value desig :agent)))
          (setf desig desig))
         ((and (desig-prop-value desig :destination)
               (desig-prop-value (desig-prop-value desig :destination) :pose))
          (setf desig desig))
         ((and (desig-prop-value desig :destination)
               (desig-prop-value (desig-prop-value desig :destination) :of))
          (setf desig desig))
         ((and (desig-prop-value desig :destination)
               (assoc  :reachable-for (desig:properties (desig-prop-value desig :destination))))
          (setf desig desig))
         ((desig-prop-value desig :agent)
          (setf desig desig))
         ((desig-prop-value desig :object)
          (setf desig desig))
         ((desig-prop-value desig :area)
          (setf desig desig))
         ((desig-prop-value desig :destination)
          (let* ((goal (desig-prop-value desig :destination))
                 (felem NIL)(tmpproplist '())
                 (proplist (desig:properties goal)))
            (if (assoc :in proplist)
                (setf proplist (list (car proplist)(second proplist))))
            (cond ((= 2 (length proplist))
                   (cond((equal (type-of (second (first (last proplist)))) 'cl-transforms:pose)
                         (setf tmpproplist
                               (append tmpproplist
                                       (list (list (first (first (last proplist)))
                                                   (second (first (last proplist))))))))
                        (t
                         (setf posy (get-elem-pose (second (first (last proplist)))))
                         (cond ((not (null posy))
                                (setf felem (second (first (last proplist))))
                                (setf pose posy)                      
                                (setf tmpproplist (append tmpproplist
                                                          (list (list (first (first (last proplist)))
                                                                      (second (first (last proplist))))))))
                               ((or (string-equal "kite" (second (first (last proplist))))
                                    (string-equal "victim" (second (first (last proplist)))))
                                (setf tmpproplist (append tmpproplist
                                                          (list (list (first (first (last proplist))))
                                                                (second (first (last proplist)))))))
                               (t
                                (setf test1 (first (get-all-elems-front-agent-by-type
                                                    (second (first (last proplist)))
                                                    (desig-prop-value (desig-prop-value desig :destination) :viewpoint))))
                                (setf test2 (first (get-all-elems-by-type 
                                                    (second (first (last proplist))))))
                                (cond ((string-equal test1 test2)
                                       (setf felem test1))
                                      (t
                                       (setf felem test2)))
                                (cond ((null felem)
                                    (setf felem (first (get-all-elems-by-type  (second (first (last proplist))))))))
                                (if (roslisp:wait-for-service "add_costmap_name" 10)
                                    (roslisp:call-service "add_costmap_name" 'hmi_interpreter-srv:text_parser :goal felem))
                              
                                (setf tmpproplist (append tmpproplist
                                                          (list (list (first (first (last proplist)))
                                                                      felem))))
                                )))))
                  ((= 2 (length proplist))
                   (setf tmpproplist (add-semantics-two-desigs proplist (desig-prop-value (desig-prop-value desig :destination) :viewpoint)))))
            (setf tmpproplist (append tmpproplist (list (list :viewpoint (desig-prop-value (desig-prop-value desig :destination) :viewpoint)))))
            (if (or (not (string-equal "robot" (desig-prop-value desig :actor)))
                    (desig-prop-value desig :actor))
                (setf actor (desig-prop-value desig :actor)))
            (setf desig (make-designator :action `((:to ,(desig-prop-value desig :to))
                                                   (:actor ,actor)
                                                   (:operator ,(desig-prop-value desig :operator))
                                                   (:destination ,(make-designator :location tmpproplist))))))))
    desig))

(defun add-semantics-two-desigs  (proplist viewpoint)
  (let*((list2 (first (last proplist)))
        (list1 (first proplist))
        (typelist1 NIL)
        (tmpproplist '())
        (dist 10)
        (selem NIL)(felem NIL))
    (setf typelist1 (get-all-elems-front-agent-by-type (second (first list1)) viewpoint))
    (cond((and (null (get-elem-pose (second (first list1)))) ;;type
               (null (get-elem-pose (second (first list2))))) ;;type
          (dotimes (index (length typelist1))
            (let((tmp (get-next-elem-based-on-prev-elem 
                       (second (first list2))
                       (first (first list1))
                       (nth index typelist1) viewpoint)))
              (dotimes (in (length tmp))
                (if (not (null (nth in tmp)))
                    (cond((and (> 20 (read-from-string (second (split-sequence:split-sequence #\: (nth in tmp)))))
                               (>= dist (read-from-string (second (split-sequence:split-sequence #\: (nth in tmp))))))
                          (setf dist (read-from-string (second (split-sequence:split-sequence #\: (nth in tmp)))))
                          (setf felem (nth index typelist1))
                          (setf selem (first (split-sequence:split-sequence #\: (nth index tmp)))))))))))
         ((and (not (null (get-elem-pose (second (first list1))))) ;;name
               (null (get-elem-pose (second (first list2))))) ;;type
          (setf selem (first
                       (split-sequence:split-sequence #\: (first
                                                           (get-next-elem-based-on-prev-elem
                                                            (second (first list2))
                                                            (first (first list1))
                                                            (second (first list1)) viewpoint)))))
          (setf felem (second (first list1))))
         ((and (null (get-elem-pose (second (first list1)))) ;;name
               (not (null (get-elem-pose (second (first list2)))))) ;;type
          (setf felem (first
                       (split-sequence:split-sequence #\: (first
                                                           (get-prev-elem-based-on-next-elem 
                                                            (second (first list1))
                                                            (first (first list1))
                                                            (second (first list2)) viewpoint)))))
          (setf selem (second (first list2)))))               
    (setf tmpproplist (append tmpproplist (list (list (first (first list1))
                                                      felem)
                                                (list (first (first list2))
                                                      selem))))                    
    tmpproplist))


(defun check-resolve-desigs-pose(desigs)
  (let((liste NIL)
       (elem NIL))
    (roslisp:wait-for-service "add_costmap_name" 10)
    (setf elem (slot-value (roslisp:call-service "add_costmap_name"
                                                 'hmi_interpreter-srv:text_parser :goal "get") 'hmi_interpreter-srv:result))
    (dotimes (index (length desigs))
      (cond((or (string-equal (desig-prop-value (nth index desigs) :to) "search")
                (string-equal (desig-prop-value (nth index desigs) :to) "charge"))
            (setf liste (append liste (list (nth index desigs)))))
           ((and (desig-prop-value (nth index desigs) :destination)
                 (assoc :of (desig:properties (desig-prop-value (nth index desigs) :destination))))
            (setf liste (append liste (list (nth index desigs)))))
           ((and (desig-prop-value (nth index desigs) :destination)
                 (not (desig-prop-value (desig-prop-value (nth index desigs) :destination) :pose)))
            (setf liste (append liste
                                (list
                                 (make-designator :action `((:to ,(desig-prop-value (nth index desigs) :to))
                                                            (:actor ,(desig-prop-value (nth index desigs) :actor))
                                                            (:operator ,(desig-prop-value (nth index desigs) :operator))
                                                            (:destination ,(make-designator :location `((:viewpoint ,(desig-prop-value (desig-prop-value (nth index desigs) :destination) :viewpoint))
                                                                                                        (:pose ,(get-elem-pose elem)))))))))))
           (t
            (setf liste (append liste (list (nth index desigs)))))))
    liste))


(defun check-resolve-designators (desigs)  
  (let((liste NIL)
       (elem NIL))
    (if (roslisp:wait-for-service "add_costmap_name" 10)
        (setf elem (slot-value (roslisp:call-service "add_costmap_name"
                                                     'hmi_interpreter-srv:text_parser :goal "get") 'hmi_interpreter-srv:result)))
    (dotimes (index (length desigs))
      (cond((and (desig-prop-value (nth index desigs) :destination)
                  (not (desig-prop-value (desig-prop-value (nth index desigs) :destination) :pose))
                  (not (desig-prop-value (desig-prop-value (nth index desigs) :destination) :of)))
             (if(not (equal NIL (resolve-designator (desig-prop-value (nth index desigs) :destination) t)))
                 (setf liste (append liste (list (nth index desigs))))
                 (setf liste (append liste
                                     (list
                                      (make-designator :action `((:to ,(desig-prop-value (nth index desigs) :to))
                                                                 (:actor ,(desig-prop-value (nth index desigs) :actor))
                                                                 (:operator ,(desig-prop-value (nth index desigs) :operator))
                                                                 (:destination ,(make-designator :location `((:viewpoint ,(desig-prop-value (desig-prop-value (nth index desigs) :destination) :viewpoint))
                                                                                                             (:pose ,(get-elem-pose elem))))))))))))
            (t
             (setf liste (append liste (list (nth index desigs)))))))
    liste)) 

(defun call-service-logging (name type position viewpoint)
  (format t "call-service-logging ~a type ~a name ~a position ~a viewpoint~%" type name position viewpoint)
  (if (roslisp:wait-for-service "store_reasoning_output" 10)
       (roslisp:call-service "store_reasoning_output" 'hmi_interpreter-srv:reasoning_algo :data name :selected type :position position :viewpoint viewpoint)))
