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
    (roslisp:wait-for-service "add_openEase_object" 10)
    (setf obj (slot-value (roslisp:call-service "add_openEase_object"
                       'hmi_interpreter-srv:text_parser :goal "get")
                         'hmi_interpreter-srv:result))
    (loop for jndex being the elements of propkeys
          do(let((pose NIL)
                 (pointed-pose NIL))
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
              (publish-pose pose :id 19208213)
              (if (string-equal "none" obj)
                  (setf pointed-pose (give-pointed-direction pose))
                  (setf pointed-pose obj))
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


(defun make-designator-with-adapted-actions (action actor operator viewpoint objname loc_desig)
  (let((desig NIL)(test1 NIL)(test2 NIL)
       (elem-name NIL))
    (cond((or (string-equal "scan-lake-area" action)
              (string-equal "scan-bridge-area" action)
              (string-equal "scan-tunnel-area" action))
          (setf elem-name  (second (split-sequence:split-sequence #\- action)))
          (setf test1 (first (get-all-elems-front-agent-by-type elem-name viewpoint)))
          (setf test2 (first (get-all-elems-by-type elem-name)))
          (if (string-equal test1 test2)
              (setf elem-name test1)
              (setf elem-name test2))
          (format t "elemname ~a~%" elem-name)
         ;; (setf elem-name (first (get-all-elems-front-agent-by-type elem-name viewpoint)))
          (setf desig (list (make-designator :action `((:to ,(set-keyword "scan"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:area ,elem-name))))))
         ((string-equal "scan-area" action)
          (setf desig (list (make-designator :action `((:to ,(set-keyword "scan"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:area ,objname))))))
         ((or (string-equal "take-picture" action)
              (string-equal "show-picture" action))
          (setf desig (list (make-designator :action `((:to ,(set-keyword action))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator)))))))
         ((string-equal "come-back" action)
          (setf desig (list (make-designator :action `((:to ,(set-keyword "go"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:destination ,(make-designator :location `((:viewpoint ,viewpoint)
                                                                                          (:pose
                                                                                           ,(tf-busy-genius-to-map))))))))))
         ((or (string-equal "look-for" action)
              (string-equal "search-for" action))
          (setf desig (list (make-designator :action `((:to ,(set-keyword "look-for"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:object ,(set-keyword objname)))))))
         ((or (string-equal "take-off" action)
              (string-equal "land" action))
          (setf desig (list (make-designator :action `((:to ,(set-keyword "land"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator)))))))
         ((string-equal "mount" action)
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
                                                         (:viewpoint ,viewpoint)
                                                         (:destination ,loc_desig)))))))
    desig))

(defun create-desig-based-on-hmi-call (desigs)
  (format t "[(CRAM-CREATE-DESIG) INFO] DESIG: ~a~%" desigs)
  (let ((act-list '())
        (action-list '())
        (actor NIL) (obj NIL)
        (flist NIL))
    (loop for index being the elements of desigs
          do (if (string-equal "go-there" (std_msgs-msg:data
                                           (hmi_interpreter-msg:action_type index)))
                 (setf action-list (append action-list (go-there-function index)))
                 (setf action-list (append action-list (various-commands-function index)))))
    (dotimes (index (length action-list))
      (let((pose NIL))
        (cond((and (not (null (desig-prop-value (nth index action-list) :destination)))
                (desig-prop-value (desig-prop-value (nth index action-list) :destination) :of))
              (setf obj (desig-prop-value (desig-prop-value (nth index action-list) :destination) :of))
              (setf act-list (append act-list (make-designator :action `((:to ,(desig-prop-value (nth index action-list) :to))
                                                                         (:actor ,actor)
                                                                         (:operator ,(desig-prop-value (nth index action-list) :operator))
                                                                         (:destination
                                                                          ,(make-designator :location `((:viewpoint ,(desig-prop-value (nth index action-list) :viewpoint))
                                                                                                        (:of ,obj)))))))))              
          ((and (not (null (desig-prop-value (nth index action-list) :destination)))
                   (null (desig-prop-value (desig-prop-value (nth index action-list) :destination) :pose)))
              (setf flist (first (first (desig:properties (desig-prop-value (nth index action-list) :destination)))))           
              (cond((string-equal (second flist) "null")
                    (setf pose (calculate-relation-by-agent-pose (desig-prop-value (nth index action-list) :viewpoint)
                                                                 (first flist)))
                    (if (or (not (string-equal "robot" (desig-prop-value (nth index action-list) :actor)))
                            (not (string-equal "null" (desig-prop-value (nth index action-list) :actor)))
                            (not (null  (desig-prop-value (nth index action-list) :actor))))
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
    act-list))

(defun various-commands-function (index)
  (let ((action-list '())
        (property-list NIL)
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
    (roslisp:wait-for-service "add_openEase_object" 10)
    (setf oe-object (slot-value
                     (roslisp:call-service "add_openEase_object"
                                           'hmi_interpreter-srv:text_parser :goal "get")
                     'hmi_interpreter-srv:result))
    (loop for jndex being the elements of propkeys
          do(let((pose NIL)
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
              (cond((and (string-equal "true" flag)
                         (null (get-elem-pose object))
                         (string-equal oe-object "none"))
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
                   ((and (not (string-equal "mount" action))
                         (string-equal "true" flag)
                        (not (string-equal oe-object "none")))
                    (setf obj oe-object))
                   (t (setf obj NIL)))
              (if (null obj)
                  (setf obj object))
              (if (string-equal spatial "to")
                  (if (string-equal "bridge" object)
                      (setf spatial "ontop")
                      (if (or (string-equal "victim" object)
                              (string-equal "kite" object))
                          (setf spatial "of")))
                  (setf spatial "next-to"))
              (setf property-list (append (list  (list 
                                                       (list (set-keyword spatial) obj)
                                                       (list :color color)
                                                       (list :size size)
                                                       (list :num num))) property-list))))
    (setf loc_desig (make-designator :location (reverse property-list)))
    (if (string-equal "robot" actor)
        (setf actor NIL))
    (setf action-list (append action-list (make-designator-with-adapted-actions action
                                                                                actor
                                                                                operator
                                                                                viewpoint
                                                                                obj
                                                                                loc_desig)))
    action-list))


(defun reset-all-services()
  (roslisp:wait-for-service "add_costmap_name" 10)
  (roslisp:call-service "add_costmap_name"
                        'hmi_interpreter-srv:text_parser :goal "none")
 (roslisp:wait-for-service "add_openEase_object" 10)
 (roslisp:call-service "add_openEase_object"
                       'hmi_interpreter-srv:text_parser :goal "none"))
  
  
  
(defun add-semantic-to-desigs (viewpoint desig)
  (format t "add semantics ~a~%" desig)
  (let((actor NIL)
       (posy NIL)(test1 NIL)(test2 NIL)
       (pose NIL))
  (cond((and (null (desig-prop-value desig :destination))
             (null (desig-prop-value desig :area))
             (null (desig-prop-value desig :obj))
             (null (desig-prop-value desig :agent)))
        (setf desig desig))
       ((and (not (null (desig-prop-value desig :destination)))
             (not (null (desig-prop-value (desig-prop-value desig :destination) :pose))))
        (setf desig desig))
       ((not (null (desig-prop-value desig :agent)))
        (setf desig desig))
       ((not (null (desig-prop-value desig :obj)))
        (setf desig desig))
       ((not (null (desig-prop-value desig :destination)))
        (let* ((goal (desig-prop-value desig :destination))(felem NIL)(tmpproplist '())
               (proplist (desig:properties goal)))
          (cond ((= 1 (length proplist))
                 (cond((equal (type-of (second (first (last proplist)))) 'cl-transforms:pose)
                       (setf tmpproplist
                             (append tmpproplist
                                     (list (list (first (first (first (last proplist))))
                                                 (second (first (first (last proplist)))))))))
                      (t (setf posy (get-elem-pose (second (first (first (last proplist))))))
                         (cond ((not (null posy))
                                (setf felem (second (first (first (last proplist)))))
                                (setf pose posy)                      
                                (setf tmpproplist (append tmpproplist
                                                          (list (list :pose pose)))))
                               ((or (string-equal "kite" (second (first (first (last proplist)))))
                                    (string-equal "victim" (second (first (first (last proplist))))))
                                (setf tmpproplist (append tmpproplist
                                                          (list (list (first (first (first (last proplist))))
                                                                      (set-keyword (second (first (first (last proplist))))))))))
                                
                               (t
                                (setf test1 (first (get-all-elems-front-agent-by-type
                                                     (second (first (first (last proplist)))) viewpoint)))
                                (setf test2 (first (get-all-elems-by-type 
                                                     (second (first (first (last proplist)))))))
                                (if (string-equal test1 test2)
                                    (setf felem test1)
                                    (setf felem test2))
                               ;; (setf felem  (first (get-all-elems-front-agent-by-type
                                ;;                     (second (first (first (last proplist)))) viewpoint)))
                                (if (null felem)
                                    (setf felem (first (get-all-elems-by-type  (second (first (first (last proplist))))))))
                                (roslisp:wait-for-service "add_costmap_name" 10)
                                (roslisp:call-service "add_costmap_name" 'hmi_interpreter-srv:text_parser :goal felem)
                                (setf tmpproplist (append tmpproplist
                                                          (list (list (first (first (first (last proplist))))
                                                                      felem)))))))))

                                          ((= 2 (length proplist))
                 (setf tmpproplist (add-semantics-two-desigs proplist viewpoint))))
          (setf tmpproplist (append (list (list :viewpoint (desig-prop-value desig :viewpoint))) tmpproplist))
          (if (or (not (string-equal "robot" (desig-prop-value desig :actor)))
                  (not (null (desig-prop-value desig :actor))))
              (setf actor (desig-prop-value desig :actor)))
          (setf desig (make-designator :action `((:to ,(desig-prop-value desig :to))
                                                 (:actor ,actor)
                                                 (:operator ,(desig-prop-value desig :operator))
                                                 (:destination ,(make-designator :location tmpproplist))))))))
desig))

(defun add-semantics-two-desigs  (proplist viewpoint)
  (let*((list2 (first (last proplist)))
        (list1 (first proplist))
        (typelist1 (get-all-elems-front-agent-by-type (second (first list1)) viewpoint))
        (tmpproplist '())
        (dist 10)
        (selem NIL)(felem NIL))
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
      (cond ((and (desig-prop-value (nth index desigs) :destination)
                  (assoc :of (desig:properties (desig-prop-value (nth index desigs) :destination))))
             (setf liste (append liste (list (nth index desigs)))))
        ((and (desig-prop-value (nth index desigs) :destination)
                  (not (desig-prop-value (desig-prop-value (nth index desigs) :destination) :pose)))
            ;; (if(not (equal NIL (resolve-designator (desig-prop-value (nth index desigs) :destination) t)))
                 ;;(setf liste (append liste (list (nth index desigs))))
                 (setf liste (append liste
                                     (list
                                      (make-designator :action `((:to ,(desig-prop-value (nth index desigs) :to))
                                                                 (:actor ,(desig-prop-value (nth index desigs) :actor))
                                                                 (:operator ,(desig-prop-value (nth index desigs) :operator))
                                                                 (:destination ,(make-designator :location `((:viewpoint ,(desig-prop-value (desig-prop-value (nth index desigs) :destination) :viewpoint))
                                                                                                             (:pose ,(get-elem-pose elem)))))))))))
            (t(format t "hier~%")
             (setf liste (append liste (list (nth index desigs)))))))
    liste))


(defun check-resolve-designators (desigs)
  
  (let((liste NIL)
       (elem NIL))
    (roslisp:wait-for-service "add_costmap_name" 10)
  (setf elem (slot-value (roslisp:call-service "add_costmap_name"
                        'hmi_interpreter-srv:text_parser :goal "get") 'hmi_interpreter-srv:result))
    (dotimes (index (length desigs))
      (cond ((and (desig-prop-value (nth index desigs) :destination)
                  (not (desig-prop-value (desig-prop-value (nth index desigs) :destination) :pose)))
             (if(not (equal NIL (resolve-designator (desig-prop-value (nth index desigs) :destination) t)))
                 (setf liste (append liste (list (nth index desigs))))
                 (setf liste (append liste
                                     (list
                                      (make-designator :action `((:to ,(desig-prop-value (nth index desigs) :to))
                                                                 (:actor ,(desig-prop-value (nth index desigs) :actor))
                                                                 (:operator ,(desig-prop-value (nth index desigs) :operator))
                                                                 (:destination ,(make-designator :location `((:viewpoint ,(desig-prop-value (desig-prop-value (nth index desigs) :destination) :viewpoint))
                                                                                                             (:pose ,(get-elem-pose elem))))))))))))
            (t(format t "hier~%")
             (setf liste (append liste (list (nth index desigs)))))))
    liste))
