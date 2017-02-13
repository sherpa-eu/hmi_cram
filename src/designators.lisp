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
  (format t "go there ~%")
  (let((action-list '())
       (loc_desig NIL)
       (actor (std_msgs-msg:data
               (hmi_interpreter-msg:actor index)))
       (operator (std_msgs-msg:data
                  (hmi_interpreter-msg:instructor index)))
       (viewpoint (std_msgs-msg:data
                   (hmi_interpreter-msg:viewpoint index)))
       (propkeys (hmi_interpreter-msg:propkeys index)))
    (loop for jndex being the elements of propkeys
          do(let((pose NIL)
                 (pointed-pose NIL))
              ;;(format t "go tssshere123 ~a~%"(hmi_interpreter-msg:pointing_gesture jndex))
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
              ;;(format t "go there123 ~%")
              (publish-pose pose :id 19208213 :zet 0.0)
              (setf pointed-pose (give-pointed-direction pose))
              ;;(format t "go there1234 ~%")
              (setf loc_desig (make-designator :location `((:viewpoint ,viewpoint)
                                                           (:pose ,pointed-pose))))
              ;;(format t "go there1235 ~%")
              (if (string-equal "robot" actor)
                  (setf actor NIL))
              (setf action-list (append action-list
                                        (list (make-designator :action `((:to ,(set-keyword "go")) 
                                                                         (:actor ,actor)
                                                                         (:operator ,(set-keyword operator))
                                                                         (:destination ,loc_desig))))))))
    ;;(format t "go there12356 ~%")
    action-list))


(defun make-designator-with-adapted-actions (action actor operator viewpoint objname loc_desig)
  (let((desig NIL)
       (elem-name NIL))
    (cond((or (string-equal "scan-lake-area" action)
              (string-equal "scan-bridge-area" action)
              (string-equal "scan-tunnel-area" action)
              (string-equal "scan-forest-area" action))
          (setf elem-name  (second (split-sequence:split-sequence #\- action)))
          (setf elem-name (get-specific-elem elem-name viewpoint))
          (setf desig (list (make-designator :action `((:to ,(set-keyword "scan"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       ;; (:viewpoint ,viewpoint)
                                                       (:area ,elem-name))))))
         ((or (string-equal "take-picture" action)
              (string-equal "show-picture" action))
          (setf desig (list (make-designator :action `((:to ,(set-keyword action))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator)))))))
         ;;(:viewpoint ,viewpoint))))))
         ((string-equal "come-back" action)
          (setf desig (list (make-designator :action `((:to ,(set-keyword "go"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       (:to ,(make-designator :location `((:viewpoint ,viewpoint)
                                                                                          (:next-to "busy_genius"))))))
                            (make-designator :action `((:to ,(set-keyword "land"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator)))))))
         ((or (string-equal "look-for" action)
              (string-equal "search-for" action))
          (setf desig (list (make-designator :action `((:to ,(set-keyword "look-for"))
                                                       (:actor ,actor)
                                                       (:operator ,(set-keyword operator))
                                                       ;; (:viewpoint ,viewpoint)
                                                       (:object ,objname))))));; (get-specific-elem objname viewpoint)))))))
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
  (let ((action-list '())
        (act-list '())
        (actor NIL)
        (flist NIL))
    (loop for index being the elements of desigs
          do (if (string-equal "go-there" (std_msgs-msg:data
                                           (hmi_interpreter-msg:action_type index)))
                 (setf action-list (append action-list (go-there-function index)))
                 (setf action-list (append action-list (various-commands-function index)))))
    (dotimes (index (length action-list))
      (let((pose NIL))  
        (cond((and (not (null (desig-prop-value (nth index action-list) :destination)))
                   (null (desig-prop-value (desig-prop-value (nth index action-list) :destination) :pose)))
              (setf flist (first (first (desig:properties (desig-prop-value (nth index action-list) :destination)))))
           
              (cond((string-equal (second flist) "null")
                    ;;(format t "index ~a~%" (nth index action-list))
                    (setf pose (calculate-relation-by-agent-pose (desig-prop-value (nth index action-list) :viewpoint)
                                                                 (first flist)))
                    ;;  (format t "index123 ~a~%" pose)
                    ;; (publish-pose pose :id 100)
                    (if (not (string-equal "robot" (desig-prop-value (nth index action-list) :actor)))
                        (setf actor (desig-prop-value (nth index action-list) :actor)))
                    (setf act-list (append act-list
                                           (list
                                            (make-designator :action `((:to ,(desig-prop-value (nth index action-list) :to))
                                                                       (:actor ,actor)
                                                                       (:operator ,(desig-prop-value (nth index action-list) :operator))
                                                                       (:destination
                                                                        ,(make-designator :location `((:viewpoint ,(desig-prop-value (nth index action-list) :viewpoint))
                                                                                                      (:pose ,pose)))))))))
                    (publish-pose pose :id 3223)

                    );;(format t "index456 ~a~%" (nth index action-list)))
                   (t (setf act-list (append act-list (list (nth index action-list))))))
              );;(format t "indexxdasdsad ~a~%" act-list))
             (t (setf act-list (append act-list (list (nth index action-list))))))))
    (setf action-list act-list)
    ;; (format t "action-list ~a~%" action-list)
    action-list))

(defun various-commands-function (index)
  (format t "various commands~%")
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
        (propkeys (hmi_interpreter-msg:propkeys index)))
     (format t "various commands1~%")
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
              (format t "various commands2~%")
              (cond((and (string-equal "true" flag)
                         (null (get-elem-pose object)))
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
              (format t "various commands13q%")
              (if (null obj)
                  (setf obj object))
              (if (string-equal spatial "to")
                  (setf spatial "next-to"))
              (setf property-list (append (list  (list (list (set-keyword spatial) obj)
                                                       (list :color color)
                                                       (list :size size)
                                                       (list :num num))) property-list))))
    (setf loc_desig (make-designator :location (reverse property-list)))
    (format t "ende~%")
    (if (string-equal "robot" actor)
        (setf actor NIL))
    (setf action-list (append action-list (make-designator-with-adapted-actions action
                                                                                actor
                                                                                operator
                                                                                viewpoint
                                                                                obj
                                                                                loc_desig)))
    action-list))

(defun add-semantic-to-desigs (viewpoint desig) 
  (setf actor NIL)
  (format t "add-semantic-to-desigs~%")
  (cond((and ;;(null (desig-prop-value desig :to))
         (null (desig-prop-value desig :destination))
         (null (desig-prop-value desig :area))
         (null (desig-prop-value desig :obj))
         (null (desig-prop-value desig :agent)))
        ;; (format t "nix~%")
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
          (format t "goal123 ~a~%" goal)
          (cond ((= 1 (length proplist))
                 (format t "teeeet~%")
                 (cond((equal (type-of (second (first (first (last proplist))))) ;;((:spatial "object")...)
                              'cl-transforms:pose) ;;if object type of pose          
                       
                       (setf tmpproplist
                             (append tmpproplist
                                     (list (list (first (first (first (last proplist))))
                                                 (second (first (first (last proplist)))))))))
                      (t (format t "jdjddj~%")
                         (setf posy (get-elem-pose (second (first (first (last proplist))))))
                         (cond ((not (null posy))
                                
                                (setf felem (second (first (first (last proplist)))))
                                (setf pose posy)                      
                                (format t "pos-------------------e ~a~%"pose)
                                
                                (setf tmpproplist (append tmpproplist
                                                          (list (list :pose
                                                                      pose)))))
                               ((and (null (get-elem-pose (second (first (first proplist))))) ;;;if object is not-name
                                     (and (or (not (null (second (first (first proplist))))) ;; if obj is not NIL or
                                              (not (string-equal "null" (second (first (first proplist)))))) ;; if obj is not "null"
                                          (and (not (null (second (first (first proplist))))) ;;if obj is not NIL and
                                               (not (string-equal "null" (second (first (first proplist)))))))) ;; if obj is not "null"
                                (format t "hier ist der type~%")
                                (setf felem  (first (get-all-elems-front-agent-by-type
                                                     (second (first (first proplist))) viewpoint)))
                                ;;    (format t "felem: ~a~%" felem)
                                (roslisp:wait-for-service "add_costmap_name" 10)
                                (setf pose 
                                      (roslisp:call-service "add_costmap_name"
                                                            'hmi_interpreter-srv:text_parser :goal felem))
                                (setf tmpproplist (append tmpproplist
                                                          (list (list (first (first (first (last proplist))))
                                                                      felem)))))
                               (t (format t "ich weiss nicht~%")
                                  (setf tmpproplist (append tmpproplist
                                                            (list (list (first (first (first (last proplist))))
                                                                        felem)))))))))
                ((= 2 (length proplist))
                 (setf tmpproplist (add-semantics-two-desigs proplist viewpoint))))
          (setf tmpproplist (append (list (list :viewpoint (desig-prop-value desig :viewpoint))) tmpproplist))
          ;; (format t "tmpproplist ~a~%" tmpproplist)
          
          (if (or (not (string-equal "robot" (desig-prop-value desig :actor)))
                  (not (null (desig-prop-value desig :actor))))
              (setf actor (desig-prop-value desig :actor)))
          (setf desig (make-designator :action `((:to ,(desig-prop-value desig :to))
                                                 (:actor ,actor)
                                                 (:operator ,(desig-prop-value desig :operator))
                                                 (:destination ,(make-designator :location tmpproplist))))))))
  ;; (format t "print desig ~a~%" desig)
  desig)

(defun add-semantics-two-desigs  (proplist viewpoint)
  ;; (format t " add-semantics-two-desigs ~a~%" proplist)
  (let*((list2 (first (last proplist)))
        (list1 (first proplist))
        (typelist1 (get-front-elems-of-agent-by-type (second (first list1)) viewpoint))
        (tmpproplist '())
        (dist 10)
        (selem NIL)(felem NIL))
    ;; (format t "teest ~a~%"  (second (first list1)))
    (cond((and (null (get-pose-by-elem (second (first list1)))) ;;type
               (null (get-pose-by-elem (second (first list2))))) ;;type
          ;; (format t "~a~% ~a~%" (second (first list2)) (first (first list1)))
          (dotimes (index (length typelist1))
            (let((tmp (get-next-elem-based-on-prev-elem 
                       (second (first list2))
                       (first (first list1))
                       (nth index typelist1))))
              (dotimes (in (length tmp))
                (if (not (null (nth in tmp)))
                    (cond((and (> 20 (read-from-string (second (split-sequence:split-sequence #\: (nth in tmp)))))
                               (>= dist (read-from-string (second (split-sequence:split-sequence #\: (nth in tmp))))))
                          (setf dist (read-from-string (second (split-sequence:split-sequence #\: (nth in tmp)))))
                          (setf felem (nth index typelist1))
                          (setf selem (first (split-sequence:split-sequence #\: (nth index tmp)))))))))))
         ((and (not (null (get-pose-by-elem (second (first list1))))) ;;name
               (null (get-pose-by-elem (second (first list2))))) ;;type
          (setf selem (first
                       (split-sequence:split-sequence #\: (first
                                                           (get-next-elem-based-on-prev-elem
                                                            (second (first list2))
                                                            (first (first list1))
                                                            (second (first list1)))))))
          (setf felem (second (first list1))))
         ((and (null (get-pose-by-elem (second (first list1)))) ;;name
               (not (null (get-pose-by-elem (second (first list2)))))) ;;type
          (setf felem (first
                       (split-sequence:split-sequence #\: (first
                                                           (get-prev-elem-based-on-next-elem 
                                                            (second (first list1))
                                                            (first (first list1))
                                                            (second (first list2)))))))
          (setf selem (second (first list2)))))               
    (setf tmpproplist (append tmpproplist (list (list (first (first list1))
                                                      felem)
                                                (list (first (first list2))
                                                      selem))))                    
    tmpproplist))



(defun reference-designators (desigs)
  (format t "reference-designators ~a~%" desigs)
  (let((newliste '())
       (loc NIL)(pose NIL))
    (dotimes (index (length desigs))
      (format t "index desigs ~a~%" (nth index desigs))
      (cond ((and (not (null (desig-prop-value (nth index desigs) :destination)))
                  (null (desig-prop-value (desig-prop-value (nth index desigs) :destination) :pose)))
             (format t "123~%")
             (setf loc (desig-prop-value (nth index desigs) :destination))
             (roslisp:wait-for-service "add_costmap_name" 10)
             (setf pose 
                   (roslisp:call-service "add_costmap_name"
                                                            'hmi_interpreter-srv:text_parser :goal "get"))
          ;;   (setf pose (get-elem-pose  loc))
             (format t "reference ~a~%" pose)
             (setf newliste (append newliste
                                    (list (make-designator :action `((:to ,(desig-prop-value (nth index desigs) :to))
                                                                     (:actor ,(desig-prop-value (nth index desigs) :actor))
                                                                     (:operator ,(desig-prop-value (nth index desigs) :operator))
                                                                     (:destination
                                                                      ,(make-designator :location `((:viewpoint ,(desig-prop-value (desig-prop-value (nth index desigs) :destination) :viewpoint))
                                                                                                    (:pose ,pose))))))))))
            (t (format t "hier~%")
               (setf newliste (append newliste (list (nth index desigs)))))))
    newliste))
