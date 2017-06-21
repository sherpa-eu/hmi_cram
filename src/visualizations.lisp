;;; Copyright (c) 2017, Fereshta Yazdani <yazdani@cs.uni-bremen.de>
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

(defun publish-elempose (pose num)
  (setf *marker-publisher*
        (roslisp:advertise "hmi_cram_location_marker" "visualization_msgs/Marker"))
  (let* ((point (cl-transforms:origin pose))
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
                             id num
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
                             (x scale) 20
                             (y scale) 20
                             (z scale) 8.0
                             (r color) 1
                             (g color) 1
                             (b color) 0
                             (a color) 0.4)))))

(defun publish-humanpose (pose num)
  (setf *marker-publisher*
        (roslisp:advertise "hmi_cram_location_marker" "visualization_msgs/Marker"))
  (let* ((point (cl-transforms:origin pose))
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
                             id num
                             type (roslisp:symbol-code 
                                   'visualization_msgs-msg:<marker> :arrow)
                             action (roslisp:symbol-code
                                     'visualization_msgs-msg:<marker> :add)
                             (x position pose) (cl-transforms:x point)
                             (y position pose) (cl-transforms:y point)
                             (z position pose) 1.0
                             (x orientation pose) (cl-transforms:x rot)
                             (y orientation pose) (cl-transforms:y rot)
                             (z orientation pose) (cl-transforms:z rot)
                             (w orientation pose) (cl-transforms:w rot)
                             (x scale) 10
                             (y scale) 3
                             (z scale) 1.0
                             (r color) 1
                             (g color) 0
                             (b color) 1
                             (a color) 1)))))

(defun publish-objpose (pose num)
  (setf *marker-publisher*
        (roslisp:advertise "hmi_cram_location_marker" "visualization_msgs/Marker"))
  (let* ((point (cl-transforms:origin pose))
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
                             id num
                             type (roslisp:symbol-code 
                                   'visualization_msgs-msg:<marker> :arrow)
                             action (roslisp:symbol-code
                                     'visualization_msgs-msg:<marker> :add)
                             (x position pose) (cl-transforms:x point)
                             (y position pose) (cl-transforms:y point)
                             (z position pose) 1.0
                             (x orientation pose) (cl-transforms:x rot)
                             (y orientation pose) (cl-transforms:y rot)
                             (z orientation pose) (cl-transforms:z rot)
                             (w orientation pose) (cl-transforms:w rot)
                             (x scale) 0.5
                             (y scale) 0.5
                             (z scale) 0.7
                             (r color) 1
                             (g color) 0
                             (b color) 0
                             (a color) 1)))))
