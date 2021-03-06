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

(defsystem hmi-cram
  :author "yazdani"
  :license "BSD"
  :depends-on (cram-designators
               cram-location-costmap
               cram-prolog
               cram-commander
               roslisp
               cram-beliefstate
               cram-semantic-map-costmap
               ;; cram-bullet-reasoning
               ;; cram-bullet-reasoning-belief-state
               ;; cram-plan-library
               ;;cram-bullet-reasoning-designators:
               ;; cram-beliefstate
               cl-tf
               ;; cram-commander
               cram-sherpa-spatial-relations
               ;; cram-semantic-map-designators
               hmi_interpreter-msg
               hmi_interpreter-srv
               ;;gazebo_msgs-srv
               alexandria
               geometry_msgs-msg
               std_msgs-msg)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "auxiliary" :depends-on ("package" "visualizations"))
     (:file "visualizations" :depends-on ("package"))
     (:file "gesture-calculation" :depends-on ("package" "auxiliary"))
     (:file "designators" :depends-on ("package" "gesture-calculation"))
     (:file "hmi-cram" :depends-on ("package" "designators" "gesture-calculation" "auxiliary" "visualizations"))))))
