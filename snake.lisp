;;;; snake.lisp 
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :snake)

(defparameter *shader-dir* (asdf:system-relative-pathname :snake "shaders/"))

(defclass snake-vertex-shader (newgl:gl-shader)
  ((newgl:layout :initform
           '(((:name . "position")
              (:count . 3)
              (:type . :float))

             ((:name . "uv")
              (:count . 2)
              (:type . :float)))
           :type (or null list))

   (newgl:shader :initform 0 :type fixnum)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "snake-vertex.glsl") :type (or pathname string))
   (newgl:shader-type :initform :vertex-shader)))

(defclass snake-fragment-shader (newgl:gl-shader)
  ((newgl:shader-type :initform :fragment-shader)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "snake-fragment.glsl") :type (or pathname string))))

(defclass snake-program (newgl:shader-program)
  ((newgl:shaders :initform (list
                       (make-instance 'snake-vertex-shader)
                       (make-instance 'snake-fragment-shader)))))

(defclass snake-game (newgl:opengl-object)
  ((vertices :initform
             (make-array
              20
              :element-type 'single-float
              :initial-contents (list
                                 -1.0f0  1.0f0  0.0f0 -1.0f0 1.0f0
                                 -1.0f0 -1.0f0  0.0f0 -1.0f0 -1.0f0
                                 1.0f0  1.0f0  0.0f0 1.0f0 1.0f0
                                 1.0f0 -1.0f0  0.0f0 1.0f0 -1.0f0)))
   (indices :initform (make-array
                       6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 1 3 2)))

   (newgl:shader-program :initform (make-instance 'snake-program)))

  (:documentation "A snake game."))

(defun make-snake-game ()
  (make-instance 'snake-game))

(defmethod newgl:rebuild-shaders ((object snake-game))
  (call-next-method)
  (with-slots (newgl:shader-program) object
    (newgl:build-shader-program newgl:shader-program)))

(defmethod newgl:fill-buffers ((object snake-game))
  (call-next-method)
  (with-slots (newgl:vbos newgl:ebos vertices indices) object
    (cond ((null newgl:vbos)
           (setf newgl:vbos (gl:gen-buffers 1))
           (setf newgl:ebos (gl:gen-buffers 1))
           (let ((gl-vertices (newgl:to-gl-float-array vertices))
                 (gl-indices (newgl:to-gl-array indices :unsigned-int)))

             (gl:bind-buffer :array-buffer (car newgl:vbos))
             (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
             (gl:free-gl-array gl-vertices)

             (gl:bind-buffer :element-array-buffer (car newgl:ebos))
             (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
             (gl:free-gl-array gl-indices)))
          (t
           (gl:bind-buffer :array-buffer (car newgl:vbos))
           (gl:bind-buffer :element-array-buffer (car newgl:ebos))))))

(defmethod newgl:render ((object snake-game))
  (with-slots (indices) object
    (gl:polygon-mode :front-and-back :fill)
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length indices))))

(defmethod newgl:handle-key ((object snake-game) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (cond
    (t
     nil)))
  


(defun start-game (&key (in-thread nil) (show-traces nil))
  
  ;; Some traces that are helpful for debugging
  (when show-traces
    (trace
     gl:bind-buffer
     gl:bind-vertex-array
     gl:draw-elements
     gl:enable-vertex-attrib-array
     gl:gen-vertex-array
     gl:get-attrib-location
     gl:polygon-mode
     gl:use-program
     gl:vertex-attrib-pointer

     newgl::build-shader-program
     newgl::ensure-vao-bound
     newgl::fill-buffers
     newgl::render
     newgl::use-layout
     newgl::use-shader-program
     newgl::handle-drag
     newgl::handle-click
     newgl::handle-scroll
     newgl::handle-key
     ))
  (newgl:viewer :objects (make-instance 'snake-game) :in-thread in-thread :show-traces show-traces))

