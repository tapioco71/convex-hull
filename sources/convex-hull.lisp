;;;; -*- mode: lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; convex-hull.lisp
;;;;
;;;; Copyright (c) 2020 Angelo Rossi
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(in-package #:convex-hull)

;; Functions.

 (defun make-path (a)
   (check-type a list)
   (loop
     for i from 0 below (1- (length a))
     for j from (1+ i)
     collect (list (nth i a)
		   (nth j a))))

(defun cw (p1 p2 p3)
  "cw return true if p1 p2 and p3 make a clockwise turn."
  (check-type p1 grid:foreign-array)
  (assert (= 1 (length (grid:dimensions p1))))
  (assert (= 2 (grid:dim0 p1)))
  (check-type p2 grid:foreign-array)
  (assert (= 1 (length (grid:dimensions p2))))
  (assert (= 2 (grid:dim0 p2)))
  (check-type p3 grid:foreign-array)
  (assert (= 1 (length (grid:dimensions p3))))
  (assert (= 2 (grid:dim0 p3)))
  (< (+ (* (grid:gref p1 0) (- (grid:gref p2 1) (grid:gref p3 1)))
        (* (grid:gref p2 0) (- (grid:gref p3 1) (grid:gref p1 1)))
        (* (grid:gref p3 0) (- (grid:gref p1 1) (grid:gref p2 1))))
     0d0))

(defun ccw (p1 p2 p3)
  "ccw return true if p1 p2 and p3 make a counter clockwise turn."
  (check-type p1 grid:foreign-array)
  (assert (= 1 (length (grid:dimensions p1))))
  (assert (= 2 (grid:dim0 p1)))
  (check-type p2 grid:foreign-array)
  (assert (= 1 (length (grid:dimensions p2))))
  (assert (= 2 (grid:dim0 p2)))
  (check-type p3 grid:foreign-array)
  (assert (= 1 (length (grid:dimensions p3))))
  (assert (= 2 (grid:dim0 p3)))
  (> (+ (* (grid:gref p1 0) (- (grid:gref p2 1) (grid:gref p3 1)))
        (* (grid:gref p2 0) (- (grid:gref p3 1) (grid:gref p1 1)))
        (* (grid:gref p3 0) (- (grid:gref p1 1) (grid:gref p2 1))))
     0d0))

(defun sort-points (points-list &rest parameters &key
                                                   (verbose nil))
  "Sorts point in respect the angular position of point."
  (declare (ignorable parameters
                      verbose))
  (check-type points-list list)
  (loop
    for point in points-list
    do
       (check-type point grid:foreign-array)
       (assert (equalp (grid:dimensions point) '(2))))
  (let ((return-value nil))
    (when verbose
      (format *standard-output* "Entering sort-points. ========~%")
      (finish-output *standard-output*))
    (setq return-value (sort (copy-seq points-list)
                             #'(lambda (a b)
                                 (or (< (grid:gref a 0) (grid:gref b 0))
                                     (and (= (grid:gref a 0) (grid:gref b 0))
                                          (< (grid:gref a 1) (grid:gref b 1)))))))
    (when verbose
      (format *standard-output* "Sorted points set: ~s~%" return-value)
      (format *standard-output* "Exiting sort-points. ========~%")
      (finish-output *standard-output*))
    return-value))

(defun perimeter (points-list &rest parameters &key (verbose nil))
  "Compute the perimeter for the convex hull."
  (declare (ignorable parameters verbose))
  (check-type points-list list)
  (let ((return-value nil))
    (when verbose
      (format *standard-output* "Entering perimeter. ========~%")
      (format *standard-output* "Input convex hull set: ~a~%" points-list)
      (finish-output *standard-output*))
    (setq return-value (loop
                         for p in points-list
                         sum (gsll:euclidean-norm (reduce #'(lambda (x y)
                                                              (gsll:elt- (grid:copy x) (grid:copy y)))
                                                          p))))
    (when verbose
      (format *standard-output* "Perimeter value: ~a~%" return-value)
      (format *standard-output* "Exiting sort-points. ========~%")
      (finish-output *standard-output*))
    return-value))

(defun convex-hull (points-list &rest parameters &key
                                                   (iterations-limit nil)
                                                   (verbose nil))
  "Compute the convex hull for points set. Result in a stack."
  (declare (ignorable parameters iterations-limnit verbose))
  (check-type points-list list)
  (loop
    for point in points-list
    do
       (check-type point grid:foreign-array)
       (assert (equalp (grid:dimensions point) '(2))))
  (when iterations-limit
    (check-type iterations-limit (integer 1)))
  (let ((sorted-points nil)
        (p1 nil)
        (p2 nil)
        (up-stack '())
        (down-stack '())
        (points nil)
        (return-value nil))
    (setq sorted-points (sort-points points-list
                                     :verbose verbose))
    (setq p1 (first sorted-points)
          p2 (first (last sorted-points)))
    (when verbose
      (format *standard-output* "Leftmost point: ~a~%Rightmost point: ~a~%" p1 p2)
      (finish-output *standard-output*))
    (push p1 up-stack)
    (push p1 down-stack)
    (loop
      named main-loop
      for i from 1 below (length sorted-points)
      for j from 0
      do
         (when iterations-limit
           (when (>= j iterations-limit)
             (when verbose
               (format *standard-output*
                       "Iterations count limit reached (~a).  Stop.~%"
                       j)
               (finish-output *standard-output*))
             (setq return-value nil)
             (return-from main-loop)))
         (when (or (= i (1- (length sorted-points)))
                   (cw p1 (nth i sorted-points) p2))
           (loop
             while (and (>= (length up-stack) 2)
                        (not (cw (second up-stack) (first up-stack) (nth i sorted-points))))
             do
                (pop up-stack))
           (push (nth i sorted-points) up-stack))
         (when (or (= i (1- (length sorted-points)))
                   (ccw p1 (nth i sorted-points)  p2))
           (loop
             while (and (>= (length down-stack) 2)
                        (not (ccw (second down-stack) (first down-stack) (nth i sorted-points))))
             do
                (pop down-stack))
           (push (nth i sorted-points) down-stack)))
    (when verbose
      (format *standard-output* "Down stack: ~s~%Up stack: ~s~%" down-stack up-stack)
      (finish-output *standard-output*))
    (setq return-value (concatenate 'list
                                    (subseq (reverse down-stack) 0 (1- (length down-stack)))
                                    up-stack))
    (when verbose
      (format *standard-output* "Convex hull: ~s~%" return-value)
      (finish-output *standard-output*))
    (make-path return-value)))


(defun main (points-list &rest parameters &key
                                            (iterations-limit nil)
                                            (verbose nil))
  "Main function."
  (declare (ignorable parameters
                      points-position-list
                      verbose))
  (check-type points-list list)
  (loop
    for point in points-list
    do
       (check-type point grid:foreign-array)
       (assert (equalp (grid:dimensions point) '(2))))
  (when iterations-limit
    (check-type iterations-limit (integer 1)))
  (convex-hull points-list
               :iterations-limit iterations-limit
               :verbose verbose))
