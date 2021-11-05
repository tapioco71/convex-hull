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

(defun sort-points (x-array y-array &rest parameters &key
                                                       (points-position-list nil points-position-list-p)
                                                       (verbose nil))
  "Sorts point in respect the angular position of point."
  (declare (ignorable parameters
                      points-position-list
                      verbose))
  (check-type x-array grid:foreign-array)
  (assert (= (length (grid:dimensions x-array)) 1))
  (check-type y-array grid:foreign-array)
  (assert (= (length (grid:dimensions y-array)) 1))
  (assert (equalp (grid:dimensions x-array)
                  (grid:dimensions y-array)))
  (check-type points-position-list list)
  (assert (<= (length points-position-list)
              (grid:dim0 x-array)))
  (let ((points-list nil)
        (sorted-points-list nil))
    (unless points-position-list
      (setq points-position-list (loop
                                   for i from 1 upto (grid:dim0 x-array)
                                   collect i)))
    (setq points-list (loop
                        for p in points-position-list
                        collect (list :position p
                                      :coordinates (grid:make-foreign-array 'double-float
                                                                            :dimensions 2
                                                                            :initial-contents (list (grid:gref x-array (1- p))
                                                                                                    (grid:gref y-array (1- p)))))))
    (setq sorted-points-list (sort points-list
                                   #'(lambda (a b)
                                       (or (< (grid:gref (getf a :coordinates) 0) (grid:gref (getf b :coordinates) 0))
                                           (and (= (grid:gref (getf a :coordinates) 0) (grid:gref (getf b :coordinates) 0))
                                                (< (grid:gref (getf a :coordinates) 1) (grid:gref (getf b :coordinates) 1)))))))
    (when verbose
      (format *standard-output* "Sorted points set: ~s~%" sorted-points-list))
    sorted-points-list))

(defun convex-hull (x-array y-array &rest parameters &key
                                                       (points-position-list nil points-position-list-p)
                                                       (verbose nil))
  "Compute the convex hull for points set. Result in a stack."
  (check-type x-array grid:foreign-array)
  (assert (= (length (grid:dimensions x-array)) 1))
  (check-type y-array grid:foreign-array)
  (assert (= (length (grid:dimensions y-array)) 1))
  (assert (equalp (grid:dimensions x-array)
                  (grid:dimensions y-array)))
  (when points-position-list-p
    (check-type points-position-list list)
    (assert (and (>= (length points-position-list) 3)
                 (<= (length points-position-list) (grid:dim0 x-array)))))
  (let ((sorted-points nil)
        (p1 nil)
        (p2 nil)
        (up-stack '())
        (down-stack '())
        (points nil)
        (return-value nil))
    (unless points-position-list
      (setq points-position-list (loop
                          for i from 1 upto (grid:dim0 x-array)
                          collect i)))
    (setq sorted-points (sort-points x-array
                                     y-array
                                     :points-position-list points-position-list
                                     :verbose verbose))
    (setq p1 (first sorted-points)
          p2 (first (last sorted-points)))
    (when verbose
      (format *standard-output* "Leftmost point: ~a~%Rightmost point: ~a~%" p1 p2))
    (push p1 up-stack)
    (push p1 down-stack)
    (loop
      for i from 1 below (length sorted-points)
      do
         (when (or (= i (1- (length sorted-points)))
                   (cw (getf p1 :coordinates)
                       (getf (nth i sorted-points) :coordinates)
                       (getf p2 :coordinates)))
           (loop
             while (and (>= (length up-stack) 2)
                        (not (cw (getf (second up-stack) :coordinates)
                                 (getf (first up-stack) :coordinates)
                                 (getf (nth i sorted-points) :coordinates))))
             do
                (pop up-stack))
           (push (nth i sorted-points) up-stack))
         (when (or (= i (1- (length sorted-points)))
                   (ccw (getf p1 :coordinates)
                        (getf (nth i sorted-points) :coordinates)
                        (getf p2 :coordinates)))
           (loop
             while (and (>= (length down-stack) 2)
                        (not (ccw (getf (second down-stack) :coordinates)
                                  (getf (first down-stack) :coordinates)
                                  (getf (nth i sorted-points) :coordinates))))
             do
                (pop down-stack))
           (push (nth i sorted-points) down-stack)))
    (when verbose
      (format *standard-output* "Down stack: ~s~%Up stack: ~s~%" down-stack up-stack))
    (setq return-value (concatenate 'list
                                    (subseq (reverse down-stack) 0 (1- (length down-stack)))
                                    up-stack))
    (when verbose
      (format *standard-output* "Convex hull: ~s~%" return-value))
    return-value))


(defun main (x-array y-array &rest parameters &key
                                                (points-position-list nil points-position-list-p)
                                                (verbose nil))
  "Main function."
  (declare (ignorable parameters
                      points-position-list
                      verbose))
  (check-type x-array (array * (*)))
  (assert (= 1 (length (array-dimensions x-array))))
  (check-type y-array (array * (*)))
  (assert (= 1 (length (array-dimensions y-array))))
  (assert (equalp (array-dimensions x-array)
                  (array-dimensions y-array)))
  (when points-position-list-p
    (check-type points-position-list list)
    (assert (and (>= (length points-position-list) 3)
                 (<= (length points-position-list) (grid:dim0 x-array)))))
  (unless points-position-list
    (setq points-position-list (loop
                                  for i from 1 upto (grid:dim0 x-array)
                                  collect i)))
  (make-path (convex-hull (grid:copy-to x-array 'grid:foreign-array 'double-float)
                          (grid:copy-to y-array 'grid:foreign-array 'double-float)
                          :points-position-list points-position-list
                          :verbose verbose)))


