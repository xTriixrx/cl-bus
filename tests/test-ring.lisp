;;;; test-ring.lisp
;;;;
;;;; A unit test suite containing different tests for the ring-buffer class
;;;; implemented for the cl-bus library.
;;;;
;;;; v0.0.1

(defpackage :rb/tests
  (:use :cl
        :rb
        :rove))

(in-package :rb/tests)

(deftest test-ring-type
  (testing "Testing ring buffer is of type ring-buffer."
    (let ((rbuffer nil))
      (ok (not (typep rbuffer 'rb::ring-buffer))
          "Ensure initially not a ring-buffer type.")
      (setf rbuffer (rb::make-rb 1))
      (ok (typep rbuffer 'rb::ring-buffer)
          "Ensure after intiialization is now ring-buffer type."))))

(deftest test-ring-empty
  (testing "Testing ring buffer rb-empty-p predicate works as expected."
    (let ((ring-buffer (rb::make-rb 2)))
      (ok (rb::rb-empty-p ring-buffer)
          "Ensure ring buffer is empty.")
      ;; Write data to buffer to insert
      (rb::rb-write ring-buffer 1)
      (ok (not (rb::rb-empty-p ring-buffer))
          "After write ensure buffer is no longer empty.")
      ;; Read data from buffer to remove
      (rb::rb-read ring-buffer)
      (ok (rb::rb-empty-p ring-buffer)
          "After read ensure buffer is empty again."))))

(deftest test-ring-full
  (testing "Testing ring buffer rb-full-p predicate works as expected."
    (let ((ring-buffer (rb::make-rb 2)))
      (ok (not (rb::rb-full-p ring-buffer))
          "Ensure ring buffer is not full.")
      (rb::rb-write ring-buffer 1)
      (rb::rb-write ring-buffer 2)
      (ok (rb::rb-full-p ring-buffer)
          "Ensure ring buffer is full after writing data.")
      (rb::rb-read ring-buffer)
      (ok (not (rb::rb-full-p ring-buffer))
          "Ensure ring buffer is no longer full after a read."))))

(deftest test-ring-reset
  (testing "Testing ring buffer reset empties out entire buffer."
    (let ((ring-buffer (rb::make-rb 2)))
      ;; Write data into buffer
      (rb::rb-write ring-buffer 1)
      (rb::rb-write ring-buffer 2)
      (ok (rb::rb-full-p ring-buffer) "Ensure ring buffer is full.")
      (rb::rb-reset ring-buffer)
      (ok (rb::rb-empty-p ring-buffer) "Ensure ring buffer is now empty after reset."))))

(deftest test-ring-read-write
  (testing "Testing ring buffer read and writes are in expected order."
    (let ((read-val nil)
          (val "Hello World")
          (ring-buffer (rb::make-rb 3)))
      ;; Write different data into ring buffer
      (ok (rb::rb-write ring-buffer (format nil val))
          "Ensure write to buffer is successful.")
      (ok (rb::rb-write ring-buffer 1)
          "Ensure write to buffer is successful.")
      (ok (rb::rb-write ring-buffer 'A)
          "Ensure write to buffer is successful.")
      (ok (not (rb::rb-write ring-buffer 1))
          "Ensure write to full buffer is unsuccessful.")
      ;; Read value and ensure it matches what was expected
      (setf read-val (rb::rb-read ring-buffer))
      (ok (equal read-val val) "Ensure read value matches written value.")
      (setf read-val (rb::rb-read ring-buffer))
      (ok (eql read-val 1) "Ensure read value matches written value.")
      (setf read-val (rb::rb-read ring-buffer))
      (ok (eql read-val 'A) "Ensure read value matches written value.")
      (ok (rb::rb-empty-p ring-buffer) "Ensure ring buffer is now empty."))))
