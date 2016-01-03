(require 'json)

(load-file "parinfer-mode.el")

(defvar parinfer-test-successful-count 0)
(defvar parinfer-test-failed-count 0)

(defun parinfer-test-load-json-file (path)
  (append
   (let ((json-object-type 'hash-table))
     (json-read-from-string
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))))
   nil))

(defconst parinfer-test-indent-mode-cases
  (parinfer-test-load-json-file "parinfer-test-indent-mode-cases.json"))

(defconst parinfer-test-paren-mode-cases
  (parinfer-test-load-json-file "parinfer-test-paren-mode-cases.json"))

(defun parinfer-test-run-tests (fn cases)
  (dolist (case cases)
    (let* ((in (string-join (gethash "lines" (gethash "in" case)) "\n"))
           (out (string-join (gethash "lines" (gethash "out" case)) "\n"))
           (cursor (gethash "cursor" (gethash "in" case)))
           (options (if cursor
                        (parinfer-make-hash
                         "cursor-x" (gethash "cursorX" cursor)
                         "cursor-line" (gethash "cursorLine" cursor))))
           (result (gethash "text" (funcall fn in options))))
      (if (string= out result)
          (setf parinfer-test-successful-count
                (+ parinfer-test-successful-count 1))
        (progn
          (setf parinfer-test-failed-count
                (+ parinfer-test-failed-count 1))
         (message "Test failed\nIn: %s\nOptions: %s\nExpected: %s\nResult: %s\n"
                 in options out result))))))

(message "Begin running tests")

(setf parinfer-test-successful-count 0)
(setf parinfer-test-failed-count 0)

(parinfer-test-run-tests
 'parinfer-indent-mode
 parinfer-test-indent-mode-cases)

(parinfer-test-run-tests
 'parinfer-paren-mode
 parinfer-test-paren-mode-cases)

(message "Done running tests\nSuccessful: %d, Failed: %d"
         parinfer-test-successful-count
         parinfer-test-failed-count)
