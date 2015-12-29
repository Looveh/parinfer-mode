;;; Elisp specific

(defun parinfer-make-hash (&rest args)
  (let ((ht (make-hash-table :test 'equal)))
    (while (> (length args) 0)
      (puthash (pop args) (pop args) ht))
    ht))

;;; Constants

(defconst parinfer-backslash "\\")
(defconst parinfer-comma ",")
(defconst parinfer-double-quoute "\"")
(defconst parinfer-newline "\n")
(defconst parinfer-semicolon ";")
(defconst parinfer-tab "\t")

(defconst parinfer-parens
  (parinfer-make-hash "{" "}"
                      "}" "{"
                      "[" "]"
                      "]" "["
                      "(" ")"
                      ")" "("))

;;; Result structure

(defun parinfer-get-initial-result ()
  (parinfer-make-hash "lines" '()
                      "line-no" -1
                      "ch" ""
                      "x" 0
                      "stack" '()
                      "backup" '()
                      "insert" (parinfer-make-hash "line-no" nil
                                                   "x" nil)
                      "paren-trail" (parinfer-make-hash "start" nil
                                                        "end" nil)
                      "cursor-x" nil
                      "cursor-line" nil
                      "cursor-dx" nil
                      "quote-danger" nil
                      "track-indent" nil
                      "cursor-in-comment" nil
                      "quit" nil
                      "process" nil
                      "success" nil
                      "max-indent" nil
                      "indent-delta" 0))


(defun parinfer-insert-string (orig idx insert)
  (concat (substring orig 0 idx)
          insert
          (substring orig idx)))

(defun parinfer-replace-string-range (orig start end replace)
  (let ((end (if (<= end (length orig))
                 end
               (length orig))))
    (concat (substring orig 0 start)
            replace
            (substring orig end))))

(defun parinfer-remove-string-range (orig start end)
  (concat (substring orig 0 start)
          (substring orig end)))

;;; Reader operations

(defun parinfer-is-open-paren (c)
  (or (string= c "{")
      (string= c "(")
      (string= c "[")))

(defun parinfer-is-close-paren (c)
  (or (string= c "}")
      (string= c ")")
      (string= c "]")))

(defun parinfer-is-whitespace (c)
  (or (string= c " ")
      (string= c parinfer-tab)
      (string= c parinfer-newline)))

;;; Lisp reader: Stack states

(defun parinfer-peek (stack i)
  (let ((idx (- (length stack) i)))
    (if (< idx 0)
        nil
      (nth idx stack))))

(defun parinfer-get-prev-ch (stack i)
  (let ((e (parinfer-peek stack i)))
    (if e (gethash "ch" e) nil)))

(defun parinfer-is-escaping (stack)
  (string= (parinfer-get-prev-ch stack 1) parinfer-backslash))

(defun parinfer-prev-non-esc-ch (stack)
  (parinfer-get-prev-ch stack (if (parinfer-is-escaping stack) 2 1)))

(defun parinfer-is-in-str (stack)
  (string= (parinfer-prev-non-esc-ch stack) parinfer-double-quoute))

(defun parinfer-is-in-comment (stack)
  (string= (parinfer-prev-non-esc-ch stack) parinfer-semicolon))

(defun parinfer-is-in-code (stack)
  (and (not (parinfer-is-in-str stack))
       (not (parinfer-is-in-comment stack))))

(defun parinfer-is-valid-closer (stack ch)
  (string= (parinfer-get-prev-ch stack 1)
           (gethash "ch" parinfer-parens)))

;;; Lisp reader: Stack operations

(defun parinfer-push-open (result)
  (let ((stack (gethash "stack" result)))
    (if (parinfer-is-escaping stack)
        (pop stack)
      (if (parinfer-is-in-code stack)
          (let ((to-push (parinfer-make-hash
                          "x" (gethash "x" result)
                          "ch" (gethash "ch" result)
                          "indent-delta" (gethash "indent-delta" result))))
            (push to-push stack))))))


(defun parinfer-push-close (result)
  (let ((stack (gethash "stack" result))
        (backup (gethash "backup" result))
        (ch (gethash "ch" result)))
    (if (parinfer-is-escaping stack)
        (pop stack)
      (if (parinfer-is-in-code stack)
          (if (parinfer-is-valid-closer stack ch)
              (let ((opener (pop stack)))
                (progn
                  (puthash "max-indent" (gethash "x" opener) result)
                  (push opener backup)))
            (puthash "ch" "" result))))))

(defun parinfer-push-tab (result)
  (if (not (parinfer-is-in-str (gethash "stack" result)))
      (puthash "ch" "  " result)))

(defun parinfer-push-semicolon (result)
  (let ((stack (gethash "stack" result)))
    (if (parinfer-is-escaping stack)
        (pop stack)
      (if (parinfer-is-in-code stack)
          (push (parinfer-make-hash "x" (gethash "x" result)
                                    "ch" (gethash "ch" result))
                stack)))))

(defun parinfer-push-newline (result)
  (let ((stack (gethash "stack" result)))
    (if (parinfer-is-escaping stack) (pop stack))
    (if (parinfer-is-in-comment stack) (pop stack))
    (puthash "ch" "" result)))

(defun parinfer-push-escape (result)
  (let ((stack (gethash "stack" result)))
    (if (parinfer-is-escaping stack)
        (pop stack)
      (push (parinfer-make-hash "x" (gethash "x" result)
                                "ch" (gethash "ch" result))
            stack))))

(defun parinfer-push-quote (result)
  (let ((stack (gethash "stack" result)))
    (cond
     ((parinfer-is-escaping stack) (pop stack))
     ((parinfer-is-in-str stack) (pop stack))
     ((parinfer-is-in-comment stack)
      (puthash "quote-danger"
               (not (gethash "quote-danger" result))
               result))
     (t (push (parinfer-make-hash "x" (gethash "x" result)
                                  "ch" (gethash "ch" result))
              stack)))))

(defun parinfer-push-default (result)
  (let ((stack (gethash "stack" result)))
    (if (parinfer-is-escaping stack)
        (pop stack))))

(defun parinfer-push-char (result)
  (let ((ch (gethash "ch" result)))
    (cond
     ((parinfer-is-open-paren ch) (parinfer-push-open result))
     ((parinfer-is-close-paren ch) (parinfer-push-close result))
     ((string= ch parinfer-tab) (parinfer-push-tab result))
     ((string= ch parinfer-semicolon) (parinfer-push-semicolon result))
     ((string= ch parinfer-newline) (parinfer-push-newline result))
     ((string= ch parinfer-backslash) (parinfer-push-escape result))
     ((string= ch parinfer-double-quoute) (parinfer-push-quote result))
     (t (parinfer-push-default result)))))

;;; Indent mode operations

(defun parinfer-close-parens (result indent-x)
  (let ((indent-x (or indent-x 0))
        (stack (gethash "stack" result))
        (parens '()))
    (catch 'break
      (while (> (length stack) 0)
        (let ((opener (parinfer-peek stack 1)))
          (if (and opener (>= (gethash "x" opener) indent-x))
              (progn
                (pop stack)
                (add-to-list 'parens (gethash (gethash "ch" opener) parinfer-parens)))
            (throw 'break t)))))
    (let ((new-string (parinfer-insert-string
                       (nth (gethash "line-no" (gethash "insert" result))
                            (gethash "lines" result))
                       (gethash "x" (gethash "insert" result))
                       (apply 'concat (reverse parens)))))
      (setf (nth (gethash "line-no" (gethash "insert" result))
                 (gethash "lines" result))
            new-string))))

(defun parinfer-update-paren-trail (result)
  (let* ((ch (gethash "ch" result))
         (stack (gethash "stack" result))
         (close-paren (parinfer-is-close-paren ch))
         (escaping (parinfer-is-escaping stack))
         (in-code (parinfer-is-in-code stack))
         (should-pass (or (string= ch parinfer-semicolon)
                          (string= ch parinfer-comma)
                          (parinfer-is-whitespace ch)
                          close-paren))
         (should-reset (and in-code
                            (or escaping
                                (not should-pass)))))
    (puthash "cursor-in-comment"
             (or (gethash "cursor-in-comment" result)
                 (let ((cursor-line (gethash "cursor-line" result))
                       (line-no (gethash "line-no" result))
                       (x (gethash "x" result))
                       (cursor-x (gethash "cursor-x" result)))
                   (and (and cursor-line line-no (= cursor-line line-no))
                        (and x cursor-x (= x cursor-x))
                        (parinfer-is-in-comment stack))))
             result)
    (let ((should-update (and in-code
                              (not escaping)
                              close-paren
                              (parinfer-is-valid-closer stack ch))))
      (if should-reset
          (progn
            (puthash "backup" '() result)
            (puthash "paren-trail" (parinfer-make-hash "start" nil "end" nil) result)
            (puthash "max-indent" nil result))
        (if should-update
            (progn
              (if (not (gethash "start" (gethash "paren-trail" result)))
                  (setf (gethash "start" (gethash "paren-trail" result))
                        (gethash "x" result)))
              (setf (gethash "end" (gethash "paren-trail" result))
                    (+ (gethash "x" result) 1))))))))

(defun parinfer-block-paren-trail (result)
  (let* ((start (gethash "start" (gethash "paren-trail" result)))
         (end (gethash "end" (gethash "paren-trail" result)))
         (is-cursor-blocking (and (= (gethash "line-no" result) (gethash "cursor-line" result))
                                  (not start)
                                  (> (gethash "cursor-x" result) start)
                                  (not (gethash "cursor-in-comment" result)))))
    (if (and start is-cursor-blocking)
        (setf start (max start (gethash "cursor-x" result))))
    (if (and end is-cursor-blocking)
        (setf start (max start (gethash "cursor-x" result))))
    (if (= start end)
        (progn
          (setf start nil)
          (setf end nil)))
    (setf (gethash "start" (gethash "paren-trail" result)) start)
    (setf (gethash "end" (gethash "paren-trail" result)) end)))

(defun parinfer-remove-paren-trail (result)
  (let ((start (gethash "start" (gethash "paren-trail" result)))
        (end (gethash "end" (gethash "paren-trail" result))))
    (if (and start end)
        (let* ((stack (gethash "stack" result))
               (backup (gethash "backup" result))
               (line (nth (gethash "line-no" result)
                          (gethash "lines" result)))
               (remove-count
                (cl-count t (mapcar
                             (lambda (x) (parinfer-is-close-paren x))
                             line)))
               (ignore-count (- (length backup) remove-count)))
          (while (not (= ignore-count (length backup)))
            (push (pop backup) stack))
          (setf (nth (gethash "line-no" result) (gethash "lines" result))
                (parinfer-remove-string-range line start end))
          (if (= (gethash "line-no" (gethash "insert" result))
                 (gethash "line-no" result))
              (setf (gethash "x" (gethash "insert" result))
                    (min (gethash "x" (gethash "insert" result)) start)))))))

(defun parinfer-update-insertion-pt (result)
  (let* ((line (nth (gethash "line-no" result) (gethash "lines" result)))
         (prev-ch-idx (- (gethash "x" result) -1))
         (prev-ch (if (>= prev-ch-idx 0) (nth prev-ch-idx line) nil))
         (ch (gethash "ch" result))
         (should-insert
          (and (parinfer-is-in-code (gethash "stack" result))
               (not (string= ch ""))
               (or (not (parinfer-is-whitespace ch))
                   (string= prev-ch parinfer-backslash))
               (or (not (parinfer-is-close-paren ch))
                   (string= (gethash "line-no" result)
                            (gethash "cursor-line" result))))))
    (if should-insert
        (setf (gethash "insert" result)
              (parinfer-make-hash "line-no" (gethash "line-no" result)
                                  "x" (+ (gethash "x" result) 1))))))

(defun parinfer-process-indent-trigger (result)
  (parinfer-close-parens result (gethash "x" result))
  (setf (gethash "track-indent" result) nil))

(defun parinfer-process-indent (result)
  (let* ((stack (gethash "stack" result))
         (ch (gethash "ch" result))
         (check-indent (and (gethash "track-indent" result)
                            (parinfer-is-in-code stack)
                            (not (parinfer-is-whitespace ch))
                            (not (string= ch parinfer-semicolon))))
         (skip (and check-indent (parinfer-is-close-paren ch)))
         (at-indent (and check-indent (not skip)))
         (quit (and at-indent (gethash "quote-danger" result))))
    (setf (gethash "quit" result) quit)
    (setf (gethash "process" result) (and (not skip) (not quit)))
    (if (and at-indent (not quit))
        (parinfer-process-indent-trigger result))))

(defun parinfer-update-line (result orig-ch)
  (let ((ch (gethash "ch" result)))
    (if (not (string= orig-ch ch))
        (setf (nth (gethash "line-no" result)
                   (gethash "lines" result))
              (parinfer-replace-string-range
               (nth (gethash "line-no" result)
                    (gethash "lines" result))
               (gethash "x" result)
               (+ (gethash "x" result) (length orig-ch))
               ch)))))

(defun parinfer-process-char (result ch)
  (let ((orig-ch ch))
    (setf (gethash "ch" result) ch)
    (parinfer-process-indent result)
    (if (not (gethash "quit" result))
        (progn
          (if (gethash "process" result)
              (progn
                (parinfer-update-paren-trail result)
                (parinfer-push-char result)
                (parinfer-update-insertion-pt result))
            (setf (gethash "ch" result) ch))
          (parinfer-update-line result orig-ch)
          (setf (gethash "x" result)
                (+ (gethash "x" result)
                   (length (gethash "ch" result))))))))

(defun parinfer-process-line (result line)
  (let ((stack (gethash "stack" result)))
    (setf (gethash "line-no" result) (+ (gethash "line-no" result) 1))
    (setf (gethash "backup" result) '())
    (setf (gethash "cursor-in-comment" result) nil)
    (setf (gethash "paren-trail" result)
          (parinfer-make-hash "start" nil "end" nil))
    (setf (gethash "track-indent" result)
          (and (> (length stack) 0)
               (not (parinfer-is-in-str stack))))
    (push line (gethash "lines" result))
    (setf (gethash "x" result) 0)
    (let ((chars (concat line parinfer-newline))
          (i 0)
          (continue t))
      (while (and (< i (length chars)) continue)
        (parinfer-process-char result (substring chars i (+ i 1)))
        (if (gethash "quit" result)
            (setf continue nil))
        (setf i (+ i 1))))
    (if (not (gethash "quit" result))
        (progn
          (parinfer-block-paren-trail result)
          (parinfer-remove-paren-trail result)))))

(defun parinfer-finalize-result (result)
  (let ((stack (gethash "stack" result)))
    (setf (gethash "success" result)
          (and (not (parinfer-is-in-str stack))
               (not (gethash "quote-danger" result))))
    (if (and (gethash "success" result)
             (> (length stack) 0))
        (parinfer-close-parens result nil))))

(defun parinfer-process-text (text options)
  (let ((result (parinfer-get-initial-result)))
    (if options
        (progn
          (setf (gethash "cursor-x" result)
                (gethash "cursor-x" options))
          (setf (gethash "cursor-line" result)
                (gethash "cursor-line" options))))
    (let ((lines (split-string text parinfer-newline))
          (i 0)
          (continue t))
      (while (and (< i (length lines)) continue)
        (parinfer-process-line result (nth i lines))
        (if (gethash "quit" result)
            (setf continue nil))
        (setf i (+ i 1))))
    (parinfer-finalize-result result)
    result))

(defun parinfer-format-text (text options)
  (let* ((result (parinfer-process-text text options))
         (out-text (if (gethash "success" result)
                       (mapconcat 'identity
                                  (gethash "lines" result)
                                  parinfer-newline)
                     text)))
    (parinfer-make-hash "text" out-text
                        "success" (gethash "success" result))))

;;; Paren mode operations

(defun parinfer-append-paren-trail (result)
  (let* ((opener (pop (gethash "stack" result)))
         (close-ch (nth (gethash "ch" opener) parinfer-parens))
         (i (gethash "line-no" (gethash "insert" result)))
         (line (nth i (gethash "lines" result))))
    (setf (gethash "max-indent" result) (gethash "x" opener))
    (setf (nth i (gethash "lines" result))
          (parinfer-insert-string line
                                  (gethash "x" (gethash "insert" result))
                                  close-ch))
    (setf (gethash "x" (gethash "insert" result))
          (+ (gethash "x" (gethash "insert" result)) 1))))

(defun parinfer-min-indent (x result)
  (let ((opener (parinfer-peek (gethash "stack" result) 1)))
    (if opener (max (+ 1 (gethash "x" opener)) x) x)))

(defun parinfer-min-dedent (x result)
  (let ((max-indent (gethash "max-indent" result)))
    (if max-indent (min max-indent x) x)))

(defun parinfer-correct-indent (result)
  (let* ((opener (parinfer-peek (gethash "stack" result) 1))
         (delta (if (and opener (gethash "indent-delta" opener))
                    (gethash "indent-delta" opener)
                  0))
         (new-x-1 (+ (gethash "x" result) delta))
         (new-x-2 (parinfer-min-indent new-x-1 result))
         (new-x-3 (parinfer-min-indent new-x-2 result)))
    (if (not (= new-x-3 (gethash "x" result)))
        (let* ((indent-str (make-string new-x-3 (string-to-char " ")))
               (line (nth (gethash "line-no" result)
                          (gethash "lines" result)))
               (new-line (parinfer-replace-string-range
                          line
                          0
                          (gethash "x" result)
                          indent-str)) )
          (setf (nth (gethash "line-no" result)
                     (gethash "lines" result))
                new-line)
          (setf (gethash "x" result) new-x-3)))
    (setf (gethash "track-indent" result) nil)
    (setf (gethash "max-indent" result) nil)))

(defun parinfer-handle-cursor-delta (result)
  (let ((has-cursor-delta (and (= (gethash "cursor-line" result)
                                  (gethash "line-no" result))
                               (= (gethash "cursor-x" result)
                                  (gethash "x" result))
                               (gethash "cursor-x" result))))
    (if (and has-cursor-delta
             (number-or-marker-p (gethash "cursor-dx" result)))
        (setf (gethash "indent-delta" result)
              (+ (gethash "indent-delta" result)
                 (gethash "cursor-dx" result))))))

(defun parinfer-process-indent-paren (result)
  (let* ((ch (gethash "ch" result))
         (stack (gethash "stack" result))
         (close-paren (parinfer-is-close-paren ch))
         (check-indent (and (gethash "track-indent" result)
                            (parinfer-is-in-code stack)
                            (not (parinfer-is-whitespace ch))
                            (not (string= (gethash "ch" result)
                                          parinfer-semicolon))))
         (at-valid-closer (and check-indent
                               close-paren
                               (parinfer-is-valid-closer stack ch)))
         (is-cursor-holding (and (= (gethash "line-no" result)
                                    (gethash "cursor-line" result))
                                 (gethash "cursor-x" result)
                                 (<= (gethash "cursor-x" result)
                                     (gethash "x" result))))
         (should-move-closer (and at-valid-closer
                                  (not is-cursor-holding)))
         (skip (and check-indent close-paren (not is-cursor-holding)))
         (at-indent (and check-indent (not skip)))
         (quit (and at-indent (gethash "quote-danger" result))))
    (setf (gethash "quit" result) quit)
    (setf (gethash "process" result) (not skip))
    (if (not quit)
        (progn
          (if should-move-closer
              (parinfer-append-paren-trail result))
          (parinfer-handle-cursor-delta result)
          (if at-indent
              (parinfer-correct-indent result))))))

(defun parinfer-process-char-paren (result ch)
  (let ((orig-ch ch))
    (setf (gethash "ch" result) ch)
    (parinfer-process-indent-paren result)
    (if (not (gethash "quit" result))
        (progn
          (if (gethash "process" result)
              (progn
                (parinfer-update-paren-trail result)
                (parinfer-push-char result)
                (parinfer-update-insertion-pt result))
            (setf (gethash "ch" result) ""))
          (parinfer-update-line result orig-ch)
          (setf (gethash "x" result)
                (+ (gethash "x" result)
                   (length (gethash "ch" result))))))))

(defun parinfer-format-paren-trail (result)
  (let ((start (gethash "start" (gethash "paren-trail" result)))
        (end (gethash "end" (gethash "paren-trail" result))))
    (if (and start end)
        (let ((line (nth (gethash "line-no" result)
                         (gethash "lines" result)))
              (new-trail '())
              (space-count 0)
              (i start))
          (while (< i end)
            (if (parinfer-is-close-paren (nth i line))
                (setf new-trail (cons (nth i line) new-trail))
              (setf space-count (+ space-count 1)))
            (setf i (+ i 1)))
          (setf new-trail (apply 'concat (reverse new-trail)))
          (if (> space-count 0)
              (progn
                (setf (nth (gethash "line-no" result)
                           (gethash "lines" result))
                      (parinfer-replace-string-range line start end new-trail))
                (setf end (- end space-count))))
          (if (= (gethash "line-no" (gethash "insert" result))
                 (gethash "line-no" result))
              (setf (gethash "x" (gethash "insert" result)) end))))))

(defun parinfer-process-line-paren (result line)
  (setf (gethash "line-no" result) (+ (gethash "line-no" result) 1))
  (setf (gethash "backup" result) '())
  (setf (gethash "cursor-in-comment" result) nil)
  (setf (gethash "paren-trail" result)
        (parinfer-make-hash "start" nil "end" nil))
  (setf (gethash "track-indent" result)
        (not (parinfer-is-in-str (gethash "stack" result))))
  (push (gethash "lines" result) line)
  (setf (gethash "x" result) 0)
  (let ((chars (concat line parinfer-newline))
        (i 0)
        (continue t))
    (while (and (< i (length chars)) continue)
      (parinfer-process-char-paren result (substring chars i (+ i 1)))
      (if (gethash "quit" result)
          (setf continue nil)))))

(defun parinfer-finalize-result-paren (result)
  (setf (gethash "success" result)
        (and (= (length (gethash "stack" result)) 0)
             (not (gethash "quote-danger" result)))))

(defun parinfer-process-text-paren (text options)
  (let ((result (parinfer-get-initial-result)))
    (if options
        (progn
          (setf (gethash "cursor-x" result) (gethash "cursor-x" options))
          (setf (gethash "cursor-line" result) (gethash "cursor-line" options))
          (setf (gethash "cursor-dx" result) (gethash "cursor-dx" options))))
    (let ((lines (split-string text parinfer-newline))
          (i 0)
          (continue t))
      (while (and (< i (length lines)) continue)
        (parinfer-process-line result (nth i lines))
        (if (gethash "quit" result)
            (setf continue nil)))
      (parinfer-finalize-result-paren result)
      result)))

(defun parinfer-format-text-paren (text options)
  (let* ((result (parinfer-process-text-paren text options))
         (out-text (if (gethash "success" result)
                       (mapconcat 'identity
                                  (gethash "lines" result)
                                  parinfer-newline)
                     text)))
    (parinfer-make-hash "text" out-text
                        "success" (gethash "success" result))))

;;; Public API

(defun parinfer-indent-mode (text &optional options)
  (parinfer-format-text text options))

(defun parinfer-paren-mode (text &optional options)
  (parinfer-format-text-paren text options))

;;; Package definition

(provide 'parinfer-mode)
