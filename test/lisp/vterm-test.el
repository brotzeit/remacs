;;; vterm-tests.el  -*- lexical-binding: t; -*-

(defun vterm-kill ()
  (let ((proc (get-buffer-process vterm-buffer-name)))
    (kill-process proc)
    (while (eq (process-status proc) 'run)
      (sit-for 0.5)))
  (sit-for 0.2))

(defun test-insertion ()
  (vterm)
  (sit-for 0.2)
  (dotimes (i 50)
    (vterm-send-string (format "echo %d;history -d $(history 1)" i))
    (vterm-send-key "<return>")
    (sit-for 0.2))
  (goto-char (point-min))
  (forward-line)
  (let ((c 0))
    (while (< c 49)
      (should (eq c (string-to-number (buffer-substring (point) (line-end-position)))))
      (forward-line 2)
      (setq c (+ c 1))))
  (vterm-kill))

;; scrollback

(defun test-scrollback ()
  (let ((vterm-max-scrollback 100))
    (vterm)
    (sit-for 0.2)
    (dotimes (i 50)
      (vterm-send-string (format "echo %d;history -d $(history 1)" i))
      (vterm-send-key "<return>")
      (sit-for 0.2))
    (let ((beg-string (buffer-substring-no-properties (point-min) (1+ (point-min)))))
      (should (string= beg-string "0")))
    (vterm-kill)))

(defun test-clear-scrollback ()
  (vterm)
  (sit-for 0.2)
  (let ((c 10))
    (while (> c 0)
      (vterm-send-key "<return>")
      (sit-for 0.2)
      (setq c (- c 1)))
    (vterm-send-string "clear;history -d $(history 1)")
    (sit-for 0.2)
    (vterm-send-key "<return>")
    (sit-for 0.2)
    (should (= (line-beginning-position) (point-min))))
  (vterm-kill))

;; cursor

(defun test-eol ()
  (interactive)
  (vterm)
  (sit-for 0.2)
  (should (vterminal-is-eol vterm--term))
      (dotimes (i 50)
      (vterm-send-string (format "echo %d;history -d $(history 1)" i))
      (vterm-send-key "<return>")
      (sit-for 0.2))
  (should (vterminal-is-eol vterm--term))
  (vterm-kill)

  (vterm)
  (sit-for 0.2)
  (vterm-send-string (format "echo 'test is eol'"))
  (should (vterminal-is-eol vterm--term))
  (vterm-send-key "<left>")
  (sit-for 0.2)
  (should (not (vterminal-is-eol vterm--term)))
  (sit-for 0.2))

(defun test-cursor-pos ()
  (interactive)
  (vterm)
  (sit-for 0.2)
  (let ((pos (vterminal-test-cursor-pos vterm--term)))
    (should (= (car pos) (vterminal-cursor-row vterm--term)))
    (should (= (cdr pos) (vterminal-cursor-col vterm--term))))
  (vterm-send-key "<return>")
  (sit-for 0.2)
  (let ((pos (vterminal-test-cursor-pos vterm--term)))
    (should (= (car pos) (vterminal-cursor-row vterm--term)))
    (should (= (cdr pos) (vterminal-cursor-col vterm--term))))
  (vterm-send-string (format "echo 'test cursorpos'"))
  (vterm-send-key "<left>")
  (sit-for 0.2)
  (let ((pos (vterminal-test-cursor-pos vterm--term)))
    (should (= (car pos) (vterminal-cursor-row vterm--term)))
    (should (= (cdr pos) (vterminal-cursor-col vterm--term))))
  (vterm-kill))

(defun vterm-run-tests ()
      (test-insertion)
      (test-scrollback)
      (test-clear-scrollback)
      ;; (test-count-lines)
      (test-eol)
      (test-cursor-pos)
    )




(defun test-fetch-cell ()
  (interactive)
  
  )



(defun test-line-contents ()
  (vterm)
  (sit-for 0.2)
  (dotimes (i 1)
    (vterm-send-string (format "echo %d;history -d $(history 1)" i))
    (vterm-send-key "<return>")
    (sit-for 0.2))
  (print (vterminal-line-contents vterm--term 0 0 30))
  
  
  ;; (vterm-kill)
  )



(defun test-line-contents ()
  (interactive)
  ;; (vterm)
  ;; (sit-for 0.2)
  ;; (dotimes (i 1)
  ;;   (vterm-send-string (format "echo %d;history -d $(history 1)" i))
  ;;   (vterm-send-key "<return>")
  ;;   (sit-for 0.2))
  (print (vterminal-line-contents vterm--term 1 1 (vterminal-width vterm--term)))
  
  
  ;; (vterm-kill)
  )


