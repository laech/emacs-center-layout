;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-

(defmacro test (frame-width layout-columns apply-right-margin &rest body)
  `(save-window-excursion
     (let ((buffer (generate-new-buffer "*temp*")))
       (unwind-protect
           (progn
             (switch-to-buffer buffer)
             (set-frame-width (selected-frame) ,frame-width)
             (setq-default center-layout-columns ,layout-columns)
             (setq-default center-layout-apply-right-margin ,apply-right-margin)
             (center-layout-mode)
             ,@body)
         (kill-buffer buffer)
         (setq-default center-layout-columns 80)
         (setq-default center-layout-apply-right-margin nil)))))

(ert-deftest should-apply-left-margin-only-when-configured-as-such ()
  (test 120 80 nil (should (equal '(20) (window-margins)))))

(ert-deftest should-apply-both-margins-when-configured-as-such ()
  (test 120 80 t (should (equal '(20 . 20) (window-margins)))))

(ert-deftest should-take-scrollbar-into-consideration ()
  (test
   120 80 t
   (set-window-scroll-bars (selected-window) (* 2 (frame-char-width)) 'right)
   (scroll-bar-mode)
   (should (equal '(19 . 19) (window-margins)))))

(ert-deftest should-maintain-margins-on-split-window-horizontally ()
  (test
   120 40 t
   (let ((window-1 (selected-window))
         (window-2 (split-window-horizontally -50)))
     (should (equal '(15 . 15) (window-margins window-1)))
     (should (equal '(5 . 5) (window-margins window-2))))))

(ert-deftest should-maintain-margins-on-split-window-vertically ()
  (test
   120 80 t
   (let ((window-1 (selected-window))
         (window-2 (split-window-vertically)))
     (should (equal '(20 . 20) (window-margins window-1)))
     (should (equal '(20 . 20) (window-margins window-2))))))

(ert-deftest should-adjust-margins-on-window-resize ()
  (test
   120 40 t
   (let ((window-1 (selected-window))
         (window-2 (split-window-horizontally)))

     (window-resize window-1 2 t)
     (should (equal '(11 . 11) (window-margins window-1)))
     (should (equal '(9 . 9) (window-margins window-2)))

     (window-resize window-1 2 t)
     (should (equal '(12 . 12) (window-margins window-1)))
     (should (equal '(8 . 8) (window-margins window-2))))))

(ert-deftest should-maintain-buffer-local-customizations ()
  (test
   120 80 t
   (let ((window-1 (selected-window))
         (window-2 (split-window-vertically)))
     (with-selected-window window-2
       (let ((buffer (generate-new-buffer "*temp*")))
         (unwind-protect
             (progn
               (switch-to-buffer buffer)
               (setq center-layout-columns 60)
               (setq center-layout-apply-right-margin nil)
               (center-layout-mode 0)
               (center-layout-mode)
               (should (equal '(20 . 20) (window-margins window-1)))
               (should (equal '(30) (window-margins window-2))))
           (kill-buffer buffer)))))))

(ert-deftest should-reset-margins-when-center-layout-mode-is-turned-off ()
  (test
   120 80 nil
   (should (> (car (window-margins)) 0))
   (center-layout-mode 0)
   (should (equal nil (car (window-margins))))))

(ert-deftest should-not-restric-dedicated-windows-to-columns ()
  (test
   10 6 t
   (set-window-dedicated-p (selected-window) t)
   (insert "12345678")
   (center-layout-mode 0)
   (center-layout-mode)
   (should (equal '(1 . 1) (window-margins)))))

(provide 'center-layout-test)
