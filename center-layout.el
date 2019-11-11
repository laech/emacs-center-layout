;;; center-layout.el --- Centers windows.

;;; Commentary:

;;; Code:

(defgroup center-layout nil
  "Center windows."
  :group 'emacs)

(defcustom center-layout-columns 80
  "Width of centered content."
  :group 'center-layout
  :type 'number)

(defcustom center-layout-apply-right-margin nil
  "If t right margin will be applied also.
If nil no right margin will be applied, allowing longer lines to
flow to the right exceeding `center-layout-columns'."
  :group 'center-layout
  :type 'boolean)

(defun center-layout--window-body-preferred-pixels (window)
  "Return preferred body width for WINDOW.
Usually `center-layout-columns', but for dedicated
windows (`window-dedicated-p') like which-key, lv (hydra),
neotree, treemacs etc, return their content width if it is
greater, as typically they are used as dedicate bottom/side
windows and have their own layout, limiting their width will
cause undesired line wrapping/truncation."
  (let ((pixels (* center-layout-columns (window-font-width window))))
    (if (window-dedicated-p window)
        (max
         pixels
         (car (window-text-pixel-size
               window
               nil
               nil
               (- (window-pixel-width window)
                  (window-scroll-bar-width window)
                  (window-right-divider-width window)
                  (frame-fringe-width (window-frame window))))))
      pixels)))

(defun center-layout--free-columns (window extra-free-pixels)
  "Computes available columns for layout use for WINDOW.
Adds EXTRA-FREE-PIXELS into the calculation."
  (max
   (ceiling
    (- (+ (window-pixel-width window) extra-free-pixels)
       (window-scroll-bar-width window)
       (window-right-divider-width window)
       (frame-fringe-width (window-frame window))
       (center-layout--window-body-preferred-pixels window))
    (frame-char-width (window-frame window)))
   0))

(defun center-layout--frame-left-child (window)
  "Return left most window container (or nil) of frame owning WINDOW."
  (window-left-child (frame-root-window window)))

(defun center-layout--window-eq-or-is-descendant (window ancestor)
  "Return t if WINDOW is eq or is a descendant of ANCESTOR."
  (cond
   ((eq window ancestor) t)
   ((not window) nil)
   ((center-layout--window-eq-or-is-descendant
     (window-parent window)
     ancestor))))

(defun center-layout--window-width-percentage (window)
  "Return WINDOW's width as a percentage out of the frame's width."
  (/ (float (window-total-width window))
     (frame-width (window-frame window))))

(defun center-layout--compute-margins (window)
  "Compute (LEFT-MARGIN . RIGHT_MARGIN) for WINDOW."
  (let*
      ((left-side-window
        (let ((left (center-layout--frame-left-child window)))
          (if (or (center-layout--window-eq-or-is-descendant window left)
                  (< (center-layout--window-width-percentage window) 0.4))
              nil
            left)))

       (left-side-window-pixels
        (if left-side-window (window-pixel-width left-side-window) 0))

       (left-side-window-columns
        (if left-side-window (window-total-width left-side-window 'ceiling) 0))

       (margins (center-layout--free-columns window left-side-window-pixels))
       (margin-left (- (ceiling margins 2) left-side-window-columns))
       (margin-right
        (if center-layout-apply-right-margin
            (- margins left-side-window-pixels margin-left)
          0)))

    `(,(max 0 margin-left) .
      ,(max 0 margin-right))))

(defun center-layout--update-window (window)
  "Update margins for WINDOW."
  (let ((margins (center-layout--compute-margins window)))
    (set-window-margins window (car margins) (cdr margins))))

(defun center-layout--update-frame (&optional frame)
  "Update margins for FRAME, if nil, update selected frames."
  (walk-windows
   (lambda (window) (center-layout--update-window window))
   t
   (or frame (selected-frame))))

(defun center-layout--advice (&rest ignored)
  "Advice for `advice-add' to update layout.
Arguments from the adviced functions are IGNORED."
  (center-layout--update-frame))

(defun center-layout--window-margins-nil (&rest args)
  "Always return '(nil) - no margins, ARGS are ignored."
  (list nil))

(defun center-layout--window-min-size-around (func &rest args)
  "An around advice for `window-min-size' (FUNC).
ARGS will be passed to the function.

`window-min-size' caculates the width by including the horizontal
margins, and since `center-layout-mode' always dynamically sets
the margins to fill available space (especially when
`center-layout-apply-right-margin' is on), this causes Emacs to
think that there is no available horizontal space anymore so
spliting windows will always occur below, never to the side,
which is undesirable, especially when trying to use side windows
like neotree.

This advice will remove the horizontal margins from the result
which is correct behaviour since margins will be dynamically
increased/decreased to fill available space therefore they don't
have a fixed size, giving desired split window behavior."

  (advice-add 'window-margins :around 'center-layout--window-margins-nil)
  (let ((result (apply func args)))
    (advice-remove 'window-margins 'center-layout--window-margins-nil)
    result))

(define-minor-mode center-layout-mode
  "Toggle center layout mode."
  :global t
  (cond
   (center-layout-mode
    (add-hook 'window-size-change-functions 'center-layout--update-frame)
    (add-hook 'window-configuration-change-hook 'center-layout--update-frame)
    (add-hook 'text-scale-mode-hook 'center-layout--update-frame)
    (add-hook 'scroll-bar-mode-hook 'center-layout--update-frame)
    (advice-add 'set-fringe-mode :after 'center-layout--advice)
    (advice-add 'window-min-size :around 'center-layout--window-min-size-around)
    (center-layout--update-frame))

   ((remove-hook 'window-size-change-functions 'center-layout--update-frame)
    (remove-hook 'window-configuration-change-hook 'center-layout--update-frame)
    (remove-hook 'text-scale-mode-hook 'center-layout--update-frame)
    (remove-hook 'scroll-bar-mode-hook 'center-layout--update-frame)
    (advice-remove 'set-fringe-mode 'center-layout--advice)
    (advice-remove 'window-min-size 'center-layout--window-min-size-around)
    (walk-windows (lambda (window) (set-window-margins window 0 0)) t))))

(provide 'center-layout)

;;; center-layout.el ends here
