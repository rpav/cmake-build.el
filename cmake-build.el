;;; -*- lexical-binding: t; -*-
;;; cmake-build.el --- Handle cmake build profiles and target/run configurations for projects

;; Copyright (C) 2019-2020  Ryan Pavlik

;; Author: Ryan Pavlik <rpavlik@gmail.com>
;; URL: https://github.com/rpav/cmake-build.el
;; Version: 1.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'tramp)

(defgroup cmake-build ()
  "Use CMake to build projects and run targets based on configurations"
  :group 'tools)

(defcustom cmake-build-local-options-file
  (expand-file-name "cmake-build-options.el" user-emacs-directory)
  "Path to file storing local cmake-build settings, such as options
passed to cmake, and the current config."
  :group 'cmake-build)

(defcustom cmake-build-run-window-autoswitch t
  "Automatically switch between Build and Run output buffers in the visible window."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-before-run t
  "Build automtically before running app, when using `cmake-build-run`."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-display-type 'split
  "How to display cmake-build output; 'split' will split the
window (using cmake-build window splitting options), 'frame' will
create a new frame.  In all cases, the buffers will be reused if
they are visible, regardless of current display type."
  :type 'symbol
  :group 'cmake-build
  :options '(split frame))

(defcustom cmake-build-raise-frame t
  "Whether to raise the frame of the build window on build. This
only applies if `cmake-build-display-type` is frame."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-override-compile-keymap t
  "Whether to use cmake-build-run-keymap for the compile window as well.
This more or less provides specific/consistent behavior for
quitting the frame or window."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-run-quit-frame-type 'lower
  "How to handle the run frame when quitting."
  :type 'symbol
  :group 'cmake-build
  :options '(lower delete))

(defcustom cmake-build-run-window-size 20
  "Size of window to split."
  :type 'integer
  :group 'cmake-build)

(defcustom cmake-build-split-threshold 40.0
  "Threshold (percentage) at which to *not* split the current window,
but instead use the other window.  That is, if `cmake-build-run-window-size`
is greater than this percentage of the current window, it will not be split."
  :type 'float
  :group 'cmake-build)

(defcustom cmake-build-never-split nil
  "Never split the window, instead always use the other window."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-switch-to-build nil
  "If non-NIL, switch to the build window when cmake-build-current is invoked.
Otherwise, leave the current window active"
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-external-term-command "xterm -e %s"
  "NOT CURRENTLY USED.  External terminal for build/run.  This is
used as a format string, where '%s' will inject the build
command, and should be appropriately escaped."
  :type 'string
  :group 'cmake-build)

(defcustom cmake-build-dir-name-function 'cmake-build-default-build-dir-function
  "Specify a function to customize the build directory name.  By
default, the name is in the form `build.<profile>`."
  :type 'function
  :group 'cmake-build)

;;; These are very temporary and likely very host-specific variables,
;;; not something we want to constantly modify in custom.el
(defvar cmake-build-profile 'clang-release
  "Build profile name to use for `cmake-build-current`.")

(defvar cmake-build-options ""
  "Additional build options passed to cmake.  For example, \"-j 7\" for parallel builds.")

(defvar cmake-build-run-config nil
  "Set name for cmake-build run, specifying the target run-config name.  Run configurations are a
path, command, and arguments for a particular run.")

(defvar cmake-build-project-root nil
  "Optionally, set this to the emacs-wide root of the current project.  Setting this to NIL will
use Projectile to determine the root on a buffer-local basis, instead.")

(defvar cmake-build-build-roots nil
  "This is an alist of build roots per-project, for out-of-source building.")

(defvar cmake-build-run-keymap (make-sparse-keymap))

(defun cmake-build-run-window-quit ()
  (interactive)
  (if (= 1 (length (window-list)))
      (case cmake-build-run-quit-frame-type
        (lower (lower-frame))
        (delete (delete-frame)))
    (delete-window)))

(defun cmake-build-kill-buffer-process (&optional buffer)
  (interactive)
  (let* ((buffer (get-buffer (or buffer (current-buffer))))
         (p (get-buffer-process buffer)))
    (when p
      (with-current-buffer buffer
        (case major-mode
          (shell-mode (kill-process p))
          (compilation-mode (kill-compilation)))))))

(defun cmake-build-kill-processes ()
  (interactive)
  (cmake-build-kill-buffer-process (cmake-build--build-buffer-name))
  (cmake-build-kill-buffer-process (cmake-build--run-buffer-name)))

(let ((map cmake-build-run-keymap))
  (define-key map (kbd "q") 'cmake-build-run-window-quit)
  (define-key map (kbd "C-c C-c") 'cmake-build-kill-buffer-process))

(cl-defmacro cmake-build--with-file ((filename &key readp writep) &body body)
  (declare (indent 1))
  `(with-temp-buffer
     (prog1
         ,(if readp
              `(when (file-exists-p ,filename)
                 (insert-file-contents ,filename)
                 ,@body)
            `(progn ,@body))
       (when ,writep
         (write-region 1 (point-max) ,filename)))))

(cl-defmacro cmake-build--with-options-file ((&key readp writep) &body body)
  (declare (indent 1))
  `(cmake-build--with-file (cmake-build-local-options-file :readp ,readp :writep ,writep) ,@body))

(defun cmake-build--project-root ()
  (or cmake-build-project-root
      (projectile-project-root)))

(defun cmake-build--maybe-remote-project-root ()
  "Return current project root path, suitable for remote invocations too."
  (let* ((project-root-raw (cmake-build--project-root))
         (project-root
          (file-name-as-directory
           (if (tramp-tramp-file-p project-root-raw)
               (let ((parsed-root (tramp-dissect-file-name project-root-raw)))
                 (tramp-file-name-localname parsed-root))
             project-root-raw))))
    (concat project-root (or (cmake-build--source-root) ""))))

(cl-defmacro cmake-build--save-project-root (nil &body body)
  (declare (indent 1))
  `(let ((cmake-build-project-root (cmake-build--project-root)))
     ,@body))

(defun cmake-build--read-options ()
  (cmake-build--with-options-file (:readp t)
    (let* ((form (read (buffer-string)))
           (build-profile (cadr (assoc :build-profile form)))
           (build-options (cadr (assoc :build-options form)))
           (build-run-config (cadr (assoc :build-run-config form)))
           (build-project-root (cadr (assoc :build-project-root form)))
           (build-roots (cadr (assoc :build-roots form))))
      (setq cmake-build-profile (or build-profile cmake-build-profile))
      (setq cmake-build-options (or build-options cmake-build-options))
      (setq cmake-build-run-config (or build-run-config cmake-build-run-config))
      (setq cmake-build-project-root (or build-project-root cmake-build-project-root))
      (setq cmake-build-build-roots (or build-roots cmake-build-build-roots)))))

(defun cmake-build--read-project-data ()
  (let ((project-data-path (concat (file-name-as-directory (cmake-build--project-root)) ".cmake-build.el")))
    (cmake-build--with-file (project-data-path :readp t)
      (read (buffer-string)))))

(defun cmake-build--write-options ()
  (cmake-build--with-options-file (:writep t)
    (print `((:build-profile ,cmake-build-profile)
             (:build-options ,cmake-build-options)
             (:build-run-config ,cmake-build-run-config)
             (:build-project-root ,cmake-build-project-root)
             (:build-roots ,cmake-build-build-roots))
           (current-buffer))))

(defun cmake-build-get-run-config-name ()
  (when (cmake-build--project-root)
    (cdr (assoc (intern (cmake-build--project-root)) cmake-build-run-config))))

(defun cmake-build--set-run-config (config)
  (when (cmake-build--project-root)
    (setf (alist-get (intern (cmake-build--project-root)) cmake-build-run-config)
          config)))

(defun cmake-build--set-build-root (path)
  (when (cmake-build--build-root)
    (setf (alist-get (intern (cmake-build--project-root)) cmake-build-build-roots)
          path)))

(defun cmake-build--validity ()
  (cond
   ((not (cmake-build--project-root)) :data-missing)
   ((not (file-directory-p (cmake-build--get-build-dir))) :build-dir-missing)
   ((null (cmake-build--get-project-data)) :data-missing)
   (t t)))

(defun cmake-build--validate (&optional tag)
  (not
   (case (cmake-build--validity)
     (:build-dir-missing
      (message "cmake-build %s: No build dir (%s)\nDo you need to initialize CMake?"
               (or tag "compile")
               (cmake-build--get-build-dir)))
     (:data-missing
      (message "cmake-build %s: Not a valid project; no .cmake-build.el data found (project root is %s)"
               (or tag "compile")
               (cmake-build--project-root)))
     (t nil))))

(defun cmake-build-project-name ()
  (let ((default-directory (cmake-build--project-root)))
    (projectile-project-name)))

(defun cmake-build--build-buffer-name (&optional name)
  (concat "*Build " (cmake-build-project-name) "/" (symbol-name cmake-build-profile) ": " (symbol-name (cmake-build-get-run-config-name)) "*"))

(defun cmake-build--run-buffer-name ()
  (concat "*Run " (cmake-build-project-name) "/" (symbol-name cmake-build-profile) ": " (symbol-name (cmake-build-get-run-config-name)) "*"))

(defun cmake-build--get-project-data ()
  (cmake-build--read-project-data))

(defun cmake-build--get-cmake-options ()
  (cadr (assoc 'cmake-build-cmake-options (cmake-build--get-project-data))))

(defun cmake-build--get-cmake-profiles ()
  (when (cmake-build--project-root)
    (cdr (assoc 'cmake-build-cmake-profiles
                (cmake-build--get-project-data)))))

(defun cmake-build--get-profile (&optional profile)
  (cdr (assoc (or profile cmake-build-profile)
              (cmake-build--get-cmake-profiles))))

(defun cmake-build--get-configs ()
  (when (cmake-build--project-root)
    (cdr (assoc 'cmake-build-run-configs
                (cmake-build--get-project-data)))))

(defun cmake-build--get-config (&optional config)
  (cdr (assoc (or config (cmake-build-get-run-config-name))
              (cmake-build--get-configs))))

(defun cmake-build--get-build-config (&optional config)
  (let ((config (cmake-build--get-config config)))
    (cdr (assoc :build config))))

(defun cmake-build--get-run-config (&optional config)
  (let ((config (cmake-build--get-config config)))
    (cdr (assoc :run config))))

(defun cmake-build--get-run-config-env (&optional config)
  (let ((config (cmake-build--get-config config)))
    (cdr (assoc :env config))))

(defun cmake-build--get-other-targets ()
  (cdr (assoc 'cmake-build-other-targets (cmake-build--get-project-data))))

(defun cmake-build--build-root ()
  (or (cdr (assoc (intern (cmake-build--project-root)) cmake-build-build-roots))
      (cmake-build--project-root)))

(defun cmake-build--source-root ()
  (cadr (assoc 'cmake-build-source-root (cmake-build--get-project-data))))

(defun cmake-build-default-build-dir-function (project-root profile)
  (concat "build." profile))

(defun cmake-build--get-build-dir (&optional subdir)
  (concat (cmake-build--build-root)
          (funcall cmake-build-dir-name-function
                   (cmake-build--project-root)
                   (symbol-name cmake-build-profile))
          "/" (or subdir "")))

(defun cmake-build--check-build-dir ()
  (let ((path (cmake-build--get-build-dir)))
    (if (file-directory-p path)
        t
      (message "Build directory doesn't exist: %s\nDo you need to initialize CMake?" path)
      nil)))

(defun cmake-build--get-run-command (config)
  (concat (cadr config) " " (caddr config)))

(defun cmake-build--switch-to-buffer (buffer buffer-window other-window)
  (if buffer-window t
    (when (and cmake-build-run-window-autoswitch
               other-window)
      (set-window-dedicated-p other-window nil)
      (set-window-buffer other-window buffer)
      (set-window-dedicated-p other-window t)
      t)))

(defun cmake-build--split-to-buffer (name other-name)
  (let* ((window-point-insertion-type t)
         ;; Make sure we have a buffer created regardless
         (buffer (get-buffer-create name))
         (current-buffer-window (get-buffer-window))
         (new-buffer-window (get-buffer-window name))
         (other-buffer-window (and other-name (get-buffer-window other-name t)))
         (split-is-current (or (eql current-buffer-window new-buffer-window)
                               (eql current-buffer-window other-buffer-window))))
    (when (or (and other-buffer-window
                   cmake-build-run-window-autoswitch)
              (and (not cmake-build-never-split)
                   (not split-is-current)
                   (<= cmake-build-run-window-size
                       (* (/ cmake-build-split-threshold 100.0)
                          (window-total-height current-buffer-window)))))
      (unless (cmake-build--switch-to-buffer buffer (get-buffer-window buffer t) other-buffer-window)
        (when (and (not other-buffer-window)
                   (not (get-buffer-window name t)))
          (let ((window (split-window-below (- cmake-build-run-window-size))))
            (set-window-buffer window buffer)
            (set-window-dedicated-p window t))))
      t)))

(defun cmake-build--popup-buffer (name other-name)
  (let* ((buffer (get-buffer-create name))
         (current-buffer-window (get-buffer-window buffer t))
         (other-buffer-window (and other-name (get-buffer-window other-name t)))
         (buffer-config-name (cmake-build-get-run-config-name)))
    (unless (cmake-build--switch-to-buffer buffer (get-buffer-window buffer t) other-buffer-window)
      (display-buffer-pop-up-frame buffer default-frame-alist))
    (when cmake-build-raise-frame
      (raise-frame (window-frame (get-buffer-window buffer t))))
    t))

(defun cmake-build--display-buffer (name &optional other-name)
  (case cmake-build-display-type
    (split (cmake-build--split-to-buffer name other-name))
    (frame (cmake-build--popup-buffer name other-name))))

(cl-defun cmake-build--compile (buffer-name command &key sentinel other-buffer-name)
  (let* ((did-split (cmake-build--display-buffer buffer-name other-buffer-name))
         (display-buffer-alist
          ;; Suppress the window only if we actually split
          (if did-split
              (cons (list buffer-name #'display-buffer-no-window)
                    display-buffer-alist)
            display-buffer-alist))
         (actual-directory default-directory))
    (if (get-buffer-process buffer-name)
        (message "Already building %s/%s"
                 (projectile-project-name)
                 (symbol-name cmake-build-profile))
      (with-current-buffer buffer-name
        (setq-local compilation-directory actual-directory)
        (setq-local default-directory actual-directory))
      ;; compile saves buffers; rely on this now
      (let* ((compilation-buffer-name-function (lambda (&rest r) buffer-name)))
        (cl-flet ((run-compile () (compile (concat "time " command))))
          (let ((w (get-buffer-window buffer-name t)))
            (if (and w (not (eql (get-buffer-window) w)))
                (if cmake-build-switch-to-build
                    (progn
                      (switch-to-buffer-other-window buffer-name)
                      (run-compile))
                  (with-selected-window w
                    (run-compile)))
              (run-compile))))
        (when sentinel
          (let ((process (get-buffer-process buffer-name)))
            (when (process-live-p process)
              (set-process-sentinel process
                                    (lambda (p e)
                                      (funcall sentinel p e)
                                      (compilation-sentinel p e))))))
        (with-current-buffer buffer-name
          (mapcar (lambda (w)
                    (set-window-point w (point-max)))
                  (get-buffer-window-list buffer-name nil t))
          (visual-line-mode 1)
          (when cmake-build-override-compile-keymap
            (use-local-map cmake-build-run-keymap)))))))

(defun cmake-build--invoke-build-current (&optional sentinel)
  (when (cmake-build--validate)
    (cmake-build--save-project-root ()
      (let* ((default-directory (cmake-build--get-build-dir))
             (config (cmake-build--get-build-config))
             (command (concat "cmake --build . " cmake-build-options " --target " (car config)))
             (buffer-name (cmake-build--build-buffer-name))
             (other-buffer-name (cmake-build--run-buffer-name)))
        (cmake-build--compile buffer-name command
                              :sentinel sentinel :other-buffer-name other-buffer-name)))))

(defun cmake-build-current ()
  (interactive)
  (cmake-build--invoke-build-current))


(defun cmake-build--invoke-run (config)
  (cmake-build--save-project-root ()
    (let* ((cmake-build-run-config config)
           (config (cmake-build--get-run-config))
           (command (cmake-build--get-run-command config))
           (default-directory (cmake-build--get-build-dir (car config)))
           (process-environment (append
                                 (list (concat "PROJECT_ROOT="
                                               (cmake-build--maybe-remote-project-root)))
                                 (cmake-build--get-run-config-env)
                                 process-environment))
           (buffer-name (cmake-build--run-buffer-name))
           (other-buffer-name (cmake-build--build-buffer-name))
           (display-buffer-alist
            (if (cmake-build--display-buffer buffer-name other-buffer-name)
                (cons (list buffer-name #'display-buffer-no-window)
                      display-buffer-alist)
              display-buffer-alist)))
      (if (get-buffer-process buffer-name)
          (message "Already running %s/%s"
                   (projectile-project-name)
                   (symbol-name cmake-build-profile))
        (async-shell-command command buffer-name)
        (with-current-buffer buffer-name
          (use-local-map cmake-build-run-keymap))))))

(defun cmake-build-run ()
  (interactive)
  ;; If we switch windows, remember what project we're building
  (when (cmake-build--validate "run")
    (let* ((this-root (cmake-build--project-root))
           (this-run-config cmake-build-run-config)
           (cmake-build-project-root this-root))
      (if cmake-build-before-run
          (cmake-build--invoke-build-current
           (lambda (process event)
             (let* ((this-root this-root)
                    (cmake-build-project-root this-root))
               (when (equalp "finished\n" event)
                 (cmake-build--invoke-run this-run-config)))))
        (cmake-build--invoke-run this-run-config)))))

(defun cmake-build-debug ()
  (interactive)
  (let* ((config (cmake-build--get-run-config))
         (command (cmake-build--get-run-command config))
         (default-directory (cmake-build--get-build-dir (car config)))
         (process-environment (append (cmake-build--get-run-config-env) process-environment)))
    (gdb (concat "gdb -i=mi --args " command))))

(defun cmake-build-set-options (option-string)
  (interactive
   (list
    (read-string "CMake build options: " cmake-build-options)))
  (setq cmake-build-options option-string))

(defun cmake-build-set-config (config-name)
  (interactive
   (list
    (let* ((configs (cmake-build--get-configs))
           (choices (mapcar (lambda (x) (symbol-name (car x))) configs)))
      (intern (ido-completing-read "CMake Config: " choices nil t nil nil (symbol-name (cmake-build-get-run-config-name)))))))
  (let* ((config (cmake-build--get-config config-name)))
    (if config
        (progn
          (cmake-build--set-run-config config-name)
          (let ((build (cmake-build--get-build-config))
                (run (cmake-build--get-run-config)))
            (message "Build: %s   Run: %s"
                     (car build)
                     (substring (cmake-build--get-run-command run) 2))))
      (message "cmake-build: %s isn't a config." config))))

(defun cmake-build-set-buffer-local-config ()
  (interactive)
  (setq-local cmake-build-run-config
              (list (copy-tree
                     (rassoc (cmake-build-get-run-config-name) cmake-build-run-config))))
  (call-interactively #'cmake-build-set-config))

(defun cmake-build-set-project-root (path)
  (interactive
   (list
    (let* ((default-directory (cmake-build--project-root)))
      (read-directory-name "CMake Build project root (blank to unset): "))))
  (message "Path: %s" path)
  (setq cmake-build-project-root (if (string= path "") nil path)))

(defun cmake-build-set-project-build-root (path)
  (interactive
   (list
    (let* ((default-directory (cmake-build--build-root)))
      (read-directory-name "CMake Build build root (blank to unset): "))))
  (if (equalp "" path)
      (progn
        (message "Build root reset to default")
        (cmake-build--set-build-root nil))
    (let* ((path (file-name-as-directory path))
           (existp (file-directory-p path)))
      (if existp
          (progn
            (message "Build root: %s" path)
            (cmake-build--set-build-root path))
        (message "Error: Path does not exist: %s" path)))))

(defun cmake-build-set-cmake-profile (profile-name)
  (interactive
   (list
    (let* ((profiles (cmake-build--get-cmake-profiles))
           (choices (mapcar (lambda (x) (symbol-name (car x))) profiles)))
      (intern (ido-completing-read "CMake Profile " choices nil t nil nil (symbol-name cmake-build-profile))))))
  (let* ((profile (cmake-build--get-profile profile-name)))
    (if profile
        (progn
          (message "Config: %s  cmake %s"
                   profile-name (car profile))
          (setq cmake-build-profile profile-name))
      (message "cmake-build: %s isn't a profile." profile))))

(defun cmake-build-run-cmake ()
  (interactive)
  (let* ((default-directory (cmake-build--get-build-dir))
         (buffer-name (cmake-build--build-buffer-name)))
    (cmake-build--compile buffer-name "cmake .")))

(defun cmake-build-clear-cache-and-configure ()
  (interactive)
  (let ((build-dir (cmake-build--get-build-dir)))
    (unless (file-exists-p build-dir)
      (make-directory build-dir t))
    (cmake-build--save-project-root ()
      (let* ((default-directory build-dir)
             (buffer-name (cmake-build--build-buffer-name))
             (other-buffer-name (cmake-build--run-buffer-name))
             (command (concat "cmake " (cmake-build--get-cmake-options)
                              " " (car (cmake-build--get-profile))
                              " " (cmake-build--maybe-remote-project-root))))
        (when (file-exists-p "CMakeCache.txt")
          (delete-file "CMakeCache.txt"))
        (cmake-build--compile buffer-name command
                              :other-buffer-name other-buffer-name)))))

(defun cmake-build-clean ()
  (interactive)
  (cmake-build--save-project-root ()
    (let* ((default-directory (cmake-build--get-build-dir))
           (buffer-name (cmake-build--build-buffer-name))
           (other-buffer-name (cmake-build--run-buffer-name)))
      (cmake-build--compile buffer-name "cmake --build . --target clean"
                            :other-buffer-name other-buffer-name))))

(defun cmake-build--get-available-targets ()
  (cmake-build--save-project-root ()
    (let* ((default-directory (cmake-build--get-build-dir))
           ;; cdr to skip the first line which is a cmake comment
           (raw-targets-list (cdr (split-string (shell-command-to-string "cmake --build . --target help") "\n"))))
      ;; the actual targets are after "... " in each string
      (mapcar 'cadr (mapcar  (function (lambda (x) (split-string x " "))) raw-targets-list)))))

(defun cmake-build-other-target (target-name)
  (interactive
   (list
    (completing-read "Target: " (cmake-build--get-available-targets))))
  (cmake-build--save-project-root ()
    (let* ((default-directory (cmake-build--get-build-dir))
           (buffer-name (cmake-build--build-buffer-name))
           (other-buffer-name (cmake-build--run-buffer-name)))
      (cmake-build--compile buffer-name
                            (concat "cmake --build . " cmake-build-options " --target " target-name)
                            :other-buffer-name other-buffer-name))))


(defun cmake-build-delete-current-windows ()
  "Delete the compile/run windows for the current run configuration"
  (interactive)
  (cl-flet ((f (name)
               (when-let ((b (get-buffer name)))
                 (mapcar #'delete-window (get-buffer-window-list b nil t)))))
    (f (cmake-build--build-buffer-name))
    (f (cmake-build--run-buffer-name))))

;;;; Menu stuff

(defun cmake-build--menu-profiles ()
  `((:set-profile menu-item ,(concat "Profile: " (symbol-name cmake-build-profile))
                  (keymap nil
                          ,@(mapcar (lambda (x)
                                      (list (car x) 'menu-item (symbol-name (car x)) t))
                                    (cmake-build--get-cmake-profiles))))))

(defun cmake-build--menu-configs ()
  (let ((config (cmake-build--get-build-config (cmake-build-get-run-config-name)))
        (name (symbol-name (cmake-build-get-run-config-name))))
    `((:set-config menu-item ,(concat "Config: "
                                      (if config name "<none selected>"))
                   (keymap nil
                           ,@(mapcar (lambda (x)
                                       (list (car x) 'menu-item (symbol-name (car x)) t))
                                     (cmake-build--get-configs)))))))

(defun cmake-build--menu-other-targets ()
  `((:build-other-target menu-item "Other Targets"
                         (keymap nil
                                 ,@(mapcar (lambda (x)
                                             (list x 'menu-item x t))
                                           (cmake-build--get-other-targets))))))


(defun cmake-build--menu-settings ()
  `((:info menu-item "Project Info" t)
    ,@(when (cmake-build--get-cmake-profiles)
        (cmake-build--menu-profiles))
    ,@(when (cmake-build--get-configs)
        (cmake-build--menu-configs))
    (nil menu-item "Tools"
         (keymap nil
                 (:cmake menu-item "Re-run cmake" t)
                 (:clean menu-item "Clean build" t)
                 (:nuke menu-item "Delete cache/Re-run cmake" t)
                 (:set-buffer-local menu-item "Set buffer-local run config" t)
                 (:set-options menu-item "Set cmake options" t)
                 (:set-root menu-item "Set project SOURCE root" t)
                 (:set-build-root menu-item "Set project BUILD root" t)))))


(defun cmake-build--menu (&optional config)
  (let ((config (or config (car (cmake-build--get-build-config)))))
    `(keymap "CMake Build"
             (:debug menu-item ,(concat "Debug " config) t)
             (:build menu-item ,(concat "Build " config) t)
             (:run menu-item ,(concat "Run " config) t)
             ,@(cmake-build--menu-configs)
             ,@(when (cmake-build--get-other-targets)
                 (cmake-build--menu-other-targets))
             ,@(cmake-build--menu-settings))))

(defun cmake-build--popup-menu (config)
  (x-popup-menu
   (list '(10 10) (selected-window))
   (cmake-build--menu config)))

(defun cmake-build--popup-settings-menu ()
  (x-popup-menu
   (list '(10 10) (selected-window))
   `(keymap "CMake Build: Settings" ,@(cmake-build--menu-settings))))

(defun cmake-build--menu-action-dispatch (action)
  (case (car action)
    (:info (message "Project root: %s" (cmake-build--project-root)))
    (:debug (cmake-build-debug))
    (:build (cmake-build-current))
    (:run (cmake-build-run))
    (:set-config (cmake-build-set-config (cadr action)))
    (:set-profile (cmake-build-set-cmake-profile (cadr action)))
    (:cmake (cmake-build-run-cmake))
    (:clean (cmake-build-clean))
    (:build-other-target (cmake-build-other-target (cadr action)))
    (:nuke (cmake-build-clear-cache-and-configure))
    (:set-options (call-interactively #'cmake-build-set-options))
    (:set-buffer-local (cmake-build-set-buffer-local-config))
    (:set-root (call-interactively #'cmake-build-set-project-root))
    (:set-build-root (call-interactively #'cmake-build-set-project-build-root))))

(defun cmake-build-menu ()
  (interactive)
  (let ((config (car (cmake-build--get-build-config))))
    (cmake-build--menu-action-dispatch
     (if config
         (cmake-build--popup-menu config)
       (cmake-build--popup-settings-menu)))))

(cmake-build--read-options)
(add-hook 'kill-emacs-hook #'cmake-build--write-options)

(provide 'cmake-build)
