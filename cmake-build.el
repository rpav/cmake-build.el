;;; -*- lexical-binding: t; -*-
;;; cmake-build.el --- Handle cmake build profiles and target/run configurations for projects

;; Copyright (C) 2019  Ryan Pavlik

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

(require 'cl)

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
  :type 'booloean
  :group 'cmake-build)

(defcustom cmake-build-run-window-size 20
  "Size of window to split."
  :type 'integer
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

(defun cmake-build--get-run-config-name ()
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

(defun cmake-build-project-name ()
  (let ((default-directory (cmake-build--project-root)))
    (projectile-project-name)))

(defun cmake-build--build-buffer-name ()
  (concat "*Build " (cmake-build-project-name) "/" (symbol-name cmake-build-profile) ": " (symbol-name (cmake-build--get-run-config-name)) "*"))

(defun cmake-build--run-buffer-name ()
  (concat "*Run " (cmake-build-project-name) "/" (symbol-name cmake-build-profile) ": " (symbol-name (cmake-build--get-run-config-name)) "*"))

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
  (cdr (assoc (or config (cmake-build--get-run-config-name))
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

(defun cmake-build-default-build-dir-function (project-root profile)
  (concat "build." profile))

(defun cmake-build--get-build-dir (&optional subdir)
  (concat (cmake-build--build-root)
          (funcall cmake-build-dir-name-function
                   (cmake-build--project-root)
                   (symbol-name cmake-build-profile))
          "/" (or subdir "")))

(defun cmake-build--get-run-command (config)
  (concat (cadr config) " " (caddr config)))

(defun cmake-build--split-to-buffer (name other-name)
  (let ((other-buffer-window (get-buffer-window other-name t)))
    (if (and cmake-build-run-window-autoswitch
             other-buffer-window)
        (progn
          (set-window-dedicated-p other-buffer-window nil)
          (set-window-buffer other-buffer-window
                             (get-buffer-create name))
          (set-window-dedicated-p other-buffer-window t))
      (when (and (not other-buffer-window)
                 (not (get-buffer-window name t)))
        (let ((window (split-window-below (- cmake-build-run-window-size))))
          (set-window-buffer window (get-buffer-create name))
          (set-window-dedicated-p window t))))))

(defun cmake-build--invoke-build-current (&optional sentinel)
  (cmake-build--save-project-root ()
    (let* ((default-directory (cmake-build--get-build-dir))
           (config (cmake-build--get-build-config))
           (command (concat "cmake --build . " cmake-build-options " --target " (car config)))
           (buffer-name (cmake-build--build-buffer-name))
           (other-buffer-name (cmake-build--run-buffer-name))
           (display-buffer-alist (cons (list buffer-name #'display-buffer-no-window)
                                       display-buffer-alist)))
      (cmake-build--split-to-buffer buffer-name other-buffer-name)
      (if (get-buffer-process buffer-name)
          (message "Already building %s/%s"
                   (projectile-project-name)
                   (symbol-name cmake-build-profile))
        (save-some-buffers 1)
        (async-shell-command (concat "time " command) buffer-name)
        (when sentinel
          (let ((process (get-buffer-process buffer-name)))
            (when (process-live-p process)
              (set-process-sentinel process sentinel))))
        (with-current-buffer buffer-name
          (visual-line-mode t))))))

(defun cmake-build-current ()
  (interactive)
  (cmake-build--invoke-build-current))

(defun cmake-build--invoke-run ()
  (cmake-build--save-project-root ()
    (let* ((config (cmake-build--get-run-config))
           (command (cmake-build--get-run-command config))
           (default-directory (cmake-build--get-build-dir (car config)))
           (process-environment (append
                                 (list (concat "PROJECT_ROOT=" (cmake-build--project-root)))
                                 (cmake-build--get-run-config-env)
                                 process-environment))
           (buffer-name (cmake-build--run-buffer-name))
           (other-buffer-name (cmake-build--build-buffer-name))
           (display-buffer-alist (cons (list buffer-name #'display-buffer-no-window)
                                       display-buffer-alist)))
      (cmake-build--split-to-buffer buffer-name other-buffer-name)
      (if (get-buffer-process buffer-name)
          (message "Already running %s/%s"
                   (projectile-project-name)
                   (symbol-name cmake-build-profile))
        (async-shell-command command buffer-name)))))

(defun cmake-build-run ()
  (interactive)
  ;; If we switch windows, remember what project we're building
  (let* ((this-root (cmake-build--project-root))
         (cmake-build-project-root this-root))
    (if cmake-build-before-run
        (cmake-build--invoke-build-current
         (lambda (process event)
           (let* ((this-root this-root)
                  (cmake-build-project-root this-root))
             (when (equalp "finished\n" event)
               (cmake-build--invoke-run)))))
      (cmake-build--invoke-run))))

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
      (intern (ido-completing-read "CMake Config: " choices nil t nil nil (symbol-name (cmake-build--get-run-config-name)))))))
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
                     (rassoc (cmake-build--get-run-config-name) cmake-build-run-config))))
  (call-interactively #'cmake-build-set-config))

(defun cmake-build-set-project-root (path)
  (interactive
   (list
    (let* ((default-directory (cmake-build--project-root)))
      (read-directory-name "CMake Build project root (blank to unset): "))))
  (message "Path: %s" path)
  (setq cmake-build-project-root (if (equal path "") nil path)))

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
         (buffer-name (cmake-build--build-buffer-name))
         (display-buffer-alist (cons (list buffer-name #'display-buffer-no-window)
                                     display-buffer-alist)))
    (save-some-buffers t)
    (cmake-build--split-to-buffer buffer-name (cmake-build--run-buffer-name))
    (async-shell-command "cmake ." buffer-name)
    (with-current-buffer buffer-name
      (visual-line-mode t))))

(defun cmake-build-clear-cache-and-configure ()
  (interactive)
  (unless (file-exists-p (cmake-build--get-build-dir))
    (make-directory (cmake-build--get-build-dir)))
  (let* ((default-directory (cmake-build--get-build-dir))
         (buffer-name (cmake-build--build-buffer-name))
         (command (concat "cmake " (cmake-build--get-cmake-options)
                          " " (car (cmake-build--get-profile))
                          " " (cmake-build--project-root)))
         (display-buffer-alist (cons (list buffer-name #'display-buffer-no-window)
                                     display-buffer-alist)))
    (save-some-buffers t)
    (cmake-build--split-to-buffer buffer-name (cmake-build--run-buffer-name))
    (when (file-exists-p "CMakeCache.txt")
      (delete-file "CMakeCache.txt"))
    (async-shell-command command buffer-name)
    (with-current-buffer buffer-name
      (visual-line-mode t))))

(defun cmake-build-clean ()
  (interactive)
  (let* ((default-directory (cmake-build--get-build-dir))
         (buffer-name (cmake-build--build-buffer-name))
         (display-buffer-alist (cons (list buffer-name #'display-buffer-no-window)
                                     display-buffer-alist)))
    (save-some-buffers t)
    (cmake-build--split-to-buffer buffer-name (cmake-build--run-buffer-name))
    (async-shell-command "cmake --build . --target clean" buffer-name)))

(defun cmake-build-other-target (target-name)
  (interactive "sTarget: ")
  (let* ((default-directory (cmake-build--get-build-dir))
         (buffer-name (cmake-build--build-buffer-name))
         (display-buffer-alist (cons (list buffer-name #'display-buffer-no-window)
                                     display-buffer-alist)))
    (save-some-buffers t)
    (cmake-build--split-to-buffer buffer-name (cmake-build--run-buffer-name))
    (async-shell-command (concat "cmake --build . --target " target-name) buffer-name)))


;;;; Interactive add stuff

(defun cmake-build-add-profile (profile-name commandline)
  (interactive "SNew profile name: \nsCommandline: " )
  )

;;;; Menu stuff

(defun cmake-build--menu-profiles ()
  `((:set-profile menu-item ,(concat "Profile: " (symbol-name cmake-build-profile))
                  (keymap nil
                          ,@(mapcar (lambda (x)
                                      (list (car x) 'menu-item (symbol-name (car x)) t))
                                    (cmake-build--get-cmake-profiles))))))

(defun cmake-build--menu-configs ()
  (let ((config (cmake-build--get-build-config (cmake-build--get-run-config-name)))
        (name (symbol-name (cmake-build--get-run-config-name))))
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
                 (:set-build-root menu-item "Set project build root" t)))))


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
  (message "action %s" action)
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
