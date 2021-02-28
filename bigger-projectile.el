;;; bigger-projectile.el --- An extension for the famous emacs project manager Projectile.

;; Copyright (C) 2021 Pierre Glandon

;; Author: Pierre Glandon <pglandon78@gmail.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An extension for the famous Emacs project manager Projectile.
;; Depends on skeletor, projectile, ido and workspaces.

;;; Code:

(require 'projectile)

;;; Routines

(defun bigger-projectile-dump-vars-to-file (varlist filename)
  "Simplistic dumping of variables in VARLIST to a file FILENAME."
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (bigger-projectile-dump-vars-to-buffer varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun bigger-projectile-dump-vars-to-buffer (varlist buffer)
  "Insert into BUFFER the setq statement to recreate the variables in VARLIST."
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

(defun bigger-projectile-read-file (filename)
  "Load Emacs Lisp FILENAME."
  (load filename))

;;; Customization and variables declarations

(defgroup bigger-projectile nil
  "Configure projects easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/Luceurre/bigger-projectile"))

(defcustom bigger-projectile-use-default t
  "Should bigger-projectile use templated configuration for new projects.")

(defcustom bigger-projectile-project-config-dir ".bigger-projectile/"
  "Directory where project's configurations are stored.")

(defcustom bigger-projectile-project-config-file "config.el"
  "File where project's configurations are stored.")

(defcustom bigger-projectile-make-new-configuration-active t
  "If non-nil, make newly created configuration the active one.")

(defvar bigger-projectile-current-project-configurations (list)
  "Contain current project configurations, nil if not in a project.")

(defvar bigger-projectile-current-project-active-configuration-id ()
  "Reference to the current project active configuration.")

(defvar bigger-projectile-current-project-root (projectile-project-p)
  "Current project root.")

(defvar bigger-projectile-next-project-configuration-id 0
  "Next project configuration id.")

(defvar bigger-projectile-default-configuration-id ()
  "What should be the default configuration on project load."
  )

(defun bigger-projectile-get-next-config-id ()
  "Return next available config id."
  (setq bigger-projectile-next-project-configuration-id (+ 1 bigger-projectile-next-project-configuration-id))
  (- bigger-projectile-next-project-configuration-id 1)
  )

(defun bigger-projectile-save-workspace ()
  "Save current workspace."
  (interactive)
  (when (projectile-project-p)
    (+workspace:save (concat "" (projectile-project-name)))
    )
  )

(defun bigger-projectile-load-workspace ()
  "Load saved workspace if it exists."
  (interactive)
  (when (projectile-project-p)
    (+workspace-delete (projectile-project-name) t)
    (+workspace:load (concat "" (projectile-project-name)))
    )
  )

(cl-defstruct
    bigger-projectile-project-config
  name id)

(defun bigger-projectile-make-new-project-config (name)
  "Create a new project configuration with given options NAME and DEFAULT and return it."
  (make-bigger-projectile-project-config :name name :id (bigger-projectile-get-next-config-id))
  )
(defun bigger-projectile-generate-default-config ()
  "Generate default configuration for current project based on project type."
  ())

(defun bigger-projectile-save-config (filename)
  "Save project configurations in FILENAME."
  (bigger-projectile-dump-vars-to-file '(bigger-projectile-current-project-configurations bigger-projectile-current-project-active-configuration-id bigger-projectile-next-project-configuration-id) filename)
  )

(defun bigger-projectile-load-config (filename)
  "Load configurations stored in FILENAME."
  (bigger-projectile-read-file filename)
  )

(defun bigger-projectile--set-active-configuration (config)
  "Set active configuration to CONFIG."
  (setq bigger-projectile-current-project-active-configuration-id (bigger-projectile-project-config-id config))
  )

(defun bigger-projectile-load-configuration-or-create-it (dir)
  "Load config files in DIR and create it if it doesn't exist."
  (let* ((config-dir (concat dir bigger-projectile-project-config-dir))
         (config-file (concat config-dir bigger-projectile-project-config-file)))
    (if (and (file-directory-p config-dir) (file-exists-p config-file))
        (bigger-projectile-load-config config-file)
      (progn
        (make-directory bigger-projectile-project-config-dir)
        (bigger-projectile-generate-default-config)
        (bigger-projectile-save-config config-file)
        )
      )
    )
  )

(defun bigger-projectile-load-current-project-configuration ()
  "Populate project's configuration with PROJECT configurations."
  (let ((project-root (projectile-project-p)))
    (when project-root
      (bigger-projectile-load-configuration-or-create-it project-root)
      )
    )
  )

(defun bigger-projectile-project-p ()
  "Assure you are in a project by returning its root, print message otherwise."
  (let ((project-root (projectile-project-p)))
    (if project-root
        project-root
      (message "You are not in a project!"))
    )
  )

(defun bigger-projectile--create-new-configuration (name)
  "Should not be called directly, create a new configuration with NAME.  Set it as default if DEFAULT."
  (interactive
   (list (read-string "Configuration name: "))
   )
  (setq bigger-projectile-current-project-configurations (push (bigger-projectile-make-new-project-config name) bigger-projectile-current-project-configurations))
  (when (or bigger-projectile-make-new-configuration-active (not bigger-projectile-current-project-active-configuration))
    (setq bigger-projectile-current-project-active-configuration-id (bigger-projectile-project-config-id (car bigger-projectile-current-project-configurations)))
    )
  (bigger-projectile--save-configurations)
  )

(defun bigger-projectile-create-new-configuration ()
  "Create and store a new configuration for current active project."
  (interactive)
  (let ((project-root (bigger-projectile-project-p)))
    (when project-root
      (call-interactively 'bigger-projectile--create-new-configuration)
      )
    )
  )

(defun bigger-projectile--get-configuration-position-from-id (id)
  "Return configuration reference from ID."
  (let ((i 0) (pos (- 1)) (configs bigger-projectile-current-project-configurations))
    (while configs
      (let ((config (car configs)))
        (when (eq (bigger-projectile-project-config-id config) id)
          (setq pos i)
          )
        (setq configs (cdr configs))
        (setq i (+ i 1))
        )
      )
    pos
    )
  )

(defun bigger-projectile--save-configurations ()
  "Save configuration without any check."
  (bigger-projectile-save-config (concat (projectile-project-p) (concat bigger-projectile-project-config-dir bigger-projectile-project-config-file)))
  )

(defun bigger-projectile--set-active-configuration-name (new-name)
  "Change active configuration name with NEW-NAME."
  (interactive "sNew name: ")
  (let ((config-pos (bigger-projectile--get-configuration-position-from-id bigger-projectile-current-project-active-configuration-id)))
    (setf (bigger-projectile-project-config-name (nth config-pos bigger-projectile-current-project-configurations)) new-name)
    (bigger-projectile--save-configurations)
    )
  )

(defun bigger-projectile-set-active-configuration-name ()
  "Change active configuration name."
  (interactive)
  (let ((project-root (bigger-projectile-project-p)))
    (when project-root
      (call-interactively 'bigger-projectile--set-active-configuration-name)
      )
    )
  )

(defun bigger-projectile--get-configurations ()
  "Return a list of list with (config-name position)."
  (let ((result-list) (i 0) (configs bigger-projectile-current-project-configurations))
    (while configs
      (let ((config (car configs)))
        (push (list (bigger-projectile-project-config-name config) i) result-list)
        )
      (setq configs (cdr configs))
      (setq i (+ 1 i))
      )
    result-list
    )
  )

(defun bigger-projectile-ask-for-configuration ()
  "Ask the user to select a configuration and return the configuration position."
  (when (projectile-project-p)
    (let* ((values-alist (bigger-projectile--get-configurations)) (result (ido-completing-read "Select config: " values-alist nil t)))
      (car (last (assoc result values-alist)))
      )
    )
  )

(defun bigger-projectile-set-active-config ()
  "Set the active configuration."
  (interactive)
  (let ((config-pos (bigger-projectile-ask-for-configuration)))
    (setq bigger-projectile-current-project-active-configuration-id (bigger-projectile-project-config-id (nth config-pos bigger-projectile-current-project-configurations)))
    )
  )

(provide 'bigger-projectile)

;; TODO add hooks with projectile
;; TODO add run

;;; bigger-projectile.el ends here
