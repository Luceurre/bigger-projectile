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
;; Depends on skeletor and projectile.

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

;;; Customization

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

(defvar bigger-projectile-current-project-active-configuration ()
  "Reference to the current project active configuration.")

(defun bigger-projectile-get-next-config-id ()
  "Return next available config id."
  1)

(cl-defstruct
    bigger-projectile-project-config
  name id default)

(defun bigger-projectile-make-new-project-config (name default)
  "Create a new project configuration with given options NAME and DEFAULT and return it."
  (make-bigger-projectile-project-config :name name :id (bigger-projectile-get-next-config-id) :default default)
  )
(defun bigger-projectile-generate-default-config ()
  "Generate default configuration for current project based on project type."
  ())

(defun bigger-projectile-save-config (filename)
  "Save project configurations in FILENAME."
  (bigger-projectile-dump-vars-to-file '(bigger-projectile-current-project-configurations) filename)
  )

(defun bigger-projectile-load-config (filename)
  "Load configurations stored in FILENAME."
  (bigger-projectile-read-file filename)
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

(defun bigger-projectile--create-new-configuration (name default)
  "Should not be called directly, create a new configuration with NAME.  Set it as default if DEFAULT."
  (interactive
   (list (read-string "Configuration name: ")
         (y-or-n-p "Make default? ")
         )
   )
  (setq bigger-projectile-current-project-configurations (push (bigger-projectile-make-new-project-config name default) bigger-projectile-current-project-configurations))
  (when bigger-projectile-make-new-configuration-active
    (setq bigger-projectile-current-project-active-configuration (gv-ref (car bigger-projectile-current-project-configurations)))
    )
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

(provide 'bigger-projectile)

;;; bigger-projectile.el ends here
;; (bigger-projectile-load-current-project-configuration)
