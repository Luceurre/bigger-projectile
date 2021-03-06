;;; bigger-projectile-test.el --- Tests for bigger-projectile.el

;; Copyright (C) 2013 Pierre Glandon

;; Author: Pierre Glandon <pglandon78@gmail.com>

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

;; Tests for bigger-projectile.el

;;; Code:

(require 'ert)
(require 'bigger-projectile)

(ert-deftest bigger-projectile-check-sum ()
  (should (equal (+ 1 1) 2)))

(provide 'bigger-projectile-test)

;;; bigger-projectile-test.el ends here
