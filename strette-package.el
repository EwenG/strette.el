;; strette-package.el ---   -*- lexical-binding: t; -*-

;; Copyright © 2018 Ewen Grosjean

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

;; This file is not part of GNU Emacs.

(require 'package-x)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defvar local-archive
  (expand-file-name "packages/" "~/strette.el")
  "Location of the package archive.")

(setq package-archive-upload-base local-archive)

(defun make-package (version)
  (let ((default-directory "~/"))
    (shell-command (format "cp -R strette.el strette-%s" version))
    (shell-command (format "COPYFILE_DISABLE=1 tar -cvf strette-%s.tar --exclude=\"strette-%s/.*\" --exclude=\"strette-%s/packages\" --exclude=\"strette-%s/*.iml\" --exclude=\"strette-%s/sample\" --exclude=\"strette-%s/test.log\" strette-%s/"
                           version version version version version version version))
    (shell-command (format "rm -r ~/strette-%s" version))
    (package-upload-file (format "~/strette-%s.tar" version))
    (shell-command (format "rm ~/strette-%s.tar" version))))

(comment
 (make-package "0.0.2")
 )

;; package-upload-file

(provide 'strette-package)
