;;; csde.el -- Integrated Development Environment for Csharp.
;; $Revision: 1.5 $ $Date: 2001/02/25 05:10:13 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; Copyright (C) 2001 Matt Bruce

;; The JDE is Copyright (C) 1997, 1998, 1999, 2000, 2001 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is one of a set of packages that make up the 
;; Csharp Development Environment (CSDE) for Emacs. See the
;; CSDE User's Guide for more information.

;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;;; custom-load.el --- automatically extracted custom dependencies

;;; Code:

(autoload 'custom-add-loads "cus-load")

(custom-add-loads 'csde-project '("csde-db" "csde-help" "csde-imenu" "csde-import" "csde-csharp-font-lock" "csde-make" "csde-parse" "csde-run" "csde"))
(custom-add-loads 'bsh '("beanshell"))
(custom-add-loads 'csde-bug-window '("csde-bug"))
(custom-add-loads 'csde-csharpdoc '("csde-csharpdoc-gen" "csde-csharpdoc"))
(custom-add-loads 'csde-db-options '("csde-db"))
(custom-add-loads 'csde-package '("csde-package"))
(custom-add-loads 'csde-compile-options '("csde-compile"))
(custom-add-loads 'csde-which-method '("csde-which-method"))
(custom-add-loads 'csde '("csde-bug" "csde-compile" "csde-db" "csde-gen" "csde-csharpdoc" "csde-run" "csde-which-method" "csde"))
(custom-add-loads 'csde-gen '("csde-gen"))
(custom-add-loads 'csde-run-options '("beanshell" "csde-run"))
(custom-add-loads 'font-lock-highlighting-faces '("csde-csharp-font-lock"))
(custom-add-loads 'csde-bug '("csde-bug" "csde-dbs"))
(custom-add-loads 'tools '("csde-package" "csde"))

;;; custom-load.el ends here
