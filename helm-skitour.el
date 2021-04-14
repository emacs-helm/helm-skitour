;;; helm-skitour.el --- helm skitour interface -*- lexical-binding: t -*-

;; Copyright (C) 2021 Thierry Volpiatto <thievol@posteo.net>

;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
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

;; Helm interface to https://skitour.fr/

;;; Code:

(require 'cl-lib)
(require 'helm)

(declare-function helm-html-decode-entities-string "ext:helm-utils.el")
(declare-function helm-comp-read "ext:helm-mode.el")

(defgroup helm-skitour nil
  "Skitour helm interface."
  :group 'helm)

;; User vars
(defcustom helm-skitour-api-key nil
  "The API key from your Skitour account."
  :type 'string)

(defcustom helm-skitour-default-massifs-ids '(6 7 8 14)
  "La liste des codes massif favoris.
Use `helm-skitour-setup-default-massifs' command
to configure this variable with completion."
  :type '(repeat integer))

;;;###autoload
(defun helm-skitour-setup-default-massifs (&optional append)
  (interactive "R")
  (let ((data (helm-skitour-get-massifs)))
    (customize-save-variable
     'helm-skitour-default-massifs-ids
     (helm-fast-remove-dups
      (append
       (helm-comp-read "Select favorite Massifs (mark candidates) : "
                       (cl-loop for (id . mass) in data
                                collect (cons mass (string-to-number id)))
                       :fc-transformer (lambda (candidates _source)
                                         (cl-loop for (d . r) in candidates
                                                  if (member r helm-skitour-default-massifs-ids)
                                                  collect (cons (propertize d 'face 'font-lock-keyword-face) r)
                                                  else collect (cons d r)))
                       :marked-candidates t)
       (and append helm-skitour-default-massifs-ids))))))
    
;; Internals
(defvar helm-skitour-massifs nil)
(defvar helm-skitour-sorties-cache nil)
(defvar helm-skitour-topos-cache nil)

(defun helm-skitour-get-data (url)
  (with-temp-buffer
    (call-process "curl" nil t nil
                  "-s" "-X" "GET" "-H"
                  (format "cle: %s" helm-skitour-api-key)
                  url)
    (json-parse-string (buffer-string) :object-type 'plist)))

(defun helm-skitour-get-massifs ()
  (let ((data (helm-skitour-get-data "https://skitour.fr/api/massifs")))
    (cl-loop for o across data
             collect (cons (plist-get o :id)
                           (format "%s%s"
                                   (plist-get o :nom)
                                   (pcase (plist-get o :groupe)
                                     ("-" "")
                                     (grp (format " (%s)" grp))))))))

(defun helm-skitour-build-source (id cache fmt-url)
  (let* ((name (assoc-default (number-to-string id) helm-skitour-massifs))
         (scache (assoc-default name cache)))
    (helm-build-sync-source name
      :candidates (lambda () (helm-skitour-get-candidates scache))
      :action (lambda (candidate)
                (browse-url (format fmt-url candidate)))
      :multiline t)))

(defun helm-skitour-build-sources (cache type)
  (let ((url (cl-case type
               (sorties "https://skitour.fr/sorties/%s")
               (topos "https://skitour.fr/topos/%s"))))
    (cl-loop for id in helm-skitour-default-massifs-ids
             collect (helm-skitour-build-source id cache url))))

(defun helm-skitour-get-candidates (data)
  (cl-loop for o across data
           for id = (plist-get o :id)
           for titre = (helm-html-decode-entities-string
                        (or (plist-get o :titre) (plist-get o :nom)))
           for date =  (plist-get o :date)
           for orientation = (plist-get o :orientation)
           for deniv = (plist-get o :denivele)
           for difficulte = (plist-get o :dif_ski)
           ;; for gpx = (plist-get o :gpx)
           for depart = (plist-get o :depart)
           for pdepart = (and depart (plist-get depart :nom))
           for adepart = (and depart (plist-get depart :altitude))
           for auteur = (plist-get (plist-get o :auteur) :pseudo)
           collect (cons (format "%s%s\n Deniv: %s, Orient: %s, Diff: %s%s%s."
                                 (if date
                                     (propertize (format-time-string
                                                  "%d/%m/%y "
                                                  (time-convert (string-to-number date)))
                                                 'face 'font-lock-keyword-face)
                                   "")
                                 (propertize titre 'face 'font-lock-type-face)
                                 deniv orientation difficulte
                                 (if auteur (format ", Aut: %s" auteur) "")
                                 (if depart (format ", Dep: %s, Alt: %s" pdepart adepart) ""))
                         id)))

;;;###autoload
(defun helm-skitour-sorties (&optional arg)
  (interactive "P")
  (cl-assert helm-skitour-api-key nil
             "Get the API key from your Skitour account and add it to `helm-skitour-api-key'")
  (unless helm-skitour-massifs
    (setq helm-skitour-massifs
          (helm-skitour-get-massifs)))
  (when (or arg (null helm-skitour-sorties-cache))
    (setq helm-skitour-sorties-cache
          (cl-loop for id in helm-skitour-default-massifs-ids
                   collect `(,(assoc-default (number-to-string id) helm-skitour-massifs)
                             . ,(helm-skitour-get-data (format "https://skitour.fr/api/sorties?a=%s&m=%s"
                                                               (format-time-string "%Y") id))))))
  (let ((sources (helm-skitour-build-sources helm-skitour-sorties-cache 'sorties)))
    (helm :sources sources
          :buffer "*helm skitour sorties*")))

;;;###autoload
(defun helm-skitour-topos (&optional arg)
  (interactive "P")
  (cl-assert helm-skitour-api-key nil
             "Get the API key from your Skitour account and add it to `helm-skitour-api-key'")
  (unless helm-skitour-massifs
    (setq helm-skitour-massifs
          (helm-skitour-get-massifs)))
  (when (or arg (null helm-skitour-topos-cache))
    (setq helm-skitour-topos-cache
          (cl-loop for id in helm-skitour-default-massifs-ids
                   collect `(,(assoc-default (number-to-string id) helm-skitour-massifs)
                             . ,(helm-skitour-get-data (format "https://skitour.fr/api/topos?m=%s" id))))))
  (let ((sources (helm-skitour-build-sources helm-skitour-topos-cache 'topos)))
    (helm :sources sources
          :buffer "*helm skitour topos*")))


(provide 'helm-skitour)

;;; helm-skitour.el ends here
