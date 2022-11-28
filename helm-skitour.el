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
;; Use the skitour API https://skitour.fr/api/

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'shr)

(declare-function helm-html-decode-entities-string "ext:helm-utils.el")
(declare-function helm-comp-read "ext:helm-mode.el")
(declare-function helm-browse-url "ext:helm-net.el")

(defvar osm-server)

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

(defcustom helm-skitour-openmap-fmt-url "https://www.openstreetmap.org/#map=14/%s/%s&layers=Y"
  "The url to browse map at latitude/longitude."
  :type 'string)

(defcustom helm-skitour-saison-end 7
  "Month when ski saison finish."
  :type 'integer)

;;;###autoload
(defun helm-skitour-setup-default-massifs (&optional append)
  (interactive "P")
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
    (let ((status (call-process "curl" nil t nil
                                "-s" "-X" "GET" "-H"
                                (format "cle: %s" helm-skitour-api-key)
                                url)))
      (if (= status 0)
          ;; Available only with emacs compiled --with-json. 
          (condition-case-unless-debug err
              (json-parse-string
               (buffer-substring-no-properties (point-min) (point-max))
               :object-type 'plist)
            (json-parse-error
             (message "Unable to parse json data from `%s': %s" url (cdr err))
             nil))
        (error "Process exited with status %s" status)))))

(defun helm-skitour-get-massifs ()
  (let ((data (helm-skitour-get-data "https://skitour.fr/api/massifs")))
    (cl-loop for o across data
             collect (cons (plist-get o :id)
                           (format "%s%s"
                                   (plist-get o :nom)
                                   (pcase (plist-get o :groupe)
                                     ("-" "")
                                     (grp (format " (%s)" grp))))))))

(defun helm-skitour-build-source (id cache fmt-url &optional type)
  (let* ((name (assoc-default (number-to-string id) helm-skitour-massifs))
         (scache (assoc-default name cache)))
    (helm-build-sync-source name
      :candidates (lambda () (helm-skitour-get-candidates scache))
      :diacritics t
      :persistent-action (cl-case type
                           (sorties #'helm-skitour-sorties-persistent-action)
                           (topos #'helm-skitour-topo-persistent-action))
      :action `(("Goto Skitour" . (lambda (candidate)
                                    (helm-browse-url (format ,fmt-url candidate))))
                ("Goto map" . helm-skitour-gotomap-action))
      :multiline t)))

(defvar helm-skitour-sortie-data-cache (make-hash-table :test 'equal))
(defun helm-skitour-get-sortie-data (id)
  (or (gethash id helm-skitour-sortie-data-cache)
      (puthash id (helm-skitour-get-data
                   (format "https://skitour.fr/api/sortie/%s" id))
               helm-skitour-sortie-data-cache)))

(defvar helm-skitour-topo-data-cache (make-hash-table :test 'equal))
(defun helm-skitour-get-topo-data (id)
  (or (gethash id helm-skitour-topo-data-cache)
      (puthash id (helm-skitour-get-data
                   (format "https://skitour.fr/api/topo/%s" id))
               helm-skitour-topo-data-cache)))

(defconst helm-skitour-sortie-conditions-tags
  '("Météo/températures :"
    "Horaires :"
    "Conditions d'accès/altitude du parking :"
    "Altitude de chaussage/déchaussage :"
    "Conditions pour le ski :"
    "Itinéraire suivi :"
    "Activité avalancheuse :"))

(defun helm-skitour-get-conditions (id)
  (let ((data (helm-skitour-get-sortie-data id)))
    (with-temp-buffer
      (save-excursion
        (insert (plist-get data :conditions)
                "\n\n"
                (plist-get data :recit))
        (shr-render-region (point-min) (point-max)))
      (while (re-search-forward
              (regexp-opt helm-skitour-sortie-conditions-tags) nil t)
        (add-face-text-property
         (match-beginning 0) (match-end 0) 'font-lock-keyword-face)
        (save-excursion
          (goto-char (match-beginning 0))
          (unless (bobp)
            (insert "\n"))))
      (buffer-string))))

(defun helm-skitour-PA-fill-buffer ()
  (goto-char (point-min))
  (save-excursion
    (while (not (eobp))
      (if (> (- (point-at-eol) (point-at-bol)) fill-column)
          (progn
            (goto-char (point-at-bol))
            (forward-char fill-column)
            (forward-word)
            (when (eq (char-after) ?\.)
              (forward-char 1))
            (insert "\n"))
        (forward-line 1)))))

(defun helm-skitour-sorties-persistent-action (id)
  (with-current-buffer (get-buffer-create "*skitour conditions*")
    (erase-buffer)
    (save-excursion
      (insert (helm-skitour-get-conditions id)))
    (display-buffer (current-buffer))))

(defun helm-skitour-topo-persistent-action (id)
  (with-current-buffer (get-buffer-create "*skitour topo*")
    (erase-buffer)
    (save-excursion
      (insert (helm-skitour-get-itin id)))
    (display-buffer (current-buffer))))

(defun helm-skitour-get-itin (id)
  (let* ((data (helm-skitour-get-topo-data id))
         (variantes (plist-get data :variantes))
         noms pos)
    (with-temp-buffer
      (insert (plist-get data :itineraire))
      (when variantes
        (setq pos (point))
        (insert "Variantes:")
        (cl-loop for lst across variantes
                 for nom = (plist-get lst :nom)
                 for desc = (plist-get lst :description)
                 for deniv = (plist-get lst :denivele)
                 for ski = (plist-get lst :ski)
                 for orientation = (plist-get lst :orientation)
                 do (push (format "\\(?:%s\\)" nom) noms)
                 do (insert (concat nom ": "
                                    (if (string= desc "")
                                        (format "(deniv: %s, ski: %s, orientation: %s)"
                                                deniv ski orientation)
                                      desc)
                                    "\n"))))
      (shr-render-region (point-min) (point-max))
      (when variantes
        (goto-char pos)
        (while (re-search-forward
                (mapconcat 'identity (cons "\\(?:Variantes\\)" (reverse noms)) "\\|") nil t)
          (add-face-text-property
           (match-beginning 0) (match-end 0) 'font-lock-keyword-face)
          (save-excursion
            (goto-char (match-beginning 0))
            (insert "\n"))))
      (buffer-string))))

(defun helm-skitour-gotomap-action (id)
  (let* ((latlon (or (get-text-property
                      0 'latlon
                      (helm-get-selection nil 'withprop))
                     (plist-get
                      (plist-get
                       (helm-skitour-get-sortie-data id)
                       :depart)
                      :latlon)))
         (lat (and latlon (aref latlon 0)))
         (lon (and latlon (aref latlon 1))))
    (when latlon
      (helm-skitour-gotomap lat lon))))

(defun helm-skitour-gotomap (lat lon &optional zoom)
  (require 'osm nil t)
  (if (fboundp 'osm-goto)
      (let ((osm-server 'opentopomap))
        (osm-goto (string-to-number lat) (string-to-number lon) (or zoom 12)))
    (helm-browse-url
     (format helm-skitour-openmap-fmt-url lat lon))))

(defun helm-skitour-build-sources (cache type)
  (let ((url (cl-case type
               (sorties "https://skitour.fr/sorties/%s")
               (topos "https://skitour.fr/topos/%s"))))
    (cl-loop for id in helm-skitour-default-massifs-ids
             collect (helm-skitour-build-source id cache url type))))

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
           for latlon = (and depart (plist-get depart :latlon))
           for auteur = (plist-get (plist-get 0 :auteur) :pseudo)
           collect (cons (propertize
                          (format "%s%s\n Deniv: %s, Orient: %s, Diff: %s%s%s."
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
                          'latlon latlon)
                         id)))

(defun helm-skitour-saison ()
  (let ((year (format-time-string "%Y"))
        (month (format-time-string "%m")))
    (if (> (string-to-number month) helm-skitour-saison-end)
        (number-to-string (1+ (string-to-number year)))
      year)))

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
          (cl-loop with saison = (helm-skitour-saison)
                   for id in helm-skitour-default-massifs-ids
                   collect `(,(assoc-default (number-to-string id) helm-skitour-massifs)
                             . ,(helm-skitour-get-data (format "https://skitour.fr/api/sorties?a=%s&m=%s"
                                                               saison id))))))
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
