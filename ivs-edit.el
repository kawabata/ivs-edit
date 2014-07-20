;;; ivs-edit.el --- IVS (Ideographic Variation Sequence) editing tool  -*- lexical-binding: t -*-

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: Tools for Editing Ideographic Variation Sequences
;; Package-Requires: ((emacs "24.3") (dash "2.6.0") (cl-lib "1.0"))
;; Created: 2014-01-01
;; Keywords: text
;; Namespace: ivs-edit-
;; Human-Keywords: Ideographic Variation Sequence
;; Version: 1.140720
;; URL: http://github.com/kawabata/ivs-edit

;;; Commentary:
;;
;; * IVS Editing Tool for Emacs
;;
;; This file provides various IVS (Ideographic Variation Sequence)
;; editing tools for Emacs. IVS represents variations of CJK
;; Ideogrpaphs.  For details, please refer Unicode Technical Standard
;; #37 (http://www.unicode.org/reports/tr37/).
;;
;; Data file "IVD_Sequences.txt", integrated into this tool, is
;; distributed from http://www.unicode.org/ivd.
;;
;; ** Supported Emacsen
;;
;; Currenty, Emacs for X-Windows (with `libotf' linked) and Emacs Mac
;; Port (with Yamamoto Mituharu patch, see
;; https://github.com/railwaycat/emacs-mac-port for details) support
;; IVS.
;;
;; ** Supported Fonts
;;
;; Most of recent Adobe-Japan1 fonts support `Adobe-Japan1' IVD
;; collection. Hanazono Mincho (http://fonts.jp/hanazono/) supports
;; `Hanyo-Denshi' IVD collection. `Moji_Joho' collection shares some
;; sequences with `Hanyo-Denshi' collection.
;;
;; ** Basic setup
;;
;; : (autoload 'ivs-edit "ivs-edit" nil t)                 ; if necessary
;; : (global-set-key (kbd "M-J") 'ivs-edit)                ; sample keybinding
;; : (setq ivs-edit-preferred-collections '(Adobe-Japan1)) ; if you only use Adobe-Japan1 IVD.
;;
;; ** Inserting and Checking IVS characters
;;
;; Executing `M-x ivs-edit' (or pressing `M-J' if configured as above)
;; on Kanji character will show, and replace to, a series of IVS. If
;; executed on IVS, the collection name and the ID of IVS will be
;; displayed in minibuffer.
;;
;; ** Converting to/from TeX representation of IVS
;;
;; `ivs-edit-aj1-to-tex-region' and `ivs-edit-tex-to-aj1-region' can
;; convert IVS to pLaTeX CID command and vice versa. XeLaTeX supports
;; IVS natively.
;;
;; ** Converting Japanese Kanji to Old Style
;;
;; `ivs-edit-old-style-region' convert Japanese Kanji to its old style
;; with Adobe-Japan1 IVS.
;;
;; ** Highlighting non-AJ1 Kanji characters.
;;
;; `M-x ivs-edit-highlight-non-aj1' highlights non-AJ1 Kanji characters.
;; This feature is useful for writing text for e-Book readers which only
;; supports Adobe-Japan1 characters.

;;; Code:

(require 'dash)
(require 'cl-lib)

(defgroup ivs-edit nil
  "IVS (Ideographic Variation Sequence) editing tool."
  :prefix "ivs-edit-"
  :group 'text)

(defcustom ivs-edit-preferred-collections '(Adobe-Japan1 Hanyo-Denshi Moji_Joho)
  "List and order of preferred collection for IVS editing."
  :type '(symbol)
  :group 'ivs-edit)

(eval-and-compile
(defun ids-edit-addhash (key value table)
  "Add to KEY a VALUE in table TABLE."
  (let* ((old-value (gethash key table)))
    (if old-value (nconc old-value (list value))
      (puthash key (list value) table)))))

(eval-and-compile
(defvar ivs-edit-table
  (eval-when-compile
    (let* ((directory (file-name-directory (or byte-compile-current-file
                                               load-file-name
                                               buffer-file-name)))
           (ivd-file (expand-file-name "IVD_Sequences.txt" directory))
           (table (make-hash-table :test 'equal)))
      (unless (file-exists-p ivd-file) (error "Data file not found!"))
      (with-temp-buffer
        (insert-file-contents ivd-file)
        (while (re-search-forward
                "^\\([0-9A-F]+\\) \\(E0[0-9A-F]+\\); \\(.+?\\); \\(.+\\)" nil t)
          (let* ((char (string-to-number (match-string 1) 16))
                 (vs (string-to-number (match-string 2) 16))
                 (collection (intern (match-string 3)))
                 (name (match-string 4)))
            (ids-edit-addhash char (list vs collection name) table))))
      table))
  "IVS table."))

(defvar ivs-edit-old-table
  (eval-when-compile
    (let* ((directory (file-name-directory (or byte-compile-current-file
                                               load-file-name
                                               buffer-file-name)))
           (jp-old-file (expand-file-name "jp-old-style.txt" directory))
           (table (make-hash-table)))
        (unless (file-exists-p jp-old-file) (error "Data file not found!"))
        (with-temp-buffer
          (insert-file-contents jp-old-file)
          (while (re-search-forward
                  "^\\(.\\)[󠄀-󠇯]?	\\(\\cC[󠄀-󠇯]?\\)" nil t)
            (let* ((char (string-to-char (match-string 1)))
                   (ivs  (match-string 2) ))
              (ids-edit-addhash char ivs table))))
        table))
    "IVS old-style char table.")

(defvar ivs-edit-cid-table
  (eval-when-compile
    (let ((table (make-hash-table)))
      (maphash
       (lambda (char entry)
         (dolist (item entry)
           (when (equal (cadr item) 'Adobe-Japan1)
             (let ((cid (string-to-number (substring (cl-caddr item) 4))))
               (unless (gethash cid table)
                 (puthash cid (string char (car item)) table))))))
       ivs-edit-table)
      table))
  "IVS CID table.")

;;;;;###autoload
(defun ivs-edit ()
  "Insert and Verify IVS after the current point."
  (interactive)
  (let* ((char    (char-after (point)))
         (vs      (char-after (1+ (point))))
         (entry   (gethash char ivs-edit-table))
         (matches (--filter (eq vs (car it)) entry)))
    (when entry
      (if matches
          (message "%s"
                   (mapconcat (lambda (x)
                                (format "collection=%s name=%s"
                                        (cadr x) (cl-caddr x))) matches ", "))
        (let ((seq-list
               (mapcar
                (lambda (collection)
                  (let ((items (--filter (equal (cadr it) collection) entry)))
                    (mapcar 'car items)))
                ivs-edit-preferred-collections)))
          (delete-char 1)
          (insert "《"
                  (mapconcat
                   (lambda (seq) (mapconcat (lambda (vs) (string char vs)) seq ""))
                   seq-list "/")
                  "》"))))))

;;;;;###autoload
(defun ivs-edit-aj1-to-tex-region (from to)
  "Convert IVS in region (FROM TO) to LaTeX \\CID{XXXX} expressions."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cC\\)\\([󠄀-󠇯]\\)" nil t)
        (let* ((entry (gethash (string-to-char (match-string 1)) ivs-edit-table))
               (item  (assq (string-to-char (match-string 2)) entry))
               (collection (cadr item))
               (name  (cl-caddr item)))
          (when (equal collection 'Adobe-Japan1)
            (replace-match (format "\\\\CID{%s}" (substring name 4)))))))))

;;;;;###autoload
(defun ivs-edit-tex-to-aj1-region (from to)
  "Convert LaTeX `\\CID{XXXX}' expressions in region (FROM TO) to IVS."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\\\CID{\\([0-9]+\\)}" nil t)
        (let ((ivs (gethash (string-to-number (match-string 1))
                            ivs-edit-cid-table)))
          (when ivs (replace-match ivs)))))))

;;;;;###autoload
(defun ivs-edit-old-style-region (from to)
  "Convert modern Japanese to Old Japanese in region (FROM TO)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cC\\)[󠄀-󠇯]?" nil t)
        (let ((old (gethash (string-to-char (match-string 1)) ivs-edit-old-table)))
          (when old
            (replace-match
             (if (equal (length old) 1) (car old)
               (apply 'concat `("[" ,@old "]"))))))))))


(defun ivs-edit-non-aj1-search (to)
  "Search for non-AJ1 Kanji character bound to TO position.
Return the position of first such character"
  (let (point)
    (while (and (null point) (re-search-forward "\\cC" to t))
      (let* ((char  (char-after (match-beginning 0)))
             (entry (gethash char ivs-edit-table))
             (aj1   (and entry (cl-remove-if-not (lambda (x) (equal (cadr x) 'Adobe-Japan1)) entry))))
        (when (and (<= #x3400 char)
                   (or (null entry) (null aj1)))
          (setq point (point)))))
    point))

;;;;;###autoload
(defun ivs-edit-highlight-non-aj1 ()
  "Highlight Kanji characters do not belong to Adobe-Japan1 character set."
  (interactive)
  (font-lock-add-keywords
   nil '((ivs-edit-non-aj1-search 0 highlight t))))

(provide 'ivs-edit)

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:

;;; ivs-edit.el ends here
