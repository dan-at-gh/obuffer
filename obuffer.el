;;; obuffer --- Buffer Menu


;;; Commentary:


;;; Code:


(defvar obuffer-database-file "/home/dan/.obuffer.db")

(defvar obuffer-current-sort-column 3)

(defvar obuffer-backend-subrepo-alist
  '(("hg" . ".hgsub")))

(defvar local-test-var nil)


;;*** Function for project roots


(defun obuffer-run-root-info ()
  (interactive)
  (message "root_infoo: Get version control backend, revision, nested, state-summary...")
  ;; To inhibit "root_info: finished" message:
  ;; (inhibit-sentinel-messages #'async-shell-command "root_info" "*Root Info Command*")
  (async-shell-command "root_info" "*Root Info Command*"))


(defun obuffer-import-project-roots ()
  (let (( lines (read-lines "/home/dan/.obuffer.log" t))
          roots)
    (dolist ( line lines)
      (let* (( props (split-string line))
             ( dir (nth 0 props))
             ( backend (nth 1 props))
             ( rev (nth 2 props))
             ( level (nth 3 props))
             ( states (nth 4 props))
             ( project-state (nth 5 props)))
        (setq roots (cons (list dir backend rev level
                                states project-state)
                          roots))))
    (reverse roots)))


(defun obuffer-root-directories ( &optional subdir)
  "List paths of project root directories.

List only the absolute path with trailing slash of each project
root.  Starting from SUBDIR, list those root directories which
contain this SUBDIR."
  (let ( dirs)
    (dolist ( root-info (obuffer-import-project-roots))
      (if subdir
          (when (string-match-p (regexp-quote (car root-info))
                                (expand-file-name subdir))
            (setq dirs (car root-info)))
        (setq dirs (cons (car root-info) dirs))))
    dirs))
    

;;*** Function for managing symbolic link dependencies


(defun obuffer-start-symlink-monitor ( &optional sleep-interval)
  (unless sleep-interval
    (setq sleep-interval 60))
  (async-shell-command (concat "symlink-info "
                               (number-to-string sleep-interval))
                       "*Symlink Info Command*"))



(defun obuffer-run-symlink-info ()
  (interactive)
  (shell-command "symlink-info"))

;; actually start the background process
;; (obuffer-start-symlink-monitor)


(defun obuffer-find-symlinks ( target)
  (let (( target-base (file-name-nondirectory target))
        symlinks)
    (with-temp-buffer
      (insert-file-contents "/home/dan/.obuffer-symlink.db")
      (goto-char (point-min))
      (while (re-search-forward
              (concat "\\(/.*?\\)\s+->\s+\\(.*?"
                      (regexp-quote target-base) "\\)\s*$")
              nil t)
        (let (( test-target (match-string 2)))
          (unless (file-name-absolute-p test-target)
            (setq test-target (expand-file-name
                               (concat (file-name-directory (match-string 1))
                                       (match-string 2)))))
          (when (string= test-target target)
            (setq symlinks (cons (match-string 1) symlinks))))))
      symlinks))


(defun obuffer-find-external-symlinks ( dir)
  (let ( symlinks)
    (with-temp-buffer
      (insert-file-contents "/home/dan/.obuffer-symlink.db")
      (goto-char (point-min))
      (while (re-search-forward "\\(/.*?\\)\s+->\s+\\(.*?\\)" nil t)
        (let* (( link (match-string 1))
               ( target (match-string 2))
               ( rel-link (not (file-name-absolute-p target))))
          (when rel-link
            (setq target (expand-file-name
                          (concat (file-name-directory (match-string 1))
                                  (match-string 2)))))
          (when (string-match (regexp-quote dir) target)
            (unless (and (string-match (regexp-quote dir) link)
                         rel-link)
              (setq symlinks (cons `(,target . ,link) symlinks)))))))
    symlinks))
    

(defun obuffer-mv-symlinks ( old-target new-target)
  (dolist ( link (obuffer-find-symlinks old-target))
    (delete-file link)
    (make-symbolic-link new-target link 1)
    (message "%s -> %s (old: %s)" link new-target old-target)))
      

;;*** Function associate files to their roots


(defun obuffer-file-projects ( file projects roots)
  "Associate buffer file name to project directory."
  (let (( match-length 0)
        end project)
    (dolist ( root roots)
      (let (( dir (car root)))
        (when (and (string-match dir file)
                   (> (setq end (match-end 0)) match-length))
          (setq project dir
                match-length end))))
    (when project
      (let (( dir project)
            pair)
        (if (not (setq pair (assoc dir projects)))
            (setq projects (cons (cons dir (list file)) projects))
            ;; Two possibilities to set the value of a alist cell, e.g.
            ;; (setcdr (assq 'three tmp) 3),
            ;; (setf (cdr (rassoc 4 tmp)) 3).
          (setcdr pair (cons file (cdr pair))))))
    (list project projects)))


(defun obuffer-get-file-buffers ( roots)
  "Get buffers visiting files inside project directories."
  (let ( file projects non-project-files non-file-buffers
         hidden-buffers file-switch)
    (dolist ( buffer (buffer-list))
      (if (not (setq file (buffer-file-name buffer)))
          (let (( name (buffer-name buffer)))
            (if (string-match "^\s" name)
                (setq hidden-buffers (cons name hidden-buffers))
              (setq non-file-buffers (cons name non-file-buffers))))
        (setq file-switch (obuffer-file-projects file projects roots))
        (if (car file-switch)
            (setq projects (cadr file-switch))
          (setq non-project-files (cons file non-project-files)))))
    (list projects non-project-files non-file-buffers hidden-buffers)))


;;*** Function for gathering file properties

(defun obuffer-file-props ( file &optional root)
  (let* (( filep (get-file-buffer file))
         ( buf (or filep (get-buffer file)))
         ( buffer (buffer-name buf))
         ( dir (file-name-directory file))
         ( rel-dir (if filep
                       (if root
                           (concat "./"
                                   (file-name-directory
                                    (file-relative-name file root)))
                         (abbreviate-file-name dir))
                     ""))
         ( modified (if (buffer-modified-p buf) "*" " "))
         ( modtime (when filep (nth 4 (file-attributes file))))
         ;; Get file size in bytes
         ;; ( size (when filep (nth 7 (file-attributes file))))
         ;; Get buffer size by number of characters
         ( size (buffer-size buf))
         ( state (if root
                     (symbol-name (or (vc-state file) '-))
                   ""))
         ;; ( rev (or (vc-working-revision file) "-1"))
         ( rev "-1")
         ( mode (replace-regexp-in-string "-mode$" ""
                 (symbol-name
                 (buffer-local-value 'major-mode
                                     buf))))
         ( read-only (if (with-current-buffer buf
                           buffer-read-only)
                         "%" " ")))
    (list file buffer dir rel-dir modified
          modtime size state rev mode read-only)))


;;*** Function for building the *Obuffer* files


(defun obuffer-truncate-buffer-name ( name width)
  (let (( truncate-string-ellipsis
          (concat (if (boundp 'truncate-string-ellipsis)
                      truncate-string-ellipsis
                    nil)
                  (file-name-extension name))))
    (truncate-string-to-width name width nil nil t)))


(defun obuffer-truncate-name ( name width)
  (let (( truncate-string-ellipsis "..."))
    (truncate-string-to-width name width nil nil t)))


(defun obuffer-insert-file ( file-props &optional projectp)
  (let* (( file (nth 0 file-props))
         ( buffer (nth 1 file-props))
         ( file-dir (nth 2 file-props))
         ( file-rel-dir (nth 3 file-props))
         ( modified (nth 4 file-props))
         ( modtime (nth 5 file-props))
         ( size (nth 6 file-props))
         ( state (nth 7 file-props))
         ( rev (nth 8 file-props))
         ( mode (nth 9 file-props))
         ( read-only (nth 10 file-props))
         ( full-buffer-name buffer)
         ( redundant-id (when (and file-dir
                                   (string= file-rel-dir "./")
                                   (string-match "<\\(.*?\\)>"
                                                 buffer))
                          (string= (match-string 1 buffer)
                                   (file-name-nondirectory
                                    (directory-file-name
                                     (file-name-directory
                                      file-dir)))))))
    (when (> (length rev) 6)
        (setq rev "id"))
    (setq redundant-id 'always)
    (when redundant-id
      (setq buffer (replace-regexp-in-string "<.*?>" "" buffer)))
    (insert (format (concat " %s%s %-30s %7s %-11s"
                            " %" (if projectp "-11" "") "s %s\n")
                    read-only modified
                    (propertize (obuffer-truncate-buffer-name buffer 30)
                                'font-lock-face 'bold
                                'mouse-face 'highlight
                                'buffer-name full-buffer-name
                                'help-echo (concat full-buffer-name
                                                   "\n" file
                                                   "\n[mouse-2] Switch to buffer."
                                                   "\n[mouse-3] Switch to buffer other window.")
                                'obuffer-file file)
                    (if projectp rev size)
                    (obuffer-truncate-name mode 11)
                    state
                    (propertize file-rel-dir
                                'mouse-face 'highlight
                                'dir-name file-dir
                                'help-echo (concat file-dir
                                                   "\n[mouse-2] Show in dired."
                                                   "\n[mouse-3] Show in dired other window.")
                                'obuffer-file file-dir)))
    (add-text-properties (line-beginning-position 0)
                         (line-end-position 0)
                       `( obuffer-buffer ,full-buffer-name
                          obuffer-dir ,file-dir
                          obuffer-file-name ,(file-name-nondirectory file)))))


;;*** Function compiling projects


(defvar obuffer-compile-commands
  '(( "waf" . "./waf install")
    ( "Makefile" . "make -k")
    ( "makefile" . "make -k")))

(defun obuffer-locate-dominating-dir ( file)
  "Find most basic folder with makefile.

Locate the makefile FILE inside the most basic project directory."
  (let* (( dir (expand-file-name default-directory))
         ( parent-dir (file-name-directory (directory-file-name dir)))
         ( root-dir (obuffer-root-directories dir))
         ( loop root-dir)
         src-root)
    (while (and loop
                (string-match-p (regexp-quote root-dir) dir))
      (when (string= dir parent-dir)
        (setq loop nil))
      (setq src-root (and (file-exists-p (concat dir file)) dir))
      (setq dir parent-dir
            parent-dir (file-name-directory (directory-file-name dir))))
    src-root))
    
(defun obuffer-compile-command ()
  (let (( path-length 0) src-root compile-cmd)
    (dolist ( make-cmd obuffer-compile-commands)
      (let (( src-root-tmp (obuffer-locate-dominating-dir (car make-cmd))))
        (when (and src-root-tmp
                   (> (length src-root-tmp) path-length))
          (setq src-root src-root-tmp
                compile-cmd (cdr make-cmd)
                path-length (length src-root)))))
    (when src-root
      `( ,src-root . ,compile-cmd))))

(defun obuffer-compile ()
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (let (( src-root-cmd (obuffer-compile-command)))
  (if src-root-cmd
      (when (y-or-n-p (format "Compile with cd %s && %s? "
                              (car src-root-cmd) (cdr src-root-cmd)))
        (with-temp-buffer
          (cd (car src-root-cmd))
          (compile (cdr src-root-cmd))))
    (call-interactively 'compile))))
  

;;*** Function for buffer header-line


(defun obuffer-header-line-sort ( &optional event)
  "Sort Obuffer entries by chosen column.

Inspired by `tabulated-list-col-sort'."
  (interactive "e")
  (let* (( pos (event-start event))
         ( obj (posn-object pos)))
    (with-current-buffer (window-buffer (posn-window pos))
      (setq obuffer-current-sort-column
            (get-text-property (cdr obj) 'sort-column (car obj)))))
  (obuffer-update))


(defun obuffer-header-line ()
  (let (( map (make-sparse-keymap)))
    (define-key map [header-line mouse-2] 'obuffer-header-line-sort)
    (let (( columns '(( 1 "Buffer" "%-30s")
                      ( 8 "Version" "%7s")
                      ( 9 "Mode" "%-11s")
                      ( 7 "State" "%-11s")
                      ( 3 "Path" "%s")))
          sel)
      (setq columns (reverse columns)
            header-line-format nil)
      (dolist ( column columns)
        (setq sel (= (nth 0 column) obuffer-current-sort-column))
        (setq header-line-format
              (cons (propertize (format (nth 2 column) (concat (nth 1 column)
                                                               (when sel " ▲")))
                                'sort-column (nth 0 column)
                                'face (if sel 'underline 'bold)
                                'mouse-face 'highlight
                                'keymap map)
                    header-line-format))
        (setq header-line-format (cons " " header-line-format)))
      (setq header-line-format (cons "CRM" header-line-format)
            header-line-format (cons mode-line-front-space
                                     header-line-format)))))


;;*** Function for creating *Obuffer* outline structure


(defun obuffer-sort-files ( files index &optional dir)
  (let (( prop-sort-files nil))
    (dolist ( file files)
      ;; Gather all info about the files for sorting.
      (setq prop-sort-files (cons (obuffer-file-props file dir)
                                  prop-sort-files)))
    (setq prop-sort-files (sort prop-sort-files (lambda ( a b)
                            (string< (nth index a) (nth index b)))))
    prop-sort-files))


(defun obuffer-insert-section-files ( files
                                      &optional
                                      root-dir sort-column projectp)
  (unless sort-column
    (setq sort-column obuffer-current-sort-column))
  (let (( prop-sort-files (obuffer-sort-files files
                                              sort-column
                                              root-dir)))
    (dolist ( file-props prop-sort-files)
      (obuffer-insert-file file-props projectp))))


(defun obuffer-project-buffers ( roots non-file-buffers)
  (let ( buffers project-buffers)
    (dolist ( dir (mapcar 'car roots))
      (setq buffers nil)
      (dolist ( name non-file-buffers)
        (when (string-match "<\\(.*?\\)>" name)
          (when (string-match (match-string 1 name) dir)
            (setq buffers (cons name buffers)))))
      (when buffers
        (dolist ( name buffers)
          (setq non-file-buffers (delete name non-file-buffers)))
        (setq project-buffers (cons (cons dir buffers)
                                    project-buffers))))
    (cons non-file-buffers project-buffers)))


(defun obuffer-insert-header ( root)
  (let* (( dir (nth 0 root))
         ( root-name (file-name-nondirectory dir))
         ( title (obuffer-directory-brief dir))
         ( type (nth 1 root))
         ( rev (nth 2 root))
         ( level (string-to-number (nth 3 root)))
         ( states (nth 4 root))
         ( map (make-sparse-keymap)))
    (define-key map [mouse-2]
      `(lambda ()
         (interactive)
         (find-file ,dir)))
    (insert (format "%s%s[%-3s %5s %4s] %s"
                    (propertize (concat (make-string level ?*) " ")
                                'invisible t)
                    (make-string (* (1- level) 4) ? )
                    type rev states
                      (propertize (if title title (abbreviate-file-name dir))
                                'mouse-face 'highlight
                                'keymap map
                                'help-echo
(format "%s\n%s\n[mouse-2] Open with dired." root-name dir)
                                'obuffer-root t
                                'obuffer-file dir)))
    (add-text-properties (line-beginning-position)
                         (line-end-position)
                         `( obuffer-dir ,dir))
    (insert "\n")))


(defun obuffer-create ()
  ;; (obuffer-mode)
  (let* (( inhibit-read-only t)
         ( roots (obuffer-import-project-roots))
         ( sort-column obuffer-current-sort-column)
         ( project-switch (obuffer-get-file-buffers roots))
         ( projects (nth 0 project-switch))
         ( non-project-files (nth 1 project-switch))
         ( non-file-buffers (nth 2 project-switch))
         ( hidden-buffers (nth 3 project-switch))
         ( project-buffers (obuffer-project-buffers
                            roots non-file-buffers))
         ( i 0)
         closed)
    (setq non-file-buffers (pop project-buffers))
    (erase-buffer)
    (dolist ( root roots)
      (setq i (+ i 1))
      (let* (( dir (nth 0 root))
             ( project-state (nth 5 root))
             ( files (cdr (assoc dir projects)))
             ( buffers (cdr (assoc dir project-buffers)))
             ( map (make-sparse-keymap)))
        (if (string= project-state "closed")
            (setq closed (cons root closed))
          (obuffer-insert-header root)
          (insert "\n")
          (obuffer-insert-section-files files dir nil 'project)
          (obuffer-insert-section-files buffers nil 9))
        (when (or files buffers)
          (insert "\n"))))
    (insert (format "* [%4s Non-project files]\n" (length non-project-files)))
    (obuffer-insert-section-files non-project-files)
    (insert (format "\n* [%4s Non-file buffers]\n" (length non-file-buffers)))
    (obuffer-insert-section-files non-file-buffers nil 9)
    (insert (format "\n* [%4s Closed projects]\n\n" (length closed)))
    (dolist ( root closed)
      (obuffer-insert-header root)
      (insert "\n")))
  (set-buffer-modified-p nil))


;;*** Function for brief description by readme files


(defun obuffer-directory-brief ( dir &optional readme)
  (let (( file (expand-file-name
                (concat (file-name-as-directory dir)
                        (or readme "README.md")))))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (markdown-mode)
        (goto-char (point-min))
        (unless (markdown-heading-at-point)
          (markdown-next-heading))
        (looking-at "^\\(#+\s\\)?\\(.*\\)$")
        (match-string 2)))))


(defun obuffer-edit-directory-brief ()
  (interactive)
  (save-excursion
    (end-of-line)
    (outline-previous-heading)
    (find-file (concat (file-name-as-directory
                        (get-text-property (point) 'obuffer-dir))
                       "README.md"))))


;;*** Function for closing a project

(defun obuffer-close-project ()
  (interactive)
  (save-excursion
    (end-of-line)
    (outline-previous-heading)
    (write-region "" nil (concat (file-name-as-directory
                                  (get-text-property (point)
                                                     'obuffer-dir))
                                 ".closed"))))


;;*** Function for VC controls

(defun obuffer-vc-working-directory ()
  (get-text-property (save-excursion
                       (end-of-line)
                       (outline-previous-heading)
                       (point))
                     'obuffer-dir))

(defun obuffer-vc-dir ()
  (interactive)
  (vc-dir (obuffer-vc-working-directory)))
  

;;*** Function for interactive opening


(defun restore-window-line ( window-line &optional pos window)
  (let (( delta (- (cdr (nth 6 (posn-at-point pos window)))
                   window-line)))
    (if (> delta 0)
        (scroll-up-line delta)
      (scroll-down-line (abs delta)))))


(defun obuffer-update ()
  (interactive)
  (save-match-data
    (let* (
           ;; ( inhibit-modification-hooks t)
           ( inhibit-read-only t)
           ( buffer (get-buffer "*Obuffer*"))
           ( windows (get-buffer-window-list buffer))
           window-line win-pos)
      (when  (and buffer windows)
        (dolist ( win windows)
          (with-selected-window win
            (redisplay)
            (setq window-line (cdr (nth 6 (posn-at-point)))
                  win-pos (cons (list win (point) window-line) win-pos))))
        (with-selected-window (car windows)
          (outline-save-fold-state)
          (obuffer-create)
          (outline-restore-fold-state))
        (dolist ( wp win-pos)
          (with-selected-window (nth 0 wp)
            (goto-char (nth 1 wp))
            (redisplay)
            (restore-window-line (nth 2 wp))))))))


(defun obuffer-open ()
  (interactive)
  (save-match-data
    (let* (
           ;; ( inhibit-modification-hooks t)
           ( inhibit-read-only t)
           ( switch-to-buffer-preserve-window-point nil)
           ( exists (get-buffer "*Obuffer*"))
           ( buffer (get-buffer-create "*Obuffer*")))
      (switch-to-buffer buffer)
      ;; (select-window (display-buffer buffer))
      ;; (unless (eq major-mode 'obuffer-mode)
      ;;   (switch-to-buffer-other-frame buffer))
      (if exists
          (obuffer-update)
        (obuffer-create)
        (obuffer-mode)
        (outline-hide-sublevels 1)
        (goto-char (point-min))))))


(defun obuffer-use-other-window ()
  (interactive)
  (when (< (count-windows) 2)
    (split-window-sensibly))
  (other-window 1)
  (obuffer-open))


;;*** Function for fontification


(defun obuffer-fontify-file-entry ( limit)
  (let* (( anchor (if (and (= (point) (line-beginning-position))
                           (not (bobp)))
                      (1- (point))
                    (point)))
         ( beg (next-single-property-change
                anchor 'obuffer-file nil limit))
         ( inhibit-read-only t))
    (when beg
      (goto-char beg)
      (when (get-text-property beg 'obuffer-file)
        (let (( rootp (get-text-property beg 'obuffer-root))
              ( end (goto-char
                     (or (when (re-search-forward "<" (line-end-position) t)
                           (1- (point)))
                         (next-single-property-change beg 'obuffer-file)))))
          (unless rootp
            (put-text-property beg end 'face 'bold)))))
    t))


;;*** Function for user interaction


(defun obuffer-set-control-key ( key)
  (beginning-of-line)
  (let (
        ;; ( inhibit-modification-hooks t)
        ( inhibit-read-only t))
    (if (outline-on-heading-p)
        (obuffer-heading-execute key)
      (unless (looking-at "$")
        (delete-char 1)
        (insert key))
      (beginning-of-line 2))))


(defun obuffer-delete ()
  (interactive)
  (obuffer-set-control-key "D"))


(defun obuffer-copy ()
  (interactive)
  (obuffer-set-control-key "C"))


(defun obuffer-remove-file ()
  (interactive)
  (obuffer-set-control-key "R"))


(defun obuffer-move ()
  "Move file."
  (interactive)
  (obuffer-set-control-key "M"))


(defun obuffer-back-to-disk-version ()
  "Revert buffer."
  (interactive)
  (obuffer-set-control-key "B"))


(defun obuffer-revert-buffer ( buffer)
  "Revert buffer to version saved on disk.

BUFFER needs to be an buffer visiting an existing file on disk."
  (with-current-buffer buffer
    (when (and (buffer-file-name)
               (file-exists-p (buffer-file-name)))
      (revert-buffer t t t))))


(defun obuffer-unmark ()
  "Remove mark."
  (interactive)
  (beginning-of-line)
  (let (( inhibit-read-only t))
    (delete-char 1)
    (unless (looking-at "$")
      (insert " ")))
  (beginning-of-line 2))


(defun obuffer-target-directory-ask ( dir &optional create-new)
  (setq dir (expand-file-name
             (read-file-name "Target directory: " dir)))
  (if (file-exists-p dir)
      (if (file-directory-p dir)
          dir
        (message "%s is not a directory." dir)
        nil)
    (when create-new
      (make-directory dir 'make-parents))
    dir))


(defun obuffer-relocate-file ( old-file new-file key &optional no-confirm)
  "Move file with obuffer key 'M'.

OLD-FILE is the existing file listed in obuffer.  NEW-FILE is a
copy of OLD-FILE.  KEY is the current control key.  NO-CONFIRM
can be used to enable confirmation for the move."
  (when (file-exists-p new-file)
    (find-file-noselect new-file)
    (when (and (string= key "M")
               (or no-confirm
                   (y-or-n-p (format "Kill and delete file %s? " old-file))))
      (obuffer-mv-symlinks old-file new-file)
      (kill-buffer (get-file-buffer old-file))
      (if (file-exists-p old-file)
          (delete-file old-file 'trash)))))


(defun obuffer-delete-frames ( buffer)
  (when (> (length (frames-on-display-list)) 1)
    (dolist ( frame (frames-on-display-list))
      (when (get-buffer-window buffer frame)
        (delete-frame frame)))))


(defun obuffer-execute ()
  (interactive)
  (let (( pos (point))
        ( pos-dir (get-text-property (point) 'obuffer-dir))
        ( window-line (cdr (nth 6 (posn-at-point))))
        pos-dir-done)
    (goto-char (point-min))
    (while (re-search-forward "^[DCRMB]" nil t)
      (let* (( key (match-string 0))
             ( dir (get-text-property (point) 'obuffer-dir))
             ( buffer (get-text-property (point) 'obuffer-buffer))
             ( file-name (get-text-property (point)
                                            'obuffer-file-name))
             ( file (concat dir file-name)))
        (cond ((or (string= key "D") (string= key "R"))
               (when buffer
                 (with-current-buffer buffer
                   (set-buffer-modified-p nil))
                 (obuffer-delete-frames buffer)
                 (kill-buffer buffer))
               (when (and (string= key "R")
                          (y-or-n-p
                           (format "Permanently remove file %s? "
                                   file)))
                 (delete-file file 'trash)))
              ((or (string= key "C") (string= key "M"))
               (unless pos-dir-done
                 (setq pos-dir (obuffer-target-directory-ask pos-dir t)
                       pos-dir-done t))
               (let (( new-file (concat (file-name-as-directory pos-dir)
                                        file-name)))
                 (copy-file file new-file 1) ; Request confirmation
                 (obuffer-relocate-file file new-file key
                                        'no-confirm)))
              ((string= key "B")
               (obuffer-revert-buffer buffer)))))
    (obuffer-update)))


(defun obuffer-recursive-relocate-files ( old-dir new-dir key)
  (when (file-directory-p new-dir)
    (dolist (buffer (buffer-list))
      (let (( file (buffer-file-name buffer)))
        (when (and file (string-match (regexp-quote old-dir) file))
            (let (( new-file (expand-file-name
                              (concat (file-name-as-directory new-dir)
                                      (substring file (match-end 0))))))
              (obuffer-relocate-file file new-file key 'no-confirm)))))))


(defun obuffer-heading-execute ( key)
  (let (( dir (directory-file-name
               (get-text-property (point) 'obuffer-dir))))
    (when (string= key "M")
      (let (( new-dir (directory-file-name
                       (obuffer-target-directory-ask dir))))
        (unless (string= dir new-dir)
          (when (y-or-n-p (format "Move directory %s to %s? " dir new-dir))
            (copy-directory dir new-dir 1)
            (delete-directory dir 'recursive 'trash)
            (obuffer-recursive-relocate-files dir new-dir key)))))))


(defun obuffer-get-root-directory ()
  "Find path of root directory for current project.

Current project is the heading or body where point is located."
  (save-excursion
    (unless (outline-on-heading-p)
      (outline-previous-heading))
    (get-text-property (point) 'obuffer-dir)))


(defun obuffer-find-file ()
  (interactive)
  (let (( dir (or (get-text-property (point) 'obuffer-dir)
                  (progn (outline-previous-heading)
                         (get-text-property (point) 'obuffer-dir))
                  (expand-file-name "~"))))
    (find-file
     (read-file-name "Find file: " (file-name-as-directory dir))))
  (obuffer-update))


(defun obuffer-open-modified ()
  "According to VC status open all added or modified files."
  (interactive)
  (let ((default-directory (obuffer-get-root-directory)))
    (dolist ( stat-line (split-string
                         (shell-command-to-string
                          (concat "hg status"))
                         "\n" t))
      (string-match "^[^\s]+\s+\\(.*\\)" stat-line)
      (find-file-noselect (expand-file-name (match-string 1 stat-line)))))
  (obuffer-update))


(defun obuffer-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (get-text-property (point) 'obuffer-buffer)))


(defun obuffer-open-at-mouse ( event)
  (interactive "e")
  (let (( buffer-name (mouse-posn-property (event-start event)
                                           'buffer-name))
        ( dir-name (mouse-posn-property (event-start event)
                                        'dir-name)))
    (if buffer-name
        (switch-to-buffer buffer-name)
        ;; (switch-to-buffer-other-frame buffer-name)
      ;;(find-file-other-frame dir-name)
      (find-file dir-name))))


(defun obuffer-open-at-mouse-other-window ( event)
  (interactive "e")
  (let (( buffer-name (mouse-posn-property (event-start event)
                                           'buffer-name))
        ( dir-name (mouse-posn-property (event-start event)
                                        'dir-name)))
    (if buffer-name
        (switch-to-buffer-other-window buffer-name)
        ;; (switch-to-buffer-other-frame buffer-name)
      ;;(find-file-other-frame dir-name)
      (find-file-other-window dir-name))))


;;*** Function for global key remaps


(defun obuffer-undo ( &optional arg)
  (interactive "*P")
  (undo arg)
  (unless (buffer-modified-p)
    (obuffer-update)))
 

(defun obuffer-dired ()
  (interactive)
  (dired (get-text-property (point) 'obuffer-dir)))


(defun obuffer-dired-two-pan ()
  (interactive)
  (let (( dir (get-text-property (point) 'obuffer-dir))
        ( file (get-text-property (point) 'obuffer-file-name)))
    (dired-fullscreen-two-pan-view dir)
    (goto-char (point-min))
    (re-search-forward file nil t)))


;;*** Hooks


;; (defun buffer-modified-after-change ( a b c)
;;   (remove-hook 'after-change-functions 'buffer-modified-after-change)
;;   (obuffer-update))


;; (add-hook 'first-change-hook
;;   (lambda ()
;;     ;; We need another change hook. first-change-hook is evaluated
;;     ;; before switching buffer-modified flag, which will not give the
;;     ;; desired condition.
;;     (when buffer-file-name
;;       (add-hook 'after-change-functions 'buffer-modified-after-change))))

;; (add-hook 'after-save-hook 'obuffer-update)


(defvar obuffer-auto-buffers-changed nil)


(define-minor-mode obuffer-auto-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  nil " OA" nil
  (unless (derived-mode-p 'obuffer-mode)
    (error "This buffer is not in Ibuffer mode"))
  (cond ( obuffer-auto-mode
         (frame-or-buffer-changed-p 'obuffer-auto-buffers-changed) ; Initialize state vector
         (add-hook 'post-command-hook 'obuffer-auto-update-changed))
        (t
         (remove-hook 'post-command-hook 'obuffer-auto-update-changed))))


(defun obuffer-auto-update-changed ()
  (when (frame-or-buffer-changed-p 'obuffer-auto-buffers-changed)
    (dolist ( buf (buffer-list))
      (ignore-errors
	    (with-current-buffer buf
	      (when (and obuffer-auto-mode
		             (derived-mode-p 'obuffer-mode))
	        (obuffer-update)))))))


(defun obuffer-buffer-changed-p ()
  (let ((index 0)
        item changed)
    (while (and (< index (length obuffer-auto-buffers-changed))
                (not changed))
      (if (not (buffer-live-p (aref obuffer-auto-buffers-changed index)))
          (setq index (+ index 1))
        (setq index (+ index 2))
        (if (aref obuffer-auto-buffers-changed index)
            (setq changed t)
          (setq index (+ index 1)))))
    changed))


;;*** Major mode definition

(setq info-index-match 'info-index-match)
(setq show-paren-mismatch 'show-paren-mismatch)
(setq trailing-whitespace 'trailing-whitespace)
(setq fringe 'fringe)


(define-derived-mode obuffer-mode outline-mode "OBuffer"
    "Major mode for listing emacs buffers."
    (unless (eq (current-buffer) (get-buffer "*Obuffer*"))
      (error "Obuffer: current buffer is no OBuffer"))
    (setq truncate-lines t)
    (obuffer-header-line)
    ;; (obuffer-auto-mode 1)
    (define-key obuffer-mode-map "d" 'obuffer-delete)
    (define-key obuffer-mode-map "r" 'obuffer-remove-file)
    (define-key obuffer-mode-map "c" 'obuffer-copy)
    (define-key obuffer-mode-map "b" 'obuffer-back-to-disk-version)
    (define-key obuffer-mode-map "m" 'obuffer-move)
    (define-key obuffer-mode-map "u" 'obuffer-unmark)
    (define-key obuffer-mode-map "x" 'obuffer-execute)
    (define-key obuffer-mode-map "g" 'obuffer-update)
    (define-key obuffer-mode-map "G" 'obuffer-run-root-info)
    (define-key obuffer-mode-map "S" 'obuffer-run-symlink-info)
    (define-key obuffer-mode-map "e" 'obuffer-edit-directory-brief)
    (define-key obuffer-mode-map (kbd "C-c c") 'obuffer-close-project)
    (define-key obuffer-mode-map "\C-x\C-f" 'obuffer-find-file)
    (define-key obuffer-mode-map (kbd "RET") 'obuffer-switch-to-buffer)
    (define-key obuffer-mode-map (kbd "C-x v d") 'obuffer-vc-dir)
    (define-key obuffer-mode-map "\C-xd" 'obuffer-dired)
    (define-key obuffer-mode-map "\C-xx" 'obuffer-dired-two-pan)
    (define-key obuffer-mode-map (kbd "<mouse-2>") 'obuffer-open-at-mouse)
    (define-key obuffer-mode-map (kbd "<mouse-3>")
      'obuffer-open-at-mouse-other-window)
    (setq font-lock-extra-managed-props nil)
    (font-lock-add-keywords nil
      '(;; (obuffer-fontify-file-entry 0 nil prepend t)
        ;; ("<.*?\\(>\\|\\.\\.\\.\\)" 0 font-lock-warning-face)
        ("edited" 0 font-lock-variable-name-face)
        ("added" 0 font-lock-keyword-face)
        ("\s\\(\s*OK\\)\\]" 1 info-index-match t)
        ("\s\\(\s*[MAD]*\\)\\]" 1 show-paren-mismatch t)
        ("\s\\(\s*[MAD]*[?!R]+[MAD]*\\)\\]" 1 font-lock-warning-face t)
        ("up-to-date" 0 font-lock-comment-face)
        ("\\*\\(Help\\|info\\)\\*" 0 font-lock-comment-face prepend t)
        ("\\*\\(Messages\\|scratch\\|Obuffer\\|Ibuffer\\|Buffer List\\)\\*"
         0 font-lock-keyword-face prepend t)
        ("^[\s%]+\\(.*?\\)\s+[0-9]+\s+dired"
         1 font-lock-function-name-face prepend t))
      'append)
    (read-only-mode 1))




(provide 'obuffer)


;;; obuffer.el ends here
