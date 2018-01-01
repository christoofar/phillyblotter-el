(defun phillyblotter-fetch-json (url)
   (setq phillyblotter-json-data (with-current-buffer (url-retrieve-synchronously url)
     ; there's probably a better way of stripping the headers
     (beginning-of-buffer)
     (search-forward "\n\n")
     (delete-region (point-min) (point))
     (message (buffer-string))
     (json-pretty-print-buffer)
     (beginning-of-buffer)
     (json-read)
     ))
   )

;;(fetch-json "http://www.philadelinquency.com/phillycrime/api/Blotter/34/")

;; (let* ((dcn (mapcar (lambda (item) (assoc-default 'DCN item)) phillyblotter-json-data))
;;        (occurred (mapcar (lambda (item) (assoc-default 'Occurred item)) phillyblotter-json-data))
;;        )
;;   )

(defun phillyblotter-calculate-age(birthdate)
  "Given a date string for BIRTHDATE, return the age of that person in years"
     (let* ((begindate (parse-time-string (current-time-string))) ;; "Sat Dec 4..."
	 (enddate (parse-time-string (phillyblotter/xah-fix-datetime-stamp birthdate))))  ;; USian format, but it can be ISO8601

  ;; This is the idea, say current date is         20171224
  ;;              .. and the other date is     < - 19700513>
  ;;                                               --------
  ;;         Using decimal subtraction you get:      470711
  ;;                                        Or:    47y 07m 11d

  ;; xah-fix-datetime-stamp is here -> http://ergoemacs.org/emacs/elisp_parse_time.html
  ;; Everything else is in (require 'parse-time)

       ;; Build the string to do date math on
       (let* ((begindateformat (concat (number-to-string (nth 5 begindate))
				       (format "%02d" (nth 4 begindate))
				       (format "%02d" (nth 3 begindate))))
	 (enddateformat (concat (number-to-string (nth 5 enddate))
				(format "%02d" (nth 4 enddate))
				(format "%02d" (nth 3 enddate))))
	 (age (number-to-string (- (string-to-number begindateformat) (string-to-number enddateformat)))))

	 ;; Magic!
	 (string-to-number (substring age 0 (- (length age) 4))))
  )
)

(defun phillyblotter-get-neighborhood(id)
  "Returns the neighborhood name for a given neighborhood ID number"
  (interactive "nEnter the neighborhood ID number: ")

  (if (not (boundp 'phillyblotter-neighborhoods))
      (progn
        (phillyblotter-fetch-json "https://www.philadelinquency.com/phillycrime/api/Neighborhood/")
        (setq phillyblotter-neighborhoods phillyblotter-json-data)	
      )
  )

  (let* ((hood (mapcar (lambda (item) (assoc-default 'Name item)) phillyblotter-neighborhoods))
         (ID (mapcar (lambda (item) (assoc-default 'ID item)) phillyblotter-neighborhoods)))

    (catch 'complete
    (mapcar* (lambda (ihood iID)
      (if (= iID id)
    	    (throw 'complete ihood))
      ) hood ID)
      nil
    )
    
  )
)

(defun phillyblotter-display-blotter (neighborhood)
  "Displays the neighborhood blotter given a neighborhood ID number"
  (interactive "sEnter the neighborhood ID number: ")
  (phillyblotter-fetch-json (concat "https://www.philadelinquency.com/phillycrime/api/Blotter/" neighborhood "/"))
  (let* ((dcn (mapcar (lambda (item) (assoc-default 'DCN item)) phillyblotter-json-data))
	 (occurred (mapcar (lambda (item) (assoc-default 'Occurred item)) phillyblotter-json-data))
	 (long (mapcar (lambda (item) (assoc-default 'Longitude item)) phillyblotter-json-data))
	 (lat (mapcar (lambda (item) (assoc-default 'Latitutde item)) phillyblotter-json-data))
	 (address (mapcar (lambda (item) (assoc-default 'Address item)) phillyblotter-json-data))
	 (title (mapcar (lambda (item) (assoc-default 'Title item)) phillyblotter-json-data))
	 (type (mapcar (lambda (item) (assoc-default 'Type item)) phillyblotter-json-data))
	 (code (mapcar (lambda (item) (assoc-default 'Code item)) phillyblotter-json-data))
	 (arrestcount (mapcar (lambda (item) (assoc-default 'ArrestCount item)) phillyblotter-json-data))
	 )

    (switch-to-buffer "*Blotter*")
    
    (insert "\n")
    (insert (concat "\t\t10-Day Neighborhood blotter for " (phillyblotter-get-neighborhood (string-to-number neighborhood)) "\n"))
    (insert (propertize "      Occurred                Crime                         Address                     Arrest\n" 'font-lock-face '(:foreground "red")))

    (org-mode)

    (defvar-local count 0)
    (mapcar* (lambda (dcn occurred title address arrestcount)
	       (setq-local count (1+ count))
	       (widget-create 'push-button (propertize (concat (number-to-string count) ".") 'font-lock-face '(:foreground "blue"))
			      :notify (lambda (&rest ignore)
					(alert "heh")))
	       (insert "  ")  
	       (widget-insert (format "%s                   " occurred))
	       (move-to-column 30)
	       (widget-insert (format "%s                   " title))
	       (move-to-column 60)
	       (widget-insert (format "%s                   " address))
	       (move-to-column 88)
	       (if (= arrestcount 1)
		   (widget-create 'link "1 arrest"
				  :notify (lambda (&rest ignore)
					    (alert "test"))
		     )
		 )
	       (if (> arrestcount 1)
		   (widget-insert (format "%s arrests" arrestcount))
	         )
	       (delete-trailing-whitespace)
	       (insert "\n")
	       ) dcn occurred title address arrestcount)
    (insert "\n\n")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (kill-buffer)
				   )
			 "Close")
    (beginning-of-buffer)
    ;;(org-cycle)
    (use-local-map widget-keymap)
    (widget-setup)
    )
  )

(defun phillyblotter/xah-fix-datetime-stamp (@input-string &optional @begin-end)
  "Change timestamp under cursor into a yyyy-mm-dd format.
If there's a text selection, use that as input, else use current line.

Any “day of week”, or “time” info, or any other parts of the string, are discarded.
For example:
 「TUESDAY, FEB 15, 2011 05:16 ET」 ⇒ 「2011-02-15」
 「November 28, 1994」              ⇒ 「1994-11-28」
 「Nov. 28, 1994」                  ⇒ 「1994-11-28」
 「11/28/1994」                     ⇒ 「1994-11-28」
 「1994/11/28」                     ⇒ 「1994-11-28」

When called in lisp program, the optional second argument “*begin-end” is a vector of region boundary. (it can also be a list)
If “*begin-end” is non-nil, the region is taken as input (and “*input-string” is ignored).

URL `http://ergoemacs.org/emacs/elisp_parse_time.html'
Version 2015-04-14"

  (interactive
   (list nil (vector (line-beginning-position) (line-end-position))))

  (let (
        ($str (if @begin-end (buffer-substring-no-properties (elt @begin-end 0) (elt @begin-end 1)) @input-string))
        ($work-on-region-p (if @begin-end t nil)))
    (require 'parse-time)

    (setq $str (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" $str)) ; remove white spaces

    (setq $str
          (cond
           ;; USA convention of mm/dd/yyyy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" $str)
            (concat (match-string 3 $str) "-" (match-string 1 $str) "-" (match-string 2 $str)))
           ;; USA convention of m/dd/yyyy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" $str)
            (concat (match-string 3 $str) "-0" (match-string 1 $str) "-" (match-string 2 $str)))

           ;; USA convention of mm/dd/yy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" $str)
            (concat (format-time-string "%C") (match-string 3 $str) "-" (match-string 1 $str) "-" (match-string 2 $str)))
           ;; USA convention of m/dd/yy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" $str)
            (concat (format-time-string "%C") (match-string 3 $str) "-0" (match-string 1 $str) "-" (match-string 2 $str)))

           ;; yyyy/mm/dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" $str)
            (concat (match-string 1 $str) "-" (match-string 2 $str) "-" (match-string 3 $str)))

           ;; some ISO 8601. yyyy-mm-ddThh:mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T[0-9][0-9]:[0-9][0-9]" $str)
            (concat (match-string 1 $str) "-" (match-string 2 $str) "-" (match-string 3 $str)))
           ;; some ISO 8601. yyyy-mm-dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)" $str)
            (concat (match-string 1 $str) "-" (match-string 2 $str) "-" (match-string 3 $str)))
           ;; some ISO 8601. yyyy-mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)" $str)
            (concat (match-string 1 $str) "-" (match-string 2 $str)))

           ;; else
           (t
            (progn
              (setq $str (replace-regexp-in-string "January " "Jan. " $str))
              (setq $str (replace-regexp-in-string "February " "Feb. " $str))
              (setq $str (replace-regexp-in-string "March " "Mar. " $str))
              (setq $str (replace-regexp-in-string "April " "Apr. " $str))
              (setq $str (replace-regexp-in-string "May " "May. " $str))
              (setq $str (replace-regexp-in-string "June " "Jun. " $str))
              (setq $str (replace-regexp-in-string "July " "Jul. " $str))
              (setq $str (replace-regexp-in-string "August " "Aug. " $str))
              (setq $str (replace-regexp-in-string "September " "Sep. " $str))
              (setq $str (replace-regexp-in-string "October " "Oct. " $str))
              (setq $str (replace-regexp-in-string "November " "Nov. " $str))
              (setq $str (replace-regexp-in-string "December " "Dec. " $str))

              (setq $str (replace-regexp-in-string "\\([0-9]+\\)st" "\\1" $str))
              (setq $str (replace-regexp-in-string "\\([0-9]+\\)nd" "\\1" $str))
              (setq $str (replace-regexp-in-string "\\([0-9]+\\)rd" "\\1" $str))
              (setq $str (replace-regexp-in-string "\\([0-9]\\)th" "\\1" $str))

              (let (dateList $year $month $date $yyyy $mm $dd )
                (setq dateList (parse-time-string $str))
                (setq $year (nth 5 dateList))
                (setq $month (nth 4 dateList))
                (setq $date (nth 3 dateList))

                (setq $yyyy (number-to-string $year))
                (setq $mm (if $month (format "%02d" $month) "" ))
                (setq $dd (if $date (format "%02d" $date) "" ))
                (concat $yyyy "-" $mm "-" $dd))))))

    (if $work-on-region-p
        (progn (delete-region  (elt @begin-end 0) (elt @begin-end 1))
               (insert $str))
      $str )))

(defun phillyblotter()
  "Launches PhillyBlotter"
  (interactive)
   (phillyblotter-fetch-json "https://www.philadelinquency.com/phillycrime/api/Neighborhood/")
   (let* ((names (mapcar (lambda (item) (assoc-default 'Name item)) phillyblotter-json-data))
          (ids (mapcar (lambda (item) (assoc-default 'ID item)) phillyblotter-json-data)))
          (switch-to-buffer "*Neighborhood List*")
          (insert "Neighborhood Selection: \n\n")
          ;;(org-mode)
          ;; (insert "[[elisp:(find-function 'describe-function)][describe-function]]\n\n")
          ;;(insert "| Neighborhood | ID |\n")
          ;;(insert "|-----\n")
          (mapcar* (lambda (name id)
   		  ;;(message "id: %s  name: %s" name id)
   		     ;;(insert (format "| %s | %s |\n" name id))
		     (widget-create 'link (format "%s" name))
		     (insert "\n")
   		   )		   
   		   names ids)
	  ;;(org-table-align)
          ;;(org-mode)
	  (insert "\n\n")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (kill-buffer)
				   )
			 "Close")
          (beginning-of-buffer)
	  (use-local-map widget-keymap)
	  (widget-setup)
         )
   )

(defun phillyblotter-display-crime(crimeid)
  "Displays a crime given the DCN number"
  (interactive "sEnter Philadelphia Police DCN number:")
  (phillyblotter-fetch-json (concat "https://www.philadelinquency.com/phillycrime/api/Crime/" crimeid "/"))

  (let* ((dcn (assoc-default 'DCN phillyblotter-json-data))
	 (fullcrimedetail (assoc-default 'FullCrimeDetail phillyblotter-json-data))
	 (fullarrestdetail (assoc-default 'FullArrestDetails phillyblotter-json-data)))

    (switch-to-buffer "*Crime Detail*")

    (widget-insert "┍━━━━━━━━━━━━━━━━━━━━━━━┑\n")
    (widget-insert "│ Crime Details         │\n")
    (widget-insert "┕━━━━━━━━━━━━━━━━━━━━━━━┙\n\n")
    (mapc (lambda (x)
	    (widget-insert (format "%s, %s  DOB: %s" (alist-get 'Defendant x) (concat "(" (number-to-string (phillyblotter-calculate-age (phillyblotter/xah-fix-datetime-stamp (alist-get 'DateOfBirth x)))) ")") (alist-get 'DateOfBirth x)))
	    (insert "\n")
	    (insert "\n")
	    (org-mode)
	    (insert "| Statute | Charge |\n")
	    (insert "|--------\n")
	    (mapc (lambda (charge)
		    (insert (format "| %s | %s |\n" (replace-regexp-in-string "Â" "" (alist-get 'Statute charge)) (alist-get 'Description charge)))
		    ) (assoc-default 'Charges x))
	    (org-table-align)
	    (org-mode)
	    )
	fullarrestdetail)

	(insert "\n\n")

	;;	(widget-insert (format "DCN: %s            Type: %s" dcn (alist-get 'TEXT_GENERAL_CODE fullcrimedetail)))
	(insert "\n")

	(widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-buffer)
			     )
		   "Close")
	(use-local-map widget-keymap)
	(widget-setup)
    )
 )
