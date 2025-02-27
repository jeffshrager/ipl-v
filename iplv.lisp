;;; (load (compile-file "iplv.lisp"))

;;; Last commit confirmed to run F1 correctly is ebc4887 (~20250228@0919)

;;; The whole symbol v. cell thing in IPL is a compelete mess. All
;;; symbols can be addresses of cells -- in fact they all are -- but
;;; sometimes they are treated as their character representation, and
;;; sometimes they are treated as the cell, and the symbol the author
;;; means is the symbol in the cell pointed to by the symbol in
;;; hand. And then there are the special symbols (H0, etc) that have a
;;; special push down mechanism. Ugh.

;;; WWW Leaves these at high debug etc or things break for unknown reasons.
(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0) (compilation-speed 0)))

(defstruct (cell (:print-function print-cell))
  (comments "")
  (type "")
  (name "")
  (sign "")
  (pq "")
  (symb "")
  (link "")
  (comments.1 "")
  (id "")
  )

(defun new-symb-cell (symbol &optional (prefix "c"))
  (make-cell :name (symbol-name (gensym prefix)) :symb symbol))

(defparameter *symbol-col-accessors* `((cell-name . ,#'cell-name) (cell-symb . ,#'cell-symb) (cell-link . ,#'cell-link)))

(defun zero? (what)
  (member (if (stringp what) what
	      (cell-symb what))
	  '("" "0") :test #'string-equal))

(defun print-cell (cell s d)
  (declare (ignore d))
  (format s "{~a~a/~a/~a/~a~a}"
	  (if (zero? (cell-id cell)) "" (format nil "~a::" (cell-id cell)))
	  (cell-name cell)
	  (cell-pq cell)
	  (cell-symb cell)
	  (cell-link cell)
	  (if (and (zero? (cell-comments cell)) (zero? (cell-comments.1 cell))) 
	      ""
	      (format nil " [~a/~a]" (cell-comments cell) (cell-comments cell)))))

;;; ===================================================================
;;; Storage and Special Symbols

(defvar *symtab* (make-hash-table :test #'equal))

;;; *cell is symbol value for stacked symbols, like H0 and W0, used where there
;;; isn't a special macro for common ones.  WWW Note the convention of adding +
;;; when the var has the whole stack. System symbols (machine stacks) are
;;; strings just like user-defined symbols. It's up to the user to ot try to
;;; push/pop things that aren't stacks!

(defmacro cell (symb) `(gethash ,symb *symtab*))
(defmacro stack (symb) `(gethash ,symb *systacks*)) ;; Only system cells have stacks

;;; Important values it have special macros (these are like (H0) = (0)
;;; in the IPL-V manual). The ...+ fns return the whole stack. (Note
;;; that you'll have to get (1), that is, the second stack entry in H0
;;; manually!)

(defmacro H0 () `(cell "H0"))
(defmacro H0+ () `(stack "H0"))

(defmacro H1 () `(cell "H1")) ;; WWW DO NOT CONFUSE H1 with (1) !!!
(defmacro H1+ () `(stack "H1")) ;; WWW DO NOT CONFUSE H1 with (1) !!!

(defmacro H5 () `(cell "H5"))
(defmacro H5+ () `(stack "H5"))

(defmacro S () `(cell "S"))
(defmacro S+ () `(stack "S"))

(defun cell? (cell?)
  (eq 'cell (type-of cell?)))

;;; This is a protected version of cell-name that de-refs if necessary.

(defun cell-name% (cell-or-name)
  (cell-name (drod cell-or-name)))

(defvar *trace-instruction* nil)

(defun drod (cell-or-name) ;; de-ref-or-die
  (let ((cell (if (cell? cell-or-name) cell-or-name
		  (if (stringp cell-or-name) (cell cell-or-name)))))
    (if (cell? cell) cell
      (break "Trying to deref ~s, which isn't a cell, while executing ~s!" cell-or-name *trace-instruction*))))

;;; ===================================================================
;;; Debugging Utils

(defvar *!!list* nil) ;; t for all, or: :load :run :run-full

(defun !! (key fmt &rest args)
  ;; WWW if the arg is actually nil, apply gets confused so we pre-fix this case.
  (unless args (setf args '(())))
  (when (or (equal *!!list* t)
	    (equal key t)
	    (member key *!!list*))
    (apply #'format t fmt args)
    (when (and (member key '(:run :run-full)) (member :run-full *!!list*))
      (report-system-cells))))

;;; This also checks to make sure that there isn't crap left on the
;;; stacks or in the cells and breaks if ther is. 

(defun report-system-cells ()
  (format t "~%~%------ RUN REGISTERS ------~%")
  (loop for cellname in *system-cells*
	do (format t "  ~a=~s ~s~%" cellname (cell cellname) (stack cellname)))
  (format t "-----------------------~%~%")
  ;; Check for disasters
  (loop for cellname in *system-cells*
	with break = nil
	as cell = (cell cellname)
	as stack = (stack cellname)
	do (cond ((illegal-value? cell) (format t "!!!!! ~s contains a zero or blank !!!!!~%" cellname) (setf break t))
		 ((loop for entry in stack if (illegal-value? entry) do (return t))
		  (format t "!!!!! An entry in ~s's stack is zero or blank !!!!!~%" cellname) (setf break t))
		 )
	finally (when break (break "--------------> Executing: ~s :: This shouldn't happen!" *trace-instruction*))))

(defun illegal-value? (val) ;; Might be other conditions.
  (or (null val) (and (stringp val) (string-equal val ""))))

;;; ===================================================================
;;; Loader (loads from files converted by tsv2lisp.py)

;;; FFF Note that the dumper puts multiple header lines in (:comments :type :name
;;; :sign :pq :symb :link :comments.1 :id). Prob. need code to ignore them
;;; rather than just skipping the first line.

(defvar *col->vals* (make-hash-table :test #'equal))
(defparameter *cols* '(:comments :type :name :sign :pq :symb :link :comments.1 :id))

(defvar *input-stream* nil) 

(defun load-ipl (file &key (reset? t) (load-mode :code))
  ;; Load-mode will be :code or :data as set by the latest type=5
  ;; cell's Q: Q=0=code, Q=1=data) And if the sym entry on a type 5
  ;; cell is filled, it's an execution start cell.
  (when reset? (reset!))
  (with-open-file
      (i file)
    (setf *input-stream* i) ;; For reads inside the program executor
    (!! :load "Loading IPL file: ~s~%" file)
    ;; First line is assumed to be the header which we just check
    (if (equal *cols* (read i))
	(!! :load "Header okay!~%")
	(error "No valid header on ~s" file)
	)
    (loop for read-row = (read i nil nil)
	  with cells = nil
	  until (null read-row)
	  do
	  (let* ((p -1)
		 (cell (make-cell
			:comments (nth (incf p) read-row)
			:type (nth (incf p) read-row)
			:name (nth (incf p) read-row)
			:sign (nth (incf p) read-row)
			:pq (nth (incf p) read-row)
			:symb (nth (incf p) read-row)
			:link (nth (incf p) read-row)
			:comments.1 (nth (incf p) read-row)
			:id (nth (incf p) read-row)
			))
		 (name (cell-name cell))
	       	 )
	    ;; Collect frequency of symbol use data.
	    (loop for col in *cols* as val in read-row
		  unless (zero? val)
		  do (push val (gethash col *col->vals*)))
	    (if (zero? (cell-type cell))
		(progn 
		  (when (global-symbol? name)
		    (!! :load "Loading global name: ~s~%" name)
		    (save-cells (reverse cells) load-mode) (setf cells nil)
		    )
	      	  (push cell cells))
		(if (string-equal "5" (cell-type cell))
		    (if (global-symbol? (cell-symb cell))
			(progn
			  (format t "** Execution start at ~s **~%" (cell-symb cell))
			  (save-cells (reverse cells) load-mode)
			  (setf cells nil)
			  (run (cell-symb cell)))
			(if (member (cell-pq cell) '("1" "01") :test #'string-equal)
			    (progn
			      (save-cells (reverse cells) load-mode) (setf cells nil)
			      (!! :load "Switching to DATA load mode.~%")
			      (setf load-mode :data))
			    (if (member (cell-pq cell) '("0" "00" "") :test #'string-equal)
				(progn
				  (!! :load "Switching to CODE load mode.~%")
				  (save-cells (reverse cells) load-mode) (setf cells nil)
				  (setf load-mode :code))
				(!! :load "Ignoring: ~s~%" read-row)))))))
	  finally (save-cells (reverse cells) load-mode)
	  )))

(defun save-cells (cells load-mode)
  ;; Once we have the thing completely in hand, we change the local
  ;; symbols to FN_9-... and save those as separate symtab
  ;; entries. This allows the code to branch, and also run through,
  ;; and also use sub sections of code in J100 meta-calls (ugh!) WWW
  ;; !!! This looks like it's duplicative as each sublist contains all
  ;; the sublists after it.  However this is unfortunately required as
  ;; sometimes the code runs through. In load-mode :data we have to
  ;; assign a local symbol to every cell. (Really we could do this in
  ;; every mode since the functions are just lists, but things would
  ;; look extremely messy and the symtab would be totally full of ugly
  ;; crap -- which is, of course, how the actual computer works, where
  ;; core is the symtab! So for the sake of a bit of cleanliness we
  ;; create a spaghetti monster out of the emulator!)
  (declare (ignore load-mode)) ;; **************** WILL BE NEEDED LATER ***************
  (when cells
    (let* ((top-name (cell-name (car cells)))
	   (local-symbols.new-names
	    (uniquify-list
	     (loop for cell in cells
		   append (loop for (nil . getter) in *symbol-col-accessors*
				as symbol = (funcall getter cell)
				if (local-symbol? symbol)
				collect (cons symbol (format nil "~a-~a" top-name symbol)))))))
      (convert-local-symbols cells local-symbols.new-names)
      (setf (gethash top-name *symtab*) (car cells)) 
      (!! :load "Saved: ~s~%" (cell-name (car cells)))
      ;; Loop through the whole list and create aa local symbol for
      ;; every cell that doesn't already have one. 
      (loop for (this-cell next-cell) on cells
	    as this-link = (cell-link this-cell)
	    as next-name = (when next-cell (cell-name next-cell))
	    when next-cell ;; This usually isn't needed anyway bcs there should be a 0
	    do (if (zero? this-link)
		   (if (zero? next-name)
		       (let ((new-symbol (new-list-symbol top-name)))
			 (setf (cell-name next-cell) new-symbol)
			 (setf (cell-link this-cell) new-symbol))
		       (setf (cell-link this-cell) next-name)))))
    (store-cells cells)))

(defun new-list-symbol (&optional (prefix "")) (format nil "~a~a" prefix (gensym "+")))

(defun store-cells (l)
    (loop for cells on l
	  as name = (cell-name (car cells))
	  unless (zero? name)
	  do (setf (gethash name *symtab*) (car cells))))

(defun convert-local-symbols (cells local-symbols.new-names)
  (labels ((replace-symbols (cell accname.accessor)
	     (let ((new-name (cdr (assoc (funcall (cdr accname.accessor) cell) local-symbols.new-names :test #'string-equal))))
	       (when new-name (setf* (car accname.accessor) cell new-name)))))
    (loop for cell in cells
	  do (loop for accname.accessor in *symbol-col-accessors*
		   do (replace-symbols cell accname.accessor)))))
			    
;;; This stupidity is needed because setf doesn't know how to set a value based
;;; on an arbitrary accessor.

(defun setf* (accname cell new-name)
  (case accname
    (cell-name (setf (cell-name cell) new-name))
    (cell-symb (setf (cell-symb cell) new-name))
    (cell-link (setf (cell-link cell) new-name))))

;;; Things like 9-xxx are local, everything else is global.

(defun global-symbol? (name)
  (and (not (zerop (length name)))
       (not (char-equal #\9 (aref name 0)))))

(defun local-symbol? (name)
  (and (not (zerop (length name)))
       (char-equal #\9 (aref name 0))))

(defun uniquify-list (l)
  (loop for i on l
	unless (member (car i) (cdr i) :test #'equal)
	collect (car i)))

(defvar *jfn-plists* (make-hash-table :test #'equal))

(defun reset! ()
  (clrhash *symtab*) 
  (setup-j-fns)
  (clrhash *col->vals*)
  )

;;; Note that S and H5 are nots cells but just symbols, but they're
;;; both stackable (protectable), so they need to have stacks.

(defparameter *system-cells* '("H0" "H1" "W0" "H5" "S"))

(defvar *systacks* (make-hash-table :test #'equal))

(defun create-system-cells ()
  (loop for name in (append *system-cells* (loop for w below 43 collect (format nil "W~a" w)))
	do (setf (cell name) (make-cell :name name))
	(setf (gethash name *systacks*) (list (format nil "~a-empty" name)))
	(format t "Created system cell: ~s and its stack.~%" name))
  (setf (cell "H5") "+")
  (setf (cell "S") "S-is-null")
  )

;;; Loaded code analysis:

(defun report-col-vals ()
  (loop for col being the hash-keys of *col->vals*
	using (hash-value vals)
	collect (list col (sort (count-vals vals) #'> :key #'cdr))))

(defvar *cell->counts* (make-hash-table :test #'equal))

(defun count-vals (lst)
  (clrhash *cell->counts*)
  (dolist (item lst)
    (setf (gethash item *cell->counts*) (1+ (gethash item *cell->counts* 0))))
  (let (result)
    (maphash (lambda (key value) (push (cons key value) result)) *cell->counts*)
    result))

;;; ===================================================================
;;; J-Functions. 

;;; (WWW You's think we could pop the input args off H0 automatically,
;;; but some IPL code leave the input args in place on purpose.)

(defmacro defj (name args explanation &rest forms)
  `(let ((uname ,(string-upcase (format nil "~a" name))))
     (setf (gethash uname *jfn-plists*) '(explanation ,explanation))
     (setf (gethash uname *symtab*)
	   (lambda ,args
	     ,@forms))))

#|

("J51" . 4)
("J52" . 1)
("J53" . 1)

("J81" . 36)
("J82" . 10)

("J91" . 4)
("J92" . 1)
("J93" . 1)

????("J" . 2)????

("J100" . 26) ("J71" . 22) ("J136" . 17) ("J10" . 17)
("J155" . 17) ("J72" . 16) ("J5" . 15)
("J2" . 15) ("J11" . 15)  ("J161" . 12) ("J50" . 12) ("J160" . 11)
("J157" . 10) ("J64" . 9)  
("J116" . 6) ("J7" . 6) ("J14" . 5) ("J133" . 5) ("J18" . 5) ("J68" . 5) 
("J125" . 5) ("J124" . 5)  ("J17" . 4) ("J19" . 4) 
("J65" . 4) ("J75" . 4) ("J78" . 4)  ("J184" . 3) ("J111" . 3)
("J138" . 3) ("J137" . 3) ("J115" . 3) ("J76" . 3) 
("J130" . 2) ("J183" . 2) ("J182" . 2) ("J114" . 2)
("J80" . 2) ("J126" . 2) ("J30" . 2) ("J15" . 2)
("J166" . 2) ("J0" . 2) ("J1" . 1) ("J79" . 1)  ("J156" . 1)
("J181" . 1) ("J186" . 1) ("J62" . 1)  ("J110" . 1)
("J147" . 1) 

|#

(defun setup-j-fns ()

  (defj J2 (arg0 arg1) "TEST (0) = (1)?" (setf (h5) (if (equal arg0 arg1) "+" "-")))
  (defj J3 () "SET H5 -" (setf (H5) "-"))
  (defj J4 () "SET H5 +" (setf (H5) "+"))

  (defj J6 () "REVERSE (0) and (1)" ;; WWW H1 is not (1)
      (let ((z (H0)))
	(setf (H0) (first (H0+)))
	(setf (first (H0+)) z)))

  (defj J8 () "RESTORE H0" (^^ "H0"))

  ;; I don't think that this is necessary as we don't need to do our own GC.
  (defj J9 () "ERASE CELL (0)" (!! :jfns "J9 is a noop as we don't need to do our own GC."))

  ;; FFF Macrofiy these!!!

  (defj J20 () "MOVE(0)-(n) into WO-n [J20: n=0]" (J2n=move-0-to-n-into-w0-wn 0))
  (defj J21 () "MOVE(0)-(n) into WO-n [J21: n=1]" (J2n=move-0-to-n-into-w0-wn 1))
  (defj J22 () "MOVE(0)-(n) into WO-n [J22: n=2]" (J2n=move-0-to-n-into-w0-wn 2))
  (defj J23 () "MOVE(0)-(n) into WO-n [J23: n=3]" (J2n=move-0-to-n-into-w0-wn 3))
  (defj J24 () "MOVE(0)-(n) into WO-n [J24: n=4]" (J2n=move-0-to-n-into-w0-wn 4))
  (defj J25 () "MOVE(0)-(n) into WO-n [J25: n=5]" (J2n=move-0-to-n-into-w0-wn 5))
  (defj J26 () "MOVE(0)-(n) into WO-n [J26: n=6]" (J2n=move-0-to-n-into-w0-wn 6))
  (defj J27 () "MOVE(0)-(n) into WO-n [J27: n=7]" (J2n=move-0-to-n-into-w0-wn 7))
  (defj J28 () "MOVE(0)-(n) into WO-n [J28: n=8]" (J2n=move-0-to-n-into-w0-wn 8))
  (defj J29 () "MOVE(0)-(n) into WO-n [J29: n=9]" (J2n=move-0-to-n-into-w0-wn 9))

  (defj J30 () "RESTORE W0-W0" (J3n=restore-wn 0))
  (defj J31 () "RESTORE W0-W1" (J3n=restore-wn 1))
  (defj J32 () "RESTORE W0-W2" (J3n=restore-wn 2))
  (defj J33 () "RESTORE W0-W3" (J3n=restore-wn 3))
  (defj J34 () "RESTORE W0-W4" (J3n=restore-wn 4))
  (defj J35 () "RESTORE W0-W5" (J3n=restore-wn 5))
  (defj J36 () "RESTORE W0-W6" (J3n=restore-wn 6))
  (defj J37 () "RESTORE W0-W7" (J3n=restore-wn 7))
  (defj J38 () "RESTORE W0-W8" (J3n=restore-wn 8))
  (defj J39 () "RESTORE W0-W9" (J3n=restore-wn 9))

  (defj J40 () "PRESERVE W0-W0" (J4n=preserve-wn 0))
  (defj J41 () "PRESERVE W0-W1" (J4n=preserve-wn 1))
  (defj J42 () "PRESERVE W0-W2" (J4n=preserve-wn 2))
  (defj J43 () "PRESERVE W0-W3" (J4n=preserve-wn 3))
  (defj J44 () "PRESERVE W0-W4" (J4n=preserve-wn 4))
  (defj J45 () "PRESERVE W0-W5" (J4n=preserve-wn 5))
  (defj J46 () "PRESERVE W0-W6" (J4n=preserve-wn 6))
  (defj J47 () "PRESERVE W0-W7" (J4n=preserve-wn 7))
  (defj J48 () "PRESERVE W0-W8" (J4n=preserve-wn 8))
  (defj J49 () "PRESERVE W0-W9" (J4n=preserve-wn 9))

  (defj J60 (arg0) "LOCATE NEXT SYMBOL AFTER CELL (0)"
    ;; LOCATE NEXT SYMBOL AFTER CELL (0). (0) is the name of a
    ;; cell. If a next cell exists (LINK of (0) not a termination
    ;; symbol), then the output (0) is the name of the next cell, and
    ;; H5 is set +.  (!!! This whole "name" thing is an f'ing lie!
    ;; It's the actual cell !!!)  If LINK is a termination symbol,
    ;; then the output (0) is the input (0), which is the name of the
    ;; last cell on the list, and H5 is set -. If the next cell is a
    ;; private termination cell, J60 will work as specified above, but
    ;; in addition, the private termination cell will be returned to
    ;; available space and the LINK of the input cell (0) will be
    ;; changed to hold 0. No test is made to see that (0) is not a
    ;; data term, and J60 will attempt to interpret a data term as a
    ;; standard IPL cell.
    (setf (h5) "+")
    ;; De-ref symbol to list if necessary
    (setf arg0 (drod arg0)) 
    (let* ((this-cell arg0)
	   (link (cell-link this-cell)))
      (!! :jfns "In J60, this-cell = ~s, link = ~s~%" this-cell link)
      (if (zero? link)
	  (setf (h5) "-")
	  (setf (H0) (cell link)) ;; (h5) is already + from above
	  )))

  (defj J66 (arg0 arg1) "INSERT (0) AT END OF LIST (1) IF NOT ALREADY ON IT"
      ;; J66 INSERT (0) AT END OF LIST (1) IF NOT ALREADY ON IT. A
      ;; search of list (1) is made. against (0) (starting with the
      ;; cell after cell (1) . If (0) is found, J66 does nothing
      ;; further. If (0) is not found, it is inserted at the end of
      ;; the list, as in J65. (??? What happens if the list
      ;; branches??? At the moment this can't do anything sensible
      ;; with a branching list!)
      (!! :jfns "J66 trying to insert ~s in ~s~%" arg0 arg1)
    (loop with list-cell = (drod arg1)
	  with symb = (if (stringp arg0)
			  arg0
			  (if (cell? arg0)
			      (cell-symb arg0)
			      (break "Error in J66: ~a should be a symbol or cell!" arg0)))
	  do
	  (cond ((string-equal (cell-symb list-cell) symb)
		 (!! :jfns "J66 found ~s in the list already. No action!~%" symb)
		 (return nil))
		((zero? (cell-link list-cell))
		 (!! :jfns "J66 hit end, adding ~s to the list!~%" symb)
		 (let* ((new-name (new-list-symbol (cell-name list-cell)))
			(new-cell (make-cell :name new-name :symb symb :link "0")))
		   (setf (cell-link list-cell) new-name)
		   (setf (cell new-name) new-cell)
		   (return t))))
	  ;; Move to next cell if nothnig above returned out
	  (setf list-cell (cell (cell-link list-cell)))))

  (defj J73 (arg0) "Copy list"
      (setf (H0)
	    (copy-list
	     (if (stringp arg0) (stack arg0)
		 (if (listp arg0) arg0
		     (error "J73 got ARG0=~s" arg0))))))

  (defj J74 (arg0) "Copy List Structure"
      ;; COPY LIST STRUCTURE (0). A new list structure is produced, the cells of
      ;; which are in one-to-one correspondence with the cells of list structure
      ;; (0). All the regional and internal symbols in the cells will be identical
      ;; to the symbols in the corresponding cells of (0), as will the contents of
      ;; data terms. There will be new local symbols, since these are the names of
      ;; the sublists of the new structure. Description lists will be copied, if
      ;; their names are local. If (0) is in auxiliary storage (Q of (0) = 6 or 7),
      ;; the copy will be produced in main storage. In all cases, list structure (0)
      ;; remains unaffected. The output (0) names the new list structure. It is
      ;; local if the input (0) is local; It is internal otherwise.
      (!! :jfns "J74 is copying list: ~s~%" (H0))
      (setf (H0) (copy-list-structure arg0))
      )

  (defj J90 () "Create a blank cell on H0"
      ;; J90: Get a cell from the available space list, H2, and leave its name in HO.
      ;; J90 creates an empty list (also used to create empty storage cells, and empty data terms).
      ;; The output (0) is the name a the new list.
      (let* ((name (new-list-symbol "L"))
	     (cell (make-cell :name name :symb "0" :link "0")))
	(!! :jfns "J90 creating blank list ~s~%" name)
	(setf (cell name) cell)
	(vv "H0" cell)))

  (defj J100 (arg0 arg1) "GENERATE SYMBOLS FROM LIST (1) FOR SUBPROCESS (0)"
      ;; J100 GENERATE SYMBOLS FROM LIST (1) FOR SUBPROCESS (0). The subprocess
      ;; named (0) is performed successively with each of the symbols of list named
      ;; (1) as input. The order is the order on the list, starting with the first
      ;; list cell. H5 is always set + at the start of the subprocess. J100 will
      ;; move in list (1) if it is on auxiliary.
      (loop with subcall = (H0)
       	    for elt in (break "(listX arg1) isn't implemented")
       	    do
	    (push elt (H0+))
	    (ipl-eval arg0)
	    (pop (H0+))
	    ))

  (defj J120 (arg0) "COPY (0)"
      ;; COPY (0). The output (0) names a new cell containing the identical
      ;; contents to (0). The name is local if the input (0) is local; other-
      ;; wise, it is internal.
      (let ((new-cell (copy-cell (H0))))
	(setf (cell-name new-cell) (new-list-symbol))
	(setf (H0) new-cell)))

  (defj J151 (arg0) "Print list (0)"
	 (prlist arg0))

  (defj J154 () "Clear print line"
      ;; Clear Print Line CLEAR PRINT LINE. Print line 1W24 is cleared and the
      ;; current entry column, 1W2S, is set equal to the left margin, 1W21.
      (format t "WWW J154 (Clear Print Line) is UNIMPLEMENTED !!!~%"))

  (defj J180 () "READ LINE J180 READLINE"
    ;; The next record on unit 1W18 is read to line 1W24. (The record
      ;; is assumed to be BCD, 80 cols.) Column 1 of the record is
      ;; read into column 1 of the read line, and so forth. H5 is
      ;; set+. If no record can be read (end-of-file condition), the
      ;; line is not changed and HS is set - .
      (let ((line (read-line *input-stream* nil nil)))
	(!! :io "J180 Read:~%~s~%%" line)
	(cond (line
	       (push line (stack "W24"))
	       (setf (cell-symb (h5)) "+"))
	      (t (setf (cell-symb (h5)) "-")))))

)

;;; ===================================================================
;;; JFn Utilities

(defun J2n=move-0-to-n-into-w0-wn (n)
  (setf (cell "W0") (H0))
  (loop for nn from 1 to n ;; Won't do anything if n=0
	as val in (H0+)
	do (setf (cell (format nil "W~a" nn)) val)))

(defun J3n=restore-wn (n)
  (loop for nn from 0 to n do (^^ (format nil "W~a" nn))))

(defun J4n=preserve-wn (n)
  (loop for nn from 0 to n do (vv (format nil "W~a" nn))))

;;; Copying an IPL list is a tricky because they aren't represented like normal
;;; lisp lists (maybe they shold be?) but instead are a pile of cells where
;;; internal structure results from the symb and links pointing to other named
;;; cells all at the top level. In order to do this we need scan the whole list
;;; recursviely and create new symbols at each point. The only situation where
;;; we don't need to fill in an explicit pointer is when the list points to the
;;; NEXT element, but we do that anyway. All lists ground out on a 0 in the symb
;;; or link.

(defun copy-list-structure (l)
  (if (zero? l) l ;; End of sublist, just return the EOsL "0"
      (let ((new-name (new-list-symbol)))
	(setf (gethash new-name *symtab*) (mapcar #'copy-list-cell l))
	new-name)))

(defun copy-list-cell (cell)
  (if (zero? cell) cell ;; End of sublist, just return the EOsL "0"
      (let* ((new-cell (copy-cell cell)))
	(setf (cell-name new-cell) (new-list-symbol))
	;; WWW ??? This has the problem that it's going to copy whole functions
	;; into copied lists, which is probably not what is intended. Maybe
	;; things that are defined in the load process shouldn't be copied? 
	(setf (cell-symb new-cell) (copy-list-structure (cell-symb cell)))
	(setf (cell-link new-cell) (copy-list-structure (cell-link cell)))
	)))
	
(defun prlist (cell)
  (setf cell (drod cell))
  (format t "~%+---------------------------------------------------------------------+~%")
  (loop do (format t "| ~s~70T|~%" cell)
	(let ((link (cell-link cell)))
	  (if (zero? link) (return :end-of-list))
	  (setf cell (cell link))))
  (format t "+---------------------------------------------------------------------+~%")
  )

;;; =========================================================================
;;; Emulator core

;;; Directly implements "3.15 THE INTERPRETATION CYCLE", pg. 164 of
;;; the IPL-V manual. It can actually be called recursively...but the
;;; caller has to keep track of H1. IPL code implements recursion "the
;;; hard way", so there's generally no need to call this fn
;;; recursively.

(defun run (start-symb)
  (initialize-machine)
  (ipl-eval (cell start-symb))
  (report-system-cells)
  )

(defun initialize-machine ()
  (create-system-cells)
  (setf (h5+) (list "+"))
  )

(defun ^^ (ssname)
  (setf (cell ssname) (pop (stack ssname))))
(defun vv (ssname &optional new-value)
  (push (cell ssname) (stack ssname))
  (when new-value (setf (cell ssname) new-value)))

(defun ipl-eval (start-cell)
  (!! :run "vvvvvvvvvvvvvvv Entering IPL-EVAL at ~s" start-cell)
  (prog (cell pq q p symb link fname-hint)
     ;; fname-hint is needed because I can't figure out how to get the
     ;; number of arguments a lambda needs in SBCL, and anyway it's
     ;; useful for tracing which jfn we think we're running when all
     ;; we have in hand is the lambda list.
     (setf (h1) (new-symb-cell "exit")) ;; Top of stack -- force exit (may be recursive)
     (vv "H1" start-cell) ;; Where we're headed this time in.
     ;; Indicates (local) top of stack for hard exit (perhaps to recursive call)
   INTERPRET-Q 
     (!! :run-full "---> At INTERPRET-Q w/H1 = ~s! (fname-hint = ~s)~%" (h1) fname-hint)
     ;; H1 contains the name of the cell holding the instruction to be
     ;; interpreted. At this point it could be a symbol or a list. If it's a
     ;; symbol, we need to de-reference it to the list. In the case of an
     ;; internal (J) funtion this will be a lambda, in which case we just call
     ;; it and then advance
     (when (null (H1)) (break "!!! PROBABLY MISSING A JFN DEFINITION !!!"))
     (when (functionp (h1))
       (let* ((arglist (second (function-lambda-expression (H1))))
	      (args (if (null arglist) ()
			(cons (H0)
			      (loop for arg in (cdr arglist)
				    as val in (h0+)
				    collect val)))))
	 (!! :run ">>>>>>>>>> Calling ~a [~a]~%           ~s = ~s~%"
	     fname-hint (getf (gethash fname-hint *jfn-plists*) 'explanation) arglist args)
	 (apply (H1) args))
       (^^ "H1") ;; Remove the JFn call
       (go ADVANCE)
       )
     (setq cell (H1)) ;; This shouldn't be needed since we're operating all in cell now.
     (!! :run "~%>>>>>>>>>> Executing: ~s~%" cell)
     (setf *trace-instruction* cell) ;; For tracing and error reporting
     (setf pq (cell-pq cell)
	   q (getpq :q pq)
	   p (getpq :p pq)
	   symb (cell-symb cell)
	   link (cell-link cell)
	   )
     (!! :run-full "~%-----> At INTERPRET-Q: CELL =~s~%      Q = ~s, symb=~s~%" cell q symb)
     (case q
       ;; 0 take the symbol itself
       (0 (setf (s) symb) (go INTERPRET-P))
       ;; 1 Take the name the symbol is pointing to
       (1 (setf (s) (cell symb)) (go INTERPRET-P))
       ;; 2 Take the symbol in the cell at the name that the symb is pointing to
       (2 (setf (s) (cell (cell-name% (cell symb)))) (go INTERPRET-P))
       (3 (format t "(Unimplemented monitor action in ~s; Executing w/o monitor!)~%" cell) (setf (s) symb) (go INTERPRET-P))
       (4 (format t "(Unimplemented monitor action in ~s; Executing w/o monitor!)~%" cell) (setf (s) symb) (go INTERPRET-P))
       (5 (call-ipl-prim symb) (go ASCEND)) ;; ??? THIS IS VERY UNCLEAR; NO PUSH ???
       (6 (error "In RUN at INTERPRET-Q:~%~s~%, Q=6 unimplmented!" cell))
       (7 (error "In RUN at INTERPRET-Q:~%~s~%, Q=7 unimplmented!" cell))
       )
   INTERPRET-P 
     (!! :run-full "-----> At INTERPRET-P w/P = ~s, (s)=~s~%" p (s))
     (case p
       (0 (go TEST-FOR-PRIMITIVE))
       (1 (vv "H0" (S)))                    ;; Input S (after preserving HO) 
       (2 (setf (cell (S)) (H0)) (^^ "H0")) ;; Output to S (then restore HO)
       (3 (^^ (s)))                         ;; Restore (pop up) S 
       (4 (vv (S)))                         ;; Preserve (push down) S
       (5 
	;; Replace (0) by S -- Here if S is just a symbol we need to
	;; make a cell to hold it because it's just a list symbol
	;; (string, actually) (But if H0 is already a cell we can
	;; just replace it.)
	(setf (H0) (if (cell? (s)) (s)
		       (if (stringp (s)) (new-symb-cell (s))
			   (break "Having trouble interpreteing (s)=~s in P=5." (s))))))
       (6 (setf (s) (cell-symb (H0))))      ;; Copy (0) in S -- opposite of 5, and we unpack the cell to a symbol.
       (7 (go BRANCH)) ;; Branch to S if H5-
       )
     (go ADVANCE)
   TEST-FOR-PRIMITIVE 
     ;; Q of S: - Q = 5: Transfer machine control to SYMB of S (executing
     ;; primitive); go to ADVANCE. - Q ~= 5: Go to DESCEND
     (!! :run-full "-----> At TEST-FOR-PRIMITIVE w/S = ~s, Q = ~s, symb=~s~%" (s) q symb)
     (case q 
       (5 (setf link (s)) (go ADVANCE))
       (t (go DESCEND)))
   ADVANCE (!! :run-full "-----> At ADVANCE")
     (when (string-equal (cell-symb (h1)) "exit")
       (!! :run "Exiting from IPL-EVAL ^^^^^^^^^^^^^^^")
       (^^ "H1") (return))
     ;; Interpret LINK: - LINK= 0: Termination; go to ASCEND. LINK ~= 0: LINK is
     ;; the name of the cell containing the next instruction; put LINK in H1; go
     ;; to INTERPRET-Q.
     (setf link (cell-link (H1)))
   ADVANCE-W/FORCED-LINK (!! :run-full "-----> At ADVANCE-W/FORCED-LINK (link=~s)" link)
     ;; If link is nil ("") in the middle of a function, go next cell, else ascend.
     (if (zero? link) (go ASCEND))
     ;; Note that if there is a link to a different function
     ;; (commonly J31, which resets W0 and W1), then when THAT
     ;; function terminates the whole prog sequence
     ;; ascends. This is a somewhat confusing yet common way to
     ;; end a function, that is, by branching off to a J
     ;; function which, when it completes, pops to whereever its
     ;; caller came from.
     (setf (h1) (cell link))
     (go INTERPRET-Q)
     ;; FFF ASCEND and DESCEND could probably be handled more cleanly and
     ;; correctly by recursing on IPL-EVAL !!!
   ASCEND 
     (!! :run-full "-----> At ASCEND w/H1 = ~s~%" (h1))
     ;; Restore H1 (returning to H1 the name of the cell holding the current
     ;; instruction, one level up); restore auxiliary region if required (not!);
     ;; go to ADVANCE.
     (^^ "H1")
     (go ADVANCE)
   DESCEND 
     (!! :run-full "-----> At DESCEND w/S = ~s~%" (s))
     ;; Preserve H1: Put S into H1 (H1 now contains the name of the cell holding
     ;; the first instruction of the subprogram list); go to INTERPRET-Q.
     (setf fname-hint (s))
     (vv "H1" (cell (s)))
     (go INTERPRET-Q)
   BRANCH 
     (!! :run-full "-----> At BRANCH w/H5 = ~s, S= ~s~%" (h5) (s))
     ;; Interpret Sign in H5: - H5-: Put S as LINK (control transfers to S); go
     ;; to ADVANCE. - H5+: Go to ADVANCE
     (when (string-equal (h5) "-") (setf link (s)) (go ADVANCE-W/FORCED-LINK))
     (go ADVANCE)
     ))

(defun call-ipl-prim (symb)
  (break "!!!!!!!! UNIMPLEMENTED: (call-ipl-prim ~s)" symb))

;;; Getting the P and Q is a little tricky because they can be blank. Blank is
;;; interpreted as zero, and if they're both blank ("") it's not a problem --
;;; both zero, but if only one is blank it can be ambiguous because these didn't
;;; come from cells. This isn't suppose to happen, so if it does, we raise a
;;; warning, and intepret it as if P is blank (0). So, for example, technically
;;; they could have entered "9_" instead of "_9", but we can't tell the
;;; difference. We should always code these as with 90 or 09 to disambiguate.

(defun getpq (pq? val &aux (l (length val)))
  (unless (stringp val) (error "GETPQ was passed VAL = ~s" val))
  (if (> l 2)
      (error "In GETPQ, val = ~s, which shouldn't happen!" val)
      (if (zerop l) 0
	  (if (= 1 l)
	      (case pq? (:p 0) (:q (parse-integer val)))
	      (parse-integer (case pq? (:p (subseq val 0 1)) (:q (subseq val 1 2))))))))

;;; =========================================================================
;;; Utilities

(defun core-dump (table)
  (format t "~a contains ~a entries:~%" table (hash-table-count table))
  (loop for key being the hash-keys of table
	using (hash-value value)
	do (format t "~s => ~s~%" key value)))

;;; =========================================================================
;;; Test calls

(untrace)
(trace ipl-eval run)
(setf *!!list* '(:run)) ;; :load :run :jfns :run-full :io (t for all)
;(load-ipl "LTFixed.lisp")
(load-ipl "F1.lisp")
