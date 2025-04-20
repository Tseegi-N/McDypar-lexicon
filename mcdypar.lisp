;;;****************************************************************************
;;; MCDYPAR
;;; KYOTO COMMON LISP VERSION
;;;
;;; JCM: Since memory is so plentiful, I'm modifying the code, making the
;;; symbol names more descriptive
;;;
;;; Adapted to Common Lisp 2/21/92 by Michael McNally 
;;;   * A simple preprocessor has been added that recognizes periods
;;;   and paragraph breaks. (It can easily be extended to recognize
;;;   additional forms of punctuation.) The preprocessor also allows
;;;   comments to appear within the text sample being analyzed. See
;;;   the file "sample".
;;; Based on 1) the TLISP version of McDypar given in the appendix of
;;;             Michael Dyer's "In Depth Understanding."
;;;          2) some ideas from the SCHEME version of McDypar
;;;             provided by John Perry
;;; PROBLEMS:
;;;
;;;   * When executing, the program will generate a sequence of
;;;   concepts CON0, CON1, .... However, on subsequent executions, the
;;;   count will not be reset to CON0, but may start with something
;;;   like CON20 or CON50. For large problems, this accumulation of
;;;   old symbols would eventually fill up available memory, but for
;;;   small parses involving a few hundred concepts it should pose no
;;;   problem.
;;;
;;;   * The print out of the trace is not quite as pretty as the TLISP
;;;   version. Currently, the flags which should control the printing
;;;   of trace information are being ignored.
;;;
;;;****************************************************************************

(setf *print-comments* T)

;;; This is the top-level function. it opens an input file, reads in
;;; the text, performing some preprocessing, then invokes the parser,
;;; then outputs the results.

(defun mcdypar (filename)
  (cond ((setf *stream* (open filename))
	 (let ((text (preprocessor)))
	   (print-line-of-*)
	   (format t "~%~%KCL MCDYPAR")
	   (format t "~%~%Sentence read:~%  ~s~%" text)
	   (print-line-of-*)
	   (parse text)
	   (print-line-of-*)
	   (format t "~%~%Program terminated~%")
	   (print-line-of-*)
	 (close *stream*)))))

(defun print-line () (format T "~%"))
(defun print-line-of-* ()
  (format t "~%********************************************************"))
(defun print-line-of-- ()
  (format t "~%--------------------------------------------------------"))
(defun print-line-of-= (&optional (demon-id NIL))
  (format t "~%========================")
  (if demon-id (format T " ~S " demon-id)
    (write-string "======="))
  (write-string "========================="))

(defun parse (sentence)
  (initialize-global-variables)
  (setf *sentence* sentence)
  (do ()
      (*stop!*)
    (word-tasks)
    (demon-tasks)
  (fin) ;; Output results!
))

;;;**********************************************************************
;;; WORD TASKS
;;;**********************************************************************

(defun word-tasks ()
  (setf *previous-word* *current-word*)
  (setf *current-word* (pop *sentence*))
  (setf *next-word* (first *sentence*))
  (setf *next-next-word* (second *sentence*))
  (if *current-word* (run-word *current-word*)
    (setf *stop!* t)))

(defun run-word (word)
  (print-line-of--)
  (format t "~%Processing word: ~s" word)
  (print-line-of--)
  (if (get word 'word) (load-word word)
    (format T "~%-- is ignored")))

;;;**********************************************************************
;;; CODE TO HANDLE DECLARATIVE NOTATION FOR WORD SENSES
;;;**********************************************************************

(defun load-word (word)
  ;; load the word into working memory, spawning any demons and
  ;; setting the definition if it is known.
  (let ((con (add-working-memory (newsym 'concept)))
	(def (get word 'def))
	(dem (get word 'demons)))
    (if def (set con (split-config con def))
      (set con nil))
    (format t "~%~s = ~s" con (eval con))
    (if dem (spawn-dforms con dem))))

(defun add-working-memory (con)
  ;; places a con at the front of the working memory queue, with before
  ;; and after pointers set. before points to all con atoms occuring before
  ;; this con in time, after = all occuring later in time.
  (format t "~%Adding to *working-memory*: ~s" con)
  (setf (get con 'word) *current-word*) ; save word that caused this concept
  (cond ((null *working-memory*) (setf *working-memory* con))
	(t (setf (get con 'before) *working-memory*)
	   (setf (get *working-memory* 'after) con)
	   (setf *working-memory* con))))

(defun newsym (sym)
  ;; create a new unique symbol in the sequence sym1 sym2 sym3 ...
  (let ((count (get sym 'usage-count)))
    (if (null count) (setf count 0))
    (setf (get sym 'usage-count) (1+ count))
    (intern (concatenate 'string (string sym)
				      (prin1-to-string count)))))

;;; a word definition in the lexicon should be in configuration form.
;;; a configuration (config) is a cd structure with any gap-filling
;;; demons associated with it. the syntax for specifying a config is:
;;;   config   --> nil | (pred) | (pred slot-name spec ... slot-name spec)
;;;   spec     --> config | gap-atom [ <== (gapdform ... gapdform) ]
;;;   gap-atom --> atom | *
;;; | means "or"
;;; pred and slot-name are atoms
;;; gapdform is a demon-form missing the con and gap arguments.
;;; [] indications optionality
;;; * indicates that the gap-node receives the associated slot-name as
;;; its newsym name.

;;; separates demons from config and returns the appropriate
;;; structure with gap-atoms created, etc.
(defun split-config (con config)
;;  (format T "~%split-config, con = ~S, config = ~S" con config)
  (if config
      (cons (car config) (split-rest con (cdr config)))))

;;; Append an atom to a list
(defun append1 (lst atm) (append lst (list atm)))

;;; split-rest takes a list of form: (slot filler ... slot filler)
;;; where each filler may be followed by: <== (-dform-)
(defun split-rest (con rest)
;;  (format T "~%split-rest, con = ~S, rest = ~S" con rest)
  (do ((struct nil)
       (slot nil)
       (filler nil)
       (gap nil)
       (dforms nil))
      ;; terminating condition and return value
      ((null rest) struct)
      ;; body of loop
      (setf slot (pop rest))
      (setf filler (pop rest))
;;      (format T "~%struct = ~S, slot = ~S" struct slot)
      (setf struct (append1 struct slot)) ;; slot names are copied
;;      (format T "~%slot = ~S, filler = ~S, struct = ~S"
;;    slot filler struct)
      (cond (;; if the gap position is nil then create a gap-node with
	     ;; the value nil.
	     (null filler)
	     (setf gap (newsym slot))
	     (set gap nil))

	    ((consp filler)
	     ;; if the gap position holds an embedded structure, then
	     ;; recursively spawn any demons for that structure.

;;	     (format T "~%#1 slot = ~S, filler = ~S, struct = ~S"
;;		     slot filler struct)

	     (setf gap (newsym slot))
	     (set gap (split-config con filler))

;;	     (format T "~%#2 slot = ~S, filler = ~S, struct = ~S"
;;		     slot filler struct)
	     )

	    ((equal filler '*)
	     ;; if the gap position holds a "*" then it must have
	     ;; "<==" and demons following, so spawn these demons.
	     (setf gap (newsym slot))
	     (set gap nil)
	     (pop rest) ; ignore "<=="
	     (setf dforms (pop rest)) ; get demons and spawn them
	     (spawn-dforms con dforms gap))

	    ((symbolp filler)
	     ;; if the gap-position holds an atom, then it may or may
	     ;; not be followed by demons. if demons do follow it, the
	     ;; save the atom so that other references to it get the
	     ;; same gap.
	     (cond ((equal (car rest) '<==)
		    (setf gap (newsym filler))
		    (set gap nil)
		    (push (cons filler gap) *gap-alist*)
		    (pop rest)
		    (setf dforms (pop rest))
		    (spawn-dforms con dforms gap))
		   (t (setf gap (cdr (assoc filler *gap-alist*)))
		      (if (null gap) (setf gap filler)))))

	    ;; Error case: should not occur
	    (t (format t "~%Error in split-rest: filler = ~s" filler)))
;;      (format T "~%struct = ~S, gap = ~S" struct gap)
      (setf struct (append1 struct gap))))
      
;;;**********************************************************************
;;; DEMON MANAGEMENT
;;;**********************************************************************

;;; go through each con in *working-memory*, testing all the demons of each. if
;;; any demon fires, then (after all have been tested) go back through
;;; con atoms again (since one firing might allow a previously tested
;;; demon to now fire.)
(defun demon-tasks ()
  (setf *demon-fired* nil)
  (examine-all-d-agendas *working-memory*)
  (if *demon-fired* (demon-tasks)))

(defun examine-all-d-agendas (con)
  (cond ((null con) nil)
	(t (dolist (d-atm (get con 'd-agenda))
	     (setf *current-demon* d-atm)
	     (run-demon-atm d-atm con))
	   (examine-all-d-agendas (get con 'before)))))

;;;**********************************************************************
;;; DEMON INTERPRETER FUNCTIONS
;;;**********************************************************************

;;; note: variables that the killpart wants to pass to testpart, or
;;; that testpart wants passed to actpart, etc. should be marked share
;;; and then just setf-ed. note: an empty test part is equivalent to
;;; (test t).
(defun run-demon-atm (d-atm con)
  (let* ((d-form (eval d-atm))       ;; d-form is named demon
	 (d-name (car d-form))
	 (argums (cdr d-form))
	 (d-body (get d-name 'demon))
	 (vars   (append (cdr (assoc 'share d-body))  ; shared variables
			 (cdr (assoc 'params d-body)) ; parameters
			 '(test))))                   ; test result
    ;; Create parameter and shared environment. (Shared values are
    ;; initialized to NIL). Once variables have been bound to values from
    ;; argums, run-demon is executed within this dynamic binding
    ;; environment.
    (do ((vl vars (cdr vl))
	 (al argums (cdr al)))
	((null vl))
	(if (not (boundp (car vl)))
	    (eval (list 'defvar (car vl) NIL))))
    (eval (list 'let (let-list vars argums)
		`(run-demon ',d-name ',d-body ',d-atm ',con)))))

(defun let-list (x y)
  (if (null x) NIL
    (cons (list (car x) (list 'quote (car y)))
	  (let-list (cdr x) (cdr y)))))

;;; Evaluate every expression in list L, return the last result.
;;; NOTE: It can be handy to insert comments into this code to trace
;;; the execution of demons.
(defun eval-list (L)
  (cond ((null L) NIL)
	((null (cdr L))
	  (eval (car L)))
	(T (eval (car L))
	   (eval-list (cdr L)))))

;;; actually interprets the demon keyword-parts
(defun run-demon (d-name d-body d-atm con)
  (let ((kill-part (assoc 'kill d-body))
	(test-part (assoc 'test d-body))
	(+act-part (assoc '+act d-body))
	(-act-part (assoc '-act d-body))
	(test nil))
    (cond ((and kill-part (eval-list (cdr kill-part))) ; do?
	   (kill-demon d-atm con))
	  ((or (null test-part)
	       (and (setf test (eval-list (cdr test-part)))))
	   (format t "~%Executing: ~s = ~s" d-atm (eval d-atm))
	   (eval-list (cdr +act-part))
	   (kill-demon d-atm con))
	  ((and -act-part (null test))
	   (format t "~%Executing -act of ~s = ~s" d-atm (eval d-atm))
	   (eval-list (cdr -act-part))
	   (kill-demon d-atm con)))))

;;; if the test is true, always kill the demon.
(defun kill-demon (d-atm con)
  (format t "~%Killing: ~s = ~s" d-atm (eval d-atm))
  (let ((dforms (get con 'd-agenda)))
    (setf dforms (remove1st d-atm dforms))
    (setf *demon-fired* t)
    (setf (get con 'd-agenda) dforms)))

;;; returns a list with first element matching 'elem' at top level removed.
(defun remove1st (elem l)
  (cond ((null l) nil)
	((equal elem (car l)) (cdr l))
	(t (cons (car l) (remove1st elem (cdr l))))))

;;;**********************************************************************
;;; FUNCTIONS TO SPAWN DEMONS
;;;**********************************************************************

;;; form is (spawn con dform1 ... dformn)
;;; where con is an atom, and dform is a named demon. (name arg1 ... argm)
;;; spawn does the following:
;;;  1) replaces the args in a dform with their values.
;;;  2) creates a d-atom whose value is the new dform
;;;  3) pushes each d-atom onto a d-agenda under a specified con
;;; note: spawn should be called from user defined demons. spawn
;;; dforms is called at word definition time.
(defun spawn (&rest grp)
  (let ((con (eval (car grp)))
	(dforms (cdr grp)))
    (dolist (dform dforms)
      (spawn-dform con dform))))

;;; adds in the myconcept and mygap for spawning when the demon occurs
;;; after a "<==" in a word sense.
(defun spawn-dforms (con dforms &optional (gap NIL))
  (if (atom (car dforms))
    (setf dforms (list dforms)))
  (dolist (dform dforms)
    (let ((head (car dform))
	  (args (cond (gap (append (list (list 'quote con)
					 (list 'quote gap))
				   (cdr dform)))
		      (t (append (list (list 'quote con))
				 (cdr dform))))))
      (spawn-name con head args))))

;;; like spawn-dforms except myconcept and mygap assumed already inserted
;;; in body. this function is called from spawn.
(defun spawn-dform (con dform)
  (let ((head (car dform))
	(body (cdr dform)))
   (format T "~%Spawn-dform, con = ~S, dform = ~S, head = ~S"
	    con dform head)
   (format T "~%body = ~S" body)
   (format T "~%get = ~S" (get head 'demon))
    (cond ((get head 'demon) (spawn-name con head body))
	  (t (format t "~%Dform not spawned since name: ~s undefined" head)))))

;;; eval args and form their vals into a new dform with name at head.
;;; then put d-atom into a d-agenda on the con.
(defun spawn-name (con head args)
;; (format T "~%spawn-name, con = ~S, head = ~S, args = ~S"
;;	  con head args)
  (cond ((null (get head 'demon))
	 (format t "~%~s not spawned since undefined" head))
	(t (let ((new-args (mapcar #'eval args))
		 (d-atom (newsym 'demon)))
	     (set d-atom (cons head new-args))
	     (format t "~%Spawning: ~s = ~s" d-atom (eval d-atom))
	     (print-comment (get head 'demon) d-atom)
	     (d-agendize con d-atom)))))

;;; the agendas are currently just lists. push the domon onto the
;;; agenda.
(defun d-agendize (con d-atom)
  (setf (get con 'd-agenda)
	(cons d-atom (get con 'd-agenda))))

;;;**********************************************************************
;;; FUNCTIONS TO DEFINE WORDS AND DEMONS
;;;**********************************************************************

;;; template for word:
;;; (word <name>
;;;    def    config
;;;    demons dforms
;;;    m1     config
;;;    m2     config
;;;       ...
;;;    mn     config)
;;;
;;; note: if the word is unambiguous its config should be put in the
;;; def field. if it is ambiguous, the configs associated with the
;;; various meanings should be placed in fields m1 ... mn with the def
;;; field remaining blank.

(defmacro word (&rest l)
  `(wordf ',l))

(defun wordf (l)
  (let ((name (pop l)))
    (setf (get name 'word) t)        ;; indicates item is defined as a word
    (do ((indic nil)                 ;; put all properties on the word
	 (val nil))
	;; term cond & return value
	((null l) name)
	;; body of loop
	(setf indic (pop l))
	(setf val (pop l))
	(cond ((equal indic 'value) (set name val))
	      (t (setf (get name indic) val))))))

;;; Macrod to define demons. <the user can add any other keywords,
;;; but they are ignored by the demon interpreter.>

(defmacro demon (name &rest l)
  `(block ()
	  (setf (get ',name 'demon) ',l)
	  ',name))

;;;**********************************************************************
;;; STANDARD UTILITY FUNCTIONS USED BY DEMONS
;;;**********************************************************************

;;; Binds a gap with a con in working memory (*working-memory*) and marks the con
;;; as inside the myconcept associated with this gap. during tracing,
;;; prints: "gap <-- con" when the binding occurs.
(defun link (myconcept mygap con-found)
  (if mygap
    (let ((myslot (slot mygap myconcept))) ; huh?
      (if (symbolp con-found)
	(setf (get con-found 'inside) (list myconcept myslot mygap)))
      (set1 mygap con-found))))

;;; Sets the con to the appropriate meaning when disambiguation has
;;; occured.
(defun con-set (myconcept meaning)
  (set myconcept (split-config myconcept meaning))
  (format t "~%~s = ~s" myconcept (eval myconcept)))

;;; Take the con-head of CON and return T if it is a member of CLASS.
(defun con-class? (con class)
  (class? (head con) class))

;;; returns non-nil if atm is a member of class (searches recursively
;;; through atm's classes for this class.)
(defun class? (atm class)
  (cond ((consp atm) (format T "~%CLASS? expects atomic first argument"))
	((and atm
	      (cond ((consp class) (member atm class)) ; member-eq
		    (T (equal atm class)))))))

;;; Path is used to selectively examine the contents of conceptual
;;; structures in working memory. it allows the user to ignore the
;;; fact that all the bindings are done via atoms. for example:
;;; given x = (ingest actor (human name (john) gender (male))
;;;                   object (food type (lobster)))
;;; then (path '(actor name) x)     ==> (john)
;;; and  (path '(object type *) x)  ==> lobster
(defun path (l cd)
  (cond ((null cd) nil)
	((atom cd) (path l (eval cd)))
	((null l) cd)
	((atom l) (format t "~%PATH expects first argument to be a list"))
	((equal (car l) '*) (car cd))
	(t (path-rest l (cdr cd)))))

(defun path-rest (l rv-lis)
  (let ((val (cadr (member (car l) rv-lis)))) ; memq?
    (if val (path (cdr l) val))))

;;; expand replaces every gap with its value
(defun expand (cd)
  (cond ((null cd) nil)
	((atom cd) (expand (eval cd)))
	(t (cons (car cd) (expand-sf (cdr cd))))))

;;; expand-sf expects a list of slot-fillers.
(defun expand-sf (sf)
  (do ((ans nil))
      ((null sf) ans)
      (setf ans (append1 ans (pop sf)))
      (setf ans (append1 ans (expand (pop sf))))))

;;; given cd-atm = (head slot fil slot fil ...) returns head
(defun head (cd)
  (cond ((null cd) nil)
	((atom cd) (head (eval cd)))
	(t (car cd))))

;;; given slot-name and cd, returns the top-level gtap-name of that
;;; slot-name (not the gap value)
(defun gap (slot cd)
  (cond ((null cd) nil)
	((atom cd) (gap slot (eval cd)))
	(t (cadr (member slot cd)))))

;;; given a gap (not the gap value) and a cd, returns the top-lvel
;;; slot-name that has gap-name associated with it.
(defun slot (gap cd)
  (cond ((null cd) nil)
	((atom cd) (slot gap (eval cd)))
	(t (cadr (member gap (reverse cd))))))

;;; search takes four parameters:
;;; 1. what to look for: a fcn-name or lambda-exp of one argument
;;; 2. where to start looking: a con atm (or variable *working-memory*)
;;; 3. when to give up looking: a fcn-name or lambda-exp of one argument.
;;; 4. what direction to look in: before or after
;;; note: a. it tries 3. on con atm first, and then tries 1.
;;;       b. it quits if runs out of atms to look at
(defun mcsearch (test-fcn &optional (start *working-memory*)
			(stop-fcn nil) (dir 'before))
  (do ((found nil)
       (ptr start (get ptr dir)))
      ((or (null ptr)
	   (and stop-fcn
		(let ((st-val (apply stop-fcn (list ptr))))
		  (cond ((equal st-val t) nil)
			(st-val (setf found t)
				(setf ptr st-val)))))
	   (setf found (apply test-fcn (list ptr))))
       (and found ptr))))

;;; look for class throughout *working-memory*
(defun find-working-memory (class myconcept)
  (mcsearch (lambda (con)
	     (and (not (equal con myconcept))
		  (con-class? class)))
	  *working-memory* nil 'before))
	
;;; like set, but used for tracing 
(defun set1 (a b)
  (set a b)
  (format t "~%~s <-- ~s" a b)
  b)

(defun print-comment (body d-atm)
  (let ((comment (cdr (assoc 'comment body))))
    (let ((test (cdr (assoc 'test comment)))
	  (act  (cdr (assoc 'act  comment)))
	  (first T))
      (cond ((and *print-comments* (or test act))
	     (print-line)
	     (print-line-of-= d-atm)
	     (format t "~%t:")
	     (dolist (str test)
		     (if first (setf first NIL)
		       (format T "~%     "))
		     (write-string str))
	     (format t "~%a:")
	     (setf first T)
	     (dolist (str act)
		     (if first (setf first NIL)
		       (format T "~%     "))
		     (write-string str))
	     (print-line-of-=)
	     (print-line))))))

;;; usage:  (copy-elem 'x 3) ==> (x x x)
(defun copy-elem (elem count)
  (if (<= count 0) nil
    (cons elem (copy-elem (1- count)))))


(defun initialize-global-variables ()
  (setf *sentence* nil)                     ;; current sentence
  (setf *gap-alist* nil)             ;; holds a list of the gap names
  (setf *stop!* nil)
  (setf *previous-word* nil)
  (setf *current-word* nil)
  (setf *next-word* nil)
  (setf *next-next-word* nil)
  (setf *local-char* nil)
  (setf *most-recent-char* nil)
  (setf *most-recent-female* nil)
  (setf *most-recent-male* nil)
  (setf *current-demon* nil)
  (setf *most-recent-object* nil)
  (setf *working-memory* nil)                    ;; working memory
  (setf *em* nil)                    ;; episodic memory
)

(defun fin ()
  (format T "~%~%Parse Results:~%")
  (pretty (do ((con *working-memory* (get con 'before))
	       (unused NIL))
	      ((null con) unused)
	      (if (and (not (get con 'inside))
		       (not (get con 'ignore)))
		(push con unused))))
  (format T "~%~%")
  )

(defun pretty (L)
  (dolist (con L)
    (pprint (expand con))))

;;;**********************************************************************
;;; PREPROCESSOR
;;;**********************************************************************

(defun preprocessor ()
  ;; Read the input file, skipping past comments (which are either
  ;; enclosed in pairs of slashes '/', or are on lines that start with
  ;; three slashes '///'.)
  ;; This function returns a list of symbols.
  (let ((s nil))
    (do ((state 'reading-words)
	 (next-char nil))
	;; terminating condition
	((equal state 'done))
	(setf next-char (read-char *stream* nil))
(if (or (null next-char)
		(equal next-char '#\@))
	    (setf state 'done))
	(case state
	      (saw-1-slash
	       (cond ((equal next-char '#\/)
		      (setf state 'saw-2-slashes))
		     (t (setf state 'in-comment-block))))
	      (saw-2-slashes
	       (cond ((equal next-char '#\/)
		      (setf state 'in-comment-line))
		     (t (setf state 'reading-words)
			(unread next-char *stream*))))
	      (in-comment-block
	       (cond ((equal next-char '#\/)
		      (setf state 'reading-words))))
	      (in-comment-line
	       (cond ((member next-char '(#\newline #\return))
		      (setf state 'reading-words))))
	      (reading-words
	       (cond ;; if we have a slash, set state to saw-1-slash
		     ((equal next-char '#\/)
		      (setf state 'saw-1-slash))
		     ;; handle case for paragraph (two newlines in a row)
		     ((and (equal next-char #\newline)
			   (equal (peek-char nil *stream* nil) #\newline))
		      (read-char *stream*)
		      (push '*paragraph* s))
		     ;; ignore spaces
		     ((member next-char '(#\space #\return #\newline #\tab)))
		     ;; default case: 
		     (t
		      (unread-char next-char *stream*)
		      (let ((symbol (read *stream*)))
			;; if it's a number just push it on
			(cond ((numberp symbol)
			       (push symbol s))
			      ;; otherwise 
			      (t (let ((str (string symbol)))
				   (push (intern
					  (string-right-trim '(#\.) str))
					 s)
				   (cond ((char= '#\. (char str (1- (length str))))
					  (push '*full-stop* s)
					  (if (equal (peek-char nil *stream* nil) #\newline)
					      (push '*paragraph* s)))))))))))))

    ;; return the reverse of sentence s (which was built in reverse
    ;; using push.)
    (reverse s)))

;;;**********************************************************************
;;; HELL: the infernal source code
;;;**********************************************************************

;;; Searches for a concept with one of the given classes in the given
;;; direction. The found concept is bound to the given gap. Search
;;; stops at a boundary (such as a clause or end of sentence)
(demon exp ; search within boundary
  (params myconcept mygap classes dir)
  (comment (test "Search for a CONCEPT with one of the given CLASSES"
		 "in the given DIRECTION until a boundary is reached.")
	   (act "The CONCEPT is bound to the given GAP."))
  (kill (eval mygap))
  (test (mcsearch (lambda (con)
		   (if (not (equal con myconcept))
		       (con-class? con classes)))
		myconcept 'stop-at-conjunction dir))
  (+act (link myconcept mygap test)))

;;; Stops at a boundary and returns the local character if the
;;; boundary is a conjuctive. This is done to handle elisions.
(defun stop-at-conjunction (c)
  (or (and (con-class? c '*conjunction*) ; created by conjuncts/punctuation
	   (cond ((class? 'human classes) *local-char*)))
      (or  (con-class? c 'boundary))))

;;; Saves actors in global variables for pronoun reference
(demon save-character
  (params myconcept)
  (comment (act "Bind the MYCONCEPT to the MOST-RECENT-CHARACTER, if there"
		"is no LOCAL-CHARACTER bind the MYCONCEPT to it also"))
  (+act (setf *most-recent-char* myconcept)
	(if (null *local-char*) (setf *local-char* myconcept))))

;;; Disambiguation for between "ex" on its own and "ex" as a modifier
(demon ex-x?
  (params myconcept)
  (comment (test "If the next word is a ...")
	   (act "Set the MYCONCEPT to the grasp configuration"))
  (kill (eval myconcept))
  (test (or (equal *next-word* 'boyfriend) (equal *next-word* 'girlfriend) (equal *next-word* 'bf) (equal *next-word* 'gf)) ; this allows limited 'look-ahead' ability
  (+act (con-set myconcept (get 'picked 'm1)))))

;;; Disambiguation for "pick-up"
; NOTE: doesn't consider case of "John picked up a girl at a bar."
(demon pick-up?
  (params myconcept)
  (comment (test "If the next word is UP...")
	   (act "Set the MYCONCEPT to the grasp configuration"))
  (kill (eval myconcept))
  (test (equal *next-word* 'up)) ; this allows limited 'look-ahead' ability
  (+act (con-set myconcept (get 'picked 'm1))))

;;; Disambiguation for "pick" to mean "choose" in the sense of "deciding."
(demon decide?
  (params myconcept)
  (comment (test "Search for a person or a physical object after the MYCONCEPT")
	   (act "Set the MYCONCEPT to the decision configuration"))
  (kill (eval myconcept))
  (test (mcsearch (lambda (con)
		   (and (not (equal con myconcept))
			(con-class? con '(human physical-object))))
		myconcept nil 'after))
  (+act (con-set myconcept (get 'pick 'm2))))


;;; Disambiguation for different grammatical configurations for dated
(demon dated-trans?
  (params myconcept)
  (comment (test "Search to see if there is a person just after")
	   (act "Set the MYCONCEPT to the transitive or intransitive configuration"))
  (kill (eval myconcept))
  (test (mcsearch (lambda (con)
		   (and (not (equal con myconcept))
			(con-class? con '(human))))
		myconcept nil 'after))
  (+act (con-set myconcept (get 'dated 'm1)))
  (-act (con-set myconcept (get 'dated 'm2)))
)

;;; Put an IGNOR property on the concept (to indicate that it has been
;;; processed and can be ignored.)
(demon ignore
  (params myconcept)
  (+act (setf (get myconcept 'ignore) T)))

;;; Saves objects in a global variable for pronoun reference.
(demon save-object
  (params myconcept)
  (comment (act "Bind the MYCONCEPT to the MOST-RECENT-OBJECT"))
  (+act (setf *most-recent-object* myconcept)))

;;; In the trace, prepositions are treated like modifiers. E.G. "with
;;; Mary" causes the concept for "mary" to be 'modified' by "PREP-OBJ
;;; prep". Demons sensitive to prepositions can the search for a class
;;; of object modifiers by prepositions. (This is not the only way to
;;; do it, of course.)
(demon preposition
  (params myconcept mygap prepositions classes dir)
  (comment (test "Search for a CONCEPT with one of the given CLASSES and"
		 "preceeded by one of the given PREPOSITIONS.")
	   (act "The CONCEPT is bound to the given GAP."))
  (kill (eval mygap))
  (test (preposition-search myconcept prepositions classes dir))
  (+act (link myconcept mygap test)))

;;; (all search functions should be defined in terms of the McDypar
;;; function search.)
(defun preposition-search (myconcept prepositions classes dir)
  (mcsearch (lambda (c)
	     (and (not (equal c myconcept))
		  (con-class? c classes)
		  (cond ((atom prepositions) (equal (path '(preposition-obj is *) c)
					     prepositions))
			(T (member (path '(preposition-obj is *) c)
				   prepositions)))))
	  myconcept nil dir))

(demon find-object-ref
  (params myconcept)
  (comment (act "Set the MYCONCEPT to the most recently mentioned object."))
  (+act (set1 myconcept *most-recent-object*)))

(demon find-female-ref
  (params myconcept)
  (comment (act "Set the MYCONCEPT to the most recently mentioned female."))
  (test (mcsearch (lambda (con)
		   (and (not (equal con myconcept))
			(con-class? con '(human))
			(equal (path '(gender) con) '(female))
			))
		myconcept nil 'before))
  (+act (set1 myconcept test)))


;;; This demon performs modifications on other concepts in *working-memory* by
;;; adding a new slot gap pair to it.
(demon insert-after
  (params myconcept classes slot)
  (comment (test "Search for a CONCEPT with one of the given CLASSES.")
	   (act "Insert the given SLOT with the MYCONCEPT as its GAP into"
		"the CONCEPT"))
  (test (mcsearch (lambda (con)
		   (and (not (equal con myconcept))
			(con-class? con classes)))
		myconcept nil 'after))
  (+act (set1 test (append (eval test) (list slot myconcept)))
	(setf (get myconcept 'inside) (list test slot))))



;;;**********************************************************************
;;; TEST LEXICON
;;;**********************************************************************
;;; These words are in the lexicon for the purpose of testing the
;;; parser (as transliterated from TLISP to Kyoto Common LISP) on the
;;; sentence "Mary picked up the ball and dropped it in the box".

(word john
  def (human name (john)
	     gender (male))
  demons (save-character))

(word mary
  def (human name (mary)
	     gender (female))
  demons (save-character))

(word picked
  demons ((pick-up?) (decide?))
  m1 (grasp actor HUMAN-GAP <== (exp 'human 'before)
	    object OBJECT-GAP <== (exp 'physical-object 'after)
	    instr (move actor HUMAN-GAP
			object (fingers)
			to OBJECT-GAP))
  m2 (mbuild actor * <== (exp 'human 'before)
	     mobj (poss actor * <== (exp 'human 'before)
			object * <== (exp '(human physical-ob) 'after)))
)

(word gave
  def (atrans
        actor * <== (exp 'human 'before)
        to * <== (exp 'human 'after)
        object * <== (exp 'physical-object 'after))
)

(word up
      demons (ignore))

(word the
  demons (ignore)
)

(word a
  demons (ignore)
)

(word book
  def (physical-object class (book)
		name (book))
  demons (save-object))

(word ball
  def (physical-object class (game-object)
		name (ball))
  demons (save-object))

(word and
  def (*conjunction*)
  demons (ignore))

(word dropped
  def (ptrans actor * <==(exp 'human 'before)
	      object THING-GAP <==(exp 'physical-object 'after)
	      to * <==(preposition '(in into on) '(human physical-object) 'after)
	      instr (propel actor (gravity)
			    object THING-GAP)))


(word it
  demons (find-object-ref)
)

(word in
  def (preposition is (in))
  ;; inserts the slot "preposition-obj" with the gap "(preposition is (in))"
  ;; the "preposition" demon in dropped is looking for the path "preposition-obj is *"
  demons (insert-after '(physical-object setting) 'preposition-obj)
  )

(word box
  def (physical-object class (container)
		name (box))
)