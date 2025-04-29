(defun act-header-p (cd)
  "act-header-p stands for act header predicate.  Returns
 non-NIL if the header is a conceptual dependency act primitive
 header, NIL otherwise.  May need updating later."
  (member cd '(ptrans move ingest expel grasp propel atrans attend mtrans mbuild speak)))

(defun convert-act-header (cd)
  (cadr (assoc cd '((ptrans *ptrans*)(move *move*)(ingest *ingest*)
                     (expel *expel*)(grasp *grasp*)(propel *propel*)
                     (atrans *atrans*)(attend *attend*)(mtrans *mtrans*)
                     (mbuild  *mbuild*)(speak *speak*)
                     ))))


(defun pp-header-p (cd)
  "pp-header-p stands for picture producer header predicate.  Returns
 non-NIL if the header is a PP header, NIL otherwise.  May need
 updating later."
  (member cd '(physical-object human)))

(defun convert-pp-header (cd)
  "For now, this will extract Mary from (human name (mary)), and
 similar for other PPs, and will work for conversion"
  (caddr cd))

(defun mcdypar-convert1 (x)
  (cond
    ((consp x) (mcdypar-convert-header-and-rest x))
    (T x)
    ))

(defun mcdypar-convert-header-and-rest (cd)
  (let ((header (car cd))
         (roles (cdr cd)))
    (cond
      ((pp-header-p header) (convert-pp-header cd))
      ((act-header-p header)
        (setq header (list (convert-act-header header)))
        (setq roles (mapcar #'(lambda (y) (mcdypar-convert1 y)) roles)) ;; convert 
        (append (list '<=> header) roles)))))

(defun mcdypar-convert-cd-to-babel-style (cd)
  (list (list (mcdypar-convert1 cd) 'time '(t-2))))

(defun get-cd ()
  (expand
    (car
      (do ((con *working-memory* (get con 'before))
	    (unused NIL))
	((null con) unused)
	(if (and (not (get con 'inside))
	      (not (get con 'ignore)))
	  (push con unused))))))


;; (mcdypar-convert-cd-to-babel-style '(PTRANS ACTOR (HUMAN NAME (MARY)) OBJECT (PHYSICAL-OBJECT NAME (BALL)) TO NIL))

;; (express (mcdypar-convert-cd-to-babel-style '(PTRANS ACTOR (HUMAN NAME (MARY)) OBJECT (PHYSICAL-OBJECT NAME (BALL)) TO NIL)))

;; (get-cd)
;; (express (mcdypar-convert-cd-to-babel-style (get-cd)))

