;;; read-bib-file-svs.lisp
;;; 30-Apr-2020 SVS

(asdf:load-system :key-value-store)

(in-package :bibtex-runtime)

(defun read-bib-file (pathname)
  (setq *bib-macros* (make-hash-table :test #'equalp))
  (setq *bib-database* (make-hash-table :test #'equalp))
  (let ((expanded-file (merge-pathnames pathname #P".bib")))
    (with-open-file (s expanded-file :if-does-not-exist nil)
      (read-bib-database s)))
  *bib-database*)

; (read-bib-file #P"/Users/svspire/git/ep4/html/bib/Covid-19-2020-04-29.bib")


(defun find-types (arg)
  "Figure out what the bib-entry-types are, and how many we have of each"
  (unless (typep arg 'hash-table)
    (setf arg (read-bib-file arg)))
  (let ((table (kvs:make-store ':hashtable :test #'equal)))
    (maphash (lambda (key value)
               (declare (ignore key))
               (kvs:tally! table (bib-entry-type value) 1))
             arg)
    (values table arg)))

#|
? (showhash (find-types *bib-database*))

"book" : 2
"article" : 642
"unpublished" : 3
"webpage" : 75
"booklet" : 1
"electronic" : 2
"techreport" : 37
"news" : 35
"misc" : 5
|#
    
(defun find-slots (bib)
  "Determines which slots each type in the bibliography should have.
  Calls find-types to determine the high-level types of publications in the
  bibliography. For each type, it collects the union of all the slots (keys)
  for all the instances of that type."
  (let ((slotnames-for-types (kvs:make-store ':hashtable :test #'equal)))
    (multiple-value-bind (types bib)
                         (find-types bib)
      (maphash (lambda (key bib-entry)
                 (declare (ignore key))
                 (let* ((type (bib-entry-type bib-entry))
                        (dict (bib-entry-dict bib-entry))
                        (newslots (when dict (loop for key being each hash-key of dict collect key)))
                        (slots-so-far (gethash type slotnames-for-types)))
                   (setf (gethash type slotnames-for-types) (union slots-so-far newslots :test #'equal))))
               bib)
      ; let's sort the slotnames to make it easier to compare them
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (setf (gethash key slotnames-for-types)
                       (sort (gethash key slotnames-for-types) #'string-lessp)))
               types)
      (values slotnames-for-types bib))))

;(showhash (find-slots *bib-database*))

(defun common-slots (bib)
  "Determine which slotnames are common for every publication type. This
   is useful for determining the root class of publications."
  (multiple-value-bind (slotnames-for-types bib)
                       (find-slots bib)
    (let ((slotname-lists (loop for slotnames being each hash-value of slotnames-for-types collect slotnames)))
    (values (sort (reduce (lambda (accum next) (intersection accum next :test #'equalp)) slotname-lists)
                  #'string-lessp)
           bib
           slotnames-for-types))))

; (common-slots *bib-database*)
;  ("Abstract" "Author" "Bdsk-Url-1" "Date-Added" "Date-Modified" "Keywords" "Month" "Title" "Year")

(defun canonicalize-name (name)
  (string-upcase (string name)))

(defun expand-slot-definition (slotname)
  (setf slotname (canonicalize-name slotname))
  (let ((symbol (intern slotname)))
    (list symbol :initarg (intern slotname :keyword) :initform nil :accessor symbol)))

(defun expand-class-definition (classname slotnames)
  (let ((symbol (intern (canonicalize-name classname))))
    (list 'defclass symbol ()
          (loop for slot in slotnames collect (expand-slot-definition slot)))))

(defun make-class-definitions (bib)
  "Makes class definitions representing publications in bib.
  Bib can either be the *bib-database* hashtable or the path to a .bib file."
  (multiple-value-bind (base-class-slotnames bib slotnames-for-types)
                       (common-slots bib)
    (flet ((unique-slots (slotlist)
             (set-difference slotlist base-class-slotnames :test #'equalp)))
      (values (list* (expand-class-definition 'publication base-class-slotnames)
                     (loop for type being each hash-key of slotnames-for-types
                       using (hash-value slots) collect
                       (expand-class-definition (canonicalize-name type)
                                                (unique-slots slots))))
              bib))))

; (make-class-definitions *bib-database*)
;  -->  <list of defclass forms>

(defun print-class-definitions (bib)
  (let ((classdefs (make-class-definitions bib))
        (*print-case* :downcase)
        (*package* (find-package :bibtex-runtime)))
    (pprint classdefs)))

; (print-class-definitions *bib-database*)
  