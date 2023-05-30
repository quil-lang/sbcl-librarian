;;;; project.lisp -- generate project templates

(in-package :sbcl-librarian.project)

;;; MAKE-PROJECT 

(defun make-project (system
                     &key
                       libname
                       (description (format nil "Shared Library for ~a " system))
                       (author "Your Name <mail@domain.com>")
                       (prefix "lib")
                       (directory
                        (asdf:system-relative-pathname system "lib/" ))
                       (template-directory
                        (asdf:system-relative-pathname :sbcl-librarian/project "template/")))
  "Create a project skeleton - a Makefile, a README, and some project
files - for building a shared library out of an existing lisp system.

SYSTEM is a symbol, keyword, or ASDF/SYSTEM:SYSTEM instance.

DIRECTORY is the location where the skeleton will be generated. By
default it will reside in a <system-directory>/lib/.

PREFIX is a string. A new system will be generated whose name is the
name of the SYSTEM prefixed by PREFIX

LIBNAME is a string. If provided, this will be the name of the
library.

TEMPLATE-DIRECTORY is a path to a directory containing template files,
by default this is the `template/` directory relative to the directory
containing sbcl-librarian"
  (when (uiop:directory-exists-p directory)
    (print "signal an error here"))
  (ensure-directories-exist directory)

  (let ((libname
          ;; The libsystem name is a string used as an argument to
          ;; templates. If your system is goobers, then libsystem-name
          ;; will be libgoobers by default.
          (or libname
              (string-downcase
               (concatenate 'string prefix (if (symbolp system) (symbol-name system)
                                               (slot-value system 'asdf::name)))))))
    (walk-directory-tree
     template-directory
     (lambda (template)
       (let ((expanded
               (translate-rel-pathname template template-directory directory)))
         (expand-template template expanded
                          :system system
                          :library libname
                          :clibrary (kabob->snake (princ-to-string libname))
                          :description description
                          :author author))))
    (rename-system-definition directory libname)

    (format t "New sbcl-librarian project in ~a~%" directory)))


;;; UTILITIES
;;;
;;; These functions are a ramshackle assembly of utilities that
;;; implement the template filling funcitonality. Forgive their
;;; ad-hocicity, and replace them as desired.

(defun rename-system-definition (directory libname)
  (rename-file (merge-pathnames "system.asd" directory)
               (merge-pathnames (string-downcase
                                 (format nil "~a.asd" libname))
                                directory)))

(defun expand-template (template expanded &rest args)
  "The stupidest posible template expansion function. It tries to assume
very little so that, if needed, it can be later refactored."
  (a:write-string-into-file 
   (format-template-expand
    (a:read-file-into-string template)
    args)
   expanded))

(defun join (strings sep)
  "join a list of strings together."
  (with-output-to-string (out)
    (loop :for (str . more) :on strings
          :do (write-string str out)
          :when more
            :do (princ sep out))))

(defun split (string sep)
  "Split a string on a substring."
  (loop
    :with len := (length sep)
    :with lastpos := 0
    :for pos := (search sep string :start2 lastpos)
    :when pos
      :collect (subseq string lastpos pos) :into parts
      :and :do (setf lastpos (+ pos len))
    :else
      :collect (subseq string lastpos) :into parts
      :and :return parts))

(defun replace-all (source match replacement)
  (join (split source match) replacement))

(defun kabob->snake (str)
  (substitute #\_ #\- str))

(defun format-template-expand (template-string env)
  "A crude-as-can-be template expansion function."
  (assert (evenp (length env)))
  (loop
    :with str := template-string
    :for (var val) :on env :by #'cddr
    :do (setf str (replace-all
                   str
                   (format nil "{{~a}}" var)
                   val))
    :finally (return str)))

(defun walk-directory-tree (dir fn)
  "Apply FN to each NON-DIRECTORY file under filesystem tree rooted at
the directory DIR."
  (map nil fn (uiop:directory-files dir))
  (dolist (subdir (uiop:subdirectories dir))
    (walk-directory-tree subdir fn)))

(defun translate-rel-pathname (fullpath from-tree to-tree)
  "Given a pathname like 
/foo/bar/goo/zar/file.txt

a parent path like
/foo/bar/goo/ 

and a target parent like 
/moo/cow/ 

return a pathname
/moo/cow/zar/file.txt"
  (uiop:merge-pathnames* (enough-namestring fullpath from-tree) to-tree))
