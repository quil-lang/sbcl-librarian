(in-package #:sbcl-librarian)

;; TODO: create a custom ASDF operation instead of monkey-patching
;; PREPARE-BUNDLE-OP
(defun call-with-recursive-compile-bundle-op (thunk)
  "Call THUNK in a dynamic context where performing
COMPILE-BUNDLE-OP on a target system recursively performs
COMPILE-BUNDLE-OP on all the systems required by the target
system. This is necessary because the the SIDEWAY-OPERATION of
PREPARE-BUNDLE-OP, which is the SELFWARD-OPERATION of
COMPILE-BUNDLE-OP, is LOAD-OP on SBCL instead of LOAD-BUNDLE-OP[^1].

[^1]: https://github.com/sbcl/sbcl/blob/sbcl-2.4.0/contrib/asdf/asdf.lisp#L3972"
  (let ((old-op (slot-value (asdf:make-operation 'asdf:prepare-bundle-op) 'asdf:sideway-operation)))
    (setf (slot-value (asdf:make-operation 'asdf:prepare-bundle-op) 'asdf:sideway-operation) 'asdf:compile-bundle-op)
    (unwind-protect
         (values (funcall thunk))
      (setf (slot-value (asdf:make-operation 'asdf:prepare-bundle-op) 'asdf:sideway-operation) old-op))))

(defmacro with-recursive-compile-bundle-op (&body body)
  "Syntactic sugar for CALL-WITH-RECURSIVE-COMPILE-BUNDLE-OP."
  `(call-with-recursive-compile-bundle-op
    (lambda ()
      ,@body)))

(defun call-with-output-translations (output-translations thunk)
  "Call THUNK in a dynamic context where ASDF's output translations are
initialized to OUTPUT-TRANSLATIONS."
  (asdf:initialize-output-translations output-translations)
  (unwind-protect
       (funcall thunk)
    (asdf:clear-output-translations)))

(defmacro with-output-translations (output-translations &body body)
  "Syntactic sugar for CALL-WITH-OUTPUT-TRANSLATIONS."
  `(call-with-output-translations
    ,output-translations
    (lambda ()
      ,@body)))

(defun compile-bundle-system-with-dependencies (target-system directory)
  "Compile TARGET-SYSTEM and all of its required systems into a single
FASL bundle file per system, placing the FASL bundle files in
DIRECTORY."
  (with-output-translations
      `(:output-translations
        :inherit-configuration
        ,@(loop :for system :in (cons target-system (system-dependencies-in-load-order target-system))
                :for fasl-filename := (system-fasl-bundle-filename system)
                :when (system-loadable-from-fasl-p system)
                  :collect `((,(asdf:component-pathname system) ,fasl-filename)
                             (,directory ,(directory-namestring fasl-filename)))))
    (with-recursive-compile-bundle-op
      (asdf:oos 'asdf:compile-bundle-op target-system))))

(defun system-dependencies-in-load-order (system)
  "Return a list of all the required systems for SYSTEM topologically
sorted by load order."
  (with-recursive-compile-bundle-op
    ;; ASDF:REQUIRED-COMPONENTS traverses the dependency operations
    ;; required to perform :GOAL-OPERATION on the provided system,
    ;; returning a list of the components involved that have
    ;; :COMPONENT-TYPE.
    ;;
    ;; TODO: what exactly do :OTHER-SYSTEMS and :KEEP-OPERATION do?
    (asdf:required-components system
                              :other-systems t
                              :component-type 'asdf:system
                              :goal-operation 'asdf:compile-bundle-op
                              :keep-operation 'asdf:compile-bundle-op)))

(defun system-fasl-bundle-filename (system)
  "Returns the name of the FASL bundle file produced by performing
COMPILE-BUNDLE-OP on SYSTEM."
  (let ((files (asdf:output-files 'asdf:compile-bundle-op system)))
    (if (null files)
        nil
        (uiop:parse-native-namestring
         (concatenate 'string (asdf:component-name system) "--system.fasl")))))

(defun require-system-p (system)
  "Returns T if SYSTEM is a REQUIRE-SYSTEM, otherwise returns NIL."
  (typep system 'asdf:require-system))

(defun system-loadable-from-fasl-p (system)
  "Returns T if SYSTEM is not a REQUIRE-SYSTEM and performing
COMPILE-BUNDLE-OP on it produces an output file, otherwise returns
NIL."
  (not (or (require-system-p system)
           (null (asdf:output-files 'asdf:compile-bundle-op system)))))
