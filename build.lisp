(require :sb-tcc)

(defparameter *python-include-path*
  "/opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/include/python3.12")

(defparameter *prog*
  "
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdio.h>

PyObject *square_it(PyObject *self, PyObject *args) {
  int x;
  printf(\"hello from square_it\\n\");
  if (!PyArg_ParseTuple(args, \"i\", &x))
    return NULL;
  return PyLong_FromLong((long) x * x);
}

PyMethodDef methods[] = {
  {\"square_it\", square_it, METH_VARARGS, \"square a number\"},
  {NULL, NULL, 0, NULL}
};

void exec_mod(PyObject *module) {
  PyGILState_STATE state = PyGILState_Ensure();
  printf(\"%p\\n\", square_it);
  PyObject_SetAttrString(module, \"dog\", PyLong_FromLong(42));
  printf(\"%d\\n\", PyModule_AddFunctions(module, methods));
  PyGILState_Release(state);
}
")

(defparameter *state* nil)

(define-alien-callable list-python-modules c-string ()
  "hello, dude, sup")

(define-alien-callable exec-module void ((module (* t)))
  (let ((state (sb-tcc:tcc-new)))
    (sb-tcc:tcc-set-output-type state sb-tcc:tcc-output-memory)
    (sb-tcc:tcc-add-include-path state *python-include-path*)
    (sb-tcc:tcc-compile-string state *prog*)
    (sb-tcc:tcc-relocate state nil)
    (let ((exec-mod (sb-tcc:tcc-get-symbol state "exec_mod")))
      (format t "~a~%" (sb-tcc:tcc-get-symbol state "square_it"))
      (finish-output)
      (alien-funcall (cast exec-mod (function void (* t)))
                     module))
    (setf *state* state)))

(save-lisp-and-die "pycore.core" :callable-exports '(list-python-modules exec-module))
