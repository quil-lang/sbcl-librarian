class lisp_err_t(int):
    _map = {
        0: "LISP_ERR_SUCCESS",
        1: "LISP_ERR_FAILURE",
        2: "LISP_ERR_BUG",
        3: "LISP_ERR_FATAL",
        4: "LISP_ERR_NOT_INITIALIZED",
    }
