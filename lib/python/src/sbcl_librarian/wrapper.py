import ctypes
import functools
import logging
import pickle
import platform
import signal
import sys
from typing import Any, Callable

import _signal  # type: ignore

import sbcl_librarian.raw
import sbcl_librarian.errors


logger = logging.getLogger(__name__)


class LispError(Exception):
    pass


class LispBug(Exception):
    pass


class LispFatal(Exception):
    pass


class LispNotInitialized(Exception):
    pass


LispHandle = ctypes.c_void_p


class LispObject:
    """A base class for wrapped Lisp objects.

    Each such Python object maintains a handle to a Lisp object.
    This handle is, by default, released on Python garbage collection.
    """

    def __init__(self, handle: LispHandle, release: bool = True):
        """Construct a new LispObject.

        Args:
          handle: the handle to wrap
          release: if True, release the handle when this object is deleted
        """
        self._handle = handle
        self._release = release

    @property
    def handle(self) -> LispHandle:
        return self._handle

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, LispObject) and sbcl_librarian.raw.lisp_handle_eq(
            self.handle, other.handle
        )

    def __del__(self) -> None:
        if self._release:
            # https://stackoverflow.com/questions/8590238/unable-to-reference-an-imported-module-in-del
            try:  # noqa: SIM105
                sbcl_librarian.raw.lisp_release_handle(self.handle)
            except AttributeError:
                pass

    def __getstate__(self) -> None:
        raise pickle.PicklingError("Unable to pickle Lisp object.")


def lift_fn(name: str, fn: Callable[..., Any]) -> Callable[..., Any]:
    def safe_call(args: Any, kwargs: Any) -> Any:
        args = tuple(a.handle if isinstance(a, LispObject) else a for a in args)
        logger.debug("%s%s", name, args)
        return fn(*args, **kwargs)

    @functools.wraps(fn)
    def with_exceptions(*args: Any, **kwargs: Any) -> Any:
        result = safe_call(args, kwargs)
        if result != 0:
            if result == 3:
                raise LispFatal(
                    "SBCL crashed with a fatal, non-recoverable error. All subsequent calls into Lisp will raise the same exception."
                )
            elif result == 4:
                raise LispNotInitialized(
                    "The SBCL runtime must be initialized before calling into Lisp."
                )
            msg = ctypes.c_char_p()
            sbcl_librarian.raw.get_error_message(ctypes.byref(msg))
            if result == 1:
                raise LispError(msg.value and msg.value.decode("utf-8"))
            else:
                raise LispBug(msg.value and msg.value.decode("utf-8"))
        return result

    return with_exceptions


__all__ = [
    "LispError",
    "LispBug",
    "LispHandle",
    "LispObject",
    "lift_fn"
]
