"""Functions for debugging Lisp"""

import ctypes
import platform
from enum import Enum

import sbcl_librarian.raw


def enable_backtrace() -> None:
    """
    Enable printing backtraces when Lisp errors are
    signaled.
    """
    sbcl_librarian.raw.enable_backtrace(1)


def disable_backtrace() -> None:
    """
    Disable printing backtraces when Lisp errors are
    signaled.
    """
    sbcl_librarian.raw.enable_backtrace(0)


def disable_debugger() -> None:
    """Disable the Lisp debugger"""
    sbcl_librarian.raw.lisp_disable_debugger()


def enable_debugger() -> None:
    """Enable the lisp debugger"""
    sbcl_librarian.raw.lisp_enable_debugger()


def gc() -> None:
    """Explicitly run the Lisp garbage collector."""
    sbcl_librarian.raw.lisp_gc()


def memory_report() -> None:
    """Print a report about memory use to standard out."""
    sbcl_librarian.raw.lisp_memory_report()


def lisp_memory() -> int:
    """Return the memory currently used by the Lisp process"""
    result = ctypes.c_ulonglong()
    sbcl_librarian.raw.lisp_dynamic_usage(result)
    return result.value


def start_swank_server(port: int) -> None:
    """
    Start a swank server so that Lisp devs can interact with the Lisp
    process directly.
    """
    if port <= 1024:
        raise ValueError("Port should be greater than 1024")

    sbcl_librarian.raw.lisp_start_swank_server(port)


def crash() -> None:
    """Signal crash in the Lisp process."""
    sbcl_librarian.raw.crash()


ProfilingMode = Enum("ProfilingMode", [":CPU", ":ALLOC", ":TIME"])


def start_profiling(
    max_samples: int = 500,
    mode: ProfilingMode = ProfilingMode[":CPU"],
    sample_interval: float = 0.01,
) -> None:
    """Start profiling the Lisp image.

    `max_samples`: The meximum number of stack traces to collect.

    `mode`: :CPU profiles cpu time, :TIME profiles wall clock time,
    :ALLOC traces thread-local allocation region overflow handlers.

    `sample_interval`: the number of seconds between samples.
    """

    if platform.system() == "Windows":
        raise Exception("Profiling is not supported on windows")

    if not max_samples > 0:
        raise ValueError(f"max_samples must be greater than zero, not {max_samples}")

    if not sample_interval > 0.0:
        raise ValueError(f"sample_interval must be greater than zero, not {sample_interval}")

    args = (
        f"(:max-samples {max_samples} :mode {mode.name} :sample-interval {sample_interval})".encode(
            "utf-8"
        )
    )
    sbcl_librarian.raw.lisp_start_profiling(args)


def stop_profiling() -> None:
    """Stop the profiler."""
    if platform.system() == "Windows":
        raise Exception("Profiling is not supported on windows")

    sbcl_librarian.raw.lisp_stop_profiling()


def profiler_report() -> None:
    """Print a report of the results of profiling."""
    if platform.system() == "Windows":
        raise Exception("Profiling is not supported on windows")

    sbcl_librarian.raw.lisp_profiler_report("(:type :FLAT)".encode("utf-8"))


def profiler_reset() -> None:
    """Reset the proflier."""
    if platform.system() == "Windows":
        raise Exception("Profiling is not supported on windows")

    sbcl_librarian.raw.lisp_reset_profiler()


def handle_count() -> int:
    """Get the number of (apparently living) handles to Lisp objects."""
    result = ctypes.c_int()
    sbcl_librarian.raw.lisp_handle_count(result)
    return result.value


def print_error() -> None:
    """Print the most recent Lisp error messages."""
    result = ctypes.c_char_p()
    sbcl_librarian.raw.get_error_message(result)
    if result.value is not None:
        print(result.value.decode("utf-8"))  # noqa: T201
