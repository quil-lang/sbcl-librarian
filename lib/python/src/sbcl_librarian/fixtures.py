import gc
from typing import Generator

import pytest

import sbcl_librarian.debug as debug


@pytest.fixture
def check_handle_leaks() -> Generator[None, None, None]:
    """
    Fixture to check that all lisp handles are freed by the end of a test.

    Useful for ensuring that memory leaks within lisp do not occur.
    """

    # Perform a GC to ensure any previously used handles are cleaned up
    gc.collect()

    # Check that there are no active handles before the test
    pre_count = debug.handle_count()

    yield

    # Perform a GC to ensure any handles are cleaned up
    gc.collect()

    # Check that no new handles were created
    post_count = debug.handle_count()
    assert post_count == pre_count, f"{post_count - pre_count} lisp handles were leaked."

    return None
