import importlib.metadata
from pathlib import Path

try:
    path = Path(__file__).parents[3] / "VERSION.txt"
    with Path.open(path, "r") as f:
        __version__ = f.readline().strip()
except FileNotFoundError:
    __version__ = importlib.metadata.version(__name__.split(".")[0])
