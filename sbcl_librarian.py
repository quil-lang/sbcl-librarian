from ctypes import *
import sys

from importlib.abc import Loader, MetaPathFinder
from importlib.util import module_from_spec, spec_from_loader


class LispFinder(MetaPathFinder):
	def __init__(self):
		self.pycore = CDLL("pycore.dylib", mode=RTLD_GLOBAL)
		self.pycore.init()
		self.pycore._list_python_modules.restype = c_char_p

	def find_spec(self, fullname, path, target=None):
		s = self.pycore._list_python_modules()
		if fullname in s.decode("utf-8").split(", "):
			return spec_from_loader(fullname, LispLoader(self.pycore))
		else:
			return None


class LispLoader(Loader):
	def __init__(self, pycore):
		self.pycore = pycore

	def create_module(self, spec):
		return None

	def exec_module(self, module):
		self.pycore._exec_module(py_object(module))


sys.meta_path.insert(0, LispFinder())
