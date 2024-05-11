const char *(*list_python_modules)(void);
void (*exec_module)(void *module);

const char *_list_python_modules(void) { return list_python_modules(); }
void _exec_module(void *module) { return exec_module(module); }

extern int initialize_lisp(int argc, char *argv[]);

int init(void)
{
	char *argv[4] = {"", "--core", "pycore.core", "--noinform"};

	return initialize_lisp(4, argv);
}
