extern int (*funcall0_by_name)(const char *name);
extern void (*set_argv)(int argc, char **argv);
extern void (*load_array_as_system)(char *data, int size, char *system_name);
extern void (*load_shared_object)(char *pathname);
