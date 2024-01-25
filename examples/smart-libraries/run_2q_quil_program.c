#include <stdio.h>

#include "libbase.h"
#include "libminiquilc.h"
#include "libminiqvm.h"

int main(void)
{
    init("build/libbase/libbase.core");
    libminiquilc_load();
    libminiqvm_load();

    char source[256];
    char *amplitudes;
    quil_program program;
    qvm qvm;

    fgets(source, 256, stdin);
    miniquilc_parse_quil(source, &program);
    miniqvm_run_program(2, program, &qvm);
    miniqvm_amplitudes(qvm, &amplitudes);
    printf("%s\n", amplitudes);

    return 0;
}
