#include <stdio.h>

extern int (*square)(int x);

int main(void)
{
    printf("hello %d\n", square(9));

    return 0;
}
