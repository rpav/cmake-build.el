#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[])
{
    if(argc && !strcmp(argv[1], "--with-cmake-build-el"))
        printf("Congrats, cmake-build.el is set up and working.\n");
    else
        printf("Didn't use cmake-build.el?\n");
}
