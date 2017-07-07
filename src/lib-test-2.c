
// TODO

/* #include <dlfcn.h> */
#include <HsFFI.h>
#include <stdio.h>

extern HsInt32 fooForeign(HsInt32 a1);

int main()
{
  /* void* handle = dlopen("./dist/libfoo.so", RTLD_LAZY); */
  /* if (!handle) { */
  /*   printf("cannot open library libfoo.so\n"); */
  /*   return 1; */
  /* } */

  printf("hi there\n");

  int i;
  i = fooForeign(25);
  printf("fooForeign of 25: %d\n", i);

  return 0;
}

