/* #include <dlfcn.h> */
#include <stdio.h>

#include <HsFFI.h>
extern void __stginit_Foo(void);
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
  hs_init(0, NULL);
  hs_add_root(__stginit_Foo);
  i = fooForeign(25);
  printf("fooForeign of 25: %d\n", i);
  hs_exit();

  return 0;
}
