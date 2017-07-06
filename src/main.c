#include <HsFFI.h>

#include "Foo_stub.h"
#include "Bar_stub.h"
extern void __stginit_Foo(void);
extern void __stginit_Bar(void);

#include <stdio.h>

int main(int argc, char *argv[])
{
  int i;
  hs_init(0, NULL);
  hs_add_root(__stginit_Foo);
  hs_add_root(__stginit_Bar);

  i = fooForeign(25);
  printf("fooForeign of 25: %d\n", i);
  i = barForeign(25);
  printf("barForeign of 25: %d\n", i);

  hs_exit();
  return 0;
}
