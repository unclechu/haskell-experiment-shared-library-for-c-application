#include <HsFFI.h>
#include <stdio.h>

extern HsInt32 fooForeign(HsInt32 a1);

int main()
{
  printf("hi there\n");
  int i;
  i = fooForeign(25);
  printf("fooForeign of 25: %d\n", i);
  return 0;
}
