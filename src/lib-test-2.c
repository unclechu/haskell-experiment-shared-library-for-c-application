#include <HsFFI.h>
#include <dlfcn.h>
#include <stdio.h>

int main()
{
  void* dl = dlopen("./dist/libfoo.so", RTLD_LAZY);
  if (!dl) {
    printf("Cannot open library 'libfoo.so': %s\n", dlerror());
    return 1;
  }

  dlerror(); // reset errors
  HsInt32 (*foo)(HsInt32 a1) = dlsym(dl, "fooForeign");
  const char *dlsym_error = dlerror();
  if (dlsym_error) {
    printf("Cannot load symbol 'fooForeign' from 'libfoo.so': %s\n", dlsym_error);
    dlclose(dl);
    return 1;
  }

  printf("hi there (2)\n");
  int i;
  i = foo(30);
  printf("fooForeign of 30: %d\n", i);

  return 0;
}

