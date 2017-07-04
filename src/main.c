#include <HsFFI.h>

#ifdef __GLASGOW_HASKELL__
#include "Foo_stub.h"
extern void __stginit_Foo(void);
#endif

#include <stdio.h>

int main(int argc, char *argv[])
{
	int i;
	hs_init(0, NULL);
#ifdef __GLASGOW_HASKELL__
	hs_add_root(__stginit_Foo);
#endif

	i = fooForeign(25);
	printf("fooForeign of 25: %d\n", i);

	hs_exit();
	return 0;
}
