#include <stdlib.h>
#include <HsFFI.h>

#define STR(a) XSTR(a)
#define XSTR(a) #a
#define CAT(a, b) XCAT(a, b)
#define XCAT(a, b) a ## b

extern void CAT (__stginit_, MODULE) (void);

static void library_init (void) __attribute__ ((constructor));
static void
library_init (void)
{
  hs_init(0, NULL);
  hs_add_root(CAT (__stginit_, MODULE));
}

static void library_exit (void) __attribute__ ((destructor));
static void
library_exit (void)
{
  hs_exit();
}
