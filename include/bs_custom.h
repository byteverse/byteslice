#define _GNU_SOURCE

#include <string.h>
#include "Rts.h"

HsInt memchr_ba_many(unsigned char *p, HsInt off, HsInt len, HsInt *sizes, HsInt sizesLen, unsigned char w);
HsInt count_ba(unsigned char *p, HsInt off, HsInt len, unsigned char w);
