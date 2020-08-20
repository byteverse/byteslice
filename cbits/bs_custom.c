#define _GNU_SOURCE

#include <bs_custom.h>
#include <string.h>
#include "Rts.h"

// Find all occurrences of a byte, writing the lengths of each piece
// to the sizes buffer. This uses rawmemchr, so the total number of
// occurrences of the delimiter must be computed in advance. This
// returns the total length of all pieces processed.
//
// Portability: On Linux, this uses rawmemchr. On all other platforms,
// it uses memchr. On Linux, the length of the byte sequence is not
// used. On other platforms, this is repeatedly decremented to provide
// an appropriate third argument for memchr.
HsInt memchr_ba_many(unsigned char *p, HsInt off, HsInt len, HsInt *sizes, HsInt sizesLen, unsigned char w) {
  HsInt szIx, delta, total;
  unsigned char* pos;
  p = p + off;
  total = 0;
  for (szIx = 0; szIx < sizesLen; ++szIx) {
#if defined(__linux__) && !AVOID_RAWMEMCHR
    pos = (unsigned char*)(rawmemchr((void*)p,(int)w));
    delta = (HsInt)(pos - p);
#else
    pos = (unsigned char*)(memchr((void*)p,(int)w,(size_t)len));
    delta = (HsInt)(pos - p);
    len = len - (delta + 1);
#endif
    sizes[szIx] = delta;
    total = total + delta + 1;
    p = pos + 1;
  }
  return total;
}

// TODO: Possibly use SIMD in here. Or check so see if gcc optimizes
// this on its own.
HsInt count_ba(unsigned char *p, HsInt off, HsInt len, unsigned char w) {
  HsInt c;
  p = p + off;
  for (c = 0; len-- != 0; ++p)
      if (*p == w)
          ++c;
  return c;
}
