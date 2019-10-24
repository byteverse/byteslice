#define _GNU_SOURCE

#include <bs_custom.h>
#include <string.h>
#include "Rts.h"

// Find all occurrences of a byte, writing the lengths of each piece
// to the sizes buffer. This uses rawmemchr, so the total number of
// occurrences of the delimiter must be computed in advance. This
// returns the total length of all pieces processed.
HsInt memchr_ba_many(unsigned char *p, HsInt off, HsInt *sizes, HsInt sizesLen, unsigned char w) {
  HsInt szIx, delta, total;
  unsigned char* pos;
  p = p + off;
  total = 0;
  for (szIx = 0; szIx < sizesLen; ++szIx) {
    pos = (unsigned char*)(rawmemchr((void*)p,w));
    delta = pos - p;
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
