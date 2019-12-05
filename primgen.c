#include <inttypes.h>
#include "primegen.h"

typedef int64_t int64;

uint64 low = 2;
uint64 high = 1000000000;

primegen pg;

uint32 digits[40];
int len;

int gen_prime_array(int array_size, int64 *array, int64 limit)
{
  uint64 p;
  uint32 u;
  int i;
  int count = 0;

  primegen_init(&pg);

  primegen_skipto(&pg,low);

  high = limit;

  for (i = 0;i < 40;++i) digits[i] = 0;

  p = primegen_peek(&pg);
  len = 0;
  do {
    digits[len++] = p % 10;
    p /= 10;
  } while (p);

  p = primegen_peek(&pg);

  for (;;) {
    u = primegen_next(&pg) - p;
    p += u;

    array[count++] = p;

    if (p > high) break;
  }

  return count;
}
