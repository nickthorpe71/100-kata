#include<stdio.h>
#include<time.h>
#include<stdlib.h>
#include<stdbool.h>
#include<string.h> 

// r = sum mod 10
// return r === 0

int* cloneIntArr(const int* original, int size) {
  int* clone = malloc(size * sizeof(int));
  if (clone == NULL) {
    return NULL;
  }
  memcpy(clone, original, size * sizeof(int));
  return clone;
}

int sumIntArr(const int* original, int size) {
  int sum = 0;
  for (int i = 0; i < size; i++) {
    sum += original[i];
  }
  return sum;
}

int getNumDigits(long n) {
  int size = 0;

  while (n != 0) {
    size++;
    n /= 10;
  }

  return size;
}

int* getDigitArr(long digits, int size) {
  int* digitArr = malloc(size * sizeof(int));
  if (digitArr == NULL) {
    return NULL;
  }

  for (int i = size - 1; i >= 0; --i) {
    digitArr[i] = digits % 10;
    digits /= 10;
  }

  return digitArr;
}

int* doubleAlternating(int* digitArr, int startIndex, int length) {
  int* arrClone = cloneIntArr(digitArr, length);
  int i = startIndex;

  while (i < length) {
    arrClone[i] *= 2;
    if (arrClone[i] > 9) {
      arrClone[i] -= 9;
    }
    i += 2;
  }

  return arrClone;
}

bool validate(long digits) {
  int len = getNumDigits(digits);
  int* arr = getDigitArr(digits, len);
  int startIndex = len % 2 == 0 ? 0 : 1;
  int* doubled = doubleAlternating(arr, startIndex, len);
  int sum = sumIntArr(doubled, len);

  free(arr);
  free(doubled);

  return sum % 10 == 0;
}

int main() 
{
  clock_t start, end;
  double cpu_time_used;

  start = clock();

  long test = 4653151235;
  printf("%d", validate(test));

  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Time elapsed: %f\n", cpu_time_used);
  return 0;
}

