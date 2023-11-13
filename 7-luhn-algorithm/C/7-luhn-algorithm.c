#include<stdio.h>
#include<time.h>
#include <stdlib.h>
#include <stdbool.h>

// determine length of number
// if even double every other starting from the first
// if odd double every other starting from second
// for each doubled number, if it is > 9, subtract 9
// sum all digits
// r = sum mod 10
// return r === 0

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

bool validate(long digits) {
  int numDigits = getNumDigits(digits);
  int* digitArr = getDigitArr(digits, numDigits);

}

int main() 
{
  clock_t start, end;
  double cpu_time_used;

  start = clock();

  long test = 4653151235;
  int testLen = getNumDigits(test);
  int* testArr = getDigitArr(test, testLen);
  
  printf("num: %ld\n", test);
  printf("len: %d should be 10\n", testLen); 
  printf("arr: ");
  for (int i = 0; i < testLen; i++) {
    printf("%d ", testArr[i]);
  }
  printf("\n");

  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Time elapsed: %f\n", cpu_time_used);
  return 0;
}

