#include<stdio.h>
#include<stdlib.h>
#include<time.h>

int compare(const void *a, const void *b)
{
  return (*(int*)a - *(int*)b);
}

int minSum(int a[], int n)
{
  qsort(a, n, sizeof(int), compare);

  int mid = n / 2;
  int sum = 0;
  
  for (int i = 0; i < mid; i++)
  {
    sum += a[i] * a[n - i - 1]; 
  }

  return sum;
}

int main() 
{
  int testLen = 1000000;
  int testNums[testLen];
  int i;

  srand(time(0));

  for (i = 0; i < testLen; i++)
  {
    testNums[i] = rand();
  }

  clock_t start, end;
  double cpu_time_used;
  start = clock();
  //call functions here\n"
  
  printf("%d", minSum(testNums, testLen));
  
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Time elapsed: %f\n", cpu_time_used);

	return 0;
}

