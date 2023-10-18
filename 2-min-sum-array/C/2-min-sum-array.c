#include<stdio.h>
#include<stdlib.h>

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
  printf("%d -> 22\n", minSum((int[]){5,4,2,3}, 4));
  printf("%d -> 342\n", minSum((int[]){12,6,10,26,3,24}, 6));
	return 0;
}

