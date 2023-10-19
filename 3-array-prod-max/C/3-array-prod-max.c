#include<stdio.h>
#include<stdlib.h>
#include<time.h>

int compare(const void *a, const void *b)
{
  return (*(int*)b - *(int*)a);
}

long long maxProduct(int numbers[], int numbers_size, int sub_size) {
    qsort(numbers, numbers_size, sizeof(int), compare); // Sort the array in descending order
    long long product = 1; // Initialize the product to 1

    // Multiply the 'size' largest elements to get the product
    for(int i = 0; i < sub_size; i++) {
        product *= numbers[i];
    }

    return product;
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

  printf("%lld", maxProduct((int[]){4,3,5}, 3, 2));
  printf("%lld", maxProduct((int[]){10,8,7,9}, 4, 3));

  clock_t start, end;
  double cpu_time_used;
  start = clock();
  
  printf("%lld", maxProduct(testNums, testLen, 5));

  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Time elapsed: %f\n", cpu_time_used);

  return 0;
}
