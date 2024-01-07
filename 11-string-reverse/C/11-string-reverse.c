#include <stdio.h>
#include <time.h>

void spinWords(const char *sentence, char *result)
{
    
}

int main()
{
    clock_t start, end;
    double cpu_time_used;

    start = clock();
    // call functions here
    end = clock();
    cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
    printf("Time elapsed: %f\n", cpu_time_used);
    return 0;
}
