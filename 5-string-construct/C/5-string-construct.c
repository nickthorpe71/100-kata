#include<stdio.h>
#include<time.h>
#include<string.h>

int stringConstructing(char* a, char* s)
{
  int i = 0, ops = 0;
  int a_len = strlen(a), s_len = strlen(s);
  char curr[2 * s_len]; // To ensure we have enough space for operations
  curr[0] = '\0'; // To init as an empty string

  while (strcmp(curr, s) != 0) 
  {
    while (i < strlen(s) && i < strlen(curr) && curr[i] == s[i])
    {
      i++;
    }

    if (i >= strlen(curr)) 
    {
      strcat(curr, a);
    } 
    else
    {
      memmove(&curr[i], &curr[i + 1], strlen(curr) - i);
    }

    ops++;
  }

  return ops;
}

int main() 
{
  clock_t start, end;
  double cpu_time_used;

  start = clock();
  //call functions here
  printf("%d\n", stringConstructing("a", "a"));
  printf("%d\n", stringConstructing("aba", "abbabba"));
  printf("%d\n", stringConstructing("a", "aaa"));
  printf("%d\n", stringConstructing("bbaabcbcbc", "bbcccbabcc"));
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Time elapsed: %f\n", cpu_time_used);
  return 0;
}

