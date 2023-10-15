#include<stdio.h>
 
const char* even_or_odd(int number)
{
  return number % 2 == 0 ? "Even" : "Odd";
}

int main() 
{
	const char* even = even_or_odd(12);
	const char* odd = even_or_odd(11);

	printf("Even: %s\n", even);
	printf("Odd: %s\n", odd);

  return 0;
}
