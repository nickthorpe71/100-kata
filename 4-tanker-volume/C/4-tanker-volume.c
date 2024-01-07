#include<stdio.h>
#include<time.h>
#include<math.h>

#define M_PI 3.14159265358979323846

int tankVol(int h, int d, int vt) 
{
  double r = d / 2.0;
  double theta = 2 * acos(1 - 2 * h / (double)d);
  double liquidSegmentArea = (r * r / 2.0) * (theta - sin(theta)); 
  double length = vt / (M_PI * r * r);
  double liquidVolume = liquidSegmentArea * length;
  return (int)floor(liquidVolume);
}

int main() 
{
  clock_t start, end;
  double cpu_time_used;

  start = clock();
  printf("%d\n", tankVol(40,120,3500));
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Time elapsed: %f\n", cpu_time_used);
  return 0;
}

