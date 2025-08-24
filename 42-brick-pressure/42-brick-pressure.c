#include <stdio.h>

double weight(unsigned row, unsigned pos)
{
    if (row == 0)
        return 0;
    if (pos == 0)
        return 0.5 * (weight(row - 1, pos) + 1);
    if (pos == row)
        return 0.5 * (weight(row - 1, pos - 1) + 1);
    return 0.5 * (weight(row - 1, pos - 1) + 1) + 0.5 * (weight(row - 1, pos) + 1);
}

int main()
{
    double r = weight(0, 0); // 0
    printf("r = %f\n", r);
    double r1 = weight(1, 0); // 0.5
    printf("r1 = %f\n", r1);
    double r2 = weight(1, 1); // 0.5
    printf("r2 = %f\n", r2);
    double r3 = weight(2, 0); // 0.75
    printf("r3 = %f\n", r3);
    double r4 = weight(2, 1); // 1.5
    printf("r4 = %f\n", r4);
    double r5 = weight(3, 0); // 0.875
    printf("r5 = %f\n", r5);
    double r6 = weight(3, 1); // 2.125
    printf("r6 = %f\n", r6);
    return 0;
}

// weight(row, pos) = 0.5 * weight(row-1, pos-1) + 0.5 * weight(row-1, pos) + 1
