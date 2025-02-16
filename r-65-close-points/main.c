#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <time.h>

// Structure for a point
typedef struct
{
    double x, y;
} Point;

// Comparator function for sorting points by x-coordinate
int compare_x(const void *a, const void *b)
{
    Point *p1 = (Point *)a;
    Point *p2 = (Point *)b;
    if (p1->x < p2->x)
        return -1;
    if (p1->x > p2->x)
        return 1;
    return 0;
}

// Function to calculate squared Euclidean distance
double distance_squared(Point p1, Point p2)
{
    double dx = p1.x - p2.x;
    double dy = p1.y - p2.y;
    return dx * dx + dy * dy;
}

// Function to find the closest pair of points
void closest_pair(Point points[], int n, Point *p1, Point *p2)
{
    if (n < 2)
    {
        printf("Error: At least two points are required.\n");
        return;
    }

    // Sort points by x-coordinate
    qsort(points, n, sizeof(Point), compare_x);

    double min_dist = DBL_MAX; // Set initial large distance
    double tolerance = DBL_MAX;

    for (int i = 0; i < n - 1; i++)
    {
        for (int j = i + 1; j < n; j++)
        {
            // If x-distance exceeds the closest found so far, stop checking
            if ((points[j].x - points[i].x) >= tolerance)
            {
                break;
            }

            double d = distance_squared(points[i], points[j]);
            if (d < min_dist)
            {
                min_dist = d;
                tolerance = sqrt(min_dist);
                *p1 = points[i];
                *p2 = points[j];
            }
        }
    }
}

// Get current time in nanoseconds
long get_nanoseconds() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1e9 + ts.tv_nsec;  // Convert to nanoseconds
}

// Test the function
int main()
{
    Point points[] = {
        {2.0, 2.0},
        {2.0, 8.0},
        {5.0, 5.0},
        {6.0, 3.0},
        {6.0, 7.0},
        {7.0, 4.0},
        {7.0, 9.0}};

    int n = sizeof(points) / sizeof(points[0]);
    Point p1, p2;

    clock_t start = get_nanoseconds();
    closest_pair(points, n, &p1, &p2);
    clock_t end = get_nanoseconds();

    printf("Closest pair: (%.2f, %.2f) and (%.2f, %.2f)\n", p1.x, p1.y, p2.x, p2.y);
    printf("Execution time: %ld nanoseconds\n", end - start);

    return 0;
}
