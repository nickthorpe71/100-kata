#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

char **splitString(const char *str, int *numSplits);
void spinWords(const char *sentence, char *result);

char **splitString(const char *str, int *numSplits)
{
    int capacity = 10;
    char **result = malloc(capacity * sizeof(char *));
    if (result == NULL)
    {
        perror("Malloc failed");
        return NULL;
    }

    *numSplits = 0;
    const char *wordStart = str;
    int wordLength;

    while (*str != '\0')
    {
        if (*str == ' ' || *str == '\0')
        {
            wordLength = str - wordStart;
            if (wordLength > 0)
            {
                result[*numSplits] = malloc(wordLength + 1);
                if (result[*numSplits] == NULL)
                {
                    perror("Malloc failed");
                    // Free already allocated strings
                    for (int i = 0; i < *numSplits; i++)
                    {
                        free(result[i]);
                    }
                    free(result);
                    return NULL;
                }
                strncpy(result[*numSplits], wordStart, wordLength);
                result[*numSplits][wordLength] = '\0';
                (*numSplits)++;

                if (*numSplits >= capacity)
                {
                    capacity *= 2;
                    char **temp = realloc(result, capacity * sizeof(char *));
                    if (temp == NULL)
                    {
                        perror("Realloc failed");
                        for (int i = 0; i < *numSplits; i++)
                        {
                            free(result[i]);
                        }
                        free(result);
                        return NULL;
                    }
                    result = temp;
                }
            }
            wordStart = str + 1;
        }
        str++;
    }
    return result;
}

void spinWords(const char *sentence, char *result)
{
}

int main()
{
    clock_t start, end;
    double cpu_time_used;

    start = clock();

    const char *myString = "This is a test string";
    int numSplits;
    char **splitStrings = splitString(myString, &numSplits);

    if (splitStrings != NULL)
    {
        for (int i = 0; i < numSplits; i++)
        {
            printf("%s\n", splitStrings[i]);
            free(splitStrings[i]);
        }
        free(splitStrings);
    }

    end = clock();
    cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
    printf("Time elapsed: %f\n", cpu_time_used);
    return 0;
}
