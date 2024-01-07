#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

typedef struct
{
    char **array;
    int length;
} StringArray;

char *joinStringArray(StringArray strArr, char delimiter);
char *reverseString(char *str);
StringArray splitString(const char *str, char delimiter);
void spinWords(const char *sentence, char *result);

char *joinStringArray(StringArray strArr, char delimiter)
{
    int totalLength = 1;
    for (int i = 0; i < strArr.length; i++)
    {
        totalLength += strlen(strArr.array[i]) + (delimiter != '\0' && i < strArr.length - 1);
    }

    char *result = malloc(totalLength * sizeof(char));
    if (result == NULL)
    {
        perror("Malloc failed");
        return NULL;
    }

    result[0] = '\0';
    for (int i = 0; i < strArr.length; i++)
    {
        strcat(result, strArr.array[i]);
        if (delimiter != '\0' && i < strArr.length - 1)
        {
            size_t len = strlen(result);
            result[len] = delimiter;
            result[len + 1] = '\0';
        }
    }
    return result;
}

char *reverseString(char *str)
{
    int len = strlen(str);
    char *result = malloc((len + 1) * sizeof(char));
    if (result == NULL)
    {
        perror("Malloc failed");
        return NULL;
    }

    for (int i = 0; i < len; i++)
    {
        result[i] = str[len - 1 - i];
    }
    result[len] = '\0';
    return result;
}

StringArray splitString(const char *str, char delimiter)
{
    if (delimiter == '\0')
    {
        delimiter = ' ';
    }

    int capacity = 10;
    StringArray result;
    result.array = malloc(capacity * sizeof(char *));
    if (result.array == NULL)
    {
        perror("Malloc failed");
        result.length = 0;
        return result;
    }

    result.length = 0;
    const char *wordStart = str;
    int wordLength;

    while (*str != '\0')
    {
        if (*str == delimiter || *(str + 1) == '\0')
        {
            if (*(str + 1) == '\0' && *str != delimiter)
            {
                str++;
            }
            wordLength = str - wordStart;
            if (wordLength > 0)
            {
                result.array[result.length] = malloc(wordLength + 1);
                if (result.array[result.length] == NULL)
                {
                    perror("Malloc failed");
                    // Free already allocated strings
                    for (int i = 0; i < result.length; i++)
                    {
                        free(result.array[i]);
                    }
                    free(result.array);
                    return result;
                }
                strncpy(result.array[result.length], wordStart, wordLength);
                result.array[result.length][wordLength] = '\0';
                result.length++;

                if (result.length >= capacity)
                {
                    capacity *= 2;
                    char **temp = realloc(result.array, capacity * sizeof(char *));
                    if (temp == NULL)
                    {
                        perror("Realloc failed");
                        for (int i = 0; i < result.length; i++)
                        {
                            free(result.array[i]);
                        }
                        free(result.array);
                        return result;
                    }
                    result.array = temp;
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
    StringArray splitStrings = splitString(sentence, ' ');

    if (splitStrings.array != NULL)
    {
        for (int i = 0; i < splitStrings.length; i++)
        {
            if (strlen(splitStrings.array[i]) >= 5)
            {
                char *reversed = reverseString(splitStrings.array[i]);
                free(splitStrings.array[i]);
                splitStrings.array[i] = reversed;
            }
        }

        char *joinedString = joinStringArray(splitStrings, ' ');
        strcpy(result, joinedString);
        free(joinedString);

        for (int i = 0; i < splitStrings.length; i++)
        {
            free(splitStrings.array[i]);
        }
        free(splitStrings.array);
    }
    else
    {
        result[0] = '\0';
    }
}

int main()
{
    clock_t start, end;
    double cpu_time_used;

    start = clock();

    char *res = malloc(1024);
    if (res == NULL)
    {
        perror("Malloc failed");
        return 1;
    }
    res[0] = '\0';

    const char *myString = "Welcome";
    const char *t2 = "if this is at the end it's broken";
    spinWords(myString, res);
    spinWords(t2, res);

    printf("---------------------\nRes: %s\n", res);
    free(res);

    end = clock();
    cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
    printf("Time elapsed: %f\n", cpu_time_used);
    return 0;
}
