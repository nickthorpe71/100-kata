#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef _WIN32
#define DIR_SEPARATOR "\\"
#else
#define DIR_SEPARATOR "/"
#endif

// Define a function type for a template
typedef char *(*TemplateFunction)();

// Define a structure to hold language name and template function
struct LanguageTemplate
{
    const char *name;
    TemplateFunction templateFn;
    const char *file_extension;
};

char *c_template()
{
    return "#include<stdio.h>\n"
           "#include<time.h>\n\n"
           "int main() \n"
           "{\n"
           "  clock_t start, end;\n"
           "  double cpu_time_used;\n\n"
           "  start = clock();\n"
           "  //call functions here\n"
           "  end = clock();\n"
           "  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;\n"
           "  printf(\"Time elapsed: %f\\n\", cpu_time_used);\n"
           "  return 0;\n"
           "}\n";
}

char *haskell_template()
{
    return "module Main where\n\n"
           "import Data.Time (getCurrentTime, diffUTCTime)\n\n"
           "-- Define functions\n"
           "oddOrEven :: Int -> String\n"
           "oddOrEven n = if odd n then \"Odd\" else \"Even\"\n\n"
           "main :: IO ()\n"
           "main = do\n"
           "  start <- getCurrentTime\n"
           "  -- run function\n"
           "  let result = oddOrEven 1\n"
           "  putStrLn $ \"res: \" ++ show result\n" 
           "  end <- getCurrentTime\n"
           "  print (diffUTCTime end start)\n";
}

char *prolog_template()
{
    return "% Define a predicate to check if a number is even\n"
           "is_even(N) :-\n"
           "  N mod 2 =:= 0.\n\n"
           "% Define a predicate to check if a number is odd\n"
           "is_odd(N) :-\n"
           "  not(is_even(N)).\n\n"
           "% Define a predicate to print whether a number is even or odd\n"
           "print_even_or_odd(N) :-\n"
           "  is_even(N),\n"
           "  write(N), write(' is even.'), nl.\n"
           "print_even_or_odd(N) :-\n"
           "  is_odd(N),\n"
           "  write(N), write(' is odd.'), nl.\n\n"
           "% Test examples\n"
           ":- initialization(main).\n"
           "main :-\n"
           "  statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),\n"
           "  print_even_or_odd(12),\n"
           "  print_even_or_odd(11),\n"
           "  statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),\n"
           "  format('Execution took ~3d seconds.~n', [ExecutionTime]),\n"
           "  halt.\n";
}

char *js_template()
{
    return "function main() {\n"
           "  console.time('Execution Time');\n"
           "  console.log(\"Hello World!\");\n"
           "  console.timeEnd('Execution Time');\n"
           "}\n\n"
           "main();\n";
}
// Create an array of LanguageTemplate structures
struct LanguageTemplate lang_templates[] = {
    {"Haskell", haskell_template, ".hs"},
    {"C", c_template, ".c"},
    {"Prolog", prolog_template, ".pl"},
    {"JS", js_template, ".js"},
};

int create_dir(const char *kata_name, const char *lang_name, TemplateFunction templateFn, const char *file_extension)
{
    char dir_path[256];
    snprintf(dir_path, sizeof(dir_path), "%s%s%s", kata_name, DIR_SEPARATOR, lang_name);

    if (mkdir(dir_path, 0755) != 0)
    {
        perror("mkdir");
        return 1;
    }

    // Construct the full file path within the directory
    char file_path[512];
    snprintf(file_path, sizeof(file_path), "%s%s%s%s", dir_path, DIR_SEPARATOR, kata_name, file_extension);

    // Create a file with the given name
    FILE *file = fopen(file_path, "w");
    if (file == NULL)
    {
        perror("fopen");
        return 1;
    }

    // Get content from the template function and write it to the file
    const char *content = templateFn();
    if (fprintf(file, "%s\n", content) < 0)
    {
        perror("fprintf");
        fclose(file);
        return 1;
    }

    // Close the file
    fclose(file);

    return 0;
}

int add_summary_file(const char *kata_name)
{
    // Create the summary.md file at the top level
    char summary_path[256];
    snprintf(summary_path, sizeof(summary_path), "%s%s%s", kata_name, DIR_SEPARATOR, "summary.md");
    FILE *summary_file = fopen(summary_path, "w");
    if (summary_file == NULL)
    {
        perror("fopen");
        return 1;
    }

    // Write content to the summary.md file
    const char *summary_content = "This is a summary file.";
    if (fprintf(summary_file, "%s\n", summary_content) < 0)
    {
        perror("fprintf");
        fclose(summary_file);
        return 1;
    }

    // Close the summary.md file
    fclose(summary_file);

    return 0;
}

int main(int argc, char *argv[])
{
    // Check for the correct number of command-line arguments
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <kata_name>\n", argv[0]);
        return 1;
    }

    // Extract command-line arguments
    const char *kata_name = argv[1];

    // Create a directory with the given kata name
    if (mkdir(kata_name, 0755) != 0)
    {
        perror("mkdir");
        return 1;
    }

    // Create a summary.md file
    if (add_summary_file(kata_name) != 0)
    {
        fprintf(stderr, "Failed to create summary file for %s\n", kata_name);
    }

    // Iterate through the language templates and create directories
    for (int i = 0; i < sizeof(lang_templates) / sizeof(lang_templates[0]); i++)
    {
        if (create_dir(kata_name, lang_templates[i].name, lang_templates[i].templateFn, lang_templates[i].file_extension) != 0)
        {
            fprintf(stderr, "Failed to create directory for %s\n", lang_templates[i].name);
        }
    }

    printf("Created directories for '%s'\n", kata_name);

    return 0;
}
