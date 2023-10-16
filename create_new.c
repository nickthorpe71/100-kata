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

  // Createa a directory with the given name
  if (mkdir(kata_name, 0755) != 0) 
  {
    perror("mkdir");
    return 1;
  }

  // Construct the full file path within the directory
  char file_path[256];
  snprintf(file_path, sizeof(file_path), "%s%s%s", kata_name, DIR_SEPARATOR, "test.txt");

  // Create a file with the given name
  FILE *file = fopen(file_path, "w");
  if (file == NULL) 
  {
    perror("fopen");
    return 1;
  }

  // Write content to the file
  if (fprintf(file, "%s\n", "tttttttttttttttttttt") < 0) 
  {
    perror("fprintf");
    fclose(file);
    return 1;
  }

  // Close the file
  fclose(file);

  printf("Created directories for '%s'\n", kata_name);

  return 0;

}
