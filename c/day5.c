/*
 * I'm not a c programmer...
 */

#include <stdio.h>
#include <stdlib.h>

static int SIZE = 1097; 
int* readInput() {
  char* filename = "../resources/day5.txt";
  FILE* file = fopen(filename, "r");
  int* output = malloc(SIZE * sizeof(int));
  int i = 0;

  if (file != NULL) {
    char line[256];
    while(fgets(line, sizeof line, file) != NULL) {
      output[i] = atoi(line);
      i++;
    }
    fclose(file);
    return output;
  } else {
    printf("file didn't open\n");
  }

  return NULL;
}

int solvePart1(int* program) {
  int jumpCount = 0;
  int i = 0;
  while(i < SIZE) {
    int offset = program[i];
    program[i] = offset + 1;
    i = i + offset;
    jumpCount++;
  }

  return jumpCount;
}

int solvePart2(int* program) {
  int jumpCount = 0;
  int i = 0;
  while(i < SIZE) {
    int offset = program[i];

    if (offset >= 3) {
      program[i] = offset - 1;
    } else {
      program[i] = offset + 1;
    }

    i = i + offset;
    jumpCount++;
  }

  return jumpCount;
}

int main (int argc, char** argv) {
  int* program1 = readInput();
  int* program2 = readInput();

  printf("%d\n", solvePart1(program1));
  free(program1);

  printf("%d\n", solvePart2(program2));
  free(program2);

  return 0;
}
