#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>

void FCreadline(int *ilen, char *string, char prompt[]) {

  char *line;
  int ln;
  int i;

  using_history();

  line=readline(prompt);
  add_history(line);

  ln = strlen(line);

  strcpy(string,line);

  for(i=ln; i<(int)*ilen; i++){
    string[i]=' ';
  }

  free(line);

}
