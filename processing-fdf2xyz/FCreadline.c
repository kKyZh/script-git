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

  printf("\n");
  int separate_line = system("echo --------------------------------------------------");
  printf("\n");
  int ls_color = system("ls --color=auto");
  printf("\n");
  int pwd_current = system("pwd");
  printf("\n");

  using_history();

  char inputprompt[128] = "Please enter the file name.";
  printf("%s\n", inputprompt);

  line=readline(prompt);
  add_history(line);

  ln = strlen(line);

  strcpy(string,line);

  for(i=ln; i<(int)*ilen; i++){
    string[i]=' ';
  }

  free(line);

}

void FCuserreadline(int *ilen, char *string, char prompt[]) {

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
