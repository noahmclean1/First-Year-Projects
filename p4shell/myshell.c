#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>


void myPrint(char *msg)
{
    write(STDOUT_FILENO, msg, strlen(msg));
}

void senderror()
{
	myPrint("An error has occurred\n");
	exit(0);
}

int pwd()
{
	char buff[1050];
	char *cwd = getcwd(buff,1050);
	myPrint(cwd);
	myPrint("\n");
	return 1;
}

int cd(char *phrase)
{
	if (phrase == NULL)
		return(chdir(getenv("HOME")));
	return(chdir(phrase));
}

int redirect_search(char *text)
// Searches for multiple redirections, return number of them
{
	if (text == NULL)
		return 0;
	int length = strlen(text);
	int i;
	int tally = 0;
	for (i=0;i<length;i++)
	{
		if (text[i] == '>')
			tally++;
	}
	return tally;
}

void run_cmd(char **input, int args, int redirect, int fileno, int advanced, int adv_fileno, char *filename)
// Given a command and argument number, execute the command to STDOUT_FILENO
{
	if (strcmp(input[0],"exit") == 0)
	{
		if (args != 1)
		{
			// If exit is called with arguments
			myPrint("An error has occurred\n");
			return;
		}
		exit(0);
	}
	else if (strcmp(input[0],"cd") == 0)
	{
		if (input[2] != NULL)
		{
			// If there're too many args
			myPrint("An error has occurred\n");
			return;
		}
		int val = cd(input[1]);
		if (val == -1)
		{
			// If cd fails
			myPrint("An error has occurred\n");
			return;
		}
	}
	else if (strcmp(input[0],"pwd") == 0)
	{
		if (args > 1)
		{
			// pwd should only have pwd
			myPrint("An error has occurred\n");
			return;
		}
		pwd();
	}
	else
	// If it enters (what should be) a user defined program
	{
		int forkval = fork();
		// Child continues the program execution
		if (forkval == 0)
		{
			execvp(input[0],input);
			// If execvp fails, then it will reach this
			senderror();
		}
		// Parent waits until child is done to continue (either to fork again or end)
		else
		{
			waitpid(forkval,NULL,0);
			if (redirect)
			{
				if (advanced)
				{
					// We need to copy in the other file's info
					char *buff;
					buff = (char*)malloc(sizeof(char)*1100);
					read(adv_fileno,buff,1100);
					myPrint(buff);
					close(adv_fileno);
					remove(filename);
					rename("blankfile",filename);
				}
				dup2(fileno,STDOUT_FILENO);
				close(fileno);
			}
		}
	}
}

void cmd_parse(char *command)
// separates a command into an array of command and args
{
	// checking for built_in command
	int built_in = 0;
	char *cpycmd = (char*)malloc(sizeof(char)*512);
	strcpy(cpycmd,command);
	strtok(cpycmd," \n\t>");
	if ((strcmp(cpycmd,"cd") == 0) || (strcmp(cpycmd,"pwd") == 0) || (strcmp(cpycmd,"exit") == 0))
	{
		built_in = 1;
	}
	// REDIRECTION PARSING HERE

	int tally = redirect_search(command);
	char *firsthalf = strtok(command,">");
	char *dest = strtok(NULL,"\n");
	int redirect = 0;
	int fileno = -1;
	int fd;
	int adv_fd;
	int adv = 0;

	// If successful, it is a redirect (either adv or not)
	if (tally > 0)
	{
		if (tally > 1)
		{
			// There's another > sign
			myPrint("An error has occurred\n");
			return;
		}
		if (built_in)
		{
			// If the function is built in
			myPrint("An error has occurred\n");
			return;
		}
		if (dest == NULL)
		{
			myPrint("An error has occurred\n");
			return;
		}

		// Check if there's no destination
		char *destcpy = (char*)malloc(sizeof(char)*512);
		strcpy(destcpy,dest);
		char *cpy2;
		cpy2 = strtok(destcpy," \n\t");
		if (cpy2 == NULL)
		{
			// There is no dest
			myPrint("An error has occurred\n");
			return;
		}
		redirect = 1;

		// If adv redirect!
		if (dest[0] == '+')
		{
			// Should set dest equal to the given destination
			dest = strtok(dest," +\n\t");
	
			if (dest == NULL)
			{
				// Extra NULL check bc why not
				myPrint("An error has occurred\n");
				return;
			}

			fd = open(dest, O_CREAT | O_RDWR | O_EXCL, 0666);
			if (fd == -1)
			{
				// fd is the given file (which exists)
				// adv_fd is a new, blank file which will be renamed after redirection
				fd = open("blankfile",O_CREAT | O_RDWR,0666);
				adv_fd = open(dest,O_RDWR,0666);
	
				if (adv_fd == -1)
				{
					//Should prevent invalid paths
					myPrint("An error has occurred\n");
					return;
				}

				adv = 1;
				fileno = dup(STDOUT_FILENO);
				dup2(fd,STDOUT_FILENO);
				// The final product goes to adv_fd, which only contains that output
				// Then, when it's done, we copy fd into adv_fd, rename it, and close them both
			}
			//Otherwise it'll run just like redirection
			
		}
		
		// Else will only run if it's not adv redirection
		else
		{
			dest = strtok(dest," \n");
			if (dest == NULL)
			{
				// An extra NULL check couldnt hurt after strtoking again
				myPrint("An error has occurred\n");
				return;
			}

			fd = open(dest, O_CREAT | O_RDWR | O_EXCL, 0666);
			if (fd == -1)
			{
				// If the file already exists it fails
				myPrint("An error has occurred\n");
				return;
			}
			// Should exit with fd = file descrip
		}

		// NORM redirect here
		if (!adv)
		{
			fileno = dup(STDOUT_FILENO);
			dup2(fd,STDOUT_FILENO);
			close(fd);
		}
	}
	char **output = malloc(sizeof(char*)*520);

	// Command is split into args and sent to be run
	int i = 0;
	char *arg;
	for (arg=strtok(firsthalf,"\t ");arg!=NULL;arg=strtok(NULL,"\t "))
	{
		output[i] = arg;
		i++;
	}
	if (i == 0)
	{
		return;
	}
	
	run_cmd(output,i,redirect,fileno,adv,adv_fd,dest);
	return;
}

void multi_cmd_parse(char *line)
// parses line for multiple commands (;)
{
	char *command;
	char **cmd_array = (char**)malloc(sizeof(char*)*512);
	int i=0;
	// command is the whole command (divides by ;)
	command = strtok(line,";\n");
	while (command!=NULL)
	{
		cmd_array[i] = command;
		i++;
		command = strtok(NULL,";\n");
	}
	int max = i;
	for (i=0;i<max;i++)
	{
		cmd_parse(cmd_array[i]);
	}
}


int main(int argc, char *argv[]) 
{
    char cmd_buff[1200];
    char *pinput;
    char *cpy = (char*)malloc(sizeof(char)*1200);

      if (argc > 2)
      {
	senderror();
      }
      // Batch Mode
      if (argc == 2)
	{
		FILE *fp = fopen(argv[1],"r");
		if (fp == NULL)
		{
			senderror();
		}
		while (!feof(fp))
		{
			pinput = fgets(cmd_buff, 1200, fp);
			if (!pinput) {
				exit(0);
			}
			
			strcpy(cpy,cmd_buff);

			if (strtok(cpy,"\n \t") == NULL)
				continue;
		
			if (strlen(cmd_buff) > 512)
			{
				// buff is too large?
				myPrint(pinput);
				myPrint("An error has occurred\n");
				continue;
			}

			myPrint(pinput);

			multi_cmd_parse(pinput);
		}
		fclose(fp);
	}

	// NOT batch mode
      else
	{
		while (1)
		{ 
			myPrint("myshell> ");
	  		pinput = fgets(cmd_buff, 1200, stdin);
      			if (!pinput) {
            			exit(0);
       			 }

			strcpy(cpy,cmd_buff);

			if (strtok(cpy,"\n \t") == NULL)
				continue;

			if (strlen(cmd_buff) > 512)
			{
				// buff is too large?
				myPrint("An error has occurred\n");
				continue;
			}
		
			multi_cmd_parse(pinput);
    		}
	}
	return 0;
}
