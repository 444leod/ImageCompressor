##
## EPITECH PROJECT, 2024
# PROJECT NAME
## File description:
## Makefile
##

NAME = EXECUTABLE_NAME

SRC = ./src/main.c

OBJ = $(SRC:.asm=.o)

CC = gcc

CFLAGS = -Wall -Wextra -Werror -Wpedantic

all: $(NAME)

$(NAME): $(OBJ)
	$(CC) -o $(NAME) $(OBJ) $(CFLAGS)

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME)

re: fclean all

tests_run: all

run: all

.PHONY: all clean fclean re
.SILENT: run
