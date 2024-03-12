##
## EPITECH PROJECT, 2024
## Bootstrap ImageCompressor
## File description:
## Makefile
##

NAME = Bootstrap-ImageCompressor

all:
	stack build
	cp $(shell stack path --local-install-root)/bin/Bootstrap-ImageCompressor-exe $(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

tests_run: all

run: all

.PHONY: all clean fclean re
.SILENT: run
