##
## EPITECH PROJECT, 2024
## Bootstrap ImageCompressor
## File description:
## Makefile
##

NAME = imageCompressor

all:
	stack build
	cp $(shell stack path --local-install-root)/bin/ImageCompressor-exe $(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

tests_run: all

run: all

.PHONY: all clean fclean re
.SILENT: run
