##
## EPITECH PROJECT, 2021
## wolfram
## File description:
## Makefile
##

NAME	=	imageCompressor

BINARY_PATH     :=  $(shell stack path --local-install-root)

SOURCES	=	src/Cell.hs \
			src/Universe.hs \
			src/WConfig.hs \
			src/Wolfram.hs \
			src/Utils.hs

all: build

build:
	@stack build
	@cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	@rm -f *\~

debug:
	@ghci $(SOURCES)

fclean: clean
	@rm -f $(NAME)

tests_run:
	@stack test

re: fclean all

.PHONY: all build clean fclean tests_run re

