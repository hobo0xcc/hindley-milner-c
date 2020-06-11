all:
	gcc -g -o hm main.c typing.c parse.c tokenize.c error.c utils.c utils_test.c tokenize_test.c parse_test.c

clean:
	rm -f hm
