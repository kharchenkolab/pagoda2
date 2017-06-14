all: main.o
	g++ -std=c++11 main.o -o p2pack
	chmod u+x p2pack
main.o: main.cpp header.h
	g++ -std=c++11 -I . -c main.cpp
clean:
	rm -rf *.o
	rm p2pack
