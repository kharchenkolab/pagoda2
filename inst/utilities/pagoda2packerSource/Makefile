all: p2pack.o
	g++ -std=c++11 p2pack.o -o p2pack
	chmod u+x p2pack
p2pack.o: p2pack.cpp p2pack.h
	g++ -std=c++11 -I . -c p2pack.cpp
clean:
	rm -rf *.o
	rm p2pack
