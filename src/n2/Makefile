CXX ?= g++
PKG_CXXFLAGS = -O3 -std=c++11 -pthread -fPIC -fopenmp 
PKG_CXXFLAGS += -I"./include"
LDFLAGS += -lpthread

LIB = ../libn2.a
lib: $(LIB)

$(LIB): base.o hnsw.o hnsw_node.o distance.o heuristic.o mmap.o
	ar rvs $@ $?


clean:
	@-rm -f *.o $(LIB) 


