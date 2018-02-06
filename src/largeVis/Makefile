CXX ?= g++
PKG_CXXFLAGS = -Ofast -march=native -ffast-math
PKG_CXXFLAGS += -I"./include"
LDFLAGS += -lm -pthread -lgsl -lgslcblas 

LIB = ../liblargeVis.a
lib: $(LIB)

$(LIB): LargeVis.o
	ar rvs $@ $?

clean:
	@-rm -f *.o $(LIB) 


