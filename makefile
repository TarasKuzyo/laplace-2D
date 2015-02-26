FC=gfortran
FCFLAGS=-g -O2 -Wall
FLFLAGS=

SOURCES=$(wildcard src/*.f90)
TARGET=laplace

all: $(TARGET) $(SOURCES) 

$(TARGET): $(SOURCES)
	$(FC) $(FCFLAGS) $^ $(FLFLAGS) -o $@

clean:	
	@rm -rf *.o *.mod
