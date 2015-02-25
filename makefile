FC=gfortran
FCFLAGS=-g -c -O2
FLFLAGS=

SOURCES=$(wildcard src/*.f90)
TARGET=laplace

all: $(TARGET) $(SOURCES) 

$(TARGET): $(SOURCES)
	$(FC) $(FCFLAGS) $^ $(FLFLAGS) -o $@

clean:	
	@rm -rf *.o *.mod
