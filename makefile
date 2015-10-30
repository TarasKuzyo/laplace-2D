FC=gfortran
FCFLAGS=-g -O2 -Wall
FLFLAGS=

SOURCES=globals.f90 utils.f90 boundaries.f90 init.f90 laplace.f90 driver.f90
OBJECTS=$(SOURCES:.f90=.o)
TARGET=laplace

all: $(OBJECTS) $(TARGET)

$(TARGET): $(OBJECTS)
	$(FC) $^ $(FCFLAGS) $(FLFLAGS) -o $@
	
%.o %.mod: %.f90
	$(FC) $(FCFLAGS) -c $<	

clean:	
	@rm -rf *.o *.mod
