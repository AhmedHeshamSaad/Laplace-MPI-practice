#GFORTRAN OS X ###
FC = gfortran
FFLAGS = #-m64 -fdefault-real-8 -fcheck=all -Wall \
			# -g -ffpe-trap=zero,invalid,overflow,underflow
TARGET_ARCH =
LDFLAGS = -m64
LIBS = 

EXE   = laplace
VPATH = mod

.SUFFIXES: .f90 .o

GRAFIC_HEADER =

SRCMOD =    			\
	vars.f90		

SRCMAIN =      			\
	laplaceSerial.f90	\
	init.f90			\
	writeVTK.f90

OBJMOD = ${SRCMOD:.f90=.o}

OBJMAIN = ${SRCMAIN:.f90=.o}

OBJ = $(OBJMOD) $(OBJMAIN)

$(EXE): $(OBJ)
	$(FC) $(LDFLAGS) $(OBJ) $(LIBS) -o $(EXE)

%.o  : %.f90
	$(FC) $(FFLAGS) -c $<

# Define dependencies for modules
$(OBJMAIN): $(OBJMOD)


# clean any generated files
clean:
	-rm -f *.o *~ core *.mod

