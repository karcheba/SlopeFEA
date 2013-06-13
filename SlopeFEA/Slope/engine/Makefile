
FC = gfortran
FFLAGS = 
LFLAGS = 
DLLFLAGS = -shared -static-libgcc -static-libgfortran
LAPACKFLAGS = -llapack -lblas
MODULES = elements.mod feutility.mod gcontrol.mod mproperty.mod \
			nodes.mod numeric.mod tractions.mod
OBJECTS = SlopeFEA3NodeMultiPhase.o
EXECUTABLE = SlopeFEA3NodeMultiPhase.exe
DLL = SlopeFEA3NodeMultiPhase.dll

.PHONY: clean

# to make the $(EXECUTABLE), change $(DLL) -> $(EXECUTABLE) here
# Note: When making the $(EXECUTABLE) rather than the $(DLL),
#		it may also be necessary to change the main SUBROUTINE into
#		the main PROGRAM in the Fortran code.
all: $(DLL)

$(DLL): $(OBJECTS)
	$(FC) $(LFLAGS) $(DLLFLAGS) $(OBJECTS) -o $(DLL) $(LAPACKFLAGS)
	
$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LFLAGS) $(OBJECTS) -o $(EXECUTABLE) $(LAPACKFLAGS)
	
%.o: %.for
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f $(MODULES) $(OBJECTS) $(EXECUTABLE) $(DLL)
	