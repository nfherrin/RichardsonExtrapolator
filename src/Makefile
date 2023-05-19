# Defining variables
FC:=gfortran
EXE:=RichExtrap
EXT:=.exe

all :     TYPE :=
debug:    TYPE := _Debug
catch:    TYPE := _Catch
coverage: TYPE := _Coverage

all:      OPT := -O3 -Wall -Werror
debug:    OPT := -O0 -g -fbacktrace -ffpe-trap=zero,overflow,underflow -fcheck=all -Wall -Werror
catch:    OPT := -O0 -g -Wall -W -Wsurprising -Wextra -Werror
coverage: OPT := -O0 -g --coverage -Wall -Werror

#-----------------------------------------------------------------------------------
#Add source files as necessary
#If the files are not compiled using the generic rules, add commands under their
#rule declaration. Add these items to FILTER
#-----------------------------------------------------------------------------------

SRC := main.f90 \
	   globals.f90 \
	   input_module.f90 \
	   output_module.f90 \
	   richardson_module.f90

OBJ := $(SRC:.f90=.o)

#-----------------------------------------------------------------------------------
#Phony targets for cleaning and building
#-----------------------------------------------------------------------------------
.PHONY: all debug catch coverage clean reset

EXEC_NAME=$(EXE)
EXEC_FILE=../$(EXEC_NAME)

all: $(EXEC_FILE)

debug: $(EXEC_FILE)

catch: $(EXEC_FILE)

coverage: $(EXEC_FILE)

#Intended to clean up compilation artifacts but leave executable & coverage
clean:
	$(RM) $(OBJ) *.mod

#Intended to reset directory to fresh state with no exe or artifacts
reset : clean
	$(RM) ../$(EXE)*

#Generics for source files
%.o : %.f90
	$(FC) $(OPT) -c $<

#Executable rule
$(EXEC_FILE): $(OBJ)
	$(FC) -o $@$(TYPE)$(EXT) $(OPT) $^ $(BLIBS)

#-----------------------------------------------------------------------------------
#Dependency List
#-----------------------------------------------------------------------------------
main.o : globals.o input_module.o output_module.o richardson_module.o
input_module.o : globals.o
output_module.o : globals.o
