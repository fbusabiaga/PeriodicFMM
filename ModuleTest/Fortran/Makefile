include ../MakefileInc.mk

INCLUDE_DIRS = $(Trilinos_INCLUDE_DIRS) $(Trilinos_TPL_INCLUDE_DIRS) $(USERINCLUDE) -I../../
LIBRARY_DIRS = $(Trilinos_LIBRARY_DIRS) $(Trilinos_TPL_LIBRARY_DIRS) 
LIBRARIES = $(Trilinos_LIBRARIES) $(Trilinos_TPL_LIBRARIES) $(USERLIB)

# Files
SRC = ../StokesFMM3D/ChebNodal.cpp ../../FMM/FMMWrapper.cpp ../../FMM/FMMWrapper_C_api.cpp 
SRC_FORTRAN = ../../FMM/FMMWrapper_mod.f90 
INC = ../StokesFMM3D/ChebNodal.h ../../FMM/FMMWrapper.h ../StokesFMM3D/Ewald.hpp ../../FMM/FMMWrapperC.h

# Definitions
TARGET = test
OBJ =    $(SRC:.cpp=.o) 
OBJ += $(SRC_FORTRAN:.f90=.o)

$(info $$OBJ is [${OBJ}])
$(info $$OBJ_FC is [${OBJ_FC}])

all: $(TARGET).exe

# Compilation rules
%.o: %.cpp 
	$(CXX) $(CXXFLAGS) $(INCLUDE_DIRS) -c $*.cpp -o $*.o

%.o: %.f90
	$(FC) $(FCFLAGS) $(INCLUDE_DIRS) -c $*.f90 -o $*.o

# Individual dependencies
$(OBJ): $(INC)

# Compile target
$(TARGET).exe: $(OBJ) $(OBJ_FC) $(TARGET).f90 ; 
	$(FC) $(FCFLAGS) $(OBJ) $(OBJ_FC) test.f90 -o $(TARGET).exe $(LINKFLAGS_FC) $(LDLIBS)


all: $(TARGET)

clean: 
	rm ./$(OBJ)
	rm ./$(TARGET).exe

