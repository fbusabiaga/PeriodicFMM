include ../MakefileInc.mk

# Select NO_WALL or WALL
DOMAIN=WALL

CXXFLAGS += -D $(DOMAIN)
INCLUDE_DIRS = $(Trilinos_INCLUDE_DIRS) $(Trilinos_TPL_INCLUDE_DIRS) $(USERINCLUDE) -I../../
LIBRARY_DIRS = $(Trilinos_LIBRARY_DIRS) $(Trilinos_TPL_LIBRARY_DIRS) 
LIBRARIES = $(Trilinos_LIBRARIES) $(Trilinos_TPL_LIBRARIES) $(USERLIB)

# Files
SRC = ../StokesFMM3D/ChebNodal.cpp  ../../FMM/FMMWrapper_C_api.cpp 
SRC_FORTRAN = ../../FMM/FMMWrapper_mod.f90 
INC = ../StokesFMM3D/ChebNodal.h  ../StokesFMM3D/Ewald.hpp ../../FMM/FMMWrapperC.h

ifeq ($(DOMAIN),NO_WALL)
SRC += ../../FMM/FMMWrapper.cpp
INC += ../../FMM/FMMWrapper.h

else

ifeq ($(DOMAIN),WALL)
SRC += ../../FMM/FMMWrapperWall2D.cpp
INC += ../../FMM/FMMWrapperWall2D.h

endif
endif


# Definitions
TARGET = test
OBJ =    $(SRC:.cpp=.o) 
OBJ_FC = $(SRC_FORTRAN:.f90=.o)

$(info $$OBJ is [${OBJ}])
$(info $$OBJ_FC is [${OBJ_FC}])

all: $(TARGET).exe

# Compilation rules
%.o: %.cpp 
	$(CXX) $(CXXFLAGS) $(INCLUDE_DIRS) -c $*.cpp -o $*.o

# Compile fortran code
../../FMM/FMMWrapper_mod.o: ../../FMM/FMMWrapper_mod.f90 ../../FMM/FMMWrapper_C_def.f90
	$(FC) -c ../../FMM/FMMWrapper_mod.f90 -o ../../FMM/FMMWrapper_mod.o

# Individual dependencies
$(OBJ): $(INC)

# Compile target
$(TARGET).exe: $(OBJ) ../../FMM/FMMWrapper_mod.o $(TARGET).f90 ; 
	$(FC) $(FCFLAGS) $(OBJ) $(OBJ_FC) test.f90 -o $(TARGET).exe $(LINKFLAGS_FC) $(LDLIBS)



clean: 
	rm ./$(OBJ)						2> /dev/null || true
	rm ./$(OBJ_FC)				2> /dev/null || true
	rm ./$(TARGET).exe		2> /dev/null || true
	rm ./libfmmwrapper.mod 2> /dev/null || true

