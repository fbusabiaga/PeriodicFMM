include ../../MakefileInc.mk

INCLUDE_DIRS = $(USERINCLUDE) -I../../
LIBRARY_DIRS = 
LIBRARIES = $(USERLIB)

# Files

SRC =   Stokes3D3D.cpp
INC =   ../../Util/SVD_pvfmm.hpp 
         
# Definitions

EXE =   $(SRC:.cpp=.X)
OBJ =   $(SRC:.cpp=.o)

# Link rule
$(EXE):	$(OBJ)
	$(LINK) $(OBJ)  -o $(EXE) $(LINKFLAGS) $(SYSLIB) $(LIBRARY_DIRS) $(LIBRARIES)

# Compilation rules
.cpp.o:
	$(CXX) $(CXXFLAGS) $(INCLUDE_DIRS) -c $*.cpp -o $*.o

# Individual dependencies
$(OBJ): $(INC)

all: $(EXE)

clean: 
	rm ./*.o
	rm ./$(EXE)
