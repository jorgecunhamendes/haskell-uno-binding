#
# Haskell UNO IDL
#
# Before using, set LO_SRC and LO_INSTDIR

# LibreOffice source directory
# LO_SRC=

# LibreOffice installation directory
# LO_INSTDIR=

.DELETE_ON_ERROR:

.PHONY : build run debug clean

build : hs_unoidl

run :
	$(LDPATH) ./hs_unoidl $(ARGS)

debug :
	$(LDPATH) gdb --args ./hs_unoidl $(ARGS)

clean :
	rm -r out

LDPATH=LD_LIBRARY_PATH=$(LO_INSTDIR)/program

OBJECTS= \
	out/main.cpp_o \
	out/writer.cxx_o \
	out/file.cxx_o \
	out/module.cxx_o \
	out/types.cxx_o

# Linking
LINK=$(LDPATH) g++
LIBS= -L"$(LO_INSTDIR)/program" -L"$(LO_INSTDIR)/sdk/lib" \
			-lunoidllo -luno_salhelpergcc3 -luno_sal

# Compilation
CPP=$(LDPATH) g++
CPPFLAGS= -std=c++11 \
					-D_GLIBCXX_DEBUG \
					-DCPPU_ENV=gcc3 -DHAVE_GCC_VISIBILITY_FEATURE -DLINUX -DUNX
INCLUDES= -I"$(LO_SRC)/include" \
					-I"$(LO_SRC)/config_host" \
					-I"$(LO_INSTDIR)/sdk/include"

# Main target
hs_unoidl : $(OBJECTS)
	$(LINK) -Wall -g -o $@ $^ $(LIBS)

# C++ file compilation
out/%.cpp_o : src/%.cpp | out
	$(CPP) -Wall -c -g -o $@ $(CPPFLAGS) $(INCLUDES) $<
out/%.cxx_o : src/%.cxx | out
	$(CPP) -Wall -c -g -o $@ $(CPPFLAGS) $(INCLUDES) $<

# C++ file dependencies
out/main.cpp_o : src/main.cpp
out/writer.cxx_o : src/writer.cxx src/writer.hxx
out/file.cxx_o : src/file.cxx src/file.hxx
out/module.cxx_o : src/module.cxx src/module.hxx
out/types.cxx_o : src/types.cxx src/types.hxx

out :
	mkdir $@
