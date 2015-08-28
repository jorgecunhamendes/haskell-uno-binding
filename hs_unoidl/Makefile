#
# Haskell UNO IDL
#
# Before using, set LO_SRC and LO_INSTDIR

# LibreOffice source directory
# LO_SRC=

# LibreOffice installation directory
# LO_INSTDIR=

.DELETE_ON_ERROR:

LDPATH=LD_LIBRARY_PATH=$(LO_INSTDIR)/program

.PHONY : build run debug clean

build : hs_unoidl

run :
	$(LDPATH) ./hs_unoidl $(ARGS)

debug :
	$(LDPATH) gdb --args ./hs_unoidl $(ARGS)

clean :
	rm -r out

OBJECTS= \
	out/utils.cxx_o \
	out/writer.cxx_o \
	out/writer/cxx.cxx_o \
	out/writer/hxx.cxx_o \
	out/writer/hs.cxx_o \
	out/writer/utils.cxx_o \
	out/file.cxx_o \
	out/module.cxx_o \
	out/types.cxx_o \
	out/main.cxx_o

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
					-I"$(LO_INSTDIR)/sdk/include" \
					-I"src"

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
out/writer.cxx_o : src/writer.cxx src/writer.hxx \
	src/writer/cxx.hxx src/writer/hxx.hxx src/writer/hs.hxx
out/writer/utils.cxx_o : src/writer/utils.cxx src/writer/utils.hxx | out/writer
out/writer/cxx.cxx_o : src/writer/cxx.cxx src/writer/cxx.hxx \
	src/writer/writer.hxx src/types.hxx | out/writer
out/writer/hxx.cxx_o : src/writer/hxx.cxx src/writer/hxx.hxx \
	src/writer/writer.hxx src/types.hxx | out/writer
out/writer/hs.cxx_o : src/writer/hs.cxx src/writer/hs.hxx \
	src/writer/writer.hxx src/types.hxx | out/writer
out/file.cxx_o : src/file.cxx src/file.hxx
out/entity.cxx_o : src/entity.cxx src/entity.hxx
out/module.cxx_o : src/module.cxx src/module.hxx
out/types.cxx_o : src/types.cxx src/types.hxx

out :
	mkdir $@

out/writer : | out
	mkdir $@
