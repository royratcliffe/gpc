SOBJ = $(PACKSODIR)/gpc.$(SOEXT)
OBJ = c/pl.o c/polygon_blob.o c/vertex_list.o c/vertex.o c/tristrip_blob.o c/gpc.o

# See notes in README.md for explanation of no-unused-result warning.
CFLAGS += -O2 -fomit-frame-pointer -Wno-unused-result

all: $(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) -o $@ $(OBJ) $(SWISOLIB)

check::
install::
clean:
	rm -f $(OBJ)
distclean: clean
	rm -f $(SOBJ)
