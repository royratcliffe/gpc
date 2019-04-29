SOBJ = $(PACKSODIR)/gpc.$(SOEXT)
OBJ = c/pl.o c/polygon_blob.o c/vertex_list.o c/vertex.o c/gpc.o

CFLAGS += -O2 -fomit-frame-pointer

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
