# This is an commentary line in a makefile
#Directories

# Defining variables
#directories
DIR := ${CURDIR}
SRCDIR = $(DIR)/src
MODDIR = $(DIR)/bin/mod
OBJDIR= $(DIR)/bin/obj
#compiler and flags
flags =  -O3
fc = /usr/bin/mpif90.mpich
#makefile
.PHONY: all

all:directories init comm


directories:
	mkdir -p $(OBJDIR) $(MODDIR) $(OUTDIR)

init:
	$(fc) $(flags) -o init.out $(SRCDIR)/initialize.f90

comm:  mesh
	$(fc) $(flags) -o comm.out  $(wildcard $(OBJDIR)/*.o) $(SRCDIR)/communication.f90  -I$(MODDIR)
mesh: dataType bndry
	$(fc) -c $(flags) -J$(MODDIR) -o $(OBJDIR)/mesh_module.o $(SRCDIR)/mesh_module.f90 -I$(MODDIR)
dataType:
	$(fc) -c $(flags) -J$(MODDIR) -o $(OBJDIR)/data_type_module.o $(SRCDIR)/data_type_module.f90 -I$(MODDIR)
bndry: dataType
	$(fc) -c $(flags) -J$(MODDIR) -o $(OBJDIR)/boundary_module.o $(SRCDIR)/boundary_module.f90 -I$(MODDIR)
clean:
	rm $(OBJDIR)/*.o $(MODDIR)/*.mod
