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

all:directories init


directories:
	mkdir -p $(OBJDIR) $(MODDIR) $(OUTDIR)

init:
	$(fc) $(flags) -o init.out $(SRCDIR)/initialize.f90

clean:
	rm $(OBJDIR)/*.o $(MODDIR)/*.mod
