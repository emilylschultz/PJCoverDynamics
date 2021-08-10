################
# PIED local adaptation Makefile
# Emily Schultz
# Updated 15 June 2020
################

# Key variables to define
RDIR = ./Code
#PRISM_OUT = ./DataPrep/prism.Rout

# Create list of R source files
RSOURCE = $(wildcard $(RDIR)/*/*.R)

# Files to indicate when the RSOURCE file was run
OUT_FILES = $(RSOURCE:.R=.Rout)

# Default target
all: $(OUT_FILES) 