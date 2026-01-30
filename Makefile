# --------------------------------------------------------------------
# Makefile for RGBDS Game Boy projects (tools in ./tools)
# Output ROM + intermediates go to ./bin
#
# Usage:
#   make
#   make run
#   make clean
# --------------------------------------------------------------------

OUTDIR ?= bin
SRCDIR ?= src
OUTDIR ?= bin
LIBDIR ?= lib
TOOLSDIR ?= tools

NAME   ?= snake
SRC    ?= $(SRCDIR)/main.asm

RGBASM  := $(TOOLSDIR)/rgbasm
RGBLINK := $(TOOLSDIR)/rgblink
RGBFIX  := $(TOOLSDIR)/rgbfix
EMU     := java -jar $(TOOLSDIR)/emulicious/Emulicious.jar

OBJ := $(OUTDIR)/main.o
ROM := $(OUTDIR)/$(NAME).gb
SYM := $(OUTDIR)/$(NAME).sym
MAP := $(OUTDIR)/$(NAME).map

.PHONY: all clean run rom

.DEFAULT_GOAL := rom

all: $(ROM)

rom: $(ROM)

$(OUTDIR):
	@mkdir -p $(OUTDIR)

# Assemble
$(OBJ): $(SRC) | $(OUTDIR)
	$(RGBASM) -Wall -i $(LIBDIR) -o $@ $<

# Link + Fix header/checksums
$(ROM): $(OBJ)
	$(RGBLINK) -o $@ -n $(SYM) -m $(MAP) $<
	$(RGBFIX) -v -p 0 $@

run: $(ROM)
	@# Emulicious accepts a ROM path as an argument
	$(EMU) $(ROM)

clean:
	@rm -rf $(OUTDIR)
