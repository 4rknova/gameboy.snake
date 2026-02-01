# --------------------------------------------------------------------
# Makefile for RGBDS Game Boy projects (tools in ./tools)
# Output ROM + intermediates go to ./bin
#
# Usage:
#   make
#   make run
#   make clean
# --------------------------------------------------------------------

ARTDIR ?= art
OUTDIR ?= bin
SRCDIR ?= src
OUTDIR ?= bin
LIBDIR ?= lib
RESDIR ?= res
TOOLSDIR ?= tools

NAME   ?= snake
SRC    ?= $(SRCDIR)/main.asm

RGBASM  := $(TOOLSDIR)/rgbasm
RGBLINK := $(TOOLSDIR)/rgblink
RGBFIX  := $(TOOLSDIR)/rgbfix
RGBGFX  := $(TOOLSDIR)/rgbgfx
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

$(RESDIR):
	@mkdir -p $(RESDIR)

# Convert tiles PNG to 2bpp
TILES_PNG  := $(ARTDIR)/tiles.png
TILES_2BPP := $(RESDIR)/tiles.2bpp

$(TILES_2BPP): $(TILES_PNG) | $(RESDIR)
	$(RGBGFX) -o $@ -L 0,0:16,16 $<

# Assemble
$(OBJ): $(SRC) $(TILES_2BPP) | $(OUTDIR)
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
	@rm -rf $(RESDIR)
