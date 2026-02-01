;------------------------------------------------
; gfx_tiles.inc - 2bpp tiles from art/tiles.png
; TileData copied to VRAM $8000 at boot (LCD off)
;------------------------------------------------

SECTION "Tiles", ROM0              ; Tiles section (ROM0)
TileData:                          ; Tile blob start
    INCBIN "res/tiles.2bpp"
TileDataEnd:                       ; Tile blob end
