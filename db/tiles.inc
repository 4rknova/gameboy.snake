;------------------------------------------------                      
; gfx_tiles.inc - 2bpp tiles 0..15
; TileData copied to VRAM $8000 at boot (LCD off)
;------------------------------------------------

SECTION "Tiles", ROM0 ; Tiles section (ROM0)
                      ; --- Tile 0: empty ---
TileData:             ; Tile blob start
    ds 16,0           ; T0 empty (16 bytes)
                      ; --- Tile 1: wall ---
REPT 8                ; T1 wall (8 rows)
    db $FF,$FF        ; T1 row (solid 3)
ENDR                  ; T1 end
                      ; --- Tile 2: snake ---
REPT 8                ; T2 snake (8 rows)
    db $00,$FF        ; T2 row (solid 2)
ENDR                  ; T2 end
                      ; --- Tile 3: food ---
    db $00,$00        ; T3 food r0
    db $00,$00        ; T3 food r1
    db $18,$00        ; T3 food r2
    db $3C,$00        ; T3 food r3
    db $3C,$00        ; T3 food r4
    db $18,$00        ; T3 food r5
    db $00,$00        ; T3 food r6
    db $00,$00        ; T3 food r7
                      ; --- Tile 4: 'A' ---
    db $38,$38        ; T4 'A' r0
    db $44,$44        ; T4 'A' r1
    db $44,$44        ; T4 'A' r2
    db $7C,$7C        ; T4 'A' r3
    db $44,$44        ; T4 'A' r4
    db $44,$44        ; T4 'A' r5
    db $44,$44        ; T4 'A' r6
    db $00,$00        ; T4 'A' r7
                      ; --- Tile 5: 'E' ---
    db $7C,$7C        ; T5 'E' r0
    db $40,$40        ; T5 'E' r1
    db $40,$40        ; T5 'E' r2
    db $78,$78        ; T5 'E' r3
    db $40,$40        ; T5 'E' r4
    db $40,$40        ; T5 'E' r5
    db $7C,$7C        ; T5 'E' r6
    db $00,$00        ; T5 'E' r7
                      ; --- Tile 6: 'G' ---
    db $38,$38        ; T6 'G' r0
    db $44,$44        ; T6 'G' r1
    db $40,$40        ; T6 'G' r2
    db $5C,$5C        ; T6 'G' r3
    db $44,$44        ; T6 'G' r4
    db $44,$44        ; T6 'G' r5
    db $38,$38        ; T6 'G' r6
    db $00,$00        ; T6 'G' r7
                      ; --- Tile 7: 'K' ---
    db $44,$44        ; T7 'K' r0
    db $48,$48        ; T7 'K' r1
    db $50,$50        ; T7 'K' r2
    db $60,$60        ; T7 'K' r3
    db $50,$50        ; T7 'K' r4
    db $48,$48        ; T7 'K' r5
    db $44,$44        ; T7 'K' r6
    db $00,$00        ; T7 'K' r7
                      ; --- Tile 8: 'M' ---
    db $44,$44        ; T8 'M' r0
    db $6C,$6C        ; T8 'M' r1
    db $54,$54        ; T8 'M' r2
    db $44,$44        ; T8 'M' r3
    db $44,$44        ; T8 'M' r4
    db $44,$44        ; T8 'M' r5
    db $44,$44        ; T8 'M' r6
    db $00,$00        ; T8 'M' r7
                      ; --- Tile 9: 'N' ---
    db $44,$44        ; T9 'N' r0
    db $64,$64        ; T9 'N' r1
    db $54,$54        ; T9 'N' r2
    db $4C,$4C        ; T9 'N' r3
    db $44,$44        ; T9 'N' r4
    db $44,$44        ; T9 'N' r5
    db $44,$44        ; T9 'N' r6
    db $00,$00        ; T9 'N' r7
                      ; --- Tile 10: 'O' ---
    db $38,$38        ; T10 'O' r0
    db $44,$44        ; T10 'O' r1
    db $44,$44        ; T10 'O' r2
    db $44,$44        ; T10 'O' r3
    db $44,$44        ; T10 'O' r4
    db $44,$44        ; T10 'O' r5
    db $38,$38        ; T10 'O' r6
    db $00,$00        ; T10 'O' r7
                      ; --- Tile 11: 'P' ---
    db $78,$78        ; T11 'P' r0
    db $44,$44        ; T11 'P' r1
    db $44,$44        ; T11 'P' r2
    db $78,$78        ; T11 'P' r3
    db $40,$40        ; T11 'P' r4
    db $40,$40        ; T11 'P' r5
    db $40,$40        ; T11 'P' r6
    db $00,$00        ; T11 'P' r7
                      ; --- Tile 12: 'R' ---
    db $78,$78        ; T12 'R' r0
    db $44,$44        ; T12 'R' r1
    db $44,$44        ; T12 'R' r2
    db $78,$78        ; T12 'R' r3
    db $50,$50        ; T12 'R' r4
    db $48,$48        ; T12 'R' r5
    db $44,$44        ; T12 'R' r6
    db $00,$00        ; T12 'R' r7
                      ; --- Tile 13: 'S' ---
    db $3C,$3C        ; T13 'S' r0
    db $40,$40        ; T13 'S' r1
    db $40,$40        ; T13 'S' r2
    db $38,$38        ; T13 'S' r3
    db $04,$04        ; T13 'S' r4
    db $04,$04        ; T13 'S' r5
    db $78,$78        ; T13 'S' r6
    db $00,$00        ; T13 'S' r7
                      ; --- Tile 14: 'T' ---
    db $7C,$7C        ; T14 'T' r0
    db $10,$10        ; T14 'T' r1
    db $10,$10        ; T14 'T' r2
    db $10,$10        ; T14 'T' r3
    db $10,$10        ; T14 'T' r4
    db $10,$10        ; T14 'T' r5
    db $10,$10        ; T14 'T' r6
    db $00,$00        ; T14 'T' r7
                      ; --- Tile 15: 'V' ---
    db $44,$44        ; T15 'V' r0
    db $44,$44        ; T15 'V' r1
    db $44,$44        ; T15 'V' r2
    db $44,$44        ; T15 'V' r3
    db $28,$28        ; T15 'V' r4
    db $10,$10        ; T15 'V' r5
    db $10,$10        ; T15 'V' r6
    db $00,$00        ; T15 'V' r7
TileDataEnd:          ; Tile blob end
