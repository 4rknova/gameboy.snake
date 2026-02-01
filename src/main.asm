; -----------------------------------------------------------------------------
; Snake
; -----------------------------------------------------------------------------

INCLUDE "hardware.inc"             ; Common hardware symbols (RGBDS)
DEF rJOYP        EQU rP1           ; Alias for JOYP/P1 register

DEF _BGMap0      EQU _SCRN0        ; BG Map base (9800h)
DEF _TileVRAM    EQU _VRAM         ; Tile data base (8000h)

DEF PLAY_W       EQU 20            ; Visible playfield width (tiles)
DEF PLAY_H       EQU 18            ; Visible playfield height (tiles)

DEF X_MAX        EQU (PLAY_W-2)    ; Inner max X (18)
DEF Y_MAX        EQU (PLAY_H-2)    ; Inner max Y (16)

DEF MAX_SNAKE    EQU 64            ; Must be power-of-two for AND wrap
DEF SNAKE_MASK   EQU (MAX_SNAKE-1) ; 63
DEF SPEED_FRAMES EQU 8             ; Move every N frames

DEF STATE_TITLE  EQU 0             ; Title
DEF STATE_PLAY   EQU 1             ; Play
DEF STATE_OVER   EQU 2             ; Game over

DEF DIR_UP       EQU 0             ; Up
DEF DIR_RIGHT    EQU 1             ; Right
DEF DIR_DOWN     EQU 2             ; Down
DEF DIR_LEFT     EQU 3             ; Left

DEF TILE_EMPTY   EQU $00           ; Tile 0x00: space (font)
DEF TILE_WALL    EQU $E1           ; Tile 0xE1: solid block
DEF TILE_SNAKE   EQU $F0           ; Tile 0xF0: snake segment
DEF TILE_FOOD    EQU $E3           ; Tile 0xE3: food

DEF JOY_RIGHT    EQU 0             ; Right
DEF JOY_LEFT     EQU 1             ; Left
DEF JOY_UP       EQU 2             ; Up
DEF JOY_DOWN     EQU 3             ; Down
DEF JOY_A        EQU 4             ; A
DEF JOY_B        EQU 5             ; B
DEF JOY_SEL      EQU 6             ; Select
DEF JOY_START    EQU 7             ; Start

DEF DIRTY_HEAD   EQU 0             ; Draw head
DEF DIRTY_TAIL   EQU 1             ; Clear tail
DEF DIRTY_FOOD   EQU 2             ; Draw food

DEF OCC_EMPTY    EQU 0             ; Occupancy: empty
DEF OCC_SNAKE    EQU 1             ; Occupancy: snake
DEF OCC_FOOD     EQU 2             ; Occupancy: food

SECTION "Header", ROM0[$0100]      ; Entry
    jp Start                       ; Jump to start
    ds $0150 - @, 0                ; Pad header area

SECTION "Code", ROM0               ; Main code

Start:                             ; Entry point
    di                             ; No interrupts
    ld sp, $DFF0                   ; Stack
    xor a                          ; A = 0
    ld [rNR52], a                  ; Sound off
    ld a, [rLCDC]                  ; LCDC
    res 7, a                       ; LCD OFF
    ld [rLCDC], a                  ; Write
    xor a                          ; A = 0
    ld [rSCX], a                   ; SCX=0
    ld [rSCY], a                   ; SCY=0
    ld a, $E4                      ; Palette
    ld [rBGP], a                   ; Set palette

    ld hl, TileData                ; HL = tile ROM
    ld de, _TileVRAM               ; DE = tile VRAM
    ld bc, TileDataEnd - TileData  ; BC = bytes
    call Memcpy                    ; Copy tiles (LCD off safe)

    ld a, [rDIV]                   ; Seed
    or $5A                         ; Mix
    ld [wRand], a                  ; Store RNG

    call InitTitleScreen           ; Build title (LCD off safe)

    ld a, %10010001                ; LCD ON, BG ON, tiles 8000, map 9800
    ld [rLCDC], a                  ; Enable LCD

MainLoop:                          ; Frame loop
    call WaitVBlankStart           ; Enter VBlank
    call DoPendingRebuildInVBlank  ; Handle queued rebuilds (LCD off bulk)
    call TitleBlinkInVBlank        ; Title blink (few writes)
    call RenderDirtyInVBlank       ; Play dirty updates (few writes)
    call PauseOverlayInVBlank      ; Pause overlay (few writes)
    call WaitVBlankEnd             ; Exit VBlank

    call ReadJoypad                ; Poll joypad

    ld a, [wState]                 ; A = state
    cp STATE_TITLE                 ; Title?
    jr z, .S_Title                 ; Branch
    cp STATE_PLAY                  ; Play?
    jr z, .S_Play                  ; Branch
    jr .S_Over                     ; Else over
    
.S_Title:                          ; Title logic
    ld a, [wJoyPressed]            ; edges
    bit JOY_START, a               ; Start?
    jr z, .DoneTitle               ; no
    ld a, STATE_PLAY               ; next = play
    ld [wNextState], a             ; queue
    ld a, 1                        ; rebuild flag
    ld [wNeedRebuild], a           ; request rebuild
.DoneTitle:                        ; done
    jr MainLoop                    ; loop

.S_Play:                           ; Play logic
    ld a, [wIgnoreStart]           ; ignore first start?
    or a                           ; any?
    jr z, .CheckPauseStart         ; no -> check start
    xor a                          ; A=0
    ld [wIgnoreStart], a           ; clear ignore flag
    jr .NoPauseToggle              ; skip toggle this frame
.CheckPauseStart:                  ; check pause toggle
    ld a, [wJoyPressed]            ; edges
    bit JOY_START, a               ; Start?
    jr z, .NoPauseToggle           ; no
    ld a, [wPaused]                ; paused?
    xor 1                          ; toggle
    ld [wPaused], a                ; store
    ld a, 1                        ; mark pause draw
    ld [wPauseDirty], a            ; request overlay update
.NoPauseToggle:                    ; done toggle
    ld a, [wPaused]                ; paused?
    or a                           ; flags
    jr nz, .DonePlay               ; if paused, skip update
    ld a, [wDirChanged]            ; direction changed this step?
    or a                           ; any?
    jr nz, .SkipDirUpdate          ; yes -> skip
    call UpdateDirectionFromHeld   ; update direction
.SkipDirUpdate:                    ; done update
    ld a, [wMoveCounter]           ; tick counter
    inc a                          ; ++
    ld [wMoveCounter], a           ; store
    cp SPEED_FRAMES                ; time to move?
    jr c, .DonePlay                ; not yet
    xor a                          ; A=0
    ld [wMoveCounter], a           ; reset
    ld [wDirChanged], a            ; allow next turn after move
    call StepSnakeLogic            ; advance snake (no VRAM)
.DonePlay:                         ; done
    jr MainLoop                    ; loop

.S_Over:                           ; Over logic
    ld a, [wJoyPressed]            ; edges
    bit JOY_START, a               ; Start?
    jr z, .DoneOver                ; no
    ld a, STATE_TITLE              ; next=title
    ld [wNextState], a             ; queue
    ld a, 1                        ; rebuild flag
    ld [wNeedRebuild], a           ; request rebuild
.DoneOver:                         ; done
    jp MainLoop                    ; loop

; -----------------------------------------------------------------------------
; Rebuild handler (VBlank-only): LCD OFF -> build -> LCD ON
; -----------------------------------------------------------------------------

DoPendingRebuildInVBlank:          ; rebuild if requested
    ld a, [wNeedRebuild]           ; flag?
    or a                           ; any?
    ret z                          ; none => return
    ld a, [rLCDC]                  ; LCDC
    res 7, a                       ; LCD OFF
    ld [rLCDC], a                  ; disable LCD

    ld a, [wNextState]             ; which screen?
    cp STATE_TITLE                 ; title?
    jr nz, .ChkPlay                ; else
    call InitTitleScreen           ; build title
    jr .Finish                     ; done
.ChkPlay:                          ; check play
    cp STATE_PLAY                  ; play?
    jr nz, .ChkOver                ; else
    call InitPlayScreen            ; build play
    jr .Finish                     ; done
.ChkOver:                          ; else over
    call InitOverScreen            ; build over
.Finish:                           ; finish
    ld a, %10010001                ; LCD ON config
    ld [rLCDC], a                  ; enable LCD
    xor a                          ; A=0
    ld [wNeedRebuild], a           ; clear rebuild
    ld a, [wNextState]             ; next
    ld [wState], a                 ; commit
    xor a                          ; A=0
    ld [wJoyPrev], a               ; clear prev joy
    ld [wDirtyFlags], a            ; clear dirty
    ret                            ; return

; -----------------------------------------------------------------------------
; Screens (LCD off safe)
; -----------------------------------------------------------------------------

InitTitleScreen:                   ; title build
    call ClearBGMap                ; clear BG map
    call DrawBorder                ; border
    ld b, 2                        ; y
    ld c, 7                        ; x
    ld de, StrTitle                ; "SNAKE"
    call BlitTextASCII             ; draw
    ld b, 13                       ; y
    ld c, 4                        ; x
    ld de, StrPressStart           ; "PRESS START"
    call BlitTextASCII             ; draw
    xor a                          ; A=0
    ld [wFrame], a                 ; blink timer
    ld [wBlink], a                 ; blink state
    ld [wMoveCounter], a           ; move counter
    ld a, STATE_TITLE              ; title
    ld [wState], a                 ; commit
    ld [wNextState], a             ; next
    ret                            ; return

InitOverScreen:                    ; game over build
    call ClearBGMap                ; clear
    call DrawBorder                ; border
    ld b, 8                        ; y
    ld c, 5                        ; x
    ld de, StrGameOver             ; "GAME OVER"
    call BlitTextASCII             ; draw
    ld b, 10                       ; y
    ld c, 4                        ; x
    ld de, StrPressStart           ; prompt
    call BlitTextASCII             ; draw
    ld a, STATE_OVER               ; over
    ld [wState], a                 ; commit
    ld [wNextState], a             ; next
    ret                            ; return

InitPlayScreen:                    ; play build
    call ClearBGMap                ; clear BG
    call DrawBorder                ; border
    call ClearOcc                  ; clear occupancy grid

    ld a, 3                        ; len=3
    ld [wSnakeLen], a              ; store
    xor a                          ; A=0
    ld [wMoveCounter], a           ; reset pacing
    ld [wPaused], a                ; clear pause
    ld [wPauseDirty], a            ; clear pause dirty
    ld [wDirChanged], a            ; clear direction change latch
    ld a, 1                        ; ignore start on first play frame
    ld [wIgnoreStart], a           ; set ignore flag
    ld [wDirtyFlags], a            ; clear dirty

    ld a, DIR_RIGHT                ; dir=right
    ld [wDir], a                   ; store

    ld a, 0                        ; tailIdx=0
    ld [wTailIdx], a               ; store
    ld a, 2                        ; headIdx=2
    ld [wHeadIdx], a               ; store

    ld hl, wSnakeX                 ; X array
    ld a, 8                        ; tail X
    ld [hli], a                    ; X[0]=8
    ld a, 9                        ; mid X
    ld [hli], a                    ; X[1]=9
    ld a, 10                       ; head X
    ld [hl], a                     ; X[2]=10

    ld hl, wSnakeY                 ; Y array
    ld a, 9                        ; Y
    ld [hli], a                    ; Y[0]=9
    ld [hli], a                    ; Y[1]=9
    ld [hl], a                     ; Y[2]=9

    ld b, 9                        ; y
    ld c, 8                        ; x
    ld a, OCC_SNAKE                ; occ=snake
    call SetOcc                    ; mark
    ld a, TILE_SNAKE               ; tile
    call SetBGTile                 ; draw

    ld b, 9                        ; y
    ld c, 9                        ; x
    ld a, OCC_SNAKE                ; occ
    call SetOcc                    ; mark
    ld a, TILE_SNAKE               ; tile
    call SetBGTile                 ; draw

    ld b, 9                        ; y
    ld c, 10                       ; x
    ld a, OCC_SNAKE                ; occ
    call SetOcc                    ; mark
    ld a, TILE_SNAKE               ; tile
    call SetBGTile                 ; draw

    call SpawnFoodNoDraw           ; choose food (uses occ)
    ld a, [wFoodY]                 ; y
    ld b, a                        ; B=y
    ld a, [wFoodX]                 ; x
    ld c, a                        ; C=x
    ld a, OCC_FOOD                 ; occ=food
    call SetOcc                    ; mark
    ld a, TILE_FOOD                ; tile
    call SetBGTile                 ; draw

    ld a, STATE_PLAY               ; play
    ld [wState], a                 ; commit
    ld [wNextState], a             ; next
    ret                            ; return

; -----------------------------------------------------------------------------
; Title blink (VBlank-only)
; -----------------------------------------------------------------------------

TitleBlinkInVBlank:                ; blink prompt
    ld a, [wState]                 ; state
    cp STATE_TITLE                 ; title?
    ret nz                         ; no
    ld a, [wFrame]                 ; frame
    inc a                          ; ++
    ld [wFrame], a                 ; store
    and 31                         ; /32
    ret nz                         ; only at wrap
    ld a, [wBlink]                 ; blink
    xor 1                          ; toggle
    ld [wBlink], a                 ; store
    or a                           ; flags
    jr z, .Show                    ; 0 => show
    ld b, 13                       ; y
    ld c, 4                        ; x
    ld de, StrSpaces11             ; spaces
    call BlitTextASCII             ; hide
    ret                            ; return
.Show:                             ; show
    ld b, 13                       ; y
    ld c, 4                        ; x
    ld de, StrPressStart           ; prompt
    call BlitTextASCII             ; show
    ret                            ; return

; -----------------------------------------------------------------------------
; Snake step (no VRAM writes): uses occupancy + ring buffer
; -----------------------------------------------------------------------------

StepSnakeLogic:                    ; advance snake
    xor a                          ; A=0
    ld [wGrow], a                  ; grow=0

    ld a, [wTailIdx]               ; tail index
    ld c, a                        ; C=idx
    ld b, 0                        ; B=0
    ld hl, wSnakeX                 ; HL=X base
    add hl, bc                     ; HL=&X[tail]
    ld a, [hl]                     ; A=tailX
    ld [wTailX], a                 ; save (also used for allowed-tail move)
    ld hl, wSnakeY                 ; HL=Y base
    add hl, bc                     ; HL=&Y[tail]
    ld a, [hl]                     ; A=tailY
    ld [wTailY], a                 ; save

    ld a, [wHeadIdx]               ; head index
    ld c, a                        ; C=idx
    ld b, 0                        ; B=0
    ld hl, wSnakeX                 ; HL=X base
    add hl, bc                     ; HL=&X[head]
    ld a, [hl]                     ; A=headX
    ld [wNewX], a                  ; newX=headX (working)
    ld hl, wSnakeY                 ; HL=Y base
    add hl, bc                     ; HL=&Y[head]
    ld a, [hl]                     ; A=headY
    ld [wNewY], a                  ; newY=headY (working)

    ld a, [wDir]                   ; dir
    cp DIR_UP                      ; up?
    jr nz, .ChkR                   ; no
    ld a, [wNewY]                  ; newY
    dec a                          ; --
    ld [wNewY], a                  ; store
    jr .DirDone                    ; done
.ChkR:                             ; right?
    cp DIR_RIGHT                   ; right?
    jr nz, .ChkD                   ; no
    ld a, [wNewX]                  ; newX
    inc a                          ; ++
    ld [wNewX], a                  ; store
    jr .DirDone                    ; done
.ChkD:                             ; down?
    cp DIR_DOWN                    ; down?
    jr nz, .DirL                   ; no
    ld a, [wNewY]                  ; newY
    inc a                          ; ++
    ld [wNewY], a                  ; store
    jr .DirDone                    ; done
.DirL:                             ; left
    ld a, [wNewX]                  ; newX
    dec a                          ; --
    ld [wNewX], a                  ; store
.DirDone:                          ; direction applied

    ld a, [wNewX]                  ; x
    cp 0                           ; wall?
    jp z, .GameOver                ; yes
    cp (PLAY_W-1)                  ; wall?
    jp z, .GameOver                ; yes
    ld a, [wNewY]                  ; y
    cp 0                           ; wall?
    jp z, .GameOver                ; yes
    cp (PLAY_H-1)                  ; wall?
    jp z, .GameOver                ; yes

    ld a, [wNewY]                  ; B=y
    ld b, a                        ; B=y
    ld a, [wNewX]                  ; C=x
    ld c, a                        ; C=x
    call GetOcc                    ; A = occ at (x,y)
    cp OCC_EMPTY                   ; empty?
    jr z, .OccOK                   ; ok
    cp OCC_FOOD                    ; food?
    jr nz, .MaybeTail              ; else maybe snake/tail
    ld a, 1                        ; grow=1
    ld [wGrow], a                  ; store
    jr .OccOK                      ; ok

.MaybeTail:                        ; occ == snake
    ld a, [wNewX]                  ; compare with tailX
    ld d, a                        ; D=newX
    ld a, [wTailX]                 ; A=tailX
    cp d                           ; newX==tailX?
    jp nz, .GameOver               ; no => collide (JP fixes range)
    ld a, [wNewY]                  ; compare with tailY
    ld d, a                        ; D=newY
    ld a, [wTailY]                 ; A=tailY
    cp d                           ; newY==tailY?
    jp nz, .GameOver               ; no => collide (JP fixes range)
                                                                 ; yes => moving into tail tile is allowed (tail will vacate)
.OccOK:                            ; destination allowed

    ld a, [wHeadIdx]               ; headIdx
    inc a                          ; headIdx++
    and SNAKE_MASK                 ; wrap 0..63
    ld [wHeadIdx], a               ; store new headIdx

    ld c, a                        ; C=headIdx
    ld b, 0                        ; B=0
    ld hl, wSnakeX                 ; HL=X base
    add hl, bc                     ; HL=&X[headIdx]
    ld a, [wNewX]                  ; newX
    ld [hl], a                     ; store
    ld hl, wSnakeY                 ; HL=Y base
    add hl, bc                     ; HL=&Y[headIdx]
    ld a, [wNewY]                  ; newY
    ld [hl], a                     ; store

    ld a, [wNewX]                  ; head dirty X
    ld [wHeadX], a                 ; store
    ld a, [wNewY]                  ; head dirty Y
    ld [wHeadY], a                 ; store
    ld a, [wDirtyFlags]            ; flags
    set DIRTY_HEAD, a              ; mark head
    ld [wDirtyFlags], a            ; store

    ld a, [wNewY]                  ; B=y
    ld b, a                        ; B=y
    ld a, [wNewX]                  ; C=x
    ld c, a                        ; C=x
    ld a, OCC_SNAKE                ; occ=snake
    call SetOcc                    ; mark occupancy

    ld a, [wGrow]                  ; grow?
    or a                           ; flags
    jr nz, .GrowPath               ; yes => no tail move

                                   ; tail move (not growing) (FIXED: handle new==tail)
    ld a, [wTailX]                 ; A = tailX
    ld d, a                        ; D = tailX
    ld a, [wNewX]                  ; A = newX
    cp d                           ; newX == tailX ?
    jr nz, .DoTailClear            ; if not, clear normally
    ld a, [wTailY]                 ; A = tailY
    ld d, a                        ; D = tailY
    ld a, [wNewY]                  ; A = newY
    cp d                           ; newY == tailY ?
    jr nz, .DoTailClear            ; if not, clear normally

                                   ; moved into tail tile: do NOT clear occ or BG
    ld a, [wTailIdx]               ; tailIdx++
    inc a                          ; ++
    and SNAKE_MASK                 ; wrap
    ld [wTailIdx], a               ; store
    ret                            ; done

.DoTailClear:                      ; normal tail clear
    ld a, [wTailY]                 ; B = tailY
    ld b, a                        ; B
    ld a, [wTailX]                 ; C = tailX
    ld c, a                        ; C
    ld a, OCC_EMPTY                ; occ = empty
    call SetOcc                    ; clear occupancy

    ld a, [wDirtyFlags]            ; flags
    set DIRTY_TAIL, a              ; mark tail clear
    ld [wDirtyFlags], a            ; store

    ld a, [wTailIdx]               ; tailIdx++
    inc a                          ; ++
    and SNAKE_MASK                 ; wrap
    ld [wTailIdx], a               ; store
    ret                            ; done


.GrowPath:                         ; grow path
    ld a, [wSnakeLen]              ; len
    cp MAX_SNAKE                   ; full?
    jp z, .GameOver                ; treat as game over
    inc a                          ; len++
    ld [wSnakeLen], a              ; store

    call SpawnFoodNoDraw           ; choose new food location (empty in occ)
    ld a, [wFoodY]                 ; B=y
    ld b, a                        ; B=y
    ld a, [wFoodX]                 ; C=x
    ld c, a                        ; C=x
    ld a, OCC_FOOD                 ; occ=food
    call SetOcc                    ; mark
    ld a, [wDirtyFlags]            ; flags
    set DIRTY_FOOD, a              ; mark food draw
    ld [wDirtyFlags], a            ; store
    ret                            ; done

.GameOver:                         ; queue game over
    ld a, STATE_OVER               ; next=over
    ld [wNextState], a             ; store
    ld a, 1                        ; rebuild flag
    ld [wNeedRebuild], a           ; request rebuild
    ret                            ; return

; -----------------------------------------------------------------------------
; Dirty renderer (VBlank-only): 0..3 BG writes
; -----------------------------------------------------------------------------

RenderDirtyInVBlank:               ; apply dirty writes
    ld a, [wState]                 ; state
    cp STATE_PLAY                  ; only play
    ret nz                         ; else return
    ld a, [wDirtyFlags]            ; flags
    or a                           ; any?
    ret z                          ; none

    ld a, [wDirtyFlags]            ; flags
    bit DIRTY_TAIL, a              ; tail?
    jr z, .NoTail                  ; skip
    ld a, [wTailY]                 ; B=tailY
    ld b, a                        ; B
    ld a, [wTailX]                 ; C=tailX
    ld c, a                        ; C
    ld a, TILE_EMPTY               ; tile
    call SetBGTile                 ; clear tail
.NoTail:                           ; done

    ld a, [wDirtyFlags]            ; flags
    bit DIRTY_HEAD, a              ; head?
    jr z, .NoHead                  ; skip
    ld a, [wHeadY]                 ; B=headY
    ld b, a                        ; B
    ld a, [wHeadX]                 ; C=headX
    ld c, a                        ; C
    ld a, TILE_SNAKE               ; tile
    call SetBGTile                 ; draw head
.NoHead:                           ; done

    ld a, [wDirtyFlags]            ; flags
    bit DIRTY_FOOD, a              ; food?
    jr z, .NoFood                  ; skip
    ld a, [wFoodY]                 ; B=foodY
    ld b, a                        ; B
    ld a, [wFoodX]                 ; C=foodX
    ld c, a                        ; C
    ld a, TILE_FOOD                ; tile
    call SetBGTile                 ; draw food
.NoFood:                           ; done

    xor a                          ; A=0
    ld [wDirtyFlags], a            ; clear flags
    ret                            ; return

; -----------------------------------------------------------------------------
; Pause overlay (VBlank-only): draw/clear "- PAUSED -"
; -----------------------------------------------------------------------------

PauseOverlayInVBlank:              ; update pause overlay
    ld a, [wState]                 ; state
    cp STATE_PLAY                  ; only play
    jr z, .InPlay                  ; ok
    xor a                          ; A=0
    ld [wPauseDirty], a            ; clear pending
    ret                            ; return
.InPlay:                           ; in play
    ld a, [wPauseDirty]            ; pending?
    or a                           ; any?
    ret z                          ; no
    ld a, [wPaused]                ; paused?
    or a                           ; flags
    jr z, .Clear                   ; if not, clear text
    ld b, 9                        ; y
    ld c, 5                        ; x
    ld de, StrPaused               ; "- PAUSED -"
    call BlitTextASCII             ; draw
    jr .Done                       ; done
.Clear:                            ; erase
    ld b, 9                        ; y
    ld c, 5                        ; x
    ld de, StrSpaces10             ; spaces
    call BlitTextASCII             ; clear
.Done:                             ; finish
    xor a                          ; A=0
    ld [wPauseDirty], a            ; clear pending
    ret                            ; return

; -----------------------------------------------------------------------------
; Direction update (held d-pad), disallow reverse
; -----------------------------------------------------------------------------

UpdateDirectionFromHeld:           ; update direction
    ld a, [wJoyCur]                ; held
    bit JOY_UP, a                  ; up?
    jr z, .ChkR                    ; no
    ld b, DIR_UP                   ; cand
    jr .Try                        ; try
.ChkR:                             ; right?
    bit JOY_RIGHT, a               ; right?
    jr z, .ChkD                    ; no
    ld b, DIR_RIGHT                ; cand
    jr .Try                        ; try
.ChkD:                             ; down?
    bit JOY_DOWN, a                ; down?
    jr z, .ChkL                    ; no
    ld b, DIR_DOWN                 ; cand
    jr .Try                        ; try
.ChkL:                             ; left?
    bit JOY_LEFT, a                ; left?
    ret z                          ; none
    ld b, DIR_LEFT                 ; cand
.Try:                              ; apply if not reverse
    ld a, [wDir]                   ; current
    cp b                           ; same?
    ret z                          ; no change
    xor 2                          ; opposite
    cp b                           ; candidate==opposite?
    ret z                          ; ignore reverse
    ld a, b                        ; new dir
    ld [wDir], a                   ; store
    ld a, 1                        ; latch change this step
    ld [wDirChanged], a            ; store
    ret                            ; return

; -----------------------------------------------------------------------------
; Joypad read (held + edges)
; -----------------------------------------------------------------------------

ReadJoypad:                        ; read joypad
    ld a, $20                      ; select d-pad
    ld [rP1], a                    ; write
    ld a, [rP1]                    ; dummy
    ld a, [rP1]                    ; read
    and $0F                        ; low nibble
    ld b, a                        ; B=dpad

    ld a, $10                      ; select buttons
    ld [rP1], a                    ; write
    ld a, [rP1]                    ; dummy
    ld a, [rP1]                    ; read
    and $0F                        ; low nibble
    swap a                         ; to high nibble
    or b                           ; combine
    cpl                            ; active-low -> pressed=1
    ld [wJoyCur], a                ; store held

    ld a, $30                      ; deselect
    ld [rP1], a                    ; write

    ld a, [wJoyPrev]               ; prev
    cpl                            ; ~prev
    ld b, a                        ; B=~prev
    ld a, [wJoyCur]                ; cur
    and b                          ; edges
    ld [wJoyPressed], a            ; store edges

    ld a, [wJoyCur]                ; cur
    ld [wJoyPrev], a               ; prev=cur
    ret                            ; return

; -----------------------------------------------------------------------------
; VBlank waits (LCD-off safe)
; -----------------------------------------------------------------------------

WaitVBlankStart:                   ; wait LY>=144
    ld a, [rLCDC]                  ; LCDC
    bit 7, a                       ; LCD on?
    ret z                          ; if off, don't wait
.vs:                               ; loop
    ld a, [rLY]                    ; LY
    cp 144                         ; VBlank?
    jr c, .vs                      ; wait
    ret                            ; return

WaitVBlankEnd:                     ; wait LY<144
    ld a, [rLCDC]                  ; LCDC
    bit 7, a                       ; LCD on?
    ret z                          ; if off, don't wait
.ve:                               ; loop
    ld a, [rLY]                    ; LY
    cp 144                         ; still VBlank?
    jr nc, .ve                     ; wait
    ret                            ; return

; -----------------------------------------------------------------------------
; Occupancy grid helpers: index = y*20 + x (0..359)
; -----------------------------------------------------------------------------

ClearOcc:                          ; clear wOcc[360] to 0 (FIXED)
    ld hl, wOcc                    ; HL = base
    ld bc, PLAY_W*PLAY_H           ; BC = 360
    xor a                          ; A = 0
    ld d, a                        ; D = 0 constant
.Lc:                               ; loop
    ld a, d                        ; A = 0
    ld [hli], a                    ; write 0
    dec bc                         ; BC--
    ld a, b                        ; test BC
    or c                           ; test BC
    jr nz, .Lc                     ; loop
    ret                            ; return

FieldIndex:                        ; B=y, C=x -> HL = &wOcc[y*20+x]
    ld h, 0                        ; H=0
    ld l, b                        ; L=y
    add hl, hl                     ; y*2
    add hl, hl                     ; y*4
    ld d, h                        ; D=hi(y*4)
    ld e, l                        ; E=lo(y*4)
    add hl, hl                     ; y*8
    add hl, hl                     ; y*16
    add hl, de                     ; y*20
    ld a, l                        ; A=low
    add a, c                       ; +x
    ld l, a                        ; store
    jr nc, .NC                     ; carry?
    inc h                          ; add carry
.NC:                               ; no carry
    ld de, wOcc                    ; DE=base
    add hl, de                     ; HL+=base
    ret                            ; return

GetOcc:                            ; B=y, C=x -> A=occ
    call FieldIndex                ; HL=&occ
    ld a, [hl]                     ; A=occ
    ret                            ; return

SetOcc:                            ; A=occ, B=y, C=x
    push af                        ; save occ
    call FieldIndex                ; HL=&occ
    pop af                         ; restore occ
    ld [hl], a                     ; write
    ret                            ; return

; -----------------------------------------------------------------------------
; Food spawn: pick random inner cell that is OCC_EMPTY
; -----------------------------------------------------------------------------

SpawnFoodNoDraw:                   ; choose empty cell for food
.Try:                              ; retry
    call Random8                   ; rnd
    and $1F                        ; 0..31
    cp X_MAX                       ; >=18?
    jr nc, .Try                    ; retry
    inc a                          ; 1..18
    ld [wFoodX], a                 ; store X
    call Random8                   ; rnd
    and $1F                        ; 0..31
    cp Y_MAX                       ; >=16?
    jr nc, .Try                    ; retry
    inc a                          ; 1..16
    ld [wFoodY], a                 ; store Y
    ld a, [wFoodY]                 ; B=y
    ld b, a                        ; B
    ld a, [wFoodX]                 ; C=x
    ld c, a                        ; C
    call GetOcc                    ; A=occ
    cp OCC_EMPTY                   ; empty?
    jr nz, .Try                    ; no => retry
    ret                            ; yes => done

Random8:                           ; tiny PRNG (state + DIV)
    ld a, [wRand]                  ; state
    ld b, a                        ; copy
    ld a, b                        ; A=state
    and 1                          ; bit0
    ld c, a                        ; C=bit0
    ld a, b                        ; A=state
    srl a                          ; >>1
    and 1                          ; bit1
    xor c                          ; feedback
    ld c, a                        ; C=feedback
    ld a, b                        ; A=state
    srl a                          ; >>1
    ld b, a                        ; B=shifted
    ld a, c                        ; feedback?
    or a                           ; flags
    jr z, .NoSet                   ; if 0 skip
    set 7, b                       ; set MSB
.NoSet:                            ; done
    ld a, b                        ; new state
    ld [wRand], a                  ; store
    ld b, a                        ; B=state
    ld a, [rDIV]                   ; DIV
    xor b                          ; mix
    ret                            ; return A

; -----------------------------------------------------------------------------
; Text blitter (ASCII 0x20..0x7F), caller must be LCD off or VBlank
; -----------------------------------------------------------------------------

BlitTextASCII:                     ; B=y, C=x, DE=0-terminated ASCII
    push de                        ; save DE
    call CalcBGAddr                ; HL = BG dest
    pop de                         ; restore DE
.Loop:                             ; loop chars
    ld a, [de]                     ; char
    inc de                         ; advance
    or a                           ; NUL?
    ret z                          ; done
    cp " "                         ; below space?
    jr c, .Space                   ; map to space
    cp $80                         ; beyond 0x7F?
    jr nc, .Space                  ; map to space
    sub " "                        ; ASCII -> tile index
    ld [hli], a                    ; write
    jr .Loop                       ; next
.Space:                            ; space/unsupported
    xor a                          ; TILE_EMPTY (space)
    ld [hli], a                    ; write
    jr .Loop                       ; next

; -----------------------------------------------------------------------------
; BG helpers
; -----------------------------------------------------------------------------

ClearBGMap:                        ; clear 32x32 BG map
    ld hl, _BGMap0                 ; base
    ld bc, 32*32                   ; 1024
    ld d, TILE_EMPTY               ; constant
.Lb:                               ; loop
    ld a, d                        ; empty
    ld [hli], a                    ; write
    dec bc                         ; --
    ld a, b                        ; test
    or c                           ; test
    jr nz, .Lb                     ; loop
    ret                            ; return

DrawBorder:                        ; 20x18 border walls
    ld c, 0                        ; x
.TopBot:                           ; top/bottom
    ld b, 0                        ; y=0
    ld a, TILE_WALL                ; wall
    call SetBGTile                 ; set
    ld b, (PLAY_H-1)               ; y=17
    ld a, TILE_WALL                ; wall
    call SetBGTile                 ; set
    inc c                          ; x++
    ld a, c                        ; x
    cp PLAY_W                      ; x<20?
    jr c, .TopBot                  ; loop

    ld b, 0                        ; y
.Sides:                            ; sides
    ld c, 0                        ; x=0
    ld a, TILE_WALL                ; wall
    call SetBGTile                 ; set
    ld c, (PLAY_W-1)               ; x=19
    ld a, TILE_WALL                ; wall
    call SetBGTile                 ; set
    inc b                          ; y++
    ld a, b                        ; y
    cp PLAY_H                      ; y<18?
    jr c, .Sides                   ; loop
    ret                            ; return

SetBGTile:                         ; A=tile, B=y, C=x
    push af                        ; save tile
    call CalcBGAddr                ; HL=addr
    pop af                         ; restore tile
    ld [hl], a                     ; write
    ret                            ; return

CalcBGAddr:                        ; HL=_BGMap0 + y*32 + x
    ld h, 0                        ; H=0
    ld l, b                        ; L=y
    add hl, hl                     ; y*2
    add hl, hl                     ; y*4
    add hl, hl                     ; y*8
    add hl, hl                     ; y*16
    add hl, hl                     ; y*32
    ld a, l                        ; low
    add a, c                       ; +x
    ld l, a                        ; store
    jr nc, .NC2                    ; carry?
    inc h                          ; add carry
.NC2:                              ; no carry
    ld de, _BGMap0                 ; base
    add hl, de                     ; add base
    ret                            ; return

Memcpy:                            ; HL->DE, BC bytes
    ld a, b                        ; test
    or c                           ; test
    ret z                          ; done
.M:                                ; loop
    ld a, [hli]                    ; read
    ld [de], a                     ; write
    inc de                         ; ++
    dec bc                         ; --
    ld a, b                        ; test
    or c                           ; test
    jr nz, .M                      ; loop
    ret                            ; return

; -----------------------------------------------------------------------------
; ROM strings + font map
; -----------------------------------------------------------------------------

SECTION "ROData", ROM0             ; Read only data section
INCLUDE "db/build.inc"             ; Build info    
INCLUDE "db/game.inc"              ; Game constants
INCLUDE "db/tiles.inc"             ; Tile definitions

; -----------------------------------------------------------------------------
; WRAM
; -----------------------------------------------------------------------------

SECTION "WRAM", WRAM0              ; Work RAM section

wState:         ds 1               ; current state
wNextState:     ds 1               ; queued state
wNeedRebuild:   ds 1               ; rebuild flag

wJoyCur:        ds 1               ; held buttons
wJoyPrev:       ds 1               ; prev held
wJoyPressed:    ds 1               ; edge presses

wFrame:         ds 1               ; title blink frame
wBlink:         ds 1               ; title blink toggle

wMoveCounter:   ds 1               ; move pacing
wPaused:        ds 1               ; pause flag
wPauseDirty:    ds 1               ; pause overlay dirty
wIgnoreStart:   ds 1               ; ignore first start edge in play
wDirChanged:    ds 1               ; direction changed this step
wDir:           ds 1               ; direction
wSnakeLen:      ds 1               ; length

wHeadIdx:       ds 1               ; ring head index
wTailIdx:       ds 1               ; ring tail index

wNewX:          ds 1               ; temp newX
wNewY:          ds 1               ; temp newY
wGrow:          ds 1               ; grow flag

wHeadX:         ds 1               ; dirty head X
wHeadY:         ds 1               ; dirty head Y
wTailX:         ds 1               ; dirty tail X (also tail compare)
wTailY:         ds 1               ; dirty tail Y (also tail compare)
wFoodX:         ds 1               ; food X
wFoodY:         ds 1               ; food Y
wDirtyFlags:    ds 1               ; dirty flags

wRand:          ds 1               ; RNG state

wSnakeX:        ds MAX_SNAKE       ; snake X ring
wSnakeY:        ds MAX_SNAKE       ; snake Y ring

wOcc:           ds PLAY_W*PLAY_H   ; 20*18 occupancy grid (360 bytes)
