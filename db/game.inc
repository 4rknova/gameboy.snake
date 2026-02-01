StrTitle:       db "SNAKE",0       ; title
StrPressStart:  db "PRESS START",0 ; prompt
StrGameOver:    db "GAME OVER",0   ; game over
StrPaused:      db "- PAUSED -",0  ; pause overlay

; 11 spaces.
; Used by the title-screen blink code to “erase”
; the PRESS START text when the blink toggles off.
StrSpaces11:
REPT 11
    db " "
ENDR
    db 0

; 10 spaces. Used to clear the PAUSED overlay.
StrSpaces10:
REPT 10
    db " "
ENDR
    db 0
