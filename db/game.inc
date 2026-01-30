StrTitle:       db "SNAKE",0       ; title
StrPressStart:  db "PRESS START",0 ; prompt
StrGameOver:    db "GAME OVER",0   ; game over

; 11 spaces.
; Used by the title-screen blink code to “erase”
; the PRESS START text when the blink toggles off.
StrSpaces11:
REPT 11
    db " "
ENDR
    db 0