INCLUDE j2.inc           
        .MODEL COMPACT
        .STACK 64
        .Data 
        
        ;-------------------phase2--------------
playername DB 33D, ?, 33 DUP (?) 
playertype     DB 0
playernum db 0
send db 0         ;0=>INITIAL STATE,1=>game invitation sent,2=>chat invitation sent
rec  db 0         ;0=>INITIAL STATE,1=>game invitation rec ,2=>chat invitation rec
isexit db 0
ispause db 0 
DUMMYY DB "$$THIS IS JUST A USELESS ARRAY OF STRINGS AND EXISTS ONLY TO STOP THE STUPID DOS BOX FROM RUINING MY STRINGS               ",'$'
 
waitingplayer db "GAME REQUEST SENT,WAITING FOR OTHER PLAYER RESPONSE$" 
waitingplayer2 db "CHAT REQUEST SENT,WAITING FOR OTHER PLAYER RESPONSE$" 
gameinv       db "GAME INVIITATION RECIEVED PRESS 1 TO ACCEPT$"
chatinv       db "CHAT INVIITATION RECIEVED PRESS 2 TO ACCEPT$" 

         VALUE DB ? 
         VALUESCAN DB ?
         
         CHAT1_POS DW 0000H             ;PAR:  PLAYER 1 START POSITION
         CHAT2_POS DW 0C00H             ;PAR:  PLAYER 2 START POSITION
        
         SPLITSCREEN EQU 0BH            ;CONSTANT: X POSITION OF SPLIT SCREEN 
         ;START_HEIGHT DB 00H            ;PAR: Y POSTION OF CHAT 
         
         IN_CHAT1_POS DW 1100H             ;PAR:  PLAYER 1 START POSITION
         IN_CHAT2_POS DW 1200H             ;PAR:  PLAYER 2 START POSITION
level_msg DB "CHOOSE LEVEL 1 OR 2  :",'$'
level_msg2 DB "WAITING FOR OTHER PLAYER TO CHOOSE GAME LEVEL",'$'
LEVEL_VAR DB 1      
typeee db 0
               
;----------------------------------------------------------         
          
                
JSTARTSCREEN1 DB "TO START GAME PRESS 1",'$'
JSTARTSCREEN2 DB "TO CHAT PRESS 2      ",'$'
JSTARTSCREEN3 DB "TO EXIT GAME PRESS 3 ",'$'
WELCOME_MSG   DB "WELCOME TO BATTLE FIELD FOR POTATOES ",'$'      
INGAME_MSG     DB "INLINE CHAT -> ANY KEY",09,09,09,"MAIN MENU -> ESC",'$'

ENTER_NAME_MSG DB "ENTER YOUR NAME THEN PRESS ENTER",'$'
CHOOSE_TAYP_MSG DB "ENTER THE WANTED SHAPE NUMBER AND DONOT DONOT PRESS ENTER",'$'




CHAT_MSG      DB "THIS MODE IS ISN'T AVILABLE YET",'$'
CHOSEN_OPTION DB ?                  ;IF 1 START GAME 2 CHAT 3 EXIT GAME 

JPLAYER1NAME  DB 33D, ?, 33 DUP (?)
JPLAYER2NAME  DB 33D, ?, 33 DUP (?) 

JPNAME1       DB "ENTER YOUR NAME : ",'$'
JPTYPE        DB " CHOSEE TYPE NO. : ", '$' 
JP1T          DB 0                  ;PLAYER 1 CHOOSEN TYPE AND TYPE 0 IS DEFAULT AND SAME FOR P2
JP2T          DB 0 
j2count dw 0                        ;NOTE USED IN MACRO PRINTSTATRTWORD
position dw 0000                    ;NOTE USED IN MACRO PRINTSTATRTWORD



READY_WORD label byte
db "  rrrrrrrrrrr        rrrrrrr    rrrrrrrr  rrrrrr   r       r  ",'$'
db "  r         r        r          r      r  r     r   r     r   ",'$'
db "  r         r        r          r      r  r      r   r   r    ",'$'
db "  r         r        r          r      r  r      r    r r     ",'$'
db "  rrrrrrrrrrr        r          rrrrrrrr  r      r     r      ",'$'
db "  r        r         rrrrr      r      r  r      r     r      ",'$'
db "  r         r        r          r      r  r      r     r      ",'$'
db "  r          r       r          r      r  r      r     r      ",'$'
db "  r           r      r          r      r  r      r     r      ",'$'
db "  r            r     r          r      r  r     r      r      ",'$'
db "  r             r    rrrrrrr    r      r  rrrrrr       r      ",'$' 


STEADY_WORD label byte
db "ssssss     sssssssss   sssssss    ssssssss  ssssss   s       s",'$'
db "s              s       s          s      s  s     s   s     s ",'$'
db "s              s       s          s      s  s      s   s   s  ",'$'
db "s              s       s          s      s  s      s    s s   ",'$'
db "s              s       s          ssssssss  s      s     s    ",'$'
db "ssssss         s       sssss      s      s  s      s     s    ",'$'
db "     s         s       s          s      s  s      s     s    ",'$'
db "     s         s       s          s      s  s      s     s    ",'$'
db "     s         s       s          s      s  s      s     s    ",'$'
db "     s         s       s          s      s  s     s      s    ",'$'
db "ssssss         s       sssssss    s      s  ssssss       s    ",'$'

GO_WORD label byte
db "   gggggggggggggggggggg             gggggggggggggggggggg      ",'$'
db "   g                   g            g                  g      ",'$'
db "   g                    g           g                  g      ",'$'
db "   g                                g                  g      ",'$'
db "   g                                g                  g      ",'$'
db "   g              ggggggggggggg     g                  g      ",'$'
db "   g                    g           g                  g      ",'$'
db "   g                    g           g                  g      ",'$'
db "   g                    g           g                  g      ",'$'
db "   g                    g           g                  g      ",'$'
db "   gggggggggggggggggggggg           gggggggggggggggggggg      ",'$'


normal_person_shape label byte
db "  (O)  ",'$'
db "  \|/  ",'$'
db "   |   ",'$'
db "  / \  ",'$'
db "       ",'$'
DB "TYPE  0",'$'
 

Tank label byte 
db " ",0dch,"   ",0dch," ",'$'  
db " ",0deh,94h,0c4h,94h,0ddh," ",'$' 
db " ",0dbh,0b2h,0b2h,0b2h,0dbh," ",'$'
db " ",0dbh,0dbh,0dbh,0dbh,0dbh," ",'$'
db "       ",'$'
DB "TYPE  1",'$' 
   

Robot label byte
db "  ,,,  ",'$'
db " (o o) ",'$'
db " /. .\ ",'$'
db "(__|__)",'$'
db "       ",'$'
DB "TYPE  2",'$'     

JPOSITION DW 0 
PO DW 0000H  
JCOUNT     DB 0 

HEALTH_MSG DB 'HEALTH : ','$'
HEALTH1 DB 7                      ;PLAYER1 HEALTH
HEALTH2 DB 7                      ;PLAYER2 HEALTH     

PLAYER1_BX DB 7 DUP (?)           ;XPOS ARRAY FOR PLAYER 1
PLAYER1_BY DB 7 DUP (?)           ;YPOS ARRAY FOR PLAYER1
PLAYER1_BF DB 7 DUP (0)           ;FLAGS ARRAY FOR PLAYER 1(INDICATING IF THE BULLETS IS ACTIVE OR NOT)

PLAYER2_BX DB 7 DUP (?)           ;SAME ARRAYS AS PLAYER 1
PLAYER2_BY DB 7 DUP (?)
PLAYER2_BF DB 7 DUP (0) 

BULLETSPEED DB 2D                 ;BULLET SPEED


PLAYER1WINSWORD label byte
db "   PPPP   P           P     P     P  PPPPP   PPPP      P      ",'$'
db "   P   P  P          P P     P   P   P       P   P     P      ",'$'
db "   P   P  P         P   P     P P    P       P   P     P      ",'$'
db "   PPPP   P        PPPPPPP     P     PPPPP   PPPP      P      ",'$'
db "   P      P       P       P    P     P       P   P     P      ",'$'
db "   P      PPPPP  P         P   P     PPPPP   P    P    P      ",'$'
db "                                                              ",'$'
db "                                                              ",'$'
db "      W           W           W  W   W     W   WWWWW          ",'$'
db "       W         W W         W   W   WW    W  W               ",'$'
db "        W       W   W       W    W   W W   W  W               ",'$'
db "         W     W     W     W     W   W  W  W   WWWWW          ",'$'
db "          W   W       W   W      W   W   W W        W         ",'$'
db "           W W         W W       W   W    WW        W         ",'$'
db "            W           W        W   W     W   WWWWW          ",'$'
                                                                 

PLAYER2WINSWORD label byte

db "   PPPP   P           P     P     P  PPPPP   PPPP     PP      ",'$'
db "   P   P  P          P P     P   P   P       P   P   P  P     ",'$'
db "   P   P  P         P   P     P P    P       P   P      P     ",'$'
db "   PPPP   P        PPPPPPP     P     PPPPP   PPPP      P      ",'$'
db "   P      P       P       P    P     P       P   P    P       ",'$'
db "   P      PPPPP  P         P   P     PPPPP   P    P  PPPP     ",'$'
db "                                                              ",'$'
db "                                                              ",'$'
db "      W           W           W  W   W     W   WWWWW          ",'$'
db "       W         W W         W   W   WW    W  W               ",'$'
db "        W       W   W       W    W   W W   W  W               ",'$'
db "         W     W     W     W     W   W  W  W   WWWWW          ",'$'
db "          W   W       W   W      W   W   W W        W         ",'$'
db "           W W         W W       W   W    WW        W         ",'$'
db "            W           W        W   W     W   WWWWW          ",'$'
     

BLOCK_POS DW 0636D
 
BLOCK LABEL BYTE 
DB  0DBH,0CBH,0CBH,0CBH,0CBH,0CBH,0CBH,0DBH,'$'
DB  0DBH,0DBH,0DBH,0DBH,0DBH,0DBH,0DBH,0DBH,'$'
DB  0DBH,0DBH,' ',' ',' ',' ',0DBH,0DBH,'$'
DB  0DBH,0DBH,' ',' ',' ',' ',0DBH,0DBH,'$'
DB  0DBH,0DBH,0DBH,0DBH,0DBH,0DBH,0DBH,0DBH,'$'
DB  0DBH,0CAH,0CAH,0CAH,0CAH,0CAH,0CAH,0DBH,'$'

LEVEL DB 2
UPFLAG DB 0  
BLOCKSPEED DB 1

 
        
;needed 
time dw 10000d                 ;10000 =>2710h
speed db 1                     ;PLAYERS SPEED
count db 0 
start_pos db 00h
end_pos db 10h       

type_player1 db '$$' 
type_player2 db '$$'

player1_pos dw 0900h
player2_pos dw 0948h 

start_drawing_screen db 0h      ;USED FOR RESIZING THE DRAWING AREA HEIGHT
end_drawing_screen db 0Ch

;----------------------------------------------
           .CODE
MAIN       PROC FAR
           MOV AX,@DATA
           MOV DS,AX 
           
           mov dx,3fbh 			; Line Control Register
           mov al,10000000b		;Set Divisor Latch Access Bit
           out dx,al
           
           mov dx,3f8h			
           mov al,0ch			
           out dx,al
           
           mov dx,3f9h
           mov al,00h
           out dx,al 
           
           mov dx,3fbh
           mov al,00011011b
           out dx,al

             ;SCROLL FULL SCREEN TO REMOVE WHAT IS WRITTEN INITIALLY IN DOS BOX
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h            
         
         GETPLAYERNAME JPNAME1, JPLAYER1NAME       ;GET PLAYERS NAMES
           
         GETUSERTYPE  playername, JPTYPE, typeee   ;GET TYPES


    ;THIS LOOP IS RESPONSIBLE FOR ALL MODES IN THE APPLICATION AND ANY LOGIC   

    START_SCREEN_LOOP:  
    ;INITIALIZING GAME MAIN CONTROLLERS 
    mov playernum,0
    MOV SEND,0
    MOV REC,0
    MOV IN_CHAT1_POS, 1100H     
    MOV IN_CHAT2_POS, 1200H             
    mov BLOCK_POS, 0623H           
    mov isexit,0  
    mov ispause,0
           
         STARTSCREEN JSTARTSCREEN1,JSTARTSCREEN2,JSTARTSCREEN3     ;PRINT START MENU AND GET RESPONSE
           
         CMP CHOSEN_OPTION,1
         jnz checkOption2    ;IF 1 START GAME
         ;sending information to the other player and rec other player information     
         cmp playernum,1
         jnz player2 
         CALL GET_LEVEL
         SEND_INV 
         jmp GAME_MAIN_LOOP 
         player2:
         CALL WAIT_LEVEL
         ACC_INV
         
          
         Jmp  GAME_MAIN_LOOP 
         checkOption2:  
         CMP CHOSEN_OPTION,2    ;IF 2 CHAT MOED 
         jnz checkoption3
         cmp playernum,1
         jnz player22
         SEND_INV
         jmp  CHAT_MODE 
         player22:
         ACC_INV 
         ;============
         jmp  CHAT_MODE 
                         
         checkoption3:                
         CMP CHOSEN_OPTION,3    ;IF 3 EXIT GAME
         JZ EXITGAME
           
                         
   
    GAME_MAIN_LOOP:
           
         
           
         READY_STEADY_GO     ;READY STEADY GO (PREAPARING THE PLAYERS FOR THE GAME)
          
         CALL UPDATEHEALTHBAR   ;DRAW THE STATUS BAR

        
        
        mov ah,3h
        mov bh,0h
        int 10h  
          
;---------------------------

        MOV AL,JP1T  
        mov type_player1[0],AL     ;DETERMINE PLAYER1 TYPE
                                                                                                    
;===============
        
        MOV AL,JP2T    
        mov type_player2[0],AL     ;DETERMINE PLAYER2 TYPE
    
    ;IN THIS LOOP THE LOGIC OF THE GAME HAPPENS            
    Mainloop:      
    
             
          MOV AH,2   
          MOV BH,0                 ;CHANGE CURSOR LOCATION  
          MOV DX,0
          MOV DH,end_drawing_screen
          ADD DH,4H
          INT 10H
    
          MOV AH,9                    ;DRAW LINE
          MOV BH,0
          MOV AL,95
          MOV CX,80
          MOV BL,01AH
          INT 10H
         
         MOV ISPAUSE,0
         
        STARTGAMEPLZ:
    
    
                        ;make blue background with WHITE FOREGROUNF printed
        MOV AH, 06h    ; Scroll up function
        XOR AL, AL     ; Clear entire screen
        XOR CX, CX     ; Upper left corner CH=row, CL=column 
        MOV DH,0FH 
        MOV DL,4FH     ; lower right corner DH=row, DL=column
                        
        MOV BH, 01Fh    ; WHITEOnBlue
        INT 10H                                    
        
        mov ah,3h
        mov bh,0h
        int 10h      
        
        
        MOV AL,LEVEL_VAR
        CMP AL,1
        JZ DONOTMOVEBLOCK
                         
        CALL MOVEBLOCK   
        
        DONOTMOVEBLOCK:
        CALL DRAWBULLET     ;DRAW BULLETS SAVED IN BOTH  ARRAYS
           
        CALL MOVEBULLET     ;INC X WITH THE SPEED GIVEN FOR EVERY BULLET(SPEED IS CONSTANT OVER ALL BULLETS) 
           
        CALL BULLET1STATUS  ;TO CHECK IF THE BULLET REACHED THE OTHER PLAYER IF YES (DECREMENT HEALTH) MAKE THE BULLET INACTIVE
 
        call KeyPressed     ;CHECK WHICH KEY IS PRESSED AND ACT UPON IT 
        CALL RECGAME
                 
        ;DRAW PLAYERS TYPES IN THEIR POSTION WHICH CHANGES BASED ON PRESSING PREDETERMINED KEYS        
       
        Draw type_player1,player1_pos
        Draw type_player2,player2_pos 
        draw_block  BLOCK,BLOCK_POS
           
        
        MOV CX,0
        MOV DX,8000H           ;DELAY TO BE ABALE TO SEE THE BULLET AND THE MOVMENT(MORE NATURAL)
        MOV AH,86H
        INT 15H
     
     mov ah,2
     mov dx,184Fh
     int 10h

 ;   MOV CX,0
 ;   LOOPDELAY10:
 ;   INC CX
 ;   CMP CX,TIME
 ;   JZ  LOOPDELAY10      
 
    jmp Mainloop           
        
        ;-----------------CALLED IF PLAYER2 HEALTH IS ZERO-----------------------
           
        PLAYER1WIN:        
        ;CLEAR SCREEN
        MOV AX,0600H
        MOV BH,7
        MOV CX,0
        MOV DX,184FH
        INT 10H 
        
        draw_shape_WINNER  PLAYER1WINSWORD  ;DRAW PLAYER1 WINS  
        
        MOV CX,40     ;DELAY TO SEE THE PLAYER1 WINS MESSAGE
        MOV AH,86H
        INT 15H  
        
        ;THEN REINTIALIZE ALL VARIABLES TO PREPARE FOR A NEW GAME 
        CALL NEW_GAME   
        
        ;THEN GO BACK TO START MENU
        JMP START_SCREEN_LOOP
         
        ;--------------------------------------------------------------------------
      
        ;-----------------CALLED IF PLAYER1 HEALTH IS ZERO-------------------------
          
        PLAYER2WIN:       ;SAME AS PLAYER1
        MOV AX,0600H
        MOV BH,7
        MOV CX,0
        MOV DX,184FH
        INT 10H
      
        draw_shape_WINNER  PLAYER2WINSWORD 
      
        MOV CX,40
        MOV AH,86H
        INT 15H
           
        CALL NEW_GAME

        JMP START_SCREEN_LOOP 
         
        ;-----------------------------------------------------------------------

        ;IF CHAT IS CHOOSED THEN CALL THIS PROC TO PRINT MSG THEN RETURN TO MAIN MENU
 
        CHAT_MODE:
        ;CALL CHATAMODE 
        CALL CHATAMODE    
        jmp START_SCREEN_LOOP
        
        ;-------------------------IF THE PLAYERS WANT TO PAUSE THE GAME----------        
   ;    PAUSELOOP: 
   ;   
   ;       Draw type_player1,player1_pos
   ;       Draw type_player2,player2_pos  
   ;       draw_block  BLOCK,BLOCK_POS   
   ;       
   ;       PAUSE55:
   ;       
   ;         mov dx , 3FDH		; Line Status Register
   ;         in al , dx 
   ;        AND al , 1
   ;        JZ CHECK  
   ;
   ;         mov dx , 03F8H
   ;         in al , dx 
   ;         mov BL , al 
   ;         
   ;         CMP BL,5
   ;         JZ Mainloop  
   ;     
   ;       CHECK:
   ;       mov ah,1     ;Check if there characters inside the keyboard buffer 
   ;       int 16h 
   ;       jz EXITPAUSE           ;if no, repeat
   ;                          ;if yes,read char and check if its p or P TO RETURN TO THE MAINLOOP      
   ;       mov ah,0
   ;       int 16h            
   ;
   ;       CMP AH,03CH
   ;       JZ PAUSEPRESSED
   ;
   ;
   ;       JMP EXITPAUSE
   ;       
   ;       PAUSEPRESSED:
   ;       
   ;    mov dx , 3FDH		; Line Status Register
   ;    AGAINPAUSE:  
   ;    In al , dx 			;Read Line Status
  ;	AND al , 00100000b
  ;	JZ AGAINPAUSE 
  ;	
  ;	mov dx , 3F8H		; Transmit data register
  ;	mov  al,5
  ;	out dx , al 
   ;       
   ;       
   ;       JMP Mainloop 
   ;         
   ;       EXITPAUSE:
   ;    JMP PAUSE55 ;--------------------IN ANY POINT WE WANT TO EXIT GAME WE JMP HERE-------------  
           
        EXITGAME:     
          ;CLEAR SCREEN BEFORE EXITING
          mov ah,6 ; function 6
          mov al,24 ; scroll by 1 line
          mov bh,7 ; normal video attribute
          mov ch,0 ; upper left Y
          mov cl,0 ; upper left X
          mov dh,24 ; lower right Y
          mov dl,79 ; lower right X
          int 10h        
          RET        
          
           HLT
MAIN       ENDP
;------------------------------------ 
;==========PROCEDURES================
;------------------------------------ 


KeyPressed proc   
   
    mov ah,1        ;Check if there characters inside the keyboard buffer 
    int 16h 
    jz senddummy ;if no, key pressed end procedure
    mov ah,0
    int 16h         ;if yes,check which key and call the function responsible for such key 
;-----------------------------------   
   
    cmp ah,72       ;scan code of key up direction and moving player1 up
    jz up
    cmp ah,80       ;scan code of key down direction and moving player1 down
    jz down
    
    CMP AL,2BH
    JZ FIRE1
    
    
    CMP AH,4BH     ;SCAN LEFT ARROW AND SHOOT BULLET FOR PLAYER2
    JZ  senddummy   
     
    CMP AH,04DH
    JZ senddummy  
    
 ;   CMP AH,03CH     ;SCAN FOR PAUSE AND PAUSE THE GAME
 ;   JZ PAUSE 
    
    CMP AH,01     ;SCAN FOR ESC KEY SCAN CODE
    JZ RETURN_MAIN
    
    MOV VALUE,AL
    MOV VALUESCAN,AH
    CALL INLINECHAT
;----------------if any other key of the controllers dont do anything ------------------    
    jmp NOKEYPRESSED

;------------------------------------------------------------------
;----Functions that will be excuted according to the key pressed---    
    
    up:
    CMP playernum,1
    jnz jmp_player2
    send_byte 0
    mov al,'1'
    Move               ;Move player 1 up
    jmp NOKEYPRESSED 
    jmp_player2:
    send_byte 0      
    mov al,'3'
    Move              ;Move player 1 up
    jmp NOKEYPRESSED 
    
    down:
    cmp playernum,1
    jnz jmp1_player2
    send_byte 1
    mov al,'2' 
    Move              ;Move player 1 down
    jmp NOKEYPRESSED 
    jmp1_player2:
    send_byte 1
    mov al,'4' 
    Move              ;Move player 1 down
    jmp NOKEYPRESSED

    
    FIRE1:            ;SHOOT BULLEET FOR P1  
    cmp playernum,1
    jnz jmp2_player2
    send_byte 2    
    CALL P1FIRE
    JMP  NOKEYPRESSED 
    jmp2_player2:
    send_byte 2    
    CALL P2FIRE
    JMP  NOKEYPRESSED                  
                      
            
                      
  ;  PAUSE:            ;PAUSE
    
        
  ;  send_byte 3 
  ;  mov ispause,1
  ;  JMP  NOKEYPRESSED 
      
    
    RETURN_MAIN:       ;EXIT THE CURRENT GAME AND RETURN TO MAIN MENU
    send_byte 5
    mov isexit,1
    ;CALL NEW_GAME
    ;JMP  START_SCREEN_LOOP 
    JMP  NOKEYPRESSED 
    
    senddummy:
    send_byte 4
                                            
    NOKEYPRESSED:     ;READ ANOTHER KEY 
    
     ret
endp KeyPressed

INLINECHAT PROC NEAR
PUSHA   
          
         MOV DX,IN_CHAT1_POS
         MOV AH,2H
         INT 10H
          
          CMP IN_CHAT1_POS,1100H
          JZ INIT1
          
         CMP VALUESCAN,1CH
         JZ IN_ENTER1
         CMP VALUE,8H   
         JZ IN_BACKSPACE1
          
         
         JMP CONTINCHAT
         INIT1:
         ;PLLAYER1
         MOV DX,IN_CHAT1_POS
         MOV AH,2H
         INT 10H
         
         MOV BX,2
         mov ah, 9
         LEA dx,JPLAYER1NAME[BX]
         int 21h
         
         mov ah,3h
         mov bh,0h
         int 10h  
       
         MOV BX,1
         ADD DL, JPLAYER1NAME[BX]
         MOV IN_CHAT1_POS,DX 
         MOV AH,2H
         INT 10H  
          
         mov ah,2
         mov dl,':'
         int 21h
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV IN_CHAT1_POS,DX
         
         JMP CONTINCHAT
         
         CONTINCHAT:
         MOV AH,2H
  		 MOV DL,VALUE
  		 INT 21H
  		 
  		 SENDBYTE VALUE  
  		   		 
  		 mov ah,3h
         mov bh,0h
         int 10h  
         MOV IN_CHAT1_POS,DX
         
         CMP IN_CHAT1_POS,114FH
         JNZ EXITINCHAT1
         
         IN_ENTER1:
         SENDBYTE 1CH   
         JMP IN_SCROLL1   
         
         JMP EXITINCHAT1
         
         IN_SCROLL1:
          MOV IN_CHAT1_POS,1100H
            
          MOV AH,2   
          MOV BH,0                    ;CHANGE CURSOR LOCATION  
          MOV DX,IN_CHAT1_POS
          INT 10H
    
          MOV AH,9                    ;DRAW LINE
          MOV BH,0
          MOV AL,32
          MOV CX,80
          MOV BL,0FH
          INT 10H
          
         JMP EXITINCHAT1 
          
         IN_BACKSPACE1:   
         SENDBYTE 08H
  		 
  		 mov ah,3h
         mov bh,0h
         int 10h  
 
  		 CMP DL,0
  		 JZ EXITINCHAT1 		 
  		  
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H
  		  
  		 MOV AH,2H
  		 MOV DL,20H
  		 INT 21H
  		 
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H
  		 
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV IN_CHAT1_POS,DX
          
      EXITINCHAT1:
POPA    
    RET
ENDP INLINECHAT

INLINECHAT_REC PROC
PUSHA    
             MOV DX,IN_CHAT2_POS
         MOV AH,2H
         INT 10H
         
         CMP IN_CHAT2_POS,1200H
         JZ INIT2
          
         CMP VALUE,1CH
         JZ IN_ENTER2
         
         CMP VALUE,8H
         JZ IN_BACKSPACE2
         

         JMP CONTINCHAT2
         INIT2: 
         ;PLAYER2
         MOV DX,IN_CHAT2_POS
         MOV AH,2H
         INT 10H
         
         MOV BX,2
         mov ah, 9
         LEA dx,JPLAYER2NAME[BX]
         int 21h   
         
         mov ah,3h
         mov bh,0h
         int 10h  
       
         MOV BX,1
         ADD DL, JPLAYER2NAME[BX]
         MOV IN_CHAT2_POS,DX 
         MOV AH,2H
         INT 10H  
         
       
         mov ah,2
         mov dl,':'
         int 21h
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV IN_CHAT2_POS,DX
         JMP CONTINCHAT2 
         
      CONTINCHAT2:

         
         MOV AH,2H
  		 MOV DL,VALUE
  		 INT 21H  
  		 
  		 mov ah,3h
         mov bh,0h
         int 10h  
         MOV IN_CHAT2_POS,DX
         
         CMP IN_CHAT2_POS,124FH
         JNZ EXITINCHAT2
                    
         IN_ENTER2:
         IN_SCROLL2:
          MOV IN_CHAT2_POS,1200H
            
          MOV AH,2   
          MOV BH,0                 ;CHANGE CURSOR LOCATION  
          MOV DX,IN_CHAT2_POS
          INT 10H
    
          MOV AH,9                    ;DRAW LINE
          MOV BH,0
          MOV AL,32
          MOV CX,80
          MOV BL,0FH
          INT 10H
          
        JMP EXITCHAT2
        
        IN_BACKSPACE2: 
  		 mov ah,3h
         mov bh,0h
         int 10h  
  		  
  		 CMP DL,0
  		 JZ EXITINCHAT2 
  		 
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H		 
  		   
  		 MOV AH,2H
  		 MOV DL,20H
  		 INT 21H
  		 
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H
  		 
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV IN_CHAT2_POS,DX 
  		 
         EXITINCHAT2:
POPA      
    RET
ENDP INLINECHAT_REC

;------------------------------------------------------------------
;-----------------------------------------------------------------
RECGAME PROC NEAR 
        Draw type_player1,player1_pos
        Draw type_player2,player2_pos 
        draw_block  BLOCK,BLOCK_POS
    
    PUSHA
   
    recv_byte bl
    
    CMP BL,0
    JZ up2
    
    CMP BL,1
    JZ down2
    
    CMP BL,2
    JZ FIRE2 
    
   ; CMP BL,3
   ; JZ PAUSE2
    
    cmp bl,4
    jz itsdummy
    
    cmp bl,5
    jz ng
    
    MOV VALUE,BL
    CALL INLINECHAT_REC 
    JMP CHK
    
    up2:
    CMP playernum,1
    jnz jmp3_player2
    mov al,'3'
    Move               ;Move player 1 up
    jmp CHK 
    jmp3_player2:     
    mov al,'1'
    Move              ;Move player 1 up
    jmp CHK  
    
    down2:
     cmp playernum,1
    jnz jmp4_player2
    mov al,'4' 
    Move              ;Move player 1 down
    jmp CHK 
    jmp4_player2:
    mov al,'2' 
    Move              ;Move player 1 down
    jmp CHK
    
    FIRE2:            ;SHOOT BULLET FOR PLAYER 2
    cmp playernum,1
    jnz jmp6_player2   
    CALL P2FIRE
    JMP  CHK
    jmp6_player2:   
    CALL P1FIRE
    JMP  CHK                  
                      
   
    ;CALL P2FIRE
    ;JMP  CHK
    
   ; PAUSE2:
   ; mov al,0
   ; JMP PAUSELOOP
    
    ng:
    CALL NEW_GAME
    JMP  START_SCREEN_LOOP
    
    itsdummy:
    
 CHK:
     ;------------
     cmp isexit,1
    jz ng 
    
    ;cmp ispause,1
    ;jz PAUSE2  
    ;-----------------------   
 POPA       
 RET
 ENDP RECGAME      
;-------------------------------------------------------------------
;====THIS PROC IS USED TO PRINT HEARTS CORRESPONDING TO THE
;====NUMBER OF HEALTH BARS REMAINING FOR EACH PLAYER
;-------------------------------------------------------------------

UPDATEHEALTHBAR PROC    ;SCROLL DOWN THE OLD HEALTH BAR       
     MOV AL,0
     MOV AH,7
     MOV BH,7
     MOV CX,1300H
     MOV DX,184FH
     INT 10H           
          
     ;CALL MACRO TO RE PRINT THE NAME ANS THE STRUCTURE OF THE STATUS BAR
                 
     PRINTSTATUSBAR  
     ;CALL MACRO TO PRINT NOTIFICATION BAR
     DRAW_NOTIF_BAR INGAME_MSG       
     MOV AH,2
     MOV BH,0                    ;CHANGE CURSOR POSITION   
     MOV DX,150AH                                   
     INT 10H
           
     ;PRINT THE HEALTH BARS OF PLAYER 1 IN THE SHAPE OF RED HEARTS (THE NUMBER DEPENDS ON HEALTH1)
     MOV AH,9
     MOV BH,0
     MOV AL,3
     MOV CX,0
     MOV CL, HEALTH1
     MOV BL,004H
     INT 10H 

     MOV AH,2
     MOV BH,0                    ;CHANGE CURSOR POSITION   
     MOV DX,1548H                                   
     INT 10H
           
     ;SAME AS PLAYER1
     MOV AH,9
     MOV BH,0
     MOV AL,3
     MOV CX,0
     MOV CL, HEALTH2
     MOV BL,004H
     INT 10H 
           
     RET
ENDP UPDATEHEALTHBAR 
;-----------------------------------------------------------------------------
;==THIS PROCEDURE IS USED TO ADD BULLET TO PLAYER 1 ARRAY OF BULLETS         
;-----------------------------------------------------------------------------           
P1FIRE PROC NEAR    
       
     PUSHA

     
     MOV BX,0                    ;USED TO LOOP THROUGH THE ARRAY
       
     FLAGLOOP1:
       MOV CL, PLAYER1_BF[BX]
       CMP CL,0                  ;CHECK IF THERE IS A PLACE FOR A BULLET IN THE ARRAY
       JZ ADDBULLET1             ;IF THERE IS A PLACE ADD BULLET
       INC BX                    ;LOOP ON THE ARRAY TO FIND EMPTY PLACE
       CMP BX,7                  ;LOOP UNTIL THE END OF THE ARRAY
       JNZ FLAGLOOP1
       JMP P1FIRE_EXIT
       
     ADDBULLET1:                  ;IF THERE IS AN EMPTY PLACE IT ADDS BULLETS
       MOV PLAYER1_BF[BX],1                 ;SET THE FLAG AS IT IS NOT EMPTY ANYMORE
       MOV PLAYER1_BX[BX],7                 ;X POSITION OF THE BULLET
       MOV CH,BYTE PTR player1_pos[1]       ;Y OF THE BULLET
       MOV PLAYER1_BY[BX], CH
       
     P1FIRE_EXIT:
       POPA
       
     RET
ENDP P1FIRE
;-----------------------------------------------------------------------------
;==THIS PROCEDURE IS USED TO ADD BULLET TO PLAYER 2 ARRAY OF BULLETS         
;-----------------------------------------------------------------------------
P2FIRE PROC NEAR                            ;SAME AS PLAYER 1
       
     PUSHA
     MOV BX,0
       
     FLAGLOOP2:
       MOV CL, PLAYER2_BF[BX]
       CMP CL,0
       JZ ADDBULLET2
       INC BX
       CMP BX,7
       JNZ FLAGLOOP2
       JMP P2FIRE_EXIT
       
     ADDBULLET2:
       MOV PLAYER2_BF[BX],1
       MOV PLAYER2_BX[BX],48H
       MOV CH,BYTE PTR player2_pos[1]
       MOV PLAYER2_BY[BX],CH 
       
     P2FIRE_EXIT:
       POPA
     
     RET
ENDP P2FIRE

                              `
DRAWBULLET PROC NEAR                          ;DRAW THE BULLETS IN THE ARRAY
     PUSHA
     MOV BX,0                       ;USED TO LOOP THROUGH ARRAY
           
     DRAWBULLET1LOOP:
        MOV CL, PLAYER1_BF[BX]
        CMP CL,1                        ;CHECK THERE IS BULLET TO BE DRAWN
        JNZ SKIP                        ;IF NO BULLET SKIP
        PUSH BX
        PUSH CX
        MOV DL,PLAYER1_BX[BX]           ;GET X POS 
        MOV DH,PLAYER1_BY[BX]           ;GET Y POS
        MOV BH,0
        MOV AH,2
        INT 10H                         ;MOVE CURSOR
           
        MOV AH,9                        ;DRAW THE BULLET
        MOV BH,0
        MOV AL,0AFH
        MOV CX,1
        MOV BL,012H
        INT 10H
        POP CX
        POP BX
           
       SKIP:
        INC BX                       ;INCREMENT AND MOVE THROUGH THE ARRAY
        CMP BX,7
     JNZ DRAWBULLET1LOOP          ;LOOP TILL THE END OF THE ARRAY
;SAME PROCEDURE FOR PLAYER 2    
     MOV BX,0
           
     DRAWBULLET2LOOP:             ;SAME AS PLAYER 1
        MOV CL, PLAYER2_BF[BX]
        CMP CL,1
        JNZ SKIP2
        PUSH BX
        PUSH CX
        MOV DL,PLAYER2_BX[BX]
        MOV DH,PLAYER2_BY[BX]
        MOV BH,0
        MOV AH,2
        INT 10H 
          
        MOV AH,9
        MOV BH,0
        MOV AL,0AEH
        MOV CX,1
        MOV BL,012H
        INT 10H
        POP CX
        POP BX
           
       SKIP2:
        INC BX
        CMP BX,7
     JNZ DRAWBULLET2LOOP
           
     POPA
     
     RET
ENDP DRAWBULLET
;---------------------------------------------------------
;==UPDATE THE LOCATION OF EACH BULLET FIRED BY P1
;----------------------------------------------------------
MOVEBULLET PROC NEAR                 
            
      PUSHA
            
      MOV BX,0
           
      MOVEBULLET1LOOP:
         MOV CL, PLAYER1_BF[BX]    ;CHECK THERE IS A BULLET
         CMP CL,1
         JNZ MOVEBULLET1SKIP       ;IF NO SKIP
         MOV DL,BULLETSPEED        ;IF YES INC BULLET POS BY THE BULLET SPEED
         ADD PLAYER1_BX[BX],DL
           
        MOVEBULLET1SKIP:
         INC BX                    ;INC TO CHECK THE WHOLE ARRAY
         CMP BX,7
      JNZ MOVEBULLET1LOOP       ;RETURN TO CHECK THE WHOLE ARRAY
           ;SAME FOR PLAYER2
      MOV BX,0
           
      MOVEBULLET2LOOP:         ;SAME AS PLAYER 1
         MOV CL, PLAYER2_BF[BX]
         CMP CL,1
         JNZ MOVEBULLET2SKIP
         MOV DL,BULLETSPEED
         CLC
         SUB PLAYER2_BX[BX],DL    ;IF THERE IS A BULLET DEC BY THE BULLET SPEED
         JC SFRHA                 ;IF X GOES BELOW ZERO CARRY FLAG IS 1
        MOVEBULLET2SKIP:
         INC BX
         CMP BX,7
      JNZ MOVEBULLET2LOOP
           
           
      JMP EXITMOVEBULLET       ;RETURN FROM THIS FUNCTION
         
      SFRHA:                  ;IF BULLET GOES BELOW ZERO
         MOV PLAYER2_BX[BX],2    ;SET IT'S POS MANUALLY TO 2 (RANDOM NUMBER) SO IT WILL NOT BE DRAWN
         JMP MOVEBULLET2SKIP  
            
     EXITMOVEBULLET: 
        
     POPA
     RET
ENDP MOVEBULLET  
;---------------------------------------------------
;===CHECK BULLETS POSITION OF PLAYER 1
;===TO REMOVE IT WHEN IT REACH PLAYER 2 AND TO CALCULATE HEALTH  
;---------------------------------------------------------
BULLET1STATUS PROC NEAR        
     PUSHA
   
     MOV BX,0                   ;USED TO LOOP
     BULLET1STATUSLOOP:
       MOV CL, PLAYER1_BF[BX]
       CMP CL,1                   ;CHECK IF THERE IS A BULLET
       JNZ BULLET1STATUSSKIP      ;IF NO SKIP
       
       ;CHECK IF THE BULLETS HITS THE WALL 
       MOV CH,PLAYER1_BX[BX]                ;PUT IN CH BULLET X POSITION
       MOV CL,BYTE PTR BLOCK_POS[0]         ;PUT IN CL BLOCK X POSITION
       CMP CH,CL                            ;COMPARE BETWEEN BOTH OF THEM
       JNAE  BULLET1STATUSSKIP              ;IF THE BULLET DIDNT REACH THE BLOCK SKIP THE LOOP
       
       ADD CL,6
       CMP CH,CL
       JNBE CHECKPLAYER2
                                            ;IT MEANS THAT IT DIDNT REACH THE PLAYER
       MOV CH,PLAYER1_BY[BX]                ;IF IT DID PASS THE BLOCK OR EQUAL TO IT CHECK Y POSTION
       MOV CL,BYTE PTR BLOCK_POS[1]
       CMP CH,CL                            ;COMPARE BETWEEN TOP OF THE BLOCK AND THE BULLET POSITION
       JB CHECKPLAYER2                      ;IF ABOVE CHECK THE OTHER Y IF NOT SKIP TO SEE IF IT HITS THE PLAYER
       
       ADD CL,5                             ;ADD 5 TO GET THE BOTTOM OF THE BLOCK AND COMPARE IF BELOW
                                            ;, IT MEANS THAT THE BULLET HITS THE BLOCK IF NOT THEN ITS OUT OF RANGE 
       CMP CH,CL
       JA CHECKPLAYER2                      ;IF IT HITS THE BLOCK MAKE THE FLAG EQUAL ZERO
       MOV PLAYER1_BF[BX],0H 
       PUSHA
       MOV AH,2H
       MOV DL,7
       INT 21H
       POPA
       JMP BULLET1STATUSSKIP
       
       CHECKPLAYER2:
       MOV CH, PLAYER1_BX[BX]     ;GET BULLETS X POSITION
       CMP CH, 49H                ;COMPARE WITH PLAYER 2 POSITION
       JNAE BULLET1STATUSSKIP     ;IF IT HAS NOT REACHED PLAYER 2 SKIP
        
       MOV PLAYER1_BF[BX],0       ;IF IT REACHED X POS OF PLAYER 2 REMOVE IT FROM THE ARRAY
       MOV CH,PLAYER1_BY[BX]             ;COMPARE Y POS OF BULLET WITH THE POS OF PLAYER 2
       MOV CL,BYTE PTR player2_pos[1]    
       CMP CH,CL
       JB BULLET1STATUSSKIP             ;IF ABOVE PLAYER 1 UPPER Y, SKIP 
   
       ADD CL,3H                      ;IF NOT ABOVE 
                                  ;CALCULATE PLAYER 1 LOWER Y
       CMP CH,CL
       JA BULLET1STATUSSKIP         ;IF BELOW LOWER Y, IT IS NOT IN RAGE .SKIP
   
       SUB HEALTH2,1            ;IF NOT BELOW , IT IS IN RANGE
       MOV DH,HEALTH2           ;IT HITS PLAYER 2, DEC HEALTH
       CMP DH,0                 ;IF HEALTH OF PLAYER 2 IS ZERO
       JZ PLAYER1WIN 
       CALL UPDATEHEALTHBAR     ;PLAYER 1 WINS
      BULLET1STATUSSKIP:     
       INC BX
       CMP BX,7                ;INC AND LOOP THROUGH ARRAY
    JNZ BULLET1STATUSLOOP
   ;---------------------------PLAYER2------------
    
    MOV BX,0                 ;SAME AS PLAYER 1
    BULLET2STATUSLOOP:
       MOV CL, PLAYER2_BF[BX]
       CMP CL,1
       JNZ BULLET2STATUSSKIP 
       
              ;CHECK IF THE BULLETS HITS THE WALL 
       MOV CH,PLAYER2_BX[BX]
       MOV CL,BYTE PTR BLOCK_POS[0]
       ADD CL,7H
       CMP CH,CL
       JNBE  BULLET2STATUSSKIP 
       
       SUB CL,6
       CMP CH,CL
       JNAE CHECKPLAYER1
       
       MOV CH,PLAYER2_BY[BX]
       MOV CL,BYTE PTR BLOCK_POS[1]
       CMP CH,CL
       JB CHECKPLAYER1
       
       ADD CL,5
       CMP CH,CL
       JA CHECKPLAYER1
       MOV PLAYER2_BF[BX],0H 
       PUSHA
       MOV AH,2H
       MOV DL,7
       INT 21H
       POPA
       JMP BULLET2STATUSSKIP
       CHECKPLAYER1:
       
       MOV CH, PLAYER2_BX[BX]   
       CMP CH, 6H                 ;PLAYER 1 X POS
       JA BULLET2STATUSSKIP
   
       MOV PLAYER2_BF[BX],0
       MOV CH,PLAYER2_BY[BX]    
       MOV CL,BYTE PTR player1_pos[1]    
       CMP CH,CL
       JB BULLET2STATUSSKIP
   
       ADD CL,3H
   
       CMP CH,CL
       JA BULLET2STATUSSKIP
    
       SUB HEALTH1,1
       MOV DH,HEALTH1
       CMP DH,0
       JZ PLAYER2WIN 
       CALL UPDATEHEALTHBAR
      BULLET2STATUSSKIP:
       INC BX
       CMP BX,7
   JNZ BULLET2STATUSLOOP
    
     POPA 
     RET 
ENDP BULLET1STATUS  
;--------------------------------------
MOVEBLOCK PROC NEAR 
    PUSHA
    
    MOV BL,UPFLAG        ;CHECK MOVE UP FLAG
    CMP BL,1
    JZ MOVEUP          
    
    MOV AH,BYTE PTR BLOCK_POS[1]     ;MOV DOWN
    MOV AL,end_drawing_screen        ;COMPARE BLOCK POSITION AND END OF DRAWING AREA
    SUB AL,2                         ;IF REACHED END OF DRAWING SCREEN
    CMP AH,AL
    JZ CHNG_TO_UP
                                     ;IF NOT END OF SCREEN, MOVE DOWN
    ADD AH,BLOCKSPEED
    MOV BYTE PTR BLOCK_POS[1],AH
    
    POPA
    RET
    
CHNG_TO_UP:             ;CHANGE UP FLAG
    MOV BL,1
    MOV UPFLAG,BL
    POPA
    RET
    
CHNG_TO_DOWN:
    MOV BL,0
    MOV UPFLAG,BL
    POPA
    RET    
    
MOVEUP:
    MOV AH,BYTE PTR BLOCK_POS[1]     ;BLOCK POSITIOSITION
    MOV AL,start_drawing_screen
    CMP AH,AL
    JZ CHNG_TO_DOWN
    
    SUB AH,BLOCKSPEED
    MOV BYTE PTR BLOCK_POS[1],AH               
    
            
              
    POPA
    RET
ENDP MOVEBLOCK
;-----------------------------------------      
;===CHAT MODE (IN PHASE2) 
;-----------------------------------  
CHATJMODE PROC NEAR
           ;CHANGE CURSER POSITION
           MOV DL,25
           MOV DH,10   
           MOV BH,0
           MOV AH,2
           INT 10H
           ;PRINT MSG
           MOV DX,OFFSET CHAT_MSG
           MOV AH,9
           INT 21H
                     
            MOV CX,25
           MOV AH,86H
           INT 15H   ;DELAY TO READ THE MSG 
           MOV SEND,0
           MOV REC,0
           ;CLEAR THE SCREEN
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h          
          
          RET
ENDP CHATJMODE   

    
;-------------------------------------------------------------------
;==THIS PROC IS FOR REINTIALIZING ALL VARIABLES FOR A NEW GAME
;------------------------------------------------------------------
NEW_GAME PROC NEAR 
         MOV BX,0
         JFLAG_LOOP:          ;THIS LOOP IS TO REINITIALIZE THE BULLETS FLAG ARRAYS WITH ZEROS
         MOV PLAYER1_BF[BX],0
         MOV PLAYER2_BF][BX],0
         INC BX
         CMP BX,7
         JNZ JFLAG_LOOP
         ;REFILL THE HEALTH BAR FOR THE TWO PLAYERS
         MOV HEALTH1,7
         MOV HEALTH2,7
         
         
        ; MOV JPLAYER1NAME,33D ;NOT NECESSARY BUT JUST TO MAKE SURE
        ; MOV JPLAYER2NAME,33D  
         ;THIS LOOP IS TO DELETE THE NAMES
        ; MOV BX,1   ;NOTE STARTED WITH 1 NOT 0 CAUSE THE FIRST PLACE IS USED TO KNOW THE MAX TOTAL LENGTH OF THE NAMES(DOSEN'T CHANGE)
         
        ; JNAME_LOOP:
        ; MOV JPLAYER1NAME[BX],'$'
        ; MOV JPLAYER2NAME[BX],'$'
        ; INC BX
        ; CMP BX,35
        ; JNZ JNAME_LOOP
         
         ;REINTIALIZE THE TYPES TO 0
        ; MOV JP1T,0
         ;MOV JP2T,0
         
         ;REINTIALZIE THE POSTIONS OF THE TWO PLAYERS
         MOV player1_pos,0900h
         MOV player2_pos,0948h 
         
         
          
         RET 
ENDP NEW_GAME
             
CHATAMODE PROC 
PUSHA
         
                  MOV AH,6H 
         MOV BH,0FH
         MOV AL,0  
         MOV CX,0H
         MOV DL,4FH
         MOV DH,18H
         INT 10H  
         
                  
         mov ah,3h
         mov bh,0h
         int 10h
                   

         ;SPLIT SCREEN  DL=X , DH=Y , STARTPOS 
         mov ah,2h
         mov dx,00H
         MOV DH,SPLITSCREEN 
         mov ah,2h 
         int 10h
         mov ah,9 ;Display
         mov al,'-' ;ASCII
         mov cx,80 ;80 times
         mov bl,00fh
         int 10h 
                  
      
         ;PREPARING CHAT APP
         ;PLLAYER1
         MOV DX,CHAT1_POS
         MOV AH,2H
         INT 10H
         
         MOV BX,2
         mov ah, 9
         LEA dx,JPLAYER1NAME[BX]
         int 21h
         
        mov ah,3h
        mov bh,0h
        int 10h  
       
        MOV BX,1
        ADD DL, JPLAYER1NAME[BX]
        MOV CHAT1_POS,DX 
        MOV AH,2H
        INT 10H  
         

        
         mov ah,2
         mov dl,':'
         int 21h
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT1_POS,DX
          
         ;PLAYER2
         MOV DX,CHAT2_POS
         MOV AH,2H
         INT 10H
         
         MOV BX,2
         mov ah, 9
         LEA dx,JPLAYER2NAME[BX]
         int 21h   
         
                 mov ah,3h
        mov bh,0h
        int 10h  
       
        MOV BX,1
        ADD DL, JPLAYER2NAME[BX]
        MOV CHAT2_POS,DX 
        MOV AH,2H
        INT 10H  
         
       
         mov ah,2
         mov dl,':'
         int 21h
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT2_POS,DX
         
   MAINLOOP3:
         ;SENDING PART.....
         MOV AH,1H
         INT 16H  
         JZ SKIPCHATMODE 
         
         ;PLAYER1 POSITION
         MOV DX,CHAT1_POS
         MOV AH,2H
         INT 10H
         
         MOV AH,0H
         INT 16H 
         CMP AH,1CH
         JZ ENTERKEY
         CMP AL,8H
         JZ BACKSPACE1
         CMP AL,1BH
         JZ EXITCHAT 
         
         
         MOV CL,SPLITSCREEN    
         DEC CL
         CMP DH,CL
         JZ SCROLL1 
         
         ;PRINTING PLAYER1PART
         MOV AH,2H
  		 MOV DL,AL
  		 INT 21H 
  		 
         MOV VALUE,AL 
         
        ;CHECK TRANSMITTER HOLDING REGISTER IS EMPTY
         MOV DX,3FDH
         AGAIN5:
         IN AL,DX
         AND AL,00100000B
         JZ AGAIN5
         
         
         ;IF EMPTY PUT VALUE IN TRANSIMTTER REGISTER
          
         MOV DX,3F8H
         MOV AL,VALUE
         OUT DX,AL
         
            		 
  		 mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT1_POS,DX 
         
         JMP SKIPCHATMODE    
         
         BACKSPACE1:
         MOV VALUE,08H 
         
        ;CHECK TRANSMITTER HOLDING REGISTER IS EMPTY
         MOV DX,3FDH
         AGAIN4:
         IN AL,DX
         AND AL,00100000B
         JZ AGAIN4
         
         
         ;IF EMPTY PUT VALUE IN TRANSIMTTER REGISTER
          
         MOV DX,3F8H
         MOV AL,VALUE
         OUT DX,AL
         
         
  		 mov ah,3h
         mov bh,0h
         int 10h  
  		  
  		 CMP DL,0
  		 JNZ CONTBACKSPACE
  		 CMP DH,0
  		 JZ SKIPCHATMODE
  		 MOV DL,4FH
  		 DEC DH
  		 MOV AH,2H
  		 INT 10H 
  		 PUSHA  		 
  		   
  		 MOV AH,2H
  		 MOV DL,20H
  		 INT 21H
  		 
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H
  		 
  		 POPA
  		 mov ah,2
         mov DL,4FH
         int 10h 
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT1_POS,DX        
         JMP SKIPCHATMODE
  		 
  		 CONTBACKSPACE:
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H
  		 
         MOV AH,2H
  		 MOV DL,20H
  		 INT 21H  
  		 
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H
          
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT1_POS,DX
                
         JMP SKIPCHATMODE
         
         ENTERKEY:
        
         MOV AH,2H
  		 MOV DL,0AH
  		 INT 21H
         MOV AH,2H
  		 MOV DL,0DH
  		 INT 21H 
  		 
  		 mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT1_POS,DX 
         
         MOV CL,SPLITSCREEN    
         DEC CL
         CMP DH,CL
         JZ SCROLL1 
         
         MOV DX,CHAT1_POS
         MOV AH,2H
         INT 10H
         
         MOV BX,2
         mov ah, 9
         LEA dx,JPLAYER1NAME[BX]
         int 21h  
         
        mov ah,3h
        mov bh,0h
        int 10h  
       
        MOV BX,1
        ADD DL, JPLAYER1NAME[BX]
        MOV CHAT1_POS,DX 
        MOV AH,2H
        INT 10H  
         
       
         mov ah,2
         mov dl,':'
         int 21h
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT1_POS,DX
          
         
                  MOV VALUE,0AH 
         
        ;CHECK TRANSMITTER HOLDING REGISTER IS EMPTY
         MOV DX,3FDH
         AGAIN2:
         IN AL,DX
         AND AL,00100000B
         JZ AGAIN2
         
         
         ;IF EMPTY PUT VALUE IN TRANSIMTTER REGISTER
          
         MOV DX,3F8H
         MOV AL,VALUE
         OUT DX,AL       
         JMP SKIPCHATMODE
         
         SCROLL1:
         MOV AH,6H 
         MOV BH,0FH
         MOV AL,0  
         MOV CX,0H
         MOV DL,4FH
         MOV DH,SPLITSCREEN
         DEC DH
         INT 10H  
         
         mov ah,3h
         mov bh,0h
         int 10h  
         
         mov ah,2
         mov dl,00h
         mov dh,00h 
         MOV CHAT1_POS,DX
         int 10h
         
         
         MOV BX,2
         mov ah, 9
         LEA dx,JPLAYER1NAME[BX]
         int 21h
         
                 mov ah,3h
        mov bh,0h
        int 10h  
       
        MOV BX,1
        ADD DL, JPLAYER1NAME[BX]
        MOV CHAT1_POS,DX 
        MOV AH,2H
        INT 10H  
         

      
         mov ah,2
         mov dl,':'
         int 21h
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT1_POS,DX
          
         JMP SKIPCHATMODE
         
         EXITCHAT:  
          MOV VALUE,1BH 
         
        ;CHECK TRANSMITTER HOLDING REGISTER IS EMPTY
         MOV DX,3FDH
         AGAIN6:
         IN AL,DX
         AND AL,00100000B
         JZ AGAIN6
         
         
         ;IF EMPTY PUT VALUE IN TRANSIMTTER REGISTER
          
         MOV DX,3F8H
         MOV AL,VALUE
         OUT DX,AL
         
                  
         MOV CHAT1_POS,0000H    
         MOV CHAT2_POS,0C00H             
   
            POPA
            RET
             
         JMP SKIPCHATMODE
         
         SKIPCHATMODE:

         ;-----------------------RECIEVE-----------------------
        
         MOV DX,CHAT2_POS
         MOV AH,2H
         INT 10H
                  
         MOV CL,16h   
         CMP DH,CL
         JZ SCROLL2  
        
        ;CHECK DATA READY
        mov dx , 3FDH		; Line Status Register
	    ;CHK:
	    in al , dx 
  		AND al , 1
  		JZ SKIPREC
  		;JZ CHK
               
        ;If Ready read the VALUE in Receive data register
  		mov dx, 03F8H
  		in  al, dx 
  		mov VALUE , al
  		CMP VALUE,0AH   ;ENTER PRESSED 
  		JZ  ENTER
        CMP AL,8H
        JZ BACKSPACE2
        CMP AL,1BH
        JZ EXITCHAT2
         
  		MOV AH,2H
  		MOV DL,VALUE
  		INT 21H


        JMP SKIPREC  
        
         BACKSPACE2:
  		 mov ah,3h
         mov bh,0h
         int 10h  
  		  
  		 CMP DL,0
  		 JNZ CONTBACKSPACE2
  		 MOV CL,SPLITSCREEN
  		 INC CL
  		 CMP DH,CL
  
  		 JZ SKIPREC
  		 MOV DL,4FH
  		 DEC DH
  		 MOV AH,2H
  		 INT 10H 
  		 PUSHA  		 
  		   
  		 MOV AH,2H
  		 MOV DL,20H
  		 INT 21H
  		 
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H
  		 
  		 POPA
  		 mov ah,2
         mov DL,4FH
         int 10h 
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT2_POS,DX 
  		 
  		 JMP SKIPREC
  		 
  		 CONTBACKSPACE2:
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H
  		 
         MOV AH,2H
  		 MOV DL,20H
  		 INT 21H  
  		 
  		 MOV AH,2H
  		 MOV DL,08H
  		 INT 21H
          
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT2_POS,DX
         
         JMP SKIPREC
                   
        ENTER:
                           
         MOV AH,2H
  		 MOV DL,0AH
  		 INT 21H
         MOV AH,2H
  		 MOV DL,0DH
  		 INT 21H 
  		 
  		 mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT2_POS,DX 
         
         MOV CL,16H   
         CMP DH,CL
         JZ SCROLL2 


         MOV BX,2
         mov ah, 9
         LEA dx,JPLAYER2NAME[BX]
         int 21h  

                 mov ah,3h
        mov bh,0h
        int 10h  
       
        MOV BX,1
        ADD DL, JPLAYER2NAME[BX]
        MOV CHAT2_POS,DX 
        MOV AH,2H
        INT 10H  
         
              
         mov ah,2
         mov dl,':'
         int 21h
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT2_POS,DX    
         JMP SKIPREC
         
           SCROLL2:
         MOV AH,7H 
         MOV BH,0FH
         MOV AL,0h  
         MOV CL,00H
         MOV CH,SPLITSCREEN
         ADD CH,1H
         MOV DX,154FH
         INT 10H  
         
         mov ah,3h
         mov bh,0h
         int 10h  
         
         mov ah,2
         mov dl,00h
         mov dh,SPLITSCREEN  
         INC DH
         MOV CHAT2_POS,DX
         int 10h
         
         
         MOV BX,2
         mov ah, 9
         LEA dx,JPLAYER2NAME[BX]
         int 21h     

                 mov ah,3h
        mov bh,0h
        int 10h  
       
        MOV BX,1
        ADD DL, JPLAYER2NAME[BX]
        MOV CHAT2_POS,DX 
        MOV AH,2H
        INT 10H  
         
             
         mov ah,2
         mov dl,':'
         int 21h
         
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT2_POS,DX  
         JMP SKIPREC
         
        
         EXITCHAT2:  
         
         MOV CHAT1_POS,0000H    
         MOV CHAT2_POS,0C00H             

         POPA
         RET
         
         JMP SKIPREC
                   
        SKIPREC: 
         mov ah,3h
         mov bh,0h
         int 10h  
         MOV CHAT2_POS,DX
         
   JMP MAINLOOP3  

POPA    
RET              
ENDP CHATAMODE  
GET_LEVEL PROC
              
           ;CLEAR SCREEN 
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h   
           
           ;CHANGE CURSER POSITION
           MOV DL,30 ;X VALUE
           MOV DH,10 ;Y VALUE    
           MOV BH,0
           MOV AH,2
           INT 10H
            
           ;PRINT MSG 
           MOV DX,OFFSET level_msg
           MOV AH,9
           INT 21H 
             
           ;READ CHAR  
           MOV AH,7
           INT 21H   
           ;MOVE READ VALUE TO THE VARIABLE
           MOV LEVEL_VAR,AL  
           
           
          ;MULTIPLE CHECKS ON THE READ CHAR TO MAKE SURE THE LEVEL IS BETWEEN 0 TO 1
           ;IF NOT MAKE 1 BY DEFAULT
             
           CMP LEVEL_VAR,'2'                
           JA DEFAULTLEVELVALUE  ;IF THE ENTERD NUMBER ABOVE 2 THEN PUT 1 (DEFAULT VALUE)
                                 
           CMP LEVEL_VAR,'1'
           JB DEFAULTLEVELVALUE    ;IF THE ENTERD NUMBER IS LESS THAN 1 THEN PUT 1 (DEFAULT VALUE)
           
           JNB SKIPFORLEVEL
           
           DEFAULTLEVELVALUE:
           MOV LEVEL_VAR,'1'
           
           SKIPFORLEVEL:            ;OTHER THAN THAT THEN IT'S EITHER ONE OR TWO  THEN LEAVE IT AS IT IS
           
           
           
           
           MOV AH,2
           MOV DL,LEVEL_VAR             ;ECHO THE NUMBER ON THE SCREEN TO MAKE IT MORE NATURAL
           INT 21H             
           
           SUB LEVEL_VAR,30H
           
           
           
           ;DELAY FUNC TO MAKE IT MORE HUMAN XD
           MOV CX,8
           MOV AH,86H
           INT 15H
           
           ;CLEAR SCREEN
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h 

           RET
ENDP GET_LEVEL               
WAIT_LEVEL PROC
    
              
           ;CLEAR SCREEN 
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h   
           
           ;CHANGE CURSER POSITION
           MOV DL,20 ;X VALUE
           MOV DH,10 ;Y VALUE    
           MOV BH,0
           MOV AH,2
           INT 10H
            
           ;PRINT MSG 
           MOV DX,OFFSET level_msg2
           MOV AH,9
           INT 21H 
    
    
    
    
       
           RET
ENDP WAIT_LEVEL

END MAIN        
;--------------------------------------------------
;=============END PROCEDURES=======================
;--------------------------------------------------