RECBYTE MACRO RESULT
LOCAL CHK            
            mov dx , 3FDH		; Line Status Register
	    in al , dx 
  	    AND al , 1
  	    JZ CHK
               
        ;If Ready read the VALUE in Receive data register
  		mov dx, 03F8H
  		in  al, dx 
  		mov RESULT , al
CHK:
ENDM RECBYTE 

RECV_BYTE MACRO RECVB
          
         LOCAL DETR 
         DETR:       
         mov dx , 3FDH		; Line Status Register
	     in al , dx 
  	     AND al , 1
  	     JZ DETR

      
         mov dx , 03F8H
  	     in al , dx  
  	     MOV RECVB,AL
          
         
          
ENDM RECV_BYTE 

SEND_INV MACRO 
         LOCAL WAITFACC
         LOCAL CONTRECV
         LOCAL WAITF
         LOCAL CONTSEND  
         
         mov cl,typeee      
         mov JP1T,cl      ;I AM THE HOST THEN I AM PLAYER1 AND MOVE TYPE TO P1
         
         
         MOV CL,1        ;SEND 1 TO INTIALIZE DATA EXCHANGE PROTOCAL AND LET HIM KNOW I AM READY
         SEND_BYTE CL
         
         
         WAITFACC:
         RECV_BYTE CL   ;WAIT FOR 5 TO MAKE SURE HE IS READY TO INTIALIZE PROTOCAL ALSO
         CMP CL,5 
         JNZ WAITFACC
         
         MOV CL,6      ;6 MEANS I AM READY TO RECIEVE HIS DATA
         SEND_BYTE CL ;MEANS READY
         
         RECV_BYTE JP2T  ;RECIEVE HID TYPE 
                  
                      MOV BX,1
                      CONTRECV: 
                      RECV_BYTE JPLAYER2NAME[BX]      ;THEN RECIEVE FULL NAME 
                      INC BX
                      CMP BX,35        ;25 BEACUSE THE MAXIMUM SIZE OF THE ARRAY IS 35
                      JNZ CONTRECV
         
         
        WAITF:
        RECV_BYTE CL    ;THEN WAIT FOR 8 TO KNOW HE IS DONE SENDING HIS DATA
        CMP CL,8
        JNZ WAITF
        
                    
         MOV CL,9     ;SEND 9 TO LET HIM KNOW I AM GOING TO SEND MY DATA
         SEND_BYTE CL
         
          SEND_BYTE JP1T   ;SEND MY TYPE
          SEND_BYTE LEVEL_VAR      ;SEND THE GAME LEVEL BEACUSE I AM THE HOST AND I WHO CHOOSE THE GAME LEVEL
         MOV BX,1 
                      CONTSEND: 
                      SEND_BYTE JPLAYER1NAME[BX]  ;SEND FULL NAME
                      INC BX
                      CMP BX,35
                      JNZ CONTSEND  
    
ENDM SEND_INV    



  
  ACC_INV MACRO
        LOCAL WAITFRE
        LOCAL CONTSEND
        LOCAL WAITFE
        LOCAL CONTRECV    
        
        
        mov cl,typeee   ;I AM THE GUEST THEN I AM PLAYER 2
        mov JP2T,cl     ; MOVE TYPE TO P2
        
        MOV CL,5        ;SEND 5 TO LET HIM KNOW I AM GOING TO SEND MY DATA
        SEND_BYTE CL 
        
        WAITFRE:
        RECV_BYTE CL    ;WAIT FOR 6 BEFORE START SENDING TO MAKE SURE HE IS ALSO READY TO RECIEVE
        CMP CL,6
        JNZ WAITFRE
        
        SEND_BYTE JP2T     ;SEND MY TYPE
        
         MOV BX,1 
                      CONTSEND: 
                      SEND_BYTE JPLAYER1NAME[BX]    ;SEND MY NAME
                      INC BX
                      CMP BX,35
                      JNZ CONTSEND
                      
                      
         MOV CL,8
         SEND_BYTE CL      ;SEND 8 TO LET HIM KNOW I AM DONE SENDING
         
         
         WAITFE:
        RECV_BYTE CL       ;WAIT FOR 9 TO KNOW HE IS ABOUT TO SEND THEN I START RECIEVING 
        CMP CL,9
        JNZ WAITFE
        
           RECV_BYTE JP1T    ;RECIEVE TYPE OF P1
           RECV_BYTE LEVEL_VAR        
                      MOV BX,1
                      CONTRECV: 
                      RECV_BYTE JPLAYER2NAME[BX]    ;RECIEVE FULL NAME
                      INC BX
                      CMP BX,35
                      JNZ CONTRECV
           pusha
           mov bx,0
           mov bl,JPLAYER2NAME[1]   
           add bx,3
           mov JPLAYER2NAME[bx],'$'       
           popa  
           
        

           
ENDM ACC_INV                 
                 


SENDBYTE MACRO SBYTE
LOCAL AGAIN

                mov dx , 3FDH		; Line Status Register
AGAIN:  	In al , dx 			;Read Line Status
  		AND al , 00100000b
  		JZ AGAIN
                
		mov dx , 3F8H		; Transmit data register
  		mov al,SBYTE
  		out dx , al 
ENDM SENDBYTE 
                            
SEND_BYTE MACRO SENDB
          
           LOCAL CHECKTHR
           
           mov dx , 3FDH		; Line Status Register
           CHECKTHR:
           In al , dx 			;Read Line Status
  		   AND al , 00100000b
  
           JZ CHECKTHR
           
           
     
           mov dx , 3F8H		; Transmit data register
  		   MOV AL,SENDB
     	   out dx , al 

    
ENDM SEND_BYTE                           



STARTSCREEN MACRO JMSTARTSCREEN1,JMSTARTSCREEN2,JMSTARTSCREEN3        ;THIS MACRO USED FOR START MENU
            
           LOCAL request 
	   LOCAL endnotify
	   LOCAL checkchat
           LOCAL checkrec
	   LOCAL checkchat2
	   LOCAL continue
LOCAL continue1
LOCAL continue2
LOCAL checkinput
              ;SCROLL FULL SCREEN 
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h           
         
           
           
           ;CHANGE OFFSET AND PRINT START GAME MSG
           
           MOV DL,34 ;X VALUE
           MOV DH,8 ;Y VALUE    
           MOV BH,0
           MOV AH,2
           INT 10H
           
           MOV DX,OFFSET JMSTARTSCREEN1
           MOV AH,9
           INT 21H
           
           
           
            ;CHANGE OFFSET (1 LINE UNDER) TO PRINT CHAT MSG
            MOV DL,34
           MOV DH,10   
           MOV BH,0
           MOV AH,2
           INT 10H
           
           MOV DX,OFFSET JMSTARTSCREEN2
           MOV AH,9
           INT 21H
                  
                  
           ;CHANGE OFFSET (1 LINE UNDER) TO PRINT EXIT GAME MSG       
           MOV DL,34
           MOV DH,12   
           MOV BH,0
           MOV AH,2
           INT 10H
           
           MOV DX,OFFSET JMSTARTSCREEN3
           MOV AH,9
           INT 21H       
           ;DRAE NOTIFICATION BAR AND PRINT MSG
           DRAW_NOTIF_BAR WELCOME_MSG
           
           ;LOOP TO CHECK IF ANY KEY WERE PRESSED OR NOT IF YES CHECK IT IF NO WAIT FOR ONE
           
           JSTARTLOOP:             ;CHECK IF KEY PRESSSED OR NOT  C->PRESSED B->NOT
;=========================RecData============
       RECBYTE rec   ;THIS PART HANDLE DETERMINING PLAYERNUM (PLAYER1 OR 2) 	   
	   cmp send,0
	   jz checkinput1
           cmp rec,0
	   jnz checkinput1
	   mov playernum,1
	   jmp checkinput
           checkinput1:
	   cmp rec,0
	   jz checkinput
           cmp send,0
	   jnz checkinput
	   mov playernum,2
	   
;================================
	  checkinput:    ;CHECK IF SEND AND REC MATCHES
	   cmp rec,3
	   jnz continue1
	   MOV CHOSEN_OPTION,3
           Jmp JEXITSTART  
           continue1:
	   cmp rec,1
	   jnz continue2
	   cmp send,1
	   jnz continue2
	   mov CHOSEN_OPTION,1
	   jmp JEXITSTART 
	continue2:
	   cmp rec,2
	   jnz continue
	   cmp send,2
	   jnz continue
	   mov CHOSEN_OPTION,2
	   jmp JEXITSTART 
;========================
continue:

;=============MSGS IN NOTIFICATION BAR===========
	CMP send,0
	jnz request
	cmp rec,0
	jnz checkrec
        DRAW_NOTIF_BAR WELCOME_MSG  
	jmp endnotify
	
	request:
	cmp send,1 
	jnz checkchat
	DRAW_NOTIF_BAR waitingplayer
	jmp endnotify 
	
	checkchat:
	cmp send,2
	jnz checkrec
	DRAW_NOTIF_BAR waitingplayer2
	jmp endnotify

	checkrec:
	cmp rec,1 
	jnz checkchat2
	DRAW_NOTIF_BAR gameinv
	jmp endnotify
	checkchat2:
	cmp rec,2
	jnz endnotify
	DRAW_NOTIF_BAR chatinv
	endnotify:
;=====================================================	  
	   MOV AH,1
           INT 16H
           JNZ C
           JZ B
           
           ;IF KEY PRESSED READ IT 
           C:MOV AH,0
           INT 16H
           
           ;IF IT'S 1 EXIT LOOP AND SET OPTION FLAG NAMED CHOSEN_OPTION WITH 1
           CMP Al,49
	   JNZ CHECK2NDKEY  
	   cmp rec,2
           jz B  
	   cmp send,1
	   jz B            
	   SENDBYTE 1 
           mov send,1         
           JMP B                                                     
           CHECK2NDKEY:
           ;IF IT'S 2 EXIT LOOP AND SET OPTION FLAG NAMED CHOSEN_OPTION WITH 2
           CMP Al,50
	   JNZ CHECK3RDKEY  
	   cmp rec,1
           jz B 
           cmp send,2 
           jz B           
	   SENDBYTE 2 
           mov send,2         
           JMP B 
       CHECK3RDKEY:
           ;IF IT'S 3 EXIT LOOP AND SET OPTION FLAG NAMED CHOSEN_OPTION WITH 3 
           CMP Al,51
           JNZ B
           SENDBYTE 3 
           mov send,3
           MOV CHOSEN_OPTION,3
           JMP JEXITSTART 
 
            ;NO KEY WAS PRSSED OR WRONG ONE WAS EITHER CASE WAIT FOR ANOTHER ONE
           B:

           JMP JSTARTLOOP
           ;AFTER CHOOSING AN OPTION EXIT THE LOOP AND CLEAR THE SCREEN
           JEXITSTART:
           ;CLEAR THE SCREEN
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h
           
    
ENDM STARTSCREEN     

;THIS MACRO USED TO GET PLAYER NAME
               
GETPLAYERNAME MACRO JMPNAME1,JMPLAYER1NAME      
           ;MOVE CURSER AND PRINT ENTER NAME MSG FOR PLAYER 1   
           
           DRAW_NOTIF_BAR ENTER_NAME_MSG ;DRAW NOTIFICATION BAR   
           MOV DL,28 ;X VALUE
           MOV DH,9  ;Y VALUE    
           MOV BH,0
           MOV AH,2
           INT 10H
           
           MOV DX,OFFSET JMPNAME1
           MOV AH,9
           INT 21H
           
           ;READ PLAYER ONE NAME
           MOV AH,0AH
           LEA DX,JMPLAYER1NAME
           INT 21H
           
            pusha
                 mov bx,0
           mov bl,JMPLAYER1NAME[1]   
           add bx,3
           mov JMPLAYER1NAME[bx],'$'
            popa
           ;CLEAR SCREEN EXCEPT FOR THE LAST TWO LINES
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,22 ; lower right Y
           mov dl,79 ; lower right X
           int 10h 
           
           
         
               
    
ENDM GETPLAYERNAME  

GETUSERTYPE MACRO JMPLAYER1NAME,JMPTYPE,JMP1T    ;THIS MACRO IS USED TO GET THE WANTED SHAPES FROM PLAYERS
    
           
           LOCAL DEFAULTTYPE
           LOCAL SKIPT
           LOCAL DEFAULTTYPE0
           LOCAL SKIPT0
 
           
            
           ;MOVE POSITION OF FIRST SHAPE
           MOV DX,0140H
           ;DRAW FIRST SHAPE
           draw_shape_select normal_person_shape 
           ;THE REST IS THE SAME AS 1ST SHAPE
                                                     
           MOV DX,0940H
           
           draw_shape_select Tank
           
           
           MOV DX,1040H
           
           draw_shape_select Robot  
           ;DRAW NOTIFICATION BAR
           DRAW_NOTIF_BAR CHOOSE_TAYP_MSG
           ;CHANGE CURSER POSITION
           MOV DL,20 ;X VALUE
           MOV DH,9  ;Y VALUE    
           MOV BH,0
           MOV AH,2
           INT 10H
           
           ;WRITE 1ST PLAYER NAME THEN WRITE CHOOSE TAYP THEN READ TAYP
           LEA DX,JMPLAYER1NAME
           ADD DX,2
           MOV AH,9
           INT 21H
           
           MOV DL,20 ;X VALUE 
           ADD DL,JMPLAYER1NAME[1]    ;ADDING THE LENGTH OF THE NAME TO PRINT SAVED STRING RIGHT
           MOV DH,9  ;Y VALUE    
           MOV BH,0
           MOV AH,2
           INT 10H
           
           LEA DX,JMPTYPE      ;PRINT STORED MSG
           MOV AH,9
           INT 21H
                                  ;READ CHAR
           MOV AH,7
           INT 21H
           
           MOV JMP1T,AL
                                      ;MULTIPLE CHECKS ON THE READ CHAR TO MAKE SURE THE TAYP IS BETWEEN 0 TO 2 
                                       ;IF NOT MAKE 0 BY DEFAULT
 
           
           CMP JMP1T,'3'                
           JAE DEFAULTTYPE  ;IF THE ENTERD NUMBER EQUAL OR ABOVE 3 THEN PUT 0 (DEFAULT VALUE)
                                 
           CMP JMP1T,'0'
           JB DEFAULTTYPE    ;IF THE ENTERD NUMBER IS LESS THAN 0 THEN PUT 0 (DEFAULT VALUE)
           
           JNB SKIPT
           
           DEFAULTTYPE:
           MOV JMP1T,'0'
           
           SKIPT:            ;OTHER THAN THAT THEN IT'S EITHER ONE OR TWO OR THREE THEN LEAVE IT AS IT IS
           
           
           
           
           MOV AH,2
           MOV DL,JMP1T             ;ECHO THE NUMBER ON THE SCREEN TO MAKE IT MORE NATURAL
           INT 21H  
           
           
           
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
            
            
            
    
ENDM GETUSERTYPE     

draw_shape_select MACRO shape          ;THIS MACRO IS USED TO DRAW SHAPES ON SCREEN
LOCAL jloop2
     pusha   
     ; CHANGE CURSER POSTION 
     ;NOTE THAT MOV DX,VALUE IS NOT HERE CUZ IN DO THE INSTRUCTION RIGHT BEFORE I CALL THIS MACRO 
     mov ah,2h
    
     MOV BH,0
     int 10h
     ;JCOUNT IS PRESENT TO KEEP TRACK OF ALL THE PRINTED CHARS (INDICATOR THAT I REACHED MY LAST SENTANCE (ARRAY))
     mov JCOUNT,0
     mov bx,0h  ;BX HERE IS USED TO GET THE OFFSET OF THE ARRAYS
     jloop2:
        mov ah,2h
        int 10h     ;MOVE THE CURSER
        mov di,dx   ;STORE THE CURSER POSTION  IN DI 
        mov bl,JCOUNT ;BL NOT BX JUST CUZ THE JCIUNT IS DB NOT DW      
        mov ah,9h   
        lea dx,shape[bx] ;PRINT MSG 
        int 21h
        add JCOUNT,8   ;ADDING ARRAY LENGHT ON BL TO GET THE OFFSET OF NEXT ARRAY FOR THE NEXT IRITATION
        mov dx,di      ;GET BACK THE CURSER POSTION IN DX
        inc dh          ;INC DH TO GO TO THE NEXT LINE 
        cmp JCOUNT,48   ;CHECK IF I PRINTED ALL THE ARRAYS OR NOT
        jnz jloop2
     
    
    popa
ENDM draw_shape_select    


PRINTSTATRTWORD MACRO JMWORD            ;THIS MACRO IS USED TO PRINT LARGE WORDS ON SCREEN (READY STEADY GO)
    
      LOCAL loop2          
                mov position,060Ah   ;SAME AS THE PREVIOUS MACRO 
     
  
      pusha  
     mov ah,2h   
     mov dx,position
     int 10h
     
     mov j2count,0
     mov bx,0h
     loop2:
        mov ah,2h  
        push bx
        mov bh,0
        int 10h 
        pop bx
        mov di,dx  
        mov bx,0
        mov bx,j2count
        mov ah,9h   
        lea dx,JMWORD[bx] 
        int 21h
        add j2count,63d 
        mov dx,di 
        mov dl,0Ah
        inc dh  
        cmp j2count,693d
        jnz loop2              
                  
        popa
    
    
    
    
    
    
ENDM PRINTSTARTWORD   


                                  ;THIS MACRO IS USED FOR THE READY STEADY GO THING 
READY_STEADY_GO MACRO
          
            ;CLEAR THE SCREEN
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h
             
                       
                       
               PRINTSTATRTWORD READY_WORD   ;CALL MACRO TO PRINT READY MSG       
                   ;make blue background with yellow chars printed
            mov ah,2
            mov dx,184Fh
            int 10h
             
           MOV CX,8
           MOV AH,86H
           INT 15H              ;DELAY TO SEE THE READY WORD
           
           ;CLEAR THE SCREEN
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h
           
           
            MOV CX,7
           MOV AH,86H
           INT 15H     ;ANOTHER DELAY TO SEE THE BLANK SCREEN BETWEEN EVERY WORD 
           
           
           PRINTSTATRTWORD STEADY_WORD    ;PRINT STEADY WORD      
                mov ah,2
     mov dx,184Fh
     int 10h        
                   
           MOV CX,8
           MOV AH,86H
           INT 15H      ;DELAY TO SEE THE STEADDY WORD
           
           ;CLEAR SCREEN
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h
           
           
            MOV CX,7
           MOV AH,86H
           INT 15H       ;DELAY
           
           
           PRINTSTATRTWORD GO_WORD ;PRINT GO WORD         
                 mov ah,2
     mov dx,184Fh
     int 10h      
                   
           MOV CX,8
           MOV AH,86H
           INT 15H    ;DELAY
           
           ;CLEAR SCREEN
           mov ah,6 ; function 6
           mov al,24 ; scroll by 1 line
           mov bh,7 ; normal video attribute
           mov ch,0 ; upper left Y
           mov cl,0 ; upper left X
           mov dh,24 ; lower right Y
           mov dl,79 ; lower right X
           int 10h
           
           
            MOV CX,7
           MOV AH,86H
           INT 15H          ;DELAY
              
              
    
ENDM      READY_STEADY_GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;AMR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Draw MACRO type,position

	LOCAL end_draw  
	LOCAL normal_person
	LOCAL tank_shape
	LOCAL robot_shape
	

	cmp type,'0'
    	jz normal_person
    	
	cmp type,'1'
	jz tank_shape
	
	
	cmp type,'2'
    	jz robot_shape
	
   	normal_person: 
  	draw_shape normal_person_shape,position 
   	jmp end_draw   
    	
	tank_shape: 
  	draw_shape Tank,position 
   	jmp end_draw

	robot_shape:
  	draw_shape Robot,position 
   	jmp end_draw 
   	
	
    
    ;End of drawing
    end_draw:
ENDM Draw 

;---------------------
draw_shape MACRO shape,position
LOCAL loop2
     pusha  
     mov ah,2h
     mov dx,position
     int 10h
     
     mov count,0
     mov bx,0h
     loop2:
        mov ah,2h
        int 10h
        mov di,dx 
        mov bl,count       
        mov ah,9h   
        lea dx,shape[bx] 
        int 21h
        add count,8 
        mov dx,di
        inc dh  
        cmp count,32
        jnz loop2
     
    
    popa
ENDM draw_shape             
;----------------------------
draw_block MACRO shape,position
LOCAL loop2
     pusha  
     mov ah,2h
     mov dx,position
     int 10h
     
     mov count,0
     mov bx,0h
     loop2:
        mov ah,2h
        int 10h
        mov di,dx 
        mov bl,count 
	pusha
	;to color the block with yellow
	mov ah,9h
	mov bh,0h
	mov al,44h
	mov cx,8
	mov bl,01eh
	int 10h
	popa      
        mov ah,9h   
        lea dx,shape[bx] 
        int 21h
        add count,9 
        mov dx,di
        inc dh  
        cmp count,54
        jnz loop2
     
    
    popa
ENDM draw_block          

;-----------------------------
Move MACRO 

LOCAL NOMOVE
LOCAL up
LOCAL up2
LOCAL down
LOCAL down2
LOCAL move1    
LOCAL move2
LOCAL move3
LOCAL move4

    
 
cmp al,'1'      ;Move player 1 up
jz up

cmp al,'2'	;Move player 1 down
jz down
 
cmp al,'3'      ;Move player 2 up
jz up2

cmp al,'4'      ;Move player 2 down
jz down2

jmp NOMOVE ;should not arrive here but in case of any failure  

up:     		 	;Logic of moving player1 up
mov dx,player1_pos  	 	;put in dx the position of player 1
cmp dh,start_drawing_screen	;if the player is already at the top of the screen 
jz NOMOVE			;if yes, dont move
sub dh,speed 	    		;if no, he is at normal position subtract the speed from the position to move
cmp dh,start_drawing_screen	;if after subtracting the speed the player is out of the drawing screen 
jns move1 			;if not negative(means that the player is still inside the screen)Move normally
mov dh,start_drawing_screen 	;if negative,returning the player to the top of the screen  
move1:
mov player1_pos,dx  		;moving the position of player 1
jmp NOMOVE

down:			 	;Logic of moving player1 down
mov dx,player1_pos	 	;put in dx the position of player1
cmp dh,end_drawing_screen 	;if the player at the end of the screen
jz NOMOVE			;if yes, dont move
add dh,speed 			;if no, add speed to the position
cmp dh,end_drawing_screen 	;if after adding the speed the player is out of the drawing screen
jna move2               	;if not above, add the speed normally then move	
mov dh,end_drawing_screen 	;if above (means the player should be at the end of the screen)put the player at the end of the drawing screen
move2:
mov player1_pos,dx 		;change the postion according the previous procedures
jmp NOMOVE 

;same logic was applied on player2 

up2:
mov dx,player2_pos
cmp dh,start_drawing_screen
jz NOMOVE
sub dh,speed 
cmp dh,start_drawing_screen
jns move3
mov dh,start_drawing_screen
move3:
mov player2_pos,dx 
jmp NOMOVE

down2:
mov dx,player2_pos
cmp dh,end_drawing_screen 
jz NOMOVE
add dh,speed
cmp dh,end_drawing_screen 
jna move4
mov dh,end_drawing_screen 
move4:
mov player2_pos,dx

NOMOVE:     
    
    
   
ENDM Move   

             
             
             
PRINTSTATUSBAR MACRO   
    
    
           LOCAL ADELHA
           LOCAL ADELHA2 
           local adehahah
           local adehahah2
    
          MOV AH,2   
          MOV BH,0                 ;CHANGE CURSOR LOCATION
          MOV DX,1300H
          INT 10H
    
          MOV AH,9                    ;DRAW LINE
          MOV BH,0
          MOV AL,95
          MOV CX,80
          MOV BL,00AH
          INT 10H 
     
          MOV AH,2
          MOV BH,0                     ;CHANGE CURSOR LOCATION TO PRINT PLAYER 1 NAME
          MOV DX,1401H
          INT 10H                             
          
          CMP playernum,2
          jz ADELHA
          
          MOV AH,9                      ;DISP PLAYER1 NAME
          MOV DX,OFFSET JPLAYER1NAME[2] 
          INT 21H
          
          
          jmp adehahah
         ADELHA: 
          MOV AH,9                      ;DISP PLAYER1 NAME
          MOV DX,OFFSET JPLAYER2NAME[2] 
          INT 21H
          
         adehahah: 
          
          
    
          MOV AH,2
          MOV BH,0
          MOV DX,143FH                 ;CHANGE CURSOR LOCATION TO PRINT PLAYER2 NAME
          INT 10H
 
          CMP playernum,2
          jz ADELHA2
 
         
 
    
          MOV AH,9                    ;DISP PLAYER 2 NAME
          MOV DX,OFFSET JPLAYER2NAME[2]
          INT 21H     
         
         
         jmp adehahah2       
          ADELHA2:      
                    
          MOV AH,9                    ;DISP PLAYER 2 NAME
          MOV DX,OFFSET JPLAYER1NAME[2]
          INT 21H               
          adehahah2:          
                
          MOV AH,2
          MOV BH,0                    ;CHANGE CURSOR POSITION   
          MOV DX,1501H                                   
          INT 10H                                        
                                                   
          MOV AH,9                                       
          MOV DX,OFFSET HEALTH_MSG           ;WRITE (HEALTH : )   FOR PLAYER 1
          INT 21H 
          
          
          MOV AH,2
          MOV BH,0                    ;CURSOR POSITION   
          MOV DX,153FH                                   
          INT 10H                                        
                                                   
          MOV AH,9                                       
          MOV DX,OFFSET HEALTH_MSG           ;WRITE (HEALTH : )  FOR PLAYER 2
          INT 21H
    
ENDM PRINTSTATUSBAR



draw_shape_WINNER MACRO WSHAPE  ;THIS MACRO IS USED FOR PRINTING THE WINNER SCREEN 
                               ;NOTE SAME AS THE ONE ABOVE IN THE FILE
      LOCAL loop2          
                mov position,060Ah
     
  
      pusha  
     mov ah,2h   
     mov dx,position
     int 10h
     
     mov j2count,0
     mov bx,0h
     loop2:
        mov ah,2h  
        push bx
        mov bh,0
        int 10h 
        pop bx
        mov di,dx  
        mov bx,0
        mov bx,j2count
        mov ah,9h   
        lea dx,WSHAPE[bx] 
        int 21h
        add j2count,63d 
        mov dx,di 
        mov dl,0Ah
        inc dh  
        cmp j2count,945d
        jnz loop2              
                  
        popa
    
     
    
    popa
ENDM draw_shape_WINNER  

DRAW_NOTIF_BAR MACRO  NOTFMSG     ;THS MACRO IS USED TO DRAW STATUS BAR AND A PRINT A MSG IN IT
    
          MOV AH,2   
          MOV BH,0                 ;CHANGE CURSOR LOCATION
          MOV DX,1600H
          INT 10H
    
          MOV AH,9                    ;DRAW LINE
          MOV BH,0
          MOV AL,95
          MOV CX,80
          MOV BL,00AH
          INT 10H 
               
          
          MOV AH,2   
          MOV BH,0                 ;CHANGE CURSOR LOCATION
          MOV DX,1700H
          INT 10H
          
          MOV AH,9                      ;PRINT MSG
          MOV DX,OFFSET NOTFMSG
          INT 21H
          
               
    
ENDM DRAW_NOTIF_BAR