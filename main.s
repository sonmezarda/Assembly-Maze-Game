Stack_Size       EQU     0x400;
	
				 AREA    STACK, NOINIT, READWRITE, ALIGN=3
Stack_Mem        SPACE   Stack_Size
__initial_sp

				 AREA 	programData, DATA, READWRITE
level_values	 DCD 0,0, level ; levelrow, lecelcol
pac_positions    DCD 0,0  		; player pixel row, pacman pixel column
pac_tile		 DCD 0,0  		; player tile row, pacman tile column
level_tile		 DCD 0,0 		; level row x level col
ssize			 EQU 8
lcd_address 	 EQU 0x40010000
direction 		 DCD 0
score 			 DCD 0
coins			 DCD 0,0,0,0 ; stores coins collected or not collected 0=not collected, 1=collected
	
				 AREA    RESET, DATA, READONLY
                 EXPORT  __Vectors
                 EXPORT  __Vectors_End

__Vectors        DCD     __initial_sp               ; Top of Stack
                 DCD     Reset_Handler              ; Reset Handler
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    0
                 DCD    Button_Handler
__Vectors_End

				 AREA    |.text|, CODE, READONLY
Reset_Handler    PROC
                 EXPORT  Reset_Handler
				 ldr	 r0, =0xE000E100
				 movs	 r1,#1
				 str	 r1,[r0]						 
			     CPSIE	 i			
				 BL clear
                 LDR     R0, =__main
                 BX      R0
                 ENDP				 

				 AREA    |.text|, CODE, READONLY
Button_Handler   PROC 					; Calls when any button is pressed.
										; changes direction depends button or restarts the game
                 EXPORT Button_Handler
				 push {lr}
				 stmdb sp!, {r1, r2, r3, r7} ; we used push-pop instructions in almost every branch
				 
				 ldr r7, =lcd_address
				 adds r7, r7, #0x10
				 
				 
                 ldr r2, =0x7FFFFFFF
                 ldr r3, [r7]
                 ands  r3, r3, r2 ; Gets the data of the button pressed with the and operation
				 
				 cmp r3, #0x20 			; up 0x10 left 0x40 right 0x80 down 0x20
				 BEQ pressed_d
				 cmp r3, #0x10
				 BEQ pressed_u 
				 cmp r3, #0x40
				 BEQ pressed_l
				 cmp r3, #0x80
				 BEQ pressed_r
				 cmp r3, #0x08
                 BEQ pressed_b
				 BNE next_butt
;direction changes according to the pressed button. Our character constantly tries to move according to the direction value.
pressed_d		 ldr r3, =direction		
				 movs r1, #4
				 str r1, [r3]
				 B next_butt 
pressed_u		 ldr r3, =direction
				 movs r1, #2
				 str r1, [r3]
				 B next_butt
pressed_l 		 ldr r3, =direction
				 movs r1, #3
				 str r1, [r3]
				 B next_butt
pressed_r 		 ldr r3, =direction
				 movs r1, #1
				 str r1, [r3]
				 B next_butt
pressed_b        ldr r0, =direction
                 movs r1, #5
                 str r1, [r0]
                 B next_butt

next_butt						 
                 ldr r2, =0x80000000
                 str r2, [r7]
				
				 ldmia sp!, {r1, r2, r3, r7}
				 pop {pc}
				 ENDP	

                 AREA    main, CODE, READONLY
                 EXPORT	 __main			 ;make __main visible to linker
				 IMPORT  image
				 IMPORT bottom_straight
				 IMPORT left_b_corner
				 IMPORT left_t_corner
				 IMPORT left_straight
				 IMPORT right_b_corner
				 IMPORT right_straight
				 IMPORT right_t_corner
				 IMPORT level
				 IMPORT finish_tile
				 IMPORT empty_box
				 IMPORT player
				 IMPORT coin
				 IMPORT zero
				 IMPORT one
				 IMPORT two
				 IMPORT three
				 IMPORT four
				 IMPORT score_txt
				 IMPORT win

                 ENTRY
				 
				 
			 

__main           PROC
				 ; Sets our variables and starts loop	
                 ldr     r0, =0x40010000
				 BL		 clear			  	; calls clear subroutine (clears screen)
				 				 
				 ldr 	 r0, =score			; Reset score
				 movs r1, #0
				 str r1, [r0]
				 
				 ldr r0, =coins				; Reset coins (Coin received or not)
				 movs r1, #0
				 str r1, [r0]
				 str r1, [r0, #0x04]
				 str r1, [r0, #0x08]
				 str r1, [r0, #0x0C]
				 
				 ldr r0, =pac_positions		; Resets player positions (as pixel)
				 movs r1, #0
				 str r1, [r0]
				 str r1, [r0, #0x04] 
				 
				 ldr r0, =pac_tile 			; Resets player positions (as tile)
				 movs r1, #0
				 str r1, [r0]
				 str r1, [r0, #0x04]
				 
				 ldr r0, =direction			; Resets player direction
				 movs r1, #1				; 1 means right direction
				 str r1, [r0]
				 
				 ; - set level row and col
				 movs r0, #30
				 movs r1, #40
				 ldr r2, =level_tile
				 str r0, [r2]
				 str r1, [r2, #0x04]
				 BL draw_level				; draws level tiles
				 ;-
				 
				 BL draw_score 				; draws score text and value
				 BL refresh					; refresh screen
				 
				 
				
loop			 	; this loop checks direction, next tile and moves player

					ldr r2, =direction		 ; checks direction
					ldr r2, [r2]
					cmp r2, #5				 ; if direction == 0 it means player pressed B (reset button)
                    BEQ Reset_Handler
					
					ldr r4, =pac_tile
					ldr r0, [r4]
					ldr r1, [r4, #0x04]
					
					push {r2} 				; push direction
					BL check_next_tile 		; r0 = current row, r1 = current col, r2=direction ; checks is next tile empty or not
					pop {r2} 				; r2 = direction
					CMP r0, #0 				; r0 = 0 empty
					BEQ empty
					B not_empty
					
					
empty				movs r0, r2				; if next tile is empty (or movable like coins) move player
					BL move_pac_tile  		; r0=direction ; moves player
not_empty					
					
					 b loop
					 ENDP
				 

draw_pix		PROC ; r0 = row, r1 = col, r2 = color 
					 ; draws 1 pixel to given address and color
				push {lr}
				push {r3}
				
				ldr r3, =lcd_address
				str r0, [r3]
				str r1, [r3, #0x04]
				str r2, [r3, #0x08]
				
				pop {r3}
 				pop {pc}
				ENDP




draw_img		PROC ; r0 = start row , r1 = start col, r2 = img_address, r3= img boundry (8x8)
					 ; draws given image to given address
				push {lr}
				stmdb sp!, {r0,r1,r2,r3,r4, r5, r6, r7}
				movs r4, r1 		; r4 = start column
				adds r5, r0, r3 	; boundry + start row = stop row
				adds r6, r1, r3 	; boundry + start col = stop col
				
draw		    movs r7, r2
				ldr r2, [r2] 		; get pixel color from img
				BL draw_pix 		; draw pixel
				movs r2, r7
				adds r1, r1, #1 	; get next column
				adds r2, r2, #0x4 	; get next pixel 
				CMP r1, r6 			; r1 == r6 (column == boundry)
				BNE draw 
				adds r0, r0, #1 	; get next row
				movs r1, r4 		; set current column as start column
				CMP r0, r5 			; r0 == r5 (row == boundry)
				BNE draw
				
				ldmia sp!, {r0,r1,r2,r3,r4, r5, r6, r7}
				pop {pc}
				ENDP

draw_level 	    PROC ;r0 = level row count, r1 = level col count
					 ; draws all level tile by tile
				push {lr}
				stmdb sp!, {r4, r5, r6, r7}
				ldr r6, =level
				movs r4, r1 	; r4 = last col 
				movs r3, r0 	; r3 = last row
				movs r0, #0
				movs r1, #0
		
draw_tile
				 ldr  r5, [r6]  ; get box data from level address
				 ; check which tile is
				 CMP r5, #0
				 BEQ em
				 CMP r5, #1
				 BEQ bs
				 CMP r5, #2
				 BEQ ts
				 CMP r5, #3
				 BEQ ls
				 CMP r5, #4
				 BEQ rs
				 CMP r5, #5
				 BEQ lbc
				 CMP r5, #6
				 BEQ rbc
				 CMP r5, #7
				 BEQ ltc
				 CMP r5, #8
				 BEQ rtc
				 CMP r5, #99
				 BEQ draw_coin
				 CMP r5, #98
				 BEQ draw_coin
				 CMP r5, #97
				 BEQ draw_coin
				 CMP r5, #96
				 BEQ draw_coin
bs				 ldr r2, =bottom_straight
				 B dr
ts				 ldr r2, =finish_tile
				 B dr
ls				 ldr r2, =left_straight
				 B dr
rs				 ldr r2, =right_straight
				 B dr
lbc				 ldr r2, =left_b_corner
				 B dr
rbc				 ldr r2, =right_b_corner
				 B dr
ltc				 ldr r2, =left_t_corner
				 B dr
rtc				 ldr r2, =right_t_corner
				 B dr
draw_coin 		 ldr r2, =coin
				 B dr
em				 ldr r2, =empty_box
				 
dr			 
				ldr r7, =ssize 		; r7 = box_size
				stmdb sp!, {r0, r1} 
				muls r0, r7, r0
				muls r1, r7, r1		; get right pixel (tile * tile pixel)
				push {r3}
				movs r3, r7
				BL draw_img  ; r2 = img address, r3 = img boundry, r0 = row, r1 = col 
							 ; draws tile
				pop {r3}
				ldmia sp!, {r0, r1}
				adds r1, r1, #1	 ; add 1 to col
				adds r6, r6, #0x04
				CMP r1, r4
				BNE draw_tile	 ; If not reached to last column 
				adds r0, r0, #1	 ; add 1 to row
				movs r1, #0		 ; set column counter to 0
				CMP r0, r3			
				BNE draw_tile	; if not reached to last row
				
				ldmia sp!, {r4, r5, r6, r7}
				pop {pc}
				ENDP

draw_score      PROC
				; draws score text and value
				push {lr}
				stmdb sp!, {r0, r1, r2, r3, r4, r5, r6, r7}
				
				; set score text position
				ldr r1, =238 	
				movs r0, #5
				movs r3, #65
				ldr r2, =score_txt
				BL draw_img  ; r2 = img address, r3 = img boundry, r0 = row,	r1 = col
				
				; set score value position
					ldr r1, =260
                    movs r0, #40
                    movs r3, #25
					
					ldr r4,=score
					ldr r4, [r4]
					cmp r4, #0
					beq draw_zero
					cmp r4, #1
					beq draw_one
					cmp r4, #2
					beq draw_two
					cmp r4, #3
					beq draw_three
					cmp r4, #4
					beq draw_four

					;bne draw_img
					
draw_zero			ldr r2, =zero
					BL draw_img
					B draw_ok
draw_one			ldr r2, =one
					BL draw_img
					B draw_ok
draw_two			ldr r2, =two
					BL draw_img
					B draw_ok
draw_three			ldr r2, =three
					BL draw_img
					B draw_ok
draw_four			ldr r2, =four
					BL draw_img
					B draw_ok


draw_ok				
				ldmia sp!, {r0, r1, r2, r3, r4, r5, r6, r7}
				pop {pc}
				ENDP


draw_black		 PROC ; r0 = start row, r1 = start col, r2 = stop row, r3 = stop col
					  ; draws black to given position with given size
					  ; it used for delete player pixels when it moves
				 push {lr}
				 stmdb sp!, {r4, r5, r6, r7}
				 
				 ldr r4, =lcd_address
				 ldr r5, =0xFF000000 		; FF000000 : black
				 movs r6, r1		        ; r6 = start column
				 
draw_p			 str r0, [r4]
				 str r1, [r4, #0x04]
				 str r5, [r4, #0x08]
				 
				 adds r1, r1, #1
				 CMP r1, r3
				 BNE draw_p
				 movs r1, r6
				 adds r0, r0, #1
				 CMP r0, r2
				 BNE draw_p
				 
				 ldmia sp!, {r4, r5, r6, r7}
				 pop {pc}
				 ENDP
					 
					 
move_pac_tile  	PROC
				; moves player to 1 tile by direciton
				push {lr}
				stmdb sp!, {r4, r5, r6, r7}
				
				ldr r0, =direction	; get direction
				ldr r0, [r0]
				
				
				CMP r0, #0
				BEQ pac_m_n
				
				
				BL move_pac
				BL move_pac
				BL move_pac
				BL move_pac
				BL refresh
				BL move_pac
				BL move_pac
				BL move_pac
				BL move_pac
				BL refresh
				
				ldr r4, =pac_tile
				ldr r5, [r4] 		; r5 = pac_tile_row 
				ldr r6, [r4, #0x04] ; r6 = pac_tile_col
				
				CMP r0, #0		 	; checks direction
				BEQ pac_m_n
				CMP r0, #1
				BEQ pac_m_r
				CMP r0, #2
				BEQ pac_m_u
				CMP r0, #3
				BEQ pac_m_l
				CMP r0, #4
				BEQ pac_m_d

pac_m_r			adds r6, r6, #1		; adds or subs to row or column by direction ( for example subs 1 to column for left move)
				str r6, [r4, #0x04]
				B pac_m_n
pac_m_u			subs r5, r5, #1
				str r5, [r4]
				B pac_m_n
pac_m_l			subs r6, r6, #1
				str r6, [r4, #0x04]
				B pac_m_n
pac_m_d			adds r5, r5, #1
				str r5, [r4]
				B pac_m_n
pac_m_n	
				ldmia sp!, {r4, r5, r6, r7}
				pop {pc}
				ENDP


check_next_tile PROC ; r0 = current row, r1 = current col, r2=direction
					 ; 1 = right 2= up 3=left 4=down 0=no movement
					 ; checks next tile and returns is tile movable ot not
				push {lr}
				stmdb sp!, {r4, r5, r6, r7}
				
				ldr r4, =level 				; r4 = level address
				ldr r5, =level_tile
				ldr r6, [r5] 				; r6 = level row count
				ldr r7, [r5, #0x04] 		; r7 = level col count
				
				cmp r2, #0
				BEQ check_next
				cmp r2, #1
				BEQ check_r
				cmp r2, #2
				BEQ check_u
				cmp r2, #3
				BEQ check_l
				cmp r2, #4
				BEQ check_d
				
				
check_r			adds r1, r1, #1
				B check_next
check_u         subs r0, r0, #1
				B check_next
check_l			subs r1, r1, #1
				B check_next
check_d			adds r0, r0, #1
				B check_next
check_next
				muls r7, r0, r7
				adds r7, r7, r1 ; r6 = level tile address
				lsls r7, #2
				adds r4, r4, r7
				ldr r5, [r4] ; r5 = level tile which pacman in
				
				
				cmp r5, #0 ; r5 == 0
				BEQ can_move
				cmp r5, #99
				BEQ get_coin1
				cmp r5, #98
				BEQ get_coin2
				cmp r5, #97
				BEQ get_coin3
				cmp r5, #96
				BEQ get_coin4
				cmp r5, #2 
				BEQ finish
				

cant_move		movs r0, #2		; if cant move return next tile id
				ldmia sp!, {r4, r5, r6, r7}
				pop {pc}
				
					
get_coin1		ldr r0, =coins
				ldr r1, [r0] 	; r1 = first coin !is_collected
				CMP r1, #0		; 
				BNE can_move
				ldr r0, =score
				ldr r1, [r0]
				adds r1, r1, #1
				str r1, [r0]
				ldr r0, =coins
				movs r1, #1
				str r1, [r0]
				movs r0, #0
				BL draw_score
				BL refresh
				B can_move
				
get_coin2		ldr r0, =coins
				ldr r1, [r0, #0x04] 	; r1 = first coin !is_collected
				CMP r1, #0		; 
				BNE can_move
				ldr r0, =score
				ldr r1, [r0]
				adds r1, r1, #1
				str r1, [r0]
				ldr r0, =coins
				movs r1, #1
				str r1, [r0, #0x04]
				movs r0, #0
				BL draw_score
				BL refresh
				B can_move
				
get_coin3		ldr r0, =coins
				ldr r1, [r0, #0x08] 	; r1 = first coin !is_collected
				CMP r1, #0		; 
				BNE can_move
				ldr r0, =score
				ldr r1, [r0]
				adds r1, r1, #1
				str r1, [r0]
				ldr r0, =coins
				movs r1, #1
				str r1, [r0, #0x08]
				movs r0, #0
				BL draw_score
				BL refresh
				B can_move
				
get_coin4		ldr r0, =coins
				ldr r1, [r0, #0x0C] 	; r1 = first coin !is_collected
				CMP r1, #0		; 
				BNE can_move
				ldr r0, =score
				ldr r1, [r0]
				adds r1, r1, #1
				str r1, [r0]
				ldr r0, =coins
				movs r1, #1
				str r1, [r0, #0x0C]
				movs r0, #0
				BL draw_score
				BL refresh
				B can_move
				
can_move		
				movs r0, #0	; if can move returns 0
				ldmia sp!, {r4, r5, r6, r7}
				pop {pc}
				ENDP

finish			
				BL clear
				BL refresh
				movs r0, #45
				movs r1, #85
				ldr r2, =win
				movs r3,#150
				BL draw_img; r0 = start row , r1 = start col, r2 = img_address, r3= img boundry (8x8)
				BL refresh
fin				B fin


move_pac		PROC ; r0 = direction
				;1 = right 2= up 3=left 4=down 0=no movement
				push {lr}
				stmdb sp!, {r0, r4, r5, r6, r7}
	
draw_pac		movs r7, r0 ; r7 = direction
				ldr r6, =pac_positions
				ldr r0, [r6] 		; r0 = pac position row
				ldr r1, [r6, #0x04]
				movs r3, #8
				ldr r2, =player
				BL draw_img ; r0 = start row , r1 = start col, r2 = img_address, r3= img boundry (8x8)
				
				CMP r7, #0
				BEQ next
				CMP r7, #1
				BEQ pac_r_m
				CMP r7, #2
				BEQ pac_u_m
				CMP r7, #3
				BEQ pac_l_m
				CMP r7, #4
				BEQ pac_d_m		

				; draws player to the next pixel depends on given direction
				; and deletes previous pixels (draws black)
pac_r_m			ldr r0, [r6] 		; r0 = pac position row
				ldr r1, [r6, #0x04] ; r1 = pac position col
				subs r1, r1, #1
				movs r2, r0 		; r2 = pac pos row
				movs r3, r1 		; r3 = pac pos col
				adds r2, r2, #8 	; r2 = r2*8 : black row stop
				adds r3, r3, #1		; r3 = r3*1 : black col stop
 				BL draw_black 		; r0 = start row, r1 = start col, r2 = stop row, r3 = stop col
				ldr r1, [r6, #0x04]
				adds r1, r1, #1
				str r1, [r6, #0x04]
				B next
				
pac_u_m			ldr r0, [r6] 		; r0 = pac position row
				ldr r1, [r6, #0x04] ; r1 = pac position col
				adds r0, r0, #8
				movs r2, r0 		; r2 = pac pos row
				movs r3, r1 		; r3 = pac pos col
				adds r2, r2, #1 	; r2 = r2*8 : black row stop
				adds r3, r3, #8 	; r3 = r3*1 : black col stop
 				BL draw_black 		; r0 = start row, r1 = start col, r2 = stop row, r3 = stop col
				ldr r0, [r6]
				subs r0, r0, #1
				str r0, [r6]
				B next

pac_l_m			ldr r0, [r6] 		; r0 = pac position row
				ldr r1, [r6, #0x04] ; r1 = pac position col
				adds r1, r1, #8
				movs r2, r0 		; r2 = pac pos row
				movs r3, r1 		; r3 = pac pos col
				adds r2, r2, #8 	; r2 = r2*8 : black row stop
				adds r3, r3, #1 	; r3 = r3*1 : black col stop
 				BL draw_black ; r0 = start row, r1 = start col, r2 = stop row, r3 = stop col
				ldr r1, [r6, #0x4]
				subs r1, r1, #1
				str r1, [r6, #0x04]
				B next
				
pac_d_m			ldr r0, [r6] 		; r0 = pac position row
				ldr r1, [r6, #0x04] ; r1 = pac position col
				subs r0, r0, #1
				movs r2, r0 		; r2 = pac pos row
				movs r3, r1 		; r3 = pac pos col
				adds r2, r2, #1 	; r2 = r2*8 : black row stop
				adds r3, r3, #8 	; r3 = r3*1 : black col stop
 				BL draw_black 		; r0 = start row, r1 = start col, r2 = stop row, r3 = stop col
				ldr r0, [r6]
				ldr r1, [r6, #0x04]
				adds r0, r0, #1
				str r0, [r6]
				B next
next			
				ldmia sp!, {r0, r4, r5, r6, r7}
				pop {pc}
				ENDP
				 
refresh			 PROC			     ; refresh screen (write 0x1 to the LCD control register)
				 push {r0, r1} 
				 ldr r0, =lcd_address
			     movs r1, #1
				 str  r1, [r0, #0xC]
				 pop {r0, r1}
				 BX lr
				 ENDP

clear			 PROC	
				 stmdb sp!, {r0, r1}
				 ldr r0, =lcd_address	; clear and refresh screen (write 0x3 to the LCD control register)
				 MOVS r1, #3
				 STR  r1, [r0, #0xC]
				 ldmia sp!, {r0, r1}
				 BX LR
				 ENDP

wait			 PROC				 ; wait between steps 
			     ldr r7, =0xFF00FF00 ; Set wait amount
neq				 SUBS r7, r7, #1				 
				 CMP r7, #0
				 BNE neq
				 BX LR
                 ENDP
					 
			     
                 END