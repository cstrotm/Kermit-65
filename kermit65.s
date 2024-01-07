; TITLE  KERMIT-65	KL10 Error-free Reciprocal Micro-interface Transfer
;
;	6502 version - Antonino N. J. Mione
;	Commodore 64 version converted from Apple version 1.1
;	By Dave Dermott  March, 1984
;	Additional improvements by Eric Lavitsky/Frank Prindle/
;	Michael Marchiondo/Ray Moody
;
;	Atari 800 version converted from Commodore version by 
;	John Dunning.
;
;	Version 3.7 
;
;.SBTTL	Revision History
 
;
; Date		Description
; ------	-----------
; 4/87 - 7/87	By JRD.  Found the Commodore version and went at it with 
;		a meat cleaver.  Made changes too numerous to detail here;
;		replaced all io with calls to CIO; revamped screen handling
;		to use CTIA instead of VIC, cleaned up user interface, fixed
;		comm port handling to work with 850, ad nauseum.
;
; ------	v3.1 released here
;
; 8/10/87 - 8/17/87
;		Fixed binary mode.  Fixed eight-bit-quoting negotiation.
;		Make erase and rename ensure rs port closed.  Fix stupid
;		but insidious bug in terminal code that caused graphics
;		mode to be turned on spuriously when processing a BS
;		in column 15 (!)
;
; ------	v 3.2 released here
;
; 8/18/87 - 
;		Finish wildcard support in pathnames.  Fix file reader
;		to deal with Spartados not adhering to spec.  Fix all
;		parameter display/entry to use decimal where apropriate,
;		ie in timeout values.  Display values in 'Status' display
;		in decimal.  Implement Atari key (/|\) as input-suspend.
;		Fix bug in flow-control that caused it to wedge up
;		unexpectedly.  Clean up 80-column font per JHS's comments.
;		Removed Speedscript file type.  Fix stack-corruption
;		in terminal code.  Clean up some of the really obscure
;		cases in vt100 mode.
;		
; 4/11/90 - jrd
;		Fix bug in Spartados EOF handling.  Remove assumption
;		about screen width, to allow XEP80 to work.
;
; release v 3.6.
;
; 5/28/90 - jrd
;		Fix the stupid D8: bug again.
;
; release v 3.7.
;
;
;----------------------------------------------------------------
;
; Things that need doing:
;
; The telnet loop is a real mess, needs to be flushed and redone completely.
;
; Rip out old debugging code.
;
; Parameterize Serial io calls so that we can run smoother with interfaces 
; other than 850
;
; Finish making all the internal pathname hacking stuff really use 
; pathnames, not just strings.
;
; Implement 'Remote ... ' command.
;
; Implement some facility for defining function keys.
;
;+
 
;
;----------------------------------------------------------------
;
; Defs for running this thing on an atari
;
kbdchan	=	$10		; keyboard IOCB
scrchan =	$00		; screen (E:) IOCB
comchan =	$30		; serial port
dskchan =	$40		; disk
dirchan =	$50		; special one for directory access
;
;.SBTTL	Jump to start of code
 
;kst:	jmp	kstart		; Go past the data to the beginning of the code
; on atari, just say where to start
; but with ca65, it done differently (cpg)
;	*=	$2E0
;	.word	kstart
 
;
;	now start at a reasonable place
;
;(cpg)	*=	$2D20		; about as low as reasonable on atari

		.org	$2D20	; needed that newaddr works
		.code		; currently everything in code segment

;
;.SBTTL	Character and string definitions
; 
nul	=	$00		; <null>
soh	=	$01		; <soh>
bel	=	$07		; c-g
bs	=	$08		; <bs>
tab	=	$09		; <tab> (ctrl/I)
lf	=	$0A		; <lf>
ffd	=	$0C		; Form feed
cr	=	$0D		; <cr>
so	=	$0E		; <shift-out>
si	=	$0F		; <shift-in>
ctrlu	=	$15		; <ctrl/U>
ctrlx	=	$18		; <ctrl/X>
ctrly	=	$19		; <ctrl/Y>
esc	=	$1B		; <esc>
sp	=	$20		; <space>
space	=	$20		; " "
del	=	$7F		; <del>
quest	=	$3F		; <?>
ctrlw	=	$17		; <ctrl/W>
dquot	=	$22		; '"'		?
quot	=	$27		; "'"		?
slash	=	$2F		; '/'		?
apos	=	quot		; "'"		?
rabr	=	$3E		; '>'		?
colon	=	$3A		; ':'		?
 
;----------------------------------------------------------------
; Atari OS defs from SYSMAC.SML inserted here
;

;	VECTOR TABLE

EDITRV	=$E400			;EDITOR
SCRENV	=$E410			;TELEVISION SCREEN
KEYBDV	=$E420			;KEYBOARD
PRINTV	=$E430			;PRINTER
CASETV	=$E440			;CASSETTE

;	JUMP VECTOR TABLE

DISKIV	=$E450			;DISK INITIALIZATION
DSKINV	=$E453			;DISK INTERFACE
CIOV	=$E456			;CIO ROUTINE
SIOV	=$E459			;SIO ROUTINE
SETVBV	=$E45C			;SET VERTICAL BLANK VECTORS
SYSVBV	=$E45F			;SYSTEM VERTICAL BLANK ROUTINE
XITVBV	=$E462			;EXIT VERTICAL BLANK ROUTINE
SIOINV	=$E465			;SIO INIT
SENDEV	=$E468			;SEND ENABLE ROUTINE
INTINV	=$E46B			;INTERRUPT HANDLER INIT
CIOINV	=$E46E			;CIO INIT
BLKBDV	=$E471			;BLACKBOARD MODE
WARMSV	=$E474			;WARM START ENTRY POINT
COLDSV	=$E477			;COLD START ENTRY POINT
RBLOKV	=$E47D			;CASSETTE READ BLOCK VECTOR
DSOPIV	=$E480			;CASSETTE OPEN FOR INPUT VECTOR

;	SOME USEFUL INTERNAL ROUTINES

;KGETCH	=$F6E2			;GET CHAR FROM KEYBOARD only on 800
EOUTCH	=$F6A4			;OUTPUT CHAR TO SCREEN
PUTLIN	=$F385			;OUTPUT LINE TO IOCB#0

;	COMMAND CODES FOR IOCB

OPEN	=$03			;OPEN FOR INPUT/OUTPUT
GETREC	=$05			;GET RECORD (TEXT)
GETCHR	=$07			;GET CHARACTER(S)
PUTREC	=$09			;PUT RECORD (TEXT)
PUTCHR	=$0B			;PUT CHARACTER(S)
CLOSE	=$0C			;CLOSE DEVICE
STATIS	=$0D			;STATUS REQUEST
SPECIL	=$0E			;SPECIAL ENTRY COMMANDS

;	SPECIAL ENTRY COMMANDS

DRAWLN	=$11			;DRAW LINE
FILLIN	=$12			;DRAW LINE WITH RIGHT FILL
RENAME	=$20			;RENAME DISK FILE
DELETE	=$21			;DELETE DISK FILE
FORMAT	=$22			;FORMAT DISK
LOCKFL	=$23			;LOCK FILE (READ ONLY)
UNLOCK	=$24			;UNLOCK FILE
POINT	=$25			;POINT SECTOR
NOTE	=$26			;NOTE SECTOR

CCIO	=$28			;CONCURRENT I/O MODE

IOCFRE	=$FF			;IOCB "FREE"

;	AUX1 VALUES FOR OPEN

APPEND	=$01			;OPEN FOR APPEND
DIRECT	=$02			;OPEN FOR DIRECTORY ACCESS
OPNIN	=$04			;OPEN FOR INPUT
OPNOT	=$08			;OPEN FOR OUTPUT
OPNINO	=OPNIN|OPNOT		;OPEN FOR INPUT/OUTPUT
MXDMOD	=$10			;OPEN FOR MIXED MODE
INSCLR	=$20			;OPEN WITHOUT CLEARING SCREEN

; OS STATUS CODES

SUCCES	=$01			;SUCCESSFUL OPERATION
BRKABT	=$80			;(128) BREAK KEY ABORT
PRVOPN	=$81			;(129) IOCB ALREADY OPEN
NONDEV	=$82			;(130) NON-EX DEVICE
WRONLY	=$83			;(131) IOCB OPENED FOR WRITE ONLY
NVALID	=$84			;(132) INVALID COMMAND
NOTOPN	=$85			;(133) DEVICE OR FILE NOT OPEN
BADIOC	=$86			;(134) INVALID IOCB NUMBER
RDONLY	=$87			;(135) IOCB OPENED FOR READ ONLY
EOFERR	=$88			;(136) END OF FILE
TRNRCD	=$89			;(137) TRUNCATED RECORD
TIMOUT	=$8A			;(138) DEVICE TIMEOUT
DNACK	=$8B			;(139) DEVICE DOES NOT ACK COMMAND
FRMERR	=$8C			;(140) SERIAL BUS FRAMING ERROR
CRSROR	=$8D			;(141) CURSOR OUT OF RANGE
OVRRUN	=$8E			;(142) SERIAL BUS DATA OVERRUN
CHKERR	=$8F			;(143) SERIAL BUS CHECKSUM ERROR
DERROR	=$90			;(144) DEVICE ERROR (OPERATION INCOMPLETE)
BADMOD	=$91			;(145) BAD SCREEN MODE NUMBER
FNCNOT	=$92			;(146) FUNCTION NOT IN HANDLER
SCRMEM	=$93			;(147) INSUFFICIENT MEMORY FOR SCREEN MODE

;	PAGE 0 LOCATIONS

LINZBS	=$00			;LINBUG STORAGE
 
;  THESE LOCS ARE NOT CLEARED

CASINI	=$02			;CASSETTE INIT LOC
RAMLO	=$04			;RAM POINTER FOR MEM TEST
TRAMSZ	=$06			;TEMP LOC FOR RAM SIZE
TSTDAT	=$07			;RAM TEST DATA LOC

;  CLEARED ON COLDSTART ONLY

WARMST	=$08			;WARM START FLAG
BOOTQ	=$09			;SUCCESSFUL BOOT FLAG
DOSVEC	=$0A			;DOS START VECTOR
DOSINI	=$0C			;DOS INIT ADDRESS
APPMHI	=$0E			;APPLICATION MEM HI LIMIT

;  CLEARED ON COLD OR WARM START

INTZBS	=$10			; START OF OS RAM CLEAR LOC => $7F
POKMSK	=$10			;SYSTEM MASK FOR POKEY IRQ ENABLE
BRKKEY	=$11			;BREAK KEY FLAG
RTCLOK	=$12			;REAL TIME CLOCK (60HZ OR 16.66666 MS)
				; 3 bytes; hi order, medium, low
BUFADR	=$15			;INDIRECT BUFFER ADDRESS REG
ICCOMT	=$17			;COMMAND FOR VECTOR HANDLER
DSKFMS	=$18			;DISK FILE MANAGER POINTER
DSKUTL	=$1A			;DISK UTILITIES POINTER
PTIMOT	=$1C			;PRINTER TIME OUT REGISTER
PBPNT	=$1D			;PRINT BUFFER POINTER
PBUFSZ	=$1E			;PRINT BUFFER SIZE
PTEMP	=$1F			;TEMP REG
ZIOCB	=$20			;PAGE 0 I/O CONTROL BLOCK
IOCBSZ	=16			;NUMBER OF BYTES / IOCB
MAXIOC	=8*IOCBSZ		;LENGTH OF IOCB AREA
IOCBAS	=ZIOCB

ICHIDZ	=$20			;HANDLER INDEX NUMBER ($FF := IOCB FREE)
ICDNOZ	=$21			;DEVICE NUMBER (DRIVE NUMBER)
ICCOMZ	=$22			;COMMAND CODE
ICSTAZ	=$23			;STATUS OF LAST IOCB ACTION
ICBALZ	=$24			;BUFFER ADDRESS (LOW)
ICBAHZ	=$25			;  "	   "	(HIGH)
ICPTLZ	=$26			;PUT BYTE ROUTINE ADDRESS - 1
ICPTHZ	=$27
ICBLLZ	=$28			;BUFFER LENGTH (LOW)
ICBLHZ	=$29			;  "	   "   (HIGH)
ICAX1Z	=$2A			;AUX INFO
ICAX2Z	=$2B
ICSPRZ	=$2C			;SPARE BYTES (CIO LOCAL USE)
ICIDNO	=ICSPRZ+2		;IOCB LUMBER * 16
CIOCHR	=ICSPRZ+3		;CHARACTER BYTE FOR CURRENT OPERATION

STATUS	=$30			;INTERNAL STATUS STORAGE
CHKSUM	=$31			;CHECKSUM (SINGLE BYTE SUM WITH CARRY)
BUNRLO	=$32			;POINTER TO DATA BUFFER (LO BYTE)
BUFRHI	=$33			;POINTER TO DATA BUFFER (HI BYTE)
BFENLO	=$34			;NEXT BYTE PAST END OF BUFFER (LO BYTE)
BNENHI	=$35			;NEXT BYTE PAST END OF BUFFER (HI BYTE)
CRETRY	=$36			;NUMBER OF COMMAND FRAM RETRIES
DRETRY	=$39			;NUMBER OF DEVICE RETRIES
BUFRFL	=$38			;DATA BUFFER FULL FLAG
RECVDN	=$39			;RECEIVE DONE FLAG
XMTDON	=$3A			;XMIT DONE FLAG
CHKSNT	=$3B			;CHECKSUM SENT FLAG
NOCKSM	=$3C			;NO CHECKSUM FOLLOWS DATA FLAG

BPTR	=$3D			;BUFFER POINTER (CASSETTE)
FTYPE	=$3E			;FILE TYPE (SHORT IRG/LONG IRG)
FEOF	=$3F			;END OF FILE FLAG (CASSETTE)
FREQ	=$40			;FREQ COUNTER FOR CONSOLE SPEAKER
SOUNDR	=$41			;NOISY I/O FLAG. (ZERO IS QUIET)
CRITIC	=$42			;CRITICAL CODE IF NON-ZERO)

FMSZPG	=$43			;DISK FILE MANAGER SYSTEM STORAGE (7 BYTES)

CKEY	=$4A			;SET WHEN GAME START PRESSED
CASSBT	=$4B			;CASSETTE BOOT FLAG
DSTAT	=$4C			;DISPLAY STATUS
ATRACT	=$4D			;ATTRACT MODE FLAG
DRKMSK	=$4E			;DARK ATTRACT MASK
COLRSH	=$4F			;ATTRACT COLOR SHIFTER (XOR'D WITH PLAYFIELD)

TMPCHR	=$50			;TEMP CHAR STORAGE (DISPLAY HANDLER)
HOLD1	=$51			;TEMP STG (DISPLAY HANDLER)
LMARGN	=$52			;LEFT MARGIN
RMARGN	=$53			;RIGHT MARGIN
ROWCRS	=$54			;CURSOR COUNTERS
COLCRS	=$55
DINDEX	=$57			;DISPLAY INDEX (VARIOUS QUANTS)
SAVMSC	=$58
OLDROW	=$5A			;PREVIOUS ROW/COL
OLDCOL	=$5B
OLDCHR	=$5D			;DATA UNDER CURSOR
OLDADR	=$5E
NEWROW	=$60			;POINT DRAWS TO HERE
NEWCOL	=$61
LOGCOL	=$63			;POINTS AT COLUMN IN LOGICAL LINE
ADRESS	=$64			;INDIRECT POINTER
MLTTMP	=$66			;MULTIPLY TEMP
OPNTMP	=MLTTMP			;FIRST BYTE IS USED IN OPEN AS TEMP
SAVADR	=$68
RAMTOP	=$6A			;RAM SIZE DEFINED BY POWER ON LOGIC
BUFCNT	=$6B			;BUFFER COUNT
BUFSTR	=$6C			;EDITOR GETCH POINTER
BITMSK	=$6E			;BIT MASK
SHFAMT	=$6F			;OUTCHR SHIFT

ROWAC	=$70			;USED BY "DRAW"
COLAC	=$72
ENDPT	=$74
DELTAR	=$76
DELTAC	=$77
ROWINC	=$79
COLINC	=$7A
SWPFLG	=$7B			;NON-0 IF TXT AND RAM SWAPPED
HOLDCH	=$7C			;CH BEFORE CNTL & SHFT PROCESSING IN KGETCH
INSDAT	=$7D			;INSERT CHAR SAVE
COUNTR	=$7E			;DRAW COUNTER

;;;	$80 TO $FF ARE RESERVED FOR USER APPLICATIONS

;	PAGE 2 LOCATIONS

INTABS	=$200			;INTERRUPT TABLE
VDSLST	=$200			;DISPLAY LIST NMI VECTOR
VPRCED	=$202			;PROCEED LINE IRQ VECTOR
VINTER	=$204			;INTERRUPT LINE IRQ VECTOR
VBREAK	=$206			;"BRK" VECTOR
VKEYBD	=$208			;POKEY KEYBOARD IRQ VECTOR
VSERIN	=$20A			;POKEY SERIAL INPUT READY
VSEROR	=$20C			;POKEY SERIAL OUTPUT READY
VSEROC	=$20E			;POKEY SERIAL OUTPUT DONE
VTIMR1	=$210			;POKEY TIMER 1 IRQ
VTIMR2	=$212			;POKEY TIMER 2 IRQ
VTIMR4	=$214			;POKEY TIMER 4 IRQ (DO NOT USE)
VIMIRQ	=$216			;IMMEDIATE IRQ VECTOR
CDTMV1	=$218			;COUNT DOWN TIMER 1
CDTMV2	=$21A			;COUNT DOWN TIMER 2
CDTMV3	=$21C			;COUNT DOWN TIMER 3
CDTMV4	=$21E			;COUNT DOWN TIMER 4
CDTMV5	=$220			;COUNT DOWN TIMER 5
VVBLKI	=$222			;IMMEDIATE VERTICAL BLANK NMI VECTOR
VVBLKD	=$224			;DEFERRED VERTICAL BLANK NMI VECTOR
CDTMA1	=$226			;COUNT DOWN TIMER 1 JSR ADDRESS
CDTMA2	=$228			;COUNT DOWN TIMER 2 JSR ADDRESS
CDTMF3	=$22A			;COUNT DOWN TIMER 3 FLAG
SRTIMR	=$22B			;SOFTWARE REPEAT TIMER
CDTMF4	=$22C			;COUNT DOWN TIMER 4 FLAG
INTEMP	=$22D			;IAN'S TEMP (???)
CDTMF5	=$22E			;COUNT DOWN TIMER 5 FLAG
SDMCTL	=$22F			;SAVE DMACTL REGISTER
DMACTL	=$D400			; the real DMA control reg
SDLSTL	=$230			;SAVE DISPLAY LIST (LOW)
SDLSTH	=$231			;SAVE DISPLAY LIST (HIGH)
SSKCTL	=$232			;SKCTL REGISTER RAM

LPENH	=$234			;LIGHT PEN HORIZ VALUE
LPENV	=$235			;LIGHT PEN VERT VALUE
				; ($236 - $239 SPARE)
CDEVIC	=$23A			;COMMAND FRAME BUFFER - DEVICE
CCOMND	=$23B			;COMMAND
CAUX1	=$23C			;COMMAND AUX BYTE 1
CAUX2	=$23D			;COMMAND AUX BYTE 2
TEMP	=$23E			;YES
ERRFLG	=$23F			;ERROR FLAG - ANY DEVICE ERROR EXCEPT TIMEOUT

DFLAGS	=$240			;DISK FLAGS FROM SECTOR ONE
DBSECT	=$241			;NUMBER OF DISK BOOT SECTORS
BOOTAD	=$242			;ADDRESS FOR DISK BOOT LOADER
COLDST	=$244			;COLDSTART FLAG (1 = DOING COLDSTART)
				;($245 SPARE)
DSKTIM	=$246			;DISK TIME OUT REG
LINBUF	=$247			;CHAR LINE BUFFER (40 BYTES)

GPRIOR	=$26F			;GLOBAL PRIORITY CELL
PADDL0	=$270			;POT 0 SHADOW
PADDL1	=$271			;POT 1 SHADOW
PADDL2	=$272			;POT 2 SHADOW
PADDL3	=$273			;POT 3 SHADOW
PADDL4	=$274			;POT 4 SHADOW
PADDL5	=$275			;POT 5 SHADOW
PADDL6	=$276			;POT 6 SHADOW
PADDL7	=$277			;POT 7 SHADOW
STICK0	=$278			;JOYSTICK 0 SHADOW
STICK1	=$279			;JOYSTICK 1 SHADOW
STICK2	=$27A			;JOYSTICK 2 SHADOW
STICK3	=$27B			;JOYSTICK 3 SHADOW
PTRIG0	=$27C			;PADDLE 0 TRIGGER
PTRIG1	=$27D			;PADDLE 1 TRIGGER
PTRIG2	=$27E			;PADDLE 2 TRIGGER
PTRIG3	=$27F			;PADDLE 3 TRIGGER
PTRIG4	=$280			;PADDLE 4 TRIGGER
PTRIG5	=$281			;PADDLE 5 TRIGGER
PTRIG6	=$282			;PADDLE 6 TRIGGER
PTRIG7	=$283			;PADDLE 7 TRIGGER
STRIG0	=$284			;JOYSTICK 0 TRIGGER
STRIG1	=$285			;JOYSTICK 1 TRIGGER
STRIG2	=$286			;JOYSTICK 2 TRIGGER
STRIG3	=$287			;JOYSTICK 3 TRIGGER

CSTAT	=$288			;(UNUSED)
WMODE	=$289			;R/W FLAG FOR CASSETTE
BLIM	=$28A			;BUFFER LIMIT (CASSETTE)
				;($28B - $28F SPARE)
TXTROW	=$290			;TEXT ROWCRS
TXTCOL	=$291			;TEXT ROWCOL
TINDEX	=$293			;TEXT INDEX
TXTMSC	=$294			;FOOLS CONVRT INTO NEW MSC
TXTOLD	=$296			;OLDROW & OLDCOL FOR TEXT (AND THEN SOME)
TMPX1	=$29C
HOLD3	=$29D
SUBTMP	=$29E
HOLD2	=$29F
DMASK	=$2A0
TMPLBT	=$2A1
ESCFLG	=$2A2			;ESCAPE FLAG
TABMAP	=$2A3			;TAB BUFFER
LOGMAP	=$2B2			;LOGICAL LINE START BIT MAP
INVFLG	=$2B6			;INVERSE VIDEO FLAG (ATARI KEY)
FILFLG	=$2B7			;RIGHT FILL FLAG FOR DRAW
TMPROW	=$2B8
TMPCOL	=$2B9
SCRFLG	=$2BB			;SET IF SCROLL OCCURS
HOLD4	=$2BC			;MORE DRAW TEMPS
HOLD5	=$2BD
SHFLOK	=$2BE			;SHIFT LOCK KEY
BOTSCR	=$2BF			;BOTTOM OF SCREEN (24 NORM, 4 SPLIT)

PCOLR0	=$2C0			;P0 COLOR
PCOLR1	=$2C1			;P1 COLOR
PCOLR2	=$2C2			;P2 COLOR
PCOLR3	=$2C3			;P3 COLOR
COLOR0	=$2C4			;COLOR 0
COLOR1	=$2C5
COLOR2	=$2C6
COLOR3	=$2C7
COLOR4	=$2C8			;BACKGROUND
				;($2C9 - $2DF SPARE)
GLBABS	=$2E0			;GLOBAL VARIABLES
				;($2E0 - $2E3 SPARE)
RAMSIZ	=$2E4			;RAM SIZE (HI BYTE ONLY)
MEMTOP	=$2E5			;TOP OF AVAILABLE MEMORY
MEMLO	=$2E7			;BOTTOM OF AVAILABLE MEMORY
				;($2E9 SPARE)
DVSTAT	=$2EA			;STATUS BUFFER
CBAUDL	=$2EE			;CASSETTE BAUD RATE (LO BYTE)
CBAUDH	=$2EF			;   "      "    "   (HI BYTE)
CRSINH	=$2F0			;CURSOR INHIBIT (00 = CURSOR ON)
KEYDEL	=$2F1			;KEY DELAY
CH1	=$2F2
CHACT	=$2F3			;CHACTL REGISTER (SHADOW)
CHBAS	=$2F4			;CHBAS REGISTER (SHADOW)
				;($2F5 - $2F9 SPARE)
CHAR	=$2FA
ATACHR	=$2FB			;ATASCII CHARACTER
CH	=$2FC			;GLOBAL VARIABLE FOR KEYBOARD
FILDAT	=$2FD			;RIGHT FILL DATA (DRAW)
DSPFLG	=$2FE			;DISPLAY FLAG: DISP CONTROLS IF NON-ZERO
SSFLAG	=$2FF			;START/STOP FLAG (CNTL-1) FOR PAGING

;	PAGE 3 LOCATIONS

DCB	=$300			;DEVICE CONTROL BLOCK
DDEVIC	=$300			;BUS I.D. NUMBER
DUNIT	=$301			;UNIT NUMBER
DCOMND	=$302			;BUS COMMAND
DSTATS	=$303			;COMMAND TYPE/STATUS RETURN
DBUFLO	=$304			;DATA BUFFER POINTER
DBUFHI	=$305			; ...
DTIMLO	=$306			;DEVICE TIME OUT IN 1 SEC. UNITS
DUNUSE	=$307			;UNUSED
DBYTLO	=$308			;BYTE COUNT
DBYTHI	=$309			; ...
DAUX1	=$30A			;COMMAND AUXILLARY BYTES
DAUX2	=$30B			; ...

TIMER1	=$30C			;INITIAL TIMER VALUE
ADDCOR	=$30E			;ADDITION CORRECTION
CASFLG	=$30F			;CASSETTE MODE WHEN SET
TIMER2	=$310			;FINAL TIME VALUE (USED TO COMPUTE BAUD RATE)
TEMP1	=$312			;TEMP LOCATIONS
TEMP2	=$314			; ...
TEMP3	=$315			; ...
SAVIO	=$316			;SAVE SERIAL IN DATA PORT
TIMFLG	=$317			;TIME OUT FLAG FOR BAUD RATE CORRECTION
STACKP	=$318			;SIO STACK POINTER SAVE LOC
TSTAT	=$319			;TEMP STATUS LOC

HATABS	=$31A			;HANDLER ADDRESS TABLE 
MAXDEV	=$21			;MAXIMUM HANDLER ADDRESS INDEX

;	IOCB OFFSETS 

IOCB	=$340			;I/O CONTROL BLOCKS
ICHID	=$340			;HANDLER INDEX ($FF = FREE)
ICDNO	=$341			;DEVICE NUMBER (DRIVE NUMBER)
ICCOM	=$342			;COMMAND CODE
ICSTA	=$343			;STATUS
ICBAL	=$344			;BUFFER ADDRESS
ICBAH	=$345			; ...
ICPTL	=$346			;PUT BYTE ROUTINE ADDRESS - 1
ICPTH	=$347			; ...
ICBLL	=$348			;BUFFER LENGTH
ICBLH	=$349			; ...
ICAX1	=$34A			;AUXILLARY INFO
ICAX2	=$34B			; ...
ICSPR	=$34C			;4 SPARE BYTES

PRNBUF	=$3C0			;PRINTER BUFFER
				;($3EA - $3FC SPARE)

;	PAGE 4 LOCATIONS

CASBUF	=$3FD			;CASSETTE BUFFER

; USER AREA STARTS HERE AND GOES TO THE END OF PAGE 5

USAREA	=$480

;
; Other random stuff
;
CONSOL	=	$D01F		; console switches start, select, option
;ATASCII CHARACTER DEFS

ATCLR	=$7D			;CLEAR SCREEN CHARACTER
ATRUB	=$7E			;BACK SPACE (RUBOUT)
ATTAB	=$7F			;TAB
ATEOL	=$9B			;END-OF-LINE
ATDELL	=$9C			; Delete line
ATBEL	=$FD			;CONSOLE BELL
ATURW	=$1C			;UP-ARROW
ATDRW	=$1D			;DOWN-ARROW
ATLRW	=$1E			;LEFT-ARROW
ATRRW	=$1F			;RIGHT-ARROW

; USEFUL VALUES

LEDGE	=2			;LMARGN'S INITIAL VALUE
REDGE	=39			;RMARGN'S INITIAL VALUE

ZPC	=0		;PC CODE FOR ZERO PAGE PC
P6PC	=1		;PC CODE FOR PAGE 6
PPC	=2		;PC CODE FOR PROGRAM MEMORY

;INIT PC VALUES

CURPC	=0
PC0	=0		;PAGE ZERO
PC1	=$600		;PAGE 6 PC
PC2	=$3800		;PROGRAM PC
;
; End of SYSMAC.SML
;----------------------------------------------------------------
 
;.SBTTL	Parser support
 
;  Define storage for pointers into command buffer. They must be
;  on zero-page to take advantage of pre- and post-indexed indirect
;  and also the simulated indirect addressing mode.
 
saddr	=	$80		; Saved string address - must be on page zero
cm_rty  =	$82		; Byte pointer to CTRL/R Text
cm_bfp  =	$84		; Byte pointer to start of text buffer
cm_ptr  =	$86		; Byte pointer to Next Input to be parsed
cm_inc  =	$88		; Number of characters left in buffer
cm_cnt  =	$89		; Space left in buffer
cminf1  =	$8A		; Information passed to comnd routines
cminf2  =	$8C		;		...
cmdptr	=	cminf2		; Pointer to default for parse
cmkptr  =	$8E		; Pointer for Cmkeyw routine
cmsptr  =	$90		; Saved character pointer
cmspt2  =	$92		; Saved keyword table pointer
cmspt3  =	$94		; Saved buffer pointer
cmhptr  =	$96		; Ptr. to current help text
cmptab  =	$98		; Ptr. to beginning of current keyword table
cmfcb	=	$9A		; Pointer to FCB
cmehpt	=	$9C		; Pointer to help commands
;
;	other leftover pointers that have to be in page 0
;
kerbf1  =	$A0		; This always points to packet buffer
kerbf2	=	$A2		; This always points to data buffer

;----------------------------------------------------------------
; pointers we need
;
source	=	$A4		;[19] indirect address to be read
dest	=	$A6		;[19] indirect address to be stored	
strptr	=	$A8		;[jrd] temp for pointing into strings
count	=	$AC		;[jrd] count for mbs
tmpptr	=	$AE		; [jrd] temp for strictly local pointer hacking
;
; pointers for pathname parsing stuff
;
pndptr	=	$B0		; pathname descriptor ptr
pnddef	=	$B2		; pathname target ptr, for merging
pnptr	=	$B4		; pathname txt ptr
pncptr	=	$B6		; pathname component pointer
;
; device names and other atari type data
;
;kbdname: .byte	"K:"		; not used in new kbd driver
;	.byte	ATEOL
scrname: .byte	"E:"
	.byte	ATEOL
comname: .byte	"R:"
	.byte	ATEOL
comopen: .byte	0		; flag for com port open
comstat: .byte	0		; status of com port, from last status op
compend: .word	0		; count of pending chars from port
;
;----------------------------------------------------------------

char:	.byte	$00		;[26] Character just read
stat:	.byte	$00		;[33] RS232 status byte
lpcnt:	.byte	$00		;[EL] cursor blink counter
;lineh:	.byte	$00		;[19] hires cursor line number
;colh:	.byte	$00		;[19] hires cursor column number
;hilo:	.byte	$F0		;[19] hires nibble mask
;rvmask:	.byte	$00		;[19] reverse video mask ($f=rev, $0=normal)
;cflag:	.byte	$FF		;[19] 0 if char under cursor has been reversed
;cstate:	.byte	$00		;[19] top nibble of char und. cursor if cflag=0
;flag79:	.byte	$00		;[19] non-0 if previous char printed in col 79
;fla79:	.byte	$00		;[19] one shot copy of previous flag79
suspend: .byte	$00		;[24] RS-232 reads suspended if non-zero
fxoff:	.byte	$00		;[24] Xoff has been sent if non-zero
;commflg: .byte	$00		;[24] non-zero if commodore key is depressed

; starting here, replaced ^L with ";ctrl-l" for now

;ctrl-l.SBTTL	Translation and Font Tables
 
;	ASCII/ATASCII Translation Tables
;	These are used for translating file data
;
;	At2as:	atascii -> ascii
; 
;at2as:	.byte	$00	;[31] ^@ NUL
;	.byte	$01	;[31] ^A SOH
;	.byte	$02	;[31] ^B 
;	.byte	$03	;[31] ^C 
;	.byte	$04	;[31] ^D 
;	.byte	$05	;[31] ^E 
;	.byte	$06	;[31] ^F 
;	.byte	$07	;[31] ^G BEL
;	.byte	$08	;[31] ^H BS
;	.byte	$09	;[31] ^I TAB
;	.byte	$0a	;[31] ^J LF
;	.byte	$0b	;[31] ^K 
;	.byte	$0c	;[31] ^L FF
;	.byte	$0d	;[31] ^M CR
;	.byte	$0e	;[31] ^N 
;	.byte	$0f	;[31] ^O 
;	.byte	$10	;[31] ^P 
;	.byte	$11	;[31] ^Q 
;	.byte	$12	;[31] ^R 
;	.byte	$13	;[31] ^S 
;	.byte	$14	;[31] ^T
;	.byte	$15	;[31] ^U 
;	.byte	$16	;[31] ^V 
;	.byte	$17	;[31] ^W 
;	.byte	$18	;[31] ^X 
;	.byte	$19	;[31] ^Y 
;	.byte	$1a	;[31] ^Z 
;	.byte	$1b	;[31] ^[ 
;	.byte	$1c	;[31] ^\ 
;	.byte	$1d	;[31] ^] 
;	.byte	$1e	;[31] ^^ 
;	.byte	$1f	;[31] ^_ 
;	.byte	$20	;[31] SPACE
;	.byte	'!'	;[31] ! 
;	.byte	'"'	;[31] " 
;	.byte	'#'	;[31] # 
;	.byte	'$'	;[31] $ 
;	.byte	'%'	;[31] % 
;	.byte	'&'	;[31] & 
;	.byte	'''	;[31] ' 
;	.byte	'('	;[31] ( 
;	.byte	')'	;[31] ) 
;	.byte	'*'	;[31] * 
;	.byte	'+'	;[31] + 
;	.byte	','	;[31] , 
;	.byte	'-'	;[31] - 
;	.byte	'.'	;[31] . 
;	.byte	'/'	;[31] / 
;	.byte	'0'	;[31] 0 
;	.byte	'1'	;[31] 1 
;	.byte	'2'	;[31] 2 
;	.byte	'3'	;[31] 3 
;	.byte	'4'	;[31] 4 
;	.byte	'5'	;[31] 5 
;	.byte	'6'	;[31] 6 
;	.byte	'7'	;[31] 7 
;	.byte	'8'	;[31] 8 
;	.byte	'9'	;[31] 9 
;	.byte	':'	;[31] : 
;	.byte	';'	;[31] ; 
;	.byte	'<'	;[31] < 
;	.byte	'='	;[31] = 
;	.byte	'>'	;[31] > 
;	.byte	'?'	;[31] ? 
;	.byte	'@'	;[31] @ 
;	.byte	'A'	;[31] A
;	.byte	'B'	;[31] B
;	.byte	'C'	;[31] C
;	.byte	'D'	;[31] D 
;	.byte	'E'	;[31] E 
;	.byte	'F'	;[31] F 
;	.byte	'G'	;[31] G 
;	.byte	'H'	;[31] H 
;	.byte	'I'	;[31] I 
;	.byte	'J'	;[31] J 
;	.byte	'K'	;[31] K 
;	.byte	'L'	;[31] L 
;	.byte	'M'	;[31] M 
;	.byte	'N'	;[31] N 
;	.byte	'O'	;[31] O 
;	.byte	'P'	;[31] P 
;	.byte	'Q'	;[31] Q 
;	.byte	'R'	;[31] R 
;	.byte	'S'	;[31] S 
;	.byte	'T'	;[31] T 
;	.byte	'U'	;[31] U 
;	.byte	'V'	;[31] V 
;	.byte	'W'	;[31] W 
;	.byte	'X'	;[31] X 
;	.byte	'Y'	;[31] Y 
;	.byte	'Z'	;[31] Z 
;	.byte	'['	;[31] [ 
;	.byte	'\'	;[31] \ 
;	.byte	']'	;[31] ] 
;	.byte	'^'	;[31] ^ 
;	.byte	'_'	;[31] _
;	.byte	$60	;[31] 
;	.byte	'a'	;[31] a 
;	.byte	'b'	;[31] b 
;	.byte	'c'	;[31] c 
;	.byte	'd'	;[31] d 
;	.byte	'e'	;[31] e 
;	.byte	'f'	;[31] f 
;	.byte	'g'	;[31] g 
;	.byte	'h'	;[31] h 
;	.byte	'i'	;[31] i 
;	.byte	'j'	;[31] j 
;	.byte	'k'	;[31] k 
;	.byte	'l'	;[31] l 
;	.byte	'm'	;[31] m 
;	.byte	'n'	;[31] n 
;	.byte	'o'	;[31] o 
;	.byte	'p'	;[31] p 
;	.byte	'q'	;[31] q 
;	.byte	'r'	;[31] r 
;	.byte	's'	;[31] s 
;	.byte	't'	;[31] t 
;	.byte	'u'	;[31] u 
;	.byte	'v'	;[31] v 
;	.byte	'w'	;[31] w 
;	.byte	'x'	;[31] x 
;	.byte	'y'	;[31] y 
;	.byte	'z'	;[31] z 
;	.byte	'{'	;[31] { 
;	.byte	'|'	;[31] | 
;	.byte	'}'	;[31] } 
;	.byte	'~'	;[31] ~ 
;	.byte	$7f	;[31] DEL
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31]
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	$0D	; [jrd] atascii EOL -> CR
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'A'	;[31] A from A key (dup)
;	.byte	'B'	;[31] B from B key (dup)
;	.byte	'C'	;[31] C from C key (dup)
;	.byte	'D'	;[31] D from D key (dup)
;	.byte	'E'	;[31] E from E key (dup)
;	.byte	'F'	;[31] F from F key (dup)
;	.byte	'G'	;[31] G from G key (dup)
;	.byte	'H'	;[31] H from H key (dup)
;	.byte	'I'	;[31] I from I key (dup)
;	.byte	'J'	;[31] J from J key (dup)
;	.byte	'K'	;[31] K from K key (dup)
;	.byte	'L'	;[31] L from L key (dup)
;	.byte	'M'	;[31] M from M key (dup)
;	.byte	'N'	;[31] N from N key (dup)
;	.byte	'O'	;[31] O from O key (dup)
;	.byte	'P'	;[31] P from P key (dup)
;	.byte	'Q'	;[31] Q from Q key (dup)
;	.byte	'R'	;[31] R from R key (dup)
;	.byte	'S'	;[31] S from S key (dup)
;	.byte	'T'	;[31] T from T key (dup)
;	.byte	'U'	;[31] U from U key (dup)
;	.byte	'V'	;[31] V from V key (dup)
;	.byte	'W'	;[31] W from W key (dup)
;	.byte	'X'	;[31] X from X key (dup)
;	.byte	'Y'	;[31] Y from Y key (dup)
;	.byte	'Z'	;[31] Z from Z key (dup)
;	.byte	'{'	;[31] { from SHIFT/+ key (dup)
;	.byte	'|'	;[31] | from ????? (dup)
;	.byte	'}'	;[31] } from SHIFT/- key (dup)
;	.byte	'~'	;[31] ~ from SHIFT/^ key (dup)
;	.byte	$7f	;[31] DEL from ?????
;	.byte	$20	;[31] SPACE from SHIFT/SPACE key (dup)
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
;	.byte	'?'	;[31] illegal
 
;
; compressed form translate table
;
xat2as:
	.byte	ATEOL,cr		; Atascii EOL -> cr
	.byte	ATTAB,tab
	.byte	ATBEL,bel
	.byte	ATRUB,del
	.byte	0

;	As2at - ASCII to ATASCII
 
;as2at:	.byte	$00	;[31] NUL
;	.byte	$01	;[31] ^A 
;	.byte	$02	;[31] ^B 
;	.byte	$03	;[31] ^C 
;	.byte	$04	;[31] ^D 
;	.byte	$05	;[31] ^E 
;	.byte	$06	;[31] ^F 
;	.byte	$07	;[31] BEL
;	.byte	$08	;[31] BS
;	.byte	$09	;[31] TAB
;	.byte	$0a	;[31] NL
;	.byte	$0b	;[31] ^K 
;	.byte	$0c	;[31] ^L 
;	.byte	ATEOL	;[31] CR 
;	.byte	$0e	;[31] ^N 
;	.byte	$0f	;[31] ^O 
;	.byte	$10	;[31] ^P 
;	.byte	$11	;[31] ^Q 
;	.byte	$12	;[31] ^R 
;	.byte	$13	;[31] ^S 
;	.byte	$14	;[31] ^T 
;	.byte	$15	;[31] ^U 
;	.byte	$16	;[31] ^V 
;	.byte	$17	;[31] ^W 
;	.byte	$18	;[31] ^X 
;	.byte	$19	;[31] ^Y 
;	.byte	$1a	;[31] ^Z 
;	.byte	$1b	;[31] ^[ 
;	.byte	$1c	;[31] ^\ 
;	.byte	$1d	;[31] ^] 
;	.byte	$1e	;[31] ^^ 
;	.byte	$1f	;[31] ^_ 
;	.byte	$20	;[31] SPACE
;	.byte	$21	;[31] ! 
;	.byte	$22	;[31] " 
;	.byte	$23	;[31] # 
;	.byte	$24	;[31] $ 
;	.byte	$25	;[31] % 
;	.byte	$26	;[31] & 
;	.byte	$27	;[31] ' 
;	.byte	$28	;[31] ( 
;	.byte	$29	;[31] ) 
;	.byte	$2a	;[31] * 
;	.byte	$2b	;[31] + 
;	.byte	$2c	;[31] , 
;	.byte	$2d	;[31] - 
;	.byte	$2e	;[31] . 
;	.byte	$2f	;[31] / 
;	.byte	$30	;[31] 0 
;	.byte	$31	;[31] 1 
;	.byte	$32	;[31] 2 
;	.byte	$33	;[31] 3 
;	.byte	$34	;[31] 4 
;	.byte	$35	;[31] 5 
;	.byte	$36	;[31] 6 
;	.byte	$37	;[31] 7 
;	.byte	$38	;[31] 8 
;	.byte	$39	;[31] 9 
;	.byte	$3a	;[31] : 
;	.byte	$3b	;[31] ; 
;	.byte	$3c	;[31] < 
;	.byte	$3d	;[31] = 
;	.byte	$3e	;[31] > 
;	.byte	$3f	;[31] ? 
;	.byte	'@' 	;[31] @ 
;	.byte	'A'	;[31] A
;	.byte	'B'	;[31] B
;	.byte	'C'	;[31] C
;	.byte	'D'	;[31] D 
;	.byte	'E'	;[31] E 
;	.byte	'F'	;[31] F 
;	.byte	'G'	;[31] G 
;	.byte	'H'	;[31] H 
;	.byte	'I'	;[31] I 
;	.byte	'J'	;[31] J 
;	.byte	'K'	;[31] K 
;	.byte	'L'	;[31] L 
;	.byte	'M'	;[31] M 
;	.byte	'N'	;[31] N 
;	.byte	'O'	;[31] O 
;	.byte	'P'	;[31] P 
;	.byte	'Q'	;[31] Q 
;	.byte	'R'	;[31] R 
;	.byte	'S'	;[31] S 
;	.byte	'T'	;[31] T 
;	.byte	'U'	;[31] U 
;	.byte	'V'	;[31] V 
;	.byte	'W'	;[31] W 
;	.byte	'X'	;[31] X 
;	.byte	'Y'	;[31] Y 
;	.byte	'Z'	;[31] Z 
;	.byte	$5b	;[31] [ 
;	.byte	$5c	;[31] \ 
;	.byte	$5d	;[31] ] 
;	.byte	$5e	;[31] ^ 
;	.byte	$5f	;[31] _ 
;	.byte	$60	;[31][52]
;	.byte	'a'	;[31] a 
;	.byte	'b'	;[31] b 
;	.byte	'c'	;[31] c 
;	.byte	'd'	;[31] d 
;	.byte	'e'	;[31] e 
;	.byte	'f'	;[31] f 
;	.byte	'g'	;[31] g 
;	.byte	'h'	;[31] h 
;	.byte	'i'	;[31] i 
;	.byte	'j'	;[31] j 
;	.byte	'k'	;[31] k 
;	.byte	'l'	;[31] l 
;	.byte	'm'	;[31] m 
;	.byte	'n'	;[31] n 
;	.byte	'o'	;[31] o 
;	.byte	'p'	;[31] p 
;	.byte	'q'	;[31] q 
;	.byte	'r'	;[31] r 
;	.byte	's'	;[31] s 
;	.byte	't'	;[31] t 
;	.byte	'u'	;[31] u 
;	.byte	'v'	;[31] v 
;	.byte	'w'	;[31] w 
;	.byte	'x'	;[31] x 
;	.byte	'y'	;[31] y 
;	.byte	'z'	;[31] z 
;	.byte	'['	; [jrd] what to do with these on atari?
;	.byte	'|'	;[31][52] | 
;	.byte	']'	;[31][52] }
;	.byte	$60	;[31][52] ~ 
;	.byte	ATRUB	;[31] DEL


;
; compressed form translate table
;
xas2at:
	.byte	cr,ATEOL		; cr -> Atascii EOL
	.byte	tab,ATTAB
	.byte	bel,ATBEL
	.byte	del,ATRUB
	.byte	0

;
;	These are used for translating to/from ascii in terminal rtn 
;	To - ATASCII to ASCII
;
;attoas:	.byte	$00	;[31] ^@ from ^@ key (NUL)
;	.byte	$01	;[31] ^A from ^A key
;	.byte	$02	;[31] ^B from ^B key
;	.byte	$03	; ^C from ^C key
;	.byte	$04	; ^D from ^D key
;	.byte	$05	; ^E from ^E key
;	.byte	$06	; ^F from ^F key
;	.byte	$07	; ^G from ^G key
;	.byte	$08	; ^H from ^H key
;	.byte	$09	; ^I from ^I key
;	.byte	$0a	; ^J from ^J key
;	.byte	$0b	; ^K from ^K key
;	.byte	$0c	; ^L from ^L key
;	.byte	$0d	; ^M from ^M and RETURN keys
;	.byte	$0e	; ^N from ^N key
;	.byte	$0f	; ^O from ^O key
;	.byte	$10	; ^P from ^P key
;	.byte	$11	; ^Q from ^Q and CURS DOWN keys
;	.byte	$12	; ^R from ^R key
;	.byte	$13	; ^S from ^S and HOME keys
;	.byte	$14	; DEL from ^T and DEL keys
;	.byte	$15	; ^U from ^U key
;	.byte	$16	; ^V from ^V key
;	.byte	$17	; ^W from ^W key
;	.byte	$18	; ^X from ^X key
;	.byte	$19	; ^Y from ^Y key
;	.byte	$1A	; ^Z from ^Z key
;	.byte	$1B	; ^[ from ^[ key
;	.byte	$1C	; atari up arrow
;	.byte	$1D	; down arrow
;	.byte	$1E	; left arrow
;	.byte	$1F	; right arrow
;	.byte	$20	; SPACE from SPACE bar
;	.byte	'!'	; ! from ! key
;	.byte	'"'	; " from " key
;	.byte	'#'	; # from # key
;	.byte	'$'	; $ from $ key
;	.byte	'%'	; % from % key
;	.byte	'&'	; & from & key
;	.byte	'''	; ' from ' key
;	.byte	'('	; ( from ( key
;	.byte	')'	; ) from ) key
;	.byte	'*'	; * from * key
;	.byte	'+'	; + from + key
;	.byte	','	; , from , key
;	.byte	'-'	; - from - key
;	.byte	'.'	; . from . key
;	.byte	'/'	; / from / key
;	.byte	'0'	; 0 from 0 key
;	.byte	'1'	; 1 from 1 key
;	.byte	'2'	; 2 from 2 key
;	.byte	'3'	; 3 from 3 key
;	.byte	'4'	; 4 from 4 key
;	.byte	'5'	; 5 from 5 key
;	.byte	'6'	; 6 from 6 key
;	.byte	'7'	; 7 from 7 key
;	.byte	'8'	; 8 from 8 key
;	.byte	'9'	; 9 from 9 key
;	.byte	':'	; : from : key
;	.byte	';'	; ; from ; key
;	.byte	'<'	; < from < key
;	.byte	'='	; = from = key
;	.byte	'>'	; > from > key
;	.byte	'?'	; ? from ? key
;	.byte	'@'	; @ from @ key
;	.byte	'A'	; A from A key
;	.byte	'B'	; B from B key
;	.byte	'C'	; C from C key
;	.byte	'D'	; D from D key
;	.byte	'E'	; E from E key
;	.byte	'F'	; F from F key
;	.byte	'G'	; G from G key
;	.byte	'H'	; H from H key
;	.byte	'I'	; I from I key
;	.byte	'J'	; J from J key
;	.byte	'K'	; K from K key
;	.byte	'L'	; L from L key
;	.byte	'M'	; M from M key
;	.byte	'N'	; N from N key
;	.byte	'O'	; O from O key
;	.byte	'P'	; P from P key
;	.byte	'Q'	; Q from Q key
;	.byte	'R'	; R from R key
;	.byte	'S'	; S from S key
;	.byte	'T'	; T from T key
;	.byte	'U'	; U from U key
;	.byte	'V'	; V from V key
;	.byte	'W'	; W from W key
;	.byte	'X'	; X from X key
;	.byte	'Y'	; Y from Y key
;	.byte	'Z'	; Z from Z key
;	.byte	'['	; [ from [ key
;	.byte	'\'	; \ from POUND key
;	.byte	']'	; ] from ] key
;	.byte	'^'	; ^ from ^ key (really UP ARROW)
;	.byte	'_'	; _ from _ key
;	.byte	$60	;  SHIFT/* key
;	.byte	'a'	; a from a key
;	.byte	'b'	; b from b key
;	.byte	'c'	; c from c key
;	.byte	'd'	; d from d key
;	.byte	'e'	; e from e key
;	.byte	'f'	; f from f key
;	.byte	'g'	; g from g key
;	.byte	'h'	; h from h key
;	.byte	'i'	; i from i key
;	.byte	'j'	; j from j key
;	.byte	'k'	; k from k key
;	.byte	'l'	; l from l key
;	.byte	'm'	; m from m key
;	.byte	'n'	; n from n key
;	.byte	'o'	; o from o key
;	.byte	'p'	; p from p key
;	.byte	'q'	; q from q key
;	.byte	'r'	; r from r key
;	.byte	's'	; s from s key
;	.byte	't'	; t from t key
;	.byte	'u'	; u from u key
;	.byte	'v'	; v from v key
;	.byte	'w'	; w from w key
;	.byte	'x'	; x from x key
;	.byte	'y'	; y from y key
;	.byte	'z'	; z from z key
;	.byte	'{'	; { from SHIFT/+ key
;	.byte	'|'	; | from ?????
;	.byte	'}'	; } from SHIFT/- key
;	.byte	$7F	; Atascii RUBOUT
;	.byte	$09	; Atascii TAB
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'_'	; _ from F1 key
;	.byte	$60	;  F3 key
;	.byte	'{'	; { from F5 key
;	.byte	$08	; ^H (BS) from F7 key
;	.byte	'|'	; | from F2 key
;	.byte	'~'	; ~ from F4 key
;	.byte	'}'	; } from F6 key
;	.byte	$14	; ^T from F8 key
;	.byte	$0a	; NL from SHIFT/CR key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	cr	; Atascii EOL
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	$20	; SPACE from SHIFT/SPACE key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	$60	;  SHIFT/* key (dup)
;	.byte	'A'	; A from A key (dup)
;	.byte	'B'	; B from B key (dup)
;	.byte	'C'	; C from C key (dup)
;	.byte	'D'	; D from D key (dup)
;	.byte	'E'	; E from E key (dup)
;	.byte	'F'	; F from F key (dup)
;	.byte	'G'	; G from G key (dup)
;	.byte	'H'	; H from H key (dup)
;	.byte	'I'	; I from I key (dup)
;	.byte	'J'	; J from J key (dup)
;	.byte	'K'	; K from K key (dup)
;	.byte	'L'	; L from L key (dup)
;	.byte	'M'	; M from M key (dup)
;	.byte	'N'	; N from N key (dup)
;	.byte	'O'	; O from O key (dup)
;	.byte	'P'	; P from P key (dup)
;	.byte	'Q'	; Q from Q key (dup)
;	.byte	'R'	; R from R key (dup)
;	.byte	'S'	; S from S key (dup)
;	.byte	'T'	; T from T key (dup)
;	.byte	'U'	; U from U key (dup)
;	.byte	'V'	; V from V key (dup)
;	.byte	'W'	; W from W key (dup)
;	.byte	'X'	; X from X key (dup)
;	.byte	'Y'	; Y from Y key (dup)
;	.byte	'Z'	; Z from Z key (dup)
;	.byte	'{'	; { from SHIFT/+ key (dup)
;	.byte	'|'	; | from ????? (dup)
;	.byte	'}'	; } from SHIFT/- key (dup)
;	.byte	'~'	; ~ from SHIFT/^ key (dup)
;	.byte	$7f	; DEL from ?????
;	.byte	$20	; SPACE from SHIFT/SPACE key (dup)
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'?'	; illegal key
;	.byte	'~'	; ~ from SHIFT/^ key (dup)
 
;
;	From ASCII to ATASCII (Terminal routine)
;
;	just use as2at
;
;astoat:	.byte	$00	;[31] NUL doesn't print
;	.byte	$00	;[31] ^A doesn't print
;	.byte	$00	;[31] ^B doesn't print
;	.byte	$00	;[31] ^C doesn't print
;	.byte	$00	;[31] ^D doesn't print
;	.byte	$00	;[31] ^E doesn't print
;	.byte	$00	; ^F doesn't print
;	.byte	$FD	; -> Atari bell.  Fix this to use beeper?
;	.byte	ATLRW	; BS prints as CURSOR LEFT
;	.byte	ATTAB	; TAB is special
;	.byte	$0A	; zzz ATDRW	; NL prints as CURSOR DOWN
;	.byte	$00	; ^K doesn't print
;	.byte	$0C	; ^L doesn't print 
;	.byte	ATEOL	; CR is special
;	.byte	$00	; ^N doesn't print 
;	.byte	$00	; ^O doesn't print
;	.byte	$00	; ^P doesn't print
;	.byte	$00	; ^Q doesn't print
;	.byte	$00	; ^R doesn't print
;	.byte	$00	; ^S doesn't print
;	.byte	$00	; ^T doesn't print
;	.byte	$00	; ^U doesn't print
;	.byte	$00	; ^V doesn't print
;	.byte	$00	; ^W doesn't print
;	.byte	$00	; ^X doesn't print
;	.byte	$00	; ^Y doesn't print
;	.byte	$00	; ^Z doesn't print
;	.byte	$1B	; escape
;	.byte	$00	; ^\ doesn't print
;	.byte	$00	; ^] doesn't print
;	.byte	$00	; ^^ doesn't print
;	.byte	$00	; ^_ doesn't print
;	.byte	$20	; SPACE prints as SPACE 
;	.byte	$21	; ! prints as !
;	.byte	$22	; " prints as "
;	.byte	$23	; # prints as #
;	.byte	$24	; $ prints as $
;	.byte	$25	; % prints as %
;	.byte	$26	; & prints as &
;	.byte	$27	; ' prints as '
;	.byte	$28	; ( prints as (
;	.byte	$29	; ) prints as )
;	.byte	$2a	; * prints as *
;	.byte	$2b	; + prints as +
;	.byte	$2c	; , prints as ,
;	.byte	$2d	; - prints as -
;	.byte	$2e	; . prints as .
;	.byte	$2f	; / prints as /
;	.byte	$30	; 0 prints as 0
;	.byte	$31	; 1 prints as 1
;	.byte	$32	; 2 prints as 2
;	.byte	$33	; 3 prints as 3
;	.byte	$34	; 4 prints as 4
;	.byte	$35	; 5 prints as 5
;	.byte	$36	; 6 prints as 6
;	.byte	$37	; 7 prints as 7
;	.byte	$38	; 8 prints as 8
;	.byte	$39	; 9 prints as 9
;	.byte	$3a	; : prints as :
;	.byte	$3b	; ; prints as ;
;	.byte	$3c	; < prints as <
;	.byte	$3d	; = prints as =
;	.byte	$3e	; > prints as >
;	.byte	$3f	; ? prints as ?
;	.byte	$40	; @ prints as @
;	.byte	$41	; a prints as a
;	.byte	$42	; b prints as b
;	.byte	$43	; c prints as c
;	.byte	$44	; d prints as d
;	.byte	$45	; e prints as e
;	.byte	$46	; f prints as f
;	.byte	$47	; g prints as g
;	.byte	$48	; h prints as h
;	.byte	$49	; i prints as i
;	.byte	$4a	; j prints as j
;	.byte	$4b	; k prints as k
;	.byte	$4c	; l prints as l
;	.byte	$4d	; m prints as m
;	.byte	$4e	; n prints as n
;	.byte	$4f	; o prints as o
;	.byte	$50	; p prints as p
;	.byte	$51	; q prints as q
;	.byte	$52	; r prints as r
;	.byte	$53	; s prints as s
;	.byte	$54	; t prints as t
;	.byte	$55	; u prints as u
;	.byte	$56	; v prints as v
;	.byte	$57	; w prints as w
;	.byte	$58	; x prints as x
;	.byte	$59	; y prints as y
;	.byte	$5a	; z prints as z
;	.byte	$5b	; [ prints as [
;	.byte	$5c	; \ prints as \
;	.byte	$5d	; ] prints as ]
;	.byte	$5e	; ^ prints as ^
;	.byte	$5f	; _ prints as _
;	.byte	$60	; 
;	.byte	$61	; A prints as A
;	.byte	$62	; B prints as B
;	.byte	$63	; C prints as C
;	.byte	$64	; D prints as D
;	.byte	$65	; E prints as E
;	.byte	$66	; F prints as F
;	.byte	$67	; G prints as G
;	.byte	$68	; H prints as H
;	.byte	$69	; I prints as I
;	.byte	$6a	; J prints as J
;	.byte	$6b	; K prints as K
;	.byte	$6c	; L prints as L
;	.byte	$6d	; M prints as M
;	.byte	$6e	; N prints as N
;	.byte	$6f	; O prints as O
;	.byte	$70	; P prints as P
;	.byte	$71	; Q prints as Q
;	.byte	$72	; R prints as R
;	.byte	$73	; S prints as S
;	.byte	$74	; T prints as T
;	.byte	$75	; U prints as U
;	.byte	$76	; V prints as V
;	.byte	$77	; W prints as W
;	.byte	$78	; X prints as X
;	.byte	$79	; Y prints as Y
;	.byte	$7a	; Z prints as Z
;	.byte	$7b	; { prints as {
;	.byte	$7c	; | prints as |
;	.byte	$7d	; } prints as }
;	.byte	$7e	; ~ prints as ~
;	.byte	ATRUB	; -> atascii rubout

;
;	character translator.
;
;	calling sequence:
;	char to xlate in A
;	address of xlate tbl in X,Y
; 
;	returns:	xlated char in A
;
xlchar:	.byte	0		; temp for original byte
xlate:
	stx	tmpptr		; set up xlate table ptr
	sty	tmpptr+1
	sta	xlchar		; store the old data
	ldy	#0
xlate1:
	lda	(tmpptr),y	; get a 'from' byte
	beq	xlate9		; zero, we're done
	cmp	xlchar		; match the one we're called with?
	beq	xlate2		; yup, go translate it
	iny			; bump y for next time
	iny			; skip the wrong one
	jmp	xlate1
xlate2:
	iny			; point at new one
	lda	(tmpptr),y	; get get translated one
	rts			; and go home
xlate9:
	lda	xlchar		; no translation, just return the original
	rts
;
;----------------------------------------------------------------
;
; pathname parsing stuff.
;
; a pathname consists of optional device, name, and optional
; extension.
;
; a pathname descriptor is a structure containing three fields,
; each of which is a byte of max, a byte of length, and a (max) bytes
; of data. they are:
;	dev	device spec 	(2 bytes)
;	name	file name	(8 bytes)
;	ext	file type	(3 bytes)
;
; equates for pathname descriptor block
;
pnd_fl  =	0		; flags byte
pnd_dm	=	1		; dev max, 1 byte
pnd_ds	=	2		; dev size, one byte
pnd_dt	=	3		; dev text, two bytes
pnd_nm	=	5		; name max, 1 byte
pnd_ns	=	6		; name size, 1 byte
pnd_nt	=	7		; name text, 8 bytes
pnd_em	=	15		; ext max
pnd_es	=	16
pnd_et	=	17
pndsiz	=	20		; total size
;
; generic component equates
;
pnc_m	=	0		; max this component
pnc_s	=	1		; size this component
pnc_t	=	2		; text this component
;
; bits in flag byte
;
pnf_dp	=	$01		; dev spec present
pnf_np	=	$02		; name present
pnf_ep	=	$04		; type present
pnf_wl	=	$08		; wild card somewhere
;
; if we had macros, the macro for building one of these would
; look like this:
;
;	.byte	0		; flags
;	.byte	2		; dev max
;	.byte	0
;	.blkb	2
;	.byte	8		; name max
;	.byte	0
;	.blkb	8
;	.byte	3
;	.byte	0
;	.blkb	3
;
ppnt0:	.byte	0		; temp for parse-pathname and friends
ppnt1:	.byte	0
ppnt2:	.byte	0
;
;	pncupc:		char-upcase char in A
;
pncupc:
	cmp	#'a'		; >= 'a' ?
	bcc	pncupc9		; nope, leave
	cmp	#'z'+1		; < 'z'?
	bcs	pncupc9		; nope, leave
	sec
	sbc	#$20		; shift to up case.  (carry's set)
pncupc9:
	rts
;
;	pnclgl:		char in a legal pathname char?
;			returns carry set if not legal
;
pnclgl:
	cmp	#':'		; colon's ok
	beq	pnclgl9
	cmp	#'.'		; dot's ok too
	beq	pnclgl9
	cmp	#'*'		; star is ok
	beq	pnclgl9
	cmp	#'?'		; q-mark is ok
	beq	pnclgl9
	cmp	#'0'		; 0..9 is ok
	bcc	pnclgl8		;  less, no good
	cmp	#'9'+1
	bcc	pnclgl9		; less, ok
	cmp	#'A'		; alpha?
	bcc	pnclgl8		; less is no good
	cmp	#'Z'+1
	bcc	pnclgl9		; A..Z's ok
pnclgl8:
	sec			; error return
	rts
pnclgl9:
	clc			; ok return
	rts
;
;	pnfindc:	find a character, in x, in (pnptr), starting
;			at y.  returns idx or -1 in y, EQ if found, NEQ
;			if not found.  Trashes A
;
pnfindc:
	stx	ppnt1		; save char
pnfindc1:
	lda	(pnptr),y	; get a char
	beq	pnfindc8	; 0? ok, stop here
	jsr	pncupc		; upcase it
	jsr	pnclgl		; legal pathname char?
	bcs	pnfindc8	; nope, go error
	cmp	ppnt1		; compare it
	beq	pnfindc9	; got it, return
	iny			; next!
	bne	pnfindc1
pnfindc8:
	ldy	#$ff		; return 'not found' (-1)
pnfindc9:
	rts
;
;	parsepn::
;	grok a pathname string into a pathname descriptor.
;	expects pathname string pointed to by x,y, desc in (pndptr).
;	pathname string terminated by any non-pathname char.
;
; this routine copies in one component.  Initial idx in Y, terminating
; character in X, component offset in desc in A
;
;ppndbg1: .byte	"Enter parsepn",ATEOL,0
;ppndbg2: .byte	"Leave parsepn",ATEOL,0
ppnct:	.byte	0		; terminator char
ppncf:	.byte	0		; flags for pathname we're parsing
ppncpf:	.byte	0		; flag to set in component we're on
ppncomp:
	stx	ppnct		; save terminator
	clc			; first calculate 
	adc	pndptr		;  pointer to pathname
	sta	pncptr		;  component
	lda	pndptr+1
	adc	#0
	sta	pncptr+1
ppncp1:
	lda	(pnptr),y	; get a char
; below?	iny			; and bump the string idx
	beq	ppncp9		; always terminate on nuls
	cmp	ppnct		; hit terminator?
	beq	ppncp8		; yes, stop this component
	cmp	#ATEOL		; eol?
	beq	ppncp9		; yes, always terminate on eols, too
	iny			; and bump the string idx
	jsr	pncupc		; upcase it
	jsr	pnclgl		; legal char?
	bcs	ppncp9		; nope, stop here
	cmp	#'*'		; is it one of the wild chars?
	beq	ppncp2		; yes, flag it as such
	cmp	#'?'
	bne	ppncp3
ppncp2:
	pha			; save char
	lda	#pnf_wl		; or in the 'wild' flag
	ora	ppncf
	sta	ppncf
	pla			; get char back
ppncp3:
	sty	ppnt0		; save y for a bit
	pha			; save char
	ldy	#pnc_s		; component size offset
	lda	(pncptr),y	; get component size
; check size
	ldy	#pnc_m		; component max
	cmp	(pncptr),y	; compare size to max
	bcs	ppncp6		; too big! ignore this byte
	ldy	#pnc_s		; idx for size again
;
	pha			; save size for later indexing
	clc			; add one to it for
	adc	#1		;  next time
	sta	(pncptr),y	; put it back
	pla			; get the old size (index) back
	clc			; zap carry again, and
	adc	#pnc_t		;  add dev text offset
	tay			; into y
	pla			; get char back
	sta	(pncptr),y	; stuff into dev text
	lda	ppncpf		; or in the flag corresponding to 
	ora	ppncf		;  this component
	sta	ppncf
	jmp	ppncp7		; and go back for more
ppncp6:
	pla			; throw char away
ppncp7:
	ldy	ppnt0		; get string idx back
	jmp	ppncp1
ppncp8:
;
; found terminator.  Skip it.
	iny
;
ppncp9:
	rts
;
;	The main routine of the pathname parser.
;
parsepn:
	stx	pnptr		; set string pointer lo
	sty	pnptr+1		;  and hi
;zzz debug
;	ldx	#<ppndbg1
;	ldy	#>ppndbg1
;	jsr	pstrnul
;zzz
	lda	#0		; first zap len flds in desc
	sta	ppncf		; and flags in progress
	ldy	#pnd_ds		; dev size
	sta	(pndptr),y	; zap
	ldy	#pnd_ns
	sta	(pndptr),y
	ldy	#pnd_es
	sta	(pndptr),y
	ldy	#0		; idx into name string
ppndev:
	ldx	#':'		; do we have a colon?
	jsr	pnfindc
	bmi	ppndev9		; nope, skip this part
	lda	#pnf_dp		; flag to set if we do it
	sta	ppncpf
	ldy	#0		; start at zero please
	lda	#pnd_dm		; do device component
	jsr	ppncomp
	jmp	ppnnam		; go do the name
ppndev9:
	ldy	#0		; reset string ptr
ppnnam:
	lda	#pnf_np		; flag to set if we do it
	sta	ppncpf
	lda	#pnd_nm		; do name component
	ldx	#'.'		; stop at dot
				; y's already set
	jsr	ppncomp
	lda	#pnf_ep		; flag to set if we do it
	sta	ppncpf
	lda	#pnd_em		; extension, please
	ldx	#ATEOL		; sort of irrelevant, as we'll stop
				;  on any illegal char.
	jsr	ppncomp		; y's already set.
	lda	ppncf		; now put in accumulated flags
	ldy	#pnd_fl
	sta	(pndptr),y
;zzz debug
;	ldx	#<ppndbg2
;	ldy	#>ppndbg2
;	jsr	pstrnul
;zzz
	rts			; done!
;
;	pn2str:		(parsed) pathname to string.
;			expects a pathname descriptor in (pndptr)
;			and a string in X,Y.  Generates a namestring
;			terminated by ATEOL, suitable for passing to
;			CIO.  Note that it wants a fully qualified
;			parsed pathname.
;ppndbg3: .byte	"Enter pn2str",ATEOL,0
;ppndbg4: .byte	"Leave pn2str",ATEOL,0
;
; this pushes one byte into output string
;
pn2sp:
	sty	ppnt2		; save y value for a bit
	ldy	ppnt0		; get string idx
	sta	(pnptr),y	; shove the char
;zzz debug
;	pha			; save a
;	txa			; save x
;	pha
;	lda	(pnptr),y	; get char back
;	pha
;	lda	#'|
;	jsr	prchr
;	pla
;	jsr	prchr
;	pla
;	tax
;	pla
;zzz
	inc	ppnt0		; bump the str idx
	ldy	ppnt2		; get y back
	rts
;
; copy one component into outgoing string.
; y contains offset into desc for component text, x contains size
;
pn2scs:
	lda	(pndptr),y	; get a char
	jsr	pn2sp		; stuff it
	iny			; bump dev text idx
	dex			; dec size
	bne	pn2scs		; back for more
	rts
;
; this inits regs, given an initial offset into the descriptor.
; returns Z if length 0.
;
pn2sin:
	lda	(pndptr),y	; get the component size
;zzz debug
;	pha
;	tya
;	pha
;	lda	#'#
;	jsr	prchr
;	pla			; y val
;	pha
;	jsr	prbyte
;	pla
;	pha
;	tay
;	lda	(pndptr),y
;	jsr	prbyte
;	pla
;	tay
;	pla
;zzz	
	iny			; point y at text
	tax			; save it as a counter, set Z for return
	rts
;
;	the main routine
;
pn2str:
	stx	pnptr		; set pathname string lo
	sty	pnptr+1		;  and hi
;zzz debug
;	ldx	#<ppndbg3
;	ldy	#>ppndbg3
;	jsr	pstrnul
;	lda	pnptr+1
;	jsr	prbyte
;	lda	pnptr
;	jsr	prbyte
;zzz
	ldy	#0		; string idx
	sty	ppnt0
	ldy	#pnd_ds		; dev component size
	jsr	pn2sin		; set up regs
	beq	pn2str1		; No dev???  ok, skip it
	jsr	pn2scs		; copy a string
	lda	#':'		; get a colon
	jsr	pn2sp		; push it in
;
pn2str1:
	ldy	#pnd_ns		; name component size
	jsr	pn2sin		; set up
	beq	pn2str2		; zero length name?? this should error ...
	jsr	pn2scs		; copy it in
;
pn2str2:
	lda	#'.'		; get a dot
	jsr	pn2sp		; push it in
;
	ldy	#pnd_es		; name component size
	jsr	pn2sin
	beq	pn2str3		; zero length ext?
	jsr	pn2scs		; copy it in
pn2str3:
	lda	#ATEOL		; get an eol
	jsr	pn2sp		; push it in
;zzz debug
;	ldx	#<ppndbg4
;	ldy	#>ppndbg4
;	jsr	pstrnul
;zzz
	rts			; done!!!
;
;	pnmerge::	Merge two pathnames.  Move components from the
;			first into missing components of the second, ie
;		merge "D1:FOO.BAR","CRUD.BAZ" -> "D1:CRUD.BAZ"
;
;	wants pnddef pointing at pn1, pndptr at pn2
;
pnmc:
	lda	(pndptr),y	; get component size in target pathname
	bne	pnmc9		; nonzero, try next
	lda	(pnddef),y	; ok, get the one we're merging from
	beq	pnmc9		; this one zero too?? ok, skip it
	tax			; get size in x
	inx			; inc to include size byte
pnmc1:
	lda	(pnddef),y	; get a byte
	sta	(pndptr),y	; put it in target
	iny			; bump component ptr
	dex			; dec count
	bne	pnmc1		; round again
pnmc9:
	rts			; done with this component

;ppndbg5: .byte	"Enter pnmerge",ATEOL,0
;ppndbg6: .byte	"Leave pnmerge",ATEOL,0

pnmerge:
;zzz debug
;	ldx	#<ppndbg5
;	ldy	#>ppndbg5
;	jsr	pstrnul
;zzz
	ldy	#pnd_ds		; look at dev component size
	jsr	pnmc		; merge this component
	ldy	#pnd_ns		; do name
	jsr	pnmc		;  ...
	ldy	#pnd_es		; and extension
	jsr	pnmc
;zzz debug
;	ldx	#<ppndbg6
;	ldy	#>ppndbg6
;	jsr	pstrnul
;zzz
	rts			; done!	
;
;
;-----------------------------------------------------------------
;

;ctrl-l;.SBTTL	Flag definitions
 
;	The following are flags passed in the Y register
 
cmfehf	=	1		;[EL] Extra help available
cmfdff	=	2		;[EL] Default value present
 
;.SBTTL	Parse types
 
;	The following are different items to parse for
 
cmini	=	0		; Token to indicate parser init
cmkey	=	1		; Token to parse for keyword
cmifi	=	2		; Token to parse for input file
cmofi	=	3		; Token to parse for output file
cmcfm	=	4		; Token to parse for confirm
cmnum	=	5		; Token to parse for a number
cmswi	=	6		; Token to parse for a switch
cmfls	=	7		; Token to parse for a floating-point number
cmtxt	=	8		; Token to parse for an unquoted string
cmtok	=	9		; Token to parse for a single char token

;.SBTTL	COMND package entry points
 
;
;	The following addresses are locations in a jump table which
;	dispatch to appropriate routines in the Comnd package.
;
 
mul16	=	comnd+3		; 16-bit multiply routine
prcrlf	=	mul16+3		; Routine to print a crelf
prstr	=	prcrlf+3	; Routine to print an ASCIZ string
rskp	=	prstr+3		; Routine to skip 3 bytes on return
setbrk	=	rskp+3		; Routine to set a break char in brkwrd
rstbrk	=	setbrk+3	; Routine to reset break char in brkwrd

;ctrl-l.SBTTL	COMND JSYS routines
 
;
;	The following set of routines provides a user oriented way of parsing
;	commands. It is similar to that of the COMND JSYS in TOPS-20. For
;	convenience, a dispatch table is used.
;
 
comnd:  jmp	comand		;  Dispatch to main command routine
	jmp	ml16		;  Dispatch to 16-bit multiply routine
	jmp	prcl_0		;  Dispatch to Prcrlf
	jmp	prst_0		;  Dispatch to Prstr
	jmp	rskp_0		;  Dispatch to Rskp
	jmp	sbrk_0		;  Dispatch to Setbrk
	jmp	rbrk_0		;  Dispatch to Rstbrk
 
;ctrl-l.SBTTL  	Storage Declarations
 
;
;	Following is the storage decalarations for the Comnd routines
;
 
;cmbuf:  .blkb	$100		; Input command buffer
cmbuf	=	scrmemlo	; why not?
;atmbuf:	.blkb	$100		; Atombuffer, (for cmtxt and cmifil)
atmbuf	=	scrmemlo+$100
lenabf:	.res	1		; Length of atom in Atombuffer
brkwrd:	.res	$16		; Break mask
savea:  .res	1		;
savex:  .res	1		;
savey:  .res	1		;
cmbase: .res	1		; Base of integer to be parsed
cmmres: .res	4		; Return value from cmmult call
cmintg: .res	4		; Return value for cminum call
cmfltp: .res	6		; Return value for cmflot call
cmflen: .res	1		; Field length
;cmcdrv: .res	1		; Current drive
cmostp: .res	2		; Save area for stack pointer
cmrprs: .res	2		; Reparse address
cmaflg: .res	1		; Non-zero when an action char has been found
cmcffl:	.res	1		; Non-Zero when previous command failed
cmfrcf:	.res	1		; Non-Zero when signif char has been seen
cmccnt: .res	1		; Non-zero if a significant char is found
cmocnt:	.res	1		; Saved length of command buffer
cmoptr:	.res	2		; Saved ptr to command buffer for <ctrl/H>
cmsflg: .res	1		; Non-zero when the last char was a space
cmstat: .res	1		; Save area for parse type
cmprmx:	.res	1		; Hold area for Comnd parameters
cmprmy:	.res	1		; Hold area for Comnd flags
cmkyln: .res	1		; Keyword length
cmtlen: .res	1		; Test length (for ?-prompting)
cmscrs: .res	1		; Screen output switch
cmentr: .res	1		; Number of remaining entries in table
cmehix:	.res	1		; Index to extra help command buffer
keylen: .res	1		; Keyword length
cmwrk1: .res	1		; Command processing scratch area
cmwrk2: .res	1		;
cmwrk3: .res	1		;
cmwrk4: .res	1		;
 
;----------------------------------------------------------------
; Misc Atari support added here by jrd
;

;
;	The new keyboard driver.
;	This keyboard stuff is here because of two problems with the
;	builtin K: driver; it can't generate all keycodes (the lack of
;	a null is especially annoying) and there's no good way to get
;	it to generate function key sequences.
;	This stuff fixes both those things.  It's also arranged so that 
;	returns ascii (not ATASCII) codes for everything.
;
;	Function key code equates.
;
fnkurw	=	$80		; up arrow
fnkdrw	=	$81		; down arrow
fnklrw	=	$82		; left arrow
fnkrrw	=	$83		; right arrow
fnkpf1	=	$84		; PF1
fnkpf2	=	$85
fnkpf3	=	$86
fnkpf4	=	$87
fnkk0	=	$88		; keypad 0
fnkk1	=	$89
fnkk2	=	$8A
fnkk3	=	$8B
fnkk4	=	$8C
fnkk5	=	$8D
fnkk6	=	$8E
fnkk7	=	$8F
fnkk8	=	$90
fnkk9	=	$91
fnkkdot	=	$92		; keypad dot
fnkkmin	=	$93		; keypad minus
fnkkcom	=	$94		; comma
fnkkent	=	$95		; enter
;
; Special functions, handled internally
;
fnksusp =	$C0		; Suspend
fnkcpsl	=	$C1		; Caps lock
fnkcpul	=	$C2		; Caps unlock
;
;	The keypress to char translate table. Data for this is from OS man
;	page 50.
;
;	The control-shift section of this table is used to generate
;	function keys for VT100 emulation.  Character value is the
;	function key code, above
;
keyxl:
; control = 0, shift = 0
	.byte	'l'		; (00)
	.byte	'j'
	.byte	';'
	.byte	$FF		; not used
	.byte	$FF		; not used
	.byte	'k'
	.byte	'+'
	.byte	'*'
	.byte	'o'		; (08)
	.byte	$FF		; not used
	.byte	'p'
	.byte	'u'
	.byte	cr		; NB! ascii cr, not ATEOL
	.byte	'i'
	.byte	'-'
	.byte	'='
	.byte	'v'		; (10)
	.byte	$FF		; not used
	.byte	'c'
	.byte	$FF		; not used
	.byte	$FF		; not used
	.byte	'b'
	.byte	'x'
	.byte	'z'
	.byte	'4'		; (18)
	.byte	$FF		; not used
	.byte	'3'
	.byte	'6'
	.byte	$1B
	.byte	'5'
	.byte	'2'
	.byte	'1'
	.byte	','		; (20)
	.byte	$20
	.byte	'.'
	.byte	'n'
	.byte	$FF		; not used
	.byte	'm'
	.byte	'/'
	.byte	fnksusp		; atari key
	.byte	'r'		; (28)
	.byte	$FF		; not used
	.byte	'e'
	.byte	'y'
	.byte	$09		; tab.  Use ascii tab here
	.byte	't'
	.byte	'w'
	.byte	'q'
	.byte	'9'		; (30)
	.byte	$FF		; not used
	.byte	'0'
	.byte	'7'
	.byte	$7F		; backspace. use ascii rubout
	.byte	'8'
	.byte	'<'
	.byte	'>'
	.byte	'f'		; (38)
	.byte	'h'
	.byte	'd'
	.byte	$FF		; not used
	.byte	fnkcpul		; caps
	.byte	'g'
	.byte	's'
	.byte	'a'
; control = 0, shift = 1
	.byte	'L'		; (40)
	.byte	'J'
	.byte	':'
	.byte	$FF		; not used
	.byte	$FF		; not used
	.byte	'K'
	.byte	'\'		; sh-+
	.byte	'^'		; sh-*
	.byte	'O'		; (48)
	.byte	$FF		; not used
	.byte	'P'
	.byte	'U'
	.byte	ATEOL		; zzz maybe cr here?
	.byte	'I'
	.byte	'_'		; sh--
	.byte	'|'		; sh-=
	.byte	'V'		; (50)
	.byte	$FF		; not used
	.byte	'C'
	.byte	$FF		; not used
	.byte	$FF		; not used
	.byte	'B'
	.byte	'X'
	.byte	'Z'
	.byte	'$'		; (18) sh-4
	.byte	$FF		; not used
	.byte	'#'		; sh-3
	.byte	'&'		; sh-6
	.byte	$1B		; shift esc???
	.byte	'%'		; sh-5
	.byte	'"'		; sh-2
	.byte	'!'		; sh-1
	.byte	'['		; (20)	sh-,
	.byte	$20		; shift space?
	.byte	']'		; sh-.
	.byte	'N'
	.byte	$FF		; not used
	.byte	'M'
	.byte	'?'		; sh-/
	.byte	fnksusp		; atari key
	.byte	'R'		; (68)
	.byte	$FF		; not used
	.byte	'E'
	.byte	'Y'
	.byte	$09		; tab.  Use ascii tab here
	.byte	'T'
	.byte	'W'
	.byte	'Q'
	.byte	'('		; (70) sh-9
	.byte	$FF		; not used
	.byte	')'		; sh-0
	.byte	'''		; sh-7
	.byte	'~'		; backspace. Use this for tilde
	.byte	'@'		; sh-8
	.byte	'{'		; shift <
	.byte	'}'		; shift >
	.byte	'F'		; (78)
	.byte	'H'
	.byte	'D'
	.byte	$FF		; not used
	.byte	fnkcpsl		; shift caps
	.byte	'G'
	.byte	'S'
	.byte	'A'
; control = 1, shift = 0
	.byte	$0C		; (80)	c-l
	.byte	$0A		; c-j
	.byte	';'		; ???
	.byte	$FF		; not used
	.byte	$FF		; not used
	.byte	$0B		; c-k
	.byte	$1C		; c-+ -> c-\
	.byte	$1E		; c-* -> c-^
	.byte	$0F		; (88) c-o
	.byte	$FF		; not used
	.byte	$10		; c-p
	.byte	$15		; c-u
	.byte	ATEOL		; zzz maybe cr here?
	.byte	$09		; c-i
	.byte	$1F		; c-- -> c-_
	.byte	$1D		; c-= -> c-]
	.byte	$16		; (90)	c-v
	.byte	$FF		; not used
	.byte	$03		; c-c
	.byte	$FF		; not used
	.byte	$FF		; not used
	.byte	$02		; c-b
	.byte	$18		; c-x
	.byte	$1A		; c-z
	.byte	'4'		; (98) ???
	.byte	$FF		; not used
	.byte	'3'		; ???
	.byte	'6'		; ???
	.byte	$1B		; control esc?
	.byte	'5'		; ???
	.byte	'2'		; ???
	.byte	'1'		; ???
	.byte	','		; (A0)	???
	.byte	$00		; control sp
	.byte	'.'		; ???
	.byte	$0E		; c-n
	.byte	$FF		; not used
	.byte	cr		; c-m
	.byte	'/'		; ???
	.byte	fnksusp		; atari key
	.byte	$12		; (A8)	c-r
	.byte	$FF		; not used
	.byte	$05		; c-e
	.byte	$19		; c-y
	.byte	$09		; tab.  Use ascii tab here
	.byte	$14		; c-t
	.byte	$17		; c-w
	.byte	$11		; c-q
	.byte	'9'		; (B0) ???
	.byte	$FF		; not used
	.byte	'0'		; ???
	.byte	$60		; ^7 -> backquote
	.byte	$60		; ^backspace -> backquote
	.byte	'8'		; ???
	.byte	$1B		; ^< -> esc
	.byte	'>'		; ???
	.byte	$06		; (B8)	c-f
	.byte	$08		; c-h
	.byte	$04		; c-d
	.byte	$FF		; not used
	.byte	fnkcpul		; caps
	.byte	bel		; c-g
	.byte	$13		; c-s
	.byte	$01		; c-a
; control = 1, shift = 1
	.byte	'l'		; (C0)
	.byte	'j'
	.byte	';'
	.byte	$FF		; not used
	.byte	$FF		; not used
	.byte	'k'
	.byte	fnklrw		; c-sh-+ -> left arrow
	.byte	fnkrrw		; c-sh-* -> right arrow
	.byte	'o'		; (08)
	.byte	$FF		; not used
	.byte	'p'
	.byte	'u'
	.byte	fnkkent		; c-sh-ret -> keypad enter
	.byte	'i'
	.byte	fnkurw		; c-sh-- -> up arrow
	.byte	fnkdrw		; c-sh-= -> down arrow
	.byte	fnkkent		; (10) c-sh-v -> enter
	.byte	$FF		; not used
	.byte	fnkk3		; c-sh-c -> kp3
	.byte	$FF		; not used
	.byte	$FF		; not used
	.byte	'b'
	.byte	fnkk2		; c-sh-x -> kp2
	.byte	fnkk1		; c-sh-z -> kp1
	.byte	fnkk4		; (18) c-sh-4
	.byte	$FF		; not used
	.byte	fnkk3		; c-sh-3
	.byte	fnkk6		; c-sh-6
	.byte	$1B
	.byte	fnkk5		; c-sh-5
	.byte	fnkk2		; c-sh-2
	.byte	fnkk1		; c-sh-1
	.byte	fnkkcom		; (E0) c-sh-comma
	.byte	fnkk0		; c-sh-space -> keypad 0
	.byte	fnkkdot		; c-sh-.
	.byte	'n'
	.byte	$FF		; not used
	.byte	'm'
	.byte	'/'
	.byte	fnksusp		; atari key
	.byte	fnkpf4		; (E8) c-sh-r -> pf4
	.byte	$FF		; not used
	.byte	fnkpf3		; c-sh-e
	.byte	'y'
	.byte	$09		; tab.  Use ascii tab here
	.byte	't'
	.byte	fnkpf2		; c-sh-w
	.byte	fnkpf1		; c-sh-q
	.byte	fnkk9		; (F0) c-sh-9
	.byte	$FF		; not used
	.byte	fnkk0		; c-sh-0
	.byte	fnkk7		; c-sh-7
	.byte	$08		; backspace. use ascii backspace?
	.byte	fnkk8		; c-sh-8
	.byte	fnklrw		; c-sh-< left arrow
	.byte	fnkrrw		; c-sh-> right arrow
	.byte	fnkkmin		; (F8)
	.byte	'h'
	.byte	fnkk6		; c-sh-d kp6
	.byte	$FF		; not used
	.byte	fnkcpsl		; caps
	.byte	'g'
	.byte	fnkk5		; c-sh-s kp5
	.byte	fnkk4		; c-sh-a kp4
;
; the code that uses the above table.
;
;	kbdget	-	get a char from keyboard, if any waiting.
;			returns char in A
;			returns Carry clear if got char, set if not
;
kbdget:	lda	CH
	cmp	#$FF		; no char pending?
	beq	kbdnone		; nope, give up
	tax			; get char in x
	lda	keyxl,x		; translate it
;
; if >= $C0, handled internally, don't return anything
;
	pha
	and	#$C0		; mask to 2 hi bits
	tax			; save that
	pla			; get original back
	cpx	#$C0		; internal code?
	beq	kbdspec		; yes, handle specially
	clc
	jmp	kbdret		; no, go return this one
kbdspec:
	cmp	#fnksusp	; suspend?
	bne	kbdspec1	; nope, ignore it
	lda	suspend		; it is; toggle suspend flag
	eor	#$01
	sta	suspend
	jmp	kbdupds
kbdspec1:
	cmp	#fnkcpsl	; caps lock?
	bne	kbdspec2	; nope
	lda	#1
	sta	capslck		; lock on
	jmp	kbdupds		; update stat line
kbdspec2:
	cmp	#fnkcpul	; caps unlock?
	bne	kbdnone		; nope
	lda	#0
	sta	capslck		; lock off
kbdupds:
	jsr	updstat		; update stat line
kbdnone:
	sec			; nope, none here
	lda	#0
kbdret:
	ldx	#$FF		; zap CH
	stx	CH
	bcs	kbd999		; if no char, just go home
	cmp	#0		; function key?
	bmi	kbd998
	ldx	capslck		; caps lock on?
	beq	kbd998		; nope, return as is
	jsr	pncupc		; borrow pathname rtn
kbd998:	clc			; make sure carry clr
kbd999:	rts			; and go home with it

;
;	Function key tables.  All these are 4 bytes long, zero padded,
;	for ease of indexing
;
vt100fk:
	.byte	"[A",0,0	; up
	.byte	"[B",0,0	; down
	.byte	"[D",0,0	; left
	.byte	"[C",0,0	; right
	.byte	"OP",0,0	; pf1
	.byte	"OQ",0,0
	.byte	"OR",0,0
	.byte	"OS",0,0
	.byte	"Op",0,0	; kp0
	.byte	"Oq",0,0
	.byte	"Or",0,0
	.byte	"Os",0,0
	.byte	"Ot",0,0
	.byte	"Ou",0,0
	.byte	"Ov",0,0
	.byte	"Ow",0,0
	.byte	"Ox",0,0
	.byte	"Oy",0,0	; kp9
	.byte	"On",0,0	; kp.
	.byte	"Om",0,0	; kp-
	.byte	"Ol",0,0	; kp,
	.byte	"OM",0,0	; enter
vt52fk:
; fill in later

;
;	fksend:		send an escape seq in terminal mode
;			Code in A
;			pointer to fk table in x,y
;
fksend:
	stx	strptr		; set up pointer to esc data
	sty	strptr+1
	and	#$1F		; trim hi bits
	asl	A
	asl	A		; * 4
	tay			; get idx in y
	lda	#$1B		; send an escape
	jsr	putrs
fksend1:
	lda	(strptr),y	; get a char
	beq	fksend9
	jsr	putrs		; send it
	iny
	jmp	fksend1		; round again
fksend9:
	rts			; done!
;
; end of new kbd code
;

;
; Other handy IO routines
;

;
;	Zap ax1, ax2
;
iozax:	lda	#0
	sta	ICAX1,X
	sta	ICAX2,X
	rts
;
;	Set BA
;
iosba:	sta	ICBAL,X
	tya
	sta	ICBAH,X
	rts
;
;	Set BL
;
iosbl:	sta	ICBLL,X
	tya
	sta	ICBLH,X
	rts
;
; Read a char from IOCB in X
;
chrin:	lda	#GETCHR		; get raw bytes
	sta	ICCOM,X		; in command code
	lda	#0
	tay
	jsr	iosba
	jsr	iosbl
	jsr	CIOV		; go do it
	rts
;
; Write a char (in A) to port in X
;
chrout:	pha			; save the char
	lda	#PUTCHR		; put raw bytes
	sta	ICCOM,X		; in command code
	lda	#0
	tay
	jsr	iosba
	jsr	iosbl
	pla			; get the char back
	jsr	CIOV		; go do it
	rts
;
; OPEN a stream; iocb in X, name in A,Y.  Mode already set
; return status in Y
;
openiocb:
	jsr	iosba
	lda	#OPEN		; open command
	sta	ICCOM,X		; stuff it in
	lda	#0		; zap buf len
	sta	ICBLL,X
	sta	ICBLH,X
	jmp	CIOV		; go do it
;
; Open for input.  IOCB in X, name in A,Y
;
opencin:
	pha			; save name lo
	lda	#OPNIN		; get input code
	sta	ICAX1,X		; shove in aux 1
	lda	#0		; clear 
	sta	ICAX2,X		; aux2
	pla			; get name ptr back
	jmp	openiocb	; go open it
;
; Similar one for output
;
opencout:
	pha			; save name lo
	lda	#OPNOT		; get output code
	sta	ICAX1,X		; shove in aux 1
	lda	#0		; clear 
	sta	ICAX2,X		; aux2
	pla			; get name ptr back
	jmp	openiocb	; go open it
;
; Similar one for io
;
opencio:
	pha			; save name lo
	lda	#OPNINO		; get IO code
	sta	ICAX1,X		; shove in aux 1
	lda	#0		; clear 
	sta	ICAX2,X		; aux2
	pla			; get name ptr back
	jmp	openiocb	; go open it
;
; And one for dirlists
;
opencdir:
	pha			; save name lo
	lda	#OPNIN|DIRECT	; get directory please
	sta	ICAX1,X		; shove in aux 1
	lda	#0		; clear 
	sta	ICAX2,X		; aux2
	pla			; get name ptr back
	jmp	openiocb	; go open it
;
; Close IOCB, in X
;
closec:
	lda	#CLOSE		; close command code
	sta	ICCOM,X
	jmp	CIOV		; go do it
;
; Open screen iocb to screen.
;
openscr:
	ldx	#scrchan	; screen iocb please
	cpx	#0		; 0?
	beq	openscr9	; yup, atari OS leaves 0 open to E:, so exit
	lda	#<scrname	; E:
	ldy	#>scrname
	jsr	opencout	; open for output.
openscr9:
	lda	SDLSTL		; remember display list addr for when we change
	sta	scraedl
	lda	SDLSTH
	sta	scraedl+1
	rts
;
; Put a byte to the screen.  char in A.
;
sputch:
	ldx	#scrchan
	jmp	chrout
;
; Out a string to screen, nul terminated.
; string pointer in X,Y.  Uses strptr.  Saves all regs
;
	.byte	0		; temp for y
pstrnul:
	pha			; Save A
	stx	strptr		; store lo byte of pointer
	sty	strptr+1	;  hi byte
	txa			; save x
	pha
	tya			; and y
	pha
	ldy	#0
pstrnul1:
	lda	(strptr),y	; get a byte
	beq	pstrnul9	; nul, go home
	sty	pstrnul-1	; save y
	cmp	#ATEOL		; special case these
	beq	pstrnul2
	and	#$7F		; no reverse vid here
	cmp	#cr		; special kludge for ascii cr
	bne	pstrnul2	; nope, go ahead
	lda	#ATEOL		; yup, substitute real EOL
pstrnul2:
;	jsr	sputch		; go put it out
	jsr	scrput		; put it out, general case
	ldy	pstrnul-1	; ignore status, get y back
	iny			; bump to next char
	bne	pstrnul1	; if zero...
	inc	strptr+1	;  bump hi word of ptr
	jmp	pstrnul1
pstrnul9:
	pla			; get y back
	tay
	pla			; get x back
	tax
	pla			; get A back
	rts			; all done.

;
;	Pstreol		Simple-minded version for outputting 
;			ATEOL terminated strings.  Doesn't print the EOL.
;			string must be shorter than 256.
;			Saves A, trashes X,Y
;
pstreol:
	pha			; save a
	stx	strptr		; set string ptr
	sty	strptr+1
	ldy	#0		; init idx
pstreol1:
	lda	(strptr),y	; get a byte
	sty	pstrnul-1	; use that temp, don't need another
	cmp	#ATEOL		; eol?
	beq	pstreol9	; yes, done
;	jsr	sputch		; out it
	jsr	scrput		; put it out, general case
	ldy	pstrnul-1	; get y back
	iny			; bump
	bne	pstreol1	; unless wrap, go back for more
pstreol9:
	pla			; get a back
	rts			; home!
	
;
;
;----------------------------------------------------------------
; 
;ctrl-l.SBTTL	Prompt subroutine
 
;
;	This routine prints the prompt for the program and specifies the
;	reparse address.
;
;		Inputs:		X - L.O. byte address of prompt
;				Y - H.O. byte address of prompt
;
;		Outputs:
;
;		Registers destroyed:	A,X,Y
;
 
prompt: pla			; Get Low order byte of return address
	sta	cmrprs		; Save that half of reparse address
	pla			; Get High order byte
	sta	cmrprs+1	; Save the half
	pha			; Restore the return
	lda	cmrprs		;  address to
	pha			;    the stack
	clc			; Clear the carry
	adc	#1		; Increment this address since it is one
	sta	cmrprs		;	short of the desired target.
	lda	cmrprs+1	; Account for the carry, if any
	adc	#0		;		...
	sta	cmrprs+1	;		...
	stx	cm_rty		; Save the address of the prompt in
	sty	cm_rty+1	; pointer to the ctrl/r text
	tsx			; Get the stack pointer
	stx	cmostp		; Save it for later restoral
	lda	#<cmbuf		; Get low order byte of buffer address
	sta	cm_bfp		; Init start of text buffer
	sta	cm_ptr		; Init next input to be parsed
	lda	#>cmbuf		; Get high order byte of buffer address
	sta	cm_bfp+1	; H.O. byte of text buffer pointer
	sta	cm_ptr+1	; H.O. byte of next input pointer
	lda	#0		; Clear AC
	sta	cmaflg		; Clear the flags
	sta	cmccnt		;
	sta	cmsflg		;
	jsr	prcrlf		; Print crlf
	ldx	cm_rty		; Get L.O. byte of prompt address to be passed
	ldy	cm_rty+1	; Get H.O. byte of prompt address
	jsr	prstr		; Print the prompt
	rts			; Return
 
;ctrl-l.SBTTL	Repars routine
 
;
;	This routine sets stuff up to reparse the current command
;	buffer.
;
;		Input:
;
;		Output:		Reinitialize comnd pointers and flags
;
;		Registers destroyed:	A,X
;
 
repars: ldx	cmostp		; Fetch old Stack pointer
	txs			; Make it the current one
	lda	#<cmbuf		; Get L.O. byte address of cmbuf
	sta	cm_ptr		; Stuff it
	lda	#>cmbuf		; Get H.O. byte address of cmbuf
	sta	cm_ptr+1	; The buffer pointer is now reset
	lda	#0		; Clear AC
	sta	cmsflg		; Clear the space flag
	jmp	(cmrprs)	; Jump at the reparse address
 
;.SBTTL	Prserr routine
 
;
;	This routine is used when a parsing error occurs. It resets ALL
;	of the pointers and flags and then goes to the reparse address.
;
;		Input:
;
;		Output:
;
;		Registers destroyed:
;
 
prserr:	lda	cm_ptr		; Store old command line pointer
	sta	cmoptr		;		...
	lda	cm_ptr+1	; 		...
	sta	cmoptr+1	; 		...
	lda	cmccnt		; Store old character count
	sta	cmocnt		;		...
	lda	#$FF		; Set the failure flag
	sta	cmcffl		;		...
	ldx	cmostp		; Fetch the saved SP
	txs			; Make it the current one
	lda	#<cmbuf		; Set up the command buffer
	sta	cm_bfp		; 	address in both the
	sta	cm_ptr		; 	buffer pointer and the 
	lda	#>cmbuf		;	next input pointer.
	sta	cm_bfp+1	;		...
	sta	cm_ptr+1	;		...
	lda	#0		; Clear AC
	sta	cmaflg		; Zero the action flag
	sta	cmccnt		;	the character count
	sta	cmsflg		;	and the space flag
	jsr	prcrlf		; Print a crelf
	ldx	cm_rty		;  Get the address of the prompt
	ldy	cm_rty+1	;		...
	jsr	prstr		; Reprint the prompt
	jmp	(cmrprs)	; Jump at the reparse address
 
;ctrl-l.SBTTL	COMND - Entry point for command Jsys stuff
 
;
;	COMND routine - This routine checks the code in the AC for
;	what parse type is wanted and then dispatches to an appropriate
;	routine to look for it. Additional information is located in
;	CMINF1 and CMINF2 on page zero.
;
;		Input:		A - parse type
;				X,Y - optional parameters
;
;		Output:		A - +1 = success
;				    +4 = failure (assumes JMP after call)
;
;		Registers destroyed:	A
;
 
comand: sta	cmstat		; Save what we are parsing
	stx	cmprmx		; Save these parameters also
	sty	cmprmy		;		...
	cmp	#cmini		; Initialize the world?
	bne	comn0		; No, handle like a normal parse type
	jmp	prompt		; Do the prompt routine to set things up
comn0:  jsr	cminbf		; Get characters until action or erase
	cmp	#cmcfm		; Parse a confirm?
	bne	comn1		; Nope
	jmp	cmcfrm		; Yes, try for the confirm
comn1:  cmp	#cmkey		; Parse a keyword perhaps?
	bne	comn2		; No, next item
	jmp	cmkeyw		; Get the keyword
comn2:  cmp	#cmifi		; Parse an input file?
	bne	comn3		; No, try next one
	jmp	cmifil		; Get the input file
comn3:  cmp	#cmofi		; Parse an output file?
	bne	comn4		; No, try next
	jmp	cmofil		; Get the output file
comn4:  cmp	#cmswi		; Parse a switch?
	bne	comn5		; No, try next again
	jmp	cmswit		; Yes, do a switch
comn5:  cmp	#cmnum		; Parse an integer?
	bne	comn6		; No, try next type
	jmp	cminum		; Do the parse integer routine
comn6:  cmp	#cmfls		; Parse a floating point?????
	bne	comn7		; Nope, thats it for types
	jmp	cmflot		; Yes, go get a floating point number
comn7:	cmp	#cmtxt		;  Parse for an unquoted string?
	bne	comn8		;  Nope, go try last type
	jmp	cmunqs		;  Go parse the string
comn8:	cmp	#cmtok		;  Parse for a single character?
	bne	comn9		;  Nope, no more parse types
	jmp	cmtokn		;  Go parse for char
comn9:  ldx	#<cmer00	; Error 0 - Bad parse type
	ldy	#>cmer00
	jsr	prstr		; Print the error text
	lda	#4		; Fail
	rts			; Return to caller
 
;ctrl-l.SBTTL	Cmcfrm routine - get a confirm
 
;
;	This routine tries to get a confirm from the command input
;	buffer.
;
;		Input:  Cm_ptr  - Beginning of next field to be parsed
;
;		Output: On success, routine skip returns
;
;		Registers destroyed:	A,X,Y
;
 
cmcfrm: lda	cm_ptr		; Save the current comand line pointer
	pha			;	on the stack in case the user
	lda	cm_ptr+1	;	wants to parse for an alternate item
	pha			;
cmcfr0: jsr	cmgtch		; Get a character
	cmp	#0		; Is it negative?
	bpl	cmcfrr		; No, fail
	and	#$7F		; Yes, zero the sign bit
	cmp	#esc		; An escape?
	bne	cmcfr2		; No, continue
	jsr	bell		; Sound bell, er
	lda	#0		; Clear AC
	sta	cmaflg		; Clear the action flag
	sec			; Set carry for subtraction
	lda	cm_bfp		; Get L.O. byte
	sbc	#1		; Decrement it once
	sta	cm_bfp		; Store it back
	sta	cm_ptr		; Make this pointer look like the other one
	bcs	cmcfr1		; If set, we don't have to do H.O. byte
	dec	cm_bfp+1	; Adjust H.O. byte
cmcfr1: lda	cm_bfp+1	; Move this to H.O. byte of the other pointer
	sta	cm_ptr+1
	dec	cmccnt		; Decrement the character count
	jmp	cmcfr0		; Try again.
cmcfr2: cmp	#'?'		; User need help??
	bne	cmcfr3		; Nope
	jsr	cout		; Print the '?'
	ldx	#<cmin00	; Get address of some help info
	ldy	#>cmin00	;
	jsr	prstr		; Print it.
	jsr	prcrlf		; Print the crelf
	ldx	cm_rty		;  Get address of prompt
	ldy	cm_rty+1	;
	jsr	prstr		; Reprint the prompt
	lda	#0		; Clear AC
	ldy	#0		; Clear Y
	sta	(cm_ptr),y	; Drop null at end of command buffer
	sec			; Set carry for subtraction
	lda	cm_bfp		; Get L.O. byte
	sbc	#1		; Decrement it
	sta	cm_bfp		; Store it back
	lda	cm_bfp+1	; Now do H.O. byte
	sbc	#0		;
	sta	cm_bfp+1	;
	ldx	#<cmbuf		; Get address of the command buffer
	ldy	#>cmbuf		;
	jsr	prstr		; Reprint the command line
	lda	#0		; Clear AC
	sta	cmaflg		; Action flag off
	jmp	repars		; Go reparse the line
cmcfr3: cmp	#ffd		; Is it a form feed?
	bne	cmcfr4		; Nope
	jsr	scred2		; Yes, blank the screen
	ldx	#0
	ldy	#0
	jsr	scrplt		; and home the cursor
cmcfr4: pla			; Since this succeeded, we can flush the
	pla			;	old command line pointer
	lda	#0		;  Reset the failure flag
	sta	cmcffl		; 
	jmp	rskp		; Do a return skip
 
cmcfrr: pla			;  Restore the old comand line pointer
	sta	cm_ptr+1	; 
	sta	cmoptr+1	; 
	pla			; 
	sta	cm_ptr		; 
	sta	cmoptr		; 
	lda	cmccnt		;  Save count in case of <ctrl/H>
	sta	cmocnt		; 
	lda	#$FF		;  Set failure
	sta	cmcffl		; 
	rts			; Return
 
;ctrl-l.SBTTL	Cmkeyw - Try to parse a keyword next
 
;
;	This routine tries to parse a keyword from the table
;	pointed to by cminf1. The keywords must be in alphabetical
;	order. The routine returns the two bytes of data associated
;	with the keyword. The format of the table is as follows:
;
;	addr:	.byte	n	; Where n is the # of entries in the table.
;		.byte	m	; m is the size of the next keyword
;		.asciz  /string/; keyword ending in a null
;		.byte	a,b	; 16 bits of data related to keyword
;
;		Input:  Cminf1- Pointer to keyword table
;
;		Output: X-	byte a
;			Y-	byte b
;
;		Registers destroyed:	A,X,Y
;
 
cmkeyw: lda	cm_ptr		; Save the old comand line pointer
	pha			;
	lda	cm_ptr+1
	pha			;
	lda	#0		;  Clear the 'real character' flag
	sta	cmfrcf		; 
	lda	cminf1		; Copy to address of
	sta	cmptab		;	the keyword table
	clc			; Clear the carry
	adc	#1		; Add one to the addr. (pass the table length)
	sta	cmkptr		; Save the keyword pointer (L.O. byte)
	lda	cminf1+1	; Get H.O. byte
	sta	cmptab+1	; Save a copy of that
	bcc	cmkey1		; Carry?
	adc	#0		; Add in the carry for cmkptr
cmkey1: sta	cmkptr+1	; Save it
	ldy	#0		; Clear Y
	lda	(cmptab),y	; Get the table length
	sta	cmentr		; Save number of entries in the table
cmky10:	jsr	cmgtch		; Get first character
	cmp	#0		; Was the first character a terminator?
	bmi	cmky11		; Yup, the saved pointer does not get decr.
	sec			; Make sure saved buffer pointer is correct
	lda	cm_ptr		; Now, reset it back one character for later
	sbc	#1		;
	sta	cm_ptr		;
	sta	cmsptr		;
	lda	cm_ptr+1	;
	sbc	#0		;
	sta	cm_ptr+1	;
	sta	cmsptr+1	;
	jmp	cmkey2		; Continue
cmky11: ldy	cm_ptr		; Just move the pointer to the save area
	sty	cmsptr		;
	ldy	cm_ptr+1	;
	sty	cmsptr+1	;
	and	#$7F		;[EL] ????
	cmp	#esc		;  Was the first terminator an escape?
	beq	cmky12		;  Yes, handle this
	jmp	cmkey2		;  No, continue
cmky12:	lda	#cmfdff		;  Is there a default?
	bit	cmprmy		; 		...
	bne	cmky13		;  Yes, go copy it
	lda	#0		;  Shut the action flag
	sta	cmaflg		; 		...
	jsr	bell		;  Yes, start by feeping terminal
	sec			;  Set the carry bit for subtraction
	lda	cm_bfp		;  Take L.O. byte of buffer pointer
	sbc	#1		;  Decrement it (back up before escape)
	sta	cm_bfp		;  Store it
	sta	cm_ptr		;  And stuff it in next input char pointer
	bcs	cmkync		;  If carry is clear, we are done
	dec	cm_bfp+1	;  Do the carry on H.O. byte
cmkync:	lda	cm_bfp+1	;  Copy this to next char to parse pointer
	sta	cm_ptr+1	; 		...
	jmp	cmky10		;  Continue by fetching a character again
cmky13:	lda	#0		;  Zero the action flag
	sta	cmaflg		; 		...
	jmp	cmcpdf		;   Do the copy	
cmkey2: lda	cmentr		; Get number of entries left
	cmp	#0		; 0 entries left?
	bne	cmky21		; No, go try next entry
	pla			; Fetch back to previous comand line pointer
	sta	cm_ptr+1	;		...
	sta	cmoptr+1	;		...
	pla			;		...
	sta	cm_ptr		;		...
	sta	cmoptr		;		...
	lda	cmccnt		; Save count in case of <ctrl/H>
	sta	cmocnt		;		...
	lda	#$FF		; Set the command-failure flag
	sta	cmcffl		;		...
	rts
cmky21: ldy	#0		; Clear Y
	lda	(cmkptr),y	; Get length of keyword
	sta	keylen		; Store it
	lda	cmkptr		; Get the new table pointer
	sta	cmspt2		;	and save it for later
	lda	cmkptr+1	;		...
	sta	cmspt2+1	;		...
	inc	cmkptr		; Increment the L.O. byte once
	bne	cmkey3		; If it didn't wrap, there is no carry
	inc	cmkptr+1	; There was a carry, add it in.
cmkey3: dec	keylen		; Decrement the number of chars. left
	lda	keylen		; Get the remaining length
	cmp	#$FF		; Have we passed the end
	bpl	cmk3a		; No
	jmp	cmkey5		; Yes
cmk3a:  jsr	cmgtch		; Get a character
	cmp	#0		; Is it a terminator?
	bmi	cmk3b		; Yup, it is negative
	jmp	cmkey4		; Nope, it's positive
cmk3b:  and	#$7F		; Shut off the minus bit
	cmp	#'?'		; Need any help?
	bne	cmky31		; Nope
	jsr	cout		; And print the question mark
	lda	#0		; Clear AC
	sta	cmaflg		; Clear the action flag
	lda	cmstat		; Get saved parse type
	cmp	#cmswi		; Are we really doing a switch?
	beq	cmk3b1		; Yes, give that message instead
	ldx	#<cmin01	; L.O. byte addr of informational message
	ldy	#>cmin01	; H.O. byte of addr
	jmp	cmk3b2		; Go print the message
cmk3b1: ldx	#<cmin02	; Load address of switch message
	ldy	#>cmin02	;		...
cmk3b2: jsr	prstr		; Print the message
	jsr	prcrlf		; Print a crelf
	jsr	cmktp		;	and the valid entries in keyword table
	jsr	prcrlf		; Print another crlf
	lda	#cmfehf		;  Load extra help flag
	bit	cmprmy		;  Test bit
	beq	cmk3b3		;  No extra help
	jsr	cmehlp		;  Go give extra help
cmk3b3:	ldx	cm_rty		; Get  address of prompt
	ldy	cm_rty+1	; 
	jsr	prstr		; Reprint the prompt
	lda	#0		; Clear AC
	ldy	#0		; Clear Y
	sta	(cm_ptr),y	; Stuff a null in the buffer at that point
	sec			; Set the carry
	lda	cm_bfp		; Get ready to decrement buffer pointer
	sbc	#1		; Subtract it
	sta	cm_bfp		; Store it
	bcs	cmky3a		; Do we have to account for carry
	dec	cm_bfp+1	; Decrement the H.O. byte
cmky3a: ldx	#<cmbuf		; Get  address of buffer
	ldy	#>cmbuf		;
	jsr	prstr		; Reprint the command line
	jmp	repars		; Go reparse all of it
cmky31: cmp	#esc		; escape character?
	beq	cmk3c		; Yup, process it
	jmp	cmky35		; Nope.
cmk3c:  lda	#0		; Clear AC
	sta	cmaflg		; Clear action flag
	lda	keylen		; Save on the stack, the
	pha			;	keylength
	lda	cmentr		;	number of entries left
	pha			;		...
	lda	cmkptr		;	L.O. byte of keyword table pointer
	pha			;		...
	lda	cmkptr+1	;	H.O. byte of keyword table pointer
	pha			;		...
	jsr	cmambg		; Is it ambiguous?
	 jmp	cmky32		; Nope
	lda	#cmfdff		;  Load the default-present flag
	bit	cmprmy		;  Check against flags
	beq	cmk3d		;  No, complain to user
	lda	cmfrcf		;  Have we seen a real character yet?
	bne	cmk3d		;  No, tell user
	jmp	cmcpdf		;  Yes, go copy the default
cmk3d:	jsr	bell		; Yes, start by feeping terminal
	sec			; Set the carry bit for subtraction
	lda	cm_bfp		; Take L.O. byte of buffer pointer
	sbc	#1		; Decrement it (back up before escape)
	sta	cm_bfp		; Store it
	sta	cm_ptr		; And stuff it in next input char pointer
	bcs	cmky3b		; If carry is clear, we are done
	dec	cm_bfp+1	; Do the carry on H.O. byte
cmky3b: lda	cm_bfp+1	; Copy this to the next char to parse pointer
	sta	cm_ptr+1	;		...
	dec	cmccnt		; Decrement the character count
	pla			;		...
	sta	cmkptr+1	; Restore the keyword table pointer
	pla			;		...
	sta	cmkptr		;
	pla			;
	sta	cmentr		;	Number of entries left in table
	pla			;		...
	sta	keylen		;	And the remaining keylength
	inc	keylen		; Adjust the keylength to make it correct
	jmp	cmkey3		; And go back to try again
cmky32: ldy	#0		; Clear Y
	sec			; Set the carry flag
	lda	cm_bfp		; Move buffer pointer behind the escape
	sbc	#1		;		...
	sta	cm_bfp		;		...
	sta	cm_ptr		;		...
	bcs	cmk32c		;		...
	dec	cm_bfp+1	; Have to adjust the H.O. byte
cmk32c: lda	cm_bfp+1	;		...
	sta	cm_ptr+1	;		...
	pla			; Fetch the old keytable pointer
	sta	cmkptr+1	;		...
	pla			;		...
	sta	cmkptr		;		...
	pha			; Now push it back on the stack
	lda	cmkptr+1	;		...
	pha			;		...
cmky33: lda	(cmkptr),y	; Get next character
	cmp	#0		; Done?
	beq	cmky34		; Yes
	tax			; No, hold on to the byte
	clc			; Clear the carry flag
	lda	cmkptr		; Adjust the keyword pointer up one place
	adc	#1		; Do L.O. byte
	sta	cmkptr		; Store it
	bcc	cmky3c		; Carry?
	inc	cmkptr+1	; Yes, increment H.O. byte
cmky3c: txa			; Get the data
	sta	(cm_ptr),y	; Stuff it in the buffer
	clc			; Clear the carry flag again
	lda	cm_ptr		; Get L.O byte of buffer pointer
	adc	#1		; Increment it
	sta	cm_ptr		; Store it
	bcc	cmky3d		; Carry?
	inc	cm_ptr+1	; Increment H.O. byte
cmky3d: inc	cmccnt		; Increment character count
	jmp	cmky33		; Get next character from table
cmky34: inc	cmccnt		; Incrment the character count
	lda	#$A0		; Clear AC (this is a terminator!)
	sta	(cm_ptr),y	; Stuff a null in the buffer
	ldx	cm_bfp		; Get L.O. byte of buffer pointer
	ldy	cm_bfp+1	;	and H.O byte - save these for later
	clc			; Clear carry
	lda	cm_ptr		; Increment next char of input pointer
	adc	#1		;		...
	sta	cm_ptr		;		...
	sta	cm_bfp		;		...
	bcc	cmky3e		; Carry?
	inc	cm_ptr+1	; Do H.O. byte
cmky3e: lda	cm_ptr+1	; Make buffer pointer match next char pointer
	sta	cm_bfp+1	;		...
	sty	savey		; Hold y for a bit
	lda	#0		; Put a null in the buffer to terminate string
	ldy	#0		;		...
	sta	(cm_ptr),y	;		...
	ldy	savey		; Get Y value back
	jsr	prstr		; Print remainder of keyword
	pla			; Restore the
	sta	cmkptr+1	;	H.O. byte of keyword table pointer
	pla			;		...
	sta	cmkptr		; 	L.O. byte of keyword table pointer
	pla			;		...
	sta	cmentr		;	Number of entries left in table
	pla			;		...
	sta	keylen		;	And the remaining keylength
	jmp	cmky37		; Go get some data to return
cmky35: lda	cmkptr		; Save on the stack the  keyword table pointer
	pha			;
	lda	cmkptr+1	;
	pha			;		...
	lda	keylen		;	The keylength
	pha			;		...
	jsr	cmambg		; Check for ambiguity
	 jmp	cmky36		; Not ambiguous
	ldx	#<cmer01	; Get addr of ambiguous error
	ldy	#>cmer01	;		...
	jsr	prstr		; Print the error message
	jmp	prserr		; Go do parsing error stuff
cmky36: pla			; Fetch off of the stack 
	sta	keylen		;	remaining keylength
	pla			;		...
	sta	cmkptr+1	;	H.O. byte of keyword table address
	pla			;		...
	sta	cmkptr		; 	L.O. byte of keyword table address
cmky37: inc	keylen		; Adjust the remaining keylength
	inc	keylen		;		...
	clc			; Clear the carry flag
	lda	cmkptr		; Get the keyword table pointer
	adc	keylen		; Add in remaining keylength
	sta	cmkptr		; Store it
	bcc	cmky3f		; Carry?
	inc	cmkptr+1	; Yes, adjust H.O. byte
cmky3f: ldy	#0		; Make sure Y is clear
	lda	(cmkptr),y	; Get first data byte
	tax			; Put it in X
	iny			; Up the index once
	lda	(cmkptr),y	; Get the second data byte
	tay			; Put that in Y
	pla			; Flush the old comand line pointer
	pla			;		...
	lda	#0		; Reset the failure flag
	sta	cmcffl		; 
	jmp	rskp		; Return skip means it succeeds!
cmkey4: cmp	#$41		; Check range for upper case
	bmi	cmky41		;		...
	cmp	#$5b		;		...
	bpl	cmky41		;		...
	ora	#$20		; Cutesy way to convert to lower case
cmky41: sta	cmwrk3		; Save the character
	lda	#$FF		;  Set the 'real character' flag
	sta	cmfrcf		; 
	ldy	#0		; Clear Y again
	lda	(cmkptr),y	; Get next keyword byte
	sta	cmwrk4		; Hold that for now
	clc			; Clear the carry flag
	lda	cmkptr		; Get L.O. byte of keyword pointer
	adc	#1		; Add one
	sta	cmkptr		; Store it
	bcc	cmky4a		; Need to do carry?
	inc	cmkptr+1	; Yes, do H.O. byte
cmky4a: lda	cmwrk3		; Get input character
	cmp	cmwrk4		; Does it match keyword character?
	bne	cmkey5		; No, advance to next keyword in table
	jmp	cmkey3		; Yup, try next input byte
cmkey5: inc	keylen		; Adjust keylength so that it is correct
	inc	keylen		;		...
	inc	keylen		;		...
	clc			; Clear carry
	lda	cmkptr		; Ok, get keyword pointer and
	adc	keylen		; Add the remaining keylength
	sta	cmkptr		; Store it
	bcc	cmky5a		; See if we have to do carry
	inc	cmkptr+1	; Yes, increment H.O. byte
cmky5a: dec	cmentr		; Decrement the number of entries left
	lda	cmsptr		; Get the saved buffer pointer and
	sta	cm_ptr		;	restore it
	lda	cmsptr+1	;		...
	sta	cm_ptr+1	;		...
	jmp	cmkey2		; Try to parse this keyword now
 
;ctrl-l.SBTTL	Cmambg - check if keyword prefix is ambiguous
 
;
;	This routine looks at the next keyword in the table and
;	determines if the prefix entered in the buffer is ambiguous
;	or not. If it is ambiguous, it skip returns, otherwise it
;	returns normally.
;
;		Input:  Cmentr- number of entries left in table
;			Cmkptr- current keyword table pointer
;			Keylen- remaining keyword length
;
;		Output: If ambiguous, does a skip return
;
;		Registers destroyed:	A,X,Y
;
 
cmambg: dec	cmentr		; Start by decrementing remaining entries
	bpl	cma1		; We still have stuff left
	rts			; Nothing left, it can't be ambiguous
cma1:	inc	keylen		; Adjust this up by one
	lda	keylen		; Save character count
	sta	cmwrk3		;		...
	clc			; Clear the carry
	adc	#3		; Adjust the keylength to include terminator
	sta	keylen		;	and data bytes
	clc			; Clear carry
	lda	cmkptr		; Up the keyword table pointer
	adc	keylen		;	by remaining keylength
	sta	cmkptr		; Save it
	bcc	cma2		; Need to adjust H.O byte?
	inc	cmkptr+1	; Yes, do it
cma2:	ldy	#0		; Clear Y
	lda	(cmkptr),y	; Get keyword length
	sta	cmwrk4		; Hold that byte
	clc			; Clear carry
	lda	cmkptr		; Advance keyword table pointer
	adc	#1		;		...
	sta	cmkptr		;		...
	bcc	cma3		;		...
	inc	cmkptr+1	;		...
cma3:	lda	(cmspt2),y	; Get previous keyword length
	sec			; Set carry
	sbc	cmwrk3		; Subtract number of characters left
	beq	cmambs		;  If test len is 0, don't bother trying
	sta	cmtlen		; This is the testing length
	cmp	cmwrk4		; Check this against length of new keyword
	bmi	cmamb0		; This may be ambiguous
	rts			; Test length is longer, cannot be ambiguous
cmamb0: ldy	#0		; Clear Y
cmamb1: dec	cmtlen		; Decrement the length to test
	bpl	cma4		; Still characters left to check
cmambs:	jmp	rskp		;  The whole thing matched, it is ambiguous
cma4:	lda	(cmkptr),y	; Get next character of keyword
	sta	cmwrk3		; Hold that for now
	lda	(cmsptr),y	; Get next parsed character
	iny			; Up the pointer once
	cmp	#$61		; Check the range for lower case
	bmi	cmamb2		;		...
	cmp	#$7B		;		...
	bpl	cmamb2		;		...
	and	#$5F		; Capitalize it
cmamb2:	and	#$7F		; Reset the H.O. bit
	cmp	cmwrk3		; Same as keyword table character
	beq	cmamb1		; Yup, check next character
	rts			; Nope, prefix is not ambiguous
 
 
;ctrl-l.SBTTL	Cmktp - print entries in keyword table matching prefix
 
;
;	This routine steps through the keyword table passed to cmkeyw
;	and prints all the keywords with the prefix currently in the
;	command buffer. If there is no prefix, it issues an error.
;
;		Input:  Cmptab- ptr to beginning of table
;			Cmsptr- saved buffer pointer
;			Cm_ptr- current buffer pointer
;
;		Output: List of possible keywords to screen
;
;		Registers destroyed:	A,X,Y
;
 
cmktp:  lda	cmptab		; Get a copy of the pointer
	sta	cminf2		;	to the beginning of
	lda	cmptab+1	;	the current keyword table
	sta	cminf2+1	;		...
	ldy	#0		; Clear Y
	sty	cmscrs		; Clear the 'which half of screen' switch
	sty	cmwrk3		; Clear the 'print any keywords?' switch
	lda	(cminf2),y	; Get the table length
	sta	cmwrk1		;	and save it in a safe place
	sec			; Prepare for some subtracting
	lda	cm_ptr		; Get difference between the current pointer
	sbc	cmsptr		;	and pointer to beginning of keyword
	sta	cmtlen		; That is how much we must test
	clc			; Clear carry
	lda	cminf2		; Increment the pointer to the table
	adc	#1		;		...
	sta	cminf2		;		...
	bcc	cmktp1		; Need to increment H.O. byte?
	inc	cminf2+1	; Yup
cmktp1: dec	cmwrk1		; 1 less keyword to do
	lda	cmwrk1		; Now...
	bmi	cmkdon		; No keywords left, we are done
	lda	(cminf2),y	; Get the keyword length
	sta	cmkyln		;	and stuff it
	clc			; Clear carry
	lda	cminf2		; Increment pointer to table again
	adc	#1		;		...
	sta	cminf2		;		...
	bcc	cmktp2		; Need to up the H.O. byte?
	inc	cminf2+1	; Yup
cmktp2: lda	cmtlen		; Get test length
	beq	cmktp3		; If test length is zero, just print keyword
cmkp21: lda	(cminf2),y	; Get character from table
; zzz do case-insensitive compare here
	cmp	(cmsptr),y	; Compare it to the buffer character
	bne	cmadk		; Nope, advance to next keyword
	iny			; Up the index
	cpy	cmtlen		; Compare with the test length
	bmi	cmkp21		; Not yet, do next character
cmktp3: jsr	cmprk		; Print the keyword
 
cmadk:  inc	cmkyln		; Adjust cmkyln to include terminator and data
	inc	cmkyln		;		...
	inc	cmkyln		;		...
	clc			; Clear the carry
	lda	cminf2		; Get the L.O. byte
	adc	cmkyln		; Add in the keyword length
	sta	cminf2		; Store it away
	bcc	cmadk2		; Need to do the H.O. byte?
	inc	cminf2+1	; Yup
cmadk2: ldy	#0		; Zero the index
	jmp	cmktp1		; Go back to the top of the loop
 
cmkdon: lda	cmwrk3		; See if we printed anything
	bne	cmkdn2		; Yup, go exit
	lda	cmstat		; Are we parsing switches or keywords?
	cmp	#cmswi		;		...
	beq	cmkdse		; The error should be for switches
	ldx	#<cmer03	; Nope, get address of error message
	ldy	#>cmer03	;		...
	jmp	cmkdn1		; Go print the message now
cmkdse: ldx	#<cmer04	; Get address of switch error message
	ldy	#>cmer04	;		...
cmkdn1: jsr	prstr		; Print error
	jsr	prcrlf		; Print a crelf
cmkdn2: lda	cmscrs		; Where did we end up?
	beq	cmkdn3		; Beginning of line, good
	jsr	prcrlf		; Print a crelf
cmkdn3: rts			; Return
 
;ctrl-l;
;	Cmprk - prints one keyword from the table. Consults the
;		cmscrs switch to see which half of the line it
;		is going to and acts accordingly.
;
;		Input:  Cmscrs- Which half of screen
;			Cminf2- Pointer to string to print
;
;		Output: print keyword on screen
;
;		Registers destroyed:	A,X,Y
;
 
cmprk:  lda	#on		; Make sure to tell them we printed something
	sta	cmwrk3		; Put it back
	lda	cmstat		; Get saved parse type
	cmp	#cmswi		; Is it a switch we are looking for?
	bne	cmpr2		;
	lda	#'/'		; Yes, do not forget slash prefix
	jsr	cout		; Print slash
cmpr2:  ldx	cminf2		; L.O. byte of string pointer
	ldy	cminf2+1	; H.O. byte of string pointer
	jsr	prstr		; Print the keyword
	lda	cmscrs		; Where were we?
	bne	cmprms		; Mid screen
	jsr	screl0		; Clear to end of line
	sec			;[37] Get cursor coordinates
	jsr	ploth		;[37]		...
;	ldy	#$14		; Advance cursor to middle of screen
	ldx	#$14		; Advance cursor to middle of screen
	clc			;[DD]		...
	jsr	ploth		;[DD][26]	...
	jmp	cmprdn		; We are done
cmprms: jsr	prcrlf		; Print a crelf
cmprdn: lda	cmscrs		; Flip the switch now
	eor	#$01
	sta	cmscrs		; Stuff it back
	rts			; Return
 
;ctrl-l.SBTTL	Cmswit - try to parse a switch next
 
;
;	This routine tries to parse a switch from the command buffer. It
;	first looks for the / and then calls cmkeyw to handle the keyword
;	lookup.
;
;		Input:  Cminf1- Address of keyword table
;
;		Output: X-	byte a
;			Y-	byte b
;
;		Registers destroyed:	A,X,Y
;
; well, this is pretty gross.  This sucker appears never to be
; called; undoubtedly cause this is a general purpose command parser
; from elsewhere.  I'm leaving the code here, but commented out in
; case anyone ever wants to use it;
; 
cmswit: brk			; [jrd] you better put it back before
				;  trying to use it
;	lda	cm_ptr		; Save the old comand line pointer
;	pha			;	user wants to try another item
;	lda	cm_ptr+1	;		...
;	pha			;		...
;cmswi0: jsr	cmgtch		; Go get a character
;	cmp	#0		; Action?
;	bmi	cmswi1		; Yes, process it
;	jmp	cmswi3		; No, it is a real character
;cmswi1: and	#$7F		; Turn off the minus
;	cmp	#'?'		; Does the user need help?
;	bne	cmsw12		; No
;	jsr	cout		; And print the question mark
;	lda	#0		; Clear AC
;	sta	cmaflg		; Clear Action flag
;	ldx	#<cmin02	; Low order byte addr of info message
;	ldy	#>cmin02	; High order byte addr of info message
;	jsr	prstr		; Print the message
;	jsr	prcrlf		; Print a crelf
;	jsr	cmktp		; Any valid entries from keyword table
;	jsr	prcrlf		; And another crelf
;	lda	#cmfehf		;  Load extra help flag
;	bit	cmprmy		;  Test bit
;	beq	cmsw10		;  No extra help
;	jsr	cmehlp		;  Go give extra help
;cmsw10:	ldx	cm_rty		; Load the address of the prompt
;	ldy	cm_rty+1	;
;	jsr	prstr		; Reprint it
;	lda	#0		; Clear AC
;	ldy	#0		; Clear Y
;	sta	(cm_ptr),y	; Stuff a null at the end of the buffer
;	sec			; Set the carry flag
;	lda	cm_bfp		; Increment buffer pointer
;	sbc	#1		;		...
;	sta	cm_bfp		;		...
;	bcs	cmsw1a		; Borrow?
;	dec	cm_bfp+1	; Yup
;cmsw1a: ldx	#<cmbuf		; L.O. addr of command buffer
;	ldy	#>cmbuf		; H.O. byte
;	jsr	prstr		; Reprint the command line
;	jmp	repars		; Go reparse everything
;cmsw12: cmp	#esc		; Lazy??
;	beq	cmsw2a		; Yes, try to help
;	jmp	cmswi2		; No, this is something else
;cmsw2a: lda	#0		; Clear AC
;	sta	cmaflg		; Clear action flag
;	lda	#cmfdff		;  See if there is a default
;	bit	cmprmy		; 
;	beq	cmswnd		;  No help, tell user
;	jmp	cmcpdf		;  Go copy the default
;cmswnd:	jsr	bell		; Yes, it is ambiguous - ring bell
;	sec			; Set carry
;	lda	cm_bfp		; Decrement buffer pointer
;	sbc	#1		;		...
;	sta	cm_bfp		;		...
;	sta	cm_ptr		; Make this pointer point there too
;	bcs	cmsw2b		; No carry to handle
;	dec	cm_bfp+1	; Do H.O. byte
;cmsw2b: lda	cm_bfp+1	; Now make H.O. byte match
;	sta	cm_ptr+1	;		...
;	dec	cmccnt		; Decrement the character count
;	jmp	cmswi0		; Try again
;cmsw2c: lda	#'/'		; Load a slash
;	jsr	cout		; Print slash
;	clc			; Clear carry
;	lda	cminf1		; Set the keyword table pointer
;	adc	#2		;	to point at the beginning
;	sta	cmkptr		;	of the keyword and move it
;	lda	cminf1+1	;	to cmkptr
;	bcc	cmsw2d		;		...
;	adc	#0		;		...
;cmsw2d: sta	cmkptr+1	;		...
;	ldy	#0		; Clear Y
;	sec			; Set carry
;	lda	cm_bfp		; Increment the buffer pointer
;	sbc	#1		;		...
;	sta	cm_bfp		;		...
;	bcs	cmsw2e		;		...
;	dec	cm_bfp+1	;		...
;cmsw2e: lda	(cmkptr),y	; Get next character
;	cmp	#0		; Done?
;	beq	cmsw13		; Yes
;	tax			; No, hold on to the byte
;	clc			;	while we increment the pointer
;	lda	cmkptr		; Do L.O. byte
;	adc	#1		;		...
;	sta	cmkptr		;		...
;	bcc	cmsw2f		; And, if neccesary
;	inc	cmkptr+1	;	the H.O. byte as well
;cmsw2f: txa			; Get the data
;	sta	(cm_ptr),y	; Stuff it in the buffer
;	clc			; Clear carry
;	lda	cm_ptr		; Increment the next character pointer
;	adc	#1		;		...
;	sta	cm_ptr		;		...
;	bcc	cmsw2g		;		...
;	inc	cm_ptr+1	;		...
;cmsw2g: inc	cmccnt		; Increment the character count
;	jmp	cmsw2e		; Get next character from table
;cmsw13: inc	cmccnt		; Increment the character count
;	lda	#0		; Clear AC
;	sta	(cm_ptr),y	; Stuff a null in the buffer
;	ldx	cm_bfp		; Hold on to this pointer
;	ldy	cm_bfp+1	;	for later printing of switch
;	clc			; Clear carry
;	lda	cm_ptr		; Now make both pointers look like
;	adc	#1		;	(cm.ptr)+1
;	sta	cm_ptr		;		...
;	sta	cm_bfp		;		...
;	bcc	cmsw3a		;		...
;	inc	cm_ptr+1	;		...
;cmsw3a: lda	cm_ptr+1	; Copy H.O. byte
;	sta	cm_bfp+1	;		...
;	jsr	prstr		; Now print string with pointer saved earlier
;;
;; well this is ridiculous...
;;	ldx	#1		; Set up argument
;;	jsr	prbl2		; Print one blank
;	lda	#space		; [jrd] now isn't this 
;	jsr	cout		;  easier?
;cmsw14: clc			; Clear carry
;	lda	cmkptr		; Increment keyword pointer
;	adc	#1		; Past null terminator
;	sta	cmkptr		;		...
;	bcc	cmsw4a		;		...
;	inc	cmkptr+1	;		...
;cmsw4a: ldy	#0		; Clear Y
;	lda	(cmkptr),y	; Get first data byte
;	tax			; Put it here
;	iny			; Up the index
;	lda	(cmkptr),y	; Get second data byte
;	tay			; Put that in Y
;	pla			; Flush the old comand line pointer
;	pla			;		...
;	lda	#0		;  Clear the failure flag
;	sta	cmcffl		; 		...
;	jmp	rskp		; And give a skip return
;cmswi2: ldy	#0		; Clear Y
;	lda	(cminf1),y	; Get length of table
;	cmp	#2		; Greater than 1
;	bmi	cmsw21		; No, go fetch data
;	ldx	#<cmer01	; Yes, fetch pointer to error message
;	ldy	#>cmer01	;		...
;	jsr	prstr		; Print the error
;	jmp	prserr		; And go handle the parser error
;cmsw21: iny			; Add one to the index
;	lda	(cminf1),y	; Get the length of the keyword
;	sta	keylen		; Save that
;	lda	cminf1+1	; Copy pointer to table
;	sta	cmkptr+1	;		...
;	clc			; Get set to increment an address
;	lda	cminf1		; Do L.O. byte last for efficiency
;	adc	keylen		; Add in the keyword length
;	adc	#2		; Now account for table length and terminator
;	sta	cmkptr		; Save the new pointer
;	bcc	cmsw22		; If no carry, continue
;	inc	cmkptr+1	; Adjust H.O. byte
;cmsw22: jmp	cmsw4a		; Go to load data and skip return
;cmswi3: cmp	#'/'		; Is the real character a slash?
;	beq	cmswi4		; Yes, go do the rest
;	tax			; Move the data byte
;	lda	#0		; Clear AC
;	pla			; Fetch back the old comand line pointer
;	sta	cm_ptr+1	;		...
;	sta	cmoptr+1	; 		...
;	pla			;		...
;	sta	cm_ptr		;		...
;	sta	cmoptr		;		...
;	lda	cmccnt		;  Save count in case of <ctrl/H>
;	sta	cmocnt		;  
;	lda	#$FF		;  Set failure  flag
;	sta	cmcffl		; 		...
;	rts			; Fail - non-skip return
;cmswi4: jsr	cmkeyw		; Let Keyw do the work for us
;	 jmp	cmswi5		; We had problems, restore comand ptr and ret.
;	pla			; Flush the old comand pointer
;	pla
;	lda	#0		;  Reset the failre flag
;	sta	cmcffl		; 
;	jmp	rskp		; Success - skip return!
;cmswi5: pla			; Fetch back the old comand line pointer
;	sta	cm_ptr+1	;		...
;	sta	cmoptr+1	; 		...
;	pla			;		...
;	sta	cm_ptr		;		...
;	sta	cmoptr		; 		...
;	lda	cmccnt		;  Save count in case of <ctrl/H>
;	sta	cmocnt		; 
;	lda	#$FF		;  Set failure flag
;	sta	cmcffl		; 
;	rts			; Now return
;
; [jrd] end of commented out switch parser
;
 
;ctrl-l.SBTTL	Cmifil - try to parse an input file spec next
 
;
;	This routine attempts to parse an input file spec.
;
;		Input:  X - Max filename length
;
;		Output: Filename parsed is in buffer pointed to by X,Y
;
;		Registers destroyed:	A,X,Y
;
 
cmifil: inx			;  Increment max file length for tests
	stx	cmprmx		;  Maximum filename length
	lda	cm_ptr		; Save the old comand line pointer in case
	pha			;
	lda	cm_ptr+1	;
	pha			;
	lda	#0		; Zero the
	sta	lenabf		;  length of the atom buffer
cmifl0: ldy	#0		; Zero Y
	lda	#' '   		; Blank the AC 
	ora	#$80		; Make it look like a terminator
cmifi0: sta	atmbuf,y	; Now zero the buffer
	iny			;		...
	cpy	cmprmx  	;  Done?
	bpl	cmifi1		; Yes, start parsing
	jmp	cmifi0		; No, continue blanking
cmifi1: jsr	cmgtch		; Get a character from command buffer
	cmp	#0		; Is it an action character?
	bmi	cmif10		;  Yes, check it out
	jmp	cmifi2		;  No , process it as a normal character
cmif10:	and	#$7F		;  Yes, turn off the minus bit
	cmp	#'?'		; Does the user need help?
	bne	cmif12		; Nope
	jsr	cout		; And print the question mark
	ldy	#0		; Yes
	sty	cmaflg		; Clear the action flag
	ldx	#<cmin03	; Now get set to give the 'file spec' message
	ldy	#>cmin03	;		...
	jsr	prstr		; Print it
	jsr	prcrlf		; Print a crelf
	lda	#cmfehf		;  Load extra help flag
	bit	cmprmy		;  Test bit
	beq	cmifnh		;  No extra help
	jsr	cmehlp		;  Go give extra help
cmifnh:	ldx	cm_rty		;  Set up to reprint the prompt
	ldy	cm_rty+1	;		...
	jsr	prstr		; Do it
	sec			; Set the carry flag for subtraction
	lda	cm_bfp		; Get the buffer pointer
	sbc	#1		; Decrement it once
	sta	cm_bfp		;		...
	bcs	cmif11		; If it's set, we need not do H.O. byte
	dec	cm_bfp+1	; Adjust the H.O. byte
cmif11: dec	cmccnt		; Decrement the character count
	ldy	#0		; Clear Y
	lda	#0		; Clear AC
	sta	(cm_bfp),y	; Stuff a null at the end of the command buffer
	ldx	#<cmbuf		; Now get the address of the command buffer
	ldy	#>cmbuf		;		...
	jsr	prstr		; Reprint the command line
	jmp	cmifi1		; Go back and continue
cmif12: cmp	#esc		; Got an escape?
	bne	cmif13		; No
	lda	#0		; Yup, clear the action flag
	sta	cmaflg		;		...
	lda	#cmfdff		;  Load default-present flag
	bit	cmprmy		;  Test bit
	beq	cmifnd		;  No default
	lda	lenabf		;  Now check if user typed anything
	bne	cmifnd		;  Yup, can't use default
	jmp	cmcpdf		;  Go copy the default
cmifnd:	jsr	bell		; Escape does not work here, ring the bell
	sec			; Set carry for subtraction
	lda	cm_bfp		; Decrement the buffer pointer
	sbc	#1		;	once
	sta	cm_bfp		;		...
	sta	cm_ptr		; Make both pointers look at the same spot
	lda	cm_bfp+1	;		...
	sbc	#0		; H.O. byte adjustment
	sta	cm_bfp+1	;		...
	sta	cm_ptr+1	;		...
	dec	cmccnt		; Decrement the character count
	jmp	repars		;	and go reparse everything
cmif13: lda	lenabf		;  Get the length of the buffer
	cmp	#0		; Is it zero?
	bne	cmif14		; No, continue
	jmp	cmifi9		; Yes, this is not good
cmif14: cmp	cmprmx  	;  Are we over the maximum file length?
	bmi	cmif15		; Not quite yet
	jmp	cmifi9		; Yes, blow up
cmif15: ldy	lenabf		;  Get the filename length
	lda	#nul		;	and stuff a null at that point
	sta	atmbuf,y	; 
	pla			; Flush the old comand line pointer
	pla			;		...
	ldx	#<atmbuf	;  Set up the atombuffer address
	ldy	#>atmbuf	;		...
	lda	#0		;  Reset the failure flag
	sta	cmcffl		; 
	lda	lenabf		;  Load length into AC to be passed back
	jmp	rskp		; No, we are successful
cmifi2: cmp	#sp		;  Bad character?
	bmi	cmifi9		; Yes, blow up
	cmp	#del		; 
	bpl	cmifi9		; This is bad, punt
	cmp	#$61		; Lower case alphabetic?
	bmi	cmifi8		; Don't capitalize if it's not alphabetic
	cmp	#$7B		;		...
	bpl	cmifi8		;		...
	and	#$5F		; Capitalize
cmifi8: ldy	lenabf		;  Set up length of buffer in Y
	sta	atmbuf,y	;  Stuff character in FCB
	inc	lenabf		;  Increment the length of the name
	jmp	cmifi1		; Go back for the next character
cmifi9: pla			; Restore the old comand line pointer
	sta	cm_ptr+1	;  in case the user wants to parse
	sta	cmoptr+1	; 		...
	pla			;	for something else
	sta	cm_ptr		;		...
	sta	cmoptr		; 		...
	lda	cmccnt		;  Save count in case of <ctrl/H>
	sta	cmocnt		; 		...
	lda	#$FF		;  Set failure flag
	sta	cmcffl		; 
	rts
 
;ctrl-l.SBTTL	Cmofil - try to parse an output file spec
 
;
;	This routine attempts to parse an output file spec from the
;	command buffer.
;
;		Input:  cminf1- Pointer to FCB
;
;		Output:
;
;		Registers destroyed:
;
 
cmofil: jmp	cmifil		; Same as parsing input file spec for now
 
;ctrl-l.SBTTL	Cminum - Try to parse an integer number
 
;
;	This routine tries to parse an integer number in the base
;	specified. It will return a 16-bit number in cmintg.
;	Cmintg is formatted H.O. byte first!
;
;		Input:  X-	Base of integer (2<=x<=16)
;
;		Output: Cmintg- 16-bit integer
;
;		Registers destroyed:	A,X,Y
;
 
cminum: lda	cm_ptr		; Save the old comand line pointer
	pha			;		...
	lda	cm_ptr+1	;		...
	pha			;		...
	cpx	#$11		; Are we within the proper range?
	bmi	cmin1		; If so, check high range
	jmp	cmine1		; No, tell them about it
cmin1:  cpx	#2		; Too small of a base??
	bpl	cmin2		; No, continue
	jmp	cmine1		; Base too small, tell them about it
cmin2:  stx	cmbase		; The base requested is good, store it
	lda	#0		; Clear AC
	sta	cmmres		;	and initialize these areas
	sta	cmmres+1	;		...
	sta	cmmres+2	;		...
	sta	cmmres+3	;		...
	sta	cmintg		;		...
	sta	cmintg+1	;		...
	sta	cmintg+2	;		...
	sta	cmintg+3	;		...
cminm1: jsr	cmgtch		; Get next character from command buffer
	cmp	#0		; Is this an action character
	bmi	cmin1a		; Yes, handle it
	jmp	cminm4		; No, look for a digit
cmin1a: and	#$7F		; It is, turn off the H.O. bit
	cmp	#esc		; Is it an escape?
	bne	cminm2		; No, try something else
	lda	#cmfdff		;  Load default-present flag
	bit	cmprmy		;  Test bit
	beq	cminnd		;  No, default
	lda	cmmres		;  Check if user typed anything significant
	ora	cmmres+1	; 		...
	bne	cminnd		;  Yup, can't use default
	jmp	cmcpdf		;  Go copy the default
cminnd:	jsr	bell		; Yes, but escape is not allowed, ring bell
	lda	#0		; Zero
	sta	cmaflg		;	the action flag
	sec			; Set the carry flag for subtraction
	lda	cm_bfp		; Get the command buffer pointer
	sbc	#1		; Decrement it once
	sta	cm_bfp		; Store it away
	sta	cm_ptr		; Make this pointer look like it also
	bcs	cmin11		; If carry set don't adjust H.O. byte
	dec	cm_bfp+1	; Adjust the H.O. byte
cmin11: lda	cm_bfp+1	; Move a copy of this H.O. byte
	sta	cm_ptr+1	;	to this pointer
	dec	cmccnt		; Decrement the character count
	jmp	cminm1		; Go try for another character
cminm2: cmp	#'?'		; Does the user need help?
	bne	cminm3		; If not, back up the pointer and accept
	jsr	cout		; And print the question mark
	ldx	#<cmin05	; Set up the pointer to info message to be
	ldy	#>cmin05	;	printed
	jsr	prstr		; Print the text of the message
	lda	cmbase		; Get the base of the integer number
	cmp	#$0A		; Is it greater than decimal 10?
	bmi	cmin21		; No, just print the L.O. digit
	clc			; Clear the carry
	lda	#1		; Print the H.O. digit as a 1
	adc	#$30		; Make it printable
	jsr	cout		; Print the '1'
	lda	cmbase		; Get the base back
	sec			; Set the carry flag for subtraction
	sbc	#$0A		; Subtract off decimal 10
cmin21: clc			; Clear carry for addition
	adc	#$30		; Make it printable
	jsr	cout		; Print the digit
	jsr	prcrlf		; Print a crelf
	lda	#cmfehf		;  Load extra help flag
	bit	cmprmy		;  Test bit
	beq	cminnh		;  No extra help
	jsr	cmehlp		;  Go give extra help
cminnh:	ldx	cm_rty		; Set up the pointer so we can print the prompt
	ldy	cm_rty+1	;		...
	jsr	prstr		; Reprint the prompt
	lda	#0		; Clear AC
	ldy	#0		; Clear Y
	sta	(cm_ptr),y	; Drop a null at the end of the command buffer
	sec			; Set the carry flag for subtraction
	lda	cm_bfp		; Get the L.O. byte of the address
	sbc	#1		; Decrement it once
	sta	cm_bfp		; Store it back
	bcs	cmin22		; If carry set, don't adjust H.O. byte
	dec	cm_bfp+1	; Adjust H.O. byte
cmin22: ldx	#<cmbuf		; Get the address of the command buffer
	ldy	#>cmbuf		;		...
	jsr	prstr		; Reprint the command buffer
	lda	#0		; Clear the
	sta	cmaflg		;	action flag
	jmp	repars		; Reparse everything
cminm3: ldx	cmmres		;  Move L.O. byte
	ldy	cmmres+1	;  Move H.O. byte
	pla			; Flush the old comand line pointer
	pla			;		...
	lda	#0		;  Reset the failure flag
	sta	cmcffl		; 
	jmp	rskp		;
cminm4: cmp	#$60		; Is this a letter?
	bmi	cmin41		; Nope, skip this stuff
	sec			; It is, bring it into the proper range
	sbc	#$27		;		...
cmin41: sec			; Set carry for subtraction
	sbc	#$30		; Make the number unprintable
	cmp	#0		; Is the number in the proper range?
	bmi	cminm5		; No, give an error
	cmp	cmbase		;		...
	bmi	cminm6		; This number is good
cminm5: pla			; Restore the old comand line pointer
	sta	cm_ptr+1	;		...
	sta	cmoptr		; 		...
	pla			;		...
	sta	cm_ptr		;		...
	sta	cmoptr		; 		...
	lda	cmccnt		;  Save count in case of <ctrl/H>
	sta	cmocnt		; 		...
	lda	#$FF		;  Set failure flag
	sta	cmcffl		; 		...
	rts			; Then return
cminm6: pha			; Save the number to add in
	lda	cmmres+1	; Move the number to multiply
	pha			; 	onto the stack for 
	lda	cmmres		;	call to mul16
	pha			;		...
	lda	#0		; Move base onto the stack (H.O. byte first)
	pha			;		...
	lda	cmbase		;		...
	pha			;		...
	jsr	mul16		; Multiply this out
	pla			; Get L.O. byte of product
	sta	cmmres		; Store it for now
	pla			; Get H.O. byte of product
	sta	cmmres+1	; Store that too
	pla			; Get the digit to add in
	clc			; Clear the carry for the add
	adc	cmmres		; Add in L.O. byte of result
	sta	cmmres		; Store it back
	lda	cmmres+1	; Get the H.O. byte
	adc	#0		; Add in the carry
	sta	cmmres+1	; Save the H.O. byte
	bcs	cmine2		; Wrong, we overflowed
	jmp	cminm1		; Try for the next digit
cmine1: ldx	#<cmer06	; Get the address of the error message
	ldy	#>cmer06	;		...
	jsr	prstr		; Print the error
	jmp	prserr		; Handle the parse error
cmine2: ldx	#<cmer07	; Get the address of the error message
	ldy	#>cmer07	;		...
	jsr	prstr		; Print the error message
	jmp	prserr		; Handle the error
 
;ctrl-l.SBTTL	Cmflot - Try to parse a floating point number
 
;
;	This routine tries to parse a floating point number in the
;	format:
;		sd-d.d-dEsddd
;
;		s is an optional sign bit
;		d is a decimal digit
;		E is the letter 'E'
;		. is a decimal point
;
;		Input:
;
;		Output: Cmfltp- 6 byte floating point number
;				4.5 byte signed mantissa
;				1.5 byte signed exponent
;
;
;		bit	0 1	  35 36 37    47
;
;		Registers destroyed:	A,X,Y
;
 
cmflot: rts
 
;ctrl-l.SBTTL	Cmunqs - Try to parse an unquoted string
 
;
;	This routine tries to parse an unquoted string terminating
;	with one of the break characters in brkwrd.
;
;		Input:	
;
;		Output:	X - L.O. byte address of ASCII string
;			Y - H.O. byte address of ASCII string
;			A - Length of string parsed
;
;		Registers destroyed:	A,X,Y
;
 
cmunqs:	lda	cm_ptr		; Save the command buffer pointer
	pha			;		...
	lda	cm_ptr+1	;		...
	pha			;		...
	lda	#0		; Zero length of Atom buffer
	sta	lenabf		;		...
cmunq1:	jsr	cmgtch		; Get a character
	jsr	chkbrk		; Is it one of the break characters?
	 jmp	cmunq3		; Yes, handle that condition
	cmp	#0		; No, is it an action character?
	bpl	cmunq2		; No, handle it as normal text
	and	#$7F		; We don't need the H.O. bit
	cmp	#'?'		; Does the user need help?
	bne	cmun13		; Nope, try next possibility
	jsr	cout		; Print '?'
	ldy	#0		; Zero the action flag
	sty	cmaflg		;		...
	ldx	#<cmin06	; Get the help message
	ldy	#>cmin06	;		...
	jsr	prstr		;	and print it.
	jsr	prcrlf		; Print a crelf after it
	lda	#cmfehf		; Check for extra help.
	bit	cmprmy		;		...
	beq	cmun11		; If no help, continue
	jsr	cmehlp		; Process extra help
cmun11:	ldx	cm_rty		; Go reprint prompt
	ldy	cm_rty+1	;		...
	jsr	prstr		;		...
	sec			; Adjust buffer pointer
	lda	cm_bfp		;		...
	sbc	#1		;		...
	sta	cm_bfp		;		...
	bcs	cmun12		;		...
	dec	cm_bfp+1	; Adjust H.O. byte
cmun12:	dec	cmccnt		; Correct character count
	ldy	#0		; Stuff a null at end of usable buffer
	lda	#0		;		...
	sta	(cm_bfp),y	;		...
	ldx	#<cmbuf		; Reprint command line
	ldy	#>cmbuf		;		...
	jsr	prstr		;		...
	jmp	cmunq1		; Go back for more characters
cmun13:	cmp	#esc		; Did the user type <esc>?
	bne	cmunq2		; No, just stuff the character and cont.
	lda	#0		; Clear the action flag
	sta	cmaflg		;		...
	lda	#cmfdff		; Check if there is a default value
	bit	cmprmy		;		...
	beq	cmun14		; If not, the <esc> loses
	lda	lenabf		; Ok, there is a default, but if
	bne	cmun14		;	something has been typed, <esc> loses
	jmp	cmcpdf		; Go copy default and reparse
cmun14:	jsr	bell		; Feep at user
	sec			;	and reset the buffer pointer
	lda	cm_bfp		;		...
	sbc	#1		;		...
	sta	cm_bfp		;		...
	sta	cm_ptr		;		...
	lda	cm_bfp+1	;		...
	sbc	#0		;		...
	sta	cm_bfp+1	;		...
	sta	cm_ptr+1	;		...
	dec	cmccnt		; Adjust the character count
	jmp	repars		;	and reparse the command line
cmunq2:	ldy	lenabf		; Fetch where we are in atmbuf
	sta	atmbuf,y	;	and store our character there
	inc	lenabf		; Reflect increased length
	jmp	cmunq1		; Go back for more characters
cmunq3:	lda	lenabf		; Get the length
	beq	cmunqf		; If we parsed a null string, fail
	pla			; Flush old command line pointer
	pla			;		...
	ldx	#<atmbuf	; Now, set up the return parameter
	ldy	#>atmbuf	;		...
	lda	#0		; Reset the failure flag
	sta	cmcffl		;		...
	lda	lenabf		; Set up atom length
	jmp	rskp		; Return
cmunqf:	pla			; Restore old command line pointer
	sta	cm_ptr+1	;		...
	sta	cmoptr+1	;		...
	pla			;		...
	sta	cm_ptr		;		...
	sta	cmoptr		;		...
	lda	cmccnt		; Save count in case of <ctrl/H>
	sta	cmocnt		;		...
	lda	#$FF		; Set failure flag
	sta	cmcffl		;		...
	rts			; Return
 
;ctrl-l.SBTTL	Cmtokn - Try to parse for a single character token
 
;
;	This routine tries to parse for the character in the X-register.
;
;		Input:	X - Character to be parsed	
;
;		Output: +1 - failed to find character
;			+4 - success, found character
;
;		Registers destroyed:	A,X,Y
;
 
cmtokn:	lda	cm_ptr		; First, save the old command pointer
	pha			;	on the stack
	lda	cm_ptr+1	;		...
	pha			;		...
cmtk0:	jsr	cmgtch		; Fetch the next character
	bpl	cmtk3		; Not an action character
	and	#$7F		; It's an action character
	cmp	#esc		; User trying to be lazy?
	bne	cmtk2		; Nope, try next option
	jsr	bell		; Yes, well, he's not allowed to be lazy
	lda	#0		; Clear the action flag
	sta	cmaflg		;		...
	sec			; Adjust the buffer pointer back once
	lda	cm_bfp		;		...
	sbc	#1		;		...
	sta	cm_bfp		;		...
	sta	cm_ptr		; Copy it into command pointer
	bcs	cmtk1		; Need to adjust H.O. byte?
	dec	cm_bfp+1	; Yes, do it
cmtk1:	lda	cm_bfp+1	; Copy it to command pointer
	sta	cm_ptr+1	;		...
	dec	cmccnt		; Adjust the character count
	jmp	cmtk0		;	and try again
cmtk2:	cmp	#'?'		; User need help?
	bne	cmtk4		; No, go fail
	jsr	cout		; Print it
	ldx	#<cmin07	; Point to the information message
	ldy	#>cmin07	;		...
	jsr	prstr		;	and print it
	lda	#dquot		; Print the character we are looking for
	jsr	cout		;	in between double quotes
	lda	cmprmx		;		...
	jsr	cout		;		...
	lda	#dquot		;		...
	jsr	cout		;		...
	jsr	prcrlf		; End it with a crelf
	lda	#cmfehf		; Load extra help flag
	bit	cmprmy		; Test bit
	beq	cmtknh		; No extra help
	jsr	cmehlp		; Go give extra help
cmtknh:	ldx	cm_rty		; Point to prompt
	ldy	cm_rty+1	;		...
	jsr	prstr		;	and print it
	sec			; Adjust the buffer pointer back one
	lda	cm_bfp		;		...
	sbc	#1		;		...
	sta	cm_bfp		;		...
	lda	cm_bfp+1	;		...
	sbc	#0		;		...
	sta	cm_bfp+1	;		...
	lda	#0		; Stuff a null at the end of the buffer
	ldy	#0		;		...
	sta	(cm_ptr),y	;		...
	ldx	#<cmbuf		; Point to command buffer
	ldy	#>cmbuf		;		...
	jsr	prstr		;	and reprint it
	lda	#0		; Clear action flag
	sta	cmaflg		;		...
	jmp	repars		;	and go reparse
cmtk3:	cmp	cmprmx		; Ok, this either is or is not the
	bne	cmtk4		;	char we want. If not, go fail.
	pla			; It is, flush the old address
	pla			;		...
	lda	#0		; Reset the failure flag
	sta	cmcffl		;		...
	jmp	rskp		;	and skip return
cmtk4:	pla			; Restore old pointer
	sta	cm_ptr+1	;		...
	sta	cmoptr+1	;		...
	pla			;		...
	sta	cm_ptr		;		...
	sta	cmoptr		;		...
	lda	cmccnt		; Save the count for <ctrl/H>
	sta	cmocnt		;		...
	lda	#$FF		; Set failure flag
	sta	cmcffl		;		...
	rts			; Return
 
;ctrl-l.SBTTL	Cminbf - read characters from keyboard
 
;
;	This routine reads characters from the keyboard until
;	an action or editing character comes up.
;
;		Input:
;
;		Output:		Cmbuf- characters from keyboard
;
;		Registers destroyed:
;
 
cminbf: pha			; Save the AC
	txa			;	and X
	pha			;		...
	tya			;	and Y
	pha			;		...
	php			; Save the processor status
	ldy	#0		; Clear Y
	lda	cmaflg		; Fetch the action flag
	cmp	#0		; Set??
	beq	cminb1		; Nope
	jmp	cminb9		; Yes, so leave
cminb1: inc	cmccnt		; Up the character count once
	bne	cminb0		;  If we are overflowing the command buffer
	jsr	bell		;    Feep at the user and do Prserr
	dec	cmccnt		;  Make sure this doesn't happen again
	jmp	prserr		;    for same string
cminb0:	jsr	rdkey		; Get next character from keyboard
;	txa			; [jrd] save x for a bit
;	pha			;  while we translate to ascii
;	ldx	char
;	lda	attoas,x	; run it thru the keyboard table
;	sta	char
;	pla			; get x back
;	tax
;	lda	char		;[31]
	cmp	#esc		; esc is a legal non-printing character
	beq	cminb8
	cmp	#cr		; cr is a legal non-printing character
	beq	cminb8
	cmp	#lf		; lf is a legal non-printing character
	beq	cminb8
	cmp	#tab		; tab is a legal non-printing character
	beq	cminb8
	cmp	#ctrlu		; ctrlu is a legal non-printing character
	beq	cminb8
	cmp	#ctrlw		; ctrlw is a legal non-printing character
	beq	cminb8
	cmp	#ffd		; form feed is a legal non-printing character
	beq	cminb8
	cmp	#del		; del is a legal non-printing character
	beq	cminb8
	cmp	#bs		; bs is a legal non-printing character
	beq	cminb8
	cmp	#$20		; ignore non-printing characters
	bcc	cminb0
	cmp	#$20+96		; ignore non-printing characters
	bcs	cminb0
cminb8:	cmp	#$7F		;[46]
	beq	cmind		;  Yes
	cmp	#bs		;  Also a retry
	bne	cmnbnh		;  No, go on
cmind:	ldx	cmccnt		;  Check character count
	cpx	#1		;  Is this the first character?
	bne	cmnbnh		;  Nope, can't help him
	ldx	cmcffl		;  Did the previous command fail?
	bpl	cmnbnh		;  No, we can't reparse a good command
	lda	cmoptr		;  Ok, get the old pointer and set up
	sta	cm_ptr		; 	the old command line again
	sta	cm_bfp		; 		...
	lda	cmoptr+1	; 		...
	sta	cm_ptr+1	; 		...
	sta	cm_bfp+1	; 		...
	lda	cmocnt		;  Restore the character count
	sta	cmccnt		; 		...
	lda	#0		;  Zero this so we can safely use the
	sta	cmwrk2		; 	code that reprints a line after ^W
	jmp	cmnbna		;  Go reprint the line
cmnbnh:	ldy	#0		;		...
	sta	(cm_bfp),y	; Stuff it in buffer
	tax			; Hold it here for a while
	clc			; Clear the carry
	lda	cm_bfp		; Increment the buffer pointer
	adc	#1		;		...
	sta	cm_bfp		;		...
	bcc	cmnb11		; Carry?
	inc	cm_bfp+1	; Yup, do H.O. byte
cmnb11: txa			; Get the data back
	cmp	#ctrlu		; Is it a ^U
	bne	cminb2		; Nope
cmnb12: jsr	screl2		; Yes, clear the whole line
	sec			;[37] Get the cursor coordinates
	jsr	ploth		;[37]		...
;	ldy	#0		;[DD] Reset cursor position to beg. of line
	ldx	#0		;[DD] Reset cursor position to beg. of line
	clc			;[DD]		...
	jsr	ploth		;[DD][26]	...
	ldx	cm_rty		;  Get L.O. byte addr of prompt
	ldy	cm_rty+1	; 	and H.O. byte
	jsr	prstr		; Reprint the prompt
	jsr	screl0		; Get rid of garbage on that line
	lda	#<cmbuf		; Now reset the buffer pointer
	sta	cm_bfp		; 	to the beginning of the buffer
	lda	#>cmbuf		;		...
	sta	cm_bfp+1	;		...
	lda	#0		; Clear AC
	sta	cmccnt		; Clear the character count
	jmp	repars		; Reparse new line from beginning
cminb2: cmp	#bs		; Is it a <bs>?
	beq	cminb3		; Yes
	cmp	#del		;[46]
	bne	cminb4		; No
cminb3: jsr	scrl		; move the cursor left
	jsr	screl0		; Now clear from there to end of line
	dec	cmccnt		; Decrement the character count
	dec	cmccnt		;	twice.
	lda	cmccnt		; Now fetch it
	cmp	#0		; Did we back up too far??
	bpl	cmnb32		; No, go on
	jsr	bell		; Yes, ring the bell and
	jmp	cmnb12		;	go reprint prompt and reparse line
cmnb32: sec			; Set the carry
	lda	cm_bfp		; Now decrement the buffer pointer
	sbc	#2		;	twice.
	sta	cm_bfp		; Store it
	bcs	cmnb33
	dec	cm_bfp+1	; Decrement to account for the borrow
cmnb33: jmp	repars		; Time to reparse everything
cminb4:	cmp	#ctrlw		;  Delete a word?
	beq	cmnb41		;  Yes, go take care of that
	jmp	cmib40		;  Nope, continue
cmnb41:	lda	#3		;  Set up negative offset count
	sta	cmwrk2		; 		...
	sec			;  Set up to adjust buffer pointer
	lda	cm_bfp		;  Get the L.O. byte
	sbc	#3		;  Adjust pointer down by 3
	sta	cm_bfp		;  Store it back
	bcs	cmnb42		;  Don't worry about H.O. byte
	dec	cm_bfp+1	;  Adjust H.O. byte also
cmnb42:	lda	cmwrk2		;  First, check the count
	cmp	cmccnt		;  Cmwrk2 > cmccnt?
	bmi	cmints		;  No, go test characters
	jmp	cmnb12		;  Yes, go clear the whole line
cmints:	ldy	#0		;  Zero Y
	lda	(cm_bfp),y	;  Get previous character
	cmp	#lf		;  Start to test ranges...
	bpl	cmits1		; 	Between <lf> and <cr>?
	jmp	cminac		;  No, not in range at all
cmits1:	cmp	#cr+1		; 		...
	bmi	cmnb43		;  Yes, handle it
	cmp	#space		;  Between <sp> and '"'?
	bpl	cmits2		;  Possible, continue
	jmp	cminac		;  No, advance to previous character
cmits2:	cmp	#dquot+1	; 		...
	bmi	cmnb43		;  Yes, delete back to there
	cmp	#apos		;  Between Apostrophy and '/'?
	bpl	cmits3		;  Could be, continue
	jmp	cminac		;  Nope, advance character
cmits3:	cmp	#slash+1	; 		...
	bmi	cmnb43		;  Yup, found a delimiter
	cmp	#colon		;  Between ':' and '>' perhaps?
	bpl	cmits4		;  Maybe
	jmp	cminac		;  Nope, advance to previous character	
cmits4:	cmp	#rabr+1 	; 		...
	bmi	cmnb43		;  It is, go delete back to there
	cmp	#quot		;  Is it a "'"?
	bne	cminac		;  No, advance
cmnb43:	dec	cmwrk2		;  Adjust this count
	clc			; 	and the buffer pointer
	lda	cm_bfp		; 		...
	adc	#1		; 		...
	sta	cm_bfp		; 		...
	bcc	cmnb44		; 		...
	inc	cm_bfp+1	; 		...
cmnb44:	lda	cmccnt		;  Get the command buffer length
cmnbcc:	sec			;[37] Get the cursor coordinates
	jsr	ploth		;[37]		...
;	sty	savey		;[37] Save cursor position
	stx	savex		;[37] Save cursor position
;	cmp	savey		;[37]  Check against horizontal cursor position
	cmp	savex		;[37]  Check against horizontal cursor position
	bmi	cmnbna		;  It's smaller, skip vert. cursor adjust
;	dex			;[37]  Adjust cursor vertical position
	dey			;[37]  Adjust cursor vertical position
	pha			; Save the AC across this call
	clc			;[37] Set the cursor to the new position
	jsr	ploth		;[26]		...
	pla			; Restore the AC
	sec			;  Reflect this in number of characters
	sbc	#$28		; 	we skipped back over
	jmp	cmnbcc		;  Go check again
cmnbna:	lda	#0		;  Put a null at the end of the buffer
	ldy	#0		; 		...
	sta	(cm_bfp),y	; 		...
	jsr	screl2		;  Clear current line
	sec			;[37] Get the cursor position
	jsr	ploth		;[37]		...
;	ldy	#0		;[EL] Zero the column number
	ldx	#0		;[EL] Zero the column number
	clc			;[37]		...
	jsr	ploth		;[26]		...
	ldx	cm_rty		;  Reprint prompt
	ldy	cm_rty+1	; 		...
	jsr	prstr		; 		...
	ldx	#<cmbuf		;  Reprint command buffer
	ldy	#>cmbuf		; 		...
	jsr	prstr		; 		...
	sec			;  Now adjust the command character count
	lda	cmccnt		; 		...
	sbc	cmwrk2		; 	by what we have accumulated
	sta	cmccnt		; 		...
	jsr	screl0		;  Clear to the end of this line
	jmp	repars		;  Go reparse the command
cminac:	inc	cmwrk2		;  Increment count of chars to back up
	sec			;  Adjust the buffer pointer down again
	lda	cm_bfp		; 		...
	sbc	#1		; 		...
	sta	cm_bfp		; 		...
	bcs	cmnb45		;  If carry set, skip H.O. byte adjustment
	dec	cm_bfp+1	;  Adjust this
cmnb45:	jmp	cmnb42		;  Go around once again
 
cmib40:	cmp	#quest		; Need help?
	beq	cminb6		;		...
	cmp	#esc		; Is he lazy?
	beq	cminb6		;		...
	cmp	#cr		; Are we at end of line?
	beq	cminb5		;		...
	cmp	#lf		; End of line?
	beq	cminb5		;		...
	cmp	#ffd		; Is it a form feed?
	bne	cminb7		; None of the above
	jsr	scred2		; clear the screen
	ldx	#0
	ldy	#0
	jsr	scrplt		; and home the cursor
cminb5: lda	cmccnt		; Fetch character count
	cmp	#1		; Any characters yet?
	bne	cminb6		; Yes
	jmp	prserr		; No, parser error
cminb6: lda	#$FF		; Go
	sta	cmaflg		;	and set the action flag
	jmp	cminb9		; Leave
cminb7:	cmp	#space		; Is the character a space ?
	bne	cmnb71		; No
	jsr	cout		; Output the character
	jmp	cminb1		; Yes, get another character
cmnb71:	cmp	#tab		; Is it a <tab>?
	bne	cmnb72		; No
;	jsr	cout		; Output the character
	jsr	prttab		;[46]
	jmp	cminb1		; Yes, get more characters 
cmnb72:	jsr	cout		; Print the character on the screen
	jmp	cminb1		; Get more characters
cminb9: dec	cmccnt		; Decrement the count once
	plp			; Restore the processor status
	pla			;	the Y register
	tay			;		...
	pla			;	the X register
	tax			;		...
	pla			;	and the AC
	rts			;	and return!
 
 
;ctrl-l.SBTTL	Cmgtch - get a character from the command buffer
 
;
;	This routine takes the next character out of the command
;	buffer, does some checking (action character, space, etc.)
;	and then returns it to the calling program in the AC
;
;		Input:  NONE
;
;		Output: A-	Next character from command buffer
;
;		Registers destroyed:	A,X,Y
;
 
cmgtch: ldy	#0		; Y should always be zero here to index buffer
	lda	cmaflg		; Fetch the action flag
	cmp	#0		; Set??
	bne	cmgt1		; Yes
	jsr	cminbf		; No, go fetch some more input
cmgt1:  lda	(cm_ptr),y	; Get the next character
	tax			; Hold on to it here for a moment
	clc			; Clear the carry flag
	lda	cm_ptr		; Increment
	adc	#1		;	the next character pointer
	sta	cm_ptr		;		...
	bcc	cmgt2		;		...
	inc	cm_ptr+1	; Have carry, increment H.O. byte
cmgt2:  txa			; Now, get the data
	cmp	#space		; Space?
	beq	cmgtc2		; Yes
	cmp	#tab		; <tab>?
	bne	cmgtc3		; Neither space nor <tab>
cmgtc2:	pha			; Hold the character here till we need it
	lda	#cmtxt		; Are we parsing a string?
	cmp	cmstat		; 		...
	beq	cmgtis		; Yes, ignore space flag test
	lda	#cmifi		; Are we parsing a file name?
	cmp	cmstat		;		...
	beq	cmgtis		; Yes, ignore the space flag test
	lda	cmsflg		; Get the space flag
	cmp	#0		; Was the last character a space?
	beq	cmgtis		;  No, go set space flag
	pla			;  Pop the character off
	jmp	cmgtch		;  But ignore it and get another
cmgtis:	lda	#$FF		; Set
	sta	cmsflg		;	the space flag
	pla			;  Restore the space or <tab>
	jmp	cmgtc5		; Go return
cmgtc3: php			; Save the processor status
	pha			; Save this so it doesn't get clobbered
	lda	#0		; Clear AC
	sta	cmsflg		; Clear space flag
	pla			; Restore old AC
	plp			; Restore the processor status
	cmp	#esc		; Escape?
	beq	cmgtc5		;
	cmp	#quest		; Need help?
	beq	cmgtc4		;
	cmp	#cr		; <cr>?
	beq	cmgtc4		;
	cmp	#lf		; <lf>?
	beq	cmgtc4		;
	cmp	#ffd		; <ff>?
	beq	cmgtc4		;
	and	#$7F		; Make sure the character is positive
	rts			; Not an action character, just return
cmgtc4: tax			; Hold the data
	sec			; Set the carry flag
	lda	cm_ptr		; Get the next character pointer
	sbc	#1		;	and decrement it
	sta	cm_ptr		;
	bcs	cmgtc5		;
	dec	cm_ptr+1	;
cmgtc5: txa			; Now, fetch the data
	ora	#$80		; Make it look like a terminator
	rts			; Go back
 
;ctrl-l.SBTTL	Prcrlf subroutine - print a crelf
 
;
;	This routine sets up a call to prstr pointing to the crlf
;	string.
;
;		Registers destroyed:	A
;
;
prcl_0:	txa			; save x
	pha
	tya			; and y
	pha
	lda	#ATEOL		; get an EOL
;	jsr	sputch		; out it goes
	jsr	scrput		; general case, please
	pla			; get
	tay			;  our
	pla			;   bags
	tax			;    back
	rts
;
;ctrl-l.SBTTL	Prstr subroutine
 
;
;	This routine prints a string ending in a null.
;
;		Input:  X-	Low order byte address of string
;			Y-	High order byte address of string
;
;		Output:		Prints string on screen
;
;		Registers destroyed:	A,X,Y
;
; superceeded by pstrnul 
;
prst_0:	jmp	pstrnul

;
; jrd removed routine 'dely' (delay 2 ms) here cause it seemed not to 
; be used anyplace 
; 
 
;ctrl-l.SBTTL	Mul16 - 16-bit multiply routine
 
;
;	This and the following four routines is math support for the
;	Comnd package. These routines come from '6502 Assembly Language
;	Subroutines' by Lance A. Leventhal. Refer to that source for
;	more complete documentation.
;
 
ml16:	pla			; Save the return address
	sta	rtaddr		;		...
	pla			;		...
	sta	rtaddr+1	;		...
	pla			; Get multiplier
	sta	mlier		;		...
	pla			;		...
	sta	mlier+1		;		...
	pla			; Get multiplicand
	sta	mcand		;		...
	pla			;		...
	sta	mcand+1		;		...
	lda	#0		; Zero
	sta	hiprod		;	high word of product
	sta	hiprod+1	;		...
	ldx	#17		; Number of bits in multiplier plus 1, the
				;	extra loop is to move the last carry
				;	into the product.
	clc			; Clear carry for first time through the loop
mullp:  ror	hiprod+1	; Shift the whole thing down
	ror	hiprod		;		...
	ror	mlier+1		;		...
	ror	mlier		;		...
	bcc	deccnt		; Branch if next bit of multiplier is 0
	clc			; next bit is 1 so add multiplicand to product
	lda	mcand		;		...
	adc	hiprod		;		...
	sta	hiprod		;		...
	lda	mcand+1		;		...
	adc	hiprod+1	;		...
	sta	hiprod+1	; Carry = overflow from add
deccnt: dex			;		...
	bne	mullp		; Continue until done
	lda	mlier+1		; Get low word of product and push it
	pha			;	onto the stack
	lda	mlier		;		...
	pha			;		...
	lda	rtaddr+1	; Restore the return address
	pha			;		...
	lda	rtaddr		;		...
	pha			;		...
	rts			; Return
 
mcand:	;  .blkb	2		; Multiplicand
	.word	0
mlier:  ;	.blkb	2		; Multiplier and low word of product
	.word	0
hiprod: ;	.blkb	2		; High word of product
	.word	0
rtaddr: ;	.blkb	2		; Save area for return address
	.word	0
 
;ctrl-l.SBTTL	Rskp - Do a skip return
 
;
;	This routine returns, skipping the instruction following the
;	original call. It is assumed that the instruction following the
;	call is a JMP.
;
;		Input:
;
;		Output:
;
;		Registers destroyed:	None
;
 
rskp_0:	sta	savea		; Save the registers
	stx	savex		;
	sty	savey		;
	pla			; Get Low order byte of return address
	tax			; Hold it
	pla			; Get High order byte
	tay			; Hold that
	txa			; Get Low order byte
	clc			; Clear the carry flag
	adc	#4		; Add 4 to the address
	bcc	rskp2		; No carry
	iny			; Increment the high order byte
rskp2:  sta	saddr		; Store L.O. byte
	sty	saddr+1		; Store H.O. byte
	lda	savea		;
	ldx	savex		;
	ldy	savey		;
	jmp	(saddr)		; Jump at the new address
 
;ctrl-l.SBTTL	Setbrk and Rstbrk
 
;
;	These routines are called from the user program to set or reset
;	break characters to be used by Cmunqs. The byte to set or reset
;	is located in the Accumulator. Rstbrk has the option to reset
;	the entire break-word. This occurs if the H.O. bit of AC is on.
;
 
sbrk_0:	and	#$7F		; We don't want the H.O. bit
	ldy	#0		; Set up Y to index the byte we want
sbrkts:	cmp	#8		; Is the offset > 8
	bmi	sbrkfw		; No, we are at the right byte now
	sec			; Yes, adjust it down again
	sbc	#8		;		...
	iny			; Advance index
	jmp	sbrkts		;	and try again
sbrkfw:	tax			; This is the remaining offset
	lda	#$80		; Start with H.O. bit on
sbrklp:	cpx	#0		; Is it necessary to shift down?
	beq	sbrkfb		; No, we are done
	dex			; Yes, adjust offset
	lsr	a		; Shift bit down once
	jmp	sbrklp		; Go back and try again
sbrkfb:	ora	brkwrd,y	; We found the bit, use the byte offset
	sta	brkwrd,y	;	from above, set the bit and resave
	rts			; Return
 
rbrk_0:	asl	a		; Check H.O. bit
	bcs	rbrkal		; If that was on, Zero entire brkwrd
	lsr	a		; Else shift back (H.O. bit is zeroed)
rbrkts:	cmp	#8		; Are we in the right word?
	bmi	rbrkfw		; Yes, go figure the rest of the offset
	sec			; No, Adjust the offset down
	sbc	#8		;		...
	iny			;	and the index up
	jmp	rbrkts		; Try again
rbrkfw:	tax			; Stuff the remaining offset in X
	lda	#$7F		; Start with H.O. bit off
rbrklp:	cpx	#0		; Do we need to offset some more?
	beq	rbrkfb		; No, we have the correct bit
	dex			; Yes, decrement the offset
	sec			; Make sure carry is on
	ror	a		;	and rotate a 1 bit into mask
	jmp	rbrklp		; Go back and try again
rbrkfb:	and	brkwrd,y	; We found the bit, now shut it off
	sta	brkwrd,y	;		...
	rts			;	and return
rbrkal:	lda	#0		; Go stuff zeros in the entire word
	ldy	#0		;		...
rbrksz:	sta	brkwrd,y	; Stuff the zero
	iny			; Up the index once
	cpy	#$10		; Are we done?
	bmi	rbrksz		; Not yet
	rts			; Yes, return
 
;ctrl-l.SBTTL	Chkbrk
 
;
;	Chkbrk - This routine looks for the flag in the break word
;	which represents the character passed to it. If this bit is
;	on, it is a break character and the routine will simply
;	return. If it is not a break character, the routine skips..
;
 
chkbrk:	sta	savea		; Save byte to be checked
	and	#$7F		; Shut H.O. bit
	ldy	#0		; Zero this index
cbrkts:	cmp	#8		; Are we at the right word?
	bmi	cbrkfw		; Yes, go calculate bit position
	sec			; No, adjust offset down
	sbc	#8		;		...
	iny			; Increment the index
	jmp	cbrkts		; Go back and test again
cbrkfw:	tax			; Stuff the remaining offset in X
	lda	#$80		; Set H.O. bit on for testing
cbrklp:	cpx	#0		; Are we in position yet?
	beq	cbrkfb		; Yes, go test the bit
	dex			; No, decrement the offset
	lsr	a		;	and adjust the bit position
	jmp	cbrklp		; Go and try again
cbrkfb:	and	brkwrd,y	; See if the bit is on
	bne	cbrkbc		; It is a break character
	lda	savea		; Restore the character
	jmp	rskp		; Not a break character, skip return
cbrkbc:	lda	savea		; Restore the character
	rts			; Return
 
;ctrl-l.SBTTL	Cmehlp - Do extra help on Question-mark prompting
 
;
;	Cmehlp - This routine uses a string of commands passed to it
;	in order to display alternate valid parse types to the user.
;
;		Input:	Cmehpt-	Pointer to valid parse types (end in 00)
;
;		Output:	Display on screen, alternate parse types
;
;		Registers destroyed:	A,X,Y
;
 
cmehlp:	lda	cmstat		; We are going to need this so
	pha			;	save it across the call
	ldy	#0		; Zero out the help index
	sty	cmehix		;		...
cmehl1:	ldy	cmehix		; Load the extra help index
	lda	(cmehpt),y	; Fetch next type
	sta	cmstat		; Store it here
	inc	cmehix		; Increase the index by one
	cmp	#0		; Is the type null?
	bne	cmeh0		; No, continue
	jmp	cmehrt		; Yes, terminate
cmeh0:	cmp	#cmtok+1	; If the type is out of range, leave
	bmi	cmeh1		;		...
	jmp	cmehrt		;		...
cmeh1:	pha			; Save the type across the call
	ldx	#<cmors		; Set up address of 'OR ' string
	ldy	#>cmors		;		...
	jsr	prstr		;	and print it
	pla			; Restore AC
	cmp	#cmkey		; Compare with keyword
	bne	cmeh2		; No, try next type
cmeh10:	tax			; Hold type in X register
	lda	cmsptr		; Save these parms so they can be restored
	pha			;		...
	lda	cmsptr+1	;		...
	pha			;		...
	lda	cm_ptr		; Copy the pointer to the saved pointer
	sta	cmsptr		;	so the keyword print routine prints
	pha			;	the entire table. Also, save it on
	lda	cm_ptr+1	;	the stack so it can be restored later
	sta	cmsptr+1	;		...
	pha			;		...
	lda	cmptab		; Save the table address also
	pha			;		...
	lda	cmptab+1	;		...
	pha			;		...
	txa			; Restore type
	cmp	#cmkey		; Keyword?
	bne	cmeh11		; No, it must be a switch table
	ldx	#<cmin01	; Set up address of message
	ldy	#>cmin01	;		...
	jmp	cmeh12		; Go print the string
cmeh11:	ldx	#<cmin02	; Set up address of 'switch' string
	ldy	#>cmin02	;		...
cmeh12:	jsr	prstr		; Print the message
	ldy	cmehix		; Get the index into help string
	lda	(cmehpt),y	; Fetch L.O. byte of table address
	sta	cmptab		; Set that up for Cmktp
	iny			; Increment the index
	lda	(cmehpt),y	; Get H.O. byte
	sta	cmptab+1	; Set it up for Cmktp
	iny			; Advance the index
	sty	cmehix		;	and store it
	jsr	cmktp		; Print the keyword table
	pla			; Now restore all the stuff we saved before
	sta	cmptab+1	;		...
	pla			;		...
	sta	cmptab		;		...
	pla			;		...
	sta	cm_ptr+1	;		...
	pla			;		...
	sta	cm_ptr		;		...
	pla			;		...
	sta	cmsptr+1	;		...
	pla			;		...
	sta	cmsptr		;		...
	jmp	cmehl1		; See if there is more to do
cmeh2:	cmp	#cmswi		; Type is switch?
	bne	cmeh3		; No, continue
	jmp	cmeh10		; We can treat this just like a keyword
cmeh3:	cmp	#cmifi		; Input file?
	bne	cmeh4		; No, go on
	ldx	#<cmin03	; Set up the message address
	ldy	#>cmin03	;		...
	jmp	cmehps		; Go print it
cmeh4:	cmp	#cmofi		; Output file?
	bne	cmeh5		; Nope, try again
	ldx	#<cmin04	; Set up message address
	ldy	#>cmin04	;		...
	jmp	cmehps		; Go print the string
cmeh5:	cmp	#cmcfm		; Confirm?
	bne	cmeh6		; No
	ldx	#<cmin00	; Set up address
	ldy	#>cmin00	;		...
	jmp	cmehps		; Print the string
cmeh6:	cmp	#cmtxt		; Unquoted string?
	bne	cmeh7		; No, try next one
	ldx	#<cmin06	; Set up address
	ldy	#>cmin06	;		...
	jmp	cmehps		; Print
cmeh7:	cmp	#cmnum		; Integer?
	bne	cmeh8		; Try again
	ldx	#<cmin05	; Set up message
	ldy	#>cmin05	;		...
	jsr	prstr		; Print it
	ldy	cmehix		; Get index
	inc	cmehix		; Advance index
	lda	(cmehpt),y	; Get base of integer
	cmp	#$0A		; Is it greater than decimal 10?
	bmi	cmeh71		; No, just print the L.O. digit
	lda	#$31		; Print the H.O. digit as a 1
	jsr	cout		; Print the '1'
	ldy	cmehix		; Load index
	dey			; Point back to last byte
	lda	(cmehpt),y	; Get the base back
	sec			; Set the carry flag for subtraction
	sbc	#$0A		; Subtract off decimal 10
cmeh71:	clc			; Clear carry for addition
	adc	#$30		; Make it printable
	jsr	cout		; Print the digit
	jsr	prcrlf		; Print a crelf
	jsr	prbyte		; Print the byte
	jmp	cmehl1		; Go back for more
cmeh8:	ldx	#<cmin07	; Assume it's a token
	ldy	#>cmin07	;		...
cmehps:	jsr	prstr		; Print string
	jsr	prcrlf		; Print a crelf
	jmp	cmehl1		; Go back
cmehrt:	pla			; Restore
	sta	cmstat		;	current parse type
	rts
 
;ctrl-l.SBTTL	Cmcpdf - Copy a default string into the command buffer
 
;
;	Cmcpdf - This routine copies a default for a field
;	into the command buffer andreparses the string.
;
;		Input:	Cmdptr-	Pointer to default field value (asciz)
;
;		Output:
;
;		Registers destroyed:	A,X,Y
;
 
cmcpdf:	sec			; Reset the buffer pointer
	lda	cm_bfp		;		...
	sbc	#1		;		...
	sta	cm_bfp		;		...
	bcs	cmcpst		; If carry set, don't adjust H.O. byte
	dec	cm_bfp+1	;		...
cmcpst:	dec	cmccnt		; Adjust the character count
	ldy	#0		; Zero the index
cmcplp:	lda	(cmdptr),y	; Get byte
	beq	cmcpdn		; Copy finished, leave
	ldx	cmccnt		; Check character count
	inx			; If it is just short of wrapping
	bne	cmcpl1		;	then we are overflowing buffer
	jsr	bell		; If that is the case, tell the user
	dec	cmccnt		; Make sure it doesn't happen again
	jmp	prserr		;	for same string.
cmcpl1:	
	ora	#$80		; Be consistent, make sure H.O. bit is on
	sta	(cm_bfp),y	; Stuff it in the buffer
	inc	cmccnt		; Adjust character count
	iny			; Up the buffer index
	jmp	cmcplp		; Go to top of loop
cmcpdn:	lda	#space		; Get a space
	sta	(cm_bfp),y	;	and place it in buffer after keyword
	iny			; Increment the buffer index
	lda	#nul		; Get a null
	sta	(cm_bfp),y	;	and stuff that at the end of buffer
	clc			; Now recompute the end of usable buffer
	tya			; Get the number of chars added
	adc	cm_bfp		; Add that to the buffer pointer
	sta	cm_bfp		;		...
	lda	#0		;		...
	adc	cm_bfp+1	;		...
	sta	cm_bfp+1	;		...
	lda	#0		; Reset the action flag
	sta	cmaflg		;		...
	sec			; Now adjust the command pointer to the
	lda	cm_ptr		;	beginning of the copied field
	sbc	#1		;		...
	tax			; Set it up in X and Y so we can call Prstr
	lda	cm_ptr+1	;		...
	sbc	#0		;		...
	tay			;		...
	jsr	prstr		; Print the added field
	jmp	repars		; Now go reparse the whole command
 
;ctrl-l.SBTTL	Comnd Jsys messages and table storage
 
cmer00: .byte	cr,"?program error:  invalid comnd call",0		; [53]
cmer01: .byte	cr,"?ambiguous",0		; [53]
cmer02: .byte	cr,"?illegal input file spec",0		; [53]
cmer03: .byte	cr,"?no keywords match this prefix",0		; [53]
cmer04: .byte	cr,"?no switches match this prefix",0		; [53]
cmer05: .byte	cr,"?bad character in integer number",0		; [53]
cmer06: .byte	cr,"?base of integer out of range",0		; [53]
cmer07: .byte	cr,"?overflow while reading integer number",0		; [53]

cmin00: .byte	" confirm with RETURN",0		; [53]
cmin01: .byte	" keyword, one of:",0		; [53]
cmin02: .byte	" switch, one of:",0		; [53]
cmin03: .byte	" input file spec",0		; [53]
cmin04: .byte	" output file spec",0		; [53]
cmin05: .byte	" integer number in base ",0		; [53]
cmin06:	.byte	" unquoted text string ",0		; [53]
cmin07:	.byte	" single character token ",0		; [53]
 
cmors:	.byte	" or ",0		; [53]
 
;ctrl-l.SBTTL	Kermit defaults for operational parameters
 
;
;	The following are the defaults which this Kermit uses for
;	the protocol.
;
 
dquote  =	'#'		; The quote character
dpakln  =	94		; The packet length
dpadch  =	nul		; The padding character
dpadln  =	0		; The padding length
dmaxtr  =	20		; The maximum number of tries
debq	=	'&'		; The eight-bit-quote character
;dtime	=	15		; The default time-out amount
dtime	=	5		; [jrd] 5 sec ought to be ok
deol	=	cr		; The end-of-line character
 
;ctrl-l.SBTTL	Kermit data
 
;
;	The following is data storage used by Kermit
;
 
mxpack  =	dpakln		; Maximum packet size
mxfnl	=	16		; Maximum file-name len, "Dn:FROBBOZZ.DAT"
eof	=	$01		; This is the value for End-of-file
buflen  =	$FF		; Buffer length for received data
true	=	$01		; Symbol for true return code
false	=	$00		; Symbol for false return code
on	=	$01		; Symbol for value of 'on' keyword
off	=	$00		; Symbol for value of 'off' keyword
yes	=	$01		; Symbol for value of 'yes' keyword
no	=	$00		; Symbol for value of 'no' keyword
terse	=	$01		; Symbol for terse debug mode
verbose	=	$02		; Symbol for verbose debug mode
xon	=	$11		; Xon for Ibm-mode
fbsbit  =	$01		; Value for SEVEN-BIT FILE-BYTE-SIZE
fbebit  =	$00		; Value for EIGHT-BIT FILE-BYTE-SIZE
nparit	=	$00		; Value for PARITY NONE
sparit	=	$01		; Value for PARITY SPACE
mparit	=	$02		; Value for PARITY MARK
oparit	=	$03		; Value for PARITY ODD
eparit	=	$04		; Value for PARITY EVEN
;
; NB! these values are internal codes, which should be
;  sequential from 0.  The real values for the port are 
;  in bdval, indexed by these values
;
bd50	=	$00		;[17] Value for BAUD 50
bd75	=	$01		;[17]
bd110	=	$02		;[17] Value for BAUD 110
bd150	=	$03		;[17] Value for BAUD 150
bd300	=	$04		;[17] Value for BAUD 300
bd1200	=	$05		;[17] Value for BAUD 1200
bd1800	=	$06		;[17] Value for BAUD 1800
bd2400	=	$07		;[17] Value for BAUD 2400
bd4800	=	$08		;[17] Value for BAUD 4800
bd9600	=	$09		;[17] Value for BAUD 9600
;
; defs for values in errcod
;
eprflg	=	$40		;	'Error packet received' flag
edoflg	=	$80		;	Dos error code
;
errcri  =	$01		; Error code - cannot receive init
errcrf  =	$02		; Error code - cannot receive file-header
errcrd  =	$03		; Error code - cannot receive data
errmrc  =	$04		; Error code - maximum retry count exceeded
errbch  =	$05		; Error code - bad checksum
errint	=	$06		; [jrd] internal error
errfta	=	$07		; [jrd] transfer aborted
errfal	=	$08		; [jrd] filename alter error
errfae  =	$09		; Error code - file already exists
errfde	=	$0A		; [jrd] some DOS (typically disk) error
;
;emesln  =	$19		; Standard error message length
kerrns  =	$1F		; Routine name and action string length
;kerdel  =	$15		; Disk error length
kerems  =	$19		; Error message size
kerfts	=	$08		; Size of file-type strings (incl. term. nul)
kerdsz	=	$09		; Length of debug mode strings
kerpsl	=	$06		; Size of parity strings
kerbsl	=	$05		;[17] Size of baud strings
keremu	=	$06		; size of terminal emulation strings
kerfrm	=	cminf1		; 'From string' pointer for Kercpy routine
kerto	=	cminf2		; 'To string' pointer for Kercpy routine
 
errrkm:	.res	mxpack-2	; Error message from remote Kermit
pdbuf:  .res	mxpack-2	; Packet buffer
plnbuf: .res	$100		;[DD] Port line buffer
pdlen:  .byte	0		; Common area to place data length
ptype:  .byte	0		; Common area to place current packet type
pnum:	.byte	0		; Common area to put packet number received
pdtend: .byte	0		; End of plnbuf pointer
pdtind: .byte	0		; Index for plnbuf
rstat:  .byte	0		; Return status
kerrta: .word	0		; Save area for return address
datind: .byte	0		; Data index into packet buffer
chebo:  .byte	0		; Switch to tell if 8th-bit was on
escflg: .byte	0		; Flag indicating we have seen an escape ($1b)
addlf:  .byte	0		; Add a <lf> flag
dellf:  .byte	0		; Flush a <lf> flag
jtaddr: .word	0		; Jump table address hold area
;hch:	.byte	0		; Hold area for ch
;hcv:	.byte	0		; Hold area for cv
kwrk01: .byte	0		; Work area for Kermit
kwrk02: .byte	0		; Work area for Kermit
kertpc:	.byte	0		; Hold area for parity check
ksavea:	.byte	0		; Save area for accumulator
ksavex:	.byte	0		; Save area for X reg
ksavey:	.byte	0		; Save area for Y reg
kerchr: .byte	0		; Current character read off port
kermbs: .word	0		; Base address of message table
debchk: .byte	0		; Checksum for debug routine
debinx: .byte	0		; Debug routine action index
fld:	.byte	0		; State of receive in rpak routine
retadr: .word	0		; Hold area for return address
n:	.byte	0		; Message #
numtry: .byte	0		; Number of tries for this packet
oldtry: .byte	0		; Number of tries for previous packet
maxtry: .byte	dmaxtr		; Maximum tries allowed for a packet
state:  .byte	0		; Current state of system
local:	.byte	0		; Local/Remote switch
size:	.byte	0		; Size of present data
chksum: .byte	0		; Checksum for packet
rtot:	.word	0		; Total number of characters received
stot:	.word	0		; Total number of characters sent
rchr:	.word	0		; Number characters received, current file
schr:	.word	0		; Number of characters sent, current file
rovr:	.word	0		; Number of overhead characters on receive
sovr:	.word	0		; Number of overhead characters on send
tpak:	.word	0		; Number of packets for this transfer
eofinp: .byte	0		; End-of-file (no characters left to send)
eodind: .byte	0		; End-of-data reached on disk
errcod: .byte	0		; Error indicator
;errrkm:	.res	mxpack-2	; Error message from remote Kermit
kerosp: .byte	0		; Save area for stack pointer
keresp:	.byte	0		; [jrd] another one for exit time
kerret:	.word	0		; [jrd] return address for exit in case
				;  stack trashed

;
; equates for terminal types
;
ttnone	=	0		; terminal type none, glass tty
tt52	=	1		; vt52
tt100	=	2		; vt100

;
; equates for character set designators.  See csg0, csg1 for use
;
csascii	=	0		; normal ascii font
csgraf	=	1		; graphics font

;
; equates for file types.  NB! the code assumes all text types
; are less than all binary types, and that "binary" is the first
; binary type.
;
ftstas	=	0		; Standard ascii
ftatas	=	1		; Atari ASCII
ftbin	=	2		; binary

;
; equates for screen types
;
scrae	=	0		; Atari E:
scr40	=	1		; 40 col visible, pannable to 80
scr80	=	2		; 80 col, using highest res graphics
;
scrtype: .byte	scrae		; Use the Atari E: screen for starters
;
; This block of stuff (escp .. quote) gets saved and restored.
;
escp:	.byte	$19		; Character for escape from connection
;fbsize: .byte	fbsbit		; File-byte-size
capslck: .byte	0		; [jrd] caps lock, default off
filmod: .byte	ftatas		; Current file type, default atascii
usehdr: .byte	off		; Switch - where to get filename (on=file-head)
lecho:  .byte	off		; Local-echo switch
ibmmod: .byte	off		; Ibm-mode switch
vtmod:  .byte	tt100		; VT-52 Emulation mode switch, default off
conscrt: .byte	scr80		; screen to use in terminal mode.  Default 80
parity: .byte	nparit		; Parity setting
baud:	.byte	bd1200		;[17] Baud setting
wrdsiz:	.byte	fbebit		;[17] Word length setting
flowmo:	.byte	off		;[24] Flow-Control switch
delay:  .byte	0		; Amount of delay before first send
filwar: .byte	off		; File-warning switch
debug:  .byte	off		; Debug switch
ebqmod: .byte	off		; Eight-bit-quoting mode
; parameters for the XIO 36 (set baud rate etc] call in openrs
x36ax1:	.byte	$0A		; [jrd] the AUX1 value for the XIO 36.  1200 bps default
x38ax1:	.byte	$00		; [jrd] the AUX1 value for the XIO 38.  no parity default
 
;
;	These fields are set parameters and should be kept in this
;	order to insure integrity when setting and showing values
;
 
srind:  .res	1		; Switch to indicate which parm to print
ebq:	.byte	debq		; Eight-bit quote character (rec. and send)
	.byte	debq		;		...
pad:	.byte	dpadln		; Number of padding characters (rec. and send)
	.byte	dpadln		;		...
padch:  .byte	dpadch		; Padding character (receive and send)
	.byte	dpadch
eol:	.byte	deol		; End-of-line character (recevie and send)
	.byte	deol
psiz:	.byte	dpakln		; Packet size (receive and send)
	.byte	dpakln
time:	.byte	dtime		; Time-out interval (receive and send)
	.byte	dtime		;
quote:  .byte	dquote		; Quote character (receive and send)
	.byte	dquote		;		...
 
;ttime:	.word	$0000		;[49] Time out interval (receive and send)
ttime:	.word	$0000		; [jrd] recv/send Timer expiration time, hi,lo
 
;
;	Some definitions to make life easier when referencing the above
;	fields.
;
 
rebq	=	ebq		; Receive eight-bit-quote char
sebq	=	ebq+1		; Send eight-bit-quote char
rpad	=	pad		; Receive padding amount
spad	=	pad+1		; Send padding amount
rpadch	=	padch		; Receive padding character
spadch	=	padch+1		; Send padding character
reol	=	eol		; Receive end-of-line character
seol	=	eol+1		; Send end-of-line character
rpsiz	=	psiz		; Receive packet length
spsiz	=	psiz+1		; Send packet length
rtime	=	time		; Receive time out interval
stime	=	time+1		; Send time out interval
rquote	=	quote		; Receive quote character
squote	=	quote+1		; Send quote character
 
;ctrl-l.SBTTL	Kermit - CBM DOS support
 
;
;	The following definitions and storage will be used when setting
;	up and executing calls to the DOS.
;
 
fncrea  =	'R'		; Read function code
fncwrt  =	'W'		; Write function code
drdoll	=	'$'		;[40] Directory string
drcolo	=	':'		;[40]
drstar	=	'*'		;[40]
; [jrd] not here you don't
;kerfcb	=	$1e		; Pointer to FCB
;buff	=	$200		; Temp disk char read
;buff:	.byte	0
;fmrcod: .byte	0		; Disk status return code
decnum:	.res	2		; [54] Number being converted to decimal
;dskers: .res	110		; Storage for disk error messages
dosffm:	.byte	$00		; 'First file modification done' switch
;dosfni:	.byte	$00		; Filename index
dosfvn:	.byte	$00		; File version number for the alter routine
;
; pathname descriptors
;
primfn: .res	mxfnl+mxfnl+1	; File name for local opens
				;  double size for renames...
	.byte	0		; sanity check...
fcb1:	.res	mxfnl+1		; buffer for typing names into
	.byte	0		; sanity check...
;
; the default pathname
;
defpath:
	.byte	pnf_dp|pnf_np|pnf_ep ; contains, dev, name, and type
	.byte	2		; dev max
	.byte	2,"D1"		; dev size, text
	.byte	8		; name max
	.byte	3,"FOO     "	; name size, text
	.byte	3		; ext max
	.byte	3,"DAT"		; ext size, text
;
wildfn:	.byte	"*.*",0		; wild file name, for defaults in cp
;
dsknum	=	defpath+4	; address of the '1'
;
;path:	.res	pndsiz		; one for random pathnames that come in
path:				; one for random pathnames that come in
	.byte	0		; flags
	.byte	2,0,0,0
	.byte	8,0,0,0,0,0,0,0,0,0
	.byte	3,0,0,0,0
;
dirpath:			; one for directory entries
	.byte	0		; flags
	.byte	2,0,0,0
	.byte	8,0,0,0,0,0,0,0,0,0
	.byte	3,0,0,0,0
;
dirplck: .byte	0		; 'locked' flag for current dirpath
dirsect: .byte	"    ",ATEOL	; buf for sector count strg
;
;
flsrw:  .byte	0		; Switch for r(ead) or w(rite)
;
prmt:	.byte	"Kermit-65>"	; Prompting text
	.byte	0		; [53]
lprmt	=	*-prmt		; Length of prompting text
connec:	.byte	$00		;[48] non-zero if in terminal mode

 
;ctrl-l.SBTTL	Kermit initialization
 
;
;	The following code sets up Kermit-65 for normal operation.
;	This is the main entry point.  There is currently no warmstart
;	entry point.
;
kstart:	
	pla			; [jrd] save return address
	sta	kerret		;  for exit time
	pla
	sta	kerret+1
	tsx			; save SP too
	stx	keresp
;
	jsr	openscr		; [jrd] open screen, so we can talk to user.
; [jrd]	jsr	clall		;[] First close all open channels
; [jrd]	jsr	ioinit		;[16] Initialize I/O devices
; [jrd]	jsr	restoi
; 	jsr	scrini		; [jrd] zzz what should this be here?
; [jrd]	lda	r6510		;[17] Start by paging out BASIC ROM
; [jrd]	and	#$fe		;[17]		...
; [jrd]	sta	r6510		;[17]		...
;
	jsr	restin		; try to read and parse init file
 
init:
;
; fix margins
;
	lda	#0
	sta	LMARGN
;
; 4/11/90 jrd.  Don't frob with right margin, so as not to screw up
; XEP 80 support.  This means we'll lose big if something else has
; screwed it up already.  BFD...
;
;	lda	#39
;	sta	RMARGN
;
	jsr	dopari		;[]
	jsr	dobad		;[]
	jsr	dowrd		;[]
	ldx	#<versio	;Get address of version message
	ldy	#>versio	;		...
	jsr	prstr		;Print the version
	jsr	prcrlf		;Print a crlf
;	jsr	kermit		;Go execute kermit
	jsr	kerm		; [jrd] Go execute kermit
	jmp	exit1		;[17] and reenter BASIC

;
; debug stack stuff
;
;dbgstk:
;	jsr	prstr		; print the string
;	tsx
;	stx	freemem		; temp
;	txa
;	jsr	prbyte
;	lda	#':
;	jsr	prchr
;	lda	#16
;	sta	freemem+1
;dbgstk1:
;	ldx	freemem
;	lda	$100,x
;	inx
;	stx	freemem
;	jsr	prbyte
;	lda	#space
;	jsr	prchr
;	dec	freemem+1
;	bne	dbgstk1
;	jsr	prcrlf
;	rts

;ctrl-l.SBTTL	Kermit - main routine
 
;
;	This routine is the main KERMIT loop. It prompts for commands
;	and then it dispatches to the appropriate routine.
;
;kmtxt:	.byte	ATEOL,"Main ",0
kerm:				; [jrd] kermit routine init
	tsx			; Get the stack pointer
	stx	kerosp		;	and save it in case of a fatal error
kermloop: 			; kermit main loop
;	ldx	#<kmtxt
;	ldy	#>kmtxt
;	jsr	dbgstk
	ldx	#<prmt		;  Fetch the address of the prompt
	ldy	#>prmt		;		...
	lda	#cmini		; Argument for comnd call
	jsr	comnd		; Set up the parser and print the prompt
	lda	#<kercmd	; addr of command table
	sta	cminf1		;		...
	lda	#>kercmd	;		...
	sta	cminf1+1	;		...
	lda	#<kerhlp	; Store address of help text
	sta	cmhptr		;  in help pointer
	lda	#>kerhlp	;		...
	sta	cmhptr+1	;		...
	ldy	#$00		;  No special flags needed
	lda	#cmkey		; Set up for keyword parse
	jsr	comnd		; Try to parse it
	 jmp	kermt2		; Failed
;
	stx	jtaddr		; x,y has vector
	sty	jtaddr+1	; set it
	jmp	(jtaddr)	; and go there

;---
;	lda	#<kermtb	; Get address of jump table
;	sta	jtaddr		;		...
;	lda	#>kermtb	;		...
;	sta	jtaddr+1	;		...
;	txa			; Offset to AC
;---
; still used in a few places...
jmpind: clc			;[DD] Jump indexed
	adc	jtaddr		; Add offset to low byte
	sta	jtaddr		;		...
	bcc	jmpin1		;		...
	inc	jtaddr+1	; If carry inc high byte
jmpin1: jmp	(jtaddr)	; Jump to address
;---

;
;kermtb: jmp	telnet		; Connect command
;	jmp	exit		; Exit command
;	jmp	help		; Help command
;	jmp	log		; Log command
;	jmp	exit		; Quit command
;	jmp	receve		; Receive command
;	jmp	send		; Send command
;	jmp	setcom		; Set command
;	jmp	show		; Show command
;	jmp	status		; Status command
;	jmp	bye		;[EL] Shut and logout remote server command
;	jmp	finish		;[EL] Shut remote server
;	jmp	getfrs		;[EL] Get file from remote server
;	jmp	rename		;[40] rename files(s)
;	jmp	dirst		;[40] Get directory
;	jmp	savst		;[47] Save parameters
;	jmp	restst		;[47] Restore parameters
;	jmp	erase		; [jrd] erase something
;---
;
kermt2: ldx	#<ermes1	; L.O. byte of error message
	ldy	#>ermes1	; H.O. byte of error message
;	jsr	prstr		; Print the error
;	jmp	kermit		; Go back
	jmp	kermzz		; [jrd] use common error vector
;
kermt3: ldx	#<ermes3	; L.O. byte of error
	ldy	#>ermes3	; H.O. byte of error
;	jsr	prstr		; Print it
;	jmp	kermit		; Try again
	jmp	kermzz		; [jrd] use common error vector
;
kermt4: ldx	#<ermes4	; L.O. byte of error
	ldy	#>ermes4	; H.O. byte of error
;	jsr	prstr		; Print the text
;	jmp	kermit		; Try again
	jmp	kermzz		; [jrd] use common error vector
;
kermt5: ldx	#<ermes6	; L.O. byte of error
	ldy	#>ermes6	; H.O. byte of error
;	jsr	prstr		; Print error text ('keyword')
;	jmp	kermit		; Start at the beginning again
	jmp	kermzz		; [jrd] use common error vector
;
kermt6: ldx	#<ermes7	; L.O. byte of error
	ldy	#>ermes7	; H.O. byte of error
;	jsr	prstr		; Print the error message ('file spec')
;	jmp	kermit		;	and try again
	jmp	kermzz		; [jrd] use common error vector
;
kermt7: ldx	#<ermes8	; L.O. byte of error message text
	ldy	#>ermes8	; H.O. byte of error
;	jsr	prstr		; Print it ('integer')
;	jmp	kermit		; Try for another command line
	jmp	kermzz		; [jrd] use common error vector
;
;kermt8: ldx	#<ermes9	; L.O. byte of error
;	ldy	#>ermes9	; H.O. byte of error
;	jsr	prstr		; Print the message ('switch')
;	jmp	kermit		; Try for another command line
;	jmp	kermzz		; [jrd] use common error vector
;
kermt9: ldx	#<ermesa	; L.O. byte of error message
	ldy	#>ermesa	; H.O. byte of error message
;	jsr	prstr		; Print the message ('')
;	jmp	kermit		; Try for another command line
	jmp	kermzz		; [jrd] use common error vector
;
kermta:	ldx	#<ermesb	; L.O. byte of error message
	ldy	#>ermesb	; H.O. byte of error message
;	jsr	prstr		; Print the message ('text')
;	jmp	kermit		; Go back to top of loop
;				; [jrd] fall into error vector
;
kermzz:				; common error vector
	jsr	prstr		; print the error msg
;	jmp	kermit		; restart the sucker
; fall into warmstart rtn

;
;	Nonftl - handles non-fatal DOS errors. When Kermit does its
;	initialization it points the error vector and the basic
;	warmstart vector here.
;
 
nonftl:
kermit:
;	lda	fmrcod		; Get the DOS return code
;	ora	#$80		;		...
;	sta	errcod		; Save that here
	ldx	kerosp		; Get the old stack pointer back
	txs			; Restore it
	jmp	kermloop	; Warmstart kermit
 
;
;	Fatal - closes and deletes a file on which a bad error
;	has occured (most likely a 'disk full' error). It then
;	restores the old stack pointer and warmstarts Kermit.
;
 
fatal:
;	lda	fmrcod		; Get the DOS return code
;	ora	#$80		; Set H.O. bit to indicate DOS error
;	sta	errcod		; Store the error code
	jsr	closef		; Close the file
;	jsr	dosdel		; Now, delete the useless file
;	ldx	kerosp		; Get the old stack pointer
;	txs			; Restore it
	jmp	kermit		; Warmstart kermit
 
;ctrl-l.SBTTL	Telnet routine
 
;
;	This routine handles the connect command. After connecting
;	to a host system, this routine alternates calling routines
;	which will pass input from the port to the screen and pass
;	output from the keyboard to the port. This kermit will
;	ignore all characters until it sees and assigned escape
;	character.
;
;		Input:  RS232 parameters
;
;		Output: NONE
;
;		Registers destroyed:	A,X,Y
;
telnet: jsr	prcfm		; Parse and print a confirm
	jsr	vttini		; init tab map
	lda	#0		; zzz debug
	sta	altcs		; getting set spuriously?
	lda	#true		;[48]
	sta	connec		;[48]
	jsr	scrext		; [jrd] make sure we're set up for
				;  re-entering E:
	lda	conscrt		; [jrd] get the screen type for connect
	sta	scrtype		; set the current screen type
	jsr	scrini		; do any required initializations
	lda	scrtype		; using non-atari screen?
	bne	tn1		; if not atari screen, use stat line
	ldx	#<inf01a	; Get address of first half of message
	ldy	#>inf01a	;		...
	jsr	prstr		; Print it out
	lda	escp		; Get the 'break connection' character
	jsr	prchr		; Print that as a special character
	ldx	#<inf01b	; Get address of second half of message
	ldy	#>inf01b	;		...
	jsr	prstr		; Print that
	jsr	prcrlf		;	and a crelf
	jmp	tn2
tn1:
	jsr	telslm		; go do message in stat line
tn2:
	jsr	openrs		;[27]
 
chrlup:	
; [jrd]	lda	ndx		;[39] Check keyboard queue
	lda	CH		; FF -> no key pending
	cmp	#$FF		; anything there?
	bne	telcnc		;[39] Keyboard has priority over RS232
telprc:	jsr	getrs		; Check for a port character
	bne	telcnc		; None available, check keyboard
;
; if debugging enough, log received characters
;
	lda	debug		; get debug flag
	cmp	#verbose	; verbose debugging?
	bne	telprnd		; nope, go on
	jsr	teldbg
telprnd:
	lda	char		;[31] Get the character read from port
	and	#$7F		;[31] Shut off the high order bit
	sta	char		;[26][31] Store the character back
	ldx	escflg		; Was previous character an escape?
	cpx	#on		;		...
	bne	telp2a		; If not, skip vt52 emulation stuff
	ldy	vtmod		; Are we in vt52 mode?
	jsr	case
	.word	telp2a		; glass tty. skip vt52 emulation
	.word	dovt52		; call vt52 and jmp to ttelprr
	.word	dovt100		; call vt100 and jmp to ttelprr

dovt52:	jsr	vt52		; process the character after the esc
	jmp	telprr

dovt100: jsr	vt100		; process a character in an esc sequence
	jmp	telprr

telp2a:
	ldy	scrtype		; kludge.  Only xlate to atascii
	bne	telp2b		;  if using atari E: screen
;
;	tax			; do the appropriate translation
;	lda	astoat,x
;
	ldx	#<xas2at	; point at table
	ldy	#>xas2at
	jsr	xlate		; translate it
;
	sta	char
telp2b:
	cmp	#$20		; if less than $20, not printable character
	bcc	telp3a
;	cmp	#$20+95		; one of the 96 printable characters?
;	bcs	telp3a		; nope
;	jsr	cout		; print the normal character
	jsr	scrput		; [jrd]
	clc			; repeat forever
	jmp	chrlup
telp3a:	jsr	telpr3		; process it
telprr:	clc			;[39] Repeat Main terminal loop
	jmp	chrlup		;[39]		...
 
telcnc:	jsr	getkey		; Get a keyboard byte
;	beq	telcrs		; None available, return
	bcs	telcrs		; None available, return
;	tax			; run the character through the table
;	lda	attoas,x
	sta	char		; save it
	bpl	tlcnc5		; if pos, process it
	ldx	#<vt100fk	; zzz for now, vt100 wired
	ldy	#>vt100fk
	jsr	fksend		; send the fun key
	jmp	telcrs		; finish terminal processing
tlcnc5:	cmp	escp		; Is it the connect-escape character?
	bne	tlcnc6		; If so, go handle the interupt character
	ldx	scrtype		; [jrd] only do this if in atari scr
;
;zzz redo this loop, it's getting gross.
;
	bne	tlcnc6		; [jrd] nope, we don'use it here
	jmp	intchr
tlcnc6:	lda	char
	jsr	putrs		;[39] Output the port character
	ldx	lecho		; Is local-echo turned on?
	cpx	#on		;		...
	bne	telcrs		; If not, we are done
;	jsr	cout		; Output a copy to the screen
	jsr	scrput		; [jrd]
telcrs:
	jsr	scrfls		;[EL] Go blink the cursor
	lda	scrtype		; in 40 mode?
	beq	telcrs1		; nope, repeat loop
	lda	#$08		; sanity value, H/W man page III.15
	sta	CONSOL
	lda	CONSOL		; read console switches
	eor	#$FF		; reverse bits
	sta	strptr		; save for a bit
	and	#$04		; Option key down?
	beq	telcrs0		; nope, see about panning
	jmp	intchr		; go do an option
telcrs0:
	lda	scrtype		; check screen type again
	cmp	#scr40
	bne	telcrs1
	lda	strptr		; get original bits
	and	#$03		; mask for start, select
	bne	telpan		; something's set, pan
telcrs1:
	clc			;[39] Repeat main terminal loop
	jmp	chrlup		;[39]		...
;
telpan:
	and	#$01		; start?
	beq	tlpl		; nope, must be select, pan left
	inc	panval		; bump pan offset
	lda	panval
	cmp	#40		; better be less
	bcs	tlpr1		; nope, leave it at 40
	jsr	pan40		; go do it
	jmp	chrlup		; go do terminal loop
tlpr1:	lda	#39
	sta	panval		; reset it to something reasonable
	jmp	chrlup
tlpl:	dec	panval		; dec panval by 1
	bmi	tlpl1		; oops, too far
	jsr	pan40		; do it
	jmp	chrlup
tlpl1:	lda	#0
	sta	panval
	jmp	chrlup

;	Handle special input characters
 
telpr3:	cmp	#bel		; Is it a ^G (bell)
	bne	tlpr3a		; No
	jsr	bell		; Ring bell
	rts			;[39]
tlpr3a: cmp	#cr		; Is it a ^M (cr) ?
	bne	tlpr3b		; No
	jsr	scrcr		; Go do a <cr>
	rts			;[39]
tlpr3b:	cmp	#tab		;[26] Is it a ^I (tab) ?
	bne	tlpr3c		;[26] No
;	jsr	prttab		;[26] Print to the next tab stop
	jsr	vtnxtt		; do a real tab
	rts			;[39]
tlpr3c:	cmp	#esc		; Was it an 'escape'?
	bne	tlpr3d		; No
	lda	#on		; Set the escape flag on
	sta	escflg		;		...
	lda	#0		; zero pointers for vt100 emulation
	sta	vt100st		; state is zero
	sta	vt100pt		; parameter pointer is zero
	rts			; Return
tlpr3d:	cmp	#lf		; was it a line feed
	bne	tlpr3e
	jsr	scrlf		; perform the line feed
	rts 
tlpr3e:	cmp	#bs		; was it a backspace?
	bne	tlpr3f
	jsr	scrl		; move the cursor left
	rts
tlpr3f: cmp	#so		; SO?
	bne	tlpr3g		; nope
	lda	#1		; yes, set alt char set (G1)
	sta	altcs		;  this isn't really right for vt52,
	rts			;  but who cares?
tlpr3g: cmp	#si		; SI?
	bne	tlpr3h		; nope
	lda	#0		; yes, clear alt char set flg
	sta	altcs
;	rts
tlpr3h:
	rts
;ctrl-l
;
;	Intchr - processes the character which follows the interupt
;	character and performs functions based on what that character
;	is.
;
 
intchr:	lda	#<conm3		; prompt in status line
	ldy	#>conm3
	ldx	#0
	jsr	slput		; display it
	jsr	rdkey		; Get the next character
	lda	char		;[31]
	sta	kerchr		; Save a copy of it
	and	#$5F		; Capitalize it
	cmp	#'C'		; Does user want the connection closed?
	bne	intch0		; If not, try next option
;
; talk about your basic brain damage...
;	pla			;[39] Fix the stack
;	pla			;[39]
;
	lda	#false		;[48]
	sta	connec		;[48]
; zzz	jsr	scrrst		; reset the screen to normal characterstics
	jsr	scrext
	lda	#scrae		; go back to atari E: screen
	sta	scrtype
	jmp	kermit		;[39]
intch0: cmp	#'S'		; Does the user want status?
	bne	intch1		; Nope
	jsr	stat01		;[EL] Give it to him
	jmp	intex		; [jrd]
intch1: cmp	#'B'		;[DD] Send break?
	bne	intc1a		; No
	jsr	sbreak		; Yes, go send one
	jmp	intex		; [jrd]
intc1a: lda	kerchr		; Fetch back the original character
	and	#$7F		; Get rid of the H.O. bit
	cmp	#'?'		; Does user need help?
	bne	intch2		; If not, continue
	ldx	#<inthlp	; Get the address of the proper help string
	ldy	#>inthlp	;		...
	jsr	prstr		; Print the help stuff
	jmp	intchr		; Get another option character
intch2: cmp	escp		; Is it another connect-escape?
;	bne	intch4		;[39]
	bne	intex		;[jrd]
	jsr	putrs		; Stuff the character at the port
	jmp	intex		;[39]
;intch4:
;	cmp	#'0'		;[39]
;	bne	intch3		;[39] Nope, this is an error
;	lda	#$00		;[39]
;	jsr	putrs		;[39]
;	jmp	intex		;[39]
intch3: jsr	bell		; Sound bell at the user
;
intex:				; put the status line back
	jsr	telslm		; do appropriate msg
	jmp	telcrs		; and go back
;
;	Display appropriate msg in status line
;
conm1:	.byte	"K65: Option",0
conm2:	.byte	",Sel,Sta",0
conm3:	.byte	"Option (C,S,B,?)",0
telslm:
	lda	#<conm1		; [jrd] point at stat line text
	ldy	#>conm1
	ldx	#0		; offset in stat line
	jsr	slput		; put it
	lda	scrtype
	cmp	#scr80		; in 80-col?
	beq	telslm1		; yes, go ahead and connect
	lda	#<conm2		; no, print start/sel msg
	ldy	#>conm2
	jsr	slput
telslm1:
	jsr	updstat		; update other stat line fields
	rts

;
; debug code.  Log char received into status line
;
teldbg:
	ldy	#0		; idx into stat line
	ldx	#18		; move 18 bytes
teldbg1:
	lda	statline+3,y	; get a byte
	sta	statline,y	; more it over
	iny			; bump idx
	dex			; dec count
	bne	teldbg1		; and go around
	lda	char		; get the char
	lsr	a		; shift it down
	lsr	a
	lsr	a
	lsr	a
	jsr	ny2hx		; get a hex char
	sec
	sbc	#$20		; offset for display
	sta	statline,y	; shove it in
	iny
	lda	char		; get char again
	and	#$0F		; mask
	jsr	ny2hx
	sec
	sbc	#$20		; offset for display
	sta	statline,y	; this one into stat line
	iny
	lda	#0		; display a space
	sta	statline,y
	rts

;ctrl-l;
;	Vt52 - will carry out the equivalent of most of the vt52 functions
;	available.
;
 
vt52:	lda	#off		; First, turn off the escape flag
	sta	escflg		;		...
	lda	char		;[26] Get the character to check
	and	#$7F		; Turn off the H.O. bit
	cmp	#'A'		; Is it greater than 'A' and less than
	bmi	vt52y		;[26]	or equal to 'z'????
	cmp	#'z'		;[26]		...
	bmi	vt52z		;[26] If it isn't, ignore it
vt52y:	jmp	vtig		;[26]
vt52z:
; all obsolete?
;	sec			;[26] Get the cursor position
;	jsr	ploth		;[26] in X,Y
;;	sty	hch		;[39]
;	stx	hch		;[39]
;;	stx	hcv		;[39]
;	sty	hcv		;[39]
	cmp	#'A'		; It is, is it an 'A'?
	bne	vt52a		; No, try next character
	jsr	scru		; Go up one line
	rts			; Return
vt52a:  cmp	#'B'		; Is it a 'B'?
	bne	vt52b		; Next char
	jsr	scrd		; Yes, go down one line
	rts			;	and go back
vt52b:  cmp	#'C'		; 'C'?
	bne	vt52c		; Nope
	jsr	scrr		; Yes, go forward one space
	rts			;	and return
vt52c:  cmp	#'D'		; 'D'?
	bne	vt52d		; No
	jsr	scrl		; Yes, do a back-space
	rts			; Return
vt52d:  cmp	#'H'		; 'H'?
	bne	vt52e		; No, try next character
	ldx	#0
	ldy	#0
	jsr	scrplt		; Home cursor (no clear screen)
	rts			;	then return
vt52e:  cmp	#'I'		; 'I'?
	bne	vt52f		; Nope
	jsr	scrrlf		;[39] Do a reverse line feed
	rts			;  and return
vt52f:  cmp	#'J'		; 'J'?
	bne	vt52g		; No
	jsr	scred0		; Clear from where we are to end-of-page
	rts			;	then return
vt52g:  cmp	#'K'		; 'K'?
	bne	vt52h		; Try last option
	jsr	screl0		; Clear to end-of-line
	rts			; Return
vt52h:  cmp	#'Y'		; 'Y'
	bne	vt52i		;[19]
	jsr	vtdca		; Do direct cursor addressing
	rts			;	then return
vt52i:	cmp	#'o'		;[19] 'o'
	bne	vt52j		;[19]
	lda	#1
	sta	reverse		; turn reverse on
	rts			;[19] Return
vt52j:	cmp	#'n'		;[19] 'n'
	bne	vtig		;[19] Must be an unimplemented fn, do vtig
	lda	#0
	sta	reverse		; turn reverse off
	rts			;[19]
vtig:	pha			; Save a copy
	lda	#esc		; Get an escape
	jsr	prchr		; Print the special character
	pla			; Fetch the other character back
	cmp	#esc		; Is it a second escape?
	bne	vtig1		; Nope, print it
	lda	#on		; Set escflg on again for next time around
	sta	escflg		;		...
	rts			;	and return
vtig1:  jsr	prchr		; Print the character
	rts			;	and return
 
vtdca:	jsr	getrs		; Check for a character from the port
	bne	vtdca		; Try again
	lda	char		;[31]
	and	#$7F		; Make sure H.O. bit is off
	sec			; Subtract hex 30 (make it num from 0 to 23)
	sbc	#$20		;		...
vtdca2: pha			; save it
vtdca3:	jsr	getrs		; Check port for character
	bne	vtdca3		;	go back and try again
	lda	char		;[31]
	and	#$7F		; Make sure h.o. bit is off
	sec			; Subtract hex 20 (make it num from 0 to 23)
	sbc	#$20		;		...
vtdca5: tax			; this is the horizontal position
	pla			; remember the vertical position
	tay
	jsr	scrplt		; move the cursor here
	jsr	scrpan		; pan if needed
	rts			;	and return
 
 
;ctrl-l.SBTTL	VT100 Emulation Routines

;
;	vt100 - parse a character in a vt100 command sequence
;
;	Input - A character in the A-reg
;
;	This routine processes characters after an esc in VT100 mode.
;	It parses the command and calls a routine to perform the requested
;	function when the last character in the sequence has been received.
;

vt100:	ldx	vt100st		; state of the command parser
vt100d:	ldy	vt100ta,x	; check the parser table
	beq	vt100b		; esape sequence is illegal
	bpl	vt100a		; is parameter expected?
	cmp	#1+'9'		; yes.  Was a digit received?
	bcs	vt100a		; no, it is not a digit
	cmp	#'0'
	bcc	vt100a		; not a digit (carry set for next line)
	sbc	#'0'		; convert the digit to a value (0..9)
	pha			; save it
	ldy	vt100pt		; pointer into parameter list
	lda	freemem,y		; get the current value
	asl	a		; multiplied by 2
	pha			; save that too
	asl	a		; multiplied by 4
	asl	a		; multiplied by 8
	sta	freemem,y
	pla
	clc
	adc	freemem,y	; multiplied by 10
	sta	freemem,y
	pla
	clc
	adc	freemem,y	; add in the digit
	sta	freemem,y	; save the new value of the parameter
	rts			; all done (for now. escflg still set)

vt100a:	cmp	vt100ta,x	; found character in table?
	beq	vt100c		; yes. go change state
	inx			; skip to the next entry
	inx
	inx
	jmp	vt100d		; check this character

vt100c:	lda	vt100ta+2,x	; high order byte of routine to call
	beq	vt100e		; $00 = state change
	sta	dest+1
	lda	vt100ta+1,x	; low order byte of routine to call
	sta	dest
	lda	#0
	sta	escflg		; this command is complete
	jmp	(dest)		; perform requested function

vt100e:	ldy	vt100ta+1,x	; state to change to
	sty	vt100st		; change to it
	lda	vt100ta,y	; is a parameter expected?
	bpl	vt100f		; no.
	inc	vt100pt		; make pointer point to next parameter
	ldy	vt100pt		; and zero the parameter
	lda	#0
	sta	freemem,y
vt100f:	rts			; all done (for now. escflg still set)

vt100b:	lda	#0		; an error has occured.  abort processing
	sta	escflg
	rts			; all done

;
;	vt100b1 - process the <esc> '['  integer 'J' vt100 sequence
;
;	This routine calls scred0, scred1, or scred2 depending on the
;	value of the integer.
;

vt100b1: ldy	freemem+1	; what is the integer
	cpy	#3		; check for strange vt100 sequences
	bcs	vt100er		; this is a strange sequence
	jsr	case		; call the proper routine
	.word	scred0		; call scred0 if the integer is 0
	.word	scred1		; call scred1 if the integer is 1
	.word	scred2		; call scred2 if the integer is 2

;
;	vt100c1 - process the <esc> '[' integer 'K'
;
;	This routine calls screl0, screl1, or screl2 depending on the
;	value of the integer.

vt100c1: ldy	freemem+1	; what is the integer
	cpy	#3		; check for strange vt100 sequences
	bcs	vt100er		; this is a strange sequence
	jsr	case		; call the proper routine
	.word	screl0		; call screl0 if the integer is 0
	.word	screl1		; call screl1 if the integer is 1
	.word	screl2		; call screl2 if the integer is 2

;
;	vt100d1 - process the <esc> '[' integer ';' integer 'f' and
;			     <esc> '[' integer ';' integer 'H' vt100 commands
;
;	This routine calls scrplt to put the cursor at the position indicated
;	by the two integers.

vt100d1: ldx	#0		; get the first integer
	ldy	#1		; default value is 1
	jsr	vt100pa
	tay
	dey			; solve the off-by-one problem
	cpy	#24		; check it for reasonability
	bcc	vt100d2
	tay			; if unreasonable, move cursor to bottom line
vt100d2: sty	dest		; save y position
	ldx	#1		; get the second integer
	ldy	#1		; default value is 1
	jsr	vt100pa
	tax
	dex			; solve the off-by-one problem
	jsr	rghchk		; check it for reasconablilty
	bcc	vt100d3
	tax			; if unreasonable, move cursor to far right
vt100d3: ldy	dest		; get y position
	jsr	scrplt		; finally move the cursor
	jsr	scrpan		; pan if necessary
	rts			; all done

;
;	vt100e1 - process the <esc> integer ';' integer 'r' sequence
;
;	This routine sets the top and bottom of the scrolling area.
;

vt100e1: ldx	#0		; get the first parameter
	ldy	#1		; default value is one
	jsr	vt100pa
	tay
	dey			; solve the off-by-one problem
	jsr	botchk		; check it for reasonability
	bcs	vt100e2
	sty	dest		; save top of screen
	tay			; get default value for bottom (from botchk)
	iny			; solve the off-by-one problem
	ldx	#1		; get the second parameter
	jsr	vt100pa
	tay
	dey			; solve the off-by-one problem
	jsr	botchk		; check it for reasonablilty
	bcs	vt100e2
	cpy	dest		; save it
	bcc	vt100e2		; unreasonable
	sty	bot		; set the bottom margin
	ldy	dest		; set the tom margin
	sty	top
;
; zzz deal with origin mode here
;
	ldy	#0		; for now assume normal origin
	ldx	#0		; move cursor to home position
	jsr	scrplt

vt100e2: rts

vt100er: jsr	beeplo
	rts
	
;
;	vt100f1 - process the <esc> '[' integer 'A' sequence
;
;	This routine moves the cursor up <integer> lines
;

vt100f1: ldx	#0		; get the parameter
	ldy	#1		; default value is one
	jsr	vt100pa
	sec			; cutsy way to subtract it form cursor pos
	eor	#$FF
	adc	ROWCRS
	tay
	bcc	vt100f3		; gone past top of screen
	cpy	top		; outside scrolling area
	bcs	vt100f2		; no
vt100f3: ldy	top		; move cursor to top
vt100f2: ldx	COLCRS
	jsr	scrplt		; plot the cursor here
	rts

;
;	vt100g1 - process the <esc> '[' integer 'B' sequence
;
;	This routine moves the cursor down <integer> lines
;

vt100g1: ldx	#0		; get the parameter
	ldy	#1
	jsr	vt100pa
	clc			; add the parameter to ROWCRS
	adc	ROWCRS
	tay
	cpy	bot		; see if still in scrolling area
	beq	vt100g2
	bcc	vt100g2
	ldy	bot		; nope. move the cursor to the bottom
vt100g2: ldx	COLCRS
	jsr	scrplt		; plot the cursor here
	rts			; all done

;
;	vt100h1 - process the <esc> '[' integer 'C' sequence
;
;	This routine moves the cursor right <integer> characters
;

vt100h1: ldx	#0		; get the parameter
	ldy	#1		; default value in one
	jsr	vt100pa
	clc			; add it into the current cursor position
	adc	COLCRS
	tax
	jsr	rghchk		; check it for reasonability
	bcc	vt100h2		; it is reasonable
	tax			; if unreasonable, move cursor to far right
vt100h2: ldy	ROWCRS		; plot the cursor here
	jsr	scrplt
	rts

;
;	vt100i1 - process the <esc>  '[' integer 'D' sequence
;
;	This routine moves the cursor left <integer> characters
;

vt100i1: ldx	#0		; get the parameter
	ldy	#1		; default value is one
	jsr	vt100pa
	sec			; cutsy way to subtract from COLCRS
	eor	#$FF
	adc	COLCRS
	bcs	vt100i2		; check if gone past left margin
	lda	#0		; if so, move to far left
vt100i2: tax
	ldy	ROWCRS		; plot the cursor here
	jsr	scrplt
	rts

;
;	vt100j1 - process the <esc> '[' [  integer ';' ...] 'm' sequence
;
;	This routine sets the graphic rendition (reverse, alternate colors,
;	underline and flashing) parameters.  Note that it may be passed
;	0 or more parameters
;

vt100j1: ldx	#0		; start with the first parameter
vt100j5: ldy	#0		; default value is zero
	jsr	vt100pa
	beq	vt100j3		; if zero, clear everything
	tay
	cpy	#8
	bcs	vt100j4		; unreasonable parameter!
	lda	#1		; set the proper parameter
	sta	vt100gr-1,y
	bne	vt100j4		; always taken
vt100j3: jsr	vt100j2		; clear everything
vt100j4: inx			; get the next parameter
	cpx	vt100pt		; all done?
	bcc	vt100j5		; nope.  Do some more
	rts			; all done.

vt100j2: lda	#0		; clear everything
	sta	alternt		; alternate color (highlighting)
	sta	flash		; flashing off
	sta	underln		; dont underline
	sta	reverse		; dont reverse
	rts			; everything cleared.

;
;	vt100k1 - process the <esc>  '[' integer 'D' sequence
;
;	This routine deletes <integer> lines
;

vt100k1: ldx	#0		; get the parameter
	ldy	#1		; default value is one
	jsr	vt100pa
	sta	tmpptr		; [jrd] this should be safe...
vt100k2:
	dec	tmpptr		; dec count
	bmi	vt100k3		; if gone minus, stop
	jsr	scrdl		; delete one
	jmp	vt100k2
vt100k3:			; done
	rts

;
;	vt100l1 - process the <esc>  '[' integer 'D' sequence
;
;	This routine inserts <integer> lines
;

vt100l1: ldx	#0		; get the parameter
	ldy	#1		; default value is one
	jsr	vt100pa
	sta	tmpptr		; [jrd] this should be safe...
vt100l2:
	dec	tmpptr		; dec count
	bmi	vt100l3		; if gone minus, stop
	jsr	scril		; insert one
	jmp	vt100l2
vt100l3:			; done
	rts


;
;	botchk - check to see if y-reg is below bottom of screen
;
;	Output:	Carry flag set if past bottom of screen
;		A-reg holds line number of screen bottom
;
;	This routine checks to see if the y-reg is greater than the bottom
;	of the screen.

botchk:	lda	#23
	cpy	#24
	rts

;
;	rghchk - check to see if x-reg is past right margin of screen
;
;	Input:	scrtype
;
;	Output:	Carry flag set if past right margin of screen
;		A-reg holds right margin of screen
;
;	This routine checks to see if the x-reg is greater than the bottom
;	of the screen.

rghchk:	lda	scrtype		; check to see if in 40-column mode
	beq	rghchk1		; branch if it is
	lda	#79
	cpx	#80
	rts
rghchk1: lda	#39		; only 40 columns available
	cpx	#40
	rts

;
;	vt100pa - get a parameter for vt100 emulation
;
;	Input:	X-reg - which parameter is desired (0..n)
;		Y-reg - default value of this parameter
;
;	Output:	A-reg - value of this parameter
;
;	This routine returns the value of the requested parameter.  If
;	the parameter is zero or undefined, it returns the default value.
;

vt100pa: cpx	vt100pt		; was the necessary number of params given
	bcs	vt100pb		; no, use the default
	lda	freemem+1,x	; get this parameter
	beq	vt100pb		; if zero, use the default
	rts
vt100pb: tya			; return the default
	rts

;
; 	things to set char set slots in response to SCS esc sequences
;
scsg0us:
	lda	#csascii	; use char set ascii 
	sta	csg0		;  for G0
	rts
scsg0gr:
	lda	#csgraf		; use graphics set
	sta	csg0		;  got G0
	rts
scsg1us:
	lda	#csascii	; use ascii
	sta	csg1		;  for G1
	rts
scsg1gr:
	lda	#csgraf		; use graphics
	sta	csg1		;  for G1
	rts

;
; set tab at current column
;
vttset:
	ldx	COLCRS		; get column
	lda	#1
	sta	tabstop,x	; zap it
	rts

;
; clear tab at current col.
;
vttclr:
	ldx	#0		; parm 0 please
	ldy	#0		; default 0
	jsr	vt100pa		; get a parameter
	cmp	#3		; clear all?
	beq	vttclra		; yes, clear all
	ldx	COLCRS		; get column
	lda	#0
	sta	tabstop,x	; zap it
	rts
vttclra:
	ldx	#79
	lda	#0
vttcn:	sta	tabstop,x
	dex
	bpl	vttcn
	rts

vttini:
	jsr	vttclra		; first clear all
	lda	#8
vtti1:
	tax
	lda	#1
	sta	tabstop,x
	txa
	clc
	adc	#8
	cmp	#80
	bcc	vtti1
	rts	

;
; next tab stop
;
vtnxtt:
	ldx	COLCRS
vtnt1:
	inx
	cpx	#80
	bcs	vtnt8
	lda	tabstop,x
	beq	vtnt1
	bne	vtnt9
vtnt8:	ldx	#79
vtnt9:	ldy	ROWCRS
	jsr	scrplt
	rts

;
;	vt100ta - parser table for vt100 commands
;
;	the first byte of each entry is a character to expect.  If the
;	character to expect is negative, it means to parse a parameter
;	and remain in the current state.  If it is zero, that is the end
;	of the entry.  If it is the character received,	the next word is looked
;	at.  If it is less than $100, the parser changes into that state.  If
;	it is greater or equal to $100, the routine at that address is called.
;

vt100ta: .byte	'['
	.word	vt100a1-vt100ta
	.byte	'M'		; <esc> 'M'
	.word	scrrlf		;		is reverse index
	.byte	'E'		; <esc> 'E'
	.word	scrlf		;		is next line
	.byte	'D'		; <esc> 'D'
	.word	scrlf		;		is index
	.byte	'7'		; <esc> '7'
	.word	scrsav		;		means save cursor position
	.byte	'8'		; <esc> '8'
	.word	scrlod		;		means load cursor position
	.byte	'H'		; <esc> 'H'
	.word	vttset		;		set tab stop 
;
; added by jrd for SCS support. 
;
	.byte	'('		; <esc> '('
	.word	vt100a4-vt100ta	; set a4 state, expecting g0 spec
	.byte	')'		; <esc> ')'
	.word	vt100a5-vt100ta	; set a5 state, expecting g1 spec
;
	.byte	$00

vt100a1: .byte	$FF
	.word	0
	.byte	'J'		; <esc> '[' integer 'J'
	.word	vt100b1
	.byte	'K'		; <esc> '[' integer 'K'
	.word	vt100c1
	.byte	'A'		; <esc> '[' integer 'A'
	.word	vt100f1
	.byte	'B'		; <esc> '[' integer 'B'
	.word	vt100g1
	.byte	'C'		; <esc> '[' integer 'C'
	.word	vt100h1
	.byte	'D'		; <esc> '[' integer 'D'
	.word	vt100i1
	.byte	'm'		; <esc> '[' [integer ';']... 'm'
	.word	vt100j1
;
; added by jrd, for insert/delete line support
;
	.byte	'M'
	.word	vt100k1		; delete line(s)
	.byte	'L'
	.word	vt100l1		; insert line(s)
;
	.byte	';'
	.word	vt100a2-vt100ta
	.byte	'f'		; <esc> '[' 'f'
	.word	vt100d1
	.byte	'H'		; <esc> '[' 'H'
	.word	vt100d1
	.byte	'r'		; <esc> '[' 'r'
	.word	vt100e1
	.byte	'?'		; <esc> '[' '?'
	.word	vt100a3-vt100ta
	.byte	'g'		; <esc> '[' g
	.word	vttclr		; clear tab stop
	.byte	$00

vt100a2: .byte	$FF
	.word	0
	.byte	'H'
	.word	vt100d1		; <esc> integer ';' integer 'H'
	.byte	'f'
	.word	vt100d1		; <esc> integer ';' integer 'f'
	.byte	'r'
	.word	vt100e1		; <esc> integer ';' integer 'r'
	.byte	'm'
	.word	vt100j1		; <esc> integer ';' integer... 'm'
;
; added by jrd.  is this necessary?
;
	.byte	';'
	.word	vt100a2-vt100ta	; ; means stay in this state
;
	.byte	0

vt100a3: .byte	$FF
	.word	0
	.byte	'h'		; <esc> '[' '?' integer 'h'
	.word	anyrts		; ignored (for now)
	.byte	'l'		; <esc> '[' '?' integer 'l'
	.word	anyrts		; ignored (for now)
	.byte	';'
	.word	vt100a3-vt100ta
	.byte	0

;
; SCS support. Note that we only support the USASCII and Graphics sets,
; others are ignored.
;
vt100a4: 			; no parameters here
	.byte	'B'		; <esc> ( B
	.word	scsg0us		; set us ascii as g0
	.byte	'0'		; <esc> ( 0
	.word	scsg0gr		; set graphics as g0
	.byte	'A'		; <esc> ( A
	.word	scsg0us		; should really be uk
	.byte	0

vt100a5: 
	.byte	'B'		; <esc> ) B
	.word	scsg1us
	.byte	'0'
	.word	scsg1gr
	.byte	'A'		; <esc> ) A
	.word	scsg1us		; should really be uk
	.byte	0

	.byte	*-vt100ta	; abort assembly if table length > $100
;ctrl-l.SBTTL	Exit routine
 
;
;	This routine exits properly from Kermit-65 and reenters
;	Atari DOS
;
;		Input:  NONE
;
;		Output: NONE
;
;		Registers destroyed:	A,X
;
 
;exit:	lda	#cmcfm		; Try to get a confirm
;	jsr	comnd		; Do it
;	 jmp	kermt3		; Give '?not confirmed' message
;
exit1:	jsr	restor		;[36] Restore everything to its' default state
;	lda	r6510		;[17] Prepare to terminate
;	ora	#1		;[17]  by paging BASIC ROM in
;	sta	r6510		;[17] 		...
;exit2:  jmp	(dos)		; Now restart BASIC
	ldx	keresp		; [jrd] Not here you don't.
	txs			;    Get our stack back
	lda	kerret+1	;    make sure ret addr is ok
	pha			;    and...
	lda	kerret
	pha
	rts			;   return

restor:
; [jrd]	jsr	clall		;[19][36] Close all channels
	jsr	scrext		; restore screen hardward to its initial state
	rts			;[36] Return
 
;ctrl-l.SBTTL	Help routine
 
;
;	This routine prints help from the current help text
;	area.
;
;		Input:  Cmhptr  - Pointer to the desired text to be printed
;
;		Output: ASCIZ string at Cmhptr is printed on screen
;
;		Registers destroyed:	A,X,Y
;
 
help:	lda	#cmcfm		; Try to get a confirm
	jsr	comnd		; Go get it
	 jmp	kermt3		; Didn't' find one? Give 'not confirmed' message
help2:  ldx	cmhptr		; L.O. byte of current help text address
	ldy	cmhptr+1	; H.O. byte of address
;	jsr	prstr		; Print it
;	jmp	kermit		; Return to main routine
	jmp	kermzz		; [jrd] use common print vector
;
 
;ctrl-l.SBTTL	Log routine
 
;
;	This routine logs a session to a disk file.
;
;		Input:  NONE
;
;		Output: NONE
;
;		Registers destroyed:	NONE
;
 
log:	jmp	kermit
 
;ctrl-l.SBTTL	Bye routine
 
;
;	This routine terminates the remote server, logs out and terminates
;	the local Kermit.
;
 
bye:	jsr	prcfm		; Go parse and print the confirm
	jsr	logo		; Tell other Kermit to log out
	 jmp	kermit		; Don't exit if there was an error
quit:
;	jmp	exit1		; Leave
	rts			; [jrd] let the main routine exit for us
 
;
;	Logo - This routine does the actual work to send the logout
;	packet to the remote server
;
 
logo:	jsr	openrs		;[27] Reset the RS-232 channel
	lda	#0		; Zero the number of tries
	sta	numtry		;		...
	sta	tpak		;	and the total packet number
	sta	tpak+1		;		...
	lda	#<pdbuf		;[29] Get the address of the packet buffer
	sta	kerbf1		;[29]   and save it for Spak
	lda	#>pdbuf		;[29]		...
	sta	kerbf1+1	;[29]		...
logo1:	lda	numtry		; Fetch the number of tries
	cmp	maxtry		; Have we exceeded Maxtry?
	bmi	logo3		; Not yet, go send the packet
logo2:	ldx	#<ermesc	; Yes, give an error message
	ldy	#>ermesc	;		...
	jsr	prstr		;		...
	jsr	prcrlf		;		...
	rts			;	and return
logo3:	inc	numtry		; Increment the number of tries for packet
	lda	#0		; Make it packet number 0
	sta	pnum		;		...
	lda	#1		; Data length is only 1
	sta	pdlen		;		...
	lda	#'L'		; The 'Logout' command
	sta	pdbuf		; Put that in first character of buffer
	lda	#'G'		; Generic command packet type
	sta	ptype		;		...
	jsr	flshin		;[25] Flush the RS232 buffer
	jsr	spak		; Send the packet
	jsr	rpak		; Try to fetch an ACK
	cmp	#true		; Did we receive successfully?
	bne	logo1		; No, try to send the packet again
	lda	ptype		; Get the type
	cmp	#'Y'		; An ACK?
	bne	logoce		; No, go check for error
	jmp	rskp		; Yes, skip return
logoce:	cmp	#'E'		; Error packet?
	bne	logo1		; Nope, resend packet
	jsr	prcerp		; Go display the error
	rts			;	and return
 
;ctrl-l.SBTTL	Finish routine
 
;
;	This routine terminates the remote server but does not log
;	it out. It also keeps the local Kermit running.
;
 
finish:	jsr	prcfm		; Go parse and print the confirm
	jsr	openrs		;[27] Reset the RS232 channel
	lda	#0		; Zero the number of tries
	sta	numtry		;		...
	sta	tpak		;	and the total packet number
	sta	tpak+1		;		...
	lda	#<pdbuf		;[29] Get the address of the packet buffer
	sta	kerbf1		;[29]   and save it for Spak
	lda	#>pdbuf		;[29]		...
	sta	kerbf1+1	;[29]		...
finsh1:	lda	numtry		; Fetch the number of tries
	cmp	maxtry		; Have we exceeded Maxtry?
	bmi	finsh3		; Not yet, go send the packet
finsh2:	ldx	#<ermesd	; Yes, give an error message
	ldy	#>ermesd	;		...
	jsr	prstr		;		...
	jsr	prcrlf		;		...
	jmp	kermit		;	and go back for more commands
finsh3:	inc	numtry		; Increment the number of tries for packet
	lda	#0		; Make it packet number 0
	sta	pnum		;		...
	lda	#1		; Data length is only 1
	sta	pdlen		;		...
	lda	#'F'		; The 'Finish' command
	sta	pdbuf		; Put that in first character of buffer
	lda	#'G'		; Generic command packet type
	sta	ptype		;		...
	jsr	flshin		;[25] Flush the RS232 buffer
	jsr	spak		; Send the packet
	jsr	rpak		; Try to fetch an ACK
	cmp	#true		; Did we receive successfully?
	bne	finsh1		; No, try to send the packet again
	lda	ptype		; Get the type
	cmp	#'Y'		; An ACK?
	bne	fince		; No, go check for error
	jmp	kermit		; Yes, go back for more commands
fince:	cmp	#'E'		; Error packet?
	bne	finsh1		; Nope, resend packet
	jsr	prcerp		;; Go display the error
	jmp	kermit		; Go back for more 
 
;ctrl-l.SBTTL	Get routine
 
;
;	This routine accepts an unquoted string terminated by 
;	<cr>,<lf>,<ff>, or <esc> and tries to fetch the file
;	represented by that string from a remote server Kermit.
;
 
getfrs:
	jsr	openrs		;[27] Reset the RS232 channel
	lda	#yes		; Make KERMIT use file headers
	sta	usehdr		;	for file names
	lda	#mxfnl+1	; The buffer size is one more than max
	sta	kwrk01		;	file name length
	lda	#<fcb1		; Point to the buffer
	sta	kerto		;		...
	lda	#>fcb1		;		...
	sta	kerto+1		;		...
	jsr	kerflm		; Clear the buffer
	lda	#$80		; Reset all break characters
	jsr	rstbrk		;		...
	lda	#cr		;		...
	jsr	setbrk		;		...
	lda	#lf		;		...
	jsr	setbrk		;		...
	lda	#ffd		;		...
	jsr	setbrk		;		...
	lda	#esc		;		...
	jsr	setbrk		;		...
	ldy	#0		;		...
	lda	#cmtxt		; Parse for text
	jsr	comnd		; Do it
	 jmp	kermta		; Found null string
	cmp	spsiz		; Larger than the set packet size?
	bmi	getf1		; No, continue
	lda	spsiz		; Yes, it will have to be truncated
getf1:	sta	kwrk01		; Store packet size for Kercpy
	sta	pdlen		;	and Spak
	lda	#<pdbuf		; Point to the data buffer as destination
	sta	kerto		;		...
	sta	kerbf1		; Store L.O.B. here for Spak routine
	lda	#>pdbuf		;		...
	sta	kerto+1		;		...
	sta	kerbf1+1	; Store H.O.B. here for Spak routine
	stx	kerfrm		; Point to the atom buffer from Comnd
	sty	kerfrm+1	;	as the source address
	txa			; Save the 'from buffer' pointers for later
	pha			;		...
	tya			;		...
	pha			;		...
	jsr	kercpy		; Copy the string
	pla			; Restore these for the next move
	sta	kerfrm+1	;		...
	pla			;		...
	sta	kerfrm		;		...
	lda	#<fcb1		; Set up the address of the target
	sta	kerto		;		...
	lda	#>fcb1		;		...
	sta	kerto+1		;		...
	jsr	clrfcb		; Clear the fcb first
	jsr	kercpy		; Go move the string
	jsr	prcfm		; Go parse and print the confirm
	lda	#'R'		; Packet type is 'Receive-init'
	sta	ptype		;		...
	lda	#0		; Packet number should be zero
	sta	pnum		;		...
	jsr	spak		; Packet length was set above, 
	jsr	rswt		;	so just call spak and try to receive
	lda	#0		; [jrd] make sure file's
	jsr	closef		;  closed in case of error
	jmp	kermit		; Go back for more commands

 
;ctrl-l.SBTTL	Receve routine
 
;
;	This routine receives a file from the remote kermit and
;	writes it to a disk file.
;
;		Input:  Filename returned from comnd, if any
;
;		Output: If file transfer is good, file is output to disk
;
;		Registers destroyed:	A,X,Y
;
 
receve:	jsr	openrs		;[27] Reset the RS232 channel
	lda	#on		; Set use file-header switch on in case we
	sta	usehdr		;	don't parse a filename
	lda	#<kerehr	; Point to extra help commands
	sta	cmehpt		;		...
	lda	#>kerehr	;		...
	sta	cmehpt+1	;		...
	ldx	#mxfnl		; Longest length a filename may be
	ldy	#cmfehf		; Tell Comnd about extra help
	lda	#cmifi		; Load opcode for parsing input files
	jsr	comnd		; Call comnd routine
	 jmp	recev1		; Continue, don't turn file-header switch off
	sta	kwrk01		; Store length of file parsed
	stx	kerfrm		; Save the from address (addr[atmbuf])
	sty	kerfrm+1	;		...
	lda	#<fcb1		; Save the to address (Fcb1)
	sta	kerto		;		...
	lda	#>fcb1		;		...
	sta	kerto+1		;		...
	jsr	clrfcb		; Clear the fcb
	jsr	kercpy		; Copy the string
	lda	#off		; We parsed a filename so we don't need the
	sta	usehdr		;	info from the file-header
recev1: ;lda	#cmcfm		; Get token for confirm
	;jsr	comnd		;	and try to parse that
	; jmp	kermt3		; Failed - give the error
	jsr	prcfm		;[] Parse and print a confirm
	jsr	rswt		; Perform send-switch routine
	jsr	closers		; [jrd] close com port
	jmp	kermit		; Go back to main routine
;
rswt:	lda	#'R'		; The state is receive-init
	sta	state		; Set that up
	lda	#0		; Zero the packet sequence number
	sta	n		;		...
	sta	numtry		;	Number of tries
	sta	oldtry		;	Old number of tries
	sta	eofinp		;	End of input flag
	sta	errcod		;	Error indicator
	sta	rtot		;	Total received characters
	sta	rtot+1		;		...
	sta	stot		;	Total Sent characters
	sta	stot+1		;		...
	sta	rchr		;	Received characters, current file
	sta	rchr+1		;		...
	sta	schr		;	and Sent characters, current file
	sta	schr+1		;		...
	sta	tpak		;	and the total packet number
	sta	tpak+1		;		...
rswt1:  lda	state		; Fetch the current system state
	cmp	#'D'		; Are we trying to receive data?
	bne	rswt2		; If not, try the next one
	jsr	rdat		; Go try for the data packet
	jmp	rswt1		; Go back to the top of the loop
rswt2:  cmp	#'F'		; Do we need a file header packet?
	bne	rswt3		; If not, continue checking
	jsr	rfil		; Go get the file-header
	jmp	rswt1		; Return to top of loop
rswt3:  cmp	#'R'		; Do we need the init?
	bne	rswt4		; No, try next state
	jsr	rini		; Yes, go get it
	jmp	rswt1		; Go back to top
rswt4:  cmp	#'C'		; Have we completed the transfer?
	bne	rswt5		; No, we are out of states, fail
	lda	#true		; Load AC for true return
	rts			; Return
rswt5:  lda	errcod		; [jrd] get the error code
	jsr	prerms		; [jrd] print apropos message
	lda	#false		; Set up AC for false return
	rts			; Return
 
rini:	lda	#<pdbuf		; Point kerbf1 at the packet data buffer
	sta	kerbf1		;		...
	lda	#>pdbuf		;		...
	sta	kerbf1+1	;		...
	lda	numtry		; Get current number of tries
	inc	numtry		; Increment it for next time
	cmp	maxtry		; Have we tried this one enougth times
	beq	rini1		; Not yet, go on
	bcs	rini1a		; Yup, go abort this transfer
rini1:  jmp	rini2		; Continue
rini1a: lda	#'A'		; Change state to 'abort'
	sta	state		;		...
	lda	#errcri		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		; Load AC with false status
	rts			;	and return
rini2:  jsr	rpak		; Go try to receive a packet
	sta	rstat		; Store the return status for later
	lda	ptype		; Fetch the packet type we got
	cmp	#'S'		; Was it an 'Init'?
	bne	rini2a		; No, check the return status
	jmp	rinici		; Go handle the init case
rini2a: lda	rstat		; Fetch the saved return status
	cmp	#false		; Is it false?
	beq	rini2b		; Yes, just return with same state
	lda	#errcri		; No, fetch the error index
	sta	errcod		;	and store it as the error code
	jsr	prcerp		; Check for error packet and process it
	lda	#'A'		; Abort this transfer
	sta	state		; State is now 'abort'
	lda	#false		; Set return status to 'false'
	rts			; Return
rini2b: lda	n		; Get packet sequence number expected
	sta	pnum		; Stuff that parameter at the Nakit routine
	jsr	nakit		; Go send the Nak
	lda	#false		; Set up failure return status
	rts			;	and go back
 
rinici: lda	pnum		; Get the packet number we received
	sta	n		; Synchronize our packet numbers with this
	jsr	rpar		; Load in the init stuff from packet buffer
	jsr	spar		; Stuff our init info into the packet buffer
	lda	#'Y'		; Store the 'Ack' code into the packet type
	sta	ptype		;		...
	lda	n		; Get sequence number
	sta	pnum		; Stuff that parameter
;
; This batch of code depends on the fact that the 'Y that the other side sends
; gets stuffed into sebq if he's merely saying 'Yes I can', not 'OK, I will'.
; 'Y''s not a legal char, so that has the effect of turning ebqmod off.
;
	lda	sebq		; See what we got for an 8-bit quoting
	cmp	#$21		; First check the character range
	bmi	rinicn		; Not in range
	cmp	#$3F		;		...
	bmi	rinicy		; Inrange
	cmp	#$60		;		...
	bmi	rinicn		; Not in range
	cmp	#$7F		;		...
	bmi	rinicy		; Inrange
rinicn: lda	#off		; No, punt 8-bit quoting
	sta	ebqmod		;		...
	lda	#6		; BTW, the data length is now only 6
	jmp	rinic1		; Continue
rinicy: lda	#on		; Make sure everything is on
	sta	ebqmod		;		...
	lda	#7		; Data length for ack-init is 7
rinic1: sta	pdlen		; Store packet data length
	jsr	spak		; Send that packet
	lda	numtry		; Move the number of tries for this packet
	sta	oldtry		;	to prev packet try count
	lda	#0		; Zero
	sta	numtry		;	the number of tries for current packet
	jsr	incn		; Increment the packet number once
	lda	#'F'		; Advance to 'File-header' state
	sta	state		;		...
	lda	#true		; Set up return code
	rts			; Return
 
rfil:	lda	numtry		; Get number of tries for this packet
	inc	numtry		; Increment it for next time around
	cmp	maxtry		; Have we tried too many times?
	beq	rfil1		; Not yet
	bcs	rfil1a		; Yes, go abort the transfer
rfil1:  jmp	rfil2		; Continue transfer
rfil1a: lda	#'A'		; Set state of system to 'abort'
	sta	state		;		...
	lda	#false		; Return code should be 'false'
	rts			; Return
rfil2:  jsr	rpak		; Try to receive a packet
	sta	rstat		; Save the return status
	lda	ptype		; Get the packet type we found
	cmp	#'S'		; Was it an 'init' packet?
	bne	rfil2a		; Nope, try next one
	jmp	rfilci		; Handle the init case
rfil2a: cmp	#'Z'		; Is it an 'eof' packet??
	bne	rfil2b		; No, try again
	jmp	rfilce		; Yes, handle that case
rfil2b: cmp	#'F'		; Is it a 'file-header' packet???
	bne	rfil2c		; Nope
	jmp	rfilcf		; Handle file-header case
rfil2c: cmp	#'B'		; Break packet????
	bne	rfil2d		; Wrong, go get the return status
	jmp	rfilcb		; Handle a break packet
rfil2d: lda	rstat		; Fetch the return status from Rpak
	cmp	#false		; Was it a false return?
	beq	rfil2e		; Yes, Nak it and return
	lda	#errcrf		; No, fetch the error index
	sta	errcod		;	and store it as the error code
	jsr	prcerp		; Check for error packet and process it
	lda	#'A'		; Abort this transfer
	sta	state		;		...
	lda	#false		; Set up failure return code
	rts			;	and return
rfil2e: lda	n		; Move the expected packet number
	sta	pnum		;	into the spot for the parameter
	jsr	nakit		; Nak the packet
	lda	#false		; Do a false return but don't change state
	rts			; Return
rfilci: lda	oldtry		; Get number of tries for prev packet
	inc	oldtry		; Increment it
	cmp	maxtry		; Have we tried this one too much?
	beq	rfili1		; Not quite yet
	bcs	rfili2		; Yes, go abort this transfer
rfili1: jmp	rfili3		; Continue
rfili2:
rfili5: lda	#'A'		; Move abort code
	sta	state		;	to system state
	lda	#errcrf		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		; Prepare failure return
	rts			;	and go back
rfili3: lda	pnum		; See if pnum=n-1
	clc			;		...
	adc	#1		;		...
	cmp	n		;		...
	beq	rfili4		; If it does, than we are ok
	jmp	rfili5		; Otherwise, abort
rfili4: jsr	spar		; Set up the init parms in the packet buffer
	lda	#'Y'		; Set up the code for Ack
	sta	ptype		; Stuff that parm
	lda	#6		; Packet length for init
	sta	pdlen		; Stuff that also
	jsr	spak		; Send the ack
	lda	#0		; Clear out
	sta	numtry		;	the number of tries for current packet
	lda	#true		; This is ok, return true with current state
	rts			; Return
rfilce: lda	oldtry		; Get number of tries for previous packet
	inc	oldtry		; Up it for next time we have to do this
	cmp	maxtry		; Too many times for this packet?
	beq	rfile1		; Not yet, continue
	bcs	rfile2		; Yes, go abort it
rfile1: jmp	rfile3		;		...
rfile2:
rfile5:	lda	#'A'		; Load abort code
	sta	state		;	into current system state
	lda	#errcrf		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		; Prepare failure return
	rts			;	and return
rfile3:	lda	pnum		; First, see if pnum=n-1
	clc			;		...
	adc	#1		;		...
	cmp	n		;		...
	beq	rfile4		; If so, continue
	jmp	rfile5		; Else, abort it
rfile4: lda	#'Y'		; Load 'ack' code
	sta	ptype		; Stuff that in the packet type
	lda	#0		; This packet will have a packet data length
	sta	pdlen		;	of zero
	jsr	spak		; Send the packet out
	lda	#0		; Zero number of tries for current packet
	sta	numtry		;		...
	lda	#true		; Set up successful return code
	rts			;	and return
rfilcf: lda	pnum		; Does pnum=n?
	cmp	n		;		...
	bne	rfilf1		; If not, abort
	jmp	rfilf2		; Else, we can continue
rfilf1:	lda	#'A'		; Load the abort code
	sta	state		;	and stuff it as current system state
	lda	#errcrf		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		; Prepare failure return
	rts			;	and go back
rfilf2: jsr	getfil		; Get the filename we are to use
	lda	#fncwrt		; Tell the open routine we want to write
	jsr	openf		; Open up the file
	jsr	logrcv		; tell user what we're receiving
	lda	#'Y'		; Stuff code for 'ack'
	sta	ptype		; Into packet type parm
	lda	#0		; Stuff a zero in as the packet data length
	sta	pdlen		;		...
	jsr	spak		; Ack the packet
	lda	numtry		; Move current tries to previous tries
	sta	oldtry		;		...
	lda	#0		; Clear the
	sta	numtry		; Number of tries for current packet
	jsr	incn		; Increment the packet sequence number once
	lda	#'D'		; Advance the system state to 'receive-data'
	sta	state		;		...
	lda	#true		; Set up success return
	rts			;	and go back
rfilcb: lda	pnum		; Does pnum=n?
	cmp	n		;		...
	bne	rfilb1		; If not, abort the transfer process
	jmp	rfilb2		; Otherwise, we can continue
rfilb1:	lda	#'A'		; Code for abort
	sta	state		; Stuff that into system state
	lda	#errcrf		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		; Load failure return status
	rts			;	and return
rfilb2: lda	#'Y'		; Set up 'ack' packet type
	sta	ptype		;		...
	lda	#0		; Zero out
	sta	pdlen		;	the packet data length
	jsr	spak		; Send out this packet
	lda	#'C'		; Advance state to 'complete'
	sta	state		;	since we are now done with the transfer
	lda	#true		; Return a true
	rts			;		...
 
rdat:	lda	numtry		; Get number of tries for current packet
	inc	numtry		; Increment it for next time around
	cmp	maxtry		; Have we gone beyond number of tries allowed?
	beq	rdat1		; Not yet, so continue
	bcs	rdat1a		; Yes, we have, so abort
rdat1:  jmp	rdat2		;		...
rdat1a: lda	#'A'		; Code for 'abort' state
	sta	state		; Stuff that in system state
	lda	#errcrd		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		; Set up failure return code
	rts			;	and go back
rdat2:  jsr	rpak		; Go try to receive a packet
	sta	rstat		; Save the return status for later
	lda	ptype		; Get the type of packet we just picked up
	cmp	#'D'		; Was it a data packet?
	bne	rdat2a		; If not, try next type
	jmp	rdatcd		; Handle a data packet
rdat2a: cmp	#'F'		; Is it a file-header packet?
	bne	rdat2b		; Nope, try again
	jmp	rdatcf		; Go handle a file-header packet
rdat2b: cmp	#'Z'		; Is it an eof packet???
	bne	rdat2c		; If not, go check the return status from rpak
	jmp	rdatce		; It is, go handle eof processing
rdat2c: lda	rstat		; Fetch the return status
	cmp	#false		; Was it a failure return?
	beq	rdat2d		; If it was, Nak it
	lda	#errcrd		; Fetch the error index
	sta	errcod		;	and store it as the error code
	jsr	prcerp		; Check for error packet and process it
	lda	#'A'		; Give up the whole transfer
	sta	state		; Set system state to 'false'
	lda	#false		; Set up a failure return
	rts			;	and go back
rdat2d: lda	n		; Get the expected packet number
	sta	pnum		; Stuff that parameter for Nak routine
	jsr	nakit		; Send a Nak packet
	lda	#false		; Give failure return
	rts			; Go back
 
rdatcd: lda	pnum		; Is pnum the right sequence number?
	cmp	n		;		...
	bne	rdatd1		; If not, try another approach
	jmp	rdatd7		; Otherwise, everything is fine
rdatd1: lda	oldtry		; Get number of tries for previous packet
	inc	oldtry		; Increment it for next time we need it
	cmp	maxtry		; Have we exceeded that limit?
	beq	rdatd2		; Not just yet, continue
	bcs	rdatd3		; Yes, go abort the whole thing
rdatd2: jmp	rdatd4		; Just continue working on the thing
rdatd3:
rdatd6:	lda	#'A'		; Load 'abort' code into the
	sta	state		;	current system state
	lda	#errcrd		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		; Make this a failure return
	rts			; Return
rdatd4: lda	pnum		; Is pnum=n-1... Is the received packet
	clc			;	the one previous to the currently
	adc	#1		;	expected packet?
	cmp	n		;		...
	beq	rdatd5		; Yes, continue transfer
	jmp	rdatd6		; Nope, abort the whole thing
rdatd5: jsr	spar		; Go set up init data
	lda	#'Y'		; Make it look like an ack to a send-init
	sta	ptype		;		...
	lda	#6		;		...
	sta	pdlen		;		...
	jsr	spak		; Go send the ack
	lda	#0		; Clear the
	sta	numtry		;	number of tries for current packet
	lda	#true		;		...
	rts			; Return (successful!)
rdatd7: 
	jsr	closers		; [jrd] Close port while doing disk things
				;  spak or rpak will reopen it
	jsr	bufemp		; Go empty the packet buffer
;
; [jrd]	if losing, send an abort
;
	cmp	#true		; [jrd] we win?
	bne	rdatd8		; [jrd] nope, go abort xfer
;
	lda	#'Y'		; Set up an ack packet
	sta	ptype		;		...
	lda	n		;		...
	sta	pnum		;		...
	lda	#0		; Don't forget, there is no data
	sta	pdlen		;		...
	jsr	spak		; Send it!
	lda	numtry		; Move tries for current packet count to
	sta	oldtry		;	tries for previous packet count
	lda	#0		; Zero the
	sta	numtry		;	number of tries for current packet
	jsr	incn		; Increment the packet sequence number once
	lda	#'D'		; Advance the system state to 'receive-data'
	sta	state		;		...
	lda	#true		;		...
	rts			; Return (successful)
;
; all this added by jrd
rdatd8:	lda	#'A'		; set packet type
	sta	ptype
	lda	#0		; set data length
	sta	pdlen
	jsr	spak		; send it
	lda	#'A'		; set
	sta	state		;  rswt machine state
	lda	#0		; Make CLOSEF see there are no errors
	jsr	closef		; We are done with this file, so close it
	lda	#false		; say we lost
	rts			; and return
;
rdatcf: lda	oldtry		; Fetch number of tries for previous packet
	inc	oldtry		; Increment it for when we need it again
	cmp	maxtry		; Have we exceeded maximum tries allowed?
	beq	rdatf1		; Not yet, go on
	bcs	rdatf2		; Yup, we have to abort this thing
rdatf1: jmp	rdatf3		; Just continue the transfer
rdatf2:
rdatf5:	lda	#'A'		; Move 'abort' code to current system state
	sta	state		;		...
	lda	#errcrd		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		;		...
	rts			;	and return false
rdatf3: lda	pnum		; Is this packet the one before the expected
	clc			;	one?
	adc	#1		;		...
	and	#$3F		; [jrd]
	cmp	n		;		...
	beq	rdatf4		; If so, we can still ack it
	jmp	rdatf5		; Otherwise, we should abort the transfer
rdatf4: lda	#'Y'		; Load 'ack' code
	sta	ptype		; Stuff that parameter
	lda	#0		; Use zero as the packet data length
	sta	pdlen		;		...
	jsr	spak		; Send it!
	lda	#0		; Zero the number of tries for current packet
	sta	numtry		;		...
	lda	#true		;		...
	rts			; Return (successful)
 
rdatce: lda	pnum		; Is this the packet we are expecting?
	cmp	n		;		...
	bne	rdate1		; No, we should go abort
	jmp	rdate2		; Yup, go handle it
rdate1:	lda	#'A'		; Load 'abort' code into
	sta	state		;	current system state
	lda	#errcrd		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		;		...
	rts			; Return (failure)
rdate2:
;	lda	#<fcb1		; Get the pointer to the fcb
;	sta	kerfcb		;	and store it where the close routine
;	lda	#>fcb1		;	can find it
;	sta	kerfcb		;		...
	lda	#0		; Make CLOSEF see there are no errors
	jsr	closef		; We are done with this file, so close it
; ?!?!	jsr	incn		; Increment the packet number
	lda	#'Y'		; Get set up for the ack
	sta	ptype		; Stuff the packet type
	lda	n		;	packet number
	sta	pnum		;		...
	lda	#0		;	and packet data length
	sta	pdlen		;	parameters
	jsr	spak		; Go send it!
;
; zzzzz try this here
;
	jsr	incn		; Increment the packet number
	lda	#'F'		; Advance system state to 'file-header'
	sta	state		;	incase more files are coming
	lda	#true		;		...
	rts			; Return (successful)

;
;	stuff for mapping ops over filenames.
;
;	state vars
;
diropen: .byte	0		; if directory in progress
direof:	.byte	0		; if we've run out of files
dirspec: .byte	"                 " ; room for max pn + ATEOL
;

;	dirini:		init the directory stuff.  Expects a pathname 
;			in path.
dirini:
	jsr	dircls		; make sure dir closed first
	lda	#<path		; point at the pathname struct
	sta	pndptr
	lda	#>path
	sta	pndptr+1
	ldx	#<dirspec	; and point at the string
	ldy	#>dirspec
	jsr	pn2str		; convert pathname to string
;
dirrini:			; entry pt for re-init
	lda	#<dirspec	; get string addr
	ldy	#>dirspec
	ldx	#dirchan	; get dir iocb
	jsr	opencdir	; open directory
	cpy	#SUCCES		; did we win?
	bne	dirini9		; nope, go die
	lda	#1		; say we've got it open
	sta	diropen
	lda	#0		; and that it's not yet at eof
	sta	direof
	rts			; done!
dirini9:
	lda	#0		; say it's not open
	sta	diropen
	jsr	closec		; make sure closed again
	rts
;
;	dircls:		Close the dir channel if it's open
;
dircls:
	lda	diropen		; open?
	beq	dircls9		; nope, go home
	ldx	#dirchan	; get the dir iocb
	jsr	closec		; and close it
	lda	#0		; say it's not open any more
	sta	diropen
dircls9:
	rts			; home!
;
;	dirnxt:		Get next entry in the directory into dirpath.
;			Returns carry clear if wins, carry set if lose
;
dirnxt:
	lda	diropen		; won't work if it's not open
	bne	dirnxt0		; We're ok
	jmp	dirnxt9		; Oops! No good
dirnxt0:
	ldx	#dirchan	; get the dir iocb
	jsr	chrin		; get a char
	sta	dirplck		; save the 'locked' flag
	cmp	#space		; good entries start with
	beq	dirnxt1		;  space or star
	cmp	#'*'
	bne	dirnxt8		; end of dir, close up and leave
dirnxt1:
	jsr	chrin		; skip this char
	ldy	#0		; zap length of name fld
	sty	dirpath+pnd_ns	; really should do something general...
	ldy	#8		; length of name fld
	sty	source		; used as temp
dirnxt2:
	jsr	chrin		; get a byte
	cmp	#space		; end of name?
	beq	dirnxt3		; yup, don't push this one
	ldy	dirpath+pnd_ns	; get name comp size
	sta	dirpath+pnd_nt,y	; and shove the byte
	iny			; bump size
	sty	dirpath+pnd_ns	; and put it back
dirnxt3:
	dec	source		; dec counter
	bne	dirnxt2		; not done yet, go round again
	ldy	#0		; zap length of ext fld
	sty	dirpath+pnd_es	; really should do something general...
	ldy	#3		; length of ext fld
	sty	source		; used as temp
dirnxt4:
	jsr	chrin		; get a byte
	cmp	#space		; end of ext?
	beq	dirnxt5		; yup, don't push this one
	ldy	dirpath+pnd_es	; get ext comp size
	sta	dirpath+pnd_et,y	; and shove the byte
	iny			; bump size
	sty	dirpath+pnd_es	; and put it back
dirnxt5:
	dec	source		; dec counter
	bne	dirnxt4		; not done yet, go round again
;
; done with name.  collect the sector count.
;
dirnxt6:
	jsr	chrin		; ignore this one
	jsr	chrin
	sta	dirsect
	jsr	chrin
	sta	dirsect+1
	jsr	chrin
	sta	dirsect+2
dirnxt6a:
	cmp	#ATEOL		; shouldn't be the first time, but be safe
	beq	dirnxt7
	jsr	chrin
	jmp	dirnxt6a
dirnxt7:
	
;
; set bits in name
;
	lda	#pnf_np|pnf_ep
	sta	dirpath+pnd_fl
	clc			; say we've got one
	rts			; and return
dirnxt8:			; copy rest of dir str someplace
				;  (primfn will do)
	sta	primfn		; that's the first byte
	ldy	#1		; that'll be the idx
	sty	strptr		; temp
dirnxt8a:
	jsr	chrin		; get a byte
	ldy	strptr		; get idx back
	sta	primfn,y	; store the byte
	iny			; bump
	sty	strptr		;  and save it
	cmp	#ATEOL		; end of line?
	bne	dirnxt8a	; nope, do some more
	jsr	dircls		; done, close up
dirnxt9:
	sec			; say we lose
	rts			; and return

;
;.SBTTL	Send routine
 
;
;	This routine reads a file from disk and sends packets
;	of data to the remote kermit.
;
;		Input:  Filename returned from Comnd routines
;
;		Output: File is sent over port
;
;		Registers destroyed:	A,X,Y
;
 
send:
;
; old send startup code commented out while debugging wildcard stuff
;	ldx	#mxfnl		; Longest length a filename may be
;	ldy	#0		; No special flags needed
;	lda	#cmifi		; Load opcode for parsing input files
;	jsr	comnd		; Call comnd routine
;	 jmp	kermt6		; Give the 'missing filespec' error
;	sta	kwrk01		; Store length of file parsed
;	stx	kerfrm		; Save the from address (addr[atmbuf])
;	sty	kerfrm+1	;		...
;	lda	#<fcb1		; Save the to address (Fcb1)
;	sta	kerto		;		...
;	lda	#>fcb1		;		...
;	sta	kerto+1		;		...
;	jsr	clrfcb		; Clear the fcb
;	jsr	kercpy		; Copy the string
;	ldy	kwrk01		; Get filename length
;;zzzzz	lda	#nul		; Fetch a null character
;	lda	#ATEOL		; terminate with eol zzz?
;	sta	(kerto),y	; Stuff a null at end-of-buffer
;-----
	lda	#0		; allow default of *.*
	jsr	enterpn		;  and get a pathname
	jsr	prcfm		; confirm it
	jsr	parsefcb	; parse and merge the resultant filespec
	jsr	closers		; make sure comm port's closed
	jsr	bldprm		; [jrd] reformat it
	jsr	dirini
	lda	#0		; zap
	sta	ssfidx		;  the file idx
	jsr	ssfnxt		; [jrd] get the first file
	bcs	send9		; zzz print error if no match
;----
	jsr	openrs		;[27] Reset the RS232 channel
	jsr	sswt		; Perform send-switch routine
	jsr	closers		; [jrd] close comm port
	jsr	closef		; make sure file closed
send9:
	jmp	kermit		; Go back to main routine

;
; util for dealing with getting next file set up, etc.  Leaves
; fcb1 with name.ext in it.  Returns carry set if fails.  Leaves 
; directory stream closed, so as not to confuse OS by opening 
; file while directory's open
;
ssfidx:	.byte	0		; how many files we've done
ssfnxt:
	jsr	closers		; make sure comm's closed
	lda	ssfidx		; get file nbr
	beq	ssfnx0		; first time?  ok, just go ahead
	pha			; save the old idx
	jsr	dirrini		; reinit directory to value from last time
ssfnsk:				; skip the next file entry
	jsr	dirnxt		; get next entry
	bcc	ssfnsk1		; more here...
	pla			; oops! no more, flush dead val
	jmp	ssfnx9		;  on stack and return error
ssfnsk1:
	dec	ssfidx		; dec count of files we've done
	bne	ssfnsk		; still more, keep skipping
	pla			; get original count back
	sta	ssfidx		; save it
ssfnx0:				; done skipping, take the next one
	jsr	dirnxt		; get one
	bcc	ssfnx1		; succeeded, go ahead
ssfnx9:	jsr	dircls		; failed, close up,
	sec			; and return error
	rts
ssfnx1:
	lda	#<dirpath	; point at the pathname struct
	sta	pndptr
	lda	#>dirpath
	sta	pndptr+1
	ldx	#<fcb1		; and point at the string
	ldy	#>fcb1
	jsr	pn2str		; convert pathname to string
	jsr	dircls		; close dir so we can hack file
	inc	ssfidx		; say we've done this one
	clc			; return success code
	rts
;
; 
sswt:	lda	#'S'		; Set up state variable as
	sta	state		;	Send-init
	lda	#0		; Clear
	sta	eodind		;	The End-of-Data indicator
	sta	n		;	Packet number
	sta	numtry		;	Number of tries
	sta	oldtry		;	Old number of tries
	sta	eofinp		;	End of input flag
	sta	errcod		;	Error indicator
	sta	rtot		;	Total received characters
	sta	rtot+1		;		...
	sta	stot		;	Total Sent characters
	sta	stot+1		;		...
	sta	rchr		;	Received characters, current file
	sta	rchr+1		;		...
	sta	schr		;	and Sent characters, current file
	sta	schr+1		;		...
	sta	tpak		;	and the total packet number
	sta	tpak+1		;		...
	lda	#<pdbuf		; Set up the address of the packet buffer
	sta	saddr		;	so that we can clear it out
	lda	#>pdbuf		;		...
	sta	saddr+1		;		...
	lda	#0		; Clear AC
	ldy	#0		; Clear Y
clpbuf: sta	(saddr),y	; Step through buffer, clearing it out
	iny			; Up the index
	cpy	#mxpack-4	; Done?
	bmi	clpbuf		; No, continue
sswt1:  lda	state		; Fetch state of the system
	cmp	#'D'		; Do Send-data?
	bne	sswt2		; No, try next one
	jsr	sdat		; Yes, send a data packet
	jmp	sswt1		; Go to the top of the loop
sswt2:  cmp	#'F'		; Do we want to send-file-header?
	bne	sswt3		; No, continue
	jsr	sfil		; Yes, send a file header packet
	jmp	sswt1		; Return to top of loop
sswt3:  cmp	#'Z'		; Are we due for an Eof packet?
	bne	sswt4		; Nope, try next state
	jsr	seof		; Yes, do it
	jmp	sswt1		; Return to top of loop
sswt4:  cmp	#'S'		; Must we send an init packet
	bne	sswt5		; No, continue
	jsr	sini		; Yes, go do it
	jmp	sswt1		; And continue
sswt5:  cmp	#'B'		; Time to break the connection?
	bne	sswt6		; No, try next state
	jsr	sbrk		; Yes, go send a break packet
	jmp	sswt1		; Continue from top of loop
sswt6:  cmp	#'C'		; Is the entire transfer complete?
	bne	sswt7		; No, something is wrong, go abort
	lda	#true		; Return true
	rts			;		...
sswt7:  lda	errcod		; [jrd] get the error code
	jsr	prerms		; [jrd] print apropos message
	lda	#false		; Return false
	rts			;		...
 
sdat:	lda	numtry		; Fetch the number for tries for current packet
	inc	numtry		; Add one to it
	cmp	maxtry		; Is it more than the maximum allowed?
	beq	sdat1		; No, not yet
	bcs	sdat1a		; If it is, go abort
sdat1:  jmp	sdat1b		; Continue
sdat1a: lda	#'A'		; Load the 'abort' code
	sta	state		; Stuff that in as current state
	lda	#false		; Enter false return code
	rts			;	and return
sdat1b: lda	#'D'		; Packet type will be 'Send-data'
	sta	ptype		;		...
	lda	n		; Get packet sequence number
	sta	pnum		; Store that parameter to Spak
	lda	size		; This is the size of the data in the packet
	sta	pdlen		; Store that where it belongs
	jsr	spak		; Go send the packet
sdat2:  jsr	rpak		; Try to get an ack
	sta	rstat		; First, save the return status
	lda	ptype		; Now get the packet type received
	cmp	#'N'		; Was it a NAK?
	bne	sdat2a		; No, try for an ACK
	jmp	sdatcn		; Go handle the nak case
sdat2a: cmp	#'Y'		; Did we get an ACK?
	bne	sdat2b		; No, try checking the return status
	jmp	sdatca		; Yes, handle the ack
sdat2b: lda	rstat		; Fetch the return status
	cmp	#false		; Failure return?
	beq	sdat2c		; Yes, just return with current state
	jsr	prcerp		; Check for error packet and process it
	lda	#'A'		; Stuff the abort code
	sta	state		;	as the current system state
	lda	#false		; Load failure return code
sdat2c: rts			; Go back
sdatcn: dec	pnum		; Decrement the packet sequence number
	lda	n		; Get the expected packet sequence number
	cmp	pnum		; If n=pnum-1 then this is like an ack
	bne	sdatn1		; No, continue handling the nak
	jmp	sdata2		; Jump to ack bypassing sequence check
sdata1:
sdatn1: lda	#false		; Failure return
	rts			;		...
sdatca: lda	n		; First check packet number
	cmp	pnum		; Did he ack the correct packet?
	bne	sdata1		; No, go give failure return
sdata2: lda	#0		; Zero out number of tries for current packet
	sta	numtry		;		...
	jsr	incn		; Increment the packet sequence number
	jsr	closers		; [jrd] Close port while doing disk things.
				;  spak or rpak will re-open it
	jsr	bufill		; Go fill the packet buffer with data
	sta	size		; Save the data size returned
	lda	eofinp		; Load end-of-file indicator
	cmp	#true		; Was this set by Bufill?
	beq	sdatrz		; If so, return state 'Z' ('Send-eof')
	jmp	sdatrd		; Otherwise, return state 'D' ('Send-data')
sdatrz:	lda	#0		; Clear
	sta	eofinp		;	End of input flag
;	lda	#<fcb1		; Get the pointer to the fcb
;	sta	kerfcb		;	and store it where the close routine
;	lda	#>fcb1		;	can find it
; zzz	sta	kerfcb		;		...
	lda	#0		; Make CLOSEF see there are no errors
	jsr	closef		; We are done with this file, so close it.  Closes rs
	lda	#'Z'		; Load the Eof code
	sta	state		;	and make it the current system state
	lda	#true		; We did succeed, so give a true return
	rts			; Go back
sdatrd: lda	#'D'		; Load the Data code
	sta	state		; Set current system state to that
	lda	#true		; Set up successful return
	rts			;	and go back
 
sfil:
sfil0:	lda	numtry		; Fetch the current number of tries
	inc	numtry		; Up it by one
	cmp	maxtry		; See if we went up to too many
	beq	sfil1		; Not yet
	bcs	sfil1a		; Yes, go abort
sfil1:	jmp	sfil1b		; If we are still ok, take this jump
sfil1a:	lda	#'A'		; Load code for abort
	sta	state		;	and drop that in as the current state
	lda	#false		; Load false for a return code
	rts			;	and return
sfil1b:	ldy	#0		; Clear Y
sfil1c:	lda	fcb1,y		; Get a byte from the filename
	cmp	#0		; Is it a null?
	beq	sfil1d		; No, continue
; zzz	cmp	#$20		; <sp>?
	cmp	#ATEOL		; end of file name in Atari-land?
	beq	sfil1d		;[DD]
	sta	pdbuf,y		; Move the byte to this buffer
	iny			; Up the index once
	jmp	sfil1c		; Loop and do it again
sfil1d:	sty	pdlen		; This is the length of the filename
	lda	#'F'		; Load type ('Send-file')
	sta	ptype		; Stuff that in as the packet type
	lda	n		; Get packet number
	sta	pnum		; Store that in its common area
	jsr	spak		; Go send the packet
sfil2:	jsr	rpak		; Go try to receive an ack
	sta	rstat		; Save the return status
	lda	ptype		; Get the returned packet type
	cmp	#'N'		; Is it a NAK?
	bne	sfil2a		; No, try the next packet type
	jmp	sfilcn		; Handle the case of a nak
sfil2a:	cmp	#'Y'		; Is it, perhaps, an ACK?
	bne	sfil2b		; If not, go to next test
	jmp	sfilca		; Go and handle the ack case
sfil2b:	lda	rstat		; Get the return status
	cmp	#false		; Is it a failure return?
	bne	sfil2c		; No, just go abort the send
	rts			; Return failure with current state
sfil2c:	jsr	prcerp		; Check for error packet and process it
	lda	#'A'		; Set state to 'abort'
	sta	state		; Stuff it in its place
	lda	#false		; Set up a failure return code
	rts			;	and go back
sfilcn:	dec	pnum		; Decrement the receive packet number once
	lda	pnum		; Load it into the AC
	cmp	n		; Compare that with what we are looking for
	bne	sfiln1		; If n=pnum-1 then this is like an ack, do it
	jmp	sfila2		; This is like an ack
sfila1:	
sfiln1:	lda	#false		; Load failure return code
	rts			;	and return
sfilca:	lda	n		; Get the packet number
	cmp	pnum		; Is that the one that was acked?
	bne	sfila1		; They are not equal
sfila2:	lda	#0		; Clear AC
	sta	numtry		; Zero the number of tries for current packet
	jsr	incn		; Up the packet sequence number
;	lda	#<fcb1		; Load the fcb address into the pointer
;	sta	kerfcb		;	for the DOS open routine
;	lda	#>fcb1		;		...
; zzz	sta	kerfcb+1	;		...
	lda	#fncrea		; Open for input
	jsr	openf		; Open the file.  Closes rs
	jsr	logsnd		; tell user what we're sending
	jsr	bufill		; Go get characters from the file
	sta	size		; Save the returned buffer size
	jsr	openrs		; [jrd] now safe to use serial again
	lda	#'D'		; Set state to 'Send-data'
	sta	state		;		...
	lda	#true		; Set up true return code
	rts			;	and return
 
seof:	lda	numtry		; Get the number of attempts for this packet
	inc	numtry		; Now up it once for next time around
	cmp	maxtry		; Are we over the allowed max?
	beq	seof1		; Not quite yet
	bcs	seof1a		; Yes, go abort
seof1:  jmp	seof1b		; Continue sending packet
seof1a: lda	#'A'		; Load 'abort' code
	sta	state		; Make that the state of the system
	lda	#errmrc		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		; Return false
	rts			;		...
seof1b: lda	#'Z'		; Load the packet type 'Z' ('Send-eof')
	sta	ptype		; Save that as a parm to Spak
	lda	n		; Get the packet sequence number
	sta	pnum		; Copy in that parm
	lda	#0		; This is our packet data length (0 for EOF)
	sta	pdlen		; Copy it
	jsr	spak		; Go send out the Eof
seof2:  jsr	rpak		; Try to receive an ack for it
	sta	rstat		; Save the return status
	lda	ptype		; Get the received packet type
	cmp	#'N'		; Was it a nak?
	bne	seof2a		; If not, try the next packet type
	jmp	seofcn		; Go take care of case nak
seof2a: cmp	#'Y'		; Was it an ack
	bne	seof2b		; If it wasn't that, try return status
	jmp	seofca		; Take care of the ack
seof2b: lda	rstat		; Fetch the return status
	cmp	#false		; Was it a failure?
	beq	seof2c		; Yes, just fail return with current state
	jsr	prcerp		; Check for error packet and process it
	lda	#'A'		; No, abort the whole thing
	sta	state		; Set the state to that
	lda	#false		; Get false return status
seof2c: rts			; Return
seofcn: dec	pnum		; Decrement the received packet sequence number
	lda	n		; Get the expected sequence number
	cmp	pnum		; If it's the same as pnum-1, it is like an ack
	bne	seofn1		; It isn't, continue handling the nak
	jmp	seofa2		; Switch to an ack but bypass sequence check
seofa1:
seofn1: lda	#false		; Load failure return status
	rts			;	and return
seofca: lda	n		; Check sequence number expected against
	cmp	pnum		;	the number we got.
	bne	seofa1		; If not identical, fail and return curr. state
seofa2: lda	#0		; Clear the number of tries for current packet
	sta	numtry		;		...
	jsr	incn		; Up the packet sequence number
	jsr	getnfl		; Call the routine to get the next file
	cmp	#eof		; If it didn't find any more
	beq	seofrb		;	then return state 'B' ('Send-Eot')
	jmp	seofrf		; Otherwise, return 'F' ('Send-file')
seofrb: lda	#'B'		; Load Eot state code
	sta	state		; Store that as the current state
	lda	#true		; Give a success on the return
	rts			;		...
seofrf: lda	#'F'		; Load File-header state code
	sta	state		; Make that the current system state
	lda	#true		; Make success the return status
	rts			;	and return
 
sini:	lda	#<pdbuf		; Load the pointer to the
	sta	kerbf1		;	packet buffer into its
	lda	#>pdbuf		;	place on page zero
	sta	kerbf1+1	;		...
	jsr	spar		; Go fill in the send init parms
	lda	numtry		; If numtry > maxtry
	cmp	maxtry		;		...
	beq	sini1		;		...
	bcs	sini1a		;	then we are in bad shape, go fail
sini1:  jmp	sini1b		; Otherwise, we just continue
sini1a:	lda	#'A'		; Set state to 'abort'
	sta	state		;		...
	lda	#errmrc		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#0		; Set return status (AC) to fail
	rts			; Return
sini1b: inc	numtry		; Increment the number of tries for this packet
	lda	#'S'		; Packet type is 'Send-init'
	sta	ptype		; Store that
;	lda	ebqmod		; Do we want 8-bit quoting?
;	cmp	#on		;		...
;	beq	sini1c		; If so, data length is 7
;	lda	#$06		; Else it is 6
;	jmp	sini1d		;		...
sini1c: lda	#7		; The length of data in a send-init is always 7
sini1d: sta	pdlen		; Store that parameter
	lda	n		; Get the packet number
	sta	pnum		; Store that in its common area
	jsr	flshin		;[25] Flush input buffer
	jsr	spak		; Call the routine to ship the packet out
	jsr	rpak		; Now go try to receive a packet
	sta	rstat		; Hold the return status from that last routine
sinics: lda	ptype		; Case statement, get the packet type
	cmp	#'Y'		; Was it an ACK?
	bne	sinic1		; If not, try next type
	jmp	sinicy		; Go handle the ack
sinic1: cmp	#'N'		; Was it a NAK?
	bne	sinic2		; If not, try next condition
	jmp	sinicn		; Handle a nak
sinic2: lda	rstat		; Fetch the return status
	cmp	#false		; Was this, perhaps false?
	bne	sinic3		; Nope, do the 'otherwise' stuff
	jmp	sinicf		; Just go and return
sinic3:	jsr	prcerp		; Check for error packet and process it
	lda	#'A'		; Set state to 'abort'
	sta	state		;		...
sinicn:
sinicf: rts			; Return
 
sinicy: ldy	#0		; Clear Y
	lda	n		; Get packet number
	cmp	pnum		; Was the ack for that packet number?
	beq	siniy1		; Yes, continue
	lda	#false		; No, set false return status
	rts			;	and go back
siniy1: jsr	rpar		; Get parms from the ack packet
	lda	sebq		; Check if other Kermit agrees to 8-bit quoting
;	cmp	#'Y'		;		...
;	beq	siniy2		; Yes!
;	lda	#off		; Shut it off
;	sta	ebqmod		;		...
	cmp	#'N'		;[30]
	bne	siniy3		;[30] Yes! Leave it alone
	lda	#off		;[30] No .. Shut it off
	sta	ebqmod		;[30]		...
siniy2:
siniy3: lda	#'F'		; Load code for 'Send-file' into AC
	sta	state		; Make that the new state
	lda	#0		; Clear AC
	sta	numtry		; Reset numtry to 0 for next send
	jsr	incn		; Up the packet sequence number
	lda	#true		; Return true
	rts
 
sbrk:	lda	numtry		; Get the number of tries for this packet
	inc	numtry		; Incrment it for next time
	cmp	maxtry		; Have we exceeded the maximum
	beq	sbrk1		; Not yet
	bcs	sbrk1a		; Yes, go abort the whole thing
sbrk1:  jmp	sbrk1b		; Continue send
sbrk1a:	lda	#'A'		; Load 'abort' code
	sta	state		; Make that the system state
	lda	#errmrc		; Fetch the error index
	sta	errcod		;	and store it as the error code
	lda	#false		; Load the failure return status
	rts			;	and return
sbrk1b: lda	#'B'		; We are sending an Eot packet
	sta	ptype		; Store that as the packet type
	lda	n		; Get the current sequence number
	sta	pnum		; Copy in that parameter
	lda	#0		; The packet data length will be 0
	sta	pdlen		; Copy that in
	jsr	spak		; Go send the packet
sbrk2:  jsr	rpak		; Try to get an ack
	sta	rstat		; First, save the return status
	lda	ptype		; Get the packet type received
	cmp	#'N'		; Was it a NAK?
	bne	sbrk2a		; If not, try for the ack
	jmp	sbrkcn		; Go handle the nak case
sbrk2a: cmp	#'Y'		; An ACK?
	bne	sbrk2b		; If not, look at the return status
	jmp	sbrkca		; Go handle the case of an ack
sbrk2b: lda	rstat		; Fetch the return status from Rpak
	cmp	#false		; Was it a failure?
	beq	sbrk2c		; Yes, just return with current state
	jsr	prcerp		; Check for error packet and process it
	lda	#'A'		; No, set up the 'abort' code
	sta	state		;	as the system state
	lda	#false		;	load the false return status
sbrk2c: rts			;	and return
sbrkcn: dec	pnum		; Decrement the received packet number once
	lda	n		; Get the expected sequence number
	cmp	pnum		; If =pnum-1 then this nak is like an ack
	bne	sbrkn1		; No, this was no the case
	jmp	sbrka2		; Yes! Go do the ack, but skip sequence check
sbrka1:
sbrkn1: lda	#false		; Load failure return code
	rts			;	and go back
sbrkca: lda	n		; Get the expected packet sequence number
	cmp	pnum		; Did we get what we expected?
	bne	sbrka1		; No, return failure with current state
sbrka2: lda	#0		; Yes, clear number of tries for this packet
	sta	numtry		;		...
	jsr	incn		; Up the packet sequence number
	lda	#'C'		; The transfer is now complete, reflect this
	sta	state		;	in the system state
	lda	#true		; Return success!
	rts
 
;ctrl-l.SBTTL	Setcom routine
 
;
;	This routine sets Kermit-65 parameters.
;
;		Input:  Parameters from command line
;
;		Output: NONE
;
;		Registers destroyed:	A,X,Y
;
 
setcom: lda	#<setcmd	; Load the address of the keyword table
	sta	cminf1		;
	lda	#>setcmd	;
	sta	cminf1+1	;
	ldy	#0		; No special flags needed
	lda	#cmkey		; Comnd code for parse keyword
	jsr	comnd		; Go get it
	 jmp	kermt2		; Give an error
;---
;	lda	#<setcmb	; Get the address of jump table
;	sta	jtaddr		;
;	lda	#>setcmb	;
;	sta	jtaddr+1	;
;	txa			; Offset to AC
;	jmp	jmpind		;[DD] Jump
;---
	stx	jtaddr		; x,y has vector
	sty	jtaddr+1	; set it
	jmp	(jtaddr)	; and go there
;
;setcmb: jmp	stesc		; Set escape character
;	jmp	stibm		; Set ibm-mode switch
;	jmp	stle		; Set local-echo switch
;	jmp	strc		; Set receive parameters
;	jmp	stsn		; Set send parameters
;	jmp	stvt		; Set vt52-emulation switch
;	jmp	stfw		; Set file-warning switch
;	jmp	steb		; Set Eight-bit quoting character
;	jmp	stdb		; Set debugging switch
;	jmp	stmod		; Set file-type mode
;	jmp	stfbs		; Set the file-byte-size for transfer
;	jmp	stccr		;[DD] Set rs232 registers 
;	jmp	stpari		; Set the parity for communication
;	jmp	stbaud		;[17] Set the baud rate for communication
;	jmp	stwrd		;[17] Set the word length for communication
;	jmp	stflow		;[24] Set flow control for communication
;	jmp	stscre		;[37] Set the screen size
;	jmp	stdef		; [jrd] set default disk

stesc:  ldx	#$10		; Base should be hex
	ldy	#0		; No special flags needed
	lda	#cmnum		; Parse for integer
	jsr	comnd		; Go!
	 jmp	kermt4		; Number is bad
	stx	ksavex		; Hold the number across the next call
	sty	ksavey		;		...
	lda	#cmcfm		; Parse for confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Not confirmed
	lda	ksavey		; If this isn't zero
	cmp	#0		;	it's not an ASCII character
	beq	stesc1		; It is, continue
	jmp	kermt4		; Bad number, tell them
stesc1:	lda	ksavex		; Get L.O. byte
	cmp	#$7F		; It shouldn't be bigger than this
	bmi	stesc2		; If it's less, it is ok
	jmp	kermt4		; Tell the user it is bad
stesc2: sta	escp		; Stuff it
	jmp	kermit
 
stibm:  jsr	prson		; Try parsing an 'on' or 'off'
	 jmp	kermt2		; Bad keyword
	stx	ibmmod		; Store value in the mode switch location
	stx	lecho		; Also set local echo accordingly
	ldy	#nparit		; Get ready to set the parity parameter
	lda	#fbebit		;[17] Get ready to set the word-size parameter
	cpx	#on		; Setting ibm mode on?
	bne	stibm1		; Nope so set parity none/word-size eight-bit
	ldy	#mparit		; Set mark parity
	lda	#fbsbit		;[17] Set up for seven bit word size
	ldx	#off		;[38] Turn off flow-control
	stx	flowmo		;[38]		...
stibm1:	sty	parity		; Store the value
	sta	wrdsiz		;[17]		...
	lda	#cmcfm		;[17] Parse for confirm
	jsr	comnd		;[17] Do it
	 jmp	kermt3		;[17] Not confirmed, tell the user that
	jsr	dopari		;[17] Really set the parity
	jsr	dowrd		;[17] Really set the word size
	jmp	kermit		;
 
stle:	jsr	prson		; Try parsing an 'on' or 'off'
	 jmp	kermt2		; Bad keyword
	stx	lecho		; Store value in the mode switch location
	lda	#cmcfm		; Parse for confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Not confirmed, tell the user that
	jmp	kermit
 
strc:	lda	#0		; Set srind for receive parms
	sta	srind		;		...
	lda	#<stscmd	; Load the address of the keyword table
	sta	cminf1		; Save it for the keyword routine
	lda	#>stscmd	;
	sta	cminf1+1	;
	ldy	#0		; No special flags needed
	lda	#cmkey		; Comnd code for parse keyword
	jsr	comnd		; Go get it
	 jmp	kermt2		; Give an error
	lda	#<stcct		; Get addr. of jump table
	sta	jtaddr		;
	lda	#>stcct		;		...
	sta	jtaddr+1	;		...
	txa			; Offset to AC
	jmp	jmpind  	;[DD] Jump
 
stsn:	lda	#1		; Set srind for send parms
	sta	srind		;		...
	lda	#<stscmd	; Load the address of the keyword table
	sta	cminf1		; Save it for the keyword routine
	lda	#>stscmd	;		...
	sta	cminf1+1	;		...
	ldy	#0		; No special flags needed
	lda	#cmkey		; Comnd code for parse keyword
	jsr	comnd		; Go get it
	 jmp	kermt2		; Give an error
	lda	#<stcct		; Get addr. of jump table
	sta	jtaddr		;
	lda	#>stcct		;
	sta	jtaddr+1	;
	txa			; offset to AC
	jmp	jmpind		;[DD] Jump
 
stcct:  jmp	stpdc		; Set send/rec padding character
	jmp	stpad		; Set amount of padding on send/rec
	jmp	stebq		; Set send/rec eight-bit-quoting character	
	jmp	steol		; Set send/rec end-of-line
	jmp	stpl		; Set send/rec packet length
	jmp	stqc		; Set send/rec quote character
	jmp	sttim		; Set send/rec timeout
 
stvt:	lda	#<termemu	; parse for terminal emulation type
	sta	cminf1
	lda	#>termemu
	sta	cminf1+1
	ldy	#0		; no special flags needed
	lda	#cmkey		; parse for a keyword
	jsr	comnd		; do it
	 jmp	kermt2		; go tell the user about the error
	stx	vtmod		; Store value in the mode switch location
	lda	#cmcfm		; Parse for confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Not confirmed, tell the user that
	jmp	kermit
 
stfw:	jsr	prson		; Try parsing an 'on' or 'off'
	 jmp	kermt2		; Bad keyword
	stx	filwar		; Store value in the mode switch location
	lda	#cmcfm		; Parse for confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Not confirmed, tell the user that
	jmp	kermit
 
steb:	jsr	prson		; Try parsing an 'on' or 'off'
	 jmp	kermt2		; Bad keyword
	stx	ebqmod		; Store value in the mode switch location
	lda	#cmcfm		; Parse for confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Not confirmed, tell the user that
	jmp	kermit
 
stdb:	ldx	#<debkey	;  Load the address of the keyword table
	ldy	#>debkey
	stx	cminf1		;  Save it for the keyword routine
	sty	cminf1+1
	ldy	#0		; No special flags needed
	lda	#cmkey		; Comnd code for parse keyword
	jsr	comnd		; Go get it
	 jmp	kermt2		; Give an error
	stx	debug		; Stuff returned value into debug switch
	lda	#cmcfm		; Parse for a confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Not confirmed, tell the user that
	jmp	kermit
 
 
stebq:  ldx	#$10		; Base for ASCII value
	ldy	#0		; No special flags needed
	lda	#cmnum		; Code for integer number
	jsr	comnd		; Go do it
	 jmp	kermt4		; The number was bad
	tya			; If this isn't zero
	cmp	#0		;	it's not an ASCII character
	beq	steb1		; It is, continue
	jmp	kermt4		; Bad number, tell them
steb1:	txa			; Get L.O. byte
	cmp	#$7F		; It shouldn't be bigger than this
	bmi	steb2		; If it's less, it is ok
	jmp	kermt4		; Tell the user it is bad
steb2:  cmp	#$21		; First check the character range
	bmi	steb4		; Not in range
	cmp	#$3f		;		...
	bmi	steb3		; Inrange
	cmp	#$60		;		...
	bmi	steb4		; Not in range
steb3:  ldx	srind		; Get index for receive or send parms
	sta	ebq,x		; Stuff it
	lda	#cmcfm		; Parse for confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Not confirmed, tell the user that
	jmp	kermit		;
steb4:  ldx	#<ermes5	; Get error message
	ldy	#>ermes5	;		...
	jsr	prstr		; Print the error
	jsr	prcfm		; Go parse and print a confirm
	jmp	kermit		; Go back
 
steol:  ldx	#$10		; Base for ASCII value
	ldy	#0		; No special flags needed
	lda	#cmnum		; Code for integer number
	jsr	comnd		; Go do it
	 jmp	kermt4		; The number was bad
	tya			; If this isn't zero
	cmp	#0		;	it's not an ASCII character
	beq	steo1		; It is, continue
	jmp	kermt4		; Bad number, tell them
steo1:	txa			; Get L.O. byte
	cmp	#$7F		; It shouldn't be bigger than this
	bmi	steo2		; If it's less, it is ok
	jmp	kermt4		; Tell the user it is bad
steo2:  ldx	srind		; Fetch index for receive or send parms
	sta	eol,x		; Stuff it
	jsr	prcfm		; Go parse and print a confirm
	jmp	kermit		; Go back
 
stpad:
;	ldx	#$10		; Base for ASCII value
	ldx	#10		; decimal, please
	ldy	#0		; No special flags needed
	lda	#cmnum		; Code for integer number
	jsr	comnd		; Go do it
	 jmp	kermt4		; The number was bad
	tya			; If this isn't zero
	cmp	#0		;	it's not an ASCII character
	beq	stpd1		; It is, continue
	jmp	kermt4		; Bad number, tell them
stpd1:	txa			; Get L.O. byte
	cmp	#$7F		; It shouldn't be bigger than this
	bmi	stpd2		; If it's less, it is ok
	jmp	kermt4		; Tell the user it is bad
stpd2:  ldx	srind		; Get index (receive or send)
	sta	pad,x		; Stuff it
	jsr	prcfm		; Go parse and print a confirm
	jmp	kermit		; Go back
 
stpdc:  ldx	#$10		; Base for ASCII value
	ldy	#0		; No special flags needed
	lda	#cmnum		; Code for integer number
	jsr	comnd		; Go do it
	 jmp	kermt4		; The number was bad
	tya			; If this isn't zero
	cmp	#0		;	it's not an ASCII character
	beq	stpc1		; It is, continue
	jmp	kermt4		; Bad number, tell them
stpc1:	txa			; Get L.O. byte
	cmp	#$7F		; It shouldn't be bigger than this
	bmi	stpc2		; If it's less, it is ok
	jmp	kermt4		; Tell the user it is bad
stpc2:  ldx	srind		; Get index for parms
	sta	padch,x		; Stuff it
	jsr	prcfm		; Go parse and print a confirm
	jmp	kermit		; Go back
 
stpl:
;	ldx	#$10		; Base for ASCII value
	ldx	#10		; decimal, please
	ldy	#0		; No special flags needed
	lda	#cmnum		; Code for integer number
	jsr	comnd		; Go do it
	 jmp	kermt4		; The number was bad
	tya			; If this isn't zero
	cmp	#0		;	it's not an ASCII character
	beq	stpl1		; It is, continue
	jmp	kermt4		; Bad number, tell them
stpl1:	txa			; Get L.O. byte
	cmp	#mxpack		; It shouldn't be bigger than this
	bmi	stpl2		; If it's less, it is ok
	jmp	kermt4		; Tell the user it is bad
stpl2:  ldx	srind		; Get index
	sta	psiz,x		; Stuff it
	jsr	prcfm		; Go parse and print a confirm
	jmp	kermit		; Go back
 
stqc:	ldx	#$10		; Base for ASCII value
	ldy	#0		; No special flags needed
	lda	#cmnum		; Code for integer number
	jsr	comnd		; Go do it
	 jmp	kermt4		; The number was bad
	tya			; If this isn't zero
	cmp	#0		;	it's not an ASCII character
	beq	stqc1		; It is, continue
	jmp	kermt4		; Bad number, tell them
stqc1:	txa			; Get L.O. byte
	cmp	#$7F		; It shouldn't be bigger than this
	bmi	stqc2		; If it's less, it is ok
	jmp	kermt4		; Tell the user it is bad
stqc2:  ldx	srind		; Fetch index for receive or send parms
	sta	quote,x		; Stuff it
	jsr	prcfm		; Go parse and print a confirm
	jmp	kermit		; Go back
 
sttim:
;	ldx	#$10		; Base for ASCII value
	ldx	#10		; decimal, please
	ldy	#0		; No special flags needed
	lda	#cmnum		; Code for integer number
	jsr	comnd		; Go do it
	 jmp	kermt4		; The number was bad
	tya			; If this isn't zero
	cmp	#0		;	it's not an ASCII character
	beq	sttm1		; It is, continue
	jmp	kermt4		; Bad number, tell them
sttm1:	txa			; Get L.O. byte
	cmp	#$7F		; It shouldn't be bigger than this
	bmi	sttm2		; If it's less, it is ok
	jmp	kermt4		; Tell the user it is bad
sttm2:  ldx	srind		; Fetch index for receive or send parms
	sta	time,x		; Stuff it
	jsr	prcfm		; Go parse and print a confirm
	jmp	kermit		; Go back
 
stmod:	lda	#<ftcmd		; Load the address of the keyword table
	sta	cminf1		;
	lda	#>ftcmd		;
	sta	cminf1+1	;
	lda	#<ftcdef	; Load default address
	sta	cmdptr		;		...
	lda	#>ftcdef	;		...
	sta	cmdptr+1	;		...
	ldy	#cmfdff		; Tell Comnd there is a default
	lda	#cmkey		; Comnd code for parse keyword
	jsr	comnd		; Go get it
	 jmp	kermt2		; Give an error
	stx	filmod		; Save the file-type mode
	lda	#cmcfm		; Parse for a confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Not confirmed, tell the user that
	jmp	kermit
 
;stfbs:	lda	#<fbskey	; Load the address of the keyword table
;	sta	cminf1		;
;	lda	#>fbskey	;
;	sta	cminf1+1	;
;	ldy	#0		; No special flags needed
;	lda	#cmkey		; Comnd code for parse keyword
;	jsr	comnd		; Go get it
;	 jmp	kermt2		; Give an error
;	stx	fbsize		; Stuff the returned value into file-byte-size
;	lda	#cmcfm		; Parse for a confirm
;	jsr	comnd		; Do it
;	 jmp	kermt3		; Not confirmed, tell the user that
;	jmp	kermit
 
 
stccr:  ldx	#$10		;[DD] Base should be hex
	ldy	#0		; No special flags needed
	lda	#cmnum		;[DD] Parse for integer
	jsr	comnd		;[DD] Go do it
	 jmp	kermt4		;[DD] The number was bad
stccr1:	stx	ksavex		; Store it while we confirm
	sty	ksavey		;		...
	lda	#cmcfm		; Set up to parse confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Wasn't properly confirmed
	lda	ksavex		; Fetch back L.O. byte
	sta	x36ax1		;[DD][EL] To rs232 reg 0
	lda	ksavey		;[18] Fetch back H.O. byte
	sta	x38ax1		;[DD] To rs232 reg 1
	jmp	kermit		;[DD] 
 
stpari:	lda	#<parkey	; Load the address of the keyword table
	sta	cminf1		; Save it for the keyword routine
	lda	#>parkey	;		...
	sta	cminf1+1	;		...
	ldy	#0		; No special flags needed
	lda	#cmkey		; Comnd code for parse keyword
	jsr	comnd		; Go get it
	 jmp	kermt2		; Give an error
	stx	parity		; Stuff returned value into parity
	lda	#cmcfm		; Parse for a confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Not confirmed, tell the user that
	jsr	dopari		;[17] Now really set the parity
	jmp	kermit		;
 
dopari:	lda	x38ax1		;[17] Get the command register
	and	#$C0		;[17] mask out in+out parity
	sta	x38ax1		;[17] Store it back
	ldx	parity		;[17] Get the index
	lda	parval,x	;[17]	and the parity value from the table
	ora	x38ax1		;[17]
	sta	x38ax1		;[17] Store it back
	rts			;[17] Return
 
stbaud:	lda	#<bdkey		;[17] Load the address of the keyword table
	sta	cminf1		;[17] Save it for the keyword routine
	lda	#>bdkey		;[17]		...
	sta	cminf1+1	;[17]		...
	ldy	#0		;[17] No special flags needed
	lda	#cmkey		;[17] Parse for a keyword
	jsr	comnd		;[17] Do it
	 jmp	kermt2		;[17] Give an error
	stx	baud		;[17] Stuff the returned value
	lda	#cmcfm		;[17] Set up for a confirm
	jsr	comnd		;[17] Do it
	 jmp	kermt3		;[17] Not confirmed
	jsr	dobad		;[17] Really set the baud rate
	jmp	kermit		;[17] 
 
dobad:	lda	x36ax1		;[17] Get the control register
	and	#$F0		;[17] Clear it
	sta	x36ax1		;[17] Store it back
	ldx	baud		;[17] Get the baud rate back
	lda	bdval,x		;[17] Get the value from the table
	ora	x36ax1		;[17] Set the baud rate
	sta	x36ax1		;[17]  and store it
	rts			;[17]
 
stwrd:	lda	#<fbskey	;[17] Load the address of the keyword table
	sta	cminf1		;[17] Save it for the keyword routine
	lda	#>fbskey	;[17]		...
	sta	cminf1+1	;[17]		...
	ldy	#0		;[17] No special flags needed
	lda	#cmkey		;[17] Comnd code for parse keyword
	jsr	comnd		;[17] Go get it
	 jmp	kermt2		;[17] Give an error
	stx	wrdsiz		;[17] Stuff the returned value into wrd len
	lda	#cmcfm		;[17] Parse for a confirm
	jsr	comnd		;[17] Do it
	 jmp	kermt3		;[17] Not confirmed, tell the user that
	jsr	dowrd		;[17] Really set the word size
	jmp	kermit		;[17]		...
 
dowrd:	lda	x36ax1		;[17] Get the control register
	and	#$8F		;[17] mask for word size
	sta	x36ax1		;[17] Store it back
	lda	wrdsiz		;[17] Get the word size
	cmp	#fbsbit		;[17] Is it seven-bit ?
	bne	dwrd1		;[17] No, we have the value for eight-bit
				; NB, this depends on fbebit = 0
	lda	#$10		;[17] Yes, get value for seven-bit word size
dwrd1:	ora	x36ax1		;[17] Set it
	sta	x36ax1		;[17] Store it
	rts			;[17] Return
 
stflow: jsr	prson		;[24] Try parsing an 'on' or 'off'
	 jmp	kermt2		;[24] Bad keyword
	stx	flowmo		;[24] Store it
	lda	#cmcfm		;[24] Parse for confirm
	jsr	comnd		;[24] Do it
	 jmp	kermt3		;[24] Not confirmed, tell the user that
	jmp	kermit		;[24]
 
stscre:	lda	#<scrkey	;[37] Get the address of the screen mode table
	sta	cminf1		;[37]		...
	lda	#>scrkey	;[37]		...
	sta	cminf1+1	;[37]		...
	ldy	#0		;[37] No special flags needed
	lda	#cmkey		;[37] Comnd code for parse keyword
	jsr	comnd		;[37] Go get it
	 jmp	kermt2		;[37] Give an error
	stx	kwrk01		;[37] Stuff the returned value into kwrk01
	lda	#cmcfm		;[37] Parse for a confirm
	jsr	comnd		;[37] Do it
	 jmp	kermt3		;[37] Not confirmed, tell the user that
	lda	kwrk01		;[37] Are we switching to 80 columns?
;
;get:
;	pha			; save the id of the screen driver we want
;	jsr	scrext		; exit the old screen driver
;	pla
; [jrd] no need to test, they're all here
;	pha			; keep the id of the screen driver on the stack
;	jsr	scrtst		; does this screen driver exist?
;	pla			; restore desired screen type
;	bcc	get1
;	lda	#1		; if it does not exist, use 80-columns instead
;get1:
	sta	conscrt		; [jrd] use the slot that connect code uses
;	jsr	scrent		; enter the screen driver
	jmp	kermit		; all done

;
; Set default disk
;
stdef:  ldx	#$10		; Base should be hex
	ldy	#0		; No special flags needed
	lda	#cmnum		; Parse for integer
	jsr	comnd		; Go!
	 jmp	kermt4		; Number is bad
	stx	ksavex		; save it for a bit
	sty	ksavey
	lda	#cmcfm		; Set up to parse confirm
	jsr	comnd		; Do it
	 jmp	kermt3		; Wasn't properly confirmed
	lda	ksavex		; value back in A
	beq	stdefng		; zero's no good
	bmi	stdefng		; neg either
	cmp	#9		; [jrd v3.7] between 1 and 8?
	bcs	stdefng		; nope, lose
	adc	#$30		; make into ascii; carry's clear
	sta	dsknum		; stuff into the relevant place
	jmp	kermit		; done!
stdefng: jmp	kermt4		; error return	

;ctrl-l.SBTTL	Show routine
 
;
;	This routine shows any of the operational parameters that
;	can be altered with the set command.
;
;		Input:  Parameters from command line
;
;		Output: Display parameter values on screen
;
;		Registers destroyed:	A,X,Y
;
 
show:	lda	#<shocmd	; Load address of keyword table
	sta	cminf1		;
	lda	#>shocmd	;
	sta	cminf1+1	;
	lda	#<shodef	; Fetch default address
	sta	cmdptr		;		...
	lda	#>shodef	;		...
	sta	cmdptr+1	;		...
	ldy	#cmfdff		; Indicate that there is a default
	lda	#cmkey		; Comnd code to parse keyword
	jsr	comnd		; Go parse the keyword
	 jmp	kermt2		; Bad keyword, go give an error
;---
;	lda	#<shocmb 	; Get addr. of jump table
;	sta	jtaddr		;
;	lda	#>shocmb	;
;	sta	jtaddr+1	;
;	txa			; Offset to AC
;	jmp	jmpind		;[DD] Jump
;---
	stx	jtaddr		; x,y has vector
	sty	jtaddr+1	; set it
;
;the routines here are set up to be jsr-ed to, so we'll do it that way
;rather than just jumping.  We'll jsr, the routine'll return, and we'll
;continue execution after our jsr
;
	jsr	show1		; jsr to the dispatcher
	jmp	kermit		; done!

show1:	jmp	(jtaddr)	; go there.  He'll return
				; to our caller.
;
;shocmb: jsr	prcfm		; Parse for confirm
;	jsr	shall		; Show all setable parameters
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	shesc		; Show escape character
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	shibm		; Show ibm-mode switch
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	shle		; Show local-echo switch
;	jmp	kermit		; Go to top of main loop
;	nop			; We should not parse for confirm
;	nop			;	since this routine parses for
;	nop			;	a keyword next
;	jsr	shrc		; Show receive parameters
;	jmp	kermit		; Go to top of main loop
;	nop			; We should not parse for confirm
;	nop			;	since this routine parses for
;	nop			;	a keyword next
;	jsr	shsn		; Show send parameters
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	shvt		; Show vt52-emulation mode switch
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	shfw		; Show file-warning switch
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	sheb		; Show eight-bit-quoting switch
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	shdb		; Show debugging mode switch
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	shmod		; Show File mode
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	shfbs		; Show the file-byte-size
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		;[DD] Parse for confirm 
;	jsr	shccr		;[DD] Show rs232 regs.
;	jmp	kermit		;[DD] Go to top of main loop
;	jsr	prcfm		; Parse for confirm
;	jsr	shpari		; Show Parity
;	jmp	kermit		; Go to top of main loop
;	jsr	prcfm		;[17] Parse for a confirm
;	jsr	shbad		;[17] Show baud
;	jmp	kermit		;[17] Go to top of main loop
;	jsr	prcfm		;[17] Parse for a confirm
;	jsr	shwrd		;[17] Show word size
;	jmp	kermit		;[17] Go to top of main loop
;	jsr	prcfm		;[24] Parse for a confirm
;	jsr	shflow		;[24] Show flow-control
;	jmp	kermit		;[24] Go to top of main loop
;
; sho def.  Surely there's a better way to do this?
;
;	jsr	prcfm		;[24] Parse for a confirm
;	jsr	shdef		;[24] show def
;	jmp	kermit		;[24] Go to top of main loop

;
; this is sort of a kludge.  Many of the things this calls
; need to be callable separately; in the latter case they 
; want to get a confirm, in this case, they don't as it's
; done once for all of them.  For those cases, we call the
; regular entry point + 3, skipping the call to prcfm.  Sigh.
;
shall:
	jsr	prcfm		; make sure confirmed
	jsr	shdb+3		; Show debugging mode switch
	jsr	shvt+3		; Show vt52-emulation mode switch
	jsr	shibm+3		; Show ibm-mode switch
	jsr	shle+3		; Show local-echo switch
	jsr	shbad+3		;[17] Show baud rate
	jsr	shpari+3		; Show parity setting
	jsr	shwrd+3		;[17] Show word length
	jsr	shflow+3		;[24] Show flow-control
	jsr	sheb+3		; Show eight-bit-quoting switch
	jsr	shfw+3		; Show file-warning switch
	jsr	shesc+3		; Show the current escape character
	jsr	shmod+3		; Show the file-type mode
;	jsr	shfbs+3		; Show the file-byte-size
	jsr	shdef+3		; [jrd] show default drive
	jsr	shccr+3		;[DD] Show rs232 regs.
;
; these last two don't needed the +3; they're only used here
;
	jsr	shrcal		; Show receive parameters
	jsr	shsnal		; Show send parameters
	rts			; Return
 
shdb:	jsr	prcfm		; make sure confirmed
	ldx	#<shin00	; Get address of message for this item
	ldy	#>shin00
	jsr	prstr		; Print that message
	lda	debug		; Get the switch value
	cmp	#3		; Is it >= 3?
	bmi	shdb1		; If not just get the string and print it
	lda	#0		; This is index for debug mode we want
shdb1:	tax			; Hold this index
	lda	#<kerdms	; Get the address of the debug strings
	sta	kermbs		; And stuff it here for genmad
	lda	#>kerdms	;		...
	sta	kermbs+1	;		...
	lda	#kerdsz		; Get the string length
	pha			; Push that
	txa			; Fetch the index back
	pha			; Push that parm then
	jsr	genmad		;	call genmad
	jsr	prstr		; Print the the string at that address
	jsr	prcrlf		; Print a crelf after it
	rts
 
shvt:	jsr	prcfm		; make sure confirmed
	ldx	#<shin01	; Get address of message for this item
	ldy	#>shin01
	jsr	prstr		; Print that message
	lda	#<kertms	; get address of messages for this item
	sta	kermbs
	lda	#>kertms
	sta	kermbs+1
	lda	#keremu		; length of the messages
	pha
	lda	vtmod		; which message
	pha
	jsr	genmad		; calculate address of selected message
	jsr	prstr		; print selected message
	jsr	prcrlf		; and a carriage return / line feed
	rts			; all done
 
shibm:	jsr	prcfm		; make sure confirmed
	ldx	#<shin02	; Get address of message for this item
	ldy	#>shin02
	jsr	prstr		; Print that message
	lda	ibmmod		; Get the switch value
	jmp	pron		; Go print the 'on' or 'off' string
 
shle:	jsr	prcfm		; make sure confirmed
	ldx	#<shin03	; Get address of message for this item
	ldy	#>shin03
	jsr	prstr		; Print that message
	lda	lecho		; Get the switch value
	jmp	pron		; Go print the 'on' or 'off' string
 
sheb:	jsr	prcfm		; make sure confirmed
	ldx	#<shin04	; Get address of message for this item
	ldy	#>shin04
	jsr	prstr		; Print that message
	lda	ebqmod		; Get the switch value
	jmp	pron		; Go print the 'on' or 'off' string
 
shfw:	jsr	prcfm		; make sure confirmed
	ldx	#<shin05	; Get address of message for this item
	ldy	#>shin05
	jsr	prstr		; Print that message
	lda	filwar		; Get the switch value
	jmp	pron		; Go print the 'on' or 'off' string
 
shesc:	jsr	prcfm		; make sure confirmed
	ldx	#<shin06	; Get address of message
	ldy	#>shin06
	jsr	prstr		; Print message
	lda	escp		; Get the escape character
	jsr	prchr		; Print the special character
	jsr	prcrlf		; Print a crelf
	rts			;	and return
 
shccr:	jsr	prcfm		; make sure confirmed
	ldx	#<shin18	;[DD][EL] Print rs232 registers x36ax1,x38ax1 
	ldy	#>shin18	;[DD]
	jsr	prstr		;[DD]
	lda	x38ax1		;[DD] Print rs232 reg 1
	jsr	prbyte		;[DD]
	lda	x36ax1		;[DD] Print rs232 reg 0
	jsr	prbyte		;[DD]
	jsr	prcrlf		;[DD]	and a crlf
	rts			;[DD]
 
 
shsn:	lda	#1		; Set up index to be used later
	sta	srind
	lda	#<stscmd	; Get the set option table address
	sta	cminf1		;
	lda	#>stscmd	;
	sta	cminf1+1	;
	ldy	#0		; No special flags needed
	lda	#cmkey		; Code for keyword parse
	jsr	comnd		; Try to parse it
	 jmp	kermt2		; Invalid keyword
	stx	kwrk01		; Hold offset into jump table
	jsr	prcfm		; Parse and print a confirm
	lda	#<shcmb  	; Get addr. of jump table
	sta	jtaddr		;
	lda	#>shcmb		;
	sta	jtaddr+1	;
	lda	kwrk01		; Get offset  back
	asl	a		; Double it
	jmp	jmpind  	;[DD] Jump
;
shrc:	lda	#0		; Set up index to be used later
	sta	srind
	lda	#<stscmd	; Get the set option table address
	sta	cminf1		;
	lda	#>stscmd	;
	sta	cminf1+1	;
	ldy	#0		; No special flags needed
	lda	#cmkey		; Code for keyword parse
	jsr	comnd		; Try to parse it
	 jmp	kermt2		; Invalid keyword
	stx	kwrk01		; Hold offset into jump table
	jsr	prcfm		; Parse and print a confirm
	lda	#<shcmb		; Get addr. ofl jump table
	sta	jtaddr		;
	lda	#>shcmb		;
	sta	jtaddr+1	;
	lda	kwrk01		; Get offset  back
	asl	a		; Double it
	jmp	jmpind		;[DD] Jump
 
shcmb:  jsr	shpdc		; Show send/rec padding character
	jmp	kermit		; Go back
	jsr	shpad		; Show amount of padding for send/rec
	jmp	kermit		; Go back
	jsr	shebq		; Show send/rec eight-bit-quoting character
	jmp	kermit		; Go back
	jsr	sheol		; Show send/rec end-of-line character
	jmp	kermit		; Go back
	jsr	shpl		; Show send/rec packet length
	jmp	kermit		; Go back
	jsr	shqc		; Show send/rec quote character
	jmp	kermit		; Go back
	jsr	shtim		; Show send/rec timeout
	jmp	kermit		; Go back
 
shpdc:  ldx	#<shin11	; Get address of 'pad char' string
	ldy	#>shin11
	jsr	prstr		; Print that
	ldx	srind		; Load index so we print correct parm
	lda	padch,x		; If index is 1, this gets spadch
	jsr	prchr		; Print the special character
	jsr	prcrlf		; Print a crelf after it
	rts
shpad:  ldx	#<shin12	; Get address of 'padding amount' string
	ldy	#>shin12
	jsr	prstr		; Print that
	ldx	srind		; Load index so we print correct parm
	lda	pad,x		; If index is 1, this gets spad
;	jsr	prbyte		; Print the amount of padding
	tax			; lo half
	lda	#0
;	jsr	prntad		; print it in decimal, please
;	jsr	prcrlf		; Print a crelf after it
	jsr	prntadnl	; use combined rtn
	rts
shebq:  ldx	#<shin08	; Get address of 'eight-bit-quote' string
	ldy	#>shin08
	jsr	prstr		; Print that
	ldx	srind		; Load index so we print correct parm
	lda	ebq,x		; If index is 1, this gets sebq
	jsr	prchr		; Print the special character
	jsr	prcrlf		; Print a crelf after it
	rts
sheol:  ldx	#<shin09	; Get address of 'end-of-line' string
	ldy	#>shin09
	jsr	prstr		; Print that
	ldx	srind		; Load index so we print correct parm
	lda	eol,x		; If index is 1, this gets seol
	jsr	prchr		; Print the special character
	jsr	prcrlf		; Print a crelf after it
	rts
shpl:	ldx	#<shin10	; Get address of 'packet length' string
	ldy	#>shin10
	jsr	prstr		; Print that
	ldx	srind		; Load index so we print correct parm
	lda	psiz,x		; If index is 1, this gets spsiz
;	jsr	prbyte		; Print the packet length
	tax
	lda	#0
;	jsr	prntad		; print in decimal please
;	jsr	prcrlf		; Print a crelf after it
	jsr	prntadnl	; use combined rtn
	rts			;	and return
shqc:	ldx	#<shin13	; Get address of 'quote-char' string
	ldy	#>shin13
	jsr	prstr		; Print that
	ldx	srind		; Load index so we print correct parm
	lda	quote,x		; If index is 1, this gets squote
	jsr	prchr		; Print the special character
	jsr	prcrlf		; Print a crelf after it
	rts
shtim:  ldx	#<shin14	; Get address of 'timeout' string
	ldy	#>shin14
	jsr	prstr		; Print that
	ldx	srind		; Load index so we print correct parm
	lda	time,x		; If index is 1, this gets stime
;---
;	jsr	prbyte		; Print the hex value
	tax			; get it into x
	lda	#0
;	jsr	prntad
;---
;	jsr	prcrlf		; Print a crelf after it
	jsr	prntadnl	; use combined rtn
	rts
 
shsnal: lda	#1		; Set up index for show parms
	sta	srind		;	and stuff it here
	ldx	#<shin07	; Get address of 'send' string
	ldy	#>shin07	;
	jsr	prstr		; Print it
	jsr	prcrlf		; Print a crelf
	jsr	shpdc		; Show the padding character
	jsr	shpad		; Show amount of padding
	jsr	shebq		; Show eight-bit-quote character
	jsr	sheol		; Show end-of-line character
	jsr	shpl		; Show packet-length
	jsr	shqc		; Show quote character
	jsr	shtim		; Show timeout length
	rts
 
shrcal: lda	#0		; Set up index for show parms
	sta	srind		;	and stuff it here
	ldx	#<shin15	; Get address of 'receive' string
	ldy	#>shin15
	jsr	prstr		; Print it
	jsr	prcrlf		; Print a crelf
	jsr	shpdc		; Show the padding character
	jsr	shpad		; Show amount of padding
	jsr	shebq		; Show eight-bit-quote character
	jsr	sheol		; Show end-of-line character
	jsr	shpl		; Show packet-length
	jsr	shqc		; Show quote character
	jsr	shtim		; Show timeout length
	rts
 
shmod:	jsr	prcfm		; make sure confirmed
	ldx	#<shin16	; Get address of 'timeout' string
	ldy	#>shin16
	jsr	prstr		; Print that
	lda	filmod		; Get the file-type mode
;	cmp	#4		; Is it >= 4?
	cmp	#ftbin+1	; Is it >= 3? [jrd] no Script
	bmi	shmod1		; If not just get the string and print it
	lda	#ftatas		; [jrd] This is the index to the file-type we want
shmod1: tax			; Hold this index
	lda	#<kerftp	; Get the address if the file type strings
	sta	kermbs		;
	lda	#>kerftp	;
	sta	kermbs+1	;
	lda	#kerfts		; Get the string length
	pha			; Push that
	txa			; Fetch the index back
	pha			; Push that parm then
	jsr	genmad		;	call genmad
	jsr	prstr		; Print the the string at that address
	jsr	prcrlf		; Print a crelf after it
	rts
 
;shfbs:	jsr	prcfm		; make sure confirmed
;	ldx	#<shin17	; Get address of 'file-byte-size' string
;	ldy	#>shin17
;	jsr	prstr		; Print that
;	lda	fbsize		; Get the file-type mode
;	beq	shfbse		; It is in eight-bit mode
;	ldx	#<shsbit	; Get address of 'SEVEN-BIT' string
;	ldy	#>shsbit	;
;	jsr	prstr		; Print that
;	jsr	prcrlf		;	then a crelf
;	rts			;	and return
;shfbse: ldx	#<shebit	; Get the address of 'EIGHT-BIT' string
;	ldy	#>shebit	;
;	jsr	prstr		; Print the the string at that address
;	jsr	prcrlf		; Print a crelf after it
;	rts
 
shpari:	jsr	prcfm		; make sure confirmed
	ldx	#<shin20	; Get address of 'parity' string
	ldy	#>shin20	;		...
	jsr	prstr		; Print that
	lda	parity		; Get the parity index
	cmp	#5		; Is it >= 5?
	bmi	shpar1		; If not just get the string and print it
	lda	#0		; This is the index to the parity we want
shpar1:	tax			; Hold this index
	lda	#<kerprs	; Get address of the parity strings
	sta	kermbs		; And stuff it here for genmad
	lda	#>kerprs	;		...
	sta	kermbs+1	;		...
	lda	#kerpsl		; Get the string length
	pha			; Push that
	txa			; Fetch the index back
	pha			; Push that parm then
	jsr	genmad		;	call genmad
	jsr	prstr		; Print the the string at that address
	jsr	prcrlf		; Print a crelf after it
	rts
 
shbad:	jsr	prcfm		; make sure confirmed
	ldx	#<shin19	;[17] Get the address of the 'baud' string
	ldy	#>shin19	;[17] 		...
	jsr	prstr		;[17] Print it
	lda	baud		;[17] Get the baud rate
	cmp	#$0A		;[17] Is it >= 10?
	bmi	shbad1		;[17] No, just print the string
	lda	#bd1200		;[17] Use 1200 baud as default
shbad1:	tax			;[17] Hold the index here
	lda	#<kerbds	;[17] Get the address of
	sta	kermbs		;[17]	the baud rate strings
	lda	#>kerbds	;[17]		...
	sta	kermbs+1	;[17]		...
	lda	#kerbsl		;[17] Get the length of the baud rate strings
	pha			;[17] Push that
	txa			;[17]
	pha			;[17]
	jsr	genmad		;[17]
	jsr	prstr		;[17]
	jsr	prcrlf		;[17]
	rts			;[17]
 
shwrd:	jsr	prcfm		; make sure confirmed
	ldx	#<shin21	;[17] Get the address of the 'wrod-size'
	ldy	#>shin21	;[17]	message
	jsr	prstr		;[17] Print that
	lda	wrdsiz		;[17] Get the word-size
	beq	shwrde		;[17] 
	ldx	#<shsbit	;[17] Get address of 'SEVEN-BIT' string
	ldy	#>shsbit	;[17]		...
	jsr	prstr		;[17] Print that
	jsr	prcrlf		;[17]	then a crelf
	rts			;[17]	and return
shwrde:	ldx	#<shebit	;[17] Get address of 'EIGHT-BIT' string
	ldy	#>shebit	;[17]		...
	jsr	prstr		;[17] Print that
	jsr	prcrlf		;[17]	and a crelf
	rts			;[17]	and return
 
shflow:	jsr	prcfm		; make sure confirmed
	ldx	#<shin22	;[24]
	ldy	#>shin22	;[24]
	jsr	prstr		;[24]
	lda	flowmo		;[24]
	jmp	pron		;[24]
 
;
; Show default disk
;
shdef:	jsr	prcfm		; make sure confirmed
	ldx	#<shin23	;[jrd] default drive string
	ldy	#>shin23	;[jrd]
	jsr	prstr		;[jrd]
	lda	dsknum		;[jrd]
	jsr	cout		;[jrd]
	jsr	prcrlf
	rts
 
;ctrl-l.SBTTL	Status routine
 
;
;	This routine shows the status of the most recent transmission
;	session.
;
;		Input:  NONE
;
;		Output: Status of last transmission is sent to screen
;
;		Registers destroyed:	A,X,Y
;
 
status: jsr	prcfm		; Parse and print a confirm
	jsr	stat01		;[45] Go Give the status
	jmp	kermit		;[45]   and parse for more commands
 
stat01: ldx	#<stin00	; Get address of first line of text
	ldy	#>stin00	;		...
	jsr	prstr		; Print that
	ldx	schr		; Get low order byte of character count
	lda	schr+1		; Get high order byte
;	jsr	prntax		; Print that pair in hex
;	jsr	prntad		; no, print it in decimal
;	jsr	prcrlf		; Add a crelf at the end
	jsr	prntadnl	; use combined rtn
	ldx	#<stin01	; Get address of second line
	ldy	#>stin01	;		....
	jsr	prstr		; Print it
	ldx	rchr		; Get L.O. byte of char count
	lda	rchr+1		; Get H.O. byte
;	jsr	prntax		; Print that count
;	jsr	prntad		; print it in decimal
;	jsr	prcrlf		; Add a crelf at the end
	jsr	prntadnl	; use combined rtn
	ldx	#<stin02	; Get L.O. address of message
	ldy	#>stin02	;
	jsr	prstr		; Print message
	ldx	stot		; Get L.O. byte of count
	lda	stot+1		; Get H.O. byte
;	jsr	prntax		; Print the count
;	jsr	prntad		; print it in decimal
;	jsr	prcrlf		; Add a crelf at the end
	jsr	prntadnl	; use combined rtn
	ldx	#<stin03	; Get address of next status item message
	ldy	#>stin03
	jsr	prstr		; Print it
	ldx	rtot		; Get the proper count (L.O. byte)
	lda	rtot+1		; Get H.O. byte
;	jsr	prntax		; Print the 16-bit count
;	jsr	prntad		; print it in decimal
;	jsr	prcrlf		; Add a crelf at the end
	jsr	prntadnl	; use combined rtn
	jsr	prcrlf		; Add a crelf at the end
	ldx	#<stin04	; Get address of overhead message
	ldy	#>stin04	;
	jsr	prstr		; Print that message
	sec			; Get ready to calculate overhead amount
	lda	stot		; Get total character count and
	sbc	schr		;	subtract off data character count
	tax			; Stuff that here for printing
	lda	stot+1
	sbc	schr+1
;	jsr	prntax		; Print it
;	jsr	prntad		; print it in decimal
;	jsr	prcrlf		; Add a crelf at the end
	jsr	prntadnl	; use combined rtn
	ldx	#<stin05	; Get address of next overhead message
	ldy	#>stin05	;		...
	jsr	prstr		; Print that
	sec			; Get ready to calculate overhead amount
	lda	rtot		; Get total character count and
	sbc	rchr		;	subtract off data character count
	tax			; Stuff that here for printing
	lda	rtot+1		;		...
	sbc	rchr+1		;		...
;	jsr	prntax		; Print the count
;	jsr	prntad		; print it in decimal
;	jsr	prcrlf		; Add a crelf at the end
	jsr	prntadnl	; use combined rtn
	jsr	prcrlf		; Add a crelf at the end
	ldx	#<stin06  	; Get message for 'last error'
	ldy	#>stin06	;		...
	jsr	prstr		; Print the message
	jsr	prcrlf		; Print a crelf before the error message
	lda	#eprflg		; [jrd] ???
	bit	errcod		; Test for 'Error packet received' bit
	bvs	statpe		; Go process an error packet
;
; all this old error stuff commented out til we write the error decoder
; for atari.
;
;	lda	#kerems		; Get the error message size
;	pha			; Push it
;	lda	errcod		; Get the error message offset in table
;	bmi	stat02		; If this is a DOS error, do extra adjusting	
;	pha			; Push that
;	lda	#<erms0a	; Put the base address in kermbs
;	sta	kermbs		;		...
;	lda	#>erms0a	;		...
;	sta	kermbs+1	;		...
;	jmp	statle		; Go print the 'last error' encountered
;stat02:	and	#$7f		; Shut off H.O. bit
;	beq	stat03		; If it is zero, we are done adjusting
;	sec			; Decrement by one for the unused error code
;	sbc	#1		;		...
;stat03:	pha			; Push that parameter
; zzz	lda	#<dskers	; Use 'dskers' as the base address
;	sta	kermbs		;		...
;	lda	#>dskers	;		...
;	sta	kermbs+1	;		...
;statle:
;	jsr	genmad		; Translate code to address of message
;	jsr	prstr		; Print the text of error message
;	jsr	prcrlf		; Add a crelf at the end
;;	jmp	kermit		; Start at the top
	lda	errcod		; [jrd] get the error code
	jsr	prerms		; [jrd] print appropriate msg
	jsr	prcrlf		; [jrd] and a crlf
	rts			;[45] Return to the caller
statpe:	ldx	#<errrkm	; L.O. byte address of remote kermit error
	ldy	#>errrkm	; H.O. byte address...
	jsr	prstr		; Print the text from the error packet
	jsr	prcrlf		; Print an extra crelf
;	jmp	kermit		; Start at the top again
	rts			;[45] Return to the caller

;
;	Given error code in A, return message addr in X,Y.  Preserves A.
;
ermadr:	pha
	and	#$0F		; not more than 15 of them
	asl	A		; mult by 2
	tax			; get it in x
	lda	kerrv+1,x	; get hi byte
	tay			; get hi byte into y
	lda	kerrv,x		; get lo byte
	tax			; into x
	pla
	rts
;
;	Print message given error code in A.  Preserves A.
;
prerms:
	cmp	#0		; No error?
	beq	prerms9		; give up
	pha			; save it
	jsr	ermadr		; get msg addr
	cpy	#0		; no message?
	beq	prerms8		; print code then
	jsr	prstr		; and go print it
	pla			; get a back
	rts
prerms8:
	ldx	#<ermess
	ldy	#>ermess
	jsr	prstr
	pla
	pha
	jsr	prbyte		; print the hex
	pla			; get a back
prerms9:
	rts			; and return

;
; take error code in A, hexify into ermsdc
;
logdoserr:
	pha
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	jsr	ny2hx		; hexify top nybble
	sta	ermsdc
	pla
	and	#$0F
	jsr	ny2hx
	sta	ermsdc+1	; and bot nybble
	rts

;ctrl-l.SBTTL	Packet routines - SPAK - send packet
 
;
;	This routine forms and sends out a complete packet in the
;	following format:
;
;	<SOH><char(pdlen)><char(pnum)><ptype><data><char(chksum)><eol>
;
;		Input:  kerbf1- Pointer to packet buffer
;			pdlen-  Length of data
;			pnum-	Packet number
;			ptype-  Packet type
;
;		Output: A-	True or False return code
;
 
spak:
	jsr	openrsm		; ensure rs port open
;	jsr	scred2		; clear the screen
;	ldx	#0
;	ldy	#0
;	jsr	scrplt		; home the cursor
	sec			; get cursor pos
	jsr	ploth
;	ldy	#0		; set col to 0
	ldx	#0		; set col to 0
	clc			; and put it back
	jsr	ploth
	ldx	#<snin01	; Give the user info on what we are doing
	ldy	#>snin01	;		...
	jsr	prstr		; Print the information
	ldx	#false		;[49]
	jsr	timerset	;[49]
;---
;	lda	tpak+1		; Get the total packets count
;	jsr	prbyte		;	and print that
;	lda	tpak		;		...
;	jsr	prbyte		;		...
	ldx	tpak
	lda	tpak+1
	jsr	prntad		; print packet num in decimal
;---
;	jsr	prcrlf		; Output a crelf
	lda	#0		; Clear packet data index
	sta	pdtind		;		...
spaknd: lda	spadch		; Get the padding character
	ldx	#0		; Init counter
spakpd: cpx	spad		; Are we done padding?
	bcs	spakst		;  Yes, start sending packet
	inx			; No, up the index and count by one
	jsr	putplc		; Output a padding character
	jmp	spakpd		; Go around again
spakst: lda	#soh		; Get the start-of-header char into AC
	jsr	putplc		; Send it
	lda	pdlen		; Get the data length
	clc			; Clear the carry
	adc	#3		; Adjust it
	pha			; Save this to be added into stot
	clc			; Clear carry again
	adc	#sp		; Make the thing a character
	sta	chksum		; First item,  start off chksum with it
	jsr	putplc		; Send the character
	pla			; Fetch the pdlen and add it into the
	clc			;	'total characters sent' counter
	adc	stot		;		...
	sta	stot		;		...
	lda	stot+1		;		...
	adc	#0		;		...
	sta	stot+1		;		...
	lda	pnum		; Get the packet number
	clc			;		...
	adc	#sp		; Char it
	pha			; Save it in this condition
	clc			; Clear carry
	adc	chksum		; Add this to the checksum
	sta	chksum		;		...
	pla			; Restore character
	jsr	putplc		; Send it
	lda	ptype		; Fetch the packet type
	and	#$7F		; Make sure H.O. bit is off for chksum
	pha			; Save it on stack
	clc			; Add to chksum
	adc	chksum		;		...
	sta	chksum		;		...
	pla			; Get the original character off stack
	jsr	putplc		; Send packet type
	ldy	#0		; Initialize data count
	sty	datind		; Hold it here
spaklp: ldy	datind		; Get the current index into the data
	cpy	pdlen		; Check against packet data length, done?
	bmi	spakdc		; Not yet, process another character
	jmp	spakch		; Go do chksum calculations
spakdc: lda	(kerbf1),y	; Fetch data from packet buffer
	clc			; Add the character into the chksum
	adc	chksum		;		...
	sta	chksum		;		...
	lda	(kerbf1),y	; Refetch data from packet buffer
	jsr	putplc		; Send it
	inc	datind		; Up the counter and index
	jmp	spaklp		; Loop to do next character
spakch: lda	chksum		; Now, adjust the chksum to fit in 6 bits
	and	#$C0		; First, take bits 6 and 7
	lsr	a		;	and shift them to the extreme right
	lsr	a		;	side of the AC
	lsr	a		;		...
	lsr	a		;		...
	lsr	a		;		...
	lsr	a		;		...
	clc			; Now add in the original chksum byte
	adc	chksum		;		...
	and	#$3F		; All this should be mod decimal 64
	clc			;		...
	adc	#sp		; Put it in printable range
	jsr	putplc		;	and send it
	lda	seol		; Fetch the eol character
	jsr	putplc		; Send that as the last byte of the packet
	lda	pdtind		; Set the end of buffer pointer
	sta	pdtend		;		...
	lda	#0		; Set index to zero
	sta	pdtind		;		...
	lda	debug		; Is the debug option turned on?
	cmp	#off		;		...
	beq	spaksp		; Nope, go stuff packet at other kermit
	lda	#0		; Option 0
	jsr	debg		; Do it
spaksp: lda	#0		; Zero the index
	sta	pdtind		;		...
spakdl: ldx	pdtind		; Are we done?
	cpx	pdtend		;		...
	bpl	spakcd		; Yes, go call debug again
	lda	plnbuf,x	; Get the byte to send
	jsr	putrs		; Ship it out
	inc	pdtind		; Increment the index once
	jmp	spakdl		; Go to top of data send loop
spakcd: lda	debug		; Get debug switch
	cmp	#off		; Do we have to do it?
	beq	spakcr		; Nope, return
	lda	#1		; Option 1
	jsr	debg		; Do the debug stuff
spakcr: rts			;	and return
 
;ctrl-l.SBTTL	Packet routines - RPAK - receive a packet
 
;
;	This routine receives a standard Kermit packet and then breaks
;	it apart returning the individuals components in their respective
;	memory locations.
;
;		Input:
;
;		Output: kerbf1- Pointer to data from packet
;			pdlen-  Length of data
;			pnum-	Packet number
;			ptype-  Packet type
;
 
rpak:	jsr	gobble		; Gobble a line up from the port
	 jmp	rpkfls		; Must have gotten a keyboard interupt, fail
	lda	ibmmod		; Is ibm-mode on?
	cmp	#on		;		...
	bne	rpakst		; If not, start working on the packet
rpakc0:	jsr	getc		; Any characters yet?
	 jmp	rpakst		; Got one from the keyboard
	lda	char		;[31]
	cmp	#xon		; Is it an XON?
	bne	rpakc0		; Nope, try again
rpakst:
;	jsr	scred2		; clear the screen
;	ldx	#$00
;	ldy	#$00
;	jsr	scrplt		; home the cursor
	sec			; get cursor pos
	jsr	ploth
;	ldy	#0		; set col to 0
	ldx	#0		; set col to 0
	clc			; and put it back
	jsr	ploth
	ldx	#<rcin01 	; Give the user info on what we are doing
	ldy	#>rcin01	;		...
	jsr	prstr		; Print the information
	ldx	#true		;[49]
	jsr	timerset	;[49] Set the timeout length
;---
;	lda	tpak+1		; Get the total packets count
;	jsr	prbyte		;	and print that
;	lda	tpak		;		...
;	jsr	prbyte		;		...
	ldx	tpak
	lda	tpak+1
	jsr	prntad
;---
;	jsr	prcrlf		; Output a crelf
	lda	debug		; Is debugging on?
	cmp	#off		;		...
	beq	rpaknd		;  Nope, no debugging, continue
	lda	#2		; Option 2 <reflect the fact we are in rpak>
	jsr	debg		; Do debug stuff
rpaknd: lda	#0		; Clear the
	sta	chksum		;	chksum
	sta	datind		;	index into packet buffer
	sta	kerchr		;	and the current character input
rpakfs: jsr	getplc		; Get a char, find SOH
	 jmp	rpkfls		; Got a keyboard interupt instead
	sta	kerchr		; Save it
	and	#$7F		; Shut off H.O. bit
	cmp	#soh		; Is it an SOH character?
	bne	rpakfs		; Nope, try again
	lda	#1		; Set up the switch for receive packet
	sta	fld		;		...
rpklp1: lda	fld		; Get switch
	cmp	#6		; Compare for <= 5
	bmi	rpklp2		; If it still is, continue
	jmp	rpkchk		; Otherwise, do the chksum calcs
rpklp2: cmp	#5		; Check fld
	bne	rpkif1		; If it is not 5, go check for SOH
	lda	datind		; Fetch the data index
	cmp	#0		; If the data index is not null
	bne	rpkif1		;	do the same thing
	jmp	rpkif2		; Go process the character
rpkif1: jsr	getplc		; Get a char, find SOH
	 jmp	rpkfls		; Got a keyboard interupt instead
	sta	kerchr		; Save that here
	and	#$7F		; Make sure H.O. bit is off
	cmp	#soh		; Was it another SOH?
	bne	rpkif2		; If not, we don't have to resynch
	lda	#0		; Yes, resynch
	sta	fld		; Reset the switch
rpkif2: lda	fld		; Get the field switch
	cmp	#4		; Is it < = 3?
	bpl	rpkswt		; No, go check the different cases now
	lda	kerchr		; Yes, it was, get the character
	clc			;	and add it into the chksum
	adc	chksum		;		...
	sta	chksum		;		...
rpkswt: lda	fld		; Now check the different cases of fld
	cmp	#0		; Case 0?
	bne	rpkc1		; Nope, try next one
	lda	#0		; Yes, zero the chksum
	sta	chksum		;		...
	jmp	rpkef		;	and restart the loop
rpkc1:  cmp	#1		; Is it case 1?
	bne	rpkc2		; No, continue checking
	lda	kerchr		; Yes, get the length of packet
	sec			;		...
	sbc	#sp		; Unchar it
	sec			;		...
	sbc	#3		; Adjust it down to data length
	sta	pdlen		; That is the packet data length, put it there
	jmp	rpkef		; Continue on to next item
rpkc2:  cmp	#2		; Case 2 (packet number)?
	bne	rpkc3		; If not, try case 3
	lda	kerchr		; Fetch the character
	sec			;		...
	sbc	#sp		; Take it down to what it really is
	sta	pnum		; That is the packet number, save it
	jmp	rpkef		; On to the next packet item
rpkc3:  cmp	#3		; Is it case 3 (packet type)?
	bne	rpkc4		; If not, try next one
	lda	kerchr		; Get the character and
	sta	ptype		;	stuff it as is into the packet type
	jmp	rpkef		; Go on to next item
rpkc4:  cmp	#4		; Is it case 4???
	bne	rpkc5		; No, try last case
	ldy	#0		; Set up the data index
	sty	datind		;		...
rpkchl: ldy	datind		; Make sure datind is in Y
	cpy	pdlen		; Compare to the packet data length, done?
	bmi	rpkif3		; Not yet, process the character as data
	jmp	rpkef		; Yes, go on to last field (chksum)
rpkif3: cpy	#0		; Is this the first time through the data loop?
	beq	rpkacc		; If so, SOH has been checked, skip it
	jsr	getplc		; Get a char, find SOH
	 jmp	rpkfls		; Got a keyboard interupt instead
	sta	kerchr		; Store it here
	and	#$7F		; Shut H.O. bit
	cmp	#soh		; Is it an SOH again?
	bne	rpkacc		; No, go accumulate chksum
	lda	#$FF		; Yup, SOH, go resynch packet input once again
	sta	fld		;		...
	jmp	rpkef		;		...
rpkacc: lda	kerchr		; Get the character
	clc			;		...
	adc	chksum		; Add it to the chksum
	sta	chksum		;	and save new chksum
	lda	kerchr		; Get the character again
	ldy	datind		; Get our current data index
	sta	(kerbf1),y	; Stuff the current character into the buffer
	inc	datind		; Up the index once
	jmp	rpkchl		; Go back and check if we have to do this again
rpkc5:  cmp	#5		; Last chance, is it case 5?
	beq	rpkc51		; Ok, continue
	jmp	rpkpe		; Warn user about program error
rpkc51: lda	chksum		; Do chksum calculations
	and	#$C0		; Grab bits 6 and 7
	lsr	a		; Shift them to the right (6 times)
	lsr	a		;		...
	lsr	a		;		...
	lsr	a		;		...
	lsr	a		;		...
	lsr	a		;		...
	clc			; Clear carry for addition
	adc	chksum		; Add this into original chksum
	and	#$3F		; Make all of this mod decimal 64
	sta	chksum		;	and resave it
rpkef:  inc	fld		; Now increment the field switch
	jmp	rpklp1		; And go check the next item
rpkchk: lda	kerchr		; Get chksum from packet
	sec			; Set carry for subtraction
	sbc	#sp		; Unchar it
	cmp	chksum		; Compare it to the one this Kermit generated
	beq	rpkret		; We were successful, tell the caller that
	lda	#errbch		; Store the error code
	sta	errcod		;		...
	ldx	#<erms15	; Create pointer to error text
	ldy	#>erms15	;
	jsr	prstr		; Print the chksum error
	lda	kerchr		; Print chksum from packet
	jsr	prbyte		;		...
	lda	#sp		; Space things out a bit
	jsr	cout		;		...
	lda	chksum		; Now get what we calculated
	jsr	prbyte		;	and print that
rpkfls:	lda	#0		; Zero the index for debug mode
	sta	pdtind		;		...
	lda	debug		; Is debug switch on?
	cmp	#off		;		...
	beq	rpkfnd		;  Return doing no debug stuff
	lda	#3		; Option 3 <we are in rpkfls>
	jsr	debg		; Output debug information
rpkfnd: lda	pdlen		; Get the packet data length
	clc			;	and add it into the
	adc	rtot		;	'total characters received' counter
	sta	rtot		;		...
	lda	rtot+1		;		...
	adc	#0		;		...
	sta	rtot+1		;		...
	lda	#false		; Set up failure return
	sta	ptype		;[DD] Set packet type false
	rts			;	and go back
rpkret:	lda	#0		; Zero the index for debug mode
	sta	pdtind		;		...
	lda	debug		; Check debug switch
	cmp	#off		; Is it on?
	beq	rpkrnd		; No, return with no debug
	lda	#4		; Yes, use option 4 <we received a packet>
	jsr	debg		; Print out the debug info
rpkrnd: lda	pdlen		; Get the packet data length
	clc			;	and add it into the
	adc	rtot		;	'total characters received' counter
	sta	rtot		;		...
	lda	rtot+1		;		...
	adc	#0		;		...
	sta	rtot+1		;		...
	lda	#true		; Show a successful return
	rts			;	and return
rpkpe:  ldx	#<erms16	; Set up pointer to error text
	ldy	#>erms16	;		...
	jsr	prstr		; Print the error
	lda	#errint		; Load error code and store in errcod
	sta	errcod		;		...
	jmp	rpkfls		; Go give a false return
 
;ctrl-l.SBTTL	Timerset and Timerexp
 
;
;	Routines to set and check for Kermit timeouts
;
 
;
;	Timerset - Set the timeout for receive or send
;
;	Input:	X - True for receive, false for send
;
;	Registers Detsroyed: A
;
 
timerset:	
	lda	stime		; [jrd] assume sending
	cpx	#true		;[49] Are we receiving?
	bne	timsst		;[49] No
	lda	rtime		; [jrd] ok, so we're receiving
timsst:
	ldx	#0		; [jrd] zap 'lo' byte of timer
	stx	ttime+1		;  ... remember it's byte flipped; hi, lo
	clc			; [jrd] shift 8 bit time offset (secs) by
	ror	A		;  2 into 16 bits, to get ticks.
	ror	ttime+1		;  ...
	ror	A		;  ...
	ror	ttime+1		;  ...
	sta	ttime		; [jrd] put low order byte in
	lda	RTCLOK+2	; [jrd] atari's real time clock, lo order byte
	clc			;[49]
	adc	ttime+1		;[49] Add in the receive timeout
	sta	ttime+1		;[49] 	and store it
	lda	ttime		;[49] Account for the carry if any
	adc	RTCLOK+1	; [jrd] get mid order byte of clock
	sta	ttime		;[49]	and store it
	rts			;[49] Return
 
;
;	Timerexp - Check to see if we have exceeded the timeout limit.
;
;	Input:  Ttim - time to timeout at
;		Clock+1 - current time
;
;	Registers Destroyed: A
;
 
timerexp:	
	lda	RTCLOK+1	; [jrd] Atari middle byte
	cmp	ttime		;[49] Compare it to the old minutes
	bmi	timskp		;[49] Still less
	lda	RTCLOK+2	; [jrd] Atari?
	cmp	ttime+1		;[49] Compare it to the old seconds
	bmi	timskp		;[49] Still less 
timret:	rts			;[49] We have timed out, return
timskp:	jmp	rskp		;[49] No timeout, return with a skip
 
;ctrl-l.SBTTL	DEBG - debugging output routines
 
;
;	When the debugging option is turned on, these routines periodically
;	display information about what data is being sent or received.
;
;		Input:  A-	Action type
;			Ptype-  Packet type sent or received
;			Pnum-	Packet number sent or received
;			Pdlen-  Packet data length
;
;		Output: Display info on current packet status
;
;		Registers destroyed:	A,X,Y
;
 
debg:	pha			; save a
	jsr	prcrlf		; Output a crelf
	pla			; get a back
	tax			; Hold the action code here
	sta	debinx		; Save it here
	lda	debug		; Get the debug switch
	cmp	#terse		; Is it terse
	bne	debgvr		; Nope, must be Verbose mode
	jmp	debgtr		; Yes, to terse debug output
debgvr:	lda	state		; Check the current state
	cmp	#0		; If we just started this thing
	beq	debgrf		;	then we don't need debug output yet
	cmp	#'C'		; If the transmission state is 'complete'
	beq	debgrf		;	we don't need debug output either
	lda	#<kerrts	; Get base address of the routine name and
	sta	kermbs		;	action table so that we can calculate
	lda	#>kerrts	;		...
	sta	kermbs+1	;		...
	lda	#kerrns		; Load the routine name size
	pha			; Push that
	txa			; Fetch the offset for the one we want
	pha			; And push that parameter
	jsr	genmad		; Go generate the message address
	jsr	prstr		; Now, go print the string
	lda	ptype		; Get the current packet type
	pha			; Save this accross the routine calls
	jsr	cout		; Write that out
	jsr	prcrlf		; Now write a crelf
	pla			; Get back the packet type
	sta	debchk		;	and start the checksum with that
	lda	debinx		; Get the debug action index
	bne	debg1		; If not 'sending', continue
	jsr	debprd		; Yes, go do some extra output
debg1:  cmp	#4		; Have we just received a packet?
	bne	debgrt		; No, just return
	jsr	debprd		; Print the packet info
debgrt:	jsr	prcrlf		; Output a crelf
	lda	#true		; Load true return code into AC
	rts			;	and return
debgrf:	jsr	prcrlf		; Output a crelf
	lda	#false		; Set up failure return
	rts			;	and go back
 
;
;	Debprd - does special information output including packet number,
;	packet data length, the entire packet buffer, and the checksum
;	of the packet as calculted by this routine.
;
 
debprd: jsr	prcrlf		; Start by giving us a new line
	ldx	#<debms1	; Get the first info message address
	ldy	#>debms1	;		...
	jsr	prstr		;	and print it
	jsr	prcrlf		; New line
	ldx	#<debms3	; Get address of message text
	ldy	#>debms3	;		...
	jsr	prstr		; Print it
	inc	pdtind		; Pass the SOH
	ldx	pdtind		; Get the index
	lda	plnbuf,x	; Get the data length
	sec			; Uncharacter this value
	sbc	#$20		;		...
	jsr	prbyte		; Print the hex value
	jsr	prcrlf		; New line
	ldx	#<debms2	; Get address of message text
	ldy	#>debms2	;		...
	jsr	prstr		; Print it
	inc	pdtind		; Next character is packet number
	ldx	pdtind		;		...
	lda	plnbuf,x	; Load it
	sec			; Uncharacter this value
	sbc	#$20		;		...
	jsr	prbyte		; Print the hex value
	jsr	prcrlf		; New line
	inc	pdtind		; Bypass the packet type
	ldy	#$FF		; Start counter at -1
	sty	kwrk02		; Store it here
debprc:	inc	kwrk02		; Increment the counter
	ldy	kwrk02		; Get counter
	cpy	pdlen		; Are we done printing the packet data?
	bpl	debdon		; If so, go finish up
	inc	pdtind		; Point to next character
	ldx	pdtind		; Fetch the index
	lda	plnbuf,x	; Get next byte from packet
	jsr	prchr		; Go output special character
	lda	#space		; Now print 1 space
	jsr	cout		;		...
	jmp	debprc		; Go check next character
debdon:	jsr	prcrlf		; Next line
	ldx	#<debms4	; Get the address to the 'checksum' message
	ldy	#>debms4	;		...
	jsr	prstr		; Print that message
	inc	pdtind		; Get next byte, this is the checksum
	ldx	pdtind		;		...
	lda	plnbuf,x	;		...
	sec			; Uncharacter this value
	sbc	#$20		;		...
	jsr	prbyte		; Print the hex value of the checksum
	jsr	prcrlf		; Print two(2) crelfs
	jsr	prcrlf		;		...
	rts			;	and return
 
;ctrl-l.SBTTL	Terse debug output
 
;
;	This routine does brief debug output. It prints only the contents
;	of the packet with no identifying text.
;
 
debgtr:	txa			; Look at Option
	cmp	#0		; Sending?
	beq	debgsn		; Yes, output 'SENDING: '
	cmp	#3		; Failed receive?
	beq	debgrc		; Yes, output 'RECEIVED: '
	cmp	#4		; Receive?
	beq	debgrc		; Yes, output 'RECEIVED: '
	rts			; Neither, just return
debgsn:	ldx	#<sstrng	; Get ready to print the string
	ldy	#>sstrng	;		...
	jsr	prstr		; Do it!
	jsr	prcrlf		; Print a crelf
	jmp	debgdp		; Go dump the packet
debgrc:	ldx	#<rstrng	; Get ready to print the string
	ldy	#>rstrng	;		...
	jsr	prstr		; Do it!
	jsr	prcrlf		; Print a crelf
debgdp:	ldx	pdtind		; Get index
	cpx	pdtend		; Are we done?
	bpl	debgfn		; Yes, return
	lda	plnbuf,x	; Get the character
	jsr	prchr		; Print it
	lda	#space		; Print a space
	jsr	cout		;		...
	inc	pdtind		; Advance the index
	jmp	debgdp		; Do next character
debgfn:	jsr	prcrlf		; Print a crelf then...
	rts			;	Return
 
;ctrl-l.SBTTL	Dos routines
 
;
;	These routines handle files and calls to the DOS
;

;
;	Logrcv:	Tells the user what file we're receiving.
;	Logsnd:	Similar one for transmits.
;		these expect primfn to contain the pathname
;		just opened, so use them AFTER open.
;
logrcv:
	ldx	#<logrcvm	; print "Receiving..." msg
	ldy	#>logrcvm
	jmp	logxr		; use common thread
logsnd:
	ldx	#<logsndm	; print "Sending..." msg
	ldy	#>logsndm
logxr:
	jsr	prstr
	ldx	#<primfn	; print the pathname
	ldy	#>primfn
	jsr	pstreol
	jsr	prcrlf		; and an eol
	rts

;
;	This routine opens a file for either input or output. If it
;	opens it for output, and the file exists, and file-warning is
;	on, the routine will issue a warning and attempt to modify
;	the filename so that it is unique.
;
;		Input:	A- Fncrea - open for read
;			   Fncwrt - open for write
;
;		Output:	File is opened or error is issued
;
 
openf:
	pha			; [jrd] save r/w code
	jsr	closers		; [jrd] make sure the rs port's closed
	pla			; [jrd] get r/w code back
	cmp	#fncwrt		; [jrd] open for writing?
	beq	openfo		; [jrd] yup
;
; open for read
;
;	lda	#<dsknam	; [jrd] file name lo
;	ldy	#>dsknam	; [jrd] file name hi
;----
	jsr	parsefcb	; [jrd] parse the pathname
	jsr	bldprm		; [jrd] reformat it
	lda	#<primfn
	ldy	#>primfn
;----
	ldx	#dskchan	; [jrd] disk IOCB please
	jsr	opencin		; [jrd] try to open it
	jmp	opnfi1		; [jrd] go handle status etc
;
openfo:
	jsr	parsefcb	; [jrd] parse and merge pathname in fcb
;	lda	flsrw		;[23] Get the file mode
;	cmp	#fncwrt		;[23] Are we opening for output?
;	bne	opnnlu		;[23] No, no lookup needed
	lda	#on		;[23] Yes, set the 'first mod' switch
	sta	dosffm		;[23]	in case we have to alter the filename
	lda	filwar		;[23] Get the file warning switch
	cmp	#on		;[23] Is it on?
	bne	opnnlu		;[23] If not, don't do the lookup
opnlu:	jsr	lookup		;[23] Do the lookup
	 jmp	opnnlu		;[23] Suceeded, open the file
;	lda	dosffm		;[23] Is this the first time through?
;	cmp	#on		;[23]		...
;	bne	opnalt		;[23] If not, continue
; need this zzz
;	jsr	prfn		; [jrd] print the conflicting file name
	lda	#ATEOL
;	jsr	sputch
	jsr	scrput
	ldx	#<primfn	; name buffer addr
	ldy	#>primfn
	jsr	pstreol		; print eol terminated string
	ldx	#<erms1a	;[23] Otherwise, print an error message since
	ldy	#>erms1a	;[23]	the file already exists
	jsr	prstr		;[23]		...
opnalt:	jsr	alterf		;[23] No good, go alter the filename
	jmp	opnlu		;[23] Try the lookup again
opnnlu:	jsr	bldprm		;[23] Build the filename again
	ldx	#dskchan	; [jrd] make sure it's still there
	lda	#<primfn	; [jrd] file name lo	
	ldy	#>primfn	; [jrd] file name hi
	jsr	opencout	; [jrd] try to open it
				; [jrd] and fall thru to status handler

opnfi1:	
	cpy	#SUCCES		; open succeed?
	bne  	opfail 		;[DD] If not, error
	lda	#0
	sta	eodind		;[DD] Clear end of dat flag
opnex:  lda	#true		;[DD] The open worked, return true
	rts			;[DD]		...
opfail:
	tya			; save error stat
	pha
	ldx	#<opnflm1	; print the open failure message
	ldy	#>opnflm1
	jsr	prstr
	pla			; get the code back
	jsr	prbyte
	ldx	#<opnflm2	; print the next part
	ldy	#>opnflm2
	jsr	prstr
	ldx	#dskchan
	lda	ICAX1,X		; get the aux value
	jsr	prbyte
	ldx	#<opnflm3	; print the next part
	ldy	#>opnflm3
	jsr	prstr
	ldx	#dskchan
	ldy	ICBAH,X		; get fn ptr hi
	lda	ICBAL,X		;  and lo
	tax			; into x...
	jsr	pstreol		; and print it.
;
	ldx	#<opnflm4	; print last part
	ldy	#>opnflm4
	jsr	prstr
;	
	jmp	fatal		;[DD] Failed, go handle that
;	rts			;[DD]		...

opnflm1: .byte	ATEOL,"Open failure ",0
opnflm2: .byte	" AUX1 ",0
opnflm3: .byte	" Name '",0
opnflm4: .byte	"'",ATEOL,0
;
;	Lookup - searches for a filename in a directory. It is used to
;	support file warning during the opening of a file.
;
 
lookup:	lda	#fncrea		;[23] Get an 'R
	sta	flsrw		;[23] Store it in the file mode switch
	jsr	locent		;[23] Go try to locate that file
	 jmp	locfnf		;[23] File not found? We are in good shape
	lda	#errfae		;[23] Store the error code
	sta	errcod		;[23]		...
	jmp	rskp		;[23] Return with skip, have to alter filename
locfnf:	lda	#fncwrt		;[23] Get a 'W
	sta	flsrw		;[23] Store that
	rts			;[23] Return without a skip
 
;
;	Alterf - changes a filename in the filename buffer to make it unique.
;	It accomplishes this in the following manner.
;
;		1) First time through, it finds the last significant character
;			in the filename and appends a '.0' to it.
;
;		2) Each succeeding time, it will increment the trailing integer
;			that it inserted the first time through.
;
 
alterf:	lda	dosffm		;[23] Get the 'first mod' flag
	cmp	#on		;[23] Is it on?
	bne	altsm		;[23] If not, drop into regular loop
;
	lda	#off		;[23] Shut the 'first mod' flag off
	sta	dosffm		;[23]		...
	lda	#0		; [jrd] set ver # to zero
	sta	dosfvn		; and drop into normal code, to inc it
	jmp	altfm		; first mod, skip inc code
;
altsm:				; nth mod...
	ldx	dosfvn		;[23] Get the file version number
	inx			;[23] Increment it
	stx	dosfvn		;[23] Save the new version number
	txa			;[23] Get the version number in the AC
; no need	cmp	#0		;[23] Is it 0 ?
	beq	altng		;[23] Yes, cannot alter name
altfm:
	jsr	altstv		;[23] Go store the version
	rts			;[23] And return
;
altng:	lda	#errfal		;[23] Store the error code
	sta	errcod		;[23]		...
	ldx	kerosp		;[23] Get the old stack pointer
	txs			;[23]	and restore it
	jmp	kermit		;[23] Go back to top of loop
 
;
;	Altstv - stores the version number passed to it into the filename
;	buffer at whatever position dosfni is pointing to.
;	Hexifies the version num, and sticks it in as the file type
;	field.  zzz later, do this in decimal
 
altstv:	
;---
;	ldy	dosfni		;[23] Get the filename index
;---
	ldy	#pnd_es		; [jrd] extension text size
	pha			;[23] Save the value
	lda	#2		; new ext is two bytes
	jsr	altdep		; stuff it in
	pla			; get value back again
	pha			; save once more
	lsr	a		;[23] Shift out the low order nibble
	lsr	a		;[23]		...
	lsr	a		;[23]		...
	lsr	a		;[23]		...
	jsr	altstf		;[23] Stuff the character
	pla			;[23] Grab back the original value
	and	#$0F		;[23] Take the low order nibble
	jsr	altstf		;[23] Stuff the next character
;---
;	iny			; bump again
;	lda	#ATEOL		; get an EOL
;	sta	fcb1,y		;  to terminate file name
;---
	rts			;[23]	and return
 
altstf:	ora	#$30		;[23] Make the character printable
	cmp	#$3A		;[23] If it is less than '9'
	bcc	altdep		;[23]	then go depisit the character
	adc	#6		;[23] Put the character in the proper range
altdep:
;---
;	sta	fcb1,y		;[23] Stuff the character
;---
	sta	path,y
	iny			; bump idx
	rts			;[23]	and return
 
;
;	Locent -  Try to find a file 
;
 
locent:	jsr	bldprm		;[23]
	ldx	#dskchan	; [jrd] disk iocb please
	lda	#<primfn	; [jrd] file name lo
	ldy	#>primfn	; [jrd] file name hi
	jsr	opencin		; [jrd] try to open it
;
;	jsr	rddsk  		;[23] Get disk status
;	cmp	#00		;[23] Is it 0?
	cpy	#SUCCES		; open succeed?
	bne  	locok 		;[23] No, file doesn't exist
;	lda	#8		;[23] Fle exists, close the file
;	jsr	close		;[23]	commodore	...
	jsr	closec		; and atari version ...	
	jmp	rskp		;[23] Return with a skip!
locok:
;	lda	#8		;[23] File doesn't exist, close the file
;	jsr	close		;[23]		...
	jsr	closec		; [jrd] just do this on atari
	rts			;[23] Return
 
;
;	Bldprm - Build the primary filename
;
 
bldprm:
;
;	we assume pathnames are all merged here, and the result is 
;	in path
;
	lda	#<path		; point at the pathname struct
	sta	pndptr
	lda	#>path
	sta	pndptr+1
	ldx	#<primfn	; and point at the string
	ldy	#>primfn
	jsr	pn2str		; convert pathname to string
;
	rts			;[23] Return
 
;
;	Parsefcb:	parse-pathname of fcb1, and merge with
;			default, leaving result in path.
;			Intended to be used prior to bldprm.
;
parsefcb:
	lda	#<path		; point at the pathname struct
	sta	pndptr
	lda	#>path
	sta	pndptr+1
	ldx	#<fcb1		; and point at the string
	ldy	#>fcb1
parsefxy:
	jsr	parsepn		; parse fcb1 -> path
	lda	#<defpath	; make sure
	sta	pnddef		;  default is set
	lda	#>defpath	;
	sta	pnddef+1	;
	jsr	pnmerge		; merge default -> pathname
	rts			; done

;
;
;	parseifn:	parse and merge the init file pathname,
;			leaving the result in path.
;
parseifn:
	lda	#<path		; point at the pathname struct
	sta	pndptr
	lda	#>path
	sta	pndptr+1
	ldx	#<inifil	; init file pathname lo
	ldy	#>inifil
	jmp	parsefxy	; jump into middle of parsefcb


;ctrl-l;
;	Closef - closes the file which was open for transfer. 
;
 
closef:
	jsr	closers		; [jrd] make sure rs port's closed
	ldx	#dskchan
	jsr	closec
	lda	#true		; the close worked, return true
	rts			;		...
 
;
;	Dirst - Get a disk directory
;
dirhrld1: .byte	"  File        Sectors",ATEOL,0
dirhrld2: .byte	" ------------ -------",ATEOL,0
direlck: .byte	"  locked",0
dirst:	
	lda	#0		; allow default of *.*
	jsr	enterpn		;  and get a pathname
	jsr	prcfm		; confirm it
	jsr	parsefcb	; parse and merge the resultant filespec
	jsr	closers		; make sure comm port's closed
;	jsr	prcrlf		; cosmetics...
;----
;	jsr	parsefcb	; [jrd] parse the pathname
	jsr	bldprm		; [jrd] reformat it
	lda	#<primfn
	ldy	#>primfn
;----
	ldx	#<dirhrld1	; print the directory
	ldy	#>dirhrld1	;  herald first line
	jsr	prstr
	ldx	#<dirhrld2	; print the directory
	ldy	#>dirhrld2	;  herald second line
	jsr	prstr
	jsr	dirini
xdir1:
	jsr	dirnxt		; get one
	bcs	xdir9		; failed
	lda	#<dirpath	; point at the pathname struct
	sta	pndptr
	lda	#>dirpath
	sta	pndptr+1
	ldx	#<primfn	; and point at the string
	ldy	#>primfn
	jsr	pn2str		; convert pathname to string
	lda	#space
	jsr	cout		; print a leading space
	ldx	#<primfn	; get the resultant pathname
	ldy	#>primfn	; and print that
	jsr	pstreol
	lda	#15		; tab to col 15
	sta	COLCRS		;  for the sector count
	lda	#space		; zap temp slot to space
	sta	strptr		; just a temp...
	ldy	#0		; idx
xdir2:
	sty	strptr+1
	lda	dirsect,y	; get a byte
	cmp	#'0'		; a zero?
	beq	xdir3		; yup, alter it maybe
	pha			; save it for a bit
	lda	#'0'		; make the altered char a 0
	sta	strptr
	pla			; get it back
	jmp	xdir4
xdir3:
	lda	strptr		; get the altered version
xdir4:
	jsr	cout		; print it
	ldy	strptr+1	; get idx
	iny			; bump
	cpy	#3		; done 3 yet?
	bcc	xdir2		; nope, go on
;	
; see if the file's locked
;
	lda	dirplck		; get the flag byte
	cmp	#space
	beq	xdir5		; space, so not locked
	ldx	#<direlck	; point at the string
	ldy	#>direlck
	jsr	prstr		; and print it
xdir5:
	jsr	prcrlf		; end line
	jmp	xdir1		; and go back for more
xdir9:
	ldx	#<primfn	; free space string in primfn
	ldy	#>primfn
	jsr	pstreol
	jsr	dircls
	jmp	kermit
;-------
;
;	ldx	#dskchan
;	jsr	opencdir	; get a dir list
;	cpy	#SUCCES		; winning?
;	beq	dirs1		; yup, go print it out
;	jmp	opfail		; go display the error
;dirs1:
;	ldx	#dskchan
;	jsr	chrin
;	cpy	#EOFERR		; eof?
;	beq	dirs9
;	cpy	#SUCCES		; ok?
;	beq	dirs2
;	jmp	opfail		; go print the error
;dirs2:
;;	jsr	sputch		; put it out
;	jsr	scrput
;	jmp	dirs1
;dirs9:
;	ldx	#dskchan
;	jsr	closec
;	jmp	kermit		; return to command loop

;
;	Enterpn:	Util used in rename, delete.  Enter a string
;			from command line, parse it into path.
;	Input:	A zero ->     default of *.*
;		  non-zero -> no default allowed
;			Returns carry set if ng, clear otherwise
;
enterpn:
	pha			; save default flag
	ldx	#<wildfn	; [jrd] default is all wild
	stx	cmdptr		; set default pointer
	ldy	#>wildfn	; ...
	sty	cmdptr+1	; ...
	lda	#3		; length of wildfn
	jsr	cmd2fcb		; copy it in ahead of time.  shouldn't
				;  be necessary, but cp appears to return
				;  no data even though default supplied
	lda	#<kerehr	;[40] Point to the extra help commands
	sta	cmehpt		;[40]		...
	lda	#>kerehr	;[40]		...
	sta	cmehpt+1	;[40]		..
	ldx	#mxfnl		;[40] Longest length a disk string may be
	pla			; get default flag back
	cmp	#0		; do we default to *.*?
	bne	enterpn1	; nope, must enter one
	ldy	#cmfehf|cmfdff
;	ldy	#cmfdff		;[40] Tell Comnd about extra help and def
	jmp	enterpn2	; go enter it
enterpn1:
	ldy	#cmfehf		; no flags, must enter one
enterpn2:
	lda	#cmifi		;[40] Load opcode for parsing file
	jsr	comnd		;[40] Call Comnd routine
	 jmp	enterpn9	;[40] Continue, no string parsed
	jsr	cmd2fcb		; [jrd] get it into fcb1
	ldx	#<fcb1		; parse it
	ldy	#>fcb1
	jsr	parsepn
	clc
	rts
enterpn9:
	sec
	rts

;
;	Rename:		Rename file(s).
;			Expects user to enter two filespecs; which
;			may be wildcarded.  The two names are run thru
;			the pathname parser and merger, checked for 
;			reasonableness, and formatted as 
;			"Dn:NAME.1,NAME.2" for the FMS's renamer.
;
rename:
	lda	#$FF		; no default, must enter one
	jsr	enterpn		; get a pathname from cmd line
	bcs	rename9		; oops!  bad/missing pathname
	jsr	parsefcb	; [jrd] parse and merge it
	jsr	bldprm		; [jrd] format it to output buf
;
;	now get the second one
;
	lda	#$FF		; no default, must enter one
	jsr	enterpn		; enter a second pathname
	bcs	rename9		; ng, quit
	jsr	prcfm		; confirm the whole command
;
; zzz check to make sure no device was specified here???
;
;	now must append this pathname to the one already in
;	primfn.  find the ATEOL, stuff a comma in its place, and
;	compute the address of the byte following, then format
;	pathname into there.
;
	ldy	#0		;start at beginning
	lda	#ATEOL		; get an eol
rename1:
	cmp	primfn,y	; this it?
	beq	rename2		; yup, go fix it
	iny			; try next
	bne	rename1
rename2:
	lda	#','		; get a comma
	sta	primfn,y	; stuff in in
	iny			; bump idx
	tya			; now compute buf addr for
	clc			;  second pathname
	adc	#<primfn	; add lo byte to offset
	tax			; save result in x
	lda	#>primfn	; get hi byte
	adc	#0		; add carry
	tay			; into y, top byte of addr
	jsr	pn2str		; convert it to string
;
	jsr	closers		; make sure comm port's closed
	ldx	#dskchan
	lda	#RENAME		; rename command code
	sta	ICCOM,X		; store into iocb
	jsr	iozax		; zap aux1, aux2
	lda	#<primfn	; set up for call to CIO
	ldy	#>primfn
	jsr	iosba		; set buf addr in iosb
	jsr	CIOV		; do the rename
;
; check status...
;
	cpy	#SUCCES		; ok?
	beq	rename8		; yup, go back
	jmp	opfail		; nope, go gripe about the error
rename8:
;	jmp	kermit		;[40] Go back for more commands
rename9:			; zzz maybe some kind of error?
	jmp	kermit

;
;	Erase:		Delete files(s) on the default disk.
;			prompts for a pathname, which may be wildcarded,
;			and does the FMS delete op on it.
erase:
	lda	#$FF		; no default, must enter one
	jsr	enterpn		; get a pathname from cmd line
	bcs	erase9		; no good, stop
	jsr	prcfm		; confirm
	jsr	parsefcb	; [jrd] parse and merge it
	jsr	bldprm		; [jrd] format it to output buf
	jsr	closers		; make sure comm port's closed
;
	ldx	#dskchan
	lda	#DELETE		; rename command code
	sta	ICCOM,X		; store into iocb
	jsr	iozax		; zap aux1, aux2
	lda	#<primfn	; set up for call to CIO
	ldy	#>primfn
	jsr	iosba		; set buf addr in iosb
	jsr	CIOV		; do the rename
;
; check status...
;
	cpy	#SUCCES		; ok?
	beq	erase8		; yup, go back
	jmp	opfail		; nope, go gripe about the error
erase8:
;	jmp	kermit		;[40] Go back for more commands
erase9:				; zzz maybe some kind of error?
	jmp	kermit
 
;ctrl-l;
;	Bufill - takes characters from the file, does any neccesary quoting,
;	and then puts them in the packet data buffer. It returns the size
;	of the data in the AC. If the size is zero and it hit end-of-file,
;	it turns on eofinp.
;
 
bufill:	lda	#0		; Zero
	sta	datind		;	the buffer index
bufil1:	jsr	fgetc		; Get a character from the file
	 jmp	bffchk		; Go check for actual end-of-file
	sta	kerchr		; Got a character, save it
	tax			;[31] and a copy to X
	lda	filmod		;[DD] Check if conversion necessary
	cmp	#ftatas		;[DD] Is it ATTASCII?
	bne	bufcv1		;[DD] No
;
;	lda	at2as,x		;[31] Get ASCII equivalent
;
	ldx	#<xat2as	; point at xlate tab
	ldy	#>xat2as
	lda	kerchr		; and get char to xlate
	jsr	xlate
;
	sta	kerchr		;
;	jmp	bufcv2		;[jrd] no need if 'script' not there
bufcv1:
;	cmp	#3		;[DD] Is it Speedscript?
;	bne	bufcv2		;[DD] No
;	jsr	cvs2a		;[DD] Conv. Speedscript to ASCII
bufcv2:
bufceb: lda	ebqmod		; Check if 8-bit quoting is on
	cmp	#on		;		...
	beq	bufil2		; If it is, see if we have to use it
	jmp	bffqc		; Otherwise, check normal quoting only
bufil2: lda	kerchr		; Get the character
	and	#$80		; Mask everything off but H.O. bit
	beq	bffqc		; H.O. bit was not on, so continue
	lda	sebq		; H.O. bit was on, get 8-bit quote
	ldy	datind		; Set up the data index
	sta	(kerbf1),y	; Stuff the quote character in buffer
	iny			; Up the data index
	sty	datind		; And save it
	lda	kerchr		; Get the original character saved
	and	#$7F		; Shut H.O. bit, we don't need it
	sta	kerchr		;		...
bffqc:  lda	kerchr		; Fetch the character
	and	#$7F		; When checking for quoting, use only 7 bits
bffqc0: cmp	#sp		; Is the character less than a space?
	bpl	bffqc1		; If not, try next possibility
	ldx	filmod		; Get the file-type
	cpx	#ftbin		; [jrd] IF >= binary
	bcs	bffctl		; If it is not text, ignore <cr> problem
	cmp	#cr		; Do we have a <cr> here?
	bne	bffctl		; Nope, continue processing
	ldx	#on		; Set flag to add a <lf> next time through
	stx	addlf		;		...
	jmp	bffctl		; This has to be controlified
bffqc1: cmp	#del		; Is the character a del?
	bne	bffqc2		; If not, try something else
	jmp	bffctl		; Controlify it
bffqc2: cmp	squote		; Is it the quote character?
	bne	bffqc3		; If not, continue trying
	jmp	bffstq		; It was, go stuff a quote in buffer
bffqc3: lda	ebqmod		; Is 8-bit quoting turned on?
	cmp	#on		;		...
	bne	bffstf		; If not, skip this junk
	lda	kerchr		;	otherwise, check for 8-bit quote char.
	cmp	sebq		; Is it an 8-bit quote?
	bne	bffstf		; Nope, just stuff the character itself
	jmp	bffstq		; Go stuff a quote in the buffer
bffctl: lda	kerchr		; Get original character back
	eor	#$40		; Ctl(AC)
	sta	kerchr		; Save the character again
bffstq: lda	squote		; Get the quote character
	ldy	datind		;	and the index into the buffer
	sta	(kerbf1),y	; Store it in the next location
	iny			; Up the data index once
	sty	datind		; Save the index again
bffstf: inc	schr		; Increment the data character count
	bne	bffsdc		;		...
	inc	schr+1		;		...
bffsdc: lda	kerchr		; Get the saved character
	ldy	datind		;	and the data index
	sta	(kerbf1),y	; This is the actual char we must store
	iny			; Increment the index
	sty	datind		; And resave it
	tya			; Take this index, put it in AC
	clc			; Clear carry for addition
	adc	#6		; Adjust it so we can see if it
	cmp	spsiz		;	is >= spsiz-6
	bpl	bffret		; If it is, go return
	jmp	bufil1		; Otherwise, go get more characters
bffret: lda	datind		; Get the index, that will be the size
	rts			; Return with the buffer size in AC
bffchk:	lda	datind		;[21] Get the data index
	cmp	#0		;[21] Is it zero?
	bne	bffne		;[21] Nope, just return
	tay			;[21] Yes, this means the entire file has
	lda	#true		; 	been transmitted so turn on
	sta	eofinp		;	the eofinp flag
	tya			;[21] Get back the size of zero
bffne:  rts			; Return
 
;
;	Bufemp - takes a full data buffer, handles all quoting transforms
;	and writes the reconstructed data out to the file using calls to
;	FPUTC.
;
 
bufemp:	lda	#0		; Zero
	sta	datind		;	the data index
bfetol: lda	datind		; Get the data index
	cmp	pdlen		; Is it >= the packet data length?
	bmi	bfemor		; No, there is more to come
	lda	#true		; [jrd] say we win
	rts			; Yes, we emptied the buffer, return
bfemor: lda	#false		; Reset the H.O.-bit-on flag to false
	sta	chebo		;		...
	ldy	datind		; Get the current buffer index
	lda	(kerbf1),y	; Fetch the character in that position
	sta	kerchr		; Save it for the moment
	cmp	rebq		; Is it the 8-bit quote?
	bne	bfeqc		; No, go check for normal quoting
	lda	ebqmod		; Is 8-bit quoting on?
	cmp	#on		;		...
	bne	bfeout		; No quoting at all, place char in file
	lda	#true		; Set H.O.-bit-on flag to true
	sta	chebo		;		...
	inc	datind		; Increment the data index
	ldy	datind		; Fetch it into Y
	lda	(kerbf1),y	; Get the next character from buffer
	sta	kerchr		; Save it
bfeqc:  cmp	rquote		; Is it the normal quote character
	bne	bfeceb		; No, pass this stuff up
	inc	datind		; Increment the data index
	ldy	datind		;	and fetch it in the Y-reg
	lda	(kerbf1),y	; Get the next character from buffer
	sta	kerchr		; Save it
	and	#$7F		; Check only 7 bits for quote
	cmp	rquote		; Were we quoting a quote?
	beq	bfeceb		; Yes, nothing has to be done
	cmp	rebq		; Check for eight-bit quote char as well
	beq	bfeceb		; Skip the character adjustment
	lda	kerchr		; Fetch back the original character
	eor	#$40		; No, so controlify this again
	sta	kerchr		; Resave it
bfeceb: lda	chebo		; Is the H.O.-bit-on flag lit?
	cmp	#true		;		...
	bne	bfeout		; Just output the character to the file
	lda	kerchr		; Fetch the character
	ora	#$80		; Light up the H.O. bit
	sta	kerchr		; Resave it
bfeout: lda	filmod		; Check if this is a text file
	cmp	#ftbin		; [jrd] Filmod < 2 ?
	bcs	bfefpc		; If not, continue normal processing
	lda	kerchr		; Get a copy of the character
	and	#$7F		; Make sure we test L.O. 7-bits only
	tax			;[31] Put a copy in X
	cmp	#cr		; Do we have a <cr>?
	bne	bfeclf		; No, then check for <lf>
	lda	#on		; Yes, set the 'Delete <lf>' flag
	sta	dellf		;		...
	jmp	bfefpc		; And then continue
bfeclf: cmp	#lf		; Do we have a <lf>?
	bne	bfenlf		; Nope, We must go shut the Dellf flag.
	lda	dellf		; We have a <lf>, is the flag on?
	cmp	#on		;		...
	bne	bfefpc		; If not, continue normally
	lda	#off		; Flag is on, <lf> follows <cr>, ignore it
	sta	dellf		; Start by zeroing flag
	jmp	bfeou1		; Now go to end of loop
bfenlf: lda	#off		; Zero Dellf
	sta	dellf		;		...
bfefpc: lda	filmod		;[DD] Get file type 
	cmp	#ftatas		;[DD] Check ATTASCII
	bne	bfefp2		;[DD]
;
;	lda	as2at,x		;[31] Get ATASCII equivalent
;
	ldx	#<xas2at	; point at xlate tab
	ldy	#>xas2at
	lda	kerchr
	jsr	xlate
;
	sta	kerchr		;[31]
;	jmp	bfefp3		;[jrd] no need if no 'script'
bfefp2:
;	cmp	#3		;[DD] Check Speedscript
;	bne	bfefp3		;[DD]
;	jsr	cva2s		;[DD] Convert ASCII to Speedscript
bfefp3: lda	kerchr		; Get the character once more
	jsr	fputc		; Go write it to the file
	 jmp	bfeerr		; Check out the error
	inc	rchr		; Increment the 'data characters receive' count
	bne	bfeou1		;		...
	inc	rchr+1		;		...
bfeou1: inc	datind		; Up the buffer index once
	jmp	bfetol		; Return to the top of the loop
 
bfeerr:
;	and	#$7F		; Shut off H.O. bit
;	sta	errcod		; Store the error code where it belongs
; fputc stored the error code
	lda	#false		; Indicate failure
	rts			;	and return
 
;ctrl-l
;
;	Getnfl - returns the next filename to be transferred, in fcb1.
;	Returns EOF if out of files
;
 
getnfl:
;
; wildcard support added here by jrd
;
	jsr	ssfnxt		; set up next file name
	bcs	getnf1		; cs means no more
	lda	#false		; say we're not at end of file list
	rts			; and return
getnf1:
	lda	#eof		; No more files (return eof)
	rts
 
;
;	Getfil - gets the filename from the receive command if one was
;	parsed. Otherwise, it returns the name in the file header packet.
;
 
getfil: lda	usehdr		; Get the use-header switch
	cmp	#on		; Is it on
	bne	getfl1		; If not, keep what we have in the fcb
	jsr	clrfcb		;		...
	ldy	#0		; Initialize the y reg
;	lda	pdlen		; Copy the packet data length
;	sec			; Now subtract off the overhead
;	sbc	#3		;		...
;	sta	kwrk02		;	into a work area
getfl0: lda	(kerbf1),y	; Get a character from the packet buffer
	sta	fcb1,y		; Stuff it in the fcb
	iny			; Up the index once
	cpy	pdlen		; Are we finished?
	bmi	getfl0		; Nope, go do next byte
;	lda	#0		;
;	sta	fcb1,y		; Nul at end
getfl1: rts
 
 
;ctrl-l;
;	Fgetc - returns the next character from the file in the AC. It
;	handles all of the low level disk I/O. Whenever it successfully
;	gets a character, it skips on return. If it does not get a
;	character, it doesn't skip.
;
 
fgetc:	lda	addlf		; Get the 'add a lf' flag
	cmp	#on		; Is it on?
	bne	fgetc1		; No, continue with normal processing
	lda	#off		; Zero the flag first
	sta	addlf		;		...
	lda	#lf		; Get a <lf>
	jmp	fgtexi		;   and return that as the next character
fgetc1: lda	eodind		;[DD] Check end-of-data flag
	cmp	#off		;[21] Is it on?
	beq	fgtc2a		;[DD][21] No, get next character
	jmp	fgteof		;[21] Yes, no data to read
fgtc2a:
	ldx	#dskchan	; [jrd] Disk iocb please
	jsr	chrin		; [jrd] get one byte please
	pha			;[DD] Save it
	cpy	#SUCCES		; [jrd] get one?
	beq	fgtgnc		; Return
	lda	#1		; [jrd] set eodind; any error -> eof
				;  zzz put some code in here to bitch
				;  about errors other than eof
	sta	eodind		; say end of data for next time
	cpy	#3		; EOF lookahead?
	beq	fgtgn0		; yup, close and return it
	cpy	#EOFERR		; a real eof?
	beq	fgteof0		; yup, close it and don't return
; debugging
;	tya			; save code for a sec
;	pha
;	ldx	#<fgtdbg
;	ldy	#>fgtdbg
;	jsr	prstr
;	pla
;	jsr	prbyte
;	jsr	prcrlf
;---
	tya			; get code into a
	jsr	logdoserr
	lda	#errfde		; say we got an io error
	sta	errcod
;
; what a crock! there's no way to return an error indication from this
; thing.  Give up.  Print the error here, and go thru fatal error vect
; to reset stack
;
	jsr	prerms
	jmp	fatal
;
fgtgn0:	jsr	closef		;[DD] Eof so close but return
fgtgnc:	pla			; Get back character
fgtgn1:
; obsolete
;	ldx	fbsize		; Get the file-byte-size
;	cpx	#fbsbit		; Is it seven-bit?
;	bne	fgtexi		; If not, leave with character intact
;	and	#$7f		; Shut off the H.O. byte
fgtexi:	jmp	rskp  		; Return skip
;
fgteof0:
	pla			; [jrd, v3.6] pop dead char, to fix stack
fgteof:	jsr	closef		; close the file
	lda	#0		; Return null
	rts			;		... 
;
;fgtcan: jmp	fatal 		; Just go give an error
;
;fgtdbg:	.byte	"Fgetc error ",0
;
;
;	Fputc - takes a character passed to it in the AC and writes it
;	to the file being transferred in.
;
 
fputc:
	ldx	#dskchan	; [jrd] disk iocb please
	jsr	chrout		;[DD] Write it to disk
;	jsr	readst		;[DD] Check for errors
;	cmp	#00		;[DD] Do we really need this?
	cpy	#SUCCES		; [jrd] io succeed?
	beq	fputex		;[DD] No error
;	sta	errcod  	;[DD] If error
;---
;	sty	errcod
;	ldx	#<erms0a	;[DD] Get the address of the error message
;	ldy	#>erms0a	;[DD]		...
;	jsr	prstr   	;[DD] Print message
;	lda	errcod		;[DD] 	and status
;	jsr	prbyte  	;[DD]		...
;	jsr	prcrlf
;---
	tya
	jsr	logdoserr	; format the code into the msg
	lda	#errfde		; say what we got
	sta	errcod
;---
; ???	jmp	fatal		;[DD] Blow up
	rts			; [jrd] return without skip, ie error
fputex: lda	#00		; Return null
	jmp	rskp	  	; 	with a skip!
 
;ctrl-l.SBTTL	Save and Restore Parameters
 
;	The following routines will save and restore kermit 
;	parameters in a file named 'KERMIT.INI'. Eventually 
;	will add ability to specify file for save/restore.
;
 
;
;	Savst - Save parameters
;
;	Registers Destroyed: A,X,Y
;
 
savst:	jsr	closers
	jsr	prcfm		;[47] Parse and print a confirm
	ldx	#<inifil	; [jrd] point at file name
	ldy	#>inifil	;  ...
	jsr	parseifn	; parse init file pathname
	jsr	bldprm		; [jrd] reformat it
	lda	#<primfn
	ldy	#>primfn
	ldx	#dskchan
	jsr	opencout	; [jrd] try to open it
	cpy	#SUCCES		; winning?
	beq	savst1		; yup. go ahead and save
	jmp	opfail		; nope, say why
savst1:
	ldy	#0		;[47] Start with the escape character
savlop:	
	sty	strptr		; [jrd] temp
	lda	escp,y		;[47]		...
	ldx	#dskchan
	jsr	chrout		;[47] Write it to disk
; zzz check status
	ldy	strptr		; get idx back
	iny			;[47]
	cpy	#quote+1-escp	;[47] Are we at the end?
	bne	savlop		;[47] No, do the next parameter
	jsr	closef
	jmp	kermit		;[47]	and parse for more commands
 
;
;	Restst - Restore parameters
;
 
restst:	jsr	prcfm		;[47] Parse and print a confirm
	jsr	restin		;[47] Go restore the parameters
	jmp	kermit		;[47] Failed, restart kermit
 
restin:
	jsr	closers		; make sure comm port's closed
	ldx	#<inifil	; [jrd] point at file name
	ldy	#>inifil	;  ...
	jsr	parseifn	; parse init file pathname
	jsr	bldprm		; [jrd] reformat it
	lda	#<primfn
	ldy	#>primfn
	ldx	#dskchan
	jsr	opencin		; [jrd] try to open it
	cpy	#SUCCES		; winning?
; zzz check for file not found here
	bne	rsterr		; nope, give up
	ldy	#0		;[47] Start index at escp
rstlop:	sty	savey		;[47] Save the current index
	ldx	#dskchan	; [jrd] point at disk IOCB
	jsr	chrin		;[47] Get a byte from the disk
; zzz save status somewhere?
	ldy	savey		;[47] Restore the index
	sta	escp,y		;[47] Store the character away
	iny			;[47] Increment the index
	cpy	#quote+1-escp	;[47] Are we at the end of the parameter list?
	bne	rstlop		;[47] No, get next parameter
;	lda	scrtype		; check if the new screen driver exists
;	jsr	scrtst		; [jrd] no need, they're all here
;	bcc	rstlop1		; no it doesnt
rsterr:	lda	#scr80		; default to 80-columns
	sta	conscrt
rstlop1: 
; not til connect time
; 	jsr	scrent		; initilize the new screen package
;	lda	#8		;[47] Close the init file
;	jsr	close		;[47]		...
	ldx	#dskchan
	jsr	closec
	rts			; all done
 
inifil:	.byte	"KERMIT.INI",ATEOL	; init file, eol terminated
;
;ctrl-l.SBTTL	Utility routines
 
;
;	The following routines are short low-level routines which help
;	shorten the code and make it more readable
;
;
;	Incn - increment the packet sequence number expected by this
;	Kermit. Then take that number Mod $3f.
;
 
incn:	pha			; Save AC
	lda	n		; Get the packet number
	clc			; Clear the carry flag for the add
	adc	#1		; Up the number by one
	and	#$3F		; Do this Mod $3f!
	sta	n		; Stuff the number where it belongs
	clc			; Clear carry again
	lda	tpak		; Increment lo byte
	adc	#1		;	total packet count
	sta	tpak		;		...
	lda	tpak+1		; Do H.O. byte
	adc	#0		;		...
	sta	tpak+1		;		...
	pla			; Restore the AC
	rts			;	and return
 
;ctrl-l;
;	Prcerp - Process error packet. Moves the Remote Kermit error
;	text into a save area, notes that there was an error received
;	from the remote Kermit in Errcod (set H.O. bit), and displays
;	the text on the screen.
;
 
prcerp:	lda	ptype		; Reload the packet type
	cmp	#'E'		; Is it an error packet?
	beq	prcer1		; Yes, continue processing
	rts			; No, return
prcer1:	lda	#<pdbuf		; Set up from-address
	sta	kerfrm		;		...
	lda	#>pdbuf		;		...
	sta	kerfrm+1	;		...
	lda	#<errrkm	; Set up the to-address
	sta	kerto		;		...
	lda	#>errrkm	;		...
	sta	kerto+1		;		...
	ldy	pdlen		; Get packet data length
	sty	kwrk01		; Store for the copy routine
	lda	#0		; Start by storing a null at the end
	sta	(kerto),y	;		...
	jsr	kercpy		; Copy the error text
	lda	errcod		; Set the bit in the error code
	ora	#eprflg		;	saying that the remote Kermit sent us
	sta	errcod		;	an error packet.
	jsr	prcrlf		; [jrd] leading crlf please
	ldx	#<errrkm	; Finally, display the error packet
	ldy	#>errrkm	;		...
	jsr	prstr		; Print string
	jsr	prcrlf		; Make it look neat, add a crlf
	rts			; Return to caller
 
;ctrl-l;
;	Gobble - snarfs a line of characters from the port up to
;	the receive end-of-line character. If it sees a keyboard
;	interupt, it punts and does not skip.
;
 
gobble:
	jsr	openrsm		; ensure rs port open
	lda	#0		; Zero the index pointing to end of line buffer
	sta	pdtend		;		...
; zzz?	sta	ndx		; Make sure no unwarranted keyboard intrpt
gobb:	jsr	getc		; Get a character
	 jmp	gobb2		; Got a keyboard interupt
	lda	char		;[31]
	cmp	#soh		; Is it a start-of-header?
	bne	gobb		; No, flush until first SOH
	jmp	gobbst		; Ok, now we can start
gobb0:	jsr	getc		; Get a character
	 jmp	gobb2		; Got a keyboard interupt
	lda	char		;[31]
	cmp	#soh		; If this not an SOH
	bne	gobb1		;	continue here
	tax			; Hold the character here
	lda	#0		; Rezero the index pointing to end of buf
	sta	pdtend		;		...
	txa			; Get the SOH back
	jmp	gobbdb		; Go stuff the character in the buffer
gobb1:	cmp	reol		; Is it the end-of-line character?
	beq	gobb3		; Yes, finish up
gobbst:	ldx	pdtend		; Get the index we need
gobbdb:	sta	plnbuf,x	; Stuff the character at the buffer
	inc	pdtend		; Increment the index once
	jmp	gobb0		; Loop for another character
gobb2:	rts			; Just return, no skip
gobb3:	ldx	pdtend		; Get end pointer again
	sta	plnbuf,x	; Store the End-of-line before we leave
	lda	#0		; Zero the index, leave eob ptr where it is
	sta	pdtind		;		...
	jmp	rskp		; Return with a skip!
 
;ctrl-l;
;	Getplc - gets a character from the port line buffer and
;	returns it. If the buffer is empty, it returns without
;	skipping.
;
 
getplc: ldx	pdtind		; Get the current index
	cpx	pdtend		; Less than the end buffer pointer?
	bmi	getpl1		; If so, go return the next character
	rts			; Return without a skip
getpl1: lda	plnbuf,x	; Get the next character from the buffer
	inc	pdtind		; Up the index once
	jmp	rskp		; Return with a skip!
 
;
;
;	Putplc - puts a character to the port line buffer.
;
 
putplc: ldx	pdtind		; Get the current index
	inx			; Check if we are at end of buffer
	bne	putpl1		; No, continue
	rts			; Return without a skip
putpl1: dex			; Set index back to what it was
	sta	plnbuf,x	; Get the next character from the buffer
	inc	pdtind		; Up the index once
	rts			; Return
 
;ctrl-l;
;	Getc - skip returns with a character from the port or does
;	a normal return if a key from the keyboard is received first.
;	If it skips, the character from the port is returned in the
;	AC.
;
 
getc:	jsr	getkey		; Try and get a keyboard character
;	bne	getcy		;[] Got one
	bcc	getcy		; [jrd] got one
	jmp	getc1		;[] None available, try port
getcy:	lda	char		;[31] Get the character read
	and	#$7F		; Shut H.O. bit
	cmp	#ctrlx		;[43] Was it an 'abort current file' interrupt?
	beq	getc3		; Yes
getc2:	cmp	#ctrly		;[43] Was it 'abort file group' interrupt ?
	bne	getc0		;[43] Nope, continue
getc3:	lda	#errfta		; Error code for 'file trans abort'
	sta	errcod		; Stuff it here
	jsr	closef		;[28] Close the current file
abo0:	lda	#0		;[43] Send a 'Z' packet with a 'D' field
	sta	numtry		;[43]
	sta	tpak		;[43]
	sta	tpak+1		;[43]
	lda	#<pdbuf		;[43] Get the address of the packet buffer
	sta	kerbf1		;[43]   and save it for Spak
	lda	#>pdbuf		;[43]		...
	sta	kerbf1+1	;[43]		...
abo1:	lda	numtry		;[43] Fetch the number of tries
	cmp	maxtry		;[43] Have we exceeded Maxtry?
	bmi	abo3		;[43] Not yet, go send the packet
abo2:	ldx	#<ermesc	;[43] Yes, give an error message
	ldy	#>ermesc	;[43]		...
	jsr	prstr		;[43]		...
	jsr	prcrlf		;[43]		...
	jmp	abo4		;[43]	and restart kermit
abo3:	inc	numtry		;[43] Increment the number of tries for packet
	lda	#0		;[43] Make it packet number 0
	sta	pnum		;[43]		...
	lda	#1		;[43] Data length is only 1
	sta	pdlen		;[43]		...
	lda	#'D'		;[43] The 'Discard' command
	sta	pdbuf		;[43] Put that in first character of buffer
	lda	#'Z'		;[43] EOF command packet type
	sta	ptype		;[43]		...
	jsr	flshin		;[43] Flush the RS232 buffer
	jsr	spak		;[43] Send the packet
	;jsr	rpak		;[43] Try to fetch an ACK
	;cmp	#true		;[43] Did we receive successfully?
	;bne	abo1		;[43] No, try to send the packet again
	;lda	ptype		;[43] Get the type
	;cmp	#'Y'		;[43] An ACK?
	;bne	aboce		;[43] No, go check for error
	jmp	abo4		;[43] Yes, restart Kermit
aboce:	;cmp	#'E'		;[43] Error packet?
	;bne	abo1		;[43] Nope, resend packet
	;jsr	prcerp		;[43] Go display the error
 
abo4:
;	ldx	kerosp		; Get the old stack pointer back
;	txs			; Restore it
	jmp	kermit		; Warmstart kermit
 
getc0:  lda	#0		;[EL] And reset the strobe
; zzz?	sta	ndx		;[EL]		...
	rts			; Keyboard interupt, return
getc1:	jsr	timerexp	;[49] Have we timed out?
	 jmp	getc0		;[49] Yes return
	jsr	getrs		; No, Check the port
	beq	getcn		;[] Got a character
	jmp	getc		;[] No char, go back to top of loop
getcn:	lda	char		;[31] Get the character read
	jmp	rskp		;	and return skip!
 
;ctrl-l;
;	Prson - parses an 'on' or an 'off' keyword and passes
;	the result back to the calling routine in the x-index
;	register. If there is an error, it pops the return
;	address off the stack and transfers control to kermt2
;	to issue the error message.
;
 
prson:  lda	#<oncmd		; Command table address
	sta	cminf1		;		...
	lda	#>oncmd		;		...
	sta	cminf1+1	;		...
	lda	#<shon		; Set up default string for parse
	sta	cmdptr		;		...
	lda	#>shon		;		...
	sta	cmdptr+1	;		...
	ldy	#cmfdff		; Show there is a default
	lda	#cmkey		; Code for keyword
	jsr	comnd		; Go do it
	 rts			; The command was not recognized
	 nop
	 nop
	jmp	rskp		; Good, skip return
 
;
;	prcfm - parses for a confirm, then transfers control directly
;	to the top of the main loop
;
 
prcfm:  lda	#cmcfm		; Load token for confirm
	jsr	comnd		; Parse a confirm
	 jmp	kermt3		; No confirm, give an error
;	lda	#cr		; Print a crlf
;	jsr	cout		;		...
	jsr	prcrlf
	rts			; Return
 
;
;	Pron - checks the value in the AC and prints either 'ON' or
;	'OFF'. (on=1, off=0).
;
 
pron:	cmp	#on		; Should we print 'on'?
	bne	pron1		; No, go print 'off'
	ldx	#<shon		; Point to the 'on' string
	ldy	#>shon		;		...
pron0:  jsr	prstr		; Print it
	jsr	prcrlf		; Add a crelf at the end
	rts			; And return
pron1:  ldx	#<shoff		; Point to the 'off' string
	ldy	#>shoff		;		...
	jmp	pron0		; Go print it
 
;
;	Clrfcb - clears the area FCB1 so the filename placed there
;	will not be corrupted.
;
 
clrfcb:	ldx	#mxfnl		; Load max filename length
	lda	#ATEOL		; [jrd] atari wants file name terminated by EOL
clrfc1:	sta	fcb1,x		; Stuff the space
	dex			; Decrement our pointer
	bpl	clrfc1		; Not done, go back
	rts			; Return
 
;
;	Kercpy - copies the string pointed to by Kerfrm to the
;	block of memory pointed to by Kerto for Kwrk01 characters.
;
 
kercpy:	ldy	kwrk01		; Get the length of the string
kerclp:	dey			; One character less
	bmi	kercrt		; If this went negative, we're done
	lda	(kerfrm),y	; Get the next character
	sta	(kerto),y	; And put it where it belongs
	jmp	kerclp		; Go back for next char
kercrt:	rts			; Job is done, return
 
;
;	cmd2fcb:	Command buffer to fcb copier.
;			Expects x,y pointing to buf,
;			size in A.  (that's what comes back from
;			the command parser when entering filenames)
;			Copies that string into fcb1, terminated
;			with an ATEOL.  Returns size in A
;
cmd2fcb:
	stx	source		; set source addr
	sty	source+1	; ...
	pha			; save size so we can return it
	ldy	#0		; zero source idx
	tax			; get size in x
	beq	cmd2fcb2	; if zero left, exit
cmd2fcb1:
	lda	(source),y	; get a byte
	sta	fcb1,y		; stuff it in
	iny			; bump idx
	dex			; dec size
	bne	cmd2fcb1	; back for more
cmd2fcb2:
	lda	#ATEOL		; terminate it
	sta	fcb1,y
	pla			; get original size back
	rts			; done!

;
;	Kerflm - fills the buffer pointed to by Kerto with the
;	character in kwrk02 for Kwrk01 characters.
;
 
kerflm:	ldy	kwrk01		; Get the length of the string
kerflp:	dey			; One character less
	bmi	kerflr		; If this went negative, we're done
	lda	kwrk02		; Get the fill character
	sta	(kerto),y	; And put it in the next position
	jmp	kerflp		; Go back to do next char
kerflr:	rts			; Job is done, return
 
;ctrl-l
;
;	Prchr - takes a character from the AC and prints it. It
;	echos control characters as '^<chr>', and wierd atari chars
;	in their graphics form.
;
 
prchr:	pha			; [jrd] save original
	and	#$7F		; [jrd] for testing control-ness
	cmp	#$20		; Less than escape??
	bpl	prchr1		; If not, continue
	lda	#'^'		; Load the up-arrow for cntrl characters
	jsr	cout		; Print the character
	pla			; Get the character back
	clc			; Clear carry for add
	adc	#$40		; Put this in the alphabetic range
	jmp	prchr		; [jrd] tail recurse...
;
prchr1: lda	#esc		; [jrd] 'quote' it with esc, in case
				;  of nasty screen hacking chars
	jsr	cout		; put that
	pla			; get original back
	jmp	cout		;	and print it

 
;
;	Genmad - takes a message base, offset and size and calculates
;	the address of the message leaving it in the X and Y registers
;	ready for a call to PRSTR. The size and offset are taken from
;	the stack and the base address is found in kermbs.
;
 
genmad: pla			; Get return address
	sta	kerrta		;	and save it till later
	pla			;
	sta	kerrta+1	;
	pla			; Get message offset
	tax			; Hold it here for a while
	pla			; Get the message length
	tay			;	and put it here
	lda	#0		; H.O. byte of message offset for mul16
	pha			;
	txa			; L.O. byte of message offset
	pha			;
	lda	#0		; H.O. byte of message size for mul16
	pha			;
	tya			; L.O. byte of message size
	pha			;
	jsr	mul16		; Calculate the actual offset in table
	pla			; Get L.O. byte of result
	clc			; Clear the carry for addition
	adc	kermbs		; Add the L.O. byte of the base address
	tax			; Put it in X for the return
	pla			; Get the H.O. byte
	adc	kermbs+1	; Add the H.O. byte of the base address w/carry
	tay			; Stuff it here for the return
	lda	kerrta+1	; Replace the return address on the stack
	pha			;		...
	lda	kerrta		;		...
	pha			;		...
	rts			; Return
 
 
;ctrl-l.SBTTL 	Video Support Routines

;
;	Prttab - Go to next tab stop
;
 
prttab:	sec			;[26] Get the cursor coordinates
	jsr	ploth		;[26]		...
;	tya			;[26] Put the column in A
	txa			;[26] Put the column in A
	lsr	a		;[26] Divide column by 8
	lsr	a		;[26]		...
	lsr	a		;[26]		...
;	tay			;[26] Add one
	tax			;[26] Add one
;	iny			;[26]		...
	inx			;[26]		...
;	tya			;[26]		...
	txa			;[26]		...
	asl	a		;[26] Multiply by 8
	asl	a		;[26]		...
	asl	a		;[26]		...
;	tay			;[26] Put the new column in Y
	tax			;[26] Put the new column in Y
;	cpy	#80
	cpx	#80
	lda	scrtype
	bne	prttab1		;[37]
;	cpy	#40		;[26] Is new column number 40?
	cpx	#40		;[26] Is new column number 40?
prttab1: bcs	prttab2		; at leftmost edge?
	jsr	ploth		; carry already clear
	rts
prttab2: jsr	scrcr		; at leftmost edge. perform a cr and lf
	jsr	scrlf
	rts
 
;
;	Ploth - Plot the cursor position
;
;	Input: Carry set to read cursor position
;	       Y-reg cursor y position			(if carry is set)
;	       X-reg cursor x position			(if carry is set)
;
;	Output:Y-reg is cursor y position		(if carry is clear)
;	       X-reg is cursor x position		(if carry is clear)
;
;	Registers Destroyed:  None			(if carry is set)
;
 
ploth:	bcc	ploth1
	ldy	ROWCRS
	ldx	COLCRS
	cpx	#80		; zzz check 40 col too
	bcc	ploth0
	ldx	#79
ploth0:	clc
	rts

ploth1:
;	tya			; swap a-reg and x-reg
;	pha
;	txa
;	tay
;	pla
;	tax
;	jsr	scrplt
	jmp	scrplt
;	rts
 
;	Print (X) spaces
; obsolete
;prbl2:  stx	savex		;[DD] Save X
;	lda	#sp		;[DD] Get a space
;	jsr	cout		;[DD] Print it
;	ldx	savex		;[DD] Get back X
;	dex			;[DD] Decrement it
;	bne	prbl2		;[DD] If not 0, do more
;	rts			;[DD] Return
 
; Print a reg as 2 hex nibbles
 
prbyte: 			;[DD] Output byte in hex
by2hx:  pha			;[DD] Save byte
	lsr	a		;[DD]
	lsr	a		;[DD]
	lsr	a		;[DD]
	lsr	a		;[DD]
	jsr	ny2hx	     	;[DD] High nyble
	tax		     	;[DD] to x
	pla		     	;[DD] Get back
	and	#$0F	     	;[DD] Low nyble
	jsr	ny2hx		;[DD] Translate to Hex
	pha			;[DD] Save low nyble
	txa			;[DD] Get high nyble
	jsr	cout		;[DD] Print it
	pla			;[DD] Get back low nyble
	jmp	cout		;[DD] Print and return
 
; Translate nyble to hex
 
ny2hx:	clc			;[DD]
	adc	#$F6		;[DD]
	bcc	ny2h2		;[DD]
	adc	#6		;[DD]
ny2h2:  adc	#$3A		;[DD]
	rts			;[DD]
 
; Print hex of A,X
; obsolete 
;prntax: stx	savex		;[DD] Save X
;	jsr	prbyte		;[DD] Print A first
;	lda	savex		;[DD] Get X into A
;	jsr	prbyte		;[DD] Print that next
;	rts			;[DD] Return
 
;	Prntad - Print a number in base 10.  Leading zeros are skipped.
;
;	Input: A,X - Number to be printed
;
;	Registers Destroyed:	A,X,Y
;
;	This routine works by repeated subtraction.  10^X is subtracted
;	until the result would be negative.  After each subtraction, Y
;	is incremented. Y starts out at '0.  Thus, Y is the ascii value
;	of the next digit.

prntad:
	stx	decnum		; [54] Save the number to print
	sta	decnum+1	; [54]

	ldx	#4		; [54] Up to 5 digits (0..4)
prntad1: lda	decnum		; [54] Compare with 10^x
	cmp	tens1,x		; [54]
	lda	decnum+1	; [54]
	sbc	tens2,x		; [54]
	bcs	prntad2		; [54] If greater, found first nonzero digit
	dex			; [54] Skip the leading zero
	bne	prntad1		; [54] Go test the next digit, unless last

prntad2: ldy	#'0'		; [54] Y is the ascii value to print
prntad3: lda	decnum		; [54] Compare with 10^x
	cmp	tens1,x		; [54]
	lda	decnum+1	; [54]
	sbc	tens2,x		; [54]
	bcc	prntad4		; [54] Result would be negative.

	lda	decnum		; [54] Now subtract 10^x
	sbc	tens1,x		; [54] carry is already set
	sta	decnum		; [54]
	lda	decnum+1	; [54]
	sbc	tens2,x		; [54]
	sta	decnum+1	; [54]
	iny			; [54] Keep track of the value of this digit
	bne	prntad3		; [54] Always taken

prntad4: txa			; [54] Save X
	pha			; [54]
	tya			; [54] Print the character in Y
	jsr	cout		; [54]
	pla			; [54] Restore X
	tax			; [54]
	dex			; [54] Print the next digit.
	bpl	prntad2		; [54]
	rts
tens1:	.byte	<1,<10,<100,<1000,<10000 ; [54] Powers of ten for prntad
tens2:	.byte	>1,>10,>100,>1000,>10000

;
;	prntadnl	prntad followed by prcrlf
;
prntadnl:
	jsr	prntad
	jmp	prcrlf

;
;	Cout - Print byte to screen
;
;	Input:	A - character to be printed
;
;	Output:
;
;	Registers Destroyed:	A,X,Y
;
 
cout:
; superceeded by sputch
;	jmp	sputch		; maybe not?
;
;	sta	source		; Save A-reg
;	pha			; save A-reg again
;	txa
;	pha			; save X-reg
;	tya
;	pha			; save Y-reg
;	lda	source
;	jsr	scrput		; print the character
;	pla			; restore Y-reg
;	tay
;	pla			; restore X-reg
;	tax
;	pla			; restore A-reg
;	rts
	jmp	scrput		; why go to all that trouble?

;	Rdkey - Read keyboard until a byte appears
;
;	Input:
;
;	Output:
;
;	Registers Destroyed:
;
 
rdkey:	jsr	getkey		;[DD] Try and get a keyboard byte
;	bne	rdret		;[DD] None, try again
	bcc	rdret		; [jrd] there's one, return it
	jsr	scrfls
	jmp	rdkey		;[]
rdret:	rts			;[DD]		...
 
;
;	Getkey - Get byte from keyboard, blink cursor
;
;	Input:	None
;
;	Output: Character read in CHAR
;		Carry set   -> character read
;		      clear -> no character read
;
;	Registers Destroyed: A,X,Y
;
 
getkey:	lda	CH		; any keys pending?
	cmp	#$FF
	beq	getkey1		; nope, return 0
;	jsr	kgetch		; get one the old way
	jsr	kbdget		; get one the new way
	bcs	getkey		; false alarm
getrt:	sta	char		;[31] Store it
	cmp	#0		; [jrd] set other flags
	clc
	rts			;[DD]		...
getkey1: lda	#0		; return nothing
	sec
	rts
;
;----------------------------------------------------------------
;	Bell - flash border color - will be terminated next cursor blink
;
;	Input:	None
;
;	Output: None
;
;	Registers Destroyed: None
;
 
bell:	pha			;[EL] Save the AC
beephi:	lda	#$3A		;some color or other
	jmp	beep		;[33]		...
beeplo:	pha			;[33] Save the AC
	lda	#$AA		;a different color
beep:
;
; this seems to completely fry POKEY; fucks over serial baud rate
;
;	sta	AUDF4		;[EL]		...
;	lda	#$E8		; Pure tone, medium volume
;	sta	AUDC4
;	lda	#$0f		;[EL] Select fast attack, slow decay
;	sta	attdec		;[EL]		...
;	lda	#$12		;[EL] Select sustain ...
;	sta	susrel		;[EL]		...
;	lda	#6		;[EL] Select not-too-loud volume
;	sta	vol		;[EL]		...
;	lda	#$21		;[EL] Select sawtooth wave
;	sta	wave		;[EL]		...
	sta	COLOR4		; flash it
;	jsr	rdtim		; remember when the flash started
	lda	RTCLOK+2
	sta	lpcnt		;[EL]		...
	pla			;[EL] Restore the AC
	rts			;[EL] Return
 
;ctrl-l.SBTTL	RS232 Support Routines

;	Debugging code.
;
;	Dump IOCB if ICSTA < 0
;
dbgstr1: .byte	ATEOL,"RS error ",0
dbgstr2: .byte	" -> ",0
debugrs:
	lda	ICSTA,X		; get status
	bpl	dbgrs9		; positive is ok
	pha			; save it temporarily
	lda	ICCOM,X		; get op that failed
	pha			; save that too
	ldx	#<dbgstr1	; print debug msg
	ldy	#>dbgstr1
	jsr	prstr
	pla			; get op back
	jsr	prbyte		; print that
	ldx	#<dbgstr2	; print second part of str
	ldy	#>dbgstr2
	jsr	prstr
	pla			; get err back
	jsr	prbyte
	jsr	prcrlf		; end line
	ldx	#comchan	; point at rs232 iocb again
dbgrs9:	rts			; and go home
	
;
;	Openrsm - Open the channel if it wasn't already
;	Openrs - Open the RS-232 Channel
;
;	Input:	RS232 Parameters in x36ax1,x38ax1
;
;	Ouput:
;
;	Registers Destroyed: A
;
openrsm:
	lda	comopen		; already open?
	beq	openrs		; no, go open it for real
	rts
openrs:	
	jsr	closers		; close it first
	ldx	#comchan	; [jrd] rs232 port iocb
	lda	#<comname	; [jrd] name lo
	sta	ICBAL,X
	lda	#>comname	; [jrd] name hi
	sta	ICBAH,X
 	lda	#$0D		; [jrd] mode in+out+concurrent
	sta	ICAX1,X
	lda	#0
	sta	ICAX2,X
	sta	ICBLL,X		; zap buf len
	sta	ICBLH,X
	lda	#OPEN
	sta	ICCOM,X		; open please
	jsr	CIOV		; do it.
;	jsr	debugrs		; zzz debug
;
; Action code from that kermit
;  CIOV(2, 34, 0, 0, 192+48, 0)
;
	lda	#34		; xio 34, set cts, dtr etc
	sta	ICCOM,X
	lda	#192+48+3	; DTR on, RTS on, XMT on
	sta	ICAX1,X
	lda	#0
;	sta	ICBLL,X
;	sta	ICBLH,X
	sta	ICBAL,X
	sta	ICBAH,X
;	sta	ICAX2,X
	jsr	CIOV
;	jsr	debugrs		; zzz debug
;
;  CIOV(2, 38, 0, 0, 32+PARITY*5, 0)
;
	lda	#38		; xio 38, translation and parity
	sta	ICCOM,X
;	lda	#32		; no translation, no parity
	lda	x38ax1
	sta	ICAX1,X
;	lda	#0
;	sta	ICBLL,X
;	sta	ICBLH,X
;	sta	ICBAL,X
;	sta	ICBAH,X
;	sta	ICAX2,X
	jsr	CIOV
;	jsr	debugrs		; zzz debug
;
;  CIOV(2, 36, 0, 0, 8+baud, 0)
;
	lda	#36		; xio 36, baud rate
	sta	ICCOM,X
	ldy	baud		; get baud value
	lda	bdval,y		; get real parameter for port
	sta	ICAX1,X
;	lda	#0
;	sta	ICBLL,X
;	sta	ICBLH,X
;	sta	ICBAL,X
;	sta	ICBAH,X
;	sta	ICAX2,X
	jsr	CIOV
;	jsr	debugrs		; zzz debug
;
;  CIOV(2, 40, 0, 0, 0, 0)
;
	lda	#40		; XIO 40, start concurrent IO
	sta	ICCOM,X
	lda	#0
	sta	ICBLL,X
;	sta	ICBLH,X
	sta	ICBAL,X
;	sta	ICBAH,X
;	sta	ICAX1,X
	sta	ICAX2,X
	lda	#$06		; use page 6 as iobuf
	sta	ICBAH,X
	lda	#$01		; size 256
	sta	ICBLH,X
	lda	#$0D		; value from 850 man, p62.  must be 0D?,
	sta	ICAX1,X		;  or any non-zero?
	jsr	CIOV
	jsr	debugrs		; zzz debug
;
	lda	#1
	sta	comopen		; com is now open for business
	jsr	comsta		; refresh pending byte count
; [jrd]

;
;	Alocrs - Subroutine - allocate the RS232 buffers
;
;	Input:	Buffer locations in RSOUT,RSIN
;
;	Output:
;
;	Registers Destroyed: A
;
 
alocrs:
; all obsolete ?
;	lda	#<rsout		;[24] Allocate the RS-232 buffers
;	sta	robuf		;[24]		...
;	lda	#rsout&$ff00^	;[24]		...
;	sta	robuf+1		;[24]		...
;	lda	#<rsin		;[24]		...
;	sta	ribuf		;[24]		...
;	lda	#rsin&$ff00^	;[24]		...
;	sta	ribuf+1		;[24]		...
	rts			;[24] Return
 
;
; Close comm port.  Added by jrd
;
closers:
	lda	comopen		; open?
	beq	closer9		; nope, just return
	lda	#0
	sta	comopen		; remember that it's closed
	sta	compend		; no more pending
	sta	compend+1
	ldx	#comchan
	jmp	closec		; just ignore status etc
closer9:
	rts
;
;	comsta - Update status of the comm port
;
;	updates pending byte count in compend.
;	Trashes regs
;
comsta:
	ldx	#comchan	; [jrd] rs232 iosb please
	lda	#STATIS		; [jrd] status request, returns bytes pending
	sta	ICCOM,X
	jsr	CIOV
	jsr	debugrs		; zzz debugging
	lda	DVSTAT		; get device status
	sta	comstat		; save for future reference
	lda	DVSTAT+1	; get byte count pending
	sta	compend
	lda	DVSTAT+2
	sta	compend+1
	lda	comstat		; now check error bits
	beq	comsta9		; none, ok
	lda	debug		; oops!  error.  Display it?
	beq	comsta8		; nope, just whack pending count
	ldx	<comstm		; display the message
	ldy	>comstm
	jsr	prstr
	lda	comstat		; display the status
	jsr	prbyte
	jsr	prcrlf
comsta8:
	lda	#0
	sta	compend		; force a refresh next time
	sta	compend+1
comsta9:
	rts
comstm:	.byte	"Com err ",0
;
;	Getrs - Get byte from rs232 port
;
;	Input:	
;
;	Output:	Character read in CHAR
;	Z set if character was read
;
;	Registers Destroyed: A,X,Y
;

getrs:
	jsr	flowco		;[24] Do flow control if necessary
	lda	suspend		;[24] Is RS-232 reading suspended?
	bne	getr3		; Yes, 
getr2:
	lda	compend		; [jrd] anything pending?
	cmp	#0
	bne	getr2a		; yup, go get one
	lda	compend+1	; ?
	cmp	#0
	bne	getr2a
	jsr	comsta		; go see if there's any since we looked
	lda	compend		; now check byte count again
	cmp	#0
	bne	getr2a
	lda	compend+1
	cmp	#0
	bne	getr2a		; there's something pending, go get it
	lda	#$FF		; bogus value so we can...
	cmp	#0		;  return NE
	rts
;
getr2a:				; dec pending count
	lda	compend		; save it
	dec	compend		; dec lo byte
	cmp	#0		; was it 0?
	bne	getr2b		; no, skip hi byte
	dec	compend+1	; yes, dec hi byte
getr2b:	ldx	#comchan	; now go read one	
	jsr	chrin		; [jrd] get one
	ldx	wrdsiz		; get word size value
	beq	getr2c		; 8-bit = 0
	and	#$7F		; if 7 bit, make sure
getr2c:
	sta	char		;[31] Store it here
	sty	stat		; [jrd]
	jsr	rserrs		;[33] Check for RS232 errrors
; bogus	bne	getr3		;[33] If error, return no byte
;	lda	stat		;[33] Check stat to see if byte was read
;	cmp	#1		; [jrd] has effect of setting Z if 1
	lda	#0		; debugging, always return Z
	cmp	#0
getr3:	rts			;[DD] Return
 
;
;	Rserrs - Check for RS232 errors
;
;	Input:	Status in STAT
;
;	Output:
;
;	Registers Destroyed: A
;
; Atari 850 RS232 error bits that show up in DVSTAT
;
RS850FE	=	$80		; Framing error
RS850OE	=	$40		; Receive overrun
RS850PE	=	$20		; Parity error
RS850OV	=	$10		; Buffer overflow
RS850IL	=	$08		; Illegal option combination
RS850NR	=	$04		; Device not ready
RS850BE	=	$02		; Data block error, ie 850 didn't xcv right
RS850CE	=	$01		; Command error
;
; corresponding messages.  All 8 bytes, for ease of indexing.
;
rsetxt:
	.byte	"Framing",0
	.byte	"Overrun",0
	.byte	"Parity ",0
	.byte	"Buf Ofl",0
	.byte	"Bad Opt",0
	.byte	"Dev NR ",0
	.byte	"850 err",0
	.byte	"Cmd err",0
;
rseokt:	.byte	"RS ok  ",0
rserrtim: .byte	0
;
rserrs:	lda	comstat		; [jrd] Get the status from last STATUS call
	beq	erret		; no bits set, go home
;
; Display err in stat line.
;
	ldx	#0		; error bit number, left to right
rserrs1:
	rol	A		; shift out a bit
	bcs	rserrs2		; found one, go display it
	inx			; bump count
	cpx	#7		; done?
	bne	rserrs1		; nope, try again
	jmp	erret
rserrs2:
	txa			; get errnum into A
	asl	A
	asl	A
	asl	A		; * 8
; zzz fix this to work with atari screen, too
	clc
	adc	#<rsetxt	; add base of err msgs
	pha			; save for a bit
	lda	#>rsetxt	; get hi addr
	adc	#0		; add carry
	tay			; into Y for statline rtn
	pla			; get lo addr back
	ldx	#32		; offset in stat line
	jsr	slput		; show it
	jsr	beeplo		;[33] Error, Feep!
	lda	RTCLOK+2	; get time displayed
	adc	#128		; set for 2 sec or so
	ora	#1		; make sure nonzero
	sta	rserrtim	; save it
	rts			; done
erret:
	lda	rserrtim	; zero?
	beq	erret8
; this isn't very accurate, but it'll do
	sbc	RTCLOK+2	; subtract time value
	bne	erret9		; not ready to clear error msg
erret8:
	sta	rserrtim	; zero flag
	lda	#<rseokt	; get 'ok' msg
	ldy	#>rseokt
	ldx	#32		; offset in stat line
	jsr	slput
erret9:
	rts			;[33]
 
;
;	Flowco - perform RS-232 flow control
;
;	Input:
;
;	Output:
;
;	Registers Destroyed: A,X
;
 
flowco:	lda	flowmo		;[24] Get the flow control mode switch
	cmp	#on		;[24] Is it on?
	bne	flowre		;[24] No
; all this stuff removed cause there's no good way to do it on atari
; 	lda	shflag		;[24] Check commodore key
;	and	#$02		;[24] Is it depressed?
;	beq	nocomm		;[24] No
;	lda	commflg		;[24] Was it depressed before
;	bne	flowch		;[24] Yes, ignore it
;	inc	commflg		;[24] Set commodore key flag
	lda	suspend		;[24] Currently suspended?
	beq	flowch		;[24] No
	jsr	comsta		; [jrd] yes, update pending count
;	lda	#0		;[24] Clear suspend flag
;	sta	suspend		;[24]		...
;	beq	flowch		;[24]
;notsus:	inc	suspend		;[24] Set suspend flag
;	bne	flowch		;[24]
;nocomm:	sta	commflg		;[24] Clear commodore key flag
 
flowch:
;	lda	ridbe		;[24] Compute number of chars
;	sec			;[24]	in RS-232 buffer
; 	sbc	ridbs		;[24]		...
;	lsr	a		;[24] Divide count by 2 for accurate check
	ldx	fxoff		;[24] Has an xoff already been sent
	bne	itsoff		;[24] Yes
	lda	compend+1	; more than 256?!?
	bne	flowch1		; yup, shut it off
	lda	compend		; check lo half
	bmi	flowch1		; hi bit set, shut it off	
	cmp	#50		;[24] Number chars in buffer reached 50?
	bcc	flowre		;[24] No - no flow control necessary yet
flowch1:
	jsr	sxoff		;[24] Send an xoff
	rts			;[24] Return
itsoff: lda	compend+1	; if > 256, leave it off
	bne	flowre
	lda	compend
	cmp	#20		;[24] Has backlog dropped to 20 or less?
	bcs	flowre		;[24] No - leave input suspended
	jsr	sxon		;[24] Send an xon
flowre:	rts			;[24] Return

;
;	Flshin - Flush the RS232 input buffer
;
;	Input:
;
;	Output:
;
;	Registers Destroyed: A
 
flshin:	
	txa			; save some regs
	pha
	tya
	pha
flshin1:
	jsr	getrs		;[25] Get from RS-232 buffer
	beq	flshin1		;[33] No, get more
	pla
	tay
	pla
	tax
	rts			;[25] Yes, finish
 
;
;	Putrs - Send byte to RS232
;
;	Input: Byte in A
;
;	Output:
;
;	Registers Destroyed:
;
	.byte	0		; [jrd] save x 
	.byte	0		; [jrd] save y
putrs:	
	stx	putrs-2		; [jrd] save x
	sty	putrs-1		; [jrd] save y
	ldx	#comchan	; [jrd]
	jsr	chrout		;[DD] Send the character
	ldx	putrs-2		; [jrd] get x back
	ldy	putrs-1		; [jrd]  "  y
	rts			;[DD] Return

 
;
;	Sbreak - Send a break signal
;
 
sbreak:	jsr	closers		; yes, we must
	ldx	#comchan
	lda	#192+48+2	; DTR on, RTS on, XMT off
	sta	ICAX1,X
	lda	#34		; xio 34, set cts, dtr etc
	sta	ICCOM,X
	lda	#<comname	; [jrd] name lo
	sta	ICBAL,X
	lda	#>comname	; [jrd] name hi
	sta	ICBAH,X
	lda	#0
	sta	ICAX2,X
	jsr	CIOV		; go do it
	ldy	#250		;[DD][28] Delay 250 ms.
sbdl1:  ldx	#250		;[DD]		...
sbdl2:  dex			;[DD] Inner loop 1 ms.
	bne	sbdl2		;[DD]		...
	dey			;[DD] Outer loop
	bne	sbdl1		;[DD]		...
	jsr	openrs		; re open it

;
; diddle the state of the xmit line.  Expects A to contain
; 2 for send-space, 3 for send-mark.
;
hacktx:	jmp	CIOV
 
;
;	Subroutine - send out ^Q (xon) to remote host
;
 
sxon:	lda	#0		;[24] Clear xoff flag
	sta	fxoff		;[24]		...
	lda	#$11		;[24] Transmit ^Q
	bne	xcom		;[24]		...
 
;
;	Subroutine - send out ^S (xoff) to remote host
;
 
sxoff:	lda	#5		;[24] Set xoff flag
	sta	fxoff		;[24][32]	...
	lda	#$13		;[24]	then, transmit ^S
xcom:	jsr	putrs		;[24]		...
	jsr	updstat		; [jrd] clean stat line
	rts			;[24] Return
 
;ctrl-l;
;
;   Cva2s - Convert ASCII to Speedscript (word processor)
;
;	Input:	Character in KERCHR
;
;	Output:	Converted character in KERCHR
;
;	Registers Destroyed: A
;
; None of this appears to be necessary on Ataris -- jrd
; 
;cva2s:  lda	kerchr		;[DD]
;	and	#$7F		;[DD]
;	cmp	#cr		;[DD]
;	bne	cva2s1  	;[DD] Check cr
;	lda	#$1F		;[DD]
;cva2s1: cmp	#$61		;[DD]
;	bcc	cva2s2		;[DD]
;	cmp	#$7B		;[DD]
;	bcs	cva2s2		;[DD]
;	and	#$1F		;[DD] Convert lower case
;cva2s2: cmp	#$5B		;[DD]
;	bcc	cva2s3		;[DD]
;	cmp	#$5F		;[DD]
;	bcs	cva2s3		;[DD]
;	and	#$1F		;[DD]
;cva2s3: sta	kerchr		;[DD]
;	rts			;[DD]
; 
;;  Convert Seedscript (word processor) to ASCII
; 
;cvs2a:  lda	kerchr		;[DD]
;	and	#$7F		;[DD]
;cvs2a1: cmp	#$1B		;[DD]
;	bcs	cvs2a2  	;[DD] If <$1b
;	ora	#$60		;[DD] Convert to lc
;cvs2a2: cmp	#$1F		;[DD]
;	bcs	cvs2a3		;[DD]
;	ora	#$40		;[DD]
;cvs2a3: bne	cvs2a4 		;[DD] If =$1f
;	lda	#cr		;[DD] cr
;cvs2a4:	sta	kerchr		;[DD]
;	rts			;[DD]
 
;ctrl-l.SBTTL	Spar and Rpar routines
 
;
;	Spar - This routine loads the data buffer with the init parameters
;	requested for this Kermit.
;
;		Input:  NONE
;
;		Output: @Kerbf1 - Operational parameters
;
;		Registers destroyed:	A,Y
;
 
spar:	ldy	#0		; Clear Y
	sty	datind		; Clear datind
	lda	rpsiz		; Fetch receive packet size
	clc			; Clear the carry flag
	adc	#$20		; Characterize it
	sta	(kerbf1),y	; Stuff it in the packet buffer
	iny			; Increment the buffer index
	lda	rtime		; Get the timeout interval
	clc			;		...
	adc	#$20		; Make that a printable character
	sta	(kerbf1),y	;	and stuff it in the buffer
	iny			; Advance the index
	lda	rpad		; Get the amount of padding required
	clc			;		...
	adc	#$20		; Make that printable
	sta	(kerbf1),y	; Put it in the buffer
	iny			; Advance index
	lda	rpadch		; Get the padding character expected
	eor	#$40		; Controlify it
	sta	(kerbf1),y	; And stuff it
	iny			; Up the packet buffer index
	lda	reol		; Get the end-of-line expected
	clc			;		...
	adc	#$20		; Characterize it
	sta	(kerbf1),y	; Place that next in the buffer
	iny			; Advance the index
	lda	rquote		; Get the quote character expected
	sta	(kerbf1),y	; Store it as-is last in the buffer
	iny			; Advance index
;	lda	#'Y'		;  Send 'Y' - I will support 8-bit quoting
;	sta	(kerbf1),y	; Stuff it into the data area
	lda	ebqmod		;[30] Get eight-bit quoting
	cmp	#off		;[30] Is it off?
	beq	spar1		;[30] Yes...say we will do it if HE wants to
;	lda	sebq		;[30] Get eight-bit quote character
	lda	rebq		;[jrd] Get RECEIVE eight-bit quote character
	sta	(kerbf1),y	;[30] So other Kermit knows we are
	rts			;[30]	requesting it
spar1:	lda	#'Y'		; Send 'Y' - I will support 8-bit quoting
	sta	(kerbf1),y	; Stuff it into the data area
	rts			;		...
 
;ctrl-l;
;
;	Rpar - This routine sets operational parameters for the other kermit
;	from the init packet data buffer.
;
;		Input:  @Kerbf1 - Operational parameters
;
;		Output: Operational parameters set
;
;		Registers destroyed:	A,Y
;
 
rpar:	ldy	#0		; Start the data index at 0!
	lda	(kerbf1),y	; Start grabbing data from packet buffer
	sec			; Uncharacterize it
	sbc	#$20		;		...
	sta	spsiz		; That must be the packet size of other Kermit
	iny			; Increment the buffer index
	lda	(kerbf1),y	; Get the next item
	sec			;		...
	sbc	#$20		; Uncharacterize that
	sta	stime		; Other Kermit's timeout interval
	iny			; Up the index once again
	lda	(kerbf1),y	; Get next char
	sec			;		...
	sbc	#$20		; Restore to original value
	sta	spad		; This is the amount of padding he wants
	iny			; Advnace index
	lda	(kerbf1),y	; Next item
	eor	#$40		; Uncontrolify this one
	sta	spadch		; That is padding character for other Kermit
	iny			; Advance index
	lda	(kerbf1),y	; Get next item of data
	cmp	#0		; If it is equal to zero
	beq	rpar2		; Use <cr> as a default
	jmp	rpar3		;		...
rpar2:  lda	#cr		; Get value of <cr>
	sta	seol		; That will be the eol character
	jmp	rpar4		; Continue
rpar3:  sec			;		...
	sbc	#$20		; unchar the character
	sta	seol		; That is the eol character other Kermit wants
rpar4:  iny			; Advance the buffer index
	lda	(kerbf1),y	; Get quoting character
	cmp	#0		; If that is zero
	beq	rpar5		; Use # sign as the quote character
	jmp	rpar6		; Otherwise, give him what he wants
rpar5:  lda	#'#'		; Load # sign
rpar6:  sta	squote		; Make that the other Kermit's quote character
	iny			; Advance the index
	lda	pdlen		; Check the data length to see
	cmp	#9		;	if the 8-bit quote is there
	bmi	rparrt		; If not, return
	lda	(kerbf1),y	; Fetch the 8-bit quote
	cmp	#'N'		; Is it 'N'
	beq	rpar8		; Yes, leave.(he doesn't support 8-bit)
	cmp	#'Y'		; Does he support 8-bit quoting?
;	beq	rpar8		; If so, leave. (we don't need it.)
	beq	rpar9		; [jrd] yes, he supports it, using our 
				;  quote character if we asked.
	cmp	#'!'		; Now, it should be a real character
	bmi	rparrt		;	Check if it is in range.
	cmp	#'?'		;	If so, we set the 8-bit quote char
	bmi	rpar7		;	and set 8-bit quoting on.
	cmp	#$60		;	If not, just leave.
	bmi	rparrt		;		...
	cmp	#del		;		...
	bpl	rparrt		;		...
rpar7:	sta	sebq		; Stuff the character here
	lda	#on		; Set 8-bit quoting on
	sta	ebqmod		;		...
	rts			; Return
rpar8:
	sta	sebq		; Make sure this parm is stored
	lda	#off		;	AND that 8-bit quoting is off.
	sta	ebqmod		;		...
	rts
rpar9:	
	sta	sebq		; save it, for now at least...
	lda	ebqmod		; did we ask for it?
	cmp	#off
	beq	rparrt		; no, just return.
	lda	rebq		; we DID ask, and he agreed.  Get the
	sta	sebq		;  char we asked for, and use it.
rparrt:	rts			; Return
 
;ctrl-l;
;
;	Nakit - sends a standard NAK packet out to the other Kermit.
;
;		Input:  NONE
;
;		Output: NONE
;
 
nakit:  lda	#0		; Zero the packet data length
	sta	pdlen		;		...
	lda	#'N'		; Set up a nak packet type
	sta	ptype		;		...
	jsr	spak		; Now, send it
	rts			; Return
 
 
;ctrl-l.SBTTL	Message text
 
versio: .byte	ATEOL
	.byte	"Atari 800 Kermit v 3.7"
	.byte	ATEOL
	.byte	"type ? for help"
	.byte	0		; [53]

 
;ctrl-l.SBTTL	Command tables and help text
 
;
; Top level command table.  The two values returned in x,y are a 
; vector to the routine that matches the command.
;
kercmd:
;	.byte	$10		;[DD][EL][40][] Table length 
	.byte	$11		; Table length with Erase command installed
;	.byte	$12		; Table length with LOG command installed
	.byte	$03,"bye",0		;,$1E,$1E
	.word	bye
	.byte	$07,"connect",0		;,$00,$00
	.word	telnet
	.byte	$09,"directory",0	;,$2A,$2A
	.word	dirst
	.byte	$05,"erase",0		;,$33,$33
	.word	erase
	.byte	$04,"exit",0		;,$03,$03
	.word	quit
	.byte	$06,"finish",0		;,$21,$21
	.word	finish
	.byte	$03,"get",0		;,$24,$24
	.word	getfrs
	.byte	$04,"help",0		;,$06,$06
	.word	help
;	.byte	$03,"log",0,0,0		; not implemented
	.byte	$04,"quit",0		;,$0C,$0C
	.word	quit
	.byte	$07,"receive",0		;,$0F,$0F
	.word	receve
	.byte	$06,"rename",0		;,$27,$27
	.word	rename
	.byte	$07,"restore",0		;,$30,$30
	.word	restst
	.byte	$04,"save",0		;,$2D,$2D
	.word	savst
	.byte	$04,"send",0		;,$12,$12
	.word	send
	.byte	$03,"set",0		;,$15,$15
	.word	setcom
	.byte	$04,"show",0		;,$18,$18
	.word	show
	.byte	$06,"status",0		;,$1B,$1B
	.word	status
 
;
; Command table for subcommands of "set".  Returned value 
; is jump vector in x,y
;
setcmd:
;	.byte	$12		; Table length with DEFAULT-DISK option in
	.byte	$11		; without file-byte-size
	.byte	$06,"escape",0		;,$00,$00
	.word	stesc
	.byte	$03,"ibm",0		;,$03,$03		; 
	.word	stibm
	.byte	$0A,"local-echo",0		;,$06,$06
	.word	stle
	.byte	$07,"receive",0		;,$09,$09
	.word	strc
	.byte	$04,"send",0		;,$0C,$0C
	.word	stsn
	.byte	$12,"terminal-emulation",0		;,$0F,$0F
	.word	stvt
	.byte	$0C,"file-warning",0		;,$12,$12
	.word	stfw
	.byte	$11,"eight-bit-quoting",0		;,$15,$15
	.word	steb
	.byte	$09,"debugging",0		;,$18,$18
	.word	stdb
	.byte	$09,"file-type",0		;,$1B,$1B
	.word	stmod
;	.byte	$0E,"file-byte-size",0		;,$1E,$1E
;	.word	stfbs
	.byte	$0F,"rs232-registers",0		;,$21,$21		;[DD]
	.word	stccr
	.byte	$06,"parity",0		;,$24,$24		; 
	.word	stpari
	.byte	$04,"baud",0		;,$27,$27		;[17]
	.word	stbaud
	.byte	$09,"word-size",0		;,$2a,$2a
	.word	stwrd
	.byte	$0C,"flow-control",0		;,$2d,$2d		;[24]
	.word	stflow
	.byte	$0D,"screen-driver",0		;,$30,$30		;[37]
	.word	stscre
	.byte	$0C,"default-disk",0		;,$33,$33
	.word	stdef

;	.byte	$05,"color",0,$36,$36

 
;
; This one too, values returned are jump vectors
;
shocmd:
;	.byte	$12		; Table length with DEFAULT-DISK opt included
	.byte	$11		; without file-byte-size
	.byte	$03
shodef:	.byte	"all",0			;,$00,$00
	.word	shall
	.byte	$04,"baud",0		;,$7e,$7e
	.word	shbad
	.byte	$09,"debugging",0		;,$51,$51
	.word	shdb
	.byte	$0C,"default-disk",0		;,$99,$99
	.word	shdef
	.byte	$11,"eight-bit-quoting",0		;,$48,$48
	.word	sheb
	.byte	$06,"escape",0		;,$09,$09
	.word	shesc
;	.byte	$0E,"file-byte-size",0		;,$63,$63
;	.word	shfbs
	.byte	$09,"file-type",0		;,$5A,$5A
	.word	shmod
	.byte	$0C,"file-warning",0		;,$3F,$3F
	.word	shfw
	.byte	$0C,"flow-control",0		;,$90,$90		;[24]
	.word	shflow
	.byte	$03,"ibm",0		;,$12,$12
	.word	shibm
	.byte	$0A,"local-echo",0		;,$1B,$1B
	.word	shle
	.byte	$06,"parity",0		;,$75,$75
	.word	shpari
	.byte	$07,"receive",0		;,$24,$24
	.word	shrc
	.byte	$0F,"rs232-registers",0		;,$6C,$6C			;[DD]
	.word	shccr
	.byte	$04,"send",0		;,$2D,$2D
	.word	shsn
	.byte	$12,"terminal-emulation",0		;,$36,$36
	.word	shvt
	.byte	$09,"word-size",0		;,$87,$87			;[17]
	.word	shwrd

stscmd: .byte	$07
	.byte	$14,"eight-bit-quote-char",0,$06,$06
	.byte	$0B,"end-of-line",0,$09,$09
	.byte	$0D,"packet-length",0,$0C,$0C
	.byte	$08,"pad-char",0,$00,$00
	.byte	$07,"padding",0,$03,$03
	.byte	$0A,"quote-char",0,$0F,$0F
	.byte	$07,"timeout",0,$12,$12
 
ftcmd:
;	.byte	$04				; len with 'script' in
	.byte	3
	.byte	$07
ftcdef:	.byte	"atascii",0,ftatas,ftatas		; defualt
	.byte	$05,"ascii",0,ftstas,ftstas
	.byte	$06,"binary",0,ftbin,ftbin
;	.byte	$06,"script",0,$03,$03
 
parkey:	.byte	$05		; LENGTH OF THIS TABLE IS 5
	.byte	$04,"even",0,$04,$04		;
	.byte	$04,"mark",0,$02,$02		;
	.byte	$04,"none",0,$00,$00		;
	.byte	$03,"odd",0,$03,$03		;
	.byte	$05,"space",0,$01,$01		;
 
bdkey:	.byte	$0A		;[17] Length of table
	.byte	2,"50",0,bd50,bd50
	.byte	2,"75",0,bd75,bd75
	.byte	3,"110",0,bd110,bd110
	.byte	3,"150",0,bd150,bd150
	.byte	3,"300",0,bd300,bd300
 	.byte	4,"1200",0,bd1200,bd1200
	.byte	4,"1800",0,bd1800,bd1800
	.byte	4,"2400",0,bd2400,bd2400
	.byte	4,"4800",0,bd4800,bd4800
	.byte	4,"9600",0,bd9600,bd9600 
 
debkey:	.byte	$03		; LENGTH OF THIS TABLE IS 3
	.byte	$03,"off",0,$00,$00		;
	.byte	$05,"terse",0,$01,$01		;
	.byte	$07,"verbose",0,$02,$02		;
 
fbskey: .byte	$02
	.byte	$09,"eight-bit",0,$00,$00
	.byte	$09,"seven-bit",0,$01,$01
 
oncmd:  .byte	$02
 	.byte	$02,"on",0,$01,$01
 	.byte	$03,"off",0,$00,$00
 
yescmd: .byte	$02
	.byte	$02,"no",0,$00,$00
 	.byte	$03,"yes",0,$01,$01
 
scrkey:	.byte	$03		;[37]
	.byte	$05,"atari",0,scrae,scrae
	.byte	$0a,"40-columns",0,scr40,scr40
	.byte	$0a,"80-columns",0,scr80,scr80

termemu: .byte	$03		;terminal emulation may be none, vt52 or vt100
	.byte	4,"none",0,ttnone,ttnone
	.byte	5,"vt100",0,tt100,tt100
	.byte	4,"vt52",0,tt52,tt52

;ddskey:	.byte	$01
 
;	.byte	$05
;	.asciz	/DRIVE/
;	.byte	$00,$00
 
kerehr:	.byte	cmcfm		; tell them they can also confirm
	.byte	nul		; end help command string
 
kereht:	.byte	cmtxt		;[]
	.byte	nul
 
kerhlp: .byte	ATEOL
	.byte	"kermit commands for this version are:",ATEOL
	.byte	ATEOL
	.byte	"bye       shut  down  and  log  out  a",ATEOL
	.byte	"          remote  kermit server,  then",ATEOL
	.byte	"          exit.",ATEOL
	.byte	ATEOL
	.byte	"connect   allow user to talk to remote",ATEOL
	.byte	"          kermit directly.",ATEOL
	.byte	ATEOL
	.byte	"directory list disk directory",ATEOL
	.byte	ATEOL
	.byte	"rename    renames one or more files on",ATEOL
	.byte	"          the default drive.",ATEOL
	.byte	ATEOL
	.byte	"erase     deletes one or more files on",ATEOL
	.byte	"          the default drive.",ATEOL
	.byte	ATEOL
	.byte	"exit      exit  from  kermit  back  to",ATEOL
	.byte	"          the  host operating  system.",ATEOL
	.byte	ATEOL
	.byte	"finish    shut   down  remote   kermit",ATEOL
	.byte	"          server  but  do not  log out",ATEOL
	.byte	"          remote job. do not exit from",ATEOL
	.byte	"          local kermit.",ATEOL
	.byte	ATEOL
	.byte	"get       fetch  a file from a  remote",ATEOL
	.byte	"          server kermit.  the filename",ATEOL
	.byte	"          is  validated by  the remote",ATEOL
	.byte	"          server.",ATEOL
	.byte	ATEOL
	.byte	"help      print instructions on",ATEOL
	.byte	"          various  commands  available",ATEOL
	.byte	"          in kermit.",ATEOL
	.byte	ATEOL
	.byte	"quit      same as exit.",ATEOL
	.byte	ATEOL
	.byte	"receive   receive a file or file group",ATEOL
	.byte	"          from the remote host.",ATEOL
	.byte	ATEOL
	.byte	"restore   restore  kermit  parameters",ATEOL
	.byte	"          from file kermit.ini",ATEOL
	.byte	ATEOL
	.byte	"save      save  kermit  parameters in",ATEOL
	.byte	"          file kermit.ini",ATEOL
	.byte	ATEOL
	.byte	"send      sends a file from the  m6502",ATEOL
	.byte	"          based computer to the remote",ATEOL
	.byte	"          host.",ATEOL
	.byte	ATEOL
	.byte	"set       establish various parameters",ATEOL
	.byte	"          such as debugging mode,  eol",ATEOL
	.byte	"          character, and  transmission",ATEOL
	.byte	"          delay.",ATEOL
	.byte	ATEOL
	.byte	"show      display  various  parameters",ATEOL
	.byte	"          established   by   the   set",ATEOL
	.byte	"          command.",ATEOL
	.byte	ATEOL
	.byte	"status    give  information about  the",ATEOL
	.byte	"          last file transfer.",ATEOL
	.byte	nul
 
inthlp: .byte	"one of the following:",ATEOL
	.byte	"     ? - this help message.",ATEOL
	.byte	"     b - send a break signal.",ATEOL
	.byte	"     c - close the connection.",ATEOL
	.byte	"     s - status of connection.",ATEOL
	.byte	"     escape-char - transmit the escape character.",ATEOL,nul
 
;ctrl-l.SBTTL	Message Text
 
ermes1: .byte	ATEOL,"?unrecognized command",0		; [53]
ermes2: .byte	ATEOL,"?illegal character",0		; [53]
ermes3: .byte	ATEOL,"?not confirmed",0		; [53]
ermes4: .byte	ATEOL,"?integer out of range",0		; [53]
ermes5: .byte	ATEOL,"?ascii character is not in proper range",0		; [53]
ermes6: .byte	ATEOL,"?expecting keyword",0		; [53]
ermes7: .byte	ATEOL,"?expecting file spec",0		; [53]
ermes8: .byte	ATEOL,"?expecting integer",0		; [53]
;ermes9: .byte	ATEOL,"?expecting switch",0		; [jrd] no switches
ermesa:	.byte	ATEOL,"?",0		; [53]
ermesb:	.byte	ATEOL,"?null string found while looking for text",0	; [53]
ermesc:	.byte	ATEOL,"?could not send generic logout packet",0		; [53]
ermesd:	.byte	ATEOL,"?could not send generic finish packet",0		; [53]
ermesf:	.byte	ATEOL,"?drive number out of range",0		; [53]
 
erms0a: .byte	ATEOL,"disk error stat = "
ermsdc:	.byte	"??",0		; [53]
erms10: .byte	ATEOL,"cannot receive init",0		; [53]
erms11: .byte	ATEOL,"cannot receive file-head",0		; [53]
erms12: .byte	ATEOL,"cannot receive data",0		; [53]
erms14: .byte	ATEOL,"max retry count exceeded",0		; [53]
erms15: .byte	ATEOL,"bad chksum:pack, actual ",0		; [53]
erms16: .byte	ATEOL,"program error in rpak",0		; [53]
erms17: .byte	ATEOL,"8-bit quoting refused",0		; [53]
erms18: .byte	ATEOL,"transfer aborted by user",0		; [53]
erms19: .byte	ATEOL,"cannot alter filename",0		; [53]
erms1a: .byte	ATEOL," file already exists",0		; [53]

;
; error message vectors, indexed by error numbers
;
kerrv:	.word	0			; padding
	.word	erms10			; errcri 
	.word	erms11			; errcrf
	.word	erms12			; errcrd
	.word	erms14			; errmrc
	.word	erms15			; errbch
	.word	0			; no internal error msg?
	.word	erms18			; errfta
	.word	erms19			; errfal
	.word	erms1a			; errfae
	.word	erms0a			; errfde
;
; general error code message
;
ermess:	.byte	ATEOL,"Error ",0
;

; file types
kerftp: .byte	"ascii  ",0		; [53]
	.byte	"atascii",0		; [53]
	.byte	"binary ",0		; [53]
;	.byte	"script ",0		; [53]
 
; parity strings
kerprs:	.byte	"none ",0		; [53]
	.byte	"space",0		; [53]
	.byte	"mark ",0		; [53]
	.byte	"odd  ",0		; [53]
	.byte	"even ",0		; [53]
;
; See 850 man pg 54 for these values.  All specify 'No translation'
; 
parval:	.byte	$20		;[17] None, ignore input, don't change output
	.byte	$2C		;[17] Space, clear input, don't change output
	.byte	$2F		;[17] Mark, clear input, set output 1
	.byte	$25		;[17] Odd, check in odd, set out odd
	.byte	$2A		;[17] Even, check in even, set out even
 
kerbds:	.byte	"50  ",0		; [53]
	.byte	"75  ",0		; [53]
	.byte	"110 ",0		; [53]
	.byte	"150 ",0		; [53]
	.byte	"300 ",0		; [53]
	.byte	"1200",0		; [53]
	.byte	"1800",0		; [53]
	.byte	"2400",0		; [53]
	.byte	"4800",0
	.byte	"9600",0
 
bdval:	.byte	$02		;[17]   50
	.byte	$04		;[17]   75
	.byte	$05		;[17]  110
	.byte	$07		;[17]  150
	.byte	$08		;[17]  300 
bddef:	.byte	$0A		;[17] 1200
	.byte	$0B		;[17] 1800
	.byte	$0C		;[17] 2400
	.byte	$0D		; [jrd] 4800
	.byte	$0E		; [jrd] 9600
 
kerdms:	.byte	"off     ",0	; Debug mode strings
	.byte	"terse   ",0	;
	.byte	"verbose ",0	;

kertms:	.byte	"none ",0		; terminal emulation strings
	.byte	"vt52 ",0
	.byte	"vt100",0

kerrts: .byte	"spak:     sending           - ",0
	.byte	"spakch:   send complete     - ",0
	.byte	"rpak:     trying to receive - ",0
	.byte	"rpkfls:   failed to receive - ",0
	.byte	"rpkret:   received          - ",0
	
debms1: .byte	"additional data",0
debms2: .byte	"     seq number           ",0
debms3: .byte	"     number of data chars ",0
debms4: .byte	"     packet checksum      ",0
snin01: .byte	"sending ..packet no. ",0
rcin01: .byte	"waiting ..packet no. ",0
logrcvm: .byte	ATEOL,"Receiving ",0
logsndm: .byte	ATEOL,"Sending ",0
shin00: .byte	"debugging is          ",0
shin01: .byte	"terminal emulation is ",0
shin02: .byte	"ibm-mode is           ",0
shin03: .byte	"local-echo is         ",0
shin04: .byte	"eight-bit-quoting is  ",0
shin05: .byte	"file-warning is       ",0
shin06: .byte	"escape character is   ",0
shin07: .byte	"send",0
shin08: .byte	"  eight-bit-quoting char is   ",0
shin09: .byte	"  end-of-line character is    ",0
shin10: .byte	"  packet-length is            ",0
shin11: .byte	"  padding character is        ",0
shin12: .byte	"  amount of padding is        ",0
shin13: .byte	"  quote character is          ",0
shin14: .byte	"  timeout (in seconds) is     ",0
shin15: .byte	"receive",0
shin16: .byte	"file-type mode is     ",0
shin17: .byte	"file-byte-size is     ",0
shin18: .byte	"rs232 registers =     $",0
shin19:	.byte	"baud rate is          ",0
shin20:	.byte	"parity is             ",0
shin21:	.byte	"word-size is          ",0
shin22:	.byte	"flow-control is       ",0
shin23:	.byte	"default-disk is       ",0
 
shon:	.byte	"on",0
shoff:  .byte	"off",0
 
shsbit: .byte	"seven-bit",0
shebit: .byte	"eight-bit",0

sstrng:	.byte	"sending: ",0
rstrng:	.byte	"received: ",0
 
stin00: .byte	"number of data chars sent is:     ",0
stin01: .byte	"number of data chars received is: ",0
stin02: .byte	"total no. of chars sent is:       ",0
stin03: .byte	"total no. of chars received is:   ",0
stin04: .byte	"overhead for send packets is:     ",0
stin05: .byte	"overhead for receive packets is:  ",0
stin06: .byte	"last error encountered is:        ",0
 
inf01a: .byte	"[connecting to host: type ",0
inf01b: .byte	" c to return]",0

;ctrl-l.SBTTL	General Screen Manipulation Routines

;
;	These routines perform screen manipulation functions.  The usually
;	call a screen driver, but some call lower-level manipulation routines.
;
;	These routines all turn the cursor off before calling the screen
;	driver.
;

;
;	scrini - call the screen drivers initilization code
;
;	Input:	None
;	Output: Assorted screen parameters are set
;
;	Registers destroyed - A,X,Y
;
;	This routine initilizes some parameters and calls all of the screen
;	drivers initilization code. The drivers should be called
;	least_favorite_device first and most_favorite device last.
;

scrini:
	lda	#0		; wrap defaults on
	sta	wrap
;	sta	line25		; the 25th line is a status line
;	jsr	scraeini
;	jsr	scr40ini
;	jsr	scr80ini
	jsr	scrent		; enter the most_favorite screen driver
	rts

;
;	scrent - start up a screen driver
;
;	Input:	Screen type in scrtype
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine sets some parameters and then calls the screen driver to
;	start it and set its parameters.  It then calls scred2 to erase the
;	screen.
;

scrent:	
	lda	#0		; cursor starts at row 1, column 1
	sta	COLCRS
	sta	ROWCRS
	sta	curstat
	lda	#1		; mark cursor flash as aborted
	sta	curabrt		; cursor is off but supposed to be on
; 	jsr	rdtim		; set cntdown to wait the usual amount of time
	lda	RTCLOK+2
	sta	cntdown
	jsr	scrent1		; call the screen driver
	jsr	scrrst		; reset parameters to normal values
	jsr	scred2		; clear entire screen
	rts			; all done

scrent1:
	ldy	scrtype
	jsr	case
	.word	scraeent
	.word	scr40ent
	.word	scr80ent
	rts

;
;	scrext - exit from the screen driver
;
;	Input:	Screen type in scrtype
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine calls the screen driver to exit.  The hardware is returned
;	to the state it was left in before kermit started.
;

scrext:	
	ldy	scrtype
	jsr	case
	.word	scraeext
	.word	scr40ext
	.word	scr80ext

;
;	scrrst - reset the screen parameters to normal values
;
;	Input:	None
;	Output: Assorted parameters changed.
;
;	Registers destroyed - A
;
;	This routine sets reverse mode off, flashing off, the scrolling
;	region to full size, and many other things
;

scrrst:	lda	#0		; top of scrolling area is line 1
	sta	top
	lda	#23		; bottom of scrolling area is line 24
	sta	bot
	lda	#0
	sta	underln		; underline is off
	sta	reverse		; reverse is off
	sta	alternt		; alternt colors are off
	sta	flash		; flashing is disabled
	jsr	scrsav		; make these the saved parameters
	rts			; all done

;
;	scrput - put a character on the screen
;
;	Input:	Character to put in a-reg.
;		Screen type in scrtype.
;	Output: Screen ram, both color rams, and cursor position are changed.
;
;	Registers destroyed - A,X,Y
;
;	Assumption:  All screens are 80 columns, but the first (40 columns)
;
;	This routine puts a character on the screen.  It advances the cursor
;	and scrolls the screen when necessary.  It handels a carriage
;	return specially.  It prints a carriage return and line feed
;

scrput:	cmp	#ATEOL		; is it a carriage return?
	bne	scrput4		; no go do usual stuff
	ldx	scrtype		; on atari screen?
	beq	scrput3		; yup, just go do it
	jsr	scrcr		; no.  Do a general purpose ret and lf
	jsr	scrlf
	rts
scrput4:
	pha			; save the character to put
	jsr	scroff		; cant use screen driver while cursor blinks
	pla			; restore the character to put
	ldy	#39		; 40 is the funny row in 40 column mode
	ldx	scrtype		; are we in 40 column mode?
	beq	scrput1		; no.  we have an 80 column screen
	ldy	#79		; 80 is the funny row in 80 column modes
scrput1: cpy	COLCRS		; are we in the funny column?
	bcs	scrput2		; no
	ldx	wrap		; are we in wrap mode
	bne	scrput3		; no. do not wrap
	pha			; save the character to put
	jsr	scrcr		; yes. do a carriage return
	jsr	scrlf		; and a linefeed
	pla			; restore the character to put
scrput2: jsr	scrput3		; call the routine to put a character.
;	inc	COLCRS
	rts	

scrput3: ldy	scrtype		; call the screen driver
	jsr	case
	.word	scraeput
	.word	scr40put
	.word	scr80put

;
;	scrcr - perform a carriage return
;
;	Input:	Screen type in scrtype.
;		Cursor position in COLCRS, ROWCRS
;
;	Output: New new cursor column in COLCRS.
;
;	Registers destroyed - A,X,Y
;
;	This routine performs a carriage return.
;

scrcr:	ldy	ROWCRS
	ldx	#0		; put cursor in column zero
	jsr	scrplt		; move the cursor there
	rts			; all done

;
;	scrlf - perform a line feed
;
;	Input:	screen type in scrtype
;		cursor column in ROWCRS
;		cursor row in COLCRS
;	Output: New cursor position in COLCRS, ROWCRS.
;
;	Registers destroyed - A,X,Y
;
;	This routine performs a line feed.
;

scrlf:	ldy	ROWCRS		; check if bottom reached
	cpy	bot
;	bcc	scrlf1		; yes. scroll screen
	bne	scrlf1		; yes. scroll screen
	jmp	scrind
scrlf1:	iny
	ldx	COLCRS
	jsr	scrplt		; no. move the cursor down one line.
	rts
;
;	scrrlf - perform a reverse line feed with scrolling
;
;	Input:	Type of screen in scrtype
;		Cursor coordinates in COLCRS, ROWCRS
;
;	Output: None
;
;	Registers Destroyed: A,X,Y
;
;	This routine performs a reverse line feed.  The cursor is moved up
;	one line.  If the cursor reaches the top of the scrolling area, scrri
;	is called to scroll the screen backwards.
;

scrrlf:	ldy	ROWCRS
	cpy	top
	beq	scrrlf1		; reached top of the screen?
	dey			; no, just move the cursor up
	ldx	COLCRS
	jsr	scrplt
	rts
scrrlf1: jsr	scrri		; yes, at top of screen.  Scroll backwards
	rts

;
;	scru - move the cursor up stopping at the top of the screen
;
;	Input:	Type of screen in scrtype
;		Cursor coordinates in COLCRS, ROWCRS
;
;	Output: None
;
;	Registers Destroyed: A,X,Y
;
;	This routine moves the cursor up.  If the cursor reaches the top
;	of the screen it stops.
;

scru:	ldy	ROWCRS
	beq	scru1		; at top of screen?
	dey
	ldx	COLCRS
	jsr	scrplt		; move the cursor to its new position
scru1:	rts

;
;	scrd - move the cursor down stopping at the bottom of the screen
;
;	Input:	Type of screen in scrtype
;		Cursor coordinates in COLCRS, ROWCRS
;
;	Output: None
;
;	Registers Destroyed: A,X,Y
;
;	This routine moves the cursor down.  If the cursor reaches the bottom
;	of the screen it stops.
;

scrd:	ldy	ROWCRS
	cpy	#23		; 24th line is at the bottom
	bcc	scrd1		; move the cursor if less that 24
	bne	scrd2		; do not move the cursor if greater than 24
scrd1:	iny
	ldx	COLCRS
	jsr	scrplt		; put the cursor at its new position
scrd2:	rts			; all done

;
;	scrl - move the cursor left stopping at the left side of the screen
;
;	Input:	Type of screen in scrtype
;		Cursor coordinates in COLCRS, ROWCRS
;
;	Output: New cursor coordinates in COLCRS, ROWCRS
;
;	Registers Destroyed: A,X,Y
;
;	This routine moves the cursor left.  If the cursor reaches the left
;	most side of the display, it stops.
;

scrl:	ldx	COLCRS
	beq	scrl1		; at left side of screen?
	dex
	ldy	ROWCRS
	jsr	scrplt		; move the cursor to its new position
scrl1:	rts

;
;	scrr - move the cursor right stopping at the right side of the screen
;
;	Input:	Type of screen in scrtype
;		Cursor coordinates in COLCRS, ROWCRS
;
;	Output: New cursor coordinates in COLCRS, ROWCRS
;
;	Registers Destroyed: A,X,Y
;
;	This routine moves the cursor right.  If the cursor reaches the right
;	side of the screen it stops.
;

scrr:	ldx	COLCRS
	cpx	#40		; check if past right side
	lda	scrtype		; in 40 column mode?
	beq	scrr1
	cpx	#80		; check if past right side
scrr1:	bcs	scrr2
	inx			; move the cursor right
	ldy	ROWCRS
	jsr	scrplt		; move the cursor to its new position
scrr2:	rts			; all done

;
;	scred0 - perform the Erase Display #0 VT100 function
;
;	Input: Type of screen to erase in scrtype
;
;	Output: None
;
;	Registers Destroyed: A,X,Y
;
;	This routine clears from the cursor position to the end of the screen.
;	This routine works in 40 column mode, 80 column mode, or Commodore 128
;	mode.
;

scred0:	lda	ROWCRS		; save the cursor y position
	pha
	jsr	screl0		; erase from the cursor to the line
scred0c: inc	ROWCRS		; do the next line
	lda	ROWCRS
	cmp	#24		; on line number 24?
	bcs	scred0b		; past line 24?  if so, stop
scred0a: jsr	screl2		; erase the entire line
	jmp	scred0c
scred0b: pla			; restore cursor y position
	sta	ROWCRS
	rts			; all done

;
;	scred1 - perform the Erase Display #2 VT100 function
;
;	Input: Type of screen to erase in scrtype
;
;	Output: None
;
;	Registers Destroyed: A,X,Y
;
;	This routine clears from the beginning of the screen to the cursor.
;	This routine works for 40 column mode, 80 column mode, and commodore
;	128 mode.
;

scred1:	lda	ROWCRS		; save the cursor y position
	pha
	lda	#0
	sta	ROWCRS
scred1b: pla			; cursors real position
	pha			; keep it on the stack
	cmp	ROWCRS		; on last line to erase?
	beq	scred1a		; yes, erase it specially
	jsr	screl2		; erase the entire line
	inc	ROWCRS
	jmp	scred1b
scred1a: pla			; restore cursor y position
	sta	ROWCRS
	jsr	screl1		; erase from beginning of line to cursor
	rts			; all done

;
;	scred2 - perform the Erase Display #2 VT100 function (clear screen)
;
;	Input: Type of screen to erase in scrtype
;
;	Output: None
;
;	Registers Destroyed: A,X,Y
;
;	This routine clears the entire screen in either 40 column mode,
;	80 column mode, or c128 mode.  It calls screl2 to do the dirty work.
;

scred2:
	ldy	scrtype		; what screen are we clearing?
	bne	scred2z
	lda	#ATCLR		; E:, just tell it to clear
	jmp	sputch		;  do this
scred2z:
	lda	ROWCRS		; save the cursor y position
	pha
	lda	#0		; move the cursor to the top
	sta	ROWCRS
scred2a: jsr	screl2		; erase the line
	inc	ROWCRS		; do the next line
	lda	ROWCRS
	cmp	#24		; on line number 24?
	bcs	scred2b		; yup, done
	bcc	scred2a		; not yet.  do another line
scred2b: pla			; restore cursor y position
	sta	ROWCRS
	rts			; all done

;
;	screl0 - Perform the VT100 Erase Line function #0
;
;	Input:	Line number to erase in ROWCRS
;		Screen type in scrtyp
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine erases from the cursor to the end of the line
;

screl0:	jsr	scroff		; cant use screen driver while curosr blinks
	ldy	scrtype		; which routine to use
	jsr	case
	.word	scraeel0
	.word	scr40el0
	.word	scr80el0

;
;	screl1 - Perform the VT100 Erase Line function #1
;
;	Input:	Line number to erase in ROWCRS
;		Screen type in scrtyp
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine erases from the beginning of line to the cursor
;

screl1:	jsr	scroff		; cant use screen driver while curosr blinks
	ldy	scrtype		; which routine to use
	jsr	case
	.word	scraeel1
	.word	scr40el1
	.word	scr80el1

;
;	screl2 - Perform the VT100 Erase Line function #2
;
;	Input:	Line number to erase in ROWCRS
;		Type of screen in scrtype
;	Output:	None
;
;	Registers destroyed - A,X,Y
;
;	This routine erases one line compleatly.
;

screl2:	
	jsr	scroff		; cant use screen driver while cursor blinks
	ldy	scrtype		; which routine to use to erase
	jsr	case		; go to proper routine
	.word	scraeel2		; erase one line on 40 column screen
	.word	scr40el2		; erase one line on 80 column screen
	.word	scr80el2		; abort with much fanfair

;
;	scrind - perfrom the VT100 index function (Move the screen one line)
;
;	Input:	Screen type in scrtyp
;	Output:	None
;
;	Registers destroyed - A,X,Y
;
;	This routine scrolls the screen down one line. It calls either scraeind,
;	scr40ind, or scr80ind depending on the screen type.
;

scrind:	jsr	scroff		; cant use screen driver while cursor blinks
	ldy	scrtype
	jsr	case
	.word	scraeind
	.word	scr40ind
	.word	scr80ind

;
;	scrri - perfrom the VT100 reverse index function (scroll backwards)
;
;	Input:	Screen type in scrtyp
;	Output: Screen and color rams are changed
;
;	Registers destroyed - A,X,Y
;
;	This routine scrolls the screen up one line. It calls either scraeri,
;	scr40ri, or scr80ri depending on the screen type.
;

scrri:	jsr	scroff		; cant use screen driver while cursor blinks
	ldy	scrtype
	jsr	case
	.word	scraeri
	.word	scr40ri
	.word	scr80ri

;
;	scrdl - Delete line
;
;	input:	current line
;	output:	current line of display zapped, rest of scrolling region moves up to
;		fill
;
;	zzz should check to make sure inside scroll rgn here
;
scrdl:
	lda	top		; get current scrolling rgn top
	pha			; save it
	lda	ROWCRS		; get current row
	sta	top		; make it top of scroll rgn temporarily
	jsr	scrind		; do an 'index', squeezing out current line
	pla			; get old top back
	sta	top
	rts

;
;	scril - Insert line
;
;	input:	current line
;	output:	Bottom line of scroll rgn zapped, blank line inserted at current
;		line
;
;	zzz should check to make sure inside scroll rgn here
;
scril:
	lda	top		; get current scrolling rgn top
	pha			; save it
	lda	ROWCRS		; get current row
	sta	top		; make it top of scroll rgn temporarily
	jsr	scrri		; do a 'reverse index', squeezing out bottom line
	pla			; get old top back
	sta	top
	rts
	
;
;	scrsav - save screen attributes and cursor position
;
;	Input:	screen attributes and cursor position
;
;	Output:	save1, save2, save3, ... save6
;
;	This routine saves the screen attributes and cursor position
;	

scrsav:	lda	COLCRS
	sta	save1
	lda	ROWCRS
	sta	save2
	lda	alternt
	sta	save3
	lda	underln
	sta	save4
	lda	flash
	sta	save5
	lda	reverse
	sta	save6
	rts

;
;	scrlod - load the saved screen attributes and cursor position
;
;	Input:	save1, save2, save3, ... save6
;
;	This routine restores the saved screen attributes and cursor position
;

scrlod:	ldx	save1
	ldy	save2
	jsr	scrplt
	lda	save3
	sta	alternt
	lda	save4
	sta	underln
	lda	save5
	sta	flash
	lda	save6
	sta	reverse
	rts

;
;	scrplt - plot the cursor
;
;	Input:	Cursor X position in X-reg
;		Cursor Y position in Y-reg
;
;	Output: COLCRS and ROWCRS are set.
;
;	Registers destroyed - A,X,Y
;
;	This routine puts the cursor at X,Y.  It checks to make sure the
;	cursor is being moved to a valid location before it moves the cursor.
;

scrplt:
	cpy	#24		; cant be greater than or equal to 24
	bmi	scrplt1		; oops.  it was
	ldy	#23		; ok, make it 23
scrplt1:
	cpy	#0		; better be >= 0
	bpl	scrplt2
	ldy	#0
scrplt2:
;
; kludge.  Because of the way the auto wrap stuff works, we have to
; allow the column to get to 80 (40)
;
	cpx	#41		; cant be greater than or equal to 40...
	lda	scrtype		; ... in 40 column mode
	beq	scrplt4
	cpx	#81		; compare with 80 if not in 40 column mode
scrplt4:
	bcs	scrplt9		; oops.  Greater than the current margin
	tya			; save the new y position	
	pha
	txa			; save the new x position
	pha		
	jsr	scroff		; turn off the cursor
	pla			; get the new x position
	sta	COLCRS
	pla			; get the new y position
	sta	ROWCRS
scrplt9: rts			; all done

;
;	scroff - disable the cursor.
;
;	Input:	COLCRS, ROWCRS, curstat, curabrt, scrtype
;
;	Output: curabrt
;
;	Registers destroyed - A,X,Y
;
;	This routine disables the cursor.  It calls the proper screen driver
;	to do the dirty work.
;

scroff:	lda	curabrt		; is the cursor flash already aborted?
	bne	scroff1		; yes.
	lda	curstat		; cursor light?
	beq	scroff1		; yes.
	sta	curabrt		; mark cursor flash as aborted
	jsr	scrtgl		; toggle the cursor
scroff1: rts			; all done

;
;	scrfls - flash the screen and cursor
;
;	Input:	curstat - status of cursor (light or dark)
;		curabrt - flag indicating if cursor flash was aborted early.
;		scrtype - type of screen
;
;	Output: curstat - curstat is toggled if time
;		curabrt - curabrt is always cleared
;
;	Registers destroyed - A,X,Y
;
;	This routine flashes the screen and toggles the cursor.  It also
;	stops the sound of the bell 6 jiffys after it started.  This routine
;	should be called a frequently as possible.
;

scrfls:	lda	curabrt		; was the cursor flash aborted early?
	beq	scrfls1		; no.  No need to light it.
	jsr	scrtgl		; toggle the cursor
	lda	#0		; clear the abort flag
	sta	curabrt
scrfls1: 
; 	jsr	rdtim		; check the time 
	lda	RTCLOK+2	; [jrd] get lo order time value
	tay			; save the time for later use
	sec
	sbc	lpcnt		; subtract the time the bell started
	cmp	#6		; been 6 jiffys since it started?
	bcc	scrfls3		; nope.  Dont stop the bell yet
;
; This screws up POKEY
;
;	lda	#0
;	sta	AUDC4		; stop the bell
	lda	bordclr		; unflash the border
	sta	COLOR4
scrfls3: tya			; check timer value again
	sec
	sbc	cntdown
	cmp	#20		; have  36 jiffies elapsed?
	bcs	scrfls2		; yes they have
	rts			; no they havent.  stop here
scrfls2: sty	cntdown		; reset the countdown timerldy	
	jsr	scrtgl		; toggle the cursor status
	ldy	scrtype		; flash the flashing characters
	jsr	case
	.word	scraefls
	.word	scr40fls
	.word	scr80fls

;
;	scrtgl - Toggle the cursor
;
;	Input:	COLCRS - x coordinate of cursor
;		ROWCRS - y coordinate of cursor
;		Type of screen in scrtype
;
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	this routine calls the screen driver to toggle the cursor
;

scrtgl:	lda	curstat		; keep track if cursor is dark or light
	eor	#$01
	sta	curstat
	ldy	scrtype		; call the screen driver
	jsr	case
	.word	scraetgl
	.word	scr40tgl
	.word	scr80tgl


;ctrl-l.SBTTL	80 Column screen driver

;
;	These routines manipulate the screen in 80 column mode.
;

;
;	scr40ini - initilize 40/80 column screen during powerup
;
;	Input:	None
;	Output: scrtype set to use 80 columns
;
;	Registers destroyed - A
;
;	This routine does all of the powerup initilization necessary for
;	80 columns that was not done in scraeini, and sets the screen type
;	to 80 columns.
;

scr40ini:
	lda	#scr40
	sta	scrtype
	rts

;
;	scr40ent - enter the 40/80 column screen driver
;
;	Input:	None
;
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine starts the 40/80 column screen driver.
;	well, sort of.  set the display list to point to ours, and build font40
;

scr40ent:
	jsr	makedl40
	lda	scr40dl
	ldy	scr40dl+1
	jsr	setdlist
	lda	#scr40		; we're now in 40/80 mode
	sta	scrtype
;
; now make up the font we want.  Copy the builtin one, and stuff in
; braces, tilde, graphics
;
	ldy	#0		; zero the y-reg
	ldx	newchar,y	; number of characters defined in this chunk
scr40ent1: iny
	lda	newchar,y	; source of characters (lo order)
	sta	source
	iny
	lda	newchar,y	; source of characters (hi order)
	sta	source+1
	iny
	lda	newchar,y	; destination of characters (lo order)
	sta	dest
	iny
	lda	newchar,y	; destination of characters (hi order)
	sta	dest+1
	iny
	tya			; save y-reg across call to move8
	pha
	jsr	move8
	pla			; restore y-reg
	tay
	ldx	newchar,y	; number of characters in this chunk (0=end)
	bne	scr40ent1	; loop until done
	lda	#>font40	; now point CTIA at it
	sta	CHBAS
	rts

;
;	Newchar - character mapping table
;
;	This table is used to define the 40 column character set
;	The format of this table is:
;		Number of characters to copy	(byte)
;		Source of characters		(word)
;		Destination for characters	(word)
;
newchar:
	.byte	128		; the whole thing
	.word	$E000		; atari builtin font
	.word	font40		; our font table
;
; graphics characters
;
	.byte	1
	.word	$E000+(84*8)	; blot
	.word	font40+(64*8)

	.byte	9
	.word	charg97		; square blot .. vt
	.word	font40+(65*8)

	.byte	2		; 2 more chars
	.word	charg111	;  for scan 1, scan3
	.word	font40+(79*8)

	.byte	2		; and 2 more
	.word	charg114	;  for scan 7, 9
	.word	font40+(82*8)

	.byte	1
	.word	$E000+(67*8)	; down right corner
	.word	font40+(74*8)

	.byte	1
	.word	$E000+(69*8)	; up right corner
	.word	font40+(75*8)

	.byte	1
	.word	$E000+(81*8)	; up left
	.word	font40+(76*8)

	.byte	1
	.word	$E000+(90*8)	; down left
	.word	font40+(77*8)

	.byte	1
	.word	$E000+(83*8)	; center cross
	.word	font40+(78*8)

	.byte	1
	.word	$E000+(82*8)	; scan 5
	.word	font40+(81*8)

	.byte	1
	.word	$E000+(124*8)	; vert bar
	.word	font40+(88*8)

	.byte	1
	.word	$E000+(65*8)	; left t
	.word	font40+(84*8)

	.byte	1
	.word	$E000+(68*8)	; right t
	.word	font40+(85*8)

	.byte	1
	.word	$E000+(88*8)	; bot t
	.word	font40+(86*8)

	.byte	1
	.word	$E000+(87*8)	; top t
	.word	font40+(87*8)


; more later zzz
	.byte	4		; { | } ~
	.word	char123
	.word	font40+(123*8)

	.byte 0		; end of table

;
;	charXXX - 40 column character definitions not available in rom
;
; 
charg97:
	.byte	$CC,$CC,$33,$33,$CC,$CC,$33,$33	; square blot
	.byte	$90,$90,$F0,$90,$8E,$08,$08,$08	; h/t
	.byte	$F0,$80,$E0,$9E,$90,$1C,$10,$10	; f/f
	.byte	$30,$40,$40,$3C,$12,$12,$1C,$12	; c/r
	.byte	$40,$40,$40,$7E,$10,$1C,$10,$10	; l/f
	.byte	$18,$24,$24,$18,$00,$00,$00,$00	; degrees
	.byte	$08,$08,$3E,$08,$08,$00,$3E,$00	; plus/minus
	.byte	$48,$68,$58,$48,$40,$08,$0E,$00	; n/l
	.byte	$44,$44,$28,$10,$3E,$08,$08,$08	; v/t
charg111:
	.byte	$FF,$FF,$00,$00,$00,$00,$00,$00	; scan 1
	.byte	$00,$FF,$FF,$00,$00,$00,$00,$00	; scan 3
charg114:
	.byte	$00,$00,$00,$00,$00,$FF,$FF,$00	; scan 7
	.byte	$00,$00,$00,$00,$00,$00,$FF,$FF	; scan 9

char123:
	.byte	$0E,$18,$18,$78,$18,$18,$0E,$00	; {
	.byte	$18,$18,$18,$18,$18,$18,$18,$00 ; |
	.byte	$70,$18,$18,$1E,$18,$18,$70,$00	; }
	.byte	$00,$00,$3B,$6E,$00,$00,$00,$00	; ~

;
;	scr40ext - exit the 80 column screen driver
;
;	Input:	None
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine exits the 40/80 column screen driver.
;	for now, just use the E: one
;

scr40ext:
	lda	#$E0		; put font pointer back
	sta	CHBAS
	jmp	scraeent

;
;	scr40put - put a character at COLCRS, ROWCRS
;
;	Input:	character to put in a-reg (use funny ascii)
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine puts a character at screen position COLCRS,ROWCRS.  
;	This routine does advance the cursor position.
;
scr40put:
	pha			; save the char code
	ldx	COLCRS		; get col nbr
	ldy	ROWCRS		; and row nbr
	jsr	scr40adrt	; figure out character address
	pla			; get the char back
	cmp	#$60		; lower case?
	bcc	scr40p0		; (blt) no, offset it
;
; handle graphics if necessary.  Look at the char set designator
; in either csg0 or csg1 (if altcs is set).  If it's csgraf, sub
; $20 from the char, to get graphics.  If it's csascii, leave the
; char alone.
;
	ldx	csg0		; assume g0 for starters
	ldy	altcs		; alt char set?
	beq	scr40px		; no, use this one
	ldx	csg1		; ok, use the alt one
scr40px:
	cpx	#csascii	; ascii font?
	beq	scr40p1		; yes, leave lc char the way it is
scr40p0:
	sec
	sbc	#$20		; compensate for atari font layout
scr40p1:
	ldy	reverse		; reverse vid?
	beq	scr40p2		; nope, go ahead
	ora	#$80		; or in hi bit for reverse vid.
scr40p2:
	ldy	#0
	sta	(dest),y	; and shove it in screen mem
	inc	COLCRS		; bump column
	jmp	scr40pan	; and go make sure we're visible

;
;	Scrpan		call scr40pan if necessary
;
scrpan:
	lda	scrtype		; what do we have here?
	cmp	#scr40		; 40 col pannable?
	beq	scr40pan	; yup, go do it
	rts
;
;	Scr40pan	make the current cursor pos visible by
;	panning left or right as needed
;
scr40pan:
	lda	COLCRS		; pan to make this column visible
	sec
	sbc	panval		; subtract current pan value
;
; want adjusted (visible) column between 2 and 37, if possible
;
	bmi	scr40pl		; neg, pan left
	cmp	#2		; < 2?
	bpl	scr40pr		; nope, try right side
scr40pl:
	lda	COLCRS		; yes, get real col
	sec
	sbc	#2		; - 2
	jmp	scr40p4		; and try to pan to there.
scr40pr:
	cmp	#37		; > 37?
	bmi	scr40p9		; nope, we're ok.  return
	lda	COLCRS		; get real col
	sec
	sbc	#37		; - 37
	jmp	scr40p5		; and try to pan to there.
scr40p4:
	cmp	#0		; only left to zero, please
	bpl	scr40p5
	lda	#0
scr40p5:
	cmp	#40		; better be less...
	bmi	scr40p6		; it is, ok
	lda	#40		; max at 40
scr40p6:
	sta	panval
	jsr	pan40		; make it happen
scr40p9:
	rts
;
;	Font80 - Character definitions
;
;	this defines the shape of the characters in 80 column mode
;	this table is in ascii sequence, offset by $20, ei 'space' is
;	the first thing here.  Graphics chars for VT100 line drawing
;	mode are the last 32 chars worth.
;
font80:
	.byte	$00,$00,$00,$00,$00,$00,$00,$00	;	' '
	.byte	$00,$44,$44,$44,$44,$00,$44,$00	;	'!'
	.byte	$00,$AA,$AA,$00,$00,$00,$00,$00	;	'"'
	.byte	$00,$AA,$EE,$AA,$EE,$AA,$00,$00	;	'#'
	.byte	$44,$66,$88,$44,$22,$EE,$44,$00	;	'$'
	.byte	$00,$99,$AA,$22,$55,$99,$00,$00	;	'%'
	.byte	$00,$44,$AA,$44,$AA,$AA,$55,$00	;	'&'
	.byte	$00,$22,$44,$00,$00,$00,$00,$00	;	'''
	.byte	$00,$22,$44,$44,$44,$44,$22,$00	;	'('
	.byte	$00,$44,$22,$22,$22,$22,$44,$00	;	')'
	.byte	$00,$99,$66,$FF,$66,$99,$00,$00	;	'*'
	.byte	$00,$00,$44,$EE,$44,$00,$00,$00	;	'+'
	.byte	$00,$00,$00,$00,$00,$22,$22,$44	;	','
	.byte	$00,$00,$00,$EE,$00,$00,$00,$00	;	'-'
	.byte	$00,$00,$00,$00,$00,$00,$44,$00	;	'.'
	.byte	$00,$22,$22,$44,$44,$88,$88,$00	;	'/'
	.byte	$00,$44,$AA,$EE,$AA,$AA,$44,$00	;	'0'
	.byte	$00,$44,$CC,$44,$44,$44,$EE,$00	;	'1'
	.byte	$00,$44,$AA,$22,$44,$88,$EE,$00	;	'2'
	.byte	$00,$EE,$22,$44,$22,$22,$CC,$00	;	'3'
	.byte	$00,$AA,$AA,$AA,$EE,$22,$22,$00	;	'4'
	.byte	$00,$EE,$88,$CC,$22,$22,$CC,$00	;	'5'
	.byte	$00,$44,$88,$CC,$AA,$AA,$44,$00	;	'6'
	.byte	$00,$EE,$22,$22,$44,$88,$88,$00	;	'7'
	.byte	$00,$44,$AA,$44,$AA,$AA,$44,$00	;	'8'
	.byte	$00,$44,$AA,$AA,$66,$44,$88,$00	;	'9'
	.byte	$00,$00,$44,$00,$00,$44,$00,$00	;	':'
	.byte	$00,$00,$44,$00,$44,$44,$88,$00	;	';'
	.byte	$00,$22,$44,$88,$44,$22,$00,$00	;	'<'
	.byte	$00,$00,$EE,$00,$EE,$00,$00,$00	;	'='
	.byte	$00,$88,$44,$22,$44,$88,$00,$00	;	'>'
	.byte	$00,$44,$AA,$22,$44,$00,$44,$00	;	'?'
	.byte	$00,$44,$EE,$AA,$88,$66,$00,$00	;	'@'
	.byte	$00,$44,$AA,$AA,$EE,$AA,$AA,$00	;	'A'
	.byte	$00,$CC,$AA,$CC,$AA,$AA,$CC,$00	;	'B'
	.byte	$00,$66,$88,$88,$88,$88,$66,$00	;	'C'
	.byte	$00,$CC,$AA,$AA,$AA,$AA,$CC,$00	;	'D'
	.byte	$00,$EE,$88,$CC,$88,$88,$EE,$00	;	'E'
	.byte	$00,$EE,$88,$CC,$88,$88,$88,$00	;	'F'
	.byte	$00,$44,$AA,$88,$AA,$AA,$44,$00	;	'G'
	.byte	$00,$AA,$AA,$EE,$AA,$AA,$AA,$00	;	'H'
	.byte	$00,$EE,$44,$44,$44,$44,$EE,$00	;	'I'
	.byte	$00,$66,$22,$22,$22,$AA,$44,$00	;	'J'
	.byte	$00,$AA,$AA,$CC,$AA,$AA,$AA,$00	;	'K'
	.byte	$00,$88,$88,$88,$88,$88,$EE,$00	;	'L'
	.byte	$00,$AA,$EE,$AA,$AA,$AA,$AA,$00	;	'M'
	.byte	$00,$CC,$AA,$AA,$AA,$AA,$AA,$00	;	'N'
	.byte	$00,$44,$AA,$AA,$AA,$AA,$44,$00	;	'O'
	.byte	$00,$CC,$AA,$AA,$CC,$88,$88,$00	;	'P'
	.byte	$00,$44,$AA,$AA,$AA,$AA,$44,$22	;	'Q'
	.byte	$00,$CC,$AA,$AA,$CC,$AA,$AA,$00	;	'R'
	.byte	$00,$66,$88,$44,$22,$22,$CC,$00	;	'S'
	.byte	$00,$EE,$44,$44,$44,$44,$44,$00	;	'T'
	.byte	$00,$AA,$AA,$AA,$AA,$AA,$EE,$00	;	'U'
	.byte	$00,$AA,$AA,$AA,$AA,$AA,$44,$00	;	'V'
	.byte	$00,$AA,$AA,$AA,$AA,$EE,$AA,$00	;	'W'
	.byte	$00,$AA,$AA,$44,$AA,$AA,$AA,$00	;	'X'
	.byte	$00,$AA,$AA,$AA,$44,$44,$44,$00	;	'Y'
	.byte	$00,$EE,$22,$44,$88,$88,$EE,$00	;	'Z'
	.byte	$00,$EE,$88,$88,$88,$88,$EE,$00	;	'['
	.byte	$00,$88,$88,$44,$44,$22,$22,$00	;	'\'
	.byte	$00,$EE,$22,$22,$22,$22,$EE,$00	;	']'
	.byte	$00,$44,$AA,$00,$00,$00,$00,$00	;	'^'
	.byte	$00,$00,$00,$00,$00,$00,$00,$FF	;	'_'
	.byte	$00,$44,$22,$00,$00,$00,$00,$00	;	'`'
	.byte	$00,$00,$CC,$22,$66,$AA,$EE,$00	;	'a'
	.byte	$00,$88,$CC,$AA,$AA,$AA,$CC,$00	;	'b'
	.byte	$00,$00,$66,$88,$88,$88,$66,$00	;	'c'
	.byte	$00,$22,$66,$AA,$AA,$AA,$66,$00	;	'd'
	.byte	$00,$00,$44,$AA,$EE,$88,$66,$00	;	'e'
	.byte	$00,$66,$88,$CC,$88,$88,$88,$00	;	'f'
	.byte	$00,$00,$44,$AA,$AA,$66,$22,$CC	;	'g'
	.byte	$00,$88,$CC,$AA,$AA,$AA,$AA,$00	;	'h'
	.byte	$00,$44,$00,$44,$44,$44,$44,$00	;	'i'
	.byte	$00,$22,$00,$22,$22,$22,$AA,$44	;	'j'
	.byte	$00,$88,$AA,$AA,$CC,$AA,$AA,$00	;	'k'
	.byte	$00,$CC,$44,$44,$44,$44,$EE,$00	;	'l'
	.byte	$00,$00,$AA,$EE,$AA,$AA,$AA,$00	;	'm'
	.byte	$00,$00,$CC,$AA,$AA,$AA,$AA,$00	;	'n'
	.byte	$00,$00,$44,$AA,$AA,$AA,$44,$00	;	'o'
	.byte	$00,$00,$CC,$AA,$AA,$CC,$88,$88	;	'p'
	.byte	$00,$00,$44,$AA,$AA,$66,$22,$33	;	'q'
;	.byte	$00,$00,$66,$88,$88,$88,$88,$00	;	'r'
	.byte	$00,$00,$CC,$AA,$88,$88,$88,$00	;	'r', experimental
	.byte	$00,$00,$66,$88,$44,$22,$CC,$00	;	's'
	.byte	$00,$44,$EE,$44,$44,$44,$66,$00	;	't'
	.byte	$00,$00,$AA,$AA,$AA,$AA,$EE,$00	;	'u'
	.byte	$00,$00,$AA,$AA,$AA,$AA,$44,$00	;	'v'
	.byte	$00,$00,$AA,$AA,$AA,$EE,$AA,$00	;	'w'
	.byte	$00,$00,$AA,$AA,$44,$AA,$AA,$00	;	'x'
	.byte	$00,$00,$AA,$AA,$AA,$66,$22,$CC	;	'y'
	.byte	$00,$00,$EE,$22,$44,$88,$EE,$00	;	'z'
	.byte	$66,$44,$44,$CC,$44,$44,$66,$00	;	'{'
	.byte	$44,$44,$44,$44,$44,$44,$44,$00	;	'|'
	.byte	$66,$22,$22,$33,$22,$22,$66,$00	;	'}'
	.byte	$00,$55,$AA,$00,$00,$00,$00,$00	;	'~'
	.byte	$00,$00,$00,$00,$00,$00,$00,$00	; space after tilde?
;
;	graphics, chars $60 thru $7f
;
	.byte	$00,$44,$44,$EE,$EE,$44,$44,$00	; blot
	.byte	$AA,$55,$AA,$55,$AA,$55,$AA,$55	; square blot
	.byte	$00,$AA,$EE,$AA,$77,$22,$22,$00	; h/t
	.byte	$00,$EE,$CC,$88,$77,$66,$44,$00	; f/f
	.byte	$00,$CC,$88,$EE,$55,$66,$55,$00	; c/r
	.byte	$00,$88,$88,$EE,$77,$66,$44,$00	; l/f
	.byte	$00,$EE,$AA,$EE,$00,$00,$00,$00	; degrees
	.byte	$00,$00,$44,$EE,$44,$44,$EE,$00	; plus/minus
	.byte	$00,$AA,$EE,$AA,$44,$44,$77,$00	; n/l
	.byte	$00,$AA,$AA,$44,$77,$22,$22,$00	; v/t
	.byte	$44,$44,$44,$CC,$00,$00,$00,$00	; down right corner
	.byte	$00,$00,$00,$CC,$44,$44,$44,$44	; up right corner
	.byte	$00,$00,$00,$77,$44,$44,$44,$44	; up left corner
	.byte	$44,$44,$44,$77,$00,$00,$00,$00	; down left corner
	.byte	$44,$44,$44,$FF,$44,$44,$44,$44	; center cross
	.byte	$FF,$00,$00,$00,$00,$00,$00,$00	; scan 1
	.byte	$00,$FF,$00,$00,$00,$00,$00,$00 ; scan 3 (really 2)
	.byte	$00,$00,$00,$FF,$00,$00,$00,$00 ; scan 5 (really 4)
	.byte	$00,$00,$00,$00,$00,$FF,$00,$00	; scan 7 (really 6)
	.byte	$00,$00,$00,$00,$00,$00,$00,$FF	; scan 9 (really 8)
	.byte	$44,$44,$44,$77,$44,$44,$44,$44	; left t
	.byte	$44,$44,$44,$CC,$44,$44,$44,$44	; right t
	.byte	$44,$44,$44,$FF,$00,$00,$00,$00	; bottom t
	.byte	$00,$00,$00,$FF,$44,$44,$44,$44	; top t
	.byte	$44,$44,$44,$44,$44,$44,$44,$44	; vert bar
	.byte	$00,$22,$44,$88,$44,$22,$EE,$00	; <=
	.byte	$00,$88,$44,$22,$44,$88,$EE,$00	; >=
	.byte	$00,$00,$00,$EE,$AA,$AA,$AA,$00	; pi
	.byte	$00,$22,$EE,$44,$EE,$88,$00,$00	; not equal
	.byte	$00,$CC,$88,$CC,$88,$88,$EE,$00	; lbs Sterling
	.byte	$00,$00,$00,$66,$66,$00,$00,$00	; center dot
; $7f ever used?  if so, it goes here

;
;	scr40el0 - Perform the vt100 erase line function #0 on 80 column screen
;
;	Input:	number of line to erase in rowcrs
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine erases from the cursor to the end of the line
;

scr40el0:
	ldy	ROWCRS
	ldx	COLCRS
	jsr	scr40adrt	; figure out where to start
	ldy	#0
	lda	#80		; subtract current col to get count
	sec
	sbc	COLCRS
	beq	scr40el0z	; done!
	tax
	lda	#0		; space minus offset
	jsr	fillx		; fill x many with (a)
scr40el0z:
	rts			; all done

;
;	scr40el1 - Perform the VT100 Erase Line function #1 on 80 column screen
;
;	Input:	Number of line to erase in ROWCRS
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine erases from the beginning of line to cursor
;

scr40el1:
	ldy	ROWCRS
	ldx	#0
	jsr	scr40adrt	; compute the cursors address
	ldy	#0		; index from start in screen mem
	ldx	COLCRS		; this many
	beq	scr40el1z
	lda	#0		; space minue offset
	jsr	fillx		; fill x many
scr40el1z:
	rts			; all done

;
;	scr40el2 - Perform the VT100 Erase Line function #2 on 40/80 column screen
;
;	Input:	Number of line to erase in ROWCRS
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine erases one line compleatly from the 40/80 column display.
;

scr40el2:
	lda	COLCRS
	pha			; save cursor temporarily
	lda	#0
	sta	COLCRS
	jsr	scr40el0	; whack from temp cursor to eol
	pla
	sta	COLCRS		; put it back
	rts

;
;	scr40ind - perform the VT100 index function (scroll the screen)
;
;	Input:	None
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine scrolls the screen in 40/80 column mode.  Only the area
;	in the scrolling region is changed.
;

scr40ind:
	lda	top		; get top row to scroll
	sta	strptr		; we'll use that as row counter
scr40ind1:
	tay			; get row into y
	iny			; top + 1
	ldx	#0		; col 1 again
	jsr	scr40adrt
	lda	dest		; this wants
	sta	source		;  to be the
	lda	dest+1		;   source addr
	sta	source+1
	ldy	strptr		; get row again
	ldx	#0		; col 1
	jsr	scr40adrt	; find address
	ldx	#80		; byte count
	jsr	movex		; move x many
	inc	strptr		; bump row counter
	lda	strptr		; get it
	cmp	bot		; we there yet?
	bcc	scr40ind1	; less than, go back for more

	ldx	#0
	ldy	bot		; clear bottom line
	jsr	scr40adrt
	ldx	#80		; byte count
	lda	#0		; space minus offset
	jsr	fillx		; fill x many
	rts			; go home

;
;	scr40ri - perform the VT100 reverse index function (scroll backwards)
;
;	Input:	None
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine scrolls the screen in 80 column mode.  Only the area
;	in the scrolling region is changed.
;
scr40ri:
	lda	bot		; bottom row
	sta	strptr		; handy temp row counter
scr40ri1:
	tay			; get it into Y
	dey			; bot - 1
	ldx	#0		; col 1 again
	jsr	scr40adrt
	lda	dest		; this wants
	sta	source		;  to be the
	lda	dest+1		;   source addr
	sta	source+1
	ldy	strptr		; row again
	ldx	#0		; col 1
	jsr	scr40adrt	; find address
	ldx	#80		; byte count
	jsr	movex
	dec	strptr		; dec row index
	lda	strptr		; get it
	cmp	top		; we there yet?
	bne	scr40ri1	; nope, do anoter one

	ldx	#0
	ldy	top		; clear bottom line
	jsr	scr40adrt
	ldx	#80
	lda	#0		; space minus offset
	jsr	fillx
	rts			; go home

;
;	scr40fls - flash the screen and cursor in 80 column mode
;
;	Input:	None
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine flashes the screen in 40/80 column mode
;	No op on Atari...
;

scr40fls: rts

;
;	scr40tgl - toggle the cursor in 40/80 column mode
;
;	Input:	None
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine toggles the cursor in 80 column mode.
;

scr40tgl:
;	ldy	ROWCRS		; compute cursor address
;	ldx	COLCRS
	sec
	jsr	ploth		; filter for rightmost col
	jsr	scr40adrt
	ldy	#0
	lda	(dest),y	; get old character value
	eor	#$80		; toggle hi bit
	sta	(dest),y	; stick it back
scr40tgl1: rts


;
;	scr40adr - calculate 80*y+x
;
;	Input:	numbers in x-reg and y-reg
;	Output: dest
;
;	Registers destroyed - A,Y
;
;	This routine calculates 80*y+x and puts the result in dest.  If x > 80,
;	one is subtracted first.  This will happen after a character is printed
;	on the last character on a line.  This routine is for calculating
;	screen addresses.
;

scr40adr:
	sty	dest		; put y-reg in dest
	lda	#0		; zero extend
	sta	dest+1
	asl	dest		; multiplied by 2
	rol	dest+1
	asl	dest		; multiplied by 4
	rol	dest+1
	tya			; add in y to get 5*y
	adc	dest		; carry is clear
	sta	dest
	bcc	scr40adr1
	inc	dest+1
scr40adr1: asl	dest		; multiplied by 10
	rol	dest+1
	asl	dest		; multiplied by 20
	rol	dest+1
	asl	dest		; multiplied by 40
	rol	dest+1
	asl	dest		; multiplied by 80
	rol	dest+1
	cpx	#80		; are we in the funny col?
	bcc	scr40adr2	; no
	ldx	#79
scr40adr2: txa			; add in x-reg
	clc
	adc	dest
	sta	dest
	bcc	scr40adr3
	inc	dest+1
scr40adr3: rts			; all done

;
;	scr40adrt - calculate address of a text character for 40/80 column mode
;
;	Input:	x coordinate in x-reg
;		y coordinate in y-reg
;	Output: dest
;
;	Registers destroyed - A,X,Y
;
;	This routine calculates the address of a character at x,y in 40/80
;	column mode.  It uses scr40adr to set things up and adds scrmem base
;

scr40adrt:
	jsr	scr40adr		; freak out scraeadr
	lda	#<scrmemlo		; add base addr
	clc
	adc	dest
	sta	dest
	lda	#>scrmemlo
	adc	dest+1
	sta	dest+1
	rts
;
; Stuff for panning the screen in 40/80 mode
;

;
;	Pan40	frobnicate the display list pointers to point
;	wherever they normally would plus panval.
;
pan40:
	lda	ROWCRS		; save cursor
	pha			;  coords thru this
	lda	COLCRS
	pha
	lda	#0		; now zap them to zero
	sta	ROWCRS
	sta	COLCRS
	lda	#<(dlist+3)	; addr of line 0 display list inst
	sta	source
	lda	#>(dlist+3)
	sta	source+1
pan40a:
	ldx	COLCRS
	ldy	ROWCRS
	jsr	scr40adrt	; figure out base address
	lda	ROWCRS		; get offset into display list
	clc
	adc	ROWCRS		; they're 3 bytes long
	adc	ROWCRS
	tay			; into y
	iny			; point at addr lo byte in DL
	lda	dest		; stuff in screen mem ptr lo
	clc
	adc	panval		; plus pan value
	sta	(source),y
	lda	dest+1		; get hi byte
	adc	#0		; plus carry
	iny
	sta	(source),y
	inc	ROWCRS		; next line please
	lda	ROWCRS
	cmp	#24		; off the end?
	bne	pan40a
	pla			; get cursor back
	sta	COLCRS
	pla
	sta	ROWCRS
	rts			; done!

;
; frob for updating status line flds that change on the fly.
;
updstat:
	lda	#$1D+$40	; internal code for down arrow
	sec
	sbc	capslck		; buys up-arrow if set
	sta	statline+26
;
	lda	fxoff		; him x'ed off?
	beq	updsxon		; no, he's on
	lda	#'-'-$20
	bne	updsx
updsxon:
	lda	#'+'-$20
updsx:	sta	statline+22
;
	lda	suspend		; suspended?
	beq	updss		; nope, display the 0, shows up as space
	lda	#$14+$40	; display a blot
	ldx	scrtype		; what kind of screen are we using?
	cpx	#scr40		; a 40-col one?
	bne	updss		; nope, it's ok
	lda	#$00+$40	; blot in 40-col font
updss:	sta	statline+24
; zzz more later
	rts 

;
; Make a display list preamble, for both 40/80 and graphics screens
;
makedlp:
	lda	#<dlist		; start of display list mem
	sta	dest
	lda	#>dlist
	sta	dest+1
	ldy	#0
	lda	#$70		; 8 blank lines
	sta	(dest),y
	iny
	sta	(dest),y	; 8 more
	iny
	lda	#$20		; 3 blank lines
	sta	(dest),y
	iny
	lda	#<scrmemlo	; set source up to point at screen mem
	sta	source
	lda	#>scrmemlo
	sta	source+1
	rts

;
; Make the common postamble
;
makedlx:
	lda	#$42		; character mode please
	sta	(dest),y
	iny
	lda	#<statline	; point at status line
	sta	(dest),y
	iny
	lda	#>statline
	sta	(dest),y
	iny
	lda	#$41		; jump and wait for Vsync
	sta	(dest),y
	iny
	lda	#<dlist		; back to beginning of list
	sta	(dest),y
	iny
	lda	#>dlist
	sta	(dest),y	; done!
	rts	
;
; Make a display list for the 40/80 pannable screen
;
makedl40:
	jsr	makedlp		; do the preamble
	ldx	#24		; 24 lines like this
mkdl40a:
	lda	#$42		; load scan pointer, char mode instruction
	sta	(dest),y
	iny
	lda	source		; get pointer to block of mem
	sta	(dest),y
	iny
	clc			; bump it by 80 while we're at it, for next pass
	adc	#80
	sta	source
	lda	source+1	; get hi byte
	sta	(dest),y	; into display list
	iny
	adc	#0		; add carry from lo byte
	sta	source+1
	dex			; dec counter
	bne	mkdl40a		; more lines, go do them
	jsr	makedlx		; make the postamble
	rts

;
; Make a display list for the 80 column graphics screen
;
makedl80:
	jsr	makedlp		; do the preamble
	lda	#$4F		; hi-res graphics please, 
	sta	(dest),y
	iny
	lda	#<scrmemlo	; starting at screen mem
	sta	(dest),y
	iny
	lda	#>scrmemlo
	sta	(dest),y
	iny
	jsr	mkdl80z		; make 95 rasters worth
	lda	#$4F		; hi-res graphics please, 
	sta	(dest),y
	iny
	lda	#<scrmemhi	; starting at screen mem
	sta	(dest),y
	iny
	lda	#>scrmemhi
	sta	(dest),y
	iny
	jsr	mkdl80z		; make another 95 rasters worth
	jsr	makedlx		; make the postamble
	rts			; done!

mkdl80z:
	ldx	#95		; 95 more rasters of hi-res graphics
	lda	#$0F
mkdl80a:
	sta	(dest),y
	iny
	dex
	bne	mkdl80a
	rts

;ctrl-l.SBTTL	40 Column screen driver

;
;	These routines manipulate the screen in 40 column mode.
;

;
;	scraeini - initilize the 40 column screen
;
;	Input:	None
;
scraeini:
	rts			; all done

;
;	scraeent - enter the 40 column screen driver
;
;	Input:	None
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine starts the 40 column screen driver.
;	these days, that's just resetting the real display list to the one we
;	got when we opened the screen, turning off wrap, 
;	and resetting the colors
;

scraeent:
	lda	scraedl
	ldy	scraedl+1
	jsr	setdlist	; go set it safely (?)
	lda	scraec1		; get color1 value
	sta	COLOR1
	lda	scraec2
	sta	COLOR2
	lda	scraec4
	sta	COLOR4
	lda	scraer		; get orig row value
	sta	ROWCRS
	lda	#$FF		; wrap off, please
	sta	wrap
	lda	#scrae		; go back to atari E: screen
	sta	scrtype
	rts			; all done

;
;	scraeext - exit the E: screen driver
;	Not really.  Just here for compatibility

;	Input:	None
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine exits from the 40 column screen driver.
;

scraeext:
	lda	SDLSTL		; remember display list addr for when we change
	sta	scraedl
	lda	SDLSTH
	sta	scraedl+1
	lda	COLOR1		; remember color settings
	sta	scraec1
	lda	COLOR2
	sta	scraec2
	lda	COLOR4
	sta	scraec4
	lda	ROWCRS
	sta	scraer		; remember row value
	rts			; all done

;	scraeput - put a character at COLCRS, ROWCRS
;
;	Input:	character to put in a-reg (use funny ascii)
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine puts a character at screen position COLCRS,ROWCRS.  
;	This routine does advance the cursor position.
;

scraeput:
	jsr	sputch		; put it using the builting E: driver
;	dec	COLCRS		; put column counter back
;	bpl	scraeput4	; should be pos
;	lda	#0		; no? ok zap it
;	sta	COLCRS
scraeput4: rts			; all done.

;
;	scraeel0 - Perform the VT100 Erase Line function #0 on 40 column screen
;
;	Input:	Number of line to erase in ROWCRS
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine erases from the cursor to the end of the line
;

scraeel0:
	lda	COLCRS		; get column
	pha			; save it
scrael01:
	lda	#39		; at eol yet?
	cmp	COLCRS
	beq	scrael02	; yup, go home
	lda	#space		; get a space
	jsr	sputch		; shove it out
	jmp	scrael01	; go around again
scrael02:
	pla			; get column back
	sta	COLCRS
	rts

;
;	scraeel1 - Perform the VT100 Erase Line function #1 on 40 column screen
;
;	Input:	Number of line to erase in ROWCRS
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine erases from the beginning of line to cursor
;

scraeel1:
	lda	COLCRS		; get col
	pha			; save it
	lda	#0		; and zap it
	sta	COLCRS
scrael11:
	pla			; get original col back
	cmp	COLCRS		; are we home yet?
	beq	scrael12	; yup, go home
	pha			; nope, save it again
	lda	#space		; get a space
	jsr	sputch		; shove it out
	jmp	scrael11	; go around again
scrael12:
	rts

;
;	scraeel2 - Perform the VT100 Erase Line function #2 on 40 column screen
;
;	Input:	Number of line to erase in ROWCRS
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This routine erases one line compleatly from the 40 column display.
;

scraeel2:
	lda	COLCRS		; save column
	pha
	lda	#0		; zap it to 0
	sta	COLCRS
	jsr	scraeel0	; zap to eol
	pla			; get it back
	sta	COLCRS
	rts			; all done

;
;	scraeind - perform the VT100 index function (scroll the screen)
;
;	Input:	None
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	Can't do this in E: device, so it's a no-op
;
;

scraeind: 
	rts
	
;
;	scraeri - perform the VT100 reverse index function (scroll backwards)
;
;	Input:	None
;	Output: None
;
;	Registers destroyed - A,X,Y
;
;	This one too.
;

scraeri: rts			; zzz can we do this?
	
;
;	scraefls - flash the screen in 40 column mode
;	No-op
;

scraefls:
	rts			; all done

;
;	scraetgl - toggle the cursor in ae column mode
;	This is really only useful for making the cursor show up
;	in the right place.
;

scraetgl:
	lda	#$1F		; go right a char
	jsr	sputch
	lda	#$1E		; go left a char.  Sigh
	jsr	sputch
	rts

;
; 80-col stuff
;

;
;	Scr80ent.	Enter the 80-column graphics screen
;
scr80ent:
	jsr	makedl80
	lda	scr80dl
	ldy	scr80dl+1
	jsr	setdlist
	lda	backclr		; grey, hi luminance
	sta	COLOR2		;  shows up on 0 bits
	lda	foreclr		; grey, lo luminance (only lum counts)
	sta	COLOR1		;  shows up on 1 bits
	lda	bordclr		; make background like 0 bits, a little
	sta	COLOR4		;  dimmer
	lda	#scr80		; we're now in graphics mode
	sta	scrtype
	rts

;
;	Scr80ext.	Exit the 80-col graphics screen
;
scr80ext:
	jmp	scraeent	; back to atari mode
	
;
;	scr80adr - calculate int(x/2) and x%2
;
;	Input:	number in x-reg
;	Output: evenodd = $0f if x is odd, $f0 if x is even
;		x-reg = x-reg/2
;
;	Registers destroyed - A,X
;
;	This routine calculated int(x/2) and x % 2.  It is used to freak
;	scraeadr into calculating addresses for 80 column mode.  Real
;	funny things happen if the x-reg is the funny column (81).
;

scr80adr:
	cpx	#80		; is the cursor in the funny column?
	bcc	scr80adr2		; no
	ldx	#81		; 81 % 2 = 1
scr80adr2:
	txa			; divide x by two
	lsr	a
	tax			; put result back in x-reg
	lda	#$0F		; put $0f in evenodd if odd
	bcs	scr80adr1		; is odd
	lda	#$F0		; put $f0 in evenodd if even
scr80adr1:
	sta	evenodd
	rts

;
; 	scr80adrt.  compute (y * 320) + (x / 2) + scrmem, leave it in dest
; 	actually, mem is split, but we hide all that in here
;
scr80adrt:
	tya			; save this value
	pha
	cmp	#12		; which half?
	bcc	s80a1		; lo half, leave it
	sbc	#12		; sub 12, carry's set
s80a1:
	sta	dest
	lda	#0
	sta	dest+1		; zap top half
	jsr	scr80adr	; do the even/odd stuff
	asl	dest		; * 2
	lda	dest		; we'll need this in a sec
	asl	dest		; * 4
	asl	dest		; * 8
	adc	dest		; carry's clear, from shifts
	sta	dest		; * 10
	asl	dest
	rol	dest+1		; * 20
	asl	dest
	rol	dest+1		; * 40
	asl	dest
	rol	dest+1		; * 80
	asl	dest
	rol	dest+1		; * 160
	asl	dest
	rol	dest+1		; * 320
	txa			; x has col / 2 in it
	adc	dest		; carry's clear from shifts
	sta	dest
	lda	dest+1
	adc	#0		; add carry to hi byte
	sta	dest+1
	pla			; now add lo or hi mem base
	cmp	#12
	bcs	s80a2		; hi half
	lda	dest		; now add screen mem lo
	adc	#<scrmemlo
	sta	dest
	lda	dest+1
	adc	#>scrmemlo
	sta	dest+1
	rts
s80a2:
	clc
	lda	dest		; now add screen mem lo
	adc	#<scrmemhi
	sta	dest
	lda	dest+1
	adc	#>scrmemhi
	sta	dest+1
	rts
;
; Put a byte in 80-col graphics mode
;
scr80put:
	and	#$7F		; seven bits only, here
;
; first figure out if we're doing graphics
;
	cmp	#$60		; lower case?
	bcc	scr80p1		; nope, do the subtract
	ldx	csg0		; default, G0
	ldy	altcs		; using alternate instead?
	beq	scr80p0		; nope, use this one
	ldx	csg1		; ok, use G1
scr80p0:
	cpx	#csascii	; using ascii?
	bne	scr80p2		; no, skip the subtract, for graphics
scr80p1:
	sec
	sbc	#$20		; offset for font80
scr80p2:
	pha			; save character put
	sta	source		; compute character*8+font80
	lda	#0
	sta	source+1
	asl	source		; multiplied by 2
	rol	source+1
	asl	source		; multiplied by 4
	rol	source+1
	asl	source		; multiplied by 8
	rol	source+1
	lda	source		; now add in font80
	adc	#<font80	; carry is clear
	sta	source
	lda	source+1
	adc	#>font80
	sta	source+1
	ldy	ROWCRS		; compute the address to store at
	ldx	COLCRS
	jsr	scr80adrt		
	ldy	#0		; index into font
scr80put1:
	ldx	#0		; to index thru dest
	lda	(dest,x)	; select hi or low half      abcdefgh
	eor	(source),y	;			     ABCDEFGH
	and	evenodd		;			     xxxx0000
	eor	(dest,x)	;			     ABCDefgh
	cpy	#7		; on row 7?
	bne	scr80put8	; nope, don't check for underlining
	ldx	underln		; underlining?
	beq	scr80put8	; nope
	ora	evenodd		; yes, set all bits this row
scr80put8:
	ldx	reverse		; $01 is reverse on, $00 is reverse off
	beq	scr80put7
	eor	evenodd		; reverse the character
scr80put7:
	ldx	#0		; for indexing again
	sta	(dest,x)	; finally put it back
	lda	dest		; bump dest by 40
	clc
	adc	#40
	sta	dest
	lda	dest+1
	adc	#0
	sta	dest+1
	iny
	cpy	#8		; off end of font yet?
	bcc	scr80put1	; put in the entire character (8bytes)
	jmp	scr80put2
scr80put6:
;	ldy	#$07
;	lda	evenodd
;	eor	#$ff
;	and	(dest),y
;	sta	(dest),y
scr80put2:
; not round here...
	pla			; check to see if color must be updated
;	bne	scr80put3		; if character is not a space, update
;	lda	reverse		; if reverse on, update
;	bne	scr80put3
;	lda	underln		; if underline on, update
;	beq	scr80put4
scr80put3:
;	ldy	ROWCRS		; calculate primary color address
;	ldx	COLCRS
;	jsr	scr80adrp
;	ldx	alternt		; 1=alternate color, 0=normal color
;	lda	foreclr,x	; get proper foreground color
;	asl	a		; put in high nybble
;	asl	a
;	asl	a
;	asl	a
;	ora	backclr		; or in background color
;	ldy	#0
;	sta	(dest),y	; adjust primary color ram
;	pha			; save for future use
;	ldy	ROWCRS		; compute alternate color address
;	ldx	COLCRS
;	jsr	scr80adra
;	pla			; restore colors used for primary color
;	ldx	flash		; can we use it?
;	beq	scr80put5		; yes.
;	lda	backclr		; no. screen is flashing.
;	asl	a		; use background color for forground
;	asl	a
;	asl	a
;	asl	a
;	ora	backclr
scr80put5:
;	ldy	#0
;	sta	(dest),y	; adjust alternate color ram
scr80put4:
	inc	COLCRS		; bump column
	rts			; all done.

; stubs
scr80fls: rts

scr80tgl:
;	ldx	COLCRS		; figure out where we are
;	ldy	ROWCRS
	sec
	jsr	ploth		; filter for rightmost col
	jsr	scr80adrt
	ldx	#8		; do 8 slots
	ldy	#0
scr80t1:
	lda	(dest),y	; get screen data
	eor	evenodd		; toggle the proper half
	sta	(dest),y	; put it back
	lda	dest		; bump pointer by 40
	clc
	adc	#40
	sta	dest
	bcc	scr80t2		; no carry, don't bother with top
	inc	dest+1
scr80t2:
	dex			; dec raster counter
	bne	scr80t1		; go back for another raster
	rts

;
;	Scr80el0:	Erase from cursor to EOL in 80-col mode
;
scr80el0: 
	ldx	COLCRS		; get col,
	ldy	ROWCRS		; and row,
	jsr	scr80adrt	; and figure out where we are
	stx	strptr+1	; x still has col / 2 in it
	lda	#8		; init raster counter
	sta	strptr		; handy temp
scr80e0a:
	ldy	#0		; start index
	lda	#40		; line length, bytes
	ldx	evenodd		; $0F if on odd col
	bmi	scr80e0b	; $F0, even col, so go ahead
	lda	#$F0		; make a mask for the col we want
	and	(dest),y	; mask it
	sta	(dest),y	; and put it back
	iny			; skip this byte
	lda	#39		; and count will be one less
scr80e0b:
	sec
	sbc	strptr+1	; remaining byte count
	beq	scr80e0d	; zero? ok, already at eol
	tax			; back to x
	lda	#0
scr80e0c:
	sta	(dest),y	; clear rest of raster
	iny
	dex
	bne	scr80e0c
scr80e0d:
	lda	dest		; get byte pointer
	clc
	adc	#40		; next row please
	sta	dest
	lda	dest+1
	adc	#0
	sta	dest+1
	dec	strptr		; next raster
	bne	scr80e0a	; done!
	rts

;
;	Scr80el1	zap from beginning of line to cursor
;
scr80el1: 
	ldx	#0		; get col,
	cpx	COLCRS		; already at 0?
	beq	scr80e1z	; yup, quit here
	ldy	ROWCRS		; and row,
	jsr	scr80adrt	; and figure out where we are
	ldx	COLCRS
	jsr	scr80adr	; figure out even/odd of this coll
	stx	strptr+1	; x has col / 2 in it
	lda	#8		; init raster counter
	sta	strptr		; handy temp
scr80e1a:
	lda	evenodd		; $0F if on odd col
	bpl	scr80e1b	; odd col, clear up to it
	ldy	strptr+1	; get the byte pos
	and	(dest),y	; mask it
	sta	(dest),y	; and put it back
scr80e1b:
	ldx	strptr+1	; remaining byte count
	beq	scr80e1d	; zero? ok, done
	lda	#0
	jsr	fillx		; fill that many
scr80e1d:
	lda	dest		; get byte pointer
	clc
	adc	#40		; next row please
	sta	dest
	lda	dest+1
	adc	#0
	sta	dest+1
	dec	strptr		; next raster
	bne	scr80e1a	; done!
scr80e1z:
	rts
; 
;	scr80el2 - zap current line
;
scr80el2: 
	ldx	#0
	ldy	ROWCRS
	jsr	scr80adrt	; get base addr of this row
	lda	#<320		; clear 320 bytes
	sta	count
	lda	#>320
	sta	count+1
	jsr	clearn
	rts

scr80ind: 
	lda	top		; init row counter
	sta	strptr		; use this as a temp
scr80i1:
	jsr	comsta		; this is slow, so make sure we
	jsr	flowco		; pay attention to line
	ldx	#0		; compute
	ldy	strptr		;  row start addr
	iny
	jsr	scr80adrt
	lda	dest
	sta	source
	lda	dest+1
	sta	source+1
	ldx	#0
	ldy	strptr
	jsr	scr80adrt	; and target row start addr
	lda	#<320		; 320 bytes per row
	sta	count
	lda	#>320
	sta	count+1
	jsr	moven		; move some
	inc	strptr		; bump row counter
	lda	strptr
	cmp	bot		; end yet?
	bcc	scr80i1		; nope, do another row
	lda	ROWCRS		; save row
	pha
	lda	bot
	sta	ROWCRS
	jsr	scr80el2	; zap this line
	pla
	sta	ROWCRS		; put row back
	rts

scr80ri:
	lda	bot		; init row counter
	sta	strptr		; use this as a temp
scr80r1:
	jsr	comsta		; this is slow, so make sure we
	jsr	flowco		; pay attention to line
	ldx	#0		; compute
	ldy	strptr		;  row start addr
	dey			; row above's source
	jsr	scr80adrt
	lda	dest
	sta	source
	lda	dest+1
	sta	source+1
	ldx	#0
	ldy	strptr
	jsr	scr80adrt	; and target row start addr
	lda	#<320		; 320 bytes per row
	sta	count
	lda	#>320
	sta	count+1
	jsr	moven		; move some
	dec	strptr		; dec row counter
	lda	strptr		; at top row yet?
	cmp	top
	bne	scr80r1		; nope, do another row
	lda	ROWCRS		; save row
	pha
	lda	top
	sta	ROWCRS
	jsr	scr80el2	; zap this line
	pla
	sta	ROWCRS		; put row back
	rts

;
;	slput:	put a string in the status line.  A,Y point to string.
;		X is initial offset in status line
;		terminates on EOL or nul. clears to end of stat line
;
slput:
	sta	strptr		; set up the string pointer
	sty	strptr+1
	ldy	#0
slp1:	lda	(strptr),y	; get a byte
	beq	slp8		; done!
	cmp	#ATEOL		; eol?
	beq	slp9		; yup, done
	cmp	#$60		; lower case?
	bcs	slp2		; yes, leave it alone
	sec
	sbc	#$20		; offset into font
slp2:	sta	statline,x	; stuff it in
	inx
	iny			; next!
	jmp	slp1
slp8:
	txa			; save x
	pha
	lda	#0		; space, sort of
slp9:	sta	statline,x
	inx
	cpx	#40
	bcc	slp9
	pla
	tax
	rts			; home

;ctrl-l.SBTTL	Miscellaneous routines

;
;	These are miscellaneous routines used in many different places
;

;
;	Moven - move (source) to (dest) for (count) bytes
;
;	Input: (count) - byte count to move
;	       (source) - address of source of memory move
;	       (dest) - address of destination of memory move
;
;	Output: Memory is moved
;
;	Registers Destroyed: A,X,Y
;
moven:
	lda	count+1		; more 256 byte chunks?
	beq	moven1		; nope, go get the rest
	jsr	move256
	inc	source+1
	inc	dest+1
	dec	count+1
	bne	moven		; try for more
moven1:
	ldx	count		; any left?
	beq	moven2		; nope, done
	jsr	movex		; move that many
moven2:
	rts
;
; Move 256 bytes (source) to (dest)
;
move256:
	ldy	#0
mov256a:
	lda	(source),y	; move 1
	sta	(dest),y
	iny			; bump
	bne	mov256a		; if not wrapped, go back
	rts			; done
;
; Move X bytes (source) to (dest)
;
movex:
	ldy	#0
movexa:
	lda	(source),y	; move 1
	sta	(dest),y
	iny			; bump
	dex			; dec count
	bne	movexa		; non zero, go back for more
	rts
;
;	Move 8 * (x) bytes
;	
move8:
	stx	count		; lo byte of count
	lda	#0
	sta	count+1
	asl	count
	rol	count+1
	asl	count
	rol	count+1
	asl	count
	rol	count+1
	jmp	moven

;
;	clearn - clear (dest) for (count)
;
;	Input: (count) - byte count to fill
;	       (dest) - address of destination of memory move
;
;	Output: Memory is cleared
;
;	Registers Destroyed: A,X,Y
;

clearn:	lda #0			; clear memory by filling with $00
	jsr filln
	rts
	
;
;	Filln - fill (dest) for (count) with what's in A
;
;	Input: (count) - byte count to fill
;	       A - Byte to fill memory with
;	       (dest) - address of destination of memory move
;
;	Output: Memory is filled
;
;	Registers Destroyed: A,X,Y
;
filln:
	ldx	count+1		; more 256 byte chunks?
	beq	filln1		; nope, go get the rest
	jsr	fill256
	inc	source+1
	inc	dest+1
	dec	count+1
	bne	filln		; try for more
filln1:
	ldx	count		; any left?
	beq	filln2		; nope, done
	jsr	fillx		; move that many
filln2:
	rts

;
; fill 256 bytes (dest)
;
fill256:
	ldy	#0
fil256a:
	sta	(dest),y
	iny			; bump
	bne	fil256a		; if not wrapped, go back
	rts			; done
;
; fill X bytes (dest)
;
fillx:
	ldy	#0
fillxa:
	sta	(dest),y
	iny			; bump
	dex			; dec count
	bne	fillxa		; non zero, go back for more
	rts

;
;	Case - Pascal like case function
;
;	Input: Y - Case statement to select
;	           The addresses of the routines to select are compiled inline
;
;	Registers Destroyed: X, Y
;
;	this routine transfers controll to a routine selected by the X register
;

case:	tax			; preserve a-reg across case statement
	pla			; get lo bype of case list
	sta source		; save it
	pla			; get hi byte of case list
	sta source+1		; save it
	tya			; put case selector into a-reg
	sec			; add one half
	rol a			; and multiply by two
	tay			; put (2*case_selector)+1 into y-reg
	lda (source),y		; get lo byte of routine to go to
	sta dest		; save it
	iny			; prepare to get hi byte of routines address
	lda (source),y		; get hi byte of routines address
	sta dest+1		; save it
	txa			; preserve a-reg across case statement
	jmp (dest)		; go to appropriate

anyrts:	rts			; a handy return from subroutine instruction
anybrk:	brk			; a handy break instruction

	
;
; Set the OS's display list cells.
; This may need to be done without interrupts or something zzz
;
setdlist:
	ldx	#1
	stx	CRITIC		; say we're doing something critical
	ldx	SDMCTL		; get old DMACTL value
	stx	dest		; save it here for a bit
	ldx	#0
	stx	DMACTL		; and whack the real one while
	sta	SDLSTL		;  we indulge in some sleight of hand
	sty	SDLSTH
	ldx	dest		; get old dma ctrl value back
	stx	DMACTL
	dec	CRITIC
	rts
;

;end_asm: =	*
;ctrl-l.SBTTL	Data for the screen package

scraedl: .word	0	; the original display list addr for E:
scraec1: .byte	0	; original color1 value
scraec2: .byte	0	; original color2 value
scraec4: .byte	0	; original color4 (background) value
scraer:	.byte	0	; original cursor row value
scr40dl: .word	dlist	; display list addr for 40/80 col screen
scr80dl: .word	dlist	; display list addr for 80 col

panval:	.byte	0	; pan offset for 40/80 mode

backclr: .byte	$0A	; background color, grey, hi lum
foreclr: .byte	$02	; foreground color, grey lo lum
;altclr:	.byte	$ff	; alternate color
bordclr: .byte	$08	; border color, grey, med lum
;
top:	.byte	$FF	; top of scrolling area
bot:	.byte	$FF	; bottom of scrolling area
vt100gr	=	*	; graphic rendition params for vt100 emulation
alternt: .byte	$FF	; $00=normal color, $01=alternate color
	.byte	$FE	; filer for vt100 emulation
	.byte	$FE	; filer for vt100 emulation
underln: .byte	$FF	; $00=underline off, $ff=underline on
flash:	.byte	$FF	; $00=normal text, $01=flashing text
	.byte	$FE	; filler for vtt100 emulation
reverse: .byte	$FF	; $00=reverse off, $ff=reverse on
wrap:	.byte	$FF	; $01=no automatic wrapping, $00=use wrapping
altcs:	.byte	0	; [jrd] alternate char set switch for vt100
csg0:	.byte	csascii	; [jrd] G0 is normal ascii
csg1:	.byte	csgraf	; [jrd] G1 is graphics
cntdown: .byte	$FF	; countdown timer
curabrt: .byte	$FF	; $00=cursor disabled.  Incremented & decremented.
curstat: .byte	$FF	; $00=cursor light now, $01=cursor dark now
evenodd: .byte	$FF	; $f0=cursor on even column, $0f=cursor on odd column
save1:	.byte	$FF	; screen save area #1
save2:	.byte	$FF	; screen save area #2
save3:	.byte	$FF	; screen save area #3
save4:	.byte	$FF	; screen save area #4
save5:	.byte	$FF	; screen save area #5
save6:	.byte	$FF	; screen save area #6

;.SBTTL	Data for the vt100 emulation package

vt100st: .byte	$FF	; parser state
vt100pt: .byte	$FF	; parameter pointer

;
; Screen mem chunks must be on a 4k bound.  
; Given the size of this thing, the best we can do is to end up
; with scrmemlo at $9000. If it's higher than that, we'll clobber
; the OS.  Beware when adding things to this code!
;
	.bss

newaddr = ((* | $fff) + 1)
	.res	newaddr - *

;	.res	* - ((* | $fff) + 1)
;.warning "beware here!"
;	*=	*|$FFF+1	; go to next 4k bound

scrmemlo: .res	3840		; lo half of graphics screen
;
; the display list area for 40/80 col mode.  max size is 3 (preamble) + 
;  3 (first graphics) + 95 (rest of graphics first half) + 
;  3 + 95 + 3 (status line) + 3 (branch back)
;
dlist:	.res	256		; room for disp list, gets us to next 4k
scrmemhi: .res	3840		; hi half of screen mem
statline: .res	40	; status line for terminal screens
;
; vt100 tabstop map
;
tabstop: .res	80
;
; scratch area for vt100 stuff
;
freemem: .res	$20
;
; Storage for font mem in 40/80 mode
;
font40	=	scrmemhi
;
; that's it!
;

	.segment "AUTOSTRT"
	.word	$02E0
	.word	$02E1
	.word	kstart

	.import		__CODE_LOAD__, __BSS_LOAD__

	.segment "EXEHDR"
	.word	$FFFF
	.word	__CODE_LOAD__
	.word	__BSS_LOAD__ - 1
