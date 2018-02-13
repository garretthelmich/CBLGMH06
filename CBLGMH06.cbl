       IDENTIFICATION DIVISION.
       program-id. CBLGMH06
       AUTHOR.     Garrett Helmich.
       DATE-WRITTEN.   1/17/2018.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT I-RECORD
               ASSIGN TO 'C:\IHCC BACKUP\COBOL\6THPROJECT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRINTLN
               ASSIGN TO 'C:\IHCC BACKUP\COBOL\RECORD2.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.
           SELECT PRINT-ERROR
               ASSIGN TO 'C:\IHCC BACKUP\COBOL\ERROR2.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.
       
      *This is a change made for my github
       DATA DIVISION.
       FILE SECTION.

       FD  I-RECORD
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 71 CHARACTERS.
           01  I-REC.
               05  P-LNAME                     PIC X(15).
               05  P-FNAME                     PIC X(15).
               05  P-ADDRESS                   PIC X(15).
               05  P-CITY                      PIC X(10).
               05  P-STATE                     PIC XX.
               05  P-ZIP.
                   10  P-1ZIP                  PIC 99999.
                   10  P-2ZIP                  PIC 9999.
               05  P-POP-TYPE                  PIC 99.
               05  P-CASES                     PIC 99.
               05  P-TEAM                      PIC X.

       FD  PRINTLN
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
           01  PRTLINE                         PIC X(132).

       FD  PRINT-ERROR
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
           01  PRTLINE                     PIC X(132).


       WORKING-STORAGE SECTION.
      ******************************************************************
      * ↓ These are variables used for calculations (no display) ↓
      ******************************************************************
       01  MISC.
           05  EOF                     PIC X       VALUE 'F'.
           05  PRICE                   PIC 99V99   VALUE 18.71.
           05  ERROR-COUNT             PIC 9999    VALUE 0.
           05  CURRENT-DATE-AND-TIME.
               10  CURRENT-YEAR        PIC X(4).
               10  CURRENT-MONTH       PIC XX.
               10  CURRENT-DAY         PIC XX.
               10  CURRENT-TIME        PIC X(11).
           05  C-PCTR                  PIC 99      VALUE 0.
           05  ERR-C-PCTR              PIC 99      VALUE 0.
           05  IS-VALID                PIC X       VALUE 'F'.
           05  C-FLAG                  PIC XXX.
           05  C-ERR-DESCRIPTION       PIC X(60).
           05  C-DEPOSIT               PIC V99.
           05  CC-DEPOSIT              PIC 999V99.
           05  C-TOTAL                 PIC 9999V99.

       01 POP.
           05  FILLER                  PIC X(22)   
                   VALUE 'COKE            000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'DIET COKE       000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'MELLO YELLO     000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'CHERRY COKE     000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'DIET CHERRY COKE000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'SPRITE          000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'DIET SPRITE     000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'DASANI          000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'C2              000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'MR. PIBB        000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'DIET LEMON COKE 000000'.
           05  FILLER                  PIC X(22)
                   VALUE 'VANILLA COKE    000000'.
       

           

       01 POP-TABLE REDEFINES POP.
           05  POP-DATA    OCCURS      12 TIMES.
               10  POP-NAME            PIC X(16).
               10  POP-TOT             PIC 9(6).
      *COMPUTE POP-TOT(I-POP-TYPE) = POP-TOT(I-POP-TYPE) + I-NUM-CASES.


       01  C-REC.
           05  I-STATE                 PIC XX.
               88  VALID-STATE     VALUE 'IA','IL','MI','MO','NE','WI'.
               88  FIVE-CENT       VALUE 'IA','NE','WI'.
               88  TEN-CENT        VALUE 'MI'.
           05  I-POP-TYPE              PIC 99.
               88  VALID-POP-TYPE  VALUE 01 THRU 12.
           05  I-TEAM                  PIC X.
               88  VALID-TEAM      VALUE 'A','B','C','D','E'.

       01  C-TEAM-TOTALS.
           05  A-TEAM-TOTAL            PIC 999999999V99    VALUE 0.
           05  B-TEAM-TOTAL            PIC 999999999V99    VALUE 0.
           05  C-TEAM-TOTAL            PIC 999999999V99    VALUE 0.
           05  D-TEAM-TOTAL            PIC 999999999V99    VALUE 0.
           05  E-TEAM-TOTAL            PIC 999999999V99    VALUE 0.
           05  C-HIGHEST               PIC 999999999V99.
           05  C-WINNER                PIC X.

       01  C-GRAND-TOTALS.
           05  CTR-ONE-GT              PIC 999999          VALUE 0.
           05  CTR-TWO-GT              PIC 999999          VALUE 0.
           05  CTR-THREE-GT            PIC 999999          VALUE 0.
           05  CTR-FOUR-GT             PIC 999999          VALUE 0.
           05  CTR-FIVE-GT             PIC 999999          VALUE 0.
           05  CTR-SIX-GT              PIC 999999          VALUE 0.
           05  CTR-SEVEN-GT            PIC 999999          VALUE 0.
           05  CTR-EIGHT-GT            PIC 999999          VALUE 0.
           05  CTR-NINE-GT             PIC 999999          VALUE 0.
           05  CTR-TEN-GT              PIC 999999          VALUE 0.
           05  CTR-ELEVEN-GT           PIC 999999          VALUE 0.
           05  CTR-TWELVE-GT           PIC 999999          VALUE 0.
      ******************************************************************
      ** ↑ end of calculation variables ↑
      ******************************************************************

       01  FIRST-HEADING.
           05  FILLER                  PIC X(6)    VALUE 'DATE: '.
           05  H1-DATE.
               10  H1-DAY              PIC 99.
               10  FILLER              PIC X       VALUE '/'.
               10  H1-MONTH            PIC 99.
               10  FILLER              PIC X       VALUE '/'.
               10  H1-YEAR             PIC 9999.
           05  FILLER                  PIC X(35)   VALUE SPACES.
           05  FILLER                  PIC X(30)
                   VALUE 'GARRETT SOCCER CLUB FUNDRAISER'.
           05  FILLER                  PIC X(43)   VALUE SPACES.
           05  FILLER                  PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR                  PIC Z9.

       01  DIVISION-HEADING.
           05  FILLER                  PIC X(52)   VALUE SPACES.
           05  O-DIVISION              PIC X(10).
           05  FILLER                  PIC X(9)    VALUE ' DIVISION'.

       01  SALES-HEADING.
           05  FILLER                  PIC X(60)   VALUE SPACES.
           05  FILLER                  PIC X(12)
                           VALUE 'SALES REPORT'.

       01  ERROR-HEADING.
           05  FILLER                  PIC X(60)   VALUE SPACES.
           05  FILLER                  PIC X(12)
                           VALUE 'ERROR REPORT'.

       01  COL-HEADING.
           05  FILLER                  PIC XXX     VALUE SPACES.
           05  FILLER                  PIC X(9)    VALUE 'LAST NAME'.
           05  FILLER                  PIC X(8)    VALUE SPACES.
           05  FILLER                  PIC X(10)   VALUE 'FIRST NAME'.
           05  FILLER                  PIC X(7)    VALUE SPACES.
           05  FILLER                  PIC XXXX    VALUE 'CITY'.
           05  FILLER                  PIC X(8)    VALUE SPACES.
           05  FILLER                  PIC X(26)
                       VALUE 'STATE ZIP CODE    POP TYPE'.
           05  FILLER                  PIC X(13)   VALUE SPACES.
           05  FILLER                  PIC X(8)    VALUE 'QUANTITY'.
           05  FILLER                  PIC X(6)    VALUE SPACES.
           05  FILLER                  PIC X(11)   VALUE 'DEPOSIT AMT'.
           05  FILLER                  PIC X(6)    VALUE SPACES.
           05  FILLER                  PIC X(11)   VALUE 'TOTAL SALES'.
           
       01  DETAIL-LINE.
           05  SO-FLAG                 PIC XXX     VALUE SPACES.
           05  SO-LNAME                PIC X(15).
           05  FILLER                  PIC XX      VALUE SPACES.
           05  SO-FNAME                PIC X(15).
           05  FILLER                  PIC XX      VALUE SPACES.
           05  SO-CITY                 PIC X(10).
           05  FILLER                  PIC XXX     VALUE SPACES.
           05  SO-STATE                PIC XX.
           05  FILLER                  PIC XXX     VALUE SPACES.
           05  SO-ZIP.
               10  SO-1ZIP             PIC 99999.
               10  FILLER              PIC X       VALUE '-'.
               10  SO-2ZIP             PIC 9999.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  SO-POP-TYPE             PIC X(16).
           05  FILLER                  PIC X(8)    VALUE SPACES.
           05  SO-QTY                  PIC Z9.
           05  FILLER                  PIC X(11)   VALUE SPACES.
           05  SO-DEPO-AMT             PIC $$$$.99.
           05  FILLER                  PIC X(9)    VALUE SPACES.
           05  SO-TOT-SALES            PIC $$,$$$.99.

       01  GT-HEADING.
           05  FILLER           PIC X(13)  VALUE 'GRAND TOTALS:'.

       01  GT-LINE.
           05  FILLER           PIC XXX    VALUE SPACES.
           05  GT-POP-ONE       PIC X(16).
           05  FILLER           PIC X      VALUE ' '.
           05  GT-ONE-SOLD      PIC ZZZ,ZZ9.
           05  FILLER           PIC XXX    VALUE SPACES.
           05  GT-POP-TWO       PIC X(16).
           05  FILLER           PIC X      VALUE ' '.
           05  GT-TWO-SOLD      PIC ZZZ,ZZ9.
           05  FILLER           PIC XXX    VALUE SPACES.
           05  GT-POP-THREE     PIC X(16).
           05  FILLER           PIC X      VALUE ' '.
           05  GT-THREE-SOLD    PIC ZZZ,ZZ9.
           05  FILLER           PIC XXX    VALUE SPACES.
           05  GT-POP-FOUR      PIC X(16).
           05  FILLER           PIC X      VALUE ' '.
           05  GT-FOUR-SOLD     PIC ZZZ,ZZ9.

       01  TEAM-TOTAL-HEADING.
           05  FILLER           PIC X(12)  VALUE 'TEAM TOTALS:'.

       01  TEAM-TOTAL-LINE.
           05  FILLER           PIC XXX    VALUE SPACES.
           05  O-TEAM           PIC X.
           05  FILLER           PIC X      VALUE ' '.
           05  O-GT-SALES       PIC $$$$,$$$,$$$.99.

       01  S-FINAL-LINE.
           05  FILLER           PIC X(8)   VALUE '***TEAM '.
           05  O-WINNER         PIC X.
           05  FILLER           PIC X(17)  VALUE ' IS THE WINNER!!!'.

       01  ERR-COL-HEADING.
           05  FILLER           PIC X(12)  VALUE 'ERROR RECORD'.
           05  FILLER           PIC X(60)  VALUE SPACES.
           05  FILLER           PIC X(17)  VALUE 'ERROR DESCRIPTION'.

       01  ERR-DETAIL-LINE.
           05  O-REC            PIC X(71).
           05  FILLER           PIC X      VALUE ' '.
           05  O-DESCRIPTION    PIC X(60).

       01  ERR-TOTALS.
           05  FILLER           PIC X(13)  VALUE 'TOTAL ERRORS '.
           05  O-TOTAL-ERRORS   PIC Z,ZZ9.
       

       PROCEDURE DIVISION.

       L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L2-MAINLINE
               UNTIL EOF = 'T'.
           PERFORM L2-CLOSURE.
           PERFORM L2-ERR-CLOSURE.
           STOP RUN.
           


       L2-INIT.
           MOVE FUNCTION CURRENT-DATE      TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-DAY                TO H1-DAY.
           MOVE CURRENT-MONTH              TO H1-MONTH.
           MOVE CURRENT-YEAR               TO H1-YEAR.
           OPEN INPUT I-RECORD.
           OPEN OUTPUT PRINTLN.
           OPEN OUTPUT PRINT-ERROR.
           PERFORM L3-READ.
           PERFORM L4-HEADINGS.
           PERFORM L4-ERR-HEADINGS.

       L2-MAINLINE.
           PERFORM L3-VALIDATION
               THROUGH L4-VALIDATION-EXIT.
           IF IS-VALID = 'T'
               PERFORM L3-CALCS
               PERFORM L3-OUTPUT
           ELSE
               ADD 1 TO ERROR-COUNT
               PERFORM L3-ERR-OUTPUT.
           PERFORM L3-READ.



      ******************************************************************
      *This prints the last dozen lines of the valid input file
      ******************************************************************
       L2-CLOSURE.
      *Prints quantity of each pop type sold
           WRITE PRTLINE OF PRINTLN FROM GT-HEADING
               AFTER ADVANCING 3 LINES.
      *****1st line
           MOVE CTR-ONE-GT TO GT-ONE-SOLD.
           MOVE CTR-TWO-GT TO GT-TWO-SOLD.
           MOVE CTR-THREE-GT TO GT-THREE-SOLD.
           MOVE CTR-FOUR-GT TO GT-FOUR-SOLD.
           MOVE 'COKE' TO GT-POP-ONE.
           MOVE 'DIET COKE' TO GT-POP-TWO.
           MOVE 'MELLO YELLO' TO GT-POP-THREE.
           MOVE 'CHERRY COKE' TO GT-POP-FOUR.
           WRITE PRTLINE OF PRINTLN FROM GT-LINE
               AFTER ADVANCING 3 LINES.
      *****2nd line
           MOVE CTR-FIVE-GT TO GT-ONE-SOLD.
           MOVE CTR-SIX-GT TO GT-TWO-SOLD.
           MOVE CTR-SEVEN-GT TO GT-THREE-SOLD.
           MOVE CTR-EIGHT-GT TO GT-FOUR-SOLD.
           MOVE 'DIET CHERRY COKE' TO GT-POP-ONE.
           MOVE 'SPRITE' TO GT-POP-TWO.
           MOVE 'DIET SPRITE' TO GT-POP-THREE.
           MOVE 'DASANI' TO GT-POP-FOUR.
           WRITE PRTLINE OF PRINTLN FROM GT-LINE
               AFTER ADVANCING 2 LINES.
      *****3rd line
           MOVE CTR-NINE-GT TO GT-ONE-SOLD.
           MOVE CTR-TEN-GT TO GT-TWO-SOLD.
           MOVE CTR-ELEVEN-GT TO GT-THREE-SOLD.
           MOVE CTR-TWELVE-GT TO GT-FOUR-SOLD.
           MOVE 'C2' TO GT-POP-ONE.
           MOVE 'MR. PIBB' TO GT-POP-TWO.
           MOVE 'DIET LEMON COKE' TO GT-POP-THREE.
           MOVE 'VANILLA COKE' TO GT-POP-FOUR.
           WRITE PRTLINE OF PRINTLN FROM GT-LINE
               AFTER ADVANCING 2 LINES.

      *Prints team totals below
           WRITE PRTLINE OF PRINTLN FROM TEAM-TOTAL-HEADING
               AFTER ADVANCING 4 LINES.
           MOVE 'A' TO O-TEAM.
           MOVE A-TEAM-TOTAL TO O-GT-SALES.
           WRITE PRTLINE OF PRINTLN FROM TEAM-TOTAL-LINE
               AFTER ADVANCING 2 LINES.
           MOVE 'B' TO O-TEAM.
           MOVE B-TEAM-TOTAL TO O-GT-SALES.
           WRITE PRTLINE OF PRINTLN FROM TEAM-TOTAL-LINE
               AFTER ADVANCING 2 LINES.
           MOVE 'C' TO O-TEAM.
           MOVE C-TEAM-TOTAL TO O-GT-SALES.
           WRITE PRTLINE OF PRINTLN FROM TEAM-TOTAL-LINE
               AFTER ADVANCING 2 LINES.
           MOVE 'D' TO O-TEAM.
           MOVE D-TEAM-TOTAL TO O-GT-SALES.
           WRITE PRTLINE OF PRINTLN FROM TEAM-TOTAL-LINE
               AFTER ADVANCING 2 LINES.
           MOVE 'E' TO O-TEAM.
           MOVE E-TEAM-TOTAL TO O-GT-SALES.
           WRITE PRTLINE OF PRINTLN FROM TEAM-TOTAL-LINE
               AFTER ADVANCING 2 LINES.

      *Calculates and prints the winning team
           PERFORM L3-WINNER.
           MOVE C-WINNER TO O-WINNER.
           WRITE PRTLINE OF PRINTLN FROM S-FINAL-LINE
               AFTER ADVANCING 2 LINES.


      ******************************************************************
      *Prints final lines of the error report and closes all files.
      *This is the last method to be called
      ******************************************************************
       L2-ERR-CLOSURE.
           MOVE ERROR-COUNT TO O-TOTAL-ERRORS.
           WRITE PRTLINE OF PRINT-ERROR FROM ERR-TOTALS
               AFTER ADVANCING 3 LINES.
           CLOSE I-RECORD.
           CLOSE PRINTLN.
           CLOSE PRINT-ERROR.

      ******************************************************************
      *Determines if the input is valid or not.
      ******************************************************************
       L3-VALIDATION.
           MOVE 'T' TO IS-VALID.
           MOVE P-STATE TO I-STATE.
           MOVE P-TEAM TO I-TEAM.

           IF P-POP-TYPE IS NUMERIC
               MOVE P-POP-TYPE TO I-POP-TYPE.
       
           IF P-LNAME EQUALS SPACES
               MOVE 'F' TO IS-VALID
               MOVE 'INVALID LAST NAME.' TO C-ERR-DESCRIPTION
               GO TO L4-VALIDATION-EXIT.
           IF P-FNAME EQUALS SPACES
               MOVE 'F' TO IS-VALID
               MOVE 'INVALID FIRST NAME.' TO C-ERR-DESCRIPTION
               GO TO L4-VALIDATION-EXIT.
           IF P-ADDRESS EQUALS SPACES
               MOVE 'F' TO IS-VALID
               MOVE 'INVALID ADDRESS.' TO C-ERR-DESCRIPTION
               GO TO L4-VALIDATION-EXIT.
           IF P-CITY EQUALS SPACES
               MOVE 'F' TO IS-VALID
               MOVE 'INVALID CITY.' TO C-ERR-DESCRIPTION
               GO TO L4-VALIDATION-EXIT.
           IF NOT VALID-STATE
               MOVE 'F' TO IS-VALID
               MOVE 'INVALID STATE.' TO C-ERR-DESCRIPTION
               GO TO L4-VALIDATION-EXIT.
           IF P-1ZIP NOT NUMERIC OR P-2ZIP NOT NUMERIC
               MOVE 'F' TO IS-VALID
               MOVE 'INVALID ZIP CODE.' TO C-ERR-DESCRIPTION
               GO TO L4-VALIDATION-EXIT.
           IF NOT VALID-POP-TYPE
               MOVE 'F' TO IS-VALID
               MOVE 'INVALID POP CODE.' TO C-ERR-DESCRIPTION
               GO TO L4-VALIDATION-EXIT.
           IF P-CASES NOT NUMERIC OR P-CASES NOT GREATER THAN 1
               MOVE 'F' TO IS-VALID
               MOVE 'INVALID NUMBER OF CASES' TO C-ERR-DESCRIPTION
               GO TO L4-VALIDATION-EXIT.
           IF NOT VALID-TEAM
               MOVE 'F' TO IS-VALID
               MOVE 'INVALID TEAM.' TO C-ERR-DESCRIPTION.
           GO TO L4-VALIDATION-EXIT.

       L4-VALIDATION-EXIT.
           EXIT.


      ******************************************************************
      *Finds which team sold the most
      ******************************************************************
       L3-WINNER.
           MOVE A-TEAM-TOTAL TO C-HIGHEST.
           MOVE 'A' TO C-WINNER.
           IF C-HIGHEST < B-TEAM-TOTAL
               MOVE 'B' TO C-WINNER
               MOVE B-TEAM-TOTAL TO C-HIGHEST.
           IF C-HIGHEST < C-TEAM-TOTAL
               MOVE 'C' TO C-WINNER
               MOVE C-TEAM-TOTAL TO C-HIGHEST.
           IF C-HIGHEST < D-TEAM-TOTAL
               MOVE 'D' TO C-WINNER
               MOVE D-TEAM-TOTAL TO C-HIGHEST.
           IF C-HIGHEST < E-TEAM-TOTAL
               MOVE 'E' TO C-WINNER
               MOVE E-TEAM-TOTAL TO C-HIGHEST.
           
           
      ******************************************************************
      *L3-calcs....runs the calculations
      ******************************************************************
       L3-CALCS.
      *If's to find the correct deposite amount                        
           IF FIVE-CENT
               MOVE .05 TO C-DEPOSIT
           ELSE
               IF TEN-CENT
                   MOVE .10 TO C-DEPOSIT
               ELSE
                   MOVE .00 TO C-DEPOSIT
               END-IF.
      *The actual calculations needed
           COMPUTE C-TOTAL = PRICE * (P-CASES + C-DEPOSIT).
           COMPUTE CC-DEPOSIT = P-CASES * C-DEPOSIT.
           PERFORM L4-POP-TYPE.
           PERFORM L4-TEAM-TOTALS.
      *Moves a flag if needed.
           IF C-TOTAL GREATER THAN 250
               MOVE '***' TO C-FLAG
           ELSE
               MOVE '   ' TO C-FLAG
           END-IF.

      ******************************************************************
      *Prints valid input out to the sales report.
      ******************************************************************
       L3-OUTPUT.
           MOVE C-FLAG TO SO-FLAG.
           MOVE P-LNAME TO SO-LNAME.
           MOVE P-FNAME TO SO-FNAME.
           MOVE P-CITY TO SO-CITY.
           MOVE P-STATE TO SO-STATE.
           MOVE P-1ZIP TO SO-1ZIP.
           MOVE P-2ZIP TO SO-2ZIP.
           MOVE P-CASES TO SO-QTY.
           MOVE CC-DEPOSIT TO SO-DEPO-AMT.
           MOVE C-TOTAL TO SO-TOT-SALES.
           WRITE PRTLINE OF PRINTLN FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM L4-HEADINGS.
           
      ******************************************************************
      *Prints invalid record to the error report along with the 
      * reason for the error.
      ******************************************************************
       L3-ERR-OUTPUT.
           MOVE I-REC TO O-REC.
           MOVE C-ERR-DESCRIPTION TO O-DESCRIPTION.
           WRITE PRTLINE OF PRINT-ERROR FROM ERR-DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM L4-ERR-HEADINGS.

      ******************************************************************
      *Reads the record.
      ******************************************************************
       L3-READ.
           READ I-RECORD
               AT END
                   MOVE 'T' TO EOF.

      ******************************************************************
       L4-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE OF PRINTLN FROM FIRST-HEADING
               AFTER ADVANCING PAGE.
           WRITE PRTLINE OF PRINTLN FROM DIVISION-HEADING
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE OF PRINTLN FROM SALES-HEADING
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE OF PRINTLN FROM COL-HEADING
               AFTER ADVANCING 2 LINES.
      ******************************************************************
       L4-ERR-HEADINGS.
           ADD 1 TO ERR-C-PCTR.
           MOVE ERR-C-PCTR TO O-PCTR.
           WRITE PRTLINE OF PRINT-ERROR FROM FIRST-HEADING
               AFTER ADVANCING PAGE.
           WRITE PRTLINE OF PRINT-ERROR FROM DIVISION-HEADING
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE OF PRINT-ERROR FROM ERROR-HEADING
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE OF PRINT-ERROR FROM ERR-COL-HEADING
               AFTER ADVANCING 2 LINES.


      ******************************************************************
      *This method adds total cost to the correct team                 *
      ******************************************************************
       L4-TEAM-TOTALS.
           IF P-TEAM EQUALS 'A'
               ADD C-TOTAL TO A-TEAM-TOTAL
           ELSE
               IF P-TEAM EQUALS 'B'
                   ADD C-TOTAL TO B-TEAM-TOTAL
               ELSE
                   IF P-TEAM EQUALS 'C'
                       ADD C-TOTAL TO C-TEAM-TOTAL
                   ELSE
                       IF P-TEAM EQUALS 'D'
                           ADD C-TOTAL TO D-TEAM-TOTAL
                       ELSE 
                           ADD C-TOTAL TO E-TEAM-TOTAL
                       END-IF
                   END-IF
               END-IF
           END-IF.


      ******************************************************************
      *This method will find the correct soda for the pop type and add *
      *the number of cases to the correct counter.                     *
      ******************************************************************
       L4-POP-TYPE.
           EVALUATE P-POP-TYPE
               WHEN 01
                   MOVE 'COKE' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-ONE-GT
               WHEN 02
                   MOVE 'DIET COKE' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-TWO-GT
               WHEN 03
                   MOVE 'MELLO YELLO' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-THREE-GT
               WHEN 04
                   MOVE 'CHERRY COKE' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-FOUR-GT
               WHEN 05
                   MOVE 'DIET CHERRY COKE' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-FIVE-GT
               WHEN 06
                   MOVE 'SPRITE' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-SIX-GT
               WHEN 07
                   MOVE 'DIET SPRITE' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-SEVEN-GT
               WHEN 08
                   MOVE 'DASANI' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-EIGHT-GT
               WHEN 09
                   MOVE 'C2' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-NINE-GT
               WHEN 10
                   MOVE 'MR. PIBB' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-TEN-GT
               WHEN 11
                   MOVE 'DIET LEMON COKE' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-ELEVEN-GT
               WHEN 12
                   MOVE 'VANILLA COKE' TO SO-POP-TYPE
                   ADD P-CASES TO CTR-TWELVE-GT
           END-EVALUATE.

           
      *end program CBLGMH041.