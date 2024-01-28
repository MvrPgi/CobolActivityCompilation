       IDENTIFICATION DIVISION.
       PROGRAM-ID. luhn.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CARD-NUMBER PIC X(32).
       01 CNT            PIC 9(3).
       01 A              PIC X.
       01 B              PIC 9.
       01 N              PIC 9(2).
       01 WS-CARD-DIGITS PIC 9(32).
       01 ITER           PIC 9(2).
       01 WS-CHECKSUM PIC 9(2).
       01 WS-VALID PIC X(5).
       
       PROCEDURE DIVISION.
       LUHN.
         PERFORM REMOVE-SPACES.
         SUBTRACT 1 FROM CNT.
         IF CNT = 1
           MOVE "FALSE" TO WS-VALID
           GOBACK
         END-IF.         
         PERFORM VARYING ITER FROM 1 BY 2 UNTIL ITER > 32
            MOVE WS-CARD-DIGITS(ITER:1) TO N
            MULTIPLY N BY 2 GIVING N
            IF N IS GREATER THAN 9
               SUBTRACT 9 FROM N GIVING B
               MOVE B TO WS-CARD-DIGITS(ITER:1)
            END-IF
         END-PERFORM.
         MOVE 0 TO WS-CHECKSUM.
         PERFORM VARYING ITER FROM 1 BY 1 UNTIL ITER = 32
            MOVE WS-CARD-DIGITS(ITER:1) TO N
            ADD N TO WS-CHECKSUM
         END-PERFORM.
         IF FUNCTION MOD(WS-CHECKSUM, 10) = 0
            MOVE "VALID" TO WS-VALID
         ELSE
            MOVE "FALSE" TO WS-VALID
         END-IF.
      
       REMOVE-SPACES.
         MOVE 1 TO CNT.
         MOVE FUNCTION TRIM(WS-CARD-NUMBER) TO WS-CARD-NUMBER.
         PERFORM VARYING ITER FROM 1 BY 1 UNTIL ITER = 32
            MOVE WS-CARD-NUMBER(ITER:1) TO A
            EVALUATE TRUE
               WHEN A = ' '
                  CONTINUE
               WHEN A="1" OR A="2" OR A="3" OR A="4" OR A="5" OR A="6"
                  MOVE A TO WS-CARD-DIGITS(CNT:1)
                  ADD 1 TO CNT
               WHEN A="7" OR A="8" OR A="9" OR A="0"
                  MOVE A TO WS-CARD-DIGITS(CNT:1)
                  ADD 1 TO CNT
               WHEN OTHER
                  MOVE "FALSE" TO WS-VALID
                  GOBACK
            END-EVALUATE
         END-PERFORM.