      IDENTIFICATION DIVISION.
       PROGRAM-ID. hamming.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DNA-1 PIC X(32).
       01 WS-DNA-2 PIC X(32).
       01 WS-HAMMING PIC 9(2).
       01 WS-ERROR PIC X(31).
       01 CONTADOR PIC 9(3).
       PROCEDURE DIVISION.
              
       
       HAMMING.
       INITIALIZE WS-HAMMING.
                    
       IF FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-1)) NOT =
          FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-2)) THEN
          MOVE "Strands must be of equal length" TO WS-ERROR
       ELSE
         PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL CONTADOR > 
         (FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-1)))
           IF WS-DNA-1(CONTADOR:1) NOT = WS-DNA-2(CONTADOR:1) THEN
             ADD 1 TO WS-HAMMING
           END-IF
         END-PERFORM
       END-IF.
        
       DISPLAY "Hamming Distance is: " WS-HAMMING.
       DISPLAY WS-ERROR. 
       
       
       END-OF-PROGRAM.
           STOP RUN.
       END PROGRAM hamming.