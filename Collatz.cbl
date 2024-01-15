       IDENTIFICATION DIVISION.
       PROGRAM-ID. collatz-conjecture.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER PIC S9(8).
       01 WS-STEPS PIC 9(4).
       01 WS-ERROR PIC X(35).
       01 WS-RESTO    PIC 9(4).
       01 WS-ITERACION PIC 9(4).
       PROCEDURE DIVISION.
       COLLATZ-CONJECTURE.
           INITIALIZE WS-STEPS
           EVALUATE TRUE
              WHEN WS-NUMBER = 1
                   MOVE ZEROS TO WS-STEPS
              WHEN WS-NUMBER < 1
                   MOVE "Only positive integers are allowed" TO WS-ERROR
              WHEN WS-NUMBER > 1
                   PERFORM OPERACION VARYING WS-ITERACION FROM 1 BY 1
                           UNTIL WS-STEPS > 0
           END-EVALUATE
           DISPLAY WS-STEPS.
           DISPLAY WS-ERROR.
       OPERACION.
           MOVE FUNCTION MOD(WS-NUMBER, 2) TO WS-RESTO
           IF WS-RESTO = 0
              COMPUTE WS-NUMBER = WS-NUMBER / 2
              IF WS-NUMBER = 1
                 MOVE WS-ITERACION TO WS-STEPS
              END-IF
           ELSE
              COMPUTE WS-NUMBER = (WS-NUMBER * 3) + 1
           END-IF
           .