       IDENTIFICATION DIVISION.
       PROGRAM-ID. difference-of-squares.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DIFFERENCE-OF-SQUARES PIC 9(8).
       01 WS-SUM-OF-SQUARES PIC 9(8).
       01 WS-SQUARE-OF-SUM PIC 9(8).
       01 WS-NUMERATOR PIC 9(8).
       01 WS-NUMBER PIC 9(8).
       01 WS-SUM PIC 9(8).
       PROCEDURE DIVISION.
       
       SQUARE-OF-SUM.
       COMPUTE WS-SUM = WS-NUMBER * (WS-NUMBER + 1) / 2.
       COMPUTE WS-SQUARE-OF-SUM = WS-SUM * WS-SUM.
       
       SUM-OF-SQUARES.
       COMPUTE WS-NUMERATOR = 
           WS-NUMBER * (WS-NUMBER + 1) * (2 * WS-NUMBER + 1).
       COMPUTE WS-SUM-OF-SQUARES = WS-NUMERATOR / 6.
       
       DIFFERENCE-OF-SQUARES.
       COMPUTE WS-DIFFERENCE-OF-SQUARES = 
           WS-SQUARE-OF-SUM - WS-SUM-OF-SQUARES.
       
       SHOW-RESULTS.
       DISPLAY WS-SQUARE-OF-SUM.
       DISPLAY WS-SUM-OF-SQUARES.
       DISPLAY WS-DIFFERENCE-OF-SQUARES.
       