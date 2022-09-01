      ******************************************************************
      * Author:JMRA
      * Date:14/06/2022
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
               ASSIGN TO "..\socios.dat"
                   ORGANISATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS soc-cod.

       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-cod          PIC 999.
           03 soc-nom          PIC X(20).
           03 soc-saldo        PIC S9(6)V99.
       WORKING-STORAGE SECTION.

       SCREEN SECTION.

      ******************************************************************
      ************************ PROGRAMA PRINCIPAL **********************
      ******************************************************************

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 010-OPEN-FILE-SOCIOS.
           PERFORM 020-CLOSE-FILE-SOCIOS.
           STOP RUN.

      ****************** R U T I N A S G E N E R A L E S ***************

       010-OPEN-FILE-SOCIOS.
           OPEN OUTPUT SOCIOS.

       020-CLOSE-FILE-SOCIOS.
           CLOSE SOCIOS.


       END PROGRAM YOUR-PROGRAM-NAME.
