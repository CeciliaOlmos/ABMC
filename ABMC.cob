      ******************************************************************
      * Author:EDUARDO Y MARCELO
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
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS soc-cod.

       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-cod          PIC 999.
           03 soc-nom          PIC X(20).
           03 soc-saldo        PIC S9(6)V99.
       WORKING-STORAGE SECTION.

      ******************************************************************
      ********************** PANTALLA MENU PRINCIPAL *******************
      ******************************************************************

       01  linea1.
           03 filler  pic X(60) value spaces.
           03 filler  pic X(7)  value "FECHA: ".
           03 l-dia   pic 99.
           03 filler  pic X     value "/".
           03 l-mes   pic 99.
           03 filler  pic XXX   value "/20".
           03 l-anio  pic 99.
           03 filler  pic XXX   value spaces.
       01  linea2.
           03 filler pic x(32)  value spaces.
           03 filler pic x(16)  value "MENU DE OPCIONES".
           03 filler pic x(32)  value spaces.
       01  linea3.
           03 filler pic x(80)  value all "-".
       01  linea4.
           03 filler pic x(35)  value spaces.
           03 filler pic x(9)  value "1 - ALTAS".
           03 filler pic x(36)  value spaces.
       01  linea5.
           03 filler pic x(35)  value spaces.
           03 filler pic x(9)  value "2 - BAJAS".
           03 filler pic x(36)  value spaces.
       01  linea6.
           03 filler pic x(35)  value spaces.
           03 filler pic x(18)  value "3 - MODIFICACIONES".
           03 filler pic x(27)  value spaces.
       01  linea7.
           03 filler pic x(35)  value spaces.
           03 filler pic x(13)  value "4 - CONSULTAS".
           03 filler pic x(32)  value spaces.
       01  linea8.
           03 filler pic x(35)  value spaces.
           03 filler pic x(9)   value "5 - SALIR".
           03 filler pic x(36)  value spaces.
       01  linea9.
           03 filler pic x(80)  value spaces.
       01  linea10.
           03 filler pic x(30)  value spaces.
           03 filler pic x(23)  value "Introduzca una opcion: ".
           03 filler pic x(27)  value spaces.
       01  lin-espacion-blanco.
           03 FILLER PIC X(80) VALUE ALL SPACES.

      ******************************************************************
      ******************** PANTALLA MENU MODIFICACION ******************
      ******************************************************************

       01  lin-modif1.
           03 filler pic x(34)  value spaces.
           03 filler pic x(12)  value "MODIFICACION".
           03 filler pic x(34)  value spaces.
       01  lin-modif2.
           03 filler pic x(80)  value all "-".
       01  lin-modif3.
           03 filler pic x(19)  value spaces.
           03 filler pic x(5)   value "SOCIO".
           03 filler pic x(12)  value spaces.
           03 filler pic x(6)   value "NOMBRE".
           03 filler pic x(13)  value spaces.
           03 filler pic x(5)   value "SALDO".
           03 filler pic x(20)  value spaces.
       01  lin-modif4.
           03 filler       pic x(20)  value spaces.
           03 l-cod-soc    pic 999.
           03 filler       pic x(6)   value spaces.
           03 l-nom-soc    pic x(20).
           03 filler       pic x(5)  value spaces.
           03 l-saldo-soc  pic 9(6),99.
           03 filler       pic x(17)  value spaces.
       01  lin-modif5.
           03 filler pic x(32)  value spaces.
           03 filler pic x(16)  value "1 - CAMPO NOMBRE".
           03 filler pic x(32)  value spaces.
       01  lin-modif6.
           03 filler pic x(32)  value spaces.
           03 filler pic x(15)  value "2 - CAMPO SALDO".
           03 filler pic x(33)  value spaces.
       01  lin-modif7.
           03 filler pic x(32)  value spaces.
           03 filler pic x(16)  value "3 - AMBOS CAMPOS".
           03 filler pic x(32)  value spaces.
       01  lin-modif8.
           03 filler pic x(32)  value spaces.
           03 filler pic x(18)   value "0 - OMITIR CAMBIOS".
           03 filler pic x(30)  value spaces.

      ******************************************************************
      ************************ VARIABLES GENERALES *********************
      ******************************************************************

       01  opcion          PIC 9   VALUE 9.
       77  guarda-enter    PIC X.
       01  fecha.
           03 anio         PIC 99.
           03 mes          PIC 99.
           03 dia          PIC 99.

      ******************************************************************
      ************************* VARIABLES SOCIOS ***********************
      ******************************************************************

       01  flagSoc        PIC X.
       77  w-acuerdo-eliminado pic x.
       77  W-CAMPO PIC 9.


       SCREEN SECTION.

      ******************************************************************
      ************************ PROGRAMA PRINCIPAL **********************
      ******************************************************************

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 020-INICIO-GENERAL.
           PERFORM 040-INGRESO-GENERAL.
           PERFORM 060-MENU-OPCIONES UNTIL opcion = 5.
           PERFORM 080-FIN-GENERAL.
           STOP RUN.

      ******************************************************************
      ****************** R U T I N A S G E N E R A L E S ***************
      ******************************************************************

       020-INICIO-GENERAL.
           ACCEPT fecha FROM DATE.
           PERFORM 025-ABRIR-ARCHIVO.

       025-ABRIR-ARCHIVO.
           OPEN I-O SOCIOS.

       040-INGRESO-GENERAL.
           PERFORM 045-TRAIGO-OPC.
           PERFORM 050-ERROR-INGRESO UNTIL opcion >= 1 AND <= 5.

       045-TRAIGO-OPC.
           PERFORM 047-MUESTRO-MENU.
           ACCEPT opcion.

       047-MUESTRO-MENU.
           PERFORM 049-TRAIGO-FECHA.
           DISPLAY linea1.
           DISPLAY linea2.
           DISPLAY linea3.
           DISPLAY linea4.
           DISPLAY linea5.
           DISPLAY linea6.
           DISPLAY linea7.
           DISPLAY linea8.
           DISPLAY linea9.
           DISPLAY linea10.

       049-TRAIGO-FECHA.
           MOVE dia    TO l-dia.
           MOVE mes    TO l-mes.
           MOVE anio   TO l-anio.

       050-ERROR-INGRESO.
           display "Error, te dije un número entre 1 y 5".
           PERFORM 055-PAUSA-PANTALLA.
           PERFORM 045-TRAIGO-OPC.

       055-PAUSA-PANTALLA.
           PERFORM 057-TRAIGO-ENTER.
           PERFORM 059-ERROR-PAUSA-PANTALLA UNTIL guarda-enter = SPACES.

       057-TRAIGO-ENTER.
           DISPLAY lin-espacion-blanco.
           DISPLAY "Presione la tecla ENTER para continuar...".
           ACCEPT guarda-enter.

       059-ERROR-PAUSA-PANTALLA.
           display "Error, tenes que ingresar ENTER".
           PERFORM 057-TRAIGO-ENTER.

       060-MENU-OPCIONES.
           EVALUATE opcion
                   WHEN 1 PERFORM 200-ALTAS
                   WHEN 2 PERFORM 300-BAJAS
                   WHEN 3 PERFORM 400-MODIFICACIONES
                   WHEN 4 PERFORM 500-CONSULTAS
           END-EVALUATE.
           PERFORM 040-INGRESO-GENERAL.

       080-FIN-GENERAL.
           PERFORM 085-CERRAR-ARCHIVO.

       085-CERRAR-ARCHIVO.
           CLOSE SOCIOS.

      ******************************************************************
      ******************* R U T I N A S  C O M U N E S *****************
      ******************************************************************

       110-INGRESO-COD-SOC.
           DISPLAY lin-espacion-blanco.
           DISPLAY "Ingrese el codigo del socio. Por fin ingrese 0".
           ACCEPT soc-cod.

       130-LEER-REGISTRO.
           READ SOCIOS INVALID KEY
                           PERFORM 140-PROCESO-INVALID-KEY
                       NOT INVALID KEY
                           PERFORM 150-PROCESO-NOT-INVALID-KEY.

       140-PROCESO-INVALID-KEY.
           EVALUATE opcion
                   WHEN 1 PERFORM 242-INVALID-KEY-ALTAS
                   WHEN 2 PERFORM 312-INVALID-KEY-BAJAS
                   WHEN 3 PERFORM 312-INVALID-KEY-BAJAS
                   WHEN 4 PERFORM 312-INVALID-KEY-BAJAS
           END-EVALUATE.

       150-PROCESO-NOT-INVALID-KEY.
           EVALUATE opcion
                   WHEN 1 PERFORM 250-NOT-INVALID-KEY-ALTAS
                   WHEN 2 PERFORM 320-NOT-INVALID-KEY-BAJAS
                   WHEN 3 PERFORM 420-NOT-INVALID-KEY-MODIF
                   WHEN 4 PERFORM 520-NOT-INVALID-KEY-CONS
           END-EVALUATE.

      ******************************************************************
      ************************ R U T I N A S OPC-1 *********************
      ******************************************************************

       200-ALTAS.
           PERFORM 110-INGRESO-COD-SOC.
           PERFORM 220-PROCESO-ALTA UNTIL SOC-COD = 0.
           PERFORM 500-FIN.

       220-PROCESO-ALTA.
           PERFORM 130-LEER-REGISTRO.
           PERFORM 110-INGRESO-COD-SOC.


       242-INVALID-KEY-ALTAS.
           PERFORM 244-INGRESO-RESTO.
           PERFORM 246-GRABAR-REGISTRO.

       244-INGRESO-RESTO.
           DISPLAY "Ingrese el nombre del Socio: ".
           ACCEPT soc-nom.
           DISPLAY "Ingrese el saldo del Socio: ".
           ACCEPT soc-saldo.

       246-GRABAR-REGISTRO.
           WRITE soc-reg INVALID KEY
                           DISPLAY "ERROR EN LA GRABACION"
                         NOT INVALID KEY
                           DISPLAY "ALTA CONFIRMADA".

       250-NOT-INVALID-KEY-ALTAS.
           DISPLAY "SOCIO EXISTENTE".

       500-FIN.

      ******************************************************************
      ************************ R U T I N A S OPC-2 *********************
      ******************************************************************

       300-BAJAS.
            PERFORM 110-INGRESO-COD-SOC.
            PERFORM 310-PROCESO-BAJA UNTIL soc-cod IS EQUALS ZERO.

       310-PROCESO-BAJA.
           PERFORM 130-LEER-REGISTRO.
           PERFORM 110-INGRESO-COD-SOC.

       312-INVALID-KEY-BAJAS.
           DISPLAY "SOCIO INEXISTENTE".

       320-NOT-INVALID-KEY-BAJAS.
           PERFORM 322-MOSTRAR-REGISTRO.
           PERFORM 324-ANALIZAR-BAJA.

       322-MOSTRAR-REGISTRO.
           DISPLAY "Socio: ", soc-cod, " Nombre: ", soc-nom.
           DISPLAY "Estas seguro que desea eliminar el registro? "-
           "S(SI), N(NO)".
           ACCEPT w-acuerdo-eliminado.

       324-ANALIZAR-BAJA.
           IF w-acuerdo-eliminado IS EQUALS 'S'
               DELETE SOCIOS NOT INVALID KEY
                                   DISPLAY lin-espacion-blanco
                                   DISPLAY "BAJA CONFIRMADA"
           END-IF.

      ******************************************************************
      ************************ R U T I N A S OPC-3 *********************
      ******************************************************************

       400-MODIFICACIONES.
            PERFORM 110-INGRESO-COD-SOC.
            PERFORM 410-PROCESO-MODIFICACION UNTIL SOC-COD = 0.

       410-PROCESO-MODIFICACION.
           PERFORM 130-LEER-REGISTRO.
           PERFORM 110-INGRESO-COD-SOC.

       420-NOT-INVALID-KEY-MODIF.
           PERFORM 430-MUESTRO-REGISTRO
           PERFORM 440-ELIJO-CAMPO
           PERFORM 450-PROCESO.

       430-MUESTRO-REGISTRO.
           DISPLAY "1 - SOCIO: " soc-nom.
           DISPLAY "2 - SALDO: " soc-saldo.

       440-ELIJO-CAMPO.
           DISPLAY lin-espacion-blanco
           DISPLAY "Ingrese el numero de campo a modificar:"
           DISPLAY "1 - CAMPO NOMBRE"
           DISPLAY "2 - CAMPO SALDO"
           DISPLAY "3 - AMBOS CAMPOS"
           DISPLAY "0 - SALIR".
           ACCEPT W-CAMPO.

       450-PROCESO.
           EVALUATE W-CAMPO
               WHEN 1
                   DISPLAY "Ingrese el nombre del socio: "
                   ACCEPT SOC-NOM
                   PERFORM 450-GRABAR-MODIFICACION
               WHEN 2
                   DISPLAY "Ingrese el saldo del socio: "
                   ACCEPT soc-saldo
                   PERFORM 450-GRABAR-MODIFICACION
               WHEN 3
                   DISPLAY "Ingrese el nombre del socio: "
                   ACCEPT SOC-NOM
                   DISPLAY "Ingrese el saldo del socio: "
                   ACCEPT soc-saldo
                   PERFORM 450-GRABAR-MODIFICACION
               WHEN 0
                    DISPLAY lin-espacion-blanco
                    DISPLAY "MODIFICACION CANCELADA"
               WHEN OTHER
                   DISPLAY lin-espacion-blanco
                   DISPLAY "CAMPO ELEGIDO INVALIDO"
                   PERFORM 440-ELIJO-CAMPO
                   PERFORM 450-PROCESO
           END-EVALUATE.

       450-GRABAR-MODIFICACION.
           REWRITE SOC-REG INVALID KEY
                               DISPLAY "ERROR EN LA GRABACION"
                           NOT INVALID KEY
                               DISPLAY "MOFIFICACION CONFIRMADA".

      ******************************************************************
      ************************ R U T I N A S OPC-4 *********************
      ******************************************************************

       500-CONSULTAS.
            PERFORM 110-INGRESO-COD-SOC.
            PERFORM 510-PROCESO-CONSULTAS UNTIL SOC-COD = 0.

       510-PROCESO-CONSULTAS.
           PERFORM 130-LEER-REGISTRO.
           PERFORM 110-INGRESO-COD-SOC.

       520-NOT-INVALID-KEY-CONS.
           DISPLAY SOC-NOM.
           DISPLAY SOC-SALDO.

       END PROGRAM YOUR-PROGRAM-NAME.
