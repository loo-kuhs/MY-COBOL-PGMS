      *================================================================*
      *           L O G    D E    M O D I F I C A C I O N E S          *
      *================================================================*
      *  AUTOR       FECHA   DESCRIPCION                               *
      * ----------- -------- ----------------------------------------- *
      * @loo-kuhs   02/10/23 PRIMERA VERSION DEL SCRIPT                *
      *                                                                *
      *                                                                *
      *================================================================*
      * AUTOR       USER PROFILE URL                                   *
      * ---------   -------------------------------------------------- *
      * @loo-kuhs   https://github.com/loo-kuhs/                       *
      *================================================================*
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. DYNAMIC-FILE-CREATION.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT ARCHIVO-LECTURA
               ASSIGN       TO ARCHIVO-ENTRADA
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT ARCHIVO-SALIDA
               ASSIGN       TO NOMBRE-ARCHIVO
               ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD  ARCHIVO-LECTURA.
       01  REG-ENTRADA                      PIC X(30000).

       FD  ARCHIVO-SALIDA.
       01  REG-SALIDA                       PIC X(30000).
      *-----------------------
       WORKING-STORAGE SECTION.
       01  ID-REG-LEIDO                     PIC X(30000).

       01  ARCHIVO-ENTRADA                  PIC X(100).
       01  NOMBRE-ARCHIVO                   PIC X(50).

       01  ARCHIVO-FORMATO.
           05 NOMBRE-INICIAL                PIC X(44).
           05 NUMERO-ARCHIVO                PIC 9(02).
           05 EXTENCION-ARCHIVO             PIC X(04).

       01  PROCESO-REGISTROS.
           05 REG-LEIDOS                    PIC 9(09) VALUE 00.
           05 REG-ESCRITOS                  PIC 9(09) VALUE 00.

       01  FORMATO-CIFRAS-CONTROL.
           05 FORMATO-LEI                   PIC ZZZ,ZZZ,ZZ9.
           05 FORMATO-ESC                   PIC ZZZ,ZZZ,ZZ9.

       77  FIN-ARCHIVO                      PIC 9(01) VALUE 00.
       77  CONTADOR-REGS                    PIC 9(09) VALUE 00.
       77  CONTADOR-ARCHIVO                 PIC 9(02) VALUE 00.
       77  LIMITE-REGISTROS                 PIC 9(09) VALUE 00.
       77  LONGITUD-NOMBRE                  PIC 9(02) VALUE 00.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       000-CONTROLAR-PROGRAMA.
           PERFORM 010-INICIAR-PROGRAMA
           PERFORM 020-MOVER-N-REGISTROS
               UNTIL FIN-ARCHIVO = 01
           PERFORM 100-TERMINAR-PROGRAMA
           .

       010-INICIAR-PROGRAMA.

           DISPLAY "NOMBRE DEL ARCHIVO A EXTRAER: "
           ACCEPT  ARCHIVO-ENTRADA

           DISPLAY "INGRESE EL NOMBRE INICIAL PARA LOS ARCHIVOS: "
           ACCEPT  NOMBRE-INICIAL

           DISPLAY X"0D"
           DISPLAY "INGRESE LA EXTENCION PARA LOS ARCHIVOS: "
           DISPLAY "EJEMPLOS: csv dat txt"
           ACCEPT  EXTENCION-ARCHIVO

           DISPLAY X"0D"
           DISPLAY "INGRESE EL LIMITE DE REGISTROS POR ARCHIVO: "
           ACCEPT  LIMITE-REGISTROS

           INSPECT ARCHIVO-ENTRADA          REPLACING ALL X"0D" BY " "
           INSPECT NOMBRE-INICIAL           REPLACING ALL X"0D" BY " "
           INSPECT EXTENCION-ARCHIVO        REPLACING ALL X"0D" BY " "
           INSPECT LIMITE-REGISTROS         REPLACING ALL X"0D" BY " "

           MOVE CONTADOR-ARCHIVO            TO NUMERO-ARCHIVO

           STRING NOMBRE-INICIAL,
                  NUMERO-ARCHIVO,
                  ".",
                  EXTENCION-ARCHIVO,
                  DELIMITED BY SPACE
               INTO NOMBRE-ARCHIVO
           END-STRING

           OPEN  INPUT ARCHIVO-LECTURA
                OUTPUT ARCHIVO-SALIDA

           PERFORM 500-LEER-REGISTRO
           .

       020-MOVER-N-REGISTROS.

           MOVE  ID-REG-LEIDO               TO REG-SALIDA
           WRITE REG-SALIDA
           ADD   +01                        TO REG-ESCRITOS

           PERFORM 500-LEER-REGISTRO

           IF CONTADOR-REGS > LIMITE-REGISTROS THEN
               PERFORM 510-CERRAR-ABRIR-ARCHIVO
               DISPLAY NOMBRE-ARCHIVO, " CREADO, DATOS ESCRITOS ",
                       REG-ESCRITOS
               DISPLAY X"0D"
           END-IF

           .

       100-TERMINAR-PROGRAMA.
           CLOSE ARCHIVO-LECTURA
                 ARCHIVO-SALIDA

           PERFORM 990-PRESENTAR-CIFRAS-CONTROL

           STOP RUN
           .

       500-LEER-REGISTRO.

           READ ARCHIVO-LECTURA
               AT END
                   MOVE 01                  TO FIN-ARCHIVO
               NOT AT END
                   ADD +01                  TO CONTADOR-REGS
                   ADD +01                  TO REG-LEIDOS
           END-READ

           MOVE REG-ENTRADA                 TO ID-REG-LEIDO
           .

       510-CERRAR-ABRIR-ARCHIVO.
           CLOSE ARCHIVO-SALIDA

           ADD +01                          TO CONTADOR-ARCHIVO
           MOVE CONTADOR-ARCHIVO            TO NUMERO-ARCHIVO
           STRING NOMBRE-INICIAL,
                  NUMERO-ARCHIVO,
                  ".",
                  EXTENCION-ARCHIVO,
                  DELIMITED BY SPACE
               INTO NOMBRE-ARCHIVO
           END-STRING

           OPEN OUTPUT ARCHIVO-SALIDA


           MOVE 00                          TO CONTADOR-REGS
           .

       990-PRESENTAR-CIFRAS-CONTROL.
           MOVE REG-LEIDOS                  TO FORMATO-LEI
           MOVE REG-ESCRITOS                TO FORMATO-ESC

           DISPLAY "****************************************"
           DISPLAY "*     REGISTROS LEIDOS: " FORMATO-LEI "    *"
           DISPLAY "*   REGISTROS ESCRITOS: " FORMATO-ESC "    *"
           DISPLAY "****************************************"
           .
       END PROGRAM DYNAMIC-FILE-CREATION.
