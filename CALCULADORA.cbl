       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADORA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC S999999999V99.
       01 NUM2 PIC S999999999V99.
       01 RESULTADO PIC S999999999V99.
       01 OPCION_OPERACION PIC 9(2).
       01 VALIDACION_MENU PIC X.
       01 VALIDACION_NUMERO PIC X.
       01 VALIDACION-PREGUNTA PIC X(2).
       01 CHECK_NUM1 PIC X(9).
       01 CHECK_NUM2 PIC X(9).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE1.

           PERFORM CAPTURA-DATOS.
           PERFORM OPERACIONES-CAPTURA UNTIL VALIDACION_MENU = "S".
           PERFORM OPERACIONES-REALIZA.
           PERFORM DISPLAY-USUARIO.
           PERFORM DISPLAY-PREGUNTA.
           STOP RUN.

           CAPTURA-DATOS.
               DISPLAY "CALCULADORA"
               DISPLAY "Ingrese el primer numero: "
           ACCEPT CHECK_NUM1.
               DISPLAY "Ingrese el segundo numero: "
           ACCEPT CHECK_NUM2.
           PERFORM EXCEPCIONES_NUMEROS.
           PERFORM CAPTURA-DATOS UNTIL VALIDACION_NUMERO = "S".


           OPERACIONES-CAPTURA.
           MOVE ZERO TO OPCION_OPERACION.
               DISPLAY "Ingresa una de las siguientes opciones:"
               DISPLAY "1.- Suma" DISPLAY "2.- Resta"
               DISPLAY "3.- Multiplicacion" DISPLAY "4.- Division"
               DISPLAY "5. Salir"
           ACCEPT OPCION_OPERACION.
           PERFORM EXCEPCIONES_MENU.


           OPERACIONES-REALIZA.
           EVALUATE TRUE
           WHEN OPCION_OPERACION = 1
           COMPUTE RESULTADO = NUM1 + NUM2

           WHEN OPCION_OPERACION = 2
           COMPUTE RESULTADO = NUM1 - NUM2

           WHEN OPCION_OPERACION = 3
           COMPUTE RESULTADO = NUM1 * NUM2

           WHEN OPCION_OPERACION = 4
           COMPUTE RESULTADO = NUM1 / NUM2

           WHEN OPCION_OPERACION = 5
               DISPLAY "Salio del programa correctamente!"
               STOP RUN
           END-EVALUATE.

           EXCEPCIONES_NUMEROS.
           IF (CHECK_NUM1 IS ALPHABETIC)
               DISPLAY "Por favor introduce 2 numeros!"
               MOVE "N" TO VALIDACION_NUMERO
           ELSE
               IF (CHECK_NUM2 IS NOT ALPHABETIC)
                   MOVE "S" TO VALIDACION_NUMERO
                   MOVE CHECK_NUM1 TO NUM1
                   MOVE CHECK_NUM2 TO NUM2
               ELSE
                   DISPLAY "Por favor introduce 2 numeros!"
                   MOVE "N" TO VALIDACION_NUMERO
               END-IF
           END-IF.

           EXCEPCIONES_MENU.
           IF OPCION_OPERACION = 1 OR OPCION_OPERACION = 2
           OR OPCION_OPERACION = 3 OR OPCION_OPERACION = 4
           OR OPCION_OPERACION = 5
               MOVE "S" TO VALIDACION_MENU
           ELSE
               DISPLAY "Ingrese una opcion valida!"
               MOVE "N" TO VALIDACION_MENU
           END-IF.


           DISPLAY-USUARIO.
               DISPLAY "El resultado de operacion es: "
           DISPLAY RESULTADO.

           DISPLAY-PREGUNTA.
               DISPLAY "¿Quiere ejecutar otra operacion?"
               DISPLAY "S/N"
           ACCEPT VALIDACION-PREGUNTA.
           IF VALIDACION-PREGUNTA = "S" OR VALIDACION-PREGUNTA = "s"
               GO TO CAPTURA-DATOS.
           IF VALIDACION-PREGUNTA = "N" OR VALIDACION-PREGUNTA = "n"
               DISPLAY "Gracias por utilizar la calculadora!"
           ELSE
               DISPLAY "Ingrese una opcion correcta!!"
               PERFORM DISPLAY-PREGUNTA
           END-IF.


       END PROGRAM CALCULADORA.
