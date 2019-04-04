//KC03BAB JOB (12345678),'JOEL',MSGLEVEL=(1,1),REGION=0M,               
// NOTIFY=&SYSUID,MSGCLASS=A,CLASS=A                                    
//****                                                                  
//COBOL1 EXEC IGYWCLG,                                                  
// PARM.COBOL='TEST,RENT,APOST,OBJECT,NODYNAM,LIB,SIZE(5048376)'        
//COBOL.SYSPRINT DD SYSOUT=*                                            
//COBOL.SYSIN DD *                                                      
        IDENTIFICATION DIVISION.                                        
        PROGRAM-ID. ALTA.                                               
        ENVIRONMENT DIVISION.                                           
        CONFIGURATION SECTION.                                          
        INPUT-OUTPUT SECTION.                                           
        FILE-CONTROL.                                                   
            SELECT T07EALTA ASSIGN TO T07EALTA.                         
            SELECT T07CLIEN ASSIGN TO T07CLIEN.                         
            SELECT T07REPUE ASSIGN TO T07REPUE.                         
            SELECT PPAL ASSIGN TO PPAL.                                 
            SELECT T07SALTA ASSIGN TO T07SALTA.                         
        DATA DIVISION.                                                  
        FILE SECTION.                                                   
        FD T07EALTA                                                     
            BLOCK CONTAINS 0 RECORDS.                                   
        01 REG-T07EALTA PIC X(240).                                     
        FD T07CLIEN                                                     
            BLOCK CONTAINS 0 RECORDS.                                   
        01 REG-T07CLIEN PIC X(240).                                     
        FD T07REPUE                                                     
            BLOCK CONTAINS 0 RECORDS.                                   
        01 REG-T07REPUE PIC X(240).                                     
        FD PPAL                                                         
            BLOCK CONTAINS 0 RECORDS.                                   
        01 REG-PPAL PIC X(240).                                         
        FD T07SALTA                                                     
            LABEL RECORDS ARE OMITTED.                                  
        01 REG-T07SALTA PIC X(126).                                     
 WORKING-STORAGE SECTION.                    
 01 REGISTRO-T07EALTA.                       
  03 IDCLIENTE-T07EALTA PIC 9(5).            
  03 IDREPUESTO-T07EALTA PIC 9(3).           
  03 FECHAINGRESO-T07EALTA PIC 9(6).         
  03 DESCREPARACION-T07EALTA PIC X(30).      
 01 REGISTRO-T07SALTA.                       
  03 IDCLIENTE-T07SALTA PIC 9(5).            
  03 IDREPUESTO-T07SALTA PIC 9(3).           
  03 FECHAINGRESO-T07SALTA PIC 9(6).         
  03 DESCREPARACION-T07SALTA PIC X(30).      
  03 RESULTADO-T07SALTA PIC X(15).           
 01 EOF-T07CLIEN PIC 9 VALUE 0.              
 01 EXISTE-CLIEN PIC 9 VALUE 0.              
 01 EXISTE-REPUE PIC 9 VALUE 0.              
 01 REGISTRO-T07CLIEN.                       
  03 IDCLIENTE-CLIEN PIC 9(5).               
  03 MODELOAUTO-CLIEN PIC X(30).             
  03 PATENTE-CLIEN PIC X(7).                 
  03 DNI-CLIEN PIC X(8).                     
 01 REGISTRO-T07REPUE.                       
  03 IDREPUESTO-REPUE PIC 9(3).              
  03 DESCREPUESTO-REPUE PIC X(30).           
 01 EOF-PPAL PIC 9 VALUE 0.                  
 01 REPETIDO PIC 9 VALUE 0.                  
 01 REGISTRO-PPAL.                           
  03 IDREPUESTO-PPAL PIC 9(3).               
  03 DESCREPUESTO-PPAL PIC X(30).            
  03 IDCLIENTE-PPAL PIC 9(5).                
  03 MODELOAUTO-PPAL PIC X(30).              
  03 PATENTE-PPAL PIC X(7).                  
  03 DNI-PPAL PIC X(8).                      
  03 DESCREPARACION-PPAL PIC X(30).          
  03 FECHAINGRESO-PPAL PIC 9(6).             
  03 FECHAEGRESO-PPAL PIC 9(6).              
  03 ESTADO-PPAL PIC X(1).                   
 01 EOF-T07EALTA PIC 9 VALUE 0.              
 01 EOF-T07REPUE PIC 9 VALUE 0.              
 PROCEDURE DIVISION.                                             
     OPEN INPUT T07EALTA.                                        
     OPEN OUTPUT T07SALTA.                                       
     READ T07EALTA INTO REGISTRO-T07EALTA.                       
     PERFORM ALTA-REGISTRO UNTIL EOF-T07EALTA = 1.               
     PERFORM TERMINAR.                                           
 ALTA-REGISTRO.                                                  
     OPEN INPUT T07CLIEN.                                        
     OPEN INPUT T07REPUE.                                        
     MOVE 0 TO EXISTE-CLIEN.                                     
     MOVE 0 TO EXISTE-REPUE.                                     
     MOVE 0 TO REPETIDO.                                         
     MOVE 0 TO EOF-T07CLIEN.                                     
     MOVE 0 TO EOF-T07REPUE.                                     
     MOVE 0 TO EOF-PPAL.                                         
     PERFORM EXISTENCIA-T07CLIEN WITH TEST BEFORE UNTIL          
     EOF-T07CLIEN = 1 OR EXISTE-CLIEN = 1.                       
     IF EXISTE-CLIEN = 1 THEN                                    
         PERFORM EXISTENCIA-T07REPUE UNTIL EOF-T07REPUE = 1      
         OR EXISTE-REPUE = 1.                                    
     IF EXISTE-CLIEN = 0 OR EXISTE-REPUE = 0 THEN                
         MOVE REGISTRO-T07EALTA TO REGISTRO-T07SALTA             
         MOVE 'INEXISTENTE' TO RESULTADO-T07SALTA                
         WRITE REG-T07SALTA FROM REGISTRO-T07SALTA.              
     IF EXISTE-CLIEN = 1 AND EXISTE-REPUE = 1 THEN               
         OPEN INPUT PPAL                                         
         PERFORM REPETIDO-PPAL UNTIL EOF-PPAL = 1 OR REPETIDO = 1
         CLOSE PPAL.                                             
     IF REPETIDO = 1 AND EXISTE-CLIEN = 1 AND EXISTE-REPUE = 1   
     THEN                                                        
        MOVE REGISTRO-T07EALTA TO REGISTRO-T07SALTA              
        MOVE 'DUPLICADO' TO RESULTADO-T07SALTA                   
        WRITE REG-T07SALTA FROM REGISTRO-T07SALTA.               
     IF REPETIDO = 0 AND EXISTE-CLIEN = 1 AND EXISTE-REPUE = 1   
     THEN                                                        
        OPEN EXTEND PPAL                                         
        MOVE REGISTRO-T07EALTA TO REGISTRO-T07SALTA              
        MOVE 'OK' TO RESULTADO-T07SALTA                          
        WRITE REG-T07SALTA FROM REGISTRO-T07SALTA                
        MOVE IDCLIENTE-CLIEN TO IDCLIENTE-PPAL                   
        MOVE MODELOAUTO-CLIEN TO MODELOAUTO-PPAL                 
        MOVE IDREPUESTO-REPUE TO IDREPUESTO-PPAL                 
               MOVE DESCREPUESTO-REPUE TO DESCREPUESTO-PPAL             
        MOVE FECHAINGRESO-T07EALTA TO FECHAINGRESO-PPAL          
               MOVE DESCREPARACION-T07EALTA TO DESCREPARACION-PPAL      
               MOVE 000000 TO FECHAEGRESO-PPAL                        
MOVE 'V' TO ESTADO-PPAL                                  
        WRITE REG-PPAL FROM REGISTRO-PPAL                        
               CLOSE PPAL.                                              
           CLOSE T07CLIEN T07REPUE.                                     
           READ T07EALTA INTO REGISTRO-T07EALTA                         
           AT END MOVE 1 TO EOF-T07EALTA.                               
       EXISTENCIA-T07CLIEN.                                             
           READ T07CLIEN INTO REGISTRO-T07CLIEN                         
           AT END MOVE 1 TO EOF-T07CLIEN.                               
           IF IDCLIENTE-CLIEN = IDCLIENTE-T07EALTA THEN                 
                   MOVE 1 TO EXISTE-CLIEN.                              
       EXISTENCIA-T07REPUE.                                             
           READ T07REPUE INTO REGISTRO-T07REPUE                         
           AT END MOVE 1 TO EOF-T07REPUE.                               
           IF IDREPUESTO-REPUE = IDREPUESTO-T07EALTA THEN               
                   MOVE 1 TO EXISTE-REPUE.                              
       REPETIDO-PPAL.                                                   
           READ PPAL INTO REGISTRO-PPAL AT END MOVE 1 TO EOF-PPAL.      
           IF IDCLIENTE-T07EALTA = IDCLIENTE-PPAL AND                   
               IDREPUESTO-T07EALTA = IDREPUESTO-PPAL THEN               
               MOVE 1 TO REPETIDO.                                      
       TERMINAR.                                                        
           CLOSE T07EALTA T07SALTA.                                     
//GO.SYSOUT DD SYSOUT=*                                                 
//GO.SYSPRINT DD SYSOUT=*                                               
//GO.T07EALTA DD DSN=KC03BAB.CATALOGO.ENTRADA(T07EALTA),DISP=SHR        
//GO.T07CLIEN DD DSN=KC03BAB.CATALOGO.ENTRADA(T07CLIEN),DISP=SHR        
//GO.T07REPUE DD DSN=KC03BAB.CATALOGO.ENTRADA(T07REPUE),DISP=SHR        
//GO.T07SALTA DD DSN=KC03BAB.CATALOGO.SALIDA(T07SALTA),DISP=SHR         
//GO.PPAL DD DSN=KC03BAB.CATALOGO.PPAL,DISP=SHR
