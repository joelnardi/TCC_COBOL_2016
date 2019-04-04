//KC03BAB JOB (12345678),'JOEL',MSGLEVEL=(1,1),REGION=0M,        
// NOTIFY=&SYSUID,MSGCLASS=A,CLASS=A                             
//****                                                           
//COBOL1 EXEC IGYWCLG,                                           
// PARM.COBOL='TEST,RENT,APOST,OBJECT,NODYNAM,LIB,SIZE(5048376)' 
//COBOL.SYSPRINT DD SYSOUT=*                                     
//COBOL.SYSIN DD *                                               
         IDENTIFICATION DIVISION.                                
         PROGRAM-ID. BAJA.                                       
         ENVIRONMENT DIVISION.                                   
         CONFIGURATION SECTION.                                  
         INPUT-OUTPUT SECTION.                                   
         FILE-CONTROL.                                           
             SELECT PPAL ASSIGN TO PPAL.                         
             SELECT T07EBAJA ASSIGN TO T07EBAJA.                 
             SELECT T07SBAJA ASSIGN TO T07SBAJA.                 
         DATA DIVISION.                                          
         FILE SECTION.                                           
         FD T07EBAJA                                             
             BLOCK CONTAINS 0 RECORDS.                           
         01 REG-T07EBAJA PIC X(240).                             
         FD PPAL                                                 
             LABEL RECORDS ARE OMITTED.                          
         01 REG-PPAL PIC X(240).                                 
         FD T07SBAJA                                             
             LABEL RECORDS ARE OMITTED.                          
         01 REG-T07SBAJA PIC X(126).                             
         WORKING-STORAGE SECTION.                                
         01 REGISTRO-T07EBAJA.                                   
          03 IDREPUESTO-T07EBAJA PIC 9(3).                       
          03 IDCLIENTE-T07EBAJA PIC 9(5).                        
          03 FECHAEGRESO-T07EBAJA PIC 9(6).                      
         01 REGISTRO-T07SBAJA.                                   
          03 IDREPUESTO-T07SBAJA PIC 9(3).                       
          03 IDCLIENTE-T07SBAJA PIC 9(5).                        
       03 FECHAEGRESO-T07SBAJA PIC 9(6).                         
       03 RESULTADO-T07SBAJA PIC X(15).                          
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
      01 EOF-PPAL PIC 9 VALUE 0.                                 
      01 EXISTE PIC 9 VALUE 0.                                   
      01 EOF-T07EBAJA PIC 9 VALUE 0.                             
      PROCEDURE DIVISION.                                        
          OPEN INPUT T07EBAJA.                                   
          OPEN OUTPUT T07SBAJA.                                  
          READ T07EBAJA INTO REGISTRO-T07EBAJA.                  
          PERFORM BAJA-REGISTRO UNTIL EOF-T07EBAJA = 1.          
          CLOSE T07EBAJA T07SBAJA.                               
          STOP RUN.                                              
      BAJA-REGISTRO.                                             
          OPEN I-O PPAL.                                         
          MOVE 0 TO EXISTE.                                      
          MOVE 0 TO EOF-PPAL.                                    
          PERFORM BUSCA-PPAL UNTIL EOF-PPAL = 1 OR EXISTE = 1.   
          IF EXISTE = 0 THEN                                     
              MOVE IDCLIENTE-T07EBAJA TO IDCLIENTE-T07SBAJA      
              MOVE IDREPUESTO-T07EBAJA TO IDREPUESTO-T07SBAJA    
              MOVE FECHAEGRESO-T07EBAJA TO FECHAEGRESO-T07SBAJA  
              MOVE ' INEXISTENTE' TO RESULTADO-T07SBAJA          
              WRITE REG-T07SBAJA FROM REGISTRO-T07SBAJA.         
          IF EXISTE = 1 THEN                                     
              MOVE IDCLIENTE-T07EBAJA TO IDCLIENTE-T07SBAJA      
              MOVE IDREPUESTO-T07EBAJA TO IDREPUESTO-T07SBAJA    
              MOVE FECHAEGRESO-T07EBAJA TO FECHAEGRESO-T07SBAJA  
                 MOVE ' ELIMINADO' TO RESULTADO-T07SBAJA            
                 WRITE REG-T07SBAJA FROM REGISTRO-T07SBAJA.         
                 MOVE FECHAEGRESO-T07EBAJA TO FECHAEGRESO-PPAL      
                 MOVE 'E' TO ESTADO-PPAL                            
                 REWRITE REG-PPAL FROM REGISTRO-PPAL.               
             CLOSE PPAL.                                            
             READ T07EBAJA INTO REGISTRO-T07EBAJA                   
             AT END MOVE 1 TO EOF-T07EBAJA.                         
         BUSCA-PPAL.                                                
             READ PPAL INTO REGISTRO-PPAL AT END MOVE 1 TO EOF-PPAL.
             IF IDCLIENTE-T07EBAJA = IDCLIENTE-PPAL                 
                 AND IDREPUESTO-T07EBAJA = IDREPUESTO-PPAL THEN     
                 MOVE 1 TO EXISTE.                                  
//GO.SYSOUT DD SYSOUT=*                                             
//GO.SYSPRINT DD SYSOUT=*                                           
//GO.T07EBAJA DD DSN=KC03BAB.CATALOGO.ENTRADA(T07EBAJA),DISP=SHR    
//GO.T07SBAJA DD DSN=KC03BAB.CATALOGO.SALIDA(T07SBAJA),DISP=SHR     
//GO.PPAL DD DSN=KC03BAB.CATALOGO.PPAL,DISP=SHR 
