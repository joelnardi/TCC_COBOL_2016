//KC03BAB JOB (12345678),'JOEL',MSGLEVEL=(1,1),REGION=0M,            
// NOTIFY=&SYSUID,MSGCLASS=A,CLASS=A                                 
//****                                                               
//COBOL1 EXEC IGYWCLG,                                               
// PARM.COBOL='TEST,RENT,APOST,OBJECT,NODYNAM,LIB,SIZE(5048376)'     
//COBOL.SYSPRINT DD SYSOUT=*                                         
//COBOL.SYSIN DD *                                                   
        IDENTIFICATION DIVISION.                                     
        PROGRAM-ID. MODI.                                            
        ENVIRONMENT DIVISION.                                        
        CONFIGURATION SECTION.                                       
        INPUT-OUTPUT SECTION.                                        
        FILE-CONTROL.                                                
            SELECT T07EMODI ASSIGN TO T07EMODI.                      
            SELECT T07SMODI ASSIGN TO T07SMODI.                      
            SELECT PPAL ASSIGN TO PPAL.                              
            SELECT T07REPUE ASSIGN TO T07REPUE.                      
        DATA DIVISION.                                               
        FILE SECTION.                                                
        FD T07EMODI                                                  
            BLOCK CONTAINS 0 RECORDS.                                
        01 REG-T07EMODI PIC X(240).                                  
        FD T07SMODI                                                  
            LABEL RECORDS ARE OMITTED.                               
        01 REG-T07SMODI PIC X(126).                                  
        FD PPAL                                                      
            LABEL RECORDS ARE OMITTED.                               
        01 REG-PPAL PIC X(240).                                      
        FD T07REPUE                                                  
            BLOCK CONTAINS 0 RECORDS.                                
        01 REG-T07REPUE PIC X(240).                                  
        WORKING-STORAGE SECTION.                                     
        01 REGISTRO-T07EMODI.                                        
         03 IDREPUESTO-T07EMODI PIC 9(3).                            
         03 IDCLIENTE-T07EMODI PIC 9(5).                             
         03 IDREPUESTO-MODI-T07EMODI PIC 9(3).                 
        01 REGISTRO-T07SMODI.                                  
         03 IDREPUESTO-T07SMODI PIC 9(3).                      
         03 IDCLIENTE-T07SMODI PIC 9(5).                       
         03 IDREPUESTO-MODI-T07SMODI PIC 9(3).                 
         03 RESULTADO-T07SMODI PIC X(15).                      
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
        01 REGISTRO-T07REPUE.                                  
         03 IDREPUESTO-REPUE PIC 9(3).                         
         03 DESCREPUESTO-REPUE PIC X(30).                      
        01 EOF-T07REPUE PIC 9 VALUE 0.                         
        01 EXISTE-REPU PIC 9 VALUE 0.                          
        01 EXISTE-REPU-ACT PIC 9 VALUE 0.                      
        01 EOF-PPAL PIC 9 VALUE 0.                             
        01 EOF-T07EMODI PIC 9 VALUE 0.                         
        PROCEDURE DIVISION.                                    
            OPEN INPUT T07EMODI.                               
            OPEN OUTPUT T07SMODI.                              
            READ T07EMODI INTO REGISTRO-T07EMODI.              
            PERFORM MODIF-REGISTRO UNTIL EOF-T07EMODI = 1.     
            CLOSE T07EMODI T07SMODI.                           
            STOP RUN.                                          
        MODIF-REGISTRO.                                        
            OPEN INPUT T07REPUE.                               
            OPEN I-O PPAL.                                     
            MOVE 0 TO EOF-T07REPUE.                            
            MOVE 0 TO EXISTE-REPU.                             
            PERFORM BUSCAR-REPU UNTIL EOF-T07REPUE = 1         
            OR EXISTE-REPU = 1.                                         
            IF EXISTE-REPU = 0 THEN                                     
              MOVE IDREPUESTO-T07EMODI TO IDREPUESTO-T07SMODI           
              MOVE IDCLIENTE-T07EMODI TO IDCLIENTE-T07SMODI             
              MOVE IDREPUESTO-MODI-T07EMODI TO IDREPUESTO-MODI-T07SMODI 
              MOVE ' INEXISTENTE' TO RESULTADO-T07SMODI                 
              WRITE REG-T07SMODI FROM REGISTRO-T07SMODI.                
            IF EXISTE-REPU = 1 THEN                                     
                MOVE 0 TO EOF-PPAL                                      
                MOVE 0 TO EXISTE-REPU-ACT                               
                PERFORM BUSCAR-REPU-ACTUAL UNTIL EOF-PPAL = 1           
                OR EXISTE-REPU-ACT = 1.                                 
            IF EXISTE-REPU = 1 AND EXISTE-REPU-ACT = 0 THEN             
              MOVE IDREPUESTO-T07EMODI TO IDREPUESTO-T07SMODI           
              MOVE IDCLIENTE-T07EMODI TO IDCLIENTE-T07SMODI             
              MOVE IDREPUESTO-MODI-T07EMODI TO IDREPUESTO-MODI-T07SMODI 
              MOVE ' NO CAMBIADO' TO RESULTADO-T07SMODI                 
              WRITE REG-T07SMODI FROM REGISTRO-T07SMODI.                
            IF EXISTE-REPU = 1 AND EXISTE-REPU-ACT = 1 THEN             
              MOVE IDREPUESTO-T07EMODI TO IDREPUESTO-T07SMODI           
              MOVE IDCLIENTE-T07EMODI TO IDCLIENTE-T07SMODI             
              MOVE IDREPUESTO-MODI-T07EMODI TO IDREPUESTO-MODI-T07SMODI 
              MOVE ' MODIFICADO' TO RESULTADO-T07SMODI                  
              WRITE REG-T07SMODI FROM REGISTRO-T07SMODI                 
              MOVE IDREPUESTO-MODI-T07EMODI TO IDREPUESTO-PPAL          
              MOVE DESCREPUESTO-REPUE TO DESCREPUESTO-PPAL              
              REWRITE REG-PPAL FROM REGISTRO-PPAL.                      
            CLOSE PPAL T07REPUE.                                        
            READ T07EMODI INTO REGISTRO-T07EMODI                        
            AT END MOVE 1 TO EOF-T07EMODI.                              
        BUSCAR-REPU-ACTUAL.                                             
            READ PPAL INTO REGISTRO-PPAL AT END MOVE 1 TO EOF-PPAL.     
            IF IDCLIENTE-T07EMODI = IDCLIENTE-PPAL                      
                AND IDREPUESTO-T07EMODI = IDREPUESTO-PPAL THEN          
                MOVE 1 TO EXISTE-REPU-ACT.                              
        BUSCAR-REPU.                                                    
            READ T07REPUE INTO REGISTRO-T07REPUE                        
            AT END MOVE 1 TO EOF-T07REPUE.                              
            IF IDREPUESTO-MODI-T07EMODI = IDREPUESTO-REPUE THEN  
                MOVE 1 TO EXISTE-REPU.                           
//GO.SYSOUT DD SYSOUT=*                                          
//GO.SYSPRINT DD SYSOUT=*                                        
//GO.T07EMODI DD DSN=KC03BAB.CATALOGO.ENTRADA(T07EMODI),DISP=SHR 
//GO.T07SMODI DD DSN=KC03BAB.CATALOGO.SALIDA(T07SMODI),DISP=SHR  
//GO.T07REPUE DD DSN=KC03BAB.CATALOGO.ENTRADA(T07REPUE),DISP=SHR 
//GO.PPAL DD DSN=KC03BAB.CATALOGO.PPAL,DISP=SHR
