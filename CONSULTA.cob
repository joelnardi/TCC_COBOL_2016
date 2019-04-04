//KC03BAB JOB (12345678),'JOEL',MSGLEVEL=(1,1),REGION=0M,            
// NOTIFY=&SYSUID,MSGCLASS=A,CLASS=A                                 
//****                                                               
//COBOL1 EXEC IGYWCLG,                                               
// PARM.COBOL='TEST,RENT,APOST,OBJECT,NODYNAM,LIB,SIZE(5048376)'     
//COBOL.SYSPRINT DD SYSOUT=*                                         
//COBOL.SYSIN DD *                                                   
        IDENTIFICATION DIVISION.                                     
        PROGRAM-ID. CONS.                                            
        ENVIRONMENT DIVISION.                                        
        CONFIGURATION SECTION.                                       
        INPUT-OUTPUT SECTION.                                        
        FILE-CONTROL.                                                
            SELECT T07ECONS ASSIGN TO T07ECONS.                      
            SELECT T07SCONS ASSIGN TO T07SCONS.                      
            SELECT PPAL ASSIGN TO PPAL.                              
        DATA DIVISION.                                               
        FILE SECTION.                                                
        FD T07ECONS                                                  
            BLOCK CONTAINS 0 RECORDS.                                
        01 REG-T07ECONS PIC X(240).                                  
        FD T07SCONS                                                  
            LABEL RECORDS ARE OMITTED.                               
        01 REG-T07SCONS PIC X(126).                                  
        FD PPAL                                                      
            BLOCK CONTAINS 0 RECORDS.                                
        01 REG-PPAL PIC X(240).                                      
        WORKING-STORAGE SECTION.                                     
        01 REGISTRO-T07ECONS.                                        
         03 IDCLIENTE-T07ECONS PIC 9(5).                             
        01 REGISTRO-T07SCONS.                                        
         03 MODELOAUTO-T07SCONS PIC X(30).                           
         03 DESCREPUESTO-T07SCONS PIC X(30).                         
         03 PATENTE-T07SCONS PIC X(7).                               
         03 DESCREPARACION-T07SCONS PIC X(30).                       
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
        01 REPUESTOS.                                               
         03 MODELOAUTO-T07REPUE PIC X(30).                          
         03 DESCREPUESTO-T07REPUE PIC X(30).                        
         03 PATENTE-T07REPUE PIC X(7).                              
         03 DESCREPARACION-T07REPUE PIC X(30).                      
        PROCEDURE DIVISION.                                         
            OPEN INPUT T07ECONS.                                    
            OPEN OUTPUT T07SCONS.                                   
            OPEN INPUT PPAL.                                        
            READ T07ECONS INTO REGISTRO-T07ECONS.                   
            WRITE REG-T07SCONS FROM REPUESTOS.                      
            PERFORM CONSULTAR-REPU UNTIL EOF-PPAL = 1.              
            CLOSE T07ECONS T07SCONS PPAL.                           
            STOP RUN.                                               
        CONSULTAR-REPU.                                             
            READ PPAL INTO REGISTRO-PPAL AT END MOVE 1 TO EOF-PPAL. 
            IF IDCLIENTE-T07ECONS = IDCLIENTE-PPAL                  
                AND EOF-PPAL = 0 THEN                               
                MOVE MODELOAUTO-PPAL TO MODELOAUTO-T07SCONS         
                MOVE DESCREPUESTO-PPAL TO DESCREPUESTO-T07SCONS     
                MOVE PATENTE-PPAL TO PATENTE-T07SCONS               
                MOVE DESCREPARACION-PPAL TO DESCREPARACION-T07SCONS 
                WRITE REG-T07SCONS FROM REGISTRO-T07SCONS.          
//GO.T07ECONS DD DSN=KC03BAB.CATALOGO.ENTRADA(T07ECONS),DISP=SHR    
//GO.T07SCONS DD DSN=KC03BAB.CATALOGO.SALIDA(T07SCONS),DISP=SHR     
//GO.PPAL DD DSN=KC03BAB.CATALOGO.PPAL,DISP=SHR
