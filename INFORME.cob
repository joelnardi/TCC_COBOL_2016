Informe
//KC03BAB JOB (12345678),'JOEL',MSGLEVEL=(1,1),REGION=0M,               
// NOTIFY=&SYSUID,MSGCLASS=A,CLASS=A                                    
//****                                                                  
//COBOL1 EXEC IGYWCLG,                                                  
// PARM.COBOL='TEST,RENT,APOST,OBJECT,NODYNAM,LIB,SIZE(5048376)'        
//COBOL.SYSPRINT DD SYSOUT=*                                            
//COBOL.SYSIN DD *                                                      
        IDENTIFICATION DIVISION.                                        
        PROGRAM-ID. INFO.                                               
        ENVIRONMENT DIVISION.                                           
        CONFIGURATION SECTION.                                          
        INPUT-OUTPUT SECTION.                                           
        FILE-CONTROL.                                                   
            SELECT T07SINFO ASSIGN TO T07SINFO.                         
            SELECT PPAL ASSIGN TO PPAL.                                 
        DATA DIVISION.                                                  
        FILE SECTION.                                                   
        FD T07SINFO                                                     
            LABEL RECORDS ARE OMITTED.                                  
        01 REG-T07SINFO PIC X(126).                                     
        FD PPAL                                                         
            BLOCK CONTAINS 0 RECORDS.                                   
        01 REG-PPAL PIC X(240).                                         
        WORKING-STORAGE SECTION.                                        
        01 TITULO.                                                      
         03 IDREPUESTO-TITULO PIC X(6) VALUE 'IDREP|'.                  
         03 DESCREPUESTO-TITULO PIC X(21) VALUE 'DESCREPUESTO         '.
         03 DESCREP-TIT PIC X(10) VALUE '         |'.                   
         03 IDCLIENTE-TITULO PIC X(6) VALUE 'IDCLI|'.                   
         03 MODAUTO-TITULO PIC X(27) VALUE 'MODELO DE AUTO            '.
         03 MODAUTO-FILLER PIC X(4) VALUE '   |'.                       
         03 PATENTE-TITULO PIC X(8) VALUE 'PATENTE|'.                   
         03 DNI-TITULO PIC X(9) VALUE 'DNI     |'.                      
         03 FECING-TITULO PIC X(7) VALUE 'FECING|'.                     
         03 DESCREP-TITULO PIC X(27) VALUE 'DESC REPARACION           '.
        01 LINEA.                                                      
         03 LINEA1 PIC X(33) VALUE '_________________________________'.
         03 LINEA2 PIC X(33) VALUE '_________________________________'.
         03 LINEA3 PIC X(33) VALUE '_________________________________'.
         03 LINEA4 PIC X(26) VALUE '________________________'.         
        01 REGISTRO-T07SINFO.                                          
         03 IDREPUESTO-T07SINFO PIC 9(3).                              
         03 FSSLER PIC X(3) VALUE '  |'.                               
         03 DESCREPUESTO-T07SINFO PIC X(30).                           
         03 FIASER PIC X(1) VALUE '|'.                                 
         03 IDCLIENTE-T07SINFO PIC 9(5).                               
         03 FILSER PIC X(1) VALUE '|'.                                 
         03 MODELOAUTO-T07SINFO PIC X(30).                             
         03 FILSSR PIC X(1) VALUE '|'.                                 
         03 PATENTE-T07SINFO PIC X(7).                                 
         03 FILLAR PIC X(1) VALUE '|'.                                 
         03 DNI-T07SINFO PIC X(8).                                     
         03 FILLDR PIC X(1) VALUE '|'.                                 
         03 FECHAINGRESO-T07SINFO PIC 9(6).                            
         03 FILSES PIC X(1) VALUE '|'.                                 
         03 DESCREPARACION-T07SINFO PIC X(30).                         
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
         03 MODELOAUTO-T07SCONS PIC X(30).                             
         03 DESCREPUESTO-T07SCONS PIC X(30).                           
         03 PATENTE-T07SCONS PIC X(7).                                 
         03 DESCREPARACION-T07SCONS PIC X(30).                         
        PROCEDURE DIVISION.                                          
            OPEN OUTPUT T07SINFO.                                    
            OPEN INPUT PPAL.                                         
            WRITE REG-T07SINFO FROM TITULO.                          
            WRITE REG-T07SINFO FROM LINEA.                           
            WRITE REG-T07SINFO FROM REPUESTOS.                       
            READ PPAL INTO REGISTRO-PPAL.                            
            PERFORM EMITIR-INFORME UNTIL EOF-PPAL = 1.               
            CLOSE T07SINFO PPAL.                                     
            STOP RUN.                                                
        EMITIR-INFORME.                                              
            IF ESTADO-PPAL = 'V' THEN                                
                MOVE IDCLIENTE-PPAL TO IDCLIENTE-T07SINFO            
                MOVE DESCREPUESTO-PPAL TO DESCREPUESTO-T07SINFO      
                MOVE IDREPUESTO-PPAL TO IDREPUESTO-T07SINFO          
                MOVE MODELOAUTO-PPAL TO MODELOAUTO-T07SINFO          
                MOVE PATENTE-PPAL TO PATENTE-T07SINFO                
                MOVE DNI-PPAL TO DNI-T07SINFO                        
                MOVE DESCREPARACION-PPAL TO DESCREPARACION-T07SINFO  
                MOVE FECHAINGRESO-PPAL TO FECHAINGRESO-T07SINFO      
                WRITE REG-T07SINFO FROM REGISTRO-T07SINFO.           
            READ PPAL INTO REGISTRO-PPAL AT END MOVE 1 TO EOF-PPAL.  
//GO.SYSOUT DD SYSOUT=*                                              
//GO.SYSPRINT DD SYSOUT=*                                            
//GO.T07SINFO DD DSN=KC03BAB.CATALOGO.SALIDA(T07SINFO),DISP=SHR      
//GO.PPAL DD DSN=KC03BAB.CATALOGO.PPAL,DISP=SHR
