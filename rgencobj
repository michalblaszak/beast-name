//RGENCOBJ JOB 1,NOTIFY=&SYSUID,CLASS=A,MSGLEVEL=(1,1),REGION=0M
//*-------------------------------------------------------------
//COBOLCMP       EXEC IGYWCL
//COBOL.SYSIN    DD DSN=&SYSUID..SOURCE(CBLGEN),DISP=SHR
//LKED.SYSLMOD   DD DSN=&SYSUID..LOAD(CBLGEN),DISP=SHR
//*-------------------------------------------------------------
// IF RC = 0 THEN
//*-------------------------------------------------------------
//CBLRUN   EXEC PGM=CBLGEN
//STEPLIB  DD DSN=&SYSUID..LOAD,DISP=SHR
//GENDD    DD DSN=&SYSUID..GENESIS,DISP=SHR
//PRTLINE  DD SYSOUT=*,OUTLIM=100000
//*SYSOUT   DD SYSOUT=*,OUTLIM=100000
//SYSOUT   DD PATH='/z/z57931/out.txt',
//           PATHOPTS=(OCREAT,ORDWR),
//           PATHMODE=(SIRUSR,SIWUSR,
//             SIRGRP,SIROTH),
//           FILEDATA=TEXT
//CEEDUMP  DD DUMMY
//SYSUDUMP DD DUMMY
//*-------------------------------------------------------------
// ENDIF
