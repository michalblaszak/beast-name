//RGENJ   JOB 1,NOTIFY=&SYSUID,CLASS=A,MSGLEVEL=(1,1),REGION=0M
//***************************************************/
//C       EXEC CBCXCB,MBR=RGEN
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN       EXEC PGM=RGEN
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//FILEIN    DD PATH='/z/z57931/genesis.txt',PATHDISP=(KEEP,KEEP)
//FILEOUT   DD DSN=&SYSUID..GENESIS,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
