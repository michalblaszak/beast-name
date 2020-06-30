      *=================================================================
      * Program which looks for all sequences of Hebrew words which
      * sum of their numeric values gives either of the values:
      * 666, 616, 646, 665
      * The example text to be scanned is Bible - Genesis.
      *-----------------------------------------------------------------
      * Input:
      *    Sequential variable record data set
      *    Each record consists of one verse.
      *    Each verse start with 3 bytes chapter number followed by
      *    3 bytes of the verse number.
      *    The text is formed of Hebrew characters coded as UTF-8
      *    (two bytes each: [X'D7', X'90'-X'AA']).
      *    Words are separated by one-byte X'20' (ASCII blank).
      * Output:
      *    The console listing of all sequences of words giving one of
      *    the desired values.
      * Algorithm:
      *    The program iterates over the values: 666, 616, 646, 665
      *    For each value the program looks for a sequence of words
      *       which give the desired number. The program is using the
      *       progressive scan algorithm where pointers to first and 
      *       last word is advancing depending if the sum of word values
      *       is smaller or bigger than the desired value. If the sum 
      *       value is smaller then the end pointer is advancing. If the
      *       value is bigger then the start pointer is advancing.
      *       If the exact value is found both start and end pointer
      *       are advancing.
      *       The found sequence of words are printed in the following 
      *       format:
      *       <SC>-<SV>-<SW> : <EC>-<EV>-<EW>
      *       (list of words transliterated to Hebrew character names)
      *       
      *       Where:
      *       SC - <START-CHAPTER>
      *       SV - <START-VERSE>
      *       SW - <START-WORD#>
      *       EC - <END-CHAPTER>
      *       EV - <END-VERSE>
      *       EW - <END-WORD>
      *========================
       IDENTIFICATION DIVISION.
      *========================
       PROGRAM-ID. CBLGEN.
       AUTHOR. Michal Blaszak.
       DATE-WRITTEN. 2020-06-22.

      *========================
       ENVIRONMENT DIVISION.
      *========================
       CONFIGURATION SECTION.
      *------------------------
      * SOURCE-COMPUTER.
      *     IBM-SYSTEM WITH DEBUGGING MODE.
      *------------------------
       INPUT-OUTPUT SECTION.
      *------------------------
       FILE-CONTROL.
           SELECT GENESIS-DS ASSIGN TO GENDD
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ECODE-I.

      *========================
       DATA DIVISION.
      *========================
       FILE SECTION.
      *------------------------
       FD  GENESIS-DS
           RECORD IS VARYING DEPENDING ON ROW-LENGTH
           RECORDING MODE IS V.
       01  IN-REC.
           05 CHAPTER   PIC X(3).
           05 VERSE     PIC X(3).
           05 TEXT-LINE PIC X(260).
      *------------------------
       WORKING-STORAGE SECTION.
      *------------------------
       77  ROW-LENGTH PIC 9(5) COMP.

       01  FILE-STATUS.
           05 ECODE-I PIC XX.
              88 IN-OK VALUE "00".
           05 READ-STATUS PIC X VALUE "N".
              88 LAST-REC     VALUE "Y".
              88 NOT-LAST-REC VALUE "N".

      * This structure is the way to convert a one-byte charater to its
      * numeric value.
      * The character is first moved to A-2 and then the entire value 
      * is taken from CHAR-CODE. The reason is that BINARY is 2-bytes
      * while single character is one-byte.
       01  CHAR-2-BYTES.
           05 A-1 PIC X VALUE X'00'.
           05 A-2 PIC X.

       01  CHAR-CODE    REDEFINES CHAR-2-BYTES PIC 999 USAGE BINARY.

      * This structure contains the complete list of words from the 
      * entire text.
      * The toal number of rows 1300000 is just a prediction. It may 
      * need to be adjusted depending on the size of text.
       01  GENESIS-NUMBERED.
           05 WORDS-COUNT PIC 9(7) USAGE BINARY VALUE 0.
           05 WORDS-TABLE OCCURS  0 TO 1300000 DEPENDING ON WORDS-COUNT.
              10 CHAPTER     PIC X(3).
              10 VERSE       PIC X(3).
              10 WORD-NO     PIC 9(4).
              10 WORD-VALUE  PIC 99999 USAGE BINARY.
              10 WORD-TEXT   PIC X(50) VALUE ALL SPACES.
              10 WORD-LENGTH PIC 9999 USAGE BINARY.

      * Loop iterators
       *> Character counter in PROCESS-LINE procedure
       01  CHAR-I          PIC 9999 USAGE BINARY.
       *> Word counter in PRINT-WORDS procedure
       01  WORD-PRINT-I    PIC 9(7) USAGE BINARY.

      * Word counter in a current line
       01  WORD-NO-TMP     PIC 9999 USAGE BINARY VALUE 0.

      * Helper variables in PRINT-REPORT
       *> Current pointer of the start word 
       01  START-WORD     PIC 9(7) USAGE BINARY.
       *> Current pointer of the end word
       01  END-WORD       PIC 9(7) USAGE BINARY.
       *> Current value of words between START-WORD and END-WORD
       01  WORD-VALUE-TMP PIC 9999 USAGE BINARY.

      * The characters of the current word (second byte from the UTF-8)
       01  WORD-TEXT-TMP  PIC X(50) VALUE ALL SPACES.
       *> It's easier to copy single bytes to the table to form a string
       01  WORD-TEXT-TABLE-TMP REDEFINES WORD-TEXT-TMP.
           05 WORD-TEXT-CHARS OCCURS 50 TIMES PIC X.

      * In PROCESS-LINE used to count characters in a word
      * In PRINT-wORDS used as a character iterator
       01  CHAR-IDX-TMP   PIC 9999 USAGE BINARY.

      * Control flags in PRINT-REPORT
       *> Is the START-WORD pointing the end of words table?
       01  START-DONE PIC X VALUE 'N'.
           88 IS-START-DONE VALUE 'Y' WHEN SET TO FALSE IS 'N'.

       *> Is the END-WORD pointing the end of words table?
       01  END-DONE   PIC X VALUE 'N'.
           88 IS-END-DONE VALUE 'Y' WHEN SET TO FALSE IS 'N'.

       *> Exit loop flag in the PRINT-REPORT procedure.
       01  DONE PIC X VALUE 'N'.
           88 IS-DONE VALUE 'Y' WHEN SET TO FALSE IS 'N'.

      * Definition of the Hebrew alphabet.
      * It's a table of structures:
      *  UTF-8 character code (the less significat byte)
      *  The character name
      *  The character numerical value
       01  ALPHABET-DATA.
           05 FILLER PIC 999  USAGE BINARY VALUE 144. *> x'90'
           05 FILLER PIC X(6)              VALUE 'alef'.
           05 FILLER PIC 999  USAGE BINARY VALUE 001.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 145.
           05 FILLER PIC X(6)              VALUE 'bet'.
           05 FILLER PIC 999  USAGE BINARY VALUE 002.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 146.
           05 FILLER PIC X(6)              VALUE 'gimel'.
           05 FILLER PIC 999  USAGE BINARY VALUE 003.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 147.
           05 FILLER PIC X(6)              VALUE 'dalet'.
           05 FILLER PIC 999  USAGE BINARY VALUE 004.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 148.
           05 FILLER PIC X(6)              VALUE 'he'.
           05 FILLER PIC 999  USAGE BINARY VALUE 005.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 149.
           05 FILLER PIC X(6)              VALUE 'waw'.
           05 FILLER PIC 999  USAGE BINARY VALUE 006.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 150.
           05 FILLER PIC X(6)              VALUE 'zajin'.
           05 FILLER PIC 999  USAGE BINARY VALUE 007.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 151.
           05 FILLER PIC X(6)              VALUE 'chet'.
           05 FILLER PIC 999  USAGE BINARY VALUE 008.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 152.
           05 FILLER PIC X(6)              VALUE 'tet'.
           05 FILLER PIC 999  USAGE BINARY VALUE 009.

           05 FILLER PIC 999  USAGE BINARY VALUE 153.
           05 FILLER PIC X(6)              VALUE 'jod'.
           05 FILLER PIC 999  USAGE BINARY VALUE 010.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 154.
           05 FILLER PIC X(6)              VALUE 'kaf'.
           05 FILLER PIC 999  USAGE BINARY VALUE 020.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 155.
           05 FILLER PIC X(6)              VALUE 'kaf'.
           05 FILLER PIC 999  USAGE BINARY VALUE 020.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 156.
           05 FILLER PIC X(6)              VALUE 'lamed'.
           05 FILLER PIC 999  USAGE BINARY VALUE 030.

           05 FILLER PIC 999  USAGE BINARY VALUE 157.
           05 FILLER PIC X(6)              VALUE 'mem'.
           05 FILLER PIC 999  USAGE BINARY VALUE 040.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 158.
           05 FILLER PIC X(6)              VALUE 'mem'.
           05 FILLER PIC 999  USAGE BINARY VALUE 040.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 159.
           05 FILLER PIC X(6)              VALUE 'nun'.
           05 FILLER PIC 999  USAGE BINARY VALUE 050.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 160.
           05 FILLER PIC X(6)              VALUE 'nun'.
           05 FILLER PIC 999  USAGE BINARY VALUE 050.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 161.
           05 FILLER PIC X(6)              VALUE 'samech'.
           05 FILLER PIC 999  USAGE BINARY VALUE 060.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 162.
           05 FILLER PIC X(6)              VALUE 'ajin'.
           05 FILLER PIC 999  USAGE BINARY VALUE 070.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 163.
           05 FILLER PIC X(6)              VALUE 'pe'.
           05 FILLER PIC 999  USAGE BINARY VALUE 080.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 164.
           05 FILLER PIC X(6)              VALUE 'pe'.
           05 FILLER PIC 999  USAGE BINARY VALUE 080.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 165.
           05 FILLER PIC X(6)              VALUE 'cadi'.
           05 FILLER PIC 999  USAGE BINARY VALUE 090.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 166.
           05 FILLER PIC X(6)              VALUE 'cadi'.
           05 FILLER PIC 999  USAGE BINARY VALUE 090.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 167.
           05 FILLER PIC X(6)              VALUE 'kof'.
           05 FILLER PIC 999  USAGE BINARY VALUE 100.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 168.
           05 FILLER PIC X(6)              VALUE 'resz'.
           05 FILLER PIC 999  USAGE BINARY VALUE 200.

           05 FILLER PIC 999  USAGE BINARY VALUE 169.
           05 FILLER PIC X(6)              VALUE 'szin'.
           05 FILLER PIC 999  USAGE BINARY VALUE 300.
           *>
           05 FILLER PIC 999  USAGE BINARY VALUE 170.
           05 FILLER PIC X(6)              VALUE 'taw'.
           05 FILLER PIC 999  USAGE BINARY VALUE 400.

      * The table representation of the alfabet definition
       01  ALPHABET-MAPPING REDEFINES ALPHABET-DATA.
           05 ALFABET-VALUES OCCURS 27 TIMES.
              10 UTF-8-CODE PIC 999 USAGE BINARY.
              10 CHAR-NAME  PIC X(6).
              10 CHAR-VALUE PIC 999 USAGE BINARY.

      * The variable to parametrize the searched value of words
       01  NAME-NUMBER PIC 9999 USAGE BINARY VALUE 666.

      *========================
       PROCEDURE DIVISION.
      *========================
       BEGIN.

           OPEN INPUT GENESIS-DS

           IF NOT IN-OK THEN
              DISPLAY "Error opening an input data set (GENESIS)"
                  ECODE-I
              GOBACK
           END-IF

           DISPLAY "Reading data ..."

           *> Build the table of words
           PERFORM READ-LINE
           PERFORM UNTIL LAST-REC
              PERFORM PROCESS-LINE
              PERFORM READ-LINE
           END-PERFORM

           *> Generate the report for individual target values

           MOVE 666 TO NAME-NUMBER
           PERFORM PRINT-REPORT

           MOVE 616 TO NAME-NUMBER
           PERFORM PRINT-REPORT

           MOVE 646 TO NAME-NUMBER
           PERFORM PRINT-REPORT

           MOVE 665 TO NAME-NUMBER
           PERFORM PRINT-REPORT

           CLOSE GENESIS-DS

           GOBACK.

       END-BEGIN.
           EXIT.
      *
       READ-LINE.
           READ GENESIS-DS AT END MOVE "Y" TO READ-STATUS
           END-READ.
       END-READ-LINE.
           EXIT.
      *-----------------------------------------------------------------
      * Converts a single line (record from the input data set) into a 
      * list of words. Words are added to the end of the common words
      * table.
      * Input:
      *    IN-REC - contains data read from the input DS
      * Output:
      *    GENESIS-NUMBERED - the table containing all words
      *-----------------------------------------------------------------
       PROCESS-LINE.
           MOVE 0 TO WORD-VALUE-TMP
           MOVE 0 TO WORD-NO-TMP
           MOVE 0 TO CHAR-IDX-TMP
           INITIALIZE WORD-TEXT-TMP.

           PERFORM VARYING CHAR-I 
              FROM 1 BY 1 UNTIL CHAR-I > ROW-LENGTH - 6
              
              IF TEXT-LINE(CHAR-I:1) = X'20' THEN
                 ADD 1 TO WORDS-COUNT OF GENESIS-NUMBERED
                 ADD 1 TO WORD-NO-TMP

                 MOVE WORD-VALUE-TMP TO 
                      WORD-VALUE OF GENESIS-NUMBERED(WORDS-COUNT)
                 MOVE CHAPTER OF IN-REC TO 
                      CHAPTER OF GENESIS-NUMBERED(WORDS-COUNT)
                 MOVE VERSE OF IN-REC TO 
                      VERSE OF GENESIS-NUMBERED(WORDS-COUNT)
                 MOVE WORD-NO-TMP TO 
                      WORD-NO OF GENESIS-NUMBERED(WORDS-COUNT)
                 MOVE WORD-TEXT-TMP TO 
                      WORD-TEXT OF GENESIS-NUMBERED(WORDS-COUNT)
                 MOVE CHAR-IDX-TMP TO 
                      WORD-LENGTH OF GENESIS-NUMBERED(WORDS-COUNT)

                 INITIALIZE WORD-TEXT-TMP
                 MOVE 0 TO CHAR-IDX-TMP
                 MOVE 0 TO WORD-VALUE-TMP
              ELSE 
                 IF TEXT-LINE(CHAR-I:1) IS NOT = X'D7' THEN
                    MOVE TEXT-LINE(CHAR-I:1) TO A-2
                    *> 143 = 144 + 1; 144 = X90 - The 1st character in 
                    *>                            the table
                    ADD CHAR-VALUE (CHAR-CODE - 143) TO WORD-VALUE-TMP

                    ADD 1 TO CHAR-IDX-TMP
                    MOVE TEXT-LINE(CHAR-I:1) TO 
                         WORD-TEXT-CHARS(CHAR-IDX-TMP)
                 END-IF
              END-IF
           END-PERFORM

           *> Add the last word
           IF WORD-VALUE-TMP IS NOT = 0 THEN
              ADD 1 TO WORDS-COUNT OF GENESIS-NUMBERED
              ADD 1 TO WORD-NO-TMP

              MOVE WORD-VALUE-TMP TO 
                   WORD-VALUE OF GENESIS-NUMBERED(WORDS-COUNT)
              MOVE CHAPTER OF IN-REC TO 
                   CHAPTER OF GENESIS-NUMBERED (WORDS-COUNT)
              MOVE VERSE OF IN-REC TO 
                   VERSE OF GENESIS-NUMBERED (WORDS-COUNT)
              MOVE WORD-NO-TMP TO 
                   WORD-NO OF GENESIS-NUMBERED (WORDS-COUNT)
              MOVE WORD-TEXT-TMP TO 
                   WORD-TEXT OF GENESIS-NUMBERED(WORDS-COUNT)
              MOVE CHAR-IDX-TMP TO 
                   WORD-LENGTH OF GENESIS-NUMBERED(WORDS-COUNT)
           END-IF.

       END-PROCESS-LINE.
           EXIT.
      *-----------------------------------------------------------------
      * The actual agorithm looking for the desired value of words.
      * The process starta at the first word. Both START-WORD and 
      * END-WORD pointers point to the first word.
      * The value of the word is added to the WORD-VALUE-TMP variable.
      * If WORD-VALUE-TMP equals to the searched value, then words
      * between START and END are printed and both pointers are moved
      * by one.
      * If the value is less then searched one, then only the END
      * pointer is moved (which increases the value of WORD-VALUE-TMP).
      * If the value is more then searched one, then only he START
      * pointer is moved (which decreases the value of WORD-VALUE-TMP).
      * The process ends after either START or END pointer exceed the 
      * last word in the table.
      *-----------------------------------------------------------------
       PRINT-REPORT.
           DISPLAY 'Generating report for ' NAME-NUMBER ' ...'

           MOVE 0 TO START-WORD
           MOVE 0 TO END-WORD
           MOVE 0 TO WORD-VALUE-TMP
           SET IS-DONE TO FALSE
           SET IS-START-DONE TO FALSE
           SET IS-END-DONE TO FALSE

           PERFORM INCREMENT-START
           PERFORM INCREMENT-END

           PERFORM TEST AFTER UNTIL IS-DONE
              EVALUATE TRUE
                 WHEN WORD-VALUE-TMP = NAME-NUMBER
                    DISPLAY CHAPTER OF GENESIS-NUMBERED(START-WORD)
                            "-"
                            VERSE OF GENESIS-NUMBERED(START-WORD)
                            "-"
                            WORD-NO OF GENESIS-NUMBERED(START-WORD)
                            " : "
                            CHAPTER OF GENESIS-NUMBERED(END-WORD)
                            "-"
                            VERSE OF GENESIS-NUMBERED(END-WORD)
                            "-"
                            WORD-NO OF GENESIS-NUMBERED(END-WORD)

                    PERFORM PRINT-WORDS

                    IF IS-START-DONE AND IS-END-DONE THEN
                       SET IS-DONE TO TRUE
                    ELSE
                       PERFORM INCREMENT-START
                       PERFORM INCREMENT-END
                    END-IF
                 WHEN WORD-VALUE-TMP > NAME-NUMBER
                    IF IS-START-DONE THEN
                       SET IS-DONE TO TRUE
                    ELSE
                       PERFORM INCREMENT-START
                    END-IF
                 WHEN OTHER *> WORD-VALUE-TMP < 666
                    IF IS-END-DONE THEN
                       SET IS-DONE TO TRUE
                    ELSE
                       PERFORM INCREMENT-END
                    END-IF
              END-EVALUATE
           END-PERFORM.

       END-PRINT-REPORT.
           EXIT.
      *-----------------------------------------------------------------
      * Helper procedure called from PRINT-REPORT.
      * Increases the START-WORD pointer and subtracts the value of the
      * word it pointed to previously.
      *-----------------------------------------------------------------
       INCREMENT-START.
           IF WORDS-COUNT OF GENESIS-NUMBERED = 0 OR 
              START-WORD >= WORDS-COUNT OF GENESIS-NUMBERED THEN
                MOVE "Y" TO START-DONE
                DISPLAY "START-DONE"
                EXIT PARAGRAPH
           END-IF

           IF START-WORD <= WORDS-COUNT OF GENESIS-NUMBERED AND
              START-WORD > 0 THEN
                SUBTRACT WORD-VALUE OF GENESIS-NUMBERED(START-WORD) FROM 
                         WORD-VALUE-TMP
           END-IF

           ADD 1 TO START-WORD.

       END-INCREMENT-STAR.
           EXIT.
      *-----------------------------------------------------------------
      * Helper procedure called from PRINT-REPORT.
      * Increases the END-WORD pointer and adds the word value it
      * started pointing to.
      *-----------------------------------------------------------------
       INCREMENT-END.
           IF WORDS-COUNT OF GENESIS-NUMBERED = 0 OR
              END-WORD >= WORDS-COUNT OF GENESIS-NUMBERED THEN
                MOVE "Y" TO END-DONE
                DISPLAY "END-DONE"
                EXIT PARAGRAPH
           END-IF

           ADD 1 TO END-WORD

           IF END-WORD <= WORDS-COUNT OF GENESIS-NUMBERED THEN
              ADD WORD-VALUE OF GENESIS-NUMBERED(END-WORD) TO
                  WORD-VALUE-TMP
           END-IF.
           
       END-INCREMENT-END.
           EXIT.
      *-----------------------------------------------------------------
      * Helper procedure called from PRINT-REPORT.
      * Prints words between START and END pointers.
      * Words are deparated by an empty line.
      * Each word is presented as a list of character names.
      *-----------------------------------------------------------------
       PRINT-WORDS.
           PERFORM VARYING WORD-PRINT-I FROM START-WORD BY 1 UNTIL
              WORD-PRINT-I > END-WORD
                 INITIALIZE WORD-TEXT-TMP
                 MOVE WORD-TEXT OF GENESIS-NUMBERED(WORD-PRINT-I) TO 
                      WORD-TEXT-TMP

                 PERFORM VARYING CHAR-IDX-TMP FROM 1 BY 1 UNTIL 
                    CHAR-IDX-TMP > WORD-LENGTH OF 
                                   GENESIS-NUMBERED(WORD-PRINT-I)
                       MOVE WORD-TEXT-CHARS(CHAR-IDX-TMP) TO A-2
                       DISPLAY CHAR-NAME (CHAR-CODE - 143)
                 END-PERFORM

                 DISPLAY " "
           END-PERFORM.

       END-PRINT-WORDS.
           EXIT.
