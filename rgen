#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <string.h>

struct Character {
    unsigned char hex[2];
    const char* name;
    unsigned int value;
};

struct Character alphabet[27] = {
    { {0xd7, 0x90}, "alef"  , 1},
    { {0xd7, 0x91}, "bet"   , 2},
    { {0xd7, 0x92}, "gimel" , 3},
    { {0xd7, 0x93}, "dalet" , 4},
    { {0xd7, 0x94}, "he"    , 5},
    { {0xd7, 0x95}, "waw"   , 6},
    { {0xd7, 0x96}, "zajin" , 7},
    { {0xd7, 0x97}, "chet"  , 8},
    { {0xd7, 0x98}, "tet"   , 9},
    { {0xd7, 0x99}, "jod"   , 10},
    { {0xd7, 0x9a}, "kaf"   , 20},
    { {0xd7, 0x9b}, "kaf"   , 20},
    { {0xd7, 0x9c}, "lamed" , 30},
    { {0xd7, 0x9d}, "mem"   , 40},
    { {0xd7, 0x9e}, "mem"   , 40},
    { {0xd7, 0x9f}, "nun"   , 50},
    { {0xd7, 0xa0}, "nun"   , 50},
    { {0xd7, 0xa1}, "samech", 60},
    { {0xd7, 0xa2}, "ajin"  , 70},
    { {0xd7, 0xa3}, "pe"    , 80},
    { {0xd7, 0xa4}, "pe"    , 80},
    { {0xd7, 0xa5}, "cadi"  , 90},
    { {0xd7, 0xa6}, "cadi"  , 90},
    { {0xd7, 0xa7}, "kof"   , 100},
    { {0xd7, 0xa8}, "resz"  , 200},
    { {0xd7, 0xa9}, "szin"  , 300},
    { {0xd7, 0xaa}, "taw"   , 400}
};

#define LINEBUF_SIZE 100
#define NEWLINE 0x0a

unsigned char ascii2ebcdic(unsigned char c) {
    switch(c) {
        case 0x20: return 0x40;
        case 0x30: return 0xF0;
        case 0x31: return 0xF1;
        case 0x32: return 0xF2;
        case 0x33: return 0xF3;
        case 0x34: return 0xF4;
        case 0x35: return 0xF5;
        case 0x36: return 0xF6;
        case 0x37: return 0xF7;
        case 0x38: return 0xF8;
        case 0x39: return 0xF9;
        default: return 0;
    }
}

int max_len = 0; /* The max lengt of the text line - needed to properly */
                 /* allocate the output data set.                       */

#define MAX(a,b) ((a) > (b) ? (a) : (b))

/****************************************************************************/
/* print_line                                                               */
/*--------------------------------------------------------------------------*/
/* s   - the string buffer to be printed                                    */
/* len - the length of the buffer in bytes                                  */
/*--------------------------------------------------------------------------*/
/* The function prints the content of the 's' buffer:                       */
/* - First 6 bytes contain the chapter and the verse numbers which are      */
/*   stored in a readable way.                                              */
/* - The reminder part contains the text coded in UTF-8. It will be printed */
/*   in HEX format.                                                         */
/* - The last byte is a new line character. It will be printed separately.  */
/****************************************************************************/
void print_line(char* s, int len) {
    int i; /* the loop counter */

    /* Calculate the longest line in entire text */
    max_len = MAX(max_len, len);

    /* Print the header: 'chapter' and 'verse' */
    printf("[%.3s:%.3s] ", s, s+3);

    /* len+6 to skip header (chapter and verse), handled above */
    /* len-1 to skip finishing new line character which        */
    /* shouldn't be printed in HEX.                            */
    for(i=6; i<len-1; i++)
        printf("%02X", s[i]);

    putchar('\n');
}

/****************************************************************************/
/* process_line                                                             */
/*--------------------------------------------------------------------------*/
/* s   - the string buffer to print.                                        */
/* len - the length of the 's' buffer.                                      */
/* fo  - the output data set file descriptor.                               */
/*--------------------------------------------------------------------------*/
/* The function prints the line of text in HEX mode.                        */
/* The same line is copied to the output data set.                          */
/* if 's' is NULL the function does nothing.                                */
/****************************************************************************/

void process_line(char* s, int len, FILE* fo) {
    int i; /* The index of the current character in the 's' input buffer     */
    char* out_buf = NULL; /* The output buffer to be written to the output   */
                          /* data set. It contains the ntire record finished */
                          /* by the new line character                       */
    int out_buf_len = 0;  /* The length of the output buffer */
    int text_len = 0; /* The length of the text part in the 's' buffer */
    int text_pos = 0; /* The position of the text block (not index) in the */
                      /* 's' input buffer.                                 */

    if (s != NULL) {
        int in_chapter = 1; /* 1 - we are in the 'chapter' part of the index */
                            /* 0 - we are in the 'verse' part of the index   */
        char chapter[3] = {' ',' ',' '}; /* the chapter # of the line */
        char verse[3] = {' ',' ',' '}; /* the lines verse # in the chapter */
        int idx_pos = 0; /* Counts the current character in either */
                         /* 'chapter' or 'verse'.                  */

        /* The following loop takes these goals:                            */
        /* - Identify the chapter number and store it in 'chapter' variable.*/
        /* - Identify the verse number and store it in 'verse' variable.    */
        /* - Identify the first text character in the input 's' buffer.     */

        /* First 3 bytes contain formatting Right-To-Left */
        for(i=3; i<len; i++) {
            unsigned char ch = (unsigned char)s[i];

            if (ch == 0x20) { /* ' ' */
                continue; /* Just skip blanks */
            }

            /* With the first UTF-8 code we leave the 'index' part and */
            /* enther the 'text' part.                                 */
            if (ch != 0xd7) { /* We are still in the 'index' part */
                if (in_chapter) {
                    /* 0x3A = ':' - leave 'chapter' and enter 'verse' */
                    if (ch == 0x3A) {
                        in_chapter = 0;
                        idx_pos = 0;
                    } else {
                        chapter[idx_pos++] = ascii2ebcdic(ch);
                    }
                } else {
                    verse[idx_pos++] = ascii2ebcdic(ch);
                }
            } else { /* This is to exit the 'index'part */
                /* The total legth of the line */
                text_len = len-i;

                text_pos = i;

                /* The first text character found, exit the loop */
                break;
            }
        }

        /* Writting to the output file set */

        /* +6 for the index part, +1 for a new line */
        out_buf_len = 6 + text_len + 1;
        out_buf = (char*) malloc(out_buf_len);

        memcpy(out_buf, chapter, 3);
        memcpy(out_buf+3, verse, 3);
        /* -6 - index part already filled, -1 - new line */
        memcpy(out_buf+6, s+text_pos, out_buf_len-6-1);
        out_buf[out_buf_len-1] = 0x15; /* The EBCDIC new line - end of record */

        fwrite(out_buf, sizeof(char), out_buf_len, fo);

        /* Print this line to the console */
        print_line(out_buf, out_buf_len);

        free(out_buf);
    }
}

/* These variables are declared as global as their content has to persist  */
/* over several calls of 'process_buff' function unless the entire line is */
/* processsed and are used in two places ('process_buff' and 'main').      */
char* out_line = NULL; /* Temporary (dynamically allocated) line buffer */
int out_len = 0;       /* The length of the 'out_line' buffer. */

/****************************************************************************/
/* process_buff                                                             */
/*--------------------------------------------------------------------------*/
/* buf - the read buffer containing the UTH-8 string read from the input    */
/*       data set.                                                          */
/* len - the number of bytes int 'buf'.                                     */
/* pos - the current position in 'buf'.                                     */
/* fo  - the output data set file descriptor.                               */
/*                                                                          */
/* returns - the position in the read buffer 'buff' for the next            */
/*           unprocessed character, pointing to the next line.              */
/*           If no new line was detected in the read buffer, the function   */
/*           retunrs the position pointing to the position one after the    */
/*           last character which means 'read the next buffer and continue  */
/*           looking for the new line.                                      */
/*--------------------------------------------------------------------------*/
/* During the processing all chacarters belonging to the current line are   */
/* copied to 'out_line' temporary buffer. The length of 'out_line' is       */
/* stored in 'out_len' global variable.                                     */
/****************************************************************************/

int process_buff(char* buf, int len, int pos, FILE* fo) {
  int i;
  for (i=pos; i<len; i++) {
     if (buf[i] == NEWLINE) {
        out_line = (char*)realloc(out_line, out_len + i - pos);
        memcpy(out_line+out_len, buf+pos, i-pos);
        out_len += i-pos;

        /* Print the line and write it to the output data set. */
        process_line(out_line, out_len, fo);

        free(out_line);  /* The line has been processed so the line buffer */
        out_line = NULL; /* can be freed/nullified.                        */
        out_len = 0;

        return i+1;
     }
  }

  out_line = (char*)realloc(out_line, out_len + i - pos);
  memcpy(out_line+out_len, buf+pos, i-pos);
  out_len += i-pos;

  return i;
}

int main(void) {
    char line_buf[LINEBUF_SIZE];
    FILE *fi, *fo;
    int n = 0;
    int buf_pos = 0; /* current position in the read buffer */

    fi = fopen("DD:FILEIN", "rb");
    fo = fopen("DD:FILEOUT", "w, lrecl=270, recfm=VB");

    if (fi == NULL) {
        perror("Failed to open input data set.\n");
        return EXIT_FAILURE;
    }

    if (fo == NULL) {
        perror("Failed to open the targed dataset.\n");
        return EXIT_FAILURE;
    }

    /* Process the input data set by reading some bytes into the buffer    */
    /* For each read process the contents of the buffer                    */
    /* if the NewLine was encountered then this new line is processed and  */
    /* the current buffer position (buf_pos) is returned. Then the rest of */
    /* the cuffer is processd to find another line.                        */
    /* If no new line found in a buffer then the next read happens.        */
    /* And so on ...                                                       */
    do {
       n = fread(line_buf, sizeof(char), LINEBUF_SIZE, fi);

        if (n != LINEBUF_SIZE && ferror(fi)) {
            printf( "Error reading the file.\n" );
            break;
        }

        buf_pos = 0;

        while(buf_pos < n) {
            buf_pos = process_buff(line_buf, n, buf_pos, fo);
        }

        if (feof(fi)) {
            break;
        }
    } while(1); /* There are two 'break' points inside the loop */

    process_line(out_line, out_len, fo);

    printf("Max len: %d\n", max_len); /* +6 for the index part */

    fclose(fi);
    fclose(fo);
    free(out_line);
}
