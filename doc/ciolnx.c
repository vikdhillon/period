/* This set of routines handles the basic terminal IO on a Sun
 * system.  These routines are all designed to be called from
 * Fortran.  Thus TTINIT can be called using the Fortran line
 *      CALL TTINIT()
 */

#include <termios.h>

static struct termios term, saveterm;

ttinit_()

/* Turn off echo, and canonical processing.  This means that most
 * things that the user types is passed into the calling program.
 * Uses POSIX terminal control.
 */
{
   if ( isatty(0) ) {
      int ier;

      ier = tcgetattr(0, &term);
      saveterm = term;

      if (ier==0) {
         term.c_iflag &= ~( ICRNL );
         term.c_lflag &= ~( ICANON );
         term.c_lflag &= ~( ECHO | ECHOK | ECHOE | ECHONL );
         term.c_cc[VMIN] = 1;
         tcsetattr(0, TCSADRAIN, &term);
      }
   }
   return 0;
}

ttrset_()

/* Reset the terminal flags to state when CSTTY was last called.
 */
{
   if ( isatty(0) ) {
      tcsetattr(0, TCSADRAIN, &saveterm);
   }
   return 0;
}

rdchr_(chr_ptr, chr_len)
char *chr_ptr;
int   chr_len;

/* Reads a single byte from the current terminal.  The read waits
 * until the user types something.
 *
 * chr_ptr   In/Ret  The character read
 * chr_len   Input   The Fortran size of the character array
 */
{
   return (read(0,chr_ptr,1));
}

cwrite_(chr_ptr, chr_len)
char *chr_ptr;
int   chr_len;

/* Write a single character to the terminal.
 *
 * chr_ptr   Input   The character to be written
 * chr_len   Input   The Fortran size of the character array
 */
{
   return (write(0,chr_ptr,chr_len));
}

cpgsze_(irow_ptr, icol_ptr)
int  *irow_ptr;
int  *icol_ptr;

/* Return the size of the current window.
 *
 * irow_ptr  Return  The number of rows in current window
 * icol_ptr  Return  The number of columns in current window
 */

{
#include <sys/ioctl.h>
   struct winsize actsize;

   ioctl(0, TIOCGWINSZ, &actsize);
   *irow_ptr=actsize.ws_row;
   *icol_ptr=actsize.ws_col;
   return 0;
}
