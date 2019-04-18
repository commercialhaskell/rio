#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

int __rio_o_tmpfile( void )
{
#ifdef __O_TMPFILE
  return __O_TMPFILE;
#else
  return 0;
#endif
}

int __rio_at_fdcwd( void )
{
  return AT_FDCWD;
}

int __rio_at_symlink_follow( void )
{
  return AT_SYMLINK_FOLLOW;
}


int __rio_s_irusr( void )
{
  return S_IRUSR;
}

int __rio_s_iwusr( void )
{
  return S_IWUSR;
}

