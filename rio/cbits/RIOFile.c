#include "RIOFile.h"

int __rio_fsync(int fd) {
  return fsync(fd);
}

int __rio_renameat(int olddirfd, char *oldpath, int newdirfd, char *newpath) {
  return renameat(olddirfd, oldpath, newdirfd, newpath);
}

int __rio_openat(int dirfd, char *file, int flags, mode_t mode) {
  return openat(dirfd, file, flags, mode);
}

int __rio_o_rdonly(void)
{
#if defined(O_RDONLY)
  return O_RDONLY;
#else
  return 0;
#endif
}

int __rio_o_wronly(void)
{
#if defined(O_WRONLY)
  return O_WRONLY;
#else
  return 0;
#endif
}

int __rio_o_rdwr(void)
{
#if defined(O_RDWR)
  return O_RDWR;
#else
  return 0;
#endif
}

int __rio_o_append(void)
{
#if defined(O_APPEND)
  return O_APPEND;
#else
  return 0;
#endif
}

int __rio_o_creat(void)
{
#if defined(O_CREAT)
  return O_CREAT;
#else
  return 0;
#endif
}

int __rio_o_noctty(void)
{
#if defined(O_NOCTTY)
  return O_NOCTTY;
#else
  return 0;
#endif
}
