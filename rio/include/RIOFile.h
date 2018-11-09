#ifndef RIOFILE_H

#define RIOFILE_H

#include "stdio.h"
#include "fcntl.h"
#include "unistd.h"

int __rio_fsync(int fd);

int __rio_renameat(int olddirfd, char *oldpath, int newdirdf, char *newpath);

int __rio_openat(int dirfd, char *file, int flags, mode_t mode);

int __rio_o_rdonly(void);

int __rio_o_wronly(void);

int __rio_o_rdwr(void);

int __rio_o_append(void);

int __rio_o_creat(void);

int __rio_o_noctty(void);

#endif /* end RIOFILE_H */
