/*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*/

/* Symlink-refusing filesystem primitives.

   Every operation is relative to an already-open directory descriptor and
   refuses to follow symbolic links where the flag exists, which neither the
   OCaml Unix library nor portable Eio can express. See nofollow.mli for the
   OCaml-side contract. */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

CAMLprim value caml_nofollow_open_dir(value path)
{
  CAMLparam1(path);
  char *p = caml_stat_strdup(String_val(path));
  int fd, err;
  caml_release_runtime_system();
  fd = open(p, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
  err = errno;
  caml_acquire_runtime_system();
  caml_stat_free(p);
  if (fd == -1) unix_error(err, "open", path);
  CAMLreturn(Val_int(fd));
}

CAMLprim value caml_nofollow_openat_dir(value dirfd, value name)
{
  CAMLparam2(dirfd, name);
  char *n = caml_stat_strdup(String_val(name));
  int fd, err;
  caml_release_runtime_system();
  fd = openat(Int_val(dirfd), n,
              O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
  err = errno;
  caml_acquire_runtime_system();
  caml_stat_free(n);
  if (fd == -1) unix_error(err, "openat", name);
  CAMLreturn(Val_int(fd));
}

CAMLprim value caml_nofollow_openat_read(value dirfd, value name)
{
  CAMLparam2(dirfd, name);
  char *n = caml_stat_strdup(String_val(name));
  int fd, err;
  caml_release_runtime_system();
  /* O_NONBLOCK so a FIFO swapped in as the target cannot block the open;
     regular-file reads are unaffected. */
  fd = openat(Int_val(dirfd), n,
              O_RDONLY | O_NOFOLLOW | O_NONBLOCK | O_CLOEXEC);
  err = errno;
  caml_acquire_runtime_system();
  caml_stat_free(n);
  if (fd == -1) unix_error(err, "openat", name);
  CAMLreturn(Val_int(fd));
}

CAMLprim value caml_nofollow_fstatat(value dirfd, value name)
{
  CAMLparam2(dirfd, name);
  CAMLlocal1(res);
  char *n = caml_stat_strdup(String_val(name));
  struct stat st;
  int ret, err, kind;
  caml_release_runtime_system();
  ret = fstatat(Int_val(dirfd), n, &st, AT_SYMLINK_NOFOLLOW);
  err = errno;
  caml_acquire_runtime_system();
  caml_stat_free(n);
  if (ret == -1) unix_error(err, "fstatat", name);
  kind = S_ISREG(st.st_mode)   ? 0
         : S_ISDIR(st.st_mode) ? 1
         : S_ISLNK(st.st_mode) ? 2
                               : 3;
  res = caml_alloc_tuple(3);
  Store_field(res, 0, Val_int(kind));
  Store_field(res, 1, Val_int(st.st_mode & 07777));
  Store_field(res, 2, caml_copy_int64((int64_t)st.st_size));
  CAMLreturn(res);
}

CAMLprim value caml_nofollow_openat_create(value dirfd, value name, value perm)
{
  CAMLparam3(dirfd, name, perm);
  char *n = caml_stat_strdup(String_val(name));
  int fd, err;
  caml_release_runtime_system();
  fd = openat(Int_val(dirfd), n,
              O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC,
              Int_val(perm));
  err = errno;
  caml_acquire_runtime_system();
  caml_stat_free(n);
  if (fd == -1) unix_error(err, "openat", name);
  CAMLreturn(Val_int(fd));
}

CAMLprim value caml_nofollow_mkdirat(value dirfd, value name, value perm)
{
  CAMLparam3(dirfd, name, perm);
  char *n = caml_stat_strdup(String_val(name));
  int ret, err;
  caml_release_runtime_system();
  ret = mkdirat(Int_val(dirfd), n, Int_val(perm));
  err = errno;
  caml_acquire_runtime_system();
  caml_stat_free(n);
  if (ret == -1) unix_error(err, "mkdirat", name);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_nofollow_unlinkat(value dirfd, value name, value dir)
{
  CAMLparam3(dirfd, name, dir);
  char *n = caml_stat_strdup(String_val(name));
  int ret, err;
  caml_release_runtime_system();
  ret = unlinkat(Int_val(dirfd), n, Bool_val(dir) ? AT_REMOVEDIR : 0);
  err = errno;
  caml_acquire_runtime_system();
  caml_stat_free(n);
  if (ret == -1) unix_error(err, "unlinkat", name);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_nofollow_renameat(value dirfd, value old_name, value new_name)
{
  CAMLparam3(dirfd, old_name, new_name);
  char *o = caml_stat_strdup(String_val(old_name));
  char *n = caml_stat_strdup(String_val(new_name));
  int ret, err;
  caml_release_runtime_system();
  ret = renameat(Int_val(dirfd), o, Int_val(dirfd), n);
  err = errno;
  caml_acquire_runtime_system();
  caml_stat_free(o);
  caml_stat_free(n);
  if (ret == -1) unix_error(err, "renameat", new_name);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_nofollow_linkat(value dirfd, value old_name, value new_name)
{
  CAMLparam3(dirfd, old_name, new_name);
  char *o = caml_stat_strdup(String_val(old_name));
  char *n = caml_stat_strdup(String_val(new_name));
  int ret, err;
  caml_release_runtime_system();
  ret = linkat(Int_val(dirfd), o, Int_val(dirfd), n, 0);
  err = errno;
  caml_acquire_runtime_system();
  caml_stat_free(o);
  caml_stat_free(n);
  if (ret == -1) unix_error(err, "linkat", new_name);
  CAMLreturn(Val_unit);
}
