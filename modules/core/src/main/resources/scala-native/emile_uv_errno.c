/*
 * Exposes libuv error code constants as functions callable from Scala Native.
 *
 * libuv error codes are compile-time macros that expand to platform-specific
 * values (negated POSIX errno on Unix, fixed constants on Windows). Scala
 * Native cannot evaluate C macros, so we expose them as functions.
 */
#if !defined(_WIN32)
/* Ensure POSIX thread types (pthread_rwlock_t) are available for uv.h */
#if !defined(_POSIX_C_SOURCE) || _POSIX_C_SOURCE < 200112L
#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200112L
#endif
#endif
#include <uv.h>

int emile_uv_eof(void)               { return UV_EOF; }
int emile_uv_ecanceled(void)         { return UV_ECANCELED; }
int emile_uv_econnrefused(void)      { return UV_ECONNREFUSED; }
int emile_uv_econnreset(void)        { return UV_ECONNRESET; }
int emile_uv_eaddrinuse(void)        { return UV_EADDRINUSE; }
int emile_uv_eaddrnotavail(void)     { return UV_EADDRNOTAVAIL; }
int emile_uv_etimedout(void)         { return UV_ETIMEDOUT; }
int emile_uv_einval(void)            { return UV_EINVAL; }
int emile_uv_ebadf(void)             { return UV_EBADF; }
int emile_uv_eacces(void)            { return UV_EACCES; }
int emile_uv_enetunreach(void)       { return UV_ENETUNREACH; }
int emile_uv_ehostunreach(void)      { return UV_EHOSTUNREACH; }
int emile_uv_epipe(void)             { return UV_EPIPE; }
int emile_uv_eagain(void)            { return UV_EAGAIN; }
int emile_uv_eisconn(void)           { return UV_EISCONN; }
int emile_uv_enotconn(void)          { return UV_ENOTCONN; }
int emile_uv_econnaborted(void)      { return UV_ECONNABORTED; }
int emile_uv_enomem(void)            { return UV_ENOMEM; }
int emile_uv_ebusy(void)             { return UV_EBUSY; }
int emile_uv_enosys(void)            { return UV_ENOSYS; }
int emile_uv_enotsup(void)           { return UV_ENOTSUP; }

/* Read the signum field from a uv_signal_t handle. */
int emile_uv_signal_signum(uv_signal_t* handle) { return handle->signum; }
