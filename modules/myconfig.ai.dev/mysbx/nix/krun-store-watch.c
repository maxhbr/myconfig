// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//
// The store watcher of the krun guests' flake-source poisoning
// instrumentation (doc/TODOs/krun-guest-flake-source-symlink.md,
// bd myconfig-yiw): an inotify watch on /nix/store that logs every
// new *-source entry with its type (dir vs symlink) and the symlink
// target when it is one. The nix wrappers start one instance in the
// background per VM (best-effort, never blocking); every line goes
// to the log file given as argv[2].
//
// Fail-open by construction: inotify_init/add failures, an unwritable
// log or a read error terminate the watcher (a stderr line, exit 1)
// — nix itself never depends on it. A single-instance lock (flock
// on argv[3], held for the watcher's lifetime) makes the wrapper's
// startup idempotent: a second instance exits 0 immediately.

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/inotify.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#define BUF_SIZE (1 << 16)

static int is_source_name(const char *name)
{
    size_t len = strlen(name);
    return len > 7 && strcmp(name + len - 7, "-source") == 0;
}

static void log_line(FILE *log, const char *fmt, ...) __attribute__((format(printf, 2, 3)));

static void log_line(FILE *log, const char *fmt, ...)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    va_list ap;
    va_start(ap, fmt);
    fprintf(log, "%ld.%03ld ", ts.tv_sec, ts.tv_nsec / 1000000);
    vfprintf(log, fmt, ap);
    va_end(ap);
    fputc('\n', log);
    fflush(log);
}

int main(int argc, char **argv)
{
    if (argc != 4) {
        fprintf(stderr, "usage: %s STORE_DIR LOG_FILE LOCK_FILE\n", argv[0]);
        return 2;
    }

    // Single instance: a non-blocking flock on the lock file, held
    // for this process's lifetime. EWOULDBLOCK means another watcher
    // already runs — a quiet exit, not an error.
    int lockfd = open(argv[3], O_CREAT | O_RDWR, 0600);
    if (lockfd < 0) {
        fprintf(stderr, "store-watch: cannot open %s: %s\n", argv[3], strerror(errno));
        return 1;
    }
    if (flock(lockfd, LOCK_EX | LOCK_NB) != 0) {
        close(lockfd);
        return 0;
    }

    int fd = inotify_init1(IN_CLOEXEC);
    if (fd < 0) {
        fprintf(stderr, "store-watch: inotify_init: %s\n", strerror(errno));
        return 1;
    }
    if (inotify_add_watch(fd, argv[1], IN_CREATE | IN_MOVED_TO) < 0) {
        fprintf(stderr, "store-watch: cannot watch %s: %s\n", argv[1], strerror(errno));
        close(fd);
        return 1;
    }

    FILE *log = fopen(argv[2], "a");
    if (log == NULL) {
        fprintf(stderr, "store-watch: cannot open %s: %s\n", argv[2], strerror(errno));
        close(fd);
        return 1;
    }

    char buf[BUF_SIZE] __attribute__((aligned(8)));
    for (;;) {
        ssize_t n = read(fd, buf, sizeof(buf));
        if (n < 0) {
            if (errno == EINTR)
                continue;
            fprintf(stderr, "store-watch: read: %s\n", strerror(errno));
            break;
        }
        if (n == 0)
            break;
        for (char *p = buf; p < buf + n;) {
            struct inotify_event *ev = (struct inotify_event *)p;
            p += sizeof(*ev) + ev->len;
            if (!(ev->mask & (IN_CREATE | IN_MOVED_TO)))
                continue;
            if (ev->len == 0 || ev->name[0] == '\0')
                continue;
            if (!is_source_name(ev->name))
                continue;
            char path[PATH_MAX];
            if (snprintf(path, sizeof(path), "%s/%s", argv[1], ev->name) >= (int)sizeof(path))
                continue;
            struct stat st;
            if (lstat(path, &st) != 0) {
                log_line(log, "MISSING %s (%s)", ev->name, strerror(errno));
                continue;
            }
            if (S_ISLNK(st.st_mode)) {
                char target[PATH_MAX];
                ssize_t t = readlink(path, target, sizeof(target) - 1);
                if (t >= 0) {
                    target[t] = '\0';
                    log_line(log, "SYMLINK %s -> %s", ev->name, target);
                } else {
                    log_line(log, "SYMLINK %s -> <unreadable: %s>", ev->name, strerror(errno));
                }
            } else if (S_ISDIR(st.st_mode)) {
                log_line(log, "DIR %s", ev->name);
            } else {
                log_line(log, "OTHER %s", ev->name);
            }
        }
    }
    fclose(log);
    close(fd);
    close(lockfd);
    return 0;
}
