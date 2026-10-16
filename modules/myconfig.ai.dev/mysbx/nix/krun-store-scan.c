// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//
// The bulk directory scanner of the krun guests' db reconcile
// (nix/krun-rootfs.nix's init, nix/krun-guest-nix.nix's setup): one
// getdents pass over the read-only store share, one
// "<d_type> <name>" line per entry. The d_type arrives in the
// readdir stream itself, so classifying ~175k share entries costs
// one bulk scan — a per-path stat of every registered store path
// costs one virtiofs FUSE round trip each instead.

#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "usage: %s DIR\n", argv[0]);
        return 2;
    }
    DIR *dir = opendir(argv[1]);
    if (dir == NULL) {
        fprintf(stderr, "store-scan: cannot open %s: %s\n", argv[1], strerror(errno));
        return 1;
    }
    for (;;) {
        errno = 0;
        struct dirent *d = readdir(dir);
        if (d == NULL) {
            if (errno != 0) {
                fprintf(stderr, "store-scan: cannot read %s: %s\n", argv[1], strerror(errno));
                closedir(dir);
                return 1;
            }
            break;
        }
        if (d->d_name[0] == '.' &&
            (d->d_name[1] == '\0' || (d->d_name[1] == '.' && d->d_name[2] == '\0')))
            continue;
        printf("%d %s\n", (int)d->d_type, d->d_name);
    }
    if (closedir(dir) != 0) {
        fprintf(stderr, "store-scan: cannot close %s: %s\n", argv[1], strerror(errno));
        return 1;
    }
    if (fflush(stdout) != 0 || fclose(stdout) != 0) {
        fprintf(stderr, "store-scan: cannot write the scan: %s\n", strerror(errno));
        return 1;
    }
    return 0;
}
