/*
 * mysbx-vsock-bridge — the krun restricted-mode guest relay
 * (bd myconfig-ild7 — n8kv.2 C3, modules/myconfig.ai.dev/mysbx).
 *
 * The proxy-only krun guest has NO net device: the only path to
 * the per-run egress broker is the EXPLICIT AF_VSOCK device whose
 * one port the launcher mapped onto the broker's Unix socket
 * (krun_add_vsock_port — the VMM's muxer connects the socket per
 * guest connection, so the host side needs no relay process).
 * Payloads speak http://127.0.0.1:<port> URLs; this bridge makes
 * that address real inside the guest: it listens on the guest's
 * own loopback and pumps every accepted connection to
 * VSOCK-CONNECT:CID 2:<port> (CID 2 = the host muxer).
 *
 * Static, no libc requirements beyond socket/poll/fork: baked
 * into the krun rootfs as bin/mysbx-vsock-bridge (the
 * krun-store-scan idiom), started by the guest init when the
 * manifest env carries MYSBX_KRUN_NET=restricted.
 *
 * Concurrency model: fork per accepted connection, SIGCHLD set to
 * SIG_IGN in the parent so the kernel auto-reaps finished children
 * — the payload execs from PID 1 (the init script), so nobody is
 * left to wait(2) the bridge's children otherwise. Each child is
 * one full-duplex byte pump: reads from whichever side is ready
 * (poll), writes to the other, exits at the first EOF/error — the
 * broker closes its side when the guest connection ends.
 *
 * Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>
#include <netinet/in.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef AF_VSOCK
#define AF_VSOCK 40
#endif

/* CID 2 is the host (VMADDR_CID_HOST): the guest dials the VMM's
 * muxer, which holds the vsock port map. */
#define VSOCK_HOST_CID 2

/* struct sockaddr_vm (linux/vm_sockets.h) spelled locally: the static
 * musl headers do not ship kernel UAPI headers. The full kernel
 * layout INCLUDING the trailing svm_flags byte — sizeof is 16, and
 * connect(2) fails with EINVAL for anything shorter (the guest
 * kernel's vsock_addr_cast rejects a short sockaddr before it
 * even looks at the CID). */
struct sockaddr_vm {
    unsigned short svm_family;
    unsigned short svm_reserved1;
    unsigned int svm_port;
    unsigned int svm_cid;
    unsigned char svm_flags;
    unsigned char __pad[3];
};

static void die(const char *what)
{
    fprintf(stderr, "mysbx-vsock-bridge: %s: %s\n", what, strerror(errno));
    exit(1);
}

/* One full-duplex pump between the accepted guest socket and the
 * connected vsock peer: poll both sides, move whichever is ready,
 * stop at the first EOF or hard error. */
static int pump(int a, int b)
{
    struct pollfd fds[2];
    char buf[16 * 1024];
    int open_a = 1, open_b = 1;

    for (;;) {
        fds[0].fd = open_a ? a : -1;
        fds[0].events = POLLIN;
        fds[1].fd = open_b ? b : -1;
        fds[1].events = POLLIN;
        if (!open_a && !open_b)
            return 0;
        int n = poll(fds, 2, -1);
        if (n < 0) {
            if (errno == EINTR)
                continue;
            return -1;
        }
        if (fds[0].revents & (POLLIN | POLLERR | POLLHUP)) {
            ssize_t r = read(a, buf, sizeof buf);
            if (r <= 0) {
                shutdown(b, SHUT_WR);
                open_a = 0;
            } else {
                ssize_t off = 0;
                while (off < r) {
                    ssize_t w = write(b, buf + off, r - off);
                    if (w < 0)
                        return -1;
                    off += w;
                }
            }
        }
        if (fds[1].revents & (POLLIN | POLLERR | POLLHUP)) {
            ssize_t r = read(b, buf, sizeof buf);
            if (r <= 0) {
                shutdown(a, SHUT_WR);
                open_b = 0;
            } else {
                ssize_t off = 0;
                while (off < r) {
                    ssize_t w = write(a, buf + off, r - off);
                    if (w < 0)
                        return -1;
                    off += w;
                }
            }
        }
    }
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "usage: mysbx-vsock-bridge PORT\n");
        return 2;
    }
    char *end = NULL;
    long port = strtol(argv[1], &end, 10);
    if (!end || *end != '\0' || port <= 0 || port > 65535) {
        fprintf(stderr, "mysbx-vsock-bridge: bad port `%s'\n", argv[1]);
        return 2;
    }

    int lsn = socket(AF_INET, SOCK_STREAM, 0);
    if (lsn < 0)
        die("socket");
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof addr);
    addr.sin_family = AF_INET;
    addr.sin_port = htons((unsigned short)port);
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    int one = 1;
    setsockopt(lsn, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    if (bind(lsn, (struct sockaddr *)&addr, sizeof addr) < 0)
        die("bind 127.0.0.1");
    if (listen(lsn, 16) < 0)
        die("listen");

    /* The init execs the payload from PID 1 — nobody waits on this
     * process's children, so let the kernel reap them. */
    signal(SIGCHLD, SIG_IGN);

    for (;;) {
        int peer = accept(lsn, NULL, NULL);
        if (peer < 0) {
            if (errno == EINTR)
                continue;
            die("accept");
        }
        pid_t pid = fork();
        if (pid < 0) {
            /* No fork left: serve THIS connection inline, then go
             * back to accepting — degraded but honest. */
            int vs = socket(AF_VSOCK, SOCK_STREAM, 0);
            if (vs < 0)
                die("socket vsock");
            struct sockaddr_vm vm;
            memset(&vm, 0, sizeof vm);
            vm.svm_family = AF_VSOCK;
            vm.svm_port = (unsigned int)port;
            vm.svm_cid = VSOCK_HOST_CID;
            if (connect(vs, (struct sockaddr *)&vm, sizeof vm) < 0)
                die("connect vsock");
            pump(peer, vs);
            close(peer);
            close(vs);
            continue;
        }
        if (pid == 0) {
            /* child: one connection, one vsock dial, one pump. */
            close(lsn);
            int vs = socket(AF_VSOCK, SOCK_STREAM, 0);
            if (vs < 0)
                die("socket vsock");
            struct sockaddr_vm vm;
            memset(&vm, 0, sizeof vm);
            vm.svm_family = AF_VSOCK;
            vm.svm_port = (unsigned int)port;
            vm.svm_cid = VSOCK_HOST_CID;
            if (connect(vs, (struct sockaddr *)&vm, sizeof vm) < 0)
                die("connect vsock");
            pump(peer, vs);
            _exit(0);
        }
        /* parent: the child owns the connection. */
        close(peer);
    }
}
