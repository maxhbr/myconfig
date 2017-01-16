#!/bin/sh
#
#   see:   https://wiki.archlinux.org/index.php/ECryptfs
#   needs: ecryptfs-utils package
#
# Setup via:
#   $ mkdir ~/.secret ~/secret ~/.ecryptfs
#   $ touch ~/.ecryptfs/secret.conf ~/.ecryptfs/secret.sig
#
#   $ echo "/home/USER/.secret /home/USER/secret ecryptfs" > ~/.ecryptfs/secret.conf
#
# add output from `ecryptfs-add-passphrase` to  ~/.ecryptfs/secret.sig twice via
#   $ echo 78c6f0645fe62da0 > ~/.ecryptfs/secret.sig
#   $ echo 78c6f0645fe62da0 >> ~/.ecryptfs/secret.sig

ecryptfs-add-passphrase && mount.ecryptfs_private secret
read -p "Press any key to unmount again... " -n1 -s
umount.ecryptfs_private secret
