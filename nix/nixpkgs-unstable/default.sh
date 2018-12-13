# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT


cd "$( dirname "${BASH_SOURCE[0]}" )"
. "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../../common.sh"
logH3 "update" "$channel"

channel="$nixUnstableChannel"

rev=$(curl -L -s "https://nixos.org/channels/${channel}/git-revision")

if ! grep -q $rev ./channel.rev 2>/dev/null; then
    echo $rev > ./channel.rev

    tarball="https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz"
    prefetchOutput=$(nix-prefetch-url --unpack --print-path --type sha256 $tarball)
    hash=$(echo "$prefetchOutput" | head -1)
    path=$(echo "$prefetchOutput" | tail -1)
    echo '{"url":"'$tarball'","rev": "'$rev'","sha256":"'$hash'","path":"'$path'"}' > ./channel.json
    logINFO "... updated ./channel.rev to rev=[$rev]"
else
    logINFO "... ./channel.rev file is already up to date, at rev=[$rev]"
fi
# TODO: needs remote (already there) and fetch
# logINFO "the channel $channel was last updated $(git log --format="%cr" $rev -1)"
