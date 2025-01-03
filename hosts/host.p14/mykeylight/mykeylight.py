#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-

import asyncio
import argparse
import os

from elgato import Elgato, State, Info

parser = argparse.ArgumentParser(description='mykeylight.')
parser.add_argument('--on', action=argparse.BooleanOptionalAction)
parser.add_argument('--off', action=argparse.BooleanOptionalAction)

config = open(os.path.expanduser('~/.mykeylight'), 'r')
with open(os.path.expanduser('~/.mykeylight'), 'r') as config:
    ips = config.read().splitlines()

async def toggle():
    async with Elgato(ips[0]) as elgato:
        state: State = await elgato.state()
        await set(not state.on)

async def set(on: bool):
    for ip in ips:
        async with Elgato(ip) as elgato:
            await elgato.light(on=on)

async def main():
    args = parser.parse_args()
    if args.on:
        await set(True)
    elif args.off:
        await set(False)
    else:
        await toggle()

if __name__ == "__main__":
    loop = asyncio.get_event_loop()
    loop.run_until_complete(main())
