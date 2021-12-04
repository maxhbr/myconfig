#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-

import asyncio

from elgato import Elgato, State, Info


async def main():
    """Show example on controlling your Elgato Light device."""
    async with Elgato("192.168.1.202") as elgato1:
        # info1: Info = await elgato1.info()
        # print(info1)

        state1: State = await elgato1.state()
        print(state1)

        async with Elgato("192.168.1.231") as elgato2:
            # info2: Info = await elgato2.info()
            # print(info2)

            state2: State = await elgato2.state()
            print(state2)

            await elgato2.light(on=(not state1.on))
        await elgato1.light(on=(not state1.on))

if __name__ == "__main__":
    loop = asyncio.get_event_loop()
    loop.run_until_complete(main())
