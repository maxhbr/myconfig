#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-

import asyncio

from elgato import Elgato, State, Info


async def main():
    """Show example on controlling your Elgato Light device."""
    async with Elgato("192.168.1.202") as elgato:
        info: Info = await elgato.info()
        print(info)

        state: State = await elgato.state()
        print(state)

        # Toggle the Light
        await elgato.light(on=(not state.on))


if __name__ == "__main__":
    loop = asyncio.get_event_loop()
    loop.run_until_complete(main())
