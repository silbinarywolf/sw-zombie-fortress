# Zombie Fortress Classic
*Version:* Pre-alpha v0.2.0

This VScript mod aims to be a faithful reimplementation of the Zombie Fortress mode made originally with [SourceMod](https://forums.alliedmods.net/showthread.php?t=77762) for Team Fortress 2.

## Description

The survivors must fight back a horde of zombies and survive an allotted amount of time before they are all killed and turned. At the beginning of the round, the teams are shuffled in such a manner that 2 thirds of the players are survivors and the remaining third are zombies. Each team has classes assigned to them and they can only be that class.

The survivors (pyro, soldier, demoman, engineer, sniper and medic) must prevent the zombies from capturing the control points while at the same time avoiding being killed in the process. If a survivor is killed, they join the zombie team immediately. The biggest issue the survivors face is running out of ammo.

The zombies (scout, spy and heavy) need to either kill all the survivors or capture all the control points to win. Unlike the survivors, the zombies are only armed with melee attacks. Group efforts to take out important targets is crucial as a lone zombie will often lose to a group of survivors. All zombies have minor health regeneration.

## Features

- Automatic tuning of capture point time on non-Zombie Fortress maps. For example, capture points on cp_dustbowl will take 13 times longer to capture. This behaviour is loosely aligned with [zf_dustbowl](https://steamcommunity.com/sharedfiles/filedetails/?id=2926638864).
- Bots can function in this game mode, if the map has a generated navmesh.

## Install

*Warning:* These instructions assume you have no other vscripts setup and will override your `mapspawn.nut` file if you made one.

- See latest patch notes and download the [latest release here](https://github.com/silbinarywolf/sw-zombie-fortress/releases).

- For Dedicated servers:
    - Go to your `steamapps\common\Team Fortress 2 Dedicated Server\tf` folder

- For Hosting in-game:
    - Go to your `steamapps\common\Team Fortress 2\tf` folder

- Copy-paste the contents of the zip into it.

- Reload the current map and you'll be playing the Zombie Fortress game mode.

## Credits

This rewrite couldn't exist without the work of those who came before me.

* [Sirot](https://forums.alliedmods.net/showthread.php?t=77762) for creating the first Zombie Fortress mod.
* [Dirtyminuth](https://forums.alliedmods.net/showthread.php?t=131282) for rewriting the original Zombie Fortress mod.
* [ktm](https://forums.alliedmods.net/showthread.php?p=2067604) for maintaining Dirtyminuth's rewrite and releasing Zombie Fortress Redux.

## Special Thanks

* [Ficool2](https://github.com/ficool2) for VScript support
* [Joshua](https://github.com/ValveSoftware/Source-1-Games/issues/4481) for VScript support
