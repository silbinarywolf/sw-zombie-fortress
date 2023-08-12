::SW_ZOMBIE_FORTRESS <- { "Version" : "0.1.0" }

ClearGameEventCallbacks();

// Learning References
// - Functions: https://developer.valvesoftware.com/wiki/Team_Fortress_2/Scripting/Script_Functions
// - Constants: https://developer.valvesoftware.com/wiki/Team_Fortress_2/Scripting/Script_Functions/Constants
// - Events:
//		- Source: https://wiki.alliedmods.net/Generic_Source_Events
//		- Source Server Only: https://wiki.alliedmods.net/Generic_Source_Server_Events
//		- TF2: https://wiki.alliedmods.net/Team_Fortress_2_Events
// - Example: https://developer.valvesoftware.com/wiki/Team_Fortress_2/Scripting/VScript_Examples/en#See_Also
// - Weapon IDs: https://wiki.alliedmods.net/Team_fortress_2_item_definition_indexes
// - VScript Libs: https://developer.valvesoftware.com/wiki/List_of_Script_Libraries
// - Gamebanana Vscript mods: https://gamebanana.com/mods/cats/20983
// - List of item attributes: https://wiki.teamfortress.com/wiki/List_of_item_attributes
// - List of item definition IDs: https://wiki.alliedmods.net/Team_fortress_2_item_definition_indexes
// - AddCondEx cheatsheet: https://wiki.teamfortress.com/wiki/Cheats
//
// - Test map: changelevel workshop/2926638864
// - Bots: tf_bot_add 3 heavyweapons blue expert
//
// Debug quicker:
// - Skip wait time - mp_waitingforplayers_cancel 1
// - Skip setup time - ent_fire team_round_timer disable

ZF_ROUND_WAITING <- 1;
ZF_ROUND_IN_SETUP <- 2;
ZF_ROUND_STARTED <- 3;

hasInitialized <- false;

is_zf_map <- false;

humanTeamNumber <- Constants.ETFTeam.TF_TEAM_RED;

zombieTeamNumber <- Constants.ETFTeam.TF_TEAM_BLUE;

// Percentage of players that start as survivors, must have at least 1 survivor
survivorPlayerPercent <- 0.65;

// We need to track round state for reasons mentioned below in comments, for example:
// - Tracking if we're still in the waiting period
// - Knowing if the game is no longer in setup when handling death events
zf_roundState <- ZF_ROUND_WAITING;
zf_roundStateFiredFirstRoundStart <- false;

// player_actual_team maps a player ID to their start team
//
// This is used to stop players changing their teams during setup, mostly
// motivation to stop bots doing it
player_actual_team <- {};

// zf_rageTimer maps to a player ID, if its above zero, the player cannot use "Call for medic"
// to get a health boost
zf_rageTimer <- {};

// zf_standardMapCapTimeScale will multiply the time it takes to capture a point time by N.
// So if it takes 6 seconds to capture, and you set this to 2, it'll take 12 seconds to capture.
//
// note(jae): 2023-08-12
// We scale by 13 to make cp_dustbowl work roughly like zf_dustbowl. We may want this to be
// configurable per map in the future.
//
// zf_dustbowl cap time = 40 seconds
// cp_dustbowl cap time = 3 seconds
zf_standardMapCapTimeScale <- 13;

// zf_rageCantUseMessageTimer maps to a player ID, if its above zero we avoid showing the player
// a warning about rage
zf_rageCantUseMessageTimer <- {};

zf_hasWelcomeMessage <- {};

// zf_debugDevMode adds chat commands for faster dev and debugging.
zf_debugDevMode <- false;

// tf_gamerules holds the tf_gamerules game objects after the round starts
tf_gamerules <- null;

::IsZombieClass <- function(classIndex) {
	return (
		classIndex == Constants.ETFClass.TF_CLASS_SCOUT ||
		classIndex == Constants.ETFClass.TF_CLASS_HEAVYWEAPONS ||
		classIndex == Constants.ETFClass.TF_CLASS_SPY
	)
}

::GetValidClass <- function(player) {
	if (player == null || !player.IsValid()) {
		return;
	}
	local team = player.GetTeam();
	local classIndex = player.GetPlayerClass();
	switch (team) {
	case zombieTeamNumber: {
		local isValidClass = (
			classIndex == Constants.ETFClass.TF_CLASS_SCOUT ||
			classIndex == Constants.ETFClass.TF_CLASS_HEAVYWEAPONS ||
			classIndex == Constants.ETFClass.TF_CLASS_SPY
		);
		if (isValidClass) {
			return classIndex;
		}
		local defaultZombieClass = Constants.ETFClass.TF_CLASS_SCOUT;
		local randomClassIndex = RandomInt(0, 2);
		switch (randomClassIndex) {
		case 0: defaultZombieClass = Constants.ETFClass.TF_CLASS_SCOUT; break;
		case 1: defaultZombieClass = Constants.ETFClass.TF_CLASS_HEAVYWEAPONS; break;
		case 2: defaultZombieClass = Constants.ETFClass.TF_CLASS_SPY; break;
		}
		return defaultZombieClass;
	}
	case humanTeamNumber: {
		local isValidClass = (
			classIndex == Constants.ETFClass.TF_CLASS_SOLDIER ||
			classIndex == Constants.ETFClass.TF_CLASS_PYRO ||
			classIndex == Constants.ETFClass.TF_CLASS_DEMOMAN ||
			classIndex == Constants.ETFClass.TF_CLASS_ENGINEER ||
			classIndex == Constants.ETFClass.TF_CLASS_MEDIC ||
			classIndex == Constants.ETFClass.TF_CLASS_SNIPER
		);
		if (isValidClass) {
			return classIndex;
		}
		local defaultHumanClass = Constants.ETFClass.TF_CLASS_PYRO;
		local randomClassIndex = RandomInt(0, 5);
		switch (randomClassIndex) {
		case 0: defaultHumanClass = Constants.ETFClass.TF_CLASS_SOLDIER; break;
		case 1: defaultHumanClass = Constants.ETFClass.TF_CLASS_PYRO; break;
		case 2: defaultHumanClass = Constants.ETFClass.TF_CLASS_DEMOMAN; break;
		case 3: defaultHumanClass = Constants.ETFClass.TF_CLASS_ENGINEER; break;
		case 4: defaultHumanClass = Constants.ETFClass.TF_CLASS_MEDIC; break;
		case 5: defaultHumanClass = Constants.ETFClass.TF_CLASS_SNIPER; break;
		}
		return defaultHumanClass;
	}
	default: {
		return classIndex;
	}
	}
}

::SetZombieSpawnTime <- function(time) {
	if (tf_gamerules == null || !tf_gamerules.ValidateScriptScope()) {
		return;
	}
	// note(jae): 2023-08-06
	// This time division occurs in "setTeamRespawnTime" as part of zombiefortress_vanilla.
	// So we keep that behaviour here
	if (time != 0) {
		time = time / 2.0;
	}
	// note(jae): 2023-08-06
	// Unable to get this to work... I think. Hard to test if this works or not.
	if (zombieTeamNumber == Constants.ETFTeam.TF_TEAM_BLUE) {
		// NetProps.SetPropFloat(tf_gamerules, "SetBlueTeamRespawnWaveTime", time);
		EntFireByHandle(tf_gamerules, "SetBlueTeamRespawnWaveTime", time.tostring(), 0.0, null, null);
	} else {
		// NetProps.SetPropFloat(tf_gamerules, "SetRedTeamRespawnWaveTime", time);
		EntFireByHandle(tf_gamerules, "SetRedTeamRespawnWaveTime", time.tostring(), 0.0, null, null);
	}
	// DEBUG
	// printl("-- Updated Zombie spawn time (logic doesnt seem to work yet for some reason? or suicide affects timers...)")
}

::HandleHumanOnTick <- function(player) {
	local classIndex = player.GetPlayerClass();

	local min = @(a, b) a < b ? a : b;
	local max = @(a, b) a > b ? a : b;

	switch (classIndex) {
	case Constants.ETFClass.TF_CLASS_SNIPER:
		// ZF Rule: SMG doesn't have to reload
		local weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", 1);
		if (weapon == null) {
			break;
		}
		if (!weapon.UsesClipsForAmmo1()) {
			break;
		}
		// Weapon definition indexes
		// 16  = SMG
		// 203 = SMG (Renamed/Strange)
		local weaponDefID = NetProps.GetPropInt(weapon, "m_AttributeManager.m_Item.m_iItemDefinitionIndex");
		if (weaponDefID == 16 || weaponDefID == 203) {
			// todo(jae): 2023-08-06
			// Confirm it only affects SMG
			local ammoType = weapon.GetPrimaryAmmoType();
			local reloadClipAmmo = weapon.Clip1();
			local ammoCount = NetProps.GetPropIntArray(player, "m_iAmmo", ammoType);
			// note(jae): 2023-08-06
			// 25 is the maximum m_iAmmo for sniper SMG
			local ammoAdj = min((25 - reloadClipAmmo), ammoCount);
			if(ammoAdj > 0){
				weapon.SetClip1(reloadClipAmmo + ammoAdj);
				NetProps.SetPropIntArray(player, "m_iAmmo", ammoCount - ammoAdj, ammoType);
			}
		}
		break;
	case Constants.ETFClass.TF_CLASS_MEDIC:
		// ZF Rule: Syringe gun / blutsauger doesn't have to reload.
		local weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", 0);
		if (weapon == null) {
			break;
		}
		if (!weapon.UsesClipsForAmmo1()) {
			break;
		}
		// Weapon definition indexes
		// 17  = Syringe Gun
		// 204 = Syringe Gun (Renamed/Strange)
		// 36  = The Blutsauger
		local weaponDefID = NetProps.GetPropInt(weapon, "m_AttributeManager.m_Item.m_iItemDefinitionIndex");
		if (weaponDefID == 17 || weaponDefID == 204 || weaponDefID == 36) {
			local ammoType = weapon.GetPrimaryAmmoType();
			local reloadClipAmmo = weapon.Clip1();
			local ammoCount = NetProps.GetPropIntArray(player, "m_iAmmo", ammoType);
			// note(jae): 2023-08-06
			// 40 is the maximum m_iAmmo for medic
			local ammoAdj = min((40 - reloadClipAmmo), ammoCount);
			if (ammoAdj > 0) {
				weapon.SetClip1(reloadClipAmmo + ammoAdj);
				NetProps.SetPropIntArray(player, "m_iAmmo", ammoCount - ammoAdj, ammoType);
			}
		}
		break;
	case Constants.ETFClass.TF_CLASS_PYRO:
		// ZF Rule: Flamethrower / backburner ammo limited to 125.
		local weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", 0);
		if (weapon == null) {
			break;
		}
		local ammoType = weapon.GetPrimaryAmmoType();
		local ammoCount = NetProps.GetPropIntArray(player, "m_iAmmo", ammoType);
		if (ammoCount > 125) {
			NetProps.SetPropIntArray(player, "m_iAmmo", 125, ammoType);
		}
		break;
	}
}

::HandleZombieOnTick <- function(player) {
	local health = player.GetHealth();
	local maxHealth = player.GetMaxHealth();
	local classIndex = player.GetPlayerClass();

	local min = @(a, b) a < b ? a : b;
	local max = @(a, b) a > b ? a : b;

	// Handle spy cloak, limit spy cloak to 80% of max.
	//
	// note(jae): 2023-08-06
	// Original mod did something like this, instead we change the consume rate
	// via "tf_spy_cloak_consume_rate".
	// if(classIndex == Constants.ETFClass.TF_CLASS_SPY &&
	// 	player.GetSpyCloakMeter() > 80.0) {
	//     player.SetSpyCloakMeter(80.0);
	// }

	// 1. Handle zombie regeneration.
	//    Zombies regenerate health based on class and number of nearby
	 //    zombies (hoarde bonus). Zombies decay health when overhealed.
	if (health < maxHealth) {
		local bonus = 0;
		switch (classIndex) {
		case Constants.ETFClass.TF_CLASS_SCOUT:
			bonus = 2; // + (1 * zf_hoardeBonus[i]);
			break;
		case Constants.ETFClass.TF_CLASS_HEAVYWEAPONS:
			bonus = 4; // + (3 * zf_hoardeBonus[i]);
			break;
		case Constants.ETFClass.TF_CLASS_SPY:
			bonus = 2; // + (1 * zf_hoardeBonus[i]);
			break;
		}
		health += bonus;
		health = min(health, maxHealth);
		player.SetHealth(health);
	}
	if (health > maxHealth) {
		// Decay if overhealed
		local bonus = 0;
		switch (classIndex) {
		case Constants.ETFClass.TF_CLASS_SCOUT:
			bonus = -3;
			break;
		case Constants.ETFClass.TF_CLASS_HEAVYWEAPONS:
			bonus = -7;
			break;
		case Constants.ETFClass.TF_CLASS_SPY:
			bonus = -3;
			break;
		}
		health += bonus;
		health = max(health, maxHealth);
		player.SetHealth(health);
	}

	// 2. Handle zombie crit rate bonus.
	//    Zombies receive crit bonus based on number of nearby zombies
	//    (hoarde bonus). Zombies only receive this bonus at full health
	//    or greater.
	// bonus = 0;
	// if(health >= maxHealth) {
	// 	switch(clientClass) {
	// 		case TFClass_Scout: bonus = 5 + (1 * zf_hoardeBonus[i]);
	// 		case TFClass_Heavy: bonus = 10 + (5 * zf_hoardeBonus[i]);
	// 		case TFClass_Spy:   bonus = 5 + (1 * zf_hoardeBonus[i]);
	// 	}
	// }
	// zf_critBonus[i] = bonus;

	// 3. Handle zombie rage timer
	//    Rage recharges every 30s.
	if (player in zf_rageTimer && zf_rageTimer[player] > 0) {
		zf_rageTimer[player]--;
		if (zf_rageTimer[player] == 0) {
			ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Rage is ready! (Call for medic for health boost)");
		}
	}
	if (player in zf_rageCantUseMessageTimer && zf_rageCantUseMessageTimer[player] > 0) {
		zf_rageCantUseMessageTimer[player]--;
	}

	// Activate rage
	//
	// - If regular player, it'll activate when you call for medic
	// - If their a bot, it'll fire immediately when they can use it.
	local isBot = IsPlayerABot(player);
	local isCallingMedicOrBot = player.IsCallingForMedic() || isBot;
	if (isCallingMedicOrBot) {
		local health = player.GetHealth();
		local maxHealth = NetProps.GetPropInt(player, "m_iMaxHealth");

		local rageTimer = 0;
		if (player in zf_rageTimer) {
			rageTimer = zf_rageTimer[player];
		}

		if (rageTimer == 0) {
			if (health >= maxHealth) {
				zf_rageTimer[player] <- 30; // set 30 seconds till rage resets
				zf_rageCantUseMessageTimer[player] <- 5;
				player.SetHealth(maxHealth * 1.5);
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Rage activated!");
			} else {
				// Avoid showing same message per tick
				if (!isBot && (!(player in zf_rageCantUseMessageTimer) || zf_rageCantUseMessageTimer[player] == 0)) {
					ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Cannot activate rage, must be at least full health");
					zf_rageCantUseMessageTimer[player] <- 5;
				}
			}
		} else {
			// Avoid showing same message per tick
			if (!isBot && (!(player in zf_rageCantUseMessageTimer) || zf_rageCantUseMessageTimer[player] == 0)) {
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Cannot activate rage, waiting " + rageTimer + " more second(s) for cooldown");
				zf_rageCantUseMessageTimer[player] <- 5;
			}
		}
	}
}

::HandleSentry <- function() {
	local min = @(a, b) a < b ? a : b;

	// ZF Rule: Handle sentry rules.
	//    + Norm sentry starts with 60 ammo and decays to 10.
	//    + Mini sentry starts with 60 ammo and decays to 0, then self destructs.
	//    + No sentry can be upgraded.
	for (local ent = null; ent = Entities.FindByClassname(ent, "obj_sentrygun");) {
		local level = NetProps.GetPropInt(ent, "m_iHighestUpgradeLevel");
		if (level > 1) {
			// Disallow upgraded sentry
			//
			// note(jae): 2023-08-12
			// Technically redundant as we have "tf_obj_upgrade_per_hit" and/or "tf_sentrygun_metal_per_shell"
			// stops this.
			ent.SetHealth(0);
			continue;
		}

		local isBuilding = NetProps.GetPropBool(ent, "m_bBuilding");
		local isPlacing = NetProps.GetPropBool(ent, "m_bPlacing");
		local isCarried = NetProps.GetPropBool(ent, "m_bCarried");
		local isMiniSentry = NetProps.GetPropBool(ent, "m_bMiniBuilding");
		if (!isBuilding && !isPlacing && !isCarried) {
			local ammoShellsCount = NetProps.GetPropInt(ent, "m_iAmmoShells");
			if(ammoShellsCount > 0) {
				if (isMiniSentry || (ammoShellsCount > 10)) {
					NetProps.SetPropInt(ent, "m_iAmmoShells", min(60, (ammoShellsCount - 1)));
				}
			} else {
				// note(jae): 2023-08-12
				// Original ZF mod says this destroys the sentry but it doesn't.
				//
				// Not sure I need to fix this since it makes the sentry killable in 1 hit
				// after it's out of ammo.
				ent.SetHealth(0);
			}
		}
	}
}

::ChangeToActualTeam <- function() {
	local player = self;
	if (player == null || !player.IsValid()) {
		return;
	}
	local team = player.GetTeam();
	if (!IsValidTeam(team)) {
		return;
	}
	local actual_team = team;
	if (player in player_actual_team) {
		actual_team = player_actual_team[player];
	}
	player.ForceChangeTeam(actual_team, false);

	// If players team is zombie, re-check player count and see if zombies win
	if (actual_team != zombieTeamNumber) {
		return;
	}
	local humanCount = 0;
	for (local i = 1; i <= Constants.Server.MAX_PLAYERS; i++) {
		local player = PlayerInstanceFromIndex(i)
		if (player == null || !(player in player_actual_team)) {
			continue;
		}
		local team = player_actual_team[player];
		switch (team) {
		case humanTeamNumber:
			humanCount++;
			break;
		}
	}
	if (humanCount == 0) {
		local game_round_win = SpawnEntityFromTable("game_round_win", {
			TeamNum=zombieTeamNumber,
			force_map_reset=true,
			switch_teams=false
		})
		EntFireByHandle( game_round_win, "RoundWin", "", 0, null, null );
	}
}

// runs on team_round_timer entity
//
// equivalent to timer_main from zombiefortress_vanilla
::TickEverySecond <- function() {
	for (local i = 1; i <= Constants.Server.MAX_PLAYERS; i++) {
		local player = PlayerInstanceFromIndex(i)
		if (player == null || !player.IsValid()) {
			continue;
		}
		local team = player.GetTeam();
		switch (team) {
		case humanTeamNumber:
			HandleHumanOnTick(player);
			break;
		case zombieTeamNumber:
			HandleZombieOnTick(player);
			break;
		}
	}

	HandleSentry();

	// 1 = one second
	return 1;
}

::IsVoodooWearable <- function(wearable) {
	local itemId = NetProps.GetPropInt(wearable, "m_AttributeManager.m_Item.m_iItemDefinitionIndex");
	return (itemId == 5617 || itemId == 5625 || itemId == 5618 || itemId == 5620 ||
			itemId == 5622 || itemId == 5619 || itemId == 5624 || itemId == 5623 ||
			itemId == 5621);
}

// AttachVoodooWearable is needed for completing the zombie skin as it contains part
// of the body model
// ::AttachVoodooWearable <- function() {
// 	local player = self;
// 	if (player == null || !player.IsValid()) {
// 		return;
// 	}

// 	// Check if has voodoo wearable already
// 	local hasVoodooWearable = false;
// 	for (local wearable = player.FirstMoveChild(); wearable != null;) {
// 		if (wearable.GetClassname() != "tf_wearable") {
// 			wearable = wearable.NextMovePeer();
// 			continue;
// 		}
// 		wearable = wearable.NextMovePeer();
// 		if (IsVoodooWearable(wearable)) {
// 			hasVoodooWearable = true;
// 			break;
// 		}
// 	}
// 	if (hasVoodooWearable) {
// 		return;
// 	}

// 	// note(jae): 2023-08-08
// 	// This logic only *sometimes* works, on Scout / Heavy and sometimes doesn't
// 	// apply at all for unknown reasons.
// 	//
// 	// Also corpses don't retain the wearable so they look buggy
// 	//
// 	// Spy also just seems to plain not work for reasons I don't understand
// 	local team = player.GetTeam();
// 	local classIndex = player.GetPlayerClass();

// 	local itemID = -1;
// 	switch (classIndex) {
// 	case Constants.ETFClass.TF_CLASS_SCOUT:        itemID = 5617; break;
// 	case Constants.ETFClass.TF_CLASS_SNIPER:       itemID = 5625; break;
// 	case Constants.ETFClass.TF_CLASS_SOLDIER:      itemID = 5618; break;
// 	case Constants.ETFClass.TF_CLASS_DEMOMAN:      itemID = 5620; break;
// 	case Constants.ETFClass.TF_CLASS_MEDIC:        itemID = 5622; break;
// 	case Constants.ETFClass.TF_CLASS_HEAVYWEAPONS: itemID = 5619; break;
// 	case Constants.ETFClass.TF_CLASS_PYRO:         itemID = 5624; break;
// 	case Constants.ETFClass.TF_CLASS_SPY:          itemID = 5623; break;
// 	case Constants.ETFClass.TF_CLASS_ENGINEER:     itemID = 5621; break;
// 	}
// 	if (itemID == -1) {
// 		return;
// 	}

// 	// Setup part of the body model
// 	local voodooWearable = Entities.CreateByClassname("tf_wearable");
// 	voodooWearable.SetAbsOrigin(player.GetLocalOrigin());
// 	voodooWearable.SetAbsAngles(player.GetLocalAngles());
// 	NetProps.SetPropBool(voodooWearable, "m_bClientSideAnimation", true);
// 	NetProps.SetPropInt(voodooWearable, "m_iTeamNum", player.GetTeam());
// 	NetProps.SetPropInt(voodooWearable, "m_Collision.m_usSolidFlags", Constants.FSolid.FSOLID_NOT_SOLID);
// 	NetProps.SetPropInt(voodooWearable, "m_CollisionGroup", 11);
// 	NetProps.SetPropInt(voodooWearable, "m_fEffects", 129); //1 and 128 bitmasks both bone merge to player model
// 	NetProps.SetPropInt(voodooWearable, "m_AttributeManager.m_Item.m_iItemDefinitionIndex", itemID);
// 	NetProps.SetPropInt(voodooWearable, "m_AttributeManager.m_Item.m_iEntityLevel", 1);
// 	//SetPropInt(voodooWearable, "m_AttributeManager.m_Item.m_iEntityQuality", 0);	//doesn't work due to vscript security reasons
// 	NetProps.SetPropBool(voodooWearable, "m_bValidatedAttachedEntity", true);
// 	NetProps.SetPropInt(voodooWearable, "m_AttributeManager.m_iReapplyProvisionParity", 3);
// 	NetProps.SetPropBool(voodooWearable, "m_AttributeManager.m_Item.m_bInitialized", true)	//Seems to bug with upgrades stations / MvM
// 	NetProps.SetPropBool(voodooWearable, "m_AttributeManager.m_Item.m_bOnlyIterateItemViewAttributes", false);

// 	NetProps.SetPropEntity(voodooWearable, "m_hOwnerEntity", player);
// 	voodooWearable.SetOwner(player);

// 	Entities.DispatchSpawn(voodooWearable);
// 	voodooWearable.ReapplyProvision();
// 	DoEntFire("!self", "SetParent", "!activator", 0, player, voodooWearable);

// 	// Setup skin
// 	NetProps.SetPropInt(player, "m_bForcedSkin", 1);
// 	switch (team) {
// 	case Constants.ETFTeam.TF_TEAM_RED:
// 		if (classIndex != Constants.ETFClass.TF_CLASS_ENGINEER) {
// 			NetProps.SetPropInt(player, "m_nForcedSkin", 4);
// 		} else {
// 			NetProps.SetPropInt(player, "m_nForcedSkin", 22);
// 		}
// 		break;
// 	case Constants.ETFTeam.TF_TEAM_BLUE:
// 		if (classIndex != Constants.ETFClass.TF_CLASS_ENGINEER) {
// 			NetProps.SetPropInt(player, "m_nForcedSkin", 5);
// 		} else {
// 			NetProps.SetPropInt(player, "m_nForcedSkin", 23);
// 		}
// 		break;
// 	}
// 	player.AddCustomAttribute("player skin override", 1.0, -1);
// 	player.AddCustomAttribute("zombiezombiezombiezombie", 1.0, -1);
// 	player.AddCustomAttribute("SPELL: Halloween voice modulation", 1.0, -1);
// }

::KillSapperWeapon <- function() {
	local player = self;
	if (player == null || !player.IsValid()) {
		return;
	}
	for (local i = 0; i < 7; i++) {
		local weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", i);
		if (weapon == null) {
			continue;
		}
		local className = weapon.GetClassname();
		// note(jae): 2023-08-10
		// "tf_weapon_builder" = the original sapper
		// "tf_weapon_sapper" = "The Red-Tape Recorder"
		if (className == "tf_weapon_builder" || className == "tf_weapon_sapper") {
			NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
			weapon.Kill();
		}
	}
}

function SW_ZF_Init(file_scope_this) {
	if (hasInitialized) {
		return;
	}
	local mapname = GetMapName();

	is_zf_map = (
		startswith(mapname, "zf_") ||
		startswith(mapname, "workshop/zf_") ||
		startswith(mapname, "zf2_") ||
		startswith(mapname, "workshop/zf2_")
	);

	// todo(jae): 2023-08-07
	// Figure out how to make ZF auto-enable for ZF maps
	// but allow server hosters to configure / opt-in for certain.
	// maps. Will likely introduce a config file later to be always on or something.
	local is_zf_enabled = true;

	if (!is_zf_enabled) {
		return;
	}

	// DEBUG
	// printl("-- Init ZF");

	// In control point, blue is attacking, so swap the teams
	local is_control_point_map = (
		startswith(mapname, "cp_") ||
		startswith(mapname, "workshop/cp_")
	);
	if (is_control_point_map) {
		// Check that defending control points are for RED team, if they are
		// then swap the zombie teams
		local should_swap_teams = true;
		for (local ent = null; ent = Entities.FindByClassname(ent, "team_control_point");) {
			local team = NetProps.GetPropInt(ent, "m_iTeamNum");
			control_point_count++;
			if (team != Constants.ETFTeam.TF_TEAM_RED) {
				// Don't swap teams if control point is owned by blue
				should_swap_teams = false;
				break;
			}
		}
		if (should_swap_teams) {
			humanTeamNumber = Constants.ETFTeam.TF_TEAM_BLUE;
			zombieTeamNumber = Constants.ETFTeam.TF_TEAM_RED;
		}
	}

	local is_payload_map = (
		startswith(mapname, "pl_") ||
		startswith(mapname, "workshop/pl_")
	);
	if (is_payload_map) {
		// Swap teams for payload
		humanTeamNumber = Constants.ETFTeam.TF_TEAM_BLUE;
		zombieTeamNumber = Constants.ETFTeam.TF_TEAM_RED;
	}

	// note(jae): 2023-08-05
	// Doesn't work, but I tried.
	// local tags = Convars.GetStr("sv_tags");
	// if (tags != null) {
	// 	Convars.SetValue("sv_tags", tags+",zf");
	// }

	// Game settings
	Convars.SetValue("tf_forced_holiday", 2); // force halloween mode, this also makes "Voodoo-Cursed_Soul" work
	Convars.SetValue("mp_scrambleteams_auto", 0);
	Convars.SetValue("tf_classlimit", 0);
	Convars.SetValue("mp_autoteambalance", 0);
	Convars.SetValue("mp_teams_unbalance_limit", 0);
	// Convars.SetValue("mp_disable_respawn_times", 0)
	Convars.SetValue("tf_dropped_weapon_lifetime", 0);
	Convars.SetValue("mp_stalemate_timelimit", 9999999);
	Convars.SetValue("mp_scrambleteams_auto_windifference", 0);
	Convars.SetValue("sv_vote_issue_autobalance_allowed", "0"); // intentionally string, copied from 2 other vscript plugins
	Convars.SetValue("sv_vote_issue_scramble_teams_allowed", "0"); // intentionally string, copied from 2 other vscript plugins

	// Bots
	Convars.SetValue("tf_bot_keep_class_after_death", true);
	Convars.SetValue("tf_bot_reevaluate_class_in_spawnroom", 0);

	// Engineer
	Convars.SetValue("tf_obj_upgrade_per_hit", 0); // Locked
	Convars.SetValue("tf_sentrygun_metal_per_shell", 201); // Locked

	// Medic
	Convars.SetValue("weapon_medigun_charge_rate", 30); // Locked, Default 40
	Convars.SetValue("weapon_medigun_chargerelease_rate", 6); // Locked, Default 8
	Convars.SetValue("tf_max_health_boost", 1.25); // Locked, Default 1.5
	Convars.SetValue("tf_boost_drain_time", 3600); // Locked, Default 15.0, Time it takes for a health boost to degrade.

	// Spy
	Convars.SetValue("tf_spy_invis_time", 0.5);
	Convars.SetValue("tf_spy_invis_unstealth_time", 0.75);
	Convars.SetValue("tf_spy_cloak_no_attack_time", 1.0);

	// Effectively makes cloak 20% smaller.
	Convars.SetValue("tf_spy_cloak_consume_rate", 12.5); // Default: 10, consumes 10 per second out of 100.

	// DEBUG: Swap teams (I used this to quickly test engineer logic)
	// local oldHumanTeam = humanTeamNumber;
	// humanTeamNumber <- zombieTeamNumber;
	// zombieTeamNumber <- oldHumanTeam;

	hasInitialized = true;
	__CollectGameEventCallbacks(file_scope_this);
}

::GivePlayerWeapon <- function(player, className, itemID) {
	// Partial copy-paste of logic from give_tf_weapon
	local weapon = SpawnEntityFromTable(className, {
		origin = player.GetOrigin(),
		angles = player.GetAbsAngles(),
		effects = 129,
		TeamNum = player.GetTeam(),
		CollisionGroup = 11,
		ltime = Time(),
	});
	NetProps.SetPropInt(weapon, "m_AttributeManager.m_Item.m_iItemDefinitionIndex", itemID);
	NetProps.SetPropInt(weapon, "m_AttributeManager.m_Item.m_iEntityLevel", 1);
	NetProps.SetPropBool(weapon, "m_AttributeManager.m_Item.m_bInitialized", true);
	NetProps.SetPropBool(weapon, "m_bClientSideAnimation", true);
	NetProps.SetPropBool(weapon, "m_bClientSideFrameReset", true);
	NetProps.SetPropBool(weapon, "m_bValidatedAttachedEntity", true);
	NetProps.SetPropInt(weapon, "m_AttributeManager.m_iReapplyProvisionParity", 1);

	NetProps.SetPropEntity(weapon, "m_hOwner", player);
	weapon.SetOwner(player);

	// Seems without this, we can't hit Engineer objects!
	local solidFlags = NetProps.GetPropInt(weapon, "m_Collision.m_usSolidFlags");
	NetProps.SetPropInt(weapon, "m_Collision.m_usSolidFlags", solidFlags | Constants.FSolid.FSOLID_NOT_SOLID);

	solidFlags = NetProps.GetPropInt(weapon, "m_Collision.m_usSolidFlags");
	NetProps.SetPropInt(weapon, "m_Collision.m_usSolidFlags", solidFlags & ~(Constants.FSolid.FSOLID_TRIGGER));
	Entities.DispatchSpawn(weapon);
	weapon.ReapplyProvision() // then applies any body attributes back onto the player.

	DoEntFire("!self", "SetParent", "!activator", 0, player, weapon);
	return weapon;
}

::IsValidTeam <- function(team) {
	return (team == humanTeamNumber || team == zombieTeamNumber);
}

::SW_PostPlayerSpawn <- function(){
	local player = self;
	if (player == null || !player.IsValid()) {
		return;
	}
	// DEBUG
	//printl("-- Player Spawned --");

	// Set max speed
	// This is based on "clientBaseSpeed" in zombiefortress_vanilla.sp
	local maxMoveSpeed = 0;
	switch (player.GetPlayerClass()) {
	case Constants.ETFClass.TF_CLASS_PYRO:
		// Default 300.0 <Slowed>
		maxMoveSpeed = 240.0;
		break;
	case Constants.ETFClass.TF_CLASS_SCOUT:
		// Default 400.0 <Slowed>
		maxMoveSpeed = 350.0;
		break;
	case Constants.ETFClass.TF_CLASS_SPY:
		// Default 300.0 <Slowed>
		maxMoveSpeed = 280.0;
		break;
	}
	if (maxMoveSpeed != 0) {
		// todo(jae): 2023-08-05
		// If we want to truly have parity with zombiefortress_vanilla need
		// to implement "clientBonusSpeed" which adds various bonuses based on
		// items equipped ,etc
		NetProps.SetPropFloat(player, "m_flMaxspeed", maxMoveSpeed);
	}
}

::ChangeClass <- function(player, classIndex) {
	if (player == null || !player.IsValid()) {
		return;
	}
	player.SetPlayerClass(classIndex);
	// note(jae): 2023-08-10
	// Without this, when the player goes to their loadout will be their desired class (the one they *tried* to choose)
	// and anytime they spawn it'll think they're their desired class again.
	NetProps.SetPropInt(player, "m_Shared.m_iDesiredPlayerClass", classIndex);
	player.ForceRegenerateAndRespawn();
}

// Based on: https://stackoverflow.com/a/2450976/5013410
::ShuffleArray <- function(array) {
	local currentIndex = array.len() - 1;

	// While there remain elements to shuffle.
	while (currentIndex > 0) {
		// Pick a remaining element.
		local randomIndex = RandomInt(0, currentIndex);
		currentIndex--;

		// And swap it with the current element.
		local current = array[currentIndex];
		local random = array[randomIndex];
		array[currentIndex] = random;
		array[randomIndex] = current;
	}
}

function OnGameEvent_teamplay_round_start(params) {
	if (!hasInitialized) {
		return;
	}

	// Setup zombie spawn time
	SetZombieSpawnTime(8.0);

	// Add our "::TickEverySecond" function to team_round_timer
	local team_round_timer = Entities.FindByClassname(null, "team_round_timer");
	if (team_round_timer == null) {
		printl("WARNING: Team round timer not found. Zombie regen and survivor behaviour won't work properly");
		return;
	}
	team_round_timer.ValidateScriptScope();
	AddThinkToEnt(team_round_timer, "TickEverySecond");

	// Update game rules
	//
	// note(jae): 2023-08-12
	// This seemingly works now but I can't test it because this goal hint
	// doesn't show up for me anymore. Not sure how to work around this...
	tf_gamerules = Entities.FindByClassname(null, "tf_gamerules");
	if (tf_gamerules != null && tf_gamerules.ValidateScriptScope()) {
		local humanGoal = "Survive the zombies";
		local zombieGoal = "Kill all survivors";
		if (humanTeamNumber == Constants.ETFTeam.TF_TEAM_RED) {
			EntFireByHandle(tf_gamerules, "SetBlueTeamGoalString", zombieGoal, 1.0, null, null);
			EntFireByHandle(tf_gamerules, "SetRedTeamGoalString", humanGoal, 1.0, null, null);
		} else {
			EntFireByHandle(tf_gamerules, "SetBlueTeamGoalString", humanGoal, 1.0, null, null);
			EntFireByHandle(tf_gamerules, "SetRedTeamGoalString", zombieGoal, 1.0, null, null);
		}
	}

	// note(jae): 2023-08-12
	// When the map first loads, the waiting round begins and this fires immediately.
	// where GetRoundState() == 3 (GR_STATE_PREROUND), so ignore the first fired event.
	// After the waiting period, it fires again with GetRoundState() == 3 (GR_STATE_PREROUND).
	//
	// If we don't do this, initial class selection feels very buggy.
	//
	// ie. Player will choose a survivor, then the game will kick them to zombie then back
	// to survivor. It's hard to describe but just trust me, it feels bad.
	//
	// I also tried detecting this via "teamplay_waiting_begins" and "teamplay_waiting_ends"
	// but they seemingly didn't fire in my testing on cp_dustbowl.
	if (!zf_roundStateFiredFirstRoundStart) {
		zf_roundStateFiredFirstRoundStart = true;
		return;
	}

	// If not full reset, don't reset any of the game such as teams
	// ie. If on cp_dustbowl you've moved on to the second or third round
	if (!params.full_reset) {
		if (zf_roundState == ZF_ROUND_WAITING ||
			zf_roundState == ZF_ROUND_STARTED) {
			zf_roundState = ZF_ROUND_IN_SETUP;
		}
		// Enable all resupply cabinets until setup ends
		for (local ent = null; ent = Entities.FindByClassname(ent, "func_regenerate");) {
			EntFireByHandle(ent, "Enable", "", 0, null, null);
		}
		// Enable spawn being blocked until setup ends
		for (local ent = null; ent = Entities.FindByClassname(ent, "func_respawnroomvisualizer");) {
			EntFireByHandle(ent, "Enable", "", 0, null, null);
		}
		return;
	}

	// reset globals
	player_actual_team = {};
	zf_rageTimer = {};
	zf_rageCantUseMessageTimer = {};
	zf_roundState = ZF_ROUND_IN_SETUP;

	// update
	//
	// note(jae): 2023-08-12
	// If this doesn't *only* happen when "params.full_reset" is true, scaling will compound across
	// stages / rounds.
	if (!is_zf_map && zf_standardMapCapTimeScale > 0) {
		// note(jae): 2023-08-12
		// I've opted to increase the cap-time by 13x by using zf_dustbowl as the comparison.
		//
		// This might feel awful on certain maps so we may want to make this configurable per
		// map or something in the future.
		//
		// zf_dustbowl cap time = 40 seconds
		// cp_dustbowl cap time = 3 seconds
		for (local ent = null; ent = Entities.FindByClassname(ent, "trigger_capture_area");) {
			local capPointName = NetProps.GetPropString(ent, "m_iszCapPointName");
			local capTimeInSeconds = NetProps.GetPropFloat(ent, "m_flCapTime");
			capTimeInSeconds *= zf_standardMapCapTimeScale;
			NetProps.SetPropFloat(ent, "m_flCapTime", capTimeInSeconds);
			// note(jae): 2023-08-12
			// If we do not fire this event, then cap time total does not propagate to the client
			// and the UI will be buggy and show the control point capping quicker then it should
			// and then "popping" back after the server updates the client.
			EntFireByHandle(ent, "SetControlPoint", capPointName, 0.0, null, null);
		}
	}

	// Test shuffle function
	// local randomIDs = [1,2,3,4,5,6,7,8,9,10];
	// ShuffleArray(randomIDs);
	// printl("-- RANDOM IDS");
	// foreach (id in randomIDs) {
	// 	printl(id);
	// }
	// printl("-- END RANDOM IDS");

	// Get players and split into survivor / zombie teams
	local playerList = [];
	for (local i = 1; i <= Constants.Server.MAX_PLAYERS; i++) {
		local player = PlayerInstanceFromIndex(i)
		if (player == null) {
			continue;
		}
		local team = player.GetTeam();
		if (!IsValidTeam(team)) {
			// ignore other teams
			continue;
		}
		playerList.push(player);
	}
	local playerCount = playerList.len();
	if (playerCount > 0) {
		local expectedHumanCount = floor(playerCount*survivorPlayerPercent);
		if (expectedHumanCount <= 0) {
			expectedHumanCount = 1;
		}
		ShuffleArray(playerList);
		for (local i = 0; i < expectedHumanCount; i++) {
			local player = playerList[i];
			player_actual_team[player] <- humanTeamNumber;
		}
		for (local i = expectedHumanCount; i < playerCount; i++) {
			local player = playerList[i];
			player_actual_team[player] <- zombieTeamNumber;
		}

		// DEBUG: Force TEAM for human (non-bot) players
		// local forcedTeamNumber = humanTeamNumber;
		// for (local i = 1; i <= Constants.Server.MAX_PLAYERS; i++) {
		// 	local player = PlayerInstanceFromIndex(i)
		// 	if (player == null) {
		// 		continue;
		// 	}
		// 	local team = player.GetTeam();
		// 	if (!IsValidTeam(team)) {
		// 		// ignore other teams
		// 		continue;
		// 	}
		// 	if (!IsPlayerABot(player)) {
		// 		player_actual_team[player] <- forcedTeamNumber;
		// 	}
		// }

		// Do a few things...
		// - Inform players they were shuffled
		// - Force their class if invalid
		// - Force regen/respawn
		for (local i = 1; i <= Constants.Server.MAX_PLAYERS; i++) {
			local player = PlayerInstanceFromIndex(i)
			if (player == null || !player.IsValid()) {
				continue;
			}
			if (!(player in player_actual_team)) {
				// Should not be possible, but skip incase.
				printl("WARNING: unexpected case occurred. couldn't find team for player")
				continue;
			}
			local team = player_actual_team[player];
			// Swap to team
			if (player.GetTeam() != team) {
				player.ForceChangeTeam(team, false);
			}
			// Inform player of their set team and some rules
			switch (team) {
			case zombieTeamNumber: {
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Teams shuffled! You're on the zombie team!");
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Zombies can only be Scout, Heavy or Spy. You can change between these at any time.");
				break;
			}
			case humanTeamNumber: {
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Teams shuffled! You're on the survivor team!");
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Survivors can be Soldier, Pyro, Demoman, Engineer, Medic or Sniper. You can change your class during setup only.");
				break;
			}
			}
			// Force to valid class
			local allowedClassIndex = GetValidClass(player);
			if (allowedClassIndex != player.GetPlayerClass()) {
				ChangeClass(player, allowedClassIndex);
			}
			// note(jae): 2023-08-12
			// Force regen and respawn or else you can end up in the enemy spawn
			player.ForceRegenerateAndRespawn();
		}
	}
}

function OnGameEvent_player_builtobject(params) {
	if (!hasInitialized) {
		return;
	}
	// note(jae): 2023-08-06
	// From ZF Vanilla sourcemod, these are the mapping for "object"
	// #define PLAYERBUILTOBJECT_ID_DISPENSER  0
	// #define PLAYERBUILTOBJECT_ID_TELENT     1
	// #define PLAYERBUILTOBJECT_ID_TELEXIT    2
	// #define PLAYERBUILTOBJECT_ID_SENTRY     3
	local object = params.object;
	local index = params.index;

	// 1. Handle dispenser rules.
	//    Disable dispensers when they begin construction.
	//    Increase max health to 250 (default level 1 is 150).
	if(object == 0) {
		local building = EntIndexToHScript(index);
		NetProps.SetPropInt(building, "m_iMaxHealth", 250);
		// note(jae): 2023-08-09
		// Dispenser *seemingly* gets enabled after it's finished building, so lets
		// disable it every second for 15 seconds to ensure it stays disabled
		// after build. It shouldn't take longer than 15 seconds to build and if it builds
		// faster due to being wrenched, this should also immediately disable it.
		//
		// This also keeps it disabled after moving it.
		for (local delayInSeconds = 0; delayInSeconds < 15; delayInSeconds++) {
			EntFireByHandle(building, "Disable", "", delayInSeconds, null, null);
		}
	}
}

function OnGameEvent_player_death(params) {
	if (!hasInitialized) {
		return;
	}
	if(!("userid" in params) || params.userid == 0) {
		return;
	}
	local player = GetPlayerFromUserID(params.userid);
	if (player == null || !player.IsValid()) {
		return;
	}
	local team = player.GetTeam();
	if (!IsValidTeam(team)) {
		// ignore other teams
		return;
	}

	// If player team doesn't match actual team, force them back
	if (player in player_actual_team && team != player_actual_team[player]) {
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTCENTER, "Cannot change your team!");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Cannot change your team!");
		player.ForceChangeTeam(player_actual_team[player], false);
		return;
	}

	switch (team) {
	case humanTeamNumber:
		if (zf_roundState != ZF_ROUND_STARTED) {
			// If human died during setup, we just remove their ammo packs
			// and don't switch them to zombie.

			// Remove dropped ammopacks from humans during setup phase
			//
			// Mostly exists to telegraph to players that they can't spawn ammo packs
			// themselves in preparation for zombies.
			for (local ent = null; ent = Entities.FindByClassname(ent, "tf_ammo_pack");) {
				if (ent.GetOwner() == player) {
					ent.Kill();
				}
			}
			break;
		}

		// Force to zombie team
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTCENTER, "You've been zombified!");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 You've been zombified!");
		player_actual_team[player] <- zombieTeamNumber;

		// note(jae): 2023-08-11
		// Delay this so when you get turned into a zombie your corpse doesn't immediately
		// become the other teams color.
		EntFireByHandle(player, "RunScriptCode", "ChangeToActualTeam()", 0.0, null, null);
		break;
	case zombieTeamNumber:
		// Remove dropped ammopacks from zombies.
		for (local ent = null; ent = Entities.FindByClassname(ent, "tf_ammo_pack");) {
			if (ent.GetOwner() == player) {
				ent.Kill();
			}
		}
		break;
	}

	// Force remove zombie wearables
	//
	// note(jae): 2023-08-08
	// We do this here to resolve a bug where if you're a Heavy, then switch to Scout
	// it'll show the heavies zombie chest over the top of the Scout.
	//
	// note(jae): 2023-08-11
	// Couldn't get zombie skinning to work reliably, commented out
	// for (local wearable = player.FirstMoveChild(); wearable != null;) {
	// 	if (wearable.GetClassname() != "tf_wearable") {
	// 		wearable = wearable.NextMovePeer();
	// 		continue;
	// 	}
	// 	local current_wearable = wearable;
	// 	wearable = wearable.NextMovePeer();

	// 	// Destroy if wearing voodoo item
	// 	local itemId = NetProps.GetPropInt(current_wearable, "m_AttributeManager.m_Item.m_iItemDefinitionIndex");
	// 	if (itemId == 5617 || itemId == 5625 || itemId == 5618 || itemId == 5620 ||
	// 		itemId == 5622 || itemId == 5619 || itemId == 5624 || itemId == 5623 ||
	// 		itemId == 5621) {
	// 		current_wearable.Kill();
	// 	}
	// }
}

function OnGameEvent_teamplay_setup_finished(params) {
	if (!hasInitialized) {
		return;
	}
	if (zf_roundState == ZF_ROUND_WAITING ||
		zf_roundState == ZF_ROUND_STARTED) {
		// note(jae): 2023-08-12
		// If state is invalid tell the user what the cause might be
		// "mp_waitingforplayers_cancel 1" can break event firing.
		if (zf_roundState == ZF_ROUND_WAITING) {
			ClientPrint(null, Constants.EHudNotify.HUD_PRINTCENTER, "Game in broken state. Round start did not fire when expected.");
			ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Game in broken state. Round start did not fire when expected. This can occur if \"mp_waitingforplayers_cancel 1\" was called or used.");
			printl("Game in broken state. Round start did not fire when expected. This can occur if \"mp_waitingforplayers_cancel 1\" was called or used.");
		}
		// Do nothing if already triggered when setup finished
		// (or ignore if still in waiting period)
		return;
	}
	zf_roundState = ZF_ROUND_STARTED;

	//
	// Disable entities for regular maps (Control Point / Payload) after setup is done
	//

	// Disable all resupply cabinets.
	for (local ent = null; ent = Entities.FindByClassname(ent, "func_regenerate");) {
		EntFireByHandle(ent, "Disable", "", 0, null, null);
	}
	// Remove all dropped ammopacks.
	for (local ent = null; ent = Entities.FindByClassname(ent, "tf_ammo_pack");) {
		ent.Kill();
	}
	// Remove all ragdolls
	for (local ent = null; ent = Entities.FindByClassname(ent, "tf_ragdoll");) {
		ent.Kill();
	}
	// zombiefortress_vanilla avoiding disabling "func_respawnroomvisualizer" if it's a ZF map
	// so we do the same
	if (!is_zf_map) {
		for (local ent = null; ent = Entities.FindByClassname(ent, "func_respawnroomvisualizer");) {
			EntFireByHandle(ent, "Disable", "", 0, null, null);
		}
		// todo(jae): 2023-08-12
		// Ideally I only want this to affect spawn doors, if I can find if this door is close to
		// "func_respawnroomvisualizer" then I can open these better.
		//
		// Doing this blindly breaks Setup doors on cp_dustbowl stage 3
		// for (local ent = null; ent = Entities.FindByClassname(ent, "func_door");) {
		// 	EntFireByHandle(ent, "Open", "", 0, null, null);
		// 	EntFireByHandle(ent, "Disable", "", 0, null, null);
		// }
	}
	// Stop payload from healing and providing ammo
	for (local ent = null; ent = Entities.FindByClassname(ent, "mapobj_cart_dispenser");) {
		EntFireByHandle(ent, "Disable", "", 0, null, null);
	}

	// Tell human players they can no longer change class
	for (local i = 1; i <= Constants.Server.MAX_PLAYERS; i++) {
		local player = PlayerInstanceFromIndex(i)
		if (player == null) {
			continue;
		}
		local team = player.GetTeam();
		switch (team) {
		case humanTeamNumber:
			ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Grace period complete. Survivors can no longer change classes.");
			break;
		}
	}
}

function OnGameEvent_player_disconnect(params) {
	if (!hasInitialized) {
		return;
	}
	local player = GetPlayerFromUserID(params.userid);
	if (player == null || !player.IsValid()) {
		return;
	}
	if (player in zf_hasWelcomeMessage) {
		delete zf_hasWelcomeMessage[player];
	}
	if (player in zf_rageCantUseMessageTimer) {
		delete zf_rageCantUseMessageTimer[player];
	}
	if (player in player_actual_team) {
		delete player_actual_team[player];
	}
}

function OnGameEvent_player_spawn(params) {
	if (!hasInitialized) {
		return;
	}
	local player = GetPlayerFromUserID(params.userid);
	if (player == null || !player.IsValid()) {
		return;
	}
	local team = player.GetTeam();
	if (!IsValidTeam(team)) {
		// ignore other teams
		return;
	}

	// Send welcome message if we haven't done so yet
	//
	// note(jae): 2023-08-12
	// Do this here so any messages about your team being forced / class being forced
	// come after this.
	if (!(player in zf_hasWelcomeMessage) || !zf_hasWelcomeMessage[player]) {
		zf_hasWelcomeMessage[player] <- true;
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTCENTER, "Welcome to Zombie Fortress Classic!");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Welcome to Zombie Fortress Classic!");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Type !help to get more information");
		if (zf_debugDevMode) {
			ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF DEBUG MODE ENABLED]");
		}
	}

	// If no longer in waiting period, force player to team
	if (zf_roundState != ZF_ROUND_WAITING) {
		// If player spawned and...
		// - Has no assigned team, force onto zombie team
		// - Has changed to team they're not meant to be on, force back
		local hasTeam = (player in player_actual_team);
		if (!hasTeam || team != player_actual_team[player]) {
			if (!hasTeam) {
				// If game started, force player onto zombie team
				player_actual_team[player] <- zombieTeamNumber;

				ClientPrint(player, Constants.EHudNotify.HUD_PRINTCENTER, "Joined mid-game, you're on the zombie team!");
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Joined mid-game, you're on the zombie team!");
			} else {
				// Disallow team switching
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTCENTER, "Cannot change your team!");
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Cannot change your team!");
			}
			if (team != player_actual_team[player]) {
				player.ForceChangeTeam(player_actual_team[player], true);
				player.ForceRegenerateAndRespawn();
				return;
			}
		}
	}

	// Force to valid class
	local classIndex = player.GetPlayerClass();
	switch (team) {
	case zombieTeamNumber: {
		local isValidClass = (
			classIndex == Constants.ETFClass.TF_CLASS_SCOUT ||
			classIndex == Constants.ETFClass.TF_CLASS_HEAVYWEAPONS ||
			classIndex == Constants.ETFClass.TF_CLASS_SPY
		);
		if (!isValidClass) {
			local defaultZombieClass = Constants.ETFClass.TF_CLASS_SCOUT;
			local randomClassIndex = RandomInt(0, 2);
			switch (randomClassIndex) {
			case 0: defaultZombieClass = Constants.ETFClass.TF_CLASS_SCOUT; break;
			case 1: defaultZombieClass = Constants.ETFClass.TF_CLASS_HEAVYWEAPONS; break;
			case 2: defaultZombieClass = Constants.ETFClass.TF_CLASS_SPY; break;
			}

			ChangeClass(player, defaultZombieClass);

			ClientPrint(player, Constants.EHudNotify.HUD_PRINTCENTER, "Must select a valid zombie class: Scout, Heavy or Spy");
			ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Must select a valid zombie class: Scout, Heavy, Spy.");
			return;
		}
		break;
	}
	case humanTeamNumber: {
		local isValidClass = (
			classIndex == Constants.ETFClass.TF_CLASS_SOLDIER ||
			classIndex == Constants.ETFClass.TF_CLASS_PYRO ||
			classIndex == Constants.ETFClass.TF_CLASS_DEMOMAN ||
			classIndex == Constants.ETFClass.TF_CLASS_ENGINEER ||
			classIndex == Constants.ETFClass.TF_CLASS_MEDIC ||
			classIndex == Constants.ETFClass.TF_CLASS_SNIPER
		);
		if (!isValidClass) {
			local defaultHumanClass = Constants.ETFClass.TF_CLASS_PYRO;
			local randomClassIndex = RandomInt(0, 5);
			switch (randomClassIndex) {
			case 0: defaultHumanClass = Constants.ETFClass.TF_CLASS_SOLDIER; break;
			case 1: defaultHumanClass = Constants.ETFClass.TF_CLASS_PYRO; break;
			case 2: defaultHumanClass = Constants.ETFClass.TF_CLASS_DEMOMAN; break;
			case 3: defaultHumanClass = Constants.ETFClass.TF_CLASS_ENGINEER; break;
			case 4: defaultHumanClass = Constants.ETFClass.TF_CLASS_MEDIC; break;
			case 5: defaultHumanClass = Constants.ETFClass.TF_CLASS_SNIPER; break;
			}
			ChangeClass(player, defaultHumanClass);

			ClientPrint(player, Constants.EHudNotify.HUD_PRINTCENTER, "Must select a valid survivor class: Soldier, Pyro, Demoman, Engineer, Medic or Sniper");
			ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x05[ZF]\x01 Must select a valid zombie class: Soldier, Pyro, Demoman, Engineer, Medic or Sniper.");
			return;
		}
		break;
	}
	}

	// note(jae): 2023-08-12
	// Setup player stats after spawn such as:
	// - Movement speed changes
	//
	// As per TF2 Vscript documentation, we need to defer our spawn logic to happen
	// at the end of the frame by firing it with this.
	EntFireByHandle(player, "RunScriptCode", "SW_PostPlayerSpawn()", 0.0, null, null);
}

function OnGameEvent_post_inventory_application(params) {
	if (!hasInitialized) {
		return;
	}
	if(!("userid" in params) || params.userid == 0) {
		return;
	}
	local player = GetPlayerFromUserID(params.userid);
	if (player == null) {
		return;
	}
	local team = player.GetTeam();
	if (!IsValidTeam(team)) {
		// ignore other teams
		return;
	}
	switch (team) {
	case zombieTeamNumber:
		// Strip all weapons
		for (local i = 0; i < 7; i++) {
			local weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", i);
			if (weapon == null) {
				continue;
			}
			NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
			weapon.Kill();
		}

		// Force re-creation of melee weapon so it's the default
		local classIndex = player.GetPlayerClass();
		local meleeWeapon = null;
		switch (classIndex) {
		case Constants.ETFClass.TF_CLASS_SCOUT:
			// https://wiki.alliedmods.net/Team_fortress_2_item_definition_indexes#Melee_.5BSlot_2.5D
			meleeWeapon = GivePlayerWeapon(player, "tf_weapon_bat", 0);
			NetProps.SetPropEntityArray(player, "m_hMyWeapons", meleeWeapon, 2);
			break;
		case Constants.ETFClass.TF_CLASS_HEAVYWEAPONS:
			// https://wiki.alliedmods.net/Team_fortress_2_item_definition_indexes#Melee_.5BSlot_2.5D_5
			meleeWeapon = GivePlayerWeapon(player, "tf_weapon_fists", 5);
			NetProps.SetPropEntityArray(player, "m_hMyWeapons", meleeWeapon, 2);
			break;
		case Constants.ETFClass.TF_CLASS_SPY:
			// https://wiki.alliedmods.net/Team_fortress_2_item_definition_indexes#Melee_.5BSlot_2.5D_9
			local knifeWeapon = GivePlayerWeapon(player, "tf_weapon_knife", 4);
			NetProps.SetPropEntityArray(player, "m_hMyWeapons", knifeWeapon, 1); // melee slot is 1 for spy
			player.Weapon_Switch(knifeWeapon);

			// Bring back invisibility watch
			local cloakPDAWeapon = GivePlayerWeapon(player, "tf_weapon_invis", 30);
			NetProps.SetPropEntityArray(player, "m_hMyWeapons", cloakPDAWeapon, 4);

			// note(jae): 2023-08-10
			// Stopping the Spy from having a sapper need this delay for some reason.
			// My best guess is giving a knife or invis brings the sapper entity back?
			// In anycase, this works.
			EntFireByHandle(player, "RunScriptCode", "KillSapperWeapon()", 0.0, null, null);
			break;
		}
		if (meleeWeapon != null) {
			player.Weapon_Switch(meleeWeapon);
		}

		// note(jae): 2023-08-11
		// Couldn't get zombie skin working reliably, so not doing right now
		//EntFireByHandle(player, "RunScriptCode", "AttachVoodooWearable()", 0.0, null, null);
		break;
	case humanTeamNumber:
		local classIndex = player.GetPlayerClass();
		switch (classIndex) {
		case Constants.ETFClass.TF_CLASS_PYRO:
			// note(jae): 2023-08-11
			// This doesn't work because the games resupply cabinet will take
			// away the pyros flamethrower if we do this. So we just reduce the max ammo on tick.

			// Flamethrower / backburner ammo limited to 125 (down from 200)
			// local weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", 0);
			// if (weapon != null) {
			// 	weapon.AddAttribute("maxammo primary reduced", 125 / 200, -1);
			// 	weapon.ReapplyProvision();
			// 	NetProps.SetPropIntArray(player, "m_iAmmo", 125, 1);
			// }
			break;
		}

		// note(jae): 2023-08-11
		// Couldn't get zombie skin working reliably, so not doing right now
		//
		// Disable "Voodoo-Cursed Soul" skin for survivor players
		// NetProps.SetPropInt(player, "m_bForcedSkin", 0);
		// NetProps.SetPropInt(player, "m_nForcedSkin", 0);
		// player.RemoveCustomAttribute("player skin override");
		// player.RemoveCustomAttribute("zombiezombiezombiezombie");
		// player.RemoveCustomAttribute("SPELL: Halloween voice modulation");
		break;
	}
}

::PrintHelpClassInfo <- function(player) {
	local itemID = 0;
	switch (player.GetPlayerClass()) {
	case Constants.ETFClass.TF_CLASS_SCOUT:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Scout [Zombie]");
		// todo(jae): 2023-08-09
		// Allow other weapons, original redux copy says "Bats / Drinks / gullotine only"
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Weapon: Bats only.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Nerf: Speed reduced to 350 (from 400).");
		// todo(jae): 2023-08-09
		// Add speed buff, original redux copy says "Rage ability: Increase speed and health"
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Rage ability: Increase health by calling for medic. Recharges after 30s.");
		break;
	case Constants.ETFClass.TF_CLASS_SNIPER:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Sniper [Survivor/Support]");
		// todo(jae): 2023-08-09
		// Add 5 rifle ammo or 2 huntsman ammo per kill
		// ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Buffs: Gains 5 Rifle/2 Huntman ammo per kill.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Buff: SMGs don't have to reload.");
		break;
	case Constants.ETFClass.TF_CLASS_SOLDIER:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Soldier [Survivor/Assault]");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- No changes");
		// todo(jae): 2023-08-10
		// Add 2 rockets per kill
		//ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Buffs: Gains 2 rockets per kill.");
		break;
	case Constants.ETFClass.TF_CLASS_DEMOMAN:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Demoman [Survivor/Assault]");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- No changes");
		// todo(jae): 2023-08-10
		// Gains 2 pipes per kill.
		//ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Buffs: Gains 2 pipes per kill.");
		break;
	case Constants.ETFClass.TF_CLASS_MEDIC:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Medic [Survivor/Support]");
		DrawPanelText(panel, "- Buff: Syringe Guns don't have to reload. Uber charges faster, but doesn't last as long.");
		DrawPanelText(panel, "- Buff: Uber charges faster, but doesn't last as long. Overheal limited to 125% of max health and decays very slowly.");
	break;
	case Constants.ETFClass.TF_CLASS_HEAVYWEAPONS:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Heavy [Zombie]");
		// todo(jae): 2023-08-10
		// Allow food + gloves
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Weapon: Fists only.");
		// todo(jae): 2023-08-09
		// Add speed buff, original redux copy says "Rage ability: Increase speed and health"
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Rage ability: Increase health by calling for medic. Recharges after 30s.");
	break;
	case Constants.ETFClass.TF_CLASS_PYRO:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Pyro [Survivor/Assault]");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Nerf: Flamethrowers limited to 125 ammo.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Nerf: Speed decreased to 240 (from 300).");
		// todo(jae): 2023-08-10
		// Update to match ZF Redux, ie.
		// - give 15 flamethrower ammo per kill
		// - Nerf to 100 ammo
		// - Airblast buff, cost 1/2 normal ammo cost
		break;
	case Constants.ETFClass.TF_CLASS_SPY:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Spy [Zombie]");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Weapon: Knives and Invisibility Watch only.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Nerf: Speed reduced to 280 (from 300).");
		// todo(jae): 2023-08-09
		// Add speed buff, original redux copy says "Rage ability: Increase speed and health"
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Rage ability: Increase health by calling for medic. Recharges after 30s.");
		break;
	case Constants.ETFClass.TF_CLASS_ENGINEER:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Engineer [Survivor/Support]");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Nerf: Buildables can't be upgraded, but can be repaired.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Nerf: Sentry ammo limited to 60 and slowly decays. More ammo can't be added.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Nerf: Sentries self destruct when ammo is depleted.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Nerf: Dispenser doesn't heal or give ammo. Health increased to 250 (from 150).");
		break;
	default:
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76No class selected");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "Typing !help after selecting a class will give you more information on buffs / nerfs in this game mode");
		break;
	}
}

::HandleDebugChatCommands <- function(player, text) {
	if (!zf_debugDevMode) {
		return;
	}
	switch (text) {
	case "!dw": {
		// debug win
		// - will auto-win the game for the person using the command
		// - if round isn't started, cancel waiting for players
		if (zf_roundState == ZF_ROUND_WAITING) {
			// Skip waiting time
			Convars.SetValue("mp_waitingforplayers_cancel", "1");
			// note(jae): 2023-08-12
			// At time of writing calling "mp_waitingforplayers_cancel" will stop
			// OnGameEvent_teamplay_round_start from firing, so fire it manually.
			OnGameEvent_teamplay_round_start({
				full_reset = true,
			})
			break;
		}
		local game_round_win = SpawnEntityFromTable("game_round_win", {
			TeamNum=player.GetTeam(),
			force_map_reset=false,
			switch_teams=false
		})
		EntFireByHandle( game_round_win, "RoundWin", "", 0, null, null );
		break;
	}
	}
}

function OnGameEvent_player_say(params) {
	local player = GetPlayerFromUserID(params.userid);
	if (player == null) {
		return;
	}
	switch (params.text) {
	case "!help":
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Zombie Fortress Classic");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76(Ported by SilbinaryWolf, Originally by Sirot, Maintained by Dirtyminuth and ktm)");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- type \"!gamemode\" to get general information about the game mode");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- type \"!class\" to get more information about your classes buffs and nerfs");
		break;
	case "!gamemode":
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Zombie Team");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Classes: Scouts, Heavies, and Spies.");
		// todo(jae): 2023-08-09
		// Figure out how to do hording calculations
		// ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "They receive regeneration bonuses for sticking together as a horde.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Buffs: They receive automatic health regeneration.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Rage Ability: Rage is activated by calling for medic, which temporarily buffs you. Recharges after 30 seconds.");

		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF76Survivor Team");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Classes: Soldiers, Demomen, Pyros, Engineers, Medics, and Snipers.");
		ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "- Buffs: None.");
		// todo(jae): 2023-08-09
		// Figure out how to do morale boosts
		//ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "They receive morale boosts for multiple kills in a row. Morale boosts grant crit bonuses.");
		break;
	case "!class":
		PrintHelpClassInfo(player);
		break;
	default:
		if (zf_debugDevMode) {
			HandleDebugChatCommands(player, params.text);
		}
		break;
	}
}

SW_ZF_Init(this);
