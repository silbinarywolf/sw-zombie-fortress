::SW_ZOMBIE_FORTRESS <- { "Version" : "0.1.0" }

ClearGameEventCallbacks();

// Learning References
// - Functions: https://developer.valvesoftware.com/wiki/Team_Fortress_2/Scripting/Script_Functions
// - Constants: https://developer.valvesoftware.com/wiki/Team_Fortress_2/Scripting/Script_Functions/Constants
// - Events: https://wiki.alliedmods.net/Team_Fortress_2_Events
// - Example: https://developer.valvesoftware.com/wiki/Team_Fortress_2/Scripting/VScript_Examples/en#See_Also
// - Weapon IDs: https://wiki.alliedmods.net/Team_fortress_2_item_definition_indexes
// - VScript Libs: https://developer.valvesoftware.com/wiki/List_of_Script_Libraries
// - Gamebanana Vscript mods: https://gamebanana.com/mods/cats/20983
// - AddCondEx cheatsheet: https://wiki.teamfortress.com/wiki/Cheats
//
// - Test map: changelevel workshop/2926638864

hasInitialized <- false;

is_zf_map <- false;

humanTeamNumber <- Constants.ETFTeam.TF_TEAM_RED;

zombieTeamNumber <- Constants.ETFTeam.TF_TEAM_BLUE;

// Percentage of players that start as survivors, must have at least 1 survivor
survivorPlayerPercent <- 0.65;

// This is set to true once setup ends
hasSetupEnded <- false;

// player_start_team maps a player ID to their start team
//
// This is used to stop players changing their teams during setup, mostly
// motivation to stop bots doing it
player_start_team <- {};

// zf_rageTimer maps to a player ID, if its above zero, the player cannot use "Call for medic"
// to get a health boost
zf_rageTimer <- {};

// zf_rageCantUseMessageTimer maps to a player ID, if its above zero we avoid showing the player
// a warning about rage
zf_rageCantUseMessageTimer <- {};

// tf_gamerules holds the tf_gamerules game objects after the round starts
tf_gamerules <- null;

// ::CTFGameRules.CanChangeTeam <- function(iCurrentTeam) {
// 	return false;
// }

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
	// Unable to get this to work...
	if (zombieTeamNumber == Constants.ETFTeam.TF_TEAM_BLUE) {
		NetProps.SetPropFloat(tf_gamerules, "SetBlueTeamRespawnWaveTime", time);
		EntFireByHandle(tf_gamerules, "SetBlueTeamRespawnWaveTime", time.tostring(), 0, null, null);
	} else {
		NetProps.SetPropFloat(tf_gamerules, "SetRedTeamRespawnWaveTime", time);
		EntFireByHandle(tf_gamerules, "SetRedTeamRespawnWaveTime", time.tostring(), 0, null, null);
	}
	printl("-- Updated Zombie spawn time (logic doesnt work yet for some reason)")
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
			ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "[ZF] Rage is ready! (Call for medic for health boost)");
		}
	}
	if (player in zf_rageCantUseMessageTimer && zf_rageCantUseMessageTimer[player] > 0) {
		zf_rageCantUseMessageTimer[player]--;
	}

	// Activate rage
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
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "[ZF] Rage activated!");
			} else {
				// Avoid showing same message per tick
				if (!isBot && (!(player in zf_rageCantUseMessageTimer) || zf_rageCantUseMessageTimer[player] == 0)) {
					ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "[ZF] Cannot activate rage, must be at least full health");
					zf_rageCantUseMessageTimer[player] <- 5;
				}
			}
		} else {
			// Avoid showing same message per tick
			if (!isBot && (!(player in zf_rageCantUseMessageTimer) || zf_rageCantUseMessageTimer[player] == 0)) {
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "[ZF] Cannot activate rage, waiting " + rageTimer + " more second(s) for cooldown");
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
				// Destroy
				ent.SetHealth(0);
			}
		}
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

function Init() {
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

	// debug(jae): Testing ZF
	// FORCE ON ALWAYS
	local is_zf_enabled = true;

	if (!is_zf_enabled) {
		return;
	}

	// DEBUG
	printl("-- Init ZF");

	// In regular map, blue is attacking, so swap the teams
	local is_control_point_map = (
		startswith(mapname, "cp_") ||
		startswith(mapname, "workshop/cp_")
	);
	if (is_control_point_map) {
		// Check that defending control points are for RED team, if they are
		// then swap the zombie teams
		local control_point_count = 0;
		local red_control_point_count = 0;
		for (local ent = null; ent = Entities.FindByClassname(ent, "team_control_point");) {
			local team = NetProps.GetPropInt(ent, "m_iTeamNum");
			control_point_count++;
			if (team == Constants.ETFTeam.TF_TEAM_RED) {
				red_control_point_count++;
			}
		}
		if (control_point_count == red_control_point_count) {
			humanTeamNumber = Constants.ETFTeam.TF_TEAM_BLUE;
			zombieTeamNumber = Constants.ETFTeam.TF_TEAM_RED;
		}
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
	Convars.SetValue("weapon_medigun_charge_rate", 30); // Locked
	Convars.SetValue("weapon_medigun_chargerelease_rate", 6); // Locked
	Convars.SetValue("tf_max_health_boost", 1.25); // Locked, Default 1.5
	Convars.SetValue("tf_boost_drain_time", 3600); // Locked, Default 15.0, Time it takes for a health boost to degrade.

	// Spy
	Convars.SetValue("tf_spy_invis_time", 0.5);
	Convars.SetValue("tf_spy_invis_unstealth_time", 0.75);
	Convars.SetValue("tf_spy_cloak_no_attack_time", 1.0);

	// Effectively makes cloak 20% smaller.
	Convars.SetValue("tf_spy_cloak_consume_rate", 12.5); // Default: 10, consumes 10 per second out of 100.

	hasInitialized = true;
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
	NetProps.SetPropInt(weapon, "m_AttributeManager.m_Item.m_iEntityLevel", 0);
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

	//player.SetForcedTauntCam(1); // enable third person
	//player.SetCustomModelVisibleToSelf(true); // show model of self
}

::ChangeClass <- function(player, classIndex) {
	if (player == null || !player.IsValid()) {
		return;
	}
	player.SetPlayerClass(classIndex);
	NetProps.SetPropInt(player, "m_PlayerClass.m_iClass", classIndex);

	// note(jae): 2023-08-05
	// Not using Regenerate and respawn as that doesn't work for only server-side scripts.
	// Might work if embedded in a map
	player.SetCustomModel("");
	player.SetHealth(player.GetMaxHealth());
	player.Regenerate(true);
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

// ::PostClassChangeZombieInvalid <- function() {
// 	local player = self;
// 	if (player == null || !player.IsValid()) {
// 		return;
// 	}
// 	// local defaultZombieClass = Constants.ETFClass.TF_CLASS_SCOUT;
// 	// NetProps.SetPropInt(player, "m_PlayerClass.m_iClass", defaultZombieClass);
// 	// NetProps.SetPropInt(player, "m_PlayerClass.m_iDesiredPlayerClass", defaultZombieClass);
// 	// player.SetPlayerClass(defaultZombieClass);
// 	// player.ForceRegenerateAndRespawn();

// 	ClientPrint(player, Constants.EHudNotify.HUD_PRINTCENTER, "Must select a valid zombie class: Scout, Heavy or Spy");
// 	ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "[ZF] Valid zombies: Scout, Heavy, Spy.");
// }

function OnGameEvent_teamplay_round_start(params) {
	if (!hasInitialized) {
		return;
	}
	printl("-- Round Start -- ");

	// reset globals
	hasSetupEnded = false;
	player_start_team = {};
	zf_rageTimer = {};
	zf_rageCantUseMessageTimer = {};
	tf_gamerules = null;

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

			player_start_team[player] <- humanTeamNumber;
			player.SetTeam(humanTeamNumber);
		}
		for (local i = expectedHumanCount; i < playerCount; i++) {
			local player = playerList[i];

			player_start_team[player] <- zombieTeamNumber;
			player.SetTeam(zombieTeamNumber);
		}

		// DEBUG: Force TEAM on any human
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
		// 		player_start_team[player] <- zombieTeamNumber;
		// 		player.SetTeam(zombieTeamNumber);
		// 	}
		// }
	}

	// Add our "::TickEverySecond" function to team_round_timer
	local team_round_timer = Entities.FindByClassname(null, "team_round_timer");
	if (team_round_timer == null) {
		printl("Team round timer not found");
		return;
	}
	team_round_timer.ValidateScriptScope();
	AddThinkToEnt(team_round_timer, "TickEverySecond");

	// Update game rules
	//
	// note(jae): 2023-08-06
	// This seemingly does nothing, at least nothing visible on client.
	tf_gamerules = Entities.FindByClassname(null, "tf_gamerules");
	if (tf_gamerules != null && tf_gamerules.ValidateScriptScope()) {
		// tf_gamerules.GetScriptScope().CanPlayerChooseClass <- function(player, classIndex) {
		// 	printl("invalid class!!!")
		// 	return false;
		// }

		// note(jae): 2023-08-06
		// Currently this doesn't seem to work or do anything.
		local humanGoal = "Survive the zombies";
		local zombieGoal = "Kill all survivors";
		if (humanTeamNumber == Constants.ETFTeam.TF_TEAM_RED) {
			NetProps.SetPropString(tf_gamerules, "SetBlueTeamGoalString", zombieGoal);
			NetProps.SetPropString(tf_gamerules, "SetRedTeamGoalString", humanGoal);
		} else {
			NetProps.SetPropString(tf_gamerules, "SetBlueTeamGoalString", humanGoal);
			NetProps.SetPropString(tf_gamerules, "SetRedTeamGoalString", zombieGoal);
		}
		//EntFireByHandle(tf_gamerules, "SetBlueTeamGoalString", zombieGoal, 0, null, null);
		//EntFireByHandle(tf_gamerules, "SetRedTeamGoalString", humanGoal, 0, null, null);

		// Setup bot classes
		// local humanBotRoster = null;
		// local zombieBotRoster = null;
		// for (local ent = null; ent = Entities.FindByClassname(ent, "bot_roster");) {
		// 	// TODO: find existing for hammer maps with this
		// 	break;
		// }
		// if (humanBotRoster == null) {
		// 	humanBotRoster = SpawnEntityFromTable("bot_roster",{});
		// 	humanBotRoster.ValidateScriptScope();
		// 	humanBotRoster.SetTeam(Constants.ETFTeam.TF_TEAM_BLUE);
		// 	NetProps.SetPropString(humanBotRoster, "m_teamName", "blue");
		// }
		// if (zombieBotRoster == null) {
		// 	zombieBotRoster = SpawnEntityFromTable("bot_roster",{});
		// 	zombieBotRoster.ValidateScriptScope();
		// 	zombieBotRoster.SetTeam(Constants.ETFTeam.TF_TEAM_RED);
		// 	NetProps.SetPropString(zombieBotRoster, "m_teamName", "red");
		// }
		// local humanBotRoster = NetProps.GetPropEntity(tf_gamerules, "m_hBlueBotRoster");
		// if (humanBotRoster != null) {
		// 	printl("-- HAS BLUE BOT ROSTER")

		// 	// Setup human bot classes
		// 	NetProps.SetPropBoolArray(humanBotRoster, "m_bAllowedClasses", true, Constants.ETFClass.TF_CLASS_SOLDIER);
		// 	NetProps.SetPropBoolArray(humanBotRoster, "m_bAllowedClasses", true, Constants.ETFClass.TF_CLASS_PYRO);
		// 	NetProps.SetPropBoolArray(humanBotRoster, "m_bAllowedClasses", true, Constants.ETFClass.TF_CLASS_DEMOMAN);
		// 	NetProps.SetPropBoolArray(humanBotRoster, "m_bAllowedClasses", true, Constants.ETFClass.TF_CLASS_ENGINEER);
		// 	NetProps.SetPropBoolArray(humanBotRoster, "m_bAllowedClasses", true, Constants.ETFClass.TF_CLASS_MEDIC);
		// 	NetProps.SetPropBoolArray(humanBotRoster, "m_bAllowedClasses", true, Constants.ETFClass.TF_CLASS_SNIPER);

		// 	NetProps.SetPropBoolArray(humanBotRoster, "m_bAllowedClasses", false, Constants.ETFClass.TF_CLASS_SCOUT);
		// 	NetProps.SetPropBoolArray(humanBotRoster, "m_bAllowedClasses", false, Constants.ETFClass.TF_CLASS_HEAVYWEAPONS);
		// 	NetProps.SetPropBoolArray(humanBotRoster, "m_bAllowedClasses", false, Constants.ETFClass.TF_CLASS_SPY);
		// } else {
		// 	printl("-- NULL BLUE BOT ROSTER")
		// }
		// local zombieBotRoster = NetProps.GetPropEntity(tf_gamerules, "m_hRedBotRoster");
		// if (zombieBotRoster != null) {
		// 	printl("-- HAS RED BOT ROSTER")

		// 	// Setup zombie bot classes
		// 	NetProps.SetPropBoolArray(zombieBotRoster, "m_bAllowedClasses", true, Constants.ETFClass.TF_CLASS_SCOUT);
		// 	NetProps.SetPropBoolArray(zombieBotRoster, "m_bAllowedClasses", true, Constants.ETFClass.TF_CLASS_HEAVYWEAPONS);
		// 	NetProps.SetPropBoolArray(zombieBotRoster, "m_bAllowedClasses", true, Constants.ETFClass.TF_CLASS_SPY);

		// 	NetProps.SetPropBoolArray(zombieBotRoster, "m_bAllowedClasses", false, Constants.ETFClass.TF_CLASS_SOLDIER);
		// 	NetProps.SetPropBoolArray(zombieBotRoster, "m_bAllowedClasses", false, Constants.ETFClass.TF_CLASS_PYRO);
		// 	NetProps.SetPropBoolArray(zombieBotRoster, "m_bAllowedClasses", false, Constants.ETFClass.TF_CLASS_DEMOMAN);
		// 	NetProps.SetPropBoolArray(zombieBotRoster, "m_bAllowedClasses", false, Constants.ETFClass.TF_CLASS_ENGINEER);
		// 	NetProps.SetPropBoolArray(zombieBotRoster, "m_bAllowedClasses", false, Constants.ETFClass.TF_CLASS_MEDIC);
		// 	NetProps.SetPropBoolArray(zombieBotRoster, "m_bAllowedClasses", false, Constants.ETFClass.TF_CLASS_SNIPER);
		// } else {
		// 	printl("-- NULL RED BOT ROSTER")
		// }
	}

	SetZombieSpawnTime(8.0);
}

function OnGameEvent_player_builtobject(params) {
	if (!hasInitialized) {
		return;
	}
	local index = params.index;
	// note(jae): 2023-08-06
	// From ZF Vanilla sourcemod, these are the mapping for "object"
	// #define PLAYERBUILTOBJECT_ID_DISPENSER  0
	// #define PLAYERBUILTOBJECT_ID_TELENT     1
	// #define PLAYERBUILTOBJECT_ID_TELEXIT    2
	// #define PLAYERBUILTOBJECT_ID_SENTRY     3
	local object = params.object;

	// 1. Handle dispenser rules.
	//    Disable dispensers when they begin construction.
	//    Increase max health to 250 (default level 1 is 150).
	if(object == 0) {
		NetProps.SetPropInt(index, "m_bDisabled", 1);
		NetProps.SetPropInt(index, "m_iMaxHealth", 250);
		EntFireByHandle(ent, "Disable", "", 0, null, null);
	}
}

function OnGameEvent_player_calledformedic(params) {
	if (!hasInitialized) {
		return;
	}
	printl("CALLED FOR MEDIC")
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
	switch (GetRoundState()) {
	case Constants.ERoundState.GR_STATE_RND_RUNNING:
		switch (team) {
		case humanTeamNumber:
			// Do nothing if human died during setup
			if (!hasSetupEnded) {
				// Remove dropped ammopacks from humans during setup phase
				//
				// Mostly exists to telegraph to players that they can't spawn ammo packs
				// themselves in preparation for zombies.
				local ent = null;
				while (ent = Entities.FindByClassname(ent, "tf_ammo_pack")) {
					if (ent.GetOwner() == player) {
						ent.Kill();
					}
				}
				break;
			}

			// Force to zombie team
			player.SetTeam(zombieTeamNumber);

			// Get human count
			local humanCount = 0;
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
				switch (team) {
				case humanTeamNumber:
					humanCount++;
					break;
				}
			}
			if (humanCount == 0) {
				local zombiewin = SpawnEntityFromTable("game_round_win", {
					TeamNum=zombieTeamNumber,
					force_map_reset=true,
					switch_teams=false
				})
				EntFireByHandle( zombiewin, "RoundWin", "", 0, null, null );
			}
			break;
		case zombieTeamNumber:
			// Remove dropped ammopacks from zombies.
			local ent = null;
			while (ent = Entities.FindByClassname(ent, "tf_ammo_pack")) {
				if (ent.GetOwner() == player) {
					ent.Kill();
				}
			}
			break;
		}
		break;
	}

	// Force player back to their original team
	if (!hasSetupEnded) {
		if (player in player_start_team && team != player_start_team[player]) {
			player.SetTeam(player_start_team[player]);
		}
		return;
	}
}

function OnGameEvent_teamplay_setup_finished(params) {
	if (!hasInitialized) {
		return;
	}
	if (hasSetupEnded == false) {
		hasSetupEnded = true;

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
			// note(jae): 2023-08-05
			// Something I added but likely not necessary. zombiefortress_vanilla.sp doesnt do this
			// but we do as people can hide in spawn
			for (local ent = null; ent = Entities.FindByClassname(ent, "func_door");) {
				EntFireByHandle(ent, "Open", "", 0, null, null);
			}
		}
		// todo(jae): 2023-08-05
		// Disable payload dispenser (test this logic)
		//for (local ent = null; ent = Entities.FindByClassname(ent, "mapobj_cart_dispenser");) {
		//	NetProps.SetEntBool(ent, "m_bDisabled", true);
		//}

		// Tell human players they can no longer change class
		for (local i = 1; i <= Constants.Server.MAX_PLAYERS; i++) {
			local player = PlayerInstanceFromIndex(i)
			if (player == null) {
				continue;
			}
			local team = player.GetTeam();
			if (!IsValidTeam(team)) {
				// ignore other teams
				return;
			}
			switch (team) {
			case humanTeamNumber:
				ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "[ZF] Grace period complete. Survivors can no longer change classes.");
				break;
			}
		}
	}
}

function OnGameEvent_player_spawn(params) {
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

	// If setup ended and player spawned, force onto zombie team
	if (hasSetupEnded) {
		switch (GetRoundState()) {
		case Constants.ERoundState.GR_STATE_RND_RUNNING:
			switch (team) {
			case humanTeamNumber:
				// If game started, force player onto zombie team
				player.SetTeam(zombieTeamNumber);
				player_start_team[player] <- zombieTeamNumber;
				return;
			}
			break;
		}
	}

	// Force to valid class
	local classIndex = player.GetPlayerClass();
	switch (team) {
	case zombieTeamNumber:
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
			ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "[ZF] Valid zombie classes: Scout, Heavy, Spy.");
			return;
		}
		break;
	case humanTeamNumber:
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
			ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "[ZF] Valid survivor classes: Soldier, Pyro, Demoman, Engineer, Medic, Sniper.");
			return;
		}
		break;
	}

	// As per TF2 Vscript documentation, we need to defer our spawn logic to happen
	// at the end of the frame by firing it with this.
	//
	// Otherwise properties/attributes/etc wont apply.
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

	// DEBUG
	// printl("-- Player Post Inventory --")

	switch (team) {
	case zombieTeamNumber:
		// Strip all weapons
		for (local i = 0; i < 7; i++) {
			local weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", i);
			if (weapon == null) {
				continue;
			}
			weapon.Destroy();
		}

		// Force re-creation of melee weapon so it's the default
		local classIndex = player.GetPlayerClass();
		local meleeWeapon = null;
		switch (classIndex) {
		case Constants.ETFClass.TF_CLASS_SCOUT:
			// https://wiki.alliedmods.net/Team_fortress_2_item_definition_indexes#Melee_.5BSlot_2.5D
			meleeWeapon = GivePlayerWeapon(player, "tf_weapon_bat", 0);
			break;
		case Constants.ETFClass.TF_CLASS_HEAVYWEAPONS:
			// https://wiki.alliedmods.net/Team_fortress_2_item_definition_indexes#Melee_.5BSlot_2.5D_5
			meleeWeapon = GivePlayerWeapon(player, "tf_weapon_fists", 5);
			break;
		case Constants.ETFClass.TF_CLASS_SPY:
			// https://wiki.alliedmods.net/Team_fortress_2_item_definition_indexes#Melee_.5BSlot_2.5D_9
			meleeWeapon = GivePlayerWeapon(player, "tf_weapon_knife", 4);

			// Bring back invisibility watch
			local cloakPDAWeapon = GivePlayerWeapon(player, "tf_weapon_invis", 30);
			NetProps.SetPropEntityArray(player, "m_hMyWeapons", cloakPDAWeapon, 4);
			break;
		}
		if (meleeWeapon != null) {
			NetProps.SetPropEntityArray(player, "m_hMyWeapons", meleeWeapon, 2);
			player.Weapon_Switch(meleeWeapon);
		}

		// Enable "Voodoo-Cursed Soul" skin
		// Source: https://forums.alliedmods.net/showthread.php?t=232434
		// meleeWeapon.AddAttribute("player skin override", 1.0, -1);
		// meleeWeapon.AddAttribute("zombiezombiezombiezombie", 1.0, -1);
		// meleeWeapon.AddAttribute("SPELL: Halloween voice modulation", 1.0, -1);
		// SetPropInt(player, "m_bForcedSkin", 1);
		// SetPropInt(player, "m_nForcedSkin", 5);
		break;
	case humanTeamNumber:
		local classIndex = player.GetPlayerClass();
		switch (classIndex) {
		case Constants.ETFClass.TF_CLASS_PYRO:
			// note(jae): 2023-08-06
			// Doesn't work here, so we do it on tick.

			// Flamethrower / backburner ammo limited to 125.
			// local weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", 0);
			// local ammoType = weapon.GetPrimaryAmmoType();
			// NetProps.SetPropIntArray(player, "m_iAmmo", 125, 1);
			// NetProps.SetPropIntArray(player, "m_PlayerClass.m_aAmmoMax", 125, 1);
			// printl("update pyro weapon")
			break;
		}

		// todo(jae): 2023-08-06
		// Disable this for human
		//player.RemoveAttribute("zombiezombiezombiezombie");
		//player.RemoveAttribute("SPELL: Halloween voice modulation");
		break;
	}

	// Strip wearables
	//
	// note(jae): 2023-08-06
	// For personal reference back when I tried porting Prophunt first
	// for (local wearable = player.FirstMoveChild(); wearable != null;) {
	// 	if (wearable.GetClassname() != "tf_wearable") {
	// 		wearable = wearable.NextMovePeer();
	// 		continue;
	// 	}
	// 	local current_wearable = wearable;
	// 	wearable = wearable.NextMovePeer();

	// 	current_wearable.Destroy();
	// }
}

// function OnGameEvent_player_changeclass(params) {
// 	if (!hasInitialized) {
// 		return;
// 	}
// 	if(!("userid" in params) || params.userid == 0) {
// 		return;
// 	}
// 	local player = GetPlayerFromUserID(params.userid);
// 	if (player == null || !player.IsValid()) {
// 		return;
// 	}
// 	local team = player.GetTeam();
// 	if (!IsValidTeam(team)) {
// 		// ignore other teams
// 		return;
// 	}
// }

// function OnGameEvent_item_pickup(params) {
// 	if (!hasInitialized) {
// 		return;
// 	}
// 	if(!("userid" in params) || params.userid == 0) {
// 		return;
// 	}
// 	// Disallow weapon pickups
// 	local ent = null
// 	ent = GetPlayerFromUserID(params.userid)
// 	local classname = "weapon_" + params["item"];
// 	RemovePlayerWeapon(ent, classname);
// }

Init();
__CollectGameEventCallbacks(this);
