::SW_ZF_Main <- function() {
	if (zf_hasInitialized) {
		return;
	}
	IncludeScript("sw_zombie_fortress/src/confsys.nut");

	// Get mapname and remove "workshop/" prefix if it exists
	local mapname = GetMapName();
	if (startswith(mapname, "workshop/")) {
		mapname = mapname.slice("workshop/".len());
	}

	local is_zf_map = (
		startswith(mapname, "zf_") ||
		startswith(mapname, "zf2_") ||
		// Support Zombie Escape maps (untested)
		startswith(mapname, "ze_") ||
		// Support Super Zombie Fortress maps (untested)
		startswith(mapname, "szf_")
	);

	SW_ZF_ConfigLoad();

	local is_zf_enabled = is_zf_map || zf_config.force_enabled;
	if (!is_zf_enabled) {
		printl("[ZF] Zombie Fortress mode is *not* enabled for this map")
		return;
	}

	// Initialize game logic if enabled
	IncludeScript("sw_zombie_fortress/src/gamemode.nut");
	SW_ZF_Init(is_zf_map);
}
