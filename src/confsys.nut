// zf_config contains the configuration for the game mode
zf_config <- {};

::SW_ZF_UserConfigLoad <- function(zf_default_config) {
	local has_user_config = false;
	try {
		IncludeScript("sw_zombie_fortress/config/config.nut");
		has_user_config = true;
	} catch (e) {
		// Handle script doesn't exist case
		if (startswith(e, "Failed to include script")) {
			// fallthrough to "user configuration not found"
		} else {
			printl("[ZF] Error importing user configuration: " + e);
			throw e;
		}
	}
	if (!has_user_config) {
		return;
	}
	local user_config = null;
	try {
		// note(jae): 2023-08-13
		// Might pass in helper variables into this later such as
		// - Is current map ZF supported.
		// - The default config loaded.
		local reserved = {};
		user_config = SW_ZF_LoadUserConfig(reserved);
	} catch (e) {
		printl("[ZF] A scripting error occurred trying to load user config: " + e);
		throw e;
	}
	if (typeof user_config != "table") {
		printl("[ZF] Invalid user config, expected table of values but got: " + typeof user_config);
		return;
	}

	// Detect config version
	if (!("config_version" in user_config)) {
		printl("[ZF] Missing config_version in user configuration");
		return;
	}
	local config_version = user_config["config_version"];
	if (typeof config_version != "string") {
		printl("[ZF] Expected config version to be string");
		return;
	}
	if (config_version != "1") {
		printl("[ZF] Expected config version to equal \"1\", that's the only valid version right now");
		return;
	}
	delete user_config["config_version"];

	// Copy default config
	local merge_user_and_default_zf_config = {};
	foreach (k, v in zf_default_config) {
		merge_user_and_default_zf_config[k] <- v;
	}
	// Validate and apply user config to config
	local isInvalid = false;
	foreach (k, v in user_config) {
		if (!(k in zf_default_config)) {
			printl("[ZF] User config error: key \"" + k + "\" does not exist and is invalid");
			isInvalid = true;
			continue;
		}
		local actualType = typeof v;
		local expectedType = typeof zf_default_config[k];
		if (actualType != expectedType) {
			printl("[ZF] User config error: key \"" + k + "\" is invalid type. Expected " + expectedType + " but got " + actualType);
			isInvalid = true;
			continue;
		}
		merge_user_and_default_zf_config[k] = v;
	}
	if (isInvalid) {
		printl("[ZF] User configuration had errors. Not applying it");
		return;
	}
	return merge_user_and_default_zf_config;
}

::SW_ZF_ConfigLoad <- function() {
	// zf_default_config contains the configuration for the game mode
	local zf_default_config = {
		// config_version is the config format, this will allow us to be backwards compatible
		// with old config file formats.
		config_version = "1",
		// force_enabled will enable zombie fortress for all maps, not just zombie maps
		force_enabled = false,
		// survivor_team_ratio will make the survivor team have 65% of the player, and zombies
		// will have 35%.
		survivor_team_ratio = 0.65,
	};

	// Load user config
	local zf_user_config = null;
	try {
		zf_user_config = SW_ZF_UserConfigLoad(zf_default_config);
	} catch (e) {
		throw e;
	}
	if (typeof zf_user_config == "table") {
		zf_config = zf_user_config;
		printl("[ZF] Loaded user configuration");
	} else {
		zf_config = zf_default_config;
		printl("[ZF] No valid user configuration found, using default settings");
	}
}
