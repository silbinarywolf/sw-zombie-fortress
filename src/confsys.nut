// zf_config contains the configuration for the game mode
zf_config <- {};

::SW_ZF_MergeConfigLoad <- function(base_config, override_config) {
	// Detect config version
	if (!("config_version" in override_config)) {
		return {
			is_error = true,
			errors = ["missing config_version"],
		};
	}
	local config_version = override_config["config_version"];
	if (typeof config_version != "string") {
		return {
			is_error = true,
			errors = ["expected config_version to be string"],
		};
	}
	if (config_version != "1") {
		return {
			is_error = true,
			errors = ["expected config_version to be equal \"1\", that's the only valid version right now"],
		};
	}
	delete override_config["config_version"];

	// Copy base config
	local merged_config = {};
	foreach (k, v in base_config) {
		merged_config[k] <- v;
	}

	// Validate and apply override config
	local errors = [];
	foreach (k, v in override_config) {
		if (!(k in base_config)) {
			errors.push("key \"" + k + "\" does not exist and is invalid");
			continue;
		}
		local actualType = typeof v;
		local expectedType = typeof base_config[k];
		if (actualType != expectedType) {
			errors.push("key \"" + k + "\" is invalid type. expected " + expectedType + " but got " + actualType);
			continue;
		}
		merged_config[k] = v;
	}
	if (errors.len() > 0) {
		return {
			is_error = true,
			errors = errors,
		};
	}
	return merged_config;
}

::SW_ZF_UserConfigLoad <- function(zf_default_config) {
	local has_config = false;
	try {
		IncludeScript("sw_zombie_fortress/config/config.nut");
		has_config = true;
	} catch (e) {
		// Handle script doesn't exist case
		if (startswith(e, "Failed to include script")) {
			// fallthrough to "user configuration not found"
		} else {
			printl("[ZF] Error importing user configuration: " + e);
			throw e;
		}
	}
	if (!has_config) {
		return false;
	}
	local config = null;
	try {
		// note(jae): 2023-08-13
		// Might pass in helper variables into this later such as
		// - Is current map ZF supported.
		// - The default config loaded.
		local reserved = {};
		config = SW_ZF_LoadUserConfig(reserved);
	} catch (e) {
		printl("[ZF] A scripting error occurred trying to load user config: " + e);
		throw e;
	}
	if (typeof config != "table") {
		return {
			is_error = true,
			errors = ["expected table of values but got: " + typeof config],
		};
	}
	local error_or_merged_config = SW_ZF_MergeConfigLoad(zf_default_config, config);
	return error_or_merged_config;
}

::SW_ZF_DefaultMapConfigLoad <- function(mapname, base_config) {
	local has_config = false;
	try {
		IncludeScript("sw_zombie_fortress/src/mapconf/"+mapname+".nut");
		has_config = true;
	} catch (e) {
		// Handle script doesn't exist case
		if (startswith(e, "Failed to include script")) {
			// fallthrough to "plugin map configuration not found"
		} else {
			printl("[ZF] Error importing plugin map configuration: " + e);
			throw e;
		}
	}
	if (!has_config) {
		return false;
	}
	local config = null;
	try {
		// note(jae): 2023-08-19
		// SW_ZF_LoadDefaultMapConfig won't support passing in other props by default
		// as they're map specific.
		config = SW_ZF_LoadDefaultMapConfig();
	} catch (e) {
		printl("[ZF] A scripting error occurred trying to load plugin map config: " + e);
		throw e;
	}
	if (typeof config != "table") {
		return {
			is_error = true,
			errors = ["expected table of values but got: " + typeof config],
		};
	}
	local error_or_merged_config = SW_ZF_MergeConfigLoad(base_config, config);
	return error_or_merged_config;
}

// SW_ZF_ParseMapBuiltin parses "Response Contexts" from the worldspawn entity (ie. map properties in Hammer)
//
// This feature is new to the Zombie Fortress Classic VScript rewrite (2023).
// ie. "zf_survivor_team_ratio:0.7;zf_other_future_setting:3"
::SW_ZF_ParseMapBuiltin <- function(responseContext) {
	// Setup valid configurables for map
	//
	// note: the values here aren't used for anything but
	// 		 to determine their type
	local base_hammer_config = {
		zf_survivor_team_ratio = 0.65,
	};

	local found_at_least_one_zf_key = false;
	local hammer_config = {};
	local errors = [];
	local keyValuesList = split(responseContext, ",");
	foreach (keyValueAsString in keyValuesList) {
		// ignore any keys that don't begin with zf_
		if (!startswith(keyValueAsString, "zf_")) {
			continue;
		}
		local keyAndValue = split(keyValueAsString, ":");
		if (keyAndValue.len() != 2) {
			if (keyAndValue.len() == 1) {
				errors.push("missing : for key: " + keyValueAsString);
				continue;
			}
			if (keyAndValue.len() > 2) {
				errors.push("too many \":\" for key: " + keyValueAsString);
				continue;
			}
			errors.push("invalid for key: " + keyValueAsString);
			continue;
		}
		local key = keyAndValue[0];
		if (!(key in base_hammer_config)) {
			errors.push("ignoring invalid key: " + key + ", possible typo?");
			continue;
		}
		found_at_least_one_zf_key = true;
		local originalValue = base_hammer_config[key];
		local valueAsString = keyAndValue[1];
		switch (typeof originalValue) {
		case "float": {
			local value = 0;
			try {
				value = valueAsString.tofloat();
			} catch (e) {
				errors.push("ignoring invalid value: " + key + ". expected valid number but got: " + valueAsString);
				continue;
			}
			hammer_config[key] <- value;
			break;
		}
		default:
			errors.push("unhandled type: "+typeof originalValue+" for key: " + key + ", ");
			break;
		}
	}
	if (!found_at_least_one_zf_key) {
		return false;
	}
	if (errors.len() > 0) {
		hammer_config["warnings"] <- errors;
	}
	return hammer_config;
}

::SW_ZF_ConfigLoad <- function(mapname) {
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

	// Use default config as the base
	local zf_new_config = zf_default_config;

	// Load built-in settings if map defined them in Hammer
	{
		local worldspawn = Entities.FindByClassname(null, "worldspawn");
		if (worldspawn != null) {
			local responseContext = NetProps.GetPropString(worldspawn, "m_iszResponseContext");
			responseContext = strip(responseContext);
			if (responseContext != "") {
				local hammer_config = SW_ZF_ParseMapBuiltin(responseContext);
				switch (typeof hammer_config) {
				case "table": {
					printl("[ZF] Map built-in configuration");
					if ("warnings" in hammer_config) {
						// Print each warning
						printl("     - Warnings:");
						foreach (error in hammer_config.warnings) {
							printl("      -- " + error);
						}
					}
					// Track changes and tell the user in console what changes took
					local values_changed = [];
					if ("zf_survivor_team_ratio" in hammer_config) {
						zf_new_config.survivor_team_ratio = hammer_config.zf_survivor_team_ratio;
						values_changed.push("survivor_team_ratio: " + zf_new_config.survivor_team_ratio);
					}
					if (values_changed.len() > 0) {
						printl("     - Values changed:");
						foreach (changes in values_changed) {
							printl("      -- " + changes);
						}
					}
					break;
				}
				default:
					printl("[ZF] Map has no builtin config: " + mapname);
					break;
				}
			}
		}
	}

	// Load plugin map config (if exists)
	{
		local zf_map_config = null;
		try {
			zf_map_config = SW_ZF_DefaultMapConfigLoad(mapname, zf_new_config);
		} catch (e) {
			throw e;
		}
		switch (typeof zf_map_config) {
		case "table":
			if ("is_error" in zf_map_config && "errors" in zf_map_config) {
				// Print each error
				printl("[ZF] Invalid plugin map configuration ("+mapname+") with errors:");
				foreach (error in zf_map_config.errors) {
					printl("- " + error);
				}
				break;
			}
			// If plugin map config is valid, update config to use that.
			zf_new_config = zf_map_config;
			printl("[ZF] Loaded plugin map config: " + mapname);
			break;
		default:
			if (zf_map_config == false) {
				// If no plugin map config exists, do nothing
				break;
			}
			// If an unexpected error occurs
			printl("[ZF] Invalid plugin map configuration found: " + mapname);
			break;
		}
	}

	// Load user config
	{
		local zf_user_config = null;
		try {
			zf_user_config = SW_ZF_UserConfigLoad(zf_new_config);
		} catch (e) {
			throw e;
		}
		switch (typeof zf_user_config) {
		case "table":
			if ("is_error" in zf_user_config && "errors" in zf_user_config) {
				// Print each error
				printl("[ZF] Invalid user configuration with errors:");
				foreach (error in zf_user_config.errors) {
					printl("- " + error);
				}
				break;
			}
			// If user config is valid, update config to use that.
			zf_new_config = zf_user_config;
			printl("[ZF] Loaded user configuration");
			break;
		default:
			if (zf_user_config == false) {
				// If no user config exists
				printl("[ZF] No user configuration found, using default settings");
				break;
			}
			// If an unexpected error occurs
			printl("[ZF] No valid user configuration found, using default settings");
			break;
		}
	}

	// note(jae): 2023-08-19
	// We could consider adding map specific configs for users here...

	// Update global zf_config
	zf_config = zf_new_config;
}
