::SW_ZF_LoadDefaultMapConfig <- function() {
	return {
		config_version = "1",
		// As per map authors recommendation, zf_dustbowl_b4 needs to have a survivor_team_ratio team
		// ratio of 0.90 or else it's very unbalanced.
		//
		// See:
		// - https://steamcommunity.com/sharedfiles/filedetails/?id=2926638864
		// - https://gamebanana.com/mods/72542
		survivor_team_ratio = 0.90,
	}
}
