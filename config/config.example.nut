// User configuration file
//
// Instructions:
// - Rename this file to "config.nut" to use it
// - Remove the // comments for values where you want to change the defaults.
//
// If it's loaded successfully, the server logs will say "[ZF] Loaded user configuration"
::SW_ZF_LoadUserConfig <- function(reserved) {
    return {
        // Do not modify this, this is the config file format version.
        // This will allow us to support old config files if this structure gets updated.
        config_version = "1",

        // force_enabled will enable zombie fortress for all maps, not just zombie maps.
        // Default: false
        // force_enabled = false,

        // survivor_team_ratio will make the survivor team have 65% of the player, and zombies
        // will have 35%.
        // Default: 0.65
        // survivor_team_ratio = 0.65,
    }
}
