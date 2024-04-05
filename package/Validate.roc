interface Validate
    exposes [validateCli, CliValidationErr]
    imports [Config.{ OptionConfig, CliConfig }]

CliValidationErr : [OverlappingOptionNames (OptionConfig, OptionConfig)]

# TODO: validate the following expectations:
# - all subcommand names are at least 1 character
# - all subcommand names are [a-zA-Z0-9\-_]+
# - no duplicate argument names
# - all short flags are exactly 1 character
# - all long flags are either at least 1 or more than 1 character, TBD
validateCli : CliConfig -> Result {} CliValidationErr
validateCli = \_config ->
    Ok {}
