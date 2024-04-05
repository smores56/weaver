interface Validate
    exposes [validateCli, CliValidationErr]
    imports [Config.{ OptionConfig, CliConfig }]

CliValidationErr : [OverlappingOptionNames (OptionConfig, OptionConfig)]

validateCli : CliConfig a -> Result {} CliValidationErr
validateCli = \_config ->
    Ok {}
