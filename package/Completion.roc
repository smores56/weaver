module [generateCompletionForShell]

import Base exposing [CliConfig]
import Completion.Fish exposing [generateCompletion]

Shell : [Bash, Fish, Zsh]

generateCompletionForShell : CliConfig, Shell -> Str
generateCompletionForShell = \config, shell ->
    when shell is
        Bash -> ""
        Fish -> generateCompletion config
        Zsh -> ""
