getRpkgs <- function(Rlibs) {
  Rpkgs <- data.frame(installed.packages(lib.loc = Rlibs, fields = c('Package', 'Version', 'Priority')), stringsAsFactors = FALSE)
  Rpkgs
}

## compares current R installation with bundles in src/contrib
##   1) removes package versions not available in src/contrib
##   2) installs all bundles not already installed in right version
cloneRmodel <- function(Rmodel, Rlib, Rlibs){
# get installed R packages
Rpgks <- getRpkgs(Rlibs)

# available packages listed in model repo
Rpgks_listed <- data.frame(availabe.packages(repos=Rmodel)[,c('Package', 'Version')], stringsAsFactors = FALSE)

# compare
Rpgks_installed <- Rpgks[is.na(Rpgks$Priority) | Rpgks$Priority != 'base', 1:2]
xx <- merge(Rpgks_installed, Rpgks_listed, all=TRUE, by='Package', suffixes = c('.installed', '.new'))
todo <- xx[is.na(xx$Version.installed) | xx$Version.installed != xx$Version.new,]

toerase <- todo$Package[!is.na(todo$Version.installed)]
toinstall <- todo$Package

# act
remove.packages(toerase, .libPaths())
rc <- install.packages(toinstall, Rlib, repos = Rmodel,
                 type = 'source', INSTALL_opts = '--install-tests')
rc
}


showRpkgs <- function(Rlibs) {
## download installed R packages
Rpgks <- getRpkgs(Rlibs)

# write sorted package inventory file
write.table(Rpgks[order(Rpgks[,'Package']),],
            file = paste0('R', getRversion(), '_pkgs.tsv'),
            quote = FALSE, sep = '\t', na = '', row.names = FALSE
            )
}
