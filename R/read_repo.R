# functions to read from and compare with model R repository

#' retrieve current R package setup
#'
#' @param Rlibs character vector of R libraries to check
#'
#' @importFrom utils installed.packages
#' @return dataframe with 3 variables: Package, Version, and Priority
#' @export
#'
#' @examples
getRpkgs <- function(Rlibs=NULL) {
  Rpkgs <- data.frame(installed.packages(lib.loc = Rlibs),
                      stringsAsFactors = FALSE)[, c('Package', 'Version', 'Priority')]
  Rpkgs
}


#' adapts packages in Rlibs according to todo dataframe
#'   1) removes package versions in Rlibs that are not in todo
#'   2) installs all missing package versions in todo
#'
#' @param todo input dataframe of packages to change
#' @param Rlibs character vector of R libraries to modify packages.
#'        Default NULL means .libPaths() is read.
#' @param installlib library to install packages to; defaults to first element of Rlibs.
#'
#' @importFrom utils install.packages remove.packages
#' @return rc named vector of installation return codes.
#' @export
#'
#' @examples
adaptRpkgs <- function(todo, Rlibs=NULL, installlib=Rlibs){
  toerase <- todo$Package[!is.na(todo$Version.installed)]
  toinstall <- todo$Package

  # act
  remove.packages(toerase, Rlibs)
  rc <- install.packages(toinstall, installlib, repos = todo$repo,
                   type = 'source', INSTALL_opts = '--install-tests')
  rc
}

#' compares current R installation in Rlibs with Rmodel
#'
#' @param Rmodel location of the model R repo
#' @param Rlibs character vector of R libraries to check
#'
#' @return dataframe with packages to change
#' @export
#' @importFrom utils  available.packages
#'
#' @examples
compareRmodel <- function(Rmodel, Rlibs){
  # get installed R packages
  Rpgks <- getRpkgs(Rlibs)

  # available packages listed in model repo
  Rpkgs_model <- data.frame(utils::available.packages(repos=Rmodel)[,c('Package', 'Version')],
                            stringsAsFactors = FALSE)

  # compare
  Rpgks_installed <- Rpgks[is.na(Rpgks$Priority) | Rpgks$Priority != 'base', 1:2]
  xx <- merge(Rpgks_installed, Rpkgs_model, all=TRUE, by='Package', suffixes = c('.installed', '.new'))
  todo <- xx[is.na(xx$Version.installed) | xx$Version.installed != xx$Version.new,]
  todo$repo <- Rmodel
  todo
}

#' write R packageset to standard output file in tsv format
#'
#' @param Rlibs vector of R libraries to check; defaults to NULL
#' @param outfile filename of output file that lists all package versions in tsv format.
#'
#' @importFrom utils write.table
#' @return output file name
#' @export
#'
#' @examples
reportRpkgs <- function(Rlibs=NULL, outfile=paste0('R-', getRversion(), '_pkgs.tsv')) {
  ## download installed R packages
  Rpgks <- getRpkgs(Rlibs)

  # write sorted package inventory file
  write.table(Rpgks[order(Rpgks[,'Package']),],
              file = outfile,
              quote = FALSE, sep = '\t', na = '', row.names = FALSE
              )
  outfile
}

