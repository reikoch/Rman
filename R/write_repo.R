# updates to the model repo

#' list packages provided in srccontrib directory
#'
#' @param srccontrib directory of source R package bundles
#'
#' @return dataframe sorted by package name
#'
#' @examples
list_src_contrib <- function(srccontrib='src/contrib') {
  bundles <- list.files(path = srccontrib, pattern = '*.tar.gz')
  structure(data.frame(t(sapply(strsplit(sub('.tar.gz$', '', bundles), split='_'), rbind)), stringsAsFactors = FALSE),
                  names=c('Package', 'Version'))
}

#' report packages provided in srccontrib directory plus current base packages
#'
#' @param srccontrib directory of source R package bundles
#' @param report name of tsv file result is written to, 
#'        default to file R-x.y.z_pkgs.tsv  
#'
#' @return dataframe sorted by package name
#' @export
#'
#' @examples
report_src_contrib <- function(srccontrib='src/contrib', report=paste0('R-', getRversion(),'_pkgs.tsv')) {
  yy <- merge(list_src_contrib(srccontrib), baserecpkgs, all=TRUE, by='Package')
  yy$Version[is.na(yy$Version)] <- as.character(getRversion())
  yy <- yy[order(yy$Package),]
  write.table(yy, file=report, row.names = FALSE, sep = '\t', na = '', quote = FALSE)
  yy
}
