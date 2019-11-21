# updates to the model repo

#' list packages provided in srccontrib directory
#'
#' @param srccontrib directory of source R package bundles
#'
#' @return dataframe sorted by package name
#' @export
#'
#' @examples
report_src_contrib <- function(srccontrib='src/contrib') {
  bundles <- list.files(path = srccontrib, pattern = '*.tar.gz')
  structure(data.frame(t(sapply(strsplit(sub('.tar.gz$', '', bundles), split='_'), rbind)), stringsAsFactors = FALSE),
                  names=c('Package', 'Version'))
}

# yy <- merge(xx, baserecpkgs, all=TRUE)
# yy$Version[is.na(yy$Version)] <- as.character(getRversion())

