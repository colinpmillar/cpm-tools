#' @docType data
#'
#' @name shep
#'
#' @title Recruitment and survey index data from Shepherd (1997)
#'
#' @description
#' data.frame containing recruitment (age 1) and survey indices from several
#' surveys over ages 0 to 2. See Shepherd (1997) for details.
#'
#' @usage
#' shep
#'
#' @format
#' Data frame containing 14 columns:
#' \tabular{ll}{
#'   \code{yearclass} \tab the yearclass\cr
#'   \code{age1} \tab the recruiment (age 1) for that yearclass\cr
#'   \code{ssoct0} \tab The age 0 survey index from the October 'NWGFS' survey\cr
#'   \code{ssjun1} \tab The age 1 survey index from June 'NWGFS' survey\cr
#'   \code{...} \tab and so on
#' }
#'
#'
#' @seealso
#'
#' \code{\link{rct3}}  run a calibrated regression to predict rectruitment.
#'
#' \code{\link{rct3-package}} gives an overview of the package.
#'
#'
#' @examples
#' data(shep)
#' head(shep)

NULL
