#' Calculate distance metric between rct3 prediction and assumed value
#' 
#' @description The RCT3 analysis gives a year‐class strength prediction based 
#'   on the survey information, and the standard error associated with the 
#'   prediction. The difference between the assumed size of the recruiting 
#'   year class in spring (before autumn surveys are available) and the RCT3 
#'   year‐class strength estimates based on summer surveys, scaled to the 
#'   internal standard error calculated by RCT3, is D = (R-A)/S, where R is the 
#'   log Weighted Average Prediction (WAP) from RCT3, A is the assumed 
#'   year‐class strength in spring assessment report, and S is the internal 
#'   standard error from RCT3 (ICES, 2008).
#'
#' @param rct3Obj results object of class rct3
#' @param A vector. Assumed numbers to compare to rct prediction
#'
#' @return vector
#' 
#' @references 
#' ICES. 2008. Report of the Ad hoc Group on Criteria for Reopening Fisheries 
#' Advice (AGCREFA), 20–22 August 2008, Copenhagen, Denmark. 
#' ICES CM 2008/ACOM:60. 30 pp.
#' 
#' @export
#'
#' @examples
#' 
#' data(pok)
#' fmla <- formula(age4 ~ ibtsq3)
#' res <- rct3(formula = fmla, data = pok)
#' calcD(rct3Obj = res, A = log(83000))
#' 
calcD <- function(rct3Obj = NULL, A = NULL){
  if(is.null(rct3Obj) | is.null(rct3Obj)){
    stop("Both rtcObj and A must be specified")}
  if(length(A) != length(rct3Obj$rct3.summary$logWAP)){
    stop("Length of A does not match the number of predicted values")}
  R <- rct3Obj$rct3.summary$logWAP
  S <- rct3Obj$rct3.summary$int.se
  D <- (R - A) / S
  return(D)
}
