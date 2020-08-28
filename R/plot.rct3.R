#' @title Plot rct3 regression fits by index
#' 
#' @description Produces a plot of the rct3 model term fits for a single year.
#'   Regression lines for each term are a black dashed line, mean  
#'   (black dashed regression line), including predicted value and confidence interval (blue). 
#'
#' @param rct3Obj rct object 
#' @param predYr prediciont year
#' @param indices Indices to plot (Default = NULL plots all) 
#' @param ylimConst logical. Use same y-lmits for all indices.
#'
#' @return plot
#' @export
#' 
#' @examples
#' data(pok)
#' head(pok)
#' fmla <- formula(age4 ~ ibtsq3)
#' pok_rct3 <- rct3(formula = fmla, data = pok, range = nrow(pok), old = FALSE)
#' plot(pok_rct3)
#' 
#' 
#' data(recdata)
#' head(recdata)
#' formula <- recruitment ~ NT1 + NT2 + NT3 +
#'   NAK1 + NAK2 + NAK3 +
#'   RT1 + RT2 + RT3 +
#'   ECO1 + ECO2 + ECO3
#' 
#' my_rct3 <- rct3(formula = formula, data = recdata, predictions = 2012:2017, 
#'   shrink = TRUE)
#' # plot(my_rct3) # produces error since no prediction year is defined
#' plot(rct3Obj = my_rct3, predYr = 2012)
#' plot(rct3Obj = my_rct3, predYr = 2012, ylimConst = F) # variable ylim
#' 
#' 
#' 
plot.rct3 <- function(rct3Obj, predYr = NULL, indices = NULL, 
  ylimConst = TRUE){
  
  if(is.null(predYr)){
    if(length(rct3Obj$rct3) == 1){
      predYr <- unlist(strsplit(names(rct3Obj$rct3), ":"))[2]
    }else{
      stop("Please specify the prediction year (predYr).\n Only a single year can be plotted")
    }
  }
  
  resp <- rct3Obj$formula[[2]]
  regs <- rct3Obj$rct3[[paste0("yearclass:", predYr)]]
  regs$pred.up <- regs$prediction + regs$se.pred
  regs$pred.low <- regs$prediction - regs$se.pred
  terms <- regs$index[which(!is.na(regs$slope))]
  meanN <- subset(regs, index == "VPA Mean")$prediction
  if(is.null(indices)){indices <- terms}
  nterms <- length(terms)
  
  if(length(nterms) == 0){ stop("No predictions available for the defined year and indices") }
  
  ncol <- ceiling(sqrt(nterms))
  panelSeq <- rep(0, ncol^2)
  panelSeq[seq(nterms)] <- seq(nterms)
  M <- matrix(panelSeq, nrow = ncol, ncol = ncol, byrow = TRUE)
  keepRows <- apply(M, 1, function(x) !sum(x==0)==length(x))
  M <- as.matrix(M[keepRows,])
  ylim.all <- range(
    log(tail(rct3Obj$data, rct3Obj$range)[,as.character(resp)]), 
    regs$pred.up, regs$pred.low, na.rm = TRUE)
 
  
  op <- par(no.readonly = TRUE, mar = c(3.5, 3.5, 0.25, 0.25), mgp = c(2,1,0))
  lo <- layout(M, widths = rep(1, ncol(M)), heights = rep(1, nrow(M)), respect = F)
  # layout.show(max(M))
  
  for(i in seq(terms)){
    fmla.i <- formula(paste(paste0("log(", resp, ")"), "~", paste0("log(", terms[i], ")")))
    reg.i <- subset(regs, index == terms[i])
    xlim.i <- range(
      log(tail(rct3Obj$data, rct3Obj$range)[,as.character(terms[i])]),
      reg.i$indices, na.rm = TRUE)
    if(ylimConst){
      ylim.i <- ylim.all
    }else{
      ylim.i <- range(
        log(tail(rct3Obj$data, rct3Obj$range)[,as.character(resp)]), 
        reg.i$pred.up, reg.i$pred.low, na.rm = TRUE)
    }
    plot(fmla.i, data = tail(rct3Obj$data, rct3Obj$range), t = "n", 
      xlim = xlim.i, ylim = ylim.i)
    abline(h = meanN, lty = 3, col = 8)
    abline(a = reg.i$intercept, b = reg.i$slope, lty = 2, col = 4)
    points(fmla.i, data = tail(rct3Obj$data, rct3Obj$range))
    points(prediction ~ indices, reg.i, pch = 2, col = 4)
    segments(x0 = reg.i$indices, x1 = reg.i$indices, 
      y0 = reg.i$prediction-reg.i$se.pred, y1 = reg.i$prediction+reg.i$se.pred, 
      col = 4)
    box()
  }
  
  par(op)

}
