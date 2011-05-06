# remove previous copies if any present
sapply( rev(which(search() %in% "RCT3_functions")), function(i) {detach(pos = i); invisible(NULL)} )     

# create a place on the search path for functions to sit
rct3.env <- attach(NULL, name = "RCT3_functions")

# put functions in the right place
evalq(envir = rct3.env, 
{

read.rct3.file <-
function(fname, sep = ",")
{
  hdr <- readLines(fname, n = 2)
  stock <- strsplit(hdr, sep)[[1]][1]
  data_info <- as.numeric(strsplit(hdr, sep)[[2]][1:3])
  ind_names <- gsub(sep, "", readLines(fname)[-(1:(data_info[2]+2))])

  dat <- t(sapply(strsplit(readLines(fname)[1:data_info[2]+2], sep), as.numeric))
  dat[dat==-11] <- NA
  
  dat <- as.data.frame(dat)  
  names(dat) <- c("yearclass", "recruitment", ind_names)
  attr(dat, "stock") <- stock
  
  dat
}   

write.rct3.file <-
function(x, fname)
{
  cat("\nNOT YET!!!\n")
}

get.rct3.inputs <-
function(stk.obj, surv.obj, control, rec.age)
{
  inputs <- list( recruitment = drop(unclass(rec(stk.obj))) / 1000 )

  for (i in 1:nrow(control))
  {
    surv.name <- tolower(gsub("_", "", with(control[i,], paste(survey, "age", ages, sep = ""))))
    inputs[[ surv.name ]] <- drop(unclass(surv.obj[[control $ survey[i]]] @ index))[paste(control $ ages[i]),]
  }

  input.ages <- c(rec.age,control $ ages)
  names(input.ages) <- names(inputs)

  inputs[] <-
  lapply(names(inputs), 
    function(nam)
    {
      x <- inputs[[nam]]
      names(x) <- as.numeric(names(x)) - input.ages[nam]
      x
    })

  yearclass <- range( unlist(sapply(inputs, function(x) as.numeric(names(x)))))
  yearclass <- yearclass[1]:yearclass[2] 
  
  out <- data.frame(yearclass = yearclass)
  out[names(inputs)] <- NA
  rownames(out) <- yearclass
  
  for (i in names(inputs))out[names(inputs[[i]]), i] <- inputs[[i]]

  out
}

# rct3 function - from the data frame, takes a formula and does an RCT3 on it
rct3 <- 
function(formula, data, predictions = NULL, shrink = FALSE)
{
  form <- formula[[3]]
  bits <- list()
  while(length(form)>1)
  {
    bits <- c(bits, form[[3]])
    form <- form[[2]]
  }
  bits <- rev(c(bits, form))
  formulas <- lapply(bits, function(x) {tmp <- formula; tmp[[3]] <- tmp[[2]]; tmp[[2]] <- x; tmp})
  formulas2 <- lapply(bits, function(x) {tmp <- formula; tmp[[3]] <- x; tmp})

  weight <- function(y, y0, D, p) pmax(0, (1 - ((y0 - y)/D)^p)^p)

  log.data <- data
  log.data[names(data) != "yearclass"] <- log(data[names(data) != "yearclass"])
    
  # fit one model at a time
  do.one.prediction <-
  function(i, predict.yr)
  {
    wk.data <- subset(log.data, yearclass < predict.yr)
    if (nrow(wk.data) < 3) stop("too few data points in one survey!") 
    m <- lm(formulas[[i]], data = wk.data)
    b <- {function(x) c(-x[1], 1)/x[2] }(unname(coef(m)))
    rss <- sum( m$residuals^2 )
    mss <- with(m, sum((fitted.values - mean(fitted.values))^2))
    sigma <- b[2] * sqrt(rss / m $ df.residual)
    rsqr <- mss / (rss + mss)
    n <- m $ df.residual + 2
    
    Xp <- unname(model.matrix(formulas2[[i]][c(1,3)], subset(log.data, yearclass == predict.yr)))
    if (nrow(Xp))
    {
      X <- unname(model.matrix(formulas2[[i]], wk.data))
      XXinv <- solve(t(X) %*% X)
      pred <- drop(Xp %*% b)
      se.pred <- sqrt(n / (n-2)) * sigma * sqrt(1 + drop(Xp %*% XXinv %*% t(Xp)))
      index <- Xp[,2]
    } else index <- pred <- se.pred <- NA
    
    data.frame(index = as.character(formulas[[i]][[2]]), 
               slope = b[2], intercept = b[1], 
               se = sigma, rsquare = rsqr, n = n,
               indices = index, prediction = pred,
               se.pred = se.pred)
  }
  
  if (is.null(predictions)) 
  {
    y <- eval(formula[[2]], log.data) 
    predictions <- log.data $ yearclass[is.na(y)] 
  }
  
  out <- 
    lapply(predictions, 
      function(yr) 
      {
        out <- do.call( rbind, lapply(1:length(formulas), do.one.prediction, predict.yr = yr))
        vpa <- eval(formula[[2]], log.data)[log.data $ yearclass < yr]
        vpa <- vpa[!is.na(vpa)] 
        out <- rbind(out, data.frame(index = "VPA Mean",
                                     slope = NA, intercept = NA, 
                                     se = NA, rsquare = NA, n = length(vpa),
                                     indices = NA, 
                                     prediction = mean(vpa),
                                     se.pred = sd(vpa)))
        if (shrink)                              
        {
          out $ WAP.weights <- with(out, (1/se.pred^2) / sum(1/se.pred^2, na.rm = TRUE))
        }
        else
        {
          out $ WAP.weights <- c(with(out[1:(nrow(out)-1),], (1/se.pred^2) / sum(1/se.pred^2, na.rm = TRUE)), 0)
        }
          
        out                                                                                                                        
      })
  names(out) <- paste("yearclass", predictions, sep=":")

  out <- list(stock = attr(data, "stock"),
              info = c(length(bits), nrow(data), range(log.data $ yearclass)), 
              rct3 = out, 
              rct3.summary = do.call(rbind, lapply(out, summarise.rct3)),
              shrink = shrink)

  class(out) <- "rct3"  
  out
}

summarise.rct3 <-
function(tmp)
{
  pred <- with(tmp, sum(prediction * WAP.weights, na.rm = TRUE))
  int.se <- 1/sqrt(sum(1/tmp $ se.pred^2, na.rm = TRUE))
  
  data.frame(WAP = exp(pred), logWAP = pred, int.se = int.se)
}

print.rct3 <-
function(x, digits = max(3, getOption("digits") - 3), ...)
{
  hdr <- with(x,
  c("Analysis by RCT3 ver4.0\n",                       
    stock,
    paste("\nData for ", info[1]," surveys over ", info[2]," year classes : ", info[3]," - ", info[4], sep=""),
    "Regression type = C",
    "Tapered time weighting not applied",
    "Survey weighting not applied",
    if (x $ shrink) "Final estimates ARE shrunk towards mean" else "Final estimates not shrunk towards mean",
    "Estimates with S.E.'S greater than that of mean included",
    "Minimum S.E. for any survey taken as    .00",
    "Minimum of   3 points used for regression\n",
    "Forecast/Hindcast variance correction used.\n"
  ))
 
  cat(paste(hdr, collapse = "\n"), "\n")
  for (i in seq_along(x $ rct3))
  {
    cat(names(x $ rct3)[i], "\n")
    print.data.frame(x $ rct3[[i]], digits = digits, row.names = FALSE)
    cat("\n")
  }
  
  print.data.frame(x $ rct3.summary, digits = digits)  
} 
})

  