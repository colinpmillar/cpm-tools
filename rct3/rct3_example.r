# get data - only requirement from this function is that data is a data.frame with names yearclass, recruitment, index1, index2, ...
#           - can optionally have an attribute "stock" with the stock name in it


# read from the rct3 format
fname <- "C:/colin/packages/mycode/rct3/whiIN.dat"
whi.dat <- read.rct3.file(fname, sep = "\t")

# create a data.frame from the stock, index objects with info on what surveys and ages to use
control <- data.frame(survey = c("IBTS_Q1", "IBTS_Q1", "IBTS_Q3", "IBTS_Q3"), ages = c(1, 2, 0, 1))
whi.dat2 <- get.rct3.inputs(stk.obj = tmp $ whi.xsa, 
                            surv.obj = tmp $ whiting.index, 
                            control = control, rec.age = 1)

formula <- recruitment ~ ibtsq1age1 + ibtsq1age2 + ibtsq3age0 + ibtsq3age1

my_rct3 <- rct3(formula, whi.dat, predictions = 2008:2010, shrink = TRUE)

my_rct3
