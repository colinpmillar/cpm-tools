
#setwd("2019-03-13-edda-rct3/")

# read from the rct3 file
recdata <- read.table("recrhad18_ed.txt", header = TRUE)

formula <- recruitment ~ NT1 + NT2 + NT3 + NAK1 + NAK2 + NAK3 + RT1 + RT2 + RT3 + EC01 + ECO2 + ECO3

# load rct3 function
source("rct3.R")

my_rct3 <- rct3(formula, recdata, predictions = 2012:2017, shrink = TRUE)

# see a short summary
my_rct3

# for a full summary do:
summary(my_rct3)

# the components are here:
my_rct3$rct3
my_rct3$rct3.summary

# predicted recruitment
t(my_rct3$rct3.summary["WAP"])

