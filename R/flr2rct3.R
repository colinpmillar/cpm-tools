# flr2rct3 <- function(stk.obj, surv.obj, control, rec.age)
# {
#   inputs <- list( recruitment = drop(unclass(rec(stk.obj))) / 1000 )
#
#   for (i in 1:nrow(control))
#   {
#     surv.name <- tolower(gsub("_", "", with(control[i,], paste(survey, "age", ages, sep = ""))))
#     inputs[[ surv.name ]] <- drop(unclass(surv.obj[[control $ survey[i]]] @ index))[paste(control $ ages[i]),]
#   }
#
#   input.ages <- c(rec.age,control $ ages)
#   names(input.ages) <- names(inputs)
#
#   inputs[] <-
#   lapply(names(inputs),
#     function(nam)
#     {
#       x <- inputs[[nam]]
#       names(x) <- as.numeric(names(x)) - input.ages[nam]
#       x
#     })
#
#   yearclass <- range( unlist(sapply(inputs, function(x) as.numeric(names(x)))))
#   yearclass <- yearclass[1]:yearclass[2]
#
#   out <- data.frame(yearclass = yearclass)
#   out[names(inputs)] <- NA
#   rownames(out) <- yearclass
#
#   for (i in names(inputs))out[names(inputs[[i]]), i] <- inputs[[i]]
#
#   out
# }

