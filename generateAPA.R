library(MplusAutomation)

generateAPA <- function(models) {
  classes <- c("1-class", "2-class",  "3-class", "4-class",  "5-class")
  vars <- c("LL", "npar", "CAIC", "BIC", "saBIC", "AWE", "LRTS", "Adj LMR p-value", "Bootstrapped p-value", "BF (K, K+1)", "cmP(K)")
  table <- matrix(nrow = length(classes), ncol = length(vars), dimnames = list(classes, vars))
  
  if ("mplus.model" %in% class(models)) {
    summaries <- list(models$summaries)
  } else if ("mplus.model.list" %in% class(models)) {
    summaries <- vector("list", length = length(models))
    for (i in 1:length(models)) {
      summaries[[i]] <- models[[i]]$summaries
    }
  }
  else {
    stop("Error! Input must be of class \"mplus.model\" or \"mplus.model.list\"")
  }
  
  n <- models[[1]]$summaries$Observations
  
  for (i in 1:length(summaries)) {
    table[i, 1] <- models[[i]]$summaries$LL
    table[i, 2] <- models[[i]]$summaries$Parameters
    table[i, 3] <- (-2*table[i, 1] + table[i, 2]*(log(n) + 1))
    table[i, 4] <- models[[i]]$summaries$BIC
    table[i, 5] <- (-2*table[i, 1] + table[i, 2]*log((n+2)/24))
    table[i, 6] <- (-2*table[i, 1] + 2*table[i, 2]*(log(n) + 1.5))
  }
  
  for (i in 2:length(summaries)) {
    table[i, 7] <- (-2*(table[i - 1, 1] - table[i, 1]))
    table[i, 8] <- models[[i]]$summaries$T11_LMR_PValue
    table[i, 9] <- models[[i]]$summaries$BLRT_PValue
  }
  
  SIC = 0
  SIC[1:length(models) + 1] = 0  
  for (i in 1:length(models)) SIC[i] <- -0.5*table[i, 4]
  
  for (i in 1:length(models)) table[i, 10] <- (exp(SIC[i] - (SIC[i + 1])))
  
  maxSIC <- max(SIC[1:length(models)])
  
  exp_SIC_max = 0
  for (i in 1:length(models)) exp_SIC_max[i] <- (exp(SIC[i]-maxSIC))
  
  sum_eSm <- sum(exp_SIC_max[1:length(models)])
  
  for (i in 1:length(models)) table[i, 11] <- (exp_SIC_max[i]/sum_eSm)
  
 return(table) 
}

AllOutput <- readModels("C:\\Input\\Your\\File\\Path\\Here")
View(generateAPA(AllOutput))
