
# Function to calculate sensitivity and specificity for each study
study_level_outcomes <- function(data = NULL, subset=NULL, formula = NULL,
                                 TP="TP", FN="FN", FP="FP", TN="TN")
{
  if(!is.null(data) & is.character(c(TP,FP,TN,FN))){
    X <- as.data.frame(data)
    origdata <- data
    TP <- getElement(X,TP)
    FN <- getElement(X,FN)
    FP <- getElement(X,FP)
    TN <- getElement(X,TN)
  }
  if(is.null(data) & !is.character(c(TP,FP,TN,FN))){
    origdata <- data.frame(TP = TP, FN = FN, FP = FP, TN = TN)
  }
  
  freqdata <- cbind(TP,FN,FP,TN)
  # Need checkdata function from basics.R (or end of this R file) for this to work
  checkdata(freqdata)
  
  # Sensitivity and specificity calculations for each study
  origdata$sens <- origdata$TP / (origdata$TP + origdata$FN)
  origdata$spec <- origdata$TN / (origdata$FP + origdata$TN)
  origdata$fpr <- 1- origdata$spec
  origdata$fnr <- 1- origdata$sens
  
  study_level <- data.frame(TP=origdata$TP, FN=origdata$FN, FP=origdata$FP, TN=origdata$TN, 
                            N=(origdata$TP+origdata$FN+origdata$FP+origdata$TN), 
                            Sensitivity=origdata$sens, Specificity=origdata$spec,
                            FPR=origdata$fpr, FNR=origdata$fnr)
  
  return(study_level)
}

# Function needed to checkdata in correct format before calculating sens and spec for each study
checkdata <- function(X, nrowwarn = 5){
  X <- as.data.frame(X)
  if(!all(c("TP","FN","FP","TN") %in% names(X))){
    stop("Data frame or matrix must have columns labelled TP, FN, FP and TN.")}
  if(!identical(round(X),as.data.frame(apply(X,2,as.numeric)))){
    warning("Some of the values of TP,FN,FP or TN do have non zero decimal places. Did you forget to round?")}
  if(nrow(X) < nrowwarn){warning("There are very few primary studies!")}
  idx_too_many_zeroes <- apply(X,1,function(x){sum(x == 0)}) > 2
  if(any(idx_too_many_zeroes)){
    stop(paste("Some study with three or more zeroes in 2x2 table! Row:", which(idx_too_many_zeroes)))
  }
  return(invisible(NULL))
}
