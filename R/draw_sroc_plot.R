#' Plot an interactive SROC curve 
#' 
#' @param data Input data set.
#' @param covcheck Number of the covariate to be displayed. Should be a named vector where the first entry is 1, corresponding to no covariate, and the rest are the integers 2 to number_of_covariates + 1, named by the covariate name.
#' @param HSROCcheck List which contains 1 if data points are to be displayed and/or 2 if SROC is to be displayed.
#' @param prevcheck TRUE to display disease prevalence.
#' @param bivcheck List containing 1, 2 and/or 3 corresponding to whether summary points, 95% confidence regions and 95% predictive regions respectively should be displayed.
#' @param cicheck List containing 1 and/or 2 corresponding to whether 95% confidence intervals for sensitivity or specificity respectively should be displayed.
#' @param QAcheck List containing any of the integers 1 to 11 to display the following quality assessment data: "None" = 1, "Risk of bias: Patient selection" = 2, "Risk of bias: Index test" = 3, "Risk of bias: Reference standard" = 4, "Risk of bias: Flow & timing" = 5, "Applicability concerns: Patient selection" = 6, "Applicability concerns: Index test" = 7, "Applicability concerns: Reference standard" = 8, "Risk of bias (all)" = 9, "Applicability concerns (all)" = 10, "Both risk of bias and applicability concerns" = 11.
#' @param cov_toggle 1, 2 or 3, whether to display covariates as "Text" = 1, "Coloured points" = 2, "Both" = 3.
#' @param title Title of the plot.
#' @param weightcheck TRUE to display percentage study weights.
#' @param ci_col 1, 2 or 3 to deteremine the colour of confidence intervals as "Black" = 1, "Quality assessment" = 2, "Covariate" = 3.
#' @param extrapp TRUE to extrapolate SROC curve beyond the range of observed data.
#' @return The SROC plot.
DrawSrocPlot <- function(data, covcheck, HSROCcheck, prevcheck, bivcheck, cicheck, QAcheck, cov_toggle, title, weightcheck, ci_col, extrapp) {
  if(is.null(data)){return()}
  else
    X <- data
  # Count the number of studies
  N <- length(X$TP)
  # Count the number of columns
  # Used to determine if quality assessment data is present
  C <- length(X[1,])
  # Store the names of the columns
  # Used to determine if covariates are present
  Names <- colnames(X)
  # Set up the data
  # Generate 5 new variables of type long. We need these before we can reshape the data.
  # n1 is number diseased
  # n0 is number without disease
  # true1 is number of true positives
  # true0 is the number of true negatives
  # study is the unique identifier for each study. _n will generate a sequence of numbers.
  X$n1 <- X$TP+X$FN
  X$n0 <- X$FP+X$TN
  X$true1 <- X$TP
  X$true0 <- X$TN
  X$study <- 1:N
  
  # Reshape the data from wide to long format
  Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
              timevar = "sens" , times = c(1,0) , v.names = c("n","true") )
  
  # Sort data by study to cluster the 2 records per study together
  Y = Y[order(Y$id),]
  Y$spec<- 1-Y$sens
  
  # Perform meta-analysis
  MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                data = Y , family = binomial , nAGQ=1 , verbose=0 )
  
  # More detail can be obtained by using the summary command
  ma_Y = summary(MA_Y)
  labels(ma_Y)
  ma_Y$coeff
  
  # Logit sensitivity and specificity
  lsens = ma_Y$coeff[1,1]
  lspec = ma_Y$coeff[2,1]
  
  # HSROC parameters
  # Use estimates of bivariate model parameters to calculate HSROC parameters
  sigma2_a <- ma_Y$varcor$study[1]
  sigma2_b <- ma_Y$varcor$study[4]
  sigma_ab <- ma_Y$varcor$study[2]
  sigma_a <- sqrt(sigma2_a)
  sigma_b <- sqrt(sigma2_b)
  beta <- log(sigma_b/sigma_a)
  Theta <- 0.5*((((sigma_b/sigma_a)^0.5 )*lsens) - (((sigma_a/sigma_b)^0.5) *lspec))
  Lambda <- (((sigma_b/sigma_a)^0.5) * lsens) + ((sigma_a/sigma_b)^0.5 *lspec)
  sigma2_theta <- 0.5*((sigma_a*sigma_b) - sigma_ab)
  sigma2_alpha <- 2*((sigma_a*sigma_b) + sigma_ab)
  
  # Calcuate points for ROC curve
  # Start with empty data frame
  roc_points <- rep(NA, 60)
  for (i in seq(from=0, to=1, by=0.01)){
    Sp_i <- i
    Fpr_i <- 1-Sp_i
    LSp_i <- qlogis(Sp_i)
    LSe_i <- Lambda*exp(-beta/2) - exp(-beta)*LSp_i
    Se_i <- plogis(LSe_i)
    roc_i <- data.frame(FPR=Fpr_i, Sen=Se_i)
    # Add results for most recent value to a data frame which contains the results of previous values
    roc_points<-rbind(roc_points,roc_i)
  }
  
  # Calculate confidence and predictive regions (based on Stata metandi commands)
  # Start by calculating derived and set parameters
  seB     <- ma_Y$coefficients[2,2]
  seA     <- ma_Y$coefficients[1,2]
  r       <- ma_Y$vcov@x[2] / (seA*seB)
  varA    <- ma_Y$varcor$study[1,1]
  varB    <- ma_Y$varcor$study[2,2]
  sAB     <- ma_Y$varcor$study[1,2]
  covAB   <- ma_Y$vcov@x[2]
  sepredA <- sqrt(varA + seA^2)
  sepredB <- sqrt(varB + seB^2)
  rpredAB <- (sAB + covAB) / (sepredA*sepredB)
  level   <- 95
  f       <- qf(0.95, df1=2, df2=N-2)
  croot   <- sqrt(2*f)
  
  # Empty data frames
  conf_region <- (rep(NA, 361))
  pred_region <- (rep(NA, 361))
  
  # Confidence region
  for (i in seq(0, 2*pi, length.out=361)){
    confB <- lspec + (seB*croot*cos(i))
    confA <- lsens + (seA*croot*cos(i + acos(r)))
    confsens <- plogis(confA)
    confspec <- plogis(confB)
    conf_i <- data.frame(X=1-confspec, Y=confsens)
    # Add results for most recent value to a data frame which contains the results of previous values
    conf_region<-rbind(conf_region, conf_i)
  }
  conf_region <- conf_region[2:362,]
  # Need to fix the
  # Predictive region
  for (i in seq(0, 2*pi, length.out=361)){
    predB <- lspec + (sepredB*croot*cos(i))
    predA <- lsens + (sepredA*croot*cos(i + acos(rpredAB)))
    predsens <- plogis(predA)
    predspec <- plogis(predB)
    pred_i <- data.frame(X=1-predspec, Y=predsens)
    # Add results for most recent value to a data frame which contains the results of previous values
    pred_region<-rbind(pred_region, pred_i)
  }
  pred_region <- pred_region[2:362,]
  
  # Sensitivity and specificity calculations for each study
  X$sens <- X$TP / (X$TP + X$FN)
  X$spec <- X$TN / (X$FP + X$TN)
  X$fpr <- 1- X$spec
  
  # Separate dataframes dependent on if quality assessment data is available
  # Need to identify if quality assessment data is present and if covariate data is present
  # If quality assessment data is avaialble need to create further dataframes to produce pie charts
  
  # No quality assessment, No covariates
  if (C == 6){
    study_level <- data.frame(ID=X$study, TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN, N=(X$TP+X$FN+X$FP+X$TN), 
                              Sensitivity=X$sens, Specificity=X$spec, FPR=X$fpr, Prev=((X$TP+X$FN)/(X$TP+X$FN+X$FP+X$TN)))
  }
  
  # No quality assessment, Yes Covariates
  if (C > 6 & Names[7] != "rob_PS"){
    study_level <- data.frame(ID=X$study, TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN, N=(X$TP+X$FN+X$FP+X$TN), 
                              Sensitivity=X$sens, Specificity=X$spec, FPR=X$fpr, Prev=((X$TP+X$FN)/(X$TP+X$FN+X$FP+X$TN)))
    if (C == 7){
      covariates <- cbind(as.character(X[,7]))
      no_cov <- 1
    }
    else {
      covariates <- X[,7:C] # extract all covariates 
      no_cov <- length(colnames(covariates)) # number of covariates
    }
    study_level <- cbind(study_level, covariates)# combine with study_level data frame
  }
  
  # Yes quality assessment, No covariates
  if (C == 13 & Names[7] == "rob_PS" ){
    study_level <- data.frame(ID=X$study, TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN, N=(X$TP+X$FN+X$FP+X$TN), 
                              Sensitivity=X$sens, Specificity=X$spec, FPR=X$fpr,rob_PS = X$rob_PS,
                              rob_IT = X$rob_IT, rob_RS = X$rob_RS,rob_FT = X$rob_FT,ac_PS = X$ac_PS,
                              ac_IT = X$ac_IT, ac_RS = X$ac_RS,  Prev = ((X$TP+X$FN)/(X$TP+X$FN+X$FP+X$TN)))
    # Reshape the data to allow for pie charts to be plotted as summary points
    # For ROB outcomes
    P_rob <- X[,-which(names(X) == "ac_PS" | names(X) == "ac_RS"| names(X) == "ac_IT")] # Delete applicability concerns for ROB plot
    P_rob <- reshape(P_rob, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"),
                     v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
    # For AC outcomes
    P_ac <- X[,-which(names(X) == "rob_PS" | names(X) == "rob_RS"| names(X) == "rob_IT"|names(X) == "rob_FT")] # Delete applicability concerns for ROB plot
    P_ac <- reshape(P_ac, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"),
                    v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
    # For both outcomes together
    P_both <- reshape(X, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                      v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
    
  }
  
  # Yes quality assessment, yes covariates 
  if (C > 13 & Names[13] == "ac_RS"){
    study_level <- data.frame(ID=X$study, TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN, N=(X$TP+X$FN+X$FP+X$TN), 
                              Sensitivity=X$sens, Specificity=X$spec, FPR=X$fpr,rob_PS = X$rob_PS,
                              rob_IT = X$rob_IT, rob_RS = X$rob_RS,rob_FT = X$rob_FT,ac_PS = X$ac_PS,
                              ac_IT = X$ac_IT, ac_RS = X$ac_RS,  Prev = ((X$TP+X$FN)/(X$TP+X$FN+X$FP+X$TN)))
    # Reshape the data to allow for pie charts to be plotted as summary points
    # For ROB outcomes
    P_rob <- X[,-which(names(X) == "ac_PS" | names(X) == "ac_RS"| names(X) == "ac_IT")] # Delete applicability concerns for ROB plot
    P_rob <- reshape(P_rob, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"),
                     v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
    # For AC outcomes
    P_ac <- X[,-which(names(X) == "rob_PS" | names(X) == "rob_RS"| names(X) == "rob_IT"|names(X) == "rob_FT")] # Delete applicability concerns for ROB plot
    P_ac <- reshape(P_ac, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"),
                    v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
    # For both outcomes together
    P_both <- reshape(X, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                      v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
    
    if (C == 14){
      covariates <- cbind(as.character(X[,14]))
      no_cov <- 1
    }
    else {
      covariates <- X[,14:C] # extract all covariates 
      no_cov <- length(colnames(covariates)) # number of covariates
    }
    study_level <- cbind(study_level, covariates)# combine with study_level data frame
    
  }
  
  # Round prevalence to 2 decimal places 
  study_level[,'Prev'] <- round(study_level[,'Prev'] , 2)
  
  # Mean point
  Sens = plogis(lsens) 
  Spec = plogis(lspec) 
  fpr <- 1-Spec
  
  mean_point=data.frame(x=fpr, y=Sens)
  
  # Calculate min and max values of sens and spec to avoid extrapolation of SROC curve
  minSens <- min(X$sens)
  maxSens <- max(X$sens)
  minFPR  <- min(X$fpr)
  maxFPR  <- max(X$fpr)
  
  # Create new data frame which restricts roc_points to being between min and max values
  roc_points2 <- subset(roc_points, FPR<maxFPR & FPR>minFPR & Sen<maxSens & Sen>minSens)
  
  # Calculate sens and spec confidence intervals at the study level
  # Add the confidence intervals to the dataset
  foreach (i=1:N) %do% {
    study_level$Sens_LCI[i] <- binconf(study_level$TP[i], study_level$TP[i]+study_level$FN[i], method="exact")[2]
    study_level$Sens_UCI[i] <- binconf(study_level$TP[i], study_level$TP[i]+study_level$FN[i], method="exact")[3]
    study_level$FPR_LCI[i]  <- 1 - binconf(study_level$TN[i], study_level$FP[i]+study_level$TN[i], method="exact")[3]
    study_level$FPR_UCI[i]  <- 1 - binconf(study_level$TN[i], study_level$FP[i]+study_level$TN[i], method="exact")[2]
  }
  
  
  # Calculations for percentage weights
  Y_pw = reshape(X, direction = "long", varying = list( c("n0" , "n1") , c( "true0","true1" ) ) ,
                 timevar = "spec" , times = c(1,0) , v.names = c("n","true") )
  Y_pw = Y_pw[order(Y_pw$id),]
  Y_pw$sens<- 1-Y_pw$spec
  X_pw <- cbind(Y_pw$sens, Y_pw$spec)
  XT_pw <- t(X_pw)
  Z <- diag(2*length(study_level$ID))
  invn <- 1/Y_pw$n
  A <- diag(invn)
  p_pw <- predict(MA_Y, type="response")
  var_pw <- p_pw*(1-p_pw)
  B <- diag(var_pw)
  G_one <- matrix(c(varA,covAB,covAB,varB),2,2)
  G <- do.call(adiag, replicate(length(study_level$ID), G_one, simplify = FALSE))
  #inverse of B (required later on)
  BI <- solve(B)
  # Create varianbce matrix for observations
  V <- (Z %*% G %*% t(Z)) + (A %*% BI)
  # invert the variance matrix
  invV <- solve(V)
  # derive the fishers information matrix
  fish <- XT_pw %*% invV %*% X_pw
  # invert Fishers information to obtain Var Beta hat
  varb <- solve(fish)
  pctse <- vector(mode="numeric", length =length(study_level$ID))
  pctsp <- vector(mode="numeric", length =length(study_level$ID))
  
  for (i in 1:length(study_level$ID)){
    DM <- V
    DM[(i*2)-1, (i*2)-1] <- 1000000000
    DM[(i*2)-1, (i*2)] <- 0
    DM[(i*2), (i*2)-1] <- 0
    DM[(i*2), (i*2)] <- 1000000000
    
    invDM <- solve(DM)
    fishD <- XT_pw %*% invDM %*% X_pw
    fishI <- fish - fishD
    weight <- varb %*% fishI %*% varb
    pctse[i] <- 100*(weight[1,1]/varb[1,1])
    pctsp[i] <- 100*(weight[2,2]/varb[2,2])
  }
  
  
  # Create 4 separate legends 
  # Standard 2x2 data
  if (C == 6){
    LL <- 6 # specifies length of the legend
    leg_col <- c("black", "black", "blue", "blue", "blue", "black") # colours for the legend
    leg_lwd <- c(NA, 2, NA, 1, 1, NA) # line width of the legend
  }
  
  # data with covariates only
  if (C > 6 & Names[7] != "rob_PS"){
    if(!('1' %in% covcheck)){ # generates legend to include covariates
      for (i in 1:no_cov){
        if(covcheck == i+1){
          unique <- unique(covariates[,i]) # calculate the number of unique entrys for the selcted covariate
          leg_covnames <- sort(as.character(unique)) #obtains names of each unique level of covariate
          col.rainbow <- matlab.like2(length(unique)) # define a new colour palette
          palette(col.rainbow)
          LL <- length(unique) + 6
        }
      }
      leg_col <- rep(0, times = length(unique))
      for(i in 1:length(unique)){ # assigns correct colour to each level 
        leg_col[i] <- palette(col.rainbow)[length(unique)+1-i]
      }
      leg_col <- c(leg_col,"black", "black", "blue", "blue", "blue", "black")
      leg_lwd <- rep(NA, times = length(unique))
      leg_lwd <- c(leg_lwd, NA, 2, NA, 1, 1, NA)
    }
    else {
      LL <- 6
      leg_col <- c("black", "black", "blue", "blue", "blue", "black")
      leg_lwd <- c(NA, 2, NA, 1, 1, NA)
    }
  }
  
  # data with QA data only 
  if (C == 13 & Names[7] == "rob_PS"){
    LL <- 9
    leg_col <- c("green","red", "lavenderblush3" ,"black", "black", "blue", "blue", "blue", "black")
    leg_lwd <- c(NA, NA, NA, NA, 2, NA, 1, 1, NA)
  }
  
  # data with covariate data and QA data 
  if (C > 13 & Names[13] == "ac_RS"){
    if(!('1' %in% covcheck)){
      for (i in 1:no_cov){
        if(covcheck == i+1){
          unique <- unique(covariates[,i]) # calculate the number of unique entrys for the selcted covariate
          leg_covnames <- sort(as.character(unique)) #obtains names of each unique level of covariate
          col.rainbow <- matlab.like2(length(unique)) # define a new colour palette
          palette(col.rainbow)
          LL <- length(unique) + 9
        }
      }
      leg_col <- rep(0, times = length(unique))
      for(i in 1:length(unique)){ # assigns correct colour to each level 
        leg_col[i] <- palette(col.rainbow)[length(unique)+1-i]
      }
      leg_col <- c(leg_col,"green", "red", "lavenderblush3", "black", "black", "blue", "blue", "blue", "black")
      leg_lwd <- rep(NA, times = length(unique))
      leg_lwd <- c(leg_lwd,NA, NA, NA, NA, 2, NA, 1, 1, NA)
    }
    else{
      LL <- 9
      leg_col <- c("green", "red", "lavenderblush3", "black", "black", "blue", "blue", "blue", "black")
      leg_lwd <- c(NA, NA, NA, NA, 2, NA, 1, 1, NA)
    }
  }
  
  leglabticks <- matrix(nrow=LL, ncol=1) #will put legend labels in this vector
  legendticks <- matrix(nrow=LL, ncol=2) #symbols column 1 for pch column 2 for lty
  
  if ('1' %in% HSROCcheck | prevcheck == TRUE){
    leglabticks[LL]<-"Data" # Legend label for data points
    legendticks[LL,1]<-1}
  
  if ('3' %in% bivcheck){
    leglabticks[LL-1]<-"95% Predictive region" #legend label
    legendticks[LL-1,2]<-3}
  
  if ('2' %in% bivcheck){
    leglabticks[LL-2]<-"95% Confidence region" #legend label
    legendticks[LL-2,2]<-2} #creates interactive vector
  
  if ('1' %in% bivcheck){
    leglabticks[LL-3]<-"Summary estimate" #legend label
    legendticks[LL-3,1]<-15} #change plotticks and legendticks if option 1 selected
  
  if ('2' %in% HSROCcheck){
    leglabticks[LL-4]<-"HSROC curve"
    legendticks[LL-4,2]<-1}
  
  if (('1' %in% cicheck) | ('2' %in% cicheck)){
    leglabticks[LL-5]<-"Uncertainty"
    legendticks[LL-5,1]<-3}
  
  if((C == 13 & Names[7] == "rob_PS") |(C > 13 & Names[13] == "ac_RS")){
    if(QAcheck != 1){
      leglabticks[LL-6]<-"Unclear"
      leglabticks[LL-7]<-"High"
      leglabticks[LL-8]<-"Low"
      legendticks[LL-8,1]<-16
      legendticks[LL-7,1]<-16
      legendticks[LL-6,1]<-16
    }
  }
  
  if(C > 6 & Names[7] != "rob_PS"){
    if(!('1' %in% covcheck)){
      if('2' %in% cov_toggle | '3' %in% cov_toggle){
        for(i in 1:length(leg_covnames)){
          leglabticks[LL-5-i]<-leg_covnames[i]
          legendticks[LL-5-i,1]<-19
        }
      }
      if('1' %in% cov_toggle){
        leglabticks[LL]<-"Data" 
        legendticks[LL,1]<-1
      }
    }
  }
  
  if (C > 13 & Names[13] == "ac_RS"){
    if(!('1' %in% covcheck)){
      if('2' %in% cov_toggle | '3' %in% cov_toggle){
        for(i in 1:length(leg_covnames)){
          leglabticks[LL-8-i]<-leg_covnames[i]
          if('1' %in% QAcheck){
            legendticks[LL-8-i,1]<-19
          }
          else{
            legendticks[LL-8-i,1]<-0
          }
        }
      }
      if('1' %in% cov_toggle){
        leglabticks[LL]<-"Data" 
        legendticks[LL,1]<-1
      }
    }
  }
  
  # Try plotting ROC curve
  # First create a plot with just one point (ann=F means that default titles for title and axis titles are not added 
  # so I can add them myself below)
  plot(1,1, ylim=c(0,1), xlim=c(0,1), ann=F, pch=20, col="white")
  
  # Next command allows me to overlay the next graph on top
  par(new=TRUE) 
  
  # Add grid lines
  abline(v=(seq(0,1,0.2)), col="lightgray", lty="dotted")
  abline(h=(seq(0,1,0.2)), col="lightgray", lty="dotted")
  
  # Add titles
  title(main=title, xlab="False Positive Rate (1 - Specificity)", ylab="Sensitivity")
  
  # Plot study level estimates 
  if ('1' %in% HSROCcheck){
    if(weightcheck == TRUE){
      draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000)
    }
    else{
      points(study_level$FPR, study_level$Sensitivity, pch=1)
    }
  }
  
  
  # Plot prevalence 
  if(prevcheck == TRUE){text(study_level$FPR, study_level$Sensitivity, study_level$Prev, cex =0.7, pos = 1, offset = 0.6)}
  
  # Plots when no quality assessment data or covariate data 
  if(C == 6){ 
    # Plot sens
    if ('1' %in% cicheck){
      arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
             length=0.05, angle=90, code=3)
    }
    # Plot spec
    if ('2' %in% cicheck){
      arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
             length=0.05, angle=90, code=3)
    }
  }
  
  # Plots when quality assessment data is available but no covariate data  
  if (C == 13 & Names[7] == "rob_PS"){ 
    # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
    for (i in 2:8){
      if(QAcheck == i){
        for (k in 1:length(study_level$ID)){
          if(weightcheck == FALSE){
            if (study_level[k,i+8] == 1){
              points(study_level$FPR[k], study_level$Sensitivity[k], pch=19, col="green")
            }
            else if (study_level[k,i+8] == 2){
              points(study_level$FPR[k], study_level$Sensitivity[k], pch=19, col="red")
            }
            else {
              points(study_level$FPR[k], study_level$Sensitivity[k], pch=19, col="lavenderblush3")
            }
          }
          if(weightcheck == TRUE){
            if (study_level[k,i+8] == 1){
              draw.ellipse(study_level$FPR[k], study_level$Sensitivity[k], a=pctsp[k]/1000, b=pctse[k]/1000, col = "green")
            }
            else if (study_level[k,i+8] == 2){
              draw.ellipse(study_level$FPR[k], study_level$Sensitivity[k], a=pctsp[k]/1000, b=pctse[k]/1000, col = "red")
            }
            else {
              draw.ellipse(study_level$FPR[k], study_level$Sensitivity[k], a=pctsp[k]/1000, b=pctse[k]/1000, col = "lavenderblush3")
            }
          }
        }
      }
    }
    # Pie charts to represent study level esitmates split into risk of bias and applicability concerns 
    # Pie charts for risk of bias
    if ('9' %in% QAcheck){
      for (i in 1:max(P_rob$id)){
        a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
        score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
        pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015,
                 col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
      }
    }
    # Pie charts for applicability concerns
    if ('10' %in% QAcheck){
      for (i in 1:max(P_ac$id)){
        a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
        score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
        pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                 col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
      }
    }
    # Pie chart that shows both applicability concerns and risk of bias
    if ('11' %in% QAcheck){
      for (i in 1:max(P_both$id)){
        a_pie3 <- rep(P_ac$id[i], times = 7) # pie chart evenly split
        #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
        score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
        pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                 col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
      }
    }
    
    # Add uncertainty for sens and spec at study level
    if ('1' %in% cicheck & (QAcheck == 1 | QAcheck == 9 |QAcheck == 10 |QAcheck == 11)){
      arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
             length=0.05, angle=90, code=3)
    }
    if ('2' %in% cicheck & (QAcheck == 1 | QAcheck == 9 |QAcheck == 10 | QAcheck == 11)){
      arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
             length=0.05, angle=90, code=3)
    }
    
    for (i in 2:8){
      if ('1' %in% cicheck & QAcheck == i){
        for (j in 1:length(study_level$ID)){
          if (study_level[j,i+8] == 1){
            arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                   length=0.05, angle=90, code=3, col="green")
          }
          else if (study_level[j,i+8] == 2){
            arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                   length=0.05, angle=90, code=3, col="red")
          }
          else {
            arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                   length=0.05, angle=90, code=3, col="lavenderblush3")
          }
        }
      }
      if('2' %in% cicheck & QAcheck == i){
        for (j in 1:length(study_level$ID)){
          if (study_level[j,i+8] == 1){
            arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                   length=0.05, angle=90, code=3, col = "green")
          }
          else if (study_level[j,i+8] == 2){
            arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                   length=0.05, angle=90, code=3, col = "red")
          }
          else {
            arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                   length=0.05, angle=90, code=3, col = "lavenderblush3")
          }
        }
      }
    }
  }
  
  # Plots when covariate data but no quality assessment data 
  if (C > 6 & Names[7] != "rob_PS"){
    
    for(i in 1:no_cov){
      if(covcheck == i+1){
        if(weightcheck == FALSE){
          if(cov_toggle == 3){
            # plot covariates as text and coloured points
            text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
            points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
          }
          if(cov_toggle == 2){
            # plot covariates as coloured points
            points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
          }
          if(cov_toggle == 1){
            # plot covariates as text and coloured points
            text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
          }
        }
        if(weightcheck == TRUE){
          if(cov_toggle == 3){
            # plot covariates as text and coloured points
            text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
            draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
          }
          if(cov_toggle == 2){
            # plot covariates as coloured points
            draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
          }
          if(cov_toggle == 1){
            # plot covariates as text and coloured points
            text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
          }
        }
      }
    }
    
    # plot sensitivity and specificity
    # When no covariates are selected (sens)
    if('1' %in% cicheck & covcheck == 1){
      arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
             length=0.05, angle=90, code=3)
    }
    #When covariates are selceted as text (sens)
    if('1' %in% cicheck & covcheck != 1){
      if(cov_toggle == 1){
        arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
               length=0.05, angle=90, code=3)
      }
    }
    #When no covariates are selected (spec)
    if('2' %in% cicheck & covcheck == 1){
      arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
             length=0.05, angle=90, code=3)
    }
    # When covariates are selected as text (spec)
    if('2' %in% cicheck & covcheck != 1){
      if(cov_toggle == 1){
        arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
               length=0.05, angle=90, code=3)
      }
    }
    
    for(i in 1:no_cov){
      # When covariates are selected as coloured points (sens)
      if('1' %in% cicheck & covcheck == i+1){
        if(cov_toggle == 2 | cov_toggle == 3){
          arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                 length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
        }
      }
      # When covariates are selected as coloured points (spec)
      if('2' %in% cicheck & covcheck == i+1){
        if((cov_toggle == 2 | cov_toggle == 3)){
          arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                 length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
        }
      }
    }
  }
  
  # Plots when quality assessment data and covariate data is available 
  if(C > 13 & Names[13] == "ac_RS"){
    
    if(weightcheck == FALSE){
      # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
      for (i in 2:8){
        if(QAcheck == i){
          for (k in 1:length(study_level$ID)){
            if (study_level[k,i+8] == 1){
              points(study_level$FPR[k], study_level$Sensitivity[k], pch=19, col="green")
            }
            else if (study_level[k,i+8] == 2){
              points(study_level$FPR[k], study_level$Sensitivity[k], pch=19, col="red")
            }
            else {
              points(study_level$FPR[k], study_level$Sensitivity[k], pch=19, col="lavenderblush3")
            }
          }
        }
      }
      
      for(i in 1:no_cov){
        if(covcheck == i+1){
          if(cov_toggle == 3){
            # plot covariates as text and coloured points
            text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
            if(QAcheck == 1){
              points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
            }
            else{
              points(study_level$FPR, study_level$Sensitivity, pch=0, col=as.factor(covariates[,i]), cex = 1.2)
            }
          }
          if(cov_toggle == 2){
            # plot covariates as coloured points 
            if(QAcheck == 1){
              points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
            }
            else{
              points(study_level$FPR, study_level$Sensitivity, pch=0, col=as.factor(covariates[,i]), cex =1.2)
            }
          }
          if(cov_toggle == 1){
            # plot covariates as text and coloured points
            text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
          }
        }
      }
    }  
    
    if(weightcheck == TRUE){
      # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
      for (i in 2:8){
        if(QAcheck == i){
          for (k in 1:length(study_level$ID)){
            if (study_level[k,i+8] == 1){
              draw.ellipse(study_level$FPR[k], study_level$Sensitivity[k], a=pctsp[k]/1000, b=pctse[k]/1000, col ="green")
            }
            else if (study_level[k,i+8] == 2){
              draw.ellipse(study_level$FPR[k], study_level$Sensitivity[k], a=pctsp[k]/1000, b=pctse[k]/1000, col ="red")
            }
            else {
              draw.ellipse(study_level$FPR[k], study_level$Sensitivity[k], a=pctsp[k]/1000, b=pctse[k]/1000, col ="lavenderblush3")
            }
          }
        }
      }
      
      for(i in 1:no_cov){
        if(covcheck == i+1){
          if(cov_toggle == 3){
            # plot covariates as text and coloured points
            text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
            if(QAcheck == 1){
              points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
              draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
              
            }
            else{
              draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = as.factor(covariates[,i]))
              
            }
          }
          if(cov_toggle == 2){
            # plot covariates as coloured points 
            if(QAcheck == 1){
              draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
            }
            else{
              draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = as.factor(covariates[,i]))
            }
          }
          if(cov_toggle == 1){
            # plot covariates as text and coloured points
            text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
          }
        }
      }
    }
    
    
    
    # Pie charts to represent study level esitmates split into risk of bias and applicability concerns 
    # Pie charts for risk of bias
    if ('9' %in% QAcheck){
      for (i in 1:max(P_rob$id)){
        a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
        score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
        pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015,
                 col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
      }
    }
    # Pie charts for applicability concerns
    if ('10' %in% QAcheck){
      for (i in 1:max(P_ac$id)){
        a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
        score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
        pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                 col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
      }
    }
    # Pie chart that shows both applicability concerns and risk of bias
    if ('11' %in% QAcheck){
      for (i in 1:max(P_both$id)){
        a_pie3 <- rep(P_ac$id[i], times = 7) # pie chart evenly split
        #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
        score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
        pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                 col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
      }
    }
    
    if('1' %in% cicheck){
      # plot sensitvity when no covariates selected no quality assesment (except piercharts)
      if(covcheck == 1 & (QAcheck == 1 | QAcheck == 9 |QAcheck == 10 |QAcheck == 11)){
        arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
               length=0.05, angle=90, code=3)
      }
      # plot sensitivity when covariates only dispalyed as text no quality assesment (except piercharts)
      if(covcheck != 1 & (QAcheck == 1 | QAcheck == 9 |QAcheck == 10 |QAcheck == 11)){
        if(cov_toggle == 1){
          arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                 length=0.05, angle=90, code=3)
        }
      }
      # Plot sensitivity when quality assessment only and no covariates
      for (i in 2:8){
        if(covcheck == 1 & QAcheck == i){
          for (j in 1:length(study_level$ID)){
            if (study_level[j,i+8] == 1){
              arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                     length=0.05, angle=90, code=3, col="green")
            }
            else if (study_level[j,i+8] == 2){
              arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                     length=0.05, angle=90, code=3, col="red")
            }
            else {
              arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                     length=0.05, angle=90, code=3, col="lavenderblush3")
            }
          }
        }
      }
      # Plot sensitivity when quality assessment selected and covariates displayed as only text
      for (i in 2:8){
        if(covcheck != 1 & QAcheck == i){
          if(cov_toggle == 1){
            for (j in 1:length(study_level$ID)){
              if (study_level[j,i+8] == 1){
                arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                       length=0.05, angle=90, code=3, col="green")
              }
              else if (study_level[j,i+8] == 2){
                arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                       length=0.05, angle=90, code=3, col="red")
              }
              else {
                arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                       length=0.05, angle=90, code=3, col="lavenderblush3")
              }
            }
          }
        }
      }
      # Plot sensitivity analysis when covariate selected as coloured and no quality assessment (except pie charts)
      for(i in 1:no_cov){
        if((QAcheck == 1 |QAcheck == 9 |QAcheck == 10 |QAcheck == 11) & covcheck == i+1){
          if(cov_toggle == 2 | cov_toggle == 3){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                   length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
          }
        }
      }
      
      # Plot sensitivity when covariates and quality assessment both selected as colours
      if(covcheck != 1 & (QAcheck == 2 |QAcheck == 3 | QAcheck == 4 |QAcheck == 5 | QAcheck == 6 |QAcheck == 7 | QAcheck == 8)){
        if(cov_toggle == 2 | cov_toggle == 3){
          # Plot default black
          if(ci_col == 1){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length =0.05, angle = 90, code =3)
          }
          # Plot colour as quality assessment 
          if(ci_col == 2){
            for(i in 2:8){
              if (QAcheck == i){
                for (j in 1:length(study_level$ID)){
                  if (study_level[j,i+8] == 1){
                    arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                           length=0.05, angle=90, code=3, col="green")
                  }
                  else if (study_level[j,i+8] == 2){
                    arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                           length=0.05, angle=90, code=3, col="red")
                  }
                  else {
                    arrows(study_level$FPR[j], study_level$Sens_LCI[j], study_level$FPR[j], study_level$Sens_UCI[j],
                           length=0.05, angle=90, code=3, col="lavenderblush3")
                  }
                }
              }
            }
          }
          # Plot colour as covariate
          if(ci_col == 3){
            for(i in 1:no_cov){
              if(covcheck == i+1){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                       length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
              }
            }
          }
        }
      }
    }
    
    if('2' %in% cicheck){
      # plot specificity when no covariates selected no quality assessment (except pie charts)
      if(covcheck == 1 & (QAcheck == 1 | QAcheck == 9 |QAcheck == 10 |QAcheck == 11)){
        arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
               length=0.05, angle=90, code=3)
      }
      # plot specificity when covariates only displayed as text no quality assessment (except pie charts)
      if(covcheck != 1 & (QAcheck == 1 | QAcheck == 9 |QAcheck == 10 |QAcheck == 11)){
        if(cov_toggle == 1){
          arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                 length=0.05, angle=90, code=3)
        }
      }
      # plot specificity when quality assessment only and no covariates 
      for (i in 2:8){
        if(covcheck == 1 & QAcheck == i){
          for (j in 1:length(study_level$ID)){
            if (study_level[j,i+8] == 1){
              arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                     length=0.05, angle=90, code=3, col = "green")
            }
            else if (study_level[j,i+8] == 2){
              arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                     length=0.05, angle=90, code=3, col = "red")
            }
            else {
              arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                     length=0.05, angle=90, code=3, col="lavenderblush3")
            }
          }
        }
      }
      # plot specificity when quality assessment selected and covariates displayed as only text
      for (i in 2:8){
        if(covcheck != 1 & QAcheck == i){
          if(cov_toggle == 1){
            for (j in 1:length(study_level$ID)){
              if (study_level[j,i+8] == 1){
                arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                       length=0.05, angle=90, code=3, col = "green")
              }
              else if (study_level[j,i+8] == 2){
                arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                       length=0.05, angle=90, code=3, col = "red")
              }
              else {
                arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                       length=0.05, angle=90, code=3, col="lavenderblush3")
              }
            }
          }
        }
      }
      # plot specificity when covariate selected as coloured and no quality assessment (except pie charts)
      for(i in 1:no_cov){
        if((QAcheck == 1 |QAcheck == 9 |QAcheck == 10 |QAcheck == 11) & covcheck == i+1){
          if(cov_toggle == 2 | cov_toggle == 3){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                   length=0.05, angle=90, code=3, col= as.factor(covariates[,i]))
          }
        }
      }
      # plot specificity when covariates and quality assessment both selected as colours 
      if(covcheck != 1 & (QAcheck == 2 |QAcheck == 3 | QAcheck == 4 |QAcheck == 5 | QAcheck == 6 |QAcheck == 7 | QAcheck == 8)){
        if(cov_toggle == 2 | cov_toggle == 3){
          # Plot default black
          if(ci_col == 1){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                   length=0.05, angle=90, code=3)
          }
          # Plot colour as quality assessment 
          if(ci_col == 2){
            for(i in 2:8){
              if (QAcheck == i){
                for (j in 1:length(study_level$ID)){
                  if (study_level[j,i+8] == 1){
                    arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                           length=0.05, angle=90, code=3, col = "green")
                  }
                  else if (study_level[j,i+8] == 2){
                    arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                           length=0.05, angle=90, code=3, col = "red")
                  }
                  else {
                    arrows(study_level$FPR_LCI[j], study_level$Sensitivity[j], study_level$FPR_UCI[j], study_level$Sensitivity[j],
                           length=0.05, angle=90, code=3, col="lavenderblush3")
                  }
                }
              }
            }
          }
          # Plot colour as covariate
          if(ci_col == 3){
            for(i in 1:no_cov){
              if(covcheck == i+1){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                       length=0.05, angle=90, code=3, col= as.factor(covariates[,i]))
              }
            }
          }
        }
      }
    }
  } # end of quality assessment and covariate plots
  
  # Add the ROC curve
  if ('2' %in% HSROCcheck){
    if (extrapp==T){points(roc_points, type="l")}
    else points(roc_points2, type="l", ann=F)}
  
  # Add summary point
  if ('1' %in% bivcheck){points(mean_point, col="blue", pch=15)}
  
  # Add confidence region
  if ('2' %in% bivcheck){lines(conf_region, lty=2, col="blue")}
  if ('3' %in% bivcheck){lines(pred_region, lty=3, col="blue")}
  
  # Add the legend 
  legend("bottomright", bty ="n", leglabticks, pch = legendticks[,1], lty = legendticks[,2], lwd=leg_lwd, col = leg_col)
}