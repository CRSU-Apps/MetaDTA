
function(input, output, session) {
  
  google_analytics_header_server(id = "analytics", app_name = "MetaDTA", google_analytics_id = "UA-135597033-2")
  
  HomePageServer(id = "home")
  
  data <- DataPageServer(id = "data")
  
  AnalysisPageServer(id = "analysis", data = data)
  
  ################################
  ### Sensitivity analysis tab ###
  ################################
  
  # Display input options for the SA tab
  output$SA_input <- renderUI({
    times <- input$SA_reset
    div(id=letters[(times %% length(letters)) + 1],
        checkboxGroupInput(inputId = "HSROCcheck2", label = h4("Options for SROC plot tab"),
                           choices = list("Data points"=1, "SROC curve"=2),
                           selected=list(1)),
        uiOutput("extrap2"),
        checkboxGroupInput(inputId = "bivcheck2", label = "Bivariate model options",
                           choices = list("Summary point"=1, "95% Confidence region"=2, "95% Predictive region"=3),
                           selected=list(1,2,3)),
        checkboxInput(inputId = "prevcheck2", label = "Display disease prevalence"),
        checkboxInput(inputId = "weightcheck2", label = "Display percentage weights"),
        checkboxGroupInput(inputId = "mod_removal", label = "Select which model to display",
                           choices = list("Original model" = 1, "Sensitivity analysis model" = 2), 
                           selected = list(1,2)),
        checkboxGroupInput(inputId = "cicheck2", label = "Display 95% study level confidence intervals",
                           choices = list("Sensitivity"=1, "Specificity"=2)),
        uiOutput("ci_colour2"),
        uiOutput("QAoption2"),
        uiOutput("covariate2"),
        uiOutput("covariate_option2"),
        checkboxGroupInput(inputId="statscheck2", label=h4("Options for Statistics tab"),
                           choices=list("Sensitivity"=1, "Specificity"=2, "False Positive Rate"=3, "Correlation"=4,
                                        "HSROC parameters"=5, "Diagnostic Odds Ratio"=6, "Likelihood Ratios"=7),
                           selected=list(1,2,3)),
        uiOutput("trials")
      )
  })
  
  # Display extrapolate option only when SROC curve selected
  output$extrap2 <- renderUI({
    if ('2' %in% input$HSROCcheck2){
      checkboxInput(inputId = "extrapp2", label = "Extrapolate SROC curve (Note: In most cases extrapolation should be avoided)", value = FALSE)
    }
    else{}
  })
  
  # Display option to change plotted CI colour when both covariate and quality assessment data are available
  output$ci_colour2 <- renderUI({
    cData <- data()
    C <- length(cData[1,])
    Names <- colnames(cData)
    if(C > 13 & Names[13] == "ac_RS"){
      if('1' %in% input$cicheck2 | '2' %in% input$cicheck2 |('1' %in% input$cicheck2 & '2' %in% input$cicheck2)){
        if(input$covcheck2 != 1 & (input$QAcheck2 == 2 |input$QAcheck2 == 3 | input$QAcheck2 == 4 | input$QAcheck2 == 5 |input$QAcheck2 == 6 | input$QAcheck2 == 7 | input$QAcheck2 == 8)){
          if(input$cov_toggle2 == 2 | input$cov_toggle2 == 3){
            radioButtons(inputId = "ci_col2", label = "Choose how to colour the confidence intervals", 
                         choices = list("Black" = 1, "Quality assessment" = 2, "Covariate" = 3), selected  = 1)
          }
        }
      }
    }
    else{}
  })
  
  # Display option to highlight quality assessment results if the data is available
  output$QAoption2 <- renderUI({
    cData <- data()
    C <- length(cData[1,])
    Names <- colnames(cData)
    if((C == 13 & Names[7] == "rob_PS") | (C > 13 & Names[13] == "ac_RS")){
      selectInput(inputId = "QAcheck2", label = "Display quality assessment scores",
                  choices = list("None"=1, "Risk of bias: Patient selection"=2, "Risk of bias: Index test"=3,
                                 "Risk of bias: Reference standard"=4, "Risk of bias: Flow & timing"=5,
                                 "Applicability concerns: Patient selection"=6, "Applicability concerns: Index test"=7,
                                 "Applicability concerns: Reference standard"=8, "Risk of bias (all)"=9, "Applicability concerns (all)"=10,
                                 "Both risk of bias and applicability concerns"=11),
                  selected = 1)
    }
    else{return()}
  })  
  
  
  # Display option to plot covariate 
  output$covariate2 <- renderUI({
    cData <- data()
    C <- length(cData[1,])
    Names <- colnames(cData)
    initial <- c("None")
    if((C > 6 & Names[7] != "rob_PS") | (C > 13 & Names[13] == "ac_RS")){
      if(C > 6 & Names[7] != "rob_PS"){
        if(C == 7){
          covariates <- colnames(cData[7])
        }
        else {
          covariates <- colnames(cData[,7:C])
        }
      }
      if(C > 13 & Names[13] == "ac_RS"){
        if(C == 14){
          covariates <- colnames(cData[14])
        }
        else{
          covariates <- colnames(cData[,14:C])
        }
      }
      combined <- c(initial, covariates)
      number <- 1:length(combined)
      choicesCov <- setNames(number, combined)
      selectInput(inputId = "covcheck2", label="Display covariates", choices = choicesCov, selected = 1)
    }
  })  
  
  # Display buttons to control how covariate is displayed 
  output$covariate_option2 <- renderUI({
    cData <- data()
    C <- length(cData[1,])
    Names <- colnames(cData)
    if((C > 6 & Names[7] != "rob_PS") | (C > 13 & Names[13] == "ac_RS")){
      if(input$covcheck2 != 1){
        radioButtons(inputId = "cov_toggle2", label = "Display options for covariates", 
                     choices = list("Text" = 1, "Coloured points"= 2, "Both" =3), selected = 1)
      }
    }
    else{}
  })
  
  #When data is loaded need a list of study names so that they can then be ticked to include studies
  #Add this list to the side bar panel on the left
  output$trials <- renderUI({
    if(is.null(data())){return()}
    else
      aData <- data()
    sadf <- data.frame(Author=aData$author, Year=aData$year, TP=aData$TP, FN=aData$FN, FP=aData$FP, TN=aData$TN)
    NT <- nrow(sadf) # count the number of rows of data
    a <- as.factor(sadf$Author)
    choicesTrials <- setNames(as.numeric(a), sadf$Author)
    checkboxGroupInput("triallist", label=h4("Select studies to include:"), choices=choicesTrials, selected=choicesTrials) #list of study names
  })
  
  # Create a table which displays study-level outcomes
  output$table2 <- DT::renderDataTable({
    if(is.null(data())){return()}
    else 
      D <- data()
      X <- D[input$triallist, ]
      b <- study_level_outcomes(X) # get sens, spec for each trial
      bb <- data.frame(Author =  X$author, Year = X$year, TP = X$TP, FN = X$FN, FP = X$FP, TN  = X$TN, N = b$N, Sens = b$Sens, Spec = b$Spec)
      bb$Sens <- sprintf('%4.3f', bb$Sens)
      bb$Spec <- sprintf('%4.3f', bb$Spec)
      #Add information about percentage weights 
      N <- length(X$TP)
      X$n1 <- X$TP+X$FN
      X$n0 <- X$FP+X$TN
      X$true1 <- X$TP
      X$true0 <- X$TN 
      X$study <- 1:N
      Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                  timevar = "sens" , times = c(1,0) , v.names = c("n","true") ) 
      Y = Y[order(Y$id),]  
      Y$spec<- 1-Y$sens
      MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                    data = Y , family = binomial , nAGQ=1 , verbose=0 ) 
      ma_Y = summary(MA_Y)
      labels(ma_Y) 
      ma_Y$coeff 
      varA    <- ma_Y$varcor$study[1,1]
      varB    <- ma_Y$varcor$study[2,2]
      covAB   <- ma_Y$vcov@x[2]
      Y_pw = reshape(X, direction = "long", varying = list( c("n0" , "n1") , c( "true0","true1" ) ) ,
                     timevar = "spec" , times = c(1,0) , v.names = c("n","true") )
      Y_pw = Y_pw[order(Y_pw$id),]
      Y_pw$sens<- 1-Y_pw$spec
      X_pw <- cbind(Y_pw$sens, Y_pw$spec)
      XT_pw <- t(X_pw)
      Z <- diag(2*N)
      invn <- 1/Y_pw$n
      A <- diag(invn)
      p_pw <- predict(MA_Y, type="response")
      var_pw <- p_pw*(1-p_pw)
      B <- diag(var_pw)
      G_one <- matrix(c(varA,covAB,covAB,varB),2,2)
      G <- do.call(adiag, replicate(N, G_one, simplify = FALSE))
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
      pctse <- vector(mode="numeric", length =N)
      pctsp <- vector(mode="numeric", length =N)
      for (i in 1:N){
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
      bb$Weight_Sens <- sprintf('%4.3f', pctse)
      bb$Weight_Spec <- sprintf('%4.3f', pctsp)
      options(DT.options = list(pageLength = 30))
      datatable(bb)
  })

  # Allow users the option to download the table of study-level outcomes
  output$downloadTable2 <- downloadHandler (
    #Specify the file name
    filename = function(){
      paste("table2.csv")
    },
    content = function(file){
      #open the device
      #create the plot
      # close the device
      if(is.null(data())){return()}
      else 
        D <- data()
      X <- D[input$triallist, ]
      b <- study_level_outcomes(X) # get sens, spec for each trial
      bb <- data.frame(Author =  X$author, Year = X$year, TP = X$TP, FN = X$FN, FP = X$FP, TN  = X$TN, N = b$N, Sens = b$Sens, Spec = b$Spec)
      bb$Sens <- sprintf('%4.3f', bb$Sens)
      bb$Spec <- sprintf('%4.3f', bb$Spec)
      #Add information about percentage weights 
      N <- length(X$TP)
      X$n1 <- X$TP+X$FN
      X$n0 <- X$FP+X$TN
      X$true1 <- X$TP
      X$true0 <- X$TN 
      X$study <- 1:N
      Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                  timevar = "sens" , times = c(1,0) , v.names = c("n","true") ) 
      Y = Y[order(Y$id),]  
      Y$spec<- 1-Y$sens
      MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                    data = Y , family = binomial , nAGQ=1 , verbose=0 ) 
      ma_Y = summary(MA_Y)
      labels(ma_Y) 
      ma_Y$coeff 
      varA    <- ma_Y$varcor$study[1,1]
      varB    <- ma_Y$varcor$study[2,2]
      covAB   <- ma_Y$vcov@x[2]
      Y_pw = reshape(X, direction = "long", varying = list( c("n0" , "n1") , c( "true0","true1" ) ) ,
                     timevar = "spec" , times = c(1,0) , v.names = c("n","true") )
      Y_pw = Y_pw[order(Y_pw$id),]
      Y_pw$sens<- 1-Y_pw$spec
      X_pw <- cbind(Y_pw$sens, Y_pw$spec)
      XT_pw <- t(X_pw)
      Z <- diag(2*N)
      invn <- 1/Y_pw$n
      A <- diag(invn)
      p_pw <- predict(MA_Y, type="response")
      var_pw <- p_pw*(1-p_pw)
      B <- diag(var_pw)
      G_one <- matrix(c(varA,covAB,covAB,varB),2,2)
      G <- do.call(adiag, replicate(N, G_one, simplify = FALSE))
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
      pctse <- vector(mode="numeric", length =N)
      pctsp <- vector(mode="numeric", length =N)
      for (i in 1:N){
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
      bb$Weight_Sens <- sprintf('%4.3f', pctse)
      bb$Weight_Spec <- sprintf('%4.3f', pctsp)
      options(DT.options = list(pageLength = 30))
      datatable(bb)
      write.table(bb, file, sep=",", row.names=FALSE)
      
    }
  )
  
  
  # Plot an interactive SROC curve for sensitivity analysis
  output$sroc_sa <- renderPlot({
    if(is.null(data())){return()}
    else
      X <- data()
    # Count the number of studies
    N <- length(X$TP)
    # Count the length of the data
    C <- length(X[1,])
    # Store the names of the columns
    # Used to determine if covariates are present 
    Names <- colnames(X)
    ## Set up the data
    ## Generate 5 new variables of type long. We need these before we can reshape the data.
    # n1 is number diseased
    # n0 is number without disease
    # true1 is number of true positives
    # true0 is the number of true negatives
    #	study is the unique identifier for each study. _n will generate a sequence of numbers.

    X$n1 <- X$TP+X$FN
    X$n0 <- X$FP+X$TN
    X$true1 <- X$TP
    X$true0 <- X$TN
    X$study <- 1:N

    ## Reshape the data from wide to long format ###

    Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c("true1","true0")) ,
                timevar = "sens" , times = c(1,0) , v.names = c("n","true"))

    ## Sort data by study to cluster the 2 records per study together ###
    Y = Y[order(Y$id),]
    Y$spec<- 1-Y$sens

    ## Perform meta-analysis ##
    MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                  data = Y , family = binomial , nAGQ=1 , verbose=0 )

    # More detail can be obtained by using the summary command
    ma_Y = summary(MA_Y)

    ## For the full list of outputs
    labels( ma_Y )

    ## Therefore, to extract the coefficients
    ma_Y$coeff

    # Logit sensitivity and specificity
    lsens = ma_Y$coeff[1,1]
    lspec = ma_Y$coeff[2,1]

    #HSROC parameters
    # Use estimates of bivariate model parameters to calculate HSROC parameters
    sigma2_a <- ma_Y$varcor$study[1]
    sigma2_b <- ma_Y$varcor$study[4]
    sigma_ab <- ma_Y$varcor$study[2]
    sigma_a <- sqrt(sigma2_a)
    sigma_b <- sqrt(sigma2_b)

    beta <- log(sigma_b/sigma_a)
    Theta <- 0.5*( (((sigma_b/sigma_a)^0.5 )*lsens) - ( ( (sigma_a/sigma_b)^0.5) *lspec) )
    Lambda <- ( ( (sigma_b/sigma_a)^0.5) * lsens) + ( (sigma_a/sigma_b)^0.5 *lspec)
    sigma2_theta <- 0.5*( (sigma_a*sigma_b) - sigma_ab )
    sigma2_alpha <- 2*( (sigma_a*sigma_b) + sigma_ab )

    # Calcuate points for ROC curve
    # Want to plot for Specifity ranging between 0.5 and 1 (then FPR ranges between 0.5 and 0)
    Sp <- 0.5
    Fpr <- 1-Sp
    LSp <- qlogis(Sp)
    LSe <- Lambda*exp(-beta/2) - exp(-beta)*LSp
    Se <- plogis(LSe)

    # Empty data frame
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

    # Confidence region based on Stata metandi commands
    seB <- ma_Y$coefficients[2,2]
    seA <- ma_Y$coefficients[1,2]
    r <- ma_Y$vcov@x[2] / (seA*seB)
    level <- 95
    f <- qf(0.95, df1=2, df2=N-2)
    croot <- sqrt(2*f)

    # Empty data frame
    conf_region <- (rep(NA, 361))
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

    # Prediction region based on Stata metandi commands
    varA <- ma_Y$varcor$study[1,1]
    varB <- ma_Y$varcor$study[2,2]
    seB <- ma_Y$coefficients[2,2]
    seA <- ma_Y$coefficients[1,2]
    sAB <- ma_Y$varcor$study[1,2]
    covAB <- ma_Y$vcov@x[2]
    sepredA <- sqrt( varA + seA^2 )
    sepredB <- sqrt( varB + seB^2 )
    rpredAB <- (sAB + covAB) / (sepredA*sepredB)
    level <- 95
    f <- qf(0.95, df1=2, df2=N-2)
    croot <- sqrt(2*f)

    # Empty data frame
    pred_region <- (rep(NA, 361))
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
    
    # Round the prevalence to 2 decimal places 
    study_level[,'Prev'] <- round(study_level[,'Prev'] , 2)

    # Calculate sens and spec confidence intervals at the study level
    # Add the confidence intervals to the dataset
    foreach (i=1:N) %do% {
      study_level$Sens_LCI[i] <- binconf(study_level$TP[i], study_level$TP[i]+study_level$FN[i], method="exact")[2]
      study_level$Sens_UCI[i] <- binconf(study_level$TP[i], study_level$TP[i]+study_level$FN[i], method="exact")[3]
      study_level$FPR_LCI[i]  <- 1 - binconf(study_level$TN[i], study_level$FP[i]+study_level$TN[i], method="exact")[3]
      study_level$FPR_UCI[i]  <- 1 - binconf(study_level$TN[i], study_level$FP[i]+study_level$TN[i], method="exact")[2]
    }

    # Mean point
    Sens = plogis(lsens)
    Spec = plogis(lspec)
    fpr <- 1-Spec
    mean_point=data.frame(x=fpr, y=Sens)
    leglabticks <- matrix(nrow=10, ncol=1) #will put legend labels in this vector
    legendticks <- matrix(nrow=10, ncol=2) #symbols column 1 for pch column 2 for lty
    leglabticks[1] <- "Original model"
    legendticks[1,1]<-3

    # Calculate min and max values of sens and spec to avoid extrapolation of SROC curve
    minSens <- min(X$sens)
    maxSens <- max(X$sens)
    minFPR <- min(X$fpr)
    maxFPR <- max(X$fpr)

    # Create new data frame which restricts roc_points to being between min and max values
    roc_points2 <- subset(roc_points, FPR<maxFPR & FPR>minFPR)
    
    ### Fit model for sensitivity analysis
    P <- X[input$triallist, ] #data set of studies in the analysis
    # Count the number of studies
    N1 <- length(P$TP)
    ## Set up the data
    ## Generate 5 new variables of type long. We need these before we can reshape the data.
    # n1 is number diseased
    # n0 is number without disease
    # true1 is number of true positives
    # true0 is the number of true negatives
    # study is the unique identifier for each study. _n will generate a sequence of numbers.

    P$na1 <- P$TP+P$FN
    P$na0 <- P$FP+P$TN
    P$truea1 <- P$TP
    P$truea0 <- P$TN
    P$studya <- 1:N1

    ## Reshape the data from wide to long format ###

    Q = reshape(P, direction = "long", varying = list( c("na1" , "na0") , c( "truea1","truea0" ) ) ,
                timevar = "sens" , times = c(1,0) , v.names = c("na","truea") )

    ## Sort data by study to cluster the 2 records per study together ###
    Q = Q[order(Q$id),]
    Q$spec<- 1-Q$sens

    ## Perform meta-analysis ##
    MA_Q = glmer(formula = cbind(truea, na - truea) ~ 0 + sens + spec + (0+sens + spec|studya),
                  data = Q , family = binomial , nAGQ=1 , verbose=0)

    # More detail can be obtained by using the summary command
    ma_Q = summary(MA_Q)

    ## For the full list of outputs
    labels(ma_Q)

    ## Therefore, to extract the coefficients
    ma_Q$coeff

    # Logit sensitivity and specificity
    lsens_a = ma_Q$coeff[1,1]
    lspec_a = ma_Q$coeff[2,1]

    #HSROC parameters
    # Use estimates of bivariate model parameters to calculate HSROC parameters
    sigma2_a_a <- ma_Q$varcor$study[1]
    sigma2_b_a <- ma_Q$varcor$study[4]
    sigma_ab_a <- ma_Q$varcor$study[2]
    sigma_a_a <- sqrt(sigma2_a_a)
    sigma_b_a <- sqrt(sigma2_b_a)

    beta_a <- log(sigma_b_a/sigma_a_a)
    Theta_a <- 0.5*( (((sigma_b_a/sigma_a_a)^0.5 )*lsens_a) - ( ( (sigma_a_a/sigma_b_a)^0.5) *lspec_a))
    Lambda_a <- ( ( (sigma_b_a/sigma_a_a)^0.5) * lsens_a) + ( (sigma_a_a/sigma_b_a)^0.5 *lspec_a)
    sigma2_theta_a <- 0.5*( (sigma_a_a*sigma_b_a) - sigma_ab_a )
    sigma2_alpha_a <- 2*( (sigma_a_a*sigma_b_a) + sigma_ab_a )

    # Calcuate points for ROC curve
    # Want to plot for Specifity ranging between 0.5 and 1 (then FPR ranges between 0.5 and 0)
    Sp_a <- 0.5
    Fpr_a <- 1-Sp_a
    LSp_a <- qlogis(Sp_a)
    LSe_a <- Lambda_a*exp(-beta_a/2) - exp(-beta_a)*LSp_a
    Se_a <- plogis(LSe_a)

    # Empty data frame
    roc_points_a <- rep(NA, 60)
    for (j in seq(from=0, to=1, by=0.01)){
      Sp_j <- j
      Fpr_j <- 1-Sp_j
      LSp_j <- qlogis(Sp_j)
      LSe_j <- Lambda_a*exp(-beta_a/2) - exp(-beta_a)*LSp_j
      Se_j <- plogis(LSe_j)
      roc_j <- data.frame(FPR=Fpr_j, Sen=Se_j)

      # Add results for most recent value to a data frame which contains the results of previous values
      roc_points_a <- rbind(roc_points_a,roc_j)
    }

    # Confidence region based on Stata metandi commands
    seB_a <- ma_Q$coefficients[2,2]
    seA_a <- ma_Q$coefficients[1,2]
    r_a <- ma_Q$vcov@x[2] / (seA_a*seB_a)
    level_a <- 95
    f_a <- qf(0.95, df1=2, df2=N1-2)
    croot_a <- sqrt(2*f_a)

    # Empty data frame
    conf_region_a <- (rep(NA, 361))
    for (j in seq(0, 2*pi, length.out=361)){
      confB_a <- lspec_a + (seB_a*croot_a*cos(j))
      confA_a <- lsens_a + (seA_a*croot_a*cos(j + acos(r_a)))
      confsens_a <- plogis(confA_a)
      confspec_a <- plogis(confB_a)
      conf_j <- data.frame(X=1-confspec_a, Y=confsens_a)

      # Add results for most recent value to a data frame which contains the results of previous values
      conf_region_a <- rbind(conf_region_a, conf_j)
    }
    conf_region_a <- conf_region_a[2:362,]

    # Prediction region based on Stata metandi commands
    varA_a <- ma_Q$varcor$study[1,1]
    varB_a <- ma_Q$varcor$study[2,2]
    seB_a <- ma_Q$coefficients[2,2]
    seA_a <- ma_Q$coefficients[1,2]
    sAB_a <- ma_Q$varcor$study[1,2]
    covAB_a <- ma_Q$vcov@x[2]
    sepredA_a <- sqrt( varA_a + seA_a^2 )
    sepredB_a <- sqrt( varB_a + seB_a^2 )
    rpredAB_a <- (sAB_a + covAB_a) / (sepredA_a*sepredB_a)
    level_a <- 95
    f_a <- qf(0.95, df1=2, df2=N1-2)
    croot_a <- sqrt(2*f_a)

    # Empty data frame
    pred_region_a <- (rep(NA, 361))
    for (j in seq(0, 2*pi, length.out=361)){
      predB_a <- lspec_a + (sepredB_a*croot_a*cos(j))
      predA_a <- lsens_a + (sepredA_a*croot_a*cos(j + acos(rpredAB_a)))
      predsens_a <- plogis(predA_a)
      predspec_a <- plogis(predB_a)
      pred_j <- data.frame(X=1-predspec_a, Y=predsens_a)
      # Add results for most recent value to a data frame which contains the results of previous values
      pred_region_a <- rbind(pred_region_a, pred_j)
    }
    pred_region_a <- pred_region_a[2:362,]

    # Sensitivity and specificity calculations for each study
    P$sens <- P$TP / (P$TP + P$FN)
    P$spec <- P$TN / (P$FP + P$TN)
    P$fpr <- 1- P$spec

    # Separate dataframes dependent on if quality assessment data is available
    # Need to identify if quality assessment data is present and if covariate data is present
    # If quality assessment data is avaialble need to create further dataframes to produce pie charts
    
    # No quality assessment, No covariates
    if (C == 6){
      study_level_P <- data.frame(ID=P$study, TP=P$TP, FN=P$FN, FP=P$FP, TN=P$TN, N=(P$TP+P$FN+P$FP+P$TN), 
                                Sensitivity=P$sens, Specificity=P$spec, FPR=P$fpr, Prev=((P$TP+P$FN)/(P$TP+P$FN+P$FP+P$TN)))
    }
    
    # No quality assessment, Yes Covariates
    if (C > 6 & Names[7] != "rob_PS"){
      study_level_P <- data.frame(ID=P$study, TP=P$TP, FN=P$FN, FP=P$FP, TN=P$TN, N=(P$TP+P$FN+P$FP+P$TN), 
                                Sensitivity=P$sens, Specificity=P$spec, FPR=P$fpr, Prev=((P$TP+P$FN)/(P$TP+P$FN+P$FP+P$TN)))
      if (C == 7){
        covariates_a <- cbind(as.character(P[,7]))
        no_cov <- 1
      }
      else {
        covariates_a <- P[,7:C] # extract all covariates 
        no_cov <- length(colnames(covariates_a)) # number of covariates
      }
      study_level_P <- cbind(study_level_P, covariates_a)# combine with study_level data frame
    }
    
    # Yes quality assessment, No covariates
    if (C == 13 & Names[7] == "rob_PS" ){
      study_level_P <- data.frame(ID=P$study, TP=P$TP, FN=P$FN, FP=P$FP, TN=P$TN, N=(P$TP+P$FN+P$FP+P$TN), 
                                Sensitivity=P$sens, Specificity=P$spec, FPR=P$fpr,rob_PS = P$rob_PS,
                                rob_IT = P$rob_IT, rob_RS = P$rob_RS,rob_FT = P$rob_FT,ac_PS = P$ac_PS,
                                ac_IT = P$ac_IT, ac_RS = P$ac_RS,  Prev = ((P$TP+P$FN)/(P$TP+P$FN+P$FP+P$TN)))
      # Reshape the data to allow for pie charts to be plotted as summary points
      # For ROB outcomes
      P_rob_a <- P[,-which(names(P) == "ac_PS" | names(P) == "ac_RS"| names(P) == "ac_IT")] # Delete applicability concerns for ROB plot
      P_rob_a <- reshape(P_rob_a, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"),
                       v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
      # For AC outcomes
      P_ac_a <- P[,-which(names(P) == "rob_PS" | names(P) == "rob_RS"| names(P) == "rob_IT"|names(P) == "rob_FT")] # Delete applicability concerns for ROB plot
      P_ac_a <- reshape(P_ac_a, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"),
                      v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
      # For both outcomes together
      P_both_a <- reshape(P, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                        v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
      
    }
    
    # Yes quality assessment, yes covariates 
    if (C > 13 & Names[13] == "ac_RS"){
      study_level_P <- data.frame(ID=P$study, TP=P$TP, FN=P$FN, FP=P$FP, TN=P$TN, N=(P$TP+P$FN+P$FP+P$TN), 
                                Sensitivity=P$sens, Specificity=P$spec, FPR=P$fpr,rob_PS = P$rob_PS,
                                rob_IT = P$rob_IT, rob_RS = P$rob_RS,rob_FT = P$rob_FT,ac_PS = P$ac_PS,
                                ac_IT = P$ac_IT, ac_RS = P$ac_RS,  Prev = ((P$TP+P$FN)/(P$TP+P$FN+P$FP+P$TN)))
      # Reshape the data to allow for pie charts to be plotted as summary points
      # For ROB outcomes
      P_rob_a <- P[,-which(names(P) == "ac_PS" | names(P) == "ac_RS"| names(P) == "ac_IT")] # Delete applicability concerns for ROB plot
      P_rob_a <- reshape(P_rob_a, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"),
                       v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
      # For AC outcomes
      P_ac_a <- P[,-which(names(P) == "rob_PS" | names(P) == "rob_RS"| names(P) == "rob_IT"|names(P) == "rob_FT")] # Delete applicability concerns for ROB plot
      P_ac_a <- reshape(P_ac_a, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"),
                      v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
      # For both outcomes together
      P_both_a <- reshape(P, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                        v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
      if (C == 14){
        covariates_a <- cbind(as.character(P[,14]))
        no_cov <- 1
      }
      else {
        covariates_a <- P[,14:C] # extract all covariates 
        no_cov <- length(colnames(covariates_a)) # number of covariates
      }
      study_level_P <- cbind(study_level_P, covariates_a)# combine with study_level data frame
      
    }
    
    # Round prevalence to 2 decimal places 
    study_level_P[,'Prev'] <- round(study_level_P[,'Prev'] , 2)

    # Calculate sens and spec confidence intervals at the study level
    # Add the confidence intervals to the dataset
    foreach (i=1:N1) %do% {
      study_level_P$Sens_LCI[i] <- binconf(study_level_P$TP[i], study_level_P$TP[i]+study_level_P$FN[i], method="exact")[2]
      study_level_P$Sens_UCI[i] <- binconf(study_level_P$TP[i], study_level_P$TP[i]+study_level_P$FN[i], method="exact")[3]
      study_level_P$FPR_LCI[i]  <- 1 - binconf(study_level_P$TN[i], study_level_P$FP[i]+study_level_P$TN[i], method="exact")[3]
      study_level_P$FPR_UCI[i]  <- 1 - binconf(study_level_P$TN[i], study_level_P$FP[i]+study_level_P$TN[i], method="exact")[2]
    }

    # Mean point
    Sens_a = plogis( lsens_a )
    Spec_a = plogis( lspec_a )
    fpr_a <- 1-Spec_a
    mean_point_a=data.frame(x=fpr_a, y=Sens_a)
    
    
    # Percentage weight calculations 
    # For model with all studies included 
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
    # For the sensitvity analysis model 
    Y_pw2 = reshape(P, direction = "long", varying = list( c("n0" , "n1") , c( "true0","true1" ) ) ,
                    timevar = "spec" , times = c(1,0) , v.names = c("n","true") )
    Y_pw2 = Y_pw2[order(Y_pw2$id),]
    Y_pw2$sens<- 1-Y_pw2$spec
    X_pw2 <- cbind(Y_pw2$sens, Y_pw2$spec)
    XT_pw2 <- t(X_pw2)
    Z2 <- diag(2*N1)
    invn2 <- 1/Y_pw2$n
    A2 <- diag(invn2)
    p_pw2 <- predict(MA_Q, type="response")
    var_pw2 <- p_pw2*(1-p_pw2)
    B2 <- diag(var_pw2)
    G_one2 <- matrix(c(varA_a,covAB_a,covAB_a,varB_a),2,2)
    G2 <- do.call(adiag, replicate(N1, G_one2, simplify = FALSE))
    #inverse of B (required later on)
    BI2 <- solve(B2)
    # Create varianbce matrix for observations
    V2 <- (Z2 %*% G2 %*% t(Z2)) + (A2 %*% BI2)
    # invert the variance matrix
    invV2 <- solve(V2)
    # derive the fishers information matrix
    fish2 <- XT_pw2 %*% invV2 %*% X_pw2
    # invert Fishers information to obtain Var Beta hat
    varb2 <- solve(fish2)
    pctse2 <- vector(mode="numeric", length =N1)
    pctsp2 <- vector(mode="numeric", length =N1)
    for (i in 1:N1){
      DM2 <- V2
      DM2[(i*2)-1, (i*2)-1] <- 1000000000
      DM2[(i*2)-1, (i*2)] <- 0
      DM2[(i*2), (i*2)-1] <- 0
      DM2[(i*2), (i*2)] <- 1000000000
      
      invDM2 <- solve(DM2)
      fishD2 <- XT_pw2 %*% invDM2 %*% X_pw2
      fishI2 <- fish2 - fishD2
      weight2 <- varb2 %*% fishI2 %*% varb2
      pctse2[i] <- 100*(weight2[1,1]/varb2[1,1])
      pctsp2[i] <- 100*(weight2[2,2]/varb2[2,2])
    }
    
    # Set up legend
    # Data no covariates no quality assessment
    if(C == 6){
      LL <- 7 # Specifies length of legend
      LL2 <- LL
      leg_col_SA <- c("white", "black", "black", "blue", "blue", "blue", "black")
      leg_col_O <- c("white", "gray", "gray", "gray", "gray", "gray", "gray")
      leg_col <- c(leg_col_O,leg_col_SA) # colours of the legend
      leg_lwd_SA <- c(NA, NA, 2, NA, 1, 1, NA)
      leg_lwd_O <- c(NA, NA, 2, NA, 1, 1, NA)
      leg_lwd <- c(leg_lwd_O, leg_lwd_SA) # line width

    }

    # Data with quality assessment only
    if( C == 13 & Names[7] == "rob_PS"){
      LL <- 10
      LL2 <- LL
      leg_col_O <- c("white","green", "red", "lavenderblush3", "gray", "gray", "gray", "gray", "gray", "gray")
      leg_col_SA <- c("white","green", "red", "lavenderblush3", "black", "black", "blue", "blue", "blue", "black")
      leg_lwd_O <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
      leg_lwd_SA <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
    }


    # Data with only covariates
    if (C > 6 & Names[7] != "rob_PS"){
      if(!('1' %in% input$covcheck2)){# generates legend to include covariates
        for(i in 1:no_cov){
          if(input$covcheck2 == i+1){
            #unique <- unique(covariates[,i]) # calculate the number of unique entrys for the selcted covariate
            unique <- unique(covariates_a[,i]) # Unique entrys for sens analysis
            leg_covnames <- sort(as.character(unique))
            col.rainbow <- matlab.like2(length(unique))
            #leg_covnames2 <- sort(as.character(unique2))
            LL <-  7
            LL2 <- length(unique) + 7
          }
        }
         leg_col <- rep(0, times = length(unique))
         for (i in 1:length(unique)){ # assigns colour to each level
           leg_col[i] <- palette(col.rainbow)[length(unique)+1-i]
         }
     
         leg_col_O <- c("white", "gray", "gray", "gray", "gray", "gray", "gray")
         leg_col_SA <-c("white", leg_col, "black", "black", "blue", "blue", "blue", "black")
    
         leg_lwd1 <- rep(NA, times = length(unique))
         leg_lwd_O <- c(NA, NA, 2, NA, 1, 1, NA)
         leg_lwd_SA <- c(NA, leg_lwd1, NA, 2, NA, 1, 1, NA)
      }
       else{
         LL <- 7
         LL2 <- LL
         leg_col_O <- c("white", "gray", "gray", "gray", "gray", "gray", "gray")
         leg_col_SA <- c("white", "black", "black", "blue", "blue", "blue", "black" )
         leg_lwd_O <- c(NA, NA, 2, NA, 1, 1, NA)
         leg_lwd_SA <- c(NA, NA, 2, NA, 1, 1, NA)
       }
    }

    # Data with covariates and QA data
    if(C > 13 & Names[13] == "ac_RS"){
      if(!('1' %in% input$covcheck2)){# generates legend to include covariates
        for(i in 1:no_cov){
          if(input$covcheck2 == i+1){
            #unique <- unique(covariates[,i]) # calculate the number of unique entrys for the selcted covariate
            unique <- unique(covariates_a[,i]) # Unique entrys for sens analysis
            leg_covnames <- sort(as.character(unique))
            col.rainbow <- matlab.like2(length(unique))
            #leg_covnames2 <- sort(as.character(unique2))
            LL <-  10
            LL2 <- length(unique) + 10
          }
        }
        leg_col <- rep(0, times = length(unique))
        for (i in 1:length(unique)){ # assigns colour to each level
          leg_col[i] <- palette(col.rainbow)[length(unique)+1-i]
        }

        leg_col_O <- c("white", "green", "red", "lavenderblush3", "gray", "gray", "gray", "gray", "gray", "gray")
        leg_col_SA <-c("white", leg_col, "green", "red", "lavenderblush3", "black", "black", "blue", "blue", "blue", "black")
        
        leg_lwd2 <- rep(NA, times = length(unique))
        leg_lwd_O <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
        leg_lwd_SA <- c(NA, leg_lwd2, NA, NA, NA, NA, 2, NA, 1, 1, NA)
      }
      else{
        LL <- 10
        LL2 <- LL
        leg_col_O <- c("white", "green", "red", "lavenderblush3", "gray", "gray", "gray", "gray", "gray", "gray")
        leg_col_SA <- c("white", "green", "red", "lavenderblush3", "black", "black", "blue", "blue", "blue", "black" )
        leg_lwd_O <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
        leg_lwd_SA <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
      }
    }

    leglabticks <- matrix(nrow=LL, ncol = 1)
    legendticks <- matrix(nrow=LL, ncol = 2)
    leglabticks2 <- matrix(nrow=LL2, ncol = 1)
    legendticks2 <- matrix(nrow=LL2, ncol = 2)

    leglabticks[1] <- "Original model"
    legendticks[1,1]<-3
    leglabticks2[1] <- "Sensitvity analysis model"
    legendticks2[1,1]<-3

    if('1' %in% input$HSROCcheck2 | input$prevcheck2 == T){
      leglabticks[LL]<- "Data"
      legendticks[LL]<- 1
      leglabticks2[LL2]<- "Data"
      legendticks2[LL2]<- 1
    }

    if ('3' %in% input$bivcheck2){
      leglabticks[LL-1]<-"95% Predictive region" #legend label
      legendticks[LL-1,2]<-3
      leglabticks2[LL2-1]<-"95% Predictive region" #legend label
      legendticks2[LL2-1,2]<-3
    }

    if ('2' %in% input$bivcheck2){
      leglabticks[LL-2]<-"95% Confidence region" #legend label
      legendticks[LL-2,2]<-2 #creates interactive vector
      leglabticks2[LL2-2]<-"95% Confidence region" #legend label
      legendticks2[LL2-2,2]<-2 #creates interactive vector
    }

    if ('1' %in% input$bivcheck2){
      leglabticks[LL-3]<-"Summary estimate" #legend label
      legendticks[LL-3,1]<-15 #change plotticks and legendticks if option 1 selected
      leglabticks2[LL2-3]<-"Summary estimate" #legend label
      legendticks2[LL2-3,1]<-15 #change plotticks and legendticks if option 1 selected
    }
    if ('2' %in% input$HSROCcheck2){
      leglabticks[LL-4]<-"HSROC curve"
      legendticks[LL-4,2]<-1
      leglabticks2[LL2-4]<-"HSROC curve"
      legendticks2[LL2-4,2]<-1
    }

    if (('1' %in% input$cicheck2) | ('2' %in% input$cicheck2)){
      leglabticks[LL-5]<-"Uncertainty"
      legendticks[LL-5,1]<-3
      leglabticks2[LL2-5]<-"Uncertainty"
      legendticks2[LL2-5,1]<-3
    }

    if((C == 13 & Names[7] == "rob_PS") |(C > 13 & Names[13] == "ac_RS")){
      if(input$QAcheck2 != 1){
        leglabticks[LL-6]<-"Unclear"
        leglabticks[LL-7]<-"High"
        leglabticks[LL-8]<-"Low"
        legendticks[LL-8,1]<-1
        legendticks[LL-7,1]<-1
        legendticks[LL-6,1]<-1
        leglabticks2[LL2-6]<-"Unclear"
        leglabticks2[LL2-7]<-"High"
        leglabticks2[LL2-8]<-"Low"
        legendticks2[LL2-8,1]<-19
        legendticks2[LL2-7,1]<-19
        legendticks2[LL2-6,1]<-19
      }
    }

    if(C > 6 & Names[7] != "rob_PS"){
      if(!('1' %in% input$covcheck2)){
        if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
          for(i in 1:length(leg_covnames)){
            leglabticks2[LL2-5-i]<-leg_covnames[i]
            legendticks2[LL2-5-i,1]<-19
          }

        }
        if('1' %in% input$cov_toggle2){
          leglabticks[LL]<-"Data"
          legendticks[LL,1]<-1
          leglabticks2[LL2]<-"Data"
          legendticks2[LL2,1]<-1
        }
      }
    }

    if(C > 13 & Names[13] == "ac_RS"){
      if(!(1 %in% input$covcheck2)){
        if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
          for(i in 1:length(leg_covnames)){
            leglabticks2[LL2-8-i]<-leg_covnames[i]
            if('1' %in% input$QAcheck2){
              legendticks2[LL2-8-i,1]<-19
            }
            else{
              legendticks2[LL2-8-i,1]<-0
            }
          }
        }
        if('1' %in% input$cov_toggle2){
          leglabticks[LL]<-"Data"
          legendticks[LL,1]<-1
          leglabticks2[LL2]<-"Data"
          legendticks2[LL2,1]<-1
        }
      }
    }

    # Calculate min and max values of sens and spec to avoid extrapolation of SROC curve
    minSens <- min(Q$sens)
    maxSens <- max(Q$sens)
    minFPR <- min(Q$fpr)
    maxFPR <- max(Q$fpr)

    # Create new data frame which restricts roc_points to being between min and max values
    roc_points2_a <- subset(roc_points_a, FPR<maxFPR & FPR>minFPR)

    # Try plotting ROC curve

    #First create a plot with just one point (ann=F means that default titles for title and axis titles are not added
    #so I can add them myself below)
    plot(1,1, ylim=c(0,1), xlim=c(0,1), ann=F, pch=20, col="white")

    # Next command allows me to overlay the next graph on top
    par(new=TRUE)

    # Add grid lines
    abline(v=(seq(0,1,0.2)), col="lightgray", lty="dotted")
    abline(h=(seq(0,1,0.2)), col="lightgray", lty="dotted")

    #Add titles
    title(main=input$title_sa, xlab="False Positive Rate", ylab="Sensitivity")

    #Plot study level estimates
    if ('1' %in% input$HSROCcheck2){
      if('1' %in% input$mod_removal){
        if(input$weightcheck2 == TRUE){
          draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = "gray")
        }
        else{
          points(study_level$FPR, study_level$Sensitivity, pch=1, col="gray")
        }
      }
      if('2' %in% input$mod_removal){
        if(input$weightcheck2 == TRUE){
          draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a=pctsp2/1000, b=pctse2/1000)
        }
        else{
          points(study_level_P$FPR, study_level_P$Sensitivity, pch=1)
        }
      }
    }
    
    #Plot prevalence as text 
    if (input$prevcheck2 == T){
      if('1' %in% input$mod_removal){ 
        text(study_level$FPR, study_level$Sensitivity, study_level$Prev, cex =0.7, pos = 1, col = "gray")
      }
      if('2' %in% input$mod_removal){
       text(study_level_P$FPR, study_level_P$Sensitivity, study_level_P$Prev, cex =0.7, pos = 1) 
      }
    }
    
    # Plots when quality assessment data available but no covariate data
    if(C == 6){
      # plot sens
      if('1' %in% input$cicheck2){
        if('1' %in% input$mod_removal){
          arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray")
        }
        if('2' %in% input$mod_removal){
          arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, length=0.05, angle=90, code=3)
        }
      }
      # plot spec
      if('2' %in% input$cicheck2){
        if('1' %in% input$mod_removal){
          arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray")
        }
        if('2' %in% input$mod_removal){
          arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, length=0.05, angle=90, code=3)
        }
      }
    }
    
    # Plots when quality assessment data is available 
    if (C == 13 & Names[7] == "rob_PS"){
      # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
      for (i in 2:8){
        if('1' %in% input$mod_removal){
          if(input$QAcheck2 == i){
            if(input$weightcheck2 == FALSE){
              points(study_level$FPR, study_level$Sensitivity, pch=1, col="gray")
            }
            if(input$weightcheck2 == TRUE){
              draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = "gray")
            }
          }
        }
        if('2' %in% input$mod_removal){
          if(input$QAcheck2 ==i){
            for(m in 1:length(study_level_P$ID)){
              if (input$weightcheck2 == FALSE){
                if (study_level_P[m,i+8] == 1){
                  points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="green")
                }
                else if (study_level_P[m,i+8] == 2){
                  points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="red")
                }
                else {
                  points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="lavenderblush3")
                }
              }
              if (input$weightcheck2 == TRUE){
                if(study_level_P[m,i+8] == 1){
                  draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "green")
                }
                if(study_level_P[m,i+8] == 2){
                  draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "red")
                }
                if(study_level_P[m,i+8] == 3){
                  draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "lavenderblush3")
                }
              }
            }
          }
        }
      }
      
      
      # Pie charts to represent study level estimates split into risk of bias and applicability concerns 
      # Pie charts for risk of bias 
      if ('9' %in% input$QAcheck2){
        if ('1' %in% input$mod_removal){
          for (i in 1:max(P_rob$id)){ # uncoloured plotws for the whole dataset
            a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
            score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015, #density = 2,
                     col = ifelse(score_pie1 == 1, "gray70", ifelse(score_pie1 == 2, "gray20", "gray45"))) # plots pie charts
          }
        }
        if ('2' %in% input$mod_removal){
          for (i in 1:max(P_rob_a$id)){ # coloured for those included in sensitivity analyses
            a_pie1 <- rep(P_rob_a$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
            score_pie1 <- P_rob_a$score[seq(i, length(P_rob_a$id), max(P_rob_a$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie1, P_rob_a$fpr[i], P_rob_a$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
          }
        }
      }
      # Pie charts for applicability concerns 
      if ('10' %in% input$QAcheck2){
        if ('1' %in% input$mod_removal){
          for (i in 1:max(P_ac$id)){
            a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
            score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie2 == 1, "gray70", ifelse(score_pie2 == 2, "gray20", "gray45"))) # plots pie charts
          }
        }
        if ('2' %in% input$mod_removal){
          for (i in 1:max(P_ac_a$id)){
            a_pie2 <- rep(P_ac_a$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
            score_pie2 <- P_ac_a$score[seq(i, length(P_ac_a$id), max(P_ac_a$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie2, P_ac_a$fpr[i], P_ac_a$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
          }
        }
      }
      # Pie chart that shows both applicability concerns and risk of bias 
      if ('11' %in% input$QAcheck2){
        if ('1' %in% input$mod_removal){
          for (i in 1:max(P_both$id)){ # uncoloured plots for the whole dataset
            a_pie3 <- rep(P_both$id[i], times = 7) # pie chart evenly split
            #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
            score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie3 == 1, "gray70", ifelse(score_pie3 == 2, "gray20", "gray45"))) # plots pie charts
          }
        }
        if ('2' %in% input$mod_removal){
          for (i in 1:max(P_both_a$id)){ # coloured for those included in sensitivity analyses
            a_pie3 <- rep(P_both$id[i], times = 7) # pie chart evenly split
            #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
            score_pie3 <- P_both_a$score[seq(i, length(P_both_a$id), max(P_both_a$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie3, P_both_a$fpr[i], P_both_a$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
          }
        }
      }
      # Add uncertainty for sens and spec at the study level 
      if ('1' %in% input$cicheck2 & (input$QAcheck2 == 1 | input$QAcheck2 == 9 | input$QAcheck2 == 10 | input$QAcheck2 == 11)){
        if('1' %in% input$mod_removal){
          arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
        }
        if ('2' %in% input$mod_removal){
          arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, length=0.05, angle=90, code=3)
        }
      }
      if ('2' %in% input$cicheck2 & (input$QAcheck2 == 1 | input$QAcheck2 == 9 | input$QAcheck2 == 10 | input$QAcheck2 == 11)){
        if('1' %in% input$mod_removal){
          arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
        }
        if ('2' %in% input$mod_removal){
          arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, length=0.05, angle=90, code=3)
        }
      }
      for (i in 2:8){
        if ('1' %in% input$cicheck2 & input$QAcheck2 == i){
          if('1' %in% input$mod_removal){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
          }
          if ('2' %in% input$mod_removal){
            for (j in 1:length(study_level_P$ID)){
              if (study_level_P[j,i+8] == 1){
                arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                       length=0.05, angle=90, code=3, col="green")
              }
              else if (study_level_P[j,i+8] == 2){
                arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                       length=0.05, angle=90, code=3, col="red")
              }
              else {
                arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                       length=0.05, angle=90, code=3, col="lavenderblush3")
              }
            }
          }
        }
        if ('2' %in% input$cicheck2 & input$QAcheck2 == i){
          if('1' %in% input$mod_removal){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
          }
          if ('2' %in% input$mod_removal){
            for (j in 1:length(study_level_P$ID)){
              if (study_level_P[j,i+8] == 1){
                arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                       length=0.05, angle=90, code=3, col = "green")
              }
              else if (study_level_P[j,i+8] == 2){
                arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                       length=0.05, angle=90, code=3, col = "red")
              }
              else {
                arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                       length=0.05, angle=90, code=3, col = "lavenderblush3")
              }
            }
          }
        }
      }
    }
    
    # Plots when covariate data is available but no quality assessment 
    if (C > 6 & Names[7] != "rob_PS"){
      for(i in 1:no_cov){
        if(input$covcheck2 == i+1){
          if('1' %in% input$mod_removal){
            if(input$weightcheck2 == FALSE){
              if(input$cov_toggle2 == 3){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2, col = "gray")
                points(study_level$FPR, study_level$Sensitivity, pch=1, col = "gray")
              }
              if(input$cov_toggle2 == 2){
                # plot covariates as coloured points
                points(study_level$FPR, study_level$Sensitivity, pch=1, col = "gray")
              }
              if(input$cov_toggle2 == 1){
                # plot covariates as text 
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2, col = "gray")
              }
            }
            if(input$weightcheck2 == TRUE){
              if(input$cov_toggle2 == 3){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = "gray")
              }
              if(input$cov_toggle2 == 2){
                # plot covariates as coloured points
                draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = "gray")
              }
              if(input$cov_toggle2 == 1){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
              }
            }
          }
          if('2' %in% input$mod_removal){
            if(input$weightcheck2 == FALSE){
              if(input$cov_toggle2 == 3){
                # plot covariates as text and coloured points
                text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                points(study_level_P$FPR, study_level_P$Sensitivity, pch=19, col=as.factor(covariates_a[,i]))
              }
              if(input$cov_toggle2 == 2){
                # plot covariates as coloured points
                points(study_level_P$FPR, study_level_P$Sensitivity, pch=19, col=as.factor(covariates_a[,i]))
              }
              if(input$cov_toggle2 == 1){
                # plot covariates as text and coloured points
                text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
              }
            }
            if(input$weightcheck2 == TRUE){
              if(input$cov_toggle2 == 3){
                # plot covariates as text and coloured points
                text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a=pctsp2/1000, b=pctse2/1000, col = as.factor(covariates_a[,i]))
              }
              if(input$cov_toggle2 == 2){
                # plot covariates as coloured points
                draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a=pctsp2/1000, b=pctse2/1000, col = as.factor(covariates_a[,i]))
              }
              if(input$cov_toggle2 == 1){
                # plot covariates as text and coloured points
                text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
              }  
            }
          }
        }
      }
      
      # Plot sensitivity and specificity (for all studies)
      if ('1' %in% input$mod_removal){
        if('1' %in% input$cicheck2){
          arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                 length=0.05, angle=90, code=3, col = "gray")
        }
        if ('2' %in% input$cicheck2){
          arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                 length=0.05, angle=90, code=3, col = "gray")
        }
      }

      # Plot sensitivity and specificitiy (Senstiviity Analysis studies only)
        if ('2' %in% input$mod_removal){
          if('1' %in% input$cicheck2){
            if('1' %in% input$covcheck2){
              # When no covariates are selected (sens)
              arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI,
                     length=0.05, angle=90, code=3)
            }
            if(!('1' %in% input$covcheck2)){
              if('1' %in% input$cov_toggle2){
                # When covariates are selected as text (sens)
                arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI,
                       length=0.05, angle=90, code=3)
              }
              if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
                for(i in 1:no_cov){
                  if(input$covcheck2 == i+1){
                    # When covariates are selected as coloured points/both (sens)
                    arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI,
                           length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                  }
                }
              }
            }
          }
          if('2' %in% input$cicheck2){
            if('1' %in% input$covcheck2){
              # When no covariates are selected (spec)
              arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity,
                     length=0.05, angle=90, code=3)
            }
            if(!('1' %in% input$covcheck2)){
              if('1' %in% input$cov_toggle2){
                # When covariates are selected as text (spec)
                arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity,
                       length=0.05, angle=90, code=3)
              }
              if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
                for(i in 1:no_cov){
                  if(input$covcheck2 == i+1){
                    # When covariates are seleceted as coloured points/both (spec)
                    arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity,
                           length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                  }
                }
              }
            }
          }
        }
    } # When plots for covariate data end 
    
    # Plot when quality assessment data and covariate data is available
    
     if(C > 13 & Names[13] == "ac_RS"){
    
    if('1' %in% input$mod_removal){
      for (i in 2:8){
        if (input$QAcheck2 == i){
          if(input$weightcheck2 == FALSE){
            points(study_level$FPR, study_level$Sensitivity, pch = 1, col = "gray")
          }
          if(input$weightcheck2 == TRUE){
            draw.ellipse(study_level$FPR, study_level$Sensitivity, a = pctsp/1000, b = pctse/1000, border = "gray")
          }
        }
      }
      for (i in 1:no_cov){
        if(input$covcheck2 == i+1){
          if(input$weightcheck2 == FALSE){
            if('3' %in% input$cov_toggle2){
              text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex = 0.7, pos = 2, col = "gray")
              if('1' %in% input$QAcheck2){
                points(study_level$FPR, study_level$Sensitivity, pch = 1, col = "gray")
              }
              else{
                points(study_level$FPR, study_level$Sensitivity, pch = 0, col = "gray")
              }
            }
            if('2' %in% input$cov_toggle2){
              if('1' %in% input$QAcheck2){
                points(study_level$FPR, study_level$Sensitivity, pch = 1, col = "gray")
              }
              else {
                points(study_level$FPR, study_level$Sensitivity, pch = 0, col = "gray")
              }
            }
            if('1' %in% input$cov_toggle2){
              text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex = 0.7, pos = 2, col = "gray")
            }
          }
          if(input$weightcheck2 == TRUE){
            if('3' %in% input$cov_toggle2){
              text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2, col = "gray")
              draw.ellipse(study_level$FPR, study_level$Sensitivity, a = pctsp/1000, b = pctse/1000, border = "gray")
            }
            if('2' %in% input$cov_toggle2){
              draw.ellipse(study_level$FPR, study_level$Sensitivity, a = pctsp/1000, b = pctse/1000, border = "gray")
            }
            if('1' %in% input$cov_toggle2){
              text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2, col = "gray")
            }
          }
        }
      }
    }
    if('2' %in% input$mod_removal){
      for(i in 2:8){
        if(input$QAcheck2 == i){
          for(m in 1:length(study_level_P$ID)){
            if(input$weightcheck2 == FALSE){
              if (study_level_P[m,i+8] == 1){
                points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="green")
              }
              else if (study_level_P[m,i+8] == 2){
                points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="red")
              }
              else {
                points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="lavenderblush3")
              }
            }
            if (input$weightcheck2 == TRUE){
              if(study_level_P[m,i+8] == 1){
                draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "green")
              }
              if(study_level_P[m,i+8] == 2){
                draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "red")
              }
              if(study_level_P[m,i+8] == 3){
                draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "lavenderblush3")
              }
            }
          }
        }
      }
      for(i in 1:no_cov){
        if(input$covcheck2 == i+1){
          if(input$weightcheck2 == FALSE){
            if('3' %in% input$cov_toggle2){
              text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
              if('1' %in% input$QAcheck2){
                points(study_level_P$FPR, study_level_P$Sensitivity, pch=19, col=as.factor(covariates_a[,i]))
              }
              else{
                points(study_level_P$FPR, study_level_P$Sensitivity, pch=0, col=as.factor(covariates_a[,i]), cex = 1.2)
              }
            }
            if('2' %in% input$cov_toggle2){
              if('1' %in% input$QAcheck2){
                points(study_level_P$FPR, study_level_P$Sensitivity, pch=19, col=as.factor(covariates_a[,i]))
              }
              else{
                points(study_level_P$FPR, study_level_P$Sensitivity, pch=0, col=as.factor(covariates_a[,i]), cex =1.2)
              }
            }
            if('1' %in% input$cov_toggle2){
              text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
            }
          }
          if(input$weightcheck2 == TRUE){
            if('3' %in% input$cov_toggle2){
              text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
              if('1' %in% input$QAcheck2){
                draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a = pctsp2/1000, b = pctse2/1000, col = as.factor(covariates_a[,i]))
              }
              else{
                draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a = pctsp2/1000, b = pctse2/1000, border = as.factor(covariates_a[,i]))
              }
            }
            if('2' %in% input$cov_toggle2){
              if('1' %in% input$QAcheck2){
                draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a = pctsp2/1000, b = pctse2/1000, col = as.factor(covariates_a[,i]))
              }
              else{
                draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a = pctsp2/1000, b = pctse2/1000, border = as.factor(covariates_a[,i]))
              }
            }
            if('1' %in% input$cov_toggle2){
              text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
            }
          }
        }
      }
    }
    
    # Pie charts to represent study level estimates split into risk of bias and applicability concerns 
    # Pie charts for risk of bias 
    if ('9' %in% input$QAcheck2){
      if ('1' %in% input$mod_removal){
        for (i in 1:max(P_rob$id)){ # uncoloured plotws for the whole dataset
          a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
          score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
          pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015,
                   col = ifelse(score_pie1 == 1, "gray70", ifelse(score_pie1 == 2, "gray20", "gray45"))) # plots pie charts
        }
      }
      if ('2' %in% input$mod_removal){
        for (i in 1:max(P_rob_a$id)){ # coloured for those included in sensitivity analyses
          a_pie1 <- rep(P_rob_a$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
          score_pie1 <- P_rob_a$score[seq(i, length(P_rob_a$id), max(P_rob_a$id))] # extracts the results from quality assessment to determine colour
          pieGlyph(a_pie1, P_rob_a$fpr[i], P_rob_a$sens[i], labels = NA, radius =0.015,
                   col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
        }
      }
    }
    # Pie charts for applicability concerns 
    if ('10' %in% input$QAcheck2){
      if ('1' %in% input$mod_removal){
        for (i in 1:max(P_ac$id)){
          a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
          score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
          pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                   col = ifelse(score_pie2 == 1, "gray70", ifelse(score_pie2 == 2, "gray20", "gray45"))) # plots pie charts
        }
      }
      if ('2' %in% input$mod_removal){
        for (i in 1:max(P_ac_a$id)){
          a_pie2 <- rep(P_ac_a$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
          score_pie2 <- P_ac_a$score[seq(i, length(P_ac_a$id), max(P_ac_a$id))] # extracts the results from quality assessment to determine colour
          pieGlyph(a_pie2, P_ac_a$fpr[i], P_ac_a$sens[i], labels = NA, radius =0.015,
                   col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
        }
      }
    }
    # Pie chart that shows both applicability concerns and risk of bias 
    if ('11' %in% input$QAcheck2){
      if ('1' %in% input$mod_removal){
        for (i in 1:max(P_both$id)){ # uncoloured plots for the whole dataset
          a_pie3 <- rep(P_both$id[i], times = 7) # pie chart evenly split
          #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
          score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
          pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                   col = ifelse(score_pie3 == 1, "gray70", ifelse(score_pie3 == 2, "gray20", "gray45"))) # plots pie charts
        }
      }
      if ('2' %in% input$mod_removal){
        for (i in 1:max(P_both_a$id)){ # coloured for those included in sensitivity analyses
          a_pie3 <- rep(P_both$id[i], times = 7) # pie chart evenly split
          #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
          score_pie3 <- P_both_a$score[seq(i, length(P_both_a$id), max(P_both_a$id))] # extracts the results from quality assessment to determine colour
          pieGlyph(a_pie3, P_both_a$fpr[i], P_both_a$sens[i], labels = NA, radius =0.015,
                   col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
        }
      }
    }
    
    
      
      if('2' %in% input$cicheck2){
        # plot sensitvity when no covariates selected no quality assesment (except piercharts)
        if('1' %in% input$covcheck2 & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
          if('1' %in% input$mod_removal){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
          }
          if('2' %in% input$mod_removal){
            arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, length=0.05, angle=90, code=3)
          }
        }
        # plot sensitivity when covariates only dispalyed as text no quality assesment (except piercharts)
        if(!('1' %in% input$covcheck2) & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
          if('1' %in% input$cov_toggle2){
            if('1' %in% input$mod_removal){
              arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
            }
            if('2' %in% input$mod_removal){
              arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, length=0.05, angle=90, code=3)
            }
          }
        }
        # Plot sensitvity when quality assessment only and no covariates
        for (i in 2:8){
          if('1' %in% input$covcheck2 & input$QAcheck2 == i){
            if('1' %in% input$mod_removal){
              arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
            }
            if ('2' %in% input$mod_removal){
              for (j in 1:length(study_level_P$ID)){
                if (study_level_P[j,i+8] == 1){
                  arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                         length=0.05, angle=90, code=3, col="green")
                }
                else if (study_level_P[j,i+8] == 2){
                  arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                         length=0.05, angle=90, code=3, col="red")
                }
                else {
                  arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                         length=0.05, angle=90, code=3, col="lavenderblush3")
                }
              }
            }
          }
        }
        # Plot sensitivity when quality assessment selected and covariates displayed as only text
        for (i in 2:8){
          if(!('1' %in% input$covcheck2) & input$QAcheck2 == i){
            if('1' %in% input$cov_toggle2){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
              }
              if ('2' %in% input$mod_removal){
                for (j in 1:length(study_level_P$ID)){
                  if (study_level_P[j,i+8] == 1){
                    arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                           length=0.05, angle=90, code=3, col="green")
                  }
                  else if (study_level_P[j,i+8] == 2){
                    arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                           length=0.05, angle=90, code=3, col="red")
                  }
                  else {
                    arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                           length=0.05, angle=90, code=3, col="lavenderblush3")
                  }
                }
              }
            }
          }
        }
        # Plot sensitivity when covariate selected as colour and no quality assessment (except pie charts)
        for(i in 1:no_cov){
          if(input$covcheck2 == i+1 & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
            if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray")
              }
              if('2' %in% input$mod_removal){
                arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, 
                       length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
              }
            }
          }
        }
        # Plot sensitivity when covariates and quality assessment both selected as colours
        if(!('1' %in% input$covcheck2) & (input$QAcheck2 == 2 |input$QAcheck2 == 3 | input$QAcheck2 == 4 |input$QAcheck2 == 5 | input$QAcheck2 == 6 |input$QAcheck2 == 7 | input$QAcheck2 == 8)){
          if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
            # Plot default black 
            if('1' %in% input$ci_col2){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col ="gray")
              }
              if('2' %in% input$mod_removal){
                arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, 
                       length=0.05, angle=90, code=3)
              }
            }
            # Plot colour as quality assessment
            if('2' %in% input$ci_col2){
              for(i in 2:8){
                if(input$QAcheck2 == i){
                  if('1' %in% input$mod_removal){
                    arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
                  }
                  if ('2' %in% input$mod_removal){
                    for (j in 1:length(study_level_P$ID)){
                      if (study_level_P[j,i+8] == 1){
                        arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                               length=0.05, angle=90, code=3, col="green")
                      }
                      else if (study_level_P[j,i+8] == 2){
                        arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                               length=0.05, angle=90, code=3, col="red")
                      }
                      else {
                        arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                               length=0.05, angle=90, code=3, col="lavenderblush3")
                      }
                    }
                  }
                }
              }
            }
            # Plot colour as covariates 
            if('3' %in% input$ci_col2){
              for(i in 1:no_cov){
                if(input$covcheck2 == i+1){
                  if('1' %in% input$mod_removal){
                    arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray")
                  }
                  if('2' %in% input$mod_removal){
                    arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, 
                           length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                  }
                }
              }
            }
          }
        }
      }
      
      if('1' %in% input$cicheck2){
        # plot sensitvity when no covariates selected no quality assesment (except piercharts)
        if('1' %in% input$covcheck2 & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
          if('1' %in% input$mod_removal){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
          }
          if('2' %in% input$mod_removal){
            arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, length=0.05, angle=90, code=3)
          }
        }
        # plot sensitivity when covariates only dispalyed as text no quality assesment (except piercharts)
        if(!('1' %in% input$covcheck2) & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
          if('1' %in% input$cov_toggle2){
            if('1' %in% input$mod_removal){
              arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
            }
            if('2' %in% input$mod_removal){
              arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, length=0.05, angle=90, code=3)
            }
          }
        }
        # Plot sensitvity when quality assessment only and no covariates
        for (i in 2:8){
          if('1' %in% input$covcheck2 & input$QAcheck2 == i){
            if('1' %in% input$mod_removal){
              arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
            }
            if ('2' %in% input$mod_removal){
              for (j in 1:length(study_level_P$ID)){
                if (study_level_P[j,i+8] == 1){
                  arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                         length=0.05, angle=90, code=3, col="green")
                }
                else if (study_level_P[j,i+8] == 2){
                  arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                         length=0.05, angle=90, code=3, col="red")
                }
                else {
                  arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                         length=0.05, angle=90, code=3, col="lavenderblush3")
                }
              }
            }
          }
        }
        # Plot sensitivity when quality assessment selected and covariates displayed as only text
        for (i in 2:8){
          if(!('1' %in% input$covcheck2) & input$QAcheck2 == i){
            if('1' %in% input$cov_toggle2){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
              }
              if ('2' %in% input$mod_removal){
                for (j in 1:length(study_level_P$ID)){
                  if (study_level_P[j,i+8] == 1){
                    arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                           length=0.05, angle=90, code=3, col="green")
                  }
                  else if (study_level_P[j,i+8] == 2){
                    arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                           length=0.05, angle=90, code=3, col="red")
                  }
                  else {
                    arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                           length=0.05, angle=90, code=3, col="lavenderblush3")
                  }
                }
              }
            }
          }
        }
        # Plot sensitivity when covariate selected as colour and no quality assessment (except pie charts)
        for(i in 1:no_cov){
          if(input$covcheck2 == i+1 & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
            if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray")
              }
              if('2' %in% input$mod_removal){
                arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, 
                       length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
              }
            }
          }
        }
        # Plot sensitivity when covariates and quality assessment both selected as colours
        if(!('1' %in% input$covcheck2) & (input$QAcheck2 == 2 |input$QAcheck2 == 3 | input$QAcheck2 == 4 |input$QAcheck2 == 5 | input$QAcheck2 == 6 |input$QAcheck2 == 7 | input$QAcheck2 == 8)){
          if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
            # Plot default black 
            if('1' %in% input$ci_col2){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col ="gray")
              }
              if('2' %in% input$mod_removal){
                arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, 
                       length=0.05, angle=90, code=3)
              }
            }
            # Plot colour as quality assessment
            if('2' %in% input$ci_col2){
              for(i in 2:8){
                if(input$QAcheck2 == i){
                  if('1' %in% input$mod_removal){
                    arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
                  }
                  if ('2' %in% input$mod_removal){
                    for (j in 1:length(study_level_P$ID)){
                      if (study_level_P[j,i+8] == 1){
                        arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                               length=0.05, angle=90, code=3, col="green")
                      }
                      else if (study_level_P[j,i+8] == 2){
                        arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                               length=0.05, angle=90, code=3, col="red")
                      }
                      else {
                        arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                               length=0.05, angle=90, code=3, col="lavenderblush3")
                      }
                    }
                  }
                }
              }
            }
            # Plot colour as covariates 
            if('3' %in% input$ci_col2){
              for(i in 1:no_cov){
                if(input$covcheck2 == i+1){
                  if('1' %in% input$mod_removal){
                    arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray")
                  }
                  if('2' %in% input$mod_removal){
                    arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, 
                           length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                  }
                }
              }
            }
          }
        }
      }
    }# End of plots for quality assessment and covariate data
    
    # Plot the original model
    if('1' %in% input$mod_removal){
      # Add the ROC curve 
      if('2' %in% input$HSROCcheck2){
        if(input$extrapp2 == T){
          points(roc_points, type = "l", col = "gray")
        }
        else{
          points(roc_points2, type = "l", col = "gray")
        }
      }
      # Add the summary point 
      if('1' %in% input$bivcheck2){
        points(mean_point, col = "gray", pch = 15)
      }
      # Add the confidence region 
      if('2' %in% input$bivcheck2){
        lines(conf_region, lty = 2, col = "gray")
      }
      if('3' %in% input$bivcheck2){
        lines(pred_region, lty = 3, col = "gray")
      }
    }
    
    # Plot the sensitivity analysis model
    if('2' %in% input$mod_removal){
      # Add the ROC curve 
      if('2' %in% input$HSROCcheck2){
        if(input$extrapp2 == T){
          points(roc_points_a, type = "l")
        }
        else{
          points(roc_points2_a, type = "l")
        }
      }
      # Add the summary point 
      if('1' %in% input$bivcheck2){
        points(mean_point_a, col = "blue", pch = 15)
      }
      # Add the confidence region 
      if('2' %in% input$bivcheck2){
        lines(conf_region_a, lty = 2, col = "blue")
      }
      if('3' %in% input$bivcheck2){
        lines(pred_region_a, lty = 3, col = "blue")
      }
    }
    
    legend("bottomright", bty = "n", leglabticks2[1:LL2,1], pch = legendticks2[1:LL2,1], lty = legendticks2[1:LL2,2],
           lwd = leg_lwd_SA, col = leg_col_SA)

    legend("bottom", bty = "n", leglabticks[1:LL,1], pch = legendticks[1:LL,1], lty = legendticks[1:LL,2],
           lwd = leg_lwd_O, col = leg_col_O)
    
  })


  #render the UI before rendering the plot
  output$sensplot <- renderUI ({
    #plotOutput("sroc_sa", height="500px", width="500px")
    plotOutput("sroc_sa", height="750px", width="750px",click=clickOpts("plot_click_ma2"))#, hover=hoverOpts("plot_hover"))
  })

  # Add hover information to the plot - when users move their mouse over the plot a crosshair icon appears
  # and at the bottom of the plot the sens and FPR for the trial they hover over is displayed
  output$clickinfo_ma2 <- renderText({
    if(is.null(data())){return()}
      else
 
    cData <- data()
      C <- length(cData[1,])
      N <- length(cData$TP)
      Names <- colnames(cData)
      b <- study_level_outcomes(cData) # get sens, spec for each trial
      bb <- data.frame(Author=cData$author, Year=cData$year, TP=cData$TP, FN=cData$FN, FP=cData$FP, 
                       TN=cData$TN, Sensitivity=b$Sensitivity, Specificity=b$Specificity, 
                       FPR=b$FPR)
      bb$Prev <- ((bb$TP+bb$FN) / (bb$TP+bb$FN+bb$TN+bb$FP))
      # extract covariates and attach to datasets
      if(C > 6 & Names[7] != "rob_PS"){
        if (C == 7){
          covariates <- cData[7]
          no_cov <- 1
        }
        else{
          covariates <-cData[,7:C]
          no_cov <- length(colnames(covariates))
        }
        for (i in 1:no_cov){
          if(input$covcheck2 == i+1){
            bb <- cbind(bb, covariates[,i]) # adds covariate to dataset
            names(bb)[length(names(bb))]<-"Cov" # renames covariate column as its last in dataset
          }
        }
      }
      if(C> 13 & Names[13] == "ac_RS"){
        if (C == 14){
          covariates <- cData[14]
          no_cov <- 1
        }
        else{
          covariates <-cData[,14:C]
          no_cov <- length(colnames(covariates))
        }
        for (i in 1:no_cov){
          if(input$covcheck2 == i+1){
            bb <- cbind(bb, covariates[,i]) # adds covariate to dataset
            names(bb)[length(names(bb))]<-"Cov" # renames covariate column as its last in dataset
          }
        }
      }
      # add information about percentage weights 
      cData$n1 <- cData$TP+cData$FN
      cData$n0 <- cData$FP+cData$TN
      cData$true1 <- cData$TP
      cData$true0 <- cData$TN 
      cData$study <- 1:N
      Y = reshape(cData, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                  timevar = "sens" , times = c(1,0) , v.names = c("n","true") ) 
      Y = Y[order(Y$id),]  
      Y$spec<- 1-Y$sens
      MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                    data = Y , family = binomial , nAGQ=1 , verbose=0 ) 
      ma_Y = summary(MA_Y)
      labels(ma_Y) 
      ma_Y$coeff 
      varA    <- ma_Y$varcor$study[1,1]
      varB    <- ma_Y$varcor$study[2,2]
      covAB   <- ma_Y$vcov@x[2]
      Y_pw = reshape(cData, direction = "long", varying = list( c("n0" , "n1") , c( "true0","true1" ) ) ,
                     timevar = "spec" , times = c(1,0) , v.names = c("n","true") )
      Y_pw = Y_pw[order(Y_pw$id),]
      Y_pw$sens<- 1-Y_pw$spec
      X_pw <- cbind(Y_pw$sens, Y_pw$spec)
      XT_pw <- t(X_pw)
      Z <- diag(2*N)
      invn <- 1/Y_pw$n
      A <- diag(invn)
      p_pw <- predict(MA_Y, type="response")
      var_pw <- p_pw*(1-p_pw)
      B <- diag(var_pw)
      G_one <- matrix(c(varA,covAB,covAB,varB),2,2)
      G <- do.call(adiag, replicate(N, G_one, simplify = FALSE))
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
      pctse <- vector(mode="numeric", length =N)
      pctsp <- vector(mode="numeric", length =N)
      for (i in 1:N){
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
      bb$pct_spec <- pctsp
      bb$pct_sens <- pctse
      
      
      # Information if no covariates
      hoverselect <- nearPoints(bb, input$plot_click_ma2, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1) #takes the row from the dataframe that matches the hover point (only allowed one)
      if(C == 6 | (C == 13 & Names[7] == "rob_PS")){  
        if(input$prevcheck2 == TRUE & input$weightcheck2 == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev, hoverselect$pct_sens, hoverselect$pct_spec))
        }
        else if (input$prevcheck2 == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev))
        }
        else if (input$weightcheck2 == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$pct_sens, hoverselect$pct_spec))
        }
        else{
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity))
        }
      }
      # Information if covariates
      else if((C > 6 & Names[7] != "rob_PS") |(C> 13 & Names[13] == "ac_RS")){
        if(input$prevcheck2 == TRUE & input$weightcheck2 == TRUE & input$covcheck2 != 1){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f, Covariate value = %10s ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev, hoverselect$pct_sens, hoverselect$pct_spec, hoverselect$Cov))
        }
        else if (input$prevcheck2 == TRUE & input$covcheck2 != 1){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f, Covariate value = %10s", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev, hoverselect$Cov))
        }
        else if (input$prevcheck2 == TRUE & input$weightcheck2 == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev, hoverselect$pct_sens, hoverselect$pct_spec))
        }
        else if (input$weightcheck2 == TRUE & input$covcheck2 != 1){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f, Covariate value = %10s ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$pct_sens, hoverselect$pct_spec, hoverselect$Cov))
        }
        else if (input$prevcheck2 == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev))
        }
        else if (input$weightcheck2 == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$pct_sens, hoverselect$pct_spec))
        }
        else if (input$covcheck2 != 1){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Covariate value = %10s", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Cov)) 
        }
        else{
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity))
        }
      }
  })

  # Produce larger pie chart if clicked on
  output$piechart2 <- renderPlot({
    if(is.null(data())){return()}
    else
      PData <- data()
    e <- study_level_outcomes(PData)
    if ('9' %in% input$QAcheck2){
      ee <- data.frame(Author=PData$author,Year=PData$year, TP=PData$TP, FN=PData$FN, FP=PData$FP,
                       TN=PData$TN, Sensitivity=e$Sensitivity, Specificity=e$Specificity,
                       FPR=e$FPR, rob_PS=PData$rob_PS, rob_IT=PData$rob_IT, rob_RS=PData$rob_RS, rob_FT=PData$rob_FT )
      clickselect_pc <- nearPoints(ee, input$plot_click_ma2, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1)
      clickselect_pc <- reshape(clickselect_pc, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"),
                                v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
      labels_rob <- c("Patient selection", "Index test", "Reference standard", "Flow & timing")
      weight <-c(1,1,1,1)
      pie(weight, labels_rob, main = "Risk of bias for each domain from the selected study",
          col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "lavenderblush3")))
      legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "lavenderblush3"))
    }
   if ('10' %in% input$QAcheck2){
      ee <- data.frame(Author=PData$author,Year=PData$year, TP=PData$TP, FN=PData$FN, FP=PData$FP,
                       TN=PData$TN, Sensitivity=e$Sensitivity, Specificity=e$Specificity,
                       FPR=e$FPR, ac_PS=PData$ac_PS, ac_IT=PData$ac_IT, ac_RS=PData$ac_RS)
      clickselect_pc <- nearPoints(ee, input$plot_click_ma2, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1)
      clickselect_pc <- reshape(clickselect_pc, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"),
                                v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
      labels_ac <- c("Patient selection", "Index test", "Reference standard")
      weight <- c(1,1,1)
      pie(weight, labels_ac, main = "Apllicability concern for each domain from the selected study",
          col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "lavenderblush3")))
      legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "lavenderblush3"))
    }
    if ('11' %in% input$QAcheck2){
      ee <- data.frame(Author=PData$author,Year=PData$year, TP=PData$TP, FN=PData$FN, FP=PData$FP,
                       TN=PData$TN, Sensitivity=e$Sensitivity, Specificity=e$Specificity,
                       FPR=e$FPR, rob_PS=PData$rob_PS, rob_IT=PData$rob_IT, rob_RS=PData$rob_RS, rob_FT=PData$rob_FT,
                       ac_PS=PData$ac_PS, ac_IT=PData$ac_IT, ac_RS=PData$ac_RS)
      clickselect_pc <- nearPoints(ee, input$plot_click_ma2, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1)
      clickselect_pc <-reshape(clickselect_pc, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                               v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
      labels_both <- c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS")
      #weight <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # weight for pie chart split in half
      weight <- c(1,1,1,1,1,1,1) # weight foe equal size
      pie(weight, labels_both, main = "Scores from each element of the QUADAS tool",
          col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "lavenderblush3")))
      legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "lavenderblush3"))
    }
  })

  # Allow users to download the sensitivity analysis SROC curve
  output$downloadROC_sa <- downloadHandler(
    # Specify the file name (either roc.png or roc.pdf)
    filename = function(){
      paste("roc_sa", input$filetype2, sep=".")
    },
    content = function(file){
      # open the device
      # create the plot
      # close the device
      if(input$filetype2 == "png")
        png(file, width = 750, height = 750)
      else
        pdf(file, width = 8.5, height = 8.5)
      X <- data()
      # Count the number of studies
      N <- length(X$TP)
      # Count the length of the data
      C <- length(X[1,])
      # Store the names of the columns
      # Used to determine if covariates are present 
      Names <- colnames(X)
      ## Set up the data
      ## Generate 5 new variables of type long. We need these before we can reshape the data.
      # n1 is number diseased
      # n0 is number without disease
      # true1 is number of true positives
      # true0 is the number of true negatives
      #	study is the unique identifier for each study. _n will generate a sequence of numbers.
      
      X$n1 <- X$TP+X$FN
      X$n0 <- X$FP+X$TN
      X$true1 <- X$TP
      X$true0 <- X$TN
      X$study <- 1:N
      
      ## Reshape the data from wide to long format ###
      
      Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c("true1","true0")) ,
                  timevar = "sens" , times = c(1,0) , v.names = c("n","true"))
      
      ## Sort data by study to cluster the 2 records per study together ###
      Y = Y[order(Y$id),]
      Y$spec<- 1-Y$sens
      
      ## Perform meta-analysis ##
      MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                    data = Y , family = binomial , nAGQ=1 , verbose=0 )
      
      # More detail can be obtained by using the summary command
      ma_Y = summary(MA_Y)
      
      ## For the full list of outputs
      labels( ma_Y )
      
      ## Therefore, to extract the coefficients
      ma_Y$coeff
      
      # Logit sensitivity and specificity
      lsens = ma_Y$coeff[1,1]
      lspec = ma_Y$coeff[2,1]
      
      #HSROC parameters
      # Use estimates of bivariate model parameters to calculate HSROC parameters
      sigma2_a <- ma_Y$varcor$study[1]
      sigma2_b <- ma_Y$varcor$study[4]
      sigma_ab <- ma_Y$varcor$study[2]
      sigma_a <- sqrt(sigma2_a)
      sigma_b <- sqrt(sigma2_b)
      
      beta <- log(sigma_b/sigma_a)
      Theta <- 0.5*( (((sigma_b/sigma_a)^0.5 )*lsens) - ( ( (sigma_a/sigma_b)^0.5) *lspec) )
      Lambda <- ( ( (sigma_b/sigma_a)^0.5) * lsens) + ( (sigma_a/sigma_b)^0.5 *lspec)
      sigma2_theta <- 0.5*( (sigma_a*sigma_b) - sigma_ab )
      sigma2_alpha <- 2*( (sigma_a*sigma_b) + sigma_ab )
      
      # Calcuate points for ROC curve
      # Want to plot for Specifity ranging between 0.5 and 1 (then FPR ranges between 0.5 and 0)
      Sp <- 0.5
      Fpr <- 1-Sp
      LSp <- qlogis(Sp)
      LSe <- Lambda*exp(-beta/2) - exp(-beta)*LSp
      Se <- plogis(LSe)
      
      # Empty data frame
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
      
      # Confidence region based on Stata metandi commands
      seB <- ma_Y$coefficients[2,2]
      seA <- ma_Y$coefficients[1,2]
      r <- ma_Y$vcov@x[2] / (seA*seB)
      level <- 95
      f <- qf(0.95, df1=2, df2=N-2)
      croot <- sqrt(2*f)
      
      # Empty data frame
      conf_region <- (rep(NA, 361))
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
      
      # Prediction region based on Stata metandi commands
      varA <- ma_Y$varcor$study[1,1]
      varB <- ma_Y$varcor$study[2,2]
      seB <- ma_Y$coefficients[2,2]
      seA <- ma_Y$coefficients[1,2]
      sAB <- ma_Y$varcor$study[1,2]
      covAB <- ma_Y$vcov@x[2]
      sepredA <- sqrt( varA + seA^2 )
      sepredB <- sqrt( varB + seB^2 )
      rpredAB <- (sAB + covAB) / (sepredA*sepredB)
      level <- 95
      f <- qf(0.95, df1=2, df2=N-2)
      croot <- sqrt(2*f)
      
      # Empty data frame
      pred_region <- (rep(NA, 361))
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
        covariates <- X[,7:C] # extract all covariates 
        no_cov <- length(colnames(covariates)) # number of covariates
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
        
        covariates <- X[,14:C] # extract all covariates 
        no_cov <- length(colnames(covariates)) # number of covariates
        study_level <- cbind(study_level, covariates)# combine with study_level data frame
        
      }
      
      # Round the prevalence to 2 decimal places 
      study_level[,'Prev'] <- round(study_level[,'Prev'] , 2)
      
      # Calculate sens and spec confidence intervals at the study level
      # Add the confidence intervals to the dataset
      foreach (i=1:N) %do% {
        study_level$Sens_LCI[i] <- binconf(study_level$TP[i], study_level$TP[i]+study_level$FN[i], method="exact")[2]
        study_level$Sens_UCI[i] <- binconf(study_level$TP[i], study_level$TP[i]+study_level$FN[i], method="exact")[3]
        study_level$FPR_LCI[i]  <- 1 - binconf(study_level$TN[i], study_level$FP[i]+study_level$TN[i], method="exact")[3]
        study_level$FPR_UCI[i]  <- 1 - binconf(study_level$TN[i], study_level$FP[i]+study_level$TN[i], method="exact")[2]
      }
      
      # Mean point
      Sens = plogis(lsens)
      Spec = plogis(lspec)
      fpr <- 1-Spec
      mean_point=data.frame(x=fpr, y=Sens)
      leglabticks <- matrix(nrow=10, ncol=1) #will put legend labels in this vector
      legendticks <- matrix(nrow=10, ncol=2) #symbols column 1 for pch column 2 for lty
      leglabticks[1] <- "Original model"
      legendticks[1,1]<-3
      
      # Calculate min and max values of sens and spec to avoid extrapolation of SROC curve
      minSens <- min(X$sens)
      maxSens <- max(X$sens)
      minFPR <- min(X$fpr)
      maxFPR <- max(X$fpr)
      
      # Create new data frame which restricts roc_points to being between min and max values
      roc_points2 <- subset(roc_points, FPR<maxFPR & FPR>minFPR)
      
      ### Fit model for sensitivity analysis
      P <- X[input$triallist, ] #data set of studies in the analysis
      # Count the number of studies
      N1 <- length(P$TP)
      ## Set up the data
      ## Generate 5 new variables of type long. We need these before we can reshape the data.
      # n1 is number diseased
      # n0 is number without disease
      # true1 is number of true positives
      # true0 is the number of true negatives
      # study is the unique identifier for each study. _n will generate a sequence of numbers.
      
      P$na1 <- P$TP+P$FN
      P$na0 <- P$FP+P$TN
      P$truea1 <- P$TP
      P$truea0 <- P$TN
      P$studya <- 1:N1
      
      ## Reshape the data from wide to long format ###
      
      Q = reshape(P, direction = "long", varying = list( c("na1" , "na0") , c( "truea1","truea0" ) ) ,
                  timevar = "sens" , times = c(1,0) , v.names = c("na","truea") )
      
      ## Sort data by study to cluster the 2 records per study together ###
      Q = Q[order(Q$id),]
      Q$spec<- 1-Q$sens
      
      ## Perform meta-analysis ##
      MA_Q = glmer(formula = cbind(truea, na - truea) ~ 0 + sens + spec + (0+sens + spec|studya),
                   data = Q , family = binomial , nAGQ=1 , verbose=0)
      
      # More detail can be obtained by using the summary command
      ma_Q = summary(MA_Q)
      
      ## For the full list of outputs
      labels(ma_Q)
      
      ## Therefore, to extract the coefficients
      ma_Q$coeff
      
      # Logit sensitivity and specificity
      lsens_a = ma_Q$coeff[1,1]
      lspec_a = ma_Q$coeff[2,1]
      
      #HSROC parameters
      # Use estimates of bivariate model parameters to calculate HSROC parameters
      sigma2_a_a <- ma_Q$varcor$study[1]
      sigma2_b_a <- ma_Q$varcor$study[4]
      sigma_ab_a <- ma_Q$varcor$study[2]
      sigma_a_a <- sqrt(sigma2_a_a)
      sigma_b_a <- sqrt(sigma2_b_a)
      
      beta_a <- log(sigma_b_a/sigma_a_a)
      Theta_a <- 0.5*( (((sigma_b_a/sigma_a_a)^0.5 )*lsens_a) - ( ( (sigma_a_a/sigma_b_a)^0.5) *lspec_a))
      Lambda_a <- ( ( (sigma_b_a/sigma_a_a)^0.5) * lsens_a) + ( (sigma_a_a/sigma_b_a)^0.5 *lspec_a)
      sigma2_theta_a <- 0.5*( (sigma_a_a*sigma_b_a) - sigma_ab_a )
      sigma2_alpha_a <- 2*( (sigma_a_a*sigma_b_a) + sigma_ab_a )
      
      # Calcuate points for ROC curve
      # Want to plot for Specifity ranging between 0.5 and 1 (then FPR ranges between 0.5 and 0)
      Sp_a <- 0.5
      Fpr_a <- 1-Sp_a
      LSp_a <- qlogis(Sp_a)
      LSe_a <- Lambda_a*exp(-beta_a/2) - exp(-beta_a)*LSp_a
      Se_a <- plogis(LSe_a)
      
      # Empty data frame
      roc_points_a <- rep(NA, 60)
      for (j in seq(from=0, to=1, by=0.01)){
        Sp_j <- j
        Fpr_j <- 1-Sp_j
        LSp_j <- qlogis(Sp_j)
        LSe_j <- Lambda_a*exp(-beta_a/2) - exp(-beta_a)*LSp_j
        Se_j <- plogis(LSe_j)
        roc_j <- data.frame(FPR=Fpr_j, Sen=Se_j)
        
        # Add results for most recent value to a data frame which contains the results of previous values
        roc_points_a <- rbind(roc_points_a,roc_j)
      }
      
      # Confidence region based on Stata metandi commands
      seB_a <- ma_Q$coefficients[2,2]
      seA_a <- ma_Q$coefficients[1,2]
      r_a <- ma_Q$vcov@x[2] / (seA_a*seB_a)
      level_a <- 95
      f_a <- qf(0.95, df1=2, df2=N1-2)
      croot_a <- sqrt(2*f_a)
      
      # Empty data frame
      conf_region_a <- (rep(NA, 361))
      for (j in seq(0, 2*pi, length.out=361)){
        confB_a <- lspec_a + (seB_a*croot_a*cos(j))
        confA_a <- lsens_a + (seA_a*croot_a*cos(j + acos(r_a)))
        confsens_a <- plogis(confA_a)
        confspec_a <- plogis(confB_a)
        conf_j <- data.frame(X=1-confspec_a, Y=confsens_a)
        
        # Add results for most recent value to a data frame which contains the results of previous values
        conf_region_a <- rbind(conf_region_a, conf_j)
      }
      conf_region_a <- conf_region_a[2:362,]
      
      # Prediction region based on Stata metandi commands
      varA_a <- ma_Q$varcor$study[1,1]
      varB_a <- ma_Q$varcor$study[2,2]
      seB_a <- ma_Q$coefficients[2,2]
      seA_a <- ma_Q$coefficients[1,2]
      sAB_a <- ma_Q$varcor$study[1,2]
      covAB_a <- ma_Q$vcov@x[2]
      sepredA_a <- sqrt( varA_a + seA_a^2 )
      sepredB_a <- sqrt( varB_a + seB_a^2 )
      rpredAB_a <- (sAB_a + covAB_a) / (sepredA_a*sepredB_a)
      level_a <- 95
      f_a <- qf(0.95, df1=2, df2=N1-2)
      croot_a <- sqrt(2*f_a)
      
      # Empty data frame
      pred_region_a <- (rep(NA, 361))
      for (j in seq(0, 2*pi, length.out=361)){
        predB_a <- lspec_a + (sepredB_a*croot_a*cos(j))
        predA_a <- lsens_a + (sepredA_a*croot_a*cos(j + acos(rpredAB_a)))
        predsens_a <- plogis(predA_a)
        predspec_a <- plogis(predB_a)
        pred_j <- data.frame(X=1-predspec_a, Y=predsens_a)
        # Add results for most recent value to a data frame which contains the results of previous values
        pred_region_a <- rbind(pred_region_a, pred_j)
      }
      pred_region_a <- pred_region_a[2:362,]
      
      # Sensitivity and specificity calculations for each study
      P$sens <- P$TP / (P$TP + P$FN)
      P$spec <- P$TN / (P$FP + P$TN)
      P$fpr <- 1- P$spec
      
      # Separate dataframes dependent on if quality assessment data is available
      # Need to identify if quality assessment data is present and if covariate data is present
      # If quality assessment data is avaialble need to create further dataframes to produce pie charts
      
      # No quality assessment, No covariates
      if (C == 6){
        study_level_P <- data.frame(ID=P$study, TP=P$TP, FN=P$FN, FP=P$FP, TN=P$TN, N=(P$TP+P$FN+P$FP+P$TN), 
                                    Sensitivity=P$sens, Specificity=P$spec, FPR=P$fpr, Prev=((P$TP+P$FN)/(P$TP+P$FN+P$FP+P$TN)))
      }
      
      # No quality assessment, Yes Covariates
      if (C > 6 & Names[7] != "rob_PS"){
        study_level_P <- data.frame(ID=P$study, TP=P$TP, FN=P$FN, FP=P$FP, TN=P$TN, N=(P$TP+P$FN+P$FP+P$TN), 
                                    Sensitivity=P$sens, Specificity=P$spec, FPR=P$fpr, Prev=((P$TP+P$FN)/(P$TP+P$FN+P$FP+P$TN)))
        covariates_a <- P[,7:C] # extract all covariates 
        no_cov <- length(colnames(covariates_a)) # number of covariates
        study_level_P <- cbind(study_level_P, covariates_a)# combine with study_level data frame
      }
      
      # Yes quality assessment, No covariates
      if (C == 13 & Names[7] == "rob_PS" ){
        study_level_P <- data.frame(ID=P$study, TP=P$TP, FN=P$FN, FP=P$FP, TN=P$TN, N=(P$TP+P$FN+P$FP+P$TN), 
                                    Sensitivity=P$sens, Specificity=P$spec, FPR=P$fpr,rob_PS = P$rob_PS,
                                    rob_IT = P$rob_IT, rob_RS = P$rob_RS,rob_FT = P$rob_FT,ac_PS = P$ac_PS,
                                    ac_IT = P$ac_IT, ac_RS = P$ac_RS,  Prev = ((P$TP+P$FN)/(P$TP+P$FN+P$FP+P$TN)))
        # Reshape the data to allow for pie charts to be plotted as summary points
        # For ROB outcomes
        P_rob_a <- P[,-which(names(P) == "ac_PS" | names(P) == "ac_RS"| names(P) == "ac_IT")] # Delete applicability concerns for ROB plot
        P_rob_a <- reshape(P_rob_a, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"),
                           v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
        # For AC outcomes
        P_ac_a <- P[,-which(names(P) == "rob_PS" | names(P) == "rob_RS"| names(P) == "rob_IT"|names(P) == "rob_FT")] # Delete applicability concerns for ROB plot
        P_ac_a <- reshape(P_ac_a, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"),
                          v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
        # For both outcomes together
        P_both_a <- reshape(P, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                            v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
        
      }
      
      # Yes quality assessment, yes covariates 
      if (C > 13 & Names[13] == "ac_RS"){
        study_level_P <- data.frame(ID=P$study, TP=P$TP, FN=P$FN, FP=P$FP, TN=P$TN, N=(P$TP+P$FN+P$FP+P$TN), 
                                    Sensitivity=P$sens, Specificity=P$spec, FPR=P$fpr,rob_PS = P$rob_PS,
                                    rob_IT = P$rob_IT, rob_RS = P$rob_RS,rob_FT = P$rob_FT,ac_PS = P$ac_PS,
                                    ac_IT = P$ac_IT, ac_RS = P$ac_RS,  Prev = ((P$TP+P$FN)/(P$TP+P$FN+P$FP+P$TN)))
        # Reshape the data to allow for pie charts to be plotted as summary points
        # For ROB outcomes
        P_rob_a <- P[,-which(names(P) == "ac_PS" | names(P) == "ac_RS"| names(P) == "ac_IT")] # Delete applicability concerns for ROB plot
        P_rob_a <- reshape(P_rob_a, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"),
                           v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
        # For AC outcomes
        P_ac_a <- P[,-which(names(P) == "rob_PS" | names(P) == "rob_RS"| names(P) == "rob_IT"|names(P) == "rob_FT")] # Delete applicability concerns for ROB plot
        P_ac_a <- reshape(P_ac_a, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"),
                          v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
        # For both outcomes together
        P_both_a <- reshape(P, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                            v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
        
        covariates_a<- P[,14:C] # extract all covariates 
        no_cov_a <- length(colnames(covariates_a)) # number of covariates
        study_level_P <- cbind(study_level_P, covariates_a)# combine with study_level data frame
        
      }
      
      # Round prevalence to 2 decimal places 
      study_level_P[,'Prev'] <- round(study_level_P[,'Prev'] , 2)
      
      # Calculate sens and spec confidence intervals at the study level
      # Add the confidence intervals to the dataset
      foreach (i=1:N1) %do% {
        study_level_P$Sens_LCI[i] <- binconf(study_level_P$TP[i], study_level_P$TP[i]+study_level_P$FN[i], method="exact")[2]
        study_level_P$Sens_UCI[i] <- binconf(study_level_P$TP[i], study_level_P$TP[i]+study_level_P$FN[i], method="exact")[3]
        study_level_P$FPR_LCI[i]  <- 1 - binconf(study_level_P$TN[i], study_level_P$FP[i]+study_level_P$TN[i], method="exact")[3]
        study_level_P$FPR_UCI[i]  <- 1 - binconf(study_level_P$TN[i], study_level_P$FP[i]+study_level_P$TN[i], method="exact")[2]
      }
      
      # Mean point
      Sens_a = plogis( lsens_a )
      Spec_a = plogis( lspec_a )
      fpr_a <- 1-Spec_a
      mean_point_a=data.frame(x=fpr_a, y=Sens_a)
      
      
      # Percentage weight calculations 
      # For model with all studies included 
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
      # For the sensitvity analysis model 
      Y_pw2 = reshape(P, direction = "long", varying = list( c("n0" , "n1") , c( "true0","true1" ) ) ,
                      timevar = "spec" , times = c(1,0) , v.names = c("n","true") )
      Y_pw2 = Y_pw2[order(Y_pw2$id),]
      Y_pw2$sens<- 1-Y_pw2$spec
      X_pw2 <- cbind(Y_pw2$sens, Y_pw2$spec)
      XT_pw2 <- t(X_pw2)
      Z2 <- diag(2*N1)
      invn2 <- 1/Y_pw2$n
      A2 <- diag(invn2)
      p_pw2 <- predict(MA_Q, type="response")
      var_pw2 <- p_pw2*(1-p_pw2)
      B2 <- diag(var_pw2)
      G_one2 <- matrix(c(varA_a,covAB_a,covAB_a,varB_a),2,2)
      G2 <- do.call(adiag, replicate(N1, G_one2, simplify = FALSE))
      #inverse of B (required later on)
      BI2 <- solve(B2)
      # Create varianbce matrix for observations
      V2 <- (Z2 %*% G2 %*% t(Z2)) + (A2 %*% BI2)
      # invert the variance matrix
      invV2 <- solve(V2)
      # derive the fishers information matrix
      fish2 <- XT_pw2 %*% invV2 %*% X_pw2
      # invert Fishers information to obtain Var Beta hat
      varb2 <- solve(fish2)
      pctse2 <- vector(mode="numeric", length =N1)
      pctsp2 <- vector(mode="numeric", length =N1)
      for (i in 1:N1){
        DM2 <- V2
        DM2[(i*2)-1, (i*2)-1] <- 1000000000
        DM2[(i*2)-1, (i*2)] <- 0
        DM2[(i*2), (i*2)-1] <- 0
        DM2[(i*2), (i*2)] <- 1000000000
        
        invDM2 <- solve(DM2)
        fishD2 <- XT_pw2 %*% invDM2 %*% X_pw2
        fishI2 <- fish2 - fishD2
        weight2 <- varb2 %*% fishI2 %*% varb2
        pctse2[i] <- 100*(weight2[1,1]/varb2[1,1])
        pctsp2[i] <- 100*(weight2[2,2]/varb2[2,2])
      }
      
      # Set up legend
      # Data no covariates no quality assessment
      if(C == 6){
        LL <- 7 # Specifies length of legend
        LL2 <- LL
        leg_col_SA <- c("white", "black", "black", "blue", "blue", "blue", "black")
        leg_col_O <- c("white", "gray", "gray", "gray", "gray", "gray", "gray")
        leg_col <- c(leg_col_O,leg_col_SA) # colours of the legend
        leg_lwd_SA <- c(NA, NA, 2, NA, 1, 1, NA)
        leg_lwd_O <- c(NA, NA, 2, NA, 1, 1, NA)
        leg_lwd <- c(leg_lwd_O, leg_lwd_SA) # line width
        
      }
      
      # Data with quality assessment only
      if( C == 13 & Names[7] == "rob_PS"){
        LL <- 10
        LL2 <- LL
        leg_col_O <- c("white","green", "red", "lavenderblush3", "gray", "gray", "gray", "gray", "gray", "gray")
        leg_col_SA <- c("white","green", "red", "lavenderblush3", "black", "black", "blue", "blue", "blue", "black")
        leg_lwd_O <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
        leg_lwd_SA <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
      }
      
      
      # Data with only covariates
      if (C > 6 & Names[7] != "rob_PS"){
        if(!('1' %in% input$covcheck2)){# generates legend to include covariates
          for(i in 1:no_cov){
            if(input$covcheck2 == i+1){
              #unique <- unique(covariates[,i]) # calculate the number of unique entrys for the selcted covariate
              unique <- unique(covariates_a[,i]) # Unique entrys for sens analysis
              leg_covnames <- sort(as.character(unique))
              #leg_covnames2 <- sort(as.character(unique2))
              LL <-  7
              LL2 <- length(unique) + 7
            }
          }
          leg_col <- rep(0, times = length(unique))
          for (i in 1:length(unique)){ # assigns colour to each level
            leg_col[i] <- palette(rainbow(length(unique)))[length(unique)+1-i]
          }
          
          leg_col_O <- c("white", "gray", "gray", "gray", "gray", "gray", "gray")
          leg_col_SA <-c("white", leg_col, "black", "black", "blue", "blue", "blue", "black")
          
          leg_lwd1 <- rep(NA, times = length(unique))
          leg_lwd_O <- c(NA, NA, 2, NA, 1, 1, NA)
          leg_lwd_SA <- c(NA, leg_lwd1, NA, 2, NA, 1, 1, NA)
        }
        else{
          LL <- 7
          LL2 <- LL
          leg_col_O <- c("white", "gray", "gray", "gray", "gray", "gray", "gray")
          leg_col_SA <- c("white", "black", "black", "blue", "blue", "blue", "black" )
          leg_lwd_O <- c(NA, NA, 2, NA, 1, 1, NA)
          leg_lwd_SA <- c(NA, NA, 2, NA, 1, 1, NA)
        }
      }
      
      # Data with covariates and QA data
      if(C > 13 & Names[13] == "ac_RS"){
        if(!('1' %in% input$covcheck2)){# generates legend to include covariates
          for(i in 1:no_cov){
            if(input$covcheck2 == i+1){
              #unique <- unique(covariates[,i]) # calculate the number of unique entrys for the selcted covariate
              unique <- unique(covariates_a[,i]) # Unique entrys for sens analysis
              leg_covnames <- sort(as.character(unique))
              #leg_covnames2 <- sort(as.character(unique2))
              LL <-  10
              LL2 <- length(unique) + 10
            }
          }
          leg_col <- rep(0, times = length(unique))
          for (i in 1:length(unique)){ # assigns colour to each level
            leg_col[i] <- palette(rainbow(length(unique)))[length(unique)+1-i]
          }
          
          leg_col_O <- c("white", "green", "red", "lavenderblush3", "gray", "gray", "gray", "gray", "gray", "gray")
          leg_col_SA <-c("white", leg_col, "green", "red", "lavenderblush3", "black", "black", "blue", "blue", "blue", "black")
          
          leg_lwd2 <- rep(NA, times = length(unique))
          leg_lwd_O <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
          leg_lwd_SA <- c(NA, leg_lwd2, NA, NA, NA, NA, 2, NA, 1, 1, NA)
        }
        else{
          LL <- 10
          LL2 <- LL
          leg_col_O <- c("white", "green", "red", "lavenderblush3", "gray", "gray", "gray", "gray", "gray", "gray")
          leg_col_SA <- c("white", "green", "red", "lavenderblush3", "black", "black", "blue", "blue", "blue", "black" )
          leg_lwd_O <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
          leg_lwd_SA <- c(NA, NA, NA, NA, NA, 2, NA, 1, 1, NA)
        }
      }
      
      leglabticks <- matrix(nrow=LL, ncol = 1)
      legendticks <- matrix(nrow=LL, ncol = 2)
      leglabticks2 <- matrix(nrow=LL2, ncol = 1)
      legendticks2 <- matrix(nrow=LL2, ncol = 2)
      
      leglabticks[1] <- "Original model"
      legendticks[1,1]<-3
      leglabticks2[1] <- "Sensitvity analysis model"
      legendticks2[1,1]<-3
      
      if('1' %in% input$HSROCcheck2 | input$prevcheck2 == T){
        leglabticks[LL]<- "Data"
        legendticks[LL]<- 1
        leglabticks2[LL2]<- "Data"
        legendticks2[LL2]<- 1
      }
      
      if ('3' %in% input$bivcheck2){
        leglabticks[LL-1]<-"95% Predictive region" #legend label
        legendticks[LL-1,2]<-3
        leglabticks2[LL2-1]<-"95% Predictive region" #legend label
        legendticks2[LL2-1,2]<-3
      }
      
      if ('2' %in% input$bivcheck2){
        leglabticks[LL-2]<-"95% Confidence region" #legend label
        legendticks[LL-2,2]<-2 #creates interactive vector
        leglabticks2[LL2-2]<-"95% Confidence region" #legend label
        legendticks2[LL2-2,2]<-2 #creates interactive vector
      }
      
      if ('1' %in% input$bivcheck2){
        leglabticks[LL-3]<-"Summary estimate" #legend label
        legendticks[LL-3,1]<-15 #change plotticks and legendticks if option 1 selected
        leglabticks2[LL2-3]<-"Summary estimate" #legend label
        legendticks2[LL2-3,1]<-15 #change plotticks and legendticks if option 1 selected
      }
      if ('2' %in% input$HSROCcheck2){
        leglabticks[LL-4]<-"HSROC curve"
        legendticks[LL-4,2]<-1
        leglabticks2[LL2-4]<-"HSROC curve"
        legendticks2[LL2-4,2]<-1
      }
      
      if (('1' %in% input$cicheck2) | ('2' %in% input$cicheck2)){
        leglabticks[LL-5]<-"Uncertainty"
        legendticks[LL-5,1]<-3
        leglabticks2[LL2-5]<-"Uncertainty"
        legendticks2[LL2-5,1]<-3
      }
      
      if((C == 13 & Names[7] == "rob_PS") |(C > 13 & Names[13] == "ac_RS")){
        if(input$QAcheck2 != 1){
          leglabticks[LL-6]<-"Unclear"
          leglabticks[LL-7]<-"High"
          leglabticks[LL-8]<-"Low"
          legendticks[LL-8,1]<-1
          legendticks[LL-7,1]<-1
          legendticks[LL-6,1]<-1
          leglabticks2[LL2-6]<-"Unclear"
          leglabticks2[LL2-7]<-"High"
          leglabticks2[LL2-8]<-"Low"
          legendticks2[LL2-8,1]<-19
          legendticks2[LL2-7,1]<-19
          legendticks2[LL2-6,1]<-19
        }
      }
      
      if(C > 6 & Names[7] != "rob_PS"){
        if(!('1' %in% input$covcheck2)){
          if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
            for(i in 1:length(leg_covnames)){
              leglabticks2[LL2-5-i]<-leg_covnames[i]
              legendticks2[LL2-5-i,1]<-19
            }
            
          }
          if('1' %in% input$cov_toggle2){
            leglabticks[LL]<-"Data"
            legendticks[LL,1]<-1
            leglabticks2[LL2]<-"Data"
            legendticks2[LL2,1]<-1
          }
        }
      }
      
      if(C > 13 & Names[13] == "ac_RS"){
        if(!(1 %in% input$covcheck2)){
          if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
            for(i in 1:length(leg_covnames)){
              leglabticks2[LL2-8-i]<-leg_covnames[i]
              if('1' %in% input$QAcheck2){
                legendticks2[LL2-8-i,1]<-19
              }
              else{
                legendticks2[LL2-8-i,1]<-0
              }
            }
          }
          if('1' %in% input$cov_toggle2){
            leglabticks[LL]<-"Data"
            legendticks[LL,1]<-1
            leglabticks2[LL2]<-"Data"
            legendticks2[LL2,1]<-1
          }
        }
      }
      
      # Calculate min and max values of sens and spec to avoid extrapolation of SROC curve
      minSens <- min(Q$sens)
      maxSens <- max(Q$sens)
      minFPR <- min(Q$fpr)
      maxFPR <- max(Q$fpr)
      
      # Create new data frame which restricts roc_points to being between min and max values
      roc_points2_a <- subset(roc_points_a, FPR<maxFPR & FPR>minFPR)
      
      # Try plotting ROC curve
      
      #First create a plot with just one point (ann=F means that default titles for title and axis titles are not added
      #so I can add them myself below)
      plot(1,1, ylim=c(0,1), xlim=c(0,1), ann=F, pch=20, col="white")
      
      # Next command allows me to overlay the next graph on top
      par(new=TRUE)
      
      # Add grid lines
      abline(v=(seq(0,1,0.2)), col="lightgray", lty="dotted")
      abline(h=(seq(0,1,0.2)), col="lightgray", lty="dotted")
      
      #Add titles
      title(main=input$title_sa, xlab="False Positive Rate", ylab="Sensitivity")
      
      #Plot study level estimates
      if ('1' %in% input$HSROCcheck2){
        if('1' %in% input$mod_removal){
          if(input$weightcheck2 == TRUE){
            draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = "gray")
          }
          else{
            points(study_level$FPR, study_level$Sensitivity, pch=1, col="gray")
          }
        }
        if('2' %in% input$mod_removal){
          if(input$weightcheck2 == TRUE){
            draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a=pctsp2/1000, b=pctse2/1000)
          }
          else{
            points(study_level_P$FPR, study_level_P$Sensitivity, pch=1)
          }
        }
      }
      
      #Plot prevalence as text 
      if (input$prevcheck2 == T){
        if('1' %in% input$mod_removal){ 
          text(study_level$FPR, study_level$Sensitivity, study_level$Prev, cex =0.7, pos = 1, col = "gray")
        }
        if('2' %in% input$mod_removal){
          text(study_level_P$FPR, study_level_P$Sensitivity, study_level_P$Prev, cex =0.7, pos = 1) 
        }
      }
      
      # Plots when quality assessment data available but no covariate data
      if(C == 6){
        # plot sens
        if('1' %in% input$cicheck2){
          if('1' %in% input$mod_removal){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray")
          }
          if('2' %in% input$mod_removal){
            arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, length=0.05, angle=90, code=3)
          }
        }
        # plot spec
        if('2' %in% input$cicheck2){
          if('1' %in% input$mod_removal){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray")
          }
          if('2' %in% input$mod_removal){
            arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, length=0.05, angle=90, code=3)
          }
        }
      }
      
      # Plots when quality assessment data is available 
      if (C == 13 & Names[7] == "rob_PS"){
        # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
        for (i in 2:8){
          if('1' %in% input$mod_removal){
            if(input$QAcheck2 == i){
              if(input$weightcheck2 == FALSE){
                points(study_level$FPR, study_level$Sensitivity, pch=1, col="gray")
              }
              if(input$weightcheck2 == TRUE){
                draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = "gray")
              }
            }
          }
          if('2' %in% input$mod_removal){
            if(input$QAcheck2 ==i){
              for(m in 1:length(study_level_P$ID)){
                if (input$weightcheck2 == FALSE){
                  if (study_level_P[m,i+8] == 1){
                    points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="green")
                  }
                  else if (study_level_P[m,i+8] == 2){
                    points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="red")
                  }
                  else {
                    points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="lavenderblush3")
                  }
                }
                if (input$weightcheck2 == TRUE){
                  if(study_level_P[m,i+8] == 1){
                    draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "green")
                  }
                  if(study_level_P[m,i+8] == 2){
                    draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "red")
                  }
                  if(study_level_P[m,i+8] == 3){
                    draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "lavenderblush3")
                  }
                }
              }
            }
          }
        }
        
        
        # Pie charts to represent study level estimates split into risk of bias and applicability concerns 
        # Pie charts for risk of bias 
        if ('9' %in% input$QAcheck2){
          if ('1' %in% input$mod_removal){
            for (i in 1:max(P_rob$id)){ # uncoloured plotws for the whole dataset
              a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
              score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie1 == 1, "gray70", ifelse(score_pie1 == 2, "gray20", "gray45"))) # plots pie charts
            }
          }
          if ('2' %in% input$mod_removal){
            for (i in 1:max(P_rob_a$id)){ # coloured for those included in sensitivity analyses
              a_pie1 <- rep(P_rob_a$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
              score_pie1 <- P_rob_a$score[seq(i, length(P_rob_a$id), max(P_rob_a$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie1, P_rob_a$fpr[i], P_rob_a$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
        }
        # Pie charts for applicability concerns 
        if ('10' %in% input$QAcheck2){
          if ('1' %in% input$mod_removal){
            for (i in 1:max(P_ac$id)){
              a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
              score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie2 == 1, "gray70", ifelse(score_pie2 == 2, "gray20", "gray45"))) # plots pie charts
            }
          }
          if ('2' %in% input$mod_removal){
            for (i in 1:max(P_ac_a$id)){
              a_pie2 <- rep(P_ac_a$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
              score_pie2 <- P_ac_a$score[seq(i, length(P_ac_a$id), max(P_ac_a$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie2, P_ac_a$fpr[i], P_ac_a$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
        }
        # Pie chart that shows both applicability concerns and risk of bias 
        if ('11' %in% input$QAcheck2){
          if ('1' %in% input$mod_removal){
            for (i in 1:max(P_both$id)){ # uncoloured plots for the whole dataset
              a_pie3 <- rep(P_both$id[i], times = 7) # pie chart evenly split
              #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
              score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie3 == 1, "gray70", ifelse(score_pie3 == 2, "gray20", "gray45"))) # plots pie charts
            }
          }
          if ('2' %in% input$mod_removal){
            for (i in 1:max(P_both_a$id)){ # coloured for those included in sensitivity analyses
              a_pie3 <- rep(P_both$id[i], times = 7) # pie chart evenly split
              #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
              score_pie3 <- P_both_a$score[seq(i, length(P_both_a$id), max(P_both_a$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie3, P_both_a$fpr[i], P_both_a$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
        }
        # Add uncertainty for sens and spec at the study level 
        if ('1' %in% input$cicheck2 & (input$QAcheck2 == 1 | input$QAcheck2 == 9 | input$QAcheck2 == 10 | input$QAcheck2 == 11)){
          if('1' %in% input$mod_removal){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
          }
          if ('2' %in% input$mod_removal){
            arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, length=0.05, angle=90, code=3)
          }
        }
        if ('2' %in% input$cicheck2 & (input$QAcheck2 == 1 | input$QAcheck2 == 9 | input$QAcheck2 == 10 | input$QAcheck2 == 11)){
          if('1' %in% input$mod_removal){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
          }
          if ('2' %in% input$mod_removal){
            arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, length=0.05, angle=90, code=3)
          }
        }
        for (i in 2:8){
          if ('1' %in% input$cicheck2 & input$QAcheck2 == i){
            if('1' %in% input$mod_removal){
              arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
            }
            if ('2' %in% input$mod_removal){
              for (j in 1:length(study_level_P$ID)){
                if (study_level_P[j,i+8] == 1){
                  arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                         length=0.05, angle=90, code=3, col="green")
                }
                else if (study_level_P[j,i+8] == 2){
                  arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                         length=0.05, angle=90, code=3, col="red")
                }
                else {
                  arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                         length=0.05, angle=90, code=3, col="lavenderblush3")
                }
              }
            }
          }
          if ('2' %in% input$cicheck2 & input$QAcheck2 == i){
            if('1' %in% input$mod_removal){
              arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
            }
            if ('2' %in% input$mod_removal){
              for (j in 1:length(study_level_P$ID)){
                if (study_level_P[j,i+8] == 1){
                  arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                         length=0.05, angle=90, code=3, col = "green")
                }
                else if (study_level_P[j,i+8] == 2){
                  arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                         length=0.05, angle=90, code=3, col = "red")
                }
                else {
                  arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                         length=0.05, angle=90, code=3, col = "lavenderblush3")
                }
              }
            }
          }
        }
      }
      
      # Plots when covariate data is available but no quality assessment 
      if (C > 6 & Names[7] != "rob_PS"){
        for(i in 1:no_cov){
          if(input$covcheck2 == i+1){
            if('1' %in% input$mod_removal){
              if(input$weightcheck2 == FALSE){
                if(input$cov_toggle2 == 3){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2, col = "gray")
                  points(study_level$FPR, study_level$Sensitivity, pch=1, col = "gray")
                }
                if(input$cov_toggle2 == 2){
                  # plot covariates as coloured points
                  points(study_level$FPR, study_level$Sensitivity, pch=1, col = "gray")
                }
                if(input$cov_toggle2 == 1){
                  # plot covariates as text 
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2, col = "gray")
                }
              }
              if(input$weightcheck2 == TRUE){
                if(input$cov_toggle2 == 3){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = "gray")
                }
                if(input$cov_toggle2 == 2){
                  # plot covariates as coloured points
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = "gray")
                }
                if(input$cov_toggle2 == 1){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                }
              }
            }
            if('2' %in% input$mod_removal){
              if(input$weightcheck2 == FALSE){
                if(input$cov_toggle2 == 3){
                  # plot covariates as text and coloured points
                  text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                  points(study_level_P$FPR, study_level_P$Sensitivity, pch=19, col=as.factor(covariates_a[,i]))
                }
                if(input$cov_toggle2 == 2){
                  # plot covariates as coloured points
                  points(study_level_P$FPR, study_level_P$Sensitivity, pch=19, col=as.factor(covariates_a[,i]))
                }
                if(input$cov_toggle2 == 1){
                  # plot covariates as text and coloured points
                  text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                }
              }
              if(input$weightcheck2 == TRUE){
                if(input$cov_toggle2 == 3){
                  # plot covariates as text and coloured points
                  text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                  draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a=pctsp2/1000, b=pctse2/1000, col = as.factor(covariates_a[,i]))
                }
                if(input$cov_toggle2 == 2){
                  # plot covariates as coloured points
                  draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a=pctsp2/1000, b=pctse2/1000, col = as.factor(covariates_a[,i]))
                }
                if(input$cov_toggle2 == 1){
                  # plot covariates as text and coloured points
                  text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                }  
              }
            }
          }
        }
        
        # Plot sensitivity and specificity (for all studies)
        if ('1' %in% input$mod_removal){
          if('1' %in% input$cicheck2){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                   length=0.05, angle=90, code=3, col = "gray")
          }
          if ('2' %in% input$cicheck2){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                   length=0.05, angle=90, code=3, col = "gray")
          }
        }
        
        # Plot sensitivity and specificitiy (Senstiviity Analysis studies only)
        if ('2' %in% input$mod_removal){
          if('1' %in% input$cicheck2){
            if('1' %in% input$covcheck2){
              # When no covariates are selected (sens)
              arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI,
                     length=0.05, angle=90, code=3)
            }
            if(!('1' %in% input$covcheck2)){
              if('1' %in% input$cov_toggle2){
                # When covariates are selected as text (sens)
                arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI,
                       length=0.05, angle=90, code=3)
              }
              if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
                for(i in 1:no_cov){
                  if(input$covcheck2 == i+1){
                    # When covariates are selected as coloured points/both (sens)
                    arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI,
                           length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                  }
                }
              }
            }
          }
          if('2' %in% input$cicheck2){
            if('1' %in% input$covcheck2){
              # When no covariates are selected (spec)
              arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity,
                     length=0.05, angle=90, code=3)
            }
            if(!('1' %in% input$covcheck2)){
              if('1' %in% input$cov_toggle2){
                # When covariates are selected as text (spec)
                arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity,
                       length=0.05, angle=90, code=3)
              }
              if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
                for(i in 1:no_cov){
                  if(input$covcheck2 == i+1){
                    # When covariates are seleceted as coloured points/both (spec)
                    arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity,
                           length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                  }
                }
              }
            }
          }
        }
      } # When plots for covariate data end 
      
      # Plot when quality assessment data and covariate data is available
      
      if(C > 13 & Names[13] == "ac_RS"){
        
        if('1' %in% input$mod_removal){
          for (i in 2:8){
            if (input$QAcheck2 == i){
              if(input$weightcheck2 == FALSE){
                points(study_level$FPR, study_level$Sensitivity, pch = 1, col = "gray")
              }
              if(input$weightcheck2 == TRUE){
                draw.ellipse(study_level$FPR, study_level$Sensitivity, a = pctsp/1000, b = pctse/1000, border = "gray")
              }
            }
          }
          for (i in 1:no_cov){
            if(input$covcheck2 == i+1){
              if(input$weightcheck2 == FALSE){
                if('3' %in% input$cov_toggle2){
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex = 0.7, pos = 2, col = "gray")
                  if('1' %in% input$QAcheck2){
                    points(study_level$FPR, study_level$Sensitivity, pch = 1, col = "gray")
                  }
                  else{
                    points(study_level$FPR, study_level$Sensitivity, pch = 0, col = "gray")
                  }
                }
                if('2' %in% input$cov_toggle2){
                  if('1' %in% input$QAcheck2){
                    points(study_level$FPR, study_level$Sensitivity, pch = 1, col = "gray")
                  }
                  else {
                    points(study_level$FPR, study_level$Sensitivity, pch = 0, col = "gray")
                  }
                }
                if('1' %in% input$cov_toggle2){
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex = 0.7, pos = 2, col = "gray")
                }
              }
              if(input$weightcheck2 == TRUE){
                if('3' %in% input$cov_toggle2){
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2, col = "gray")
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a = pctsp/1000, b = pctse/1000, border = "gray")
                }
                if('2' %in% input$cov_toggle2){
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a = pctsp/1000, b = pctse/1000, border = "gray")
                }
                if('1' %in% input$cov_toggle2){
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2, col = "gray")
                }
              }
            }
          }
        }
        if('2' %in% input$mod_removal){
          for(i in 2:8){
            if(input$QAcheck2 == i){
              for(m in 1:length(study_level_P$ID)){
                if(input$weightcheck2 == FALSE){
                  if (study_level_P[m,i+8] == 1){
                    points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="green")
                  }
                  else if (study_level_P[m,i+8] == 2){
                    points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="red")
                  }
                  else {
                    points(study_level_P$FPR[m], study_level_P$Sensitivity[m], pch=19, col="lavenderblush3")
                  }
                }
                if (input$weightcheck2 == TRUE){
                  if(study_level_P[m,i+8] == 1){
                    draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "green")
                  }
                  if(study_level_P[m,i+8] == 2){
                    draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "red")
                  }
                  if(study_level_P[m,i+8] == 3){
                    draw.ellipse(study_level_P$FPR[m], study_level_P$Sensitivity[m], a=pctsp2[m]/1000, b=pctse2[m]/1000, col = "lavenderblush3")
                  }
                }
              }
            }
          }
          for(i in 1:no_cov){
            if(input$covcheck2 == i+1){
              if(input$weightcheck2 == FALSE){
                if('3' %in% input$cov_toggle2){
                  text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                  if('1' %in% input$QAcheck2){
                    points(study_level_P$FPR, study_level_P$Sensitivity, pch=19, col=as.factor(covariates_a[,i]))
                  }
                  else{
                    points(study_level_P$FPR, study_level_P$Sensitivity, pch=0, col=as.factor(covariates_a[,i]), cex = 1.2)
                  }
                }
                if('2' %in% input$cov_toggle2){
                  if('1' %in% input$QAcheck2){
                    points(study_level_P$FPR, study_level_P$Sensitivity, pch=19, col=as.factor(covariates_a[,i]))
                  }
                  else{
                    points(study_level_P$FPR, study_level_P$Sensitivity, pch=0, col=as.factor(covariates_a[,i]), cex =1.2)
                  }
                }
                if('1' %in% input$cov_toggle2){
                  text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                }
              }
              if(input$weightcheck2 == TRUE){
                if('3' %in% input$cov_toggle2){
                  text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                  if('1' %in% input$QAcheck2){
                    draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a = pctsp2/1000, b = pctse2/1000, col = as.factor(covariates_a[,i]))
                  }
                  else{
                    draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a = pctsp2/1000, b = pctse2/1000, border = as.factor(covariates_a[,i]))
                  }
                }
                if('2' %in% input$cov_toggle2){
                  if('1' %in% input$QAcheck2){
                    draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a = pctsp2/1000, b = pctse2/1000, col = as.factor(covariates_a[,i]))
                  }
                  else{
                    draw.ellipse(study_level_P$FPR, study_level_P$Sensitivity, a = pctsp2/1000, b = pctse2/1000, border = as.factor(covariates_a[,i]))
                  }
                }
                if('1' %in% input$cov_toggle2){
                  text(study_level_P$FPR, study_level_P$Sensitivity, covariates_a[,i], cex =0.7, pos = 2)
                }
              }
            }
          }
        }
        
        # Pie charts to represent study level estimates split into risk of bias and applicability concerns 
        # Pie charts for risk of bias 
        if ('9' %in% input$QAcheck2){
          if ('1' %in% input$mod_removal){
            for (i in 1:max(P_rob$id)){ # uncoloured plotws for the whole dataset
              a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
              score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie1 == 1, "gray70", ifelse(score_pie1 == 2, "gray20", "gray45"))) # plots pie charts
            }
          }
          if ('2' %in% input$mod_removal){
            for (i in 1:max(P_rob_a$id)){ # coloured for those included in sensitivity analyses
              a_pie1 <- rep(P_rob_a$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
              score_pie1 <- P_rob_a$score[seq(i, length(P_rob_a$id), max(P_rob_a$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie1, P_rob_a$fpr[i], P_rob_a$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
        }
        # Pie charts for applicability concerns 
        if ('10' %in% input$QAcheck2){
          if ('1' %in% input$mod_removal){
            for (i in 1:max(P_ac$id)){
              a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
              score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie2 == 1, "gray70", ifelse(score_pie2 == 2, "gray20", "gray45"))) # plots pie charts
            }
          }
          if ('2' %in% input$mod_removal){
            for (i in 1:max(P_ac_a$id)){
              a_pie2 <- rep(P_ac_a$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
              score_pie2 <- P_ac_a$score[seq(i, length(P_ac_a$id), max(P_ac_a$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie2, P_ac_a$fpr[i], P_ac_a$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
        }
        # Pie chart that shows both applicability concerns and risk of bias 
        if ('11' %in% input$QAcheck2){
          if ('1' %in% input$mod_removal){
            for (i in 1:max(P_both$id)){ # uncoloured plots for the whole dataset
              a_pie3 <- rep(P_both$id[i], times = 7) # pie chart evenly split
              #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
              score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie3 == 1, "gray70", ifelse(score_pie3 == 2, "gray20", "gray45"))) # plots pie charts
            }
          }
          if ('2' %in% input$mod_removal){
            for (i in 1:max(P_both_a$id)){ # coloured for those included in sensitivity analyses
              a_pie3 <- rep(P_both$id[i], times = 7) # pie chart evenly split
              #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
              score_pie3 <- P_both_a$score[seq(i, length(P_both_a$id), max(P_both_a$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie3, P_both_a$fpr[i], P_both_a$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
        }
        
        
        
        if('2' %in% input$cicheck2){
          # plot sensitvity when no covariates selected no quality assesment (except piercharts)
          if('1' %in% input$covcheck2 & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
            if('1' %in% input$mod_removal){
              arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
            }
            if('2' %in% input$mod_removal){
              arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, length=0.05, angle=90, code=3)
            }
          }
          # plot sensitivity when covariates only dispalyed as text no quality assesment (except piercharts)
          if(!('1' %in% input$covcheck2) & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
            if('1' %in% input$cov_toggle2){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
              }
              if('2' %in% input$mod_removal){
                arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, length=0.05, angle=90, code=3)
              }
            }
          }
          # Plot sensitvity when quality assessment only and no covariates
          for (i in 2:8){
            if('1' %in% input$covcheck2 & input$QAcheck2 == i){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
              }
              if ('2' %in% input$mod_removal){
                for (j in 1:length(study_level_P$ID)){
                  if (study_level_P[j,i+8] == 1){
                    arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                           length=0.05, angle=90, code=3, col="green")
                  }
                  else if (study_level_P[j,i+8] == 2){
                    arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                           length=0.05, angle=90, code=3, col="red")
                  }
                  else {
                    arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                           length=0.05, angle=90, code=3, col="lavenderblush3")
                  }
                }
              }
            }
          }
          # Plot sensitivity when quality assessment selected and covariates displayed as only text
          for (i in 2:8){
            if(!('1' %in% input$covcheck2) & input$QAcheck2 == i){
              if('1' %in% input$cov_toggle2){
                if('1' %in% input$mod_removal){
                  arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
                }
                if ('2' %in% input$mod_removal){
                  for (j in 1:length(study_level_P$ID)){
                    if (study_level_P[j,i+8] == 1){
                      arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                             length=0.05, angle=90, code=3, col="green")
                    }
                    else if (study_level_P[j,i+8] == 2){
                      arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                             length=0.05, angle=90, code=3, col="red")
                    }
                    else {
                      arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                             length=0.05, angle=90, code=3, col="lavenderblush3")
                    }
                  }
                }
              }
            }
          }
          # Plot sensitivity when covariate selected as colour and no quality assessment (except pie charts)
          for(i in 1:no_cov){
            if(input$covcheck2 == i+1 & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
              if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
                if('1' %in% input$mod_removal){
                  arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray")
                }
                if('2' %in% input$mod_removal){
                  arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, 
                         length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                }
              }
            }
          }
          # Plot sensitivity when covariates and quality assessment both selected as colours
          if(!('1' %in% input$covcheck2) & (input$QAcheck2 == 2 |input$QAcheck2 == 3 | input$QAcheck2 == 4 |input$QAcheck2 == 5 | input$QAcheck2 == 6 |input$QAcheck2 == 7 | input$QAcheck2 == 8)){
            if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
              # Plot default black 
              if('1' %in% input$ci_col2){
                if('1' %in% input$mod_removal){
                  arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col ="gray")
                }
                if('2' %in% input$mod_removal){
                  arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, 
                         length=0.05, angle=90, code=3)
                }
              }
              # Plot colour as quality assessment
              if('2' %in% input$ci_col2){
                for(i in 2:8){
                  if(input$QAcheck2 == i){
                    if('1' %in% input$mod_removal){
                      arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray50")
                    }
                    if ('2' %in% input$mod_removal){
                      for (j in 1:length(study_level_P$ID)){
                        if (study_level_P[j,i+8] == 1){
                          arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                                 length=0.05, angle=90, code=3, col="green")
                        }
                        else if (study_level_P[j,i+8] == 2){
                          arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                                 length=0.05, angle=90, code=3, col="red")
                        }
                        else {
                          arrows(study_level_P$FPR_LCI[j], study_level_P$Sensitivity[j], study_level_P$FPR_UCI[j], study_level_P$Sensitivity[j],
                                 length=0.05, angle=90, code=3, col="lavenderblush3")
                        }
                      }
                    }
                  }
                }
              }
              # Plot colour as covariates 
              if('3' %in% input$ci_col2){
                for(i in 1:no_cov){
                  if(input$covcheck2 == i+1){
                    if('1' %in% input$mod_removal){
                      arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity, length=0.05, angle=90, code=3, col="gray")
                    }
                    if('2' %in% input$mod_removal){
                      arrows(study_level_P$FPR_LCI, study_level_P$Sensitivity, study_level_P$FPR_UCI, study_level_P$Sensitivity, 
                             length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                    }
                  }
                }
              }
            }
          }
        }
        
        if('1' %in% input$cicheck2){
          # plot sensitvity when no covariates selected no quality assesment (except piercharts)
          if('1' %in% input$covcheck2 & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
            if('1' %in% input$mod_removal){
              arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
            }
            if('2' %in% input$mod_removal){
              arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, length=0.05, angle=90, code=3)
            }
          }
          # plot sensitivity when covariates only dispalyed as text no quality assesment (except piercharts)
          if(!('1' %in% input$covcheck2) & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
            if('1' %in% input$cov_toggle2){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
              }
              if('2' %in% input$mod_removal){
                arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, length=0.05, angle=90, code=3)
              }
            }
          }
          # Plot sensitvity when quality assessment only and no covariates
          for (i in 2:8){
            if('1' %in% input$covcheck2 & input$QAcheck2 == i){
              if('1' %in% input$mod_removal){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
              }
              if ('2' %in% input$mod_removal){
                for (j in 1:length(study_level_P$ID)){
                  if (study_level_P[j,i+8] == 1){
                    arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                           length=0.05, angle=90, code=3, col="green")
                  }
                  else if (study_level_P[j,i+8] == 2){
                    arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                           length=0.05, angle=90, code=3, col="red")
                  }
                  else {
                    arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                           length=0.05, angle=90, code=3, col="lavenderblush3")
                  }
                }
              }
            }
          }
          # Plot sensitivity when quality assessment selected and covariates displayed as only text
          for (i in 2:8){
            if(!('1' %in% input$covcheck2) & input$QAcheck2 == i){
              if('1' %in% input$cov_toggle2){
                if('1' %in% input$mod_removal){
                  arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
                }
                if ('2' %in% input$mod_removal){
                  for (j in 1:length(study_level_P$ID)){
                    if (study_level_P[j,i+8] == 1){
                      arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                             length=0.05, angle=90, code=3, col="green")
                    }
                    else if (study_level_P[j,i+8] == 2){
                      arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                             length=0.05, angle=90, code=3, col="red")
                    }
                    else {
                      arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                             length=0.05, angle=90, code=3, col="lavenderblush3")
                    }
                  }
                }
              }
            }
          }
          # Plot sensitivity when covariate selected as colour and no quality assessment (except pie charts)
          for(i in 1:no_cov){
            if(input$covcheck2 == i+1 & ('1' %in% input$QAcheck2 | '9' %in% input$QAcheck2 |'10' %in% input$QAcheck2 | '11' %in% input$QAcheck2)){
              if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
                if('1' %in% input$mod_removal){
                  arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray")
                }
                if('2' %in% input$mod_removal){
                  arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, 
                         length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                }
              }
            }
          }
          # Plot sensitivity when covariates and quality assessment both selected as colours
          if(!('1' %in% input$covcheck2) & (input$QAcheck2 == 2 |input$QAcheck2 == 3 | input$QAcheck2 == 4 |input$QAcheck2 == 5 | input$QAcheck2 == 6 |input$QAcheck2 == 7 | input$QAcheck2 == 8)){
            if('2' %in% input$cov_toggle2 | '3' %in% input$cov_toggle2){
              # Plot default black 
              if('1' %in% input$ci_col2){
                if('1' %in% input$mod_removal){
                  arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col ="gray")
                }
                if('2' %in% input$mod_removal){
                  arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, 
                         length=0.05, angle=90, code=3)
                }
              }
              # Plot colour as quality assessment
              if('2' %in% input$ci_col2){
                for(i in 2:8){
                  if(input$QAcheck2 == i){
                    if('1' %in% input$mod_removal){
                      arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray50")
                    }
                    if ('2' %in% input$mod_removal){
                      for (j in 1:length(study_level_P$ID)){
                        if (study_level_P[j,i+8] == 1){
                          arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                                 length=0.05, angle=90, code=3, col="green")
                        }
                        else if (study_level_P[j,i+8] == 2){
                          arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                                 length=0.05, angle=90, code=3, col="red")
                        }
                        else {
                          arrows(study_level_P$FPR[j], study_level_P$Sens_LCI[j], study_level_P$FPR[j], study_level_P$Sens_UCI[j],
                                 length=0.05, angle=90, code=3, col="lavenderblush3")
                        }
                      }
                    }
                  }
                }
              }
              # Plot colour as covariates 
              if('3' %in% input$ci_col2){
                for(i in 1:no_cov){
                  if(input$covcheck2 == i+1){
                    if('1' %in% input$mod_removal){
                      arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length=0.05, angle=90, code=3, col="gray")
                    }
                    if('2' %in% input$mod_removal){
                      arrows(study_level_P$FPR, study_level_P$Sens_LCI, study_level_P$FPR, study_level_P$Sens_UCI, 
                             length=0.05, angle=90, code=3, col = as.factor(covariates_a[,i]))
                    }
                  }
                }
              }
            }
          }
        }
      }# End of plots for quality assessment and covariate data
      
      # Plot the original model
      if('1' %in% input$mod_removal){
        # Add the ROC curve 
        if('2' %in% input$HSROCcheck2){
          if(input$extrapp2 == T){
            points(roc_points, type = "l", col = "gray")
          }
          else{
            points(roc_points2, type = "l", col = "gray")
          }
        }
        # Add the summary point 
        if('1' %in% input$bivcheck2){
          points(mean_point, col = "gray", pch = 15)
        }
        # Add the confidence region 
        if('2' %in% input$bivcheck2){
          lines(conf_region, lty = 2, col = "gray")
        }
        if('3' %in% input$bivcheck2){
          lines(pred_region, lty = 3, col = "gray")
        }
      }
      
      # Plot the sensitivity analysis model
      if('2' %in% input$mod_removal){
        # Add the ROC curve 
        if('2' %in% input$HSROCcheck2){
          if(input$extrapp2 == T){
            points(roc_points_a, type = "l")
          }
          else{
            points(roc_points2_a, type = "l")
          }
        }
        # Add the summary point 
        if('1' %in% input$bivcheck2){
          points(mean_point_a, col = "blue", pch = 15)
        }
        # Add the confidence region 
        if('2' %in% input$bivcheck2){
          lines(conf_region_a, lty = 2, col = "blue")
        }
        if('3' %in% input$bivcheck2){
          lines(pred_region_a, lty = 3, col = "blue")
        }
      }
      
      legend("bottomright", bty = "n", leglabticks2[1:LL2,1], pch = legendticks2[1:LL2,1], lty = legendticks2[1:LL2,2],
             lwd = leg_lwd_SA, col = leg_col_SA)
      
      legend("bottom", bty = "n", leglabticks[1:LL,1], pch = legendticks[1:LL,1], lty = legendticks[1:LL,2],
             lwd = leg_lwd_O, col = leg_col_O)

      dev.off()

    }

  )

  # Want to compare the statistics from all trials to the statistics from only the included trials
  # Need to create two tables
  # Start with all trials
  # Table of MA statistics - sensitivity tab, original data - overall sens, spec etc
  output$orig_statTable <- renderTable({
    if(is.null(data())){return()}
    else
      X <- data()

    N <- length(X$TP)

    ## In order to specify the generalized linear model, first, we need to set up the data
    ## Set up the data
    ## Generate 5 new variables of type long. We need these before we can reshape the data.
    # Â	n1 is number diseased
    # Â	n0 is number without disease
    # Â	true1 is number of true positives
    # Â	true0 is the number of true negatives
    # Â	study is the unique identifier for each study. _n will generate a sequence of numbers.

    X$n1 <- X$TP+X$FN
    X$n0 <- X$FP+X$TN
    X$true1 <- X$TP
    X$true0 <- X$TN
    X$study <- 1:N

    ## Reshape the data from wide to long format ###
    Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                timevar = "sens" , times = c(1,0) , v.names = c("n","true") )

    ## Sort data by study to cluster the 2 records per study together ###
    Y = Y[order(Y$id),]
    Y$spec<- 1-Y$sens

    ## Perform meta-analysis ##
    MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                  data = Y , family = binomial , nAGQ=1 , verbose=0 )

    ma_Y = summary(MA_Y)
    labels( ma_Y )
    ma_Y$coeff

    lsens = ma_Y$coeff[1,1]
    lspec = ma_Y$coeff[2,1]

    se.lsens = ma_Y$coeff[1,2]
    se.lspec = ma_Y$coeff[2,2]

    ## Then we can manually create 95% confidence intervals for logit sens and spec
    logit_Sens = c(lsens, lsens-qnorm(0.975)*se.lsens, lsens+qnorm(0.975)*se.lsens )
    logit_Spec = c(lspec, lspec-qnorm(0.975)*se.lspec, lspec+qnorm(0.975)*se.lspec )
    Sens = plogis( logit_Sens )
    Spec = plogis( logit_Spec )

    # Calculate false positive rate
    fpr <- c(1-Spec[1], 1-Spec[3], 1-Spec[2])

    # Extract correlation
    correlation <- attributes(ma_Y$varcor$study)$correlation[2]

    ####### DOR and likelihood ratios
    DOR = exp(lsens+lspec )
    LRp = plogis(lsens)/(1-plogis(lspec))
    LRn = ((1-plogis(lsens))/plogis(lspec))

    ## Standard errors and confidence intervals can be calculated using deltamethod.
    # This requires the package msm
    se.DOR = deltamethod (~ exp(x1+x2) , mean = c(lsens,lspec) , cov = ma_Y$vcov )
    se.LRp = deltamethod (~ (exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2)))) ,
                          mean = c(lsens,lspec) , cov = ma_Y$vcov )
    se.LRn = deltamethod (~ (1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2))) ,
                          mean = c(lsens,lspec) , cov = ma_Y$vcov )
    
    # Now calculate standrad error for log DOR, log LR+ and log LR-
    logse.DOR = deltamethod (~ log(exp(x1+x2)) , mean = c(lsens,lspec) , cov = ma_Y$vcov )
    
    logse.LRp = deltamethod (~ log((exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2))))) , 
                             mean = c(lsens,lspec) , cov = ma_Y$vcov )
    
    logse.LRn = deltamethod (~ log((1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2)))) , 
                             mean = c(lsens,lspec) , cov = ma_Y$vcov )

    # Use estimates of bivariate model parameters to calculate HSROC parameters
    sigma2_a <- ma_Y$varcor$study[1]
    sigma2_b <- ma_Y$varcor$study[4]
    sigma_ab <- ma_Y$varcor$study[2]
    sigma_a <- sqrt(sigma2_a)
    sigma_b <- sqrt(sigma2_b)

    beta <- log(sigma_b/sigma_a)
    Theta <- 0.5*( (((sigma_b/sigma_a)^0.5 )*lsens) - ( ( (sigma_a/sigma_b)^0.5) *lspec) )
    Lambda <- ( ( (sigma_b/sigma_a)^0.5) * lsens) + ( (sigma_a/sigma_b)^0.5 *lspec)
    sigma2_theta <- 0.5*( (sigma_a*sigma_b) - sigma_ab )
    sigma2_alpha <- 2*( (sigma_a*sigma_b) + sigma_ab )

    # We don't get confidence intervals for the HSROC parameters 
    # If I put them in th data frame as NA then they turn the LCI and UCI into character strings
    # Then I used to use as.numeric to convert them to numeric but this doesn't work in R version 4.0.1
    # So now I use 9999 instead. I consider this to be such a number that noone would believe it is correct
    # so it shouldn't have any impact on the user experience
    Z = data.frame(estimate = c(logit_Sens[1], logit_Spec[1], Sens[1], Spec[1], fpr[1], DOR , LRp , LRn, correlation,
                                Theta, Lambda, beta, sigma2_theta, sigma2_alpha),
                   lci = c(logit_Sens[2], logit_Spec[2], Sens[2], Spec[2], fpr[2], exp(log(DOR)-qnorm(0.975)*logse.DOR) , 
                           exp(log(LRp)-qnorm(0.975)*logse.LRp) , 
                           exp(log(LRn)-qnorm(0.975)*logse.LRn), 9999, 9999, 9999, 9999, 9999, 9999),
                   uci = c(logit_Sens[3], logit_Spec[3], Sens[3], Spec[3], fpr[3], exp(log(DOR)+qnorm(0.975)*logse.DOR) , 
                           exp(log(LRp)+qnorm(0.975)*logse.LRp) , 
                           exp(log(LRn)+qnorm(0.975)*logse.LRn), 9999, 9999, 9999, 9999, 9999, 9999),
                   row.names=c("Logit Sens", "Logit Spec", "Sensitivity", "Specifcity", "FPR", "DOR", "LR+" , "LR-", "Correlation",
                               "Theta", "Lambda", "beta", 
                               "sigma2_theta", "sigma2_alpha")
    )
    
    # Create a matrix to store the parameter estimates
    s.matrix <- matrix(nrow=15, ncol=4)
    s.matrix[1,1] <- "Sensitivity"
    s.matrix[2,1] <- "Specificity"
    s.matrix[3,1] <- "False Positive Rate"
    s.matrix[4,1] <- "Random Effects Correlation"
    s.matrix[5,1] <- HTML("&theta;")
    s.matrix[6,1] <- HTML("&lambda;")
    s.matrix[7,1] <- HTML("&beta;")
    s.matrix[8,1] <- HTML("&sigma;<sub>&theta;</sub>")
    s.matrix[9,1] <- HTML("&sigma;<sub>&alpha;</sub>")
    s.matrix[10,1] <- "Diagnostic Odds Ratio"
    s.matrix[11,1] <- "Likelihood Ratio +ve"
    s.matrix[12,1] <- "Likelihood Ratio -ve"
    s.matrix[13,1] <- "logit(sensitivity)"
    s.matrix[14,1] <- "logit(specificity)"
    s.matrix[1,2] <- sprintf('%4.3f', Z[3,1])
    s.matrix[2,2] <- sprintf('%4.3f', Z[4,1])
    s.matrix[3,2] <- sprintf('%4.3f', Z[5,1])
    s.matrix[4,2] <- sprintf('%4.3f', Z[9,1])
    s.matrix[5,2] <- sprintf('%4.3f', Z[10,1])
    s.matrix[6,2] <- sprintf('%4.3f', Z[11,1])
    s.matrix[7,2] <- sprintf('%4.3f', Z[12,1])
    s.matrix[8,2] <- sprintf('%4.3f', Z[13,1])
    s.matrix[9,2] <- sprintf('%4.3f', Z[14,1])
    s.matrix[10,2] <- sprintf('%4.3f', Z[6,1])
    s.matrix[11,2] <- sprintf('%4.3f', Z[7,1])
    s.matrix[12,2] <- sprintf('%4.3f', Z[8,1])
    s.matrix[13,2] <- sprintf('%4.3f', Z[1,1])
    s.matrix[14,2] <- sprintf('%4.3f', Z[2,1])
    s.matrix[1,3] <- sprintf('%4.3f', Z[3,2])
    s.matrix[1,4] <- sprintf('%4.3f', Z[3,3])
    s.matrix[2,3] <- sprintf('%4.3f', Z[4,2])
    s.matrix[2,4] <- sprintf('%4.3f', Z[4,3])
    s.matrix[3,3] <- sprintf('%4.3f', Z[5,2])
    s.matrix[3,4] <- sprintf('%4.3f', Z[5,3])
    s.matrix[4,3] <- ""
    s.matrix[4,4] <- ""
    s.matrix[10,3] <- sprintf('%4.3f', Z[6,2])
    s.matrix[10,4] <- sprintf('%4.3f', Z[6,3])
    s.matrix[11,3] <- sprintf('%4.3f', Z[7,2])
    s.matrix[11,4] <- sprintf('%4.3f', Z[7,3])
    s.matrix[12,3] <- sprintf('%4.3f', Z[8,2])
    s.matrix[12,4] <- sprintf('%4.3f', Z[8,3])
    s.matrix[5,3] <- ""
    s.matrix[5,4] <- ""
    s.matrix[6,3] <- ""
    s.matrix[6,4] <- ""
    s.matrix[7,3] <- ""
    s.matrix[7,4] <- ""
    s.matrix[8,3] <- ""
    s.matrix[8,4] <- ""
    s.matrix[9,3] <- ""
    s.matrix[9,4] <- ""
    s.matrix[13,3] <- sprintf('%4.3f', Z[1,2])
    s.matrix[14,3] <- sprintf('%4.3f', Z[2,2])
    s.matrix[13,4] <- sprintf('%4.3f', Z[1,3])
    s.matrix[14,4] <- sprintf('%4.3f', Z[2,3])
    s.matrix[15, 1:4] <- ""

    #Name the columns of the matrix
    colnames(s.matrix) <- c("Parameter", "Estimate", "2.5% CI", "97.5% CI")

    #Conditions to display which statistics are shown in the table
    #Start with a logical vector of false and replace ths with true if the corresponding box is ticked
    statticks <- logical(length=15) # default is false
    # always have the bottom empty row showing
    statticks[15] <- TRUE
    # which rows are displayed will depend on the options selected
    if ('1' %in% input$statscheck2) {statticks[1] <- T}
    if ('2' %in% input$statscheck2) {statticks[2] <- T}
    if ('3' %in% input$statscheck2) {statticks[3] <- T}
    if ('4' %in% input$statscheck2) {statticks[4] <- T}
    if ('5' %in% input$statscheck2) {statticks[5:9] <- T}
    if ('6' %in% input$statscheck2) {statticks[10] <- T}
    if ('7' %in% input$statscheck2) {statticks[11:12] <- T}
    if ('1' %in% input$statscheck2) {statticks[13] <- T}
    if ('2' %in% input$statscheck2) {statticks[14] <- T}

    #Only the rows of s.matrix where statticks=T will be displayed
    s.matrix[statticks,]
  }, sanitize.text.function = function(x) x)



  # Second table to display statistics for included trials only
  # Table of MA statistics - sensitivity tab, original data - overall sens, spec etc
  output$sa_statTable <- renderTable({
    if(is.null(data())){return()}
    else
      adf <- data()
    X <- adf[input$triallist, ]

    N <- length(X$TP)

    ## In order to specify the generalized linear model, first, we need to set up the data
    ## Set up the data
    ## Generate 5 new variables of type long. We need these before we can reshape the data.
    # Â	n1 is number diseased
    # Â	n0 is number without disease
    # Â	true1 is number of true positives
    # Â	true0 is the number of true negatives
    # Â	study is the unique identifier for each study. _n will generate a sequence of numbers.

    X$n1 <- X$TP+X$FN
    X$n0 <- X$FP+X$TN
    X$true1 <- X$TP
    X$true0 <- X$TN
    X$study <- 1:N

    ## Reshape the data from wide to long format ###
    Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                timevar = "sens" , times = c(1,0) , v.names = c("n","true") )

    ## Sort data by study to cluster the 2 records per study together ###
    Y = Y[order(Y$id),]
    Y$spec<- 1-Y$sens

    ## Perform meta-analysis ##
    MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                  data = Y , family = binomial , nAGQ=1 , verbose=0 )

    ma_Y = summary(MA_Y)
    labels( ma_Y )
    ma_Y$coeff

    lsens = ma_Y$coeff[1,1]
    lspec = ma_Y$coeff[2,1]

    se.lsens = ma_Y$coeff[1,2]
    se.lspec = ma_Y$coeff[2,2]

    ## Then we can manually create 95% confidence intervals for logit sens and spec
    logit_Sens = c(lsens, lsens-qnorm(0.975)*se.lsens, lsens+qnorm(0.975)*se.lsens )
    logit_Spec = c(lspec, lspec-qnorm(0.975)*se.lspec, lspec+qnorm(0.975)*se.lspec )
    Sens = plogis( logit_Sens )
    Spec = plogis( logit_Spec )

    # Calculate false positive rate
    fpr <- c(1-Spec[1], 1-Spec[3], 1-Spec[2])

    # Extract correlation
    correlation <- attributes(ma_Y$varcor$study)$correlation[2]

    ####### DOR and likelihood ratios
    DOR = exp(lsens+lspec )
    LRp = plogis(lsens)/(1-plogis(lspec))
    LRn = ((1-plogis(lsens))/plogis(lspec))

    ## Standard errors and confidence intervals can be calculated using deltamethod.
    # This requires the package msm
    se.DOR = deltamethod (~ exp(x1+x2) , mean = c(lsens,lspec) , cov = ma_Y$vcov )
    se.LRp = deltamethod (~ (exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2)))) ,
                          mean = c(lsens,lspec) , cov = ma_Y$vcov )
    se.LRn = deltamethod (~ (1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2))) ,
                          mean = c(lsens,lspec) , cov = ma_Y$vcov )
    
    # Now calculate standard error for log DOR, log LR+ and log LR-
    logse.DOR = deltamethod (~ log(exp(x1+x2)) , mean = c(lsens,lspec) , cov = ma_Y$vcov )
    
    logse.LRp = deltamethod (~ log((exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2))))) , 
                             mean = c(lsens,lspec) , cov = ma_Y$vcov )
    
    logse.LRn = deltamethod (~ log((1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2)))) , 
                             mean = c(lsens,lspec) , cov = ma_Y$vcov )

    # Use estimates of bivariate model parameters to calculate HSROC parameters
    sigma2_a <- ma_Y$varcor$study[1]
    sigma2_b <- ma_Y$varcor$study[4]
    sigma_ab <- ma_Y$varcor$study[2]
    sigma_a <- sqrt(sigma2_a)
    sigma_b <- sqrt(sigma2_b)

    beta <- log(sigma_b/sigma_a)
    Theta <- 0.5*( (((sigma_b/sigma_a)^0.5 )*lsens) - ( ( (sigma_a/sigma_b)^0.5) *lspec) )
    Lambda <- ( ( (sigma_b/sigma_a)^0.5) * lsens) + ( (sigma_a/sigma_b)^0.5 *lspec)
    sigma2_theta <- 0.5*( (sigma_a*sigma_b) - sigma_ab )
    sigma2_alpha <- 2*( (sigma_a*sigma_b) + sigma_ab )

    # We don't get confidence intervals for the HSROC parameters 
    # If I put them in th data frame as NA then they turn the LCI and UCI into character strings
    # Then I used to use as.numeric to convert them to numeric but this doesn't work in R version 4.0.1
    # So now I use 9999 instead. I consider this to be such a number that noone would believe it is correct
    # so it shouldn't have any impact on the user experience
    Z = data.frame(estimate = c(logit_Sens[1], logit_Spec[1], Sens[1], Spec[1], fpr[1], DOR , LRp , LRn, correlation,
                                Theta, Lambda, beta, sigma2_theta, sigma2_alpha),
                   lci = c(logit_Sens[2], logit_Spec[2], Sens[2], Spec[2], fpr[2], exp(log(DOR)-qnorm(0.975)*logse.DOR) , 
                           exp(log(LRp)-qnorm(0.975)*logse.LRp) , 
                           exp(log(LRn)-qnorm(0.975)*logse.LRn), 9999, 9999, 9999, 9999, 9999, 9999),
                   uci = c(logit_Sens[3], logit_Spec[3], Sens[3], Spec[3], fpr[3], exp(log(DOR)+qnorm(0.975)*logse.DOR) , 
                           exp(log(LRp)+qnorm(0.975)*logse.LRp) , 
                           exp(log(LRn)+qnorm(0.975)*logse.LRn), 9999, 9999, 9999, 9999, 9999, 9999),
                   row.names=c("Logit Sens", "Logit Spec", "Sensitivity", "Specifcity", "FPR", "DOR", "LR+" , "LR-", "Correlation",
                               "Theta", "Lambda", "beta", 
                               "sigma2_theta", "sigma2_alpha")
    )
    
    # Create a matrix to store the parameter estimates
    s.matrix <- matrix(nrow=15, ncol=4)
    s.matrix[1,1] <- "Sensitivity"
    s.matrix[2,1] <- "Specificity"
    s.matrix[3,1] <- "False Positive Rate"
    s.matrix[4,1] <- "Random Effects Correlation"
    s.matrix[5,1] <- HTML("&theta;")
    s.matrix[6,1] <- HTML("&lambda;")
    s.matrix[7,1] <- HTML("&beta;")
    s.matrix[8,1] <- HTML("&sigma;<sub>&theta;</sub>")
    s.matrix[9,1] <- HTML("&sigma;<sub>&alpha;</sub>")
    s.matrix[10,1] <- "Diagnostic Odds Ratio"
    s.matrix[11,1] <- "Likelihood Ratio +ve"
    s.matrix[12,1] <- "Likelihood Ratio -ve"
    s.matrix[13,1] <- "logit(sensitivity)"
    s.matrix[14,1] <- "logit(specificity)"
    s.matrix[1,2] <- sprintf('%4.3f', Z[3,1])
    s.matrix[2,2] <- sprintf('%4.3f', Z[4,1])
    s.matrix[3,2] <- sprintf('%4.3f', Z[5,1])
    s.matrix[4,2] <- sprintf('%4.3f', Z[9,1])
    s.matrix[5,2] <- sprintf('%4.3f', Z[10,1])
    s.matrix[6,2] <- sprintf('%4.3f', Z[11,1])
    s.matrix[7,2] <- sprintf('%4.3f', Z[12,1])
    s.matrix[8,2] <- sprintf('%4.3f', Z[13,1])
    s.matrix[9,2] <- sprintf('%4.3f', Z[14,1])
    s.matrix[10,2] <- sprintf('%4.3f', Z[6,1])
    s.matrix[11,2] <- sprintf('%4.3f', Z[7,1])
    s.matrix[12,2] <- sprintf('%4.3f', Z[8,1])
    s.matrix[13,2] <- sprintf('%4.3f', Z[1,1])
    s.matrix[14,2] <- sprintf('%4.3f', Z[2,1])
    s.matrix[1,3] <- sprintf('%4.3f', Z[3,2])
    s.matrix[1,4] <- sprintf('%4.3f', Z[3,3])
    s.matrix[2,3] <- sprintf('%4.3f', Z[4,2])
    s.matrix[2,4] <- sprintf('%4.3f', Z[4,3])
    s.matrix[3,3] <- sprintf('%4.3f', Z[5,2])
    s.matrix[3,4] <- sprintf('%4.3f', Z[5,3])
    s.matrix[4,3] <- ""
    s.matrix[4,4] <- ""
    s.matrix[10,3] <- sprintf('%4.3f', Z[6,2])
    s.matrix[10,4] <- sprintf('%4.3f', Z[6,3])
    s.matrix[11,3] <- sprintf('%4.3f', Z[7,2])
    s.matrix[11,4] <- sprintf('%4.3f', Z[7,3])
    s.matrix[12,3] <- sprintf('%4.3f', Z[8,2])
    s.matrix[12,4] <- sprintf('%4.3f', Z[8,3])
    s.matrix[5,3] <- ""
    s.matrix[5,4] <- ""
    s.matrix[6,3] <- ""
    s.matrix[6,4] <- ""
    s.matrix[7,3] <- ""
    s.matrix[7,4] <- ""
    s.matrix[8,3] <- ""
    s.matrix[8,4] <- ""
    s.matrix[9,3] <- ""
    s.matrix[9,4] <- ""
    s.matrix[13,3] <- sprintf('%4.3f', Z[1,2])
    s.matrix[14,3] <- sprintf('%4.3f', Z[2,2])
    s.matrix[13,4] <- sprintf('%4.3f', Z[1,3])
    s.matrix[14,4] <- sprintf('%4.3f', Z[2,3])
    s.matrix[15, 1:4] <- ""

    #Name the columns of the matrix
    colnames(s.matrix) <- c("Parameter", "Estimate", "2.5% CI", "97.5% CI")

    #Conditions to display which statistics are shown in the table
    #Start with a logical vector of false and replace ths with true if the corresponding box is ticked
    statticks <- logical(length=15) # default is false
    # always have the bottom empty row showing
    statticks[15] <- TRUE
    # which rows are displayed will depend on the options selected
    if ('1' %in% input$statscheck2) {statticks[1] <- T}
    if ('2' %in% input$statscheck2) {statticks[2] <- T}
    if ('3' %in% input$statscheck2) {statticks[3] <- T}
    if ('4' %in% input$statscheck2) {statticks[4] <- T}
    if ('5' %in% input$statscheck2) {statticks[5:9] <- T}
    if ('6' %in% input$statscheck2) {statticks[10] <- T}
    if ('7' %in% input$statscheck2) {statticks[11:12] <- T}
    if ('1' %in% input$statscheck2) {statticks[13] <- T}
    if ('2' %in% input$statscheck2) {statticks[14] <- T}

    #Only the rows of s.matrix where statticks=T will be displayed
    s.matrix[statticks,]
  }, sanitize.text.function = function(x) x)


  # Allow users the option to download the table of statistics for included trials only
  output$downloadSATable <- downloadHandler(
    # Specify the file name
    filename = function(){
      paste("SATable.csv")
    },
    content = function(file){
      # open the device
      # create the plot
      # close the device

      if(is.null(data())){return()}
      else
        adf <- data()
      X <- adf[input$triallist, ]

      N <- length(X$TP)

      ## In order to specify the generalized linear model, first, we need to set up the data
      ## Set up the data
      ## Generate 5 new variables of type long. We need these before we can reshape the data.
      # Â	n1 is number diseased
      # Â	n0 is number without disease
      # Â	true1 is number of true positives
      # Â	true0 is the number of true negatives
      # Â	study is the unique identifier for each study. _n will generate a sequence of numbers.

      X$n1 <- X$TP+X$FN
      X$n0 <- X$FP+X$TN
      X$true1 <- X$TP
      X$true0 <- X$TN
      X$study <- 1:N

      ## Reshape the data from wide to long format ###
      Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                  timevar = "sens" , times = c(1,0) , v.names = c("n","true") )

      ## Sort data by study to cluster the 2 records per study together ###
      Y = Y[order(Y$id),]
      Y$spec<- 1-Y$sens

      ## Perform meta-analysis ##
      MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                    data = Y , family = binomial , nAGQ=1 , verbose=0 )

      ma_Y = summary(MA_Y)
      labels( ma_Y )
      ma_Y$coeff

      lsens = ma_Y$coeff[1,1]
      lspec = ma_Y$coeff[2,1]

      se.lsens = ma_Y$coeff[1,2]
      se.lspec = ma_Y$coeff[2,2]

      ## Then we can manually create 95% confidence intervals for logit sens and spec
      logit_Sens = c(lsens, lsens-qnorm(0.975)*se.lsens, lsens+qnorm(0.975)*se.lsens )
      logit_Spec = c(lspec, lspec-qnorm(0.975)*se.lspec, lspec+qnorm(0.975)*se.lspec )
      Sens = plogis( logit_Sens )
      Spec = plogis( logit_Spec )

      # Calculate false positive rate
      fpr <- c(1-Spec[1], 1-Spec[3], 1-Spec[2])

      # Extract correlation
      correlation <- attributes(ma_Y$varcor$study)$correlation[2]

      ####### DOR and likelihood ratios
      DOR = exp(lsens+lspec )
      LRp = plogis(lsens)/(1-plogis(lspec))
      LRn = ((1-plogis(lsens))/plogis(lspec))

      ## Standard errors and confidence intervals can be calculated using deltamethod.
      # This requires the package msm
      se.DOR = deltamethod (~ exp(x1+x2) , mean = c(lsens,lspec) , cov = ma_Y$vcov )
      se.LRp = deltamethod (~ (exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2)))) ,
                            mean = c(lsens,lspec) , cov = ma_Y$vcov )
      se.LRn = deltamethod (~ (1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2))) ,
                            mean = c(lsens,lspec) , cov = ma_Y$vcov )
      
      # Now calculate standard error for log DOR, log LR+ and log LR-
      logse.DOR = deltamethod (~ log(exp(x1+x2)) , mean = c(lsens,lspec) , cov = ma_Y$vcov )
      
      logse.LRp = deltamethod (~ log((exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2))))) , 
                               mean = c(lsens,lspec) , cov = ma_Y$vcov )
      
      logse.LRn = deltamethod (~ log((1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2)))) , 
                               mean = c(lsens,lspec) , cov = ma_Y$vcov )

      # Use estimates of bivariate model parameters to calculate HSROC parameters
      sigma2_a <- ma_Y$varcor$study[1]
      sigma2_b <- ma_Y$varcor$study[4]
      sigma_ab <- ma_Y$varcor$study[2]
      sigma_a <- sqrt(sigma2_a)
      sigma_b <- sqrt(sigma2_b)

      beta <- log(sigma_b/sigma_a)
      Theta <- 0.5*( (((sigma_b/sigma_a)^0.5 )*lsens) - ( ( (sigma_a/sigma_b)^0.5) *lspec) )
      Lambda <- ( ( (sigma_b/sigma_a)^0.5) * lsens) + ( (sigma_a/sigma_b)^0.5 *lspec)
      sigma2_theta <- 0.5*( (sigma_a*sigma_b) - sigma_ab )
      sigma2_alpha <- 2*( (sigma_a*sigma_b) + sigma_ab )

      # We don't get confidence intervals for the HSROC parameters 
      # If I put them in th data frame as NA then they turn the LCI and UCI into character strings
      # Then I used to use as.numeric to convert them to numeric but this doesn't work in R version 4.0.1
      # So now I use 9999 instead. I consider this to be such a number that noone would believe it is correct
      # so it shouldn't have any impact on the user experience
      Z = data.frame(estimate = c(logit_Sens[1], logit_Spec[1], Sens[1], Spec[1], fpr[1], DOR , LRp , LRn, correlation,
                                  Theta, Lambda, beta, sigma2_theta, sigma2_alpha),
                     lci = c(logit_Sens[2], logit_Spec[2], Sens[2], Spec[2], fpr[2], exp(log(DOR)-qnorm(0.975)*logse.DOR) , 
                             exp(log(LRp)-qnorm(0.975)*logse.LRp) , 
                             exp(log(LRn)-qnorm(0.975)*logse.LRn), 9999, 9999, 9999, 9999, 9999, 9999),
                     uci = c(logit_Sens[3], logit_Spec[3], Sens[3], Spec[3], fpr[3], exp(log(DOR)+qnorm(0.975)*logse.DOR) , 
                             exp(log(LRp)+qnorm(0.975)*logse.LRp) , 
                             exp(log(LRn)+qnorm(0.975)*logse.LRn), 9999, 9999, 9999, 9999, 9999, 9999),
                     row.names=c("Logit Sens", "Logit Spec", "Sensitivity", "Specifcity", "FPR", "DOR", "LR+" , "LR-", "Correlation",
                                 "Theta", "Lambda", "beta", 
                                 "sigma2_theta", "sigma2_alpha")
      )
      
      # Create a matrix to store the parameter estimates
      s.matrix <- matrix(nrow=15, ncol=4)
      s.matrix[1,1] <- "Sensitivity"
      s.matrix[2,1] <- "Specificity"
      s.matrix[3,1] <- "False Positive Rate"
      s.matrix[4,1] <- "Random Effects Correlation"
      s.matrix[5,1] <- HTML("&theta;")
      s.matrix[6,1] <- HTML("&lambda;")
      s.matrix[7,1] <- HTML("&beta;")
      s.matrix[8,1] <- HTML("&sigma;<sub>&theta;</sub>")
      s.matrix[9,1] <- HTML("&sigma;<sub>&alpha;</sub>")
      s.matrix[10,1] <- "Diagnostic Odds Ratio"
      s.matrix[11,1] <- "Likelihood Ratio +ve"
      s.matrix[12,1] <- "Likelihood Ratio -ve"
      s.matrix[13,1] <- "logit(sensitivity)"
      s.matrix[14,1] <- "logit(specificity)"
      s.matrix[1,2] <- sprintf('%4.3f', Z[3,1])
      s.matrix[2,2] <- sprintf('%4.3f', Z[4,1])
      s.matrix[3,2] <- sprintf('%4.3f', Z[5,1])
      s.matrix[4,2] <- sprintf('%4.3f', Z[9,1])
      s.matrix[5,2] <- sprintf('%4.3f', Z[10,1])
      s.matrix[6,2] <- sprintf('%4.3f', Z[11,1])
      s.matrix[7,2] <- sprintf('%4.3f', Z[12,1])
      s.matrix[8,2] <- sprintf('%4.3f', Z[13,1])
      s.matrix[9,2] <- sprintf('%4.3f', Z[14,1])
      s.matrix[10,2] <- sprintf('%4.3f', Z[6,1])
      s.matrix[11,2] <- sprintf('%4.3f', Z[7,1])
      s.matrix[12,2] <- sprintf('%4.3f', Z[8,1])
      s.matrix[13,2] <- sprintf('%4.3f', Z[1,1])
      s.matrix[14,2] <- sprintf('%4.3f', Z[2,1])
      s.matrix[1,3] <- sprintf('%4.3f', Z[3,2])
      s.matrix[1,4] <- sprintf('%4.3f', Z[3,3])
      s.matrix[2,3] <- sprintf('%4.3f', Z[4,2])
      s.matrix[2,4] <- sprintf('%4.3f', Z[4,3])
      s.matrix[3,3] <- sprintf('%4.3f', Z[5,2])
      s.matrix[3,4] <- sprintf('%4.3f', Z[5,3])
      s.matrix[4,3] <- ""
      s.matrix[4,4] <- ""
      s.matrix[10,3] <- sprintf('%4.3f', Z[6,2])
      s.matrix[10,4] <- sprintf('%4.3f', Z[6,3])
      s.matrix[11,3] <- sprintf('%4.3f', Z[7,2])
      s.matrix[11,4] <- sprintf('%4.3f', Z[7,3])
      s.matrix[12,3] <- sprintf('%4.3f', Z[8,2])
      s.matrix[12,4] <- sprintf('%4.3f', Z[8,3])
      s.matrix[5,3] <- ""
      s.matrix[5,4] <- ""
      s.matrix[6,3] <- ""
      s.matrix[6,4] <- ""
      s.matrix[7,3] <- ""
      s.matrix[7,4] <- ""
      s.matrix[8,3] <- ""
      s.matrix[8,4] <- ""
      s.matrix[9,3] <- ""
      s.matrix[9,4] <- ""
      s.matrix[13,3] <- sprintf('%4.3f', Z[1,2])
      s.matrix[14,3] <- sprintf('%4.3f', Z[2,2])
      s.matrix[13,4] <- sprintf('%4.3f', Z[1,3])
      s.matrix[14,4] <- sprintf('%4.3f', Z[2,3])
      s.matrix[15, 1:4] <- ""

      #Name the columns of the matrix
      colnames(s.matrix) <- c("Parameter", "Estimate", "2.5% CI", "97.5% CI")

      #Conditions to display which statistics are shown in the table
      #Start with a logical vector of false and replace ths with true if the corresponding box is ticked
      statticks <- logical(length=15) # default is false
      # always have the bottom empty row showing
      statticks[15] <- TRUE
      # which rows are displayed will depend on the options selected
      if ('1' %in% input$statscheck2) {statticks[1] <- T}
      if ('2' %in% input$statscheck2) {statticks[2] <- T}
      if ('3' %in% input$statscheck2) {statticks[3] <- T}
      if ('4' %in% input$statscheck2) {statticks[4] <- T}
      if ('5' %in% input$statscheck2) {statticks[5:9] <- T}
      if ('6' %in% input$statscheck2) {statticks[10] <- T}
      if ('7' %in% input$statscheck2) {statticks[11:12] <- T}
      if ('1' %in% input$statscheck2) {statticks[13] <- T}
      if ('2' %in% input$statscheck2) {statticks[14] <- T}

      #Only the rows of s.matrix where statticks=T will be displayed
      write.table(s.matrix[statticks,], file, sep=",", row.names=FALSE)


    }#, sanitize.text.function = function(x) x
  )
  
  # Stats for decision modelling distribution
  output$DecisionModel2 <- renderTable({
    if(is.null(data())){return()}
    else
      X <- data()
      X <- X[input$triallist, ]
    N <- length(X$TP)
    
    ## In order to specify the generalized linear model, first, we need to set up the data 
    ## Set up the data
    ## Generate 5 new variables of type long. We need these before we can reshape the data.
    # 	n1 is number diseased
    # 	n0 is number without disease
    # 	true1 is number of true positives
    # 	true0 is the number of true negatives
    # 	study is the unique identifier for each study. _n will generate a sequence of numbers. 
    
    X$n1 <- X$TP+X$FN
    X$n0 <- X$FP+X$TN
    X$true1 <- X$TP
    X$true0 <- X$TN 
    X$study <- 1:N
    
    ## Reshape the data from wide to long format ###
    Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                timevar = "sens" , times = c(1,0) , v.names = c("n","true") ) 
    
    ## Sort data by study to cluster the 2 records per study together ###
    Y = Y[order(Y$id),]  
    Y$spec<- 1-Y$sens
    
    ## Perform meta-analysis ## 
    MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                  data = Y , family = binomial , nAGQ=1 , verbose=0 ) 
    ma_Y = summary(MA_Y)
    labels( ma_Y ) 
    ma_Y$coeff 
    lsens = ma_Y$coeff[1,1]
    lspec = ma_Y$coeff[2,1]
    varlsens = ma_Y$vcov[1,1]
    varlspec = ma_Y$vcov[2,2]
    covar = ma_Y$vcov[1,2]
    Z = data.frame(estimate = c(lsens, lspec, varlsens, varlspec, covar),
                   row.names=c("logit(sens)", "logit(spec)", "Variance logit(sens)", "Variance logit(spec)", "Covariance")
    )
    
    # Create a matrix to store the parameter estimates
    s.matrix <- matrix(nrow=5, ncol=2)
    s.matrix[1,1] <- "logit(sens)"
    s.matrix[2,1] <- "logit(spec)"
    #s.matrix[3,1] <- "sigma2_logit(sens)"
    s.matrix[3,1] <- HTML("&sigma;<sup>2</sup><sub style='position: relative; left: -.5em;'>logit(sens)</sub>")
    s.matrix[4,1] <- HTML("&sigma;<sup>2</sup><sub style='position: relative; left: -.5em;'>logit(spec)</sub>")
    s.matrix[5,1] <- "covariance"
    s.matrix[1,2] <- sprintf('%4.3f', Z[1,])
    s.matrix[2,2] <- sprintf('%4.3f', Z[2,])
    s.matrix[3,2] <- sprintf('%4.3f', Z[3,])
    s.matrix[4,2] <- sprintf('%4.3f', Z[4,])
    s.matrix[5,2] <- sprintf('%4.3f', Z[5,])
    
    
    #Name the columns of the matrix
    colnames(s.matrix) <- c("Parameter", "Estimate")
    
    #Only the rows of s.matrix where statticks=T will be displayed
    s.matrix
  }, sanitize.text.function = function(x) x)
  
  
  # Allow users the option to download the parameter estimates
  output$downloadParameters2 <- downloadHandler(
    # Specify the file name 
    filename = function(){
      paste("Parameters_SA.csv")
    },
    content = function(file){
      # open the device
      # create the plot
      # close the device
      if(is.null(data())){return()}
      else
        X <- data()
        X <- X[input$triallist, ]
      N <- length(X$TP)
      
      ## In order to specify the generalized linear model, first, we need to set up the data 
      ## Set up the data
      ## Generate 5 new variables of type long. We need these before we can reshape the data.
      # 	n1 is number diseased
      # 	n0 is number without disease
      # 	true1 is number of true positives
      # 	true0 is the number of true negatives
      # 	study is the unique identifier for each study. _n will generate a sequence of numbers. 
      
      X$n1 <- X$TP+X$FN
      X$n0 <- X$FP+X$TN
      X$true1 <- X$TP
      X$true0 <- X$TN 
      X$study <- 1:N
      
      ## Reshape the data from wide to long format ###
      Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                  timevar = "sens" , times = c(1,0) , v.names = c("n","true") ) 
      
      ## Sort data by study to cluster the 2 records per study together ###
      Y = Y[order(Y$id),]  
      Y$spec<- 1-Y$sens
      
      ## Perform meta-analysis ## 
      MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                    data = Y , family = binomial , nAGQ=1 , verbose=0 ) 
      
      ma_Y = summary(MA_Y)
      labels( ma_Y ) 
      ma_Y$coeff 
      lsens = ma_Y$coeff[1,1]
      lspec = ma_Y$coeff[2,1]
      varlsens = ma_Y$vcov[1,1]
      varlspec = ma_Y$vcov[2,2]
      covar = ma_Y$vcov[1,2]
      Z = data.frame(estimate = c(lsens, lspec, varlsens, varlspec, covar),
                     row.names=c("logit(sens)", "logit(spec)", "Variance logit(sens)", "Variance logit(spec)", "Covariance")
      )
      
      # Create a matrix to store the parameter estimates
      s.matrix <- matrix(nrow=5, ncol=2)
      s.matrix[1,1] <- "logit(sens)"
      s.matrix[2,1] <- "logit(spec)"
      #s.matrix[3,1] <- "sigma2_logit(sens)"
      s.matrix[3,1] <- HTML("&sigma;<sup>2</sup><sub style='position: relative; left: -.5em;'>logit(sens)</sub>")
      s.matrix[4,1] <- HTML("&sigma;<sup>2</sup><sub style='position: relative; left: -.5em;'>logit(spec)</sub>")
      s.matrix[5,1] <- "covariance"
      s.matrix[1,2] <- sprintf('%4.3f', Z[1,])
      s.matrix[2,2] <- sprintf('%4.3f', Z[2,])
      s.matrix[3,2] <- sprintf('%4.3f', Z[3,])
      s.matrix[4,2] <- sprintf('%4.3f', Z[4,])
      s.matrix[5,2] <- sprintf('%4.3f', Z[5,])
      #Name the columns of the matrix
      colnames(s.matrix) <- c("Parameter", "Estimate")
      write.table(s.matrix, file, sep=",", row.names=FALSE)
    }#, sanitize.text.function = function(x) x
  )
  
  # RevMan parameters
  output$revman2 <- renderTable({
    if(is.null(data())){return()}
    else
      X <- data()
      X <- X[input$triallist, ]
    N <- length(X$TP)
    
    ## In order to specify the generalized linear model, first, we need to set up the data 
    ## Set up the data
    ## Generate 5 new variables of type long. We need these before we can reshape the data.
    # 	n1 is number diseased
    # 	n0 is number without disease
    # 	true1 is number of true positives
    # 	true0 is the number of true negatives
    # 	study is the unique identifier for each study. _n will generate a sequence of numbers. 
    
    X$n1 <- X$TP+X$FN
    X$n0 <- X$FP+X$TN
    X$true1 <- X$TP
    X$true0 <- X$TN 
    X$study <- 1:N
    
    ## Reshape the data from wide to long format ###
    Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                timevar = "sens" , times = c(1,0) , v.names = c("n","true") ) 
    
    ## Sort data by study to cluster the 2 records per study together ###
    Y = Y[order(Y$id),]  
    Y$spec<- 1-Y$sens
    
    ## Perform meta-analysis ## 
    MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                  data = Y , family = binomial , nAGQ=1 , verbose=0 ) 
    
    ma_Y = summary(MA_Y)
    labels( ma_Y ) 
    ma_Y$coeff 
    lsens = ma_Y$coeff[1,1]
    lspec = ma_Y$coeff[2,1]
    selsens = ma_Y$coeff[1,2]
    selspec = ma_Y$coeff[2,2]
    varlsens = ma_Y$varcor$study[1,1]
    varlspec = ma_Y$varcor$study[2,2]
    covar = ma_Y$vcov[1,2]
    
    #corr = ma_Y$varcor$study[1,2]
    corr = attributes(ma_Y$varcor$study)$correlation[2]
    Z = data.frame(estimate = c(lsens, lspec, varlsens, varlspec, covar, selsens, selspec, corr),
                   row.names=c("logit(sens)", "logit(spec)", "Variance logit(sens)", "Variance logit(spec)", "Covariance",
                               "SE(logit(sens))", "SE(logit(spec))", "Correlation")
    )
    
    # Create a matrix to store the parameter estimates
    s.matrix <- matrix(nrow=8, ncol=2)
    s.matrix[1,1] <- "E(logitSe)"
    s.matrix[2,1] <- "E(logitSp)"
    s.matrix[3,1] <- "Var(logitSe)"
    s.matrix[4,1] <- "Var(logitSp)"
    s.matrix[5,1] <- "Corr(logits)"
    s.matrix[6,1] <- "SE(E(logitSe))"
    s.matrix[7,1] <- "SE(E(logitSp))"
    s.matrix[8,1] <- "Cov(Es)"
    s.matrix[1,2] <- sprintf('%4.6f', Z[1,])
    s.matrix[2,2] <- sprintf('%4.6f', Z[2,])
    s.matrix[3,2] <- sprintf('%4.6f', Z[3,])
    s.matrix[4,2] <- sprintf('%4.6f', Z[4,])
    s.matrix[5,2] <- sprintf('%4.6f', Z[8,])
    s.matrix[6,2] <- sprintf('%4.6f', Z[6,])
    s.matrix[7,2] <- sprintf('%4.6f', Z[7,])
    s.matrix[8,2] <- sprintf('%4.6f', Z[5,])
    
    
    #Name the columns of the matrix
    colnames(s.matrix) <- c("Parameter", "Estimate")
    
    #Only the rows of s.matrix where statticks=T will be displayed
    s.matrix
  }, sanitize.text.function = function(x) x)
  
  
  # Allow users the option to download the RevMan parameters
  output$downloadRevMan2 <- downloadHandler(
    # Specify the file name 
    filename = function(){
      paste("RevMan_SA.csv")
    },
    content = function(file){
      # open the device
      # create the plot
      # close the device
      
      if(is.null(data())){return()}
      else
        X <- data()
        X <- X[input$triallist, ]
      N <- length(X$TP)
      
      ## In order to specify the generalized linear model, first, we need to set up the data 
      ## Set up the data
      ## Generate 5 new variables of type long. We need these before we can reshape the data.
      # 	n1 is number diseased
      # 	n0 is number without disease
      # 	true1 is number of true positives
      # 	true0 is the number of true negatives
      # 	study is the unique identifier for each study. _n will generate a sequence of numbers. 
      
      X$n1 <- X$TP+X$FN
      X$n0 <- X$FP+X$TN
      X$true1 <- X$TP
      X$true0 <- X$TN 
      X$study <- 1:N
      
      ## Reshape the data from wide to long format ###
      Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
                  timevar = "sens" , times = c(1,0) , v.names = c("n","true") ) 
      ## Sort data by study to cluster the 2 records per study together ###
      Y = Y[order(Y$id),]  
      Y$spec<- 1-Y$sens
      
      ## Perform meta-analysis ## 
      MA_Y = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                    data = Y , family = binomial , nAGQ=1 , verbose=0 ) 
      
      ma_Y = summary(MA_Y)
      labels( ma_Y ) 
      ma_Y$coeff 
      lsens = ma_Y$coeff[1,1]
      lspec = ma_Y$coeff[2,1]
      selsens = ma_Y$coeff[1,2]
      selspec = ma_Y$coeff[2,2]
      varlsens = ma_Y$varcor$study[1,1]
      varlspec = ma_Y$varcor$study[2,2]
      covar = ma_Y$vcov[1,2]
      
      #corr = ma_Y$varcor$study[1,2]
      corr = attributes(ma_Y$varcor$study)$correlation[2]
      Z = data.frame(estimate = c(lsens, lspec, varlsens, varlspec, covar, selsens, selspec, corr),
                     row.names=c("logit(sens)", "logit(spec)", "Variance logit(sens)", "Variance logit(spec)", "Covariance",
                                 "SE(logit(sens))", "SE(logit(spec))", "Correlation")
      )
      
      # Create a matrix to store the parameter estimates
      s.matrix <- matrix(nrow=8, ncol=2)
      s.matrix[1,1] <- "E(logitSe)"
      s.matrix[2,1] <- "E(logitSp)"
      s.matrix[3,1] <- "Var(logitSe)"
      s.matrix[4,1] <- "Var(logitSp)"
      s.matrix[5,1] <- "Corr(logits)"
      s.matrix[6,1] <- "SE(E(logitSe))"
      s.matrix[7,1] <- "SE(E(logitSp))"
      s.matrix[8,1] <- "Cov(Es)"
      s.matrix[1,2] <- sprintf('%4.6f', Z[1,])
      s.matrix[2,2] <- sprintf('%4.6f', Z[2,])
      s.matrix[3,2] <- sprintf('%4.6f', Z[3,])
      s.matrix[4,2] <- sprintf('%4.6f', Z[4,])
      s.matrix[5,2] <- sprintf('%4.6f', Z[8,])
      s.matrix[6,2] <- sprintf('%4.6f', Z[6,])
      s.matrix[7,2] <- sprintf('%4.6f', Z[7,])
      s.matrix[8,2] <- sprintf('%4.6f', Z[5,])
      
      #Name the columns of the matrix
      colnames(s.matrix) <- c("Parameter", "Estimate")
      write.table(s.matrix, file, sep=",", row.names=FALSE)
    }#, sanitize.text.function = function(x) x
  )
  
  # Produce the forest plots for sensitivity
  observe ({
    output$forestSA_sens <- renderPlot({
      if(is.null(data())){return()}
      else
        adf <- data()
      X <- adf[input$triallist, ]
      D <- madad(X, correction.control = "any")
      forest(D, type = "sens", snames = X$author, xlab = "Sensitivity", main = "Forest plot of sensitivity")
    }, height = calculate_forest_height_pixel(length(input$triallist))
    )
  })
  
  # Produce the forest plots for specificity
  observe({
    output$forestSA_spec <- renderPlot({
      if(is.null(data())){return()}
      else
        adf <- data()
      X <- adf[input$triallist, ]
      D <- madad(X, correction.control = "any")
    
      forest(D, type = "spec", snames = X$author, xlab = "Specificity", main = "Forest plot of specificity")
    }, height = calculate_forest_height_pixel(length(input$triallist))
    )
  })
  
  # Allow users to download the sensitivity forest plot
  output$download_forestSA_sens <- downloadHandler(
    # Specify the file name (either roc.png or roc.pdf)
    filename = function(){
      paste("Sensitivity Forest Plot_SA", input$filetype_forest2, sep=".")
    },
    content = function(file){
      # open the device
      # create the plot
      # close the device
      if(input$filetype_forest2 == "png")
        png(file, height = calculate_forest_height_pixel(length(input$triallist)))
      else
        pdf(file, height = calculate_forest_height_pdf(length(input$triallist)))
      
      adf <- data()
      X <- adf[input$triallist, ]
      D <- madad(X, correction.control = "any")
      forest(D, type = "sens", snames = X$author, xlab = "Sensitivity", main = "Forest plot of sensitivity")
      dev.off()
    })
  
  # Allow users to download the specificity forest plot
  output$download_forestSA_spec <- downloadHandler(
    # Specify the file name (either roc.png or roc.pdf)
    filename = function(){
      paste("Specificity Forest Plot_SA", input$filetype_forest2, sep=".")
    },
    content = function(file){
      # open the device
      # create the plot
      # close the device
      if(input$filetype_forest2 == "png")
        png(file, height = calculate_forest_height_pixel(length(input$triallist)))
      else
        pdf(file, height = calculate_forest_height_pdf(length(input$triallist)))
      
      adf <- data()
      X <- adf[input$triallist, ]
      D <- madad(X, correction.control = "any")
      forest(D, type = "spec", snames = X$author, xlab = "Specificity", main = "Forest plot of specificity")
      
      dev.off()
    })

  PrevalencePageServer(id = "prevalence", data = data)
  
  ReferencesPageServer(id = "references")
}
