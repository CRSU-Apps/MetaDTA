
#' Module UI for the analysis page.
#' 
#' @param id ID of the module
#' @return Div for the analysis page
AnalysisPageUi <- function(id) {
  ns <- NS(id)
  
  div(
    h1("Meta-Analysis of Diagnostic Test Accuracy Studies"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        uiOutput(outputId = ns("MA_input")),
        actionButton(inputId = ns("MA_reset"), label = "Reset all inputs")
      ),
      mainPanel = mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Study-level Outcomes", 
            br(), 
            p(
              "Note: If the presence of zero's for two of TP, FN, FP and TN causes sensitivity or specificity to be calculated",
              "as 0/0 than an error message will appear."
            ),
            br(),
            DT::dataTableOutput(outputId = ns("table")),
            downloadButton(outputId = ns("downloadTable"), label = "Download Table"),
            br(),
            br(),
            p("Note: Arrows to the right of the column headings can be used to sort data into ascending or descending order. "),
            p("N is  the total number of individuals in each study ( N = TP + FN + FP + TN )"),
            p(
              "Sens is the sensitivity, which is the probability of a positive test result given that",
              "the patient has the disease ( Sens = TP / [TP + FN] )"
            ),
            p(
              "Spec is the specificity, which is the probability of a negative test result given that",
              "the patient does not have the disease ( Spec = TN / [TN + FP] )"
            ),
            p("Weight_Sens is the precentage study weight of sensitvitiy, calculated using methods by Burke et al."),
            p("Weight_Spec is the percentage study weight of sensitivity, calculated using methods by Burke et al."),
            br()
          ),
          tabPanel(
            title = "SROC plot",
            h5("Note: At least one box under 'Options for SROC plot tab' must be selected to avoid an error message"),
            br(),
            textInput(
              inputId = ns("title"),
              label = h4("Plot title"),
              value = "Random Effects Meta-Analysis",
              width = '500px'
            ),
            plotOutput(outputId = ns("sroc"), width = "750px", height = "750px", click = clickOpts(id = ns("plot_click_ma"))),
            br(),
            radioButtons(
              inputId = ns("filetype"),
              label = "Select plot format",
              choices = c(
                PNG = "png",
                PDF = "pdf"
              )
            ),
            downloadButton(outputId = ns("downloadROC"), label = "Download Plot"),
            br(),
            br(),
            h5(
              "Click the middle of the data points for individual study summaries (an error message may occur if not",
              "selecting the middle of the pie chart when displaying risk of bias or acceptability concerns)"
            ),
            textOutput(outputId = ns("clickinfo_ma")),
            conditionalPanel(
              condition = "input.plot_click_ma != null",
              ns = ns,
              plotOutput(outputId = ns("piechart"))
            ),
            p(
              "Note: If quality asessment data is being used and pie charts are being plotted then study weight and",
              "covariates cannot be displayed. However, if selected on the sidebar the information will still be",
              "displayed when individual studies are clicked on."
            )
          ),
          tabPanel(
            title = "Statistics",
            br(),
            tableOutput(outputId = ns("statTable")),
            downloadButton(outputId = ns("downloadStatTable"), label = "Download Table"),
            br(),
            br()
          ),
          tabPanel(
            title = "Parameter Estimates", 
            h5(
              "Below are the parameter estimates for the bivariate normal distribution for mean sensitivity and",
              "specificty (on the logit scale). Users may find these useful for further modelling e.g. inclusion",
              "of test accuracy in a decision modelling framework."
            ),
            br(),
            img(src = "decision_modelling_distribution.png", height = 100, width = 400),
            br(),
            h5("where:"),
            tableOutput(outputId = ns("DecisionModel")),
            downloadButton(outputId = ns("downloadParameters"), label = "Download Table")
          ),
          tabPanel(
            title = "Parameters for RevMan",
            h5(
              "Below are the parameter values required by Cochrane's RevMan software to",
              "construct plots in the ROC space for users who wish to include the analysis results ",
              "as part of a Cochrane review."
            ),
            tableOutput(outputId = ns("revman")),
            downloadButton(outputId = ns("downloadRevMan"), label = "Download Table")
          ),
          tabPanel(
            title = "Forest Plots",
            fluidRow(
              splitLayout(
                cellWidths = c("50%", "50%"),
                plotOutput(outputId = ns("forestMA_sens")),
                plotOutput(outputId = ns("forestMA_spec"))
              )
            ),
            radioButtons(
              inputId = ns("filetype_forest"),
              label = "Select plot format",
              choices = c(
                PNG = "png",
                PDF = "pdf"
              )
            ),
            downloadButton(outputId = ns("download_forestMA_sens"), label = "Download Sensitivity Forest Plot"),
            downloadButton(outputId = ns("download_forestMA_spec"), label = "Download Specificity Forest Plot")
          )
        ) 
      )
    )
  )
}

#' Module server for the analysis page.
#' 
#' @param id ID of the module
#' @param data Reactive containing data frame.
AnalysisPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Display all input options for MA tab
    output$MA_input <- renderUI({
      times <- input$MA_reset
      div(
        id = letters[(times %% length(letters)) + 1],
        checkboxGroupInput(
          inputId = ns("HSROCcheck"),
          label = h4("Options for SROC plot tab"),
          choices = list(
            "Data Points" = 1,
            "SROC curve" = 2
          ),
          selected = 1
        ),
        uiOutput(outputId = ns("extrap")),
        checkboxInput(inputId = ns("prevcheck"), label = "Display disease prevalence", value = FALSE),
        checkboxInput(inputId = ns("weightcheck"), label = "Display percentage study weights", value = FALSE),
        
        checkboxGroupInput(
          inputId = ns("bivcheck"),
          label = "Bivariate model options",
          choices = c(
            "Summary point" = 1,
            "95% Confidence region" = 2,
            "95% Predictive region" = 3
          ),
          selected = c(1, 2, 3)
        ),
        checkboxGroupInput(
          inputId = ns("cicheck"),
          label = "Display 95% study level confidence intervals",
          choices = c(
            Sensitivity = 1,
            Specificity = 2
          )
        ),
        uiOutput(outputId = ns("ci_colour")),
        uiOutput(outputId = ns("QAoption")),
        uiOutput(outputId = ns("covariate")),
        uiOutput(outputId = ns("covariate_option")),
        br(),
        checkboxGroupInput(
          inputId = ns("statscheck"),
          label = h4("Options for Statistics tab"),
          choices = list(
            "Sensitivity" = 1,
            "Specificity" = 2,
            "False Positive Rate" = 3,
            "Correlation" = 4,
            "HSROC parameters" = 5,
            "Diagnostic Odds Ratio" = 6,
            "Likelihood Ratios" = 7
          ),
          selected = c(1, 2, 3)
        )
      )
    })  
    
    # Display extrapolate option only when SROC curve selected
    output$extrap <- renderUI({
      if ('2' %in% input$HSROCcheck) {
        checkboxInput(
          inputId = ns("extrapp"),
          label = "Extrapolate SROC curve (Note: In most cases extrapolation should be avoided)",
          value = FALSE
        )
      }
    })
    
    # Display option to change plotted CI colour when both covariate and quality assessment data are available
    output$ci_colour <- renderUI({
      cData <- data()
      C <- length(cData[1, ])
      Names <- colnames(cData)
      if (C > 13 & Names[13] == "ac_RS") {
        if ('1' %in% input$cicheck | '2' %in% input$cicheck |('1' %in% input$cicheck & '2' %in% input$cicheck)) {
          if (input$covcheck != 1 & (input$QAcheck == 2 |input$QAcheck == 3 | input$QAcheck == 4 | input$QAcheck == 5 |input$QAcheck == 6 | input$QAcheck == 7 | input$QAcheck == 8)){
            if (input$cov_toggle == 2 | input$cov_toggle == 3) {
              radioButtons(
                inputId = ns("ci_col"),
                label = "Choose how to colour the confidence intervals", 
                choices = c(
                  "Black" = 1,
                  "Quality assessment" = 2,
                  "Covariate" = 3
                )
              )
            }
          }
        }
      }
    })
    
    # Display option to explore quality assessment results only if the data is available
    output$QAoption <- renderUI({
      cData <- data()
      C <- length(cData[1, ])
      Names <- colnames(cData)
      if ((C == 13 & Names[7] == "rob_PS") | (C > 13 & Names[13] == "ac_RS")) {
        selectInput(
          inputId = ns("QAcheck"),
          label = "Display quality assessment scores",
          choices = c(
            "None" = 1,
            "Risk of bias: Patient selection" = 2,
            "Risk of bias: Index test" = 3,
            "Risk of bias: Reference standard" = 4,
            "Risk of bias: Flow & timing" = 5,
            "Applicability concerns: Patient selection" = 6,
            "Applicability concerns: Index test" = 7,
            "Applicability concerns: Reference standard" = 8,
            "Risk of bias (all)" = 9,
            "Applicability concerns (all)" = 10,
            "Both risk of bias and applicability concerns" = 11)
        )
      }
    })
    
    # Display option to plot covariate 
    output$covariate <- renderUI({
      cData <- data()
      C <- length(cData[1, ])
      Names <- colnames(cData)
      initial <- c("None")
      if ((C > 6 & Names[7] != "rob_PS") | (C > 13 & Names[13] == "ac_RS")){
        if (C > 6 & Names[7] != "rob_PS"){
          if (C == 7){
            covariates <- colnames(cData[7])
          } else {
            covariates <- colnames(cData[,7:C])
          }
        }
        if (C > 13 & Names[13] == "ac_RS") {
          if(C == 14){
            covariates <- colnames(cData[14])
          } else {
            covariates <- colnames(cData[,14:C])
          }
        }
        combined <- c(initial, covariates)
        number <- 1:length(combined)
        choicesCov <- setNames(number, combined)
        selectInput(inputId = ns("covcheck"), label = "Display covariates", choices = choicesCov)
      }
    })
    
    # Display buttons to control how covariate is displayed 
    output$covariate_option <- renderUI({
      cData <- data()
      C <- length(cData[1, ])
      Names <- colnames(cData)
      if ((C > 6 & Names[7] != "rob_PS") | (C > 13 & Names[13] == "ac_RS")) {
        if (input$covcheck != 1) {
          radioButtons(
            inputId = ns("cov_toggle"),
            label = "Display options for covariates", 
            choices = c(
              "Text" = 1,
              "Coloured points" = 2,
              "Both" = 3
            )
          )
        }
      }
    })
    
    # Create a table which displays sensitivity, specificity for each trial
    output$table <- DT::renderDataTable({
      if(is.null(data())){return()}
      else
        newData <- data()
      b <- study_level_outcomes(newData) # get sens, spec for each trial
      bb <- data.frame(Author=newData$author, Year=newData$year, TP=newData$TP, FN=newData$FN, FP=newData$FP, 
                       TN=newData$TN, N=b$N, Sens=b$Sens, Spec=b$Spec)
      bb$Sens <- sprintf('%4.3f', bb$Sens) # restrict number of figures after decimal place for sens
      bb$Spec <- sprintf('%4.3f', bb$Spec)
      # add information about percentage weights 
      N <- length(newData$TP)
      newData$n1 <- newData$TP+newData$FN
      newData$n0 <- newData$FP+newData$TN
      newData$true1 <- newData$TP
      newData$true0 <- newData$TN 
      newData$study <- 1:N
      Y = reshape(newData, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
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
      Y_pw = reshape(newData, direction = "long", varying = list( c("n0" , "n1") , c( "true0","true1" ) ) ,
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
    
    # Allow users the option to download the table of sens, spec for each trial
    output$downloadTable <- downloadHandler(
      # Specify the file name 
      filename = function(){
        paste("table.csv")
      },
      content = function(file){
        # open the device
        # create the plot
        # close the device
        if(is.null(data())){return()}
        else
          newData <- data()
        b <- study_level_outcomes(newData) # get sens, spec for each trial
        bb <- data.frame(Author=newData$author, Year=newData$year, TP=newData$TP, FN=newData$FN, FP=newData$FP, 
                         TN=newData$TN, N=b$N, Sens=b$Sens, Spec=b$Spec)
        bb$Sens <- sprintf('%4.3f', bb$Sens) # restrict number of figures after decimal place for sens
        bb$Spec <- sprintf('%4.3f', bb$Spec)
        # add information about percentage weights 
        N <- length(newData$TP)
        newData$n1 <- newData$TP+newData$FN
        newData$n0 <- newData$FP+newData$TN
        newData$true1 <- newData$TP
        newData$true0 <- newData$TN 
        newData$study <- 1:N
        Y = reshape(newData, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
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
        Y_pw = reshape(newData, direction = "long", varying = list( c("n0" , "n1") , c( "true0","true1" ) ) ,
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
        bb$Weight_Spec <- sprintf('%4.3f', pctsp)
        bb$Weight_Sens <- sprintf('%4.3f', pctse)
        options(DT.options = list(pageLength = 30))
        datatable(bb)
        write.table(bb, file, sep=",", row.names=FALSE)
      }
    )
    
    # Plot an interactive SROC curve 
    output$sroc <- renderPlot({
      if(is.null(data())){return()}
      else
        X <- data()
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
        if(!('1' %in% input$covcheck)){ # generates legend to include covariates
          for (i in 1:no_cov){
            if(input$covcheck == i+1){
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
        if(!('1' %in% input$covcheck)){
          for (i in 1:no_cov){
            if(input$covcheck == i+1){
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
      
      if ('1' %in% input$HSROCcheck | input$prevcheck == TRUE){
        leglabticks[LL]<-"Data" # Legend label for data points
        legendticks[LL,1]<-1}
      
      if ('3' %in% input$bivcheck){
        leglabticks[LL-1]<-"95% Predictive region" #legend label
        legendticks[LL-1,2]<-3}
      
      if ('2' %in% input$bivcheck){
        leglabticks[LL-2]<-"95% Confidence region" #legend label
        legendticks[LL-2,2]<-2} #creates interactive vector
      
      if ('1' %in% input$bivcheck){
        leglabticks[LL-3]<-"Summary estimate" #legend label
        legendticks[LL-3,1]<-15} #change plotticks and legendticks if option 1 selected
      
      if ('2' %in% input$HSROCcheck){
        leglabticks[LL-4]<-"HSROC curve"
        legendticks[LL-4,2]<-1}
      
      if (('1' %in% input$cicheck) | ('2' %in% input$cicheck)){
        leglabticks[LL-5]<-"Uncertainty"
        legendticks[LL-5,1]<-3}
      
      if((C == 13 & Names[7] == "rob_PS") |(C > 13 & Names[13] == "ac_RS")){
        if(input$QAcheck != 1){
          leglabticks[LL-6]<-"Unclear"
          leglabticks[LL-7]<-"High"
          leglabticks[LL-8]<-"Low"
          legendticks[LL-8,1]<-16
          legendticks[LL-7,1]<-16
          legendticks[LL-6,1]<-16
        }
      }
      
      if(C > 6 & Names[7] != "rob_PS"){
        if(!('1' %in% input$covcheck)){
          if('2' %in% input$cov_toggle | '3' %in% input$cov_toggle){
            for(i in 1:length(leg_covnames)){
              leglabticks[LL-5-i]<-leg_covnames[i]
              legendticks[LL-5-i,1]<-19
            }
          }
          if('1' %in% input$cov_toggle){
            leglabticks[LL]<-"Data" 
            legendticks[LL,1]<-1
          }
        }
      }
      
      if (C > 13 & Names[13] == "ac_RS"){
        if(!('1' %in% input$covcheck)){
          if('2' %in% input$cov_toggle | '3' %in% input$cov_toggle){
            for(i in 1:length(leg_covnames)){
              leglabticks[LL-8-i]<-leg_covnames[i]
              if('1' %in% input$QAcheck){
                legendticks[LL-8-i,1]<-19
              }
              else{
                legendticks[LL-8-i,1]<-0
              }
            }
          }
          if('1' %in% input$cov_toggle){
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
      title(main=input$title, xlab="False Positive Rate (1 - Specificity)", ylab="Sensitivity")
      
      # Plot study level estimates 
      if ('1' %in% input$HSROCcheck){
        if(input$weightcheck == TRUE){
          draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000)
        }
        else{
          points(study_level$FPR, study_level$Sensitivity, pch=1)
        }
      }
      
      
      # Plot prevalence 
      if(input$prevcheck == TRUE){text(study_level$FPR, study_level$Sensitivity, study_level$Prev, cex =0.7, pos = 1, offset = 0.6)}
      
      # Plots when no quality assessment data or covariate data 
      if(C == 6){ 
        # Plot sens
        if ('1' %in% input$cicheck){
          arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                 length=0.05, angle=90, code=3)
        }
        # Plot spec
        if ('2' %in% input$cicheck){
          arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                 length=0.05, angle=90, code=3)
        }
      }
      
      # Plots when quality assessment data is available but no covariate data  
      if (C == 13 & Names[7] == "rob_PS"){ 
        # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
        for (i in 2:8){
          if(input$QAcheck == i){
            for (k in 1:length(study_level$ID)){
              if(input$weightcheck == FALSE){
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
              if(input$weightcheck == TRUE){
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
        if ('9' %in% input$QAcheck){
          for (i in 1:max(P_rob$id)){
            a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
            score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
          }
        }
        # Pie charts for applicability concerns
        if ('10' %in% input$QAcheck){
          for (i in 1:max(P_ac$id)){
            a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
            score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
          }
        }
        # Pie chart that shows both applicability concerns and risk of bias
        if ('11' %in% input$QAcheck){
          for (i in 1:max(P_both$id)){
            a_pie3 <- rep(P_ac$id[i], times = 7) # pie chart evenly split
            #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
            score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
          }
        }
        
        # Add uncertainty for sens and spec at study level
        if ('1' %in% input$cicheck & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
          arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                 length=0.05, angle=90, code=3)
        }
        if ('2' %in% input$cicheck & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 | input$QAcheck == 11)){
          arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                 length=0.05, angle=90, code=3)
        }
        
        for (i in 2:8){
          if ('1' %in% input$cicheck & input$QAcheck == i){
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
          if('2' %in% input$cicheck & input$QAcheck == i){
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
          if(input$covcheck == i+1){
            if(input$weightcheck == FALSE){
              if(input$cov_toggle == 3){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
              }
              if(input$cov_toggle == 2){
                # plot covariates as coloured points
                points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
              }
              if(input$cov_toggle == 1){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
              }
            }
            if(input$weightcheck == TRUE){
              if(input$cov_toggle == 3){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
              }
              if(input$cov_toggle == 2){
                # plot covariates as coloured points
                draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
              }
              if(input$cov_toggle == 1){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
              }
            }
          }
        }
        
        # plot sensitivity and specificity
        # When no covariates are selected (sens)
        if('1' %in% input$cicheck & input$covcheck == 1){
          arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                 length=0.05, angle=90, code=3)
        }
        #When covariates are selceted as text (sens)
        if('1' %in% input$cicheck & input$covcheck != 1){
          if(input$cov_toggle == 1){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                   length=0.05, angle=90, code=3)
          }
        }
        #When no covariates are selected (spec)
        if('2' %in% input$cicheck & input$covcheck == 1){
          arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                 length=0.05, angle=90, code=3)
        }
        # When covariates are selected as text (spec)
        if('2' %in% input$cicheck & input$covcheck != 1){
          if(input$cov_toggle == 1){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                   length=0.05, angle=90, code=3)
          }
        }
        
        for(i in 1:no_cov){
          # When covariates are selected as coloured points (sens)
          if('1' %in% input$cicheck & input$covcheck == i+1){
            if(input$cov_toggle == 2 | input$cov_toggle == 3){
              arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                     length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
            }
          }
          # When covariates are selected as coloured points (spec)
          if('2' %in% input$cicheck & input$covcheck == i+1){
            if((input$cov_toggle == 2 | input$cov_toggle == 3)){
              arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                     length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
            }
          }
        }
      }
      
      # Plots when quality assessment data and covariate data is available 
      if(C > 13 & Names[13] == "ac_RS"){
        
        if(input$weightcheck == FALSE){
          # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
          for (i in 2:8){
            if(input$QAcheck == i){
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
            if(input$covcheck == i+1){
              if(input$cov_toggle == 3){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                if(input$QAcheck == 1){
                  points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
                }
                else{
                  points(study_level$FPR, study_level$Sensitivity, pch=0, col=as.factor(covariates[,i]), cex = 1.2)
                }
              }
              if(input$cov_toggle == 2){
                # plot covariates as coloured points 
                if(input$QAcheck == 1){
                  points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
                }
                else{
                  points(study_level$FPR, study_level$Sensitivity, pch=0, col=as.factor(covariates[,i]), cex =1.2)
                }
              }
              if(input$cov_toggle == 1){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
              }
            }
          }
        }  
        
        if(input$weightcheck == TRUE){
          # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
          for (i in 2:8){
            if(input$QAcheck == i){
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
            if(input$covcheck == i+1){
              if(input$cov_toggle == 3){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                if(input$QAcheck == 1){
                  points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
                  
                }
                else{
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = as.factor(covariates[,i]))
                  
                }
              }
              if(input$cov_toggle == 2){
                # plot covariates as coloured points 
                if(input$QAcheck == 1){
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
                }
                else{
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = as.factor(covariates[,i]))
                }
              }
              if(input$cov_toggle == 1){
                # plot covariates as text and coloured points
                text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
              }
            }
          }
        }
        
        
        
        # Pie charts to represent study level esitmates split into risk of bias and applicability concerns 
        # Pie charts for risk of bias
        if ('9' %in% input$QAcheck){
          for (i in 1:max(P_rob$id)){
            a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
            score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
          }
        }
        # Pie charts for applicability concerns
        if ('10' %in% input$QAcheck){
          for (i in 1:max(P_ac$id)){
            a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
            score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
          }
        }
        # Pie chart that shows both applicability concerns and risk of bias
        if ('11' %in% input$QAcheck){
          for (i in 1:max(P_both$id)){
            a_pie3 <- rep(P_ac$id[i], times = 7) # pie chart evenly split
            #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
            score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
            pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                     col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
          }
        }
        
        if('1' %in% input$cicheck){
          # plot sensitvity when no covariates selected no quality assesment (except piercharts)
          if(input$covcheck == 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                   length=0.05, angle=90, code=3)
          }
          # plot sensitivity when covariates only dispalyed as text no quality assesment (except piercharts)
          if(input$covcheck != 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
            if(input$cov_toggle == 1){
              arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                     length=0.05, angle=90, code=3)
            }
          }
          # Plot sensitivity when quality assessment only and no covariates
          for (i in 2:8){
            if(input$covcheck == 1 & input$QAcheck == i){
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
            if(input$covcheck != 1 & input$QAcheck == i){
              if(input$cov_toggle == 1){
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
            if((input$QAcheck == 1 |input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11) & input$covcheck == i+1){
              if(input$cov_toggle == 2 | input$cov_toggle == 3){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                       length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
              }
            }
          }
          
          # Plot sensitivity when covariates and quality assessment both selected as colours
          if(input$covcheck != 1 & (input$QAcheck == 2 |input$QAcheck == 3 | input$QAcheck == 4 |input$QAcheck == 5 | input$QAcheck == 6 |input$QAcheck == 7 | input$QAcheck == 8)){
            if(input$cov_toggle == 2 | input$cov_toggle == 3){
              # Plot default black
              if(input$ci_col == 1){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length =0.05, angle = 90, code =3)
              }
              # Plot colour as quality assessment 
              if(input$ci_col == 2){
                for(i in 2:8){
                  if (input$QAcheck == i){
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
              if(input$ci_col == 3){
                for(i in 1:no_cov){
                  if(input$covcheck == i+1){
                    arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                           length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
                  }
                }
              }
            }
          }
        }
        
        if('2' %in% input$cicheck){
          # plot specificity when no covariates selected no quality assessment (except pie charts)
          if(input$covcheck == 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                   length=0.05, angle=90, code=3)
          }
          # plot specificity when covariates only displayed as text no quality assessment (except pie charts)
          if(input$covcheck != 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
            if(input$cov_toggle == 1){
              arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                     length=0.05, angle=90, code=3)
            }
          }
          # plot specificity when quality assessment only and no covariates 
          for (i in 2:8){
            if(input$covcheck == 1 & input$QAcheck == i){
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
            if(input$covcheck != 1 & input$QAcheck == i){
              if(input$cov_toggle == 1){
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
            if((input$QAcheck == 1 |input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11) & input$covcheck == i+1){
              if(input$cov_toggle == 2 | input$cov_toggle == 3){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                       length=0.05, angle=90, code=3, col= as.factor(covariates[,i]))
              }
            }
          }
          # plot specificity when covariates and quality assessment both selected as colours 
          if(input$covcheck != 1 & (input$QAcheck == 2 |input$QAcheck == 3 | input$QAcheck == 4 |input$QAcheck == 5 | input$QAcheck == 6 |input$QAcheck == 7 | input$QAcheck == 8)){
            if(input$cov_toggle == 2 | input$cov_toggle == 3){
              # Plot default black
              if(input$ci_col == 1){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                       length=0.05, angle=90, code=3)
              }
              # Plot colour as quality assessment 
              if(input$ci_col == 2){
                for(i in 2:8){
                  if (input$QAcheck == i){
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
              if(input$ci_col == 3){
                for(i in 1:no_cov){
                  if(input$covcheck == i+1){
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
      if ('2' %in% input$HSROCcheck){
        if (input$extrapp==T){points(roc_points, type="l")}
        else points(roc_points2, type="l", ann=F)}
      
      # Add summary point
      if ('1' %in% input$bivcheck){points(mean_point, col="blue", pch=15)}
      
      # Add confidence region
      if ('2' %in% input$bivcheck){lines(conf_region, lty=2, col="blue")}
      if ('3' %in% input$bivcheck){lines(pred_region, lty=3, col="blue")}
      
      # Add the legend 
      legend("bottomright", bty ="n", leglabticks, pch = legendticks[,1], lty = legendticks[,2], lwd=leg_lwd, col = leg_col)
      
    })
    
    # Allow users to download the interactive SROC curve
    output$downloadROC <- downloadHandler(
      # Specify the file name (either roc.png or roc.pdf)
      filename = function(){
        paste("roc", input$filetype, sep=".")
      },
      content = function(file){
        # open the device
        # create the plot
        # close the device
        if(input$filetype == "png")
          png(file, width = 750, height = 750)
        else
          pdf(file, width = 8.5, height = 8.5)
        
        X <- data()
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
          if(!('1' %in% input$covcheck)){ # generates legend to include covariates
            for (i in 1:no_cov){
              if(input$covcheck == i+1){
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
          if(!('1' %in% input$covcheck)){
            for (i in 1:no_cov){
              if(input$covcheck == i+1){
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
        
        if ('1' %in% input$HSROCcheck | input$prevcheck == TRUE){
          leglabticks[LL]<-"Data" # Legend label for data points
          legendticks[LL,1]<-1}
        
        if ('3' %in% input$bivcheck){
          leglabticks[LL-1]<-"95% Predictive region" #legend label
          legendticks[LL-1,2]<-3}
        
        if ('2' %in% input$bivcheck){
          leglabticks[LL-2]<-"95% Confidence region" #legend label
          legendticks[LL-2,2]<-2} #creates interactive vector
        
        if ('1' %in% input$bivcheck){
          leglabticks[LL-3]<-"Summary estimate" #legend label
          legendticks[LL-3,1]<-15} #change plotticks and legendticks if option 1 selected
        
        if ('2' %in% input$HSROCcheck){
          leglabticks[LL-4]<-"HSROC curve"
          legendticks[LL-4,2]<-1}
        
        if (('1' %in% input$cicheck) | ('2' %in% input$cicheck)){
          leglabticks[LL-5]<-"Uncertainty"
          legendticks[LL-5,1]<-3}
        
        if((C == 13 & Names[7] == "rob_PS") |(C > 13 & Names[13] == "ac_RS")){
          if(input$QAcheck != 1){
            leglabticks[LL-6]<-"Unclear"
            leglabticks[LL-7]<-"High"
            leglabticks[LL-8]<-"Low"
            legendticks[LL-8,1]<-16
            legendticks[LL-7,1]<-16
            legendticks[LL-6,1]<-16
          }
        }
        
        if(C > 6 & Names[7] != "rob_PS"){
          if(!('1' %in% input$covcheck)){
            if('2' %in% input$cov_toggle | '3' %in% input$cov_toggle){
              for(i in 1:length(leg_covnames)){
                leglabticks[LL-5-i]<-leg_covnames[i]
                legendticks[LL-5-i,1]<-19
              }
            }
            if('1' %in% input$cov_toggle){
              leglabticks[LL]<-"Data" 
              legendticks[LL,1]<-1
            }
          }
        }
        
        if (C > 13 & Names[13] == "ac_RS"){
          if(!('1' %in% input$covcheck)){
            if('2' %in% input$cov_toggle | '3' %in% input$cov_toggle){
              for(i in 1:length(leg_covnames)){
                leglabticks[LL-8-i]<-leg_covnames[i]
                if('1' %in% input$QAcheck){
                  legendticks[LL-8-i,1]<-19
                }
                else{
                  legendticks[LL-8-i,1]<-0
                }
              }
            }
            if('1' %in% input$cov_toggle){
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
        title(main=input$title, xlab="False Positive Rate (1 - Specificity)", ylab="Sensitivity")
        
        # Plot study level estimates 
        if ('1' %in% input$HSROCcheck){
          if(input$weightcheck == TRUE){
            draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000)
          }
          else{
            points(study_level$FPR, study_level$Sensitivity, pch=1)
          }
        }
        
        
        # Plot prevalence 
        if(input$prevcheck == TRUE){text(study_level$FPR, study_level$Sensitivity, study_level$Prev, cex =0.7, pos = 1)}
        
        # Plots when no quality assessment data or covariate data 
        if(C == 6){ 
          # Plot sens
          if ('1' %in% input$cicheck){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                   length=0.05, angle=90, code=3)
          }
          # Plot spec
          if ('2' %in% input$cicheck){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                   length=0.05, angle=90, code=3)
          }
        }
        
        # Plots when quality assessment data is available but no covariate data  
        if (C == 13 & Names[7] == "rob_PS"){ 
          # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
          for (i in 2:8){
            if(input$QAcheck == i){
              for (k in 1:length(study_level$ID)){
                if(input$weightcheck == FALSE){
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
                if(input$weightcheck == TRUE){
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
          if ('9' %in% input$QAcheck){
            for (i in 1:max(P_rob$id)){
              a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
              score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
          # Pie charts for applicability concerns
          if ('10' %in% input$QAcheck){
            for (i in 1:max(P_ac$id)){
              a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
              score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
          # Pie chart that shows both applicability concerns and risk of bias
          if ('11' %in% input$QAcheck){
            for (i in 1:max(P_both$id)){
              a_pie3 <- rep(P_ac$id[i], times = 7) # pie chart evenly split
              #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
              score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
          
          # Add uncertainty for sens and spec at study level
          if ('1' %in% input$cicheck & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                   length=0.05, angle=90, code=3)
          }
          if ('2' %in% input$cicheck & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 | input$QAcheck == 11)){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                   length=0.05, angle=90, code=3)
          }
          
          for (i in 2:8){
            if ('1' %in% input$cicheck & input$QAcheck == i){
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
            if('2' %in% input$cicheck & input$QAcheck == i){
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
            if(input$covcheck == i+1){
              if(input$weightcheck == FALSE){
                if(input$cov_toggle == 3){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                  points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
                }
                if(input$cov_toggle == 2){
                  # plot covariates as coloured points
                  points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
                }
                if(input$cov_toggle == 1){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                }
              }
              if(input$weightcheck == TRUE){
                if(input$cov_toggle == 3){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
                }
                if(input$cov_toggle == 2){
                  # plot covariates as coloured points
                  draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
                }
                if(input$cov_toggle == 1){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                }
              }
            }
          }
          
          # plot sensitivity and specificity
          # When no covariates are selected (sens)
          if('1' %in% input$cicheck & input$covcheck == 1){
            arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                   length=0.05, angle=90, code=3)
          }
          #When covariates are selceted as text (sens)
          if('1' %in% input$cicheck & input$covcheck != 1){
            if(input$cov_toggle == 1){
              arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                     length=0.05, angle=90, code=3)
            }
          }
          #When no covariates are selected (spec)
          if('2' %in% input$cicheck & input$covcheck == 1){
            arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                   length=0.05, angle=90, code=3)
          }
          # When covariates are selected as text (spec)
          if('2' %in% input$cicheck & input$covcheck != 1){
            if(input$cov_toggle == 1){
              arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                     length=0.05, angle=90, code=3)
            }
          }
          
          for(i in 1:no_cov){
            # When covariates are selected as coloured points (sens)
            if('1' %in% input$cicheck & input$covcheck == i+1){
              if(input$cov_toggle == 2 | input$cov_toggle == 3){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                       length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
              }
            }
            # When covariates are selected as coloured points (spec)
            if('2' %in% input$cicheck & input$covcheck == i+1){
              if((input$cov_toggle == 2 | input$cov_toggle == 3)){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                       length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
              }
            }
          }
        }
        
        # Plots when quality assessment data and covariate data is available 
        if(C > 13 & Names[13] == "ac_RS"){
          
          if(input$weightcheck == FALSE){
            # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
            for (i in 2:8){
              if(input$QAcheck == i){
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
              if(input$covcheck == i+1){
                if(input$cov_toggle == 3){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                  if(input$QAcheck == 1){
                    points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
                  }
                  else{
                    points(study_level$FPR, study_level$Sensitivity, pch=0, col=as.factor(covariates[,i]), cex = 1.2)
                  }
                }
                if(input$cov_toggle == 2){
                  # plot covariates as coloured points 
                  if(input$QAcheck == 1){
                    points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
                  }
                  else{
                    points(study_level$FPR, study_level$Sensitivity, pch=0, col=as.factor(covariates[,i]), cex =1.2)
                  }
                }
                if(input$cov_toggle == 1){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                }
              }
            }
          }  
          
          if(input$weightcheck == TRUE){
            # Coloured study level estimates based on 1 of the 7 outcomes of quality assessment 
            for (i in 2:8){
              if(input$QAcheck == i){
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
              if(input$covcheck == i+1){
                if(input$cov_toggle == 3){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                  if(input$QAcheck == 1){
                    points(study_level$FPR, study_level$Sensitivity, pch=19, col=as.factor(covariates[,i]))
                    draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
                    
                  }
                  else{
                    draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = as.factor(covariates[,i]))
                    
                  }
                }
                if(input$cov_toggle == 2){
                  # plot covariates as coloured points 
                  if(input$QAcheck == 1){
                    draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, col = as.factor(covariates[,i]))
                  }
                  else{
                    draw.ellipse(study_level$FPR, study_level$Sensitivity, a=pctsp/1000, b=pctse/1000, border = as.factor(covariates[,i]))
                  }
                }
                if(input$cov_toggle == 1){
                  # plot covariates as text and coloured points
                  text(study_level$FPR, study_level$Sensitivity, covariates[,i], cex =0.7, pos = 2)
                }
              }
            }
          }
          
          
          
          # Pie charts to represent study level esitmates split into risk of bias and applicability concerns 
          # Pie charts for risk of bias
          if ('9' %in% input$QAcheck){
            for (i in 1:max(P_rob$id)){
              a_pie1 <- rep(P_rob$id[i], times = 4) # generates a vector of equal values, so pie chart is evenly split
              score_pie1 <- P_rob$score[seq(i, length(P_rob$id), max(P_rob$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie1, P_rob$fpr[i], P_rob$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie1 == 1, "green", ifelse(score_pie1 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
          # Pie charts for applicability concerns
          if ('10' %in% input$QAcheck){
            for (i in 1:max(P_ac$id)){
              a_pie2 <- rep(P_ac$id[i], times = 3) # generates a vector of equal values, so pie chart is evenly split
              score_pie2 <- P_ac$score[seq(i, length(P_ac$id), max(P_ac$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie2, P_ac$fpr[i], P_ac$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie2 == 1, "green", ifelse(score_pie2 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
          # Pie chart that shows both applicability concerns and risk of bias
          if ('11' %in% input$QAcheck){
            for (i in 1:max(P_both$id)){
              a_pie3 <- rep(P_ac$id[i], times = 7) # pie chart evenly split
              #a_pie3 <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # split circle in half then seperate
              score_pie3 <- P_both$score[seq(i, length(P_both$id), max(P_both$id))] # extracts the results from quality assessment to determine colour
              pieGlyph(a_pie3, P_both$fpr[i], P_both$sens[i], labels = NA, radius =0.015,
                       col = ifelse(score_pie3 == 1, "green", ifelse(score_pie3 == 2, "red", "lavenderblush3"))) # plots pie charts
            }
          }
          
          if('1' %in% input$cicheck){
            # plot sensitvity when no covariates selected no quality assesment (except piercharts)
            if(input$covcheck == 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
              arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                     length=0.05, angle=90, code=3)
            }
            # plot sensitivity when covariates only dispalyed as text no quality assesment (except piercharts)
            if(input$covcheck != 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
              if(input$cov_toggle == 1){
                arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                       length=0.05, angle=90, code=3)
              }
            }
            # Plot sensitivity when quality assessment only and no covariates
            for (i in 2:8){
              if(input$covcheck == 1 & input$QAcheck == i){
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
              if(input$covcheck != 1 & input$QAcheck == i){
                if(input$cov_toggle == 1){
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
              if((input$QAcheck == 1 |input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11) & input$covcheck == i+1){
                if(input$cov_toggle == 2 | input$cov_toggle == 3){
                  arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                         length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
                }
              }
            }
            
            # Plot sensitivity when covariates and quality assessment both selected as colours
            if(input$covcheck != 1 & (input$QAcheck == 2 |input$QAcheck == 3 | input$QAcheck == 4 |input$QAcheck == 5 | input$QAcheck == 6 |input$QAcheck == 7 | input$QAcheck == 8)){
              if(input$cov_toggle == 2 | input$cov_toggle == 3){
                # Plot default black
                if(input$ci_col == 1){
                  arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI, length =0.05, angle = 90, code =3)
                }
                # Plot colour as quality assessment 
                if(input$ci_col == 2){
                  for(i in 2:8){
                    if (input$QAcheck == i){
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
                if(input$ci_col == 3){
                  for(i in 1:no_cov){
                    if(input$covcheck == i+1){
                      arrows(study_level$FPR, study_level$Sens_LCI, study_level$FPR, study_level$Sens_UCI,
                             length=0.05, angle=90, code=3, col = as.factor(covariates[,i]))
                    }
                  }
                }
              }
            }
          }
          
          if('2' %in% input$cicheck){
            # plot specificity when no covariates selected no quality assessment (except pie charts)
            if(input$covcheck == 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
              arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                     length=0.05, angle=90, code=3)
            }
            # plot specificity when covariates only displayed as text no quality assessment (except pie charts)
            if(input$covcheck != 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)){
              if(input$cov_toggle == 1){
                arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                       length=0.05, angle=90, code=3)
              }
            }
            # plot specificity when quality assessment only and no covariates 
            for (i in 2:8){
              if(input$covcheck == 1 & input$QAcheck == i){
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
              if(input$covcheck != 1 & input$QAcheck == i){
                if(input$cov_toggle == 1){
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
              if((input$QAcheck == 1 |input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11) & input$covcheck == i+1){
                if(input$cov_toggle == 2 | input$cov_toggle == 3){
                  arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                         length=0.05, angle=90, code=3, col= as.factor(covariates[,i]))
                }
              }
            }
            # plot specificity when covariates and quality assessment both selected as colours 
            if(input$covcheck != 1 & (input$QAcheck == 2 |input$QAcheck == 3 | input$QAcheck == 4 |input$QAcheck == 5 | input$QAcheck == 6 |input$QAcheck == 7 | input$QAcheck == 8)){
              if(input$cov_toggle == 2 | input$cov_toggle == 3){
                # Plot default black
                if(input$ci_col == 1){
                  arrows(study_level$FPR_LCI, study_level$Sensitivity, study_level$FPR_UCI, study_level$Sensitivity,
                         length=0.05, angle=90, code=3)
                }
                # Plot colour as quality assessment 
                if(input$ci_col == 2){
                  for(i in 2:8){
                    if (input$QAcheck == i){
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
                if(input$ci_col == 3){
                  for(i in 1:no_cov){
                    if(input$covcheck == i+1){
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
        if ('2' %in% input$HSROCcheck){
          if (input$extrapp==T){points(roc_points, type="l")}
          else points(roc_points2, type="l", ann=F)}
        
        # Add summary point
        if ('1' %in% input$bivcheck){points(mean_point, col="blue", pch=15)}
        
        # Add confidence region
        if ('2' %in% input$bivcheck){lines(conf_region, lty=2, col="blue")}
        if ('3' %in% input$bivcheck){lines(pred_region, lty=3, col="blue")}
        
        # Add the legend 
        legend("bottomright", bty ="n", leglabticks, pch = legendticks[,1], lty = legendticks[,2], lwd=leg_lwd, col = leg_col)
        dev.off()
      }
    )
    
    # Plot of individual pie chart if clicked on
    output$piechart <- renderPlot({
      if(is.null(data())){return()}
      else
        PData <- data()
      e <- study_level_outcomes(PData)
      if ('9' %in% input$QAcheck){
        ee <- data.frame(Author=PData$author,Year=PData$year, TP=PData$TP, FN=PData$FN, FP=PData$FP, 
                         TN=PData$TN, Sensitivity=e$Sensitivity, Specificity=e$Specificity, 
                         FPR=e$FPR, rob_PS=PData$rob_PS, rob_IT=PData$rob_IT, rob_RS=PData$rob_RS, rob_FT=PData$rob_FT )
        clickselect_pc <- nearPoints(ee, input$plot_click_ma, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1)
        clickselect_pc <- reshape(clickselect_pc, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"), 
                                  v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
        labels_rob <- c("Patient selection", "Index test", "Reference standard", "Flow & timing")
        weight <-c(1,1,1,1)
        pie(weight, labels_rob, main = "Risk of bias for each domain from the selected study",
            col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "lavenderblush3")))
        legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "lavenderblush3"))
      }
      if ('10' %in% input$QAcheck){
        ee <- data.frame(Author=PData$author,Year=PData$year, TP=PData$TP, FN=PData$FN, FP=PData$FP, 
                         TN=PData$TN, Sensitivity=e$Sensitivity, Specificity=e$Specificity, 
                         FPR=e$FPR, ac_PS=PData$ac_PS, ac_IT=PData$ac_IT, ac_RS=PData$ac_RS)
        clickselect_pc <- nearPoints(ee, input$plot_click_ma, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1)
        clickselect_pc <- reshape(clickselect_pc, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"), 
                                  v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
        labels_ac <- c("Patient selection", "Index test", "Reference standard")
        weight <-c(1,1,1)
        pie(weight, labels_ac, main = "Applicability concern for each domain from the selected study",
            col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "lavenderblush3")))
        legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "lavenderblush3"))
      }
      if ('11' %in% input$QAcheck){
        ee <- data.frame(Author=PData$author,Year=PData$year, TP=PData$TP, FN=PData$FN, FP=PData$FP, 
                         TN=PData$TN, Sensitivity=e$Sensitivity, Specificity=e$Specificity, 
                         FPR=e$FPR, rob_PS=PData$rob_PS, rob_IT=PData$rob_IT, rob_RS=PData$rob_RS, rob_FT=PData$rob_FT,
                         ac_PS=PData$ac_PS, ac_IT=PData$ac_IT, ac_RS=PData$ac_RS)
        clickselect_pc <- nearPoints(ee, input$plot_click_ma, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1)
        clickselect_pc <-reshape(clickselect_pc, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                                 v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
        labels_both <- c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS")
        #weight <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # weight for pie chart split in half
        weight <- c(1,1,1,1,1,1,1) # weight where each section is equally split
        pie(weight, labels_both, main = "Scores from each element of the QUADAS-2 tool",
            col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "lavenderblush3")))
        legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "lavenderblush3"))
      } else {
        return()
      }
      
    })
    
    
    # Text displaying details about studies if clicked on in the plot
    output$clickinfo_ma <- renderText({
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
          covariates <- cbind(as.character(cData[,7]))
          no_cov <- 1 
        }
        else {
          covariates <- cData[,7:C]
          no_cov <- length(colnames(covariates))
        }
        #covariates <- cData[,7:C]
        #no_cov <- length(colnames(covariates)) # number of covariates
        for (i in 1:no_cov){
          if(input$covcheck == i+1){
            bb <- cbind(bb, covariates[,i]) # adds covariate to dataset
            names(bb)[length(names(bb))]<-"Cov" # renames covariate column as its last in dataset
          }
        }
      }
      if(C> 13 & Names[13] == "ac_RS"){
        if (C == 14){
          covariates <- cbind(as.character(cData[,14]))
          no_cov <- 1
        }
        else {
          covariates <- cData[,14:C]
          no_cov <- length(colnames(covariates))
        }
        #covariates <- cData[,14:C]
        #no_cov <- length(colnames(covariates)) # number of covariates
        for (i in 1:no_cov){
          if(input$covcheck == i+1){
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
      hoverselect <- nearPoints(bb, input$plot_click_ma, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1) #takes the row from the dataframe that matches the hover point (only allowed one)
      
      if((C == 6) | (C == 13 & Names[7] == "rob_PS")){  
        if (input$prevcheck == TRUE & input$weightcheck == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev, hoverselect$pct_sens, hoverselect$pct_spec))
        }
        else if (input$prevcheck == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev))
        }
        else if (input$weightcheck == TRUE){
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
        if(input$prevcheck == TRUE & input$weightcheck == TRUE & input$covcheck != 1){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f, Covariate value = %10s ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev, hoverselect$pct_sens, hoverselect$pct_spec, hoverselect$Cov))
        }
        else if (input$prevcheck == TRUE & input$covcheck != 1){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f, Covariate value = %10s", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev, hoverselect$Cov))
        }
        else if (input$prevcheck == TRUE & input$weightcheck == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev, hoverselect$pct_sens, hoverselect$pct_spec))
        }
        else if (input$weightcheck == TRUE & input$covcheck != 1){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f, Covariate value = %10s ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$pct_sens, hoverselect$pct_spec, hoverselect$Cov))
        }
        else if (input$prevcheck == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Prev = %4.3f", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Prev))
        }
        else if (input$weightcheck == TRUE){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Sensitivity weight = %4.3f, Specificity weight = %4.3f ", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$pct_sens, hoverselect$pct_spec))
        }
        else if (input$covcheck != 1){
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f, Covariate value = %10s", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity, hoverselect$Cov)) 
        }
        else{
          print(sprintf("%10s (%4.0f) : Sens. = %4.3f, Spec. = %4.3f", 
                        hoverselect$Author, hoverselect$Year, hoverselect$Sensitivity, hoverselect$Specificity))
        }
      }
    })
    
    
    # Table of MA statistics - overall sens, spec etc
    output$statTable <- renderTable({
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
      if ('1' %in% input$statscheck) {statticks[1] <- T} 
      if ('2' %in% input$statscheck) {statticks[2] <- T} 
      if ('3' %in% input$statscheck) {statticks[3] <- T} 
      if ('4' %in% input$statscheck) {statticks[4] <- T} 
      if ('5' %in% input$statscheck) {statticks[5:9] <- T} 
      if ('6' %in% input$statscheck) {statticks[10] <- T} 
      if ('7' %in% input$statscheck) {statticks[11:12] <- T} 
      if ('1' %in% input$statscheck) {statticks[13] <- T} 
      if ('2' %in% input$statscheck) {statticks[14] <- T} 
      
      #Only the rows of s.matrix where statticks=T will be displayed
      s.matrix[statticks,]
    }, sanitize.text.function = function(x) x)
    
    # Allow users the option to download the table of statistics
    output$downloadStatTable <- downloadHandler(
      # Specify the file name 
      filename = function(){
        paste("statTable.csv")
      },
      content = function(file){
        # open the device
        # create the plot
        # close the device
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
        
        
        #Obtain confidence intervals as numeric
        Z_lci <- as.numeric(levels(Z$lci)[Z$lci])
        Z_uci <- as.numeric(levels(Z$uci)[Z$uci])
        
        # Create a matrix to store the parameter estimates
        s.matrix <- matrix(nrow=15, ncol=4)
        s.matrix[1,1] <- "Sensitivity"
        s.matrix[2,1] <- "Specificity"
        s.matrix[3,1] <- "False Positive Rate"
        s.matrix[4,1] <- "Random Effects Correlation"
        s.matrix[5,1] <- "theta"
        s.matrix[6,1] <- "lambda"
        s.matrix[7,1] <- "beta"
        s.matrix[8,1] <- "sigma_theta"
        s.matrix[9,1] <- "sigma_alpha"
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
        if ('1' %in% input$statscheck) {statticks[1] <- T} 
        if ('2' %in% input$statscheck) {statticks[2] <- T} 
        if ('3' %in% input$statscheck) {statticks[3] <- T} 
        if ('4' %in% input$statscheck) {statticks[4] <- T} 
        if ('5' %in% input$statscheck) {statticks[5:9] <- T} 
        if ('6' %in% input$statscheck) {statticks[10] <- T} 
        if ('7' %in% input$statscheck) {statticks[11:12] <- T} 
        if ('1' %in% input$statscheck) {statticks[13] <- T} 
        if ('2' %in% input$statscheck) {statticks[14] <- T} 
        
        #Only the rows of s.matrix where statticks=T will be displayed
        write.table(s.matrix[statticks,], file, sep=",", row.names=FALSE)
        
        
      }#, sanitize.text.function = function(x) x 
    )
    
    
    # Stats for decision modelling distribution
    output$DecisionModel <- renderTable({
      if(is.null(data())){return()}
      else
        X <- data()
      
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
    output$downloadParameters <- downloadHandler(
      # Specify the file name 
      filename = function(){
        paste("Parameters.csv")
      },
      content = function(file){
        # open the device
        # create the plot
        # close the device
        if(is.null(data())){return()}
        else
          X <- data()
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
    output$revman <- renderTable({
      if(is.null(data())){return()}
      else
        X <- data()
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
    output$downloadRevMan <- downloadHandler(
      # Specify the file name 
      filename = function(){
        paste("RevMan.csv")
      },
      content = function(file){
        # open the device
        # create the plot
        # close the device
        
        if(is.null(data())){return()}
        else
          X <- data()
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
    observe({
      output$forestMA_sens <- renderPlot({
        if(is.null(data())){return()}
        else
          X <- data()
        D <- madad(X, correction.control = "any")
        
        fplot <- forest(D, type = "sens", snames = X$author,
                        xlab = "Sensitivity", main = "Forest plot of sensitivity")
        
      }, height = calculate_forest_height_pixel(nrow(data()))
      )
    })
    
    # Produce the forest plots for specificity
    observe({
      output$forestMA_spec <- renderPlot({
        if(is.null(data())){return()}
        else
          X <- data()
        D <- madad(X, correction.control = "any")
        forest(D, type = "spec", snames = X$author, xlab = "Specificity", main = "Forest plot of specificity")
      }, height = calculate_forest_height_pixel(nrow(data()))
      )
    })
    
    # Allow users to download the sensitivity forest plot
    output$download_forestMA_sens <- downloadHandler(
      # Specify the file name (either roc.png or roc.pdf)
      filename = function(){
        paste("Sensitivity Forest Plot", input$filetype_forest, sep=".")
      },
      content = function(file){
        # open the device
        # create the plot
        # close the device
        if(input$filetype_forest == "png")
          png(file = file, height = calculate_forest_height_pixel(nrow(data())))
        else
          pdf(file, height = calculate_forest_height_pdf(nrow(data())))
        
        X <- data()
        D <- madad(X, correction.control = "any")
        forest(D, type = "sens", snames = X$author, xlab = "Sensitivity", main = "Forest plot of sensitivity")
        
        dev.off()
      })
    
    # Allow users to download the specificity forest plot
    output$download_forestMA_spec <- downloadHandler(
      # Specify the file name (either roc.png or roc.pdf)
      filename = function(){
        paste("Specificity Forest Plot", input$filetype_forest, sep=".")
      },
      content = function(file){
        # open the device
        # create the plot
        # close the device
        if(input$filetype_forest == "png")
          png(file, height = calculate_forest_height_pixel(nrow(data())))
        else
          pdf(file, height = calculate_forest_height_pdf(nrow(data())))
        
        X <- data()
        D <- madad(X, correction.control = "any")
        forest(D, type = "spec", snames = X$author, xlab = "Specificity", main = "Forest plot of specificity")
        
        dev.off()
      })
  })
}
