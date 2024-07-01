
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
              "Note: If the presence of zeros for two of TP, FN, FP and TN causes sensitivity or specificity to be calculated",
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
            conditionalPanel(
              condition = "output.converged",
              ns = ns,
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
            conditionalPanel(
              condition = "!output.converged",
              ns = ns,
              non_convergence_warning_ui()
            )
          ),
          tabPanel(
            title = "Statistics",
            conditionalPanel(
              condition = "output.converged",
              ns = ns,
              br(),
              tableOutput(outputId = ns("statTable")),
              downloadButton(outputId = ns("downloadStatTable"), label = "Download Table"),
              br(),
              br()
              ),
            conditionalPanel(
              condition = "!output.converged",
              ns = ns,
              non_convergence_warning_ui()
            )
            ),
          tabPanel(
            title = "Parameter Estimates", 
            conditionalPanel(
              condition = "output.converged",
              ns = ns,
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
            conditionalPanel(
              condition = "!output.converged",
              ns = ns,
              non_convergence_warning_ui()
            )
          ),
          tabPanel(
            title = "Parameters for RevMan",
            conditionalPanel(
              condition = "output.converged",
              ns = ns,
              h5(
                "Below are the parameter values required by Cochrane's RevMan software to",
                "construct plots in the ROC space for users who wish to include the analysis results ",
                "as part of a Cochrane review."
              ),
              tableOutput(outputId = ns("revman")),
              downloadButton(outputId = ns("downloadRevMan"), label = "Download Table")
            ),
            conditionalPanel(
              condition = "!output.converged",
              ns = ns,
              non_convergence_warning_ui()
            )
          ),
          tabPanel(
            title = "Forest Plots",
            conditionalPanel(
              condition = "output.converged",
              ns = ns,
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
            ),
            conditionalPanel(
              condition = "!output.converged",
              ns = ns,
              non_convergence_warning_ui()
            )
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
    
    #Run the model in the server (i.e. not inside an output)
    model_output <- reactive({
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
      MA_Y <- glmer(formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
                    data = Y , family = binomial , nAGQ=1 , verbose=0 )
      return(MA_Y)
    })
    
    #Model summary
    model_summary <- reactive(summary(model_output()))
    
    #Check if the model converged
    converged <- reactive(is.null(model_summary()$optinfo$conv$lme4$messages))
    
    #Add converged() reactive to the output list
    output$converged <- reactive(converged())
    outputOptions(x = output, name = "converged", suspendWhenHidden = FALSE)
    
    #If the model did not converge, display a warning
    observe({
      if (!converged()) {
        shinyalert(
          title = "WARNING - model failed to converge",
          text = paste0("Most output will not be displayed. </br> </br> MetaDTA is not equipped for model diagnostics. You are advised to use MetaBayesDTA ", tags$a("https://www.gla.ac.uk/research/az/crsu/apps-materials-guidence/", href="https://www.gla.ac.uk/research/az/crsu/apps-materials-guidence/", target="_blank"), " or other software. </br> </br> Error message: ", model_summary()$optinfo$conv$lme4$messages),
          size = "l",
          html = TRUE
        )
      }
    })
    
    # Create a table which displays sensitivity, specificity for each trial
    sens_spec_table <- reactive({
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
      return(bb)
    })
    
    output$table <- DT::renderDataTable({
      datatable(sens_spec_table())
    })
    
    # Allow users the option to download the table of sens, spec for each trial
    output$downloadTable <- downloadHandler(
      filename = function(){
        paste("table.csv")
      },
      content = function(file){
        write.table(sens_spec_table(), file, sep=",", row.names=FALSE)
      }
    )
    
    sroc_plot <- reactive(
      DrawSrocPlot(data = data(),
                   covcheck = input$covcheck,
                   HSROCcheck = input$HSROCcheck,
                   prevcheck = input$prevcheck,
                   bivcheck = input$bivcheck,
                   cicheck = input$cicheck,
                   QAcheck = input$QAcheck,
                   cov_toggle = input$cov_toggle,
                   title = input$title,
                   weightcheck = input$weightcheck,
                   ci_col = input$ci_col,
                   extrapp = input$extrapp)
      )
    
    output$sroc <- renderPlot({
      sroc_plot()
    })
    
    # Allow users to download the interactive SROC curve
    output$downloadROC <- downloadHandler(
      # Specify the file name (either roc.png or roc.pdf)
      filename = function(){
        paste("roc", input$filetype, sep=".")
      },
      content = function(file){
        if (input$filetype == "png") {
          png(file, width = 750, height = 750)
        } else{
          pdf(file, width = 8.5, height = 8.5)
        }
        sroc_plot()
        dev.off()
      }
    )
    
    
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
    statTable <- reactive({
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
    })
    
    output$statTable <- renderTable({
      statTable()
    },
    sanitize.text.function = function(x) x
    )
    
    # Allow users the option to download the table of statistics
    output$downloadStatTable <- downloadHandler(
      # Specify the file name 
      filename = function(){
        paste("statTable.csv")
      },
      content = function(file){
        write.table(statTable(), file, sep=",", row.names=FALSE)
      }
    )
    
    
    # Stats for decision modelling distribution
    DecisionModel <- reactive({
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
    })
    
    output$DecisionModel <- renderTable({
      DecisionModel()
    }, sanitize.text.function = function(x) x)
    
    
    # Allow users the option to download the parameter estimates
    output$downloadParameters <- downloadHandler(
      filename = function(){
        paste("Parameters.csv")
      },
      content = function(file){
        write.table(DecisionModel(), file, sep=",", row.names=FALSE)
      }
    )
    
    
    # RevMan parameters
    revman <- reactive({
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
    })
    
    output$revman <- renderTable({
      revman()
    }, sanitize.text.function = function(x) x)
    
    
    # Allow users the option to download the RevMan parameters
    output$downloadRevMan <- downloadHandler(
      filename = function(){
        paste("RevMan.csv")
      },
      content = function(file){
        write.table(revman(), file, sep=",", row.names=FALSE)
      }
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
