
#' Module UI for the prevalence page.
#' 
#' @param id ID of the module
#' @return Div for the prevalence page
PrevalencePageUi <- function(id) {
  ns <- NS(id)
  
  div(
    h1("Prevalence"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        uiOutput(outputId = ns("Prev_input")),
        actionButton(inputId = ns("Prev_reset"), label = "Reset inputs"),
        br(),
        br(),
        radioButtons(
          inputId = ns("treecheck"),
          label = "Choose format to view expected results",
          choices = list(
            "Test first",
            "Disease first"
          )
        )
      ),
      mainPanel = mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Meta-analysis", 
            plotOutput(outputId = ns("MA_treeplot")),
            radioButtons(
              inputId = ns("filetype3"),
              label = "Select image format",
              choices = list(
                PNG = "png",
                PDF = "pdf"
              )
            ),
            downloadButton(outputId = ns("downloadPrev_MA"), label = "Download Plot"),
            br(),
            br(),
            p("Note: The numbers in brackets represent 95% confidence intervals.")
          ),
          tabPanel(
            title = "Sensitivity analysis",
            plotOutput(outputId = ns("SA_treeplot")),
            radioButtons(
              inputId = ns("filetype4"),
              label = "Select image format",
              choices = list(
                PNG = "png",
                PDF = "pdf"
              )
            ),
            downloadButton(outputId = ns("downloadPrev_SA"), label = "Download Plot"),
            br(),
            br(),
            p(
              "Note: The sensitvity analysis tab should be visited in order to produce a plot.",
              "The numbers here will be the same as those in the meta-analysis tree diagram if no",
              "studies are excluded from the analysis in the sensitivity analysis tab."
            ),
            p("The numbers in brackets represent 95% confidence intervals.")
          )
        )
      )
    )
  )
}

#' Module server for the prevalence page.
#' 
#' @param id ID of the module
#' @param data Reactive containing data frame.
PrevalencePageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Loading in images for TP, FP, FN, TN
    TPimg <- readPNG("www/TP.png")
    TNimg <- readPNG("www/TN.png")
    FPimg <- readPNG("www/FP.png")
    FNimg <- readPNG("www/FN.png")
    
    # Display inputs for the prevalence tab
    output$Prev_input <- renderUI({
      times <- input$Prev_reset
      X <- data()
      Prev <- ((X$TP + X$FN) / (X$TP + X$FN + X$FP + X$TN))*100 # multiply by 100 to get as a percentage
      meanPrev <- round(mean(Prev), 0)
      div(
        id = letters[(times %% length(letters)) + 1],
        numericInput(
          inputId = ns("patients"),
          label = "Number of patients",
          min = 0,
          value = 1000
        ),
        sliderInput(
          inputId = ns("prevslide"),
          label = "Prevalence of the disease in the population (as a percentage)",
          min = 0,
          max = 100,
          value = meanPrev
        ),
        p("The default value of the slider is the mean value of the prevalence of the disease from all studies included in the dataset.")
      )
    })
    
    # To plot the tree diagrams for the meta-analysis tab need to recalculate sens and spec
    # Tree diagrams for the MA tab 
    
    output$MA_treeplot <- renderPlot({
      X <- data()
      
      if (is.null(X)) {
        return()
      }
      
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
      
      
      # Sensitivity and specificity with 95% CI's 
      Sens <- plogis(lsens)
      Sens_lci <- plogis(lsens-qnorm(0.975)*se.lsens)
      Sens_uci <- plogis(lsens+qnorm(0.975)*se.lsens)
      Spec <- plogis(lspec)
      Spec_lci <- plogis(lspec-qnorm(0.975)*se.lspec)
      Spec_uci <- plogis(lspec+qnorm(0.975)*se.lspec)
      
      # Calculate TP, FP, FN and TN based on sensitivity, specificity and inputted prevalence 
      Diseased <-(input$prevslide / 100) * input$patients # assuming population of 1000 
      NonDiseased <- input$patients - Diseased 
      
      TP <- round(Sens*Diseased)
      TN <- round(Spec*NonDiseased)
      FN <- round((1-Sens)*Diseased)
      FP <- round((1-Spec)*NonDiseased)
      Pos <- TP + FP # number of positive results
      Neg <- TN + FN # number of negative results
      
      # Calculate for lci and uci 
      TP_lci <- round(Sens_lci*Diseased)
      TP_uci <- round(Sens_uci*Diseased)
      
      TN_lci <- round(Spec_lci*NonDiseased)
      TN_uci <- round(Spec_uci*NonDiseased)
      
      # Calculations of lci and uci specifically for FN and FP differ slightly
      # the lci is used to calculate the uci and vice versa due to the mapping of sens/spec and 1-sens/1-spec
      FN_uci <- round((1-Sens_lci)*Diseased)
      FP_uci <- round((1-Spec_lci)*NonDiseased) 
      
      FN_lci <- round((1-Sens_uci)*Diseased)
      FP_lci <- round((1-Spec_uci)*NonDiseased) 
      
      Pos_lci <- TP_lci  + FP_lci  # number of positive results
      Neg_lci <- TN_lci  + FN_lci  # number of negative results
      
      Pos_uci <- TP_uci  + FP_uci  # number of positive results
      Neg_uci <- TN_uci  + FN_uci  # number of negative results
      
      Diseased_lci <- TP_lci + FN_lci
      NonDiseased_lci <- TN_lci + FP_lci
      
      Diseased_uci <- TP_uci + FN_uci
      NonDiseased_uci <- TN_uci + FP_uci
      
      # Plot tree 1 - patients - positive/negative -  
      if (input$treecheck == "Test first"){
        par(mar=c(0,0,0,0))
        plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
        
        text(2.5,5.2,labels=bquote(paste(.(input$patients), " Patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
        
        text(1.5,3.75,labels=bquote(paste(.(Pos), " (", .(Pos_lci), ", ", .(Pos_uci),")")), cex=1.2, col="blue")
        text(3.5,3.75,labels=bquote(paste(.(Neg), " (", .(Neg_lci), ", ", .(Neg_uci),")")), cex=1.2, col="blue")
        text(1.5,3.45,"test +ve", cex=1.2)
        text(3.5,3.45,"test -ve", cex=1.2)
        
        text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
        text(1,1.8," are diseased", cex=1.2)
        text(1,1.5,"and test +ve", cex=1.2)
        
        text(2,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
        text(2,1.8," are not diseased", cex=1.2)
        text(2,1.5,"but test +ve", cex=1.2)
        
        text(3,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
        text(3,1.8, " are diseased", cex=1.2)
        text(3,1.5,"but test -ve", cex=1.2)
        
        text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
        text(4,1.8," are not diseased", cex=1.2)
        text(4.0,1.5,"and test -ve", cex=1.2)
        
        ax<-c(2.2,2.8,1.4,1.6,3.4,3.6) 
        ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
        axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
        ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
        
        for (i in(1:6)) {arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)}
        
        rasterImage(TPimg,0.8,-0.5,1.2,1.2)
        rasterImage(FPimg,1.8,-0.5,2.2,1.2)
        rasterImage(FNimg,2.8,-0.5,3.2,1.2)
        rasterImage(TNimg,3.8,-0.5,4.2,1.2)
        
      }
      
      if (input$treecheck == "Disease first"){
        par(mar=c(0,0,0,0))
        plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
        
        
        text(2.5,5.2,labels=bquote(paste(.(input$patients), " Patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
        
        text(1.5,3.75,labels=bquote(paste(.(Diseased), " (", .(Diseased_lci), ", ", .(Diseased_uci),")")), cex=1.2, col="blue")
        text(3.5,3.75,labels=bquote(paste(.(NonDiseased), " (", .(NonDiseased_lci), ", ", .(NonDiseased_uci),")")), cex=1.2, col="blue")
        text(1.5,3.45,"are diseased", cex=1.2)
        text(3.5,3.45,"are healthy", cex=1.2)
        
        text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
        text(1,1.8," are diseased", cex=1.2)
        text(1,1.5,"and test +ve", cex=1.2)
        
        text(2,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
        text(2,1.8," are diseased", cex=1.2)
        text(2,1.5,"but test -ve", cex=1.2)
        
        text(3,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
        text(3,1.8, " are not diseased", cex=1.2)
        text(3,1.5,"but test +ve", cex=1.2)
        
        text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
        text(4,1.8," are not diseased", cex=1.2)
        text(4.0,1.5,"and test -ve", cex=1.2)
        
        ax<-c(2.2,2.8,1.4,1.6,3.4,3.6) 
        ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
        axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
        ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
        
        for (i in(1:6)) {arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)}
        
        rasterImage(TPimg,0.8,-0.5,1.2,1.2)
        rasterImage(FNimg,1.8,-0.5,2.2,1.2)
        rasterImage(FPimg,2.8,-0.5,3.2,1.2)
        rasterImage(TNimg,3.8,-0.5,4.2,1.2)
      }
      
      
    })
    
    # Plot tree diagrams using the results from the sensitivity analysis
    output$SA_treeplot <- renderPlot({
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
      
      # Sensitivity and specificity with 95% CI's 
      Sens <- plogis(lsens)
      Sens_lci <- plogis(lsens-qnorm(0.975)*se.lsens)
      Sens_uci <- plogis(lsens+qnorm(0.975)*se.lsens)
      Spec <- plogis(lspec)
      Spec_lci <- plogis(lspec-qnorm(0.975)*se.lspec)
      Spec_uci <- plogis(lspec+qnorm(0.975)*se.lspec)
      
      # Calculate TP, FP, FN and TN based on sensitivity, specificity and inputted prevalence 
      Diseased <-(input$prevslide / 100) * input$patients # assuming population of 1000 
      NonDiseased <- input$patients - Diseased 
      
      TP <- round(Sens*Diseased)
      TN <- round(Spec*NonDiseased)
      FN <- round((1-Sens)*Diseased)
      FP <- round((1-Spec)*NonDiseased)
      Pos <- TP + FP # number of positive results
      Neg <- TN + FN # number of negative results
      
      # Calculate for lci and uci 
      TP_lci <- round(Sens_lci*Diseased)
      TP_uci <- round(Sens_uci*Diseased)
      
      TN_lci <- round(Spec_lci*NonDiseased)
      TN_uci <- round(Spec_uci*NonDiseased)
      
      # Calculations of lci and uci specifically for FN and FP differ slightly
      # the lci is used to calculate the uci and vice versa due to the mapping of sens/spec and 1-sens/1-spec
      FN_uci <- round((1-Sens_lci)*Diseased)
      FP_uci <- round((1-Spec_lci)*NonDiseased) 
      
      FN_lci <- round((1-Sens_uci)*Diseased)
      FP_lci <- round((1-Spec_uci)*NonDiseased) 
      
      Pos_lci <- TP_lci  + FP_lci  # number of positive results
      Neg_lci <- TN_lci  + FN_lci  # number of negative results
      
      Pos_uci <- TP_uci  + FP_uci  # number of positive results
      Neg_uci <- TN_uci  + FN_uci  # number of negative results
      
      Diseased_lci <- TP_lci + FN_lci
      NonDiseased_lci <- TN_lci + FP_lci
      
      Diseased_uci <- TP_uci + FN_uci
      NonDiseased_uci <- TN_uci + FP_uci
      
      # Plot tree 1 - patients - positive/negative -  
      if (input$treecheck == "Test first"){
        par(mar=c(0,0,0,0))
        plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
        
        text(2.5,5.2,labels=bquote(paste(.(input$patients), " Patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
        
        text(1.5,3.75,labels=bquote(paste(.(Pos), " (", .(Pos_lci), ", ", .(Pos_uci),")")), cex=1.2, col="blue")
        text(3.5,3.75,labels=bquote(paste(.(Neg), " (", .(Neg_lci), ", ", .(Neg_uci),")")), cex=1.2, col="blue")
        text(1.5,3.45,"test +ve", cex=1.2)
        text(3.5,3.45,"test -ve", cex=1.2)
        
        text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
        text(1,1.8," are diseased", cex=1.2)
        text(1,1.5,"and test +ve", cex=1.2)
        
        text(2,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
        text(2,1.8," are not diseased", cex=1.2)
        text(2,1.5,"but test +ve", cex=1.2)
        
        text(3,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
        text(3,1.8, " are diseased", cex=1.2)
        text(3,1.5,"but test -ve", cex=1.2)
        
        text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
        text(4,1.8," are not diseased", cex=1.2)
        text(4.0,1.5,"and test -ve", cex=1.2)
        
        ax<-c(2.2,2.8,1.4,1.6,3.4,3.6) 
        ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
        axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
        ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
        
        for (i in(1:6)) {arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)}
        
        rasterImage(TPimg,0.8,-0.5,1.2,1.2)
        rasterImage(FPimg,1.8,-0.5,2.2,1.2)
        rasterImage(FNimg,2.8,-0.5,3.2,1.2)
        rasterImage(TNimg,3.8,-0.5,4.2,1.2)
        
      }
      
      if (input$treecheck == "Disease first"){
        par(mar=c(0,0,0,0))
        plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
        
        
        text(2.5,5.2,labels=bquote(paste(.(input$patients), " Patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
        
        text(1.5,3.75,labels=bquote(paste(.(Diseased), " (", .(Diseased_lci), ", ", .(Diseased_uci),")")), cex=1.2, col="blue")
        text(3.5,3.75,labels=bquote(paste(.(NonDiseased), " (", .(NonDiseased_lci), ", ", .(NonDiseased_uci),")")), cex=1.2, col="blue")
        text(1.5,3.45,"are diseased", cex=1.2)
        text(3.5,3.45,"are healthy", cex=1.2)
        
        text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
        text(1,1.8," are diseased", cex=1.2)
        text(1,1.5,"and test +ve", cex=1.2)
        
        text(2,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
        text(2,1.8," are diseased", cex=1.2)
        text(2,1.5,"but test -ve", cex=1.2)
        
        text(3,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
        text(3,1.8, " are not diseased", cex=1.2)
        text(3,1.5,"but test +ve", cex=1.2)
        
        text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
        text(4,1.8," are not diseased", cex=1.2)
        text(4.0,1.5,"and test -ve", cex=1.2)
        
        ax<-c(2.2,2.8,1.4,1.6,3.4,3.6) 
        ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
        axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
        ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
        
        for (i in(1:6)) {arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)}
        
        rasterImage(TPimg,0.8,-0.5,1.2,1.2)
        rasterImage(FNimg,1.8,-0.5,2.2,1.2)
        rasterImage(FPimg,2.8,-0.5,3.2,1.2)
        rasterImage(TNimg,3.8,-0.5,4.2,1.2)
      }
      
    })
    
    # Allow users to download the meta-analysis tree diagram
    output$downloadPrev_MA <- downloadHandler(
      # Specify the file name (either roc.png or roc.pdf)
      filename = function(){
        paste("MA_tree", input$filetype3, sep=".")
      },
      content = function(file){
        # open the device
        # create the plot
        # close the device
        if(input$filetype3 == "png")
          png(file)
        else
          pdf(file)
        
        
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
        
        
        # Sensitivity and specificity with 95% CI's
        Sens <- plogis(lsens)
        Sens_lci <- plogis(lsens-qnorm(0.975)*se.lsens)
        Sens_uci <- plogis(lsens+qnorm(0.975)*se.lsens)
        Spec <- plogis(lspec)
        Spec_lci <- plogis(lspec-qnorm(0.975)*se.lspec)
        Spec_uci <- plogis(lspec+qnorm(0.975)*se.lspec)
        
        # Calculate TP, FP, FN and TN based on sensitivity, specificity and inputted prevalence
        Diseased <-(input$prevslide / 100) * input$patients # assuming population of 1000
        NonDiseased <- input$patients - Diseased
        
        TP <- round(Sens*Diseased)
        TN <- round(Spec*NonDiseased)
        FN <- round((1-Sens)*Diseased)
        FP <- round((1-Spec)*NonDiseased)
        Pos <- TP + FP # number of positive results
        Neg <- TN + FN # number of negative results
        
        # Calculate for lci and uci
        TP_lci <- round(Sens_lci*Diseased)
        TP_uci <- round(Sens_uci*Diseased)
        
        TN_lci <- round(Spec_lci*NonDiseased)
        TN_uci <- round(Spec_uci*NonDiseased)
        
        # Calculations of lci and uci specifically for FN and FP differ slightly
        # the lci is used to calculate the uci and vice versa due to the mapping of sens/spec and 1-sens/1-spec
        FN_uci <- round((1-Sens_lci)*Diseased)
        FP_uci <- round((1-Spec_lci)*NonDiseased)
        
        FN_lci <- round((1-Sens_uci)*Diseased)
        FP_lci <- round((1-Spec_uci)*NonDiseased)
        
        Pos_lci <- TP_lci  + FP_lci  # number of positive results
        Neg_lci <- TN_lci  + FN_lci  # number of negative results
        
        Pos_uci <- TP_uci  + FP_uci  # number of positive results
        Neg_uci <- TN_uci  + FN_uci  # number of negative results
        
        Diseased_lci <- TP_lci + FN_lci
        NonDiseased_lci <- TN_lci + FP_lci
        
        Diseased_uci <- TP_uci + FN_uci
        NonDiseased_uci <- TN_uci + FP_uci
        
        # Plot tree 1 - patients - positive/negative -
        if (input$treecheck == "Test first"){
          par(mar=c(0,0,0,0))
          plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
          
          text(2.5,5.2,labels=bquote(paste(.(input$patients), " patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
          
          text(1.5,3.75,labels=bquote(paste(.(Pos), " (", .(Pos_lci), ", ", .(Pos_uci),")")), cex=1.2, col="blue")
          text(3.5,3.75,labels=bquote(paste(.(Neg), " (", .(Neg_lci), ", ", .(Neg_uci),")")), cex=1.2, col="blue")
          text(1.5,3.45,"test +ve", cex=1.2)
          text(3.5,3.45,"test -ve", cex=1.2)
          
          text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
          text(1,1.8," are diseased", cex=1.2)
          text(1,1.5,"and test +ve", cex=1.2)
          
          text(2,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
          text(2,1.8," are not diseased", cex=1.2)
          text(2,1.5,"but test +ve", cex=1.2)
          
          text(3,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
          text(3,1.8, " are diseased", cex=1.2)
          text(3,1.5,"but test -ve", cex=1.2)
          
          text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
          text(4,1.8," are not diseased", cex=1.2)
          text(4.0,1.5,"and test -ve", cex=1.2)
          
          ax<-c(2.2,2.8,1.4,1.6,3.4,3.6)
          ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
          axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
          ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
          
          for (i in(1:6)) {arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)}
          
          rasterImage(TPimg,0.8,-0.5,1.2,1.2)
          rasterImage(FPimg,1.8,-0.5,2.2,1.2)
          rasterImage(FNimg,2.8,-0.5,3.2,1.2)
          rasterImage(TNimg,3.8,-0.5,4.2,1.2)
          
        }
        
        if (input$treecheck == "Disease first"){
          par(mar=c(0,0,0,0))
          plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
          
          
          text(2.5,5.2,labels=bquote(paste(.(input$patients), " patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
          
          text(1.5,3.75,labels=bquote(paste(.(Diseased), " (", .(Diseased_lci), ", ", .(Diseased_uci),")")), cex=1.2, col="blue")
          text(3.5,3.75,labels=bquote(paste(.(NonDiseased), " (", .(NonDiseased_lci), ", ", .(NonDiseased_uci),")")), cex=1.2, col="blue")
          text(1.5,3.45,"are diseased", cex=1.2)
          text(3.5,3.45,"are healthy", cex=1.2)
          
          text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
          text(1,1.8," are diseased", cex=1.2)
          text(1,1.5,"and test +ve", cex=1.2)
          
          text(2,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
          text(2,1.8," are diseased", cex=1.2)
          text(2,1.5,"but test -ve", cex=1.2)
          
          text(3,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
          text(3,1.8, " are not diseased", cex=1.2)
          text(3,1.5,"but test +ve", cex=1.2)
          
          text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
          text(4,1.8," are not diseased", cex=1.2)
          text(4.0,1.5,"and test -ve", cex=1.2)
          
          ax<-c(2.2,2.8,1.4,1.6,3.4,3.6)
          ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
          axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
          ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
          
          for (i in(1:6)) {arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)}
          
          rasterImage(TPimg,0.8,-0.5,1.2,1.2)
          rasterImage(FNimg,1.8,-0.5,2.2,1.2)
          rasterImage(FPimg,2.8,-0.5,3.2,1.2)
          rasterImage(TNimg,3.8,-0.5,4.2,1.2)
        }
        
        dev.off()
      })
    
    # Allow users to download the meta-analysis tree diagram
    output$downloadPrev_SA <- downloadHandler(
      # Specify the file name (either roc.png or roc.pdf)
      filename = function(){
        paste("SA_tree", input$filetype3, sep=".")
      },
      content = function(file){
        # open the device
        # create the plot
        # close the device
        if(input$filetype4 == "png")
          png(file)
        else
          pdf(file)
        
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
        
        # Sensitivity and specificity with 95% CI's 
        Sens <- plogis(lsens)
        Sens_lci <- plogis(lsens-qnorm(0.975)*se.lsens)
        Sens_uci <- plogis(lsens+qnorm(0.975)*se.lsens)
        Spec <- plogis(lspec)
        Spec_lci <- plogis(lspec-qnorm(0.975)*se.lspec)
        Spec_uci <- plogis(lspec+qnorm(0.975)*se.lspec)
        
        # Calculate TP, FP, FN and TN based on sensitivity, specificity and inputted prevalence 
        Diseased <-(input$prevslide / 100) * input$patients # assuming population of 1000 
        NonDiseased <- input$patients - Diseased 
        
        TP <- round(Sens*Diseased)
        TN <- round(Spec*NonDiseased)
        FN <- round((1-Sens)*Diseased)
        FP <- round((1-Spec)*NonDiseased)
        Pos <- TP + FP # number of positive results
        Neg <- TN + FN # number of negative results
        
        # Calculate for lci and uci 
        TP_lci <- round(Sens_lci*Diseased)
        TP_uci <- round(Sens_uci*Diseased)
        
        TN_lci <- round(Spec_lci*NonDiseased)
        TN_uci <- round(Spec_uci*NonDiseased)
        
        # Calculations of lci and uci specifically for FN and FP differ slightly
        # the lci is used to calculate the uci and vice versa due to the mapping of sens/spec and 1-sens/1-spec
        FN_uci <- round((1-Sens_lci)*Diseased)
        FP_uci <- round((1-Spec_lci)*NonDiseased) 
        
        FN_lci <- round((1-Sens_uci)*Diseased)
        FP_lci <- round((1-Spec_uci)*NonDiseased) 
        
        Pos_lci <- TP_lci  + FP_lci  # number of positive results
        Neg_lci <- TN_lci  + FN_lci  # number of negative results
        
        Pos_uci <- TP_uci  + FP_uci  # number of positive results
        Neg_uci <- TN_uci  + FN_uci  # number of negative results
        
        Diseased_lci <- TP_lci + FN_lci
        NonDiseased_lci <- TN_lci + FP_lci
        
        Diseased_uci <- TP_uci + FN_uci
        NonDiseased_uci <- TN_uci + FP_uci
        
        # Plot tree 1 - patients - positive/negative -  
        if (input$treecheck == "Test first"){
          par(mar=c(0,0,0,0))
          plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
          
          text(2.5,5.2,labels=bquote(paste(.(input$patients), " patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
          
          text(1.5,3.75,labels=bquote(paste(.(Pos), " (", .(Pos_lci), ", ", .(Pos_uci),")")), cex=1.2, col="blue")
          text(3.5,3.75,labels=bquote(paste(.(Neg), " (", .(Neg_lci), ", ", .(Neg_uci),")")), cex=1.2, col="blue")
          text(1.5,3.45,"test +ve", cex=1.2)
          text(3.5,3.45,"test -ve", cex=1.2)
          
          text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
          text(1,1.8," are diseased", cex=1.2)
          text(1,1.5,"and test +ve", cex=1.2)
          
          text(2,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
          text(2,1.8," are not diseased", cex=1.2)
          text(2,1.5,"but test +ve", cex=1.2)
          
          text(3,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
          text(3,1.8, " are diseased", cex=1.2)
          text(3,1.5,"but test -ve", cex=1.2)
          
          text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
          text(4,1.8," are not diseased", cex=1.2)
          text(4.0,1.5,"and test -ve", cex=1.2)
          
          ax<-c(2.2,2.8,1.4,1.6,3.4,3.6) 
          ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
          axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
          ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
          
          for (i in(1:6)) {arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)}
          
          rasterImage(TPimg,0.8,-0.5,1.2,1.2)
          rasterImage(FPimg,1.8,-0.5,2.2,1.2)
          rasterImage(FNimg,2.8,-0.5,3.2,1.2)
          rasterImage(TNimg,3.8,-0.5,4.2,1.2)
          
        }
        
        if (input$treecheck == "Disease first"){
          par(mar=c(0,0,0,0))
          plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
          
          
          text(2.5,5.2,labels=bquote(paste(.(input$patients), " patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
          
          text(1.5,3.75,labels=bquote(paste(.(Diseased), " (", .(Diseased_lci), ", ", .(Diseased_uci),")")), cex=1.2, col="blue")
          text(3.5,3.75,labels=bquote(paste(.(NonDiseased), " (", .(NonDiseased_lci), ", ", .(NonDiseased_uci),")")), cex=1.2, col="blue")
          text(1.5,3.45,"are diseased", cex=1.2)
          text(3.5,3.45,"are healthy", cex=1.2)
          
          text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
          text(1,1.8," are diseased", cex=1.2)
          text(1,1.5,"and test +ve", cex=1.2)
          
          text(2,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
          text(2,1.8," are diseased", cex=1.2)
          text(2,1.5,"but test -ve", cex=1.2)
          
          text(3,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
          text(3,1.8, " are not diseased", cex=1.2)
          text(3,1.5,"but test +ve", cex=1.2)
          
          text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
          text(4,1.8," are not diseased", cex=1.2)
          text(4.0,1.5,"and test -ve", cex=1.2)
          
          ax<-c(2.2,2.8,1.4,1.6,3.4,3.6) 
          ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
          axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
          ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
          
          for (i in(1:6)) {arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)}
          
          rasterImage(TPimg,0.8,-0.5,1.2,1.2)
          rasterImage(FNimg,1.8,-0.5,2.2,1.2)
          rasterImage(FPimg,2.8,-0.5,3.2,1.2)
          rasterImage(TNimg,3.8,-0.5,4.2,1.2)
        }
        dev.off()
        
      })
  })
}
