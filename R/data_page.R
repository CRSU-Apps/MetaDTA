
#' Module UI for the data upload page.
#' 
#' @param id ID of the module
#' @return Div for the data upload page
DataPageUi <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fileInput(
        inputId = ns("data"),
        label = "Please select a file",
        buttonLabel = "Select",
        accept = c(".csv", ".xlsx")
      ),
      helpText("Default maximum file size is 5MB. Both Excel (.xlsx) and Comma Seperated Value (.csv) files are accepted."),
      tags$hr(),
      h4(helpText(tags$strong("File options"))),
      checkboxInput(inputId = ns("header"), label = "First row as column headings", value = TRUE),
      br(),
      radioButtons(
        inputId = ns("default"),
        label = h4(helpText(tags$strong("Select example dataset"))),
        choices = list(
          "Standard",
          "With Quality Assessment",
          "With Covariates",
          "With Quality assessment and Covariates"
        )
      ),
      br(),
      h4(helpText(tags$strong("Download example datasets"))),
      downloadButton(outputId = ns("downloadData1"), label = "Standard Example"),
      br(),
      downloadButton(outputId = ns("downloadData2"), label = "Quality Assessment Example"),
      br(),
      downloadButton(outputId = ns("downloadData3"), label = "Covariate Example"),
      br(),
      downloadButton(outputId = ns("downloadData4"), label = "Quality Assessment and Covariate Example")
    ),
    mainPanel = mainPanel(
      tabsetPanel(
        tabPanel(
          title = "File Upload", 
          h3("Please select a file to upload"),
          br(),
          p("The file should contain at least six columns. Labelling of columns is case sensitive."),
          p("The", tags$strong("first"), "column should be labelled", tags$strong("author"), "and contain the name of 
          the study author. The author name must be unique for each study."),
          p(
            "The",
            tags$strong("second"),
            "column should be labelled",
            tags$strong("year"),
            "and contain the year of publication."
          ),
          p(
            "The",
            tags$strong("third"),
            "column should be labelled",
            tags$strong("TP"),
            "and contain the number of patients with a true positive test result."
          ),
          p(
            "The",
            tags$strong("fourth"),
            "column should be labelled",
            tags$strong("FN"),
            "and contain the number of patients with a false negative test result."
          ),
          p(
            "The",
            tags$strong("fifth"),
            "column should be labelled",
            tags$strong("FP"),
            "and contain the number of patients with a false positive test result."
          ),
          p(
            "The",
            tags$strong("sixth"),
            "column should be labelled",
            tags$strong("TN"),
            "and contain the number of patients with a true negative test result."
          ),
          br(),
          h4("Including quality assessment data (optional)"),
          p("To allow the quality assessment results from the QUADAS-2 tool to be incorporated into the plots an additional seven columns are required."),
          p(
            "The",
            tags$strong("seventh"),
            "column should be labelled",
            tags$strong("rob_PS"),
            ", representing the risk of bias in terms of the patient selection."
          ),
          p(
            "The",
            tags$strong("eighth"),
            "column should be labelled",
            tags$strong("rob_IT"),
            ", representing the risk of bias in terms of the index test."
          ),
          p(
            "The",
            tags$strong("ninth"),
            "column should be labelled",
            tags$strong("rob_RS"),
            ", representing the risk of bias in terms of the reference standard."
          ),
          p(
            "The",
            tags$strong("tenth"),
            "column should be labelled",
            tags$strong("rob_FT"),
            ", representing the risk of bias in terms of the flow and timing."
          ),
          p(
            "The",
            tags$strong("eleventh"),
            "column should be labelled",
            tags$strong("ac_PS"),
            ", representing the applicability concerns in terms of the patient selection."
          ),
          p(
            "The",
            tags$strong("twelfth"),
            "column should be labelled",
            tags$strong("ac_IT"),
            ", representing the applicability concerns in terms of the index test."
          ),
          p(
            "The",
            tags$strong("thirteenth"),
            "column should be labelled",
            tags$strong("ac_RS"),
            ", representing the applicability concerns in terms of the reference standard."
          ),
          p(
            "These columns should contain the numbers",
            tags$strong("1, 2 or 3"),
            "which represent",
            tags$strong("low, high or unclear"),
            "risk of bias/applicability concerncs respectively."
          ),
          br(),
          p("For information about the QUADAS-2 tool and how to use it please visit:"),
          tags$a(
            href = "https://www.bristol.ac.uk/population-health-sciences/projects/quadas/quadas-2/",
            "https://www.bristol.ac.uk/population-health-sciences/projects/quadas/quadas-2/",
            target="_blank"
          ),
          br(),
          br(),
          h4("Including covariates (optional)"), 
          p("If any covariates are to be added to the file they should be included as the last columns in the file. If quality 
                                           assessment data is not included in the file the covariates should be entered starting at the", tags$strong("seventh"), "column. If quality assessment
                                           data is included in the file the covariate data should be entered starting at the", tags$strong("fourteenth"), "column. Multiple covariates can be entered."
          ),
          br(),
          p("The default dataset, pre-loaded on the 'Data for Analysis' tab will be used for analysis if no file is 
                                            selected. The 'Data for Analysis' tab will automatically update once a file is successfully loaded."
          ),
          p("The default datasets can be downloaded using the buttons in the sidebar and used as templates to enter your own data."),
          br(),
          h4("Sensitivity analysis"),
          p(
            "To ensure the correct studies are excluded from sensitivity analyses please ensure that study data rows are ordered",
            "by the 'author' column alphabetically from A to Z prior to uploading to MetaDTA (Excel can do this easily)."
          )
        ),
        tabPanel(
          title = "Example datasets",
          br(),
          p(
            "The default dataset uses data from a systematic review investigating the accuracy of an informant-based questionnaire, for detection of all cause",
            "dementia in adults. The dataset consists of thirteen studies assessing the use of the IQCODE (Informant Questionnaire",
            "on Cognitive Decline in the Elderly) tool for identifying adults with dementia within a secondary care setting."
          ),
          p(
            "The IQCODE tool contains a number of questions which are scored on a five point scale. The IQCODE tool has a number of",
            "different variants, depending on how many questions are asked. The questions are based on the performance of everyday",
            "tasks related to cognitive function. These are then rated on a scale of 1-5. The final score is an average score for each",
            "question. The IQCODE tool is only a screening tool and does not offer a definitive diagnosis of dementia."
          ),
          p(
            "Under the 'Select example dataset' option there are four different datasets to choose from. The default is the 'Standard' dataset,",
            "which includes the author and year of each study along with the true positives (TP), false positives (FP), false negatives (FN) and",
            "true negatives (TN). The other options add data onto this 'Standard' dataset and highlight how datasets with quality assessment scores and/or",
            "covariates should be displayed."
          ),
          p(
            "With this dataset there are three different covariates. The first being the country in which each individual study was conducted.",
            "The second is the threshold used in each individual study. In this case if an individuals final score was higher than the threshold the individual",
            "was classified as having dementia and would require further diagnosis. The final covariate is labelled as 'IQCODE' and indicates",
            "which variant of the tool was used in each individual study. The variants are identified by the number of questions used in the",
            "questionnaire. There are three different variants the 16-item, 26-item and 32-item."
          )
        ),
        tabPanel(
          title = "Data for Analysis",
          tableOutput(ns("rawtable"))
        )
      )
    )
  )
}

#' Module server for the data upload page.
#' 
#' @param id ID of the module
#' 
#' @return Reactive containing data frame.
DataPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    Standard <- rio::import("Data/Standard.csv")
    QA <- rio::import("Data/QA.csv") 
    Cov <- rio::import("Data/Cov.csv")
    QA_Cov <- rio::import("Data/QA_Cov.csv")
    
    # Make the data file that was uploaded reactive by giving it the name file1
    # now I can use file1 to refer to the different parts of the data file
    # Put data into a data frame so it can be used in analysis
    data <- reactive({ 
      file1 <- input$data
      if (is.null(file1)) {
        if (input$default == "Standard") {
          return(Standard)
        } else if(input$default == "With Quality Assessment") {
          return(QA)
        } else if(input$default == "With Covariates") {
          return(Cov)
        } else if(input$default == "With Quality assessment and Covariates") {
          return(QA_Cov)
        } else {
          stop(glue::glue("Data set '{input$default}' not supported"))
        }
      } else {
        return(rio::import(file1$datapath, header = input$header, stringsAsFactors = FALSE))
      }
    })
    
    # Create a table which displays the raw data just uploaded by the user
    output$rawtable <- renderTable({
      # if(!is.null(data())){
        return(data())
      # }
    })
    
    # Allow users the option to download the standard example dataset
    output$downloadData1 <- downloadHandler(
      # Specify the file name 
      filename = function() {
        paste("Standard.csv")
      },
      content = function(file) {
        write.table(Standard, file, sep = ",", row.names = FALSE)
      }
    )
    
    # Allow users the option to download the quality assessment example dataset
    output$downloadData2 <- downloadHandler(
      # Specify the file name 
      filename = function() {
        paste("QA.csv")
      },
      content = function(file) {
        write.table(QA, file, sep = ",", row.names = FALSE)
      }
    )
    
    # Allow users the option to download the covariate example dataset
    output$downloadData3 <- downloadHandler(
      # Specify the file name
      filename = function() {
        paste("Cov.csv")
      },
      content = function(file) {
        write.table(Cov, file, sep = ",", row.names = FALSE)
      }
    )
    
    # Allow users the option to download the quality assessment and covariate example dataset
    output$downloadData4 <- downloadHandler(
      # Specify the file name
      filename = function() {
        paste("QA_Cov.csv")
      },
      content = function(file) {
        write.table(QA_Cov, file, sep = ",", row.names = FALSE)
      }
    )
    
    return(data)
  })
}
