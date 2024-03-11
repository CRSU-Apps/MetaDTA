
# Loading in images for TP, FP, FN, TN
TPimg<-readPNG('./www/TP.png')
TNimg<-readPNG('./www/TN.png')
FPimg<-readPNG('./www/FP.png')
FNimg<-readPNG('./www/FN.png')


#################################################################################################################

# Set up user interface to be a series of tabs using navbarPage
navbarPage(
  title = "MetaDTA: Diagnostic Test Accuracy Meta-analysis",
  
  #Set up google analytics 
  header = singleton(
    tags$head(
      google_analytics_header_ui(id = "analytics"),
      includeHTML("www/favicon/favicon.html"),
      tags$meta(name="description", content="An online interactive application for conducting meta-analysis of diagnostic test accuracy studies"),
      tags$meta(name="keywords", content="MetaDTA, DTA, Diagnostic, Test, Accuracy, Meta, Analysis, App"),
      tags$meta(property="og:title", content="MetaDTA: Diagnostic Test Accuracy Meta-analysis: V2.1.1"),
      tags$meta(property="og:description", content="An online interactive application for conducting meta-analysis of diagnostic test accuracy studies"),
      tags$meta(property="og:image", content="https://raw.githubusercontent.com/CRSU-Apps/MetaDTA/main/www/roc_curve.png")
    )
  ),
  
  tabPanel(
    title = "Home",
    HomePageUi(id = "home")
  ),
  tabPanel(
    title = "Load Data",
    DataPageUi(id = "data")
  ),
                 
                 #############################
                 ### Tab 3 - Meta-analysis ###
                 #############################
                 
                 # Set up a third tab for showing the results of the meta-analysis
                 tabPanel("Meta-Analysis", h1("Meta-Analysis of Diagnostic Test Accuracy Studies"),
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("MA_input"),
                              actionButton(inputId = "MA_reset", label = "Reset all inputs")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Study-level Outcomes", 
                                         br(), 
                                         p("Note: If the presence of zero's for two of TP, FN, FP and TN causes sensitivity or specificity to be calculated
                                           as 0/0 than an error message will appear."),
                                         br(),
                                         DT::dataTableOutput("table"),
                                         downloadButton("downloadTable", "Download Table"),
                                         br(),
                                         br(),
                                         p("Note: Arrows to the right of the column headings can be used to sort data into ascending or descending order. "),
                                         p("N is  the total number of individuals in each study ( N = TP + FN + FP + TN )"),
                                         p("Sens is the sensitivity, which is the probability of a positive test result given that 
                                           the patient has the disease ( Sens = TP / [TP + FN] )"),
                                         p("Spec is the specificity, which is the probability of a negative test result given that
                                           the patient does not have the disease ( Spec = TN / [TN + FP] )"), 
                                         p("Weight_Sens is the precentage study weight of sensitvitiy, calculated using methods by Burke et al."),
                                         p("Weight_Spec is the percentage study weight of sensitivity, calculated using methods by Burke et al."),
                                         br()
                                         ),
                                tabPanel("SROC plot",
                                         h5("Note: At least one box under 'Options for SROC plot tab' must be selected to avoid an error 
                                            message"),
                                         br(),
                                         textInput("title", label = h4("Plot title"), value = "Random Effects Meta-Analysis", width='500px'),
                                         plotOutput("sroc", width="750px", height="750px", click=clickOpts("plot_click_ma")),
                                         br(),
                                         radioButtons("filetype", label="Select plot format", choices=list("png", "PDF")),
                                         downloadButton("downloadROC", "Download Plot"),
                                         br(),
                                         br(),
                                         h5("Click the middle of the data points for individual study summaries (an error message may occur if not
                                            selecting the middle of the pie chart when displaying risk of bias or acceptability concerns)"),
                                         textOutput("clickinfo_ma"),
                                         conditionalPanel(condition = "input.plot_click_ma != null", plotOutput("piechart")),
                                         p("Note: If quality asessment data is being used and pie charts are being plotted then study weight and 
                                            covariates cannot be displayed. However, if selected on the sidebar the information will still be 
                                            displayed when individual studies are clicked on.")
                                         ),
                                tabPanel("Statistics", br(),
                                         tableOutput("statTable"),
                                         downloadButton("downloadStatTable", "Download Table"),
                                         br(),
                                         br()
                                         ),
                                tabPanel("Parameter Estimates", 
                                         h5("Below are the parameter estimates for the bivariate normal distribution for mean sensitivity and 
                                            specificty (on the logit scale). Users may find these useful for further modelling e.g. inclusion
                                            of test accuracy in a decision modelling framework."),
                                         br(),
                                         img(src="decision_modelling_distribution.png", height=100, width=400),
                                         br(),
                                         h5("where:"),
                                         tableOutput("DecisionModel"),
                                         downloadButton("downloadParameters", "Download Table")
                                         ),
                                tabPanel("Parameters for RevMan",
                                         h5("Below are the parameter values required by Cochrane's RevMan software to 
                                            construct plots in the ROC space for users who wish to include the analysis results 
                                            as part of a Cochrane review."),
                                         tableOutput("revman"),
                                         downloadButton("downloadRevMan", "Download Table")
                                         ),
                                tabPanel("Forest Plots",
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"), plotOutput("forestMA_sens"),plotOutput("forestMA_spec"))
                                         ),
                                         radioButtons("filetype_forest", label="Select plot format", choices=list("png", "PDF")),
                                         downloadButton("download_forestMA_sens", "Download Sensitivity Forest Plot"),
                                         downloadButton("download_forestMA_spec", "Download Specificity Forest Plot")
                                         )
                                ) 
                              )
                            )
                            ),
                 
                 ####################################
                 ### Tab 4 - Sensitivity analysis ###
                 ####################################
                 
                 # Set up a fourth tab for conducting sensitivity analyses
                 tabPanel("Sensitivity Analysis", h1("Sensitivity Analysis"),
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("SA_input"),
                              actionButton(inputId = "SA_reset", label = "Reset all inputs")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Study-level Outcomes",
                                         br(), 
                                         DT::dataTableOutput("table2"),
                                         downloadButton("downloadTable2", "Download Table"),
                                         br(),
                                         br(),
                                         h5("Note: This table only includes studies selected in the sidebar."),
                                         p("N is  the total number of individuals in each study ( N = TP + FN + FP + TN )."),
                                         p("Sens is the sensitivity, which is the probability of a positive test result given that 
                                           the patient has the disease ( Sens = TP / [TP + FN] )."),
                                         p("Spec is the specificity, which is the probability of a negative test result given that
                                           the patient does not have the disease ( Spec = TN / [TN + FP] )."), 
                                         p("Weight_Sens is the precentage study weight of sensitvitiy, calculated using methods by Burke et al."),
                                         p("Weight_Spec is the percentage study weight of sensitivity, calculated using methods by Burke et al."),
                                         br()
                                         ),
                                tabPanel("SROC plot", 
                                         h5("Note: At least two studies must be selected for inclusion to avoid an error 
                                            message"),
                                         br(),
                                         textInput("title_sa", label = h4("Plot title"), value = "Random Effects Meta-Analysis", 
                                                   width='500px'),
                                         uiOutput(outputId="sensplot"),
                                         br(),
                                         radioButtons("filetype2", label="Select plot format", choices=list("png", "PDF")),
                                         downloadButton("downloadROC_sa", "Download Plot"),
                                         br(),
                                         br(),
                                         h5("Click the middle of the data points for individual study summaries (an error message may occur if not
                                            selecting the middle of the pie chart when displaying risk of bias or acceptability concerns)"),
                                         textOutput("clickinfo_ma2"),
                                         conditionalPanel(condition = "input.plot_click_ma2 != null", plotOutput("piechart2"))
                                         ),
                                tabPanel("Statistics", 
                                         h4("All studies"), 
                                         tableOutput("orig_statTable"),
                                         br(),
                                         h4("Selected studies only"), tableOutput("sa_statTable"),#)
                                         downloadButton("downloadSATable", "Download Table")
                                         ),
                                tabPanel("Parameter Estimates",
                                         h5("Below are the parameter estimates for the bivariate normal distribution for mean sensitivity and 
                                            specificty (on the logit scale). Estimates here are taken from the senstivity analysis model which 
                                            only includes studies selected in the sidebar. Users may find these useful for further modelling 
                                            e.g. inclusion of test accuracy in a decision modelling framework."),
                                         br(),
                                         img(src="decision_modelling_distribution.png", height=100, width=400),
                                         br(),
                                         h5("where:"),
                                         tableOutput("DecisionModel2"),
                                         downloadButton("downloadParameters2", "Download Table")
                                         ),
                                tabPanel("Parameters for RevMan",
                                         h5("Below are the parameter values required by Cochrane's RevMan software to 
                                            construct plots in the ROC space for users who wish to include the analysis results 
                                            as part of a Cochrane review."),
                                         tableOutput("revman2"),
                                         downloadButton("downloadRevMan2", "Download Table")),
                                tabPanel("Forest Plots",
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"), plotOutput("forestSA_sens"),plotOutput("forestSA_spec"))
                                         ),
                                         radioButtons("filetype_forest2", label="Select plot format", choices=list("png", "PDF")),
                                         downloadButton("download_forestSA_sens", "Download Sensitivity Forest Plot"),
                                         downloadButton("download_forestSA_spec", "Download Specificity Forest Plot"),
                                         br(),
                                         br(),
                                         p("Note: These plots only include studies selected in the sidebar.")
                                )
                              )
                          )
                         )
                    ),
                 
                 ##########################
                 ### Tab 5 - Prevalence ###
                 ##########################
                 tabPanel("Prevalence", h1("Prevalence"),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("Prev_input"),
                                actionButton(inputId = "Prev_reset", label = "Reset inputs"),
                                br(),
                                br(),
                                radioButtons(inputId = "treecheck", label = "Choose format to view expected results", choices = list("Tree 1"=1,
                                             "Tree 2" = 2), selected = 1)
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Meta-analysis", 
                                           plotOutput("MA_treeplot"),
                                           radioButtons("filetype3", label="Select image format", choices=list("png", "PDF")),
                                           downloadButton("downloadPrev_MA", "Download Plot"),
                                           br(),
                                           br(),
                                           p("Note: The numbers in brackets represent 95% confidence intervals.")
                                           ),
                                  tabPanel("Sensitivity analysis",
                                           plotOutput("SA_treeplot"),
                                           radioButtons("filetype4", label="Select image format", choices=list("png", "PDF")),
                                           downloadButton("downloadPrev_SA", "Download Plot"),
                                           br(),
                                           br(),
                                           p("Note: The sensitvity analysis tab should be visited in order to produce a plot. 
                                             The numbers here will be the same as those in the meta-analysis tree diagram if no 
                                             studies are excluded from the analysis in the sensitivity analysis tab."),
                                           p("The numbers in brackets represent 95% confidence intervals.")
                                           )
                              
                                )
                              )
                            )),
                 
                 ##########################
                 ### Tab 6 - References ###
                 ##########################
                 
                 # Set up a sixth tab for conducting sensitivity analyses
                 tabPanel("References", h1("References"),
                          br(),
                          p("Bates D, Maechler M, Bolker B, Walker S, Haubo Bojesen Christensen R, Singmann H, et al. lme4: Linear
                            Mixed-Effects Models using 'Eigen' and S4. R package version 1.1-18-1 2018"),
                          p(a("Burke DL, Ensor J, Snell KI, van der Windt D, Riley RD. Guidance for deriving and presenting percentage
                              study weights in meta-analysis of test accuracy studies. Research synthesis methods. 2018 Jun.", 
                              href = "https://onlinelibrary.wiley.com/doi/full/10.1002/jrsm.1283", target = "_blank")),
                          p(a("Chu H, Cole SR. Bivariate meta-analysis of sensitivity and specificity with sparse data: a generalized
                              linear mixed model approach. J Clin Epidemiol. 2006;59(12):1331-2; author reply 2-3",
                              href = "https://www.ncbi.nlm.nih.gov/pubmed/17098577", target = "_blank")),
                          p(a("Harbord R. A unification of models for meta-analysis of diagnostic accuracy studies. Biostatistics. 2007;8:239-251",
                              href = "https://www.ncbi.nlm.nih.gov/pubmed/16698768", target = "_blank")),
                          p(a("Harrison, J.K., Fearon, P., Noel-Storr, A.H., McShane, R., Stott, D.J. and Quinn, T.J., 2015. Informant 
                              Questionnaire on Cognitive Decline in the Elderly (IQCODE) for the diagnosis of dementia within a secondary 
                              care setting. Cochrane database of systematic reviews, (3).", href = "https://www.ncbi.nlm.nih.gov/pubmed/25754745", 
                              target = "_blank")),
                          #p("Kriston L, Holzel L, Weiser A, Berner M, Harter M. Meta-analysis: Are 3 questions enough to detect 
                          #  unhealthy alcohol use? Ann Intern Med. 2008;149:879-88"),
                          p(a("Partlett C, Takwoingi Y. Meta-analysis of test accuracy studies in R: a summary of user-written
                            programs and step-by-step guide to using glmer. Version 1.0. August 2016.",
                            href = "http://methods.cochrane.org/sdt/", target="_blank")),
                          # p("Ritchie  C, Smailagic  N, Noel-Storr  AH, Takwoingi  Y, Flicker  L, Mason  SE, McShane  R.
                          #   Plasma and cerebrospinal fluid amyloid beta for the diagnosis of Alzheimer's disease dementia
                          #   and other dementias in people with mild cognitive impairment (MCI). Cochrane Database of
                          #   Systematic Reviews 2014, Issue 6. Art. No.: CD008782. DOI: 10.1002/14651858.CD008782.pub4."),
                          p(a("Rutter CM, Gatsonis CA. A hierarchical regression approach to meta-analysis of diagnostic test
                              accuracy evaluations. Stat Med. 2001;20(19):2865-84", href = "https://www.ncbi.nlm.nih.gov/pubmed/11568945",
                              target = "_blank")),
                          p(a("QUADAS-2 tool", href = "https://www.bristol.ac.uk/population-health-sciences/projects/quadas/quadas-2/", target = "_blank")),
                          br(),
                           
                          h4("Packages"),
                          p(a("abind", href="https://CRAN.R-project.org/package=abind", target="_blank")),
                          p(a("colorRamps", href = "https://CRAN.R-project.org/package=colorRamps", target="_blank")),
                          p(a("crossralk", href = "https://CRAN.R-project.org/package=crosstalk", target="_blank")),
                          p(a("DT", href = "https://CRAN.R-project.org/package=DT", target="_blank")),
                          p(a("ellipse", href = "https://CRAN.R-project.org/package=ellipse", target="_blank")),
                          p(a("foreach", href = "https://CRAN.R-project.org/package=foreach", target="_blank")),
                          p(a("jsonlite", href = "https://CRAN.R-project.org/package=jsonlite", target="_blank")),
                          p(a("lme4", href = "https://CRAN.R-project.org/package=lme4", target="_blank")),
                          p(a("mada", href = "https://CRAN.R-project.org/package=mada", target="_blank")),
                          p(a("magic", href = "https://CRAN.R-project.org/package=magic", target="_blank")),
                          p(a("Matrix", href = "https://CRAN.R-project.org/package=Matrix", target="_blank")),
                          p(a("msm", href = "https://CRAN.R-project.org/package=msm", target="_blank")),
                          p(a("mvmeta", href = "https://CRAN.R-project.org/package=mvmeta", target="_blank")),
                          p(a("mvtnorm", href = "https://CRAN.R-project.org/package=mvtnorm", target="_blank")),
                          p(a("packrat", href = "https://CRAN.R-project.org/package=packrat", target="_blank")),
                          p(a("plotrix", href = "https://CRAN.R-project.org/package=plotrix", target="_blank")), 
                          p(a("png", href = "https://CRAN.R-project.org/package=png", target="_blank")),
                          p(a("Rgraphviz", href = "https://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html", target="_blank")),
                          p(a("rlang", href = "https://CRAN.R-project.org/package=rlang", target="_blank")),
                          p(a("shiny", href = "https://CRAN.R-project.org/package=shiny", target="_blank")), 
                          p(a("shinyWidgets", href = "https://CRAN.R-project.org/package=shinyWidgets", target="_blank")),
                          p(a("stats", href = "https://CRAN.R-project.org/package=stats", target="_blank")), 
                          p(a("yaml", href = "https://CRAN.R-project.org/package=https://CRAN.R-project.org/package=yaml", target="_blank"))
                          
                      ),
          )
