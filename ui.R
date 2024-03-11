
# Loading in images for TP, FP, FN, TN
TPimg<-readPNG('./www/TP.png')
TNimg<-readPNG('./www/TN.png')
FPimg<-readPNG('./www/FP.png')
FNimg<-readPNG('./www/FN.png')


#################################################################################################################

# Set up user interface to be a series of tabs using navbarPage
navbarPage(title = "MetaDTA: Diagnostic Test Accuracy Meta-analysis",
                  
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
                 
                
                 #########################
                 ### Tab 1 - Home page ###
                 #########################
                 
    
                 
                 # Start with a home tab
                 tabPanel("Home", 
                          h1("MetaDTA: Diagnostic Test Accuracy Meta-Analysis v2.1.1 (February 2024)"),
                          br(),
                          h4("Version 2.0 is the version as described in the paper:",
                             tags$a(href="https://onlinelibrary.wiley.com/doi/full/10.1002/jrsm.1439", "Patel A, Cooper NJ, Freeman SC, Sutton AJ. Graphical enhancements to summary receiver operating charcateristic plots to facilitate the analysis and reporting of meta-analysis of diagnostic test accuracy data. Research Synthesis Methods 2020, https://doi.org/10.1002/jrsm.1439.
                                 ")),
                          h4("This builds on the previous version as described in the paper:",
                             tags$a(href="https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-019-0724-x", "Freeman SC, Kerby CR, Patel A, Cooper NJ, Quinn T, Sutton AJ. Development of an interactive web-based tool to conduct and interrogate meta-analysis of diagnostic test accuracy studies: MetaDTA. BMC Medical Research Methodology 2019; 19: 81
                                 "),
                             "which can be accessed at", tags$a(href="https://crsu.shinyapps.io/dta_ma_v1/", "MetaDTA version 1.27.")), 
                          h4("If you use MetaDTA please cite these papers."),
                          br(),
                          h3(tags$a(href="https://crsu.shinyapps.io/MetaBayesDTA/",
                                    "MetaBayesDTA is now available!", style="color:#14DBA8")),
                          h5("MetaBayesDTA is an extended, Bayesian version of MetaDTA, which allows users to conduct meta-analysis of test accuracy, with or without assuming a gold standard. Due to its user-friendliness and broad array of features, MetaBayesDTA should appeal to a wide variety of applied researchers, including those who do not have the specific expertise required 
                             to fit such models using statistical software. Furthermore, MetaBayesDTA has many features not available in other apps. For instance, for the bivariate model, one can conduct subgroup analysis and univariate meta-regression. Meanwhile, for the model which does not assume a perfect gold standard, the app can partially account for the fact that different 
                             studies in a meta-analysis often use different reference tests using meta-regression.",
                             tags$br(),
                             style="border-style: groove; border-color: #14DBA8; padding: 20px"),
                          br(),
                          fluidRow(column(5, img(height=600, width=600, src="roc_curve.png")),
                                   column(6, offset=1, align='center', 
                                          h4("MetaDTA 20-minute tutorial as part of ESMARConf2023"),
                                          HTML('<iframe width="840" height="472.5" src="https://www.youtube.com/embed/wCcbU9mKIbE" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'))),
                          br(),
                          h4("Suzanne Freeman, Clareece Nevill, Amit Patel, Nicola Cooper, Terry Quinn, Alex Sutton"),
                          p("For feedback/questions about this app please contact apps@crsu.org.uk"),
                          p("App powered by Rshiny with statistical analyses performed using the package lme4:"),
                          tags$a(href="https://CRAN.R-project.org/package=lme4", "https://CRAN.R-project.org/package=lme4", target="_blank"),
                          br(),
                          p("Codes for this app are available on GitHub:",
                            tags$a(href="https://github.com/CRSU-Apps/MetaDTA", "https://github.com/CRSU-Apps/MetaDTA"),
                          ),
                          br(),
                          p("Download a copy of the MetaDTA User Guide here:"),
                          downloadButton("downloadUG", "Download User Guide"),
                          br(),
                          br(),
                          p("An interactive primer on diagnostic test accuracy can be found at:"),
                          tags$a(href="https://crsu.shinyapps.io/diagprimer/", "https://crsu.shinyapps.io/diagprimer/", target="_blank"),
                          br(),
                          br(),
                          p(tags$b("Latest update:")),
                          p(tags$b("v2.1.1 - February 2024")),
                          p("Updated funding statement and logo"),
                          p(tags$b("v2.1.0 - January 2024")),
                          p("Uploading data made easier - excel files can be uploaded and non-standard characters are accepted."),
                          p("Click", 
                            tags$a(href="https://github.com/CRSU-Apps/MetaDTA/wiki/Changelog", "here", target="_blank"), 
                            "to view a full update history of MetaDTA"),
                          br(),
                          br(),
                          wellPanel(
                            div(style = "display: inline;",
                                img(src = 'funded-by-nihr-logo.png', width = "55%")
                                ),
                            div(style = "display: inline;",
                                img(src = 'CRSU_logo.png', width = "40%")
                                ),
                            div(tags$strong("Funding and Support Acknowledgement:"),
                                tags$p("MetaDTA is part of the Complex Reviews Synthesis Unit (CRSU) suite of evidence synthesis apps.
                                       The development of these apps is currently funded (majority) and overseen by the Evidence Synthesis Group @ CRSU (NIHR153934).
                                       Further details of other funders and support, current and past, can be found ",
                                       tags$a(href = "https://github.com/CRSU-Apps/.github/wiki/Detailed-Funding-Statement", "on our GitHub page"),
                                       ". The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care.",
                                       target = "_blank"),
                                tags$p("More information about the UK NIHR Complex Reviews Synthesis Unit (CRSU) can be found ",
                                       tags$a(href = "https://www.gla.ac.uk/research/az/evidencesynthesis/apps-materials-guidence/", "on our website.", target = "_blank"),
                              )
                            )
                          ),
                          br(),
                          p("THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING 
                            BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
                            NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, 
                            DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
                            OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.")
                          ),
                 
                 
                 
                 
                 #########################
                 ### Tab 2 - Load data ###
                 #########################
                 
                 # Within the load data tab let users select a file to upload, the upload happens in a sidebarPanel on
                 # the left and the mainPanel will show the data once file uploaded. Code to show data is in the server 
                 # section below
                 tabPanel("Load Data",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput(inputId="data", label="Please select a file", buttonLabel="Select", placeholder="No file selected", accept = c(".csv", ".xlsx")),
                              helpText("Default maximum file size is 5MB. Both Excel (.xlsx) and Comma Seperated Value (.csv) files are accepted."),
                              tags$hr(),
                              h4(helpText(tags$strong("File options"))),
                              checkboxInput(inputId = "header", label = "First row as column headings", value = TRUE),
                              br(),
                              radioButtons(inputId = "default", label = h4(helpText(tags$strong("Select example dataset"))),
                                           choices = list("Standard" = 1, "With Quality Assessment" = 2, "With Covariates" = 3, " With Quality assessment and Covariates" = 4), selected = 1),
                              br(),
                              h4(helpText(tags$strong("Download example datasets"))),
                              downloadButton("downloadData1", "Standard Example"),
                              br(),
                              downloadButton("downloadData2", "Quality Assessment Example"),
                              br(),
                              downloadButton("downloadData3", "Covariate Example"),
                              br(),
                              downloadButton("downloadData4", "Quality Assessment and Covariate Example")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("File Upload", 
                                         h3("Please select a file to upload"),
                                         br(),
                                         p("The file should contain at least six columns. Labelling of columns is case sensitive."),
                                         p("The", tags$strong("first"), "column should be labelled", tags$strong("author"), "and contain the name of 
                                           the study author. The author name must be unique for each study."),
                                         p("The", tags$strong("second"), "column should be labelled", tags$strong("year"), "and contain the year of 
                                           publication."),
                                         p("The", tags$strong("third"), "column should be labelled", tags$strong("TP"), "and contain the number of 
                                           patients with a true positive test result."),
                                         p("The", tags$strong("fourth"), "column should be labelled", tags$strong("FN"), "and contain the number of 
                                           patients with a false negative test result."),
                                         p("The", tags$strong("fifth"), "column should be labelled", tags$strong("FP"), "and contain the number of 
                                           patients with a false positive test result."),
                                         p("The", tags$strong("sixth"), "column should be labelled", tags$strong("TN"), "and contain the number of 
                                           patients with a true negative test result."),
                                         br(),
                                         h4("Including quality assessment data (optional)"),
                                         p("To allow the quality assessment results from the QUADAS-2 tool to be incorporated into the plots an additional seven columns are required."),
                                         p("The", tags$strong("seventh"), "column should be labelled", tags$strong("rob_PS"), ", representing the 
                                           risk of bias in terms of the patient selection."),
                                         p("The", tags$strong("eighth"), "column should be labelled", tags$strong("rob_IT"), ", representing the 
                                           risk of bias in terms of the index test."),
                                         p("The", tags$strong("ninth"), "column should be labelled", tags$strong("rob_RS"), ", representing the 
                                           risk of bias in terms of the reference standard."),
                                         p("The", tags$strong("tenth"), "column should be labelled", tags$strong("rob_FT"), ", representing the 
                                           risk of bias in terms of the flow and timing."),
                                         p("The", tags$strong("eleventh"), "column should be labelled", tags$strong("ac_PS"), ", representing the 
                                           applicability concerns in terms of the patient selection."),
                                         p("The", tags$strong("twelfth"), "column should be labelled", tags$strong("ac_IT"), ", representing the 
                                           applicability concerns in terms of the index test."),
                                         p("The", tags$strong("thirteenth"), "column should be labelled", tags$strong("ac_RS"), ", representing the 
                                           applicability concerns in terms of the reference standard."),
                                         p("These columns should contain the numbers", tags$strong("1, 2 or 3"), "which represent", tags$strong("low, high or unclear"),
                                           "risk of bias/applicability concerncs respectively."),
                                         br(),
                                         p("For information about the QUADAS-2 tool and how to use it please visit:"),
                                         tags$a(href="https://www.bristol.ac.uk/population-health-sciences/projects/quadas/quadas-2/", "https://www.bristol.ac.uk/population-health-sciences/projects/quadas/quadas-2/", target="_blank"),
                                         br(),
                                         br(),
                                         h4("Including covariates (optional)"), 
                                         p("If any covariates are to be added to the file they should be included as the last columns in the file. If quality 
                                           assessment data is not included in the file the covariates should be entered starting at the", tags$strong("seventh"), "column. If quality assessment
                                           data is included in the file the covariate data should be entered starting at the", tags$strong("fourteenth"), "column. Multiple covariates can be entered."),
                                         br(),
                                         p("The default dataset, pre-loaded on the 'Data for Analysis' tab will be used for analysis if no file is 
                                            selected. The 'Data for Analysis' tab will automatically update once a file is successfully loaded."),
                                         p("The default datasets can be downloaded using the buttons in the sidebar and used as templates to enter your own data."),
                                         #p("In the case of zero's in the dataset a continuity correction is used to allow the calculation of confidence 
                                         #   intervals for individual studies which can then be displayed on the SROC curve. No continuity corrections are used 
                                         #   for the meta-analysis."),
                                br(),
                                h4("Sensitivity analysis"),
                                p("To ensure the correct studies are excluded from sensitivity analyses please ensure that study data rows are ordered 
                                  by the 'author' column alphabetically from A to Z prior to uploading to MetaDTA (Excel can do this easily).")),
                    tabPanel("Example datasets",
                                         br(),
                                         p("The default dataset uses data from a systematic review investigating the accuracy of an informant-based questionnaire, for detection of all cause
                                           dementia in adults. The dataset consists of thirteen studies assessing the use of the IQCODE (Informant Questionnaire
                                           on Cognitive Decline in the Elderly) tool for identifying adults with dementia within a secondary care setting."),
                                         p("The IQCODE tool contains a number of questions which are scored on a five point scale. The IQCODE tool has a number of 
                                           different variants, depending on how many questions are asked. The questions are based on the performance of everyday
                                           tasks related to cognitive function. These are then rated on a scale of 1-5. The final score is an average score for each
                                           question. The IQCODE tool is only a screening tool and does not offer a definitive diagnosis of dementia."),
                                         p("Under the 'Select example dataset' option there are four different datasets to choose from. The default is the 'Standard' dataset,
                                           which includes the author and year of each study along with the true positives (TP), false positives (FP), false negatives (FN) and 
                                           true negatives (TN). The other options add data onto this 'Standard' dataset and highlight how datasets with quality assessment scores and/or
                                           covariates should be displayed."),
                                         p("With this dataset there are three different covariates. The first being the country in which each individual study was conducted.
                                           The second is the threshold used in each individual study. In this case if an individuals final score was higher than the threshold the individual
                                           was classified as having dementia and would require further diagnosis. The final covariate is labelled as 'IQCODE' and indicates 
                                           which variant of the tool was used in each individual study. The variants are identified by the number of questions used in the 
                                           questionnaire. There are three different variants the 16-item, 26-item and 32-item.")
                                         ),
                    tabPanel("Data for Analysis", uiOutput("tb"))
                                         )
                                         )
                                         )),
                 
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
