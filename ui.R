
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
  tabPanel(
    title = "Meta-Analysis",
    AnalysisPageUi(id = "analysis")
  ),
  tabPanel(
    title = "Sensitivity Analysis",
    SensitivityAnalysisPageUi(id = "sensitivity")
  ),
  tabPanel(
    title = "Prevalence",
    PrevalencePageUi(id = "prevalence")
  ),
  tabPanel(
    title = "References",
    ReferencesPageUi(id = "references")
  )
)
