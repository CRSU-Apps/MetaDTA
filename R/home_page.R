
#' Module UI for the home page.
#' 
#' @param id ID of the module
#' @return Div for the home page
HomePageUi <- function(id) {
  div(
    h1("MetaDTA: Diagnostic Test Accuracy Meta-Analysis v2.1.3 (August 2024)"),
    br(),
    h4(
      "Version 2.0 is the version as described in the paper:",
       tags$a(
         href = "https://onlinelibrary.wiley.com/doi/full/10.1002/jrsm.1439",
         "Patel A, Cooper NJ, Freeman SC, Sutton AJ. Graphical enhancements to summary receiver operating charcateristic plots to facilitate the analysis and reporting of meta-analysis of diagnostic test accuracy data. Research Synthesis Methods 2020, https://doi.org/10.1002/jrsm.1439.",
         target = "_blank"
        )
    ),
    h4(
      "This builds on the previous version as described in the paper:",
      tags$a(
        href = "https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-019-0724-x",
        "Freeman SC, Kerby CR, Patel A, Cooper NJ, Quinn T, Sutton AJ. Development of an interactive web-based tool to conduct and interrogate meta-analysis of diagnostic test accuracy studies: MetaDTA. BMC Medical Research Methodology 2019; 19: 81",
        target = "_blank"
      ),
      "which can be accessed at",
      tags$a(href = "https://crsu.shinyapps.io/dta_ma_v1/", "MetaDTA version 1.27.",
      target = "_blank")
    ), 
    h4("If you use MetaDTA please cite these papers."),
    br(),
    h3(
      tags$a(
        href = "https://crsu.shinyapps.io/MetaBayesDTA/",
        "MetaBayesDTA is now available!",
        style = "color:#14DBA8",
        target = "_blank"
      )
    ),
    h5(
      "MetaBayesDTA is an extended, Bayesian version of MetaDTA, which allows users to conduct meta-analysis of test accuracy, with or without assuming a gold standard. Due to its user-friendliness and broad array of features, MetaBayesDTA should appeal to a wide variety of applied researchers, including those who do not have the specific expertise required",
      "to fit such models using statistical software. Furthermore, MetaBayesDTA has many features not available in other apps. For instance, for the bivariate model, one can conduct subgroup analysis and univariate meta-regression. Meanwhile, for the model which does not assume a perfect gold standard, the app can partially account for the fact that different",
      "studies in a meta-analysis often use different reference tests using meta-regression.",
       style = "border-style: groove; border-color: #14DBA8; padding: 20px"
    ),
    br(),
    fluidRow(
      column(
        width = 5,
        img(height = 600, width = 600, src = "roc_curve.png")
      ),
      column(
        width = 6,
        offset = 1,
        align = 'center', 
        h4("MetaDTA 20-minute tutorial as part of ESMARConf2023"),
        HTML('<iframe width="840" height="472.5" src="https://www.youtube.com/embed/wCcbU9mKIbE" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>')
      )
    ),
    br(),
    h4("Suzanne Freeman, Clareece Nevill, Tom Morris, Naomi Bradbury, Janion Nevill, Ryan Field, Amit Patel, Nicola Cooper, Terry Quinn, Alex Sutton"),
    p("For feedback/questions about this app please contact apps@crsu.org.uk"),
    p("App powered by Rshiny with statistical analyses performed using the package lme4:"),
    tags$a(
      href = "https://CRAN.R-project.org/package=lme4",
      "https://CRAN.R-project.org/package=lme4",
      target = "_blank"
    ),
    br(),
    p(
      "Codes for this app are available on GitHub:",
      tags$a(href = "https://github.com/CRSU-Apps/MetaDTA", "https://github.com/CRSU-Apps/MetaDTA",
      target = "_blank"),
    ),
    br(),
    p("Download a copy of the MetaDTA User Guide here:"),
    downloadButton("downloadUG", "Download User Guide"),
    br(),
    br(),
    p("An interactive primer on diagnostic test accuracy can be found at:"),
    tags$a(
      href = "https://crsu.shinyapps.io/diagprimer/",
      "https://crsu.shinyapps.io/diagprimer/",
      target = "_blank"
    ),
    br(),
    br(),
    p(tags$b("Latest update:")),
    p(tags$b("v2.1.3 - August 2024")),
    p("Fixed a bug which prevented the risk of bias piecharts from diplaying under the SROC plots."),
    p(tags$b("v2.1.2 - June 2024")),
    p("Added an error message when the model doesn't converge, and removed most of the output in this case."),
    p("In previous versions, on rare occasions the model may not have converged without any warning being displayed to the user."),
    p(tags$b("v2.1.1 - February 2024")),
    p("Updated funding statement and logo"),
    p(
      "A full update history of MetaDTA can be found on", 
      tags$a(
        href = "https://github.com/CRSU-Apps/MetaDTA/wiki/Changelog",
        "our GitHub repository.",
        target = "_blank")
    ),
    br(),
    br(),
    wellPanel(
      div(
        style = "display: inline;",
        img(src = 'funded-by-nihr-logo.png', width = "55%")
      ),
      div(
        style = "display: inline;",
        img(src = 'CRSU_logo.png', width = "40%")
      ),
      div(
        tags$strong("Funding and Support Acknowledgement:"),
        tags$p(
          "MetaDTA is part of the Complex Reviews Synthesis Unit (CRSU) suite of evidence synthesis apps.",
          "The development of these apps is currently funded (majority) and overseen by the Evidence Synthesis Group @ CRSU (NIHR153934).",
          "Further details of other funders and support, current and past, can be found ",
          tags$a(
            href = "https://github.com/CRSU-Apps/.github/wiki/Detailed-Funding-Statement",
            "on our GitHub page",
            target = "_blank"
          ),
          ". The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care."
        ),
        tags$p(
          "More information about the UK NIHR Complex Reviews Synthesis Unit (CRSU) can be found ",
          tags$a(
            href = "https://www.gla.ac.uk/research/az/evidencesynthesis/apps-materials-guidence/",
            "on our website.",
            target = "_blank"
          ),
        )
      )
    ),
    br(),
    p(
      "THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING ",
      "BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND ",
      "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, ",
      "DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, ",
      "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."
    )
  )
}

#' Module server for the home page.
#' 
#' @param id ID of the module
HomePageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #Download User Guide
    output$downloadUG <- downloadHandler(
      # Specify the file name 
      filename = "MetaDTA User Guide v1_0.pdf",
      content = function(file) {
        file.copy("www/MetaDTA User Guide v1_0.pdf", file)
      }
    )
    
  })
}
