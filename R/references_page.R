
#' Module UI for the references page.
#' 
#' @param id ID of the module
#' @return Div for the references page
ReferencesPageUi <- function(id) {
  div(
    h1("References"),
    br(),
    p(
      "Bates D, Maechler M, Bolker B, Walker S, Haubo Bojesen Christensen R, Singmann H, et al. lme4: Linear",
      "Mixed-Effects Models using 'Eigen' and S4. R package version 1.1-18-1 2018"
    ),
    p(
      a(
        "Burke DL, Ensor J, Snell KI, van der Windt D, Riley RD. Guidance for deriving and presenting percentage",
        "study weights in meta-analysis of test accuracy studies. Research synthesis methods. 2018 Jun.", 
        href = "https://onlinelibrary.wiley.com/doi/full/10.1002/jrsm.1283",
        target = "_blank"
      )
    ),
    p(
      a(
        "Chu H, Cole SR. Bivariate meta-analysis of sensitivity and specificity with sparse data: a generalized",
        "linear mixed model approach. J Clin Epidemiol. 2006;59(12):1331-2; author reply 2-3",
        href = "https://www.ncbi.nlm.nih.gov/pubmed/17098577",
        target = "_blank"
      )
    ),
    p(
      a(
        "Harbord R. A unification of models for meta-analysis of diagnostic accuracy studies. Biostatistics. 2007;8:239-251",
        href = "https://www.ncbi.nlm.nih.gov/pubmed/16698768",
        target = "_blank"
      )
    ),
    p(
      a(
        "Harrison, J.K., Fearon, P., Noel-Storr, A.H., McShane, R., Stott, D.J. and Quinn, T.J., 2015. Informant",
        "Questionnaire on Cognitive Decline in the Elderly (IQCODE) for the diagnosis of dementia within a secondary",
        "setting. Cochrane database of systematic reviews, (3).",
        href = "https://www.ncbi.nlm.nih.gov/pubmed/25754745", 
        target = "_blank"
      )
    ),
    p(
      a(
        "Partlett C, Takwoingi Y. Meta-analysis of test accuracy studies in R: a summary of user-written",
        "programs and step-by-step guide to using glmer. Version 1.0. August 2016.",
        href = "http://methods.cochrane.org/sdt/",
        target= "_blank"
      )
    ),
    p(
      a(
        "Rutter CM, Gatsonis CA. A hierarchical regression approach to meta-analysis of diagnostic test",
        "accuracy evaluations. Stat Med. 2001;20(19):2865-84",
        href = "https://www.ncbi.nlm.nih.gov/pubmed/11568945",
        target = "_blank"
      )
    ),
    p(
      a(
        "QUADAS-2 tool",
        href = "https://www.bristol.ac.uk/population-health-sciences/projects/quadas/quadas-2/",
        target = "_blank"
      )
    ),
    br(),
    
    h4("Packages"),
    p(a("abind", href = "https://CRAN.R-project.org/package=abind", target = "_blank")),
    p(a("colorRamps", href = "https://CRAN.R-project.org/package=colorRamps", target = "_blank")),
    p(a("crossralk", href = "https://CRAN.R-project.org/package=crosstalk", target = "_blank")),
    p(a("DT", href = "https://CRAN.R-project.org/package=DT", target = "_blank")),
    p(a("ellipse", href = "https://CRAN.R-project.org/package=ellipse", target = "_blank")),
    p(a("foreach", href = "https://CRAN.R-project.org/package=foreach", target = "_blank")),
    p(a("jsonlite", href = "https://CRAN.R-project.org/package=jsonlite", target = "_blank")),
    p(a("lme4", href = "https://CRAN.R-project.org/package=lme4", target = "_blank")),
    p(a("mada", href = "https://CRAN.R-project.org/package=mada", target = "_blank")),
    p(a("magic", href = "https://CRAN.R-project.org/package=magic", target = "_blank")),
    p(a("Matrix", href = "https://CRAN.R-project.org/package=Matrix", target = "_blank")),
    p(a("msm", href = "https://CRAN.R-project.org/package=msm", target = "_blank")),
    p(a("mvmeta", href = "https://CRAN.R-project.org/package=mvmeta", target = "_blank")),
    p(a("mvtnorm", href = "https://CRAN.R-project.org/package=mvtnorm", target = "_blank")),
    p(a("packrat", href = "https://CRAN.R-project.org/package=packrat", target = "_blank")),
    p(a("plotrix", href = "https://CRAN.R-project.org/package=plotrix", target = "_blank")), 
    p(a("png", href = "https://CRAN.R-project.org/package=png", target = "_blank")),
    p(a("Rgraphviz", href = "https://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html", target = "_blank")),
    p(a("rlang", href = "https://CRAN.R-project.org/package=rlang", target = "_blank")),
    p(a("shiny", href = "https://CRAN.R-project.org/package=shiny", target = "_blank")), 
    p(a("shinyWidgets", href = "https://CRAN.R-project.org/package=shinyWidgets", target = "_blank")),
    p(a("stats", href = "https://CRAN.R-project.org/package=stats", target = "_blank")), 
    p(a("yaml", href = "https://CRAN.R-project.org/package=https://CRAN.R-project.org/package=yaml", target = "_blank"))
  )
}

#' Module server for the references page.
#' 
#' @param id ID of the module
ReferencesPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Do nothing
  })
}
