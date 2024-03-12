
function(input, output, session) {
  
  google_analytics_header_server(id = "analytics", app_name = "MetaDTA", google_analytics_id = "UA-135597033-2")
  
  HomePageServer(id = "home")
  
  data <- DataPageServer(id = "data")
  
  AnalysisPageServer(id = "analysis", data = data)
  
  SensitivityAnalysisPageServer(id = "sensitivity", data = data)

  PrevalencePageServer(id = "prevalence", data = data)
  
  ReferencesPageServer(id = "references")
}
