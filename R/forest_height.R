# Function to decide pixel height of forest plots based on the number of studies
# 
# param studies
# A positive number (an int). The number of studies to be plotted.
# return 
# A number 
# Height in pixels (for png plots)
#
# author
# NVB

calculate_forest_height_pixel <- function(studies) {
  ifelse(studies <=20, 400, 400 + 15*(studies-20))
}

# Function to decide inch height of pdf forest plots based on the number of studies
# 
# param studies
# A positive number (an int). The number of studies to be plotted.
# return 
# A number 
# Height in inches for pdf plots
#
# author
# NVB

calculate_forest_height_pdf <- function(studies) {
  ifelse(studies <=25, 6, 6 + 0.2*(studies-25))
}
