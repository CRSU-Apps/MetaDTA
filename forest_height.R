# Function to decide height of forest plots based on the number of studies
# and the output format
# 
# param studies
# A number (an int). The number of studies to be plotted.
# param tab 
# A string representing the way the plot height is measured
# "pixel" for png files or plots rendered in the app 
# "inch" for pdf files
#
# return 
# A number 
# Height in pixels for png plots
# Height in inches for pdf plots
#
# author
# NVB

forest_height <- function(studies, measure){
  if (measure == "pixel") {
    ifelse(studies <=20, 400, 20*studies)
  } else if (measure == "inch") {
    ifelse(studies <=20, 6, 6 + 0.2*(studies-20))
  }
}