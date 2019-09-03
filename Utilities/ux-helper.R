# this file contains functions that help build the ux

# sliders ---

# get slider inputs
GetSliderParams <- function(ux, name)
{
  slider <- ux %>%
    filter(`Input Name` == name)
  list(
    inputId = slider$`Input Name`,
    label = slider$`Full Name`,
    min = round(slider$`lower bound`, digits = 3),
    max = round(slider$`upper bound`, digits = 3),
    description = slider$`Description`
  )
}
