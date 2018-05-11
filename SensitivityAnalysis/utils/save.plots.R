
#Save plots

save.plots <- function(plot, plot.name, plots.dir='SensitivityAnalysis/figures/', width = 8, height = 6)
{
  full.name <- paste0(plots.dir, plot.name, '.pdf')
  ggsave(full.name, plot, width = width, height = height)
}