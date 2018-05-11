# load libraries

library(tidyverse)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(flexdashboard)
library(rhandsontable)
library(visNetwork)
library(RColorBrewer)
library(intergraph)
library(grDevices)
library(htmltools)

# load helper functions

source("Utilities/ux-helper.R")
source("TaxModel.R")

#
# load data -------------------------------------------------------------------

# tax rates schedule ---

# read the tax rate schedule and rename some variables
tax.schedule <- read_csv("Inputs/US_Income_Tax_Rates_2016.csv", col_types = cols())

# list of filing statuses
filing.status <- c("Single", "Head of Household", "Married - Filing Jointly", "Married - Filing Separately")
sel.f.stat <- NULL
disp.eff.tax.rate <- NA
# deterrence strategy ---

# read the audit rate and rename some variables
IRS.table.audit.rates <- read.csv("Inputs/IRS Examination coverage in FY2015.csv", stringsAsFactors = FALSE)
IRS.table.audit.rates <- IRS.table.audit.rates[-1, ] #Removing the NA's

# ux ---

# this file is used to create all the model input ux elements, e.g., sliders
ux <- read_csv("Inputs/tool/ux-inputs.csv", col_types = "ccnnclc")

#
# TODO: this needs to be cleaned up -------------------------------------------

# load data
load("Inputs/tool/default_state.RData")

population.data <- default.state$population.data #Population dataset with status quo tax brackets
g.info <- default.state$g.info
initial.state <- default.state$final.state
aggregated.dyn <- default.state$agg.dyn
config <- default.state$config
#Just to make sure it doesn't run till equilibrium, unless requested explicitly through the tool
config[config$config.vars == 'run.till.equilibrium', 'value'] <- F
gov.dyn <- default.state$gov.dyn
baseline.gov.dyn <- default.state$baseline.gov.dyn
network.data <- default.state$network.data
track.dyn <- default.state$track.dyn

table.audit.rates <- get.effective.audit.rate(population.data, IRS.table.audit.rates, 
                                              current.audit.rate = config %>% get.config.param("audit.rate")) %>%
  mutate(Examination.Coverage = Examination.Coverage / 100)


population.data.file = "Inputs/PN1_population_data.csv"
network.data.file = "Inputs/PN1_network_data.csv"
network.data.manipulation.file = "Inputs/ALP.network.degree.manipulations.Rdata"
tax.refund.proportions.file = "Inputs/Tax Refund Proportions.csv"
g.data <- readRDS(file="Network data/PN1.visNetwork.hideableincome.Rdata")
g.data$nodes$group <- as.character(g.data$nodes$group)

net.degree.info<-readRDS(file=network.data.manipulation.file)
N <- NULL
nn <- NULL
nn.int <- NULL
g <- NULL

table.refund <- NULL
odds.ratios <- NULL
net.plots <- NULL
plots <- NULL
gov.plots <- NULL

tax.gap.at.t <- 0

#To track which config parameters have been enabled, since not all of them are enabled
ux.config.params <- NULL
