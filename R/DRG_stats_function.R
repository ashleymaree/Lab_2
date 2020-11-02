## load in libraries
library(dplyr)
library(pastecs)
library(stats)

## read in data
data <- read.csv("Inpatient_Prospective_Payment_System__IPPS__Provider_Summary_for_the_Top_100_Diagnosis-Related_Groups__DRG__-_FY2011.csv")

#' Statistics of DRG Codes Function
#'
#' This function produces a table of DRG codes and the corresponding test statistic of interest (ie, mean, median) of average Medicare payments
#'
#' @param df a dataframe
#' @param statistic a test statistic (ie, mean, median)
#'
#' @return A table with DRG code and \code{statistic} of average Medicare payments
#' @export
#'
#'@importFrom dplyr group_by
#'@importFrom dplyr summarize
#'@importFrom dplyr across
#'@importFrom knitr kable
#'
#' @examples
#' DRG codes by mean average Medicare payments
#' DRG_stats_function(data, mean)
#'
#'
DRG_stats_function <- function(df, statistic){ ## initiate function with a data frame and a test statistic (ie, mean, median)
  col_name <- deparse(substitute(statistic)) ## re-name column name to the name of the test statistic of interest
  df %>% ## pass data frame
    group_by(DRG_num) %>% ## group data frame by DRG codes
    summarize(across(Average.Medicare.Payments, statistic)) %>% ## summarize by the test statistic of interest for each DRG code
    knitr::kable(col.names = c('DRG code', paste0(col_name, ' of average medicare payments'))) ## add column names for the table
}
