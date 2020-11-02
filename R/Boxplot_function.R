## load in libraries
library(dplyr)
library(ggplot2)
library(devtools)
library(roxygen2)

## read in data and save in R package
data <- read.csv("Inpatient_Prospective_Payment_System__IPPS__Provider_Summary_for_the_Top_100_Diagnosis-Related_Groups__DRG__-_FY2011.csv")
save(data, file ='Inpatient_Prospective_Payment_System__IPPS__Provider_Summary_for_the_Top_100_Diagnosis-Related_Groups__DRG__-_FY2011.RData')

data_wrangled <- data %>% mutate(DRG_num = substr(DRG.Definition, 0, 3)) ## wrangle data to have numerical column for DRG code only

#' Boxplot Function
#'
#' This function produces a ggplot boxplot of DRG code versus \code{vary}
#'
#' @param df a dataframe
#' @param vary a string name for variable y in the dataframe df
#'
#' @return A plot with boxplots of each DRG code versus \code{vary}
#' @export
#'
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_boxplot
#'@importFrom ggplot2 labs
#'@importFrom ggplot2 theme
#'
#' @examples
#' DRG code versus average Medicare payments
#' boxplot_function(data, Average.Medicare.Payments)
#'
#'
boxplot_function <- function(df, vary){ ## initiate function with data frame and one variable, y
  df %>% ## pass data frame in function
    ggplot(aes(x = DRG_num, y = {{vary}})) + ## initiate ggplot keeping DRG code as the constant x variable and y is a variable of the function
    geom_boxplot() + ## build boxplot
    labs(title = paste("DRG code versus", as_label(enquo(vary))), ## add title varying by y variable
         x = "DRG code") + ## add x-axis label as DRG code is constant
    theme(axis.text.x = element_text(angle = 45)) ## rotate x-axis labels 45 degrees for better visibility
}
