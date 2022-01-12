
#######################################################################################################
##                                                                                                   ##
##  Script name: ess7_loneliness.R                                                                   ##
##  Purpose of script: Find the proportion of people who experience loneliness in Estonia from       ##
##                     European Social Survey 2014.                                                  ##
##                                                                                                   ##
##  Author: Mart Roben                                                                               ##
##  Date Created: 13. Jan 2022                                                                       ##
##                                                                                                   ##
##  Copyright: MIT License                                                                           ##
##  https://github.com/martroben/ess7_loneliness                                                     ##
##                                                                                                   ##
##  Contact: fb.com/martroben                                                                        ##
##                                                                                                   ##
#######################################################################################################


#################
# Load packages #
#################

if(!require("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load("magrittr",
               "tibble",
               "dplyr",
               "tidyr",
               "stringr",
               "purrr",
               "httr",
               "haven")



##########
# Inputs #
##########

# register at https://www.europeansocialsurvey.org/user/new
login_email <- "mart.roben@gmail.com"



################
# Data sources #
################

ess_login_url <- "https://www.europeansocialsurvey.org/user/login"
ess_data_url <- "https://www.europeansocialsurvey.org/file/download?f=ESS7e02_2.spss.zip&c=&y=2014"
sav_file_name_in_archive <- "ESS7e02_2.sav"



##################
# Importing data #
##################

httr::POST(ess_login_url, body = list(u = login_email))
ess_data_spss <- httr::GET(url = ess_data_url)

tempfile_name <- tempfile()
writeBin(httr::content(ess_data_spss, as = "raw"), tempfile_name)
ess_data <- unz(tempfile_name, sav_file_name_in_archive) %>% haven::read_sav()
file.remove(tempfile_name)

ess_data_EE <- ess_data %>% dplyr::filter(cntry == "EE")



######################
# Questionnaire info #
######################

questions <- ess_data %>% purrr::map_dfr(~attributes(.x)$label) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "var", values_to = "question")

# Question associated with the variable
ess_data_EE$fltlnl %>% attr("label")
# Felt lonely, how often past week

# Answer key
ess_data_EE$fltlnl %>% attr("labels")
# 1     None or almost none of the time
# 2                    Some of the time
# 3                    Most of the time
# 4       All or almost all of the time
# 7                             Refusal
# 8                          Don't know
# 9                           No answer



#########################
# Loneliness statistics #
#########################

# Treat loneliness as a Bernoully random variable: I(felt lonely most of the time or all of the time last week)
ess_data_EE_only_filled <- ess_data_EE %>%
  dplyr::filter(fltlnl %in% 1:4) %>%
  dplyr::mutate(lonely = fltlnl %in% 3:4,
                normalized_weight = pspwght / sum(pspwght),
                weighted_lonely = lonely * normalized_weight)

loneliness_prevalence <- sum(ess_data_EE_only_filled$weighted_lonely)

# Std error of weighted mean (https://en.wikipedia.org/wiki/Weighted_arithmetic_mean#Mathematical_definition)
loneliness_prevalence_std_err <- sd(ess_data_EE_only_filled$lonely) * sqrt(sum(ess_data_EE_only_filled$normalized_weight^2))

confidence_level <- 0.95
margin_of_error <- qnorm(1 - (1 - confidence_level) / 2) * loneliness_prevalence_std_err



##########
# Result #
##########

stringr::str_glue("{round(loneliness_prevalence, 3)} ± {round(margin_of_error, 3)}")
# 0.073 ± 0.012
