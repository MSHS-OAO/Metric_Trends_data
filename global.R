

# Metric Trends Dashboard

# Import Libraries -------------------------------------------------
suppressMessages({
library(tidyverse)
library(readxl)
library(plotly)
library(scales)
library(reshape2)
library(zoo)
library(gridExtra)
library(ggrepel) 
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
})

# Work directory
#dir <- "C:/Users/aghaer01/Downloads/Metric_Trends_data/"
dir <- "J:/deans/Presidents/HSPI-PM/Operations Planning/Financials/Metric Trends/"

  

# Import data --------------------------------------------------------
# Import the latest aggregated file
repo <- file.info(list.files(path = paste0(dir,"REPO/"), full.names = T, pattern = "Metric_Trends_Data_updated"))
repo_file <- rownames(repo)[which.max(repo$ctime)]
repo <- read.csv(repo_file)


# Get file names in raw data folder
raw_data_list <- file.info(list.files(path = paste0(dir,"Monthly Financial Data/"), full.names = T)) %>%
                                arrange(mtime)

# Select new data
new_metric_data <- basename(rownames(raw_data_list))[!(basename(rownames(raw_data_list)) 
                                                                     %in% repo$Filename)]
new_metric_data <-  "MSHG- Summary Financials June 2023v4.xlsm"

if (length(new_metric_data) > 0) {
  
#Read files in the folder
raw_data_files <- lapply(paste0(dir, "Monthly Financial Data/", new_metric_data), function(x) {
                 data <- read_excel(x, sheet = "Current month") %>%
                                 mutate(Filename = basename(x))
                             })

data <- do.call(rbind.data.frame, raw_data_files )



# Extract the date from file name
data <- data %>% 
  mutate(Filename_v2 = gsub( "v+\\d", "", Filename),
         month = month(mdy(Filename_v2)), year = year(mdy(Filename_v2))) %>%
  arrange(month, year)

data$Filename_v2 <- NULL

# Remove columns with no information
data <- data %>% select(-c("...4", "...5", "...8" , "...9", "...12", "...13",
                           "...16", "...17", "...20", "...21", "...24", "...25",   
                           "...28", "...29",  "...32", "...33", "...36"))


colnames(data) <- c("Metrics", "MSHS-Actual", "MSHS-Budget",  
                    "MSH-Actual", "MSH-Budget", 
                    "MSQ-Actual", "MSQ-Budget",
                    "MSBI-Actual", "MSBI-Budget", 
                    "MSB-Actual", "MSB-Budget", 
                    "MSM-Actual", "MSM-Budget", 
                    "MSW-Actual", "MSW-Budget", 
                    "NYEE-Actual", "NYEE-Budget", 
                    "MSSN-Actual", "MSSN-Budget", 
                    "Filename", "month", "year" )


# Selects required metrics
data <- data %>% 
  filter(Metrics %in% c("Total Hospital Revenue", "Total Hospital Expenses", 
                        "Salaries & Wages", "Contractual & Other Benefits",
                        "Discharges", "Average Length of Stay", "Outpatient",
                        "Other Operating", "340B Pharmacy Program", "CARTS", 
                        "CMI", "Nursing Agency Costs", "Supplies & Expenses"))


# replace all - with NA
data[data == "-"] <- NA


# Convert olumns to numeric
data <- data %>% mutate_at(colnames(data[,2:19]), as.numeric)
  

# Create Expense to Revenue Ratio
exp_rev <- data %>%
  filter(Metrics %in% c("Total Hospital Expenses", "Total Hospital Revenue"))%>%
  select(-c("Filename", "month", "year"))

# change th data from long to wide
exp_rev <-  dcast(melt(exp_rev, id.vars=c("Metrics")), variable ~ Metrics )

exp_rev <- exp_rev %>% 
  mutate( `Total Hospital Expenses`= as.numeric(`Total Hospital Expenses`),
          `Total Hospital Revenue` = as.numeric(`Total Hospital Revenue`),
          `Expense to Revenue Ratio` = round(`Total Hospital Expenses`/`Total Hospital Revenue`, 2))

# change th data from wide to long
exp_rev <- exp_rev %>% 
  select(variable, `Expense to Revenue Ratio`)%>%
  spread(key = variable, value = `Expense to Revenue Ratio`)

# Add other metrics
exp_rev <- exp_rev %>%
  mutate(Metrics= "Expense to Revenue Ratio",
         Filename = unique(data$Filename), 
         month = unique(data$month),
         year = unique(data$year))

# Bind data and exp_rev        
data <- rbind(data, exp_rev)


# Create Salaries and Benefits
salary <- data %>%
  filter(Metrics %in% c("Salaries & Wages", "Contractual & Other Benefits"))%>%
  select(-c("Filename", "month", "year"))

# change th data from long to wide
salary <- dcast(melt(salary, id.vars=c("Metrics")), variable ~ Metrics )

# estimate salaries and benefit
salary <- salary %>% 
  mutate( `Salaries & Wages`= as.numeric(`Salaries & Wages`),
          `Contractual & Other Benefits` = as.numeric(`Contractual & Other Benefits`),
          `Salaries and Benefits` = `Contractual & Other Benefits`+ `Salaries & Wages`)

# change th data from wide to long
salary <- salary %>% 
  select(variable, `Salaries and Benefits`)%>%
  spread(key = variable, value = `Salaries and Benefits`) %>%
  mutate(Metrics= "Salaries and Benefits",
         Filename = unique(data$Filename), 
         month = unique(data$month),
         year = unique(data$year))

# Bind data and salary
data <- rbind(data, salary)


#Create 340B/Other Operating Revenue
operating <- data %>%
  filter(Metrics %in% c("Other Operating", "340B Pharmacy Program"))%>%
  select(-c("Filename", "month", "year")) %>%
  replace(is.na(.), 0) 
  
# change th data from long to wide
operating <- dcast(melt(operating, id.vars=c("Metrics")), variable ~ Metrics )

# estimate 340B/Other Operating Revenue and change the data to long
operating <- operating %>%
  mutate(`340B/Other Operating Revenue`= `340B Pharmacy Program`+ `Other Operating`)%>%
  select(-c("340B Pharmacy Program", "Other Operating"))%>%
   spread(key = variable, value = `340B/Other Operating Revenue`) %>%
   mutate(Metrics= "340B/Other Operating Revenue",
         Filename = unique(data$Filename), 
         month = unique(data$month),
         year = unique(data$year))



# Bind data and salary
data <- rbind(data, operating)  

# Removed unnecessary Metrics
data <- data %>% 
  filter(!(Metrics %in% c("Salaries & Wages", "Contractual & Other Benefits",
                      "340B Pharmacy Program", "Other Operating")))

data <- data %>%
  mutate(Metrics = ifelse(Metrics == "Outpatient", "Outpatient Revenue", Metrics),
         Metrics = ifelse(Metrics == "Average Length of Stay", "ALOS", Metrics))

# subset actual data
actual <- data %>%
  select("Metrics", "Filename", "month", "year", matches("Actual"))%>%
  gather(-c("Metrics", "Filename", "month", "year"), key = Site, value = Actual) %>%
  mutate(Site = gsub("\\-.*","", Site)) %>%
  mutate(Actual = round(as.numeric(Actual), 2))

# subset budget data
budget <- data %>%
  select("Metrics", "Filename", "month", "year", matches("Budget"))%>%
  gather(-c("Metrics", "Filename", "month", "year"), key = Site, value = Budget) %>%
  mutate(Site = gsub("\\-.*","", Site)) %>%
  mutate(Budget = as.numeric(Budget))


# merge actual and budget data
final_data <- left_join(actual, budget %>%
              select("Metrics", "Site", "Budget"), by = c("Metrics", "Site"))



# add the new data to the repo file
new_repo <- rbind(repo, final_data) %>%
  distinct()

rm(actual, budget, final_data, data, salary, exp_rev, operating)


# Save the data
updated_date <- Sys.Date()

write.csv(new_repo, paste0(dir, "REPO/Metric_Trends_Data_updated_", 
                       updated_date, ".csv"), row.names = FALSE  )

} else {
  new_repo <- repo
}


# Define date variable
new_repo <- new_repo %>%
  mutate(month= ifelse(nchar(month) < 2, paste0("0", month), month),
         date = paste0(year, "-", month),
         month= as.numeric(month)) %>%
  arrange(month, year)

# fill na with zero if one of the Actual or Budget is available 
new_repo <- new_repo %>% mutate(Actual = ifelse(!is.na(Budget) & is.na(Actual), 0, Actual),
                                     Budget = ifelse(is.na(Budget) & !is.na(Actual), 0, Budget))



# for these metrics YTD variance = (budget - Actual)/budget
metrics_dif <- c("CARTS", "Nursing Agency Costs", "Salaries and Benefits", 
                 "Supplies & Expenses", "Total Hospital Expenses", "ALOS" )


#define Variance
new_repo <- new_repo %>%
  group_by(Site, Metrics, year) %>%
  mutate(Variance = ifelse(Metrics %in% metrics_dif, 
                                      round(Budget - Actual, 2),
                                      round(Actual - Budget, 2)))


# define YTD variables
new_repo <- new_repo %>%
  group_by(Site, Metrics, year) %>%
  mutate(Actual_YTD = cumsum(Actual),
         Budget_YTD = cumsum(Budget), 
         Variance.From.Budget.YTD = ifelse(Metrics %in% metrics_dif, 
                                           round((Budget_YTD - Actual_YTD)/Budget_YTD, 2),
                                           round((Actual_YTD - Budget_YTD)/Budget_YTD, 2)))


new_repo <- new_repo %>%
  mutate(Variance.From.Budget.YTD = 
           ifelse(Variance.From.Budget.YTD %in% c("Inf", "-Inf", "NaN"), 0, Variance.From.Budget.YTD))




# Import Historical Expense to Revenue Ratio data
Exp_Rev_Ratio <- read_excel(paste0(dir,  "REPO/Exp_Rev_Ratio.xlsx"))

Exp_Rev_Ratio <- Exp_Rev_Ratio %>%
  gather(-c("month", "year"), key = Site, value = Actual) %>%
  mutate(Metrics = "Expense to Revenue Ratio")%>%
  mutate(month= ifelse(nchar(month) < 2, paste0("0", month), month),
         date = paste0(year, "-", month),
         month= as.numeric(month)) %>%
  arrange(month, year)


# Color Theme -----------------------------------------------------------

# Mount Sinai corporate colors
mount_sinai_colors <- c(
  `light pink`   = "#fcc9e9",
  `med pink`     = "#fa93d4",
  `dark pink`    = "#DC298D",
  `light purple` = "#c7c6ef",
  `med purple`   = "#8f8ce0",
  `light blue`   = "#5cd3ff",
  `med blue`     = "#06ABEB",
  `dark blue`    = "#212070",
  `light grey`   = "#b2b3b2",
  `dark grey`    = "#686868",
  `yellow`       = "#E69F00"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of mount_sinai_colors

mount_sinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols)) {
    return(mount_sinai_colors)
  }
  
  mount_sinai_colors[cols]
}


#Create palettes
mount_sinai_palettes <- list(
  `all` = mount_sinai_cols(
    "med blue", "dark pink", "dark blue", "light grey",
    "light blue", "light pink", "light purple",
    "med pink", "med purple", "yellow"
  ),
  `main` = mount_sinai_cols(
   "dark blue", "med blue", "dark pink", "dark grey"
  ),
  `pink` = mount_sinai_cols("light pink", "dark pink"),
  `blue` = mount_sinai_cols("light blue", "dark blue"),
  `grey` = mount_sinai_cols("light grey", "med blue")
)

mount_sinai_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- mount_sinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, interpolate = "spline", ...)
}

#Scale Function for ggplot can be used instead of scale_color_manual
scale_color_mount_sinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- mount_sinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", mount_sinai_palettes, palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}




# Filter choices -----------------------------------------------
hospital_choices <- sort(unique(new_repo$Site))
date_options <- unique(new_repo$date)
mshs_metric_choices <- sort(unique(new_repo$Metrics))

metric_choices <- sort(unique(new_repo$Metrics))
index <- which(metric_choices == "Expense to Revenue Ratio")
metric_choices <- metric_choices[- index]


ratio_date_option <- sort(c( unique(Exp_Rev_Ratio$date), date_options), 
                          decreasing = TRUE)
