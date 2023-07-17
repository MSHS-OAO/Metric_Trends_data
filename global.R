
# Metric Trends Dashboard

# Import Libraries -------------------------------------------------
suppressMessages({
library(tidyverse)
library(readxl)
library(plotly)
library(zoo)
library(gridExtra)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
})

# Work directory
dir <- "C:/Users/aghaer01/Downloads/Metric_Trends_data/"


# Import data --------------------------------------------------------
# Import the latest aggregated file
repo <- file.info(list.files(path = paste0(dir,"Repo/"), full.names = T))
repo_file <- rownames(repo)[which.max(repo$ctime)]
repo <- read.csv(repo_file)


# Get file names in raw data folder
raw_data_list <- file.info(list.files(path = paste0(dir,"Raw Data/"), full.names = T)) %>%
                                arrange(mtime)

# Select new data
new_metric_data <- basename(rownames(raw_data_list))[!(basename(rownames(raw_data_list)) 
                                                                     %in% repo$Filename)]

if (length(new_metric_data) > 0) {
  
#Read files in the folder
raw_data_files <- lapply(paste0(dir, "Raw Data/", new_metric_data), function(x) {
                 data <- read_excel(x, sheet = "Hospital Financials") %>%
                                 mutate(Filename = basename(x))
                             })

data <- do.call(rbind.data.frame, raw_data_files )


# Extract the date from file name
data <- data %>% 
  rename(Metrics = colnames(data[,1]))   %>%
  mutate(month = month(mdy(Filename)), year = year(mdy(Filename))) %>%
  arrange(month, year)

# Remove columns with no information
data <- data %>% select(-c("...5", "...9", "...13","...17", 
                           "...21",  "...25","...29", "...33" ))


colnames(data) <- c("Metrics", "MSHS-ActualCM", "MSHS-BudgetCM", "MSHS-Variance", 
                    "MSH-ActualCM", "MSH-BudgetCM", "MSH-Variance",
                    "MSQ-ActualCM", "MSQ-BudgetCM", "MSQ-Variance",
                    "MSBI-ActualCM", "MSBI-BudgetCM", "MSBI-Variance",
                    "MSB-ActualCM", "MSB-BudgetCM", "MSB-Variance",
                    "MSM-ActualCM", "MSM-BudgetCM", "MSM-Variance",
                    "MSW-ActualCM", "MSW-BudgetCM", "MSW-Variance",
                    "NYEE-ActualCM", "NYEE-BudgetCM", "NYEE-Variance",
                    "MSSN-ActualCM", "MSSN-BudgetCM", "MSSN-Variance",
                    "Filename", "month", "year" )

# define the first rows with useful info
var <- which(data$Metrics == "Hospital Beds in Use")
data <- data[var: nrow(data), ]

# keep the first CMI
cmi <- which(data$Metrics == "CMI")

if (length(cmi)> 1){
  index <- cmi[-1]
  data <- data[-c(index),]
}


# keep the first ALOS
los <- which(data$Metrics == "Average Length of Stay")

if (length(los)> 1){
    index <- los[-1]
     data <- data[-c(index),]
}

# remove rows with all na
data$count_na <- rowSums(is.na(data))

#remove rows with all missing values
data <- data %>% filter(count_na !=19)

data$count_na <- NULL

# replace all - with NA
data[data == "-"] <- NA


# Convert olumns to numeric
data <- data %>% mutate_at(colnames(data[,2:28]), as.numeric)


# Create Expense to Revenue Ratio
exp_rev <- data %>%
  filter(Metrics %in% c("Total Hospital Expenses", "Total Hospital Revenue"))%>%
  select(-c("Filename", "month", "year"))

exp_rev <- data.frame(t(exp_rev))
exp_rev <- janitor::row_to_names(exp_rev, 1, remove_rows_above = F)

exp_rev <- exp_rev %>% 
  mutate( `Total Hospital Expenses`= as.numeric(`Total Hospital Expenses`),
          `Total Hospital Revenue` = as.numeric(`Total Hospital Revenue`),
          `Expense to Revenue Ratio` = round(`Total Hospital Expenses`/`Total Hospital Revenue`, 2))%>%
  mutate(Site = row.names(exp_rev))

exp_rev <- exp_rev %>% 
  select(Site, `Expense to Revenue Ratio`)%>%
  spread(key = Site, value = `Expense to Revenue Ratio`)

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

salary <- data.frame(t(salary))
salary <- janitor::row_to_names(salary, 1, remove_rows_above = F)

salary <- salary %>% 
  mutate( `Salaries & Wages`= as.numeric(`Salaries & Wages`),
          `Contractual & Other Benefits` = as.numeric(`Contractual & Other Benefits`),
          `Salaries and Benefits` = `Contractual & Other Benefits`+ `Salaries & Wages`)%>%
  mutate(Site = row.names(salary))

salary <- salary %>% 
  select(Site, `Salaries and Benefits`)%>%
  spread(key = Site, value = `Salaries and Benefits`) %>%
  mutate(Metrics= "Salaries and Benefits",
         Filename = unique(data$Filename), 
         month = unique(data$month),
         year = unique(data$year))

# Bind data and exp_rev        
data <- rbind(data, salary)



# Select required Metrics
data <- data %>% 
  filter(Metrics %in% c("Expense to Revenue Ratio", "Total Hospital Revenue", 
                        "Total Hospital Expenses", "Salaries and Benefits",
                        "Discharges", "Average Length of Stay", "Outpatient",
                      "Other Operating", "CARTS", "CMI", "Nursing Agency Costs", 
                                      "Supplies & Expenses"))

data <- data %>%
  mutate(Metrics = ifelse(Metrics == "Outpatient", "Outpatient Revenue", Metrics),
         Metrics = ifelse(Metrics == "Average Length of Stay", "ALOS", Metrics),
         Metrics = ifelse(Metrics == "Other Operating",
                          "340B/Other Operating Revenue", Metrics))

# subset actual data
actual <- data %>%
  select("Metrics", "Filename", "month", "year", matches("ActualCM"))%>%
  gather(-c("Metrics", "Filename", "month", "year"), key = Site, value = Actual) %>%
  mutate(Site = gsub("\\-.*","", Site)) %>%
  mutate(Actual = round(as.numeric(Actual), 2))

# subset budget data
budget <- data %>%
  select("Metrics", "Filename", "month", "year", matches("BudgetCM"))%>%
  gather(-c("Metrics", "Filename", "month", "year"), key = Site, value = Budget) %>%
  mutate(Site = gsub("\\-.*","", Site)) %>%
  mutate(Budget = as.numeric(Budget))

# subset variance data
Variance <- data %>%
  select("Metrics", "Filename", "month", "year", matches("Variance"))%>%
  gather(-c("Metrics", "Filename", "month", "year"), key = Site, value = Variance) %>%
  mutate(Site = gsub("\\-.*","", Site)) %>%
  mutate(Variance = round(as.numeric(Variance), 2))

# merge actual and budget data
final_data <- left_join(actual, budget %>%
              select("Metrics", "Site", "Budget"), by = c("Metrics", "Site"))

# merge actual and final data and variance
final_data <- left_join(final_data , Variance %>%
                          select("Metrics", "Site", "Variance"), by = c("Metrics", "Site"))

final_data <- final_data %>% mutate(Variance= ifelse(is.na(Variance), Budget - Actual , Variance))

# add the new data to the repo file
new_repo <- rbind(repo, final_data) %>%
  distinct()

rm(actual, budget, Variance, final_data, data, salary, exp_rev)

# Save the data
updated_date <- Sys.Date()

write.csv(new_repo, paste0(dir, "Repo/Metric_Trends_Data_updated_", 
                       updated_date, ".csv"), row.names = FALSE  )

} else {
  new_repo <- repo
}


new_repo <- new_repo %>% mutate(Actual = ifelse(!is.na(Budget) & is.na(Actual), 0, Actual),
                                Budget = ifelse(is.na(Budget) & !is.na(Actual), 0, Budget))
                                


new_repo <- new_repo %>%
  mutate(month= ifelse(nchar(month) < 2, paste0("0", month), month),
         date = paste0(year, "-", month),
         month= as.numeric(month)) %>%
  arrange(month, year)%>%
  filter(!is.na(Actual)) 


# for these metrics YTD variance = (budget - Actual)/budget
metrics_dif <- c("CARTS", "Nursing Agency Costs", "Salaries and Benefits", 
                 "Supplies & Expenses", "Total Hospital Expenses", "ALOS" )


# define YTD variables
new_repo <- new_repo %>% 
  group_by(Site, Metrics, year) %>%
  mutate( Actual_YTD = cumsum(Actual),
          Budget_YTD = cumsum(Budget), 
          Variance.From.Budget.YTD = ifelse(Metrics %in% metrics_dif, 
                                            round((Budget_YTD - Actual_YTD )/Budget_YTD, 2),
                                            round((Actual_YTD - Budget_YTD)/Budget_YTD, 2)))

new_repo <- new_repo %>%
  mutate(Variance.From.Budget.YTD = 
           ifelse(Variance.From.Budget.YTD %in% c("Inf", "-Inf", "NaN"), 0, Variance.From.Budget.YTD))
  


# new_repo <- new_repo %>% 
#   group_by(Site, Metrics) %>%
#   mutate(Variance.From.Budget= round(Actual - Budget, 2))

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
mshs_date_options <- unique(new_repo$date)
mshs_metric_choices <- sort(unique(new_repo$Metrics))

metric_choices <- sort(unique(new_repo$Metrics))
index <- which(metric_choices == "Expense to Revenue Ratio")
metric_choices <- metric_choices[- index]
date_options <- unique(new_repo$date)

