
# Metric Trends Dashboard

# Import Libraries
library(tidyverse)
library(readxl)


# Work directory
dir <- "C:/Users/aghaer01/Downloads/Metrics Trends Data/"


# Import data --------------------------------------------------------
# Import the latest aggregated file
repo <- file.info(list.files(path = paste0(dir,"/Repo"), full.names = T))
repo_file <- rownames(repo)[which.max(repo$ctime)]
repo <- read.csv(repo_file)


# Get file names in raw data folder
raw_data_list <- file.info(list.files(path = paste0(dir,"Raw Data/"), full.names = T)) %>%
                                arrange(mtime)

# Select new data
new_metric_data <- basename(rownames(raw_data_list))[!(basename(rownames(raw_data_list)) 
                                                                     %in% repo$Filename)]

#Read files in the folder
raw_data_files <- lapply(new_metric_data, function(x) {
                 data <- read_excel(x, sheet = "Hospital Financials") %>%
                                 mutate(Filename = basename(x))%>%
                                 rename(Metrics = "...1")
                             })

data <- do.call(rbind.data.frame, raw_data_files )

# Extract the date from file name
data <- data %>% mutate(date = sub(".*Trends ", "", Filename),
                                date = sub(".xlsm.*", "", date))

# Remove columns with no information
data <- data %>% select(-c("...4", "...5", "...8", "...9", "...12", "...13",
                           "...16", "...17", "...20", "...21", "...24", "...25",
                           "...28", "...29", "...32", "...33", "...36" ))

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

# replace all - with NA
data[data == "-"] <- NA


# Select required Metrics
data <- data %>% 
  filter(Metrics %in% c("Total Hospital Revenue", "Total Hospital Expenses",
                      "Discharges", "Average Length of Stay", "Outpatient",
                      "Other Operating",  "Salaries & Wages", "CARTS", "CMI",
                      "Nursing Agency Costs", "Contractual & Other Benefits",
                      "Supplies & Expenses"))

data <- data %>%
mutate(Metrics = ifelse(Metrics == "Outpatient", "Outpatient Revenue", Metrics),
       Metrics = ifelse(Metrics == "Average Length of Stay", "ALOS", Metrics),
       Metrics = ifelse(Metrics == "Other Operating",
                        "340B/Other Operating Revenue", Metrics))


# subset actual data
actual <- data %>%
  select("Metrics", "Filename", "date", matches("ActualCM"))%>%
  gather(-c("Metrics", "Filename", "date"), key = Site, value = Actual) %>%
  mutate(Site = gsub("\\-.*","", Site)) %>%
  mutate(Actual = round(as.numeric(Actual), 2))

# subset budget data
budget <- data %>%
  select("Metrics", "Filename", "date", matches("BudgetCM"))%>%
  gather(-c("Metrics", "Filename", "date"), key = Site, value = Budget) %>%
  mutate(Site = gsub("\\-.*","", Site)) %>%
  mutate(Budget = as.numeric(Budget))


# merge actual and budget data
final_data <- left_join(actual, budget %>%
              select("Metrics", "Site", "Budget"), by = c("Metrics", "Site"))


final_data <- final_data %>%
  mutate(Actual = ifelse(is.na(Actual) & !is.na(Budget), 0, Actual),
         Budget = ifelse(!is.na(Actual) & is.na(Budget), 0, Budget))

# Estimate variance from budget
final_data <- final_data %>% mutate(`Variance From Budget` =  Actual - Budget)


# add the new data to the repo file
repo <- rbind(repo, final_data) %>%
  distinct()

rm(actual, budget, final_data, data)

# Save the data
updated_date <- Sys.Date()

write.csv(repo, paste0(dir, "Repo/Metric_Trends_Data_updated_", 
                       updated_date, ".csv"), row.names = FALSE  )


