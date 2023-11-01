

# Metric Trends Dashboard

# Import Libraries -------------------------------------------------
suppressMessages({
library(tidyverse)
library(readxl)
library(plotly)
library(scales)
library(reshape2)
library(ggtext)
library(zoo)
library(gridExtra)
library(ggrepel) 
library(shiny)
library(shinyBS)
library(shinyscreenshot)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
})

# Work directory
#dir <- "J:/deans/Presidents/HSPI-PM/Operations Planning/Financials/Metric Trends/"
dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Planning/Financials/Metric Trends/"
  

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
                        "Other Operating", "CARTS", "340B Pharmacy Program",
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
  mutate(Site = gsub("\\-.*","", Site)) 

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
# new_repo <- new_repo %>%
#   mutate(month= ifelse(nchar(month) < 2, paste0("0", month), month),
#          date = paste0(year, "-", month),
#          month= as.numeric(month)) %>%
#   arrange(month, year)


new_repo <- new_repo %>%
  mutate(date = paste0(month.abb[c(month)], "-", substr(year, 3, 4)))
levels_options <- unique(new_repo$date)
new_repo <- new_repo %>%
  mutate(date = factor(date, levels = levels_options))%>%
  arrange(month, year)

# fill na with zero if one of the Actual or Budget is available 
new_repo <- new_repo %>% mutate(Actual = ifelse(!is.na(Budget) & is.na(Actual), 0, Actual),
                                     Budget = ifelse(is.na(Budget) & !is.na(Actual), 0, Budget))
                                     

new_repo <- new_repo %>% mutate(Actual = round(Actual, 3), 
                                Budget = round(Budget, 3))

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
                                           round(100*(Budget_YTD - Actual_YTD)/Budget_YTD, 1),
                                           round(100*(Actual_YTD - Budget_YTD)/Budget_YTD, 1)))


new_repo <- new_repo %>%
  mutate(Variance.From.Budget.YTD = 
           ifelse(Variance.From.Budget.YTD %in% c("Inf", "-Inf", "NaN"), 0, Variance.From.Budget.YTD))




# Import Historical Expense to Revenue Ratio data
Exp_Rev_Ratio <- read_excel(paste0(dir,  "REPO/Exp_Rev_Ratio.xlsx"))

Exp_Rev_Ratio <- Exp_Rev_Ratio %>%
  gather(-c("month", "year"), key = Site, value = Actual) %>%
  mutate(Metrics = "Expense to Revenue Ratio")%>%
  # mutate(month= ifelse(nchar(month) < 2, paste0("0", month), month),
  #        date = paste0(year, "-", month),
  #        month= as.numeric(month)) %>%
  mutate(date = paste0(month.abb[c(month)], "-", substr(year, 3, 4)))

levels_options <- unique(Exp_Rev_Ratio$date)
Exp_Rev_Ratio <- Exp_Rev_Ratio %>%
  mutate(date = factor(date, levels = levels_options))%>%
  arrange(month, year)    





# Filter choices -----------------------------------------------
hospital_choices <- sort(unique(new_repo$Site))
date_options <- as.character(unique(new_repo$date))
mshs_metric_choices <- sort(unique(new_repo$Metrics))

metric_choices <- sort(unique(new_repo$Metrics))
index <- which(metric_choices == "Expense to Revenue Ratio")
metric_choices <- metric_choices[- index]


ratio_date_option <- sort(c(unique(Exp_Rev_Ratio$date), unique(new_repo$date)), 
                          decreasing = TRUE)

ratio_date_option <- as.character(ratio_date_option)


options(ggrepel.max.overlaps = Inf)

# graph functions
# graph functions
ratio_graph <- function(data, site) {
  ggplot(data)  + 
    geom_rect(xmin= 0, xmax= Inf , ymin = 1, ymax= Inf, fill= "#990000", alpha=0.02)+
    geom_line(aes(x=date, y= Actual, group = 1), 
              colour = "#212070", stat="identity", linewidth = 1.25)+
    geom_point(mapping = aes(date, Actual),
               colour = "#212070", size = 3) +
    labs(x = "Date", y = "Expense to Revenue Ratio" , 
         title = paste0(site, " Expense to Revenue Ratio" )
    )+
    geom_hline(aes(yintercept= 1), colour="#990000", linetype="dashed")+
    geom_hline(aes(yintercept = 0))+
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(face = "bold"),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold", size = 10),
          panel.background = element_rect(fill = "white", color = "black", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, colour = "grey"),
          legend.position = "none")+
    geom_text(aes(label= Actual, 
                  x=date, y= Actual),
              position = position_dodge(width = 1), fontface = "bold",
              vjust = 0.5 - sign(data$Actual), size = 4)+
    scale_y_continuous(limits=c(0.8, 1.5), breaks = seq(0.8, 1.5, by = 0.1))
}


graph_style <- function(data,site, metric,  min, max, text, y_label, ratio){
  data_end <- data %>% filter(month == max(month))
  
  p1 <- ggplot(data)  + 
    geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#b2b3b2")+
    labs(x = "Date", y = y_label, 
         title = paste0(site, " ", metric),
         subtitle = paste0("($ in Thousands)"))+
    theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(face='bold'),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold", size = 10),
          panel.background = element_rect(fill = "white", color = "black", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, colour = "grey"),
          legend.position = "non")+
    geom_text_repel(aes(label= data_end$text_label, 
                  x=date, y= Variance, color = sign),
              data = data_end,
              position = position_dodge(width = 1), fontface = "bold",
              vjust = 0.5- sign(data_end$Variance)/2, 
              size = 3)+
    scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
    geom_hline(aes(yintercept = 0)) 
  
  p1 <- p1 +
    geom_line(mapping = aes(date, Variance_scaled, group = 1),
              colour = "#212070", linewidth = 1.0) +
    geom_point(mapping = aes(date, Variance_scaled),
               colour = "#212070", size = 1.6) +
    scale_y_continuous(limits=c(min, max), 
                       sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                    labels = scales::label_percent(scale = 1),
                                                    name = "YTD Variance To Budget Ratio %"))+
    geom_text_repel(aes(label= paste0(data_end$ratio_label, "%"),
                  x=date, y= Variance_scaled, color = sign.YTD),
              data = data_end,
              position = position_dodge(width = 1), fontface='bold',
              vjust = 0.5 - sign(data_end$Variance.From.Budget.YTD)/2, size = 3 )
  
  return(p1)
}



# graph functions
ytd_graph <- function(data, site, metric, min, max) {
  
  ggplot(data)  + 
    geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
              colour = "#212070", stat="identity", linewidth = 1.25)+
    geom_point(mapping = aes(date, Variance.From.Budget.YTD),
               colour = "#212070", size = 3) +
    labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
         title = paste0(site, " " , metric ))+
    theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(face = "bold"),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold",  size = 10),
          panel.background = element_rect(fill = "white", color = "black", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, colour = "grey"),
          legend.position = "none")+
    geom_text(aes(label= paste0(data$ratio_label, "%"), 
                  x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
              position = position_dodge(width = 1), fontface = "bold",
              vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
    scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
    scale_y_continuous(limits=c(min, max))+
    geom_hline(aes(yintercept = 0))
  
}

var_graph <- function(data, site, metric, min, max, y_label, text) {
  
  ggplot(data)  + 
    geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#b2b3b2")+
    labs(x = "Date", y = y_label, 
         title = paste0(site , " ", metric),
         subtitle = paste0("($ in Thousands)"))+
    theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(face = "bold"),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold",  size = 10),
          panel.background = element_rect(fill = "white", color = "black", size = 0.5),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_line(size = 0.25, colour = "grey"),
          legend.position = "non")+
    geom_text(aes(label= text, x=date, y= Variance, color = sign),
              position = position_dodge(width = 1), fontface = "bold",
              vjust = 0.5 - sign(data$Variance)/2, size = 4)+
    scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
    scale_y_continuous(limits=c(min, max))+
    geom_hline(aes(yintercept = 0))
  
}


# Graph style with breaks
graph_style_break <- function(data,site, metric, min, max, break_value, 
                              min_ratio, max_ratio, breaks_ratio, 
                              text, y_label, ratio){
  data_end <- data %>% filter(month == max(month))
  
  p1 <- ggplot(data)  + 
    geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#b2b3b2")+
    labs(x = "Date", y = y_label, 
         title = paste0(site, " ", metric),
         subtitle = paste0("($ in Thousands)"))+
    theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(face='bold'),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold", size = 10),
          panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.minor = element_line(linewidth = 0.25, colour = "grey"),
          legend.position = "non")+
    geom_text_repel(aes(label= data_end$text_label, 
                        x=date, y= Variance, color = sign),
                    data = data_end,
                    position = position_dodge(width = 1), fontface = "bold",
                    vjust = 0.5- sign(data_end$Variance)/2, 
                    size = 3)+
    scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
    geom_hline(aes(yintercept = 0)) 
  
  p1 <- p1 +
    geom_line(mapping = aes(date, Variance_scaled, group = 1),
              colour = "#212070", linewidth = 1.0) +
    geom_point(mapping = aes(date, Variance_scaled),
               colour = "#212070", size = 1.6) +
    scale_y_continuous(limits= c(min, max), breaks = seq(min, max, by = break_value),
                       sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                    breaks = seq(min_ratio, max_ratio, by = breaks_ratio),
                                                    #labels = paste0(data$Variance.From.Budget.YTD, "%"),
                                                    labels = scales::label_percent(scale = 1),
                                                    name = "YTD Variance To Budget Ratio %"))+
    geom_text_repel(aes(label= paste0(data_end$ratio_label, "%"),
                        x=date, y= Variance_scaled, color = sign.YTD),
                    data = data_end,
                    position = position_dodge(width = 1), fontface='bold',
                    vjust = 0.5 - sign(data_end$Variance.From.Budget.YTD)/2, size = 3 )
  
  return(p1)
}



# graph functions
ytd_graph_break <- function(data, site, metric, min, max, breaks) {
  
  ggplot(data)  + 
    geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
              colour = "#212070", stat="identity", linewidth = 1.25)+
    geom_point(mapping = aes(date, Variance.From.Budget.YTD),
               colour = "#212070", size = 3) +
    labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
         title = paste0(site, " " , metric ))+
    theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(face = "bold"),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold",  size = 10),
          panel.background = element_rect(fill = "white", color = "black", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, colour = "grey"),
          legend.position = "none")+
    geom_text(aes(label= paste0(data$ratio_label, "%"), 
                  x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
              position = position_dodge(width = 1), fontface = "bold",
              vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
    scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
    scale_y_continuous(limits=c(min, max), breaks = seq(min, max, by = breaks))+
    geom_hline(aes(yintercept = 0))
  
}

var_graph_break <- function(data, site, metric, min, max, breaks, y_label, text) {
  
  ggplot(data)  + 
    geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#b2b3b2")+
    labs(x = "Date", y = y_label, 
         title = paste0(site , " ", metric),
         subtitle = paste0("($ in Thousands)"))+
    theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(face = "bold"),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold",  size = 10),
          panel.background = element_rect(fill = "white", color = "black", size = 0.5),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_line(size = 0.25, colour = "grey"),
          legend.position = "non")+
    geom_text(aes(label= text, x=date, y= Variance, color = sign),
              position = position_dodge(width = 1), fontface = "bold",
              vjust = 0.5 - sign(data$Variance)/2, size = 4)+
    scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
    scale_y_continuous(limits=c(min, max), breaks = seq(min, max, by = breaks))+
    geom_hline(aes(yintercept = 0))
  
}
