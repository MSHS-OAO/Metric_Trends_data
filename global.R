

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
raw_data_list <- file.info(list.files(path = paste0(dir, "Monthly Financial Data/"), pattern = "^[^~]", full.names = T)) %>%
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
  filter(Metrics %in% c("Total Hospital Expenses", "Total Hospital Revenue"))


# change th data from long to wide
exp_rev <-  dcast(melt(exp_rev, id.vars=c("Metrics", "Filename", "month", "year")), 
                  Filename + month + year + variable ~ Metrics )

exp_rev <- exp_rev %>% 
  mutate( `Total Hospital Expenses`= as.numeric(`Total Hospital Expenses`),
          `Total Hospital Revenue` = as.numeric(`Total Hospital Revenue`),
          `Expense to Revenue Ratio` = round(`Total Hospital Expenses`/`Total Hospital Revenue`, 2))


# change th data from wide to long
exp_rev <- exp_rev %>% 
  select(-c( `Total Hospital Expenses`, `Total Hospital Revenue`))%>%
  spread(key = variable, value = `Expense to Revenue Ratio`)%>%
  mutate(Metrics= "Expense to Revenue Ratio")

# Bind data and exp_rev        
data <- rbind(data, exp_rev)


# Create Salaries and Benefits
salary <- data %>%
  filter(Metrics %in% c("Salaries & Wages", "Contractual & Other Benefits"))
  

# change th data from long to wide
salary <- dcast(melt(salary, id.vars=c("Metrics", "Filename", "month", "year")),
                  Filename + month + year + variable ~ Metrics )


# estimate salaries and benefit
salary <- salary %>% 
  mutate( `Salaries & Wages`= as.numeric(`Salaries & Wages`),
          `Contractual & Other Benefits` = as.numeric(`Contractual & Other Benefits`),
          `Salaries and Benefits` = `Contractual & Other Benefits`+ `Salaries & Wages`)

# change th data from wide to long
salary <- salary %>% 
  select(-c(`Salaries & Wages`, `Contractual & Other Benefits`))%>%
  spread(key = variable, value = `Salaries and Benefits`) %>%
  mutate(Metrics= "Salaries and Benefits")

# Bind data and salary
data <- rbind(data, salary)


#Create 340B/Other Operating Revenue
operating <- data %>%
  filter(Metrics %in% c("Other Operating", "340B Pharmacy Program"))%>%
  replace(is.na(.), 0) 
  
# change th data from long to wide
operating <- dcast(melt(operating, id.vars=c("Metrics", "Filename", "month", "year")),
                   Filename + month + year + variable ~ Metrics )

# estimate 340B/Other Operating Revenue and change the data to long
operating <- operating %>%
  mutate(`340B/Other Operating Revenue`= `340B Pharmacy Program`+ `Other Operating`)%>%
  select(-c(`340B Pharmacy Program`, `Other Operating`))%>%
   spread(key = variable, value = `340B/Other Operating Revenue`) %>%
   mutate(Metrics= "340B/Other Operating Revenue")


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
final_data <- left_join(actual, budget , by = c( "Metrics", "Filename", "month", "year","Site"))



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
         sortedDate = as.Date(paste0(date,  "-01")))%>%
            arrange(sortedDate)

# change the format of date into mm-yy
new_repo <- new_repo %>%
  mutate(date = paste0(month.abb[c(as.numeric(month))], "-", substr(year, 3, 4)))

levels_options <- unique(new_repo$date)
new_repo <- new_repo %>%
  mutate(date = factor(date, levels = levels_options))


# fill NAs with zero if one of the Actual or Budget is available 
new_repo <- new_repo %>% mutate(Actual = ifelse(!is.na(Budget) & is.na(Actual), 0, Actual),
                                Budget = ifelse(is.na(Budget) & !is.na(Actual), 0, Budget))


# change the Actual and Budget values into million
new_repo <- new_repo %>%
  mutate( Actual= ifelse(Metrics %in% c("ALOS", "CMI", "Discharges", "Expense to Revenue Ratio"), Actual, Actual/1000),
          Budget= ifelse(Metrics %in% c("ALOS", "CMI", "Discharges", "Expense to Revenue Ratio"), Budget, Budget/1000),
          Actual = round(Actual, 3), Budget = round(Budget, 3))




# for these metrics YTD variance = (budget - Actual)/budget
metrics_dif <- c("CARTS", "Nursing Agency Costs", "Salaries and Benefits", 
                 "Supplies & Expenses", "Total Hospital Expenses", "ALOS" )




#define Variance
new_repo <- new_repo %>%
  group_by(Site, Metrics) %>%
  mutate(Variance = ifelse(Metrics %in% metrics_dif, 
                                      round(Budget - Actual, 2),
                                      round(Actual - Budget, 2)))


# define YTD variables
new_repo <- new_repo %>%
  group_by(Site, Metrics, year) %>%
  mutate(Actual_YTD = cumsum(Actual),
         Budget_YTD = cumsum(Budget),
         Variance.From.Budget.YTD = ifelse(Metrics %in% metrics_dif, 
                                           round((Budget_YTD - Actual_YTD)/Budget_YTD, 3),
                                           round((Actual_YTD - Budget_YTD)/Budget_YTD, 3)))


new_repo <- new_repo %>%
  mutate(Variance.From.Budget.YTD = 
           ifelse(Variance.From.Budget.YTD %in% c("Inf", "-Inf", "NaN"), NA, Variance.From.Budget.YTD))




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


# date options for initial selection must be based on the current year
max_year <- max(new_repo$year)
date_selected <- new_repo %>% filter(year == max_year) %>%
  ungroup() %>% select(date)
date_selected <- as.character(unique(date_selected$date))

metric_choices <- sort(unique(new_repo$Metrics))
index <- which(metric_choices == "Expense to Revenue Ratio")
metric_choices <- metric_choices[- index]


ratio_date_option <- c("Dec-22", "Nov-22", "Oct-22", "Sep-22", "Aug-22", "Jul-22",
                       "Jun-22", "May-22", "Apr-22", "Mar-22", "Feb-22", "Jan-22",
                       "Dec-21", "Nov-21", "Oct-21", "Sep-21", "Aug-21", "Jul-21",
                       "Jun-21", "May-21", "Apr-21", "Mar-21", "Feb-21", "Jan-21",
                       "Dec-20", "Nov-20", "Oct-20", "Sep-20", "Aug-20", "Jul-20",
                       "Jun-20", "May-20", "Apr-20", "Mar-20", "Feb-20", "Jan-20")


ratio_date_option <- c(rev(date_options), ratio_date_option)

#options(ggrepel.max.overlaps = Inf)


# graph functions
ratio_graph <- function(data, site) {
  
  max_ratio <- 1.9
  min_ratio <- 0.7
 
  ggplotly(
  ggplot(data)  + 
    geom_rect(xmin= 0, xmax= 13 , aes(ymin = 1, ymax= max_ratio), fill= "#990000", alpha=0.2)+
    geom_line(aes(x=date, y= Actual, group = 1), 
              colour = "#212070", stat="identity", linewidth = 0.5)+
    geom_point(mapping = aes(date, Actual),
               colour = "#212070", size = 1.5) +
      labs(title = paste0("<b>", site, " Expense to Revenue Ratio <b>" ))+
    geom_hline(aes(yintercept= 1), colour="#990000", linetype="dashed")+
    geom_hline(aes(yintercept = 0))+
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(face = "bold", size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size =8),
          axis.text.y = element_text(face = "bold", size = 8),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.minor = element_line(linewidth = 0.25, colour = "grey"),
          legend.position = "none")+
    geom_text(aes(label= Actual, x=date, y= Actual+0.04),
              position = position_dodge(width = 1), vjust = 0 , size = 3)+
    scale_y_continuous(limits=c(min_ratio, max_ratio), breaks = seq(min_ratio, max_ratio, by = 0.1)))%>%
    layout( xaxis = list(title = "<b> Date <b> ", showline = TRUE, mirror = "ticks", titlefont = list(size = 12),
                   linewidth = 2, linecolor = "black", showgrid = F),
      yaxis = list(title = "<b> Expense to Revenue Ratio <b>", range = c(min_ratio, max_ratio), titlefont = list(size = 12),
                   showline = TRUE, mirror = "ticks",linewidth = 2, linecolor = "black"))
}

graph_style_break <- function(data, site, metric, y_range, ratio_range){
  
  # if(metric %in% c("Total Hospital Revenue")) {
  #   y_range <- c(-30000, 20000) 	
  #   breaks  <- 5000 
  #   ratio_range <- c(-0.24, 0.16)
  #   breaks_ratio <- 0.04
  # } else if(metric %in% c("Outpatient Revenue")) {
  #   y_range  <- c(-8000, 12000) 	
  #   breaks  <- 2000 
  #   ratio_range <- c(-0.12, 0.18)
  #   breaks_ratio <- 0.03
  # } else if(metric %in% c("340B/Other Operating Revenue")) {
  #   y_range  <- c(-14000, 6000 )
  #   breaks  <- 2000 
  #   ratio_range <- c(-0.42, 0.18)
  #   breaks_ratio <- 0.06
  # } else if(metric %in% c("Total Hospital Expenses")) {
  #   y_range  <- c(-40000, 10000) 	
  #   breaks  <- 5000 
  #   ratio_range <- c(-0.24, 0.06)
  #   breaks_ratio <- 3
  # } else if(metric %in% c("Salaries and Benefits")) {
  #   y_range  <- c(-14000, 10000)	
  #   breaks  <- 2000 
  #   ratio_range <- c(-0.12, 0.10)
  #   breaks_ratio <- 0.02
  # } else if(metric %in% c("Supplies & Expenses")) {
  #   y_range  <- c(-25000, 30000)	
  #   breaks  <- 5000
  #   ratio_range <- c(-0.25, 0.30)
  #   breaks_ratio <- 0.05
  # } else if(metric %in% c("CARTS")) {
  #   y_range  <- c(-5000, 5000)	
  #   breaks  <- 1000 
  #   ratio_range <- c(-0.40, 0.40)
  #   breaks_ratio <- 0.8
  # } else if(metric %in% c("Nursing Agency Costs")) {
  #   y_range <- c(-8000, 2000)
  #   breaks  <- 2000 
  #   ratio_range <- c(-8.00, 2.00)
  #   breaks_ratio <- 2.00
  # } else if(metric %in% c("Discharges")) {
  #   y_range  <- c(-750, 750) 	
  #   breaks  <- 150
  #   ratio_range <- c(-0.75, 0.75)
  #   breaks_ratio <- 0.5
  # } else if(metric %in% c("CMI")) {
  #   y_range  <- c(-0.25, 0.25)	
  #   breaks  <- 0.05
  #   ratio_range <- c(-0.10, 0.10)
  #   breaks_ratio <- 0.02
  # } else if(metric %in% c("ALOS")) {
  #   y_range  <- c(-2.5, 2.5)	
  #   breaks  <- 0.5
  #   ratio_range <- c(-0.25, 0.25)
  #   breaks_ratio <- 0.05
  # }

 
  
  last_data_point <-  data %>% filter(year == max_year) %>% filter(month == max(month))
  text_color <- ifelse(last_data_point$Variance < 0, "red", "black")
  text_color_ratio <- ifelse(last_data_point$Variance.From.Budget.YTD < 0, "red", "black")
  
  # Define different labels for metrics    
  if (metric %in% c("CMI", "ALOS", "Discharges")) {
    text_label <- last_data_point$text_label
    y_label <- "Monthly Variance to Budget"
    y_tick  <- ""
  } else {
    text_label <- paste0("$", last_data_point$text_label)
    y_label <- "Monthly Variance to Budget $"
    y_tick  <- "$,.0f"
  }
  
  
plot_ly(data, x = ~sortedDate, y = ~Variance, type = "bar", showlegend = F, 
          marker = list(color =  "#b2b3b2")) %>%
    add_trace(data, x = ~sortedDate, y = ~Variance.From.Budget.YTD, type = "scatter", 
              mode = "lines+markers", yaxis = "y2", marker = list(color = "#212070"),
              line = list(color =  "#212070")) %>%
    layout(title= paste0("<b> ", site, " ", metric, "<b>"),
           yaxis2 = list(title = "<b> YTD Variance To Budget Ratio % </b>",
                         overlaying = "y", side = "right", range = ratio_range, tickformat= ',.0%', 
                         titlefont = list(size = 12)), 
           xaxis = list(title="<b> Date <b>", titlefont = list(size = 12), fixedrange = TRUE,
                        showline = TRUE, mirror= "ticks", linewidth = 2, tickangle= -45),
                        #tickformat = "%b-%y"),
           yaxis = list(title= paste0("<b>", y_label,"<b>"), tickformat = y_tick, 
                        titlefont = list(size = 12), range = y_range, showline = TRUE,
                        mirror = "ticks", linewidth = 2))%>%
    add_text(x = last_data_point$sortedDate, y = last_data_point$Variance, 
             text = paste0("<b>", text_label, "<b> "), 
             textposition = "outside center", 
             textfont = list(color = text_color, size = 10))%>%
    add_text(x = last_data_point$sortedDate, y = last_data_point$Variance.From.Budget.YTD, 
             text = paste0("<b>", last_data_point$ratio_label,"%", "<b>"), 
             textposition = "top center", textfont = list(color = text_color_ratio, size = 10))%>%
    layout(margin = list(l = 50, r = 50, t = 50, b = 50),
           annotations = list(
             list(x = 0.5,  
                  y = 1.09,   
                  text = "($ in Million)",
                  showarrow = FALSE,
                  xref='paper', yref='paper',
                  font = list(size = 12))))
  

}


# var_graph_break <- function(data, site, metric) {
#   
#   if(metric %in% c("Total Hospital Revenue")) {
#     max <- 20000 	
#     min <- -30000
#     breaks  <- 5000 
#   } else if(metric %in% c("Outpatient Revenue")) {
#     max  <- 12000 	
#     min  <- -8000
#     breaks  <- 2000 
#   } else if(metric %in% c("340B/Other Operating Revenue")) {
#     max  <- 6000 	
#     min  <- -14000
#     breaks  <- 2000 
#   } else if(metric %in% c("Total Hospital Expenses")) {
#     max  <- 10000 	
#     min  <- -40000
#     breaks  <- 5000 
#   } else if(metric %in% c("Salaries and Benefits")) {
#     max  <- 10000 	
#     min  <- -14000
#     breaks  <- 2000 
#   } else if(metric %in% c("Supplies & Expenses")) {
#     max  <- 30000 	
#     min  <- -25000
#     breaks  <- 5000
#   } else if(metric %in% c("CARTS")) {
#     max  <- 5000 	
#     min  <- -5000
#     breaks  <- 1000 
#   } else if(metric %in% c("Nursing Agency Costs")) {
#     max  <- 2000 	
#     min  <- -8000
#     breaks  <- 2000 
#   } else if(metric %in% c("Discharges")) {
#     max  <- 750 	
#     min  <- -750
#     breaks  <- 150
#   } else if(metric %in% c("CMI")) {
#     max  <- 0.25	
#     min  <- -0.25
#     breaks  <- 0.05
#   } else if(metric %in% c("ALOS")) {
#     max  <- 2.5	
#     min  <- -2.5
#     breaks  <- 0.5
#   }
#   
#   last_data_point <-  data %>% filter(month == max(month))
#   text_color <- ifelse(last_data_point$Variance < 0, "red", "black")
#   
#   # Define different labels for metrics    
#   if (metric %in% c("CMI", "ALOS", "Discharges")) {
#     text_label <- last_data_point$text_label
#     y_label <- "Monthly Variance to Budget"
#   } else {
#     text_label <- paste0("$", last_data_point$text_label)
#     y_label <- "Monthly Variance to Budget $"
#   }
#   
#   
#   
#   ggplotly(
#   ggplot(data)  + 
#     geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#b2b3b2")+
#     labs(x = "Date", y = y_label, 
#          title = paste0(site , " ", metric),
#          subtitle = paste0("($ in Thousands)"))+
#     theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
#           plot.subtitle = element_text(hjust = 0.5, size = 10),
#           axis.title = element_text(face = "bold"),
#           legend.text = element_text(size = 6),
#           axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
#           axis.text.y = element_text(face = "bold",  size = 10),
#           panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
#           panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor = element_line(linewidth = 0.25, colour = "grey"),
#           legend.position = "non")+
#     geom_text(aes(label= text_label, x=date, y= Variance, color = sign),
#               position = position_dodge(width = 1), fontface = "bold",
#               vjust = 0.5 - sign(data$Variance)/2, size = 3)+
#     scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
#     scale_y_continuous(limits=c(min, max), breaks = seq(min, max, by = breaks))+
#     geom_hline(aes(yintercept = 0)))
#   
# }


var_graph_break <- function(data, site, metric, y_range) {
  
  # if(metric %in% c("Total Hospital Revenue")) {
  #   y_range <- c(-30000, 20000) 	
  #   breaks  <- 5000 
  # } else if(metric %in% c("Outpatient Revenue")) {
  #   y_range  <- c(-8000, 12000) 	
  #   breaks  <- 2000 
  # } else if(metric %in% c("340B/Other Operating Revenue")) {
  #   y_range  <- c(-14000, 6000 )
  #   breaks  <- 2000 
  # } else if(metric %in% c("Total Hospital Expenses")) {
  #   y_range  <- c(-40000, 10000) 	
  #   breaks  <- 5000 
  # } else if(metric %in% c("Salaries and Benefits")) {
  #   y_range  <- c(-14000, 10000)	
  #   breaks  <- 2000 
  # } else if(metric %in% c("Supplies & Expenses")) {
  #   y_range  <- c(-25000, 30000)	
  #   breaks  <- 5000
  # } else if(metric %in% c("CARTS")) {
  #   y_range  <- c(-5000, 5000)	
  #   breaks  <- 1000 
  # } else if(metric %in% c("Nursing Agency Costs")) {
  #   y_range <- c(-8000, 2000)
  #   breaks  <- 2000 
  # } else if(metric %in% c("Discharges")) {
  #   y_range  <- c(-750, 750) 	
  #   breaks  <- 150
  # } else if(metric %in% c("CMI")) {
  #   y_range  <- c(-0.25, 0.25)	
  #   breaks  <- 0.05
  # } else if(metric %in% c("ALOS")) {
  #   y_range  <- c(-2.5, 2.5)	
  #   breaks  <- 0.5
  # }
  
  last_data_point <-  data %>% filter(year == max_year) %>% filter(month == max(month))
  text_color <- ifelse(last_data_point$Variance < 0, "red", "black")
  
  # Define different labels for metrics    
  if (metric %in% c("CMI", "ALOS", "Discharges")) {
    text_label <- last_data_point$text_label
    y_label <- "Monthly Variance to Budget"
    y_tick  <- ""
  } else {
    text_label <- paste0("$", last_data_point$text_label)
    y_label <- "Monthly Variance to Budget $"
    y_tick  <- "$,.0f"
  }
  
  
  plot_ly(data, x = ~date, y = ~Variance, type = "bar", showlegend = F, 
          marker = list(color =  "#b2b3b2")) %>%
    layout(title= paste0("<b> ", site, " ", metric, "<b>"),
           xaxis = list(title="<b> Date <b>", titlefont = list(size = 12), 
                        showline = TRUE, mirror= "ticks", linewidth = 2, tickangle= -45),
           yaxis = list(title= paste0("<b>", y_label,"<b>"), 
                        titlefont = list(size = 12), tickformat = y_tick,
                        range = y_range, showline = TRUE, mirror = "ticks", linewidth = 2))%>%
    add_text(x = last_data_point$date, y = last_data_point$Variance, 
             text = paste0("<b>", text_label, "<b> "), 
             textposition = "outside center",  
             textfont = list(color = text_color, size = 10))%>%
    layout(margin = list(l = 50, r = 50, t = 50, b = 50),
           annotations = list(
             list(x = 0.5,  
                  y = 1.09,   
                  text = "($ in Million)",
                  showarrow = FALSE,
                  xref='paper', yref='paper',
                  font = list(size = 12))))
}

# graph functions
# ytd_graph_break <- function(data, site, metric) {
#   
#   if(metric %in% c("Total Hospital Revenue")) {
#     min_ratio <- -24
#     max_ratio <- 16
#     breaks_ratio <- 4
#   } else if(metric %in% c("Outpatient Revenue")) {
#     min_ratio <- -12
#     max_ratio <- 18
#     breaks_ratio <- 3
#   } else if(metric %in% c("340B/Other Operating Revenue")) {
#     min_ratio <- -36
#     max_ratio <- 24
#     breaks_ratio <- 6
#   } else if(metric %in% c("Total Hospital Expenses")) {
#     min_ratio <- -24
#     max_ratio <- 6
#     breaks_ratio <- 3
#   } else if(metric %in% c("Salaries and Benefits")) {
#     min_ratio <- -12
#     max_ratio <- 10
#     breaks_ratio <- 2
#   } else if(metric %in% c("Supplies & Expenses")) {
#     min_ratio <- -25
#     max_ratio <- 30
#     breaks_ratio <- 5
#   } else if(metric %in% c("CARTS")) {
#     min_ratio <- -40
#     max_ratio <- 40
#     breaks_ratio <- 8
#   } else if(metric %in% c("Nursing Agency Costs")) {
#     min_ratio <- -800
#     max_ratio <- 200
#     breaks_ratio <- 200
#   } else if(metric %in% c("Discharges")) {
#     min_ratio <- -25
#     max_ratio <- 25
#     breaks_ratio <- 5
#   } else if(metric %in% c("CMI")) {
#     min_ratio <- -10
#     max_ratio <- 10
#     breaks_ratio <- 2
#   } else if(metric %in% c("ALOS")) {
#     min_ratio <- -25
#     max_ratio <- 25
#     breaks_ratio <- 5
#   }
#   
#   ggplotly(
#   ggplot(data)  + 
#     geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
#               colour = "#212070", stat="identity", linewidth = 1.25)+
#     geom_point(mapping = aes(date, Variance.From.Budget.YTD),
#                colour = "#212070", size = 3) +
#     labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
#          title = paste0(site, " " , metric ))+
#     theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
#           plot.subtitle = element_text(hjust = 0.5, size = 10),
#           axis.title = element_text(face = "bold"),
#           legend.text = element_text(size = 6),
#           axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
#           axis.text.y = element_text(face = "bold",  size = 10),
#           panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
#           panel.grid.major.x = element_blank(),
#           panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
#           panel.grid.minor = element_line(linewidth = 0.25, colour = "grey"),
#           legend.position = "none")+
#     geom_text(aes(label= paste0(data$ratio_label, "%"), 
#                   x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
#               position = position_dodge(width = 1), fontface = "bold",
#               vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
#     scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
#     scale_y_continuous(limits=c(min_ratio, max_ratio), breaks = seq(min_ratio, max_ratio, by = breaks_ratio))+
#     geom_hline(aes(yintercept = 0)))
#   
# }


ytd_graph_break <- function(data, site, metric, ratio_range) {
  
  # if(metric %in% c("Total Hospital Revenue")) {
  #   ratio_range <- c(-0.24, 0.16)
  #   breaks_ratio <- 0.04
  # } else if(metric %in% c("Outpatient Revenue")) {
  #   ratio_range <- c(-0.12, 0.18)
  #   breaks_ratio <- 0.03
  # } else if(metric %in% c("340B/Other Operating Revenue")) {
  #   ratio_range <- c(-0.42, 0.18)
  #   breaks_ratio <- 0.06
  # } else if(metric %in% c("Total Hospital Expenses")) {
  #   ratio_range <- c(-0.24, 0.06)
  #   breaks_ratio <- 0.03
  # } else if(metric %in% c("Salaries and Benefits")) {
  #   ratio_range <- c(-0.12, 0.10)
  #   breaks_ratio <- 0.02
  # } else if(metric %in% c("Supplies & Expenses")) {
  #   ratio_range <- c(-0.25, 0.30)
  #   breaks_ratio <- 0.05
  # } else if(metric %in% c("CARTS")) {
  #   ratio_range <- c(-0.40, 0.40)
  #   breaks_ratio <- 0.08
  # } else if(metric %in% c("Nursing Agency Costs")) {
  #   ratio_range <- c(-8, 2)
  #   breaks_ratio <- 2
  # } else if(metric %in% c("Discharges")) {
  #   ratio_range <- c(-0.25, 0.75)
  #   breaks_ratio <- 0.05
  # } else if(metric %in% c("CMI")) {
  #   ratio_range <- c(-0.10, 0.10)
  #   breaks_ratio <- 0.02
  # } else if(metric %in% c("ALOS")) {
  #   ratio_range <- c(-0.25, 0.25)
  #   breaks_ratio <- 0.05
  # }
  # 
  
  last_data_point <-  data %>% filter(year == max_year) %>% filter(month == max(month))
  text_color_ratio <- ifelse(last_data_point$Variance.From.Budget.YTD < 0, "red", "black")
 
  plot_ly(data, x = ~date, y = ~Variance.From.Budget.YTD, type = "scatter", showlegend = F,
          mode = "lines+markers", marker = list(color = "#212070"),
          line = list(color =  "#212070")) %>%
    layout(title= paste0("<b> ", site, " ", metric, "<b>"),
           xaxis = list(title="<b> Date <b>", titlefont = list(size = 12), 
                        showline = TRUE, mirror= "ticks", linewidth = 2, tickangle= -45),
           yaxis = list(title= paste0("<b> YTD Variance to Budget Ratio % <b>"), 
                        titlefont = list(size = 12), tickformat= ',.0%', 
                        range = ratio_range, showline = TRUE, mirror = "ticks", linewidth = 2))%>%
    add_text(x = last_data_point$date, y = last_data_point$Variance.From.Budget.YTD, 
             text = paste0("<b>", last_data_point$ratio_label,"%", "<b>"),
             textposition = "top center", textfont = list(color = text_color_ratio, size = 10))%>%
    layout(margin = list(l = 50, r = 50, t = 50, b = 50))
}



# ratio_graph_hosp <- function(data, site) {
#     ggplot(data)  +
#       geom_rect(xmin= 0, xmax= Inf , aes(ymin = 1, ymax= Inf), fill= "#990000", alpha=0.02)+
#       geom_line(aes(x=date, y= Actual, group = 1),
#                 colour = "#212070", stat="identity", linewidth = 1)+
#       geom_point(mapping = aes(date, Actual),
#                  colour = "#212070", size = 2) +
#       labs(x = "Date", y = "Expense to Revenue Ratio" ,
#            title = paste0(site, " Expense to Revenue Ratio" ))+
#       geom_hline(aes(yintercept= 1), colour="#990000", linetype="dashed")+
#       geom_hline(aes(yintercept = 0))+
#       theme(plot.title = element_text(hjust = 0.5, size = 15),
#             axis.title = element_text(face = "bold", size = 10),
#             axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size =10),
#             axis.text.y = element_text(face = "bold", size = 10),
#             #text = element_text(face = "bold", size = 1),
#             panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
#             panel.grid.major.x = element_blank(),
#             panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
#             panel.grid.minor = element_line(linewidth = 0.25, colour = "grey"),
#             legend.position = "none")+
#       geom_text(aes(label= Actual, x=date, y= Actual+0.02),
#                 position = position_dodge(width = 1), vjust = 0 )+
#       scale_y_continuous(limits=c(0.8, 1.5), breaks = seq(0.8, 1.5, by = 0.1))
# }



# graph_style <- function(data,site, metric,  min, max, text, y_label, ratio){
#   data_end <- data %>% filter(month == max(month))
# 
# 
#   p1 <- ggplot(data)  +
#     geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#b2b3b2")+
#     labs(x = "Date", y = y_label,
#          title = paste0(site, " ", metric),
#          subtitle = paste0("($ in Thousands)"))+
#     theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
#           plot.subtitle = element_text(hjust = 0.5, size = 10),
#           axis.title = element_text(face='bold'),
#           legend.text = element_text(size = 6),
#           axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
#           axis.text.y = element_text(face = "bold", size = 10),
#           panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
#           panel.grid.major.x = element_blank(),
#           panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
#           panel.grid.minor = element_line(linewidth = 0.25, colour = "grey"),
#           legend.position = "non")+
#     geom_text_repel(aes(label= data_end$text_label,
#                   x=date, y= Variance, color = sign),
#               data = data_end,
#               position = position_dodge(width = 1), fontface = "bold",
#               vjust = 0.5- sign(data_end$Variance)/2,
#               size = 3)+
#     scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
#     geom_hline(aes(yintercept = 0))
# 
#   p1 <- p1 +
#     geom_line(mapping = aes(date, Variance_scaled, group = 1),
#               colour = "#212070", linewidth = 1.0) +
#     geom_point(mapping = aes(date, Variance_scaled),
#                colour = "#212070", size = 1.6) +
#     scale_y_continuous(limits=c(min, max),
#                        sec.axis = ggplot2::sec_axis(~. / ratio ,
#                                                     labels = scales::label_percent(scale = 1),
#                                                     name = "YTD Variance To Budget Ratio %"))+
#     geom_text_repel(aes(label= paste0(data_end$ratio_label, "%"),
#                   x=date, y= Variance_scaled, color = sign.YTD),
#               data = data_end,
#               position = position_dodge(width = 1), fontface='bold',
#               vjust = 0.5 - sign(data_end$Variance.From.Budget.YTD)/2, size = 3 )
# 
#   return(p1)
# }


graph_style <- function(data,site, metric){
  
  last_data_point <-  data %>% filter(month == max(month), year == max(year))
  text_color <- ifelse(last_data_point$Variance < 0, "red", "black")
  text_color_ratio <- ifelse(last_data_point$Variance.From.Budget.YTD < 0, "red", "black")
  
  # Define different labels for metrics    
  if (metric %in% c("CMI", "ALOS", "Discharges")) {
    text_label <- last_data_point$text_label
    y_label <- "Monthly Variance to Budget"
    y_tick  <- ""
  } else {
    text_label <- paste0("$", last_data_point$text_label)
    y_label <- "Monthly Variance to Budget $"
    y_tick  <- "$,.0f"
  }
  
  
  plot_ly(data, x = ~date, y = ~Variance, type = "bar", showlegend = F, 
          marker = list(color =  "#b2b3b2")) %>%
    add_trace(data, x = ~date, y = ~Variance.From.Budget.YTD, type = "scatter", 
              mode = "lines+markers", yaxis = "y2", marker = list(color = "#212070"),
              line = list(color =  "#212070")) %>%
    layout(title= paste0("<b> ", site, " ", metric, "<b>"),
           yaxis2 = list(title = "<b> YTD Variance To Budget Ratio % </b>",
                         scaleratio = 1, constraintoward = "bottom",
                         overlaying = "y",  side = "right", tickformat= ',.0%', 
                         titlefont = list(size = 12)), 
           xaxis = list(title="<b> Date <b>", titlefont = list(size = 12), 
                        showline = TRUE, mirror= "ticks", linewidth = 2, tickangle= -45),
           yaxis = list(title= paste0("<b>", y_label,"<b>"), 
                        titlefont = list(size = 12), tickformat = y_tick,
                         showline = TRUE, mirror = "ticks", linewidth = 2))%>%
    add_text(x = last_data_point$date, y = last_data_point$Variance, 
             text = paste0("<b>", text_label, "<b> "), 
             textposition = "outside center", 
             textfont = list(color = text_color, size = 10))%>%
    add_text(x = last_data_point$date, y = last_data_point$Variance.From.Budget.YTD, 
             text = paste0("<b>", last_data_point$ratio_label,"%", "<b>"), 
             textposition = "outside center", textfont = list(color = text_color_ratio, size = 10))%>%
    layout(margin = list(l = 50, r = 50, t = 50, b = 50),
           annotations = list(
             list(x = 0.5,  
                  y = 1.09,   
                  text = "($ in Million)",
                  showarrow = FALSE,
                  xref='paper', yref='paper',
                  font = list(size = 12))))
  
  
}




# # graph functions
# ytd_graph <- function(data, site, metric, min, max) {
#   
# 
#   ggplot(data)  + 
#     geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
#               colour = "#212070", stat="identity", linewidth = 1.25)+
#     geom_point(mapping = aes(date, Variance.From.Budget.YTD),
#                colour = "#212070", size = 3) +
#     labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
#          title = paste0(site, " " , metric ))+
#     theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
#           plot.subtitle = element_text(hjust = 0.5, size = 10),
#           axis.title = element_text(face = "bold"),
#           legend.text = element_text(size = 6),
#           axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
#           axis.text.y = element_text(face = "bold",  size = 10),
#           panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
#           panel.grid.major.x = element_blank(),
#           panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
#           panel.grid.minor = element_line(linewidth = 0.25, colour = "grey"),
#           legend.position = "none")+
#     geom_text(aes(label= paste0(data$ratio_label, "%"), 
#                   x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
#               position = position_dodge(width = 1), fontface = "bold",
#               vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
#     scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
#     scale_y_continuous(limits=c(min, max))+
#     geom_hline(aes(yintercept = 0))
#   
# }
# 
# var_graph <- function(data, site, metric, min, max, y_label, text) {
#   
# 
#   ggplot(data)  + 
#     geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#b2b3b2")+
#     labs(x = "Date", y = y_label, 
#          title = paste0(site , " ", metric),
#          subtitle = paste0("($ in Thousands)"))+
#     theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
#           plot.subtitle = element_text(hjust = 0.5, size = 10),
#           axis.title = element_text(face = "bold"),
#           legend.text = element_text(size = 6),
#           axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
#           axis.text.y = element_text(face = "bold",  size = 10),
#           panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
#           panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor = element_line(linewidth = 0.25, colour = "grey"),
#           legend.position = "non")+
#     geom_text(aes(label= text, x=date, y= Variance, color = sign),
#               position = position_dodge(width = 1), fontface = "bold",
#               vjust = 0.5 - sign(data$Variance)/2, size = 4)+
#     scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "black"))+
#     scale_y_continuous(limits=c(min, max))+
#     geom_hline(aes(yintercept = 0))
#   
# }
# 



var_graph <- function(data, site, metric) {
  

  if( (max(data$Variance, na.rm = TRUE))*1.2 < 0){
    max_value <- 0
  } else {
    max_value <- (max(data$Variance, na.rm = TRUE))*1.2
  }
  
  if( (min(data$Variance, na.rm = TRUE))*1.2 > 0){
    min_value <- 0
  } else {
    min_value <- (min(data$Variance, na.rm = TRUE))*1.2
  }
  
    y_range <- c(min_value, max_value) 	
    
  
    
  last_data_point <-  data %>% filter(month == max(month), year == max(year))
  text_color <- ifelse(last_data_point$Variance < 0, "red", "black")
  
  # Define different labels for metrics    
  if (metric %in% c("CMI", "ALOS", "Discharges")) {
    text_label <- last_data_point$text_label
    y_label <- "Monthly Variance to Budget"
    y_tick  <- ""
  } else {
    text_label <- paste0("$", last_data_point$text_label)
    y_label <- "Monthly Variance to Budget $"
    y_tick  <- "$,.0f"
  }
  
  
  plot_ly(data, x = ~date, y = ~Variance, type = "bar", showlegend = F, 
          marker = list(color =  "#b2b3b2")) %>%
    layout(title= paste0("<b> ", site, " ", metric, "<b>"),
           xaxis = list(title="<b> Date <b>", titlefont = list(size = 12), 
                        showline = TRUE, mirror= "ticks", linewidth = 2, tickangle= -45),
           yaxis = list(title= paste0("<b>", y_label,"<b>"), tickformat = y_tick,
                        titlefont = list(size = 12), 
                        range = y_range, showline = TRUE, mirror = "ticks", linewidth = 2))%>%
    add_text(x = last_data_point$date, y = last_data_point$Variance, 
             text = paste0("<b>", text_label, "<b> "), 
             textposition = "outside center",  
             textfont = list(color = text_color, size = 10))%>%
    layout(margin = list(l = 50, r = 50, t = 50, b = 50),
           annotations = list(
             list(x = 0.5,  
                  y = 1.09,   
                  text = "($ in Million)",
                  showarrow = FALSE,
                  xref='paper', yref='paper',
                  font = list(size = 12))))
}



ytd_graph <- function(data, site, metric) {
  
  
  if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2 < 0){
    max_value_ytd <- 0
  } else {
    max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2
  }
  
  if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2 > 0){
    min_value_ytd <- 0
  } else {
    min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2
  }
  
  
    ratio_range <- c(min_value_ytd, max_value_ytd)
    
  
  
  last_data_point <-  data %>% filter(month == max(month), year == max(year))
  text_color_ratio <- ifelse(last_data_point$Variance.From.Budget.YTD < 0, "red", "black")
  
  plot_ly(data, x = ~date, y = ~Variance.From.Budget.YTD, type = "scatter", showlegend = F,
          mode = "lines+markers", marker = list(color = "#212070"),
          line = list(color =  "#212070")) %>%
    layout(title= paste0("<b> ", site, " ", metric, "<b>"),
           xaxis = list(title="<b> Date <b>", titlefont = list(size = 12), 
                        showline = TRUE, mirror= "ticks", linewidth = 2, tickangle= -45),
           yaxis = list(title= paste0("<b> YTD Variance to Budget Ratio % <b>"), 
                        titlefont = list(size = 12), tickformat= ',.0%',
                        range = ratio_range, showline = TRUE, mirror = "ticks", linewidth = 2))%>%
    add_text(x = last_data_point$date, y = last_data_point$Variance.From.Budget.YTD, 
             text = paste0("<b>", last_data_point$ratio_label,"%", "<b>"),
             textposition = "top center", textfont = list(color = text_color_ratio, size = 10))%>%
    layout(margin = list(l = 50, r = 50, t = 50, b = 50))
}



