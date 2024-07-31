

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
  mutate(month= ifelse(nchar(month) < 2, paste0("0", month), month),
         date = paste0(year, "-", month),
         sortedDate = as.Date(paste0(date,  "-01")))%>%
        arrange(sortedDate) %>%
mutate(date = paste0(month.abb[c(as.numeric(month))], "-", substr(year, 3, 4)))

repo_exp_rev_ratio <- new_repo %>%
  filter(Metrics %in% "Expense to Revenue Ratio")%>%
  select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")

Exp_Rev_Ratio <- rbind(Exp_Rev_Ratio, repo_exp_rev_ratio)

levels_options <- unique(Exp_Rev_Ratio$date)
Exp_Rev_Ratio <- Exp_Rev_Ratio %>%
  mutate(date = factor(date, levels = levels_options))%>%
  arrange(month, year)

# Filter choices -----------------------------------------------
hospital_choices <- sort(unique(new_repo$Site))
date_options <- rev(as.character(unique(new_repo$date)))
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

# Mount Sinai corporate colors 
MountSinai_colors <- c(
  `dark purple`  = "#212070",
  `dark pink`    = "#d80b8c",
  `dark blue`    = "#00aeef",
  `dark grey`    = "#7f7f7f",
  `yellow`       = "#ffc000",
  `purple`       = "#7030a0",
  `med purple`   = "#5753d0",
  `med pink`     = "#f75dbe",
  `med blue`     = "#5cd3ff",
  `med grey`     = "#a5a7a5",
  `light purple` = "#c7c6ef",
  `light pink`   = "#fcc9e9",
  `light blue`   = "#c9f0ff",
  `light grey`   = "#dddedd",
  `navy`         = "#00002D"
)


MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Create palettes 
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("dark purple","dark pink","dark blue","dark grey",
                            "med purple","med pink","med blue","med grey", 
                            "light purple","light pink","light blue","light grey"),
  
  `dark`  = MountSinai_cols("med purple","med grey","med blue", "navy",
                            "med pink","dark purple","dark blue", "dark grey",                   
                            "dark pink"),
  
  `main`  = MountSinai_cols("dark purple","dark grey","dark pink","dark blue",
                            "med purple","med pink","med blue","med grey"),
  
  `purple`  = MountSinai_cols("dark purple","med purple","light purple"),
  
  `pink`  = MountSinai_cols("dark pink","med pink","light pink"),
  
  `blue`  = MountSinai_cols("dark blue", "med blue", "light blue"),
  
  `grey`  = MountSinai_cols("dark grey", "med grey", "light grey"),
  
  `purpleGrey` = MountSinai_cols("dark purple", "dark grey"),
  
  `pinkBlue` = MountSinai_cols("dark pink", "dark blue")
  
)


MountSinai_pal <- function(palette = "all", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

# Scale Function for ggplot can be used instead of scale_color_manual
scale_color_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Scale Fill for ggplot insetead of scale_fill_manual 
scale_fill_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}  

# graph functions
ratio_graph <- function(data, site, min_ratio, max_ratio) {
  
 
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
  
  last_data_point <-  data %>% filter(year == max_year) %>% filter(month == max(month))
  text_color <- ifelse(last_data_point$Variance < 0, "red", "black")
  text_color_ratio <- ifelse(last_data_point$Variance.From.Budget.YTD < 0, "red", "black")
  
  # Define different labels for metrics    
  if (metric %in% c("CMI", "ALOS", "Discharges")) {
    text_label <- last_data_point$text_label
    y_label <- "Monthly Variance to Budget"
    y_tick  <- ""
    subtitle_option<- ""
  } else {
    text_label <- paste0("$", last_data_point$text_label)
    y_label <- "Monthly Variance to Budget $"
    y_tick  <- "$,.0f"
    subtitle_option<- "($ in Million)"
  }
  
  
plot_ly(data, x = ~date, y = ~Variance, type = "bar", showlegend = F, 
          marker = list(color =  "#b2b3b2")) %>%
    add_trace(data, x = ~date, y = ~Variance.From.Budget.YTD, type = "scatter", 
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
    add_text(x = last_data_point$date, y = last_data_point$Variance, 
             text = paste0("<b>", text_label, "<b> "), 
             textposition = "outside center", 
             textfont = list(color = text_color, size = 10))%>%
    add_text(x = last_data_point$date, y = last_data_point$Variance.From.Budget.YTD, 
             text = paste0("<b>", last_data_point$ratio_label,"%", "<b>"), 
             textposition = "top center", textfont = list(color = text_color_ratio, size = 10))%>%
    layout(margin = list(l = 50, r = 50, t = 50, b = 50),
           annotations = list(
             list(x = 0.5,  
                  y = 1.09,   
                  text = subtitle_option,
                  showarrow = FALSE,
                  xref='paper', yref='paper',
                  font = list(size = 12))))
}

var_graph_break <- function(data, site, metric, y_range) {
  
  
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

ytd_graph_break <- function(data, site, metric, ratio_range) {
  
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
    
  last_data_point <-  data %>% filter(year %in% max_year) %>% filter(month == max(month)) 
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
    
  last_data_point <-  data %>% filter(year == max(year))%>% filter(month == max(month))
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



