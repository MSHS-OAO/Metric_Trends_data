<<<<<<< HEAD
# ui function

# define title for ui
header <- dashboardHeader(title = "Metrics Trends Dashboard", titleWidth = 450)

# define logos for ui
header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <-
  tags$a(href = "https://peak.mountsinai.org/",
         tags$img(src = "Sinai_logo_white.png",
                  height = "100%", width = "30%"))

ui <- dashboardPage(
  header,
  dashboardSidebar(width = 150,
                   # change the navigation bar color
                   tags$head(tags$style(HTML("
                              .navbar {
                              background-color: #221F72 !important;
                              }"
                   ))),
                   #change the logo bar color
                   tags$head(tags$style(HTML("
                              .logo {
                              background-color: #221F72 !important;
                              }"
                   ))),
                   sidebarMenu(menuItem("Home", tabName = "home", icon = icon("home")),
                               menuItem("ALL Hospitals", tabName = "all", icon = icon("hospital")),
                               menuItem("MSHS", tabName = "mshs", icon = icon("hospital")),
                               menuItem("Hospital", tabName = "site", icon = icon("hospital"))
                   )),
  dashboardBody(
    tabItems(
      ## tab HOME ----------------------------------------
      tabItem(tabName = "home",
              column(12,
                     tags$img(src = "Sinai_logo_color.png", height = "200px",
                              width = "300px", deleteFiles = FALSE)),
              column(12,
                     tags$div("Metrics Trends Dashboard",
                              style = "color: #221f72;
                                           font-weight:bold;
                                           font-size:34px;
                                           margin-left: 20px",
                              h3("Health System Operations"),
                              h4(paste0("Publish Date: ",
                                 format(as.Date(Sys.Date(),
                                      "%B %d %Y"), "%m/%d/%y")))
                     )),
              column(12,
                     tags$div(id = "Objective",
                              style = "color:	#221f72;
                                  margin-left: 20px",
                              h3("Description:"),
                              p(paste("This dashboard summarizes MSHS Metrics Trends. The data is",
                                      "stratified by hospitals and Financial Metrics."),
                                style = "font-size:16px"))),
              column(12,
                     tags$div(id = "data description", style = "color: #221f72; font-size:14px; margin-left: 20px",
                              h3("Data Notes"),
                              h5("We can explain the data and any notes here.")))),
      
      ## tab MSHS --------------------------------------------
      tabItem(tabName = "all",
              div("MSHS Metrics Trends Dashboard", style = paste("color:	#221f72; font-family:Calibri; font-weight:bold;",
                                                             "font-size:25px; margin-left: 20px")),
              textOutput("mshs_date_show"),
              tags$head(tags$style(paste("#mshs_date_show{color:#7f7f7f; font-family:Calibri; font-style: italic;",
                                         "font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left:",
                                         "20px}"))), hr(),
              tags$head(tags$style(HTML(paste("#all_filters_update {background-color: #d80b8c;color: #FFFFFF;",
                                              "font-size: 18px}")))),
              fluidRow(
                tags$style(HTML(".box.box-solid.box-primary>.box-header {background:#221f72; color:#fff}")),
                column(11,
                       box(
                         title = NULL, width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         fluidRow(
                           box(width = 4, height = "100px", title = "Select Hospitals:", solidHeader = FALSE,
                               pickerInput("all_hospital", label = NULL, multiple = FALSE,
                                           options = pickerOptions(actionsBox = TRUE), 
                                           choices = hospital_choices,
                                           selected = "MSHS")),
                           box(width = 4, height = "100px", title = "Select Metrics:", solidHeader = FALSE,
                               pickerInput("all_metrics", label = NULL, multiple = FALSE,
                                           options = pickerOptions(actionsBox = TRUE), 
                                           choices = metric_choices,
                                           selected = metric_choices[1])),
                           box(width = 4, height = "100px",
                               title = "Select Date:",  solidHeader = FALSE,
                               pickerInput("all_date_range", label = NULL, multiple = TRUE,
                                           options = pickerOptions(
                                                     actionsBox = TRUE,
                                                     dropupAuto = FALSE),
                                           choices = date_options, 
                                           selected = date_options)),
                          
                           column(width = 4,
                                  actionButton("all_filters_update", "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br())))),
                column(11,
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("mshs_plot"),  width = 12))
                

                ) # close fluidRow
              ) # close tabname All
      
      
      
=======
# ui function

# define title for ui
header <- dashboardHeader(title = "Metrics Trends Dashboard", titleWidth = 450)

# define logos for ui
header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <-
  tags$a(href = "https://peak.mountsinai.org/",
         tags$img(src = "Sinai_logo_white.png",
                  height = "100%", width = "30%"))

ui <- dashboardPage(
  header,
  dashboardSidebar(width = 150,
                   # change the navigation bar color
                   tags$head(tags$style(HTML("
                              .navbar {
                              background-color: #221F72 !important;
                              }"
                   ))),
                   #change the logo bar color
                   tags$head(tags$style(HTML("
                              .logo {
                              background-color: #221F72 !important;
                              }"
                   ))),
                   sidebarMenu(menuItem("Home", tabName = "home", icon = icon("home")),
                               menuItem("ALL Hospitals", tabName = "all", icon = icon("hospital")),
                               menuItem("MSHS", tabName = "mshs", icon = icon("hospital")),
                               menuItem("Hospital", tabName = "site", icon = icon("hospital"))
                   )),
  dashboardBody(
    tabItems(
      ## tab HOME ----------------------------------------
      tabItem(tabName = "home",
              column(12,
                     tags$img(src = "Sinai_logo_color.png", height = "200px",
                              width = "300px", deleteFiles = FALSE)),
              column(12,
                     tags$div("Metrics Trends Dashboard",
                              style = "color: #221f72;
                                           font-weight:bold;
                                           font-size:34px;
                                           margin-left: 20px",
                              h3("Health System Operations"),
                              h4(paste0("Publish Date: ",
                                 format(as.Date(Sys.Date(),
                                      "%B %d %Y"), "%m/%d/%y")))
                     )),
              column(12,
                     tags$div(id = "Objective",
                              style = "color:	#221f72;
                                  margin-left: 20px",
                              h3("Description:"),
                              p(paste("This dashboard summarizes MSHS Metrics Trends. The data is",
                                      "stratified by hospitals and Financial Metrics."),
                                style = "font-size:16px"))),
              column(12,
                     tags$div(id = "data description", style = "color: #221f72; font-size:14px; margin-left: 20px",
                              h3("Data Notes"),
                              h5("We can explain the data and any notes here.")))),
      
      ## tab MSHS --------------------------------------------
      tabItem(tabName = "all",
              div("MSHS Metrics Trends Dashboard", style = paste("color:	#221f72; font-family:Calibri; font-weight:bold;",
                                                             "font-size:25px; margin-left: 20px")),
              textOutput("mshs_date_show"),
              tags$head(tags$style(paste("#mshs_date_show{color:#7f7f7f; font-family:Calibri; font-style: italic;",
                                         "font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left:",
                                         "20px}"))), hr(),
              tags$head(tags$style(HTML(paste("#all_filters_update {background-color: #d80b8c;color: #FFFFFF;",
                                              "font-size: 18px}")))),
              fluidRow(
                tags$style(HTML(".box.box-solid.box-primary>.box-header {background:#221f72; color:#fff}")),
                column(11,
                       box(
                         title = NULL, width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         fluidRow(
                           box(width = 4, height = "100px", title = "Select Hospitals:", solidHeader = FALSE,
                               pickerInput("all_hospital", label = NULL, multiple = FALSE,
                                           options = pickerOptions(actionsBox = TRUE), 
                                           choices = hospital_choices,
                                           selected = "MSHS")),
                           box(width = 4, height = "100px", title = "Select Metrics:", solidHeader = FALSE,
                               pickerInput("all_metrics", label = NULL, multiple = FALSE,
                                           options = pickerOptions(actionsBox = TRUE), 
                                           choices = metric_choices,
                                           selected = metric_choices[1])),
                           box(width = 4, height = "100px",
                               title = "Select Date:",  solidHeader = FALSE,
                               pickerInput("all_date_range", label = NULL, multiple = TRUE,
                                           options = pickerOptions(
                                                     actionsBox = TRUE,
                                                     dropupAuto = FALSE),
                                           choices = date_options, 
                                           selected = date_options)),
                          
                           column(width = 4,
                                  actionButton("all_filters_update", "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br())))),
                column(11,
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("mshs_plot"),  width = 12))
                

                ) # close fluidRow
              ) # close tabname All
      
      
      
>>>>>>> 80e6a1b3113085aae5a4d3f9244e11a4016c4244
      )))