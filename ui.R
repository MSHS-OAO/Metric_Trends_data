
# ui function

# define title for ui
header <- dashboardHeader(title = "Metrics Trends Dashboard", titleWidth = 450,
                          tags$li(class = "dropdown", actionButton("download1",
                                                                   label = icon("download")
                          )))

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
                               menuItem("Metrics", tabName = "mshs", icon = icon("hospital")),
                               menuItem("Hospitals", tabName = "site", icon = icon("hospital")),
                               menuItem("Ratio", tabName = "ratio", icon = icon("hospital"))
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
                              p(paste("Metric Trends: The graphs show the Monthly and YTD variances trend of Operating ",
                                      "and Financial Metrics against budget for the fiscal year. For each metric, ",
                                      "the bar graphs display the monthly variances ($) against the corresponding month’s ", 
                                      "budget and the line graph displays the YTD variance (%) against the YTD budget. ",
                                       "A positive variance is favorable to the budget and negative variance is ",
                                      "unfavorable to the budget."),
                                p("The follow metrics are displayed for the health system combined, and by each hospital:"),
                                style = "font-size:16px"))),
               column(12,
                      tags$div(id = "data description", style = "color: #221f72; font-size:14px; margin-left: 20px",
                              h4("Operating Metrics:"),
                              h5("Discharges, Case Mix Index (CMI), and Average Length of Stay (ALOS)"),
                              
                              h4("Revenue Metrics:"),
                              h5("Total Hospital Revenue, Outpatient Revenue, 340B Pharmacy Program, and Other Operating Revenue"),
                              
                              h4("Expense Metrics:"),
                              h5("Total Hospital Expenses, Salaries and Benefits, Supplies and Expenses, Nurse Agency Costs, and CARTS")))),
      
      ## tab MSHS --------------------------------------------
      tabItem(tabName = "mshs",
              div("MSHS Metrics Trends Dashboard", style = paste("color:	#221f72; font-family:Calibri; font-weight:bold;",
                                                                 "font-size:25px; margin-left: 20px")),
              textOutput("mshs_date_show"),
              tags$head(tags$style(paste("#mshs_date_show{color:#7f7f7f; font-family:Calibri; font-style: italic;",
                                         "font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left:",
                                         "20px}"))), hr(),
              
              tags$head(
                tags$style(HTML("
                  #download1 {
                    background: #212070;
                    color: #fff;
                    padding: 8px 15px;
                    font-size: 24px;
                    font-family: inherit;
                    height: 54px;
                    width: 54px;
                    line-height: 44px;
                    outline: none;
                    box-shadow: none;
                    border-radius: 50%;
                    border-color: transparent;}"))),
              
              bsTooltip("download1", "Download (PNG) current tab.",
                        "top", options = list(container = "body")),

              tags$head(tags$style(HTML(paste("#mshs_filters_update {background-color: #d80b8c;color: #FFFFFF;",
                                              "font-size: 18px}")))),
              
              tags$head(tags$style(HTML(paste("#mshs_filters_update_ytd {background-color: #d80b8c;color: #FFFFFF;",
                                              "font-size: 18px}")))),
              
              tags$head(tags$style(HTML(paste("#mshs_filters_update_var {background-color: #d80b8c;color: #FFFFFF;",
                                              "font-size: 18px}")))),
              
              
              fluidRow(
                tags$style(HTML(".box.box-solid.box-primary>.box-header {background:#221f72; color:#fff}")),
                column(12,
                       tabBox(title = NULL, id = "tabset1", width = "100%", type = 'pills', 
                      
                    tabsetPanel(id = "tabSwitch",     
                       tabPanel(title = "Health System Summary",
                       box(
                         title = NULL, width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         fluidRow(
                           box(width = 4, height = "100px", title = "Select Metrics:", solidHeader = FALSE,
                               pickerInput("mshs_metrics", label = NULL, multiple = FALSE,
                                           options = pickerOptions(actionsBox = TRUE), 
                                           choices = mshs_metric_choices,
                                           selected = "Total Hospital Expenses")),
                           box(width = 4, height = "100px",
                               title = "Select Date:",  solidHeader = FALSE,
                               pickerInput("mshs_date_range", label = NULL, multiple = TRUE,
                                           options = pickerOptions(
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 1", 
                                           countSelectedText = "{0}/{1} Dates",
                                           dropupAuto = FALSE),
                                           choices = date_options, 
                                           selected = date_options)),
                           
                           column(width = 4,
                                  actionButton("mshs_filters_update", "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br()))
                         ),
              
                    fluidRow(
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotlyOutput("mshs_plot"),  width = 4),
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotlyOutput("msb_plot"),  width = 4),
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotlyOutput("msbi_plot"),  width = 4),
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotlyOutput("msh_plot"),  width = 4),
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotlyOutput("msm_plot"),  width = 4),
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotlyOutput("msq_plot"),  width = 4),
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotlyOutput("mssn_plot"),  width = 4),
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotlyOutput("msw_plot"),  width = 4),
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotlyOutput("nyee_plot"),  width = 4))),
                
                
                tabPanel(title = "Monthly Variance to Budget",
                         box(
                           title = NULL, width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                           fluidRow(
                             
                             box(width = 4, height = "100px", title = "Select Metrics:", solidHeader = FALSE,
                                 pickerInput("var_metrics", label = NULL, multiple = FALSE,
                                             options = pickerOptions(actionsBox = TRUE), 
                                             choices = metric_choices,
                                             selected = "Total Hospital Expenses")),
                             box(width = 4, height = "100px",
                                 title = "Select Date:",  solidHeader = FALSE,
                                 pickerInput("var_date_range", label = NULL, multiple = TRUE,
                                             options = pickerOptions(
                                             actionsBox = TRUE,
                                             selectedTextFormat = "count > 1", 
                                             countSelectedText = "{0}/{1} Dates",
                                             dropupAuto = FALSE),
                                             choices = date_options, 
                                             selected = date_options)),
                             
                             column(width = 4,
                                    actionButton("mshs_filters_update_var", "CLICK TO UPDATE", width = "75%"),
                                    br(),
                                    br()))
                         ),
                         
                         fluidRow(
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("mshs_var"),  width = 4),
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("msb_var"),  width = 4),
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("msbi_var"),  width = 4),
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("msh_var"),  width = 4),
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("msm_var"),  width = 4),
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("msq_var"),  width = 4),
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("mssn_var"),  width = 4),
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("msw_var"),  width = 4),
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("nyee_var"),  width = 4)
                         )),

                tabPanel(title = "YTD Variance to Budget Ratio",
                         box(
                           title = NULL, width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                           fluidRow(
                             
                             box(width = 4, height = "100px", title = "Select Metrics:", solidHeader = FALSE,
                                 pickerInput("mshs_metrics_ytd", label = NULL, multiple = FALSE,
                                             options = pickerOptions(actionsBox = TRUE), 
                                             choices = metric_choices,
                                             selected = metric_choices[10])),
                             box(width = 4, height = "100px",
                                 title = "Select Date:",  solidHeader = FALSE,
                                 pickerInput("mshs_date_range_ytd", label = NULL, multiple = TRUE,
                                             options = pickerOptions(
                                               actionsBox = TRUE,
                                               selectedTextFormat = "count > 1", 
                                               countSelectedText = "{0}/{1} Dates",
                                               dropupAuto = FALSE),
                                             choices = date_options, 
                                             selected = date_options)),
                             
                             column(width = 4,
                                    actionButton("mshs_filters_update_ytd", "CLICK TO UPDATE", width = "75%"),
                                    br(),
                                    br()))
                         ),
                         
                        fluidRow(
                                box(title = NULL, status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                    plotlyOutput("mshs_plot_ytd"),  width = 4),
                                box(title = NULL, status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                    plotlyOutput("msb_plot_ytd"),  width = 4),
                                box(title = NULL, status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                    plotlyOutput("msbi_plot_ytd"),  width = 4),
                                box(title = NULL, status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                    plotlyOutput("msh_plot_ytd"),  width = 4),
                                box(title = NULL, status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                    plotlyOutput("msm_plot_ytd"),  width = 4),
                                box(title = NULL, status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                    plotlyOutput("msq_plot_ytd"),  width = 4),
                                box(title = NULL, status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                    plotlyOutput("mssn_plot_ytd"),  width = 4),
                                box(title = NULL, status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                    plotlyOutput("msw_plot_ytd"),  width = 4),
                                box(title = NULL, status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                    plotlyOutput("nyee_plot_ytd"),  width = 4),
                                
                         ))) # close tabPanel
                      ))
              ) # close fluidRow
      ),   # close tab MSHS  
      
      ## tab Site --------------------------------------------
      tabItem(tabName = "site",
             div("Hospitals Metrics Trends Dashboard", 
        style = paste("color:	#221f72; font-family:Calibri; font-weight:bold;",
                                        "font-size:25px; margin-left: 20px")),
              textOutput("all_date_show"),
              tags$head(tags$style(paste("#all_date_show{color:#7f7f7f; 
                                      font-family:Calibri; font-style: italic;",
                                      "font-size: 18px; margin-top: -0.2em;
                                      margin-bottom: 0.5em; margin-left:",
                                         "20px}"))), hr(),
              tags$head(tags$style(
               HTML(paste("#all_filters_update {background-color: #d80b8c;color: #FFFFFF;",
                                              "font-size: 18px}")))),
        
              tags$head(tags$style(
               HTML(paste("#all_filters_update_var {background-color: #d80b8c;color: #FFFFFF;",
                           "font-size: 18px}")))),
        
              tags$head(tags$style(
                HTML(paste("#all_filters_update_ytd {background-color: #d80b8c;color: #FFFFFF;",
                     "font-size: 18px}")))),
        
        fluidRow(
          tags$style(HTML(".box.box-solid.box-primary>.box-header {background:#221f72; color:#fff}")),
          column(12,
            tabBox(title = NULL, id = "tabset2", width = "100%", type = 'pills', 
                        
            tabsetPanel(id = "tabSwitch2",     
              tabPanel(title = "Health System Summary",
                
                fluidRow(
                       box(
                         title = NULL, width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE,
                                       closable = TRUE, br(),
                         fluidRow(
                           box(width = 4, height = "100px",
                               title = "Select Hospitals:", solidHeader = FALSE,
                               pickerInput("all_hospital", label = NULL, multiple = FALSE,
                                           options = pickerOptions(actionsBox = TRUE), 
                                           choices = hospital_choices,
                                           selected = "MSHS")),
                        
                           box(width = 4, height = "100px",
                               title = "Select Date:",  solidHeader = FALSE,
                               pickerInput("all_date_range", label = NULL, 
                                           multiple = TRUE,
                                           options = pickerOptions(
                                                     actionsBox = TRUE,
                                                     selectedTextFormat = "count > 1", 
                                                     countSelectedText = "{0}/{1} Dates",
                                                     dropupAuto = FALSE),
                                           choices = date_options, 
                                           selected = date_options)),
                          
                           column(width = 4,
                                  actionButton("all_filters_update", 
                                            "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br())))),
                fluidRow(
                       box(title = NULL, status = "primary",
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("ratio_plot"),  width = 4),
                       
                       box(title = NULL, status = "primary", 
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("revenue_plot"),  width = 4),
                
                       box(title = NULL, status = "primary",
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("expense_plot"),  width = 4),
                       
                       box(title = NULL, status = "primary", 
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("discharges_plot"),  width = 4),
                
               
                       box(title = NULL, status = "primary",
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("cmi_plot"),  width = 4),
                       
                       box(title = NULL, status = "primary", 
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("alos_plot"),  width = 4),
                
              
                       box(title = NULL, status = "primary",
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("outpt_plot"),  width = 4),
                       
                       box(title = NULL, status = "primary", 
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("operate_plot"),  width = 4),
                
               
                       box(title = NULL, status = "primary",
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("salary_plot"),  width = 4),
                       
                       box(title = NULL, status = "primary", 
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("supply_plot"),  width = 4),
                
               
                       box(title = NULL, status = "primary",
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("nurse_plot"),  width = 4),
                       
                       box(title = NULL, status = "primary", 
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("carts_plot"),  width = 4))

                ),
                
              tabPanel(title = "Monthly Variance to Budget",
                       fluidRow(
                         box(
                           title = NULL, width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE,
                           closable = TRUE, br(),
                           fluidRow(
                             box(width = 4, height = "100px",
                                 title = "Select Hospitals:", solidHeader = FALSE,
                                 pickerInput("all_hospital_var", label = NULL, multiple = FALSE,
                                             options = pickerOptions(actionsBox = TRUE), 
                                             choices = hospital_choices,
                                             selected = "MSHS")),
                             
                             box(width = 4, height = "100px",
                                 title = "Select Date:",  solidHeader = FALSE,
                                 pickerInput("all_date_range_var", label = NULL, 
                                             multiple = TRUE,
                                             options = pickerOptions(
                                               actionsBox = TRUE,
                                               selectedTextFormat = "count > 1", 
                                               countSelectedText = "{0}/{1} Dates",
                                               dropupAuto = FALSE),
                                             choices = date_options, 
                                             selected = date_options)),
                             
                             column(width = 4,
                                    actionButton("all_filters_update_var", 
                                                 "CLICK TO UPDATE", width = "75%"),
                                    br(),
                                    br())))),
                       fluidRow(
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("ratio_plot_var"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("revenue_plot_var"),  width = 4),
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("expense_plot_var"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("discharges_plot_var"),  width = 4),
                         
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("cmi_plot_var"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("alos_plot_var"),  width = 4),
                         
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("outpt_plot_var"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("operate_plot_var"),  width = 4),
                         
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("salary_plot_var"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("supply_plot_var"),  width = 4),
                         
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("nurse_plot_var"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("carts_plot_var"),  width = 4))
                       
              ),
              
              tabPanel(title = "YTD Variance to Budget Ratio",
                       fluidRow(
                         box(
                           title = NULL, width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE,
                           closable = TRUE, br(),
                           fluidRow(
                             box(width = 4, height = "100px",
                                 title = "Select Hospitals:", solidHeader = FALSE,
                                 pickerInput("all_hospital_ytd", label = NULL, multiple = FALSE,
                                             options = pickerOptions(actionsBox = TRUE), 
                                             choices = hospital_choices,
                                             selected = "MSHS")),
                             
                             box(width = 4, height = "100px",
                                 title = "Select Date:",  solidHeader = FALSE,
                                 pickerInput("all_date_range_ytd", label = NULL, 
                                             multiple = TRUE,
                                             options = pickerOptions(
                                               actionsBox = TRUE,
                                               selectedTextFormat = "count > 1", 
                                               countSelectedText = "{0}/{1} Dates",
                                               dropupAuto = FALSE),
                                             choices = date_options, 
                                             selected = date_options)),
                             
                             column(width = 4,
                                    actionButton("all_filters_update_ytd", 
                                                 "CLICK TO UPDATE", width = "75%"),
                                    br(),
                                    br())))),
                       fluidRow(
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("ratio_plot_ytd"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("revenue_plot_ytd"),  width = 4),
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("expense_plot_ytd"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("discharges_plot_ytd"),  width = 4),
                         
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("cmi_plot_ytd"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("alos_plot_ytd"),  width = 4),
                         
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("outpt_plot_ytd"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("operate_plot_ytd"),  width = 4),
                         
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("salary_plot_ytd"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("supply_plot_ytd"),  width = 4),
                         
                         
                         box(title = NULL, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("nurse_plot_ytd"),  width = 4),
                         
                         box(title = NULL, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             plotOutput("carts_plot_ytd"),  width = 4))
                       
              )
                
                
                
                
                
                )))) # close fluidRow
              ), # close tabname Site
      
      ## tab Ratio --------------------------------------------
      tabItem(tabName = "ratio",
              div("Expense to Revenue Ratio Dashboard", style = paste("color:	#221f72; font-family:Calibri; font-weight:bold;",
                                                                      "font-size:25px; margin-left: 20px")),
              textOutput("ratio_date_show"),
              tags$head(tags$style(paste("#ratio_date_show{color:#7f7f7f; font-family:Calibri; font-style: italic;",
                                         "font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left:",
                                         "20px}"))), hr(),
              tags$head(tags$style(HTML(paste("#ratio_filters_update {background-color: #d80b8c;color: #FFFFFF;",
                                              "font-size: 18px}")))),
              fluidRow(
                tags$style(HTML(".box.box-solid.box-primary>.box-header {background:#221f72; color:#fff}")),
                column(11,
                       box(
                         title = NULL, width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         fluidRow(
                           box(width = 4, height = "100px", title = "Select Hospitals:", solidHeader = FALSE,
                               pickerInput("ratio_hospital", label = NULL, multiple = FALSE,
                                           options = pickerOptions(actionsBox = TRUE), 
                                           choices = hospital_choices,
                                           selected = "MSHS")),
                           box(width = 4, height = "100px", title = "Select Date:", solidHeader = FALSE,
                               pickerInput("ratio_date_range", label = NULL, multiple = TRUE,
                                           options = pickerOptions(
                                             actionsBox = TRUE,
                                             selectedTextFormat = "count > 1", 
                                             countSelectedText = "{0}/{1} Dates",
                                             dropupAuto = FALSE),
                                           choices = ratio_date_option,
                                           selected = ratio_date_option[1:24])),
                           
                           column(width = 4,
                                  actionButton("ratio_filters_update", "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br())))),
                column(11,
                       box(title = NULL, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           plotOutput("ratio_plot_all"),  width = 12)
                )
              )) #close tab item ratio
      
      
    ))  )
