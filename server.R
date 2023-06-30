
server <- function(input, output, session) {
  
  ## eventReactive for all sites ------------------------------
  all_data  <- eventReactive(input$all_filters_update, {
    validate(need(input$all_metrics != "", "Please Select a Metric"),
             need(input$all_hospital != "", "Please Select a Hospital"),
             need(input$all_date_range != "", "Please Select a Date"))
   
    new_repo %>%
      filter( Site %in% input$all_hospital,
              Metrics %in% input$all_metrics,
              date %in% input$all_date_range)
  }, ignoreNULL = FALSE)
 
  
  # Observeevent for All sites ----------------------------------------------
  ## Observeevent for metrics
  observeEvent(input$all_hospital, {
    metric_choices <- sort(unique(new_repo$Metrics[
      new_repo$Site %in% input$all_hospital]))
    print("3")
    updatePickerInput(session,
                      inputId = "all_metrics",
                      choices = metric_choices,
                      selected = metric_choices[1])
    print("4")
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  

  
  
  # All sites visualization ------------------------------
  
  output$mshs_plot <- renderPlot({
    data <- all_data()  
    test <<- data
    #data <- data%>% filter(Site %in% "MSHS")
    
    validate(need(nrow(data)>0, paste0(input$all_metrics, " is not available for ", input$all_hospital)))
    
    data <- data %>%
      mutate(date= as.yearmon(date, "%Y-%m"))
   
    
   if (isolate(input$all_metrics) %in% c("Expense to Revenue Ratio")) {
     
     # data <- new_repo %>%
     #     filter(Site == "MSHS" & Metrics == "Expense to Revenue Ratio") 
    
     
     if((max(data$Actual, na.rm = TRUE))*1.5 < 0){
       max_value <- 0
     } else {
       max_value <- (max(data$Actual, na.rm = TRUE))*1.5
     }
     
     if( (min(data$Actual, na.rm = TRUE))*1.5 > 0){
       min_value <- 0
     } else {
       min_value <- (min(data$Actual, na.rm = TRUE))*1.5
     }

      ggplot(data = data, aes(x = date, y = Actual, group = Metrics))+
        geom_line(linewidth = 1.25, color = "#212070") +
        geom_point(size = 2.6) +
        labs(x = NULL, y = NULL, 
             #title = paste0(isolate(input$all_hospital), " Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 30),
              plot.subtitle = element_text(hjust = 0.5, size = 20),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
   } else {
     
     # data <- new_repo %>%
     #     filter(Site == "MSHS" & Metrics == "CMI")
     
     
     if( (max(data$Variance.From.Budget, na.rm = TRUE))*1.5 < 0){
       max_value <- 0
     } else {
       max_value <- (max(data$Variance.From.Budget, na.rm = TRUE))*1.5
     }
     
     if( (min(data$Variance.From.Budget, na.rm = TRUE))*1.5 > 0){
       min_value <- 0
     } else {
       min_value <- (min(data$Variance.From.Budget, na.rm = TRUE))*1.5
     }
     
     ggplot(data, aes(x = date, y = Variance.From.Budget))+
       geom_bar(position = "stack", stat="identity" , fill ="#212070")+
       labs(x = NULL, y = NULL, 
           #title = isolate(paste0(input$all_hospital, " ", input$all_metrics , " Monthly Variance to Budget" )),
            subtitle = paste0("($ in Thousands)"))+
         scale_y_continuous(limits=c(min_value, max_value))+
       theme(plot.title = element_text(hjust = 0.5, size = 30),
             plot.subtitle = element_text(hjust = 0.5, size = 20),
             axis.text.x = element_text(angle = 0, hjust = 0.5))
   }
 
  })
  
}