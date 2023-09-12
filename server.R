
# Server
server <- function(input, output, session) {
  
  # Text output ---------------------------------
  ### Text output for Metrics tab -------------------------------
  system_text <- eventReactive(input$mshs_filters_update, {
    end_date <- isolate(max(input$mshs_date_range))
    start_date <- isolate(min(input$mshs_date_range))
    metric <- isolate(input$mshs_metrics)
   paste0("Based on data from ", start_date, " to ", end_date, " for ", metric)
  }, ignoreNULL = FALSE)
  
  var_text <- eventReactive(input$mshs_filters_update_var, {
    end_date <- isolate(max(input$var_date_range))
    start_date <- isolate(min(input$var_date_range))
    metric <- isolate(input$var_metrics)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", metric)
  }, ignoreNULL = FALSE)
  
  
  ytd_text <- eventReactive(input$mshs_filters_update_ytd, {
    end_date <- isolate(max(input$mshs_date_range_ytd))
    start_date <- isolate(min(input$mshs_date_range_ytd))
    metric <- isolate(input$mshs_metrics_ytd)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", metric)
  }, ignoreNULL = FALSE)
  

  ### Observeevent to update the text based on different filters
  observeEvent(input$tabSwitch, {
    print(input$tabSwitch)
    if(input$tabSwitch == "Health System Summary") {
      output$mshs_date_show  <- renderText({
        system_text()
      })
    }
    if(input$tabSwitch == "Monthly Variance to Budget") {
      output$mshs_date_show  <- renderText({
        var_text()
      })
    }
    if(input$tabSwitch == "YTD Variance to Budget Ratio") {
      output$mshs_date_show  <- renderText({
        ytd_text()
      })
    }
  })


  ### Text output for Hospital tab -------------------------------
  all_mshs_text <- eventReactive(input$all_filters_update, {
    end_date <- isolate(max(input$all_date_range))
    start_date <- isolate(min(input$all_date_range))
    hospitals <- isolate(input$all_hospital)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", hospitals)
  }, ignoreNULL = FALSE)
  
  all_mshs_text_var <- eventReactive(input$all_filters_update_var, {
    end_date <- isolate(max(input$all_date_range_var))
    start_date <- isolate(min(input$all_date_range_var))
    hospitals <- isolate(input$all_hospital_var)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", hospitals)
  }, ignoreNULL = FALSE)
  
  all_mshs_text_ytd <- eventReactive(input$all_filters_update_ytd, {
    end_date <- isolate(max(input$all_date_range_ytd))
    start_date <- isolate(min(input$all_date_range_ytd))
    hospitals <- isolate(input$all_hospital_ytd)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", hospitals)
  }, ignoreNULL = FALSE)
  
  
  ### Observeevent to update the text based on different filters
  observeEvent(input$tabSwitch2, {
    print(input$tabSwitch2)
    if(input$tabSwitch2 == "Health System Summary") {
      output$all_date_show  <- renderText({
        all_mshs_text()
      })
    }
    if(input$tabSwitch2 == "Monthly Variance to Budget") {
      output$all_date_show  <- renderText({
        all_mshs_text_var()
      })
    }
    if(input$tabSwitch2 == "YTD Variance to Budget Ratio") {
      output$all_date_show  <- renderText({
        all_mshs_text_ytd()
      })
    }
  })
  
  
  
  ### Text output for Ratio tab -------------------------------
  ratio_mshs_text <- eventReactive(input$ratio_filters_update, {
    end_date <- isolate(max(input$ratio_date_range))
    start_date <- isolate(min(input$ratio_date_range))
    hospitals <- isolate(input$ratio_hospital)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", hospitals)
  }, ignoreNULL = FALSE)
  
  output$ratio_date_show <- renderText({
    ratio_mshs_text()
  })
  
  # eventReactive -------------------------------------
  ### eventReactive for Metrics Tab: System - ------------------------------
  mshs_data  <- eventReactive(input$mshs_filters_update, {
    validate(need(input$mshs_metrics != "", "Please Select a Metric"),
             need(input$mshs_date_range != "", "Please Select a Date"))
    
    new_repo %>%
      filter(Metrics %in% input$mshs_metrics,
             date %in% input$mshs_date_range)
  }, ignoreNULL = FALSE)
  
  ### eventReactive for Metrics Tab: Variance ------------------------------
  var_data  <- eventReactive(input$mshs_filters_update_var, {
    validate(need(input$var_metrics != "", "Please Select a Metric"),
             need(input$var_date_range != "", "Please Select a Date"))
    
    new_repo %>%
      filter(Metrics %in% input$var_metrics,
             date %in% input$var_date_range)
  }, ignoreNULL = FALSE)
  
  
  ### eventReactive for Metrics Tab: YTD ------------------------------
  mshs_data_ytd  <- eventReactive(input$mshs_filters_update_ytd, {
    validate(need(input$mshs_metrics_ytd != "", "Please Select a Metric"),
             need(input$mshs_date_range_ytd != "", "Please Select a Date"))
    
    new_repo %>%
      filter(Metrics %in% input$mshs_metrics_ytd,
             date %in% input$mshs_date_range_ytd)
  }, ignoreNULL = FALSE)
  
  
  ### eventReactive for hospitals tab ------------------------------
  metric_data  <- eventReactive(input$all_filters_update,{
    validate(need(input$all_hospital != "", "Please Select a Hospital"),
             need(input$all_date_range != "", "Please Select a Date"))
    
    new_repo %>%
      filter(Site %in% input$all_hospital,
             date %in% input$all_date_range)
  }, ignoreNULL = FALSE)
  
  metric_data_var  <- eventReactive(input$all_filters_update_var,{
    validate(need(input$all_hospital_var != "", "Please Select a Hospital"),
             need(input$all_date_range_var != "", "Please Select a Date"))
    
    new_repo %>%
      filter(Site %in% input$all_hospital_var,
             date %in% input$all_date_range_var)
  }, ignoreNULL = FALSE)
  
  metric_data_ytd  <- eventReactive(input$all_filters_update_ytd,{
    validate(need(input$all_hospital_ytd != "", "Please Select a Hospital"),
             need(input$all_date_range_ytd != "", "Please Select a Date"))
    
    new_repo %>%
      filter(Site %in% input$all_hospital_ytd,
             date %in% input$all_date_range_ytd)
  }, ignoreNULL = FALSE)
  
  
  ### eventReactive for ratio tab ------------------------------
  ratio_data  <- eventReactive(input$ratio_filters_update,{
    validate(need(input$ratio_hospital != "", "Please Select a Hospital"),
             need(input$ratio_date_range != "", "Please Select a Date"))
             
    data <-  new_repo  %>% 
      filter(Metrics == "Expense to Revenue Ratio") %>%
      select("month", "year", "Site", "Actual", "Metrics", "date")
    
    data <- rbind(Exp_Rev_Ratio, data)
    
    data %>%
      filter(Site %in% input$ratio_hospital,
             date %in% input$ratio_date_range)
  }, ignoreNULL = FALSE)
 
  
  # Metrics visualization ------------------------------
  # System Summary -----------------------------
  ### MSHS -------------------------------------
  output$mshs_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSHS")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSHS")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "Expense to Revenue Ratio")
      
      data <- data %>% 
      select("month", "year", "Site", "Actual", "Metrics", "date")

      history <- Exp_Rev_Ratio %>% filter(Site== "MSHS")
      
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      
      
      if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.2
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.2
      }
      
      ratio_graph(data, site = "MSHS", min = min_value, max = max_value)
      
    } else {
      
      # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
      
      #define a scale for second axis
      ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
      
      if (ratio %in% c("Inf", "-Inf", "NaN")) {
        ratio <- 0
      }
      
      data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
    
    # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$text_label
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$text_label)
        y_label <- "Variance to Budget $"
      }
      
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "MSHS", metric = metric_choice, min =  min_value,
                  max = max_value, text = text_label, y_label = y_label, ratio = ratio)
      
    }
    
  })
  
  ### MSB -------------------------------------
  output$msb_plot <- renderPlot({
    data <- mshs_data()  %>%
      filter(Site == "MSB")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                    Variance.From.Budget.YTD))
             
      
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSB")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "Expense to Revenue Ratio") 
      
      data <- data %>% 
        select("month", "year", "Site", "Actual", "Metrics", "date")
      
      history <- Exp_Rev_Ratio %>% filter(Site== "MSB")
      
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      
      
      if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.2
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.2
      }
      
      ratio_graph(data, site = "MSB", min = min_value, max = max_value)
      
      
    } else {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "CMI")
      
      #define a scale for second axis
      ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
      
      if (ratio %in% c("Inf", "-Inf", "NaN")) {
        ratio <- 0
      }
      
      data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$text_label
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$text_label)
        y_label <- "Variance to Budget $"
      }
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "MSB", metric = metric_choice, min =  min_value,
            max = max_value, text = text_label, y_label = y_label, ratio = ratio)
      
    }
    
  })
  
  
  ### MSBI -------------------------------------
  output$msbi_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSBI")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSBI")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "Expense to Revenue Ratio") 
      
      data <- data %>% 
        select("month", "year", "Site", "Actual", "Metrics", "date")
      
      history <- Exp_Rev_Ratio %>% filter(Site== "MSBI")
      
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      
      if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.2
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.2
      }
      
      ratio_graph(data, site = "MSBI", min = min_value, max = max_value)
      
    } else {
      
      # data <- new_repo %>% filter(Site == "MSBI" & Metrics == "CMI")
      
      #define a scale for second axis
      ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
      
      if (ratio %in% c("Inf", "-Inf", "NaN")) {
        ratio <- 0
      }
      
      data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$text_label
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$text_label)
        y_label <- "Variance to Budget $"
      }
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "MSBI", metric = metric_choice, min =  min_value,
                  max = max_value, text = text_label, y_label = y_label, ratio = ratio)
      
    }
    
  })
  
  ### MSH -------------------------------------
  output$msh_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSH")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSH")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "Expense to Revenue Ratio") 
      
      data <- data %>% 
        select("month", "year", "Site", "Actual", "Metrics", "date")
      
      history <- Exp_Rev_Ratio %>% filter(Site== "MSH")
      
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      
      
      if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.2
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.2
      }
      
      ratio_graph(data, site = "MSH", min = min_value, max = max_value)
      
    } else {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "CMI")
      
      #define a scale for second axis
      ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
      
      if (ratio %in% c("Inf", "-Inf", "NaN")) {
        ratio <- 0
      }
      
      data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$text_label
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$text_label)
        y_label <- "Variance to Budget $"
      }
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "MSH", metric = metric_choice, min =  min_value,
                  max = max_value, text = text_label, y_label = y_label, ratio = ratio)
    }
    
  })
  
  ### MSM -------------------------------------
  output$msm_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSM")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSM")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "Expense to Revenue Ratio") 
      data <- data %>% 
        select("month", "year", "Site", "Actual", "Metrics", "date")
      
      history <- Exp_Rev_Ratio %>% filter(Site== "MSM")
      
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      
      if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.2
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.2
      }
      
      ratio_graph(data, site = "MSM", min = min_value, max = max_value)
    } else {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "CMI")
      
      #define a scale for second axis
      ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
      
      if (ratio %in% c("Inf", "-Inf", "NaN")) {
        ratio <- 0
      }
      
      data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$text_label
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$text_label)
        y_label <- "Variance to Budget $"
      }
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "MSM", metric = metric_choice, min =  min_value,
                  max = max_value, text = text_label, y_label = y_label, ratio = ratio)
      
    }
    
  })
  
  ### MSQ -------------------------------------
  output$msq_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSQ")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSQ")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "Expense to Revenue Ratio") 
      data <- data %>% 
        select("month", "year", "Site", "Actual", "Metrics", "date")
      
      history <- Exp_Rev_Ratio %>% filter(Site== "MSQ")
      
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      
      if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.2
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.2
      }
      
      ratio_graph(data, site = "MSQ", min = min_value, max = max_value)
      
    } else {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "CMI")
      
      #define a scale for second axis
      ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
      
      if (ratio %in% c("Inf", "-Inf", "NaN")) {
        ratio <- 0
      }
      
      data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$text_label
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$text_label)
        y_label <- "Variance to Budget $"
      }
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "MSQ", metric = metric_choice, min =  min_value,
                  max = max_value, text = text_label, y_label = y_label, ratio = ratio)
      
    }
    
  })
  
  ### MSSN -------------------------------------
  output$mssn_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSSN")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSSN")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "Expense to Revenue Ratio") 
      data <- data %>% 
        select("month", "year", "Site", "Actual", "Metrics", "date")
      
      history <- Exp_Rev_Ratio %>% filter(Site== "MSSN")
      
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      
      if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.2
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.2
      }
      
      ratio_graph(data, site = "MSSN", min = min_value, max = max_value)
      
    } else {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "CMI")
      
      #define a scale for second axis
      ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
      
      if (ratio %in% c("Inf", "-Inf", "NaN")) {
        ratio <- 0
      }
      
      data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$text_label
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$text_label)
        y_label <- "Variance to Budget $"
      }
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "MSSN", metric = metric_choice, min =  min_value,
                  max = max_value, text = text_label, y_label = y_label, ratio = ratio)
      
      
    }
    
  })
  
  ### MSW -------------------------------------
  output$msw_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSW")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSW")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "Expense to Revenue Ratio") 
      data <- data %>% 
        select("month", "year", "Site", "Actual", "Metrics", "date")
      
      history <- Exp_Rev_Ratio %>% filter(Site== "MSW")
      
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      
      if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.2
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.2
      }
      
      ratio_graph(data, site = "MSW", min = min_value, max = max_value)
      
    } else {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "CMI")
      
      #define a scale for second axis
      ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
      
      if (ratio %in% c("Inf", "-Inf", "NaN")) {
        ratio <- 0
      }
      
      data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$text_label
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$text_label)
        y_label <- "Variance to Budget $"
      }
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "MSW", metric = metric_choice, min =  min_value,
                  max = max_value, text = text_label, y_label = y_label, ratio = ratio)
      
      
    }
    
  })
  
  
  ### NYEE -------------------------------------
  output$nyee_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "NYEE")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for NYEE")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "Expense to Revenue Ratio") 
      
      data <- data %>% 
        select("month", "year", "Site", "Actual", "Metrics", "date")
      
      history <- Exp_Rev_Ratio %>% filter(Site== "NYEE")
      
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      
      if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.2
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.2
      }
      
      ratio_graph(data, site = "NYEE", min = min_value, max = max_value)
    } else {
      
      # data <- new_repo %>% filter(Site == "MSB" & Metrics == "CMI")
      
      #define a scale for second axis
      ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
      
      if (ratio %in% c("Inf", "-Inf", "NaN")) {
        ratio <- 0
      }
      
      data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$text_label
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$text_label)
        y_label <- "Variance to Budget $"
      }
      
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "NYEE", metric = metric_choice, min =  min_value,
                  max = max_value, text = text_label, y_label = y_label, ratio = ratio)
      
    }
    
  })
  
  
  # Variance Tab ---------------------------
  ### MSHS -------------------------------------
  output$mshs_var <- renderPlot({
    
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSHS"
    
    data <- var_data() %>%
      #data <- new_repo %>% filter(Metrics == metric_option)%>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    if((max(data$Variance, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.2
    }
    
    # Define different labels for metrics    
    if (metric_option %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$text_label
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$text_label)
      y_label <- "Variance to Budget $"
    }
    
    var_graph(data, site= hospital, metric = metric_option, min= min_value, 
              max = max_value, y_label= y_label, text = text_label)
    
  })
  
  ### MSB -------------------------------------
  output$msb_var <- renderPlot({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSB"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    if((max(data$Variance, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.2
    }
    
    # Define different labels for metrics    
    if (metric_option %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$text_label
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$text_label)
      y_label <- "Variance to Budget $"
    }
    
    var_graph(data, site= hospital, metric = metric_option, min= min_value, 
              max = max_value, y_label= y_label, text = text_label)
    
    
  })
  
  ### MSBI -------------------------------------
  output$msbi_var <- renderPlot({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSBI"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    if((max(data$Variance, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.2
    }
    
    # Define different labels for metrics    
    if (metric_option %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$text_label
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$text_label)
      y_label <- "Variance to Budget $"
    }
    
    var_graph(data, site= hospital, metric = metric_option, min= min_value, 
              max = max_value, y_label= y_label, text = text_label)
    
    
  })
  
  ### MSH -------------------------------------
  output$msh_var <- renderPlot({
    
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSH"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    if((max(data$Variance, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.2
    }
    
    # Define different labels for metrics    
    if (metric_option %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$text_label
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$text_label)
      y_label <- "Variance to Budget $"
    }
    
    var_graph(data, site= hospital, metric = metric_option, min= min_value, 
              max = max_value, y_label= y_label, text = text_label)
    
  })
  
  ### MSM -------------------------------------
  output$msm_var <- renderPlot({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSM"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    if((max(data$Variance, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.2
    }
    
    # Define different labels for metrics    
    if (metric_option %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$text_label
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$text_label)
      y_label <- "Variance to Budget $"
    }
    
    var_graph(data, site= hospital, metric = metric_option, min= min_value, 
              max = max_value, y_label= y_label, text = text_label)
    
  })
  
  ### MSQ -------------------------------------
  output$msq_var <- renderPlot({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSQ"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    if((max(data$Variance, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.2
    }
    
    # Define different labels for metrics    
    if (metric_option %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$text_label
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$text_label)
      y_label <- "Variance to Budget $"
    }
    
    var_graph(data, site= hospital, metric = metric_option, min= min_value, 
              max = max_value, y_label= y_label, text = text_label)
    
  })
  
  ### MSSN -------------------------------------
  output$mssn_var <- renderPlot({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSSN"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    if((max(data$Variance, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.2
    }
    
    # Define different labels for metrics    
    if (metric_option %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$text_label
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$text_label)
      y_label <- "Variance to Budget $"
    }
    
    var_graph(data, site= hospital, metric = metric_option, min= min_value, 
              max = max_value, y_label= y_label, text = text_label)
    
  })
  
  ### MSW -------------------------------------
  output$msw_var <- renderPlot({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSW"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    if((max(data$Variance, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.2
    }
    
    # Define different labels for metrics    
    if (metric_option %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$text_label
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$text_label)
      y_label <- "Variance to Budget $"
    }
    
    var_graph(data, site= hospital, metric = metric_option, min= min_value, 
              max = max_value, y_label= y_label,text = text_label)
    
    
  })
  
  ### NYEE -------------------------------------
  output$nyee_var <- renderPlot({
    metric_option <- isolate(input$var_metrics)
    hospital <- "NYEE"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    if((max(data$Variance, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.2
    }
    
    # Define different labels for metrics    
    if (metric_option %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$text_label
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$text_label)
      y_label <- "Variance to Budget $"
    }
    
    var_graph(data, site= hospital, metric = metric_option, min= min_value, 
              max = max_value, y_label= y_label, text = text_label)
    
  })
  
  
  # YTD Variance To Budget Ratio ----------------
  ### MSHS -----------------------
  output$mshs_plot_ytd <- renderPlot({
    
    hospital <- "MSHS"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = metric_option, 
              min = min_value_ytd, max= max_value_ytd) 
  })
  
  ### MSB -----------------------
  output$msb_plot_ytd <- renderPlot({
    
    hospital <- "MSB"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = metric_option, 
              min = min_value_ytd, max= max_value_ytd) 
  })
  
  ### MSBI -----------------------
  output$msbi_plot_ytd <- renderPlot({
    hospital <- "MSBI"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = metric_option, 
              min = min_value_ytd, max= max_value_ytd) 
  })
  
  ### MSH -----------------------
  output$msh_plot_ytd <- renderPlot({
    hospital <- "MSH"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = metric_option, 
              min = min_value_ytd, max= max_value_ytd) 
  })
  
  ### MSM -----------------------
  output$msm_plot_ytd <- renderPlot({
    
    hospital <- "MSM"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = metric_option, 
              min = min_value_ytd, max= max_value_ytd) 
  })
  
  ### MSQ -----------------------
  output$msq_plot_ytd <- renderPlot({
    
    hospital <- "MSQ"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = metric_option, 
              min = min_value_ytd, max= max_value_ytd) 
  })
  
  ### MSSN -----------------------
  output$mssn_plot_ytd <- renderPlot({
    hospital <- "MSSN"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = metric_option, 
              min = min_value_ytd, max= max_value_ytd) 
    
  })
  
  ### MSW -----------------------
  output$msw_plot_ytd <- renderPlot({
    
    hospital <- "MSW"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = metric_option, 
              min = min_value_ytd, max= max_value_ytd) 
  })

  ### NYEE -----------------------
  output$nyee_plot_ytd <- renderPlot({
    
    hospital <- "NYEE"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = metric_option, 
              min = min_value_ytd, max= max_value_ytd) 
  })
  
  
  # All sites visualization ------------------------------
  ## summary tab --------------------
  
  output$ratio_plot <- renderPlot({
    data <- metric_data() %>% 
      filter(Metrics == "Expense to Revenue Ratio" ) %>%
      select("month", "year", "Site", "Actual", "Metrics", "date")
    
    hospital <- isolate(input$all_hospital)
    
    history <- Exp_Rev_Ratio %>% filter(Site== hospital)
    
    
    data <- rbind(history, data) %>%
      mutate(Actual = round(Actual, 2))%>%
      arrange(date) %>%
      slice(tail(row_number(), 12))
    
    #data <- new_repo %>% filter(Site == "MSHS", Metrics == "Expense to Revenue Ratio")
    
    validate(need(nrow(data)>0, paste0("Expense to Revenue Ratio is not available for ", isolate(input$all_hospital))))
    
    
    if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Actual, na.rm = TRUE))*1.2
    }
    
    if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Actual, na.rm = TRUE))*1.2
    }
    
    ratio_graph(data, site = hospital, min= min_value, max= max_value) 
    
  })
  
  
  output$revenue_plot <- renderPlot({
    metric_choice <-  "Total Hospital Revenue"
    
    data <-  metric_data() %>%
      filter(Metrics ==  metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- paste0("$", data$`Variance`)
    y_label <- "Variance to Budget $"
 
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  
  output$expense_plot <- renderPlot({
    
    metric_choice <-  "Total Hospital Expenses"
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%   metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- paste0("$", data$`Variance`)
    y_label <- "Variance to Budget $"
    
   
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  output$discharges_plot <- renderPlot({
    
    metric_choice <-  "Discharges"
    
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%  metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- data$`Variance`
    y_label <- "Variance to Budget"
    
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  output$cmi_plot <- renderPlot({
    
    metric_choice <-  "CMI"
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%  metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- data$`Variance`
    y_label <- "Variance to Budget"
    
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  
  output$alos_plot <- renderPlot({
    metric_choice <-  "ALOS"
    
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%  metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- data$`Variance`
    y_label <- "Variance to Budget"
    
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  
  output$outpt_plot <- renderPlot({
    
    metric_choice <-  "Outpatient Revenue"
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%  metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- paste0("$", data$`Variance`)
    y_label <- "Variance to Budget $"
    
    
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  output$operate_plot <- renderPlot({
    
    metric_choice <-  "340B/Other Operating Revenue"
    
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%  metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- paste0("$", data$`Variance`)
    y_label <- "Variance to Budget $"
    
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  output$salary_plot <- renderPlot({
    
    metric_choice <-  "Salaries and Benefits"
    
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%  metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- paste0("$", data$`Variance`)
    y_label <- "Variance to Budget $"
    
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  output$supply_plot <- renderPlot({
    
    metric_choice <-  "Supplies & Expenses"
    
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%  metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- paste0("$", data$`Variance`)
    y_label <- "Variance to Budget $"
    
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  output$nurse_plot <- renderPlot({
    
    metric_choice <- "Nursing Agency Costs" 
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%  metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0(metric_choice , " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- paste0("$", data$`Variance`)
    y_label <- "Variance to Budget $"
    
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  output$carts_plot <- renderPlot({
    metric_choice <- "CARTS" 
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in% metric_choice)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    carts<<- data
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    #define a scale for second axis
    ratio <- max(abs(data$Variance), na.rm = TRUE)/ max(abs(data$Variance.From.Budget.YTD), na.rm = TRUE)
    
    if (ratio %in% c("Inf", "-Inf", "NaN")) {
      ratio <- 0
    }
    
    data <- data %>% mutate(Variance_scaled = Variance.From.Budget.YTD * ratio)
    
    
    if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.2
    }
    
    # Define labels for metrics    
    text_label <- paste0("$", data$`Variance`)
    y_label <- "Variance to Budget $"
    
    graph_style(data, site = isolate(input$all_hospital), metric = metric_choice, min =  min_value,
                max = max_value, text = text_label, y_label = y_label, ratio = ratio)
  })
  
  ## Monthly Variance tab ----------------------
  output$ratio_plot_var <- renderPlot({
    data <- metric_data_var() %>% 
      filter(Metrics == "Expense to Revenue Ratio" ) %>%
      select("month", "year", "Site", "Actual", "Metrics", "date")
    
    hospital <- isolate(input$all_hospital_var)
    
    history <- Exp_Rev_Ratio %>% filter(Site== hospital)
    
    
    data <- rbind(history, data) %>%
      mutate(Actual = round(Actual, 2))%>%
      arrange(date) %>%
      slice(tail(row_number(), 12))
  
    
    validate(need(nrow(data)>0, paste0("Expense to Revenue Ratio is not available for ", isolate(input$all_hospital_var))))
    
    
    if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Actual, na.rm = TRUE))*1.2
    }
    
    if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Actual, na.rm = TRUE))*1.2
    }
    
   
    ratio_graph(data, site = hospital, min= min_value, max= max_value) 
    
  })
  
  
  output$revenue_plot_var <- renderPlot({
    
    hospital <- isolate(input$all_hospital_var)
    metric_option <- "Total Hospital Revenue"
    
    
   data <-  metric_data_var() %>%
   #   data <- new_repo %>% filter(Site == hospital)%>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
  
  
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
    
  y_label <- "Variance to Budget $"
  text_label <- paste0("$", data$text_label)
  var_graph(data, site =hospital, metric = metric_option, 
            min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  output$expense_plot_var <- renderPlot({
    
    metric_option <- "Total Hospital Expenses"
    hospital <- isolate(input$all_hospital_var)
    
    
    data <-  metric_data_var() %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    y_label <- "Variance to Budget $"
    text_label <- paste0("$", data$text_label)
    var_graph(data, site =hospital, metric = metric_option, 
              min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  output$discharges_plot_var <- renderPlot({
    
    metric_option <-  "Discharges"
    hospital <- isolate(input$all_hospital_var)
      
      data <-  metric_data_var() %>%
        filter(Metrics ==  metric_option)%>%
        mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
               text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
      
      validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
      
      
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
      
      y_label <- "Variance to Budget"
      text_label <- data$text_label
      var_graph(data, site =hospital, metric = metric_option, 
                min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  output$cmi_plot_var <- renderPlot({
    metric_option <-  "CMI"
    hospital <- isolate(input$all_hospital_var)
    
    
    data <-  metric_data_var() %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    y_label <- "Variance to Budget"
    text_label <- data$text_label
    var_graph(data, site =hospital, metric = metric_option, 
              min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  
  output$alos_plot_var <- renderPlot({
    metric_option <-  "ALOS"
    hospital <- isolate(input$all_hospital_var)

    data <-  metric_data_var() %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    y_label <- "Variance to Budget"
    text_label <- data$text_label
    var_graph(data, site =hospital, metric = metric_option, 
              min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  
  output$outpt_plot_var <- renderPlot({
    metric_option <-  "Outpatient Revenue"  
    hospital <- isolate(input$all_hospital_var)
    
    
    data <-  metric_data_var() %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
    
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
    
    y_label <- "Variance to Budget $"
    text_label <- paste0("$", data$text_label)
    var_graph(data, site =hospital, metric = metric_option, 
              min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  
  output$operate_plot_var <- renderPlot({
    metric_option <-  "340B/Other Operating Revenue" 
    hospital <- isolate(input$all_hospital_var)
    
    data <-  metric_data_var() %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
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
    
    y_label <- "Variance to Budget $"
    text_label <- paste0("$", data$text_label)
    var_graph(data, site =hospital, metric = metric_option, 
              min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  output$salary_plot_var <- renderPlot({
     
      metric_option <-  "Salaries and Benefits" 
      hospital <- isolate(input$all_hospital_var)
      
      data <-  metric_data_var() %>%
        filter(Metrics ==  metric_option)%>%
        mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
               text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
      
      validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
      
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
      
      y_label <- "Variance to Budget $"
      text_label <- paste0("$", data$text_label)
      var_graph(data, site =hospital, metric = metric_option, 
                min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  
  output$supply_plot_var <- renderPlot({
      metric_option <-  "Supplies & Expenses" 
      hospital <- isolate(input$all_hospital_var)
      
      data <-  metric_data_var() %>%
        filter(Metrics ==  metric_option)%>%
        mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
               text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
      
      validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
      
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
      
      y_label <- "Variance to Budget $"
      text_label <- paste0("$", data$text_label)
      var_graph(data, site =hospital, metric = metric_option, 
                min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  
  output$nurse_plot_var <- renderPlot({
      metric_option <-  "Nursing Agency Costs" 
      hospital <- isolate(input$all_hospital_var)
      
      data <-  metric_data_var() %>%
        filter(Metrics ==  metric_option)%>%
        mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
               text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
      
      validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
      
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
      
      y_label <- "Variance to Budget $"
      text_label <- paste0("$", data$text_label)
      var_graph(data, site =hospital, metric = metric_option, 
                min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  

  output$carts_plot_var <- renderPlot({
    metric_option <-  "CARTS" 
    hospital <- isolate(input$all_hospital_var)
    
    data <-  metric_data_var() %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", abs(as.numeric(Variance)), ")"), Variance))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
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
    
    y_label <- "Variance to Budget $"
    text_label <- paste0("$", data$text_label)
    var_graph(data, site =hospital, metric = metric_option, 
         min = min_value, max = max_value, y_label = y_label, text = text_label )
  })
  
  ## YTD variance to budget ratio --------------------
  output$ratio_plot_ytd <- renderPlot({
    data <- metric_data_ytd() %>% 
      filter(Metrics == "Expense to Revenue Ratio" ) %>%
      select("month", "year", "Site", "Actual", "Metrics", "date")
    
    hospital <- isolate(input$all_hospital_ytd)
    
    history <- Exp_Rev_Ratio %>% filter(Site == hospital)
    
    
    data <- rbind(history, data) %>%
      mutate(Actual = round(Actual, 2))%>%
      arrange(date) %>%
      slice(tail(row_number(), 12))
    
    
    validate(need(nrow(data)>0, paste0("Expense to Revenue Ratio is not available for ", isolate(input$all_hospital_ytd))))
    
    
    if((max(data$Actual, na.rm = TRUE))*1.2 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Actual, na.rm = TRUE))*1.2
    }
    
    if( (min(data$Actual, na.rm = TRUE))*1.2 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Actual, na.rm = TRUE))*1.2
    }
    
    ratio_graph(data, site = hospital, min= min_value, max= max_value) 
    
  })
  
  output$revenue_plot_ytd <- renderPlot({
    
    Metric_option <- "Total Hospital Revenue"
    
    data <- metric_data_ytd()%>%
    #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  output$expense_plot_ytd <- renderPlot({
    
    Metric_option <- "Total Hospital Expenses"
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  output$discharges_plot_ytd<- renderPlot({
    
    Metric_option <- "Discharges" 
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  
  output$cmi_plot_ytd <- renderPlot({
    
    Metric_option <- "CMI"
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  
  output$alos_plot_ytd <- renderPlot({
    
    Metric_option <- "ALOS"
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  
  output$outpt_plot_ytd <- renderPlot({
    
    Metric_option <- "Outpatient Revenue"
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  output$operate_plot_ytd <- renderPlot({
    
    Metric_option <- "340B/Other Operating Revenue"
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  output$salary_plot_ytd <- renderPlot({
    
    Metric_option <- "Salaries and Benefits" 
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  output$supply_plot_ytd <- renderPlot({
    
    Metric_option <- "Supplies & Expenses" 
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  output$nurse_plot_ytd <- renderPlot({
    
    Metric_option <- "Nursing Agency Costs"
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  output$carts_plot_ytd <- renderPlot({
    
    Metric_option <- "CARTS"
    
    data <- metric_data_ytd()%>%
      #data <- new_repo %>% 
      filter(Metrics == Metric_option)%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)), ")"),
                                  Variance.From.Budget.YTD))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
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
    
    ytd_graph(data, site = hospital, metric = Metric_option, min = min_value_ytd, max = max_value_ytd)
    
  })
  
  # Ratio tab --------------------------------------
  
  output$ratio_plot_all <- renderPlot({
    
    data <- ratio_data()
 
    data <- data %>%
      mutate(Actual = round(Actual, 2))%>%
      arrange(date) %>% 
      mutate(month= month.abb[as.numeric(month)],
                            year = as.factor(year))

    ggplot(data, 
           aes(x=month, y=Actual, fill= year, group = year))+
      geom_bar(position= position_dodge(),stat="identity", width=0.7)+
      scale_fill_manual(values=c("#d80b8c",	"#00aeef","#863198", "#212070"))+
      labs(x = "Date", y = "Expense to Revenue Ratio" , 
           title = isolate(paste0(input$ratio_hospital, " Expense to Revenue Ratio" ))
      )+
      guides(fill=guide_legend(title="Year"))+
      theme_bw()+
      theme(plot.title = element_text(size = 20, hjust = 0.5), 
            legend.position='top', 
            legend.justification='center',
            legend.direction='horizontal',
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),
            axis.text.y = element_text(size = 10, face = "bold"),
            panel.grid.major = element_line(color = "lightgrey"),
            panel.grid.minor = element_line(color = "lightgrey"))+
      scale_x_discrete(limits = month.abb)+
      scale_y_continuous(limits = c(0, max(data$Actual)*1.2), breaks= pretty_breaks())+
      geom_text(aes(label= Actual, x=month, y= Actual), color="#212070",
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Actual), size = 4)+
      geom_hline(aes(yintercept= 1), colour="#990000", linetype="dashed")+
      geom_hline(aes(yintercept = 0))
    
    
    
  })
  
}