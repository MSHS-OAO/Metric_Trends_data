
# Server
server <- function(input, output, session) {
  
  observeEvent(input$download1, {
    screenshot(filename = "MSHS Metrics Trends Dashboard")
  })
  
  # Text output ---------------------------------
  ### Text output for Metrics tab -------------------------------
  system_text <- eventReactive(input$mshs_filters_update, {
    end_date <- isolate(input$mshs_date_range[1])
    
    if (input$mshs_metrics == "Expense to Revenue Ratio"){
      index <- which(date_options == end_date)
      start_date <- date_options[index+11]
    } else{
      start_date <- isolate(tail(input$mshs_date_range, 1))
    }
    
    metric <- isolate(input$mshs_metrics)
   paste0("Based on data from ", start_date, " to ", end_date, " for ", metric)
  }, ignoreNULL = FALSE)
  
  var_text <- eventReactive(input$mshs_filters_update_var, {
    start_date <- isolate(tail(input$var_date_range, 1))
    end_date <- isolate(input$var_date_range[1])
    metric <- isolate(input$var_metrics)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", metric)
  }, ignoreNULL = FALSE)
  
  
  ytd_text <- eventReactive(input$mshs_filters_update_ytd, {
    start_date <- isolate(tail(input$mshs_date_range_ytd, 1))
    end_date <- isolate(input$mshs_date_range_ytd[1])
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
    start_date <- isolate(tail(input$all_date_range, 1))
    end_date <- isolate(input$all_date_range[1])
    hospitals <- isolate(input$all_hospital)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", hospitals)
  }, ignoreNULL = FALSE)
  
  all_mshs_text_var <- eventReactive(input$all_filters_update_var, {
    start_date <- isolate(tail(input$all_date_range_var, 1))
    end_date <- isolate(input$all_date_range_var[1])
    hospitals <- isolate(input$all_hospital_var)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", hospitals)
  }, ignoreNULL = FALSE)
  
  all_mshs_text_ytd <- eventReactive(input$all_filters_update_ytd, {
    start_date <- isolate(tail(input$all_date_range_ytd, 1))
    end_date <- isolate(input$all_date_range_ytd[1])
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
    end_date <- isolate(tail(input$ratio_date_range, 1))
    start_date <- isolate(input$ratio_date_range[1])
    hospitals <- isolate(input$ratio_hospital)
    paste0("Based on data from ", end_date, " to ", start_date, " for ", hospitals)
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
      #filter(year %in% max(year)) %>%
      filter(Metrics %in% input$mshs_metrics,
             date %in% input$mshs_date_range)
  }, ignoreNULL = FALSE)
  
  
  
  ### eventReactive for Metrics Tab: Variance ------------------------------
  var_data  <- eventReactive(input$mshs_filters_update_var, {
    validate(need(input$var_metrics != "", "Please Select a Metric"),
             need(input$var_date_range != "", "Please Select a Date"))
    
    new_repo %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in% input$var_metrics,
             date %in% input$var_date_range)
  }, ignoreNULL = FALSE)
  
  
  ### eventReactive for Metrics Tab: YTD ------------------------------
  mshs_data_ytd  <- eventReactive(input$mshs_filters_update_ytd, {
    validate(need(input$mshs_metrics_ytd != "", "Please Select a Metric"),
             need(input$mshs_date_range_ytd != "", "Please Select a Date"))
    
    new_repo %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in% input$mshs_metrics_ytd,
             date %in% input$mshs_date_range_ytd)
  }, ignoreNULL = FALSE)
  
  
  ### eventReactive for hospitals tab ------------------------------
  metric_data  <- eventReactive(input$all_filters_update,{
    validate(need(input$all_hospital != "", "Please Select a Hospital"),
             need(input$all_date_range != "", "Please Select a Date"))
    
    new_repo %>%
      #filter(year %in% max(year)) %>%
      filter(Site %in% input$all_hospital,
             date %in% input$all_date_range)
  }, ignoreNULL = FALSE)
  
  metric_data_var  <- eventReactive(input$all_filters_update_var,{
    validate(need(input$all_hospital_var != "", "Please Select a Hospital"),
             need(input$all_date_range_var != "", "Please Select a Date"))
    
    new_repo %>%
      #filter(year %in% max(year)) %>%
      filter(Site %in% input$all_hospital_var,
             date %in% input$all_date_range_var)
  }, ignoreNULL = FALSE)
  
  metric_data_ytd  <- eventReactive(input$all_filters_update_ytd,{
    validate(need(input$all_hospital_ytd != "", "Please Select a Hospital"),
             need(input$all_date_range_ytd != "", "Please Select a Date"))
    
    new_repo %>%
     # filter(year %in% max(year)) %>%
      filter(Site %in% input$all_hospital_ytd,
             date %in% input$all_date_range_ytd)
  }, ignoreNULL = FALSE)
  
  
  ### eventReactive for ratio tab ------------------------------
  ratio_data  <- eventReactive(input$ratio_filters_update,{
    validate(need(input$ratio_hospital != "", "Please Select a Hospital"),
             need(input$ratio_date_range != "", "Please Select a Date"))
             
    # data <-  new_repo  %>% 
    #   filter(Metrics == "Expense to Revenue Ratio") %>%
    #   select("month", "year", "Site", "Actual", "Metrics", "date")
    # 
    # data <- rbind(Exp_Rev_Ratio, data)
    
    Exp_Rev_Ratio %>%
      filter(Site %in% input$ratio_hospital,
             date %in% input$ratio_date_range)
  }, ignoreNULL = FALSE)
 
  
  # Metrics visualization ------------------------------
  # System Summary -----------------------------
  ### MSHS -------------------------------------
  output$mshs_plot <- renderPlotly({
   
    metric_option <- isolate(input$mshs_metrics)
    site_option <- "MSHS"
    
    

    if (metric_option %in% c("Expense to Revenue Ratio")) {
     
      data <- mshs_data() %>% 
        filter(Site == site_option)%>%
        select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
      
      history <- Exp_Rev_Ratio %>%
        filter(Site== site_option,
                 sortedDate < min(data$sortedDate))
      
  
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>% 
        slice(tail(row_number(), 12))
      ratio_graph(data, site = site_option, min_ratio = 0.7, max_ratio = 1.9)
      
    } else {
      data <- mshs_data() %>%
        filter(Site == site_option)%>% ungroup()%>%
        mutate(text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
               ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                    Variance.From.Budget.YTD*100))
      
      data <- data %>%
        filter(!is.na(Variance)) %>%
        filter(!is.na(Variance.From.Budget.YTD))
      
      max_range <- max(abs(min(mshs_data()$Variance, na.rm = TRUE))*1.2,
                       max(mshs_data()$Variance, na.rm = TRUE)*1.2)
      
      
      y_range <- c(-max_range, max_range)
      
      
      max_ratio  <- max(abs(min(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                        max(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
      ratio_range <- c(-max_ratio, max_ratio)
      
      

      validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", site_option)))
      graph_style_break(data = data, site = site_option, metric= metric_option, 
                        y_range = y_range, ratio_range = ratio_range)
    }
    
  })
  
  ### MSB -------------------------------------
  output$msb_plot <- renderPlotly({
    metric_option <- isolate(input$mshs_metrics)
    site_option <- "MSB"
    
    if (metric_option %in% c("Expense to Revenue Ratio")) {
      
      data <- mshs_data() %>% 
        filter(Site == site_option)%>%
        select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
      
      history <- Exp_Rev_Ratio %>%
        filter(Site== site_option,
               sortedDate < min(data$sortedDate))
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      ratio_graph(data, site = site_option, min_ratio = 0.7, max_ratio = 1.9)
      
    } else {
      data <- mshs_data() %>%
        #filter(year %in% max(year)) %>%
        filter(Site == site_option)%>% ungroup()%>%
        mutate(text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
               ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                    Variance.From.Budget.YTD*100))%>%
                                     arrange(date)
      
      max_range <- max(abs(min(mshs_data()$Variance, na.rm = TRUE))*1.2,
                       max(mshs_data()$Variance, na.rm = TRUE)*1.2)
      
      
      y_range <- c(-max_range, max_range)
      
      
      max_ratio  <- max(abs(min(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                        max(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
      ratio_range <- c(-max_ratio, max_ratio)
      
      
      validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", site_option)))
      graph_style_break(data = data, site = site_option, metric= metric_option, 
                        y_range = y_range, ratio_range = ratio_range)
    }
    
    
  })
  
  
  ### MSBI -------------------------------------
  output$msbi_plot <- renderPlotly({
    metric_option <- isolate(input$mshs_metrics)
    site_option <- "MSBI"
    
    if (metric_option %in% c("Expense to Revenue Ratio")) {
      
      data <- mshs_data() %>% 
        filter(Site == site_option)%>%
        select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
      
      history <- Exp_Rev_Ratio %>%
        filter(Site== site_option,
               sortedDate < min(data$sortedDate))
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      ratio_graph(data, site = site_option, min_ratio = 0.7, max_ratio = 1.9)
      
    } else {
      data <- mshs_data() %>%
        #filter(year %in% max(year)) %>%
        filter(Site == site_option)%>% ungroup()%>%
        mutate(text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
               ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                    Variance.From.Budget.YTD*100))%>%
        ungroup()%>% arrange(date)
      
      max_range <- max(abs(min(mshs_data()$Variance, na.rm = TRUE))*1.2,
                       max(mshs_data()$Variance, na.rm = TRUE)*1.2)
      
      
      y_range <- c(-max_range, max_range)
      
      
      max_ratio  <- max(abs(min(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                        max(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
      ratio_range <- c(-max_ratio, max_ratio)
      
      
      validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", site_option)))
      graph_style_break(data = data, site = site_option, metric= metric_option, 
                        y_range = y_range, ratio_range = ratio_range)
    }
    
  })
  
  ### MSH -------------------------------------
  output$msh_plot <- renderPlotly({
    metric_option <- isolate(input$mshs_metrics)
    site_option <- "MSH"
    
    if (metric_option %in% c("Expense to Revenue Ratio")) {
      
      data <- mshs_data() %>% 
        filter(Site == site_option)%>%
        select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
      
      history <- Exp_Rev_Ratio %>%
        filter(Site== site_option,
               sortedDate < min(data$sortedDate))
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      ratio_graph(data, site = site_option, min_ratio = 0.7, max_ratio = 1.9)
      
    } else {
      data <- mshs_data() %>%
        #filter(year %in% max(year)) %>%
        filter(Site == site_option)%>% ungroup()%>%
        mutate(text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
               ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                    Variance.From.Budget.YTD*100))%>%
                                       arrange(date)
      
      max_range <- max(abs(min(mshs_data()$Variance, na.rm = TRUE))*1.2,
                       max(mshs_data()$Variance, na.rm = TRUE)*1.2)
      
      
      y_range <- c(-max_range, max_range)
      
      
      max_ratio  <- max(abs(min(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                        max(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
      ratio_range <- c(-max_ratio, max_ratio)
      
      
      validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", site_option)))
      graph_style_break(data = data, site = site_option, metric= metric_option, 
                        y_range = y_range, ratio_range = ratio_range)
    }
    
  })
  
  ### MSM -------------------------------------
  output$msm_plot <- renderPlotly({
    metric_option <- isolate(input$mshs_metrics)
    site_option <- "MSM"
    
    if (metric_option %in% c("Expense to Revenue Ratio")) {
      
      data <- mshs_data() %>% 
        filter(Site == site_option)%>%
        select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
      
      history <- Exp_Rev_Ratio %>%
        filter(Site== site_option,
               sortedDate < min(data$sortedDate))
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      ratio_graph(data, site = site_option, min_ratio = 0.7, max_ratio = 1.9)
      
    } else {
      data <- mshs_data() %>%
        #filter(year %in% max(year)) %>%
        filter(Site == site_option)%>% ungroup()%>%
        mutate(text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
               ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                    Variance.From.Budget.YTD*100))%>%
                                      arrange(date)
      
      max_range <- max(abs(min(mshs_data()$Variance, na.rm = TRUE))*1.2,
                       max(mshs_data()$Variance, na.rm = TRUE)*1.2)
      
      
      y_range <- c(-max_range, max_range)
      
      
      max_ratio  <- max(abs(min(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                        max(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
      ratio_range <- c(-max_ratio, max_ratio)
      
      
      validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", site_option)))
      graph_style_break(data = data, site = site_option, metric= metric_option, 
                        y_range = y_range, ratio_range = ratio_range)
    }
    
  })
  
  ### MSQ -------------------------------------
  output$msq_plot <- renderPlotly({
    metric_option <- isolate(input$mshs_metrics)
    site_option <- "MSQ"
    
    if (metric_option %in% c("Expense to Revenue Ratio")) {
      
      data <- mshs_data() %>% 
        filter(Site == site_option)%>%
        select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
      
      history <- Exp_Rev_Ratio %>%
        filter(Site== site_option,
               sortedDate < min(data$sortedDate))
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      ratio_graph(data, site = site_option, min_ratio = 0.7, max_ratio = 1.9)
      
    } else {
      data <- mshs_data() %>%
        #filter(year %in% max(year)) %>%
        filter(Site == site_option)%>% ungroup()
        
        
        # remove NAs or fill them with zero
        if(all(is.na(data$Variance.From.Budget.YTD)) && all(is.na(data$Variance))){
          data <- data %>%
            filter(!is.na(Variance)) %>%
            filter(!is.na(Variance.From.Budget.YTD))
        } else{
          data <- data %>% mutate(Variance = case_when(is.na(Variance) ~ 0,
                                                       TRUE ~ Variance), 
                                  Variance.From.Budget.YTD = case_when(is.na(Variance.From.Budget.YTD) ~ 0,
                                                                       TRUE ~ Variance.From.Budget.YTD),  )
        }
      
        
       data<- data %>% 
         mutate(text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
               ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                    Variance.From.Budget.YTD*100))%>%
                                     arrange(date)
      
      max_range <- max(abs(min(mshs_data()$Variance, na.rm = TRUE))*1.2,
                       max(mshs_data()$Variance, na.rm = TRUE)*1.2)
      
      
      y_range <- c(-max_range, max_range)
      
      
      max_ratio  <- max(abs(min(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                        max(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
      ratio_range <- c(-max_ratio, max_ratio)
      
      
      validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", site_option)))
      graph_style_break(data = data, site = site_option, metric= metric_option, 
                        y_range = y_range, ratio_range = ratio_range)
    }
    
  })
  
  ### MSSN -------------------------------------
  output$mssn_plot <- renderPlotly({
    metric_option <- isolate(input$mshs_metrics)
    site_option <- "MSSN"
    
    if (metric_option %in% c("Expense to Revenue Ratio")) {
      
      data <- mshs_data() %>% 
        filter(Site == site_option)%>%
        select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
      
      history <- Exp_Rev_Ratio %>%
        filter(Site== site_option,
               sortedDate < min(data$sortedDate))
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      ratio_graph(data, site = site_option, min_ratio = 0.7, max_ratio = 1.9)
      
    } else {
      data <- mshs_data() %>%
        #filter(year %in% max(year)) %>%
        filter(Site == site_option)%>% ungroup()
      
      # remove NAs or fill them with zero
      if(all(is.na(data$Variance.From.Budget.YTD)) | all(is.na(data$Variance))){
        data <- data %>%
          filter(!is.na(Variance)) %>%
          filter(!is.na(Variance.From.Budget.YTD))
      } else{
        data <- data %>% mutate(Variance = case_when(is.na(Variance) ~ 0,
                                                     TRUE ~ Variance), 
                                Variance.From.Budget.YTD = case_when(is.na(Variance.From.Budget.YTD) ~ 0,
                                                                     TRUE ~ Variance.From.Budget.YTD),  )
      }
      
      
      data <- data %>% 
        mutate(text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
               ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                    Variance.From.Budget.YTD*100))%>%
        arrange(date)
      
      
      max_range <- max(abs(min(mshs_data()$Variance, na.rm = TRUE))*1.2,
                       max(mshs_data()$Variance, na.rm = TRUE)*1.2)
      
      
      y_range <- c(-max_range, max_range)
      
      
      max_ratio  <- max(abs(min(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                        max(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
      ratio_range <- c(-max_ratio, max_ratio)
      
      validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", site_option)))
      graph_style_break(data = data, site = site_option, metric= metric_option, 
                        y_range = y_range, ratio_range = ratio_range)
    }
    
  })
  
  ### MSW -------------------------------------
  output$msw_plot <- renderPlotly({
    metric_option <- isolate(input$mshs_metrics)
    site_option <- "MSW"
    
    if (metric_option %in% c("Expense to Revenue Ratio")) {
      
      data <- mshs_data() %>% 
        filter(Site == site_option)%>%
        select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
      
      history <- Exp_Rev_Ratio %>%
        filter(Site== site_option,
               sortedDate < min(data$sortedDate))
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))
      ratio_graph(data, site = site_option, min_ratio = 0.7, max_ratio = 1.9)
      
    } else {
      data <- mshs_data() %>%
        #filter(year %in% max(year)) %>%
        filter(Site == site_option)%>% ungroup()%>%
        mutate(text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
               ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                    Variance.From.Budget.YTD*100))%>%
                                      arrange(date)
      
      max_range <- max(abs(min(mshs_data()$Variance, na.rm = TRUE))*1.2,
                       max(mshs_data()$Variance, na.rm = TRUE)*1.2)
      
      
      y_range <- c(-max_range, max_range)
      
      
      max_ratio  <- max(abs(min(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                        max(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
      ratio_range <- c(-max_ratio, max_ratio)
      
      
      validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", site_option)))
      graph_style_break(data = data, site = site_option, metric= metric_option, 
                        y_range = y_range, ratio_range = ratio_range)
    }
    
    
  })
  
  
  ### NYEE -------------------------------------
  output$nyee_plot <- renderPlotly({
    metric_option <- isolate(input$mshs_metrics)
    site_option <- "NYEE"
    
   
    
    if (metric_option %in% c("Expense to Revenue Ratio")) {
      
      data <- mshs_data() %>% 
        filter(Site == site_option)%>%
        select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
      
      history <- Exp_Rev_Ratio %>%
        filter(Site== site_option,
               sortedDate < min(data$sortedDate))
      
      data <- rbind(history, data) %>%
        mutate(Actual = round(Actual, 2))%>%
        arrange(date) %>%
        slice(tail(row_number(), 12))

      ratio_graph(data, site = site_option, min_ratio = 0.7, max_ratio = 1.9)
      
    } else {
      data <- mshs_data() %>%
        #filter(year %in% max(year)) %>%
        filter(Site == site_option)%>%  ungroup()
      
      # remove NAs or fill them with zero
      if(all(is.na(data$Variance.From.Budget.YTD)) | all(is.na(data$Variance))){
        data <- data %>%
          filter(!is.na(Variance)) %>%
          filter(!is.na(Variance.From.Budget.YTD))
      } else{
        data <- data %>% mutate(Variance = case_when(is.na(Variance) ~ 0,
                                                     TRUE ~ Variance), 
                                Variance.From.Budget.YTD = case_when(is.na(Variance.From.Budget.YTD) ~ 0,
                                                                     TRUE ~ Variance.From.Budget.YTD),  )
      }

      data <- data %>% 
        mutate(text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
               ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                    Variance.From.Budget.YTD*100))%>%
               arrange(date)
      
     
      data <- data %>%
        filter(!is.na(Variance)) %>%
        filter(!is.na(Variance.From.Budget.YTD))
      
      validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", site_option)))
      max_range <- max(abs(min(mshs_data()$Variance, na.rm = TRUE))*1.2,
                       max(mshs_data()$Variance, na.rm = TRUE)*1.2)
      
      
      y_range <- c(-max_range, max_range)
      
      
      max_ratio  <- max(abs(min(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                        max(mshs_data()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
      ratio_range <- c(-max_ratio, max_ratio)
      
      
      
      graph_style_break(data = data, site = site_option, metric= metric_option, 
                        y_range = y_range, ratio_range = ratio_range)
    }

    
  })
  
  
  # Variance Tab ---------------------------
  ### MSHS -------------------------------------
  output$mshs_var <- renderPlotly({
    
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSHS"
    
    data <- var_data() %>%
      #data <- new_repo %>% filter(Metrics == metric_option)%>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    data <- data %>%
      filter(!is.na(Variance)) 
    
    y_range <- c(min(var_data()$Variance, na.rm = TRUE)*1.2, 
                 max(var_data()$Variance, na.rm = TRUE)*1.2)
    
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))

    
    var_graph_break(data, site= hospital, metric = metric_option, y_range = y_range)
    
  })
  
  ### MSB -------------------------------------
  output$msb_var <- renderPlotly({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSB"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    
    y_range <- c(min(var_data()$Variance, na.rm = TRUE)*1.2, 
                 max(var_data()$Variance, na.rm = TRUE)*1.2)
    
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    var_graph_break(data, site= hospital, metric = metric_option, y_range = y_range)
    
    
    
  })
  
  ### MSBI -------------------------------------
  output$msbi_var <- renderPlotly({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSBI"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    y_range <- c(min(var_data()$Variance, na.rm = TRUE)*1.2, 
                 max(var_data()$Variance, na.rm = TRUE)*1.2)
    
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    var_graph_break(data, site= hospital, metric = metric_option, y_range = y_range)
    
    
    
  })
  
  ### MSH -------------------------------------
  output$msh_var <- renderPlotly({
    
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSH"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    y_range <- c(min(var_data()$Variance, na.rm = TRUE)*1.2, 
                 max(var_data()$Variance, na.rm = TRUE)*1.2)
    
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    var_graph_break(data, site= hospital, metric = metric_option, y_range = y_range)
    
    
  })
  
  ### MSM -------------------------------------
  output$msm_var <- renderPlotly({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSM"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    y_range <- c(min(var_data()$Variance, na.rm = TRUE)*1.2, 
                 max(var_data()$Variance, na.rm = TRUE)*1.2)
    
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    var_graph_break(data, site= hospital, metric = metric_option, y_range = y_range)
    
    
  })
  
  ### MSQ -------------------------------------
  output$msq_var <- renderPlotly({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSQ"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    y_range <- c(min(var_data()$Variance, na.rm = TRUE)*1.2, 
                 max(var_data()$Variance, na.rm = TRUE)*1.2)
    
    
    # remove NAs or fill them with zero
    if(all(is.na(data$Variance)) | all(data$Variance == 0)){
      data <- data %>%
        filter(!is.na(Variance)) %>%
        filter(Variance != 0)
    } else{
      data <- data %>% mutate(Variance = case_when(is.na(Variance) ~ 0,
                                                   TRUE ~ Variance))
    }
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    var_graph_break(data, site= hospital, metric = metric_option, y_range = y_range)
    
    
  })
  
  ### MSSN -------------------------------------
  output$mssn_var <- renderPlotly({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSSN"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    y_range <- c(min(var_data()$Variance, na.rm = TRUE)*1.2, 
                 max(var_data()$Variance, na.rm = TRUE)*1.2)
    
    
    # remove NAs or fill them with zero
    if(all(is.na(data$Variance)) | all(data$Variance == 0)){
      data <- data %>%
        filter(!is.na(Variance)) %>%
        filter(Variance != 0)
    } else{
      data <- data %>% mutate(Variance = case_when(is.na(Variance) ~ 0,
                                                   TRUE ~ Variance))
    }
    
    
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    var_graph_break(data, site= hospital, metric = metric_option, y_range = y_range)
    
    
  })
  
  ### MSW -------------------------------------
  output$msw_var <- renderPlotly({
    metric_option <- isolate(input$var_metrics)
    hospital <- "MSW"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    y_range <- c(min(var_data()$Variance, na.rm = TRUE)*1.2, 
                 max(var_data()$Variance, na.rm = TRUE)*1.2)
    
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    var_graph_break(data, site= hospital, metric = metric_option, y_range = y_range)
    
    
  })
  
  ### NYEE -------------------------------------
  output$nyee_var <- renderPlotly({
    metric_option <- isolate(input$var_metrics)
    hospital <- "NYEE"
    
    data <- var_data() %>%
      filter(Site == hospital)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    y_range <- c(min(var_data()$Variance, na.rm = TRUE)*1.2, 
                 max(var_data()$Variance, na.rm = TRUE)*1.2)
    
    # remove NAs or fill them with zero
    if(all(is.na(data$Variance)) | all(data$Variance == 0)){
      data <- data %>%
        filter(!is.na(Variance)) %>%
        filter(Variance != 0)
    } else{
      data <- data %>% mutate(Variance = case_when(is.na(Variance) ~ 0,
                                                   TRUE ~ Variance))
    }
    
    
    validate(need(nrow(data) > 0, paste0(metric_option , " is not available for ", hospital)))
    
    
    var_graph_break(data, site= hospital, metric = metric_option, y_range = y_range)
    
    
  })
  
  
  # YTD Variance To Budget Ratio ----------------
  ### MSHS -----------------------
  output$mshs_plot_ytd <- renderPlotly({
    
    hospital <- "MSHS"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>% ungroup() %>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD*100)),")"),
                                  Variance.From.Budget.YTD*100))
    
    data <- data %>%
      filter(!is.na(Variance.From.Budget.YTD))
    
    ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 
                     max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    if(ratio_range[1]>0 & ratio_range[2]>0){
      
      ratio_range <- c(0, max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
    } else if(ratio_range[1]<0 & ratio_range[2]<0){
      ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 0)
    }
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))

    ytd_graph_break(data, site = hospital, metric = metric_option, ratio_range= ratio_range)
             
  })
  
  ### MSB -----------------------
  output$msb_plot_ytd <- renderPlotly({
    
    hospital <- "MSB"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>% ungroup() %>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD*100)), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 
                     max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    if(ratio_range[1]>0 & ratio_range[2]>0){
      
      ratio_range <- c(0, max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
    } else if(ratio_range[1]<0 & ratio_range[2]<0){
      ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 0)
    }
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    ytd_graph_break(data, site = hospital, metric = metric_option, ratio_range= ratio_range)
                 
  })
  
  ### MSBI -----------------------
  output$msbi_plot_ytd <- renderPlotly({
    hospital <- "MSBI"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>% ungroup() %>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD*100)), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 
                     max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    if(ratio_range[1]>0 & ratio_range[2]>0){
      
      ratio_range <- c(0, max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
    } else if(ratio_range[1]<0 & ratio_range[2]<0){
      ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 0)
    }
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    ytd_graph_break(data, site = hospital, metric = metric_option, ratio_range= ratio_range)
                    
  })
  
  ### MSH -----------------------
  output$msh_plot_ytd <- renderPlotly({
    hospital <- "MSH"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>% ungroup() %>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD*100)), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 
                     max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    if(ratio_range[1]>0 & ratio_range[2]>0){
      
      ratio_range <- c(0, max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
    } else if(ratio_range[1]<0 & ratio_range[2]<0){
      ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 0)
    }
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    ytd_graph_break(data, site = hospital, metric = metric_option, ratio_range= ratio_range)
                    
  })
  
  ### MSM -----------------------
  output$msm_plot_ytd <- renderPlotly({
    
    hospital <- "MSM"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>% ungroup() %>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD*100)), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 
                     max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    if(ratio_range[1]>0 & ratio_range[2]>0){
      
      ratio_range <- c(0, max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
    } else if(ratio_range[1]<0 & ratio_range[2]<0){
      ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 0)
    }
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    ytd_graph_break(data, site = hospital, metric = metric_option, ratio_range= ratio_range) 
                    
  })
  
  ### MSQ -----------------------
  output$msq_plot_ytd <- renderPlotly({
    
    hospital <- "MSQ"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>% ungroup() 
    
    
    
    # remove NAs or fill them with zero
    if(all(is.na(data$Variance.From.Budget.YTD)) | all(data$Variance.From.Budget.YTD == 0)){
      data <- data %>%
        filter(!is.na(Variance.From.Budget.YTD)) %>%
        filter(Variance.From.Budget.YTD != 0)
    } else{
      data <- data %>% mutate(Variance.From.Budget.YTD = case_when(is.na(Variance.From.Budget.YTD) ~ 0,
                                                                   TRUE ~ Variance.From.Budget.YTD))
    }
    
    data <- data %>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD*100)), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    
    ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 
                     max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    if(ratio_range[1]>0 & ratio_range[2]>0){
      
      ratio_range <- c(0, max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
    } else if(ratio_range[1]<0 & ratio_range[2]<0){
      ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 0)
    }
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    ytd_graph_break(data, site = hospital, metric = metric_option, ratio_range= ratio_range)
                     
  })
  
  ### MSSN -----------------------
  output$mssn_plot_ytd <- renderPlotly({
    hospital <- "MSSN"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>% ungroup() 
    
    
    
    # remove NAs or fill them with zero
    if(all(is.na(data$Variance.From.Budget.YTD)) | all(data$Variance.From.Budget.YTD == 0)){
      data <- data %>%
        filter(!is.na(Variance.From.Budget.YTD)) %>%
        filter(Variance.From.Budget.YTD != 0)
    } else{
      data <- data %>% mutate(Variance.From.Budget.YTD = case_when(is.na(Variance.From.Budget.YTD) ~ 0,
                                                                   TRUE ~ Variance.From.Budget.YTD))
    }
    
    data <- data %>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD*100)), ")"),
                                  Variance.From.Budget.YTD*100))

    
    ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 
                     max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    if(ratio_range[1]>0 & ratio_range[2]>0){
      
      ratio_range <- c(0, max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
    } else if(ratio_range[1]<0 & ratio_range[2]<0){
      ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 0)
    }
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    ytd_graph_break(data, site = hospital, metric = metric_option, ratio_range= ratio_range)
    
  })
  
  ### MSW -----------------------
  output$msw_plot_ytd <- renderPlotly({
    
    hospital <- "MSW"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>% ungroup() %>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD*100)), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 
                     max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    if(ratio_range[1]>0 & ratio_range[2]>0){
      
      ratio_range <- c(0, max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
    } else if(ratio_range[1]<0 & ratio_range[2]<0){
      ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 0)
    }
    
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    ytd_graph_break(data, site = hospital, metric = metric_option, ratio_range= ratio_range) 
  })

  ### NYEE -----------------------
  output$nyee_plot_ytd <- renderPlotly({
    
    hospital <- "NYEE"
    metric_option <- isolate(input$mshs_metrics_ytd)
    
    data <- mshs_data_ytd()%>%
      filter(Site == hospital)%>% ungroup() 
    
    
    
    # remove NAs or fill them with zero
    if(all(is.na(data$Variance.From.Budget.YTD)) | all(data$Variance.From.Budget.YTD == 0)){
      data <- data %>%
        filter(!is.na(Variance.From.Budget.YTD)) %>%
        filter(Variance.From.Budget.YTD != 0)
    } else{
      data <- data %>% mutate(Variance.From.Budget.YTD = case_when(is.na(Variance.From.Budget.YTD) ~ 0,
                                                                   TRUE ~ Variance.From.Budget.YTD))
    }
    
    data <- data %>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD*100)), ")"),
                                  Variance.From.Budget.YTD*100))
    
 
    ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 
                     max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    if(ratio_range[1]>0 & ratio_range[2]>0){
      
      ratio_range <- c(0, max(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
      
    } else if(ratio_range[1]<0 & ratio_range[2]<0){
      ratio_range <- c(min(mshs_data_ytd()$Variance.From.Budget.YTD, na.rm = TRUE)*1.2, 0)
    }
  
    
    validate(need(nrow(data) > 0, paste0(metric_option, " is not available for ", hospital)))
    
    ytd_graph_break(data, site = hospital, metric = metric_option, ratio_range= ratio_range)
  })
  
  
  # All sites visualization ------------------------------
  ## summary tab --------------------
  
  output$ratio_plot <- renderPlotly({
    
    hospital <- isolate(input$all_hospital)
    
    data <- metric_data() %>% 
      filter(Metrics == "Expense to Revenue Ratio", Site== hospital) %>%
      select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate" )

    
    history <- Exp_Rev_Ratio %>%
      filter(Site== hospital,
             sortedDate < min(data$sortedDate))
    
    
    data <- rbind(history, data) %>%
      ungroup()%>%
      mutate(Actual = round(Actual, 2))%>%
      arrange(date) %>%
      slice(tail(row_number(), 12))
    
    #data <- new_repo %>% filter(Site == "MSHS", Metrics == "Expense to Revenue Ratio")
    
    validate(need(nrow(data)>0, paste0("Expense to Revenue Ratio is not available for ", isolate(input$all_hospital))))
    
    
    max_ratio <- round(max(data$Actual, na.rm = TRUE)*1.2, 1)
    
    ratio_graph(data, site = hospital, min_ratio = 0.7, max_ratio = max_ratio) 
    
  })
  
  
  output$revenue_plot <- renderPlotly({
    metric_choice <-  "Total Hospital Revenue"
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      filter(Metrics ==  metric_choice)%>% ungroup()%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ", isolate(input$all_hospital))))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  
  output$expense_plot <- renderPlotly({
    
    metric_choice <-  "Total Hospital Expenses"
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in%   metric_choice)%>% ungroup() %>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  output$discharges_plot <- renderPlotly({
    
    metric_choice <-  "Discharges"
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in%  metric_choice)%>% ungroup()%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    validate(need(nrow(data)>0, paste0(metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  output$cmi_plot <- renderPlotly({
    
    metric_choice <-  "CMI"
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in%   metric_choice)%>% ungroup() %>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  
  output$alos_plot <- renderPlotly({
    metric_choice <-  "ALOS"
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in%   metric_choice)%>% ungroup() %>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    data <- data %>%
      filter(!is.na(Variance)) %>%
      filter(!is.na(Variance.From.Budget.YTD))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  
  output$outpt_plot <- renderPlotly({
    
    metric_choice <-  "Outpatient Revenue"
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in%   metric_choice)%>% ungroup() %>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
    
  })
  
  output$operate_plot <- renderPlotly({
    
    metric_choice <-  "340B/Other Operating Revenue"
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in%   metric_choice)%>% ungroup() %>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  output$salary_plot <- renderPlotly({
    
    metric_choice <-  "Salaries and Benefits"
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in%   metric_choice)%>% ungroup() %>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  output$supply_plot <- renderPlotly({
    
    metric_choice <-  "Supplies & Expenses"
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics %in%   metric_choice)%>% ungroup() %>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  output$nurse_plot <- renderPlotly({
    
    metric_choice <- "Nursing Agency Costs" 
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      filter(Metrics %in%   metric_choice)%>% ungroup() 
    
    # remove NAs or fill them with zero
    if(all(is.na(data$Variance.From.Budget.YTD)) | all(is.na(data$Variance))){
      data <- data %>%
        filter(!is.na(Variance)) %>%
        filter(!is.na(Variance.From.Budget.YTD))
    } else{
      data <- data %>% 
        mutate(Variance = case_when(is.na(Variance) ~ 0,
                                    TRUE ~ Variance), 
               Variance.From.Budget.YTD = case_when(is.na(Variance.From.Budget.YTD) ~ 0,
                                                    TRUE ~ Variance.From.Budget.YTD))
    }
    
    data <- data %>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  output$carts_plot <- renderPlotly({
    metric_choice <- "CARTS" 
    hospital <- isolate(input$all_hospital)
    
    data <-  metric_data() %>%
      filter(Metrics %in%   metric_choice)%>% ungroup() 
    
    # remove NAs or fill them with zero
    if(all(is.na(data$Variance.From.Budget.YTD)) | all(is.na(data$Variance))){
      data <- data %>%
        filter(!is.na(Variance)) %>%
        filter(!is.na(Variance.From.Budget.YTD))
    } else{
      data <- data %>% 
        mutate(Variance = case_when(is.na(Variance) ~ 0,
                                    TRUE ~ Variance), 
               Variance.From.Budget.YTD = case_when(is.na(Variance.From.Budget.YTD) ~ 0,
                                                    TRUE ~ Variance.From.Budget.YTD))
    }
    
    data <- data %>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)),
             ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    validate(need(nrow(data)>0, paste0( metric_choice, " is not available for ",  hospital)))
    
    max_range <- max(abs(min(data$Variance, na.rm = TRUE))*1.2,
                     max(data$Variance, na.rm = TRUE)*1.2)
    
    
    y_range <- c(-max_range, max_range)
    
    
    max_ratio  <- max(abs(min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.2, 
                      max(data$Variance.From.Budget.YTD, na.rm = TRUE)*1.2)
    
    ratio_range <- c(-max_ratio, max_ratio)
    
    graph_style_break(data, site = hospital, metric = metric_choice, 
                      y_range = y_range, ratio_range = ratio_range)
  })
  
  ## Monthly Variance tab ----------------------
  output$ratio_plot_var <- renderPlotly({
    data <- metric_data_var() %>% 
      filter(Metrics == "Expense to Revenue Ratio" ) %>%
      select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
    
    hospital <- isolate(input$all_hospital_var)
    
   
    
    history <- Exp_Rev_Ratio %>%
      filter(Site== hospital,
             sortedDate < min(data$sortedDate))
    
    
    data <- rbind(history, data) %>%
      ungroup()%>%
      mutate(Actual = round(Actual, 2))%>%
      arrange(date) %>%
      slice(tail(row_number(), 12))
  
    
    validate(need(nrow(data)>0, paste0("Expense to Revenue Ratio is not available for ", isolate(input$all_hospital_var))))
    
  
    max_ratio <- round(max(data$Actual, na.rm = TRUE)*1.2, 1)
    
    ratio_graph(data, site = hospital, min_ratio = 0.7, max_ratio = max_ratio) 
    
  })
  
  
  output$revenue_plot_var <- renderPlotly({
    
    hospital <- isolate(input$all_hospital_var)
    metric_option <- "Total Hospital Revenue"
    
    
   data <-  metric_data_var() %>%
     #filter(year %in% max(year)) %>%
   #   data <- new_repo %>% filter(Site == hospital)%>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "black", "red"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))

 
  var_graph(data, site =hospital, metric = metric_option)
            
  })
  
  output$expense_plot_var <- renderPlotly({
    
    metric_option <- "Total Hospital Expenses"
    hospital <- isolate(input$all_hospital_var)
    
    
    data <-  metric_data_var() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
    
    var_graph(data, site =hospital, metric = metric_option)
  })
  
  output$discharges_plot_var <- renderPlotly({
    
    metric_option <-  "Discharges"
    hospital <- isolate(input$all_hospital_var)
      
      data <-  metric_data_var() %>%
        #filter(year %in% max(year)) %>%
        filter(Metrics ==  metric_option)%>%
        mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
               text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
      
      validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
      
      var_graph(data, site =hospital, metric = metric_option)
  })
  
  output$cmi_plot_var <- renderPlotly({
    metric_option <-  "CMI"
    hospital <- isolate(input$all_hospital_var)
    
    
    data <-  metric_data_var() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))

    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
    var_graph(data, site =hospital, metric = metric_option)
  })
  
  
  output$alos_plot_var <- renderPlotly({
    metric_option <-  "ALOS"
    hospital <- isolate(input$all_hospital_var)

    data <-  metric_data_var() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    
    data <- data %>%
      filter(!is.na(Variance))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
    var_graph(data, site =hospital, metric = metric_option)
  })
  
  
  output$outpt_plot_var <- renderPlotly({
    metric_option <-  "Outpatient Revenue"  
    hospital <- isolate(input$all_hospital_var)
    
    
    data <-  metric_data_var() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    var_graph(data, site =hospital, metric = metric_option)
  })
  
  
  output$operate_plot_var <- renderPlotly({
    metric_option <-  "340B/Other Operating Revenue" 
    hospital <- isolate(input$all_hospital_var)
    
    data <-  metric_data_var() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
    
    var_graph(data, site =hospital, metric = metric_option)
  })
  
  output$salary_plot_var <- renderPlotly({
     
      metric_option <-  "Salaries and Benefits" 
      hospital <- isolate(input$all_hospital_var)
      
      data <-  metric_data_var() %>%
        #filter(year %in% max(year)) %>%
        filter(Metrics ==  metric_option)%>%
        mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
               text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
      
      validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
      
      var_graph(data, site =hospital, metric = metric_option)
  })
  
  
  output$supply_plot_var <- renderPlotly({
      metric_option <-  "Supplies & Expenses" 
      hospital <- isolate(input$all_hospital_var)
      
      data <-  metric_data_var() %>%
        #filter(year %in% max(year)) %>%
        filter(Metrics ==  metric_option)%>%
        mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
               text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
      
      validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
      
      var_graph(data, site =hospital, metric = metric_option)
  })
  
  
  output$nurse_plot_var <- renderPlotly({
      metric_option <-  "Nursing Agency Costs" 
      hospital <- isolate(input$all_hospital_var)
      
      data <-  metric_data_var() %>%
        #filter(year %in% max(year)) %>%
        filter(Metrics ==  metric_option)
      
      
      # remove NAs or fill them with zero
      if(all(is.na(data$Variance)) | all(data$Variance== 0)){
        data <- data %>%
          filter(!is.na(Variance)) %>%
          filter(Variance != 0)
      } else{
        data <- data %>% 
          mutate(Variance = case_when(is.na(Variance) ~ 0,
                                      TRUE ~ Variance))
      }
      
      
      data <- data %>%
        mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
               text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
      
      validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
      
      var_graph(data, site =hospital, metric = metric_option)
  })
  

  output$carts_plot_var <- renderPlotly({
    metric_option <-  "CARTS" 
    hospital <- isolate(input$all_hospital_var)
    
    data <-  metric_data_var() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics ==  metric_option)%>%
      mutate(sign = ifelse(Variance >= 0, "positive", "negative"),
             text_label = ifelse(Variance <0 , paste0("(", comma(abs(Variance)), ")"), comma(Variance)))
    
    validate(need(nrow(data)> 0, paste0(metric_option, " is not available for ", hospital)))
  
    var_graph(data, site =hospital, metric = metric_option)
  })
  
  ## YTD variance to budget ratio --------------------
  output$ratio_plot_ytd <- renderPlotly({
    data <- metric_data_ytd() %>% 
      filter(Metrics == "Expense to Revenue Ratio" ) %>%
      select("month", "year", "Site", "Actual", "Metrics", "date", "sortedDate")
    
    hospital <- isolate(input$all_hospital_ytd)
    
    history <- Exp_Rev_Ratio %>%
      filter(Site== hospital,
             sortedDate < min(data$sortedDate))
    
    data <- rbind(history, data) %>%
      ungroup()%>%
      mutate(Actual = round(Actual, 2))%>%
      arrange(date) %>%
      slice(tail(row_number(), 12))
    
    
    validate(need(nrow(data)>0, paste0("Expense to Revenue Ratio is not available for ", isolate(input$all_hospital_ytd))))

    
    max_ratio <- round(max(data$Actual, na.rm = TRUE)*1.2, 1)
    
    ratio_graph(data, site = hospital, min_ratio = 0.7, max_ratio = max_ratio) 
    
    
  })
  
  output$revenue_plot_ytd <- renderPlotly({
    
    Metric_option <- "Total Hospital Revenue"
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    ytd_graph(data, site = hospital, metric = Metric_option)
    
  })
  
  output$expense_plot_ytd <- renderPlotly({
    
    Metric_option <- "Total Hospital Expenses"
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
  
    
    ytd_graph(data, site = hospital, metric = Metric_option)
    
  })
  
  output$discharges_plot_ytd <- renderPlotly({
    
    Metric_option <- "Discharges" 
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    ytd_graph(data, site = hospital, metric = Metric_option)
    
  })
  
  
  output$cmi_plot_ytd <- renderPlotly({
    
    Metric_option <- "CMI"
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
    ytd_graph(data, site = hospital, metric = Metric_option)
  })
  
  
  output$alos_plot_ytd <- renderPlotly({
    
    Metric_option <- "ALOS"
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    data <- data %>%
      filter(!is.na(Variance.From.Budget.YTD))
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
  
    
    ytd_graph(data, site = hospital, metric = Metric_option)
    
  })
  
  
  output$outpt_plot_ytd <- renderPlotly({
    
    Metric_option <- "Outpatient Revenue"
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    ytd_graph(data, site = hospital, metric = Metric_option)
    
  })
  
  output$operate_plot_ytd <- renderPlotly({
    
    Metric_option <- "340B/Other Operating Revenue"
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
    ytd_graph(data, site = hospital, metric = Metric_option)
    
  })
  
  output$salary_plot_ytd <- renderPlotly({
    
    Metric_option <- "Salaries and Benefits" 
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    
    ytd_graph(data, site = hospital, metric = Metric_option)
    
  })
  
  output$supply_plot_ytd <- renderPlotly({
    
    Metric_option <- "Supplies & Expenses" 
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
  
    ytd_graph(data, site = hospital, metric = Metric_option)
  })
  
  output$nurse_plot_ytd <- renderPlotly({
    
    Metric_option <- "Nursing Agency Costs"
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)
    
    
    
    # remove NAs or fill them with zero
    if(all(is.na(data$Variance.From.Budget.YTD)) | all(data$Variance.From.Budget.YTD == 0)){
      data <- data %>%
        filter(Variance.From.Budget.YTD !=0 ) %>%
        filter(!is.na(Variance.From.Budget.YTD))
    } else{
      data <- data %>% 
        mutate(Variance.From.Budget.YTD = case_when(is.na(Variance.From.Budget.YTD) ~ 0,
                                                    TRUE ~ Variance.From.Budget.YTD))
    }
    
    data <- data %>% 
     mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    
    ytd_graph(data, site = hospital, metric = Metric_option)
    
  })
  
  output$carts_plot_ytd <- renderPlotly({
    
    Metric_option <- "CARTS"
    
    data <- metric_data_ytd()%>%
      ungroup() %>%
      #filter(year %in% max(year)) %>%
      filter(Metrics == Metric_option)%>%
      mutate(ratio_label = ifelse(Variance.From.Budget.YTD < 0, paste0("(", abs(as.numeric(Variance.From.Budget.YTD)*100), ")"),
                                  Variance.From.Budget.YTD*100))
    
    
    hospital <- isolate(input$all_hospital_ytd)
    
    validate(need(nrow(data) > 0, paste0(Metric_option, " is not available for ", hospital)))
    

    ytd_graph(data, site = hospital, metric = Metric_option)
  })
  
  # Ratio tab --------------------------------------
  
  output$ratio_plot_all <- renderPlot({
    
    data <- ratio_data() %>%
      mutate(Actual = round(Actual, 2))%>%
      arrange(year, month) %>% 
      mutate(month= month.abb[as.numeric(month)],
                            year = as.factor(year))

    ggplot(data, 
           aes(x=month, y=Actual, fill= year, group = year))+
      geom_bar(position= position_dodge(),stat="identity", width=0.7)+
      scale_fill_manual(values=c("#7f7f7f", "#d80b8c",	"#00aeef","#863198", "#212070"))+
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
            legend.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),
            axis.text.y = element_text(size = 10, face = "bold"),
            panel.grid.major = element_line(color = "lightgrey"),
            panel.grid.minor = element_line(color = "lightgrey"))+
      scale_x_discrete(limits = month.abb)+
      #scale_y_continuous(limits = c(0, max(data$Actual)*1.2), breaks= pretty_breaks())+
      geom_text(aes(label= Actual, x=month, y= Actual), color="#212070",
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Actual), size = 4)+
      geom_hline(aes(yintercept= 1), colour="#990000", linetype="dashed")+
      geom_hline(aes(yintercept = 0))
    
    
    
  })
  
}