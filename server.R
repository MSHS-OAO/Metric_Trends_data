
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
  all_mshs_text <- eventReactive(input$mshs_filters_update, {
    end_date <- isolate(max(input$all_date_range))
    start_date <- isolate(min(input$all_date_range))
    hospitals <- isolate(input$all_hospital)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", hospitals)
  }, ignoreNULL = FALSE)
  
  output$all_date_show  <- renderText({
    all_mshs_text()
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
      
      
      if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.3
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.3
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
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
    
    # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$`Variance`
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$`Variance`)
        y_label <- "Variance to Budget $"
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = y_label, 
            title = isolate(paste0("MSHS ", input$mshs_metrics , 
                                   " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face='bold'),
              legend.text = element_text(size = 6),
              axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              legend.position = "non")+
        geom_text_repel(aes(label= text_label,
                      x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1), fontface = "bold",
                  vjust = 0.5 - sign(data$Variance), size = 4)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        geom_hline(aes(yintercept = 0)) 
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance_scaled, group = 1),
                  colour = "#212070", linewidth = 1.25) +
        geom_point(mapping = aes(date, Variance_scaled),
                   colour = "#212070", size = 2.6) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                        labels = scales::label_percent(scale = 1),
                                                        name = "YTD Variance To Budget Ratio %"))+
        geom_text_repel(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                            x=date, y= Variance_scaled , color = sign.YTD),
                        position = position_dodge(width = 1), fontface='bold',
                        vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4 )
      
      p1
      
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
      
      
      if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.3
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.3
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
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$`Variance`
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$`Variance`)
        y_label <- "Variance to Budget $"
      }
      
      metric_choice <- isolate(input$mshs_metrics)
      graph_style(data, site = "MSB", metric = metric_choice, min =  min_value,
            max = max_value, text = text_label, y_label = y_label, ratio = ratio)
      
      # p1 <- ggplot(data)  + 
      #   geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
      #   labs(x = "Date", y = y_label, 
      #         title = isolate(paste0("MSB ", input$mshs_metrics , " Monthly Variance to Budget")),
      #        subtitle = paste0("($ in Thousands)"))+
      #   theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
      #         plot.subtitle = element_text(hjust = 0.5, size = 10),
      #         axis.title = element_text(face='bold'),
      #         legend.text = element_text(size = 6),
      #         axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
      #         axis.text.y = element_text(face = "bold"),
      #         legend.position = "non")+
      #   geom_text_repel(aes(label= text_label,
      #                 x=date, y= Variance, color = sign),
      #             position = position_dodge(width = 1), fontface = "bold",
      #             vjust = 0.5 - sign(data$Variance), size = 4)+
      #   scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      #   geom_hline(aes(yintercept = 0)) 
      # 
      # p1 <- p1 +
      #   geom_line(mapping = aes(date, Variance_scaled, group = 1),
      #             colour = "#212070", linewidth = 1.25) +
      #   geom_point(mapping = aes(date, Variance_scaled),
      #              colour = "#212070", size = 2.6) +
      #   scale_y_continuous(limits=c(min_value, max_value), 
      #                      sec.axis = ggplot2::sec_axis(~. / ratio , 
      #                                                   labels = scales::label_percent(scale = 1),
      #                                                   name = "YTD Variance To Budget Ratio %"))+
      #   geom_text_repel(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
      #                       x=date, y= Variance_scaled , color = sign.YTD),
      #                   position = position_dodge(width = 1), fontface='bold',
      #                   vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4 )
      # 
      # p1
      
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
      
      if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.3
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.3
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
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$`Variance`
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$`Variance`)
        y_label <- "Variance to Budget $"
      }
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = y_label, 
             title = isolate(paste0("MSBI ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face='bold'),
              legend.text = element_text(size = 6),
              axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              legend.position = "non")+
        geom_text_repel(aes(label= text_label,
                      x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1), fontface = "bold",
                  vjust = 0.5 - sign(data$Variance), size = 4)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        geom_hline(aes(yintercept = 0)) 
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance_scaled, group = 1),
                  colour = "#212070", linewidth = 1.25) +
        geom_point(mapping = aes(date, Variance_scaled),
                   colour = "#212070", size = 2.6) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                        labels = scales::label_percent(scale = 1),
                                                        name = "YTD Variance To Budget Ratio %"))+
        geom_text_repel(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                            x=date, y= Variance_scaled , color = sign.YTD),
                        position = position_dodge(width = 1), fontface='bold',
                        vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4 )
      
      p1
      
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
      
      
      if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.3
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.3
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
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$`Variance`
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$`Variance`)
        y_label <- "Variance to Budget $"
      }
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = y_label, 
             title = isolate(paste0("MSH ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face='bold'),
              legend.text = element_text(size = 6),
              axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              legend.position = "non")+
        geom_text_repel(aes(label= text_label,
                      x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1), fontface = "bold",
                  vjust = 0.5 - sign(data$Variance), size = 4)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        geom_hline(aes(yintercept = 0)) 
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance_scaled, group = 1),
                  colour = "#212070", linewidth = 1.25) +
        geom_point(mapping = aes(date, Variance_scaled),
                   colour = "#212070", size = 2.6) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                        labels = scales::label_percent(scale = 1),
                                                        name = "YTD Variance To Budget Ratio %"))+
        geom_text_repel(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                            x=date, y= Variance_scaled , color = sign.YTD),
                        position = position_dodge(width = 1), fontface='bold',
                        vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4 )
      
      p1
      
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
      
      if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.3
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.3
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
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$`Variance`
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$`Variance`)
        y_label <- "Variance to Budget $"
      }
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = y_label, 
             title = isolate(paste0("MSM ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face='bold'),
              axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              legend.text = element_text(size = 6),
              legend.position = "non")+
        geom_text_repel(aes(label= text_label,
                      x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1), fontface = "bold",
                  vjust = 0.5 - sign(data$Variance), size = 4)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        geom_hline(aes(yintercept = 0)) 
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance_scaled, group = 1),
                  colour = "#212070", linewidth = 1.25) +
        geom_point(mapping = aes(date, Variance_scaled),
                   colour = "#212070", size = 2.6) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                        labels = scales::label_percent(scale = 1),
                                                        name = "YTD Variance To Budget Ratio %"))+
        geom_text_repel(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                            x=date, y= Variance_scaled , color = sign.YTD),
                        position = position_dodge(width = 1), fontface='bold',
                        vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4 )
      
      p1
      
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
      
      if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.3
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.3
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
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$`Variance`
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$`Variance`)
        y_label <- "Variance to Budget $"
      }
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = y_label, 
             title = isolate(paste0("MSQ ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face='bold'),
              legend.text = element_text(size = 6),
              axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              legend.position = "non")+
        geom_text_repel(aes(label= text_label,
                      x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1), fontface = "bold",
                  vjust = 0.5 - sign(data$Variance), size = 4)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        geom_hline(aes(yintercept = 0)) 
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance_scaled, group = 1),
                  colour = "#212070", linewidth = 1.25) +
        geom_point(mapping = aes(date, Variance_scaled),
                   colour = "#212070", size = 2.6) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                        labels = scales::label_percent(scale = 1),
                                                        name = "YTD Variance To Budget Ratio %"))+
        geom_text_repel(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                            x=date, y= Variance_scaled , color = sign.YTD),
                        position = position_dodge(width = 1), fontface='bold',
                        vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4 )
      
      p1
      
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
      
      if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.3
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.3
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
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$`Variance`
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$`Variance`)
        y_label <- "Variance to Budget $"
      }
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = y_label, 
             title = isolate(paste0("MSSN ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face='bold'),
              axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              legend.text = element_text(size = 6),
              legend.position = "non")+
        geom_text_repel(aes(label= text_label,
                      x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1), fontface = "bold",
                  vjust = 0.5 - sign(data$Variance), size = 4)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        geom_hline(aes(yintercept = 0)) 
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance_scaled, group = 1),
                  colour = "#212070", linewidth = 1.25) +
        geom_point(mapping = aes(date, Variance_scaled),
                   colour = "#212070", size = 2.6) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                        labels = scales::label_percent(scale = 1),
                                                        name = "YTD Variance To Budget Ratio %"))+
        geom_text_repel(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                            x=date, y= Variance_scaled , color = sign.YTD),
                        position = position_dodge(width = 1), fontface='bold',
                        vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4 )
      
      p1
      
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
      
      if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.3
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.3
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
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$`Variance`
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$`Variance`)
        y_label <- "Variance to Budget $"
      }
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = y_label,  
             title = isolate(paste0("MSW ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face='bold'),
              legend.text = element_text(size = 6),
              axis.text.x = element_text(angle = 45, hjust = 0.5, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              legend.position = "non")+
        geom_text_repel(aes(label= text_label,
                      x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1), fontface = "bold",
                  vjust = 0.5 - sign(data$Variance), size = 4)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        geom_hline(aes(yintercept = 0)) 
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance_scaled, group = 1),
                  colour = "#212070", linewidth = 1.25) +
        geom_point(mapping = aes(date, Variance_scaled),
                   colour = "#212070", size = 2.6) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                        labels = scales::label_percent(scale = 1),
                                                        name = "YTD Variance To Budget Ratio %"))+
        geom_text_repel(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                            x=date, y= Variance_scaled , color = sign.YTD),
                        position = position_dodge(width = 1), fontface='bold',
                        vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4 )
      
      p1
      
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
      
      if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Actual, na.rm = TRUE))*1.3
      }
      
      if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Actual, na.rm = TRUE))*1.3
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
      
      
      if((max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$Variance_scaled, na.rm = TRUE))*1.3
      }
      
      # Define different labels for metrics    
      if (isolate(input$mshs_metrics) %in% c("CMI", "ALOS", "Discharges")) {
        text_label <- data$`Variance`
        y_label <- "Variance to Budget"
      } else {
        text_label <- paste0("$", data$`Variance`)
        y_label <- "Variance to Budget $"
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = y_label, 
             title = isolate(paste0("NYEE ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_textbox_simple(size = 15, halign=0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face='bold'),
              legend.text = element_text(size = 6),
              axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              legend.position = "non")+
        geom_text_repel(aes(label= text_label,
                      x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1), fontface = "bold",
                  vjust = 0.5 - sign(data$Variance), size = 4)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        geom_hline(aes(yintercept = 0)) 
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance_scaled, group = 1),
                  colour = "#212070", linewidth = 1.25) +
        geom_point(mapping = aes(date, Variance_scaled),
                   colour = "#212070", size = 2.6) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / ratio , 
                                                        labels = scales::label_percent(scale = 1),
                                                        name = "YTD Variance To Budget Ratio %"))+
        geom_text_repel(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                            x=date, y= Variance_scaled , color = sign.YTD),
                        position = position_dodge(width = 1), fontface='bold',
                        vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4 )
      
      p1
      
    }
    
  })
  
  
  # Variance Tab ---------------------------
  ### MSHS -------------------------------------
  output$mshs_var <- renderPlot({
    data <- var_data() %>%
      filter(Site == "MSHS")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$var_metrics), " is not available for MSHS")))
    
      # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
      

      if((max(data$Variance, na.rm = TRUE))*1.3 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, na.rm = TRUE))*1.3
      }
      
      if((min(data$Variance, na.rm = TRUE))*1.3 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, na.rm = TRUE))*1.3
      }
    
    # Define different labels for metrics    
    if (isolate(input$var_metrics) %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$`Variance`
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$`Variance`)
      y_label <- "Variance to Budget $"
    }
  
      ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
        labs(x = "Date", y = y_label, 
             title = isolate(paste0("MSHS ", input$var_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        geom_text(aes(label= text_label,
                      x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1), fontface = "bold",
                  vjust = 0.5 - sign(data$Variance), size = 4)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6),
              legend.position = "non")+
        geom_hline(aes(yintercept = 0))+
        scale_y_continuous(limits=c(min_value, max_value))
  })
  
  ### MSB -------------------------------------
  output$msb_var <- renderPlot({
    data <- var_data() %>%
      filter(Site == "MSB")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$var_metrics), " is not available for MSB")))
    
    # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
    
    
    if((max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    # Define different labels for metrics    
    if (isolate(input$var_metrics) %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$`Variance`
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$`Variance`)
      y_label <- "Variance to Budget $"
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = y_label, 
           title = isolate(paste0("MSB ", input$var_metrics , " Monthly Variance to Budget")),
           subtitle = paste0("($ in Thousands)"))+
      geom_text(aes(label= text_label,
                    x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_hline(aes(yintercept = 0))+
      scale_y_continuous(limits=c(min_value, max_value))
  })
  
  ### MSBI -------------------------------------
  output$msbi_var <- renderPlot({
    data <- var_data() %>%
      filter(Site == "MSBI")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$var_metrics), " is not available for MSBI")))
    
    # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
    
    
    if((max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    # Define different labels for metrics    
    if (isolate(input$var_metrics) %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$`Variance`
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$`Variance`)
      y_label <- "Variance to Budget $"
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = y_label, 
           title = isolate(paste0("MSBI ", input$var_metrics , " Monthly Variance to Budget")),
           subtitle = paste0("($ in Thousands)"))+
      geom_text(aes(label= text_label,
                    x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_hline(aes(yintercept = 0))+
      scale_y_continuous(limits=c(min_value, max_value))
  })
  
  ### MSH -------------------------------------
  output$msh_var <- renderPlot({
    data <- var_data() %>%
      filter(Site == "MSH")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$var_metrics), " is not available for MSH")))
    
    # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
    
    
    if((max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    # Define different labels for metrics    
    if (isolate(input$var_metrics) %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$`Variance`
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$`Variance`)
      y_label <- "Variance to Budget $"
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = y_label, 
           title = isolate(paste0("MSH ", input$var_metrics , " Monthly Variance to Budget")),
           subtitle = paste0("($ in Thousands)"))+
      geom_text(aes(label= text_label,
                    x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_hline(aes(yintercept = 0))+
      scale_y_continuous(limits=c(min_value, max_value))
  })
  
  ### MSM -------------------------------------
  output$msm_var <- renderPlot({
    data <- var_data() %>%
      filter(Site == "MSM")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$var_metrics), " is not available for MSM")))
    
    # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
    
    
    if((max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    # Define different labels for metrics    
    if (isolate(input$var_metrics) %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$`Variance`
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$`Variance`)
      y_label <- "Variance to Budget $"
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = y_label, 
           title = isolate(paste0("MSM ", input$var_metrics , " Monthly Variance to Budget")),
           subtitle = paste0("($ in Thousands)"))+
      geom_text(aes(label= text_label,
                    x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_hline(aes(yintercept = 0))+
      scale_y_continuous(limits=c(min_value, max_value))
  })
  
  ### MSQ -------------------------------------
  output$msq_var <- renderPlot({
    data <- var_data() %>%
      filter(Site == "MSQ")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$var_metrics), " is not available for MSQ")))
    
    # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
    
    
    if((max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    # Define different labels for metrics    
    if (isolate(input$var_metrics) %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$`Variance`
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$`Variance`)
      y_label <- "Variance to Budget $"
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = y_label, 
           title = isolate(paste0("MSQ ", input$var_metrics , " Monthly Variance to Budget")),
           subtitle = paste0("($ in Thousands)"))+
      geom_text(aes(label= text_label,
                    x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_hline(aes(yintercept = 0))+
      scale_y_continuous(limits=c(min_value, max_value))
  })
  
  ### MSSN -------------------------------------
  output$mssn_var <- renderPlot({
    data <- var_data() %>%
      filter(Site == "MSSN")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$var_metrics), " is not available for MSSN")))
    
    # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
    
    
    if((max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    # Define different labels for metrics    
    if (isolate(input$var_metrics) %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$`Variance`
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$`Variance`)
      y_label <- "Variance to Budget $"
    }
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = y_label, 
           title = isolate(paste0("MSSN ", input$var_metrics , " Monthly Variance to Budget")),
           subtitle = paste0("($ in Thousands)"))+
      geom_text(aes(label= text_label,
                    x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_hline(aes(yintercept = 0))+
      scale_y_continuous(limits=c(min_value, max_value))
  })
  
  ### MSW -------------------------------------
  output$msw_var <- renderPlot({
    data <- var_data() %>%
      filter(Site == "MSW")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$var_metrics), " is not available for MSW")))
    
    # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
    
    
    if((max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    # Define different labels for metrics    
    if (isolate(input$var_metrics) %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$`Variance`
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$`Variance`)
      y_label <- "Variance to Budget $"
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = y_label, 
           title = isolate(paste0("MSW ", input$var_metrics , " Monthly Variance to Budget")),
           subtitle = paste0("($ in Thousands)"))+
      geom_text(aes(label= text_label,
                    x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_hline(aes(yintercept = 0))+
      scale_y_continuous(limits=c(min_value, max_value))
  })
  
  ### NYEE -------------------------------------
  output$nyee_var <- renderPlot({
    data <- var_data() %>%
      filter(Site == "NYEE")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$var_metrics), " is not available for NYEE")))
    
    # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "CMI")
    
    
    if((max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if((min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    # Define different labels for metrics    
    if (isolate(input$var_metrics) %in% c("CMI", "ALOS", "Discharges")) {
      text_label <- data$`Variance`
      y_label <- "Variance to Budget"
    } else {
      text_label <- paste0("$", data$`Variance`)
      y_label <- "Variance to Budget $"
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = y_label, 
           title = isolate(paste0("NYEE ", input$var_metrics , " Monthly Variance to Budget")),
           subtitle = paste0("($ in Thousands)"))+
      geom_text(aes(label= text_label,
                    x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_hline(aes(yintercept = 0))+
      scale_y_continuous(limits=c(min_value, max_value))
  })
  
  
  # YTD Variance To Ratio ----------------
  ### MSHS -----------------------
  output$mshs_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSHS")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSHS")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
           title = isolate(paste0("MSHS " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      geom_hline(aes(yintercept = 0))
  })
  
  ### MSB -----------------------
  output$msb_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSB")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSB")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
           title = isolate(paste0("MSB " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      geom_hline(aes(yintercept = 0))
  })
  
  ### MSBI -----------------------
  output$msbi_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSBI")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSBI")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
           title = isolate(paste0("MSBI " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      geom_hline(aes(yintercept = 0))
  })
  
  ### MSH -----------------------
  output$msh_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSH")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSH")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
           title = isolate(paste0("MSH " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      geom_hline(aes(yintercept = 0))
  })
  
  ### MSM -----------------------
  output$msm_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSM")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSM")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
           title = isolate(paste0("MSM " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      geom_hline(aes(yintercept = 0))
  })
  
  ### MSQ -----------------------
  output$msq_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSQ")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSQ")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
           title = isolate(paste0("MSQ " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      geom_hline(aes(yintercept = 0))
  })
  
  ### MSSN -----------------------
  output$mssn_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSSN")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSSN")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
           title = isolate(paste0("MSSN " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      geom_hline(aes(yintercept = 0))
  })
  
  ### MSW -----------------------
  output$msw_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSW")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSW")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
           title = isolate(paste0("MSW " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      geom_hline(aes(yintercept = 0))
  })

  ### NYEE -----------------------
  output$nyee_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "NYEE")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for NYEE")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio %" , 
           title = isolate(paste0("NYEE " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      geom_hline(aes(yintercept = 0))
  })
  
  
  # All sites visualization ------------------------------
  
  output$ratio_plot <- renderPlot({
    data <- metric_data() %>% 
      filter(Metrics == "Expense to Revenue Ratio" ) %>%
      select("month", "year", "Site", "Actual", "Metrics", "date")
    
    hospital <- isolate(input$all_hospital)
    
    history <- Exp_Rev_Ratio %>% filter(Site== hospital)
    
    
    data <- rbind(history, data) %>%
      mutate(Actual = round(Actual, 2))%>%
      arrange(date) %>%
      slice(tail(row_number(), 24))
    
    #data <- new_repo %>% filter(Site == "MSHS", Metrics == "Expense to Revenue Ratio")
    
    validate(need(nrow(data)>0, paste0("Expense to Revenue Ratio is not available for ", isolate(input$all_hospital))))
    
    
    if((max(data$Actual, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Actual, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Actual, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Actual, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_rect(xmin= 0, xmax= Inf , ymin = 1, ymax= Inf, fill= "#8f8ce0", alpha=0.02)+
      geom_line(aes(x=date, y= Actual, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Actual), colour = "#212070", size = 3) +
      labs(x = "Date", y = "Expense to Revenue Ratio" , 
           title = paste0(isolate(input$all_hospital), " Expense to Revenue Ratio")
      )+
      geom_hline(aes(yintercept= 1), colour="#990000", linetype="dashed")+
      geom_hline(aes(yintercept = 0))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 45, hjust = 0.5),
            legend.position = "none")+
      geom_text(aes(label= Actual, 
                    x=date, y= Actual),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Actual), size = 4)+
      scale_y_continuous(limits=c(min_value, max_value))
    
  })
  
  
  output$revenue_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics ==  "Total Hospital Revenue")%>%
      #mutate(date= as.yearmon(date, "%Y-%m")) %>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Total Hospital Revenue is not available for ",isolate(input$all_hospital))))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Total Hospital Revenue Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= paste0("$", `Variance`), x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
    
  })
  
  output$expense_plot <- renderPlot({
    data <-  metric_data() %>%
      # mutate(date= as.yearmon(date, "%Y-%m"))
      filter(Metrics %in%  "Total Hospital Expenses")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Total Hospital Expenses is not available for ", isolate(input$all_hospital))))
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Total Hospital Expenses Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= paste0("$", `Variance`), x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
  })
  
  output$discharges_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Discharges")%>%
      # mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Discharges is not available for ", isolate(input$all_hospital))))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget", 
           title = paste0(isolate(input$all_hospital) , " Discharges Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
  })
  
  output$cmi_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "CMI")%>%
      # mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("CMI is not available for ", isolate(input$all_hospital))))
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget", 
           title = paste0(isolate(input$all_hospital) , " CMI Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
  })
  
  output$alos_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "ALOS")%>%
      # mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("ALOS is not available for ", isolate(input$all_hospital))))
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget", 
           title = paste0(isolate(input$all_hospital) , " ALOS Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
  })
  
  output$outpt_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Outpatient Revenue")%>%
      # mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Outpatient Revenue is not available for ", isolate(input$all_hospital))))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Outpatient Revenue Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= paste0("$", `Variance`), x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
  })
  
  output$operate_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "340B/Other Operating Revenue")%>%
      # mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("340B/Other Operating Revenue is not available for ", isolate(input$all_hospital))))
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " 340B/Other Operating Revenue Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= paste0("$", `Variance`), x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
  })
  
  output$salary_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Salaries and Benefits")%>%
      # mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0( "Salaries and Benefits is not available for ", isolate(input$all_hospital))))
    
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Salaries and Benefits Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= paste0("$", `Variance`), x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
  })
  
  
  output$supply_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Supplies & Expenses")%>%
      # mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Supplies & Expenses is not available for ", isolate(input$all_hospital))))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Supplies & Expenses Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= paste0("$", `Variance`), x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
  })
  
  
  output$nurse_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Nursing Agency Costs")%>%
      # mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Nursing Agency Costs is not available for ", isolate(input$all_hospital))))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Nursing Agency Costs Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= paste0("$", `Variance`), x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
  })
  

  output$carts_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "CARTS")%>%
      # mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("CARTS is not available for ", isolate(input$all_hospital))))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.3 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.3
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.3 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.3
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " CARTS Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_textbox_simple(size = 20, halign=0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            legend.position = "non")+
      geom_text(aes(label= paste0("$", `Variance`), x=date, y= Variance, color = sign),
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Variance)/2, size = 4)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      geom_hline(aes(yintercept = 0))
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
      scale_y_continuous(limits = c(0, max(data$Actual)*1.3), breaks= pretty_breaks())+
      geom_text(aes(label= Actual, x=month, y= Actual), color="#212070",
                position = position_dodge(width = 1), fontface = "bold",
                vjust = 0.5 - sign(data$Actual), size = 4)+
      geom_hline(aes(yintercept= 1), colour="#990000", linetype="dashed")+
      geom_hline(aes(yintercept = 0))
    
    
    
  })
  
}