
server <- function(input, output, session) {
  
  ## Text output for Metrics tab -------------------------------
  mshs_text <- eventReactive(input$mshs_filters_update, {
    end_date <- isolate(max(input$mshs_date_range))
    start_date <- isolate(min(input$mshs_date_range))
    metric <- isolate(input$mshs_metrics)
    paste0("Based on data from ", start_date, " to ", end_date)
  }, ignoreNULL = FALSE)
  
  output$mshs_date_show  <- renderText({
    mshs_text()
  })
  
  ## Text output for Metrics tab -------------------------------
  all_mshs_text <- eventReactive(input$mshs_filters_update, {
    end_date <- isolate(max(input$all_date_range))
    start_date <- isolate(min(input$all_date_range))
    hospitals <- isolate(input$all_hospital)
    paste0("Based on data from ", start_date, " to ", end_date, " for ", hospitals)
  }, ignoreNULL = FALSE)
  
  output$ all_date_show  <- renderText({
   all_mshs_text()
  })
  
 
  
  ## eventReactive for  Metrics Tab ------------------------------
  mshs_data  <- eventReactive(input$mshs_filters_update, {
    validate(need(input$mshs_metrics != "", "Please Select a Metric"),
             need(input$mshs_date_range != "", "Please Select a Date"))
    
    new_repo %>%
      filter(Metrics %in% input$mshs_metrics,
              date %in% input$mshs_date_range)
  }, ignoreNULL = FALSE)
  
  
  ## eventReactive for  Metrics YTD Tab ------------------------------
  mshs_data_ytd  <- eventReactive(input$mshs_filters_update_ytd, {
    validate(need(input$mshs_metrics_ytd != "", "Please Select a Metric"),
             need(input$mshs_date_range_ytd != "", "Please Select a Date"))
    
    new_repo %>%
      filter(Metrics %in% input$mshs_metrics_ytd,
             date %in% input$mshs_date_range_ytd)
  }, ignoreNULL = FALSE)
  
  
  ## eventReactive for all sites ------------------------------
  metric_data  <- eventReactive(input$all_filters_update,{
    validate(need(input$all_hospital != "", "Please Select a Hospital"),
             need(input$all_date_range != "", "Please Select a Date"))
    
    new_repo %>%
      filter(Site %in% input$all_hospital,
             date %in% input$all_date_range)
  }, ignoreNULL = FALSE)
  

  # Metrics visualization ------------------------------
  ## MSHS -------------------------------------
  output$mshs_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSHS")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSHS")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
      # data <- new_repo %>% filter(Site == "MSHS" & Metrics == "Expense to Revenue Ratio") 
      
      
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
        labs(x = "Date", y = "Expense to Revenue Ratio", 
             title = paste0("MSHS Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
    } else {
      
      # data <- new_repo %>%
      #     filter(Site == "MSHS" & Metrics == "CMI")
      
      
      # Define a variable for to balance the sec axis
      data <- data %>% mutate(char = nchar(floor(max(abs(Variance)))),
                             balance_value = ifelse(char> 1, 10^(char), char),
                             balance = Variance.From.Budget.YTD * balance_value, 
                             balance_value = ifelse(nchar(floor(abs(balance)))> char, 
                                                balance_value/(10^(nchar(floor(abs(balance))) - char)), 
                                                balance_value),
                             balance = Variance.From.Budget.YTD * balance_value) %>% 
        fill(balance_value, .direction = "downup")
      
      
      
      if((max(data$Variance, data$balance, na.rm = TRUE))*1.5 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      if((min(data$Variance, data$balance, na.rm = TRUE))*1.5 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = "Variance to Budget $", 
             #title = isolate(paste0("MSHS ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5))+
        geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(legend.position = "non")
      
      
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance.From.Budget.YTD * balance_value, group = 1),
                  colour = "#212070", linewidth = 1.2) +
        geom_point(mapping = aes(date, Variance.From.Budget.YTD * balance_value),
                   colour = "#212070", size = 2) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / data$balance_value,
                               name = "YTD Variance To Budget Ratio"
        ))+
        geom_text(aes(label= paste0(`Variance.From.Budget.YTD`*100, "%"), x=date, y= Variance.From.Budget.YTD, color = sign.YTD),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)
      
      p1
      
    }
    
  })
  
  ## MSB -----------------------------------------------
  output$msb_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSB")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSB")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
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
        labs(x = "Date", y = "Expense to Revenue Ratio", 
             title = paste0("MSB Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
    } else {
      
      # data <- new_repo %>%
      #     filter(Site == "MSHS" & Metrics == "CMI")
      
      # Define a variable for to balance the sec axis
      data <- data %>% mutate(char = nchar(floor(max(abs(Variance)))),
                             balance_value = ifelse(char> 1, 10^(char), char),
                             balance = Variance.From.Budget.YTD * balance_value, 
                             balance_value = ifelse(nchar(floor(abs(balance)))> char, 
                                                    balance_value/(10^(nchar(floor(abs(balance))) - char)), 
                                                    balance_value),
                             balance = Variance.From.Budget.YTD * balance_value) %>% 
        fill(balance_value, .direction = "downup")
      
      
      
      if((max(data$Variance, data$balance, na.rm = TRUE))*1.5 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      if((min(data$Variance, data$balance, na.rm = TRUE))*1.5 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = "Variance to Budget $", 
             title = isolate(paste0("MSB ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5))+
        geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(legend.position = "non")
      
      
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance.From.Budget.YTD *balance_value , group = 1),
                  colour = "#212070", linewidth = 1.2) +
        geom_point(mapping = aes(date, Variance.From.Budget.YTD * balance_value),
                   colour = "#212070", size = 2) +
        scale_y_continuous(limits=c(min_value, max_value), 
                             sec.axis = ggplot2::sec_axis(~. / data$balance_value,
                                name = "YTD Variance To Budget Ratio"))+
        geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), x=date, y= Variance.From.Budget.YTD, color = sign.YTD),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)
      
      p1
      
    }
    
  })
  
  ## MSBI -----------------------------
  
  output$msbi_plot <- renderPlot({
  
    data <- mshs_data() %>%
      filter(Site == "MSBI")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSBI")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
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
        labs(x = "Date", y = "Expense to Revenue Ratio", 
             title = paste0("MSBI Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
    } else {
      
      # data <- new_repo %>%
      #     filter(Site == "MSHS" & Metrics == "CMI")
      
      # Define a variable for to balance the sec axis
      data <- data %>% mutate(char = nchar(floor(max(abs(Variance)))),
                              balance_value = ifelse(char> 1, 10^(char), char),
                              balance = Variance.From.Budget.YTD * balance_value, 
                              balance_value = ifelse(nchar(floor(abs(balance)))> char, 
                                                     balance_value/(10^(nchar(floor(abs(balance))) - char)), 
                                                     balance_value),
                              balance = Variance.From.Budget.YTD * balance_value) %>% 
        fill(balance_value, .direction = "downup")
      
      
      
      
      if((max(data$Variance, data$balance, na.rm = TRUE))*1.5 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      if((min(data$Variance, data$balance, na.rm = TRUE))*1.5 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      
      
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = "Variance to Budget $", 
             title = isolate(paste0("MSBI ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5))+
        geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(legend.position = "non")
      
      
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance.From.Budget.YTD *balance_value, group = 1),
                  colour = "#212070", linewidth = 1.2) +
        geom_point(mapping = aes(date, Variance.From.Budget.YTD * balance_value),
                   colour = "#212070", size = 2) +
        scale_y_continuous(limits=c(min_value, max_value), sec.axis = ggplot2::sec_axis(~. / data$balance_value,
                                                                                        name = "YTD Variance To Budget Ratio"
        ))+
        geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), x=date, y= Variance.From.Budget.YTD, color = sign.YTD),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)
      
      p1
      
    }
    
  })
  
  ## MSH -----------------------------------------------
  output$msh_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSH")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSH")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
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
        labs(x = "Date", y = "Expense to Revenue Ratio", 
             title = paste0("MSH Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
    } else {
      
      # data <- new_repo %>%
      #     filter(Site == "MSHS" & Metrics == "CMI")
      
      # Define a variable for to balance the sec axis
      data <- data %>% mutate(char = nchar(floor(max(abs(Variance)))),
                              balance_value = ifelse(char> 1, 10^(char), char),
                              balance = Variance.From.Budget.YTD * balance_value, 
                              balance_value = ifelse(nchar(floor(abs(balance)))> char, 
                                                     balance_value/(10^(nchar(floor(abs(balance))) - char)), 
                                                     balance_value),
                              balance = Variance.From.Budget.YTD * balance_value) %>% 
        fill(balance_value, .direction = "downup")
      
      
      
      if((max(data$Variance, data$balance, na.rm = TRUE))*1.5 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      if((min(data$Variance, data$balance, na.rm = TRUE))*1.5 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = "Variance to Budget $", 
             title = isolate(paste0("MSH ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5))+
        geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(legend.position = "non")
      
      
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance.From.Budget.YTD * balance_value, group = 1),
                  colour = "#212070", linewidth = 1.2) +
        geom_point(mapping = aes(date, Variance.From.Budget.YTD * balance_value),
                   colour = "#212070", size = 2) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / data$balance_value,
                                                        name = "YTD Variance To Budget Ratio"))+
        geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), x=date, y= Variance.From.Budget.YTD, color = sign.YTD),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)
      
      p1
      
    }
    
  })
  
  ## MSM -----------------------------------------------
  output$msm_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSM")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSM")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
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
        labs(x = "Date", y = "Expense to Revenue Ratio", 
             title = paste0("MSM Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
    } else {
      
      # data <- new_repo %>%
      #     filter(Site == "MSHS" & Metrics == "CMI")
      
      # Define a variable for to balance the sec axis
      data <- data %>% mutate(char = nchar(floor(max(abs(Variance)))),
                              balance_value = ifelse(char> 1, 10^(char), char),
                              balance = Variance.From.Budget.YTD * balance_value, 
                              balance_value = ifelse(nchar(floor(abs(balance)))> char, 
                                                     balance_value/(10^(nchar(floor(abs(balance))) - char)), 
                                                     balance_value),
                              balance = Variance.From.Budget.YTD * balance_value) %>% 
        fill(balance_value, .direction = "downup")
      
      
      
      
      if((max(data$Variance, data$balance, na.rm = TRUE))*1.5 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      if((min(data$Variance, data$balance, na.rm = TRUE))*1.5 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = "Variance to Budget $", 
             title = isolate(paste0("MSM ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5))+
        geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(legend.position = "non")
      
      
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance.From.Budget.YTD * balance_value, group = 1),
                  colour = "#212070", linewidth = 1.2) +
        geom_point(mapping = aes(date, Variance.From.Budget.YTD * balance_value),
                   colour = "#212070", size = 2) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / data$balance_value,
                                                        name = "YTD Variance To Budget Ratio"))+
        geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), x=date, y= Variance.From.Budget.YTD, color = sign.YTD),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)
      
      p1
      
    }
    
  })
  
  
  ## MSQ -----------------------------------------------
  output$msq_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSQ")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSQ")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
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
        labs(x = "Date", y = "Expense to Revenue Ratio", 
             title = paste0("MSQ Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
    } else {
      
      # data <- new_repo %>%
      #     filter(Site == "MSHS" & Metrics == "CMI")
      
      # Define a variable for to balance the sec axis
      data <- data %>% mutate(char = nchar(floor(max(abs(Variance)))),
                              balance_value = ifelse(char> 1, 10^(char), char),
                              balance = Variance.From.Budget.YTD * balance_value, 
                              balance_value = ifelse(nchar(floor(abs(balance)))> char, 
                                                     balance_value/(10^(nchar(floor(abs(balance))) - char)), 
                                                     balance_value),
                              balance = Variance.From.Budget.YTD * balance_value) %>% 
        fill(balance_value, .direction = "downup")
      
      
      
      
      if((max(data$Variance, data$balance, na.rm = TRUE))*1.5 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      if((min(data$Variance, data$balance, na.rm = TRUE))*1.5 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = "Variance to Budget $", 
             title = isolate(paste0("MSQ ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5))+
        geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(legend.position = "non")
      
      
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance.From.Budget.YTD * balance_value, group = 1),
                  colour = "#212070", linewidth = 1.2) +
        geom_point(mapping = aes(date, Variance.From.Budget.YTD * balance_value),
                   colour = "#212070", size = 2) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / data$balance_value,
                                                        name = "YTD Variance To Budget Ratio"))+
        geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), x=date, y= Variance.From.Budget.YTD, color = sign.YTD),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)
      
      p1
      
    }
    
  })
  
  ## MSSN -----------------------------------------------
  output$mssn_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSSN")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSSN")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
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
        labs(x = "Date", y = "Expense to Revenue Ratio", 
             title = paste0("MSSN Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
    } else {
      
      # data <- new_repo %>%
      #     filter(Site == "MSHS" & Metrics == "CMI")
      
      # Define a variable for to balance the sec axis
      data <- data %>% mutate(char = nchar(floor(max(abs(Variance)))),
                              balance_value = ifelse(char> 1, 10^(char), char),
                              balance = Variance.From.Budget.YTD * balance_value, 
                              balance_value = ifelse(nchar(floor(abs(balance)))> char, 
                                                     balance_value/(10^(nchar(floor(abs(balance))) - char)), 
                                                     balance_value),
                              balance = Variance.From.Budget.YTD * balance_value) %>% 
        fill(balance_value, .direction = "downup")
      
      
      
      
      if((max(data$Variance, data$balance, na.rm = TRUE))*1.5 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      if((min(data$Variance, data$balance, na.rm = TRUE))*1.5 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = "Variance to Budget $", 
             title = isolate(paste0("MSSN ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5))+
        geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(legend.position = "non")
      
      
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance.From.Budget.YTD * balance_value, group = 1),
                  colour = "#212070", linewidth = 1.2) +
        geom_point(mapping = aes(date, Variance.From.Budget.YTD * balance_value),
                   colour = "#212070", size = 2) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / data$balance_value,
                                                        name = "YTD Variance To Budget Ratio"))+
        geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), x=date, y= Variance.From.Budget.YTD, color = sign.YTD),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)
      
      p1
      
    }
    
  })
  
  ## MSW -----------------------------------------------
  output$msw_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "MSW")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for MSW")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
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
        labs(x = "Date", y = "Expense to Revenue Ratio", 
             title = paste0("MSW Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
    } else {
      
      # data <- new_repo %>%
      #     filter(Site == "MSHS" & Metrics == "CMI")
      
      # Define a variable for to balance the sec axis
      data <- data %>% mutate(char = nchar(floor(max(abs(Variance)))),
                              balance_value = ifelse(char> 1, 10^(char), char),
                              balance = Variance.From.Budget.YTD * balance_value, 
                              balance_value = ifelse(nchar(floor(abs(balance)))> char, 
                                                     balance_value/(10^(nchar(floor(abs(balance))) - char)), 
                                                     balance_value),
                              balance = Variance.From.Budget.YTD * balance_value) %>% 
        fill(balance_value, .direction = "downup")
      
      
      
      
      if((max(data$Variance, data$balance, na.rm = TRUE))*1.5 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      if((min(data$Variance, data$balance, na.rm = TRUE))*1.5 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = "Variance to Budget $", 
             title = isolate(paste0("MSW ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5))+
        geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(legend.position = "non")
      
      
      
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance.From.Budget.YTD * balance_value, group = 1),
                  colour = "#212070", linewidth = 1.2) +
        geom_point(mapping = aes(date, Variance.From.Budget.YTD * balance_value),
                   colour = "#212070", size = 2) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / data$balance_value,
                                                        name = "YTD Variance To Budget Ratio"))+
        geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), x=date, y= Variance.From.Budget.YTD, color = sign.YTD),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)
      
      p1
      
    }
    
  })

  ## NYEE -----------------------------------------------
  output$nyee_plot <- renderPlot({
    data <- mshs_data() %>%
      filter(Site == "NYEE")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"),
             sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics), " is not available for NYEE")))
    
    
    if (isolate(input$mshs_metrics) %in% c("Expense to Revenue Ratio")) {
      
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
        labs(x = "Date", y = "Expense to Revenue Ratio", 
             title = paste0("NYEE Expense to Revenue Ratio" ),
             subtitle = paste0("(Cost to earn $1 of revenue)"))+
        scale_y_continuous(limits = c(0, max(data$Actual) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)) 
    } else {
      
      # data <- new_repo %>%
      #     filter(Site == "MSHS" & Metrics == "CMI")
      
      # Define a variable for to balance the sec axis
      data <- data %>% mutate(char = nchar(floor(max(abs(Variance)))),
                              balance_value = ifelse(char> 1, 10^(char), char),
                              balance = Variance.From.Budget.YTD * balance_value, 
                              balance_value = ifelse(nchar(floor(abs(balance)))> char, 
                                                     balance_value/(10^(nchar(floor(abs(balance))) - char)), 
                                                     balance_value),
                              balance = Variance.From.Budget.YTD * balance_value) %>% 
        fill(balance_value, .direction = "downup")
      
      
      
      
      if((max(data$Variance, data$balance, na.rm = TRUE))*1.5 < 0){
        max_value <- 0
      } else {
        max_value <- (max(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      if((min(data$Variance, data$balance, na.rm = TRUE))*1.5 > 0){
        min_value <- 0
      } else {
        min_value <- (min(data$Variance, data$balance, na.rm = TRUE))*1.5
      }
      
      
      p1 <- ggplot(data)  + 
        geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#06ABEB")+
        labs(x = "Date", y = "Variance to Budget $", 
             #title = isolate(paste0("NYEE ", input$mshs_metrics , " Monthly Variance to Budget")),
             subtitle = paste0("($ in Thousands)"))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5))+
        geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
        scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
        theme(legend.position = "non")
      
  
      p1 <- p1 +
        geom_line(mapping = aes(date, Variance.From.Budget.YTD * balance_value, group = 1),
                  colour = "#212070", linewidth = 1.2) +
        geom_point(mapping = aes(date, Variance.From.Budget.YTD * balance_value),
                   colour = "#212070", size = 2) +
        scale_y_continuous(limits=c(min_value, max_value), 
                           sec.axis = ggplot2::sec_axis(~. / data$balance_value,
                                                        #breaks = seq(min.value.YTD, max.value.YTD, by = 0.05),
                                                        name = "YTD Variance To Budget Ratio"))+
        geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), x=date, y= Variance.From.Budget.YTD, color = sign.YTD),
                  position = position_dodge(width = 1),
                  vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)
      
      p1
      
    }
    
  })
  
  
  # Tabpanel YTD Variance To Ratio ----------------
  
  ## MSHS -----------------------
  output$mshs_plot_ytd <- renderPlot({
    
      data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSHS")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSHS")))
    
  
  if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 < 0){
    max_value_ytd <- 0
  } else {
    max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
  }
  
  if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 > 0){
    min_value_ytd <- 0
  } else {
    min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
  }
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                  colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
    labs(x = "Date", y = "YTD Variance to Budget Ratio" , 
         title = isolate(paste0("MSHS " , input$mshs_metrics_ytd, " YTD Variance to Budget Ratio" ))
    )+
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(face = "bold"),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 0, hjust = 0.5))+
    geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                  x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
              position = position_dodge(width = 1),
              vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
    scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
    theme(legend.position = "none")
  
  
  })
  
  ## MSB -----------------------
  output$msb_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSB")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSB")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                    color = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio" , 
           title = isolate(paste0("MSB " , input$mshs_metrics_ytd , " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      theme(legend.position = "none")
    
    
  })
  
  ## MSBI -----------------------
  output$msbi_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSBI")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSBI")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                    colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio" , 
           title = isolate(paste0("MSBI " , input$mshs_metrics_ytd , " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      theme(legend.position = "none")
    
    
  })
  
  ## MSH -----------------------
  output$msh_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "Total Hospital Expenses"  )%>%
      filter(Site == "MSH")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSH")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                    colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio" , 
           #title = isolate(paste0("MSH " , input$mshs_metrics_ytd , " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      theme(legend.position = "none")
    
    
  })
  
  ## MSM -----------------------
  output$msm_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSM")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSM")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                    colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio" , 
           title = isolate(paste0("MSM " , input$mshs_metrics_ytd , " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      theme(legend.position = "none")
    
    
  })
  
  ## MSQ -----------------------
  output$msq_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSQ")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSQ")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                    colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio" , 
           title = isolate(paste0("MSQ " , input$mshs_metrics_ytd , " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      theme(legend.position = "none")
    
    
  })
  
  ## MSSN -----------------------
  output$mssn_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSSN")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSSN")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                    colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio" , 
           title = isolate(paste0("MSSN " , input$mshs_metrics_ytd , " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      theme(legend.position = "none")
    
    
  })
  
  ## MSW -----------------------
  output$msw_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "MSW")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for MSW")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                    colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio" , 
           title = isolate(paste0("MSW " , input$mshs_metrics_ytd , " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      theme(legend.position = "none")
    
    
  })
  
  ## NYEE -----------------------
  output$nyee_plot_ytd <- renderPlot({
    
    data <- mshs_data_ytd()%>%
      #data <- new_repo %>% filter(Metrics == "CARTS")%>%
      filter(Site == "NYEE")%>%
      #mutate(date= as.yearmon(date, "%Y-%m"))%>%
      mutate(sign.YTD = ifelse(Variance.From.Budget.YTD > 0, "positive", "negative"))
    
    validate(need(nrow(data) > 0, paste0(isolate(input$mshs_metrics_ytd), " is not available for NYEE")))
    
    
    if( (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 < 0){
      max_value_ytd <- 0
    } else {
      max_value_ytd <- (max(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5 > 0){
      min_value_ytd <- 0
    } else {
      min_value_ytd <- (min(data$Variance.From.Budget.YTD, na.rm = TRUE))*1.5
    }
    ggplot(data)  + 
      geom_line(aes(x=date, y= Variance.From.Budget.YTD, group = 1), 
                    colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Variance.From.Budget.YTD),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "YTD Variance to Budget Ratio" , 
           title = isolate(paste0("NYEE " , input$mshs_metrics_ytd , " YTD Variance to Budget Ratio" ))
      )+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= paste0(`Variance.From.Budget.YTD`, "%"), 
                    x=date, y= Variance.From.Budget.YTD, color= sign.YTD),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance.From.Budget.YTD), size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value_ytd, max_value_ytd))+
      theme(legend.position = "none")
    
    
  })
  
  
  
  
  
  
  # All sites visualization ------------------------------
  
  output$ratio_plot <- renderPlotly({
    data <- metric_data() %>% 
      filter(Metrics == "Expense to Revenue Ratio" )
    
    
    test <<- data
    
    #data <- new_repo %>% filter(Site == "MSHS", Metrics == "Expense to Revenue Ratio")
    
    validate(need(nrow(data)>0, paste0("Expense to Revenue Ratio is not available for ", input$all_hospital)))
  
    
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
      
    ggplotly(
    ggplot(data)  + 
      geom_line(aes(x=date, y= Actual, group = 1), 
                colour = "#212070", stat="identity", linewidth = 1.25)+
      geom_point(mapping = aes(date, Actual),
                 colour = "#212070", size = 3) +
      labs(x = "Date", y = "Expense to Revenue Ratio" , 
          title = isolate(paste0(input$all_hospital , " Expense to Revenue Ratio" ))
      )+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      # geom_text(aes(label= Actual, x=date, y= Actual),
      #           position = position_dodge(width = 1),
      #           vjust = 0.5 - sign(data$Actual), size = 3.5)+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "none")
    )
      
    })
  
  
  
  output$revenue_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics ==  "Total Hospital Revenue")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
      
    test <<- data
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Total Hospital Revenue is not available for ",isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
   
   
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
   ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Total Hospital Revenue Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2, size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
    
  })
  
  output$expense_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Total Hospital Expenses")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    test_exp <<- data
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Total Hospital Expenses is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Total Hospital Expenses Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
  output$discharges_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Discharges")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))

    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Discharges is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Discharges Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
  output$cmi_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "CMI")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("CMI is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " CMI Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
  output$alos_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "ALOS")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("ALOS is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " ALOS Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
  output$outpt_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Outpatient Revenue")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Outpatient Revenue is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Outpatient Revenue Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
  output$operate_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "340B/Other Operating Revenue")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("340B/Other Operating Revenue is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " 340B/Other Operating Revenue Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
  output$salary_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Salaries and Benefits")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0( "Salaries and Benefits is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Salaries and Benefits Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
  
  output$supply_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Supplies & Expenses")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Supplies & Expenses is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Supplies & Expenses Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
  
  output$nurse_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "Nursing Agency Costs")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("Nursing Agency Costs is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " Nursing Agency Costs Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
  
  
  output$carts_plot <- renderPlot({
    data <-  metric_data() %>%
      filter(Metrics %in%  "CARTS")%>%
      mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    
    # data <- new_repo %>% filter(Site %in% "MSHS", Metrics ==  "Total Hospital Revenue")%>%
    # mutate(sign = ifelse(Variance > 0, "positive", "negative"))
    
    validate(need(nrow(data)> 0, paste0("CARTS is not available for ", isolate(input$all_hospital))))
    
    #data <- data %>% mutate(date= as.yearmon(date, "%Y-%m"))
    
    
    if( (max(data$Variance, na.rm = TRUE))*1.5 < 0){
      max_value <- 0
    } else {
      max_value <- (max(data$Variance, na.rm = TRUE))*1.5
    }
    
    if( (min(data$Variance, na.rm = TRUE))*1.5 > 0){
      min_value <- 0
    } else {
      min_value <- (min(data$Variance, na.rm = TRUE))*1.5
    }
    
    ggplot(data)  + 
      geom_bar(aes(x=date, y= Variance), stat="identity", fill= "#212070")+
      labs(x = "Date", y = "Variance to Budget $", 
           title = paste0(isolate(input$all_hospital) , " CARTS Monthly Variance to Budget"),
           subtitle = paste0("($ in Thousands)"))+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label= `Variance`, x=date, y= Variance, color = sign),
                position = position_dodge(width = 1),
                vjust = 0.5 - sign(data$Variance)/2 , size = 3.5)+
      scale_colour_manual(values=c("negative"= "#D2042D", "positive"= "#228B22"))+
      scale_y_continuous(limits=c(min_value, max_value))+
      theme(legend.position = "non")
  })
  
}