# Projet betclic server.R

library("plotly") 
library("RSQLite")

# Database connection
conn <- dbConnect(RSQLite::SQLite(), "./betclic.db")

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  #--------------------------------------------------------------#
  #  Reactive KPI data from SQL and dates chosen in dashboard    #
  #--------------------------------------------------------------#
  
  KPI=reactiveValues()
  Others_KPI=reactiveValues()
  
  observe({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    print("voici les données dacquisition source")
    if( is.null(input$Acquisition_source))
    {
      add_request=""
      add_request2=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
        add_request2=""
      }else{
        print("changement input Acquisition_source")
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
        add_request2=paste(" and a.Acquisition_Source='", input$Acquisition_source, "'",sep="")
        
      }
    }
    SQL_request=
        paste("select * , Round(Total_Cost / New_clients,2) as Mean_Cost, ( Total_Revenue - Total_Cost) as Total_Profit, ROUND(Total_revenue / New_clients,2) as Mean_Revenue, ROUND((Total_Revenue / New_clients) - (Total_Cost / New_clients),2) as Mean_Profit  from
  (
    select ROUND(sum(Revenue)) as Total_Revenue, count(distinct user.User_id) as New_clients
    from user
    left join  (select User_id, sum(Revenue) as Revenue from monthly_user_revenue group by User_id) monthly_user_id
    on user.User_id=monthly_user_id.User_id
    where Registration_MonthDate >= '", date_min ,"' and Registration_MonthDate <= '", date_max ,"'
  ", add_request ," ) a JOIN
  (select ROUND(sum(Cost)) as Total_Cost
    from monthly_acquisition_cost 
    where MonthDate >= '", date_min ,"' and MonthDate <= '",date_max,"'
  ", add_request ,") b on 1=1
  ",sep="")
    
    print(SQL_request)

    data_query_result=DBI::dbGetQuery(conn, SQL_request)
    KPI$results=data_query_result
    
    SQL_request2=paste("select ROUND(Total_Revenue_1M / New_clients , 2) as Mean_Revenue_1M, Mean_Revenue_2M, Mean_Revenue_3M, Mean_Revenue_6M  from ( 
      select count(distinct a.User_id) as New_Clients, sum(Revenue) as Total_Revenue_1M   from user a
      left join monthly_user_revenue b
      on a.User_id=b.User_id
      where b.MonthDate < substr( datetime( Registration_MonthDate ||  '-01' ,'start of month','+1 month') ,1,7) and a.Registration_MonthDate >= '", date_min ,"' and a.Registration_MonthDate <= '", date_max ,"'
     ", add_request2 ," ) a join 
    (
      select Round(Total_Revenue_2M / New_Clients , 2) as Mean_Revenue_2M from ( 
        select count(distinct a.User_id) as New_Clients, sum(Revenue) as Total_Revenue_2M  from user a
        left join monthly_user_revenue b
        on a.User_id=b.User_id
        where b.MonthDate < substr( datetime( Registration_MonthDate ||  '-01' ,'start of month','+2 month') ,1,7) and a.Registration_MonthDate >= '", date_min ,"' and a.Registration_MonthDate <= '", date_max ,"'
      ", add_request2 ,"  )
    ) b join
    (
      select Round(Total_Revenue_3M / New_Clients , 2) as Mean_Revenue_3M from ( 
        select count(distinct a.User_id) as New_Clients, sum(Revenue) as Total_Revenue_3M  from user a
        left join monthly_user_revenue b
        on a.User_id=b.User_id
        where b.MonthDate < substr( datetime( Registration_MonthDate ||  '-01' ,'start of month','+3 month') ,1,7) and a.Registration_MonthDate >= '", date_min ,"' and a.Registration_MonthDate <= '", date_max ,"'
       ", add_request2 ," )
    ) c join
    (
      select Round(Total_Revenue_6M / New_Clients , 2) as Mean_Revenue_6M from ( 
        select count(distinct a.User_id) as New_Clients, sum(Revenue) as Total_Revenue_6M  from user a
        left join monthly_user_revenue b
        on a.User_id=b.User_id
        where b.MonthDate < substr( datetime( Registration_MonthDate ||  '-01' ,'start of month','+6 month') ,1,7) and a.Registration_MonthDate >= '", date_min ,"' and a.Registration_MonthDate <= '", date_max ,"'
       ", add_request ,"  )
    )",sep="")
    data_query_result2=DBI::dbGetQuery(conn, SQL_request2)
    Others_KPI$results=data_query_result2
    
    
    
    
    
  })
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$mytable = DT::renderDataTable({
    mtcars
  })
  
  #-----------------------------------------------#
  #                                               #
  #    Pie chart categorical variables            #
  #                                               #
  #-----------------------------------------------#
  
  #--------------------------------#
  # Pie chart acquisition source   #
  #--------------------------------#
  
  output$box_piechart_acquisition_source<- renderUI({
                 box(
                   title = "Acquisition source",
                   status = "primary",
                   width = 4,
                   solidHeader = FALSE,
                   collapsible = TRUE,
                   plotlyOutput("piechart_acquisition_source",height = 200)
                 )})
  
  output$piechart_acquisition_source <- renderPlotly({
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("SELECT Acquisition_Source, 
    ROUND( 100.0 * COUNT(*) / (SELECT COUNT(*) FROM user where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'",add_request,"),1) AS freq
    FROM user
    where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'
    ",add_request,"
    GROUP BY Acquisition_Source",sep="")
    
    data_query_result=DBI::dbGetQuery(conn, SQL_request)

    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot <- plot_ly(data_query_result, labels=~Acquisition_Source,values=~freq, marker = list(line = list(color = '#FFFFFF', width = 1)), type="pie",
                    textposition = 'auto',textinfo = 'text',
                    hoverinfo = 'text',source = "subset",
                    text=~paste(sub(" ","<br>",Acquisition_Source),":","<br>",paste0(freq,"%") ),
                    insidetextfont = list(color = '#FFFFFF')) %>%
      layout(xaxis = ax, yaxis = ax,showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
    plot
  })
  #-------------------------#
  # pie chart cash balance  #
  #-------------------------#
  
  output$box_piechart_cash_balance<- renderUI({
    box(
      title = "Cash Balance",
      status = "primary",
      width = 4,
      solidHeader = FALSE,
      collapsible = TRUE,
      plotlyOutput("piechart_cash_balance",height = 200)
    )})
  
  output$piechart_cash_balance <- renderPlotly({
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("SELECT Cash_Balance_Category, 
    ROUND( 100.0 * COUNT(*) / (SELECT COUNT(*) FROM user where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'",add_request,"),1) AS freq
    FROM user
    where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'
    ",add_request,"
    GROUP BY Cash_Balance_Category",sep="")
    
    data_query_result=DBI::dbGetQuery(conn, SQL_request)

    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot <- plot_ly(data_query_result, labels=~Cash_Balance_Category,values=~freq, marker = list(line = list(color = '#FFFFFF', width = 1)), type="pie",
                    textposition = 'auto',textinfo = 'text',
                    hoverinfo = 'text',source = "subset",
                    text=~paste(sub(" ","<br>",Cash_Balance_Category),":","<br>",paste0(freq,"%") ),
                    insidetextfont = list(color = '#FFFFFF')) %>%
      layout(xaxis = ax, yaxis = ax,showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
    plot
  })
  
  #---------------------------------#
  # pie chart Activity/ Inactivity  #
  #---------------------------------#
  
  output$box_piechart_inactivity_category<- renderUI({
    box(
      title = "Activity",
      status = "primary",
      width = 4,
      solidHeader = FALSE,
      collapsible = TRUE,
      plotlyOutput("piechart_inactivity_category",height = 200)
    )})
  
  output$piechart_inactivity_category <- renderPlotly({

    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("SELECT Inactivity_Category, 
    ROUND( 100.0 * COUNT(*) / (SELECT COUNT(*) FROM user where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'",add_request,"),1) AS freq
    FROM user
    where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'
    ",add_request,"
    GROUP BY Inactivity_Category",sep="")
    
    data_query_result=DBI::dbGetQuery(conn, SQL_request)

    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot <- plot_ly(data_query_result, labels=~Inactivity_Category,values=~freq, marker = list(line = list(color = '#FFFFFF', width = 1)), type="pie",
                    textposition = 'auto',textinfo = 'text',
                    hoverinfo = 'text',source = "subset",
                    text=~paste(sub(" ","<br>",Inactivity_Category),":","<br>",paste0(freq,"%") ),
                    insidetextfont = list(color = '#FFFFFF')) %>%
      layout(xaxis = ax, yaxis = ax,showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
    plot
  })
  
  #---------------------------------#
  # pie chart Sexe                  #
  #---------------------------------#
  
  output$box_piechart_sexe_category<- renderUI({
    box(
      title = "Sex",
      status = "primary",
      width = 4,
      solidHeader = FALSE,
      collapsible = TRUE,
      plotlyOutput("piechart_sexe_category",height = 200)
    )})
  
  output$piechart_sexe_category <- renderPlotly({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    SQL_request=paste("SELECT Sexe, 
    ROUND( 100.0 * COUNT(*) / (SELECT COUNT(*) FROM user where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'",add_request,"),1) AS freq
    FROM user
    where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'
    ",add_request,"
    GROUP BY Sexe",sep="")
    
    data_query_result=DBI::dbGetQuery(conn, SQL_request)

    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot <- plot_ly(data_query_result, labels=~Sexe,values=~freq, marker = list(line = list(color = '#FFFFFF', width = 1)), type="pie",
                    textposition = 'auto',textinfo = 'text',
                    hoverinfo = 'text',source = "subset",
                    text=~paste(sub(" ","<br>",Sexe),":","<br>",paste0(freq,"%") ),
                    insidetextfont = list(color = '#FFFFFF')) %>%
      layout(xaxis = ax, yaxis = ax,showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
    plot
  })
  
  #--------------------#
  #   Pie chart Age    #
  #--------------------#
  
  output$box_piechart_age_category<- renderUI({
    box(
      title = "Age",
      status = "primary",
      width = 4,
      solidHeader = FALSE,
      collapsible = TRUE,
      plotlyOutput("piechart_age_category",height = 200)
    )})
  
  output$piechart_age_category <- renderPlotly({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("SELECT Age_Category, 
    ROUND( 100.0 * COUNT(*) / (SELECT COUNT(*) FROM user where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'",add_request,"),1) AS freq
    FROM user
    where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'
    ",add_request,"
    GROUP BY Age_Category",sep="")
    
    data_query_result=DBI::dbGetQuery(conn, SQL_request)

    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot <- plot_ly(data_query_result, labels=~Age_Category,values=~freq, marker = list(line = list(color = '#FFFFFF', width = 1)), type="pie",
                    textposition = 'auto',textinfo = 'text',
                    hoverinfo = 'text',source = "subset",
                    text=~paste(sub(" ","<br>",Age_Category),":","<br>",paste0(freq,"%") ),
                    insidetextfont = list(color = '#FFFFFF')) %>%
      layout(xaxis = ax, yaxis = ax,showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
    plot
  })
  
  #---------------------------------#
  # pie chart Behaviour Segment     #
  #---------------------------------#
  
  output$box_piechart_behaviour_segment<- renderUI({
    box(
      title = "Behaviour_Segment",
      status = "primary",
      width = 4,
      solidHeader = FALSE,
      collapsible = TRUE,
      plotlyOutput("piechart_behaviour_segment",height = 200)
    )})
  
  output$piechart_behaviour_segment <- renderPlotly({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("SELECT Behaviour_Segment, 
                      ROUND( 100.0 * COUNT(*) / (SELECT COUNT(*) FROM user where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'",add_request,"),1) AS freq
                      FROM user
                      where Registration_MonthDate >= '", date_min ,"' and user.Registration_MonthDate <= '", date_max ,"'
                      ",add_request,"
                      GROUP BY Behaviour_Segment",sep="")
    
    data_query_result=DBI::dbGetQuery(conn, SQL_request)

    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot <- plot_ly(data_query_result, labels=~Behaviour_Segment,values=~freq, marker = list(line = list(color = '#FFFFFF', width = 1)), type="pie",
                    textposition = 'auto',textinfo = 'text',
                    hoverinfo = 'text',source = "subset",
                    text=~paste(sub(" ","<br>",Behaviour_Segment),":","<br>",paste0(freq,"%") ),
                    insidetextfont = list(color = '#FFFFFF')) %>%
      layout(xaxis = ax, yaxis = ax,showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
    plot
  })
  
  #-----------------------------------------------#
  #                                               #
  #    KPI Numeric Vboxes                         #
  #                                               #
  #-----------------------------------------------#
  

  output$vbox_new_clients <- renderValueBox({
    # Value box number of new clients
    valueBox(value = KPI$results$New_clients,
               subtitle = "New clients", 
               color = "green",
               width = 3)
  })
  
  output$vbox_mean_revenue<- renderValueBox({
    # Value box total acquisition cost
    valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(KPI$results$Mean_Revenue), big.mark = ",")), "&#8364;")), icon = icon("euro"),
             subtitle = "Mean Revenue", 
             color = "yellow",
             width = 3)
  })
  
  output$vbox_total_revenue<- renderValueBox({
    # Value box total acquisition cost
    valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(KPI$results$Total_Revenue), big.mark = ",")), "&#8364;")), icon = icon("euro"),
             subtitle = "Total Revenue", 
             color = "yellow",
             width = 3)
  })
  
  
  output$vbox_mean_acquisition_cost <- renderValueBox({
    # Value box total acquisition cost
    valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(KPI$results$Mean_Cost), big.mark = ",")), "&#8364;")), icon = icon("euro"),
             subtitle = "Mean Acquisition Cost", 
             color = "red",
             width = 3)
  })
  
  output$vbox_total_acquisition_cost <- renderValueBox({
    # Value box total acquisition cost
    valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(KPI$results$Total_Cost), big.mark = ",")), "&#8364;")), icon = icon("euro"),
             subtitle = "Total Acquisition Cost", 
             color = "red",
             width = 3)
  })
  
  output$vbox_mean_profit <- renderValueBox({
    valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(KPI$results$Mean_Profit), big.mark = ",")), "&#8364;")), icon = icon("euro"),
             subtitle = "Mean Profit", 
             color = "blue",
             width = 3)
  })
  
  output$vbox_total_profit <- renderValueBox({
    valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(KPI$results$Total_Profit), big.mark = ",")), "&#8364;")), icon = icon("euro"),
             subtitle = "Total Profit", 
             color = "blue",
             width = 3)
  })
  
    #-------------------------------------------#
    #   Revenue 1M, revenue 2M, 3M, 6M          #
    #-------------------------------------------#
  
      output$vbox_mean_revenue_1M <- renderValueBox({
        valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(Others_KPI$results$Mean_Revenue_1M), big.mark = ",")), "&#8364;")), icon = icon("euro"),
                 subtitle = "Mean Revenue after 1M", 
                 color = "orange",
                 width = 3)
      })
  
      output$vbox_mean_revenue_2M <- renderValueBox({
        valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(Others_KPI$results$Mean_Revenue_2M), big.mark = ",")), "&#8364;")), icon = icon("euro"),
                 subtitle = "Mean Revenue after 2M", 
                 color = "orange",
                 width = 3)
      })
      
      output$vbox_mean_revenue_3M <- renderValueBox({
        valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(Others_KPI$results$Mean_Revenue_3M), big.mark = ",")), "&#8364;")), icon = icon("euro"),
                 subtitle = "Mean Revenue after 3M", 
                 color = "orange",
                 width = 3)
      })
      
      output$vbox_mean_revenue_6M <- renderValueBox({
        valueBox(value = HTML(paste0(sprintf("%s", prettyNum(as.numeric(Others_KPI$results$Mean_Revenue_6M), big.mark = ",")), "&#8364;")), icon = icon("euro"),
                 subtitle = "Mean Revenue after 6M", 
                 color = "orange",
                 width = 3)
      })
  
  
  
  # Dynamic slider from SQL result

  request_txt="select min(Registration_MonthDate),max(Registration_MonthDate) from user"
  request=dbSendQuery(conn, request_txt)
  data_query_result <- dbFetch(request)
  print(data_query_result)
  print(str(data_query_result))
  
  output$slider <- renderUI({
    sliderInput(
      "sliderserver",
      "Acquisition_period",
      min = as.Date(paste(data_query_result[1,1],"-01",sep="")),
      max = as.Date(paste(data_query_result[1,2],"-01",sep="")),
      value =  c( as.Date(paste(data_query_result[1,1],"-01",sep="")) , as.Date(paste(data_query_result[1,2],"-01",sep=""))),
      timeFormat="%b %Y"
    )
  })
  
  # Render plot animation
  output$new_clients_evolution <- renderPlotly({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if(input$By_variable == "---")
    {
      By_variable=paste(",", input$By_variable )
      By_variable=""
    }else {
      By_variable=paste("," , input$By_variable , sep="")
    }
    # Acquisition_source
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("select * , Round(Total_Revenue / New_Clients , 2) as Mean_Revenue from ( 
                      select Registration_MonthDate ",  By_variable , ", count(distinct a.User_id) as New_Clients, sum(Revenue) as Total_Revenue  from user a
                      left join (select User_id, sum(Revenue) as Revenue from monthly_user_revenue group by User_id)  b
                      on a.User_id=b.User_id
                      where Registration_MonthDate >= '", date_min ,"' and Registration_MonthDate <= '", date_max ,"'
                      ", add_request ,"
                      group by Registration_MonthDate ", By_variable , ") a",sep="")
    print("SQL_request")
    print(SQL_request)
    result=DBI::dbGetQuery(conn, SQL_request)
    if(input$By_variable == "---"){
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~New_Clients ) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"New_Clients"]))
          )
        )
    }else{
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~New_Clients, color = result[,input$By_variable]) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"New_Clients"]))
          )
        )
    }
    p
  })
  
  output$mean_revenue_evolution <- renderPlotly({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if(input$By_variable == "---")
    {
      By_variable=paste(",", input$By_variable )
      By_variable=""
    }else {
      By_variable=paste("," , input$By_variable , sep="")
    }
    # Acquisition_source
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("select * , Round(Total_Revenue / New_Clients , 2) as Mean_Revenue from ( 
select Registration_MonthDate ",  By_variable , ", count(distinct a.User_id) as New_Clients, sum(Revenue) as Total_Revenue  from user a
left join (select User_id, sum(Revenue) as Revenue from monthly_user_revenue group by User_id)  b
on a.User_id=b.User_id
where Registration_MonthDate >= '", date_min ,"' and Registration_MonthDate <= '", date_max ,"'
", add_request ,"
group by Registration_MonthDate ", By_variable , ") a",sep="")
    print("SQL_request")
    print(SQL_request)
    result=DBI::dbGetQuery(conn, SQL_request)
    if(input$By_variable == "---"){
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~Mean_Revenue ) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"Mean_Revenue"]))
          )
        )
      
    }else{
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~Mean_Revenue, color = result[,input$By_variable]) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"Mean_Revenue"]))
          )
        )
    }
    p
  })
  
  output$mean_revenue_1M_evolution <- renderPlotly({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if(input$By_variable == "---")
    {
      By_variable=paste(",", input$By_variable )
      By_variable=""
    }else {
      By_variable=paste("," , input$By_variable , sep="")
    }
    # Acquisition_source
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("select * , Round(Total_Revenue / New_Clients , 2) as Mean_Revenue_1M from ( 
select Registration_MonthDate ",  By_variable , ", count(distinct a.User_id) as New_Clients, sum(Revenue) as Total_Revenue  from user a
    left join monthly_user_revenue b
    on a.User_id=b.User_id
    where b.MonthDate < substr( datetime( Registration_MonthDate ||  '-01' ,'start of month','+1 month') ,1,7)
    and Registration_MonthDate >= '", date_min ,"' and Registration_MonthDate <= '", date_max ,"'
    ", add_request ,"
    group by Registration_MonthDate", By_variable , ")",sep="")
    result=DBI::dbGetQuery(conn, SQL_request)

    if(input$By_variable == "---"){
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~Mean_Revenue_1M ) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"Mean_Revenue_1M"]))
          )
        )
      
    }else{
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~Mean_Revenue_1M, color = result[,input$By_variable] ) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"Mean_Revenue_1M"]))
          )
        )
      
    }
    p
    
  })
  
  output$mean_revenue_2M_evolution <- renderPlotly({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if(input$By_variable == "---")
    {
      By_variable=paste(",", input$By_variable )
      By_variable=""
    }else {
      By_variable=paste("," , input$By_variable , sep="")
    }
    # Acquisition_source
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("select * , Round(Total_Revenue / New_Clients , 2) as Mean_Revenue_2M from ( 
select Registration_MonthDate ",  By_variable , ", count(distinct a.User_id) as New_Clients, sum(Revenue) as Total_Revenue  from user a
    left join monthly_user_revenue b
    on a.User_id=b.User_id
    where b.MonthDate < substr( datetime( Registration_MonthDate ||  '-01' ,'start of month','+2 month') ,1,7)
    and Registration_MonthDate >= '", date_min ,"' and Registration_MonthDate <= '", date_max ,"'
    ", add_request ,"
    group by Registration_MonthDate", By_variable , ")",sep="")
    result=DBI::dbGetQuery(conn, SQL_request)

    if(input$By_variable == "---"){
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~Mean_Revenue_2M ) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"Mean_Revenue_2M"]))
          )
        )
      
    }else{
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~Mean_Revenue_2M, color = result[,input$By_variable] ) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"Mean_Revenue_2M"]))
          )
        )
    }
    p
    
  })
  
  output$mean_revenue_3M_evolution <- renderPlotly({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if(input$By_variable == "---")
    {
      By_variable=paste(",", input$By_variable )
      By_variable=""
    }else {
      By_variable=paste("," , input$By_variable , sep="")
    }
    # Acquisition_source
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("select * , Round(Total_Revenue / New_Clients , 2) as Mean_Revenue_3M from ( 
                      select Registration_MonthDate ",  By_variable , ", count(distinct a.User_id) as New_Clients, sum(Revenue) as Total_Revenue  from user a
                      left join monthly_user_revenue b
                      on a.User_id=b.User_id
                      where b.MonthDate < substr( datetime( Registration_MonthDate ||  '-01' ,'start of month','+3 month') ,1,7)
                      and Registration_MonthDate >= '", date_min ,"' and Registration_MonthDate <= '", date_max ,"'
                      ", add_request ,"
                      group by Registration_MonthDate", By_variable , ")",sep="")
    result=DBI::dbGetQuery(conn, SQL_request)
    
    if(input$By_variable == "---"){
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~Mean_Revenue_3M ) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"Mean_Revenue_3M"]))
          )
        )
      
    }else{
      p <- plot_ly(result, x = ~Registration_MonthDate, y = ~Mean_Revenue_3M, color = result[,input$By_variable] ) %>%
        add_lines() %>%
        layout(
          yaxis = list(
            range=c(0,max(result[,"Mean_Revenue_3M"]))
          )
        )
    }
    p
    
  })
  
  output$mean_revenue_in_time <- renderPlotly({
    
    date_min=substr(input$sliderserver[1],1,7)
    date_max=substr(input$sliderserver[2],1,7)
    
    if(input$By_variable == "---")
    {
      By_variable=paste(",", input$By_variable )
      By_variable=""
    }else {
      By_variable=paste("," , input$By_variable , sep="")
    }
    # Acquisition_source
    if( is.null(input$Acquisition_source))
    {
      add_request=""
    }else{
      if( input$Acquisition_source =="All"){
        add_request=""
      }else{
        add_request=paste(" and Acquisition_Source='", input$Acquisition_source, "'",sep="")
      }
    }
    
    SQL_request=paste("select MonthDate ",   By_variable , ", ROUND(sum(Revenue)) as Revenue from user a 
                      left join monthly_user_revenue b
                      on a.User_id=b.User_id
                      where Registration_MonthDate >= '", date_min ,"' and Registration_MonthDate <= '", date_max ,"'
                      ", add_request ,"
                      group by MonthDate ", By_variable ,sep="")
    result=DBI::dbGetQuery(conn, SQL_request)


  if(input$By_variable == "---"){
    p <- plot_ly(result, x = ~MonthDate, y = ~Revenue ) %>%
      add_lines() %>%
      layout(
        yaxis = list(
          range=c(0,max(result[,"Revenue"]))
        )
      )
    
  }else{
    p <- plot_ly(result, x = ~MonthDate, y = ~Revenue, color = result[,input$By_variable] ) %>%
      add_lines() %>%
      layout(
        yaxis = list(
          range=c(0,max(result[,"Revenue"]))
        )
      )
  }
  p
  
})
  
}
