library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(htmlwidgets)
library(RColorBrewer)

col <- brewer.pal(7, "Set1")

### Define Functions
# Base Market 1
base1 <- function(q){
  1-q
}

# Base Market 2
base2 <- function(s){
  1-s
}

### Simulate Data
x1 <- c()
x2 <- c()
count <- 1

for (i in seq(-5,5,0.01)) {
  x1[count] <- base1(i)
  x2[count] <- base2(i)
  count <- count + 1
}

### Create Dataframe
df <- data.frame(q_base= seq(-5,5,0.01),
                 s_base= seq(-5,5,0.01),
                 p_base = x1,
                 r_base = x2)

ui <- dashboardPage(
  dashboardHeader(

    title = "Ökonomik Digitaler Märkte",
    titleWidth = 300
    
  ),
  
  # (1) Sidebar ####
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText",
                      buttonId = "searchButton",
                      label = "Search..."),
    
    menuItem("Plattformmärkte (Monopol)", 
             icon = icon("line-chart"),
             menuSubItem("Nachfrage-Angebot", 
                         tabName = "plattform1"),
             menuSubItem("Mengen, Preise & Gewinn",
                         tabName = "preise"),
             menuSubItem("Sozialer Planer",
                         tabName = "sozialerPlaner")),
    
    menuItem("Plattformmärkte (Oligopol)",
             icon = icon("line-chart"),
             menuSubItem("Wettbewerbs- vs. Netzwerkeffekte",
                         tabName = "oligopol1"))
  ),
  
  # (2) Body ####
  dashboardBody(
    tabItems(
      
      # Nachfrage-Angebot ####
      tabItem(tabName = "plattform1",
              
              fluidRow(
                column(width = 4,
                       sliderInput("d1", label = "d", min = -2, max = 2,
                                   value = 0.2, step = 0.1)
                ),
                column(width = 4,
                       sliderInput("g1", label = "g", min = -2, max = 2,
                                   value = 0.2, step = 0.1)
                ),
                column(width = 4,
                       sliderInput("c1", label = "c", min = 0, max = 1,
                                   value = 0, step = 0.1)
                )
              ),
              fluidRow(
                       valueBoxOutput("summe1"),
                       valueBoxOutput("cost1")
              ),
              fluidRow(
                box(width = 6,
                       p("Markt 1 (p=1-q+ds)"),
                       plotOutput("plot1")),
                box(width = 6,
                       p("Markt 2 (r=1-s+gq)"),
                       plotOutput("plot2"))
              )
      ),
      
      ####### Preise, Mengen #########
      tabItem(tabName = "preise",
              
              fluidRow(
                column(width = 4,
                       sliderInput("d2", label = "d", min = -2, max = 2,
                                   value = 0.2, step = 0.1)
                ),
                column(width = 4,
                       sliderInput("g2", label = "g", min = -2, max = 2,
                                   value = 0.2, step = 0.1)
                ),
                column(width = 4,
                       sliderInput("c2", label = "c", min = 0, max = 1,
                                   value = 0, step = 0.1)
                )
                ),
              fluidRow(
                box(width = 8,
                    status = "primary", solidHeader = TRUE,
                    title = "Ergebnisstabelle",
                    tableOutput('table1')
                )
              ),
              fluidRow(
                box(width = 12,
                    status = "primary", solidHeader = TRUE,
                    title = "optimale Preise",
                    plotOutput("plot3"))
              ),
              fluidRow(
                box(width = 12,
                    status = "primary", solidHeader = TRUE,
                    title = "optimale Mengen & Gewinn",
                    plotOutput("plot4"))
              )
              ),
      
      ####### Sozialer Planer ######
      tabItem(tabName = "sozialerPlaner",
              
              fluidRow(
                column(width = 4,
                       sliderInput("d3", label = "d", min = -2, max = 2,
                                   value = 0.2, step = 0.1)
                ),
                column(width = 4,
                       sliderInput("g3", label = "g", min = -2, max = 2,
                                   value = 0.2, step = 0.1)
                ),
                column(width = 4,
                       sliderInput("c3", label = "c", min = 0, max = 1,
                                   value = 0, step = 0.1)
                )
              ),
              
              fluidRow(
                box(width = 12,
                    status = "primary", solidHeader = TRUE,
                    title = "optimale Preise",
                    plotOutput("plot5"))
              ),
              fluidRow(
                box(width = 10,
                    status = "primary", solidHeader = TRUE,
                    title = "Einfache Wohlfahrtsoptimierung (W1)",
                    tableOutput('table2')
                )
              ),
              fluidRow(
                box(width = 10,
                    status = "primary", solidHeader = TRUE,
                    title = "Wohlfahrtsoptimierung Nullgewinnbedingung (W2)",
                    tableOutput('table3')
                )
              ),
              fluidRow(
                box(width = 12,
                    status = "primary", solidHeader = TRUE,
                    title = "Wohlfahrtsvergleich (W1 / W2)",
                    plotOutput("plot6"))
              )
              ),
      
      #### Oligopol ########
      tabItem(tabName = "oligopol1",
              
              fluidRow(
                column(width = 4,
                       sliderInput("d4", label = "d", min = -2, max = 2,
                                   value = 0.2, step = 0.1)
                ),
                column(width = 4,
                       sliderInput("g4", label = "g", min = -2, max = 2,
                                   value = 0.2, step = 0.1)
                )
                ),
              fluidRow(
                valueBoxOutput("summe2")
              ),
              fluidRow(
                box(width = 12,
                    status = "primary", solidHeader = TRUE,
                    title = "Wettbewerbs- vs. Netzwerkeffekt",
                    plotOutput("plot7"))
              )
      )
    )
  )
  
)

server <- function(input, output, session) {
  
  ######### Nachfrage-Angebot ##########
  d1_out <- reactive({
    input$d1
  })
  
  g1_out <- reactive({
    input$g1
  })
  
  c1_out <- reactive({
    input$c1
  })
  
  ### Sum ###
  output$summe1 <- renderValueBox({
    
    result = d1_out() + g1_out()
    valueBox(subtitle = "Sum of Networkeffects",
      value=prettyNum(result))

      })
  
  ### Cost ###
  output$cost1 <- renderValueBox({
    
    result = c1_out()
    valueBox(subtitle = "Marginal Cost",
             value=prettyNum(result),
             color = "blue")
    
  })
  
  ### Plot 1 ###
  output$plot1 <- renderPlot({
    df1 <- df %>% mutate(p = p_base,
                         q = q_base+d1_out(),
                         r = r_base,
                         s = s_base+g1_out(),
                         mr = (q_base+d1_out()+g1_out())/2)
    c <- c1_out()
    
    ggplot(data=df1, aes(q,p)) + 
      # q
      geom_line(color=col[2]) +
      # marginal revenue
      geom_line(data=df1, aes(mr,p),
                color=col[3],
                linetype = 2) +
      # marginal cost
      geom_hline(yintercept = c, 
                 color=col[4]) +
      annotate("text",2, c, vjust = -1, 
               label = "c'", size=5, color=col[4]) +
      scale_x_continuous(expand = c(0, 0),
                         #breaks = c(0.5,1,1.5,2), 
                         limits = c(-0.5,2.5)) +
      scale_y_continuous(expand = c(0, 0),
                         #breaks = c(0.5,1,1.5,2), 
                         limits = c(-0.5,2.5)) +
      labs(title= "",x="q",y="p") +
      theme_classic(base_size = 14)
  })
  
  ### Plot 2 ###
  output$plot2 <- renderPlot({
    df1 <- df %>% mutate(p = p_base,
                         q = q_base+d1_out(),
                         r = r_base,
                         s = s_base+g1_out(),
                         mr = (q_base+d1_out()+g1_out())/2)
    c <- c1_out()
    
    ggplot(data=df1, aes(s,r)) + 
      # base market
      geom_line(color=col[2]) +
      # marginal rev
      geom_line(data=df1, aes(mr,p),
                color=col[3],
                linetype = 2) +
      # marginal cost
      geom_hline(yintercept = c, 
                 color=col[4]) +
      annotate("text",2, c, vjust = -1, 
               label = "c'", size=5, color=col[4]) +
      scale_x_continuous(expand = c(0, 0),
                         limits = c(-0.5,2.5)) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(-0.5,2.5)) +
      labs(title= "",x="s",y="r") +
      theme_classic(base_size = 14) 
  })
  
  #### Preise, Mengen ######
  d2_out <- reactive({
    input$d2
  })
  
  g2_out <- reactive({
    input$g2
  })
  
  c2_out <- reactive({
    input$c2
  })
  
  ### Table 1 ###
  output$table1 <- renderTable({
    
    d <- d2_out()
    g <- g2_out()
    c <- c2_out()
    sum <- d+g
    
    q <- (1-c)/(2-(d+g))
    s <- (1-c)/(2-(d+g))
    pi <- (1-c)^2/(2-(d+g))
    
    p <- c+((1-c)*(1-g))/(2-(g+d))
    r <- c+((1-c)*(1-d))/(2-(g+d))
    
    data.frame(d,g,sum,s,q,p,r,pi)
    
  })
  
  ### Plot 3 ###
  output$plot3 <- renderPlot({
    
    d <- seq(-2,2,0.01)
    g <- g2_out()
    c <- c2_out()
    
    p <- c+((1-c)*(1-g))/(2-(g+d))
    r <- c+((1-c)*(1-d))/(2-(g+d))
    
    df2 <- data.frame(d,p,r)
    
    ggplot(df2, aes(d,p)) + 
      # p
      geom_line(color=col[2],size=1) +
      annotate("text",-0.8, 0.1, 
               label = "p", color=col[2], size=5) +
      # r
      geom_line(data=df2, aes(d,r), color=col[3],
                size=1) +
      annotate("text",-0.8, 0.9, color=col[3],
               label = "r", size=5) +
      # c
      geom_hline(yintercept = c, color=col[4]) +
      annotate("text",1.1, c, vjust = -1, 
               label = "c", color=col[4], size=5) +
      
      labs(y="p,r",
           title=expression(paste("p = ",frac("(1-c)(1-g)","2-(d+g)"),", r = ",frac("(1-c)(1-d)","2-(d+g)")))) +
      scale_x_continuous(expand=c(0,0), limits = c(-1,1.2)) +
      scale_y_continuous(expand = c(0,0), limits = c(-1,1)) +
      theme_classic(base_size = 14)
    
  })
  
  ### plot 4 ###
  output$plot4 <- renderPlot({
    
    d <- seq(-2,2,0.01)
    g <- g2_out()
    c <- c2_out()
    
    q <- (1-c)/(2-(d+g))
    s <- (1-c)/(2-(d+g))
    pi <- (1-c)^2/(2-(d+g))
    
    df3 <- data.frame(d,g,q,s,pi)
    
    ggplot(df3, aes(d,q)) + 
      # q,s
      geom_line(color=col[2],size=1) +
      annotate("text",1.4, 10, 
               label = "q,s", color=col[2], size=7) +
      # pi
      geom_line(data=df3, aes(d,pi), color=col[3],
                size=1) +
      annotate("text",1.4, 9, color=col[3],
               label = expression(pi), size=7) +
      # c
      geom_hline(yintercept = c, color=col[4]) +
      annotate("text",1.4, c, vjust = -1, 
               label = "c'", color=col[4], size=5) +
      
      labs(y=expression(paste("q,s",pi)),
           title=expression(paste("q = s =",frac("1-c","2-(d+g)"),", ",pi," = ",frac("(1-c)"^"2","2-(d+g)")))) +
      scale_x_continuous(expand=c(0,0), limits = c(0,1.5)) +
      scale_y_continuous(expand = c(0,0), limits = c(-0.5,12)) +
      theme_classic(base_size = 14) 
    
  })
  
  ### Sozialer Planer ####
  d3_out <- reactive({
    input$d3
  })
  
  g3_out <- reactive({
    input$g3
  })
  
  c3_out <- reactive({
    input$c3
  })
  
  ### Plot 4 ###
  output$plot5 <- renderPlot({
    
    d <- seq(-2,2,0.01)
    g <- g3_out()
    c <- c3_out()
    
    p <- ((2*c*d)-(2*c)-d+g)/(d+g-2)
    r <- ((2*c*g)-(2*c)+d-g)/(d+g-2)
    
    df4 <- data.frame(d,p,r)
    
    ggplot(df4, aes(d,p)) + 
      # p
      geom_line(color=col[2],size=1) +
      annotate("text",-0.9, -0.9, 
               label = "p", color=col[2], size=5) +
      # r
      geom_line(data=df4, aes(d,r), color=col[3],
                size=1) +
      annotate("text",-0.9, 0.9, color=col[3],
               label = "r", size=5) +
      # c
      geom_hline(yintercept = c, color=col[4]) +
      annotate("text",1.1, c, vjust = -1, 
               label = "c'", color=col[4], size=5) +
      
      labs(y="p,r",
           title=expression(paste("p = ",frac("2cd-2c-d+g","2-(d+g)"),", r = ",frac("2cg-2c+d-g","2-(d+g)")))) +
      scale_x_continuous(expand=c(0,0), limits = c(-1,1.2)) +
      scale_y_continuous(expand = c(0,0), limits = c(-1,1.2)) +
      theme_classic(base_size = 14) 
  })
    
    ### Table 2 ###
    output$table2 <- renderTable({
      
      d <- d3_out()
      g <- g3_out()
      c <- c3_out()
      sum <- d+g
      
      PR <- -((c-1)^2*(d+g))/((1-(d+g))^2)
      KR <- (c-1)^2/((1-(sum))^2)
      W1 <- PR+KR
      
      q <- (c-1)/(d+g-1)
      s <- (c-1)/(d+g-1)
      
      p <- ((c*d)-c+g)/(d+g-1)
      r <- ((c*g)-c+d)/(d+g-1)
      
      
      data.frame(d,g,sum,s,q,p,r,PR,KR,W1)
    })
    
    ### Table 3 ###
    output$table3 <- renderTable({
      
      d <- d3_out()
      g <- g3_out()
      c <- c3_out()
      sum <- d+g
      
      PR <- 0
      KR <- (4*(c-1)^2)/((d+g-2)^2)
      W2 <- PR + KR
      
      q <- 2*(c-1)/(d+g-2)
      s <- 2*(c-1)/(d+g-2)
      
      p <- ((2*c*d)-(2*c)-d+g)/(d+g-2)
      r <- ((2*c*g)-(2*c)+d-g)/(d+g-2)
      
      
      data.frame(d,g,sum,s,q,p,r,PR,KR,W2)
    })
    
    ### Plot 6 ###
    output$plot6 <- renderPlot({
      
      x <- seq(-2,2,0.01)
      sum <- d3_out()+g3_out()
      c <- c3_out()
      
      w1 <- -(c-1)/(1-(x))
      w2 <- (4*(c-1)^2)/((x-2)^2)
      
      df5 <- data.frame(x,w1,w2)
      
      ggplot(df5, aes(x,w1)) + 
        # p
        geom_line(color=col[2],size=1) +
        annotate("text",.1, 11, 
                 label = "W1", color=col[2], size=5) +
        # r
        geom_line(data=df5, aes(x,w2), color=col[3],
                  size=1) +
        annotate("text",.1, 10, color=col[3],
                 label = "W2", size=5) +
        # c
        geom_hline(yintercept = c, color=col[4]) +
        annotate("text",.8, c, vjust = -1, 
                 label = "c'", color=col[4], size=5) +
        # d+g
        geom_vline(xintercept = sum, color=col[5]) +
        
        labs(y="p,r",x="d+g",
             title=expression(paste("W1 = ",frac("-(c-1)","1-(d+g)"),", W2 = ",frac("4(c-1)","2-(d+g)^2")))) +
        scale_x_continuous(expand=c(0,0), limits = c(0,1)) +
        scale_y_continuous(expand = c(0,0), limits = c(0,12)) +
        theme_classic(base_size = 14) 
    })
    
    #### Oligopol ####
    d4_out <- reactive({
      input$d4
    })
    
    g4_out <- reactive({
      input$g4
    })
    
    ### Summe 2 ###
    output$summe2 <- renderValueBox({
      
      result = d4_out() + g4_out()
      valueBox(subtitle = "Sum of Networkeffects",
               value=prettyNum(result))
      
    })
    
    ### Plot 7 ###
    output$plot7 <- renderPlot({
      
      n <- seq(2,24,1)
      sum <- d4_out()+g4_out()
      
      We <- 1/((n+2)*(n+1))
      Ne <- (1-sum)/(((n+2)-sum)*((n+1)-sum))
      Nwe <- (sum*(1-n^2-n+sum))/(((n+2)-sum)*((n+1)-sum)*(n+2)*(n+1))
      #Nwe <- Ne-We
      
      
      df6 <- data.frame(n,sum,We,Nwe)
      
      ggplot(df6, aes(n,We)) + 
        # qm
        geom_line(color=col[2], size=1) +
        annotate("text",5, 0.1, 
                 label = "Wettbewerbseffekt", color=col[2], size=5) +
        # Qd
        geom_line(data=df6, aes(n,Nwe), color=col[3], size=1) +
        annotate("text",5, -0.1, color=col[3],
                 label = "Netzwerkeffekt", size=5) +
        labs(y="Effekte",x="n") +
        scale_x_continuous(expand=c(0,0), limits = c(2,24), 
                           breaks = seq(2,24,1)) +
        scale_y_continuous(expand = c(0,0), limits = c(-0.2,0.2)) +
        theme(text = element_text(size=12)) 
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

## Pubish
# library(rsconnect)
# rsconnect::deployApp("~/GitHub/Lehre/DigitalMarkets/VL5_app")