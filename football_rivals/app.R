#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Explore Historical Football Results"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
     DT::dataTableOutput("team1"),
     DT::dataTableOutput("team2")
     ),
      mainPanel(
      # Show a plot of the generated distribution
      tabsetPanel(type="tabs",
#        tabPanel("Games Played",plotOutput("totalGames",width="100%")),
        tabPanel("Progress", plotOutput("progress",width="100%")),
        tabPanel("Head to Head", plotOutput("head2head"),
                 tableOutput("head2headTable"),
                 h2("Fixture first played"),
                 textOutput("firstPlayed"),
                 h2("Fixture last played"),
                 textOutput("lastPlayed"),
                 h2("Biggest wins for Team 1"),
                 tableOutput("biggestWinsForTeam1"),
                 h2("Biggest wins for Team 2"),
                 tableOutput("biggestWinsForTeam2"))
      )
   )
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    library(dplyr)
  library(ggplot2)
   source("pre_process.R")
   teamList <- df %>% ungroup %>% arrange(desc(Season),tier,home) %>% select(home) %>% distinct
   output$team1 <- DT::renderDataTable(teamList,selection="single")
   
   team2Table <- reactive({
     
     team1 <- team1()
     df %>% filter(home==get("team1")) %>% ungroup %>% select(visitor) %>% distinct
   })
   
   output$team2 <- DT::renderDataTable(teamList,selection="single")

   selectedTeam <- reactive({
     selectedRow1 <- input$team1_rows_selected
     selectedRow2 <- input$team2_rows_selected
     slice(teamList,c(selectedRow1,selectedRow2)) %>% pull(home) %>% as.character
   })
   
    team1 <-   reactive({
      selectedRow1 <- input$team1_rows_selected
      if(is.null(selectedRow1)) selectedRow1 <- 1
      
      slice(teamList, selectedRow1) %>% pull(home) %>% as.character
    })
    
    team2 <-   reactive({
      selectedRow2 <- input$team2_rows_selected
      if(is.null(selectedRow2)) selectedRow2 <- 2
      
      slice(teamList, selectedRow2) %>% pull(home) %>% as.character
      
    })
   

  output$progress <- renderPlot({
     team1 <- team1()
     team2 <- team2()
     
     
     filter(df, home %in% c(get("team1"),get("team2"))) %>% 
       mutate(home=factor(home, levels=c(get("team1"),get("team2")))) %>% 
       ggplot(aes(x=Season,y=4-tier,col=home)) + 
       geom_line(alpha=0.4,lwd=3) + 
       scale_y_continuous(breaks = c(3,2,1,0),labels = c("Tier1","Tier2","Tier3","Tier4"),limits = c(0,4)) + 
       labs(y="") + 
       xlim(min(df$Season),max(df$Season)) +
       scale_color_manual(values= c("#66C2A5", "#FC8D62", "#8DA0CB"))
   }) 
  
  output$firstPlayed <- renderText({
    team1 <- team1()
    team2 <- team2()
        firstPlayed <- bind_rows(filter(df, home==get("team1"),visitor==get("team2")),
                        filter(df, home==get("team2"),visitor==get("team1"))) %>% 
        arrange(Season) %>% slice(1) %>% pull(Date)
    paste("Fixture first played on", firstPlayed)
  })
  
  output$lastPlayed <- renderText({
    team1 <- team1()
    team2 <- team2()
    lastPlayed <- bind_rows(filter(df, home==get("team1"),visitor==get("team2")),
                             filter(df, home==get("team2"),visitor==get("team1"))) %>% 
      arrange(desc(Date)) %>% slice(1) %>% pull(Date)
    paste("Fixture last played on", lastPlayed)
  })
  
#  output$firstPlayed <- verbatimTextOutput({
#    firstPlayed <- bind_rows(filter(df, home==get("team1"),visitor==get("team2")),
 #                    filter(df, home==get("team2"),visitor==get("team1"))) %>% 
#    arrange(Season) %>% slice(1) %>% pull(Date)
#    paste("Fixture first played on ",firstPlayed)
#  })
  
  output$head2headTable <- renderTable({
    
    team1 <- team1()
    team2 <- team2()
    
    bind_rows(filter(df, home==get("team1"),visitor==get("team2")),
                     filter(df, home==get("team2"),visitor==get("team1"))) %>% 
      count(winner) %>% 
      arrange(n) %>% rename(`Number of Wins`=n)
    
    
  })
  
  output$biggestWinsForTeam1 <- renderTable({
    team1 <- team1()
    team2 <- team2()
    
    bind_rows(filter(df, home==get("team1"),visitor==get("team2")),
              filter(df, home==get("team2"),visitor==get("team1"))) %>% 
      filter(winner == get("team1")) %>% 
      arrange(desc(abs(goaldif))) %>% 
      select(Date,Season,home,visitor,FT) %>% head(n=5) %>% 
      data.frame
    
  })
  
  output$biggestWinsForTeam2 <- renderTable({
    team1 <- team1()
    team2 <- team2()
    
    bind_rows(filter(df, home==get("team1"),visitor==get("team2")),
              filter(df, home==get("team2"),visitor==get("team1"))) %>% 
      filter(winner == get("team2")) %>% 
      arrange(desc(abs(goaldif))) %>% 
      select(Date,Season,home,visitor,FT) %>% head(n=5) %>% 
      data.frame
    
  })
  output$head2head <- renderPlot({
    team1 <- team1()
    team2 <- team2()
    
    dat <- bind_rows(filter(df, home==get("team1"),visitor==get("team2")),
              filter(df, home==get("team2"),visitor==get("team1"))) %>% 
    count(winner) %>% 
    mutate(winner = factor(winner,levels=c(get("team1"),get("team2"),"Draw")))
    
    # Add addition columns, needed for drawing with geom_rect.
    dat$fraction = dat$n / sum(dat$n)
    dat = dat[order(dat$fraction), ]
    dat$ymax = cumsum(dat$fraction)
    dat$ymin = c(0, head(dat$ymax, n=-1))
    
    ggplot(dat, aes(fill=winner, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(colour="grey30") +
      coord_polar(theta="y") +
      xlim(c(0, 4)) +
      labs(title="Basic ring plot") +
      theme_bw() +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      labs(title="Number of Wins") + scale_fill_manual(name="",values=c( "#66C2A5", "#FC8D62" ,"#8DA0CB"))
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

