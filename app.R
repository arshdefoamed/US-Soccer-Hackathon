library(shinythemes)
library(shiny)
library(shinydashboard)
library(markdown)
library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(jpeg)
library(DT)
library(grid)
library(rsconnect)
library(scales)
library(leaflet)
library(geosphere)
library(gridExtra)
library(maptools)
library(raster)
library(stringi)
library(htmltools)
library(ggthemes)
library(plotly)
library(pracma)
#Preprocessing 
mls_11_12_df=fread( file = "Hackathon Raw Files//Trimmed Columns - Opta/MLS/MLS 2011-2012 trimmed.csv",header=TRUE,select=c("player","team","player_id","team_id","away","home","game_date","game_id"))
mls_12_13_df=fread( file = "Hackathon Raw Files//Trimmed Columns - Opta/MLS/MLS 2012-2013 trimmed.csv",header=TRUE,select=c("player","team","player_id","team_id","away","home","game_date","game_id"))
mls_13_14_df=fread( file = "Hackathon Raw Files//Trimmed Columns - Opta/MLS/MLS 2013-2014 trimmed.csv",header=TRUE,select=c("player","team","player_id","team_id","away","home","game_date","game_id"))
mls_14_15_df=fread( file = "Hackathon Raw Files//Trimmed Columns - Opta/MLS/MLS 2014-2015 trimmed.csv",header=TRUE,select=c("player","team","player_id","team_id","away","home","game_date","game_id"))
mls_15_16_df=fread( file = "Hackathon Raw Files//Trimmed Columns - Opta/MLS/MLS 2015-2016 trimmed.csv",header=TRUE,select=c("player","team","player_id","team_id","away","home","game_date","game_id"))
mls_16_17_df=fread( file = "Hackathon Raw Files//Trimmed Columns - Opta/MLS/MLS 2016-2017 trimmed.csv",header=TRUE,select=c("player","team","player_id","team_id","away","home","game_date","game_id"))
mls_17_18_df=fread( file = "Hackathon Raw Files//Trimmed Columns - Opta/MLS/MLS 2017-2018 trimmed.csv",header=TRUE,select=c("player","team","player_id","team_id","away","home","game_date","game_id"))
mls_18_19_df=fread( file = "Hackathon Raw Files//Trimmed Columns - Opta/MLS/MLS 2018-2019 trimmed.csv",header=TRUE,select=c("player","team","player_id","team_id","away","home","game_date","game_id"))
knn=fread( file = "knn_values.csv",header=TRUE)
pen=fread( file = "penalty.csv",header=TRUE)
#mls_18_19_df$game_date=as.Date(mls_18_19_df$game_date, "%Y-%m-%d %H:%M:%S")
player_id_name_team=fread( file = "player_id_team.csv",header=TRUE)
all_player_stats=fread( file = "new.csv",header=TRUE)
datasets=list(mls_11_12_df,mls_12_13_df,mls_13_14_df,mls_14_15_df,mls_15_16_df,mls_16_17_df,mls_17_18_df,mls_18_19_df)
all_data=rbindlist(datasets)
#all_data<-all_data[!(all_data$player!=""),]
player_stats=fread( file = "new.csv",header=TRUE)
mls_18_19_df$game_date=as.Date(mls_18_19_df$game_date, "%Y-%m-%d %H:%M:%S")


#players_list <- vector("list", length(players_id))
#players_id=unique(all_data[,c('player_id')])
#len=1
#for(x in players_id){
#for (y in x){
#player_found=unique(subset(all_data, all_data$player_id==y , 
 #      select=c('player')))
#players_list[len]<-player_found[1]
#len=len+1
#}}

players_list= unique(all_data[,c('player')])
clubs=unique(all_data[,c('team')])
#clubs[order(clubs)]
#players[order(players)]
players_list=players_list[2:1504]
len=1
#player_clubs<-[]
player_clubs<-NULL
player_clubs <- vector("list", length(players_list))


for (x in clubs){
  for (y in x){
  print(y)
  subset_all_data=unique(subset(all_data, all_data$team==y , 
                         select=c('player')))
  player_clubs[len]<-subset_all_data
  len=len+1
  }
}

jp <- jpeg::readJPEG('MLSlogo.jpg')
#player_11_12 <- na.omit(player_11_12)
#clubs_11_12 <- na.omit(clubs_11_12)
#players[which(names(players) %in% c("",NULL))] <- NULL
#player_11_12=purrr::discard(player_11_12,.p = ~stringr::str_detect(.x,""))
lengt=length(player_clubs)

#names(player_clubs) <- clubs
is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

subset_dataset<-function(club){
  #club="Atlanta United FC"
 temp=subset(mls_18_19_df,mls_18_19_df$home==club | mls_18_19_df$away==club)
 temp[rev(order(temp$game_date)),]
 temp=temp %>%
   group_by(game_date) 
 temp
}


shinyApp(
  ui = navbarPage("U.S Soccer\n",
                  theme = shinytheme("superhero"),
                tabPanel(h2("1"),
                  sidebarLayout(
                    sidebarPanel(width = 3,h1("MLS Player Scouting\n"),
                            #plotOutput("MLSLogo",hover = hoverOpts(id ="plot_hover")),
                            "",
                            h3("Select Club"),
                            column(12,selectInput("club_select", "", clubs,selected="Real Salt Lake")),
                            h3("Select Player"),
                            column(12,selectInput("player_select", "",player_clubs[[1]]))),
                    mainPanel (
                      fluidRow(width = 11,     
                            column(6,column(12, htmlOutput("caption1")) ,
                                   column(12, htmlOutput("caption2")) ,
                                          column(12, htmlOutput("caption3")) ,
                                                 column(12, htmlOutput("caption4")) ,
                                                        column(12, htmlOutput("caption5"))),
                            column(5,
                            "Similar Performance by other Players",
                                                               column(12, htmlOutput("caption6")) ,
                                                               column(12, htmlOutput("caption7")))
                                   
                                   )))),
                  tabPanel(h2("2"),
                           h3("Select Club"),
                           column(12,selectInput("club_select_tab2", "", clubs,selected="Real Salt Lake"))
                           
                  )
  ),
  server = function(input, output,session) {
    #output$plot <- renderPlot( plot(head(cars, input$n)) )
    output$myImage <- renderPlot({
      g <-  annotation_custom(rasterGrob(jp, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
      g
    })
    observe({
    club_selected <- input$club_select
    club_selected_index= which(clubs[[1]] %in% club_selected)
    updateSelectInput(session, "player_select",
                     
                      choices = player_clubs[[club_selected_index]])}
                      
    )
    observe({
      club_selected <- input$club_select
      player_selected <- input$player_select
      player_stats_player=subset(player_stats, player_stats$player==player_selected & player_stats$clubName==club_selected )
      output$caption1 <- renderText({
        text=""
        if (!is.null(input$player_select) | input$player_select==""){
        if (is.integer0(player_stats_player[['pass']])){
          HTML("<h3>PASS:","0","<br>")
        }
        else{
          HTML("<h3>PASS:",player_stats_player[['pass']],"<br>")
        }}
        

        
      })
      observe({
      output$caption2 <- renderText({
        text=""
        
        if (is.integer0(player_stats_player[['goal']])){
          HTML("<h3>GOAL:","0","<br>")
        }
        else{
          HTML("<h3>GOAL:",player_stats_player[['goal']],"<br>")
        }
      
      })})
      observe({
      output$caption3 <- renderText({
        text=""
        
        if (is.integer0(player_stats_player[['cross']])){
          HTML("<h3>CROSS:","0","<br>")
        }
        else{
          HTML("<h3>CROSS:",player_stats_player[['cross']],"<br>")
        }
        
      })})
      output$caption4 <- renderText({
        text=""
        
        if (is.integer0(player_stats_player[['shot']])){
          HTML("<h3>SHOT:","0","<br>")
        }
        else{
          HTML("<h3>SHOT:",player_stats_player[['shot']],"<br>")
        }
        
      })
      output$caption5 <- renderText({
        text=""
        
        if (is.integer0(player_stats_player[['tackle']])){
          HTML("<h3>TACKLE:","0","<br>")
        }
        else{
          HTML("<h3>TACKLE:",player_stats_player[['tackle']],"<br>")
        }
        
        
      })
      output$caption6 <- renderText({
        
        subset_player<-unique(subset(player_id_name_team,
                           player_id_name_team$player==player_selected & player_id_name_team$V3==club_selected,
                           select=c('player_id')))
        
        
        
        subset_knn<-subset(knn,
                           knn$player_id==subset_player[[1]],
                           select=c(3,4,5))
        text=""
        len=1
        for (x in subset_knn[1])
          {
            for (y in x){
              print(y)
              subset_temp_data<-unique(subset(all_player_stats,all_player_stats$player_id==y))
              text=paste(text, "<br>", sep=" ")
              text=paste(text, "Player Name:", sep=" ")
              text=paste(text, subset_temp_data$player, sep=" ")
              text=paste(text, "<br>", sep=" ")
              text=paste(text, "Current Club Name:", sep=" ")
              text=paste(text, subset_temp_data$clubName, sep=" ")
              text=paste(text, "<br>", sep=" ")
              text=paste(text, "Goal:", sep=" ")
              text=paste(text, subset_temp_data$goal, sep=" ")
              text=paste(text, "<br>", sep=" ")
              text=paste(text, "Shot:", sep=" ")
              text=paste(text, subset_temp_data$shot, sep=" ")
              text=paste(text, "<br>", sep=" ")
              text=paste(text, "Cross", sep=" ")
              text=paste(text, subset_temp_data$cross, sep=" ")
              text=paste(text, "<br>", sep=" ")
              text=paste(text, "Pass", sep=" ")
              text=paste(text, subset_temp_data$pass, sep=" ")
              text=paste(text, "<br>", sep=" ")
              text=paste(text, "Tackle", sep=" ")
              text=paste(text, subset_temp_data$tackle, sep=" ")
              text=paste(text, "<br>", sep=" ")
              text=paste(text, "<br>", sep=" ")
              len=len+1
            }
          }
        HTML("<H3>",text[1],"</H3>")                 
      })
      output$caption7 <- renderText({
  
      })
      
    observe({
      club_select_tab_2<=input$club_select_tab2
      temp_data=subset_dataset(club_select_tab_2)
      temp_list=unique(temp_data[7])
      
      latest_dates=temp_list[c(2,3,4,6,7),]
      temp_data=subset(temp_data,temp_data$game_date %in% list(latest_dates))
    }) 
    })
    output$heatMap<- renderPlot(
      ggplot(fatalities_df, aes(x=long, y=lat, group=group, fill=numbers))+ ggtitle("") + 
        geom_polygon()+
        scale_fill_gradientn(
          colours=c("lightgreen","yellow","orange","darkorange" ,"red"),
          #  values=rescale(c(-3, -2, -1,0)),
          guide="colorbar"
        )+theme_void()+
        theme(
          plot.title = element_text(color="Yellow", size=20),
          legend.title = element_text(colour = 'white',size=15),
          legend.text=element_text(colour = 'white',size=15),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="midnightblue"),
          plot.background = element_rect(fill = "midnightblue")
        ))
    })
    
  }


)