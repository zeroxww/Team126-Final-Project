library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(bubbles)
library(plotly)


# read data
data_eu = read.csv("euw1_challengers_12.12.450.4196_matches.csv")
data_kr = read.csv("kr_challengers_12.12.450.4196_matches.csv")
data_oc = read.csv("oc1_challengers_12.12.450.4196_matches.csv")

data_champion = read.csv("TFT-Champions.csv")
data_class = read.csv("TFT_Classes.csv")
data_trait = read.csv("Champtrait.csv")

# UI part
ui <- dashboardPage(
  # layout color
    skin = "purple",
    
    dashboardHeader(title = "TFT Revolution"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "page1", icon = icon("line-chart")),
            menuItem("Database", tabName = "page2", icon = icon("area-chart"),
                     startExpanded = TRUE,
                     menuSubItem("Champions",tabName = "subpage1"),
                     menuSubItem("Synergies",tabName = "subpage2")
                     ),
            menuItem("Team Comps", tabName = "page3", icon = icon("map-o")),
            menuItem("Github Link",icon = icon("bar-chart-o"), href = "https://github.com/zeroxww/Team126-Final-Project")
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    box(title = "Description", status = "primary", 
                        solidHeader = TRUE, 
                        collapsible = TRUE,
                        width = 12,
                        tags$figure(align = "center",
                                    tags$img(src = "tft.jpg",width = 600),
                                   ),
                        p(),
                        p("Welcome to our Teamfight Tactics Helper for Season 7, Dragonlands."),
                        p("These infographics will give you an easy reference to start this new season and climb to a higher rank."),
                        p("By taking thousands of high-elo rank games from the League of Legends North America Server, we are able to discover the best way to play this game."),
                        p("You will get a trait overview, an item cheat sheet, and comp recommendations on our webpage.")
                        ),
                    
                    box(title = "About Teamfight Tactics (TFT)", status = "success", 
                        solidHeader = TRUE, 
                        collapsible = TRUE,
                        width = 12,
                        tags$figure(align = "center",
                                    tags$img(src = "tft_2.jpg",width = 600),
                        ),
                        p(),
                        p("Teamfight Tactics (TFT) is an auto battler game developed and published by Riot Games. The game is a spinoff of League of Legends and is based on Dota Auto Chess, where players compete online against seven other opponents by building a team to be the last one standing."),
                        p("Draft, deploy, and dominate with a revolving roster of League of Legends champions in a round-based battle for supremacy. Outsmart your opponents and adapt as you go—the strategy is all up to you."),
                        ),
                    
                    box(title = "About Season 7 (S7)", status = "danger", 
                        solidHeader = TRUE, 
                        collapsible = TRUE,
                        width = 12,
                        tags$figure(align = "center",
                                    tags$img(src = "tft_set7.jpg",width = 600),
                        ),
                        p(),
                        p("Teamfight Tactics Season 7 is live, and it is about Dragonlands. The seventh season takes place in the dragon realms where clans compete for power across various islands, each worshipping their own powerful ancient dragons locked behind the mysteries of dormant shrines. New champions, traits, items, and a new way to play TFT."),
                        ),
                    
                    box(title = "About us", status = "warning", 
                        solidHeader = TRUE, 
                        collapsible = TRUE,
                        width = 12,
                        p("Group member: Zhiruo Wang, Ziqi Xiao, Jiashu Yue, Andy Yu"),
                        p("We are Carey students from BARM and IS major, being new generations and new graduates, we are aware of the importance of data in today’s world. Instead of visualizing traditional fields, this time we decide to work on a game we all play: League of legends We believe in the power of passion, and we also hope you have fun on this journey.
")
                       )
                    ),
            
            tabItem(tabName = "subpage1",
                    tabBox(
                      title = "Champion information",
                      id = "tabset1",
                      width = 12,
                      tabPanel("1 cost",
                               dataTableOutput("cost_1_Table")
                               ),
      
                      tabPanel("2 cost", 
                               dataTableOutput("cost_2_Table")
                               ),
                      
                      tabPanel("3 cost", 
                               dataTableOutput("cost_3_Table")
                               ),
                      
                      tabPanel("4 cost", 
                               dataTableOutput("cost_4_Table")
                               ),
                      
                      tabPanel("5 cost", 
                               dataTableOutput("cost_5_Table")
                               ),
                      
                      tabPanel("8 cost", 
                               dataTableOutput("cost_8_Table")
                      ),
                      
                      tabPanel("10 cost", 
                               dataTableOutput("cost_10_Table")
                      )
                    )
            ),
            tabItem(tabName = "subpage2",
                    selectInput("var", 
                                label = "Choose a synergy to display",
                                choices = list("Classes", 
                                               "Origins"),
                                selected = "Classes"),
                    dataTableOutput("synergy_Table")
            ),
            tabItem(tabName = "page3",
                    fluidRow(
                      valueBox("Choose a country",selectInput("test",label = " ",
                                                                     choices = list("Europe","Korean","Other Countries"),
                                                                     selected = "Europe")),
                      valueBoxOutput("count"),
                      valueBoxOutput("users")
                    ),
                    fluidRow(
                      box(
                        width = 8, status = "info", solidHeader = TRUE,
                        title = "Popularity by synergy",
                        bubblesOutput("synergyPlot", width = "100%", height = 600)
                      ),
                      box(
                        width = 4, status = "info",
                        title = "Top synergy",
                        dataTableOutput("synergyTable")
                      ),
                    )
                    ),
            tabItem(tabName = "page4")
        )
    )
)


server <- function(input, output, session) {

datasetInput = reactive({
  if (input$test == "Europe")
    {data = data_eu}
  else if (input$test == "Korean")
    {data = data_kr}
  else 
    {data = data_oc}
  }) 

  
  
  champion_1 = data_champion %>% filter(Cost == "1")
  champion_2 = data_champion %>% filter(Cost == "2")
  champion_3 = data_champion %>% filter(Cost == "3")
  champion_4 = data_champion %>% filter(Cost == "4")
  champion_5 = data_champion %>% filter(Cost == "5")
  champion_8 = data_champion %>% filter(Cost == "8")
  champion_10 = data_champion %>% filter(Cost == "10")
  
    # page 2 - 1
    output$cost_1_Table = renderDataTable(
      datatable(champion_1,
                options = list(scrollX = TRUE), 
                rownames= FALSE)
    )
    
    output$cost_2_Table = renderDataTable(
      datatable(champion_2,
                options = list(scrollX = TRUE), 
                rownames= FALSE)
    )
    
    output$cost_3_Table = renderDataTable(
      datatable(champion_3,
                options = list(scrollX = TRUE), 
                rownames= FALSE)
    )
    
    output$cost_4_Table = renderDataTable(
      datatable(champion_4,
                options = list(scrollX = TRUE), 
                rownames= FALSE)
    )
    output$cost_5_Table = renderDataTable(
      datatable(champion_5,
                options = list(scrollX = TRUE), 
                rownames= FALSE)
    )
    
    output$cost_8_Table = renderDataTable(
      datatable(champion_8,
                options = list(scrollX = TRUE), 
                rownames= FALSE)
    )
    
    output$cost_10_Table = renderDataTable(
      datatable(champion_10,
                options = list(scrollX = TRUE), 
                rownames= FALSE)
    )
    
    
    # page 2-2
    output$synergy_Table = renderDataTable(
      
      if (input$var == "Classes")
      {datatable(data_class,
                 options = list(scrollX = TRUE), 
                 rownames= FALSE)}
      else
      {datatable(data_trait,
                 options = list(scrollX = TRUE), 
                 rownames= FALSE)}
    )
    
    # page 3
    output$count <- renderValueBox({
      valueBox(
        value = 28,
        subtitle = "Total Synergies",
        icon = icon("area-chart"),
        color = "teal"
      )
    })
    
    output$users <- renderValueBox({
      #read data
      data_1 = datasetInput() %>%
        select(starts_with('Set7'))
      
      synergy<- names(data_1)
      frequency_synergy <- colSums(!is.na(data_1))
      df <- data.frame(synergy,frequency_synergy)
      df <- df %>% arrange(desc(frequency_synergy))
      
      
      valueBox(
        value = nrow(data_1),
        subtitle = "Samples",
        icon = icon("users"),
        color = "olive" #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      )
    })
    
    output$synergyPlot = renderBubbles({
      #read data
      data_1 = datasetInput() %>%
        select(starts_with('Set7'))
      
      synergy<- names(data_1)
      frequency_synergy <- colSums(!is.na(data_1))
      df <- data.frame(synergy,frequency_synergy)
      df <- df %>% arrange(desc(frequency_synergy))
      
      #bubble chart
      bubbles(df$frequency_synergy, df$synergy,
              width = "600px", 
              height = "600px",
              color = rainbow(28, alpha=NULL)
              )
    })
    
    output$synergyTable = renderDataTable({
      #read data
      data_1 = datasetInput() %>%
        select(starts_with('Set7'))
      
      synergy<- names(data_1)
      frequency_synergy <- colSums(!is.na(data_1))
      df <- data.frame(synergy,frequency_synergy)
      df <- df %>% arrange(desc(frequency_synergy))
      
      #display the table
      datatable(df,
                options = list(scrollX = TRUE), 
                rownames= FALSE)
      })

}

shinyApp(ui = ui, server = server)
