library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)


# read data
# data = read.csv("data.csv")

# UI part
ui <- dashboardPage(
  # layout color
    skin = "purple",
    
    dashboardHeader(title = "TFT Revolution"),
    dashboardSidebar(
        # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search..."),
        sidebarMenu(
            menuItem("Overview", tabName = "page1", icon = icon("line-chart")),
            menuItem("Database", tabName = "page2", icon = icon("area-chart"),
                     startExpanded = TRUE,
                     menuSubItem("Champions",tabName = "subpage1"),
                     menuSubItem("Items",tabName = "subpage2"),
                     menuSubItem("Synergies",tabName = "subpage3")
                     ),
            menuItem("Team Comps", tabName = "page3", icon = icon("map-o")),
            menuItem("Github Link", tabName = "page4", icon = icon("bar-chart-o"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    box(title = "About the Application", status = "primary", 
                        solidHeader = TRUE, 
                        collapsible = TRUE,
                        width = 12,
                        textOutput("text1")
                        ),
                    box(title = "Dataset Information", status = "success", 
                        solidHeader = TRUE, 
                        collapsible = TRUE,
                        width = 12,
                        textOutput("text2"),
                        ),
                    box(title = "Group Members", status = "warning", 
                        solidHeader = TRUE, 
                        collapsible = TRUE,
                        width = 12,
                        textOutput("text3")
                       )
                    ),
            
            tabItem(tabName = "subpage1",
                    tabBox(
                      title = "Champion information",
                      id = "tabset1",
                      width = 12,
                      tabPanel("1 cost", "First tab content"),
                      tabPanel("2 cost", "Tab content 2"),
                      tabPanel("3 cost", "First tab content"),
                      tabPanel("4 cost", "First tab content"),
                      tabPanel("5 cost", "First tab content")
                    )
            ),
            tabItem(tabName = "subpage2",
                    fluidRow(
                      valueBoxOutput("rate"),
                      valueBoxOutput("count"),
                      valueBoxOutput("users")
                    ),
                    fluidRow(
                      box(
                        width = 8, status = "info", solidHeader = TRUE,
                        title = "Popularity by items"
                        #bubblesOutput("packagePlot", width = "100%", height = 600)
                      ),
                      box(
                        width = 4, status = "info",
                        title = "Top items",
                        #tableOutput("packageTable")
                      ),
                    )
            ),
            tabItem(tabName = "subpage3",
                    selectInput("var", 
                                label = "Choose a synergy to display",
                                choices = list("Classes", 
                                               "Origins"),
                                selected = "Classes"),
                    textOutput("synergy_text", container = tags$h3)
            ),
            tabItem(tabName = "page3",
                    leafletOutput("myMap", width="100%")
                    ),
            tabItem(tabName = "page4",
                    dataTableOutput("table1"))
        )
    )
)


server <- function(input, output, session) {

  # learn how to make the searchbar work later
  # input$searchText
  # input$searchButton
    
  output$rate <- renderValueBox({
    valueBox(
      value = 25,
      subtitle = "Downloads per sec",
      icon = icon("area-chart"),
    )
  })
  
  output$count <- renderValueBox({
    valueBox(
      value = 50,
      subtitle = "Total items",
      icon = icon("download")
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      value = 1000,
      subtitle = "Sample",
      icon = icon("users")
    )
  })
    
    output$text1 = renderText("tft")
    
    output$text2 = renderText("Source:")
    
    output$text3 = renderText("Zhiruo Wang, Yuejia Shu, Lisa Xiao, Andy Yu")
    
    output$synergy_text = renderText({
          if (input$var == "Classes")
          {paste("Assassin")}
      else
          {paste("Astral")}
          })

}

shinyApp(ui = ui, server = server)
