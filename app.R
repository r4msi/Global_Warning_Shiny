
library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(ggplot2)
library(forecast)
library(ggthemes)

lb <- shinyDashboardLogoDIY(
    
    boldText = "",
    mainText = "",
    textSize = 20,
    badgeText = "GLOBAL WARMING",
    badgeTextColor = "white",
    badgeTextSize = 2,
    badgeBackColor = "#40E0D0",
    badgeBorderRadius = 3
    
    
)

########################################################
theme_grey_blue <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "rgb(255,255,255)"
    ,primaryFontColor = "rgb(255,255,255)"
    ,infoFontColor = "rgb(255,255,255)"
    ,successFontColor = "rgb(255,255,255)"
    ,warningFontColor = "rgb(255,255,255)"
    ,dangerFontColor = "rgb(255,255,255)"
    ,bodyBackColor = cssGradientThreeColors(
        direction = "down",
        colorStart = "rgb(52,62,70)",
        colorMiddle = "rgb(52, 62, 80)",
        colorEnd = "rgb(89, 98, 112)",
        colorStartPos = 0,
        colorMiddlePos = 30,
        colorEndPos = 100
    )
    
    ### header
    ,logoBackColor = "rgb(70,80,90)"
    
    ,headerButtonBackColor = "rgb(70,80,90)"
    ,headerButtonIconColor = "rgb(25,35,45)"
    ,headerButtonBackColorHover = "rgb(40,50,60)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"
    
    ,headerBackColor = "rgb(70,80,90)"
    ,headerBoxShadowColor = ""
    ,headerBoxShadowSize = "0px 0px 0px"
    
    ### sidebar
    ,sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "rgb(52, 62, 80)"
        ,colorMiddle = "rgb(52, 62, 80)"
        ,colorEnd = "rgb(89, 98, 112)"
        ,colorStartPos = 0
        ,colorMiddlePos = 70
        ,colorEndPos = 100
    )
    
    ,sidebarShadowRadius = ""
    ,sidebarPadding = 10
    ,sidebarShadowColor = "0px 0px 0px"
    
    ,sidebarMenuBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "rgb(52,62,70)"
        ,colorMiddle = "rgb(52, 62, 80)"
        ,colorEnd = "rgb(89, 98, 112)"
        ,colorStartPos = 0
        ,colorMiddlePos = 70
        ,colorEndPos = 100
    )
    ,sidebarMenuPadding = 5
    ,sidebarMenuBorderRadius = 20
    
    ,sidebarUserTextColor = "rgb(128,177,221)"
    
    ,sidebarSearchBackColor = "rgb(40,70,115)"
    ,sidebarSearchIconColor = "rgb(50,115,145)"
    ,sidebarSearchBorderColor = "rgb(30,60,105)"
    
    ,sidebarTabTextColor = "rgb(128,177,221)"
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = 0
    
    ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgb(56,137,189)"
        ,colorMiddle = "rgb(65,95,145)"
        ,colorEnd = "rgb(68,84,137)"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorSelected = "rgb(255,255,255)"
    ,sidebarTabRadiusSelected = "30px"
    
    ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgb(56,137,189)"
        ,colorMiddle = "rgb(65,95,145)"
        ,colorEnd = "rgb(68,84,137)"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorHover = "rgb(255,255,255)"
    ,sidebarTabBorderStyleHover = "none"
    ,sidebarTabBorderColorHover = "none"
    ,sidebarTabBorderWidthHover = 0
    ,sidebarTabRadiusHover = "30px"
    
    ### boxes
    ,boxBackColor = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgb(70,75,125)"
        ,colorMiddle = "rgb(65,79,129)"
        ,colorEnd = "rgb(55,70,120)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,boxBorderRadius = 15
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = ""
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(49,56,107)"
    ,boxPrimaryColor = "rgb(141,192,241)"
    ,boxInfoColor = "rgb(20,100,160)"
    ,boxSuccessColor = "rgb(64,186,170)"
    ,boxWarningColor = "rgb(255,217,144)"
    ,boxDangerColor = "rgb(249,144,144)"
    
    ,tabBoxTabColor = "rgb(80,95,155)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(128,177,221)"
    ,tabBoxTabTextColorSelected = "rgb(255,255,255)"
    ,tabBoxBackColor = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgb(70,75,125)"
        ,colorMiddle = "rgb(65,79,129)"
        ,colorEnd = "rgb(55,70,120)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,tabBoxHighlightColor = "rgb(80,95,155)"
    ,tabBoxBorderRadius = 15
    
    ### inputs
    ,buttonBackColor = "rgb(230,230,230)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(50,50,50)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(180,180,180)"
    ,buttonTextColorHover = "rgb(50,50,50)"
    ,buttonBorderColorHover = "rgb(50,50,50)"
    
    ,textboxBackColor = "rgb(68,80,90)"
    ,textboxBorderColor = "rgb(76,90,103)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(80,90,100)"
    ,textboxBorderColorSelect = "rgb(255,255,255)"
    
    ### tables
    ,tableBackColor = "transparent"
    ,tableBorderColor = "rgb(80,95,155)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
    
)
########################################### DATA

gw <- readRDS(file = "rds/global_w.rds")
hw <- readRDS(file = "rds/hw.rds")
ar <- readRDS(file = "rds/arima.rds")
ar = ar$mean
nn = readRDS("rds/nn.rds")
se = readRDS("rds/season.rds")
de = readRDS("rds/decompose.rds")

a1 = readRDS("rds/a1.rds")
a2 = readRDS("rds/a2.rds")
a3 = readRDS("rds/a3.rds")
a4 = readRDS("rds/a4.rds")
a5 = readRDS("rds/a5.rds")
a6 = readRDS("rds/a6.rds")

ac.ar = readRDS("rds/aca.rds")
ac.hw = readRDS("rds/achw.rds")
ac.nn = readRDS("rds/acnn.rds")

ar.test = readRDS("rds/arimatest.rds")
hw.test = readRDS("rds/hwtest.rds")
nn.test = readRDS("rds/nntest.rds")




############################################ Header
header <- dashboardHeader(
  title = lb
    )

############################################## sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        HTML(paste0(
            "<br>",
            "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://image.flaticon.com/icons/svg/289/289892.svg' height = '60' width = '120'></a>",
            "<br>"
            
        
        )),
        menuItem("Parameters",
                 icon = icon("tasks"),
                 tabName = "parameters",
                 startExpanded = T,
                 sliderInput(inputId = "date"
                             ,label = "Date"
                             ,min = 1970
                             ,max = 2010
                             ,value = 1970
                             ,animate = T
                     
                    )
                 
                 
                 
                 
        ),
        menuItem("Models"
                 ,icon = icon("sun", lib = "font-awesome")
                 ,tabName = "pred"
                 ,selected = T
                 
        ),
        
        menuItem("Arima Game"
                 ,icon = icon("gamepad",lib = "font-awesome")
                 ,tabName = "arimagame"
                
            
        ),
        menuItem("Train/Test"
                 ,icon = icon("vial", lib = "font-awesome")
                 ,tabName = "tt"
            
        ),
        menuItem("Download Report"
                    ,icon = icon("download", lib = "font-awesome")
                    ,downloadButton(outputId = "datadown",
                                    label = "Download"  
                    )
        )
        
        
        ,HTML(paste0(
            
            "<table style='margin-left:auto; margin-right:auto;'>",
            "<tr>",
            "<td style='padding: 5px;'><a href='https://github.com/r4msi' target='_blank'><i class='fab fa-github'></i></i></i></a></td>",
            "<td style='padding: 5px;'><a href='https://www.linkedin.com/in/manuel-alda-mart%C3%ADn-mora-566ba419b/' target='_blank'><i class='fab fa-linkedin-in'></i></i></a></td>",
            "</table>",
            "<br>"))
        
    )
    
    
)

####################################################### body
body <- dashboardBody(
    theme_grey_blue,
    tabItems(
        tabItem(tabName = "pred",
                gradientBox(
                    title = "Models"
                    ,icon = "fa fa-th"
                    ,gradientColor = "maroon"
                    ,boxToolSize = "xs"
                    ,width = 6
                    ,footer = " ",radioButtons(inputId = "models"
                                           ,label = "Models"
                                           ,choices = list("All","Neural Network","ARIMA","Holt-Winters")
                                           ,selected = "All"
                                           
                                           
                    )
                )
                ,gradientBox(
                    title = "Data"
                    ,icon = "fa fa-th"
                    ,gradientColor = "teal"
                    ,boxToolSize = "xs"
                    ,width =6
                    ,footer = " ",valueBoxOutput("like",12) 
                                           
                                           
                    )
                
                
               
                ,widgetUserBox(
                    title = "Charts"
                    ,src='https://image.flaticon.com/icons/svg/289/289892.svg'
                    ,background = T
                    ,backgroundUrl = "https://www.publicdomainpictures.net/pictures/280000/velka/gradient-colors-blur-background.jpg"
                    ,width = 12
                    
                 
                    ,footer_padding = F
                    ,footer = plotlyOutput("timeseries")
                    
                    
                )
         
                
                
                
        ),
        tabItem(tabName = "arimagame"
                ,gradientBox(
                    title = "Arima[12]"
                    ,icon = "fa fa-bar-chart "
                    , width = 6
                    ,gradientColor = "teal"
                    ,boxToolSize = "sm"
                    ,footer = selectInput(inputId = "arimasinput"
                                          ,label = NULL
                                          ,choices = list(
                                              "(0,1,1)(0,1,1)",
                                              "(1,1,1)(1,1,1)",
                                              "(2,1,1)(2,1,1)",
                                              "(2,1,2)(2,1,2)",
                                              "(2,2,2)(2,2,2)",
                                              "(2,2,2)(2,3,2)")
                                          ,selected = "(2,2,2)(2,3,2)"

                    ), br(),br(),"Select created ARIMAs or try yours! Notice that high numbers may not work.", br(),
                    "Recall that lowecase letters are the not seasonal part, being p(AR), d(diff), q(MA)", br(), 
                    "Uppercase letters stand for the Seasonal part, being P(AR), D(DIFF), Q(MA).", br(),br(),br()

                )
              
              ,fluidRow(
                gradientBox(title = "Your Arima"
                            ,gradientColor = "orange"
                            ,width = 6
                            
                  ,column(width = 4,
                         sliderInput(inputId = "p", label ="p", min = 0, max=2,value = 0),sliderInput(inputId = "D", label="D",min= 0, max=2, value=0)
                  ),
                  column(width = 4, 
                         sliderInput(inputId = "q", label ="q", min=0, max=2, value =0),sliderInput(inputId = "P", label="P",min= 0, max=2, value =0)
                  ),
                  column(width =4,sliderInput(inputId = "d", label= "d",min= 0, max=3, value =0),sliderInput(inputId = "Q", label="Q",min= 0, max=2, value=0))
                ))
                            
                    
                    
                
                ,widgetUserBox(
                    title = "Forecast"
                    ,src = "https://image.flaticon.com/icons/svg/289/289892.svg"
                    ,background = T
                    ,backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350"
                    ,width = 12
                    ,footer_padding = F
                    
                    
                    ,footer = plotlyOutput("ar")
                    
                )
               
            
        )
        ,tabItem(
            tabName = "tt"
            
            ,gradientBox(
                title = "Holt-Winters"
                ,icon = "fa fa-bar-chart "
                
                ,gradientColor = "teal"
                ,boxToolSize = "sm"
                ,footer = plotOutput("hwtest")
            )
            ,gradientBox(
                title = "Arima[12]"
                ,icon = "fa fa-bar-chart "
                
                ,gradientColor = "info"
                ,boxToolSize = "sm"
                ,footer = plotlyOutput("artest")
            )
            ,gradientBox(
                title = "NN"
                ,icon = "fa fa-bar-chart "
                
                ,gradientColor = "orange"
                ,boxToolSize = "sm"
                ,footer = plotOutput("nntest")
            )
            ,gradientBox(
                title = "Accuracy"
                ,icon =  "fa fa-check-square"
                ,gradientColor = "black"
                ,footer = 
                        br()
                        ,"HW"
                        ,tableOutput("achw")
                        ,br()
                        ,"Arima"
                        ,br()
                        ,tableOutput("acar")
                        ,br()
                        ,"NN"
                        ,br()
                        ,tableOutput("acnn")
            )
            
            
        )
                
    )
)

####################################################### ui

ui <- dashboardPage(header, sidebar, body, title = "GW")


####################################################### server
server <- function(input, output) {

    output$timeseries <- renderPlotly ({
        
        
        a = autoplot(window(gw, start = c(input$date,1))) + 
            geom_line(color="turquoise") + theme_solarized_2(light = F) + 
            labs(y="Temperature") 
        
        
        if (input$models == "Holt-Winters") {
            b = a + autolayer(hw, series="H-W")
        } else if (input$models == "ARIMA") { 
            b = a + autolayer(ar, series = "Arima") 
        } else if (input$models == "Neural Network") {
            b = a + autolayer(nn, series="NN") 
        } else if (input$models == "All") {
            b = a + autolayer(nn, series="N") +autolayer(ar, series = "Ar")+ autolayer(hw, series="H-W")
        } 
        
        ggplotly(b)
    })
    
  
    
    output$ar <- renderPlotly({
        
        a = autoplot(window(gw, start = c(input$date,1))) + 
            geom_line(color="turquoise") + theme_solarized_2(light = F) + 
            labs(y="Temperature") 
        
        if (input$arimasinput ==  "(0,1,1)(0,1,1)") {
            b = a + autolayer(a1, series = "(0,1,1)(0,1,1)")
        } else if (input$arimasinput == "(1,1,1)(1,1,1)") {
            b = a + autolayer(a2, series = "(1,1,1)(1,1,1)")
        } else if (input$arimasinput == "(2,1,1)(2,1,1)") {
            b = a + autolayer(a3, series = "(2,1,1)(2,1,1)")
        } else if (input$arimasinput == "(2,1,2)(2,1,2)") {
            b = a + autolayer(a4, series = "(2,1,2)(2,1,2)")
        } else if (input$arimasinput == "(2,2,2)(2,2,2)") {
            b = a + autolayer(a5, series = "(2,2,2)(2,2,2)")
        } else if (input$arimasinput == "(2,2,2)(2,3,2)") {
            b = a + autolayer(a6, series = "(2,2,2)(2,3,2)")
        }
        
        f = forecast(Arima(gw, order = c(input$p, input$d, input$q), seasonal = c(input$P, input$D, input$Q)),h=60)
        
        
        b = b + autolayer(f$mean, color = "green")
        
        ggplotly(b)
    })
    
    output$hwtest <- renderPlot({
        hw.test
    })
    
    output$artest <- renderPlotly({
        ggplotly(ar.test)
    })
    
    output$nntest <- renderPlot({
        nn.test
    })
    
    output$achw <- renderTable({
        ac.hw
    })
    
    output$acar <- renderTable({
        ac.ar
    })
    
    output$acnn <- renderTable({
        ac.nn
    })
    
    
    output$like <- renderValueBox({
     
        valueBox(
            "HERE", "NASA DATA", icon = icon("user-astronaut", lib = "font-awesome"),
            color = "blue", href = "https://data.giss.nasa.gov/gistemp/"
        )
    })
    
    output$datadown <- downloadHandler(
        
        filename = function() {
            paste("GlobalW", "html", sep=".")
        },
        content = function(file) {
            file.copy(from = "GlobalW.html", file)
        }
        
    )
    
    
}

######################################################## app
shinyApp(ui = ui, server = server)
