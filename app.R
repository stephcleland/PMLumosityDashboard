## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(ggplot2)
library(forcats)
library(tigris)
library(dplyr)
library(gridExtra)

# Data for Primary results
primary <- data.table(readRDS("./data/primary_results_full.RDS"))
cols <- c("Beta","Lower","Upper","# Users","P-Value")
primary[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
sapply(primary, class)
primary$Group = factor(primary$Group,levels=c("All","18-29","30-39","40-49","50-59","60-69","70+",
                                              "Male","Female","Habitual","Non-habitual",
                                              "Android","iPhone","iPad","Web"))
primary_palette <- data.frame("Group" = levels(primary$Group), "Color" = c("black","#4477AA","#EE6677",
                                                                           "#228833","#CCBB44","#66CCEE","#AA3377",
                                                                           "#4393C3","#D6604D","#9970AB","#5AAE61",
                                                                           "#5385BC","#99DDFF","#BBCC33","#44BB99"))

# Data for sensitivity results
sensitivity <- data.table(readRDS("./data/sensitivity_results_full.RDS"))
sensitivity <- sensitivity[!(SpecificAnalysis%in%c("1dates","1plays")),]
sensitivity[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
spline_west <- data.table(readRDS("./data/spline_results.RDS"))
spline_contig <- data.table(readRDS("./data/spline_results_nationwide.RDS"))


# Data for learning curves
learning_curves <- readRDS("./data/learning_curves.RDS")
table1_west <-  readRDS("./data/table1_west.RDS")
table1_contig <- readRDS("./data/table1_contig.RDS")

header <- dashboardHeader(
  title = span(
    "The Effects of Short-Term PM2.5 and Wildfire Smoke Exposure on Cognitive Performance in US Adults: Dashboard",
    style = "position: absolute;left: 10px; font-weight: bold"
  ),
  titleWidth = "1200px"
)

sidebar <- dashboardSidebar(
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  sidebarMenu(
    menuItem("Primary Results", tabName = "primary", icon = icon("bar-chart")),
    menuItem("Sensitivity Analyses", tabName = "sensitivity", icon = icon("table")),
    menuItem("Exposure Surfaces", tabName = "exposure_surf", icon = icon("fire")),
    menuItem("Lumosity User Data", tabName = "lumosity", icon = icon("user")),
    menuItem("About",  tabName = "about", icon = icon("question-circle"))
  ),
  tags$p("Please view in full screen",style="text-align:center;")
  
)

body <-  dashboardBody(
  tags$head(tags$style(".shiny-plot-output{height:50vh !important;}")),
  tags$head(tags$style("#lumosity_plot{height:65vh !important;}")),
  tabItems(
    tabItem(tabName = "primary",
            h2("Primary Results"),
            fluidRow(
              tabBox(width = 9,id="tabset1",
                     tabPanel("Primary",
                              plotOutput("primary_plot"),
                              htmlOutput("primary_text"),
                              htmlOutput("habit_def")
                     ),
                     tabPanel("All Daily",
                              plotOutput("daily_plot"),
                              htmlOutput("daily_text"),
                              htmlOutput("habit_def_daily")
                     ),
                     tabPanel("All Hourly",
                              plotOutput("hourly_plot"),
                              htmlOutput("hourly_text"),
                              htmlOutput("habit_def_hourly")
                     )
              ),
              box(
                title = "Options", status="info",solidHeader = TRUE,width = 3,
                radioButtons("exposure", "Exposure:",
                             c("PM2.5" = "PM2.5",
                               "Smoke" = "Smoke")),
                radioButtons("region", "Region:",
                             c("Western US" = "Western",
                               "Contiguous US" = "Contiguous")),
                radioButtons("subgroup", "Subgroup:",
                             c("All" = "All",
                               "Age Group" = "Age",
                               "Gender" = "Gender",
                               "Habitual" = "Habit")),
                checkboxGroupInput("compare", "Comparison:",
                                   c("Compare western & contiguous US" = "Yes"))
              )
            ),
            fluidRow(
              box(
                title = "Table of Results", width = 12, status = "primary",
                solidHeader = TRUE, 
                div(style = "height:500px; overflow-y: scroll;overflow-x: scroll;", 
                    DT::dataTableOutput('table'))
              )
            )
    ),
    tabItem(tabName = 'sensitivity',
            h2("Sensitivity Analyses"),
            fluidRow(
              box(title = "", status="primary",solidHeader = F,width = 9,
                  plotOutput("sensitivity_plot"),
                  htmlOutput("sensitivity_text")
              ),
              box(
                title = "Options", status="info",solidHeader = TRUE,width = 3,
                radioButtons("analyses_sens", "Sensitivity Analyses:",
                             c("Score Percentile" = "ScorePctile",
                               "Number of Lags" = "Lags",
                               "Non-Linear Relationship" = "Spline",
                               "Habitual Definition" = "Habit",
                               "User Definition (All Hourly Users)" = "AllUsers",
                               "User Definition (# Dates)" = "UserDefDate",
                               "User Definition (# Plays)" = "UserDefPlay"
                             )),
                radioButtons("exposure_sens", "Exposure:",
                             c("PM2.5" = "PM2.5",
                               "Smoke" = "Smoke")),
                radioButtons("region_sens", "Region:",
                             c("Western US" = "Western",
                               "Contiguous US" = "Contiguous"))
              )
            ),
            fluidRow(
              box(
                id = "sens_table",title = "Table of Results", width = 12, status = "primary",
                solidHeader = TRUE, 
                div(style = "height:500px; overflow-y: scroll;overflow-x: scroll;", 
                    DT::dataTableOutput('sensitivity_table'))
              )
            )
    ),
    tabItem(tabName = 'exposure_surf',
            h2("Exposure Surfaces"),
            fluidRow(
              tabBox(width = 9,id="tabset_exp",
                     tabPanel("ZIP3-Level",
                              sliderInput("dates_exp",
                                          "Select a Date:",
                                          min = as.Date("2017-01-01","%Y-%m-%d"),
                                          max = as.Date("2018-12-31","%Y-%m-%d"),
                                          value=as.Date("2017-01-01"),
                                          timeFormat="%Y-%m-%d",
                                          animate=animationOptions(interval = 1000, loop = FALSE), step=1,width='100%'),
                              div(style = "display:inline-block; float:right",  actionButton("minus", "<"),actionButton("plus", ">")),
                              imageOutput("exposure_img",inline=T),
                              htmlOutput("exposure_text")
                     ),
                     tabPanel("Original",
                              sliderInput("dates_exp2",
                                          "Select a Date:",
                                          min = as.Date("2017-01-01","%Y-%m-%d"),
                                          max = as.Date("2018-12-31","%Y-%m-%d"),
                                          value=as.Date("2017-01-01"),
                                          timeFormat="%Y-%m-%d",
                                          animate=animationOptions(interval = 1000, loop = FALSE), step=1,width='100%'),
                              div(style = "display:inline-block; float:right",  actionButton("minus2", "<"),actionButton("plus2", ">")),
                              imageOutput("exposure_img2",inline=T),
                              htmlOutput("exposure_text2")
                     ),
                     tabPanel("Monitoring Stations",
                              tabsetPanel(id="station_select",
                                          tabPanel("Daily",
                                                   imageOutput("exposure_img3",inline=T),
                                                   htmlOutput("exposure_text3")
                                          ),
                                          tabPanel("Hourly",
                                                   imageOutput("exposure_img4",inline=T),
                                                   htmlOutput("exposure_text4")
                                          )
                              )
                     )
                     
              ),
              box(
                title = "Options", status="info",solidHeader = TRUE,width = 3,
                radioButtons("exposure_exp", "Exposure:",
                             c("PM2.5" = "PM2.5",
                               "Smoke" = "Smoke"))
              )
            )
    ),
    tabItem(tabName = 'lumosity',
            h2("Lumosity User Data"),
            fluidRow(
              tabBox(width = 9,id="tabset_lum",
                     tabPanel("Learning Curve",
                              plotOutput("lumosity_plot"),
                              htmlOutput("lumosity_text")
                     ),
                     tabPanel("Characteristics Table",
                              htmlOutput("lumosity_tab"),
                              div(style = "height:700px; overflow-y: scroll;overflow-x: scroll;", 
                                  DT::dataTableOutput('lumosity_table'))
                     )
              ),
              box(
                title = "Options", status="info",solidHeader = TRUE,width = 3,
                radioButtons("subgroup_lum", "Subgroup 1:",
                             c("All" = "All",
                               "Age Group" = "Age",
                               "Gender" = "Gender",
                               "Habitual" = "Habit",
                               "Device" = "Device")),
                radioButtons("subgroup_lum2", "Subgroup 2 (Optional):",
                             c("None" = "None",
                               "Age Group" = "Age",
                               "Gender" = "Gender",
                               "Habitual" = "Habit",
                               "Device" = "Device")),
                radioButtons("region_lum", "Region:",
                             c("Western US" = "Western",
                               "Contiguous US" = "Contiguous"))
              )
            )
    ),
    tabItem(tabName = 'about',
            h2("About"),
            fluidRow(box(title = "About this Dashboard", status="primary",solidHeader = F,width = 9,
                         htmlOutput("about_text")
            ))
    )
  )
  
)

ui <- dashboardPage(shinyjs::useShinyjs(),
                    skin = "black",header=header, sidebar=sidebar, body=body
)

server <- function(input, output,session) { 
  
  ############## PRIMARY RESULTS #############################################
  observeEvent (input$exposure, {
    if(input$exposure=="Smoke"){ 
      hideTab("tabset1",target ="All Daily") 
      hideTab("tabset1",target ="All Hourly") 
      shinyjs::disable("compare")
      shinyjs::disable("region")
      updateCheckboxGroupInput(session, "compare",
                               selected = character(0)
      )
      updateRadioButtons(session,"region",selected="Western")
      
    } else {
      showTab("tabset1",target ="All Daily") 
      showTab("tabset1",target ="All Hourly") 
      shinyjs::enable("compare")
      shinyjs::enable("region")
    }
  })
  
  output$primary_plot <- renderPlot({
    plot_data <- primary[Exposure==input$exposure&(GroupType==input$subgroup|GroupType=="All")&Region==input$region,]
    if (input$exposure == "PM2.5") {
      if (input$region=="Contiguous") {
        compare_region <- "Western"
        shape=15
        compare_shape=16
      } else {
        compare_region <- "Contiguous"
        shape=16
        compare_shape=15
      }
      plot_data <- plot_data[ExposureDuration %in% c("Lag 0","7-Day Cumulative","3-Hour Max","12-Hour Max"),]
      plot_data$ExposureDuration = factor(plot_data$ExposureDuration,levels=c("3-Hour Max","12-Hour Max","Lag 0","7-Day Cumulative"))
      plot <- ggplot(data=plot_data,
                     aes(x=ExposureDuration,y=Beta,ymin=Lower,ymax=Upper,col=Group,group=Group)) +
        geom_hline(yintercept=0,lty=2) +
        geom_errorbar(position=position_dodge(width = 0.6),width=0.25,size=1) +
        geom_point(aes(shape=input$region),position=position_dodge(width = 0.6),size=4)+
        theme_bw(base_size=14)+ 
        ylab(expression("Change in Score per 10" ~ mu *"g/m"^3 * " PM"[2.5])) +
        xlab("")  + 
        scale_color_manual(values=(primary_palette[primary_palette$Group%in%unique(plot_data$Group),2])) + 
        theme( panel.grid.major.x = element_blank() ,
               panel.grid.major.y = element_line(size=.1, color="grey90" ),
               panel.grid.minor.x = element_blank()) + 
        scale_shape_manual(name = "Region",
                           values = c(shape),
                           breaks = c(input$region), guide='none') + 
        guides(color = guide_legend(override.aes = list(shape = shape)))
      if (length(input$compare > 0)) {
        if (input$compare == "Yes") {
          compare <- primary[Exposure==input$exposure&(GroupType==input$subgroup|GroupType=="All")&Region==compare_region,]
          compare <- compare[ExposureDuration %in% c("Lag 0","7-Day Cumulative","3-Hour Max","12-Hour Max"),]
          compare$ExposureDuration = factor(compare$ExposureDuration,levels=c("7-Day Cumulative","Lag 0","12-Hour Max","3-Hour Max"))
          plot <- plot +
            geom_errorbar(data=compare, mapping=aes(x=ExposureDuration,ymin=Lower,ymax=Upper,col=Group,group=Group),
                          alpha = 0.4, position=position_dodge(width = 0.6),width=0.25,size=1) +
            geom_point(data=compare,  mapping=aes(x=ExposureDuration,y=Beta,col=Group,group=Group,shape=compare_region),
                       alpha = 0.4,position=position_dodge(width = 0.6),size=4) +
            scale_shape_manual(name = "Region",
                               values = c(shape, compare_shape),
                               breaks = c(input$region,compare_region), guide="legend") +
            guides(color  = guide_legend(order = 1,override.aes = list(shape = shape)),
                   shape = guide_legend(order = 2))
        }
      }
      plot
    } else {
      if (input$region=="Western") {
        plot_data$ExposureLevel = factor(plot_data$ExposureLevel, levels=c('Light','Medium','Heavy'))
        plot_data$ExposureDuration = factor(plot_data$ExposureDuration,levels=c("Lag 0","Lag 1","1-Week Max", "2-Week Max"))
        levels(plot_data$ExposureDuration) <- c("Lag 0","Lag 1","1-Week","2-Week")
        ggplot(data=plot_data,aes(x=ExposureDuration,y=Beta,ymin=Lower,ymax=Upper,col=Group,group=Group)) +
          geom_hline(yintercept=0,lty=2) +
          geom_errorbar(position=position_dodge(width = 0.6),width=0.25,size=1) +
          geom_point(shape = 16, position=position_dodge(width = 0.6),size=4)+
          facet_grid(.~ExposureLevel,switch="x") + 
          theme_bw(base_size=14)+ 
          scale_color_manual(values=(primary_palette[primary_palette$Group%in%unique(plot_data$Group),2])) + 
          theme(strip.placement = "outside",                    
                strip.background = element_rect(fill = "white",color="white"),
                strip.text = element_text(size = 14,angle=0,face="bold"),
                panel.grid.major.x = element_blank() ,
                panel.grid.major.y = element_line(size=.1, color="grey90" ),
                panel.grid.minor.x = element_blank(),
                axis.title.y = element_text(size = 12)) + 
          ylab('Change in Score Relative to No Smoke')+
          xlab("")
      }
    }
    
  })
  
  output$daily_plot <- renderPlot({
    if (input$exposure == "PM2.5") {
      if (input$region=="Contiguous") {
        compare_region <- "Western"
        shape=15
        compare_shape=16
      } else {
        compare_region <- "Contiguous"
        shape=16
        compare_shape=15
      }
      plot_data <- primary[ExposureLevel=="Daily"&Exposure==input$exposure&(GroupType==input$subgroup|GroupType=="All")&Region==input$region,]
      plot_data$ExposureDuration = factor(plot_data$ExposureDuration,levels=c("Lag 0","Lag 1","Lag 2","Lag 3","Lag 4","Lag 5","Lag 6","7-Day Cumulative"))
      plot <- ggplot(data=plot_data,
                     aes(x=ExposureDuration,y=Beta,ymin=Lower,ymax=Upper,col=Group,group=Group)) +
        geom_hline(yintercept=0,lty=2) +
        geom_errorbar(position=position_dodge(width = 0.6),width=0.25,size=1) +
        geom_point(aes(shape=input$region),position=position_dodge(width = 0.6),size=4)+
        theme_bw(base_size=14)+ 
        ylab(expression("Change in Score per 10" ~ mu *"g/m"^3 * " PM"[2.5])) +
        xlab("")  + 
        scale_color_manual(values=(primary_palette[primary_palette$Group%in%unique(plot_data$Group),2])) + 
        theme( panel.grid.major.x = element_blank() ,
               panel.grid.major.y = element_line(size=.1, color="grey90" ),
               panel.grid.minor.x = element_blank()) + 
        scale_shape_manual(name = "Region",
                           values = c(shape),
                           breaks = c(input$region), guide='none') + 
        guides(color = guide_legend(override.aes = list(shape = shape)))
      if (length(input$compare > 0)) {
        if (input$compare == "Yes") {
          compare <- primary[ExposureLevel=="Daily"&Exposure==input$exposure&(GroupType==input$subgroup|GroupType=="All")&Region==compare_region,]
          compare$ExposureDuration = factor(compare$ExposureDuration,levels=c("Lag 0","Lag 1","Lag 2","Lag 3","Lag 4","Lag 5","Lag 6","7-Day Cumulative"))
          plot <- plot +  
            geom_errorbar(data=compare, mapping=aes(x=ExposureDuration,ymin=Lower,ymax=Upper,col=Group,group=Group), 
                          alpha = 0.4, position=position_dodge(width = 0.6),width=0.25,size=1) +
            geom_point(data=compare,  mapping=aes(x=ExposureDuration,y=Beta,col=Group,group=Group,shape=compare_region),
                       alpha = 0.4,position=position_dodge(width = 0.6),size=4) + 
            scale_shape_manual(name = "Region",
                               values = c(shape, compare_shape),
                               breaks = c(input$region,compare_region), guide="legend") + 
            guides(color  = guide_legend(order = 1,override.aes = list(shape = shape)),
                   shape = guide_legend(order = 2))
        }
      }
      plot
    } 
  })
  
  output$hourly_plot <- renderPlot({
    if (input$exposure == "PM2.5") {
      plot_data <- primary[ExposureLevel=="Hourly"&Exposure==input$exposure&(GroupType==input$subgroup|GroupType=="All")&Region==input$region,]
      plot_data$ExposureDuration = factor(plot_data$ExposureDuration,levels=c("3-Hour Max","6-Hour Max", "12-Hour Max"))
      if (input$region=="Contiguous") {
        compare_region <- "Western"
        shape=15
        compare_shape=16
      } else {
        compare_region <- "Contiguous"
        shape=16
        compare_shape=15
      }
      plot<- ggplot(data=plot_data,
                    aes(x=ExposureDuration,y=Beta,ymin=Lower,ymax=Upper,col=Group,group=Group)) +
        geom_hline(yintercept=0,lty=2) +
        geom_errorbar(position=position_dodge(width = 0.6),width=0.25,size=1) +
        geom_point(aes(shape=input$region),position=position_dodge(width = 0.6),size=4)+
        theme_bw(base_size=14)+ 
        ylab(expression("Change in Score per 10" ~ mu *"g/m"^3 * " PM"[2.5])) +
        xlab("")  + 
        scale_color_manual(values=(primary_palette[primary_palette$Group%in%unique(plot_data$Group),2])) + 
        theme(panel.grid.major.x = element_blank() ,
              panel.grid.major.y = element_line(size=.1, color="grey90" ),
              panel.grid.minor.x = element_blank())+ 
        scale_shape_manual(name = "Region",
                           values = c(shape),
                           breaks = c(input$region), guide='none') + 
        guides(color = guide_legend(override.aes = list(shape = shape)))
      if (length(input$compare > 0)) {
        if (input$compare == "Yes") {
          compare <- primary[ExposureLevel=="Hourly"&Exposure==input$exposure&(GroupType==input$subgroup|GroupType=="All")&Region==compare_region,]
          compare$ExposureDuration = factor(compare$ExposureDuration,levels=c("12-Hour Max","6-Hour Max", "3-Hour Max"))
          plot <- plot +  
            geom_errorbar(data=compare, mapping=aes(x=ExposureDuration,ymin=Lower,ymax=Upper,col=Group,group=Group), 
                          alpha = 0.4, position=position_dodge(width = 0.6),width=0.25,size=1) +
            geom_point(data=compare,  mapping=aes(x=ExposureDuration,y=Beta,col=Group,group=Group,shape=compare_region),
                       alpha = 0.4,position=position_dodge(width = 0.6),size=4) + 
            scale_shape_manual(name = "Region",
                               values = c(shape, compare_shape),
                               breaks = c(input$region,compare_region), guide="legend") + 
            guides(color  = guide_legend(order = 1,override.aes = list(shape = shape)),
                   shape = guide_legend(order = 2))
        }
      }
      plot
    } 
  })
  
  output$primary_text <- renderText({
    if (input$subgroup == "Habit") {
      subgroup <- "habitual behavior"
    } else {
      subgroup <- input$subgroup
    }

    if (input$region == "Contiguous" & input$exposure == "Smoke") {
      paste("Smoke results for the contiguous US are unavailable")
    } else {
      if (input$exposure == "Smoke") {
        exposure_text = tolower(input$exposure)
        if (input$subgroup=="All") {
          paste("<b>Figure.</b>","Change in attention score due to light, medium, or heavy density smoke, relative to no smoke, for all", tolower(input$region), "US users.")
        } else {
          paste("<b>Figure.</b>","Change in attention score due to light, medium, or heavy density smoke, relative to no smoke, for", tolower(input$region),"US users by", tolower(subgroup))
        }  
      } else {
        exposure_text = input$exposure
        if (input$subgroup=="All") {
          paste("<b>Figure.</b>","Change in attention score per 10 ug/m3 change in PM2.5 for all", tolower(input$region), "US users.")
        } else {
          paste("<b>Figure.</b>","Change in attention score per 10 ug/m3 change in PM2.5 for", tolower(input$region),"US users by", tolower(subgroup))
        }  
      } 
    }
  })
  
  output$daily_text <- renderText({
    if (input$subgroup == "Habit") {
      subgroup <- "habitual behavior"
    } else {
      subgroup <- input$subgroup
    }
    if (input$exposure == "Smoke") {
      exposure_text = tolower(input$exposure)
      if (input$subgroup=="All") {
        paste("<b>Figure.</b>","Change in attention score due to light, medium, or heavy density smoke, relative to no smoke, for all", tolower(input$region), "US users.")
      } else {
        paste("<b>Figure.</b>","Change in attention score due to light, medium, or heavy density smoke, relative to no smoke, for", tolower(input$region),"US users by", tolower(subgroup))
      }  
    } else {
      exposure_text = input$exposure
      if (input$subgroup=="All") {
        paste("<b>Figure.</b>","Change in attention score per 10 ug/m3 change in PM2.5 for all", tolower(input$region), "US users.")
      } else {
        paste("<b>Figure.</b>","Change in attention score per 10 ug/m3 change in PM2.5 for", tolower(input$region),"US users by", tolower(subgroup))
      }  
    } 
  })
  
  output$hourly_text <- renderText({
    if (input$subgroup == "Habit") {
      subgroup <- "habitual behavior"
    } else {
      subgroup <- input$subgroup
    }
    if (input$exposure == "Smoke") {
      exposure_text = tolower(input$exposure)
      if (input$subgroup=="All") {
        paste("<b>Figure.</b>","Change in attention score due to light, medium, or heavy density smoke, relative to no smoke, for all", tolower(input$region), "US users.")
      } else {
        paste("<b>Figure.</b>","Change in attention score due to light, medium, or heavy density smoke, relative to no smoke, for", tolower(input$region),"US users by", tolower(subgroup))
      }  
    } else {
      exposure_text = input$exposure
      if (input$subgroup=="All") {
        paste("<b>Figure.</b>","Change in attention score per 10 ug/m3 change in PM2.5 for all", tolower(input$region), "US users.")
      } else {
        paste("<b>Figure.</b>","Change in attention score per 10 ug/m3 change in PM2.5 for", tolower(input$region),"US users by", tolower(subgroup))
      }  
    } 
  })
  
  output$habit_def <- renderText({
    if (input$subgroup=="Habit") {
      paste("<br><i>Habitual users:</i>","median time between plays <= 7 days and std. dev. of time of play <= 2 hours")
    } else {
      paste("")
    }  
  })
  
  output$habit_def_hourly <- renderText({
    if (input$subgroup=="Habit") {
      paste("<br><i>Habitual users:</i>","median time between plays <= 7 days and std. dev. of time of play <= 2 hours")
    } else {
      paste("")
    }  
  })
  
  output$habit_def_daily <- renderText({
    if (input$subgroup=="Habit") {
      paste("<br><i>Habitual users:</i>","median time between plays <= 7 days and std. dev. of time of play <= 2 hours")
    } else {
      paste("")
    }  
  })
  
  output$table <- renderDT({
    
    if (length(input$compare > 0)) {
      if (input$compare == "Yes") {
        table_data <- primary[Exposure==input$exposure&(GroupType==input$subgroup|GroupType=="All"),]
      } 
    } else {
      table_data <- primary[Exposure==input$exposure&(GroupType==input$subgroup|GroupType=="All")&Region==input$region,]
    }
    table_data$Beta <- as.numeric(sprintf("%.2f",table_data$Beta))
    if (input$exposure == "PM2.5") {
      if (input$tabset1=="Primary") {
        table_data <- table_data[ExposureDuration %in% c("7-Day Cumulative","Lag 0","12-Hour Max","3-Hour Max")]
      } else if (input$tabset1 == "All Daily") {
        table_data <- table_data[ExposureDuration %in% c("Lag 0","Lag 1","Lag 2","Lag 3","Lag 4","Lag 5","Lag 6","7-Day Cumulative")]
      } else if (input$tabset1 == "All Hourly") {
        table_data <- table_data[ExposureDuration %in% c("12-Hour Max","6-Hour Max", "3-Hour Max")]
      }
      names(table_data)[names(table_data) == "ExposureDuration"] <- "Exposure Duration"
      datatable(table_data[,c("Exposure","Region","Exposure Duration","Group","Beta","95% CI","P-Value","# Users")], selection = 'none',options = list(paging = FALSE)) 
    } else {
      names(table_data)[names(table_data) == "ExposureDuration"] <- "Exposure Duration"
      names(table_data)[names(table_data) == "ExposureLevel"] <- "Exposure Level"
      datatable(table_data[,c("Exposure","Region","Exposure Duration","Exposure Level","Group","Beta","95% CI","P-Value","# Users")], 
                selection = 'single',options = list(paging = FALSE)) 
    }
  })
  
  ##################### SENSITIVITY RESULTS #########################################
  observeEvent (input$analyses_sens, {
    if(input$analyses_sens %in% c("ScorePctile","UserDefPlay","UserDefDate","Habit")){ 
      shinyjs::enable("exposure_sens")
    } else {
      shinyjs::disable("exposure_sens")
      updateRadioButtons(session, "exposure_sens", selected = "PM2.5")
    }
    if (input$analyses_sens == "Spline") {
      shinyjs::hide("sens_table")
    } else {
      shinyjs::show("sens_table")
    }
  })
  
  observeEvent (input$exposure_sens, {
    if(input$exposure_sens == "PM2.5"){ 
      shinyjs::enable("region_sens")
    } else {
      shinyjs::disable("region_sens")
      updateRadioButtons(session, "region_sens", selected = "Western")
    }
  })
  
  output$sensitivity_text <- renderText({
    if (input$exposure_sens == "Smoke") {
      exposure_text = tolower(input$exposure_sens)
    } else {
      exposure_text = input$exposure_sens
    }
    text_part1 <- paste("<b>Figure.</b>","Sensitivity of observed associations between",exposure_text,"and attention score in the",tolower(input$region_sens),
                        "US")
    if (input$analyses_sens == "AllUsers") {
      text_part2 <- paste("to the user inclusion criteria. <br><br>",
                          "For this sensitivity analysis, we compared the primary inclusion criteria for users, those who completed 20 plays on 20 unique dates,",
                          "to a looser inclusion criteria, users who completed 20 plays on 20 unique timestamps.",
                          "This change in inclusion criteria only applies to the hourly PM2.5 analyses, shown above."
      )
    } else if (input$analyses_sens == "UserDefDate") {
      text_part2 <- paste("to the user inclusion criteria.<br><br>",
                          "The primary inclusion criteria for users is those who completed 20 plays on 20 unique dates.",
                          "For this sensitivity analysis, we considered looser inclusion criteria, including users who completed 20 plays on less than 20 unique dates.",
                          "We only included a users first play on any given date (e.g., if play 4-6 occurred on one date, only play 4 is included in the analysis)."
      )
    } else if (input$analyses_sens == "UserDefPlay") {
      text_part2 <- paste("to the user inclusion criteria. <br><br>",
                          "The primary inclusion criteria for users is those who completed 20 plays on 20 unique dates.",
                          "For this sensitivity analysis, we considered looser inclusion criteria, including users who completed less than 20 plays.",
                          "To meet the criteria, users still must have played each game on a unique date (e.g., if a user has 18 plays, they must have completed them on 18 unique dates)."
      )
    } else if (input$analyses_sens == "Lags") {
      text_part2 <- paste("to the number of lags included in distributed lag model. <br><br>",
                          "The primary analyses for daily PM2.5 included 7 lags of population-weighted PM2.5.",
                          "For this sensitivity analysis, we evaluated the impact of including 1-6 lags, instead of 7, on the observed effect at lag 0 and the n-day cumulative effect.",
                          "The cumulative effect is calculated from all lags included in the model (e.g., if 5 lags are included, the value shown is the 5-day cumulative effect)."
      )
    } else if (input$analyses_sens == "Habit") {
      text_part2 <- paste("to the definition of habitual users. <br><br>",
                          "The primary definition of a habitual user was users whos median time between plays was <= 7 days and  standard deviation of time of play was <= 2 hours.",
                          "For this sensitivity analysis, we considered alternative definitions of habitual users, changing either the cutoff for the median time between plays (14 days instead of 7)",
                          "or the cutoff for the standard deviation of the time of day (3 or 4 hours instead of 2)."
      )
    } else if (input$analyses_sens == "Spline") {
      text_part1 <-  paste("<b>Figure.</b>","Non-linear association between daily PM2.5 and attention score in the",tolower(input$region_sens),"US.<br><br>")
      text_part2<-paste("This sensitivity analysis considered the possibility of a non-linear association between daily PM2.5 and game score. Cubic splines were fit to all 7 lags and",
                        "results are shown for lag 0 and the 7-day cumulative effect."
      )
    } else if (input$analyses_sens == "ScorePctile") {
      text_part2 <- paste("to the type of score used. <br><br>",
                          "For this sensitivity analysis, we investigated the impact of using the percentile score, a normalized score, as the measure of Lost in Migration performance",
                          "instead of the raw score. "
      )
    } else {
      text_part2<-""      
    }
    paste(text_part1,text_part2)
  })
  
  output$sensitivity_plot <- renderPlot ({
    if (input$exposure_sens =="Smoke" & input$region_sens == "Contiguous") {
    } else {
      if (input$analyses_sens == "Habit") {
        plot_data <- sensitivity[grepl(input$analyses_sens, Sensitivity, fixed=TRUE)&Exposure==input$exposure_sens&Region==input$region_sens,]
        
      } else {
        plot_data <- sensitivity[Sensitivity==input$analyses_sens&Exposure==input$exposure_sens&Region==input$region_sens,]
      }
      if (input$exposure_sens=="PM2.5") { # PM2.5
        
        if (input$analyses_sens != "Spline") {
          
          if (input$region_sens=="Contiguous") {
            shape_reg=15
          } else {
            shape_reg=16
          }
          
          if (input$analyses_sens == "AllUsers") {
            plot_data <- plot_data[ExposureDuration %in% c("3-Hour Max","6-Hour Max","12-Hour Max"),]
            plot_data$ExposureDuration = factor(plot_data$ExposureDuration,levels=c("3-Hour Max","6-Hour Max", "12-Hour Max"))        
          } else {
            plot_data <- plot_data[ExposureDuration %in% c("3-Hour Max","12-Hour Max","Lag 0","7-Day Cumulative"),]
            plot_data$ExposureDuration = factor(plot_data$ExposureDuration,levels=c("3-Hour Max","12-Hour Max","Lag 0","7-Day Cumulative"))
            
            if (input$analyses_sens == "Lags") {
              plot_data <- plot_data[SpecificAnalysis != "lags7",]
              plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("lags1","lags2","lags3","lags4","lags5","lags6"))
            }
            if (input$analyses_sens == "UserDefDate") {
              plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("5dates","10dates","15dates"))
            }
            if (input$analyses_sens == "UserDefPlay") {
              if (input$region_sens == "Contiguous") {
                plot_data <- plot_data[plot_data$SpecificAnalysis!="5plays",]
                plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("6plays","10plays","15plays"))
              } else {
                plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("5plays","10plays","15plays"))
              }
            }
            if (input$analyses_sens == "Habit") {
              plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("7days_2hours",
                                                                                      "7days_3hours",
                                                                                      "7days_4hours",
                                                                                      '14days_2hours'))
            }
            
          }
          
          if (input$analyses_sens == "Habit") {
            plot_data[Sensitivity == "HabitDefHabitual",]$Sensitivity <- "Habitual"
            plot_data[Sensitivity == "HabitDefNonhabitual",]$Sensitivity <- "Non-habitual"
            compare_data <- primary[GroupType=="Habit"&Exposure==input$exposure_sens&Region==input$region_sens,]
            compare_data <- compare_data %>% 
              rename(
                SpecificAnalysis = GroupType,
                Sensitivity = Group
              )
            compare_data$SpecificAnalysis <- rep("7days_2hours",nrow(compare_data))
            compare_data <- compare_data[ExposureDuration %in% c("3-Hour Max","12-Hour Max","Lag 0","7-Day Cumulative"),]
            compare_data$ExposureDuration = factor(compare_data$ExposureDuration,levels=c("7-Day Cumulative","Lag 0","12-Hour Max","3-Hour Max"))
            
          } else {
            compare_data <- primary[GroupType=="All"&Exposure==input$exposure_sens&Region==input$region_sens,]
            if (input$analyses_sens == "AllUsers") {
              compare_data <- compare_data[ExposureDuration %in% c("3-Hour Max","6-Hour Max","12-Hour Max"),]
              compare_data$ExposureDuration = factor(compare_data$ExposureDuration,levels=c("12-Hour Max","6-Hour Max", "3-Hour Max"))        
            } else if (input$analyses_sens == "Lags" || input$analyses_sens == "Learning") {
              compare_data <- compare_data[ExposureDuration %in% c("Lag 0","7-Day Cumulative"),]
              compare_data$ExposureDuration = factor(compare_data$ExposureDuration,levels=c("7-Day Cumulative","Lag 0"))        
            } else {
              compare_data <- compare_data[ExposureDuration %in% c("3-Hour Max","12-Hour Max","Lag 0","7-Day Cumulative"),]
              compare_data$ExposureDuration = factor(compare_data$ExposureDuration,levels=c("7-Day Cumulative","Lag 0","12-Hour Max","3-Hour Max"))
            }
            
            compare_data$GroupType <- rep("Primary Result",nrow(compare_data))
            compare_data$Group <- rep("All",nrow(compare_data))
            compare_data <- compare_data %>% 
              rename(
                Sensitivity = GroupType,
                SpecificAnalysis = Group
              )
            
          }
          
          plot_data <- rbind(plot_data,compare_data)
          
          if (input$analyses_sens == "ScorePctile") {
            plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("Score Pctile","All"),labels=c("Percentile Score","Raw Score"))
          } else if (input$analyses_sens == "Learning"){
            plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("Learning","Score","All"))
            plot_data$Sensitivity = factor(plot_data$Sensitivity,levels=c("Learning","Primary Result"),labels = c("Model with PM-Nth Play Interaction","Model w/o PM-Nth Play Interaction"))
          }
          
          
          plot <- ggplot(data=plot_data,
                         aes(x=ExposureDuration,y=Beta,ymin=Lower,ymax=Upper,col=SpecificAnalysis,group=SpecificAnalysis)) +
            geom_hline(yintercept=0,lty=2) +
            geom_errorbar(position=position_dodge(width = 0.6),width=0.25,size=1) +
            geom_point(position=position_dodge(width = 0.6),size=4,shape=shape_reg)+
            theme_bw(base_size=14)+ 
            ylab(expression("Change in Score per 10" ~ mu *"g/m"^3 * " PM"[2.5])) +
            xlab("")  + 
            theme( panel.grid.major.x = element_blank() ,
                   panel.grid.major.y = element_line(size=.1, color="grey90" ),
                   panel.grid.minor.x = element_blank())
          
          if (input$analyses_sens == "AllUsers") {
            plot + scale_color_manual(name = "User Type", labels = c("All Hourly Users", "All Daily Users"),
                                      values = c("#BB5566","black"))
          } else if (input$analyses_sens == "Habit") {
            ggplot(data=plot_data,
                   aes(x=ExposureDuration,y=Beta,ymin=Lower,ymax=Upper,
                       col=SpecificAnalysis,group=SpecificAnalysis,alpha=Sensitivity,shape=Sensitivity)) +
              geom_hline(yintercept=0,lty=2) +
              geom_errorbar(position=position_dodge(width = 0.6),width=0.25,size=1) +
              geom_point(position=position_dodge(width = 0.6),size=4)+
              theme_bw(base_size=14)+ 
              ylab(expression("Change in Score per 10" ~ mu *"g/m"^3 * " PM"[2.5])) +
              xlab("")  + 
              theme( panel.grid.major.x = element_blank() ,
                     panel.grid.major.y = element_line(size=.1, color="grey90" ),
                     panel.grid.minor.x = element_blank()) + 
              scale_color_manual(name = "Habitual Definition",labels = c("7 days, 2 hours","7 days, 3 hours",
                                                                         "7 days, 4 hours","14 days, 2 hours"),
                                 values = c("black","#8A9045FF","#155F83FF","#C16622FF")) +
              scale_alpha_manual(name="User Type",values=c(1,0.5)) +
              scale_shape_manual(name="User Type",values=c(16,17))
            
          } else if (input$analyses_sens == "UserDefDate") {
            plot + scale_color_manual(name = "# Unique Dates", labels = c("5+ Dates","10+ Dates","15+ Dates","20 Dates"),
                                      values = c("#364B9A","#FDD081","#E34D34","black"))
          } else if (input$analyses_sens == "UserDefPlay") {
            if (input$region_sens == "Contiguous") {
              plot + scale_color_manual(name = "# Plays", labels = c("6+ Plays","10+ Plays","15+ Plays","20 Plays"),
                                        values = c("#117733","#88CCEE","#AA4499","black"))
            } else {
              plot + scale_color_manual(name = "# Plays", labels = c("5+ Plays","10+ Plays","15+ Plays","20 Plays"),
                                        values = c("#117733","#88CCEE","#AA4499","black"))
            }
          } else if (input$analyses_sens == "Lags") {
            plot + scale_color_manual(name = "# Lags Included", labels = c("1 Lag","2 Lags","3 Lags","4 Lags", "5 Lags","6 Lags","7 Lags"),
                                      values = c("#AE76A3","#5289C7","#90C987","#F1932D","#DC050C","#FFAABB","black")) +
              scale_x_discrete(labels=c("Lag 0" = "Lag 0", "7-Day Cumulative" = "N-Day Cumulative"))
          } else if (input$analyses_sens == "ScorePctile") {
            plot + 
              facet_wrap(.~SpecificAnalysis,scales="free") + 
              scale_color_manual(name = "Score Type", labels = c("Percentile Score", "Raw Score"),
                                 values = c("#009988","black")) + 
              theme(
                strip.placement = "outside",                    
                strip.background = element_rect(fill = "white",color="black"),
                strip.text.y.left = element_text(size = 12,angle=0),
                panel.grid.major.x = element_blank() ,
                panel.grid.major.y = element_line(size=.1, color="grey90" ),
                panel.grid.minor.x = element_blank(),
                axis.text.x=element_text(angle=30,hjust=1)
              )
          } else if (input$analyses_sens == "Learning") {
            plot + facet_wrap(.~Sensitivity, scales="free") + 
              theme(
                strip.placement = "outside",                    
                strip.background = element_rect(fill = "white",color="black"),
                strip.text.y.left = element_text(size = 12,angle=0),
                panel.grid.major.x = element_blank() ,
                panel.grid.major.y = element_line(size=.1, color="grey90" ),
                panel.grid.minor.x = element_blank()) + 
              scale_color_manual(name = "Metric", labels = c("Change in Learning", "Change in Score","Change in Score"),
                                 values = c("#762A83","#1B7837","black")) + 
              ylab(expression("Change per 10" ~ mu *"g/m"^3 * " PM"[2.5])) 
          } else {
            plot
          }
        } else { # SPLINE
          if (input$region_sens=="Contiguous") {
            plot_data=spline_contig
            ylim <- c(-2500,2500)
          } else {
            plot_data=spline_west
            ylim <- c(-4000, 2000)
          }
          
          lag0<- ggplot(plot_data) + geom_line(aes(x=PM, y=lag0),size=1,color="#004488") +
            geom_ribbon(aes(x=PM,ymin=low_lag0,ymax=high_lag0),alpha=0.2,fill="#004488") +
            scale_colour_brewer(palette="Set1") + xlab(expression("PM"[2.5]*" ("~mu *"g/m"^3*")")) + ylab("Change in Score") +
            geom_hline(yintercept=0)  + theme_bw(base_size = 12) + 
            scale_x_continuous(limits=c(0, 30), expand = c(0, 0)) +
            scale_y_continuous(limits=ylim) + ggtitle("Lag 0")
          
          cumu <- ggplot(plot_data) + geom_line(aes(x=PM, y=t.allfit),size=1,color="#882255") +
            geom_ribbon(aes(x=PM,ymin=t.alllow,ymax=t.allhigh),alpha=0.2,fill="#882255") +
            scale_colour_brewer(palette="Set1") + xlab(expression("PM"[2.5]*" ("~mu *"g/m"^3*")")) + ylab("Change in Score") +
            geom_hline(yintercept=0)  + theme_bw(base_size=14) + 
            scale_x_continuous(limits=c(0, 30), expand = c(0, 0)) +
            scale_y_continuous(limits=ylim) + ggtitle("7-Day Cumulative")
          
          grid.arrange(lag0,cumu,ncol=2)
          
          
        }
      } else { # SMOKE
        plot_data$ExposureLevel = factor(plot_data$ExposureLevel, levels=c('Light','Medium','Heavy'))
        plot_data$ExposureDuration = factor(plot_data$ExposureDuration,levels=c("Lag 0","Lag 1","1-Week Max", "2-Week Max"))
        #levels(plot_data$ExposureDuration) <- c("Lag 0","Lag 1","1-Week","2-Week")
        if (input$analyses_sens == "UserDefDate") {
          plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("5dates","10dates","15dates"))
        } else if (input$analyses_sens == "UserDefPlay") {
          if (input$region_sens == "Contiguous") {
            plot_data <- plot_data[plot_data$SpecificAnalysis!="5plays",]
            plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("6plays","10plays","15plays"))
          } else {
            plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("5plays","10plays","15plays"))
          }
        } else if (input$analyses_sens == "Habit") {
          plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("7days_2hours",
                                                                                  "7days_3hours",
                                                                                  "7days_4hours",
                                                                                  '14days_2hours'))
        }
        
        if (input$analyses_sens == "Habit") {
          plot_data[Sensitivity == "HabitDefHabitual",]$Sensitivity <- "Habitual"
          plot_data[Sensitivity == "HabitDefNonhabitual",]$Sensitivity <- "Non-habitual"
          plot_data <- plot_data[SpecificAnalysis != "7days_2hours",]
          compare_data <- primary[GroupType=="Habit"&Exposure==input$exposure_sens&Region==input$region_sens,]
          compare_data <- compare_data %>% 
            rename(
              SpecificAnalysis = GroupType,
              Sensitivity = Group
            )
          compare_data$SpecificAnalysis <- rep("7days_2hours",nrow(compare_data))
          
        } else {
          compare_data <- primary[GroupType=="All"&Exposure==input$exposure_sens&Region==input$region_sens,]
          
          compare_data$GroupType <- rep("Primary Result",nrow(compare_data))
          compare_data$Group <- rep("All",nrow(compare_data))
          
          compare_data <- compare_data %>% 
            rename(
              Sensitivity = GroupType,
              SpecificAnalysis = Group
            )
        }
        
        
        plot_data <- rbind(plot_data,compare_data)
        
        if (input$analyses_sens == "ScorePctile") {
          plot_data$SpecificAnalysis = factor(plot_data$SpecificAnalysis,levels=c("Score Pctile","All"),labels=c("Pctile Score","Raw Score"))
        }
        
        plot <- ggplot(data=plot_data,aes(x=ExposureDuration,y=Beta,ymin=Lower,ymax=Upper,col=SpecificAnalysis,group=SpecificAnalysis)) +
          geom_hline(yintercept=0,lty=2) +
          geom_errorbar(position=position_dodge(width = 0.6),width=0.25,size=1) +
          geom_point(shape = 16, position=position_dodge(width = 0.6),size=4)+
          facet_grid(.~ExposureLevel,switch="x") + 
          theme_bw(base_size=14)+ 
          theme(strip.placement = "outside",                    
                strip.background = element_rect(fill = "white",color="white"),
                strip.text = element_text(size = 14,angle=0,face="bold"),
                panel.grid.major.x = element_blank() ,
                panel.grid.major.y = element_line(size=.1, color="grey90" ),
                panel.grid.minor.x = element_blank(),
                axis.title.y = element_text(size = 12)) + 
          ylab('Change in Score Relative to No Smoke')+
          xlab("")
        
        if (input$analyses_sens == "UserDefDate") {
          plot + scale_color_manual(name = "# Unique Dates", labels = c("5+ Dates","10+ Dates","15+ Dates","20 Dates"),
                                    values = c("#364B9A","#FDD081","#E34D34","black")) +
            theme(axis.text.x=element_text(angle=30,hjust=1))
        } else if (input$analyses_sens == "UserDefPlay") {
          if (input$region_sens == "Contiguous") {
            plot + scale_color_manual(name = "# Unique Plays", labels = c("6+ Plays","10+ Plays","15+ Plays","20 Plays"),
                                      values = c("#117733","#88CCEE","#AA4499","black")) +
              theme(axis.text.x=element_text(angle=30,hjust=1))
          } else {
            plot + scale_color_manual(name = "# Unique Plays", labels = c("5+ Plays","10+ Plays","15+ Plays","20 Plays"),
                                      values = c("#117733","#88CCEE","#AA4499","black")) +
              theme(axis.text.x=element_text(angle=30,hjust=1))
          }
        } else if (input$analyses_sens == "ScorePctile") {
          plot + 
            facet_grid(SpecificAnalysis~ExposureLevel,scales="free") + 
            scale_color_manual(name = "Score Type", labels = c("Percentile Score", "Raw Score"),
                               values = c("#009988","black")) + 
            theme(
              strip.placement = "outside",                    
              strip.background = element_rect(fill = "white",color="black"),
              strip.text.y.left = element_text(size = 12,angle=0),
              panel.grid.major.x = element_blank() ,
              panel.grid.major.y = element_line(size=.1, color="grey90" ),
              panel.grid.minor.x = element_blank(),
              axis.text.x=element_text(angle=30,hjust=1)
            )
        } else if (input$analyses_sens == "Habit") {
          
          ggplot(data=plot_data,aes(x=ExposureDuration,y=Beta,ymin=Lower,ymax=Upper,
                                    col=SpecificAnalysis,group=SpecificAnalysis,alpha=Sensitivity,shape=Sensitivity)) +
            geom_hline(yintercept=0,lty=2) +
            geom_errorbar(position=position_dodge(width = 0.6),width=0.25,size=1) +
            geom_point(position=position_dodge(width = 0.6),size=4)+
            facet_grid(.~ExposureLevel,switch="x") + 
            theme_bw(base_size=14)+ 
            theme(strip.placement = "outside",                    
                  strip.background = element_rect(fill = "white",color="white"),
                  strip.text = element_text(size = 14,angle=0,face="bold"),
                  panel.grid.major.x = element_blank() ,
                  panel.grid.major.y = element_line(size=.1, color="grey90" ),
                  panel.grid.minor.x = element_blank(),
                  axis.title.y = element_text(size = 12),
                  axis.text.x=element_text(angle=30,hjust=1)) + 
            ylab('Change in Score Relative to No Smoke')+
            xlab("") + 
            scale_color_manual(name = "Habitual Definition",labels = c("7 days, 2 hours","7 days, 3 hours",
                                                                       "7 days, 4 hours","14 days, 2 hours"),
                               values = c("black","#8A9045FF","#155F83FF","#C16622FF")) +
            scale_alpha_manual(name="User Type",values=c(1,0.5)) +
            scale_shape_manual(name="User Type",values=c(16,17))
          
        } else {
          plot
        }
      }
    }
  })
  
  output$sensitivity_table <- renderDT({
    if (input$analyses_sens != "Spline") {
      if (input$analyses_sens == "Habit") {
        table_data <- sensitivity[grepl(input$analyses_sens, Sensitivity, fixed=TRUE)&Exposure==input$exposure_sens&Region==input$region_sens,]
        compare_data <- primary[GroupType=="Habit"&Exposure==input$exposure_sens&Region==input$region_sens,]
        
      } else {
        table_data <- sensitivity[Sensitivity==input$analyses_sens&Exposure==input$exposure_sens&Region==input$region_sens,]
        compare_data <- primary[GroupType=="All"&Exposure==input$exposure_sens&Region==input$region_sens,]
        
      }
      
      if (input$exposure_sens == "PM2.5") {
        cols <- c("Exposure","Analysis","Region","Exposure Duration","Beta","95% CI","P-Value","# Users")
        if (input$analyses_sens == "AllUsers") {
          table_data <- table_data[ExposureDuration %in% c("3-Hour Max","6-Hour Max","12-Hour Max"),]
        } else if (input$analyses_sens == "Habit") {
          cols <- c("Exposure","Analysis","Habitual Definition","Region","Exposure Duration","Beta","95% CI","P-Value","# Users")
          
        } else {
          table_data <- table_data[ExposureDuration %in% c("3-Hour Max","12-Hour Max","Lag 0","7-Day Cumulative"),]
          
          if (input$analyses_sens == "Lags") {
            table_data <- table_data[SpecificAnalysis != "lags7",]
          }
        }
        if (input$analyses_sens == "AllUsers") {
          compare_data <- compare_data[ExposureDuration %in% c("3-Hour Max","6-Hour Max","12-Hour Max"),]
        } else if (input$analyses_sens == "Lags" || input$analyses_sens == "Learning") {
          compare_data <- compare_data[ExposureDuration %in% c("Lag 0","7-Day Cumulative"),]
        } else {
          compare_data <- compare_data[ExposureDuration %in% c("3-Hour Max","12-Hour Max","Lag 0","7-Day Cumulative"),]
        }
        
      } else {
        if (input$analyses_sens == "Habit") {
          cols <- c("Exposure","Analysis","Habitual Definition","Region","Exposure Duration","Exposure Level","Beta","95% CI","P-Value","# Users")
        } else {
          cols <- c("Exposure","Analysis","Region","Exposure Duration","Exposure Level","Beta","95% CI","P-Value","# Users")
        }
      }
      compare_data <- compare_data %>% 
        rename(
          Sensitivity = GroupType,
          SpecificAnalysis = Group
        )
      table_data <- rbind(table_data,compare_data)
      
      table_data$Analysis <- rep("Primary",nrow(table_data))
      
      if (input$analyses_sens != "Habit") {
        table_data[SpecificAnalysis != "All"]$Analysis <- rep("Sensitivity",nrow(table_data[SpecificAnalysis != "All",]))
      } else {
        table_data[!(SpecificAnalysis %in% c("Habitual","Non-habitual"))]$Analysis <- rep("Sensitivity",nrow(table_data[!(SpecificAnalysis %in% c("Habitual","Non-habitual")),]))
      }
      
      if (input$analyses_sens == "ScorePctile") {
        table_data[Analysis=="Primary"]$SpecificAnalysis <- rep("Raw Score",nrow(table_data[Analysis=="Primary",]))
        table_data[Analysis=="Sensitivity"]$SpecificAnalysis <- rep("Percentile Score",nrow(table_data[Analysis=="Sensitivity",]))
        names(table_data)[names(table_data) == "SpecificAnalysis"] <- "Score Type"
        cols <- c(cols[1:2],"Score Type",cols[3:length(cols)])
      } else if (input$analyses_sens == "AllUsers") {
        table_data[Analysis=="Primary"]$SpecificAnalysis <- rep("All Daily Users",nrow(table_data[Analysis=="Primary",]))
        table_data[Analysis=="Sensitivity"]$SpecificAnalysis <- rep("All Hourly Users",nrow(table_data[Analysis=="Sensitivity",]))
        names(table_data)[names(table_data) == "SpecificAnalysis"] <- "User Definition"
        cols <- c(cols[1:2],"User Definition",cols[3:length(cols)])
      } else if (input$analyses_sens == "UserDefDate") {
        table_data <- table_data[SpecificAnalysis!="1dates",]
        table_data[Analysis=="Primary"]$SpecificAnalysis <- rep("20 Dates",nrow(table_data[Analysis=="Primary",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="5dates"]$SpecificAnalysis <- rep("5+ Dates",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="5dates",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="10dates"]$SpecificAnalysis <- rep("10+ Dates",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="10dates",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="15dates"]$SpecificAnalysis <- rep("15+ Dates",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="15dates",]))
        names(table_data)[names(table_data) == "SpecificAnalysis"] <- "User Definition"
        cols <- c(cols[1:2],"User Definition",cols[3:length(cols)])
      } else if (input$analyses_sens == "UserDefPlay") {
        table_data <- table_data[SpecificAnalysis!="1plays",]
        if (input$region_sens == "Contiguous") {
          
          table_data <- table_data[SpecificAnalysis!="5plays",]
          table_data[Analysis=="Sensitivity"&SpecificAnalysis=="6plays"]$SpecificAnalysis <- rep("6+ Plays",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="6plays",]))
          print(table_data)
          
        } else {
          table_data[Analysis=="Sensitivity"&SpecificAnalysis=="5plays"]$SpecificAnalysis <- rep("5+ Plays",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="5plays",]))
        }
        table_data[Analysis=="Primary"]$SpecificAnalysis <- rep("20 Plays",nrow(table_data[Analysis=="Primary",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="10plays"]$SpecificAnalysis <- rep("10+ Plays",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="10plays",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="15plays"]$SpecificAnalysis <- rep("15+ Plays",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="15plays",]))
        names(table_data)[names(table_data) == "SpecificAnalysis"] <- "User Definition"
        cols <- c(cols[1:2],"User Definition",cols[3:length(cols)])
      } else if (input$analyses_sens == "Lags") {
        table_data[Analysis=="Primary"]$SpecificAnalysis <- rep("7 Lags",nrow(table_data[Analysis=="Primary",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags1"]$SpecificAnalysis <- rep("1 Lag",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags1",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags2"]$SpecificAnalysis <- rep("2 Lags",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags2",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags3"]$SpecificAnalysis <- rep("3 Lags",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags3",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags4"]$SpecificAnalysis <- rep("4 Lags",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags4",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags5"]$SpecificAnalysis <- rep("5 Lags",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags5",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags6"]$SpecificAnalysis <- rep("6 Lags",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="lags6",]))
        names(table_data)[names(table_data) == "SpecificAnalysis"] <- "# of Lags"
        cols <- c(cols[1:2],"# of Lags",cols[3:length(cols)])
      } else if (input$analyses_sens == "Learning") {
        table_data[Analysis=="Primary"]$SpecificAnalysis <- rep("Change in Score (No PM-Nth Play Interaction)",nrow(table_data[Analysis=="Primary",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="Score"]$SpecificAnalysis <- rep("Change in Score (PM-Nth Play Interaction)",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="Score",]))
        table_data[Analysis=="Sensitivity"&SpecificAnalysis=="Learning"]$SpecificAnalysis <- rep("Change in Learning (PM-Nth Play Interaction)",nrow(table_data[Analysis=="Sensitivity"&SpecificAnalysis=="Learning",]))
      } else if (input$analyses_sens == "Habit") {
        table_data[SpecificAnalysis=="14days_2hours"]$SpecificAnalysis <- rep("14 Days, 2 hours",nrow(table_data[SpecificAnalysis=="14days_2hours",]))
        table_data[SpecificAnalysis=="7days_4hours"]$SpecificAnalysis <- rep("7 Days, 4 hours",nrow(table_data[SpecificAnalysis=="7days_4hours",]))
        table_data[SpecificAnalysis=="7days_3hours"]$SpecificAnalysis <- rep("7 Days, 3 hours",nrow(table_data[SpecificAnalysis=="7days_3hours",]))
        table_data[SpecificAnalysis=="All"]$SpecificAnalysis <- rep("7 Days, 2 hours",nrow(table_data[SpecificAnalysis=="All",]))
        names(table_data)[names(table_data)=="Sensitivity"] <- "HabitType"
        names(table_data)[names(table_data)=="SpecificAnalysis"] <- "HabitualDefinition"
        table_data[HabitType=="HabitDefHabitual"]$HabitType <- rep("Habitual",nrow(table_data[HabitType=="HabitDefHabitual",]))
        table_data[HabitType=="HabitDefNonhabitual"]$HabitType <- rep("Non-habitual",nrow(table_data[HabitType=="HabitDefNonhabitual",]))
        table_data[Analysis=="Primary"]$HabitType <- table_data[Analysis=="Primary"]$HabitualDefinition
        table_data[Analysis=="Primary"]$HabitualDefinition <- rep("7 days, 2 hours",nrow(table_data[Analysis=="Primary",]))
        names(table_data)[names(table_data) == "HabitualDefinition"] <- "Habitual Definition"
        names(table_data)[names(table_data) == "HabitType"] <- "Group"
        cols <- c(cols[1:3],"Group",cols[4:length(cols)])
      }
      table_data$Beta <- as.numeric(sprintf("%.2f",table_data$Beta))
      
      names(table_data)[names(table_data) == "ExposureDuration"] <- "Exposure Duration"
      if (input$exposure_sens == "Smoke"){
        names(table_data)[names(table_data) == "ExposureLevel"] <- "Exposure Level"
      }
      
      datatable(table_data[,..cols],selection = 'none',options = list(paging = FALSE))
    }
  })
  
  ##################### EXPOSURE RESULTS #########################################
  
  observeEvent(input$minus, {
    new_date = input$dates_exp - 1
    updateSliderInput(session,"dates_exp", value = new_date)
    updateSliderInput(session,"dates_exp2", value = new_date)
  })
  
  observeEvent(input$plus, {
    new_date = input$dates_exp + 1
    updateSliderInput(session,"dates_exp", value = new_date)
    updateSliderInput(session,"dates_exp2", value = new_date)
  }) 
  
  observeEvent(input$minus2, {
    new_date = input$dates_exp2 - 1
    updateSliderInput(session,"dates_exp2", value = new_date)
    updateSliderInput(session,"dates_exp", value = new_date)
  })
  
  observeEvent(input$plus2, {
    new_date = input$dates_exp2 + 1
    updateSliderInput(session,"dates_exp2", value = new_date)
    updateSliderInput(session,"dates_exp", value = new_date)
  }) 
  
  observeEvent(input$tabset_exp, {
    if (input$tabset_exp=="Original") {
      updateSliderInput(session, "dates_exp2", value=input$dates_exp )
    } else if (input$tabset_exp=="ZIP3-Level"){
      updateSliderInput(session, "dates_exp", value=input$dates_exp2 )
    }
  })
  
  observeEvent(input$exposure_exp, {
    if (input$exposure_exp == "PM2.5") {
      showTab("tabset_exp",target ="Monitoring Stations") 
    } else {
      hideTab("tabset_exp",target ="Monitoring Stations") 
    }
  })
  
  output$exposure_text <- renderText({
    if (input$exposure_exp == "PM2.5") {
      paste("<b>Figure.</b>","Daily average ZIP3-level population-weighted PM2.5 on", input$dates_exp,".",
            "<br><br>The ZIP3-level daily and hourly average population-weighted PM2.5 exposure data can be downloaded <a href='' target='_blank'>here</a>.")
      
    } else {
      paste("<b>Figure.</b>","Maximum daily ZIP3-level smoke density on", input$dates_exp,".",
            "<br><br>The ZIP3-level daily maximum smoke density exposure data can be downloaded <a href='' target='_blank'>here</a>.")
    }
    
  })
  
  output$exposure_img <- renderImage({
    
    y <- year(input$dates_exp)
    d <- yday(input$dates_exp)
    if (input$exposure_exp == "PM2.5") {
      filename <- sprintf("./www/pm_zip3_%d_%d.png",d,y)
      list(src = filename,
           width = "95%")
    } else {
      filename <- sprintf("./www/smoke_zip3_%d_%d.png",d,y)
      list(src = filename,
           width = "62.5%")
    }
  }, deleteFile = FALSE)
  
  output$exposure_text2 <- renderText({
    if (input$exposure_exp == "PM2.5") {
      paste0("<b>Figure.</b>"," Daily average PM2.5 on ", input$dates_exp,", estimated through the BME data fusion of observations from",
             "<a href='https://www.epa.gov/aqs'>FRM/FEM</a> and <a href='https://api.purpleair.com/' target='blank'>PurpleAir</a> monitors.",
             "<br><br>The BME data fusion estimates of daily and hourly average PM2.5 at the census tract population centers can be downloaded <a href='https://doi.org/10.15139/S3/Z9WSWC' target='_blank'>here</a>.")
  
    } else {
      paste0("<b>Figure.</b>"," Smoke density on ", input$dates_exp,", from NOAA's <a href='https://www.ospo.noaa.gov/Products/land/hms.html' target='_blank'>Hazard Mapping System Fire and Smoke product</a>")
    }
  })
  
  output$exposure_img2 <- renderImage({
    y <- year(input$dates_exp2)
    d <- format(input$dates_exp2,format="%d")
    if (nchar(d) == 1) {
      d <- paste0("0",as.character(d))
    } else {
      d <- as.character(d)
    }
    m <- month(input$dates_exp2)
    if (input$exposure_exp == "PM2.5") {
      filename <- sprintf("./www/fusion_raw_%d_%s_%d.png",m,d,y)
      list(src = filename,
           width = "95%")
    } else {
      if (m < 10) {
        m <- paste0("0",as.character(m))
      } else {
        m <- as.character(m)
      }
      filename <- sprintf("./www/hms_raw_%s_%s_%d.png",m,d,y)
      list(src = filename,
           width = "62.5%")
    }
  }, deleteFile = FALSE)
  
  output$exposure_text3 <- renderText({
    paste("<b>Figure.</b> Location of FRM/FEM and PurpleAir monitoring stations used to estimate daily average PM2.5")
  })
  
  output$exposure_img3 <- renderImage({
    filename <- "./www/station_loc_daily.png"
    list(src = filename,
         width = "95%")
  }, deleteFile = FALSE)
  
  
  output$exposure_text4 <- renderText({
    paste("<b>Figure.</b> Location of FRM/FEM and PurpleAir monitoring stations used to estimate hourly average PM2.5")
  })
  
  output$exposure_img4 <- renderImage({
    filename <- "./www/station_loc_hourly.png"
    list(src = filename,
         width = "95%")
  }, deleteFile = FALSE)
  
  
  ##################### LUMOSITY RESULTS #########################################
  observeEvent (input$tabset_lum, {
    if(input$tabset_lum == "Learning Curve"){ 
      shinyjs::enable("subgroup_lum2")
    } else {
      shinyjs::disable("subgroup_lum2")
      updateRadioButtons(session, "subgroup_lum2", selected = "None")
    }
  })
  
  output$lumosity_text <- renderText({
    if(input$tabset_lum == "Learning Curve"){
      if (input$subgroup_lum == "Habit") {
        subgroup_lum <- "habitual behavior"
      } else {
        subgroup_lum <- input$subgroup_lum
      }
      if (input$subgroup_lum2 == "Habit") {
        subgroup_lum2 <- "habitual behavior"
      } else {
        subgroup_lum2 <- input$subgroup_lum2
      }
      if (input$subgroup_lum == "All" & input$subgroup_lum2 == "None") {
        paste("<b>Figure.</b> Average learning curve for all",tolower(input$region_lum),"US users")
      } else if (input$subgroup_lum == input$subgroup_lum2 || 
                 (input$subgroup_lum == "All" || input$subgroup_lum2 == "None")) {
        if (input$subgroup_lum != "All") {
          paste("<b>Figure.</b> Average learning curve for",tolower(input$region_lum),"US users by", tolower(subgroup_lum))
        } else {
          paste("<b>Figure.</b> Average learning curve for",tolower(input$region_lum),"US users by", tolower(subgroup_lum2))
        }
      } else {
        paste("<b>Figure.</b> Average learning curve for",tolower(input$region_lum),"US users by", tolower(subgroup_lum),
              "and",tolower(subgroup_lum2))
      }
    }
  })
  
  output$lumosity_tab <- renderText({
    if(input$tabset_lum == "Characteristics Table"){
      if (input$subgroup_lum == "Habit") {
        subgroup_lum <- "habitual behavior"
      } else {
        subgroup_lum <- input$subgroup_lum
      }
      if (input$subgroup_lum == "All") {
        if (input$region_lum == 'Western') {
          other_region = 'contiguous'
        } else {
          other_region = 'western'
        }
        paste("<b>Table.</b> User characteristics of the",tolower(input$region_lum),"and",other_region,"US study populations")
      } else {
        paste("<b>Table.</b> User characteristics of the",tolower(input$region_lum),"US study population by", tolower(subgroup_lum))
        
      }
    }
  })
  
  output$lumosity_plot <- renderPlot ({
    
    if (input$subgroup_lum2 == "None") {
      plot_data <- learning_curves[group1 == input$subgroup_lum&group2==input$subgroup_lum&
                                     region==input$region_lum,]
    } else if (input$subgroup_lum == "All" & input$subgroup_lum2 != "None") {
      plot_data <- learning_curves[group1 == input$subgroup_lum2&group2==input$subgroup_lum2&
                                     region==input$region_lum,]
    } else {
      plot_data <- learning_curves[group1 == input$subgroup_lum&group2==input$subgroup_lum2&
                                     region==input$region_lum,]
    }
    
    if (input$subgroup_lum2 == "None" || input$subgroup_lum==input$subgroup_lum2 ||
        (input$subgroup_lum == "All" && input$subgroup_lum2 != "None")) {
      ggplot(data=plot_data, aes(x=nth_play_factor, y=score_raw, group=var1,color=var1)) +
        geom_line(size=1.25) +
        scale_x_discrete(name="Nth Play")+
        scale_color_manual(name="Group",values=(primary_palette[primary_palette$Group%in%unique(plot_data$var1),2])) +
        scale_y_continuous(name="Raw Score",breaks=c(6000,9400,12800,16200,19600,23000),limits=c(6000,23000)) + 
        theme_bw(base_size=14)
    } else {
      ggplot(data=plot_data, aes(x=nth_play_factor, y=score_raw, group=var1,color=var1)) +
        geom_line(size=1.25) +
        scale_x_discrete(name="Nth Play")+
        scale_color_manual(name="Group",values=(primary_palette[primary_palette$Group%in%unique(plot_data$var1),2])) +
        scale_y_continuous(name="Raw Score",breaks=c(6000,9400,12800,16200,19600,23000),limits=c(6000,23000)) + 
        theme_bw(base_size=14)+ facet_wrap(~var2) + 
        theme(
          strip.placement = "outside",                    
          strip.background = element_rect(fill = "white",color="black"),
          strip.text.y.left = element_text(size = 12,angle=0),
          panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size=.1, color="grey90" ),
          panel.grid.minor.x = element_blank()
        )
      
    }
    
  })
  
  output$lumosity_table <- renderDT({
    
    if (input$region_lum == "Contiguous") {
      table_data <- table1_contig
    } else {
      table_data <- table1_west
    }
    
    if (input$subgroup_lum == "Gender") {
      table_data <- table_data[!(rownames(table_data)%in%rownames(table_data)[c(2)]),]
      table_data <- table_data[,c("All","Male","Female")]
    } else if (input$subgroup_lum == "Age") {
      table_data <- table_data[!(rownames(table_data)%in%rownames(table_data)[c(13:19)]),]
      table_data <- table_data[,c("All","18-29","30-39","40-49","50-59","60-69","70+")]
    } else if (input$subgroup_lum == "Habit") {
      table_data <- table_data[!(rownames(table_data)%in%rownames(table_data)[c(25)]),]
      table_data <- table_data[,c("All","Habitual","Non-habitual")]
    } else if (input$subgroup_lum == "Device") {
      table_data <- table_data[!(rownames(table_data)%in%rownames(table_data)[c(20:24)]),]
      table_data <- table_data[,c("All","Android","iPhone","iPad","Web")]
      names(table_data)[names(table_data)%in%c("iPad","iPhone")] <- c("iPad","iPhone")
    } else {
      if (input$region_lum == "Contiguous") {
        table_data2 <- table1_west
        table_data <- cbind(table_data[,c("All")],table_data2[,c("All")])
        colnames(table_data) <- c("Contiguous","Western")
      } else {
        table_data2 <- table1_contig
        table_data <- cbind(table_data[,c("All")],table_data2[,c("All")])
        colnames(table_data) <- c("Western","Contiguous")
      }
      
      rownames(table_data) <- rownames(table_data2)
      
    }
    
    rownames(table_data)[rownames(table_data) == "Lumosity Score, mean (SD)"] <- "Lost in Migration Score, mean (SD)"
    
    rownames(table_data) <- gsub(' ', '&nbsp', rownames(table_data))
    
    
    datatable(table_data,selection = 'none',
              options = list(paging = FALSE,ordering=F),
              escape = FALSE)
  })
  
  ##################################################################
  ################# ABOUT TEXT ####################################
  output$about_text <- renderText({
    paste("This dashboard serves as supporting information for the manuscript <b><i>The Effects of Short-Term PM2.5 and Wildfire Smoke Exposure on Cognitive Performance in US Adults.</i></b>",
          " It allows users to interact with the findings from both the primary and sensitivity analyses (Primary Results and Sensitivity Analyses tabs, respectively) as well as the exposure surfaces and Lumosity user data",
          " used to generate the results (Exposure Surfaces and Lumosity User Data tabs, respectively). <br><br>",
          "The source code for the dashboard can be viewed <a href='https://github.com/stephcleland/PMLumosityDashboard' target='_blank'>here</a>. <br><br>",
          "The BME data fusion estimates of daily and hourly average PM2.5 at census tract population centers can be downloaded <a href='https://doi.org/10.15139/S3/Z9WSWC' target='_blank'>here</a>. <br><br> ",
          "The ZIP3-level daily maximum smoke density and daily and hourly average population-weighted PM2.5 exposure data can be downloaded <a href='' target='_blank'>here</a>. <br><br>",
          "For any questions, please email: <a href = 'mailto: cleland.stephanie@epa.gov'>cleland.stephanie@epa.gov</a><br><br>",
          "<b>Abstract</b><br>",
          "There is increasing evidence that exposure to air pollution adversely impacts cognitive performance. While fine particulate matter (PM2.5) and wildfire smoke are pollutants of growing concern, there is limited research on how short-term exposure affects cognitive function. We aimed to evaluate the cognitive effects of daily and sub-daily PM2.5 and smoke exposure. Cognitive performance data from a brain training platform was obtained for 10,288 contiguous United States (US) adults, age 18+, who completed 20 plays of a game targeted to improve attention. We considered two measures of daily and sub-daily exposure: (1) daily and hourly PM2.5 estimates, obtained from a data fusion of observations from US Environmental Protection Agency and PurpleAir monitors, and (2) daily smoke density, obtained from satellite images of smoke plumes. We used a retrospective longitudinal repeated measures design with linear mixed effects models to test for associations between short-term exposure metrics and cognitive performance, overall and by age group, gender, habit, and region. Smoke analyses were only conducted in the western US. All measures of daily and sub-daily PM2.5 exposure were negatively associated with attention. A 10 ug/m3 increase in PM2.5 the day of gameplay was associated with 44.28 (95% CI: -84.27, -4.28) point decrease in score in the western US, with an estimated 6.5% reduction in final score attributable to exposure. The effects were most pronounced in western US users, habitual players, and younger (18-29) and older (70+) adults, with no observed differences by gender. The presence of medium and heavy smoke density in the days and weeks prior to play were also negatively associated with performance. Heavy smoke density the day before gameplay was associated with a 119.30 (95% CI: -212.24, -26.36) point decrease in score relative to no smoke. Younger users (18-29), habitual players, and men were most affected. Overall, our results indicate that short-term exposure to PM2.5 and wildfire smoke adversely impacts attention in adults, but further research is needed to elucidate these relationships.",
          "<br><br>",
          "<b>Authors:</b> Stephanie E. Cleland, Lauren H. Wyatt, Linda Wei, Naman Paul, Amrita Patil, Marc L. Serre, J. Jason West, Lumosity team, Sarah B. Henderson, Ana G. Rappold")
  })
  
}

shinyApp(ui, server)


