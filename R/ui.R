########################################################################
# Dashboard EpiLinx v. 1.3.2 for hospital data.
#
# Author: ANEH, SSI
########################################################################

epilinx_ui <- function(){

  pth <- system.file("www", "EpiLinxHeader.png", package="EpiLinx")
  header <- dashboardHeader(title=div(img(src=pth,height=42,width=160)))

  sidebar <- dashboardSidebar(
    fileInput("LPRfile", "Upload data file (.csv or .rds):",multiple = T,
      accept = c(".csv",".xlsx")),
    hr(),
    selectizeInput("outbreak",
      label="Choose outbreak:",
      choices = NULL,
      multiple=T,
      options = list(maxItems = 1)

    ),
    dateRangeInput("dates", "Select date range:",start = "2010-01-01",
      format= "dd-mm-yyyy"),
    selectizeInput("area",
      label="Select Region(s):",
      choices = NULL,
      multiple=T),
    sliderInput("daysbetween", "Select no. of days between admissions:",
      min = 0, max = 50,
      value = 14),
    actionButton("refresh","Refresh")
  )

  body <- dashboardBody(
    tags$head(tags$style(HTML(".progress-bar {background-color: #b51b21;}
                              #welcome{color:#009999;
                              font-size: 30px;}
                              #welcome2{font-size: 25px;}
                              .skin-black .main-sidebar {background-color: #009999;}
                              .skin-black .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #009999;}
                              .skin-black .main-sidebar. sidebar .sidebar-menu .active a{
                              background-color: #009999; color: #005555;}
                              .nav-tabs-custom .nav-tabs li.active {
                              border-top-color: #b51b21;}
                              .box.box-solid.box-primary>.box-header {
                              color:#fff;
                              background:#009999}
                              .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                              background: #b51b21;
                              border-top: 1px solid #FFF ;
                              border-bottom: 1px solid #FFF ;
                              border-right: 1px solid #FFF;
                              border-left: 1px solid #FFF;}
                              .irs-from, .irs-to, .irs-single { background: #FFF}
                              .box.box-solid.box-primary{
                              border-bottom-color:#009999;
                              border-left-color:#009999;
                              border-right-color:#009999;
                              border-top-color:#009999;}
                              .fa-home {color:#009999}
                              .fa-hospital {color:#009999}
                              .fa-hospital-alt {color:#009999}
                              .fa-business-time {color:#009999}
                              .fa-bed {color:#009999}
                              .fa-venus-mars {color:#009999}
                              .fa-bezier-curve {color:#009999}
                              .fa-chart-area {color:#009999}
                              .fa-door-open {color:#009999}
                              .fa-address-card {color:#009999}
                              .fa-align-justify {color:#009999}
                              .fa-stream {color:#009999}
                              .fa-chart-bar {color:#009999}
                              .fa-table {color:#009999}
                              .bg-purple {background-color: #b51b21 !important;}
                              #noLink{color:white; font-size:16px; font-style:bold; background: #b51b21;}"))),
    fluidRow(
      tabBox(id="tabmenu",width=12,
        tabPanel("Home",icon = icon("home","fa-lg"),
          fluidRow(
            column(width=12,
              uiOutput("welcome"),
              textOutput("welcome2"),
              hr(),
              tableOutput("out"),
              infoBoxOutput("no_outbreaks",width=3),
              infoBoxOutput("no_pt",width=3),
              infoBoxOutput("deaths",width=3),
              infoBoxOutput("timeline",width=3),
              verbatimTextOutput("noLink"))
          )),
        tabPanel("Unit overview", icon = icon("hospital","fa-lg"), h2(style="color:#009999","Patient overlaps"),
          fluidRow(
            tabBox(id="Overlaps",width=12,
              tabPanel("Direct overlaps",icon = icon("align-justify","fa-lg"),
                radioButtons('location', h4('Select location: ',style = "color:#b51b21;"), choices = c("Unit","Ward","Hospital")),
                h5("Mouse over datapoint in plot to show values of patient ID, date and unit respectively!",style = "color:#b51b21; font-weight: bold;"),
                plotOutput("DepPlot",height = "500px",
                  dblclick = "MyPlot_dblclick",
                  brush = brushOpts(
                    id = "MyPlot_brush",
                    resetOnNew = T),
                  hover = hoverOpts(
                    id = "dep_hover",clip=T))%>%withSpinner(color="#009999"),
                uiOutput("hover_dep"),
                dataTableOutput("Dep_table")),
              tabPanel("Unit/shifted time",icon = icon("stream","fa-lg"),
                dataTableOutput("Buffer_table"))
            ))),
        tabPanel("Unit activity", icon = icon("bed","fa-lg"), h2(style="color:#009999","Activity per unit"),
          fluidRow(
            tabBox(id="Units", width=12,
              tabPanel("Events per unit", icon = icon("chart-bar","fa-lg"),
                plotOutput("FreqPlot",height = "400px")%>%withSpinner(color="#009999"),
                plotOutput("VisPlot",height = "400px")%>%withSpinner(color="#009999")),
              tabPanel("Data for selected unit", icon = icon("table","fa-lg"),
                selectizeInput("unit",
                  label=h3("Select unit:",style = "color:#b51b21;") ,
                  choices = NULL,
                  multiple=F),
                plotOutput("unitCurve",height = "500px")%>%withSpinner(color="#009999"),
                dataTableOutput('unit_table'))
            ))),
        tabPanel("Epicurve", icon = icon("chart-area","fa-lg"), h2(style="color:#009999","Epicurve based on direct epilinks"),
          fluidRow(
            column(width=12,
              verbatimTextOutput("nodirectlink"),
              plotOutput("EpiCurve",height = "500px")%>%withSpinner(color="#009999")
            ))),
        tabPanel("Demographics", icon = icon("venus-mars","fa-lg"), h2(style="color:#009999","Gender/Age distribution"),
          fluidRow(
            column(width=12,
              plotOutput("AGPlot",height = "500px")%>%withSpinner(color="#009999"),
              dataTableOutput('AG_table')
            ))),
        tabPanel("Networks",icon = icon("bezier-curve","fa-lg"), h2(style="color:#009999","Patient network"),
          fluidRow(
            column(width=12,
              checkboxGroupInput('linktypes', h4('Select linktypes: ',style = "color:#b51b21;"),
                choices = list("Direct unit links"=1,"Indirect unit links"=3,"Hospital links"=2),
                selected=c(1,2,3),inline=T),
              plotOutput("NetworkPlot",height= "1200px",width="1200px")%>%withSpinner(color="#009999"),
              verbatimTextOutput("net_text"),
              DT::dataTableOutput('net_table')
            )
          )),
        tabPanel("Track patient", icon = icon("address-card","fa-lg"),
          fluidRow(
            column(width=12,
              selectizeInput("patient",
                label=h3("Select patient:",style = "color:#b51b21;") ,
                choices = NULL,
                multiple=F),
              hr(),
              plotOutput("PtMap",height = "300px",
                dblclick = "MyPlot_dblclick",
                brush = brushOpts(
                  id = "MyPlot_brush",
                  resetOnNew = T),
                hover = hoverOpts(
                  id = "pt_hover",clip=T))%>%withSpinner(color="#009999"),
              uiOutput("hover_pt"),
              actionButton("ptTab", "Show table of admissions",icon=icon("table")),
              bsModal("exTab", "Table of admissions","ptTab",dataTableOutput("pt_tab")),
              box(
                title = textOutput('title_pt_net'), width = 6, solidHeader = TRUE, status = "primary",
                checkboxGroupInput('linktypes_pt', h4('Select linktypes: ',style = "color:#b51b21;"),
                  choices = list("Direct unit links"=1,"Indirect unit links"=3,"Hospital links"=2),
                  selected=c(1,2,3),inline=T),
                plotOutput("Net_pt",height= "400px",width="750px")%>%withSpinner(color="#009999")
              )

            )))
      )
    )
  )

  dashboardPage(header, sidebar, body, skin = "black")

}
