########################################################################
# Dashboard EpiLinx
# 2024
# Author: ANEH, SSI
########################################################################

#' @importFrom utils head read.csv
#' @importFrom stats median reorder ave
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr gather unnest
#' @import ggraph
#' @importFrom igraph graph_from_data_frame
#' @import reshape2
#' @import shiny
#' @import ggnetwork
#' @import sna
#' @import shinydashboard
#' @import shinycssloaders
#' @importFrom DT renderDT datatable
#' @import shinyWidgets
#' @import shinyjs
#' @import shinyBS
#' @importFrom rlang .data
## TODO: rationalise imports

epilinx_server <- function(input, output,session) {
  options(shiny.maxRequestSize=30*1024^2)
  output$welcome <- renderUI({HTML(paste("Welcome to", em("EpiLinx"),"1.3.2!"))})
  output$welcome2 <- renderText({paste("Please submit .csv or .rds file for analysis.")})

  df <- reactive({
    inFile <- input$LPRfile
    if (is.null(inFile)){
      return(NULL)
    }

    df <- read.csv(inFile$datapath,sep=";",stringsAsFactors = F)
    names(df)[names(df)=="MuligPatientUdbrudsnr"]<- "patient"
    names(df)[names(df)=="dept_ind_dato"]<- "InDate"
    names(df)[names(df)=="dept_ud_dato"]<- "OutDate"
    names(df)[names(df)=="hospital_name"]<- "Hospital"
    names(df)[names(df)=="name"]<- "Department"
    names(df)[names(df)=="Provedato"]<- "Sample date"
    names(df)[names(df)=="D_STATUS_HEN_START"] <- "Died"
    names(df)[names(df)=="CPR_male"] <- "Gender"
    names(df)[names(df)=="alder"] <- "Age"
    df$Gender[df$Gender=="0"] <- "F"
    df$Gender[df$Gender=="1"] <- "M"
    df$InDate <- as.Date(df$InDate,format="%Y-%m-%d")
    df$OutDate <- as.Date(df$OutDate,format="%Y-%m-%d")
    df$`Sample date` <- as.Date(df$`Sample date`,format="%Y-%m-%d")
    if(!all(is.na(df$Died))){
      df$Died <- as.Date(df$Died,format="%Y-%m-%d")
    }
    if(!"Region" %in% names(df)){
      df$Region <- ""
    }

    df <- df[order(df$patient),]

  })


  ################ Update user inputs

  observe({
    updateDateRangeInput(session, "dates", start="2016-01-01",
                         end=Sys.Date())
    updateRadioButtons(session,"location",choices=c("Hospital","Department","Room"),
                       selected="Department",inline=T)
    updateSelectizeInput(session,"area",choices=as.list(unique(df()$Region)),
                         server=T,
                         selected=unique(df()$Region))
    updateSliderInput(session, "daysbetween", value=input$daysbetween)

  })



  observeEvent(input$area,{
    ardf <- df()[df()$Region %in% input$area,]

    observeEvent(input$location,{
      input$refresh
      dib <- ardf
      if (input$location == "Hospital") {
        dib$unit <- dib$Hospital
      } else if (input$location == "Department") {
        dib$unit <- paste(dib$Hospital, ", ", dib$Department)
      } else if (input$location == "Room") {
        dib$unit <- paste(dib$Hospital,", ", dib$Department,", ", dib$Room)
      }


      observeEvent(input$dates,{
        sub <- dib[as.Date(dib$InDate,format="%d-%m-%Y") >= as.Date(input$dates[1],format="%d-%m-%Y")
                   &as.Date(dib$OutDate,format="%d-%m-%Y") <= as.Date(input$dates[2],format="%d-%m-%Y"),]
        anonames <- paste0("Patient ",unique(sub$patient))
        sub$patient <- factor(sub$patient, labels=anonames)

        ranges <- reactiveValues(x = NULL, y = NULL)

        observe({
          updateSelectizeInput(session,"patient",choices=as.list(unique(sub$patient)),
                               server=T,
                               selected=sub$patient[1])
          updateSelectizeInput(session, "unit", choices = as.list(unique(sub$unit)))
        })

        incperiodDF <- eventReactive(input$act,{
          updateSliderInput(session, "incperiod", value=input$incperiod)
        })

        ############################################################
        # infoboxes
        ############################################################
        output$no_outbreaks <- renderInfoBox({
          infoBox(
            HTML(paste("Number of Outbreaks",br(),"in file")),length(unique(df()$MuligeUdbrud)), icon = icon("hornbill"),
            color = "purple", fill=T
          )
        })
        output$no_pt <- renderInfoBox({
          infoBox(
            HTML(paste("Number of Patients",br(),"in current outbreak")),length(unique(sub$patient)), icon = icon("diagnoses"),
            color = "purple",fill=T
          )
        })
        output$timeline <- renderInfoBox({
          infoBox(
            HTML(paste("Full timeline",br(),"of current outbreak")),tags$p(paste(format(min(sub$InDate),format="%d. %b. %Y"),
                                                                                 "-",format(max(sub$OutDate),format="%d. %b. %Y")),
                                                                           style = "font-size: 81%;"),
            icon = icon("calendar-alt"),
            color = "purple",fill=T
          )
        })
        output$deaths <- renderInfoBox({
          infoBox(
            HTML(paste("Number of deaths",br(),"in current outbreak")),length(unique(sub$Died))-1, icon = icon("cross"),
            color = "purple",fill=T
          )
        })


        #############################################################
        # Demographics
        #############################################################
        AGT <- NULL
        if("Age" %in% colnames(sub)){
          age_gender <- unique(sub[,c("patient","Age","Gender")])
          age_gender$AgeGrp <- cut(age_gender$Age,breaks=c(0,10,20,30,40,50,60,70,80,90,100))
          AGT <- age_gender %>% dplyr::group_by(.data$Gender)%>%dplyr::summarize("No. of patients" = n(),
                                                                           Median = median(.data$Age),
                                                                           "Min Age" = sprintf("%1.0f",min(.data$Age)),
                                                                           "Max Age" = sprintf("%1.0f",max(.data$Age)))
          output$AG_table <- DT::renderDataTable(AGT,filter="top",options = list(
            columnDefs = list(list(className = 'dt-center', targets = "_all"))))

          #   ##### Age/Gender plot #####
          if(!is.null(AGT)){
            output$AGPlot <- renderPlot({
              ggplot(age_gender,aes(x=.data$AgeGrp,fill=.data$Gender))+
                geom_bar(position="dodge")+
                labs(x = "Age group",y = "Number of patients")+
                scale_y_continuous(breaks = function(x)seq(ceiling(x[1]),floor(x[2]),by=1))+
                scale_fill_manual(values=c("M"="#00BFC4","F"="#F8766D"))
            })
          }
        }

        #############################################################
        # Patients per region
        #############################################################
        regs <- sub %>%
          group_by(.data$Region, .data$patient) %>%
          slice(1) %>%
          ungroup()

        output$RegionPlot <- renderPlot({
          ggplot(regs,aes(x=.data$Region))+
            geom_bar(position="dodge")+
            labs(x = "Region",y = "Number of patients")
        })

        #############################################################
        # Patients per department
        #############################################################

        freq <- sub %>%
          group_by(.data$unit) %>%
          mutate(count = n_distinct(.data$patient)) %>%
          ungroup()%>%
          filter (.data$count>1) %>%
          arrange(desc(.data$count))%>%
          distinct(.data$unit, .keep_all = T)%>%
          head(10)

        freq$unit <- as.factor(freq$unit)
        levels(freq$unit) <- gsub(", ", "\n", levels(freq$unit))

        ##### Frequency plot #####
        output$FreqPlot <- renderPlot({
          ggplot(freq,aes(x=reorder(unit,count,function(x)-max(x)),y=count))+
            geom_bar(stat="identity",position="dodge",aes(fill=freq$count))+
            theme(axis.text.x = element_text(angle=90,size=12,vjust=0.5),
                  legend.position = "none")+
            scale_y_continuous(breaks = seq(1,max(freq$count),by = 2))+
            geom_text(aes(label=count,y=freq$count-(0.5*count)),colour="white")+
            scale_fill_gradient2(position="bottom", low = "grey50", mid = "grey50", high = "#b51b21",
                                 midpoint = median(freq$count))+
            labs(x = "Unit")
        })

        ##### Individual unit curves #####
        observeEvent(input$unit,{
          selected_unit <- sub[sub$unit == input$unit,c("patient", "unit","InDate","OutDate")]
          if(min(as.Date(selected_unit$InDate)) != Inf){
            unit_dates <- selected_unit %>%
              rowwise %>%
              ## TODO: Can we use .data with InDate/OutDate/Date here??
              mutate(Date = list(seq(InDate, OutDate, by = "day"))) %>%
              unnest(cols = c(Date)) %>%
              group_by(.data$Date, .data$patient) %>%
              summarise(count = n_distinct(.data$patient), .groups = 'drop')
          }


          output$unitCurve <- renderPlot({
            input$refresh
            ggplot(unit_dates,aes(Date,count))+
              geom_bar(stat="identity",aes(fill=patient))+
              theme(axis.text.x = element_text(angle=90,size=10),panel.grid = element_blank())+
              labs(y="No of links")+
              scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")
          })
          output$unit_table <- DT::renderDataTable(selected_unit,filter="top",options = list(
            columnDefs = list(list(className = 'dt-center', targets = "_all"))))

        })


        if(min(sub$InDate)!=Inf){
          mindate <- min(as.Date(sub$InDate))
          dates <- seq(mindate, Sys.Date(), by=1)

          #############################################################
          # Generate epidemiological link tables
          #############################################################

          #n_days <- input$daysbetween
          links <- OverlapCalc(sub,"unit", n_days = 0)
          indirect_links <- OverlapCalc(sub,"unit", n_days = input$daysbetween)

          #### If no links found ####
          if(nrow(links)<1){
            output$noLink <- renderText({paste("OBS! Your outbreak contains no epilinks on current level")})
          }else{
            output$noLink <- renderText({paste(NULL)})}

          #### Table of indirect links ####
          output$Buffer_table <- renderDT(indirect_links,filter="top",options = list(
            columnDefs = list(list(className = 'dt-center', targets = "_all"))))


          # Matrices for tile visualization
          ResMat <- Matrix4Viz(sub, anonames, dates, "unit")

          # Tables or tilemaps
          map_filt <- Tbl4Viz(ResMat, "unit")

          #### Prepare procedure ####
          procedure <- sub %>%
            gather(key = "Event", value = "Date", .data$`Sample date`, .data$Died)%>%
            filter(!is.na(.data$Date))%>%
            mutate(Patient = .data$patient, Event = ifelse(.data$Event == "Died", "Death date", "Sampling date")) %>%
            select(.data$Patient,.data$Date,.data$unit, .data$Event) %>%
            distinct()


          ##### Collapse data #####
          coll_dat <- sub %>%
            group_by(.data$unit) %>%
            mutate(count = n_distinct(.data$patient),
                   Unit = as.character(.data$unit),
                   Unit_uniq_pt = if_else(.data$count == 1, NA_character_, as.character(.data$unit))) %>%
            ungroup()  %>%
            rowwise() %>%
            ## TODO: Can we use .data with InDate/OutDate/dates here??
            mutate(dates = list(seq(InDate, OutDate, by = "day"))) %>%
            unnest(cols = c(dates)) %>%
            select(Patient = .data$patient, Date = .data$dates, .data$Unit, .data$Unit_uniq_pt) %>%
            mutate(Date = as.Date(.data$Date)) %>%
            distinct()


          # For grey areas where only 1 patient visits
          null_point <- coll_dat %>%
            filter(is.na(.data$Unit_uniq_pt))%>%
            mutate(Unit = .data$Unit_uniq_pt)



          output$Dep_table <- renderDT(links,filter="top",extensions=c("Buttons"),options = list(
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            dom ="Brftip", buttons=c("csv","excel")))

          ##### Tilemaps #####
          output$DepPlot <- renderPlot({
            if("Death date" %in% procedure$Event){
              input$refresh
              ggplot(coll_dat, aes(Date,Patient))+
                geom_tile(aes(fill=coll_dat$Unit))+
                geom_tile(data=null_point,fill="grey72")+
                geom_hline(yintercept = seq_along(coll_dat$Patient) + 0.5,col= "white") +
                geom_point(data=subset(procedure, Event == "Sampling date"),aes(Date,Patient,colour= Event,shape= Event))+
                geom_point(data=subset(procedure, Event == "Death date"),aes(Date,Patient,colour= Event,shape= Event,cex=1.2))+
                theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5),
                      panel.grid.major = element_blank(),
                      legend.position = "none")+
                scale_fill_discrete(na.value="grey92")+
                scale_colour_manual(values=c("Sampling date"="black","Death date"="black"))+
                scale_shape_manual(values=c("Sampling date"=16,"Death date"=134))+
                scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
                labs(x = "Date",fill="Location")+
                coord_cartesian(xlim = ranges$x, ylim= ranges$y, expand=F)+
                guides(shape = guide_legend(override.aes = list(size=3)))
            }else{
              ggplot(coll_dat, aes(Date,Patient))+
                geom_tile(aes(fill=coll_dat$Unit))+
                geom_tile(data=null_point,fill="grey72")+
                geom_hline(yintercept = seq_along(coll_dat$Patient) + 0.5,col= "white") +
                geom_point(data=subset(procedure, Event == "Sampling date"),aes(as.Date(Date),Patient,colour= Event, shape= Event))+
                theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5),
                      panel.grid.major = element_blank(),
                      legend.position = "none")+
                scale_fill_discrete(na.value="grey92")+
                scale_colour_manual(values=c("Sampling date"="black"))+
                scale_shape_manual(values=c("Sampling date"=16))+
                scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
                labs(x = "Date",fill="Location")+
                coord_cartesian(xlim = ranges$x, ylim= ranges$y, expand=F)+
                guides(shape = guide_legend(override.aes = list(size=3)))
            }
            })

          observeEvent(input$MyPlot_dblclick,{
            brush <- input$MyPlot_brush
            if(!is.null(brush)){
              ranges$x <- c(as.Date(brush$xmin,origin="1970-01-01"), as.Date(brush$xmax,origin="1970-01-01"))
              ranges$y <- c(brush$ymin, brush$ymax)
            } else{
              ranges$x <- NULL
              ranges$y <- NULL
            }
          })

          # Hover Department
          output$hover_dep <- renderUI({
            if(!is.null(input$dep_hover)){
              hover <- input$dep_hover
              point<-  nearPoints(map_filt, hover, maxpoints = 1)

              left_px <- hover$range$left + (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left) * (hover$range$right - hover$range$left)
              top_px <- hover$range$top +(hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top)

              style <- paste0("position:absolute; z-index:100;", "left:", left_px + 2 , "px; top:", top_px + 2, "px;")

              wellPanel(
                style = style,
                p(HTML(paste0("<b>ID: </b>", point$Patient,"<br><b>Unit: </b>",point$unit,"<br><b>Date: </b>",point$Date)))
              )
            } else{
            }
          })
        }


        if(min(as.Date(links$Start)) != Inf){
          dir_link <- links[,c("Patient.1","Patient.2","unit","End")]
          dir_link$weight <- 1


          if(nrow(indirect_links)!=0){
            buf_link <- indirect_links[,c("Patient.1","Patient.2","unit")]
            buf_link$End <- " "
            buf_link$weight <- 2
          }else{
            buf_link <- NULL
          }
          net1 <- rbind(dir_link,buf_link)
          ## TODO: I don't think we can use .data here...
          net1<- unique(subset(net1,weight==ave(weight,Patient.1,Patient.2,FUN=min)))

          observeEvent(input$patient,{
            death_pt <- NULL
            output$title_pt_net <- renderText({paste0(input$patient,"'s network")})
            pt_df <- map_filt[map_filt$Patient== input$patient,]
            null_pt <- null_point[null_point$Patient== input$patient,]
            proc_pt <- procedure[procedure$Patient== input$patient,]
            pt_tb <- pt_df[!is.na(pt_df$unit),]
            pt_tb <- pt_tb %>% arrange(desc(.data$Date))


            output$pt_tab<- DT::renderDataTable(
              pt_tb,
              server=F,
              extensions=c("Buttons"),
              options=list(lengthChange=F,dom="Brftip",buttons=c("csv","excel")
              ))


            pt_contact <- net1[net1$Patient.1 == input$patient | net1$Patient.2 == input$patient,]
            pt_contact$Patient.1 <-as.character(as.numeric(sub("\\D+","",pt_contact$Patient.1)))
            pt_contact$Patient.2 <-as.character(as.numeric(sub("\\D+","",pt_contact$Patient.2)))
            pt_contact <- pt_contact %>% dplyr::group_by(Patient.1, Patient.2) %>% filter(End == max(End))


            output$PtMap <- renderPlot({
              if("Death date" %in% procedure$Event){
                ggplot(pt_df, aes(Date,Patient))+
                  geom_tile(aes(fill=unit))+
                  geom_point(data=subset(proc_pt, Event == "Sampling date"),aes(Date,Patient,colour= Event, shape= Event))+
                  geom_point(data=subset(proc_pt, Event == "Death date"),aes(Date,Patient,colour= Event, shape= Event,cex=1.2))+
                  theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5),panel.grid.major = element_blank(),
                        legend.position = "none",legend.direction = "horizontal")+
                  scale_fill_discrete(na.value="grey92")+
                  scale_colour_manual(values=c("Sampling date"="black","Death date"="black"))+
                  scale_shape_manual(values=c("Sampling date"=16,"Death date"=134))+
                  scale_size_continuous(guide=F)+
                  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
                  labs(x = "Date",y=element_blank(),fill="Unit")+
                  coord_cartesian(xlim = ranges$x, ylim= ranges$y, expand=F)+
                  guides(shape = guide_legend(override.aes = list(size=3)))
              }else{
                ggplot(pt_df, aes(Date,Patient))+
                  geom_tile(aes(fill=unit))+
                  geom_point(data=subset(proc_pt, Event == "Sampling date"),aes(as.Date(Date),Patient,colour= Event,shape= Event))+
                  theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5),panel.grid.major = element_blank(),
                        legend.position = "none",legend.direction = "horizontal")+
                  scale_fill_discrete(na.value="grey92")+
                  scale_colour_manual(values=c("Sampling date"="black"))+
                  scale_shape_manual(values=c("Sampling date"=16))+
                  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
                  labs(x = "Date",y=element_blank(),fill="Unit")+
                  coord_cartesian(xlim = ranges$x, ylim= ranges$y, expand=F)+
                  guides(shape = guide_legend(override.aes = list(size=3)))
              }
            })

            output$hover_pt <- renderUI({
              if(!is.null(input$pt_hover)){
                hover <- input$pt_hover
                point<-  nearPoints(pt_df, hover, maxpoints = 1)

                left_px <- hover$range$left + (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left) * (hover$range$right - hover$range$left)
                top_px <- hover$range$top +(hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top)

                style <- paste0("position:absolute; z-index:100;", "left:", left_px + 2, "px; top:", top_px + 2, "px;")

                wellPanel(
                  style = style,
                  p(HTML(paste0("<br><b>Unit: </b>",point$unit,"<br><b>Date: </b>",point$Date)))
                )
              } else{
              }
            })


            observeEvent(input$linktypes_pt,{

              pt_contact_net<-reactive({
                return(pt_contact[pt_contact$weight%in%input$linktypes_pt,])
              })

              nodesfirst_pt <- procedure[(procedure$Patient) %in% pt_contact_net()$Patient.1 |(procedure$Patient) %in% pt_contact_net()$Patient.2 ,]


              output$Net_pt <- renderPlot({res=256
              gr_pt <- graph_from_data_frame(pt_contact_net(), directed=F,nodesfirst_pt)
              set.seed(7)
              ggraph(gr_pt,layout="fr")+
                geom_edge_link(aes(edge_color=as.factor(.data$unit),edge_linetype=as.factor(.data$weight),label=substr(as.character(.data$End),1,7)),
                               edge_width=1,angle_calc= 'along',label_dodge = unit(2,'mm')) +
                geom_node_point(size = 12.5) +
                geom_node_point(size = 11.5,aes(colour = as.character(.data$unit))) +
                geom_node_text(aes(label = .data$name))+
                labs(color = "Sampling unit", edge_color = "Linking unit", edge_linetype="Link type")+
                theme_void()+
                scale_edge_linetype_manual(values=c("1"="solid","2"="dotted"),labels = c("1"="Direct link (Same unit/same period)","2"="Indirect link (Same unit/shifted period within 13 days)"))+
                theme(legend.box = "vertical",legend.spacing = unit(1,"cm"))
              })
            })
          })

          #############################################################
          # Networks
          # ###########################################################

          if(min(as.Date(links$Start)) != Inf){
            dir_link_net <- links[,c("Patient.1","Patient.2","unit","End")]
            dir_link_net$weight <- 1

            if(nrow(indirect_links)!=0){
              buf_link_net <- indirect_links[,c("Patient.1","Patient.2","unit")]
              buf_link_net$End <- " "
              buf_link_net$weight <- 2
            }else{
              buf_link_net <- NULL
            }


            if (nrow(dir_link_net) == 0 && is.null(buf_link_net)) {
              netM <- data.frame(Patient.1 = character(), Patient.2 = character(),
                                 unit = character(), End = character(),
                                 weight = numeric())
            } else {
              netM <- rbind(dir_link_net, buf_link_net)
            }


            #netM <- rbind(dir_link_net,buf_link_net)
            netM<- unique(subset(netM,weight==ave(weight,Patient.1,Patient.2,FUN=min)))
            netM$Patient.1 <-as.character(as.numeric(sub("\\D+","",netM$Patient.1)))
            netM$Patient.2 <-as.character(as.numeric(sub("\\D+","",netM$Patient.2)))
            netM <- netM %>% dplyr::group_by(Patient.1, Patient.2) %>% filter(End == max(End))

            observeEvent(input$linktypes,{

              net <- reactive({
                input$refresh  # Opdater på knappen
                isolate({
                  filtered_net <- netM[netM$weight %in% input$linktypes, ]
                  if (nrow(filtered_net) == 0) {
                    return(data.frame())  # Returner tom dataframe
                  }
                  return(filtered_net)
                })
              })


              # Additional nodes
              nodes <- unique(c(netM$Patient.1, netM$Patient.2))

              if(length(net())>=3){

                #For table
                procedure$Patient <- as.character(as.numeric(sub("\\D+","",procedure$Patient)))
                no_net <- procedure[!(procedure$Patient) %in% nodes,]
                no_net <- no_net[,c(1,3)]
                no_net$Patient <- as.character(no_net$Patient)

                output$net_text <-renderText({paste("Following patients are not linked to other patients by any EpiLinx criterion:")})
                output$net_table <- DT::renderDataTable(datatable(no_net,rownames=F),filter="top",options = list(
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))))

                no_pat <-procedure
                #no_pat$patient <-as.character(as.numeric(sub("\\D+","",no_pat$patient)))
                nodesfirst <- no_pat %>%
                  filter(Patient %in% net()$Patient.1 | Patient %in% net()$Patient.2) %>%
                  distinct(Patient, .keep_all = TRUE)

                gr1<-reactive({input$refresh
                  isolate({
                    if(!is.null(net()) && nrow(net()) >= 3){
                      graph1 <- graph_from_data_frame(net(), directed=F,nodesfirst)
                      return(graph1)
                    }else{
                      return(NULL)
                    }
                  })
                })

                ##### network Plot #####
                output$NetworkPlot <- renderPlot({res=256
                #gr1 <- graph_from_data_frame(net(), directed=F,nodesfirst)
                if(!is.null(gr1())){
                  set.seed(7)
                  ggraph(gr1(),layout="fr")+
                    geom_edge_link(aes(edge_color=as.factor(unit),edge_linetype=as.factor(weight),label=substr(as.character(End),1,7)),
                                   edge_width=1,angle_calc= 'along',label_dodge = unit(2,'mm')) +
                    geom_node_point(size = 12.5) +
                    geom_node_point(size = 11.5,aes(colour = as.character(unit))) +
                    geom_node_text(aes(label = name))+
                    labs(color = "Sampling unit", edge_color = "Linking unit", edge_linetype="Link type")+
                    theme_void()+
                    scale_edge_linetype_manual(values=c("1"="solid","2"="dotted"),labels = c("1"="Direct link (Same unit/same period)","2"="Indirect link (Same unit/shifted period within 13 days)"))+
                    theme(legend.box = "vertical",legend.spacing = unit(1,"cm"), legend.position="bottom")
                }else{
                  ggplot() +
                    annotate("text", x = 0.5, y = 0.5, label = "No network data available", size = 6, hjust = 0.5) +
                    theme_void()
                }

                })
              }
            })
          }

          #############################################################
          # Visit frequencies per department
          #############################################################

          vis_dep <- links
          vis_dep <- vis_dep %>% dplyr::group_by(unit)%>% dplyr::mutate(count = n())
          vis_dep <- vis_dep[,c("unit","count")]
          vis_dep$unit <- as.factor(vis_dep$unit)
          levels(vis_dep$unit) <- gsub(", ", "\n", levels(vis_dep$unit))


          ##### Event plot #####
          output$VisPlot <- renderPlot({
            ggplot(vis_dep,aes(x=reorder(unit,count,function(x)-max(x)),y=count))+
              geom_bar(stat="identity",position="dodge",aes(fill=vis_dep$count))+
              labs(x = "unit", y = "Number of possible links")+
              theme(axis.text.x = element_text(angle=90,size=12,vjust=0.5),
                    legend.position = "none")+
              scale_y_continuous(breaks = seq(1,max(vis_dep$count),by = 2))+
              geom_text(aes(label=count,y=vis_dep$count-(0.5*count)),colour="white")+
              scale_fill_gradient2(position="bottom", low = "grey50", mid = "grey50", high = "#b51b21")
          })




          #############################################################
          # Epicurve
          #############################################################
          epi_df <- sub %>% select(.data$patient,.data$`Sample date`,.data$unit) %>% distinct(.data$patient, .keep_all=T)


          ##### Epicurve plot #####
          output$EpiCurve <- renderPlot({
            ggplot(epi_df,aes(`Sample date`))+
              geom_histogram(aes(fill=unit))+
              theme(axis.text.x = element_text(angle=90,size=10),panel.grid = element_blank(),
                    legend.position = "bottom")+
              labs(y="No of patients")+
              scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")
          })

        }
      })
    })
  })
}
