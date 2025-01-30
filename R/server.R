########################################################################
# Dashboard EpiLinx v. 1.3.2 for hospital data.
#
# Author: ANEH, SSI
########################################################################

epilinx_server <- function(input, output,session) {
  options(shiny.maxRequestSize=30*1024^2)
  output$welcome <- renderUI({HTML(paste("Welcome to", em("EpiLinx"),"1.3.2!"))})
  output$welcome2 <- renderText({paste("Please submit .csv or .rds file for analysis.")})

  df <- reactive({
    inFile <- input$LPRfile
    if (is.null(inFile)){
      return(NULL)
    }


    if (endsWith(inFile$name, '.csv')){
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
    } else{
      df <- readRDS(inFile$datapath)
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
    }
  })


  ################ Update user inputs

  observe({
    updateSelectizeInput(session,"outbreak",choices=as.list(unique(df()$MuligeUdbrud)),
                         server=T,
                         selected=df()$MuligeUdbrud[1])
    updateDateRangeInput(session, "dates", start="2016-01-01",
                         end=Sys.Date())
    updateRadioButtons(session,"location",choices=c("Unit","Ward","Hospital"),
                       selected="Unit",inline=T)
    updateSelectizeInput(session,"area",choices=as.list(unique(df()$Region)),
                         server=T,
                         selected=unique(df()$Region))
    updateSliderInput(session, "daysbetween", value=input$daysbetween)

  })


  observeEvent(input$outbreak,{
      input$refresh
    dib <- df()[df()$MuligeUdbrud == input$outbreak,]
    observeEvent(input$area,{
      ardf <- dib[dib$Region %in% input$area,]

      observeEvent(input$dates,{
        sub <- ardf[as.Date(ardf$InDate,format="%d-%m-%Y") >= as.Date(input$dates[1],format="%d-%m-%Y") &as.Date(ardf$OutDate,format="%d-%m-%Y") <= as.Date(input$dates[2],format="%d-%m-%Y"),]
        sub$unit <- paste(sub$Hospital,sub$Department,sep=", ")
        sub$Ward <- paste(sub$Hospital,sub$Department,sub$Ward)
        anonames <- paste0("Patient ",unique(sub$patient))
        sub$patient <- factor(sub$patient, labels=anonames)

        ranges <- reactiveValues(x = NULL, y = NULL)

        observe({
          updateSelectizeInput(session,"patient",choices=as.list(unique(sub$patient)),
                               server=T,
                               selected=sub$patient[1])
          updateSelectizeInput(session, "unit", choices = as.list(unique(sub$Department)))
        })

        incperiodDF <- eventReactive(input$act,{
          updateSliderInput(session, "incperiod", value=input$incperiod)
        })

        #############################################################
        # Demographics
        #############################################################
        AGT <- NULL
        if("Age" %in% colnames(sub)){
          age_gender <- unique(sub[,c("patient","Age","Gender")])
          age_gender$AgeGrp <- cut(age_gender$Age,breaks=c(0,10,20,30,40,50,60,70,80,90,100))
          AGT <- age_gender %>% dplyr::group_by(Gender)%>%dplyr::summarize("No. of patients" = n(),
                                                                           Median = median(Age),
                                                                           "Min Age" = sprintf("%1.0f",min(Age)),
                                                                           "Max Age" = sprintf("%1.0f",max(Age)))
          output$AG_table <- renderDataTable(AGT,filter="top",options = list(
            columnDefs = list(list(className = 'dt-center', targets = "_all"))))

          ##### Age/Gender plot #####
          if(!is.null(AGT)){
            output$AGPlot <- renderPlot({
              ggplot(age_gender,aes(x=AgeGrp,fill=Gender))+
                geom_bar(position="dodge")+
                labs(x = "Age group",y = "Number of patients")+
                scale_y_continuous(breaks = function(x)seq(ceiling(x[1]),floor(x[2]),by=1))+
                scale_fill_manual(values=c("M"="#00BFC4","F"="#F8766D"))
            })
          }
        }


        #############################################################
        # Patients per department
        #############################################################
        freq <- sub %>% mutate(count=ave(as.character(sub$patient),as.character(sub$unit),FUN=function(x)length(unique(x))))
        freq$unit[freq$count==1]<-0
        freq <- freq[,c("unit","count")]%>%filter(count>1)
        freq$count <- as.integer(freq$count)
        freq <- freq[!duplicated(freq),]
        freq_dep<-head(freq[order(-freq$count),],10)
        freq_dep$unit <- as.factor(freq_dep$unit)
        levels(freq_dep$unit) <- gsub(", ", "\n", levels(freq_dep$unit))

        ##### Frequency plot #####
        output$FreqPlot <- renderPlot({
          ggplot(freq_dep,aes(x=reorder(unit,count,function(x)-max(x)),y=count))+
            geom_bar(stat="identity",position="dodge",aes(fill=freq_dep$count))+
            labs(x = "unit", y = "Unique patient admissions")+
            theme(axis.text.x = element_text(angle=90,size=12,vjust=0.5),
                  legend.position = "none")+
            scale_y_continuous(breaks = seq(1,max(freq_dep$count),by = 2))+
            geom_text(aes(label=count,y=freq_dep$count-(0.5*count)),colour="white")+
            scale_fill_gradient2(position="bottom", low = "grey50", mid = "grey50", high = "#b51b21",
                                 midpoint = median(freq_dep$count))
        })

        observeEvent(input$unit,{
          selected_unit <- sub[sub$Department == input$unit,c("patient", "Department", "Hospital",
                                                              "InDate","OutDate")]
          if(min(as.Date(selected_unit$InDate)) != Inf){
            unit_dates <- seq(min(as.Date(selected_unit$InDate)),max(as.Date(selected_unit$OutDate)),by="day")

            unit_mat <- matrix(data=NA, nrow=length(unique(selected_unit$patient)), ncol = length(unit_dates))
            colnames(unit_mat) <- unit_dates
            rownames(unit_mat) <- unique(selected_unit$patient)

            for (i in  1:nrow(selected_unit)){
              tempDays=seq(min(as.Date(selected_unit$InDate[i])),max(as.Date(selected_unit$OutDate[i])),by="day")
              colIndx=which(unit_dates %in% tempDays)
              rowIndx =which(rownames(unit_mat) %in% unique(as.character(selected_unit$patient[i])))
              unit_mat[rowIndx,colIndx] = as.character(selected_unit$patient[i])
            }
            colnames(unit_mat) <- format(as.Date(unit_dates, format= "%Y-%m-%d"), format= "%Y-%m-%d")
            unit_df <- melt(as.table(unit_mat))
            colnames(unit_df) <- c("Link", "Date", "Patient")

            count<-unit_df %>% group_by(Date)%>%count(Patient)
            count <- na.omit(count)
            count$Date <- as.Date(count$Date)

            output$unitCurve <- renderPlot({
              input$refresh
              ggplot(count,aes(Date,n))+
                geom_bar(stat="identity",aes(fill=Patient))+
                theme(axis.text.x = element_text(angle=90,size=10),panel.grid = element_blank())+
                #scale_y_continuous(breaks=seq(0,max(count$n)*2,by=1))+
                labs(y="No of links")+
                scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")
            })
            output$unit_table <- renderDataTable(selected_unit,filter="top",options = list(
              columnDefs = list(list(className = 'dt-center', targets = "_all"))))
          }
        })


        if(min(sub$InDate)!=Inf){
          mindate <- min(as.Date(sub$InDate))
          dates <- seq(mindate, Sys.Date(), by=1)

          #Link tables
          Department <- OverlapCalc(sub,"unit")
          hospital <- OverlapCalc(sub,"Hospital")
          ward <- OverlapCalc(sub,"Ward")

          ##### Buffer time table
          buff_dep <- NULL
          n_days <- input$daysbetween
          for (i in 1:(nrow(sub)-1)){
            # Look through Department and hospital
            loc = ((1:nrow(sub))>i) & (!grepl(sub$patient[i],sub$patient))&
              (sub$unit[i]==sub$unit)&
              (((sub$InDate[i]>sub$InDate)&(sub$InDate[i]<sub$OutDate+n_days)) |
                 ((sub$InDate>sub$InDate[i])&(sub$InDate<sub$OutDate[i]+n_days))|
                 ((sub$OutDate[i]+n_days<sub$OutDate+n_days)&(sub$OutDate[i]+n_days>sub$InDate))|
                 ((sub$OutDate+n_days<sub$OutDate[i]+n_days)&(sub$OutDate+n_days>sub$InDate[i])))
            if (sum(na.omit(loc))>0){
              dep_append <- as.data.frame(list('Patient 1' =paste0(ifelse(sub$OutDate[i]>sub$InDate[loc],as.character(sub$patient[loc]),as.character(sub$patient[i]))),
                                               'Patient 2' =paste0(ifelse(sub$OutDate[loc]>sub$InDate[i],as.character(sub$patient[loc]),as.character(sub$patient[i]))),
                                               'unit'=sub$unit[loc],
                                               'Patient 1 End'= ifelse(sub$OutDate[i]>sub$InDate[loc],as.character(sub$OutDate[loc]),as.character(sub$OutDate[i])),
                                               'Patient 2 Start'= ifelse(sub$OutDate[i]>sub$InDate[loc],as.character(sub$InDate[i]),as.character(sub$InDate[loc]))
              ))
              buff_dep <- rbind(dep_append,buff_dep)
              rm(dep_append)
            }
            rm(loc)
          }
          if(!is.null(buff_dep)){
            buff_dep$Difference_days <- as.integer(as.Date(buff_dep$Patient.2.Start)-as.Date(buff_dep$Patient.1.End))
            buffer_df <- buff_dep %>% filter(buff_dep$Difference_days > 0)
            buffer_df <- buffer_df[order(buffer_df$Difference_days),]

            buffer_df$Patient.1.End <- as.character(buffer_df$Patient.1.End)
            buffer_df$Patient.2.Start <- as.character(buffer_df$Patient.2.Start)

            output$Buffer_table <- renderDT(buffer_df,filter="top",options = list(
              columnDefs = list(list(className = 'dt-center', targets = "_all"))))
          }

         # if(!is.null(Department)&!is.null(hospital)){
            # Matrices for tile visualization
            ResMat <- Matrix4Viz(sub, anonames, dates, "unit")
            ResHosp <- Matrix4Viz(sub, anonames, dates, "Hospital")
            ResWard <- Matrix4Viz(sub, anonames, dates, "Ward")

            # Tables or tilemaps
            map_filt <- Tbl4Viz(ResMat, "unit")
            map_hosp <- Tbl4Viz(ResHosp, "Hospital")
            map_ward <- Tbl4Viz(ResMat, "Ward")

            #### Prepare procedure
            proc <-  matrix(data=NA, nrow=length(rownames(ResMat)), ncol = length(colnames(ResMat)))
            proc_pt <- rownames(ResMat)
        proc_dates <- as.Date(colnames(ResMat))
            proc_days<- unique(sub$`Sample date`)
            rownames(proc) <- proc_pt
            colnames(proc) <- proc_dates

            # Procedure center
            proc_center <- matrix(data=NA, nrow=length(rownames(ResMat)), ncol = length(proc_days))
            rownames(proc_center) <- proc_pt
            colnames(proc_center) <- proc_days

            #Prepare death dates
            df_DD <- NULL
            death_filt <- NULL
            if(!all(is.na(sub$Died))){
              df_DD <- sub%>%filter(!is.na(sub$Died))
              death_pt <- unique(df_DD$patient)
              death_dates <- seq(min(as.Date(df_DD$Died)),max(as.Date(df_DD$Died)),by="day")

              death <- matrix(data=NA, nrow=length(death_pt), ncol = length(death_dates))

              rownames(death) <- death_pt
              colnames(death) <- death_dates
              # Add death dates
              for (i in  1:nrow(df_DD)){
                tempDeath=seq(min(as.Date(df_DD$Died[i])),max(as.Date(df_DD$Died[i])), "day")
                coldeath=which(death_dates %in% tempDeath)
                rowdeath =which(death_pt %in% df_DD$patient[i])
                death[rowdeath,coldeath] = df_DD$Died[i]
              }

              colnames(death) <- format(as.Date(death_dates, format= "%Y-%m-%d"), format= "%Y-%m-%d")
              death_filt <- melt(as.table(death))
              colnames(death_filt) <- c("Patient", "Date", "Died")
              death_filt <- death_filt[complete.cases(death_filt),]
              death_filt$Date <- as.Date(death_filt$Date)
              death_filt$Patient <- as.factor(death_filt$Patient)

              death_filt$Died <- "Death date"

            }

            # Add procedure
            for (i in  1:nrow(sub)){
              singleProc=sub$`Sample date`[i]
              tempDays=seq(min(as.Date(sub$InDate[i])),max(as.Date(sub$OutDate[i])), "day")
              tempProc=seq(min(as.Date(sub$`Sample date`[i])),max(as.Date(sub$`Sample date`[i])), "day")
              colproc=which(proc_dates %in% tempProc)
              rowproc =which(proc_pt %in% sub$patient[i])
              coldays <- which(singleProc %in% tempDays)
              proc[rowproc,colproc] =paste(sub$`Sample date`[i])
              proc_center[rowproc,coldays] =sub$unit[i]
            }

            #Trim procedure df
            colnames(proc) <- format(as.Date(dates, format= "%Y-%m-%d"), format= "%Y-%m-%d")
            proc_filt <- melt(as.table(proc))
            colnames(proc_filt) <- c("Patient", "Date", "Procedure")
            proc_filt <- proc_filt[complete.cases(proc_filt),]
            proc_filt$Patient <- as.factor(proc_filt$Patient)
            proc_filt$Date <- as.Date(proc_filt$Date)
            proc_filt$Procedure <- "Sampling date"

            #Trim procedure center df
            no_pat <-sub("\\D+","", unique(sub$patient))

            colnames(proc_center) <- format(as.Date(proc_days, format= "%Y-%m-%d"), format= "%Y-%m-%d")
            proc_center_filt <- melt(as.table(proc_center))
            colnames(proc_center_filt) <- c("Patient", "Date", "unit")
            proc_center_filt <- proc_center_filt[1:length(no_pat),]

            proc_center_filt$Patient <- as.numeric(sub("\\D+","",proc_center_filt$Patient))
            proc_center_filt<- proc_center_filt[order(proc_center_filt$Patient),]
            proc$center_filt <- as.character(proc_center_filt$unit)

            ##### Collapse data #####
            overl_data <- sub %>% mutate(count=ave(as.character(sub$patient),as.character(sub$unit),FUN=function(x)length(unique(x))))
            overl_data$unit[overl_data$count==1]<-0

            overl_data_ward <- sub %>% mutate(count=ave(as.character(sub$patient),as.character(sub$Ward),FUN=function(x)length(unique(x))))
            overl_data_ward$Ward[overl_data_ward$count==1]<-0

            # Prepare matrix for visualization
            collapseMat <- matrix(data=NA, nrow=length(anonames), ncol = length(dates))
            rownames(collapseMat) <- anonames
            colnames(collapseMat) <- dates

            collapseWardMat <- collapseMat

            for (i in  1:nrow(overl_data)){
              tempDays=seq(min(as.Date(overl_data$InDate[i])),max(as.Date(overl_data$OutDate[i])), "day")
              colIndx=which(dates %in% tempDays)
              rowIndx =which(anonames %in% overl_data$patient[i])
              collapseMat[rowIndx,colIndx] = overl_data$unit[i]
              collapseWardMat[rowIndx,colIndx] = overl_data_ward$Ward[i]
            }

            colnames(collapseMat) <- format(as.Date(dates, format= "%Y-%m-%d"), format= "%Y-%m-%d")
            colnames(collapseWardMat) <- format(as.Date(dates, format= "%Y-%m-%d"), format= "%Y-%m-%d")
            # Filter non-admitted patients
            collapseMat <- collapseMat[rowSums(is.na(collapseMat))!=ncol(collapseMat),]

            collapseWardMat <- collapseWardMat[rowSums(is.na(collapseWardMat))!=ncol(collapseWardMat),]

            #Full timeline
            coll_dat <- melt(as.table(collapseMat))
            colnames(coll_dat) <- c("Patient", "Date", "Unit")
            coll_dat$Date <- as.Date(coll_dat$Date)

            coll_ward <- melt(as.table(collapseWardMat))
            colnames(coll_ward) <- c("Patient", "Date", "Ward")
            coll_ward$Date <- as.Date(coll_ward$Date)

            #Define white areas
            null_point <- subset(coll_dat, Unit==0)
            coll_dat$Unit[coll_dat$Unit==0]<-NA

            null_point_hosp <- subset(map_hosp, Hospital==0)
            map_hosp$Hospital[map_hosp$Hospital==0]<-NA

            null_point_ward <- subset(coll_ward, Ward==0)
            coll_ward$Ward[coll_ward$Ward==0]<-NA

            observeEvent(input$location,{
              if(input$location == "Unit"){
                coll_dat <- coll_dat
                null_point <- null_point
                map_filt <- map_filt
                output$Dep_table <- renderDT(Department,filter="top",extensions=c("Buttons"),options = list(
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  dom ="Brftip", buttons=c("csv","excel")))
              } else if(input$location=="Hospital"){
                coll_dat <- map_hosp
                null_point <- null_point_hosp
                map_filt <- map_hosp
                names(map_filt)[names(map_filt)=="Hospital"]<- "unit"
                output$Dep_table <- renderDataTable(hospital,filter="top",extensions=c("Buttons"),options = list(
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  dom ="Brftip", buttons=c("csv","excel")))
              } else{
                coll_dat <- coll_ward
                null_point <- null_point_ward
                map_filt <- map_ward
                names(map_filt)[names(map_filt)=="Ward"]<- "unit"
                output$Dep_table <- renderDataTable(ward,filter="top",extensions=c("Buttons"),options = list(
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  dom ="Brftip", buttons=c("csv","excel")))
              }


              ##### Tilemaps #####
              output$DepPlot <- renderPlot({
                if(!is.null(death_filt)){
                  input$refresh
                  ggplot(coll_dat, aes(Date,Patient))+
                    geom_tile(aes(fill=coll_dat[,input$location]))+
                    geom_tile(data=null_point,fill="grey72")+
                    geom_hline(yintercept = seq_along(coll_dat$Patient) + 0.5,col= "white") +
                    geom_point(data=proc_filt,aes(Date,Patient,colour=Procedure,shape=Procedure))+
                    geom_point(data=death_filt,aes(Date,Patient,colour=Died,shape=Died,cex=1.2))+
                    theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5),
                          panel.grid.major = element_blank(),
                          legend.position = "none")+
                    scale_fill_discrete(na.value="grey92")+
                    scale_colour_manual(values=c("Sampling date"="black","Death date"="black"))+
                    scale_shape_manual(values=c("Sampling date"=16,"Death date"=134))+
                    scale_size_continuous(guide=F)+
                    scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
                    labs(x = "Date",fill="Location")+
                    coord_cartesian(xlim = ranges$x, ylim= ranges$y, expand=F)+
                    guides(shape = guide_legend(override.aes = list(size=3)))
                } else{
                  input$refresh
                  ggplot(coll_dat, aes(Date,Patient))+
                    geom_tile(aes(fill=coll_dat[,input$location]))+
                    geom_tile(data=null_point,fill="grey72")+
                    geom_hline(yintercept = seq_along(coll_dat$Patient) + 0.5,col= "white") +
                    geom_point(data=proc_filt,aes(Date,Patient,colour=Procedure,shape=Procedure))+
                    theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5),
                          panel.grid.major = element_blank(), legend.position = "none")+
                    scale_fill_discrete(na.value="grey92")+
                    scale_colour_manual(values="black")+
                    scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
                    labs(x = "Date",fill="Location")+
                    coord_cartesian(xlim = ranges$x, ylim= ranges$y, expand=F)
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

                  style <- paste0("position:absolute; z-index:100;", "left:", left_px + 2, "px; top:", top_px + 2, "px;")

                  wellPanel(
                    style = style,
                    p(HTML(paste0("<b>ID: </b>", point$Patient,"<br><b>Unit: </b>",point$unit,"<br><b>Date: </b>",point$Date)))
                  )
                } else{
                }
              })
            })


            if(min(as.Date(Department$Start)) != Inf){
              dir_link <- Department[,c("Patient.1","Patient.2","unit","End")]
              dir_link$weight <- 1

              hos_link <- hospital[,c("Patient.1","Patient.2","Hospital","End")]
              hos_link$weight <- 2
              names(hos_link)[names(hos_link)=="Hospital"]<- "unit"

              if(nrow(buffer_df)!=0){
                buf_link <- buffer_df[,c("Patient.1","Patient.2","unit")]
                buf_link$End <- " "
                buf_link$weight <- 3
              }else{
                buf_link <- NULL
              }
              net1 <- rbind(dir_link,hos_link,buf_link)
              net1<- unique(subset(net1,weight==ave(weight,Patient.1,Patient.2,FUN=min)))

              observeEvent(input$patient,{
                death_pt <- NULL
                output$title_pt_net <- renderText({paste0(input$patient,"'s network")})
                pt_df <- map_filt[map_filt$Patient== input$patient,]
                null_pt <- null_point[null_point$Patient== input$patient,]
                proc_pt <- proc_filt[proc_filt$Patient== input$patient,]
                death_pt <-death_filt[death_filt$Patient== input$patient,]
                pt_tb <- pt_df[!is.na(pt_df$unit),]
                pt_tb <- pt_tb %>% arrange(desc(Date))


                output$pt_tab<- renderDataTable(
                  pt_tb,
                  server=F,
                  extensions=c("Buttons"),
                  options=list(lengthChange=F,dom="Brftip",buttons=c("csv","excel")
                ))


                pt_contact <- net1[net1$Patient.1 == input$patient | net1$Patient.2 == input$patient,]
                pt_contact$Patient.1 <-as.character(as.numeric(sub("\\D+","",pt_contact$Patient.1)))
                pt_contact$Patient.2 <-as.character(as.numeric(sub("\\D+","",pt_contact$Patient.2)))
                pt_contact <- pt_contact %>% dplyr::group_by(Patient.1, Patient.2) %>% filter(End == max(End))

                #output$pt_meet<- renderDataTable(pt_contact)

                output$PtMap <- renderPlot({
                  if(!is.na(death_pt$Died[1])){
                    ggplot(pt_df, aes(Date,Patient))+
                      geom_tile(aes(fill=unit))+
                      #geom_tile(data=null_pt,fill="grey72")+
                      geom_point(data=proc_pt,aes(Date,Patient,colour=Procedure,shape=Procedure))+
                      geom_point(data=death_pt,aes(Date,Patient,colour=Died,shape=Died,cex=1.2))+
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
                      geom_point(data=proc_pt,aes(Date,Patient,colour=Procedure,shape=Procedure))+
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

                  nodesfirst_pt <- proc_center_filt[(proc_center_filt$Patient) %in% pt_contact_net()$Patient.1 |(proc_center_filt$Patient) %in% pt_contact_net()$Patient.2 ,]


                  output$Net_pt <- renderPlot({res=256
                  gr_pt <- graph_from_data_frame(pt_contact_net(), directed=F,nodesfirst_pt)
                  set.seed(7)
                  ggraph(gr_pt,layout="fr")+
                    geom_edge_link(aes(edge_color=as.factor(unit),edge_linetype=as.factor(weight),label=substr(as.character(End),1,7)),
                                   edge_width=1,angle_calc= 'along',label_dodge = unit(2,'mm')) +
                    geom_node_point(size = 12.5) +
                    geom_node_point(size = 11.5,aes(colour = as.character(unit))) +
                    geom_node_text(aes(label = name))+
                    labs(color = "Sampling unit", edge_color = "Linking unit", edge_linetype="Link type")+
                    theme_void()+
                    scale_edge_linetype_manual(values=c("1"="solid","2"="twodash","3"="dotted"),labels = c("1"="Direct link (Same unit/same period)","2"="Hospital link (Same hospital/same period)","3"="Indirect link (Same unit/shifted period within 13 days)"))+
                    theme(legend.box = "vertical",legend.spacing = unit(1,"cm"))
                  })
                })
              })

              #############################################################
              # Networks
              # #############################################################
              if(min(as.Date(Department$Start)) != Inf){
                dir_link_net <- Department[,c("Patient.1","Patient.2","unit","End")]
                dir_link_net$weight <- 1

                hos_link_net <- hospital[,c("Patient.1","Patient.2","Hospital","End")]
                hos_link_net$weight <- 2
                names(hos_link_net)[names(hos_link_net)=="Hospital"]<- "unit"

                if(nrow(buffer_df)!=0){
                  buf_link_net <- buffer_df[,c("Patient.1","Patient.2","unit")]
                  buf_link_net$End <- " "
                  buf_link_net$weight <- 3
                }else{
                  buf_link_net <- NULL
                }
                netM <- rbind(dir_link_net,hos_link_net,buf_link_net)
                netM<- unique(subset(netM,weight==ave(weight,Patient.1,Patient.2,FUN=min)))
                netM$Patient.1 <-as.character(as.numeric(sub("\\D+","",netM$Patient.1)))
                netM$Patient.2 <-as.character(as.numeric(sub("\\D+","",netM$Patient.2)))
                netM <- netM %>% dplyr::group_by(Patient.1, Patient.2) %>% filter(End == max(End))

                observeEvent(input$linktypes,{

                  net<-reactive({
                    return(netM[netM$weight%in%input$linktypes,])
                  })


                  # Additional nodes
                  nodes <- no_pat[!no_pat %in% net()$Patient.1 & !no_pat %in% net()$Patient.2]
                  if(length(net())>=3){
                    #Calculate networks and set vertices/edges
                    ign <- network(net(),matrix.type="edgelist",directed=F,multiple=T)
                    edg <- network.edgecount(ign)
                    vertices <- ign %v% "vertex.names"
                    centers_net <- proc_center_filt[as.character(proc_center_filt$Patient) %in% vertices,]
                    centers_net$Patient <- as.character(centers_net$Patient)
                    centers_net<-centers_net[order(centers_net$Patient),]
                    set.edge.attribute(ign,"weight",as.character(net()$weight))
                    set.edge.attribute(ign,"lty",as.character(net()$lty))
                    set.vertex.attribute(ign,"samplecenter",as.character(centers_net$unit))

                    #For table
                    no_net <- proc_center_filt[(proc_center_filt$Patient) %in% nodes,]
                    no_net <- no_net[,c(1,3)]
                    no_net$Patient <- as.character(no_net$Patient)

                    output$net_text <-renderText({paste("Following patients are not linked to other patients by any EpiLinx criterion:")})
                    output$net_table <- DT::renderDataTable(datatable(no_net,rownames=F),filter="top",options = list(
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))))


                    nodesfirst <- proc_center_filt[(proc_center_filt$Patient) %in% net()$Patient.1|(proc_center_filt$Patient) %in% net()$Patient.2 ,]

                    ##### network Plot #####
                    output$NetworkPlot <- renderPlot({res=256
                    gr1 <- graph_from_data_frame(net(), directed=F,nodesfirst)
                    set.seed(7)
                    ggraph(gr1,layout="fr")+
                      geom_edge_link(aes(edge_color=as.factor(unit),edge_linetype=as.factor(weight),label=substr(as.character(End),1,7)),
                                     edge_width=1,angle_calc= 'along',label_dodge = unit(2,'mm')) +
                      geom_node_point(size = 12.5) +
                      geom_node_point(size = 11.5,aes(colour = as.character(unit))) +
                      geom_node_text(aes(label = name))+
                      labs(color = "Sampling unit", edge_color = "Linking unit", edge_linetype="Link type")+
                      theme_void()+
                      scale_edge_linetype_manual(values=c("1"="solid","2"="twodash","3"="dotted"),labels = c("1"="Direct link (Same unit/same period)","2"="Hospital link (Same hospital/same period)","3"="Indirect link (Same unit/shifted period within 13 days)"))+
                      theme(legend.box = "vertical",legend.spacing = unit(1,"cm"), legend.position="bottom")
                    })

                  }else{
                    output$nodirectlink <- renderText({paste("Your outbreak contains no direct epilinks")})
                  }

                })
              }

              #############################################################
              # Visit frequencies per department
              #############################################################

              vis_dep <- Department
              #overl_data[,c("unit","count")]%>%filter(count>1)
              vis_dep <- vis_dep %>% dplyr::group_by(unit)%>% dplyr::mutate(count = n())
              vis_dep <- vis_dep[,c("unit","count")]
              vis_dep$unit <- as.factor(vis_dep$unit)
              levels(vis_dep$unit) <- gsub(", ", "\n", levels(vis_dep$unit))
              #%>%filter(count>1)


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
              if(min(as.Date(Department$Start)) != Inf){
                epi_dates <- seq(min(as.Date(Department$Start)),max(as.Date(Department$End)),by="day")

                epi_mat <- matrix(data=NA, nrow=nrow(Department), ncol = length(epi_dates))
                colnames(epi_mat) <- epi_dates
                rownames(epi_mat) <- rownames(Department)


                for (i in  1:nrow(Department)){
                  tempDays=seq(min(as.Date(Department$Start[i])),max(as.Date(Department$End[i])),by="day")
                  colIndx=which(epi_dates %in% tempDays)
                  rowIndx =which(rownames(epi_mat) %in% rownames(Department)[i])
                  epi_mat[rowIndx,colIndx] = as.character(Department$unit[i])
                }
                colnames(epi_mat) <- format(as.Date(epi_dates, format= "%Y-%m-%d"), format= "%Y-%m-%d")
                epi_df <- melt(as.table(epi_mat))
                colnames(epi_df) <- c("Link", "Date", "unit")

                count<-epi_df %>% group_by(Date)%>%count(unit)
                count <- na.omit(count)
                count$Date <- as.Date(count$Date)

                ##### Epicurve plot #####
                output$EpiCurve <- renderPlot({
                  ggplot(count,aes(Date,n))+
                    geom_bar(stat="identity",aes(fill=unit))+
                    theme(axis.text.x = element_text(angle=90,size=10),panel.grid = element_blank())+
                    scale_y_continuous(breaks=seq(0,max(count$n)*2,by=1))+
                    labs(y="No of links")+
                    scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")
                })
              }else{
                output$nodirectlink <- renderText({paste("Your outbreak contains no direct epilinks")})
              }

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
           # }
          }
          output$noLink <- renderText({validate(
            need(!is.null(Department)
                 , 'Your outbreak contains no epilinks'))})
        }
      })
    })
  })
}
