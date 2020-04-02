library(RHRV)
library(shiny)
library(plotly)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading cleaned IBI Files - with header IBI and times in milliseconds, one per row"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"),
      plotlyOutput("physioIBIplot"),
      
      tableOutput('HRVtable'),
      plotOutput("powerBandPlot"),
      plotOutput("poincarePlot")
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  #definition variables functions
  # print_IBI_data_error_msg <- function() {
  #   msg <- paste("IBI Data not valid for selection. (")
  #   
  #   mail <- all_accounts[[1]]
  #   if (!is.null(input$emailSelect)) {
  #     mail <- input$emailSelect
  #   }
  #   msg <- paste(msg, mail, sep='')
  #   
  #   if (!is.null(pid_name)) {
  #     msg <- paste(msg, ", Participant ", pid_name, sep='')
  #   }
  #   msg <- paste(msg, ").", sep='')
  #   
  # }  
  # 
  
  print_nodata_msg <- function() {
    msg <- paste("No data to show for ", sep='')
      }
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        dfIBI <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(dfIBI)
             
             )
      
    }
    else {
      return(dfIBI)
    }
    
  })
  
if(exists(dfIBI)){
  eHRV <<- ""
  tryCatch({
    names(dfIBI)[1] <- "IBI"
    
    # ##### HRV stuff -------
    tsIBI<<-as.data.frame(cumsum(c(0, dfIBI[2:nrow(dfIBI),]$IBI/1000)))
    names(tsIBI)<<-c('beats')
    
    # #need to write data back to file as I couldn't figure out how to simply inject it into the data structure, the file should be safe to delete after this
    #beatAscii <- write.table(tsIBI$beats, file = "", sep = ",", qmethod = "double", row.names = FALSE, col.names = FALSE)
    # #create data structure
    
    hrv.data  <<- CreateHRVData()
    # #load the beat data
    hrv.data <<- LoadBeatString(hrv.data, tsIBI$beats)
    # #make a non-interpolated plot of the heart rate
    hrv.data <<- BuildNIHR(hrv.data)
    hrv.data <<- FilterNIHR(hrv.data)
    hrv.data <<- InterpolateNIHR(hrv.data, freqhr = 4)
    # 
    # PlotNIHR(hrv.data, main = "niHR",Tags = "all")
    # 
    # #Create a time analysis, the values here are the same as the default  
    hrv.data <<- CreateTimeAnalysis(hrv.data, size = floor(max(dfphysio$TimeLine))/2, interval = 7.8125)
    # 
    # #Do the frequency analysis
    hrv.data <<- CreateFreqAnalysis(hrv.data)
    
    hrv.data <<-
      CalculatePowerBand(hrv.data , indexFreqAnalysis = 1,
                         size = 100, shift = 2, type = "fourier",
                         ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                         LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )        
    
    # #create nonlinear analysis
    hrv.data<<-CreateNonLinearAnalysis(hrv.data)
    if (!is.na(hrv.data$TimeAnalysis[[1]]$SDANN)) {
      hrv.data<<-NonlinearityTests(hrv.data)
      hrv.data<<-PoincarePlot(hrv.data,indexNonLinearAnalysis = 1,timeLag = 1,confidenceEstimation = TRUE,confidence = 0.9, doPlot = TRUE)

      
      
      output$physioIBIplot <- renderPlotly({
        validate( need(nrow(dfIBI) > 0, print_nodata_msg()))
        validate( need(eHRV == "", "eHRV - not required"))
        
        
        IBIplot = ggplot(dfIBI,aes(x=TimeLine,y=IBI))+geom_point()+ylab("inter-beat interval in ms")+xlab("time line in seconds")+geom_line()+theme_bw() + scale_y_continuous(breaks=seq(0,max(dfIBI$IBI),200))+scale_x_continuous(breaks=seq(0,max(dfIBI$TimeLine),1))+ expand_limits(x = 0, y = 0)+geom_hline(yintercept=300,color='red')+geom_hline(yintercept=2000, color='green')
        ggplotly(p = IBIplot) %>% config(scrollZoom = TRUE)
      })      
      
      
      output$HRVtable <- renderTable({
        validate( need(nrow(dfIBI) > 0, print_nodata_msg()))
        validate( need(eHRV == "", print_nodata_msg()))
        # #Put all the values into a single variable each for easier display  in a table
        
        SDNN <- round(hrv.data$TimeAnalysis[[1]]$SDNN,1)
        pNN50 <- round(hrv.data$TimeAnalysis[[1]]$pNN50,1)
        rMSSD <- round(hrv.data$TimeAnalysis[[1]]$rMSSD,1)
        avgLF <- round(mean(hrv.data$FreqAnalysis[[1]]$LF),1)
        avgHF <- round(mean(hrv.data$FreqAnalysis[[1]]$HF),1)
        avgLFHF <- round(avgLF/avgHF,1)
        SD1<- ifelse(!is.na(hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD1), round(hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD1,1),NA)
        SD2<- ifelse(!is.na(hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD1), round(hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD2,1), NA)
        
        types<-c("Time domain","","","Frequency domain","","","Non-linear","")
        measures<-c("SDNN","pNN50","rMSSD","avg LF","avg HF","avg LF/HF","SD1","SD2")
        mvalues<-c(SDNN,pNN50,rMSSD,avgLF,avgHF,avgLFHF,SD1,SD2)
        dfHRV<<-data.frame(cbind(types,measures,mvalues))
        dfHRV$types<-as.character(dfHRV$types)
        dfHRV$measures<-as.character(dfHRV$measures)
        dfHRV
      })
      
      
      output$powerBandPlot <<- renderPlot({
        validate( need(nrow(dfphysio) > 0, print_nodata_msg()))
        validate( need(eHRV == "", print_IBI_data_error_msg()))
        dev.control("enable")
        
        # ###################
        # #Plots the powerband calculations from above, ymax can be changed to change the y-max value on ULF VLF LF and HF graphs while ymaxratio changes the max y value on the LF/HF graph.
        # #Creates a power bands plot to see the values of LF/HF etc. over time
        
        powerBandPlotX<<-PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 1200, ymaxratio = 16)
        powerBandPlotXRec<<-recordPlot()
        
        replayPlot(powerBandPlotXRec)
        dev.off()
      })
      
      
      output$poincarePlot <- renderPlot({
        validate( need(nrow(dfphysio) > 0, print_nodata_msg()))
        validate( need(eHRV == "", print_IBI_data_error_msg()))
        
        dev.control("enable")
        poincareRecordplot<<-recordPlot()        
        
        replayPlot(poincareRecordplot)
        dev.off()
      })
      
      
      
          }
  }, 
  

  error=function(cond) {
    stop(safeError(e))
  })

}
    
  #server function  
}

# Create Shiny app ----
shinyApp(ui, server)

