source("https://bioconductor.org/biocLite.R")

if(!require(shiny)){install.packages("shiny")}
if(!require(rhandsontable)){install.packages("rhandsontable")}
if(!require(shinyFiles)){install.packages("shinyFiles")}
if(!require(flowCore)){biocLite("flowCore")}


ui=fluidPage(
  titlePanel("Change the Names of Channels and Parameters in .fcs Files"),
  helpText("This script will only change the names of parameters and channels in the .fcs file. All the original metadata will be preserved, i.e. filename, date and time of acquistion, instrument, operator and others.",
           br(),br(),
           "1. Please select the directory where the .fcs files are located before starting.",br(),
           "2. If error  reload the home page to restart.",br(),
           "3. Although your original files will not be altered or changed, ensure you have backed up your original files.",br(),
           "4. A new folder (NEW_Files) will be created within the selected directory, which will contain the new .fcs files (NEW_XXXXX.fcs).",
           br(),
           "5. Edit names directly on the table. You can also copy and paste from excel."),
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 shinyDirButton("dir2","Choose input directory","Choose Input Directory"),br(),br(),br(),
                 actionButton("save", "Apply NEW.name & NEW.desc to current file only"),br(),
                 helpText("It applies names to current file and go to the next file."),
                 actionButton("all", "Apply NEW.name & NEW.desc to all files"),
                 helpText(h5("Note:"),"Apply Changes to All Files: it will only execute if files in the folder have the same number of parameters, i.e. same experimental panel."),
                 br(),br(),
                 actionButton("stop", "Stop application")
    ),
    
    mainPanel(width = 9,
              fluidRow(
                column(width = 2, actionButton("back", "Previous File")),
                column(width = 2, actionButton("forward", "Next File")),
                column(width = 8, verbatimTextOutput("counts"))
                
              ),
              helpText("This is a excel-like table. Click on the cell and type to change or copy and paste from an excel file"),
              fluidRow(
                column(width = 10, rHandsontableOutput("flow"))
              ),
              br(),br())
  )
)

server=function(input,output){
  
  v<-reactiveValues(counts=0)
  
  setwd("~/")
  volumes=c(home=getwd())
  shinyDirChoose(input,id = "dir2",roots=volumes)
  
  dir<-reactive(parseDirPath(volumes,input$dir2))
  
  files<-reactive({
    setwd(dir())
    flist<-list.files(dir(),pattern = ".fcs|.FCS")
    withProgress(message = "Please wait...reading files",value = 0.85,{
      fls<-lapply(flist,read.FCS)
      fls})
    
  })
  
  observeEvent(input$forward,{
    if(!is.null(input$dir2)){
      v$counts<-v$counts+1}})
  
  observeEvent(input$back,{
    if (v$counts>0){v$counts<-v$counts-1
    }else{v$counts<-0}
  })
  
  observeEvent(input$dir2,{
    v$counts=0
  })
  
  output$flow<-renderRHandsontable({
    if(!is.null(input$dir2)){
      if ((v$counts<length(files()))){
        fs<-files()[[v$counts+1]]
        df<-as.data.frame.matrix(fs@parameters@data[1:2])
        df<-cbind(Old=df,NEW=df)
        rhandsontable(df,readOnly = T) %>% hot_col("NEW.name",readOnly=F) %>% hot_col("NEW.desc",readOnly=F) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      }else{
        v$counts=length(files())
        rhandsontable(data.frame(Old.names=NA,Old.desc=NA,NEW.name=NA,NEW.desc=NA))
      }
      
    }})
  
  output$counts<-renderPrint({
    if (is.null(input$dir2)){paste0("Please choose a directory to start.",v$counts)
    } else if ((v$counts<length(files()))) {
      cat("You are changing file: ",files()[[v$counts+1]]@description$`$FIL`,"\n","this file is: ",v$counts+1, " out of",length(files()),
          " in the folder:",dir(), v$counts)
    }else {cat("No more files to read.","\n","Click previous to continue navigating, stop application or choose another directory.",v$counts)}
    #v$counts
  })
  
  observeEvent(input$save,{
    if(!is.null(input$dir2)){
      dt<-hot_to_r(input$flow)
      if(!is.na(dt)){
        fs<-files()[[v$counts+1]]
        parindex<-grep("[0-9]S$",names(fs@description))
        par<-gsub("S","",names(fs@description)[grep("[0-9]S$",names(fs@description))])
        chnindex<-(grep("[0-9]N$",names(fs@description)))
        chnnew<-as.character(dt[,3])
        parnam<-as.character(dt[par,4])
        
        fs@description[chnindex]<-chnnew
        fs@description[parindex]<-parnam
        fs@parameters@data$name<-chnnew
        
        if(!is.null(colnames(fs@description$SPILL))){
          colnames(fs@description$SPILL)<-chnnew[row.names(dt) %in% par]
        }
        
        dir.create(file.path(dir(),"NEW_files"))
        setwd(file.path(dir(),"NEW_files"))
        write.FCS(fs,filename = paste0("NEW_",fs@description$`$FIL`))
        v$counts<-v$counts+1
      }
    }
  })
  
  observeEvent(input$all,{
    if(!is.null(input$dir2)){
      withProgress(message = "Wait...Files being processed",value = 0.75,{
        dt<-hot_to_r(input$flow)
        if(!is.na(dt)){
          lapply(files(),function(x){
            parindex<-grep("[0-9]S$",names(x@description))
            par<-gsub("S","",names(x@description)[grep("[0-9]S$",names(x@description))])
            chnindex<-(grep("[0-9]N$",names(x@description)))
            chnnew<-as.character(dt[,3])
            parnam<-as.character(dt[par,4])
            
            x@description[chnindex]<-chnnew
            x@description[parindex]<-parnam
            x@parameters@data$name<-chnnew
            
            if(!is.null(colnames(x@description$SPILL))){
              colnames(x@description$SPILL)<-chnnew[row.names(dt) %in% par]
            }
            dir.create(file.path(dir(),"NEW_files"))
            setwd(file.path(dir(),"NEW_files"))
            write.FCS(x,filename = paste0("NEW_",x@description$`$FIL`))
            v$counts=length(files())
          })
        }
      })
    }
  })
  
  observeEvent(input$stop,{
    stopApp()
  })
}

shinyApp(ui=ui,server=server)