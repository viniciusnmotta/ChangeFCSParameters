# process file by file lowering memory usage
# read only 100 events while setting new parameters
# original file name is used
# $FIL is not available in some mass cytometry FCS


if (getRversion() < "3.5.0") {
  
  stop("R >= 3.5.0 is required!")

  # installation based on biocLite  
  source("https://bioconductor.org/biocLite.R")
  
  if(!require(shiny)){install.packages("shiny")}
  if(!require(rhandsontable)){install.packages("rhandsontable")}
  if(!require(shinyFiles)){install.packages("shinyFiles")}
  if(!require(flowCore)){biocLite("flowCore")}

} else {
  
  # installation based on BiocManager
  if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")

  if(!require(shiny)) BiocManager::install("shiny")
  if(!require(rhandsontable)) BiocManager::install("rhandsontable")
  if(!require(shinyFiles)) BiocManager::install("shinyFiles")
  if(!require(flowCore)) BiocManager::install("flowCore")
  
}

# load required libraries
library("shiny")
library("rhandsontable")
library("shinyFiles")
library("flowCore")

message(paste0("flowCore v", packageVersion("flowCore")))


# overload flowCore IO functions
my.read.FCS = function(filename, which.lines = NULL) {
  read.FCS(filename = filename, transformation = FALSE, 
           which.lines = which.lines, min.limit = NULL,
           truncate_max_range = FALSE, emptyValue = TRUE)
}

# main function
ChangeFCSParameters <- function(ff, dt, dir) {

  # fileName or flowFrame
  if (class(ff) == "character") {
    ofile = ifile = ff
    ff = my.read.FCS(ifile)  # read the FCS file
  } else if( class(ff) == "flowFrame") {
    ofile = ff@description$`$FIL`  # guess output file
  }
  
  # check the parameter translation table match the parameters
  stopifnot(nrow(dt) == nrow(ff@parameters@data))
  stopifnot(dt[,1:2] == ff@parameters@data[,1:2] | is.na(dt[,1:2]) | is.na(ff@parameters@data[,1:2]))

  # Following code is not used anymore with flowCore >= 1.48
  # # replace parameters in the description
  # chnindex <- grep("[0-9]N$",names(ff@description))
  # parindex <- grep("[0-9]S$",names(ff@description))
  # par <- gsub("S","",names(ff@description)[parindex])
  # # N and S are considered in the same order
  # # the order is given by the order of $PxS in @description
  # ff@description[chnindex] <- as.character(dt[par,3])
  # ff@description[parindex] <- as.character(dt[par,4])
  
  # replace parameters in the meta data
  # dt is in the same order as @data by construction
  ff@parameters@data$name  <- as.character(dt[,3])  # $PxN
  ff@parameters@data$desc  <- as.character(dt[,4])  # $PxS
  
  # change in SPILL matrix
  if(!is.null(colnames(ff@description$SPILL))) {
    idx = match(colnames(ff@description$SPILL), dt[,1])
    colnames(ff@description$SPILL) <- dt[idx,3]
    message("SPILL changed")
  } else if(!is.null(ff@description$SPILLOVER)) {
    idx = match(colnames(ff@description$SPILLOVER), dt[,1])
    colnames(ff@description$SPILLOVER) <- dt[idx,3]
    message("SPILLOVER changed")
  } else if(!is.null(fcs@description$COMP)) {
    idx = match(colnames(ff@description$COMP), dt[,1])
    colnames(ff@description$COM) <- dt[idx,3]
    message("COMP changed")
  }

  # return a flowFrame
  if (is.null(dir)) return(ff)
  # or write a FCS file
  odir <- file.path(dir, "NEW_files")
  if (!dir.exists(odir)) dir.create(odir)
  ofile = file.path(odir, paste0("NEW_", ofile))
  write.FCS(ff, filename = ofile)
  message(ifile, " changed")
}

# User Interface

ui = fluidPage(
  titlePanel("Change the Names of Channels and Parameters in .fcs Files"),
  helpText(
    "This script will only change the names of parameters and channels ",
    "in the .fcs file. All the original metadata will be preserved, i.e. filename, ",
    "date and time of acquistion, instrument, operator and others.",
    br(),br(),
    "1. Please select the directory where the .fcs files are located before starting.",br(),
    "2. If error  reload the home page to restart.",br(),
    "3. Although your original files will not be altered or changed, ",
    "ensure you have backed up your original files.",br(),
    "4. A new folder (NEW_Files) will be created within the selected directory, ",
    "which will contain the new .fcs files (NEW_XXXXX.fcs).",br(),
    "5. Edit names directly on the table. You can also copy and paste from excel."),
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      shinyDirButton("dir2","Choose input directory","Choose Input Directory"),
      helpText("Select a directory with FCS files to process."),
      br(),br(),br(),
      actionButton("save", "Apply to CURRENT file"),
      helpText("Apply NEW.name & NEW.desc to current file and go to the next file."),
      br(),br(),
      actionButton("all", "Apply to ALL files"),
      helpText(
        "Apply Changes to All Files. It will break if a file in the directory",
        "does not have the same parameters as in the table."),
      br(),br(),
      actionButton("stop", "Stop application")
    ),
    
    mainPanel(
      width = 8,
      fluidRow(
        column(width = 5, actionButton("back", "Previous File")),
        column(width = 5, actionButton("forward", "Next File"))
      ),
      fluidRow(
        column(width = 10, verbatimTextOutput("counts"))
      ),
      helpText(
        "This is a excel-like table. Click on the cell and type to change or ",
        "copy and paste from an excel file"),
      fluidRow(
        column(width = 10, rHandsontableOutput("flow"))
      )
    )
  )
)

# Server side

server = function(input,output){
  
  v<-reactiveValues(counts=0)
  
  setwd("~")
  volumes = c(C="C:",D="D:",E="E:",F="F:",home=getwd())
  shinyDirChoose(input, id = "dir2", roots = volumes)

  # store path of FCS files
  dirFCS<-reactiveVal(value = NULL)

  # handle chooseDir  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$dir2
    },
    handlerExpr = {
      if (exists("root", input$dir2)) {
        cat("\ninput$directory value:\n\n")
        dirFCS(parseDirPath(volumes,input$dir2))
        v$counts <- 0
      }
    }
  )
  
  # update flowFrame from FCS
  # store file name and flowframe of 100 events
  files<-reactive({
    if (is.null(dirFCS())) return(NULL)
    setwd(dirFCS())
    flist<-list.files(dirFCS(),pattern = ".fcs|.FCS")
    withProgress(message = "Please wait...reading files",value = 0.85,{
      fls<-lapply(flist, function(fileName) 
        list(
          fileName = fileName,
          ffHeader = my.read.FCS(filename = fileName, which.lines = 1:100)
        )
      )
      fls
    })
  })
  
  # next button
  observeEvent(input$forward,{
    if (is.null(files())) return(NULL)
    v$counts<-v$counts+1
  })
  
  # previous button
  observeEvent(input$back,{
    if (is.null(files())) return(NULL)
    if (v$counts>0) v$counts<-v$counts-1 else v$counts<-0
  })
  
  # interactive table
  output$flow<-renderRHandsontable({
    if (is.null(files())) return(NULL)
    if (v$counts<length(files())){
      # get parameters from flowFrame
      fs<-files()[[v$counts+1]][["ffHeader"]]
      df<-as.data.frame.matrix(fs@parameters@data[,1:2])
      # append prefilled columns
      df<-cbind(Old=df,NEW=df)
      rhandsontable(df,readOnly = T) %>% 
        hot_col("NEW.name",readOnly=F) %>% 
        hot_col("NEW.desc",readOnly=F) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }else{
      v$counts <- length(files())
      rhandsontable(data.frame(Old.names=NA,Old.desc=NA,NEW.name=NA,NEW.desc=NA))
    }
  })
  
  output$counts<-renderPrint({
    message(v$counts)
    if (is.null(files())){
      cat("Please choose a directory to start.")
    } else if ((v$counts<length(files()))) {
      cat("You are changing file: ", files()[[v$counts+1]][["fileName"]], "\n",
          "this file is: " ,v$counts+1, " out of ", length(files()),
          " in the folder: ", dirFCS(), sep = '')
    } else {
      cat("No more files to read.\n",
          "Click previous to continue navigating, stop application or ", 
          "choose another directory.")
    }
  })
  
  # save button
  observeEvent(input$save,{
    if (is.null(files())) return(NULL)
    dt <- hot_to_r(input$flow)
    if (is.null(dt)) return(NULL)
    fs <- files()[[v$counts+1]][["fileName"]]
    ChangeFCSParameters(fs, dt, dirFCS())
    v$counts<-v$counts+1
  })
  
  # process all files
  observeEvent(input$all,{
    if (is.null(files())) return(NULL)
    dt <- hot_to_r(input$flow)
    if (is.null(dt)) return(NULL)
    withProgress(message = "Wait...Files being processed", value = 0.75, {
      lapply(files(), function(fs) ChangeFCSParameters(fs[["fileName"]], dt, dirFCS()))
      v$counts=length(files())
    })
  })
  
  # end this app
  observeEvent(input$stop,{
    stopApp()
  })
}

# shiny application
shinyApp(ui=ui,server=server)
