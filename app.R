require(shiny)
library(xlsx)
source("schedule_alg_function.R")

ui = fluidPage(
  titlePanel("DSST Student Scheduler - Prototype v0.1"),
  
  tags$head(tags$style(
    HTML(
      "
      .errortext {
      font-size: 1em;
      color: red;
      padding-top: 1em;
      padding-bottom: 1em;
      }
      "
    )
    )),
  
  sidebarLayout(
    sidebarPanel(
      h4('Why should I use this app?'),
      p('Because creating a class schedule is a pain.'),
      p(
        'This app reduces that pain by helping you to quickly schedule students into
        classes that they need for an upcoming school year and rapidly determine if
        the schedule that you have created will work. If not, you can rework the schedule and reupload to try again.'
      ),
      p(
        strong('Hey!'),
        tags$i(
          ' Currently, this app prototype is only coded to run for 10th grade at DSST Stapleton,
          though it may work for other schools/grades. No promises...'
        )
        ),
      br(),
      
      
      h5('Step 1: Create an Excel file with student and scheduling data'),
      
      helpText(
        'Hey! This app requires an excel (xls or xlsx) file. See the link in the directions to the right for an example file.'
      ),
      # checkboxInput(inputId = 'dataheader',label = 'Yes',value = TRUE),
      br(),
      h5('Step 2: Upload the  file you just created'),
      
      fileInput(inputId = 'datafile', label = ''),
      uiOutput('worksheets'),
      
      h5('Step 3: Run the Student Scheduling Algorithm'),
     

        actionButton(
          inputId = "runscheduler",
          label = "Run Scheduler",
          icon = icon("mail-forward")
        )
      ,
    
      br(),
      br(),
      
      
      h5('Step 4: Download the data'),
      # radioButtons(inputId = 'outputformat',
      #              label = 'In what format would you like to download the data?',
      #              choices = c('Excel' = 'excel', 'CSV' = 'csv')),
      downloadButton('downloadData', 'Download Schedule Data')
      ),
    
    mainPanel(
      #textOutput('testing'),
      br(),
      h4('How should I use this web app?'),
      p(
        strong('Step 1:'),
        'Using the example file in the link at the bottom of these instructions, create a file that contains
        the necessary student data, class/section data, and any paired sections (i.e., classes that a student has to take together).
        Crucially, please do',
        strong('NOT'),
        'include any personally identifying student data (e.g. names, phone nums, or whatever). If you do,',
        a(
          'I will find you, and I will kill you.',
          href = 'https://media.tenor.com/images/a38b7ac02a752bd50350ca02a25faa1d/tenor.gif',
          target = '_blank'
        ),
        'You should, however, include an anonymized Student ID field as shown in the example file.'
      ),
      p('There are three tabs in the example file. Please ensure that you:'),
      tags$ol(
        tags$li("keep the tabs named as is,"),
        tags$li("keep the fields listed in each tab,"),
        tags$li(
          "include valid (i.e., unique) student IDs and section IDs (which you can create yourself) for the relevant
          tabs. The underlying code needs these fields in order to work properly."
        ),
        tags$li(
          "do NOT upload any personally identifiable student data. Remember ",
          a('what happens,',
            href = 'https://media.tenor.com/images/a38b7ac02a752bd50350ca02a25faa1d/tenor.gif',
            target = '_blank'),
          ' if you do.'
        )
        ),
      p(
        strong('Step 2:'),
        'Upload the data file you have created with the upload button to the left.'
      ),
      p(
        strong('Step 3:'),
        'Click "Run Scheduler" to have the code run to attempt to schedule all students.
        This may take several minutes depending on the number of students to process. A progress
        bar will appear along the top of the page that shows the percentage of URLs processed.'
      ),
      p(
        strong('Step 4:'),
        'After the students are assigned the file can be downloaded in .xlsx
        format by clicking "Download." Students with subjects/periods that could not be matched
        will be listed in the first few tabs. If this list is short, then you are probably only
        a few manual tweaks away from having this done. If there are lots of records on these tabs,
        you will probably want to re-work your schedule, reupload the file and try again.'
      ),
      a(
        'Click here to download example schedule file',
        href = 'https://github.com/jrstevenson3/dsst_scheduler_app/raw/master/example.xlsx',
        
        alt = 'Link to public Dropbox account with test files',
        target = '_blank'
      ),
      br(),
      br(),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       h4('I am now scheduling students. Please sit 
                         tight for about 2 minutes until this cool circle disappears, then you can click the download button to the left...'),
                       tags$img(src="loading_circle.gif")
      )#, 
      # uiOutput('errortext'),
      # 
      # tabsetPanel(
      #   id = "datatabs",
      #   tabPanel(
      #     title = "Data Summary",
      #     value = 'datasumtab',
      #     verbatimTextOutput("text")
      #   ),
      #   tabPanel(title = "Input Data", value = 'inputdatatab', tableOutput('inputdata')),
      #   tabPanel(
      #     title = "Output Data",
      #     value = 'outputdatatab',
      #     tableOutput('outputdata')
      #   )
      # )
      )
  )
    )

######################################################
# James McCammon
# 28 December 2014
# Get Full URLs function
# Version 4
######################################################

# Load libraries and source functions
require(shiny)
#require(RCurl)
#require(XML)
#require(XLConnect)
#source("functions.R")

# Define server instance
server = function(input, output, session) {
  #
  # # Initialize reactive values
  # values = reactiveValues()
  #
  # # Function to get file path of uploaded file
  filepath = reactive({
    file = input$datafile
    if (is.null(file))
      return()
    return(file$datapath)
  })
  
  
  student_data = reactive({
    if (is.null(filepath()))
      return()
    return({
      read.xlsx2(
        file = filepath(),
        sheetName = "student_data",
        stringsAsFactors = F
      )
    })
  })
  schedule_data = reactive({
    if (is.null(filepath()))
      return()
    return({
      read.xlsx2(
        file = filepath(),
        sheetName = "schedule_data",
        stringsAsFactors = F
      )
    })
  })
  paired_sections = reactive({
    if (is.null(file()))
      return()
    return({
      read.xlsx2(
        file = filepath(),
        sheetName = "paired_sections",
        stringsAsFactors = F
      )
    })
  })
  #
  # # If the uploaded file is Excel create a dropdown menu so the user can select the
  # # worksheet with the relevant data.
  # observe({
  #   if(is.null(input$dynamicinput)) {
  #     if(input$inputformat == 'excel' & !is.null(filepath())) {
  #       possibleerror = try(values$workbook <- loadWorkbook(filepath()), silent = TRUE)
  #       if(class(possibleerror) == 'try-error') { seterror('excelloaderror'); return() }
  #       sheetNames = getSheets(values$workbook)
  #       output$worksheets = renderUI({
  #         selectInput(inputId = "dynamicinput",
  #                     label = "Select Worksheet",
  #                     choices = c(sheetNames,'Select' = 'select'),
  #                     selected = 'select')
  #       })
  #     }
  #   }
  # })
  #
  # # Create a table with summary data of the file
  # output$inputsum = renderTable({
  #   if(is.null(input$datafile)) return()
  #   return(input$datafile)
  # })
  #
  # # Create a table with the first 10 rows of the input data
  # output$inputdata = renderTable({
  #   if(is.null(input$datafile) | is.na(input$datacol)) return()
  #   clearerror()
  #   # Load the relvant data depending on the file type. If the specified file type doesn't
  #   # match the loaded file throw an error.
  #   inputdata = switch(input$inputformat,
  #                      'excel' = {
  #                        if(is.null(input$dynamicinput)) return()
  #                        if(input$inputformat == 'excel' & input$dynamicinput == 'select') return()
  #                        tryCatch({ readWorksheet(values$workbook, sheet = input$dynamicinput, header = input$dataheader)
  #                        }, error = function(e) { seterror('excelreaderror'); return() })
  #                      },
  #                      'csv' = {
  #                        tryCatch({ read.csv(file = filepath(), header = input$dataheader, stringsAsFactors = FALSE)
  #                        }, error = function(e) { seterror('csvloaderror'); return() })
  #                      })
  #   # Take the data and get out the first 10 rows. If there is an error it's likely because
  #   # the specified worksheet or data column has no data. Tell the user this if an error occurs.
  #   values$inputdata = inputdata
  #   possibleerror = try(inputdata <- inputdata[[input$datacol]], silent = TRUE)
  #   if(class(possibleerror) == 'try-error') { seterror('subscripterror'); return() }
  #   inputdata = as.data.frame(inputdata[1:10])
  #   names(inputdata)[1] = "short_url_preview"
  #   return(inputdata)
  # })
  #
  # # When the users pushes the "Get Full URLs" button get the URLs by calling the getFWLinks function
  # # found in Functions.R. If there is no inpupt data let the user know they forgot to load it.
  # observe({
  #   input$runscheduler
  #   if(input$runscheduler == 0) return()
  #   else {
  #
  #     # updateTabsetPanel(session, inputId = "datatabs", selected = "outputdatatab")
  #     # output$outputdata = renderTable({
  #     #   possibleerror = try(values$output <- isolate(getFwLinks(as.data.frame(values$inputdata), input$datacol)), silent = TRUE)
  #     #   if(class(possibleerror) == 'try-error') { seterror('nodataerror'); return() }
  #     #   return(as.data.frame(values$output[1:10,]))
  #     # })
  #   }
  # })
  
  observeEvent(input$runscheduler, {
    withCallingHandlers({
     data_out = scheduler_10(
        student_data_var = student_data(),
        schedule_data_var = schedule_data(),
        paired_sections_var = paired_sections(),
        section_capacity = 35
      ) 
     output$downloadData <- downloadHandler(filename = "student_data_out.xlsx", 
                                            content = function(file) {
                                              #student_free_subjects
                                              write.xlsx2(data_out$student_free_subjects, file = file, row.names = F, sheetName = "student_free_subjects", append = F)
                                              # student_free_periods
                                              write.xlsx2(data_out$student_free_periods, file = file, row.names = F, sheetName = "student_free_periods", append = T)
                                              #
                                              write.xlsx2(data_out$student_schedule_out, file = file, row.names = F, sheetName = "student_schedule_out", append = T)
                                              #                                               # 
                                               write.xlsx2(data_out$schedule_data, file = file, row.names = F, sheetName = "schedule_data", append = T)
                                               
                                            })
                                            
     } ,
      message = function(m)
        output$text <- renderPrint(m$message)
    )
  })
  #
  # # When the user selects "Download Full URLs" download them in the specified format.
  # # Note the file.rename function is used to handle the temporary filepath created by Shiny.
  # output$downloadlinks = downloadHandler(
  #   filename = function() {
  #     filename = switch(input$outputformat,
  #                       'excel' = 'Full_URLs.xlsx',
  #                       'csv' = 'Full_URLs.csv'
  #     )
  #   },
  #   content = function(file) {
  #     if(input$outputformat == 'csv') {
  #       write.csv(values$output, 'temp.csv', row.names = FALSE)
  #       file.rename('temp.csv', file)
  #     }
  #     else {
  #       outputdata = loadWorkbook('temp.xlsx', create = TRUE)
  #       createSheet(object = outputdata, name = 'Full_URLs')
  #       writeWorksheet(outputdata, data = values$output, sheet = 'Full_URLs')
  #       saveWorkbook(outputdata, 'temp.xlsx')
  #       file.rename('temp.xlsx', file)
  #     }
  #   }
  # )
  #
  # # Create a function to ouput various error messages
  # seterror = function(error) {
  #   errormessage = switch(error,
  #                         'excelloaderror' =  'Error: There was an error loading the file.
  #                                        Are you sure it is an Excel file? Try changing
  #                                        your selection to CSV.',
  #                         'excelreaderror' =  'Error: The workbook loaded, but there was
  #                                        an error reading the specified worksheet',
  #                         'csvloaderror'   =  'Error: There was an error loading the file.
  #                                        Are you sure it is a csv file?',
  #                         'fullurlserror'  =  'Error: There was a problem getting the full URLs.
  #                                        Are you sure you selected the correct data column?',
  #                         'subscripterror' =  'Error: There does not seem to be any data there.',
  #                         'nodataerror'    =  'Error: Did you forget to upload data?')
  #
  #   output$errortext = renderUI({
  #     tags$div(class = "errortext", checked = NA,
  #              tags$p(errormessage))
  #   })
  # }
  #
  # # Define a function to clear error messages.
  # clearerror = function() {
  #   output$errortext = renderUI({
  #     p('')
  #   })
  # }
}

shinyApp(ui, server)