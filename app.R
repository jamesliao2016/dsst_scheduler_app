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
     
      h5('Step 1: Create an Excel file with student and scheduling data'),
      
      helpText(
        'Hey! This app requires an excel (xls or xlsx) file. See the link in the directions to the right for an example file.'
      ),
      
      h5('Step 2: Upload the  file you just created'),
      
      fileInput(inputId = 'datafile', label = ''),
      
      
      h5('Step 3: Run the Student Scheduling Algorithm'),
     

        actionButton(
          inputId = "runscheduler",
          label = "Run Scheduler",
          icon = icon("mail-forward")
        )
      ,
    
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
      conditionalPanel(condition="$('html').hasClass('shiny-busy') && input.runscheduler > 0",
                       h4("I am now scheduling students. Please sit 
                         tight for about 2 minutes until this little circle of algorithmic lovin' disappears, 
                          then you can click the download button to the left in step 4..."),
                       tags$img(src="loading_circle.gif")),
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
      )
  
      )
  )
    )

# Define server instance
server = function(input, output, session) {

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
  
  
}

shinyApp(ui, server)