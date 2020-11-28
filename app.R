library(shiny)
library(shinyWidgets)
library(sqldf)

ui <- navbarPage("CUNYHub",
    tabPanel("Fun Facts",
             sidebarPanel(
               selectInput("Schools_fun", "Choose a school district",
                           choices = StudentInfo$Campus),
               h4(textOutput("finaid")),
               h4(textOutput("size")),
               h4(textOutput("type")),
               h4(textOutput("ageall")),
               h4(textOutput("agefull")),
               h4(textOutput("part")),
               h4(textOutput("under")),
               h4(textOutput("grad")),
               h4(textOutput("ratio")),
               h4(textOutput("sat")),
               h4(textOutput("adtype")),
               h4(textOutput("adprnt"))
             ),
             
             mainPanel(
               plotOutput("gender"),
               plotOutput("race"),
               plotOutput("state")
             )
    ),
    tabPanel("Contact Info",
             sidebarPanel(
               selectInput("Schools_cont", "Choose a school district",
                           choices = CUNY_Location$Campus)
             ),
             mainPanel(
               h3(textOutput("phone")),
               h3(uiOutput("website")),
               h3(textOutput("address")),
               leafletOutput("map")
             )
    ),
    tabPanel("School Expenses",
             sidebarPanel(
               selectInput("Schools_exp", "Choose a school district",
                           choices = CUNY_Location$Campus)
             ),
             mainPanel(
               h4(textOutput("resident")),
               h4(textOutput("transp")),
               h4(textOutput("books")),
               h4(textOutput("lunch")),
               h4(textOutput("personal")),
               h4(textOutput("room")),
               h4(textOutput("credit"))
             )
      
    ),
    navbarMenu("Courses and Majors",
               tabPanel("Courses",
                        sidebarPanel(
                          selectInput("Schools_courses",
                                      "Choose a school district",
                                      choices = StudentInfo$Campus),
                          selectInput("Schools_depts",
                                      "Choose a department",
                                      choices = sort(unique(departments$dept_name)))
                        ),
                        mainPanel(
                          tableOutput("courses")
                          )
                        
                        ),
               
               tabPanel("Majors")
               
               )
    

)


# ----------------------------------------------------------
# ----------------------------------------------------------
# ----------------------------------------------------------


server <- function(input, output, session) {

  datain <- reactive({
    switch(input$Schools_fun,
           "Baruch College" = 1,
           "Borough of Manhattan Community College" = 2,
           "Bronx Community College" = 3,
           "Brooklyn College" = 4,
           "College of Staten Island" = 5,
           "Guttman Community College" = 6,
           "Hostos Community College" = 7,
           "Hunter College" = 8,
           "John Jay College of Criminal Justice" = 9,
           "Kingsborough Community College" = 10,
           "LaGuardia Community College" = 11,
           "Lehman College" = 12,
           "Medgar Evers College" = 13,
           "New York City College of Technology" = 14,
           "Queensborough Community College" = 15,
           "Queens College" = 16,
           "The City College of New York" = 17,
           "York College" = 18
           )
  })
  
  # ----------------------------------------------------------
  
  contactin <- reactive({
    switch(input$Schools_cont,
           "Baruch College" = 1,
           "Borough of Manhattan Community College" = 2,
           "Bronx Community College" = 3,
           "Brooklyn College" = 4,
           "College of Staten Island" = 5,
           "Guttman Community College" = 6,
           "Hostos Community College" = 7,
           "Hunter College" = 8,
           "John Jay College of Criminal Justice" = 9,
           "Kingsborough Community College" = 10,
           "LaGuardia Community College" = 11,
           "Lehman College" = 12,
           "Medgar Evers College" = 13,
           "New York City College of Technology" = 14,
           "Queensborough Community College" = 15,
           "Queens College" = 16,
           "The City College of New York" = 17,
           "York College" = 18
    )
  })
  
  # ----------------------------------------------------------  
  
  expin <- reactive({
    switch(input$Schools_exp,
           "Baruch College" = 1,
           "Borough of Manhattan Community College" = 2,
           "Bronx Community College" = 3,
           "Brooklyn College" = 4,
           "College of Staten Island" = 5,
           "Guttman Community College" = 6,
           "Hostos Community College" = 7,
           "Hunter College" = 8,
           "John Jay College of Criminal Justice" = 9,
           "Kingsborough Community College" = 10,
           "LaGuardia Community College" = 11,
           "Lehman College" = 12,
           "Medgar Evers College" = 13,
           "New York City College of Technology" = 14,
           "Queensborough Community College" = 15,
           "Queens College" = 16,
           "The City College of New York" = 17,
           "York College" = 18
    )
  })
  
  # ----------------------------------------------------------  
  
  crsin <- reactive({
    switch(input$Schools_courses,
           "Baruch College" = 1,
           "Borough of Manhattan Community College" = 2,
           "Bronx Community College" = 3,
           "Brooklyn College" = 4,
           "College of Staten Island" = 5,
           "Guttman Community College" = 6,
           "Hostos Community College" = 7,
           "Hunter College" = 8,
           "John Jay College of Criminal Justice" = 9,
           "Kingsborough Community College" = 10,
           "LaGuardia Community College" = 11,
           "Lehman College" = 12,
           "Medgar Evers College" = 13,
           "New York City College of Technology" = 14,
           "Queensborough Community College" = 15,
           "Queens College" = 16,
           "The City College of New York" = 17,
           "York College" = 18
    )
  })
  
  # ---------------------------------------------------------- 
  
  output$gender <- renderPlot({
    nums_g <- c(StudentInfo$`Women (%)`[datain()],
              StudentInfo$`Men (%)`[datain()])
    labs_g <- c(paste("Women ", nums_g[1], "%", sep = ""),
                paste("Men", nums_g[2] ,"%", sep = ""))
    pie(nums_g, labels = labs_g, main = "Pie Chart for Student Gender")
  })
  
  # ----------------------------------------------------------
  
  output$race <- renderPlot({
    nums_r <- c(StudentInfo$`Hispanic/Latino(%)`[datain()],
                StudentInfo$`White(%)`[datain()],                
                StudentInfo$`Black/African American(%)`[datain()]
                )
    labs_r = c(paste("Hispanic/Latino ", nums_r[1], "%", sep = ""),
               paste("White ", nums_r[2], "%", sep = ""),
               paste("Black/African American ", nums_r[3], "%", sep = "")
               )
    if(is.na(StudentInfo$`Asian(%)`[datain()]) == F) {
      nums_r <- c(nums_r, StudentInfo$`Asian(%)`[datain()])
      labs_r <- c(labs_r, paste("Asian ", tail(nums_r, 1), "%", sep = ""))
    }
    if(is.na(StudentInfo$`Unknown(%)`[datain()]) == F) {
      nums_r <- c(nums_r, StudentInfo$`Unknown(%)`[datain()])
      labs_r <- c(labs_r, paste("Unknown ", tail(nums_r, 1), "%", sep = ""))
    }
    if(is.na(StudentInfo$`Two or more races(%)`[datain()]) == F) {
      nums_r <- c(nums_r, StudentInfo$`Two or more races(%)`[datain()])
      labs_r <- c(labs_r, paste("Two or more races ", tail(nums_r, 1), "%", sep = ""))
    }
    if(is.na(StudentInfo$`Non-Resident Alien(%)`[datain()]) == F) {
      nums_r <- c(nums_r, StudentInfo$`Non-Resident Alien(%)`[datain()])
      labs_r <- c(labs_r, paste("Non-Resident Alien ", tail(nums_r, 1), "%", sep = ""))
    }
    
    pie(nums_r, labels = labs_r, main = "Pie Chart for Student Demographics")
  })
  
  # ----------------------------------------------------------
  
  output$state <- renderPlot({
    nums_s <- c(StudentInfo$`In-State(%)`[datain()], 
                StudentInfo$`Out of State(%)`[datain()])
    labs_s <- c(paste("In-state ", nums_s[1], "%", sep = ""), 
                paste("Out of state ", nums_s[2], "%", sep = ""))
    pie(nums_s, labels = labs_s, main = "Pie Chart for Student State")
  })
  
  # ----------------------------------------------------------
  
  output$ageall <- renderText({
    paste("Average age of all students: ", StudentInfo$`AvrgAge (All students)`[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$agefull <- renderText({
    paste("Average age of full-time students: ", StudentInfo$`AvrgAge (Full Time)`[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$phone <- renderText({
    paste("Phone number: ", CUNY_Location$`Phone Number`[contactin()])
  })
  
  # ----------------------------------------------------------
  
  output$website <- renderUI({
    a("Click here for the school website", href = CUNY_Location$`Campus Website`[contactin()])
  })
  
  # ----------------------------------------------------------
  
  output$address <- renderText({
    paste("Address: ", CUNY_Location$Street[contactin()], " ", 
          CUNY_Location$City[contactin()], " ",
          CUNY_Location$State[contactin()], " ",
          CUNY_Location$Zip[contactin()])
    
  })
  
  # ----------------------------------------------------------
    
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      addMarkers(lng = CUNY_Location$Longitude[contactin()], 
                 lat = CUNY_Location$Latitude[contactin()],
                 popup = CUNY_Location$Campus[contactin()])
        
  })
  
  # ----------------------------------------------------------
  
  output$resident <- renderText({
    paste("Average residency expenses: ", 
          school_expenses$reident[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$transp <- renderText({
    paste("Average transportation expenses: ", 
          school_expenses$trsp[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$book <- renderText({
    paste("Average books and supplies expenses: ", 
          school_expenses$bookspl[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$lunch <- renderText({
    paste("Average lunch expenses: ", 
          school_expenses$lunch[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$personal <- renderText({
    paste("Average personal expenses: ", 
          school_expenses$prsnl[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$room <- renderText({
    paste("Average room and boarding expenses: ", 
          school_expenses$rmbrd[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$credit <- renderText({
    paste("Average cost per credit: ", 
          school_expenses$prcrdt[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$under <- renderText({
    paste("Total undergraduages:", 
          StudentInfo$`Total undergraduates`[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$grad <- renderText({
    paste("Total graduages:", 
          StudentInfo$`Total graduates`[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$ratio <- renderText({
    paste("Student to faculty ratio: ", 
          StudentInfo$`Student Faculty Ratio`[datain()],
          ":1", sep = "")
  })
  
  # ----------------------------------------------------------
  
  output$sat <- renderText({
    paste("Median SAT score:", 
          StudentInfo$`Median-SAT`[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$adtype <- renderText({
    paste("Admission type:", 
          StudentInfo$`Admission Type`[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$adprnt <- renderText({
    paste("Admitted applicants: ", 
          StudentInfo$`Admitted Applicants(%)`[datain()],
          "%", sep = "")
  })
  
  # ----------------------------------------------------------
  
  output$finaid <- renderText({
    paste("Average financial aid package: ", 
          StudentInfo$`AvgFinAid($)`[datain()],
          "$", sep = "")
  })
  
  # ----------------------------------------------------------
  
  output$size <- renderText({
    paste("School size:", 
          StudentInfo$Size[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$part <- renderText({
    paste("Part time students: ", 
          StudentInfo$`Part Time Students (%)`[datain()],
          "%", sep = "")
  })
  
  # ----------------------------------------------------------
  
  output$type <- renderText({
    paste("School type:", 
          StudentInfo$`College Type`[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$courses <- renderTable({
    dptId <- 0
    i <- 1
    while(i <= length(departments$`dept_id`)) {
      if(departments$schid[i] == crsin() & departments$dept_name[i] == input$Schools_depts) {
          dptId <- departments$`dept_id`[i]
          break
      }
      i <- i + 1
    }
    

    crs2 <- fn$sqldf("SELECT A.dept_name Department, B.courseno Course, B.coursecr
                     Credits FROM (SELECT dept_id, dept_name FROM departments WHERE dept_id = '$dptId') A JOIN
                     (SELECT dept_id, courseno, coursecr FROM courses WHERE dept_id = '$dptId') B ON
                     A.dept_id = B.dept_id")

    crs2
  })
  
}

shinyApp(ui = ui, server = server)