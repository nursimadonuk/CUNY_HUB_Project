# Libraries used
library(shiny)
library(sqldf)
library(leaflet)
library(readr)
library(shinythemes)
library(shinyWidgets)


# Loading data - uncomment lines below to load
extra <- read_csv("data/extra.csv")
departments <- read_csv("data/departments.csv")
depsTaught <- read_csv("data/depsTaught.csv")
degrees <- read_csv("data/degrees.csv")
courses <- read_csv("data/courses.csv")
contact <- read_csv("data/contact.csv")
honors <- read_csv("data/honors.csv")
faculty <- read_csv("data/faculty.csv")
expenses <- read_csv("data/expenses.csv")


# User Interface
ui <- fluidPage(theme=shinytheme("slate"),setBackgroundImage(
    src = "https://images.unsplash.com/photo-1517416635928-82ef32a975b7?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"),
    div(navbarPage(title = div(
                              div(
                                id = "img-id",
                                img(style = "height: 50px; position: fixed; right: 0px; top:0px", src = "logo.png")
                              ),
                            ),
    
    tabPanel("Fun Facts",
             fluidRow(column(4, wellPanel(selectInput("Schools_fun", strong(style = "color:white;", "Choose a school district"),
                                            choices = contact$Campus)))),
             fluidRow(
               column(6,
                      h4(strong(style="text-align: center; color:white", textOutput("finaid"))),
                      h4(strong(style="text-align: center; color:white", textOutput("size"))),
                      h4(strong(style="text-align: center; color:white", textOutput("type"))),
                      h4(strong(style="text-align: center; color:white", textOutput("ageall"))),
                      h4(strong(style="text-align: center; color:white", textOutput("agefull"))),
                      h4(strong(style="text-align: center; color:white", textOutput("part"))),
                      h4(strong(style="text-align: center; color:white", textOutput("under"))),
                      h4(strong(style="text-align: center; color:white", textOutput("grad"))),
                      h4(strong(style="text-align: center; color:white", textOutput("ratio"))),
                      h4(strong(style="text-align: center; color:white", textOutput("sat"))),
                      h4(strong(style="text-align: center; color:white", textOutput("adtype"))),
                      h4(strong(style="text-align: center; color:white", textOutput("adprnt")))),
               column(6,
                      wellPanel(
                        plotOutput("gender"))
             )),
             fluidRow(
               column(6,
                      wellPanel(
                        plotOutput("race"))
                      ),
               column(6,
                      wellPanel(
                        plotOutput("state"))
                      )
             )
    ),
    tabPanel("Contact Info",
             sidebarPanel(
               selectInput("Schools_cont", strong(style = "color:white;", "Choose a school district"),
                           choices = contact$Campus)
             ),
             mainPanel(
               h3(strong(style = "color:white;", textOutput("phone"))),
               h3(strong(uiOutput("website"))),
               h3(strong(style = "color:white;", textOutput("address"))),
               leafletOutput("map")
             )
    ),
    tabPanel("School Expenses",
             sidebarPanel(
               selectInput("Schools_exp", strong(style = "color:white;", "Choose a school district"),
                           choices = contact$Campus)
             ),
             mainPanel(
               fluidRow(
                column(6, wellPanel(style = "height:300px; background-size: cover; background-image: url(https://images.unsplash.com/photo-1517963140-f60c9eceb504?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=802&q=80)",
                  h4(strong(style="text-align: center; color:black", textOutput("resident"))))), 
                column(6, wellPanel(style = "height:300px; background-size: cover; background-image: url(https://images.unsplash.com/photo-1555124618-81b95d0e5892?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1650&q=80)",
                  h4(strong(style="text-align: center; color:black", textOutput("transp")))))
               ),
               fluidRow(
                 column(6, wellPanel(style = "height:300px; background-size: cover; background-image: url(https://images.unsplash.com/photo-1543352634-99a5d50ae78e?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1651&q=80)",
                   h4(strong(style="text-align: center; color:black", textOutput("lunch"))))),
                 column(6, wellPanel(style = "height:300px; background-size: cover; background-image: url(https://images.unsplash.com/photo-1502465771179-51f3535da42c?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1649&q=80)",
                   h4(strong(style="text-align: center; color:teal", textOutput("personal")))))
               ),
               fluidRow(
                 column(6, wellPanel(style = "height:300px; background-size: cover; background-image: url(https://images.unsplash.com/photo-1504390747618-f9ea2a96c487?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1017&q=80https://images.unsplash.com/photo-1541829070764-84a7d30dd3f3?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=2849&q=80)",
                   h4(strong(style="text-align: center; color:teal", textOutput("room"))))),
                 column(6, wellPanel(style = "height:300px; background-size: cover; background-image: url(https://images.unsplash.com/photo-1541829070764-84a7d30dd3f3?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=2849&q=80)",
                   h4(strong(style="text-align: center; color:teal", textOutput("credit")))))
              ),
               #fluidRow(
                 #column(6, offset = 3, wellPanel(h4(textOutput("books"))))
             #)
             # This last text output doesn't show anything 
            )
      
    ),
    navbarMenu("Courses and Majors",
               tabPanel("Courses",
                        sidebarPanel(
                          selectInput("Schools_courses",
                                      strong(style = "color:white;", "Choose a school district"),
                                      choices = contact$Campus),
                          selectInput("Schools_depts",
                                      strong(style = "color:white;", "Choose a department"),
                                      choices = sort(unique(departments$DeptName)))
                        ),
                        mainPanel(
                          dataTableOutput("courses")
                          )
                        
                ),
               
               tabPanel("Majors",
                        sidebarPanel(
                          selectInput("Schools_d",
                                      strong(style = "color:white;", "Choose a Major"),
                                      choices = sort(unique(departments$DeptName))
                                      )
                        ),
                        mainPanel(
                          dataTableOutput("departments")
                        )
               )
    ),
    tabPanel("Programs",
             fluidPage(
               dataTableOutput("honors")
             )
    ),
    tabPanel("Faculty",
             sidebarPanel(
               selectInput("Schools_f",
                           strong(style = "color:white;", "Choose a School District"),
                           choices = contact$Campus
                           ),
               selectInput("Depts_f",
                           strong(style = "color:white;", "Choose a Department"),
                           choices = sort(unique(departments$DeptName))
               )
             ),
             mainPanel(
               dataTableOutput("faculty")
             )
    ),
    tabPanel("Compare Schools",
             fluidRow(
               column(12,
                      h3(strong(style = "color:white;", "Choose two school districts to compare them")),
                      fluidRow(
                        column(6,
                               selectInput("Comp1",
                                           strong(style = "color:white;", "Choose First School District"),
                                           choices = contact$Campus)
                        ),
                        column(width = 6,
                               selectInput("Comp2",
                                           strong(style = "color:white;", "Choose Second School District"),
                                           choices = contact$Campus)
                        )
                      )
               )
             )
    ),
    tabPanel("Available Degrees",
             sidebarPanel(
               selectInput("Deg",
                           strong(style = "color:white;", "Choose a School District"),
                           choices = contact$Campus),
             ),
             mainPanel(
               dataTableOutput("degrees")
             )
             )
    
    
    ))
  
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
  
  facin <- reactive({
    switch(input$Schools_f,
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
  
  firstin <- reactive({
    switch(input$Comp1,
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
  
  secindin <- reactive({
    switch(input$Comp2,
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
  
  degin <- reactive({
    switch(input$Deg,
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
    nums_g <- c(extra$`Women%`[datain()],
              extra$`Men%`[datain()])
    labs_g <- c(paste("Women ", (nums_g[1]*100), "%", sep = ""),
                paste("Men ", (nums_g[2]*100) ,"%", sep = ""))
    pie(nums_g, labels = labs_g, main = "Pie Chart for Student Gender")
  })
  
  # ----------------------------------------------------------
  
  output$race <- renderPlot({
    nums_r <- c(extra$`Hispanic/Latino%`[datain()],
                extra$`White%`[datain()],                
                extra$`Black/AfricanAmerican%`[datain()]
                )
    labs_r = c(paste("Hispanic/Latino ", (nums_r[1]*100), "%", sep = ""),
               paste("White ", (nums_r[2]*100), "%", sep = ""),
               paste("Black/African American ", (nums_r[3]*100), "%", sep = "")
               )
    if(is.na(extra$`Asian%`[datain()]) == F) {
      nums_r <- c(nums_r, extra$`Asian%`[datain()])
      labs_r <- c(labs_r, paste("Asian ", (tail(nums_r, 1)*100), "%", sep = ""))
    }
    if(is.na(extra$`Unknown%`[datain()]) == F) {
      nums_r <- c(nums_r, extra$`Unknown%`[datain()])
      labs_r <- c(labs_r, paste("Unknown ", (tail(nums_r, 1)*100), "%", sep = ""))
    }
    if(is.na(extra$`TwoOrMore%`[datain()]) == F) {
      nums_r <- c(nums_r, extra$`TwoOrMore%`[datain()])
      labs_r <- c(labs_r, paste("Two or more races ", (tail(nums_r, 1)*100), "%", sep = ""))
    }
    if(is.na(extra$`NonResidentAlien%`[datain()]) == F) {
      nums_r <- c(nums_r, extra$`NonResidentAlien%`[datain()])
      labs_r <- c(labs_r, paste("Non-Resident Alien ", (tail(nums_r, 1)*100), "%", sep = ""))
    }
    
    pie(nums_r, labels = labs_r, main = "Pie Chart for Student Demographics")
  })
  
  # ----------------------------------------------------------
  
  output$state <- renderPlot({
    nums_s <- c(extra$`InState%`[datain()], 
                extra$`OutOfState%`[datain()])
    labs_s <- c(paste("In-state ", (nums_s[1]*100), "%", sep = ""), 
                paste("Out of state ", (nums_s[2]*100), "%", sep = ""))
    pie(nums_s, labels = labs_s, main = "Pie Chart for Student State")
  })
  
  # ----------------------------------------------------------
  
  output$ageall <- renderText({
    paste("Average age of all students: ", extra$`AvrgAgeAll`[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$agefull <- renderText({
    paste("Average age of full-time students: ", extra$`AvrgAgeFull`[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$phone <- renderText({
    paste("Phone number: ", contact$`PhoneNumber`[contactin()])
  })
  
  # ----------------------------------------------------------
  
  output$website <- renderUI({
    a("Click here for the school website", href = contact$`CampusWebsite`[contactin()])
  })
  
  # ----------------------------------------------------------
  
  output$address <- renderText({
    paste("Address: ", contact$Street[contactin()], " ", 
          contact$City[contactin()], " ",
          contact$State[contactin()], " ",
          contact$Zip[contactin()])
    
  })
  
  # ----------------------------------------------------------
    
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      addMarkers(lng = contact$Longitude[contactin()], 
                 lat = contact$Latitude[contactin()],
                 popup = contact$Campus[contactin()])
        
  })
  
  # ----------------------------------------------------------
  
  output$resident <- renderText({
    paste("Average residency expenses: ", 
          expenses$Resident[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$transp <- renderText({
    paste("Average transportation expenses: ", 
          expenses$Transp[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$book <- renderText({
    paste("Average books and supplies expenses: ", 
          expenses$BookSpl[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$lunch <- renderText({
    paste("Average lunch expenses: ", 
          expenses$Lunch[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$personal <- renderText({
    paste("Average personal expenses: ", 
          expenses$Prsnl[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$room <- renderText({
    paste("Average room and boarding expenses: ", 
          expenses$RoomBrd[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$credit <- renderText({
    paste("Average cost per credit: ", 
          expenses$PerCr[expin()], "$", sep = "")
    
  })
  
  # ----------------------------------------------------------
  
  output$under <- renderText({
    paste("Total undergraduages:", 
          extra$TotalUndergraduates[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$grad <- renderText({
    paste("Total graduages:", 
          extra$TotalGraduates[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$ratio <- renderText({
    paste("Student to faculty ratio: ", 
          extra$StudentFacultyRatio[datain()],
          ":1", sep = "")
  })
  
  # ----------------------------------------------------------
  
  output$sat <- renderText({
    paste("Median SAT score:", 
          extra$MedianSAT[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$adtype <- renderText({
    paste("Admission type:", 
          extra$AdmissionType[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$adprnt <- renderText({
    paste("Admitted applicants: ", 
          extra$`AdmittedApplicants%`[datain()],
          "%", sep = "")
  })
  
  # ----------------------------------------------------------
  
  output$finaid <- renderText({
    paste("Average financial aid package: ", 
          extra$AvgFinAid[datain()],
          "$", sep = "")
  })
  
  # ----------------------------------------------------------
  
  output$size <- renderText({
    paste("School size:", 
          extra$Size[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$part <- renderText({
    paste("Part time students: ", 
          extra$`PartTime%`[datain()],
          "%", sep = "")
  })
  
  # ----------------------------------------------------------
  
  output$type <- renderText({
    paste("School type:", 
          extra$Type[datain()])
  })
  
  # ----------------------------------------------------------
  
  output$courses <- renderDataTable({
    dptId <- 0
    i <- 1
    while(i <= length(departments$DeptId)) {
      if(departments$SchoolId[i] == crsin() & departments$DeptName[i] == input$Schools_depts) {
          dptId <- departments$DeptId[i]
          break
      }
      i <- i + 1
    }
    

    crs <- fn$sqldf("SELECT A.DeptName Department, B.CourseNo Course, B.CourseCr
                     Credits FROM (SELECT DeptId, DeptName FROM departments WHERE DeptId = '$dptId') A JOIN
                     (SELECT DeptId, CourseNo, CourseCr FROM courses WHERE DeptId = '$dptId') B ON
                     A.DeptId = B.DeptId")

    crs
  })
  
  # ----------------------------------------------------------
  
  output$departments <- renderDataTable({
    dptId <- c()
    schId <- c()
    i<- 1
    while(i <= length(departments$DeptId)) {
      if(departments$DeptName[i] == input$Schools_d) {
        dptId <- c(dptId, departments$DeptId[i])
        schId <- c(schId, departments$SchoolId[i])
      }
      i <- i + 1
    }
    
    schName <- c()
    for(j in schId) {
      schName <- c(schName, contact$Campus[j])
    }
    
    numCourses <- c()
    totCredits <- c()
    for(k in dptId) {
      startIndex <- match(k, courses$DeptId)
      i <- startIndex
      count <- 0
      total <- 0
      while(i <= length(courses$DeptId)) {
        if(courses$DeptId[i] == k) {
          count <- count + 1
          total <- total + courses$CourseCr[i]
        }
        if(courses$DeptId[i] != k) {
          i <- length(courses$DeptId)+1
        }
        i <- i + 1
      }
      numCourses <- c(numCourses, count)
      totCredits <- c(totCredits, total)
    }
    
    
    # returns a list of vectors. Each vector has the indexes of the 
    # appearances of the elements in dptId in depsTaught$DeptId.
    facId <- lapply(dptId, function(x) which(depsTaught$DeptId %in% x)) 
    
    factIds <- list()
    
    # goes through each vector in the list
    # facId has a length equal to the length of dptId
    # each vector in facId has a length equal to the # of
    # occurrences of the corresponding department ID
    for(vec in facId) {
      i <- 1
      x <- c()
      # goes through the elements of the vector
      # each element represents an index
      # replace each element with the corresponding faculty Id
      while(i <= length(vec)) {
        x <- c(x, depsTaught$FacultyId[vec[i]])
        i <- i+1
      }
      factIds[[length(factIds)+1]] <- x
    }

    # now the vectors in facId have the faculty IDs 
    
    factRats <- list()
    
    # go through the vectors in facId and replace with 
    # the rating corresponding to the faculty ID
    for(facs in facId) {
      i <- 1
      y <- c()
      while(i <= length(facs)) {
        y <- c(y, faculty$Rating[facs[i]])
        i <- i+1
      }
      factRats[[length(factRats)+1]] <- y
    }
    
    myMean <- function(x) {
      sum <- 0
      count <- 0
      for(item in x) {
        if(is.na(item) == F) {
          sum<-sum+ as.numeric(item)
          count <- count +1
        }
        
      }
      return(sum/count)
    }
    
    ratings <- c()
    i <- 1
    while(i <= length(factRats)) {
      # need to double check this
      ratings <- c(ratings, myMean(factRats[[i]]))
      i <- i+1
    }
    
    mjrs <- data.frame(School = schName,
                       `Num of Courses` = numCourses,
                       `Total Credits` = totCredits,
                       `Avg Faculty Rating` = ratings)
    
    
    

    mjrs
    
  })
  
  # ----------------------------------------------------------
  
  output$honors <- renderDataTable({
    honrs <- fn$sqldf("SELECT contact.Campus Campus, honors.HonorsProgram Program, honors.Link Link FROM 
                      contact JOIN honors ON contact.SchoolId = honors.SchoolId")
    
    honrs
  })
  
  # ----------------------------------------------------------
  
  output$faculty <- renderDataTable({
    schid <- facin()
    dptid <- input$Depts_f
    facty <- fn$sqldf("SELECT faculty.LastName Last_Name, faculty.FirstName First_Name, faculty.Rating Rating,
                      faculty.Email Email, faculty.StartYear CUNY_Start_Year FROM faculty JOIN (SELECT * FROM 
                      depsTaught WHERE DeptId=(SELECT DeptId FROM departments WHERE SchoolId='$schid' AND 
                      DeptName='$dptid')) A ON faculty.FacultyId = A.FacultyId")
    
    
    facty
  })
  
  # ----------------------------------------------------------
  
  output$degrees <- renderDataTable({
    schid <- degin()
    fn$sqldf("SELECT contact.Campus Campus, degrees.ProgramName Program, degrees.AwardName Degree,
             degrees.CIP201TITLESHORT Title FROM degrees JOIN 
             contact ON contact.SchoolId=degrees.SchoolId AND degrees.SchoolId='$schid'")
  })
  
 
}

shinyApp(ui = ui, server = server)
