library(shiny)

ui = fluidPage(
    
    titlePanel('Grade Calculator for PSYC3510'),
    
    fluidRow(
        # Current grades
        column(4,
               wellPanel(
                   h3('Current grades'),
                   p("Enter the points you've received so far."),
                   
                   # Quizzes
                   hr(),
                   sliderInput('numQuizzes', 'Number of Reading Quizzes so far', min = 0, max = 18, value = 12),
                   sliderInput('pointsQuizzes', 'Reading Quiz total points earned', min = 0, max = 150, value = 90),
                   tags$small("Drop the lowest 3 reading quizzes (don't count them)."),
                   
                   # Exams
                   hr(),
                   sliderInput('numExams', 'Number of Unit Exams so far', min = 0, max = 7, value = 5),
                   sliderInput('pointsExams', 'Unit Exam total points earned', min = 0, max = 700, value = 400),
                   tags$small('Count all unit exams so far, but enter your two lowest unit exam scores below:', br(), br()),
                   numericInput('lowest1', 'Lowest Unit Exam points', min = 0, max = 105, value = 65),
                   numericInput('lowest2', '2nd Lowest Unit Exam points', min = 0, max = 105, value = 41)
                   
               )
        ),
        
        # Future grades
        column(4,
               wellPanel(
                   h3('Future grades'),
                   p('Enter your hypothetical grades for the rest of the semester.'),
                   
                   # Quizzes
                   hr(),
                   sliderInput('futureQuizzes', 'Reading Quiz average', min = 0, max = 100, value = 100, post = '%'),
                   tags$small('Remember, you can always get 100% on the reading quizzes. These are "free" points if you put in a bit of effort.'),
                   
                   # Exams
                   hr(),
                   sliderInput('futureExams', 'Unit Exam average', min = 0, max = 100, value = 80, post = '%'),
                   checkboxInput('takesFinal', 'I plan on taking the final exam.'),
                   conditionalPanel('input.takesFinal',
                                    sliderInput('finalExam', 'Final Exam', min = 0, max = 100, value = 80, post = '%')
                   ),
                   
                   tags$small(strong('Option 1:'), 'You drop your lowest 2 unit exams and take the final.', br()),
                   tags$small(strong('Option 2:'), "You count your 7 unit exams and don't take the final."),
                   
                   # Extra Credit
                   hr(),
                   sliderInput('extraCredit', 'Extra Credit', min = 0, max = 10, value = 0, step = 1),
                   tags$small('You can earn up to 10 extra credit points. See iCollege instructions for details.')
                   
               )
        ),
        
        # Grade calculation
        column(4,
               tabsetPanel(
                   tabPanel('Grade Calculation', 
                            br(),
                            tableOutput('table'),
                            # make last row bold
                            tags$style(type = 'text/css', '#table tr:last-child {font-weight: bold;}'),
                            
                            # warning (if applicable)
                            span(textOutput('warning'), style='color: red;')
                   ), 
                   
                   tabPanel('Info',
                            br(),
                            p('This is a grade calculator for PSYC3510 (Introduction to Research Design and Analysis) Section 015, taught at Georgia State University in Spring 2019.'),
                            p('Use at your own risk. If you find errors or have other comments/suggestions, please let me know at', a(href = 'https://twitter.com/watzoever', '@watzoever')),
                            
                            hr(),
                            h4('Grading Scheme'),
                            tableOutput('scheme')
                   )
               )
        )
    )
)

server = function(input, output, session) {
    
    # change max possible points for reading quizzes based on number of quizzes completed
    observeEvent(input$numQuizzes, {
        updateSliderInput(session, 'pointsQuizzes', max = (input$numQuizzes-3) * 10)
    })
    
    # change max possible points for unit exams based on number of exams completed
    observeEvent(input$numExams, {
        updateSliderInput(session, 'pointsExams', max = input$numExams * 100)
    })
    
    gradeTable = reactive({
        # total points for reading quizzes = current + future points
        # future points = percentage/100 to get proportion * 10 points per quiz * number of quizzes left
        futureQuizzes = input$futureQuizzes/100 * 10
        if (input$numQuizzes >= 3) { totQuizzes = input$pointsQuizzes + futureQuizzes * (18 - input$numQuizzes) }
        else { totQuizzes = input$pointsQuizzes + futureQuizzes * 15 }
        
        # future points for unit exams = percentage/100 to get proportion * 100 points per exam * number of remaining exams
        futureUnit = input$futureExams/100 * 100 * (7 - input$numExams)
        
        # total points for unit exams
        # option1: current, drop (subtract) 2 lowest), add future points
        totUnit = input$pointsExams - input$lowest1 - input$lowest2 + futureUnit
        if (input$numExams <= 2) { totUnit = futureUnit }
        
        # option2: current + future points
        totUnit2 = input$pointsExams + futureUnit
        
        # total points for final exam = percentage/100 to get proportion * 200 points
        totFinal = input$finalExam/100 * 200 #points
        
        ec = input$extraCredit
        
        total = totQuizzes + totUnit + totFinal + ec
        total2 = totQuizzes + totUnit2 + ec
        
        perc = round(total/850*100 + .01, 1)
        perc2 = round(total2/850*100 + .01, 1)
        
        ptsToLetter = function(points) {
            if (points >= 824) return('A+')
            else if (points >= 790) return('A')
            else if (points >= 765) return('A-')
            else if (points >= 739) return('B+')
            else if (points >= 705) return('B')
            else if (points >= 680) return('B-')
            else if (points >= 654) return('C+')
            else if (points >= 620) return('C')
            else if (points >= 595) return('C-')
            else if (points >= 510) return('D')
            return('F')
        }
        
        if (input$takesFinal) {
            data.frame(
                Name = c('Reading Quizzes', 'Unit Exams', 'Final Exam', 'Extra Credit', 'Total', ''),
                Points = as.character(c(totQuizzes, totUnit, totFinal, ec, total, paste0(perc, '%'))),
                Out_Of = as.character(c(150, 500, 200, '(10)', 850, ptsToLetter(total))),
                stringsAsFactors = F)
            
        } else {
            data.frame(
                Name = c('Reading Quizzes', 'Unit Exams', 'Extra Credit', 'Total', ''),
                Points = as.character(c(totQuizzes, totUnit2, ec, total2, paste0(perc2, '%'))),
                Out_Of = as.character(c(150, 700, '(10)', 850, ptsToLetter(total2))),
                stringsAsFactors = F)
        }
    })
    
    warningText = reactive({
        pts = input$pointsExams
        l1 = input$lowest1
        l2 = input$lowest2
        
        if (pts - l1 - l2 > 105 * (input$numExams - 2)) { "With the given Unit Exam total, the lowest two score/s can't be this low." }
        else if (pts < l1 + l2) { "Unit Exam total can't be lower than the two lowest scores combined." }
        else { '' }
    })
    
    # table
    gradingScheme = data.frame(
        Points = c('824-850', '790-823', '765-789', '739-764', '705-738', '680-704', '654-679', '620-653', '595-619', '510-594', '0-509'),
        Percent = c('97-100%', '93-96.9%', '90-92.9%', '87-89.9%', '83-86.9%', '80-82.9%', '77-79.9%', '73-76.9%', '70-72.9%', '60-69.9%', '0-59.9%'),
        Grade = c('A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D', 'F'),
        stringsAsFactors = F)
    
    # render
    output$table = renderTable({ gradeTable() })
    output$warning = renderText({ warningText() })
    output$scheme = renderTable({ gradingScheme })
    
}

shinyApp(ui = ui, server = server)
