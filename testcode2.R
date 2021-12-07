library(markdown)
library(tidyr)
library(tibble)
library(dplyr)
library(shiny)
library(GGally)
library(ggplot2)

dotquy <- read.delim("/home/bao/DataAnalysis/Stroke-Analysis/www/dotquy.txt")
dotquy2 <- read.delim("/home/bao/DataAnalysis/Stroke-Analysis/www/dotquy2.txt")
stroke <- read.csv("/home/bao/DataAnalysis/stroke-data.csv")
stroke$bmi<- as.numeric(stroke$bmi)
df <- stroke
df$stroke_t <- ifelse(df$stroke == 1, 'stroke', 'no stroke')
df$stroke_t <- factor(df$stroke_t)


df$hypertension_t <- ifelse(df$hypertension == 1, 'hypertension', 'no hypertension')
df$hypertension_t <- factor(df$hypertension_t)


df$gender_t <- factor(df$gender)
df$ever_married_t <- factor(df$ever_married)
df$work_type_t <- factor(df$work_type)
df$Residence_type_t <- factor(df$Residence_type)
df$smoking_status_t <- factor(df$smoking_status)


df$heart_disease_t <- ifelse(df$heart_disease == 1, 'heart disease', 'no heart disease')
df$heart_disease_t <- factor(df$heart_disease_t)
str(df)


df$gender <- ifelse(df$gender == "Male", 1, 0)
df$gender <- as.integer(df$gender)

df$ever_married <- ifelse(df$ever_married == "Yes", 1, 0)
df$ever_married <- as.integer(df$ever_married)


df$work_type <- sapply(df$work_type, switch, "Private"=1, "Self-employed"=2, "Govt_job"=3, "children"=4, "Never_worked"=5)
df$work_type <- as.integer(df$work_type)

df$Residence_type <- ifelse(df$Residence_type == "Urban", 1, 0)
df$Residence_type <- as.integer(df$Residence_type)

df$smoking_status <- sapply(df$smoking_status, switch, "formerly smoked"=1, "never smoked"=2, "smokes"=3, "Unknown"=4)
df$smoking_status <- as.integer(df$smoking_status)

df$stroke_t <- ifelse(df$stroke == 1, 'stroke', 'no stroke')
df$stroke_t <- factor(df$stroke_t)


model <- glm( stroke ~ gender + age + hypertension + heart_disease + work_type + Residence_type + avg_glucose_level + bmi,
              data = df, family = binomial)
#summary(model)$coef
#coef(model)
#summary(model )$coef

newdata <- data.frame(gender = 1, age = 20, hypertension = 1, heart_disease = 1, work_type =2, Residence_type =1, avg_glucose_level = 150, bmi = 25)
probabilities <- model %>% predict(newdata, type = "response")*100

stroked <- stroke[stroke$stroke == 1,]
strokedBMI <- mean(stroked$bmi, na.rm = TRUE)

stroke$smoking_status <- factor(stroke$smoking_status)

print(strokedBMI)


ui = navbarPage("Stroke!",
            tabPanel("Introduction",
                                    titlePanel(h1(id="big-heading","Introduction")),

                                    mainPanel(
                                      tags$style(HTML("#big-heading{color: #483D8B;}")),
                                      h1(id = "heading","Báo cáo chuyên đề: Đề tài phân tích dữ liệu và dự đoán các ca bệnh đột quỵ"),

                                      tags$style(HTML("#heading{color: #4B0082;}")),

                                      h3(id = "h3","Nguyen Thai Bao 18IT2 18IT051", align="right"),
                                      h3(id = "h3","Le Cao Nguyen 18it1 18IT029",align="right"),
                                      tags$style(HTML("#h3{color: #7B68EE;}")),
                                      h2(id = "h2","Đột quỵ là gì?"),
                                      tags$style(HTML("#h2{color: #FF00FF;}")),
                                      p( toString(dotquy2[,1])),

                                      h2(id = "h2","Các yếu tố gây nên đột quỵ?"),
                                      p( toString(dotquy[,1])),


                                      h3(id = "h2","Các triệu chứng nhận biết và điều nên làm?"),
                                      img(class="img-polaroid",
                                            style="width= 100px; height = 100px",
                                            src="FAST.jpg"),

                                    ),
                ),

            navbarMenu("About data",
                       tabPanel("Table",
                                h2(id = "h2","Take a look at the data"),
                                DT::dataTableOutput("table")
                       ),

                       tabPanel("Summary",
                                h2(id = "h2","Summary table"),
                                verbatimTextOutput("summary")
                       ),
                       tabPanel("Multi Plot",
                                h2("Age, average glucose, Body mass index"),
                                plotOutput("multiPlot1"),
                                h2("WorkType"),
                                plotOutput("workPie")
                       ),
                       tabPanel("Multi Plot 2",
                                h2(id = "h2","BoxPlot age of ..."),
                                sidebarLayout(
                                  sidebarPanel(

                                    selectInput(
                                      inputId  = "multi2",
                                      label = "Select plot",
                                      choices = c(
                                        'gender_t', 'hypertension_t', 'heart_disease_t',
                                        'ever_married_t', 'work_type_t', 'Residence_type_t',
                                        'smoking_status_t', 'stroke_t'
                                      ),
                                      selected = "gender"
                                    ),
                                  ),
                                  mainPanel(
                                    plotOutput("multiPlot2")
                                  )
                                )
                       ),

            ),
            navbarMenu("Some unrelated information",

                       tabPanel("Diabetes and BMI",
                                h2(id = "h2","Diabetes and BMI"),

                                sidebarLayout(
                                  sidebarPanel(
                                    radioButtons("plotType", "Plot type",
                                                 c("Scatter"="p", "Line"="l")
                                    ),

                                  ),
                                  mainPanel(
                                    plotOutput("plot"),
                                    p("Chi so BMI trung binh cua cac benh nhan bi dot quy la ", toString(strokedBMI), " va chi so BMI trung binh cua bo du lieu nay la " ,mean(stroke$bmi, na.rm = TRUE), "
                                      va doi voi nguoi khoe manh, chi so BMI se giao dong tu:"),
                                    img(class="img-polaroid",
                                        style="width= 100px; height = 100px",
                                        src="BMI.jpg"),
                                  )
                                )
                       ),

                       tabPanel("Hypertension effect to heart dicease?",
                                h2(id = "h2","Hypertension effect to heart dicease?"),
                                sidebarLayout(
                                  sidebarPanel(

                                    radioButtons(

                                      inputId  = "hypertensionToHeart",
                                      label = "Select Hypertenstion",
                                      choices = c("Yes"=1, "No"=0),
                                      selected = ""
                                    ),
                                  ),
                                  mainPanel(
                                    plotOutput("hypertensionToHeartPlot")
                                  )
                                )
                       ),
              ),

           navbarMenu("Factors affecting stroke",
                      tabPanel("Age of stroke",
                               sidebarLayout(
                                 sidebarPanel(
                                   sliderInput(inputId = "fromAge",
                                               label = "Age from",
                                               min = min(stroke$age, na.rm = TRUE),
                                               max = max(stroke$age, na.rm = TRUE),
                                               value = min(stroke$age, na.rm = TRUE)),
                                   sliderInput(inputId = "toAge",
                                               label = "To",
                                               min = min(stroke$age, na.rm = TRUE),
                                               max = max(stroke$age, na.rm = TRUE),
                                               value = max(stroke$age, na.rm = TRUE)),
                                   img(height="200px", width="300px", align="left",
                                       src="DotQuyONguoiGia.jpg"),

                                 ),
                                 mainPanel(
                                   plotOutput("age3Plot"),
                                   plotOutput("agePlot")

                                 )
                               )
                      ),
                      tabPanel("Gender of stroke",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(
                                     inputId  = "gender",
                                     label = "Select gender",
                                     choices = unique(stroke$gender),
                                     selected = ""
                                   ),
                                 ),
                                 mainPanel(
                                   plotOutput("genderPlot")
                                 )
                               )
                      ),
                      tabPanel("Hypertension of stroke",
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons(
                                     inputId  = "hypertension",
                                     label = "Select Hypertenstion",
                                     choices = c("Yes"=1, "No"=0),
                                     selected = ""
                                   ),
                                 ),
                                 mainPanel(
                                   plotOutput("hypertensionPlot")
                                 )
                               )
                      ),
                      tabPanel("Heart of stroke",
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons(
                                     inputId  = "heart",
                                     label = "Heart decease",
                                     choices = c("Yes"=1, "No"=0),
                                     selected = ""
                                   ),
                                 ),
                                 mainPanel(
                                   plotOutput("heartPlot")
                                 )
                               )
                      ),
                      tabPanel("Smoke effect to stroke",
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons(
                                     inputId  = "smoke",
                                     label = "Smoke",
                                     choices = unique(stroke$smoking_status),
                                     selected = ""
                                   ),
                                 ),
                                 mainPanel(
                                   plotOutput("smokePlot")
                                 )
                               )
                      ),
                      tabPanel("Glucose of stroke",
                               sidebarLayout(
                                 sidebarPanel(
                                   sliderInput(inputId = "fromGlucose",
                                               label = "Glucose average from",
                                               min = min(stroke$avg_glucose_level, na.rm = TRUE),
                                               max = max(stroke$avg_glucose_level, na.rm = TRUE),
                                               value = min(stroke$avg_glucose_level, na.rm = TRUE)),
                                   sliderInput(inputId = "toGlucose",
                                               label = "Glucose average to",
                                               min = min(stroke$avg_glucose_level, na.rm = TRUE),
                                               max = max(stroke$avg_glucose_level, na.rm = TRUE),
                                               value = max(stroke$avg_glucose_level, na.rm = TRUE))
                                 ),
                                 mainPanel(
                                   plotOutput("glucosePlot2"),
                                   plotOutput("glucosePlot")
                                 )
                               )
                      ),
                      tabPanel("BMI of stroke",
                               sidebarLayout(
                                 sidebarPanel(
                                   sliderInput(inputId = "frombmi",
                                               label = "BMI from",
                                               min = min(stroke$bmi, na.rm = TRUE),
                                               max = max(stroke$bmi, na.rm = TRUE),
                                               value = min(stroke$bmi, na.rm = TRUE)),
                                   sliderInput(inputId = "tobmi",
                                               label = "To",
                                               min = min(stroke$bmi, na.rm = TRUE),
                                               max = max(stroke$bmi, na.rm = TRUE),
                                               value = max(stroke$bmi, na.rm = TRUE)),
                                   img( height="200px", width="300px", align="left",
                                       src="Beo.jpg"),
                                 ),
                                 mainPanel(
                                   plotOutput("bmiPlot2"),
                                   plotOutput("BMIPlot")
                                 )
                               )
                      ),
                      tabPanel("Work Type",
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons(
                                     inputId  = "work",
                                     label = "Work Type",
                                     choices = unique(stroke$work_type),
                                     selected = ""
                                   ),
                                 ),
                                 mainPanel(
                                   plotOutput("workPlot")
                                 )
                               )
                      ),
                      tabPanel("Residence Type",
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons(
                                     inputId  = "residence",
                                     label = "Residence Type",
                                     choices = unique(stroke$Residence_type),
                                     selected = ""
                                   ),
                                 ),
                                 mainPanel(
                                   plotOutput("residencePlot")
                                 )
                               )
                      ),



           ),
           tabPanel("Prediction",
                    sidebarLayout(
                      sidebarPanel(
                        numericInput("age_p", "Your age:", 20, 1, 150),
                        selectInput(
                          inputId  = "gender_p",
                          label = "Select gender",
                          choices = c("Male"=1,"Female"=0),
                          selected = "Male"
                        ),
                        selectInput(
                          inputId  = "worktype_p",
                          label = "Select work type",
                          choices = c( "Private"=1, "Self-employed"=2, "Govt_job"=3, "children"=4, "Never_worked"=5),
                          selected = "Self-employed"
                        ),
                        selectInput(
                          inputId  = "residence_p",
                          label = "Live in?",
                          choices = c("Urban"=1,"Rural"=0),
                          selected = "Urban"
                        ),
                        selectInput(
                          inputId  = "hypertension_p",
                          label = "Select Hypertenstion",
                          choices = c("Yes"=1, "No"=0),
                          selected = "No"
                        ),
                        selectInput(
                          inputId  = "heart_p",
                          label = "Heart dicease",
                          choices = c("Yes"=1, "No"=0),
                          selected = "No"
                        ),
                        sliderInput(inputId = "bmi_p",
                                    label = "BMI",
                                    min = min(df$bmi, na.rm = TRUE),
                                    max = max(df$bmi, na.rm = TRUE),
                                    value = mean(df$bmi, na.rm = TRUE)
                                    ),
                        sliderInput(inputId = "glucose_p",
                                    label = "Average Glucose",
                                    min = min(df$avg_glucose_level, na.rm = TRUE),
                                    max = max(df$avg_glucose_level, na.rm = TRUE),
                                    value = mean(df$avg_glucose_level, na.rm = TRUE)),
                        ),

                      mainPanel(

                        verbatimTextOutput("predict"),
                        verbatimTextOutput("predictSummary")
                      )
                    )
           ),
           navbarMenu("More",


                      tabPanel("About",
                               fluidRow(

                                 ),
                                 column(3,
                                        img(class="img-polaroid",
                                            src=paste0("https://upload.wikimedia.org/",
                                                       "wikipedia/commons/b/b4//",
                                                       "Blausen_0836_Stroke.png")),
                                        tags$small(
                                          "Source: Photographed at the Bay State Antique ",
                                          "Automobile Club's July 10, 2005 show at the ",
                                          "Endicott Estate in Dedham, MA by ",
                                          a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                            "User:Sfoskett")
                                        )
                                 )
                               )
                      )
           )

server = function(input, output, session) {
  output$plot <- renderPlot({
    plot(stroke[10:9], type=input$plotType, col = "blue")
  })

  output$agePlot <- renderPlot({
    ggplot(stroke[ stroke$age <= input$toAge & stroke$age >= input$fromAge,], aes(x = stroke))+
      geom_bar(col = 'black', fill = "#FF6666")

  })

  output$age3Plot <- renderPlot(
    {
      ggplot(df, aes(x = age, fill = stroke_t))+
        geom_density(alpha = 0.3)
    }
  )
  output$bmiPlot2 <- renderPlot(
    {
      ggplot(df, aes(x = bmi, fill = stroke_t))+
        geom_density(alpha = 0.3)
    }
  )

  output$multiPlot1 <- renderPlot(
    {
      df %>%
        select(age, avg_glucose_level, bmi) %>%
        drop_na() %>%
        ggpairs()
    }
  )

  output$multiPlot2 <- renderPlot(
    {
      ggplot(df, aes(x=!!sym(input$multi2), y=age, fill = !!sym(input$multi2))) +
        geom_boxplot()
    }
  )
  output$workPie <- renderPlot(
    {
      pie( table(stroke$work_type),
           col = c("white", "gray90", "gray60", "black","gray30") )
    }
  )

  output$genderPlot <- renderPlot({
    ggplot(stroke[ stroke$gender == input$gender,], aes(x = stroke))+
      geom_bar(col = 'black', fill = "#FFF000")
  })

  output$hypertensionPlot <- renderPlot({
    ggplot(stroke[ stroke$hypertension == input$hypertension,], aes(x = stroke))+
      geom_bar(col = 'black', fill = "#666FFF")
  })
  output$hypertensionToHeartPlot <- renderPlot({
    ggplot(stroke[ stroke$hypertension == input$hypertensionToHeart,], aes(x = heart_disease))+
      geom_bar(col = 'black', fill = "#666FFF")
  })

  output$smokePlot <- renderPlot({
    ggplot(stroke[ stroke$smoking_status == input$smoke,], aes(x = stroke))+
      geom_bar(col = 'black', fill = "#A284BF")
  })

  output$heartPlot <- renderPlot({
    ggplot(stroke[ stroke$heart_disease == input$heart,], aes(x = stroke))+
      geom_bar(col = 'black', fill = "#FF6666")
  })
  output$workPlot <- renderPlot({
    ggplot(stroke[ stroke$work_type == input$work,], aes(x = stroke))+
      geom_bar(col = 'black', fill = "pink")
  })
  output$residencePlot <- renderPlot({
    ggplot(stroke[ stroke$Residence_type == input$residence,], aes(x = stroke))+
      geom_bar(col = 'black', fill = "green")
  })
  output$BMIPlot <- renderPlot({
    ggplot(stroke[ stroke$bmi <= input$tobmi & stroke$bmi >= input$frombmi,], aes(x = stroke))+
      geom_bar(col = 'black', fill = "#10A8BF")
  })
  output$glucosePlot <- renderPlot({
    ggplot(stroke[ stroke$avg_glucose_level >= input$fromGlucose & stroke$avg_glucose_level <= input$toGlucose,], aes(x = stroke))+
      geom_bar()
  })
  output$glucosePlot2 <- renderPlot(
    {
      ggplot(df, aes(x =avg_glucose_level, fill = stroke_t))+
        geom_density(alpha = 0.3)
    }
  )



  output$predictSummary <- renderPrint(
    {
      summary(model)
    }
  )
  output$predict <- renderPrint(
    {
      probability <- model %>% predict(data.frame(gender = as.integer(input$gender_p), age = input$age_p, hypertension = as.integer(input$hypertension_p), heart_disease = as.integer(input$heart_p), work_type =as.integer(input$worktype_p), Residence_type =as.integer(input$residence_p), avg_glucose_level = input$glucose_p, bmi = input$bmi_p), type = "response")*100
      cat("Khả năng bị đột quỵ của người này là ",probability , "%")
    }
  )

  output$summary <- renderPrint({
    summary(stroke)
  })

  output$table <- DT::renderDataTable({
    DT::datatable(stroke)
  })
}
shinyApp(ui, server)

