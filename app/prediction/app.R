# library(shiny)
# options(shiny.port = 8889)
# runApp('.')

library(shiny)
library(tidyverse)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Predict prolonged/multiple vasopressor requirement"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select quotes ----
      radioButtons("careunit", "Current ICU disposition",
                   choices = c("MICU" = "Medical Intensive Care Unit (MICU)",
                               "MICU/SICU" = 'Medical/Surgical Intensive Care Unit (MICU/SICU)',
                               "Neuro SICU" = "Neuro Surgical Intensive Care Unit (Neuro SICU)",
							   "SICU" = "Surgical Intensive Care Unit (SICU)",
							   "TSICU" = "Trauma SICU (TSICU)'",
							   "CVICU" = "Cardiac Vascular Intensive Care Unit (CVICU)"
							   ),
                   selected = '"'),

      # Horizontal line ----
      tags$hr(),
	  
	  checkboxInput("intub", "Currently intubated?", FALSE),
	  
	  tags$hr(),

	  checkboxInput("pressor_choice", "Will norepinephrine be needed?", FALSE),
	  
	  tags$hr(),
	  
	  checkboxInput("creatinine", "Creatinine over 1.2 mg/dL?", FALSE),
	  
	  tags$hr(),
	  
	  checkboxInput("ed_admission", "Admitted from ED?", FALSE),
	  
	  tags$hr(),
	  
	        radioButtons("simple_diagnosis", "Diagnosis Type",
	                     choices = c("Infectious" = "INFECTION",
	                                 "Other" = 'OTHER DIAGNOSIS',
	                                 "Cardiac" = "CARDIAC"),
	                     selected = '"'),


	  tags$hr(),

	        radioButtons("gender", "Gender",
	                     choices = c("Male" = "M",
	                                 "Female" = "F"),
	                     selected = '"'),


	  tags$hr(),
	  
	  textInput("mbp", "Mean Blood Pressure (in mmHg)", ""),
	  
	  tags$hr(),
	  
	  textInput("hr", "Heart Rate (in bpm)", ""),
	  
	  tags$hr(),
	  
	  textInput("rr", "Respiratory Rate (in bpm)", ""),
	  
	  tags$hr(),
	  
	  textInput("spo2", "SpO2 (in %)", ""),
	  
	  tags$hr(),
	  
	  textInput("age_cohort", "Age (years)", ""),
	  
	   # Horizontal line ----	   
	   tags$hr(),
				   
	   actionButton("submit", label = "Submit", class = "btn-success")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
	  
	  p("This model reports the likelihood that a patient in the ICU will require more than 24 hours of vasopressor support or receive more than one vasopressor at the same time. That is, this model will report the likelihood that a patient will fulfill the indication for central venous catheter placement for vasopressor administration utilized by most hospital protocols. Model statistics are provided in the reference below. Source code available here https://github.com/ruoyijiang/haimovich_pressors."),
	  br(),
	  h6("Haimovich, Adrian D., et al. “Risk Factor Identification and Predictive Models for Central Line Requirements for Patients on Vasopressors.” Anaesthesia and Intensive Care, vol. 49, no. 4, July 2021, pp. 275–83. PubMed, https://doi.org/10.1177/0310057X211024258."),
	  br(),
	  
      # Output: Data file ----
      tableOutput("contents")

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

	req(input$careunit)
	#req(input$intub)
	req(input$simple_diagnosis)
	req(input$age_cohort)
	req(input$gender)
	#req(input$creatinine)
	#req(input$ed_admission)
	#req(input$pressor_choice)
	
	req(input$mbp)
	req(input$hr)
	req(input$rr)
	req(input$spo2)
	#print(input$careunit)
	#req(input$chain)
	#req(input$submit)
	
	output_df <- data.frame(
			#CAREUNIT = input$careunit,
			first_careunit=input$careunit,
			intub=input$intub %>% as.character(),
			simple_diagnosis=input$simple_diagnosis,
			age_cohort=as.numeric(input$age_cohort),
			gender=input$gender,
			creatinine=input$creatinine %>% as.character(),
			ed_admission=input$ed_admission %>% as.character(),
			pressor_choice=input$pressor_choice %>% as.character(), 
			MBP=as.numeric(input$mbp),
			HR=as.numeric(input$hr),
			RR=as.numeric(input$rr),
			SPO2=as.numeric(input$spo2)
			)
	
	model <- readRDS("./model.rds") 
	
	odds <- predict(model, newdata=output_df) %>% exp()
	#
	result_df <- data.frame(
		LIKELIHOOD = odds / (1+odds)
	)
	
	req(result_df)
	
	isolate({

	return(result_df)
	
	})
		
  })

}
# Run the app ----
shinyApp(ui, server)