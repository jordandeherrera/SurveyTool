library(shiny)
library(shinyjs)
library(googleVis)

# Define UI for slider demo application
shinyUI(fluidPage(
  
  # Include bootstrap theme
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
  
  #  Application title
  headerPanel("Project Management Survey"),
  
  # Loading image gif
  div(id="loading",
	img(id="loading-image", `src`="loading.gif", `alt`="Loading...")
  ),
  
  
  #  Jumbotron to explain the survey, etc.
  fluidRow(
  column(7,
  div(id="jumbotron",class="jumbotron",
	# Activates Shinyjs javascript
	useShinyjs(),
      
	  # This is intentionally an empty object.
      h6(textOutput("save.results")),
      
	  h2("About the Survey"),
	  h5("The following survey was designed as a self-evaluation and benchmarking tool to determine competencies across the categories listed in the Project Management Framework as determined by the Project Management Institute.  The competency levels follow the developmental model defined by the Capability Maturity Model framework.  A guide for the PMBOK can be found through the following link:"),
	  tags$a(href="http://www.cs.bilkent.edu.tr/~cagatay/cs413/PMBOK.pdf",class="btn btn-primary btn-lg","Learn More About PMBOK"),
    
	h2("How It Works"),
	h5("The survey tool is designed to provide valuable data about your organization's project management maturity model.  In addition to being a self-assessment tool, the survey tool gathers data from all respondents along with some basic demographic data in order to provide relevant benchmarks to all respondents.  Benchmark data is provided at the end of the survey.  All data is self-reported.  Current survey demographics are listed in the 'Survey Statistics' section and are automatically updated with each completed survey result."),
	
	h2("Survey Instructions"),
	tags$li("Click the 'Next' button to get started"),
	tags$li("Please, fill out demographic data as it helps in providing insight and relevant benchmarks to other respondents"),
	tags$li("Assess your organization's competency for each statement based on the most appropriate capability maturity level listed")
	)),

  
  # Show a table summarizing the values entered
  column(5,
	div(class="panel panel-default", 
	  div(id="Validation", class="panel-body",
	        h2("Validation Code"),
	        h4("Please, input a validation code before beginning the survey.  Survey questions will appear in this section when a valid code is entered.  The validation code helps ensure that all aggregate data reported is valid."),
	        textInput("Validation.Code","Validation Code:"),
	        actionButton("Submit.Code", "Submit",class="btn btn-primary")
	    ),
    div(class="panel-body",
			h2("Survey Questions"),
			# Main Action is where most everything is happenning in the
			# object (where the welcome message, survey, and results appear)
			uiOutput("MainAction"),
			# This displays the action putton Next.
			shinyjs::hidden(div(id="survey", actionButton("Click.Counter", "Next",class="btn btn-primary")))    
		)
    ))),
  fluidRow(
	column(12,
	div(class="panel panel-default", 
		div(class="panel-body",
			h2("Survey Demographics"),
			h5(textOutput("counter")),
			#htmlOutput("Demographic.1.plot"),			
			tableOutput("Demographic.1"),
			tableOutput("Demographic.2"),
			tableOutput("Demographic.3")
		)
	))),
	shinyjs::hidden(div(id="results",
    fluidRow(
  	column(12,
  	div(id="surveyresults",class="panel panel-default", 
  		div(class="panel-body",
  			h2("Survey Results"),
  			h3("Industry Summary"),
  			dataTableOutput("surveyresultstable"),
  			h3("Market Cap Summary"),
  			dataTableOutput("surveyresultstablemarketcap"),
  			h3("Self-Evaluation Summary"),
  			htmlOutput("surveyresultsindividual")
  			#htmlOutput("surveyresults")
  		  )
  	  )
    )
    )
	))
))