library(shiny)
library(shinyjs)
library(stringr)
library(googleVis)
library(reshape)

# Read the survey questions
Qlist <- read.csv("Qlist.csv")
# Qlist <- Qlist[1,]

# Function to hide the next button
  hidenextbutton <- function(){
  toggle("Click.Counter")
  }
  
# Function to hide the loading image
  hideloadingimage <- function(){
  toggle("loading")
  }

# Function to hide the loading image
  hidesurveyresults <- function(){
  show("results")
  }

shinyServer(function(input, output) {
  
  # Create an empty vector to hold survey results
  results <<- rep("", nrow(Qlist))
  # Name each element of the vector based on the
  # second column of the Qlist
  names(results)  <<- Qlist[,3]
  
  # Hit counter
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) counter <- 0
      if (file.exists("counter.Rdata")) load(file="counter.Rdata")
      counter <- counter <<- counter + 1
      
      save(counter, file="counter.Rdata")     
      paste0("Visitors: ", counter)
    })

	# Load data for demographics
	  if (file.exists("survey.results.Rdata")) {
		load(file="survey.results.Rdata")
		
		#Create demographic data frames dynamically
		for (i in 1:3)
			{
			presults.1 <- unique(presults[,i])

			presults.1.df <- data.frame(sapply(presults.1, str_count, presults[,i]))

			presults.1.sum <- data.frame(Answer=presults.1,Total=sapply(presults.1.df,sum))

			colnames(presults.1.sum)[1] <- colnames(presults)[i]

			assign(paste("Demographic.",i,sep=""), presults.1.sum)
			}
		
		
		  # This function renders the table of results from the
		  # survey.
		  output$Demographic.1 <- renderTable({
			Demographic.1}, include.rownames=FALSE
		)
		
		  # Creates a column chart for output
		  # output$Demographic.1.plot <- renderGvis({
			# gvisColumnChart(Demographic.1,
				# options=list(title="Respondent Role",
				# legend="none"
				# )
			# )
		  # })
		
		output$Demographic.2 <- renderTable({
			Demographic.2}, include.rownames=FALSE
		)		
		
		output$Demographic.3 <- renderTable({
			Demographic.3}, include.rownames=FALSE
		)		
	}
	
  
  # This renderUI function holds the primary actions of the
  # survey area.
  output$MainAction <- renderUI( {
    return(list(dynamicUi(),hideloadingimage()))
  })
  
  # Dynamic UI is the interface which changes as the survey
  # progresses.  
  dynamicUi <- reactive({
	# At the end of the survey, display the results
    if (input$Click.Counter>nrow(Qlist))
    {  return(
        list(
          h4("Thanks for taking the survey!  You may now view the results of the survey as well as benchmarking data in the 'Results' at the bottom of this page."),
          downloadButton('downloadData', 'Download Individual Results'),
		  hidenextbutton(),
		  hideloadingimage(),
      hidesurveyresults()
          )
        )
	}
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist))  
    {
	return(
        list(
          h3(textOutput("question")),
          radioButtons("survey", "Please, select the most appropriate option:", 
            c("Prefer not to answer", option.list())),
		  hideloadingimage()			
          )
        )
	}   
  })
  
  if (file.exists("Validation.Rdata")) {load(file="Validation.Rdata")}
  
  # Check to see if the validation code exists within the data frame and has already been used
  shinyjs::onclick("Submit.Code",    
                   if(Validation.Code[Validation.Code$Code == input$Validation.Code, 2] < 1)
                   {
                     shinyjs::toggle(id="survey")
                     shinyjs::hide(id="Validation")
                     Validation.Code[Validation.Code$Code == input$Validation.Code, 2] <- 1
                     save(Validation.Code, file="Validation.Rdata")
                   }
                   else
                   {
                     shinyjs::info("Invalid code.  Please, try again.")
                   }
  )
  
  # This reactive function is concerned primarily with
  # saving the results of the survey for this individual.
  output$save.results <- renderText({
    # After each click, save the results of the radio buttons.
    if ((input$Click.Counter>0)&(input$Click.Counter<=nrow(Qlist)))
	{
      try(results[input$Click.Counter] <<- input$survey)
    }
	# try is used because there is a brief moment in which
      # the if condition is true but input$survey = NULL
    
    # If the user has clicked through all of the survey questions
    # then R saves the results to the survey file.
    else if (input$Click.Counter>nrow(Qlist)) {
      if (file.exists("survey.results.Rdata")) 
        load(file="survey.results.Rdata")
      if (!file.exists("survey.results.Rdata")) 
        presults<-NULL
      presults <- presults <<- rbind(presults, results)
      rownames(presults) <- rownames(presults) <<- 
        paste("Respondent ", 1:nrow(presults))
      save(presults, file="survey.results.Rdata")
	  
	  ######
	  #Sector and Market Cap Score Calcs
	  ######
	  
		#Load maturity model values and Qlist
		load(file="maturity.model.values.Rdata")

		#Get category names
		QCategory <- Qlist[1:nrow(Qlist),1:2]

		#Get responses
		QResponse <- unique(Qlist[,1])

		#Create a copy of presults
		pcategories <- presults[,1:ncol(presults)]

		#Rename columns after categories instead of questions
		colnames(pcategories) <- QCategory[,2]

		#Create loop to replace all values
		#For loop for columns is letter i
		for (i in 4:ncol(pcategories)){
		#For loop for rows is letter j
		  for (j in 1:nrow(pcategories)){
		  
			pcategories[j,i] <- MaturityModelValues[MaturityModelValues$Category==pcategories[j,i],2]
		  }}

		#Create unique sector, market cap, and category lists for loop
		unique.sectors <- unique(pcategories[,2])
		
		unique.marketcap <- unique(pcategories[,3])

		unique.categories <- unique(colnames(pcategories[,4:ncol(pcategories)]))

		## Market Cap Calculation
		#Create a score data frame
		pcategories.score.marketcap <- data.frame(MarketCap=unique.marketcap, Category=sort(rep(unique.categories, length(unique.marketcap))), Score=0)

		#Replace pcategories score with means from survey results
		for (i in 1:length(unique.marketcap)){
		  
		  for (j in 1:length(unique.categories)){
			
			tmp.vector <- as.vector(as.numeric(pcategories[pcategories[,3]==unique.marketcap[i],colnames(pcategories)==unique.categories[j]]))
			
			pcategories.score.marketcap[pcategories.score.marketcap$MarketCap==unique.marketcap[i] & pcategories.score.marketcap$Category==unique.categories[j],3] <- round(mean(tmp.vector[!is.na(tmp.vector)]),2)
		  }}

		pcategories.score.marketcap <- pcategories.score.marketcap[complete.cases(pcategories.score.marketcap),]

		pcategories.score.marketcap <- cast(melt(pcategories.score.marketcap),MarketCap~Category)
		
		## Sector Calculation
		#Create a score data frame
		pcategories.score <- data.frame(Sector=unique.sectors, Category=sort(rep(unique.categories, length(unique.sectors))), Score=0)

		#Replace pcategories score with means from survey results
		for (i in 1:length(unique.sectors)){
		  
		  for (j in 1:length(unique.categories)){
			
			tmp.vector <- as.vector(as.numeric(pcategories[pcategories[,2]==unique.sectors[i],colnames(pcategories)==unique.categories[j]]))
			
			pcategories.score[pcategories.score$Sector==unique.sectors[i] & pcategories.score$Category==unique.categories[j],3] <- round(mean(tmp.vector[!is.na(tmp.vector)]),2)
		  }}

		pcategories.score <- pcategories.score[complete.cases(pcategories.score),]	  	
		
		pcategories.score <- cast(melt(pcategories.score),Sector~Category)
		
		#####
		# Self-assessment score calculation
		#####
			#Create a score data frame for the last input
			pcategories.score.individual <- data.frame(Category=unique(colnames(pcategories[,4:ncol(pcategories)])), Score=0)

			#Replace pcategories score with means from survey results
			unique.categories <- unique(pcategories.score.individual$Category)
			  
			  for (j in 1:length(unique.categories)){
				
				tmp.vector <- as.vector(as.numeric(pcategories[nrow(pcategories),colnames(pcategories)==unique.categories[j]]))
				
				pcategories.score.individual[pcategories.score.individual$Category==unique.categories[j],2] <- round(mean(tmp.vector[!is.na(tmp.vector)]),2)
			  }

			pcategories.score.individual <- pcategories.score.individual[complete.cases(pcategories.score.individual),]
	  
	    # # This function renders the table of results from the
		  # # survey.
		  # output$surveyresults <- renderGvis({
			# gvisColumnChart(pcategories.score[pcategories.score$Sector==results[2],c(2,3)],
				# options=list(
					# title=paste(results[2], " Average Scores"),
					# legend="none"
				# )
			# )
		  # })
		  
	    # This function renders the table of results from the
		  # survey.
		  output$surveyresultsindividual <- renderGvis({
			gvisColumnChart(pcategories.score.individual,
				options=list(
					title="Score Based on Self-Assessment",
					legend="none"
				)
			)
		  })		  
		  
		# Render data tables for sector and market
		  output$surveyresultstable <- renderDataTable({
			pcategories.score}, options=list(bInfo=FALSE, paging=FALSE, searching=FALSE, searchable=FALSE)
		  )
		  
		  output$surveyresultstablemarketcap <- renderDataTable({
			pcategories.score.marketcap}, options=list(bInfo=FALSE, paging=FALSE, searching=FALSE, searchable=FALSE)
		  )
	  }
    # Because there has to be a UI object to call this
    # function I set up render text that distplays the content
    # of this funciton.
    ""
  })
  
  # This renders the data downloader
  output$downloadData <- downloadHandler(
    filename = "IndividualData.csv",
    content = function(file) {
      write.csv(presults, file)
    }
  )
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list <- reactive({
    qlist <- Qlist[input$Click.Counter,4:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function shows the question number (Q:)
  # Followed by the question text.
  output$question <- renderText({
    paste0(
      input$Click.Counter," of ", nrow(Qlist),":  ", 
      Qlist[input$Click.Counter,3]
    )
  })
  
})