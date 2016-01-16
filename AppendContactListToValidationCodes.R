# Import the contacts to add
Contacts <- read.csv("Contacts.csv")

# Load the random library in order to generate survey validation codes
library(random)

# Set the x variable equal to the length of the new contacts to add
x <- length(Contacts[,1])

# Create a new data frame to add to Validation.Code
Validation.Code.New <- data.frame(Code = randomStrings(n=x, len=5), Count = rep(0,x), Name = Contacts$Name, Title = Contacts$Title, Organization = Contacts$Organization, ResponseNo = rep(0,x), Email = rep(NA,x), Contact = rep(0,x))

# Change first column name to append it
colnames(Validation.Code.New)[1] <- "Code"

# Add rows to Validation.Code
Validation.Code <- rbind(Validation.Code, Validation.Code.New)

# Save the results
save(Validation.Code, file="Validation.Rdata")