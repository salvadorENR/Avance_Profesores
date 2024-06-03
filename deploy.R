# Install the rsconnect package if it's not already installed
install.packages("rsconnect")

# Load the rsconnect package
library(rsconnect)

# Set your shinyapps.io account information
rsconnect::setAccountInfo(name='salvadorhernandez',
                          token='1BCDF7FEFCE106A56040AC3AF45F2380',
                          secret='Xu/+W3YeXdivSmXgmvOavpZAi6I9TWMGC/Hv+6Fi')

# Set the working directory to where your app.R is located
setwd("C:/Users/SERHernandez/Documents/PsycologicWellBeing/Avance_Profesores")

# Deploy the application
rsconnect::deployApp("C:/Users/SERHernandez/Documents/PsycologicWellBeing/Avance_Profesores")



