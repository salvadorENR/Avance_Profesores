# Install the rsconnect package if it's not already installed
install.packages("rsconnect")

# Load the rsconnect package
library(rsconnect)

# Set your shinyapps.io account information
rsconnect::setAccountInfo(name='salvadorhernandez',
                          token='1BCDF7FEFCE106A56040AC3AF45F2380',
                          secret='Xu/+W3YeXdivSmXgmvOavpZAi6I9TWMGC/Hv+6Fi')

# Set the working directory to where your app.R is located
setwd("C:/Users/kevin/OneDrive/Documents/GitHub/Avance_Profesores")
#setwd("C:/Users/Wendy Rodríguez/Documents/Avance_Profesores")

# Deploy the application
rsconnect::deployApp("C:/Users/kevin/OneDrive/Documents/GitHub/Avance_Profesores")
#rsconnect::deployApp("C:/Users/Wendy Rodríguez/Documents/Avance_Profesores")



