# Install the rsconnect package if it's not already installed
install.packages("rsconnect")

# Load the rsconnect package
library(rsconnect)

# Set your shinyapps.io account information
rsconnect::setAccountInfo(name='mined',
                          token='D9838BF3F50E873531DB43A0A6086DD7',
                          secret='D4ONbu50XZ8keiXTpGtnkLLV7sSJUMfHmKTkNvki')

# Set the working directory to where your app.R is located
#setwd("C:/Users/kevin/OneDrive/Documents/GitHub/Avance_Profesores")
#setwd("C:/Users/Wendy Rodríguez/Documents/Avance_Profesores")
setwd("C:/Users/KSerrano/Documents/GitHub/Avance_Profesores")

# Deploy the application
#rsconnect::deployApp("C:/Users/kevin/OneDrive/Documents/GitHub/Avance_Profesores")
#rsconnect::deployApp("C:/Users/Wendy Rodríguez/Documents/Avance_Profesores")
rsconnect::deployApp("C:/Users/KSerrano/Documents/GitHub/Avance_Profesores")


