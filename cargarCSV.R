library(RMySQL)
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "sql3709755",
                 host = "sql3.freesqldatabase.com",
                 port = 3306,
                 user = "sql3709755",
                 password = "PPK48mhriE")

datos <- read.csv('mis_datos.csv')

# Escribe los datos del csv en la base de datos
dbWriteTable(con, name = "page_data", value = datos[-1,], row.names = FALSE, overwrite = FALSE)

# Cierra la conexión
dbDisconnect(con)
