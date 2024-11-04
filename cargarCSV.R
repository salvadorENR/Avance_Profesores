library(RMySQL)
library(DBI)
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "sql3720918",
                 host = "sql3.freesqldatabase.com",
                 port = 3306,
                 user = "sql3720918",
                 password = "kHY2iadreR")

yes##Lee el csv
datos <- read.csv('mis_datos.csv')

# Escribe los datos del csv en la base de datos
#dbWriteTable(con, name = "page_data", value = datos[-1,], row.names = FALSE, overwrite = FALSE)


##extraer datos y guardar en csv
resultados <- dbGetQuery(con, "SELECT * FROM page_data")
write.csv(resultados, "respaldoDBJunio.csv")

# Ejecutar la consulta SQL para eliminar todos los datos
#dbSendQuery(con, "DELETE FROM page_data")

# Ejecutar la consulta SQL para obtener el tamaño de la tabla "page_data"
consulta <- "SELECT table_name AS 'Tabla', ROUND(((data_length + index_length) / 1024 / 1024), 2) AS 'Tamaño (MB)'
             FROM information_schema.tables
             WHERE table_schema = 'sql3709755' AND table_name = 'page_data'"

tamaño <- dbGetQuery(con, consulta)

# Imprimir el tamaño en MB
print(tamaño)
# Cerrar la conexión
dbDisconnect(con)
