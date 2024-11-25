library(RMySQL)
library(DBI)

con <- dbConnect(RMySQL::MySQL(),
                 dbname = "sql3742502",
                 host = "sql3.freesqldatabase.com",
                 port = 3306,
                 user = "sql3742502",
                 password = "hyRBRPmM1E")

yes##Lee el csv
datos <- read.csv('mis_datos.csv')

# Escribe los datos del csv en la base de datos
#dbWriteTable(con, name = "page_data", value = datos[-1,], row.names = FALSE, overwrite = FALSE)

# Verifica cuántos registros cumplen la condición antes de eliminar
before_delete <- dbGetQuery(con, "SELECT COUNT(*) AS count FROM page_data WHERE Page BETWEEN 0 AND 10")
cat("Registros con Page entre 0 y 10 antes de eliminar:", before_delete$count, "\n")

# Eliminar los registros
affected_rows <- dbExecute(con, "DELETE FROM page_data WHERE Page BETWEEN 0 AND 10")
cat("Registros eliminados:", affected_rows, "\n")

# Verifica nuevamente
after_delete <- dbGetQuery(con, "SELECT COUNT(*) AS count FROM page_data WHERE Page BETWEEN 0 AND 10")
cat("Registros con Page entre 0 y 10 después de eliminar:", after_delete$count, "\n")



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
