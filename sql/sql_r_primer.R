# https://irene.rbind.io/post/using-sql-in-rstudio/
# https://db.rstudio.com/

library(dplyr)
library(dbplyr)

conn <- src_memdb() # create a SQLite database in memory
copy_to(conn, 
        storms,     # this is a dataset built into dplyr
        overwrite = TRUE)
tbl(conn, sql("SELECT * FROM storms LIMIT 5"))



library(odbc)

con <- dbConnect(RPostgres::Postgres(), 
                 user = "postgres", # r_user
                 password = rstudioapi::askForPassword("Database password"))

sort(unique(odbcListDrivers()[[1]]))

con_odbc <- DBI::dbConnect(odbc::odbc(),
                       Driver   = "PostgreSQL ODBC Driver(UNICODE)",
                       Server   = "localhost",
                       Database = "db_r_test",
                       UID      = rstudioapi::askForPassword("Database user"),
                       PWD      = rstudioapi::askForPassword("Database password"),
                       Port     = 5432)

# Top level objects
odbcListObjects(con_odbc)

# Tables in a schema
odbcListObjects(con_odbc, catalog="mydb", schema="dbo")

# Columns in a table
odbcListColumns(con_odbc, catalog="mydb", schema="dbo", table="cars")

# Database structure
odbcListObjectTypes(con_odbc)

# All data sources
odbcListDataSources()

# All drivers
odbcListDrivers()

copy_to(con_odbc, 
        storms,     
        overwrite = TRUE)
tbl(con_odbc, sql("SELECT * FROM storms LIMIT 5"))

dbListTables(con_odbc)
dbListFields(con_odbc, "storms")
if(dbExistsTable(con_odbc, "storms"))
  dbRemoveTable(con_odbc, "storms")
dbWriteTable(con_odbc, "storms", storms)

tbl(con_odbc, sql("SELECT * FROM storms LIMIT 5"))

# Remote
q1 <- tbl(con_odbc, "storms") %>% 
  select(name:hour) %>% 
  filter(year == 1977)

# Compute query and save in remote table
compute(q1)

# Compute query bring back to this session
collect(q1)

# Creates a fresh query based on the generated SQL
collapse(q1)

DBI::dbDisconnect(con_odbc)
