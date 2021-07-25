# https://irene.rbind.io/post/using-sql-in-rstudio/
# https://db.rstudio.com/

library(dplyr)
library(dbplyr)

conn <- src_memdb() # create a SQLite database in memory
copy_to(conn, 
        storms,     # this is a dataset built into dplyr
        overwrite = TRUE)
tbl(conn, sql("SELECT * FROM storms LIMIT 5"))
