# examine and extract bits of old blog from wordpress MySQL database
library(tidyverse)
library(DBI)
library(dbplyr)


mysql_file = "data\artsteinmetz_wordpress.sql"

