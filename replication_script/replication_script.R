## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----install/load packages-----------------------------------------------
# library(devtools)
# devtools::install_git(url = "https://gitlab.com/luckinet/software/arealDB")
# install.packages("readr", "magrittr")
library(arealDB)
library(readr)
library(magrittr)

## ----project setup, include=FALSE----------------------------------------
setPath(root = "/home/se87kuhe/my_science/r-dev/arealDB/vignettes/replication_data/newProject")

## ----show project setup, eval=FALSE--------------------------------------
#  setPath(root = ".../vignettes/replication_data/newProject")

## ----setup index tables--------------------------------------------------
read_csv(file = "replication_data/fao_commodities.csv", 
         col_types = "iclciccc") %>%
  setVariables(variable = "commodities", 
               pid = "faoID", 
               target = "simpleName")

read_csv(file = "replication_data/newProject/id_commodities.csv", col_types = "iclciccc")

read_csv(file = "replication_data/newProject/tt_commodities.csv", col_types = "cccic")

## ----setup translation tables--------------------------------------------
read_csv(file = "replication_data/nation_translations.csv", col_types = "cccDi") %>%
  setVariables(variable = "nations", 
               type = "tt", 
               origin = "origin", 
               target = "target")

read_csv(file = "replication_data/newProject/tt_nations.csv", col_types = "cccic")

read_csv(file = "replication_data/territory_translations.csv", col_types = "cccDi") %>%
  setVariables(variable = "territories", 
               type = "tt", 
               origin = "origin", 
               target = "target")

## ----register data-series------------------------------------------------
regDataseries(name = "gadm",
              description = "Database of Global Administrative Areas",
              website = "https://gadm.org/index.html",
              update = TRUE)

regDataseries(name = "ibge",
              description = "Instituto Brasileiro de Geografia e Estatistica",
              website = "https://sidra.ibge.gov.br/tabela/5457",
              update = TRUE)

regDataseries(name = "usda",
              description = "US Dept. of Agriculture",
              website = "https://www.nass.usda.gov/Quick_Stats/Lite/index.php",
              update = TRUE)

## ---- include=FALSE, eval=FALSE------------------------------------------
#  # little hack that is required to show the output of the tables, which could not
#  # happen otherwise, because the previous chunks need to be cached, which doesn't
#  # update the tables.
#  cen <- readRDS(file = "data/intermediateCensus.rds")
#  write_csv(x = cen, path = "newProject/inv_tables.csv")
#  geo <- readRDS(file = "data/intermediateGeometries.rds")
#  write_csv(x = geo, path = "newProject/inv_geometries.csv")

