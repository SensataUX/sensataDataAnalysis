# Lego movilizatorio tablas de frecuencias
# Survey id: General: 4lCe7ANQx2lvr8oWCBwSsb, Antioquia: 4ERuLlrKHahPCskP48QA6I, qr: 6Hzl7eAuhJQVgSslieTAVz, offline: 3deFwUPGLoIybfXniriUGZ
# Created by: Gabriel N. Camargo-Toledo
# Created on: Nov/23/2021
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Nov/29/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook Pop!_OS 21.04 8gb Ram R4.1.2


# INIT --------------------------------------------------------------------
library(sensataDataAnalysis)
library(openxlsx)
not_all_na <- function(x) any(!is.na(x))

legoIntData <- readRDS("~/sensataux/sensata2021/projects/LegoMovilizatorio/data/interim/legoDic03Data.rds") %>% select(-(contains("_time")))
legoIntDataSinSal <- legoIntData
legoIntDataSinSal$createdAt <- as.character(legoIntDataSinSal$createdAt)
legoIntDataSinSal[legoIntDataSinSal=="Saltar pregunta"] <- NA
legoIntDataSinSal <- droplevels(legoIntDataSinSal)
var_label(legoIntDataSinSal) <- var_label(legoIntData)


# Create tableau matrices ---------------------

rows4Table <- legoIntDataSinSal %>%
  select(starts_with("q_MV"),
         Edad,
         NSE,
         -q_MV_CA_12,
         -q_MV_CA_03,
         -q_MV_CA_23_OPC_OTRO,
         q_MV_CA_04_Tot,
         EdadHijo_Tot,
         Ciudad
  ) %>%
  select(where(not_all_na)) %>%
  names()

temaVec <- c("PREGUNTA 1" = "TEMA 1",
             "PREGUNTA 2" = "TEMA 1",
             "PREGUNTA 3" = "TEMA 2",
             "PREGUNTA 4" = "TEMA 2")

# DICCIONARIO EXTENDIDO CON ROWS (questions) COLS (desagreg) LABELS (de las desagreg) Y TEMAS, COMENTARIOS, ENUNCIADO ABREVIADO,

rowsTot <- legoIntDataSinSal %>% select(ends_with("_TOT")) %>% n

tabFreq <- createFreqTables(df = legoIntDataSinSal,
                            rows = rows4Table,
                            cols = c("PMC",
                                     "q_MV_CA_13",
                                     "PadMadCuid",
                                     "Edad",
                                     "NSE",
                                     "Ciudad",
                                     "q_MV_CA_03",
                                     "q_MV_CA_04_Tot",
                                     "EdadHijo_Tot"),
                            labels = c("PMC",
                                       "Género",
                                       "Padre, madre, cuidador o cuidadora",
                                       "Edad",
                                       "Nivel socioeconómico",
                                       "Ciudad",
                                       "Número de hijos",
                                       "Sexo niño o niña",
                                       "Edad niño o niña"),
                            tema = c("PREGUNTA 1" = "TEMA 1",
                                     "PREGUNTA 2" = "TEMA 1",
                                     "PREGUNTA 3" = "TEMA 2",
                                     "PREGUNTA 4" = "TEMA 2"
                                     ...),
                            addIdentifier = T,
                            wide = T)

tabPer <- createFreqTables(df = legoIntDataSinSal,
                           rows = rows4Table,
                           cols = c("PMC",
                                    "q_MV_CA_13",
                                    "PadMadCuid",
                                    "Edad",
                                    "NSE",
                                    "Ciudad",
                                    "q_MV_CA_03",
                                    "q_MV_CA_04_Tot",
                                    "EdadHijo_Tot"),
                           labels = c("PMC",
                                      "Género",
                                      "Padre, madre, cuidador o cuidadora",
                                      "Edad",
                                      "Nivel socioeconómico",
                                      "Ciudad",
                                      "Número de hijos",
                                      "Sexo niño o niña",
                                      "Edad niño o niña"),
                           addIdentifier = T,
                           percent = T,
                           wide = T)

tabFreqTot <- createFreqTables(df = legoIntDataSinSal,
                               rows = rows4Table,
                               addIdentifier = T,
                               wide = T)

tabPerTot <- createFreqTables(df = legoIntDataSinSal,
                              rows = rows4Table,
                              addIdentifier = T,
                              wide = T,
                              percent = T)

outputWb <- createWorkbook()
modifyBaseFont(outputWb, fontSize = 11, fontName = "Monserrat")
addWorksheet(outputWb, sheetName = "Frec", gridLines = FALSE)
addWorksheet(outputWb, sheetName = "Porcentaje", gridLines = FALSE)
addWorksheet(outputWb, sheetName = "Total", gridLines = FALSE)
addWorksheet(outputWb, sheetName = "TotalPorcentaje", gridLines = FALSE)


writeDataTable(outputWb, sheet = 1, tabFreq, colNames = T)
writeDataTable(outputWb, sheet = 2, tabPer, colNames = T)
writeDataTable(outputWb, sheet = 3, tabFreqTot, colNames = T)
writeDataTable(outputWb, sheet = 4, tabPerTot, colNames = T)

saveWorkbook(outputWb, paste0("results/[LegoMov] tablas frecuencia sin saltar",
                              format(Sys.time(),
                                     "%Y_%m_%d"), ".xlsx"), overwrite = T)
