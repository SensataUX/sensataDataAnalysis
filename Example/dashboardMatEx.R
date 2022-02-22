devtools::install_github(repo = "https://github.com/SensataUx/sensataDataAnalysis", ref = "main", build_vignettes = T)

library(sensataDataAnalysis)

rawData <- sensataDataProg::sensataExample
Dict <- dictGenerator(
  df = rawData,
  questionPrefix = "",
  forceOrdered = "q_AB_NI_01")

questions <- c("q_AB_NI_01", "q_AB_EA_07", "q_AB_RA_01")

disag <- c("q_AB_CA_07", "q_AB_CA_09")

labels <- c("Marital status", "Student")

topic <- c("TEMA 1", "TEMA 2", "TEMA 2")

extDict <- createExtendedDict(dict = Dict,
                              questions = questions,
                              disag = disag,
                              labels = labels,
                              topic = topic)

intData <- rawData %>% cleanCols(Dict)
intData <- intData %>% makeFactors(Dict)

dashMatrix <- createDashboardMatrix(extDict, intData)

