rawData <- sensataDataProg::sensataExample
Dict <- dictGenerator(
  df = rawData,
  questionPrefix = "",
  forceOrdered = "q_AB_NI_01")

questions <- c("q_AB_NI_01", "q_AB_EA_07", "q_AB_RA_01")

disag <- c("q_AB_CA_07", "q_AB_CA_09")

labels <- c("Marital status", "Student")

topic <- c("TEMA 1", "TEMA 2", "TEMA 2")

createExtendedDict(dict = Dict,
                   questions = questions,
                   disag = disag,
                   labels = labels,
                   topic = topic)

