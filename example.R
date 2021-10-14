library(devtools)
library(roxygen2)
load_all()
sensataExAnalysis <- sensataExAnalysis

createGraphData5option(df = sensataExAnalysis, originVar = "q_EA_IN_03", groupVar = "q_EA_CA_10")
