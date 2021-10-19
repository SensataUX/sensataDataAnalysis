library(devtools)
library(roxygen2)
load_all()
sensataExAnalysis <- sensataExAnalysis

# 5 options (likert)
createGraphData(df = sensataExAnalysis, originVar = "q_EA_IN_03", groupVar = "q_EA_CA_10")

# 4 options (text)
createGraphData(df = sensataExAnalysis, originVar = "q_EA_IN_03", groupVar = "q_EA_CA_10")
