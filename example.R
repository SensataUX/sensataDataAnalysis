library(devtools)
library(roxygen2)
load_all()
sensataExAnalysis <- sensataExAnalysis

# 5 options (likert)
createGraphData(df = sensataExAnalysis, originVar = "q_EA_IN_03",groupVar = "q_EA_CA_10")

# 4 options (text)
graphData  <- createGraphData(df = sensataExAnalysis, originVar = "q_EA_IN_03", groupVar = "q_EA_CA_10")

p <- ggplot(graphData, aes(x = q_EA_CA_10, y = Porcentaje, fill = Value))
g <- geom_col()

p + g + theme_sensata() + scale_fill_manual(values = sensata_palette())

# TODO: CREATE FREQ TABLES EXAMPLE
