# TITULO QUE DIGA ALGO
# Survey id: DE CONTENTFUL
# Created by: Gabriel N. Camargo-Toledo
# Created on: Nov/24/2021
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Dec/02/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook Pop!_OS 21.04 8gb Ram R4.1.2

# Asignar objetos ----------------------------------------------------------------

"Hola bolaaaa"

a <- "Hola bolaaaa"
b <- 1
c <- b + 35

# Paquete -------------------------------

library(sensataDataAnalysis)

# Fuentes y tema ----------------------------------------------------------

#Cargar fuentes

font_add_google(name = "Open Sans", family = "Open Sans")
font_add_google(name = "News Cycle", family = "News Cycle")
font_add_google(name = "Montserrat", family = "montserrat")
showtext_auto()

# Paleta de colores OJO el orden importa
movPaleta5 <- c("#003f5c",
                "#58508d",
                "#bc5090",
                "#ff6361",
                "#ffa600")

# tema sensata

theme_sensata <-   theme(text = element_text(family = "Open Sans", size = 22),
                         axis.title = element_text(colour = "dimgray", size = 20),
                         axis.text.x = element_text(angle = 0,
                                                    vjust = 0.5,
                                                    hjust = 0.5,
                                                    size = 18,
                                                    colour = "dimgray"),
                         axis.text.y = element_text(size = 18,
                                                    colour = "dimgray"),
                         legend.text = element_text(size = 16,
                                                    colour = "dimgray"),
                         legend.title = element_text(colour = "dimgray"),
                         plot.title = element_text(family = "Montserrat",
                                                   colour = "black",
                                                   size = 26,
                                                   lineheight=1),
                         plot.caption = element_text(colour = "dimgray"),
                         plot.subtitle = element_text(family = "Montserrat",
                                                      size = 18),
                         panel.background = element_rect(size = 0, fill = "white"),
                         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                         colour = "gray85"),
                         panel.grid.major.x = element_blank(),
                         legend.key=element_rect(fill = "white", colour = "white"))


# Cargar datos ------------------------------------------------------------

sensataExAnalysis <- sensataDataAnalysis::sensataExAnalysis

# Crear graphData ---------------------------------------------------------

graphData <- createGraphData(df = sensataExAnalysis,
                             originVar = "q_EA_IN_03",
                             groupVar = "q_EA_CA_10")

# crear gráfica -----------------------------------------------------------

p <- ggplot(graphData, aes(x=q_EA_CA_10, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)


p + g + labs(title = "Pretty title",
             # subtitle = "Total",
             caption = "Sensata UX",
             fill = "Answer") +
  xlab("") + ylab("Percentage") +
  theme_sensata +
  scale_fill_manual(values = movPaleta5)
