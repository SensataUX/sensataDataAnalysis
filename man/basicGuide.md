Intro a R y SensataDataAnalysis
================
Gabriel N. Camargo-Toledo
11/02/2022

# Introducción a sensata Data Framework

Este documento presenta una guía de los paquetes de R utilizados para
preparar, limpiar y analizar los datos de encuestas recolectados con la
plataforma Sensata. Para eso tenemos una serie de paquetes que preparan
los datos y ayudan a hacer más sencillo y uniforme los análisis.

Después de que recolectamos las encuestas en nuestra plataforma, los
datos quedan cargados en un servidor (mongoDB). Estos datos quedan un
formato poco amigable para el análisis, es un archivo con muchas
columnas:

``` r
rawMetaDataExample
```

    ## # A tibble: 6 × 19
    ##   `_id`                 createdAt           surveyId surveyName params.utm_sour…
    ##   <chr>                 <dttm>              <chr>    <chr>      <chr>           
    ## 1 ObjectId(613ec9e4e9c… 2021-09-13 03:47:48 11QdjXo… [BPO] ENC… fb              
    ## 2 ObjectId(613ed04623d… 2021-09-13 04:15:02 11QdjXo… [BPO] ENC… fb              
    ## 3 ObjectId(6145000aec5… 2021-09-17 20:52:26 11QdjXo… [BPO] ENC… fb              
    ## 4 ObjectId(61450071ec5… 2021-09-17 20:54:09 11QdjXo… [BPO] ENC… fb              
    ## 5 ObjectId(61450357ec5… 2021-09-17 21:06:31 11QdjXo… [BPO] ENC… fb              
    ## 6 ObjectId(614509a93f8… 2021-09-17 21:33:29 11QdjXo… [BPO] ENC… fb              
    ## # … with 14 more variables: params.utm_medium <chr>, params.utm_campaign <chr>,
    ## #   params.test <lgl>, params.testing <lgl>, totalTimeMin <dbl>,
    ## #   fingerprint <chr>, sensataId <chr>, geolocation.coordinates <chr>,
    ## #   lat <dbl>, long <dbl>, browserReport.ip <chr>, browserReport.os.name <chr>,
    ## #   browserReport.browser.name <chr>, trialUser.userId <lgl>

``` r
rawQ0DataExample
```

    ## # A tibble: 6 × 14
    ##   newResponses.q0.identifier newResponses.q0.… newResponses.q0… newResponses.q0…
    ##   <chr>                      <chr>                        <dbl> <chr>           
    ## 1 q_AB_SC_00                 4ov5DSXKkrU7fbTz…                0 screen          
    ## 2 q_AB_SC_00                 4ov5DSXKkrU7fbTz…                0 screen          
    ## 3 q_AB_SC_00                 4ov5DSXKkrU7fbTz…                0 screen          
    ## 4 q_AB_SC_00                 4ov5DSXKkrU7fbTz…                0 screen          
    ## 5 q_AB_SC_00                 4ov5DSXKkrU7fbTz…                0 screen          
    ## 6 q_AB_SC_00                 4ov5DSXKkrU7fbTz…                0 screen          
    ## # … with 10 more variables: newResponses.q0.questionLength <dbl>,
    ## #   newResponses.q0.question <chr>, newResponses.q0.selected <chr>,
    ## #   newResponses.q0.timeToCompletion <dbl>, newResponses.q0.maxResponses <dbl>,
    ## #   newResponses.q0.isSorting <lgl>, newResponses.q0.isOrdered <lgl>,
    ## #   newResponses.q0.altOption <lgl>, newResponses.q0.options <chr>,
    ## #   newResponses.q0.numberOfOptions <dbl>

Normalmente el equipo de data utiliza el paquete *sensataDataProg* para
dejar los datos como los queremos, una fila por individuo y cada columna
es una pregunta:

``` r
sensataExAnalysis
```

    ## # A tibble: 2,831 × 86
    ##    id    geolocation.coo… q_EA_IS_00 q_EA_CA_01 q_EA_CA_04 q_EA_CA_21 q_EA_CA_05
    ##    <chr> <chr>            <fct>      <chr>      <fct>      <fct>      <fct>     
    ##  1 6101… [-73.9477268999… <NA>       Colombia   1-2 times… 2-3 times… 1-2 times…
    ##  2 6101… [-74.7922088999… <NA>       Colombia   Never      2-3 times… 1-2 times…
    ##  3 6101… [-74.1212159999… <NA>       Colombia   1-2 times… Every day  Never     
    ##  4 6101… [-76.5372512,3.… <NA>       Colombia   Never      2-3 times… Never     
    ##  5 6101… [-75.5393042,6.… <NA>       Colombia   Never      Every day  Never     
    ##  6 6101… [-74.1015551999… <NA>       Colombia   2-3 times… Every day  Every day 
    ##  7 6101… [-74.5354914,4.… <NA>       Colombia   Never      Every day  Never     
    ##  8 6101… [-76.4678653,3.… <NA>       Colombia   Never      2-3 times… Never     
    ##  9 6101… [-75.5826688,6.… <NA>       Colombia   1-2 times… Every day  Never     
    ## 10 6101… [-74.0589567999… <NA>       Colombia   1-2 times… 2-3 times… Never     
    ## # … with 2,821 more rows, and 79 more variables: q_EA_CA_22 <fct>,
    ## #   q_EA_CA_06 <fct>, q_EA_MO_02 <fct>, q_EA_CU_01 <fct>, q_EA_CU_02 <fct>,
    ## #   q_EA_JS_01 <fct>, q_EA_IN_01 <fct>, q_EA_CU_04 <fct>, q_EA_CU_05 <fct>,
    ## #   q_EA_CU_06 <ord>, q_EA_IN_02 <ord>, q_EA_IN_08 <ord>, q_EA_IN_03 <ord>,
    ## #   q_EA_IN_04 <ord>, q_EA_JS_02 <ord>, q_EA_IN_05 <ord>, q_EA_JS_03 <ord>,
    ## #   q_EA_IN_07 <fct>, q_EA_MO_07 <fct>, q_EA_MO_08 <fct>, q_EA_MO_09 <fct>,
    ## #   q_EA_IN_06 <fct>, q_EA_JS_04 <fct>, q_EA_JS_05 <fct>, q_EA_JS_06 <fct>, …

R nos permite limpiar los datos como queremos, además de crear gráficos
cómo nos gustan. Tiene la ventaja de que es freeware, gratis y
totalmente customizable para nuestras necesidades. Tenemos dos paquetes
que hacen que los procesos que debemos crear siempre sean más sencillos:
*sensataDataProg* y *sensataDataAnalysis*. El paquete *sensataDataProg*
está ya en versión 1.0.0, es decir es completo. Por el contrario,
*sensataDataAnalysis* aún necesita trabajo.

En esta guía vamos a hablar solo de *sensataDataAnalysis*, que es el
necesario para crear gráficas.

## Qué es R y Rstudio

“R es un lenguaje y ambiente de programación para computación
estadística y gráficas.” Es flexible y altamente extensible, por lo que
permite crear “paquetes” para muchas cosas. En sensata usamos el grupo
de paquetes [tidyverse](https://www.tidyverse.org). Los paquetes añaden
funcionalidad adicional a R, y el tidyverse en especial está pensando en
hacer la mayoría de las cosas de una manera más consistente y con un
lenguaje más sencillo. Pueden aprender más con este
[libro](https://r4ds.had.co.nz/).

Rstudio es una interfaz gráfica de desarrollo que hace que usar el
lenguaje R sea más fácil y amigable. Revisemos cómo funciona Rstudio
acá.

(Trucos para ver el historial, para ver el ambiente, ver las gráficas,
etc)

## Primer paso, paquete, cargar fuentes y decisiones estéticas

Al abrir Rstudio (o R) se carga solo la funcionalidad básica. Para
utilizar las funciones sensata necesitamos instalar y cargar el paquete
*sensataDataAnalysis*. Para instalar el paquete de github hay que
instalar antes otro paquete, *devtools* y suele evitar problemas si
instalamos antes el *tidyverse*:

``` r
install.packages("devtools")
install.packages("tidyverse")
devtools::install_github(repo = "https://github.com/SensataUx/sensataDataAnalysis", ref = "main")
```

Instalar el paquete no implica que esté disponible para usar su
funcionalidad. Por eso en el chunk anterior, después de instalar usé
`devtools::install_github()`, accede al paquete *devtools* y utiliza la
función *install\_github()*. Pero a veces vamos a usar muchas veces
distintas funciones de un paquete, para esto es necesario cargarlo. Esto
se hace así:

``` r
library(sensataDataAnalysis)
```

Después de esto ya podemos cargar las fuentes y decisiones estéticas que
vamos a usar:

``` r
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

# Decisiones sobre estética de las gráficas
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
                                                   lineheight=.5),
                         plot.caption = element_text(colour = "dimgray"),
                         plot.subtitle = element_text(family = "Montserrat",
                                                      size = 18),
                         panel.background = element_rect(size = 0, fill = "white"),
                         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                         colour = "gray85"),
                         panel.grid.major.x = element_blank(),
                         legend.key=element_rect(fill = "white", colour = "white"))
```

## Primer paso, cargar datos

Por ahora vamos a usar unos datos de ejemplo que vienen en el paquete
*sensataDataAnalysis*, después aprendemos cómo cargar datos que tenemos
en nuestra computadora.

``` r
sensataExAnalysis <- sensataDataAnalysis::sensataExAnalysis
```

Fijense en los nombres de los objetos que voy creando.

## Segundo paso, preparar datos para gráfica

Los microdatos no son lo que necesitamos para hacer una gráfica, es por
eso que el paquete *sensataDataAnalysis* tiene una función que
simplifica la manipulación de los datos a algo que podamos usar en
ggplot. ggplot es el sistema de gráficas de tidyverse del cuál vamos a
ir aprendiendo hoy.

``` r
graphData <- createGraphData(df = sensataExAnalysis, originVar = "q_EA_IN_03", groupVar = "q_EA_CA_10")
graphData
```

    ## # A tibble: 20 × 3
    ## # Groups:   q_EA_CA_10 [4]
    ##    q_EA_CA_10 Porcentaje Value      
    ##    <chr>           <dbl> <ord>      
    ##  1 Female          54.7  1. Disagree
    ##  2 Female          14.5  2          
    ##  3 Female          12.8  3          
    ##  4 Female           4.79 4          
    ##  5 Female          13.3  5. Agree   
    ##  6 Male            51.6  1. Disagree
    ##  7 Male            22.2  2          
    ##  8 Male             7.35 3          
    ##  9 Male             4.80 4          
    ## 10 Male            14.1  5. Agree   
    ## 11 Other           45.5  1. Disagree
    ## 12 Other            9.09 2          
    ## 13 Other           36.4  3          
    ## 14 Other            0    4          
    ## 15 Other            9.09 5. Agree   
    ## 16 Total           54.0  1. Disagree
    ## 17 Total           16.2  2          
    ## 18 Total           11.7  3          
    ## 19 Total            4.77 4          
    ## 20 Total           13.4  5. Agree

## Tercer paso, crear la gráfica

Para crear la gráfica vamos a usar ggplot

``` r
p <- ggplot(graphData, aes(x=q_EA_CA_10, y=Porcentaje)) 
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + labs(title = var_label(sensataExAnalysis$q_EA_CA_10),
             # subtitle = "Total",
             caption = "Sensata UX",
             fill = "Answer") + 
  xlab("") + ylab("") +
  theme_sensata +
  scale_fill_manual(values = movPaleta5)
```

![](basicGuide_files/figure-gfm/createGraph-1.png)<!-- -->

``` r
# ggsave(filename = "figures/q_MV_JG_26_Tot_PMC.png", device = "png", height = 7, width = 14, units = "cm")
```
