

### Introducción

La aplicación **SiteDrought App** proporciona estimaciones diarias
de **impacto de la sequía** y **riesgo de incendio** en parcelas de bosque y
matorral de Cataluña del último año. Este servicio está pensado para
proporcionar información que resulte de utilidad para estimar el peligro
de incendio diario en áreas de vegetación de interés. La aplicación actualiza
diariamente la humedad del suelo y el estrés de la vegetación a partir de
datos de meteorología diaria obtenidos por interpolación a partir de
datos de las redes de estaciones automáticas proporcionadas por la [Agencia
Estatal de Meteorología (AEMET)](http://www.aemet.es) y el [Servicio
Meteorológico de Cataluña (SMC)](http://www.meteo.cat). Para poder
realizar los cálculos, los datos de meteorología diaria se emplean como
entrada de un modelo de balance hídrico forestal parametrizado con datos
edáficos (derivadas de bases de datos globales) y de vegetación.


### Introducción

Actualmente, la aplicación **SiteDrought App** proporciona información para
cuatro conjuntos de parcelas de vegetación:

1. Parcelas del cuarto Inventario Forestal Nacional (IFN4) en Cataluña.
2. Parcelas de seguimiento de matorral (proyecto
     [MatoSeg](https://matoseg.ctfc.es), CTFC).
3. Parcelas forestales permanentes del Parque Nacional de Aigüestortes y
     Estanque de San Mauricio.
4. Parcelas del Inventario Forestal Nacional (IFN3) del Parque Nacional
     de Ordesa y Monteperdido.

Es importante recordar que los datos proporcionados incluyen cálculos
basados en modelos, por lo que los valores resultantes pueden contener
diferencias notables respecto a mediciones reales.

### Fuentes de datos

- **Topografía** - La elevación, pendiente y orientación provienen de un
    modelo digital de elevaciones a 30 m de resolución.
- **Meteorología** - Las interpolaciones de meteorología diaria se
    realizan a partir de mediciones obtenidas en estaciones
    meteorológicas de la [Agencia Estatal de Meteorología
    (AEMET)](http://www.aemet.es) y el [Servicio Meteorológico de Cataluña
    (SMC)](http://www.meteo.cat). Dado que se trata de estaciones
    automáticas y los datos se descargan al final del día en curso,
    éstas no han pasado todos los controles de calidad deseables,
    lo que puede generar artefactos en las estimaciones de balance hídrico
    de algunas zonas.
- **Suelos** - La textura, densidad aparente y materia orgánica del suelo
    provienen de la base de datos global **SoilGrids** a 250 m (Hengl et
    al. 2017), mientras que la profundidad de la roca madre proviene de
    Shangguan et al. (2017), también a 250 m de resolución.
- **Vegetación** - Se utilizan tres fuentes de datos de la composición y
    estructura del bosque:
    1. Mapa Forestal de España a 1:25000 (MFE25)
    2. Cuarto Inventario Forestal Nacional (ver aplicación **IFN App**)
        o datos de campo equivalentes, para los demás grupos de parcelas
        de vegetación.
    3. Variables biofísicas derivadas de LiDAR, en su edición de
        2016-2017 (ver aplicación **LiDAR App**)


### Modelo de balance hídrico

El modelo de balance hídrico forestal descrito en De Cáceres et al. (2015) y
De Cáceres et al. (2021). El modelo actualiza a diario el contenido
de agua en el suelo en función de la estructura y composición de la vegetación,
la meteorología y las propiedades del suelo. El balance de agua en el suelo es el
resultado de la diferencia entre los procesos que determinan la entrada
de agua (precipitación) y la salida de agua (intercepción de las
copas, transpiración, evaporación de la superficie del suelo, escorrentía
superficial y percolación en profundidad). El modelo incorpora también rutinas
que permiten calcular el comportamiento del fuego potencial dadas unas
condiciones atmosféricas y de sequía (Sánchez-Pinillos et al. 2021;
Miezite et al. 2022). El modelo está implementado en lenguaje R, en la
librería [medfate](https://emf-creaf.github.io/medfate/) y la
documentación detallada sobre el diseño y formulación de ecuaciones del
modelo se puede encontrar en
[medfatebook](https://emf-creaf.github.io/medfatebook/).



### Parametrización

Los suelos se dividen en 4 capas \[0-30 cm; 30-100 cm; 100 - SD i SD -
400 cm\] donde SD es la profundidad del horizonte R indicado en Shangguan et
al. (2017). La textura, materia orgánica y aparente densidad de las
diferentes capas del suelo se obtienen de SoilGrids (Hengl et al. 2017).
Aprovechando que en el Inventario Forestal Nacional se recogen estimaciones
de rocosidad superficial en las parcelas, ésta se utiliza para
estimar el porcentaje de rocas en las distintas capas del suelo.
El inventario forestal proporciona los diámetros y alturas de los árboles
medidos, así como sus factores de densidad asociados. La biomasa
foliar de cada especie se estima a partir de relaciones alométricas
dependientes del diámetro y nivel de competencia en la parcela
(Ameztegui, Cabon et al. 2017), véase aplicación **Allometr App**.
La estimación de la biomasa foliar en el caso de los arbustos deriva de las
alturas y porcentajes de cobertura medidos en campo y sigue también
la aplicación de relaciones alométricas (De Cáceres et al. 2019). El área
foliar específica de cada especie permite traducir la biomasa foliar en
superficie de hoja y de ahí estimar el índice de área foliar. La
distribución vertical de las raíces en el suelo se estima mediante las
ecuaciones empíricas obtenidas por Cabon et al. (2018).

### Operativa diaria

El motor de cálculo de la aplicación **SiteDrought App** se nutre de
datos meteorológicos descargados de las API del SMC y AEMET.
Estos datos se descargan y se utilizan para interpolar la
meteorología del día en curso sobre la localidad objetivo, tal y como se
describe a De Cáceres et al. (2018), y de forma similar a cómo hace
la aplicación **Meteoland App**. Una vez que los datos meteorológicos están
disponibles por una localidad, el motor ejecuta el modelo de balance
hídrico, teniendo en cuenta el estado de humedad del suelo del día anterior. El
modelo genera los flujos de agua que conforman el balance hídrico en el suelo y
actualiza el estado de humedad del mismo. A continuación, el motor de
cálculo estima el estrés por sequía de la vegetación y el contenido
de humedad del combustible vive a partir del estado de humedad del suelo y
las propiedades hidráulicas de las distintas especies. El motor también
ama la humedad del combustible muerto, siguiendo la aproximación de Resco
De Dios et al. (2015). Por último, todos estos parámetros - así como las
estimas de la carga de combustible de superficie y de copas, y las
condiciones de viento, temperatura y humedad diarias - permiten estimar dos
índices de comportamiento potencial del fuego, derivados de la aproximación de
*Fuel Characteristics Classification System* (Prichard et al. 2013).

### Variables de salida

La aplicación proporciona información de diferentes variables, agrupadas por
categorías:

**Variables climáticas** - Las variables meteorológicas más relevantes para el modelo de balance hídrico son la precipitación (ya sea en forma líquida o sólida) y la evapotranspiración potencial:

Variable | Definición | Unidades
--------- | ----------------------- | -----
Evapotranspiración potencial | Evapotranspiración potencial diaria, calculada a partir de la ecuación de Penman (1956) | $mm \cdot d^{-1}$
Precipitación | Precipitación diaria (incluye lluvia y nieve) | $mm \cdot d^{-1}$

**Indicadores de sequía** - Dos variables proporcionan información sobre la sequía edáfica y de la vegetación. Por un lado, se proporciona un indicador normalizado de la cantidad de agua disponible en el suelo. Para la misma humedad en el suelo algunas especies sufren más el estrés por sequía que otras. La conducta relativa de la planta es una medida relativa de reducción de la transpiración debida al estrés por sequía. La intensidad de estrés por sequía se define en el modelo como el complemento de la conductancia relativa de la planta.

Variable | Definición | Unidades
--------- | ----------------------- | -----
Agua extraíble relativa | Humedad disponible en el suelo, normalizada entre capacidad de campo (que correspondería a un valor de 1) y el punto de marchitamiento a -1.5 MPa (que correspondería a un valor de 0). | [0-1]
Estrés de la vegetación | Media ponderada de la intensidad del estrés diario de las diferentes especies, utilizando los valores de LAI como pesos. | [0-1]

**Indicadores de riesgo de incendio** - Otra forma de expresar el estrés de la vegetación es mediante la humedad del combustible vivo, que depende de la estructura de la hoja y el contenido relativo de agua de los tejidos de la planta.

Variable | Definición | Unidades
--------- | ----------------------- | -----
Humedad del combustible vivo | Contenido de humedad del combustible fino vivo en relación al peso seco. Media ponderada de las diferentes especies, utilizando los valores de LAI como pesos. | %
Humedad del combustible muerto | Contenido de humedad del combustible fino muerto en relación al peso seco, estimado siguiendo Resco De Dios (2015). | %
Potencial de fuego de superficie | Índice de comportamiento potencial del fuego de superficie, según una modificación del sistema *Fuel Characteristics Classification System* (FCCS; Prichard et al. 2013). | [0-9]
Potencial de fuego de copa | Índice de comportamiento potencial del fuego de copa, según una modificación del sistema *Fuel Characteristics Classification System* (FCCS; Prichard et al. 2013)). | [0-9]


**Percentiles** - Por último, la aplicación ofrece la posibilidad de expresar algunas de las variables anteriores en forma de percentiles respecto a una distribución de valores históricos, para representar hasta qué punto unos niveles dados de sequía o riesgo de incendio son extremos. El período histórico de referencia es fijo y se ha obtenido mediante simulaciones para el período 1981-2020 para cada parcela. El percentil del 50% corresponde a la mediana de la distribución histórica; percentiles > 50% indicarían que la variable tiene valores más altos que los históricos; analogamente, percentiles < 50% indicarían valores más bajos que los históricos. Actualmente se proporcionan percentiles por las siguientes variables:

Variable | Definición | Unidades
--------- | ----------------------- | -----
Porcentil de agua disponible en el suelo | Porcentil del valor de agua disponible en el suelo en relación a la distribución de valores de la serie histórica (1981-2020) | %
Porcentil de estrés de la vegetación | Porcentil del valor de estrés de la vegetación en relación con la distribución de valores de la serie histórica (1981-2020) | %
Porcentil de humedad del combustible vivo | Porcentil del valor de humedad del combustible vivo en relación a la distribución de valores de la serie histórica (1981-2020) | %

### Autoría y agradecimientos

Las siguientes personas han trabajado en el desarrollo de **SiteDrought**:

  + Motor de cálculo de la aplicación - **Miquel De Cáceres**
  + Diseño de la base de datos y aplicación shiny - **Oleguer Amorós** y **Victor Granda**
 
Los autores expresan su gratitud hacia la [Agencia Estatal de Meteorología (AEMET)](http://www.aemet.es) y el [Servicio Meteorológico de Cataluña (SMC)](http://www.meteo.cat ) para proporcionar diariamente datos meteorológicos necesarios para esta aplicación; y al Servicio de Prevención de Incendios para los consejos en su desarrollo.



### Bibliografía


+ Ameztegui, A., Cabon, A., De Cáceres, M. & Coll, L. (2017). Managing stand density to enhance the adaptability of Scots pine stands to climate change: A modelling approach. Ecol. Modell., 356, 141–150. https://doi.org/10.1016/j.ecolmodel.2017.04.006
+ Cabon A, Martínez-Vilalta J, Martínez-de-Aragón J, De Cáceres M (2018) Applying the eco-hydrological equilibrium hypothesis to model root distribution in water-limited forests. Ecohydrology  11: e2015.  https://doi.org/10.1002/eco.2015
+ De Cáceres, M., Martinez-Vilalta, J., Coll, L., Llorens, P., Casals, P., Poyatos, R., et al. (2015). Coupling a water balance model with forest inventory data to predict drought stress: the role of forest structural changes vs. climate changes. Agricultural and Forest Meteorology, 213, 77–90. https://doi.org/10.1016/j.agrformet.2015.06.012
+ De Cáceres, M., Martin-StPaul, N., Turco, M., Cabon, A., Granda, V., 2018. Estimating daily meteorological data and downscaling climate models over landscapes. Environ. Model. Softw. 108, 186–196. https://doi.org/10.1016/j.envsoft.2018.08.003
+ De Cáceres, M., Casals, P., Gabriel, E., Castro, X., 2019. Scaling-up individual-level allometric equations to predict stand-level fuel loading in Mediterranean shrublands. Ann. For. Sci. 76, 87. https://doi.org/10.1007/s13595-019-0873-4
+ De Cáceres M, Mencuccini M, Martin-StPaul N, Limousin JM, Coll L, Poyatos R, Cabon A, Granda
V, Forner A, Valladares F, Martínez-Vilalta J (2021) Unravelling the effect of species mixing on water
use and drought stress in Mediterranean forests: a modelling approach. Agricultural and Forest
Meteorology 296: 108233. https://doi.org/10.1016/j.agrformet.2020.108233 
+ Hengl, T., Mendes De Jesus, J., Heuvelink, G.B.M., Gonzalez, M.R., Kilibarda, M., Blagotí, A., Shangguan, W., Wright, M.N., Geng, X., Bauer-Marschallinger, B., Guevara, M.A., Vargas, R., Macmillan, R.A., Batjes, N.H., Leenaars, J.G.B., Ribeiro, E., Wheeler, I., Mantel, S., Kempen, B., 2017. SoilGrids250m: Global Gridded Soil Information Based on Machine Learning. PLoS One 12, e0169748. https://doi.org/10.1371/journal.pone.0169748
+ Miezite LE, Ameztegui A, De Cáceres M, Coll L, Morán-Ordóñez A, Vega-García C, Rodrigues M
(2022). Trajectories of wildfire behavior under climate change. Can forest management mitigate the
increasing hazard? Journal of Environmental Management 322: 116134. https://doi.org/10.1016/j.jenvman.2022.116134
+ Penman, H. L. 1956. Evaporation: An introductory survey. Netherlands Journal of Agricultural Science, 4, 9-29.
+ Prichard, S. J., D. V Sandberg, R. D. Ottmar, E. Eberhardt, A. Andreu, P. Eagle, and K. Swedin. (2013). Classification System Version 3.0: Technical Documentation.
+ Resco de Dios, V., A. W. Fellows, R. H. Nolan, M. M. Boer, R. A. Bradstock, F. Domingo, and M. L. Goulden. (2015). A semi-mechanistic model for predicting the moisture content of fine litter. Agricultural and Forest Meteorology 203:64–73. https://doi.org/10.1016/j.agrformet.2015.01.002
+ Sánchez-Pinillos M, De Cáceres M, Casals P, Alvarez A, Beltrán M, Pausas JG, Vayreda J, Coll L.
(2021). Spatial and temporal variations of overstory and understory fuels in Mediterranean landscapes.
Forest Ecology and Management 490: 119094. https://doi.org/10.1016/j.foreco.2021.119094
+ Shangguan, W., Hengl, T., Mendes de Jesus, J., Yuan, H., Dai, Y., 2017. Mapping the global depth to bedrock for land surface modeling. J. Adv. Model. Earth Syst. 9, 65–88. https://doi.org/10.1002/2016MS000686