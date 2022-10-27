

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





```
  ##################################################################3
                    
                   AKIIIII MHE QUEDAT
                      
  ##################################################################3
 
```






### Operativa diària

El motor de càlcul de l’aplicació **SiteDrought App** es nodreix de
dades meteorològiques descarregades de les API del SMC i l’AEMET.
Aquestes dades es descarregen i s’utilitzen per a interpolar la
meteorologia del dia en curs sobre la localitat objectiu, tal i com es
descriu a De Cáceres et al. (2018), i de manera semblant a com fa
l’aplicació **Meteoland App**. Un cop les dades meteorològiques estan
disponibles per una localitat, el motor executa el model de balanç
hídric, tenint en compte l’estat d’humitat del sòl del dia anterior. El
model genera els fluxos d’aigua que conformen el balanç hídric al sòl i
actualitza l’estat d’humitat del mateix. A continuació, el motor de
càlcul estima l’estrés per sequera de la vegetació i el contingut
d’humitat del combustible viu a partir de l’estat d’humitat del sòl i
les propietats hidràuliques de les diferents espècies. El motor també
estima la humitat del combustible mort, seguint l’aproximació de Resco
De Dios et al. (2015). Finalment, tots aquests paràmetres - així com les
estimes de la càrrega de combustible de superfície i de capçades, i les
condicions de vent, temperatura i humitat diàries - permeten estimar dos
índexos de comportament potencial del foc, derivats de l’aproximació de
*Fuel Characteristics Classification System* (Prichard et al. 2013).

### Variables de sortida

L’aplicació proporciona informació de diferents variables, agrupades per
categories:

**Variables climàtiques** - Les variables meteorològiques més rellevants
per al model de balanç hídric són la precipitació (ja sigui en forma
líquida o sòlida) i l’evapotranspiració potencial:

<table>
<colgroup>
<col style="width: 24%" />
<col style="width: 62%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr class="header">
<th>Variable</th>
<th>Definició</th>
<th>Unitats</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Evapotranspiració potencial</td>
<td>Evapotranspiració potencial diària, calculada a partir de l’equació
de Penman (1956)</td>
<td><span
class="math inline"><em>m</em><em>m</em> ⋅ <em>d</em><sup>−1</sup></span></td>
</tr>
<tr class="even">
<td>Precipitació</td>
<td>Precipitació diària (inclou pluja i neu)</td>
<td><span
class="math inline"><em>m</em><em>m</em> ⋅ <em>d</em><sup>−1</sup></span></td>
</tr>
</tbody>
</table>

**Indicadors de sequera** - Dues variables proporcionen informació sobre
la sequera edàfica i de la vegetació. D’una banda, es proporciona un
indicador normalitzat de la quantitat d’aigua disponible al sòl. Per a
la mateixa humitat al sòl algunes espècies pateixen més l’estrés per
sequera que altres. La conductància relativa de la planta és una mesura
relativa de reducció de la transpiració deguda a l’estrés per sequera.
La intensitat d’estrés per sequera es defineix al model com el
complement de la conductància relativa de la planta.

<table>
<colgroup>
<col style="width: 24%" />
<col style="width: 62%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr class="header">
<th>Variable</th>
<th>Definició</th>
<th>Unitats</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Aigua extraïble relativa</td>
<td>Humitat disponible al sòl, normalitzada entre capacitat de camp (que
correspondria a un valor d’1) i el punt de marciment a -1.5 MPa (que
correspondria a un valor de 0).</td>
<td>[0-1]</td>
</tr>
<tr class="even">
<td>Estrés de la vegetació</td>
<td>Mitjana ponderada de la intensitat de l’estrés diari de les
diferents espècies, fent servir els valors de LAI com a pesos.</td>
<td>[0-1]</td>
</tr>
</tbody>
</table>

**Indicadors de risc d’incendi** - Una altra manera d’expressar l’estrés
de la vegetació és mitjançant la humitat del combustible viu, que depèn
de l’estructura de la fulla i el contingut relatiu d’aigua dels teixits
de la planta.

<table>
<colgroup>
<col style="width: 24%" />
<col style="width: 62%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr class="header">
<th>Variable</th>
<th>Definició</th>
<th>Unitats</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Humitat del combustible viu</td>
<td>Contingut d’humitat del combustible fi viu en relació al pes
sec. Mitjana ponderada de les diferents espècies, fent servir els valors
de LAI com a pesos.</td>
<td>%</td>
</tr>
<tr class="even">
<td>Humitat del combustible mort</td>
<td>Contingut d’humitat del combustible fi mort en relació al pes sec,
estimat seguint Resco De Dios (2015).</td>
<td>%</td>
</tr>
<tr class="odd">
<td>Potencial de foc de superfície</td>
<td>Index de comportament potencial del foc de superfície, segons una
modificació del sistema <em>Fuel Characteristics Classification
System</em> (FCCS; Prichard et al. 2013).</td>
<td>[0-9]</td>
</tr>
<tr class="even">
<td>Potencial de foc de capçada</td>
<td>Index de comportament potencial del foc de capçada, segons una
modificació del sistema <em>Fuel Characteristics Classification
System</em> (FCCS; Prichard et al. 2013)).</td>
<td>[0-9]</td>
</tr>
</tbody>
</table>

**Percentils** - Finalment, l’aplicació ofereix la possibilitat
d’expressar algunes de les variables anteriors en forma de percentils
respecte a una distribució de valors històrics, per tal de representar
fins a quin punt uns nivells donats de sequera o risc d’incendi són
extrems. El periode històric de referència és fixe i s’ha obtingut
mitjançant simulacions per al periode 1981-2020 per a cada parcel·la. El
percentil del 50% correspon a la mediana de la distribució històrica;
percentils &gt; 50 % indicarien que la variable té valors més alts que
els històrics; analogament, percentils &lt; 50% indicarien valors més
baixos que els històrics. Actualment es proporcionen percentils per les
següents variables:

<table>
<colgroup>
<col style="width: 24%" />
<col style="width: 62%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr class="header">
<th>Variable</th>
<th>Definició</th>
<th>Unitats</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Percentil d’aigua disponible al sòl</td>
<td>Percentil del valor d’aigua disponible al sòl en relació a la
distribució de valors de la sèrie històrica (1981-2020)</td>
<td>%</td>
</tr>
<tr class="even">
<td>Percentil d’estrés de la vegetació</td>
<td>Percentil del valor d’estrés de la vegetació en relació a la
distribució de valors de la sèrie històrica (1981-2020)</td>
<td>%</td>
</tr>
<tr class="odd">
<td>Percentil d’humitat del combustible viu</td>
<td>Percentil del valor d’humitat del combustible viu en relació a la
distribució de valors de la sèrie històrica (1981-2020)</td>
<td>%</td>
</tr>
</tbody>
</table>

### Autoria i agraïments

Les següents persones han treballat en el desenvolupament de
**SiteDrought**:

-   Motor de càlcul de l’aplicació - **Miquel De Cáceres**
-   Disseny de la base de dades i aplicació shiny - **Oleguer Amorós** i
    **Victor Granda**

Els autors expressen la seva gratitut vers l’[Agencia Estatal de
Meteorologia (AEMET)](http://www.aemet.es) i el [Servei Meteorologic de
Catalunya (SMC)](http://www.meteo.cat) per proporcionar diàriament dades
meteorològiques necessaries per aquesta aplicació; i al Servei de
Prevenció d’Incendis per als consells en el seu desenvolupament.

### Bibliografia

-   Ameztegui, A., Cabon, A., De Cáceres, M. & Coll, L. (2017). Managing
    stand density to enhance the adaptability of Scots pine stands to
    climate change: A modelling approach. Ecol. Modell., 356, 141–150.
    <https://doi.org/10.1016/j.ecolmodel.2017.04.006>
-   Cabon A, Martínez-Vilalta J, Martínez-de-Aragón J, De Cáceres
    M (2018) Applying the eco-hydrological equilibrium hypothesis to
    model root distribution in water-limited forests. Ecohydrology 11:
    e2015. <https://doi.org/10.1002/eco.2015>
-   De Cáceres, M., Martinez-Vilalta, J., Coll, L., Llorens, P., Casals,
    P., Poyatos, R., et al. (2015). Coupling a water balance model with
    forest inventory data to predict drought stress: the role of forest
    structural changes vs. climate changes. Agricultural and Forest
    Meteorology, 213, 77–90.
    <https://doi.org/10.1016/j.agrformet.2015.06.012>
-   De Cáceres, M., Martin-StPaul, N., Turco, M., Cabon, A., Granda,
    V., 2018. Estimating daily meteorological data and downscaling
    climate models over landscapes. Environ. Model. Softw. 108, 186–196.
    <https://doi.org/10.1016/j.envsoft.2018.08.003>
-   De Cáceres, M., Casals, P., Gabriel, E., Castro, X., 2019.
    Scaling-up individual-level allometric equations to predict
    stand-level fuel loading in Mediterranean shrublands. Ann. For. Sci.
    76, 87. <https://doi.org/10.1007/s13595-019-0873-4>
-   De Cáceres M, Mencuccini M, Martin-StPaul N, Limousin JM, Coll L,
    Poyatos R, Cabon A, Granda V, Forner A, Valladares F,
    Martínez-Vilalta J (2021) Unravelling the effect of species mixing
    on water use and drought stress in Mediterranean forests: a
    modelling approach. Agricultural and Forest Meteorology 296: 108233.
    <https://doi.org/10.1016/j.agrformet.2020.108233>
-   Hengl, T., Mendes De Jesus, J., Heuvelink, G.B.M., Gonzalez, M.R.,
    Kilibarda, M., Blagotí, A., Shangguan, W., Wright, M.N., Geng, X.,
    Bauer-Marschallinger, B., Guevara, M.A., Vargas, R., Macmillan,
    R.A., Batjes, N.H., Leenaars, J.G.B., Ribeiro, E., Wheeler, I.,
    Mantel, S., Kempen, B., 2017. SoilGrids250m: Global Gridded Soil
    Information Based on Machine Learning. PLoS One 12, e0169748.
    <https://doi.org/10.1371/journal.pone.0169748>
-   Miezite LE, Ameztegui A, De Cáceres M, Coll L, Morán-Ordóñez A,
    Vega-García C, Rodrigues M (2022). Trajectories of wildfire behavior
    under climate change. Can forest management mitigate the increasing
    hazard? Journal of Environmental Management 322: 116134.
    <https://doi.org/10.1016/j.jenvman.2022.116134>
-   Penman, H. L. 1956. Evaporation: An introductory survey. Netherlands
    Journal of Agricultural Science, 4, 9-29.
-   Prichard, S. J., D. V Sandberg, R. D. Ottmar, E. Eberhardt, A.
    Andreu, P. Eagle, and K. Swedin. (2013). Classification System
    Version 3.0: Technical Documentation.
-   Resco de Dios, V., A. W. Fellows, R. H. Nolan, M. M. Boer, R. A.
    Bradstock, F. Domingo, and M. L. Goulden. (2015). A semi-mechanistic
    model for predicting the moisture content of fine litter.
    Agricultural and Forest Meteorology 203:64–73.
    <https://doi.org/10.1016/j.agrformet.2015.01.002>
-   Sánchez-Pinillos M, De Cáceres M, Casals P, Alvarez A, Beltrán M,
    Pausas JG, Vayreda J, Coll L. (2021). Spatial and temporal
    variations of overstory and understory fuels in Mediterranean
    landscapes. Forest Ecology and Management 490: 119094.
    <https://doi.org/10.1016/j.foreco.2021.119094>
-   Shangguan, W., Hengl, T., Mendes de Jesus, J., Yuan, H., Dai,
    Y., 2017. Mapping the global depth to bedrock for land surface
    modeling. J. Adv. Model. Earth Syst. 9, 65–88.
    <https://doi.org/10.1002/2016MS000686>
