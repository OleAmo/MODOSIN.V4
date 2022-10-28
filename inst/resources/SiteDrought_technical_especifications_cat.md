

### Introducció

L’aplicació **SiteDrought App** proporciona estimacions diàries
d’**impacte de la sequera** i **risc d’incendi** en parcel·les bosc i
matollar de Catalunya del darrer any. Aquest servei està pensat per
proporcionar informació que resulti d’utilitat per a estimar el perill
d’incendi diari en arees de vegetació d’interès. L’aplicació actualitza
diàriament la humitat del sòl i l’estrés de la vegetació a partir de
dades de meteorologia diària obtingudes per interpolació a partir de
dades de les xarxes d’estacions automàtiques proporcionades l’[Agencia
Estatal de Meteorologia (AEMET)](http://www.aemet.es) i el [Servei
Meteorologic de Catalunya (SMC)](http://www.meteo.cat). Per a poder
realitzar els càlculs, les dades de meteorologia diària s’empren com a
entrada d’un model de balanç hídric forestal parametritzat amb dades
edàfiques (derivades de bases de dades globals) i de vegetació.

Actualment, l’aplicació **SiteDrought App** proporciona informació per a
quatre conjunts de parcel·les de vegetació:

1.  Parcel·les del quart Inventari Forestal Nacional (IFN4) a Catalunya.
2.  Parcel·les de seguiment de matollar (projecte
    [MatoSeg](https://matoseg.ctfc.es), CTFC).
3.  Parcel·les forestals permanents del Parc Nacional d’Aigüestortes i
    Estany de Sant Maurici.
4.  Parcel·les de l’Inventari Forestal Nacional (IFN3) del Parc Nacional
    d’Ordesa y Monteperdido.

És important recordar que les dades proporcionades inclouen càlculs
basats en models, pel que els valors resultants poden contenir
diferències notables respecte a mesuraments reals.

### Fonts de dades

-   **Topografia** - L’elevació, pendent i orientació provenen d’un
    model digital d’elevacions a 30 m de resolució.
-   **Meteorologia** - Les interpolacions de meteorologia diària es
    realitzen a partir de mesuraments obtinguts en estacions
    meteorològiques de l’[Agencia Estatal de Meteorologia
    (AEMET)](http://www.aemet.es) i el [Servei Meteorologic de Catalunya
    (SMC)](http://www.meteo.cat). Donat que es tracta d’estacions
    automàtiques i les dades es descarreguen al final del dia en curs,
    aquestes no han passat tots els controls de qualitat desitjables,
    cosa que pot generar artefactes en les estimacions de balanç hídric
    d’algunes zones.
-   **Sòls** - La textura, densitat aparent i matèria orgànica del sòl
    provenen de la base de dades global **SoilGrids** a 250 m (Hengl et
    al. 2017), mentre que la profunditat de la roca mare prové de
    Shangguan et al. (2017), també a 250 m de resolució.
-   **Vegetació** - S’utilitzen tres fonts de dades de la composició i
    estructura del bosc:
    1.  Mapa Forestal de España a 1:25000 (MFE25)
    2.  Quart Inventari Forestal Nacional (vegeu aplicació **IFN App**)
        o dades de camp equivalents, per als altres grups de parcel·les
        de vegetació.
    3.  Variables biofísiques derivades de LiDAR, en la seva edició de
        2016-2017 (vegeu aplicació **LiDAR App**)

### Model de balanç hídric

El model de balanç hídric forestal descrit a De Cáceres et al. (2015) i
De Cáceres et al. (2021). El model actualitza diàriament el contingut
d’aigua al sòl en funció de l’estructura i composició de la vegetació,
la meteorologia i les propietats del sòl. El balanç d’aigua al sòl és el
resultat de la diferencia entre els processos que determinen l’entrada
d’aigua (precipitació) i la sortida d’aigua (intercepció de les
capçades, transpiració, evaporació de la superfície del sòl, escolament
superficial i percolació en fondària). El model incorpora també rutines
que permeten càlcular el comportament del foc potencial donades unes
condicions atmosfèriques i de sequera (Sánchez-Pinillos et al. 2021;
Miezite et al. 2022). El model està implementat en llenguatge R, a la
llibreria [medfate](https://emf-creaf.github.io/medfate/) i la
documentació detallada sobre el disseny i formulació d’equacions del
model es pot trobar al
[medfatebook](https://emf-creaf.github.io/medfatebook/).

### Parametrització

Els sòls es divideixen en 4 capes \[0-30 cm; 30-100 cm; 100 - SD i SD -
400 cm\] on SD és la fondària de l’horitzó R indicat a Shangguan et
al. (2017). La textura, matèria orgànica i densitat aparent de les
diferents capes del sòl s’obtenen de SoilGrids (Hengl et al. 2017).
Aprofitant que a l’Inventari Forestal Nacional es recullen estimacions
de rocositat superficial a les parcel·les, aquesta s’utilitza per
estimar el percentatge de roques a les diferents capes del sòl.
L’inventari forestal proporciona els diàmetres i alçades dels arbres
mesurats, així com els seus factors de densitat associats. La biomassa
foliar de cada espècie s’estima a partir de relacions al·lomètriques
depenents del diàmetre i nivell de competència a la parcel·la
(Ameztegui, Cabon et al. 2017), vegeu aplicació **Allometr App**.
L’estimació de la biomassa foliar en el cas dels arbusts deriva de les
alçades i percentatges de cobertura mesurats a camp i segueix també
l’aplicació de relacions al·lomètriques (De Cáceres et al. 2019). L’àrea
foliar específica de cada espècie permet traduir la biomassa foliar en
superfície de fulla i d’aquí estimar l’índex d’àrea foliar. La
distribució vertical de les arrels al sòl s’estima mitjançant les
equacions empíriques obtingudes per Cabon et al. (2018).

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

**Variables climàtiques** - Les variables meteorològiques més rellevants per al model de balanç hídric són la precipitació (ja sigui en forma líquida o sòlida) i l'evapotranspiració potencial:

Variable 	| Definició               | Unitats 
--------- | ----------------------- | ----- 
Evapotranspiració potencial	| Evapotranspiració potencial diària, calculada a partir de l'equació de Penman (1956)	| $mm \cdot d^{-1}$
Precipitació	| Precipitació diària (inclou pluja i neu)	| $mm \cdot d^{-1}$

**Indicadors de sequera** - Dues variables proporcionen informació sobre la sequera edàfica i de la vegetació. D'una banda, es proporciona un indicador normalitzat de la quantitat d'aigua disponible al sòl. Per a la mateixa humitat al sòl algunes espècies pateixen més l'estrés per sequera que altres. La conductància relativa de la planta és una mesura relativa de reducció de la transpiració deguda a l'estrés per sequera. La intensitat d'estrés per sequera es defineix al model com el complement de la conductància relativa de la planta. 

Variable 	| Definició               | Unitats 
--------- | ----------------------- | ----- 
Aigua extraïble relativa | Humitat disponible al sòl, normalitzada entre capacitat de camp (que correspondria a un valor d'1) i el punt de marciment a -1.5 MPa (que correspondria a un valor de 0). | [0-1] 
Estrés de la vegetació | Mitjana ponderada de la intensitat de l'estrés diari de les diferents espècies, fent servir els valors de LAI com a pesos. | [0-1] 

**Indicadors de risc d'incendi** -  Una altra manera d'expressar l'estrés de la vegetació és mitjançant la humitat del combustible viu, que depèn de l'estructura de la fulla i el contingut relatiu d'aigua dels teixits de la planta.

Variable 	| Definició               | Unitats 
--------- | ----------------------- | ----- 
Humitat del combustible viu | Contingut d'humitat del combustible fi viu en relació al pes sec. Mitjana ponderada de les diferents espècies, fent servir els valors de LAI com a pesos. | % 
Humitat del combustible mort | Contingut d'humitat del combustible fi mort en relació al pes sec, estimat seguint Resco De Dios (2015). | % 
Potencial de foc de superfície | Index de comportament potencial del foc de superfície, segons una modificació del sistema *Fuel Characteristics Classification System* (FCCS; Prichard et al. 2013). | [0-9]
Potencial de foc de capçada | Index de comportament potencial del foc de capçada, segons una modificació del sistema *Fuel Characteristics Classification System* (FCCS; Prichard et al. 2013)). | [0-9] 

**Percentils** - Finalment, l'aplicació ofereix la possibilitat d'expressar algunes de les variables anteriors en forma de percentils respecte a una distribució de valors històrics, per tal de representar fins a quin punt uns nivells donats de sequera o risc d'incendi són extrems. El periode històric de referència és fixe i s'ha obtingut mitjançant simulacions per al periode 1981-2020 per a cada parcel·la. El percentil del 50% correspon a la mediana de la distribució històrica; percentils > 50 % indicarien que la variable té valors més alts que els històrics; analogament, percentils < 50% indicarien valors més baixos que els històrics. Actualment es proporcionen percentils per les següents variables:

Variable 	| Definició               | Unitats 
--------- | ----------------------- | ----- 
Percentil d'aigua disponible al sòl | Percentil del valor d'aigua disponible al sòl en relació a la distribució de valors de la sèrie històrica (1981-2020) | %
Percentil d'estrés de la vegetació |  Percentil del valor d'estrés de la vegetació en relació a la distribució de valors de la sèrie històrica (1981-2020) | %
Percentil d'humitat del combustible viu | Percentil del valor d'humitat del combustible viu en relació a la distribució de valors de la sèrie històrica (1981-2020)  | %

### Autoria i agraïments

Les següents persones han treballat en el desenvolupament de **SiteDrought**:

 + Motor de càlcul de l'aplicació - **Miquel De Cáceres**
 + Disseny de la base de dades i aplicació shiny - **Oleguer Amorós** i **Victor Granda**
 
Els autors expressen la seva gratitut vers l'[Agencia Estatal de Meteorologia (AEMET)](http://www.aemet.es) i el [Servei Meteorologic de Catalunya (SMC)](http://www.meteo.cat) per proporcionar diàriament dades meteorològiques necessaries per aquesta aplicació; i al  Servei de Prevenció d'Incendis per als consells en el seu desenvolupament.



### Bibliografia

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