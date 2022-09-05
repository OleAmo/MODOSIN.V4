## Script for creating the translations

tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,

  # var_daily sel
  
  "Precipitation", "Precipitació (mm/dia)", "Precipitation (mm/day)", "Precipitación (mm/día)",
  "PET", "Evapo-transpiració potencial (mm/dia)", "Potential evapo-transpiration (mm/day)", "Evap-transpiración potencial (mm/día)",
  "REW", "Aigua extraïble relativa (%)", "Relative extractable water (%)", "Agua extraible relativa (%)",
  "REW_q", "Quantil Aigua extraïble relativa (%)", "Quantile Relative extractable water (%)", "Cuantil Agua extraible relativa (%)",
  "DDS", "Intensitat de l'estrès (%)", "Stress intensity (%)", "Intensidad del estrés (%)",
  "DDS_q", "Quantil Intensitat de l'estrès (%)", "Quantile Stress intensity (%)", "Cuantil Intensidad del estrés (%)",
  "fire variables","Variables Risc Incendi", "Fire Risk Variables", "Variables Riesgo Incendio",
  "LFMC", "Contingut d’Humitat de Combustible Viu", "Live Fuel Moisture Contents","Contendio de Humedad de Combustible Vivo",
  "LFMC_q", "Quantil Contingut d’Humitat de Combustible Viu", "Quantile Live Fuel Moisture Contents","Cuantil Contendio de Humedad de Combustible Vivo",
  "DFMC", "Contingut d’Humitat de Combustible Mort", "Dead Fuel Moisture Contents","Contendio de Humedad de Combustible Muerto",
  "SFP","SFP","SFP","SFP",
  "CFP","CFP","CFP","CFP",
  
  
  
  # "NetPrec", "Precipitació neta (mm)", "Net precipitation (mm)", "Precipitación neta (mm)",
  "Eplant", "Transpiració (mm/dia)", "Plant transpiration (mm/day)", "Transpiración plantas (mm/día)",
  "Esoil", "Evaporació del sòl (mm/dia)", "Soil evaporation (mm/day)", "Evaporación del suelo (mm/día)",
  "Runoff", "Escorrentia (mm/dia)", "Run-off (mm/day)", "Escorrentía (mm/día)",
  "DeepDrainage", "Drenatje a profunditat (mm/dia)", "Deep drainage (mm/day)", "Drenaje a profundidad (mm/día)",
  
  "Theta", "Contingut d'humitat (m3/m3)", "Soil moisture content (m3/m3)", "Contenido de humedad del suelo (m3/m3)",
  "Psi", "Potencial hídric del sòl (-MPa)", "Soil water potential (-MPa)", "Potencial hídrico del suelo (-MPa)",
 
  # "NDD", "Duració de l'estrès (dies)", "Stress duration (days)", "Duración del estrés (días)",
  "LMFC", "Contingut d'humitat de el combustible viu (%)", "Live Fuel Moisture Content (%)", "Contenido de humedad del combustible vivo (%)",
  "LAI", "Índex d'àrea foliar (m2/m2)", "Leaf area index (m2/m2)", "Índice de área foliar (m2/m2)",
  "Infiltration", "Infiltració (mm/dia)", "Infiltration (mm/day)", "Infiltración (mm/día)",
  "Interception", "Intercepció (mm/dia)", "Interception (mm/day)", "Intercepción (mm/día)",
  'Soil moisture', "Humitat del sòl", 'Soil moisture', "Humedad del suelo",
  'Climate', "Clima", 'Climate', "Clima",
  'Evaporative surface', "Superficie evaporativa", 'Evaporative surface', "Superficie evaporativa",
  'Water balance', "Balanç hídric", 'Water balance', "Balance hídrico",
  'Drought stress', "Estrès hídric", 'Drought stress', "Estrés hídrico",

  # date_daily sel
  

  # .......... MODOSIN ...........
  # ..............................
  
  #       .) Creo los TABS de la 1ra y 2nd PESTAÑA
  #       .) Seran de IGUAL valor (cat,eng,spa)
  #       .) Y diferente ID
  
  #       .) Diferentes LABELS de la APP
  #       .) Son los ID de la función TRANSLATE_APP
  
  "var_daily_label", 'Tria la variable MODOSIN', 'Choose variable MODOSIN', 'Elige la variable MODOSIN',
  "main_tab_translation", "Explora Parceles", "Plots Explore", "Explora Parcelas",
  "data_translation", "Dades", "Data", "Datos",
  "map_translation", "Mapa", "Map", "Mapa",
  "save_translation", "Guardar", "Save", "Guardar",
  "tech_specs_translation", "Especificacions tècniques", "Technical specifications", "Especificaciones técnicas",
  "date_daily_label", 'Data MODOSIN', 'Date MODOSIN', 'Fecha MODOSIN',
  'series_tab_translation', "Sèries temporals", "Time series", "Series temporales",
  "size_label", 'Parcel·les (Tamany)', 'Plots (Size)', 'Parcelas (Tamaño)',
  "type_legend_label", "Tipus de Legenda", 'Legend Type','Tipo de Leyenda',
  "layers_label",'Selecció Capes','Layers Selection','Selección Capas',
  'Provincia_select_entorno', "Provincies", "Provinces", "Provincias",
  'Comararca_select_entorno', "Comarques", "Counties", "Comarcas",
  'Nucleos_select_entorno','Nuclis Població','Population centers','Nucleos Pobalción',
  'Embass_select_entorno','Embassaments','Reservoirs','Embalses',
  'Constante_label','Constant','Constant','Constante',
  'funcio_label','f(x) de la Variable','Variable Function','f(x) de la Variable',
  'Continua_label','Contínua','Continues','Continua',
  'Quantil_label','Quantils','Quantiles','Cuantiles',
  "sub_layer_label","----- Capes Principals ------","----- Main Layers -----------","----- Capas Principales -----",
  "sub_layer_label_2","----- Capa Epecial ----------",  "----- Special layer ---------","----- Capa Epecial ----------",
  
  
  "main_tab_polygon", "Explora Polígons", "Polygon Explore", "Explora Polígonos",
  "var_daily_label_polygon", 'Tria la variable MODOSIN', 'Choose variable MODOSIN', 'Elige la variable MODOSIN',
  "main_tab_translation_polygon", "Explora Parceles", "Plots Explore", "Explora Parcelas",
  "data_translation_polygon", "Dades", "Data", "Datos",
  "map_translation_polygon", "Mapa", "Map", "Mapa",
  "save_translation_polygon", "Guardar", "Save", "Guardar",
  "tech_specs_translation_polygon", "Especificacions tècniques", "Technical specifications", "Especificaciones técnicas",
  "date_daily_label_polygon", 'Data MODOSIN', 'Date MODOSIN', 'Fecha MODOSIN',
  'series_tab_translation_polygon', "Sèries temporals", "Time series", "Series temporales",
  "main_tab_polygon_polygon", "Explora Polígons", "Polygon Explore", "Explora Polígonos",
  "size_label_polygon", 'Parcel·les (Tamany)', 'Plots (Size)', 'Parcelas (Tamaño)',
  "type_legend_label_polygon", "Tipus de Legenda", 'Legend Type','Tipo de Leyenda',
  "entorno_label_polygon",'Entorn selecció','Selection environment','Entorno selección',
  'Constante_label_polygon','Constant','Constant','Constante',
  'funcio_label_polygon','f(x) de la Variable','Variable Function','f(x) de la Variable',
  'Continua_label_polygon','Contínua','Continues','Continua',
  'Quantil_label_polygon','Quantils','Quantiles','Cuantiles',
  "progress_plots", "Obtenció de les parcel·les", "Retrieving the Plots", "Obteniendo las parcelas",
  "progress_detail_plots", "Això pot trigar una mica", "This may take some time", "Esto puede llevar algo de tiempo",
  
  'entorno_hidden','Nuclis Població / Àrea (Ha)','Population centers / Area (Ha)','Nucleos Pobalción / Area (Ha)',
  
  "plot_origin_label", 'Origen Parcel·la', 'Plot Origin', 'Origen Parcela',
  "P", "IFN 4", "NFI 4" ,"IFN 4",
  "A", "Parc Nacional d'Aiguestortes","Aiguestortes National Park","Parque Nacional de Aiguetortes",
  "S", "Matollar","Scrubland","Matorral",
  "T", "Totes les Parcel·les","All Plots","Todas las Parcelas",
  "display_label", "Mostra divisions", "Show divisions", "Mostrar divisiones",
  'PRO', "Provincies", "Provinces", "Provincias",
  'AT' ,"Parque Nacional de Aigüestortes i Estany de Sant Maurici","Parque Nacional de Aigüestortes i Estany de Sant Maurici","Parque Nacional de Aigüestortes i Estany de Sant Maurici",
  'O',"Parque Nacional Ordesa y Monte Perdido","Parque Nacional Ordesa y Monte Perdido","Parque Nacional Ordesa y Monte Perdido",
  "ALL", "Totes les divisions","All divisions","Todas las divisiones",
  "NO", "Cap divisió","No division","Ninguna división",
  "no_p", "Cap Parcel·la","No Plot","Ninguna Parcela",
 
  
  
  


  # display_daily sel
  
  'none', "Cap", "None", "Ninguno",
  "Watersheds", "Conques", "Watersheds", "Cuencas hidrológicas",
  "Counties", "Comarques", "Counties", "Comarcas",
  "Municipalities", "Municipis", "Municipalities", "Municipios",
  "IFN plots", "Parcel·les IFN", "NFI plots", "Parcelas IFN",
  "file", "Arxiu espacial", "Spatial file", "Archivo espacial",

  # resolution_daily sel
  "resolution_daily_label", "Resolució del ràster", "Raster res", "Resolución del ráster",
  'Smoothed', "Suavitzat (1km)", "Smoothed (1km)", "Suavizado (1km)",
  '1km', "1km", "1km", "1km",
  '200m', "200m", "200m", "200m",

  # map translations
  'Relief', 'Relleu', 'Relief', 'Relieve',
  'Imagery', 'Satèl·lit', 'Imagery', 'Satélite',

  # download raster button
  "download_raster_label", "Descàrrega el ràster", "Download raster", "Descarga el ráster",
  "download_raster_format", "Format", "Format", "Formato",
  "gtiff", "GeoTiff", "GeoTiff", "GeoTiff",
  "gpkg", "GeoPackage", "GeoPackage", "GeoPackage",

  # download trend button
  "download_series_label", "Descàrrega la sèrie", "Download series", "Descarga la serie",

  # tabs translations
 

  # modal dialog waiting
  'modal_waiting_p', "Extraient tots els valors per a l'àrea seleccionada. Això pot trigar algun temps depenent del tipus i mida de l'àrea seleccionada (~ 5-60s)", "Extracting all values for the selected area. This can take a while depending on the type and size of the selected area (5 ~ 60 secs)", "Extrayendo todos los valores para el área seleccionada. Esto puede tardar algún tiempo dependiendo del tipo y tamaño del área seleccionada (~ 5-60s)",
  'dismiss_btn', "Tancar", "Dismiss", "Cerrar",

  # daily trends title
  'daily_trends_ifn_title', "{clicked_marker$id} en [{round(clicked_marker$lng, 3)} lng, {round(clicked_marker$lat, 3)} lat]", "{clicked_marker$id} at [{round(clicked_marker$lng, 3)} lng, {round(clicked_marker$lat, 3)} lat]", "{clicked_marker$id} en [{round(clicked_marker$lng, 3)} lng, {round(clicked_marker$lat, 3)} lat]",
  'daily_trends_other_title', "pixel en [{round(clicked_pixel$lng, 3)} lng, {round(clicked_pixel$lat, 3)} lat]", "pixel at [{round(clicked_pixel$lng, 3)} lng, {round(clicked_pixel$lat, 3)} lat]", "pixel en [{round(clicked_pixel$lng, 3)} long, {round(clicked_pixel$lat, 3)} lat]",

  # main tab titles
  'actual_tab_title', "Actual", "Current", "Actual",

  # use file selection
  "user_file_sel_label", "Selecciona l'arxiu a carregar", "Select the file to upload", "Selecciona el archivo a cargar",
  "user_file_sel_buttonLabel", "Inspecciona...", "Browse...", "Inspecciona...",
  "user_file_sel_placeholder", "Cap fitxer seleccionat", "No file selected", "Ningún archivo seleccionado",
  "file_text", 'El fitxer pot ser un shapefile (comprimit en un fitxer zip) o un fitxer GeoPackage (.gpkg). Han de tenir un camp anomenat "poly_id" amb els identificadors dels geometries continguts.', 'File can be a shapefile (compressed in a zip file) or GeoPackage file (.gpkg). They must have a field called "poly_id" with the identifiers of the contained geometries.', 'El archivo puede ser un shapefile (comprimido en un archivo zip) o un archivo GeoPackage (.gpkg). Deben tener un campo llamado "poly_id" con los identificadores de las geometrías contenidas.',

  # sweet alerts
  'sweet_alert_fileext_title', "Format de fitxer no acceptat", "File format not accepted", "Formato de archivo no aceptado",
  'sweet_alert_fileext_text', "L'arxiu carregat ha de ser un zip o gpkg", "Uploaded file must be a zip or a gpkg file", "El archivo cargado debe ser un zip o un gpkg",

  # progress
  "progress_raster", "Obtenció del ràster", "Retrieving the raster", "Obteniendo el ráster",
  "progress_detail_raster", "Això pot trigar una mica", "This may take some time", "Esto puede llevar algo de tiempo",
  "progress_ts", "Càlcul de les sèries temporals", "Calculating the time series", "Calculando las series temporales",
  "progress_detail_ts", "Això pot trigar una mica, en funció del nombre i / o la mida dels objectes espacials", "This may take some time, depending on the number and/or size of the spatial objects", "Esto puede llevar algún tiempo, dependiendo de número y/o tamaño de los objetos espaciales",
  # poly_id_var_check
  "poly_id_missing_title", "No s'ha trobat cap variable anomenada 'poly_id' al fitxer", "Not 'poly_id' variable found in file", "No se ha encontrado ninguna variable llamada 'poly_id' en el archivo",
  "poly_id_missing_message", "S'ha fet servir la primera variable del fitxer com a poly_id", "First variable found in file used as poly_id", "Se ha usado la primera variable del archivo como poly_id"

) %>%
  {.} -> language_dictionary  



# usethis::use_data( app_traduccions ,internal = TRUE, overwrite = TRUE )
