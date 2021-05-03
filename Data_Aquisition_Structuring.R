# Cette version du code est fortement commentée.

#---------------------- Importation Packages --------------------
library(jsonlite)
library(plyr)
library(magrittr)
library(data.table)
library(rbenchmark)
library(rvest)
#_________________________________________________________________________________________________________________________________________
#                                                 Récupération de données via une API
#_________________________________________________________________________________________________________________________________________
api_key<- "put your API key here"
# Premier appel uniquement pour explorer la structure retournée
urlAPI<- paste0("https://api.nasa.gov/neo/rest/v1/feed?start_date=2020-04-09&end_date=2020-04-16&api_key=", api_key)
neo <- fromJSON(urlAPI)
View(neo)
#------------ Exploration de la structure récupérée -----------

class(neo)
View(neo)
names(neo)
class(neo$near_earth_objects)
View(neo$near_earth_objects)
str(neo$near_earth_objects[1])
View(neo$near_earth_objects[[1]])
View(neo$near_earth_objects$"2020-01-01"$links)
View(neo$near_earth_objects$"2020-01-01"$close_approach_data)

# La structure globale est une liste ayant 3 champs:
#links : contenant les liens vers les données json des 7 jours précédants la date début de la requete, les 7 jours suivant la date fin de
#la requete et vers les données de la requete  : ce champs ne nous interesse pas
#count : un entier indiquant le nombre d'individus retournés par la requete :uniquement une donnée d'exploration, ne sera pas gardé pour le jeu de données
#near_earth_objects : contenant les données effectives
# La structure de 'neo$near_earth_objects' est celle d'une liste de dataframes 
# Chaque elmnt de la liste correspondant à une date de la plage de la requete
# et est un data frame d'un nombre variable d'individus et de ... variables 
# 3 de ces variables( links, estimated diameter etclose_approach_data) sont respectivement des dataframes et une liste de dataframes

#_________________________________________________________________________________________________________________________________________
#---------------  Transformation de la structure: but: avoir un dataframe (ou data.table) contenant toutes les variables -----------------

# Les dataframes de la liste globale (neo$near_earth_objects) contiennet des variables qui ont des dataframes
# Les concaténer avec les méthodes de ransformation de liste de datafrmes en un dataframe 
#(à savoir ldply(x,rbind), rbind.fill(x ),rbindlist(x)  avec un lapply sur chaque element de la liste globale) ne fonctionnent pas 
# Solution adoptée :
# 1- Extraction de ces 3 variables , 2- leur ransformation en data.frames/data.tables ,3- leur suppression de la structure globale
# 4- tranformation de celle-ci en data.frame (les méthodes citées plu haut marcheront donc)   5- concaténation de ce dernier avec les 3 dataframes
# des 3 variables

#-----------------------------------------------------------------------------------------------------------------------------------------

#La fonction prend en entrée la liste near_earth_object, en extrait les 3 variables de type structures, les transforment en data.tables
#les supprime de la liste, transforme cette dernière en data.table , et enfin concatène ces 4 data.tables et retourne un data.table
construct_neo_df <- function(neolist){
      
      # I -------- La variable close_approach_data (liste de dataframes ) ---------
      
      close_approach <- lapply(neolist,
                               function(x) {unlist(x[['close_approach_data']],recursive = T)}
                               ) %>%                             # A ce niveau on a une liste dont chaque elmnt est un vecteur de caractères
      
                        lapply(.,                                                                             #les transformer en dataframes
                               function(x){ x <-x %>% split(.,names(.)) %>% data.frame(.,stringsAsFactors = F); x}
                               ) %>%                             # A ce niveau on a une liste de dataframes (les listes close_approach_data 
                                                                                         # de dataframes transformées en un seul dataframe)
                        rbindlist()                                               # Transformer cette liste en un seul datframe(data.table)
      
      #  Pour la transformation d'une liste de dtaframes en un dataframe les
      #  3 méthodes suivantes ont été Benchmarkées  pour voir quelle est la plus rapide
      #  (Ceci a été fait en dehors de cette fonction et a été mis ici en commentaire uniquement pour le mentionner)
      #  benchmark(ldply(close_approach,rbind), rbind.fill(close_approach),rbindlist(close_approach))
      #  Methode retenue: rbindlist(close_approach) la plus rapide ( à noter que celle-ci retourne donc un data.table)

      #View(close_approach)
      #class(close_approach)  #"data.table" "data.frame" 
      
      # II -------- La variable estimated_diameter(dataframe de dataframes ) ---------
      
      estim_diam <- lapply(neolist,
                           function(x) {x <- unlist(x[['estimated_diameter']],recursive = F) %>% as.data.frame(.) ;x}
                           ) %>%                                                                     # A ce niveau: une liste de dataframes
                    rbindlist()                                                   # Transformer cette liste en un seul datframe(data.table)
      #View(estim_diam) 
      
      # III------------------------------------- La variable links(dataframe) -----------------------------------
      links <- lapply(neolist,function(x) {x <- x[['links']];x}) %>%                                 # A ce niveau: une liste de dataframes
               rbindlist()                                                        # Transformer cette liste en un seul datframe(data.table)
      #View(links)
      
      # -------------------------- Suppression de ces variables de la liste globale ---------------------
      nr_erth_objs_f <- lapply(neolist,
                               function(x) { x[c("links","close_approach_data","estimated_diameter")] <- NULL; x }
                               )
      #View(nr_erth_objs_f)
      
      # ------------------------ Transformation de cette nouvelle liste en dataframe(data.table) -------------------------
      #      + Concaténation avec les data.tables close_approach,estim_diam et links pour constituer le dataframe global
      nr_erth_objs_f<- rbindlist(nr_erth_objs_f)%>%
                       cbind(.,close_approach,estim_diam,links)
      
      
      return(nr_erth_objs_f)
}

#--------------------------

# Dataframe contenant les dates début et fin des appels API (puisque avec un appel on peut avoir au maximum les données sur 7 jours
#pour en avoir plus il faut faire plusieurs appels)

dates <- data.frame(start_date=c('2020-04-01','2020-04-08'),end_date=c('2020-04-09','2020-04-16'),stringsAsFactors = FALSE)  
# (l'usage de tibble pour ce genre de données intermédiares ,rien que pour éviter stringsAsFactors = FALSE, n'est pas pertinent )
#--------------------------
# Fonction pour construire les appels API. Prend en paramètre un dataframe de dates début et fin, Retourne une liste de chaines
#de caractères

construct_API_call <- function(dates){
   
   l <- lapply(dates,
               function(x){paste0('https://api.nasa.gov/neo/rest/v1/feed?start_date=',as.character(x[1])) %>% 
                           paste0(.,'&end_date=',as.character(x[2]),'&api_key=Lg2Kk3Pr2yWvlrUTRwnABZj2yNqwj2SNBknWMVhx')
                          } 
               )
   #print(l)
   return(l)  
}

#----------------------- Appels API, construction du dataframe final des données API ------------------------------------------------

nr_erth_objs <- construct_API_call(dates) %>%
                       lapply(.,fromJSON) %>%                                          # une liste des listes globales retournées
                       lapply(.,function(x) {x <- x[['near_earth_objects']];x}) %>%    # Extraction de la liste near_earth_objects
                                                                                       # de chaque liste globale
                       lapply(.,construct_neo_df) %>%                                  # A ce niveau on a une liste de dataframes
                       rbindlist()                                                     # Concaténation des dataframes en un seul

class(nr_erth_objs)
View(nr_erth_objs)


#_________________________________________________________________________________________________________________________________________
#                                    Récupération de données supplémentaires avec du web scrapping 
#_________________________________________________________________________________________________________________________________________
# Chaque individu des données récupérées via l'API a un lien vers une page HTML fournissant 
# plus d'informations sur celui-ci (la variable nasa_jpl_url) 
# C'est cette page que nous scrappons ici
#---------------------------------------------------------------------------------------------------------------------------

# Cette fonction prend en entrée l'url nasa jpl , et scrappe 2 elements : un href contenant la classification de l'orbite de l'asteroid 
# et une table contenant des informations orbitales de celui-ci, elle retourne un data.table de toutes ces données
jpl_info <- function(url) {
      jplpage <- read_html(url)
      
      # --------- classe de l'objet spatial selon son orbit---------
      orbit_class <- html_nodes(jplpage, 'font+ font a') %>%
                     html_text()
      
      # ------------ Données orbitales --------------
      orbital_data <- html_nodes(jplpage, css = 'td td td td td table:nth-child(2) td') %>%
                      html_nodes("table") %>%
                                 # orbital_data es une liste de 2 tables html, uniquement la première contient les données nous interessant
                                 # cette table est transformée en data.frame (ensuite transposée pour que les variables soient en colonnes)

                             html_table() %>%
                                 '[['(1)  %>% 
                             transpose(make.names=1) 
      
      #--------- Construction d'un data.frame à partir de ce qui est récupéré --------
      result <- cbind(orbital_data,orbit_class)
      
      Sys.sleep(runif(1,0.75,0.9))  
      return (result)
}

# Accéder à toutes les url NASA JPL des individus du data.table récupéré via l'API et appeler la fonction de scrapping avec
orbit_data <- lapply(nr_erth_objs$nasa_jpl_url,jpl_info) %>%    # res est une liste de dataframes, concaténer ses elements(les dataframes)
                                                                 # pour constituer un seul dataframe à partir de ceux-ci (ils ont potentiellement  
                                                                #  un nombre de variables différents , si c'est le cas, remplir avec NA les
                                                                                                            #cellules vides(d'où fill=TRUE)) 
              rbindlist(.,fill=TRUE)
class(orbit_data)
View(orbit_data) 


#__________________________________________________________________________________________________________________________________
#                               Concaténation des data.table API et Scrapping + Nettoyage des données(types,formats)
#__________________________________________________________________________________________________________________________________

# I-----------------------------------------  Suppression et réorganisation des variables -----------------------------------------

tables()  #juste pour voir kes tables existantes en mémoire (nr_erth_objs:celle des données obtenues via lAPI et orbit_data :celle des données obtenues avec web scrapping )

#1----- Concaténation des 2 data.tables
neo_dt <- cbind(nr_erth_objs,orbit_data)
class(neo_dt)   # un data.table

#2----- Suppression des variables inutiles
# Ont été supprimées:
#Les variables doppler.obs.used et delay.obs.used parce que la plupart des individus étaient à NA pour ces variables
# Les variables orbiting_body,source....  parceque tous les individus ont la meme valeur pour cette variable
# Les variables miss_distance_miles, miss_distance_meters...  parceque elles donnent la meme information avec des unités
#de mesures différentes 
#Les variables first.obs.used et last.obsv.used parce qu'elles sont résumées par data.arc.span (le nombre de jours pendant lesquels l'objet
#a été observé pour calculer ces données orbitales)
#La variable self parce que le lien vers ces meme données en html ne nous interesse pas
#La variable id parce que redendonte (neo_reference_id)

neo_dt <- neo_dt[,-c(1,7,9,10,13:16,18,19,20,23:27,30:33,36:38,40,41)] %>%

#3----- Réorganisation des variables (changement d'ordre) 
          setcolorder(., c(1,2,4,10,11,5,16,6,7,8,9,13,12,15,14,3))

# II--------------------------------------------------- 'Nettoyage' des données --------------------------------------------------

str(neo_dt) 
neo_dt$'# obs. used (total)' <- as.integer(neo_dt$'# obs. used (total)')
neo_dt$'condition code' <- neo_dt$'condition code'%>% as.integer(.) %>% as.factor(.)

#Les variables qui doivent etre converties de char en numeric
neo_dt$'miss_distance.astronomical' <- as.numeric(neo_dt$'miss_distance.astronomical')
neo_dt$'miss_distance.kilometers' <- as.numeric(neo_dt$'miss_distance.kilometers')
neo_dt$'relative_velocity.kilometers_per_second'<- as.numeric(neo_dt$'relative_velocity.kilometers_per_second')
neo_dt$'norm. resid. RMS' <- as.numeric(neo_dt$'norm. resid. RMS')

#Les manières de faire ci dessous n'ayant pas fonctionné
# lapply(list(neo_dt$'norm. resid. RMS',neo_dt$'miss_distance.astronomical',neo_dt$'miss_distance.kilometers',neo_dt$'relative_velocity.kilometers_per_second'),
#        function(x){print(x); x <<- as.numeric(x)})
# colum <- c(10,11,12,15)
# neo_dt[colum] <- sapply(neo_dt[colum],as.numeric)
#neo_dt[,c(10,11,12,15)] <- apply(neo_dt[,c(10,11,12,15)],length(colum),function(x){ x <- as.numeric(x);x})

#La variable close_approach_date de char a date
neo_dt$'close_approach_date' <- as.Date(neo_dt$'close_approach_date')

# Supprimer les chaines du genre "days..." de la vraibales du nombre de jours d'observation, pour garder ce nombre uniquement
neo_dt$'data-arc span'<- neo_dt$'data-arc span' %>% 
                                gsub(" .*","",.) %>%
                                as.integer()
# cette variable a été renommée uniquement pour mentionner que l'unité est donc days
setnames(neo_dt,'data-arc span','data-arc span(in days)')  

View(neo_dt)



# Ceci a été tenté (avec le package lubridate) pour extraire l'heure et minutes d'approche en vain, donc la variable close_approach_date_full
#a finalement été supprimée
# neo_dt$'close_approach_date_full' <- neo_dt$'close_approach_date_full'%>%
#    #substring(.,13) %>%
#    #format(as.POSIXct(strptime(.,"%d/%m/%Y %H:%M",tz="")) ,format = "%H:%M") 
#    ymd_hm()

#------------------------------- Exportation du jeu de données --------------------------------------------------
fwrite(neo_dt, "NEO.csv", row.names=FALSE,quote = 'auto')
#__________________________________________________________________________________________________________________________________

