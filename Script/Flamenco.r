flamenco_pesticides <- function(){
  #regarde les pesticides issus de Flamenco
  library(hubeau)
  library(dplyr)
  library(sf)
  library(purrr)
  library(mapview)
  f_get_pesti_station <- function(station){
    return(get_qualite_rivieres_analyse_pc(code_station=station,code_parametre=c('2077','1141','1288','1149',
                                                                                 '1480','1212','1765','2804',
                                                                                 '1810','1816','5562','5617'),
                                           date_debut_prelevement='2019-01-01'))
  }
  
 


    stations <- get_qualite_rivieres_station(code_region='53') %>% 
    select(c(1,2,9,10,12,13))
    #recup du libelle sttaion
    stationslibelle <- read.csv('Data/Stations.CSV',header=T,sep=';') %>% select(1:2)
    stations <- left_join(stations,stationslibelle,by=c('code_station'='CdStationMesureEauxSurface')) %>% 
      select(-2)
  
# on ne peut pas injecter dans l'API
# + de 200 stations, on le fait petit à petit alors !
nbsta <- dim(stations) 
nbsta <- nbsta[1]
i <- 1
mat = matrix(ncol = 190, nrow = 0)
rep <- data.frame(mat)
while (i<=nbsta){
print(i)  
sta200 <- tryCatch(
  {
    # Just to highlight: if you want to use more than one
    # R expression in the "try" part then you'll have to
    # use curly brackets.
    # 'tryCatch()' will return the last evaluated expression
    # in case the "try" part was completed successfully
    
    message("This is the 'try' part")
    
    data <- map_df(stations[i:min(i+50,nbsta),1],f_get_pesti_station)
    # The return value of `readLines()` is the actual value
    # that will be returned in case there is no condition
    # (e.g. warning or error).
  },
  error = function(cond) {
    message("erreur")
    message(conditionMessage(cond))
    # Choose a return value in case of error
    0
  },
  warning = function(cond) {
    message("warning message")
    message(conditionMessage(cond))
    # Choose a return value in case of warning
    NULL
  },
  finally = {
    # NOTE:
    # Here goes everything that should be executed at the end,
    # regardless of success or error.
    # If you want more than one expression to be executed, then you
    # need to wrap them in curly brackets ({...}); otherwise you could
    # just have written 'finally = <expression>' 
    message(paste("fin du traitement"))
  }
)
#on teste s'il y a de la donnée dans ce qu'on a récupéré, si oui on aoute
# a ce qu'on a , sinon on ne fait rien
if(dim(sta200)[2]==190){
  rep <- rbind(rep,sta200)
}
i <- i+51
}

##### traitement
Analyses <- select(rep,c(1,10,14,15,17,48))
stationsjointure <- select(stations,c(1,2,3,6))
Analysesgeom <- left_join(Analyses,stationsjointure,by='code_station') %>% 
  filter(date_prelevement>'2019-01-01') %>% filter(resultat>0.1) %>% 
group_by(code_station) %>% 
  summarise(nb_depassements=n()) %>%
  left_join(stationsjointure,by=('code_station')) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
mapview::mapview(Analysesgeom,cex='nb_depassements',zcol='nb_depassements',
                 at=seq(5,max(Analysesgeom$nb_depassements),5),
                label=Analysesgeom$LbStationMesureEauxSurface)




      ## recupération des ventes à la commune
      Ventes_communes <- readxl::read_xlsx('Data/commune.xlsx') %>% 
      group_by(Cpostal) %>%  summarise(nb=n())
      #recup base code insee CP
      code_insee_cp <- readxl::read_xlsx('Data/CP-inseel.xlsx') %>% select(1,3)
      code_insee_cp$code_insee <- as.character(code_insee_cp$code_insee)
      Communes <- read_sf('Data/COMMUNE.shp') %>% 
        filter(INSEE_REG=='53') %>% 
        select(2,4,12)  %>% 
        left_join(code_insee_cp,by=c('INSEE_COM'='code_insee')) %>% select(1,4,2,3)
      Communes$Code_postal <- as.character(Communes$Code_postal)
      
      Ventes_communes_geom <- left_join(Communes,Ventes_communes,by=c('Code_postal'='Cpostal')) %>% 
        filter(!is.na(INSEE_COM)) %>% 
        filter(!is.na(nb))
      ggplot(Ventes_communes_geom)+geom_sf(color='white',aes(fill='nb'))
      
      Nb_ventes_par_commune <- Ventes_communes_geom
      pal = mapviewPalette("mapviewVectorColors")
      reds =reds = colorRampPalette(c('pink', 'red'))
    mapview(Nb_ventes_par_commune,zcol='nb',alpha.regions=0.9,col.regions = reds)+ mapview::mapview(Analysesgeom,cex='nb_depassements',zcol='nb_depassements',
                                                at=seq(5,max(Analysesgeom$nb_depassements),5),
                                                label=Analysesgeom$LbStationMesureEauxSurface)
  
}
