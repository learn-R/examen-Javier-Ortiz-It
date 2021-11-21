
# Práctico 7: Trabajo con muestras complejas ------------------------------


# 1. Cargar librerías -----------------------------------------------------

pacman::p_load(tidyverse, #Universo de paquetes
               sjmisc, #Para explorar datos
               srvyr, #Para trabajar con muestras complejas
               dplyr, #Para manipular datos
               tidyr) #Para transformar la estructura de los datos


# 2. Cargar datos ---------------------------------------------------------

data <- readRDS(url("https://github.com/learn-R/07-class/blob/main/input/data/casen_proc.rds?raw=true"))
head(data)

# 3. Explorar datos -------------------------------------------------------

## Variables categóricas

frq(data$region) 
frq(data$pobreza)
frq(data$sexo)

## Variables cuantitativas

descr(data$exp) #Ponderador regional
sum(data$exp) #Total de la población
descr(data$varstrat) #Estrato de varianza
descr(data$varunit) #Conglomerado de varianza
descr(data$ing_tot_hog)

# 4. Crear objeto encuesta ------------------------------------------------

data <- data %>% 
  group_by(varstrat) %>% #Agrupando por varstrat
  mutate(stratn = sum(exp)) #Calculamos el total de personas por estrato

casen_regional <- data %>% #Creamos un nuevo objeto llamado casen_regional con la información de data
  as_survey_design(ids = varunit, #Aplicamos diseño muestral, especificando los ids a partir de varunit,
                   strata = varstrat,#los estratos a partir de varstrat,
                   fpc = stratn, #especificando que la estimación es con una población finita
                   weights = exp) #y los ponderadores con exp

# 5. Cálculo de medias para variables cuantitativas -----------------------

## Cálculo simple
casen_regional %>% #Con casen_regional
  summarize(ing_medio = srvyr::survey_mean(ing_tot_hog, na.rm=T)) #Calculamos el ingreso medio poblacional

## Comparemos
data %>% #Con data
  summarise(ing_medio = mean(ing_tot_hog, na.rm=T)) #Calculamos el ingreso medio muestral


## Con Intervalos de confianza al 95%
casen_regional %>%#Con casen_regional
  summarise(ing_medio = survey_mean(ing_tot_hog, vartype = "ci", na.rm=T)) #Calculamos el 
                                                                           #ingreso medio poblacional, 
                                                                           #y sus intervalos de confianza

## Con Intervalos de confianza al 95% y al 99%
casen_regional %>% #Con casen_regional
  summarise(ing_medio95 = survey_mean(ing_tot_hog, vartype = "ci", level = .95, na.rm=T), #Al 95%
            ing_medio99 = survey_mean(ing_tot_hog, vartype = "ci", level = .99, na.rm=T)) #Al 99%

## Agrupando por sexo
casen_regional %>% #Con casen_regional
  group_by(sexo) %>% #Agrupamos por sexo
  summarise(ing_medio = survey_mean(ing_tot_hog, vartype = "ci", na.rm=T)) #Calculamos el ingreso medio 
                                                                           #poblacional, y sus intervalos de 
                                                                           #confianza
## Agrupando por región
casen_regional %>% #Con casen_regional
  group_by(region) %>% #Agrupamos por region
  summarise(ing_medio = survey_mean(ing_tot_hog, vartype = "ci", na.rm=T)) #Calculamos el ingreso medio 
                                                                           #poblacional, y sus intervalos de 
                                                                           #confianza

## Crear dataframe pivoteando datos
ing_region <- casen_regional %>% 
  group_by(sexo) %>% #Agrupamos por region
  summarise(ing_medio = survey_mean(ing_tot_hog, vartype = "ci", na.rm=T)) %>% #Calculamos el ingreso medio poblacional, y sus intervalos de confianza
  select(sexo, ing_medio) %>% #Seleccionamos region e ing_medio
  pivot_wider(names_from = "sexo", #Pivoteamos, extrayendo los nombres de las columnas desde region
              values_from = "ing_medio") #Y los valores desde ing_medio

head(ing_region) #Visualizamos


# 6. Cálculo de proporciones y frecuencias para variables categóricas ----------------------

## Cálculo simple
casen_regional %>% #Con casen_regional
  summarise(prop = survey_prop(pobreza, na.rm = T)) #Y calculamos las proporciones

## Cálculo simple
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(na.rm = T)) #Y calculamos las proporciones

## Con survey_mean
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_mean(na.rm = T)) #Y calculamos las proporciones

## Transformando a porcentaje
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(na.rm = T))%>% #Calculamos las proporciones
  mutate(per = prop*100) #Creamos una nueva columna multiplicando las proporciones *100 para obtener porcentajes

## Incorporamos cálculo de frecuencias
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(na.rm = T), #Calculamos las proporciones
            total = survey_total(na.rm=T))%>% #Y el total por categorías
  mutate(per = prop*100) #Creamos una nueva columna multiplicando las proporciones *100 para obtener porcentajes

## Con Intervalos de confianza al 95%
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T)) #Incorporamos intervalos de confianza

## Transformamos el estimador puntual y los límites del intervalo a porcentajes
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T)) %>% #Incorporamos intervalos de confianza
  mutate(prop = prop*100, #Multiplicamos las proporciones *100,
         prop_low = prop_low*100, #así como el límite inferior 
         prop_upp = prop_upp*100) #y superior, para obtener porcentajes

## Incorporamos el total
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(prop = prop*100, #Multiplicamos las proporciones *100,
         prop_low = prop_low*100, #así como el límite inferior 
         prop_upp = prop_upp*100) #y superior, para obtener porcentajes

## Cruzar dos variables
casen_regional %>% #Creamos un objeto llamado pobreza_reg con datos de casen_regional
  group_by(pobreza, sexo) %>% #Agrupamos por pobreza y sexo
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(prop = prop*100)

## Crear objeto wide
pobreza_reg <- casen_regional %>% #Creamos un objeto llamado pobreza_reg con datos de casen_regional
  group_by(region, pobreza) %>% #Agrupamos por region y pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(per = prop*100) %>% #Multiplicamos las proporciones *100 para obtener porcentajes
  select(region, pobreza, per, total) %>% #Seleccionamos region, pobreza, per y total
  pivot_wider(names_from = "pobreza", #Pivoteamos a lo ancho, extrayendo los nombres de las columnas desde pobreza
              values_from = c("per", "total")) #y los valores desde per y total

head(pobreza_reg)
