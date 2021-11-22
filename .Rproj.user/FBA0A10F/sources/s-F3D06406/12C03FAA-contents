## Procesamiento de datos Examen - Javier Ortiz ##

## 1. Carga de paquetes: -----------------------------------
pacman::p_load(tidyverse, # para abrir la base de datos original (rds)
               sjmisc, #para explorar datos
               magrittr, #para hacer uso de operadores
               sjPlot, #para visualizar libro de codigos
               dplyr, #para realizar la manipulacion de datos y su exploracion
               car, #para utilizar las funciones mutate y car::recode y trasnformar las variables
               sjlabelled) #para agregar etiquetas a variables y dejarlas debidamente organizadas

## 2. Carga de base de datos original: ------------------------------

data <- read_rds(url("https://github.com/learn-R/examen-Javier-Ortiz-It/raw/main/input/data/Latinobarometro_2020.rds"))
                     
## 3. Explorar datos:--------------------------------------                 

view_df(data)

## 4. Seleccion de variables: -----------------------------

# Nivel de coanficanza

#P13ST.G: Confianza en los Partidos Políticos

# Participacion politica deliverativa

#P62ST.B Las elecciones ofrecen a los votantes una real opción de elegir entre partidos y candidatos
#P56N: Mejor forma de actuar para que Ud. y el país avancen más (categorias deliverativas y participativas)

# Paricipacion politica participativa

#P62N.A Grado de acuerdo con: Las protestas
#P55ST.B: Acción política: Asistir a manifestaciones autorizadas

# valoracion de la democracia

#P22STM.B No me importaría que un gobierno no democrático llegara al poder si resuelve los problemas
#P20STM.C La democracia permite que se solucionen los problemas que tenemos
#P11STGBS.A Satisfacción con la democracia

# Variables demograficas 

#IDENPA: identificacion del pais
#eda
#edad en tramos
#sexo

# seleccion de variables
data_proc <- data %>% 
  select(nc_partidos = p13st_g,
         p62st_b,
         p56n,
         p55st_b,
         p62n_a,
         p22stm_b,
         p20stm_c,
         satisfaccion_demo = P11STGBS_A,
         pais = idenpa,
         edad,
         sexo,
         ponderador = wt,
         numentre)

# Exploracipn de variables seleccionadas
view_df(data_proc)

frq(data_proc$nc_partidos)
frq(data_proc$p62st_b)
frq(data_proc$p56n)
frq(data_proc$p55st_b)
frq(data_proc$p62n_a)
frq(data_proc$p22stm_b)
frq(data_proc$p20stm_c)
frq(data_proc$satisfaccion_demo)
frq(data_proc$pais)
frq(data_proc$sexo)


# 5. Transformacion de variables:----------------------

# a)numero de entrevista, ponderador y variables demograficas

#numero de entrevista
data_proc <- data_proc %>% 
  mutate(numentre = car::recode(.$numentre,
                                recodes = c("-1 = 'NA';
                                        -2 = 'NA';
                                        -3 = 'NA';
                                        -4 = 'NA';
                                        -5 = 'NA'"),
                                as.numeric = T))

#ponderador
data_proc <- data_proc %>% 
  mutate(ponderador = car::recode(.$ponderador,
                                  recodes = c("-1 = 'NA';
                                        -2 = 'NA';
                                        -3 = 'NA';
                                        -4 = 'NA';
                                        -5 = 'NA'"),
                                  as.numeric = T))

#comprobamos
frq(data$wt) #originalmente el ponderador era de tipo caracter
descr(data_proc$ponderador)

#edad y filtro para mayores de edad
data_proc <- data_proc %>% 
  mutate(edad = car::recode(.$edad,
                            recodes = c("-1 = 'NA';
                                        -2 = 'NA';
                                        -3 = 'NA';
                                        -4 = 'NA';
                                        -5 = 'NA'"),
                            as.numeric = T))

#filtro de edad
data_proc <- filter(data_proc, edad >= 18)

#Comprobemos
frq(data$edad) #Edad estaba originalmente como variable de tipo caracter
descr(data_proc$edad)

# creacion de edad en tramos (dejando a grupos desde lo 18 años)
data_proc <- data_proc %>% 
  mutate(edad_tramo = case_when(edad >= 18 & edad <=29 ~ "Adulto joven (18 a 29 anos)",
                                edad >= 30 & edad <=  59 ~ "Adulto (30 a 59 anos)",
                                edad >= 60 ~ "Adulto mayor (60+)",
                                TRUE ~ NA_character_))

#comprobamos 
frq(data$edad)
frq(data_proc$edad_tramo)

#pais y filtro para casos de Chile
# Pais
data_proc <- data_proc %>% 
  mutate(pais = car::recode(.$pais,
                            recodes = c("32 = 'Argentina';
                                        68 = 'Bolivia';
                                        76 = 'Brasil';
                                        152 = 'Chile';
                                        170 = 'Colombia';
                                        188 = 'Costa Rica';
                                        214 = 'Rep. Dominicana';
                                        218 = 'Ecuador';
                                        222 = 'El Salvador';
                                        320 = 'Guatemala';
                                        340 = 'Honduras';
                                        484 = 'Mexico';
                                        558 = 'Nicaragua';
                                        591 = 'Panama';
                                        600 = 'Paraguay';
                                        604 = 'Peru';
                                        724 = 'NA';
                                        858 = 'Uruguay';
                                        862 = 'Venezuela';
                                        -1 = 'NA';
                                        -2 = 'NA';
                                        -3 = 'NA';
                                        -4 = 'NA';
                                        -5 = 'NA'"),
                            as.factor = T,
                            levels = c('Argentina',
                                       'Bolivia',
                                       'Brasil',
                                       'Chile',
                                       'Colombia',
                                       'Costa Rica',
                                       'Rep. Dominicana',
                                       'Ecuador',
                                       'El Salvador',
                                       'Guatemala',
                                       'Honduras',
                                       'Mexico',
                                       'Nicaragua',
                                       'Panama',
                                       'Paraguay',
                                       'Peru',
                                       'Uruguay',
                                       'Venezuela')))

# Filtro para casos de Chile
data_proc <- filter(data_proc, pais == "Chile")

#Sexo 
data_proc <- data_proc %>% 
  mutate(sexo = car::recode(.$sexo,
                            recodes = c("1 = 'Hombre';
                                        2 = 'Mujer'"),
                            as.factor = T,
                            levels = c('Hombre',
                                       'Mujer')))
#comprobamos
frq(data_proc$sexo)
  
# b) politica representativa/deliverativa
# Confianza en los Partidos politicos
data_proc <- data_proc %>% 
  mutate(nc_partidos = car::recode(.$nc_partidos,
                                   recodes = c("1 = 'Mucha';
                                               2 = 'Algo';
                                               3 = 'Poca';
                                               4 = 'Ninguna';
                                               -1 = 'No sabe';
                                               -2 = 'No contesta';
                                               -3 = 'NA';
                                               -4 = 'NA';
                                               -5 = 'NA'"),
                                   as.factor = T,
                                   levels = c('Ninguna',
                                              'Poca',
                                              'Algo',
                                              'Mucha',
                                              'No sabe',
                                              'No contesta'))) 
# Comprobamos
frq(data$p13st_g)
frq(data_proc$nc_partidos)

# Las elecciones ofrecen a los votantes una real opción de elegir entre partidos y candidatos
data_proc <- data_proc %>% 
  mutate(p62st_b = car::recode(.$p62st_b,
                               recodes = c("1 ='Muy de acuerdo';
                                           2 = 'De acuerdo';
                                           3 = 'En desacuerdo';
                                           4 = 'Muy en desacuerdo';
                                           -1 = 'NA';
                                           -2 = 'NA';
                                           -3 = 'NA';
                                           -4 = 'NA';
                                           -5 = 'NA'"),
                               as.factor = T,
                               levels = c('Muy en desacuerdo',
                                          'En desacuerdo',
                                          'De acuerdo',
                                          'Muy de acuerdo')))

#comrpobamos
frq(data$p62st_b)
frq(data_proc$p62st_b)

#Mejor forma de actuar para que Ud. y el país avancen más
data_proc <- data_proc %>% 
  mutate(p56n = car::recode(.$p56n,
                            recodes = c("1 = 'Hay que votar siempre';
                                        2 = 'Hay que votar pero tambien protestar';
                                        3 = 'Solo hay que protestar, no sirve votar';
                                        4 = 'No hay que hacer nada, ni votar ni protestar';
                                        5 = 'No hay que hacer nada';
                                        -1 = 'NA';
                                        -2 = 'NA';
                                        -3 = 'NA';
                                        -4 = 'NA';
                                        -5 = 'NA'"),
                            as.factor = T,
                            levels = c('Hay que votar siempre',
                                       'Hay que votar pero tambien protestar',
                                       'Solo hay que protestar, no sirve votar',
                                       'No hay que hacer nada, ni votar ni protestar',
                                       'No hay que hacer nada')))

# Comprobemos
frq(data$p56n)
frq(data_proc$p56n)

# b)politica participativa
#Grado de acuerdo con las protestas
data_proc <- data_proc %>% 
  mutate(p62n_a = car::recode(.$p62n_a,
                              recodes = c("1 = 'Muy de acuerdo';
                                          2 = 'De acuerdo';
                                          3 = 'En desacuerdo';
                                          4 = 'Muy en desacuerdo';
                                          -1 = 'NA';
                                          -2 = 'NA';
                                          -3 = 'NA';
                                          -4= 'NA';
                                          -5 = 'NA'"),
                              as.factor = T,
                              levels = c('Muy en desacuerdo',
                                         'En desacuerdo',
                                         'De acuerdo',
                                         'Muy de acuerdo')))

#comprobamos
frq(data$p62n_a)
frq(data_proc$p62n_a)

#Acción política: Asistir a manifestaciones autorizadas
data_proc <- data_proc %>% 
  mutate(p55st_b = car::recode(.$p55st_b,
                               recodes = c("1 = 'Lo ha realizado';
                                           2 = 'Lo podria realizar';
                                           3 = 'Nunca lo haria';
                                           -1 = 'NA';
                                           -2 = 'NA';
                                           -3 = 'NA';
                                           -4 = 'NA';
                                           -5 = 'NA'"),
                               as.factor = T,
                               levels = c('Lo ha realizado',
                                          'Lo podria realizar',
                                          'Nunca lo haria')))

#Comprobamos
frq(data$p55st_b)
frq(data_proc$p55st_b)

## valoracion de la democracia
# No me importaría que un gobierno no democrático llegara al poder si resuelve los problemas
data_proc <- data_proc %>% 
  mutate(p22stm_b = car::recode(.$p22stm_b,
                                recodes = c("1 = 'Muy de acuerdo';
                                            2 = 'De acuerdo';
                                            3 = 'En desacuerdo';
                                            4 = 'Muy en desacuerdo';
                                            -1 = 'NA';
                                            -2 = 'NA';
                                            -3 = 'NA';
                                            -4 = 'NA'"),
                                as.factor = T,
                                levels = c('Muy en desacuerdo',
                                           'En desacuerdo',
                                           'De acuerdo',
                                           'Muy de acuerdo')))

#Comprobemos
frq(data$p22stm_b)
frq(data_proc$p22stm_b)

#La democracia permite que se solucionen los problemas que tenemos
data_proc <- data_proc %>% 
  mutate(p20stm_c = car::recode(.$p20stm_c,
                                recodes = c("1 = 'Muy de acuerdo';
                                            2 = 'De acuerdo';
                                            3 = 'En desacuerdo';
                                            4 = 'Muy en desacuerdo';
                                            -1 = 'NA';
                                            -2 = 'NA';
                                            -3 = 'NA';
                                            -4 = 'NA'"),
                                as.factor = T,
                                levels = c('Muy en desacuerdo',
                                           'En desacuerdo',
                                           'De acuerdo',
                                           'Muy de acuerdo')))

#Comprobemos
frq(data$p20stm_c)
frq(data_proc$p20stm_c)

#Satisfacción con la democracia
data_proc <- data_proc %>% 
  mutate(satisfaccion_demo = car::recode(.$satisfaccion_demo,
                                         recodes = c("1 = 'Muy satisfecho';
                                                     2 = 'Satisfecho';
                                                     3 = 'No muy satisfecho';
                                                     4 = 'Nada satisfecho';
                                                     -1 = 'No sabe';
                                                     -2 = 'NA';
                                                     -3 = 'NA';
                                                     -4 = 'NA';
                                                     -5 = 'NA'"),
                                         as.factor = T,
                                         levels = c('Nada satisfecho',
                                                    'No muy satisfecho',
                                                    'Satisfecho',
                                                    'Muy satisfecho',
                                                    'No sabe')))

#Comprobemos
frq(data$P11STGBS_A)
frq(data_proc$satisfaccion_demo)

# 6. Agregar etiquetas a variables:--------------------------
data_proc$nc_partidos <- set_label(data_proc$nc_partidos, 'Confianza en los Partidos Politicos')
data_proc$p62st_b <- set_label(data_proc$p62st_b, 'Las elecciones ofrecen a los votantes una real opcion de elegir entre partidos y candidatos')
data_proc$p56n <- set_label(data_proc$p56n, 'La mejor forma de actuar para que Ud. y el pais avancen mas')
data_proc$p62n_a <- set_label(data_proc$p62n_a, 'Grado de acuerdo con las protestas')
data_proc$p55st_b <- set_label(data_proc$p55st_b, 'Accion politica: Asistir a manifestaciones autorizadas')
data_proc$p22stm_b <- set_label(data_proc$p22stm_b, 'No me importaria que un gobierno no democratico llegara al poder si resuelve los problemas')
data_proc$p20stm_c <- set_label(data_proc$p20stm_c, 'La democracia permite que se solucionen los problemas que tenemos')
data_proc$satisfaccion_demo <- set_label(data_proc$satisfaccion_demo, 'Satisfaccion con la democracia')
data_proc$pais <- set_label(data_proc$pais, 'Pais')
data_proc$edad <- set_label(data_proc$edad, 'Edad')
data_proc$edad_tramo <- set_label(data_proc$edad_tramo, 'Edad en tramos')
data_proc$sexo <- set_label(data_proc$sexo, 'Sexo')
data_proc$ponderador <- set_label(data_proc$ponderador, 'Ponderador')
data_proc$numentre <- set_label(data_proc$numentre, 'Id entrevista')

#revisemos libro de codigos de la base recortada
view_df(data_proc,
        encoding = "UTF-8")

# 7. Guardar base de datos procesada:-----------------------

saveRDS(data_proc, file = "output/data/datos_proc.rds")

