# Taller para SobreMéxico (UIA) | Mercado laboral en México durante la pandemia ----
# ENOE (2020T1) y ENOE_n (2020T3 y 2020T4)
# ¿Por qué no ETOE para 2020T2?
# Metodología telefónica no es necesariamente comparable con encuestas en vivienda.

# ¿Qué parte del mercado laboral queremos analizar? 
# Me enfocaré en ingresos laborales

# Problema: alrededor del 30% de las personas no reportan ingresos laborales
# estos missing values no son aleatorios, los deciles más altos tienden a dejar de reportar ingresos
# Solución CONEVAL: imputar NAs con pregunta de salarios mínimos
# Deficiencia: muy pocas personas contestan esta pregunta sin contestar ingresos mensuales
# El método Hotdeck: explicación del método


Sys.setlocale("LC_TIME", "es_ES")

# Paquetes ----
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("hot.deck")) install.packages("hot.deck") & require("hot.deck")
if(!require("zoo")) install.packages("zoo") & require("zoo")
if(!require("stringi")) install.packages("stringi") & require("stringi")
if(!require("gganimate")) install.packages("gganimate") & require("gganimate")
if(!require("gridExtra")) install.packages("gridExtra") & require("gridExtra")
if(!require("ggthemes")) install.packages("ggthemes") & require("ggthemes")
if(!require("hrbrthemes")) install.packages("hrbrthemes") & require("hrbrthemes")
if(!require("magick")) install.packages("magick") & require("magick")
if(!require("scales")) install.packages("scales") & require("scales")
if(!require("RColorBrewer")) install.packages("RColorBrewer") & require("RColorBrewer")
if(!require("foreign")) install.packages("foreign") & require("foreign")
if(!require("VIM")) install.packages("VIM") & require("VIM")
if(!require("geofacet")) install.packages("geofacet") & require("geofacet")
if(!require("srvyr")) install.packages("srvyr") & require("srvyr")

# Tidyverse <3
require(tidyverse)



# Desactiva notación científica
options(scipen=999)

# Colores MCV -----
mcv_discrete <- c(
  "#6950d8", "#00b783", "#ff6260", "#ffaf84", "#ffbd41"
)

mcv_semaforo <- c("#00b783", "#ff6260", "#ffbd41")

mcv_blacks <- c("black", "#D2D0CD", "#777777")

# Directorios ----
paste_inp <- function(x){paste0("mcv/enoe/01_datos/", x)}
paste_out <- function(x){paste0("mcv/enoe/03_gráficas/", x)}
fiuffi <- "Fuente: elaboración de @MexicoComoVamos con información del @INEGI_INFORMA | @guzmart_\nNo se incluyeron a trabajadores sin pago. Se utilizó el método de imputación hotdeck para tratar la no respuesta."

# 1. Imputación hotdeck (sólo hacer para replicar imputación) ----
# 1.1 ENOE_2020T1 ----
# Vectores para loop
year_vector <- c(
  "05",  "06",  "07",  "08", "09", "10", "11", "12",  "13",  "14",  "15",  "16",  "17",  "18", "19", "20"
)

trim_vector <- c("1","2","3","4")
i = 16
x = 1

# Microdatos ENOE contiene 5 bases de datos
# Cuestionario de Ocupación y Empleo 1 - COE1T120.dbf
# Cuestionario de Ocupación y Empleo 2 - COE2T120.dbf
# Sociodemográficos - SDEMT120.dbf
# Hogares - HOGT120.dbf
# Vivienda - VIVT120.dbf

# Sólo nos interesan las primeras tres bases (COEs y SDEM)

d_sdem <- read.dbf(
  # Abrir archivo de acuerdo con rutas diseñadas con base en nombres
  paste_inp(
    paste0("20", year_vector[i], "trim", trim_vector[x], "_dbf/SDEMT", trim_vector[x], year_vector[i], ".dbf")
  ), as.is = T
) %>% 
  rename_all(tolower) %>% 
  mutate(
    # La "llaveh" es la clave que nos permite hacer joins entre bases de datos por hogar
    llaveh = paste0(
      cd_a, ent, con, v_sel, n_hog, h_mud
    ),
    # La "llave" es la clave que nos permite hacer joins entre bases de datos por persona
    llave = paste0(
      cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
    )
  ) %>% 
  mutate_at(
    vars(-c(starts_with("llave"),r_def,c_res)),
    as.numeric
  ) %>% 
  filter(
    as.character(r_def)=="00" & (as.character(c_res)=="1" | as.character(c_res)=="3")
  ) 

d_coe1 <- read.dbf(
  paste_inp(
    paste0("20", year_vector[i], "trim", trim_vector[x], "_dbf/COE1T", trim_vector[x], year_vector[i], ".dbf")
  ), as.is = T
) %>% 
  rename_all(tolower) %>% 
  mutate(
    llave = paste0(
      cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
    )
  )%>% 
  mutate_at(
    vars(-starts_with("llave")),
    as.numeric
  ) 

d_coe2 <- read.dbf(
  paste_inp(
    paste0("20", year_vector[i], "trim", trim_vector[x], "_dbf/COE2T", trim_vector[x], year_vector[i], ".dbf")
  ), as.is = T
) %>% 
  rename_all(tolower) %>% 
  mutate(
    llave = paste0(
      cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
    )
  ) %>% 
  mutate_at(
    vars(-starts_with("llave")),
    as.numeric
  ) 


tempo <- d_sdem %>%
  # Quitar variables que se repiten en COEs
  select(-c(cd_a, con, upm, d_sem, n_pro_viv, v_sel,
            n_hog, h_mud, n_ent, per, n_ren, eda, ur)) %>% 
  left_join(d_coe1 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  left_join(d_coe2 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  # Quitar variables repetidas en COE2
  select(-ends_with(".y")) %>% 
  # Eliminar sufijo ".x" de variables
  rename_at(
    vars(ends_with(".x")),
    ~str_remove_all(., ".x")
  ) 

ingreso_imputado_tempo <- tempo %>% 
  # Filtrar personas ocupadas *con pago*
  filter(pos_ocu < 4, clase1 == 1 & clase2 == 1) %>% 
  mutate(
    ingreso_sm = case_when(
      (is.na(p6b2) & (p6_9==9 | p6a3==3)) ~ NA_real_,
      is.na(p6b2) & p6c == 1 ~ salario*0.5,
      is.na(p6b2) & p6c == 2 ~ salario*1,
      is.na(p6b2) & p6c == 3 ~ salario*1.5,
      is.na(p6b2) & p6c == 4 ~ salario*2.5,
      is.na(p6b2) & p6c == 5 ~ salario*4,
      is.na(p6b2) & p6c == 6 ~ salario*7.5,
      is.na(p6b2) & p6c == 7 ~ salario*10,
      T ~ p6b2
    ),
    ingreso = ifelse(ingocup==0,NA,ingocup)
  )

na_ingreso <- prop.table(table(is.na(ingreso_imputado_tempo$ingreso)))
na_ingreso_sm <- prop.table(table(is.na(ingreso_imputado_tempo$ingreso_sm)))

ingreso_imputado_tempo <- tempo %>% 
  # Filtrar personas ocupadas *con pago*
  filter(pos_ocu < 4, clase1 == 1 & clase2 == 1) %>% 
  mutate(
    ingreso_sm = case_when(
      (is.na(p6b2) & (p6_9==9 | p6a3==3)) ~ NA_real_,
      is.na(p6b2) & p6c == 1 ~ salario*0.5,
      is.na(p6b2) & p6c == 2 ~ salario*1,
      is.na(p6b2) & p6c == 3 ~ salario*1.5,
      is.na(p6b2) & p6c == 4 ~ salario*2.5,
      is.na(p6b2) & p6c == 5 ~ salario*4,
      is.na(p6b2) & p6c == 6 ~ salario*7.5,
      is.na(p6b2) & p6c == 7 ~ salario*10,
      T ~ p6b2
    ),
    ingreso = ifelse(ingocup==0,NA,ingocup),
    anios_esc = ifelse(anios_esc==99,NA,anios_esc),
    tipo_formalidad = case_when(
      emp_ppal == 1 ~ "Empleo informal",
      emp_ppal == 2 ~ "Empleo formal",
      T ~ NA_character_
    ),
    jornada_completa = case_when(
      dur_est == 4 | dur_est == 5 ~ "Jornada de tiempo completo",
      dur_est <=3 ~ "Jornada de medio tiempo",
      T ~ NA_character_
    ),
    cve_ent = str_pad(ent, 2, "l", "0"),
    # La llave especial permite hacer seguimiento de la muestra panel
    llave_especial = paste0(cd_a, cve_ent, con, v_sel, n_hog, h_mud, n_ren)
  ) %>% 
  select(llaveh, llave, llave_especial, pos_ocu, starts_with("clase"), ingreso, ingreso_sm, sex, eda19c, cd_a, anios_esc, t_loc, ing7c,cve_ent, factor = fac) %>% 
  VIM::hotdeck(variable = c("ingreso","ingreso_sm"), domain_var = c("sex", "eda19c", "cd_a", "anios_esc", "t_loc", "ing7c")) 

na_ingreso
prop.table(table(is.na(ingreso_imputado_tempo$ingreso)))

na_ingreso_sm
prop.table(table(is.na(ingreso_imputado_tempo$ingreso_sm)))

ingreso_per_cápita_2020 <- tempo %>% 
  mutate(tamh = 1) %>% 
  group_by(llaveh) %>% 
  mutate(tamh = sum(tamh)) %>% 
  ungroup() %>% 
  distinct(llaveh, .keep_all = T) %>% 
  left_join(
    ingreso_imputado_tempo %>% 
      select(starts_with("llave"), cve_ent, starts_with("ingre"), factor) %>% 
      group_by(llaveh) %>% 
      mutate(
        ingreso_h = sum(ingreso, na.rm=T),
        ingreso_sm_h = sum(ingreso_sm, na.rm=T),
        factorh = mean(factor, na.rm = T)
      ) %>% 
      select(-c(factor, llave)) %>% 
      distinct(llaveh, .keep_all = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    factorp = factorh*tamh,
    ingreso_per_capita = ingreso_h/tamh,
    ingreso_sm_per_capita = ingreso_sm_h/tamh,
    periodo = paste0("20",year_vector[i],"T",trim_vector[x]),
    año = paste0("20", year_vector[i]),
    trimestre = trim_vector[x]
  ) %>% 
  select(periodo, año, trimestre, starts_with("llave"), starts_with("ingre"), tamh, pos_ocu, starts_with("clase"), sex, eda19c, cd_a, anios_esc, t_loc, ing7c, cve_ent, factorh,factorp, factor = fac)

# 1.2 Imputación para 2005T1 - 2020T1  ----

ingreso_per_cápita <- data.frame()
for(i in 1:15){
  ingreso_per_cápita <- data.frame()
  for(x in 1:4){
    
    print(
      paste0(
        "Proceso para año '20", 
        year_vector[i], "', T0",
        trim_vector[x]
      )
    )
    
    d_coe1 <- read.dbf(
      paste_inp(
        paste0("20", year_vector[i], "trim", trim_vector[x], "_dbf/COE1T", trim_vector[x], year_vector[i], ".dbf")
      ), as.is = T
    ) %>% 
      rename_all(tolower) %>% 
      mutate(
        llave = paste0(
          cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
        )
      )%>% 
      mutate_at(
        vars(-starts_with("llave")),
        as.numeric
      ) 
    
    d_coe2 <- read.dbf(
      paste_inp(
        paste0("20", year_vector[i], "trim", trim_vector[x], "_dbf/COE2T", trim_vector[x], year_vector[i], ".dbf")
      ), as.is = T
    ) %>% 
      rename_all(tolower) %>% 
      mutate(
        llave = paste0(
          cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
        )
      ) %>% 
      mutate_at(
        vars(-starts_with("llave")),
        as.numeric
      ) 
    
    d_sdem <- read.dbf(
      paste_inp(
        paste0("20", year_vector[i], "trim", trim_vector[x], "_dbf/SDEMT", trim_vector[x], year_vector[i], ".dbf")
      ), as.is = T
    ) %>% 
      rename_all(tolower) %>% 
      mutate(
        llaveh = paste0(
          cd_a, ent, con, v_sel, n_hog, h_mud
        ),
        llave = paste0(
          cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
        )
      ) %>% 
      mutate_at(
        vars(-c(starts_with("llave"),r_def,c_res)),
        as.numeric
      ) %>% 
      filter(
        as.character(r_def)=="00" & (as.character(c_res)=="1" | as.character(c_res)=="3")
      ) 
    
    tempo <- d_sdem %>%
      select(-c(cd_a, con, upm, d_sem, n_pro_viv, v_sel,
                n_hog, h_mud, n_ent, per, n_ren, eda, ur)) %>% 
      left_join(d_coe1 %>% select(-c(fac, ent)), by = c("llave")) %>% 
      left_join(d_coe2 %>% select(-c(fac, ent)), by = c("llave")) %>% 
      select(-ends_with(".y")) %>% 
      rename_at(
        vars(ends_with(".x")),
        ~str_remove_all(., ".x")
      ) 
    
    ingreso_imputado_tempo <- tempo %>% 
      # Filtrar personas ocupadas *con pago*
      filter(pos_ocu < 4, clase1 == 1 & clase2 == 1) %>% 
      mutate(
        ingreso_sm = case_when(
          (is.na(p6b2) & (p6_9==9 | p6a3==3)) ~ NA_real_,
          is.na(p6b2) & p6c == 1 ~ salario*0.5,
          is.na(p6b2) & p6c == 2 ~ salario*1,
          is.na(p6b2) & p6c == 3 ~ salario*1.5,
          is.na(p6b2) & p6c == 4 ~ salario*2.5,
          is.na(p6b2) & p6c == 5 ~ salario*4,
          is.na(p6b2) & p6c == 6 ~ salario*7.5,
          is.na(p6b2) & p6c == 7 ~ salario*10,
          T ~ p6b2
        ),
        ingreso = ifelse(ingocup==0,NA,ingocup),
        anios_esc = ifelse(anios_esc==99,NA,anios_esc),
        tipo_formalidad = case_when(
          emp_ppal == 1 ~ "Empleo informal",
          emp_ppal == 2 ~ "Empleo formal",
          T ~ NA_character_
        ),
        jornada_completa = case_when(
          dur_est == 4 | dur_est == 5 ~ "Jornada de tiempo completo",
          dur_est <=3 ~ "Jornada de medio tiempo",
          T ~ NA_character_
        ),
        cve_ent = str_pad(ent, 2, "l", "0"),
        llave_especial = paste0(cd_a, cve_ent, con, v_sel, n_hog, h_mud, n_ren)
      ) %>% 
      select(llaveh, llave, llave_especial, pos_ocu, starts_with("clase"), ingreso, ingreso_sm, sex, eda19c, cd_a, anios_esc, t_loc, ing7c,cve_ent, factor = fac) %>% 
      VIM::hotdeck(variable = c("ingreso","ingreso_sm"), domain_var = c("sex", "eda19c", "cd_a", "anios_esc", "t_loc", "ing7c")) 
    
    ingreso_per_cápita_tempo <- tempo %>% 
      mutate(tamh = 1) %>% 
      group_by(llaveh) %>% 
      mutate(tamh = sum(tamh)) %>% 
      ungroup() %>% 
      distinct(llaveh, .keep_all = T) %>% 
      left_join(
        ingreso_imputado_tempo %>% 
          select(starts_with("llave"), cve_ent, starts_with("ingre"), factor) %>% 
          group_by(llaveh) %>% 
          mutate(
            ingreso_h = sum(ingreso, na.rm=T),
            ingreso_sm_h = sum(ingreso_sm, na.rm=T),
            factorh = mean(factor, na.rm = T)
          ) %>% 
          select(-c(factor, llave)) %>% 
          distinct(llaveh, .keep_all = T)
      ) %>% 
      ungroup() %>% 
      mutate(
        factorp = factorh*tamh,
        ingreso_per_capita = ingreso_h/tamh,
        ingreso_sm_per_capita = ingreso_sm_h/tamh,
        periodo = paste0("20",year_vector[i],"T",trim_vector[x]),
        año = paste0("20", year_vector[i]),
        trimestre = trim_vector[x]
      ) %>% 
      select(periodo, año, trimestre, starts_with("llave"), starts_with("ingre"), tamh, pos_ocu, starts_with("clase"), sex, eda19c, cd_a, anios_esc, t_loc, ing7c, cve_ent, factorh,factorp, factor = fac)
    
    
    ingreso_per_cápita <- bind_rows(ingreso_per_cápita, ingreso_per_cápita_tempo)
    
  }
  # Aquí va a guardar para cada año en ruta:
  # "mcv/enoe/01_datos/00_ingresos_imputados/year_vector[i], "T", trim_vector[x].rds"
  saveRDS(ingreso_per_cápita, paste_inp(paste0("00_ingresos_imputados/20", year_vector[i],"_ingresos_imputados_per_cápita.rds")))
  # Aquí pushover me va a avisar que cada año ya se terminó
  pushover_normal(paste0("20",year_vector[i]," terminado"))
}

# 1.3 Imputación para ENOE_n (a partir de 2020T3) ----
# Cambia estructura de llaves y nombre de variables (se incluyen dos factores de expansión)
for(i in 16){
  for(x in 3:4){
    
    print(
      paste0(
        "Proceso para año '20", 
        year_vector[i], "', T0",
        trim_vector[x]
      )
    )
    
    d_coe1 <- read.dbf(
      paste_inp(
        # ENOE
        # paste0("20", i, "trim", x, "_dbf/COE1T", x, i, ".dbf")
        # ENOE_N
        paste0("enoe_n_2020_trim", trim_vector[x], "_dbf/COE1T", trim_vector[x], "20.dbf")
      ), as.is = T
    ) %>% 
      rename_all(tolower) %>% 
      mutate(
        llave = paste0(
          # cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
          cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, ca, mes_cal
        )
      )%>% 
      mutate_at(
        vars(-starts_with("llave")),
        as.numeric
      ) 
    
    d_coe2 <- read.dbf(
      paste_inp(
        # paste0("20", i, "trim", x, "_dbf/COE2T", x, i, ".dbf")
        paste0("enoe_n_2020_trim", trim_vector[x], "_dbf/COE2T", trim_vector[x], "20.dbf")
      ), as.is = T
    ) %>% 
      rename_all(tolower) %>% 
      mutate(
        llave = paste0(
          # cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
          cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, ca, mes_cal
        )
      ) %>% 
      mutate_at(
        vars(-starts_with("llave")),
        as.numeric
      ) 
    
    d_sdem <- read.dbf(
      paste_inp(
        # paste0("20", i, "trim", x, "_dbf/SDEMT", x, i, ".dbf")
        paste0("enoe_n_2020_trim", trim_vector[x], "_dbf/SDEMT", trim_vector[x], "20.dbf")
      ), as.is = T
    ) %>% 
      rename_all(tolower) %>% 
      mutate(
        llaveh = paste0(
          # cd_a, ent, con, v_sel, n_hog, h_mud
          cd_a, ent, con, v_sel, n_hog, h_mud, tipo, ca, mes_cal
        ),
        llave = paste0(
          # cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
          cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, ca, mes_cal
        )
      ) %>% 
      mutate_at(
        vars(-c(starts_with("llave"),r_def,c_res)),
        as.numeric
      ) %>% 
      filter(
        as.character(r_def)=="00" & (as.character(c_res)=="1" | as.character(c_res)=="3")
      ) 
    
    tempo <- d_sdem %>%
      rename(fac = fac_tri, t_loc = t_loc_tri) %>% 
      select(-c(upm, n_pro_viv, fac_men, per, ur)) %>% 
      # left_join(d_coe1 %>% select(-c(fac, ent)), by = c("llave")) %>% 
      left_join(d_coe1 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>% 
      # left_join(d_coe2 %>% select(-c(fac, ent)), by = c("llave")) %>% 
      left_join(d_coe2 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>% 
      select(-c(ends_with(".y"),ends_with(".x")))
    
    ingreso_imputado_tempo <- tempo %>% 
      # Filtrar personas ocupadas *con pago*
      filter(pos_ocu < 4, clase1 == 1 & clase2 == 1) %>% 
      mutate(
        ingreso_sm = case_when(
          (is.na(p6b2) & (p6_9==9 | p6a3==3)) ~ NA_real_,
          is.na(p6b2) & p6c == 1 ~ salario*0.5,
          is.na(p6b2) & p6c == 2 ~ salario*1,
          is.na(p6b2) & p6c == 3 ~ salario*1.5,
          is.na(p6b2) & p6c == 4 ~ salario*2.5,
          is.na(p6b2) & p6c == 5 ~ salario*4,
          is.na(p6b2) & p6c == 6 ~ salario*7.5,
          is.na(p6b2) & p6c == 7 ~ salario*10,
          T ~ p6b2
        ),
        ingreso = ifelse(ingocup==0,NA,ingocup),
        anios_esc = ifelse(anios_esc==99,NA,anios_esc),
        tipo_formalidad = case_when(
          emp_ppal == 1 ~ "Empleo informal",
          emp_ppal == 2 ~ "Empleo formal",
          T ~ NA_character_
        ),
        jornada_completa = case_when(
          dur_est == 4 | dur_est == 5 ~ "Jornada de tiempo completo",
          dur_est <=3 ~ "Jornada de medio tiempo",
          T ~ NA_character_
        ),
        cve_ent = str_pad(ent, 2, "l", "0"),
        llave_especial = paste0(cd_a, cve_ent, con, v_sel, n_hog, h_mud, n_ren)
      ) %>% 
      select(llaveh, llave, llave_especial, pos_ocu, starts_with("clase"), ingreso, ingreso_sm, sex, eda19c, cd_a, anios_esc, t_loc, ing7c,cve_ent, factor = fac) %>% 
      VIM::hotdeck(variable = c("ingreso","ingreso_sm"), domain_var = c("sex", "eda19c", "cd_a", "anios_esc", "t_loc", "ing7c")) 
    
    ingreso_per_cápita_tempo <- tempo %>% 
      mutate(tamh = 1) %>% 
      group_by(llaveh) %>% 
      mutate(tamh = sum(tamh)) %>% 
      ungroup() %>% 
      distinct(llaveh, .keep_all = T) %>% 
      left_join(
        ingreso_imputado_tempo %>% 
          select(starts_with("llave"), cve_ent, starts_with("ingre"), factor) %>% 
          group_by(llaveh) %>% 
          mutate(
            ingreso_h = sum(ingreso, na.rm=T),
            ingreso_sm_h = sum(ingreso_sm, na.rm=T),
            factorh = mean(factor, na.rm = T)
          ) %>% 
          select(-c(factor, llave)) %>% 
          distinct(llaveh, .keep_all = T)
      ) %>% 
      ungroup() %>% 
      mutate(
        factorp = factorh*tamh,
        ingreso_per_capita = ingreso_h/tamh,
        ingreso_sm_per_capita = ingreso_sm_h/tamh,
        periodo = paste0("20",year_vector[i],"T",trim_vector[x]),
        año = paste0("20", year_vector[i]),
        trimestre = trim_vector[x]
      ) %>% 
      select(periodo, año, trimestre, starts_with("llave"), starts_with("ingre"), tamh, pos_ocu, starts_with("clase"), sex, eda19c, cd_a, anios_esc, t_loc, ing7c, cve_ent, factorh,factorp, factor = fac)
    
    
    # Aquí está la base que guardamos para 2020T1
    ingreso_per_cápita_2020 <- bind_rows(ingreso_per_cápita_2020, ingreso_per_cápita_tempo)
    
  }
}

saveRDS(
  ingreso_per_cápita_2020,
  paste_inp(paste0("00_ingresos_imputados/20", year_vector[i],"_ingresos_imputados_per_cápita.rds"))
)

# 2. Estimación de variación en ingresos per cápita por cincuentil de ingreso ----
ingresos_per_capita <- readRDS(paste_inp(paste0("00_ingresos_imputados/2020_ingresos_imputados_per_cápita.rds")))%>% 
  distinct(llaveh, .keep_all = T)

# 2.1 Función para encontrar rangos de cincuentiles por periodo ----
cincuentiles_hog <-  ingreso_per_cápita_2020 %>% 
  filter(!ingreso_sm_per_capita==0) %>% 
  drop_na(ingreso_sm_per_capita) %>% 
  as_survey_design(weights = factorp) %>% 
  group_by(periodo) %>% 
  summarise(
    ingreso_sm_per_capita = survey_quantile(ingreso_sm_per_capita, seq(0.02,1,0.02), na.rm = T)
  ) %>% 
  select(-ends_with("_se")) %>% 
  pivot_longer(
    starts_with("ingreso"),
    names_to = "cincuentil",
    values_to = "ingreso",
    names_prefix = "ingreso_sm_per_capita_q"
  ) %>% 
  mutate(
    cincuentil = as.character(as.numeric(cincuentil)/2)
  ) 

assign_cincuentil_hog <- function(periodo_func, ingreso_per_capita_func){
  
  cincuentiles_hog %>% 
    filter(periodo == periodo_func) %>% 
    distinct(ingreso, .keep_all = T) %>% 
    mutate(dist = (ingreso-ingreso_per_capita_func)**2, 
           min = dist == min(dist)) %>% 
    filter(min) %>% 
    mutate(
      cincuentil = ifelse(ingreso_per_capita_func>ingreso, as.numeric(cincuentil)+1, as.numeric(cincuentil))
    ) %>% 
    distinct(cincuentil) %>% 
    unlist() %>% 
    min()
  
}

assign_cincuentil_hog <- Vectorize(assign_cincuentil_hog)

# NO HACER EN TALLER
ingresos_per_capita_cincuentil_sm <- ingreso_per_cápita_2020 %>% 
  filter(!ingreso_sm_per_capita==0) %>% 
  mutate(
    cincuentil_hog = ifelse(
      is.na(ingreso_sm_per_capita), NA,
      assign_cincuentil_hog(periodo_func = periodo, ingreso_per_capita_func = ingreso_sm_per_capita)
    )
  )

saveRDS(ingresos_per_capita_cincuentil_sm, paste_inp("00_ingresos_imputados/ENOE_2020_ING_PER_CAP_CINCUENTIL.rds"))
ingresos_per_capita_cincuentil_sm <- readRDS(paste_inp("00_ingresos_imputados/ENOE_2020_ING_PER_CAP_CINCUENTIL.rds"))

# 2.2 Estimar ingreso per cápita promedio por cincuentil ----
tuti <- ingresos_per_capita_cincuentil_sm %>% 
  drop_na(ingreso_sm_per_capita) %>% 
  as_survey_design(weights = factorp) %>% 
  group_by(periodo, cincuentil_hog) %>% 
  summarise(
    ingreso_per_capita_sm = survey_mean(ingreso_sm_per_capita, na.rm = T)
  ) %>% 
  select(-ends_with("se")) %>% 
  pivot_wider(
    names_from = periodo, 
    values_from = ingreso_per_capita_sm
  ) %>% 
  mutate(
    dif_T1T3 = (`2020T3`-`2020T1`)/`2020T1`,
    dif_T1T4 = (`2020T4`-`2020T1`)/`2020T1`
  )

ggplot(
  tuti %>% 
    pivot_longer(
      starts_with("dif"),
      names_to = "periodo",
      values_to = "dif"
    ) %>% 
    mutate(periodo = ifelse(str_detect(periodo,"T3"),"2020T3","2020T4")),
  aes(
    x = as.numeric(cincuentil_hog),
    y = dif,
    group = periodo,
    col = periodo
  )
) + 
  geom_line(size = 3) + 
  geom_point(size = 5) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Trimestre de comparación", values = c(mcv_discrete[1],mcv_discrete[2]))+
  labs(title= "Variación porcentual en ingresos laborales per cápita en 2020",
       subtitle = "Comparación de 2020T1 por cincuentiles de ingresos laborales",
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Trebuchet MS"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        legend.position = "bottom",
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.key.size = unit(4,"line"))
ggsave(filename = paste_out("01_ingreso_per_cap_cincuentiles.png"), width = 17, height = 12, dpi = 100)

# Dependiendo del tiempo, un análisis sencillo pero muy lindo 
# 3. Mapa de variación porcentual en población ocupada ----
# Geofacet para México, incluye nacional
mx_grid <- data.frame(
  code = c(2L, 8L, 26L, 3L, 5L, 10L, 19L, 25L, 28L, 1L, 18L, 24L, 32L, 
           11L, 13L, 14L, 22L, 30L, 6L, 15L, 29L, 9L, 17L, 21L, 31L, 4L, 
           12L, 16L, 20L, 23L, 27L, 7L, 0L),
  name = c("Baja California", "Chihuahua", "Sonora", "Baja California Sur", "Coahuila", "Durango", "Nuevo León", "Sinaloa", "Tamaulipas", "Aguascalientes", "Nayarit", "San Luis Potosí", "Zacatecas", "Guanajuato", "Hidalgo", "Jalisco", "Querétaro", "Veracruz", "Colima", "México", "Tlaxcala", "Ciudad de México", "Morelos", "Puebla", "Yucatán", "Campeche", "Guerrero", "Michoacán", "Oaxaca", "Quintana Roo", "Tabasco", "Chiapas", "Nacional"),
  name_official = c("Baja California", "Chihuahua", "Sonora", "Baja California Sur", "Coahuila de Zaragoza", "Durango", "Nuevo León", "Sinaloa", "Tamaulipas", "Aguascalientes", "Nayarit", "San Luis Potosí", "Zacatecas", "Guanajuato", "Hidalgo", "Jalisco", "Querétaro", "Veracruz de Ignacio de la Llave", "Colima", "México", "Tlaxcala", "Ciudad de México", "Morelos", "Puebla", "Yucatán", "Campeche", "Guerrero", "Michoacán de Ocampo", "Oaxaca", "Quintana Roo", "Tabasco", "Chiapas", "Nacional"),
  name_abbr = c("BC", "CHIH", "SON", "BCS", "COAH", "DGO", "NL", "SIN", "TAM", "AGS", "NAY", "SLP", "ZAC", "GTO", "HGO", "JAL", "QRO", "VER", "COL", "MEX", "TLAX", "CDMX", "MOR", "PUE", "YUC", "CAMP", "GRO", "MICH", "OAX", "QROO", "TAB", "CHPS", "Nacional"),
  name_abbr_iso = c("BCN", "CHH", "SON", "BCS", "COA", "DUR", "NLE", "SIN", "TAM", "AGU", "NAY", "SLP", "ZAC", "GUA", "HID", "JAL", "QUE", "VER", "COL", "MEX", "TLA", "CMX", "MOR", "PUE", "YUC", "CAM", "GRO", "MIC", "OAX", "ROO", "TAB", "CHP", "Nacional"),
  name_abbr_official = c("BC", "Chih.", "Son.", "BCS", "Coah.", "Dgo.", "NL", "Sin.", "Tamps.", "Ags.", "Nay.", "SLP", "Zac.", "Gto.", "Hgo.", "Jal.", "Qro.", "Ver.", "Col.", "Mex.", "Tlax.", "CDMX", "Mor.", "Pue.", "Yuc.", "Camp.", "Gro.", "Mich.", "Oax.", "Q. Roo", "Tab.", "Chis.", "Nacional"),
  col = c(1, 3, 2, 1, 4, 3, 5, 2, 6, 5, 3, 6, 4, 4, 6, 3, 5, 7, 4, 5, 6, 5, 4, 6, 9, 8, 5, 4, 6, 9, 7, 7,8),
  row = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 6, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8,3)
)
geofacet::grid_preview(mx_grid) + 
  coord_map()


# 3.1 PEA ----
fiuffi <- "Fuente: elaboración de @MexicoComoVamos con información del @INEGI_INFORMA | @guzmart_"

# 3.1 General ----
general <- readxl::read_excel(paste_inp("01_general_2005_2020.xlsx")) %>% 
  glimpse()
tipo <- c("Ocupada", "Desocupada", "Disponibles", "No disponibles")
x = 1

a <- general %>% 
  filter(
    str_detect(tipo_ocu, tipo[x]), 
    año == "2020"
  ) %>% 
  select(cve_ent, ent_abr, periodo, tipo_ocu, tot_ocu) %>% 
  mutate(
    periodo = str_remove_all(periodo, "2020"), 
    cve_ent = as.numeric(cve_ent)
  )%>% 
  pivot_wider(
    names_from = periodo,
    values_from = tot_ocu
  ) %>% 
  mutate(
    T1_T3 = (T3-T1)/T1,
    T1_T4 = (T4-T1)/T1,
    T3_T4 = (T4-T3)/T3
  ) %>% 
  select(-c(T1,T3,T4)) %>% 
  pivot_longer(
    T1_T3:T3_T4,
    names_to = "var_trim",
    values_to = "var"
  ) %>% 
  mutate(
    col_var = ifelse(var <= 0, "0", "1"),
    var_trim = str_replace_all(var_trim, "_", " vs. ")
  )

fiuf <- paste0("Variación porcentual en Población ", tipo[x])
ggplot(
  a,
  aes(
    x = var_trim,
    y = var,
    fill = col_var,
    label = paste0(round(var*100,1),"%")
  )
) +
  geom_col() +
  geom_text(vjust = a$var > 0, fontface = "bold") +
  geofacet::facet_geo(~ cve_ent, grid = mx_grid, label = "name_abbr") +
  coord_cartesian() +
  scale_y_continuous("", labels = scales::percent) +
  scale_fill_manual("", values = c(mcv_semaforo[2], mcv_semaforo[1])) +
  labs(title= fiuf,
       subtitle = "Comparación entre trimestres de 2020",
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 25),
        strip.text.x = element_text(size = 15),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Trebuchet MS"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")

for(x in 1:4){
  
  a <- general %>% 
    filter(
      str_detect(tipo_ocu, tipo[x]), 
      año == "2020"
    ) %>% 
    select(cve_ent, ent_abr, periodo, tipo_ocu, tot_ocu) %>% 
    mutate(
      periodo = str_remove_all(periodo, "2020"), 
      cve_ent = as.numeric(cve_ent)
    )%>% 
    pivot_wider(
      names_from = periodo,
      values_from = tot_ocu
    ) %>% 
    mutate(
      T1_T3 = (T3-T1)/T1,
      T1_T4 = (T4-T1)/T1,
      T3_T4 = (T4-T3)/T3
    ) %>% 
    select(-c(T1,T3,T4)) %>% 
    pivot_longer(
      T1_T3:T3_T4,
      names_to = "var_trim",
      values_to = "var"
    ) %>% 
    mutate(
      col_var = ifelse(var <= 0, "0", "1"),
      var_trim = str_replace_all(var_trim, "_", " vs. ")
    )
  
  fiuf <- paste0("Variación porcentual en Población ", tipo[x])
  ggplot(
    a,
    aes(
      x = var_trim,
      y = var,
      fill = col_var,
      label = paste0(round(var*100,1),"%")
    )
  ) +
    geom_col() +
    geom_text(vjust = a$var > 0, fontface = "bold") +
    geofacet::facet_geo(~ cve_ent, grid = mx_grid, label = "name_abbr") +
    coord_cartesian() +
    scale_y_continuous("", labels = scales::percent) +
    scale_fill_manual("", values = c(mcv_semaforo[2], mcv_semaforo[1])) +
    labs(title= fiuf,
         subtitle = "Comparación entre trimestres de 2020",
         caption = fiuffi) +
    theme_minimal() +
    theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
          plot.subtitle = element_text(size = 30, hjust = 0.5),
          plot.caption = element_text(size = 25),
          strip.text.x = element_text(size = 15),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Trebuchet MS"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "none")
  ggsave(filename = paste_out(paste0("0",x+1,"_",tolower(tipo[x]),".png")),
         width = 20, height = 14, dpi = 100)
  
  
  
}

