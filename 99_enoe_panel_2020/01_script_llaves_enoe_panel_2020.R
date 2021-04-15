# Seguimiento al panel ----
Sys.setlocale("LC_TIME", "es_ES")

# Paquetes ----
if(!require("fastDummies")) install.packages("fastDummies") & require("fastDummies")
if(!require("ggalluvial")) install.packages("ggalluvial") & require("ggalluvial")
if(!require("hot.deck")) install.packages("hot.deck") & require("hot.deck")
if(!require("gganimate")) install.packages("gganimate") & require("gganimate")
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
if(!require("srvyr")) install.packages("srvyr") & require("srvyr")

# Tidyverse <3
require(tidyverse)



# Desactiva notación científica
options(scipen=999)

# Colores MCV -----
mcv_discrete <- c(
  "#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41"
)

mcv_semaforo <- c("#00b783", "#ffbd41", "#ff6260")

mcv_blacks <- c("black", "#D2D0CD", "#777777")

# Directorios ----
paste_inp <- function(x){paste0("mcv/enoe/01_datos/", x)}
paste_out <- function(x){paste0("mcv/mercado_laboral/03_productos/", x)}
fiuffi <- "Fuente: elaboración de @MexicoComoVamos con información del @INEGI_INFORMA | @guzmart_\nNo se incluyeron a trabajadores sin pago. Se utilizó el método de imputación hotdeck para tratar la no respuesta."


# 2. ENOE 2020 ----
# 2.1 ENOE 2020T1 ----
trim_vector <- c("1","2","3","4")
x = 1

d_coe1 <- read.dbf(
  paste_inp(
    paste0("2020trim", trim_vector[x], "_dbf/COE1T", trim_vector[x], "20.dbf")
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
    paste0("2020trim", trim_vector[x], "_dbf/COE2T", trim_vector[x], "20.dbf")
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
    paste0("2020trim", trim_vector[x], "_dbf/SDEMT", trim_vector[x], "20.dbf")
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

enoe_2020T1 <- d_sdem %>%
  select(-c(cd_a, con, upm, d_sem, n_pro_viv, v_sel,
            n_hog, h_mud, n_ent, per, n_ren, eda, ur)) %>% 
  left_join(d_coe1 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  left_join(d_coe2 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  select(-ends_with(".y")) %>% 
  rename_at(
    vars(ends_with(".x")),
    ~str_remove_all(., ".x")
  ) %>% 
  filter(!str_starts(d_sem, "1|2")) %>% 
  mutate(
    tipo_ocu = case_when(
      clase2 == 1 ~ "Ocupada",
      clase2 == 2 ~ "Desocupada",
      clase2 == 3 ~ "Disponibles",
      clase2 == 4 ~ "No disponibles",
      T ~ NA_character_
    ),
    cve_ent = str_pad(ent, 2, "l", "0"),
    llave_especial = paste0(cd_a, cve_ent, con, v_sel, n_hog, h_mud, n_ren),
    sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
    p11_h1 = ifelse(p11_h1>97,NA,p11_h1),
    horas_semana_estudio = p11_h1 + (p11_m1/60),
    p11_h2 = ifelse(p11_h2>97,NA,p11_h2),
    horas_semana_cuidados = p11_h2 + (p11_m2/60),
    p11_h3 = ifelse(p11_h3>97,NA,p11_h3),
    horas_semana_cuentas_y_seguridad = p11_h3 + (p11_m3/60),
    p11_h4 = ifelse(p11_h4>97,NA,p11_h4),
    horas_semana_transporte = p11_h4 + (p11_m4/60),
    p11_h5 = ifelse(p11_h5>97,NA,p11_h5),
    horas_semana_construcción = p11_h5 + (p11_m5/60),
    p11_h6 = ifelse(p11_h6>97,NA,p11_h6),
    horas_semana_reparación = p11_h6 + (p11_m6/60),
    p11_h7 = ifelse(p11_h7>97,NA,p11_h7),
    horas_semana_quehaceres = p11_h7 + (p11_m7/60),
    p11_h8 = ifelse(p11_h8>97,NA,p11_h8),
    horas_semana_servicio_comunidad = p11_h8 + (p11_m8/60),
    horas_semana_trabajo = ifelse(hrsocup==0,NA,hrsocup),
    costo_tipo_ausencia = case_when(
      p2e == 1 ~ "Temporalmente ausente",
      p2e == 2 ~ "Pensionado o jubilado de su empleo", 
      p2e == 3 ~ "Estudiante", 
      p2e == 4 ~ "Una persona que se dedica a los quehaceres de su hogar", 
      p2e == 5 ~ "Una persona con alguna limitación física o mental que le impide trabajar por el resto de su vida", 
      p2e == 6 ~ "Otra condición",
      T ~ NA_character_
    ),
    costo_tipo_ausencia_ord = p2e,
    costo_razon_ausencia = case_when(
      p2g2 == 1 ~ "Está esperando la respuesta a una solicitud o está apalabrado con un patrón que lo llamará en fecha próxima",
      p2g2 == 2 ~ "No hay trabajo en su especialidad, oficio o profesión", 
      p2g2 == 3 ~ "No cuenta con la escolaridad, los papeles o la experiencia necesaria para realizar un trabajo", 
      p2g2 == 4 ~ "Piensa que por su edad o por su aspecto no lo aceptarían en un trabajo", 
      p2g2 == 5 ~ "En su localidad no hay trabajo o solo se realiza en ciertas temporadas del año", 
      p2g2 == 6 ~ "La inseguridad pública o el exceso de trámites lo desalientan a iniciar una actividad",
      p2g2 == 7 ~ "Espera recuperarse de una enfermedad o accidente",
      p2g2 == 8 ~ "Está embarazada", 
      p2g2 == 9 ~ "No tiene quién le cuide a sus hijos pequeños, ancianos o enfermos", 
      p2g2 == 10 ~ "No lo(a) deja un familiar", 
      p2g2 == 11 ~ "Otras razones de mercado", 
      p2g2 == 12 ~ "Otras razones personales",
      T ~ NA_character_
    ),
    costo_razon_ausencia_ord = p2g2
  )

# 2.3 ENOE2020T3 ----
x = 3

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

enoe_2020T3 <- d_sdem %>%
  rename(fac = fac_tri, t_loc = t_loc_tri) %>% 
  select(-c(upm, n_pro_viv, fac_men, per, ur)) %>% 
  # left_join(d_coe1 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  left_join(d_coe1 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>% 
  # left_join(d_coe2 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  left_join(d_coe2 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>% 
  select(-c(ends_with(".y"),ends_with(".x"))) %>% 
  filter(tipo == 1) %>% 
  filter(!n_ent == 1, !n_ent == 2) %>% 
  mutate(
    tipo_ocu = case_when(
      clase2 == 1 ~ "Ocupada",
      clase2 == 2 ~ "Desocupada",
      clase2 == 3 ~ "Disponibles",
      clase2 == 4 ~ "No disponibles",
      T ~ NA_character_
    ),
    cve_ent = str_pad(ent, 2, "l", "0"),
    llave_especial = paste0(cd_a, cve_ent, con, v_sel, n_hog, h_mud, n_ren), 
    sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
    p9_h1 = ifelse(p9_h1>97,NA,p9_h1),
    horas_semana_estudio = p9_h1 + (p9_m1/60),
    p9_h2 = ifelse(p9_h2>97,NA,p9_h2),
    horas_semana_cuidados = p9_h2 + (p9_m2/60),
    p9_h3 = ifelse(p9_h3>97,NA,p9_h3),
    horas_semana_cuentas_y_seguridad = p9_h3 + (p9_m3/60),
    p9_h4 = ifelse(p9_h4>97,NA,p9_h4),
    horas_semana_transporte = p9_h4 + (p9_m4/60),
    p9_h5 = ifelse(p9_h5>97,NA,p9_h5),
    horas_semana_construcción = p9_h5 + (p9_m5/60),
    p9_h6 = ifelse(p9_h6>97,NA,p9_h6),
    horas_semana_reparación = p9_h6 + (p9_m6/60),
    p9_h7 = ifelse(p9_h7>97,NA,p9_h7),
    horas_semana_quehaceres = p9_h7 + (p9_m7/60),
    p9_h8 = ifelse(p9_h8>97,NA,p9_h8),
    horas_semana_servicio_comunidad = p9_h8 + (p9_m8/60),
    horas_semana_trabajo = ifelse(hrsocup==0,NA,hrsocup),
    costo_tipo_ausencia = case_when(
      p2e == 1 ~ "Temporalmente ausente",
      p2e == 2 ~ "Pensionado o jubilado de su empleo", 
      p2e == 3 ~ "Estudiante", 
      p2e == 4 ~ "Una persona que se dedica a los quehaceres de su hogar", 
      p2e == 5 ~ "Una persona con alguna limitación física o mental que le impide trabajar por el resto de su vida", 
      p2e == 6 ~ "Otra condición",
      T ~ NA_character_
    ),
    costo_tipo_ausencia_ord = p2e,
    costo_razon_ausencia = case_when(
      p2g2 == 1 ~ "Está esperando la respuesta a una solicitud o está apalabrado con un patrón que lo llamará en fecha próxima",
      p2g2 == 2 ~ "No hay trabajo en su especialidad, oficio o profesión", 
      p2g2 == 3 ~ "No cuenta con la escolaridad, los papeles o la experiencia necesaria para realizar un trabajo", 
      p2g2 == 4 ~ "Piensa que por su edad o por su aspecto no lo aceptarían en un trabajo", 
      p2g2 == 5 ~ "En su localidad no hay trabajo o solo se realiza en ciertas temporadas del año", 
      p2g2 == 6 ~ "La inseguridad pública o el exceso de trámites lo desalientan a iniciar una actividad",
      p2g2 == 7 ~ "Espera recuperarse de una enfermedad o accidente",
      p2g2 == 8 ~ "Está embarazada", 
      p2g2 == 9 ~ "No tiene quién le cuide a sus hijos pequeños, ancianos o enfermos", 
      p2g2 == 10 ~ "No lo(a) deja un familiar", 
      p2g2 == 11 ~ "Otras razones de mercado", 
      p2g2 == 12 ~ "Otras razones personales",
      T ~ NA_character_
    ),
    costo_razon_ausencia_ord = p2g2
  )

# 2.4 ENOE2020T4 ----
x = 4

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

enoe_2020T4 <- d_sdem %>%
  rename(fac = fac_tri, t_loc = t_loc_tri) %>% 
  select(-c(upm, n_pro_viv,fac_men, per, ur)) %>% 
  # left_join(d_coe1 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  left_join(d_coe1 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>% 
  # left_join(d_coe2 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  left_join(d_coe2 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>% 
  select(-c(ends_with(".y"),ends_with(".x"))) %>% 
  filter(tipo == 1) %>% 
  filter(!n_ent == 1, !n_ent == 2) %>% 
  mutate(
    tipo_ocu = case_when(
      clase2 == 1 ~ "Ocupada",
      clase2 == 2 ~ "Desocupada",
      clase2 == 3 ~ "Disponibles",
      clase2 == 4 ~ "No disponibles",
      T ~ NA_character_
    ),
    cve_ent = str_pad(ent, 2, "l", "0"),
    llave_especial = paste0(cd_a, cve_ent, con, v_sel, n_hog, h_mud, n_ren),
    sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
    p9_h1 = ifelse(p9_h1>97,NA,p9_h1),
    horas_semana_estudio = p9_h1 + (p9_m1/60),
    p9_h2 = ifelse(p9_h2>97,NA,p9_h2),
    horas_semana_cuidados = p9_h2 + (p9_m2/60),
    p9_h3 = ifelse(p9_h3>97,NA,p9_h3),
    horas_semana_cuentas_y_seguridad = p9_h3 + (p9_m3/60),
    p9_h4 = ifelse(p9_h4>97,NA,p9_h4),
    horas_semana_transporte = p9_h4 + (p9_m4/60),
    p9_h5 = ifelse(p9_h5>97,NA,p9_h5),
    horas_semana_construcción = p9_h5 + (p9_m5/60),
    p9_h6 = ifelse(p9_h6>97,NA,p9_h6),
    horas_semana_reparación = p9_h6 + (p9_m6/60),
    p9_h7 = ifelse(p9_h7>97,NA,p9_h7),
    horas_semana_quehaceres = p9_h7 + (p9_m7/60),
    p9_h8 = ifelse(p9_h8>97,NA,p9_h8),
    horas_semana_servicio_comunidad = p9_h8 + (p9_m8/60),
    horas_semana_trabajo = ifelse(hrsocup==0,NA,hrsocup),
    costo_tipo_ausencia = case_when(
      p2e == 1 ~ "Temporalmente ausente",
      p2e == 2 ~ "Pensionado o jubilado de su empleo", 
      p2e == 3 ~ "Estudiante", 
      p2e == 4 ~ "Una persona que se dedica a los quehaceres de su hogar", 
      p2e == 5 ~ "Una persona con alguna limitación física o mental que le impide trabajar por el resto de su vida", 
      p2e == 6 ~ "Otra condición",
      T ~ NA_character_
    ),
    costo_tipo_ausencia_ord = p2e,
    costo_razon_ausencia = case_when(
      p2g2 == 1 ~ "Está esperando la respuesta a una solicitud o está apalabrado con un patrón que lo llamará en fecha próxima",
      p2g2 == 2 ~ "No hay trabajo en su especialidad, oficio o profesión", 
      p2g2 == 3 ~ "No cuenta con la escolaridad, los papeles o la experiencia necesaria para realizar un trabajo", 
      p2g2 == 4 ~ "Piensa que por su edad o por su aspecto no lo aceptarían en un trabajo", 
      p2g2 == 5 ~ "En su localidad no hay trabajo o solo se realiza en ciertas temporadas del año", 
      p2g2 == 6 ~ "La inseguridad pública o el exceso de trámites lo desalientan a iniciar una actividad",
      p2g2 == 7 ~ "Espera recuperarse de una enfermedad o accidente",
      p2g2 == 8 ~ "Está embarazada", 
      p2g2 == 9 ~ "No tiene quién le cuide a sus hijos pequeños, ancianos o enfermos", 
      p2g2 == 10 ~ "No lo(a) deja un familiar", 
      p2g2 == 11 ~ "Otras razones de mercado", 
      p2g2 == 12 ~ "Otras razones personales",
      T ~ NA_character_
    ),
    costo_razon_ausencia_ord = p2g2
  )

# 2.5 Llaves para panel ----
llaves <- enoe_2020T1 %>% 
  distinct(llave_especial) %>% 
  mutate(T1 = 1) %>% 
  left_join(
    enoe_2020T3 %>% 
      distinct(llave_especial) %>% 
      mutate(T3 = 1)
  ) %>% 
  left_join(
    enoe_2020T4 %>% 
      distinct(llave_especial) %>% 
      mutate(T4 = 1)
  ) %>% 
  mutate(
    panel_2020 = ifelse(
      T1 == 1 & T3 == 1 & T4 == 1, 1, 0
    ),
    panel_2020T1T3 = ifelse(T1 == 1 & T3 == 1, 1, 0),
    panel_2020T1T3 = ifelse(T1 == 1 & T3 == 1, 1, 0)
  ) %>% 
  filter(
    panel_2020 == 1 | panel_2020T1T3 == 1
  ) %>% 
  select(llave_especial, starts_with("panel")) %>% 
  mutate_at(
    vars(starts_with("panel")),
    ~ifelse(is.na(.),0,.)
  ) %>% 
  glimpse()

openxlsx::write.xlsx(llaves, paste_out("99_llaves_enoe_panel_2020.xlsx"))
