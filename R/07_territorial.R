# 07_territorial.R — Indicadores por local de votación, La Reina 2024–2025 (SERVEL).
# Requiere haber corrido los scripts 01 y 02. Salida: output/tablas/07_territorial_locales_2024.csv
suppressMessages({library(dplyr); library(tidyr)})
mb <- readRDS("data/processed/lr_votos_mesa_bloques.rds")
mv <- readRDS("data/processed/lr_votos_mesa.rds")

con <- mb %>% filter(id=="2024_concejales") %>% group_by(local) %>%
  summarise(emit=sum(votos), mesas=n_distinct(mesa),
    der=sum(votos[familia=="Derecha"],na.rm=T), ult=sum(votos[familia=="Ultraderecha"],na.rm=T),
    izq=sum(votos[familia=="Izquierda"],na.rm=T), ci=sum(votos[familia=="Centroizquierda"],na.rm=T),
    cen=sum(votos[familia=="Centro"],na.rm=T),
    huer=sum(votos[codigo %in% c("AH","PH","PAVP")],na.rm=T),
    nb=sum(votos[tipo_voto!="valido"])) %>%
  mutate(across(der:huer, ~round(100*.x/(emit-nb),1)), nb=round(100*nb/emit,1))
alc <- mv %>% filter(id=="2024_alcalde") %>% group_by(local) %>%
  summarise(e=sum(votos), v=sum(votos[tipo_voto=="valido"]),
   pal=round(100*sum(votos[grepl("PALACIOS",candidato)])/v,1),
   nb_alc=round(100*sum(votos[tipo_voto!="valido"])/e,1))
f2 <- function(i, pat, nm){ mv %>% filter(id==i, tipo_voto=="valido") %>% group_by(local) %>%
  summarise(!!nm := round(100*sum(votos[grepl(pat,candidato)])/sum(votos),1))}
# En 2025 el SERVEL renombró o reorganizó tres locales respecto de 2024:
# "ESCUELA YANGTSE" (antes "ESCUELA BASICA YANGTSE"); la Sección Media del Complejo
# Educacional funcionó como un solo local (en 2024, L1 y L2), y el Colegio Sagrado Corazón
# no fue local de votación (sin dato 2025). Se homologan los nombres para unir con 2024.
armoniza25 <- function(df){
  df <- df %>% mutate(local = recode(local, "ESCUELA YANGTSE" = "ESCUELA BASICA YANGTSE"))
  media <- df %>% filter(local == "COMPLEJO EDUCACIONAL LA REINA SECCION MEDIA")
  bind_rows(df %>% filter(local != "COMPLEJO EDUCACIONAL LA REINA SECCION MEDIA"),
            media %>% mutate(local = "COMPLEJO EDUCACIONAL LA REINA SECCION MEDIA L1"),
            media %>% mutate(local = "COMPLEJO EDUCACIONAL LA REINA SECCION MEDIA L2"))
}
gore <- f2("2024_gore_2v","CLAUDIO ORREGO","c_orrego")
jara <- f2("2025_pres_2v","JARA","jara2v") %>% armoniza25()
dip <- mb %>% filter(id=="2025_diputados", tipo_voto=="valido") %>% group_by(local) %>%
  summarise(izq25=round(100*sum(votos[familia=="Izquierda"],na.rm=T)/sum(votos),1),
            ult25=round(100*sum(votos[familia=="Ultraderecha"],na.rm=T)/sum(votos),1)) %>% armoniza25()
t <- con %>% left_join(alc %>% select(local,pal,nb_alc)) %>% left_join(gore) %>% left_join(jara) %>% left_join(dip) %>%
  mutate(opos=izq+ci+cen, brecha_pal=round(pal-(der+ult),1), dif_nb=nb-nb_alc) %>% arrange(desc(opos))
options(width=250); print(as.data.frame(t %>% select(local,mesas,emit,der,ult,izq,ci,cen,opos,huer,nb,nb_alc,pal,brecha_pal,c_orrego,jara2v,izq25,ult25)))
write.csv2(t, "output/tablas/07_territorial_locales_2024.csv", row.names=FALSE)
print(cor(t$opos, t$c_orrego)); print(cor(t$opos,t$brecha_pal)); print(cor(t$nb,t$opos))

# Agregación por sector según la unidad vecinal (UV) de cada local
# (límites de UV: lareina.cl/conocetuunidad; sectores validados por el autor).
t <- read.csv2(file.path("output", "tablas", "07_territorial_locales_2024.csv"))
sec <- c("COLEGIO SAN CONSTANTINO"="Villa La Reina (UV 13-15)","COMPLEJO EDUCACIONAL LA REINA SECCION BASICA"="Villa La Reina (UV 13-15)",
"COMPLEJO EDUCACIONAL LA REINA SECCION MEDIA L1"="Villa La Reina (UV 13-15)","COMPLEJO EDUCACIONAL LA REINA SECCION MEDIA L2"="Villa La Reina (UV 13-15)",
"COLEGIO SAGRADO CORAZON DE JESUS DE LA REINA"="Villa La Reina (UV 13-15)",
"COLEGIO JOSE ARRIETA"="Borde sur con Peñalolén (UV 6-8-10)","COLEGIO CONFEDERACION SUIZA"="Borde sur con Peñalolén (UV 6-8-10)",
"COLEGIO SANTA CATALINA LABOURE"="Borde sur con Peñalolén (UV 6-8-10)","ESCUELA PALESTINA"="Borde sur con Peñalolén (UV 6-8-10)",
"COLEGIO DE LA SALLE"="Poniente, borde Ñuñoa (UV 4)","COLEGIO TERESIANO ENRIQUE DE OSSO L1"="Poniente, borde Ñuñoa (UV 4)","COLEGIO TERESIANO ENRIQUE DE OSSO L2"="Poniente, borde Ñuñoa (UV 4)",
"COLEGIO SANTA MARIA REINA"="Centro (UV 11)","ESCUELA BASICA YANGTSE"="Centro (UV 11)",
"LICEO EUGENIO MARIA DE HOSTOS"="Centro-norte (UV 12)","COLEGIO ANDREE ENGLISH SCHOOL"="Centro-norte (UV 12)",
"ESCUELA SAN LEONARDO MURIALDO"="Reina Alta (UV 14)")
t$sector <- sec[t$local]; stopifnot(!any(is.na(t$sector)))
t <- t %>% mutate(val=emit*(1-nb/100))
s <- t %>% group_by(sector) %>% summarise(locales=n(), mesas=sum(mesas),
  opos=round(weighted.mean(opos,val),1), der_total=round(weighted.mean(der+ult,val),1),
  ult=round(weighted.mean(ult,val),1), huer=round(weighted.mean(huer,val),1),
  nb=round(weighted.mean(nb,emit),1), brecha=round(weighted.mean(brecha_pal,val),1),
  c_orrego=round(weighted.mean(c_orrego,val),1), emitidos=sum(emit)) %>% arrange(desc(opos))
print(as.data.frame(s))
write.csv2(s, file.path("output", "tablas", "07_territorial_sectores_2024.csv"), row.names = FALSE)
