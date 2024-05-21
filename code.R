library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(data.table)
library(xtable)
library(zip)



rm(list=ls())
setwd("~/Scrips R")
### TIEMPO
timing <- system.time({
  ##### MDP ####
  #mdp <- read.table(file = "~/Scrips R/mdpcsv.csv",sep = ",",header = T, encoding = "latin1")
  mdp1 <- read.xlsx(xlsxFile = "MDP.xlsx",sheet = 1)
  mdp2 <- read.xlsx(xlsxFile = "MDP.xlsx",sheet = 2)
  mdp <- rbind(mdp1,mdp2)
  rm(mdp1,mdp2)
  write.xlsx(x = mdp, file = "~/Scrips R/MdpC.xlsx")
  saveRDS(mdp,"~/Scrips R/MdpC.rds")
  
  mdp <- mdp %>%
    rename(
      `CLAVE PLAZA` = CLAVE.DE.LA.PLAZA,
      OPERACIÓN = CÓDIGO.DEL.MOVIMIENTO,
      `FECHA INICIAL` = FECHA.INICIAL,
      `FECHA FINAL` = FECHA.FINAL,
      `FECHA BAJA` = FECHA.DE.BAJA,
      `FECHA DE REGISTRO` = FECHA.DE.REGISTRO,
      ESTATUS = ESTATUS.DEL.NOMBRAMIENTO
    )
  #mdp <- mdp %>% mutate(`FECHA INICIAL` = case_when(substr(x = OPERACIÓN, start = 1, stop = 2) %in% c("06","07","09") ~ ALTA.VIGENTE, TRUE ~ `FECHA INICIAL`))
  #saveRDS(object = mdp, file = "mdp.rds")
  #mdp <- readRDS("mdp.rds")
  mdp <- mdp %>% mutate(f=substr(`FECHA DE REGISTRO`,start = 1,stop = 10),
                        f=as.Date(x = f,format="%d/%m/%Y"),
                        `FECHA INICIAL`= as.Date(x=`FECHA INICIAL`, format="%d/%m/%Y"),
                        `FECHA FINAL`  = as.Date(x=`FECHA FINAL`, format="%d/%m/%Y"),
                        `FECHA BAJA`   = as.Date(x=`FECHA BAJA`, format="%d/%m/%Y"),
                        FECHA_REGISTRO = as.Date(substr(x     = `FECHA DE REGISTRO`,
                                                        start = 1,
                                                        stop  = 10), format = "%d/%m/%Y"),
                        ID=paste(CURP,`CLAVE PLAZA`,sep=""))
  mdp <- mdp %>% arrange(FECHA_REGISTRO)
  mdp <- mdp %>% select(!c(f))
  write.xlsx(x = mdp,file = "outputs/mdp_hoy.xlsx")
  saveRDS(object = mdp, file="mdp.rds")
  saveRDS(object = mdp, file="~/Scrips R/Shiny Nominas/Modelo4/MDP23/mdp.rds")
})
elapsed_time <- timing[["elapsed"]]
cat("Elapsed time:", elapsed_time, "seconds\n")


##### Analítico ####
analitico <- read.xlsx("analitico.xlsx")
analitico <- analitico %>% mutate(Fecha=substr(x = FECHA,start = 1,stop = 10),
                                  Fecha=as.Date(FECHA, format="%Y-%m-%d"),
                                  Estatus=case_when(ESTATUS=="O"~ 1,
                                                    ESTATUS=="V"~ 2,
                                                    TRUE ~ 3))
analitico <- analitico %>% arrange(CODIGO_PLAZA,desc(Fecha),Estatus) %>% 
  mutate(Plaza_ESP = CODIGO_PLAZA)
analitico$Plaza_ESP <- str_replace(analitico$Plaza_ESP,"\\s+","")
analitico$Plaza_ESP <- str_replace(analitico$Plaza_ESP,"[.]","")

saveRDS(object = analitico, file = "analitico.rds")
write.xlsx(x = analitico, file = "Analitico_hoy.xlsx")