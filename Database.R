# GVC Trade and PTA Database 

# [A] Setup ----

pacman::p_load("readxl", "tidyverse", "RDS")

pathkaz = ("~/Documents/ADB/WTO KAZ/")
pathout = paste0(pathkaz, "03 Analysis 2021/")
pathpta = paste0(pathkaz, "02 Data/PTA Database/")
pathcep = paste0(pathkaz, "02 Data/CEPII/")
pathgvc = ("~/Documents/ADB/GVC 2021/Decomposition WWZ/ADB-MRIO-WWZ-Decomposition-")

map.iso = read.csv("~/Documents/ADB/MRIO/MRIO-ISO.csv")
map.country = read_xlsx("~/Documents/ADB/MRIO/ADB-MRIO-Legends.xlsx", sheet = 2)

# [B] PTA Database ----

countries = as.list(map.iso %>% select(ISO3.digit))
pta.raw = read_excel(paste0(pathpta, "01 Horizontal Depth_bilateral.xlsx"))

cols = as.vector(colnames(pta.raw)[-c(1:8)])
wtoplus.prov = paste0(str_subset(str_subset(cols, "wto_plus"), "_le", negate = TRUE), "_le")
wtox.prov = paste0(str_subset(str_subset(cols, "wto_X"), "_le", negate = TRUE), "_le")
depthle.prov = str_subset(cols, "_le")
core.prov = c(wtoplus.prov, 
              "wto_X_competitionpolicy_le", 
              "wto_X_ipr_le", 
              "wto_X_investment_le", 
              "wto_X_movementofcapital_le")

list = list(wtoplus.prov, wtox.prov, core.prov)
names(list) <- c("wtoplus", "wtox", "core")

pta = pta.raw %>% 
        filter(iso1 %in% unlist(countries),
               iso2 %in% unlist(countries))

depth = pta %>% 
        select(one_of(depthle.prov)) %>% 
        mutate(depth = apply(.,1,function(x) sum(x > 0))) %>% 
        select(depth)

for (l in 1:length(list)) {
  x <-  pta %>% select(one_of(list[[l]]))
  #y <- rowSums(x)
  y <- x %>% mutate(z = apply(.,1,function(x) sum(x > 0))) %>% select(z)
  colnames(y) <- names(list)[l]
  vars <- names(list)[l]
  assign(vars,y,envir=globalenv())
}

pta.fin = cbind(pta, depth, core, wtoplus, wtox)

# [C] Export Decomposition: Country ----

wwz.list = list()
years = c(2000, 2007:2020)

for (y in years) {
  file = paste0(pathgvc,y,".xlsx")
  x = read_excel(file, skip = 3)
  z = x %>% filter(Aggregation == "35-sector") %>% 
    select(Economy, Foreign, Sector, Term1:Exports) 
    z$year <- y 
    wwz.list[[y]] = z
}

wwz <- do.call(rbind, wwz.list)

wwz.agg <- wwz %>% select(!Sector) %>% 
           aggregate(. ~ year + Economy + Foreign, ., FUN = sum) %>% 
           mutate(DVA_FIN = Term1,
                  DVA_INT = Term2, 
                  DVA_INTrex = Term3 + Term4 + Term5,
                  RDV = Term6 + Term7 + Term8,
                  FVA_FIN = Term11 + Term14,
                  FVA_INT = Term12 + Term15) %>%
           mutate(DVA = DVA_FIN + DVA_INT + DVA_INTrex + RDV,
                  DVA_inter = DVA_INT + DVA_INTrex + RDV,
                  DVA_interx = DVA_INTrex + RDV,
                  FVA = FVA_FIN + FVA_INT) %>% 
           merge(., map.iso, by.x = "Economy", by.y = "Country") %>% 
           merge(., map.iso, by.x = "Foreign", by.y = "Country") %>% 
           select(Foreign:FVA, Abbrev.x, ISO3.digit.x, Abbrev.y, ISO3.digit.y)

colnames(wwz.agg)[31:34] <- c("MRIO.Economy", "ISO.Economy", "MRIO.Partner", "ISO.Partner")

# [D] Gravity Model Variables ----

gravity = readRDS(paste0(pathcep, "Gravity_V202102.Rds"))

# [E] Aggregate: Dataset Prelim ----

pta.2015 <- pta.fin %>% filter(year == 2015)

pta.list = list()
time = c(2016:2020)
for (t in time) {
  q = pta.2015 %>% 
      mutate(year = replace(year, year == 2015, t))
  pta.list[[t]] = q
}

pta.new = rbind(pta.fin, do.call(rbind, pta.list))

df = merge(wwz.agg, pta.new, by.x = c("ISO.Economy", "ISO.Partner", "year"), 
            by.y = c("iso1", "iso2", "year"), all.x = TRUE) %>% 
      mutate(pta = ifelse(depth > 0, 1, 0)) %>% 
     select(!c(cty1, cty2))

df[, 33:146][is.na(df[, 33:146])] <- 0
#df[, 30:141][is.na(df[, 30:141])] <- 0

out = paste0(pathout,"Baseline Dataset1.csv")
write.table(df,out,sep=",",row.names=FALSE)

# [F] Aggregate: Dataset Final ----

df.agg = merge(df, gravity, by.x = c("ISO.Economy", "ISO.Partner", "year"),
               by.y = c("iso3_o", "iso3_d", "year"), all.x = TRUE) %>%
         merge(., map.country, by.x = "MRIO.Economy", by.y = "Abbrev") %>%
         merge(., map.country, by.x = "MRIO.Partner", by.y = "Abbrev") %>% 
         mutate(cover = ifelse(Continent.x == Continent.y, "Intra-regional", "Extra-regional")) %>% 
         fill(iso3num_o:tradeflow_imf_d)  #fill 2020 NAs with 2019 values

out2 = paste0(pathout,"Baseline Dataset2.csv")
write.table(df.agg,out2,sep=",",row.names=FALSE)


# [G] Countries ---- 

sample_countries = c("KAZ", "BRN", "MYS", "SGP", "PAK")

for (c in sample_countries) {
  x = as_tibble(df.agg %>% filter(ISO.Economy == c)) %>% 
    select(., !contains("wto_") & !contains("Term"))
  
  country <- paste0(c)
  assign(country,x,envir=globalenv())
  
}

write.csv(KAZ, "Baseline Kazakhstan.csv")



