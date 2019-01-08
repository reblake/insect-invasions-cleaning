###############################################################
# Cleaning Japanese interception data
# version 3, no ITIS, using a file updated by Takehiko so that no species are absent

setwd("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data")
library(dplyr) # This is a data manipulation package
library(rgbif) # taxonomic package
library(taxize) # taxonomic package
library(stringr)


Jint<-read.csv("190107IntersectionJapanPlantProtectionJapan_Cleanedv2.csv") # 50633 obs
# Jint<-Jint[!Jint$Species=="",] # removing rows where no species information was present at all

# extracting original species names
JintGS <- gsub(" $","", Jint$Species, perl=T) # removing trailing space
JintGSunique<-unique(JintGS) #2236 species or genus or higher

###################################################################
# Day 1
# Have already run and saved this section, see next section

AA <- data.frame(status=character(),
                 matchType=character(),
                 rank=character(),
                 family=character(),
                 canonicalName=character(),
                 order=character(),
                 class=character(),
                 key=character(),
                 stringsAsFactors=FALSE)
for (i in 1:length(JintGSunique)){
  db<-name_backbone(JintGSunique[i], verbose=TRUE)[[1]] # name_backbone
  AA[i,]<-rep(1,8)
  if (!db$matchType=="NONE"){
    AA[i,c("status","matchType","canonicalName","rank")]<-db[,c("status","matchType","canonicalName","rank")]
    if (length(db$family)==1){
      AA[i,"family"]<-db$family
    }
    if (length(db$order)==1){
      AA[i,"order"]<-db$order
    }
    if (length(db$class)==1){
      AA[i,"class"]<-db$class
    }
    if (db$status=="SYNONYM"){
      AA[i,"key"]<-db$acceptedUsageKey
    }
    else{
      AA[i,"key"]<-db$usageKey
    }
  }
}
AA2<-cbind(AA,JintGSunique) # merging with original names
# fails to match 39 Canonicalnames length(AA2[AA2$canonicalName=="1",1])
# 784 assigned to higher rank length(AA2[AA2$matchType=="HIGHERRANK",1])
# need to merge with original names

write.csv(AA2,"JapanAA2name_backbone2.csv",row.names=FALSE)
# need to fix synonyms
# need to fix 1's and places where name has changed to genus when original name was species level (gnr_resolve and then manually)

#####################################################################
# Day 2, reload AA2

AA2<-read.csv("JapanAA2name_backbone2.csv")

# Rerunning name_backbone for Insecta class where name was not previously identified, or was reduced to higher rank

CC <- data.frame(status=character(),
                 matchType=character(),
                 rank=character(),
                 family=character(),
                 canonicalName=character(),
                 order=character(),
                 class=character(),
                 key=character(),
                 stringsAsFactors=FALSE)
GS<-AA2[AA2$canonicalName==1|(AA2$matchType=="HIGHERRANK"&(AA2$class=="Insecta"|AA2$class=="1")),"JintGSunique"]
for (i in 1:length(GS)){
  db<-name_backbone(GS[i],class="Insecta", verbose=TRUE)[[1]]
  CC[i,]<-rep(1,8)
  if (!db$matchType=="NONE"){
    CC[i,c("status","matchType","canonicalName","rank")]<-db[,c("status","matchType","canonicalName","rank")]
    if (length(db$family)==1){
      CC[i,"family"]<-db$family
    }
    if (length(db$order)==1){
      CC[i,"order"]<-db$order
    }
    if (length(db$class)==1){
      CC[i,"class"]<-db$class
    }
    if (db$status=="SYNONYM"){
      CC[i,"key"]<-db$acceptedUsageKey
    }
    else{
      CC[i,"key"]<-db$usageKey
    }
  }
}
CC2<-cbind(CC,GS)

# there is now only one error in the higher rank section, and much fewer higher ranks to fix. Some additional synonyms, and subspecies to fix once remerged.
######################################################################

CC2[CC2$GS=="Paropsisterna m-fuscum","canonicalName"]<-"Paropsisterna m-fuscum"
CC2[CC2$GS=="Paropsisterna m-fuscum","rank"]<-"SPECIES"
# https://en.wikipedia.org/wiki/Paropsisterna_m-fuscum
# Atlas of living Australia:
# https://bie.ala.org.au/species/urn:lsid:biodiversity.org.au:afd.taxon:111a394f-46d1-4ec6-8795-a37c951ed166#tab_classification

########################################################################
# remerging list
colnames(CC2)[colnames(CC2)=="GS"] <- "JintGSunique"
BB2<-rbind(CC2,AA2[!(AA2$canonicalName==1|(AA2$matchType=="HIGHERRANK"&(AA2$class=="Insecta"|AA2$class=="1"))),])

write.csv(BB2,"JapanBB2name_backbone2.csv",row.names=FALSE)
# length(BB2[BB2$matchType=="HIGHERRANK",1])
########################################################################

# Day 3 reload BB2
BB2<-read.csv("JapanBB2name_backbone2.csv")

# Using gnr_resolve to find more of the names which are identified to species level but which were relegated to higher rank
# i.e. these were species unknown to gbif

Cq<-BB2[BB2$matchType=="HIGHERRANK"&(BB2$class=="Insecta"|BB2$class=="1")&(!(BB2$rank=="FAMILY"|BB2$rank=="SPECIES")),"JintGSunique"] #74
C1<-as.character(rep(1,length(Cq)))
DD<-data.frame(user_supplied_name=C1,
               submitted_name=C1,
               data_source_title=C1,
               score=C1,
               matched_name2=C1,
               stringsAsFactors=FALSE)
for (i in 1:length(Cq)){
  match1<-gnr_resolve(as.character(Cq[i]),data_source_ids=c(1,2,3,12,4,152),canonical=TRUE,best_match_only=TRUE) # only finds 63 of 76
  if (length(match1)>0){
    DD[i,]<-match1
  }
}
DD2<-cbind(DD,BB2[BB2$matchType=="HIGHERRANK"&(BB2$class=="Insecta"|BB2$class=="1")&(!(BB2$rank=="FAMILY"|BB2$rank=="SPECIES")),"canonicalName"])
DD2$user_supplied_name<-Cq

###############################################
# need to fix a few manually
DD2[DD2$user_supplied_name=="Bruchidius japonicus","matched_name2"]<-"Bruchidius japonicus"
# http://www.aemnp.eu/PDF/57_1/57_1_161.pdf
# Anton, K. W., & Delobel, A. (2017). Three new Asian species of Bruchidius (Coleoptera: Chrysomelidae: Bruchinae). Acta Entomologica Musei Nationalis Pragae, 57(1), 161-172.

# ask Takeheko about Mecysolobus bubo (Fabricius) Curculionidae
# Semiothisa begabunda
# Caresa sp.
DD2[DD2$user_supplied_name=="Megalurothrips kellyanus","matched_name2"]<-"Pezothrips kellyanus"
# https://gd.eppo.int/taxon/PEZTKE

DD2[DD2$user_supplied_name=="Precis atlites","matched_name2"]<-"Junonia atlites"
# https://en.wikipedia.org/wiki/Junonia_atlites
# doi:10.13140/RG.2.1.3966.2164. ISBN 978-81-929826-4-9.

DD2[DD2$user_supplied_name=="Cydia kurokoi","matched_name2"]<-"Cydia kurokoi"
# https://en.wikipedia.org/wiki/Cydia_kurokoi
# Natural hisotry museum
# http://www.nhm.ac.uk/our-science/data/lepindex/detail/?taxonno=94018

DD2[DD2$user_supplied_name=="Mycterothrips tschirkunae","matched_name2"]<-"Mycterothrips tschirkunae"
# https://thrips.info/wiki/Mycterothrips_tschirkunae
# Masumoto M & Okajima S (2006) A revision of and key to the world species of Mycterothrips Trybom (Thysanoptera, Thripidae). Zootaxa 1261: 1-90.

DD2[DD2$user_supplied_name=="Princeps demoleus","matched_name2"]<-"Papilio demoleus"
# https://lepidoptera.eu/species/11897
# https://www.cabi.org/isc/datasheet/38758

DD2[DD2$user_supplied_name=="Acrobasis tokiella","matched_name2"]<-"Acrobasis tokiella"
# http://www.pherobase.com/database/invasive-species/species-Acrobasis-tokiella.php
# http://insecta.pro/taxonomy/21172
# ?????????????? ?????????????????????? (Lepidoptera) ????????????. ?????? ??????. ??. ??. ????????????. ??????.; ??.: ???????????????????????? ?????????????? ?????????????? ??????, 2008

DD2[DD2$user_supplied_name=="Liriomyza betae","matched_name2"]<-"Liriomyza betae"
# https://link.springer.com/content/pdf/bbm%3A978-94-009-1874-0%2F1.pdf
# Spencer, K. A. (2012). Host specialization in the world Agromyzidae (Diptera) (Vol. 45). Springer Science & Business Media.

DD2[DD2$user_supplied_name=="Panchaetothrips indicus","matched_name2"]<-"Panchaetothrips indicus"
# https://thrips.info/wiki/Panchaetothrips_indicus
# Bagnall RS (1912) On a new genus of Indian thrips (Thysanoptera) injurious to Turmeric. Records of the Indian Museum 7: 257-260.
# https://bie.ala.org.au/species/ALA_Panchaetothrips_indicus

DD2[DD2$user_supplied_name=="Mudaria luteileprosa","matched_name2"]<-"Mudaria luteileprosa"
# http://www.mothsofborneo.com/part-12/amphipyrinae/amphipyrinae_30_5.php
# Ooi, P. A., Winotai, A., & Peña, J. E. (2002). Pests of minor tropical fruits. Tropical fruit pests and pollinators: biology, economic importance, natural enemies and control, 315, 321.

DD2[DD2$user_supplied_name=="Pseudococcus aurantiacus","matched_name2"]<-"Pseudococcus aurantiacus"
# http://www.idtools.org/id/scales/factsheet.php?name=7001
# Williams, D.J. 2004 A synopsis of the subterranean mealybug genus Neochavesia Williams and Granara de Willink (Hemiptera: Pseudococcidae: Rhizoecinae).. Journal of Natural History 38(22): 2883-2899.

DD2[DD2$user_supplied_name=="Synaptothrips paradoxus","matched_name2"]<-"Synaptothrips paradoxus"
# https://thrips.info/wiki/Synaptothrips_paradoxus
# Synaptothrips paradoxus (Bagnall, 1919)

DD2[DD2$user_supplied_name=="Scotinophara coarctata","matched_name2"]<-"Scotinophara coarctata"
# Anandhi, P., & Pillai, M. A. K. (2006). Effect of entomopathogenic fungi against Scotinophara coarctata. Annals of Plant Protection Sciences, 14(1), 221-222.

DD2[DD2$user_supplied_name=="Lithraeus elegans","matched_name2"]<-"Lithraeus elegans"
# Chaboo, C. S., & Morse, G. E. (2015). Beetles (Coleoptera) of Peru: A Survey of the Families. Chrysomelidae: Bruchinae Latreille, 1802. Journal of the Kansas Entomological Society, 88(3), 356-360.

DD2[DD2$user_supplied_name=="Antestiopsis orbitalis","matched_name2"]<-"Antestiopsis orbitalis"
# https://gd.eppo.int/taxon/ANTEOR
# Kutywayo, D., Chemura, A., Kusena, W., Chidoko, P., & Mahoya, C. (2013). The impact of climate change on the potential distribution of agricultural pests: the case of the coffee white stem borer (Monochamus leuconotus P.) in Zimbabwe. PLoS One, 8(8), e73432.

DD2[DD2$user_supplied_name=="Ocirrhoe unimaculata","matched_name2"]<-"Ocirrhoe unimaculata"
# https://bie.ala.org.au/species/urn:lsid:biodiversity.org.au:afd.taxon:572c1d51-5ff6-4e98-a117-987a6ec72151
# Published in:Gross, G.F. 1976, vol. 2, pp. 251-501 pp., A.B. James, Adelaide

DD2[DD2$user_supplied_name=="Rhabdoscelus lineaticollis","matched_name2"]<-"Rhabdoscelus similis"
# http://weevil.info/taxonomy/term/33409/descriptions

DD2[DD2$user_supplied_name=="Sorghothrips jonnaphilus","matched_name2"]<-"Sorghothrips jonnaphilus"
# Kumar, V., Tyagi, K., & Bhatti, J. S. (2007). Checklist of Terebrantia (Thysanoptera: Insecta) of Delhi. Zoos Print Journal, 22(6), 2714-2718.
# https://thrips.info/wiki/Sorghothrips_jonnaphilus

DD2[DD2$user_supplied_name=="Protopulivinaria sp.","matched_name2"]<-"Protopulvinaria"
# http://scalenet.info/catalogue/Protopulvinaria%20pyriformis/

DD2[DD2$user_supplied_name=="Halyomorepha mista","matched_name2"]<-"Halyomorpha mista"
# https://gd.eppo.int/taxon/HALYMI

DD2[DD2$user_supplied_name=="Aglirus sp.","matched_name2"]<-"Agrilus"
# https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=1580618

DD2[DD2$user_supplied_name=="Paraleurodes sp.","matched_name2"]<-"Paraleyrodes"
# as in nesting whitefly

DD2[DD2$user_supplied_name=="Crytoclytus sp.","matched_name2"]<-"Cyrtoclytus"
# https://www.inaturalist.org/taxa/500741-Crytoclytus-capra

DD2[DD2$user_supplied_name=="Eucetonia pilifera","matched_name2"]<-"Eucetonia pilifera"
# https://eol.org/pages/3241084

DD2[DD2$user_supplied_name=="Alstromeria mosaic virus","matched_name2"]<-"Alstroemeria mosaic virus"
# Fuji, S., Terami, F., Furuya, H., Naito, H., & Fukumoto, F. (2004). Nucleotide sequence of the coat protein genes of Alstroemeria mosaic virus and Amazon lily mosaic virus, a tentative species of genus Potyvirus. Archives of virology, 149(9), 1843-1849.

DD2[DD2$user_supplied_name=="Acanthoplusia agnata","matched_name2"]<-"Acanthoplusia agnata"
# https://gd.eppo.int/taxon/ACAUAG

DD2[DD2$user_supplied_name=="Hordeolicoccus nephelii","matched_name2"]<-"Hordeolicoccus nephelii"
# http://www.idtools.org/id/scales/factsheet.php?name=6979
# http://scalenet.info/catalogue/Hordeolicoccus%20nephelii/

DD2[DD2$user_supplied_name=="Acanthoplusia sp.","matched_name2"]<-"Acanthoplusia"
# https://gd.eppo.int/taxon/ACAUSP


DD2[DD2$user_supplied_name=="Scobinoides dentatus","matched_name2"]<-"Scobinoides dentatus"
# Lee, W., Hwang, J. H., Lee, J. H., & Hong, K. J. (2017). Interception of weevils on cut flowers from South Africa by Korea plant quarantine. Journal of Asia-Pacific Biodiversity, 10(4), 527-531.


# unknown names
# ask Takeheko about Mecysolobus bubo (Fabricius) Curculionidae
# Semiothisa begabunda
# Caresa sp.
DD2[DD2$user_supplied_name=="Mecysolobus bubo","matched_name2"]<-"unknown"
DD2[DD2$user_supplied_name=="Semiothisa begabunda","matched_name2"]<-"Macaria abydata" # this is what the Japanese name referes to according to Takehiko
DD2[DD2$user_supplied_name=="Caresa sp.","matched_name2"]<-"unknown"

#####################################################
# this bit is interactive

# filling in higher taxonomic info: 

#rank
RR<-tax_rank(DD2$matched_name2,db="gbif")
DD2$rank<-RR
RR2<-tax_rank(DD2$matched_name2,db="ncbi")
DD2$rank2<-RR2
DD2[is.na(DD2$rank2),"rank2"]<-"species"
DD2[is.na(DD2$rank),"rank"]<-"species"

DD2[DD2$rank=="species"|DD2$rank2=="species","rank"]<-"SPECIES"
DD2[DD2$rank=="genus","rank"]<-"GENUS"

DD2[DD2$matched_name2=="Acanthoplusia","rank"]<-"GENUS"

# filling in higher taxonomic info: family, order and class
NN<-tax_name(DD2$matched_name2, get=c("family","order","class"), db = "both", pref = "ncbi", messages = TRUE)
NNi<-NN[seq(1,length(NN$db),2),]
NNn<-NN[seq(2,length(NN$db),2),]

NNn[is.na(NNn$family),]<-NNi[is.na(NNn$family),]

# this did way better than itis or ncbi
Ff<-as.character(rep(1,length(NNn$query)))
FF<-data.frame(family=Ff,
               order=Ff,
               class=Ff,
               stringsAsFactors=FALSE)
for (i in 1:length(NNn$query)){
  f<-name_backbone(as.character(NNn$query[i]),class="Insecta")$family
  o<-name_backbone(as.character(NNn$query[i]),class="Insecta")$order
  c<-name_backbone(as.character(NNn$query[i]),class="Insecta")$class
  if (length(f)>0){
    FF[i,"family"]<-f}
  if (length(o)>0){
    FF[i,"order"]<-o}
  if (length(c)>0){
    FF[i,"class"]<-c}
}

NN2<-cbind(NNn$query,FF)
NN2[NN2$family==1,"family"]<-NNn[NN2$family==1,"family"]
NN2[NN2$order==1,"order"]<-NNn[NN2$order==1,"order"]
NN2[NN2$class==1,"class"]<-NNn[NN2$class==1,"class"]

NN2[NN2$`NNn$query`=="Hordeolicoccus nephelii","family"]<-"Pseudococcidae"
NN2[NN2$`NNn$query`=="Acanthoplusia","family"]<-"Noctuidae"
NN2[NN2$`NNn$query`=="Acanthoplusia","order"]<-"Lepidoptera"
NN2[NN2$`NNn$query`=="Acanthoplusia agnata","family"]<-"Noctuidae"
NN2[NN2$`NNn$query`=="Acanthoplusia agnata","order"]<-"Lepidoptera"
NN2[NN2$`NNn$query`=="Uredo","class"]<-"Basidiomycetes"
NN2[NN2$`NNn$query`=="Alstroemeria mosaic virus","class"]<-"virus"

NN2[NN2$`NNn$query`=="Acremoniella","class"]<-"Ascomycota"
NN2[NN2$`NNn$query`=="Zygosporium","class"]<-"Sordariomycetes"

# Reconstruct taxonomic table
DD2fixed <- data.frame("status" = "DOUBTFUL", "matchType" = "EXACT","rank"=as.character(DD2$rank),"family"=NN2$family,"canonicalName"=DD2$matched_name2,"order"=NN2$order,"class"=NN2$class,"JintGSunique"=DD2$user_supplied_name,"key"="1",stringsAsFactors=FALSE)

#########################################
# putting table back together and fixing synonyms

EE2<-rbind(DD2fixed,BB2[!(BB2$matchType=="HIGHERRANK"&(BB2$class=="Insecta"|BB2$class=="1")&(!(BB2$rank=="FAMILY"|BB2$rank=="SPECIES"))),])

EE2[EE2$JintGSunique=="Esbenia sp.","family"]<-"Acanthosomatidae"

EE22<-EE2
len<-numeric()
# fixing synonyms
for (i in 1:length(EE22$rank)){
  if (EE22$status[i]=="SYNONYM"&EE22$class=="Insecta"){
    s<-name_usage(EE22$key[i])$data$species
    len[i]<-length(s)
    if (length(s)>=1){
      EE22[i,"canonicalName"]<-s[1]
    }
    else{
      EE22[i,"canonicalName"]<-name_usage(EE22$key[i])$data$genus
    }
  }
}

write.csv(EE22,"Cleaned_190107IntersectionJapanPlantProtectionJapan_Cleanedv2.csv",row.names=FALSE)
