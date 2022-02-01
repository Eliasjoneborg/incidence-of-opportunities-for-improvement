install.packages("tidyverse")

library(tidyr)

För att göra om "pat_id" och "tra_id" till en ny variabel vid namn "id":

swetrau2 <-
  unite(swetrau_scrambled, id, tra_id, pat_id, remove = FALSE)

För att mergea de två datasetsen efter variabeln "id" (exkluderar alla "id":s som inte finns i bägge):

  "id" fanns redan som variabel.

Göra om "problem_scrambled" till ett nytt data set där alla inputs som listas nedan, görs om till antingen "yes" eller "no".

problem_scrambled$`Problemomrade_ FMP`[problem_scrambled$`Problemomrade_ FMP`=='ok'|problem_scrambled$`Problemomrade_ FMP`=='Ok'|problem_scrambled$`Problemomrade_ FMP`=='OK'|problem_scrambled$`Problemomrade_ FMP`=='Föredömligt handlagd']<-'No'
problem_scrambled$`Problemomrade_ FMP`[problem_scrambled$`Problemomrade_ FMP`=='bristande rutin'|problem_scrambled$`Problemomrade_ FMP`=='Dokumentation'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Dokumetation'|problem_scrambled$`Problemomrade_ FMP`=='Handläggning'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Kommunikation'|problem_scrambled$`Problemomrade_ FMP`=='kompetens brist'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Logistik/teknik'|problem_scrambled$`Problemomrade_ FMP`=='Lång tid till DT'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Lång tid till op'|problem_scrambled$`Problemomrade_ FMP`=='Missad skada'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Neurokirurg'|problem_scrambled$`Problemomrade_ FMP`=='Tertiär survey'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Resurs'|problem_scrambled$`Problemomrade_ FMP`=='Traumakriterier/styrning'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Triage på akutmottagningen'|problem_scrambled$`Problemomrade_ FMP`=='Vårdnivå']<-'Yes'

Göra om "problem_scrambled" till ett nytt data set där alla N/A inputs är exkluderade vid namn "problemområde"

problemområde<-na.omit(problem_scrambled)

"Problemområde 2"

problemområde2<- merge(swetrau2, problem_scrambled, by= "id")

library(dplyr)
problemområde3<-problemområde2 %>% select(inj_dominant, pt_age_yrs, ISS, pre_sbp_value, pre_sbp_rtscat, ed_sbp_value, ed_sbp_rtscat, 154:203)

problemområde4 <- filter(problemområde3, ISS >9, ISS<76, pt_age_yrs >15, pt_age_yrs <120, inj_dominant >0, inj_dominant <3)

Kohorter:

  Shock_cohort <- filter(problemområde4, pre_sbp_value <90 | ed_sbp_value <90 | pre_sbp_rtscat <4 | ed_sbp_rtscat <4)

Shock definierat som ett giltigt SBP>90 prehospitalt eller på traumarum.

  Elderly_cohort <- filter(problemområde4, pt_age_yrs <120, pt_age_yrs >65)

  Severe TBI
    AIS 1XXXXX.(> 3)

  Blunt multisystem
    Inj_dominant=1,
    AIS = YXXXXX.(≥ 3) +
    AIS = ZXXXXX.(≥ 3),  Y=/=Z, 1<Y<9, 1<X<9

  Penetrating
   Inj_dominant=2,
   AIS = (3-5)XXXXX.(≥ 3),

 JONATAN:

     AIS-kodfix:

  AISCode<-problemområde4 %>% select(AISCode_01)

    as.numeric(strsplit(as.character(AISCode$AISCode_01),"")[[1]])

    *genererar denna*

    [1]  6  5  0  2  1  7 NA  2 vilket korresponderar till första talet i AISCode_1-kolumnen (650217.2)

    Jag vill skapa en ny data frame där varje enskild siffra utgör en kolumn, och som loopas för alla rader i AISCode.
    Det borde tillåta mig att göra det för samtliga AIS-kolumner i min data frame "problemområde 4" för att sedan åter mergea.
    Därefter skulle jag lätt kunna göra min gruppering.

