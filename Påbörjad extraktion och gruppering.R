install.packages("tidyverse")

library(tidyr)

swetrau2 <-
  unite(swetrau_scrambled, id, tra_id, pat_id, remove = FALSE)

problem_scrambled$`Problemomrade_ FMP`[problem_scrambled$`Problemomrade_ FMP`=='ok'|problem_scrambled$`Problemomrade_ FMP`=='Ok'|problem_scrambled$`Problemomrade_ FMP`=='OK'|problem_scrambled$`Problemomrade_ FMP`=='Föredömligt handlagd']<-'No'
problem_scrambled$`Problemomrade_ FMP`[problem_scrambled$`Problemomrade_ FMP`=='bristande rutin'|problem_scrambled$`Problemomrade_ FMP`=='Dokumentation'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Dokumetation'|problem_scrambled$`Problemomrade_ FMP`=='Handläggning'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Kommunikation'|problem_scrambled$`Problemomrade_ FMP`=='kompetens brist'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Logistik/teknik'|problem_scrambled$`Problemomrade_ FMP`=='Lång tid till DT'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Lång tid till op'|problem_scrambled$`Problemomrade_ FMP`=='Missad skada'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Neurokirurg'|problem_scrambled$`Problemomrade_ FMP`=='Tertiär survey'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Resurs'|problem_scrambled$`Problemomrade_ FMP`=='Traumakriterier/styrning'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Triage på akutmottagningen'|problem_scrambled$`Problemomrade_ FMP`=='Vårdnivå']<-'Yes'

problemområde<-na.omit(problem_scrambled)

problemområde2<- merge(swetrau2, problem_scrambled, by= "id")

library(dplyr)
problemområde3<-problemområde2 %>% select(inj_dominant, pt_age_yrs, NISS, pre_sbp_value, pre_sbp_rtscat, ed_sbp_value, ed_sbp_rtscat, 154:203)

problemområde4 <- filter(problemområde3, NISS >15, NISS<76, pt_age_yrs >15, pt_age_yrs <120, inj_dominant >0, inj_dominant <3)

#AIS-fix

AISCode<-problemområde4 %>% select(AISCode_01)

getLastNumber = function(x) str_sub(x, 8L, 8L)

AIS2 <- apply(AIS, c(1,2), getLastNumber)

names(AIS2) <- c('AIS_sev1', 'AIS_sev2', 'AIS_sev3', 'AIS_sev4', 'AIS_sev5', 'AIS_sev6', 'AIS_sev7', 'AIS_sev8', 'AIS_sev9', 'AIS_sev10',
                 'AIS_sev11', 'AIS_sev12', 'AIS_sev13', 'AIS_sev14', 'AIS_sev15', 'AIS_sev16', 'AIS_sev17', 'AIS_sev18', 'AIS_sev19',
                 'AIS_sev20', 'AIS_sev21', 'AIS_sev22', 'AIS_sev23', 'AIS_sev24', 'AIS_sev25', 'AIS_sev26', 'AIS_sev27', 'AIS_sev28',
                 'AIS_sev29', 'AIS_sev30', 'AIS_sev31', 'AIS_sev32', 'AIS_sev33', 'AIS_sev34', 'AIS_sev35', 'AIS_sev36', 'AIS_sev37',
                 'AIS_sev38', 'AIS_sev39', 'AI_sev40', 'AIS_sev41', 'AIS_sev42', 'AIS_sev43', 'AIS_sev44', 'AIS_sev45', 'AIS_sev46',
                 'AIS_sev47', 'AIS_sev48', 'AIS_sev49', 'AIS_sev50')

problemområde5<-cbind(problemområde4, AIS2)

problemområde5 %>% filter(AISCode_1 > 99999 & AISCode_1 <200000  & AIS_sev1>3)

#Ny variabel vid namn AIS_sev(1-50) som representerar AIS severity.

#KOHORTER:

#SHOCK

Shock_cohort <- filter(problemområde4, pre_sbp_value <90 |ed_sbp_value <90 & pre_sbp_rtscat <4 | ed_sbp_rtscat <4)

library(table1)

factor(Shock_cohort$inj_dominant, levels=c(1,2),
       labels=c("Blunt", "Penetrating")

label(Shock_cohort$NISS)              <- "New Injury Severity Score"
label(Shock_cohort$pre_sbp_value)    <- "Pre Hospital Systolic BP"
label(Shock_cohort$ed_sbp_value)     <- "Emergency Department Systolic BP"

units(Shock_cohort$pre_sbp_value)  <- "mmHg"
units(Shock_cohort$ed_sbp_value)   <- "mmHg"

table1(~ inj_dominant + NISS + pre_sbp_value + ed_sbp_value | inj_dominant data=Shock_cohort)

#ELDERLY

Elderly_cohort <- filter(problemområde4, pt_age_yrs <120, pt_age_yrs >65)

library(table1)

factor(Elderly_cohort$inj_dominant, levels=c(1,2),
       labels=c("Blunt", "Penetrating")

                label(Elderly_cohort$NISS)              <- "New Injury Severity Score"
                label(Elderly_cohort$pre_sbp_value)    <- "Pre Hospital Systolic BP"
                label(Elderly_cohort$ed_sbp_value)     <- "Emergency Department Systolic BP"

                units(Elderly_cohort$pre_sbp_value)  <- "mmHg"
                units(Elderly_cohort$ed_sbp_value)   <- "mmHg"

                table1(~ inj_dominant + NISS + pre_sbp_value + ed_sbp_value | inj_dominant data=Elderly_cohort)

  #BLUNT MULTISYSTEM

  #Har lekt runt en del, men har svårt att komma på ett smidigt sätt att extrahera just den här gruppen.

    #Inj_dominant=1,
    #AIS = YXXXXX.(≥ 3) +
    #AIS = ZXXXXX.(≥ 3),  Y=/=Z, 1<Y<9, 1<X<9

  #SEVERE TBI

  #Har misslyckats med att lösa det på ett smidigt sätt, för att inte ödsla mer tid har jag kört på nödplan, dvs:

    TBI_1 <-filter(problemområde5, AISCode_01 <100000 & AIS_sev01>3)
    TBI_2 <-filter(problemområde5, AISCode_02 <100000 & AIS_sev02>3)
    TBI_3 <-filter(problemområde5, AISCode_03 <100000 & AIS_sev03>3)
    TBI_4 <-filter(problemområde5, AISCode_04 <100000 & AIS_sev04>3)
    TBI_5 <-filter(problemområde5, AISCode_05 <100000 & AIS_sev05>3)
    TBI_6 <-filter(problemområde5, AISCode_06 <100000 & AIS_sev06>3)
    TBI_7 <-filter(problemområde5, AISCode_07 <100000 & AIS_sev07>3)
    TBI_8 <-filter(problemområde5, AISCode_08 <100000 & AIS_sev08>3)
    TBI_9 <-filter(problemområde5, AISCode_09 <100000 & AIS_sev09>3)
    TBI_10 <-filter(problemområde5, AISCode_10 <100000 & AIS_sev10>3)
    TBI_11 <-filter(problemområde5, AISCode_11 <100000 & AIS_sev11>3)
    TBI_12 <-filter(problemområde5, AISCode_12 <100000 & AIS_sev12>3)
    TBI_13 <-filter(problemområde5, AISCode_13 <100000 & AIS_sev13>3)
    TBI_14 <-filter(problemområde5, AISCode_14 <100000 & AIS_sev14>3)
    TBI_15 <-filter(problemområde5, AISCode_15 <100000 & AIS_sev15>3)
    TBI_16 <-filter(problemområde5, AISCode_16 <100000 & AIS_sev16>3)
    TBI_17 <-filter(problemområde5, AISCode_17 <100000 & AIS_sev17>3)
    TBI_18 <-filter(problemområde5, AISCode_18 <100000 & AIS_sev18>3)
    TBI_19 <-filter(problemområde5, AISCode_19 <100000 & AIS_sev19>3)
    TBI_20 <-filter(problemområde5, AISCode_20 <100000 & AIS_sev20>3)
    TBI_21 <-filter(problemområde5, AISCode_21 <100000 & AIS_sev21>3)
    TBI_22 <-filter(problemområde5, AISCode_22 <100000 & AIS_sev22>3)
    TBI_23 <-filter(problemområde5, AISCode_23 <100000 & AIS_sev23>3)
    TBI_24 <-filter(problemområde5, AISCode_24 <100000 & AIS_sev24>3)
    TBI_25 <-filter(problemområde5, AISCode_25 <100000 & AIS_sev25>3)
    TBI_26 <-filter(problemområde5, AISCode_26 <100000 & AIS_sev26>3)
    TBI_27 <-filter(problemområde5, AISCode_27 <100000 & AIS_sev27>3)
    TBI_28 <-filter(problemområde5, AISCode_28 <100000 & AIS_sev28>3)
    TBI_29 <-filter(problemområde5, AISCode_29 <100000 & AIS_sev29>3)
    TBI_30 <-filter(problemområde5, AISCode_30 <100000 & AIS_sev30>3)
    TBI_31 <-filter(problemområde5, AISCode_31 <100000 & AIS_sev31>3)
    TBI_32 <-filter(problemområde5, AISCode_32 <100000 & AIS_sev32>3)
    TBI_33 <-filter(problemområde5, AISCode_33 <100000 & AIS_sev33>3)
    TBI_34 <-filter(problemområde5, AISCode_34 <100000 & AIS_sev34>3)
    TBI_35 <-filter(problemområde5, AISCode_35 <100000 & AIS_sev35>3)
    TBI_36 <-filter(problemområde5, AISCode_36 <100000 & AIS_sev36>3)
    TBI_37 <-filter(problemområde5, AISCode_37 <100000 & AIS_sev37>3)
    TBI_38 <-filter(problemområde5, AISCode_38 <100000 & AIS_sev38>3)
    TBI_39 <-filter(problemområde5, AISCode_39 <100000 & AIS_sev39>3)
    TBI_40 <-filter(problemområde5, AISCode_40 <100000 & AIS_sev40>3)
    TBI_41 <-filter(problemområde5, AISCode_41 <100000 & AIS_sev41>3)
    TBI_42 <-filter(problemområde5, AISCode_42 <100000 & AIS_sev42>3)
    TBI_43 <-filter(problemområde5, AISCode_43 <100000 & AIS_sev43>3)
    TBI_44 <-filter(problemområde5, AISCode_44 <100000 & AIS_sev44>3)
    TBI_45 <-filter(problemområde5, AISCode_45 <100000 & AIS_sev45>3)
    TBI_46 <-filter(problemområde5, AISCode_46 <100000 & AIS_sev46>3)
    TBI_47 <-filter(problemområde5, AISCode_47 <100000 & AIS_sev47>3)
    TBI_48 <-filter(problemområde5, AISCode_48 <100000 & AIS_sev48>3)
    TBI_49 <-filter(problemområde5, AISCode_49 <100000 & AIS_sev49>3)
    TBI_50 <-filter(problemområde5, AISCode_50 <100000 & AIS_sev50>3)

   SevereTBI_cohort <- Reduce(function(x, y) merge(x, y, all=TRUE),
                              list(TBI_1, TBI_2, TBI_3, TBI_4, TBI_5,
                                   TBI_6, TBI_7, TBI_8, TBI_9, TBI_10,
                                   TBI_11, TBI_12, TBI_13, TBI_14, TBI_15,
                                   TBI_16, TBI_17, TBI_18, TBI_19, TBI_20,
                                   TBI_21, TBI_22, TBI_23, TBI_24, TBI_25,
                                   TBI_26, TBI_27, TBI_28, TBI_29, TBI_30,
                                   TBI_31, TBI_32, TBI_33, TBI_34, TBI_35,
                                   TBI_36, TBI_37, TBI_38, TBI_39, TBI_40,
                                   TBI_41, TBI_42, TBI_43, TBI_44, TBI_45,
                                   TBI_46, TBI_47, TBI_48, TBI_49, TBI_50))

  library(table1)
   factor(SevereTBI_cohort1$inj_dominant, levels=c(1,2),
          labels=c("Blunt", "Penetrating")

                   label(SevereTBI_cohort1$NISS)              <- "New Injury Severity Score"
                   label(SevereTBI_cohort1$pre_sbp_value)    <- "Pre Hospital Systolic BP"
                   label(SevereTBI_cohort1$ed_sbp_value)     <- "Emergency Department Systolic BP"

                   units(SevereTBI_cohort1$pre_sbp_value)  <- "mmHg"
                   units(SevereTBI_cohort1$ed_sbp_value)   <- "mmHg"

                   table1(~ pt_age_yrs + NISS + pre_sbp_value + ed_sbp_value | inj_dominant, data=SevereTBI_cohort1)

  #Att göra: exkludera patienter vars AISnummer börjar på en 0:a,borde ha gjorts tidigare, borde gå att använda starts_with t.ex.
  #Exkludera patienter med AIS_sev>6 (dvs 9), då skalan slutar på 6 och nästa steg (9) betyder "not further specified"borde gå att använda starts_with t.ex.

   #PENETRATING

    Penetrating_1 <-filter(problemområde5, AISCode_01 <500000 & AISCode_01 >300000 & AIS_sev01>2 & AIS_sev01<7)
    Penetrating_2 <-filter(problemområde5, AISCode_02 <500000 & AISCode_02 >300000 & AIS_sev02>2 & AIS_sev02<7)
    Penetrating_3 <-filter(problemområde5, AISCode_03 <500000 & AISCode_03 >300000 & AIS_sev03>2 & AIS_sev03<7)
    Penetrating_4 <-filter(problemområde5, AISCode_04 <500000 & AISCode_04 >300000 & AIS_sev04>2 & AIS_sev04<7)
    Penetrating_5 <-filter(problemområde5, AISCode_05 <500000 & AISCode_05 >300000 & AIS_sev05>2 & AIS_sev05<7)
    Penetrating_6 <-filter(problemområde5, AISCode_06 <500000 & AISCode_06 >300000 & AIS_sev06>2 & AIS_sev06<7)
    Penetrating_7 <-filter(problemområde5, AISCode_07 <500000 & AISCode_07 >300000 & AIS_sev07>2 & AIS_sev07<7)
    Penetrating_8 <-filter(problemområde5, AISCode_08 <500000 & AISCode_08 >300000 & AIS_sev08>2 & AIS_sev08<7)
    Penetrating_9 <-filter(problemområde5, AISCode_09 <500000 & AISCode_09 >300000 & AIS_sev09>2 & AIS_sev09<7)
    Penetrating_10 <-filter(problemområde5, AISCode_10 <500000 & AISCode_10 >300000 & AIS_sev10>2 & AIS_sev10<7)
    Penetrating_11 <-filter(problemområde5, AISCode_11 <500000 & AISCode_11 >300000 & AIS_sev11>2 & AIS_sev11<7)
    Penetrating_12 <-filter(problemområde5, AISCode_12 <500000 & AISCode_12 >300000 & AIS_sev12>2 & AIS_sev01<7)
    Penetrating_12 <-filter(problemområde5, AISCode_12 <500000 & AISCode_12 >300000 & AIS_sev12>2 & AIS_sev12<7)
    Penetrating_13 <-filter(problemområde5, AISCode_13 <500000 & AISCode_13 >300000 & AIS_sev13>2 & AIS_sev13<7)
    Penetrating_14 <-filter(problemområde5, AISCode_14 <500000 & AISCode_14 >300000 & AIS_sev14>2 & AIS_sev14<7)
    Penetrating_15 <-filter(problemområde5, AISCode_15 <500000 & AISCode_15 >300000 & AIS_sev15>2 & AIS_sev15<7)
    Penetrating_16 <-filter(problemområde5, AISCode_16 <500000 & AISCode_16 >300000 & AIS_sev16>2 & AIS_sev16<7)
    Penetrating_17 <-filter(problemområde5, AISCode_17 <500000 & AISCode_17 >300000 & AIS_sev17>2 & AIS_sev17<7)
    Penetrating_18 <-filter(problemområde5, AISCode_18 <500000 & AISCode_18 >300000 & AIS_sev18>2 & AIS_sev18<7)
    Penetrating_19 <-filter(problemområde5, AISCode_19 <500000 & AISCode_19 >300000 & AIS_sev19>2 & AIS_sev19<7)
    Penetrating_20 <-filter(problemområde5, AISCode_20 <500000 & AISCode_20 >300000 & AIS_sev20>2 & AIS_sev20<7)
    Penetrating_21 <-filter(problemområde5, AISCode_21 <500000 & AISCode_21 >300000 & AIS_sev21>2 & AIS_sev21<7)
    Penetrating_22 <-filter(problemområde5, AISCode_22 <500000 & AISCode_22 >300000 & AIS_sev22>2 & AIS_sev22<7)
    Penetrating_23 <-filter(problemområde5, AISCode_23 <500000 & AISCode_23 >300000 & AIS_sev23>2 & AIS_sev23<7)
    Penetrating_24 <-filter(problemområde5, AISCode_24 <500000 & AISCode_24 >300000 & AIS_sev24>2 & AIS_sev24<7)
    Penetrating_25 <-filter(problemområde5, AISCode_25 <500000 & AISCode_25 >300000 & AIS_sev25>2 & AIS_sev25<7)
    Penetrating_26 <-filter(problemområde5, AISCode_26 <500000 & AISCode_26 >300000 & AIS_sev26>2 & AIS_sev26<7)
    Penetrating_27 <-filter(problemområde5, AISCode_27 <500000 & AISCode_27 >300000 & AIS_sev27>2 & AIS_sev27<7)
    Penetrating_28 <-filter(problemområde5, AISCode_28 <500000 & AISCode_28 >300000 & AIS_sev28>2 & AIS_sev28<7)
    Penetrating_29 <-filter(problemområde5, AISCode_29 <500000 & AISCode_29 >300000 & AIS_sev29>2 & AIS_sev29<7)
    Penetrating_30 <-filter(problemområde5, AISCode_30 <500000 & AISCode_30 >300000 & AIS_sev30>2 & AIS_sev30<7)
    Penetrating_31 <-filter(problemområde5, AISCode_31 <500000 & AISCode_31 >300000 & AIS_sev31>2 & AIS_sev31<7)
    Penetrating_32 <-filter(problemområde5, AISCode_32 <500000 & AISCode_32 >300000 & AIS_sev32>2 & AIS_sev32<7)
    Penetrating_33 <-filter(problemområde5, AISCode_33 <500000 & AISCode_33 >300000 & AIS_sev33>2 & AIS_sev33<7)
    Penetrating_34 <-filter(problemområde5, AISCode_34 <500000 & AISCode_34 >300000 & AIS_sev34>2 & AIS_sev34<7)
    Penetrating_35 <-filter(problemområde5, AISCode_35 <500000 & AISCode_35 >300000 & AIS_sev35>2 & AIS_sev35<7)
    Penetrating_36 <-filter(problemområde5, AISCode_36 <500000 & AISCode_36 >300000 & AIS_sev36>2 & AIS_sev36<7)
    Penetrating_37 <-filter(problemområde5, AISCode_37 <500000 & AISCode_37 >300000 & AIS_sev37>2 & AIS_sev37<7)
    Penetrating_38 <-filter(problemområde5, AISCode_38 <500000 & AISCode_38 >300000 & AIS_sev38>2 & AIS_sev38<7)
    Penetrating_39 <-filter(problemområde5, AISCode_39 <500000 & AISCode_39 >300000 & AIS_sev39>2 & AIS_sev39<7)
    Penetrating_40 <-filter(problemområde5, AISCode_40 <500000 & AISCode_40 >300000 & AIS_sev40>2 & AIS_sev40<7)
    Penetrating_41 <-filter(problemområde5, AISCode_41 <500000 & AISCode_41 >300000 & AIS_sev41>2 & AIS_sev41<7)
    Penetrating_42 <-filter(problemområde5, AISCode_42 <500000 & AISCode_42 >300000 & AIS_sev42>2 & AIS_sev42<7)
    Penetrating_43 <-filter(problemområde5, AISCode_43 <500000 & AISCode_43 >300000 & AIS_sev43>2 & AIS_sev43<7)
    Penetrating_44 <-filter(problemområde5, AISCode_44 <500000 & AISCode_44 >300000 & AIS_sev44>2 & AIS_sev44<7)
    Penetrating_45 <-filter(problemområde5, AISCode_45 <500000 & AISCode_45 >300000 & AIS_sev45>2 & AIS_sev45<7)
    Penetrating_46 <-filter(problemområde5, AISCode_46 <500000 & AISCode_46 >300000 & AIS_sev46>2 & AIS_sev46<7)
    Penetrating_47 <-filter(problemområde5, AISCode_47 <500000 & AISCode_47 >300000 & AIS_sev47>2 & AIS_sev47<7)
    Penetrating_48 <-filter(problemområde5, AISCode_48 <500000 & AISCode_48 >300000 & AIS_sev48>2 & AIS_sev48<7)
    Penetrating_49 <-filter(problemområde5, AISCode_49 <500000 & AISCode_49 >300000 & AIS_sev49>2 & AIS_sev49<7)
    Penetrating_50 <-filter(problemområde5, AISCode_50 <500000 & AISCode_50 >300000 & AIS_sev50>2 & AIS_sev50<7)

    Penetrating_cohort <- Reduce(function(x, y) merge(x, y, all=TRUE),
                                  +                            list(Penetrating_1, Penetrating_2, Penetrating_3, Penetrating_4, Penetrating_5,
                                                                    Penetrating_6, Penetrating_7, Penetrating_8, Penetrating_9, Penetrating_10,
                                                                    Penetrating_11, Penetrating_12, Penetrating_13, Penetrating_14, Penetrating_15,
                                                                    Penetrating_16, Penetrating_17, Penetrating_18, Penetrating_19, Penetrating_20,
                                                                    Penetrating_21, Penetrating_22, Penetrating_23, Penetrating_24, Penetrating_25,
                                                                    Penetrating_26, Penetrating_27, Penetrating_28, Penetrating_29, Penetrating_30,
                                                                    Penetrating_31, Penetrating_32, Penetrating_33, Penetrating_34, Penetrating_35,
                                                                    Penetrating_36, Penetrating_37, Penetrating_38, Penetrating_39, Penetrating_40,
                                                                    Penetrating_41, Penetrating_42, Penetrating_43, Penetrating_44, Penetrating_45,
                                                                    Penetrating_46, Penetrating_47, Penetrating_48, Penetrating_49, Penetrating_50))

  Penetrating_cohort <- filter(Penetrating_cohort, inj_dominant == 2)

  library(table1)

                  label(Penetrating_cohort$NISS)              <- "New Injury Severity Score"
                  label(Penetrating_cohort$pre_sbp_value)    <- "Pre Hospital Systolic BP"
                  label(Penetrating_cohort$ed_sbp_value)     <- "Emergency Department Systolic BP"

                  units(Penetrating_cohort$pre_sbp_value)  <- "mmHg"
                  units(Penetrating_cohort$ed_sbp_value)   <- "mmHg"

                  table1(~ + NISS + pre_sbp_value + ed_sbp_value | data=SevereTBI_cohort1)
