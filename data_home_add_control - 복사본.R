#data_가정학습_통제변수까지 포함해보기
library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)
#setwd("C:/Users/scele/OneDrive/Desktop/child_social development/data_w3-w5")
getwd()
  
#age1_attachment

#dat_atch = read_csv("w2_2009_data2_220324.csv")
#dat_atch %>% select(GCh09qst000a) -> CR_atch

# age2_Wave3_외국어학습여부 or(DIn10chc051jk,  DCh10cht015)

# 시작시기DCh10cht018 # 학습도구DCh10cht016

# 이용시간_DCh10cht020, DIn10chc051j,DIn10chc051k(주당 총 ()분)
# SES_DHu10ses006 (만원/월)
# 기질_ECh10tmp000

dat1_home = read_csv("w3_2010_data_220603.csv")

dat1_home %>% select(N_ID,DCh10dmg001, DHu10ses006, DIn10chc002j,DIn10chc002k, DCh10cht015, DIn10chc051jk,
                     DCh10cht016a, DCh10cht016b, DCh10cht016c, DCh10cht016d, 
                     DCh10cht016e, DCh10cht016f, DCh10cht016g, DCh10cht016h, DCh10cht016i, DCh10cht016j,
                     DCh10cht018a, DCh10cht018b, DCh10cht018c, DCh10cht018d,
                     DCh10cht018e, DCh10cht018f, DCh10cht018g, DCh10cht018h, DCh10cht018i, DCh10cht018j,
                     DCh10cht018a:DCh10cht018j,
                     DIn10chc002j,DIn10chc002k,
                     DCh10cht020,
                     DIn10chc051j,DIn10chc051k,
                     ECh10psa001:ECh10psa005) -> CR1_home


#age3_wave4 #기질_ECh11tmp #또래관계_HIn11psa
dat2_home = read_csv("w4_2011_data_230411.csv")
dat2_home %>% select(ECh10tmp001:ECh10tmp020, 
                EMt11psa010:EMt11psa039,
                HIn11psa010:HIn11psa039) -> CR2_home

#age4_wave5
dat3_home = read_csv("w5_2012_data_230411.csv")

dat3_home %>% select(HCh12psa010:HCh12psa039) -> CR3_home



CR_home = cbind(CR1_home,CR2_home,CR3_home)
#CR_home2 = cbind(CR1_home,CR2_home,CR3_home, CR_atch)

CR_home %>% apply(2,
             function(x) ifelse (x==99999999|x==88888888,NA,x)) %>% as.data.frame() -> KCR_home
#CR_home2 %>% apply(2,
#                  function(x) ifelse (x==99999999|x==88888888,NA,x)) %>% as.data.frame() -> KCR_home2

# reverse_temper
reverse = c("ECh10tmp001", "ECh10tmp007", "ECh10tmp014", "ECh10tmp016", "ECh10tmp017")
KCR_home[, reverse] = 6 - KCR_home[, reverse]
#KCR_home2[, reverse] = 6 - KCR_home2[, reverse]

# sum of temper_tmp4
KCR_home %>% 
  mutate(tmp4 = rowSums(cur_data()[,c("ECh10tmp001","ECh10tmp003","ECh10tmp005", "ECh10tmp008", 
                                    "ECh10tmp010", "ECh10tmp012", "ECh10tmp014",
                                     "ECh10tmp016", "ECh10tmp018", "ECh10tmp020")],na.rm=T)) -> KCR_atmp

# 성별 코드 변경 female=1, male=0
KCR_atmp[, "DCh10dmg001"] = KCR_atmp[, "DCh10dmg001"] - 1 


#time
#KCR_home %>% as.data.frame() -> KCR_hd

#sum times 80분 이상인 사람만_DCh10cht020 + DIn10chc051j + DIn10chc051k
KCR_home %>% 
  mutate(Ttime = rowSums(cur_data()[,c("DCh10cht020","DIn10chc051j","DIn10chc051k")],na.rm=T)) %>%
  mutate(marker = ifelse(Ttime >=80,Ttime,0)) %>%
  mutate(lan = rowSums(cur_data()[,c("DIn10chc051jk","DCh10cht015")],na.rm=T)) %>%
  mutate(lang = recode(lan,'2'= 1),.keep = "unused") -> KCR_el

#시간 전체 사용
KCR_atmp %>% 
  mutate(Ttime = rowSums(cur_data()[,c("DCh10cht020","DIn10chc051j","DIn10chc051k")],na.rm=T)) %>%
  mutate(lan = rowSums(cur_data()[,c("DIn10chc051jk","DCh10cht015")],na.rm=T)) %>%
  mutate(lang = recode(lan,'2'= 1),.keep = "unused") -> KCR_atmp2

#KCR_el2 #애착 상관점수 포함
#KCR_home2 %>% 
#  mutate(Ttime = rowSums(cur_data()[,c("DCh10cht020","DIn10chc051j","DIn10chc051k")],na.rm=T)) %>%
#  mutate(marker = ifelse(Ttime >=80,Ttime,0)) %>%
#  mutate(lan = rowSums(cur_data()[,c("DIn10chc051jk","DCh10cht015")],na.rm=T)) %>%
#  mutate(lang = recode(lan,'2'= 1),.keep = "unused") -> KCR_el2

#min -> hour
KCR_atmp2[, "Ttime"] = KCR_atmp2[, "Ttime"] / 60

#End!!
str(KCR_atmp2)
#description
describe(KCR_atmp2)
mean(KCR_atmp2$Ttime)
summary(KCR_atmp2)
          
#reformulate_simplified
#model1_language exposure[0,1]

mod_home1 = 'W4 =~ 1*HIn11psa010 + a1*HIn11psa029 + a2*HIn11psa031 + a3*HIn11psa039 
                  + 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a3*HCh12psa039
            D4_5 =~ 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a3*HCh12psa039
            D4_5 ~ lang
            D4_5 ~~ W4
            
            HIn11psa010 ~~ e1*HIn11psa010
            HIn11psa029 ~~ e2*HIn11psa029
            HIn11psa031 ~~ e3*HIn11psa031
            HIn11psa039 ~~ e4*HIn11psa039
            HCh12psa010 ~~ e1*HCh12psa010
            HCh12psa029 ~~ e2*HCh12psa029
            HCh12psa031 ~~ e3*HCh12psa031
            HCh12psa039 ~~ e4*HCh12psa039
            
            W4 ~ 1
            D4_5 ~ 1
            HIn11psa010 ~ 0
            HIn11psa029 ~ i1*1
            HIn11psa031 ~ i2*1
            HIn11psa039 ~ i3*1
            HCh12psa010 ~ 0
            HCh12psa029 ~ i1*1
            HCh12psa031 ~ i2*1
            HCh12psa039 ~ i3*1
            '

fit_home1 = sem(model = mod_home1, estimator = "ML", missing = "fiml", 
               data = KCR_atmp2, meanstructure = T)
summary(fit_home1, fit.measures = T, standardized = T)
semPaths(fit_home1, what = 'est', style = 'lisrel')

fit_home1 %>% modificationindices() %>% arrange(mi) #modification
#HCh12psa010 ~~ HCh12psa029 이 친구 없이 되나 보기

#model2_english exposure time(minute)
mod_home2 = 'W4 =~ 1*HIn11psa010 + a1*HIn11psa029 + a2*HIn11psa031 + a3*HIn11psa039 
                  + 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a3*HCh12psa039
                  
            D4_5 =~ 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a3*HCh12psa039
            
            D4_5 ~ Ttime
            D4_5 ~~ W4

            
            HIn11psa010 ~~ e1*HIn11psa010
            HIn11psa029 ~~ e2*HIn11psa029
            HIn11psa031 ~~ e3*HIn11psa031
            HIn11psa039 ~~ e4*HIn11psa039
            HCh12psa010 ~~ e1*HCh12psa010
            HCh12psa029 ~~ e2*HCh12psa029
            HCh12psa031 ~~ e3*HCh12psa031
            HCh12psa039 ~~ e4*HCh12psa039
            
            W4 ~ 1
            D4_5 ~ 1
            HIn11psa010 ~ 0
            HIn11psa029 ~ i1*1
            HIn11psa031 ~ i2*1
            HIn11psa039 ~ i3*1
            HCh12psa010 ~ 0
            HCh12psa029 ~ i1*1
            HCh12psa031 ~ i2*1
            HCh12psa039 ~ i3*1
            '
fit_home2 = sem(model = mod_home2, estimator = "ML", missing = "FIML", 
                data = KCR_atmp2, meanstructure = T)
summary(fit_home2, fit.measures = T, standardized = T)
semPaths(fit_home2, what = 'est', style = 'lisrel')

fit_home2 %>% modificationindices() %>% arrange(mi) #modification


# anova(fit_home1,fit_home2,method = "LRT")
#model3_english exposure time(minute) #sex #attachment
mod_home3 = 'W4 =~ 1*HIn11psa010 + a1*HIn11psa029 + a2*HIn11psa031 + a3*HIn11psa039 
                  + 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a3*HCh12psa039
                  
            D4_5 =~ 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a3*HCh12psa039
            W4 ~ DCh10dmg001 
            D4_5 ~ Ttime + DHu10ses006
            D4_5 ~~ W4
            
            HIn11psa010 ~~ e1*HIn11psa010
            HIn11psa029 ~~ e2*HIn11psa029
            HIn11psa031 ~~ e3*HIn11psa031
            HIn11psa039 ~~ e4*HIn11psa039
            HCh12psa010 ~~ e1*HCh12psa010
            HCh12psa029 ~~ e2*HCh12psa029
            HCh12psa031 ~~ e3*HCh12psa031
            HCh12psa039 ~~ e4*HCh12psa039
            
            W4 ~ 1
            D4_5 ~ 1
            HIn11psa010 ~ 0
            HIn11psa029 ~ i1*1
            HIn11psa031 ~ i2*1
            HIn11psa039 ~ i3*1
            HCh12psa010 ~ 0
            HCh12psa029 ~ i1*1
            HCh12psa031 ~ i2*1
            HCh12psa039 ~ i3*1
            '
fit_home3 = sem(model = mod_home3, estimator = "ML", missing = "fiml", 
                data = KCR_atmp2, meanstructure = T)

summary(fit_home3, fit.measures = T, standardized = T)

semPaths(fit_home3, what = 'est', style = 'lisrel')

#last_model_time(h/month) & control_sex & control_SES predict English & temper predict W4
mod_fin = 'W4 =~ 1*HIn11psa010 + a1*HIn11psa029 + a2*HIn11psa031 + a4*HIn11psa021
                  + 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a4*HCh12psa021
            D4_5 =~ 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a4*HCh12psa021
            D4_5 ~ Ttime
            D4_5 ~~ W4
            W4 ~ DCh10dmg001
            Ttime ~ DHu10ses006
            
            HIn11psa010 ~~ e1*HIn11psa010
            HIn11psa029 ~~ e2*HIn11psa029
            HIn11psa031 ~~ e3*HIn11psa031
            HIn11psa021 ~~ e5*HIn11psa021
            HCh12psa010 ~~ e1*HCh12psa010
            HCh12psa029 ~~ e2*HCh12psa029
            HCh12psa031 ~~ e3*HCh12psa031
            HCh12psa021 ~~ e5*HCh12psa021
            
            W4 ~ 1
            D4_5 ~ 1
            HIn11psa010 ~ 0
            HIn11psa029 ~ i1*1
            HIn11psa031 ~ i2*1
            HIn11psa021 ~ i4*1
            HCh12psa010 ~ 0
            HCh12psa029 ~ i1*1
            HCh12psa031 ~ i2*1
            HCh12psa021 ~ i4*1
            '

fit_fin = sem(model = mod_fin, estimator = "ML", missing = "fiml", 
              data = KCR_atmp2, meanstructure = T)
summary(fit_fin, fit.measures = T, standardized = T)
semPaths(fit_fin, what = 'est', style = 'lisrel')

#add_model_time(h/month) & control_sex & control_SES predict English & control_education
mod_add = 'W4 =~ 1*HIn11psa010 + a1*HIn11psa029 + a2*HIn11psa031 + a4*HIn11psa021
                  + 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a4*HCh12psa021
            D4_5 =~ 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a4*HCh12psa021
            D4_5 ~ Ttime
            D4_5 ~~ W4
            W4 ~ DCh10dmg001
            Ttime ~ DHu10ses006 + DMt10dmg014
            
            HIn11psa010 ~~ e1*HIn11psa010
            HIn11psa029 ~~ e2*HIn11psa029
            HIn11psa031 ~~ e3*HIn11psa031
            HIn11psa021 ~~ e5*HIn11psa021
            HCh12psa010 ~~ e1*HCh12psa010
            HCh12psa029 ~~ e2*HCh12psa029
            HCh12psa031 ~~ e3*HCh12psa031
            HCh12psa021 ~~ e5*HCh12psa021
            
            W4 ~ 1
            D4_5 ~ 1
            HIn11psa010 ~ 0
            HIn11psa029 ~ i1*1
            HIn11psa031 ~ i2*1
            HIn11psa021 ~ i4*1
            HCh12psa010 ~ 0
            HCh12psa029 ~ i1*1
            HCh12psa031 ~ i2*1
            HCh12psa021 ~ i4*1
            
            HCh12psa010 ~~ HCh12psa029
            '

fit_add = sem(model = mod_add, estimator = "ML", missing = "fiml", 
              data = KCR_atmp4, meanstructure = T)
summary(fit_add, fit.measures = T, standardized = T)
semPaths(fit_add, what = 'est', style = 'lisrel')

#latent change score model_sum score
{
# write.csv(KCR_atmp2, 'lcmdd.csv')
read.csv("lcmdd.csv") -> KCR
#peer relationship sumscore
KCR %>% 
  mutate(W4pr = rowSums(cur_data()[,c("HIn11psa039","HIn11psa031","HIn11psa033",
                                      "HIn11psa010","HIn11psa021","HIn11psa038","HIn11psa029","HIn11psa036")],na.rm=T)) %>%
  mutate(W5pr = rowSums(cur_data()[,c("HCh12psa039","HCh12psa021","HCh12psa031",
                                      "HCh12psa033","HCh12psa010","HCh12psa038","HCh12psa036","HCh12psa029")],na.rm=T)) -> lcmdata
#lcm1: no predictor
no.predic.model <- 'W5pr ~ 1*W4pr
                    d =~ 1*W5pr
                   '
fit.no.pre <- sem(model = no.predic.model, estimator="ML", missing = "fiml",
                  data=lcmdata, meanstructure = T)
summary(fit.no.pre)
#lcm2: predictor_language
step1 <- 'W5pr ~ 1*W4pr
          d =~ 1*W5pr + W4pr
          d ~ Ttime
         '
fit.step1 <- sem(model=step1, estimator="ML", missing = "fiml",
                 data=lcmdata, meanstructure = T)
summary(fit.step1)#n.s.

#add control
#lcm2: predictor_language
step3 <- 'W5pr ~ 1*W4pr
          W4pr ~ DCh10dmg001 + tmp4
          d =~ 1*W5pr + W4pr
          d ~ Ttime
          Ttime ~ DHu10ses006
         '
fit.step3 <- sem(model=step1, estimator="ML", missing = "fiml",
                 data=lcmdata, meanstructure = T)
summary(fit.step3)#n.s.

KCR_atmp2 %>% 
  mutate(W4pr = rowSums(cur_data()[,c("HIn11psa039","HIn11psa031","HIn11psa033",
                                      "HIn11psa010","HIn11psa021")],na.rm=T)) %>%
  mutate(W5pr = rowSums(cur_data()[,c("HCh12psa039","HCh12psa021","HCh12psa031",
                                      "HCh12psa033","HCh12psa010")],na.rm=T)) -> lcmdata2
stept <- 'W5pr ~ 1*W4pr
          d =~ 1*W5pr
          d ~ Ttime
         '
fit.stept <- sem(model=step1, estimator="ML", missing = "fiml",
                 data=lcmdata2, meanstructure = T)
summary(fit.stept)#?!
}
#after EFA_change items
{
mod_add <- 'W4 =~ 1*HIn11psa010 + a1*HIn11psa021 + a2*HIn11psa031 + a4*HIn11psa033 + a5*HIn11psa039
                  + 1*HCh12psa010 + a1*HCh12psa021 + a2*HCh12psa031 + a4*HCh12psa033 + a5*HIn11psa039
            D4_5 =~ 1*HCh12psa010 + a1*HCh12psa021 + a2*HCh12psa031 + a4*HCh12psa033 + a5*HCh12psa039
            D4_5 ~ Ttime
            D4_5 ~~ W4
            W4 ~ DCh10dmg001 + tmp4
            Ttime ~ DHu10ses006
            
            HIn11psa010 ~~ e1*HIn11psa010
            HIn11psa021 ~~ e2*HIn11psa021
            HIn11psa031 ~~ e4*HIn11psa031
            HIn11psa033 ~~ e3*HIn11psa033
            HIn11psa039 ~~ e5*HIn11psa039
            HCh12psa010 ~~ e1*HCh12psa010
            HCh12psa021 ~~ e2*HCh12psa021
            HCh12psa031 ~~ e4*HCh12psa031
            HCh12psa033 ~~ e3*HCh12psa033
            HCh12psa039 ~~ e5*HCh12psa039
            
            W4 ~ 1
            D4_5 ~ 1
            HIn11psa010 ~ 0
            HIn11psa021 ~ i1*1
            HIn11psa031 ~ i2*1
            HIn11psa033 ~ i3*1
            HIn11psa039 ~ i4*1
            HCh12psa010 ~ 0
            HCh12psa021 ~ i1*1
            HCh12psa031 ~ i2*1
            HCh12psa033 ~ i3*1
            HCh12psa039 ~ i4*1
            '

fit_add = sem(model = mod_add, estimator = "ML", missing = "fiml", 
              data = KCR_atmp2, meanstructure = T)
summary(fit_add, fit.measures = T, standardized = T)
semPaths(fit_add, what = 'est', style = 'lisrel')
#add item23
mod_add2 <- 'W4 =~ 1*HIn11psa010 + a1*HIn11psa021 + a2*HIn11psa031 + a4*HIn11psa033 + a5*HIn11psa039 + a6*HIn11psa023
                  + 1*HCh12psa010 + a1*HCh12psa021 + a2*HCh12psa031 + a4*HCh12psa033 + a5*HCh12psa039 + a6*HCh12psa023
            D4_5 =~ 1*HCh12psa010 + a1*HCh12psa021 + a2*HCh12psa031 + a4*HCh12psa033 + a5*HCh12psa039 + a6*HCh12psa023
            D4_5 ~ Ttime
            D4_5 ~~ W4
            W4 ~ DCh10dmg001 + tmp4
            Ttime ~ DHu10ses006
            
            HIn11psa010 ~~ e1*HIn11psa010
            HIn11psa021 ~~ e2*HIn11psa021
            HIn11psa031 ~~ e4*HIn11psa031
            HIn11psa033 ~~ e3*HIn11psa033
            HIn11psa039 ~~ e5*HIn11psa039
            HIn11psa023 ~~ e6*HIn11psa023
            HCh12psa010 ~~ e1*HCh12psa010
            HCh12psa021 ~~ e2*HCh12psa021
            HCh12psa031 ~~ e4*HCh12psa031
            HCh12psa033 ~~ e3*HCh12psa033
            HCh12psa039 ~~ e5*HCh12psa039
            HCh12psa023 ~~ e6*HCh12psa023
            
            W4 ~ 1
            D4_5 ~ 1
            HIn11psa010 ~ 0
            HIn11psa021 ~ i1*1
            HIn11psa031 ~ i2*1
            HIn11psa033 ~ i3*1
            HIn11psa039 ~ i4*1
            HIn11psa023 ~ i5*1
            HCh12psa010 ~ 0
            HCh12psa021 ~ i1*1
            HCh12psa031 ~ i2*1
            HCh12psa033 ~ i3*1
            HCh12psa039 ~ i4*1
            HCh12psa023 ~ i5*1
            '

fit_add2 = sem(model = mod_add2, estimator = "ML", missing = "fiml", 
              data = KCR_atmp2, meanstructure = T)
summary(fit_add2, fit.measures = T, standardized = T)
semPaths(fit_add, what = 'est', style = 'lisrel')

#add item23_except tmp4
mod_add23 <- 'W4 =~ 1*HIn11psa010 + a1*HIn11psa021 + a2*HIn11psa031 + a4*HIn11psa033 + a5*HIn11psa039 + a6*HIn11psa023
                  + 1*HCh12psa010 + a1*HCh12psa021 + a2*HCh12psa031 + a4*HCh12psa033 + a5*HCh12psa039 + a6*HCh12psa023
            D4_5 =~ 1*HCh12psa010 + a1*HCh12psa021 + a2*HCh12psa031 + a4*HCh12psa033 + a5*HCh12psa039 + a6*HCh12psa023
            D4_5 ~ Ttime
            D4_5 ~~ W4
            W4 ~ DCh10dmg001
            Ttime ~ DHu10ses006
            
            HIn11psa010 ~~ e1*HIn11psa010
            HIn11psa021 ~~ e2*HIn11psa021
            HIn11psa031 ~~ e4*HIn11psa031
            HIn11psa033 ~~ e3*HIn11psa033
            HIn11psa039 ~~ e5*HIn11psa039
            HIn11psa023 ~~ e6*HIn11psa023
            HCh12psa010 ~~ e1*HCh12psa010
            HCh12psa021 ~~ e2*HCh12psa021
            HCh12psa031 ~~ e4*HCh12psa031
            HCh12psa033 ~~ e3*HCh12psa033
            HCh12psa039 ~~ e5*HCh12psa039
            HCh12psa023 ~~ e6*HCh12psa023
            
            W4 ~ 1
            D4_5 ~ 1
            HIn11psa010 ~ 0
            HIn11psa021 ~ i1*1
            HIn11psa031 ~ i2*1
            HIn11psa033 ~ i3*1
            HIn11psa039 ~ i4*1
            HIn11psa023 ~ i5*1
            HCh12psa010 ~ 0
            HCh12psa021 ~ i1*1
            HCh12psa031 ~ i2*1
            HCh12psa033 ~ i3*1
            HCh12psa039 ~ i4*1
            HCh12psa023 ~ i5*1
            '

fit_add23 = sem(model = mod_add23, estimator = "ML", missing = "fiml", 
               data = KCR_atmp2, meanstructure = T)
summary(fit_add23, fit.measures = T, standardized = T)

#no control
mod_add.noc <- 'W4 =~ 1*HIn11psa010 + a1*HIn11psa021 + a2*HIn11psa031 + a4*HIn11psa033 + a5*HIn11psa039 + a6*HIn11psa023
                  + 1*HCh12psa010 + a1*HCh12psa021 + a2*HCh12psa031 + a4*HCh12psa033 + a5*HCh12psa039 + a6*HCh12psa023
            D4_5 =~ 1*HCh12psa010 + a1*HCh12psa021 + a2*HCh12psa031 + a4*HCh12psa033 + a5*HCh12psa039 + a6*HCh12psa023
            D4_5 ~ Ttime
            D4_5 ~~ W4

            HIn11psa010 ~~ e1*HIn11psa010
            HIn11psa021 ~~ e2*HIn11psa021
            HIn11psa031 ~~ e4*HIn11psa031
            HIn11psa033 ~~ e3*HIn11psa033
            HIn11psa039 ~~ e5*HIn11psa039
            HIn11psa023 ~~ e6*HIn11psa023
            HCh12psa010 ~~ e1*HCh12psa010
            HCh12psa021 ~~ e2*HCh12psa021
            HCh12psa031 ~~ e4*HCh12psa031
            HCh12psa033 ~~ e3*HCh12psa033
            HCh12psa039 ~~ e5*HCh12psa039
            HCh12psa023 ~~ e6*HCh12psa023
            
            W4 ~ 1
            D4_5 ~ 1
            HIn11psa010 ~ 0
            HIn11psa021 ~ i1*1
            HIn11psa031 ~ i2*1
            HIn11psa033 ~ i3*1
            HIn11psa039 ~ i4*1
            HIn11psa023 ~ i5*1
            HCh12psa010 ~ 0
            HCh12psa021 ~ i1*1
            HCh12psa031 ~ i2*1
            HCh12psa033 ~ i3*1
            HCh12psa039 ~ i4*1
            HCh12psa023 ~ i5*1
            '

fit_add.noc = sem(model = mod_add.noc, estimator = "ML", missing = "fiml", 
               data = KCR_atmp2, meanstructure = T)
summary(fit_add.noc, fit.measures = T, standardized = T)
}