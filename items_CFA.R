#peer relationship items CFA
Cpeer = 'social4 =~ 1*HIn11psa010 + a1*HIn11psa029 + a2*HIn11psa031  + a3*HIn11psa021
          ideal4 =~ 1*HIn11psa027 + b1*HIn11psa033 + b2*HIn11psa036 + b3*HIn11psa038 + b4*HIn11psa039
          
        '

mod_peer <- cfa(Cpeer, data = KCR_atmp3)
summary(mod_peer, fit.measures = TRUE, standardized = TRUE, rsquare = T)

mod_peer %>% modificationindices() %>% arrange(mi)

Cpeer_5 = 'social5 =~ HCh12psa010 + HCh12psa029 + HCh12psa031 + HCh12psa021
          ideal5 =~ HCh12psa027 + HCh12psa033 + HCh12psa036 + HCh12psa038  + HCh12psa039
          
          HCh12psa010 ~~ HCh12psa029'

mod_peer2 <- cfa(Cpeer_5, data = KCR_atmp3)
summary(mod_peer2, fit.measures = TRUE, standardized = TRUE, rsquare = T)

Cpeer_all = 'social4 =~ 1*HIn11psa010 + a1*HIn11psa029 + a2*HIn11psa031  + a3*HIn11psa021
          ideal4 =~ 1*HIn11psa027 + b1*HIn11psa033 + b2*HIn11psa036 + b3*HIn11psa038
          social5 =~ 1*HCh12psa010 + a1*HCh12psa029 + a2*HCh12psa031 + a3*HCh12psa021
          ideal5 =~ 1*HCh12psa027 + b1*HCh12psa033 + b2*HCh12psa036 + b3*HCh12psa038
          
          HCh12psa010 ~~ HCh12psa029'
mod_peerall <- cfa(Cpeer_all, data = KCR_atmp3)
summary(mod_peerall, fit.measures = TRUE, standardized = TRUE, rsquare = T)

mod_peerall %>% modificationindices() %>% arrange(mi)

######description######

#전체 기술통계
summary(KCR_atmp3)
describe(KCR_atmp3)
describe(KCR_atmp3$Ttime)
#성별
table(KCR_atmp3$DCh10dmg001)
tbls <- table(KCR_atmp3$DCh10dmg001)
glimpse(tbls)
glimpse(prop.table(tbls))*100
#영어 학습 유무 비율
table(KCR_atmp3$lang)
tbl <- table(KCR_atmp3$lang)
glimpse(tbl)
glimpse(prop.table(tbl))
round(prop.table(tbl)*100,2)
