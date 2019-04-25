
options(java.parameters = "- Xmx1024m")
lapply(c('dplyr',
         'data.table',
         'reshape',
         'XLConnect',
         'rowr',
         'lubridate'), require, character.only=T)

#값진 교훈을 얻었다. 내 꿈을 말하려면, 기본이 되어있어야 한다. 학문적 기본을 다져야 내 꿈을 논할 자격이 있다
#어제는 멘탈이 나가서 상황 파악이 어려웠다. 시간이 지나니까 점점 현실적인 대안을 생각하게 된다.
#지금 내 상황을 제 3자가 객관적으로 판단했을 때 저런 생각이 드는 것. 정말 소중한 피드백이다. 
#피드백에 맞춰 내 입지를 다시 다져야 한다. 
#
read.xls <- function(filename, sheetnumber=1, sheetname=NULL, forceConversion=TRUE, startCol=0,  stringsAsFactors=TRUE) {
  wb <- loadWorkbook(filename)
  if (is.null(sheetname)) sheetname = getSheets(wb)[sheetnumber]
  df <- readWorksheet(wb, sheet=sheetname, forceConversion=forceConversion, startCol=startCol)
  if (stringsAsFactors) {
    ischar <- sapply(df, class) == "character"
    for (i in 1:length(df)) {
      if (ischar[i]) df[,i] <- factor(df[,i])
    }
  }
  df
}

levct <- function(data, level){
  data <- data %>% as.data.frame()
  sf <- sapply(data, FUN = is.factor) %>% as.data.frame()
  ss <- sapply(sapply(data, levels), length) %>% as.data.frame()
  rn <- rownames(sf)[sf[,1] == 'TRUE' & ss[,1] < as.numeric(level) ]
  
  res <- c()
  for(i in seq_along(rn)){
    cb <- data[,rn[i]] %>% table() %>% as.data.frame() %>% arrange(desc(.)) %>% rename(c(.=rn[i]))
    res <- cbind.fill(res, cb, fill=NA)
  }
  res <- res[,-1]
  return(res)
}


setwd('C:\\Users\\sseo\\Desktop\\R\\HF')
hf <- read.xls('KORAHF_ML_FINAL_Dataset.xlsx', sheetnumber = 1)
hf2 <- read.csv('KORAHF_0227_death.csv')

levct(hf, 100) %>% View('level')

c.hf <- colnames(hf)
c.hf2 <- colnames(hf2)
cbind(c.hf, c.hf %in% c.hf2) %>% 
  as.data.frame() %>%
  mutate(c.hf = as.character(c.hf),
         V2 = as.logical(V2)) %>%
  mutate(c.hf2 = ifelse(V2, c.hf, NA)) %>%
  select(-V2) %>%
  View('colnames')

c.hf2[!c.hf2 %in% c.hf]






hf %>%
  mutate(SUBJNO = as.factor(substr(as.character(SUBJNO), 7,9)),
         SEX = as.factor(SEX),
         Lung_Cong = as.factor(Lung_Cong),
         LV_dysf = as.factor(LV_dysf),
         HF_new = as.factor(HF_new),
         HF_type = as.factor(HF_type),
         Adm_route = as.factor(Adm_route),
         Adm_otherhos = as.factor(Adm_otherhos),
         Adm_otherdep = as.factor(Adm_otherdep)) %>%
  select(-c(3:5,10),
         -c(which(rowSums(is.na(t(hf))) == nrow(hf)) %>% names()),
         -HF_explain,
         -Dx_date,
         -C2C__2,
  )


#사용하는 변수 중 numeric
hf %>% select(SEX, AGE, Ht, Wt, BSA, BMI, SBP, DBP, HR, TC, albumin:glucose, hsCRP_CRP, CK_MB, cTnI, LVEDD:LVEF_vol,
              LVEF_M:LA_dia, discharge_SBP:discharge_HR, )

#사용하는 변수 중 numeric인데 factor로 고쳐야하는 것
hf %>% select(Instituaion_Code, Lung_Cong, LV_dysf, HF_new, HF_type, Adm_route, Adm_otherhos, Adm_otherdep,
              HTN:malignancy, firstHTN:CRT_yn, smoking, alcohol, NYHA, Afib_ECG, SR_AF_TM, VT,EKG_Qwave:EKG_IVCD,
              NoDM_DM_7.0, NP_decile, Etiol_HF, HF_Agg_ACS:HF_Agg_UK, HF_Agg_numb, 
              med.numb,
              Med_DM_insulin:Med_DM_meglitinide, Med_DM_num, Med_IV_diuretics:Med_IV_NG, Med_IV_number,
              IVmed_inotropics, MechVentil, AssDev_number, 
              renalRT2,
              transfusion, discharge_date, discharge_state, discharge_NYHA,
              ICUAdm_times2,
)
#HF_Agg_ACS:HF_Agg_UK는 affravating factor of HF를 dummy var로 나타낸 것. 복합적인 요인이 많아 이렇게 표시함.
#분석할 때, 이 dummy를 포함시키거나 / 복합 요인 개수(HF_Agg_numb)만 포함시키거나 해야할 듯
#Med 변수는 어떤 약을 어떤 시기에 얼마나 복용했는지 자잘하게 나타낸 것. 모든 사람이 모든 약을
#복용하지 않았기 때문에 결측값이 많아 불필요한 변수, 정보를 제거할 필요가 있다. 이를 축소시킨다면, 
#약을 몇 개 복용해본적 있는지? 어떤 약을 복용했는지가 환자 그룹을 나누는데 유의할까?
#아니면 얼마나 다양한 약을 복용했는지가 환자 그룹을 나누는데 유의할까?
med.numb <- hf %>%
  transmute(Med_ACEI = ifelse(any(Med_ACEI_befAdm==1, Med_ACEI_durAdm==1, Med_ACEI_dis==1), 1, 0),
            Med_ARB = ifelse(any(Med_ARB_befAdm==1, Med_ARB_durAdm==1, Med_ARB_dis==1), 1, 0),
            Med_BB = ifelse(any(Med_BB_befAdm==1, Med_BB_durAdm==1, Med_BB_dis==1), 1, 0),
            Med_AA = ifelse(any(Med_AA_befAdm==1, Med_AA_durAdm==1, Med_AA_dis==1), 1, 0),
            Med_nitrate = ifelse(Med_nitrate == 3, 0, 1),
            Med_hydralazine = ifelse(Med_hydralazine ==3, 0, 1),
            Med_LoopD = ifelse(Med_LoopD==3, 0, 1),
            Med_Thiazide = ifelse(Med_LoopD==3, 0, 1),
            Med_Amiodarone = ifelse(Med_Amiodarone==3, 0, 1),
            Med_digoxin = ifelse(Med_digoxin==3, 0, 1),
            Med_HepLMWH = ifelse(Med_HepLMWH==3, 0, 1),
            Med_Warfarin = ifelse(Med_Warfarin==3, 0, 1),
            Med_asp = ifelse(Med_asp==3, 0, 1),
            Med_statin = ifelse(Med_statin==3, 0, 1),
            Med_Ivabradine = ifelse(Med_Ivabradine==3, 0, 1),
            Med_dronedarone = ifelse(Med_dronedarone==3, 0, 1)) %>%
  apply(., 1, sum)
#IVmed_diuretics는 Med_IV_diuretics와 같고, IVmed_vasodilater는 Med_IV_NG와 매우 같다. IVmed_inotropics는
#변수명만 수정해서 사용.
#AssistingDev부분은 몇개 device를 사용했는지만 봄. 약과 달리 device는 어떤 장치를 썼는지가 군집에 큰 영향을 
#안 미칠것 같아서? 일단 여러 방법으로 분석해봐야함.
#걸러낸 변수의 factor level 통일시켜야함. 1이 사용, 2가 미사용 이런식으로.
hf %>%
  mutate(renalf = factor(cbind(renalRT_HD, renalRT_CRRT, renalRT_other) %*% 1:3 +1, labels=1:5)) %>%
  mutate(renalRT2 = ifelse(renalRT == 1, renalf, 0)) %>%
  select(renalRT, renalRT2) %>%
  View()
#1:HD,CRRT,other 2:CRRT,other 3:HD,CRRT 4:CRRT 5:HD
hf %>%
  mutate(ICUAdm_times = ifelse(ICUAdm==1, ICUAdm_times, 0))

hf %>%
  mutate(Afib = ifelse(Afib_duringAdm==1, Afib_type, 0))
#0:No 1:Paroxysmal 2:Permanent or persistent AF

hf %>%
  mutate(sinus_conv = ifelse(sinus_conv==1, sinus_conv_type, 0))
#0:No 1:자발적 2:약물사용에 의해서 3:DC cardioversion에 의해서

hf %>%
  mutate(OtherTreatment2 = factor(as.matrix(hf[,328:336]) %*% 1:9 +1 )) %>%
  mutate(OtherTreatment3 = ifelse(OtherTreatment==1, OtherTreatment2, 0)) %>%
  select(OtherTreatment, OtherTreatment3) %>%
  select(OtherTreatment3) %>% table()


#사용하는 변수 중 factor
hf %>% select(SUBJSTAT, Adm_date, EKG_QRSdur:EKG_HR, WBC:Platelet, )

#결측치가 많지만 유진썜이 사용한 변수 - imputation 어떻게?
hf %>% select(New_Afib, VT_type, pacing, No_BBB, No_wide_QRS:ETC_No_BBB, HbA1c, HbA1c_7_new, TG, LDLcal,
              BNP:NTproBNP_decile, cTnT, LVEF_vis, LAVI, E_wave:E_over_Eprime, ActiveBleed, )




#다음 세가지 날짜 변수는 거의 비슷한 날. Adm_date만 일단 사용. 정확히 얼마나 비슷? 차이가 있다면 파생변수로
#활용가능한지?
hf %>% select(Adm_date, Dx_date, EKG_date, lab_date, Echo_date) %>% View('a')
dmy(hf$EKG_date)

#structure
str(hf[,430:450])

#level count
hf %>%
  select(base_DC_dt:FU_ACdeath_HHF_1y_dur) %>%
  mutate_if(is.numeric, as.factor) %>%
  levct(., 50) %>%
  t() %>%
  View()

#NA count  
rowSums(is.na(t(hf)))  %>% View('NA')


hf[as.character(hf$Adm_date) != as.character(hf$Dx_date),] %>% View()









