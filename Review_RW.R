# Adverse Childhood Experiences and Adult Cardiometabolic Health in Colombia
# Independent review by Rin Wada


library(openxlsx)
library(tidyverse)

### Load data
pathways_final <- read.xlsx("~/OneDrive - Imperial College London/Datasets & script Paper 2 Co-occurrence ACE/pathways_final.xlsx")

pathways_final = pathways_final %>% mutate_if(is.character, as.factor)

dim(pathways_final)
table(pathways_final$sex)

## ACE Table 1 ----
table(pathways_final$emotional_abuse, useNA="always")
round(prop.table(table(pathways_final$emotional_abuse, useNA="always"))*100, 1)
table(pathways_final$poor_food_environment, useNA="always")
round(prop.table(table(pathways_final$poor_food_environment, useNA="always"))*100, 1)
table(pathways_final$poor_childhood_health, useNA="always")
round(prop.table(table(pathways_final$poor_childhood_health, useNA="always"))*100, 1)
table(pathways_final$domestic_violence, useNA="always")
round(prop.table(table(pathways_final$domestic_violence, useNA="always"))*100, 1)
table(pathways_final$childhood_migration, useNA="always")
round(prop.table(table(pathways_final$childhood_migration, useNA="always"))*100, 1)
table(pathways_final$early_infection, useNA="always")
round(prop.table(table(pathways_final$early_infection, useNA="always"))*100, 1)

table(pathways_final$aces_score, useNA="always")
round(prop.table(table(pathways_final$aces_score, useNA="always"))*100, 1)
table(pathways_final$Low_childhood_sep, useNA="always")
round(prop.table(table(pathways_final$Low_childhood_sep, useNA="always"))*100, 1)

table(pathways_final$emotional_abuse, pathways_final$sex, useNA="always")
round(prop.table(table(pathways_final$emotional_abuse, pathways_final$sex, useNA="always"),2)*100,1)
table(pathways_final$poor_food_environment, pathways_final$sex, useNA="always")
round(prop.table(table(pathways_final$poor_food_environment, pathways_final$sex, useNA="always"),2)*100,1)
table(pathways_final$poor_childhood_health, pathways_final$sex, useNA="always")
round(prop.table(table(pathways_final$poor_childhood_health, pathways_final$sex, useNA="always"),2)*100,1)
table(pathways_final$domestic_violence, pathways_final$sex, useNA="always")
round(prop.table(table(pathways_final$domestic_violence, pathways_final$sex, useNA="always"),2)*100,1)
table(pathways_final$childhood_migration, pathways_final$sex, useNA="always")
round(prop.table(table(pathways_final$childhood_migration, pathways_final$sex, useNA="always"),2)*100,1)
table(pathways_final$early_infection, pathways_final$sex, useNA="always")
round(prop.table(table(pathways_final$early_infection, pathways_final$sex, useNA="always"),2)*100,1)


table(pathways_final$aces_score, pathways_final$sex, useNA="always")
round(prop.table(table(pathways_final$aces_score, pathways_final$sex, useNA="always"),2)*100,1)

table(pathways_final$Low_childhood_sep, pathways_final$sex, useNA="always")
round(prop.table(table(pathways_final$Low_childhood_sep, pathways_final$sex, useNA="always"),2)*100,1)

## ACE correlation analysis ----
library(tidyverse)

mydata = pathways_final %>% select(emotional_abuse, poor_food_environment, poor_childhood_health, domestic_violence,childhood_migration, early_infection, Low_childhood_sep)

for(i in 1:ncol(mydata)){
  for(col in 1:ncol(mydata[,-i])){
    print(signif(sum(mydata[,i] == "Yes" &
                       mydata[,-i][col] == "Yes")/sum(mydata[,i] == "Yes")*100,3))
  }
}

X = apply(mydata, 2, function(x) ifelse(x == "Yes", 1, 0))

# Calculate the pearson's correlation between all pairs of ACE variables (binary)
cor = cor(X)
round(cor,2)

# Calculate the tetrachoric correlation between all pairs of ACE variables (binary)
tetrachoric(X)

## Logistic regression ----

source("functions.R")

for(cat in levels(pathways_final$sex)){
  # Load data
  mydata = pathways_final[pathways_final$sex==cat,]
  
  Z = mydata %>% select(age, ethnic_group)
  Y = mydata %>% select(CVD, HTA, Diabetes, Obesity) %>%
    mutate_if(is.factor, as.numeric) %>% -1
  
  X = mydata %>% select(aces_score, emotional_abuse, poor_food_environment,
                        poor_childhood_health, domestic_violence, childhood_migration,
                        early_infection, Low_childhood_sep)
  X = model.matrix(~ ., X)[,-1]
  
  for(m in 1:2){
    if (m==1){
      # Linear regression
      f0='Y[,k] ~ X[,j]'
    } else{
      # Linear regression
      f0='Y[,k] ~ X[,j] + age + ethnic_group'
    }
    t0=Sys.time()
    betas = se = pvals = NULL
    for (k in 1:ncol(Y)){
      for (j in 1:ncol(X)){
        model=glm(as.formula(f0), data = Z, family = "binomial")
        betas=c(betas, summary(model)$coefficients[2,1])
        se=c(se, summary(model)$coefficients[2,2])
        pvals=c(pvals, summary(model)$coefficients[2,4])
      }
    }
    betas = matrix(betas, nrow = ncol(X), ncol = ncol(Y))
    se = matrix(se, nrow = ncol(X), ncol = ncol(Y))
    pvals = matrix(pvals, nrow = ncol(X), ncol = ncol(Y))
    rownames(pvals) = rownames(betas) = rownames(se) = colnames(X)
    colnames(pvals) = colnames(betas) = colnames(se) = colnames(Y)
    
    bonf = 0.05/nrow(pvals)
    mytable = NULL
    for (c in 1:ncol(pvals)){
      beta = paste0(formatC(exp(betas[,c]), format="f", digits=2),
                    " (",formatC(exp(betas[,c]-qnorm(0.975)*se[,c]), format="f", digits=2),
                    ",",formatC(exp(betas[,c]+qnorm(0.975)*se[,c]), format="f", digits=2),")")
      pval = formatC(pvals[,c], format = "e", digits = 2)
      signif = ifelse(pvals[,c] < bonf, "*","")
      mytable = cbind(mytable,beta,pval,signif)
    }
    dim(mytable)
    mytable = rbind(rep(c("OR (95% CI)","P-value","Bonf."), ncol(pvals)), mytable)
    colnames(mytable) = rep(colnames(pvals), each = 3)
    SaveExcelWithSuperscripts(cbind(c("",rownames(pvals)), mytable),paste0("Table3_",cat,"_m",m,".xlsx"))
  }
}

## Cochran Armitage ----
library(DescTools)

for(cat in levels(pathways_final$sex)){
  print(cat)
  # Load data
  mydata = pathways_final[pathways_final$sex==cat,]
  
  Y = mydata %>% select(CVD, HTA, Diabetes, Obesity) %>%
    mutate_if(is.factor, as.numeric) %>% -1
  
  for(i in 1:ncol(Y)){
    print(colnames(Y)[i])
    test = CochranArmitageTest(table(mydata$aces_score, Y[,i]))
    print(test$p.value)
  }
}

















