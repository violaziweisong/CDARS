#find the first diagnosis date
library(stringr)
library(dplyr)
library(magrittr)
library(haven)
library(data.table)
#diagnosis data
data=read.csv("C:/Users/Viola/Desktop/merged/dm/combined/dx/all_dx_withold.csv")
names(data)[1]="ReferenceKey"
dat=as.data.table(data)
#dm cohort
dm=read.csv("C:/Users/Viola/Desktop/merged/dm/shortdm.csv")
names(dm)[1]="ReferenceKey"
dmm=dm

setwd("C:/Users/Viola/Desktop/merged/dm/diagnosis")
path=paste(getwd(),"/diagnosis code",sep = "")
file <- dir(path)
disname=file%>%{gsub(".sps", "",.)}%>%{gsub(" ", "_",.)}
code=matrix(0,ncol=4,nrow=length(file))%>%`colnames<-`(c("orignal","start","end","criteria"))

############################################################################################
#Diagnosis code
for (i in 1:length(file)) {
  code[i,1]=paste(read.csv(paste(path,file[i],sep="/"),header = F),collapse = "")%>%
    {gsub("\", \"","",.)}%>%{gsub("\", \n\"","",.)}%>%{gsub("=","==",.)}%>%{gsub("and","&",.)}%>%{gsub("or","|",.)}%>%{gsub(">==",">=",.)}%>%{gsub("<==","<=",.)}
  code[i,2]=regexpr('SELECT.IF..',code[i])[1]+11
  code[i,3]=regexpr(')\\.',code[i])[1]-1
  code[i,4]=substr(code[i,1],code[i,2],code[i,3])
}

for(i in 1:length(file)){
  dmm=eval(parse(text = paste("dat[",code[i,4],",]",sep="")))%>%
    group_by(ReferenceKey)%>%summarise(date= min(ReferenceDate))%>%
    setNames(.,c("ReferenceKey",paste(disname[i],"first_dx_date",sep = "_")))%>%
    right_join(dmm,by ="ReferenceKey")
  print(paste("Done:",disname[i]))
}
dmm$Baseline_date=dmm$Fatty_liver_dx_date
names(dmm) <- gsub("[.():,-]", "", names(dmm))
names(dmm) <- gsub("'", "", names(dmm))

write_sav(dmm, "dm_with_dx_date.sav")




