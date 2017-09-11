rm(list = ls())
library(corrplot)
library(plotly)
library(dplyr)
library(reshape2)
library(nFactors)
library(nnet)
library(stargazer)
library(Metrics)
library(rattle)
library(rpart)
library(randomForest)
library(ggplot2)
library(xgboost)
library(DT)
library(pROC)
library(performanceEstimation)
library(rCharts)
library(highcharter)
inputfolder<-paste0(getwd(),"/input/")
outputfolder<-paste0(getwd(),"/output/")
train = read.csv(paste0(inputfolder,"train.csv"))
test = read.csv(paste0(inputfolder,"test.csv"))
var_kind<-c("Product_Info_", "Ins_Age", "Ht", "Wt","BMI","Employment_Info_","InsuredInfo_",
            "Insurance_History_", "Family_Hist_","Medical_History_", "Medical_Keyword_")

########## remove variables with excess NAs in both test and train#######
rmNAvars<-function(dat,threshold){
  dat<-dat[, -which(colMeans(is.na(dat)) > threshold)]
}
train_clean<-rmNAvars(train,0.3)
test_clean<-test[,intersect(colnames(test), colnames(train_clean))]

################### replacing Missing value with median ###########
manage_na <- function(datafra)
{
  for(i in 1:ncol(datafra))
  {
    if(is.numeric(datafra[,i]))
    {
      datafra[is.na(datafra[,i]),i] <- median(datafra[!is.na(datafra[,i]),i])
    }
  }
  datafra
}
train_clean <- manage_na(train_clean)
test_clean <- manage_na(test)
train_conti<-train_clean[,c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI",
                            "Employment_Info_1", "Employment_Info_4", "Employment_Info_6")] 


####### converting nonnumeric column to numeric ####
train_clean[, !(sapply(train_clean, class) == "numeric" | sapply(train_clean, class) == 
                  "integer")]<-
  as.numeric(train_clean[, !(sapply(train_clean, class) == "numeric" | 
                               sapply(train_clean, class) == "integer")])

### Type of variables
temp1<- data.frame(Variable_Type = c("Product Information",
                                     "Insurance Age",
                                     "Height",
                                     "Weight", 
                                     "BMI",
                                     "Employment Information",
                                     "Insured Information",
                                     "Insurance History",
                                     "Family History",
                                     "Medical History",
                                     "Medical Keyword"))

temp1$Continous<-c(1,1,1,1,1,3,0,1,4,0,0)
temp1$Categorical<-c(6,0,0,0,0,3,7,8,1,41,0)
temp1$Dummy<-c(0,0,0,0,0,0,0,0,0,0,48)
temp1$Total<-rowSums(temp1[,-1])
temp1[12,2:5]<-colSums(temp1[,-1])
temp1$Variable_Type[12]<-"Total"
datatable(temp1, options = list(pageLength = 13,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}")))

### Distribution of Response Variable
hchart(as.character(train$Response), type = "pie")%>%
  hc_title(text = "Distribution of Response Variable across its range")

### Missing percentage on a particular data
missing_prct<-data.frame(variable=colnames(train),missing=sapply(train,function(x)
{sum(is.na(x))}/nrow(train)))
missing_prct_test<-data.frame(variable=colnames(test),missing=sapply(test,function(x)
{sum(is.na(x))}/nrow(test)))
## Generating Summary Table

summ_conti<-data.frame(Variables =  colnames(train_conti))
summ_conti$Min<-apply(train_conti,2,function(x){min(x, na.rm = T)})
summ_conti$Max<-apply(train_conti,2,function(x){max(x, na.rm = T)})
summ_conti$Mean<-apply(train_conti,2,function(x){mean(x, na.rm = T)})
summ_conti$Median<-apply(train_conti,2,function(x){median(x, na.rm = T)})
datatable(summ_conti, options = list(initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")))

### Box Plot
plt<-htmltools::tagList()
temp1<-train_conti[1:5]
p<-plot_ly( data=melt(temp1), type = "box",
            split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")
plt[[1]] <- as_widget(p)
p<-plot_ly( data=melt(train_conti[,6:length(train_conti)]), type = "box",
            split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")
plt[[2]] <- as_widget(p)
plt

### Density Plots

hcdensity(train_conti[,1], area = TRUE, name= colnames(train_conti)[1])%>%
  hc_add_series(density(train_conti[,2]), area = TRUE, name= colnames(train_conti)[2])

hcdensity(train_conti[,3], area = TRUE, name= colnames(train_conti)[3])%>%
  hc_add_series(density(train_conti[,4]), area = TRUE, name= colnames(train_conti)[4])%>%
  hc_add_series(density(train_conti[,5]), area = TRUE, name= colnames(train_conti)[5])

hcdensity(train_conti[,6], area = TRUE, name= colnames(train_conti)[6])%>%
  hc_add_series(density(train_conti[,8]), area = TRUE, name= colnames(train_conti)[8])

hcdensity(train_conti[,7], area = TRUE, name= colnames(train_conti)[7])

### Missing Value

par(mfrow=c(2,2))
for(i in var_kind){
  plot(x=as.factor(as.character(missing_prct[grep(i, row.names(missing_prct)),1])),
       y=missing_prct$missing[grep(i, row.names(missing_prct))], ylim = c(0,1),
       main =gsub("_"," ",i))
  
}
### Missing vs Response Chart
train_na_response <- sapply(sort(unique(train$Response)), function(x) { 
  apply(train[train$Response == x, ], 2, function(y) { sum(is.na(y)) }) })
train_na_response<-data.frame(train_na_response)
train_na_response<-train_na_response[which(rowSums(train_na_response)>0),]
train_na_response$ID<-rownames(train_na_response)
train_na_response_melt<-melt(train_na_response)
nPlot(value ~ ID, group = "variable", data = train_na_response_melt,
      type = "multiBarChart")

### Event Rate Charts

train_categ<-train_clean[,-which(colnames(train_clean) %in% colnames(train_conti))]
nPlot(Freq ~ Var1, group = "Var2", data = data_freq,
      type = "multiBarChart")
i="Product_Info"
train_temp<-train_categ[,grep(i,colnames(train_categ))]
index<-1
plt<-c()
for (i in colnames(train_temp)){
  data_freq<-as.data.frame(table(train_temp[,i],train_clean$Response)/(as.data.frame(table(train_temp[,i]))[,2]))
  nPlot(Freq ~ Var1, group = "Var2", data = data_freq,
           type = "multiBarChart")
  #p
}
plt