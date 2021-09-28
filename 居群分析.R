#install.packages("survminer") # 安装survminer包
#install.packages("survival") # 安装survival包
#install.packages("ggpmisc"
#install.packages("gridExtra")
library(gridExtra)
library(survminer) # 加载包
library(survival) # 加载包
library(dplyr) #加载dplyr包
library(ggpmisc) #加载ggpmisc包
library(ggplot2)

# abh = subset(abh, Gen. == "Dalmanella")

exampledfpath0<-file.choose()
exampledfpath1<-file.choose()
exampledfpath2<-file.choose()

###运行这一行命令，跳出对话框，选择刚刚保存的csv格式的数据
dfS01_D<-read.csv(exampledfpath0,header=TRUE)
dfS10_D<-read.csv(exampledfpath1,header=TRUE) 
dfS11_D<-read.csv(exampledfpath2,header=TRUE)### 运行这一行命令读入数据

SurT <- function(data, no.cols) {
  col1 = data[,no.cols]
  s = max(col1)/log(101,base = exp(1))
  Life = exp(col1/s)-1
  return(Life)
}

dfS11_D$life <- SurT(dfS11_D,1)
dfS11_D$status <- 2
dfS11_D$genus <- "Dalmanella"
dfS11_D$formation <- "SBW-11-14"
dfS11_D$No.Strat <- "SBW-11"

dfS10_D$life <- SurT(dfS10_D,1)
dfS10_D$status <- 2
dfS10_D$genus <- "Dalmanella"
dfS10_D$formation <- "SBW-10"
dfS10_D$No.Strat <- "SBW-10"

dfS01_D$life <- SurT(dfS01_D,1)
dfS01_D$status <- 2
dfS01_D$genus <- "Dalmanella"
dfS01_D$formation <- "SBW-01"
dfS01_D$No.Strat <- "SBW-01"

dfS11_H$life <- SurT(dfS11_H,1)
dfS11_H$status <- 2
dfS11_H$genus <- "Hindella"
dfS11_H$formation <- "SBW-10"

D_S11_S10_S01 <- rbind(dfS11_D, dfS10_D, dfS01_D)
H_D_S11 <- rbind(dfS11_H, dfS11_D)


#频率直方图
p0 <- ggplot(dfS01_D,aes(x=b)) + 
  geom_histogram(binwidth=2,fill="#69b3a2" ,
                 color="#e9ecef", alpha=0.8)+
  
  geom_density(aes(y=..density..*n), color = "lightblue", size = 2)+
  labs(x="Width of Shell (mm)",y="Frequency",title = "A",
       subtitle = "Number of specimens:39")+
  theme(plot.title = element_text(hjust = 0)) 
p0 <- ggpar(
  p0,
  font.title    = c(20, "bold", "black"),
  font.subtitle = c(10,  "black"))


p1 <- ggplot(dfS10_D,aes(x=b)) + 
  geom_histogram(binwidth=2,fill="#69b3a2" ,
                 color="#e9ecef", alpha=0.8)+
 
  geom_density(aes(y=..density..*n), color = "lightblue", size = 2)+
  labs(x="Width of Shell (mm)",y="Frequency",title = "B",
       subtitle = "Number of specimens:52")+
  theme(plot.title = element_text(hjust = 0)) 
p1 <- ggpar(
  p1,
  font.title    = c(20, "bold", "black"),
  font.subtitle = c(10,  "black"))

#频率直方图
p2 <- ggplot(dfS11_D ,aes(x=b)) + 
  geom_histogram(binwidth=2,fill="#69b3a2" ,
                 color="#e9ecef", alpha=0.8)+
  
  geom_density(aes(y=..density..*n), color = "lightblue", size = 2)+
  labs(x="Width of Shell (mm)",y="Frequency",title = "C",
       subtitle = "Number of specimens:49")+
  theme(plot.title = element_text(hjust = 0)) 

p2 <- ggpar(
  p2,
  font.title    = c(20, "bold", "black"),
  font.subtitle = c(10,  "black"))

grid.arrange(p0,p1,p2,ncol=3,nrow=1)
??grid.arrange

#频率直方图
p3 <- ggplot(dfS11_H ,aes(x=a)) + 
  geom_histogram(binwidth=1,fill="#69b3a2" ,
                 color="#e9ecef", alpha=0.8)+
  
  geom_density(aes(y=..density..*n), color = "lightblue", size = 2)+
  labs(x="Length of Shell (mm)",y="Frequency",title = "Size-Frequency distributions for Hindella (SBW-11)",
       subtitle = "Number of specimens:19")+
  theme(plot.title = element_text(hjust = 0.5)) 

ggpar(
  p3,
  font.title    = c(20, "bold", "black"),
  font.subtitle = c(15, "bold", "black"))


ggplot(D_S11_S10_S01, aes(x=a, y=b))+
  geom_point()+
  stat_smooth(method = "lm", se = TRUE)

#Regression Analysis
p_Dalmanella <- ggplot(abh_only_dal, aes(x=a, y=b, colour = NO, shape = NO))+
  geom_point(size = 2)+
  stat_smooth(method = "lm", formula = y~x)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~~')), 
               formula = y ~ x, parse = T)+
  theme(axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=16,face="bold"))+
  labs(x="Length of Shell (mm)",y="Width of Shell (mm)")+
       # subtitle = "Number of specimens: \n SBW-01  39\n SBW-10  52\n SBW-11  49")+
  theme(legend.background = element_rect(colour ="BLACK", 
                                         fill = 'grey90', size = 1, linetype='solid'))+
  theme(legend.position=c(0.95,0.05))+
  theme(plot.title = element_text(hjust = 0.5))
p_Dalmanella


ggpar(
  p_Dalmanella,
  font.title    = c(20, "bold", "darkblue"),
  font.subtitle = c(15, "bold", "purple"))

p_Hindella <- ggplot(df_S11_H, aes(x=a, y=b))+
  geom_point(size = 3 )+
  stat_smooth(method = "lm", formula = y~x)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~~')), 
               formula = y ~ x, parse = T)+
  theme(axis.text=element_text(size=16,face="bold"),
         axis.title=element_text(size=16,face="bold"))+
  labs(x="Length of Shell (mm)",y="Width of Shell (mm)",title = "Regression Analysis for Hindella",
       subtitle = "Number of specimens:19")+
  theme(legend.background = element_rect(colour ="BLACK", 
                                         fill = 'grey90', size = 1, linetype='solid'))+
  theme(legend.position=c(0.96,0.95))+
  theme(plot.title = element_text(hjust = 0.5))


ggpar(
  p_Hindella,
  font.title    = c(20, "bold", "darkblue"),
  font.subtitle = c(15, "bold", "purple"))







#生存曲线
attach(H_D_S11)
Surv(life,status)
fit <- survfit(Surv(life,status) ~ genus,
               conf.type="log",
               data = H_D_S11)

fit

surv11 <- ggsurvplot(fit, data = H_D_S11,
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           
          surv.median.line = "hv",  # 添加中位生存时间线
           palette = "aaas",
           legend.labs = c("Dalmanella", "Hindella"),
           ggtheme = theme(plot.title = element_text(hjust = 0.5)),
          risk.table.y.text.col = T,# colour risk table text annotations. 
          risk.table.height = 0.25, # the height of the risk table 
          risk.table.y.text = FALSE,# show bars instead of names in text annotations 
           # in legend of risk table. 
           risk.table = TRUE, # show risk 
           table.pval = TRUE, # show p-value of log-rank test.
           xlab = "Percentage of maximum age",   # customize X axis label.
           ylab = "Survival percentage",   # customize X axis label.
           surv.scale = "percent"
           )

# Changing Labels
# %%%%%%%%%%%%%%%%%%%%%%%%%%
# Labels for Survival Curves (plot)
surv11$plot <- surv11$plot + labs(
  title    = "Survival curve",
  subtitle = "Number of specimens:\n Dalmanella--49\n Hindella--19"
)+scale_y_log10()



# Labels for ncensor plot 
#surv1$ncensor.plot <- surv1$ncensor.plot + labs(
#  title    = "Number of censorings",
#  subtitle = "over the time.",
#  caption  = "source code: website.com")

# Changing the font size, style and color
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Applying the same font style to all the components of surv1:
# survival curves, risk table and censor part

surv11 <- ggpar(
  surv11,
  font.title    = c(30, "bold", "darkblue"),
  font.subtitle = c(15, "bold.italic", "purple"),
  font.x        = c(14, "bold", "red"),
  font.y        = c(14, "bold", "red"),
  legend = "right"
)

surv11



detach(H_D_S11)

attach(D_S11_S10_S01)

Surv(life,status)
fit <- survfit(Surv(life,status)~formation,
               data = D_S11_S10_S01)

fit

surv22 <- ggsurvplot(fit, data = D_S11_S10_S01,
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",
           palette = "aaas",# 添加中位生存时间线
           ggtheme = theme(plot.title = element_text(hjust = 0.5)),
           risk.table.y.text.col = T,# colour risk table text annotations. 
           risk.table.height = 0.25, # the height of the risk table 
           risk.table.y.text = FALSE,# show bars instead of names in text annotations 
           # in legend of risk table. 
          risk.table = FALSE, # show risk 
          legend.labs = c("SBW-01","SBW-10", "SBW-11"),
           table.pval = TRUE, # show p-value of log-rank test.
           xlab = "Percentage of maximum age",   # customize X axis label.
           ylab = "Survival percentage",   # customize X axis label.
           surv.scale = "percent"
)
  


surv22$plot <- surv22$plot + labs(
  title    = "Survival curve",
  subtitle = "Number of specimens: \n SBW-01  39\n SBW-10  52\n SBW-11  49"
)+scale_y_log10()

surv22 <- ggpar(
  surv22,
  font.title    = c(30, "bold", "darkblue"),
  font.subtitle = c(15, "bold", "purple"),
  font.x        = c(14, "bold", "red"),
  font.y        = c(14, "bold", "red"),
  legend = "right"
)



surv22

?ggsurvplot
ggsurvplot(fit, # 创建的拟合对象
           data = lung,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv", # 添加中位生存时间线
           palette = "hue")  # 自定义调色板
#可选调色板有 "grey","npg","aaas","lancet","jco", 
#"ucscgb","uchicago","simpsons"和"rickandmorty".