library(tidyverse) # for various task 
library(ggpubr) # for correlation plotting
library(PerformanceAnalytics) # for correlation plotting
library(corrplot) # for corrplot
library(GGally) # for ggcorr
library(RColorBrewer) # for color combination in corplot
library(MASS) # for ANOVA
library(dplyr)
#library(plyr)
library(ggthemes)
library(BBmisc) # for normalization of data

# Load Data
rust <- read.csv("G:/My Drive/Thesis/Data/Final Data For Analysis/Revision- Rust V-index and other metrics data - NA removed - Duplicate filtered.csv")
#rust$language <- 1
rust$language <- "Rust"


python <- read.csv("G:/My Drive/Thesis/Data/Final Data For Analysis/Revision- Python V-index and other metrics data - NA removed - Duplicate filtered.csv")
#python$language <- 2
python$language <- "Python"


java <- read.csv("G:/My Drive/Thesis/Data/Final Data For Analysis/Revision- Java V-index and other metrics data - NA removed - Duplicate filtered.csv")
#java$language <- 1
java$language <- "Java"


# Joining Rust and Python Data
data <- rbind(rust,python,java)
data$language <- as.factor(data$language)

# Renaming Direct dependencies
colnames(data)[6] <- "Direct_Dependent"

# Converting into numeric variable
data$Direct_Dependent <- as.numeric(data$Direct_Dependent)
data$Vindex <- as.numeric(data$Vindex)

View(data)
# Normalize data
View(data[,c(6,10,12:23)])
data_normalized <- normalize(data[,c(6,10,12:23)], method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")



# Min Max V-index
quantile(data$Vindex)
quantile(python$Vindex)
quantile(python$Direct_Dependent)
hist(python$Vindex)
hist(python$Direct_Dependent)
ggplot(data = python, aes(x=Vindex))+
  geom_histogram(bins = 30)
  

#output
# 0%  25%  50%  75% 100% 
# 0    1    2    5   51 
# Min V-index is 0 and max V-index is 51


# Range
range(java$Vindex)
range(python$Vindex)
range(rust$Vindex)

range(java$Dependent.Projects.Count)
range(python$Dependent.Projects.Count)
range(rust$Dependent.Projects.Count)




pvec <- seq(0,1,0.25) 
quantile(data$Vindex, pvec)

# Summary of all variables
summary(data[,c(10,12:22)])

library(Hmisc)
Hmisc::describe(data[,c(10,12:22)])

# Best summary
library(psych)
psych::describe(data[,c(10,12:22)])

# Also show histogram of data
library(skimr)
skim(data[,c(10,12:22)])

# Summary by each group
group_by(data[,c(10,6,12:23)], language) %>%
  skim()
View(data)



# summary of V-Index and Direct Dependencies
skim_with(numeric = list(hist = NULL, complete = NULL, missing=NULL, median = median, mean = mean, sd = sd), append = TRUE)
skim_with_defaults()
group_by(data[,c(6,10,23)], language) %>% 
  skim()

# Finding mode
Mode <- function(x) {
  if (is.numeric(x)) {
    x_table <- table(x)
    return(as.numeric(names(x_table)[which.max(x_table)]))
  }
}

# Number of projects with zero V-index
sum(java$Vindex == 0)
sum(python$Vindex == 0)
sum(rust$Vindex == 0)


Mode(java$Vindex)
sum(java$Vindex == Mode(java$Vindex))


Mode(java$Dependent.Projects.Count)
sum(java$Dependent.Projects.Count == Mode(java$Dependent.Projects.Count))

Mode(python$Vindex)
sum(python$Vindex == Mode(python$Vindex))



Mode(python$Dependent.Projects.Count)
sum(python$Dependent.Projects.Count == Mode(python$Dependent.Projects.Count))

Mode(rust$Vindex)
sum(rust$Vindex == Mode(rust$Vindex))

Mode(rust$Dependent.Projects.Count)
sum(rust$Dependent.Projects.Count == Mode(rust$Dependent.Projects.Count))

summary(java$Dependent.Projects.Count)
summary(python$Dependent.Projects.Count)
summary(rust$Dependent.Projects.Count)


ggplot(rust, aes(y=rust$Dependent.Projects.Count))+geom_boxplot()
boxplot(rust$Dependent.Projects.Count)

mode(data$Direct_Dependent)
summary(rust$Dependent.Projects.Count)
# Summary by each group (language and classification)
summary_group <- group_by(data[,c(10,12:24)], language, Classification) %>%
  skim()
print(summary_group)
str(summary_group)
options(max.print=1000000)

library(summarytools)
summarytools::descr(data[,c(10,12:22)])
#kable(as.data.frame(summarytools::descr(data[,c(10,12:23)])))
summarytools::descr(data[,c(10,12:23)], transpose = TRUE )







# H0 V-index of all the languages are identical
# H1 V-index of all the languages are not identical



kruskal.test(Vindex ~ as.factor(language), data = data_normalized)

#Kruskal-Wallis rank sum test

#data:  Vindex by as.factor(language)
#Kruskal-Wallis chi-squared = 130.89, df = 2, p-value < 2.2e-16

# Result indicate that p values is less then 0.05 so we reject the nul hypothesis that  V-index of all three languages are identitical and accept alternative hypothesis that V-index of all languages are different.

#kruskal.test(Vindex ~ Classification, data = data)

View(data)

library(conover.test)
conover.test(x=data_normalized$Vindex,g=data$language, altp=TRUE)
plot(ecdf(data$Vindex))

conover.test(x=data$Direct_Dependent,g=data$language, altp=TRUE)

# Histogram of V-index of all the languages
ggplot(data,aes(x=Vindex, group=language,fill=language))+geom_histogram(position="dodge",binwidth=0.25)+facet_grid(~language)+theme_bw()
ggplot(data,aes(x=Vindex, group=language,fill=language))+geom_histogram(position="dodge")+facet_grid(~language)+theme_bw()+xlab("V-index")+ guides(fill=FALSE, color=FALSE)
# Normalized data
ggplot(data_normalized,aes(x=Vindex, group=language,fill=language))+geom_histogram(position="dodge")+facet_grid(~language)+theme_bw()+xlab("V-index")+ guides(fill=FALSE, color=FALSE)



# Histogram of first order depndencies
ggplot(data,aes(x=data$Direct_Dependent, group=language,fill=language))+geom_histogram(position="dodge",binwidth=0.25)+facet_grid(~language)+theme_bw()+guides(fill=FALSE, color=FALSE)+ylim(0,100)
ggplot(data,aes(x=data$Direct_Dependent, group=language,fill=language))+geom_histogram(position="dodge")+facet_grid(~language)+theme_bw()+xlab("Direct Dependencies")+guides(fill=FALSE, color=FALSE)


ggplot(rust,aes(x=rust$Dependent.Projects.Count))+geom_histogram(position="dodge")+facet_grid(~language)+theme_bw()+xlab("Direct Dependencies")+guides(fill=FALSE, color=FALSE)


#Normalized data
ggplot(data_normalized,aes(x=data_normalized$Direct_Dependent, group=language,fill=language))+geom_histogram(position="dodge")+facet_grid(~language)+theme_bw()+xlab("Direct Dependencies")+guides(fill=FALSE, color=FALSE)


ggplot(rust,aes(x=rust$Dependent.Projects.Count))+geom_histogram(position="dodge",binwidth=0.5)
hist(rust$Dependent.Projects.Count)

#other tested methods
conover.test(x=data$Vindex,g=data$language, altp=TRUE)
conover.test(x=data$Vindex,g=data$language, altp=TRUE, method="bonferroni")
conover.test(x=data$Vindex,g=data$language, altp=TRUE, method="sidak")
conover.test(x=data$Vindex,g=data$language, altp=TRUE, method="holm")
conover.test(x=data$Vindex,g=data$language, altp=TRUE, method="hs")
conover.test(x=data$Vindex,g=data$language, altp=TRUE, method="hochberg")
conover.test(x=data$Vindex,g=data$language, altp=TRUE, method="bh")
conover.test(x=data$Vindex,g=data$language, altp=TRUE, method="by")
conover.test(x=data$Vindex,g=data$language, altp=TRUE, method=p.adjustment.methods)

conoverdata<- data[,c(6,10,23)]
conover.test(x=conoverdata,g=data$language, altp=TRUE)

View(data_normalized)
# Sub data
rust_subset <- data_normalized[ which(data_normalized$language=='Rust'), ]
python_subset <- data_normalized[ which(data_normalized$language=='Python'), ]
java_subset <- data_normalized[ which(data_normalized$language=='Java'), ]

# Wilcoxon matched pair test for companring V-index and direct dependencies 
#all combine
wilcox.test(data$Vindex, data$Direct_Dependent, paired = TRUE, alternative = "two.sided")

wilcox.test(data_normalized$Vindex, data_normalized$Direct_Dependent, paired = TRUE, alternative = "two.sided", correct = FALSE)

#by language normalized data
wilcox.test(rust_subset$Vindex, rust_subset$Direct_Dependent, paired = TRUE, alternative = "two.sided", correct = FALSE)
wilcox.test(python_subset$Vindex, python_subset$Direct_Dependent, paired = TRUE, alternative = "two.sided", correct = FALSE)
wilcox.test(java_subset$Vindex, java_subset$Direct_Dependent, paired = TRUE, alternative = "two.sided", correct = FALSE)


x$statistic


boxplot(Ozone ~ Month, data = airquality)
wilcox.test(Ozone ~ Month, data = airquality,
            subset = Month %in% c(5, 8))

boxplot(Vindex ~ language, data = data)
boxplot(Direct_Dependent ~ language, data = data)

wilcox.test(Vindex ~ language, data = data,
            subset = language %in% c("Java", "Python"))


friedman.test(Vindex ~ Direct_Dependent | language,
              data = data)



dfsummary(data[,c(10,12:22)])
hist(data$Vindex, breaks = 20, prob=T)

#QQ plot
qqnorm(data$Vindex)
qqline(data$Vindex)

# Box plot of all languages
ggplot(data = data, aes(y=Vindex))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,max(data$Vindex)), breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,40,50,60))+
  labs(title="Box Plot of V-index of all projects")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  xlab("All Language")





# Box plot of V-index
ggplot(data = data, aes(y=Vindex, x=language, fill=language))+
  geom_boxplot()+
  #geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=4, notch = TRUE)+
  stat_boxplot()+
  labs(title="Box Plot of V-index")+theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_y_continuous(limits = c(0,max(data$Vindex)), breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,40,50,60))
  #stat_summary(fun.y=quantile, geom="point", shape=1, size=4)+
  #geom_jitter(shape=16, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1, binwidth = 1)

max(data$Vindex)

# Box plot of Direct Dependencies
ggplot(data = data, aes(y=data$Direct_Dependent, x=language, fill=language))+
  geom_boxplot()+
  #geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=4, notch = TRUE)+
  stat_boxplot()+
  labs(title="Box Plot of Direct Dependencies")+theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_y_continuous(limits = c(0,max(data$Vindex)), breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,40,50))+
  ylab("Direct Dependencies")
  #stat_summary(fun.y=quantile, geom="point", shape=1, size=4)+
  #geom_jitter(shape=16, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1, binwidth = 1)
  #breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,40,50,3000)


ggplot(data = rust, aes(y=rust$Dependent.Projects.Count))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,max(rust$Dependent.Projects.Count)), breaks=c(seq(0,max(rust$Dependent.Projects.Count),20)))

breaks=c(seq(-3.5,3.5,0.5))



# Classifying Data of Rust in low medium and High
rust$Classification[rust$Vindex <2] = "Low"
rust$Classification[rust$Vindex >= 2 & rust$Vindex <= 7] = "Medium"
rust$Classification[rust$Vindex >7] = "High"


# Classifying Data of Python low medium and High
python$Classification[python$Vindex <=4] = "Medium"
python$Classification[python$Vindex >4] = "High"


# Classifying Data of Java in low medium and High
java$Classification[java$Vindex <1] = "Low"
java$Classification[java$Vindex >= 1 & java$Vindex <= 4] = "Medium"
java$Classification[java$Vindex >4] = "High"



# Re-joining Rust and Python Data after classifying
data <- rbind(rust,python,java)

# Arranging the order of classification from high medium to low
data$Classification <- factor(data$Classification, levels = c("High", "Medium","Low"))


write.csv(data, "G:/My Drive/Thesis/Data/Final Data For Analysis/combine data for plotting in tableau.csv")

View(data)


# Pairwise plot
selected_columns <- data[,c(6,10,12:23)]

ggpairs(data=selected_columns, # data.frame with variables
        #mapping = aes(color = language),
        columns=1:13, # columns to plot, default to all.
        title="Pairwise plot of all the project", # title of the plot
        upper = list(
          continuous = wrap("cor", size = 3.5, alignPercent = 1)
        )
) +
  theme_grey(base_size = 8)


ggpairs(data=data_normalized, # data.frame with variables
        mapping = aes(color = language),
        columns=1:13, # columns to plot, default to all.
        title="Pairwise plot of all the project", # title of the plot
        upper = list(
          continuous = wrap("cor", size = 3.5, alignPercent = 1,method = "spearman")
        )
) +
  theme_grey(base_size = 8)
View(data_normalized)

assignInNamespace("ggally_cor", ggally_cor, "GGally")
View(selected_columns)
ggpairs(data=selected_columns, # data.frame with variables
        mapping = aes(color = language),
        columns=1:13, # columns to plot, default to all.
        title="Pairwise plot of all the project by language", # title of the plot
        upper = list(
          continuous = wrap("cor", size = 3, alignPercent = 1,method = "spearman")
        )
) +
  theme_grey(base_size = 7.5)+
  stat_compare_means(aes(label = ..p.signif.., color = language))


selected_columns_ <- data[,c(6,10,12:22)]
view(selected_columns_)
corrplot(selected_columns_ )
chart.Correlation(selected_columns_, histogram = TRUE, method = c("spearman"))




library("Hmisc")
str(selected_columns_)
res <- rcorr(asNumericMatrix(selected_columns_), type = "spearman")
res$P


selected_columns_ <- rust[,c(6,10,12:22)]
M <- cor(selected_columns_, method = "spearman")
res <- cor.mtest(selected_columns_, conf.level = .95, method = "spearman")
corrplot(M, p.mat = res$p, type = "upper",
         sig.level = c(.001, .01, .05), pch.cex = .9,
         insig = "label_sig", pch.col = "red")




#Subsetting data by group
selected_columns_h <- filter(selected_columns, Classification == "High")

ggpairs(data=selected_columns_h, # data.frame with variables
        mapping = aes(color = language),
        columns=1:12, # columns to plot, default to all.
        title="Pairwise plot of high impacting projects", # title of the plot
        upper = list(
          continuous = wrap("cor", size = 3.5, alignPercent = 1)
        )
) +
  theme_grey(base_size = 7.5)


#Subsetting data by group
selected_columns_m <- filter(selected_columns, Classification == "Medium")

ggpairs(data=selected_columns_m, # data.frame with variables
        mapping = aes(color = language),
        columns=1:12, # columns to plot, default to all.
        title="Pairwise plot of medium impacting projects", # title of the plot
        upper = list(
          continuous = wrap("cor", size = 3.5, alignPercent = 1)
        )
) +
  theme_grey(base_size = 7.5)


#Subsetting data by group
selected_columns_l <- filter(selected_columns, Classification == "Low")

ggpairs(data=selected_columns_l, # data.frame with variables
        mapping = aes(color = language),
        columns=1:12, # columns to plot, default to all.
        title="Pairwise plot of low impacting projects", # title of the plot
        upper = list(
          continuous = wrap("cor", size = 3.5, alignPercent = 1)
        )
) +
  theme_grey(base_size = 7.5)





# Rust Correlation
rust_cor <- cor(rust[,c(10,12:22)])
rust_cor <- round(rust_cor,2)
rust_cor_p <- cor.mtest(rust[,c(10,12:22)]) 
corrplot(rust_cor, method="number", type="lower", 
         col=c("red", "blue", "grey10", "darkgreen", "chocolate4", "midnightblue"), 
         number.cex = 0.7, tl.col="black",
         p.mat = rust_cor_p$p, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 0.1, pch.col = "red")


# Python Correlation
python_cor <- cor(python[,c(10,12:22)])
python_cor <- round(python_cor,2)
python_cor_p <- cor.mtest(python[,c(10,12:22)]) 
corrplot(python_cor, method="number", type="lower", 
         col=c("red", "blue", "grey10", "darkgreen", "chocolate4", "midnightblue"), 
         number.cex = 0.7, tl.col="black",
         p.mat = python_cor_p$p, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 0.1, pch.col = "red")


# Java Correlation
java_cor <- cor(java[,c(10,12:22)])
java_cor <- round(java_cor,2)
java_cor_p <- cor.mtest(java[,c(10,12:22)]) 
corrplot(java_cor, method="number", type="lower", 
         col=c("red", "blue", "grey10", "darkgreen", "chocolate4", "midnightblue"), 
         number.cex = 0.7, tl.col="black",
         p.mat = java_cor_p$p, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 0.1, pch.col = "red")

# All combine Correlation
data_cor <- cor(data[,c(10,12:22)])
data_cor <- round(data_cor,2)
data_cor_p <- cor.mtest(data[,c(10,12:22)]) 
#View(data_cor)
corrplot(data_cor, method="number", type="lower", 
         col=c("red", "blue", "grey10", "darkgreen", "chocolate4", "midnightblue"), 
         number.cex = 0.7, tl.col="black",
         p.mat = data_cor_p$p, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 0.1, pch.col = "red")

View(data)
# Histogram
ggplot(data=data[,c(10,12:22,24)], aes(data$TotalWatchers, fill = data$language)) + 
  geom_histogram(bins = 20)+
  #geom_histogram(data=subset(data[,c(10,12:22,24)],data$language == 'rust'), alpha =1, binwidth=50 ,position="dodge"  )+
  #geom_histogram(data=subset(data[,c(10,12:22,24)],data$language == 'python'), alpha = 0.5, binwidth=50,position="dodge"  )+
  #geom_histogram(data=subset(data[,c(10,12:22,24)],data$language == 'java'), alpha = 0.5, binwidth=50,position="dodge"  )+
  scale_y_continuous(expand = c(0,0), limits = c(0, 175))+
  scale_x_continuous(expand = c(0,0), limits = c(0, 13000))+
  xlab("Total Watchers")+ylab("Total Prjects Count")


ggplot(data=data[,c(10,12:22,24)], aes(data$TotalWatchers, fill = data$language, color = data$Classification)) + 
  geom_histogram(bins = 20)+
  #geom_histogram(data=subset(data[,c(10,12:22,24)],data$language == 'rust'), alpha =1, binwidth=50 ,position="dodge"  )+
  #geom_histogram(data=subset(data[,c(10,12:22,24)],data$language == 'python'), alpha = 0.5, binwidth=50,position="dodge"  )+
  #geom_histogram(data=subset(data[,c(10,12:22,24)],data$language == 'java'), alpha = 0.5, binwidth=50,position="dodge"  )+
  scale_y_continuous(expand = c(0,0), limits = c(0, 175))+
  scale_x_continuous(expand = c(0,0), limits = c(0, 13000))+
  xlab("Total Watchers")+ylab("Total Prjects Count")



# Histogram Commiters
ggplot(data=data[,c(10,12:22,24)], aes(x=data$TotalCommitters, fill = data$language)) + 
  #geom_density()+
  geom_histogram(bin=20, alpha=.5, position="identity")+
  #geom_histogram(data=subset(data[,c(10,12:22,24)],data$language == 'rust'), alpha =1, binwidth=50,position="dodge"  )+
  #geom_histogram(data=subset(data[,c(10,12:22,24)],data$language == 'python'), alpha = 0.5, binwidth=50,position="dodge" )+
  scale_y_continuous(expand = c(0,0), limits = c(0, 200))+
  scale_x_continuous(expand = c(0,0),limits = c(0, 800))+
  #scale_fill_solarized() +
  xlab("Total Committers")+ylab("Total Projects Count")


# Histogram Commiters
ggplot(data=data[,c(10,12:22,24)], aes(data$TotalCommitters, fill = data$language)) + 
  geom_density(data=subset(data[,c(10,12:22,24)],data$language == 'rust'), alpha =1, binwidth=50,position="dodge"  )+
  geom_density(data=subset(data[,c(10,12:22,24)],data$language == 'python'), alpha = 0.5, binwidth=50,position="dodge" )




fit <- factanal(data, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(data),cex=.7) # add variable names

