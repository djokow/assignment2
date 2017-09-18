setwd("D:\\temp")

library(XLConnect)
wk <- loadWorkbook("GCI_dataset.xlsx")
df <- readWorksheet(wk, sheet="Sheet1")

#fix PHL data issue
df$PHL = as.numeric(df$PHL)


# Q1 ----

library(dplyr)
hiv_prevalence <- df %>% filter(Edition=="2014-2015", Series.code=="4.05", Attribute=="Value") %>% select(KHM:VNM)
colnames <- colnames(hiv_prevalence)
hiv_prevalence <- t(hiv_prevalence)
hiv_df = data.frame(colnames, hiv_prevalence)

library(ggplot2)
ggplot(hiv_df, aes(x=reorder(colnames, hiv_prevalence), weight=hiv_prevalence)) +
    geom_bar() +
    labs(x="Country", y="HIV Prevalence (%)")

hiv_prevalence_rank <- df %>% filter(Edition=="2014-2015", Series.code=="4.05", Attribute=="Rank") %>% select(KHM:VNM)
colnames <- colnames(hiv_prevalence_rank)
hiv_prevalence_rank <- t(hiv_prevalence_rank)
hiv_df_rank <- data.frame(colnames, hiv_prevalence_rank)

ggplot(hiv_df_rank, aes(x=reorder(colnames, hiv_prevalence_rank), y=hiv_prevalence_rank)) +
  geom_point() +
  labs(x="Country", y="HIV Prevalence (Rank)") +
  scale_y_reverse()



# Q2 ----

irreg_pay_cambodia = df %>% filter(Series.code=="1.05", Attribute=="Value") %>% select(Edition, KHM)
reliability_police_cambodia = df %>% filter(Series.code=="1.16", Attribute=="Value") %>% select(Edition, KHM)

cor(irreg_pay_cambodia$KHM, reliability_police_cambodia$KHM[1:5])
# [1] 0.8921799

ggplot(subset(df, Series.code %in% c("1.05", "1.16") & Attribute=="Value"),
       aes(x=substr(Edition,1,4),
           y=KHM,
           group=Series.code,
           color=factor(Series.code))) +
  geom_point() +
  geom_line() +
  labs(x="Year", y="Rating (1-7)") +
  scale_color_discrete(name="Series",
                        breaks=c("1.05", "1.16"),
                        labels=c("Irregular payments and bribes", "Reliability of police services"))

library(tidyr)
df2 <- df1[names(df1) != "Series"]
df2$Series.code <- paste("c", df2$Series.code, sep="")
df2 <- spread(df2[names(df2) != "Series"], Series.code, Value)

ggplot(df2[df2$Attribute=="Value" & df2$Country=="KHM",],
       aes(x=c1.05,
           y=c1.16,
           group=1,
           color=Edition))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Irregular payment and bribes",
       y="Reliability of Police services")


# Q3 ----


df1 <- df %>% gather(Country, Value, KHM:VNM)

ggplot(df1[df1$Series.code=="10.01" & df1$Attribute=="Value",],
       aes(x=substr(Edition,1,4),
           y=Value,
           group=Country,
           color=Country)) +
  geom_point() +
  geom_line() +
  labs(x="Year", y="Domestic Market Size Index")

ggplot(df1[df1$Series.code=="10.02" & df1$Attribute=="Value",],
       aes(x=substr(Edition,1,4),
           y=Value,
           group=Country,
           color=Country)) +
  geom_point() +
  geom_line() +
  labs(x="Year", y="Foreign Market Size Index")



ggplot(df1[df1$Series.code=="10.01" & df1$Attribute=="Rank",],
       aes(x=substr(Edition,1,4),
           y=Value,
           group=Country,
           color=Country)) +
  geom_point() +
  geom_line() +
  labs(x="Year", y="Domestic Market Size Index Rank") +
  scale_y_reverse()

ggplot(df1[df1$Series.code=="10.02" & df1$Attribute=="Rank",],
       aes(x=substr(Edition,1,4),
           y=Value,
           group=Country,
           color=Country)) +
  geom_point() +
  geom_line() +
  labs(x="Year", y="Foreign Market Size Index Rank") +
  scale_y_reverse()



# Q4 ----


# clean data
df1$Value <- df1$Value * 10^(-3*(df1$Value > 100000 & df1$Series.code=="0.01" & df1$Attribute=="Value"))

ggplot(df1[df1$Series.code=="0.01" & df1$Attribute=="Value" & (df1$Country=="IDN"|df1$Country=="THA"),],
       aes(x=substr(Edition,1,4),
           y=Value,
           group=Country,
           color=Country))+
  geom_point()+
  geom_line()+
  labs(x="Year", y="GDP (US$ billions)")

cor(
  df1[df1$Series.code=="0.01" & df1$Attribute=="Value" & df1$Country=="IDN",]$Value,
  df1[df1$Series.code=="0.01" & df1$Attribute=="Value" & df1$Country=="THA",]$Value
)
# [1] 0.9908999

ggplot(df1[df1$Series.code=="5.03" & df1$Attribute=="Value" & (df1$Country=="IDN"|df1$Country=="THA"),],
       aes(x=substr(Edition,1,4),
           y=Value,
           group=Country,
           color=Country))+
  geom_point()+
  geom_line()+
  labs(x="Year", y="Quality of Education System")


cor(
df1[df1$Series.code=="5.03" & df1$Attribute=="Value" & df1$Country=="IDN",]$Value,
df1[df1$Series.code=="5.03" & df1$Attribute=="Value" & df1$Country=="THA",]$Value
)
# [1] 0.4382189


df2 <- df1[names(df1) != "Series"]
df2$Series.code <- paste("c", df2$Series.code, sep="")
df2 <- spread(df2[names(df2) != "Series"], Series.code, Value)


cor(
  df1[df1$Series.code=="0.01" & df1$Attribute=="Value" & df1$Country=="IDN" & as.integer(substr(df1$Edition, 1, 4)) >= 2008 & as.integer(substr(df1$Edition, 1, 4)) <= 2013,]$Value,
  df1[df1$Series.code=="5.03" & df1$Attribute=="Value" & df1$Country=="IDN" & as.integer(substr(df1$Edition, 1, 4)) >= 2008 & as.integer(substr(df1$Edition, 1, 4)) <= 2013,]$Value
)
# [1] 0.2664105

ggplot(df2[df2$Attribute=="Value" & df2$Country=="IDN" & as.integer(substr(df2$Edition, 1, 4)) >= 2008 & as.integer(substr(df2$Edition, 1, 4)) <= 2013,],
       aes(x=c5.03,
           y=c0.01,
           group=1,
           color=Edition))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Quality of Education System",
       y="GDP (US$ billions)")

cor(
  df1[df1$Series.code=="0.01" & df1$Attribute=="Value" & df1$Country=="THA" & as.integer(substr(df1$Edition, 1, 4)) >= 2008 & as.integer(substr(df1$Edition, 1, 4)) <= 2013,]$Value,
  df1[df1$Series.code=="5.03" & df1$Attribute=="Value" & df1$Country=="THA" & as.integer(substr(df1$Edition, 1, 4)) >= 2008 & as.integer(substr(df1$Edition, 1, 4)) <= 2013,]$Value
)
# [1] -0.8402767

ggplot(df2[df2$Attribute=="Value" & df2$Country=="THA" & as.integer(substr(df2$Edition, 1, 4)) >= 2008 & as.integer(substr(df2$Edition, 1, 4)) <= 2013,],
       aes(x=c5.03,
           y=c0.01,
           group=1,
           color=Edition))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Quality of Education System",
       y="GDP (US$ billions)")

cor(
  df1[df1$Series.code=="0.01" & df1$Attribute=="Value" & (df1$Country=="IDN" | df1$Country=="THA") & as.integer(substr(df1$Edition, 1, 4)) >= 2008 & as.integer(substr(df1$Edition, 1, 4)) <= 2013,]$Value,
  df1[df1$Series.code=="5.03" & df1$Attribute=="Value" & (df1$Country=="IDN" | df1$Country=="THA") & as.integer(substr(df1$Edition, 1, 4)) >= 2008 & as.integer(substr(df1$Edition, 1, 4)) <= 2013,]$Value
)
# [1] 0.7769784

ggplot(df2[df2$Attribute=="Value" & (df2$Country=="IDN" | df2$Country=="THA"),],
       aes(x=c5.03,
           y=c0.01,
           group=1,
           color=Country))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Quality of Education System",
       y="GDP (US$ billions)")

# Q5 ----

df2 <- df1[names(df1) != "Series"]
df2$Series.code <- paste("c", df2$Series.code, sep="")
df2 <- spread(df2[names(df2) != "Series"], Series.code, Value)


ggplot(df2[df2$Attribute=="Value",],
       aes(x=c3.03,
           y=c8.06,
           group=Country,
           color=c0.03))+
  geom_point()+
  labs(x="Inflation (%)", y="Soundness of Banks", color="GDP per capita")



data.matrix(cor(df2[df2$Attribute=="Value" & as.integer(substr(df2$Edition, 1, 4)) >= 2008 & as.integer(substr(df2$Edition, 1, 4)) <= 2013,] %>% select(c0.03, c3.03, c8.06)))
#            c0.03      c3.03      c8.06
# c0.03  1.0000000 -0.2090328  0.7293571
# c3.03 -0.2090328  1.0000000 -0.5214003
# c8.06  0.7293571 -0.5214003  1.0000000



ggplot(df2[df2$Attribute=="Value",],
       aes(x=c8.06,
           y=c0.03,
           group=1,
           color=Country))+
  geom_point()+
  geom_smooth()+
  labs(x="Soundness of Banks", y="GDP per capita (US$ billions)")


ggplot(df2[df2$Attribute=="Value",],
       aes(x=c8.06,
           y=log(c0.03),
           group=1,
           color=Country))+
  geom_point()+
  geom_smooth()+
  labs(x="Soundness of Banks", y="GDP per capita (US$ billions - log scale)")


ggplot(df2[df2$Attribute=="Value",],
       aes(x=c8.06,
           y=log(c0.03),
           group=1,
           color=Country))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Soundness of Banks", y="GDP per capita (US$ billions - log scale)")

