## Libraries ----------------------------------

library(data.table)
library(base)
library(mice)
library(ggplot2)
library(GGally)
library(ggthemes)
library(haven)
library(readxl)
library(plm)
library(stargazer)
library(lubridate)
library(plotly)

rm(list = ls())

setwd("C:/413 Projesi")

## MEVDUAT ve KREDI BUYUMESI --------------------------------------------

rm(list = ls())

data <- read_xlsx("PROJE.xlsx", sheet = "Mevduatlar")
data2 <- read_xlsx("PROJE.xlsx", sheet ="Kredi Hacmi")

data <- setDT(data)
data2 <- setDT(data2)

head(data)

setnames(data, as.character(data[1,]))
data <- data[-1,]

setnames(data2, as.character(data2[1,]))
data2 <- data2[-1,]

setnames(data, "NA","Date")
setnames(data2, "NA","Date")

head(data)
head(data2)

sub_data <- data[, c(1,121), with = FALSE]
names(sub_data) <- c("Date","Variable")
head(sub_data)
sub_data$Date <- as.Date(sub_data$Date)
sub_data$Variable <- as.numeric(sub_data$Variable)
class(sub_data$Date)
class(sub_data$Variable)

sub_data[, AnnualChange := (Variable - shift(Variable,12))/shift(Variable,12)*100]
head(sub_data,20)

sub_data2 <- data2[, c(1,2), with = FALSE]
names(sub_data2) <- c("Date","Variable2")
head(sub_data2)
sub_data2$Date <- as.Date(sub_data2$Date)
sub_data2$Variable2 <- as.numeric(sub_data2$Variable2)
class(sub_data2$Date)
class(sub_data2$Variable2)
sub_data2[, AnnualChange2 := (Variable2 - shift(Variable2, 12))/shift(Variable2, 12)*100]
head(sub_data2,20)

all_data <- merge(sub_data, sub_data2, on = "Date")

all_data <- all_data[, c("Date","AnnualChange","AnnualChange2"), with = FALSE]

names(all_data) <- c("Date","Total Deposits","Total Loans")

all_data_long <- melt(all_data, id.vars = "Date")

names(all_data_long) <- c("Date","Variable","Value")


p1 <-ggplotly(
      ggplot(all_data_long, aes(x=Date, y=Value, color = Variable)) + 
      geom_line() +
      scale_color_manual(values = c("Total Deposits" = "red","Total Loans" = "blue")) +
      labs(title = "Total Loans vs. Deposits Growth (yoy, %)", x = " ", y = " ", color = " ") +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) + 
      scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
      scale_y_continuous(breaks = seq(-25, 150, by = 25), labels = seq(-25, 150, by = 25))
      ) %>%
      layout(legend = list(orientation = "h", x = 0.38, y = 0.85)) 
p1

## MEVDUAT BANKALARI TUKETICI KREDILERI -----------------------------------

rm(list = ls())

df <- read_xlsx("PROJE.xlsx", sheet = "Krediler")
df <- setDT(df)
setnames(df, as.character(df[1,]))
df <- df[-1,]
setnames(df, "NA","Date")
sub_df <- df[, c(1,3,6,8), with = FALSE]
names(sub_df) <- c("Date","Consumer","Housing","Automobile")
sub_df$Date <- as.Date(sub_df$Date)
sub_df$Consumer <- as.numeric(sub_df$Consumer)
sub_df$Housing <- as.numeric(sub_df$Housing)
sub_df$Automobile <- as.numeric(sub_df$Automobile)

sub_df[, Consumer_change := (Consumer - shift(Consumer, 12))/shift(Consumer, 12)*100]
sub_df[, Housing_change := (Housing - shift(Housing, 12))/shift(Housing, 12)*100]
sub_df[, Automobile_change := (Automobile - shift(Automobile, 12))/shift(Automobile, 12)*100]

sub_df_2 <- sub_df[c(13:nrow(sub_df)),c(1,5,6,7)]
names(sub_df_2) <- c("Date","Consumer","Housing","Automobile")

all_data_long <- melt(sub_df_2, id.vars = "Date")
names(all_data_long) <- c("Date","Variable","Value")

p2 <- ggplotly(
        ggplot(all_data_long, aes(x=Date, y=Value, color = Variable)) + 
          geom_line() +
          scale_color_manual(values = c("Consumer" = "red","Housing" = "blue", "Automobile" = "darkgreen")) +
          labs(title = "Deposit Money Bank Consumer Credits Growth (yoy, %)", x = " ", y = " ", color = " ") +
          theme_minimal() +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) + 
          scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
          scale_y_continuous(breaks = seq(-100, 500, by = 100), labels = seq(-100, 500, by = 100))
      ) %>%
        layout(legend = list(orientation = "h", x = 0.35, y = 0.77)) 
p2

## MEVDUAT BANKALARI KREDI STOKU-------------------------------------------

rm(list = ls())

DT <- read_xlsx("PROJE.xlsx", sheet = "Kredi & GSYH")
DT <- setDT(DT)
setnames(DT, as.character(DT[1,]))
DT <- DT[-1,]
setnames(DT, "NA","Date")

names(DT) <- c("Date", "GDP","Loans","TL_Loans","FX_Loans")


DT$Date <- as.Date(DT$Date)
DT$GDP <- as.numeric(DT$GDP)
DT$Loans <- as.numeric(DT$Loans)
DT$TL_Loans <- as.numeric(DT$TL_Loans)
DT$FX_Loans <- as.numeric(DT$FX_Loans)
DT[, TL := TL_Loans/GDP*100]
DT[, FX := FX_Loans/GDP*100]
names(DT) <- c("Date", "GDP","Loans","TL Loans","FX Loans","TL/GDP","FX/GDP")
DT <- DT[,c(1,6,7)]
head(DT,20)

all_data_long <- melt(DT, id.vars = "Date")
names(all_data_long) <- c("Date","Variable","Value")


p3 <- ggplotly(
        ggplot(all_data_long, aes(x = Date, y = Value, fill = Variable)) + 
          geom_bar(stat = "identity") + 
          scale_fill_manual(values = c("TL/GDP" = "red", "FX/GDP" = "blue")) +  # Change to scale_fill_manual
          labs(title = "Domestic Money Banks Loans (as % of GDP)", x = " ", y = " ", fill = " ") +
          theme_minimal() +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) + 
          scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
          scale_y_continuous(breaks = seq(0, 100, by = 20), labels = seq(0, 100, by = 20))
      ) %>%
        layout(legend = list(orientation = "h", x = 0.15, y = 0.77))
p3

## DOLARIZASYON ve NOMINAL KUR----------------------------

rm(list = ls())

DT <- read_xlsx("PROJE.xlsx", sheet = "Dolarizasyon")
DT <- setDT(DT)
setnames(DT, as.character(DT[1,]))
DT <- DT[-1,]
setnames(DT, "NA","Date")
names(DT) <- c("Date", "M2","Exchange","Deposit")


DT$Date <- as.Date(DT$Date)
DT$Deposit <- as.numeric(DT$Deposit)
DT$Exchange <- as.numeric(DT$Exchange)
DT$M2 <- as.numeric(DT$M2)
DT <- DT[, Dolarizasyon := Deposit/M2*100]
DT <- DT[,c("Date","Dolarizasyon","Exchange")]
names(DT) <- c("Date","Dollarization","Exchange")

all_data_long <- melt(DT, id.vars = "Date")
names(all_data_long) <- c("Date","Variable","Value")

p4 <- ggplotly(
        ggplot(all_data_long[c(72:nrow(all_data_long)),][Variable == "Dollarization"], aes(x=Date, y=Value, color = Variable)) + 
          geom_line() +
          scale_color_manual(values = c("Dollarization" = "red")) +
          labs(title = "Time Deposits (FX) / M2", x = " ", y = " ", color = " ") +
          theme_minimal() +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) + 
          scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
          scale_y_continuous(breaks = seq(0, 50, by = 5), labels = seq(0, 50, by = 5)) 
      ) %>%
        layout(legend = list(orientation = "h", x = 0.2, y = 0.80))


## REGRESYON------------------------------------

rm(list = ls())

DT <- read_xlsx("PROJE.xlsx", sheet = "Regresyon")
DT <- setDT(DT)
setnames(DT, as.character(DT[1,]))
DT <- DT[-1,]
setnames(DT, "NA","Date")
names(DT) <- c("Date", "GDP","Consumer_interest","Commercial_interest","Consumer_growth","Commercial_growth","credit_growth")


DT$Date <- as.Date(DT$Date)
DT$GDP <- as.numeric(DT$GDP)
DT$Consumer_interest <- as.numeric(DT$Consumer_interest)
DT$Commercial_interest <- as.numeric(DT$Commercial_interest)
DT$Consumer_growth <- as.numeric(DT$Consumer_growth)
DT$Commercial_growth <- as.numeric(DT$Commercial_growth)
DT$credit_growth <- as.numeric(DT$credit_growth)

DT[, l_consumer_interest :=shift(Consumer_interest, 3)]
DT[, l_commercial_interest :=shift(Commercial_interest, 3)]
DT[, l_consumer_growth :=shift(Consumer_growth, 3)]
DT[, l_commercial_growth :=shift(Commercial_growth, 3)]
DT[, l_credit_growth :=shift(credit_growth, 3)]

DT1 <- DT[c(45:nrow(DT)-1),]
head(DT1)

DT2 <- DT[,c(1,2,7)]
head(DT2)

model_1 <- lm(GDP ~  credit_growth , data = DT)

summary(model_1) # more information about model


p5 <- ggplotly(
        ggplot(DT2, aes(x=credit_growth, y=GDP)) + geom_point(color = "red") +
          geom_smooth(formula = y ~ x, method = "lm", se = TRUE, color = "blue") +
          theme_minimal() +
          labs(title = "GDP Growth & Credit Growth (Quarterly, %)", x = "Credit Growth", y = "GDP Growth") +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) + 
          scale_x_continuous(breaks = seq(0, 80, by = 10), labels = seq(0, 80, by = 10)) +
          scale_y_continuous(breaks = seq(-20, 40, by = 5), labels = seq(-20, 40, by = 5)) 
      )
p5



















































