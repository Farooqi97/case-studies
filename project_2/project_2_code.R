# Install if needed
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("zoo")
# install.packages("changepoint")
# install.packages("trend")
# install.packages("modifiedmk")
# install.packages("viridis")

library(viridis)
library(readxl)
library(dplyr)
library(zoo)
library(changepoint)
library(trend)
library(ggplot2)
library(modifiedmk)

grau <- read_excel("E:/case_study/project_1/case_studies/project_2/Tankbuch_grau.xlsx")
karr <- read_excel("E:/case_study/project_1/case_studies/project_2/Tankbuch_karriert.xlsx")

####################################### pre-processing #########################
################# visualization of typos in the data or car changes ############

plot(grau$Datum, grau$Kilometerstand, xlab = "Date", ylab = "Odometer reading", type = "l")
plot(karr$Datum, karr$Kilometerstand, xlab = "Date", ylab = "Odometer reading", type = "l")

grau$Datum <- as.Date(grau$Datum)
karr$Datum <- as.Date(karr$Datum)
# correct dates

plot(1:382, diff(grau$Datum), type = "l")
plot(1:356, diff(karr$Datum), type = "l")
# two typos in the date column
# fix those

which(diff(grau$Datum) < 0)
# 215, 367, 381

grau$Datum[215] <- as.Date("2006-08-12")
# most likely mistook a 1 for a 7
grau$Datum[368] <- as.Date("2014-05-26")
# wrong year, obviously
grau$Datum[382] <- as.Date("2015-01-29")
grau$Datum[383] <- as.Date("2015-03-12")
# wrong year again

which(is.na(grau$Datum) == TRUE)
# no missing dates

which(diff(karr$Datum) < 0)
# 35, 274

karr$Datum[36] <- as.Date("1987-06-09")
karr$Datum[38] <- as.Date("1987-06-13")
# wrong year
karr$Datum[37] <- median(c(as.Date("1987-06-13"), as.Date("1987-06-09")))
# missing date, linear interpolation
karr$Datum[275] <- as.Date("2002-07-04")

which(is.na(karr$Datum) == TRUE)
# 78

karr[78,]$Datum <- median(c(karr[77,]$Datum, karr[79,]$Datum))

grau$kilodiff <- c(diff(grau$Kilometerstand),NA)
karr$kilodiff <- c(diff(karr$Kilometerstand),NA)


################################################################################
################ fix the differrent currencies #################################


which(karr$Währung == "Dinar")
karr[48,]$Preis <- 12600
# fix this obvious typo

table(grau$Währung)
# exchange rate for DM to Euro
# 1 DEM = 0.51129 EUR
grau$PreisEU <- grau$Preis
grau[which(grau$Währung == "DM"),]$PreisEU <- 
  grau[which(grau$Währung == "DM"),]$PreisEU * 0.51129
grau[which(grau$Währung == "DM"),]$PreisEU <-
  round(grau[which(grau$Währung == "DM"),]$PreisEU, digits = 2)

which(grau$Währung != "Euro" & grau$Währung != "DM")
# 109 110 111 131 190 191 192 213 214 215 216 238 239 240 241 242 243 244 245

grau[109,]$PreisEU <- grau[109,]$PreisEU * 0.0727
grau[110,]$PreisEU <- grau[110,]$PreisEU * 0.0295
grau[111,]$PreisEU <- grau[111,]$PreisEU * 0.0295
grau[131,]$PreisEU <- grau[131,]$PreisEU * 1.59
grau[190,]$PreisEU <- grau[190,]$PreisEU * 0.242
grau[191,]$PreisEU <- grau[191,]$PreisEU * 0.246
grau[192,]$PreisEU <- grau[192,]$PreisEU * 0.246
grau[213,]$PreisEU <- grau[213,]$PreisEU * 0.127
grau[214,]$PreisEU <- grau[214,]$PreisEU * 0.127
grau[215,]$PreisEU <- grau[215,]$PreisEU * 0.126
grau[216,]$PreisEU <- grau[216,]$PreisEU * 0.126
grau[238,]$PreisEU <- grau[238,]$PreisEU * 0.126
grau[239,]$PreisEU <- grau[239,]$PreisEU * 0.126
grau[240,]$PreisEU <- grau[240,]$PreisEU * 0.0110
grau[241,]$PreisEU <- grau[241,]$PreisEU * 0.0110
grau[242,]$PreisEU <- grau[242,]$PreisEU * 0.0112
grau[243,]$PreisEU <- grau[243,]$PreisEU * 0.0110
grau[244,]$PreisEU <- grau[244,]$PreisEU * 0.134
grau[245,]$PreisEU <- grau[245,]$PreisEU * 0.134

# 1 ATS = 0.07267 EUR (fixed)
# 1 CZK ≈ 0.02914 EUR (fixed)
# 1 GBP ≈ 1.5723 EUR (fixed)

# exchange rate for DM to Euro
# 1 DEM = 0.51129 EUR
karr$PreisEU <- karr$Preis
karr[which(karr$Währung == "DM"),]$PreisEU <- 
  karr[which(karr$Währung == "DM"),]$PreisEU * 0.51129
karr[which(karr$Währung == "DM"),]$PreisEU <-
  round(karr[which(karr$Währung == "DM"),]$PreisEU, digits = 2)

which(karr$Währung != "Euro" & karr$Währung != "DM")
# 10  11  12  13  14  15  16  17  18  19  44  45  46  47  48  49  50  51  52  
# 53  54  56  57  58 190 191


karr[10,]$PreisEU <- karr[10,]$PreisEU * 0.454
karr[11,]$PreisEU <- karr[11,]$PreisEU * 1.41
karr[12,]$PreisEU <- karr[12,]$PreisEU * 1.41
karr[13,]$PreisEU <- karr[13,]$PreisEU * 1.41
karr[14,]$PreisEU <- karr[14,]$PreisEU * 1.41
karr[15,]$PreisEU <- karr[15,]$PreisEU * 1.41
karr[16,]$PreisEU <- karr[16,]$PreisEU * 1.27
karr[17,]$PreisEU <- karr[17,]$PreisEU * 1.27
karr[18,]$PreisEU <- karr[18,]$PreisEU * 1.41
karr[19,]$PreisEU <- karr[19,]$PreisEU * 1.41
karr[44,]$PreisEU <- karr[44,]$PreisEU * 0.0727
karr[45,]$PreisEU <- karr[45,]$PreisEU * 0.001266322
karr[46,]$PreisEU <- karr[46,]$PreisEU * 0.001263786
karr[47,]$PreisEU <- karr[47,]$PreisEU * 0.001256241
karr[48,]$PreisEU <- karr[48,]$PreisEU * 0.001248754
karr[49,]$PreisEU <- karr[49,]$PreisEU * 0.00293 
karr[50,]$PreisEU <- karr[50,]$PreisEU * 0.00293 
karr[51,]$PreisEU <- karr[51,]$PreisEU * 0.00293 
karr[52,]$PreisEU <- karr[52,]$PreisEU * 0.00293 
karr[53,]$PreisEU <- karr[53,]$PreisEU * 0.00293 
karr[54,]$PreisEU <- karr[54,]$PreisEU * 0.00293 
karr[56,]$PreisEU <- karr[56,]$PreisEU * 0.004   
karr[57,]$PreisEU <- karr[57,]$PreisEU * 0.004   
karr[58,]$PreisEU <- karr[58,]$PreisEU * 0.0727  
karr[190,]$PreisEU <- karr[190,]$PreisEU * 0.1342
karr[191,]$PreisEU <- karr[191,]$PreisEU * 0.1342


# NAs in Preis
which(is.na(karr$Preis) == TRUE)
# 1   2  36  37  38  55  64  78 189 269
# remove them from the time series

which(is.na(grau$Preis) == TRUE)

grau$ppl <- grau$Preis / grau$Liter
karr$ppl <- karr$Preis / karr$Liter

karr <- karr[-which(karr$Währung == "Forint"),]


################################################################################
############################### check for outliers in the currencies ###########


sapply(unique(grau$Währung), function(x) sd(grau$ppl[which(grau$Währung == x)]))
sapply(unique(karr$Währung), function(x) sd(karr$ppl[which(karr$Währung == x)]))
# check Dinar in karr

which(karr$Währung == "Dinar")
karr[48,]$Preis <- 12600
# fix this obvious typo

grau[is.na(grau$Währung),]
# 4 NAs
# 50,000 lira
grau[21,]$Währung <- "ITL"
grau[21,]$PreisEU <- 50 * 0.51646 
grau[100,]$Währung <- "tschechische Krone"
grau[100,]$PreisEU <- grau[100,]$Preis * 0.0295
grau[293,]$Währung <- "Zloty"
grau[293,]$Preis <- 135
grau[293,]$PreisEU <- grau[293,]$Preis * 0.2457
grau[294,]$Währung <- "Zloty"
grau[294,]$Preis <- 82
grau[294,]$PreisEU <- grau[294,]$Preis * 0.2457


# https://www.kursna-lista.com/en/historical-serbian-dinar-rates
karr[55,]$Preis <- 15750*2
karr[55,]$Währung <- "Dinar"
karr[55,]$PreisEU <- karr[55,]$Preis * 0.001108127
# ask Christine

# timeseries object

grau$ppl <- grau$PreisEU / grau$Liter
karr$ppl <- karr$PreisEU / karr$Liter

# Time series petrol prices grau
# Time series petrol prices karriert
# --- Plot for grau ---
ggplot(grau, aes(x = Datum, y = ppl)) +
  geom_line(color = viridis(1), linewidth = 0.8) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Time Series of Petrol Prices (grau)",
    x = "Date (Year–Month)",
    y = "Price (€/liter)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
    axis.text.y = element_text(size = 9, face = "bold")
  )

# --- Plot for karr ---
ggplot(karr, aes(x = Datum, y = ppl)) +
  geom_line(color = viridis(1), linewidth = 0.8) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Time Series of Petrol Prices (karr)",
    x = "Date (Year–Month)",
    y = "Price (€/liter)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
    axis.text.y = element_text(size = 9, face = "bold")
  )


################################################################################
###################problems in the odometer reading ############################


karr <- karr %>%
  arrange(Datum) %>%
  mutate(
    kilodiff = Kilometerstand - lag(Kilometerstand),
    Verbrauch = ifelse(
      !is.na(kilodiff) & !is.na(lag(Liter)) & kilodiff > 0,
      round((lag(Liter) / kilodiff) * 100, 2),
      NA_real_
    )
  )

grau <- grau %>%
  arrange(Datum) %>%
  # Step 1: compute kilodiff
  mutate(kilodiff = Kilometerstand - lag(Kilometerstand)) %>%
  # Step 2: update Verbrauch only if it's missing
  mutate(
    Verbrauch = ifelse(
      is.na(Verbrauch) & !is.na(kilodiff) & !is.na(lag(Liter)) & kilodiff > 0,
      round((lag(Liter) / kilodiff) * 100, 2),
      Verbrauch  # retain original
    )
  )


which(grau$kilodiff < 0)
# 181, 207

grau[181,]$Kilometerstand <- 133047

sum(karr$kilodiff < 0, na.rm = TRUE)
which(karr$kilodiff < 0)
karr[270,]$Kilometerstand <- 28392

karr <- karr %>%
  arrange(Datum) %>%
  mutate(
    kilodiff = Kilometerstand - lag(Kilometerstand),
    Verbrauch = ifelse(
      !is.na(kilodiff) & !is.na(lag(Liter)) & kilodiff > 0,
      round((lag(Liter) / kilodiff) * 100, 2),
      NA_real_
    )
  )

grau <- grau %>%
  arrange(Datum) %>%
  # Step 1: compute kilodiff
  mutate(kilodiff = Kilometerstand - lag(Kilometerstand)) %>%
  # Step 2: update Verbrauch only if it's missing
  mutate(
    Verbrauch = ifelse(
      is.na(Verbrauch) & !is.na(kilodiff) & !is.na(lag(Liter)) & kilodiff > 0,
      round((lag(Liter) / kilodiff) * 100, 2),
      Verbrauch  # retain original
    )
  )


# --- Time series petrol consumption (grau) ---
ggplot(grau, aes(x = Datum, y = Verbrauch)) +
  geom_line(color = "orange", linewidth = 0.8) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Time Series for Petrol Consumption (grau)",
    x = "Date (Year–Month)",
    y = "Liter per 100 km"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
    axis.text.y = element_text(size = 9, face = "bold")
  )

# --- Time series petrol consumption (karr) ---
ggplot(karr, aes(x = Datum, y = Verbrauch)) +
  geom_line(color = "orange", linewidth = 0.8) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Time Series for Petrol Consumption (karr)",
    x = "Date (Year–Month)",
    y = "Liter per 100 km"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
    axis.text.y = element_text(size = 9, face = "bold")
  )


################################################################################
########################### change point detection in consumption ##############


plot_pelt <- function(data, book = "grau", date_col = "Datum", value_col, cpt_obj,
                      binseg_obj) {
  
  # Extract changepoint indices
  changepoints <- cpt_obj@cpts
  n <- nrow(data)
  changepoints_full <- c(0, changepoints)
  
  
  # Segment start and end as date
  get_segments <- function(data, date_col, value_col, cpts) {
    cpts_full <- c(0, cpts)
    segment_data <- data.frame(
      start = data[[date_col]][cpts_full[-length(cpts_full)] + 1],
      end = data[[date_col]][cpts_full[-1]],
      mean_value = NA_real_
    )
    for (i in seq_len(nrow(segment_data))) {
      start_idx <- cpts_full[i] + 1
      end_idx <- cpts_full[i + 1]
      segment_data$mean_value[i] <- mean(data[[value_col]][start_idx:end_idx])
    }
    step_df <- data.frame(
      Date = c(rbind(segment_data$start, segment_data$end)),
      Mean = rep(segment_data$mean_value, each = 2)
    )
    step_df$Date <- as.Date(step_df$Date)
    return(step_df)
  }
  
  step_pelt <- get_segments(data, date_col, value_col, cpt_obj@cpts)
  step_binseg <- get_segments(data, date_col, value_col, binseg_obj@cpts) 
  
  
  # Plot titles
  if(value_col == "Verbrauch"){
    yax <- "Liter per 100 km"
    mt <- paste("Changepoints for petrol consumption of logbook", book) 
    if(data$Kilometerstand[173] == 148183){ # car change for grau or karriert
      change <- as.Date(data$Datum[174])
    } else{
      change <- as.Date(data$Datum[214])
    }
  }else{
    yax <- "Euro per liter"
    mt <- paste("Changepoints for petrol price of logbook", book)
    change <- head(data$Datum[which(data$Datum >= "2002-01-01")], 1)
  }
  
  # Create the plot
  p <- ggplot(data, aes(x = !!sym(date_col), y = !!sym(value_col))) +
    geom_line(aes(color = "Time series", linetype = "Time series")) +
    geom_step(data = step_pelt, aes(x = Date, y = Mean, 
                                    color = "PELT", linetype = "PELT"),
              linewidth = 1) +
    labs(title = mt,
         y = yax, x = "Date")
  p <- p + geom_step(data = step_binseg, aes(x = Date, y = Mean,
                                             color = "BinSeg", linetype = "BinSeg"),
                     linewidth = 1)
  
  if(value_col == "Verbrauch"){
    p <- p + geom_vline(aes(xintercept = change, 
                            color = "Car change", linetype = "Car change"),
                        linewidth = 0.8)
    p <- p +
      scale_color_manual(name = "", values = c(
        "Car change" = "black",
        "BinSeg" = "orange",
        "PELT" =  "blue",
        "Time series" = "black"
      )) +
      
      scale_linetype_manual(name = "", values = c(
        "Car change" = "dotted",
        "BinSeg" = "dashed",
        "PELT" =  "solid",
        "Time series" = "solid"
      )) +
      theme(legend.position = "bottom")
  }else{
    p <- p + geom_vline(aes(xintercept = change, 
                            color = "Euro introduced", linetype = "Euro introduced"),
                        linewidth = 0.8)
    p <- p +
      scale_color_manual(name = "", values = c(
        "Euro introduced" = "black",
        "BinSeg" = "orange",
        "PELT" =  "blue",
        "Time series" = "black"
      )) +
      
      scale_linetype_manual(name = "", values = c(
        "Euro introduced" = "dotted",
        "BinSeg" = "dashed",
        "PELT" =  "solid",
        "Time series" = "solid"
      )) +
      theme(legend.position = "bottom")
  }
  
  
  return(p)
}


# remove outliers in grau
graux <- grau[-which(grau$Verbrauch > 12 | grau$Verbrauch < 4),]
grauxNA <- graux[!is.na(graux$Verbrauch),]

m.pelt2 <- cpt.meanvar(grauxNA$Verbrauch, method = "PELT", minseglen = 10)
m.bs2 <- cpt.meanvar(grauxNA$Verbrauch, method = "BinSeg", minseglen = 10)

plot_pelt(grauxNA, book = "grau", value_col = "Verbrauch",
          cpt_obj = m.pelt2, binseg_obj = m.bs2)

# smoothing
m.pelt3 <- cpt.mean(rollmean(grau[!is.na(grau$Verbrauch),]$Verbrauch, k = 7), method = "PELT")
plot(m.pelt3, type = "l", cpt.col = "blue", xlab = "date", cpt.width = 4)

m.bs3 <- cpt.mean(rollmean(grau[!is.na(grau$Verbrauch),]$Verbrauch, k = 7), method = "BinSeg")
plot(m.bs3, type = "l", cpt.col = "blue", xlab = "date", cpt.width = 4)

# remove outliers in karr
karrx <- karr[-which(karr$Verbrauch > 12 | karr$Verbrauch < 4),]
karrxver <- karrx[!is.na(karrx$Verbrauch),]
m.pelt4 <- cpt.meanvar(karrxver$Verbrauch, method = "PELT", minseglen = 10)
# change points at 119 and 128
m.bs4 <- cpt.meanvar(karrxver$Verbrauch, method = "BinSeg", minseglen = 10)
# change points at 139

plot_pelt(karrxver, book = "karriert", value_col = "Verbrauch",
          cpt_obj = m.pelt4, binseg_obj = m.bs4)

# smoothing for karr
m.pelt5 <- cpt.mean(rollmean(karr[!is.na(karr$Verbrauch),]$Verbrauch, k = 7), method = "PELT")
plot(m.pelt5, type = "l", cpt.col = "blue", xlab = "date", cpt.width = 4)

m.bs5 <- cpt.mean(rollmean(karr[!is.na(karr$Verbrauch),]$Verbrauch, k = 7), method = "BinSeg")
plot(m.bs5, type = "l", cpt.col = "blue", xlab = "date", cpt.width = 4)


################################################################################
#=====================  trends in the petrol consumption  =====================#


# --- Plot for grau ---
p_grau <- ggplot(grauxNA, aes(x = Datum, y = Verbrauch)) +
  geom_line(aes(color = "Actual"), size = 0.4, alpha = 0.6) +
  geom_smooth(aes(color = "LOESS Trend"), method = "loess", se = FALSE, size = 0.8) +
  scale_color_manual(values = c("Actual" = "gray40", "LOESS Trend" = "blue")) +
  labs(
    title = "Trend in Petrol Consumption (grau)",
    subtitle = "LOESS smoothing applied to daily consumption",
    x = "Date",
    y = "L/100km",
    color = "Legend"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, face = "italic"),
    panel.grid.minor = element_blank()
  )

# --- Plot for karrx ---
p_karrx <- ggplot(karrxNA, aes(x = Datum, y = Verbrauch)) +
  geom_line(aes(color = "Actual"), size = 0.4, alpha = 0.6) +
  geom_smooth(aes(color = "LOESS Trend"), method = "loess", se = FALSE, size = 0.8) +
  scale_color_manual(values = c("Actual" = "gray40", "LOESS Trend" = "blue")) +
  labs(
    title = "Trend in Petrol Consumption (karr)",
    subtitle = "LOESS smoothing applied to daily consumption",
    x = "Date",
    y = "L/100km",
    color = "Legend"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, face = "italic"),
    panel.grid.minor = element_blank()
  )

# ggsave("E:/case_study/project_2/grau_trend.pdf",
#       plot = p_grau, device = cairo_pdf, width = 10, height = 6, units = "in")

# ggsave("E:/case_study/project_2/karrx_trend.pdf",
#       plot = p_karrx, device = cairo_pdf, width = 10, height = 6, units = "in")


################## Mann-Kendall trend test Petrol Consumption ##################
acf(grauxNA$Verbrauch, na.action = na.pass)
# autocorrelation present

mk_result <- mmkh(grauxNA$Verbrauch)
print(mk_result)

### p-value 0.0000008465
### Sen's slope = −0.0087 per time step > small, steady decline

acf(karrxNA$Verbrauch, na.action = na.pass)
# lag of 1 seems to be significant (reaches out of the blue band)

mk_result_karrx <- mmkh(karrxNA$Verbrauch)
print(mk_result_karrx)

### p-value 0.19144 not statistically significant


################################################################################
######################### change points in petrol prices (scaled) ##############


grauppl_scaled <- scale(grau$ppl)

m.pelt <- cpt.mean(as.vector(grauppl_scaled), method = "PELT")
m.peltbs <- cpt.mean(as.vector(grauppl_scaled), method = "BinSeg")
plot(m.peltbs, type = "l", cpt.col = "blue", xlab = "date", cpt.width = 4)
#no changepoints in price per liter, but steady incline

plot(grau$Datum,cumsum(grau$ppl), type = "l")

karrxppl <- karr[which(is.na(karr$ppl) == FALSE),]
karrppl_scaled <- scale(karrxppl$ppl)
m.pelt0 <- cpt.mean(as.vector(karrppl_scaled), method = "PELT")
plot(m.pelt0, type = "l", cpt.col = "blue", xlab = "date", cpt.width = 4)
m.pelt0bs <- cpt.mean(as.vector(karrppl_scaled), method = "BinSeg")
plot(m.pelt0bs, type = "l", cpt.col = "blue", xlab = "date", cpt.width = 4)


################################################################################
######################### change points in petrol prices #######################


m.pelt_org1 <- cpt.meanvar(grauxNA$ppl, method = "PELT", minseglen = 10)
m.bs_org1 <- cpt.meanvar(grauxNA$ppl, method = "BinSeg",  Q = 10,minseglen = 10)
plot_pelt(grauxNA, book = "grau", value_col = "ppl",
          cpt_obj = m.pelt_org1, binseg_obj = m.bs_org1)

m.pelt_org2 <- cpt.meanvar(karrxppl$ppl, method = "PELT", minseglen = 10)
m.bs_org2 <- cpt.meanvar(karrxppl$ppl, method = "BinSeg", Q = 15, minseglen = 10)
plot_pelt(karrxppl, book = "karriert", value_col = "ppl",
          cpt_obj = m.pelt_org2, binseg_obj = m.bs_org2)


################################################################################
#===================== trend detection in petrol prices =======================#


# --- Plot 1: grau ---
p1 <- ggplot(grau, aes(x = Datum, y = ppl)) +
  geom_line(aes(color = "Actual"), linewidth = 0.5) +
  geom_smooth(aes(color = "LOESS Trend"), method = "loess", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("Actual" = "gray70", "LOESS Trend" = "blue")) +
  labs(
    title = "Petrol Price Trend Over Time (grau)",
    subtitle = "LOESS smoothing shows general pattern in €/liter",
    x = "Date",
    y = "Price (€/liter)",
    color = "Legend"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

# --- Plot 2: karrxppl ---
p2 <- ggplot(karrxppl, aes(x = Datum, y = ppl)) +
  geom_line(aes(color = "Actual"), linewidth = 0.5) +
  geom_smooth(aes(color = "LOESS Trend"), method = "loess", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("Actual" = "gray70", "LOESS Trend" = "blue")) +
  labs(
    title = "Petrol Price Trend Over Time (karriert)",
    subtitle = "LOESS smoothing shows general pattern in €/liter",
    x = "Date",
    y = "Price (€/liter)",
    color = "Legend"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

# Save plots
ggsave("E:/case_study/project_2/petrol_price_trend_grau.pdf",
       plot = p1, device = cairo_pdf, width = 7, height = 3.5, units = "in")

ggsave("E:/case_study/project_2/petrol_price_trend_karrx.pdf",
       plot = p2, device = cairo_pdf, width = 7, height = 3.5, units = "in")


# Save as PNG with reduced resolution and dimensions
ggsave("E:/case_study/project_2/petrol_price_trend_grau.png",
       plot = p1, width = 5, height = 3.5, units = "in")

ggsave("E:/case_study/project_2/petrol_price_trend_karrx.png",
        plot = p2, width = 5, height = 3.5, units = "in")


################################################################################
################################# Mann-Kendall trend test Petrol Price #########


acf(grau$ppl)
# autocorrelation
mmkh(grau$ppl)
# p-value 0.000000000000000017499 significant

acf(karrxppl$ppl)
# autocorrelation

mmkh(karrxppl$ppl)
# p-value 0.0000072963 significant

grau$time_index <- as.numeric(grau$Datum)

model_grauppl <- lm(ppl ~ time_index, data = grau)
summary(model_grauppl)
# significant p-value, upwards trend

karrxppl$time_index <- as.numeric(karrxppl$Datum)
model_karrxppl <- lm(ppl ~ time_index, data = karrxppl)
summary(model_karrxppl)
# significant p-value, upwards trend


################################################################################
########################## Odometer reding before vs after VISUAL ##############


# Load libraries
library(readxl)

# Read original data
grau <- read_excel("E:/case_study/project_1/case_studies/project_2/Tankbuch_grau.xlsx")
karr <- read_excel("E:/case_study/project_1/case_studies/project_2/Tankbuch_karriert.xlsx")

# Store original data
grau_before <- grau
karr_before <- karr

# Convert dates
grau_before$Datum <- as.Date(grau_before$Datum)
karr_before$Datum <- as.Date(karr_before$Datum)

# Create corrected versions
grau <- grau_before
karr <- karr_before

# Apply known corrections
grau$Datum[215] <- as.Date("2006-08-12")
grau$Datum[368] <- as.Date("2014-05-26")
grau$Datum[382] <- as.Date("2015-01-29")
grau$Datum[383] <- as.Date("2015-03-12")

karr$Datum[36] <- as.Date("1987-06-09")
karr$Datum[38] <- as.Date("1987-06-13")
karr$Datum[37] <- median(c(as.Date("1987-06-13"), as.Date("1987-06-09")))
karr$Datum[275] <- as.Date("2002-07-04")
karr[78,]$Datum <- median(c(karr[77,]$Datum, karr[79,]$Datum))

# Define visual shadow offsets (no change to data)
x_offset_days <- 5
y_offset_km <- -300

# ===================== Grau Plot =====================
plot(grau$Datum, grau$Kilometerstand, type = "l", col = "green", lwd = 4.5,
     xlab = "Date", ylab = "grau Odometer reading",
     font.lab = 2, font.axis = 2, cex.lab = 1.3, cex.axis = 1.2,
     main = "Grau: Before vs After", cex.main = 1.4)

lines(grau_before$Datum + x_offset_days,
      grau_before$Kilometerstand + y_offset_km,
      col = "#FF6666", lwd = 2.0)

legend("topleft", legend = c("Before Correction", "After Correction"),
       col = c("#FF6666", "green"), lty = 1, lwd = c(2, 4.5),
       text.font = 2, cex = 0.9, bty = "n")


# ===================== Karr Plot =====================
plot(karr$Datum, karr$Kilometerstand, type = "l", col = "green", lwd = 4.5,
     xlab = "Date", ylab = "karr Odometer reading",
     font.lab = 2, font.axis = 2, cex.lab = 1.3, cex.axis = 1.2,
     main = "Karr: Before vs After", cex.main = 1.4)

lines(karr_before$Datum + x_offset_days,
      karr_before$Kilometerstand + y_offset_km,
      col = "#FF6666", lwd = 2.0)

legend("topleft", legend = c("Before Correction", "After Correction"),
       col = c("#FF6666", "green"), lty = 1, lwd = c(2, 4.5),
       text.font = 2, cex = 0.9, bty = "n")

