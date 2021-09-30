library("tidyverse")
library("mgcv")
library("gratia")
library("lindia")
library("forecast")

AusTemp <- read_delim("http://www.bom.gov.au/web01/ncc/www/cli_chg/timeseries/tmean/0112/aus/latest.txt",
                      delim=" ",
                      col_names = c("Year", "Temp"),
                      col_types = "cd",
                      trim_ws = TRUE) %>%
           mutate(Year = substr(Year, 0, 4)) %>%
           mutate(Year = as.numeric(Year)) %>%
           filter(Year < 2020) #for backwards compatibility, remove years after 2019

ggplot(AusTemp, aes(Year,Temp)) +
  geom_hline(yintercept = 0, colour="grey") +
  geom_line() +
  labs(y="°C", title="Annual Australian temperature anomaly",
       subtitle = "Reference period: 1961-1990") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust=0.5))

ggsave("TempLine.png")

TempGAM <- gam(Temp ~ s(Year, bs="cr"), data=AusTemp)
summary(TempGAM)
draw(TempGAM, residuals = TRUE)
ggsave("TempGAMFit.png")
g <- appraise(TempGAM)
ggsave("TempGAMDiag.png", g, width = 5.59*2, height = 3.64*2)

plot_grid(ggAcf(AusTemp$Temp), ggAcf(TempGAM$residuals))
ggsave("TempGAMACF.png", width = 5.59*2, height = 3.64)

AusTemp$YearSq <- AusTemp$Year**2
TempLM <- lm(Temp ~ Year, data=AusTemp)
g <- gg_diagnose(TempLM, theme = theme_minimal())
ggsave("TempLMDiag.png", g, width = 5.59*2, height = 3.64*4)

TempLM2 <- lm(Temp ~ Year + YearSq, data=AusTemp)
g <- gg_diagnose(TempLM2, theme = theme_minimal())
ggsave("TempLM2Diag.png", g, width = 5.59*2, height = 3.64*4)

ggplot(AusTemp, aes(Year,Temp)) +
  geom_hline(yintercept = 0, colour="grey") +
  geom_line() +
  labs(y="°C", title="Annual Australian temperature anomaly",
       subtitle = "Reference period: 1961-1990") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust=0.5)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, colour="orange") +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), se = FALSE)
ggsave("TempGAMvsLM.png")

deriv <- derivatives(TempGAM)
deriv$sigIncreasing <- (deriv$lower > 0)
deriv$sigIncreasing <- ifelse(deriv$sigIncreasing, deriv$derivative, NA)
deriv$sigDecreasing <- (deriv$upper < 0)
deriv$sigDecreasing <- ifelse(deriv$sigDecreasing, deriv$derivative, NA)
deriv$Year <- deriv$data
ggplot(deriv, aes(x = Year, y = derivative)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.3, fill = "grey") +
  geom_line() +
  geom_line(aes(y = sigIncreasing), size = 1.5) +
  #geom_line(aes(y = sigDecreasing), size = 1.5) +
  ylab(expression(italic(hat(f) * "'") * (Year))) +
  xlab("Year") +
  theme_minimal()
ggsave("TempGAMDeriv.png")
