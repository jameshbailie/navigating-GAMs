library("tidyverse")
library("mgcv")
library("gratia")

df <- bird_move %>% filter(species == "sp1")
df <- df[rep(row.names(df), df$count), 1:2]

ggplot(df, aes(x=week, y=latitude)) +
  geom_jitter() +
  theme_minimal() +
  labs(title="Observations of tagged birds (species A) by time and location")
ggsave("species1.png")

birdGAM <- gam(count ~ te(week, latitude, bs=c("cc", "tp"), k=c(10, 10)),
    data=bird_move, method="REML", family="poisson")
draw(birdGAM)
ggsave("birdGAM.png")

g <- appraise(birdGAM)
ggsave("birdGAMDiag.png", g, width = 5.59*2, height = 3.64*2)

