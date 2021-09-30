library("tidyverse")

n <- 10
set.seed(1000)
data <- tibble(y=rnorm(n),x=1:n)
f <- y ~ poly(x,n-1,raw=TRUE)
interpolate <- lm(f, data)
ggplot(data, aes(x,y)) + 
  geom_point() +
  geom_smooth(method="lm", formula = f, se = FALSE) +
  labs(y="Gaussian noise", title="Interpolating Gaussian noise",
       x = "Index") +
  theme_minimal()

ggsave("Interpolating.png")
