library(hexSticker)
library(tidyverse)

# Linear Regression 1
p <- ggplot(data = mtcars, 
         aes(x = log(hp), y = mpg)) +
  geom_point(size = 0.6, color = "black", fill = "white") +
  geom_smooth(method='lm', formula= y~x, se = FALSE, size =0.3, col = "red") +
  theme_void() + 
  theme_transparent()

p

# Linear Regression 2
p <- ggplot(data = mtcars, 
            aes(x = disp, y = mpg)) +
  geom_point(size = 0.6, color = "black", fill = "white") +
  geom_smooth(method='lm', formula= y~I(x^9) + I(x^7) + I(x^5) + I(x^3), se = FALSE, size =0.3, col = "red") +
  theme_void() + 
  theme_transparent()

p

# Linear Regression 3
lstat_sorted <- sort(MASS::Boston$lstat)

y <- rep(0, 253)
x <- rep(0, 253)

for (i in 1:253){
  
  y[i] <- lstat_sorted[507-i] - median(lstat_sorted, na.rm = TRUE)
  
  x[i] <- median(lstat_sorted, na.rm = TRUE) - lstat_sorted[0+i] 
  
}

p <- tibble(below = x,
       above = y) %>%
  ggplot(aes(x = below, y = above)) +
  geom_point(size = 0.1, color = "black", fill = "white") + 
  geom_abline(intercept = 0, slope = 1, size = 0.2, color = "red") +
  theme_void() + 
  theme_transparent()

p

# Binary Regression 1
p <- 
  ggplot(data = mtcars,
        aes(x = mpg, y = am)) +
  geom_point(size = 0.6, color = "black", fill = "white") +
  stat_smooth(method="glm", method.args=list(family=binomial(link="logit")),
              se = F,size =0.3, col = "red") +
  theme_void() + 
  theme_transparent()

p

# Panel Data Analysis 1
data("Grunfeld", package="plm")
fe_model_lm <- lm(inv ~ capital + factor(firm), data = Grunfeld)

p <- 
  Grunfeld %>%
  group_by(firm) %>%
  summarise(inv_mean = mean(inv)) %>%
  left_join(Grunfeld) %>%
  ggplot(data = ., 
         aes(x = reorder(as.character(firm), firm), y = inv)) +
  geom_point(size = 0.2) +
  geom_line(aes(x = firm, y = inv_mean), col = "red", size =0.2) +
  theme_void() + 
  theme_transparent() +
  theme(legend.position = "none")
p

# Statistical Inference 1
p <- tibble(x = rnorm(1:1000000)) %>%
  ggplot(., aes(x=x)) + 
  geom_density() +
  theme_void() + 
  theme_transparent()

p 

# RDD
carpenter_dobkin_2009 <- readRDS("carpenter_dobkin_2009")

p <- 
  carpenter_dobkin_2009 %>% 
  ggplot(aes(x = agecell, y = all)) + 
  geom_point(size = 0.3) +
  geom_vline(xintercept = 21, color = "red", size = 0.3, linetype = "dashed") + 
  theme_void() + 
  theme_transparent()
 
p

# DiD
card_krueger_1994_mod <- readRDS("card_krueger_1994") %>%
  mutate(emptot = empft + nmgrs + 0.5 * emppt,
         pct_fte = empft / emptot * 100)

differences <- card_krueger_1994_mod %>%
  group_by(observation, state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE))

# Treatment group (NJ) before treatment
njfeb <- differences[1,3]

# Control group (PA) before treatment
pafeb <- differences[2,3]

# Treatment group (NJ) after treatment
njnov <- differences[3,3]

# Control group (PA) after treatment
panov <- differences[4,3]

nj_counterfactual <- tibble(
  observation = c("February 1992","November 1992"), 
  state = c("New Jersey (Counterfactual)","New Jersey (Counterfactual)"),
  emptot = as.numeric(c(njfeb, njfeb-(pafeb-panov)))
) 

# Data points for treatment event
intervention <- tibble(
  observation = c("Intervention", "Intervention", "Intervention"),
  state = c("New Jersey", "Pennsylvania", "New Jersey (Counterfactual)"),
  emptot = c(19.35, 22.3, 19.35)
) 

# Combine data
did_plotdata <- bind_rows(differences, 
                          nj_counterfactual, 
                          intervention)

p <- 
  did_plotdata %>%
  mutate(label = if_else(observation == "November 1992", as.character(state), NA_character_)) %>%
  ggplot(aes(x=observation,y=emptot, group=state)) +
  geom_line(aes(color=state), size=0.3) +
  geom_vline(xintercept = "Intervention", linetype="dotted", 
             color = "black", size=0.3) + 
  scale_color_manual(values = c("#ff9933", "#ff0033","#0000ff")) +
  guides(color=FALSE) +
  theme_void() + 
  theme_transparent()

p
 # Sticker
s <- sticker(p, package="LinReg2", p_size=20, s_x=1, s_y=.85, s_width=1.3, s_height=0.7,
             h_fill="#1B9E77", h_color="#000000",  p_color = "#ffffff",
             filename="LinReg2.png")


plot(s)


