#data import and plot settings ----
base = read.csv("data/prepared/aggregated.csv")
str(base)
base$Date = as.Date(base$Date, format = "%Y-%m-%d")

base = base %>%
  mutate(Month = substr(as.character(Date), 1, 7),
         Calories_Ratio = Calories - Calories_Burned)


#color palette
mainBackground = rgb(22,24,36, maxColorValue = 255)
grid = rgb(42,44,56, maxColorValue = 255)
axis = rgb(150,150,150, maxColorValue = 255)
title = rgb(230,230,230, maxColorValue = 255)
text = rgb(200,200,200, maxColorValue = 255)

update_geom_defaults("point",   list(colour = "lightblue"))
update_geom_defaults("line",   list(colour = "lightblue"))
options(scipen=999) #prevent scientific notions

##$&Now it is time to create the custom theme. I tried to apply the same design as the one I used on my website. However, it is pretty easy to change most of the parameters thanks to the color palette we defined previously.

customTheme = theme(panel.background = element_rect(fill = mainBackground, 
                                                    color = mainBackground),
                    plot.background = element_rect(fill = mainBackground, 
                                                  color = mainBackground),
                    panel.grid = element_line(color = grid),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    plot.title = element_text(color = title, size = 14, face = "bold", 
                                              margin = margin(t = 10)),
                    plot.subtitle = element_text(color = text, size = 11, margin = 
                                                 margin(t = 5)),
                    plot.caption = element_text(color = axis, size = 7),
                    legend.background = element_rect(fill = "transparent"),
                    legend.title = element_text(color = text, size = 9, face = "bold"),
                    legend.text = element_text(color = axis, size = 9),
                    legend.position = "top", 
                    legend.justification = "left",
                    legend.box.background = element_blank(),
                    legend.box = "horizontal",
                    legend.direction="horizontal",
                    legend.key = element_rect(fill = "transparent", size = 5),
                    axis.ticks.x = element_blank(),
                    axis.line = element_line(color = mainBackground),
                    axis.text = element_text(color = axis),
                    axis.title = element_text(color = text, size = 9, face = "bold"),
                    axis.text.y = element_text(margin = margin(r = 10)),
                    axis.text.y.right = element_text(margin = margin(l = 10)),
                    axis.text.x = element_text(margin = margin(t = 10)),
                    axis.title.y = element_text(margin = margin(r = 10, l = 10)),
                    axis.title.y.right = element_text(margin = margin(l = 10, r = 10)),
                    axis.title.x = element_text(margin = margin(t = 10)))


#water consumption
filter(base, !is.na(Water_Amount)) %>%
ggplot() +
  geom_jitter(mapping = aes(y = Water_Amount, x = Date, color = Water_Amount > 2000),
              size = 3) +
  coord_cartesian(ylim=c(0, 4000)) + 
  labs(title = "Water Consumption", 
       subtitle = "Weight Loss Journey", 
       y = "Water Amount in ml", x = "", 
       caption = "Andrea's Health Trip") +
  scale_color_discrete(name = "Min. Consumption (2 Liter)", labels = c("Failed", "Reached"),
                       type = RColorBrewer::brewer.pal(n = 3, name = "Set2")) +
  scale_x_date(date_labels = "%b %Y") +
  customTheme

#ggsave("waterConsumption.png")

#nutrition
filter(base, Date > as.Date("2020-10-05")) %>%
ggplot2::ggplot(mapping = aes(x = Date)) +
  geom_bar(mapping = aes(y = Calories, fill = Calories), stat = "identity") +
  geom_line(mapping = aes(y = Food_Amount * 1.1, color = Food_Amount), size = 1) +
  labs(title = "Nutrition in Calories and Gram",
       subtitle = "Weight Loss Journey",
       y = "Calories", x = "",
       colour = "Amount in Gram") +
  coord_cartesian(ylim = c(1000,3600)) +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 1.1, name = "Amount in Gram")
  ) +
  scale_color_gradient(name = "Amount in Gram", labels = NULL,
                       low = "ivory2", high = "ivory4") +
  scale_fill_gradient(name = "Calories", labels = NULL) +
  customTheme

#ggsave("caloriesGram.png")

#calories
ggplot(data = base, aes(x = Date)) + 
  stat_smooth(mapping = aes(y = Calories), geom = 'area', 
              method = 'loess', span = 1/100,
              alpha = 1, fill = "lightblue") +
  stat_smooth(mapping = aes(y = Calories_Burned), geom = 'area',
              method = 'loess', span = 1/100,
              alpha = 1, fill = "lightskyblue4") +
  geom_smooth(mapping = aes(y = Calories_Ratio, color = "Balance"),
              size = 2, se = F) +
  scale_x_date(date_labels = "%b %Y") +
  labs(title = "Calories Balance",
       subtitle = "Weight Loss Journey",
       y = "Calories", x = "",
       colour = "Unit") +
  scale_color_manual(name = "Value", 
                     values = c("Ingested" = "lightblue", 
                                "Burned" = "lightblue2", 
                                "Balance" = "gold2")) +
  customTheme

#ggsave("caloriesBalance.png")

#steps
filter(base, Date > as.Date("2020-11-04")) %>%
ggplot(mapping =  aes(x = Date)) +
  geom_path(mapping = aes(y = Steps), size = 2, color = "lightblue") +
  geom_smooth(mapping = aes(y = Steps), method = "lm",
              formula = y ~ poly(x, 10), se = T, color = "orange", size = 2) +
  scale_x_date(date_labels = "%b %Y") +
  labs(title = "Steps per Day Development",
       subtitle = "Weight Loss Journey",
       y = "Steps", x = "") +
  customTheme

#ggsave("steps.png")

#weight development
filter(base, Date > as.Date("2020-01-03")) %>%
ggplot(mapping = aes(x = Date)) +
  geom_smooth(mapping = aes(y = Weight), size = 2, color = "lightblue2") +
  scale_x_date(date_labels = "%b %Y") +
  labs(title = "Weight Development",
       subtitle = "Weight Loss Journey",
       y = "Weight", x = "") +
  customTheme

#ggsave("weight.png")

#active minutes by activity type
filter(base, Date > as.Date("2020-05-03")) %>%
ggplot(mapping = aes(x = Date)) +
  stat_smooth(mapping = aes(y = Duration_Minutes), geom = 'area',
              method = 'loess', span = 1/100,
              alpha = 1, fill = "dodgerblue") +
  geom_point(mapping = aes(y = Duration_Minutes), color = "gray") +
  geom_smooth(mapping = aes(y = Duration_Minutes), color = "orange", size = 1.5) +
  scale_x_date(date_labels = "%b %Y") +
  labs(title = "Active Minutes (incl. Walking, Running, etc.)",
       subtitle = "Weight Loss Journey",
       y = "Minutes", x = "") +
  customTheme

#ggsave("activeMinutes.png")

#get exercise data
exercise = read.csv("data/prepared/exercise.csv")
str(exercise)
exercise = mutate(exercise, Date = as.Date(Date, format = "%Y-%m-%d"))

#active minutes by activity type
filter(exercise, Activity_Name %in% 
                 c("Aerobic", "Laufen", "Pilates", "Radfahren")) %>%
ggplot(mapping = aes(x = Date)) +
  geom_bar(mapping = aes(y = Duration_Minutes, fill = Activity_Name),
           stat = "identity") +
  labs(title = "Active Minutes by Activity",
       subtitle = "Weight Loss Journey",
       y = "Minutes", x = "") +
  scale_fill_manual(name = "Activity",
                    values = c("gold2", "dodgerblue", "#94d687", "#d68787"),
                    labels = c("Aerobic", "Running", "Pilates", "Cycling")) +
  customTheme

#ggsave("activities.png")

filter(exercise, Activity_Name == "Gehen") %>%
ggplot(mapping = aes(x = Date)) +
  geom_bar(mapping = aes(y = Duration_Minutes),
           stat = "identity", fill = "dodgerblue2") +
  geom_smooth(mapping = aes(y = Steps / 30),
              size = 1.5, color = "gray",
              method = "loess", span = 0.1) +
  labs(title = "Walking Minutes and Steps",
       subtitle = "Weight Loss Journey",
       y = "Minutes", x = "") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 1.1, name = "Steps")
  ) +
  customTheme

#ggsave("walking.png")

#create nutrition distribution data frame
nutritionDistr = as.data.frame(matrix(data = NA, 
                                      nrow = (3* nrow(base)),
                                      ncol = 0)) %>%
  mutate(Date = rep(base$Date,3),
         Month = substr(Date, 1, 7),
         Food_Amount = c(base$Carbs, base$Protein, base$Fat),
         Type = c(rep("Carbs", nrow(base)), 
                  rep("Protein", nrow(base)),
                  rep("Fat", nrow(base))),
         Type = ifelse(is.na(Food_Amount), NA, Type))

#food ingredients
ggplot(data = nutritionDistr) +
  geom_bar(mapping = aes(x = "", y = Food_Amount, fill = Type), stat = "identity") +
  coord_polar("y", start=0) +
  facet_wrap(~ Month, ncol = 4, dir = "h") +
  scale_fill_manual(values=c("dodgerblue", "lightblue", "gold2")) +
  labs(title = "Food Ingredients per Month",
       subtitle = "Weight Loss Journey",
       y = "", x = "") +
  customTheme +
  theme(legend.position = "top",
        legend.justification=c(0, 1),
        legend.direction = "horizontal",
        axis.text.x=element_blank())

#ggsave("nutritionDistribution.png")