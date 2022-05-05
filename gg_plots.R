#Load libraries. Install them with install.packages() if not found
library("ggplot2")
library("patchwork")
library("tidyverse")
library("ggpubr")

#Read data
readfile <- read.csv("rooks.csv", row.names = 1,sep=";",header=F)
obs<-as.data.frame(t(readfile))
obs$Music <- as.factor(obs$Music)
obs$Year <- as.factor(obs$Year)

#Reshape
obs_l <- pivot_longer(obs, -c(Day, Week, Year, Music), values_to = "Observations", names_to = "Behavior")

#Normalize
n20 = 1
n21 = 1
obs20 <- filter(obs_l, Year == 2020)
obs21 <- filter(obs_l, Year == 2021)
obs20$Observations <- obs20$Observations / n20
obs21$Observations <- obs21$Observations / n21
obs_n <- rbind(obs20, obs21)

#Filter
social <- c("Getting nearer", "Shared grooming", "Agression")
auto <- c ("Beak cleaning", "Grooming", "Eating")
exploration <- c("Hopping", "Pecking objects")
obs_social <- filter(obs_n, Behavior %in% social)
obs_auto <- filter(obs_n, Behavior %in% auto)
obs_explor <- filter(obs_n, Behavior %in% exploration)

#Line graphs by day
weeks <- c(0, 7.5, 14.5, 21.5, 28.5, 35)
social1 <- ggplot(obs_social, aes(x = Day, y = Observations)) + 
    scale_x_continuous(breaks = seq(0,34, by = 7), expand = c(0, 0)) + scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(0, .1))) +
	geom_line(aes(linetype = Behavior, color = Behavior)) +
	geom_point(aes(shape = Behavior, color = Behavior)) +
	scale_color_brewer(palette = "Dark2") +
	annotate (geom = "rect", xmin =7, ymin = -Inf, xmax = 14, ymax = Inf, alpha =0.2, fill = "gray") + 
	annotate (geom = "rect", xmin =21, ymin = -Inf, xmax = 28, ymax = Inf, alpha =0.2, fill = "gray") + 
	ylab("Social behavior") +
    facet_grid (.~Year) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(linetype = "dotted", color = "black"))

auto1 <- ggplot(obs_auto, aes(x = Day, y = Observations)) + 
    scale_x_continuous(breaks = seq(0,34, by = 7), expand = c(0, 0)) + scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(0, .1))) +
	geom_line(aes(linetype = Behavior, color = Behavior)) +
	geom_point(aes(shape = Behavior, color = Behavior)) +
	scale_color_brewer(palette = "Dark2") +
	annotate (geom = "rect", xmin =7, ymin = -Inf, xmax = 14, ymax = Inf, alpha =0.2, fill = "gray") + 
	annotate (geom = "rect", xmin =21, ymin = -Inf, xmax = 28, ymax = Inf, alpha =0.2, fill = "gray") + 
	ylab("Autobehavior") +
    facet_grid (.~Year) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(linetype = "dotted", color = "black"))
    
explor1 <- ggplot(obs_explor, aes(x = Day, y = Observations)) + 
    scale_x_continuous(breaks = seq(0,34, by = 7), expand = c(0, 0)) + scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(0, .1))) +
	geom_line(aes(linetype = Behavior, color = Behavior)) +
	geom_point(aes(shape = Behavior, color = Behavior)) +
	scale_color_brewer(palette = "Dark2") +
	annotate (geom = "rect", xmin =7, ymin = -Inf, xmax = 14, ymax = Inf, alpha =0.2, fill = "gray") + 
	annotate (geom = "rect", xmin =21, ymin = -Inf, xmax = 28, ymax = Inf, alpha =0.2, fill = "gray") + 
	ylab("Exploration") +
    facet_grid (.~Year) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(linetype = "dotted", color = "black"))    

figure1 <- ggarrange(social1, auto1, explor1, labels = c("A", "B","C"), ncol =1, nrow =3)
ggsave("lines.pdf", width = 20, height = 27)  

#Points
show_tests = list(c(1,2), c(2,3), c(3,4), c(4,5))
social_p1 <- ggplot(obs, aes(group = Week, x = Week, y = `Getting nearer`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	stat_compare_means(comparisons = show_tests) +
	facet_grid (.~Year) +
	theme_bw() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank())
	
social_p2 <- ggplot(obs, aes(group = Week, x = Week, y = `Shared grooming`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	stat_compare_means(comparisons = show_tests) +
	facet_grid (.~Year) +
	theme_bw() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank())
  
social_p3 <- ggplot(obs, aes(group = Week, x = Week, y = `Agression`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75) +
	stat_compare_means(comparisons = show_tests) +
	facet_grid (.~Year) +
	theme_bw() +
	theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank())

social2 <- social_p1 + social_p2 + social_p3

figure2 <- ggarrange(social1, social2, labels = c("A", "B"), ncol =1, nrow =2)
ggsave("social.pdf", width = 20, height = 20)  

