library(ggplot2)
setwd("path")

# Load the database from a local directory
db = read.csv("database.csv")

### Figure 1. Age-standardized leukemia mortality rates (world standard population) 
### per 100,000 among boys and girls 0-14 years of age
### from 15 Latin American countries between 2013 and 2017
figure_1 = ggplot(db, aes(x = reorder (country, rates), y = rates)) +
  geom_bar (stat = "identity", color = "black", fill =  "black", alpha = 0.7) + 
  geom_text(aes(label = round(rates,2)), vjust= 0.5, hjust = -0.5, size=3.5)+
  theme_classic() + 
  labs(title = title)+
  xlab (ylab) + 
  ylab ("Age-standardized\nmortality rates") +
  scale_y_continuous (breaks = c(0,1,2,3,4), limits = c(0,4.5)) +
  coord_flip ()

### Figure 2. Percent changes between the periods 
### 2000-2005 and 2012-2017 in Latin American countries 
figure_2 = ggplot(db, aes(x = reorder (country, rates), y = rates, fill = sex)) +
  geom_bar (stat = "identity", alpha = .8)+
  geom_text (aes(label = round(rates,1), y = lab2), hjust=-.15, size = 2)+
  scale_y_continuous (breaks = c(-120,-90,-60,-30, 0, 30), limits = c(-145,48)) +
  scale_fill_manual(name = "Sex", values = c("darkgoldenrod1", "blue4"))+
  ylab("Percentage change")+
  xlab("Countries")+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 7))

### Figure 3. Leukemia mortality trends in children from 15 Latin American 
### countries 2000-2017
figure_3 = db %>%
  ggplot(aes(year, rates, shape = sex, color = sex, lintype = sex))+
  geom_smooth(se = F, alpha = .7, size = .5)+
  geom_point(size = 2)+
  scale_shape_manual(values = c(2, 1), 
                     guide = guide_legend(override.aes = 
                                          list(colour = c("darkgoldenrod1", "blue4"))))+
  scale_colour_manual(values = c("darkgoldenrod1", "blue4"))+
  scale_x_continuous(breaks = c(2000,2009,2017), limits = c(1998,2019))+
  ylab("Age-standardized\nmortality rates\n")+
  xlab("\nYear")+
  theme_bw()+
  facet_wrap(~country)+
  guides(color = F)+
  labs(shape ="Sex")+
  theme(
    axis.text.x = element_text (size = 8),
    legend.position = "bottom")

### Figure 4. Estimated annual percent change (EAPC, %) and 95% confidence interval (CI) 
### for leukemia mortality rates among children in Latin America and the Caribbean
figure_4 = ggplot(db, aes(reorder(country, trend), trend))+
  geom_pointrange(aes(ymin = low, ymax = up), color = "black", alpha = .7,
                  size = .35)+
  geom_errorbar(aes(ymin = low, ymax = up), color = "darkblue")+
  geom_hline(yintercept = 0, alpha = 1, color = "brown4", linetype = "dashed")+
  scale_y_continuous(breaks = c(-30,-25,-20,-15,-10, -5, 0, 5, 10), 
                     limits = c(-30,10))+
  theme_classic()+
  coord_flip()+
  xlab(xlab)+
  ylab("EAPC")+
  ggtitle(titulo)+
  theme(text = element_text(size = 9))