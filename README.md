# exercise-03
Third Excercise at ADA Course
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/data-wrangling.csv"

d <- read_csv(file = f, col_names = TRUE)
d
class(d)
str(d)
glimpse(d)
dim(d)
head(d)
tail(d)

names(d)
pi

d$Body_mass_male_mean
names(d)
d[6,7]

head(d)

BodySD <- select(d, Family, Genus, Body_mass_male_mean, Body_mass_female_mean)
head(BodySD)

#Create a new variable "sex_ratio"
d <- mutate(d, BSD = (Body_mass_male_mean/ Body_mass_female_mean))
head(d$BSD)

s <- select(d, Family, Genus, Body_mass_male_mean, Body_mass_female_mean, BSD)
head(s)

#Create a new variable "sex_ratio"

d <- mutate(d, sex_ratio = (AdultFemale/ AdultMales))
head(d$sex_ratio)

s <- select(d, Family, Genus, AdultFemale, AdultMales, sex_ratio)
head(s)

#Create a new variable "DI"

s <- select(d, HomeRange_km2)
head(s)
d <- mutate(d, DI = (DayLength_km/ HomeRange_km2))
head(d$DI)

s <- select(d, Family, Genus, DayLength_km, HomeRange_km2, DI)
head(s)

#Plot the relationship between DayLenght and time spent moving 

s <- select(d, Move)
head(s)

p <- ggplot(data = d, aes(x = log(Move), y =log(DayLength_km)))
p
(p + geom_point(na.rm = TRUE))
p <- p + geom_point(aes(color = factor(Family)), na.rm = TRUE)
p
p <- p + facet_wrap(aes(color = factor(Genus)), na.rm = TRUE)
p
p<- p + xlab("log(Time spent moving)") + ylab("log(Day range length)")
p <- p + theme(legend.position = "bottom", legend.title = element_blank())
p

### Plot the relationship between day range length and group size

p1 <- ggplot(data = d, aes(x = log(MeanGroupSize), y =log(DayLength_km)))
(p1 + geom_point(na.rm = TRUE))
p1 <- p1 + geom_point(aes(color = factor(Family)), na.rm = TRUE)
p1
p1<- p1 + xlab("log(Group Size)") + ylab("log(Day range length)")
p1 <- p1 + theme(legend.position = "bottom", legend.title = element_blank())
p1

### Plot the relationship between canine size dimorphism and body size dimorphism 

p2 <- ggplot(data = d, aes(x = log(MeanGroupSize), y =log(Canine_Dimorphism)))
(p2 + geom_point(na.rm = TRUE))
p2 <- p2 + geom_point(aes(color = factor(Family)), na.rm = TRUE)
p2
p2<- p2 + xlab("log(Group Size)") + ylab("log(Canine Dimorphism)")
p2 <- p2 + theme(legend.position = "bottom", legend.title = element_blank())
p2

### Create a new variable diet_strategy and plot a graphic

d <- mutate(d, diet_strategy = ifelse(Fruit >= 50, "frugivore", ifelse(Leaves >= 50, "folivore", ifelse(Fruit < 50 & Leaves < 50, "omnivore", NA))))
head(d$diet_strategy)

s <- select(d, Family, Genus, diet_strategy)
head(s)

p3 <- ggplot(data= d, aes (x = Family, y = diet_strategy))+geom_boxplot(na.rm = TRUE)
p3
p3 <- p3 + geom_point(aes(color = factor(diet_strategy)), na.rm = TRUE)
p3
p3<- p3 + xlab("log(Family)") + ylab("log(Diet Strategy)")
p3 <- p3 + theme(legend.position = "bottom", legend.title = element_blank())
p3

### dplyr and pipe 

s <- mutate(d, Binomial = paste(Genus, Species, sep = " ")) %>%
  select(Binomial, Family, Brain_Size_Species_Mean, Body_mass_male_mean) %>%
  group_by(Family) %>%
  summarise(avgB = mean(Brain_Size_Species_Mean, na.rm = TRUE), avgBM = mean(Body_mass_male_mean, na.rm = TRUE)) %>%
  arrange(desc(avgB))
s


