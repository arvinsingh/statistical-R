######################################################
#                                                    #
# Workshop 3                                         #
# Statistical Modelling in R and Data Visualisation  #
#                                                    #
######################################################

## Remember anything after "#" is comments
## I've included comments to make it easier for us both

#####################################################
### Part 1: Statistical Modelling in R
#####################################################

###############################################################################
###############################################################################
### Linear Regression (Linear)

## Import the data - copy and paste from excel "income.data"
income.data <- read.csv("clipboard", header = TRUE, sep = "\t")
attach(income.data)
names(income.data)
summary(income.data)
## This gives us a numeric summary table

#### Making sure your data meet the assumptions
## Checking assumptions

# 2.Normality of dependent variable
hist(happiness)
# observations are roughly bell-shaped,
# so we can proceed with the linear regression

# 3.Testing for Linearity
plot(happiness ~ income, data = income.data)
# the relationship looks roughly linear, so we can proceed with the linear model

## Running the linear model (PP)
income.happiness.lm <- lm(happiness ~ income, data = income.data)
summary(income.happiness.lm)
## interpretation of output (PP)

## 4.Check for homoscedasticity
par(mfrow = c(2, 2))
plot(income.happiness.lm)
par(mfrow = c(1, 1))
##The par(mfrow()) code will divide the plots window
##in the number of rows and columns specified in the brackets
##Makes it easily to see the graphs together
##What do these graphs mean? (PP)

detach(income.data)
rm(income.data)
## rm command removes models and datasets from your global environment
rm(income.happiness.lm)

###############################################################################
## For you to try: linear regression

## Hypothesis: There will be a relationship between tree height and diameter
## Prediction: The diameter of the tree will
## increase as the height of the tree increases

## load data - built in R so not the same as copy and pasting from excel
data(trees)
names(trees)
## Girth is diameter
head(trees)
## shows the first 6 rows of data in the dataset
summary(trees)
attach(trees)

#### Making sure your data meet the assumptions
# Normality of dependent variable
hist(Height)
# observations are roughly bell-shaped,
# so we can proceed with the linear regression
# Linearity
plot(Height ~ Girth, data = trees)
# the relationship looks roughly linear, so we can proceed with the linear model

## Running the linear model (PP)
trees.lm <- lm(Height ~ Girth, data = trees)
summary(trees.lm)
## significant positive relationship, as height increases girth increases
## for each unit increase in height,
## girth increases by 1.05 +/- 0.32 (p<0.001 or p<0.05)
## Check for homoscedasticity 
par(mfrow = c(2, 2))
plot(trees.lm)
par(mfrow = c(1, 1))
##Not as good as the previous model,
##but this is still alright (bit of a judgement call)
##Perhaps a result of fewer observations (PP)

detach(trees)
rm(trees)
rm(trees.lm)

#############################################################################
## Multiple linear regression

## Import the data - copy and paste from excel "heart.data"
heart.data <- read.csv("clipboard", header = TRUE, sep = "\t")
attach(heart.data)
names(heart.data)
summary(heart.data)
## This gives us a numeric summary table

## Making sure assumptions are met

## 1.Independence of observations
## test both independent (explanatory) variables
cor(biking, smoking)
## output is 0.015, which means the correlation between
## biking and smoking is small (0.015 or 1.5%),
## so we can include both parameters in our model
## this is also a bit of judgement call on the cut of line for "what is good"
## read up on this online for more information

## 2.Normality
## test dependent variable follows a normal distribution
hist(heart.disease)
## the distribution of observations is roughy bell-shaped
## so we can proceed with the linear regression

## 3.Linearity
## we can check this using two scatterplots (for both independent variables):
## one for biking and heart disease, and one for smoking and heart disease
## biking and heart disease
plot(heart.disease ~ biking, data=heart.data)
## looks linear
## smoking and heart disease
plot(heart.disease ~ smoking, data=heart.data)
## although the relationship between smoking and
## heart disease is a bit less clear it still appears linear
## we can proceed with lienar regression

## Running your linear model
heart.disease.lm <- lm(heart.disease ~ biking + smoking, data = heart.data)
summary(heart.disease.lm)
## interpretation of output (PP)

## 4.Check for homoscedasticity
par(mfrow = c(2, 2))
plot(heart.disease.lm)
par(mfrow = c(1, 1))
##What do these graphs mean? (PP)

### multiple linear regression with an interaction
## running the model
heart.disease.lm2 <- lm(heart.disease ~ biking * smoking, data = heart.data)
summary(heart.disease.lm2)
## interpretation of the output (PP)

detach(heart.data)
rm(heart.data)
rm(heart.disease.lm)
rm(heart.disease.lm2)

###############################################################################
###############################################################################
## Generalised linear regression: Poisson Regression example

## input the data - using a dataset built into R so no need to copy and paste
## just calling the data from within R

wooldata <- warpbreaks
##pulling the dataset "warpbreaks" and calling it wooldata
print(head(wooldata))
##print shows you the dataset
attach(wooldata)
names(wooldata)

##checking assumptions of the response variable 'breaks'
hist(breaks)
##data is skewed to the left
shapiro.test(breaks)
##data is not normally distributed
##as we have count data, and assumptions of lm
##not met we are justified to use glm with poisson family

##running the model (PP)
m1 <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson)
summary(m1)
##interpretation the output (PP)

##checking model outputs
##checking for overdispersion
residual_deviance = summary(m1)$deviance
residual_df = summary(m1)$df.residual
dispersion = residual_deviance / residual_df
dispersion
##bit of a judgement call with the results - 4.21 is high shows over dispersion
##good practice to report this in your write up (PP)
##as this is a made up dataset this isn't surprising -
##can potential fix with running a quasipoisson model
##read up on quasipoisson outside this class -
##would just add quasipoisson to the family instead of poisson

##Chi-square goodness of fit
pchisq(210.39, 50, lower.tail = FALSE)
##The p-value for this test is is much smaller than the
##significance level of 0.05. We can conclude that
##the data doesn't fit the model reasonably well

## writing up results (pp)

rm(m1)
rm(wooldata)
rm(warpbreaks)

###############################################################################
###############################################################################
### Mixed Modelling

###############################################################################
### Linear mixed modelling

## Load data - need to download data from the link
## in the powerpoint and open with Rstudios
attach(dragons)
head(dragons)

## Let's say we want to know how the body length affects test scores.

## Have a look at the data distribution
## You don't need to worry about the distribution of your explanatory variables
## Have a look at the distribution of the response variable:

hist(testScore)  # seems close to normal distribution - good!

## It is good practice to  standardise your explanatory variables
## before proceeding - you can use scale() to do that:
## This is so that they have a mean of zero ("centering) and
## standard deviation of one ("scaling")
## It ensures that the estimated coefficients are all on the same scale,
## making it easier to compare effect sizes
## This is needed for this example, but in general not always needed

dragons$bodyLength2 <- scale(dragons$bodyLength)

## Back to our question: is test score affected by body length?
## One way to analyse this data would be to try fitting a linear model
## to all our data, ignoring the sites and the mountain ranges for now.

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)
## what does this tell us (PP)

### Assumptions?
## Plot the residuals - the red line should be close to being flat,
##like the dashed grey line
plot(basic.lm, which = 1)  # not perfect
## Have a quick look at the  qqplot too - point should
## ideally fall onto the diagonal dashed line
# a bit off at the extremes, but that's often the case;
# again doesn't look too bad

plot(basic.lm, which = 2)

## However, what about observation independence?
## Are our data independent? (number 1)
## We collected multiple samples from eight mountain ranges
## It's perfectly plausible that the data from within each
## mountain range are more similar to each other than the data from
## different mountain ranges - they are correlated.
## So what do we do? (PP)

## Modify the model

## We want to use all the data, but account for the data
## coming from different mountain ranges
## let's add mountain range as a fixed effect to our basic.lm
mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)
## can see that now body length is not significant, but each moutain range is
## but let's think about what we are doing here for a second (PP)

### Running our linear mixed model
### two packages you need to install and load

install.packages(lme4)
install.packages(lmetTest)
## remember you only need to install a package once in R
## you need to load using the library function below in each session

library(lme4) ## to allow you run a mixed model
library(lmerTest) ##to get pvalues in your model output

mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

##run an anova between the lm and lmer to get p-value of random effects
anova(mixed.lmer, basic.lm)

##interpretating this outcome (PP)

## checking our model assumptions

plot(mixed.lmer)  # looks alright, no patterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

detach(dragons)
rm(mountain.lm)
rm(mixed.lmer)
rm(basic.lm)
rm (dragons)

##############################################################################
##############################################################################

#####################################################
### Part 2: Visualising Data in R
#####################################################

###############################################################################
## scatter plots

## Using "StudyTimevsExam" excel sheet
## load data
exams <- read.csv("clipboard", header = TRUE, sep = "\t")
attach(exams)
names(exams)

## simple scatter plpt
plot(Time, Score)

## plots the relationship, but not the most visually appealing is it!
## ggplot2 is a package that you can easy change
## elements of a graph and build up what you want

install.packages("ggplot2")
library(ggplot2)

# Basic scatter plot
ggplot(exams, aes(x = Time, y = Score)) + geom_point()

# Change the point size, and shape
ggplot(exams, aes(x = Time, y = Score)) +
  geom_point(size = 3, shape = 21)

## can look online for "cheatsheets" that
## list all the possible size and shape options


ggplot(exams, aes(x = Time, y = Score, color = "#E69F00")) +
  geom_point(size = 3, shape = 22) +
  labs(title = "Time spent study vs exam score",
       x = "Time spent studying (hours)", y = "Exam score (out of 100)") +
  theme_classic()

detach(exams)
rm(exams)

## can also axis breaks, change the text size, lots of things!

###############################################################################
### For you to try: Scatter plot

## Using "Video_Puzzle_R" excel sheet
## load data
puzzles <- read.csv("clipboard", header = TRUE, sep = "\t")
attach(puzzles)
names(puzzles)

## simple scatter plot
plot(Video.Score, Puzzle.Score)

ggplot(puzzles, aes(x = Video.Score, y = Puzzle.Score)) +
  geom_point(size = 4, shape = 21, colour = "green") +
  labs(title = "Video score vs puzzle score",
       x = "Video Score", y = "Puzzle Score") +
  theme_classic()

detach(puzzles)
rm(puzzles)

## (PP)

###############################################################################
### Boxplots

## use "Awards_R" data
maths <- read.csv("clipboard", header = TRUE, sep = "\t")
attach(maths)
names(maths)

## basic boxplot
boxplot(MathScore ~ ProgType,
        data = maths,
        ylab = "Average Math Score",
        xlab = "Programme type")

## boxplot using ggplot - lot more you can do with boxplots
# Basic box plot
a <- ggplot(maths, aes(x = ProgType, y = MathScore)) +
  geom_boxplot()
## I am assigning the boxplot to "a" here,
## so that I can add to it without having to write it all out again
# Rotate the box plot
a + coord_flip()

# Change outlier, color, shape and size
ggplot(maths, aes(x = ProgType, y = MathScore)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8,
               outlier.size = 4)

# Change box plot line colors by groups
p<-ggplot(maths, aes(x = ProgType, y = MathScore, color = ProgType)) +
  geom_boxplot()
p
# Use custom color palettes
p+scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
# Use brewer color palettes
p+scale_color_brewer(palette = "Dark2")
# change box plot fill colour
# Use single color
ggplot(maths, aes(x = ProgType, y = MathScore)) +
  geom_boxplot(fill = "#A4A4A4", color = "black") +
  theme_classic()
# Change box plot colors by groups
p <- ggplot(maths, aes(x = ProgType, y = MathScore, fill = ProgType)) +
  geom_boxplot()
p
# Use custom color palettes
p + scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
# use brewer color palettes
p + scale_fill_brewer(palette = "Dark2")
##remove backgroud
p <- p + theme_classic()
p
##adding titles 
p<-p + labs(title = "Math scores of different programmes",
            x = "Programme Type",
            y = "Math Score")
p

rm(a)
rm(p)
detach(maths)
rm(maths)
##(PP)

###############################################################################
### For you to try: Boxplot 

## Using "bulb type and price" excel 
## loading data
bulbs <- read.csv("clipboard", header=TRUE, sep = "\t")
attach(bulbs)
names(bulbs)

##basic boxplot
boxplot(Price ~ Type,
        data = bulbs,
        ylab = "Average Bulb Price (£)",
        xlab = "Bulb type")

##ggplot boxplot
a <- ggplot(bulbs, aes(x = Type, y = Price, fill = Type)) + geom_boxplot()
a <- a + labs(title = "Price of different bulb type", x = "Type", y = "Price")
a <- a + theme_classic()
a

rm(a)
detach(bulbs)
rm(bulbs)

## (PP)

###############################################################################
## Plotting a simple linear regression model

## Using "bulb type and price" excel
## simple to plotting a correlation, but with some added steps
## loading data
income.data <- read.csv("clipboard", header = TRUE, sep = "\t")
attach(income.data)
names(income.data)

## 1. Plot the data points on a graph
income.graph <- ggplot(income.data, aes(x = income, y = happiness)) + 
  geom_point()
income.graph
## this code says using the income.data, plot a graph with the stated
## x and y variable geom_point is used in ggplot to create scatter plots,
## you can add to it to change colour, shape or if you had
## different groups they can be represented in different colours/shapes
income.graph.2 <- ggplot(income.data, aes(x = income, y = happiness)) +
  geom_point(colour = "red")
income.graph.2
## 2. Add the linear regression line to the plotted data and 3. equation for
## regression line using geom_smooth and lm as method to create the line,
## this will add the line of linear regression as well as the standard
## error of the estimate (in this case +/- 0.01) as a light grey stripe

income.graph <- income.graph + geom_smooth(method = "lm", col = "black")
income.graph
## 4. Make the graph ready for publication
## can add style parameters using theme_bw()
## and making custom labels using labs()
income.graph +
  theme_classic() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x£10,000)",
       y = "Happiness score (0 to 10)")

rm(income.graph.2)
rm(income.graph)
detach(income.data)
rm(income.data)

## (PP)

###############################################################################
### Plotting your GLM results

## same as you did your boxplot!

## using the built in wooldata
wooldata <- warpbreaks
attach(wooldata)
names(wooldata)

a <- ggplot(wooldata, aes(x = tension, y = breaks)) +
  geom_boxplot(aes(fill = factor(wool)), alpha = .2)
a <- a + labs(x = "Wool tension", y = "Number of breaks in yarn")
a <- a + theme_classic()
a

rm(a)
detach(wooldata)
rm(wooldata)

(PP)

###############################################################################
### END!