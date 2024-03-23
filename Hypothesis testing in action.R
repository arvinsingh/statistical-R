#############################################
#                                           #
# Workshop 2: Hypothesis testing in action  #
#                                           #
#############################################

## QUick note: Lines that start with '#' will not be read by R as code
## Very useful for writing notes around your code

###########################################################
###########################################################
#### Example 1: Video scores and puzzle score

### Open Video scores_puzzle scores_R excel
### Copy all data to clipboard

scores <- read.csv("clipboard", header = TRUE, sep = "\t")
## copies the data in, header=T treats the first row as headers not actual data
attach(scores)   ## ataches the data into R
## To see the coloum headers to check you have imported the correct data
names(scores)

## The shapiro test will test each variable separately
shapiro.test(Video.Score)
shapiro.test(Puzzle.Score)


cor.test(Video.Score, Puzzle.Score, method = "spearman")
## if you were running a pearsons, replace 'spearman' with 'pearson'

detach(scores)


#######################################################################
#######################################################################
#### Example 2: Math Scores in different programmes

## Open Awards_R excel file
## Copy data into clipboard

maths<-read.csv("clipboard", header = TRUE, sep = "\t")
attach(maths)
names(maths)

## Normality test

shapiro.test(MathScore)

## Running a Kruskall Wallis test

kruskal.test(MathScore ~ ProgType, data = maths)

## back to powerpoint


install.packages("FSA")
## some tests will require you to install and load a package to run.
## You only need to install the first time you use the package
## loads the library so you can use it, need to load the library each session.
library(FSA)
dunnTest(MathScore ~ ProgType, data = maths)


##simple plot
boxplot(MathScore ~ ProgType,
        data = maths,
        ylab = "Average Math Score",
        xlab = "Programme type")

detach(maths)

###################################################
###################################################
########## Example 3: GPA in public and private schools

## Open GPA_R excel file
## Copy data into clipboard

GPAScore <- read.csv("clipboard", header = TRUE, sep = "\t")
attach(GPAScore)
names(GPAScore)

## Normality test

shapiro.test(gpa)


## Unpaired t-test

### first need to test for variance between the groups
res.ftest <- var.test(gpa ~ public, data = GPAScore)
res.ftest

##(PP)

t.test(gpa, public, var.equal = TRUE)

## Visualise this 

boxplot(gpa, public,
        data = GPAScore,
        ylab = "Average GPA Score",
        xlab = "School type")


################################################################
##### For you to try #1 ########################################

##### Study Time vs Exam Score

## Hyothesis: Time students spend studying is related to their exam score
## Prediction: Students that spend more
## time studying will recieve higher exam scores

## load data

Exam <- read.csv("clipboard", header = TRUE, sep = "\t")
names(Exam)
attach(Exam)



## normality 
shapiro.test(time)
shapiro.test(score)

## test
cor.test(time, score, method = "spearman")

## significant positive correlation between revision time and test score

################################################################
##### For you to try #2 ########################################

##### Bulb type and prices

## loading the data
bulbs <- read.csv("clipboard", header = TRUE, sep = "\t")
names(bulbs)
attach(bulbs)

## Hypothesis: The prices of the two types of bulbs will be different
## Prediction: Red bulbs will cost more than the orange bulbs

## normality
shapiro.test(Price)
## not normal - remember only need to test dependent variable price

## test
m1 <- wilcox.test(Price ~ Type,
                   data = bulbs,
                   exact = FALSE)
m1

## interpretation
## wilcox test is no significant,
## therefore there is no significant
## differences in the two bulb type prices

################################################################
##### For you to try #3 ########################################

## Dog breeds and agility

## Open Dog agility_R excel file
## Copy data into clipboard

## Hypothesis: Agibility scores will be different
## in the three different dog breeds

## Prediction: German shepards and Springer
## Spaniels will score higher than French
## Bulldogs in the agility test

dogs <- read.csv("clipboard", header = TRUE, sep = "\t")
attach(dogs)
names(dogs)

## Normality test

shapiro.test(AgilityScore)
## only need to test dependent variable for normality

## Running a Kruskall Wallis test

kruskal.test(AgilityScore ~ DogBreed, data = dogs)
## significant differences in dog breed agility scores,
## need to run future analysis to find out what breeds
## the significant differences lie

library(FSA)
dunnTest(AgilityScore ~ DogBreed, data = dogs)
## differences between each dog breed

detach(dogs)
