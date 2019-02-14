
# Public Interest Data Lab 2019
# Best Practices for Coding in R

# .................................................................................

# Setting Up

## It's often best to keep all setup code at the top of a file so it's clear what 
## libraries you're using and where all of your set up code is. 

# Setting a Working Directory
## A working directory is the location of the files you're using with this script
## This will become important in importing data. 
## You can use
setwd("../../") # to navigate out from your default folder
## And use
setwd("path/to/file") # to navigate in to subfolders
## Tip: You can also click Session > Set Working Directory > To Source File Location 
## in the menu bar up top if your script is in the same folder as the data. 

# Using Packages
## It's also best practices to load all packaages at the beginning of a script
## You can load a package with the library(). Note: Loading is not the same as installing. 
## A package only needs to be installed once (with install.packages("packagename")),
## but needs to be loaded every time you open a new R Session where you use that package. 
library(tidyverse) # the tidyverse package is used for almost everything. 

# .................................................................................

# BEST PRACTICES 

# Titles
## Add a descriptive title to the top of your script to easily identify 
# the project and the purpose immediately on opening the file. 

# Organization
## With long scripts, it's often useful to separate code into sections using this
###################################################################################
## or this
# .................................................................................
# or any consistent separator after a # comment sign
## Adding a section title after a separator is also super helpful in organizing code

# Object Naming Conventions
## No spaces or dashes in object names. Dots, underscores, and capital letters are fine 
## (but remember that object names are case sensitive).
## Chose names that are descriptive but not too hard to type.
## There are many different conventions for naming variables and objects. It's best to 
## pick one and stick with it.
SnakeCase # this one is somewhat annoying because you have to find the capital letters
camel_case # this is often much easier, we will use this convention for this project. 

# Indenting
## This is bad code:
sim1 <- sapply(1:nrow(cond), FUN=function(i){
print(cond[i,])
if(cond[i,4]==1){
ac <- c(0,0)
} else {
ac <- c(.95,.95)
}
data <- twsimdata(N=cond[i,2],
T=cond[i,3], time.ac=ac[1],
spatial.ac=ac[2])$data
})
colnames(sim1) <- c("iter", "N", "T", "ac",
"caseFE", "timeFE","twowayFE",
"pooled", "re")

## This is good code:
sim1 <- sapply(1:nrow(cond), FUN=function(i){
  print(cond[i,])
  if(cond[i,4]==1){
    ac <- c(0,0)
  } else {
    ac <- c(.95,.95)
  }
  data <- twsimdata(N=cond[i,2],
                    T=cond[i,3], time.ac=ac[1],
                    spatial.ac=ac[2])$data
})
colnames(sim1) <- c("iter", "N", "T", "ac",
                    "caseFE", "timeFE","twowayFE",
                    "pooled", "re")
## Indents make it easier to read and understand code, and understand what arguments
## go with each function you're using. 

# Code Length & Line Breaks
## If I type a command or comment without any line breaks, it gets really annoying to read because the reader has to now scroll to the right to figure out how the command or comment ends, which is so annoying.
## The comments in this script are a good approximate line length.
## Break lines at points where R knows to expect another object on the next line, 
## like after a comma (,), a plus sign (+), or an equals sign (=), for example:
ggplot(data, aes(x=variable_one, y=variable_two)) + 
  geom_point(color="red") + 
  annotate("text", x=1, y=1, label="Example")
## or 
df <- mutate(df, 
             var = fct_recode(var, 
                              "New Name"="This Old Name",
                              "New Name"="That Old Name"))

# Commenting Out Code
## Comments make code much easier to read for other people, and much easier to remember
## what you were thinking when you wrote that line of code. I often comment the question
## I'm asking and the result I get, so I remember what I was trying to do. For example:

# How many cars do we have in the data with each number of gears?
mtcars %>% group_by(gear) %>% count() # 15 with 3 gears, 12 with 4, 5 with 5

# Temporary Data Sets
## You might want to create a temporary data frame to look at something, but not care
## about keeping it as an object. Consistently name your arbitrary data sets if you want to
## you write over them next time you use them to avoid cluttering up your environment. 
## I use arb, others use text, temp, or check as consistent names. 

arb <- filter(df, is.na(var1))


# Saving an Image
## Instead of writing all of the objects in your environment as CSV files, you can save
## an image of your entire environment. 
save.image("imagename.Rdata")

# .................................................................................

# USEFUL TIPS

# Useful Keyboard Shortcuts
## Cmd + Shift + C to comment out multiple lines at a time
## Option + - to use the assignment operator ( <- )
## Ctrl + Shift + M to create pipes (%>%)

# .................................................................................

# I often keep a to do list at the end of my script so I remember where I left off
# TO DO:
## explore missing data, is it random? 
## run logit model of binary outcome

# .................................................................................

## Some code conventions and examples borrowed with gratitude to Professor Jon Kropko. 