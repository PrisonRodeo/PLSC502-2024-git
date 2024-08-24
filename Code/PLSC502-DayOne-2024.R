# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# INTRODUCTION                                 ####
#
# This is some code from PLSC 502, "Statistical 
# Methods for Political Research," designed to 
# provide a (very) basic introduction to the R 
# language.
#
# NOTE: Anything you see to the right of a "#"
# (hashtag) is a comment; it's here to explain
# the code. R won't read it. If you're viewing
# this code in RStudio, the comments usually
# appear as green text.
#
# File initially created June 11, 2021
#
# File last updated August 24, 2024
#
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# OBJECTS                                      ####
#
# R is an "object oriented" language, and the
# most basic thing one does in R is an "assignment."
# An assignment is essentially telling R that 
# a bit (or lot) of information is attached to
# a particular thing in the "environment." 
# So, to assign the number "6" to an object
# named "A" we do:

A <- 6

# (Note: When you see something that does *not*
# have a hashtag next to it, that is code that 
# R will "see," interpret, and run. If you're 
# reading this in RStudio/Posit, that code will
# typically be black, or blue.)
#
# In RStudio, when you run that line of code, 
# an object called "A" will appear in the 
# "Environment" window over to the right -->
#
# Now, whenever you refer to "A",
# you're referring to the number 6.

A  

# This returns "6" in the window below.
# The square braces and [1] indicate
# that this is a single value / scalar
# (one row, one column)
#
# As I noted above, everything to the 
# right of a # is a comment; it appears here,
# in the code, but R ignores it. Putting lots
# of comments in your code is like leaving
# little gifts for your future self, who
# will go back to old code and have a 
# much better idea of what you were thinking
# when you wrote it.

A + 5  # returns "11" in the window below.

# ^ Note that you can put comments "in-line,"
# *after* some code.

A^4    # returns "1296" (6x6x6x6).

"A" 

# This prints "A". Using quotes tells R 
# "take me literally; actually return
# whatever is in the quotes."
# 
# If we want to get rid of A, we can remove 
# it:

rm(A)

# Objects can have multiple elements.
# Here is the Penn State women's volleyball
# team's overall winning proportions, 
# from 2005-2022:

PSUWVB<-c(31/34,32/35,34/36,38/38,38/38,
          32/37,25/32,33/36,34/36,36/39,
          28/34,24/34,33/35,25/32,27/33,
          10/16,21/32,26/34)

# The "c()" says "combine these values into
# a vector or list." Note the new object 
# called "PSUWVB" in the "Environment" window.
#
# We can list this object:

PSUWVB

# We can do transformations on it:

PSUWVB * 100

# Note that we now have numbers in the 
# square braces that indicate the position
# of each element (number) in the object;
# the first one is [1], the second is [2],
# etc. We can assign those transformed 
# values to another, different object:

WinPct <- PSUWVB * 100

# We can also combine objects; usually, we
# do this by creating a "data frame."
# Think of a data frame as like an Excel
# spreadsheet. 
#
# So, we might create another object that lists
# the years from 2005 to 2022, in order:

Year <- c(2005,2006,2007,2008,2009,
          2010,2011,2012,2013,2014,
          2015,2016,2017,2018,2019,
          2020,2021,2022)

Year

# Note that a faster way to do this is to 
# use the "seq()" (for "sequence") command:

Year <- seq(2005,2022)

# We just "overwrote" the old "Year" object
# with the new one. This happens without
# warning, so be careful.
#
# Now we can combine these two objects into
# a single data frame, using the
# "data.frame" command:

VB <- data.frame(Year = Year,
                 WinProp = PSUWVB,
                 WinPct = WinPct)

# Note that there's now a new data frame
# called "VB" in RStudio's Environment 
# window; it has 18 observations (rows) 
# and three variables (columns).
#
# NOTE: If you want to "see" a data frame,
# you can simply click on it in the
# Environment window; this has the same
# effect as running "View()""

View(VB)

# Also important is the fact that there are
# different **types** of objects. Some of the
# most commonly-used ones are:
#
# - Numeric objects (numeric scalars and vectors):

foo<-c(1,2,3,4,5) # creates it
foo               # prints it
is.numeric(foo)   # verifies what it is

# - Character objects (letters and things, including
#                      most text):

bar<-"We apply law to facts. We don't apply feelings to facts."
is.character(bar)

# - Matrices (blocks of numbers):

baz<-matrix(c(1,2,3,4,5,6),nrow=3) # creates it
baz                                # prints it
is.matrix(baz)                     # verifies what it is

# - Data frames (we talked about these back on
#                line 134 or so.)
#
# - Lists (collections of other objects; lists
#          can be complicated, so don't sweat them
#          for now.)
#
# If you ever want to see what an object "looks like"
# (that is, check out its various parts) you can use
# the "structure" [str()] command:

str(VB)

# This says that the object "VB"
# - ...is a data frame
# - ...has three columns (variables) and 18 rows (observations),
# - ...one of which is integers, and the other two of which
#      are numerics, and
# - ...with names "Year," "WinProp," and "WinPct"
#
# str() works on *any* kind of object, and is quite useful
# when you're just getting started.
#
# There's lots more to say about objects, but 
# that's enough for now. 
#
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# READING IN DATA                            ####
#
# Most of the time, you won't want to enter
# data by hand. Instead, you will read data
# from something like a spreadsheet into
# Rstudio from some other source file. There
# are many ways to do this.
#
# RStudio can read data in many different
# formats, but the simplest is ".csv" 
# ("comma-separated values"), which is
# simply an Excel-type spreadsheet that 
# has been converted into plain text, with
# "cells" of the spreadsheet separated 
# from each other by a comma.
#
# One way we can read a file is to have it
# "locally" on our own machine. The line at
# the top of the "Console" window below 
# tells you where "local" is; at the moment,
# it's just "~/", the "root" of the drive.
# We can change this by clicking the "Session"
# menu option above, or by using "setwd". So, 
# for example:

setwd("~/Dropbox (Personal)/PLSC 502")

# ...sets the working directory to the local
# folder with that name. Reading the data is
# then easy:

SCOTUS <- read.csv("Data/SCOTUS-votes.csv")

# (Note that this command does not work unless
# you have the data stored "locally" on your
# computer, in a subfolder called "Data".) 
#
# These data are on U.S. Supreme Court 
# justices who have served since 1946.
# The first column is the justice's name,
# the second is the percentage of cases
# involving civil rights and liberties in
# which s/he voted in a pro-civil rights
# direction, and the third column records 
# whether (=1) or not (=0) the justice
# was appointed by a Republican president.
#
# We can also read the same data directly
# from the web, using a URL for the file.
# Because the file is a comma-delimited
# ASCII file (.csv) we can do this using
# the "read.csv" command:

SCOTUS <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/main/Data/SCOTUS-votes.csv")

# (We'll talk about where that URL came from
# in class.) From there, we can do things like 
# "look at" the data:

View(SCOTUS)

# ...and summarize it:

summary(SCOTUS)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SINGLING OUT ELEMENTS                ###############
#
# A data frame is rows and columns. We can
# extract rows, columns, or cells by specifying
# which one we need using the square-brace
# ("[]") notation. Watch what happens when we
# print SCOTUS:

SCOTUS

# The notation "FOO[R,C]" denotes the Rth
# row and the Cth column of the object called
# "FOO." So if we wanted to get Samuel Alito's
# row from the SCOTUS data, we could enter:

SCOTUS[28,]

# The fact that we left the second value
# blank means "give me all the columns
# from that row." Similarly, if we just 
# wanted the column with the justices' 
# voting percentages, we would enter:

SCOTUS[,2]

# which means "give me the values of 
# all the rows from column 2 of SCOTUS."
# A single value would then just be:

SCOTUS[28,3]

# which tells us that Alito was appointed 
# by a Republican. This is useful for a
# lot of things; for example, we can use
# it along with conditional statements to
# subset the data, like this:

GOPJustices <- SCOTUS[SCOTUS$GOP==1,]

# That means "Create an object called 
# 'GOPJustices' that consists of all of
# the columns of SCOTUS, but that only
# includes the rows that represent GOP
# appointees." 
#
# Short digression: The "$" is how we 
# denote which column in the data frame
# we want to single out, and the part
# that says "SCOTUS$GOP==1" is a
# "logical statement." Logicals are
# hugely important; watch what happens
# when we just feed R a logical statement:

SCOTUS$GOP==1

# That's essentially whether (=TRUE)
# or not (=FALSE) each justice in the
# data was appointed by a GOP president.
# We can do the same for Democratic
# appointees:

SCOTUS$GOP==0

# The double-equals (==) is the logical
# operator "is equal to" / "is the same 
# as"; we can use "!=" to say "is not
# equal to" / "is not the same as," like
# so:

SCOTUS$GOP!=1

# (Because GOP either equals 0 or 1, that
# the two immediately previous commands give
# the same result.)
#
# We can see that the command we did above
# worked:

View(GOPJustices)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# PLOTTING / GRAPHS               ####################
#
# R / RStudio is fantastic for visualizing and
# plotting / graphing data. This is a fast
# introduction to the most basic graphing
# command, called "plot."
#
# A (very) basic plot of the volleyball team's
# winning percentage by year looks like this:

plot(VB$Year,VB$WinPct,t="l")

# The plot appears at the lower right.
# Recall that the dollar-sign "XXX$YYY" 
# notation means "use the element YYY
# from object XXX." We can get around this
# in a few ways, the best probably being
# to use the "with()" command:

with(VB, plot(Year,WinPct,t="l"))

# This says "Use the object called 'VB'
# to draw a plot of the objects 'WinPct'
# and 'Year'." We can add a bunch of things
# to make it nicer looking:

with(VB, plot(Year,WinPct,
      t="l",         # make a line plot
      lwd=2,         # line width is 2
      col="navy",    # line color is navy blue
      main="PSU Women's Volleyball Winning\nPercentages, 2005-2022", # title
      xlab="Season", # label X axis
      ylab="Winning Percentage")) # label Y axis

# We can do a similar thing with the 
# Supreme Court data:

with(SCOTUS, plot(LiberalPct~GOP))

# This is a (very bad) "scatterplot"; the values
# of LiberalPct are vertical, and the values
# of GOP (either 1 for Republican appointees
# or 0 for Democratic ones) are horizontal.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# LOOPS AND BASIC PROGRAMMING          ###############
# 
# We'll do some work with simulations
# in this course. That means repetition,
# and that means loops. The basic structure
# of a loop in R is:
#
#  for (i in 1:N) {
#     <<activity>>
#  }
#
# This means "Take the activity, and repeat 
# it N times." Note that you don't need to
# use "i" specifically as the index.
#
# A simple example: we'll generate 100 
# "coin flips": draws of either heads (=1)
# or tails (=0) from a binary (bernoulli)
# distribution with P = 0.5:

set.seed(7222009)        # set a random number seed
N <- 100                 # how many flips?
flips <- rbinom(N,1,0.5) # simulate

# (type "?rbinom" for details on this). We
# can summarize this by (e.g.) the number of
# "heads" out of 100:

sum(flips)             # tally up heads

# Now, suppose we wanted to repeat our
# flip-100-coins simulation many (say, 1000)
# times, and each time we want to record 
# the number of heads we get.
#
# First, we need to create a "container" to
# put the head-counts in, one with 1000 bins:

counts <- numeric(1000)   # count storage

# Next, we write a loop that repeats the 
# 100-flip simulation 1000 times, and each 
# time stores the count of "heads" as an
# element of "counts":

NSims <- 1000   # N of simulations
for(i in 1:NSims) {
  flips <- rbinom(N,1,0.5) # simulate
  counts[i] <- sum(flips)  # store count
}

# Now we can take a look at "counts":

summary(counts)        # summary statistics
hist(counts)           # histogram
plot(density(counts))  # kernel density plot

# Note that one can put loops inside of
# other loops. So, suppose I wanted to
# do the same simulation, but vary the
# number of coins I flipped (say, from
# 10 to 20 to 50 to 100 to 1000). I
# can do that all with a single set
# of nested loops:

NFlips <- c(10,20,50,100,1000)       # N of flips
Sums <- data.frame(matrix(ncol=length(NFlips),
                          nrow=NSims))

for(j in 1:length(NFlips)) {         # open outer loop
  for(i in 1:NSims) {                # open inner loop  
    flips <- rbinom(NFlips[j],1,0.5) # simulate
    Sums[i,j] <- sum(flips)          # store count
  }                                  # close inner loop
}                                    # close outer loop

# We can then summarize the results:

summary(Sums)

# ...and we could plot the results, etc.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# FIVE RANDOM TIPS                                ####
#
# 1. <-- The little down-carat next to the line   ####
#                 number is there because of these ^.
# Putting four hashtags in a commented line creates
# a section break in the code; you can click on it
# to hide or reveal everything below it until the next
# section break. (Click on it to see what happens.)
# This is useful when you're working with long blocks
# of code.
#
# 2. If you want to get rid of *everything* you've 
# done and start with a "clean" / empty environment,
# you can of course just restart R / RStudio.
# Alternatively:

rm(list=ls())

# ...removes *everything* in the current environment
# and starts over (though it retains any loaded 
# packages). Use with care.
#
# 3. R is *always* case-sensitive, so "gop," "Gop,"
# and "GOP" can and will be different objects unless
# you assign them to the same thing.
# 
# 4. We'll talk more about RStudio's "anatomy" in class,
# but the "Tutorial" tab in the window to the upper-
# right is one to explore. It will prompt you to install
# a package called "learnr" which, as the name suggests,
# is an aid for learning how to use R. I recommend it.

# 5. Finally: Many large language models (LLMs) -- 
# things like ChatGPT -- are pretty good at writing 
# R code. My own experience circa Fall 2024 is that
# most will get you 80-90% of the way to where you
# want to be, but with caveats:
#
# a) You need to be good at writing prompts, which
#    in turn means you have to know what you're doing
#    in terms of the data and statistics...
#
# b) You need to vet / modify / validate not only 
#    that the code you get "works," but also that it
#    **does what you want it to do**. That usually
#    means running it on some simple "test" data
#    and verifying that it's working properly.
#    Caveat emptor.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# GETTING HELP                                    ####
#
# R has extensive documentation, and a help
# function that is available at the command
# line / console. The basic help syntax uses
# a question mark:

?summary

# Note that this will bring up a help file
# on any command that is part of a currently-
# loaded package. If the package is not loaded,
# you'll get an error:

?read.dta                   # ERROR, lol

install.packages("foreign") # install the "foreign" package
library(foreign)            # load the package
?read.dta                   # see the help file!

# One can also do a omnibus search for help on
# a topic; this can be valuable if you know what
# you want to do, but don't know the package
# to do it with. That uses two question marks:

??spss

# The "??" command will work with almost anything:

??sunspots
??tuna
??Belgium

# (This is the end of the code.)