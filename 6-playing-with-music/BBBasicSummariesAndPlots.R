## First set the working directory (Session menu) to where this file is located
## Then import data

bb <- read.csv('all_chords.csv')


## View a summary of the data set and the number of chords in the data set

summary(bb)
length(bb$song)


## There are some problems with this import. See "song" on the summary. "sus7" is a chord quality, not a song title. 
## What's going on?
## Let's look at the entries where "sus7" is the song title.

bb[bb$song=='sus7',]


## Now look at just the first 20 of them.

bb[bb$song=='sus7',][1:20,]


## Not super-helpful. But there are a bunch really close together at 11255 and following. 
## Let's look at that section.

bb[11250:11270,]


## This looks like a comma was used inside one of the fields, but the field was not quoted in the CSV.
## sus4(b7,9) - a chord containing a "suspended fourth," a flat-seventh, and a ninth.
## So R is parsing the chord symbol as having an extra field, and it's throwing off the database.
## I cleaned this up in my text editor (search-and-replace). Now import the new data file, and start exploring again.

bb <- read.csv('all_chords_fixed.csv')
summary(bb)
length(bb$song)


## View a table of each parameter

table(bb$song) ## number of chords per song
table(bb$onset) ## beginning of chord
table(bb$beat.of.bar) ## beat.of.bar of chord
table(as.numeric(bb$bar.of.phrase)) ## what bar of the phrase does each chord begin on - not helpful
table(bb$tonic.name) ## number of chords assigned to each tonic
table(bb$tonic.pc) ## 
table(bb$root.name) ## number of chords with each root
table(bb$root.pc) ## 
table(bb$bass.name) ## number of chords with each bass note
table(bb$bass.pc) ## 
table(bb$quality) ## number of chords with each quality
table(bb$simple.quality) ## 'simple' quality (what's the difference?)


## use 'plot' to visualize some basic data

plot(bb$song) ## visualize the number of chords per song
plot(bb$beat.of.bar) ## what beat does each chord begin on - not helpful
hist(bb$beat.of.bar)  ## a histogram is more helpful
plot(as.numeric(bb$bar.of.phrase)) ## what bar of the phrase does each chord begin on - not helpful
plot(bb$tonic.name) ## number of chords assigned to each tonic
plot(bb$tonic.pc) ## 
plot(bb$root.name) ## number of chords with each root
plot(bb$root.pc) ## 
plot(bb$bass.name) ## number of chords with each bass note
plot(bb$bass.pc) ## 
plot(bb$quality) ## number of chords with each quality
plot(bb$simple.quality) ## 'simple' quality (what's the difference?)


## note that since the McGill Billboard dataset includes root/tonic/bass *names*, we don't need to plot the table to get a meaningful graph. Why?


## The McGill Billboard dataset does *not* include 'relative roots' - the relationship of chord roots to tonic.
## We can construct that by subtracting the tonic from each root (using pitch-class numbers).
## Since each column of the data frame is the same length, we can just subtract one column from the other.
## The result is a vector the same length as those columns.
## Since we want a number between 0 and 11 to designate the relative root, we use the '%%' operator for modular arithmetic.

root.pc.in.key <- (bb$root.pc - bb$tonic.pc) %% 12
summary(root.pc.in.key)


## Then we use cbind() to attach our new vector to the original data frame as an additional column.

bb <- cbind(bb,root.pc.in.key)
summary(bb)


## Now we can explore this new variable.

plot(bb$root.pc.in.key)


## But like the deClercq/Temperley corpus, this is a series of numeric values, so we need to plot the table.

plot(table(bb$root.pc.in.key))


## How does it compare to deClercq/Temperley?