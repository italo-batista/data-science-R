## First set the working directory (Session menu) to where this file is located
## Then import data

dt <- read.csv('resultsBySong.csv')


## View a summary of the data set and the number of chords in the data set

summary(dt)
length(dt$song)


## View a table of each parameter

table(dt$song) ## number of chords per song
table(dt$start) ## beginning of chord
table(dt$end) ## end of chord
table(dt$RN) ## Roman numeral and inversion of chord relative to tonic
table(dt$relChr) ## number of chromatic steps from tonic to chord root (modulo12)
table(dt$relDia) ## diatonic scale degree (1-7) of chord root
table(dt$tonic) ## tonic scale degree (C = 0)
table(dt$abs) ## chord root scale degree (C = 0)


## You can enclose each of those in 'sort()' to rank the data

sort(table(dt$song))
sort(table(dt$RN))
sort(table(dt$relChr))
sort(table(dt$relDia))
sort(table(dt$tonic))
sort(table(dt$abs))


## use 'plot' to visualize some basic data

plot(dt$song) ## visualize the number of chords per song
plot(dt$end) ## shows (indirectly) the lengths of the songs
plot(dt$RN) ## frequency of occurrence of specific Roman numeral/quality/inversion
plot(dt$relChr) ## not helpful
plot(dt$relDia) ## also not helpful
plot(dt$tonic) ## still not helpful
plot(dt$abs) ## ditto

## but...

plot(table(dt$relChr)) ## frequency of occurrence of each chord root (relative to tonic)
plot(table(dt$relDia)) ## frequency of occurrence of each chord root (scale-degrees 1-7)
plot(table(dt$tonic)) ## number of chords assigned to each chromatic pitch-class (C = 0)
plot(table(dt$abs)) ## frequency of occurrence of each chromatic chord root (C = 0)
