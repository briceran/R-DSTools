# helpful functions for working with dates
library(lubridate)
ymd(19931123)
mdy(11231993)
mytimepoint <- ymd_hm("1993-11-23 11:23", tz = "Europe/Prague")
minute(mytimepoint)
OlsonNames() #all time zones

wday(mytimepoint) #getting day number
wday(mytimepoint, label=T, abbr=F) # label to display the name of the day, no abbrev

mytimepoint #note difference
with_tz(mytimepoint, tz="Europe/London")

time1 = ymd_hm("1993-04-23 11:23", tz = "America/Los_Angeles")
time2 = ymd_hm("1995-05-23 15:23", tz = "US/Pacific" )

myinterval = interval(time1, time2)
myinterval
class(myinterval)
## Example clicks to website
