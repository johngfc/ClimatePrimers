#from calypso
pwd -print working directory
cd /data/nccsc # gets onto the n drive
ls # to get paths from here
print("hello")
install.packages("ncdf4",repos = "http://cran.r-project.org")
#install.packages("climates",repos = "http://cran.r-project.org") #not on CRAN and I forget where to get it

install.packages("rgdal",repos = "http://cran.r-project.org")
install.packages("stats",repos = "http://cran.r-project.org")
install.packages("chron",repos = "http://cran.r-project.org")
install.packages("zoo",repos = "http://cran.r-project.org")

library("stats")
library("chron")
library("zoo")