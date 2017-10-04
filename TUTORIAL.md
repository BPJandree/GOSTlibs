This tutorial was written for the Geospatial Operations Support Team (GOST) of the World Bank, Washington D.C. The example can be followed by anybody that is interested in building R packages.


# BUILDING AN R PACKAGE

## Requirements

First you will need to download and install [R studio](https://www.rstudio.com/products/rstudio/download/#download)
Also, download and install LaTex. For example, download and install [TexStudio](http://www.texstudio.org/)
This is a LaTex editor. Upon running it first time, it will suggest several LaTex distributions with download links.
Then, download and install [Rtools](https://cran.r-project.org/bin/windows/Rtools/). Rtools manages compilers that are needed when you use C-type code, or when you want to compile your manuals into a PDF.


## Setting up R studio

Open R studio and install and load devtools and roxygen2

```
install.packages("devtools")
library("devtools")
install.packages("roxygen2")
library("roxygen2")
```

After that, click on Build -> Configure Build tools
activate:
Generate Documentation with Roxygen, activate all sub options
click OK

This tells R studio to create manual files when building a package, which is what we want to do later on.


## Setting up your Package

In R studio, click File -> New Project -> New Directory -> R Package

Give your package a name, we call it GOSTlibs. Select a folder where you want to store the package project and Click create project. 

R studio will generate several files and subdirectories. Important for us is the folder called R, this is where you can store your R functions. Rstudio has also created a folder called man, once we start working on the actual R code and it documentation, Rstudio will write .Rd files into this directory that contain the documentation which you can access from your terminal. Each function in R has a description, for example try:
```
?library
```

By default there will be a Hello World example in both the R and man folder. We're going to delete that and replace it with some actual code that Ben wrote.

Go ahead and create a file called jitterSurveyPoints.R in the R folder in your package directory and copy paste the following code in it:

```
jitterSurveyPoints <- function(inPts, inAdmin, urbanField="Id",
        urbanDist=2000, ruralDist=5000, ruralDistFar=10000){
    # Jitter an xy location by the maxDist
    jitterPoint <- function(x, y, maxDist) {
        #Determine a direction between 0-360
        angle = runif(1,0,360) * (pi/180)
        distance = runif(1, 0, maxDist)

        dX = tan(angle) * distance
        dY = sin(angle) * distance

        return(c(x + dX, y + dY))
    }
    ### TODO ###
    # 1. There shold be some parameter checking here
    #   a. the projection for the dataset has to be measured in metres
    #   b. the script creates two coluns newX and newY, which should be checked first

    # loop through all the points in the input dataset
    inPts$newX = 0
    inPts$newY = 0
    inPts$displace = urbanDist
    ruralIdx = which(inPts@data[,urbanField] == 1)
    inPts[ruralIdx,"displace"] = ruralDist
    ruralFarIdx = sample(ruralIdx,ceiling(0.01*length(ruralIdx)),replace=FALSE)
    inPts[ruralFarIdx,"displace"] = ruralDistFar
    for (idx in 1:nrow(inPts)) {
        curPt = inPts[idx,]
        #Choose the distance based on the ubran definition
        curDist = curPt@data$displace
        curCoords = coordinates(curPt)
        curAdmin = inAdmin[which(gIntersects(curPt, inAdmin, byid=TRUE)),]
        jitteredIn = TRUE
        while (jitteredIn) {
            jitterPt = curPt@data
            newCoords = jitterPoint(curCoords[1], curCoords[2], curDist)
            jitterPt$newX = newCoords[1]
            jitterPt$newY = newCoords[2]
            coordinates(jitterPt) = c("newX", "newY")

            jitteredIn = !(gIntersects(jitterPt, curAdmin))
        }
        inPts[idx,"newX"] = jitterPt$newX
        inPts[idx,"newY"]= jitterPt$newY
    }

    return(inPts)
}
#' This is a function to jitter GPS survey points.
#'
#' Following DHS guidelines, survey GPS locations need to be purposefully displaced (jittered) before disseminating in order to preserve annonymity. Each point is displaced based on its urban/rural definition. All points must stay within the administrative 2 boundaries in which they originate.
#' @param inPts SpatialPointsDataFrame containing the points to be jittered.
#' @param inAdmin SpatialPolygonsDataFrame containing administrative boundaries. Jittered points are not allowed to be moved outside their original administrative boundary.
#' @param urbanField (optional) string indicating the column that contains a binary indicator that defines urban and rural points. 1 = Rural.
#' @param urbanDist (optional) numeric distance (in metres) to jitter urban points. Default is 2000m.
#' @param ruralDist (optional) numeric distance (in metres) to jitter rural points. Default is 5000m.
#' @param ruralDistFar (optional) numeric distance (in metres) to jitter 1\% of rural points. Default is 10000m.
#' @keywords jittering
#' @return A spatial data frame with randomly jittered coordinates
#' @export
#' @examples
#' # First load the input points. You can use the example data:
#'
#' data("inPts")
#'
#' # As you can see this is a shapefile with the following structure:
#'
#' str(inPts)
#'
#' # You will also need administrative boundaries
#' to ensure that the Jittered survey locations remain within administrative units.
#'
#' data("inAdmin")
#'
#' # Finally pass on the objects to the jitterSurveyPoints function.
#'
#' newPts = jitterSurveyPoints(inPts, inAdmin)
```

The first line is a short title the describes your function. It is followed by a general description of the function and its use case. 
Arguments are declared uisng ```@param argumentName argumentDescription```. After you have declared all the arguments, end with ```@export```. It is important that you escape special symbols. For example, write ```1\%```.

Preferably, but optionally, add a working example in R code. As you can see, anything after ```@examples``` is just plain R code including comments, with an additional ```#'``` in front of it.

## Adding data to your package

In our working example, we load two datasets: 
```
data("inPts")
data("inAdmin")
```

In R you can load data that is provided in packages by first opening the package to which the data belongs to, and then running the ```data()``` command. We will have to add our datasets to the package.


To add example data, first download the RawSampleData from the [RawSampleData subfolder on my github]()

Save it somewhere on your computer, and load it to your R environment. Then save the objects as .rda files into the data folder in your R package.

```
# install and load packages to handle spatial data frames
install.packages("rgdal")
library("rgdal")
install.packages("rgeos")
library("rgeos")

# read the shape files 
inAdmin = readOGR("C:/Users/.../RawSampleData", "pakistan_admin2")
inPts = readOGR("C:/Users/.../RawSampleData", "random_points")

# save the in-memory objects to .rda files in the data folder in your package
save(inAdmin, file="C:/Users/.../GOSTlibs/data/inAdminFiles.rda")
save(inPts, file="C:/Users/.../GOSTlibs/data/inPts.rda")

```

Similar to the jitterSurveyPoints function, we will need to add some documentation to the data. 

It is good practice to make a single R file for every function and every dataset in your package. This makes maintaining the package a lot easer, especially when multiple people start collaborating and suggesting edits to your package code. Having a single R file allows them to focus on code  that they work with, and not get distracted by lengthy code blocks that perhaps are only useful to others.

Create one file called inPts.R in the R folder of your package, and paste the following code into it:

```
#' A sample points dataset for testing point jittering
#'
#' To load the sample data, data(inPts)
#'
#' @name inPts
#' @docType data
#' @usage data(inPts)
#' @format a SpatialPointsDataFrame of randomly dropped point locations inside the Pakistan admin2 boundaries
#' @keywords datasets

NULL
```

Create a second file called inAdmin.R in the R folder of your package and paste the following code into it:

```
#' A sample administraive boundaries dataset describing admin2 boundaries in Pakistan
#'
#' To load the sample data, data(inAdmin)
#'
#' @name inAdmin
#' @docType data
#' @usage data(inAdmin)
#' @format a SpatialPolygonsDataFrame of the admin2 Pakistan boundaries
#' @keywords datasets

NULL
```

# Building your R package


Once you have the R code with your function and documentation in place, we can build and load your package into your R environment. Go to the right upper panel in R studio and click Build an Reload under the build tab.

Because we have told Rstudio to use roxygen to build documentation, R studio will create several documentation filed. If you want to rebuild the documentation without rebuilding and reloading your package, you can use:

```
devtools::document()
```

If everything worked, R studio will relaunch an R environment and you will see that it has loaded our package:
```
library(GOSTlibs)
```

To see if it worked, you can write
```
?jitterSurveyPoints
```

In the right bottom panel, R studio will now show the documentation that we have created. Go ahead and compare it to the code that we have written above our function in jitterSurveyPoints.R
Similarly
```
?inPts
```
and
```
?inAdmin
```
will show the documentation that we have added to our datasets.


## Finalizing your R package

R package usually have a general description too. In the package directory you will find a file called DESCRIPTION.
Open it with an editor, and add additional information. The DESCRIPTION file accepts only very specific formatting, and if your format incorrect you will have trouble building your package.
My description file looks like:

```
Package: GOSTlibs
Type: Package
Title: The first GOST package
Version: 0.1.0
Authors: Bo Pieter Johannes Andree <bandree@worldbank.org>, Benjamin P Stewart <bstewart@worldbank.org>
AuthorAffiliation: World Bank
Description: This is an example package that we build during one of the Show Intel meetings at GOST - Worldbank, Washington D.C. The code is functional and allows you to Jitter GPS locations.
Depends: rgdal, rgeos
License: MIT
Encoding: UTF-8
LazyData: true
RoxygenNote: 6.0.1.9000
ByteCompile: TRUE
```

As you can see, I have added a line:
```
Depends: rgdal, rgeos
```
If your package uses code from another library, you can add them as dependencies. This will ensure that whenever someone installs your library, the dependencies are also installed. When someone loads your library, the dependencies are also loaded. The code that we have written does not actually depend on rgdal or rgeos, I have added this line mostly as an example since it is commonly asked how to add dependencies. 

I have also added a line:
```
ByteCompile: TRUE
```
This makes sure that your R code is byte-compiled when you build your package. Compiled R may run faster than Pure R.


Finally you want to have a pdf manual containing all the documentation and meta descriptions. To do this, we need to run a shell command within the package directory.
It is convenient to run this from the R terminal.

First, set your working directory to the folder that contains your package. For example, if your package is located in a folder called Rpackages, with GOSTlibs as the package folder within Rpackages, run:

```
setwd("C:/Users/.../Rpackages/")
```
Then run:
```
system("R CMD Rd2pdf GOSTlibs")
```
You will find your manual in Rpackages, it is useful to make a subfolder within your package named "vignette" and place the PDF in there. If you want to create a new manual, delete the old one.



## Distributing your package

Finally, you may want to share your package with others. If you're confident about your code and it's use, you can submit it to CRAN and go through a review and get your package on the CRAN server. I more lightweight alternative is to upload your package to your github.


