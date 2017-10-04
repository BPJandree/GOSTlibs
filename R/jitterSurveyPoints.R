
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
    # 1. There shold be some parameter checking here, but I ain't doin' that shit yet
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


