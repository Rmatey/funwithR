#Code to solve the following problem from MindYourDecisions' puzzle: https://www.youtube.com/watch?v=i4VqXRRXi68&t=44s

#initialize
  SampleSize<-100000
  DATA <- structure(list(
                      y1=double(),
                      y2=double(),
                      x1=double(),
                      x2=double(),
                      ED=double()),
          class = "data.frame")
  
  KevinsEuclideanDistanceunction <- function(p_x,p_y,q_x,q_y){
    sqrt((q_x - p_x)^2 + (q_y - p_y)^2)
  }

#create random points
  DATA.x1<-c(runif(SampleSize,0,1))
  DATA.y1<-c(runif(SampleSize,0,1))
  DATA.x2<-c(runif(SampleSize,0,1))
  DATA.y2<-c(runif(SampleSize,0,1))

#calculate eucledian distance of the points
  DATA.ED<- KevinsEuclideanDistanceunction(DATA.x1,DATA.y1,DATA.x2,DATA.y2)

#average the distances
  average_ED <- mean(DATA.ED)

#plot the ordered distribution
#output the average and all or some of the data
  barplot(table(round(DATA.ED,2)))
  print(average_ED)