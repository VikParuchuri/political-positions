setwd("~/vikparuchuri/political-positions")

is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
      install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    options(java.parameters = "-Xmx8g")
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}

sorted_vec <- function(dat){
  dat$vote[dat$vote=="Yea"] = 1
  dat$vote[dat$vote=="Nay"] = 0
  dat$vote[dat$vote=="Not Voting"] = 2
  unique_senators = unique(dat$sen)
  unique_cong = sort(unique(dat$congress))
  cols = list()
  for(c in unique_cong){
    subc = dat[dat$congress==c,]
    unique_session = sort(unique(subc$session))
    for(s in unique_session){
      subs = subc[subc$session==s,]
      unique_number = sort(unique(subs$number))
      for(n in unique_number){
        subn = subs[subs$number==n,]
        votes = lapply(unique_senators,function(x){
          ret <- 3
          if(x%in% subn$sen){
            ret = subn[subn$sen==x,'vote']
          }
          ret
        })
        ret = data.frame(as.numeric(votes),stringsAsFactors=FALSE)
        colnames(ret) = paste("(",c,"|",s,"|",n,")",sep="")
        cols[[length(cols)+1]] = ret
      }
    }
  }
  ret = data.frame(do.call(cbind,cols),stringsAsFactors=FALSE)
  rownames(ret) = unique_senators
  ret
}

load_or_install(c("RJSONIO","ggplot2","stringr","foreach","wordcloud","lsa","MASS","openNLP","tm","fastmatch","reshape","openNLPmodels.en",'e1071','gridExtra'))

senate = fromJSON("data/senate.json")

frames = lapply(senate,function(x){
 data.frame(sen=gsub(", ","",as.character(names(x$data))),vote=as.character(x$data),congress=as.numeric(x$congress),number=as.numeric(x$number),session=as.numeric(x$session),stringsAsFactors=FALSE)
})
frame = do.call(rbind,frames)

frame2013 = sorted_vec(frame[frame$congress==113,])

frame2013$name = gsub(" ","",as.character(lapply(strsplit(rownames(frame2013),"\\("),function(x) x[1])))
frame2013$party = as.character(lapply(strsplit(rownames(frame2013),"\\("),function(x) strsplit(x[2],"-")[[1]][1]))
frame2013$state = gsub("\\)","",as.character(lapply(strsplit(rownames(frame2013),"\\("),function(x) strsplit(x[2],"-")[[1]][2])))

non_predictors = c("name","party","state")

features = frame2013

feature_names = names(features)[!names(features) %in% c(non_predictors)]

for(f in feature_names){
  features[,f] = as.numeric(features[,f])
}

scaled_data = scale(features[,feature_names])
scaled_data = apply(scaled_data,2,function(x) {
  x[is.na(x)] = -1
  x
})
svd_train<-svd(scaled_data,2)$u

newtrain<-data.frame(x=svd_train[,1],y=svd_train[,2],label_code=as.numeric(as.factor(features$party)),label=features$party,state=features$state,name=features$name,full_name=rownames(features),stringsAsFactors=FALSE)
distances = rowMeans(as.matrix(dist(newtrain[,c("x","y")],method="euclidean")))
newtrain = data.frame(newtrain,distances=distances,stringsAsFactors=FALSE)
interesting_senators = c("Chiesa (R-NJ)","Markey (D-MA)","Kerry (D-MA)","Cowan (D-MA)", "Lautenberg (D-NJ)","McCain (R-AZ)", "Rubio (R-FL)","Cruz (R-TX)","Scott (R-SC)","Roberts (R-KS)", "Inhofe (R-OK)","Barrasso (R-WY)", "Johnson (R-WI)", "Reid (D-NV)", "Durbin (D-IL)", "Schumer (D-NY)", "McConnell (R-KY)", "Cornyn (R-TX)", "Thune (R-SD)", "Murkowski (R-AK)", "Collins (R-ME)", "Manchin (D-WV)", "Pryor (D-AR)", "King (I-ME)", "Sanders (I-VT)")

#model = svm(score ~ x + y, data = newtrain)
#plot(model,newtrain)

collapse_frame = do.call(rbind,by(features[,feature_names],features$label_code,function(x) apply(x,2,mean)))
line_count = tapply(tf$result_label,tf$result_label,length)
scaled_data = scale(collapse_frame)
scaled_data = apply(scaled_data,2,function(x) {
  x[is.na(x)] = -1
  x
})


svd_train<-data.frame(svd(scaled_data,2)$u,line_count=line_count,label=rownames(line_count))
svd_train <- svd_train[svd_train$X1<mean(svd_train$X1)+1.4*sd(svd_train$X1) & svd_train$X1>mean(svd_train$X1)-1.4*sd(svd_train$X1),]
svd_train <- svd_train[svd_train$X2<mean(svd_train$X2)+1.4*sd(svd_train$X2) & svd_train$X2>mean(svd_train$X2)-1.4*sd(svd_train$X2),]

p <- ggplot(newtrain, aes(x, y))
p = p + geom_point(aes(colour =label_code-1, size=10)) + scale_colour_gradient(low = "darkblue", high="red") + scale_size_area(max_size=7) + geom_text(data = newtrain[newtrain$full_name %in% interesting_senators,], aes(x+.2,y, label = full_name), hjust = 2)
p = p +   theme(axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks=element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank()) 
p = p +labs(colour="Type of Music")
p


unique_congress = sort(unique(frame$congress))
polarization = list()
for(c in unique_congress){
  sframe = frame[frame$congress==c,]
  if(sum(is.na(sframe))==0){
    
    frame2013 = sorted_vec(sframe)
    
    frame2013$name = gsub(" ","",as.character(lapply(strsplit(rownames(frame2013),"\\("),function(x) x[1])))
    frame2013$party = as.character(lapply(strsplit(rownames(frame2013),"\\("),function(x) strsplit(x[2],"-")[[1]][1]))
    frame2013$state = gsub("\\)","",as.character(lapply(strsplit(rownames(frame2013),"\\("),function(x) strsplit(x[2],"-")[[1]][2])))
    features = frame2013
    
    feature_names = names(features)[!names(features) %in% c(non_predictors)]
    
    for(f in feature_names){
      features[,f] = as.numeric(features[,f])
    }
    
    scaled_data = scale(features[,feature_names])
    scaled_data = apply(scaled_data,2,function(x) {
      x[is.na(x)] = -1
      x
    })
    svd_train<-svd(scaled_data,2)$u
    
    newtrain<-data.frame(x=svd_train[,1],y=svd_train[,2],label_code=as.numeric(as.factor(features$party)),label=features$party,state=features$state,name=features$name,full_name=rownames(features),stringsAsFactors=FALSE)
    distances = rowMeans(as.matrix(dist(newtrain[,c("x","y")],method="euclidean")))
    party_distances = tapply(distances,newtrain$label,mean)
    dist_frame = data.frame(D=party_distances['D'],R=party_distances['R'],I=party_distances['I'],congress=c)
    polarization[[length(polarization)+1]] = dist_frame
  }
}

dists = do.call(rbind,polarization)
