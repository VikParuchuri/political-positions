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

features = read.csv("stored_data/midi_features.csv",stringsAsFactors=FALSE)

midi_feats = read.csv("stored_data/generated_midi_features.csv",stringsAsFactors=FALSE)

bad_feats = c("fs","enc","fname")
non_predictors = c("label","fs","enc","fname","label_code")

names(features)[(ncol(features)-4):ncol(features)] = non_predictors

names(midi_feats)[(ncol(midi_feats)-1):ncol(midi_feats)] = c("label_code","label")
features = features[,!names(features) %in% bad_feats]
features = do.call(rbind,list(features,midi_feats))

features$label_code = as.numeric(as.factor(features$label))
feature_names = names(features)[!names(features) %in% c(non_predictors,"X")]

for(f in feature_names){
  features[,f] = as.numeric(features[,f])
}

scaled_data = scale(features[,feature_names])
scaled_data = apply(scaled_data,2,function(x) {
  x[is.na(x)] = -1
  x
})
svd_train<-svd(scaled_data,2)$u

newtrain<-data.frame(x=svd_train[,1],y=svd_train[,2],label_code=features$label_code,label=features$label)

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
p = p + geom_point(aes(colour =label_code)) + scale_size_area(max_size=20)
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
