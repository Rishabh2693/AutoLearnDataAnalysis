img.df <- read.csv(file="hw1-data.csv",head=TRUE,sep=",");
print(img.df);
img.vec <- unlist(img.df);
print(img.vec);
img.mean <- sum(img.vec)/length(img.vec);
img.mean;
img.stdDev <- sqrt(sum((img.vec-img.mean)*(img.vec-img.mean))/(length(img.vec)-1))
img.stdDev;
img.mm.vec <- ((img.df - min(img.df))*255)/(max(img.df)-min(img.df));
print(img.mm.vec);
img.zn.vec <- (img.df-img.mean)/img.stdDev;
print(img.zn.vec);
