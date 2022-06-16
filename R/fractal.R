library(VoxR)
library(data.table)
#read in TLS cloud
tree<-fread("input/trees.xyz")

#normalize density to 1 cm
vox_ls<-list()
res<-c(0.01, 0.1, 0.5, 2, 5, 10)
vox.temp<-vox(tree, res[1])

vox_ls[[1]]<-data.frame(filled=nrow(vox.temp),
           res= res[1])

for(i in 2:length(res)){
  temp<-vox(vox.temp, res[i])
  
  vox_ls[[i]]<-data.frame(filled=nrow(temp),
                          res= res[i])
}

fractal<-do.call(rbind,vox_ls)
fractal$divider<-1/fractal$res
fractal$V<-fractal$filled*(fractal$res^3)

plot(log(fractal[,c(3,1)]))
m<-lm(log(filled)~log(divider),fractal[,c(1,3)])
lines(log(fractal$divider), predict(m), col="red")
summary(m)

3-(log(fractal$V)/log(fractal$divider))

plot(log(fractal$res), log(fractal$V))
plot(log(fractal$res), log(fractal$V))


plot(log(fractal[,c(4,1)]))
m<-lm(log(filled)~log(divider),fractal[,c(1,3)])
lines(log(fractal$divider), predict(m), col="red")
summary(m)
