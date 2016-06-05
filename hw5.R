setwd("/Users/jiahonghu/Desktop/hw5")

### problem 2 

## part a 

e_x<-seq(from=0,to=4,length=100)
ex_density<-dexp(e_x,rate=1)
plot(e_x,ex_density,type="l",main="exponential density of theta=1",xlab=" X",ylab="Density")

## part b 
x_b=c(1,2,4)
l_value=function(x){
        exp(-x)
}
l_value_set=l_value(x_b)
likelihood=prod(l_value_set)

plot(e_x,l_value(e_x),type="l",xlab="X", ylab="likelihood")
points(x_b,l_value_set,pch=2,col="red",main="likelihood plot")

## part c 

e_x<-seq(from=0,to=4,length=100)
ex_density_2<-dexp(e_x,rate=2)
plot(e_x,ex_density_2,type="l",main="exponential density of theta=2",col="red",xlab=" X",ylab="Density")
lines(e_x,ex_density)
 
x_b=c(1,2,4)
l_value_2=function(x){
        2*exp(-2*x)
}
l_value_set_2=l_value_2(x_b)
likelihood_2=prod(l_value_set_2)

plot(e_x,l_value_2(e_x),type="l",xlab="X", ylab="likelihood")
points(x_b,l_value_set_2,pch=2,col="red")
lines(e_x,l_value(e_x))
points(x_b,l_value_set,pch=2,col="blue")


## part g 

data<-rexp(n=256,rate=1)
alpha_0<-2
beta_0<-0.2
n=1:256
theta_list<-seq(from=0,to=4,length=100)
alpha=NULL
beta=NULL
      
for(i in 1:256){
        
        alpha[i]=alpha_0+n[i]
        beta[i]=beta_0+sum(data[1:i])
        
}

a=dgamma(theta_list,shape=alpha[4],scale=1/beta[4])
plot(theta_list,a,ylim=c(0,8),type="l",col="red")
b=dgamma(theta_list,shape=alpha[8],scale=1/beta[8])
lines(theta_list,b,col="blue")
c=dgamma(theta_list,shape=alpha[16],scale=1/beta[16])
lines(theta_list,c,col="green")
d=dgamma(theta_list,shape=alpha[256],scale=1/beta[256])
lines(theta_list,d,col="yellow")


legend("topright",c("n=4","n=8","n=16","n=256"),col=c("red","blue","green","yellow"),cex=0.9,lty=c(1,1,1,1))







