


setwd("/Users/jiahonghu/Desktop/hw4")
H<-matrix(readBin("histograms.bin","double",640000),40000,16)
dim(H)

for(i in 1:40000){
        for(j in 1:16){
                if (H[i,j]==0){
                        H[i,j]=H[i,j]+0.01
                }
        }
}

##Problem 1##
#EM algorithm 

###input###
#H - dim=n*d 
#K - number of clusters 
#tau - terminate threshold paramter

MultinomialEM=function(H,K,tau){
        
        n=nrow(H) # number of histograms 
        d=ncol(H) # number of bin per histogram 
        
        
        delta=1000 # meansure the change of assigments between iteraiton i and i+1 
        i=1 # the number of iteration of the algorithm 
        prop=rep(1/K,K) # initiate a cluster proportion 
        
        # step 1:Initial centriod by selecting K rows from n rows of H matrix 
        # ctd is a matrix of dim = k*d  and represents the centriod of K clusters, each row represents a centriod of a cluster 
        set.seed=1
        ctd=H[sample(1:n,K),]
        dim(ctd)
        
        # step 2: EM algorithm 
        # the algorithm stops only if the change is less than tau 
        while(delta>tau){
                
                # E-step
                
                phi=exp(H%*%t(log(ctd)))
                phi_cp=phi*prop
                row_sum_phi_cp=apply(phi_cp,1,sum)
                inv_row_sum=1/row_sum_phi_cp
                A_matrix=matrix(rep(inv_row_sum,K),n,K)
                A=A_matrix*phi_cp
                
                # M-step
                
                ctd_current=ctd # theta at the iteration i 
                ctd=t(A)%*%H
                
                col_sum_ctd=apply(ctd,1,sum)
                inv_col_sum=1/col_sum_ctd
                
                ctd_matrix=matrix(rep(inv_col_sum,d),K,d)
                ctd=ctd_matrix*ctd # new theta at the iteraiton i +1 
                
                prop_sum=sum(apply(A,2,sum)) 
                prop=prop/prop_sum # update proportion for clusters
                
                i=i+1
                
                delta=norm(ctd_current-ctd,type="O")
                
                
                
        }
        return(result=apply(A,1,which.max)) 
}


##Problem 2 ##

# for tau=10,50,100 

a1=MultinomialEM(H,K=3,tau=10)
a1
summary(as.factor(a1))
a2=MultinomialEM(H,K=4,tau=10)
a2
summary(as.factor(a2))
a3=MultinomialEM(H,K=5,tau=10)
a3
summary(as.factor(a3))

b1=MultinomialEM(H,K=3,tau=50)
summary(as.factor(b1))
b2=MultinomialEM(H,K=4,tau=50)
summary(as.factor(b2))
b3=MultinomialEM(H,K=5,tau=50)
summary(as.factor(b3))



## problem 3 ## 

img_function=function(x){
        x_matrix=matrix(x,nrow=200,byrow=TRUE)
        img_matrix=NULL
        ind=nrow(x_matrix)
        for( i in 0:(ind-1)){
              img_matrix=cbind(img_matrix,x_matrix[ind-i,]) 
        }
        image(x=1:200,y=1:200,img_matrix)
}


img_function(a1)
img_function(a2)
img_function(a3)
img_function(b3)