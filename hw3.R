setwd("/Users/jiahonghu/Desktop/HW3")

#### Probelm 1 ####
# x is the n*p data matrix 
# y is the responce variable, -1 and 1 
# B_num is the number of weak classifiers 

## part a ##

## adaboost function is aimed to find alpha, allpars under the adaboost 
AdaBoost=function(x,y,B_num){
  
      n=nrow(x)
  
      # start with an intial weight vector 
      w_original=1/n
      w_vector=rep(w_original,times=n)
  
      ##pre-allocate en empty vector and list 
      alpha=rep(NA,times=B_num)
      allpars=rep(list(list()),length=B_num)
  

      for(i in 1:B_num){
          # step 1 Training parameters using train function 
          pars=train(x,w_vector,y)
          allpars[[i]]=pars
          # step 2  classification y function 
          label=Classify(x,pars)
          # step 3 check errors
          error_ind=(y!=label)
          # step 4 compute errors
          w_error_vector=w_vector[error_ind]
          error=sum(w_error_vector)/sum(w_vector)
          # step 5 compute alpha 
          alpha_value=log((1-error)/error)
          alpha[i]=alpha_value 
          #step 6 compute the new weight 
          w_vector=w_vector*exp(alpha_value*error_ind)
    
    
}
      return (list(alpha=alpha,allpars=allpars))
}



## part b ##
## agg_class function make the final y classfication under the adaboost, which is c_hat

agg_class=function(x,alpha,allpars){
      
      n=nrow(x)
      B_num=length(alpha)
      ## Create an empty list  
      lable_matrix=matrix(NA,nrow=B_num,ncol=n)
  
      for(i in 1:B_num){
          lable_matrix[i,]=alpha[i]*Classify(x,allpars[[i]])
      }
 
      hat=apply(lable_matrix,2,sum)
      c_hat=sign(hat)
      return(c_hat)
}






#### problem 2 ####

#part a #

train=function (x, w, y){
        p=ncol(x)
        n=nrow(x)
        
        y_hat=vector()
        error=vector()
        best_i=vector()
        best_i_error=vector()
        
        
        for (j in 1:p) {
                
                #reoder x, y,w from small to large 
                x_col_j=x[,j]
                ind_order=order(x_col_j)
                x_col_j_new=x_col_j[ind_order]
                y_new=y[ind_order]
                w_new=w[ind_order]
                
                # check for duplicate 
                x_col_j_new_single=unique(x_col_j_new)
                n_new=length(x_col_j_new_single)
                
                for(i in 1:n_new){
                        ind_1=x_col_j_new <= x_col_j_new_single[i]
                        ind_2=x_col_j_new > x_col_j_new_single[i]
                        y_hat[ind_1]=-1
                        y_hat[ind_2]=1
                        error[i]=sum(w_new*(y_new !=y_hat))/sum(w_new)
                        
                }
                
                best_i[j]= x_col_j_new_single[which.min(error)]
                best_i_error[j]=min(error)
        }
        best_j=which.min(best_i_error)
        best_i=best_i[best_j]
        pars<-list(j = best_j, theta = best_i,m = 1)
        return(pars)
}




#part b #
Classify=function(x,pars){
  label=vector()
  j_ind=pars$j
  x_j=x[,j_ind]
  ind_1=x_j <= pars$theta
  ind_2=x_j > pars$theta
  label[ind_1]=-1
  label[ind_2]=1
  return(label)
}



#### problem 3 ####

# test if the algorithm works 
# B_num=8
x<-read.table("uspsdata.txt")
y<-read.table("uspscl.txt")[,1]
n=nrow(x)
set.seed=1
ada_60=AdaBoost(x,y,60)
allpars_60=ada_60$allpars
alpha_60=ada_60$alpha 
c_hat_60=agg_class(x,alpha_60,allpars_60)


## implement 5-fold cross validation 


B_num_max=100

test_error_matrix=matrix(NA,nrow=100,ncol=5)
train_error_matrix=matrix(NA,nrow=100,ncol=5)


set.seed=50
ind=sample.int(200)
ind_1=which(ind<=40)
ind_2=which(ind>=41 & ind<=80)
ind_3=which(ind>=81 & ind<=120)
ind_4=which(ind>=121 & ind<=160)
ind_5=which(ind>=161 & ind<=200)

ind_matrix=matrix(NA,nrow=5,ncol=40)
ind_matrix[1,]=ind_1
ind_matrix[2,]=ind_2
ind_matrix[3,]=ind_3
ind_matrix[4,]=ind_4
ind_matrix[5,]=ind_5


for(i in 1:5){
        test_ind=ind_matrix[i,]
        test_ind=as.vector(test_ind)
        train_ind=ind_matrix[-i,]
        train_ind=as.vector(train_ind)
        
        x_test=x[test_ind,]
        x_train=x[train_ind,]
        
        y_test=y[test_ind]
        y_train=y[train_ind]
        
        result=AdaBoost(x_train,y_train,B_num_max)
        allpars=result$allpars
        alpha=result$alpha 
        
        for(j in 1:B_num_max){
                
                
                c_hat_test=agg_class(x_test,alpha[1:j],allpars[1:j])
                test_error_ind=(c_hat_test != y_test)
                test_error_matrix[j,i]=mean(test_error_ind)
                
                c_hat_train=agg_class(x_train,alpha[1:j],allpars[1:j])
                train_error_ind=(c_hat_train != y_train)
                train_error_matrix[j,i]=mean(train_error_ind)
                
                
                
                
        }
        
        
}


# vector of length b that represent the average test error for each iteration b 
avg_test_b=apply(test_error_matrix,1,mean)
which.min(avg_test_b)
# vector of length b that represent the average train error for each iteration b 
avg_train_b=apply(train_error_matrix,1,mean)






### Problem 4 ###

plot(1:B_num_max,avg_test_b,type="l",main="mean test error for each b")
plot(1:B_num_max,avg_train_b,type="l",main="mean train error for each iteraiton b")











