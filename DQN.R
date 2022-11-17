# create S A R S' tuples

# identify the gameover step
A <- A[-1] # get rid of first NA
R <- R[-1]
H <- H[-1]
tmp <- array(c(S_HR,S_RR,S_BPs,S_BPd,S_BPm,S_T,S_W,S_PaCO2,S_PaO2,S_FiO2,S_SpO2,S_GCS,S_WBC,S_TBI,S_PC,S_ALB,S_PH,S_Ca,S_AC,S_Hb,S_Mg,S_PTT,S_K,S_BUN,S_Cl,S_INR,S_Na,S_TCO2,S_Cr,S_PT),dim=c(length(S_HR),30))
tmp <- tmp[-1,] # get rid of first NA
see <- sapply(1:dim(tmp)[1],function(i,x)sum(is.na(x[i,])),tmp)
Done <- rep(0,dim(tmp)[1])
j <- which(see == dim(tmp)[2])
Done[j-1] <- 1
tmp <- tmp[-j,]
Done <- Done[-j]
A <- A[-j]
R <- R[-j]
H <- H[-j]


# impute missing values
tmp_missing<- tmp
export(tmp, "tmp_missing.csv")
for (i in 1:dim(tmp)[2]) tmp[is.na(tmp[,i]),i] <- median(tmp[,i],na.rm=T) 
export(tmp, "tmp_median.csv")
#knn
tmp_knn <- kNN(tmp_m, k = 5, numFun = weightedMean, weightDist=TRUE)
#mice
tmp_mice <- mice(tmp_missing, m=1, maxit = 3, method = 'pmm', seed = 500)
tmp_mice1<-complete(tmp_mice, 1)
tmpm<-data.matrix(tmp_mice1)


# get the 4 frames
tmp2 <- tmp[c(2:dim(tmp)[1],1),]    # shifting the time
tmp3 <- tmp[c(3:dim(tmp)[1],1:2),]
tmp4 <- tmp[c(4:dim(tmp)[1],1:3),]
tmp5 <- tmp[c(5:dim(tmp)[1],1:4),]
myTuples <- array(c(tmp5,tmp4,tmp3,tmp2,tmp),dim=c(dim(tmp)[1],dim(tmp)[2],1,5))
A[is.na(A)] <- 1


dim(tmp)#8121 30
sum(is.na(R))
windows()
plot(A)
windows()
plot(R)


# myTuples, A, R, we are ready for DQN

# DQN in Keras


range(H)#1,38
set.seed(4)
k <- sample(1:38,30)
j <- which(H %in% k)
length(j) #6437


x_train <- array(myTuples[j,,,],c(length(j),dim(myTuples)[2],dim(myTuples)[3],dim(myTuples)[4]))
x_test  <- array(myTuples[-j,,,],c(dim(myTuples)[1]-length(j),dim(myTuples)[2],dim(myTuples)[3],dim(myTuples)[4]))


dim(x_train) # N:6437   30    1    5
A_train    <- A[j]
A_test     <- A[-j]
R_train    <- R[j]
R_test     <- R[-j]
Done_train <- Done[j]
Done_test  <- Done[-j]
H_train    <- H[j]
H_test     <- H[-j]

h <- unique(H_test) #h:4  8  9 10 14 17 19 21 23 28 29 36


library(keras)
library(tensorflow)


# create our DQN in keras
create_model <- function() {
 model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 8, kernel_size = c(5,1), activation = 'relu',#kernel_size = c(3,1)
                input_shape = c(30, 1, 4)) %>% 
    layer_conv_2d(filters = 8, kernel_size = c(9,1), activation = 'relu') %>% #kernel_size = c(3,1)
    #layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_dropout(rate = 0.25) %>% 
    layer_flatten() %>% 
    layer_dense(units = 256, activation = 'relu') %>% #128
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 25, activation = 'linear') # 25 action class

 model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(learning_rate = 0.0001, decay = 1e-6),
    #optimizer = optimizer_sgd(learning_rate = 0.001, momentum = 0, decay = 0),
    metrics = list("mean_absolute_error")
 )
 model
}

# DQN training epochs
gamma <- 0.9
policyDQN <- create_model()
targetDQN <- create_model()
policyDQN %>% save_model_weights_tf("targetWs.ckpt")
targetDQN %>% load_model_weights_tf("targetWs.ckpt")
batch_size <- 32

i <- 1
dL <- 1

DQNhistory<-data.frame(i=NA,loss=NA)

ptm <- proc.time()#timer

while (i < 5000) {#dL > 1e-7 #i < 10000
      j <- sample(1:dim(x_train)[1], batch_size)
      policy_q <- predict_on_batch(policyDQN, array(x_train[j,,,1:4],c(32,30,1,4)))
      target_q <- policy_q
      see <- predict_on_batch(targetDQN, array(x_train[j,,,2:5],c(32,30,1,4)))
      see <- apply(see,1,max)
      target_q[cbind(1:batch_size,A_train[j])] <- R_train[j] + gamma * see * (1 - Done_train[j])
      #
      
	  if(mean((policy_q - target_q)^2) != 0)
	  {
		train_on_batch(policyDQN, array(x_train[j,,,1:4],c(32,30,1,4)), array(target_q,c(32,25)))#
		if (i %% 50 == 0) {#50 or 10
         policyDQN %>% save_model_weights_tf("targetWs.ckpt")
         targetDQN %>% load_model_weights_tf("targetWs.ckpt")
		 print("model weights tf")
		}
		
		if(i <= 10){
			dL <- mean((policy_q - target_q)^2)
			DQNhistory<-rbind(DQNhistory,c(i,dL))
			print(paste('i = ', i,'loss = ',dL))
		}
		else{
			dL <- mean((policy_q - target_q)^2)
			DQNhistory<-rbind(DQNhistory,c(i,dL))
			DQNhistory[i,2] <- mean(DQNhistory[(i-9):i,2])
			
			print(paste('i = ', i,'loss = ',DQNhistory[i,2]))
		}
		
		
		i <- i + 1		
	  }#end if
}
proc.time() - ptm #End timer

#tiff("DQN.tiff", width=2500, height=1500)
ggplot(DQNhistory, aes(x = i, y = loss)) + geom_point() + #geom_smooth(aes(x=i,y=loss)) + 
labs(title="DQN History",x="step",y="loss") + 
theme(plot.title=element_text(hjust=0.5,size=25))+
theme(axis.text=element_text(size=15),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20))

dev.off()
#end DQN



#adjust DRQN layers( filters and sizse of unit )




#create our DRQN in keras
# dataset for DRQN

x_train2 <- array(x_train[,,,5:4],c(dim(x_train)[1:3],2))
dim(x_train2) <- c(dim(x_train)[1],1,dim(x_train)[2],1,2)
x_train2 <- x_train2[,rep(1,4),,,]

x_test2 <- array(x_test[,,,5:4],c(dim(x_test)[1:3],2))
dim(x_test2) <- c(dim(x_test)[1],1,dim(x_test)[2],1,2)
x_test2 <- x_test2[,rep(1,4),,,]


# DRQN training epochs
x_train2 <- array(x_train[,,,5:4],c(dim(x_train)[1:3],2))


create_model2 <- function() {
 model <- keras_model_sequential() %>%
    time_distributed(layer_conv_2d(filters = 16, kernel_size = c(3,1), activation = 'relu'),input_shape=c(4, 30, 1, 1)) %>%#
    time_distributed(layer_conv_2d(filters = 16, kernel_size = c(3,1), activation = 'relu')) %>%
    time_distributed(layer_dropout(rate = 0.25)) %>%
    time_distributed(layer_flatten()) %>%
    layer_lstm(units = 256, activation = 'relu') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 25, activation = 'linear') # 25 action class

 model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(learning_rate = 0.0001, decay = 1e-6),#learning_rate = 0.0001
    #optimizer = optimizer_sgd(learning_rate = 0.001, momentum = 0, decay = 0),
    metrics = list("mean_absolute_error")
 )
 model
}

myGamma <- 0.9
policyDQN <- create_model2()
targetDQN <- create_model2()
policyDQN %>% save_model_weights_tf("targetWs2.ckpt")
targetDQN %>% load_model_weights_tf("targetWs2.ckpt")
batch_size <- 32


DRQNhistory<-data.frame(i=NA,loss=NA)
ptm <- proc.time()#timer

i <- 1
dL <- 1
while (i<500) {#dL > 1e-5
      j <- sample(1:dim(x_train2)[1], batch_size)#
      #policy_q <- predict_on_batch(policyDQN, array(x_train[j,,,1:4],c(32,6,1,4)))
      policy_q <- predict_on_batch(policyDQN, array(x_train2[j,,,1],c(32,4,30,1,1)))#
      target_q <- policy_q
      see <- predict_on_batch(targetDQN, array(x_train2[j,,,2],c(32,4,30,1,1)))#
      see <- apply(see,1,max)
      target_q[cbind(1:batch_size,A_train[j])] <- R_train[j] + myGamma * see * (1 - Done_train[j])
      train_on_batch(policyDQN, array(x_train2[j,,,1],c(32,4,30,1,1)), array(target_q,c(32,25)))#
      
	  if(sqrt(sum((policy_q - target_q)^2)/batch_size) != 0)
	  {
		if (i %% 10 == 0) {#every 10 steps
         policyDQN %>% save_model_weights_tf("targetWs2.ckpt")
         targetDQN %>% load_model_weights_tf("targetWs2.ckpt")
		 print("model weights tf")
		}
		
		if(i <= 10){#mean for DRQNhistory
			dL <- sqrt(sum((policy_q - target_q)^2)/batch_size)
			DRQNhistory<-rbind(DRQNhistory,c(i,dL))
			print(paste('i = ', i,'loss = ',dL))
		}
		else{
			dL <- sqrt(sum((policy_q - target_q)^2)/batch_size)
			DRQNhistory<-rbind(DRQNhistory,c(i,dL))
			DRQNhistory[i,2] <- mean(DRQNhistory[(i-9):i,2])
			
			print(paste('i = ', i,'loss = ',DRQNhistory[i,2]))
		}
		i <- i + 1		
		#if (i %% 100 == 0) print(paste('i = ', i,'loss = ',dL))
	  }#end if
}
proc.time() - ptm #End timer

#tiff("DRQN.tiff", width=2500, height=1500)
ggplot(DRQNhistory, aes(x = i, y = loss)) + geom_point()+ #+ geom_smooth(aes(x=i,y=loss))
labs(title="DRQN History",x="step",y="loss") + 
theme(plot.title=element_text(hjust=0.5,size=25))+
theme(axis.text=element_text(size=15),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20))


dev.off()