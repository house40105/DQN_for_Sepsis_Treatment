# off-policy evaluation (OPE) by importance sampling (WIS)
library(keras)

pi_1 <- create_model()
pi_1 %>% load_model_weights_tf("targetWs.ckpt")
h #4  8  9 10 14 17 19 21 23 28 29 36

dim(x_test) #1270   30    1    5


knn_train <- cbind(x_train[,,1,1],x_train[,,1,2],x_train[,,1,3],x_train[,,1,4])

# dqn policy

V <- NULL
rho_H <- NULL
for (i in 1:length(h)) {
    j <- which(H_test == h[i])
    pi_1q <- predict_on_batch(pi_1, array(x_test[j,,,1:4],c(length(j),30,1,4))) #
    a_1 <- apply(pi_1q,1,which.max)
    knn_test <- cbind(x_test[j,,1,1],x_test[j,,1,2],x_test[j,,1,3],x_test[j,,1,4])
    a_0 <- knn(knn_train,knn_test,factor(A_train),k=100,prob=T)

    r <- 0
    tmp <- 1
    for (k in 1:length(j)) {
        if (a_1[k] == as.integer(as.character(a_0[k])))
           tmp <- tmp * 0.9/attributes(a_0)$prob[k]
        else 
           tmp <- tmp * 0.1/attributes(a_0)$prob[k]
        r <- r + myGamma^(k-1) * R_test[j[k]]
    }
    rho_H <- c(rho_H, tmp)
    V <- c(V, tmp*r)
}
V <- V/mean(rho_H)
print(mean(V)) #-20.70511  
#rho_H 3.453061e-173  0.000000e+00  6.188316e-95  1.742027e-28 6.377458e-213  3.766781e-76  2.083377e-66  1.676795e-15
#V -5.950680e-156   0.000000e+00  -3.993732e-78  -2.898505e-11 -5.988928e-196  -2.610248e-60  -1.427517e-51  -1.656409e+02





# dqn, random, null and clinician policies

V1 <- NULL
rho_H1 <- NULL
Vr <- NULL
rho_Hr <- NULL
V0 <- NULL
rho_H0 <- NULL
Vb <- NULL

for (i in 1:length(h)) {               # 8 trajectories
    j <- which(H_test == h[i])
    pi_1q <- predict_on_batch(pi_1, array(x_test[j,,,1:4],c(length(j),30,1,4)))#
    a_1 <- apply(pi_1q,1,which.max) # the action that has the maximal q value
    knn_test <- cbind(x_test[j,,1,1],x_test[j,,1,2],x_test[j,,1,3],x_test[j,,1,4])
    a_b <- knn(knn_train,knn_test,factor(A_train),k=100,prob=T)
    a_bp <- attributes(a_b)$prob
    a_bp[a_bp > 0.9] <- 0.9
    a_bp[a_bp < 0.1] <- 0.1
    a_b <- as.integer(as.character(a_b))

    r <- 0
    tmp1 <- 1
    tmpr <- 1
    tmp0 <- 1
    tmpb <- 1
    for (k in 1:length(j)) {              # steps in a trajectory
        r <- r + myGamma^(k-1) * R_test[j[k]]

        if (a_b[k] == A_test[j[k]] & a_1[k] == A_test[j[k]])
           tmp1 <- tmp1 * 0.9/a_bp[k]
        if (a_b[k] == A_test[j[k]] & a_1[k] != A_test[j[k]])
           tmp1 <- tmp1 * 0.1/(actionsN-1)/a_bp[k]
        if (a_b[k] != A_test[j[k]] & a_1[k] == A_test[j[k]])
           tmp1 <- tmp1 * 0.9/((1-a_bp[k])/(actionsN-1))
        if (a_b[k] != A_test[j[k]] & a_1[k] != A_test[j[k]])   
           tmp1 <- tmp1 * 0.1/(actionsN-1)/((1-a_bp[k])/(actionsN-1))
        a_r <- sample(1:actionsN, 1)
        if (a_b[k] == A_test[j[k]] & a_r == A_test[j[k]])
           tmpr <- tmpr * 1/actionsN/a_bp[k]
        if (a_b[k] == A_test[j[k]] & a_r != A_test[j[k]])
           tmpr <- tmpr * (actionsN-1)/actionsN/a_bp[k]
        if (a_b[k] != A_test[j[k]] & a_r == A_test[j[k]])
           tmpr <- tmpr * 1/actionsN/((1-a_bp[k])/(actionsN-1))
        if (a_b[k] != A_test[j[k]] & a_r != A_test[j[k]])
           tmpr <- tmpr * (actionsN-1)/actionsN/((1-a_bp[k])/(actionsN-1))

        if (a_b[k] == A_test[j[k]] & A_test[j[k]] == 1)
           tmp0 <- tmp0 * 0.9/a_bp[k]
        if (a_b[k] == A_test[j[k]] & A_test[j[k]] != 1)
           tmp0 <- tmp0 * 0.1/(actionsN-1)/a_bp[k]
        if (a_b[k] != A_test[j[k]] & A_test[j[k]] == 1)
           tmp0 <- tmp0 * 0.9/((1-a_bp[k])/(actionsN-1))
        if (a_b[k] != A_test[j[k]] & A_test[j[k]] != 1)
           tmp0 <- tmp0 * 0.1/(actionsN-1)/((1-a_bp[k])/(actionsN-1))
        #if (is.infinite(tmpr) | is.infinite(tmp0)) print(k)
        #print(paste(k,tmpr,tmp0))
    }   # steps
    if (is.finite(tmp1) & is.finite(tmpr) & is.finite(tmp0)) {
       rho_H1 <- c(rho_H1, tmp1)
       V1 <- c(V1, tmp1*r)
       rho_Hr <- c(rho_Hr, tmpr)
       Vr <- c(Vr, tmpr*r)
       rho_H0 <- c(rho_H0, tmp0)
       V0 <- c(V0, tmp0*r)
       Vb <- c(Vb, r)
    }
}
V1 <- V1/mean(rho_H1)
Vr <- Vr/mean(rho_Hr)
V0 <- V0/mean(rho_H0)

print(paste('dqn:',mean(V1)))
print(paste('random:',mean(Vr)))
print(paste('null:',mean(V0)))
print(paste('clinician:',mean(Vb)))

print(paste('dqn:',median(V1)))
print(paste('random:',median(Vr)))
print(paste('null:',median(V0)))
print(paste('clinician:',median(Vb)))

#"dqn: -20.7051064344117"
#"random: -38.3196948277905"
#"null: -20.7051064344117"
#"clinician: -15.8216565428686"

#"dqn: -4.86805199928573e-188"
#"random: -6.40895640690736e-63"
#"null: -4.86805199928573e-188"
#"clinician: -12.57491083516"


#mean 
#"dqn: -20.7051064344117"
#"random: -38.3196948277905"
#"null: -20.7051064344117"
#"clinician: -20.6032086237993"

#median
#"dqn: -4.97663930013302e-140"
#"random: -1.30802858293843e-64"
#"null: -4.97663930013302e-140"
#"clinician: -20.1940532144605"