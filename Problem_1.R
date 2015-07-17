#	step	link #	connections created	# connections	chains #
#	1	4	0	                0       	1
#	2	1	0               	0       	2
#	3	5	1               	1       	2
#	4	7	0               	1       	3
#	5	3	1               	2       	3
#	6	2	2               	4       	2
#	7	8	1               	5       	2
#	8	6	2               	7       	1

max_sub_chains <- function(nl){
        #generate links drawing order
        links <- sample(1:nl)
        n_sub_chains <- 0
        conn_counter <- 0
        for(i in 1:nl){
                # decide how many connections between links will be formed
                # between links at each step
                if((links[i]-1) %in% links[1:(i-1)]){conn_counter<-conn_counter+1}
                if((links[i]+1) %in% links[1:(i-1)]){conn_counter<-conn_counter+1}
                # record the maximum number of subchains
                temp=i-conn_counter
                n_sub_chains=ifelse(temp>n_sub_chains,temp,n_sub_chains)
        }
        return(n_sub_chains)
}

sub_chain_mean_sd <- function(nl,num_sim = 1000,sample_size = 1000){
        # set seed for reproducibility
        set.seed(1)
        # apply central limit theorem
        Mmean <- vector(mode="numeric", length=num_sim)
        for(j in 1:num_sim){
                # generate a vector maximum numbers of subchains
                M<-rep(nl, sample_size)
                M <- sapply(M,max_sub_chains)
                # record it's mean
                Mmean[j] <- mean(M)  
        }
        return(c(mean(Mmean),sd(Mmean)))
}


sub_chain_mean_sd(nl=8,num_sim = 1000,sample_size = 1000)
# [1] 2.83143900 0.01898986
sub_chain_mean_sd(nl=16,num_sim = 2000,sample_size = 2000)
# [1] 5.06638025 0.01854437
sub_chain_mean_sd(nl=32,num_sim = 5000,sample_size = 5000)
# [1] 9.37496808 0.01712874
