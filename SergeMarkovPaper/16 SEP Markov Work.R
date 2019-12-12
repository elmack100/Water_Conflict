remove(list=ls())

setwd("C:/Users/hernd008/Dropbox/WaterScarcity_NationalSecurity/Herndon") #desktop
setwd("C:/Users/Jay/Dropbox/WaterScarcity_NationalSecurity/Herndon") #laptop

load(file="markov_data_adjusted_cell.Rdata") #alt.specifications
attach(markov_data_adjusted_cell)


###########################################################################
###Work for unique cells   conflict type counts############################
###########################################################################

sum(markov_data_adjusted_cell$total)
sum(markov_data_adjusted_cell$protests)
sum(markov_data_adjusted_cell$exposions_remote_violence)
sum(markov_data_adjusted_cell$strategic_developments)
sum(markov_data_adjusted_cell$violence_against_civilians)
sum(markov_data_adjusted_cell$riots)
sum(markov_data_adjusted_cell$violent)
sum(markov_data_adjusted_cell$nonviolent)

subset <- markov_data_adjusted_cell[markov_data_adjusted_cell$total>0,]
length(unique(subset$cell))
subset <- markov_data_adjusted_cell[markov_data_adjusted_cell$protests>0,]
length(unique(subset$cell))
subset <- markov_data_adjusted_cell[markov_data_adjusted_cell$exposions_remote_violence>0,]
length(unique(subset$cell))
subset <- markov_data_adjusted_cell[markov_data_adjusted_cell$strategic_developments>0,]
length(unique(subset$cell))
subset <- markov_data_adjusted_cell[markov_data_adjusted_cell$battles>0,]
length(unique(subset$cell))
subset <- markov_data_adjusted_cell[markov_data_adjusted_cell$violence_against_civilians>0,]
length(unique(subset$cell))
subset <- markov_data_adjusted_cell[markov_data_adjusted_cell$riots>0,]
length(unique(subset$cell))
subset <- markov_data_adjusted_cell[markov_data_adjusted_cell$violent>0,]
length(unique(subset$cell))
subset <- markov_data_adjusted_cell[markov_data_adjusted_cell$nonviolent>0,]
length(unique(subset$cell))

subset <-  markov_data_adjusted_cell[complete.cases(markov_data_adjusted_cell),]
summary(subset$rain)
length(unique(subset$cell))
sum(subset$total)

##########################################################################
##Work for markov chains##################################################
##########################################################################
#while <= seems like an arbitrary choise, this handles the (many) cells that have all zero

#threshold  specification 
markov_data_adjusted_cell$mod_total <- ifelse(total_mean <= 0,0,1) 
markov_data_adjusted_cell$mod_protests <- ifelse( protests_mean <= 0,0,1) 
markov_data_adjusted_cell$mod_explosions_remote_violence <- ifelse( exposions_remote_violence_mean <= 0,0,1) #sorry for the typo
markov_data_adjusted_cell$mod_strategic_developments <- ifelse(  strategic_developments_mean<= 0,0,1) 
markov_data_adjusted_cell$mod_battles <- ifelse(battles_mean<= 0,0,1) 
markov_data_adjusted_cell$mod_violence_against_civilians <- ifelse(violence_against_civilians_mean<= 0,0,1) 
markov_data_adjusted_cell$mod_riots <- ifelse(riots_mean<= 0,0,1) 
markov_data_adjusted_cell$mod_violent <- ifelse(violent_mean<= 0,0,1) 
markov_data_adjusted_cell$mod_nonviolent <- ifelse(nonviolent_mean<= 0,0,1) 

unique_cell <- unique(markov_data_adjusted_cell$cell)
base <- length(unique_cell)

years <- 1997:2018

results <- as.data.frame(matrix(0,nrow=0,ncol=3))

for(j in 2:22){#length(years)-1
  #j <- 2
  
  year_1  <- years[j-1]
  year_2 <- years[j]
  
  subset_years <- markov_data_adjusted_cell[year==year_1|year==year_2,]
  subset_years <- subset_years[order(subset_years$year,subset_years$cell),]
  row.names(subset_years) <- 1:nrow(subset_years)
  
  
  
  year_1_subset <- subset_years[subset_years$year==year_1,]
  
  #mod_total, mod_protests, mod_explosions_remote_violence, mod_strategic_developments, mod_battles
  #mod_violence_against_civilians, mod_riots, mod_violent, mod_nonviolent
  
  base_1 <- nrow(year_1_subset)-sum(year_1_subset$mod_nonviolent) #change dep. on event type
  
  base_2 <- sum(year_1_subset$mod_nonviolent) #change dep. on event type
  
  
  P_11 <- 0
  P_12 <- 0
  P_21 <- 0
  P_22 <- 0
  
  for(i in 1:base){
    
    #mod_total (23), mod_protests (24), mod_explosions_remote_violence(25) mod_strategic_developments (26) 
    #mod_battles (27) mod_violence_against_civilians (28) mod_riots (29) 
    #mod_violent (30) mod_nonviolent (31)
    
    if(subset_years[i,31]==0 & subset_years[i+26026,31]==0){ #change dep. on event type
      P_11 <- P_11+1
    }else if(subset_years[i,31]==0 & subset_years[i+26026,31]==1){#change dep. on event type
      P_12 <- P_12+1
    }else if(subset_years[i,31]==1 & subset_years[i+26026,31]==0){#change dep. on event type
      P_21 <- P_21+1
    }else {
      P_22 <- P_22+1
    }
  }
  
  p_11 <- P_11/base_1
  p_12 <- P_12/base_1
  p_21 <- P_21/base_2
  p_22 <- P_22/base_2
  
  
  year_results <- as.data.frame(matrix(0,nrow=8),ncol=3)
  year_results[,1] <- paste0(year_1,"/",year_2)
  year_results[,2] <- c("P_11","P_12","P_21","P_22","p_11","p_12","p_21","p_22")
  year_results[,3] <- c(P_11,P_12,P_21,P_22,p_11,p_12,p_21,p_22)
  
  results <- rbind(results, year_results)
}

colnames(results) <- c("years","identifier","count_or_prob")

par( mfrow = c(2,2))

peace_peace_absolute <- results[results$identifier=="p_11",]
#plot(peace_peace_absolute$count_or_prob,type="l",
#     main = "% Transition peaceful -> peaceful")
mean(peace_peace_absolute$count_or_prob)
      
peace_war_absolute <- results[results$identifier=="p_12",]
#plot(peace_war_absolute$count_or_prob,type="l",
#     main = "% Transition peaceful -> violent")
mean(peace_war_absolute$count_or_prob)

war_peace_absolute <- results[results$identifier=="p_21",]
#plot(war_peace_absolute$count_or_prob,type="l",
#     main = "% Transition violent -> peaceful")
mean(war_peace_absolute$count_or_prob)

war_war_absolute <- results[results$identifier=="p_22",]
#plot(war_war_absolute$count_or_prob,type="l",
#     main = "% Transition violent -> violent")
mean(war_war_absolute$count_or_prob)


