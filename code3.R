###########
###########

a <- "!series_matrix_table_begin"
Design_matrix <-  as.matrix(read.table("/home/chadia/Bureau/GSE21374_series_matrix.txt",header=TRUE,sep="\n")) ;
row_start <- which(Design_matrix==a)
row_start <- row_start+1
mat_res <-  Design_matrix[c(row_start:nrow(Design_matrix)-1),]

t<- matrix()
res_unlist <- list()
for(i in 2:length(mat_res) ){
	res_unlist[i] <-  strsplit(mat_res[[i]],"\t") 
 
}
t<- do.call(rbind,res_unlist)
write.table( t,"table1.xlsx",row.names = FALSE,col.names = FALSE,sep="\t" )



##########

a2 <- "!Sample_characteristics_ch1"
a3 <- "!Series_sample_id"
 
Design <-  as.matrix(read.table("/home/chadia/Bureau/GSE21374_series_matrix.txt",header=TRUE,sep="\n")) ;

 res_unlist_Design <- list()
 final_matrix<- matrix()
 final_matrix_titles<- matrix()
for(i in 1:nrow(Design )){
 res_unlist_Design[i] <-  strsplit(Design[[i]],"\t") 
 }
 
  
 m<-0
 
 for(i in 1:length(res_unlist_Design)){
 final_matrix<- do.call(rbind,res_unlist_Design[i])
  
 if(final_matrix[1,1]==a2){
    
 m=m+1
 n = ncol(final_matrix)
 
 }
 if(final_matrix[1,1]==a3){
 titles <- final_matrix

 }
 
 }
 
 final_matrix_all <- matrix(nrow=m,ncol=n)
 
 j<-1
 for(i in 1:length(res_unlist_Design)){
 final_matrix<- do.call(rbind,res_unlist_Design[i])
  
 if(final_matrix[1,1]==a2){
   for(k in 1:ncol(final_matrix))
 final_matrix_all[j,k]  <- final_matrix[1,k] 
 
 j=j+1
 
 }
 }

title_list <- list()  
title_vector <- matrix()

title_list <-  strsplit(titles[1,2]," ") 

title_list <- do.call(rbind,title_list)

title_vector <- cbind(titles[1,1],title_list)

final_matrix_all_2 <- matrix()

final_matrix_all_2 <-rbind(title_vector,final_matrix_all)


write.table( final_matrix_all_2,"Samples_info.xlsx",row.names = FALSE,col.names = FALSE,sep="\t" )

###########

samples_selected <-  as.matrix(read.table("/home/chadia/Bureau/Geo_samples_all",header=TRUE,sep="\t")) ;

 
rownames(t) <- t[,1] 

colnames(t) <- t[1,] 

final_samples_selected <-  t[ ,samples_selected[,1] ]


write.table( final_samples_selected,"final_samples_selected.xlsx",row.names = TRUE,col.names = FALSE,sep="\t" )

###########
rownames(final_matrix_all_2) <- final_matrix_all_2[,1] 

colnames(final_matrix_all_2) <- final_matrix_all_2[1,] 

info_final_samples_selected <-  final_matrix_all_2[ ,samples_selected[,1] ]

write.table( info_final_samples_selected,"info_final_samples_selected.xlsx",row.names = TRUE,col.names = FALSE,sep="\t" )


###########


###########Samples test######

info_final_samples_selected_test <- info_final_samples_selected[c(3,4,5,),]

rownames(info_final_samples_selected_test) <- c()

rownames_tests <- do.call(rbind,strsplit(info_final_samples_selected_test[,1], ":"))

rownames(info_final_samples_selected_test) <- rownames_tests[,1]
 
 
 
for(j in 1:ncol(info_final_samples_selected_test)){
c <- strsplit(info_final_samples_selected_test[,j], ":")
 
info_final_samples_selected_test [1,j] <- c$"time of biopsy post transplant (days)"[2]
info_final_samples_selected_test [2,j] <- c$"time from biopsy to failure/censoring (days)"[2]
info_final_samples_selected_test [3,j] <- c$"sample included in the main analysis- the first biopsy per patient from the 105 late (> 1 year) patients"[2]
 
}

Samples_selected_First_20_probs <- rbind(final_samples_selected[c(1:21),],info_final_samples_selected_test)
  
write.table( Samples_selected_First_20_probs,"Samples_selected_First_20_probs.xlsx",row.names = TRUE,col.names = FALSE,sep="\t" )



