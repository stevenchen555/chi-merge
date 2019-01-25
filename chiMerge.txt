chi_merge <- function(attr,freque,threshold){
  contingency_table <- table(attr,freque)
  contingency_table <- as.data.frame.matrix(contingency_table)
  
  print(contingency_table)
  #calculate chi value and create a table
  n = nrow(contingency_table)
  for (i in 1:(n-1) ) {
    chi_val = chiSq(contingency_table[i:(i+1),1:ncol(contingency_table)])
    if(grepl("-",rownames(contingency_table)[i])){
      lower_bound = as.numeric(strsplit(rownames(contingency_table)[i],"-")[[1]][1])
    }else{
      lower_bound = as.numeric(rownames(contingency_table)[i])
    }
    if(grepl("-",rownames(contingency_table)[(i+1)])){
      upper_bound = as.numeric(strsplit(rownames(contingency_table)[(i+1)],"-")[[1]][2])
    }else{
      upper_bound = as.numeric(rownames(contingency_table)[(i+1)])
    }
    if (!exists("chi_table")) chi_table <- c(lower_bound,upper_bound,chi_val)
    else chi_table <- rbind(chi_table,c(lower_bound,upper_bound,chi_val))
  }
  colnames(chi_table) <- c("lower_bound","upper_bound","chi_val")
  chi_table = as.data.frame(chi_table)
  min_chi = min(chi_table$chi_val)
  p_value = qchisq(1 - threshold,ncol(contingency_table)-1)
  print(chi_table)
  #merge the min chi_val entil the threshold is reached
  while(min_chi < p_value){
    n = nrow(chi_table)
    # generate new contingency_table
    for(i in 1:n){
      if(chi_table[i,3] == min_chi){
        if((i==n) || chi_table[(i+1),3] != min_chi){
          if(!exists("index_lower")) index_lower = i
          index_upper = (i+1)
          interval = paste(chi_table[index_lower,1],chi_table[i,2],sep="-")
          rownames(contingency_table)[index_lower] <- interval
          col_num = ncol(contingency_table)
          while(index_upper!=index_lower){
            for (j in 1:col_num){
              contingency_table[(index_upper-1),j] = contingency_table[(index_upper-1),j]+contingency_table[index_upper,j]
            }
            if(!exists("row_index_to_delete")){
              row_index_to_delete <- index_upper
            }
            else{
              row_index_to_delete <- c(row_index_to_delete,index_upper)
            }
            index_upper = index_upper - 1
          }
        }
        # we need to do cumulative merging if two adjacent chi_val are min value
        else{
          if(!exists("index_lower")) index_lower = i
          next
        }  
      }
      if(exists("index_lower")) rm(index_lower)
    }
    contingency_table <- contingency_table[-row_index_to_delete,]
    rm(row_index_to_delete)
    print(contingency_table)
    # calculate chi value and generate chi_table
    n = nrow(contingency_table)
    rm(chi_table)
    for (i in 1:(n-1)){
      chi_val = chiSq(contingency_table[i:(i+1),1:ncol(contingency_table)])
      if(grepl("-",rownames(contingency_table)[i])){
        lower_bound = as.numeric(strsplit(rownames(contingency_table)[i],"-")[[1]][1])
      }else{
        lower_bound = as.numeric(rownames(contingency_table)[i])
      }
      if(grepl("-",rownames(contingency_table)[(i+1)])){
        upper_bound = as.numeric(strsplit(rownames(contingency_table)[(i+1)],"-")[[1]][2])
      }else{
        upper_bound = as.numeric(rownames(contingency_table)[(i+1)])
      }
      if (!exists("chi_table")) chi_table <- c(lower_bound,upper_bound,chi_val)
      else chi_table <- rbind(chi_table,c(lower_bound,upper_bound,chi_val))
    }
    colnames(chi_table) <- c("lower_bound","upper_bound","chi_val")
    chi_table = as.data.frame(chi_table)
    print(chi_table)
    min_chi = min(chi_table$chi_val)
  }
  return(contingency_table)
}

