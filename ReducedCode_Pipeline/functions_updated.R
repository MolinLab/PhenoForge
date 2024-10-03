
library(readxl)
library(tidyverse)
library(reshape2)
library(stringr)
library(sjmisc)
library(ggpubr)
library(matrixStats)
library(gridExtra)
library(corrplot)

list_extension<-function(x){
  list_Fc_Plant<-list()
  list_Kinetic<-list()
  list_Hc_Plant<-list()
  list_Hc_avgSpectrum<-list()
  list_Rgb_Morpho_Plant<-list()
  list_Rgb_Color_Plant<-list()
  
  list_allLists<-list()
  y<-c("Fc_Plant","Kinetic", "Hc_Plant", "Hc_avgSpectrum", "Rgb_Morpho_Plant",
       "Rgb_Color_Plant", "Rgb_Morph_Plant")
  for (i in x){
    #y<-c("Fc_Plant","Kinetic", "Hc_Plant", "Hc_avgSpectrum", "Rgb_Morpho_Plant","Rgb_Color_Plant" )
    for (k in y){
      if (str_contains(i,k)==TRUE){
        vector_indices<-c()
        vector_indices[length(vector_indices)+1]=which(x==i)
        
        if (k == "Kinetic"){
          list_Kinetic<-append(list_Kinetic, list_dfs[vector_indices])
          
        }
        
        else if(k == "Hc_Plant"){
          list_Hc_Plant<-append(list_Hc_Plant, list_dfs[vector_indices])
          
        }
        
        else if (k == "Rgb_Morpho_Plant"){
          list_Rgb_Morpho_Plant<-append(list_Rgb_Morpho_Plant,list_dfs[vector_indices])
          
        }
        
        else if (k=="Rgb_Morph_Plant"){
          list_Rgb_Morpho_Plant<-append(list_Rgb_Morpho_Plant,list_dfs[vector_indices])
        }
        
        else if (k == "Rgb_Color_Plant"){
          list_Rgb_Color_Plant<-append(list_Rgb_Color_Plant,list_dfs[vector_indices])
          
        }
        
        else if(k=="Fc_Plant"){
          list_Fc_Plant<-append(list_Fc_Plant,list_dfs[vector_indices])
          
        }
        
        else if (k == "Hc_avgSpectrum"){
          list_Hc_avgSpectrum<-append(list_Hc_avgSpectrum,list_dfs[vector_indices])
          
        }}   
      
    }}
  list_allLists<-list(list_Kinetic, list_Hc_avgSpectrum, 
                      list_Hc_Plant, list_Fc_Plant, list_Rgb_Color_Plant, list_Rgb_Morpho_Plant)
  names(list_allLists)<-c("list_Kinetic", "list_Hc_avgSpectrum","list_Hc_Plant", 
                          "list_Fc_Plant", "list_Rgb_Color_Plant", "list_Rgb_Morpho_Plant")
  return(list_allLists)
}



join_and_calc<-function(x){
  x%>%
    inner_join(dplyr::select(metatable, -'Tray Info'),
               by="Plant ID")%>%group_by(variable, Condition)%>%
    summarise_at(vars(value), list(Mean=mean, SD=sd))%>%mutate(Date=x$'Measuring Date'[1])
}

join_and_calcDate<-function(x){
  x%>%
    inner_join(dplyr::select(metatable, -'Tray Info'),
               by="Plant.ID")%>%group_by('Measuring Date', Condition)%>%
    summarise_at(vars(value), list(Mean=mean, SD=sd))
}




select_indices<-function(x,y){
  result<-lapply(x, function(z){
    z%>%filter(variable==y)})
  
  return(result)
}



indices.to.list<-function(x,y){element101<-list_meltTables_Named[which(names(list_meltTables_Named)==y)]%>%
  select_indices(x)%>%bind_rows()
return(element101)}


select_date<-function(x,y){
  result<-lapply(x, function(z){
    z%>%filter(Measuring.Date==y)
  })
  return(result)
}

select_columns<-function(x,y){
  result<-lapply(x, function(z){
    z%>%select(all_of(y))})
  return(result)
}

select_condition<-function(x,y){
  result<-lapply(x, function(z){
    z%>%filter(Condition==y)})
  
  return(result)
}


testing_fun<-function(x,z){x%>%slice(grep(z, Genotype))}


merge_and_select<-function(x){
  x%>%merge(metatable, by="Plant ID", all=TRUE)%>%dplyr::select(- 'Tray Info')}



make_trayInfo_tables<-function(x){
  
  new_list<-list()
  for (i in x){
    tray_column<-data.frame(Tray=1)
    #tray_list<-list()
    
    for(k in i$`Plant ID`){
      tray_number<-str_extract_all(k, "^\\d+")
      
      tray_column[nrow(tray_column)+1,]<-tray_number
    }
    tray_column<-tray_column[-1,]
    #tray_list[[length(tray_list)+1]]<<-tray_column
    #new_list<-list()
    new_list[[length(new_list)+1]]<-i%>%mutate(Tray=tray_column)
  }
  
  return(new_list)}


###for a single df () e.g. metatable)
function_make_trayINfo<-function(i){tray_column<-data.frame(Tray=1)
#tray_list<-list()
new_list<-list()
for(k in i$`Plant ID`){
  tray_number<-str_extract_all(k, "^\\d+")
  
  tray_column[nrow(tray_column)+1,]<-tray_number
  
  
  
}
tray_column<-tray_column[-1,]
#tray_list[[length(tray_list)+1]]<<-tray_column
#new_list<-list()
new_list[[length(new_list)+1]]<-i%>%mutate(Tray=tray_column)
}



###############


build_water_info<-function(x){
  vector_indices_trays<-c()
  for(k in unique(x$Tray)){
    
    for(t in list_water_trayNumbers){
      if(k%in%t==TRUE){
        
        vector_indices_trays[length(vector_indices_trays)+1]<-match(t, list_water_trayNumbers)
      }
      
    }
    
  }
  
  dataframe_new<-do.call(cbind.data.frame, water_readings_justValues[,vector_indices_trays])
  dataframe_new<-dataframe_new%>%
    mutate(Mean=rowMeans(dataframe_new[,1:ncol(dataframe_new)]))%>%
    mutate(SD=rowSds(as.matrix(dataframe_new),cols = grep("DS", colnames(dataframe_new))))%>%mutate(Date=water_readings_DSOnly$Date)%>%relocate(Date)
  return(dataframe_new)
}


######################
#RGB code

#new function triplet_to_colour() hexcode genereation for list and renaming in dfs:


triplet_to_colour<-function(x){
  vector_convertedColours<-c()
  p<-colnames(x[,3:ncol(x)])
  for(i in p){
    
    
    mystring<-i
    
    extracted_numbers <- unlist(strsplit(regmatches(mystring, 
                                                    gregexpr("(\\d+(?:,\\d+)*)", mystring))[[1]], ","))
    
    
    colour<-rgb(extracted_numbers[1],extracted_numbers[2], extracted_numbers[3], maxColorValue = 255)
    vector_convertedColours[length(vector_convertedColours)+1]<-colour}
  colnames(x)<-c(colnames(x[1:2]), vector_convertedColours)
  return(x)
  
}


#########################################
#merging the datframes for ( wide ) PCA, kmeans and other stuff


############jetzt erstmal so, fusing the Hc and Fc data
fuse_me<-function(df_split){
  
  
  data_merged<-lapply(list_cutFc, function(x){
    
    df_split_date <- as.Date(df_split$`Measuring Date`[1])
    x_date <- as.Date(x$`Measuring Date`[1])
    
    
    if(df_split_date == x_date | df_split_date +1 == x_date){
      x$`Measuring Date`<-df_split$`Measuring Date`
      inner_join(df_split, x, by=c("Measuring Date", "Plant ID"))
    }
    else {
      return(NULL)
    }
  }
  )
  
  data_merged <- Filter(Negate(is.null), data_merged)
  
  if (length(data_merged) == 0) {
    data_merged <- list(df_split)
  }
  
  
  #names(data_merged) <- timepoints[1:length(data_merged)]
  return(data_merged)
  
}
####fusing hcFc and RGB data
#kann ich später noch zu eienr zusammenfügen Fubktion


fuse_me2<-function(df_split){
  
  
  data_merged<-lapply(list_cutRGB_C_hexcodes, function(x){
    
    df_split_date <- as.Date(df_split$`Measuring Date`[1])
    x_date <- as.Date(x$`Measuring Date`[1])
    
    
    if(df_split_date == x_date | df_split_date +1 == x_date){
      x$`Measuring Date`<-df_split$`Measuring Date`
      inner_join(df_split, x, by=c("Measuring Date", "Plant ID"))
    }
    else {
      return(NULL)
    }
  }
  )
  
  data_merged <- Filter(Negate(is.null), data_merged)
  
  if (length(data_merged) == 0) {
    data_merged <- list(df_split)
  }
  
  
  #names(data_merged) <- timepoints[1:length(data_merged)]
  return(data_merged)
  
}


fuse_me_Bonitur<-function(df_split, extra_data){
  
  extraData_list<-split_dataframe(extra_data, "Measuring Date")
  
  data_merged<-lapply(extraData_list, function(x){
    
    df_split_date <- as.Date(df_split$`Measuring Date`[1])
    x_date <- as.Date(x$`Measuring Date`[1])
    
    
    if(df_split_date == x_date | df_split_date +1 == x_date){
      x$`Measuring Date`<-df_split$`Measuring Date`
      inner_join(df_split, x, by=c("Measuring Date", "Plant ID", "Genotype"))
    }
    else {
      return(NULL)
    }
  }
  )
  
  data_merged <- Filter(Negate(is.null), data_merged)
  
  if (length(data_merged) == 0) {
    data_merged <- list(df_split)
  }
  
  
  #names(data_merged) <- timepoints[1:length(data_merged)]
  return(data_merged)
  
}



###kleien funktion, die mir die dataframes splittet wie ich es will split() alleine tut das nicht....

split_dataframe <- function(df, column) {
  split_list <- split(df, df[[column]])
  return(split_list)
}
##################################################################


##################################################################
#random forests loops
#####Indices and Genotypes from Dates
create_Geno_rfs<-function(df_list){
  
  list_Geno_rf<-list()
  list_TG_rf<-list()
  
  pip<-lapply(df_list, function(df){
    
    rf_name<-paste0("rfGeno",gsub("-", "",df$Index[1]))
    rf_Index_Geno <- rfsrc(Genotype ~ .,
                           data = dplyr::select(df,Condition,Genotype,
                                                where(is.numeric)),
                           ntree=999, importance = TRUE)
    #assign(rf_name, rf_Index_Geno, envir= .GlobalEnv)
    
  })
  names(pip)<-list_indices_Hc
  return(pip)
}


create_TG_rfs<-function(df_list){
  
  list_Geno_rf<-list()
  list_TG_rf<-list()
  
  pip<-lapply(df_list, function(df){
    
    rf_name<-paste0("rfTG",gsub("-", "",df$Index[1]))
    rf_Index_Geno <- rfsrc(TG ~ .,
                           data = dplyr::select(df,Condition,Genotype,
                                                where(is.numeric)),
                           ntree=999, importance = TRUE)
    #assign(rf_name, rf_Index_Geno, envir= .GlobalEnv)
    
  })
  names(pip)<-list_indices_Hc
  return(pip)
}



#####genotypes from Indices


create_TG_rfs_GenoIndex<-function(df_list){
  
  list_Geno_rf<-list()
  list_TG_rf<-list()
  
  pip<-lapply(df_list, function(df){
    
    rf_name<-paste0("rfTG",gsub("-", "",df$Genotype[1]))
    rf_Index_Geno <- rfsrc(TG ~ .,
                           data = dplyr::select(df,Condition,`Measuring Date`,
                                                where(is.numeric)),
                           ntree=999, importance = TRUE)
    assign(rf_name, rf_Index_Geno, envir= .GlobalEnv)
    
  })
  names(pip)<-list_of_names
  return(pip)
}

