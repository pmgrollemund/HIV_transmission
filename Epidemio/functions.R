############ ---- is.unique ---- ############ 
#' @title is.unique
#' @description Tests if its argument is a unique vector. 
#' @param v a vector.
#' @export 
#' @example
#' is.unique(1:5)
#' is.unique(c(1:5,4:5))
is.unique <- function(v) length(unique(v)) == length(v)

############ ---- build_yearClass ---- ############ 
#' @title build_yearClass
#' @description Gather years with respect to a specific calendar.
#' @param vec a numerical vector corresponding to years.
#' @param calendar a string vector of range years "YYYY-YYYY", see example.
#' @export 
#' @example
#' quinquennium <- 
#' c("1980-1984","1984-1988","1989-1993","1994-1998","1999-2003","2004-2008",
#'    "2009-2013","2014-2018")
#' years <- sample(1980:2018,10,replace=T)
#' build_yearClass(years,quinquennium)
build_yearClass <- function(vec,calendar){
 res <- rep("",length(vec))
 for(b in calendar){
  years <- as.numeric(strsplit(b,split="-")[[1]])
  years <- seq(years[1],years[2],by=1) 
  
  res[vec %in% years] <- b
 }
 res
}

############ ---- create_new_var ---- ############ 
#' @title create_new_var
#' @description Create new variables from the object "person".
#' @param person a data.table.
#' @param sequences_meta a data.frame.
#' @param biannual a specific calendar, see "build_yearClass" function.
#' @param quinquennium a specific calendar, see "build_yearClass" function.
#' @param min_occur a numerical value corresponding to a threshold. A factor  
#'        concerning less persons than this threshold will be moved as "Other".
#' @param min_occur_county a numerical value corresponding to a threshold. A county  
#'        concerning less persons than this threshold will be moved as "Other".
#' @export 
#' @example
create_new_var <- function(person,sequences_meta,
                           biannual,quinquennium,
                           min_occur = 1000,
                           min_occur_county = 100){
 #### Create other race variables -----
 person[,race2 := cases(
  race == "" | race == "(9)Unknown"                 -> "Unknown",
  race == "(1)Hispanic, All races"                  -> "Hispanic",
  race == "(2)Not Hispanic, Amer Indian/Alaska Nat" -> "Amer. Ind.",
  race == "(3)Not Hispanic, Asian"                  -> "Asian",
  race == "(4)Not Hispanic, Black"                  -> "Black",
  race == "(5)Not Hispanic, Nat Hawaiian/Pac Isl"   -> "Pac. Isl.",
  race == "(6)Not Hispanic, White"                  -> "White",
  race == "(8)Not Hispanic, Multi-race"             -> "Multi-race"
 )]
 
 person[,is.white := cases(
  race2 == "White" -> "White", 
  race2 != "White" -> "Non-white"
 )]
 
 main_races <- names(which(table(person$race2)>min_occur))
 person[  race2 %in% main_races , main_race := race2  ]
 person[!(race2 %in% main_races), main_race := "Other"]
 
 #### Create other country/county variables -----
 person[,birthCountry2 := substring(birthCountry,6) ] 
 {
  person$birthCountry2[person$birthCountry2 == "United States"] <- "USA"
  person$birthCountry2[person$birthCountry2 == "Tanzania, United Republic of"] <- "Tanzania"
  person$birthCountry2[person$birthCountry2 == "Viet nam"] <- "Vietnam"
  person$birthCountry2[person$birthCountry2 == "Cote d'Ivoire (Ivory Coast)"] <- "Ivory Coast"
  person$birthCountry2[person$birthCountry2 == "Hong Kong"] <- "China" ## Yes I know...
  person$birthCountry2[person$birthCountry2 == "Taiwan, Province of China"] <- "Taiwan"
  person$birthCountry2[person$birthCountry2 == "Korea, Republic of (South)"] <- "South Korea"
  person$birthCountry2[person$birthCountry2 == "Lao People's Democratic Republic"] <- "Laos"
  person$birthCountry2[person$birthCountry2 == "United Kingdom"] <- "UK"
  person$birthCountry2[person$birthCountry2 == "Russian Federation"] <- "Russia"
  person$birthCountry2[person$birthCountry2 == "Congo"] <- "Republic of Congo"
  person$birthCountry2[person$birthCountry2 == "Myanmar (Burma)"] <- "Myanmar"
  person$birthCountry2[person$birthCountry2 == "Congo, Democratic Republic of Zaire"] <- "Democratic Republic of the Congo"
  person$birthCountry2[person$birthCountry2 == "Syrian Arab Republic"] <- "Syria"
  person$birthCountry2[person$birthCountry2 == "Iran, Islamic Republic of"] <- "Iran"
  person$birthCountry2[person$birthCountry2 == "Virgin Islands, U.S."] <- "Virgin Islands"
  person$birthCountry2[person$birthCountry2 == "Yugoslavia"] <- "Other"
  person$birthCountry2[person$birthCountry2 == ""] <- "Unknown"
  person$birthCountry2[person$birthCountry2 == "Moldova, Republic of"] <- "Moldova"
  person$birthCountry2[person$birthCountry2 == "Trinidad and Tobago"] <- "Trinidad"
  person$birthCountry2[person$birthCountry2 == "Antigua and Barbuda"] <- "Antigua"
  person$birthCountry2[person$birthCountry2 == "Libyan Arab Jamahiriya"] <- "Libya"
  person$birthCountry2[person$birthCountry2 == "Micronesia, Federated States of"] <- "Micronesia"
  person$birthCountry2[person$birthCountry2 == "U.S. Minor Outlying Areas"] <- "USA"
  person$birthCountry2[person$birthCountry2 == "Korea, Dem People's Rep of (North)"] <- "North Korea"
 }
 birthCou_tmp <- substring(person$birthCountry,2,4)
 person[,main_birthCountry := cases(
  birthCou_tmp %in% c("AFG","AZE","BHR","GEO","IRN","IRQ","ISR","KAZ","LBN","LKA",
                      "PAK","QAT","SAU","SLE","SYR","TUR","UZB","YEM"
  )  -> "Middel East",
  birthCou_tmp %in% c("AGO","BDI","BEN","BFA","BWA",'CIV',"CMR","COD","COG","ERI",
                      "ETH","GAB","GHA","GIN","GMB","GNB","KEN","LBR","LSO","MLI",
                      "MOZ","MWI","NAM","NER","NGA","RWA","SDN","SEN","SOM","SWZ",
                      "TCD","TGO","TON","TZA","UGA","ZAF",'ZMB',"ZWE","MRT"
  )  -> "SSA",
  birthCou_tmp %in% c("ALB","AUT","BEL","BGR","BIH","CHE","CZE","DEU","DJI","DNK",
                      "ESP","FIN","FRA","GBR","GRC","HUN","IRL","ISL","ITA","LUX",
                      "LVA","MDA","NLD","NOR","POL","PRT","ROU","RUS","SVK","SWE",
                      "UKR","YUG"
  )  -> "Europe",
  birthCou_tmp %in% c("ARG","ATG","BHS","BLZ","BOL","BRA",'BRB',"CHL","COL","CRI",
                      "CUB","DMA","DOM","ECU","GTM","GUF","GUY","HND","HTI","JAM",
                      "NIC","PAN","PER","PRI","SLV",'TTO',"URY","VEN","VIR","MTQ"
  )  -> "S/M America",
  birthCou_tmp %in% c("ASM","AUS","FJI","FSM","IDN","NZL","PHL","PNG","PRY"
  )  -> "Ocenia",
  birthCou_tmp %in% c("BMU","GUM","MHL","MNP","UMI","WSM","X98"
  )  -> "Other",
  birthCou_tmp %in% c("CAN","MEX"
  )  -> "Neighbors",
  birthCou_tmp %in% c("CHN","HKG","IND","JPN","KHM","KOR","LAO","MMR","MNG","MYS",
                      "NPL","SGP","THA","TWN","VNM"
  )  -> "Asia",
  birthCou_tmp %in% c("DZA","EGY","LBY","MAR","PRK"
  )  -> "North Africa",
  birthCou_tmp %in% c("USA"
  )  -> "USA",
  birthCou_tmp %in% c("X99",""
  )  -> "Unknown"
 )]
 rm(birthCou_tmp)
 
 person[,is.USA := cases(
  birthCountry2 == "USA" -> "USA",
  birthCountry2 != "USA" -> "Foreign"
 )]
 person[,is.KC := cases(
  cur_county_name == "KING CO." -> "KING CO.",
  cur_county_name != "KING CO." -> "OTHER"
 )]
 
 person[cur_county_name == "", cur_county_name := "OTHER" ]
 tables <- table(person$cur_county_name)
 main_counties <- names(tables[tables>min_occur_county])
 person[cur_county_name %in% main_counties, main_cur_county_name := cur_county_name]
 person[!(cur_county_name %in% main_counties), main_cur_county_name := "OTHER"]
 
 #### Create other transmission variables -----
 person$transm2  <- substring(person$transm,3)
 person[transm2 == "HETEROp",transm2 := "HETERO"]
 
 main_transms <- names(which(table(person$transm2)>min_occur))
 person[,main_transm := cases(
    transm2 %in% main_transms  -> transm2, 
  !(transm2 %in% main_transms) -> "OTHER"
 )]
 
 person[,main_transm2 := cases(
  transm2 %in% c("IDU","MSM/IDU")  -> "IDU", 
  transm2 == "MSM"                 -> "MSM", 
  transm2 == "HETERO" & sex == "M" -> "HSX_M",
  transm2 == "HETERO" & sex == "F" -> "HSX_F", 
  !(transm2 %in% c("IDU","MSM/IDU","MSM","HETERO")) -> "OTHER" 
 )]
 
 person[,is.IDU := transm2 %in% c('MSM/IDU',"IDU")]
 
 #### Create other sex variables -----
 person[,sex2 := cases(
  transm %in% c("MSM","MSM/IDU") | 
   (!(transm %in% c("MSM","MSM/IDU")) & sex == "M" & sex_with_male_and_female == "Y")
  -> "MSM",
  !(transm %in% c("MSM","MSM/IDU")) & sex == "F" & sex_with_male_and_female != "Y" -> "HTsex_F",
  !(transm %in% c("MSM","MSM/IDU")) & sex == "F" & sex_with_male_and_female == "Y" -> "HMsex_F",
  !(transm %in% c("MSM","MSM/IDU")) & sex == "M" & sex_with_male_and_female != "Y" -> "HTsex_M"
 )]
 
 #### Create other age and death variables -----
 person[deathyr == "", age           := 2018                - as.numeric(b_yr) ]
 person[             , age_diag      := as.numeric(hivyr)   - as.numeric(b_yr)]
 person[deathyr != "", age           := as.numeric(deathyr) - as.numeric(b_yr)]
 person[             , death         := deathyr != ""]
 person[death==TRUE  , death_age     := as.numeric(deathyr) - as.numeric(b_yr)]
 person[death==TRUE  , survival_time := as.numeric(deathyr) - as.numeric(hivyr)]
 
 person[,ageClass := cases(
  age <= 14              -> "<15",
  age >= 15  & age <= 24 -> "15-24",
  age >= 25  & age <= 29 -> "25-29",
  age >= 30  & age <= 34 -> "30-34",
  age >= 35  & age <= 39 -> "35-39",
  age >= 40  & age <= 44 -> "40-44",
  age >= 45  & age <= 49 -> "45-49",
  age >= 50              -> ">50"
 )]
 
 #### Create other time variables -----
 person[,hivyr_biann := build_yearClass(hivyr,biannual)]
 person[,hivyr_quinq := build_yearClass(hivyr,quinquennium)]
 
 #### Create variables related to RNA sequences -----
 person[,matching := newnum %in% sequences_meta$newnum  ]   
 
 #### Create variables related to ART -----
 person[,ART := NNRTI_ever == 1 | NRTI_ever == 1  | Protease_ever == 1 | 
         Integrase_ever == 1 | CCR5_ever == 1 | Entry_ever == 1 | 
         Fusion_ever == 1 | Unknown_ever ==1]
 
 first_yr <- cbind(as.numeric(person$NNRTI_drug_first_yr),
                   as.numeric(person$NRTI_drug_first_yr), 
                   as.numeric(person$Protease_drug_first_yr), 
                   as.numeric(person$Integrase_drug_first_yr), 
                   as.numeric(person$CCR5_drug_first_yr), 
                   as.numeric(person$Entry_drug_first_yr), 
                   as.numeric(person$Fusion_drug_first_yr), 
                   as.numeric(person$Unknown_drug_first_yr))
 
 first_yr[is.na(first_yr)] <- Inf
 first_yr <- apply(first_yr,1,min)
 first_yr[first_yr==Inf] <- NA
 
 person[,ART_first_yr   := first_yr]
 person[,one_year_after := hivyr        < first_vl_yr ]
 person[,ART_before_vl  := ART_first_yr < first_vl_yr ]
 
 person[first_vl_yr != "", time_before_first_vl_test := 
         as.numeric(first_vl_yr) - as.numeric(hivyr)]
 person[,time_to_start_ART := as.numeric(first_yr) - as.numeric(hivyr) ]
 
 #### Return result -----
 return(person)
}

############ ---- ggbarplot_wrap ---- ############ 
#' @title ggbarplot_wrap
#' @description XXXX
#' @param XXXX XXXX
#' @export 
#' @example
# var_supp=c("b_yr","deathyr","hivyr")
# x= "main_transm2"
# y=""
# fill="is.USA"
# color="main_transm2"
# alpha="main_transm2"
# size="main_transm2"
# facet_grid=""
# function_facet = function_facet
# args=list(biannual=biannual)
# Melt=F
# xlab=""
# ylab=""
# slab="Transmission"
# flab="Region of origin"
# type=NULL
# range_alpha=c(0.4,1)
# range_size=c(0.1,1.5)
# function_facet <- function(d,args=args){
#  biannual <- args$biannual
#  d[is.na(d$hivyr), hivyr:= Inf]
# 
#  ncol_init <- ncol(d)
#  sup_names <- NULL
#  for(b in biannual){
#   y <- as.numeric(strsplit(b,split="-")[[1]])[2]
#   d <- cbind(d, d$hivyr < y & (
#    (d$b_yr < y & d$deathyr == "") | (d$b_yr < y & y < d$deathyr)) )
#   sup_names <- c(sup_names,y)
#  }
#  sup_names <- as.character(sup_names)
#  names(d)[-(1:ncol_init)] <- sup_names
# 
#  d$deathyr <- NULL
#  d$hivyr   <- NULL
#  d$b_yr    <- NULL
# 
#  d <- melt(d, id.vars=c("x","fill"),measure.vars=sup_names)
#  return(d)
# }
# 
# ggbarplot_wrap(person_WS,var_supp=var_supp,
#                x=x, y=y, color=color,fill=fill,alpha=alpha,size=size,
#                facet_grid=facet_grid,range_alpha=range_alpha,args=args,
#                range_size=range_size,slab=slab,flab,flab,Melt=Melt)
ggbarplot_wrap <- function(dataset, var_supp=NULL, 
                           x,y="",fill="",color="",alpha="",size="",
                           facet_grid="",function_facet = NULL,args=NULL, Melt=TRUE, 
                           xlab="",ylab="",slab=NULL,flab=NULL,clab=NULL,
                           range_alpha=c(0.4,1),range_size=c(0.1,1.5),
                           position=""){
 vars <- unique(c(x,y,fill,color,alpha,size,facet_grid))
 vars <- vars[vars != ""]
 if(!is.null(var_supp)) vars <- c(vars,var_supp)
 names_var <- rbind(c("x" ,x),
                    c("y", y),
                    c("fill" ,fill),
                    c("color",color),
                    c("size" ,size),
                    c("facet",facet_grid))
 
 d <- subset(dataset, select=vars)
 for(i in 1:nrow(names_var)){
  if(any(names(d) == names_var[i,2]))
   names(d)[names(d) == names_var[i,2]] <- names_var[i,1]
 }
 
 if(y == ""){
  if(!is.null(x))    n_x    <- length(unique(d$x))
  if(!is.null(fill)) n_fill <- length(unique(d$fill))
  if(Melt){
   id.vars_tmp <- names(d)[names(d) != "fill"]
   d      <- melt(d, id.vars=id.vars_tmp)
   names(d)[names(d) == "value"] <- "fill"
  }
  if(!is.null(function_facet)){
   d <- function_facet(d,args)
  }
 }else{
  d2 <- NULL
  if(color == "")
   d$color <- rep(1,nrow(d))
  for(c in unique(d$color)){
   for(r in unique(d$x)){
    if(color == ""){
     tmp <- table(d[x == r,y])
    }else{
     tmp <- table(d[x == r & color == c,y])
    }
    if( length(tmp) == 1){
     if(names(tmp) == "TRUE"){
      tmp <- c(0,tmp)
     }else{
      tmp <- c(tmp,0)
     }
    } 
    d2 <- rbind(d2,binconf(tmp[2],sum(tmp),include.x=TRUE,include.n=TRUE))
   }
  }
  d2 <- data.frame(x  = unique(d$x),
                   y = d2[,3],
                   Upper = d2[,'Upper'],
                   Lower = d2[,'Lower'])
  if(color != ""){
   d2$color <- rep(unique(d$color),each =length(unique(d$x    )))
   d2$x     <- rep(unique(d$x    ),times=length(unique(d$color)))
   n_col    <- length(unique(d$color))
  }else{
   d2$x <- unique(d$x)
  }
 }
 
 if(y==""){
  p <- ggplot(d, aes(x=x, fill=fill,alpha=x,color=x,size=x)) +
   scale_alpha_manual(values = c(seq(range_alpha[2],range_alpha[1],le=n_x)), guide='none')+
   scale_fill_manual(values=rainbow(n_fill))
  
  if(slab!="remove"){
   p <- p + scale_size_manual(values = seq(range_size[1],range_size[2],le=n_x)) +
    scale_color_manual(values = rep("black",n_x)) 
  }else{
   p <- p + scale_size_manual(values = seq(range_size[1],range_size[2],le=n_x), guide='none') +
    scale_color_manual(values = rep("black",n_x), guide='none') 
  }
 }else{
  if(color == ""){
   p <- ggplot(d2, aes(x=x, y=y))+ geom_bar(stat = "identity"  ) +
    geom_errorbar(width=.25,
                  aes(ymin=Lower, ymax=Upper))
  }else{
   p <- ggplot(d2, aes(x=x, y=y,fill=color))+ 
    geom_bar(stat = "identity",position=position_dodge()) +
    geom_errorbar(width=.25,position=position_dodge(0.9),aes(ymin=Lower, ymax=Upper))+
    scale_fill_manual(values=rainbow(n_col))
  }
 }
 p <- p +
  theme(axis.text.x=element_text(angle = 90, hjust = 0)) + xlab(xlab) + ylab(ylab)
 
 if(!is.null(clab)) p <- p + labs(fill=clab)
 if(facet_grid != "") p <- p + facet_grid(~facet) 
 if(is.null(function_facet) & position == "" & y == "") p <- p +  
  geom_bar(position = position_stack(reverse = TRUE))
 if(is.null(function_facet) & position == "fill") p <- p +  
  geom_bar(position = "fill") 
 if(!is.null(slab)) p <- p + labs(color = slab, size  = slab)
 if(!is.null(flab)) p <- p + labs(fill  = flab)
 if(!is.null(function_facet)){
  p <- p + facet_grid(~variable)
  for(l in unique(d$variable)){
   p <- p + geom_bar(data = d[variable == l & value ==TRUE,], stat="count",
                     position = position_stack(reverse = TRUE))
  }
 }
 p
 return(p)
}

############ ---- ggline_wrap ---- ############ 
#' @title ggline_wrap
#' @description XXXX
#' @param XXXX XXXX
#' @export 
#' @example
# x <- biannual 
# y <- "total"
# group <- "main_transm2"
# color <- "main_transm2"
# facet_grid <-"is.USA"
# var_supp <- c("b_yr","deathyr","hivyr")
# dataset <- person_WS
# ggline_wrap(person_WS,var_supp =c("b_yr","deathyr","hivyr"),
#             x=biannual, y="total", color="main_transm2",
#             facet_grid='is.USA',
#             ylab="Number of alived people with HIV",
#             glab="Transmission")
ggline_wrap <- function(dataset, var_supp=NULL,
                        x,y="",color="",alpha="",size="",linetype=FALSE,
                        facet_grid="",
                        function_facet = NULL,args=NULL, 
                        Melt=TRUE, 
                        xlab="",ylab="",slab=NULL,clab=NULL,
                        range_alpha=c(0.4,1),range_size=c(0.6,1.5)){
 
 vars <- unique(c(color,alpha,size,facet_grid))
 vars <- vars[vars != ""]
 if(!is.null(var_supp)) vars <- c(vars,var_supp)
 names_var <- rbind(c("x"    ,x),
                    c("y"    ,y),
                    c("color",color),
                    c("size" ,size),
                    c("facet",facet_grid))
 if(length(x) == 1 & is.character(x)){
  vars <- c(vars,x)
 }
 
 d <- subset(dataset, select=vars)
 for(i in 1:nrow(names_var)){
  if(any(names(d) == names_var[i,2]))
   names(d)[names(d) == names_var[i,2]] <- names_var[i,1]
 }
 if(length(x) == 1 & is.character(x)){
  d <- d[!is.na(d$x),]
 }
 
 ncol_init <- ncol(d)
 if(length(x) == 1 & is.character(x)){
  years <- unique(d$x)
  years <- sort(years[!is.na(years)])
 }
 if(length(x) != 1 & is.character(x)){
  years <- as.numeric(unlist(lapply(strsplit(biannual,split="-"),function(v) v[2] ))) 
 }
 # if(is.numeric(x)) years <- sort(x)
 
 for(t in years){
  if(y %in% c('Cumulative','Cumulative percent')){
   d <- cbind(d, as.numeric(d$x) <= t )
  }
  if(y %in% c("total","percent")){
   d <- cbind(d, d$hivyr < t & ( 
    (d$b_yr < t & d$deathyr == "") | (d$b_yr < t & t < d$deathyr)) )
  }
 }
 years <- as.character(years)
 names(d)[-(1:ncol_init)] <- years
 
 if(!is.null(var_supp)) 
  for(var in var_supp) d[,which(names(d) == var )] <- NULL
 
 if(length(x) == 1 & is.character(x)){
  d[,x := NULL] 
 }
 
 if(y=='Cumulative percent'){
  n_dataset <- nrow(dataset)
 }
 
 col_years <- which(names(d) %in% years)
 d2 <- NULL 
 levels1 <- unique(d$color)
 if(facet_grid!="" & size =="")
  levels2 <- unique(d$facet)
 if(size!="" & facet_grid =="")
  levels2 <- unique(d$size)
 
 if(size != "" | facet_grid != "") {
  for(l1 in levels1){
   for(l2 in levels2){
    if(facet_grid!="" & size =="")
     tmp <- as.data.frame(d)[d$color == l1 & d$facet == l2, col_years ] 
    if(size!="" & facet_grid =="")
     tmp <- as.data.frame(d)[d$color == l1 & d$size == l2, col_years ] 
    tmp[is.na(tmp)] <- FALSE 
    total <- apply(as.matrix(tmp),2,sum)
    if(y=='Cumulative percent'){
     total <- total / n_dataset
    }
    d2 <- rbind(d2,
                cbind(rep(l1,length(years)),
                      rep(l2,length(years)),
                      years,
                      total))
   }
  }
  d2 <- data.table(d2)
  colnames(d2) <- c( names(d)[-col_years], "x", "y")
  d2$y <- as.numeric(d2$y)
 }
 if(size == "" & facet_grid == "") {
  for(l1 in levels1){
   tmp <- as.data.frame(d)[d$color == l1, col_years ] 
   tmp[is.na(tmp)] <- FALSE 
   total <- apply(as.matrix(tmp),2,sum)
   if(y=='Cumulative percent'){
    total <- total / n_dataset
   }
   d2 <- rbind(d2,
               cbind(rep(l1,length(years)),
                     years,
                     total))
  }
  d2 <- data.table(d2)
  colnames(d2) <- c( "color", "x", "y")
  d2$y <- as.numeric(d2$y)
 }
 
 if(y=="percent"){
  levels3 <- unique(d$facet)
  for(l3 in levels3){
   for(year in years){
    max_total <- sum(d2[facet == l3 & x == year,y])
    if(max_total != 0)
     d2[facet == l3 & x == year,y] <- d2[facet == l3 & x == year,y] / max_total * 100 
   }
  }
  colnames(d2) <- c("color","facet","x","total","y")
 }
 
 n_col <- length(unique(d2$color))
 
 # if(y=='Cumulative' | y=='Cumulative percent'){
  if(size != ""){
   n_size <- length(unique(d2$size))
   p <- ggplot(d2, aes(x = as.numeric(x), y = y, group = interaction(color,size),colour = color)) +
    scale_size_manual(values = seq(range_size[1],range_size[2],le=n_size))
   if(linetype){
    p <- p + geom_line(aes(linetype=size,size=size)) +
     labs(linetype=slab,size=slab)
   } 
   if(!linetype) p <- p + geom_line(aes(size=size)) + labs(size=slab)
  }else{
   p <- ggplot(d2, aes(x = as.numeric(x), y = y, group = color,colour = color))+
    geom_line() 
  }
 if(facet_grid != "") p <- p + facet_grid(~facet)
 # }
 # if(y=="total" | y=="percent"){
 #  p <- ggplot(d2, aes(x = x, y = y, group = color,colour = color)) +
 #   geom_line() 
 #  
 # }
 p <- p + scale_color_manual(values=rainbow(n_col)) +
  ylab(ylab)+ xlab(xlab) + labs(color = clab)+
  theme(axis.text.x=element_text(angle = 90))
 
 p
 return(p)
}

############ ---- ggmap_wrap ---- ############ 
#' @title ggmap_wrap
#' @description XXXX
#' @param XXXX XXXX
#' @export 
#' @example
# count = table(person_WS[,cur_county_name])
# ggmap_wrap(map="county",
#            count=count,
#            legend_lab = "Count")
# 
# count= table(person_WS[cur_county_name %in% Washington_counties,cur_county_name])
# ggmap_wrap(map="county",legend_lab = "Count", state="washington", count= count,
#            text=TRUE)
# 
# dataset <- person_WS[hivyr < begin & cur_county_name %in% Washington_counties,
#                      c("main_transm", "cur_county_name")]
# 
# tmp  <- table(dataset[main_transm %in% c("MSM","MSM/IDU"), cur_county_name])
# tmp0 <- table(dataset[,c(cur_county_name)])
# 
# tmp <- table(person_WS[hivyr < begin & cur_county_name %in% Washington_counties &
#                         main_transm %in% c("MSM","MSM/IDU"), cur_county_name])
# if(length(tmp) !=  length(tmp0)){
#  tmp2 <- tmp0
#  tmp2[names(tmp0) %in% names(tmp)] <- tmp
#  tmp2[!(names(tmp0) %in% names(tmp))] <- NA
#  tmp <- tmp2
# }
# count <- tmp / tmp0 * 100
# 
# ggmap_wrap(map="county",legend_lab = "Count", state="washington", count= count,
#            text=TRUE)
ggmap_wrap <- function(map,count,submap="",
                       text=FALSE,text_color = "blue", text_size = 4,
                       legend_lab = "",option_palette = "inferno" ){
 if(map=="county"){
  names(count) <- str_to_lower(names(count))
  names(count) <- gsub(names(count),pattern=" co.",replacement="")
 }
 
 if(map=="county"){
  map.region <- data.table(map_data(map))
  if(submap != ""){
   map.region <- map.region[region==submap,]
  }
  names(map.region)[names(map.region) ==  "subregion"] <- "target"
 }
 if(map=="state"){
  map.region <- data.table(map_data('world'))
  names(map.region)[names(map.region) ==  "region"] <- "target"
 }
 
 tmp_map <- data.table(data.frame(
  region = names(count),
  n  = as.numeric(count)))
 setkey(map.region,target)
 setkey(tmp_map,region)
 map.df <- map.region[tmp_map]
 
 # Write in lands 
 if(text){
  to_write <- aggregate(cbind(long, lat) ~ target, data=map.df,
                        FUN=function(x) mean(range(x)))
  to_write$n <- round(tmp_map$n,2)
 }
 
 # Fill the region without data
  if(any(!(unique(map.region$target) %in% names(count)))){
   region0 <- unique(map.region$target)[!(unique(map.region$target) %in% names(count))]
   tmp_map0 <- data.table(data.frame(
    region = region0,
    n  = NA))
   setkey(map.region,target)
   setkey(tmp_map0,region)
   map.df0 <- map.region[tmp_map0]
   
   # Merge both maps
   map.df <- rbind(map.df,map.df0)
   map.df <- map.df[ !is.na(map.df$target)  , ]
  }
 
 # Plot the map 
 p <- ggplot(map.df, aes(x=long, y=lat, fill=n,group=group)) + 
  geom_polygon( aes(group=group),color="black",size=0.05) +
  labs(fill=legend_lab)+ 
  scale_fill_viridis(option = option_palette, direction = -1)
 if(map=="state")
  p <- p + coord_map(xlim = c(-180, 180))
 if(map=="county")
  p <- p + coord_map()
 if(text){
  to_write <- to_write[!is.na(to_write$n), ]
  p <- p + 
   geom_text(data=to_write, aes(long, lat, label = n,group=target),
             colour=text_color,size=text_size)
 }
 
 p
 
 return(p)
}


