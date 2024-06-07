library(data.table)
library(rlang)
library(agricolae)
library(tidyr)
#===============================================================================
# fill na 
change.na <- function(x){
  x[is.na(x)] <- ""
  return(x)
}

change.na.num <- function(x){
  x[is.na(x)] <- 0
  return(x)
}


change.zero <- function(x){
  x[is.na(x)] <- "0(0%)"
  return(x)
}

#===============================================================================
#去重複計算 ----
countd <- function(x){
  n <- length(unique(x))
  return(n)
}

#count NA
count.na <- function(x){
  n <- sum(is.na(x))
  return(n)
}

# check zero
count_zero <- function(x){
  return(sum(x!=0))
}

#===============================================================================
# 字串欄位切割:
# 拆分欄位(一欄裡有多筆類別)
split.question <- function(dt,id.var,split.var,connect.sign=";"){
  dt <- data.table(dt)
  #split
  s1 <- strsplit(dt[,get(split.var)],connect.sign)
  s <- unlist(s1)
  #rep id
  n  <- unlist(lapply(s1,length))
  dt.n <- dt[,id.var,with=F]
  id <-  dt.n[as.numeric(rep(row.names(dt.n), n)),]
  #output
  dt.f <- data.table(id,s)
  colnames(dt.f) <- c(id.var,split.var)
  return(dt.f)
}

# 合併多筆row在同個group
combin.question <- function(dt,id.var,combin.var,connect.sign=";"){
  dt <- data.table(dt)
  dt[,volumn := 1:.N,by=c(id.var)]
  if(length(id.var) == 1){
    m.formula <- as.formula(paste0(id.var," ~ ","volumn"))
  }else{
    id.var.v <- paste(id.var,collapse = " + ")
    m.formula <- as.formula(paste0(id.var.v," ~ ","volumn"))
  }
  dt.t <- dcast(dt,m.formula,value.var = combin.var)
  dt.t[,new:=apply( dt.t[,colnames(dt.t)[(length(id.var) + 1) : ncol(dt.t)],with=F] ,
                    1 , paste , collapse = connect.sign )]
  #remove na
  re <- paste0(connect.sign,"NA")
  dt.t[,new:=gsub(re,"",new)]
  dt.f <- dt.t[,c(id.var,"new"),with=F]
  colnames(dt.f)[colnames(dt.f) == "new"] <- combin.var
  return(dt.f)
}

#category test
category.test <- function(m.table){
  
  #m.table <- table(dt.now)
  m.table.o <- m.table 
  
  if(!is.null(row.names(m.table)) & !is.null(colnames(m.table))){
    m.table <- m.table[!is.na(row.names(m.table)),]
    if(is.null(colnames(m.table))){
      m.table <- m.table[!is.na(names(m.table))]
    }else{
      m.table <- m.table[,!is.na(colnames(m.table))] 
    }
  }
  
  
  if(!is.table(m.table) & !is.matrix(m.table)){
    p.value <- NA
    method <- NA
  }else if(nrow(m.table)==1 | ncol(m.table)==1 ){
    p.value <- NA
    method <- NA
  }else if(sum(apply(m.table.o,2,count_zero) == 0) >=1){
    p.value <- NA
    method <- NA
  }else{
    c.test <- chisq.test(m.table,simulate.p.value = T)
    if(sum(c.test$expected < 5) >  length(m.table)*0.2){
      fisher.res <- fisher.test(m.table,simulate.p.value=TRUE)
      p.value <- round(fisher.res$p.value,3)
      p.value[p.value==0] <- "<0.001"
      method <- "Fisher Exact test"
    }else{
      p.value <- round(c.test$p.value,3)
      p.value[p.value==0] <- "<0.001"
      method <- "chi-squared test"
    }
  }
  return(list(table = m.table.o,p.value=p.value,method=method))
  
}

#numeric test
numeric.test.ind <- function(dt,need.col,group){
  
  dt <- data.table(dt)
  dt.now <- dt[,c(need.col,group),with=F]
  colnames(dt.now) <- c("value","group")
  
  dt.now[is.na(group),group:="NA"]
  summary <- dt.now[,.(`Mean(sd)` = paste0(round(mean(value,na.rm=T),3),"(" ,round(sd(value,na.rm=T),3),")"),
                       `Median(IQR)` = paste0(round(median(value,na.rm=T),3),"(" ,round(IQR(value,na.rm=T),3),")"),
                       N=.N,
                       na.n = count.na(value)),
                    by=group]
  
  dt.now[group == "NA",group:=NA]
  #判斷數值是否都一致
  
  
  dt.now <- dt.now[complete.cases(dt.now)]
  er.dt <- dt.now[,countd(value),by=group]
  
  
  
  if(countd(dt.now$group)==1 | summary[group != "NA" & (N - na.n)<3,.N]>0 | er.dt[,sum(V1 ==1)]>0 ){
    p.value <- c(NA,NA)
    method <- c("Mean(sd)","Median(IQR)")
    return(list(summary = summary ,p.value=p.value,method=method))
  }else if(countd(dt.now$group)==2){
    t.res <- t.test(value~group,dt.now)
    w.res <- wilcox.test(value~group,dt.now,exact =F)
    p.value <- c(round(t.res$p.value,3),round(w.res$p.value,3))
    p.value[p.value==0] <- "<0.001"
    method <- c("Mean(sd)","Median(IQR)")
    return(list(summary = summary ,p.value=p.value,method=method))
  }else{
    a.test <- aov(value~group,dt.now)
    a <- HSD.test(a.test, "group", group=TRUE)
    post.res <- data.table(rownames(a$groups),a$groups$groups)
    colnames(post.res) <- c("group.name","group")
    k.test <- kruskal.test(value ~ group, data = dt.now)
    p.value <- c(round(summary(a.test)[[1]]$`Pr(>F)`[1],3),round(k.test$p.value,3))
    p.value[p.value==0] <- "<0.001"
    method <- c("Mean(sd)","Median(IQR)")
    return(list(summary = summary ,p.value=p.value,method=method,post.res=post.res))
  }
  
}


#p.value change star
p.star <- function(x){
  x[x == "<0.001"] <- 0
  a <- x
  x.n <- as.numeric(x)
  a[x.n >0.05] <- ""
  a[x.n <= 0.05 & x.n>=0.01] <- "*"
  a[x.n < 0.01 & x.n>=0.001] <- "**"
  a[x.n < 0.001 ] <- "***"
  return(a)
}

#merge list
merge.list <- function(x,column,type="all"){
  
  if(type == "all"){
    for(i in 1:(length(x)-1)){
      if(i == 1){
        dtm <- merge(x[[1]],x[[2]],by=column,all=T)
      }else{
        dtm <- merge(dtm,x[[(i+1)]],by=column,all=T)
      }
    }
  }else if(type=="intersect"){
    for(i in 1:(length(x)-1)){
      if(i == 1){
        dtm <- merge(x[[1]],x[[2]],by=column)
      }else{
        dtm <- merge(dtm,x[[(i+1)]],by=column)
      }
    }
  }
  
  return(dtm)
}


#create.table1
create.table1 <- function(dt,need.col,group="",column.level="",item.level="",
                          overall=T,class.both="", percent_na=F){
  
  
  need.col <- need.col[need.col != group]
  column.level <- column.level[column.level!= group]
  #----overall
  tot.n <- dt[,.N]
  ## category count
  char.col <- need.col[!sapply(dt[,need.col,with=F],is.numeric)]
  if(sum(class.both !="")!=0){
    char.col <- c(char.col,class.both)
    char.col <- unique(char.col)
  }
  
  if(length(char.col)!=0){
    data.char <- dt[,char.col,with=F]
    data.char <- data.table(apply(data.char,2,as.character))
    data.char[,obs:=1:.N]
    data.char.m <- melt(data.char,id.vars="obs")
    data.char.res.count <- data.char.m[,.N,by=.(variable,value)]
    
    if(percent_na){
      data.char.res.all <- data.char.m[!is.na(value),.(all=.N),by=.(variable)]
    }else{
      data.char.res.all <- data.char.m[,.(all=.N),by=.(variable)]
    }
    
    data.char <- merge(data.char.res.count,data.char.res.all,by="variable",all=T)
    data.char[,percent:=paste0("(", round(N/all,4)*100,"%", ")")]
    
    if(percent_na){
      data.char[is.na(value), percent:=""]
    }
    data.char.overall <- data.char[,.(variable,value,overall=paste0(N, percent))] 
    colnames(data.char.overall)[1:2] <- c("column","item") 
  }else{
    data.char.overall <- list()
  }
  
  ##numeric 
  num.col <- need.col[sapply(dt[,need.col,with=F],is.numeric)]
  if(sum(class.both!="")!=0){
    num.col <- c(num.col,class.both)
    num.col <- unique(num.col)
  }
  
  if(length(num.col)!=0){
    data.num <- dt[,num.col,with=F]
    data.num <- data.table(apply(data.num,2,as.numeric))
    data.num[,obs:=1:.N]
    data.num.m <- melt(data.num,id.vars="obs")
    data.num <- data.num.m[,.(`Mean(sd)`=paste0(round(mean(value,na.rm = T),2),"(",round(sd(value,na.rm = T),2),")"),
                              `Median(IQR)`=paste0(round(median(value,na.rm = T),2),"(",IQR=round(IQR(value,na.rm = T),2),")"),
                              `(Min,Max)`=paste0("(",round(min(value,na.rm = T),2),",",round(max(value,na.rm = T),2),")"),
                              na.n = as.character(count.na(value))),by=c("variable")]
    
    data.num.overall <- melt(data.num,id.vars = "variable",value.name = "overall",variable.name = "variable.1")
    colnames(data.num.overall)[1:2] <- c("column","item")
  }else{
    data.num.overall <- list()
  }
  
  
  data.base.overall <- rbind(data.char.overall,data.num.overall)
  colnames(data.base.overall)[3] <- paste0("overall","(n=",tot.n,")")
  data.base.overall[is.na(item),item:=""]
  
  ###group
  if(group!=""){
    
    ###category group
    char.col <- need.col[!sapply(dt[,need.col,with=F],is.numeric)]
    if(sum(class.both !="")!=0){
      char.col <- c(char.col,class.both)
      char.col <- unique(char.col)
    }
    
    if(length(char.col)!=0){
      char.col <- c(group,char.col)
      char.col <- char.col[!duplicated(char.col)]
      data.char <- dt[,char.col,with=F]
      data.char <- data.table(apply(data.char,2,as.character))
      data.char[,obs:=1:.N]
      data.char.m <- melt(data.char,id.vars=c("obs",group))
      data.char.res.count <- data.char.m[,.N,by=c(group,"variable","value")]
      
      if(percent_na){
        data.char.res.all <- data.char.m[!is.na(value),.(all=.N),by=c(group,"variable")]
      }else{
        data.char.res.all <- data.char.m[,.(all=.N),by=c(group,"variable")]
      }
      
      data.char <- merge(data.char.res.count,data.char.res.all,by=c(group,"variable"),all=T)
      data.char[,percent:=paste0("(",round(N/all,4)*100,"%)")]
      if(percent_na){
        data.char[is.na(value), percent:=""]
      }

      data.char[,out:=paste0(N, percent)]
      data.char.group <- dcast(data.char,variable + value~get(group),value.var = "out") 
      
      if(data.char.group[,.N == 1]){
        colnames(data.char.group)[1:2] <- c("column","item")
      }else{
        data.char.group <- data.table(data.char.group[,c("variable","value"),with=F],
                                      apply(data.char.group[,-c("variable","value"),with=F],
                                            2,
                                            change.zero)
        )
        colnames(data.char.group)[1:2] <- c("column","item")
      }
    }else{
      data.char.group  <- list()
    }
    
    ##numeric group ----
    num.col <- need.col[sapply(dt[,need.col,with=F],is.numeric)]
    if(sum(class.both!="")!=0){
      num.col <- c(num.col,class.both)
      num.col <- unique(num.col)
    }
    if(length(num.col)!=0){
      num.col_n <- c(group,num.col)
      num.col_n <- num.col_n[!duplicated(num.col_n)]
      data.num <- dt[,num.col_n,with=F]
      data.num <- data.num[ , (num.col) := lapply(.SD,as.numeric),.SDcols = num.col]
      data.num[,obs:=1:.N]
      data.num.m <- melt(data.num,id.vars=c("obs",group))
      data.num <- data.num.m[,.(`Mean(sd)`=paste0(round(mean(value,na.rm = T),2),"(",round(sd(value,na.rm = T),2),")"),
                                `Median(IQR)`=paste0(round(median(value,na.rm = T),2),"(",IQR=round(IQR(value,na.rm = T),2),")"),
                                `(Min,Max)`=paste0("(",round(min(value,na.rm = T),2),",",round(max(value,na.rm = T),2),")"),
                                na.n = as.character(count.na(value))),by=c("variable",group)]
      data.num.m <- melt(data.num,id.vars = c("variable",group),variable.name = "variable.1")
      data.num.group <- dcast(data.num.m,variable+variable.1~get(group),value.var = "value")
      
      colnames(data.num.group)[1:2] <- c("column","item")
    }else{
      data.num.group  <- list()
    }
    
    ### test ----
    need.col.test <- need.col[!(need.col%in%group)]
    data.type <- sapply(dt[,need.col.test,with=F],class)
    data.type[names(data.type) %in% class.both] <- "both"
    
    data.test <- list()
    i <- 1
    for(i in 1:length(need.col.test)){
      column <-  need.col.test[i]
      dt.now <- dt[,c(group,need.col.test[i]),with=F]
      colnames(dt.now)[1] <- "group"
      dt.now[,group := as.character(group)]
      
      if(data.type[i]== "both"){
        test.res.1 <- category.test(table(dt.now))
        test.res.2 <- numeric.test.ind(dt= dt.now,
                                       need.col = column,
                                       group = "group")
        
        p.value <- c(test.res.1$p.value,test.res.2$p.value)
        method  <- c(test.res.1$method,test.res.2$method) 
        
      }else{
        if(data.type[i]=="character" | data.type[i]=="factor"){
          test.res <- category.test(table(dt.now))
        }else{
          test.res <- numeric.test.ind(dt= dt.now,
                                       need.col = column,
                                       group = "group")
        }
        
        p.value <- test.res$p.value
        method  <- test.res$method 
      }
      
      data.test[[i]] <- data.table(column,p.value,method)  
    }
    
    data.test.res <- do.call(rbind,data.test)
    data.base.group <- rbind(data.char.group,data.num.group)
    data.base.group[is.na(item),item :=""]
    
    data.num.test <- data.test.res[method %in% c("Mean(sd)","Median(IQR)","na.n")]
    
    if(data.num.test[,.N]!=0){
      data.m <- merge(data.base.group,data.num.test,by.x=c("column","item"),by.y=c("column","method"),all.x=T) 
      data.m[,method:=""]
      
      data.test.res <- data.test.res[method !="Median(IQR)" | is.na(method)]
      data.test.res[method=="Mean(sd)",p.value:=""]
      data.test.res[method=="Mean(sd)",method:=""]
    }else{
      data.m <- data.base.group
      data.m[,p.value:=""]
      data.m[,method:=""]
    }
    
    ###merge 
    #merge ----
    if(overall==T){
      data.m <- merge(data.m, data.base.overall,by=c("column","item"))
    }
    
    #order
    #column levels ----
    if(sum(column.level=="")==1 ){
      column.level <- need.col
      data.m$column <- factor(data.m$column,levels = column.level)
    }else{
      column.no <- setdiff(need.col,column.level)
      column.level <- c(column.level,column.no)
      data.m$column <- factor(data.m$column,levels = column.level)
    }
    
    #item levels ----
    if(sum(data.type == "factor") != 0){
      dt.now <- dt[,names(data.type[data.type == "factor"]),with=F]
      item.level <- c(item.level,unlist(lapply(dt.now,levels)))
    }
    
    item.level <- item.level[!(duplicated(item.level,fromLast = T))]
    item.level <- item.level[!is.na(item.level)]
    
    if(sum(item.level=="")==1){
      level.n <- unique(data.m$item)[!(unique(data.m$item) %in% c("","Mean(sd)","Median(IQR)","(Min,Max)","na.n"))]
      level.n <- as.character(level.n)
      item.level <- c(level.n,"","Mean(sd)","Median(IQR)","(Min,Max)")
      data.m$item <- factor(data.m$item,levels = item.level)
      
    }else{
      item.no <- setdiff(unique(data.m$item)[!(unique(data.m$item) %in% c("","Mean(sd)","Median(IQR)","(Min,Max)","na.n"))],
                         as.character(item.level))
      
      item.level <- c(as.character(item.level),item.no,"","Mean(sd)","Median(IQR)","(Min,Max)","na.n")
      item.level <- item.level[!is.na(item.level)]
      data.m$item <- factor(data.m$item,levels = item.level)
    }
    
    data.m <- data.m[order(column,item)]
    
    #clean table
    data.m[,obs:=1:.N,by=column]
    data.m[,row.obs:=1:.N]
    t.dt <- tibble(data.m)
    
    if(sum(class.both!="")!=0){
      data.test.res <- data.test.res[!(column %in% class.both & p.value == "")]
    }
    
    for(i in 1:data.test.res[,.N]){
      n.column <- data.test.res[i,column]
      n.p.value <- data.test.res[i,p.value]
      n.method <- data.test.res[i,method]
      data.m1 <- data.table(data.m)
      idx <- data.m1[column==n.column & obs==1,row.obs]
      idx <- idx+(i-1)
      
      data.m <- tibble::add_row(t.dt, column=n.column,p.value=n.p.value,method=n.method,.before = idx)
      t.dt <- tibble(data.m)
    }
    
    data.final <- data.table(t.dt)
    data.final[!is.na(obs),column:=""]
    data.final <- data.final[,-c("obs","row.obs")]
    
    data.final <- data.table(apply(data.final,2,change.na))
    
    # each group n ----
    data.group.n <- dt[,.N,by=group]
    colnames(data.group.n)[1] <- "item" 
    
    i <- 1
    for(i in 1 :data.group.n[,.N]){
      colnames(data.final)[colnames(data.final)==data.group.n[i,item]] <- paste0(data.group.n[i,item],
                                                                                 "(n=",data.group.n[i,N],")")
    }
    
  }else{
    
    data.type <- sapply(dt[,need.col,with=F],class)
    data.m <- data.base.overall
    #no group
    #order
    #column levels ----
    if(sum(column.level=="")==1 ){
      column.level <- need.col
      data.m$column <- factor(data.m$column,levels = column.level)
    }else{
      column.no <- setdiff(need.col,column.level)
      column.level <- c(column.level,column.no)
      data.m$column <- factor(data.m$column,levels = column.level)
    }
    
    #item levels ----
    if(sum(data.type == "factor") != 0){
      dt.now <- dt[,names(data.type[data.type == "factor"]),with=F]
      item.level <- c(item.level,unlist(lapply(dt.now,levels)))
    }
    
    item.level <- item.level[!(duplicated(item.level,fromLast = T))]
    item.level <- item.level[!is.na(item.level)]
    
    if(sum(item.level=="")==1){
      level.n <- unique(data.m$item)[!(unique(data.m$item) %in% c("","Mean(sd)","Median(IQR)","(Min,Max)","na.n"))]
      level.n <- as.character(level.n)
      item.level <- c(level.n,"","Mean(sd)","Median(IQR)","(Min,Max)")
      data.m$item <- factor(data.m$item,levels = item.level)
      
    }else{
      item.no <- setdiff(unique(data.m$item)[!(unique(data.m$item) %in% c("","Mean(sd)","Median(IQR)","(Min,Max)","na.n"))],
                         as.character(item.level))
      
      item.level <- c(as.character(item.level),item.no,"","Mean(sd)","Median(IQR)","(Min,Max)","na.n")
      item.level <- item.level[!is.na(item.level)]
      data.m$item <- factor(data.m$item,levels = item.level)
    }
    
    data.m <- data.m[order(column,item)]
    
    # clean table nogroup----
    data.empty <- data.table(column.level,"")
    data.m[,obs:=1:.N,by=column]
    data.m[,row.obs:=1:.N]
    t.dt <- tibble(data.m)
    for(i in 1:data.empty[,.N]){
      n.column <- data.empty[i,column.level]
      n.item <- data.empty[i,V2]
      data.m1 <- data.table(data.m)
      idx <- data.m1[column==n.column & obs==1,row.obs]
      idx <- idx+(i-1)
      
      data.m <- tibble::add_row(t.dt, column=n.column,item=n.item,.before = idx)
      t.dt <- tibble(data.m)
    }
    
    data.final <- data.table(t.dt)
    data.final[!is.na(obs),column:=""]
    data.final <- data.final[,-c("obs","row.obs")]
    data.final <- data.table(apply(data.final,2,change.na))
    
  }
  
  colnames(data.final)[c(1:2)] <- c("","") 
  return(data.final)
}



##fit.lm 
my.lm <- function(dt,formula.list,round.n=3,model,column.level=""){
  
  dt <- data.frame(dt)
  dt <- data.table(dt)
  
  dt.final <- list()
  type.level.list <- list()
  ref.level.list <- list()
  
  for(i in 1:length(formula.list)){
    
    s.split <- strsplit(formula.list[[i]],"~")
    variable <- strsplit(s.split[[1]][2],"\\+")[[1]]
    variable <- gsub(" ","",variable)
    
    y <- s.split[[1]][1]
    y <- gsub(" ","",y)
    
    dt.now <- dt[,c(y,variable),with=F]
    
    #check type
    data.type <- sapply(dt[,variable,with=F],class)
    f.col <- variable[data.type == "character" ]
    o.col <- variable[data.type != "character" ]
    ## character change vector
    dt.f <- data.table(dt.now[,c(y,o.col),with=F]  ,
                       dt.now[,lapply(.SD,as.factor),.SDcols=f.col])
    
    #reference table
    f.col <- variable[sapply(dt.f[,variable,with=F],class) == "factor"]
    ref <- unlist(lapply(dt.f[,f.col,with=F],levels))
    variable.n <- unlist(lapply(dt.f[,f.col,with=F],nlevels))
    type <- rep(f.col,variable.n)
    ref.dt <- data.table(type,ref)
    ref.dt[,m.col:=paste0(type,ref)]
    
    #model
    formula.n <- as.formula(formula.list[[i]])
    
    if(model == "lm"){
      n.lm <- lm(formula.n,data=dt.f)
    }else if(model == "logit"){
      n.lm <- glm(formula.n,data=dt.f,family=binomial(link="logit"))
    }
    
    lm.res <- summary(n.lm)
    variable.n <- rownames(lm.res$coefficients)
    estimate <- lm.res$coefficients[,c(1,4)]
    estimate.dt <- data.table(variable.n,estimate)
    colnames(estimate.dt)[3] <- "p.value"
    estimate.dt$Estimate <- round(estimate.dt$Estimate,round.n)
    estimate.dt[,p.value:=as.character(round(p.value,3))]
    estimate.dt[p.value==0,p.value:="<0.001"]
    
    #ci
    ci <- confint(n.lm)
    ci.table <- data.table(rownames(ci) ,ci)
    ci.table[,`2.5 %`:=round(`2.5 %`,round.n)]
    ci.table[,`97.5 %`:=round(`97.5 %`,round.n)]
    
    
    #merge
    dt.res <- merge(estimate.dt,ci.table,by.x="variable.n",by.y="V1",all=T)
    if(ref.dt[,.N]  == 0){
      dt.res[,type:=variable.n]
      dt.res[,ref:=""]
    }else{
      dt.res <- merge(dt.res,ref.dt,by.x="variable.n",by.y="m.col",all=T)
      dt.res[is.na(type),type:=variable.n]
    }
    
    m.formula <- formula.list[[i]]
    dt.f <- dt.res[,.(m.formula,type,ref,Estimate,`2.5 %`, `97.5 %`,p.value)]
    dt.f[,type:=factor(type,levels= c("(Intercept)",variable) )]
    
    #level 
    #ref level
    if(ref.dt[,.N]  != 0){
      ref.level <- ref.dt[,ref]
      ref.level <- ref.level[!duplicated(ref.level)]
      dt.f[,ref:=factor(ref,levels = ref.level )]
    }else{
      ref.level <- ""
    }
    
    #column level
    if(sum(column.level=="")==1 ){
      column.level <- unique(dt.f$type)
      dt.f[,type:=factor(type,levels = column.level)]
    }else{
      column.level <- column.level[column.level != "(Intercept)"]
      column.no <- setdiff(unique(dt.f$type),c(column.level,"(Intercept)"))
      column.level <- c("(Intercept)",column.level,column.no)
      dt.f[,type:=factor(type,levels = column.level)]
    }
    
    dt.f <- dt.f[order(type,ref)]
    dt.final[[i]] <- data.table(apply(dt.f,2,change.na))
    type.level.list[[i]] <- column.level
    ref.level.list[[i]] <- ref.level
    
  }
  
  
  
  type.level <- unique(unlist(type.level.list))
  ref.level <- unique(unlist(ref.level.list))
  
  dt.output <- list(res = dt.final,
                    type.level=type.level,
                    ref.level=ref.level)
  
  return(dt.output)
}

#create lm table
create.lm.table <- function(lm.res,table.type){
  #cbind
  if(table.type == "cbind"){
    dt.f <- list()
    for(i in 1:length(lm.res$res)){
      
      dt.n <- lm.res$res[[i]]
      
      s.split <- strsplit(dt.n$m.formula[1],"~")
      y <- s.split[[1]][1]
      y <- gsub(" ","",y)
      
      
      dt.temp <- dt.n[,.(type,ref,Estimate=paste0(Estimate,p.star(p.value)))]
      colnames(dt.temp)[3] <- y
      dt.f[[i]] <- dt.temp
    }
    
    dt.f <- merge.list(dt.f,c("type","ref"))
    
    dt.f[,type:=factor(type,levels = lm.res$type.level)]
    dt.f[,ref:=factor(ref,levels = lm.res$ref.level)]
    dt.f <- dt.f[order(type,ref)]
    dt.f <- data.table(apply(dt.f,2,change.na))
    
    dt.f[,obs:=1:.N,by=type]
    dt.f[obs!=1,type:=""]
    dt.f <- dt.f[,-c("obs"),with=F]
    return(dt.f)
  }else if(table.type == "rbind"){
    #rbind
    dt.n <- do.call(rbind,lm.res$res)
    dt.n[,row.obs:=1:.N]
    dt.n[,obs.1:=1:.N,by=.(m.formula)]
    dt.n[,obs.2:=1:.N,by=.(m.formula,type)]
    
    formula.n <- dt.n[obs.1== "1" , m.formula]
    t.dt <- tibble(dt.n)
    
    
    for(i in 1:length(formula.n)){
      now.formula <- formula.n[i]
      
      idx <- dt.n[m.formula==now.formula & obs.1==1,row.obs]
      idx <- idx+(i-1)
      dt.n <- tibble::add_row(t.dt$dt.n, 
                              m.formula=now.formula,
                              type="",
                              ref="",
                              Estimate="",
                              `2.5 %` = "",
                              `97.5 %` = "",
                              p.value = "",
                              row.obs = 0,
                              obs.1 = 0,
                              obs.2 = 0,
                              .before = idx)
      t.dt <- tibble(dt.n)
    }
    dt.f <- data.table(t.dt$dt.n)
    dt.f[row.obs != 0,m.formula:=""]
    dt.f[obs.2 != 1,type:=""]
    dt.f <- dt.f[,-c("row.obs","obs.1","obs.2"),with=F]
    return(dt.f)
  }
}



# correlation matrix
corr_matrix <- function(df, col_v){
  # data
  library(gtools)
  res <- list()
  for(i in 1:length(col_v)){
    for(j in 1:length(col_v)){
      
      k <- (i-1)*length(col_v)+j
      var1 <- col_v[i]
      var2 <- col_v[j]
      a <- df[,get(col_v[i])]
      b <- df[,get(col_v[j])]
      data.cor <- data.table(a,b)
      data.cor.c <- data.cor[complete.cases(data.cor)]
      m.cor <- round(cor(data.cor.c$a,data.cor.c$b),2)
      test <- cor.test(a,b)
      p.value <- test$p.value
      stars <- gtools::stars.pval(p.value)
      res[[k]] <- data.table(var1,var2,m.cor,p.value,stars) 
      
    }
  }
  res.f <- do.call(rbind,res)
  
  # plot
  library(ggplot2)
  res.f$var1 <- factor(res.f$var1,levels=col_v)
  res.f$var2 <- factor(res.f$var2,levels=col_v)
  
  p1 <- ggplot(res.f,aes(x=var1,y=var2,fill=m.cor))+geom_tile()+
    scale_fill_gradient2(low = "#2196F3", high = "#F44336", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation")+
    theme(axis.text.x=element_text(angle = -50,hjust=0,size=10,face="bold"),
          axis.text.y=element_text(size=10,face="bold"))+
    theme(panel.background = element_blank(),)+
    ggtitle(paste0("correlation matrix", "(n=",df[,.N],")"))+ylab("")+xlab("")+
    geom_text(aes(y=var2,label=paste0(m.cor,"\n",stars)))+
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p1)
}


bar_plot <- function(dt_p,x,y,t){
  
  p2 <- ggplot(dt_p, aes(x=get(x), y=mean, fill=get(x))) + 
    geom_bar(stat="identity", position=position_dodge()) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                  position=position_dodge(.9))+
    geom_text(aes(label=paste0(mean, "±d")), vjust=1.6, color="black",
              position = position_dodge(0.9), size=5)+
    labs(title = t, fill = x)+ylab(y)+xlab(x)
  return(p2)
}








