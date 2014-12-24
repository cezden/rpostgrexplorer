### (experimental) LDA on attributes
create.text.representation=function(attributesList){
  tablesTaken=unique(attributesList$tablename)
  attid=paste(attributesList$attname,".",attributesList$typname,sep="")
  att.map=by(attributesList$tablename,attid,function(x){as.character(unique(x))})
  table.map=by(attid,attributesList$tablename,function(x){as.character(unique(x))})
  att.counts=lapply(att.map,length)
  #att.matrix=matrix(0,ncol=length(att.map),nrow=length(tablesTaken))
  
  table.map.v=lapply(table.map,function(x){
    paste(x,collapse=" ")
  })
  
  docs <- data.frame(docs = unlist(table.map.v))
  ds <- DataframeSource(docs)
  cds=Corpus(ds)
  dtm <- DocumentTermMatrix(cds, control = list(weighting =
                                                  function(x)
                                                    weightTfIdf(x, normalize =FALSE)))
  dtmtf <- DocumentTermMatrix(cds, control = list(weighting =
                                                    function(x)
                                                      weightTf(x)))
  
  list(dtm.tfidf=dtm, dtm.tf=dtmtf)  
}

#### creates the index of attributes based on the result of get.attributes.query
create.attribute.index=function(attributesList){
  #filter-out non-informative attributes
  statDetermined=!is.na(attributesList[,"null_frac"])
  allNull=!(attributesList[,"null_frac"]<1.0)
  someNull=(attributesList[,"null_frac"]>0.0)
  allSame=(attributesList[,"n_distinct"]==1)
  attributeBad=((allSame & !someNull) | allNull)
  attributeOk=(!statDetermined | !attributeBad)
  atttaken=attributesList[attributeOk,]
  tablesTaken=unique(atttaken$tablename)
  attid=paste(atttaken$attname,".",atttaken$typname,sep="")
  att.map=by(atttaken$tablename,attid,function(x){as.character(unique(x))})
  att.counts=unlist(lapply(att.map,length))
  textRep=create.text.representation(atttaken)
  return (list(att.map=att.map,att.counts=att.counts,attid=attid,dtm=textRep$dtm.tfidf,dtm.tf=textRep$dtm.tf))
}




infer.relation=function(colName,baseTable,relTables,schemaname=NA){
  relTables.enf=setdiff(unlist(relTables),baseTable)
  
  queryList=list()
  #queryList[baseTable]=get.groupping.att.query(colName,baseTable,schemaname)
  for(tabname in relTables.enf){
    #queryList[tabname]=get.groupping.att.query(colName,tabname,schemaname)
    queryList[tabname]=sql.entity.relation(colName,baseTable,tabname,schemaname)
  }
  mmm=query.load.execute(queryList,control.connection)
  resultRel=NULL
  for(tabname in relTables.enf){
    z=data.frame(f_tablename=baseTable,s_tablename=tabname,groupping_col=colName,mmm[[tabname]],stringsAsFactors=FALSE)    
    resultRel=rbind(resultRel,z)
  }
  resultRel[order(resultRel$distentities_match_frac,decreasing=TRUE),]
}


demo=function(){
  
  
  control.connection <- list(user="dbuser", host="dbhost", port=0000, dbname="dbname", password="dbpass")  
  
  
  schemaname="dbschema"
  
  vv.data=load.metadata(schemaname=schemaname, control.connection=control.connection)
  
  #DF describing tables (eg. estimated counts)
  tablesList=vv.data$tablesList
  head(tablesList)
  
  #DF describing attributes (and some basic statistics, if present)
  attributesList=vv.data$attributesList
  head(attributesList[attributesList$tablename=="sometablename",])
  head(attributesList)
  
  att.idx=create.attribute.index(attributesList)
  
  #non-unique attribute names (sorted by #of tables)
  sort(att.idx$att.counts[att.idx$att.counts>1])
  
  #all tables containing attributename with type int8
  att.idx$att.map[["attributename.int8"]]
  
  
  # listing the columns matching the query
  att.idx$attid[grep("[:alnum:]*chocolate[:alnum:]*",att.idx$attid,ignore.case = TRUE)]
  
  sort(att.idx$attid[grep("[:alnum:]*amount[:alnum:]*",att.idx$attid,ignore.case = TRUE)])
  
  attributesList[grep("[:alnum:]*amount[:alnum:]*",attributesList$attname,ignore.case = TRUE),]
  
  attributesList[grep("[:alnum:]*term[:alnum:]*",attributesList$attname,ignore.case = TRUE),]
  
  
  #the relation discovery
  somePKFKattribute.rel=infer.relation("somePKFKattribute","dbschema.PKtable",head(paste("dbschema",att.idx$att.map[["somePKFKattribute.int8"]],sep=".")))
  somePKFKattribute.rel  
  
  
  
  # 32 most frequent attributes (sorted by TF-IDF)
  
  zz=inspect(att.idx$dtm)
  ttt=apply(zz,2,sum)
  ttt2=sort(ttt,decreasing=TRUE)
  ttt2[1:32]
  
  # attributes correlated with attributename.int8
  assocList=findAssocs(att.idx$dtm,"attributename.int8",0.0)
  assocList
  
  # attributes correlated with attributename.int8 having the type timestamp
  assocList[grep("[:alnum:]*timestamp[:alnum:]*",names(assocList),ignore.case = TRUE)]
  
  
  
  # 32 most informative(?) tables (sorted by TF-IDF) (w.r.t. columns, not the number of rows...)
  ttt=apply(zz,1,sum)
  ttt2=sort(ttt,decreasing=TRUE)
  ttt2[1:32]
  
  
}

get.all.columns.matching=function(substring){
  subsel.colnames.type=unique(att.idx$attid[grep(paste("[:alnum:]*",substring,"[:alnum:]*",sep=""),att.idx$attid,ignore.case = TRUE)])
  all.colnames.type=paste(attributesList$attname,".",attributesList$typname,sep="")
  attributesList.subsel=attributesList[which(all.colnames.type %in% subsel.colnames.type),]
  attributesList.subsel=attributesList.subsel[order(attributesList.subsel$tablename),]
  attributesList.subsel
}

demo2<- function(){

get.all.columns.matching("chocolate")

findAssocs(att.idx$dtm,"chocoholic.int8",0.0)
### exact intersection
intersect(unique(get.all.columns.matching("chocoholic")$tablename), unique(get.all.columns.matching("coffeeholic")$tablename))
### potential matchings
collist1=attributesList[attributesList$tablename %in% unique(get.all.columns.matching("chocoholic")$tablename),]
collist2=attributesList[attributesList$tablename %in% unique(get.all.columns.matching("coffeeholic")$tablename),]
representcol=function(attributesList.subsel){
  paste(attributesList.subsel$tablename,".",attributesList.subsel$attname,".",attributesList.subsel$typname,sep="")
}
intersect(representcol(collist1),representcol(collist2))
representcol(collist1)

t1="(select emp.empoholicid from dbschema.empoholic emp
join prospectousemopholic vemp on (emp.prospectousemopholicid=vemp.prospectousemopholicid))"

t2.pre=setdiff(att.idx$att.map[["empoholicid.int8"]],"empoholic")

t2=paste("(select emp.empoholicid from dbschema.empoholic emp
         join prospectousemopholic vemp on (emp.prospectousemopholicid=vemp.prospectousemopholicid) join dbschema.",t2.pre," ppp on (emp.empoholicid=ppp.empoholicid))",sep="")

t2=paste("(select e.empoholicid 
         from dbschema.",t2.pre," e, dbschema.empoholic f, prospectousemopholic vemp
         where e.empoholicid=f.empoholicid and f.prospectousemopholicid=vemp.prospectousemopholicid)",sep="")


empoholicid.rel3=infer.relation("empoholicid",t1,t2,schemaname=NA)
empoholicid.rel3b=empoholicid.rel3
empoholicid.rel3b$f_tablename="empoholic"
empoholicid.rel3b$s_tablename=t2.pre

empoholicid.rel2=infer.relation("prospectousemopholicid","prospectousempoholic",att.idx$att.map[["prospectousemopholicid.int8"]],schemaname=schemaname)


intersect(att.idx$att.map[["empoholicid.int8"]],att.idx$att.map[["prospectousemopholicid.int8"]])      

att.idx$attid[grep("[:alnum:]*cookie[:alnum:]*",att.idx$attid,ignore.case = TRUE)]
att.idx$att.map[["cookie.int8"]]

att.idx$dtm

unique(att.idx$attid[grep("[:alnum:]*cleverid[:alnum:]*",att.idx$attid,ignore.case = TRUE)])
att.idx$att.map[["cleverid.int8"]]



zz=LDA(att.idx$dtm.tf,control = list(alpha = 0.01),k=10)
mean(zz@loglikelihood)
str(zz)
qq=zz@wordassignments
str(qq)
mm=topics(zz,k=3)
mm
mm=topics(zz,k=1)
str(mm)
by(data=names(mm),INDICES=mm,FUN=function(x) as.character(x))
terms(zz,k=30)
nn=posterior(zz)
str(nn)
sort(apply(nn$terms,2,sum))
sort(nn$terms[8,]/sum(nn$terms[8,]))
perplexity(zz)
cbind(nn$topics,zz@loglikelihood)

str(att.idx$att.map)
attributesList[attributesList$tablename=="sometable",]$attname
vv=distHellinger(nn$topics)
rownames(vv)=rownames(nn$topics)
colnames(vv)=rownames(nn$topics)
dist
any(is.na(vv))
str(vv)
heatmap(vv)
vv[1,]
ccl=hclust(d=as.dist(vv))
plot(ccl)
mm=cutree(ccl,k=10)
by(data=names(mm),INDICES=mm,FUN=function(x) as.character(x))











}
