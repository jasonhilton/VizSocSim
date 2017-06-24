########################################################################
##Wed May 26 12:44:18 PDT 2010
## imports data from UN dvd and writes in happy socsim read format
## fertility and mortality rate files for whatever countries.
########################################################################

setwd("../UNdata")
########################################################################
## Usefull functions

########################################################################
readlx<-function(file="wpp_lx_male_medium.csv"){
  ## reads lx file from WPP UN forecast
  unpop <- read.csv(file,skip=16,as.is=T)
  
  names(unpop) <-c("Index","var","country","note","ccode",
                   "rdate",paste("U",c(0,1,seq(5,85,by=5)),sep=''))
  
  unpop$country<-ifelse(unpop$country=="United States of America",
                        "USA",unpop$country)

  unpop$country<-ifelse(unpop$country==  "Russian Federation",
                        "Russia",unpop$country)


  unpop$country<-ifelse(unpop$country== "Czech Republic",
                        "CzechRepublic" ,unpop$country)

  rownames(unpop)<-paste(unpop$country,unpop$rdate,sep=':')
  return(unpop)
}

 ########################################################################
readnfx <- function(file="nfx_medium.csv"){
unfert <- read.csv(file,skip=16,as.is=T)

names(unfert) <- c("Index","var","country","note","ccode",
                   "rdate",paste("U",c(seq(19,49,by=5)),sep=''))

unfert$country<-ifelse(unfert$country=="United States of America",
                       "USA",unfert$country)

unfert$country<-ifelse(unfert$country==  "Russian Federation",
                        "Russia",unfert$country)

unfert$country<-ifelse(unfert$country== "Czech Republic",
                        "CzechRepublic" ,unfert$country)

rownames(unfert)<-paste(unfert$country,unfert$rdate,sep=':')
return(unfert)
}

########################################################################
writeFert<-function(Nfx=nfx,
                    Country="USA",
                    Variant="Medium",
                    outdir="../UNdata"){

  options(scipen=9999)
  ## called by likelihood_simul before running a bunch of
  ## socsimulations this will write a legal file of fertility rates
  ## scaleFactor should be about the mean age at childbirth ignoring
  ## mortality we'll do this by letting fertility be dnorm with
  ## scaleFactor being the mean

  ratesets<-grep(x=rownames(Nfx[[Variant]]),pattern=Country,value=T)

  for(rs in ratesets){
    bprob<-unlist(
                  Nfx[[Variant]][rs,paste("U",seq(19,49,by=5),sep='')]
                  )
    Srate<-cbind(seq(19,49,by=5),
                 0,
                 bprob/12000) #rates are per 1000 per year
    Srate<-rbind(c(15,0,0),
                 Srate,
                 c(100,0,0))
    ## make sure directory exists
    out<-paste(outdir,Country,sep='/')
    system(paste("mkdir -p ", out))

    ystring<-sub(x=rs,pattern=Country,replacement='')
    ystring<-sub(x=ystring,pattern=':',replacement='')
    outfile=paste(out,"/Fert",Variant,ystring,sep="")
    cat(file=outfile,
        x="*fertility rates written by writeFert in  import_UN_rates.r\n",
        append=F)

    cat(file=outfile,x=paste('*',"UN forecast Variant:",Variant," year:",rs)
                     ,append=T)
    cat(file=outfile,x="\nbirth 1 F married 0\n",append=T)

    write.table(file=outfile,x=Srate,append=T,
              quote=FALSE,row.names=FALSE,col.names=FALSE) 
    
    ## divorced and widowed will default to single rate so only married
    ## and single need to be specified
  
    ##singleRates<-data.frame(yr=100,mo=0,rate=0)
    
    cat(file=outfile,x="\nbirth 1 F single 0\n",append=T)
    write.table(file=outfile,x=Srate,append=T,
                quote=FALSE,row.names=FALSE,col.names=FALSE) 
    
  }

}
########################################################################

writeMort<-function(Lx=LXmed,
                    Country="USA",
                    outdir="../UNdata"){

  options(scipen=9999)
  ## called by likelihood_simul before running a bunch of
  ## socsimulations this will write a legal file of fertility rates
  ## scaleFactor should be about the mean age at childbirth ignoring
  ## mortality we'll do this by letting fertility be dnorm with
  ## scaleFactor being the mean

  for(Sex in c("Female","Male")){
    ratesets<-grep(x=rownames(Lx[[Sex]]),pattern=Country,value=T)

    for(rs in ratesets){
      lx<-unlist(
                 Lx[[Sex]][rs,paste("U",c(0,1,seq(5,85,by=5)),sep='')]
                 )
      mprob<-lx[-1]/lx[-length(lx)]
      qx<-1-(mprob^c(1/12,1/48,rep(1/60,length(mprob)-2)))
      Srate<-cbind(c(1,seq(5,80,by=5),99),
                   0,
                   qx) #rates per 1000
      Srate<-rbind( Srate,
                   c(100,0,.999))
      ## make sure directory exists
      out<-paste(outdir,Country,sep='/')
      system(paste("mkdir -p ", out))
      ystring<-sub(x=rs,pattern=Country,replacement='')
      ystring<-sub(x=ystring,pattern=':',replacement='')
      outfile=paste(out,"/Mort",ystring,sep="")
      cat(file=outfile,
          x="*mortality rates written by writeMort in  import_UN_rates.r\n",
          append=ifelse(Sex=="Female",FALSE,TRUE))
      
      cat(file=outfile,x=paste('*',"UN projection Medium, year:",rs),
          append=T)
      
      cat(file=outfile,x=paste('*'," ALERT -- these data are full of mistakes\n* do not use for Science.\n* Get the UN projections from UN.org"),
        append=T)
      cat(file=outfile,
          x=ifelse(Sex=="Female",
            "\ndeath 1  F single \n",
            "\ndeath 1  M single \n"),
          append=T)
      
      write.table(file=outfile,x=Srate,append=T,
                  quote=FALSE,row.names=FALSE,col.names=FALSE) 
      
    }
  }
}    
    
########################################################################


LXmed <- list(
Male = readlx(file="wpp_lx_male_medium.csv"),
Female = readlx(file="wpp_lx_female_medium.csv")
)
###
nfx<-list(
Medium = readnfx(file="nfx_medium.csv"),
Low = readnfx(file="nfx_low.csv"),
High = readnfx(file="nfx_high.csv")
          )

sort(unique(LXmed$Male$country))

for(cnt in c("USA","Austria","CzechRepublic","Netherlands","Russia",
             "Slovakia","Sweden" ,"Switzerland")){

  writeMort(Lx=LXmed,Country=cnt)
  writeFert(Nfx=nfx,Country=cnt,Variant="Medium")
  writeFert(Nfx=nfx,Country=cnt,Variant="High")
  writeFert(Nfx=nfx,Country=cnt,Variant="Low")
}
