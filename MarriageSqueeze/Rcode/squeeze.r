########################################################################
## Mon Jul  1 09:49:40 PDT 2013
##Socsim example: The Marriage Squeeze
##After running the simulation with squeeze.sup this code will examine
##the results.  The interesting part of the simulation corresponds to
##the years 1950-2028. The mortality and fertility rates come from the
##Human Mortality Database and the Human Fertility Database
##respectively. 2008-2028 use 2008 rates.  During the interesting
##period, fertility declined substantially. If Demography 101 is
##correct, there should be evidence of a "marriage squeeze" operating
##to the detriment of males
########################################################################
## read the simulation results into R. ReadSocOut.r contains a
## function for doing this
code_dir <- "MarriageSqueeze/Rcode/"
results_dir <- "MarriageSqueeze/SimResults/"
source(paste0(code_dir,"ReadSocOut.r"))
## stem corresponds to the "output_file" directive in the .sup file

temp <- ReadSocOut(stem=paste0(results_dir, "squeeze"))

## the .omar and .opop files are now data.frames.
opop <- temp$opop
## the .opop file contains
## one row for each simulated person who every "lived". It generally
## includes many who "died" long ago.

names(opop)
## Here are what the columns mean
# pid - id number
# fem - sex indicator
# group - group indicator ?  - not used
# nev - (time of? age of?) next scheduled event 
# dob - date of birth
# mom - person id of mother
# pop - person id of father
# nesibm - id of next oldest sibling through mother
# nesibp - id of next oldest sibling through father
# lborn - id of last born child.
# marid - id of marriage in omar file.
# mstat - marriage status 1=single, 2=divorced, 3=widowed, 4=married.
# dod - date of death or 0 if still alive.
# fmult - fertility multiplier - beta distributed (?) random fertility multiplier to allow for
#         heterogenity. (ranges between 0 ans 2.4ish) 

########################################################################
## # # # Constants for converting socsim months to earth time
########################################################################

## final actuall calendar year for end of simulation
FinalSimYear<-2028  ## check .sup file Slovakia use 2008 rates for 20years
endmo<-max(c(opop$dob,opop$dod))     ##3914 ##  # last month of simulation
simDuration<- (59+20)*12 ## period of fertility decline

FirstMo<-endmo- simDuration +1   
EndYr<-endmo:(endmo-11)
FirstYr<-FirstMo:(FirstMo+11) 

########################################################################
## Useful functions and constants
########################################################################

asYr<-function(x,lastmo=endmo){
  ## convert sim month to a real calendar year
  ## handy for graphing mainly.
  ## requires that FinalSimYear be set in the GlobalEnv
  stopifnot("FinalSimYear" %in% objects(pos=1))
  yr<-ifelse(x==lastmo,FinalSimYear,
             FinalSimYear - trunc((lastmo -x )/12))
  return(yr)
}
########################################################################
## returns NA if x is zero -- useful for maintaining the corespondence
## between pid and row number in the opop file.
zna<-function(x){return(ifelse(x==0,NA,x))}

########################################################################
dec<-function(year){
  ## returns simulation month equiv of dec of the given real calendar year
  ## requires that FinalSimYear be set in the GlobalEnv
  stopifnot("FinalSimYear" %in% objects(pos=1))
  return( endmo - (FinalSimYear - year)*12)
}
########################################################################


omar<-temp$omar
names(omar)
# mid - marriage id
# wpid - wife id
# hpid - husband id
# dstart - start date
# dend  - end date
# rend  - reason for end 2=divorce 3=death of one partner
# wprior - wife prior marriage id
# hprior - husband prior marriage id.
########################################################################
## Add some useful columns to opop and omar
########################################################################
## get spouse ages  at marriage and add them to the *marriage* record
omar$hage<-omar$dstart - opop[paste(omar$hpid),"dob"]
omar$wage<-omar$dstart - opop[paste(omar$wpid),"dob"]
omar$agediff<- omar$hage - omar$wage   ## in months

opop$birthYr<-asYr(opop$dob)

## get first marriage id -- socsim stores marriage ids as linked list
## headed by most *recent* marriage.  If a person is married more than
## once, we need to follow the linked list back to the first marriage.

## The (h/w)prior field stores the id of each spouses prior marriage

## initialize a colum of opop to hold the id of the first marriage
## it will remain NA for never marrieds
opop$fmid<-NA
## select a subset of marriages which are first for at least one partner
fomar<-omar[omar$hprior == 0 | omar$wprior == 0,]


## use match() to lookup the marriage id of each person (in opop)'s
## first marriage id
opop[opop$fem==0,"fmid"]<-
  fomar[match(opop[opop$fem==0,"pid"],fomar$hpid),"mid"]
opop[opop$fem==1,"fmid"]<-
  fomar[match(opop[opop$fem==1,"pid"],fomar$wpid),"mid"]


########################################################################
## draw some pictures and perhaps a conclusion or two
########################################################################

## count marriages by year
Yrange<-1940:2028  ## range of years to investigate

## marriages by year of marriage 'sapply' returns a vector (if
##possible) of the same length as 'X', each element of which is the
##result of applying 'FUN' to the corresponding element of 'X'. 

## for each year in Yrange
marriageCount<-sapply(Yrange,
                      FUN=function(x){
                        sum(asYr(omar$dstart) == x)}
                      )

## births by year
birthCount<-sapply(Yrange,
                   function(x){
                     sum(opop$birthYr == x)}
                   )

## population living (in december of year)
liveCount<- sapply(Yrange,function(x){
  sum(opop$dob <= dec(x) &
      (opop$dod >= dec(x) | opop$dod ==0))
})

Pop18.40<- sapply(Yrange,function(x){
  ## alive and date of birth in proper range
  sum(opop$dob <= (dec(x) - 18*12) & opop$dob > (dec(x) - 40*12) &
      (opop$dod >= dec(x) | opop$dod ==0))
})


## population and birth cohort sizes
plot(liveCount~Yrange,main="Population and birth cohort sizes",
          type='l',ylab="Population")
par(new=T)
plot(I(birthCount)~I(Yrange),col='blue',axes=F,type='l',
          ylab='',xlab='')
axis(side=4,col.axis='blue')
mtext(side=4,col='blue',line=-1,text="Births")





## nuptiality rate and cohort size
par(mfrow=c(1,1))
plot(I(marriageCount/Pop18.40)~I(Yrange),type='l',lwd=2,
     main="Birth cohort size falls...then crude nuptiality rate (18-40yr olds) falls",
     ylab='',xlab='Year')
## Note that the atrisk set is 18-40 year olds but we do include
## marriages to people outside of that age range.  Can we live with this?

mtext(side=2,line=-1,text="Marriages/person 18-40")
par(new=T)
plot(I(birthCount)~I(Yrange),col='blue',axes=F,type='l',
     ylab='',xlab='')
axis(side=4,col.axis='blue')
mtext(side=4,col='blue',line=-1,text="Births")
      
########################################################################
##NOTE that fertility decline begins in 1950 BUT COHORT SIZE does not
##drop unil 1980 or so and the "crude marriage rate" dose not drop
##until about yesterday.  Note also that birth cohort sizes are in the
##hundreds.
########################################################################


##
## distribution of age at first marriage for two birth cohorts
## collect marriage ids for two intersting birth cohorts
midGrowing<-opop[opop$fem==0 & opop$birthYr == 1940,"fmid"]
midShrinking<-opop[opop$fem==0 & opop$birthYr == 1990,"fmid"]
## remove those who never marry
midGrowing<-midGrowing[!is.na(midGrowing)]
midShrinking<-midShrinking[!is.na(midShrinking)]

par(mfrow=c(1,1))
plot(density(trunc(omar$hage[midShrinking])/12),col='red',
     main="age at first marriage men born 1940 and 1990")
lines(density(trunc(omar$hage[midGrowing])/12))


## distribution of age difference at marriage
summary(omar$agediff)
plot(density(omar$agediff/12),main="Groom age - bride age",lwd=2)
lines(density(omar$agediff[midGrowing]/12),col='blue')
lines(density(omar$agediff[midShrinking]/12),col='green')
legend(x='topright',
       fill=c('black','blue','green'),
       bty='n',
       legend=c("All","1940","1990"))
## Note that we are only looking here at those who marry.

## Kaplan - Meier plots of marriage
library(survival)

########################################################################
##All survival analysis functions require a special dependent variable
##which includes both duration and whether an observation ends with
##an event or is censored.  We're going to stratify by birth cohort so
##our dependent variable will be based solely on age.  Start time will
##be zero, end time will be the minimum of: age at first marriage,
##age at death or age at end of simulation.

opop$Kdep<-with(opop,
                Surv(time=0*pid,
                    time2 = ifelse(is.na(fmid),
                      ## never married so age at death is the end time
                      ifelse(dod ==0,    
                             endmo-dob+.0001,  # end of sim + to avoid zero
                                        #length durations
                             dod-dob+.0001),   ## age at death
                      ## age at marriage for those who married
                      ifelse(fem==0, 
                             omar$hage[fmid],
                             omar$wage[fmid])),
                     ## censored (0) unless fmid is known
                     event= 1*(! is.na(fmid))))


## survfit calculates survival curves based on the formula given
## Let's look at survival curves where survival means NOT getting married.
##
km.resM.4080<-survfit(opop$Kdep ~opop$birthYr,
                subset=opop$birthYr %in% c(1940,1980) & opop$fem ==0)

km.resF.4080<-survfit(opop$Kdep ~opop$birthYr,
                subset=opop$birthYr %in% c(1940,1980) & opop$fem ==1)

## Comparing 1940 and 1980
par(mfrow=c(2,1))
colors<-c("purple","black")
plot(km.resM.4080,xlim=c(18,30)*12,col=colors,
     main="Males 1940 & 80")
legend(x="topright",fill=colors,legend=c("1940","1980"))
plot(km.resF.4080,xlim=c(18,30)*12,col=colors,
     main="Females")

## Now focus on sex difference
km.res40<-survfit(opop$Kdep ~opop$fem,
                subset=opop$birthYr %in% c(1940))

km.res80<-survfit(opop$Kdep ~opop$fem,
                subset=opop$birthYr %in% c(1980))

par(mfrow=c(2,1))
colors<-c("red","blue")

par(mfrow=c(2,1))
plot(km.res40,col=c('red','blue'),     xlim=c(18,40)*12,
     main="'Survival' as unmarried 1940 birth cohort")
legend(x="topright",fill=colors,legend=c("male","female"))
plot(km.res80,col=c('red','blue'),xlim=c(18,40)*12,
     main="1980")


###
### median age at first marriage
###



medianAgeMales<- sapply(Yrange,
         function(x){
           cohort<-opop[opop$birthYr ==x & opop$fem==0,]
           ##ages<-omar[ paste(cohort$fmid),"hage" ] safer
           ages<-omar[ zna(cohort$fmid),"hage" ] ## faster
           return(sort(ages,na.last=TRUE)[trunc(length(ages)/2)])
         }
         )

medianAgeFemales<- sapply(Yrange,
         function(x){
           cohort<-opop[opop$birthYr ==x & opop$fem==1,]
           ages<-omar[ zna(cohort$fmid),"wage" ] ## faster
           ## median ages with NAs as high ages
           return(sort(ages,na.last=TRUE)[trunc(length(ages)/2)])
         }
         )


par(mfrow=c(1,1))
plot(range(c(medianAgeMales,medianAgeFemales),na.rm=T)~range(Yrange),type='n',
     xlab="Years",ylab='Median Age at first Marriage')
lines(medianAgeMales ~ Yrange,type='l',col='red')
lines(supsmu(y=medianAgeMales,x= Yrange),lty=2,col='red')
lines(medianAgeFemales ~ Yrange,type='l',col='blue')
lines(supsmu(y=medianAgeFemales,x= Yrange),lty=2,col='blue')

###
#### plot median age difference by calendar year
medianAgeDiff<- sapply(Yrange,
         function(x){
           ages<-fomar[asYr(fomar$dstart)==x,"agediff" ] 

           return(sort(ages,na.last=TRUE)[trunc(length(ages)/2)])
         }
         )

plot(medianAgeDiff ~ Yrange,type='l',
     main="Median age difference in first marriages by year of marriage",
     xlab='year',ylab='husband age - wife age in months')



########################################################################
##Extensions these are only suggestions.  You are welcome == even
## encouraged to pursue other better ideas.
########################################################################

##(0) Socsim can run two different kinds of marriage markets. The
##"traditional" two queue system and the "new" one queue scheme.  The
##legacy two queue system initiates marriage searches according to the
##specified nuptiality rates but if appropraite spouses are not found
##then "suitors" wind up in the "marriage queue" where they wait to be
##selected by other suitors.  Under the newer, "one queue" system,
##males are always waiting and only the femal nuptiality rates
##determin when a marriage occurs. In this scheme, a women who has
##marriage even chooses the "best" available spouse from among all of
##the unmarried males in the simulation.  Under both systems age and
##group are the characteristics that determin the desirability of a
##potential spouse.

## Don't hesitate to read Socsim Oversimplified to learn more about
## the different marriage market schemes


## (1) Experiment with the directives in the marriageRates file. This
## file allows you to switch between the marriage_queue and
## marriage_evaluation schemes and also to change the marriage
## rates. Remember - the point is that we want to keep the marriage
## preferences the same across time, and see what happens as a result
## of changes in the demography.

## (2) Under the one queue scheme, it is easy to raise the
## agedif_marriage_mean.

## (3) Experiment with remarriage after divorce and widowhood.

## (4) Compare the Slovakian case to one or more of the other countries
## for which data are readily available -- all you need to do is to
## edit squeeze.sup. Just change all occurances of "Slovakia" to "USA"
## or one of the other countries with a directory in HFDdata and
## HMDdata.  Of course you may also wish to graph different years.

##(5) Devise and construct a better measure of the marriage patterns
##    over time. Consider perhaps an estimate of the average duration
##    of marriage search for cohorts at various ages and sexes.  In
##    other words look at the difference between the rates of marriage
##    search initiation (in the marriageRates file) and the observed
##    ages at marriage. This only makes sense for the legacy two queue system.


