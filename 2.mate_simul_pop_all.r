#   this is a simple simulation program written for Andrea Schreier by Tom Famula
#  the basic idea - read in a data set of an animal with a set of genotypes
#   some are diploid, others tetraploid
#   then pick two critters at random, make gametes and mate them
#  just keeptrack of the genotype of the kids

#   this little function will scan a vector with all zeros and one 1
#  and report back the position of the 1
find.all <- function(rr) {     # begin the function
 nl <- length(rr)
 pos <- 0
 for(i in 1:nl){
  if(rr[i] == 1)pos <- i
}  # end of if
pos
}  # end of function

parents <- read.csv(file="output2.csv",header=F)
nr <- length(parents[,1])
#   format for parents - id followed by microsatellite genotypes  

#   USER CHANGES
nmate <- 50   #   number of matings to perform
nkid <- 15  #   number of kids to make per set of parents
nanim <- 66 #   number of animals in parent data set
ndip <- 1    # number of diploid loci
ntet <- 5     # number of tetraploid loci 
noct <- 8    #  number of octoploid loci

#   Before we start building genotypes for progeny, we need to find the "NA"
#   in the parents and randomly fill in that missing allele with one already
#   present
for (i in 1:nr){        #   loop on number of animals in parent
#    fix the diploids
kk <- 0
for (kk in 1:ndip) {    #  loop on diploids
k1 <- kk*2   #   first col of locus
k2 <- k1 + 1   #  second col of locus
tm <- cbind(parents[,k1:k2]) #  matrix of parent columns
ttm <- as.matrix(tm)
all.vec <- as.vector(ttm)
all.frq <- table(all.vec)/sum(table(all.vec)) # pop allele freq
all.nms <- as.numeric(names(all.frq)) # allele names in pop at this locus
r1 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r2 <- rmultinom(1,1,all.frq)  #  randomly selected allele
if(is.na(parents[i,k1])) parents[i,k1] <- all.nms[find.all(r1)]
if(is.na(parents[i,k2])) parents[i,k2] <- all.nms[find.all(r2)]
}     # end of diploid loop

#    fix the tetraploids
kk <- 0
for (kk in 1:ntet) {    #  loop on tetraploids
k1 <- kk*4 + (ndip*2)-2 
k2 <- k1 + 1   #   these are the four allele positions
k3 <- k1 + 2
k4 <- k1 + 3
tm <- cbind(parents[,k1:k4]) #  matrix of parent columns
ttm <- as.matrix(tm)
all.vec <- as.vector(ttm)
all.frq <- table(all.vec)/sum(table(all.vec)) # pop allele freq
all.nms <- as.numeric(names(all.frq)) # allele names in pop at this locus
r1 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r2 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r3 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r4 <- rmultinom(1,1,all.frq)  #  randomly selected allele
if(is.na(parents[i,k1])) parents[i,k1] <- all.nms[find.all(r1)]
if(is.na(parents[i,k2])) parents[i,k2] <- all.nms[find.all(r2)]
if(is.na(parents[i,k3])) parents[i,k3] <- all.nms[find.all(r3)]
if(is.na(parents[i,k4])) parents[i,k4] <- all.nms[find.all(r4)]
}     # end of tetraploid loop

#    fix the octoploids
kk <- 0
for (kk in 1:noct) {    #  loop on octoploids
k1 <- kk*8 + (ntet*4)-4 + (ndip*2)-2 
k2 <- k1 + 1   #   these are the 8 allele locators
k3 <- k1 + 2
k4 <- k1 + 3
k5 <- k1 + 4
k6 <- k1 + 5
k7 <- k1 + 6
k8 <- k1 + 7
tm <- cbind(parents[,k1:k8]) #  matrix of parent columns
ttm <- as.matrix(tm)
all.vec <- as.vector(ttm)
all.frq <- table(all.vec)/sum(table(all.vec)) # pop allele freq
all.nms <- as.numeric(names(all.frq)) # allele names in pop at this locus
r1 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r2 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r3 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r4 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r5 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r6 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r7 <- rmultinom(1,1,all.frq)  #  randomly selected allele
r8 <- rmultinom(1,1,all.frq)  #  randomly selected allele
if(is.na(parents[i,k1])) parents[i,k1] <- all.nms[find.all(r1)]
if(is.na(parents[i,k2])) parents[i,k2] <- all.nms[find.all(r2)]
if(is.na(parents[i,k3])) parents[i,k3] <- all.nms[find.all(r3)]
if(is.na(parents[i,k4])) parents[i,k4] <- all.nms[find.all(r4)]
if(is.na(parents[i,k5])) parents[i,k5] <- all.nms[find.all(r5)]
if(is.na(parents[i,k6])) parents[i,k6] <- all.nms[find.all(r6)]
if(is.na(parents[i,k7])) parents[i,k7] <- all.nms[find.all(r7)]
if(is.na(parents[i,k8])) parents[i,k8] <- all.nms[find.all(r8)]
}     # end of octoploid loop

}             #  end of loop i for animal id


totkid <- nmate*nkid
ncnt <- 0       #   counter for total kids being made
totcol <- ndip*2 + ntet*4 + 8*noct + 3
kids <- matrix(nrow=totkid,ncol=totcol)
#   loop for selection of parents                           
i <- 0
for (i in 1:nmate){
  r1 <- runif(1,min=1,max=nanim+1)
  r2 <- runif(1,min=1,max=nanim+1)
  mate1 <- trunc(r1)
  mate2 <- trunc(r2)
#   now, we have two mates, mate1 & mate2

#   eliminate selfing
#  while(mate1 != mate2){               #  begin while restriction
#   get genotypes for parents
  colcnt <- 2*ndip+1
 dip1 <- parents[mate1,2:colcnt]
 dip2 <- parents[mate2,2:colcnt]
  colcnt1 <- 2*ndip+2
  colcnt2 <- 2*ndip+4*ntet+1
 tet1 <- parents[mate1,colcnt1:colcnt2]
 tet2 <- parents[mate2,colcnt1:colcnt2]
  colcnt3 <- 2*ndip+4*ntet+2
  colcnt4 <- 2*ndip+4*ntet+8*noct+1
 oct1 <- parents[mate1,colcnt3:colcnt4]
 oct2 <- parents[mate2,colcnt3:colcnt4] 
#     now we can loop on kids
j <- 0
for (j in 1:nkid) {
ncnt <- ncnt + 1
#   the diploids
dx <- runif(ndip,min=1,max=3)
d1 <- trunc(dx)
dy <- runif(ndip,min=1,max=3)
d2 <- trunc(dy)
# the tetraploids
tx <- runif(ntet*2,min=1,max=5)    
t1 <- trunc(tx)
ty <- runif(ntet*2,min=1,max=5)
t2 <- trunc(ty)
# the octoploids
ox <- runif(noct*4,min=1,max=9)
o1 <- trunc(ox)
oy <- runif(noct*4,min=1,max=9)
o2 <- trunc(oy)

kids[ncnt,1] <- ncnt
kids[ncnt,2] <- mate1
kids[ncnt,3] <- mate2
#  loop for the diploids                                          

   kk <- 0
   for (kk in 1:ndip) {
     k1 <-  kk*2+2
     k2 <-  kk*2+3
     p1 <-  kk*2+d1[kk]-1
     p2 <-  kk*2+d2[kk]-1
    kids[ncnt,k1] <- parents[mate1,p1]
    kids[ncnt,k2] <- parents[mate2,p2]
     }    # end of the kk loop for diploid loci


#  loop for the tetraploids

   kk <- 0
   for (kk in 1:ntet) {
     k1 <-  ((ndip*2)-1)+kk*4+1
     k2 <-  ((ndip*2)-1)+kk*4+2
     k3 <-  ((ndip*2)-1)+kk*4+3
     k4 <-  ((ndip*2)-1)+kk*4+4
     c1 <- kk*2 - 1
     c2 <- kk*2
     p1 <-  ((ndip*2)-2)+kk*4+t1[c1]-1
     p2 <-  ((ndip*2)-2)+kk*4+t1[c2]-1
     p3 <-  ((ndip*2)-2)+kk*4+t2[c1]-1
     p4 <-  ((ndip*2)-2)+kk*4+t2[c2]-1
    kids[ncnt,k1] <- parents[mate1,p1]
    kids[ncnt,k2] <- parents[mate1,p2]
    kids[ncnt,k3] <- parents[mate2,p3]
    kids[ncnt,k4] <- parents[mate2,p4]    
     }    # end of the kk loop for tetraploid loci


#  loop for the octoploids

   kk <- 0
   for (kk in 1:noct) {
     k1 <-  ((ndip*2)+(ntet*4)-5)+kk*8+1
     k2 <-  ((ndip*2)+(ntet*4)-5)+kk*8+2
     k3 <-  ((ndip*2)+(ntet*4)-5)+kk*8+3
     k4 <-  ((ndip*2)+(ntet*4)-5)+kk*8+4
     k5 <-  ((ndip*2)+(ntet*4)-5)+kk*8+5
     k6 <-  ((ndip*2)+(ntet*4)-5)+kk*8+6
     k7 <-  ((ndip*2)+(ntet*4)-5)+kk*8+7
     k8 <-  ((ndip*2)+(ntet*4)-5)+kk*8+8     
          
     c1 <- kk*4 - 3
     c2 <- kk*4 - 2
     c3 <- kk*4 - 1
     c4 <- kk*4
     
     p1 <-  ((ndip*2)+(ntet*4)-6)+kk*8+o1[c1]-1
     p2 <-  ((ndip*2)+(ntet*4)-6)+kk*8+o1[c2]-1
     p3 <-  ((ndip*2)+(ntet*4)-6)+kk*8+o1[c3]-1
     p4 <-  ((ndip*2)+(ntet*4)-6)+kk*8+o1[c4]-1
     p5 <-  ((ndip*2)+(ntet*4)-6)+kk*8+o2[c1]-1
     p6 <-  ((ndip*2)+(ntet*4)-6)+kk*8+o2[c2]-1
     p7 <-  ((ndip*2)+(ntet*4)-6)+kk*8+o2[c3]-1
     p8 <-  ((ndip*2)+(ntet*4)-6)+kk*8+o2[c4]-1 
         
    kids[ncnt,k1] <- parents[mate1,p1]
    kids[ncnt,k2] <- parents[mate1,p2]
    kids[ncnt,k3] <- parents[mate1,p3]
    kids[ncnt,k4] <- parents[mate1,p4]   
    kids[ncnt,k5] <- parents[mate2,p5]
    kids[ncnt,k6] <- parents[mate2,p6]
    kids[ncnt,k7] <- parents[mate2,p7]
    kids[ncnt,k8] <- parents[mate2,p8]  
     }    # end of the kk loop for octoploid loci


}    # end of loop j for the kids

#}     #    end of while restriction on selfing

}    # endof the i loop for number of matings
#    write out the kids file
write.csv(kids, file = "try1.csv",append=T)
