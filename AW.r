AdWds<-function(NL){
      L<-as.character(read.csv('NW.csv', sep=',')[,2])
      return(write.csv(sort(unique(c(L, NL))), 'NW.csv'))
                         }
                         
RmWds<-function(RL){
      L<-as.character(read.csv('NW.csv', sep=',')[,2])
      return(write.csv(sort(setdiff(L, RL)), 'NW.csv'))
                         }
                         
                         
MassageList<-function(T){}