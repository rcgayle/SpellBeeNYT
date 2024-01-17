

beeselectp<-function(L,m){
     RL<-c()
     for (i in 1:length(L)){
          if (is.element(m, unlist(strsplit(L[i],''))))
                {
                 RL<-c(RL, L[i])
                 }   
          else {}
                          }
     return(RL)
                             }                            
                            
                            
beeselect<-function(L, l, m){
     RL<-beeselectp(L,m)
     KEEP<-c()
     if (length(RL)==0){}
     else{                                         
     for (j in 1:length(RL))
                             {
           TW<-unlist(strsplit(RL[j],''))
           k<-1
           V<-TRUE
           while (V==TRUE & k<=length(TW))
                {
                 if (!is.element(TW[k], c(l,m)))
                        {V<-FALSE}
                 else 
                        {k<-k+1}
                }
           if (V==TRUE)
                {KEEP<-c(KEEP, RL[j])}
           else {}
                              }
           }
      return(sort(KEEP))
                            }
                            
beeselectB<-function(RL, I){
     KEEP<-c()                                         
     for (j in 1:length(RL))
                             {
           TW<-setdiff(unique(unlist(strsplit(RL[j],''))),letters[I[1]])
           k<-1
           V<-TRUE
           while (V==TRUE & k<=length(TW))
                {
                 if (!is.element(TW[k], letters[I[2:7]]))
                        {V<-FALSE}
                 else 
                        {k<-k+1}
                }
           if (V==TRUE)
                {KEEP<-c(KEEP, RL[j])}
           else {}
                              }
      return(sort(KEEP))
                            }
                            
                            
                            
wordindex<-function(L,w){
            I<-c()
            for (i in 1:length(L))
                 {
                  if (L[i]==w)
                        {I<-c(I,i)}
                  else {}
                  }
            return(I)
                       }
                       
addwords<-function(L, NL){
      return(sort(unique(c(L, NL))))
                         }
        
        
      

beeselectp<-function(L,m){
     RL<-c()
     for (i in 1:length(L)){
          if (is.element(m, unlist(strsplit(L[i],''))))
                {
                 RL<-c(RL, L[i])
                 }   
          else {}
                          }
     return(RL)
                             }                            
                            
                            
beeselect<-function(L, l, m){
     RL<-beeselectp(L,m)
     KEEP<-c()
     if (length(RL)==0){}
     else{                                         
     for (j in 1:length(RL))
                             {
           TW<-unlist(strsplit(RL[j],''))
           k<-1
           V<-TRUE
           while (V==TRUE & k<=length(TW))
                {
                 if (!is.element(TW[k], c(l,m)))
                        {V<-FALSE}
                 else 
                        {k<-k+1}
                }
           if (V==TRUE)
                {KEEP<-c(KEEP, RL[j])}
           else {}
                              }
           }
      return(KEEP)
                            }
                            
                            
                            
                            
wordindex<-function(L,w){
            I<-c()
            for (i in 1:length(L))
                 {
                  if (L[i]==w)
                        {I<-c(I,i)}
                  else {}
                  }
            return(I)
                       }
                       
addwords<-function(L, NL){
      return(sort(unique(c(L, NL))))
                         }
        
        
      