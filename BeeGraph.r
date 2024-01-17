SLG<-function(){
      x<-c()
      y<-c()
      plot(x, y, xlim=c(-7,7), ylim=c(-5,5), xlab='', ylab='', axes=FALSE,
                asp=1)
      P<-seq(-6,6,1)
      text(0, 4, 'SELECT LETTERS', col='red', font=2, cex=2)
      text(0, 2.75, '(required letter first)', col='red', font=2, cex=1.5)
      for (i in 1:length(P)){
            text(P[i],0.75, LETTERS[i], col='darkblue', font=2, cex=2)
            text(P[i],-2.25, LETTERS[i+13], col='darkblue', font=2, cex=2)
                            }
                  }
                  

        
IPTFUN<-function(i){
        if (i<14){return(c(i-7, 0.75))}
        else {return(c(i-20, -2.25))}
                   }
                   
SDLG<-function(I){
      x<-c()
      y<-c()
      plot(x, y, xlim=c(-7,7), ylim=c(-5,5), xlab='', ylab='', axes=FALSE,
                asp=1)
      P<-seq(-6,6,1)
      text(0, 4, 'SELECT LETTERS', col='red', font=2, cex=2)
      text(0, 2.75, '(required letter first)', col='red', font=2, cex=1.5)
      S<-seq(1,26,1)
      NI<-setdiff(S,I)
      text (IPTFUN(I[1])[1], IPTFUN(I[1])[2], LETTERS[I[1]], col='magenta',
                 font=2, cex=2)
      if (length(I)>1){
        for (i in 2:length(I))
           {
             text (IPTFUN(I[i])[1], IPTFUN(I[i])[2], LETTERS[I[i]],
                      col='green',font=2, cex=2)
            }
                      }
      else {}
      for (k in 1:length(NI))
         {
         text (IPTFUN(NI[k])[1], IPTFUN(NI[k])[2], LETTERS[NI[k]],
                      col='darkblue',font=2, cex=2)
         }
                    }
                  

IFUN<-function(Q){
       P<-seq(-6,6,1)
       for (i in 1:13){
            if (abs(P[i]-Q[[1]])<0.25 & abs(Q[[2]]-0.5)<0.5)
                   {return(i)}
            else if (abs(P[i]-Q[[1]])<0.25 & abs(Q[[2]]+2.5)<0.5)    
                   {return(i+13)}
            else {}
                      }
               
                   }   
                  

                                    
      
      
 
                  
                  
                  
                  
                  
                  