setwd("C:/Users/lihon/Desktop/paper/bias-sigir22/code/Generalized-propensity-score-master")
for (t in 1:10){
  path = paste0("C:/Users/lihon/Desktop/paper/bias-sigir22/code/data/2German/gps_",t,".csv")
  print(path)
  xstream<-read.table(path,header=F,sep=",") 
  theta<-function (x,xstream) 
  {
    stream=xstream[x,]
    ma <- matrix(0.0,20,27)
    do <- matrix(0.0,20,100)
    for (h in 2:21)
    {
      if(h==2){yyy=stream[,2]~stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==3){ yyy=stream[,3]~stream[,2]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==4){ yyy=stream[,4]~stream[,2]+stream[,3]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==5){ yyy=stream[,5]~stream[,2]+stream[,3]+stream[,4]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==6){ yyy=stream[,6]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==7){ yyy=stream[,7]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==8){ yyy=stream[,8]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==9){ yyy=stream[,9]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==10){ yyy=stream[,10]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==11){ yyy=stream[,11]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==12){ yyy=stream[,12]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==13){ yyy=stream[,13]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==14){ yyy=stream[,14]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==15){ yyy=stream[,15]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==16){ yyy=stream[,16]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,17]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==17){ yyy=stream[,17]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,18]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==18){ yyy=stream[,18]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,19]+stream[,20]+stream[,21]
      }else if(h==19){ yyy=stream[,19]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,20]+stream[,21]
      }else if(h==20){ yyy=stream[,20]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,21]
      }else if(h==21){ yyy=stream[,21]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]+stream[,8]+stream[,9]+stream[,10]+stream[,11]+stream[,12]+stream[,13]+stream[,14]+stream[,15]+stream[,16]+stream[,17]+stream[,18]+stream[,19]+stream[,20]
      }
      pre_mlr<-lm(yyy,data=stream)
      pre_mlr_sum=summary(pre_mlr)
      sdd=pre_mlr_sum$sigma
      resi=residuals(pre_mlr)
      ps=dnorm(resi,0,sdd)
      stream_ps=stream
      stream_ps$ps=ps
      pre=predict(pre_mlr)
      ps_sq=ps^2
      tm=stream[,h]
      tm_sq=tm^2
      Y=stream[,1]
      lmps<-lm(Y~tm+tm_sq+ps+ps_sq+tm*ps) # this is the implementation of equation (4)
      EY=rep(0,length(x)) 
      for (k in 1:6) {
        if(is.na(lmps$coefficients[k]))
        {lmps$coefficients[k]=0}
      }
      temp= h-1
      for (i in 1:length(x))
      {
        for(j in 1:length(x))
        {
          EY[i]=EY[i]+lmps$coefficients[1]+lmps$coefficients[2]*tm[i]+lmps$coefficients[3]*tm_sq[i]+lmps$coefficients[4]*dnorm(tm[i]-pre[j],0,sdd)+lmps$coefficients[5]*dnorm(tm[i]-pre[j],0,sdd)*dnorm(tm[i]-pre[j],0,sdd)+lmps$coefficients[6]*tm[i]*dnorm(tm[i]-pre[j],0,sdd)
        }
      }
      EY=EY/length(x)
      SaveDR <- c("C:/Users/lihon/Desktop/paper/bias-sigir22/code/data/2German/")
      png(filename = paste0(SaveDR,t,h,".png"),width = 700, height = 700)
      par(mfrow=c(1,1),mar=c(4.2,4.2,1,0.5),oma=c(0,0,0,0))
      plot(tm,Y,xlim=c(min(tm)-0.1,max(tm)+0.1),ylab="EY",xlab="treatment",cex.lab=1.4,cex.axis=1.4)
      lines(spline(tm,EY),col="blue",lty=1,lwd=2)
      dev.off()
      ma[temp,] = c(pre_mlr$coefficients[1],pre_mlr$coefficients[2],pre_mlr$coefficients[3],pre_mlr$coefficients[4],pre_mlr$coefficients[5],
                    pre_mlr$coefficients[6],pre_mlr$coefficients[7],pre_mlr$coefficients[8],pre_mlr$coefficients[9],pre_mlr$coefficients[10],
                    pre_mlr$coefficients[11],pre_mlr$coefficients[12],pre_mlr$coefficients[13],pre_mlr$coefficients[14],pre_mlr$coefficients[15],
                    pre_mlr$coefficients[16],pre_mlr$coefficients[17],pre_mlr$coefficients[18],pre_mlr$coefficients[19],pre_mlr$coefficients[20],sdd,
                    lmps$coefficients[1],lmps$coefficients[2],lmps$coefficients[3],lmps$coefficients[4],lmps$coefficients[5],lmps$coefficients[6])
      do[temp,] = EY
    }
    e <- list(data.frame(ma),data.frame(do))
    write.table(e,"C:/Users/lihon/Desktop/paper/bias-sigir22/code/data/2German/temp.csv",sep=",",row.names=FALSE,col.names=FALSE,append=T)
    }
  theta (1:100,xstream)
}





