setwd("C:/Users/lihon/Desktop/paper/fairness-22/code/data/MC/Generalized-propensity-score-master")
xstream<-read.table("C:/Users/lihon/Desktop/paper/fairness-22/code/data/MC/gps_200.csv",header=F,sep=",")


theta<-function (x,xstream)
  {
  stream=xstream[x,]
  ma <- matrix(0.0,6,13)
  do <- matrix(0.0,6,200)
  for (h in 2:7)
    {
      if(h==2){yyy=stream[,2]~stream[,3]+stream[,4]+stream[,5]+stream[,6]+stream[,7]
      }else if(h==3){ yyy=stream[,3]~stream[,2]+stream[,4]+stream[,5]+stream[,6]+stream[,7]
      }else if(h==4){ yyy=stream[,4]~stream[,2]+stream[,3]+stream[,5]+stream[,6]+stream[,7]
      }else if(h==5){ yyy=stream[,5]~stream[,2]+stream[,3]+stream[,4]+stream[,6]+stream[,7]
      }else if(h==6){ yyy=stream[,6]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,7]
      }else if(h==7){ yyy=stream[,7]~stream[,2]+stream[,3]+stream[,4]+stream[,5]+stream[,6]
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
      lmps<-lm(Y~tm+tm_sq+ps+ps_sq+tm*ps) 
      EY=rep(0,length(x)) 
      for (k in 1:6) {
        if(is.na(lmps$coefficients[k]))
          {lmps$coefficients[k]=0}
      }
      for (i in 1:length(x))
      {
        for(j in 1:length(x))
        {
          EY[i]=EY[i]+lmps$coefficients[1]+lmps$coefficients[2]*tm[i]+lmps$coefficients[3]*tm_sq[i]+lmps$coefficients[4]*dnorm(tm[i]-pre[j],0,sdd)+lmps$coefficients[5]*dnorm(tm[i]-pre[j],0,sdd)*dnorm(tm[i]-pre[j],0,sdd)+lmps$coefficients[6]*tm[i]*dnorm(tm[i]-pre[j],0,sdd)
        }
      }
      EY=EY/length(x)
      fn = paste0(h,".eps")
      setEPS()
      postscript(fn,width = 3, height = 3)
      par(mfrow=c(1,1),mar=c(4.2,4.2,1,0.5),oma=c(0,0,0,0))
      plot(tm,Y,xlim=c(min(tm)-0.1,max(tm)+0.1),
           ylab="Probability",xlab="Treatment",cex.lab=1.4,cex.axis=1.4)
      lines(spline(tm,EY),col="red",lty=1,lwd=2)
      dev.off()
      temp= h-1
      ma[temp,] = c(pre_mlr$coefficients[1],pre_mlr$coefficients[2],pre_mlr$coefficients[3],pre_mlr$coefficients[4],pre_mlr$coefficients[5],pre_mlr$coefficients[6],sdd,
                    lmps$coefficients[1],lmps$coefficients[2],lmps$coefficients[3],lmps$coefficients[4],lmps$coefficients[5],lmps$coefficients[6])
      do[temp,] = EY
  }
  e <- list(ma,do) 
}
theta (1:200,xstream)



