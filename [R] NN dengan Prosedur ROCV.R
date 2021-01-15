##PEMBAGIAN DATA
Train1x=ts(Penumpang[1:33],frequency = 12,start = c(2017,1))
Train2x=ts(Penumpang[1:34],frequency = 12,start = c(2017,1))
Train3x=ts(Penumpang[1:35],frequency = 12,start = c(2017,1))
Train4x=ts(Penumpang[1:36],frequency = 12,start = c(2017,1))
Train5x=ts(Penumpang[1:37],frequency = 12,start = c(2017,1))
Train6x=ts(Penumpang[1:38],frequency = 12,start = c(2017,1))
Train7x=ts(Penumpang[1:39],frequency = 12,start = c(2017,1))
Train8x=ts(Penumpang[1:40],frequency = 12,start = c(2017,1))
Train9x=ts(Penumpang[1:41],frequency = 12,start = c(2017,1))

datatrain.x = cbind(Train1x,Train2x,Train3x,Train4x,Train5x,Train6x,
                    Train7x,Train8x,Train9x)
datatest.x = Penumpang[34:42]

##TRAINING, TESTING, EVALUASI
rollingx = function(datatrain,datatest,hd.max=NULL,lags=NULL){
  mse.train=array(NA,c(hd.max,1),dimnames=list(paste0("H",1:hd.max)))
  mse.test=array(NA,c(hd.max,1),dimnames=list(paste0("H",1:hd.max)))
  mape.train=array(NA,c(hd.max,1),dimnames=list(paste0("H",1:hd.max)))
  mape.test=array(NA,c(hd.max,1),dimnames=list(paste0("H",1:hd.max)))
  forecast.hd=list()
  for (h in 1:hd.max){
    k=ncol(datatrain)
    e=c()
    mse=c()
    mape=c()
    forecast=c()
    for (i in 1:k) {
      model=mlp(na.omit(datatrain[,i]),hd=h,lags=lags,sel.lag=F,
                allow.det.season=F,difforder=0,act.fct="tanh")
      frc=forecast(model,1)
      forecast[i]=frc$mean
      e[i]=datatest[i]-forecast[i]
      mse[i]=model$MSE
      mape[i]=accuracy(frc)[5]
    }
    forecast.hd[h]=list(data.frame(forecast))
    mse.train[h]=mean(mse)
    mse.test[h]=mean(e^2)
    mape.train[h]=mean(mape)
    mape.test[h]=mean(abs(e/datatest))*100
  }
  hd.best=which(mape.test==min(mape.test))[1]
  hasil=data.frame(datatest,forecast.hd[hd.best])
  colnames(hasil)=c("datatest","forecast")
  print(list(hd.best=hd.best,mse.train=mse.train,mse.test=mse.test,
             mape.train=mape.train,mape.test=mape.test,hasil=hasil))
}

rolling.x = rollingx(datatrain.x,datatest.x,hd.max = 7,lags = c(1:12))

ts.plot(ts(rolling.x$hasil$datatest,frequency = 12),
        ts(rolling.x$hasil$forecast,frequency = 12),
        lty=c(1,3),col=c("blue","red"),type="o",
        main="Plot Hasil Peramalan Training X dan Data Testing X")

##PENERAPAN MODEL FFNN TERBAIK PADA SELURUH DATA
hx = rolling.x$hd.best
model.x = mlp(Penumpang.all,hd = 3,lags = c(1:12),sel.lag = F,difforder=0,
              allow.det.season = F,outplot = F,act.fct="tanh")

ts.plot(Penumpang.all,fitted(model.x),lty=c(1,3),col=c("blue","red"),
        main="Plot Aktual dan Prediksi Jumlah Penumpang")

##PERAMALAN
frcx = forecast(model.x,h=6)
plot(frcx)
summary(frcx)
round(exp(frcx$mean))

ts.plot(Penumpang.all,frcx$mean,col=c("black","blue"),type="l",
        main="Peramalan Jumlah Penumpang")