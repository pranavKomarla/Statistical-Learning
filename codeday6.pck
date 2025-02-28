codeday6.pck <-
c("codeday6.pck", "my.bootstrap1", "gui.bootstrap1", "my.bootstrap2", 
"gui.bootstrap2", "my.bootstrap3", "gui.bootstrapxy", "my.dat.plot5", 
"my.dat.plot5a")
my.bootstrap1 <-
function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000){
#function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T)
#stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1)
par(mfrow=c(1,2))
stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F)
F0<-stat.out0$F
resid0<-stat.out0$resid.lin
bootvec<-NULL
y0<-predict(stat.out0$smstrlin,mat[,i])$y
matb<-mat
for(i1 in 1:nboot){
	if(floor(i1/500)==(i1/500)){print(i1)}
	residb<-sample(resid0,replace=T)
	Yb<-y0+residb
	matb[,j]<-Yb
	stat.outb<-my.dat.plot5(matb,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T)
	bootvec<-c(bootvec,stat.outb$F)
}
pvalboot<-sum(bootvec>F0)/nboot
boxplot(bootvec)
stat.out0$pvalboot<-pvalboot
stat.out0	
}
gui.bootstrap1 <-
function(){
library(tcltk)
inputs <- function(){

   x <- tclVar("NOAA")
   y <- tclVar("1")
   z <- tclVar("2")
   w<-tclVar("\"delta temp\"") 
wa<-tclVar("\"disasters\"")
wb<-tclVar("\"Disasters vs warming\"")
zc<-tclVar("2")
qc<-tclVar("F")
rc<-tclVar("10000")

   tt <- tktoplevel()
   tkwm.title(tt,"Choose parameters for new function                   ")
   x.entry <- tkentry(tt, textvariable=x)
   y.entry <- tkentry(tt, textvariable=y)
   z.entry <- tkentry(tt, textvariable=z)
   w.entry<-tkentry(tt, textvariable=w)  
wa.entry<-tkentry(tt,textvariable=wa)
wb.entry<-tkentry(tt,textvariable=wb)
zc.entry<-tkentry(tt,textvariable=zc)
qc.entry<-tkentry(tt,textvariable=qc)
rc.entry<-tkentry(tt,textvariable=rc)
   reset <- function()
    {
     tclvalue(x)<-""
     tclvalue(y)<-""
     tclvalue(z)<-""
	tclvalue(w)<-""
tclvalue(wa.entry)<-""
tclvalue(wb.entry)<-""
tclvalue(zc.entry)<-""
tclvalue(qc.entry)<-""
tclvalue(rc.entry)<-""
       }

   reset.but <- tkbutton(tt, text="Reset", command=reset)

   submit <- function() {
     x <- tclvalue(x)
     y <- tclvalue(y)
     z <- tclvalue(z)
       w<-tclvalue(w)
	wa<-tclvalue(wa)
	wb<-tclvalue(wb)
	zc<-tclvalue(zc)  
	qc<-tclvalue(qc)
	rc<-tclvalue(rc)
     e <- parent.env(environment())
     e$x <- x
     e$y <- y
     e$z <- z
e$w<-w
e$wa<-wa
e$wb<-wb
e$zc<-zc
e$qc<-qc
e$rc<-rc
        tkdestroy(tt)
   }

   submit.but <- tkbutton(tt, text="start", command=submit)
   tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2)
   tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
   tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
   tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)

  tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
   tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
   tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
   tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
   
tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
   tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)

   tkgrid(submit.but, reset.but)

  tkwait.window(tt)
  return(c(x,y,z,w,wa,wb,zc,qc,rc))
}
#Now run the function like:
predictor_para <- inputs()
print(predictor_para)
mat<-eval(parse(text=predictor_para[1]))
ind1<-eval(parse(text=predictor_para[2]))
ind2<-eval(parse(text=predictor_para[3]))
xlab<-eval(parse(text=predictor_para[4]))
ylab<-eval(parse(text=predictor_para[5]))
maintitle<-eval(parse(text=predictor_para[6]))
zcol<-eval(parse(text=predictor_para[7]))
zsqrt<-eval(parse(text=predictor_para[8]))
znboot<-eval(parse(text=predictor_para[9]))
my.bootstrap1(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot)

}
my.bootstrap2 <-
function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000,pred.bound=T,conf.lev=.95,pivotal=T){
#function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T)
#stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1)
par(mfrow=c(1,1))
stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F)
resid0<-stat.out0$resid.mod
bootmat<-NULL
y0<-predict(stat.out0$smstrmod,mat[,i])$y
matb<-mat
for(i1 in 1:nboot){
	if(floor(i1/500)==(i1/500)){print(i1)}
	residb<-sample(resid0,replace=T)
	Yb<-y0+residb
	matb[,j]<-Yb
#print(matb)
	stat.outb<-my.dat.plot5(matb,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T)
	Ybp<-predict(stat.outb$smstrmod,matb[,i])$y
	if(pred.bound){
		if(pivotal){
			bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp-y0)
		}else{
			bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp)
		}
	}else{
		if(pivotal){
			bootmat<-rbind(bootmat,Ybp-y0)
		}else{
			bootmat<-rbind(bootmat,Ybp)
		}
	}

}
alpha<-(1-conf.lev)/2
my.quant<-function(x,a=alpha){quantile(x,c(a,1-a))}
bounds<-apply(bootmat,2,my.quant)
if(pivotal){
	bounds[1,]<-y0-bounds[1,]
	bounds[2,]<-y0-bounds[2,]
}
x<-mat[,i]
if(do.sqrt){
	x<-sqrt(x)
}
o1<-order(x)
lines(x[o1],bounds[1,o1],col=zcol+2)
lines(x[o1],bounds[2,o1],col=zcol+2)
}
gui.bootstrap2 <-
function(){
library(tcltk)
#,pred.bound=T,conf.lev=.95,pivotal=T
inputs <- function(){

   x <- tclVar("NOAA")
   y <- tclVar("1")
   z <- tclVar("2")
   w<-tclVar("\"delta temp\"") 
wa<-tclVar("\"disasters\"")
wb<-tclVar("\"Disasters vs warming\"")
zc<-tclVar("2")
qc<-tclVar("F")
rc<-tclVar("10000")
za<-tclVar("T")
zb<-tclVar(".95")
wc<-tclVar("T")

   tt <- tktoplevel()
   tkwm.title(tt,"Choose parameters for new function                   ")
   x.entry <- tkentry(tt, textvariable=x)
   y.entry <- tkentry(tt, textvariable=y)
   z.entry <- tkentry(tt, textvariable=z)
   w.entry<-tkentry(tt, textvariable=w)  
wa.entry<-tkentry(tt,textvariable=wa)
wb.entry<-tkentry(tt,textvariable=wb)
zc.entry<-tkentry(tt,textvariable=zc)
qc.entry<-tkentry(tt,textvariable=qc)
rc.entry<-tkentry(tt,textvariable=rc)
za.entry<-tkentry(tt,textvariable=za)
zb.entry<-tkentry(tt,textvariable=zb)
wc.entry<-tkentry(tt,textvariable=wc)

   reset <- function()
    {
     tclvalue(x)<-""
     tclvalue(y)<-""
     tclvalue(z)<-""
	tclvalue(w)<-""
tclvalue(wa.entry)<-""
tclvalue(wb.entry)<-""
tclvalue(zc.entry)<-""
tclvalue(qc.entry)<-""
tclvalue(rc.entry)<-""
tclvalue(za.entry)<-""
tclvalue(zb.entry)<-""
tclvalue(wc.entry)<-""

       }

   reset.but <- tkbutton(tt, text="Reset", command=reset)

   submit <- function() {
     x <- tclvalue(x)
     y <- tclvalue(y)
     z <- tclvalue(z)
       w<-tclvalue(w)
	wa<-tclvalue(wa)
	wb<-tclvalue(wb)
	zc<-tclvalue(zc)  
	qc<-tclvalue(qc)
	rc<-tclvalue(rc)
	za<-tclvalue(za)
zb<-tclvalue(zb)
wc<-tclvalue(wc)

     e <- parent.env(environment())
     e$x <- x
     e$y <- y
     e$z <- z
e$w<-w
e$wa<-wa
e$wb<-wb
e$zc<-zc
e$qc<-qc
e$rc<-rc
e$za<-za
e$zb<-zb
e$wc<-wc

        tkdestroy(tt)
   }

   submit.but <- tkbutton(tt, text="start", command=submit)
   tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2)
   tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
   tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
   tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)

  tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
   tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
   tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
   tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
   
tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
   tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Prediction Bound?"),columnspan=2)
   tkgrid(tklabel(tt,text="Pbound=T"),za.entry, pady=10, padx =30)
tkgrid(tklabel(tt,text="Conf.level"),columnspan=2)
   tkgrid(tklabel(tt,text="Confidence"),zb.entry, pady=10, padx =30)
tkgrid(tklabel(tt,text="Pivotal=T"),columnspan=2)
   tkgrid(tklabel(tt,text="Pivotal"),wc.entry, pady=10, padx =30)


   tkgrid(submit.but, reset.but)

  tkwait.window(tt)
  return(c(x,y,z,w,wa,wb,zc,qc,rc,za,zb,wc))
}
#Now run the function like:
predictor_para <- inputs()
print(predictor_para)
mat<-eval(parse(text=predictor_para[1]))
ind1<-eval(parse(text=predictor_para[2]))
ind2<-eval(parse(text=predictor_para[3]))
xlab<-eval(parse(text=predictor_para[4]))
ylab<-eval(parse(text=predictor_para[5]))
maintitle<-eval(parse(text=predictor_para[6]))
zcol<-eval(parse(text=predictor_para[7]))
zsqrt<-eval(parse(text=predictor_para[8]))
znboot<-eval(parse(text=predictor_para[9]))
zpred<-eval(parse(text=predictor_para[10]))
zconf<-eval(parse(text=predictor_para[11]))
zpivot<-eval(parse(text=predictor_para[12]))

my.bootstrap2(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot,zpred,zconf,zpivot)

}
my.bootstrap3 <-
function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000,pred.bound=T,conf.lev=.95,pivotal=T){
par(mfrow=c(1,1))
stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F)
bootmat<-NULL
y0<-predict(stat.out0$smstrmod,mat[,i])$y
matb<-mat
nm<-length(matb[,1])
for(i1 in 1:nboot){
	if(floor(i1/500)==(i1/500)){print(i1)}
	zed<-sample(nm,replace=T)
	matb<-mat[zed,]
#print(matb)
	stat.outb<-my.dat.plot5a(matb,mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T)
	Ybp<-predict(stat.outb$smstrmod,mat[,i])$y
	if(pred.bound){
		if(pivotal){
			bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp-y0)
		}else{
			bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp)
		}
	}else{
		if(pivotal){
			bootmat<-rbind(bootmat,Ybp-y0)
		}else{
			bootmat<-rbind(bootmat,Ybp)
		}
	}

}
alpha<-(1-conf.lev)/2
my.quant<-function(x,a=alpha){quantile(x,c(a,1-a))}
bounds<-apply(bootmat,2,my.quant)
if(pivotal){
	bounds[1,]<-y0-bounds[1,]
	bounds[2,]<-y0-bounds[2,]
}
x<-mat[,i]
if(do.sqrt){
	x<-sqrt(x)
}
o1<-order(x)
lines(x[o1],bounds[1,o1],col=zcol+2)
lines(x[o1],bounds[2,o1],col=zcol+2)
}
gui.bootstrapxy <-
function(){
library(tcltk)
#,pred.bound=T,conf.lev=.95,pivotal=T
inputs <- function(){

   x <- tclVar("NOAA")
   y <- tclVar("1")
   z <- tclVar("2")
   w<-tclVar("\"delta temp\"") 
wa<-tclVar("\"disasters\"")
wb<-tclVar("\"Disasters vs warming\"")
zc<-tclVar("2")
qc<-tclVar("F")
rc<-tclVar("10000")
za<-tclVar("T")
zb<-tclVar(".95")
wc<-tclVar("T")

   tt <- tktoplevel()
   tkwm.title(tt,"Choose parameters for new function                   ")
   x.entry <- tkentry(tt, textvariable=x)
   y.entry <- tkentry(tt, textvariable=y)
   z.entry <- tkentry(tt, textvariable=z)
   w.entry<-tkentry(tt, textvariable=w)  
wa.entry<-tkentry(tt,textvariable=wa)
wb.entry<-tkentry(tt,textvariable=wb)
zc.entry<-tkentry(tt,textvariable=zc)
qc.entry<-tkentry(tt,textvariable=qc)
rc.entry<-tkentry(tt,textvariable=rc)
za.entry<-tkentry(tt,textvariable=za)
zb.entry<-tkentry(tt,textvariable=zb)
wc.entry<-tkentry(tt,textvariable=wc)

   reset <- function()
    {
     tclvalue(x)<-""
     tclvalue(y)<-""
     tclvalue(z)<-""
	tclvalue(w)<-""
tclvalue(wa.entry)<-""
tclvalue(wb.entry)<-""
tclvalue(zc.entry)<-""
tclvalue(qc.entry)<-""
tclvalue(rc.entry)<-""
tclvalue(za.entry)<-""
tclvalue(zb.entry)<-""
tclvalue(wc.entry)<-""

       }

   reset.but <- tkbutton(tt, text="Reset", command=reset)

   submit <- function() {
     x <- tclvalue(x)
     y <- tclvalue(y)
     z <- tclvalue(z)
       w<-tclvalue(w)
	wa<-tclvalue(wa)
	wb<-tclvalue(wb)
	zc<-tclvalue(zc)  
	qc<-tclvalue(qc)
	rc<-tclvalue(rc)
	za<-tclvalue(za)
zb<-tclvalue(zb)
wc<-tclvalue(wc)

     e <- parent.env(environment())
     e$x <- x
     e$y <- y
     e$z <- z
e$w<-w
e$wa<-wa
e$wb<-wb
e$zc<-zc
e$qc<-qc
e$rc<-rc
e$za<-za
e$zb<-zb
e$wc<-wc

        tkdestroy(tt)
   }

   submit.but <- tkbutton(tt, text="start", command=submit)
   tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2)
   tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
   tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
   tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)

  tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
   tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
   tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
   tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
   
tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
   tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Prediction Bound?"),columnspan=2)
   tkgrid(tklabel(tt,text="Pbound=T"),za.entry, pady=10, padx =30)
tkgrid(tklabel(tt,text="Conf.level"),columnspan=2)
   tkgrid(tklabel(tt,text="Confidence"),zb.entry, pady=10, padx =30)
tkgrid(tklabel(tt,text="Pivotal=T"),columnspan=2)
   tkgrid(tklabel(tt,text="Pivotal"),wc.entry, pady=10, padx =30)


   tkgrid(submit.but, reset.but)

  tkwait.window(tt)
  return(c(x,y,z,w,wa,wb,zc,qc,rc,za,zb,wc))
}
#Now run the function like:
predictor_para <- inputs()
print(predictor_para)
mat<-eval(parse(text=predictor_para[1]))
ind1<-eval(parse(text=predictor_para[2]))
ind2<-eval(parse(text=predictor_para[3]))
xlab<-eval(parse(text=predictor_para[4]))
ylab<-eval(parse(text=predictor_para[5]))
maintitle<-eval(parse(text=predictor_para[6]))
zcol<-eval(parse(text=predictor_para[7]))
zsqrt<-eval(parse(text=predictor_para[8]))
znboot<-eval(parse(text=predictor_para[9]))
zpred<-eval(parse(text=predictor_para[10]))
zconf<-eval(parse(text=predictor_para[11]))
zpivot<-eval(parse(text=predictor_para[12]))

my.bootstrap3(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot,zpred,zconf,zpivot)

}
my.dat.plot5 <-
function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T){
if(in.boot){
do.sqrt<-F
}
if(!do.sqrt){

smstr<-smooth.spline(mat[,i],mat[,j])
smstr.lin<-smooth.spline(mat[,i],mat[,j],df=2)
if(do.plot){
plot(mat[,i],mat[,j],xlab=zxlab,ylab=zylab,main=zmain)
lines(smstr,col=zcol)
lines(smstr.lin,col=(zcol+1))

}
resid1<-mat[,j]-predict(smstr,mat[,i])$y
resid2<-mat[,j]-predict(smstr.lin,mat[,i])$y
}else{
smstr<-smooth.spline(mat[,i],sqrt(mat[,j]))
smstr.lin<-smooth.spline(mat[,i],sqrt(mat[,j]),df=2)
if(do.plot){
plot(mat[,i],sqrt(mat[,j]),xlab=zxlab,ylab=zylab,main=zmain)
lines(smstr,col=zcol)
lines(smstr.lin,col=(zcol+1))
}
resid1<-sqrt(mat[,j])-predict(smstr,mat[,i])$y
resid2<-sqrt(mat[,j])-predict(smstr.lin,mat[,i])$y
}

dfmod<-smstr$df
dflin<-2
ssmod<-sum(resid1^2)
sslin<-sum(resid2^2)
numss<-sslin-ssmod
n1<-length(mat[,j])
Fstat<-(numss/(dfmod-dflin))/(ssmod/(n1-dfmod))
pvalue<-1-pf(Fstat,dfmod-dflin,n1-dfmod)
stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1)
}
my.dat.plot5a <-
function(mat=NOAA,mat0,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T){
if(in.boot){
do.sqrt<-F
}
if(!do.sqrt){

smstr<-smooth.spline(mat[,i],mat[,j])
pstr<-predict(smstr,mat0[,i])
smstr$x<-pstr$x
smstr$y<-pstr$y
smstr.lin<-smooth.spline(mat[,i],mat[,j],df=2)
if(do.plot){
plot(mat[,i],mat[,j],xlab=zxlab,ylab=zylab,main=zmain)
lines(smstr,col=zcol)
lines(smstr.lin,col=(zcol+1))

}
resid1<-mat[,j]-predict(smstr,mat[,i])$y
resid2<-mat[,j]-predict(smstr.lin,mat[,i])$y
}else{
smstr<-smooth.spline(mat[,i],sqrt(mat[,j]))
pstr<-predict(smstr,mat0[,i])
smstr$x<-pstr$x
smstr$y<-pstr$y

smstr.lin<-smooth.spline(mat[,i],sqrt(mat[,j]),df=2)
if(do.plot){
plot(mat[,i],sqrt(mat[,j]),xlab=zxlab,ylab=zylab,main=zmain)
lines(smstr,col=zcol)
lines(smstr.lin,col=(zcol+1))
}
resid1<-sqrt(mat[,j])-predict(smstr,mat[,i])$y
resid2<-sqrt(mat[,j])-predict(smstr.lin,mat[,i])$y
}

dfmod<-smstr$df
dflin<-2
ssmod<-sum(resid1^2)
sslin<-sum(resid2^2)
numss<-sslin-ssmod
n1<-length(mat[,j])
Fstat<-(numss/(dfmod-dflin))/(ssmod/(n1-dfmod))
pvalue<-pf(Fstat,dfmod-dflin,n1-dfmod)
stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1)
}
