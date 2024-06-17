! Copyright Juha Lappi 17.6 2024 

!** fig2 can be made by giving Jlp22 command fig2 ,etc
*** all figures can be done giving command all
***extra figures and computations can done with extra
!*** figure 1 is NOT done here
;again:
macros=macro(again,init,all,extra,scale,fig2...fig18,table1,table2,
luke,rpine,deftrans,fig70,compare,volcomp,asp,comvtot,fp,onetwo,printonetwo,scaleu,rdataharv,
cpuu,lukebark,current)

cont0=ask('continue after figures (1/0)>',default->0)
extra0=ask('do extras (1/0)>',default->0)
tit=ask('put fig number to title (1/0)>',default->0)
!**Note ** Figure1 is obtained from outside file figure1.pdf
init

;return

;all:

 ;do(ifi,2,18) 
fig"ifi"
;if(ifi.eq.9)table1
;if(ifi.eq.15)table2
;enddo
! ;enddo
! fig2
! fig3
! fig4
! fig5
! fig6
! fig7
! fig8
! fig9
! table1
! fig10
! fig11
! fig12
! fig13
! fig14
! fig15
! table2
! fig16
! fig17
! fig18


;return
*************figures and tables 
;current:
fi=draw(func->"Weight0",width->2,x->Dtop,color->Red,append->,continue->,xrange->(15,50),show->0)
fi=draw(func->"Func0",width->2,x->Dtop,color->Blue,append->,continue->,xrange->(15,50),show->0,append->)
fi=draw(func->"Weight",width->4,x->Dtop,color->Red,append->,continue->,xrange->(15,50),show->0)
fi=draw(func->"Func",width->4,x->Dtop,color->Blue,append->,continue->,xrange->(15,50),show->0,append->)
show(fi)
;return

;init:
cpine=0.9
Table1=0
comparedone=0
ndat=0

**this can be used when making  figures
hoo=matrix(350,do->)

fdone=0
aspdone=0

font=']Arial,11['
font2=']Arial,13['
pi4=Pi/400
Pi400=Pi/400   !makes volumes into litres from v0=D*D*L

volcom=0

eng=1   !figures in english
finn=.not.eng

** make diameter data file which looks like harvester data, 
** i.e. it has the diameter at 1 dm intervals starting from stump

** make also a master data which tells for each tree the stand index and
**  number of
** nonzero diameter measurements and the stump height

lmi=matrix(9,values->(1,2,1,1,1,3,3,3,3))
lma=matrix(9,values->(1,2,1,3,3,1,1,3,3))
dmi=matrix(9,values->(1,2,3,1,3,1,3,1,3))
out=matrix(9,values->(2,1,3,4,5,6,7,8,9))

!
!fi=draw(func->(x*(x2-x1)*y1*y2/((x-x1)*x2*y1+(x2-x)*x1*y2)),width->2,x->x,color->Blue,append->,show->0,xrange->(15,50))
x1=15
!y1=0.89929009   !1.1  !10*1.35/48.5  ;  !10*arvop/48.5  macro scaleu
!y1=y1*coeff;
y1=0.90035352
x2=30
!y2=1.06279738     !1.3  !10*1.53/48.5;
!y2=y2*coeff;
y2=1.06405415
y1y2=y1*y2;
x2x1=x2-x1;
x2y1=x2*y1;
x1y2=x1*y2;

Weight='(Dtop*x2x1*y1y2/((Dtop-x1)*x2y1+(x2-Dtop)*x1y2))'
y10=y1*cpine;
y20=y2*cpine;
y1y20=y10*y20;
x2y10=x2*y10;
x1y20=x1*y20;
Weight0='(Dtop*x2x1*y1y20/((Dtop-x1)*x2y10+(x2-Dtop)*x1y20))'

X1=15
Y1=0.72
X2=30
Y2=0.81
Y1Y2=Y1*Y2;
X2X1=X2-X1;
X2Y1=X2*Y1;
X1Y2=X1*Y2;
Func0='(Dtop*X2X1*Y1Y2/((Dtop-X1)*X2Y1+(X2-Dtop)*X1Y2))'
Func='(0.9*Dtop*X2X1*Y1Y2/((Dtop-X1)*X2Y1+(X2-Dtop)*X1Y2))'
Dtop=20;
Weight;
val="Weight"
val;
val="Weight";
!y1=y1/val;
!y2=y2/val;
y1y2=y1*y2;
x2y1=x2*y1;
x1y2=x1*y2;

val2="Weight";

;return



;fig2:
!*** fig2:
Window='450,450'
h=6.81280373
trset=trans()

cyl=0.01*Pi*(D/2)**2*L
if(Obs.eq.9)haketil=cyl-(S+purutil)
f0=(S+purutil)/cyl
h0=haketil/cyl
su=f0+h0
f00=S/cyl
arvo0=arvo-hake-kuori
arvop=h*arvo0/(f0*cyl)
!arvop2=40*arvop/48.5
suh2=V/S
hh=hake/max(haketil,0.01)
V2=S+purutil+haketil+kuoritil

v12=V-V2
/
V=0
setdat=data(read->(D,L,V,S,suh,purutil,hake,kuori,arvo,haketil,kuoritil,puru),in->,maketrans->trset) !7Th
20.4,48.5,178,102.4,1.74,20.93,1.22,0.2,19.83,30.49,19.59,0.21
20.4,48.5,178,90.5,1.94,23.98,1.5,0.2,18.85,37.54,19.59,0.24
24.3,48.5,253,145.3,1.74,32.31,1.51,0.28,27.67,37.85,27.8,0.32
24.3,48.5,253,142.4,1.77,28.16,1.8,0.28,28.69,45.11,27.8,0.28
28,48.5,336,209.3,1.6,33.75,1.78,0.37,39.54,44.56,36.91,0.34
28,48.5,336,204.3,1.64,38.86,1.68,0.37,38.20,42.08,36.91,0.39
15.6,48.5,104,56.4,1.85,13.45,0.82,0.11,10.07,20.56,11.46,0.13
16.3,48.5,114,63.5,1.79,15.07,0.76,0.13,11.85,19.08,12.51,0.15
28,48.5,397 ,204.1,1.95,39.7,0,0,0,44.56,43.7,0
/
setdat%matrix;
stat(until->8)
stat()


fi=plotyx(arvop,D,until->8,continue->,show->0)

! x1=15
! y1=h*0.132  !10*1.35/48.5  ;  !10*arvop/48.5

! x2=30
! y2=h*0.156  !10*1.53/48.5;
! y1y2=y1*y2;
! x2x1=x2-x1;
! x2y1=x2*y1;
! x1y2=x1*y2;

!fi=draw(func->"Weight0",width->2,x->Dtop,color->Red,append->,continue->,xrange->(15,50),show->0)
fi=plotyx(f0,D,mark->3,show->0,color->Blue,data->setdat)

fi=plotyx(arvop,D,mark->3,show->0,color->Red,until->8,append->,data->setdat)


fi=drawline(x1,x2,y10,y20,pointsonly->,mark->5,marksize->2,show->0,append->,color->Red)


fi=draw(func->"Func0",width->2,x->Dtop,color->Blue,append->,width->2,show->0,xrange->(13,50))

! X1=15
! Y1=0.72
! X2=30
! Y2=0.81

fi=draw(func->"Weight0",width->2,x->Dtop,color->Red,append->,show->0,xrange->(13,50))

fi=draw(func->("Func0"*"Weight0"),
width->2,x->Dtop,color->Orange,append->,show->0,xrange->(13,50))
fi=drawline(X1,X2,Y1,Y2,pointsonly->,mark->5,marksize->2,show->0,append->,color->Blue,width->2)
!show(fi,continue->)
! b=0.23
! fi=draw(func->(b*log(x-x1+exp(y1/b))),width->2,x->x,color->Orange,append->,show->0,xrange->(15,50))
! b=0.15
!;return
! fi=draw(func->(b*log(x-x1+exp(y1/b))),width->2,x->x,color->Red,append->,show->0,xrange->(15,50))
 fi=drawline(35,0.795,label->'f(d_{top})"font"',append->,show->0,width->2)
 fi=drawline(35,1.14,label->'u(d_{top})"font"',append->,show->0,width->2)
  fi=drawline(35,0.98,label->'f(d_{top})*u(d_{top})"font"',append->,show->0,width->2)
	;if(tit)show(fi,title->'Figure 2',show->0)
show(fi,continue->,xlabel->'d_{top}, cm"font"',ylabel->' ',yrange->(0.6,1.2),continue->cont0)

;return  !fig2
;fig3:
!*** fig3:
;if(type(pinedee).ne.MATRIX)rpine
Window='450,450'
!pi4=Pi/400

trf1=trans()

mi=2145
lento=pinelentot(mi);
dee=pinedee(mi,1,-lento)
aa=pi4*(dee*.dee)
vee=pinevee(2145,1,-lento)
coe=dee(50)/aa(50)
aa2=coe*aa
hcur=hoo(1,-lento)
h5=interpolate(dee,hcur,5,down->);

v5=interpolate(hcur,vee,h5) ; !vee(h5);
v5b=vee(h5);
logv=vee(50);
cyl=50*aa(50);
outc=vee(50)-cyl;
cylp=100*cyl/v5;
logvp=100*logv/v5;
outcp=100*outc/v5;
pulpp=100*(v5-logv)/v5;
Dtop=16
f="Func"
inc=(1-f)*cyl
incp=100*inc/v5;
vsaw=cyl-inc;
vsawp=100*vsaw/v5
chip=inc+outc;
chipp=100*chip/v5;
/

call(trf1)
fi=drawline(hcur,dee,width->3,color->Black,show->0)
fi=drawline(h5,h5,0,dee(h5),color->Orange,append->,show->0,width->3)
fi=drawline(hcur,aa2,width->2,color->Green,append->,show->0)
fi=drawline(102,109,dee(100),dee(100),width->2,color->Red,append->,show->0,
label->' D(L) "font"')

fi=drawline(2,16.9,label->'V_{OUTC}="outcp]f4.1["%"font"',append->,show->0,width->2)
fi=drawline(2,14,label->'V_{INC}="incp]f4.1["%"font"',append->,show->0,width->2)

fi=drawline(50,54,5,5,label->' Log cylinder"font"',append->,show->0,width->2)

fi=drawline(60,2,label->'V_{pulp}="pulpp]f4.1["%"font"',append->,show->0)

fi=drawline(22,29,aa2(20),aa2(20),width->2,color->Green,append->,show->0,
label->' A(L) "font"')
fi=drawline(0,50,f*dee(50),f*dee(50),width->2,color->Blue,show->0,append->)

fi=drawline(0,50,50,dee(50),dee(50),0,width->2,color->Blue,xlabel->'L, dm"font"',
ylabel->'D, cm"font"',append->,continue->,show->0)
fi=drawline(2,7,label->'V_{saw}="vsawp]f4.1["%"font"',append->,show->0)

fi=drawline(50,20.5,label->'V_{CYL} =V_{saw}+V_{INC} = "cylp]f4.1["%"font"',append->,show->0)
fi=drawline(50,18.7,label->'V_{log} =V_{CYL}+V_{OUTC} = "logvp]f4.1["%"font"',append->,show->0)

fi=drawline(50,22.3,label->'V_{chip}=V_{INC}+V_{OUTC} = "chipp]f4.1["%"font"',append->,show->0,continue->)
fi=drawline(47.1,-0.5,label->'50',append->,show->0,continue->)
;if(tit)show(fi,title->'Figure 3',show->0)
fi=drawline(121.,-0.5,label->'125',append->,continue->cont0)

;return !fig3

;fig4:
!*** fig4
;if(type(pinelentot).ne.MATRIX)rpine
Window='450,450'
fi1=0
fi2=0
traf=trans()
!getobs(pinenew,2145)
!lento;
lento=pinelentot(2145);
!aa=pineaa(2145,1,-lento)
dee=pinedee(2145,1,-lento)

aa=dee*.dee
vee=pinevee(2145,1,-lento)
aaf=pineaaf(2145,1,-lento)
uaaf=pineuaaf(2145,1,-lento)
saw_=matrix(1,lento)
sawu_=matrix(1,lento)
cyl_=matrix(1,lento);
ho=matrix(lento,do->);

h14=find(dee,filter->$.lt.14);
h14=int(h14)
h14=min(h14,58)
!pause('h14>')
do(i,1,lento)
Dtop=dee(i)
aaf(i)="Func"*aa(i)
uaaf(i)="Weight"*aaf(i)
cyl_(i)=aa(i)*i
saw_(i)=aaf(i)*i
sawu_(i)=uaaf(i)*i
enddo
hms=maxloc(saw_);
ms=saw_(hms)
! c*saw_(hms)=dee(hms)  =>c= dee(hms)/saw_(hms)
sawhms=saw_(hms)
csaw=(dee(hms)/sawhms)*saw_
csawu=(dee(hms)/sawu_(hms))*sawu_
cvol=(dee(hms)/vee(hms))*vee
ccyl=(dee(hms)/cyl_(hms))*cyl_
/

call(traf)
fi1=drawline(ho,dee,show->0,width->3)
fi1=drawline(100,112,20,20,append->,show->0,width->3,label->' D(L), cm"font"')
fi1=drawline(100,112,18,18,append->,show->0,color->Green,width->2,label->' V_{log}(L)"font"')

fi1=drawline(15,29,append->,show->0,label->
'Max    L    V_{log} V_{CYL} V_{saw} V_{sawu}"font"')
ii=maxloc(cyl_)
ve=vee(ii)
cy=0.01*cyl_(ii)
sa=0.01*saw_(ii)
sau=0.01*sawu_(ii)
fi1=drawline(3,15,27,27,append->,show->0,color->Blue,width->2,
label->' V_{CYL}   "ii"  "ve" "cy"  "sa"  "sau""font"')
ii=maxloc(saw_)
ve=vee(ii)
cy=0.01*cyl_(ii)
sa=0.01*saw_(ii)
sau=0.01*sawu_(ii)

fi1=drawline(3,15,25.5,25.5,append->,show->0,color->Red,width->2,
label->' V_{saw}   "ii"  "ve" "cy"  "sa"  "sau""font"')

ii=maxloc(sawu_)
ve=vee(ii)
cy=0.01*cyl_(ii)
sa=0.01*saw_(ii)
sau=0.01*sawu_(ii)

fi1=drawline(3,15,24,24,append->,show->0,color->Orange,width->2,
label->' V_{sawu}  "ii"  "ve" "cy"  "sa"  "sau""font"')

fi1=drawline(ho,csaw,append->,show->0,color->Red,width->2)
fi1=drawline(ho,csawu,append->,show->0,color->Orange,width->2)
fi1=drawline(ho,ccyl,append->,show->0,color->Blue,width->2)
fi1=drawline(ho,cvol,append->,continue->,color->Green,width->2,
xlabel->'L, dm"font"',ylabel->'D, cm"font"',show->0)



fi2=0
vto=vee(125);
saa=0.01*saw_(1,-125)
masa=max(saa);
veec=vee(1,-125)
chi=vto-saa
hony=matrix(125,do->)
col=Red
;do(ip,0,10)
pr=0.1*ip;
!prp=10*ip


va=saa+pr*chi;

!mava=max(va);
va=va/masa;
fi2=drawline(hony,va,append->,show->0,color->col,width->2,
label->'"pr]f3.1[""font"')
col=Black
;enddo
fi2=drawline(30,2.59,append->,label->'Value of stem"font"',show->0)
fi2=drawline(112,2.59,append->,label->'P_{chip}/P_{saw}"font"',show->0)

fi2=drawline(37,37,0,va(1),append->,show->0)
fi2=drawline(h14,h14,0,va(1),append->,show->0)
fi1=drawline(37,37,0,20,append->,show->0)
fi1=drawline(h14,h14,0,20,append->,show->0)
fi2=drawline(1,125,1,1,append->,color->Blue,width->2,xlabel->'L, dm"font"',show->0,
yrange->(0,2.7))

!show(fib)
Window='900,450'
;if(tit)show(fi1,title->'Figure 4 (left)',show->0)
;if(tit)show(fi2,title->'Figure 4 (right)',show->0)
fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',
continue->cont0)
;return  !fig4

;fig5:
!*** fig5
;if(type(pinedee).ne.DATA)rpine
Window='450,450'
Lmin=40
Lmax=55
Dmin=15
dmin=5

tranb=trans() !like trana in ana7
less=0
nob=0
saw=0
fi=0
vol=0
veesum=0
iup=0
ipp=0

do(iobs,1,nrows(pinedee))
if((100*int(iobs/100)).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then

top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume

iobs;

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->U%saw)

if(U%nlog.ge.0)then
print(U%vars,expand->)
ones=matrix(120,do->)
syl=matrix(120)
saw=matrix(120)
syl0=matrix(120)
saw0=matrix(120)
do(i,1,55)
syl(i)=pineaa(iobs,i)*i
Dtop=pinedee(iobs,i)
f="Func"
saw(i)=pineaa(iobs,i)*i*f
enddo
syl0(1,-55)=syl(1,-55)
saw0(1,-55)=saw(1,-55)
!h*v(40)=d(40)
!h=d(40)/v(40)
h=pinedee(iobs,30)/syl(30)
j=0
do(i,56,120)
j=j+1
Dtop=pinedee(iobs,i)
f="Func"
syl(i)=syl(55)+pineaa(iobs,i)*j
saw(i)=saw(55)+pineaa(iobs,i)*j*f
syl0(i)=pineaa(iobs,i)*i
saw0(i)=pineaa(iobs,i)*i*f
enddo
dtop1=pinedee(iobs,55);
fi=drawline(ones,h*pinevee(iobs,1,-120),show->0,color->Green,width->2)
fi=drawline(ones,h*saw0,width->1,append->,color->Red,show->0)
fi=drawline(ones,h*syl0,width->1,append->,color->Blue,continue->,show->0)
fi=drawline(ones,h*saw,width->2,append->,color->Red,show->0)
fi=drawline(ones,h*syl,width->2,append->,color->Blue,continue->,show->0)

fi=drawline(10,20,85,85,show->0,append->,width->2,color->Black,label->' D(L), cm"font"')
fi=drawline(10,20,75,75,show->0,append->,width->2,color->Green,label->' V_{log}"font"')
fi=drawline(10,20,65,65,show->0,append->,width->2,color->Blue,label->' V_{CYL}"font"')
fi=drawline(10,20,55,55,show->0,append->,width->2,color->Red,label->' V_{saw}"font"')

fi=drawline(55,55,0,h*pinevee(iobs,55),show->0,append->)
fi=drawline(95,95,0,h*pinevee(iobs,95),show->0,append->)
fi=drawline(10,112,label->'L_{min}=40 dm  L_{max}= 55 dm  D_{min}= 15 cm"font"',append->,show->0)
fi=drawline(10,103,label->'L_1 = 55 dm         L_2 = 40 dm"font"',append->,show->0)
fi=drawline(10,94,label->'D_{top1} = 30.0 cm  D_{top2} = 20.1 cm"font"',append->,show->0)
fi=drawline(ones,pinedee(iobs,1,-120),width->2,append->,color->Black,continue->,show->0,
 xlabel->'L, dm"font"',ylabel->'D, cm"font"')
 if(tit)show(fi,title->'Figure 5',show->0)
 show(fi,yrange->(0,120),continue->cont0)

return
endif
endif
enddo
/
call(tranb)

;return  !fig5

;fig6:
!*** fig6:
;if(type(pinedee).ne.DATA)rpine
Window='450,450'
metr=matrix(80)
hoo=matrix(1,400,do->)
Dmin=15;

tre=trans()
do(iobs,1,nrows(pinedee))
if(pinedee(iobs,40).ge.Dmin)then
top=fi
top=find(pinedee(iobs,All),filter->$.lt.Dmin)-1
nlog=int(top/40)
nlog1=nlog-1
sa=0
do(ilog,1,nlog1)
sa=sa+pineaaf(ilog*40)*40
enddo
v0=0
if(nlog1.gt.0)v0=pinevee(iobs,nlog1*40)
pvee=pinevee(iobs,nlog1*40+1,-top)-v0
vtop=len(pvee)
met=int(pvee(vtop)/40)
if(metr(met).eq.0)then
paaf=pineaaf(iobs,nlog1*40+1,-top)
hoo0=hoo(1,-top+nlog1*40)
sa=Pi400*paaf*.hoo0
!pause('<>')
metr(met)=1
fi=drawline(pvee,sa,show->0,append->,color->Red)
up=min(58,len(pvee))
fi=drawline(pvee(37),sa(37),mark->3,marksize->2,
append->,color->Blue,show->0)
fi=drawline(pvee(up),sa(up),mark->3,marksize->2,
append->,color->Red,show->0)
nlog1;
top;
dd=nlog1*40+1;
top2=top-nlog1*40;  
;now(99;)
fi=drawline(pvee(top2),sa(top2),label->char(top2),append->,show->0)
topo=maxloc(sa)-1
fim=drawline(pvee(topo),sa(topo),label->char(topo),append->,show->0)
endif
endif
enddo
/

fi=0
call(tre)
;if(tit)show(fi,title->'Figure 6',show->0)
show(fi,xlabel->'V_{log}, dm^3"font"',ylabel->'V_{saw}, dm^3"font"',continue->cont0) 
;return  !fig6

;fig7:
!***fig7:
;if(type(datp).ne.DATA)fig70 
Window='450,450'
stat(data->datp)
clas=20
minob=7
xvars=list(Dtop,Logv)

fi1=0   !without fine tuning absolute values
fi2=0

s0=0
c0=1
cont=1
;do(kier,1,2)  !x-axes =dtop  Log

cl=classify(Chipp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,continue->c0,color->Green,se->,sd->,show->s0,width->(1,2))


cl=classify(Outcp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,continue->c0,color->Blue,se->,sd->,width->(1,2),
show->s0,append->,width->2)

cl=classify(Sawp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,color->Orange,se->,sd->,width->2,append->,show->s0)

cl=classify(Sawup,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,color->Orange,width->1,append->,show->s0)

 cl=classify(Incp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
 fi"kier"=drawclass(cl,continue->cont,color->Violet,append->,show->s0,width->2)

 cl=classify(Cylp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
 fi"kier"=drawclass(cl,continue->cont,se->,sd->,width->(1,2),color->Red,append->,show->s0,width->2)

 cl=classify(Incp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
 fi"kier"=drawclass(cl,continue->cont,color->Violet,se->,sd->,width->(1,2),append->,show->s0,width->2)

 cl=classify(Cylp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
 fi"kier"=drawclass(cl,continue->cont,se->,sd->,width->(1,2),color->Red,append->,show->s0,width->2)

;enddo

xloc=35
xdd=2.7
yloc=76
yloc2=67
ydd=3.9
!fi7=drawline(xloc,xloc+xdd,yloc,yloc,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc-ydd,yloc-ydd,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc-2*ydd,yloc-2*ydd,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi1=drawline(25,25+xdd,yloc-2*ydd,yloc-2*ydd,color->Orange,width->1,label->' V_{sawu}"font"',append->,show->0)

fi1=drawline(xloc,xloc+xdd,47,47,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,13,13,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,27,27,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)

fi1=drawline(13,85,label->'%]Arial,13[',show->0,append->)
fi1=drawline(15.4,87,label->'/V_{log}]Arial,13[',show->0,append->)
xloc=500
xdd=47
yloc=76
yloc2=67
ydd=3.9

fi2=drawline(xloc,xloc+xdd,yloc-ydd,yloc-ydd,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,yloc-2*ydd,yloc-2*ydd,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi2=drawline(300,300+xdd,yloc-2*ydd,yloc-2*ydd,color->Orange,width->1,label->' V_{sawu}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,50,50,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,13,13,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,27,27,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)
fi2=drawline(60,85,label->'%]Arial,13[',show->0,append->)
fi2=drawline(103,87,label->'/V_{log}]Arial,13[',show->0,append->)

co=0

show(fi1,xlabel->'D_{top}, cm"font"',xrange->(15,42),show->0)
show(fi2,xlabel->'V_{log}, dm^3"font"',show->0)
xloc=35
xdd=2.7
yloc=63
ydd=4
Window='900,450'
;if(tit)show(fi1,title->'Figure 7 (left)',show->0)
;if(tit)show(fi2,title->'Figure 7 (right)',show->0)
fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',
continue->cont0)

;if(extra0.eq.0);return
fi1=0   !without fine tuning absolute values
fi2=0
s0=0
c0=1
cont=1
;do(kier,1,2)  !x-axes =dtop  Log

cl=classify(Chip,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,continue->c0,color->Green,se->,sd->,show->s0,width->(1,2))


cl=classify(Outc,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,continue->c0,color->Blue,se->,sd->,width->(1,2),
show->s0,append->,width->2)

cl=classify(Saw,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,color->Orange,se->,sd->,width->2,append->,show->s0)

cl=classify(Sawu,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,color->Orange,width->1,append->,show->s0)


 cl=classify(Inc,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
 fi"kier"=drawclass(cl,continue->cont,color->Violet,append->,show->s0,width->2)

 cl=classify(Cyl,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
 fi"kier"=drawclass(cl,continue->cont,se->,sd->,width->(1,2),color->Red,append->,show->s0,width->2)


 cl=classify(Inc,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
 fi"kier"=drawclass(cl,continue->cont,color->Violet,se->,sd->,width->(1,2),append->,show->s0,width->2)

 cl=classify(Cyl,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
 fi"kier"=drawclass(cl,continue->cont,se->,sd->,width->(1,2),color->Red,append->,show->s0,width->2)

;enddo
show(fi1,xlabel->'D_{top}, cm"font"',xrange->(15,42),show->0)
show(fi2,xlabel->'V_{log}, dm^3"font"',show->0)
Window='900,450'
;if(tit)show(fi1,title->'Figure 7 (left)',show->0)
;if(tit)show(fi2,title->'Figure 7 (right)',show->0)
fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1')

;return !fig7

;fig8:
!*** fig8:
;if(type(pinedee).ne.MATRIX)rpine
! fi 11 cumulative wr vcom fi12 cumulative wr dbh
! FI 13 PROS VS DBH
Window='450,450'
Lmin=40
Lmax=55
Dmin=15
dmin=5
clas=30
minob=7
   !without fine tuning absolute values
stemlist0=list(U%logv,U%cyl,U%saw,U%inc,U%outc,U%chip,U%pulp,U%sawu);
stemlistp=list(logvp,cylp,sawp,incp,outcp,chipp,pulpp,sawup);
stemlist=list(dbh,U%com,@stemlist0,@stemlistp);
stemmat=matrix(nrows(pinedee),len(stemlist));
vec=matrix(len(stemlist0));

tranf11=trans()
nob=0
do(iobs,1,nrows(pinedee))
dbh=pinedee(iobs,13)
if(pinedee(iobs,Lmin).ge.Dmin)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",weight->"Weight",obj->U%saw)
nob=nob+1
vec=stemlist0
vec=100*vec/U%com
stemlistp=vec
stemmat(nob,All)=stemlist
endif
enddo
/
call(tranf11)
varlist=list(Logv,Cyl,Saw,Inc,Outc,Chip,Pulp,Sawu)
readlist=list(dbh,Com,@varlist,@stemlistp)
nob;
stemdat=newdata(stemmat(1,-nob,All),read->@readlist)
stat(min->,max->)

colors=matrix(len(varlist),values->(Cyan,Red,Orange,Violet,Blue,Green,Black,Orange))

xvar='Com'

xvar='dbh'
fi1=0
;do(i,1,len(stemlistp)-1)
cl=classify(@stemlistp(i),data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi1=drawclass(cl,continue->,color->colors(i),se->,sd->,show->0,width->(1,2),append->)
;enddo

cl=classify(sawup,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi1=drawclass(cl,continue->,color->Orange,show->0,append->)


xloc=41
xdd=3
yloc1=87
yloc=72
ydd=3.7

fi1=drawline(xloc,xloc+xdd,90,90,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc1-1.2*ydd,yloc1-1.2*ydd,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,61,61,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,46,46,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,28,28,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,16,16,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,10,10,color->Black,width->2,label->' V_{pulp}"font"',append->,show->0)

fi1=drawline(32,32+xdd,60,60,color->Orange,width->1,label->' V_{sawu}"font"',append->,show->0)

fi1=drawline(13.5,95,label->'%]Arial,13[',show->0,append->)
fi1=drawline(16.4,95,label->'/V_{com}]Arial,13[',show->0,append->)
;if(tit)show(fi1,title->'Figure 8 (left)',show->0)
show(fi1,xlabel->'dbh, cm"font"',show->0)

Window='450,450'
xvar='Com'
fi2=0
;do(i,1,len(stemlistp)-1)
cl=classify(@stemlistp(i),data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi2=drawclass(cl,continue->,color->colors(i),se->,sd->,show->0,width->(1,2),append->)
;enddo

cl=classify(sawup,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi2=drawclass(cl,continue->,color->Orange,append->,show->0)

xloc=1300
xdd=120
yloc1=90

yloc=57
ydd=3.7


fi2=drawline(xloc,xloc+xdd,yloc1,yloc1,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,85,85,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,63,63,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,47,47,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,15,15,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,26,26,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,10,10,color->Black,width->2,label->' V_{pulp}"font"',append->,show->0)

fi2=drawline(800,800+xdd,62,62,color->Orange,width->1,label->' V_{sawu}"font"',append->,show->0)

fi2=drawline(-100,95,label->'%]Arial,13[',show->0,append->)
fi2=drawline(60,95,label->'/V_{com}]Arial,13[',show->0,append->)

Window='450,450'
;if(tit)show(fi2,title->'Figure 8 (right)',show->0)
show(fi2,xlabel->'V_{com}, dm^3"font"',show->0)  !!yrange->(0,600),

Window='900,450'

fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->cont0)

;if(extra0.eq.0);return
Window='450,450'
fi1=0

;do(i,1,len(varlist)-1)
cl=classify(@varlist(i),data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi1=drawclass(cl,continue->,color->colors(i),se->,sd->,show->0,width->(1,2),append->)
;enddo
cl=classify(Sawu,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi1=drawclass(cl,continue->,color->Orange,append->,show->0)
xloc=300
xdd=120
yloc=1400
ydd=100

fi1=drawline(xloc,xloc+xdd,yloc+ydd,yloc+ydd,color->Black,width->1,label->' V_{com}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc,yloc,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc-ydd,yloc-ydd,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc-2*ydd,yloc-2*ydd,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc-3*ydd,yloc-3*ydd,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc-4*ydd,yloc-4*ydd,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc-5*ydd,yloc-5*ydd,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)
fi1=drawline(xloc,xloc+xdd,yloc-6*ydd,yloc-6*ydd,color->Black,width->2,label->' V_{pulp}"font"',append->,show->0)

fi1=drawline(600,600+xdd,45,45,color->Orange,width->1,label->' V_{sawu}"font"',append->,show->0)

fi1=drawline(0,1800,0,1800,yrange->(0,1600),
xlabel->'V_{com}, dm^3"font"',  !yrange->(0,600),
ylabel->'V_{*}"font"',append->,show->0)

xvar='dbh'
fi2=0
;do(i,1,len(varlist)-1)
cl=classify(@varlist(i),data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi2=drawclass(cl,continue->,color->colors(i),se->,sd->,show->0,width->(1,2),append->)
;enddo

cl=classify(Sawu,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi2=drawclass(cl,continue->,color->Orange,append->,show->0)


cl=classify(Com,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi2=drawclass(cl,continue->,color->Black,se->,sd->,show->0,width->(1,2),append->)

xloc=20
xdd=3
yloc=1400
ydd=100

fi2=drawline(xloc,xloc+xdd,yloc+ydd,yloc+ydd,color->Black,width->2,label->' V_{com}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,yloc,yloc,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,yloc-ydd,yloc-ydd,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,yloc-2*ydd,yloc-2*ydd,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,yloc-3*ydd,yloc-3*ydd,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,yloc-4*ydd,yloc-4*ydd,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,yloc-5*ydd,yloc-5*ydd,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)
fi2=drawline(xloc,xloc+xdd,yloc-6*ydd,yloc-6*ydd,color->Black,width->2,label->' V_{pulp}"font"',append->,show->0)

fi2=drawline(yrange->(0,1600),
xlabel->'dbh, cm"font"',  !yrange->(0,600),
ylabel->'V_{*}"font"',append->,show->0)
Window='900,450'
;if(tit)show(fi1,title->'Extra for Figure 8 (left)',show->0)
;if(tit)show(fi2,title->'Extra Figure 8 (right)',show->0)
fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->cont0)

;return !fig8

;table1:
!*** table1:
Table1=1
;fig9:
!*** fig9:
;if(comparedone.eq.0)compare
!objs='u%saw;u%sawu;u%saw/u%logv;-u%logv-e*u%saw;>>
! -u%logv+e*u%saw;u%logv-e*u%saw;u%logv+e*u%saw'
!*** after compare
Table1;
;if(Table1);then
Table1=0
!resu=matrix(9,24)
!objs='u%saw;u%sawu;(u%saw/u%logv);(-u%logv-e*u%saw);>>
!(-u%logv+e*u%saw);(u%logv-e*u%saw);(u%logv+e*u%saw)'  irow of matrix resu
!**** Table 1
row=matrix(3,7)
;do(ir,1,7)
row(1,ir)=resu(9,7+ir)  !vlog
;enddo

;do(ir,1,7)
row(2,ir)=resu(9,ir)  !vsaw
;enddo

;do(ir,1,7)
row(3,ir)=resu(9,ir+14) !vsaw
;enddo
write($,'(7f5.1)',row)

;return
;endif

Window='600,450'


objs0='S;U;S/V;vs;vS;Vs;VS'

is=1
fi=0

cols=matrix(7,values->(Red,Orange,Green,Cyan,Violet,Blue,Blue))
take1=vector(1,3,4,7)
;do(irow,1,9)
Irow=irow+0.3
;do(iob,1,7)
iob;
Obj=objs0(iob,';');
isit=find(take1,filter->$.eq.iob)
;if(is.and.isit);then
fi=drawline(Irow-0.4,Irow+0.4,resu2(irow,iob),resu2(irow,iob),unset->'xtics',show->0,
xrange->(0,14),yrange->(0,100),color->cols(iob),width->3) !horizontal
is=0
;elseif(isit);then

fi=drawline(Irow-0.4,Irow+0.4,resu2(irow,iob),resu2(irow,iob),show->0,
color->cols(iob),append->,width->3)

;endif

;enddo
wid=2

fi=drawline(Irow-0.4,Irow-0.4,0,resu2(irow,1),
color->Red,width->wid,show->0,append->)
fi=drawline(Irow+0.4,Irow+0.4,0,resu2(irow,1),
color->Red,width->wid,show->0,append->)
fi=drawline(Irow,11,label->'style( center )"dim(irow,1)""font"',show->0,append->,show->0)
fi=drawline(Irow,7,label->'style( center )"dim(irow,2)""font"',show->0,append->,show->0)
fi=drawline(Irow,3,label->'style( center )"dim(irow,3)""font"',show->0,append->,show->0)

;enddo
fi=drawline(0.05,11,label->'L_{min}"font"',append->,show->0)
fi=drawline(0.05,7,label->'L_{max}"font"',append->,show->0)
fi=drawline(0.05,3,label->'D_{min}"font"',append->,show->0)
!i=drawline(0.2,54,label->'style( font \"Arial,11\" )V_{saw}/V_{com}, %',append->,continue->)

!!!!
!objs='u%saw;u%sawu;u%saw/u%logv;-u%logv-e*u%saw;>>
!-u%logv+e*u%saw;u%logv-e*u%saw;u%logv+e*u%saw'
objs2='S;U;S/V;v;v;V;V'

!revu2=matrix(1,2,matrix->,values->(resu,revu))
!sort(revu2,key->1)
is=0
!fi2=0
take2=vector(1,3,4,7)
;do(irow,1,9)
Irow=irow+0.3
;do(iob,1,7)
!Obj=objs2(iob,';');
!;if(is);then
!fi=drawline(irow,revu2(irow,iob+1),unset->'xtics',label->'style( center )"Obj"',show->0,
!xrange->(0,10))! ,yrange->(37.5,55))
!is=0
!;else
!;if(iob.le.4.or.iob.eq.6)
isit=find(take2,filter->$.eq.iob)
;if(isit)fi=drawline(Irow-0.4,Irow+0.4,revu2(irow,iob),revu2(irow,iob),
color->cols(iob),show->0,append->,width->3)
!;endif

;enddo
wid=2

fi=drawline(Irow-0.4,Irow-0.4,
resu2(irow,1),revu2(irow,7),color->Blue,width->wid,show->0,append->)
fi=drawline(Irow+0.4,Irow+0.4,
resu2(irow,1),revu2(irow,7),color->Blue,width->wid,show->0,append->)

;enddo
fi=drawline(-0.55,90,label->'style( font \"Arial,12\" )%',append->,continue->,show->0)

xa=10
xy=10.8
yd=6
fi=drawline(xa,xy,30,30,label->'style( font \"Arial,11\" ) Max V_{log}',color->cols(6),
append->,show->0,width->3)
fi=drawline(xa,xy,30-yd,30-yd,label->'style( font \"Arial,11\" ) Max V_{saw}',color->cols(1),
append->,show->0,width->3)

fi=drawline(xa,xy,30-2*yd,30-2*yd,label->'style( font \"Arial,11\" ) Max V_{saw}/V_{log}',color->cols(3),
append->,show->0,width->3)
fi=drawline(xa,xy,30-3*yd,30-3*yd,label->'style( font \"Arial,11\" ) Min V_{log}',color->cols(4),
append->,show->0,width->3)

fi=drawline(xa,90,label->'style( font \"Arial,11\" )V_{log}/V_{com}',color->cols(4),
append->,show->0)
fi=drawline(xa,47,label->'style( font \"Arial,11\" )V_{saw}/V_{com}',color->cols(4),
append->,show->0)
;if(tit)show(fi,title->'Figure 9',show->0)
show(fi,continue->cont0)


;return !fig9


;fig10:
!*** fig10:
aspdone;

;if(aspdone.eq.0);then
!*** calling asp upper case
asp
;endif
fdone;
;if(fdone.eq.0);then
!*** calling fp free lower case
fp
;endif
Window='450,450'
fi1=0


fi2=0
Sv=100*Saaw/.Vee;
Svi=100*Saawi/.Veei;
Svu=100*Saawu/.Vee;
Sviu=100*Saawui/.Veei;
;do(il,1,3)
fi2=drawline(cprice,Vee(il,All),color->colors(il),width->2,append->,show->0)

fi2=drawline(cprice,Saaw(il,All),color->colors(il),width->2,append->,show->0)

fi1=drawline(cprice,Veei(il,All),color->colors(il),width->1,append->,show->0)
fi1=drawline(cprice,Saawi(il,All),color->colors(il),width->2,append->,show->0)
;enddo

labxv=80
labd=20
labelhv=vector(59,62,65)
;do(il,1,3)
fi2=drawline(cprice,vee(il,All),color->colors(il),width->1,append->,show->0)
fi2=drawline(cprice,saaw(il,All),color->colors(il),width->1,append->,show->0)
nonz=find(vee2(il,All),filter->$.lt.50)-1
;if(nonz.lt.0)nonz=lenc
fi2=drawline(cprice(1,-nonz),vee2(il,1,-nonz),color->colors(il),width->2,append->,show->0)

!fi3=drawline(cprice,vee2(il,All),color->colors(il),width->2,append->,show->0)
!nonz=find(saaw2(il,All),filter->$.lt.40)-1
!;if(nonz.lt.0)nonz=lenc
fi2=drawline(cprice,saaw2(il,All),color->colors(il),width->2,append->,show->0)

!fi3=drawline(cprice,saaw2(il,All),color->colors(il),width->2,append->,show->0)
!fi=drawline(cprice,saaw(il,All),color->colors(il),width->2,append->,show->0)
fi1=drawline(cprice,veei(il,All),color->colors(il),width->2,append->,show->0)
fi1=drawline(labxv,labxv+labd,labelhv(il),labelhv(il),color->colors(il),width->3,append->,
label->' P_{saw}= "psaws(il)""font"',append->,show->0)
fi2=drawline(labxv,labxv+labd,labelhv(il),labelhv(il),color->colors(il),width->3,append->,
label->' P_{saw}= "psaws(il)""font"',append->,show->0)
;enddo

fi1=drawline(10,81,label->'Free"font"',append->,show->0)
fi1=drawline(10,88,label->'as."font"',append->,show->0)
! show(fi,set->('arrow from 188,87.3 to 120,87.3'),show->0)
fi2=drawline(150,87,label->'Free"font"',append->,show->0)
fi2=drawline(40,80,label->'as."font"',append->,show->0)

!fi2=drawline(160,52.5,label->'style(rotate by 0)SM pulp, free"font"',append->,show->0)

fi2=drawline(80,44,label->'Free."font"',append->,show->0)
fi2=drawline(170,45,label->'as."font"',append->,show->0)

fi1=drawline(cprice(1),cprice(lenc),logmax,logmax,append->,show->0,width->2,color->Cyan)
fi1=drawline(cprice(1),cprice(lenc),logmin,logmin,append->,show->0,width->2,color->Cyan)
 fi1=drawline(cprice(1),cprice(lenc),sawmax,sawmax,append->,show->0,width->2,color->Cyan)
 fi1=drawline(cprice(1),cprice(lenc),sawmin,sawmin,append->,show->0,width->2,color->Cyan)
 fi1=drawline(2,92,label->'V_{log}/V_{com}"font"',append->,show->0)
 fi1=drawline(2,50,label->'V_{saw}/V_{com}"font"',append->,show->0)
  fi1=drawline(-16,85,label->'%"font"',append->,show->0)
	fi2=drawline(cprice(1),cprice(lenc),logmax,logmax,append->,show->0,width->2,color->Cyan)
fi2=drawline(cprice(1),cprice(lenc),logmin,logmin,append->,show->0,width->2,color->Cyan)
 fi2=drawline(cprice(1),cprice(lenc),sawmax,sawmax,append->,show->0,width->2,color->Cyan)
 fi2=drawline(cprice(1),cprice(lenc),sawmin,sawmin,append->,show->0,width->2,color->Cyan)
 fi2=drawline(2,92,label->'V_{log}/V_{com}"font"',append->,show->0)
 fi2=drawline(2,50,label->'V_{saw}/V_{com}"font"',append->,show->0)
  fi2=drawline(-16,85,label->'%"font"',append->,show->0)
!	show(fi1,yrange->(40,95),xlabel->'P_{chip}, €/m^3"font"',
!title->'Figure 10:  V_{log}/V_{com} and V_{saw}/V_{com}"font"',continue->cont0)
!show(fi2,yrange->(40,95),xlabel->'P_{chip}, €/m^3"font"',
!title->'Figure 10:  V_{log}/V_{com} and V_{saw}/V_{com}"font"',continue->cont0)
fi1=drawline(100,45,label->'Free + as."font"',append->,show->0)
;if(tit);then
	show(fi1,yrange->(40,95),xlabel->'P_{chip}, €/m^3"font"',
title->'Figure 10:  V_{log}/V_{com} and V_{saw}/V_{com}"font"',continue->cont0)
	;else
show(fi1,yrange->(40,95),xlabel->'P_{chip}, €/m^3"font"',
title->'Independent sawmills"font"',continue->cont0,show->0)
show(fi2,yrange->(40,95),xlabel->'P_{chip}, €/m^3"font"',
title->'Sawmills with pulp"font"',continue->cont0,show->0)

;endif
Window='900,450'
	fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',
continue->cont0)
;return !fig10

;fig11:
!*** fig11
;if(vtot.le.0)comvtot
cprice=matrix(do->(0,250,10))
lenc=len(cprice)
colors=matrix(3,values->(Blue,Green,Red))

Nlogs=matrix(3,lenc)
Lenns=matrix(3,lenc)
Nlogsi=matrix(3,lenc)
Lennsi=matrix(3,lenc)

psaws=matrix(3,values->(180,220,260));

Lmin=40
Lmax=55
Dmin=15
dmin=5
!psaw=220
plog=75
ppulp=25
trch=trans()
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,Lmin).ge.Dmin)then
iro=0

do(irow,1,3)
psaw=psaws(irow)

do(j,1,lenc)

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cprice(j)*u%totc+psaw*u%sawu-plog*u%logv-ppulp*u%pulp))
Nlogs(irow,j)=Nlogs(irow,j)+u%nlog
Lenns(irow,j)=Lenns(irow,j)+u%lenn

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cprice(j)*u%chip+psaw*u%sawu-plog*u%logv))
Nlogsi(irow,j)=Nlogsi(irow,j)+u%nlog
Lennsi(irow,j)=Lennsi(irow,j)+u%lenn

enddo
enddo
endif
enddo
/
call(trch)

li=;list(u%?);

;if(vtot.le.0)comvtot
cprice=matrix(do->(0,250,10))
lenc=len(cprice)
colors=matrix(3,values->(Blue,Green,Red))

nlogs=matrix(3,lenc)
lenns=matrix(3,lenc)
nlogsi=matrix(3,lenc)
lennsi=matrix(3,lenc)

psaws=matrix(3,values->(180,220,260));

Lmin=40
Lmax=55
Dmin=15
dmin=5

plog=75
ppulp=25
trchf=trans()
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,Lmin).ge.Dmin)then
iro=0

do(irow,1,3)
psaw=psaws(irow)

do(j,1,lenc)

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cprice(j)*u%totc+psaw*u%sawu),any->)
nlogs(irow,j)=nlogs(irow,j)+u%nlog
lenns(irow,j)=lenns(irow,j)+u%lenn

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cprice(j)*u%chip+psaw*u%sawu))
nlogsi(irow,j)=nlogsi(irow,j)+u%nlog
lennsi(irow,j)=lennsi(irow,j)+u%lenn

enddo
enddo
endif
enddo
/
call(trchf)

li=;list(u%?);
!nlogs;
!lenns;

Window='450,450'

Lee=Lenns/.Nlogs;
Leei=Lennsi/.Nlogsi;
nlo=nlogs+0.00001
lee=lenns/.nlo;
leei=lennsi/.nlogsi;
fi1=0
fi2=0
;do(il,1,3)
nz=find(nlogs(il,All),filter->$.eq.0)
la=lenc
;if(nz.gt.0)la=nz-1

fi1=drawline(cprice,Lee(il,All),color->colors(il),width->2,append->,show->0)

fi1=drawline(cprice,Leei(il,All),color->colors(il),width->2,append->,show->0)

fi1=drawline(10,35,45.3+0.6*il,45.3+0.6*il,label->' P_{saw}= "psaws(il)""font"',append->,show->0,
color->colors(il),width->2)

fi2=drawline(cprice(1,-la),lee(il,1,-la),color->colors(il),width->2,append->,show->0)

fi2=drawline(cprice,leei(il,All),color->colors(il),width->2,append->,show->0)

fi2=drawline(10,35,41+0.6*il,41+0.6*il,label->' P_{saw}= "psaws(il)""font"',append->,show->0,
color->colors(il),width->2)

;enddo
fi1=drawline(130,45.8,label->'Independent sawmills"font"',append->,show->0)
fi1=drawline(130,43.8,label->'Sawmills with pulp"font"',append->,show->0)
;if(tit);then
show(fi1,xlabel->'P_{chip}, €/m^3"font"',ylabel->'Average log length, dm"font"',
title->'Figure 11 Assortment pricing"font"',yrange->(40,$),show->0)
;else

show(fi1,xlabel->'P_{chip}, €/m^3"font"',ylabel->'Average log length, dm"font"',
title->'Assortment pricing"font"',yrange->(40,$),show->0)
;endif



fi2=drawline(100,46.2,label->'Independent sawmillls"font"',append->,show->0)
fi2=drawline(100,44.5,label->'Sawmills with pulp"font"',append->,show->0)

;if(tit);then
show(fi2,xlabel->'P_{chip}, €/m^3"font"',ylabel->'Average log length, dm"font"',
yrange->(40,$),title->'Figure 11 Free bucking"font"',show->0)
;else
show(fi2,xlabel->'P_{chip}, €/m^3"font"',ylabel->'Average log length, dm"font"',
yrange->(40,$),title->'Free bucking"font"',show->0)
;endif

Window='900,450'
 fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)
 
  
	;if(extra0.eq.0);return
	fi1=0
	fi2=0
	;do(il,1,3)
	fi1=drawline(cprice,Nlogsi(il,All),color->colors(il),width->2,append->,show->0)
fi1=drawline(cprice,Nlogs(il,All),color->colors(il),width->2,append->,show->0)
	fi2=drawline(cprice,nlogsi(il,All),color->colors(il),width->2,append->,show->0)
	fi2=drawline(cprice,nlogs(il,All),color->colors(il),width->2,append->,show->0)
	;enddo
	
	show(fi1,yrange->(2100,2800),title->'Extra fig11 Assortment pricing',show->0,
	xlabel->'P_{chip}, €/m^3"font"',ylabel->'number of logs"font"')
	show(fi2,xlabel->'P_{chip}, €/m^3"font"',ylabel->'number of logs"font"',
yrange->(40,$),title->'Extra fig11 Free bucking"font"',show->0)


	fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',
	continue->cont0)
;return !fig11

;fig12:
!*** fig12:
;if(type(stump).ne.MATRIX)fp
;if(type(Stumpi).ne.MATRIX)asp
Window='450,450'

fi=0
ht=vector(62.8,63.3,  63.8)
;do(il,1,3)
fi=drawline(cprice,stumpi(il,All),append->,
color->colors(il),width->2,show->0)
fi=drawline(cprice,stump(il,All),append->,
color->colors(il),width->2,show->0)
fi=drawline(cprice,Stumpi(il,All),append->,
color->colors(il),width->2,show->0)
fi=drawline(cprice,Stump(il,All),append->,
color->colors(il),width->2,show->0)
fi=drawline(10,30,ht(il),ht(il),
color->colors(il),width->2,label->' P_{saw}= "psaws(il)""font"',append->,show->0)

;enddo
fi=drawline(5,68.9,label->'style(rotate by 12)Independent SM, free"font"',append->,show->0)
fi=drawline(120,68.56,label->'style(rotate by 18)Indep. SM as."font"',append->,show->0)
fi=drawline(120,68.1,label->'style(rotate by -10)SM with pulp, free"font"',append->,show->0)
fi=drawline(60,67.1,label->'style(rotate by -20)SM with pulp, as."font"',append->,show->0)
fi=drawline(-25,69.5,label->'€/m^3"font"',append->,show->0)

;if(tit);then
show(fi,xlabel->'P_{chip}, €/m^3"font"',title->'Figure 12 :Stumpage price"font"',continue->cont0)
;else
show(fi,xlabel->'P_{chip}, €/m^3"font"',title->'Stumpage price"font"',continue->cont0)
;endif

;return !fig12

;fig13:
!*** fig13
Window='450,450'
;if(type(obj).ne.MATRIX)fp

;if(type(Obji).ne.MATRIX)asp


ilo=Obji-obji
plo=Obj-obj
!ilo2=Obji-obji2
plo2=Obj-obj2
labxv=4
labd=20
labelhv=vector(-2.75,-2.5,-2.25)
fi=0
;do(il,1,3)
fi=drawline(cprice,ilo(il,All),color->colors(il),width->2,append->,show->0)
fi=drawline(cprice,plo(il,All),color->colors(il),width->2,append->,show->0)

!fil=drawline(cprice,ilo2(il,All),color->colors(il),width->4,append->,show->0)
!fil=drawline(cprice,plo2(il,All),color->colors(il),width->2,append->,show->0)

fi=drawline(labxv,labxv+labd,labelhv(il),labelhv(il),color->colors(il),width->3,append->,
label->' P_{saw}= "psaws(il)""font"',append->,show->0)
;enddo
 fi=drawline(-25,-0.25,label->'€/m^3"font"',append->,show->0)
 fi=drawline(2,-0.4,label->'SM pulp"font"',append->,show->0)
 fi=drawline(80,-0.7,label->'style(rotate by 45)Indep. SM"font"',append->,show->0)
;if(tit)show(fi,title->'Figure 13',show->0)
show(fi,continue->cont0,yrange->(-3,0),title->'Dead weight loss"font"',xlabel->'P_{chip}, €/m^3"font"')
;return !fig13

;fig14:
!*** fig14
;if(fdone.eq.0)fp  !lower case saaw u%saw saawu u%sawu saawi indepndent saawui u saawu
;if(aspdone.eq.0)asp  !upper case
 Window='450,450'
cee=100-saaw;
 ceei=veei-saawi; ! amount of chips 
 ceeitot=100-saawi;
 Cee=100-Saaw;
 Ceei=Veei-Saawi;
 Ceeitot=100-Saawi;
inc=cee(All,2,-lenc)-cee(All,1);  !increase of chips
Inc=Cee(All,2,-lenc)-Cee(All,1);  !increase of chips asp
inci=ceei(All,2,-lenc)-ceei(All,1);
Inci=Ceei(All,2,-lenc)-Ceei(All,1);
decval=-(saawu(All,2,-lenc)-saawu(All,1));
Decval=-(Saawu(All,2,-lenc)-Saawu(All,1));
decvali=-(saawui(All,2,-lenc)-saawui(All,1));
Decvali=-(Saawui(All,2,-lenc)-Saawui(All,1));
pdecval=psaws*.decval;
Pdecval=psaws*.Decval;
pdecvali=psaws*.decvali;
Pdecvali=psaws*.Decvali;

	! plog-ppulp net saving in price
Floss=(plog-ppulp)*(Vee(All,2,-lenc)-Vee(All,1));  ! ! Vee= 100 Vlog/vtot Vlog=0.01*Vtot
pf=pdecval/.inc;
Pf=(Pdecval+Floss)/.Inc;

pfi=pdecvali/.inci;
! Veei(All,2,-lenc)-Veei(All,1)) increase of log volume
Flossi=plog*(Veei(All,2,-lenc)-Veei(All,1));

! ob0=psaws*.Saawui(All,1)-plog*Veei(All,1);
! ob1=psaws*.Saawui(All,2)-plog*Veei(All,2);

Pfi=(Pdecvali+Flossi)/.Inci;

cprice2=cprice(2,-lenc)
ht=vector(200,180, 160)


pratio=Pf/.pf
iratio=Pfi/.pfi


Window='450,450'
fi1=0
fi2=0

HT=vector(0.8,0.7, 0.6)
;do(il,1,3)

fi1=drawline(cprice2,pf(il,All),color->colors(il),width->2,show->0,append->)
fi2=drawline(cprice2,pratio(il,All),color->colors(il),width->2,show->0,append->)
!fi2=drawline(cprice2,Pf(il,All),color->colors(il),width->2,show->0,append->)

fi1=drawline(cprice2,pfi(il,All),color->colors(il),append->,width->2,show->0,append->)
fi2=drawline(cprice2,iratio(il,All),color->colors(il),append->,width->2,show->0,append->)
!fi2=drawline(cprice2,Pfi(il,All),color->colors(il),append->,width->2,show->0,append->)

fi1=drawline(40,60,ht(il),ht(il),
color->colors(il),width->2,label->' P_{saw}= "psaws(il)""font"',append->,show->0)

fi2=drawline(30,50,HT(il),HT(il),
color->colors(il),width->2,label->' P_{saw}= "psaws(il)""font"',append->,show->0)

;enddo
fi1=drawline(0,250,0,250,width->2,append->,show->0)
fi2=drawline(140,0.48,label->'Sawmills with pulp"font"',append->,
continue->,show->0)
fi1=drawline(130,68,label->'style(rotate by 15)Independent sawmills"font"',append->,
continue->,show->0)

fi1=drawline(130,120,label->'style(rotate by 40)Sawmills with pulp"font"',append->,
continue->,show->0)
fi2=drawline(32,1.15,label->'style(rotate by 40)Independent sawmills"font"',append->,
continue->,show->0)

!fi2=drawline(0,250,0,250,width->2,append->,show->0)
fi1=drawline(-25,230,label->'€/m^3"font"',append->,show->0)
!fi2=drawline(-25,230,label->'€/m^3"font"',append->,show->0)
;if(tit);then
show(fi1,show->0,title->'Figure 14 Cost of chips, free bucking"font"',xlabel->'P_{chip}, €/m^3"font"',continue->)
show(fi2,show->0,title->'Assortment pricing / free bucking"font"',xlabel->'P_{chip}, €/m^3"font"',continue->)

;else
show(fi1,show->0,title->'Cost of chips, free bucking"font"',xlabel->'P_{chip}, €/m^3"font"',continue->)
show(fi2,show->0,title->'Assortment pricing / free bucking"font"',xlabel->'P_{chip}, €/m^3"font"',continue->)
;endif
!
 Window='900,450'
 fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',
 continue->cont0)
;return !fig14
;fig15b:
Window='450,450'
pratio=Pf/.pf
iratio=Pfi/.pfi
fi2=0
;do(il,1,3)

fi2=drawline(cprice2,iratio(il,All),color->colors(il),width->1,show->0,append->)
fi2=drawline(cprice2,pratio(il,All),color->colors(il),width->3,show->0,append->)
;enddo
show(fi2)
;return


fi=drawline(0,250,0,250,show->0)
;do(il,1,3)

fi=drawline(pf(il,All),Pf(il,All),color->colors(il),width->1,append->,show->0)
;enddo
;do(il,1,3)


fi=drawline(pfi(il,All),Pfi(il,All),color->colors(il),width->3,append->,show->0)

fi=drawline(50,70,150+il*10,150+il*10,color->colors(il),width->2,append->,show->0,
label->' P_{saw}= "psaws(il)""font"')

;enddo

show(fi,title->'assortment pricing',xlabel->'cost of chips in free bucking')

;return

;fig15old:
!*** fig15
onetwo
Window='450,450'
x106=top
x43=u%lens(1)
x44=x43+1
x52=U%lens(1)
x83=u%lencs(2)
lento=pinelentot(iobs);

dee=pinedee(iobs,1,-lento)
aa=dee*.dee
vee=pinevee(iobs,1,-lento)
aaf=pineaaf(iobs,1,-lento)
uaaf=pineuaaf(iobs,1,-lento)
saw_=matrix(1,lento)
sawu_=matrix(1,lento)
cyl_=matrix(1,lento);
saw2_=matrix(1,lento)
sawu2_=matrix(1,lento)
cyl2_=matrix(1,lento);
saw2=matrix(1,lento)
sawu2=matrix(1,lento)
cyl2=matrix(1,lento);
ho=matrix(lento,do->);
top=find(pinedee(iobs,All),filter->$.lt.dmin)-1
foo=drawline(ho,pinedee(iobs,1,-top),width->2,show->0)
!aa=pineaa(iobs,1,-lento)
vot=pinevee(iobs,top);

traf=trans()
do(i,1,lento)
Dtop=dee(i)
aaf(i)="Func"*aa(i)
uaaf(i)="Weight"*aaf(i)
cyl_(i)=aa(i)*i
saw_(i)=aaf(i)*i
sawu_(i)=uaaf(i)*i
enddo
hms=x43  !maxloc(saw_);
ms=saw_(hms)
! c*saw_(hms)=dee(hms)  =>c= dee(hms)/saw_(hms)
sawhms=saw_(hms)
csaw=(dee(hms)/sawhms)*saw_
csawu=(dee(hms)/sawu_(hms))*sawu_
cvol=(dee(hms)/vee(hms))*vee
ccyl=(dee(hms)/cyl_(hms))*cyl_
/

call(traf)

fi=drawline(ho,dee,show->0,width->3)
fi=drawline(ho,csaw,append->,show->0,color->Red,width->2)
fi=drawline(ho,csawu,append->,show->0,color->Orange,width->2)
fi=drawline(ho,ccyl,append->,show->0,color->Blue,width->2)
fi=drawline(ho,cvol,append->,continue->,color->Green,width->2,show->0,
xlabel->'L, dm"font"') !,ylabel->'D, cm"font"')
fi=drawline(-8,56,label->'D"font"',append->,show->0)
fi=drawline(-8,53,label->'cm"font"',append->,show->0)
cyl2_(1,x43)=cyl_(1,x43)
saw2_(1,x43)=saw_(1,x43)
sawu2_(1,x43)=sawu_(1,x43)
traf2=trans()
do(i,x44,x106)
cyl2_(i)=cyl2_(x43)+aa(i)*(i-x43)
saw2_(i)=saw2_(x43)+aaf(i)*(i-x43)
sawu2_(i)=sawu2_(x43)+uaaf(i)*(i-x43)
enddo
csaw2=(dee(hms)/sawhms)*saw2_
csawu2=(dee(hms)/sawu_(hms))*sawu2_
cyl2=(dee(hms)/cyl_(hms))*cyl2_
/

call(traf2)

fi=drawline(ho(x43,-x106),csaw2(x43,-x106),append->,show->0,color->Red,width->2)
fi=drawline(ho(x43,-x106),csawu2(x43,-x106),append->,show->0,color->Orange,width->2)
fi=drawline(ho(x43,-x106),cyl2(x43,-x106),append->,show->0,color->Blue,width->2)
fi=drawline(x43,x43,0,cvol(x43),width->3,color->Cyan,append->,show->0)
fi=drawline(x83,x83,0,cvol(x83),width->3,color->Cyan,append->,show->0)
fi=drawline(x52,x52,0,cvol(x52),width->3,color->Violet,append->,show->0)
fi=drawline(10,16,57,57,width->2,color->Black,append->,show->0,label->' D(L)"font"')
fi=drawline(10,16,54,54,width->2,color->Green,append->,show->0,label->' V_{log} or V_{com}"font"')
fi=drawline(10,16,51,51,width->2,color->Blue,append->,show->0,label->' V_{CYL}"font"')
fi=drawline(10,16,48,48,width->2,color->Red,append->,show->0,label->' V_{saw}"font"')
fi=drawline(10,16,45,45,width->2,color->Orange,append->,show->0,label->' V_{sawu}"font"')
topc=find(dee,filter->$.lt.dmin)-1;
fi=drawline(topc,topc,0,cvol(topc),width->3,color->Black,append->,show->0)
;if(tit);then
show(fi,continue->,title->'Figure 15 One or two logs?"font"',set->'bmargin 3.5',continue->cont0)
;else

show(fi,continue->,title->'One or two logs?"font"',set->'bmargin 3.5',continue->cont0)
;endif
! U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
! func->"Func",obj->(cpri*U%totc+psaw*U%sawu-plog*U%logv-ppulp*U%pulp))
!;current:

*** with pulp
stu=plog*U%logv+ppulp*U%pulp
U%nlog,U%logv,U%pulp,U%sawu,U%chip,U%totc,U%obj,stu;

! u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
! func->"Func",obj->(cpri*u%totc+psaw*u%sawu))

**wp 
stu=plog*u%logv+ppulp*u%pulp
u%nlog,u%logv,u%pulp,u%sawu,u%chip,u%totc,u%obj,stu;

! v=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
! func->"Func",obj->(cpri*v%chip+psaw*v%sawu-plog*v%logv))  !in
!top=find(pinedee(iobs,All),filter->$.le.0)-1
***inde asp

*** indefre
stu=plog*V%logv+ppulp*V%pulp
V%nlog,V%logv,V%pulp,V%sawu,V%chip,V%totc,V%obj,stu;

stu=plog*v%logv+ppulp*v%pulp
v%nlog,v%logv,v%pulp,v%sawu,v%chip,v%totc,v%obj,stu;
;return  !fig15

;fig15:
!*** fig16:
;if(vtot.le.0)comvtot
fi=0
colors=matrix(3,values->(Blue,Green,Red))
cprice=matrix(do->(0,250,10))
lenc=len(cprice)
Vee=matrix(3,lenc)
Saaw=matrix(3,lenc)
Saawu=matrix(3,lenc)
Stump=matrix(3,lenc)
Obj=matrix(3,lenc)
vee=matrix(3,lenc)
saaw=matrix(3,lenc)
saawu=matrix(3,lenc)
stump=matrix(3,lenc)
obj=matrix(3,lenc)
Veei=matrix(3,lenc)
Saawi=matrix(3,lenc)
Saawui=matrix(3,lenc)
Stumpi=matrix(3,lenc)
Obji=matrix(3,lenc)
veei=matrix(3,lenc)
saawi=matrix(3,lenc)
saawui=matrix(3,lenc)
stumpi=matrix(3,lenc)
obji=matrix(3,lenc)
Nlogs2=matrix(3,lenc)
!Nlogs2b=matrix(3,lenc)
nlogs2=matrix(3,lenc)
Nlogs2i=matrix(3,lenc)
nlogs2i=matrix(3,lenc)
Nlog2=matrix(3,lenc)
nlog2=matrix(3,lenc)
Nlog2i=matrix(3,lenc)
nlog2i=matrix(3,lenc)
 psaws=matrix(3,values->(180,220,260));

 unos=matrix(150)
 nunos=0

Lmin=40
Lmax=55
Dmin=15
dmin=5

plog=75
ppulp=25

lmax2=Lmax+Lmin
nobstot=0

trc21=trans()
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,2*Lmin).ge.Dmin.and.pinedee(iobs,lmax2).lt.Dmin)then
nobstot=nobstot+1
do(j,1,lenc)
cpri=cprice(j)
do(irow,1,3)
psaw=psaws(irow)
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*U%totc+psaw*U%sawu-plog*U%logv-ppulp*U%pulp))
Vee(irow,j)=Vee(irow,j)+U%logv
Saaw(irow,j)=Saaw(irow,j)+U%saw
Saawu(irow,j)=Saawu(irow,j)+U%sawu
Stump(irow,j)=Stump(irow,j)+plog*U%logv+ppulp*U%pulp
Obj(irow,j)=Obj(irow,j)+U%obj
Nlogs2(irow,j)=Nlogs2(irow,j)+(U%nlog.eq.2)
Nlog2(irow,j)=Nlog2(irow,j)+U%nlog

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*u%totc+psaw*u%sawu))
vee(irow,j)=vee(irow,j)+u%logv
saaw(irow,j)=saaw(irow,j)+u%saw
saawu(irow,j)=saawu(irow,j)+u%sawu
stump(irow,j)=stump(irow,j)+plog*u%logv+ppulp*u%pulp
obj(irow,j)=obj(irow,j)+u%obj
nlogs2(irow,j)=nlogs2(irow,j)+(u%nlog.eq.2)

V=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*V%chip+psaw*V%sawu-plog*V%logv))
Veei(irow,j)=Veei(irow,j)+V%logv
Saawi(irow,j)=Saawi(irow,j)+V%saw
Saawui(irow,j)=Saawui(irow,j)+V%sawu
Stumpi(irow,j)=Stumpi(irow,j)+plog*V%logv+ppulp*V%pulp
Obji(irow,j)=Obji(irow,j)+V%obj
Nlogs2i(irow,j)=Nlogs2i(irow,j)+(V%nlog.eq.2)

v=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*v%chip+psaw*v%sawu))
veei(irow,j)=veei(irow,j)+v%logv
saawi(irow,j)=saawi(irow,j)+v%saw
saawui(irow,j)=saawui(irow,j)+v%sawu
stumpi(irow,j)=stumpi(irow,j)+plog*v%logv+ppulp*v%pulp
obji(irow,j)=obji(irow,j)+v%obj
nlogs2i(irow,j)=nlogs2i(irow,j)+(v%nlog.eq.2)
if(V%nlog.eq.1)then
nunos=nunos+1
unos(nunos)=iobs
endif
enddo
enddo
endif
enddo
/

call(trc21)

Vee;
Saaw;  !matrix(3,lenc)
Saawu;  !matrix(3,lenc)
Stump;  !matrix(3,lenc)
Obj;  !matrix(3,lenc)
Veei;  !matrix(3,lenc)
Saawi;  !matrix(3,lenc)
Saawui;  !matrix(3,lenc)
Stumpi;  !matrix(3,lenc)
Obji;  !matrix(3,lenc)
NAI=Nlogs2i;
 colors=matrix(3,values->(Blue,Green,Red))
Nlogs2=100*Nlogs2/nobstot;  !matrix(3,lenc)

nlogs2=100*nlogs2/nobstot;  !matrix(3,lenc)
Nlogs2i=100*Nlogs2i/nobstot;  !matrix(3,lenc)
nlogs2i=100*nlogs2i/nobstot;  !matrix(3,lenc)
nobstot;
Nlog2/nobstot
nunos;
unos;

Window='450,450'
fu=0
ht=vector(70,50,30)
;do(il,1,3)

fi=drawline(cprice,200+nlogs2(il,All),color->colors(il),width->2,show->0,append->)
fi=drawline(cprice,100+Nlogs2(il,All),color->colors(il),width->2,show->0,append->)
fi=drawline(cprice,Nlogs2i(il,All),color->colors(il),width->2,show->0,append->)
fi=drawline(150,170,ht(il),ht(il),color->colors(il),width->2,show->0,append->,
label->' P_{saw}= "psaws(il)""font"')
;enddo
fi=drawline(cprice(1),cprice(lenc),100,100,append->,show->0,width->2)
fi=drawline(cprice(1),cprice(lenc),200,200,append->,show->0,width->2)
fi=drawline(5,240,label->'SM with pulp"font"',append->,show->0)
fi=drawline(5,220,label->'Free bucking"font"',append->,show->0)
fi=drawline(5,140,label->'SM with pulp"font"',append->,show->0)
fi=drawline(5,120,label->'Assortment pricing"font"',append->,show->0)
fi=drawline(5,40,label->'Independent SM"font"',append->,show->0)
fi=drawline(5,20,label->'Assortment pricing"font"',append->,show->0)

fi=drawline(-15,280,label->'%"font"',append->,show->0)
fi=drawline(-15,180,label->'%"font"',append->,show->0)
fi=drawline(-15,80,label->'%"font"',append->,show->0)
;if(tit);then
show(fi,set->'bmargin 3.5;ytics(\"0\" 0,\"50\" 50,\"100\" 100,\"50\" 150,\"100\" 200,\"50\" 250,\"100\" 300)',
yrange->(0,300),
xlabel->'P_{chip}, €/m^3"font"',title->'Figure 16 Share of two-log buckings"font"',continue->cont0)
;else

show(fi,set->'bmargin 3.5;ytics(\"0\" 0,\"50\" 50,\"100\" 100,\"50\" 150,\"100\" 200,\"50\" 250,\"100\" 300)',
yrange->(0,300),
xlabel->'P_{chip}, €/m^3"font"',title->'Share of two-log buckings"font"',continue->cont0)
;endif

;return  !fig16

;fig16:
!*** fig17:
;if(type(pine0).ne.DATA)luke
!also fig14
Window='450,450'
col=ilist(Red,Blue,Green)
fi1=0
fi2=0
tra=trans()
der(a,b,c)
f=a*(x+c)**b
der(A,B)
F=A*(exp(B*x)-1)
! dc=(a*(x+c+0.001)**b-f)/0.001
! da=((a+0.00001)*(x+c)**b-f)/0.00001
! db=(a*(x+c)**(b+0.00001)-f)/0.00001
!f\a,f\b;
/
datat0=list(pine0,spruce0,birch0)
;do(id,1,3)

trd=trans()
;do(di,5,15)
dif"di"=vtop4-vtop"di"
;enddo
/
stat(dif5...dif15,data->@datat0(id),mean->,sd->,filter->htop15.gt.26,trans->trd)
vc=varcomp(dif5...dif15,data->@datat0(id),class->plot,trans->trd,filter->htop15.gt.26)
stat(vtop4...vtop15,data->@datat0(id),mean->,sd->,filter->htop15.gt.26)
vc2=varcomp(vtop4...vtop15,data->@datat0(id),class->plot,filter->htop15.gt.26)
cum=0
!latv=vtop5%mean-vtop15%mean
;do(j,5,15)
cum=cum+vtop"j-1"%mean-vtop"j"%mean
tros"j"=cum

;enddo
;if(id.eq.1);then
fi2=0
fi2=drawline(4...15,0,dif5%mean...dif16%mean,color->Red,width->2,show->0)
;do(j,5,15)
fi2=drawline(j,j,dif"j"%mean-dif"j"%sd,dif"j"%mean+dif"j"%sd,width->2,color->Cyan,append->,show->0)
sb=sqrt(vc%varb(j-4))
dif"j"%mean,j,sb;
fi2=drawline(j,j,dif"j"%mean-sb,dif"j"%mean+sb,width->4,color->Blue,append->,show->0)
;enddo
fi2=drawline(6,57,label->'Pine"font"',append->,show->0)
fi2=drawline(5,6,50,50,label->' mean"font"',width->2,color->Red,append->,show->0)
fi2=drawline(5,6,45,45,label->' sd"font"',width->2,color->Cyan,append->,show->0)
fi2=drawline(5,6,40,40,label->' between stand deviation"font"',width->2,color->Blue,append->,show->0)
fi2=drawline(5,6,40,40,label->' between stand deviation"font"',width->2,color->Blue,append->,show->0)

show(fi2,show->0,xlabel->'D, cm"font"',ylabel->'V(4,D), dm^3"font"',xrange->(4,15.4))

;endif

fu=matrix(11,2)
;do(ii,1,11)
fu(ii,1)=ii
fu(ii,2)=tros"ii+4"
;enddo
fud=newdata(fu,read->(x,y),extra->(Regf,Resid))
fud%matrix;
stat()

B=0.1
A=1
re=regr(y,F,trans->tra)
A=coef(re,F)
nl=nonlin(y,F,par->(A,B),trans->tra,var->,corr->)

 fi1=draw(func->(A*(exp(B*(x-4))-1)),x->x,xrange->(4,15),append->,show->0)
fi1=drawline(4...15,0,tros5...tros15,width->2,color->col(id),
show->i0,append->)

;if(id.eq.1.and.finn)fi1=drawline(5,6,48,48,label->' pine 2.5837(e^{0.27260(D-4)}-1)"font"',
 show->0,color->col(id),width->2,append->)

;if(id.eq.1.and.eng)fi1=drawline(5,6,48,48,label->' Pine 2.5837(e^{0.27260(D-4)}-1)"font"',
 show->0,color->col(id),width->2,append->) 
 
 ;if(id.eq.2.and.finn)fi1=drawline(5,6,55,55,label->' spruce 4.0261(e^{0.24901(D-4)}-1)"font"',
 show->0,append->,color->col(id),width->2)
  ;if(id.eq.2.and.eng)fi1=drawline(5,6,55,55,label->' Spruce 4.0261(e^{0.24901(D-4)}-1)"font"',
 show->0,append->,color->col(id),width->2)
 
;if(id.eq.3.and.finn)fi1=drawline(5,6,62,62,label->' koivu 3.9682(e^{0.25750(D-4)}-1)"font"',
 show->0,append->,color->col(id),width->2)
 ;if(id.eq.3.and.eng)fi1=drawline(5,6,62,62,label->' Birch 3.9682(e^{0.25750(D-4)}-1)"font"',
 show->0,append->,color->col(id),width->2)
 
;enddo
show(fi1,show->0,xlabel->'D, cm"font"',ylabel->'V(4,D), dm^3"font"',xrange->(4,15.4),continue->)
;if(tit)show(fi1,title->'Figure 17 (left)',show->0)
;if(tit)show(fi2,title->'Figure 17 (right)',show->0)
Window='900,450'
 fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',
 continue->cont0)
;return



;return !fig17

;fig17:
!*** fig18
Window='450,450'
;if(type(pinedee).ne.MATRIX)rpine
Lmin=40
Lmax=55
Dmin=15
dmin=5
finmat=matrix(nrows(pinedee),11)   !   dbh,com,saw,logs,logmin,logmax

tranb=trans() !like trana in ana7
nob=0

do(iobs,1,nrows(pinedee))
if(pinedee(iobs,30).ge.dmin)then
nob=nob+1
finmat(iobs,1)=pinedee(iobs,13)
top=find(pinedee(iobs,All),filter->$.lt.dmin)-1
finmat(iobs,2)=pinevee(iobs,top) 
if(pinedee(iobs,Lmin).ge.Dmin)then

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->u%saw)
finmat(iobs,3)=u%saw
finmat(iobs,4)=u%logv
finmat(iobs,7)=100*u%logv/u%com
finmat(iobs,8)=100*u%saw/u%com

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(-u%logv))
finmat(iobs,5)=u%logv

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->u%logv)
finmat(iobs,6)=u%logv

endif
endif
enddo
/
call(tranb)
finmat(1,-nob,9,-11)=10*finmat(1,-nob,2,-4)
findat=newdata(finmat(1,-nob,All),
read->(dbh,com,saw,logs,logmin,logmax,logpros,sawpros,com10,saw10,logs10))

Window='450,450'
stat(min->,max->,mean->,data->findat)

stat(filter->(dbh.ge.(dbh%mean-1).and.dbh.le.(dbh%mean+1)),mean->)
com%mean,saw%mean,saw%mean/com%mean;

logperc=100*logs%mean/com%mean
sawperc=100*saw%mean/com%mean
logpros%mean,logperc,sawpros%mean,sawperc;
fi1=0
cl=classify(com,x->dbh,xrange->,dx->2,minobs->7,data->findat)
fi1=drawclass(cl,color->Blue,width->2,show->0,append->)

cl=classify(saw,x->dbh,xrange->,dx->2,minobs->7,data->findat)
fi1=drawclass(cl,color->Red,se->,sd->,width->(1,2),show->0,append->)

cl=classify(logs,x->dbh,xrange->,dx->2,minobs->7,data->findat)
fi1=drawclass(cl,color->Orange,width->2,show->0,append->)

cl=classify(logmin,x->dbh,xrange->,dx->2,minobs->7,data->findat)
fi1=drawclass(cl,color->Cyan,width->2,show->0,append->)

cl=classify(logmax,x->dbh,xrange->,dx->2,minobs->7,data->findat)
fi1=drawclass(cl,color->Cyan,width->2,show->0,append->)

fi1=drawline(5,10,1600,1600,label->' V_{com}"font"',append->,show->0,width->2,color->Blue)
fi1=drawline(5,10,1450,1450,label->' V_{log}"font"',append->,show->0,width->2,color->Orange)
fi1=drawline(5,10,1300,1300,label->' V_{log}, max V_{log} or min V_{log}"font"',
append->,show->0,width->2,color->Cyan)
fi1=drawline(5,10,1150,1150,label->' V_{saw}"font"',append->,show->0,width->2,color->Red)
fi1=drawline(-5.7,1700,label->' dm^3"font"',append->,show->0)

show(fi1,show->0,yrange->(0,$),xlabel->'dbh, cm^ "font"',continue->)



fi2=0


cl=classify(saw,x->com,xrange->,classes->25,minobs->7,data->findat)
fi2=drawclass(cl,color->Red,se->,sd->,width->(1,2),show->0,append->)
re=regr(saw,com,data->findat)
fi2=drawline(com%min,com%max,re(com%min),re(com%max),show->0,append->)

re=regr(logs,com,data->findat)
fi2=drawline(com%min,com%max,re(com%min),re(com%max),show->0,append->)

cl=classify(logs,x->com,xrange->,classes->25,minobs->7,data->findat)
fi2=drawclass(cl,color->Orange,width->2,show->0,append->)

cl=classify(logmin,x->com,xrange->,classes->25,minobs->7,data->findat)
fi2=drawclass(cl,color->Cyan,width->2,show->0,append->)


cl=classify(logmax,x->com,xrange->,classes->25,minobs->7,data->findat)
fi2=drawclass(cl,color->Cyan,width->2,show->0,append->)

cl=classify(logs10,x->com10,xrange->,classes->250,minobs->7,data->findat)
fi2=drawclass(cl,color->Orange,width->2,show->0,append->)

cl=classify(saw10,x->com10,xrange->,classes->250,minobs->7,data->findat)
fi2=drawclass(cl,color->Red,width->2,show->0,append->)

fi2=drawline(0,250,250,250,1600,250,250,0,50,300,show->0,append->,
label->'  zoom 1000%"font"')
!fi2=drawline(790,345,label->'zoom 1000%"font"',show->0,append->)
fi2=drawline(-230,2200,label->'dm^3"font"',show->0,append->)


fi2=drawline(100,300,2000,2000,label->' V_{log}"font"',append->,show->0,width->2,color->Orange)
fi2=drawline(100,300,0.5*(1600+2000),0.5*(1600+2000),label->' V_{log}, max V_{log} or min V_{log}"font"',
append->,show->0,width->2,color->Cyan)
fi2=drawline(100,300,1600,1600,label->' V_{saw}"font"',append->,show->0,width->2,color->Red)


show(fi2,show->0,xlabel->'V_{com}, dm^3"font"',continue->,xrange->(0,2500),yrange->(0,2500))
Window='900,450'
;if(tit)show(fi1,title->'Figure 18 (left)',show->0)
;if(tit)show(fi2,title->'Figure 18 (right)',show->0)
fi=show(fi1,fi2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',
continue->)

;return !fig18

****needed  bark
;luke0:
diam=list(d1...d14)   ! diamters measured at relative heights
pros=matrix(1,14,in->) ! percentages at which daimters are measured  !pons.inc
1,2.5,5,7.5,10,15,20,30,40,50,60,70,80,90
/
ht=list(h1...h14) ! height where diamters were measured
 pi4=Pi/400
rtr0=trans()
kanto=max(0.2,ero)
plot=1000*$tr+$koe
;do(j,1,14)
h"j"=0.1*pros("j")*h
;enddo

! h=10*h
! !@diam;
! kanto=10*kanto
! $calle=stemcurve(@ht,h,@diam,0.4)
! dbh2=$calle(13+kanto)
! Vtot=$calle(kanto,h)
! hmer=$calle(-Dpmin)
! merv=0
! if(hmer.gt.kanto)merv=$calle(kanto,hmer)
! hmer14=$calle(-14)
! merv14=0
! if(hmer14.gt.kanto)merv14=$calle(kanto,hmer14)
! !@diam;
! !$calle2=stemcurve(@ht,h,@diam,0.4,print->)
! !dbh22=$calle2(1.3+kanto);
!kanto;
!h;
!Vtot=$calle2(kanto,h);
! ;do(i,4,16)
! htop"i"=$calle(-"i")
! !Htop"i"=$calle2(-"i");
! vtop"i"=0
! if(htop"i".gt.kanto)vtop"i"=$calle(kanto,htop"i")
! !htop"i";
! !if(htop"i".gt.kanto)Vtop"i"=$calle2(kanto,htop"i");
! ;enddo
;do(i,1,14)
dw"i"=d"i"-b"i"
a"i"=pi4*d"i"*d"i"
aw"i"=pi4*dw"i"*dw"i"
;enddo
/
readv0=list($tr,$koe,$t,$t,$t,$t,$t,$t,$t,$t,ba,$t,$t,$t,$t,$t,
$mage,$t,$t,$t,$t,$t,lat,$t,$t,$t,$t,$t,$t,$t,$t,
$age,$t,$t,lara,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,h,
dbh,$t,$t,$t,dd6,$t,$t,ero,$t,v,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,
d1...d13,$t,d14,$t,b1...b13,$t,b14,$t)  !$t,
!$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t) 

** original data of Luke
pine00=data(read->(@readv0),in->'MANTY.TXT',
maketrans->rtr0)   !,reject->(htop5.lt.26))
stat()
;return
;lukebark:
;if(type(pine0).ne.DATA)luke
cls
bdatat=list(pine0,spruce0)
;do(id,1,2)
mink=0
;do(k,6,13)
k;
trr=trans()
if(dw"k".lt.14.or.h"k".lt.37)then
rej=1
else
Dtop=dw"k"
wei="Weight"
func="Func"
rej=0
saw=func*aw"k"
sawu=wei*saw
Dtop=d"k"
wei0="Weight"
func0="Func"
saw0=func0*a"k"
sawu0=wei0*saw0
sawrat=saw/saw0
sawurat=sawu/sawu0
endif
/

newd"k"=newdata(@bdatat(id),maketrans->trr,reject->rej,keep->(saw,saw0,sawu,sawu0,d"k"))


k,newd"k"%obs;
;if(newd"k"%obs.gt.0);then
;if(mink.eq.0)mink=k;
maxk=k;
! stat()
! fi=plotyx(saw,saw0,title->'saw/saw0')
! fi=plotyx(sawu,sawu0,title->'sawu/sawu0')
! fi=plotyx(sawrat,d"k",title->'saw/saw0 vs d')
! fi=plotyx(sawurat,d"k",title->'sawu/sawu0 vs d')
!regr(saw,saw0,d"k")
!regr(sawu,sawu0,d"k")
!regr(saw,saw0,noint->)
!regr(sawu,sawu0,noint->)

;endif
;enddo
mink,maxk;
nmat=maxk-mink+1;
totm=matrix(nmat,1,values->(newd"mink"%matrix...newd"maxk"%matrix))
findat=newdata(totm,read->(@newd"mink"%keep))
regr(saw,saw0,d"k")
regr(sawu,sawu0,d"k")
regr(saw,saw0,noint->)
regr(sawu,sawu0,noint->)
id;
;pause('bark')
;enddo


;return



;luke:
!*** luke:
diam=list(d1...d14)   ! diamters measured at relative heights
pros=matrix(1,14,in->) ! percentages at which daimters are measured  !pons.inc
1,2.5,5,7.5,10,15,20,30,40,50,60,70,80,90
/
ht=list(h1...h14) ! height where diamters were measured

rtr0=trans()
kanto=max(0.2,ero)
plot=1000*$tr+$koe
;do(j,1,14)
h"j"=0.1*pros("j")*h
;enddo

! h=10*h
! !@diam;
! kanto=10*kanto
! $calle=stemcurve(@ht,h,@diam,0.4)
! dbh2=$calle(13+kanto)
! Vtot=$calle(kanto,h)
! hmer=$calle(-Dpmin)
! merv=0
! if(hmer.gt.kanto)merv=$calle(kanto,hmer)
! hmer14=$calle(-14)
! merv14=0
! if(hmer14.gt.kanto)merv14=$calle(kanto,hmer14)
! !@diam;
! !$calle2=stemcurve(@ht,h,@diam,0.4,print->)
! !dbh22=$calle2(1.3+kanto);
!kanto;
!h;
!Vtot=$calle2(kanto,h);
! ;do(i,4,16)
! htop"i"=$calle(-"i")
! !Htop"i"=$calle2(-"i");
! vtop"i"=0
! if(htop"i".gt.kanto)vtop"i"=$calle(kanto,htop"i")
! !htop"i";
! !if(htop"i".gt.kanto)Vtop"i"=$calle2(kanto,htop"i");
! ;enddo
;do(i,1,14)
dw"i"=d"i"-b"i"
a"i"=pi4*d"i"*d"i"
aw"i"=pi4*dw"i"*dw"i"
;enddo
/

rtrb=trans()  !without bark
kanto=max(0.2,ero)
plot=1000*$tr+$koe
;do(j,1,14)
h"j"=0.1*pros("j")*h
;enddo

h=10*h
!@diam;
kanto=10*kanto
$calle=stemcurve(@ht,h,@diam,0.4)
dbh2=$calle(13+kanto)
Vtot=$calle(kanto,h)
hmer=$calle(-Dpmin)
merv=0
if(hmer.gt.kanto)merv=$calle(kanto,hmer)
hmer14=$calle(-14)
merv14=0
if(hmer14.gt.kanto)merv14=$calle(kanto,hmer14)
!@diam;
!$calle2=stemcurve(@ht,h,@diam,0.4,print->)
!dbh22=$calle2(1.3+kanto);
!kanto;
!h;
!Vtot=$calle2(kanto,h);
;do(i,4,16)
htop"i"=$calle(-"i")
!Htop"i"=$calle2(-"i");
vtop"i"=0
if(htop"i".gt.kanto)vtop"i"=$calle(kanto,htop"i")
!htop"i";
!if(htop"i".gt.kanto)Vtop"i"=$calle2(kanto,htop"i");
;enddo
! ;do(i,1,14)
! dw"i"=d"i"-b"i"
! a"i"=pi4*d"i"*d"i"
! aw"i"=pi4*dw"i"*dw"i"
! ;enddo
/

birch0=data(in->'koivul2.asc',read->($tr,$koe,ko3...ko12,h,dbh,ko15...ko20,ero,ko22...ko26,
d1...d13,ko40,d14,ko42),keep->(plot,h,dbh,dbh2,ero,d1...d13,d14,@rtrb%output,Regf,Resid,Cregf,Cresid),
maketrans->rtrb)  !,reject->(htop5.lt.26))

stat()
delete_o(v)
readv=list($tr,$koe,$t,$t,$t,$t,$t,$t,$t,$t,ba,$t,$t,$t,$t,$t,
$mage,$t,$t,$t,$t,$t,lat,$t,$t,$t,$t,$t,$t,$t,$t,
$age,$t,$t,lara,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,h,
dbh,$t,$t,$t,dd6,$t,$t,ero,$t,v,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,
d1...d13,$t,d14,$t,
b1...b13,$t,b14,$t)
!$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t) 

rtr=trans()  !with bark
kanto=max(0.2,ero)
plot=1000*$tr+$koe
;do(j,1,14)
h"j"=0.1*pros("j")*h
;enddo

h=10*h
!@diam;
kanto=10*kanto
$calle=stemcurve(@ht,h,@diam,0.4)
dbh2=$calle(13+kanto)
Vtot=$calle(kanto,h)
hmer=$calle(-Dpmin)
merv=0
if(hmer.gt.kanto)merv=$calle(kanto,hmer)
hmer14=$calle(-14)
merv14=0
if(hmer14.gt.kanto)merv14=$calle(kanto,hmer14)
!@diam;
!$calle2=stemcurve(@ht,h,@diam,0.4,print->)
!dbh22=$calle2(1.3+kanto);
!kanto;
!h;
!Vtot=$calle2(kanto,h);
;do(i,4,16)
htop"i"=$calle(-"i")
!Htop"i"=$calle2(-"i");
vtop"i"=0
if(htop"i".gt.kanto)vtop"i"=$calle(kanto,htop"i")
!htop"i";
!if(htop"i".gt.kanto)Vtop"i"=$calle2(kanto,htop"i");
;enddo
;do(i,1,14)
dw"i"=d"i"-b"i"
a"i"=pi4*d"i"*d"i"
aw"i"=pi4*dw"i"*dw"i"
;enddo
/




** original data of Luke
readv;
pine0=data(read->(@readv),in->'MANTY.TXT',keep->(plot,h,dbh,dbh2,ero,d1...d13,d14,@rtr%output,Regf,Resid,Cregf,Cresid,
b1...b14),
maketrans->rtr,reject->(htop5.lt.26))
stat(max->'%pine')


!;return

readv=list($tr,$koe,$t,$t,$t,$t,$t,$t,$t,$t,ba,$t,$t,$t,$t,$t,
mage,$t,$t,$t,$t,$t,lat,$t,$t,$t,$t,$t,$t,$t,$t,
$age,$t,$t,lara,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,$t,h,
dbh,$bbh,$t,$t,dd6,$bd6,$t,ero,$t,v,vi,$t,$t,$t,$t,$t,$dero,$bero,$t,$t,$t,$t,$pv,$kk,$t,
d1...d13,$t,d14,$t,
b1...b13,$t,b14,$t,$t,$t,$t,$t,$t) 

spruce0=data(read->(@readv),in->'kuusip.asc',keep->(plot,h,dbh,dbh2,ero,d1...d13,d14,@rtr%output,Regf,Resid,Cregf,Cresid,
b1...b14),
form->'(53f5.0,10f5.1,7f10.7,4f5.1,5f2.0,32f5.1,2f4.0,2f10.7,f4.0)',
maketrans->rtr,reject->(htop5.lt.26))
stat(max->'%spruce')
datat0=list(pine0,spruce0,birch0)

! luketoharv=ask('is harvester looking data made (1/0)>')
;if(.not.extra0);return
datat=list(pine,spruce,birch)

**makes data which looks like harvester data
**makes data which looks like harvester data
** this data is written to disk and can accessed later using rdata section

dee0=matrix(350)

trharv=trans()
$calle=stemcurve(@ht,h,@diam,0.4) 
lento=int(h-kanto)+1
dee0=0
aa0=0
vee0=0
i1=0
old=0
dkanto=$calle(kanto)
dbottom=$calle(0)
!if(Obs.eq.30)Obs,dkanto,kanto;
ddown=dkanto
adown=ddown*ddown

do(i,1,lento)
dee0(i)=$calle(kanto+i)
enddo

write(master,$,plot,lento,dbottom,kanto,dkanto)
write(deedat,'b',dee0)
/


;do(id,1,3)
deedat='@datat(id)dee.bin';
master='@datat(id)new.txt';
;if(exist_f(deedat))delete_f(deedat)
;if(exist_f(master))delete_f(master)
iobs=0
maxdif=0
transdata(data->@datat(id)0,trans->trharv)
close(deedat,master)
!after writing stem vector 

@datat(id)dee=data(read->(x1...x350),form->'b',in->deedat)
stat(x1...x30)
;enddo

;return !luke

;rpine:
!*** rpine:
;if(exist_f('pinenew.txt').eq.0)luke
ndat=1

;rdata:
!*** rdata:
!!this section reads lukes 'harvester' data
datat=list(pine,spruce,birch)

;if(ndat.eq.0)ndat=3
!vars=list(dee)

;do(id,1,ndat)
@datat(id)new=data(read->(plot,lento,dbottom,kanto,dkanto),in->'@datat(id)new.txt')
stat()
!keep datas in matrix form

!@datat(id)%"j"=data(read->(x1...x350),form->'b',in->'@vars(j)@datat(id).bin')
!stat(x1...x30)
nob=nobs(@datat(id)new)
@datat(id)dee=matrix(nob,350)
read('@datat(id)dee.bin','b',@datat(id)dee)
close('@datat(id)dee.bin')
@datat(id)aaf=matrix(nob,350)
@datat(id)aa=matrix(nob,350)
@datat(id)vee=matrix(nob,350)
@datat(id)uaaf=matrix(nob,350)
@datat(id)lentot=matrix(nob)

tra=trans()
do(i,1,nob)
ve=0
do(j,1,350)
Dtop=@datat(id)dee(i,j)
if(Dtop.le.0)then
@datat(id)lentot(i)=j-1
goto(next)
endif
aa=Dtop*Dtop

@datat(id)aa(i,j)=aa

ve=ve+aa
@datat(id)vee(i,j)=ve
!pause('<>')
@datat(id)aaf(i,j)="Func"*aa
@datat(id)uaaf(i,j)="Weight"*aa
enddo

next:enddo
@datat(id)vee=Pi400*@datat(id)vee
@datat(id)aa=Pi400*@datat(id)aa
/
call(tra)

;enddo

;return !rdata !rpine

;scale:
;if(type(pinedee).ne.MATRIX)rpine
!scale of 
Lmin=40
Lmax=55
Dmin=15
dmin=5
transc=trans()
nob=0
saw=0
sawu=0
do(iobs,1,nrows(pinedee))

if(pinedee(iobs,Lmin).ge.Dmin)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",weight->"Weight",obj->U%saw)
nob=nob+1
saw=saw+U%saw
sawu=sawu+U%sawu
endif
enddo
/
call(transc)
saw;
sawu;
coeff=saw/sawu;


;return

;deftrans:
!*** deftrans:
Obj;
tranco=trans()
saw=0
vol=0
sawu=0
do(iobs,1,nrows(pinedee))

if(pinedee(iobs,Lmin).ge.Dmin)then
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,func->"Func",weight->"Weight",
obj->"Obj")
vol=vol+u%logv
saw=saw+u%saw
sawu=sawu+u%sawu
endif
enddo
/
!;pause('per')

;return !deftrans

;fig70:
!*** fig70
;if(type(pinedee).ne.MATRIX)rpine

**logwise results for 40 dm logs
Dmin=15
Lmin=40
loglist0=list(Cyl,Saw,Inc,Outc,Chip,Sawu)
loglistp=list(Cylp,Sawp,Incp,Outcp,Chipp,Sawup)
loglist=list(Dtop,Logv,f,@loglist0,@loglistp)
vec=matrix(len(loglist0))

tralog=trans()
ntree=nrows(pinedee)  !diamters
nlogs=0
do(it,1,ntree)
top=find(pinedee(it,All),filter->$.lt.Dmin)-1
nlogs=nlogs+int(top/Lmin)
enddo

logmat=matrix(nlogs,len(loglist))
nob=0
do(it,1,ntree)
top=find(pinedee(it,All),filter->$.lt.Dmin)-1
nlog=int(top/Lmin)
lenc=0
volc=0

do(ilog,1,nlog)
lenc=lenc+Lmin
Logv=pinevee(it,lenc)-volc
volc=volc+Logv
Cyl=pineaa(it,lenc)*Lmin  !pineaa areas
Dtop=pinedee(it,lenc)
f="Func"   !1-bf*gam**Dtop
Saw=f*Cyl
Sawu="Weight"*Saw
Inc=Cyl-Saw
Outc=Logv-Cyl
Chip=Logv-Saw
nob=nob+1
vec=loglist0
vec2=100*vec/Logv
loglistp=vec2
logmat(nob,All)=loglist
!@loglist;
!pause('<>')
enddo
enddo
/
call(tralog)
datp=newdata(logmat,read->@loglist)

stat(min->,max->)
;return !fig70

;compare:
!*** compare:
dmin=5
;if(type(pinedee).ne.MATRIX)rpine
;if(volcom.le.0)volcomp

Maxlines=23
lmin=37
lmax=55
e=0.000001
lmins=matrix(2,values->(37,43))  !40,43))
lmaxs=matrix(2,values->(52,58))  !55,58))
dmins=matrix(2,values->(14,16)) !15,16))

objs='u%saw;u%sawu;(u%saw/u%logv);(-u%logv-e*u%saw);>>
(-u%logv+e*u%saw);(u%logv-e*u%saw);(u%logv+e*u%saw)'

! resu=matrix(9,10)
! revu=matrix(9,10)
! reva=matrix(9,10)

resu=matrix(9,24)
iclmi=22
iclma=23
icdmi=24

;do(iob,1,7)
Obj=objs(iob,';');
deftrans

irow=0
;do(mi,1,2)
Lmin=lmins(mi);
;do(ma,1,2)
Lmax=lmaxs(ma);
;do(idmi,1,2)
Dmin=dmins(idmi);
irow=irow+1
delete_o(saw,vol,sawu)
call(tranco)
resu(irow,iob)=100*saw/volcom
resu(irow,7+iob)=100*vol/volcom
resu(irow,14+iob)=100*sawu/volcom

resu(irow,iclmi)=Lmin
resu(irow,iclma)=Lmax
resu(irow,icdmi)=Dmin

;enddo
;enddo
;enddo
Lmin=40
Lmax=55
Dmin=15
call(tranco)
resu(9,iob)=100*saw/volcom
resu(9,iob+7)=100*vol/volcom
resu(9,iob+14)=100*sawu/volcom

;enddo
resu(9,iclmi)=40
resu(9,iclma)=55
resu(9,icdmi)=15

sort(resu,key->1)

resu2=resu(All,1,-7);
revu2=resu(All,8,-14);
reva2=resu(All,15,-21);
dim=resu(All,22,-24);
comparedone=1

;return !compare

;volcomp:
!*** volcomp:
tran0=trans()
volcom=0
do(iobs,1,nrows(pinedee))
if(pinedee(iobs,37).ge.14)then
top=find(pinedee(iobs,All),filter->$.lt.dmin)
volcom=volcom+pinevee(iobs,top-1)  !commercial volume
endif
enddo
/
call(tran0)
volcom;
;return !volcom

;asp:
!*** ;asp:  assortment pricing computations
;if(vtot.le.0)comvtot
cprice=matrix(do->(0,250,10))
lenc=len(cprice)
colors=matrix(3,values->(Blue,Green,Red))

Vee=matrix(3,lenc)
Saaw=matrix(3,lenc)
Saawu=matrix(3,lenc)
Stump=matrix(3,lenc)
Obj=matrix(3,lenc)
Veei=matrix(3,lenc)
Saawi=matrix(3,lenc)
Saawui=matrix(3,lenc)
Stumpi=matrix(3,lenc)
Obji=matrix(3,lenc)
psaws=matrix(3,values->(180,220,260));

Lmin=40
Lmax=55
Dmin=15
dmin=5
!psaw=220
plog=75
ppulp=25
trc=trans()
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,Lmin).ge.Dmin)then
iro=0

do(irow,1,3)
psaw=psaws(irow)

do(j,1,lenc)

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cprice(j)*u%totc+psaw*u%sawu-plog*u%logv-ppulp*u%pulp))
Vee(irow,j)=Vee(irow,j)+u%logv
Saaw(irow,j)=Saaw(irow,j)+u%saw
Saawu(irow,j)=Saawu(irow,j)+u%sawu
Stump(irow,j)=Stump(irow,j)+plog*u%logv+ppulp*u%pulp
Obj(irow,j)=Obj(irow,j)+cprice(j)*u%totc+psaw*u%sawu

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cprice(j)*u%chip+psaw*u%sawu-plog*u%logv))
Veei(irow,j)=Veei(irow,j)+u%logv
Saawi(irow,j)=Saawi(irow,j)+u%saw
Saawui(irow,j)=Saawui(irow,j)+u%sawu
Stumpi(irow,j)=Stumpi(irow,j)+plog*u%logv+ppulp*u%pulp
Obji(irow,j)=Obji(irow,j)+cprice(j)*u%chip+psaw*u%sawu
enddo
enddo
endif
enddo
/
call(trc)
Vee=100.*Vee/vtot;
 Saaw=100*Saaw/vtot;
 Saawu=100*Saawu/vtot;
Stump=Stump/vtot;

Veei=100.*Veei/vtot;
 Saawi=100*Saawi/vtot;
 Saawui=100*Saawui/vtot;
Stumpi=Stumpi/vtot;
Obj=Obj/vtot
Obji=Obji/vtot
aspdone=1
;return !asp

;comvtot:
;if(type(pinedee).ne.MATRIX)rpine

*** ;comvtot:

Lmin=40
Lmax=55
Dmin=15
dmin=5

trani0=trans() !like trana in ana7

do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,40).ge.Dmin)then

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",obj->u%saw)   !,any->)
vtot=vtot+u%com
sawmax=sawmax+u%saw
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",obj->(-u%logv))   !,any->)
logmin=logmin+u%logv
sawmin=sawmin+u%saw
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",obj->u%logv)   !,any->)
logmax=logmax+u%logv

endif

enddo
/
vtot=0
logmin=0
sawmin=0
logmax=0
sawmax=0
call(trani0)
logmin=100*logmin/vtot;
sawmin=100*sawmin/vtot;
logmax=100*logmax/vtot;
sawmax=100*sawmax/vtot;
vtot;
;return !comvtot


;fp:
! free bucking
*** fp:
;if(vtot.le.0)comvtot
fdone=1
cprice=matrix(do->(0,250,10))
lenc=len(cprice)
colors=matrix(3,values->(Blue,Green,Red))

vee=matrix(3,lenc)
saaw=matrix(3,lenc)
saawu=matrix(3,lenc)
stump=matrix(3,lenc)
veei=matrix(3,lenc)
obj=matrix(3,lenc)
saawi=matrix(3,lenc)
saawui=matrix(3,lenc)
stumpi=matrix(3,lenc)
obji=matrix(3,lenc)
psaws=matrix(3,values->(180,220,260));

Lmin=40
Lmax=55
Dmin=15
dmin=5
!psaw=220
plog=75
ppulp=25
trc=trans()
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,Lmin).ge.Dmin)then
iro=0

do(irow,1,3)
psaw=psaws(irow)

do(j,1,lenc)

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, func->"Func",obj->u%saw)


ohi:
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",any->isany,
func->"Func",obj->(cprice(j)*u%totc+psaw*u%sawu))
vee(irow,j)=vee(irow,j)+u%logv
saaw(irow,j)=saaw(irow,j)+u%saw
saawu(irow,j)=saawu(irow,j)+u%sawu 
stump(irow,j)=stump(irow,j)+plog*u%logv+ppulp*u%pulp
obj(irow,j)=obj(irow,j)+u%obj

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",any->isany,
func->"Func",obj->(cprice(j)*u%chip+psaw*u%sawu))
veei(irow,j)=veei(irow,j)+u%logv
saawi(irow,j)=saawi(irow,j)+u%saw
saawui(irow,j)=saawui(irow,j)+u%sawu
stumpi(irow,j)=stumpi(irow,j)+plog*u%logv+ppulp*u%pulp
obji(irow,j)=obji(irow,j)+u%obj
enddo
enddo
endif
enddo
/
isany=1
call(trc)

vee2=100.*vee/vtot;
 saaw2=100*saaw/vtot;
 saawu2=100*saawu/vtot;
stump2=stump/vtot;
obj2=obj/vtot;
veei2=100.*veei/vtot;
 saawi2=100*saawi/vtot;
 saawui2=100*saawui/vtot;
stumpi2=stumpi/vtot;
obji2=obji/vtot
isany=0
vee=0
saaw=0
saawu=0
stump=0
obj=0
veei=0
saawi=0
saawui=0
stumpi=0
obji=0
call(trc)
vee=100.*vee/vtot;
 saaw=100*saaw/vtot;
 saawu=100*saawu/vtot;
stump=stump/vtot;
obj=obj/vtot;
veei=100.*veei/vtot;
 saawi=100*saawi/vtot;
 saawui=100*saawui/vtot;
stumpi=stumpi/vtot;
obji=obji/vtot
;return !fp

;onetwo:
!*** onetwo:
!!*one or two logs example
;if(type(pinedee).ne.MATRIX)rpine
Lmin=40
Lmax=55
Dmin=15
dmin=5
!! ;if(vtot.le.0)comvtot

Window='450,450'
cpri=30;
psaw=180;
iobs=1829
 !iobs=1592
plog=75
ppulp=25
 
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*U%totc+psaw*U%sawu-plog*U%logv-ppulp*U%pulp))
vto=U%com;
!***assortment pricing sawmill with pulp
U='U'
printonetwo


u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*u%totc+psaw*u%sawu))
!*** free SM with pulp
U='u'
printonetwo

V=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*V%chip+psaw*V%sawu-plog*V%logv))  !independetn asp
top=find(pinedee(iobs,All),filter->$.le.0)-1

!*** assortment pricing  indep SM 
!
U='V'
printonetwo

v=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*V%chip+psaw*v%sawu))
!*** free pricing  indep SM 

U='v'
printonetwo
;return !onetwo

;printonetwo:
!*** printonetwo:
!**logs
"U"%nlog;
Ufi=cpri*"U"%chip+psaw*"U"%sawu
Ufp=cpri*"U"%totc+psaw*"U"%sawu
Uai=Ufi-plog*"U"%logv
pstump=plog*"U"%logv+ppulp*"U"%pulp
Uap=Ufp-pstump
pstump=pstump/"U"%com

!** vlog,vpulp,vsawu,vchip,vtotc,pstum,Ufi,Ufp,Uai,Uap
"U"%logv,"U"%pulp,"U"%sawu,"U"%chip,"U"%totc,pstump,Ufi,Ufp,Uai,Uap;
write($,'(5f5.0,f5.1,2f8.0,2f8.0)',"U"%logv,"U"%pulp,"U"%sawu,"U"%chip,"U"%totc,pstump,Ufi,Ufp,Uai,Uap)
;return !printonetwo

;scaleu:
!*** scaleu:  scales weight
;if(type(pinedee).ne.MATRIX)rpine
Lmin=40
Lmax=55
Dmin=15
dmin=5

transc=trans()
sawu=0
saw=0
do(iobs,1,nrows(pinedee))
if(pinedee(iobs,Lmin).ge.Dmin)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",weight->"Weight",obj->U%sawu)
sawu=sawu+U%sawu
saw=saw+U%saw

endif
enddo
/
call(transc)
saw,sawu;
coefu= saw/sawu;  !coef*sawu=saw
y1,y2;
yy1=y1*coefu;
yy2=y2*coefu;
;return !scaleu

;rdataharv:
!*** rdataharv:
!!*this section reads lukes 'harvester' data
datat=list(pine,spruce,birch)
;do(id,1,3)
lukeharv
!@datat(id)%"j"=data(read->(x1...x350),form->'b',in->'@vars(j)@datat(id).bin')
!stat(x1...x30)
@datat(id)deeh=matrix(Inf,350,in->'@datat(id)deeh.bin',form->'b')
nro=nrows(@datat(id)deeh);
!nroold=nrows(@datat(id)dee);
;enddo

**this can be used when making 
hoo=matrix(350,do->)
;return !rdataharv

;cpuu:
!*** cpuu: cpu comparisons
Lmin=40
Lmax=55
Dmin=15
dmin=5
Psaw=220
Pchip=35

trancpu=trans()
nob=0
Saw=0
Chip=0
Logv=0
delete_o(saw)
saw=0
chip=0
logv=0
do(iobs,1,nrows(pinedee))
dbh=pinedee(iobs,13)
if(pinedee(iobs,Lmin).ge.Dmin)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->U%saw)
nob=nob+1
Saw=Saw+U%saw
Chip=Chip+U%chip
Logv=Logv+U%logv
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(Psaw*u%saw+Pchip*u%chip))
saw=saw+u%saw
chip=chip+u%chip
logv=logv+u%logv
endif
enddo
/
cpu0=cpu()
call(trancpu)
cpu1=cpu()

cputot=cpu1-cpu0
cputot,nrows(pinedee);
Saw,Chip,Logv;
saw,chip,logv;
;return !cpuuu















****extras







;fig11old:
;if(aspdone.eq.0)asp
;if(fdone.eq.0)fp

Window='450,450'

fuu=0
Sv=100*Saaw/.Vee;
Svi=100*Saawi/.Veei;
sv=100*saaw/.vee;
svi=100*saawi/.veei;
!Svu=100*Saawu/.Vee;
!Sviu=100*Saawui/.Veei;
!lower case free
hhot=vector(59.5,59,58.5)

;do(il,1,3)

fuu=drawline(cprice,Sv(il,All),color->colors(il),width->2,append->,show->0)
fuu=drawline(cprice,Svi(il,All),color->colors(il),width->2,append->,show->0)

fuu=drawline(cprice,sv(il,All),color->colors(il),width->1,append->,show->0)
fuu=drawline(cprice,svi(il,All),color->colors(il),width->1,append->,show->0)
!fu=drawline(cprice,svu(il,All),color->colors(il),width->2,append->,show->0)
!fu=drawline(cprice,sviu(il,All),color->colors(il),width->1,append->,show->0)
fuu=drawline(165,185,hhot(il),hhot(il),color->colors(il),width->2,append->,show->0,
label->' P_{saw}= "psaws(il)""font"')
;enddo
fuu=drawline(30,58.8,label->'style(rotate by -42)Indep. SM, free"font"',append->,show->0)
fuu=drawline(100,59.5,label->'style(rotate by -42)Indep. SM, as."font"',append->,show->0)
fuu=drawline(50,61.8,label->'style(rotate by 0)SM with pulp, as."font"',append->,show->0)
fuu=drawline(100,59.7,label->'style(rotate by 15)SM with pulp, free"font"',append->,show->0)
fuu=drawline(20,57,label->'V_{saw}/V_{log}"font"',append->,show->0)
fuu=drawline(-15,61.5,label->'%"font"',append->,show->0)



show(fuu,continue->,xlabel->'P_{chip}, €/m^3"font"',
title->'V_{saw}/V_{log}"font"')

!show(fu,continue->)
;return  !fig11old




























;jare:
Window='450,450'
man=data(read->(ja,price),in->)
200,36.40
210,36.90
220,37.40
230,37.80
240,38.30
250,38.70
260,39.20
270,39.60
280,40.00
290,40.30
300,40.70
310,41.00
320,41.40
330,41.70
340,42.00
350,42.30
/
stat(min->,max->)
fi=0
fi=plotyx(price,ja,show->0)
re=regr(price,ja)
fi=drawline(ja%min,ja%max,re(ja%min),re(ja%max),color->Red,append->,continue->)
! Lähtötaso
! Kuusi
! Keskijäreys Hinta
! €/m3
kuus=data(read->(ja,price),in->)
200,37.90
210,38.20
220,38.60
230,38.90
240,39.30
250,39.60
260,39.90
270,40.20
280,40.40
290,40.70
300,40.90
310,41.20
320,41.40
330,41.60
340,41.90
350,42.10
/
stat(min->,max->)
fi=0
fi=plotyx(price,ja,show->0)
re=regr(price,ja)
fi=drawline(ja%min,ja%max,re(ja%min),re(ja%max),color->Red,append->,continue->)



;return













show(fi9,xlabel->'D_{top}, cm"font"',continue->co)
show(fi10,continue->co)
;return

;fignew:

;if(type(pinedee).ne.MATRIX)rpine
! fi 11 cumulative wr vcom fi12 cumulative wr dbh
! FI 13 PROS VS DBH
Window='450,450'
Lmin=40
Lmax=55
Dmin=15
dmin=5
clas=30
minob=7
   !without fine tuning absolute values


stemlist0=list(U%logv,U%cyl,U%saw,U%inc,U%outc,U%chip,U%pulp,U%sawu);
stemlistp=list(logvp,cylp,sawp,incp,outcp,chipp,pulpp,sawup);
stemlist=list(dbh,U%com,@stemlist0,@stemlistp);
stemmat=matrix(nrows(pinedee),len(stemlist));
vec=matrix(len(stemlist0));

tranf11=trans()
nob=0
do(iobs,1,nrows(pinedee))
dbh=pinedee(iobs,13)
if(pinedee(iobs,Lmin).ge.Dmin)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",weight->"Weight",obj->U%saw)
nob=nob+1
vec=stemlist0
vec=100*vec/U%logv !U%com

stemlistp=vec
stemmat(nob,All)=stemlist
endif
enddo
/
call(tranf11)
varlist=list(Logv,Cyl,Saw,Inc,Outc,Chip,Pulp,Sawu)
readlist=list(dbh,Com,@varlist,@stemlistp)
nob;
stemdat=newdata(stemmat(1,-nob,All),read->@readlist)
stat(min->,max->)

colors=matrix(len(varlist),values->(Cyan,Red,Orange,Violet,Blue,Green,Black,Orange))

xvar='Com'
fi11=0

;do(i,1,len(varlist)-1)
cl=classify(@varlist(i),data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi11=drawclass(cl,continue->,color->colors(i),se->,sd->,show->0,width->(1,2),append->)
;enddo
cl=classify(Sawu,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi11=drawclass(cl,continue->,color->Orange,append->)
xloc=300
xdd=120
yloc=1400
ydd=100

fi11=drawline(xloc,xloc+xdd,yloc+ydd,yloc+ydd,color->Black,width->1,label->' V_{com}"font"',append->,show->0)
fi11=drawline(xloc,xloc+xdd,yloc,yloc,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi11=drawline(xloc,xloc+xdd,yloc-ydd,yloc-ydd,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi11=drawline(xloc,xloc+xdd,yloc-2*ydd,yloc-2*ydd,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi11=drawline(xloc,xloc+xdd,yloc-3*ydd,yloc-3*ydd,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi11=drawline(xloc,xloc+xdd,yloc-4*ydd,yloc-4*ydd,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi11=drawline(xloc,xloc+xdd,yloc-5*ydd,yloc-5*ydd,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)
fi11=drawline(xloc,xloc+xdd,yloc-6*ydd,yloc-6*ydd,color->Black,width->2,label->' V_{pulp}"font"',append->,show->0)

fi11=drawline(600,600+xdd,45,45,color->Orange,width->1,label->' V_{sawu}"font"',append->,show->0)

fi11=drawline(0,1800,0,1800,yrange->(0,1600),
xlabel->'V_{com}, dm^3"font"',  !yrange->(0,600),
ylabel->'V_{*}"font"',append->,show->0)
show(fi11,continue->)

xvar='dbh'
fi12=0
;do(i,1,len(varlist)-1)
cl=classify(@varlist(i),data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi12=drawclass(cl,continue->,color->colors(i),se->,sd->,show->0,width->(1,2),append->)
;enddo

cl=classify(Sawu,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi12=drawclass(cl,continue->,color->Orange,append->)


cl=classify(Com,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi12=drawclass(cl,continue->,color->Black,se->,sd->,show->0,width->(1,2),append->)

xloc=20
xdd=3
yloc=1400
ydd=100

fi12=drawline(xloc,xloc+xdd,yloc+ydd,yloc+ydd,color->Black,width->2,label->' V_{com}"font"',append->,show->0)
fi12=drawline(xloc,xloc+xdd,yloc,yloc,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi12=drawline(xloc,xloc+xdd,yloc-ydd,yloc-ydd,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi12=drawline(xloc,xloc+xdd,yloc-2*ydd,yloc-2*ydd,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi12=drawline(xloc,xloc+xdd,yloc-3*ydd,yloc-3*ydd,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi12=drawline(xloc,xloc+xdd,yloc-4*ydd,yloc-4*ydd,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi12=drawline(xloc,xloc+xdd,yloc-5*ydd,yloc-5*ydd,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)
fi12=drawline(xloc,xloc+xdd,yloc-6*ydd,yloc-6*ydd,color->Black,width->2,label->' V_{pulp}"font"',append->,show->0)

fi12=drawline(yrange->(0,1600),
xlabel->'dbh, cm"font"',  !yrange->(0,600),
ylabel->'V_{*}"font"',append->,show->0)



show(fi12,continue->)
xvar='dbh'
fi13=0
;do(i,1,len(stemlistp)-1)
cl=classify(@stemlistp(i),data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi13=drawclass(cl,continue->,color->colors(i),se->,sd->,show->0,width->(1,2),append->)
;enddo

cl=classify(sawup,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi13=drawclass(cl,continue->,color->Orange,show->0,append->)

! cl=classify(Com,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
! fi12=drawclass(cl,continue->,color->colors(i),se->,sd->,show->0,width->(1,2),append->)

xloc=41
xdd=3
yloc1=87
yloc=57
ydd=3.7

!fi12=drawline(xloc,xloc+xdd,yloc+ydd,yloc+ydd,color->Black,width->2,label->' V_{com}"font"',append->,show->0)
fi13=drawline(xloc,xloc+xdd,yloc1,yloc1,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi13=drawline(xloc,xloc+xdd,yloc1-ydd,yloc1-ydd,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi13=drawline(xloc,xloc+xdd,yloc,yloc,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi13=drawline(xloc,xloc+xdd,yloc-ydd,yloc-ydd,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi13=drawline(xloc,xloc+xdd,yloc-2*ydd,yloc-2*ydd,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi13=drawline(xloc,xloc+xdd,yloc-3*ydd,yloc-3*ydd,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)
fi13=drawline(xloc,xloc+xdd,yloc-4*ydd,yloc-4*ydd,color->Black,width->2,label->' V_{pulp}"font"',append->,show->0)

fi13=drawline(32,32+xdd,45,45,color->Orange,width->1,label->' V_{sawu}"font"',append->,show->0)
!fi13=drawline(
!xlabel->'dbh, cm"font"',  !yrange->(0,600),
!ylabel->'V_{*}/V_{com}"font"',append->,show->0)
fi13=drawline(13.5,95,label->'%]Arial,13[',show->0,append->)

show(fi13,xlabel->'dbh, cm"font"')


Window='450,450'
xvar='Com'
fi14=0
;do(i,1,len(stemlistp)-1)
cl=classify(@stemlistp(i),data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi14=drawclass(cl,continue->,color->colors(i),se->,sd->,show->0,width->(1,2),append->)
;enddo

cl=classify(sawup,data->stemdat,x->"xvar",xrange->,classes->clas,minobs->minob)
fi14=drawclass(cl,continue->,color->Orange,append->,show->0)

xloc=1300
xdd=120
yloc1=90

yloc=57
ydd=3.7

!fi8=drawline(xloc,xloc+xdd,yloc,yloc,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi14=drawline(xloc,xloc+xdd,yloc1,yloc1,color->Cyan,width->2,label->' V_{log}"font"',append->,show->0)
fi14=drawline(xloc,xloc+xdd,yloc1-ydd,yloc1-ydd,color->Red,width->2,label->' V_{CYL}"font"',append->,show->0)
fi14=drawline(xloc,xloc+xdd,yloc,yloc,color->Orange,width->2,label->' V_{saw}"font"',append->,show->0)
fi14=drawline(xloc,xloc+xdd,yloc-1*ydd,yloc-1*ydd,color->Green,width->2,label->' V_{chip}"font"',append->,show->0)
fi14=drawline(xloc,xloc+xdd,yloc-2*ydd,yloc-2*ydd,color->Violet,width->2,label->' V_{INC}"font"',append->,show->0)
fi14=drawline(xloc,xloc+xdd,yloc-3*ydd,yloc-3*ydd,color->Blue,width->2,label->' V_{OUTC}"font"',append->,show->0)
fi14=drawline(xloc,xloc+xdd,yloc-4*ydd,yloc-4*ydd,color->Black,width->2,label->' V_{pulp}"font"',append->,show->0)

fi14=drawline(800,800+xdd,48,48,color->Orange,width->1,label->' V_{sawu}"font"',append->,show->0)

fi14=drawline(-100,95,label->'%]Arial,13[',show->0,append->)


Window='450,450'

show(fi14,xlabel->'V_{com}, dm^3"font"')  !!yrange->(0,600),

Window='900,450'

!ylabel->'V_{*}/V_{com}, %"fo)
fig5f=show(fi13,fi14,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)


;return







;fig4:
;if(type(pinedee).ne.MATRIX)rpine
!;lat2:

trl=trans()
;do(i,4,14)
 ;do(j,i+1,15)
 V"i"%"j"=vtop"i"-vtop"j"  !volu between top diamters i and j 
 ;enddo
 ;enddo
 
 ;do(i,5,14)  ! last measurement
 !;do(j,5,14)
 I="i"
 if(V"i"%"i+1".gt.0)then
 der(c0"i",c"i",p"i")          !derivative are neede in nonliear regression
f"i"=c0"i"+c"i"*V"i"%"i+1"**p"i"

der(C0"i",C"i",P"i")
F"i"=C0"i"-C"i"*P"i"**V"i"%"i+1"
g"i"=P"i"**V"i"%"i+1"
endif

 ;enddo
 !pause('<1>')
 
/


C05...C014=0.8
P5...P14=0.95
C5...C14=0.4


;do(id,1,1)  !data sets pine only
latvat"id"=matrix(7,10)
;do(i,6,6)   !5,14)
p"i"=1
c0"i"=0
c"i"=1
re=regr(V4%"i",f"i",
filter->htop15.gt.26,trans->trl,data->@datat0(id))

p"i"=1
c0"i"=coef(re,1)
c"i"=coef(re,f"i")
nl=nonlin(V4%"i",f"i",par->(c0"i",c"i",p"i"),
filter->htop15.gt.26,trans->trl,data->@datat0(id))
stat(V4%6,V6%7,min->,max->,
filter->htop15.gt.26,trans->trl,data->@datat0(id))
P"i"=0.59
! ;do(k,1,19)
! P"i"=P"i"+0.01;
re=regr(V4%"i",g"i",filter->htop15.gt.26,trans->trl,data->@datat0(id))
!;enddo
 C0"i"=coef(re,1);
 C"i"=-coef(re,g"i");
 nl2=nonlin(V4%"i",F"i",par->(C0"i",C"i",P"i"),
 filter->htop15.gt.26,trans->trl,data->@datat0(id),slow->0.01)
  vc=varcomp(Resid,class->plot,data->@datat0(id),filter->htop15.gt.26)
i0="i-4"	
! latvat"id"(1,i0)=C0"i"
! latvat"id"(2,i0)=C"i"
! latvat"id"(3,i0)=P"i"
! latvat"id"(4,i0)=rmse(nl2)

! latvat"id"(5,i0)=r2(nl2)
! latvat"id"(6,i0)=sqrt(vc%varb)
! latvat"id"(7,i0)=sqrt(vc%varw)
ia=0
!pause('<2>')
! ;if(i.ge.14);then


! P"i"=0.59
! ! ;do(k,1,19)
! ! P"i"=P"i"+0.01;
! re=regr(V4%"i",g"i",filter->htop15.gt.26,trans->trl,data->@datat0(id))
! !;enddo
 ! C0"i"=coef(re,1);
 ! C"i"=-coef(re,g"i");
 ! nl2=nonlin(V4%"i",F"i",par->(C0"i",C"i",P"i"),
 ! filter->htop15.gt.26,trans->trl,data->@datat0(id),slow->0.01)
! P"i"=0.59
! ! ;do(k,1,19)
! ! P"i"=P"i"+0.01;

! stat(V"i"%"i+1",filter->htop15.gt.26,trans->trl,data->@datat0(id),min->,max->)
! cl=classify(V4%"i",x->V"i"%"i+1",xrange->,classes->20,minobs->9,filter->htop15.gt.26,
! trans->trl,data->@datat0(id))
! fi"i"=drawclass(cl,sd->,mean->,continue->,color->Red,show->0)
! fi"i"=drawclass(cl,se->,width->2,append->)


!;endif
id,i;

!;if(id.eq.1.and.i.eq.6);then
!pause('<3>')
stat(V4%6,V6%7,min->,max->,
filter->htop15.gt.26,trans->trl,data->@datat0(id))
P"i"=0.59
! ;do(k,1,19)
! P"i"=P"i"+0.01;
re=regr(V4%"i",g"i",filter->htop15.gt.26,trans->trl,data->@datat0(id))
!;enddo
 C0"i"=coef(re,1);
 C"i"=-coef(re,g"i");
 nl2=nonlin(V4%"i",F"i",par->(C0"i",C"i",P"i"),
 filter->htop15.gt.26,trans->trl,data->@datat0(id),slow->0.01)


cl=classify(V4%6,x->V6%7,xrange->,classes->20,minobs->9,filter->htop15.gt.26,
trans->trl,data->@datat0(id))
fig4=drawclass(cl,sd->,mean->,continue->,color->Red,show->0)
fig4=drawclass(cl,se->,show->0,width->2,append->)

fig4=drawline(1,1.5,0.75,0.75,color->Blue,label->' 3.448-3.730*0.5890^{V(6,7)}"font"',
append->,show->0)
fig4=drawline(1.5,0.5,label->'RMSE=0.1859 R^2=0.8135"font"',
append->,show->0,width->2)
fig4=drawline(1.5,0.25,label->'s_b=0.0333   s_w=0.242"font"',
append->,show->0)

fig4=draw(func->(C0"i"-C"i"*P"i"**V6%7),xrange->(V6%7%min,V6%7%max),
x->V6%7,append->,color->Blue,width->2,xlabel->'V(6,7), dm^3"font"',
ylabel->'V(4,6), dm^3"font"')


!;endif

;enddo

;enddo



;return  ! fig4



;fig15vv:
;if(type(pinedee).ne.MATRIX)rpine



Lmin=40
Lmax=55
Dmin=15
dmin=5

trani0=trans() !like trana in ana7
! delete_o(saw)
! chip=0
! logv=0
! obj=0
delete_o(saw0)
saw0;
chip0=0
vol0=0
volmin=0
sawmin=0
objb0=0
vtot=0
is3=1
is2=1
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",obj->u%saw)   !,any->)
vtot=vtot+u%com
saw0=saw0+u%saw
chip0=chip0+u%chip
vol0=vol0+u%logv
obj0=obj0+u%ob0

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",obj->(-u%logv)) 
volmin=volmin+u%logv
sawmin=sawmin+u%saw
endif

enddo
/

call(trani0)


saw0;
volmin;
saw0=100*saw0/vtot;
logv0=100*logv0/vtot;
chip0=100*chip0/vtot;
vol0=100*vol0/vtot;
volmin=100*volmin/vtot;
sawmin=100*sawmin/vtot;
pause('<gsgs')

trani=trans() !like 
saw=0
sawu=0
chip=0
vol=0
obj=0
Saw=0
Sawu=0
Chip=0
Vol=0
Obj=0
maxvolero=0

do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",weight->"Weight",obj->(Psaw*U%sawu+pchip*U%chip-plog*U%logv))
saw=saw+U%saw
sawu=sawu+U%sawu
chip=chip+U%chip
vol=vol+U%logv
obj=obj+U%obj
! U2=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
! func->"Func",weight->"Weight",obj->(psaw*U%saw+pchip*U%chip-plog*U%logv))

! U%saw,U2%saw;
! U%sawu,U2%sawu;
! U%logv,U2%logv;
! U%obj,U2%obj;
!pause('<>')

V=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",weight->"Weight",obj->(Psaw*V%sawu+pchip*V%chip))
Saw=Saw+V%saw
Sawu=Saw+V%sawu

Chip=Chip+V%chip
Vol=Vol+V%logv
Obj=Obj+V%obj
volero=Vol-vol
if(volero.gt.maxvolero.and.U%nlog.gt.2)then
maxvolero=volero
iobsmax=iobs
sawero=Saw-saw
psawero=Psaw
pchipero=pchip
lminero=Lmin
lmaxero=Lmax
dminero=Dmin
endif
endif
enddo
/

Psaws=matrix(do->(150,300,25));

Lenpsaw=len(Psaws)
pchips=matrix(3,values->(20,35,50))
lenpchip=len(pchips)
colors=matrix(3,values->(Blue,Green,Red))
!lenlam=len(lams)
resus=matrix(lenpchip,Lenpsaw)
resusu=matrix(lenpchip,Lenpsaw)
resuv=matrix(lenpchip,Lenpsaw)

Resusu=matrix(lenpchip,Lenpsaw)
Resus=matrix(lenpchip,Lenpsaw)
Resuv=matrix(lenpchip,Lenpsaw)
!lam=20
plog=75
!psaw=200
!pchip=50
;do(ip,1,Lenpsaw)
Psaw=Psaws(ip);
;do(il,1,lenpchip)
!uti=(psaw-lam-plog)*saw+(pchip-lam-plog)*chip

pchip=pchips(il);
call(trani)
resus(il,ip)=saw
resusu(il,ip)=sawu
resuv(il,ip)=vol
Resus(il,ip)=Saw
Resusu(il,ip)=Sawu
Resuv(il,ip)=Vol
;enddo
;enddo
resuv=100*resuv/vtot;
resus=100*resus/vtot;
Resuv=100*Resuv/vtot;
Resus=100*Resus/vtot;

rs=Resus/.resus;
rv=Resuv/.resuv;
rs0=Resus/saw0;
rv0=Resuv/vol0;

maxvolero;
iobsmax;
psawero;
Psaw=psawero;
pchip=pchipero;
Lmin=lminero;
Lmax=lmaxero;
Dmin=dminero;
dmin;
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobsmax, 
func->"Func",weight->"Weight",obj->(Psaw*U%sawu+pchip*U%chip-plog*U%logv))
V=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobsmax, 
func->"Func",weight->"Weight",obj->(Psaw*V%sawu+pchip*V%chip))
U%saw,V%saw;
U%nlog,V%nlog;
U%lens,V%lens;
hoo=matrix(350,do->)

pause('<here>')






;fig15b:
 colors=matrix(3,values->(Blue,Green,Red))
 !$$=777
labxv=41
labxs=45
labelhv=matrix(3,values->(76.2,77.6,79))

labelhs=matrix(3,values->(56,54.5,53))
fi15=0
fi16=0
!upper case func->"Func",weight->"Weight",obj->(psaw*V%sawu+pchip*V%chip)
;do(il,1,lenpchip)
fi15=drawline(Psaws,resuv(il,All),append->,show->0,color->colors(il),width->3)
fi15=drawline(Psaws,Resuv(il,All),append->,show->0,color->colors(il),width->1)
!Fi15=drawline(Psaws,resuv(il,All),append->,show->0,color->colors(il),width->2)
!Fi15=drawline(Psaws,Resuv(il,All),append->,show->0,color->colors(il),width->1)
!Fi15=drawline(Psaws,resus(il,All),append->,show->0,color->colors(il),width->2)
!Fi15=drawline(Psaws,Resus(il,All),append->,show->0,color->colors(il),width->1)


fi15=drawline(labxv,labxv+labd,labelhv(il),labelhv(il),color->colors(il),width->3,append->,
label->' P_{chip}= "pchips(il)""font"',append->,show->0)

!fi16=drawline(labxs,labxs+labd,labelhs(il),labelhs(il),color->colors(il),width->2,append->,
!label->' P_{chip}= "pchips(il)""font"',append->,show->0)
	
fi16=drawline(Psaws,resus(il,All),append->,show->0,color->colors(il),width->3)
fi16=drawline(Psaws,Resus(il,All),append->,show->0,color->colors(il),width->1)
;enddo

fi15=drawline(Psaws(1),Psaws(Lenpsaw)-30,vol0,vol0,append->,width->3,show->0,label->' Max V_{saw}"font"')
fi15=drawline(Psaws(1)+25,Psaws(Lenpsaw),volmin,volmin,append->,width->3,show->0,
color->Cyan)

fi16=drawline(155,56,label->'Thin:   max U_{saw}"font"',append->,show->0)
fi16=drawline(155,54,label->'Thick: max U_{saw} - p_{asp}"font"',append->,show->0)

fi16=drawline(250,56,label->'Above: V_{log}/V_{com}"font"',append->,show->0)
fi16=drawline(250,54,label->'Below: V_{log}/V_{com}"font"',append->,show->0)

fi15=drawline(Psaws(1),volmin,append->,show->0,label->' Min V_{log}"font"')


fi16=drawline(Psaws(1),Psaws(Lenpsaw)-20,saw0,saw0,append->,width->3,show->0)
fi16=drawline(Psaws(1)+25,Psaws(Lenpsaw),sawmin,sawmin,append->,width->3,show->0,color->Cyan)

fi16=drawline(Psaws(Lenpsaw)-30,saw0+0.15,append->,width->3,show->0,label->' Max V_{saw}"font"')
fi16=drawline(Psaws(1),sawmin,append->,show->0,label->'Min V_{log}"font"')
!fi16=drawline(Psaws(Lenpsaw)+30,sawmin+0.15,append->,width->3,show->0,label->' Min V_{log}"font"')
!fi15=drawline(150,78,label->'Above: princing independent of bucking"font"',append->,show->0)
!fi15=drawline(150,77.2,label->'Below: assortment pricing"font"',append->,show->0)
!fi16=drawline(180,51.5,label->'Upper curves:"font"',append->,show->0)
!fi16=drawline(180,51.1,label->'princing independent of bucking"font"',append->,show->0)
!fi16=drawline(180,50.7,label->'Lower curves:"font"',append->,show->0)
!fi16=drawline(180,50.3,label->'Assortment princing"font"',append->,show->0)
!fi15=drawline(xlabel->'P_{saw}, cm"font"',ylabel->'V_{log}/V_{com}, %"font"',append->,show->0,yrange->(74,89))
!fi16=drawline(xlabel->'P_{saw}, cm"font"',yrange->(47,53.2),ylabel->'V_{saw}/V_{com}, %"font"',append->,show->0)

! fi=drawline(labx,75,append->,show->0,label->' Upper curves: V_{log}/V_{com}"font"')
! fi=drawline(labx,59,append->,show->0,label->' Lower curves: V_{saw}/V_{com}"font"')

! fi=drawline(xlabel->'P_{saw}"font"',ylabel->'%"font"',append->,continue->,xrange->(150,300))
!;fig15multi:
Window='450,450'
lmargin=3
!fi15;
fi15b=fi15
!fi15b;
!;return
fi16b=fi16
!'border 2+4+8;lmargin "lmargin" ;rmargin 1;tmargin 0;bmargin 1.5',
!unset->'xtics;raxis',show->0) 
!fi15b;
show(fi15b,yrange->(72.5,87.5),xrange->(150,300),
continue->,set->'border 2+4+8;lmargin "lmargin" ;rmargin 1;tmargin 0;bmargin 1.5',
unset->'xtics;raxis',show->0)
!fi15b;
!;return


show(fi16b,yrange->(42.5,57.5),xrange->(150,300),
continue->,
set->'border 1+2+8;xtics nomirror;lmargin "lmargin" ;rmargin 1;tmargin 0;bmargin 1.5',
unset->'rtics',show->0)
!fi15b=drawline(235,74,label->'V_{log}/V_{com}"font"',append->,show->0)
fi16b=drawline(148,58.5,label->'%           Independent sawmills"font2"',append->,show->0)

!fi16b=drawline(155,56,label->'Thin:   max U_{saw}"font"',append->,show->0)
!fi16b=drawline(155,54,label->'Thick: max U_{saw} - p_{asp}"font"',append->,show->0)

!fi16b=drawline(250,56,label->'Above: V_{log}/V_{com}"font"',append->,show->0)
!fi16b=drawline(250,54,label->'Below: V_{saw}/V_{com}"font"',append->,show->0)


!fi16b;
;return

;fig15multi:

! !show(fi15,yrange->(72.5,87.5),gnuplot->'set bmargin at screen 0.01',show->0)
! show(fi15,yrange->(72.5,87.5),show->0)
! show(fi16,yrange->(42.5,57.5),show->0,xlabel->'P_{saw}"font"',show->0)
fi1516=show(fi15b,fi16b,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 2,1 scale 1,1',continue->)
;return


fi1516=show(fi15b,fi16b,gnuplot->
'set terminal pngcairo size 600,900;set lmargin at screen 0.15;set rmargin at screen 0.95 ',
multiplot->'set offset 0,0,graph 0.05, graph 0.05 ',continue->)


;return
set tmargin at screen TOP-2*DY
set bmargin at screen TOP-3*DY
set ytics -1000,500,1000
!;return
fi1516=show(fi15,fi16,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 2,1 scale 1,1')
!show(Fi15)
;return

;fig17vv:
!chipot=vol0-saw0;
labx=0.5
laby=3
labd=0.4
labdh=0.3
chi=resuv-resus;  !obtaine chips
chipot=Resuv-Resus; !chips in potential
chigain=chi-chipot;  !differencee
loss=(plog-ppulp)*(Resuv-resuv);  !forest ownere
!sawlossv=(Resus-resus);
!chiplossv=chi-chipot;
!sawlosssaw=t(Psaws)*.sawlossv;
!gain current utility - utility if true objective is used 
!func->"Func",obj->(psaw*V%saw+pchip*V%chip))  utility
! when log price is included 
utipot=t(Psaws)*.Resus+pchips*.(Resuv-Resus);
utigot=t(Psaws)*.resus+pchips*.(resuv-resus);
utiloss=utipot-utigot;
gain=loss-utiloss;
fi17=0
labxv=1
labd=0.3
labelh=matrix(3,values->(4,3.6,3.2))
;do(il,1,lenpchip)


fi17=drawline(0.01*loss(il,All),0.01*gain(il,All),append->,show->0,color->colors(il),width->2)
fi17=drawline(labxv,labxv+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
label->' P_{chip}= "pchips(il)""font"',append->,show->0)
!fi=drawline(labx,labx+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
!label->' P_{chip}= "pchips(il)""font"',append->,show->0)
!fi2=drawline(labx,labx+labd,laby-il*labdh,laby-il*labdh,color->colors(il),width->2,append->,
!label->' P_{chip}= "pchips(il)""font"',append->,show->0)
! ;if(il.eq.1);then
! gain00=0.01*gain(3)
! loss00=0.01*loss(3)
! ;endif
!fi=drawline(Psaws,resus(il,All),append->,show->0,color->colors(il),width->2)
;enddo
fi17=drawline(0,6,0,6,width->2,append->,show->0)

xva=0.01*loss(1,4);
yva=0.01*gain(1,4);
fi17=drawline(xva,xva,yva,xva,append->,show->0,color->Orange,width->2)

fi17=drawline(xva+0.1,2.2,label->'Black hole"font"',append->,show->0,
append->,show->0,color->Orange,width->2)


fi17=drawline(xlabel->'Forest owner loss, Euro/m^3"font"', 
 ylabel->'Sawmill gain, Euro/m^3"font"', xrange->(0,6),yrange->(0,6),append->,show->0)
show(fi17,continue->)

;return
fi2=drawline(loss00,loss00,gain00,loss00,width->2,color->Orange,show->0,append->)
fi2=drawline(loss00+0.02,1.8,
label->'Black hole"font"',show->0,append->)

fi2=drawline(0,4,0,4,append->,width->2,show->0,
xlabel->'Forest owner loss /m^3"font"',
ylabel->'Sawmill gain /m^3"font"')
show(fi2,continue->)
;return

;fig13start:

!sawmills with pulp
;if(type(pinedee).ne.MATRIX)rpine
!pinedee;
Lmin=40
Lmax=55
Dmin=15
dmin=5

trani0=trans() !like trana in ana7
saw=0
chip=0
logv=0
obj=0
saw0=0
volmin=0
sawmin=0
chip0=0
vol0=0
objb0=0
vtot=0
sawmax=0
volmax=0
sawumax=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee(iobs,All),
func->"Func",obj->u%saw)   !,

vtot=vtot+u%com
u%totc,u%chip,u%pulp,u%chip+u%pulp
!pause('<>')
saw0=saw0+u%saw
chip0=chip0+u%chip
vol0=vol0+u%logv
obj0=obj0+u%ob0
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",obj->(-u%logv)) 
volmin=volmin+u%logv
sawmin=sawmin+u%saw

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",obj->u%sawu) 
sawmax=sawmax+u%saw
volmax=volmax+u%logv
sawumax=sawumax+u%sawu

endif

enddo
/

cpu0=cpu()
!;do(k,1,100)
call(trani0)
!;enddo
cpu1=cpu()

cpuu=cpu1-cpu0;

!;return

saw0=100*saw0/vtot;
logv0=100*logv0/vtot;
cip0=100*chip0/vtot;
vol0=100*vol0/vtot;

volmin=100*volmin/vtot;
sawmin=100*sawmin/vtot;
volmax=100*volmax/vtot;
sawmax=100*sawmax/vtot;

tranic=trans() !like 
delete_o(saw)
chip=0
vol=0
obj=0
Saw=0
Saw2=0
saw=0
Chip=0
Vol=0
Vol2=0
Obj=0

do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",weight->"Weight",obj->(psaw*U%sawu+Pchip*U%totc-plog*U%logv-ppulp*U%pulp))
saw=saw+U%saw
chip=chip+U%chip
vol=vol+U%logv
obj=obj+U%obj

V=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",weight->"Weight",obj->(psaw*V%sawu+Pchip*V%totc))

Saw=Saw+V%saw
Chip=Chip+V%chip
Vol=Vol+V%logv
Obj=Obj+V%obj
V2=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs,
func->"Func",weight->"Weight",obj->(psaw*V2%sawu+Pchip*V2%totc),any->)
Saw2=Saw2+V2%saw
Vol2=Vol2+V2%logv

endif
enddo
/

Pchips=matrix(do->(40,200,25));
pause('<xx>')
Lenpchip=len(Pchips)
psaws=vector(180,220,260)
lenpsaw=len(psaws)

colors=matrix(3,values->(Blue,Green,Red))
!lenlam=len(lams)
resusc=matrix(lenpsaw,Lenpchip)
resuvc=matrix(lenpsaw,Lenpchip)

reesus=matrix(lenpsaw)
reesuv=matrix(lenpsaw)

Resusc=matrix(lenpsaw,Lenpchip)
Resuvc=matrix(lenpsaw,Lenpchip)
Resus2=matrix(lenpsaw,Lenpchip)
Resuv2=matrix(lenpsaw,Lenpchip)
!lam=20
plog=75
ppulp=25
!psaw=200
!Pchip=50
;do(ip,1,lenpsaw)
psaw=psaws(ip);

Pchip=0
call(tranic)
reesus(ip)=saw
reesuv(ip)=vol
;do(il,1,Lenpchip)
!uti=(psaw-lam-plog)*saw+(Pchip-lam-plog)*chip

Pchip=Pchips(il);
call(tranic)
resusc(ip,il)=saw !stumpage price included
resuvc(ip,il)=vol  !stumpage price not 
Resusc(ip,il)=Saw  ! !stumpage price not
Resuvc(ip,il)=Vol
Resus2(ip,il)=Saw2  ! !stumpage price not
Resuv2(ip,il)=Vol2
ip,il,Saw,Vol,saw,vol;
!pause('<klk>')
;enddo
;enddo
 sawloss=psaws*.(Resusc-resusc);
 stump=(plog-ppulp)*(Resuvc-resuvc);
 gt1=sawloss-stump;
 deno=Resuvc-resuvc;
 
 cp=(psaws*.(Resusc-resusc)-(plog-ppulp)*(Resuvc-resuvc))/.(Resuvc-resuvc);


resuvc=100*resuvc/vtot;
resusc=100*resusc/vtot;
Resuvc=100*Resuvc/vtot;
Resusc=100*Resusc/vtot;
Resuv2=100*Resuv2/vtot;
Resus2=100*Resus2/vtot;
reesus=100*reesus/vtot;
reesuv=100*reesuv/vtot;

rs=Resusc/.resusc;
rv=Resuvc/.resuvc;
rs0=Resusc/saw0;
rv0=Resuvc/vol0;





;fig18b:
Window='450,450'
 colors=matrix(3,values->(Blue,Green,Red))
 
 sawloss=psaws*.(reesus-resusc);
 ! plog*Resuvc+ppulp*(100-Resuvc)  -plog*resuvc-ppulp*(100-resuvc)
 !(plog-ppulp)*Resuvc +ppulp*100  -(plog-ppulp)*resuvc -ppulp*100
 
 stump=(plog-ppulp)*(reesuv-resuvc);
 gt1=sawloss-stump;
 deno =reesus-resusc  ; !100-resusc-(100-Resusc)  ;  !resuvc-resusc-(resuvc-resusc)
 !  chips
 cp=(psaws*.(reesus-resusc)-(plog-ppulp)*(reesuv-resuvc))/.deno;
 
 
! pause('<>')



fi18=0
fi19=0
fi19b=0
!Fi18=0
Resuvc;
resuvc;
Resusc;
resusc;
saw0,vol0;
Resus2,Resuv2;
!;return
! (psaw*V%sawu+Pchip*V%totc)) Resus vapaa Resus2 any->
!pause('<>')
labd=6
labxv=41
labxs=45
labelhv=matrix(3,values->(76.2,77.6,79))
labelhs=matrix(3,values->(56,54.5,53))

!fi18 free bucking
;do(il,1,lenpsaw)
!Fi18=drawline(Pchips,Resuvc(il,All),append->,show->0,color->colors(il),width->2)
fi18=drawline(Pchips,Resuvc(il,All),append->,show->0,color->colors(il),width->1) !free 
;if(il.eq.1)show(fi18,yrange->(72.5,87.5),xrange->(40,190),show->0)
;if(il.eq.1)fi18=drawline(Pchips(Lenpchip-1),Pchips(Lenpchip),Resuv2(il,Lenpchip-1),Resuv2(il,Lenpchip),
append->,show->0,color->colors(il),width->1)  !free wtih any->
!;if(il.eq.1)Fi18=drawline(Pchips(Lenpchip-1),Pchips(Lenpchip),Resuv2(il,Lenpchip-1),Resuv2(il,Lenpchip),
!append->,show->0,color->colors(il),width->1)  !free wtih any->

fi18=drawline(Pchips,resuvc(il,All),append->,show->0,color->colors(il),width->3)

!Fi18=drawline(Pchips,resuvc(il,All),append->,show->0,color->colors(il),width->2)

fi19=drawline(Pchips,resusc(il,All),append->,show->0,color->colors(il),width->3)
fi19=drawline(Pchips,Resusc(il,All),append->,show->0,color->colors(il),width->1)

!Fi18=drawline(Pchips,resusc(il,All),append->,show->0,color->colors(il),width->2)
! Fi18=drawline(Pchips,Resusc(il,All),append->,show->0,color->colors(il),width->2)
!(psaws*.(Resusc-resusc)-(plog-ppulp)*(Resuv-resuvc))/(Resusc-resusc)
! 
fi19b=drawline(Pchips,cp(il,All),append->,show->0,color->colors(il),width->2)

;if(il.eq.1)fi19=drawline(Pchips(Lenpchip-1),Pchips(Lenpchip),Resus2(il,Lenpchip-1),Resus2(il,Lenpchip),
append->,show->0,color->colors(il),width->1)
;if(il.eq.1)show(fi19,yrange->(42.5,57.5),xrange->(40,190),show->0)

fi18=drawline(labxv,labxv+labd,labelhv(il),labelhv(il),color->colors(il),width->3,append->,
label->' P_{saw}= "psaws(il)""font"',append->,show->0)

!fi19=drawline(labxs,labxs+labd,labelhs(il),labelhs(il),color->colors(il),width->2,append->,
!label->' P_{saw}= "psaws(il)""font"',append->,show->0)

!;if(il.eq.1)Fi18=drawline(pchips(Lenpchip-1),pchips(Lenpchip),Resus2(il,Lenpchip-1),Resus2(il,Lenpchip),
!append->,show->0,color->colors(il),width->1)

;enddo


! show(fi18,yrange->(72.5,87.5))
! show(fi19,yrange->(42.5,57.5))
!;return
fi18=drawline(Pchips(Lenpchip),Resuvc(1,Lenpchip),mark->1,marksize->3,
append->,show->0,color->colors(1),width->1)

fi19=drawline(Pchips(Lenpchip),Resusc(1,Lenpchip),mark->1,marksize->3,
append->,show->0,color->colors(1),width->1)

fi18=drawline(Pchips(1),Pchips(Lenpchip)-30,vol0,vol0,append->,width->2,show->0,label->' Max V_{saw}"font"')
fi18=drawline(Pchips(1),volmin,volmin,append->,width->2,
show->0,label->' Min V_{log}"font"')

fi18=drawline(Pchips(1)+25,Pchips(Lenpchip)-30,volmin,volmin,append->,width->2,
color->Cyan,show->0)


fi19=drawline(Pchips(1),Pchips(Lenpchip)-30,saw0,saw0,append->,width->2,show->0,label->' Max V_{saw}"font"')
fi19=drawline(Pchips(1),sawmin,append->,
color->Cyan,show->0,label->' Min V_{log}"font"')

fi19=drawline(Pchips(1)+25,Pchips(Lenpchip),sawmin,sawmin,append->,width->2,
color->Cyan,show->0)

!fi18=drawline(xlabel->'U_{chip} "font"',append->,show->0)  !,yrange->(74,89),
!ylabel->'V_{log}/V_{com}, %"font"',append->,show->0)
!fi19=drawline(xlabel->'U_{chip}"font"',append->,show->0) ! ,yrange->(37,43.2),
!ylabel->'V_{saw}/V_{com}, %"font"',append->,show->0)
!fi18=drawline(125,74,label->'V_{log}/V_{com}"font"',append->,show->0)
fi19=drawline(102,43.5,label->'U_{chip}"font"',append->,show->0)


!fi19=drawline(125,55,label->'V_{saw}/V_{com}"font"',append->,show->0)

fi19=drawline(38,58.5,label->'%            Sawmills with pulp"font2"',append->,show->0)

fi19=drawline(45,56,label->'Thin:   max U_{saw}"font"',append->,show->0)
fi19=drawline(45,54,label->'Thick: max U_{saw} - p_{asp}"font"',append->,show->0)

fi19=drawline(140,56,label->'Above: V_{log}/V_{com}"font"',append->,show->0)
fi19=drawline(140,54,label->'Below: V_{saw}/V_{com}"font"',append->,show->0)

fi18=drawline(170.5,73,label->'Free"font"',append->,show->0)

fi19=drawline(170.5,43.5,label->'Free"font"',append->,show->0)
! show(fi18,continue->,yrange->(75,90))
! show(fi19,continue->,yrange->(45,55))

;fig18multi:
lmargin=3
!$$=777
!fi18;
show(fi18,yrange->(72.5,87.5),  ! xrange->(40,190),
set->'border 2+4+8;lmargin "lmargin" ;rmargin 1;tmargin 0;bmargin 1.5',
unset->'xtics;raxis',show->0)
!fi18;
!;return
!show(fi19,yrange->(42.5,57.5),xrange->(40,190),xlabel->'P_{saw}"font"',show->0)
!show(fi18,continue->,set->'border 2+4+8',unset->'xtics;raxis')
show(fi19,continue->,set->'border 1+2+8;xtics nomirror',unset->'rtics',show->0)

fi1819=show(fi18,fi19,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 2,1 scale 1,1')

;return

;fig1519:
Window='800,450'
fi1819=show(fi15b,fi18,fi16b,fi19,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 2,2 scale 1,1')

;return

fi19b=drawline(60,80,160,160,label->' P_{saw}= 260"font"',width->2,append->,show->0,color->Red)
fi19b=drawline(60,80,147,147,label->' P_{saw}= 220"font"',width->2,append->,show->0,color->Green)
fi19b=drawline(60,80,134,134,label->' P_{saw}= 180"font"',width->2,append->,show->0,color->Blue)

fi19b=drawline(40,200,40,200,append->,width->2,xlabel->'U_{chip}"font"',
ylabel->'p_{chip} "font"',continue->)
show(fi19b,continue->)
;return

;fig13joku:

!chipot=vol0-saw0;
Window='450,450'
labx=0.5
laby=3
labd=0.4
labdh=0.3
chi=resuvc-resusc;  !obtaine chips
chipot=Resuvc-Resusc; !chips in potential
chigain=chi-chipot;  !differencee
loss=(plog-ppulp)*(Resuvc-resuvc)  !-ppulp*(Resuvc-resuvc) ;  !forest ownere plog*Resuv-ppulp*(100-Resuv)
!                                                 plog*resuv-ppulp*(100-resuv)
! plog*Resuv-ppulp*Resuv +ppulp*100
! plog*resuv-ppulp*resuv +ppulp*100
!sawlossv=(Resus-resus);
!chiplossv=chi-chipot;
!sawlosssaw=t(Psaws)*.sawlossv;
!gain current utility - utility if true objective is used 
!func->"Func",obj->(psaw*V%saw+pchip*V%chip))  utility
! when log price is included 
!chips=100-
utipot=psaws*.Resusc+t(Pchips)*.(100-Resusc);
utigot=psaws*.resusc+t(Pchips)*.(100-resusc);
utiloss=utipot-utigot;
gain=loss-utiloss;
!!! gain=loss-utiloss=loss -utipot+utigot =
!! loss -Psaws*.Resus+t(pchips)*.(100-Resus) +Psaws*.resus+t(pchips)*.(100-resus);
! old: Psaws*.Resus +t(pchips)*.(100-Resus)
!new: Psaws*.resus +t(pchips)*.(100-resus)
! sawlogloss = Psaws*.(Resus-resus)
! stumpage price gain (plog-ppulp)*(Resuv-resuv);
! price for obtained additional chips  sawlogloss-stumpage gain
!  price of chips per volu is (sawlogloss-stumpage gain)/(100-resus-100+Resus)
! (Psaws*.(Resus-resus)-(plog-ppulp)*(Resuv-resuv))/(Resus-resus)
! 
Fi20=0
labxv=1
labd=0.3
labelh=matrix(3,values->(3.0,3.3,3.6))
!labelh=matrix(3,values->(4,3.6,3.2))
is=0
lec=lenpchip-1
;do(il,1,lenpchip)

!oa=0.01*loss(il,All);
!lob=loss(il,All);

;if(il.eq.1);then
Fi20=drawline(0.01*loss(il,All),0.01*gain(il,All), xrange->(0,5),yrange->(0,5),
show->is,color->colors(il),width->2,xlabel->'Forest owner loss, €/m^3"font"')
!ylabel->'Sawmill gain, Euro/m^3"font"')
;else
Fi20=drawline(0.01*loss(il,All),0.01*gain(il,All),append->,show->is,color->colors(il),width->2)
;endif
lec=lenpchip
Fi20=drawline(labxv,labxv+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
label->' P_{saw}= "psaws(il)""font"',append->,show->0)
!fi=drawline(labx,labx+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
!label->' P_{chip}= "pchips(il)""font"',append->,show->0)
!fi2=drawline(labx,labx+labd,laby-il*labdh,laby-il*labdh,color->colors(il),width->2,append->,
!label->' P_{chip}= "pchips(il)""font"',append->,show->0)
! ;if(il.eq.1);then
! gain00=0.01*gain(3)
! loss00=0.01*loss(3)
! ;endif
!fi=drawline(Psaws,resus(il,All),append->,show->0,color->colors(il),width->2)
;enddo
Fi20=drawline(0,6,0,6,width->2,append->,show->is)
!fi20=drawline(0.01*loss(1,lenpchip),0.01*gain(1,lenpchip),append->,show->is,
!color->colors(1),width->2)

xva=0.01*loss(3,2);
yva=0.01*gain(3,2);
Fi20=drawline(xva,xva,yva,xva,append->,show->is,color->Orange,width->3)
Fi20=drawline(xva+0.1,0.9,label->'Dead weight loss"font"',append->,show->0)
Fi20=drawline(1,4.2,label->'Sawmills with pulp"font2"',append->,show->0)
Fi20=drawline(-0.42,4.7,label->'€/m^3  Sawmill gain"font"',append->,show->0)
!Fi20=drawline(xva+0.1,2.62,label->'rotate(by 45)Dead weight loss"font"',append->,show->is,
!append->,color->Orange,width->2)

!Fi20a=Fi20
!Fi20=drawline(xlabel->'Forest owner loss, Euro/m^3"font"',unset->'ylabel',append->,show->0)

 !ylabel->'Sawmifl gain, Euro/m^3"font"',append->,show->is)
 show(Fi20,set->'rmargin 0.2',append->,show->0)

show(Fi20,continue->)



;return
;dead:
Window='900,450'

fi20b=fi20
Fi20b=Fi20
!fi20b;
$$=777
!show(fi20b,set->'size square',show->0)
fi20b;
!show(fi20b,set->'size square',show->0)
!Fi20b=show(Fi20b,set->'lmargin 0',show->0) !unset->'ylabel',show->0)
!set ytics format "" '
!Fi20b=show(Fi20b,set->'ytics format "" ;lmargin 0',ylabel->'. ',show->0) !unset->'ylabel',show->0)

fidead=show(fi20b,Fi20b,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)

;return
;fin:  !
! black hole 
Window='450,450'
resuf=matrix(lenpsaw,Lenpchip)
saw0;
vol0;
resusc;
resuvc;
Resusc;
Resuvc;
!;return
;do(il,1,lenpsaw)
!chips=100-saw
!when saw is maximized 100-saw0
!otherwise  100-resusc
resuf(il,All)=(psaw*(resusc(il,All)-saw0)-(plog-ppulp)*(resuvc(il,All)-vol0))/.(saw0-resusc(il,All))

;enddo
resuf;

finf=0
;do(il,1,lenpsaw)
finf=drawline(Pchips,resuf(il,All),append->,show->0,color->colors(il),width->2)
! if(il.eq.1)fi18=drawline(pchips(lenpchip-1),pchips(lenpchip),Resuvc(il,lenpchip-1),Resuv(il,lenpchip), }
! append->,show->0,color->colors(il),width->1) }
! fi18=drawline(labxv,labxv+labd,labelhv(il),labelhv(il),color->colors(il),width->2,append->, }
! label->' P_!saw}= "psaws(il)""font"',append->,show->0) }

! fi19=drawline(labxs,labxs+labd,labelhs(il),labelhs(il),color->colors(il),width->2,append->, }
! label->' P_!saw}= "psaws(il)""font"',append->,show->0) }
	
! fi19=drawline(pchips,resusc(il,All),append->,show->0,color->colors(il),width->2) }
! !(psaws*.(Resusc-resus)-(plog-ppulp)*(Resuv-resuv))/(Resusc-resus) }
! !  }
! fi19b=drawline(pchips,cp(il,All),append->,show->0,color->colors(il),width->2) }

! if(il.eq.1)fi19=drawline(pchips(lenpchip-1),pchips(lenpchip),Resusc(il,lenpchip-1),Resusc(il,lenpchip), }
! append->,show->0,color->colors(il),width->1) }
;enddo
show(finf)
;return

fi18=drawline(pchips(lenpchip),Resuv(1,lenpchip),mark->1,marksize->3,
append->,show->0,color->colors(1),width->1)

fi19=drawline(pchips(lenpchip),Resusc(1,lenpchip),mark->1,marksize->3,
append->,show->0,color->colors(1),width->1)

fi18=drawline(pchips(1),pchips(lenpchip)-30,vol0,vol0,append->,width->2,show->0,label->' Max V_{saw}"font"')
fi19=drawline(pchips(1),pchips(lenpchip)-30,saw0,saw0,append->,width->2,show->0,label->' Max V_{saw}"font"')

!fi18=drawline(xlabel->'U_{chip} "font"',yrange->(74,89),
!ylabel->'V_{log}/V_{com}, %"font"',append->,show->0)
!fi19=drawline(xlabel->'U_{chip}"font"',yrange->(37,43.2),
!ylabel->'V_{saw}/V_{com}, %"font"',append->,show->0)


show(fi18,continue->)
show(fi19,continue->)
fi19b=drawline(60,80,160,160,label->' P_{saw}= 260"font"',width->2,append->,show->0,color->Red)
fi19b=drawline(60,80,147,147,label->' P_{saw}= 220"font"',width->2,append->,show->0,color->Green)
fi19b=drawline(60,80,134,134,label->' P_{saw}= 180"font"',width->2,append->,show->0,color->Blue)

!fi19b=drawline(40,200,40,200,append->,width->2,xlabel->'U_{chip}"font"',
!ylabel->'p_{chip} "font"',continue->)
show(fi19b,continue->)



;return



;fig20:
!chipot=vol0-saw0;
Window='450,450'
labx=0.5
laby=3
labd=0.4
labdh=0.3
chi=resuv-resus;  !obtaine chips
chipot=Resuv-Resus; !chips in potential
chigain=chi-chipot;  !differencee
loss=(plog-ppulp)*(Resuv-resuv);  !forest ownere
!sawlossv=(Resus-resus);
!chiplossv=chi-chipot;
!sawlosssaw=t(Psaws)*.sawlossv;
!gain current utility - utility if true objective is used 
!func->"Func",obj->(psaw*V%saw+pchip*V%chip))  utility
! when log price is included 
!chips=100-
utipot=t(Psaws)*.Resus+pchips*.(100-Resus);
utigot=t(Psaws)*.resus+pchips*.(100-resus);
utiloss=utipot-utigot;
gain=loss-utiloss;
!!! gain=loss-utiloss=loss -utipot+utigot =
!! loss -Psaws*.Resus+t(pchips)*.(100-Resus) +Psaws*.resus+t(pchips)*.(100-resus);
! old: Psaws*.Resus +t(pchips)*.(100-Resus)
!new: Psaws*.resus +t(pchips)*.(100-resus)
! sawlogloss = Psaws*.(Resus-resus)
! stumpage price gain (plog-ppulp)*(Resuv-resuv);
! price for obtained additional chips  sawlogloss-stumpage gain
!  price of chips per volu is (sawlogloss-stumpage gain)/(100-resus-100+Resus)
! (Psaws*.(Resus-resus)-(plog-ppulp)*(Resuv-resuv))/(Resus-resus)
! 
fi20=0
labxv=1
labd=0.3
labelh=matrix(3,values->(3.0,3.3,3.6))
is=0
lec=lenpchip-1
;do(il,1,lenpchip)

!oa=0.01*loss(il,All);
!lob=loss(il,All);
;if(il.eq.1);then
fi20=drawline(0.01*loss(il,All),0.01*gain(il,All),xrange->(0,5),
yrange->(0,5),show->is,color->colors(il),width->2,
xlabel->'Forest owner loss, €/m^3"font"')
;else
fi20=drawline(0.01*loss(il,All),0.01*gain(il,All),append->,show->is,color->colors(il),width->2)
;endif
lec=lenpchip
fi20=drawline(labxv,labxv+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
label->' P_{chip}= "pchips(il)""font"',append->,show->0)
!fi=drawline(labx,labx+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
!label->' P_{chip}= "pchips(il)""font"',append->,show->0)
!fi2=drawline(labx,labx+labd,laby-il*labdh,laby-il*labdh,color->colors(il),width->2,append->,
!label->' P_{chip}= "pchips(il)""font"',append->,show->0)
! ;if(il.eq.1);then
! gain00=0.01*gain(3)
! loss00=0.01*loss(3)
! ;endif
!fi=drawline(Psaws,resus(il,All),append->,show->0,color->colors(il),width->2)
;enddo
fi20=drawline(0,6,0,6,width->2,append->,show->is)
!fi20=drawline(0.01*loss(1,lenpchip),0.01*gain(1,lenpchip),append->,show->is,
!color->colors(1),width->2)
fi20=drawline(1,4.2,label->'Independent sawmills"font2"',append->,show->0)
!fi20=drawline(0.3,4.7,label->'Sawmill gain"font"',append->,show->0)
fi20=drawline(-0.42,4.7,label->'€/m^3  Sawmill gain"font"',append->,show->0)
xva=0.01*0.5*(loss(3,1)+loss(3,2));
yva=0.01*0.5*(gain(3,1)+gain(3,2));
fi20=drawline(xva,xva,yva,xva,append->,show->is,color->Orange,width->3)

fi20=drawline(xva+0.07,xva-0.1,label->'Dead weight loss"font"',append->,show->is,
append->,color->Orange,width->2)
show(fi20,set->'rmargin 0.2',append->,show->0)

!fi20=drawline(xlabel->'Forest owner loss, Euro/m^3"font"', 
! ylabel->'Sawmill gain, Euro/m^3"font"',append->,show->is)
show(fi20,continue->)

;cp:
;if(vtot.le.0)comvtot
cprice=matrix(do->(0,250,10))
lenc=len(cprice)

vee=matrix(3,lenc)
saaw=matrix(3,lenc)
saawu=matrix(3,lenc)

veea=matrix(3,lenc)
saawa=matrix(3,lenc)
saawua=matrix(3,lenc)
psaws=matrix(3,values->(180,220,260));

Lmin=40
Lmax=55
Dmin=15
dmin=5
!psaw=220
plog=75
ppulp=25
trc=trans()
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,Lmin).ge.Dmin)then
do(irow,1,3)
psaw=psaws(irow)
do(j,1,lenc)
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cprice(j)*u%totc+psaw*u%sawu))
vee(irow,j)=vee(irow,j)+u%logv
saaw(irow,j)=saaw(irow,j)+u%saw
saawu(irow,j)=saawu(irow,j)+u%sawu

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cprice(j)*u%totc+psaw*u%sawu-plog*u%logv-ppulp*u%pulp))
veea(irow,j)=veea(irow,j)+u%logv
saawa(irow,j)=saawa(irow,j)+u%saw
saawua(irow,j)=saawua(irow,j)+u%sawu
enddo
enddo
endif
enddo
/
call(trc)
vee=100.*vee/vtot;
saaw=100*saaw/vtot;
saawu=100*saawu/vtot;
veea=100.*veea/vtot;
saawa=100*saawa/vtot;
saawua=100*saawua/vtot;

cee=100-saaw;
ceea=100-saawa

;cp2:
colors=matrix(3,values->(Blue,Green,Red))
fi=0
;do(il,1,3)
!fi=drawline(cprice,vee(il,All),color->colors(il),show->0,width->2,append->)
!fi=drawline(cprice,saaw(il,All),color->colors(il),append->,show->0,width->2)
fi=drawline(cprice,cee(il,All),color->colors(il),append->,show->0,width->1)
!fi=drawline(cprice,saawu(il,All),color->colors(il),append->,show->0,width->2)
!fi=drawline(cprice,veea(il,All),color->colors(il),show->0,width->3,append->)
!fi=drawline(cprice,saawa(il,All),color->colors(il),append->,show->0,width->3)
fi=drawline(cprice,ceea(il,All),color->colors(il),append->,show->0,width->2)
! fi=drawline(cprice,saawua(il,All),color->colors(il),append->,show->0,width->3)
;enddo
show(fi)


inc=cee(All,2,-lenc)-cee(All,1);
inca=ceea(All,2,-lenc)-ceea(All,1);
decval=saawu(All,1)-saawu(All,2,-lenc)
pf=psaw*decval/.inc;
decvala=saawua(All,1)-saawua(All,2,-lenc)
pfa=psaw*decvala/.inca;

cprice2=cprice(2,-lenc)
fu=0
;do(il,1,3)

fu=drawline(cprice2,pf(il,All),color->colors(il),width->2,show->0,append->)

fu=drawline(cprice2,pfa(il,All),color->colors(il),append->,width->3,show->0,append->)
;enddo
show(fu)

fu=drawline(10,70,25,25,append->,continue->,width->2,show->0)
cep1=interpolate(pf(1,All),cprice2,25);
cep2=interpolate(pf(2,All),cprice2,25);
cep3=interpolate(pf(3,All),cprice2,25);
fu=drawline(cep1,cep1,25,0,append->,width->2,show->0)
fu=drawline(cep2,cep2,25,0,append->,width->2,show->0)
fu=drawline(cep3,cep3,25,0,append->,width->2,show->0)



;return

;cp3:
;if(vtot.le.0)comvtot
cprice=vector(30,40,50,60)
lenc=len(cprice)
plogs=matrix(do->(75,80,0.5))
lenp=len(plogs)



veea=matrix(3*lenp,lenc)
saawa=matrix(3*lenp,lenc)
saawua=matrix(3*lenp,lenc)
stumpa=matrix(3*lenp,lenc)
psaws=matrix(3,values->(180,220,260));



Lmin=40
Lmax=55
Dmin=15
dmin=5
!psaw=220
Plog=75
ppulp=25
trc=trans()
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,Lmin).ge.Dmin)then
iro=0

do(irow,1,3)
psaw=psaws(irow)
do(ip,1,lenp)
plog=plogs(ip)
iro=iro+1

do(j,1,lenc)


u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cprice(j)*u%totc+psaw*u%sawu-plog*u%logv-ppulp*u%pulp))
veea(iro,j)=veea(iro,j)+u%logv
saawa(iro,j)=saawa(iro,j)+u%saw
saawua(iro,j)=saawua(iro,j)+u%sawu
stumpa(iro,j)=stumpa(iro,j)+plog*u%logv+ppulp*u%pulp
enddo
enddo
enddo
endif
enddo
/
call(trc)
;cp4:
 pchips=matrix(3,values->(20,35,50))
 lencs=len(pchips)
 vee=matrix(3,lencs)
 saaw=matrix(3,lencs)
 saawu=matrix(3,lencs)
 stump=matrix(3,lencs)
obje=matrix(3,lencs)

trc2=trans()
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,Lmin).ge.Dmin)then


do(irow,1,3)
psaw=psaws(irow)
do(ic,1,lencs)


u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(pchips(ic)*u%chip+psaw*u%sawu-Plog*u%logv-ppulp*u%pulp))
vee(irow,ic)=vee(irow,ic)+u%logv
saaw(irow,ic)=saaw(irow,ic)+u%saw
saawu(irow,ic)=saawu(irow,ic)+u%sawu
stump(irow,ic)=stump(irow,ic)+Plog*u%logv+ppulp*u%pulp
obje(irow,ic)=obje(irow,ic)+u%obj

enddo
enddo
!vee,saaw,saawu,stump;
!pause('hdh ')
endif
enddo
/
call(trc2)

 vee=100.*vee/vtot;
 saaw=100*saaw/vtot;
 saawu=100*saawu/vtot;
veea=100.*veea/vtot;
saawa=100*saawa/vtot;
saawua=100*saawua/vtot;
stump=stump/vtot;
stumpa=stumpa/vtot;
obje=obje/vtot;
!cee=100-saaw;
ceea=100-saawa;





;return


;fp2:
 Window='450,450'
fifp=0
fu=0
sv=100*saaw/.vee;
svi=100*saawi/.veei;
svu=100*saawu/.vee;
sviu=100*saawui/.veei;
svmin=100*sawmin/logmin;

sv2=100*saaw2/.(vee2+0.000001);
svi2=100*saawi2/.(veei2+0.000001);
svu2=100*saawu2/.(vee2+0.0000001);
sviu2=100*saawui2/.(veei2+0.000001);

svmin2=100*sawmin2/(logmin2+0.000001);

fifp=drawline(cprice(1),cprice(lenc),logmin,logmin,append->,show->0,color->Cyan,yrange->(45,90))
fifp=drawline(cprice(1),cprice(lenc),logmax,logmax,append->,show->0,color->Cyan)
fifp=drawline(cprice(1),cprice(lenc),sawmin,sawmin,append->,show->0,color->Cyan)
fifp=drawline(cprice(1),cprice(lenc),sawmax,sawmax,append->,show->0,color->Cyan)
!fu=drawline(cprice(1),cprice(lenc),svmin,svmin,append->,show->0,color->Cyan)
s0=0
;do(il,1,3)
fifp=drawline(cprice,vee(il,All),color->colors(il),width->1,append->,show->0)
fifp=drawline(cprice,saaw(il,All),color->colors(il),width->1,append->,show->0)
nonz=find(vee2(il,All),filter->$.lt.50)-1
;if(nonz.le.0)nonz=lenc
fifp=drawline(cprice(1,-nonz),vee2(il,1,-nonz),color->colors(il),width->2,append->,show->s0)

nonz=find(saaw2(il,All),filter->$.lt.5)-1
;if(nonz.le.0)nonz=lenc
fifp=drawline(cprice(1,-nonz),saaw2(il,1,-nonz),color->colors(il),width->2,append->,show->s0)
!fi=drawline(cprice,saaw(il,All),color->colors(il),width->2,append->,show->0)
fifp=drawline(cprice,veei(il,All),color->colors(il),width->1,append->,show->0)
nonz=find(veei2(il,All),filter->$.lt.50)-1
;if(nonz.le.0)nonz=lenc
fifp=drawline(cprice(1,-nonz),veei2(il,1,-nonz),color->colors(il),width->2,append->,show->s0)
!fi=drawline(cprice,saawi(il,All)+5,color->colors(il),width->2,append->,show->0)

fu=drawline(cprice,sv(il,All),color->colors(il),width->1,append->,show->s0)
fu=drawline(cprice,svi(il,All),color->colors(il),width->1,append->,show->s0)
nonz=find(vee2(il,All),filter->$.lt.50)-1
;if(nonz.le.0)nonz=lenc
fu=drawline(cprice(1,-nonz),sv2(il,1,-nonz),color->colors(il),width->2,append->,show->s0)
nonz=find(veei2(il,All),filter->$.lt.50)-1
;if(nonz.le.0)nonz=lenc
fu=drawline(cprice(1,-nonz),svi2(il,1,-nonz),color->colors(il),width->2,append->,show->s0)
!fu=drawline(cprice,svu(il,All),color->colors(il),width->2,append->,show->0)
!fu=drawline(cprice,sviu(il,All),color->colors(il),width->1,append->,show->0)

;enddo
fifp=drawline(-13,83,label->'%"font"',append->,show->0)
fu=drawline(-13,62.5,label->'%"font"',append->,show->0)
show(fifp,continue->,xlabel->'P_{chip}"font"')

show(fu,continue->,xlabel->'P_{chip}"font"',yrange->(56,64))

;return
;splog:
;if(type(pinedee).ne.MATRIX)rpine

Lmin=40
Lmax=55
Dmin=15
dmin=5
psaws=vector(180,220,260)
lom=matrix(nrows(pinedee))
som=matrix(nrows(pinedee))
soum=matrix(nrows(pinedee))
dbhm=matrix(nrows(pinedee))
solom=matrix(nrows(pinedee))
soloum=matrix(nrows(pinedee))
obm=matrix(nrows(pinedee))
comm=matrix(nrows(pinedee))
totcm=matrix(nrows(pinedee))
nob=0
trs=trans()
irow=2
pchip=40

nob=0
do(iobs,1,nrows(pinedee))

if(pinedee(iobs,Lmin).ge.Dmin)then
nob=nob+1
dbhm(nob)=pinedee(iobs,13)
!do(irow,1,3)

psaw=psaws(irow)



u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",any->0,
func->"Func",obj->(psaw*u%sawu+pchip*u%totc))

lom(nob)=u%logv
som(nob)=u%saw
soum(nob)=u%sawu
obm(nob)=u%obj
solom(nob)=u%saw/u%logv
soloum(nob)=u%sawu/u%logv
totcm(nob)=u%totc
comm(nob)=u%com
endif
enddo
/
call(trs)
solom=100*solom
soloum=100*soloum
nob;
!;splog2:
spd=newdata(dbhm(1,-nob),lom(1,-nob),som(1,-nob),solom(1,-nob),
soum(1,-nob),soloum(1,-nob),obm(1,-nob),totcm(1,-nob),comm(1,-nob),read->(dbh,lo,so,solo,sou,solou,obj,totc,com))

;splog2:
Window='450,450'
stat(min->,max->)
c=classify(solou,data->spd,x->solo,xrange->,classes->14,minobs->7)
c2=classify(sou,data->spd,x->so,xrange->,classes->14,minobs->7)
fo=drawclass(c,sd->,se->,width->(1,2),color->Blue,continue->,show->0)
fo2=drawclass(c2,sd->,se->,width->(1,2),color->Blue,continue->,show->0)
re=regr(solou,solo)
fo=drawline(solo%min,solo%max,re(solo%min),re(solo%max),append->,show->0,color->Red,width->2)
re=regr(solou,solo,noint->)
fo=drawline(solo%min,solo%max,re(solo%min),re(solo%max),append->,show->0,color->Green,width->2)



re=regr(sou,so)
fo2=drawline(so%min,so%max,re(so%min),re(so%max),append->,show->0,color->Red,width->2)
re=regr(sou,so,noint->)
fo2=drawline(so%min,so%max,re(so%min),re(so%max),append->,show->0,color->Green,width->2)

fo=drawline(37,41,70,70,label->' without intercept"font"',append->,show->0,width->2,color->Green)
fo=drawline(37,41,67,67,label->' with intercept"font"',append->,show->0,width->2,color->Red)

show(fo,continue->,xrange->(35,75),yrange->(35,75),
title->'V_{sawu}/V_{log} | V_{saw}/V_{log}"font"',xlabel->'V_{saw}/V_{log}, %"font"',
ylabel->'V_{sawu}/V_{log}, %"font"')

fo2=drawline(200,300,1200,1200,label->' without intercept"font"',append->,show->0,width->2,color->Red)
fo2=drawline(200,300,1100,1100,label->' with intercept"font"',append->,show->0,width->2,color->Green)

show(fo2,continue->,
title->'V_{sawu} | V_{saw}"font"',xlabel->'V_{saw}, dm^3"font"',ylabel->'V_{sawu}, dm^3"font"')

ree=regr(solou,solo,dbh)
ree=regr(solou,solo,com)
ree=regr(solou,solo,com,dbh)

ree=regr(sou,so,dbh)
ree=regr(sou,so,com)
ree=regr(sou,so,com,dbh)
Window='900,450'
fotot=show(fo,fo2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)

;return

re=regr(obj,so,totc)
re2=regr(obj,sou,totc)
;splog3:
Window='450,450'
c=classify(solo,data->spd,x->dbh,xrange->,dx->2,minobs->7)
c2=classify(solou,data->spd,x->dbh,xrange->,dx->2,minobs->7)
c2b=c2
c2b(2,All)=c2b(2,All)+0.4
fo=drawclass(c,sd->,se->,width->(1,2),color->Blue,continue->,show->0)
fo=drawclass(c2b,sd->,se->,width->(1,2),color->Red,continue->,append->,show->0)
fo=drawline(19,22,72,72,label->' V_{sawu}/V_{log}"font"',append->,show->0,width->2,color->Red)
fo=drawline(19,22,69,69,label->' V_{saw}/V_{log}"font"',append->,show->0,width->2,color->Blue)
fo=drawline(12.5,72.5,label->'%"font"',append->,show->0)
show(fo,xlabel->'dbh, cm"font"',title->'V_{sawu}/V_{log} and V_{saw}/V_{log}"font"',continue->)

c=classify(solo,data->spd,x->com,xrange->,classes->14,minobs->7)
c2=classify(solou,data->spd,x->com,xrange->,classes->14,minobs->7)
c2b=c2
c2b(2,All)=c2b(2,All)+0.4
fo2=drawclass(c,sd->,se->,width->(1,2),color->Blue,continue->,show->0)
fo2=drawclass(c2b,sd->,se->,width->(1,2),color->Red,continue->,append->,show->0)
fo2=drawline(300,390,72,72,label->' V_{sawu}/V_{log}"font"',append->,show->0,width->2,color->Red)
fo2=drawline(300,390,69,69,label->' V_{saw}/V_{log}"font"',append->,show->0,width->2,color->Blue)
fo2=drawline(120,72.5,label->'%"font"',append->,show->0)
show(fo2,xlabel->'V_{com}, dm^3"font"',title->'V_{sawu}/V_{log} and V_{saw}/V_{log}"font"',continue->)
Window='900,450'
fotot=show(fo,fo2,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)
;return

clo=classify(lo,data->spd,x->dbh,xrange->,dx->2,minobs->7)


cl=classify(solo,data->spd,x->dbh,xrange->,dx->2,minobs->7)
fo=drawclass(cl,sd->,se->,width->(1,2),color->Blue,continue->,show->0)
cl2=classify(solou,data->spd,x->dbh2,xrange->,dx->2,minobs->7)
fo=drawclass(cl2,sd->,se->,width->(1,2),color->Red,continue->,append->)
fo=drawclass(cl(,sd->,se->,width->(1,2),color->Red,continue->,append->)

;return
clu=classify(solou,data->spd,x->dbh,xrange->,dx->2,minobs->7)
fou=drawclass(clu,sd->,se->,width->(1,2),color->Red,continue->)

!re1=regr(so1,lo1)
!re2=regr(so2,lo2)
!re3=regr(so3,lo3)
!fo=plotyx(solo1,lo1)

;return
;fp36:
 Window='450,450'
fifp2=fifp
ht=vector(64,67,70)
;do(il,1,3)
fifp2=drawline(40,60,ht(il),ht(il),label->' P_{saw}= "psaws(il)""font"',
color->colors(il),width->2,append->,show->0)
;enddo

show(fifp2,set->('arrow from 100,81 to 100,88'),show->0)
fifp2=drawline(0,83,label->'V_{log}/V_{com}"font"',append->,show->0)
fifp2=drawline(50,80,label->'Independent sawmills"font"',append->,show->0)
fifp2=drawline(20,54,label->'V_{saw}/V_{com}   Sawmills with pulp"font"',append->,show->0)
fifp2=drawline(10,77,label->'Sawmills with pulp"font"',append->,show->0)
show(fifp2,set->('arrow from 46,78 to 46,86'))
;return
;fp32:
 Window='450,450'
fu2=fu

fu2=drawline(7,63.5,label->'V_{saw}/V_{log}"font"',show->0,append->)
ht2=vector(61.5,62,62.5)
;do(il,1,3)
fu2=drawline(40,60,ht2(il),ht2(il),label->' P_{saw}= "psaws(il)""font"',
color->colors(il),width->2,append->,show->0)
;enddo

fu2=drawline(50,60.1,label->'style(rotate by 15)Sawmills with pulp"font"',append->,
continue->,show->0)

fu2=drawline(50,59.2,label->'style(rotate by -20)Independent sawmills"font"',append->,
continue->)

;return
;fp3:
Window='450,450'

fifp2b=fifp2
show(fifp2b,set->'rmargin 2;lmargin 4',show->0)
fu2b=fu2
show(fu2b,set->'rmargin 2;lmargin 4',show->0)
Window='900,450'
!show(fi20b,set->'size square',show->0)
!Fi20b=show(Fi20b,set->'lmargin 0',show->0) !unset->'ylabel',show->0)
!set ytics format "" '
!Fi20b=show(Fi20b,set->'ytics format "" ;lmargin 0',ylabel->'. ',show->0) !unset->'ylabel',show->0)

Fifp=show(fifp2b,fu2b,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)

;return

;return





;fig7:
Window='900,450'

	figg8=show(fi3,fuu,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)




;return
;fig8:
Window='900,450'

	fig8=show(fstu,fil,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)



;return










;return
Obj=Obj/vtot;
Obji=Obji/vtot;
obj=obj/vtot;
obji=obji/vtot;

Obj=Obj/100
obj=obj/100
obji=obji/100
Obji=Obji/100

;fa:
 Window='450,300'
obero=100*(Obj-obj)/.obj;
oberoi=100*(Obji-obji)/.obji;



vero=100*(Vee-vee)/.vee;
sero=100*(Saaw-saaw)/.saaw;
veroi=100*(Veei-veei)/.veei;
seroi=100*(Saawi-saawi)/.saawi;
stumpero=100*(Stump-stump)/.stump
stumperoi=100*(Stumpi-stumpi)/.stumpi



forstump=0
forstumpi=0
;do(il,1,3)
forstump=drawline(cprice,stumpero(il,All),append->,
color->colors(il),width->2,show->0)
forstumpi=drawline(cprice,stumperoi(il,All),append->,
color->colors(il),width->2,show->0)
;enddo
show(forstump,continue->,yrange->(-12,1),continue->)

show(forstumpi,yrange->(-12,1))

forob=0
forobi=0
;do(il,1,3)
forob=drawline(cprice,obero(il,All),append->,
color->colors(il),width->2,show->0)
forobi=drawline(cprice,oberoi(il,All),append->,
color->colors(il),width->2,show->0)
;enddo
show(forob,continue->,yrange->(-12,1),continue->)
show(forobi,yrange->(-12,1))

! ;return
fve=0
;do(il,1,3)
fve=drawline(cprice,vero(il,All),append->,yrange->(-12,1),
color->colors(il),width->2,show->0)
;enddo
show(fve)
fse=0
;do(il,1,3)
fse=drawline(cprice,sero(il,All),append->,yrange->(-12,1),
color->colors(il),width->2,show->0)
;enddo
show(fse)
fvei=0
;do(il,1,3)
fvei=drawline(cprice,veroi(il,All),append->,yrange->(-12,1),
color->colors(il),width->2,show->0)
;enddo
show(fvei)
fsei=0
;do(il,1,3)
fsei=drawline(cprice,seroi(il,All),append->,yrange->(-12,1),
color->colors(il),width->2,show->0)
;enddo
show(fsei)

;return
;fa2:

 fvei2=fvei
 fve2=fve
 fsei2=fsei
 fse2=fse
 forstumpi2=forstumpi
 forstump2=forstump
 forobi2=forobi
 forob2=forob
 Window='450,300'
 margin='bmargin 2;tmargin 1;lmargin 3.5;rmargin 1'
 so=0
  fvei2=drawline(2,0,label->'Independent sawmills"font"',append->,show->so)
	fve2=drawline(2,0,label->'Sawmills with pulp"font"',append->,show->so)
	fvei2=drawline(30,-8,label->'Decrease of V_{log}, % "font"',append->,show->so)
	fsei2=drawline(30,-8,label->'Decrease of V_{saw}, % "font"',append->,show->so)
	forstumpi2=drawline(30,-8,label->'Decrease of stumpage price, % "font"',append->,show->so)
	forobi2=drawline(5,-8,label->'Dead weight loss, decrease of objective, % "font"',append->,show->so)
	

	
 fvei2=show(fvei2,set->'"margin"',show->so)
 fve2=show(fve2,set->'"margin"',show->so)
 fsei2=show(fsei2,set->'"margin"',show->so)
 fse2=show(fse2,set->'"margin"',show->so)
 forstumpi2=show(forstumpi2,set->'"margin"',show->so)
 forstump2=show(forstump2,set->'"margin"',show->so)
 forobi2=show(forobi2,set->'"margin"',show->so)
 forob2=show(forob2,set->'"margin"',show->so)
 
  Window='900,900'
 fafi2=show(fvei2,fve2,fsei2,fse2,forstumpi2,forstump2,forobi2,forob2,
 multiplot->'set size 1,1;set origin 0,0;set multiplot layout 4,2 scale 1,1',continue->)

;return



;fig8:
 Window='900,450'
eurof=show(fstu,fil,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)


;return
! sawmill with pulp get the difference of sawnwood more chips
! the price in free bucking 
;fa3c:
!Saaw obbj->(cprice(j)*u%totc+psaw*u%sawu-plog*u%logv-ppulp*u%pulp)
 cee=100-saaw;
 ceei=veei-saawi;
 ceeitot=100-saawi;
 Cee=100-Saaw;
 Ceei=Veei-Saawi;
 Ceeitot=100-Saawi;
colors=matrix(3,values->(Blue,Green,Red))
fi=0
Fi=0
;do(il,1,3)
!fi=drawline(cprice,vee(il,All),color->colors(il),show->0,width->2,append->)
!fi=drawline(cprice,saaw(il,All),color->colors(il),append->,show->0,width->2)
fi=drawline(cprice,cee(il,All),color->colors(il),append->,show->0,width->1)

fi=drawline(cprice,Cee(il,All),color->colors(il),append->,show->0,width->1)
!fi=drawline(cprice,saawu(il,All),color->colors(il),append->,show->0,width->2)
!fi=drawline(cprice,veea(il,All),color->colors(il),show->0,width->3,append->)
!fi=drawline(cprice,saawa(il,All),color->colors(il),append->,show->0,width->3)
fi=drawline(cprice,ceei(il,All),color->colors(il),append->,show->0,width->2)
fi=drawline(cprice,ceeitot(il,All),color->colors(il),append->,show->0,width->2)

fi=drawline(cprice,Ceei(il,All),color->colors(il),append->,show->0,width->2)
fi=drawline(cprice,Ceeitot(il,All),color->colors(il),append->,show->0,width->2)
! fi=drawline(cprice,saawua(il,All),color->colors(il),append->,show->0,width->3)
;enddo
show(fi,title->'fi free paksu inde')
!show(Fi,title->'as free paksu inde')
;return





;fig10:
Window='900,450'

fifo=show(fi,fu,multiplot->'set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1',continue->)

;return




;table2:
!**** Table 2 can be picked from the following output
onetwo



;return



uu2=cpri*U%totc+psaw*U%sawu;
u%obj,uu2,(u%obj-uu2)/u%obj;


;return
U%lens;
u%lens;
v%lens;
U%lencs;
u%lencs;
v%lencs;
u%logv,U%logv,v%logv;
u%sawu,U%sawu,v%sawu;
u%saw,U%saw,v%saw;
u%nlog,U%nlog,v%nlog;
show(foo,continue->)
;return



colors=matrix(3,values->(Blue,Green,Red))

! Vee=matrix(3,lenc)
! Saaw=matrix(3,lenc)
! Saawu=matrix(3,lenc)
! Stump=matrix(3,lenc)
! Obj=matrix(3,lenc)
! Veei=matrix(3,lenc)
! Saawi=matrix(3,lenc)
! Saawui=matrix(3,lenc)
! Stumpi=matrix(3,lenc)
! Obji=matrix(3,lenc)
 psaws=matrix(3,values->(180,220,260));

Lmin=40
Lmax=55
Dmin=15
dmin=5
!psaw=220
plog=75
ppulp=25
vemax=matrix(3)

stumpobs=matrix(3)
nomax=matrix(3)
cpris=matrix(do->(10,120,10))
lec=len(cpris)
psaw=psaws(irow)


trc4=trans()
do(ic,1,lec)
cpri=cpris(ic);
do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,Lmin).ge.Dmin)then


!do(irow,1,3)


U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*U%totc+psaw*U%sawu-plog*U%logv-ppulp*U%pulp))

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*u%totc+psaw*u%sawu))

v=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, weight->"Weight",
func->"Func",obj->(cpri*v%chip+psaw*v%sawu-plog*v%logv))  !in
if(U%nlog.eq.1.and.u%nlog.eq.2.and.v%nlog.eq.2)then
!if(u%vlog-U%vlog.gt.vemax(irow))then
iobs;
u%logv,U%logv,v%logv;
u%sawu,U%sawu,v%sawu;
u%nlog,U%nlog,v%nlog;

pause('<>')
!endif
endif

endif
enddo
enddo
/
call(trc4)
vemax;
nomax;
stumpobs;




;return
fi=0
fu=0
fui=0
fu2=0
foloss=100*(stump-Stump);
soloss=obj-Obj
folossi=100*(stumpi-Stumpi)
solossi=obji-Obji
gain=foloss-soloss
gaini=folossi-solossi

;do(il,1,3)
fi=drawline(cprice,stump(il,All),color->colors(il),width->4,append->,show->0)
fi=drawline(cprice,stumpi(il,All),color->colors(il),width->2,append->,show->0)
fi=drawline(cprice,Stump(il,All),color->colors(il),width->4,append->,show->0)
fi=drawline(cprice,Stumpi(il,All),color->colors(il),width->2,append->,show->0)
fu=drawline(cprice,stump(il,All)-Stump(il,All),color->colors(il),width->3,append->,show->0)
fu=drawline(cprice,obj(il,All)-Obj(il,All),color->colors(il),width->2,append->,show->0)
fu2=drawline(cprice,stumpi(il,All)-Stumpi(il,All),color->colors(il),width->3,append->,show->0)
fu2=drawline(cprice,obji(il,All)-Obji(il,All),color->colors(il),width->2,append->,show->0)

;enddo
;fa2:
show(fi,continue->)
show(fu,continue->,yrange->(0,$))
show(fu2,continue->,yrange->(0,$))


;return
chi=100-resusc;  !obtaine chips 
chipot=100-sawmax; !chips in potential
chigain=chi-chipot;  !differencee
loss=(plog-ppulp)*(Resuvc-resuvc);  !forest ownere
sawloss=psaws*.(sawmax-resusc);
nume=sawloss-loss;
prici=nume/.chigain;
fu=0
;do(il,1,3)
fu=drawline(chigain(il,All),prici(il,All),append->,color->colors(il),show->0)
;enddo
show(fu,continue->)
;return

;uus:
asp=plog*Resuv;
Asp=plog*resuv;

fo=0
fo2=0
fo3=0
fo4=0

;do(il,1,lenpchip)
!fo=drawline(asp(il,All),Resuv(il,All),append->,show->0)
fo2=drawline(Asp(il,All),Resus(il,All),append->,show->0,color->Red)
fo3=drawline(Asp(il,All),resus(il,All),append->,show->0,color->Blue)
fo4=drawline(Asp(il,All),resuv(il,All),append->,show->0,color->Green)
;enddo
!show(fo,continue->)
show(fo2,continue->)
show(fo3,continue->)
show(fo4,continue->)

;return





!perk=';incl(stemf.inc,from->perk)'
;perk:
Obj='u%saw'
deftrans
call(tranco)
vol;
saw;
sawu;
Obj='u%logv';
deftrans
call(tranco)
vol;
saw;
sawu;
;return



;compare3:

summ=matrix(3,1,matrix->,values->(revu2(5,All),resu2(5,All),reva2(5,All)));


;return

eromax=reva2(All,2)-reva2(All,7);
eromaxp=100*eromax/.reva2(All,2);
erovalm=reva2(All,5)-reva2(All,4);
erovalmp=100*erovalm/.reva2(All,4);
erovalm2=reva2(All,7)-reva2(All,6);
erovalmp2=erovalm2/.reva2(All,6);
pause('<g')

erovolm=revu2(All,5)-revu2(All,4);
erovolmp=erovolm/.revu2(All,4);
erovolm2=revu2(All,7)-revu2(All,6);
erovolmp2=erovolm2/.revu2(All,6);

;return
fi3=drawline(48,56,48,56,width->2,show->0)
fi3=drawline(reva2(All,2),reva2(All,1),color->Blue,show->0,append->)
fi3=drawline(reva2(All,2),reva2(All,7),color->Red,show->0,append->)
show(fi3,continue->)
ero=reva2(All,2)-reva2(All,7);


;return
;table1:
;if(type(pinedee).ne.DATA)rpine
Maxlines=23
lmin=37
lmax=55
dmin=15
dmin=5
lmins=matrix(3,values->(37,40,43))
lmaxs=matrix(3,values->(52,55,58))
dmins=matrix(3,values->(14,15,16))

resu=matrix(9,8)

reslist=list(u%logv,u%cyl,u%saw,u%inc,u%outc,
u%dtop,u%lenn,u%nlog)  !mlen mean lenthg
resv=matrix(8)
veesum=0

trana=trans()

nobr=matrix(9)
nobr2=matrix(9)  !number of logs
nob=0
do(iobs,1,nrows(pinedee))
if((100*int(iobs/100)).eq.iobs)iobs;
if(pinedee(iobs,lmins(1)).lt.dmins(1))goto(nex)
!top=find(pinedee(iobs,All),filter->$.lt.dmin)
!veesum=veesum+pinevee(iobs,top-1)  !commercial volume
nob=nob+1

Lmin=lmins(1)
Lmax=lmaxs(1)
Dmin=dmins(1)

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs,func->"Func",weight->"Weight",
obj->u%sawu)
!u%nlog;
! if(u%nlog.gt.2)then
! u%vars;
! print(u%vars,expand->)
! pause('<cc>')
! endif
if(u%nlog.le.0)goto(nex)
veesum=veesum+u%com
nobr(2)=nobr(2)+1
nobr2(2)=nobr2(2)+u%nlog

resv=reslist

resu(2,All)=resu(2,All)+resv

!mean
Lmin=lmins(2)
Lmax=lmaxs(2)
Dmin=dmins(2)

u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs,
func->"Func",obj->u%saw)
if(u%nlog.le.0)goto(start)
resv=reslist

resu(1,All)=resu(1,All)+resv
nobr(1)=nobr(1)+1
nobr2(1)=nobr2(1)+u%nlog

start:irow=2
do(le1,1,3,2)
Lmin=lmins(le1)
do(le2,1,3,2)
Lmax=lmaxs(le2)
do(dm,1,3,2)
Dmin=dmins(dm)
if((le1+le2+dm).ne.3)then
irow=irow+1
u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs,
func->"Func",obj->u%saw)
if(u%nlog.gt.0)then

resu(irow,All)=resu(irow,All)+resv
nobr(irow)=nobr(irow)+1
nobr2(irow)=nobr2(irow)+u%nlog
!write('resutot.bin','B',@totlist)
!if(u%data(u%dtop,row->u%saw%maxobs).lt.dmins(dm))pause('here>')


endif
endif



enddo
enddo
enddo

nex:enddo
nob,nob2;
resu;
logsum=resu(All,1)

resu(All,1)=100*resu(All,1)/veesum  !logvolume / commercial volume
resu(All,2,-5)=100*resu(All,2,-5)/veesum  !cylinder,saw,in,out
resu(All,6)=resu(All,6)/.nobr  ! dtop
resu(All,7)=resu(All,7)/.nobr2  ! len
resu(All,8)=resu(All,8)/.nobr
!close('resutot.bin')
/
Maxlines=60
call(trana)

resu;

;return
;table1b:
head=matrix(9,3)
head(1,1)=lmins(2)
head(1,2)=lmaxs(2)
head(1,3)=dmins(2)
irow=1
;do(le1,1,3,2)  !Lmin
;do(le2,1,3,2)  !Lmax
;do(dm,1,3,2)   !Dimin

irow=irow+1
head(irow,1)=lmins(le1)
head(irow,2)=lmaxs(le2)
head(irow,3)=dmins(dm)

;enddo
;enddo
;enddo

head;
total=matrix(1,2,matrix->,values->(head,resu(All,1,-8)));
;if(exist_f('table1.txt'))delete_f('table1.txt')
write('table1.txt','(f3.0,2f4.0,8f5.1)',total)
close('table1.txt')
print_f('table1.txt')
! ;if(exist_f('ana1b.txt'))delete_f('ana1b.txt')
! write('ana1b.txt','(3f4.0,8f8.4)',total)
! close('ana1b.txt')
! print_f('ana1b.txt')

;return


;table2:  compare different objectives

Maxlines=23
lmin=37
lmax=55
dmin=15
dmin=5
lmins=matrix(3,values->(37,40,43))
lmaxs=matrix(3,values->(52,55,58))
dmins=matrix(3,values->(14,15,16))


resu=matrix(9,12)  !maxlog log/veesum saw/veesum   minlog  maxsaw/log maxsaw maxlog

veesum=0

trana=trans()

! { nobr=matrix(9) }
! { nobr2=matrix(9)  !number of logs }
nob=0

do(iobs,1,nrows(pinedee))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,lmins(1)).lt.dmins(1))goto(nex) 


u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs,
func->"Func",obj->(-u%logv-0.0001*u%saw))  !,print->)  !(-u%logv+1000))

if(u%nlog.le.0)goto(nex)

veesum=veesum+u%com

nob=nob+1

resu(2,1)=resu(2,1)+u%logv
resu(2,2)=resu(2,2)+u%saw

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs,
func->"Func",obj->(-u%logv+0.0001*u%saw))  !,print->)  !(-u%logv+1000))





resu(2,3)=resu(2,3)+u%logv
resu(2,4)=resu(2,4)+u%saw

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs,
func->"Func",obj->(u%saw/u%logv))

resu(2,5)=resu(2,5)+u%logv
resu(2,6)=resu(2,6)+u%saw

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs,
func->"Func",obj->u%saw)

resu(2,7)=resu(2,7)+u%logv
resu(2,8)=resu(2,8)+u%saw

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs,
func->"Func",obj->u%logv-0.00001*u%saw)

resu(2,9)=resu(2,9)+u%logv
resu(2,10)=resu(2,10)+u%saw

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs,
func->"Func",obj->u%logv+0.00001*u%saw)

resu(2,11)=resu(2,11)+u%logv
resu(2,12)=resu(2,12)+u%saw


!***
u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs,
func->"Func",obj->(-u%logv-0.0001*u%saw))  !!(-u%logv+10000))
!if(u%nlog.le.0)goto(start)

resu(1,1)=resu(1,1)+u%logv
resu(1,2)=resu(1,2)+u%saw

u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs,
func->"Func",obj->(-u%logv+0.0001*u%saw))  !!(-u%logv+10000))
!if(u%nlog.le.0)goto(start)

resu(1,3)=resu(1,3)+u%logv
resu(1,4)=resu(1,4)+u%saw



u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs,
func->"Func",obj->(u%saw/u%logv))
!if(u%nlog.le.0)goto(start)

resu(1,5)=resu(1,5)+u%logv
resu(1,6)=resu(1,6)+u%saw

u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs,
func->"Func",obj->(u%saw))
!if(u%nlog.le.0)goto(start)

resu(1,7)=resu(1,7)+u%logv
resu(1,8)=resu(1,8)+u%saw



u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs,
func->"Func",obj->(u%logv-0.001*u%saw))
!if(u%nlog.le.0)goto(start)

resu(1,9)=resu(1,9)+u%logv
resu(1,10)=resu(1,10)+u%saw

u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs,
func->"Func",obj->(u%logv+0.001*u%saw))
!if(u%nlog.le.0)goto(start)

resu(1,11)=resu(1,11)+u%logv
resu(1,12)=resu(1,12)+u%saw


start:irow=2
do(le1,1,3,2)

do(le2,1,3,2)

do(dm,1,3,2)

if((le1+le2+dm).ne.3)then
irow=irow+1
u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs,
func->"Func",obj->(-u%logv-0.0001*u%saw))   !-u%logv+10000))


resu(irow,1)=resu(irow,1)+u%logv
resu(irow,2)=resu(irow,2)+u%saw

u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs,
func->"Func",obj->(-u%logv+0.0001*u%saw))   !-u%logv+10000))


resu(irow,3)=resu(irow,3)+u%logv
resu(irow,4)=resu(irow,4)+u%saw

u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs,
func->"Func",obj->(u%saw/u%logv))
resu(irow,5)=resu(irow,5)+u%logv
resu(irow,6)=resu(irow,6)+u%saw


u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs,
func->"Func",obj->u%saw)
resu(irow,7)=resu(irow,7)+u%logv
resu(irow,8)=resu(irow,8)+u%saw


u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs,
func->"Func",obj->(u%logv-0.0001*u%saw))
resu(irow,9)=resu(irow,9)+u%logv
resu(irow,10)=resu(irow,10)+u%saw


u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs,
func->"Func",obj->u%logv)
resu(irow,11)=resu(irow,11)+u%logv
resu(irow,12)=resu(irow,12)+u%saw

endif

enddo
enddo
enddo



nex:enddo
nob;
resu;
!logsum=resu(All,1)
resu=100*resu/veesum;
! resu(All,1)=100*resu(All,1)/veesum  !logvolume / commercial volume
! resu(All,2,-5)=100*resu(All,2,-5)/veesum  !cylinder,saw,in,out
! resu(All,6)=resu(All,6)/.nobr  ! dtop
! resu(All,7)=resu(All,7)/.nobr2  ! len
! resu(All,8)=resu(All,8)/.nobr
!close('resutot.bin')
/
Maxlines=60
call(trana)

resu;

***finetuning of table2

;table2b:
head=matrix(9,3)
head(1,1)=lmins(2)
head(1,2)=lmaxs(2)
head(1,3)=dmins(2)
irow=1
;do(le1,1,3,2)
;do(le2,1,3,2)
;do(dm,1,3,2)
irow=irow+1
head(irow,1)=lmins(le1)
head(irow,2)=lmaxs(le2)
head(irow,3)=dmins(dm)
;enddo
;enddo
;enddo
head;
total=matrix(1,2,matrix->,values->(head,resu(All,All)));
;if(exist_f('range2.txt'))delete_f('range2.txt')
write('range2.txt','(f3.0,2f4.0,12f5.1)',total)
close('range2.txt')
print_f('range2.txt')
! ;if(exist_f('ana1b.txt'))delete_f('ana1b.txt')
! write('ana1b.txt','(3f4.0,8f8.4)',total)
! close('ana1b.txt')
! print_f('ana1b.txt')
;return




;return
fi2=drawline(loss00,loss00,gain00,loss00,width->2,color->Orange,show->0,append->)
fi2=drawline(loss00+0.02,1.8,
label->'Black hole"font"',show->0,append->)

fi2=drawline(0,4,0,4,append->,width->2,show->0,
xlabel->'Forest owner loss /m^3"font"',
ylabel->'Sawmill gain /m^3"font"')
show(fi2,continue->)
;return





;return






;return


;fig2c:

Lmin=40
Lmax=55
Dmin=15
dmin=5

trani0=trans() !like trana in ana7
saw=0
sawu=0
chip=0
logv=0
obj=0

Saw=0
Sawu=0
Chip=0
Logv=0
Obj=0

vtot=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee(iobs,All),
func->"Func",obj->u%saw,weight->"Weight")    !,

vtot=vtot+u%com

!pause('<>')
saw=saw+u%saw
sawu=sawu+u%sawu
chip=chip+u%chip
vol=vol+u%logv
obj=obj+u%obj
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee(iobs,All),
func->"Func",obj->U%sawu,weight->"Weight")   !,

Saw=Saw+U%saw
Sawu=Sawu+U%sawu
Chip=Chip+U%chip
Vol=Vol+U%logv
Obj=Obj+U%obj

endif

enddo
/

cpu0=cpu()
!;do(k,1,100)
call(trani0)
!;enddo
cpu1=cpu()

cpuu=cpu1-cpu0;

!;return

saw,sawu; 
Saw,Sawu;
h=Saw/Sawu;
saw/Saw,sawu/Sawu;

;return



x1=20
y1=0.77
x2=30
y2=0.82


Y2=0.85

fi=plotyx(f0,D,mark->3,yrange->(0.55,0.95),show->0,color->Blue)
fi=drawline(x1,x2,y1,y2,pointsonly,mark->1,marksize->4,show->0,append->)

fi=draw(func->log(((x2-x)/(x2-x1))*exp(y1)+((x-x1)/(x2-x1))*exp(y2)),width->2,x->x,color->Blue,append->,show->0,xrange->(15,50),
xlabel->'D_{top}, cm"font"',ylabel->'f(D_{top})"font"')

fi=draw(func->(y1*(1-(Y2/y1-1)**(x/x1))/(2-Y2/y1)),width->2,x->x,color->Red,append->,show->0,xrange->(15,50))

fi=draw(func->(y1*(y2/y1)**(log(x/x1)/log(x2/x1))),width->2,x->x,color->Green,append->,show->0,xrange->(15,50))

fi=draw(func->(x*(x2-x1)*y1*y2/((x-x1)*x2*y1+(x2-x)*x1*y2)),width->2,x->x,color->Orange,append->,show->0,xrange->(15,50))

show(fi)
;return

;fig2c:
if(type(setdat).ne.DATA)fig2
x1=15
y1=0.72
x2=30
y2=0.81

!  x-x1+exp(y1/b)>0
!  15-20+exp(0.77/b)>0
! exp(0.77/b)>5
! 0.77/b>1.6  0.77>b*1.6   b< 0.77/1.6  b<0.48
fi=plotyx(f0,D,mark->3,yrange->(0.55,0.95),show->0,color->Blue,data->setdat)
fi=draw(func->(y1*(y2/y1)**(log(x/x1)/log(x2/x1))),width->2,x->x,color->Green,append->,show->0,xrange->(15,50))

fi=draw(func->(x*(x2-x1)*y1*y2/((x-x1)*x2*y1+(x2-x)*x1*y2)),width->2,x->x,color->Orange,append->,show->0,xrange->(15,50))

! b=0.23
! fi=draw(func->(b*log(x-x1+exp(y1/b))),width->2,x->x,color->Orange,append->,show->0,xrange->(15,50))
! b=0.15
! fi=draw(func->(b*log(x-x1+exp(y1/b))),width->2,x->x,color->Red,append->,show->0,xrange->(15,50))
show(fi,continue->)
;return

;fig2:
;if(type(setdat).ne.DATA)fig2
! x1=15
! y1=1.1  !10*1.35/48.5  ;  !10*arvop/48.5
! x2=30
! y2=1.3  !10*1.53/48.5;

!  x-x1+exp(y1/b)>0
!  15-20+exp(0.77/b)>0
! exp(0.77/b)>5
! 0.77/b>1.6  0.77>b*1.6   b< 0.77/1.6  b<0.48
fi=plotyx(f0,D,mark->3,show->0,color->Blue,data->setdat)

fi=plotyx(arvop,D,mark->3,show->0,color->Red,until->8,append->,data->setdat)

sh=0
fi=drawline(x1,x2,y1,y2,pointsonly->,mark->3,marksize->3,show->sh,append->,color->Blue)


fi=draw(func->"Func0",width->2,x->Dtop,color->Blue,append->,width->2,show->0,xrange->(13,50))

! X1=15
! Y1=0.72
! X2=30
! Y2=0.81

fi=draw(func->"Weight0",width->2,x->Dtop,color->Red,append->,show->0,xrange->(13,50))

fi=draw(func->("Func0"*"Weight0"),
width->2,x->Dtop,color->Orange,append->,show->0,xrange->(13,50))
fi=drawline(X1,X2,Y1,Y2,pointsonly->,mark->3,marksize->3,show->sh,append->,color->Red,width->2)
!show(fi,continue->)
! b=0.23
! fi=draw(func->(b*log(x-x1+exp(y1/b))),width->2,x->x,color->Orange,append->,show->0,xrange->(15,50))
! b=0.15
!;return
! fi=draw(func->(b*log(x-x1+exp(y1/b))),width->2,x->x,color->Red,append->,show->0,xrange->(15,50))
 fi=drawline(35,0.795,label->'f(d_{top})"font"',append->,show->0,width->2)
 fi=drawline(35,1.14,label->'u(D_{top})"font"',append->,show->0,width->2)
  fi=drawline(35,0.98,label->'f(d_{top})*u(d_{top})"font"',append->,show->0,width->2)
show(fi,continue->,xlabel->'d_{top}, cm"font"',ylabel->' ',yrange->(0.6,1.2))
;return


fi=draw(func->"Func0",width->2,x->Dtop,color->Blue,append->,continue->,xrange->(15,50),
xlabel->'D_{top}, cm"font"',ylabel->'f(d_{top})"font"')
!fii=plotyx(suh,D,xrange->(10,50),continue->)
fi2=plotyx(arvop,D,mark->3,continue->,color->Red,until->8)

fi=plotyx(f00,D,mark->3,yrange->(0.55,0.85),show->0,color->Blue)
fi=draw(func->"Func0",width->2,x->Dtop,color->Blue,append->,continue->,xrange->(15,50),
xlabel->'d_{top}, cm"font"',ylabel->'f(d_{top})"font"')
;return




L1=48.5
V1=178
s1=102.4
suh1=

setmat=matrix(6,2,values->


;return

;sawmat:
**turning maximum sawing into primatrix
Lmin=40
Lmax=55
Dmin=15
dmin=5


les=matrix((55-40)/3+1)
lenles=len(les);
des=matrix(47-15+1)
lendes=len(des);
pmat=matrix(lendes,lenles)
pmat2=matrix(lendes,lenles)
pmatmin=matrix(lendes,lenles)
pmatmax=matrix(lendes,lenles)
pmatmin=100000
pmatmax=-1000000
nmat=matrix(lendes,lenles)
nmat=0.0001
nn=nmat(1,1);

transawmat=trans() !l
saw=0
chip=0
vol=0
obj=0
totc=0
saw2=0
ntot=0
nob=0
vtot=0

do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->U%saw)
!u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
!func->"Func",obj->(Psaw*u%saw+Pchip*u%totc-plog*u%logv-ppulp*u%pulp))

do(ilog,1,U%nlog)

ntot=ntot+1

leni=int((U%lens(ilog)-40)/3+1)
dto=int(U%dtops(ilog)-14)
uti=U%saws(ilog)
pmat(dto,leni,sum->)=uti
pmatmin(dto,leni)=min(pmatmin(dto,leni),uti)
pmatmax(dto,leni)=max(pmatmax(dto,leni),uti)
pmat2(dto,leni,sum->)=uti*uti
nmat(dto,leni,sum->)=1
! if(dto.eq.1.and.leni.eq.1)then
! iobs,U%nlog,nmat(dto,leni);
! U%lens;
! U%saw;
! U%saws;
! U%logvs;
! U%dtops;
! pause('<>')

! endif

enddo

saw=saw+U%saw
chip=chip+U%chip
vol=vol+u%logv
obj=obj+U%obj
totc=totc+U%totc
nob=nob+1
vtot=vtot+U%com

endif
enddo
saw=100*saw/vtot;
vol=100*vol/vtot;
obj=100*obj/vtot;
totc=100*totc/vtot;
chip=100*chip/vtot;
pmatmean=pmat/.nmat;
pmatvar=pmat2/.nmat-pmatmean*.pmatmean
pmatsd=sqrt(pmatvar);
pmatmin;
pmatmax;
nsum=sum(nmat,any->);
nmatp=100*nmat/nsum;
maxp=max(pmatmean,any->);
pmatmeanp=100*pmatmean/maxp;
pmatsdp=pmatsd/maxp;
saw,vol,obj,totc,chip;
/

call(transawmat)

**drawing

les=matrix(do->(40,55,3));
lenles=len(les)
des=matrix(47-15+1,do->)+14
lendes=len(des);
pmax=max(pmatmean,any->);
coe=100/pmax;

fi=0
;do(il,1,lenles)
fi=drawline(des,coe*pmatmean(All,il),append->,reject->$y.eq.0,show->0,mark->1,color->Blue) 
;enddo
fi=drawline(xlabel->'D_{top}, cm"font"',ylabel->'Value for different L_{log}"font"',xrange->(15,45),append->,continue->)
!show(fi,continue->)
fib=0
;do(il,1,lendes)
fib=drawline(les,coe*pmatmean(il,All),append->,reject->$y.eq.0,show->0,color->Red,mark->1) 
;enddo
fib=drawline(xlabel->'L_{log}, dm"font"',ylabel->'Value for different D_{top}"font"',xrange->(40,55),append->,continue->)
!show(fib,conti nue->)

;current:
pmatmean2=fill(pmatmean,row->);
i=0
;do(il,1,lenles)
fi=drawline(des,coe*pmatmean2(All,il),append->,reject->$y.eq.0,show->0,mark->1,color->Blue) 
;enddo
fi=drawline(xlabel->'D_{top}, cm"font"',ylabel->'Value for different L_{log}"font"',xrange->(15,45),append->,continue->)
!show(fi,continue->)
fib=0
;do(il,1,lendes)
fib=drawline(les,coe*pmatmean2(il,All),append->,reject->$y.eq.0,show->0,color->Red,mark->1) 
;enddo
fib=drawline(xlabel->'L_{log}, dm"font"',ylabel->'Value for different D_{top}"font"',xrange->(40,55),append->,continue->)
!show(fib,conti nue->)


;return


transawmat2=trans() !l
saw2=0
chip2=0
vol2=0
obj2=0
totc2=0

vtot2=0

do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->sum(pmatmean(U%dtops-14,(U%lens-40)/3+1),any->))
!u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
!func->"Func",obj->(Psaw*u%saw+Pchip*u%totc-plog*u%logv-ppulp*u%pulp))

saw2=saw2+U%saw
chip2=chip2+U%chip
vol2=vol2+u%logv
obj2=obj2+U%obj
totc2=totc2+U%totc

vtot2=vtot2+U%com

endif
enddo
saw2=100*saw2/vtot2;
vol2=100*vol2/vtot2;
obj2=100*obj2/vtot2;
totc2=100*totc2/vtot2;
chip2=100*chip2/vtot2;
vtot2;
saw,vtot,obj,totc,chip;

/
call(transawmat2)



;return


tranm=trans() !like trana in ana7
sawm=0
chipm=0
volm=0
objm=0
totcm=0
saw2m=0
nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(Psaw*u%saw+Pchip*u%totc-plog*u%logv-ppulp*u%pulp))
! u%saw+u%chip,u%logv;
! u%logv+u%pulp,u%com;
! u%pulp+u%chip,u%totc;
! Psaw*u%saw+Pchip*u%totc-plog*u%logv-ppulp*u%pulp,u%obj;

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->sum(pmatmean(U%dtops-14,(U%lens-40)/3+1),any->))

!V=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
!func->"Func",obj->(Psawdif*V%saw-pvdif*V%logv))

! V%saw+u%chip,V%logv;
! V%logv+V%pulp,V%com;
! V%pulp+V%chip,V%totc;
! Psawdif*V%saw+Pvdif*V%logv,V%obj;

!func->"Func",obj->(Psaw*U%saw+Pchip*U%totc-plog*U%logv-ppulp*U%pulp))
if(iobs.eq.1)then
u%saw,U%saw,u%nlog,U%nlog,u%logv,U%logv;
u%totc,U%totc;
u%dtops;
U%dtops;
u%lens;
U%lens;
u%obj;
! V%saw,V%nlog,V%logv;
! V%dtops;
! V%lens;
! V%obj;

pm=pmatmean(U%dtops-14,(U%lens-40)/3+1);
pms=sum(pm,any->);
utis=0
do(ilog,1,U%nlog)
ilog;
uti=Psawdif*U%saws(ilog)-pvdif*U%logvs(ilog);
leni=(U%lens(ilog)-40)/3+1;
dto=U%dtops(ilog)-14;
utis=utis+uti;
enddo
utis2=utis+pcom*U%com;
u%lens;
u%dtops;
pause('<>')


endif

sawm=sawm+U%saw
chipm=chipm+U%chip
volm=volm+u%logv
objm=objm+U%obj
totcm=totcm+U%totc
nob=nob+1
!pause('<')
! if(any)then
! u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, any->,
! func->"Func",obj->(psaw*u%saw+pchip*u%totc-plog*u%logv-ppulp*u%pulp))
! !pause('<>')
! !print(u%vars,expand->)
! endif
endif
enddo
sawm=100*sawm/vtot;
volm=100*volm/vtot;
objm=100*objm/vtot;
totcm=100*totcm/vtot;
chipm=100*chipm/vtot;
/
call(tranm)











;return !sawmat


fib1
fi0=drawline(xlabel->'D_{top}, cm"font"',ylabel->'V_{*}, dm^3"font"',
yrange->(0,600),append->,show->0)

fi0=drawline(labelminb1,labelmaxb1,580,580,label->' V_{log}"font"',append->,show->0,color->Cyan,
width->2)


fi0=drawline(labelminb1,labelmaxb1,70,70,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)
fi0=drawline(labelminb1,labelmaxb1,30,30,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2)
fi0=drawline(labelminb1,labelmaxb1,190,190,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)
fi0=drawline(labelminb1,labelmaxb1,250,250,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)

show(fi0,continue->)
;logjarc:


fi=draw(func->"Func",x->D,xrange->(10,50),continue->,
xlabel->'D, cm"font"',ylabel->'f(D)"font"',width->2,color->Blue,show->0)
fi=drawline(d1,d2,f1v,f2v,mark->1,marksize->4,append->,pointsonly->,continue->)
fig6=fi6

fig7=fi7


!fig8=fi8


;return

;fig3uu:

traf2=trans()
h100=pinedee(All,100)
dif=abs(h100-16)
mi=minloc(dif);
getobs(pinenew,mi)
lento;
dee=pinedee(mi,1,-lento)
aa=pineaa(mi,1,-lento)
vee=pinevee(mi,1,-lento)
aaf=matrix(1,lento)
do(j,10,lento)
aaf(j)=(1-bf*gam**dee(j))*aa(j)
enddo
syl=matrix(lento)
alkusyl=matrix(lento)
loppusyl=matrix(lento)
alkusaw=matrix(lento)
loppusaw=matrix(lento)
saw=matrix(lento)
alkusyllen=matrix(lento)
loppusyllen=matrix(lento)
alkusawlen=matrix(lento)
loppusawlen=matrix(lento)
ho=matrix(lento,do->)
maxsyltot=0
maxsawtot=0
do(i,68,lento,3)
maxsyl=0
maxsaw=0
do(j,34,i-34,3)
if(j.gt.58)goto(cycle)
alkusyl0=j*aa(j)
loppusyl0=(i-j)*aa(i)
totsyl=alkusyl0+loppusyl0
if(totsyl.gt.maxsyl.and.i-j.ge.34.and.i-j.le.58)then
syl(i)=totsyl
alkusyllen(i)=j
loppusyllen(i)=i-j

alkusyl(i)=alkusyl0
loppusyl(i)=loppusyl0
maxsyl=totsyl
if(maxsyl.gt.maxsyltot)then
maxsyltot=maxsyl
alkusyllentot=alkusyllen(i)
loppusyllentot=loppusyllen(i)
syllentot=i
endif
endif

alkusaw0=j*aaf(j)
loppusaw0=(i-j)*aaf(i)
totsaw=alkusaw0+loppusaw0
if(totsaw.gt.maxsaw.and.i-j.ge.34.and.i-j.le.58)then
saw(i)=totsaw
alkusawlen(i)=j

loppusawlen(i)=i-j
alkusaw(i)=alkusaw0
loppusaw(i)=loppusaw0
maxsaw=totsaw
if(maxsaw.gt.maxsawtot)then
maxsawtot=maxsyl
alkusawlentot=alkusawlen(i)
loppusawlentot=loppusawlen(i)
syllentot=i
endif
endif


cycle: enddo
enddo
hms=maxloc(saw);
csaw=(dee(hms)/saw(hms))*saw
calkusaw=(dee(hms)/saw(hms))*alkusaw
cloppusaw=(dee(hms)/saw(hms))*loppusaw
fi=drawline(ho,dee,show->0,continue->,width->2)
fi=drawline(hms,hms,0,dee(hms),append->,show->0,continue->)
fi=drawline(alkusawlen(hms),alkusawlen(hms),
0,dee(alkusawlen(hms)),append->,show->0,continue->)
fi=drawline(ho(68,-116),csaw(68,-116),show->0,step->3,color->Green,append->,width->2)
fi=drawline(ho(68,-116),calkusaw(68,-116),append->,color->Blue,step->3,show->0,width->2)
fi=drawline(ho(68,-116),cloppusaw(68,-116),append->,color->Red,step->3,continue->,width->2)
alkusl=alkusawlen(hms);

log1d=dee(alkusl);
vee1=vee(alkusl);
saw1=alkusaw(hms);
log2d=dee(hms);
log2vee=vee(hms)-vee1;

fi=drawline(40,35,label->'Log 1: L=40 dm, D=25.5 cm, V_{log}=269 dm^3  ' ,  
append->,continue->)  
fi=drawline(40,31,label->'1. log L=  dm, D= cm   ' ,  
append->,continue->)            


! fi2=drawline(ho(68,-116),syl(68,-116),step->3,show->0,color->Green)
! fi2=drawline(ho(68,-116),alkusyl(68,-116),append->,color->Blue,step->3,show->0)
! fi2=drawline(ho(68,-116),loppusyl(68,-116),append->,continue->,step->3,color->Red)

! fi2=drawline(ho(68,-116),alkusyllen(68,-116),step->3,show->0,color->Blue)

! fi2=drawline(ho(68,-116),loppusyllen(68,-116),append->,continue->,color->Red,step->3)

fi2=drawline(ho(68,-116),alkusawlen(68,-116),step->3,show->0,color->Blue)

fi2=drawline(ho(68,-116),loppusawlen(68,-116),append->,continue->,color->Red,step->3)

/
call(traf2)

;return



traf=trans()
getobs(pinenew,2145)
lento;
aa=pineaa(2145,1,-lento)
dee=pinedee(2145,1,-lento)
vee=pinevee(2145,1,-lento)
aaf=matrix(1,lento)
saw=matrix(1,lento)
syl=matrix(1,lento)
ho=matrix(lento,do->)
do(i,1,lento)
aaf(i)=(1-bf*gam**dee(i))*aa(i)
syl(i)=aa(i)*i
saw(i)=aaf(i)*i
enddo
hms=maxloc(saw)
ms=saw(hms)
! c*saw(hms)=dee(hms)  =>c= dee(hms)/saw(hms)
sawhms=saw(hms)
csaw=(dee(hms)/saw(hms))*saw
cvol=(dee(hms)/vee(hms))*vee
csyl=(dee(hms)/syl(hms))*syl
fi=drawline(ho,dee,show->0,width->2)
fi=drawline(ho,csaw,append->,show->0,color->Red,width->2)
fi=drawline(ho,csyl,append->,show->0,color->Blue,width->2)
fi=drawline(ho,cvol,append->,continue->,color->Green,width->2,
xlabel->'L, dm"font"',ylabel->'D, cm"font"')


maxsaw=maxloc(saw);
maxcyl=maxloc(syl);

syl=matrix(lento)
alkusyl=matrix(lento)
loppusyl=matrix(lento)
alkusaw=matrix(lento)
loppusaw=matrix(lento)
saw=matrix(lento)
alkusyllen=matrix(lento)
loppusyllen=matrix(lento)
alkusawlen=matrix(lento)
loppusawlen=matrix(lento)
ho=matrix(lento,do->)
maxsyltot=0
maxsawtot=0
do(i,68,lento,3)
maxsyl=0
maxsaw=0
do(j,34,i-34,3)
if(j.gt.58)goto(cycle)
alkusyl0=j*aa(j)
loppusyl0=(i-j)*aa(i)
totsyl=alkusyl0+loppusyl0
if(totsyl.gt.maxsyl.and.i-j.ge.34.and.i-j.le.58)then
syl(i)=totsyl
alkusyllen(i)=j
loppusyllen(i)=i-j

alkusyl(i)=alkusyl0
loppusyl(i)=loppusyl0
maxsyl=totsyl
if(maxsyl.gt.maxsyltot)then
maxsyltot=maxsyl
alkusyllentot=alkusyllen(i)
loppusyllentot=loppusyllen(i)
syllentot=i
endif
endif

alkusaw0=j*aaf(j)
loppusaw0=(i-j)*aaf(i)
totsaw=alkusaw0+loppusaw0
if(totsaw.gt.maxsaw.and.i-j.ge.34.and.i-j.le.58)then
saw(i)=totsaw
alkusawlen(i)=j

loppusawlen(i)=i-j
alkusaw(i)=alkusaw0
loppusaw(i)=loppusaw0
maxsaw=totsaw
if(maxsaw.gt.maxsawtot)then
maxsawtot=maxsyl
alkusawlentot=alkusawlen(i)
loppusawlentot=loppusawlen(i)
syllentot=i
endif
endif


cycle: enddo
enddo
!csaw=sawhms=saw(hms)
csaw=(dee(hms)/sawhms)*saw
fi=drawline(ho(68,-116),csaw(68,-116),show->0,step->3,
append->,color->Violet,width->2,continue->)
maxc=maxloc(csaw);
dem=dee(maxc);
fi=drawline(maxc+2,maxc+11,csaw(maxc),csaw(maxc),color->Black,
label->'L=86 dm, D=11.8 cm"font"',append->,continue->,width->2,show->0)
demmi=dee(74);
fi=drawline(118,csaw(116),
label->'two logs"font"',append->,continue->,show->0)
fi=drawline(74,74,csaw(74),28,width->2,
label->' L=74 dm, D=13.2 cm"font"',append->,continue->,show->0)


/
call(traf)

!aaf;
!saw;
!syl;


;return
;ana:
Maxlines=23

trana=trans()

lmin=37
lmax=55
dmin=15
dmin=5
!getobs(pinenew,2145)
u=stemopt(lmin,lmax,dmin,dmin,2145,pinedee, 
func->"Func")
stat(data->u%data,until->u%Nobs)

u%Nobs;

do(iobs,3,3)
getobs(pinenew,iobs)
u=stemopt(lmin,lmax,dmin,dmin,pinedee,iobs, 
func->"Func")
!u%Nobs;
!iobs;
stat(data->u%data,until->u%Nobs)
!u%matrix(1,-u%Nobs,6,-12);

enddo

/
Maxlines=60
call(trana)
;return
li=;list(u%?);
@li;
stat(data->u%data,until->u%Nobs)
!fi=plotyx(u%saw,u%loglen,data->u%data,until->u%Nobs,continue->)
!stat(data->u%data,until->u%Nobs,filter->u%loglen.eq.52)

;return



;ana1:
;if(type(pinedee).ne.MATRIX)rpine
Maxlines=23
lmin=37
lmax=55
dmin=15
dmin=5
lmins=matrix(3,values->(37,40,43))
lmaxs=matrix(3,values->(52,55,58))
dmins=matrix(3,values->(14,15,16))

resu=matrix(9,8)

reslist=list(u%logv,u%syl,u%saw,u%inc,u%outc,
u%dtop,u%len,u%nlog)
resv=matrix(8)
veesum=0

trana=trans()

nobr=matrix(9)
nobr2=matrix(9)  !number of logs
nob=0

do(iobs,1,nrows(pinedee))
if((100*int(iobs/100)).eq.iobs)iobs;
if(pinedee(iobs,lmins(1)).lt.dmins(1))goto(nex)
top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume
nob=nob+1
getobs(pinenew,iobs)  !gives lento
Lmin=lmins(1)
Lmax=lmaxs(1)
Dmin=dmins(1)
u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs, obj->u%sawu,
func->"Func",weight->"Weight")
!u%nlog;
if(u%nlog.le.0)goto(nex)

nobr(2)=nobr(2)+1
nobr2(2)=nobr2(2)+u%nlog
resv=@reslist
resu(2,All)=resu(2,All)+resv

!mean
Lmin=lmins(2)
Lmax=lmaxs(2)
Dmin=dmins(2)
u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs, 
func->"Func",weight->"Weight",obj->u%sawu)
if(u%nlog.le.0)goto(start)
resv=@reslist
!@u%vars;
!pause('hep>')
resu(1,All)=resu(1,All)+resv
nobr(1)=nobr(1)+1
nobr2(1)=nobr2(1)+u%nlog


!endif
start:irow=2
do(le1,1,3,2)
Lmin=lmins(le1)
do(le2,1,3,2)
Lmax=lmaxs(le2)
do(dm,1,3,2)
Dmin=dmins(dm)
if((le1+le2+dm).ne.3)then
irow=irow+1
u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs, 
func->"Func",weight->"Weight",obj->u%sawu)
if(u%nlog.gt.0)then
resv=@reslist

resu(irow,All)=resu(irow,All)+resv
nobr(irow)=nobr(irow)+1
nobr2(irow)=nobr2(irow)+u%nlog
!write('resutot.bin','B',@totlist)
!if(u%data(u%dtop,row->u%saw%maxobs).lt.dmins(dm))pause('here>')


endif
endif



enddo
enddo
enddo

nex:enddo
!nob,nob2;
!resu;
logsum=resu(All,1)

resu(All,1)=100*resu(All,1)/veesum  !logvolume / commercial volume
resu(All,2,-5)=100*resu(All,2,-5)/veesum  !cylinder,saw,in,out
resu(All,6)=resu(All,6)/.nobr  ! dtop
resu(All,7)=resu(All,7)/.nobr2  ! len
resu(All,8)=resu(All,8)/.nobr
!close('resutot.bin')
/
Maxlines=60
call(trana)

resu;

;return
;ana1b:
head=matrix(9,3)
head(1,1)=lmins(2)
head(1,2)=lmaxs(2)
head(1,3)=dmins(2)
irow=1
;do(le1,1,3,2)

;do(le2,1,3,2)
;do(dm,1,3,2)
irow=irow+1
head(irow,1)=lmins(le1)
head(irow,2)=lmaxs(le2)
head(irow,3)=dmins(dm)




;enddo
;enddo
;enddo
head;
total=matrix(1,2,matrix->,values->(head,resu(All,1,-8)));
;if(exist_f('ana1.txt'))delete_f('ana1.txt')
write('ana1.txt','(f3.0,2f4.0,8f5.1)',total)
close('ana1.txt')
print_f('ana1.txt')
;if(exist_f('ana1b.txt'))delete_f('ana1b.txt')
write('ana1b.txt','(3f4.0,8f8.4)',total)
close('ana1b.txt')
print_f('ana1b.txt')





scal=trans()
Lmin2=(Lmin-lmins(2))/3
Lmax2=(Lmax-lmaxs(2))/3
Dmin2=Dmin-dmins(2)
/
ana1d=data(read->(Lmin,Lmax,Dmin,logv,vsyl,saw,inc,outc,pulp,
dtop,loglen),in->'ana1b.txt',maketrans->scal)
stat()
regr(saw,Lmin2,Lmax2,Dmin2)

total2$=total(2,-9,3,-8)/.total(1,3,-8)
total2=matrix(1,2,matrix->,values->(head(2,-9,All),total2$));



;if(exist_f('ana1c.txt'))delete_f('ana1c.txt')
write('ana1c.txt','(f3.0,2f4.0,8f5.2)',total2)
close('ana1c.txt')
print_f('ana1c.txt')


;return
li=;list(u%?);
@li;
stat(data->u%data,until->u%Nobs)
!fi=plotyx(u%saw,u%loglen,data->u%data,until->u%Nobs,continue->)
!stat(data->u%data,until->u%Nobs,filter->u%loglen.eq.52)

;return
;ana1c:

dat=data(read->@totlist,in->'resutot.bin',form->'B')
stat()
re=regr(u%saw,Lmin,Lmax,Dmin)
vc=varcom(Resid,class->plot)
;return

;ana2:


Maxlines=23
lmin=37
lmax=55
dmin=15
dmin=5
lmins=matrix(3,values->(37,40,43))
lmaxs=matrix(3,values->(52,55,58))
dmins=matrix(3,values->(14,15,16))

resu=matrix(9,8)


!reslist=list(u%logv,u%syl,u%saw,u%inc,u%outc,
!u%dtop,u%len,u%nlog)
!resv=matrix(8)
veesum=0
lam=matrix(do->(2.5,6,0.5));
lyh=matrix(9,8)
lyhs=matrix(9,8)
lyhv=matrix(9,8)
sawtot=matrix(9)
logtot=matrix(9)

trana=trans()

nobr=matrix(9)
nobr2=matrix(9)  !number of logs
nob=0

do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,lmins(1)).lt.dmins(1))goto(nex)
top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume
nob=nob+1
getobs(pinenew,iobs)  !gives lento
Lmin=lmins(1)
Lmax=lmaxs(1)
Dmin=dmins(1)
u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs, 
func->"Func")
u%nlog;
if(u%nlog.le.0.or.u%len.eq.u%nlog*Lmin)goto(nex)
if(u%nlog.ne.u%nlogmax)pause('ajajj>')
nobr(2)=nobr(2)+1  !smallest row 2
nobr2(2)=nobr2(2)+u%nlog
!resv=@reslist
resu(2,All)=resu(2,All)+resv
sawtot(2)=sawtot(2)+u%saw
logtot(2)=logtot(2)+u%logv
do(ii,1,8)
U=stemopt(2,lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs, 
func->"Func",param->lam(ii),print->0)
if(U%len.lt.u%len)then
u%saw,U%saw;
@U%vars;
!pause('hep2>')
lyh(2,ii)=lyh(2,ii)+1
lyhs(2,ii)=lyhs(2,ii)+u%saw-U%saw
lyhv(2,ii)=lyhv(2,ii)+u%logv-u%logv
endif

enddo

!mean
Lmin=lmins(2)
Lmax=lmaxs(2)
Dmin=dmins(2)
u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs, 
func->"Func")
if(u%nlog.le.0.or.u%len.eq.u%nlog*Lmin)goto(start)
!resv=@reslist
!@u%vars;
!pause('hep>')
!resu(1,All)=resu(1,All)+resv
nobr(1)=nobr(1)+1  !medium row 1
nobr2(1)=nobr2(1)+u%nlog
sawtot(1)=sawtot(1)+u%saw
logtot(1)=logtot(1)+u%logv
do(ii,1,8)
U=stemopt(2,lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs, 
func->"Func",param->lam(ii))
if(U%len.lt.u%len)then
u%saw,U%saw;
!pause('hep2b>')
lyh(1,ii)=lyh(1,ii)+1
lyhs(1,ii)=lyhs(1,ii)+u%saw-U%saw
lyhv(1,ii)=lyhv(1,ii)+u%logv-u%logv
endif

enddo



!endif
start:irow=2
do(le1,1,3,2)
Lmin=lmins(le1)
do(le2,1,3,2)
Lmax=lmaxs(le2)
do(dm,1,3,2)
Dmin=dmins(dm)
if((le1+le2+dm).ne.3)then
irow=irow+1
u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs, 
func->"Func")
if(u%nlog.gt.0.and.u%nlog*Lmin.lt.u%len)then
!resv=@reslist
sawtot(irow)=sawtot(irow)+u%saw
logtot(irow)=logtot(irow)+u%logv
!resu(irow,All)=resu(irow,All)+resv
nobr(irow)=nobr(irow)+1
nobr2(irow)=nobr2(irow)+u%nlog
!write('resutot.bin','B',@totlist)
!if(u%data(u%dtop,row->u%saw%maxobs).lt.dmins(dm))pause('here>')
do(ii,1,8)
U=stemopt(2,lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs, 
func->"Func",param->lam(ii))
if(U%len.lt.u%len)then
lyh(irow,ii)=lyh(irow,ii)+1
lyhs(irow,ii)=lyhs(irow,ii)+u%saw-U%saw
lyhv(irow,ii)=lyhv(irow,ii)+u%logv-u%logv
endif

enddo


endif
endif



enddo
enddo
enddo

nex:enddo
nob,nob2;
!resu;
!logsum=resu(All,1)

! resu(All,1)=100*resu(All,1)/veesum  !logvolume / commercial volume
! resu(All,2,-5)=100*resu(All,2,-5)/veesum  !cylinder,saw,in,out
! resu(All,6)=resu(All,6)/.nobr  ! dtop
! resu(All,7)=resu(All,7)/.nobr2  ! len
! resu(All,8)=resu(All,8)/.nobr
lyh;
nobr;
lyh=100*lyh/.nobr;
lyhs;

lyhs2=100*lyhs/.sawtot;
lyhv2=100*lyhv/.logtot;
!close('resutot.bin')
/
Maxlines=60
call(trana)

!resu;

;return
;ana2b:
head=matrix(9,3)
head(1,1)=lmins(2)
	head(1,2)=lmaxs(2)
	 head(1,3)=dmins(2)
irow=1
;do(le1,1,3,2)

;do(le2,1,3,2)
;do(dm,1,3,2)
irow=irow+1
	
	 head(irow,1)=lmins(le1)
	head(irow,2)=lmaxs(le2)
	head(irow,3)=dmins(dm)
;enddo
;enddo
;enddo

head;
totly=matrix(1,2,matrix->,values->(head,lyh(All,1,-8)));
write('ana2a.txt','(f3.0,2f4.0,8f5.1)',totly)
close('ana2a.txt')
print_f('ana2a.txt')
totlys=matrix(1,2,matrix->,values->(head,lyhs2(All,1,-8)));
write('ana2b.txt','(f3.0,2f4.0,8f5.1)',totlys)
close('ana2b.txt')
print_f('ana2b.txt')
totlyv=matrix(1,2,matrix->,values->(head,lyhv2(All,1,-8)));
write('ana2c.txt','(f3.0,2f4.0,8f5.1)',totlyv)
close('ana2c.txt')
print_f('ana2c.txt')
;return

;ana3:
Maxlines=23
lmin=37
lmax=55
dmin=15
dmin=5
lmins=matrix(3,values->(37,40,43))
lmaxs=matrix(3,values->(52,55,58))
dmins=matrix(3,values->(14,15,16))

saw=matrix(9,5)
vol=matrix(9,5)
;do(i,1,5)
;if(i.ne.2);then
!u"i"=stemopt(0,37,52,14,5,1,pinedee, 
!func->"Func")
;else

;endif

li=;list(u"i"%?);
;enddo

trana=trans()

nobr=matrix(9)

!nobr2=matrix(9)  !number of logs
nob=0

do(iobs,1,nrows(pinedee))
if(iobs.eq.6)ipr=1
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,lmins(1)).lt.dmins(1))goto(nex)
top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume
nob=nob+1

;do(iro,1,9)


Lmin=lmins(lmi("iro"))
Lmax=lmaxs(lma("iro"))
Dmin=dmins(dmi("iro"))
;do(i,1,5)
;if(i.ne.2);then
ipr=0
if("i".eq.4)ipr=1
u"i"=stemopt("i",Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",print->ipr)
;else
u"i"=stemopt("i",Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",param->4)! ,print->)


;endif
iobs,"i",u"i"%nlog,u"i"%saw,u"i"%log;
!pause('vv>')
if(u"i"%nlog.le.0)goto(nex)
saw("iro","i")=saw("iro","i")+u"i"%saw
vol("iro","i")=vol("iro","i")+u"i"%log
nex:
;enddo
!@u4%vars;
!@u5%vars;
!pause('hep>')
;enddo

u%nlog;
enddo

/
Maxlines=60
call(trana)
** 1 max saw
** 2 max util
**3 max sawlog volume
** 4 minimize saw log volume
** maximize saw/saw log volume
saw;
vol;
suhd=saw/.vol;

;return




;return

;ana6:


Maxlines=23

Lmin=40
Lmax=55
Dmin=15
dmin=5

udif=matrix(8,values->(3,4,5,6,7,8,9,10))
pdif=matrix(6,values->(1.5,2,2.5,3,3.5,4))

resus=matrix(8,6)
resuv=matrix(8,6)
less=matrix(8,6)
less2=less
u=stemopt(0,Lmin,Lmax,Dmin,dmin,1,pinedee, 
func->"Func")
u%vars;

!reslist=list(u%logv,u%syl,u%saw,u%inc,u%outc,
!u%dtop,u%len,u%nlog)
!resv=matrix(8)
! veesum=0
! lam=matrix(do->(2.5,6,0.5));
! lyh=matrix(9,8)
! lyhs=matrix(9,8)
! lyhv=matrix(9,8)
! sawtot=matrix(9)
! logtot=matrix(9)

trana=trans()


less=0
nob=0
saw=0
vol=0
veesum=0
iup=0
ipp=0

do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
!dmin;
top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume

getobs(pinenew,iobs)  !gives lento
! Lmin=lmins(1)
! Lmax=lmaxs(1)
! Dmin=dmins(1)
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func")

u%nlog
if(u%nlog.gt.0)then
nob=nob+1
!@u%vars;
saw=saw+u%saw
vol=vol+u%logv
do(iu,1,8)
do(ip,1,6)

u=stemopt(6,Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",param->(udif(iu),pdif(ip)))
if(j_err)return
resus(iu,ip)=resus(iu,ip)+u%saw
resuv(iu,ip)=resuv(iu,ip)+u%logv
iip=u%nlog.lt.u%nlogmax
!ess(iu,ip)=less(iu,ip)+u%nlog.lt.u%nlogmax
less(iu,ip)=less(iu,ip)+iip

!u%nlog,u%nlogmax,u%nlog.lt.u%nlogmax;

if(u%lenpot-u%len.ge.Lmin)then
@u%vars;

!pause('per>')
endif
enddo
enddo
endif !nlog
endif  !min
enddo

/
Maxlines=60
call(trana)
nob;
saw=saw/nob;
vol=vol/nob;
resus=resus/nob;
resuv=resuv/nob;
udif;
pdif;
less;
less2;
less=100*less/nob;
;ana6b:
resus2=100*resus/saw;
resuv2=100*resuv/vol;

resus2b=matrixborder(resus2,column->udif,row->pdif);

write('ana6s.txt','(f3.0,8f6.1)',resus2b)
close('ana6s.txt')
print_f('ana6s.txt')
resuv2b=matrixborder(resuv2,column->udif,row->pdif)

write('ana6v.txt','(f3.0,8f6.1)',resuv2b)
close('ana6v.txt')
print_f('ana6v.txt')
;return

;ana7:

Maxlines=23

Lmin=40
Lmax=55
Dmin=15
dmin=5

!dif=matrix(11,values->(2.5,2.75,3,3.5,4,4.5,5,6,7,8,9))
udif=matrix(do->(2.5,7,0.25))
plogs=matrix(do->(2.5,4,0.1))
nplogs=len(plogs)
!pdif=matrix(5,values->(1.2,1.9,2,2.1,2.8))
pdif=plogs-1
!;return
! 3*75 - 2*25 =25/25  =1
i1=1
i2=6
i3=12
! 
n3=6
n11=len(udif)
resus=matrix(n11,nplogs)
resuv=matrix(n11,nplogs)
less=matrix(n11,nplogs)
!less2=less
!u=stemopt(0,Lmin,Lmax,Dmin,dmin,1,pinedee, 
!func->"Func")
!U=stemopt(0,Lmin,Lmax,Dmin,dmin,1,pinedee, 
!func->"Func")
u%vars;



trana=trans()


less=0
nob=0
saw=0
vol=0
veesum=0
iup=0
ipp=0


do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
!dmin;
top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume

getobs(pinenew,iobs)  !gives lento
! Lmin=lmins(1)
! Lmax=lmaxs(1)
! Dmin=dmins(1)
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->U%saw)

!U%nlog;
if(U%nlog.gt.0)then
nob=nob+1
!@u%vars;
saw=saw+U%saw
vol=vol+u%logv
do(iu,1,n11)
do(ip,1,nplogs)

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(udif(iu)*u%saw+pdif(ip)*u%pulp))
!endif

resus(iu,ip)=resus(iu,ip)+u%saw
resuv(iu,ip)=resuv(iu,ip)+u%logv
iip=u%nlog.lt.u%nlogmax
!ess(iu,ip)=less(iu,ip)+u%nlog.lt.u%nlogmax
less(iu,ip)=less(iu,ip)+iip



if(u%lenpot-u%len.ge.Lmin)then
@u%vars;

!pause('per>')
endif
enddo
enddo
endif !nlog
endif  !min
enddo

/
Maxlines=60
call(trana)
nob;
saw=saw/nob;
vol=vol/nob;
resus=resus/nob;
resuv=resuv/nob;
veesum=veesum/nob;

udif;
pdif;
less;

resusf=100*resus/veesum;
resuvf=100*resuv/veesum;

less=100*less/nob;





!;ana7b:

ss=resus(All,n3)
vv=resuv(All,n3)
waste=udif*.(saw-ss)
upulp=2
usaw=upulp+udif
uti=usaw*saw+upulp*(veesum-saw)
wastep=100.*waste/.uti
foloss=100*pdif(n3)*(vol-vv)
potprice=veesum-vv+plogs(n3)*vv
folossp=foloss/.potprice

fi=drawline(udif,wastep,color->Blue,show->0,ylabel->'Loss, %"font"',width->2,
xlabel->'u_{saw}-u_{pulp}"font"')

fi=drawline(udif,folossp,color->Red,show->0,width->2,continue->,append->)

upulp=3
usaw=upulp+udif
uti=usaw*saw+upulp*(veesum-saw)
wastep=100.*waste/.uti

fi=drawline(udif,wastep,color->Green,show->0,width->2,continue->,append->)

fi=drawline(4.2,4.7,7,7,color->Red,append->,show->0,label->' Forest owner"font"',width->2)

fi=drawline(4.2,4.7,2.2,2.2,color->Blue,append->,show->0,label->' Black hole, U_{pulp}=2"font"',width->2,continue->)
fi=drawline(4.2,4.7,1.7,1.7,color->Green,append->,show->1,label->' Black hole, U_{pulp}=3"font"',width->2,continue->)


!;return




ss=resusf(All,i1)
vv=resuvf(All,i1)
fi=drawline(udif,ss,color->2,show->0,ylabel->'V_{log} +  V_{saw}, %"font"',width->2,
xlabel->'u_{saw}-u_{pulp}"font"')
fi=drawline(udif,vv,color->2,show->0,width->2,
append->)

!;do(p,2,n3)
ss=resusf(All,i2)
vv=resuvf(All,i2)
!show(fi)
fi=drawline(udif,ss,color->3,append->,show->0,width->2)
!show(fi)
fi=drawline(udif,vv,color->3,append->,width->2,show->0)

ss=resusf(All,i3)
vv=resuvf(All,i3)
!show(fi)
fi=drawline(udif,ss,color->4,append->,show->0,width->2)
!show(fi)
fi=drawline(udif,vv,color->4,append->,width->2,show->0)

!;enddo
vp=100*vol/veesum;
fi=drawline(udif(1),udif(n11),vp,vp,append->,show->0,width->2)
vs=100*saw/veesum;
fi=drawline(udif(1),udif(n11),vs,vs,append->,show->0,width->2)
!pdif=matrix(3,values->(1.2,2,2.8))
!pdif=matrix(3,values->(1.2,2,2.8)) pienempi korkeampi
fi=drawline(4,4.7,65,65,color->2,append->,show->0,label->' p_{log}-p_{pulp}=1.2"font"',width->2)
!fi=drawline(4,4.7,81,81,color->2,append->,show->0,label->' p_{log}-p_{pulp}=1.5"font"',width->2,append->)

fi=drawline(4,4.7,61,61,color->3,append->,show->0,label->' p_{log}-p_{pulp}=2.0"font"',width->2)
!fi=drawline(4,4.7,80,80,color->3,append->,show->0,label->' p_{log}-p_{pulp}=2.0"font"',width->2)
fi=drawline(4,4.7,57,57,color->4,append->,show->0,label->' p_{log}-p_{pulp}=2.5"font"',width->2)

show(fi,continue->)
;ana7b:

ss=resusf(3,All)
vv=resuvf(3,All)
fi2=drawline(pdif,ss,show->0,color->Blue,width->2)
fi2=drawline(pdif,vv,append->,show->0,color->Blue,width->2)

ss=resusf(7,All)
vv=resuvf(7,All)
fi2=drawline(pdif,ss,append->,color->Green,show->0,width->2)
fi2=drawline(pdif,vv,append->,color->Green,show->0,width->2)


ss=resusf(11,All)
vv=resuvf(11,All)
fi2=drawline(pdif,ss,append->,show->0,color->Red,width->2)
fi2=drawline(pdif,vv,append->,color->Red,width->2,show->0)

fi2=drawline(1.8,72,append->,show->0,
label->' Above: 100*V_{log}/V_{com}"font"')

up=10
fi2=drawline(1.8,1.95,57+up,57+up,color->Red,append->,show->0,width->2,
label->' u_{saw}-u_{pulp}=5"font"',
ylabel->' %"font"',
xlabel->'p_{log}-p_{pulp}"font"')

fi2=drawline(1.8,1.95,54+up,54+up,color->Green,append->,show->0,width->2,
label->' u_{saw}-u_{pulp}=4"font"')

fi2=drawline(1.8,1.95,51+up,51+up,color->Blue,append->,show->0,continue->,width->2,
label->' u_{saw}-u_{pulp}=3"font"')

fi2=drawline(1.8,55,append->,show->0,
label->' Below: 100*V_{saw}/V_{com}"font"')
sawp=100*saw/veesum
volp=100*vol/veesum
fi2=drawline(pdif(1),pdif(nplogs),sawp,sawp,color->Black,append->,show->0,continue->)
fi2=drawline(pdif(1),pdif(nplogs),volp,volp,color->Black,append->,show->1,continue->)



!show(fi2,continue->)
;return


;ela:

Maxlines=23
!like ana7
Maxlines=23

Lmin=40
Lmax=55
Dmin=15
dmin=5

!dif=matrix(11,values->(2.5,2.75,3,3.5,4,4.5,5,6,7,8,9))
udif=matrix(do->(2.5,7,0.25))
plogs=matrix(do->(2.5,4,0.1))
nplogs=len(plogs)
!pdif=matrix(5,values->(1.2,1.9,2,2.1,2.8))
pdif=plogs-1
!;return
! 3*75 - 2*25 =25/25  =1
i1=1
i2=6
i3=12
! 
n3=6
n11=len(udif)
resus=matrix(n11,nplogs)
resuv=matrix(n11,nplogs)
less=matrix(n11,nplogs)
!less2=less
! u=stemopt(0,Lmin,Lmax,Dmin,dmin,1,pinedee, 
! func->"Func")
! U=stemopt(0,Lmin,Lmax,Dmin,dmin,1,pinedee, 
! func->"Func")
! u%vars;



tranb=trans() !like trana in ana7


less=0
nob=0
saw=0
vol=0
veesum=0
iup=0
ipp=0


do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
!dmin;
top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume

getobs(pinenew,iobs)  !gives lento
! Lmin=lmins(1)
! Lmax=lmaxs(1)
! Dmin=dmins(1)
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->U%saw)

!U%nlog;
if(U%nlog.gt.0)then
nob=nob+1
!@u%vars;
saw=saw+U%saw
vol=vol+u%logv
do(iu,1,n11)
do(ip,1,nplogs)

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(udif(iu)*u%saw+pdif(ip)*u%pulp))
!endif

resus(iu,ip)=resus(iu,ip)+u%saw
resuv(iu,ip)=resuv(iu,ip)+u%logv
iip=u%nlog.lt.u%nlogmax
!ess(iu,ip)=less(iu,ip)+u%nlog.lt.u%nlogmax
less(iu,ip)=less(iu,ip)+iip



enddo
enddo
endif !nlog
endif  !min
enddo

/
Maxlines=60
call(tranb)
nob;
saw=saw/nob;
vol=vol/nob;
resus=resus/nob;
resuv=resuv/nob;
veesum=veesum/nob;

udif;
pdif;
less;

resusf=100*resus/veesum;
resuvf=100*resuv/veesum;

less=100*less/nob;

elas=vector(0.25,0.5,0.75,1,1.25,1.5)

;elab:

ask(upulp)
ask(append)
plogs=pdif+1;   !matrix(do->(2.5,4,0.1))
plog0=plogs(6)
!;return
nelas=len(elas)
nplogs=len(plogs)
!ask(plog0)
uucom=matrix(nelas,nplogs)
uuveesum=matrix(nelas,nplogs)
utilm=matrix(n11,nelas)
sawrm=matrix(n11,nelas)
volrm=matrix(n11,nelas)
transel=trans()
do(ie,1,nelas)
elascur=elas(ie);
do(ip,1,nplogs)

uucom(ie,ip)=(elas(ie)*(plogs(ip)-plog0)/plog0+1)
uuveesum(ie,ip)=uucom(ie,ip)*veesum
enddo
enddo
do(iu,1,n11)

do(ie,1,nelas)
ie,elas(ie),iu,udif(iu);
resuv0=resuv(iu,All);
uuvol=uucom(ie,All)*.resuv0;
resus0=resus(iu,All);
uusaw=uucom(ie,All)*.resus0;
uuveesum0=uuveesum(ie,All);
sawr=100*uusaw/.uuveesum0;
volr=100*uuvol/.uuveesum0;


util=(upulp-t(plogs))*.uuveesum0+udif(iu)*uusaw+pdif(ip)*(uuveesum0-uuvol);
utilma=maxloc(util)
utilm(iu,ie)=plogs(utilma)
sawrm(iu,ie)=sawr(utilma)
volrm(iu,ie)=volr(utilma)
enddo 
enddo
/

call(transel)
!3,7,11 3=3, 7=4, 11=5
!Lower lines for higher upulp
!;current:
uut=sawrm(3,All)
fi2=drawline(elas,uut,show->0,color->Blue,append->append,width->2)
uut=volrm(3,All)
fi2=drawline(elas,uut,show->0,color->Blue,append->,width->2)


uut=sawrm(7,All)
fi2=drawline(elas,uut,append->,show->0,append->,color->Green,width->2)
uut=volrm(7,All)
fi2=drawline(elas,uut,append->,show->0,append->,color->Green,width->2)

;if(append);then
fi2=drawline(0.6,72,append->,show->0,
label->' Above: 100*V_{log}/V_{com}"font"')
fi2=drawline(0.6,52,append->,show->0,
label->' Below: 100*V_{saw}/V_{com}"font"')
pro=100*saw/veesum
fi2=drawline(elas(1),elas(nelas)-0.1,pro,pro,append->,show->0,append->,
color->black,width->1,label->' Max"font"')
pro=100*vol/veesum
fi2=drawline(elas(1),elas(nelas)-0.1,pro,pro,append->,show->0,append->,color->black,
width->1,label->' Max"font"')
fi2=drawline(0.6,0.8,69,69,color->Red,append->,show->0,continue->,width->2,
label->'  u_{dif}=5"font"')
fi2=drawline(0.6,0.8,66,66,color->Green,append->,show->0,continue->,width->2,
label->'  u_{dif}=4"font"')
fi2=drawline(0.6,0.8,63,63,color->Blue,append->,show->0,continue->,width->2,
label->' u_{dif}=3"font"')
fi2=drawline(0.6,59,append->,show->0,
label->' Upper curves: u_{pulp}=3"font"')
fi2=drawline(0.6,56,append->,show->0,
label->' Lower curves: u_{pulp}=4"font"')
;endif
uut=sawrm(11,All)

fi2=drawline(elas,uut,append->,color->Red,continue->,width->2,
ylabel->'%"font"',xlabel->'Elasticity"font"',show->0)
uut=volrm(11,All)
!;if(upulp.eq.4)fi2=drawline(elas,uut,append->,color->Red,continue->,width->4,show->0)
fi2=drawline(elas,uut,append->,color->Red,continue->,width->2)

;return

fi2=drawline(0.4,0.5,3.9,3.9,color->Red,label->' u_{dif}=5"font"',append->,show->0,width->2)
fi2=drawline(0.4,0.5,3.8,3.8,color->Green,label->' u_{dif}=4"font"',append->,show->0,width->2)
fi2=drawline(0.4,0.5,3.7,3.7,color->Blue,label->' u_{dif}=3"font"',append->,show->0,width->2)
fi2=drawline(0.4,3.5,label->'Upper: u_{pulp}=3"font"',append->,show->0)
fi2=drawline(0.4,3.4,label->'Lower: u_{pulp}=4"font"',append->,show->0)
fi2=drawline(elas,uut,append->,color->Red,continue->,width->2,
ylabel->'p_{log}"font"',xlabel->'Elasticity"font"')



!;current:
uut=utilm(3,All)
!Lower lines for higher upulp
fi=drawline(elas,uut,show->0,color->Blue,append->append,width->2)
uut=utilm(7,All)
fi=drawline(elas,uut,append->,show->0,append->,color->Green,width->2)
uut=utilm(11,All)
fi=drawline(0.4,0.5,3.9,3.9,color->Red,label->' u_{dif}=5"font"',append->,show->0,width->2)
fi=drawline(0.4,0.5,3.8,3.8,color->Green,label->' u_{dif}=4"font"',append->,show->0,width->2)
fi=drawline(0.4,0.5,3.7,3.7,color->Blue,label->' u_{dif}=3"font"',append->,show->0,width->2)
fi=drawline(0.4,3.5,label->'Upper: u_{pulp}=3"font"',append->,show->0)
fi=drawline(0.4,3.4,label->'Lower: u_{pulp}=4"font"',append->,show->0)
fi=drawline(elas,uut,append->,color->Red,continue->,width->2,
ylabel->'p_{log}"font"',xlabel->'Elasticity"font"')



;return
pros=matrix(nelas)

uuveesum=matrix(nelas,nplogs)


!kullekin elastisuudelle 
!askella plog
!laske Vsaw-käyrä udifi:n suhteen
!katso Vsa käyrältä udif joka tuottaa tavoitteen  plog*logv+   Vpulp ppulp*vpulp
!ullekin plog laske käyr' udifIn suhteen


elast=trans()
Pdif=plog0-1
call(tranb)
 !uu0=interpolate(Resus,Udif,target);
 v0=suv  !interpolate(Udif,Resuv,uu0);
 sus0=sus
 suv0=suv

pulp0=veesum-v0
chip0=veesum-s0!interpolate(Udif,Resus,uu0);
! ppulp=1plog0-Pdif  !polog-ppulp=pdif
util0=upulp*chip0+usaw*s0-plog0*v0-pulp0
veesum0=veesum

pros0=100*v0/veesum
!fi=drawline(Udif,Resuv,show->0)
!fi=drawline(Udif,Resus,append->,continue->)
!pause('c0>')

do(i,1,nelas)
elascur=elas(i);
do(j,1,nplogs)
!Pdif=2

Pdif=plogs(j)-1


uucom(i,j)=(elas(i)*(plogs(j)-plog0)/plog0+1)
uuveesum(i,j)=uucom(i,j)*veesum

ssn=sus*uucom(i,j)
ssn=min(ssn,target)
ssv=suv*uucom(i,j)
pulp=uuveesum(i,j)-ssv
chip=uuveesum(i,j)-ssn!interpolate(Udif,Resus,uu0);
 !ppulp=plog0-Pdif  !polog-ppulp=pdif
util(i,j)=upulp*chip+usaw*ssn-plogs(j)*ssv-pulp

!if(uu(i,j).lt.0)pause('hdhd>')

v(i,j)=ssv
 s(i,j)=ssn
!plog=Pdif+1
price(i,j)=plogs(j)*v(i,j)+uuveesum(i,j)-v(i,j)
!util(i,j)=upulp*(veesum-sus)-plogs(j)*v(i,j)-ppulp*(veesum-v(i,j))
! if(uu(i,j).lt.0)util(i,j)=0
! util2(i,j)=uucom(i,j)*util(i,j)
! v2(i,j)=uucom(i,j)*v(i,j)
! s2(i,j)=uucom(i,j)*s(i,j)
! !pause('cc>')
! price2(i,j)=uucom(i,j)*price(i,j)
!pause('<>')
enddo
maxlo=maxloc(util(i,All));
pros(i)=100*v(i,maxlo)/uuveesum(i,maxlo)

enddo
fi=drawline(elas,pros)
/
;elab:
!(upulp-plog)*vtot
!Pdif0=2
plog0=ask()
!target%=ask()   !48
!targetp=0.01*target%
Udif=ask()
upulp=ask()
usaw=upulp+Udif
!ulog=ask()
!udif0=ulog-upulp
call(elast)
uucom;
!v0;
v;
!v2;
s0;
uuveesum;
suh0=s0/veesum0;
suh=s/.uuveesum;

!s;
!s2;
!u!u0;
!uu;
!price0;
!price;
!price2;
s;

util0;
util;
!util2;
target,ulog;
;return
! upulp=4
! usaw=4 l=3.5 
!upulp=3*ppulp   =3
! usaw
!target=47.5
! call(elast)
! uucom;
! v0;
! v;
! uu0;
! uu;
! price0;
! price;

;return

;ana8:


Maxlines=23

Lmin=40
Lmax=55
Dmin=15
dmin=5
ask(ifast)

udif=matrix(do->(1,4,0.25));
!udif=udif-1

n9=len(udif)

pdif=matrix(3,values->(1.5,2,2.5))
npdif=len(pdif)
resus=matrix(npdif,n9)
resuv=matrix(npdif,n9)

Resus=matrix(npdif,n9)
Resuv=matrix(npdif,n9)


trana=trans()

less=0
nob=0
saw=0
vol=0
veesum=0
iup=0
ipp=0

do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
!dmin;
top=find(pinedee(iobs,All),filter->$.lt.dmin)
comv=pinevee(iobs,top-1)  !commercial volume

getobs(pinenew,iobs)  !gives lento

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->U%saw,fast->ifast)
!endif

!U%nlog;
if(U%nlog.gt.0)then
nob=nob+1
!@u%vars;
saw=saw+U%saw
vol=vol+u%logv
veesum=veesum+comv
do(ip,1,npdif)
do(iu,1,n9)

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(udif(iu)*u%saw-pdif(ip)*u%logv),fast->ifast)

if(j_err)return
resus(ip,iu)=resus(ip,iu)+u%saw
resuv(ip,iu)=resuv(ip,iu)+u%logv

if(u%lenpot-u%len.ge.Lmin)then
print(u%vars,expand->)

pause('per>')
endif
enddo
enddo
endif !nlog
endif  !min
enddo

/
Maxlines=60
call(trana)
nob;
saw=saw/nob;
vol=vol/nob;
resus=resus/nob;
veesum=veesum/nob;
resus=100*resus/veesum;
resuv=resuv/nob;
resuv=100*resuv/veesum;
saw=100*saw/veesum;
vol=100*vol/veesum;
!volsaw0=vol/.saw;
volsaw=resuv/saw
udif;
! pdif;
!ess;
!less2;
!less=100*less/nob;

;ana8b:
!sk(ifast)
pp=26
pv=72
ps=100



!;return
trans8=trans()
up=ico*pp;

psa=(pv-pp)*volsaw(2,All)+pp;
pause('hel>')
uti=(udif*pp-psa+pp)*saw+(up-pp)*100;

utiold=(up-pv)*100+pp*t(udif)*.resus(2,All)+(pv-pp)*(100-resuv(2,All));

!(udif*pp-psa+pp)*saw+(up-pp)*100=utiold
! (udif*pp+pp)*saw+(up-pp)*100=psa*saw

psa2=(udif*pp+pp)+(up-pp)*100/saw-utiold/saw;

!uti2=(udif*pp-psa2+pp)*saw+(up-pp)*100;

incold=pv*resuv(2,All)+pp*(100-resuv(2,All));
!incnew=psa*saw+pp*(100-saw);
incnew2=psa2*saw+pp*(100-saw) ;  !saem utility
!utiold;
!uti2;
!;return
! xlabel->'u_{dif}"font"',ylabel->'P_{saw} "font"') 


rat=100*(uti-utiold)/.utiold;

rati2=100*(incnew2-incold)/.incold;  !does not depend on upulp
if(iappend.eq.0)then
fip=drawline(udif,psa,color->Red,continue->,width->2,show->0,
 xlabel->'u_{dif}"font"',ylabel->'P_{saw} "font"')
 fip=drawline(3.1,3.4,101.1,101.1,width->2,color->Red,show->0,append->,label->' Same owner income"font"')
fip=drawline(udif,psa2,color->Blue,width->2,append->,show->0)
fip=drawline(3.1,3.4,101.9,101.9,width->2,color->Blue,append->,label->' Same company utility"font"',
continue->)


endif
 
fi=drawline(udif,rat,color->icolor,continue->,width->2,show->0,append->iappend)

!i=drawline(udif,rati2,color->icolor,width->2,show->0,
!append->)
if(iappend.eq.0)then
fi=drawline(udif,rati2,color->Orange,continue->,width->2,show->0,
append->,
 xlabel->'u_{dif}"font"',ylabel->'Gain, %"font"')
 endif
iappend=1
/

ico=2
!up=2*pp
icolor=Red
iappend=0
!ishow=0

call(trans8)
fi=drawline(3.4,3.7,15,15,color->icolor,append->,show->0,label->' u_{pulp}="up""font"',width->2)
fi=drawline(3.,17,append->,show->0,label->' Same owner income"font"',width->2)
ico=3
icolor=Green

call(trans8)
fi=drawline(3.4,3.7,14,14,color->icolor,append->,show->0,label->' u_{pulp}="up""font"',width->2)
ico=4
ishow=1
icolor=Blue
call(trans8)
fi=drawline(3.1,3.4,11.5,11.5,color->Orange,width->2,append->,show->0,label->' Same company utility"font"',width->2)
fi=drawline(3.4,3.7,13,13,color->icolor,append->,show->1,label->' u_{pulp}="up""font"',width->2,continue->)


;return


upulps=matrix(3,values->(2,3,4))
upulps=upulps+1

append=0
;do(ip,1,npdif)
usaw=upulps(ip)+udif
fi=drawline(udif,resus(ip,All),show->0,color->(1+ip),width->2,show->0,append->append)
plog=pdif(ip)+1
denom=(plog*vol+(100-vol))
loss=100*pdif(ip)*(vol-resuv(ip,All))/denom
fi2=drawline(udif,loss,show->0,color->(1+ip),width->2,show->0,append->append)
denom=upulp*(veetot-saw)+saw*usaw
loss=100*t(udif)*.(saw-resus(ip,All))/.denom  !black hole
fi3=drawline(udif,loss,show->0,color->(1+ip),width->2,show->0,append->append)

append=1

fi=drawline(udif,resuv(ip,All),append->,color->(1+ip),continue->,width->2,show->0)

!Blue=4
!Red=2
;enddo
fi2=drawline(2.2,2.5,2.4,2.4,color->Blue,append->,show->0,width->2,
label->' p_{dif}=2.5"font"')
fi2=drawline(2.2,2.5,2,2,color->Green,append->,show->0,width->2,
label->' p_{dif}=2.0"font"')
fi2=drawline(2.2,2.5,1.6,1.6,color->Red,append->,show->1,width->2,continue->,
label->' p_{dif}=1.5"font"',
xlabel->'u_{dif}"font"',ylabel->'Forest owner loss, %"font"',
yrange->(0,8))

fi3=drawline(3.2,3.5,6.4,6.4,color->Blue,append->,show->0,width->2,
label->' u_{pulp}=4"font"')

fi3=drawline(3.2,3.5,6.,6.,color->Green,append->,show->0,width->2,
label->' u_{pulp}=3"font"')

fi3=drawline(3.2,3.5,5.6,5.6,color->Red,append->,show->1,width->2,continue->,
label->' u_{pulp}=2"font"',
xlabel->'u_{dif}"font"',ylabel->'Utility black hole, %"font"',
yrange->(0,8))



!fi2=drawline(2.1,2,append->,show->0,label->'Upper curves: Forest owner"font"',width->2)

!fi2=drawline(2.1,1,append->,show->1,
!label->'Lower curves: Black hole"font"',width->2,continue->)



!show(fi2,continue->)
!fi=drawline(10,12,97.5,97.5,show->0,append->,width->2,color->Red,label->' V_{saw}"font"')
fi=drawline(udif(1),udif(len(udif))-0.3,saw,saw,append->,color->Black,show->0,label->' Max"font"')
!fi=drawline(10,12,98,98,show->0,width->2,append->,color->Blue,label->' V_{log}"font"')

fi=drawline(3.2,72,append->,show->0,
label->' "ifast" "vol]f5.2[" "saw]f5.2[" Above: 100*V_{log}/V_{com}"font"')

up=10
fi=drawline(3.2,3.4,57+up,57+up,color->Red,append->,show->0,width->2,
label->' p_{dif}=1.5"font"')

fi=drawline(3.2,3.4,54+up,54+up,color->Green,append->,show->0,width->2,
label->' p_{dif}=2.0"font"')

fi=drawline(3.2,3.4,51+up,51+up,color->Blue,append->,show->0,continue->,width->2,
label->' p_{dif}=2.5"font"')

fi=drawline(3.2,55,append->,show->0,
label->' Below: 100*V_{saw}/V_{com}"font"')

!3.5
fi=drawline(udif(1),udif(len(udif))-0.3,vol,vol,append->,color->Black,show->1,continue->,
label->' Max"font"',
xlabel->'u_{dif}"font"',ylabel->'%"font"')
;return

!;current:


fi=drawline(resuv,resus,continue->)
;return

fig=draw(func->(3*interpolate(udif,resus,lam,q->)-interpolate(udif,resuv,lam,q->)),
x->lam,xrange->(udif(1),5),continue->)

fig=draw(func->(3.5*interpolate(udif,resus,lam,q->)-interpolate(udif,resuv,lam,q->)),
x->lam,xrange->(udif(1),5),continue->,color->Red,append->)

;return


!fi2=drawline(resuv,resus,continue->)
lun=len(udif)
sawder=matrix(lun-1)
volder=matrix(lun-1)
udif2=matrix(lun-1)

;do(i,1,lun-1)
sawder(i)=(resus(i+1)-resus(i))/(udif(i+1)-udif(i))
volder(i)=(resuv(i+1)-resuv(i))/(udif(i+1)-udif(i))
udif2(i)=0.5*(udif(i+1)+udif(i))
;enddo
fi2=drawline(udif2,sawder,color->Red,show->0)
fi2=drawline(udif2,volder,color->Blue,append->,continue->)


;return



;range:  compare different objectives

Maxlines=23
lmin=37
lmax=55
dmin=15
dmin=5
lmins=matrix(3,values->(37,40,43))
lmaxs=matrix(3,values->(52,55,58))
dmins=matrix(3,values->(14,15,16))


resu=matrix(9,12)  !maxlog log/veesum saw/veesum   minlog  maxsaw/log maxsaw maxlog

! u=stemopt(0,lmins(1),lmaxs(1),dmins(1),dmin,1,pinedee, 
! func->"Func")
! u%vars;
!reslist=list(u%logv,u%syl,u%saw,u%inc,u%outc,
!u%dtop,u%len,u%nlog)
!resv=matrix(8)
veesum=0

trana=trans()

! { nobr=matrix(9) }
! { nobr2=matrix(9)  !number of logs }
nob=0

do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,lmins(1)).lt.dmins(1))goto(nex) 
top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume


u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs, 
func->"Func",obj->(-u%logv-0.0001*u%saw))  !,print->)  !(-u%logv+1000))

if(u%nlog.le.0)goto(nex)

nob=nob+1

resu(2,1)=resu(2,1)+u%logv
resu(2,2)=resu(2,2)+u%saw

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs, 
func->"Func",obj->(-u%logv+0.0001*u%saw))  !,print->)  !(-u%logv+1000))





resu(2,3)=resu(2,3)+u%logv
resu(2,4)=resu(2,4)+u%saw

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs, 
func->"Func",obj->(u%saw/u%logv))

resu(2,5)=resu(2,5)+u%logv
resu(2,6)=resu(2,6)+u%saw

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs, 
func->"Func",obj->u%saw)

resu(2,7)=resu(2,7)+u%logv
resu(2,8)=resu(2,8)+u%saw



u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs, 
func->"Func",obj->u%logv-0.00001*u%saw)

resu(2,9)=resu(2,9)+u%logv
resu(2,10)=resu(2,10)+u%saw

u=stemopt(lmins(1),lmaxs(1),dmins(1),dmin,pinedee,iobs, 
func->"Func",obj->u%logv+0.00001*u%saw)

resu(2,11)=resu(2,11)+u%logv
resu(2,12)=resu(2,12)+u%saw


!***
u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs, 
func->"Func",obj->(-u%logv-0.0001*u%saw))  !!(-u%logv+10000))
!if(u%nlog.le.0)goto(start)

resu(1,1)=resu(1,1)+u%logv
resu(1,2)=resu(1,2)+u%saw

u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs, 
func->"Func",obj->(-u%logv+0.0001*u%saw))  !!(-u%logv+10000))
!if(u%nlog.le.0)goto(start)

resu(1,3)=resu(1,3)+u%logv
resu(1,4)=resu(1,4)+u%saw



u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs, 
func->"Func",obj->(u%saw/u%logv))
!if(u%nlog.le.0)goto(start)

resu(1,5)=resu(1,5)+u%logv
resu(1,6)=resu(1,6)+u%saw

u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs, 
func->"Func",obj->(u%saw))
!if(u%nlog.le.0)goto(start)

resu(1,7)=resu(1,7)+u%logv
resu(1,8)=resu(1,8)+u%saw



u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs, 
func->"Func",obj->(u%logv-0.001*u%saw))
!if(u%nlog.le.0)goto(start)

resu(1,9)=resu(1,9)+u%logv
resu(1,10)=resu(1,10)+u%saw

u=stemopt(lmins(2),lmaxs(2),dmins(2),dmin,pinedee,iobs, 
func->"Func",obj->(u%logv+0.001*u%saw))
!if(u%nlog.le.0)goto(start)

resu(1,11)=resu(1,11)+u%logv
resu(1,12)=resu(1,12)+u%saw


start:irow=2
do(le1,1,3,2)

do(le2,1,3,2)

do(dm,1,3,2)

if((le1+le2+dm).ne.3)then
irow=irow+1
u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs, 
func->"Func",obj->(-u%logv-0.0001*u%saw))   !-u%logv+10000))


resu(irow,1)=resu(irow,1)+u%logv
resu(irow,2)=resu(irow,2)+u%saw

u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs, 
func->"Func",obj->(-u%logv+0.0001*u%saw))   !-u%logv+10000))


resu(irow,3)=resu(irow,3)+u%logv
resu(irow,4)=resu(irow,4)+u%saw

u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs, 
func->"Func",obj->(u%saw/u%logv))
resu(irow,5)=resu(irow,5)+u%logv
resu(irow,6)=resu(irow,6)+u%saw


u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs, 
func->"Func",obj->u%saw)
resu(irow,7)=resu(irow,7)+u%logv
resu(irow,8)=resu(irow,8)+u%saw


u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs, 
func->"Func",obj->(u%logv-0.0001*u%saw))
resu(irow,9)=resu(irow,9)+u%logv
resu(irow,10)=resu(irow,10)+u%saw


u=stemopt(lmins(le1),lmaxs(le2),dmins(dm),dmin,pinedee,iobs, 
func->"Func",obj->u%logv)
resu(irow,11)=resu(irow,11)+u%logv
resu(irow,12)=resu(irow,12)+u%saw

endif

enddo
enddo
enddo



nex:enddo
nob;
resu;
!logsum=resu(All,1)
resu=100*resu/veesum;
! resu(All,1)=100*resu(All,1)/veesum  !logvolume / commercial volume
! resu(All,2,-5)=100*resu(All,2,-5)/veesum  !cylinder,saw,in,out
! resu(All,6)=resu(All,6)/.nobr  ! dtop
! resu(All,7)=resu(All,7)/.nobr2  ! len
! resu(All,8)=resu(All,8)/.nobr
!close('resutot.bin')
/
Maxlines=60
call(trana)

resu;
;rangeb:
head=matrix(9,3)
head(1,1)=lmins(2)
head(1,2)=lmaxs(2)
head(1,3)=dmins(2)
irow=1
;do(le1,1,3,2)

;do(le2,1,3,2)
;do(dm,1,3,2)
irow=irow+1
head(irow,1)=lmins(le1)
head(irow,2)=lmaxs(le2)
head(irow,3)=dmins(dm)




;enddo
;enddo
;enddo
head;
total=matrix(1,2,matrix->,values->(head,resu(All,All)));
;if(exist_f('range2.txt'))delete_f('range2.txt')
write('range2.txt','(f3.0,2f4.0,12f5.1)',total)
close('range2.txt')
print_f('range2.txt')
! ;if(exist_f('ana1b.txt'))delete_f('ana1b.txt')
! write('ana1b.txt','(3f4.0,8f8.4)',total)
! close('ana1b.txt')
! print_f('ana1b.txt')
;return


!;logjarc:
labelmin1=25
labelmax1=28
!=matrix(2,values->(32,390)) !%
!labelmax=matrix(2,values->(35,440))
fi0=fi1
fi0=draw(func->(100*bf*gam**Dtop),x->D,xrange->(Dtop%min,40),append->,show->0,
color->Violet)
fi0=drawline(16,31,color->Violet,append->,show->0,label->'100*(1-f(D))"font"')
fi0=drawline(labelmin1,labelmax1,22,22,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)
fi0=drawline(labelmin1,labelmax1,12,12,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2)
fi0=drawline(labelmin1,labelmax1,42,42,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)
fi0=drawline(labelmin1,labelmax1,57,57,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)
fi0=drawline(xlabel->'D_{top}, cm"font"',ylabel->'V_{*} / V_{com}, %"font"',append->,show->0)
show(fi0,continue->)

!;logjarc:
labelmin2=390
labelmax2=440

fi0=fi2
fi0=drawline(labelmin2,labelmax2,18.5,18.5,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)
fi0=drawline(labelmin2,labelmax2,14,14,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2)
fi0=drawline(labelmin2,labelmax2,42,42,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)
fi0=drawline(labelmin2,labelmax2,57,57,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)
fi0=drawline(xlabel->'V_{log}, dm^3"font"',ylabel->'V_{*} / V_{com}, %"font"',append->,show->0)
show(fi0,continue->)

;!logjarc:
labelminb1=32
labelmaxb1=35
fi0=fib1
fi0=drawline(xlabel->'D_{top}, cm"font"',ylabel->'V_{*}, dm^3"font"',
yrange->(0,600),append->,show->0)

fi0=drawline(labelminb1,labelmaxb1,580,580,label->' V_{log}"font"',append->,show->0,color->Cyan,
width->2)


fi0=drawline(labelminb1,labelmaxb1,70,70,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)
fi0=drawline(labelminb1,labelmaxb1,30,30,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2)
fi0=drawline(labelminb1,labelmaxb1,190,190,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)
fi0=drawline(labelminb1,labelmaxb1,250,250,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)

show(fi0,continue->)
;logjarc:
labelminb2=550
labelmaxb2=580
fi0=fib2
fi0=drawline(xlabel->'V_{log}, dm^3"font"',ylabel->'V_{*}, dm^3"font"',
yrange->(0,500),append->,show->0)
fi0=drawline(labelminb2,labelmaxb2,110,110,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)
fi0=drawline(labelminb2,labelmaxb2,30,30,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2)
fi0=drawline(labelminb2,labelmaxb2,220,220,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)
fi0=drawline(labelminb2,labelmaxb2,330,330,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)
!b1
show(fi0,continue->)


labelmin=matrix(2,values->(34,800))
labelmax=matrix(2,values->(38,950))
fi=0
;do(kier,1,2)

cl=classify(Logp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)


!fi=drawclass(cl,color->Red,width->2,continue->cont,show->0)
fi=drawclass(cl,sd->,se->,continue->,color->Red,show->0,append->,width->(1,2))
!fi=drawclass(cl,se->,mean->,color->Black,width->2,append->,show->0)

fi=drawline(labelmin(kier),labelmax(kier),90,90,label->' V_{log}"font"',append->,show->0,color->Red,
width->2)

cl=classify(Pulpp,data->datp,x->@xvars(kier),xrange->,
classes->clas,minobs->minob)


fi=drawclass(cl,sd->,se->,continue->,color->Cyan,show->0,append->,width->(1,2))


fi=drawline(labelmin(kier),labelmax(kier),9,9,label->' V_{pulp}"font"',append->,show->0,color->Cyan,
width->2)
iap=1
cl=classify(Chipp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)

fi=drawclass(cl,sd->,se->,continue->,color->Green,show->0,append->,width->(1,2))

fi=drawline(labelmin(kier),labelmax(kier),39,39,label->' V_{chip}"font"',append->,show->0,color->Green,
width->2)

Lmax;;

cl=classify(outcp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)

fi=drawclass(cl,sd->,se->,continue->,color->Blue,show->0,append->,width->(1,2))

fi=drawline(labelmin(kier),labelmax(kier),25,25,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)

cl=classify(Sawp,data->datp,x->@xvars(kier),
xrange->,classes->clas,minobs->minob)


fi=drawclass(cl,sd->,se->,continue->,color->Orange,show->0,append->,width->(1,2))


fi=drawline(labelmin(kier),labelmax(kier),65,65,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)

cl=classify(incp,data->datp,x->@xvars(kier),xrange->,
classes->clas,minobs->minob)

;if(kier.eq.1);then
fi=drawclass(cl,continue->cont,color->Violet,append->,
xlabel->'Dbh, cm"font"',sd->,se->,
ylabel->'Component / V_{com}, %"font"',width->2,continue->cont,show->is)

fi4=drawclass(cl,continue->cont,color->Violet,append->,
xlabel->'Dbh, cm"font"',sd->,se->,
ylabel->'Component / V_{com}, %"font"',width->2,continue->cont,show->is)

;else
fi=drawclass(cl,continue->cont,sd->,se->,color->Violet,append->iap,
xlabel->'V_{com}, dm^3"font"',sd->,se->,
ylabel->'Component / V_{com}, %"font"',width->2,continue->cont,show->is)

;endif
fi=drawline(labelmin(kier),labelmax(kier),15,15,label->' V_{INC}"font"',append->,show->1,color->Violet,
width->2,continue->,yrange->(0,100))

fi=0

;enddo



;return

;logjar:

lmins=matrix(3,values->(37,40,43))
lmaxs=matrix(3,values->(52,55,58))
dmins=matrix(3,values->(14,15,16))

!resu=matrix(9,8)  !maxlog log/veesum saw/veesum   minlog  maxsaw/log maxsaw maxlog

!reslist=list(u%logv,u%syl,u%saw,u%inc,u%outc,
!u%dtop,u%len,u%nlog,u%com,u%pulp,u%chip,Dbh)
reslist=list(Log,Syl,Saw,inc,outc,
Dtop,ilog,Nlog,Chip)
!resv=matrix(8)
veesum=0
Lmin=lmins(2)
Lmax=lmaxs(2)
Dmin=15
dmin=5

trana=trans()

! { nobr=matrix(9) }
! { nobr2=matrix(9)  !number of logs }
nob=0
comp=matrix(2*nrows(pinedee),len(reslist))

do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
!if(pinedee(iobs,Lmin).ge.Dmin)then 
!ob=nob+1
top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume
Dbh=pinedee(iobs,14)
if(Dbh.gt.Dmin)then

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->u%saw)  !,print->)  !(-u%logv+1000))
lentot=0
Log=0
Nlog=u%nlog
do(ilog,1,u%nlog)
nob=nob+1

lentot=lentot+u%lens(ilog)
if(lentot.gt.Lmin)then
Log=pinevee(iobs,lentot)-pinevee(iobs,lentot-Lmin)
else
Log=pinevee(iobs,lentot)
endif
Dtop=pinedee(iobs,lentot)
Syl=pineaa(iobs,lentot)*Lmin
Saw=(1-bf*gam**Dtop)*Syl
inc=Syl-Saw
outc=Log-Syl
Chip=inc+outc

comp(nob,All)=reslist
enddo

endif
!endif
enddo
/
call(trana)
!dat=newdata(comp(1,-nob,All),read->@reslist2)
dat=newdata(comp(1,-nob,All),read->@reslist)
stat()

;logjarb:
!stat(data->@datat(id),min->,max->,filter->(vtop5.ge.100))
mt=trans()
!Logp=100*Log/Com
Sawp=100*Saw/Log
incp=100*inc/Log
outcp=100*outc/Log
Chipp=100*Chip/Log
/


datp=newdata(dat,maketrans->mt)
stat(min->,max->)

cont=1
clas=20
minob=7
xvars=list(Dtop,Log)



Dtop%min=15
Log%min=200
fi1=0
fi2=0
fib1=0
fib2=0

;do(kier,1,2)  !x-axes =dtop  Log


cl=classify(Chipp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,continue->,color->Green,se->,sd->,show->0,width->(1,2))

cl=classify(Chip,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fib"kier"=drawclass(cl,se->,sd->,continue->,color->Green,show->0,width->(1,2))

cl=classify(outcp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fi"kier"=drawclass(cl,continue->cont,color->Blue,show->0,append->,width->2)

cl=classify(outc,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fib"kier"=drawclass(cl,continue->cont,color->Blue,show->0,append->,width->2)

cl=classify(Sawp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)

fi"kier"=drawclass(cl,color->Orange,se->,sd->,width->2,append->,show->0)


cl=classify(Saw,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)
fib"kier"=drawclass(cl,color->Orange,se->,sd->,width->(1,2),append->,show->0)


cl=classify(incp,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)

fi"kier"=drawclass(cl,continue->cont,color->Violet,append->,show->0,width->2)

cl=classify(inc,data->datp,x->@xvars(kier),xrange->,classes->clas,minobs->minob)

fib"kier"=drawclass(cl,continue->cont,color->Violet,append->,show->0,width->2)


;enddo

cl=classify(Log,data->datp,x->Dtop,xrange->,classes->clas,minobs->minob)
fib1=drawclass(cl,color->Cyan,se->,sd->,width->(1,2),append->,show->0)

;return
!;logjarc:
labelmin1=25
labelmax1=28
!=matrix(2,values->(32,390)) !%
!labelmax=matrix(2,values->(35,440))
fi0=fi1
fi0=draw(func->(100*bf*gam**Dtop),x->D,xrange->(Dtop%min,40),append->,show->0,
color->Violet)
fi0=drawline(16,31,color->Violet,append->,show->0,label->'100*(1-f(D))"font"')
fi0=drawline(labelmin1,labelmax1,22,22,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)
fi0=drawline(labelmin1,labelmax1,12,12,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2)
fi0=drawline(labelmin1,labelmax1,42,42,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)
fi0=drawline(labelmin1,labelmax1,57,57,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)
fi0=drawline(xlabel->'D_{top}, cm"font"',ylabel->'V_{*} / V_{com}, %"font"',append->,show->0)
show(fi0,continue->)

!;logjarc:
labelmin2=390
labelmax2=440

fi0=fi2
fi0=drawline(labelmin2,labelmax2,18.5,18.5,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)
fi0=drawline(labelmin2,labelmax2,14,14,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2)
fi0=drawline(labelmin2,labelmax2,42,42,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)
fi0=drawline(labelmin2,labelmax2,57,57,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)
fi0=drawline(xlabel->'V_{log}, dm^3"font"',ylabel->'V_{*} / V_{com}, %"font"',append->,show->0)
show(fi0,continue->)

;!logjarc:
labelminb1=32
labelmaxb1=35
fi0=fib1
fi0=drawline(xlabel->'D_{top}, cm"font"',ylabel->'V_{*}, dm^3"font"',
yrange->(0,600),append->,show->0)

fi0=drawline(labelminb1,labelmaxb1,580,580,label->' V_{log}"font"',append->,show->0,color->Cyan,
width->2)




fi0=drawline(labelminb1,labelmaxb1,70,70,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)
fi0=drawline(labelminb1,labelmaxb1,30,30,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2)
fi0=drawline(labelminb1,labelmaxb1,190,190,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)
fi0=drawline(labelminb1,labelmaxb1,250,250,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)

show(fi0,continue->)
;logjarc:
labelminb2=550
labelmaxb2=580
fi0=fib2
fi0=drawline(xlabel->'V_{log}, dm^3"font"',ylabel->'V_{*}, dm^3"font"',
yrange->(0,500),append->,show->0)
fi0=drawline(labelminb2,labelmaxb2,110,110,label->' V_{OUTC}"font"',append->,show->0,color->Blue,
width->2)
fi0=drawline(labelminb2,labelmaxb2,30,30,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2)
fi0=drawline(labelminb2,labelmaxb2,220,220,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)
fi0=drawline(labelminb2,labelmaxb2,330,330,label->' V_{saw}"font"',append->,show->0,color->Orange,
width->2)
!b1
show(fi0,continue->)


;return




;return



show(fi0,continue->)
;return

fi1=drawline(labelmin(kier),labelmax(kier),37,37,label->' V_{chip}"font"',show->0,color->Green,
width->2,append->)





fi=drawline(labelmin(kier),labelmax(kier),9,9,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2,continue->)
fi=drawline(labelmin(kier),labelmax(kier),24,24,label->' V_{OUTC}"font"',append->,show->1,color->Blue,
width->2,continue->)

fi=drawline(labelmin(kier),labelmax(kier),10,10,label->' V_{INC}"font"',append->,show->0,color->Violet,
width->2,continue->)
fi=drawline(labelmin(kier),labelmax(kier),26,26,label->' V_{OUTC}"font"',append->,show->1,color->Blue,
width->2,continue->)

xlabel->'D_{top}, cm"font"',
ylabel->'Component / V_{log}, %"font"',width->2,continue->cont,show->0)


;return

;ela2:

Maxlines=23
!like ana7
Maxlines=23

Lmin=40
Lmax=55
Dmin=15
dmin=5

!dif=matrix(11,values->(2.5,2.75,3,3.5,4,4.5,5,6,7,8,9))
udif=matrix(do->(2.5,7,0.25))
plogs=matrix(do->(2.5,4,0.1))
nplogs=len(plogs)
!pdif=matrix(5,values->(1.2,1.9,2,2.1,2.8))
pdif=plogs-1
!;return
! 3*75 - 2*25 =25/25  =1
i1=1
i2=6
i3=12
! 
n3=6
n11=len(udif)
resus=matrix(n11,nplogs)
resuv=matrix(n11,nplogs)
less=matrix(n11,nplogs)
!less2=less
! u=stemopt(0,Lmin,Lmax,Dmin,dmin,1,pinedee, 
! func->(1-bf*gam**D))
! U=stemopt(0,Lmin,Lmax,Dmin,dmin,1,pinedee, 
! func->(1-bf*gam**D))
! u%vars;



tranb=trans() !like trana in ana7


less=0
nob=0
saw=0
vol=0
veesum=0
iup=0
ipp=0


do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
!dmin;
top=find(pinedee(iobs,All),filter->$.lt.dmin)
veesum=veesum+pinevee(iobs,top-1)  !commercial volume

getobs(pinenew,iobs)  !gives lento
! Lmin=lmins(1)
! Lmax=lmaxs(1)
! Dmin=dmins(1)
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->U%saw)

!U%nlog;
if(U%nlog.gt.0)then
nob=nob+1
!@u%vars;
saw=saw+U%saw
vol=vol+u%logv
do(iu,1,n11)
do(ip,1,nplogs)

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(udif(iu)*u%saw+pdif(ip)*u%pulp))
!endif

resus(iu,ip)=resus(iu,ip)+u%saw
resuv(iu,ip)=resuv(iu,ip)+u%logv
iip=u%nlog.lt.u%nlogmax
!ess(iu,ip)=less(iu,ip)+u%nlog.lt.u%nlogmax
less(iu,ip)=less(iu,ip)+iip



enddo
enddo
endif !nlog
endif  !min
enddo

/
Maxlines=60
call(tranb)
nob;
saw=saw/nob;
vol=vol/nob;
resus=resus/nob;
resuv=resuv/nob;
veesum=veesum/nob;
sawopt=100*saw/veesum
volopt=100*vol/veesum
udif;
pdif;
less;

resusf=100*resus/veesum;
resuvf=100*resuv/veesum;

!less=100*less/nob;



;ela2b:
elas=matrix(3,values->(0.5,0.8,1.1))
upulps=matrix(2,values->(2,4)) 
nupulp=len(upulps)
!ask(upulp)
!ask(append)
plogs=pdif+1;   !matrix(do->(2.5,4,0.1))
plog0=plogs(6);
!;return
nelas=len(elas)
nplogs=len(plogs)
!ask(plog0)
uucom=matrix(nelas,nplogs)
uuveesum=matrix(nelas,nplogs)
utilm=matrix(n11,nelas)
sawrm=matrix(n11,nelas)
volrm=matrix(n11,nelas)
transel=trans()
do(ie,1,nelas)
elascur=elas(ie);
do(ip,1,nplogs)

uucom(ie,ip)=(elas(ie)*(plogs(ip)-plog0)/plog0+1)
uuveesum(ie,ip)=uucom(ie,ip)*veesum
enddo
enddo
do(iu,1,n11)

do(ie,1,nelas)
ie,elas(ie),iu,udif(iu);
resuv0=resuv(iu,All);
uuvol=uucom(ie,All)*.resuv0;
resus0=resus(iu,All);
uusaw=uucom(ie,All)*.resus0;
uuveesum0=uuveesum(ie,All);
sawr=100*uusaw/.uuveesum0;
volr=100*uuvol/.uuveesum0;


util=(upulp-t(plogs))*.uuveesum0+udif(iu)*uusaw+pdif(ip)*(uuveesum0-uuvol);
utilma=maxloc(util)
utilm(iu,ie)=plogs(utilma)
sawrm(iu,ie)=sawr(utilma)
volrm(iu,ie)=volr(utilma)
enddo 
enddo
/

append=0
icolor=1
;do(ipu,1,2)
upulp=upulps(ipu)
call(transel)
!3,7,11 3=3, 7=4, 11=5
!Lower lines for higher upulp

;do(ie,1,nelas)
icolor=icolor+1
!uut=sawrm(3,All)
us=sawrm(All,ie)/.resusf(All,6)
fi2=drawline(udif,us,show->0,color->icolor,append->append,width->2,
ylabel->'saw')

uv=volrm(All,ie)/.resuvf(All,6)
fi3=drawline(udif,uv,show->0,color->icolor,append->append,width->2,
ylabel->'V_{log} / V_{log} without elasticity"font"',xlabel->'u_{dif}"font"')
append=1
fi3=drawline(4.4,4.9,1.01+icolor*0.0043,1.01+icolor*0.0043,color->icolor,
label->' e="elas(ie)]f3.1[" u_{pulp}="upulp""font"',append->,show->0,width->2)
fi2=drawline(4.4,4.9,1.01+icolor*0.0043,1.01+icolor*0.0043,color->icolor,
label->' e="elas(ie)]f3.1[", u_{pulp}="upulp""font"',append->,show->0,width->2)
!i2=drawline(4,4.2,65+3*ie,65+3*ie,color->(1+ie),append->,width->2,show->0,
!label->'Elasticity="elas(ie)]f3.1[""font"')

;enddo
!utilm"ipu"=utilm
!sawrm"ipu"=sawrm
!volrm"ipu"=volrm
;enddo
show(fi2,continue->)
show(fi3,continue->)
;return

append=0
;do(ipu,1,2)
upulp=upulps(ipu)
call(transel)
!3,7,11 3=3, 7=4, 11=5
!Lower lines for higher upulp
;do(ie,1,nelas)
!uut=sawrm(3,All)
us=sawrm(All,ie)
fi2=drawline(udif,us,show->0,color->(1+ie),append->append,width->2)
append=1
uv=volrm(All,ie)
fi2=drawline(udif,uv,show->0,color->(1+ie),append->,width->2)

fi2=drawline(4,4.2,65+3*ie,65+3*ie,color->(1+ie),append->,width->2,show->0,
label->'Elasticity="elas(ie)]f3.1[""font"')

;enddo
utilm"ipu"=utilm
sawrm"ipu"=sawrm
volrm"ipu"=volrm
;enddo
fi2=drawline(udif(1),udif(n11),sawopt,sawopt,append->,show->0)
fi2=drawline(udif(1),udif(n11),volopt,volopt,append->,continue->,
xlabel->'u_{dif}"font"',ylabel->'%"font"')
utilm1;
utilm2;
sawrm1;
sawrm2;
volrm1;
volrm2;

;return

uut=sawrm(7,All)
fi2=drawline(elas,uut,append->,show->0,append->,color->Green,width->2)
uut=volrm(7,All)
fi2=drawline(elas,uut,append->,show->0,append->,color->Green,width->2)

;if(append);then
fi2=drawline(0.6,72,append->,show->0,
label->' Above: 100*V_{log}/V_{com}"font"')
fi2=drawline(0.6,52,append->,show->0,
label->' Below: 100*V_{saw}/V_{com}"font"')
pro=100*saw/veesum
fi2=drawline(elas(1),elas(nelas)-0.1,pro,pro,append->,show->0,append->,
color->black,width->1,label->' Max"font"')
pro=100*vol/veesum
fi2=drawline(elas(1),elas(nelas)-0.1,pro,pro,append->,show->0,append->,color->black,
width->1,label->' Max"font"')
fi2=drawline(0.6,0.8,69,69,color->Red,append->,show->0,continue->,width->2,
label->'  u_{dif}=5"font"')
fi2=drawline(0.6,0.8,66,66,color->Green,append->,show->0,continue->,width->2,
label->'  u_{dif}=4"font"')
fi2=drawline(0.6,0.8,63,63,color->Blue,append->,show->0,continue->,width->2,
label->' u_{dif}=3"font"')
fi2=drawline(0.6,59,append->,show->0,
label->' Upper curves: u_{pulp}=3"font"')
fi2=drawline(0.6,56,append->,show->0,
label->' Lower curves: u_{pulp}=4"font"')
;endif
uut=sawrm(11,All)

fi2=drawline(elas,uut,append->,color->Red,continue->,width->2,
ylabel->'%"font"',xlabel->'Elasticity"font"',show->0)
uut=volrm(11,All)
!;if(upulp.eq.4)fi2=drawline(elas,uut,append->,color->Red,continue->,width->4,show->0)
fi2=drawline(elas,uut,append->,color->Red,continue->,width->2)

;return

;iis:

Maxlines=23

Lmin=40
Lmax=55
Dmin=15
dmin=5

udif=matrix(do->(1,4,0.25));
!udif=udif-1

n9=len(udif)

pdif=matrix(3,values->(0.5,0.6,0.7))  !uchip
pdif2=1-pdif  !
npdif=len(pdif)
resus=matrix(npdif,n9)
resuv=matrix(npdif,n9)
!less=matrix(n9)
!ess2=less
! u=stemopt(0,Lmin,Lmax,Dmin,dmin,1,pinedee, 
! func->"Func")
! u%vars;
! U=stemopt(0,Lmin,Lmax,Dmin,dmin,1,pinedee, 
! func->"Func")

!reslist=list(u%logv,u%syl,u%saw,u%inc,u%outc,
!u%dtop,u%len,u%nlog)
!resv=matrix(8)
! veesum=0
! lam=matrix(do->(2.5,6,0.5));
! lyh=matrix(n9,8)
! lyhs=matrix(n9,8)
! lyhv=matrix(n9,8)
! sawtot=matrix(n9)
! logtot=matrix(n9)

trana=trans()


less=0
nob=0
saw=0
vol=0
veesum=0
iup=0
ipp=0
iprint=0
do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
!dmin;
top=find(pinedee(iobs,All),filter->$.lt.dmin)
comv=pinevee(iobs,top-1)  !commercial volume

getobs(pinenew,iobs)  !gives lento
! Lmin=lmins(1)
! Lmax=lmaxs(1)
! Dmin=dmins(1)
!U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
!func->"Func")
! if(iobs.eq.79)then
! U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
! func->"Func",obj->U%saw,print->)
! else

!if(iobs.eq.22)iprint=1
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->U%saw,fast->ifast) !,max->)

!endif

!U%nlog;
if(U%nlog.gt.0)then
nob=nob+1
!@u%vars;
saw=saw+U%saw
vol=vol+u%logv
veesum=veesum+comv
do(ip,1,npdif)
do(iu,1,n9)
!do(ip,1,6)

!u=stemopt(6,Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
!func->"Func",param->(udif(iu),pdif(ip)))
! if(iobs.eq.3)then
! u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
! func->"Func",obj->(udif(iu)*u%saw-u%logv),print->)

!else

u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(udif(iu)*u%saw-pdif2(ip)*u%logv),fast->ifast) !,max->)
!endif
! if(iu.eq.8.and.(U%saw.ne.u%saw.or.u%logv.ne.u%logv))then
 ! @u%vars;
 ! @U%vars;
 ! pause('hep>')

 ! endif
if(j_err)return
resus(ip,iu)=resus(ip,iu)+u%saw
resuv(ip,iu)=resuv(ip,iu)+u%logv
!iip=u%nlog.lt.u%nlogmax
!ess(iu,ip)=less(iu,ip)+u%nlog.lt.u%nlogmax
!less(iu)=less(iu)+iip

!u%nlog,u%nlogmax,u%nlog.lt.u%nlogmax;

if(u%lenpot-u%len.ge.Lmin)then
print(u%vars,expand->)

pause('per>')
endif
enddo
enddo
endif !nlog
endif  !min
enddo

/
Maxlines=60
cpu0=cpu()
call(trana)
cpu1=cpu()
cputot=cpu1-cpu0;
nob;
saw=saw/nob;
vol=vol/nob;
resus=resus/nob;
veesum=veesum/nob;
resus=100*resus/veesum;
resuv=resuv/nob;
resuv=100*resuv/veesum;
saw=100*saw/veesum;
vol=100*vol/veesum;
volsaw=resuv/saw
udif;
! pdif;
!ess;
!less2;
!less=100*less/nob;

;iisb:
pp=26;
!ask(PP)
PP=26
pv=72;
ps=100

!udif(5)=2
iappend=0
!;do(ii,1,5)
!ask(PP)
psa=(pv-pp)*volsaw(2,All)+PP+(pp-PP)*100/saw;

fi=drawline(udif,psa,append->iappend)
psai=psa
resuvi=resuv
volsawi=volsaw
!iappend=1
!;enddo

;return
trans8=trans()
up=ico*pp;
!psa=(pv-pp)*volsaw(2,All)+pp;

!psa=(pv-pp)*volsaw(2,All)+pp;
psa=(pv-pp)*volsaw(2,All)+PP+(pp-PP)*100/saw;

fi=drawline(udif,psa,append->iappend
!price same
!udif=usaw-upulp
!uti=usaw*saw+upulp*(100-saw)-psaw*saw-ppulp*(100-saw)
usaw=pv*udif+up;
psa;
resus(2,All);

utibas=usaw*saw+up*(100-saw)-pp*(100-saw) !without psa
uti=utibas-psa*.resus(2,All);

! uti11=usaw*saw
! uti12=up*(100-saw)
! uti21=psa*.resus(2,All)
! uti22=pp*(100-saw)
!pause('>>>')
!uti omistaja saaa sman
!uti=udif*pv*saw-pdif2*pv*vol

!pdif2=1-pdif  pdif=plog-ppulp   plog=1  ppulp=1-pdif
!obj->(udif(iu)*u%saw-pdif2(ip)*u%logv)

!utiold=(up-pv)*100+pp*t(udif)*.resus(2,All)+(pv-pp)*(100-resuv(2,All));
usaw;
resus(2,All);
!pause('<<<')

utiold=t(usaw)*.resus(2,All)+up*(100-resus(2,All))-pv*.resuv(2,All)-pp*(100-resuv(2,All));
! oti11=t(usaw)*.resus(2,All)
! oti12=up*(100-resus(2,All))
! oti21=pv*resuv(2,All)
! oti22=pp*(100-resuv(2,All))
! uti11,oti11;
! uti12,oti12;
! uti21,oti21;
! uti22,oti22;
! usaw*saw+up*(100-saw)-psa*.resus(2,All)-pp*(100-saw)=utiold;
! uti=usaw*saw+up*(100-saw)-psa*.resus(2,All)-pp*(100-saw)=utiold; ratk psa
!pause('>f>')
!(udif*pp-psa+pp)*saw+(up-pp)*100=utiold
! (udif*pp+pp)*saw+(up-pp)*100=psa*saw
!psa=(pv-pp)*volsaw(2,All)+pp;
res=resus(2,All)
!uti=utibas-psa*.resus(2,All)=utiold
psa2=(utibas-utiold)/.res;
!uti usaw*saw+up*(100-saw)-psa*.resus(2,All)-pp*(100-saw)=utiold
!uti=utiold :
usaw;
res;
!pause('xb<')
! sama utilitetti kuin ennene

!psa2=saw*t(usaw)/.res+up*(100-saw)/.res+pp*(100-saw)/.res-utiold/.res;

!uti2=(udif*pp-psa2+pp)*saw+(up-pp)*100;

incold=pv*resuv(2,All)+pp*(100-resuv(2,All));
!incnew=psa*saw+pp*(100-saw);
incnew2=psa2*saw+pp*(100-saw) ;  !saem utility
!utiold;
!uti2;
!;return
! xlabel->'u_{dif}"font"',ylabel->'P_{saw} "font"') 


rat=100*(uti-utiold)/.utiold;
!rat2=100*(uti2-utiold)/.utiold;

!rati=100*(incnew-incold)/.incold;
rati2=100*(incnew2-incold)/.incold;  !does not depend on upulp
if(iappend.eq.0)then
fip=drawline(udif,psa,color->Red,continue->,width->2,show->0,
 xlabel->'u_{dif}"font"',ylabel->'P_{saw} "font"')
 fip=drawline(2.1,2.4,101.1,101.1,width->2,color->Red,show->0,append->,label->' Same owner income"font"')
fip=drawline(udif,psa2,color->Blue,width->2,append->,show->0)
fip=drawline(2.1,2.4,101.9,101.9,width->2,color->Blue,append->,label->' Same company utility"font"',
continue->)


endif
 
fi=drawline(udif,rat,color->icolor,continue->,width->2,show->0,append->iappend)

!i=drawline(udif,rati2,color->icolor,width->2,show->0,
!append->)
if(iappend.eq.0)then
fi=drawline(udif,rati2,color->Orange,continue->,width->2,show->0,
append->,
 xlabel->'u_{dif}"font"',ylabel->'Gain, %"font"')
 endif
iappend=1
/

ico=2
!up=2*pp
icolor=Red
iappend=0
!ishow=0

call(trans8)
fi=drawline(2.4,2.7,6,6,color->icolor,append->,show->0,label->' u_{pulp}="up""font"',width->2)
fi=drawline(2.,7,append->,show->0,label->' Same owner income"font"',width->2)
ico=3
icolor=Green

call(trans8)
fi=drawline(2.4,2.7,5.5,5.5,color->icolor,append->,show->0,label->' u_{pulp}="up""font"',width->2)
ico=4
ishow=1
icolor=Blue
call(trans8)
fi=drawline(2.1,2.4,4,4,color->Orange,width->2,append->,show->0,label->' Same company utility"font"',width->2)
fi=drawline(2.4,2.7,5.,5.,color->icolor,append->,show->1,label->' u_{pulp}="up""font"',width->2,continue->)

;return




upulps=matrix(3,values->(2,3,4))
upulps=upulps+1

append=0
;do(ip,1,npdif)
usaw=upulps(ip)+udif
fi=drawline(udif,resus(ip,All),show->0,color->(1+ip),width->2,show->0,append->append)
! plog=pdif(ip)+1
! denom=(plog*vol+(100-vol))
! loss=100*pdif(ip)*(vol-resuv(ip,All))/denom
! fi2=drawline(udif,loss,show->0,color->(1+ip),width->2,show->0,append->append)
! denom=upulp*(veetot-saw)+saw*usaw
! loss=100*t(udif)*.(saw-resus(ip,All))/.denom  !black hole
! fi3=drawline(udif,loss,show->0,color->(1+ip),width->2,show->0,append->append)

append=1

fi=drawline(udif,resuv(ip,All),append->,color->(1+ip),continue->,width->2,show->0)

!Blue=4
!Red=2
;enddo
! fi2=drawline(2.2,2.5,2.4,2.4,color->Blue,append->,show->0,width->2,
! label->' p_{dif}=2.5"font"')
! fi2=drawline(2.2,2.5,2,2,color->Green,append->,show->0,width->2,
! label->' p_{dif}=2.0"font"')
! fi2=drawline(2.2,2.5,1.6,1.6,color->Red,append->,show->1,width->2,continue->,
! label->' p_{dif}=1.5"font"',
! xlabel->'u_{dif}"font"',ylabel->'Forest owner loss, %"font"',
! yrange->(0,8))

! fi3=drawline(3.2,3.5,6.4,6.4,color->Blue,append->,show->0,width->2,
! label->' u_{pulp}=4"font"')

! fi3=drawline(3.2,3.5,6.,6.,color->Green,append->,show->0,width->2,
! label->' u_{pulp}=3"font"')

! fi3=drawline(3.2,3.5,5.6,5.6,color->Red,append->,show->1,width->2,continue->,
! label->' u_{pulp}=2"font"',
! xlabel->'u_{dif}"font"',ylabel->'Utility black hole, %"font"',
! yrange->(0,8))



!fi2=drawline(2.1,2,append->,show->0,label->'Upper curves: Forest owner"font"',width->2)

!fi2=drawline(2.1,1,append->,show->1,
!label->'Lower curves: Black hole"font"',width->2,continue->)



!show(fi2,continue->)
!fi=drawline(10,12,97.5,97.5,show->0,append->,width->2,color->Red,label->' V_{saw}"font"')
fi=drawline(udif(1),udif(len(udif))-0.3,saw,saw,append->,color->Black,show->0,label->' Max"font"')
!fi=drawline(10,12,98,98,show->0,width->2,append->,color->Blue,label->' V_{log}"font"')

fi=drawline(2.2,72,append->,show->0,
label->' Above: 100*V_{log}/V_{com}"font"')

;do(ip,1,3)
fi=drawline(2.2,2.4,58+ip*3,58+ip*3,color->(ip+1),append->,show->0,width->2,
label->' u_{chip}="pdif(ip)]f3.1[""font"')

;enddo
! up=10
! fi=drawline(2.2,3.4,57+up,57+up,color->Red,append->,show->0,width->2,
! label->' p_{dif}=1.5"font"')

! fi=drawline(2.2,3.4,54+up,54+up,color->Green,append->,show->0,width->2,
! label->' p_{dif}=2.0"font"')

! fi=drawline(2.2,3.4,51+up,51+up,color->Blue,append->,show->0,continue->,width->2,
! label->' p_{dif}=2.5"font"')

fi=drawline(2.2,55,append->,show->0,
label->' Below: 100*V_{saw}/V_{com}"font"')
!3.2,3.4
!3.5
fi=drawline(udif(1),udif(len(udif))-0.3,vol,vol,append->,color->Black,show->1,continue->,
label->' Max"font"',
xlabel->'u_{saw}-u_{chip}"font"',ylabel->'%"font"')
;return

;lp:

Lmin=40
Lmax=55
Dmin=15
dmin=5

tranb=trans() !like trana in ana7
saw=0
com0=0
do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->U%saw,write->('lp0.bin','B',All))
!U%obs,U%nlog,U%saw,U%chip,U%pulp,u%logv))
!rint(U%vars,expand->)

!pause('<>')
!saw=saw+U%saw
!com0=com0+U%com
endif
!iobs,U%saw;
enddo
/
cpu0=cpu()
$$=0
call(tranb)
close('lp0.bin')
cpu1=cpu()-cpu0;
$$=51
cpu0=cpu()
$$=0
call(tranb)
close('lp0.bin')
cpu1=cpu()-cpu0;


saw;
close('lp0.bin')
;return

;lpb:

! lp0vars=list(obj,nlogmin,nlogmax,lenpot,logpot,com,lentot,nlog,dtop,logv,syl,inc,outc,saw,pulp,lenn,pulplen,lens1...lens7,logs1...logs7,syls1...syls7,incs1...incs7,outcs1...outcs7,saws1...saws7,chips1...chips7,chip,tree,totc,s,lencs1...lencs7,dtops1...dtops7)

;incl(lp0.inc)

tot=trans()
saw=100*saw/com0
chip=100*chip/com0
pulp=100*pulp/com0
totc=chip+pulp
com=totc+saw
logv=100*logv/com0
/
!;lpb:
!
lpdat0=data(read->@lp0vars,in->'lp0.bin',form->'B')  !,maketrans->tot)
stat()
classvector(com,class->tree,min->,data->lpdat0)
com0=sum(com[tree]%min);
tot=trans()
saw=100*saw/com0
chip=100*chip/com0
pulp=100*pulp/com0
totc=chip+pulp
com=totc+saw
logv=100*logv/com0
/
lpdat=newdata(lpdat0,maketrans->tot)
stat()


;return
lpdat1=newdata(lpdat0,filter->nlog)


stat()
write('lp.bin','B',lpdat0%matrix)
close('lp.bin')
write('lp1.bin','B',lpdat1%matrix)
close('lp1.bin')


;lpb:
lpdat=data(read->(tree,nlog,saw,chip,pulp,logv,totc,com),in->'lp.bin',form->'B')
stat()
lpdat1=data(read->(tree,nlog,saw,chip,pulp,logv,totc,com),in->'lp1.bin',form->'B')
stat()
;return



!jl=jlp(unit->tree,data->lpdat,problem->prob)



;return



;clus0:
Lmin=40
Lmax=55
Dmin=15
dmin=5

trani0=trans() !like trana in ana7
saw=0
chip=0
logv=0
obj=0
saw0=0
chip0=0
vol0=0
objb0=0
vtot=0
do(iobs,1,nrows(pinedee))
if(100*int(iobs/100).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->u%saw)  !,any->)
!print(u%vars,expand->)
!uu=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
!unc->(1-bf*gam**Dtop),obj->uu%saw,fast->)
!print(u%vars,expand->)
!pause('<>')
vtot=vtot+u%com
u%totc,u%chip,u%pulp,u%chip+u%pulp
!pause('<>')
saw0=saw0+u%saw
chip0=chip0+u%chip
vol0=vol0+u%logv
obj0=obj0+u%ob0
totc0=totc0+u%totc
endif

!iobs,U%saw;
enddo
/

call(trani0)
saw0=100*saw0/vtot;
logv0=100*logv0/vtot;
chip0=100*chip0/vtot;
vol0=100*vol0/vtot;
totc0=100*totc0/vtot;

! lenpsaw=len(psaws)
! lams=matrix(3,values->(20,30,40))
! colors=matrix(3,values->(Blue,Green,Red))
colors=matrix(3,values->(Blue,Green,Red))
Lmin=40
Lmax=55
Dmin=15
dmin=5



tranc=trans() !like trana in ana7
saw=0
chip=0
vol=0
obj=0
totc=0
saw2=0

nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(psaw*U%saw+pchip*U%totc-plog*u%logv-ppulp*U%pulp))
saw=saw+U%saw
chip=chip+U%chip
vol=vol+u%logv
obj=obj+U%obj
totc=totc+U%totc
nob=nob+1
!pause('<')
! if(any)then
! u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, any->,
! func->"Func",obj->(psaw*u%saw+pchip*u%totc-plog*u%logv-ppulp*u%pulp))
! !pause('<>')
! !print(u%vars,expand->)
! endif
endif
enddo
saw=100*saw/vtot;
vol=100*vol/vtot;
obj=100*obj/vtot;
totc=100*totc/vtot;
chip=100*chip/vtot;
/
;return

;clus:
! lenlam=len(lams)

!lam=20
plog=75
ppulp=25
!saw=200
lam=30
cc=3
any=0


pchips=matrix(do->(30,220,20))
psaws=matrix(3,values->(180,220,260))
lenpchip=len(pchips)
lenpsaw=len(psaws)
resus=matrix(lenpsaw,lenpchip)
resuv=matrix(lenpsaw,lenpchip)
resuc=matrix(lenpsaw,lenpchip)
resuo=matrix(lenpsaw,lenpchip)

resus2=matrix(lenpsaw,lenpchip)
resuv2=matrix(lenpsaw,lenpchip)
resuc2=matrix(lenpsaw,lenpchip)

!pchip=50

;do(il,1,lenpsaw)
psaw=psaws(il);
;do(ic,1,lenpchip)
pchip=pchips(ic);
!Ppulp=pchip-cc;


call(tranc)

resus(il,ic)=saw
resuv(il,ic)=vol
resuc(il,ic)=totc
resuo(il,ic)=obj
! ;if(any);then
! resus2(il,ic)=saw2
! resuv2(il,ic)=vol2
! resuc2(il,ic)=totc2

! ;endif
;enddo
;enddo
! resuv=100*resuv/vtot;
! resus=100*resus/vtot;
! resuc=100*resuc/vtot;
! resuo=100*resuo/vtot;
;clusb:
fi=0
labx=70
labd=7
labelh=matrix(3,values->(70,67,64))
;do(il,1,lenpsaw)
fi=drawline(pchips,resuv(il,All),append->,show->0,color->colors(il),width->2)
fi=drawline(pchips,resus(il,All),append->,show->0,color->colors(il),width->2)
fi=drawline(pchips,resuc(il,All),append->,show->0,color->colors(il),width->2)
fi=drawline(labx,74,append->,show->0,label->' Upper curves: V_{log}"font"')
fi=drawline(labx,60,append->,show->0,label->' Lower increasing curves: V_{totc}"font"')
fi=drawline(labx,57,append->,show->0,label->' Lower decreasing curves: V_{saw}"font"')
fi=drawline(labx,labx+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
label->' P_{saw}= "psaws(il)""font"',append->,show->0)
;enddo
! ;if(any);then
! resuv2=100*resuv2/vtot;
! resus2=100*resus2/vtot;
! resuc2=100*resuc2/vtot;
! fi=drawline(pchips,resuv2,append->,show->0,color->Green,width->2)
! fi=drawline(pchips,resus2,append->,show->0,color->Green,width->2)
! fi=drawline(pchips,resuc2,append->,show->0,color->Cyan,width->2)


! ;endif

fi0=fi
fi0=drawline(pchips(1),pchips(lenpchip)-15,saw0,saw0,append->,show->0,label->' Max"font"')
fi0=drawline(pchips(1),pchips(lenpchip)-15,vol0,vol0,append->,show->0,label->' Max"font"')
fi0=drawline(xlabel->'P_{chip}"font"',ylabel->'%"font"',
xrange->(30,210),append->,continue->)

show(fi0,continue->)
nob;
;return

;cluso:
plog=75
ppulp=25
pchip=120
psaw=220
! !call(tranc)
! saw0=100*saw/vtot;
! vol0=100*vol/vtot;
! totc0=100*totc/vtot;
! chip0=100*chip/vtot;
probmin0=problem()

pchip*totc+psaw*saw-plog*logv-ppulp*pulp==max

/


call(tranc)

jl9=jlp(unit->tree,data->lpdat,problem->probmin0,stop->Change%.lt.0.0001)


probmin=problem()

psaw*saw-plog*logv-ppulp*pulp==max
totc=51.5432755
/


pchip=0
call(tranc)
jl9=jlp(unit->tree,data->lpdat,problem->probmin,stop->Change%.lt.0.0001)

;clusf:
li=;list(jl9?);
jl9%xvar,jl9%vc,jl9%vx,jl9%shprice,jl9%xprice,jl9%xsum;

probs=problem()
jl9%xprice*jl9%xvar==max
/



les=matrix((55-40)/3+1)
lenles=len(les);
des=matrix(47-15+1)
lendes=len(des);
pmat=matrix(lendes,lenles)
pmat2=matrix(lendes,lenles)
pmatmin=matrix(lendes,lenles)
pmatmin=100000
pmatmax=-1000000
pmatmax=matrix(lendes,lenles

nmat=matrix(lendes,lenles)
nmat=0.0001


dtops=list(dtops1...dtops7)
lens=list(lens1...lens7)
saws=list(saws1...saws7)
logvs=list(logvs1...logvs7)
chips=list(chips1...chips7)

dtopsm=matrix(7)
lensm=matrix(7)
sawsm=matrix(7)
logvsm=matrix(7)
chipsm=matrix(7)
!
!pulp=com-logv
!logv=chip+saw
!totc=chip+pulp=chip+com-logv
!ptotc*totc=ptotc*(chip+com-logv)=ptotc*(chip-logv)+
!psaw*saw-plog*logv-ppulp*(com-logv)
!!psaw*saw+(ppulp-plog)*logv-ppulp*com
!uti=psaw*saw+plog*logv+ppulp*pulp+ptotc*totc
psaw=jl9%xprice(1);
pdif=jl9%xprice(3)-jl9%xprice(2);
ptotc=jl9%xprice(4);



transp=trans()
ibas=0
ntot=0
do(iobs,1,nobs(lpdat))
getobs(lpdat,iobs)
dtopsm=dtops
lensm=lens
sawsm=saws
logvsm=logvs
chipsm=chips
!pause('<>')
do(ilog,1,nlog)
ntot=ntot+1
uti=psaw*sawsm(ilog)+pdif*logvsm(ilog)+ptotc*(chipsm(ilog)-logvsm(ilog))
leni=(lensm(ilog)-40)/3+1
dto=dtopsm(ilog)-14
pmat(dto,leni,sum->)=uti
pmat2(dto,leni,sum->)=uti*uti
nmat(dto,leni,sum->)=1

enddo
enddo
ntot;
pmat;
nmat;
pmatmean=pmat/.nmat;
pmatvar=pmat2/.nmat-pmatmean*.pmatmean;
pmatsd=sqrt(pmatvar);
nsum=sum(nmat,any->);
nmatp=100*nmat/nsum;
maxp=max(pmatmean,any->);
pmatmeanp=100*pmatmean/maxp;
pmatsdp=pmatsd/maxp;
/

call(transp)

!l=jlp(unit->tree,data->lpdat,problem->probs,stop->Change%.lt.0.0001)

;return

;clusnow:

Lmin=40
Lmax=55
Dmin=15
dmin=5

plog=75
ppulp=25
Pchip=120
!pcom=Pchip-ppulp
Psaw=220
Psawdif=Psaw-Pchip
pvdif=plog-ppulp

les=matrix((55-40)/3+1)
lenles=len(les);
des=matrix(47-15+1)
lendes=len(des);
pmat=matrix(lendes,lenles)
pmatmin=matrix(lendes,lenles)
pmatmin=1000000
pmatmax=-10000
pmatmax=matrix(lendes,lenles)
pmat2=matrix(lendes,lenles)
nmat=matrix(lendes,lenles)
nmat=0.0001

trand=trans() !like trana in ana7
saw=0
chip=0
vol=0
obj=0
totc=0
saw2=0
ntot=0
nob=0
vtot=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(Psaw*U%saw+Pchip*U%totc-plog*U%logv-ppulp*U%pulp))
!u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
!func->"Func",obj->(Psaw*u%saw+Pchip*u%totc-plog*u%logv-ppulp*u%pulp))

do(ilog,1,U%nlog)

ntot=ntot+1
uti=Psawdif*U%saws(ilog)-pvdif*U%logvs(ilog)
leni=(U%lens(ilog)-40)/3+1
dto=U%dtops(ilog)-14
pmat(dto,leni,sum->)=uti
pmatmin(dto,leni)=min(pmatmin(dto,leni),uti)
pmatmax(dto,leni)=max(pmatmax(dto,leni),uti)
pmat2(dto,leni,sum->)=uti*uti
nmat(dto,leni,sum->)=1


if(dto.eq.2.and.leni.eq.1)then
dto,leni,uti;
ilog,uti;
leni,dto;
sa=U%saws(ilog);
vo=U%logvs(ilog);
uti1=Psawdif*U%saws(ilog);
uti2=-pvdif*U%logvs(ilog);
utia=Psawdif*sa;
utib=-pvdif*vo;

Psawdif,pvdif;
U%lens;
U%dtops;

pause('<dh>')
endif
enddo
saw=saw+U%saw
chip=chip+U%chip
vol=vol+u%logv
obj=obj+U%obj
totc=totc+U%totc
nob=nob+1
vtot=vtot+U%com
!pause('<')
! if(any)then
! u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, any->,
! func->"Func",obj->(psaw*u%saw+pchip*u%totc-plog*u%logv-ppulp*u%pulp))
! !pause('<>')
! !print(u%vars,expand->)
! endif
endif
enddo
saw=100*saw/vtot;
vol=100*vol/vtot;
obj=100*obj/vtot;
totc=100*totc/vtot;
chip=100*chip/vtot;
pmatmean=pmat/.nmat;
pmatvar=pmat2/.nmat-pmatmean*.pmatmean;
pmatsd=sqrt(pmatvar);
nsum=sum(nmat,any->);
nmatp=100*nmat/nsum;
maxp=max(pmatmean,any->);
pmatmeanp=100*pmatmean/maxp;
pmatsdp=pmatsd/maxp;
/

call(trand)

tranm=trans() !like trana in ana7
sawm=0
chipm=0
volm=0
objm=0
totcm=0
saw2m=0
nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(Psaw*u%saw+Pchip*u%totc-plog*u%logv-ppulp*u%pulp))
! u%saw+u%chip,u%logv;
! u%logv+u%pulp,u%com;
! u%pulp+u%chip,u%totc;
! Psaw*u%saw+Pchip*u%totc-plog*u%logv-ppulp*u%pulp,u%obj;

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->sum(pmatmean(U%dtops-14,(U%lens-40)/3+1),any->))

!V=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
!func->"Func",obj->(Psawdif*V%saw-pvdif*V%logv))

! V%saw+u%chip,V%logv;
! V%logv+V%pulp,V%com;
! V%pulp+V%chip,V%totc;
! Psawdif*V%saw+Pvdif*V%logv,V%obj;

!func->"Func",obj->(Psaw*U%saw+Pchip*U%totc-plog*U%logv-ppulp*U%pulp))
if(iobs.eq.1)then
u%saw,U%saw,u%nlog,U%nlog,u%logv,U%logv;
u%totc,U%totc;
u%dtops;
U%dtops;
u%lens;
U%lens;
u%obj;
! V%saw,V%nlog,V%logv;
! V%dtops;
! V%lens;
! V%obj;

pm=pmatmean(U%dtops-14,(U%lens-40)/3+1);
pms=sum(pm,any->);
utis=0
do(ilog,1,U%nlog)
ilog;
uti=Psawdif*U%saws(ilog)-pvdif*U%logvs(ilog);
leni=(U%lens(ilog)-40)/3+1;
dto=U%dtops(ilog)-14;
utis=utis+uti;
enddo
utis2=utis+pcom*U%com;
u%lens;
u%dtops;
pause('<>')


endif

sawm=sawm+U%saw
chipm=chipm+U%chip
volm=volm+u%logv
objm=objm+U%obj
totcm=totcm+U%totc
nob=nob+1
!pause('<')
! if(any)then
! u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, any->,
! func->"Func",obj->(psaw*u%saw+pchip*u%totc-plog*u%logv-ppulp*u%pulp))
! !pause('<>')
! !print(u%vars,expand->)
! endif
endif
enddo
sawm=100*sawm/vtot;
volm=100*volm/vtot;
objm=100*objm/vtot;
totcm=100*totcm/vtot;
chipm=100*chipm/vtot;
/
call(tranm)

;return

;pmatfig:
Pchip=35
Psaw=200
Lmin=40
Lmax=55
Dmin=15
dmin=5
!les=matrix((55-40)/3+1)
les=matrix(do->(40,55,3));
lenles=len(les);
!des=matrix(47-15+1)
des=matrix(do->(15,47));
pchips=matrix(do->(30,220,20))
psaws=matrix(3,values->(180,220,260))
lenpchip=len(pchips)
lenpsaw=len(psaws)

lendes=len(des);
pmat=matrix(lendes,lenles)
pmat2=matrix(lendes,lenles)
nmat=matrix(lendes,lenles)

tranf=trans() !like trana in ana7
saw=0
chip=0
logv=0
obj=0
totc=0
saw2=0
com=0
nob=0
if(ismat)then
utilb=0
else
utila=0
endif


do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then


if(ismat)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->sum(pmatmean(U%dtops-14,(U%lens-40)/3+1),any->))
utilb=utilb+Psaw*U%saw

else
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(Psaw*U%saw))
utila=utila+U%obj
do(ilog,1,U%nlog)

ntot=ntot+1
uti=Psaw*U%saws(ilog)
leni=(U%lens(ilog)-40)/3+1
dto=U%dtops(ilog)-14
pmat(dto,leni,sum->)=uti
pmat2(dto,leni,sum->)=uti*uti
nmat(dto,leni,sum->)=1

saw=saw+U%saw
chip=chip+U%chip
logv=vol+U%logv
obj=obj+U%obj
totc=totc+U%totc
com=com+U%com
nob=nob+1

enddo
endif
endif
enddo
if(.not.ismat)then
saw=100*saw/com;
logv=100*logv/com;
obj=100*obj/com;
totc=100*totc/com;
chip=100*chip/com;
endif
/
pmatm=matrix(lenpchip)
pmatsd=matrix(lenpchip)

nmat=0.001
pmat=0
pmat2=0
ismat=0
call(tranf)
utila;
pmatmean=pmat/.nmat;
pmax=max(pmatmean,any->);
coe=100/pmax;
sdmat=sqrt(pmat2/.nmat-pmatmean*.pmatmean);
fi=0
;do(il,1,lenles)
fi=drawline(des,coe*pmatmean(All,il),append->,reject->$y.eq.0,show->0,mark->1,color->Blue) 
;enddo
fi=drawline(xlabel->'D_{top}, cm"font"',ylabel->'Value for different L_{log}"font"',xrange->(15,45),append->,continue->)
!show(fi,continue->)
fib=0
;do(il,1,lendes)
fib=drawline(les,coe*pmatmean(il,All),append->,reject->$y.eq.0,show->0,color->Red,mark->1) 
;enddo
fib=drawline(xlabel->'L_{log}, dm"font"',ylabel->'Value for different D_{top}"font"',xrange->(40,55),append->,continue->)
!show(fib,continue->)

ismat=1
call(tranf)
utilb;
rat=utilb/utila;
;enddo 

;return


;cluspmat:
plog=75
ppulp=25
Pchip=120
pcom=Pchip-ppulp
Psaw=220
Psawdif=Psaw-Pchip
pvdif=plog-ppulp
les=matrix((55-40)/3+1)
lenles=len(les);
des=matrix(47-15+1)
lendes=len(des);
pmat=matrix(lendes,lenles)
pmat2=matrix(lendes,lenles)
nmat=matrix(lendes,lenles)
nmat=0.0001
pchips=matrix(do->(30,220,20))
psaws=matrix(3,values->(180,220,260))
lenpchip=len(pchips)
lenpsaw=len(psaws)
!lenpsaw=1
resus=matrix(lenpsaw,lenpchip) !
resuv=matrix(lenpsaw,lenpchip) !
!resuc=matrix(lenpsaw,lenpchip)
!resuo=matrix(lenpsaw,lenpchip)

tranp=trans() !like trana in ana7
saw=0
chip=0
logv=0
obj=0
totc=0
saw2=0
com=0
nob=0
utila=0
utilb=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then


if(ismat)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->sum(pmatmean(U%dtops-14,(U%lens-40)/3+1),any->))
utilb=utilb+Psaw*U%saw+Pchip*U%totc-plog*U%logv-ppulp*U%pulp

else
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(Psaw*U%saw+Pchip*U%totc-plog*U%logv-ppulp*U%pulp))
utila=utila+U%obj
do(ilog,1,U%nlog)

ntot=ntot+1
uti=Psawdif*U%saws(ilog)-pvdif*U%logvs(ilog)
leni=(U%lens(ilog)-40)/3+1
dto=U%dtops(ilog)-14
pmat(dto,leni,sum->)=uti
pmat2(dto,leni,sum->)=uti*uti
nmat(dto,leni,sum->)=1

saw=saw+U%saw
chip=chip+U%chip
logv=vol+U%logv
obj=obj+U%obj
totc=totc+U%totc
com=com+U%com
nob=nob+1

enddo
endif
endif
enddo
if(.not.ismat)then
saw=100*saw/com;
logv=100*logv/com;
obj=100*obj/com;
totc=100*totc/com;
chip=100*chip/com;
endif
/
pmatm=matrix(lenpchip)
pmatsd=matrix(lenpchip)
util0=matrix(lenpchip,lenpsaw)
util=matrix(lenpchip,lenpsaw)
;do(ip,1,lenpsaw)
Psaw=psaws(ip);
;do(il,1,lenpchip)
Pchip=pchips(il)
Psawdif=Psaw-Pchip;
nmat=0.001
pmat=0
pmat2=0
ismat=0
call(tranp)
util0(il,ip)=utila
Pchip;
pmatmean=pmat/.nmat;
sdmat=sqrt(pmat2/.nmat-pmatmean*.pmatmean);
!nm=nmat(3,1);
pmatm(il)=pmatmean(3,1);
pmatsd(il)=sdmat(3,1);
ismat=1
call(tranp)
util(il,ip)=utilb
;enddo 
;enddo
;cluspmatb:
fi=0
labx=140
labd=9
labelh=matrix(3,values->(96.5,96.8,97.1))
;do(il,1,lenpsaw)
rat=100*util(All,il)/.util0(All,il);
fi=drawline(pchips,rat,append->,show->0,color->colors(il),width->2)

! fi=drawline(labx,74,append->,show->0,label->' Upper curves: V_{log}"font"')
! fi=drawline(labx,60,append->,show->0,label->' Lower increasing curves: V_{totc}"font"')
! fi=drawline(labx,57,append->,show->0,label->' Lower decreasing curves: V_{saw}"font"')
fi=drawline(labx,labx+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
 label->' P_{saw}= "psaws(il)""font"',append->,show->0)
;enddo
fi=drawline(xlabel->'P_{chip}"font"',ylabel->'%"font"',
xrange->(30,210),append->,continue->)

! fi=drawline(pchips,util0,show->0)
! fi=drawline(pchips,util,append->,color->Red,continue->)

! pmatm;
! pmatsd;
! fim=drawline(pchips,pmatm,continue->)
! fid=drawline(pchips,pmatsd,continue->)


;return
!;cluso:
plog=75
ppulp=25
pchip=120
psaw=220
call(tranc)

! saw0=100*saw/vtot;
! vol0=100*vol/vtot;
! totc0=100*totc/vtot;
! chip0=100*chip/vtot;
probmin0=problem()
pchip*totc+psaw*saw-plog*logv-ppulp*pulp==max
/

call(tranc)

jl9=jlp(unit->tree,data->lpdat,problem->probmin0,stop->Change%.lt.0.0001)

probmin=problem()
psaw*saw-plog*logv-ppulp*pulp==max
totc=51.5432755
/


pchip=0
call(tranc)
jl9=jlp(unit->tree,data->lpdat,problem->probmin,stop->Change%.lt.0.0001)
;clusf:
li=;list(jl9?);
jl9%xvar,jl9%vc,jl9%vx,jl9%shprice,jl9%xprice,jl9%xsum;

probs=problem()
jl9%xprice*jl9%xvar==max
/



les=matrix((55-40)/3+1)
lenles=len(les);
des=matrix(47-15+1)
lendes=len(des);
pmat=matrix(lendes,lenles)
pmat2=matrix(lendes,lenles)
nmat=matrix(lendes,lenles)
nmat=0.0001


dtops=list(dtops1...dtops7)
lens=list(lens1...lens7)
saws=list(saws1...saws7)
logvs=list(logvs1...logvs7)
chips=list(chips1...chips7)

dtopsm=matrix(7)
lensm=matrix(7)
sawsm=matrix(7)
logvsm=matrix(7)
chipsm=matrix(7)
!
!pulp=com-logv
!logv=chip+saw
!totc=chip+pulp=chip+com-logv
!ptotc*totc=ptotc*(chip+com-logv)=ptotc*(chip-logv)+
!psaw*saw-plog*logv-ppulp*(com-logv)
!!psaw*saw+(ppulp-plog)*logv-ppulp*com
!uti=psaw*saw+plog*logv+ppulp*pulp+ptotc*totc
psaw=jl9%xprice(1);
pdif=jl9%xprice(3)-jl9%xprice(2);
ptotc=jl9%xprice(4);

transp=trans()
ibas=0
ntot=0
do(iobs,1,nobs(lpdat))
getobs(lpdat,iobs)
dtopsm=dtops
lensm=lens
sawsm=saws
logvsm=logvs
chipsm=chips
!pause('<>')
do(ilog,1,nlog)
ntot=ntot+1
uti=psaw*sawsm(ilog)+pdif*logvsm(ilog)+ptotc*(chipsm(ilog)-logvsm(ilog))
leni=(lensm(ilog)-40)/3+1
dto=dtopsm(ilog)-14
pmat(dto,leni,sum->)=uti
pmat2(dto,leni,sum->)=uti*uti
nmat(dto,leni,sum->)=1

enddo
enddo
ntot;
pmat;
nmat;
pmatmean=pmat/.nmat;
pmatvar=pmat2/.nmat-pmatmean*.pmatmean;
pmatsd=sqrt(pmatvar);
nsum=sum(nmat,any->);
nmatp=100*nmat/nsum;
maxp=max(pmatmean,any->);
pmatmeanp=100*pmatmean/maxp;
pmatsdp=pmatsd/maxp;
/

call(transp)

;clustotb:
fi=0
les=matrix(do->(40,55,3));
lenles=len(les)
des=matrix(47-15+1,do->)+14
lendes=len(des);


;do(i,1,lenles)
fi=drawline(des,pmatmeanp(All,i),append->,show->0)

;enddo
show(fi)
fi2=0
;do(i,1,lendes)
fi2=drawline(les,pmatmeanp(i,All),append->,show->0)

;enddo
show(fi2)
;return



tranfi=trans() !like trana in ana7
saw=0
chip=0
logv=0
obj=0
totc=0
saw2=0
com=0
nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then
!iobs;
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(sum(pmatmean(U%dtops-14,(U%lens-40)/3+1),any->)))
!pause('<>')
!func->"Func",obj->(psaw*U%saw+pchip*U%totc-plog*U%logv-ppulp*U%pulp))
saw=saw+U%saw
chip=chip+U%chip
logv=vol+U%logv
obj=obj+U%obj
totc=totc+U%totc
com=com+U%com
nob=nob+1

endif
enddo
saw=100*saw/com;
logv=100*logv/com;
obj=100*obj/com;
totc=100*totc/com;
chip=100*chip/com;
/
close(17)
!$$=56
call(tranfi)

;return



pchip=119.967727
jl9=jlp(unit->tree,data->lpdat,problem->probmin0,stop->Change%.lt.0.0001)


;return


;clusd:
sawloss=matrix(lenpsaw,lenpchip)
chipgain=matrix(lenpsaw,lenpchip)
gain=matrix(lenpsaw,lenpchip)
loss=matrix(lenpsaw,lenpchip)

fi2=0
chipot=100-saw0;
labx=1.5
laby=6
labd=0.6
labdh=0.5
;do(il,1,lenpsaw)
!hi=100-resus(il,All);
!chi=resuc(il,All);
loss(il,All)=plog*(vol0-resuv(il,All))-ppulp*(vol0-resuv(il,All));
!gain0=plog*(vol0-resuv(il,All));
sawloss(il,All)=psaws(il)*(saw0-resus(il,All));
chipgain(il,All)=t(pchips)*.(resuc(il,All)-chipot);
gain(il,All)=loss(il,All)-sawloss(il,All)+chipgain(il,All);
fi2=drawline(0.01*loss(il,All)-il*0.08,0.01*gain(il,All),append->,show->0,color->colors(il),width->2)
!fi=drawline(labx,labx+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
!label->' P_{chip}= "pchips(il)""font"',append->,show->0)
fi2=drawline(labx,labx+labd,laby-il*labdh,laby-il*labdh,color->colors(il),width->2,append->,
label->' P_{saw}= "psaws(il)""font"',append->,show->0)
;if(il.eq.1);then
gain00=0.01*gain(3)
loss00=0.01*loss(3)
;endif
!fi=drawline(psaws,resus(il,All),append->,show->0,color->colors(il),width->2)
;enddo

fi2=drawline(loss00,loss00,gain00,loss00,width->2,color->Orange,show->0,append->)
fi2=drawline(loss00+0.04,2.1,
label->'Black hole"font"',show->0,append->)

fi2=drawline(0,6,0,6,append->,width->2,show->0,
xlabel->'Forest owner loss /m^3"font"',
ylabel->'Sawmill gain /m^3"font"')
show(fi2,continue->)
;return






;return
psaw=0
plog=0
ppulp=0
pchip=1
call(tranc)
jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)

psaw=0
plog=0
ppulp=-1
pchip=0
call(tranc)
jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)

psaw=0
plog=0
ppulp=0
pchip=1
call(tranc)
jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)

;return

plog=75
ppulp=25
pchip=120
psaw=220

call(tranc)


jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)

;return





;return
chip2= 51.54328;
jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)
;return
jl9=jlp(unit->tree,data->lpdat1,problem->probmin0,stop->Change%.lt.0.0001)

;return

psaw=220
 jl=jlp(unit->tree,data->lpdat1,problem->probmin0,stop->Change%.lt.0.0001)
pchip=116.34288
li=;list(jl%?);
jl%obj, jl%feasible, jl%coef, jl%nterminrow, jl%termvar, jl%vars, jl%rowofterm;
 jl%obj, jl%objects, jl%objtype, jl%unit, jl%nxinrow, jl%xvar;  ! jl%ibaunit;
 jl%ix ,jl%tree, jl%nlog, jl%saw, jl%chip, jl%pulp, jl%logv;
 jl%totc, jl%com ,jl%xsum;

maketr=trans()
ob0=wei*(psaw*saw-plog*logv-ppulp*pulp)
ob=psaw*saw-plog*logv-ppulp*pulp
obj2=wei*(pchip*totc+psaw*saw-plog*logv-ppulp*pulp)
/
weda=newdata(jl%weights,lpdat1,read->wei,maketrans->maketr)
stat(sum->)
obj2%sum;
!stat(data->weda,filter->(wei.gt.0.and.wei.lt.1))
probmin00=problem()

psaw*saw-plog*logv-ppulp*pulp==max

/
jl0=jlp(unit->tree,data->lpdat1,problem->probmin00,stop->Change%.lt.0.0001)
li0=;list(jl0%?);
! jl0%obj, jl0%feasible, jl0%coef, jl0%nterminrow, jl0%termvar, jl0%vars, jl0%rowofterm;
 ! jl0%obj, jl0%objects, jl0%objtype, jl0%unit, jl0%nxinrow, jl0%xvar;  ! jl0%ibaunit;
 ! jl0%ix ,jl0%tree, jl0%nlog, jl0%saw, jl0%chip, jl0%pulp, jl0%logv;
 ! jl0%totc, jl0%com ,jl0%xsum;
 
 weda0=newdata(jl0%weights,lpdat1,read->wei,maketrans->maketr)
stat(sum->)
obj2%sum;
!stat(data->weda0,filter->(wei.gt.0.and.wei.lt.1))


pchip=120
jl1=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)
weda1=newdata(jl0%weights,lpdat1,read->wei,maketrans->maketr)
stat(sum->)
obj2%sum;
pchip=116.34288
jl2=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)
weda2=newdata(jl0%weights,lpdat1,read->wei,maketrans->maketr)
stat(sum->)
obj2%sum;
;return
transc2=trans()
saw=0
chip=0
vol=0
obj=0
totc=0

nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(psaw*U%saw+pchip*U%totc-plog*u%logv-ppulp*U%pulp))
saw=saw+U%saw
chip=chip+U%chip
vol=vol+u%logv
obj=obj+U%obj
totc=totc+U%totc
nob=nob+1
!pause('<')

endif
enddo
/
call(transc2)
vol,totc,totc/vol;

;return
;clusb:
cpros=matrix(do->(40,80,2.5))
lepo=len(cpros)
resuvMAX=matrix(lepo)
resuvmax=matrix(lepo)

resuvMIN=matrix(lepo)
resuvmin=matrix(lepo)



stat(sum->)
;clusb:
!com%sum,saw%sum,chip%sum,pulp%sum;
!logv=saw%sum+chip%sum+pulp%sum;

probmax=problem()
logv==max
totc>chip2
/

probmin=problem()
logv==min
totc>chip2
/
probmin0=problem()
logv==min
totc=chip2
/


les=matrix((55-40)/3+1)
lenles=len(les);
des=matrix(47-15+1)
lendes=len(des);
pmat=matrix(lendes,lenles)
pmat2=matrix(lendes,lenles)
nmat=matrix(lendes,lenles)
nmat=0.0001

trand=trans() !like trana in ana7
saw=0
chip=0
vol=0
obj=0
totc=0
saw2=0
ntot=0
nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(Psaw*U%saw+Pchip*U%totc-plog*U%logv-ppulp*U%pulp))
!u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
!func->"Func",obj->(Psaw*u%saw+Pchip*u%totc-plog*u%logv-ppulp*u%pulp))

do(ilog,1,U%nlog)

ntot=ntot+1
uti=Psawdif*U%saws(ilog)-pvdif*U%logvs(ilog)
leni=(U%lens(ilog)-40)/3+1
dto=U%dtops(ilog)-14
pmat(dto,leni,sum->)=uti
pmat2(dto,leni,sum->)=uti*uti
nmat(dto,leni,sum->)=1


if(dto.eq.2.and.leni.eq.1)then
dto,leni,uti;
ilog,uti;
leni,dto;
sa=U%saws(ilog);
vo=U%logvs(ilog);
uti1=Psawdif*U%saws(ilog);
uti2=-pvdif*U%logvs(ilog);
utia=Psawdif*sa;
utib=-pvdif*vo;

Psawdif,pvdif;
U%lens;
U%dtops;

pause('<dh>')
endif
enddo
saw=saw+U%saw
chip=chip+U%chip
vol=vol+u%logv
obj=obj+U%obj
totc=totc+U%totc
nob=nob+1
!pause('<')
! if(any)then
! u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, any->,
! func->"Func",obj->(psaw*u%saw+pchip*u%totc-plog*u%logv-ppulp*u%pulp))
! !pause('<>')
! !print(u%vars,expand->)
! endif
endif
enddo
saw=100*saw/vtot;
vol=100*vol/vtot;
obj=100*obj/vtot;
totc=100*totc/vtot;
chip=100*chip/vtot;
pmatmean=pmat/.nmat;
pmatvar=pmat2/.nmat-pmatmean*.pmatmean;
pmatsd=sqrt(pmatvar);
nsum=sum(nmat,any->);
nmatp=100*nmat/nsum;
maxp=max(pmatmean,any->);
pmatmeanp=100*pmatmean/maxp;
pmatsdp=pmatsd/maxp;
/

call(trand)





;return

;clusmat:

tranmat=trans() !like trana in ana7
sawm=0
chipm=0
logvm=0
objm=0
totcm=0
saw2m=0

nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then
!leni=(lensm(ilog)-40)/3+1
!dto=dtopsm(ilog)-14
!pmat(dto,leni,sum->)=uti

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(sum(pmatmean(U%dtops-14,(U%lens-40)/3+1))))
!func->"Func",obj->(psaw*U%saw+pchip*U%totc-plog*u%logv-ppulp*U%pulp))
sawm=sawm+U%saw
chipm=chipm+U%chip
logvm=logvm+u%logv
objm=objm+U%obj
totcm=totcm+U%totc
nob=nob+1
!pause('<')
! if(any)then
! u=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, any->,
! func->"Func",obj->(psaw*u%saw+pchip*u%totc-plog*u%logv-ppulp*u%pulp))
! !pause('<>')
! !print(u%vars,expand->)
! endif
endif
enddo
sawm=100*sawm/vtot;
logvm=100*logvm/vtot;
objm=100*objm/vtot;
totcm=100*totcm/vtot;
chipm=100*chipm/vtot;
/
call(tranmat)


;return

;clustot:
Lmin=40
Lmax=55
Dmin=15
dmin=5

! lams=matrix(3,values->(20,30,40))
! colors=matrix(3,values->(Blue,Green,Red))
colors=matrix(3,values->(Blue,Green,Red))

tranc=trans() !like trana in ana7
saw=0
chip=0
logv=0
obj=0
totc=0
saw2=0
com=0
nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then

U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(psaw*U%saw+pchip*U%totc-plog*U%logv-ppulp*U%pulp))
saw=saw+U%saw
chip=chip+U%chip
logv=vol+U%logv
obj=obj+U%obj
totc=totc+U%totc
com=com+U%com
nob=nob+1

endif
enddo
saw=100*saw/com;
logv=100*logv/com;
obj=100*obj/com;
totc=100*totc/com;
chip=100*chip/com;
/

!;cluso:
plog=75
ppulp=25
pchip=120
psaw=220
call(tranc)

! saw0=100*saw/vtot;
! vol0=100*vol/vtot;
! totc0=100*totc/vtot;
! chip0=100*chip/vtot;
probmin0=problem()
pchip*totc+psaw*saw-plog*logv-ppulp*pulp==max
/

call(tranc)

jl9=jlp(unit->tree,data->lpdat,problem->probmin0,stop->Change%.lt.0.0001)

probmin=problem()
psaw*saw-plog*logv-ppulp*pulp==max
totc=51.5432755
/


pchip=0
call(tranc)
jl9=jlp(unit->tree,data->lpdat,problem->probmin,stop->Change%.lt.0.0001)
;clusf:
li=;list(jl9?);
jl9%xvar,jl9%vc,jl9%vx,jl9%shprice,jl9%xprice,jl9%xsum;

probs=problem()
jl9%xprice*jl9%xvar==max
/



les=matrix((55-40)/3+1)
lenles=len(les);
des=matrix(47-15+1)
lendes=len(des);
pmat=matrix(lendes,lenles)
pmat2=matrix(lendes,lenles)
nmat=matrix(lendes,lenles)
nmat=0.0001


dtops=list(dtops1...dtops7)
lens=list(lens1...lens7)
saws=list(saws1...saws7)
logvs=list(logvs1...logvs7)
chips=list(chips1...chips7)

dtopsm=matrix(7)
lensm=matrix(7)
sawsm=matrix(7)
logvsm=matrix(7)
chipsm=matrix(7)
!
!pulp=com-logv
!logv=chip+saw
!totc=chip+pulp=chip+com-logv
!ptotc*totc=ptotc*(chip+com-logv)=ptotc*(chip-logv)+
!psaw*saw-plog*logv-ppulp*(com-logv)
!!psaw*saw+(ppulp-plog)*logv-ppulp*com
!uti=psaw*saw+plog*logv+ppulp*pulp+ptotc*totc
psaw=jl9%xprice(1);
pdif=jl9%xprice(3)-jl9%xprice(2);
ptotc=jl9%xprice(4);

transp=trans()
ibas=0
ntot=0
do(iobs,1,nobs(lpdat))
getobs(lpdat,iobs)
dtopsm=dtops
lensm=lens
sawsm=saws
logvsm=logvs
chipsm=chips
!pause('<>')
do(ilog,1,nlog)
ntot=ntot+1
uti=psaw*sawsm(ilog)+pdif*logvsm(ilog)+ptotc*(chipsm(ilog)-logvsm(ilog))
leni=(lensm(ilog)-40)/3+1
dto=dtopsm(ilog)-14
pmat(dto,leni,sum->)=uti
pmat2(dto,leni,sum->)=uti*uti
nmat(dto,leni,sum->)=1

enddo
enddo
ntot;
pmat;
nmat;
pmatmean=pmat/.nmat;
pmatvar=pmat2/.nmat-pmatmean*.pmatmean;
pmatsd=sqrt(pmatvar);
nsum=sum(nmat,any->);
nmatp=100*nmat/nsum;
maxp=max(pmatmean,any->);
pmatmeanp=100*pmatmean/maxp;
pmatsdp=pmatsd/maxp;
/

call(transp)

;clustotb:
fi=0
les=matrix(do->(40,55,3));
lenles=len(les)
des=matrix(47-15+1,do->)+14
lendes=len(des);


;do(i,1,lenles)
fi=drawline(des,pmatmeanp(All,i),append->,show->0)

;enddo
show(fi)
fi2=0
;do(i,1,lendes)
fi2=drawline(les,pmatmeanp(i,All),append->,show->0)

;enddo
show(fi2)
;return



tranfi=trans() !like trana in ana7
saw=0
chip=0
logv=0
obj=0
totc=0
saw2=0
com=0
nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;

if(pinedee(iobs,40).ge.15)then
!iobs;
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(sum(pmatmean(U%dtops-14,(U%lens-40)/3+1),any->)))
!pause('<>')
!func->"Func",obj->(psaw*U%saw+pchip*U%totc-plog*U%logv-ppulp*U%pulp))
saw=saw+U%saw
chip=chip+U%chip
logv=vol+U%logv
obj=obj+U%obj
totc=totc+U%totc
com=com+U%com
nob=nob+1

endif
enddo
saw=100*saw/com;
logv=100*logv/com;
obj=100*obj/com;
totc=100*totc/com;
chip=100*chip/com;
/
close(17)
!$$=56
call(tranfi)

;return



pchip=119.967727
jl9=jlp(unit->tree,data->lpdat,problem->probmin0,stop->Change%.lt.0.0001)


;return


;clusd:
sawloss=matrix(lenpsaw,lenpchip)
chipgain=matrix(lenpsaw,lenpchip)
gain=matrix(lenpsaw,lenpchip)
loss=matrix(lenpsaw,lenpchip)

fi2=0
chipot=100-saw0;
labx=1.5
laby=6
labd=0.6
labdh=0.5
;do(il,1,lenpsaw)
!hi=100-resus(il,All);
!chi=resuc(il,All);
loss(il,All)=plog*(vol0-resuv(il,All))-ppulp*(vol0-resuv(il,All));
!gain0=plog*(vol0-resuv(il,All));
sawloss(il,All)=psaws(il)*(saw0-resus(il,All));
chipgain(il,All)=t(pchips)*.(resuc(il,All)-chipot);
gain(il,All)=loss(il,All)-sawloss(il,All)+chipgain(il,All);
fi2=drawline(0.01*loss(il,All)-il*0.08,0.01*gain(il,All),append->,show->0,color->colors(il),width->2)
!fi=drawline(labx,labx+labd,labelh(il),labelh(il),color->colors(il),width->2,append->,
!label->' P_{chip}= "pchips(il)""font"',append->,show->0)
fi2=drawline(labx,labx+labd,laby-il*labdh,laby-il*labdh,color->colors(il),width->2,append->,
label->' P_{saw}= "psaws(il)""font"',append->,show->0)
;if(il.eq.1);then
gain00=0.01*gain(3)
loss00=0.01*loss(3)
;endif
!fi=drawline(psaws,resus(il,All),append->,show->0,color->colors(il),width->2)
;enddo

fi2=drawline(loss00,loss00,gain00,loss00,width->2,color->Orange,show->0,append->)
fi2=drawline(loss00+0.04,2.1,
label->'Black hole"font"',show->0,append->)

fi2=drawline(0,6,0,6,append->,width->2,show->0,
xlabel->'Forest owner loss /m^3"font"',
ylabel->'Sawmill gain /m^3"font"')
show(fi2,continue->)
;return






;return
psaw=0
plog=0
ppulp=0
pchip=1
call(tranc)
jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)

psaw=0
plog=0
ppulp=-1
pchip=0
call(tranc)
jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)

psaw=0
plog=0
ppulp=0
pchip=1
call(tranc)
jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)

;return

plog=75
ppulp=25
pchip=120
psaw=220

call(tranc)


jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)

;return





;return
chip2= 51.54328;
jl9=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)
;return
jl9=jlp(unit->tree,data->lpdat1,problem->probmin0,stop->Change%.lt.0.0001)

;return

psaw=220
 jl=jlp(unit->tree,data->lpdat1,problem->probmin0,stop->Change%.lt.0.0001)
pchip=116.34288
li=;list(jl%?);
jl%obj, jl%feasible, jl%coef, jl%nterminrow, jl%termvar, jl%vars, jl%rowofterm;
 jl%obj, jl%objects, jl%objtype, jl%unit, jl%nxinrow, jl%xvar;  ! jl%ibaunit;
 jl%ix ,jl%tree, jl%nlog, jl%saw, jl%chip, jl%pulp, jl%logv;
 jl%totc, jl%com ,jl%xsum;

maketr=trans()
ob0=wei*(psaw*saw-plog*logv-ppulp*pulp)
ob=psaw*saw-plog*logv-ppulp*pulp
obj2=wei*(pchip*totc+psaw*saw-plog*logv-ppulp*pulp)
/
weda=newdata(jl%weights,lpdat1,read->wei,maketrans->maketr)
stat(sum->)
obj2%sum;
!stat(data->weda,filter->(wei.gt.0.and.wei.lt.1))
probmin00=problem()

psaw*saw-plog*logv-ppulp*pulp==max

/
jl0=jlp(unit->tree,data->lpdat1,problem->probmin00,stop->Change%.lt.0.0001)
li0=;list(jl0%?);
! jl0%obj, jl0%feasible, jl0%coef, jl0%nterminrow, jl0%termvar, jl0%vars, jl0%rowofterm;
 ! jl0%obj, jl0%objects, jl0%objtype, jl0%unit, jl0%nxinrow, jl0%xvar;  ! jl0%ibaunit;
 ! jl0%ix ,jl0%tree, jl0%nlog, jl0%saw, jl0%chip, jl0%pulp, jl0%logv;
 ! jl0%totc, jl0%com ,jl0%xsum;
 
 weda0=newdata(jl0%weights,lpdat1,read->wei,maketrans->maketr)
stat(sum->)
obj2%sum;
!stat(data->weda0,filter->(wei.gt.0.and.wei.lt.1))


pchip=120
jl1=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)
weda1=newdata(jl0%weights,lpdat1,read->wei,maketrans->maketr)
stat(sum->)
obj2%sum;
pchip=116.34288
jl2=jlp(unit->tree,data->lpdat1,problem->probmin01,stop->Change%.lt.0.0001)
weda2=newdata(jl0%weights,lpdat1,read->wei,maketrans->maketr)
stat(sum->)
obj2%sum;
;return
transc2=trans()
saw=0
chip=0
vol=0
obj=0
totc=0

nob=0
do(iobs,1,nrows(pinedee))
!if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedee(iobs,40).ge.15)then
U=stemopt(Lmin,Lmax,Dmin,dmin,pinedee,iobs, 
func->"Func",obj->(psaw*U%saw+pchip*U%totc-plog*u%logv-ppulp*U%pulp))
saw=saw+U%saw
chip=chip+U%chip
vol=vol+u%logv
obj=obj+U%obj
totc=totc+U%totc
nob=nob+1
!pause('<')

endif
enddo
/
call(transc2)
vol,totc,totc/vol;

;return
;clusb:
cpros=matrix(do->(40,80,2.5))
lepo=len(cpros)
resuvMAX=matrix(lepo)
resuvmax=matrix(lepo)

resuvMIN=matrix(lepo)
resuvmin=matrix(lepo)



stat(sum->)
;clusb:
!com%sum,saw%sum,chip%sum,pulp%sum;
!logv=saw%sum+chip%sum+pulp%sum;

probmax=problem()
logv==max
totc>chip2
/

probmin=problem()
logv==min
totc>chip2
/
probmin0=problem()
logv==min
totc=chip2
/

!lpb
;do(ic,1,lepo)
chip2=cpros(ic)  !resuc(ic);
jl=jlp(unit->tree,data->lpdat1,problem->probmax)
!ili=;list(jl%?);
!@ili;
jl%saw,jl%chip,jl%pulp,jl%totc,jl%com;
!resusmax(ic)=jl%saw
if(jl%feasible)resuvmax(ic)=jl%logv
jl=jlp(unit->tree,data->lpdat1,problem->probmin)
!resusmin(ic)=jl%saw
!pause('<>')
if(jl%feasible)resuvmin(ic)=jl%logv
jl=jlp(unit->tree,data->lpdat,problem->probmax)

if(jl%feasible)resuvMAX(ic)=jl%logv
jl=jlp(unit->tree,data->lpdat,problem->probmin0)

if(jl%feasible)resuvMIN(ic)=jl%logv

;enddo
;clusc:
resuvmax,resuvmin;

fig=drawline(cpros,resuvmax,show->0,color->Red,width->2)
fig=drawline(cpros,resuvmin,append->,show->0,color->Blue,width->2)
fig=drawline(cpros,resuvMAX,append->,show->0,color->Green,width->2)
fig=drawline(cpros,resuvMIN,append->,continue->,color->Cyan,width->2)

;return

;set:

sets=data(read->(set,D1_set,D2_set),in->,case->,sparse->)
set1,15,17,d50x100,2,d22x100,2
set2,15,17,d100x100,1,d22x100,2
set3,16,18,d100x100,1,d22x100,2
set3,16,18,d100x100,1,d22x100,2
set3,17,19,d100x100,1,d22x100,2
set3,17,19,d100x100,1,d22x100,2

set3,18,19,d50x75,4,d20x100,2
set4,18,19,d75x75,2,d22x100,2
/

;harvest:
;if(type(pinetoppara).ne.MATRIX)lukeharv
;if(type(pinedeeh).ne.MATRIX)rdataharv
Lmin=40
Lmax=55
Dmin=15
dmin=5
hoo=matrix(350,do->)
trani0=trans() !like trana in ana7
! delete_o(saw)luke
! chip=0
! logv=0
! obj=0
delete_o(saw0)
saw0;
chip0=0
vol0=0
objb0=0
vtot=0
do(iobs,1,nrows(pinedeeh))
if((500*int(iobs/500)).eq.iobs)iobs;
if(pinedeeh(iobs,40).ge.15)then

! pinetoppara;
u=stemopt(Lmin,Lmax,Dmin,dmin,pinedeeh,iobs,
func->"Func",obj->u%saw,par->pinetoppara)   !,any->)

vtot=vtot+u%com
!u%totc,u%chip,u%pulp,u%chip+u%pulp
!pause('<>')
saw0=saw0+u%saw
chip0=chip0+u%chip
vol0=vol0+u%logv
obj0=obj0+u%obj
!pause('<gg>')
endif

enddo
/

call(trani0)
saw0;

;return





! Vee=100.*Vee/vtot;
 ! Saaw=100*Saaw/vtot;
 ! Saawu=100*Saawu/vtot;
! Stump=Stump/vtot;





! https://stackoverflow.com/questions/63850032/gnuplot-multiplot-equal-height-of-each-plot
! ### multiplot with equal plot heights
! reset session

! myBottomMargin = 0.15
! myPlotHeight = (1. - myBottomMargin)/3

! set multiplot

    ! set xlabel "This is the xlabel for all plots"
    ! set ytics 0.5
    ! set grid xtics, ytics
    ! set origin 0, myBottomMargin
    ! set size 1, myPlotHeight
    ! set bmargin 0
    ! set label 1 at graph 1.0, graph 1.0 "185.72 keV" right offset -1,-1
    ! plot sin(x) w l lc 1 notitle
    
    ! unset xlabel
    ! set format x ""
    ! set origin 0, myBottomMargin+myPlotHeight
    ! set label 1 at graph 1.0, graph 1.0 "351.03 keV" right offset -1,-1
    ! plot sin(2*x) w l lc 2 notitle
    
    ! set origin 0, myBottomMargin+2*myPlotHeight
    ! set label 1 at graph 1.0, graph 1.0 "831.98 keV" right offset -1,-1
    ! plot sin(3*x) w l lc 3 notitle

! unset multiplot
! ### end of code