exfile=thisfile()
ask(wait,q->'pause after each example(1/0)>')
ask(fcont,q->'continue after each figure(1/0)>')
current=';incl(exfile,from->current)'
inpuparag=';incl(exfile,from->inpuparag)'
shortex=';incl(exfile,from->shortex)'
dotsex=';incl(exfile,from->dotsex)'
jfuncex=';incl(exfile,from->jfuncex)'
codeoptex=';incl(exfile,from->codeoptex)'
comoptex=';incl(exfile,from->comoptex)'
recursion=';incl(exfile,from->recursion)'
transfunc=';incl(exfile,from->transfunc)'
matrixop=';incl(exfile,from->matrixop)'
doex=';incl(exfile,from->doex)'
retex=';incl(exfile,from->retex)'
errexitex=';incl(exfile,from->errexitex)'
gotoex=';incl(exfile,from->gotoex)'
assignex=';incl(exfile,from->assignex)'
operexr=';incl(exfile,from->operexr)'
inpuincl=';incl(exfile,from->inpuincl)'
inpugotoex=';incl(exfile,from->inpugotoex)'
inpudoex=';incl(exfile,from->inpudoex)'
inpusumex=';incl(exfile,from->inpusumex)'
getset=';incl(exfile,from->getset)'
list2ex=';incl(exfile,from->list2ex)'
ilistex=';incl(exfile,from->ilistex)'
typeex=';incl(exfile,from->typeex)'
whichex=';incl(exfile,from->whichex)'
findex=';incl(exfile,from->findex)'
newdataex=';incl(exfile,from->newdataex)'
newclassdata=';incl(exfile,from->newclassdata)'
addingclass=';incl(exfile,from->addingclass)'
logisticex=';incl(exfile,from->logisticex)'
showex=';incl(exfile,from->showex)'
plot3dex=';incl(exfile,from->plot3dex)'
ranex=';incl(exfile,from->ranex)'
selectex=';incl(exfile,from->selectex)'
rannex=';incl(exfile,from->rannex)'
ranbinex=';incl(exfile,from->ranbinex)'
cpuex=';incl(exfile,from->cpuex)'
secondsex=';incl(exfile,from->secondsex)'
drawex=';incl(exfile,from->drawex)'
drawlineex=';incl(exfile,from->drawlineex)'
drawclassex=';incl(exfile,from->drawclassex)'
plotyxex=';incl(exfile,from->plotyxex)'
askex=';incl(exfile,from->askex)'
mergex=';incl(exfile,from->mergex)'
dataex=';incl(exfile,from->dataex)'
statex=';incl(exfile,from->statex)'
covex=';incl(exfile,from->covex)'
matrixex=';incl(exfile,from->matrixex)'
transex=';incl(exfile,from->transex)'
list2ex=';incl(exfile,from->list2ex)'
Rex=';incl(exfile,from->Rex)'
diffex=';incl(exfile,from->diffex)'
derex=';incl(exfile,from->derex)'
derex2=';incl(exfile,from->derex2)'
densityex=';incl(exfile,from->densityex)'
AGAIN=';incl(exfile)'
ALL=';incl(exfile,from->ALL)'
;return
 
;inpuparag:  
! Example of inputparagraph (line   1153 file jsections.txt)
tr=trans()
a=log(b)
write($,'(~sinlog is=~,f4.0)',sin(a))
/
b=matrix(2,3,in->)
1,2,3
5,6,7
/
;if(wait);pause
;return
 
;shortex:  
! Example of using shortcuts and include files (line   1171 file jsections.txt)
short1='sin(Pi)+cos(Pi);'
short1
te=text()
this=thisfile()
ju1=';incl(this,from->a1)'
ju2=';incl(this,from->a2)'
;return
;a1:
'greetings from a1'
;return
;a2:
'here, jump to a1';
ju1
'back here, return to sit>'
;return
//
write('shortex.txt',$,te)
;incl(shortex.txt)
ju1
ju2
delete_f('shortext.txt')
te=0 !delete also text object te
;if(wait);pause
;return
 
;dotsex:  
! Example of dots construct (line   1301 file jsections.txt)
dat=data(read->(x4...x7),in->)
1,2,3,4
11,12,13,14
/
stat(min->,max->,mean->)
x3%mean...x7%mean;
A...D=4...1;
Continue=1  !demo of error in data()
dat=data(read->(x3...x7),in->)
1,2,3,4
11,12,13,14
/
Continue=0
;if(wait);pause
;return
 
;jfuncex:  
! Examples of J-functions (line   1473 file jsections.txt)
a = sin(cos(c)+b) ! Usual arithmetic functions have numeric values as arguments
! here the value of the argument of cos is obtained by 'computing' the value of real variable c.
Dm=matrix(do->(0.1,40))
nob=nrows(Dm)
e=matrix(nob)
e=rann()
Hm=0.5+Dm**0.7+e
dat=newdata(Dm,Hm,read->(D,H))
stat(D,H,min->,max->) ! Here arguments must be variable names
plotyx(H,D) !arguments of the function are variables
h=matrix(5,5);
h=rann();
g=matrix(5,do->5);
c = inverse(h+t(g)); ! The argument can be intermediate result from matrix computations.
;if(wait);pause
;return
 
;codeoptex:  
! Codeoptions (line   1499 file jsections.txt)
dat=data(read->(D,H),in->)
3,2
2,4
4,1
/
stat(D,H,filter->(H.gt.D)) ! only those observations are accepted which pass the filter
fi=draw(func->(sin($x)+1),x->$x,xrange->(0,10),color->Red,ylabel->'sin(x)+1',xlabel->'x',width->2) ! the func-> option transmits the function to be drawn not a single value.
;if(wait);pause
;return
 
;comoptex:  
! data1 (line   1564 file jsections.txt)
dat=data(read->(x,y),in->)
1,2
3,4
5,6
/
tr=trans()
xy=x*y
x,y,xy;
/
stat(trans->tr)
;if(wait);pause
;return
 
;recursion:  
! Recursion produces system crash. (line   1603 file jsections.txt)
tr=trans() !level will be initialized as zero
level;
level=level+1
call(tr)
/
Continue=1  !error is produced
call(tr)
Continue=0
;if(wait);pause
;return
 
;transfunc:  
! Transformation as a function (line   1626 file jsections.txt)
delete_o(a,c)
tra=trans()
a=8;
c=2;
/
trb=trans()
a=5;
c=1;
/
c=2
a=c+trb(a)+c+tra(a);
;if(wait);pause
;return
 
;matrixop:  
! Matrix operations (line   1794 file jsections.txt)
A=matrix(3,4,do->);
A+2;
A-1;
2*A;
A*.matrix(3,4,do->2);
A/.A;
b=matrix(4,values->(1,3,6,8));
A*b;
Continue=1  !Error
A*.b;
A+b(1,-3);  ! NONSTANDARD
B=matrix(3,4,values->2);
A;
A.eq.B;
li=list(x1...x3)
li=4,5,7;
A+li;
li=matrix(3,do->);
@li;
A+li;
b*A;
A+t(b);
li=matrix(4,do->);
Continue=0
;if(wait);pause
;return
 
;doex:  
! do-loop (line   1869 file jsections.txt)
'begin';
tr=trans()
do(i,1,5)
i;
ad1: if(i.eq.3)goto(cycle)
i;
if(i.eq.4)goto(jump)
cycle:enddo
jump:i;
!goto(ad1)  ! it is not allowed to jump into a loop
/
call(tr)
;if(wait);pause
;return
 
;retex:  
! example of return and goto () (line   1890 file jsections.txt)
tr=trans()
ad1:r=ran();
if(r.lt.0.2)return
goto(ad1)
/
call(tr)
;if(wait);pause
;return
 
;errexitex:  
! (line   1910 file jsections.txt)
tr=trans()
if(a.eq.0)errexit('illegal value ',a)
s=3/a; ! division with zero is teste automatically
/
a=3.7
call(tr)
tr(s); !tr can also be used as a function
a=0
Continue=1  !Do not stop in thsi seflmade error
call(tr)
Continue=0
;if(wait);pause
;return
 
;gotoex:  
! (line   1929 file jsections.txt)
tr=trans()
i=0
if(i.eq.0)goto(koe)
'here';
koe:ch='here2';
/
call(tr)
ch;
;if(wait);pause
;return
 
;assignex:  
! Examples of assignments (line   1980 file jsections.txt)
a=matrix(2,3);
a=4;
a=rann();
v1...5=2...6;
v1...5=77;
Continue=1  ! ERROR
v1..3=1,5;
Continue=0
;if(wait);pause
;return
 
;operexr:  
! Exapmple of operation of J. (line   2097 file jsections.txt)
;if(wait);pause
;return
 
;inpuincl:  
! Example of ;incl() (line   9380 file c:/j3/jutilities.f90)
file=text()
i=1;
goto(ad1)
i=2;
ad1:i=66;
goto(ad2,ad3,2)  !select label from a label list
ad2:
i=3;
ad3:i=4;
goto(5)  !select label from the list of all labels
ad4:i=5;
ad5:i=6;
//
write('file.txt',file)
close('file.txt')
;incl(file.txt)
;incl(file.txt,from->ad2)
;if(wait);pause
;return
 
;inpugotoex:  
! Example of ;goto() and ;incl() (line   9410 file c:/j3/jutilities.f90)
gototxt=text()
'Start jumping';
;goto(ad2)
;ad1:
'Greetings from ad1';
;return
;ad2:
'Greetings from ad2';
;goto(ad1)
//
print(gototxt)
if(exist_f('goto.txt')delete_f('goto.txt')
write('goto.txt',gototxt)
close('goto.txt')
print('goto.txt')
;incl(goto.txt)
;incl(goto.txt,from->ad1)
delete_f('goto.txt')
;if(wait);pause
;return
 
;inpudoex:  
! Examples of ;do() (line   9781 file c:/j3/jutilities.f90)
;do(i,1,2)
x"i"="i"*10
print('Greetings from iteration "i"')
;enddo
print(x1,x2)
;if(wait);pause
;return
 
;inpusumex:  
! Example of ;sum() (line  10101 file c:/j3/jutilities.f90)
su=';sum(i,1,5)(a"i"*x")'
print(su)
prob=problem()
;sum(i,1,5)(a"i"*x"i")==max
;if(wait);pause
;return
 
;getset:  
! Get or set submatrices (line   1919 file c:/j3/j.f90)
A=matrix(3,4,do->);
B=A(1,-2,3,-4);
A(1,-2,3,-4)=B+3;
A(1,-2,3,-4,sum->)=-5;
A(1,-2,3,-4,sum->2)=A(2,-3,1,-2,sum->2);
C=A({1,3},{4...2});
H=matrix(4,4,diag->,do->3);
H(3,-4,diag->)=matrix(2,values->(4,7));
;if(wait);pause
;return
 
;list2ex:  
! (line   3146 file c:/j3/j.f90)
lis2=list2(x1...x4,3,5);
li1=list(x1,x2);
li2=list(z,y);
lis=list2(li1,li2,77);
;if(wait);pause
;return
 
;ilistex:  
! ILIST examples (line   3192 file c:/j3/j.f90)
{1,4,5};
{4...1};
A=matrix(4,4)
A({1,5},{3}=
;if(wait);pause
;return
 
;typeex:  
! Example of type (line   3310 file c:/j3/j.f90)
ttt=8;   !REAL
type(ttt);
type('ttt'); !type is CHAR
type('ttt',content->);
cttt='ttt'
type(cttt);
type(cttt,content->);
;if(wait);pause
;return
 
;whichex:  
! Example of which() (line   3605 file c:/j3/j.f90)
c=9
which(a.eg.3.or.c.gt.8,5,a.eq.7,55);
a=7
which(a.eg.3.or.c.gt.8,5,a.eq.7,55);
a=5
which(a.eg.3.or.c.gt.8,5,a.eq.7,55);
which(a.eg.3.or.c.gt.8,5,a.eq.7,55,108);
;if(wait);pause
;return
 
;findex:  
! Example of find, illustrating also rann() (line   6388 file c:/j3/j.f90)
!Repeating the example, different results will be obtained
rm=matrix(100)
m,s=2,3
rm=rann(m,s)
print(mean(rm),sd(rm),min(rm),max(rm))
first=find(rm,filter->($.ge.m+1.96*s))
large=find(rm,filter->($.ge.m+1.96*s),any->)
large2=find(rm,filter->($.ge.m+1.96*s),expand->)
print(first,100*nrows(large)/nrows(rm),100*sum(large2)/nrows(rm))
;if(wait);pause
;return
 
;newdataex:  
! newdata() generates a new data object. (line   6799 file c:/j3/j.f90)
data1=data(read->(x1...x3),in->)
1,2,3
4,5,6
7,8,9
/
matrix1=matrix(3,2,in->)
10,20
30,40
50,60
/
newtr=trans()
;do(i,1,3)
;do(j,1,2)
x"i"#z"j"=x"i"*z"j"
;enddo
;enddo
/
new=newdata(data1,matrix1,read->(z1,z2),maketrans->newtr)
print(new)
;if(wait);pause
;return
 
;newclassdata:  
! Making class level data object (line   7467 file c:/j3/j.f90)
classvector(x1,x2,class->stand,data->treedata,mean->,min->)
standdata=newdata(x1#stand%mean,x2#stand%mean,x1#stand%min,x2#stand%min,
read->(x1,x2,x1min,x2min))
;if(wait);pause
;return
 
;addingclass:  
! Adding class means and deviations from class means (line   7472 file c:/j3/j.f90)
classvector(x1,x2,class->stand,data->treedata,mean->,expand->)
tr=trans()
relx1=x1-x1mean
relx2=x2-x2mean
/
treedata=newdata(treedata,x1#stand%mean,x2#stand%mean,read->(x1mean,x2mean),
maketrans->tr)
;if(wait);pause
;return
 
;logisticex:  
! Example of logistic function (line   8210 file c:/j3/j.f90)
tr=trans()
der(a,x0)
f=logistic(a*(x-x0));
/
x,x0,a=10,5,0.1
d[a],d[x],tr(d[x0]);
;if(wait);pause
;return
 
;showex:  
! Example of show() (line   9760 file c:/j3/j.f90)
fi=draw(func->sqrt2(x),x->x,xrange->(-50,50),continue->fcont)
show(fi,xrange->(-60,60),xlabel->'NEWX',ylabel->'NEWY')
show(fi,axes->10)
show(fi,axes->01)
show(fi(axes->00)
Window='400,800'
show(fi)
Window='700,700'
fi=drawline(1,10,3,1,color->Red,continue->fcont)
show(fi,xrange->(1.1,11)) !the line is not visible
dat=data(read->(x,y),in->)
1,4
2,6
3,2
5,1
/
stat()
fi=plotyx(y,x,continue->fcont)  ! Gnuplot hides points at border
show(fi,xrange->(0,6),yrange->(0,7))
;if(wait);pause
;return
 
;plot3dex:  
! plot3d() example see p.  328 in Mehtatalo & Lappi 2020 (line   9818 file c:/j3/j.f90)
mat=matrix(1000000,3)
mat2=matrix(1000000,3)
tr=trans() !second order response surface
x=0
x2=0
xy=0
irow=1
do(ix,1,1000)
y=0
y2=0
xy=0
do(iy,1,1000)
mat(irow,1)=x
mat(irow,2)=y
mat(irow,3)=12+8*x-7*x2+124*y+8*xy-13*y2
mat2(irow,1)=x
mat2(irow,2)=y
mat2(irow,3)=50+160*x-5*x2-40*y-20*xy+10*y2
irow=irow+1
y=y+0.01
y2=y*y
xy=x*y
enddo
x=x+0.01
x2=x*x
enddo
/
call(tr)
fi=plot3d(mat,sorted->)
fi=plot3d(mat2,sorted->)
;if(wait);pause
;return
 
;ranex:  
! (line  12129 file c:/j3/j.f90)
ran();
ran();
cpu0=cpu()
A=matrix(10000,5)
A=ran()
mean(A);
mean(A,any->) !mean over all elements
mean(A(All,2));
sd(A);
sd(A,any->);
min(A);
min(A,any->);
max(A);
cpu()-cpu0;
;if(wait);pause
;return
 
;selectex:  
! Random selection (line  12184 file c:/j3/j.f90)
S=select(500,10000)
mean(S),sum(S),500/10000;
;if(wait);pause
;return
 
;rannex:  
! Random normal variates, illustrating also find (line  12352 file c:/j3/j.f90)
rx=rann()  !Output is REAL
rm=matrix(100)
print(mean(rm),sd(rm),min(rm),max(rm))
large=find(rm,filter->($.ge.2),any)
print(100*nrows(large)/nrows(rm))
cpu0=cpu()
rm2=matrix(1000000)
rm2=rann(10,2)  !there cannot be arithmetix opreations in the right side
cpu()-cpu0;
mean(rm2),sd(rm2),min(rm2),max(rm2);
large=find(rm,filter->($.ge.14),any->)
print(100*nrows(large)/nrows(rm))
!
;if(wait);pause
;return
 
;ranbinex:  
! (line  12458 file c:/j3/j.f90)
ranbin(10,0.1);
ranbin(10,0.1);
!
A=matrix(1000,2)
A(All,1)=ranbin(20,0.2)
A(All,2)=ranbin(20,0.2)
da=newdata(A,read->(s1,s2))
stat()
cl=classify(1,x->s1)
fi=drawclass(cl,histogram->,color->Blue,continue->fcont)
cl=classify(1,x->s2)
fi=drawclass(cl,histogram->,color->Red,append->,continue->fcont)
;if(wait);pause
;return
 
;cpuex:  
! Example of cpu-timing (line  12527 file c:/j3/j.f90)
cpu0=cpu()
a=matrix(100000)
a=ran() !uniform
mean(a),sd(a),min(a),max(a);
cpu1=cpu()
elapsed=cpu1-cpu0;
;if(wait);pause
;return
 
;secondsex:  
! Example of elapsed time (line  12555 file c:/j3/j.f90)
cpu0=cpu()
sec0=seconds()
a=matrix(100000)
a=ran() !uniform
mean(a),sd(a),min(a),max(a);
cpu1=cpu()
sec1=seconds()
elapsed=cpu1-cpu0;
selapsed=sec1-sec0;
;if(wait);pause
;return
 
;drawex:  
! Example of draw() (line  13424 file c:/j3/j.f90)
fi=draw(func->sin(x),xrange->(0,2*Pi),color->Blue,continue->fcont)
fi=draw(func->cos(x),xrange->(0,2*Pi),color->Red,append->,continue->fcont)
if(type(figyx).ne.FIGURE)plotyxex
show(figyx,cont->fcont)
reg0=regr(y,x)
stat(data->datyx,min->,max->)
figyx=draw(func->reg0(),x->x,xrange->,color->Violet,append->,continue->fcont)
tr=trans()
x2=x*x
fu=reg2()
/
reg2=regr(y,x,x2,data->datyx,trans->tr)
figyx=draw(func->tr(fu),xrange->,color->Orange,append->,continue->fcont)
Continue=1  !Errors
fi=draw(func->sin(x),x->x)
fi=draw(xrange->(1,100),func->Sin(x),x->x)
Continue=0
;if(wait);pause
;return
 
;drawlineex:  
! Example of drawline() (line  13614 file c:/j3/j.f90)
fi=draw(func->sin(x),xrange->(0,2*Pi),color->Blue,continue->fcont)
fi=drawline(Pi,sin(Pi)+0.1,label->'sin()',append->,continue->fcont)
xval=matrix(do->(1,100))
mat=matrix(values->(xval,xval+1,xval,xval+2,xval,xval+3))
fi=drawline(mat,color->Red)
fi=drawline(mat,break,color->Orange,break->,continue->fcont)
x=matrix(do->(0,100,1)
e=matrix(101)
e=rann()
y=2*x+0.4+e
da=newdata(x,y,read->(x,y))
reg=regr(y,x)
if(type(figyx).ne.FIGURE)plotyxex
show(figyx)
reg0=regr(y,x)
stat(data->datyx,min->,max->)
figyx=draw(func->reg0(),x->x,xrange->,color->Violet,append->,continue->fcont)
tr=trans()
x2=x*x
fu=reg2()
/
reg2=regr(y,x,x2,data->datyx,trans->tr)
figyx=draw(func->tr(fu),xrange->,color->Orange,append->,continue->fcont)
Continue=1  !Errors
fi=draw(func->sin(x),x->x)
fi=draw(xrange->(1,100),func->Sin(x),x->x)
Continue=0
;if(wait);pause
;return
 
;drawclassex:  
! Examples of drawclass() (line  14044 file c:/j3/j.f90)
X=matrix(do->(1,100,0.1))
e=matrix(nrows(X))
e=rann()
X2=0.01*x*.x !elementwise product
Y=2*x+0.01*X2+(1+0.3*X)*.e  !nonequal error variance,quadratic function
dat=newdata(x,y,x2,read->(x,y,x2),extra->(Regf,Resid))
stat(min->,max->)
reg=regr(y,x) ! Regf and resid are put into the data
fi=plotyx(y,x,continue->fcont)
fi=drawline(x%min,x%max,reg(x%min),reg(x%max),width->3,color->Cyan,append->,continue->fcont)
cl=classify(Resid,x->x,xrange->,classes->5)
fi=drawclass(cl,color->Blue,continue->fcont)
fi=drawclass(cl,se->,continue->fcont)
fi=drawclass(cl,sd->,continue->fcont)
fi=drawclass(cl,var->,continue->fcont)
fi=drawclass(cl,histo->,area->,continue->fcont)
fi=draw(func->pdf(0,rmse(reg)),x->x,xrange->,append->,continue->fcont) ! xrange comes from stat()
;if(wait);pause
;return
 
;plotyxex:  
! plotyx() (line  14263 file c:/j3/j.f90)
xmat=matrix(do->(0,10,0.001))
tr=trans()
y=2+3*x+0.4*x*x+4*rann()
/
da=newdata(xmat,read->x,maketrans->tr,extra->(Regf,Resid))
fi=plotyx(y,x,continue->fcont)
fi=plotyx(x,func->tr(y),mark->3,color->Orange,continue->fcont)
reg=regr(y,x)
fi=plotyx(y,x,show->0)
fi=plotyx(Regf,x,append->,continue->fcont)
fir=plotyx(Resid,x,continue->fcont)
;if(wait);pause
;return
 
;askex:  
! Examples for ask() (line  14753 file c:/j3/j.f90)
a=ask(default->8)
ask(a,default->8)
print(ask()+ask()) ! ask without argument is a numeric function
ask(v,q->'Give v>')
;if(wait);pause
;return
 
;mergex:  
! Merging list (line  16091 file c:/j3/j.f90)
x1...x3=1,2,3
mat=matrix(3,values->(4,5,6))
lis0=list(x2,x1)
lis2=merge(x1,mat,lis0)
print(lis2)
;if(wait);pause
;return
 
;dataex:  
! data() generates a new data object by reading data. (line  17011 file c:/j3/j.f90)
data1=data(read->(x1...x3),in->)
1,2,3
4,5,6
7,8,9
/
;if(wait);pause
;return
 
;statex:  
! stat() computes minimums, maximums, means and std devaitons (line  17928 file c:/j3/j.f90)
;if(type(data1).ne.DATA)dataex
stat()
stat(area,data->cd,sum->bon20,filter->(site.ge.18.5))
stat(ba,data->cd,weight->area)
stat(vol,weight->(1/dbh***2))
;if(wait);pause
;return
 
;covex:  
! Example of covariance (line  18197 file c:/j3/j.f90)
X1=matrix(200)
X1=rann()
;do(i,2,6)
ad=matrix(200)
ad=rann()
X"i"=X"i-1"+0.6*ad
;enddo
dat=newdata(X1...X6,read->(x1...x5))
co=cov(x1...x5);
co=cov(dat%keep);
;if(wait);pause
;return
 
;matrixex:  
! Example of generating matrices (line  18410 file c:/j3/j.f90)
A=matrix(3,
;if(wait);pause
;return
 
;transex:  
! Demonstrates also error handling (line  19153 file c:/j3/j.f90)
tr=trans()
$x3=x1+3
x2=2/$x3;
/
tr%input,tr%output,tr%source;
x1=8
call(tr)
tr2=trans(x1,x2)
$x3=x1+3
x2=2/$x3;
x3=x1+x2+$x3;
/
tr2%input,tr2%output,tr2%source;
call(tr2)
tr2\x3;   !x3 is now local
tr3=trans()
x1=-3
call(tr) !this is causing division by zero
/
Continue=1   ! continue after error
call(tr3)
;if(wait);pause
;return
 
;list2ex:  
! (line  19520 file c:/j3/j.f90)
all=list();  ! empty list
sub=list();
nper=3
;do(i,1,nper)
period#"i"=list(ba#"i",vol#"i",age#"i",harv#"i")
sub#"i"=list(@period#"i",mask->(-2,1,-1))
all=list(@all,@period#"i") !note that all is on both sides
sub=list(@sub,@sub#"i")
;end do
;if(wait);pause
;return
 
;Rex:  
! Example of Rscript (line  20636 file c:/j3/j.f90)
rscript=text()
# A simple R-script that generates a small data to file mydat.txt
wd<-"C:/j3/"
x<-runif(10,0,10)
y<-cbind(1,x)%*%c(1,2)+rnorm(10)
mydat<-data.frame(y,x)
write.table(mydat,file=paste(wd,"/mydat.txt",sep=""))
//
write('miniscript'.r',rscript)
close('miniscript.r)
R('miniscript.r')
print('mydat.txt')
;if(wait);pause
;return
 
;diffex:  
! (line  21399 file c:/j3/j.f90)
lis=list(x1...x3,z3..z5);
lis2=list(x1,z5);
liso=difference(lis,lis2);
liso2=difference(liso,z3);
Continue=1
lisoer=difference(lis,z6); ! error occurs
liser=difference(Lis,x3);  !error occurs
Continue=0
;if(wait);pause
;return
 
;derex:  
! Derivatives with der() (line  21732 file c:/j3/j.f90)
tr=trans()
der(x)
f=(1+x)*cos(x)
/
fi=draw(func->tr(d[x]),x->x,xrange->(0,10),color->Blue,continue->)
fi=draw(func->tr(f),x->x,xrange->(0,10),color->Cyan,append->,continue->fcont)
;if(wait);pause
;return
 
;derex2:  
! (line  21740 file c:/j3/j.f90)
X=matrix(do->(0,1000,10))
e=matrix(nrows(X))
e=rann(0,2);
A,Pmax,R=0.1,20,2
A*Pmax*1000/(A*1000+Pmax);
Y=A*Pmax*X/.(A*X+Pmax)-R+e  !rectangular hyperbola used often for photosynthesis
rect=trans()
der(A,Pmax,R)
f=A*Pmax*I/(A*I+Pmax)-R
/
fi=draw(func->(rect(f)),x->I,xrange->(0,1000),color->Orange,width->2,continue->,show->0)
da=newdata(X,Y,e,extra->(Regf,Resid),read->(I,P,er))
stat()
fi=plotyx(P,I,append->,show->0,continue->fcont)
A,Pmax,R=0.07,17,3 !initial values
fi=draw(func->(rect(f)),x->I,xrange->(0,1000),color->Green,width->2,append->,show->0,continue->)
reg=nonlin(P,f,par->(A,Pmax,R),var->,corr->,data->da,trans->rect)
reg%var;
reg%corr;
corrmatrix(reg%var);
fi=draw(func->(rect(f)),x->I,xrange->(0,1000),color->Violet,append->,continue->fcont)
;if(wait);pause
;return
 
;densityex:  
! Example of distributions (line  22668 file c:/j3/j.f90)
ber=density(func->(1-p+(2*p-1)*$),xrange->(0,1),discrete->); Bernoully
bim=matrix(100)
bim=random(ber)
mean(bim);
p*(1-p);  !theoretical variance
var(bim);
pd=density(func->exp(-0.5*$*$),xrange->(-3,3))  !Normal distribution
ra=random(pd);
f=matrix(1000)
f=random(pd)
da=newdata(f,read->x)
stat(min->,max->)
cl=classify(x->x,xrange->);
fi=drawclass(cl)
fi=drawclass(cl,area->)
fi=draw(func->pdf(x),x->x,xrange->,append->)
f=matrix(1000)
f=rann()
da=newdata(f,read->x)
stat(min->,max->)
cl=classify(x->x,xrange->)
fi=drawclass(cl,histogram->,classes->20)
den=density(cl);
fi=drawline(den)
;if(wait);pause
;return
  
;ALL:
inpuparag
shortex
dotsex
jfuncex
codeoptex
comoptex
recursion
transfunc
matrixop
doex
retex
errexitex
gotoex
assignex
operexr
inpuincl
inpugotoex
inpudoex
inpusumex
getset
list2ex
ilistex
typeex
whichex
findex
newdataex
newclassdata
addingclass
logisticex
showex
plot3dex
ranex
selectex
rannex
ranbinex
cpuex
secondsex
drawex
drawlineex
drawclassex
plotyxex
askex
mergex
dataex
statex
covex
matrixex
transex
list2ex
Rex
diffex
derex
derex2
densityex
;return
