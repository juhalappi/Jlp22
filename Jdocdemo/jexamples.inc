 ** this file contains           70  examples of using Jlp22
** launch Jlp22 in the command promt in the folder of this file 
** using: Jlp22 jexamples.inc
** if you want to pause after each example answer to first question with 1
** if not, answer with <return>
** if you want to pause after each figure answer to second question with 1
** if not, answer with <return>
exfile=thisfile()
current=';incl(exfile,from->current)'
inpuparag=';incl(exfile,from->inpuparag)'
shortex=';incl(exfile,from->shortex)'
dotsex=';incl(exfile,from->dotsex)'
inpulistelem=';incl(exfile,from->inpulistelem)'
sublistex=';incl(exfile,from->sublistex)'
expandex=';incl(exfile,from->expandex)'
subobjex=';incl(exfile,from->subobjex)'
subobjex2=';incl(exfile,from->subobjex2)'
subobjex2=';incl(exfile,from->subobjex2)'
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
congotoex=';incl(exfile,from->congotoex)'
assignex=';incl(exfile,from->assignex)'
showex=';incl(exfile,from->showex)'
errorex=';incl(exfile,from->errorex)'
inpuincl=';incl(exfile,from->inpuincl)'
inpugotoex=';incl(exfile,from->inpugotoex)'
inpudoex=';incl(exfile,from->inpudoex)'
inpusumex=';incl(exfile,from->inpusumex)'
getset=';incl(exfile,from->getset)'
list2ex=';incl(exfile,from->list2ex)'
ilistex=';incl(exfile,from->ilistex)'
typeex=';incl(exfile,from->typeex)'
deleteoex=';incl(exfile,from->deleteoex)'
deletfex=';incl(exfile,from->deletfex)'
whichex=';incl(exfile,from->whichex)'
findex=';incl(exfile,from->findex)'
newdataex=';incl(exfile,from->newdataex)'
classdata=';incl(exfile,from->classdata)'
logisticex=';incl(exfile,from->logisticex)'
datawcaseex=';incl(exfile,from->datawcaseex)'
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
joindataex=';incl(exfile,from->joindataex)'
partdataex=';incl(exfile,from->partdataex)'
linkdataex=';incl(exfile,from->linkdataex)'
askex=';incl(exfile,from->askex)'
inverseex=';incl(exfile,from->inverseex)'
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
jlpzex=';incl(exfile,from->jlpzex)'
jlpex=';incl(exfile,from->jlpex)'
factopt=';incl(exfile,from->factopt)'
AGAIN=';incl(exfile)'
ALL=';incl(exfile,from->ALL)'
ask(wait,q->'pause after each example(1/0)>')
ask(fpause,q->'pause after each figure(1/0)>')
fcont=.not.fpause
**
** if you want to run all examples type: ALL
** if you want to run a specific example type the name of the example
** if you want to run all examples after a specific example
** go to the final section after ;ALL
** and put text ;current: before the the name of the example , an type then 
** current
** even if running with ALL without pause for figures and examples
** Jlp22 asks some numbers to demonstrate ask() function
;return
 
 
;inpuparag:  
** Example of inputparagraph (line   1292 file jsections.txt)
transa=trans()
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
** Example of using shortcuts and include files (line   1334 file jsections.txt)
short1='sin(Pi)+cos(Pi);'
short1
te=text()
this=thisfile()
ju1=';incl(this,from->a1)'
ju2=';incl(this,from->a2)'
;return
;a1:
!! greetings from a1
;return
;a2:
** here, jump to a1
ju1
*! back here, return to sit> or next example in ALL
;return
//
write('shortex.txt',$,te)
close('shortex.txt')
;incl(shortex.txt)
ju1
ju2
delete_f('shortex.txt')
te=0 !delete also text object te
;if(wait);pause
;return
 
 
;dotsex:  
** Example of dots construct (line   1469 file jsections.txt)
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
 
 
;inpulistelem:  
** Getting name from LIST (line   1490 file jsections.txt)
lis=list(c2...c5);
@lis=2...5;
@lis(2)=6;
@lis(4)=@lis(1);
@lis(2)%@lis(4)=66;
** Utilizing LISTs  of LISTs
lists=list(lis1...lis3);
lis1=list(x1...x4);
@lis1=1...4
lis2=list(z1...z3);
@lis2=10,20,30
lis3=list(y1,y2);
@lis3=100,200
** @lists(2) drops lis2 to the input line giving @lis2; which cause
** that elements of lis2 are printed
@@lists(2);
;if(wait);pause
;return
 
 
;sublistex:  
** taking sublist (line   1513 file jsections.txt)
lista=list(x1...x5);
lista2%5=lista(2,-5);
;if(wait);pause
;return
 
 
;expandex:  
** Example of expanding lists with @ (line   1533 file jsections.txt)
list0=list(site,altitude)
list1=list(
;do(i,1,3)
vol"i",ba"i",
;enddo
vol4,ba4);
dat=data(in->,read->(@list0,@list1))
1,2,3,4,5,6,7,8,9,10
2,3,4,5,6,7,8,9,10,11
/
stat()
;if(wait);pause
;return
 
 
;subobjex:  
** Seeing subobjects (line   1557 file jsections.txt)
dataa=data(in->,read->(x,y))
1,2
3,4
/
dataa%?; !prints subobjects
;if(wait);pause
;return
 
 
;subobjex2:  
** (line   1571 file jsections.txt)
dataa=data(in->,read->(x,y))
1,2
3,4
/
lista=;list(dataa%?);
**Then all objects in the list are printed.
@lista;
**Seeing and printing relatives
x1a...x3a=3...5
x?a;
rel=;list(x?a);
@rel;
;if(wait);pause
;return
 
 
;subobjex2:  
** (line   1592 file jsections.txt)
dataa=data(in->,read->(x,y))
1,2
3,4
/
lista=;list(dataa%?);
**Then all objects in the list are printed.
@lista;
;if(wait);pause
;return
 
 
;jfuncex:  
** Examples of Jlp22-functions (line   1874 file jsections.txt)
b,c=2,3;
a = sin(cos(c)+b); ! Usual arithmetic functions can have numeric values as arguments
c=matrix(2,2,in->);
1,2
3,4
/
** now c is MATRIX
a = sin(cos(c)+b);
Dm=matrix(do->(0.1,40))
nob=nrows(Dm)
e=matrix(nob)
e=rann()
Hm=0.5+Dm**0.7+e
dat=newdata(Dm,Hm,read->(D,H))
stat(D,H,min->,max->) ! Here arguments must be variable names
plotyx(H,D,continue->fcont) !arguments of the function are variables
h=matrix(5,5);
h=rann();
g=matrix(5,do->5);
c = inverse(h+t(g)); ! The argument can be intermediate result from matrix computations.
;if(wait);pause
;return
 
 
;codeoptex:  
** Codeoptions (line   1907 file jsections.txt)
dat=data(read->(D,H),in->)
3,2
2,4
4,1
/
stat(D,H,filter->(H.gt.D)) ! only those observations are accepted which pass the filter
fi=draw(func->(sin($x)+1),x->$x,xrange->(0,10),color->Red,ylabel->'sin(x)+1',
continue->fcont,xlabel->'x',width->2) ! the func-> option transmits the function to be drawn not a single value.
;if(wait);pause
;return
 
 
;comoptex:  
** data1 (line   1972 file jsections.txt)
dat=data(read->(x,y),in->)
1,2
3,4
5,6
/
transa=trans()
xy=x*y
x,y,xy;
/
stat(trans->transa)
;if(wait);pause
;return
 
 
;recursion:  
** Recursion produces system crash. (line   2013 file jsections.txt)
transa=trans() !level will be initialized as zero
level;
level=level+1
call(transa)
/
Continue=1  !error is produced
call(transa)
Continue=0
;if(wait);pause
;return
 
 
;transfunc:  
** Transformation as a function (line   2036 file jsections.txt)
delete_o(a,c)
transa=trans()
a=8;
c=2;
/
transb=trans()
a=5;
c=1;
/
c=2
a=c+transb(a)+c+transa(a);
;if(wait);pause
;return
 
 
;matrixop:  
** Matrix operations (line   2256 file jsections.txt)
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
** do-loop (line   2331 file jsections.txt)
!!begin
transa=trans()
do(i,1,5)
i;
ad1: if(i.eq.3)goto(cycle)
i;
if(i.eq.4)goto(jump)
cycle:enddo
jump:i;
/
call(transa)
;if(wait);pause
;return
 
 
;retex:  
** example of return and goto () (line   2352 file jsections.txt)
transa=trans()
ad1:r=ran();
if(r.lt.0.2)return
goto(ad1)
/
call(transa)
;if(wait);pause
;return
 
 
;errexitex:  
** errexit() returns as if error had occured (line   2372 file jsections.txt)
transa=trans()
if(a.eq.0)errexit('illegal value ',a)
s=3/a; ! division with zero is tested automatically
/
a=3.7
call(transa)
transa(s); !tr can also be used as a function
a=0
Continue=1  !Do not stop in this selfmade error
call(transa)
Continue=0
;if(wait);pause
;return
 
 
;gotoex:  
** Example of unconditional goto (line   2411 file jsections.txt)
transa=trans()
i=0
if(i.eq.0)goto(koe)
out=99;
koe:out=88;
/
call(transa)
out;
;if(wait);pause
;return
 
 
;congotoex:  
** (line   2426 file jsections.txt)
transa=trans()
out=999
goto(ad1)
77;
ad1:
1;
goto(2,ad1,ad3)
88;
ad2:
2;
goto(go,ad1,ad3)
out=0;
return
ad3:
3;
goto(3,ad3,ad1,ad2)
/
go=0  ! This determines the last goto
call(transa)
out;
go=4
** Now error occurs
Continue=1
call(transa)
Continue=0
;return
;if(wait);pause
;return
 
 
;assignex:  
** Examples of assignments (line   2491 file jsections.txt)
a=matrix(2,3);
a=4;
a=rann();
v1...v5=2...6;
v1...v5=77;
Continue=1  ! ERROR
v1..v3=1,5;
v1..3=1...3   ! v is missing from the front of 3
Continue=0
;if(wait);pause
;return
 
 
;showex:  
** Example of show() (line   2566 file jsections.txt)
fi=draw(func->sqrt2(x),x->x,xrange->(-50,50),continue->fcont)
show(fi,xrange->(-60,60),xlabel->'NEWX]Times Roman,16[',ylabel->'NEWY]Courier,10[',continue->fcont)
show(fi,axes->10,continue->fcont)
show(fi,axes->01,continue->fcont)
show(fi,axes->00,continue->fcont)
Window='400,800'
show(fi,continue->fcont)
Window='700,700'
fi=drawline(1,10,3,1,color->Red,continue->fcont)
show(fi,xrange->(1.1,11),continue->fcont) !the line is not visible
dat=data(read->(x,y),in->)
1,4
2,6
3,2
5,1
/
stat()
fi=plotyx(y,x,continue->fcont)  ! Gnuplot hides points at border
show(fi,xrange->(0,6),yrange->(0,7),continue->fcont)
;if(wait);pause
;return
 
 
;errorex:  
** (line   3210 file jsections.txt)
round=5
write('inca.txt',$,'**we are in inca.txt')
write('inca.txt',$,'round;')
write('inca.txt',$,';incl(incb.txt)')
close('inca.txt')
print_f('inca.txt')
write('incb.txt',$,'**we are in incb.txt')
write('incb.txt',$,'round;')
write('incb.txt',$,'round=round-1;')
write('incb.txt',$,'call(transa)')
write('incb.txt',$,';incl(inca.txt)')
close('incb.txt')
print_f('incb.txt')
transa=trans()
c=4/round;
/
** let us see what happens
Continue=1
;incl(inca.txt)
** maximum ;incl nesting was obtained
round=2
** what happens now?
;incl(inca.txt)
Continue=0
delete_f('inca.txt','incb.txt')
;if(wait);pause
;return
 
 
;inpuincl:  
** Example of ;incl() (line  11752 file c:/jlp22/jutilities.f90)
file=text()
** File start
i=1;
;goto(ad1)
** Never here
i=2;
;ad1:i=66;
**After ad1
;goto(ad2,ad3,2)  !select label from a label list
;ad2:
** After ad2
i=3;
;ad3:
** After ad3
i=4;
;ad4:
** After ad4
i=5;
;ad5:
** After ad5
i=6;
//
file;
;if(exist_f('file.txt'))delete_f('file.txt')
write('file.txt',$,file)
close('file.txt')
;incl(file.txt)
;incl(file.txt,from->ad2)
;if(wait);pause
;return
 
 
;inpugotoex:  
** Example of ;goto() and ;incl() (line  11793 file c:/jlp22/jutilities.f90)
gototxt=text()
!! Start jumping
;goto(ad2)
;ad1:
!!Greetings from ad1
;return
;ad2:
!!Greetings from ad2
;goto(ad1)
//
print(gototxt)
if(exist_f('goto.txt'))delete_f('goto.txt')
write('goto.txt',gototxt)
close('goto.txt')
print('goto.txt')
;incl(goto.txt)
;incl(goto.txt,from->ad1)
delete_f('goto.txt')
;if(wait);pause
;return
 
 
;inpudoex:  
** Examples of ;do() (line  12365 file c:/jlp22/jutilities.f90)
;do(i,1,2)
x"i"="i"*10
print('Greetings from iteration "i"')
;enddo
print(x1,x2)
varlist=list(x0,y0,
;do(i,1,3)
x"i",y"i",
;enddo
x4,y4);
;if(wait);pause
;return
 
 
;inpusumex:  
** Example of ;sum() (line  12623 file c:/jlp22/jutilities.f90)
prob=problem()
;sum(i,1,5)(a"i"*x"i")==max
;sum(i,1,3)(a"i"*x"i")<8
/
;if(wait);pause
;return
 
 
;getset:  
** Get or set submatrices (line   2007 file c:/jlp22/j.f90)
A=matrix(3,4,do->);
B=A(1,-2,3,-4);
A(1,-2,3,-4)=B+3;
A(1,-2,3,-4,sum->)=-5;
A(1,-2,3,-4,sum->2)=A(2,-3,1,-2);
C=A({1,3},{4...2});
H=matrix(4,4,diag->,do->3);
H(3,-4,diag->)=matrix(2,values->(4,7));
;if(wait);pause
;return
 
 
;list2ex:  
** (line   3587 file c:/jlp22/j.f90)
lis2=list2(x1...x4,3,5);
li1=list(x1,x2);
li2=list(z,y);
lis=list2(@li1,@li2,77);
;if(wait);pause
;return
 
 
;ilistex:  
** ILIST examples (line   3634 file c:/jlp22/j.f90)
{1,4,5};
{4...1};
A=matrix(4,4)
A({1,5},{3}=
;if(wait);pause
;return
 
 
;typeex:  
** Example of type (line   3806 file c:/jlp22/j.f90)
ttt=8;   !REAL
type(ttt);
type('ttt'); !type is CHAR
type('ttt',content->);
cttt='ttt'
type(cttt);
type(cttt,content->);
;if(wait);pause
;return
 
 
;deleteoex:  
** Delete object (line   3859 file c:/jlp22/j.f90)
a=matrix(2,3,do->);
b=t(a);
delete_o(a,b)
a,b;
;if(wait);pause
;return
 
 
;deletfex:  
** Deletef file (line   3882 file c:/jlp22/j.f90)
write('delete_fex.txt',$,'a=matrix(2,4,do->);')
write('delete_fex.txt',$,'delete_o(a)')
write('delete_fex.txt',$,'a;')
close('delete_fex.txt')
;incl(delete_fex.txt)
delete_f('delete_fex.txt')
;if(wait);pause
;return
 
 
;whichex:  
** Example of which() (line   4158 file c:/jlp22/j.f90)
c=9
which(a.eq.3.or.c.gt.8,5,a.eq.7,55);
a=7
which(a.eq.3.or.c.gt.8,5,a.eq.7,55);
a=5
which(a.eq.3.or.c.gt.8,5,a.eq.7,55);
which(a.eq.3.or.c.gt.8,5,a.eq.7,55,108);
;if(wait);pause
;return
 
 
;findex:  
** Finding something from matrix (line   7162 file c:/jlp22/j.f90)
** Repeating the example, different results will be obtained
rm=matrix(500)
m,s=2,3
rm=rann(m,s)
mean(rm),sd(rm),min(rm),max(rm);
m+1.96*s;
** index of first row satisfying the condition:
first=find(rm,filter->($.ge.m+1.96*s));
** indeces of all rows satisfying the condition
large=find(rm,filter->($.ge.m+1.96*s),any->);
nrows(large),nrows(large)/nrows(rm),mean(large),sd(large),min(large),max(large);
** vector of equal size as rm containing 1 or 0
large2=find(rm,filter->($.ge.m+1.96*s),expand->)
mean(large2),min(large2),max(large2);
;if(wait);pause
;return
 
 
;newdataex:  
** newdata() generates a new data object. (line   7585 file c:/jlp22/j.f90)
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
transa=trans()
;do(i,1,3)
;do(j,1,2)
x"i"#z"j"=x"i"*z"j"
;enddo
;enddo
/
new=newdata(data1,matrix1,read->(z1,z2),maketrans->transa)
print(new)
;if(wait);pause
;return
 
 
;classdata:  
** Hierarchical data (line   8758 file c:/jlp22/j.f90)
nstand=10
xm=matrix(nstand)
xm=rann(3)
ym=0.7*xm+0.1*xm
xm;
ym;
standdata=newdata(xm,ym,read->(X,Y))
stat()
ntree=6
xt=matrix(ntree*nstand)
yt=matrix(ntree*nstand)
standv=matrix(ntree*nstand)
ex=matrix(ntree*nstand)
ey=matrix(ntree*nstand)
transa=trans()
jj=0
do(i,1,nstand)
do(j,1,ntree)
jj=jj+1
standv(jj)=i
ex(jj)=rann()
ey(jj)=0.3*ex(jj)+0.3*rann()
xt(jj)=xm(i)+ex(jj)
yt(jj)=ym(i)+0.3*ex(jj)+0.3*rann()
enddo
enddo
/
call(transa)
treedata=newdata(standv,xt,yt,read->(stand,x,y))
stat()
!! Making class level data object from treedata
classvector(x,y,class->stand,data->treedata,mean->,min->)
standdata2=newdata(x[stand]%mean,y[stand]%mean,x[stand]%min,y[stand]%min,
read->(x,y,xmin,ymin))
stat()
classvector(x,y,class->stand,data->treedata,mean->,expand->)
ex2=treedata(x)-x
;if(wait);pause
;return
 
 
;logisticex:  
** Example of logistic function (line   9583 file c:/jlp22/j.f90)
transa=trans()
der(a,x0)
f=logistic(a*(x-x0));
/
x,x0,a=10,5,0.1
call(transa)
;if(wait);pause
;return
 
 
;datawcaseex:  
** Example of datawcase (line  10336 file c:/jlp22/j.f90)
sawmills=datawcase(read->(capacity1...capacity4))
Kotka,110,120,130,140
Oulu,210,220,230,240
/
sawsub=;list(sawmills%?);
@sawsub;
stat()
;if(wait);pause
;return
 
 
;plot3dex:  
** plot3d() example see p.  328 in Mehtatalo & Lappi 2020 (line  11200 file c:/jlp22/j.f90)
mat=matrix(1000000,3)
mat2=matrix(1000000,3)
transa=trans() !second order response surface
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
*** This example takes some time
call(transa)
fi=plot3d(mat,sorted->,continue->fcont)
** This is commneted because it takes some time
** fi=plot3d(mat2,sorted->,continue->fcont)
;if(wait);pause
;return
 
 
;ranex:  
** (line  13755 file c:/jlp22/j.f90)
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
** Random selection (line  13811 file c:/jlp22/j.f90)
** select 500 numbers without replacement from 10000
** output is vector of 10000 rows containing 0 or 1
S=select(500,10000)
nrows(S),mean(S),sum(S),500/10000;
;if(wait);pause
;return
 
 
;rannex:  
** Random normal variates, illustrating also find (line  13983 file c:/jlp22/j.f90)
rx=rann();  !Output is REAL
rm=matrix(1000)
rm=rann()
rm(1,-5);
print(mean(rm),sd(rm),min(rm),max(rm))
Continue=1 !an error
large=find(rm,filter->($.ge.2),any)
Continue=0
large=find(rm,filter->($.ge.2),any->)
100*nrows(large)/nrows(rm);
cpu0=cpu()
rm2=matrix(1000000)
rm2=rann(10,2)  !there cannot be arithmetix opreations in the right side
cpu()-cpu0;
mean(rm2),sd(rm2),min(rm2),max(rm2);
large=find(rm2,filter->($.ge.14),any->)
100*nrows(large)/nrows(rm2);
	!
;if(wait);pause
;return
 
 
;ranbinex:  
** Random binomial (line  14097 file c:/jlp22/j.f90)
ranbin(10,0.1);
ranbin(10,0.1);
A=matrix(1000,2)
Continue=1
A(All,1)=ranbin(20,0.2)
Continue=0
A=matrix(1000)
A=ranbin(20,0.2)
B=matrix(1000)
B=ranbin(20,0.2)
da=newdata(A,B,read->(s1,s2))
stat(min->,max->)
cl=classify(1,x->s1,xrange->)
fi=drawclass(cl,histogram->,color->Blue,continue->fcont)
cl=classify(1,x->s2,xrange->)
fi=drawclass(cl,histogram->,color->Red,append->,continue->fcont)
;if(wait);pause
;return
 
 
;cpuex:  
** Example of cpu-timing (line  14174 file c:/jlp22/j.f90)
cpu0=cpu()
a=matrix(100000)
a=ran() !uniform
mean(a),sd(a),min(a),max(a);
cpu1=cpu()
elapsed=cpu1-cpu0;
;if(wait);pause
;return
 
 
;secondsex:  
** Example of elapsed time (line  14359 file c:/jlp22/j.f90)
cpu0=cpu()
sec0=secnds()
a=matrix(100000)
a=ran() !uniform
mean(a),sd(a),min(a),max(a);
cpu1=cpu()
sec1=secnds()
elapsed=cpu1-cpu0;
selapsed=sec1-sec0;
;if(wait);pause
;return
 
 
;drawex:  
** Example of draw() (line  14618 file c:/jlp22/j.f90)
fi=draw(func->sin(x),x->x,xrange->(0,2*Pi),color->Blue,continue->fcont)
fi=draw(func->cos(x),x->x,xrange->(0,2*Pi),color->Red,append->,continue->fcont)
;if(type(figyx).ne.FIGURE)plotyxex
show(figyx,continue->fcont)
reg0=regr(y,x)
stat(data->datyx,min->,max->)
figyx=draw(func->reg0(),x->x,xrange->,color->Violet,append->,continue->fcont)
transa=trans()
x2=x*x
/
reg2=regr(y,x,x2,data->datyx,trans->transa)
transa=trans()
x2=x*x
fu=reg2()
/
Continue=1  !Error
figyx=draw(func->transa(fu),xrange->,color->Orange,append->,continue->fcont)
continue=0
figyx=draw(func->transa(fu),x-x,xrange->,color->Orange,append->,continue->fcont)
Continue=1  !Errors
fi=draw(func->sin(x),x->x)
fi=draw(xrange->(1,100),func->Sin(x),x->x)
Continue=0
;if(wait);pause
;return
 
 
;drawlineex:  
** Example of drawline() (line  14820 file c:/jlp22/j.f90)
fi=draw(func->sin(x),x->x,xrange->(0,2*Pi),color->Blue,continue->fcont)
fi=drawline(Pi,sin(Pi)+0.1,label->'sin()',append->,continue->fcont)
xval=matrix(do->(1,10));
mat=matrix(values->(xval,xval+1,xval,xval+2,xval,xval+3))
fi=drawline(mat,color->Red,continue->fcont)
fi=drawline(mat,color->Orange,break->,continue->fcont)
xm=matrix(do->(0,100,1))
e=matrix(101)
e=rann(0,3)
ym=2*x+0.3*xm*.xm+0.4+e
dat=newdata(xm,ym,read->(x,y),extra->(Regf,Resid))
reg=regr(y,x)
figyx=plotyx(y,x,continue->fcont)
figr=plotyx(Resid,x,continue->fcont)
reg0=regr(y,x)
stat(min->,max->)
figyx=draw(func->reg0(),x->x,xrange->,color->Violet,append->,continue->fcont)
transa=trans()
x2=x*x
if(type(reg2).eq.REGR)fu=reg2()
/
reg2=regr(y,x,x2,trans->transa)
figyx=draw(func->transa(fu),x->x,xrange->,color->Orange,append->,continue->fcont)
Continue=1  !Errors
fi=draw(func->sin(x),x->x)
fi=draw(xrange->(1,100),func->Sin(x),x->x)
Continue=0
;if(wait);pause
;return
;if(wait);pause
;return
 
 
;drawclassex:  
** Examples of drawclass() (line  15292 file c:/jlp22/j.f90)
X=matrix(do->(1,100,0.1))
e=matrix(nrows(X))
e=rann()
X2=0.01*X*.X !elementwise product
Y=2*X+0.01*X2+(1+0.3*X)*.e  !nonequal error variance,quadratic function
dat=newdata(X,Y,X2,read->(x,y,x2),extra->(Regf,Resid))
stat(min->,max->)
reg=regr(y,x) ! Regf and resid are put into the data
fi=plotyx(y,x,continue->fcont)
fi=drawline(x%min,x%max,reg(x%min),reg(x%max),width->3,color->Cyan,append->,continue->fcont)
cl=classify(Resid,x->x,xrange->,classes->5)
fi=drawclass(cl,color->Blue,continue->fcont)
fi=drawclass(cl,mean->,sd->,continue->fcont,color->Red)
fi2=drawclass(cl,se->,continue->fcont)
fi=drawclass(cl,se->,continue->fcont,color->Black,width->2,append->)
fi=drawclass(cl,sd->,continue->fcont)
fi=drawclass(cl,var->,continue->fcont)
** x-values were equally distributed due to data construction
fi=drawclass(cl,histogram->,area->,continue->fcont)
fi=draw(func->pdf(0,rmse(reg)),x->x,xrange->,append->,continue->fcont) ! xrange comes from stat()
;if(wait);pause
;return
 
 
;plotyxex:  
** plotyx() (line  15562 file c:/jlp22/j.f90)
xmat=matrix(do->(0,10,0.001))
transa=trans()
y=2+3*x+0.4*x*x+4*rann()
/
datyx=newdata(xmat,read->x,maketrans->transa,extra->(Regf,Resid))
fi=plotyx(y,x,continue->fcont)
fi=plotyx(x,func->transa(y),mark->3,color->Orange,continue->fcont)
reg=regr(y,x)
figyx=plotyx(y,x,show->0)
fi=plotyx(Regf,x,append->,continue->fcont)
fir=plotyx(Resid,x,continue->fcont)
;if(wait);pause
;return
 
 
;joindataex:  
** splitting and joining period DATA (line  15777 file c:/jlp22/j.f90)
cdata=data(in->'cdat.txt',read->(cdata%nobsw,site))
stat()
xdata=data(in->'xdat.txt',read->(npv#0,npv#5,income1...income5),time->)
stat()
linkdata(cdata,xdata)
stat(up->)
;do(i,1,4)
pv"i"=list(income"i")
;enddo
pv5=list(income5,npv#0,npv#5)
xdatap=splitdata(xdata,periodvars->(pv1...pv5))
stat(data->cdata)
;do(i,1,5)
stat(data->xdatap#"i")
write('xdatap.txt',$,xdatap#"i"%matrix)
;enddo
close('xdatap.txt')
contd=0
keepo=1
stat(data->cdata,sum->)
nobscur=cdata%nobsw%sum;
;do(i,1,5)
Pv"i"=list(xdataP#"i"%nobsw,@pv"i")
;enddo
;do(i,1,5)
;if(i.eq.5)keepo=0
xdataP#"i"=data(in->'xdatap.txt',read->(@Pv"i"),keepopen->keepo,continue->contd,nobs->nobscur)
stat(data->xdataP#"i",sum->)
nobscur=xdataP#"i"%nobsw%sum;
contd=1
stat(data->xdataP#"i")
;enddo
stat(data->cdata)
xdatanew=joindata(cdata,xdataP#1...xdataP#5)
stat(data->xdatanew)
** Did the procedure loose any data
stat(data->xdata)
delete_f('xdatap.txt')
;if(wait);pause
;return
 
 
;partdataex:  
** (line  16533 file c:/jlp22/j.f90)
da=data(in->,read->(x1,x2,x3))
1,2,3
4,5,6
7,8,9
/
da1=partdata(data->da,filter->(x3.le.6))
da1%matrix;
da2=partdata(data->da,reject->(x3.gt.6))
da2%matrix;
da3=partdata(data->da,from->2)
da3%matrix;
da4=partdata(data->da,keep->(x1,x2))
da4%matrix;
da4%keep;
da=partdata(data->da,drop->x2,filter->x1.lt.7)
da%matrix;
da%keep;
;if(wait);pause
;return
 
 
;linkdataex:  
** Example for linkdata. (line  16684 file c:/jlp22/j.f90)
** make upper level DATA
dataa=data(read->(dataa%nobsw,site),in->)
2,4
3,5
/
dataa%matrix;
** make subdata as an ordinary DATA
datab=data(read->(x1,y),in->)
1,2
3,4
5,6
7,8
6,9
/
datab%matrix;
datab%keep;
**link now DATAs
** First varaible in the upper level dataa must be dataa%nobsw
linkdata(dataa,datab)
listb=;list(datab%?);
@listb;
** when working with subdata the upper level data is feeded in for all observations
** if up-> is present
**   even if they are not part of the data matrix as seen from datab%keep.
stat()
stat(up->)
**
** Note stat() and all functions assume that the last DATA created is used
** Thus when there are several DATAs around it is safer all use data-> option
** i.e. the above could/should be stat(data->datab)
**
**
** The flat file can be created also as follows:
** when dealing with the subdata the upper level data is automatically used
transa=trans()
Unit=Unit !adds Unit and site variables from up-data to sub-data
site=site
Unit%obsw=Unit%obsw
/
** In TRANS transa, the input variables come from the upper level data, and
** outputvariables go to the new data based on DATA datab.
datac=newdata(datab,maketrans->transa)
stat()
datac%matrix;
;if(wait);pause
;return
 
 
;askex:  
** Examples for ask() (line  16969 file c:/jlp22/j.f90)
a=ask(default->8)
ask(a,default->8)
print(ask()+ask()) ! ask without argument is a numeric function
ask(v,q->'Give v>')
;if(wait);pause
;return
 
 
;inverseex:  
** inverse() and condition number (line  18204 file c:/jlp22/j.f90)
matrixa=matrix(4,4)
matrixa=1
***  well conditoned matrix
matrixa(diag->)=10
matrixa;
matrixb=inverse(matrixa);
matrixb%condition;
** almost singular matrix
matrixa(diag->)=1.05
matrixb=inverse(matrixa);
matrixb%condition;
** figure of condition number
transa=trans()
matrixa(diag->)=diag
matrixb=inverse(matrixa)
/
** Note that the lower bound is equal to the dimension
figa=draw(x->diag,xrange->(1.05,50),func->transa(matrixb%condition),
color->Blue,continue->fcont)
;if(wait);pause
;return
 
 
;mergex:  
** Merging list (line  18403 file c:/jlp22/j.f90)
lista=list(x1...x5,x3);
** LISTs must be expanded with @
listb=merge(@lista)
ilista=ilist(1...5,3);
ilistb=merge(ilista,ilist->);
*** without ilist the arguments are interpreted as object indices
ilistc=merge(ilista);
;if(wait);pause
;return
 
 
;dataex:  
** data() generates a new data object by reading data. (line  19271 file c:/jlp22/j.f90)
**This example now contains several combinations of options
** and this example is not yet cleaned
** it may not contain all possible option comination s
da=data(read->(x1,x2),in->)
1,2
3,4
/
da%keep;
da%matrix;
da2=data(read->(x1...x4),in->)
1,2,3,4
5,6,7,8
/
da2%keep;
da2%matrix;
write('da2.txt',$,da2%matrix)
close('da2.txt')
tr=trans()
x13=x1+x3
/
da2=data(in->'da2.txt',read->(x1...x4),keep->(x2,x4,x13),maketrans->tr)
da2%keep;
da2%matrix;
delete_f('da2.txt')
write('da3.txt',$,'x1...x3')
write('da3.txt',$,da2%matrix)
close('da3.txt')
da3=data(in->'da3.txt',read->)
da3%keep;
da3%matrix;
delete_f('da3.txt')
da=data(read->(x1,x2),keep->(x1,x2,Regf,Resid),in->)
1,2
3,4
/
da%keep;
da%matrix;
tr=trans()
x1#x2=x1*x2
x1%x2=x1+x2
/
dab=data(read->(x1,x2),maketrans->tr,in->)
1,2
3,4
/
dab%keep;
dab%matrix;
trup=trans()
x1#y1=x1*y1
x2%y2=x2+y2
/
**first variable must be %nobsw
da%keep(1)=da%nobsw
dasub=data(read->(y1,y2),in->,up->da,maketrans->trup)
3,4
6,7
8,9
10,11
/
dasub%keep;
dasub%matrix;
stat(data->dasub)
**utilize upper level data
stat(data->dasub,up->)
ndupl=trans()
Duplicates=0
if(da%obs.eq. 2.and.Obsw.eq.2)Duplicates=2
/
dupl=trans()
x1#y1=x1*y1
x2%y2=x2+y2
if(Duplicate.gt.0)then
x1#y1=x1#y1+Duplicate
x1%y1=x1#y1+Duplicate
endif
/
da%keep;
da%matrix;
dasub3=data(read->(y1,y2),in->,up->da,maketrans->trup) !,duplicate->(ndupl,dupl))
3,4
6,7
8,9
10,11
/
dasub3%keep;
dasub3%matrix;
da%keep;
da%matrix;
dasub2=data(read->(y1,y2),in->,up->da,duplicate->(ndupl,dupl))
3,4
6,7
8,9
10,11
/
da%matrix;
dasub2%keep;
dasub2%matrix;
stat(data->dasub)
**utilize upper level data
stat(data->dasub2,up->)
;return
write('dasub.txt',$,dasub%matrix)
close('dasub.txt')
**this must be redefined because duplication changed nobsw
da=data(read->(da%nobsw,x2),keep->(da%nobsw,x2,Regf,Resid),in->)
1,2
3,4
/
da%keep;
da%matrix;
dasub=data(read->(y1,y2),in->'dasub.txt',up->da,maketrans->trup,
keep->(y1,y2,@trup%output,Regf,Resid))
dasub%keep;
dasub%matrix;
** x2 is now alwasy zero
regr(y1,x2,data->dasub)
** Regf and Resid are put into the data matrix
dasub%matrix;
** now x2 is picked from the up-data
regr(y1,x2,data->dasub,up->)
dasub%matrix;
delete_f('dasub.txt')
write('data.txt',$,da%matrix)
close('data.txt')
dc=data(read->(x1,x2),in->'data.txt')
dc%matrix;
dc=data(read->(x1,x2),in->'data.txt',maketrans->tr)
dc%keep;
dc%matrix;
** another way to make data
damat=matrix(nrows(da%matrix),ncols(da%matrix),in->'data.txt')
da2=newdata(damat,read->(x1...x4))
da2%matrix;
delete_f('data.txt')
write('data.txt','(4f4.0)',da%matrix)
close('data.txt')
dc=data(read->(x1,x2),form->'(2f4.0)',in->'data.txt')
delete_f('data.txt')
dc%matrix;
write('data.bin','b',da%matrix)
close('data.bin')
dc=data(read->(x1,x2),form->'b',in->'data.bin')
dc%matrix;
dc=data(read->(x1,x2),form->'b',in->'data.bin',maketrans->tr)
dc%keep;
dc%matrix;
delete_f('data.bin')
write('data.bin','B',da%matrix)
close('data.bin')
dc=data(read->(x1,x2),form->'B',in->'data.bin')
dc%keep;
dc%matrix;
dc=data(read->(x1,x2),form->'B',in->'data.bin',maketrans->tr)
dc%keep;
dc%matrix;
delete_f('data.bin')
;return
;if(wait);pause
;return
 
 
;statex:  
** stat() computes minimums, maximums, means and std deviatons (line  21260 file c:/jlp22/j.f90)
data1=data(in->,read->(x1,x2,x3))
1,2,3
4,6,8
3,8,20
6,8,9
/
stat()
stat(data->data1,sum->x2,mean->,filter->(x3.le.18.5))
li=;list(x2%?);
@li;
stat(x1,data->data1,weight->x2)
stat(x1,weight->(x2**1.2))
;if(wait);pause
;return
 
 
;covex:  
** Example of covariance (line  21632 file c:/jlp22/j.f90)
X1=matrix(200)
X1=rann()
;do(i,2,6)
ad=matrix(200)
ad=rann()
X"i"=X"i-1"+0.6*ad
;enddo
Continue=1   !error
dat=newdata(X1...X6,read->(x1...x5))
Continue=0
dat=newdata(X1...X6,read->(x1...x6))
co=cov(x1...x5);
co=cov(dat%keep);
;if(wait);pause
;return
 
 
;matrixex:  
** Example of generatig matrices (line  21856 file c:/jlp22/j.f90)
A=matrix(3,
;if(wait);pause
;return
 
 
;transex:  
** Demonstrates also error handling (line  22669 file c:/jlp22/j.f90)
transa=trans()
$x3=x1+3
x2=2/$x3;
/
transa%input,transa%output,transa%source;
x1=8
call(transa)
transb=trans(x1,x2)
$x3=x1+3
x2=2/$x3;
x3=x1+x2+$x3;
/
transb%input,transb%output,transb%source;
call(transb)
transb|x3;   !x3 is now local
transc=trans()
x1=-3
call(transb) !this is causing division by zero
/
Continue=1   ! continue after error
call(transc)
;if(wait);pause
;return
 
 
;list2ex:  
** (line  22993 file c:/jlp22/j.f90)
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
** Example of Rscript (line  24264 file c:/jlp22/j.f90)
rscript=text()
# A simple R-script that generates a small data to file mydat.txt
wd<-"C:/jlp22/jmanual"
x<-runif(10,0,10)
y<-cbind(1,x)%*%c(1,2)+rnorm(10)
mydat<-data.frame(y,x)
write.table(mydat,file=paste(wd,"/mydat.txt",sep=""))
//
write('miniscript.r',rscript)
close('miniscript.r')
R('miniscript.r')
print('mydat.txt')
delete_f('mydat.txt','miniscript.r')
;if(wait);pause
;return
 
 
;diffex:  
** (line  25037 file c:/jlp22/j.f90)
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
** Derivatives with der() (line  25387 file c:/jlp22/j.f90)
transa=trans()
der(x)
f=(1+x)*cos(x)
/
fi=draw(func->transa(f\x),x->x,xrange->(0,10),color->Blue,continue->)
fi=draw(func->transa(f),x->x,xrange->(0,10),color->Cyan,append->,continue->fcont)
;if(wait);pause
;return
 
 
;derex2:  
** (line  25395 file c:/jlp22/j.f90)
X=matrix(do->(0,1000,10))
e=matrix(nrows(X))
e=rann(0,2);
A,Pmax,Res=0.1,20,2
A*Pmax*1000/(A*1000+Pmax);
Y=A*Pmax*X/.(A*X+Pmax)-Res+e  !rectangular hyperbola used often for photosynthesis
transa=trans()
der(A,Pmax,Res)
f=A*Pmax*I/(A*I+Pmax)-Res
/
fi=draw(func->(transa(f)),x->I,xrange->(0,1000),color->Orange,width->2,continue->,show->0)
da=newdata(X,Y,e,extra->(Regf,Resid),read->(I,P,er))
stat()
fi=plotyx(P,I,append->,show->0,continue->fcont)
A,Pmax,Res=0.07,17,3 !initial values
fi=draw(func->(transa(f)),x->I,xrange->(0,1000),color->Green,width->2,append->,show->0,continue->)
reg=nonlin(P,f,par->(A,Pmax,Res),var->,corr->,data->da,trans->transa)
reg%var;
reg%corr;
corrmatrix(reg%var);
fi=draw(func->(transa(f)),x->I,xrange->(0,1000),color->Violet,append->,continue->fcont)
;if(wait);pause
;return
 
 
;densityex:  
** Example of distributions (line  26243 file c:/jlp22/j.f90)
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
fi=drawclass(cl,continue->fcont)
fi=drawclass(cl,area->,continue->fcont)
fi=draw(func->pdf(x),x->x,xrange->,append->,continue->fcont)
f=matrix(1000)
f=rann()
da=newdata(f,read->x)
stat(min->,max->)
cl=classify(x->x,xrange->)
fi=drawclass(cl,histogram->,classes->20,continue->fcont)
den=density(cl);
fi=drawline(den,continue->fcont)
;if(wait);pause
;return
 
 
;jlpzex:  
** Problem with only z-variables. (line   2956 file c:/jlp22/jlp.f90)
probza=problem()
2*x1+x2+3*x3-2*x4+10*x5==min
x1+x3-x4+2*x5=5
x2+2*x3+2*x4+x5=9
x1<7
x2<10
x3<1
x4<5
x5<3
/
probzalist=;list(probza%?); !subobjects created
@probzalist; !printing the subobjects
jlpza=jlpz(problem->probza,dpivot->1)
jlpzalist=;list(jlpza%?);
@jlpzalist;
** The same problem is defined using different tools available.
	!
probzb=problem()
2*x1+x2+x34c*x34+10*x5==min
x1+x3-x4+(2+0)*x5=5
x2+2*x3+2*x4+x5=9
x1<7
x2<i10
x3<'1+zero'
x4<5
x5<3
/
x34=list(x3,x4)
x34c=matrix(2,values->(3,-2))
i10=10
zero=0
jlpzb=jlpz(problem->probzb,dpivot->1)
**Now different problem is obtained
x34c=matrix(2,values->(3,-3))
zero=1
jlpzb=jlpz(problem->probzb,dpivot->1)
**
**The matrices needed to use jlpz without problem-> can be obtained from a problem as follows
jlpcoefa=jlpcoef(probza)
jlpcoefalist=;list(jlpcoefa%?);
jlpza=jlpz(zmatrix->jlpcoefa%matrix,rhs->jlpcoefa%rhs,rhs2->jlpcoefa%rhs2,min->jlpcoefa%objective)
;if(wait);pause
;return
 
 
;jlpex:  
** jlp() solves linear programming problem (line   4088 file c:/jlp22/jlp.f90)
cdata=data(in->'cdat.txt',read->(cdata%nobsw,site))
stat()
xdata=data(in->'xdat.txt',read->(npv#0,npv#5,income1...income5),up->cdata)
stat()
proba=problem();
** In this problem the 4% net present value at the beginning is maximixe
** subject to the constraints telling that net incomes are nondecreasing
npv#0==max
;do(i,2,5)
income"i"-income"i-1"=0
;enddo
npv#5-npv#0>0
/
plist=;list(proba%?);
@plist;
jlpa=jlp(problem->proba,data->xdata,showdomain->('site.le.3','site.gt.3'))
** sums of x-variables are stored into the same variables with jlpa%prefix
jlpa%income5;
jlpa%income5[site.le.3];
** Note income variables are in theory equal but as their
** values are computed numerically, they differ.
jlist=;list(jlpa%?);
**  these could be printed with @jlist;
** jlpa%weights gets the weights of schdedules
** combain the weights with the data
xdataw=newdata(xdata,jlpa%weights,read->w)
stat(sum->)
**sum of weights is equal to the number of stands
w%sum;
** weighted statistics
** thesw agree with the jlp solution
stat(weight->w,sum->)
;do(i,1,len(xdataw%keep))
@xdataw%keep(i)%sum;
;enddo
***Problem with domains
probb=problem();
npv#0==max
**Domain definitions:
**there can be several domain definitions on a row
** one domain definition is:
**    a logical statement in terms of stand variable
**    a stand variable whose nonzero value implies that the domain applies
**    All  indicates all stands.
** before first domain definition row the default Domain is All
site.le.3: site.gt.3:
;do(i,2,5)
income"i"-income"i-1"=0
;enddo
npv#5-npv#0>0
/
plistb=;list(probb%?);
*** these  could be printed with @plistb;
jlpb=jlp(problem->probb,data->xdata,showdomain->'3.lt.site')
jlpb%income5;
jlpb%income5[site.gt.3];
jlpb%income5[3.lt.site];
Continue=1
** Now problem without constraints
probc=problem();
npv#0==max
/
jlpc=jlp(problem->probc,data->xdata,showdomain->'3.lt.site')
Continue=0
;if(wait);pause
;return
 
 
;factopt:  
** Factory optimization (in preparation) (line   4190 file c:/jlp22/jlp.f90)
**	The following example shows how the factory optimization will work.
** There are 10 stands
** coordinates of stands in 100 x 100 area
loc=matrix(10,2);
loc=ran();
loc=100*loc;
** numbers of schedules in each stand
ns=matrix(10,in->)
3
4
3
4
3
2
4
3
3
3
/
** x and y are coordinates of of stands
cdata=newdata(ns,loc,read->(cdata%nobsw,x,y))
stat(sum->)
** NS is total number of schedules generated randomly fro 5 periods
NS=cdata%nobsw%sum;
xmat0=matrix(NS)
xmat0=ran()
xmat0=10*xmat0
tr=trans()
;do(i,2,5)
x"i"=x"i-1"+ran()
;enddo
;do(i,1,5)
sawlog"i"=0.2*x"i"
pulplog"i"=0.3*sawlog"i"
;enddo
/
xdata=newdata(xmat0,read->x1,maketrans->tr)
stat()
linkdata(cdata,xdata)
** sawmill properties, X and Y are coordinates of sawmills
sawdata=datawcase(read->(X,Y,capacity1...capacity5,util1...util5))
S1,20,30,5,6,7,8,6,10,10,11,10,10,12
S2,70,60,6,6,7,8,6,10,12,13,13,11,15
S3,10,70,5,6,7,8,6,10,13,14,14,11,13
/
**pulpmill properties
pulpdata=datawcase(read->(X,Y,capacity1...capacity5,util1...util5))
P1,40,30,2,3,2,4,3,4,3,3,4,4,4
P2,80,70,2,3,2,4,3,6,4,4,5,5,5
P3,20,90,2,3,2,4,3,6,5,5,6,6,4
/
** sawlogs for different periods
sawlogs=list(sawlog1...sawlog5);
pulplogs=list(pulplog1...pulplog5);
** utility obtained from sawmills
sutil=table(sawdata%case,sawlogs);
** matrix to which utilities are computed
sutil%coef=matrix(len(sawdata%case),len(sawlogs))
** utility from pulpmills
putil=table(pulpdata%case,pulplogs);
** matrix to which utilities are computed
putil%coef=matrix(len(pulpdata%case),len(pulplogs))
scost=0.1  ! cost of transporting sawlogs, these could be period dependent
pcost=0.2  ! cost of transporting pulp logs
util=trans()
;do(i,1,len(sawdata%case))
;do(j,1,len(sawlogs))
*sawdata("i",util"j") is value of variable util"j" for observation i in saw mill data
sutil%coef("i","j")=sawdata("i",util"j")-
scost*sqrt((sawdata("i",X)-x)***2+(sawdata("i",Y)-y)***2)
;enddo
;enddo
;do(i,1,len(pulpdata%case))
;do(j,1,len(pulplogs))
putil%coef("i","j")=pulpdata("i",util"j")-
pcost*sqrt((pulpdata("i",X)-x)***2+(pulpdata("i",Y)-y)***2)
;enddo
;enddo
/
fprob=problem()
** utility from sawmills + utility from pulpmills
sutil+putil==max
;do(j,1,len(sawlogs))
;do(i,1,len(sawdata%case))
* capacity constraints
;if(sawdata("i",capacity"j").gt.0)@sawlogs("j")%2%@sawdata%case("i")<sawdata("i",capacity"j")
;enddo
;enddo
;do(j,1,len(pulplogs))
;do(i,1,len(pulpdata%case))
;if(pulpdata("i",capacity"j").gt.0)@pulplogs("j")%2%@pulpdata%case("i")<pulpdata("i",capacity"j")
;enddo
;enddo
/
li=;list(fprob%?);
@li;
f=jlp(problem->fprob,data->xdata,utiltrans->util,knn->2)
;return
;if(wait);pause
;return
 
  
;ALL:
inpuparag
shortex
dotsex
inpulistelem
sublistex
expandex
subobjex
subobjex2
subobjex2
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
congotoex
assignex
showex
errorex
inpuincl
inpugotoex
inpudoex
inpusumex
getset
list2ex
ilistex
typeex
deleteoex
deletfex
whichex
findex
newdataex
classdata
logisticex
datawcaseex
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
joindataex
partdataex
linkdataex
askex
inverseex
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
jlpzex
jlpex
factopt
;return
