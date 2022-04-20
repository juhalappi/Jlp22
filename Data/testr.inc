pert=trans()
one=1
/
!print(Names)
xd=data(in->'test.xda',read->(d2...d6,i1),maketrans->pert)
stat()
*print(Names)
stat(d2,d3,d4,d5,d6,i1)
* variables are: 
* i1= income in first period
* d2= income in second period - income in first period
* d3= income in third period - income in first (sic!) period
* d4,d5,d6= income in periods 4 5 6 - income in first period
!print(Names)
doma=0
ct=trans()
doma=Obs.gt.20
/
*doma=Obs.eq.5.or.Obs.gt.20

cd=data(in->'test.cda',read->(Ns),maketrans->ct,obs->Unit)
print(cd%keep)

*print(Names)
stat(Ns,doma)

*val=120000
val=0
prob=problem()
i1==max
*i1%%f1=100
*i1%%flist=30
d2="val"
d3="val"
d4="val"
d5="val"
d6="val"
z>10
/


*print(rhs%prob,rhs2%prob,rows%prob,domains%prob)
*print(vars%prob)
*print(Names)
linkdata(data->cd,subdata->xd,nobsw->Ns)
prob2=problem()
i1==max
/
*jlp(problem->prob2,data->cd)
prob2=problem()
i1==max
i1>0
/
*jlp(problem->prob2,data->cd)
*print(Names)

flist=list(f1,f3)
!print(Names)
cpu0=cpu()
jlp(problem->prob,data->cd,filter->(2.eq.2),z->)
stat(data->xd)
cpu1=cpu()
print('cpu',cpu1-cpu0)

;return
