!
! Jlp22 Software
!
! Copyright (C) 2022 Juha Lappi and Natural Resources Institute Finland (Luke)
! Author  Juha Lappi and Reetta Lempinen (in factory optimization)

! Juha Lappi has rewritten the most part after his retirement. Only the key steps
! of the simplex algorithm are as previously but the huge subroutine files  are
! divided into reasonable subroutines so that the flow of control can be managed. All variables needed for the communication
! between subroutines are put into modules from where te necessary use statements are
! generated with the precompiler jpre. The prefix for the variables needed
! have prefix p_ as compared to prefix j_ in other part of the software.
! The output has been made more reasonable. It is now possible to stop  iterations using stop->
! code option.
! The factory optimization is under restructuring and is not usable.
!-----------------------------------------------------------
!
! jlp.f90	All subroutines needed in jlp-functions
!
!-----------------------------------------------------------

!first modules which define Fletchers common areas
module fletdmod   !nopre!  !!!!
	integer, dimension(:),allocatable::lws
	integer, dimension(:),allocatable::lwsll1
	real*8, dimension(:),allocatable::ws
	real*8, dimension(:),allocatable::wslu1
	real*8, dimension(:),allocatable::e
	real*8, dimension(:),allocatable::r
	real*8, dimension(:),allocatable::g
end module !module fletdmod

module fletdmod2   !nopre!  !!!!
	use fletdmod
	contains
	subroutine initflet(nrow,ncol,a,la,lav,ls,lu1,ll1,ifail,nrefac)
		use fletcherdmod
		use jmod, only: p_mxlws,j_0
		implicit REAL*8 (a-h,o-z)
		!implicit REAL*8 (a-h,r-z), integer (i-q)
		integer lav(1)
		!nm=n+v
		! flletcher m =ncol
		!           n=nrow /  ei nrow+1, Fletcher ei näe objetitriciä ?
		integer ls(1:*)
		real*8 a(la,*)
		common /wsc/kk,ll,kkk,lll,mxws,mxlws  !?? piti poistaaa bqpd:stä
		! ongelma: datalla ei saa pistää arvoja kahdessa eri paikassa
		common /mxm1c/mxm1
		common/bqpdc/irh1,na,na1,nb,nb1,ka1,kb1,kc1,irg1,lu1fl,lv,lv1,ll1fl
		common/densec/ns,ns1,nt,nt1,nu,nu1,mx1,lc,lc1,li,li1
		common/noutc/nout
		common/factorc/m0,m1,mm0,mm,mp,mq
		common/epsc/eps,tol,emin
		common/repc/sgnf,nrep,npiv,nres
		common/refactorc/nup,nfreq


		eps=1111.D-19;tol=1.D-13; emin=j_0    ! was emin= 1.D0 corrected 4.5.2015 folowing the advice of Fletcher
		sgnf=1.D-4;nrep=2;npiv=3;nres=2;nfreq=nrefac;nup=0
		!	write(6,*)'hep'
		kk=0;ll=0
		kmax=0
		nm=nrow+ncol ;nmi=nm
		call  stmap(nrow,nm,kmax)
		!		write(6,*)'hep2,nrow,ncol,kmax,nm',nrow,ncol,kmax,nm
		mode=0;ifail=0
		mxm1=min(ncol+1,nrow)
		mx1=mxm1
		mxws= mxm1*(mxm1+1)/2+3*nrow+mxm1   !required by dense
		mxws=mxws+2*nrow+ncol
		if(allocated(ws))deallocate(ws)  !linux
		if(allocated(wslu1))deallocate(wslu1)  !linux
		lu1=lu1fl
		allocate( ws(1:lu1-1),wslu1(1:mxws-lu1+1))  !linux
		!	write(6,*)'<size wslu1************************',mxws,lu1,mxws-lu1+1
		mxlws= nrow+mxm1+nm  !     n+mxm1+n+m
		if(allocated(lws))deallocate(lws)         !    lo
		if(allocated(lwsll1))deallocate(lwsll1)
		if(allocated(e))deallocate(e)
		if(allocated(g))deallocate(g)
		if(allocated(r))deallocate(r)

		ll1=ll1fl          !get stmap
		p_mxlws=mxlws-ll1+1
		allocate(lws(1:ll1-1),lwsll1(1:p_mxlws),e(1:nm),g(1:nrow),r(1:nm))           !linux
		!	p_lws=>lwsll1(1:p_mxlws)
		!	write(6,*)'heptas',nrow,nm,nmi,nk
		nk=0
		call start_up(nrow,nm,nmi,a,lav,nk,e,ls,wslu1,lwsll1,&   !LWSLL1
			mode,ifail)
		!write(6,*)'heptasnyy',nrow,nm,nmi,nk
		return
	end subroutine initflet !subroutine initflet(nrow,ncol,a,la,lav,ls,lu1,ll1,ifail,nrefac)

	subroutine closeflet()
		use lastmod !nopre!
		if (allocated(ws))deallocate(ws)
		if(allocated(lws))deallocate(lws)
		if(allocated(e))deallocate(e)
		if(allocated(g))deallocate(g)
		if(allocated(r))deallocate(r)
		if(allocated(last))deallocate(last)
		return
	end subroutine closeflet!subroutine closeflet()

end module !module fletdmod2

!global j-variables
! !module jlpdmod    !!!!
! !contains   !!!!

! subroutine pullout(LWSLL1)
! ! logical er
! ! character*1 ch
! ! integer ipivotv
! integer,dimension(*)::LWSLL1
! ! !	write(6,*)'inbasis,p_pivot',p_pivot
! ! if(p_pivot.eq.ipivotv.and..not.j_err)return
! ! if(j_err.and..not.p_ispullout.or.p_ispullout.and.p_pivot.lt.p_pullout1)then
! ! write(6,*)'pullout with j_err=.true.'
! ! ch='1'
! ! else
! ! ch=char(49+p_pivot-p_pullout1)
! ! endif !if(j_err.and..not.p_ispullout.or.p_ispullout.and.p_pivot.    116
! call pullout0(LWSLL1)

! ! !write(6,*)'pullout pivot del ',p_pivot,p_pullout1,p_pivot-p_pullout1+1,ch,' feas ',p_feasible
! ! er=j_err
! ! j_err=.false.
! ! call j_command("po%misctitle"//ch//"='Pivot,objf,objfv,unit,lr0,lz0,ld0,tmax,icolold,icolnew,pivotcase,FEAS,nkeys'")
! ! call j_defmatrix(0,'po%misc'//ch,1,13,j_matreg,iv)
! ! j_o(iv)%d(1)=p_pivot
! ! j_o(iv)%d(2)=p_objf
! ! j_o(iv)%d(3)=p_objfv
! ! j_o(iv)%d(4)=p_unit
! ! j_o(iv)%d(5)=p_lr0
! ! j_o(iv)%d(6)=p_lz0
! ! j_o(iv)%d(7)=p_lx0
! ! j_o(iv)%d(8)=p_tmax
! ! j_o(iv)%d(9)=p_oldc
! ! j_o(iv)%d(10)=newc
! ! j_o(iv)%d(11)=p_pivotcase
! ! ifeas=0
! ! if(p_feasible)ifeas=1
! ! j_o(iv)%d(12)=ifeas
! ! j_o(iv)%d(13)=p_nkeys
! !j_yes=j_err
! p_nrow8=p_nrow
! j_err=.false.
! if(p_nrow.gt.0)then
! p_ivpomatrix=j_defmatrix(j_ivout,'%basis',p_nrow8,p_nrow8,j_matreg)
! ! call j_defmatrix(0,'po%xps'//ch,1,p_nrow+1,j_matreg,iv)
! ! j_o(iv)%d(1:p_nrow+1)=p_xps
! ! call j_defmatrix(0,'po%rowx'//ch,1,p_nrow+1,j_matreg,ivrow)
! ! j_o(ivrow)%d(1:p_nrow+1)=p_xps
! ! call j_defmatrix(0,'po%rhsw'//ch,1,p_nrow,j_matreg,ivrhs)
! p_ivpox=j_defmatrix(j_ivout,'%x',j_18,p_nrow8,j_matreg)
! !	p_ivpoobj=j_defmatrix(j_ivout,'%objrow',j_18,int8(p_nrow2z),j_matreg) !ivob)
! endif !if(p_nrow.gt.0)    172
! ! call j_defmatrix(0,'po%vc'//ch,1,p_nrow,j_matreg,ivc)

! if(p_xpresent.and.p_nrow.gt.0)then
! p_ivpounit=j_deflist(j_ivout,'%bunit',list0=p_nrow,ilist=.true.) !basic units
! !		p_ivpokey=j_deflist(j_ivout,'%key',list0=p_nrow,ilist=.true.)
! p_ivpos=j_deflist(j_ivout,'%s',list0=p_nrow,ilist=.true.)
! !	call j_deflist(j_ivout,'%baunit',p_ivpoibaunit,list0=p_nrow,ilist=.true.)

! ivpolist=j_deflist(j_ivout,'%list',list0=5)
! j_o(ivpolist)%i2(1:5)=(/p_ivpomatrix,p_ivpox,p_ivpoobj,p_ivpounit,p_ivpoibaunit/)
! else
! ivpolist=j_deflist(j_ivout,'%list',list0=3)
! j_o(ivpolist)%i2(1:3)=(/p_ivpomatrix,p_ivpox,p_ivpoobj/)
! endif !if(p_xpresent.and.p_nrow.gt.0)    184
! ! call j_defmatrix(0,'po%a'//ch,p_nrow,p_nrowz+1,j_matreg,iv)
! ! ibas=0
! ! do i=1,p_nrow
! ! j_o(iv)%d(ibas+1:ibas+p_nrowz+1)=p_a(i,0:p_nrowz)
! ! ibas=ibas+p_nrowz+1
! ! enddo !i=1,p_nrow    165

! ! ! j_o(iv)%d((i-1)*p_nrow+ls(i))=j_1
! ! ! j_o(ivx)%d(i)=p_x(p_ls(i))
! ! ! else
! ! ! do j=1,p_nrow
! ! ! j_o(iv)%d((j-1)*p_nrow+ls(i))=p_a(i,ld(i))
! ! ! enddo
! ! ! j_o(ivx)%d(i)=p_x(p_ls(i))
! ! ! endif

! ! ! enddo

! do i=1,p_nrow
! j_o(p_ivpox)%d(i)=p_x(p_ls(i))
! if(p_ls(i).le.p_nrow)then
! j_o(p_ivpomatrix)%d((p_ls(i)-1)*p_nrow+i)=j_1


! else
! do j=1,p_nrow
! j_o(p_ivpomatrix)%d((j-1)*p_nrow+i)=p_a(j+p_abas(p_ls(i)-p_nrow))

! enddo !j=1,p_nrow    221
! if(p_xpresent)then
! if(p_ls(i).gt.p_nrowz)then

! !	j_o(ivob)%d(p_lr0+p_lz0+i)=p_objr(p_nrowz+p_lx(i))
! iunit=p_lunit(p_ls(i)-p_nrowz)
! j_o(p_ivpounit)%i2(i)=iunit
! !j_o(p_ivpokey)%i2(i)=p_keys( iunit ) !p_keys(p_lunit(p_lcur))
! j_o(p_ivpos)%i2(i)=p_isch(p_ls(i)-p_nrowz)
! j_o(p_ivpoibaunit)%i2(i)=p_ibaunit( iunit)
! !		j_o(p_ivpoobj)%d(1)=j_o(p_ivpoobj)%d(1)+p_x(p_ls(i))*p_objr(p_ls(i))

! ! do j=1,p_nrow
! ! j_o(p_ivpoobj)%d(j)=j_o(p_ivpoobj)%d(j)+p_x(p_ls(i))*p_a(j+p_abas(p_ls(i)-p_nrow))

! ! enddo !j=1,p_nrow    223



! !ivrow

! endif !if(p_ls(i).gt.p_nrowz)    226
! endif !if(p_xpresent)    225
! endif !if(p_ls(i).le.p_nrow)    216

! !	j_o(p_ivpoobj)%d(i)=p_objr(p_ls(i))
! j_o(p_ivpox)%d(i)=p_x(p_ls(i))

! enddo !i=1,p_nrow    214

! j_o(p_ivpoobj)%d=p_objr

! end subroutine




subroutine problem(iob,io)   !new version old version is problem2  %%jlp  !!!!
	use jmod, only: j_o
	use jmod, only: j_otype
	use jmod, only: j_ipreal
	use jmod, only: j_del
	use jmod, only: j_ivresult
	use jmod, only: j_err
	use jmod, only: j_v
	use jmod, only: j_ivdollar2
	use jmod, only: p_p8
	use jmod, only: j_yes
	use jmod, only: j_isoption
	use jmod, only: j_mprint
	use jmod, only: j_ivprintinput
	use jmod, only: j_getname
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: p_ndoms
	use jmod, only: j_ninc
	use jmod, only: j_inciv
	use jmod, only: p_isdomain
	use jmod, only: j_getinput
	use jmod, only: j_countlim
	use jmod, only: j_inp
	use jmod, only: j_linp
	use jmod, only: j_deflist
	use jmod, only: p_ivrhsvars
	use jmod, only: p_ivrhsplus
	use jmod, only: p_ivrhs2vars
	use jmod, only: p_ivrhs2plus
	use jmod, only: j_deftext
	use jmod, only: p_ivrow
	use jmod, only: p_ivdomvars
	use jmod, only: j_putlistobject
	use jmod, only: j_ivall
	use jmod, only: j_ipilist
	use jmod, only: p_ivdomain
	use jmod, only: j_puttext
	use jmod, only: p_nrowtot
	use jmod, only: j_0
	use jmod, only: p_nrow
	use jmod, only: j_yes2
	use jmod, only: j_nextlim
	use jmod, only: j_checki
	use jmod, only: p_getrow
	use jmod, only: j_getobject
	use jmod, only: j_ipproblem
	use jmod, only: j_getline
	use jmod, only: j_tempchar
	use jmod, only: j_inlistobject
	use jmod, only: j_command
	use jmod, only: j_object
 
	!Section problem problem() PROB for jlp() and jlpz()
	! An LP-problem is defined in similar way as a TEXT object.
	!The following rules apply for problem rows:
	!\begin{itemize}
	!\item On the left there is any number of terms separated with + or -.
	!\item Each term is either a variable name or coefficient*variable.
	!\item A coefficient can be
	!\begin {itemize}
	! \item a number
	! \item Computation code inside parenthesis. These coefficients are computed within problem().
	! \item computation code within apostrophes. These coefficients are computed in jlp(), jlpz() or jlpcoef() functions.
	! \item  A legal name for an object.
	! \end{itemize}
	! \item The variable must be a legal object name.
	! The optimization variable can either be a z-variable or x-variable.
	!A x-variable is an variable in schedules data set.
	! In jlpz() all variables are z variables. Function jlpz() unpacks lists to z-variables.
	!\item The right side of the first row ends either by ==min or ==max.
	!\item On the right side for other rows there is a number or code for computing a numeric value
	! within parnthesis or within apostrophes or a variable name. Numbers within parenthesis are
	! computed within problem() but numbers within apostrophes are computed in jlp(), jlpz() or jlpcoef() functions.
	!Between the left side and the right side there is\\
	! \begin{itemize}
	! \item  >Low <Up
	! \item >Low
	!\item <Up
	! \item = Value
	! \end{itemize}
	!\end{itemize}
 
	!Low, Up and Value can be\\
	! \begin{itemize}
	! \item Numeric constant
	! \item Text within apostrophes. This value is interpreted later in jlp() or jlpz()
	!\item A REAL variable. The value is looked later in jlp() or jlpz()
	! \item A REAL variable preceded by '-' or '+'. The value is obtained in jlp() or jlpz()
	!\item Text with or without surrounding parenthesis. This value is computed now.
	! \end{itemize}
 
 
	! Sign < means less or equal, and > means greater or equal. Pure less or greater would be meaningless in this context.\\
	!If there two different identical rows
	! the other having '<' and the other '>', an error occurs, because the solution is obtained faster that way.
	! If all rows have both lower limit and upper limit, the solution is obtained in half time
	! 	when merging the lines.
 
	! In problems with x-data, there can be domain rows, which tell for what subset
	! of the treament units the following constraints apply.
	! Domains are defined using c-variables, i.e. variables in the unit data,
	! or in nonhierarchical, flat data set, the value of the c-variable is obtained
	!from the first observation where the variable given in unit-> gets a different
	!value than in the previous observation. The variables in the flat data file
	! having the same value for all observations in the same unit are called also c-variables.
	!	Later there will be variables related to factory problems.
	!A domain definition ends with ':'. In a domain row there can be any number of domain definitions.
	!There are three diffent kinds of domain definitions\\
	!All indicates all units. This domain is assumed to all rows before the first domain row.\\
	!c-variable, a nonzero value tells that the unit belongs to the domain.\\
	! A piece of code which tells how the  indicator is computed from the c-variables. A nonzero value indicates
	! that the unit belongs to the domain. Recall tha logical operations produce 1 for True and 0 for false.
	! The code is parsed at this point, so syntax errors are detected at this point,
	! but other errors (e.g. division by zero) are detected in jlp().
 
	!If  a coefficient is a column vector and the variable is a x-variable in the objective row, this
	!indicate a piecewise linear objective. The if the value of the x-varaible is smaller than
	! the first value in the vector, the second value gives the coeffient. If
	! the x-variable is between the first and third value, then the fourth value gives the coeffient which applies after the third value.
	! The value is obtained using the value at the first knot plus the value coming from the second section.
	! A nonlinear objective can be devloped quite easily using the same data structures and the derivative
	! computations already present in Jlp22. 	An example is presented for optimization of sawmill sets, for
	! which also the new properties of data() function are developed.
 
 
	! endheader
	! Option
	! Output &1 & PROB & the PROB object created
	! print &0|1 & REAL & If print-> gives a value, then values >2 tell that the problem is printed (default)
	!endoption
	!Note Examples are give in connection of jlpz() and jlp().
	!endnote
	!Note the cofficients in a PROB can be interpreted also
	! using jlpcoef() function, which is used also by jlp() and jlpz() functions.
	!endnote
	!Note
	! Problems without x-variables can be solved also without problem() function by
	! feeding in the necessary matrices.
	!endnote
 
	!endsection
 
 
	! ! !Exx jlpex1 Example of ordinary Lp-problem
	! ! !prob1=problem()
	! ! !2*x1+x2+3*x3-2*x4+10*x5==min
	! ! ! x1+x3-x4+2*x5=5
	! ! !x2+2*x3+2*x4+x5=9
	! ! !x1<7
	! ! !x2<10
	! ! !x3<1
	! ! !x4<5
	! ! !x5<3
	! ! ! /
	! ! !prob1list=;list(prob1%?); !subobject created
	! ! !@prob1list; !printing the subobjects
	! ! !jlp1=jlpz(problem->prob1)
	! ! !enxdex
 
 
	! ! !Exx jfjfj
	! ! !xd=data(read->(unit,X),in->)
	! ! !1,2
	! ! !1,0
	! ! !2,5
	! ! !2,5
	! ! !2,0
	! ! !3,0
	! ! !3,3
	! ! !3,5
	! ! !/
 
 
	! ! !jlp1list=;list(jlp1%?);
	! ! !@jlp1list;
	! ! !endex
	! ! !Ex jlpex2 example of problem() and jlp() in forest data
	! ! !xd=data(in->'test.xda',read->(d2...d6,i1))
	! ! ! **see all sub objects created
	! ! ! xd%?;
 
	! ! ! Maxlines=4
	! ! ! ** print all subobjects created up to line 4
	! ! ! @xd%?;
 
	! ! ! jlp2list=;list(xd%?);
	! ! ! @jlp2list;
 
	! ! ! stat()
	! ! ! ** variables are:
	! ! ! ** i1= income in first period
	! ! ! ** d2= income in second period - income in first period
	! ! ! ** d3= income in third period - income in first (sic!) period
	! ! ! ** d4,d5,d6= income in periods 4 5 6 - income in first period
 
	! ! ! **make indicator variable for a domain
	! ! ! doma=0
	! ! ! ct=trans()
	! ! ! doma=Unit.gt.20
	! ! ! /
 
	! ! ! cd=data(in->'test.cda',read->(Ns),maketrans->ct,obs->Unit)
	! ! ! cd%?;
	! ! ! stat()
 
	! ! ! ** link data sets, Ns is the number of schedules in each unit
 
	! ! ! linkdata(data->cd,subdata->xd,nobsw->Ns)
 
	! ! ! prob2=problem() ! timber management planning problem
	! ! ! npv#0==max
	! ! !;do(i,1,3)
	! ! !i"i+1"-i"i"=0
	! ! !;enddo
	! ! ! /
	! ! !problist2=;list(prob2%?);
	! ! !@problist2;
	! ! !jlp2=jlp(data->cd,problem->prob2)
	! ! !jlp2list=;list(jlp2%?)
	! ! !@jlp2list;
 
	! ! ! prob3=problem() ! timber management planning problem with domains
	! ! ! npv#0==max
	! ! !Unit.le.10:doma:
	! ! !;do(i,1,3)
	! ! !i"i+1"-i"i">0
	! ! !;enddo
	! ! !All:
	! ! !i4-i1=0
	! ! ! /
	! ! !problist3=;list(prob3%?);
	! ! !@problist3;
	! ! !jlp3=jlp(data->cd,problem->prob2)
	! ! !jlp3list=;list(jlp2%?)
	! ! !@jlp3list;
	! ! !endex
	! ! !endsection
 
	!Section problem2 vnhaa
	! In the above example domain7 is a data variable. Unit belongs to domain if the value of the
	! variable domain7 is anything else than zero.
	! The objective row must be the first row. The objective must always be present. If the purpose
	! is to just get a feasible solution without objective, this can be obtained by minimizing a zvariable which does not otherwise appear in the problem (remember
	! function.
	! In problems having large number of variables in a row it is possible to give the coefficients as
	! a vector and variables as a list e.g. \
	! coef1*var1+coef2*var2 <5 \
	! here coef1 is matrix having as many elements as
	! In problems with x-variables it is possible to maximize or minimize the objective without any
	! constraints. In factory problems this would also be quite straightforward to implement, but i
	! In problems with x-variables it is possible to maximize or minimize the objective without any
	! constraints. In factory problems this would also be quite straightforward to implement, but it
	! does not come as a side effect of computations as in the case of maximization of x-variables,
	! and thus it has not been implemented. The maximization of a factory objective without
	! constraints can be obtained by adding to the problem constraints which require that the
	! amounts of transported timber assortments to different factories are greater than or equal to
	! zero.
	! Function problem() interprets the problem paragraph, and extracts the coefficients of
	! variables in the object row and in constraint rows. The coefficients can be defined using
	! arithmetic statements utilizing the input programming "-sequence or enclosing the coefficient
	! in parenthesis. The right hand side can utilize arithmetic computations without parenthesis.
	! The values are computed immediately. So if the variables used in coefficients change their
	! values later, the problem() function must be computed again in order to get updated
	! coefficients. Note that a problem definition does not yet define a JLP task. Final interpretation
	! is possibly only when the problem definition and simulated data are linked in a call to jlp()
	! function. At the problem definition stage it is not yet known which variables are z-variables,
	! which are x-variables and which are factory variables (see Lappi 1992).
	! Note# that ‘<’ means less or equal, and ‘>’ means greater or equal. The equality is always
	! part of linear programming constraints.
	! The logic of jlp() function is the same as in the old JLP software. There is one difference
	! which makes the life a little easier with Jlp22.  In Jlp22 the problem definition can use c-variables which
	! are defined in the stand data. These are used similarly as if they would become from the xdata. It does not make any sense to have on a problem row only c-va
	! constraints like
	! vol#1-vol#0>0
	! where vol#0 is the initial volume, i.e. a c-variable, and vol#1 is the volume during first period.
	! In old JLP these initial values had to be put into the x-data.
	! Note# also that problem definition rows are not in one-to-one relation to the constraint rows in
	! the final lp problem. A problem definition row may belong to several domains, thus several lpconstraint rows may be generated from one problem definition row
	! taking multiple domains in domain definition rows into account is called ‘expanded problem’.
	! Domain definitions describe logical or arithmetic statements indicating for what management
	! units the following rows apply. Problem will generate problem definition object, which is
	! described below.
	! Starting from J3.0 it is also possible to specify the period of the row for each row containing xvariables. The period is given between two ‘#’ signs at the
	! #2# income.2-income.1 >0
	! If the row contains x variables from several periods, the period of the row is the last period of
	! the x variables. If the period is given for some rows containing x variables, it must be given for
	! all except for the objective row. The period of the objective is assumed to be the last period as
	! having any other period for the objective would not make any sense. If wrong period is given
	! for a row, Jlp22 computes the correct solution but not as efficiently as with correct periods.
	! If periods are given for rows, Jlp22 is utilizing the tree structure of schedules in the optimization.
	! This leads to smaller amount of additions and multiplications as the computation of the valu
	! of a branch of the tree can for each node utilize the value of branch before that node.
	! Unfortunately this was not more efficient e.g. in test problems with five periods.
	!endheader
	! Note  Only maximization is allowed in problems including factories. To change a
	! minimization problem to a maximization problem, multiply the objective function by -1.
	!endnote
 
 
	! Note  If optimization problem includes factories
	!  there have to be variables in the objective function or at least in one
	! constraint row.
	!endnote
	! Note  An ordinary linear programming problem contains only z-variables.
	!endnote
	! Note It is not necessary to define problem() function if the problem includes only zvariables.
	! In jlp() function you can use zmatrix-> option instead of
	! For more information see chapter 11.8 Solving a large problem with z-variables: jlp( ).
	!endnote
	! Note If the problem contains harvest/area constraints for several domains, it saves memory,
	! if the constraints are written in form
	! harvest < area_of_domain*constant
	! instead of
	! (1/area_of_domain)*harvest < constant.
	! The latter formulation takes the number of domains times more memory than the former
	! formulation.
	!endnote
	! endsection
 
 
	! domvars%.. added as a part of the problem object Feb. 2011
 
 
	!	double precision, dimension(:),pointer::rhs_   !lower bounds
	!	double precision, dimension(:),pointer::rhs2_  !upper bounds
	!integer, dimension(:), allocatable::isetd  ! nsetr,nsetd pois
	integer, dimension(:), allocatable:: pvars,varofterm,coefvars,isplus
	!logical, dimension(:), allocatable:: isplus
	integer, dimension(:),allocatable :: nterminrow
	!	double precision, dimension(:),allocatable::coef
	!double precision coe
	integer ::ivdefrow=0,ndefrow
	integer ::ivdefdomain=0,ndefdomain=0
	logical repeatdom,newprice
 
	character*30 ::probname
	!logical isminus
	!logical isdomrow
 
	!tree structure
	integer ,dimension(:), allocatable :: iperiods
	!integer ,dimension(:), allocatable::ivdomains
	!	logical isobjective
 
	!coefficients in vector, variablese in list
	!	logical :: coevec_ = .false.
	logical :: varlist_ = .false.
	logical printline
 
	ivproblem=j_o(iob)%i(io+2+j_o(iob)%i(io+1))
	if(j_otype(ivproblem).ne.j_ipreal)call j_del(ivproblem)
	if(ivproblem.eq.j_ivresult)then
		write(*,*)'problem must have output'
		j_err=.true.
		return
	endif !if(ivproblem.eq.j_ivresult)    541
	!if(.not.allocated(isetd))allocate(isetd(1:1000))
	if(.not.allocated(pvars))allocate(pvars(0:1000))  !problem variables
	pvars(0)=0
	allocate(varofterm(1:5000),nterminrow(1:500),isplus(1:5000),coefvars(1:5000))
	! write(6,*)'allo varofterm'
	isplus=0
	p_p8=j_v(j_ivdollar2).eq.8.d0
	nterminrow=0
	!if(.not.allocated(ivdomains))allocate(ivdomains(1:500))
	!	p_isobjective=.false.
	newprice=.true.
	newc=0
	inprint=0
	!how many domains are in one set, or in one row set
 
	!	write(6,*)'PROB'
 
	j_yes=j_isoption(iob,io,j_mprint)
	if(j_yes.or.j_v(j_ivprintinput).ge.3)inprint=1
 
 
	!!call j_clearoption(iob,io)  ! problem(iob,io)
 
	call j_getname(ivproblem)
	probname=j_oname
	lprobname=j_loname
 
 
	p_ndoms=0  !counting all occurences
 
	if(j_ninc.eq.1)then
		write(6,*)'*problem must be in incl-file'
		j_err=.true.
		return
	endif !if(j_ninc.eq.1)    576
	iiv=j_inciv( j_ninc)
	p_isdomain=.false.
 
	!go through problem paragraph
	!	j_o(iiv)%i(6)=j_o(iiv)%i(6)+1
	!	j_incline(j_ninc)=j_incline(j_ninc)+1
 
	iline=j_o(iiv)%i(6)
	!domains in section
	ic=1
 
	lendom=3  !for All
	call j_getinput('prob> ',single=.true.)
	!write(6,*)'<33>',j_inp(1:j_linp)
	if(j_err)return
	! write(6,*)'<1>',j_inp(1:j_linp)
	ic0=j_countlim(j_inp,j_linp,':')
	lendom=3
	j_yes=.false.
	if(ic0.gt.1)then
		write(6,*)'objective row cannot be in many domains'
		j_err=.true.;return
 
	elseif(ic0.eq.1)then
		lendom=lendom+j_linp
		call j_getinput('prob> ',single=.true.)
		!write(6,*)'<4>',j_inp(1:j_linp)
		if(j_err)return
		j_yes=.true.
	endif !if(ic0.gt.1)    600
	linptot=j_linp
	!		ial=max(linpr-4,1)
	if(j_inp(j_linp-4:j_linp).eq.'==min')then
		iobjtype=-1
	elseif(j_inp(j_linp-4:j_linp).eq.'==max')then
		iobjtype=1
	else
		write(6,*)'*no objective, you can find feasible using artificial objective: anything==max'
		j_err=.true.;return
	endif !if(j_inp(j_linp-4:j_linp).eq.'==min')    613
	p_ndoms=1
	nrow=-1
 
	do while(j_inp(1:j_linp).ne.'/')
 
		! at first round it is checked the number of lines and domains
 
		! write(6,*)'<88>',j_linp   !j_inp(1:j_linp)
		! write(6,*)'<88>',j_linp
 
		! !write(6,*)'ial',ial,linpr,j_inpr(1:linpr)
 
		ic0=j_countlim(j_inp,j_linp,':')
		if(ic0.gt.0)then
			ic=ic0
			p_ndoms=p_ndoms+ic0
			lendom=lendom+j_linp
		else
			linptot=linptot+j_linp
			nrow=nrow+ic
		endif !if(ic0.gt.0)    634
		!write(6,*)'nrow ',nrow
		!write(6,*)'ic0,ic,nrows,lendom ',ic0,ic,nrows,lendom
		call j_getinput('prob> ',single=.true.)
		!write(6,*)'<55>',j_inp(1:j_linp)
		if(j_err)return
		!	write(6,*)'<2>',j_inp(1:j_linp)
	enddo !while(j_inp(1:j_linp).ne.'/')    624
 
 
	p_isdomain=p_ndoms.gt.1
	!if(p_isobjective)then
	nrowtot=nrow+1
	!write(6,*)'nrow ',nrow
	! else
	! nrowtot0=nrows+1
	! nrow=nrows
	! endif !if(p_isobjective)    561
	!	write(6,*)'prob1',nrowtot,linptot,ivproblem
	!	call j_defmatrix(ivproblem,'%rhs',nrow,1,j_matreg,p_ivrhs)
	if(nrow.gt.0)then
		p_ivrhsvars=j_deflist(ivproblem,'%rhsvars',list0=nrow)
		p_ivrhsplus=j_deflist(ivproblem,'%rhsplus',list0=nrow,ilist=.true.)
 
		!	rhs_=>j_o(ivrhs)%d
 
		!	call j_defmatrix(ivproblem,'%rhs2',nrow,1,j_matreg,p_ivrhs2)
		p_ivrhs2vars=j_deflist(ivproblem,'%rhsvars2',list0=nrow)
		p_ivrhs2plus=j_deflist(ivproblem,'%rhs2plus',list0=nrow,ilist=.true.)
	endif !if(nrow.gt.0)    661
	!	rhs2_=>j_o(ivrhs2)%d
 
	p_ivrow=j_deftext(ivproblem,'%rows',nrowtot,linptot)   ! rows of the problem, text object
	!	write(6,*)'p_ivrow',p_ivrow
	!	call j_getname(p_ivrow)
	!	write(6,*)j_oname(1:j_loname)
	!write(6,*)'p_isdomain ',p_isdomain
	if(p_isdomain)then
		p_ivdomvars=j_deflist(ivproblem,'%domvars',nres=p_ndoms)
		iper=j_putlistobject(p_ivdomvars,single=j_ivall)
		ivrowdomvar=j_deflist(ivproblem,'%rowdomain',nres=nrowtot)
		ivrowdomnum=j_deflist(ivproblem,'%rowdomnum',nres=nrowtot,ilist=.true.)
		j_otype(ivrowdomnum)=j_ipilist
		ndefdomain=100
 
		p_ivdomain=j_deftext(ivproblem,'%domains',ndefdomain,lendom) !domains of the problem,
		if(.not.j_yes)call j_puttext(p_ivdomain,'All') !cleantext cannot be used
		ivdomaintext=j_deftext(0,'$domaintext',ndefdomain,lendom+10*p_ndoms)
		!write(6,*)'<4747 ivdomaintext,p_ndoms ',ivdomaintext,p_ndoms
		!	call j_putnewcleantext(ivdomain,'All',ilin)
	endif !if(p_isdomain)    678
 
	ipv=0
	ndom=0  !counting separate domains, All not counted?
	nvar=0
	nterm=0
 
	!	irowobj=0 !row where objective is
	iall=0
	idom=j_ivall
	ialdom=1
	!write(6,*)'P-isdomain ',p_isdomain
	!	iobjtype=0  !object type 0 no objective 1 is max -1 min
	p_nrowtot=0
 
	!iline=j_o(iiv)%i(6)
	j_o(iiv)%i(6)=iline
	printold=j_v(j_ivprintinput)
	j_v(j_ivprintinput)=j_0  !do not print lines twice
 
 
 
	!domains in section
 
	ic=1
	linptot=0
	lendom=3  !for All
	p_nrow=-1
	if(p_p8)write(6,*)'hep2'
 
	j_yes=.false. !line already ontained
	j_yes2=.true.  !inital All section
	! ***************************
	nlin=0
	!write(6,*)'ilineinit ',ilineinit
mainloop:	do while(.true.)
		call j_getinput('prob> ',0,single=.true.)
		!	write(6,*)'<66>',j_inp(1:j_linp),j_err
		if(j_err)return
		!write(6,*)'<4> ',j_yes, j_inp(1:j_linp),p_isdomain
		!	j_yes=.false.
 
		!if(p_isdomain)then
		if(.not.p_isdomain.and.j_inp(1:j_linp).eq.'/')exit
 
		if(p_isdomain)then
			if(j_inp(1:j_linp).eq.'/'.and.ic0.le.1)exit
			ic00=j_countlim(j_inp,j_linp,':')
			if(ic00.gt.0)irow0=p_nrow
			!	write(6,*)'ic00,ic0',ic00,ic0,'irow0 ',irow0
			if((ic00.gt.0.or.j_inp(1:j_linp).eq.'/').and.ic0.gt.1)then
				!if(ic.gt.1)then
				nterm2=nterm
				!write(6,*)'nterm1,nterm2 ',nterm1,nterm2,ic0
				ntermd=nterm2-nterm1+1
				do ic=2,ic0
					j_o(iiv)%i(6)=ilineinit-1   !note
					!write(6,*)'ilineinit ',ilineinit
					call j_getinput('prob> ',0,single=.true.)
					!	write(6,*)'<773>',j_inp(1:j_linp)
					if(j_err)return
					ip=j_nextlim(j_inp,iald,j_linp,':')
					!write(6,*)'ic,iald',ic,iald,ip
					!write(6,*)'gotnextdomain ',j_inp(1:j_linp),' domain ',j_inp(iald:ip-1),' nlin ',nlin
					call getdomain(j_inp(iald:ip-1),ivdomaintext,ndom,idom,idomnum)
					write(6,*)'ndom ',ndom,j_inp(iald:ip-1)
					!		call j_puttext(p_ivdomain,j_inp(iald:ip-1))
					iald=ip+1
					do ili=1,nlin
						call j_getinput('prob> ',0,single=.true.)
						!write(6,*)'<388>',j_inp(1:j_linp)
						if(j_err)return
						!write(6,*)'nyttul ',j_inp(1:j_linp),' idom',idom
						call j_puttext(p_ivrow,j_inp(1:j_linp))
						ipe=j_putlistobject(ivrowdomvar,single=idom,append=.true.)
						ipe2=j_putlistobject(ivrowdomnum,single=idomnum,append=.true.)
 
					enddo !ili=1,nlin    759
					!write(6,*)'nterm1,nterm2,ntermd,nterm',nterm1,nterm2,ntermd,nterm
 
					!write(6,*)
 
					nterminrow(p_nrowtot+1:p_nrowtot+nlin)=nterminrow(nrow0+1:nrow0+nlin)
 
					!		write(6,*)'nrow',p_nrow,j_inp(1:j_linp)
					p_nrowtot=p_nrowtot+nlin
					p_nrow=p_nrowtot-1
					!	call j_checkd(coef,nterm2)
					call j_checki(varofterm,nterm2)
					call j_checki(coefvars,nterm2)
					call j_checki(isplus,nterm2)
					!	coef(nterm+1:nterm+ntermd)=coef(nterm1:nterm2)
					varofterm(nterm+1:nterm+ntermd)=varofterm(nterm1:nterm2)
					coefvars(nterm+1:nterm+ntermd)=coefvars(nterm1:nterm2)
					isplus(nterm+1:nterm+ntermd)=isplus(nterm1:nterm2)
 
					!			write(6,*)'ic,nlin,irow0',ic,nlin,irow0,irow0+(ic-1)*nlin+1,irow0+ic*nlin,irow0+1,irow0+nlin
					j_o(p_ivrhsvars)%i2(irow0+(ic-1)*nlin+1:irow0+ic*nlin)=j_o(p_ivrhsvars)%i2(irow0+1:irow0+nlin)
					j_o(p_ivrhs2vars)%i2(irow0+(ic-1)*nlin+1:irow0+ic*nlin)=j_o(p_ivrhs2vars)%i2(irow0+1:irow0+nlin)
				enddo !ic=2,ic0    746
 
 
 
				nterm=nterm+ntermd
				!		j_o(p_ivrhs)%d(irow0+(ic-1)*nlin+1:irow0+ic*nlin)=j_o(p_ivrhs)%d(irow0+1:irow0+nlin)
				!	j_o(p_ivrhs2)%d(irow0+(ic-1)*nlin+1:irow0+ic*nlin)=j_o(p_ivrhs2)%d(irow0+1:irow0+nlin)
 
 
 
				call j_getinput('prob> ',0,single=.true.)
				!	write(6,*)'<399>',j_inp(1:j_linp)
				if(j_err)return
				!write(6,*)'gotagain ',j_inp(1:j_linp)
				!if(j_inp(1:j_linp).eq.'/')exit
				!endif !if(ic.gt.1)    675
 
			endif !if((ic00.gt.0.or.j_inp(1:j_linp).eq.'/').and.ic0.gt.1)    741
			if(j_inp(1:j_linp).eq.'/')exit
			if(ic00.eq.0.and.j_yes2)then
				p_ndoms=1
				!		call j_clean(j_inprp,linpr)  !it is possible to have
				!		ial=max(linpr-4,1)
				!	p_isobjective=j_inp(j_linp-4:j_linp).eq.'==min'.or.j_inp(j_linp-4:j_linp).eq.'==max'
				!	ic0=1   !All
 
				ic0=1
				!	write(6,*)ic0
 
				call getdomain('All',ivdomaintext,ndom,idom,idomind)
				idomnum=1
 
 
			elseif(ic00.gt.0)then
 
				! if(ic0.gt.1)call j_getinput('prob> ',10,single=.true.)
				! if(ic0.gt.1)write(6,*)'gothere ',j_inp(1:j_linp)
 
				ic0=ic00
				j_yes2=.false.
				!write(6,*)'ic0 tas ',ic0
				nterm1=nterm+1
				nrow0=p_nrowtot
				nlin=0
				!			ndom=ndom+1
				ip=j_nextlim(j_inp,1,j_linp,':')
 
				call getdomain(j_inp(1:ip-1),ivdomaintext,ndom,idom,idomnum)
				!write(6,*)'igotdomain,ndom,idom ',j_inp(1:ip-1),ndom,idom,j_inp(1:j_linp)
				if(j_err)return
				iald=ip+1
				!	ip=j_nextlim(j_inp,iald,j_linp,':')
				if(ic00.gt.1)then
					ilineinit=j_o(iiv)%i(6)
					!write(6,*)'ilineinit ',ilineinit
				endif !if(ic00.gt.1)    842
				cycle mainloop
 
			endif !if(ic00.eq.0.and.j_yes2)    809
 
		end if !if(p_isdomain)    736
		!here ordinary proble row
		!	do while(.true.)
		nlin=nlin+1
		!	write(6,*)'prob2'
		p_nrow=p_nrow+1
 
		p_nrowtot=p_nrow+1
		!write(6,*)'nlintas ',nlin,'p_nrowtot ',p_nrow, ' puttext ',j_inp(1:j_linp)
		call j_puttext(p_ivrow,j_inp(1:j_linp)) ! put row text into the 'rows%'...  object.
		if(p_isdomain)then
			ipe=j_putlistobject(ivrowdomvar,single=idom,append=.true.)
			ipe2=j_putlistobject(ivrowdomnum,single=idomnum,append=.true.)
		endif !if(p_isdomain)    860
 
		!write(6,*)'nterm,nterm1 ,p_nrow',nterm,nterm1,p_nrow,j_inp(1:j_linp)
 
		le=j_nextlim(j_inp,1,j_linp,'=<>')-1
		if(le.ge.j_linp)then
			write(6,*)'*rhs missing in:',j_inp(1:j_linp)
			j_err=.true.;return
		endif !if(le.ge.j_linp)    868
		!	write(6,*)'hep'
		if(p_nrow.gt.0)call getrhs(le)
 
 
 
		!	write(6,*)'<344343>'
		call p_getrow(le,nterminrow,nterm,varofterm,pvars,coefvars,isplus)
		!	write(6,*)'<34434399>'
		!write(6,*)'<aftergetrow,888,nterm ',nterm,j_inp(1:j_linp),'p_nrowtot ',p_nrowtot
		if(j_err)return
 
 
		! if(ic0.eq.1)j_yes=.true.  !next line obtained
	enddo  mainloop !nloop:	do while(.true.)    726
	!enddo icloop !oop:	do ic=1,ic0    678
	!write(6,*)'<5555'
 
	!write(6,*)'ivproblem',ivproblem
	ivoout=j_getobject(ivproblem,' ',j_ipproblem)   !miksei ivproblem käy suoraan, kunhan typen määrittelsi?
	!	write(6,*)'ivout,ivproblem',ivout,ivproblem
	ih=  20        !
	allocate(j_o(ivproblem)%i(1:ih))  !
	j_o(ivproblem)%i=0
 
	!	write(6,*)'prob3'
 
	nvar=pvars(0)
 
 
 
	j_o(ivproblem)%i(1)=p_ivrhsvars
	!	j_o(iv)%d(1:nrowtot)=rhs_(1:nrowtot)
	!	deallocate(rhs_)
 
	if(p_p8)write(6,*)'hep'
	j_o(ivproblem)%i(2)=p_ivrhs2vars
 
	!	write(6,*)'p_ivrhsvars,p_ivrhs2vars',p_ivrhsvars,p_ivrhs2vars
	!	j_o(iv)%d(1:nrowtot)=rhs2_(1:nrowtot)
	!	deallocate(rhs2_)
	!write(6,*)'nvar ',nvar
	ivvars=j_deflist(ivproblem,'%vars',list0=nvar,list=pvars(1:nvar)) !p-variables
	j_o(ivproblem)%i(3)=ivvars
	!	deallocate(pvars) !all variables of the problem
 
	j_o(ivproblem)%i(4)=p_ivrow  !text for rows
 
	j_o(ivproblem)%i(5)=iobjtype !-1 min 0 no objective 1 max
	!	write(6,*)'iobjtype ++++++++++++++',iobjtype,p_nrowtot
	! if(p_nrowtot.ne.nrowtot0)then
	! !write(6,*)'p_nrowtot,nrowtot0 ',p_nrowtot,nrowtot0
	! stop 'p_nrowtot'
	! endif !if(p_nrowtot.ne.nrowtot0)    869
 
	ibas=nterminrow(1)+nterminrow(2)
	do iro=3,p_nrowtot
 
		ibas1=nterminrow(1)
		!	write(6,*)'iro ibas,ibas1 ',iro,ibas,ibas1
		do iro1=2,iro-1
			if(nterminrow(iro).ne.nterminrow(iro1))goto 200
			if(p_isdomain)then
				if(j_o(ivrowdomvar)%i2(iro).ne.j_o(ivrowdomvar)%i2(iro1))goto 200
			endif !if(p_isdomain)    933
			do j=1,nterminrow(iro)
				if(varofterm(ibas+j).ne.varofterm(ibas1+j))goto 200
				if(coefvars(ibas+j).ne.coefvars(ibas1+j))goto 200
				!	if(coef(ibas+j).ne.coef(ibas1+j))goto 200
			enddo !j=1,nterminrow(iro)    936
			call j_getline(p_ivrow,iro,j_tempchar,le)
			ir=j_nextlim(j_tempchar,1,le,'=><')
			write(6,*)j_tempchar(1:ir-1)
			write(6,*)'constraint rows ',iro1-1,iro-1,' are equal, put >low <up to same row'
			write(6,*)' '
			j_err=.true.
200			ibas1=ibas1+nterminrow(iro1)
		enddo !iro1=2,iro-1    931
		ibas=ibas+nterminrow(iro)
	enddo !iro=3,p_nrowtot    927
	if(j_err)return
	write(6,*)'number of constraints ',p_nrow,' total number of elements ',nterm
	iv=j_deflist(ivproblem,'%rowofterm',list0=nterm,ilist=.true.) !row of each term
	!if(allocated(p_xpsrow))deallocate(p_xpsrow);allocate(p_xpsrow(1:p_nrow))
	it=0
	do ir=1,p_nrowtot
		do j=1,nterminrow(ir)
			it=it+1
			j_o(iv)%i2(it)=ir-1
 
		enddo !j=1,nterminrow(ir)    957
	enddo !ir=1,p_nrowtot    956
	j_o(ivproblem)%i(6)=iv
 
	!write(6,*)'%%%%%%%%%%% ',p_nrowtot,p_nrowtot
	iv=j_deflist(ivproblem,'%nterminrow',list0=p_nrowtot,list=nterminrow(1:p_nrowtot),ilist=.true.)
	j_o(ivproblem)%i(7)=iv  !number of terms for each row
	iv=j_deflist(ivproblem,'%termbas',list0=p_nrowtot,ilist=.true.)
	j_o(iv)%i(1)=0
	do ir=2,p_nrowtot
		j_o(iv)%i2(ir)=j_o(iv)%i2(ir-1)+nterminrow(ir-1)
	enddo !ir=2,p_nrowtot    970
	j_o(ivproblem)%i(19)=iv  !number of terms for each row
	deallocate(nterminrow)
 
 
 
	iv=j_deflist(ivproblem,'%varofterm',list0=nterm,list=varofterm(1:nterm))
	j_o(ivproblem)%i(8)=iv
 
	iv=j_deflist(ivproblem,'%colofterm',list0=nterm,ilist=.true.)
	do i=1,nterm
		j_o(iv)%i2(i)=j_inlistobject(varofterm(i),ivvars)
	enddo !i=1,nterm    982
	j_o(ivproblem)%i(20)=iv
 
	deallocate(varofterm,pvars)
 
	! call j_defmatrix(ivproblem,'%coef',nterm,1,j_matreg,iv) !vector of coeffcients
	! j_o(ivproblem)%i(9)=iv
	! j_o(iv)%d(1:nterm)=coef(1:nterm)
	! deallocate(coef)
 
	!write(6,*)'5333333333333 ndom',ndom,p_isdomain,p_ivdomvars
 
	if(p_isdomain)then
		!		write(6,*)'<ndom,ivdomains ',ivdomains(1:ndom)
		!if(p_p8)write(6,*)'<6646 ivdomains ',ivdomains(1:ndom)
		!call j_deflist(ivproblem,'%domvars',iv,list0=ndom,list=ivdomains(1:ndom))
		!variable showing for each domain if domain is
		j_o(ivproblem)%i(10)=p_ivdomvars
		!	deallocate(ivdomains)
 
		j_o(ivproblem)%i(11)=p_ivdomain  !p_ndoms !is this needed
 
		call j_getname(p_ivdomain)
 
		!write(6,*)'p_ivdomain ',j_oname(1:j_loname)
		!write(6,*)'domains ',j_o(p_ivdomain)%i(0)
		!	j_o(ivproblem)%i(10)=p_ivdomvars
 
		!	call j_printtext(ivdomaintext,0)
		ivdomtrans=0
 
		call j_getname(ivdomaintext)
		!		write(6,*)'<37 ',probname(1:lprobname),'*',j_oname(1:j_loname)
		write(6,*)' '
		write(6,*)'interpreting domain transformations from text object ',j_oname(1:j_loname)
		call j_command(probname(1:lprobname)//'%domtrans=trans(in->'//j_oname(1:j_loname)//')')
 
		!	write(6,*)'<34774 ',j_object(probname(1:lprobname)//'%domtrans')
		if(j_err)goto 900
 
 
		j_o(ivproblem)%i(12)=j_object(probname(1:lprobname)//'%domtrans')
 
 
		!	j_o(ivproblem)%i(13)=p_ivdomvars
 
		ndomvars=j_o(p_ivdomvars)%i(1)
		ipe=j_inlistobject(j_ivall,p_ivdomvars)
		if(ipe.gt.0)ndomvars=ndomvars-1
 
		j_o(ivproblem)%i(14)=  ceiling(real(ndomvars)/32.)   !ndomv packed domvars
		j_o(ivproblem)%i(15)=ivrowdomvar
		j_o(ivproblem)%i(16)=ivrowdomnum
 
		!irowdomain tells for eacg row the domain
 
	endif !if(p_isdomain)    996
 
	iv=j_deflist(ivproblem,'%coefvars',list0=nterm,list=coefvars(1:nterm))
	iv2=j_deflist(ivproblem,'%coefplus',list0=nterm,list=isplus(1:nterm))
	j_otype(iv2)=j_ipilist
	j_o(ivproblem)%i(17)=iv
	j_o(ivproblem)%i(18)=iv2
	!	j_o(ivproblem)%i(19)=p_ivrhsvars
	!	j_o(ivproblem)%i(20)=p_ivrhs2vars
 
	if(p_p8)write(6,*)'<33probi',j_o(ivproblem)%i
 
 
 
	900	j_v(j_ivprintinput)=printold
	return
end subroutine problem !subroutine problem(iob,io)

subroutine getdomain(domdef,ivdomaintext,ndom,idom,idomnum)
	use jmod, only: p_ndoms
	use jmod, only: j_object
	use jmod, only: j_putnewcleantext
	use jmod, only: p_ivdomain
	use jmod, only: j_clean
	use jmod, only: j_err
	use jmod, only: p_p8
	use jmod, only: j_getobject
	use jmod, only: j_ipreal
	use jmod, only: j_puttext
	use jmod, only: j_putlistobject
	use jmod, only: p_ivdomvars
	character*(*)::domdef
	character*4 domnum
	p_ndoms=p_ndoms+1
	idom=j_object(domdef)
 
	call j_putnewcleantext(p_ivdomain,domdef,iline)
	write(6,*)'domdef',domdef
	idomnum=iline
	!write(6,*)'domdef ',domdef,'idomnum',idomnum,'ndom',ndom
	if(iline.gt.ndom)then  ! new domain definition ?
		ndom=iline
		write(domnum,'(i4)')iline
		le=len_trim(domnum)
		call j_clean(domnum,le)
		if(j_err)return
		!	call j_clean(j_inp,j_linp)
		idom=j_object(domdef)
		!write(6,*)'<37idom ',idom
		if(idom.le.0)then
			idom=j_object('$Dom'//domnum(1:le))
			!write(6,*)'<38idom ',idom
			if(idom.le.0.and.p_p8)write(6,*)'<466>getobj','$Dom'//domnum(1:le)
			if(idom.le.0)idom=j_getobject(0,'$Dom'//domnum(1:le),j_ipreal)
			!write(6,*)'<39idom',idom
			if(p_p8)		write(6,*)'here>','$Dom'//domnum(1:le)//'='//domdef
			if(p_p8)write(6,*)ivdomaintext
			call j_puttext(ivdomaintext,&
				'$Dom'//domnum(1:le)//'='//domdef)
			ndom=iline
		endif !if(idom.le.0)   1077
	endif !if(iline.gt.ndom)   1068
	!write(6,*)'<889>',p_ivdomvars,ndom,idom
	iperk=j_putlistobject(p_ivdomvars,single=idom)
 
 
end subroutine

! subroutine getobjective(iobjtype)
! integer :: iobjtype
! !	if(p_nrowtot.eq.1)then
! !		write(6,*)'34here'
! ipmin=index(j_inp(1:j_linp+1),'==min')
! if(ipmin.gt.0)then
! !			call j_putd( rhs_,p_nrowtot,j_0)
! !			call j_putd(rhs2_,p_nrowtot,j_ninf)
! ip=ipmin
! p_isobjective=.true.  !iobjtype remains 0
! iobjtype=-1
! !	irowobj=p_nrowtot
! else !if(ipmin.gt.0)then
! ipmax=index(j_inp(1:j_linp+1),'==max')
! if(ipmax.le.0)then  !no objective
! write(6,*)'*no objective, you can find feasible using artificial objective: anything==max'
! j_err=.true.;return
! endif !if(ipmax.le.0)    971

! ip=ipmax
! p_isobjective=.true.
! iobjtype=1
! end if !if(ipmin.gt.0)    962
! ! if(p_isdomain)then
! ! ipe=j_putlistobject(ivrowdomvar,single=idom,append=.true.)
! ! ipe=j_putlistobject(ivrowdomnum,single=idomnum,append=.true.)
! ! endif !if(p_isdomain.and.p_isobjective)    991

!end subroutine



subroutine p_getrow(le,nterminrow,nterm,varofterm,pvars,coefvars,isplus)
	use jmod, only: j_inp
	use jmod, only: j_ivone
	use jmod, only: j_nextlim
	use jmod, only: j_nextrp
	use jmod, only: j_err
	use jmod, only: j_tex2iv
	use jmod, only: j_object
	use jmod, only: j_getobject
	use jmod, only: j_ipreal
	use jmod, only: j_puti
	use jmod, only: j_putlist
	use jmod, only: p_nrowtot
	integer::le !length os the left side
	integer, dimension(:), allocatable :: nterminrow,varofterm,pvars,coefvars,isplus
 
	!	double precision,allocatable, dimension(:)::coef
 
	!	double precision :: coe
	logical isminus,iscoef
	integer isplus0
	!	logical :: coevec = .false.
	!	logical :: listvar = .false.
	!	p_nrowtot=p_nrowtot+1
	!write(6,*)'<477474,iip',iip,j_inp(1:j_linp),' p_ivrow',p_ivrow, 'interpret coef','idomain ',p_isdomain
 
	!	write(6,*)'getrow',le,nterm,allocated(nterminrow),allocated(varofterm),allocated(pvars),allocated(coefvars),allocated(isplus)
	! interpret:
	!			call j_puti(nterminrow,p_nrowtot,0)
 
	!	le=ip-1
 
	!	write(6,*)'<339le',le
	!  -x+3*x+z-3*y  x+4*c-   -2*g   !ial is at + or -1 or intially
	!		18    continue
	! this section is reading the coefficient, which can start with '-' or '+' or the first
	! cofficient can be without sign. And if coefficient is not given, it get value 1.
 
	!	nex=0
	isminus=j_inp(1:1).eq.'-'
	if(isminus)then
		icoef1=2
	elseif(j_inp(1:1).eq.'+')then
		icoef1=2
	else
		icoef1=1
	endif !if(isminus)   1156
	!	write(6,*)'<44>',j_inp(1:j_linp)
	!	coevec = .false.
 
	! ial7=ial
	! if(j_inp(ial:ial).eq.'-')ial7=ial+1
	!		write(6,*)'tasnrowtot',p_nrowtot
		termloop: do while(icoef1.le.le)
!	coevec=.false.
		isplus0=1
		ivcoe=j_ivone
		!	write(6,*)'icoef1,nex ',icoef1,nex
		nex=j_nextlim(j_inp,icoef1,le,'(*+-')
		!	write(6,*)'icoef1,nex ',icoef1,nex
		!	if(nterm.gt.20)stop
		if(j_inp(nex:nex).eq.'*'.or.j_inp(nex:nex).eq.'(')then
			if(j_inp(nex:nex).eq.'(')then
				irp= j_nextrp(j_inp,nex,le) !nextrp starts from (
				if(irp.gt.le)then
					write(6,*)'**cannot find ) at ',j_inp(1:nex)
					j_err=.true.  ;return
				endif !if(irp.gt.le)   1180
				if(nex.ne.icoef1)then
					write(6,*)'cannot understand what is ',j_inp(icoef1:nex)
					j_err=.true.;return
				endif !if(nex.ne.icoef1)   1184
				if(j_inp(irp+1:irp+1).ne.'*')then
					write(6,*)j_inp(nex:irp),' must be followed with *'
					j_err=.true.;return
				endif !if(j_inp(irp+1:irp+1).ne.'*')   1188
				icoef1=nex  !+1
				icoef2=irp  !-1
				ivar1=irp+2
			else  !'*
				icoef2=nex-1
				ivar1=nex+1
				!write(6,*)'ivar1 ',j_inp(1:ivar1)
			endif !if(j_inp(nex:nex).eq.'(')   1178
 
 
			ivcoe=j_tex2iv(j_inp(icoef1:icoef2),isplus0)
			if(j_err)return
			!	coe=j_v(ivcoe)
 
			!	call j_getname(ivcoe)
 
			!write(6,*)'ivcoe ',j_inp(icoef1:icoef2),' ',ivcoe,j_oname(1:j_loname)
			! j_object(j_inp(icoef1:icoef2))
			! write(6,*)'icoef1:icoef2',icoef1,icoef2,j_inp(icoef1:icoef2),ivcoe
			! call j_getname(ivcoe)
			! write(6,*)'coef ',j_oname(1:j_loname)
			! if(ivcoe.le.0)then
			! call j_getobject(0,j_inp(icoef1:icoef2),j_ipreal,ivcoe,silent=.true.)
			! if(j_err)then
			! j_err=.false.
			! coe=j_val(j_inp(icoef1:icoef2))
			! ivcoe=j_isconst(j_inp(icoef1:icoef2))
 
 
			! if(j_err)then
			! write(6,*)'error in interpreting ',j_inp(icoef1:icoef2),' in cols ',icoef1,'-',icoef2,' in line:'
			! write(6,*)j_inp(1:j_linp)
			! return
			! endif !if(j_err)   1373
			! endif !if(j_err)   1367
			! elseif(j_otype(ivcoe).eq.j_ipreal)then
			! coe=j_v(ivcoe)
			! else
			! coe=j_1
			! ivcoe=j_ivone
			! ! elseif(j_otype(ivcoe).eq.j_ipmatrix)then
			! ! if(j_o(ivcoe)%i(1).eq.1.or.j_o(ivcoe)%i(2).eq.1)coevec=.true.
			! endif !if(ivcoe.le.0)   1365
 
		else
			!	coe=j_1
			ivcoe=j_ivone
			ivar1=icoef1
		endif !if(j_inp(nex:nex).eq.'*'.or.j_inp(nex:nex).eq.'(')   1177
		if(isminus)then
			!		coe=-coe
			isplus0=0
		endif !if(isminus)   1241
		nex=j_nextlim(j_inp,ivar1,le,'+-')
		ivar2=nex-1
		!	write(6,*)'ivar2 ',ivar1,ivar2,j_inp(1:ivar2)
		ivvar=j_object(j_inp(ivar1:ivar2))
		if(ivvar.le.0)ivvar=j_getobject(0,j_inp(ivar1:ivar2),j_ipreal)
		if(j_err)return
 
		! if(j_otype(ivvar).ne.j_ipreal.and.j_otype(ivvar).ne.j_iplist.and.j_otype(ivvar).ne.j_iptable)then
		! call j_getname(ivvar)
		! write(6,*)'wrn* deleting ',j_oname(1:j_loname),' which was ',j_otypes(j_otype(ivvar))
		! call j_del(ivvar)
		! endif !if(j_otype(ivvar).ne.j_ipreal.and.j_otype(ivvar).ne.j_ipli   1364
 
		!	if(j_otype(ivvar).eq.j_ipreal)then
		! if(coevec)then
		! call j_getname(ivcoe)
		! write(6,*)'because ',j_oname(1:j_loname),' is vector ',j_inp(ivar1:nex-1), 'must be LIST'
		! j_err=.true.;return
 
		! endif !if(coevec)   1371
		nterm=nterm+1
		!	call j_putd(coef,nterm,coe)  !coeffcients of terms
		call j_puti(varofterm,nterm,ivvar)  !varofterm variables of terms
		ipo= j_putlist(ivvar,pvars)
		call j_puti(nterminrow,p_nrowtot,nterminrow(p_nrowtot)+1)  !increase number of variables
		!write(6,*)'nterm,ivcoe',nterm,ivcoe,isplus0
		call j_puti(coefvars,nterm,ivcoe)
 
		call j_puti(isplus,nterm,isplus0)
 
		! elseif(j_otype(ivvar).eq.j_iplist)then
		! if(coevec)then
		! if(j_o(ivcoe)%i(3).ne.j_o(ivvar)%i(1))then
		! call j_getname(ivcoe,ivvar)
		! write(6,*)j_oname(1:j_loname),' has ',j_o(ivcoe)%i(3),' elements and ',j_oname2(1:j_loname2),' ',j_o(ivvar)%i(1)
		! j_err=.true.;return
		! endif !if(j_o(ivcoe)%i(3).ne.j_o(ivvar)%i(1))   1385
		! do j=1,j_o(ivvar)%i(1)
		! nterm=nterm+1
		! call j_putd(coef,nterm,j_o(ivcoe)%d(j))  !coeffcients of terms
		! call j_puti(varofterm,nterm,j_o(ivvar)%i2(j))  !varofterm variables of terms
		! ipo= j_putlist(j_o(ivvar)%i2(j),pvars)
		! call j_puti(nterminrow,p_nrowtot,nterminrow(p_nrowtot)+1)  !increase number of variables
		! enddo !j=1,j_o(ivvar)%i(1)   1390
		! else
		! do j=1,j_o(ivvar)%i(1)
		! nterm=nterm+1
		! call j_putd(coef,nterm,coe)  !coeffcients of terms
		! call j_puti(varofterm,nterm,j_o(ivvar)%i2(j))  !varofterm variables of terms
		! ipo= j_putlist(j_o(ivvar)%i2(j),pvars)
		! call j_puti(nterminrow,p_nrowtot,nterminrow(p_nrowtot)+1)  !increase number of variables
		! enddo !j=1,j_o(ivvar)%i(1)   1398
 
		! endif !if(coevec)   1384
		! elseif(j_otype(ivvar).eq.j_iptable)then
		! nterm=nterm+1
		! if(ivcoe.gt.0)then
		! call j_putd(coef,nterm,dble(ivcoe))
		! else
 
		! call j_putd(coef,nterm,dble(j_ivconst(coe)))
		! endif !if(ivcoe.gt.0)   1409
		! call j_puti(varofterm,nterm,ivvar)  !varofterm variables of terms
		! call j_puti(nterminrow,p_nrowtot,nterminrow(p_nrowtot)+1)  !increase number of variables
		! ipo= j_putlist(ivvar,pvars)
		! endif !if(j_otype(ivvar).eq.j_ipreal)   1370
 
		if(ivar2.eq.le)exit
		isminus=j_inp(ivar2+1:ivar2+1).eq.'-'
 
		icoef1=ivar2+2
		!write(6,*)'ivar2,le ',ivar2,le,j_inp(1:ivar2)
 
	enddo termloop !mloop: do while(icoef1.le.le)   1169
	return
	!	end if !if(j_inp(1:j_linp).ne.'/')    532
end subroutine

subroutine getrhs(le)
	use jmod, only: j_nextlim0
	use jmod, only: j_inp
	use jmod, only: j_linp
	use jmod, only: j_ivninf
	use jmod, only: j_ivinf
	use jmod, only: j_tex2iv
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: p_ivrhsvars
	use jmod, only: p_nrow
	use jmod, only: p_ivrhs2vars
	use jmod, only: p_ivrhsplus
	use jmod, only: p_ivrhs2plus
	integer::le !length os the left side
	integer isplus,isplus2
	!write(6,*)'<334>constraint',j_inp(1:j_linp)
	le1=le+1
	ip=j_nextlim0(j_inp,le1,j_linp,'=')
	!		write(6,*)'<334>constraint ',j_inp(1:j_linp),' ip=',ip
	iv=j_ivninf
	iv2=j_ivinf
	isplus=1
	isplus2=1
	if(ip.gt.0)then
		iv=j_tex2iv(j_inp(ip+1:j_linp),isplus)
		iv2=iv
		isplus2=isplus
		if(j_err)then  ! j_err coming from val
			write(6,*)'*problem: error in interpreting rhs ',j_inp(ip+1:j_linp)
			return
		endif !if(j_err)   1338
		!	j_o(p_ivrhs)%d(p_nrow)=j_val(j_inp(ip+1:j_linp))
 
 
		! j_o(p_ivrhs2)%d(p_nrow)=j_o(p_ivrhs)%d(p_nrow)
 
		! j_o(p_ivrhsvars)%i2(p_nrow)=j_tex2iv(j_inp(ip+1:j_linp),isplus0)
		! j_o(p_ivrhs2vars)%i2(p_nrow)=j_o(p_ivrhsvars)%i2(p_nrow)
 
 
	else !if(ipe.gt.0)then
		ipg=j_nextlim0(j_inp,1,j_linp,'>')
 
		if(ipg.gt.0)then
			if(j_inp(ipg-1:ipg-1).eq.'-')then
				ipg=0
 
			endif !if(j_inp(ipg-1:ipg-1).eq.'-')   1355
 
 
		endif !if(ipg.gt.0)   1354
		!		write(6,*)'<456ipg',ipg
		ipl=j_nextlim0(j_inp,1,j_linp,'<')
		if(ipg+ipl.gt.0)then
			! this section is rather comlicated, but it read the rhs_ or rhs2_ (upper bound) when either or
			!  both are given, and if both are given they can (hopefully) be in any order
			if(ipg.gt.0)then
				lu=j_linp
				if(ipl.gt.ipg)lu=ipl-1
				!	write(6,*)'p_nrow,p_nrowtot',p_nrow,p_nrowtot
				iv=j_tex2iv(j_inp(ipg+1:lu),isplus)
 
				! j_o(p_ivrhsvar)%i2(p_nrow)=iv
				! j_o(p_ivrhs)%d(p_nrow)=j_v(iv)
 
				!		call j_putd( rhs_,p_nrowtot,j_val(j_inp(ipg+1:lu)) )     !val gives numeric value of a expression
				if(j_err)then  ! j_err coming from val
					write(6,*)'*problem: error in interpreting rhs ',j_inp(ipg+1:lu)
					return
				endif !if(j_err)   1377
				!		j_o(p_ivrhsvars)%i2(p_nrow)=j_tex2iv(j_inp(ipg+1:lu),isplus0)
 
			else !if(ipg.gt.0)then
				iv=j_ivninf
				! j_o(p_ivrhs)%d(p_nrow)=j_ninf
				! j_o(p_ivrhsvars)%i2(p_nrow)=j_ivtolast
				!	call j_putd(rhs_,p_nrowtot,j_ninf )
			end if !if(ipg.gt.0)   1367
			if(ipl.gt.0)then
				lu=j_linp;if(ipg.gt.ipl)lu=ipg-1
				iv2=j_tex2iv(j_inp(ipl+1:lu),isplus2)
 
				!j_o(p_ivrhs2)%d(p_nrow)=j_v(iv)  !j_val(j_inp(ipl+1:lu))
				!	call j_putd(rhs2_,p_nrowtot,j_val(j_inp(ipl+1:lu)) )
				!		write(6,*)'<rhs2',ipl,ipg,j_inp(ipl+1:lu),j_val(j_inp(ipl+1:lu))
				if(j_err)then  ! j_err coming from val
					write(6,*)'*problem: error in interpreting rhs2 ',j_inp(ipl+1:lu)
					return
				endif !if(j_err)   1396
				!	j_o(p_ivrhs2vars)%i2(p_nrow)=iv !j_tex2iv(j_inp(ipl+1:lu),isplus0)
 
			else !if(ipl.gt.0)then
				iv2=j_ivinf
				! j_o(p_ivrhs2)%d(p_nrow)=j_inf
				! j_o(p_ivrhs2vars)%i2(p_nrow)=j_ivinf
				!		call j_putd(rhs2_,p_nrowtot,j_inf)
			end if !if(ipl.gt.0)   1389
			if(ipg.gt.0.and.ipl.gt.0)then
				ip=min(ipg,ipl)
			else !if(ipg.gt.0.and.ipl.gt.0)then
				ip=ipg+ipl
			end if !if(ipg.gt.0.and.ipl.gt.0)   1408
 
		end if !if(ipg+ipl.gt.0)   1364
	end if !if(ip.gt.0)   1334
	!	j_o(p_ivrhs)%d(p_nrow)=j_v(iv)
	!	j_o(p_ivrhs2)%d(p_nrow)=j_v(iv2)
	!write(6,*)'rhs ',p_ivrhsvars,p_ivrhs2vars
	j_o(p_ivrhsvars)%i2(p_nrow)=iv
	j_o(p_ivrhs2vars)%i2(p_nrow)=iv2
	j_o(p_ivrhsplus)%i2(p_nrow)=isplus
	j_o(p_ivrhs2plus)%i2(p_nrow)=isplus2
end subroutine getrhs


!	contains    !!!!
! subroutine jlpcurix(iunit)   !!!! p_idomba tells unit
! ! determines for each row if the unit iunit belonggs to the domain of the row
! ! units are accessed one by one
! ! p_nrowcurx
! ! p_rowcurx
! ! p_idomba


! !write(6,*)'<45 ',iunit,idomba

! p_nrowcurx=p_nrowx0
! !p_ixcur=p_ixcur0
! !	p_ixcur=.false.
! !	write(6,*)'p_unit,p_domba,p_ndomvars,p_ido1',p_unit,p_idomba,p_ndomvars,p_ido1
! idomba=iunit-1
! !	write(6,*)'p_ido1,p_domvars',p_ido1,p_ndomvars,p_nrowx0,'*',p_rowcurx
! do ido=p_ido1,p_ndomvars
! !icurint=(id-1)/32+1
! !	icurbit=p_id-(icurint-1)*32-1
! !	write(6,*)'ido',p_icurint(ido),p_icurbit(ido)
! !		if(p_p8)write(6,*)'curix,iuni,p_idomba,p_icurint(ido)',iuni,ido,p_idomba,p_icurint(ido)
! !	write(16,*)'punit',p_unit,p_idomba,ido,p_icurint(ido),p_icurbit(ido)

! if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido)))then
! !	write(6,*)'idook ',ido,'nixcu',p_nixcu(ido),'bas',p_ixcubas(ido)
! do j=1,p_nixcu(ido)
! p_nrowcurx=p_nrowcurx+1
! p_rowcurx(p_nrowcurx)=p_ixcurow(p_ixcubas(ido)+j)
! ! p_ixcur(p_rowcurx(p_nrowcurx))=.true.
! !		p_ixcur(p_ixcurow(p_ixcubas(ido)+j))=.true.

! enddo !j=1,p_nixcu(ido)   1468
! endif !if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido   1466
! enddo !ido=p_ido1,p_ndomvars   1459

! write(16,*)'ppp',idomba,p_nrowcurx,p_rowcurx(1:p_nrowcurx)
! !	p_nrowcurx=p_nrowx0
! ! p_nrowcurx=0
! ! do j=0,p_nrow
! ! if(p_ixcur(j))then
! ! p_nrowcurx=p_nrowcurx+1
! ! p_rowcurx(p_nrowcurx)=j
! ! endif !if(p_ixcur(j))   1257
! ! enddo !j=0,p_nrow   1256
! ! !if(p_p)write(p_n16,*)'p_ixcur0',p_ixcur0,p_idomba
! !if(p_p)write(p_n16,*)'p_ixcur',p_ixcur
! !	p_row0=1
! !	if(p_rowcurx(1).eq.0)p_row0=2
! !	p_idomba=p_idomba+p_ndomv
! !write(6,*)'p_nrowcurx ',p_nrowcurx
! end subroutine !subroutine j_curix(iuni)

subroutine jlpcurix(iuni)   !!!!
	use jmod, only: p_isndomv
	use jmod, only: p_ndomv
	use jmod, only: p_nrowcurx
	use jmod, only: p_nrowx0
	use jmod, only: p_ido1
	use jmod, only: p_ndomvars
	use jmod, only: p_domainbits
	use jmod, only: p_icurint
	use jmod, only: p_icurbit
	use jmod, only: p_nixcu
	use jmod, only: p_rowcurx
	use jmod, only: p_ixcurow
	use jmod, only: p_ixcubas
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units can be in random order
	! p_nrowcurx
	! p_rowcurx
 
	!determines for each row if the unit iunit belonggs to the domain of the row
	if(p_isndomv)then
		idomba=(iuni-1)*p_ndomv
	else
		idomba=iuni-1
	endif !if(p_isndomv)   1486
 
	p_nrowcurx=p_nrowx0
	!p_ixcur=p_ixcur0
	!	p_ixcur=.false.
	!	write(6,*)'p_unitcurix3,p_domba',iuni,idomba
	!	write(6,*)'p_ido1,p_domvars',p_ido1,p_ndomvars,p_nrowx0,'*',p_rowcurx
	do ido=p_ido1,p_ndomvars
		!icurint=(id-1)/32+1
		!	icurbit=p_id-(icurint-1)*32-1
		!	write(6,*)'curix,iuni,p_idomba,p_icurint(ido)',iuni,ido,idomba,p_icurint(ido)
		!	write(6,*)'icurint,icurbit',p_icurint(ido),p_icurbit(ido)
		if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido)))then
			!	write(6,*)'idook ',ido,'nixcu',p_nixcu(ido),'bas',p_ixcubas(ido)
			do j=1,p_nixcu(ido)
				p_nrowcurx=p_nrowcurx+1
				p_rowcurx(p_nrowcurx)=p_ixcurow(p_ixcubas(ido)+j)
				! p_ixcur(p_rowcurx(p_nrowcurx))=.true.
				!		p_ixcur(p_ixcurow(p_ixcubas(ido)+j))=.true.
 
			enddo !j=1,p_nixcu(ido)   1504
		else
			!		write(6,*)'***NO iuni,ido',iuni,ido
		endif !if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido   1502
	enddo !ido=p_ido1,p_ndomvars   1497
	!	p_nrowcurx=p_nrowx0
 
end subroutine !subroutine




! !determines for each row if the unit iunit belonggs to the domain of the row
! !matrix  domainbits(number,mxunit) contains bits for telling if unit belongs to certain domain

! ! nfxfrow : montako tehdasmuuttujaa rivillä on
! ! frowx : millä riveillä tehdasmuuttujia on
! p_nfrowx=0
! if(p_ndom.le.0)then
! do jcurix=0,p_nrow
! p_ixcurfact(jcurix)=.false.
! !  if(p_nfrowx(jcurix+1).ne.0)then  !j_irowrow(jcurix)=jcurix+-1
! if(p_nfrowx(jcurix+1).ne.0)then  !j_irowrow(jcurix)=jcurix+-1
! p_nfrowx=p_nfrowx+1
! p_frowx(p_nfrowx)=jcurix
! p_ixcurfact(jcurix)=.true.
! endif !if(p_nfrowx(jcurix+1).ne.0)    963
! enddo;return !jcurix=0,p_nrow    960
! endif !if(p_ndom.le.0)    959

! ! fdomain
! idombas=(iuni-1)*p_ndomv
! do jcurix=0,p_nrow
! p_ixcurfact(jcurix)=.false.
! if(p_nfrowx(jcurix+1).ne.0)then !p_irowrow(jcurix)).ne.0)then

! icurint=(p_irowdomain(jcurix)-1)/32+1  !integer part
! icurbit=p_irowdomain(jcurix)-(icurint-1)*32-1  !bit part
! if(btest(p_domainbits(idombas+icurint),icurbit))then
! !		if(btest(p_domainbits(icurint,iuni),icurbit))then
! p_nfrowx=p_nfrowx+1
! p_frowx(p_nfrowx)=jcurix
! p_ixcurfact(jcurix)=.true.
! endif !if(btest(p_domainbits(idombas+icurint),icurbit))    979
! endif !if(p_nfrowx(jcurix+1).ne.0)    975
! enddo !jcurix=0,p_nrow    973
! end subroutine !subroutine j_fcurix(iuni)

! subroutine jlpfcurixy(iuni)  !!!!

! !determines for each row if the unit iunit belonggs to the domain of the row
! !matrix  domainbits(number,mxunit) contains bits for telling if unit belongs to certain domain

! ! nfyfrow : montako tehdasmuuttujaa rivillä on
! ! fyrow : millä riveillä tehdasmuuttujia on
! p_nfyrow=0
! if(p_ndom.le.0)then
! do jcurix=0,p_nrow
! if(p_nfyrow(jcurix+1).ne.0)then
! p_nfyrow=p_nfyrow+1
! p_fyrow(p_nfyrow)=jcurix
! p_ixcurfact(jcurix)=.true.
! endif !if(p_nfyrow(jcurix+1).ne.0)    998
! enddo !jcurix=0,p_nrow    997
! return !do jcurix=0,j_nrow
! endif !if(p_ndom.le.0)    996
! !		write(6,*)'<88888 ',p_nfyrow
! ! fdomain
! idombas=(iuni-1)*p_ndomv
! do jcurix=0,p_nrow
! if(p_nfyrow(jcurix+1).ne.0)then
! icurint=(p_irowdomain(jcurix)-1)/32+1  !integer part
! icurbit=p_irowdomain(jcurix)-(icurint-1)*32-1  !bit part
! if(btest(p_domainbits(idombas+icurint),icurbit))then
! p_nfyrow=p_nfyrow+1
! p_fyrow(p_nfyrow)=jcurix
! p_ixcurfact(jcurix)=.true.
! endif !if(btest(p_domainbits(idombas+icurint),icurbit))   1013
! endif !if(p_nfyrow(jcurix+1).ne.0)   1010
! enddo !jcurix=0,p_nrow   1009
! end subroutine !subroutine j_fcurixy(iuni)

! subroutine jlpfirow2curix(iuni)   !!!!

! ! muodostaa taulukon, jonka perusteella tiedetään mitkä tehdasmuuttujia sisältävät
! ! lavennetut tehtävärivit vastaavat kutakin alkuperäistä tehtäväriviä
! ! irow2curix(0, alkup_rivi,)= #lavennetut rivit
! ! irow2curix(i, alkup_rivi)= i:nnen lavennetun rivin numero

! p_irow2curix(0,:) = 0
! if(p_ndom.le.0)then
! do jcurix=0,p_nrow
! irow_ = jcurix+1 !p_irowrow(jcurix)  !irow_jcurix+1
! if((p_nfrowx(irow_).ne.0).or.(p_nfyrow(irow_).ne.0))then
! p_irow2curix(0,irow_)=p_irow2curix(0,irow_)+1
! p_irow2curix(p_irow2curix(0,irow_),irow_)=jcurix
! endif !if((p_nfrowx(irow_).ne.0).or.(p_nfyrow(irow_).ne.0))   1033
! enddo !jcurix=0,p_nrow   1031
! return !do jcurix=0,j_nrow
! endif !if(p_ndom.le.0)   1030


! !	 fdomain
! idombas=(iuni-1)*p_ndomv
! do jcurix=0,p_nrow
! irow_ = jcurix+1 !p_irowrow(jcurix)
! !	write(6,*)'<33>',jcurix,irow_
! if((p_nfrowx(irow_).ne.0).or.(p_nfyrow(irow_).ne.0))then
! icurint=(p_irowdomain(jcurix)-1)/32+1  !integer part
! icurbit=p_irowdomain(jcurix)-(icurint-1)*32-1  !bit part
! if(btest(p_domainbits(idombas+icurint),icurbit))then
! p_irow2curix(0,irow_)=p_irow2curix(0,irow_)+1
! p_irow2curix(p_irow2curix(0,irow_),irow_)=jcurix
! endif !if(btest(p_domainbits(idombas+icurint),icurbit))   1050
! endif !if((p_nfrowx(irow_).ne.0).or.(p_nfyrow(irow_).ne.0))   1047
! enddo !jcurix=0,p_nrow   1044

! end subroutine !subroutine j_firow2curix(iuni)

!end module !module jlpmod


!*----------------------------------------------------------------
subroutine jlplex(list,i1,i2,listi)   !!!!
 
	! exchange elements i1 and i2 in list, and update iverese list telling where
	! each element is
	integer list(*),listi(*)
	!  print_ivalues(6, i1,i2 ',i1,i2)
	! i1 and i2 may change if actual arguments are elements of list, or listi
	ii1=i1
	ii2=i2
	k=list(ii1)
	list(ii1)=list(ii2)
	list(ii2)=k
	listi(list(ii1))=ii1
	listi(list(ii2))=ii2
	return
end subroutine!subroutine jlplex(list,i1,i2,listi)

subroutine lexf(list,i1,i2,listi,imin,imax)   !!!!
 
	! exchange elements i1 and i2 in list, and update iverese list telling where
	! each element is
	integer list(imin:imax),listi(imin:imax)
 
	! i1 and i2 may change if actual arguments are elements of list, or listi
	ii1=i1
	ii2=i2
	k=list(ii1)
	list(ii1)=list(ii2)
	list(ii2)=k
	listi(list(ii1))=ii1
	listi(list(ii2))=ii2
	! print_ivalues(6, i1,i2 ',i1,i2)
	return
end subroutine!subroutine lexf(list,i1,i2,listi,imin,imax)

subroutine printrowinfo(ir)   !!!!
	use jmod, only: p_isdomain
	use jmod, only: p_buf
	use jmod, only: p_rowdomnum
	use jmod, only: j_getline
	use jmod, only: p_ivdomain
	use jmod, only: j_chi5
	use jmod, only: p_domainunits
	use jmod, only: p_ivrow
	!character*5 chi5 comes now from getmod
	if(p_isdomain)then
 
p_buf='DOMAIN:'
		idom=p_rowdomnum(ir)
		!	call j_getline(p_ivdomain,idom,p_buf,le)
		p_buf=' '
		call j_getline(p_ivdomain,idom,p_buf(8:),le)
		p_buf(74:78)='units'
		p_buf(68:72)=j_chi5(p_domainunits(idom),0)
		write(6,'(a)')p_buf(1:72)
	else
		!for each expanded problem
		! irowrow tells the the initial row in the nonexpanded problem
		call j_getline(p_ivrow,ir+1,p_buf,le)
		write(6,'(a)')p_buf(1:le)
	endif !if(p_isdomain)   1670
	return
end subroutine!subroutine printrowinfo(ir)



subroutine domain(iob,io)   !!!!
	use jmod, only: j_o
	use jmod, only: j_v
	use jmod, only: j_line2
	use jmod, only: p_ivdomain
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	j_v(iout)= j_line2(p_ivdomain,j_o(iob)%i(io+2))
	!	io=io+narg+3
	return
end subroutine!subroutine domain(iob,io)

subroutine priceunit(iob,io)  !!!!
	use jmod, only: j_err
	!xmatiba(iobs)=(iobs-1)*j_ntemp0
	write(6,*)'priceunit is obsolete, look vactor outpu%priceunit'
	j_err=.true.
	return
	! if(j_v(p_ivstartedjlp).eq.0)then
	! write(6,*)'**priceunit: Started_jlp=0'
	! j_err=.true.
	! return
	! endif !if(j_v(p_ivstartedjlp).eq.0)   1954
 
	! narg=j_o(iob)%i(io+1)
	! iout=j_o(iob)%i(io+2+narg)
	! if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	! iunit=j_v( j_o(iob)%i(io+2) )
	! if(iunit.gt.p_nunits.or.iunit.le.0)then
	! write(6,*)'**illegal unit in price%unit ',iunit,' max is',p_nunits
	! j_err=.true. ;return
	! endif !if(iunit.gt.p_nunits.or.iunit.le.0)   1964
	! if(p_isdomain)call jlpcurix(iunit) !
	! ! determines for each row if the unit iunit belonggs to the domain of the row
	! ! units can be in random order
	! ! p_nrowcurx
	! ! p_rowcurx
 
	! p_value=j_0
	! iobs=p_ibaunit(iunit)+p_keys(iunit)
	! ibaxmat=(iobs-1)*p_ntemp0  !xmatiba(iobs) !,1)
	! do jj=1,p_nrowcurx
	! j=p_rowcurx(jj)
	! !	j_value=j_value+j_vx(j)*j_xmat(j_ix(j),iobs) !v(ix(j))
	! p_value=p_value+p_vx(j)*p_xmat(ibaxmat+p_ix(j)) !v(ix(j))
	! enddo !jj=1,p_nrowcurx   1977
	! j_v(iout)=p_value
 
	!	io=io+narg+3
	return
end subroutine !subroutine priceunit(iob,io)

! subroutine priceunit_()  !!!!
! !xmatiba(iobs)=(iobs-1)*j_ntemp0
! ivout=j_defmatrix(j_ivout,'%priceunit',p_nunits,1,j_matreg)
! do iunit=1,p_nunits
! !	iunit=j_v( j_o(iob)%i(io+2) )

! if(p_isdomain)call jlpcurix(iunit) !
! ! determines for each row if the unit iunit belonggs to the domain of the row
! ! units can be in random order
! ! p_nrowcurx
! ! p_rowcurx

! j_dapu=j_0
! iobs=p_ibaunit(iunit)+p_keys(iunit)
! ibaxmat=(iobs-1)*p_ntemp0  !xmatiba(iobs) !,1)
! do jj=1,p_nrowcurx
! j=p_rowcurx(jj)
! !	j_value=j_value+j_vx(j)*j_xmat(j_ix(j),iobs) !v(ix(j))
! j_dapu=j_dapu+p_vx(j)*p_xmat(ibaxmat+p_ix(j)) !v(ix(j))
! enddo !jj=1,p_nrowcurx   2003
! j_o(ivout)%d(iunit)=j_dapu
! end do !iunit=1,p_nunits   1991
! write(6,*)'keys',p_keys
! write(6,*)'iba',p_ibaunit
! write(6,*)'vx',p_vx
! write(6,*)'xmat',p_xmat
! write(6,*)p_nrowcurx

! !	io=io+narg+3
! return
! end subroutine !subroutine priceunit(iob,io)

subroutine priceschedw(iob,io)   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: j_otype
	use jmod, only: j_ipreal
	use jmod, only: j_del
	use jmod, only: p_nunits
	use jmod, only: p_ns
	use jmod, only: p_isdomain
	use jmod, only: j_0
	use jmod, only: p_value
	use jmod, only: p_ibaunit
	use jmod, only: p_ntemp0
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: p_vx
	use jmod, only: p_xmat
	use jmod, only: p_ix
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**priceschedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   1773
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iunit=j_v( j_o(iob)%i(io+2) )
	is=j_v( j_o(iob)%i(io+3) )
 
	if(iunit.gt.p_nunits.or.iunit.le.0)then
		write(6,*)'**illegal unit in price%schedw ',iunit,' max is',p_nunits
		j_err=.true. ;return
	endif !if(iunit.gt.p_nunits.or.iunit.le.0)   1784
	if(is.gt.p_ns(iunit).or.is.le.0)then
		write(6,*)'**illegal sched ',is,'  in price%schedw for unit ',iunit, 'max=',p_ns(iunit)
		j_err=.true. ;return
	endif !if(is.gt.p_ns(iunit).or.is.le.0)   1788
	if(p_isdomain)call jlpcurix(iunit)
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units can be in random order
	! p_nrowcurx
	! p_rowcurx
 
	p_value=j_0
	iobs=p_ibaunit(iunit)+is
	ibaxmat=(iobs-1)*p_ntemp0  !xmatiba(iobs) ! ,1)
	do jj=1,p_nrowcurx
		j=p_rowcurx(jj)
		p_value=p_value+p_vx(j)*p_xmat(ibaxmat+p_ix(j)) ! ,iobs) !v(ix(j))
	enddo !jj=1,p_nrowcurx   1801
	j_v(iout)=p_value
 
	!	io=io+narg+3
	return
end subroutine!subroutine priceschedw(iob,io)

subroutine priceschedcum(iob,io)   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: j_otype
	use jmod, only: j_ipreal
	use jmod, only: j_del
	use jmod, only: j_dnobs
	use jmod, only: p_ntemp0
	use jmod, only: j_0
	use jmod, only: p_value
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: p_vx
	use jmod, only: p_xmat
	use jmod, only: p_ix
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**priceschedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   1813
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	is=j_v( j_o(iob)%i(io+2) )
	if(is.gt.j_dnobs.or.is.le.0)then
		write(6,*)'**illegal sched in price%schedcum: ',is,' max is',j_dnobs
		j_err=.true. ;return
	endif !if(is.gt.j_dnobs.or.is.le.0)   1822
	ibaxmat=(is-1)*p_ntemp0  !xmatiba(is) !,1)
	! if(p_isdomain)then
	! write(
	! call jlpcurix(iunit)
	! endif
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units can be in random order
	! p_nrowcurx
	! p_rowcurx
	p_value=j_0
	do jj=1,p_nrowcurx
		j=p_rowcurx(jj)
		!j_value=j_value+j_vx(j)*j_xmat(j_ix(j),is) !,is) !v(ix(j))
		p_value=p_value+p_vx(j)*p_xmat(ibaxmat+p_ix(j)) !,is) !v(ix(j))
	enddo !jj=1,p_nrowcurx   1836
	j_v(iout)=p_value
 
	!	io=io+narg+3
	return
end subroutine !subroutine priceschedcum(iob,io)


subroutine weights(iob,io)   !!!!
	use jmod, only: j_oname
	use jmod, only: j_loname
	! Section weights weights() weights of schedules
	! TO BE RAPORTED LATER
	! endsection
	!	call j_getname(p_ivout)
	write(6,*)'weights() is obsolete, use vector ',j_oname(1:j_loname)//'%weights  to access weights'
	! if(j_v(p_ivstartedjlp).eq.0)then
	! write(6,*)'**weights: Started_jlp=0'
	! j_err=.true.
	! return
	! endif !if(j_v(p_ivstartedjlp).eq.0)   2005
	! ! if narg=1 then total number of weights, if narg=2 then weight in unit
	! narg=j_o(iob)%i(io+1)
	! iout=j_o(iob)%i(io+2+narg)
	! if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	! if(narg.eq.0)then
	! j_v(iout)=p_nunits+p_lx0
	! else !if(narg.eq.0)then
	! iunit=j_o(iob)%i(io+2)
	! ns=0
	! do i=1,p_ndiv
	! if(p_unitdiv(i).eq.iunit)then
	! ns=ns+1
	! elseif(p_unitdiv(i).gt.iunit)then !if(j_iunitdiv(i).eq.iunit)then
	! goto 77
	! endif !if(p_unitdiv(i).eq.iunit)   2020
	! end do !i=1,p_ndiv   2019
	! ns=1
	! !
	! 77  j_v(iout)=ns
	! end if !if(narg.eq.0)   2014
	!	io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine weights(iob,io)

function nweights()  !number of nonzero weights tehty Markulle  !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: p_nunits
	use jmod, only: p_lx0
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**nweights: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   1884
 
	nweights=p_nunits+p_lx0
 
	return
end function!function nweights()


subroutine partweights(iob,io)   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: j_otype
	use jmod, only: j_ipreal
	use jmod, only: j_del
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	! Section partweights partweight() weights of split schedules
	! TO BE RAPORTED LATER
	! endsection
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**partweights: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   1901
	! if narg=1 then total number of weights, if narg=2 then weight in unit
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	if(narg.eq.0)then
		j_v(iout)=p_ndiv
	else !if(narg.eq.0)then
		iunit=j_o(iob)%i(io+2)
		ns=0
		do i=1,p_ndiv
			if(p_unitdiv(i).eq.iunit)then
				ns=ns+1
			endif !if(p_unitdiv(i).eq.iunit)   1917
		enddo !i=1,p_ndiv   1916
		77  j_v(iout)=ns
	endif !if(narg.eq.0)   1911
	!	io=io+narg+3
	return
end subroutine!subroutine partweights(iob,io)


!unit
subroutine jlpunit(iob,io)   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: p_nunits
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**unit: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   1930
	ibas=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted
	if(ibas.le.0.or.ibas.gt.p_nunits+p_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access weight',ibas, 'maximum is:',p_nunits+p_ndiv
	else !if(ibas.le.0.or.ibas.gt.p_nunits+j_ndiv)then
		! bas numbering: for each unit count the number of parts, subtract one
		! add number of units
		niv=0 ! count of (numb of parts-1)
		iunitv=-1
 
		do j=1,p_ndiv
			! ib=p_nunits before
			iunit=p_unitdiv(j) !o(isol)%i(p_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			! write(6,*)'j,unit,niv',j,iunit,niv'
			ib=iunit+niv
			if(ib.ge.ibas)then
				iunit=iunit-(ib-ibas)
				goto 77
			endif !if(ib.ge.ibas)   1951
			iunitv=iunit
		enddo !j=1,p_ndiv   1945
		! all
		iunit=ibas-niv
		77  j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=iunit
 
	endif !if(ibas.le.0.or.ibas.gt.p_nunits+p_ndiv)   1936
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine jlpunit(iob,io)

function lunit_(ibas)    !!!!
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
 
	integer ibas ! for which the unit is wanted
	! bas numbering: for each unit count the number of parts, subtract one
	! add number of units
 
	niv=0 ! count of (numb of parts-1)
	iunitv=-1
 
	do j=1,p_ndiv
		! ib=p_nunits before
		iunit=p_unitdiv(j) !o(isol)%i(p_nunits+j)
		if(iunit.eq.iunitv)niv=niv+1
		! write(6,*)'j,unit,niv',j,iunit,niv'
		ib=iunit+niv
		if(ib.ge.ibas)then
			iunit=iunit-(ib-ibas)
			goto 77
		endif !if(ib.ge.ibas)   1981
		iunitv=iunit
	enddo !j=1,p_ndiv   1975
	! all
	iunit=ibas-niv
	77 lunit_=iunit
 
	return
end function!function lunit_(ibas)

subroutine partunit(iob,io)   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: j_otype
	use jmod, only: j_ipreal
	use jmod, only: j_del
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**partunit: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   1996
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	idiv=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted
	if(idiv.le.0.or.idiv.gt.p_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access partunit',idiv, 'maximum is:',p_ndiv
	else !if(idiv.le.0.or.idiv.gt.j_ndiv)then
		j_v(iout)=p_unitdiv(idiv) !o(isol)%i(p_nunits+idiv)
		! all
	endif !if(idiv.le.0.or.idiv.gt.p_ndiv)   2005
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine !subroutine partunit(iob,io)

subroutine weight(iob,io)   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: p_nunits
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_wdiv
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**weight: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2017
	ibas=j_v(j_o(iob)%i(io+2))  ! for which the weight is wanted
	if(ibas.le.0.or.ibas.gt.p_nunits+p_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access weight',ibas, 'maximum is:',p_nunits+p_ndiv
	else !if(ibas.le.0.or.ibas.gt.p_nunits+j_ndiv)then
 
		niv=0 ! count of (numb of parts-1)
		iunitv=-1
		do j=1,p_ndiv
			iunit=p_unitdiv(j) !o(isol)%i(p_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			ib=iunit+niv
			if(ib.eq.ibas)then
				wei=p_wdiv(j) ! o(isol)%r(j)
				goto 77
			else if(ib.gt.ibas)then !if(ib.eq.ibas)then
				wei=1.   !are
				goto 77
			endif !if(ib.eq.ibas)   2034
			iunitv=iunit
		enddo !j=1,p_ndiv   2030
		! all
		wei=1.
		77  j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei
 
	endif !if(ibas.le.0.or.ibas.gt.p_nunits+p_ndiv)   2023
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine weight(iob,io)

! weight -funktion melaoptj versio
function we_(ibas)    !!!!
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_wdiv
 
	integer ibas ! for which the weight is wanted
	niv=0 ! count of (numb of parts-1)
	iunitv=-1
	do j=1,p_ndiv
		! ib=p_nunits before
		iunit=p_unitdiv(j) !o(isol)%i(p_nunits+j)
		if(iunit.eq.iunitv)niv=niv+1
		ib=iunit+niv
		if(ib.eq.ibas)then
			wei=p_wdiv(j) ! o(isol)%r(j)
			goto 77
		else if(ib.gt.ibas)then !if(ib.eq.ibas)then
			wei=1.   !are
			!  iunit=ibas-nivv
			goto 77
		endif !if(ib.eq.ibas)   2063
		iunitv=iunit
	enddo !j=1,p_ndiv   2058
	! all
	wei=1.
	77  we_=wei
	return
end function!function we_(ibas)

subroutine schedweight(ibas,is,wei) !returns for ibas'th schedule twith nonzero weight the   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: p_nunits
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_wdiv
	use jmod, only: p_ibaunit
	use jmod, only: p_isdiv
	use jmod, only: p_keys
	! schedule number is (cumulative) and the weight w
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**schedweight: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2081
	!  ibas for which the weight is wanted
	if(ibas.le.0.or.ibas.gt.p_nunits+p_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access weight',ibas, 'maximum is:',p_nunits+p_ndiv
	else !if(ibas.le.0.or.ibas.gt.p_nunits+j_ndiv)then
 
		niv=0 ! count of (numb of parts-1)
		iunitv=-1
		do j=1,p_ndiv
			! ib=p_nunits before
			iunit=p_unitdiv(j) !o(isol)%i(p_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			ib=iunit+niv
			if(ib.eq.ibas)then
				wei=p_wdiv(j) ! o(isol)%r(j)
				is=p_ibaunit(iunit)+p_isdiv(j)
				goto 77
			else if(ib.gt.ibas)then !if(ib.eq.ibas)then
				iunit=iunit-(ib-ibas)
				is=p_ibaunit(iunit)+p_keys(iunit)
				wei=1.   !are
				goto 77
			end if !if(ib.eq.ibas)   2099
			iunitv=iunit
		end do !j=1,p_ndiv   2094
		! all
		wei=1.
		iunit=ibas-niv;is=p_ibaunit(iunit)+p_keys(iunit)
		77  continue !  v(o(iob)%i(io+o(iob)%i(io+1)+2))=wei
 
	end if !if(ibas.le.0.or.ibas.gt.p_nunits+p_ndiv)   2087
 
	return
end subroutine!subroutine schedweight(ibas,is,wei)


subroutine weightschedcum(iob,io)   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: j_dnobs
	use jmod, only: p_nunits
	use jmod, only: p_ibaunit
	use jmod, only: j_isoption
	use jmod, only: j_minteger
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_wdiv
	use jmod, only: p_isdiv
	use jmod, only: j_0
	use jmod, only: p_keys
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**weightschedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2123
	isc=j_v(j_o(iob)%i(io+2))  ! for which the weight is wanted
	if(isc.le.0.or.isc.gt.j_dnobs)then
		j_err=.true.
		write(6,*)'**weight%schedcum: trying to access weight',isc, 'maximum is:',j_dnobs
		return
	endif !if(isc.le.0.or.isc.gt.j_dnobs)   2129
 
	iunit0=1+j_dnobs/p_nunits;iunit0=min(p_nunits,iunit0)
	if(isc.le.p_ibaunit(iunit0))then
		do iunit=iunit0-1,1,-1
			if(isc.gt.p_ibaunit(iunit))exit
		enddo !iunit=iunit0-1,1,-1   2137
	else !if(isc.le.j_ibaunit(iunit0))then
		do iunit=iunit0,p_nunits
			if(isc.le.p_ibaunit(iunit+1))exit
		enddo !iunit=iunit0,p_nunits   2141
	endif !if(isc.le.p_ibaunit(iunit0))   2136
	is=isc-p_ibaunit(iunit)
 
	if(j_isoption(iob,io,j_minteger))then
		!!call j_clearoption(iob,io)  ! weightschedcum(iob,io)
		do j=1,p_ndiv
			! ib=p_nunits before
			if(p_unitdiv(j).eq.iunit)then
				jmax=j
				do j2=j+1,p_ndiv
					if(p_unitdiv(j2).ne.iunit)exit
					if(p_wdiv(j2).gt.p_wdiv(jmax))jmax=j2
				enddo !j2=j+1,p_ndiv   2153
				if(is.eq.p_isdiv(jmax))then
					wei=1.
				else !if(is.eq.j_isdiv(jmax))then
					wei=j_0
					goto 90
				endif !if(is.eq.p_isdiv(jmax))   2157
			endif !if(p_unitdiv(j).eq.iunit)   2151
		enddo !j=1,p_ndiv   2149
 
	else !if(j_linkoption(iob,io,j_minteger).gt.0)then
 
		do j=1,p_ndiv
			! ib=p_nunits before
			if(p_unitdiv(j).eq.iunit)then
				do j2=j,p_ndiv
					if(p_unitdiv(j2).ne.iunit)exit
					if(is.eq.p_isdiv(j2))then
						wei=p_wdiv(j2)
						goto 90
					endif !if(is.eq.p_isdiv(j2))   2173
				enddo !j2=j,p_ndiv   2171
				wei=j_0
				goto 90
			endif !if(p_unitdiv(j).eq.iunit)   2170
		enddo !j=1,p_ndiv   2168
	endif !if(j_isoption(iob,io,j_minteger))   2147
 
	if(is.eq.p_keys(iunit))then
		wei=1.
	else !if(is.eq.j_keys(iunit))then
		wei=j_0
	endif !if(is.eq.p_keys(iunit))   2184
	90 continue
 
	j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei
 
	!	io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine weightschedcum(iob,io)


subroutine integersched(iob,io)  !both schedw andf schedcum   !!!! EI TOIMI
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: p_nunits
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_wdiv
	use jmod, only: p_isdiv
	use jmod, only: p_keys
	use jmod, only: p_ibaunit
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**integersched: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2199
	iunit=j_v(j_o(iob)%i(io+2))
	if(iunit.le.0.or.iunit.gt.p_nunits)then
		j_err=.true.
		write(6,*)'**weight%schedw: trying to access unit',iunit, 'maximum is:',p_nunits
		return
	endif !if(iunit.le.0.or.iunit.gt.p_nunits)   2205
 
	do j=1,p_ndiv
		! ib=p_nunits before
		if(p_unitdiv(j).eq.iunit)then
			jmax=j
			do j2=j+1,p_ndiv
				if(p_unitdiv(j2).ne.iunit)exit
				if(p_wdiv(j2).gt.p_wdiv(jmax))jmax=j2
			enddo !j2=j+1,p_ndiv   2215
			wei=p_isdiv(jmax)
			goto 90
		endif !if(p_unitdiv(j).eq.iunit)   2213
	enddo !j=1,p_ndiv   2211
 
	wei=p_keys(iunit)
 
	90 continue
	if(j_o(iob)%i(io).eq.jifintegerschedcum)wei=wei+p_ibaunit(iunit) ! ?j3
 
	j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei
 
	!	io=io+j_o(iob)%i(io+1)+3
 
	return
end subroutine !subroutine integersched(iob,io)


subroutine weightschedw(iob,io)  !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: p_nunits
	use jmod, only: p_ns
	use jmod, only: j_isoption
	use jmod, only: j_minteger
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_wdiv
	use jmod, only: p_isdiv
	use jmod, only: j_0
	use jmod, only: p_keys
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**weightschedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2239
 
	iunit=j_v(j_o(iob)%i(io+2))
	if(iunit.le.0.or.iunit.gt.p_nunits)then
		j_err=.true.
		write(6,*)'**weight%schedw: trying to access unit',iunit, 'maximum is:',p_nunits
		return
	endif !if(iunit.le.0.or.iunit.gt.p_nunits)   2246
	is=j_v(j_o(iob)%i(io+3))  ! for which the weight is wanted
	if(is.le.0.or.is.gt.p_ns(iunit))then
		j_err=.true.
		write(6,*)'**weight%schedw: trying to access sched',is, 'maximum is:',p_ns(iunit)
		return
	endif !if(is.le.0.or.is.gt.p_ns(iunit))   2252
 
	if(j_isoption(iob,io,j_minteger))then
		!!call j_clearoption(iob,io)  ! ubroutine weightschedw(iob,io)
		do j=1,p_ndiv
			! ib=p_nunits before
			if(p_unitdiv(j).eq.iunit)then
				jmax=j
				do j2=j+1,p_ndiv
					if(p_unitdiv(j2).ne.iunit)exit
					if(p_wdiv(j2).gt.p_wdiv(jmax))jmax=j2
				enddo !j2=j+1,p_ndiv   2264
				if(is.eq.p_isdiv(jmax))then
					wei=1.
				else !if(is.eq.j_isdiv(jmax))then
					wei=j_0
					goto 90
				endif !if(is.eq.p_isdiv(jmax))   2268
			endif !if(p_unitdiv(j).eq.iunit)   2262
		enddo !j=1,p_ndiv   2260
 
	else !if(j_linkoption(iob,io,j_minteger).gt.0)then
 
		do j=1,p_ndiv
			! ib=p_nunits before
			if(p_unitdiv(j).eq.iunit)then
				do j2=j,p_ndiv
					if(p_unitdiv(j2).ne.iunit)exit
					if(is.eq.p_isdiv(j2))then
						wei=p_wdiv(j2)
						goto 90
					endif !if(is.eq.p_isdiv(j2))   2284
				enddo !j2=j,p_ndiv   2282
				wei=j_0
				goto 90
			endif !if(p_unitdiv(j).eq.iunit)   2281
		enddo !j=1,p_ndiv   2279
 
	endif !if(j_isoption(iob,io,j_minteger))   2258
 
	if(is.eq.p_keys(iunit))then
		wei=1.
	else !if(is.eq.j_keys(iunit))then
		wei=j_0
	endif !if(is.eq.p_keys(iunit))   2296
	90 continue
 
	j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei
 
	!	io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine weightschedw(iob,io)


subroutine partweight(iob,io)  !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: j_otype
	use jmod, only: j_ipreal
	use jmod, only: j_del
	use jmod, only: p_ndiv
	use jmod, only: p_wdiv
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**partweight: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2312
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	idiv=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted
	if(idiv.le.0.or.idiv.gt.p_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access partunit',idiv, 'maximum is:',p_ndiv
	else !if(idiv.le.0.or.idiv.gt.j_ndiv)then
		j_v(iout)=p_wdiv(idiv) !o(isol)%r(idiv)
		! all
	endif !if(idiv.le.0.or.idiv.gt.p_ndiv)   2321
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine !subroutine partweight(iob,io)

subroutine schedw(iob,io)  !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: p_nunits
	use jmod, only: p_lx0
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_isdiv
	use jmod, only: p_keys
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**schedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2333
	ibas=j_v(j_o(iob)%i(io+2))  ! for which the weight is wanted
	if(ibas.le.0.or.ibas.gt.p_nunits+p_lx0)then
		j_err=.true.
		write(6,*)'**trying to access schedw',ibas, 'maximum is:',p_nunits+p_ndiv
	else !if(ibas.le.0.or.ibas.gt.p_nunits+j_ld0)then
		niv=0 ! count of (numb of parts-1)
		iunitv=-1
		do j=1,p_ndiv
			! ib=p_nunits before
 
			iunit=p_unitdiv(j) ! o(isol)%i(p_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			ib=iunit+niv
			if(ib.eq.ibas)then
				is=p_isdiv(j) ! o(isol)%i(p_nunits+ndiv+j)
				goto 77
				!  eivät ole sortattu
			else if(ib.gt.ibas)then !if(ib.eq.ibas)then
				iunit=iunit-(ib-ibas)
				is=p_keys(iunit) ! o(isol)%i(iunit)
				goto 77
			end if !if(ib.eq.ibas)   2351
			iunitv=iunit
		end do !j=1,p_ndiv   2345
		! all
		iunit=ibas-niv;is=p_keys(iunit) ! o(isol)%i(iunit)
		77 j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=is
 
	end if !if(ibas.le.0.or.ibas.gt.p_nunits+p_lx0)   2339
 
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine schedw(iob,io)

! schewd melaoptj versio
function ls_(ibas)   !!!!
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_isdiv
	use jmod, only: p_keys
	! weight()
 
	integer ibas ! for which the weight is wanted
	niv=0 ! count of (numb of parts-1)
	iunitv=-1
	do j=1,p_ndiv
		! ib=p_nunits before
 
		iunit=p_unitdiv(j) ! o(isol)%i(p_nunits+j)
		if(iunit.eq.iunitv)niv=niv+1
		ib=iunit+niv
		if(ib.eq.ibas)then
			is=p_isdiv(j) ! o(isol)%i(p_nunits+ndiv+j)
			goto 77
			!  eivät ole sortattu
		else if(ib.gt.ibas)then !if(ib.eq.ibas)then
			iunit=iunit-(ib-ibas)
			is=p_keys(iunit) ! o(isol)%i(iunit)
			goto 77
		end if !if(ib.eq.ibas)   2385
		iunitv=iunit
	end do !j=1,p_ndiv   2379
	! all
	iunit=ibas-niv;is=p_keys(iunit) ! o(isol)%i(iunit)
	77 ls_=is
 
	return
end function!function ls_(ibas)

subroutine schedcum(iob,io)  !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: p_nunits
	use jmod, only: p_lx0
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_ibaunit
	use jmod, only: p_isdiv
	use jmod, only: p_keys
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**schedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2405
 
	ibas=j_v(j_o(iob)%i(io+2))  ! for which the weight is wanted
	if(ibas.le.0.or.ibas.gt.p_nunits+p_lx0)then
		j_err=.true.
		write(6,*)'**trying to access schedcum',ibas, 'maximum is:',p_nunits+p_lx0
	else !if(ibas.le.0.or.ibas.gt.p_nunits+j_ld0)then
		niv=0 ! count of (numb of parts-1)
		iunitv=-1
		do j=1,p_ndiv
 
			iunit=p_unitdiv(j) !o(isol)%i(p_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			ib=iunit+niv
			if(ib.eq.ibas)then
				is=p_ibaunit(iunit)+p_isdiv(j)
				goto 77
			else if(ib.gt.ibas)then !if(ib.eq.ibas)then
 
				iunit=iunit-(ib-ibas)
				is=p_ibaunit(iunit)+p_keys(iunit) ! o(isol)%i(iunit)+o(isol)%i2(4+iunit)
				goto 77
			end if !if(ib.eq.ibas)   2423
			iunitv=iunit
		end do !j=1,p_ndiv   2418
		! all
		iunit=ibas-niv;is=p_ibaunit(iunit)+p_keys(iunit) !o(isol)%i(iunit)+ o(isol)%i2(4+iunit)
 
		77  j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=is
 
	end if !if(ibas.le.0.or.ibas.gt.p_nunits+p_lx0)   2412
 
	90 return
!	io=io+j_o(iob)%i(io+1)+3
!	return
end subroutine!subroutine schedcum(iob,io)

subroutine partschedw(iob,io)   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: j_otype
	use jmod, only: j_ipreal
	use jmod, only: j_del
	use jmod, only: p_ndiv
	use jmod, only: p_isdiv
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**partschedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2448
	narg=j_o(iob)%i(io+1)
 
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	idiv=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted
	if(idiv.le.0.or.idiv.gt.p_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access partsched',idiv, 'maximum is:',p_ndiv
	else !if(idiv.le.0.or.idiv.gt.j_ndiv)then
 
		j_v(iout)=p_isdiv(idiv) !o(isol)%i(p_nunits+ndiv+idiv)
		! all
 
	endif !if(idiv.le.0.or.idiv.gt.p_ndiv)   2459
	return
	!io=io+j_o(iob)%i(io+1)+3
	!	return
end subroutine!subroutine partschedw(iob,io)

subroutine partschedcum(iob,io)   !!!!
	use jmod, only: j_v
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_err
	use jmod, only: j_o
	use jmod, only: j_otype
	use jmod, only: j_ipreal
	use jmod, only: j_del
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_ibaunit
	use jmod, only: p_isdiv
	if(j_v(p_ivstartedjlp).eq.0)then
 
		write(6,*)'**partschedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2474
	narg=j_o(iob)%i(io+1)
	isol=j_o(iob)%i(io+2)
 
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	idiv=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted
 
	if(idiv.le.0.or.idiv.gt.p_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access partsched',idiv, 'maximum is:',p_ndiv
	else !if(idiv.le.0.or.idiv.gt.j_ndiv)then
		iunit=p_unitdiv(idiv) !o(isol)%i(p_nunits+idiv)
		j_v(iout)=p_ibaunit(iunit)+p_isdiv(idiv) !o(isol)%i(p_nunits+ndiv+idiv)+o(isol)%i2(4+iunit)
		! all
 
	endif !if(idiv.le.0.or.idiv.gt.p_ndiv)   2488
	return
	!	90 io=io+narg+3
	!	return
end subroutine!subroutine partschedcum(iob,io)




integer function integerschedw(iunit)  ! %%integer solution   !!!!
	use jmod, only: p_nunits
	use jmod, only: j_err
	use jmod, only: p_ndiv
	use jmod, only: p_unitdiv
	use jmod, only: p_wdiv
	use jmod, only: p_isdiv
	use jmod, only: p_keys
	integer,intent(in) ::iunit
	!could be used in subroutine integersched(iob,io)
	integer wei
	if(iunit.le.0.or.iunit.gt.p_nunits)then
		j_err=.true.
		write(6,*)'*j* integersched trying to access unit',iunit, 'maximum is:',p_nunits
		integerschedw=0
		return
	endif !if(iunit.le.0.or.iunit.gt.p_nunits)   2509
 
	do j=1,p_ndiv
		! ib=p_nunits before
		if(p_unitdiv(j).eq.iunit)then
			jmax=j
			do j2=j+1,p_ndiv
				if(p_unitdiv(j2).ne.iunit)exit
				if(p_wdiv(j2).gt.p_wdiv(jmax))jmax=j2
			enddo !j2=j+1,p_ndiv   2520
			wei=p_isdiv(jmax)
			goto 90
		endif !if(p_unitdiv(j).eq.iunit)   2518
	enddo !j=1,p_ndiv   2516
 
	wei=p_keys(iunit)
 
	90 continue
	integerschedw=wei
 
	return
end function !integer function integerschedw(iunit)

!factory3
! isfvar kesken: kuljetuskustannukset matriisina käsittely puuttuu
! Analysoidaan tehdasmuuttujien sisältö
! In: 	iv 			- probleeman muuttujan indeksi
!	Out:	itrans 	- muuttujaan (...%%...%%...) liittyvän muunnosjoukko-olion indeksi
!				imat		- (varaus) muuttujan kuljetuskustannusdata (luultavasti)
!				iv2			- iv-muuttujan toiseksi viimeistä osaa (...%%***%%...) vastaavan olion indeksi
!				iv3			- iv-muuttujan viimeistä osaa (...%%...%%***) vastaavan olion indeksi
! ! 			(Jos muuttuja kolmiosainen, ensimmäinen osa ei ole olio) - ??
! subroutine isfvar(iv,itrans,imat,len1,iv2,iv3)  !!!!
! character*24 name
! integer ivt(4)
! ibas=j_o(j_ivnames)%i(iv)-1
! le=j_o(j_ivnames)%i(iv+1)-ibas-1 !le= the length of rest
! np=0
! iv1=0;iv2=0;iv3=0
! itrans=0
! imat=0

! 1000 do i=2,le-2
! if((j_o(j_ivnames)%ch(ibas+i).eq.'%'.and.j_o(j_ivnames)%ch(ibas+i+1).eq.'%'))goto 1
! end do !0 do i=2,le-2   2970
! if(np.eq.0)return

! i=le+1

! 1 do j=1,i-1
! name(j:j)=j_o(j_ivnames)%ch(ibas+j)
! end do !o j=1,i-1   2977
! np=np+1
! if(np.eq.1)len1=i-1  ! length of the first part before '%'

! if(np.gt.3)then
! write(6,*)'**too many %% in a name of problem'
! j_err=.true.
! return
! endif !if(np.gt.3)   2983
! ivt(np)=j_object(name(1:i-1))

! ibas=ibas+i+1
! le=le-(i+1)
! if(le.gt.0)goto 1000

! if(np.eq.2)then
! !  pulp#"j"%%@pulpfactory(i)<pcapacity%@pulpfactory(i)
! iv3=ivt(2)
! iv2=ivt(1)
! iv1=0
! if(iv2.le.0)then
! call j_printname('**first part unknown ',iv,' ');
! j_err=.true.
! endif !if(iv2.le.0)   2999
! if(iv3.le.0)then
! call j_printname('**second part unknown ',iv,' ');
! j_err=.true.
! endif !if(iv3.le.0)   3003
! if(j_err)return

! else !if(np.eq.2)then

! ! util%%sawlog%%sawmill
! iv2=ivt(2)
! iv3=ivt(3)

! if(iv2.le.0)then
! call j_printname('**second part unknown ',iv,' ');
! j_err=.true.
! endif !if(iv2.le.0)   3015
! if(iv3.le.0)then
! call j_printname('**third part unknown ',iv,' ');
! j_err=.true.
! endif !if(iv3.le.0)   3019
! if(j_err)return
! if(j_otype(iv2).ne.j_iplist)then
! call j_printname('**second part is not a list ',iv,' ')
! j_err=.true.
! endif !if(j_otype(iv2).ne.j_iplist)   3024
! if(j_otype(iv2).ne.j_iplist)then
! call j_printname('**third part is not a list ',iv,' ')
! j_err=.true.
! endif !if(j_otype(iv2).ne.j_iplist)   3028

! itrans=j_object2('trans%',iv)

! if(itrans.le.0)then
! call j_printname('**transformation trans%',iv,' does not exist')
! j_err=.true.
! endif !if(itrans.le.0)   3035
! if(j_err)return
! if(j_otype(itrans).ne.j_iptrans)then
! call j_printname('**object ',itrans,' is not a transformation')
! j_err=.true.
! return
! endif !if(j_otype(itrans).ne.j_iptrans)   3040

! return
! endif !if(np.eq.2)   2994
! end subroutine !subroutine isfvar(iv,itrans,imat,len1,iv2,iv3)

! pack2
! Korvaa bugisen pack-funktion, joka joskus kaatuu kun palautetaan
! samaan vektoriin joka on parametrina
! nterm: vektoreiden itarget ja is koko
subroutine pack2(itarget, is, n)    !!!!
	integer itarget(*)
	logical is(*)
	integer n
 
	i2_= 0
	do i_= 1,n
		if(is(i_)) then
			i2_=i2_+1
			itarget(i2_)=itarget(i_)
		endif !if(is(i_))   2647
	enddo !i_= 1,n   2646
	return
end subroutine !subroutine pack2(itarget, is, n)



subroutine jlpz(iob,io)
	use jmod, only: p_n16
	use jmod, only: j_v
	use jmod, only: j_ivdollar2
	use jmod, only: p_isn16
	use jmod, only: p_pivotrz
	use jmod, only: p_fast
	use jmod, only: p_p
	use jmod, only: p_xpresent
	use jmod, only: p_wasfeasible
	use jmod, only: p_isz
	use jmod, only: j_startfunction
	use jmod, only: j_arg
	use jmod, only: j_ivout
	use jmod, only: j_err
	use jmod, only: j_igetopt
	use jmod, only: j_mprint
	use jmod, only: p_iprint
	use jmod, only: p_ncol
	use jmod, only: p_nvar
	use jmod, only: p_nz
	use jmod, only: p_ivproblem
	use jmod, only: p_ncoltot
	use jmod, only: p_nrow
	use jmod, only: p_nrowz
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_time0
	use jmod, only: p_time00
	use jmod, only: j_0
	use jmod, only: p_ivoptimal
	use jmod, only: p_ivfeasible
	use jmod, only: j_1
	use jmod, only: p_ivstartedjlp
	use jmod, only: p_objfv
	use jmod, only: p_small
	use jmod, only: p_objf
	use jmod, only: p_maxo
	use jmod, only: p_rhsw
	use jmod, only: p_rhscur
	use jmod, only: p_ebou
	use jmod, only: p_rhs
	use jmod, only: p_lower
	use jmod, only: p_lbou
	use jmod, only: p_rhs2
	use jmod, only: p_x
	use jmod, only: p_issurplus
	use jmod, only: p_enter
	use jmod, only: p_feasible
	use jmod, only: p_xps00
	use jmod, only: p_pivot
	use jmod, only: p_coefmax
	use jmod, only: p_lr0
	use jmod, only: p_lz0
	use jmod, only: p_nnf
	use jmod, only: j_printname
	use jmod, only: p_nureport
	use jmod, only: p_ivreport
	use jmod, only: p_echo
	use jmod, only: p_buf
	use jmod, only: j_ivcpu
	use fletdmod
	use fletdmod2
	use fletcherdmod
	logical dorz
	p_n16=16
	p_isn16=j_v(j_ivdollar2).eq.157.
	p_pivotrz=0
	if(p_isn16)write(p_n16,*)'jlpz()'
	p_fast=.false.
 
	! implicit none
	! integer nout,i,irow,irowinf,j
	! common/noutc/nout
 
	!Section jlpz jlpz() for an ordinary Lp-problem.
	!The problem defined in the problem() function can be given in problem-> or by giving values
	!for max-> or min->, and zmatriz->, rhs-> and rhs2-> options.
	!endheader
	! Option
	! Output &1 &  & Theres is no jlpz -object, but the output is used to name
	!several objects created with the function. The list of created objects can be seen
	! with \\
	!outlist=;list(Output%?);\\
	!The objects can then be be seen with \\
	!@outlist;\\
	! The objects created can be used in debugging the algorithm and also in teaching how the alogorithm
	! proceeds. This will demonstrated later.
	!problem& -1|1&PROB&Problem defined in problem(). If problem-> is not present
	! the following 3 options must be present and either min-> or max->.
	!zmatrix &-1|1&MATRIX& Constraint matrix.
	!rhs &-1|1&MATRIX& Lower bounds as row or column vector having as many elements as there are
	! 	rows in the matrix given in zmatrix->. Use value -1.e20 to indicate that there is no lower bound.
	!rhs2 &-1|1&MATRIX& Upper bounds as row or column vector having as many elements as there are
	! 	rows  in the matrix given in zmatrix->. Use value 1.e20 to indicate that there is no upper bound.
	!max&-1|1&MATRIX& The objective vector for a maximization problem. It must have as many elements as the
	! constraint matrix has columns.
	!min&-1|1&MATRIX& The objective vector for a minimization problem.
	!dpivot&-1|1&REAL& The objective function etc are printed after  dpivot pivots.
	!debug&-1|0|1&REAL& NOT UP TO DATE Gives the value of Pivot at which a pause is generated. During the pause all essential
	! matrices can be studied. Pure debug-> is the same as debug->0, which implies that pause is generated before
	! pivoting. If variable Debug is given a new value, the the next pause is generated when Pivot.eq.Debug. The default is that
	! the next pause is generated after the next pivot. The pause() function can now use also \\
	!lis=;list(Output%?); \\
	! even if ;list is input programming function which are not otherwise allowed during pause().
	!
	!endoption
	!Note The ordinary Lp-algorithm can be taught using matrices generated in pause to show how the algorithm proceeds.
	!endnote
	!Ex jlpzex Problem with only z-variables.
	! probza=problem()
	! 2*x1+x2+3*x3-2*x4+10*x5==min
	! x1+x3-x4+2*x5=5
	! x2+2*x3+2*x4+x5=9
	! x1<7
	! x2<10
	! x3<1
	! x4<5
	! x5<3
	! /
	! probzalist=;list(probza%?); !subobjects created
	! @probzalist; !printing the subobjects
 
	!jlpza=jlpz(problem->probza,dpivot->1)
	! jlpzalist=;list(jlpza%?);
	! @jlpzalist;
	! ** The same problem is defined using different tools available.
	!
	! probzb=problem()
	! 2*x1+x2+x34c*x34+10*x5==min
	! x1+x3-x4+(2+0)*x5=5
	! x2+2*x3+2*x4+x5=9
	! x1<7
	! x2<i10
	! x3<'1+zero'
	! x4<5
	! x5<3
	! /
	! 	x34=list(x3,x4)
	! x34c=matrix(1,2,values->(3,-2))
	!i10=10
	!zero=0
	!jlpzb=jlpz(problem->probzb,dpivot->1)
	!**Now different problem is obtained
	!x34c=matrix(1,2,values->(3,-3))
	!zero=1
	!jlpzb=jlpz(problem->probzb,dpivot->1)
	!**
	! **The matrices needed to use jlpz without problem-> can be obtained from a problem as follows
	! jlpcoefa=jlpcoef(probza)
	!jlpcoefalist=;list(jlpcoefa%?);
	!jlpza=jlpz(zmatrix->jlpcoefa%matrix,rhs->jlpcoefa%rhs,rhs2->jlpcoefa%rhs2,min->jlpcoefa%objrow)
	!li=;list(jlpza%?);
	!jlpza%zvalues,jlpza%rows,jlpza%shprice,jlpza%rows,jlpza%obj;
	!endex
	!endsection
 
 
	! ** The matrices can be obtained from the problem using jlpcoef() and then used as an input to jlpz()
 
	p_p=j_v(j_ivdollar2).eq.8.d0
	p_xpresent=.false.
	p_wasfeasible=.false.
	p_isz=.true.
	!p_isobj0=.false.
	!p_isobj2=.false.
	!	p_xpresent2=.false.
	call j_startfunction(iob,io,0,narg,j_arg,j_ivout,needsout=.true.)
	if(j_err)return
	ivprint=j_igetopt(iob,io,j_mprint)
 
	if(ivprint.gt.0)then
 
		p_iprint=j_v(ivprint)
	elseif(ivprint.eq.0)then !if(ivprint.gt.0)then
		p_iprint=2 !testauksen ajaksi
	else !if(ivprint.gt.0)then
		p_iprint=1
	endif !if(ivprint.gt.0)   2768
	call start(iob,io) !jlpz
	p_ncol=p_nvar !jlpz
	p_nz=p_ncol
 
	!	p_row0=1   !updated for domainprob
	!p_ivproblem=j_optarg0(1)
	if(p_ivproblem.le.0)then
		call zmatrix(iob,io)
 
 
		if(j_err)return
 
	else
 
 
	endif !if(p_ivproblem.le.0)   2782
 
 
	!write(6,*)'p_nz',p_nz
	!	write(6,*)'p_pivotstep',p_pivotstep
	!	p_nz=p_nvar
	!	p_ncol=p_nz
	write(6,*)'z-variables',p_nz
	!	read(5,*)dhhd
	! jlpz(  basis (first element -1) of D part, when I is included
	! p_nrow2z=p_ncol+p_nrow
	! p_nrowz=p_nz
	! p_nrow2z=p_nrowz+p_nrow
	! p_zopt=.true.
	!p_ncol=p_nz
	p_ncoltot=p_ncol+p_nrow
	p_nrowz=p_ncoltot
	call commonopt(iob,io)  !jlpz
	if(j_err)return
	!	write(6,*)'after commonopt'
	if(p_nrow.eq.0)then
		write(6,*)'jlpz() needs constraints'
		j_err=.true.;return
	endif !if(p_nrow.eq.0)   2811
 
	call startlist0()   !jlpz()
	call initz0()
	call initzjlpz()
 
 
	if(j_err)return
	! p_ivls p_ls
	! p_ivlsi p_lsi
	! p_ivlr p_lr
	! p_ivlri p_lri
	! p_ivobjr0
	! p_objr0
	! p_ivobjr2
	! p_objr2
	! p_ivlz
	! p_lz
	! p_ivlx
	! p_lx
	! p_ivlxi
	! p_lxi
	! p_ivlf
	! p_lf
	! p_ivlfi
	! p_lfi
	! p_ivvc
	! p_vc
	! p_ivx
	! p_x
	! p_ivb
	! p_b
	! p_xmi
	! p_xma
	! p_iva
	! p_a
 
	!call initzjlp()  !in jlpz()
 
	if(j_err)return
 
	call initfltot()  !jlpz(
	if(j_err)return
	if(p_isn16)then
		write(p_n16,*)'z matrix'
		do ii=1,p_nrow
			write(p_n16,'(i2,(15f7.3))')ii,(p_a(p_abas(iz)+ii),iz=1,p_nz)
		enddo !ii=1,p_nrow   2859
	endif !if(p_isn16)   2857
 
 
	ival=0
	!write(6,*)'p_nterminrow',p_nterminrow(0:p_nrow)
	!write(6,*)'vars',p_varofterm
	!write(6,*)'coef',p_coef
	!write(6,*)'zvarsf',p_zvars(1:p_nz)
 
	call cpu_time(p_time0)
 
	p_time00=p_time0
 
	j_v(p_ivoptimal)=j_0
	j_v(p_ivfeasible)=j_0
 
	j_v(p_ivstartedjlp)=j_1 !!!
 
	!write(6,*)'p_ls',p_ls,'lu1',lu1,ll1,p_ifail,nrefac
 
	!	p_objf=j_0
	p_objfv=p_small
	p_objf = p_small
	if(.not.p_maxo)write(6,*)'**minimization: maximize -objective'
 
 
	write(6,*)' '
	write(6,*)'** Resid = # of basic residuals       z = # of basic z-variables'
	!	write(6,*)'   sched = # of explicit basic scheds  xkf = # of basic factory transportations'
	write(6,*)'      NF = # of nonfeafible rows      When NF>0 objective is a temporal objective'
	write(6,*)' '
	! write(6,*) 'Pivots      Objective     Change%  resid   z     NF     dCPU       CPU'
	! write(6,*)' ',('_',kk=1,80)
	p_rhsw=>p_rhscur
 
	do irow=1,p_nrow
		if(p_ebou(irow))then
			if(p_rhs(irow).gt.j_0)then
				p_lower(irow)=.false.
			else
 
				p_lower(irow)=.true.
			endif !if(p_rhs(irow).gt.j_0)   2899
			p_rhscur(irow)=p_rhs(irow)
		elseif(p_lbou(irow))then
			p_rhscur(irow)=p_rhs(irow)
			p_lower(irow)=.true.
 
		else
			p_rhscur(irow)=p_rhs2(irow)
			p_lower(irow)=.false.
		endif !if(p_ebou(irow))   2898
	enddo !irow=1,p_nrow   2897
	p_x(1:p_nrow)=p_rhscur  !posit =slack negat surplus
	p_issurplus=p_x.le.j_0
 
 
 
	! call leftvec(p_rhsw,p_x)
	! write(6,*)'px',p_x
	!write(p_n16,*)'aftpivothere'
 
	call aftpivot()
	if(j_err)return
	call isfeasible()   ! jlpz( initially
 
	write(6,*) 'Pivots      Objective     Change%  resid   z     NF     dCPU       CPU'
	write(6,*)' ',('_',kk=1,80)
	p_enter=.false.
	dorz=.true.
	p_pivotrz=0
 
	do while(dorz)
		dorz=.false.
		call zenter(dorz)  !jlpz
		!p_enter(2)=.false.
		!	write(16,*)' '
		!write(16,*)'dorzobj',dorz,p_objf
		call renter(dorz)
		!	p_enter(1)=.false.
		!write(16,*)'dorzobjaftr',dorz,p_objf
	enddo !while(dorz)   2934
 
800	call cpu_time(time)
	!write(6,*)'timetas',time,p_time0,p_time00
	iminc=(time-p_time0)/60.
	imint=(time-p_time00)/60.
	secd=time-p_time0-iminc*60.
	sect=time-p_time00-imint*60.
	p_time0=time
	if(p_feasible)then
		j_v(p_ivoptimal)=j_1
 
		pros=abs(100.*(p_objf-p_objfv)/(p_objfv+p_xps00))
		write(6,'(i8,g19.12,1x,f5.2,3i6,4x,i3,a,f5.2,i5,a,f5.2,a)')&
			p_pivot,p_coefmax*p_objf,pros,p_lr0,p_lz0,p_nnf,&
			iminc,':',secd,imint,':',sect,' *OPTIMAL*'
		!	write(6,*)'p_objfv ',p_objfv
	else
		j_v(p_ivoptimal)=j_1
 
		pros=abs(100.*(p_objf-p_objfv)/p_objfv)
		write(6,'(i8,g19.12,1x,f5.2,3i6,4x,i3,a,f5.2,i5,a,f5.2,a)')&
			p_pivot,p_coefmax*p_objf,pros,p_lr0,p_lz0,p_nnf,&
			iminc,':',secd,imint,':',sect,' *NONFEASIBLE*'
 
 
	endif !if(p_feasible)   2952
 
	!	write(6,*)'sol',p_nureport,p_ivreport
	!call tulostele()
 
	if(p_nureport.ne.6)call j_printname('*report written into:',p_ivreport,' which remains open ')
 
	call repoz(p_nureport,.false.)
	if(p_echo.and.p_nureport.ne.6)call repoz(6,.false.)
 
	!if(allocated(p_xmi)) deallocate(p_xmi)   !xvar;
	!if(allocated(p_xma)) deallocate(p_xma)   !xvar;
 
 
	if(j_err) then
		p_buf='jlp error exit'
	else !if(j_err) then
		p_buf='jlp normal exit'
	endif !if(j_err)   2983
	!j_v(p_ivpivots)=p_pivot
	!	call  cpu_time(p_time0)
	call getout() !computes cpu
	write(p_nureport,*)'total cpu-time in jlpz() ',j_v(j_ivcpu)
	write(p_nureport,*)' '
	! write(6,*)'hep'
 
 
end subroutine jlpz


subroutine jlps(iob,io)
 
end subroutine !subroutine jlps(iob,io)

! ! subroutine initfletsp(nrow,ncol,a,la,lav,ls,lu1,ll1,ifail,nrefac)  !nopre!
! use fletchersparse
! use fletdmod
! implicit REAL*8 (a-h,o-z)
! integer lav(0:*)
! !nm=n+m
! ! flletcher m =ncol
! !           n=nrow /  ei nrow+1, Fletcher ei näe objetitriviä ?
! integer ls(1:*)
! real*8 a(la,*)
! common /wsc/kk,ll,kkk,lll,mxws,mxlws  !?? piti poistaaa bqpd:stä
! common /mxm1c/mxm1
! common/bqpdc/irh1,na,na1,nb,nb1,ka1,kb1,kc1,irg1,lu1fl,lv,lv1,ll1fl
! common/densec/ns,ns1,nt,nt1,nu,nu1,mx1,lc,lc1,li,li1
! common/noutc/nout
! common/factorc/m0,m1,mm0,mm,mp,mq
! common/epsc/eps,tol,emin
! common/repc/sgnf,nrep,npiv,nres
! common/refactorc/nup,nfreq

! eps=1111.D-19;tol=1.D-13; emin=j_0   ! corrected 4.5.2015 following Fletchers advice emin= 1.D0
! sgnf=1.D-4;nrep=2;npiv=3;nres=2;nfreq=nrefac;nup=0

! kk=0;ll=0
! kmax=0
! !C  n     number of variables  here number of rows
! !C  m     number of general constraints (columns of A)
! !c  k     dimension of the 'reduced space' obtained by eliminating the active
! !c        constraints (only to be set if mode>=2). The number of constraints in
! !c        the active set is n-k
! !C  kmax  maximum value of k (set kmax=0 iff the problem is an LP problem)
! nm=nrow+ncol ;nmi=nm
! call  stmap(nrow,nm,kmax)
! mode=0;ifail=0
! mxm1=min(ncol+1,nrow+ncol);mx1=mxm1

! !c  sparse.f requires
! !c     5*n+nprof          locations of real workspace, and
! !c     9*n+m              locations of integer workspace
! !c  where nprof is the space required for storing the row spikes of the L matrix.
! !c  Storage for sparseL.f is situated at the end of the workspace arrays ws
! !c  and lws in bqpd.
! !c  Allow as much space for nprof as you can afford: the routine will report if
! !c  there is not enough. So far 10^6 locations has proved adequate for problems
! !c  of up to 5000 variables.
! mxws=5*nrow+ 4*nrow*nrow  ! (nrow+1)              !5*nrow+4*nrow*nrow
! mxlws=9*nrow+ncol

! allocate( ws(1:mxws))
! allocate(lws(1:mxlws),e(1:nm),g(1:nrow),r(1:nm))
! lu1=lu1fl;ll1=ll1fl          !get stmap
! nk=0

! call start_up(nrow,nm,nmi,a,lav,nk,e,ls,wslu1,lwsll1,&
! mode,ifail)   !linux
! return
! end subroutine !subroutine initfletsp(nrow,ncol,a,la,lav,ls,lu1,ll1,ifail,nrefac)

! subroutine fbsubsp(nrow,ione,nrow2,a,lavec,iq,rhsw,x,ls,ws,&     !nopre!
! lws,save)

! use fletchersparse
! real*8 rhsw(*),x(*),a(*),ws(*)
! integer lws(*),ls(*),lavec(0:*)
! logical save
! call fbsub(nrow,ione,nrow2,a,lavec,iq,rhsw,x,ls,ws,&
! lws,.false.)

! return
! end subroutine !subroutine fbsubsp(nrow,ione,nrow2,a,lavec,iq,rhsw,x,ls,ws,&

! subroutine tfbsubsp(nrow,a,lavec,irow,objr,vc,ws,lws,apu,save)  !nopre!
! use fletchersparse
! real*8 a(*),ws(*),apu,objr(*),vc(*)
! integer lws(*),lavec(0:*)
! logical save

! call tfbsub(nrow,a,lavec,irow,objr,vc,ws,lws,apu,.false.)

! return
! end subroutine !subroutine tfbsubsp(nrow,a,lavec,irow,objr,vc,ws,lws,apu,save)

! subroutine pivotsp(nexnrowz,newc,nrow, &       !nopre!
! nm,a,lavec,e,ws,lws,ifail,info)
! use fletchersparse
! integer lws(*),lavec(0:*),info(*)
! real*8 a(*),ws(*),apu,e(*)

! call pivot(nexnrowz,newc,nrow, &
! nm,a,lavec,e,ws,lws,ifail,info)
! return
! end subroutine !subroutine pivotsp(nexnrowz,newc,nrow, &
subroutine jlpcoef(iob,io)
	use jmod, only: j_startfunction
	use jmod, only: j_ipproblem
	use jmod, only: j_arg
	use jmod, only: j_err
	!Section jlpcoef  jlpcoef() PROB into numeric form
	! This function is used at the beginning of jlp() and jlpz().
	!endheader
	!Note see jlpz() for how jlpcoef() can be used to demonstate problem defintion using matrices only.
	!endnote
	!endsection
 
	call j_startfunction(iob,io,j_ipproblem,narg,j_arg,ivout)
	if(j_err)return
	call jlpcoef_(j_arg(1),ivout)  ! jlpcoef(iob,io)
	return
 
end subroutine



subroutine jlpcoef_(ivproblem,ivout)  ! jlpcoef(
	use jmod, only: j_otype
	use jmod, only: j_ipproblem
	use jmod, only: j_getname
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: j_err
	use jmod, only: j_ivresult
	use jmod, only: j_named
	use jmod, only: j_o
	use jmod, only: p_nrowtot
	use jmod, only: p_npiece
	use jmod, only: p_ispiece
	use jmod, only: p_piecenotinited
	use jmod, only: p_fpresent
	use jmod, only: j_ipneigtable
	use jmod, only: j_iplist
	use jmod, only: j_ipmatrix
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	use jmod, only: p_rowofterm
	use jmod, only: j_ipreal
	use jmod, only: j_ipchar
	use jmod, only: j_defmatrix
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: p_coef
	use jmod, only: j_deflist
	use jmod, only: p_nterminrow
	use jmod, only: p_nterm
	use jmod, only: p_ivvars
	use jmod, only: p_vars
	use jmod, only: p_coefvars
	use jmod, only: p_varofterm
	use jmod, only: p_colofterm
	use jmod, only: j_num2iv
	use jmod, only: p_nvar
	use jmod, only: j_inlist
	use jmod, only: j_iv2val
	use jmod, only: p_ispiecevar
	use jmod, only: p_istermpiece
	use jmod, only: p_ivpiecevar
	use jmod, only: p_ivpiececoef
	use jmod, only: p_piececoef
	use jmod, only: p_ivpiecekeep
	use jmod, only: p_piecekeep
	use jmod, only: j_ivout
	use jmod, only: p_ivpiecexps
	use jmod, only: p_ivpieceprice
	use jmod, only: p_pieceprice
	use jmod, only: p_ivpieceint
	use jmod, only: p_pieceint
	use jmod, only: p_piecexps
	use jmod, only: p_ivpiecesum
	use jmod, only: p_piecesum
	use jmod, only: j_inlistobject
	use jmod, only: j_divkeep
	use jmod, only: p_coefplus
	use jmod, only: p_nrow
	use jmod, only: p_rhs
	use jmod, only: p_rhs2
	use jmod, only: j_getobject
	use jmod, only: j_v
	use jmod, only: j_0
	use jmod, only: p_maxo
	use jmod, only: j_1
	use jmod, only: p_coefmax
	use jmod, only: j_n1
	!generates coeffficients and rhs's taking into account lists and matricec
	! p_coefvars
	! p_varofterm
	!	p_nterminrow
	! p_rowofterm
	! p_coefplus
	! p_vars
 
	integer,intent(in)::ivproblem,ivout
	!	integer,intent(out)::ivobjects
 
	integer ,dimension(:), pointer::nterminrow !=>null()
	integer ,dimension(:), pointer::vars !=>null()
	integer ,dimension(:), pointer::coefvars  !=>null(
	integer ,dimension(:), pointer::coefplus  !=>null(
	integer,dimension(:),allocatable,target::coefplus2
	double precision, dimension(:),pointer ::coef
	integer ,dimension(:), pointer::varofterm !=>null()
	integer  ,dimension(:), pointer::rowofterm  !=>null()
	integer  ,dimension(:), pointer::colofterm  !=>null()
	integer  ,dimension(:), pointer::termbas  !=>null()
	!	write(6,*)'*jlpcoef_'  !,ivproblem,ivout,ivobjects
	! global pointer refer to the original
 
	if(j_otype(ivproblem).ne.j_ipproblem)then
		call j_getname(ivproblem)
		write(6,*)'*jlpcoef: ',j_oname(1:j_loname),' is not PROB'
		j_err=.true.;return
	endif !if(j_otype(ivproblem).ne.j_ipproblem)   3137
	if(ivout.eq.j_ivresult.or.ivout.le.20.or.ivout.gt.j_named)then
		write(6,*)'*jlpcoef: illegal ivout ',ivout
		j_err=.true.;return
	endif !if(ivout.eq.j_ivresult.or.ivout.le.20.or.ivout.gt.j_named)   3142
	!varibales and pointers without p_ com from the problem
 
	ivcoefvars= j_o(ivproblem)%i(17)
	nterm=j_o(ivcoefvars)%i(1)  !local initial
	coefvars=>j_o(ivcoefvars)%i2(1:nterm)
 
	iv=j_o(ivproblem)%i(8)
	varofterm=>j_o(iv)%i2(1:nterm)
 
	iv=j_o(ivproblem)%i(7)
	nterminrow(0:)=>j_o(iv)%i2(1:p_nrowtot)
 
	iv=j_o(ivproblem)%i(19)
	termbas(0:)=>j_o(iv)%i2(1:p_nrowtot)
 
	ivrowofterm=j_o(ivproblem)%i(6)  !rowofterms
	rowofterm=>j_o(ivrowofterm)%i2(1:nterm)
 
	iv=j_o(ivproblem)%i(20)  !rowofterms
	colofterm=>j_o(iv)%i2(1:nterm)
 
 
	iv= j_o(ivproblem)%i(18)
	coefplus=>j_o(iv)%i2(1:nterm)
 
	ivvars=j_o(ivproblem)%i(3)
	nvar=j_o(ivvars)%i(1)
	vars=>j_o(ivvars)%i2(1:nvar)
 
	! do i=1,nvar
	! call j_getname(vars(i))
	! write(6,*)'i,var0 ',j_oname(1:j_loname)
	! enddo !i=1,nvar   3168
	!write(6,*)p*ivout,p_nrow,
	!	p_ivmatrix=j_defmatrix(p_ivout,'%matrix',p_nrow,int8(p_nrow),nvar,j_matreg) !vector of coeffcients
	!	maxpiece=0
 
	nad=0 ! how many additonal
	p_npiece=0
	p_ispiece=.false.
	p_piecenotinited=.true.
	npmax=0
	p_fpresent=.false.
 
 
	do i=1,nterm
 
		ivterm=varofterm(i)
		ivcoe=coefvars(i)
		!	write(6,*)'ii',i,ivterm
		if(j_otype(ivterm).eq.j_ipneigtable)then
 
			p_fpresent=.true.
 
			cycle
			!write(6,*)'iterm',i,ivterm
		elseif(j_otype(ivterm).eq.j_iplist)then
			if(j_otype(ivcoe).eq.j_ipmatrix)then
				if(j_o(ivcoe)%i(2).ne.j_o(ivterm)%i(1).or.j_o(ivcoe)%i(1).ne.1)then
					call j_getname(ivcoe,ivterm)
					write(6,*)'coefficient ',j_oname(1:j_loname),' of ',j_oname2(1:j_loname2), ' on row ',&
						p_rowofterm(i),'is not row vector of ',j_o(ivterm)%i(1),' elements'
					j_err=.true. ;return
				endif !if(j_o(ivcoe)%i(2).ne.j_o(ivterm)%i(1).or.j_o(ivcoe)%i(1).   3204
 
			elseif(j_otype(ivcoe).ne.j_ipreal.and.j_otype(ivcoe).ne.j_ipchar)then
				call j_getname(ivcoe)
				write(6,*)'coefficient ',j_oname(1:j_loname), ' is not REAL or MATRIX'
				j_err=.true. ;return
			endif !if(j_otype(ivcoe).eq.j_ipmatrix)   3203
			nad=nad+j_o(ivterm)%i(1)-1
		elseif(j_otype(ivterm).eq.j_ipreal.and.j_otype(ivcoe).eq.j_ipmatrix)then
			np=j_o(ivcoe)%i(3)/2
			if(np.lt.2.or.2*np.ne.j_o(ivcoe)%i(3))then
				call j_getname(ivcoe,ivterm)
				write(6,*)'coefficient ',j_oname(1:j_loname),' of ',j_oname2(1:j_loname2), &
					' must have even number and at least 4 elements'
				j_err=.true.;return
 
			endif !if(np.lt.2.or.2*np.ne.j_o(ivcoe)%i(3))   3219
			j_o(ivcoe)%i(1)=np
			j_o(ivcoe)%i(2)=2
			npmax=max(npmax,np)
			p_ispiece=.true.
			p_npiece=p_npiece+1
 
			!	maxpiece=max(maxpiece,j_o(ivcoe)%i(1))
		endif !if(j_otype(ivterm).eq.j_ipneigtable)   3196
	enddo !i=1,nterm   3191
	!write(6,*)'npmax ',npmax
	nterm2=nterm+nad
	nvar2=nvar+nad !may have some extra
 
	! write(6,*)'nterm2,nvar2',nterm2,nvar2,nterm,nvar,nad
	! write(6,*)'rowofter',rowofterm
	! write(6,*)'varofter',varofterm
 
	iv=j_defmatrix(ivout,'%coef',j_18,int8(nterm2),j_matreg)
	p_coef=>j_o(iv)%d(1:nterm2)
 
	iv=j_deflist(ivout,'%nterminrow',list0=p_nrowtot, ilist=.true.)
	p_nterminrow(0:)=>j_o(iv)%i2(1:p_nrowtot)
	p_nterminrow=nterminrow !is final if nad.eq.0
	! p_pointer refer to original
	p_nterm=nterm2
 
 
	p_ivvars=j_deflist(ivout,'%vars',list0=nvar2) !p-variables
	p_vars=>j_o(p_ivvars)%i2(1:nvar2)
 
	iv=j_deflist(ivout,'%coefvars',list0=nterm2)
	p_coefvars=>j_o(iv)%i2(1:nterm2)
 
	iv=j_deflist(ivout,'%varofterm',list0=nterm2)
	p_varofterm=>j_o(iv)%i2(1:nterm2)
 
	iv=j_deflist(ivout,'%rowofterm',list0=nterm2,ilist=.true.) !row of each term
	p_rowofterm=>j_o(iv)%i2(1:nterm2)
 
	iv=j_deflist(ivout,'%colofterm',list0=nterm2,ilist=.true.) !row of each term
	p_colofterm=>j_o(iv)%i2(1:nterm2)
 
	!	write(6,*)'nad',nad,nterm2
	if(nad.gt.0)then
 
		!	iv=j_deflist(ivout,'%coefplus',list0=nterm2,ilist=.true.) !row of each term
		if(allocated(coefplus2))deallocate(coefplus2)
		allocate(coefplus2(1:nterm2))
		!coefpulus not needed as object because it is just interpreted ffor makeing coef
		nterm2=0
		nvar2=0
 
		! write(6,*)'rowofterm',rowofterm
		! write(6,*)'pvarofterm',varofterm
		! write(6,*)'coefplus',coefplus
 
		do i=1,nterm
 
			iro=rowofterm(i)
 
			ivterm=varofterm(i)
 
 
			!	write(6,*)'ihere,iro',i,iro,' ivterm ',ivterm
			if(j_otype(ivterm).eq.j_iplist)then
				ivcoe=coefvars(i)
				!	call j_getname(ivcoe)
				!write(6,*)'coe ',j_oname(1:j_loname),j_otype(ivcoe),j_ipmatrix
				isiz=j_o(ivterm)%i(1)
				call j_getname(ivcoe)
				!	write(6,*)'coef ',ivcoe,j_oname(1:j_loname),isiz,nterm2
				p_varofterm(nterm2+1:nterm2+isiz)=j_o(ivterm)%i2(1:isiz)
				p_colofterm(nterm2+1:nterm2+isiz)=-j_o(ivterm)%i2(1:isiz)
				!negativer values turned to columns when P-ivvars is ready
				!j_inlistobject(j_o(ivvarofterm)%i2(i),ivvars)
				! do jj=1,isiz
				! write(6,*)'var',p_varofterm(nterm2+jj)
				! write(6,*)'vars',j_o(p_ivvars)%i2
				! p_colofterm(nterm2+jj)=j_inlistobject(p_varofterm(nterm2+jj),p_ivvars)
				! enddo !jj=1,isiz   3292
 
				if(j_otype(ivcoe).eq.j_ipmatrix)then
					!write(6,*)'matrixcoef', j_o(ivcoe)%d(1:isiz)
					! if(coefplus(i).ne.0)then
 
					! p_coef(nterm2+1:nterm2+isiz)=j_o(ivcoe)%d(1:isiz)
					! else
					! p_coef(nterm2+1:nterm2+isiz)=-j_o(ivcoe)%d(1:isiz)
					! endif !if(p_coefplus(i).ne.0)   3248
					do j=1,isiz
						p_coefvars(nterm2+j)=j_num2iv(j_o(ivcoe)%d(j))
					enddo !j=1,isiz   3315
 
				endif !if(j_otype(ivcoe).eq.j_ipmatrix)   3307
				! if(coefplus(i).ne.0)then
 
				! p_coef(nterm2+1:nterm2+isiz)=j_iv2val(ivcoe)
				! else
				! p_coef(nterm2+1:nterm2+isiz)=-j_iv2val(ivcoe)
				! endif !if(p_coefplus(i).ne.0)   3259
				! endif !if(j_otype(ivcoe).eq.j_ipmatrix)   3246
				p_rowofterm(nterm2+1:nterm2+isiz)=iro
				coefplus2(nterm2+1:nterm2+isiz)=coefplus(i)
				nterm2=nterm2+isiz
				p_nterminrow(iro)=p_nterminrow(iro)+isiz-1
 
	loop:		do j=1,isiz
					do k=1,nvar2
						if(j_otype(p_vars(k)).eq.j_iplist) cycle loop
						if(p_vars(k).eq.j_o(ivterm)%i2(j))cycle loop
					enddo !k=1,nvar2   3333
					nvar2=nvar2+1
					p_vars(nvar2)=j_o(ivterm)%i2(j)
				enddo loop !p:		do j=1,isiz   3332
 
			else
				!not list
				do k=1,nvar2
 
					if(p_vars(k).eq.ivterm)goto 700
				enddo !k=1,nvar2   3343
				nvar2=nvar2+1
				p_vars(nvar2)=ivterm
 
	700			nterm2=nterm2+1
 
				p_rowofterm(nterm2)=iro
 
				!			p_coef(nterm2)=j_iv2val(p_coefvars(i)) !p_coef(i)
				!	call j_getname(p_coefvars(i))
				!	write(6,*)'coeny ',j_oname(1:j_loname),' ',nterm2,p_coefvars(i),j_v(p_coefvars(i)),coef(nterm2)
 
				!	if(coefplus(i).eq.0)p_coef(nterm2)=-p_coef(nterm2)
	666			p_varofterm(nterm2)=varofterm(i)
				coefplus2(nterm2)=coefplus(i)
				p_coefvars(nterm2)=coefvars(i)
				p_colofterm(nterm2)=colofterm(i)
			endif !if(j_otype(ivterm).eq.j_iplist)   3290
 
			!write(6,*)'nterm ',nterm
		enddo !i=1,nterm   3282
 
		!write(6,*)'nterm,nvar ',nterm,nvar
		!write(6,*)'p_befvars ',p_vars
		!j_o(ivvars)%i2(1:nvar2)=vars(1:nvar2)
 
		j_o(ivvars)%i(1)=nvar2
 
 
		!	do i=1,nvar2
		! call j_getname(p_vars(i))
		! write(6,*)'i,varb ',j_oname(1:j_loname)
		! enddo !i=1,nvar2   3364
		p_nvar=nvar2
		p_nterm=nterm2
		coefplus=>coefplus2
		do j=1,p_nterm
			if(p_colofterm(j).lt.0)p_colofterm(j)=j_inlist(-p_colofterm(j),p_nterm,p_vars)
		enddo !j=1,p_nterm   3382
		!if(nad.gt.0)then
	else
 
		! p_coefplus=coefplus
		p_vars=vars
		p_coefvars=coefvars
		p_varofterm=varofterm
		!	p_nterminrow=nterminrow
		p_rowofterm=rowofterm
		!	write(6,*)'pcol',p_colofterm
		!	write(6,*)'col',colofterm
		!	write(6,*)'nterm,nterm2',nterm,nterm2
		p_colofterm=colofterm
		p_nterm=nterm
		p_nvar=nvar
 
	endif !if(nad.gt.0)   3269
	!	write(6,*)'varofter',p_varofterm
	!	write(6,*)'rowofter',p_rowofterm
 
	do i=1,p_nterm
 
		p_coef(i)=j_iv2val(p_coefvars(i))
		if(coefplus(i).eq.0)p_coef(i)=-p_coef(i)
	enddo !i=1,p_nterm   3405
	if(nad.gt.0)deallocate(coefplus2)
	if(p_ispiece)then  !allocate vectors
		!	write(6,*)'p_npiece%% ',p_npiece
		if(allocated(p_ispiecevar))deallocate(p_ispiecevar);allocate(p_ispiecevar(1:nvar))
		p_ispiecevar=.false.
		if(allocated(p_istermpiece))deallocate(p_istermpiece);allocate(p_istermpiece(1:p_nterm))
		p_istermpiece=.false.
		!p_ivtermpiece=j_deflist(ivout,'%termpiece',list0=nterm, ilist=.true.)
		!p_ivpiece=j_deflist(ivout,'%piece',list0=p_npiece, ilist=.true.)
		!	p_ivnpiece=j_deflist(ivout,'%npiece',list0=p_npiece,ilist=.true.)
		p_ivpiecevar=j_deflist(ivout,'%piecevar',list0=p_npiece)
		p_ivpiececoef=j_deflist(ivout,'%piececoef',list0=p_npiece,point=p_piececoef)
		p_ivpiecekeep=j_deflist(ivout,'%piecekeep',list0=p_npiece, ilist=.true.)
		p_piecekeep=>j_o(p_ivpiecekeep)%i2(1:p_npiece)
		!	p_ivpiecematrix=j_defmatrix(j_ivout,'%piecematrix',maxpiece,p_npiece,j_matreg)
		!	p_ivpieceint=j_defmatrix(j_ivout,'%pieceint',p_npiece,1,j_matreg)
		!	p_ivpieceprice=j_defmatrix(j_ivout,'%pieceprice',p_npiece,1,j_matreg)
		p_ivpiecexps=j_defmatrix(j_ivout,'%piecexps',int8(p_npiece),j_18,j_matreg)
		p_ivpieceprice=j_defmatrix(j_ivout,'%pieceprice',int8(p_npiece),j_18,j_matreg)
		p_pieceprice=>j_o(p_ivpieceprice)%d(1:p_npiece)
		p_ivpieceint=j_defmatrix(j_ivout,'%pieceint',int8(p_npiece),int8(npmax),j_matreg)
		p_pieceint=>j_o(p_ivpieceint)%d(1:p_npiece*npmax)
 
 
		p_piecexps=>j_o(p_ivpiecexps)%d(1:p_npiece)
		p_ivpiecesum=j_defmatrix(j_ivout,'%piecesum',int8(p_npiece),j_18,j_matreg)
		p_piecesum=>j_o(p_ivpiecesum)%d(1:p_npiece)
 
		ii=0
		ibasint=0
		do i=1,p_nterm
 
			ivterm=p_varofterm(i)
			ivcoe=coefvars(i)
			if(j_otype(ivterm).eq.j_ipreal.and.j_otype(ivcoe).eq.j_ipmatrix)then
				ii=ii+1
				p_istermpiece(i)=.true.
				jix=j_inlistobject(ivterm,ivvars)
				call j_getname(ivterm,ivcoe)
				write(6,*)'ispiece ',i,j_oname(1:j_loname),' jix ',jix ,j_oname2(1:j_loname2)
				p_ispiecevar(jix)=.true.
				!	j_o(p_ivnpiece)%i2(ii)=j_o(ivcoe)%i(1)
				j_o(p_ivpiecevar)%i2(ii)=ivterm
				j_o(p_ivpiececoef)%i2(ii)=ivcoe
				npoints=j_o(ivcoe)%i(1)
				iib=0
 
				p_pieceint(ibasint+1)=j_o(ivcoe)%d(2)*j_o(ivcoe)%d(1)
				do ip=2,npoints
					p_pieceint(ibasint+ip)=p_pieceint(ibasint+ip-1)+j_o(ivcoe)%d(iib+4)*&
						(j_o(ivcoe)%d(iib+3)-j_o(ivcoe)%d(iib+1))
					iib=iib+2
 
				enddo !ip=2,npoints   3458
				ibasint=ibasint+npmax
 
				!			j_o(p_ivpieceprice)%d(ii)=j_o(ivcoe)%d(1)
				j_o(p_ivpiecekeep)%i2(ii)=j_inlistobject(ivterm,j_divkeep)
				if(j_o(p_ivpiecekeep)%i2(ii).le.0)then
					call j_getname(ivterm)
					write(6,*)'variable ',j_oname(1:j_loname),' is not in data'
					j_err=.true.;return
				endif !if(j_o(p_ivpiecekeep)%i2(ii).le.0)   3468
				if(p_coefplus(i).eq.0)then
					write(6,*)'ask J Lappi to allow negative piecewise term'
					j_err=.true.;return
 
				endif !if(p_coefplus(i).eq.0)   3473
 
			endif !if(j_otype(ivterm).eq.j_ipreal.and.j_otype(ivcoe).eq.j_ipm   3444
		enddo !i=1,p_nterm   3440
 
 
	endif !if(p_ispiece)   3411
 
 
	!	write(6,*)'hellurei'
	! RHS
	ivrhsvars=j_o(ivproblem)%i(1)
	ivrhs2vars=j_o(ivproblem)%i(2)
	if(p_nrow.gt.0)then
		ivrhs=j_defmatrix(ivout,'%rhs',j_18,int8(p_nrow),j_matreg)
		p_rhs=>j_o(ivrhs)%d
		!write(6,*)'NROW ',nrow
 
		ivrhs2= j_defmatrix(ivout,'%rhs2',j_18,int8(p_nrow),j_matreg)
		p_rhs2=>j_o(ivrhs2)%d
		do i=1,p_nrow
			j_o(ivrhs)%d(i)=j_iv2val(j_o(ivrhsvars)%i2(i))
			j_o(ivrhs2)%d(i)=j_iv2val(j_o(ivrhs2vars)%i2(i))
		enddo !i=1,p_nrow   3497
 
	else
		ivrhs=0
		ivrhs2=0
	endif !if(p_nrow.gt.0)   3490
 
	ivobj=j_defmatrix(ivout,'%objrow',j_18,int8(nvar),j_matreg)
	!write(6,*)'nvar',nvar,j_o(ivnterminrow)%i2(1)
	!write(6,*)'i2',j_o(ivnterminrow)%i2
	!write(6,*)'i',j_o(ivnterminrow)%i
	!write(6,*)'colofterm',colofterm
	!write(6,*)'pcolofterm',p_colofterm
	do i=1,p_nterminrow(0)  !j_o(ivnterminrow)%i(1)
		!ico=j_inlistobject(j_o(ivvarofterm)%i2(i),ivvars)
		j_o(ivobj)%d(p_colofterm(i))=p_coef(i)
	enddo !i=1,p_nterminrow(0)   3513
	!	call j_getname(ivrowofterm,ivnterminrow,ivvarofterm)
	!	write(6,*)'onbam ',j_oname(1:j_loname),j_o(ivrowofterm)%i,' i2  ',j_o(ivrowofterm)%i2
	!	write(6,*)j_oname2(1:j_loname2),j_o(ivnterminrow)%i,' i2  ',j_o(ivnterminrow)%i2
	!	write(6,*)j_oname3(1:j_loname3),j_o(ivvarofterm)%i,' i2  ',j_o(ivvarofterm)%i2
	ivmatrix=0
 
	if(p_nrow.gt.0)ivmatrix=j_defmatrix(ivout,'%matrix',int8(p_nrow),int8(p_nvar),j_matreg)
 
	!write(6,*)'nterminrow',p_nterminrow
	!write(6,*)'rowofterm',p_rowofterm
	! write(6,*)'nvar,nvar2,p_nvar ',nvar,nvar2,p_nvar
	! write(6,*)'colofterm',p_colofterm
	! write(6,*)
 
	do i=p_nterminrow(0)+1,p_nterm   !j_o(ivnterminrow)%i(1)+1,nterm
		iro=p_rowofterm(i)
		ico=p_colofterm(i)  !j_inlistobject(j_o(ivvarofterm)%i2(i),ivvars)
		!	write(6,*)'iro,ico,j_o(ivvarofterm)%i2(i),',iro,ico !,p_coef(i)
		j_o(ivmatrix)%d((iro-1)*p_nvar+ico)=p_coef(i)
	enddo !i=p_nterminrow(0)+1,p_nterm   3531
 
 
 
	iv=j_getobject(ivout,'%objtype',j_ipreal)
	j_v(iv)=j_o(ivproblem)%i(5)
	if( j_v(iv).gt.j_0)then
		p_maxo=.true.
		p_coefmax=j_1
	else
		p_maxo=.false.
		p_coefmax=j_n1
	endif !if( j_v(iv).gt.j_0)   3542
	!	j_o(ivobjects)%i2(13)=iv
 
	!	write(6,*)'coef',j_o(ivcoef)%d
	!	write(6,*)'coefssh',j_o(ivobjects)%i2
 
end subroutine

subroutine neigtable(iob,io)
	use jmod, only: j_startfunction
	use jmod, only: j_arg
	use jmod, only: j_err
	use jmod, only: j_getoption
	use jmod, only: j_mfrom
	use jmod, only: j_ipdata
	use jmod, only: j_optarg0
	use jmod, only: j_mto
	use jmod, only: j_mlogs
	use jmod, only: j_iplist
	use jmod, only: j_o
	use jmod, only: j_getname
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: j_mtrans
	use jmod, only: j_iptrans
	use jmod, only: j_mknn
	use jmod, only: j_ipreal
	use jmod, only: j_v
	use jmod, only: j_nrows
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	use jmod, only: j_defmatrix
	use jmod, only: j_matreg
	use jmod, only: j_deflist
	use jmod, only: j_18
	use jmod, only: j_getobs0
	use jmod, only: j_dapu
	use jmod, only: j_0
	use jmod, only: j_otype
	use jmod, only: j_ipneigtable
	integer*8 ::nobs1,nobs2,knn,i8,ifact
	double precision,dimension(:),pointer::neigu,futil
	real,dimension(:,:),allocatable::coefmat
	integer,dimension(:),allocatable::itempvector
	call j_startfunction(iob,io,0,narg,j_arg,ivout,needsout=.true.,delout=.true.)
	if(j_err)return
	call j_getoption(iob,io,j_mfrom,1,1,j_ipdata,.true.,noptarg,j_optarg0)
	if(j_err)return
	iv1=j_optarg0(1)
	call j_getoption(iob,io,j_mto,1,1,j_ipdata,.true.,noptarg,j_optarg0)
	if(j_err)return
	iv2=j_optarg0(1)
	call j_getoption(iob,io,j_mlogs,1,1,j_iplist,.true.,noptarg,j_optarg0)
	if(j_err)return
	ivlogs=j_optarg0(1)
	nlog=j_o(ivlogs)%i(3)
	!if(present(ivcases))j_o(iv)%i(10)=ivcases
	ivcases=j_o(iv2)%i(10)
	if(ivcases.le.0)then
		call j_getname(iv2)
		write(6,*)j_oname(1:j_loname),' is not DATA with case'
		j_err=.true.;return
	endif !if(ivcases.le.0)   3575
 
	call j_getoption(iob,io,j_mtrans,1,1,j_iptrans,.true.,noptarg,j_optarg0)
	if(j_err)return
	itrans=j_optarg0(1)
 
	call j_getoption(iob,io,j_mknn,1,1,j_ipreal,.true.,narg,j_optarg0)
	if(j_err)return
	knn=j_v(j_optarg0(1))
 
 
	ivmat1=j_o(iv1)%i(1)
	ivkeep1=j_o(iv1)%i(2)
	nkeep1=j_o(ivkeep1)%i(1)
	nobs1=j_nrows(ivmat1)
 
	ivmat2=j_o(iv2)%i(1)
	ivkeep2=j_o(iv2)%i(2)
	nkeep2=j_o(ivkeep2)%i(1)
	nfact=j_nrows(ivmat2)
	if(knn.gt.nfact)then
		write(6,*)'there are only ',nfact , ' factories knn changed to ',nfact
		knn=nfact
 
 
	endif !if(knn.gt.nfact)   3599
	kntot=knn*nlog
	kntot2=kntot*nobs1
	call j_getname(iv2,ivmat2)
	write(6,*)j_oname(1:j_loname+1),j_oname2(1:j_loname2+1),iv2,ivmat2,'nfact',nfact
 
	if(allocated(coefmat))deallocate(coefmat,itempvector)
	allocate(coefmat(nfact,nlog),itempvector(1:nfact))
	ivneigu=j_defmatrix(ivout,'%neigu',nobs1,int8(kntot),j_matreg,point=neigu) !ivob)
	ivneig=j_deflist(ivout,'%neig',list0=kntot2,ilist=.true.,ncol=kntot)
 
	ivutil=j_defmatrix(0,'$util',int8(nlog),j_18,j_matreg)
 
	ibasn=0
	do i8=1,nobs1
		!ibadat0=ibadat
		!	write(6,*)'ibadat0',ibadat0
		!ibasnold=ibasn
		call j_getobs0(iv1,i8)
 
		do ifact=1,nfact   !logs
			call j_getobs0(iv2,ifact)
			call dotrans(itrans,1)
			if(j_err)then
				write(6,*)'error occurred for unit ',i
				deallocate(coefmat,itempvector)
				return
			endif !if(j_err)   3627
			coefmat(ifact,1:nlog)=-j_o(ivutil)%d
		enddo !ifact=1,nfact   3624
		do ilog=1,nlog
			call ssortp(coefmat(1:nfact,ilog),1,nfact,itempvector)
			do k=1,knn
				j_dapu=coefmat(itempvector(k),ilog)
				if(j_dapu.ne.j_0)then
					j_o(ivneig)%i2(ibasn+k)=itempvector(k)
					j_o(ivneigu)%d(ibasn+k)=-j_dapu
				endif !if(j_dapu.ne.j_0)   3638
			enddo !k=1,knn   3636
			ibasn=ibasn+knn
		enddo !ilog=1,nlog   3634
		if(i8.le.5)write(6,'(25f6.1)')j_o(ivneigu)%d(ibasn-kntot+1:ibasn)
	enddo !i8=1,nobs1   3618
 
 
	allocate(j_o(ivout)%i(1:10))
	j_o(ivout)%i(1)=nlog
	j_o(ivout)%i(2)=nfact
	j_o(ivout)%i(3)=nlog*nfact
	j_o(ivout)%i(4)=ivlogs
	j_o(ivout)%i(5)=ivcases
	j_o(ivout)%i(6)=iv1
	j_o(ivout)%i(7)=iv2
	j_o(ivout)%i(8)=ivneig
	j_o(ivout)%i(9)=ivneigu
	j_o(ivout)%i(10)=knn
	j_otype(ivout)=j_ipneigtable
	deallocate(coefmat,itempvector)
 
end subroutine

subroutine jlp(iob,io)   ! %%jlp  !!!!****************************************************
	use jmod, only: p_iob
	use jmod, only: j_0
	use jmod, only: p_xps00
	use jmod, only: p_pivotrz
	use jmod, only: p_n16
	use jmod, only: j_v
	use jmod, only: j_ivdollar2
	use jmod, only: p_isn16
	use jmod, only: p_newc
	use jmod, only: j_startfunction
	use jmod, only: j_arg
	use jmod, only: j_ivout
	use jmod, only: j_err
	use jmod, only: p_npiece
	use jmod, only: p_nterm
	use jmod, only: p_nrow
	use jmod, only: p_fpresent
	use jmod, only: j_igetopt
	use jmod, only: j_mprint
	use jmod, only: p_iprint
	use jmod, only: p_p8
	use jmod, only: p_p
	use jmod, only: p_p9
	use jmod, only: p_zerob
	use jmod, only: p_feasible1
	use jmod, only: p_time00
	use jmod, only: j_o
	use jmod, only: j_divdataup
	use jmod, only: p_isunit
	use jmod, only: p_maxns
	use jmod, only: p_xpresent
	use jmod, only: p_nxvar
	use jmod, only: j_dnkeep
	use jmod, only: p_nxvartot
	use jmod, only: p_nsumx
	use jmod, only: p_nxvararea
	use jmod, only: p_nz
	use jmod, only: p_nvar
	use jmod, only: p_nvarf
	use jmod, only: p_isz
	use jmod, only: p_intapp
	use jmod, only: p_nrowz
	use jmod, only: p_lavec
	use jmod, only: p_ncol
	use jmod, only: p_ncoltot
	use jmod, only: p_nrow2z
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: j_isoption
	use jmod, only: j_mmatrix
	use jmod, only: j_defmatrix
	use jmod, only: j_dnobs
	use jmod, only: j_matreg
	use jmod, only: p_nrowtot
	use jmod, only: j_getname
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: p_isdomain
	use jmod, only: p_nunits
	use jmod, only: p_ix
	use jmod, only: p_ns
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: p_xmat
	use jmod, only: p_ntemp0
	use jmod, only: p_ntable
	use jmod, only: p_nfact
	use jmod, only: p_nlog
	use jmod, only: p_nlogfact
	use jmod, only: p_zmatrix
	use jmod, only: p_objf0
	use jmod, only: p_xps
	use jmod, only: p_ivobj0
	use jmod, only: p_zopt
	use jmod, only: p_objf
	use jmod, only: j_1
	use jmod, only: p_ivfeasible
	use jmod, only: p_feasible
	use jmod, only: p_nureport
	use jmod, only: p_small
	use jmod, only: p_unitv
	use jmod, only: p_kierv
	use jmod, only: p_xirowold2
	use jmod, only: p_next
	use jmod, only: p_prev
	use jmod, only: j_18
	use jmod, only: p_ivrhsw
	use jmod, only: p_rhsw
	use jmod, only: p_xpsrow
	use jmod, only: p_lbou
	use jmod, only: p_ubou
	use jmod, only: p_rhs
	use jmod, only: p_rhscur
	use jmod, only: p_rhs2
	use jmod, only: p_lower
	use jmod, only: p_xma
	use jmod, only: p_ivoptimal
	use jmod, only: p_ivobj
	use jmod, only: p_lx0
	use jmod, only: p_lz0
	use jmod, only: p_lr0
	use jmod, only: j_one
	use jmod, only: p_coefmax
	use jmod, only: p_ivstartedjlp
	use jmod, only: j_printname
	use jmod, only: p_echo
	use jmod, only: p_muutosb
	use jmod, only: p_objfv
	use jmod, only: p_objfprev
	use jmod, only: p_maxo
	use jmod, only: p_niter
	use jmod, only: p_exitkier
	use jmod, only: p_factnow
	use jmod, only: p_time0
	use jmod, only: p_pivotold
	use jmod, only: p_fastmakes
	use jmod, only: p_nrefac
	use jmod, only: p_tolep
	use jmod, only: p_warm
	use jmod, only: p_maxrounds
	use jmod, only: p_fastusesame
	use jmod, only: p_isfastp
	use jmod, only: p_fastpros
	use jmod, only: p_isstop
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	use jmod, only: p_nimp
	use jmod, only: p_asv
	use jmod, only: p_x
	use jmod, only: p_nnf
	use jmod, only: p_kierstep
	use jmod, only: p_kierout
	use jmod, only: p_enter
	use jmod, only: j_ninf
	use jmod, only: p_objfz
	use jmod, only: p_pivot9
	use jmod, only: p_nextlkm
	use jmod, only: p_kier
	use jmod, only: p_unit
	use jmod, only: p_ibadat
	use jmod, only: p_inebas
	use jmod, only: p_keyfactbas
	use jmod, only: p_pivot
	use jmod, only: p_nnfold2
	use jmod, only: p_fastmake
	use jmod, only: j_dapu
	use jmod, only: p_ls
	use jmod, only: p_objr
	use jmod, only: j_chr10
	use jmod, only: p_as
	use jmod, only: p_activeunit
	use jmod, only: p_ivreport
	use jmod, only: p_buf
	use jmod, only: j_ivcpu
 
	use fletdmod2,only: closeflet
	!	logical zpresent
 
	logical dorz,nopivots !exitkier
 
	integer ::unitchange,kierchange
	p_iob=iob
	nopivots=.false.
	!	logical cancel
	!Section jlp jlp() for schedules DATA
	! jlp() solves linear programming problems. The function now assumes that there is
	!schedules data. Without schedules jlpz() must be used.
	!endheader
	!Option
	! Output &1 &  & Output tells how objects created by jlp() are named. There is no JLP object type,
	! but the output indicates that e.g. the following objects are created. Many other objects are created
	! but they are currently used for debuggging purposes and they will be described later. They can be
	! used also to teach how the algorithm proceeds.
	!\begin{itemize}
	!\item Output%weights The weights of the schedules, see teh example below.
	!\item  Output%obj= value of the objective function
	!\item  	Output%rows= the vector the valuef of the constraint rows.
	!\item   Output%shprice = vector of shadow prices of the rows
	!\item  Output%xvar= LIST of xvariables in the schedules data
	!Output%xvarproblem= LIST of x-variables in the PROB
	!\item  Output%xsum= Vector of sums of variables in Output%xvarproblem
	!\item  Output%xprice Shadow prices of the variables in Output%xvarproblem
	!\item   Output%xsumint The sums of x-variables in the integer approximation, generated if integer-> is present
	!\end{itemize}
	!problem&1&PROB &Problem object produced with problem()
	!data&1&DATA& Schedules data (Sic!) linked to it with unitdata if unit-> is not given
	!unit&-1-99&REAL& gives the  unit variables from which at least one  changes when unit changes.
	!area&-1|1&REAL& The values in variables in xdata are per area variables, except
	! variables given in notareavars-> option
	!z&-1|0 & &This option must be present when there are z-variables in the problem, but the z-variables
	! need not to be listed. The reason for this option is that often the purpose
	! is to define the problem using only x-variables, but due to typing errors all variables
	! are not among x-variables.
	!showdomain&-1-99&CHAR&the sums of the x-variables are computed also for these domains.
	!print&-1:1&REAL& print-> set printing level to 2, print->value set the printling level to value, where zero indicates no
	!printing. Default level is 1.
 
	!debug&-1|0|1&REAl& debug-> sets debugging on at start debug->value sets debugging on when pivot=value,
	!After the debugging pivot, Jlp22 generates pause() and during the pause the user can do any computations. Before
	! the pause some additional matrices are generated in addition to matrices which are used
	! the computations. NOT WORKS THIS WAY NOW
	!stop&-1|1&CODE& codeoption telling when iterations over units stop. The variables
	! Round (current number of rounds over units), Change% (change of objective during the last 10
	! rounds), Imp (number of entering schedules outside the active set when updating the active set),
	! 	Active% ( %-size of the active set) and all global variables in JLP22.
	!default is stop->(Change%.lt.0.01.and.Round.ge.10).
	! fast%&-1|1&CODE & codeoption computing Fast%. All schedules whose price is larger than Fast%
	!of the current key schedule. Same variables can be used as for stop-> and also current Fast%.
	! A possible rule is fast%->(min(Fast%+5-(Imp.gt.0)*10,98))). The default is Fast%=85.
	&!fastrounds&-1|1&REAL%Number of rounds using the same active set. Deafault is 10.
		! maxrounds%&-1|1&REAL & maximum number of rounds over all units. Default maxrounds->3000
		!report&-1|1& CHAR & the results are written to the file spesified.
		!echo|-1|0& & &When results are printed to a file, echo-> implies that they are written alo to the terminal.
		!refac&-1|1|&REAL& refac->value tells that the factors of the basis matrix are recomputed after value pivot operations.
		!The default is refac->1000.
		!tole&-1|1&REAL& tole->value tells that the default tolerances are multiplied with the value.
		!matrix&-1-1& REAL&Tells that matrix output%xmattot is generated which the
		! x-data for all rows in order as the algorithm sees it, i.e., takes domains
		!into account. Works only when doamins are present as without domains
		!output%xmat shows the x-data.
		!endoption
		!Note The data-> must now always refer to schdedules data (for several reasons)
		!endnote
		!Note In small problems dCPU, i.e. increase of used CPU time is not very accurate.
		!endnote
		!Note jlp() stores the sums of x-variables into output% -variables. If there are domains
		! or showdomains, the variable names get postfix [domaindefinition], show examples
		! below
		!endnote
		!Ex jlpex jlp() solves linear programming problem
		! cdata=data(in->'cdat.txt',read->(cdata%nobsw,site))
		! stat()
		! xdata=data(in->'xdat.txt',read->(npv#0,npv#5,income1...income5),up->cdata)
		! stat()
 
 
		! proba=problem();
		!** In this problem the 4% net present value at the beginning is maximixe
		!** subject to the constraints telling that net incomes are nondecreasing
		! npv#0==max
		! ;do(i,2,5)
		! income"i"-income"i-1"=0
		! ;enddo
		! npv#5-npv#0>0
		! /
		! plist=;list(proba%?);
		! @plist;
 
		! jlpa=jlp(problem->proba,data->xdata,showdomain->('site.le.3','site.gt.3'))
		! ** sums of x-variables are stored into the same variables with jlpa% prefix
		!jlpa%income5;
		!jlpa%income5[site.le.3];
		!** Note income variables are in theory equal but as their
		!** values are computed numerically, they differ.
		! jlist=;list(jlpa%?);
		! **  these could be printed with @jlist;
 
		! ** jlpa%weights gets the weights of schdedules
		! ** combain the weights with the data
		! xdataw=newdata(xdata,jlpa%weights,read->w)
		! stat(sum->)
		! **sum of weights is equal to the number of stands
		! w%sum;
		! ** weighted statistics
		! ** thesw agree with the jlp solution
		! stat(weight->w,sum->)
		! ;do(i,1,len(xdataw%keep))
		! @xdataw%keep(i)%sum;
		!;enddo
		!***Problem with domains
		! probb=problem();
		! npv#0==max
		!**Domain definitions:
		!**there can be several domain definitions on a row
		!** one domain definition is:
		!**    a logical statement in terms of stand variables
		!**    a stand variable whose nonzero value implies that the domain applies
		!**    All  indicates all stands.
		!** before first domain definition row the default Domain is All
		! site.le.3: site.gt.3:
		! ;do(i,2,5)
		! income"i"-income"i-1"=0
		! ;enddo
		! npv#5-npv#0>0
		! /
		! plistb=;list(probb%?);
		! *** these  could be printed with @plistb;
		!jlpb=jlp(problem->probb,data->xdata,showdomain->'3.lt.site')
		! jlpb%income5;
		!jlpb%income5[site.gt.3];
		! jlpb%income5[3.lt.site];
 
		!** Now problem without constraints
		! probc=problem();
		! npv#0==max
		!/
		!jlpc=jlp(problem->probc,data->xdata,showdomain->'3.lt.site')
 
		!endex
		!endsection
 
		!Section factopt Factory opimization
		! In factory optimization problems logs are transported to factories. The utility of tansported logs is defined
		!  using terms which are TABLE objects. A TABLE consists of two LIST objects.
		! The first LIST in factory optimization gives log types transported to to factories which are stored in the second LIST.
		! The properties of the factories in one factory list are defined with datawcase() function, which produces a standard DATA
		! object with an additional link to the
		! LIST of cases, which must the same as or equal to the first argument of table() function.
		!	Associated with
		! with each TABLE term in a problem there is a MATRIX with name table%coef
		!(table is the name of the table) having as many rows as there are
		! elements in the log type
		! LIST in the TABLE term and as many columns as there are in the factory LIST of the TABLE.
		! An element of the matrix correponding to a log type and a factory tells what is the utility obtained when
		! one unit of the log type is tranported to the factory.
		! There is a transformation which describes how the utility coefficients of all TABLE terms can be computed from the
		! unit variables and the factory variables defined with the datawcase() function.
		! Constraints can contain variables for variables telling how much of a given log type is trasnported
		! to a given factory. Typically these constraints describe the capacities of factories.
		! These variables look like  logtype%2%factory.
		!There are two factory optimization options:
		!endheader
		!Option
		! knn&1&REAL & When considering to which factory a log type is transported, only knn
		! best factories according to values in table%coef are considered.
		!utiltrans&1&TRANS& Transformation telling how table%coef are computed for each unit.
		!endoption
		!Note If a given factory has no capacity for a given log type, no capacity constraint is needed.
		! The zero capacity is indicated by giving a negative utility in %coef matrix for a log type for which
		! there is no capacity.
		!endnote
		!Ex factopt Factory optimization (in preparation)
		! **	The following example shows how the factory optimization will work.
 
		! ** There are 10 stands
		! ** coordinates of stands in 100 x 100 area
		! loc=matrix(10,2);
		! loc=ran();
		! loc=100*loc;
		! ** numbers of schedules in each stand
		! ns=matrix(10,in->)
		! 3
		! 4
		! 3
		! 4
		! 3
		! 2
		! 4
		! 3
		! 3
		! 3
		! /
 
		! ** x and y are coordinates of of stands
		! cdata=newdata(ns,loc,read->(cdata%nobsw,x,y))
		! stat(sum->)
 
		! ** NS is total number of schedules generated randomly fro 5 periods
		! NS=cdata%nobsw%sum;
		! xmat0=matrix(NS)
		! xmat0=ran()
		! xmat0=10*xmat0
 
		! tr=trans()
		! ;do(i,2,5)
		! x"i"=x"i-1"+ran()
		! ;enddo
		! ;do(i,1,5)
		! sawlog"i"=0.2*x"i"
		! pulplog"i"=0.3*sawlog"i"
		! ;enddo
		! /
 
		! xdata=newdata(xmat0,read->x1,maketrans->tr)
		! stat()
		! linkdata(cdata,xdata)
 
		! ** sawmill properties, X and Y are coordinates of sawmills
		! ** X and Y are now matrices, arguments of read-> must be REAL, so let us make them REAL
		!X,Y=0
		! sawdata=data(read->(case,X,Y,capacity1...capacity5,util1...util5),case->,in->)
		! S1,20,30,5,6,7,8,6,10,10,11,10,10,12
		! S2,70,60,6,6,7,8,6,10,12,13,13,11,15
		! S3,10,70,5,6,7,8,6,10,13,14,14,11,13
		! /
 
		! **pulpmill properties
		! pulpdata=data(read->(case,X,Y,capacity1...capacity5,util1...util5),case->,in->)
		! P1,40,30,2,3,2,4,3,4,3,3,4,4,4
		! P2,80,70,2,3,2,4,3,6,4,4,5,5,5
		! P3,20,90,2,3,2,4,3,6,5,5,6,6,4
		! /
 
		! ** sawlogs for different periods
		! sawlogs=list(sawlog1...sawlog5);
		! pulplogs=list(pulplog1...pulplog5);
 
		! ** utility obtained from sawmills
		! sutil=table(sawdata%case,sawlogs);
 
		! ** matrix to which utilities are computed
		! sutil%coef=matrix(len(sawdata%case),len(sawlogs))
 
		! ** utility from pulpmills
		! putil=table(pulpdata%case,pulplogs);
 
		! ** matrix to which utilities are computed
		! putil%coef=matrix(len(pulpdata%case),len(pulplogs))
 
		! scost=0.1  ! cost of transporting sawlogs, these could be period dependent
		! pcost=0.2  ! cost of transporting pulp logs
 
		! util=trans()
		! ;do(i,1,len(sawdata%case))
		! ;do(j,1,len(sawlogs))
 
		! *sawdata(util"j",row->"i") is value of variable util"j" for observation i in saw mill data
		! sutil%coef("i","j")=sawdata(util"j",row->"i")-
		! scost*sqrt((sawdata(X,row->"i")-x)***2+(sawdata(Y,row->"i")-y)***2)
 
		! ;enddo
		! ;enddo
 
		! ;do(i,1,len(pulpdata%case))
		! ;do(j,1,len(pulplogs))
 
		! putil%coef("i","j")=pulpdata(util"j",row->"i")-
		! pcost*sqrt((pulpdata(X,row->"i")-x)***2+(pulpdata(Y,row->"i")-y)***2)
 
		! ;enddo
		! ;enddo
		! /
 
		! fprob=problem()
		! ** utility from sawmills + utility from pulpmills
		! sutil+putil==max
 
		! ;do(j,1,len(sawlogs))
		! ;do(i,1,len(sawdata%case))
 
		! * capacity constraints
		! ;if(sawdata(capacity"j",row->"i").gt.0)@sawlogs("j")%2%@sawdata%case("i")<sawdata(capacity"j",row->"i")
 
		! ;enddo
		! ;enddo
 
		! ;do(j,1,len(pulplogs))
		! ;do(i,1,len(pulpdata%case))
 
		! ;if(pulpdata(capacity"j",row->"i").gt.0)@pulplogs("j")%2%@pulpdata%case("i")<pulpdata(capacity"j",row->"i")
 
		! ;enddo
		! ;enddo
		! /
 
		! li=;list(fprob%?);
		! @li;
		! f=jlp(problem->fprob,data->xdata,utiltrans->util,knn->2)
		! ;return
 
 
 
 
		!endex
		!endsection
		!VAR
 
		!	p_ivproblem problem
		! p_ivrow text for rows
		! p_nrow number of constraint rows
		! p_nrowtot =p_nrow+1
		! p_isdomain are there domains
		! p_ndom number of domains
		! p_ivdomain  object
		! p_ivobj REAL for the objective
		! p_nshow number of showdomains
		! p_ivshowunits %showunits numbers of units
		! p_ivshowtext '$showtext' domain defintions
		! p_ivshowmatrix $showdom
		! $showtrans transformation computing showdomains
		! p_nshowin  number of input variables in showdoamin
 
		! p_ivfeasible  %feasible
		! p_nxvar number of ordinary x-variables
		! p_nz number of z-variables
		! p_xpresent are there x-variables (not necessary in factory problems
		! p_fpresent are factories present
		! p_factnow factories now
		! p_nxvartot number of keep variables in xdata
		! p_ivrowx ILIST for rows having x-var
		! p_rowx points to x-rows
		! p_nsumx currently p_nxvartot later possibly variables made with transformations
		! p_fastdif given in fastdif->
		! p_p8 p_p p_p9 printing logicals
		! p_n16 unit for test writing
		! p_feasible1
		! p_time00 starting time
		! p_ivrhs p_ivrhs2 p_ivrhsw
		! p_rhs p_rhs2 p_rhsw p_rhscur pointers to RHS
		! p_lower is lower bound active
		! p_nvar number of variables in the problem
		! p_vars point to variables
		! p_nterm number of terms
		! p_varofterm variable of each term
		! p_coef coefficients of the terms
		! p_rowofterm row of term
		! p_ix temp variables for roows 0:p_nrow
 
 
		!optimization
		! p_maxrounds maximum number of rounds
		! p_kierstep ?
		! p_kierout ?
		! p_kier current round
		! p_unit current unit
		! p_xirowold2=p_objf ?
		! p_coefmax -1 for minimization
		! p_maxo is maximized
		! p_objf  objective
		! p_objfv  previous objective
		! p_objfprev objective in the previous round
		! j_divobstarted is optimization started
		! newc
		! p_muutosb  changes in the basis
		! p_pivot number of pivots
		! p_pivotold number of previous pivots
		! p_idebug pivot where to debug
		! p_fastmakes how many times fast made
		! p_isfastp is fast%->
		! p_activeunit is unit active
		! p_nrefac refactorization
		! p_isstop is stop->
		! p_nimp !number of improvements outside active set
		! p_warm
		! p_time0 time when round started
		! p_asv percent in the active set
 
		!        = 1 residual enters
		!        =2
		!        =3
 
		! p_nnf number of infeasible rows
		!	p_nnfold2=p_nnf
 
		! output
		! p_iprint is there printing
		! p_nureport unit for report
		! p_echo print to screen if p_nureport.ne.6
 
 
		! FACTORIES
		! p_ntable number of tables
		! p_nfact number of factories
		! p_fact  all factories
		! p_nlog number of log types
		! p_log all log types
		! p_nlogfact number of log%2%fact terms
		! p_logfactbas basis
		! p_knn
		! p_kntot
		! p_kntot2  neigbours of all units
		!
 
		!DATA
		! p_isunit is unit->
		! p_nunits
		! j_dnobs number of schedules
		! j_divkeepup
		! j_divdataup p_ivdatax
		! j_divmat
		!
		! Fletcher
 
		! p_lavec
		! p_nrowz mx number of columns of A , D + z-var
		! p_ncol =p_nrowz
		! p_nrow2z=p_ncol+p_nrow
		! p_nrowz=p_nrow+p_nz  ! basis (first element -1) of D part, when I is included
		! p_xma maximum number of w
		! p_lx0 number of basic w
		! p_lz0 number of basix z
		! p_lr0 number of basic residuasl
		! p_lf0 number og basci factory vars
 
		!	p_priced=.false.
		p_xps00=j_0
	p_pivotrz=0
	p_n16=16
	p_isn16=j_v(j_ivdollar2).eq.157.
	if(p_isn16)write(p_n16,*)'jlp()'
	!	p_bad=0
	p_newc=0  ! for testing purposes in aftpivot
	call j_startfunction(iob,io,0,narg,j_arg,j_ivout,needsout=.true.)
	if(j_err)return
	!	call j_getdataobject(iob,io)
	call start(iob,io) !   jlp(   p_ivproblem,p_ivrow,p_nrow,p_nrowtot,call jlpcoef,usejlpcoef,p_ivfeasible,p_ivobj
	!j_mdpivot,p_pivotstep
	!jlpcoef: ! p_coefvars,p_varofterm,p_nterminrow,p_rowofterm,p_coefplus,p_vars
	! usejlpcoef
 
	!	call jlpcoef_(p_ivproblem,j_ivout,ivobjects)  !jlp(  interpret coef
	write(6,*)'piece-variables ',p_npiece
	! p_coefvars
	! p_varofterm
	!	p_nterminrow
	! p_rowofterm
	! p_coefplus
	! p_vars
	!	write(6,*)'heer2,fpresent',p_fpresent,p_ntable
 
 
 
	if(j_err)return
	!	call j_getname(ivobjects)
	!	write(6,*)'ivobjectstas ',j_oname(1:j_loname)
	!call usejlpcoef(ivobjects)  !fpresent comes from here
	!	if(j_err)return
	call initdata0(iob,io)  !! init data also when there are no constraints
	! p_ivarea,p_isunit,j_divdataup,p_ivdatax,j_divobsup,j_divkeep,j_dnkeep,j_divmatup
	! j_divkeepup,j_divobsup,j_divmat,j_dnobs,j_dmat,j_divobs,j_ivns,j_filterlink
	!p_filter,j_rejectlink,p_reject,p_filre,p_isxvar,p_istermx,p_ivnxinrow,p_ninrowx(0:)
	!p_nrowcurx,p_nxvar,p_nstot,p_ibaunitbas,p_nrejtot,,p_maxns,p_nunits,p_ivxvar
	!p_xvar,p_ivibaunit,p_ibaunit,p_ivkeys,p_keys,	p_ivns,p_ns
	!p_isunit,p_divobsup,p_unitvars,p_unitkeep,
	! integer::j_divdata,j_dfilterlink,j_drejectlink,j_divtrans,j_divvars,j_divmat,j_divvars2
	! integer::j_divdataup,j_dfilterlinkup,j_drejectlinkup,j_divtransup,j_divvarsup,j_divmatup,j_divvars2up
	!p_istermx,p_nxvar etc
 
	if(j_err)return
	write(6,*)p_nterm,' terms ',p_nrow,' constraints'
	write(6,*)'factories present:',p_fpresent
 
	if(p_fpresent)write(6,*)'aftfterms'
	!write(6,*)'tablepart',p_tablepart
	!write(6,*)'logpart',p_logpart
	!write(6,*)'factpart',p_factpart
	if(j_err)return
	!	call j_getname(ivobjects)
	!write(6,*)'ivobjectstas ',j_oname(1:j_loname)
 
 
 
	!	write(6,*)'p_nsumx ',p_nsumx
	!	ndom2=max(p_ndom,1)
 
	ivprint=j_igetopt(iob,io,j_mprint)
 
	if(ivprint.gt.0)then
 
		p_iprint=j_v(ivprint)
	elseif(ivprint.eq.0)then !if(ivprint.gt.0)then
		p_iprint=2 !testauksen ajaksi
	else !if(ivprint.gt.0)then
		p_iprint=1
	endif !if(ivprint.gt.0)   4159
 
 
	!	p_row0=1   !updated for domainprob
 
 
	!	ibaxdat(iobs)=(iobs-1)*j_dnkeep
	p_p8=.false.  !.true. !.true. !j_v(j_ivdollar2).eq.8.d0
	p_p=j_v(j_ivdollar2).eq.8.d0   !.false.  !.true.
	p_p9=.false.  !.true.
 
	p_zerob=0
	p_feasible1=.true.
	call cpu_time(p_time00)
	!write(6,*)'time00tas',time,p_time0,p_time00
 
	!	p_fpresent=j_o(ivout)%i(17).ne.0
	if(p_p8)write(p_n16,*)'<33startinitjlp'
	if(p_p8)write(6,*)' '
 
 
	call commonopt(iob,io)
	!p_ispullout,p_idebug1,p_idebug2,p_ivoptimal,p_ivpivots,p_stoplink,p_isstop,p_fastplink
	!p_nrefact,Pubou etc
	!write(6,*)'tasa'
	if(j_err)return
 
	!	p_fpresent=.false.  !j_o(p_ivproblem)%i(17).ne.0  !factgroup
	!done always even if no x
 
 
	! her x-data or c-data
 
 
 
	!if(p_xpresent)then
	!	write(6,*) 'call inijlp2 ',p_xpresent
	call initjlp2(iob,io)  ! jlp(
	!get options and when xpresent
	! p_ivtrans
	! p_maxrounds
	! p_zopt
	! p_ivix
	! p_ix
	! p_nnotareavars
	! p_tried
	! p_fastusedsame
	! p_intapp
	! p_fastmake
	! p_fastpros p_fastpros2  /100
	! p_fast
	! p_warm p_warmf
 
	!	call initflet()
	!	write(6,*)'xphe',p_xpresent
	if(j_err)return
	!write(p_n16,*)'start initxdata'
	if(p_p)write(6,*)' '
 
 
	if(.not.p_isunit)p_maxns=j_o(j_divdataup)%i(9)
	!is(p_p8)write(6,*)'<pmaxns ',p_maxns
 
	!	call j_getdat(p_ivdatax,j_dnobs,j_divmat,j_divkeep)
 
	if(j_err)return
	!	j_dnkeep=j_o(j_divkeep)%i(1)
	!	j_dmat=>j_o(j_divmat)%d(1:j_dnkeep*j_dnobs)
	p_xpresent=p_nxvar.gt.0
 
	if(.not.(p_xpresent.or.p_fpresent))then
		write(6,*)'factory optimization does not work' !there are no x-variables or  f-variables'
		j_err=.true.
		return
 
	endif !if(.not.(p_xpresent.or.p_fpresent))   4236
	!write(6,*)'xpresent ',p_xpresent
	!write(6,*)'p_ibaunit, bef istarxataw',p_ibaunit
 
 
	write(6,*)' '
	write(6,*)'x-variables',p_nxvar
	!	if(p_nxvar.gt.0)write(6,*)j_o(p_ivxvar)%i2(1:p_nxvar)
 
 
	p_nxvartot=j_dnkeep  !j_o(j_divkeep)%i(1)
 
	call initarea(iob,io)
	!write(6,*)'area', p_nxvararea,p_xvararea
	if(j_err)return
	p_nsumx=p_nxvararea  !+p_noutsubtrans !p_ncvar+
 
 
	p_nz=p_nvar-p_nxvar-p_nvarf
	p_isz=p_nz.gt.0
	!	write(6,*)'nzhere',p_nz
	if(p_intapp.and.p_isz)then
		write(6,*)' '
		write(6,*)'*there were ',p_nz," z-variables, can't use integer->"
		j_err=.true.;return
	endif !if(p_intapp.and.p_isz)   4262
	!if(p_isz)call initzjlpz() !jlp(
 
 
	if(j_err)return
 
	!	zpresent=p_isz
	write(6,7777)'z-variables',p_nz
	p_nrowz=p_nrow+p_nz
 
	if(p_xpresent)then
		call initxdatawx()
		call initxdata()
		if(p_nrow.gt.0)then
			!	la=p_nrow ! number of rows in Fletcher
			p_lavec(1)=p_nrow  !la
			!	p_mxd=p_nrowtot+4
			!	p_nrowz=p_nrow+p_nz !mx number of columns in A  , I.e. D+ coefficients of z-variables
 
			!		write(6,*)'p_nrowz',p_nrowz
			!		read(5,*)jhhdhd
			p_ncol=p_nrowz+1 !!!
			p_ncoltot=p_ncol+p_nrow
			p_nrow2z=p_nrowz+p_nrow !mx number of columns (icluding the I part)
			!	p_nrow2z=p_ncol+p_nrow = p_nrow2z
			!	write(6,*)'p_ibaunit, bef istart',p_ibaunit
			call startlist()
			!	write(6,*)'p_lf aft start',p_lf(1:10)
		endif !if(p_nrow.gt.0)   4279
		!	call initxdatjlp()
		!	write(6,*)'p_lf ahggdg',p_lf(1:10)
	endif !if(p_xpresent)   4276
 
	write(6,*)' '
	!if(p_isz)write(6,*)'zvars',p_zvars(1:p_nz)
	if(p_p)write(6,*)'<444datainitilized'
	if(p_p)write(6,*)' '
 
 
	if(p_fpresent)then
		call fterms(iob,io)
		call initfact()
		if(j_err)return
		! else
 
		! call initkeysx()
	endif !if(p_fpresent)   4305
	!write(6,*)'p_ibaunit,af istart',p_ibaunit
	! p_ivls p_ls
	! p_ivlsi p_lsi
	! p_ivlr p_lr
	! p_ivlri p_lri
	! p_ivobjr0
	! p_objr0
	! p_ivobjr2
	! p_objr2
	! p_ivlz
	! p_lz
	! p_ivlx
	! p_lx
	! p_ivlxi
	! p_lxi
	! p_ivlf
	! p_lf
	! p_ivlfi
	! p_lfi
	! p_ivvc
	! p_vc
	! p_ivx
	! p_x
	! p_ivb
	! p_b
	! p_xmi
	! p_xma
	! p_iva
	! p_a
	!	write(6,*)'afterstart'
	!	write(6,*)'ls ',p_ls
	!write(6,*)'nshye',p_ns(1:5)
	if(j_err)return
 
 
 
 
 
	!	write(6,*)'inttt',p_intapp
 
 		 7777 format(a17,i5)
	call startx(iob,io)
	if(j_err)return
	call initxdatjlp()
	if(j_err)return
	if(p_isz)then
		call initz0()
		if(j_err)return
		call initzjlp() !jlp(
		if(j_err)return
		if(p_n16.gt.0)then
			write(p_n16,*)'z matrix'
			do ii=1,p_nrow
				write(p_n16,'(i2,(15f7.3))')ii,(p_a(p_abas(iz)+ii),iz=1,p_nz)
			enddo !ii=1,p_nrow   4365
		endif !if(p_n16.gt.0)   4363
	endif !if(p_isz)   4358
	!	p_tolecur=1.d-6
	if(p_fpresent)then
 
		call initfeas0()
		if(j_err)return
		call printfact()
		if(j_err)return
	else
		call initxmatxps()
		if(j_err)return
		if(j_isoption(iob,io,j_mmatrix))then
			ivtot=j_defmatrix(j_ivout,'%xmattot',int8(j_dnobs),int8(p_nrowtot),j_matreg)
 
 
			call j_getname(ivtot)
			write(6,*)'generated ',j_oname(1:j_loname),' i ',j_dnobs, ' X ',p_nrowtot,&
				' matrix due to matrix->'
 
			if(p_isdomain)then
				ibaout=0
				ibain=0
				! write(6,*)'p_ndomv',p_ndomv,'p_ndomvars',p_ndomvars,'p_ido1',p_ido1
				! write(6,*)'p_nixcu',p_nixcu
				! write(6,*)'p_icurint',p_icurint
				! write(6,*)'p_icurbit',p_icurbit
				! write(6,*)'p_ixcubas',p_ixcubas
				! write(6,*)'	p_ixcurow',p_ixcurow
				! write(6,*)'	p_domainbits',p_domainbits
 
				do iuni=1,p_nunits
					call jlpcurix(iuni)
					write(6,*)'unit',iuni,p_ix
					do j=1,p_ns(iuni)
						do i=1,p_nrowcurx
							ir=p_rowcurx(i)
							j_o(ivtot)%d(ibaout+ir+1)= &
								p_xmat(ibain+p_ix(ir))
 
						enddo !i=1,p_nrowcurx   4403
						ibain=ibain+p_ntemp0
						ibaout=ibaout+p_nrowtot
					enddo !j=1,p_ns(iuni)   4402
				enddo !iuni=1,p_nunits   4399
 
			else
				j_o(ivtot)%d=p_xmat
 
			endif !if(p_isdomain)   4388
			!	write(6,*)' *return due to matrix->'
			!		return
		endif !if(j_isoption(iob,io,j_mmatrix))   4380
 
	endif !if(p_fpresent)   4371
 
 
 
	if(p_fpresent)then
		if(p_p)write(6,*)'start initfact'
		if(p_p)write(6,*)' '
		!	call initfact(iob,io)
		if(p_p)write(6,*)'end initfact'
		if(j_err)return
		write(6,7777)'tables',p_ntable
		write(6,7777)'factories',p_nfact
		write(6,7777)'log-variables',p_nlog
		write(6,7777)'logfact-variables',p_nlogfact
		if(j_err)return
		!	call initfact()
 
		if(.not.p_zmatrix) p_nz=p_nvar-p_nxvar-p_nlogfact-p_ntable
		!write(6,*)'nztas',p_nz
	else
		if(.not.p_zmatrix) p_nz=p_nvar-p_nxvar
 
 
	endif !if(p_fpresent)   4426
	!write(6,*)'p_ibaunit, bef init',p_ibaunit prednisoloni  pantopratsolia
	!	call initxdata()
	!write(6,*)'p_ibaunit, af init',p_ibaunit
 
	if(p_p)write(6,*)'start initopt'
 
	!	write(p_n16,*)'p_nrow2z ',p_nrow2z
	if(p_nrow.gt.0)call initopt()  ! jlp
	!	write(6,*)'nshtase',p_ns(1:5)
	!	write(p_n16,*)'p_nmaftinitop ',p_nrow2z
	if(j_err)return
	if(p_p)write(6,*)'start initxdatjlp'
	!	if(allocated(p_rowcurx))write(6,*)'i66rowx cur ',p_rowcurx(1:p_nrowcurx)
	!	write(6,*)'rethere'
	!	if(p_fpresent)return
	!if(p_nrow.gt.0)then
	!write(6,*)'jerrhere',j_err
	!call initxdatjlp() !needs fact    updates p_ns
	p_objf0=p_xps(0)
	j_v(p_ivobj0)=p_xps(0)
	write(6,*)'***initial objective ',p_objf0
	! if(p_nrow.le.0)then
	! write(6,*)'No constraints, that was all'
	! p_feasible=.true.
	! call tulostele()
	! return
 
	! endif !if(p_nrow.le.0)   4356
	!	endif !if(p_nrow.gt.0)   4349
	!write(6,*)'after',j_err
	!write(6,*)'nshNYT',p_ns(1:5)
	!	write(p_n16,*)'p_nmaftinitop ',p_nrow2z
	if(j_err)return
	!	if(allocated(p_rowcurx))write(6,*)'i77rowx cur ',p_rowcurx(1:p_nrowcurx)
	!	if(p_fpresent)call factxps(i),p_zopt
	write(6,*)p_nz,' z-variables:'
	!write(6,*)'p_zopt,j_err',p_zopt,j_err
	if(p_isz)	call printz()
	if(p_isz.and. .not.p_zopt)then
		write(6,*)'*jlp: if there are z-variables, there must be z-> option, zvariables:'
		call printz()
		j_err=.true. ;return
	endif !if(p_isz.and. .not.p_zopt)   4484
 
 
 
 
 
	call cpu_time(time1)
	write(6,*)' '
	write(6,*)'*jlp initialization took ',time1-p_time00,' sec',j_err
	if(p_nrow.eq.0)then
		write(6,*)'no constraints'
		p_objf=p_xps(0)
		if(p_nvar.ne.p_nxvar.and..not.p_fpresent)then
			write(6,*)p_nvar-p_nxvar,' variables were not in data'
			j_err=.true.
 
		endif !if(p_nvar.ne.p_nxvar.and..not.p_fpresent)   4500
		j_v(p_ivfeasible)=j_1
		p_feasible=.true.
		!	call jlp00(iob,io)
		if(j_err)return
		!	write(6,*)'getsumxobj',p_objf
		call getsumxkey(0)
		!	write(6,*)'tasasas ',j_v(p_xvararea(3))
		!	write(6,*)'sumx',p_sumx
		write(6,*)'rpony'
		call repo(p_nureport)
 
		return
		!		call tulostele
 
	endif !if(p_nrow.eq.0)   4497
	!!call j_clearoption(iob,io)  !jlp()
 
	!	write(6,*)'ibaunittas',p_ibaunit
	!p_n16=16
	!p_p=.true.
	!	write(6,*)'j_divmatup,j_divmat ',j_divmatup,j_divmat
	!xpresent2: are there ordinary x-variables
	!	p_xpresent= p_nxvar.ne.0
 
	! if(.not.p_xpresent)then
	! if(.not.p_zmatrix) p_nz=p_nvar
	! !*p_ix0 the x-variable of the objective function
	! p_ix0=0
	! p_nunits=0
	! end if !if(.not.p_xpresent)   2856
	!if(p_nrow.ne.0.and..not.p_fpresent.and.p_nz.eq.0)call preopt()
	!p_kier=0  !number of iterations through data !!!!
	!if(allocated(p_rowcurx))write(6,*)'i555rowx cur ',p_rowcurx(1:p_nrowcurx)
	!write(6,*)'j_err bef feas ',j_err
	p_objf=p_small
	if(p_xpresent)then
		j_v(p_ivfeasible)=j_1
		call checkinfeas()
		!if(allocated(p_rowcurx))write(6,*)'i5333rowx cur ',p_rowcurx(1:p_nrowcurx)
		!	call checkinfeas()
		!write(6,*)'j_err feas ',j_err
		if(j_v(p_ivfeasible).le.j_0)return
 
	endif !if(p_xpresent)   4540
 
	!  nrow =number of constraints=number of variables for Fletcher's LP
	! first index is for row number second for column
 
	call initfltot() !jlp(
 
	p_unitv=-1
	p_kierv=1
1234 continue  !we come here after failure,   !jl 20160510 siirretty tähän !!!!
!write(6,*)'newstart'
!write(16,*)'newstart'
	p_xirowold2=p_small
 
	!	if(p_xpresent)then
	p_next(0)=0;p_prev(0)=0
 
 
	p_ivrhsw=j_defmatrix(j_ivout,'%rhsw',int8(p_nrow),j_18,j_matreg)
	p_rhsw=>j_o(p_ivrhsw)%d(1:p_nrow)
	! get currenet RHS and working rhs
	!p_lower=.true.
	do i=1,p_nrow
		! lower-logiikka epävarma vielä
 
		if(p_xpsrow(i))then   ! there is a x-variable in the row
			! x-variable present -xps goes to the rhs
 
			if(p_lbou(i).and.p_ubou(i))then
				if(p_xps(i).lt.p_rhs(i))then
 
					p_rhscur(i)=p_rhs2(i)
					p_lower(i)=.false.  ! jlp(
				elseif(p_xps(i).gt.p_rhs2(i))then
					p_rhscur(i)=p_rhs(i)
					p_lower(i)=.true.
 
				else
					p_rhscur(i)=p_rhs(i)
					p_lower(i)=.true.
 
 
 
				endif !if(p_xps(i).lt.p_rhs(i))   4577
 
 
			elseif(p_lbou(i))then
				p_lower(i)=.true.
				p_rhscur(i)=p_rhs(i)
 
 
			else   !ubou
				p_lower(i)=.false.
				p_rhscur(i)=p_rhs2(i)
			endif !if(p_lbou(i).and.p_ubou(i))   4576
 
			!xps is the sum over key schedules
			p_rhsw(i)=p_rhscur(i)-p_xps(i)  ! 6.27 and 6.28 p. 110 , get working rhs
			!	if(p_fpresent)then
			!		p_rhsw(i)=p_rhscur(i)-p_xps(i)-p_xpsf(i)
			!	else
			!	p_rhsw(i)=p_rhscur(i)-p_xps(i)
			!	endif !if(p_fpresent)   4981
			!	if(i.eq.2)	write(27,*)'<353553rhs ',p_rhscur(i),p_xps(i)
		else !if((j_ix(i).ne.0).or.j_fpresent)then
			if(p_lbou(i))then
				p_rhscur(i)=p_rhs(i)
				p_lower(i)=.true.
			else !if(j_lbou(i))then
				p_rhscur(i)=p_rhs2(i)
				p_lower(i)=.false.
			endif !if(p_lbou(i))   4613
			p_rhsw(i)=p_rhscur(i)
		endif !if(p_xpsrow(i))   4573
		if(p_rhs2(i).lt.p_rhs(i))then
			write(6,*)'*constraint ',i,' upper limit ',p_rhs2(i),' less than lower limit ',p_rhs(i)
			j_err=.true.
		endif !if(p_rhs2(i).lt.p_rhs(i))   4622
		!	write(6,*)'iii',i,p_xpsrow(i),p_xps(i),p_rhs(i),p_rhs2(i),p_lower(i)
	enddo !i=1,p_nrow   4570
	if(j_err)return
	!	p_x(1:p_nrow)=p_rhsw(1:p_nrow)
	!	write(6,*)'rhscur',p_rhscur
	!	write(6,*)'rhsw',p_rhsw
	!	write(6,*)'lower',p_lower
 
	! Fletcher subroutines initilize data structures for taht many cols
	!*nrowz
	!	p_nrowz=p_nrow+p_nz  ! basis (first element -1) of D part, when I is included
	! coefficients for residuals
	! coefficients for d columns
	do j=p_nrowz+1,p_nrow2z !!!ncol ol liian vähän
		p_xma(j)=1.  ! maximum value for w, later area of the unit
	enddo !j=p_nrowz+1,p_nrow2z   4639
 
 
	if(p_nrow.eq.0.and..not.p_fpresent)then   ! no constraints, was optimized directly
		if(p_isz)then
			write(6,*)'**no constraints and z variables=> illegal'
			j_v(p_ivfeasible)=j_0
			j_v(p_ivoptimal)=j_0
			j_v(p_ivobj)=-9.9
			j_err=.true.;return
		endif !if(p_isz)   4645
		!	p_objfv=p_objf
		p_objf=p_xps(0)
		!	p_vx(0)=j_one
 
		p_lx0=0
		p_lz0=0
		p_lr0=0
		!p_ln0=0
		!	p_lf0=0
		if(p_xpresent)then
			!	call defsolu()
			call getsolx()
		endif !if(p_xpresent)   4661
		j_v(p_ivfeasible)=j_one
		j_v(p_ivoptimal)=j_one
		j_v(p_ivobj)=p_coefmax*p_objf
		j_v(p_ivstartedjlp)=1.
		write(6,*)'no constraints, objective ',p_coefmax*p_objf
		if(p_nureport.ne.6)call j_printname( &
		'*report written into:',ivreport,' which remains open')
		call repo(p_nureport)
		!	write(6,*)'77 ',p_nureport,p_echo
		if(p_nureport.ne.6.and.p_echo)call repo(6)
 
		return
	endif !if(p_nrow.eq.0.and..not.p_fpresent)   4644
	j_v(p_ivstartedjlp)=j_1 !!!!
 
 
 
	newc=0
 
	p_muutosb=0  !changes in the basis
	p_objfv=p_small    !for checking if there has not been improvemne tsince last !!!!
	p_objf = p_small   !!!!
	p_objfprev=p_small
	! visit to unit one
 
	!  solves a system  B.x=b
	!      subroutine fbsub(n,jmin,jmax,a,la,q,b,x,ls,aa,ll,save)
	!
	!   ls(*)  an index vector, listing the components of x that are required.
	!       Only the absolute value of the elements of ls are used (this allows
	!       the possibility of using of the contents of the ls parameter of bqpd).
	!       Elements of x in the range abs(ls(j)), j=jmin:jmax are set by fbsub.
	!       These contortions allow bqpd to be independent of the basis matrix code.
	!       call fbsub(n,1,3,a,la,0,b,x,ls,ws(lu1),lws(ll1),.false.)
	! **********************************************************************************
	! structure of the algorithm
 
	!55    compute values of basic variables
 
	!(* poista kun toimii
	! testing w-varaibles, if problem goto 667
	! testing structure of ld and next, problem->write 'saa **', return)
 
	! check if rows are feasible, i.e . if residuals get legal values
	! if residual too small, start maximizing it, if too large, start to minimize it
	! compute prices goto 34
	! if rows are feasible, maximize objective, compute prices , goto 34
 
	! 34   choose the entering variable
	!        if residual could enter, ienter=1, goto 100 (forward)
	!       note: it is useful to enter residual even if the objective function
	! does not increase, this will deacrese unnecessary change of schdedules
	! and laso the residual part is computationally faster
	!       if z could enter, ienter=2 goto 100 (forward)
	!       compute vx, ie. prices for x-varianbles
 
	!667   tähän tullaan kun havaitaan epäloogisuus w:ssä
	!       tai tullaan ylhäältä , testataan loogisuuksia, poistetaan myöhemmin
 
	!400      goto next unit , or return if you were here earlier
	!           and since then there has been no change in basis
	! if we are in first unit and there has not been any improvement then
	! also return
 
	! compute if new schedule can enter
	! if not, goto 400 (i.e. goto next unit)
	!       ienter=3
 
	!100   selecting leaving variable
	!first compute r-vector which tells how basic varaibles will change
	!  per unit of entering variable
	! go trough all possible cominations when a basic variable can p_leave
	! residual (and later z variable) can p_leave by reaching upper or lower limit
	! check if key schedule is leaving ***************************************************
	!        if key is leaving udate xps (sums over  keyschedules) and objr
	!       if there are additional basic schedules pivot first these columns to correspond
	!      the new key
	!if schedule is entering and no variable is leaving-> change just key
	! otherwise pivot entering and leaving columns
	! update link lists
	! goto 55 (backwards)
 
 
	! computes values of basic variables
	!		subroutine fbsub(n,jmin,jmax,a,la,q,b,x,ls,aa,ll,save)
	!  solves a system  B.x=b
 
	!  Parameter list
	!  **************
	!   n   number of variables (as for bqpd)
	!   a,la   specification of QP problem data (as for bqpd)
	!   jmin,jmax  (see description of ls below)
	!   q   an integer which, if in the range 1:n+m, specifies that the rhs vector
	!       b is to be column q of the matrix A of general constraint normals.
	!       In this case the parameter b is not referenced by fbsub.
	!       If q=0 then b is taken as the vector given in the parameter b.
	!   b(n)  must be set to the r.h.s. vector b (but only if q=0)
	!   x(n+m)  contains the required part of the solution x, set according to the
	!       index number of that component (in the range 1:n for a simple bound and
	!       n+1:n+m for a general constraint)
	!   ls(*)  an index vector, listing the components of x that are required.
	!       Only the absolute value of the elements of ls are used (this allows
	!       the possibility of using of the contents of the ls parameter of bqpd).
	!       Elements of x in the range abs(ls(j)), j=jmin:jmax are set by fbsub.
	!       These contortions allow bqpd to be independent of the basis matrix code.
	!   aa(*)  real storage used by the basis matrix code (supply the vector
	!       ws(lu1) with ws as in the call of bqpd and lu1 as in common/bqpdc/...)
	!   ll(*)  integer storage used by the basis matrix code (supply the vector
	!       lws(ll1) with lws as in the call of bqpd and ll1 as in common/bqpdc/...)
	!   save   indicates if fbsub is to save its copy of the solution for possible
	!       future use. We suggest that the user only sets save = .false.
 
 
 
	! compute the values of basic variables
	if(.not.p_maxo)write(6,*)'**minimization: maximize -objective'
	p_niter=0
	!nkier=p_mxiter
	nunits=p_nunits
 
	! if(p_idebug.eq.p_pivot.or.j_err)then
	! idebug1=j_v(j_ivdebug)
	! !call pullout(LWSLL1)
	! write(6,*)'pivot ',p_pivot
	! j_yes=j_err
	! call j_pause('Bdebug>',do=.true.)
	! if(j_yes)then
	! j_err=.true.
	! return
	! endif !if(j_yes)   4781
	! !if(p_pivot.ge.p_pullout1.or.j_err)call pullout(LWSLL1)
	! idebug2=j_v(j_ivdebug)
	! if(idebug1.ne.idebug2)then
	! p_idebug=idebug2
	! else
	! p_idebug=p_idebug+1
	! endif !if(idebug1.ne.idebug2)   4787
 
	! endif !if(p_idebug.eq.p_pivot.or.j_err)   4775
	!	if(j_err)return
 
 
	!write(16,*)'ALKU updgoto900,goto1234,goto8888,goto8889',goto900,goto1234,goto8888,goto8889,&
	!p_unit,p_pivot,j_objf
	p_exitkier=.false.
 
 
	!note all other gotos tell where to go next in the upper level
	! except goto8888 and goto8889 are set in updatejlp
	!and they tell where to go in leaving()
	!all gotos are cleared in each subroutine if they are set in the subroutine
 
	!if(kier.gt.1.and.iterxkf*int(p_kier/iterxkf).ne.p_kier)cycle unitloop !goto 400
	!		iterxkf number of rounds without cheking factories
	nofact=0  !current number of rounds without factories
	p_factnow=p_fpresent
 
	!write(6,*)'alus',j_xmatlopp,j_xdatlopp,j_ibamatxs2,j_xdatibas2,j_xmatlast,j_xdatlast
	!write(16,*)'ienteralsu',ienter,'p_nnf',p_nnf
	!write(6,*)'br, bz, bs,bxkf =number of basic residuals, z-vars, scheds, xkf-vars'
	call cpu_time(p_time0)
	!write(6,*)'timehuis',time,p_time0,p_time00
	write(6,*)'filling xmat took ',p_time0-p_time00,' cpu seconds'
	! write(6,*)'J-divmat',j_divmat
 
	! write(6,*)'ntermrow0',p_nterminrow(0)
	! write(6,*)'cofe',p_coef(1:p_nterminrow(0))
	! write(6,*)'objr0',p_objr0
	!	write(6,*)'objr ini',p_objr
 
	!write(6,*)'ALLO',allocated(p_rowcurx)
	!if(allocated(p_rowcurx))write(6,*)p_rowcurx
	write(6,*)' '
	write(6,*)'*optimization options:'
	p_pivotold=0
	p_fastmakes=0
	write(6,*)'refac->',p_nrefac,' tole->',p_tolep, &
		' p_warm->',p_warm, 'maxrounds->',p_maxrounds
 
	!if(p_xpresent2)then
	write(6,*)'fastrounds->',p_fastusesame
	if(p_isfastp)then
		write(6,*)'fast%->  see jlp(fast%->  initial value is Fast%=',p_fastpros
		write(6,*)'Note the Fast% shown in print is the previous Fast%'
	else
 
		write(6,*)'there was no fast%-> code option ,fast% will be ',p_fastpros
	endif !if(p_isfastp)   4846
 
	if(p_fpresent)write(6,*)'finterval->',iterxkf
 
	if(p_isstop)then
		write(6,*)'stop->   :see jlp(stop->'
	else !if(p_isstop)then
		write(6,*)'stop->(Change%.lt.0.01.and.Round.ge.10)'
	endif !if(p_isstop)   4856
	!endif !if(p_xpresent2)   4909
	write(6,*)' '
	write(6,*)'printing option, print->',p_iprint
	!if(memory.eq.0.and.j_xdatfrodisk)write(6,*)'without memory->, xmat is put into memory, but xdata is used from disk'
 
	!if(memory.eq.0.and..not.j_xdatfrodisk)write(6,*)'without memory-> all data is in memory'
	!	if(p_xpresent2)then
	write(6,*)' '
	write(6,*)'** Resid = # of basic residuals         z = # of basic z-variables'
	write(6,*)'   sched = # of explicit basic scheds  xkf = # of basic factory transportations'
	write(6,*)'      NF = # of nonfeafible rows       imp = entering sched not in active set'
	write(6,*)' Change% is multiplied with 10 before round 10'
 
	write(6,*)' '
	call j_getname(p_ivobj0,p_ivobj)
	write(6,*)'**objective initially ',j_v(p_ivobj0),' put into ',j_oname(1:j_loname),', final objective into ',j_oname2(1:j_loname2)
 
	iobs=0
 
	p_nimp=0  !number of improvements outside active set
	!	nimpr=0   !number of rounds with fastmake between printing
	p_asv=100.
	p_objfprev=p_small
	!pros=p_small
 
	call cpu_time(p_time0)
 
	p_time00=p_time0
	!	write(6,*)'p_time0,p_time00',p_time0,p_time00
	ncyc=0
	! write(6,*)' '
	! !	write(6,*)'Round  Pivots      Objective                           active% resid   z   sched  xkf  NF-rows'
	! write(6,*) &
	! 'Round  Pivots    Objective      Change% active%  resid z  sched xkf  NF   dCPU      CPU      Imp  Fast%'
	! !   2           19  15489.5951070      5.6924 100.00    1    0    4    0    0  0: 0.14    0: 0.14    0
	!	write(6,'(i5,i8,g19.12,7x,f8.2,5i5,f6.2,f7.2,i5)')&  written later
	!		0,0,p_coefmax*p_objf,100.,p_lr0,p_lz0,p_lx0,p_lf0,p_nnf,j_0, &
	!		j_0
 
	!write(6,*)'xmatfrodisk',j_xdatformdisk
	!if(j_o(ivxdatmat)%r(10261*keepx).eq.0.)stop 100
 
	ipivotstart=-1
 
	if(p_isn16)write(16,*)'isfeas0'
	if(p_isn16)write(16,*)'xps ',p_xps
	if(p_isn16)write(16,*)'rhscur',p_rhscur
 
	!row +resid =rhsw row initially 0 so initially p_x=p_rhsw
	p_rhsw=p_rhscur-p_xps(1:p_nrow)
	if(p_isn16)write(16,*)'rhsw ',p_rhsw,'pitais olla -rhscur -xps'
 
 
 
	call aftpivot() ! computes p_x, p_vc and p_objf
	if(j_err)return
	p_feasible=.false.
	!call leftvec(p_rhsw,p_x)
	if(p_isn16)write(p_n16,*)'into isfERS initx',p_x(1:p_nrow)
	call isfeasible()  ! jlp() initially
 
 
	write(6,*)'Initially ',p_nnf,' infeasible rows'
	write(6,*)' '
	!	write(6,*)'Round  Pivots      Objective                           active% resid   z   sched  xkf  NF-rows'
	write(6,*) &
		'Round  Pivots    Objective      Change% active%  resid z  sched xkf  NF   dCPU      CPU      Imp  Fast%'
 
 
 
	p_kierstep=1
	p_kierout=0
	!	p_changed=.true.
	p_enter=.false.
 
	if(p_isz)then
		p_objfz=j_ninf
		dorz=.true.
		do while(dorz)
			dorz=.false.
			if(p_isn16.and.p_isz)write(p_n16,*)'*into zenter befkier'
			if(p_isz)call zenter(dorz)
			if(p_isn16)write(p_n16,*)'*into renter befkier'
			call renter(dorz)
			!	write(6,*)hup
			!	read(5,*)rup
		enddo !while(dorz)   4939
	endif !if(p_isz)   4936
	p_pivot9=0
	p_nextlkm=0
	npivotold=-1
 
	kierloop: do p_kier=0,p_maxrounds
	!	write(6,*)'kier ',p_kier
		! ik100=p_kier/100
		! if(p_kier.eq.ik100*100)close(16)
		p_unit=1
		p_ibadat=0
		p_inebas=0
		p_keyfactbas=0
		! if(p_objf.gt.p_objfv)then
		! p_objfv=p_objf
		! else
		! exit
		! endif !if(p_objf.gt.p_objfv)   5183
		nopivots=p_pivot.eq.npivotold
		npivotold=p_pivot
 
		if(p_kier.eq.p_kierout)then
			call tulostele2(iob)
			if(p_exitkier)goto 900 !exit kierloop
			if(p_kier.eq.10)p_kierstep=10
			p_kierout=p_kierout+p_kierstep
		endif !if(p_kier.eq.p_kierout)   4969
 
		p_xirowold2=p_objf
		if(p_nnf.ne.p_nnfold2)p_xirowold2=p_small
		p_nnfold2=p_nnf
		if(p_fastmake)p_fastmakes=p_fastmakes+1
		p_pivotold=-1
		unitloop: do p_unit=1,p_nunits
		!	write(6,*)'punit',p_unit
			if((p_unit.eq.unitchange.and.p_kier.eq.kierchange+1).or.nopivots)then
				write(6,*)'*no change  Round=',p_kier,' unit', p_unit,' last change in round ',kierchange,' obj ',p_objf
				write(6,*)'unitchange ',unitchange,'nopivot',nopivot
				if(p_feasible)write(6,*)'SOLUTION FOUND'
				j_dapu=p_xps(0)+dot_product(p_x(p_ls(1:p_nrow)),p_objr(p_ls(1:p_nrow)))
 
				!	write(6,*)'dapu ',j_dapu
				goto 900
			endif !if((p_unit.eq.unitchange.and.p_kier.eq.kierchange+1).or.no   4983
 
			if(p_feasible.and.p_feasible1)then !first time feasible
				if(p_xps00.ne.j_0)then
 
					write(6,'(72x,a)')'**FEASIBLE* reference objf '//j_chr10(p_xps00)
				else
					write(6,'(72x,a)')'**FEASIBLE*'
				endif !if(p_xps00.ne.j_0)   4994
 
				write(6,'(i5,i8,g19.12,7x,f8.2,4i5,i3,a,f5.2,i5,a,f5.2)')&
					p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0, &
					p_nnf  !,iminc,':',secd,imint,':',sect
				if(p_isn16)write(16,*)'*FEASIBLE'
				p_feasible1=.false.
			endif !if(p_feasible.and.p_feasible1)   4993
			!	if(p_unit.eq.17383)write(6,*)'pkier,unit,act,fastmake',p_kier,p_unit,p_fastmake,p_activeunit(p_unit),'ns',p_ns(p_unit)
			if(p_activeunit(p_unit))then
				dorz=.false.
				!	if(p_isn16)write(p_n16,*)'into senter kierloop'
				!		if(p_unit.eq.17383)write(6,*)'senter**'
				call senter(dorz)  !changed
				!		if(p_unit.eq.17383)stop
 
 
				!			if(p_pivot.gt.325400)write(21,*)'unitdorz',p_unit,dorz
				if(j_err)return
				!	call rint('aftsenter',p_n16)
				if(dorz)then
					unitchange=p_unit
					kierchange=p_kier
					if(p_isn16)write(p_n16,*)'dorz, unitc,kier',unitchange,kierchange
 
				endif !if(dorz)   5019
				!	dorz=.true.
				do while(dorz)
					dorz=.false.
					if(p_isz)call zenter(dorz)
					call renter(dorz)
					!			if(p_isn16)call rint('aftrenter',p_n16)
					if(p_isn16)write(p_n16,*)'aft renter kier,dorz '
				enddo !while(dorz)   5026
			endif !if(p_activeunit(p_unit))   5008
 
			if(j_err)then
				call getout()
				write(6,*)'befsenter'
				return
			endif !if(j_err)   5035
			!		if(p_unit.eq.17383)write(6,*)'FINpkier,unit,ac t,fastmake',p_kier,p_unit,p_fastmake,p_activeunit(p_unit)
			!		if(p_unit.eq.17383.and.p_kier.gt.2)stop
		enddo unitloop !tloop: do p_unit=1,p_nunits   4981
		!	write(6,*)'DOMM',p_isdomain
	enddo kierloop !rloop: do p_kier=0,p_maxrounds   4953
 
	write(6,*)'iteration stops, maximum number of rounds through units ',p_maxrounds,' reached'
	write(6,*)'note you can increase this using maxrounds-> option'
 
900 continue
!	write(6,*)'p_kier,unitchan,kierca',p_kier,unitchange,kierchange
 
	p_objf=p_objf+p_xps00
	!	write(6,*)'tulostele'
	call tulostele()
	if(p_nureport.ne.6)call j_printname('*report written into:',p_ivreport,' which remains open ')
	!write(6,*)'repohere'
	call repo(p_nureport)
	if(p_echo.and.p_nureport.ne.6)call repo(6)
 
	call weightstot()
	call closeflet()
 
990  continue
	if(j_err) then
		p_buf='jlp error exit'
	else !if(j_err) then
		p_buf='jlp normal exit'
	endif !if(j_err)   5064
	!	call testxps('xpsexit')
	write(p_nureport,'(a)')p_buf(1:79)
	!	call  cpu_time(p_time0)
	p_buf=' '
	call getout()
	write(p_nureport,*)'total cpu-time in jlp() ',j_v(j_ivcpu)
	write(p_nureport,*)' '
 
	return
 
end subroutine jlp   ! jlp(iob

subroutine tulostele3()
	use jmod, only: p_pivot9
	use jmod, only: p_pivot
	use jmod, only: p_time0
	use jmod, only: p_time00
	use jmod, only: p_feasible
	use jmod, only: p_objfprev
	use jmod, only: p_small
	use jmod, only: p_coefmax
	use jmod, only: p_objf
	use jmod, only: p_nimp2
	use jmod, only: p_as
	use jmod, only: p_asv
	use jmod, only: p_nimp
	use jmod, only: p_kier
	use jmod, only: p_lr0
	use jmod, only: p_lz0
	use jmod, only: p_lx0
	use jmod, only: p_nnf
 
	!				if((p_unitprev.eq.p_unitv.and.p_kier.gt.p_kierv).or.(p_unitv==-1.and.p_kier.gt.p_kierv))then
	!!!!! there was no change since last visit to the same unit , optimum found,return !<B331>
	!maxiter
	!	write(16,*)'tasny,goto700',goto700
	!call tulostele()
 
	if(p_pivot9.ne.p_pivot)then  !vois katsoa muutkin muutokset
		call cpu_time(time)
		!					write(6,*)'timetakui',time,p_time0,p_time00
		iminc=(time-p_time0)/60.
		imint=(time-p_time00)/60.
		secd=time-p_time0-iminc*60.
		sect=time-p_time00-imint*60.
		p_time0=time
		if(p_feasible)then
			!		if(p_xpresent2)then
			if(p_objfprev.ne.p_small)pros= &
				abs(	100.d0*p_coefmax*(p_objf-p_objfprev)/p_objfprev)
			!	if(p_objfprev.ne.p_small)write(6,*)'proshere ',pros
			p_nimp2=-1
			if(p_as.ne.p_asv.and.p_asv.ne.100.)p_nimp2=p_nimp
 
 
			p_asv=p_as
			!	j_buf(1:12)=' ';j_buf(2:11)=j_chr10(j_objf-j_objfprev)
			!	j_objfprev=j_objf
			!write(6,*)'tassa'
			if(p_objfprev.ne.p_small)then
				write(6,'(i5,i8,g19.12,f8.5,f7.2,4i5,i3,a,f5.2,i5,a,f5.2,i6)')&
					p_kier,p_pivot,p_coefmax*p_objf,pros,p_as,p_lr0,p_lz0,p_lx0,&
					p_nnf,iminc,':',secd,imint,':',sect,p_nimp2
			else
				write(6,'(i5,i8,g19.12,7x,f8.2,4i5,i3,a,f5.2,i5,a,f5.2)')&
					p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0, &
					p_nnf,iminc,':',secd,imint,':',sect
 
			endif !if(p_objfprev.ne.p_small)   5110
			! else !if(j_xpresent2)then
			! write(6,*)'Feasible, objective:', &
			! p_coefmax*p_objf,'basic residuals ', p_lr0, &
			! ' basic z-vars ',p_lz0, ' cpu ',(time-p_time00)/60.
			! endif !if(p_xpresent2)   5633
			!time0=time
		endif !if(p_feasible)   5097
	else !if(ipivot9.ne.p_pivot)then
		!		if(p_xpresent2)then
		!write(6,*)'tassa2'
		write(6,'(i5,i8,g19.12,7x,f8.2,4i5,i3,a,f5.2,i5,a,f5.2)')&
			p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0, &
			p_nnf,iminc,':',secd,imint,':',sect
		!	else !if(j_xpresent2)then
		!	write(6,*)'Optimal, objective:', &
		!		p_coefmax*p_objf,'basic residuals ', p_lr0, ' basic z-vars ',p_lz0, ' cpu ',(time-p_time00)/60.
		!	endif !if(p_xpresent2)   4715
	endif !if(p_pivot9.ne.p_pivot)   5089
	!write(6,*)'<654exitkier'
 
 
 
end subroutine tulostele3


subroutine getnvarz()
	use jmod, only: j_linkoption
	use jmod, only: j_mdata
	use jmod, only: j_err
	if(j_linkoption(iob,io,j_mdata).ge.0)then
		write(6,*)'jlpz() cannot use data->, problems with schedules are solved with jlp()'
		j_err=.true.;return
	endif !if(j_linkoption(iob,io,j_mdata).ge.0)   5146
 
 
end subroutine
subroutine startlist0()
	use jmod, only: p_objr
	use jmod, only: p_ncoltot
	use jmod, only: j_defmatrix
	use jmod, only: j_ivout
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: p_ivvc
	use jmod, only: p_nrow
	use jmod, only: j_o
	use jmod, only: p_vc
	use jmod, only: p_vcold
	use jmod, only: p_r
	use jmod, only: p_r0
	use jmod, only: p_ivx
	use jmod, only: p_x
	use jmod, only: p_iva
	use jmod, only: p_ncol
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: j_deflist
	use jmod, only: p_ivls
	use jmod, only: p_ls
	use jmod, only: p_ivlsi
	use jmod, only: p_lsi
	use jmod, only: p_ivlr
	use jmod, only: p_lr
	use jmod, only: p_ivlri
	use jmod, only: p_lri
	use jmod, only: p_lr0
	! initilize lists defined in startlist(
	write(6,*)'*startlist0'  !,p_nrow,p_ncol,p_ncoltot
 
 
 
 
	if(allocated(p_objr))deallocate(p_objr);allocate(p_objr(1:p_ncoltot))
	!	p_objr=>j_o(p_ivobjr)%d(1:p_ncoltot)
	!	p_objr=>p_objr0
 
	! p_ivobjr2=j_defmatrix(j_ivout,'%objr2',j_18,int8(p_nrow2z),j_matreg)
	! p_objr2=>j_o(p_ivobjr2)%d(1:p_nrow2z)
 
 
	p_ivvc=j_defmatrix(j_ivout,'%vc',int8(p_nrow),j_18,j_matreg)
	p_vc=>j_o(p_ivvc)%d(1:p_nrow)
	if(allocated(p_vcold))deallocate(p_vcold)
	allocate(p_vcold(1:p_nrow))
 
	if(allocated(p_r))deallocate(p_r,p_r0)
	allocate(p_r(1:p_ncoltot),p_r0(1:p_ncoltot ))  ! eq.
 
	p_ivx=j_defmatrix(j_ivout,'%x',int8(p_ncoltot),j_18,j_matreg)
	p_x=>j_o(p_ivx)%d(1:p_ncoltot)
 
	!	p_a=j_0
	p_iva=j_defmatrix(j_ivout,'%a',int8(p_ncol+1),int8(p_nrow),j_matreg)
	p_a=>j_o(p_iva)%d(1:p_nrow*(p_ncol+1))
 
	!write(6,*)'p_ncol',p_ncol,p_ncoltot
	!read(5,*)jdfj
 
	if(allocated(p_abas))deallocate(p_abas)
	allocate(p_abas(1:p_ncol))
 
	! a-matrix is stored in column order because Fletcher assumes it so
	! p_abas(icol)+irow tells the index of column icol and row irow
	p_abas(1)=p_nrow   !there must be zero column initially
	!	p_abas1(1)=0
	do j=2,p_ncol
		p_abas(j)=p_abas(j-1)+p_nrow
		!	p_abas1(j)=p_abas1(j-1)+p_nrowtot
	enddo !j=2,p_ncol   5193
 
	! if(allocated(p_a))deallocate(p_a)
	! allocate(p_a(1:p_nrow,0:p_nrowz) ) ;p_a=j_0
 
 
	p_ivls=j_deflist(j_ivout,'%ls',list0=p_ncoltot,ilist=.true.)
	p_ls=>j_o(p_ivls)%i2(1:p_ncoltot)
 
	p_ivlsi=j_deflist(j_ivout,'%lsi',list0=p_ncoltot,ilist=.true.)
	p_lsi=>j_o(p_ivlsi)%i2(1:p_ncoltot)
 
	p_ivlr=j_deflist(j_ivout,'%lr',list0=p_nrow,ilist=.true.)
	p_lr=>j_o(p_ivlr)%i2(1:p_nrow)
 
	p_ivlri=j_deflist(j_ivout,'%lri',list0=p_nrow,ilist=.true.)
	p_lri=>j_o(p_ivlri)%i2(1:p_nrow)
 
 
	!p_xmi=p_zero ;p_xma=p_zero
 
	do j=1,p_ncoltot !nm=ncol+nrow    ! number of columns in (I A)
		p_ls(j)=j
		p_lsi(j)=j    !initially ls is in order, residuasl are in the basis
	enddo !j=1,p_ncoltot   5217
	!	write(6,*)p_nrow2z,'lsi',p_lsi
	do i=1,p_nrow
		p_lr(i)=i
		p_lri(i)=i	!inverse list
 
	enddo !i=1,p_nrow   5222
	p_lr0=p_nrow
 
 
 
end subroutine startlist0



subroutine initdomain()
	use jmod, only: j_o
	use jmod, only: p_ndomvars
	use jmod, only: p_ivdomvars
	use jmod, only: p_domvars
	use jmod, only: p_ivdomaintrans
	use jmod, only: p_ivproblem
	use jmod, only: j_divdataup
	use jmod, only: j_getname
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: p_isunit
	use jmod, only: j_inlistobject
	use jmod, only: p_ivunit
	use jmod, only: j_divkeepup
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	use jmod, only: j_err
	use jmod, only: p_isdomaintrans
	use jmod, only: p_ndomv
	use jmod, only: p_isndomv
	use jmod, only: p_rowdomvar
	use jmod, only: p_rowdomnum
	use jmod, only: p_ixcur0
	use jmod, only: p_nrow
	use jmod, only: j_line
	use jmod, only: p_ialldomain
	use jmod, only: p_ivdomain
	use jmod, only: j_itempvector
	use jmod, only: p_nrowtot
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivnixcu
	use jmod, only: p_ivixcubas
	use jmod, only: p_nixcu
	use jmod, only: p_ixcubas
	use jmod, only: p_ido1
	use jmod, only: p_idostep
	use jmod, only: j_ivall
	use jmod, only: p_ninrowx
	use jmod, only: p_ivixcurow
	use jmod, only: p_ixcurow
	use jmod, only: p_ivicurint
	use jmod, only: p_icurint
	use jmod, only: p_ivicurbit
	use jmod, only: p_icurbit
	use jmod, only: p_ix
	use jmod, only: p_ivdomainbits
	use jmod, only: p_nunits
	use jmod, only: p_domainbits
	use jmod, only: p_ivdomainunits
	use jmod, only: p_ndom
	use jmod, only: p_domainunits
	use jmod, only: p_nrowx0
	use jmod, only: p_rowcurx
	! 	if(p_isdomain)then
	write(6,*)'*initdomain'
	p_ndomvars=j_o(p_ivdomvars)%i(1)
	p_domvars=>j_o(p_ivdomvars)%i2(1:p_ndomvars)
 
	!write(6,*)'p_ndom ',p_ndom
	!	p_ndoms=j_o(p_ivproblem)%i(11)
 
	p_ivdomaintrans=j_o(p_ivproblem)%i(12)
 
	!j_o(ivout)%i2(1)=ivinl
	if(p_ivdomaintrans.gt.0)then
		ivinl=j_o(p_ivdomaintrans)%i2(1)
		ivunit=j_o(j_divdataup)%i(6)
		!	if(p_isunit)ivunit=j_divobsup
		call j_getname(ivunit,j_divdataup)
		write(6,*)'%domain joku',' ',j_oname(1:j_loname)
 
		do j=1,j_o(ivinl)%i(1)
			iiv=j_o(ivinl)%i2(j)
 
 
			if(p_isunit)then
				if(j_inlistobject(iiv,p_ivunit).gt.0)cycle
			else
				if(j_inlistobject(iiv,j_divkeepup).gt.0)cycle
				if(iiv.eq.ivunit)cycle
			endif !if(p_isunit)   5258
			call j_getname(iiv,ivunit)
			write(6,*)' '
			write(6,*)'*domain variable ',j_oname(1:j_loname),&
				' is not in c-data and is not the unit variable ',j_oname2(1:j_loname2)
			j_err=.true.
 
		enddo !j=1,j_o(ivinl)%i(1)   5254
		p_isdomaintrans=.true.
	else
		p_isdomaintrans=.false.
	endif !if(p_ivdomaintrans.gt.0)   5247
	if(j_err)return
	! iv=j_o(p_ivproblem)%i(13)
	! p_ndomvars=j_o(iv)%i(1)
	! p_domvars=>j_o(iv)%i2(1:p_ndomvars)
	! if(p_p8)write(6,*)'<p_domvars',p_domvars
 
 
	p_ndomv=j_o(p_ivproblem)%i(14)
	p_isndomv=p_ndomv.gt.1
 
	!is(p_p8)write(6,*)'*********p_ndomv ',p_ndomv
	iv=j_o(p_ivproblem)%i(15)
	p_rowdomvar(0:)=>j_o(iv)%i2( 1:j_o(iv)%i(1)) !start from
 
	iv=j_o(p_ivproblem)%i(16)  !index of row in domvars
 
	p_rowdomnum(0:)=>j_o(iv)%i2( 1:j_o(iv)%i(1))
	!is(p_p8)write(6,*)'********p_rowdomvar ',p_rowdomvar
	!	if(max(p_ndom,1).ne.p_ndoms) write(6,*)'number of domain occurences =',p_ndoms
 
	!	endif !if(p_isdomain)   4068
	!	if(p_isdomain)then
	!	if(allocated(p_irowdomain))deallocate(p_irowdomain)
	!allocate(p_irowdomain(0:p_nrow))
	allocate(p_ixcur0(0:p_nrow))
	!	p_ixcur0=.false.
	!	p_irowdomain=0
	!	write(6,*)'<666>',p_ivdomain
	p_ialldomain=j_line(p_ivdomain,'All') !which domain is the All-domain
	!	endif !if(p_isdomain)   4527
	!domains
	!	if(p_isdomain)then
	if(allocated(j_itempvector))deallocate(j_itempvector)
	allocate(j_itempvector(1:p_ndomvars*(p_nrowtot+1)))
	!is(p_p8)write(6,*)'START DOMAINS p_ninrowx',p_ninrowx
 
	ibas=p_ndomvars*p_nrowtot
	j_itempvector=0
 
	p_ivnixcu=j_deflist(j_ivout,'%nixcu',list0=p_ndomvars,ilist=.true.)
	p_ivixcubas=j_deflist(j_ivout,'%ixcubas',list0=p_ndomvars,ilist=.true.)
	p_nixcu=>j_o(p_ivnixcu)%i2(1:p_ndomvars)
	p_ixcubas=>j_o(p_ivixcubas)%i2(1:p_ndomvars)
	! if(allocated(p_nixcu))deallocate(p_nixcu,p_ixcubas)
	! allocate(p_nixcu(1:p_ndomvars),p_ixcubas(1:p_ndomvars))
 
	p_ido1=1
	p_idostep=0
	if(p_domvars(1).eq.j_ivall)then !All is not
		p_ido1=2
		p_idostep=1
 
 
	endif !if(p_domvars(1).eq.j_ivall)   5323
 
 
 
 
 
	p_nixcu=0
	p_ixcubas=0
	lkm=0
	do ido=1,p_ndomvars
		!		write(6,*)'p_ninrowx%%%%%%%%%%%%',p_ninrowx
		do iro=0,p_nrow
			if(p_ninrowx(iro).gt.0)then
				idomv=p_rowdomvar(iro) !domainvar
				if(idomv.eq.p_domvars(ido))then
					p_nixcu(ido)=p_nixcu(ido)+1  !how many
					j_itempvector((ido-1)*p_nrowtot+p_nixcu(ido))=iro
					!write(6,*)'ido,iro',ido,iro
				endif !if(idomv.eq.p_domvars(ido))   5342
			endif !if(p_ninrowx(iro).gt.0)   5340
 
		enddo !iro=0,p_nrow   5339
		lkm=lkm+p_nixcu(ido)
	enddo !ido=1,p_ndomvars   5337
 
 
	p_ivixcurow=j_deflist(j_ivout,'%ixcurow',list0=lkm,ilist=.true.)
	p_ixcurow=>j_o(p_ivixcurow)%i2(1:lkm)
	! if(allocated(p_ixcurow))deallocate(p_ixcurow)
	! allocate(p_ixcurow(1:lkm))
	! !is(p_p8)write(6,*)'p_nixcu ',p_nixcu
 
	lkm=0
	do ido=p_ido1,p_ndomvars
 
		do j=1,p_nixcu(ido)
			lkm=lkm+1
			p_ixcurow(lkm)=j_itempvector((ido-1)*p_nrowtot+j)
 
 
		enddo !j=1,p_nixcu(ido)   5363
		if(ido.lt.p_ndomvars)p_ixcubas(ido+1)=p_ixcubas(ido)+p_nixcu(ido)
	enddo !ido=p_ido1,p_ndomvars   5361
	!is(p_p8)write(6,*)'<ixcurow ',p_ixcurow,' p_ixcubas',p_ixcubas
	deallocate(j_itempvector)
 
 
 
	p_ivicurint=j_deflist(j_ivout,'%icurint',list0=p_ndomvars,ilist=.true.)
	p_icurint=>j_o(p_ivicurint)%i2(1:p_ndomvars)
	p_ivicurbit=j_deflist(j_ivout,'%icurbit',list0=p_ndomvars,ilist=.true.)
	p_icurbit=>j_o(p_ivicurbit)%i2(1:p_ndomvars)
 
	! if(allocated(p_icurint))deallocate(p_icurint,p_icurbit)
	! allocate(p_icurint(1:p_ndomvars),p_icurbit(1:p_ndomvars))
 
	do ido=p_ido1,p_ndomvars
		p_icurint(ido)=(ido-p_idostep)/32+1
		p_icurbit(ido)=ido-(p_icurint(ido)-1)*32-1
	enddo !ido=p_ido1,p_ndomvars   5384
	!is(p_p8)write(6,*)'<66icurint',p_icurint,'bit',p_icurbit
 
 
 
	!	endif !if(p_isdomain)   4962
	!if(p_isdomain)then
 
	!is(p_p8)write(6,*)'<4996 p_rowx',p_rowx
	!only if there are domains it  must be made difference between xror ans rowcurx
	! otherwise only cur is needed
	do iro=0,p_nrow
		if(p_ix(iro).ne.0.and.p_rowdomvar(iro).eq.j_ivall)p_ixcur0(iro)=.true.
 
	enddo !iro=0,p_nrow   5398
	! else
	! do iro=0,p_nrow
	! if(p_ix(iro).ne.0)p_ixcur(iro)=.true.
 
	! enddo
	!	else
	!	if(p_isdomain)then
 
	!	endif !if(p_isdomain)  11632
	!		if(p_isdomain)then
	!is(p_p8)write(6,*)'generate domains ,p_ndom,p_ndomv,p_nunits ',p_ndom,p_ndomv,p_nunits
	!write(6,*)'<55p_ndomv ',p_ndomv
	! store domain information in bit form into domainbits
	! one element in domainbits can store 32 bits
	p_ivdomainbits=j_deflist(j_ivout,'%ivdomainbits',list0=p_ndomv*p_nunits,ilist=.true.)
	p_domainbits=>j_o(p_ivdomainbits)%i2(1:p_ndomv*p_nunits)
 
	p_ivdomainunits=j_deflist(j_ivout,'%domainunits',list0=p_ndom,ilist=.true.)
	p_domainunits=>j_o(p_ivdomainunits)%i2(1:p_ndom)
	!	write(6,*)'p_ndom,p_ndomv,',p_ndom,p_ndomv
	!	write(6,*)'domainunits',p_ndomv,p_nunits,p_ndomv*p_nunits
	! if(allocated(p_domainbits))deallocate(p_domainbits)
	! allocate(p_domainbits(1:p_ndomv*p_nunits));p_domainbits=0
 
	! ! domainunits tells the number of units in each domain
	! if(allocated(p_domainunits))deallocate(p_domainunits)
	! allocate(p_domainunits(1:p_ndom));p_domainunits=0
	!	p_domainunits=0
	if(p_domvars(1).eq.j_ivall)p_domainunits(1)=p_nunits
	!			p_domainbits=0
	!	endif !if(p_ndom.gt.0)   5239
	p_nrowx0=0
	! write(6,*)'p_ix ',p_ix
	do j=0,p_nrow
		if(p_domvars(p_rowdomnum(j)).eq.j_ivall.and.p_ix(j).ne.0)then
			p_nrowx0=p_nrowx0+1
			p_rowcurx(p_nrowx0)=j
		endif !if(p_domvars(p_rowdomnum(j)).eq.j_ivall.and.p_ix(j).ne.0)   5436
	enddo !j=0,p_nrow   5435
	!	 p_row0=1
	!	if(p_rowcurx(1).eq.0)p_row0=2
	!updated for domainprob
	! if(p_rowcurx(1).eq.0) p_row0=2  !there is x-variable in objective thus constraits mus start
	! ! from rowcurx(2)
 
	! if(p_ido1.eq.2)then
 
	! p_nrowx0=p_nixcu(1)
	! ! p_rowcurx(1:p_nrowx0)=p_ixcurow(1:p_nrowx0)
	! ! !is(p_p8)write(6,*)'p_nrowx0 ',p_nrowx0,' *',p_ixcurow(1:p_nrowx0)
	! ! !is(p_p8)write(6,*)'p_nixcu ',p_nixcu
	! else
	! p_nrowx0=0
	! endif !if(p_ido1.eq.2)   5041
	!write(6,*)'p_nrowx0 ',p_nrowx0,p_rowcurx(1:p_nrowx0)
 
 
end subroutine initdomain

subroutine initopt()
	use jmod, only: p_ifail
	use jmod, only: j_err
	use jmod, only: p_lr0
	use jmod, only: p_nrow
	use jmod, only: p_lz0
	use jmod, only: p_lx0
	use jmod, only: p_unit
	use jmod, only: p_lunit0
	use jmod, only: j_o
	use jmod, only: p_ivrow
	use jmod, only: p_ivproblem
	use jmod, only: p_maxo
	use jmod, only: j_1
	use jmod, only: p_coefmax
	use jmod, only: p_nonlin
	use jmod, only: p_ivdomvars
	use fletdmod !nopre!   !file jlpd.f90
	use fletdmod2 !nopre!
	!	use jlpdmod !nopre!
 
	use fletcherdmod  !nopre!
	use lastmod !nopre!
	integer ::nup
	logical indomain  !,indomain2
	common/noutc/nout
	common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj  !Fletcher parameters
	!dense common/factorc/m0,m1,mm0,mm,mp,mq
	!sparse common/factorc/m1,m2,mp,mq,lastr,p_irow
 
	!this means that when in dense mpjj and mqjj are changed
	! then in sparse mpjj is the same as
 
	common/ipivotc/ipivot99  !used only in fltcherd.for to print pivot at malfunctio
	common/refactorc/nup,nfreq
 
	write(6,*)'*initopt'  !,pmxd,p_nz',p_nz
	!is(p_p)write(p_n16,*)'initopt'  ,p_nrow2z!
 
 
 
	! if(allocated(p_ls))deallocate(p_ls)
	! allocate(p_ls(1:p_nrow2z))   ! ls= list of columns in A which are in the basis
	! i.e. ls contains columns in ld-list plus columns of z-part of the A matrix
	!!is(p_p) write(p_n16,*)'lsmax****',p_nrow2z,p_nrow2z
	! A matrix
 
	!*objr0 the 'official' objective row
	!*objr2 the working obective row when the problem is infeasible
	!	p_objr0=p_zero;p_objr2=p_zero
 
	! z variables in the problem *******************
 
	!	!is(p_p8)write(6,*)'startup,nrow,p_ncol',p_nrow,p_ncol,' p_ifail :',p_ifail
	if(p_ifail.gt.0)then
		write(6,*)'**at startup p_ifail=',p_ifail, 'tell J. Lappi'
		j_err=.true.;return
 
	endif !if(p_ifail.gt.0)   5499
 
	ll0=ll1-1
 
	p_lr0=p_nrow		! all residuals are in the basis
 
 
	p_lz0=0			! no z-variable is in the basis
 
	p_lx0=0	! number of d -variables in the basis
 
 
	p_unit=0  !!!!!current unit
	p_lunit0=0 ! last unit with improvement
	p_ivrow=j_o(p_ivproblem)%i(4)  !text for rows
 
	iobjtype=j_o(p_ivproblem)%i(5)
	!	p_isobjective=iobjtype.ne.0
	!	if(p_isobjective)then
	if(iobjtype.gt.0)then
		p_maxo=.true.
		p_coefmax=j_1
	else
		if(p_nonlin)then
			write(6,*)'nonlinear objective must be maximized'
			j_err=.true.;	return
		endif !if(p_nonlin)   5526
		p_maxo=.false.
		p_coefmax=-j_1
 
	endif !if(iobjtype.gt.0)   5522
 
 
	!is(p_p8)write(6,*)'p_ndoms,p_nrowtot,p_nterm,p_ndom',p_ndoms,p_nrowtot,p_nterm,p_ndom
 
	p_ivdomvars=j_o(p_ivproblem)%i(10)
 
 
	!write(6,*)'end initopt'
 
 
end subroutine initopt

subroutine initzjlpz()  !jlpz(
	use jmod, only: p_isn16
	use jmod, only: p_nrow
	use jmod, only: p_lavec
	use jmod, only: p_zmatrix
	use jmod, only: p_maxo
	use jmod, only: p_objrz
	use jmod, only: p_nz
	use jmod, only: j_o
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_ivzmatrix
	use jmod, only: j_defmatrix
	use jmod, only: j_ivout
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: j_deflist
	use jmod, only: p_ivzvars
	use jmod, only: p_zvars
	use jmod, only: p_vars
	use jmod, only: p_nvar
	use jmod, only: p_nterm
	use jmod, only: p_nxval
	use jmod, only: p_nterminrow
	use jmod, only: p_coef
	use jmod, only: j_inlist
	use jmod, only: p_varofterm
	use jmod, only: p_n16
	use jmod, only: p_ivlower
	use jmod, only: p_lowerJ
	use jmod, only: p_lower
	use fletdmod !nopre!   !file jlpd.f90
	use fletdmod2 !nopre!
	!	use jlpdmod !nopre!
 
	use fletcherdmod  !nopre!
	use lastmod !nopre!
	integer ::nup
	logical indomain  !,indomain2
	common/noutc/nout
	common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj  !Fletcher parameters
	!dense common/factorc/m0,m1,mm0,mm,mp,mq
	!sparse common/factorc/m1,m2,mp,mq,lastr,p_irow
 
	!this means that when in dense mpjj and mqjj are changed
	! then in sparse mpjj is the same as
 
	common/ipivotc/ipivot99  !used only in fltcherd.for to print pivot at malfunctio
	common/refactorc/nup,nfreq
 
	write(6,*)'*initzjlpz'  !,p_nrow,p_nz
	if(p_isn16)write(16,*)'*initzjlpz'  !,p_nrow,p_nz
	! other zmatrix things are done in zmatrix
	! mitkä olivat xvar-muuttujat
	la=p_nrow ! number of rows in Fletcher
	p_lavec(1)=la
 
 
	if(p_zmatrix)then
		!		write(6,*)'dghgfgfgdh'
 
		if(.not.p_maxo)then
			!	p_objrz(1:p_nz)=j_o(p_ivzobj)%d(1:p_nz) was done earlier
			!	else
			p_objrz(1:p_nz)=-p_objrz(1:p_nz)
		endif !if(.not.p_maxo)   5577
		iel=0
		do i=1,p_nrow
			do j=1,p_nz
				iel=iel+1
				p_a(p_abas(j)+i)=j_o(p_ivzmatrix)%d(iel) !p_a(i,j)=j_o(p_ivzmatrix)%d(iel)
			enddo !j=1,p_nz   5584
		enddo !i=1,p_nrow   5583
		!		write(6,*)'hdhdh'
 
	else
		if(allocated(p_objrz))deallocate(p_objrz);allocate(p_objrz(1:p_nz))
		!	ivmatrix=j_defmatrix(j_ivout,'%zmatrix',int8(p_nrow),int8(p_nz),j_matreg)
		ivobjrow=j_defmatrix(j_ivout,'%objrow',j_18,int8(p_nz),j_matreg)
		p_ivzvars=j_deflist(j_ivout,'%zvars',list0=p_nz)
		p_zvars=>j_o(p_ivzvars)%i2(1:p_nz)
		p_zvars=p_vars(1:p_nvar)  !jlpz(
		!	p_nz=p_nz
		! p_nterm= number of coefficients in the
		nzval=p_nterm-p_nxval   !muuta kun fact
		!		p_idomba=0  ! basis for values
		!		p_irow0=0
		!		p_irow=0
 
		ival=0
 
		do ii=1,p_nterminrow(0)
			ival=ival+1
			! if(p_ispullout)then
			! iz=j_inlist(p_varofterm(ival),p_nz,p_zvars)
			! j_o(ivobj)%d(iz)=p_coef(ival)
			! endif !if(p_ispullout)   6785
			!if(.not.p_istermx(ival))then
			!iz=j_inlist(p_varofterm(ival),p_nz,p_zvars)
			j_o(ivobjrow)%d(ii)=p_coef(ival)
			if(p_maxo)then  ! objective maximized
				p_objrz(ii)=p_coef(ival) !object row
			else !if(j_maxo)then
				p_objrz(ii)=-p_coef(ival) ! -objective maximized
			endif !if(p_maxo)   5616
			!endif !if(.not.p_istermx(ival))   5316
		enddo !ii=1,p_nterminrow(0)   5607
 
		!	iba=0
 
		do irow=1,p_nrow
 
			!constraint row
			!		p_irow=p_irow+1
			do ii=1,p_nterminrow(irow)
				ival=ival+1   ! the number of variable with coefficient
 
				iz=j_inlist(p_varofterm(ival),p_nz,p_zvars)  !is it z-variable
 
 
				! put coefficients of z variables into A matrix
				p_a(p_abas(iz)+irow)=p_coef(ival)  !	p_a(irow,iz)=p_coef(ival)
				!		j_o(ivmatrix)%d((irow-1)*p_nz+iz)=p_coef(ival)  !	p_a(irow,iz)=p_coef(ival)
				! if(p_ispullout)then
				! write(6,*)'p_varofterm(ival)',p_varofterm(ival),iz,iba
				! j_o(ivconstr)%d(iba+iz)=p_coef(ival)
				! endif !if(p_ispullout)   6812
 
			enddo !ii=1,p_nterminrow(irow)   5630
			!	iba=iba+p_nz
		enddo !irow=1,p_nrow   5626
 
	endif !if(p_zmatrix)   5574
 
	if(p_isn16)write(p_n16,'(a,(15f7.3))')'objrz tasa',p_objrz
	p_ivlower=j_deflist(j_ivout,'%lower',list0=p_nrow,ilist=.true.)
	p_lowerJ=>j_o(p_ivlower)%i2(1:p_nrow)
	p_lowerJ=p_lower
 
 
end subroutine initzjlpz


subroutine initz0()
	use jmod, only: p_isn16
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivlz
	use jmod, only: p_nz
	use jmod, only: j_o
	use jmod, only: p_lz
	use jmod, only: p_ivlzi
	use jmod, only: p_lzi
	use jmod, only: p_lz0
	use jmod, only: j_defmatrix
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: p_ivredcost
	use jmod, only: p_redcost
	write(6,*)'*initz0'
	if(p_isn16)write(16,*)'*initz0'
	p_ivlz=j_deflist(j_ivout,'%lz',list0=p_nz,ilist=.true.)
	p_lz=>j_o(p_ivlz)%i2(1:p_nz)
	p_ivlzi=j_deflist(j_ivout,'%lzi',list0=p_nz,ilist=.true.)
	p_lzi=>j_o(p_ivlzi)%i2(1:p_nz)
	do i=1,p_nz
		p_lz(i)=i
	enddo !i=1,p_nz   5666
	p_lz0=0
	p_lzi=p_lz
	!	write(6,*)'lztasa'
	p_ivredcost=j_defmatrix(j_ivout,'%redcost',int8(p_nz),j_18,j_matreg)
	p_redcost=>j_o(p_ivredcost)%d(1:p_nz)
 
 
	!		if(p_isz)then  !p_nz=number of z-variables
 
end subroutine



subroutine fromli(iel,list,listi,le)
	integer list(*),listi(*)
	! 3 2 1  list
	! 3 2 1 ilist
	loc=listi(iel) !to loc last element  in loc iel
	iel2=list(le) !what is in last
	list(loc)=iel2  !last to loc
	list(le)=iel  !loc content to last
	listi(iel)=le
	listi(iel2)=loc
	le=le-1
end subroutine

subroutine fromlbb(iel)
	use jmod, only: p_lbb
	use jmod, only: p_lbb0
	integer loc(2)
	loc=findloc(p_lbb(1:p_lbb0),iel,dim=1) !to loc last element  in loc iel
	p_lbb(loc(1))=p_lbb(p_lbb0)
	p_lbb(p_lbb0)=iel!loc content to last
	p_lbb0=p_lbb0-1
end subroutine

subroutine tolbb(iel)
	use jmod, only: p_lbb0
	use jmod, only: p_lbb
	integer loc(2)
	p_lbb0=p_lbb0+1
	loc=findloc(p_lbb,iel,dim=1)
	iel0=p_lbb(p_lbb0)
	p_lbb(p_lbb0)=iel
	p_lbb(loc(1))=iel0
end subroutine



subroutine toli(iel,list,listi,le)
	integer list(*),listi(*)
	! 3 2 1  list
	! 3 2 1 ilist
	le1=le+1
	loc=listi(iel) ! to loc last element  in loc iel
	iel2=list(le1) !what is in last+1
	list(le1)=iel  !last to loc
	list(loc)=iel2  !loc content to last
	listi(iel)=le1
	listi(iel2)=loc
	le=le1
end subroutine




! subroutine tolx(iel)
! !loc0=p_lxi(iel)
! lx01=p_lx0+1
! k0=0
! do loc=1,p_lx0  !after which to put
! if(p_lunit(iel).ge.k0.and.p_lunit(iel).le.p_lunit(p_lx(loc)))exit
! k0=p_lunit(p_lx(loc))
! enddo !loc=1,p_lx0   8960

! !put to  element loc
! iel2=p_lx(lx01)
! do j=lx01,loc+1,-1  !shift right
! p_lx(j)=p_lx(j-1)
! p_lxi(p_lx(j))=j
! enddo !j=lx01,loc+1,-1   8967

! p_lx(loc)=iel
! p_lxi(iel)=loc
! p_lx(lx01)=iel2
! p_lxi(iel2)=lx01
! p_lx0=lx01

! end subroutine
subroutine startfast()
	use jmod, only: p_faststart
	use jmod, only: p_unit
	use jmod, only: p_fastnow
	use jmod, only: p_fastusedsame
	use jmod, only: p_fastmake
	use jmod, only: p_activeunit
	use jmod, only: p_isfastp
	use jmod, only: j_codevalue
	use jmod, only: p_fastpros
	use jmod, only: p_iob
	use jmod, only: p_fastplink
	use jmod, only: j_v
	use jmod, only: j_ivfastp
	use jmod, only: p_fastpros2
	use jmod, only: p_nimp
	p_faststart=p_unit
	p_fastnow=.false.
	p_fastusedsame=0
	p_fastmake=.true.
	p_activeunit=.true.
	if(p_isfastp)then
 
		p_fastpros=j_codevalue(p_iob,p_fastplink)
		j_v(j_ivfastp)=p_fastpros
		p_fastpros2=p_fastpros/100.d0
		!	write(6,*)'fastprosaft ',p_fastpros
 
	endif !if(p_isfastp)   5760
	p_nimp=0  !fastmake alkaa
	!	write(21,*)'start kierunitpivot',p_kier,p_unit,p_pivot
 
end subroutine startfast

! subroutine closefastmake()
! p_fastnow=.true.
! p_fastemake=.false.






subroutine getvc(objr)
	use jmod, only: p_ncoltot
	use jmod, only: p_nrow
	use jmod, only: p_a
	use jmod, only: p_lavec
	use jmod, only: p_vc
	use jmod, only: p_apu
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use fletdmod
	use fletdmod2
	use fletcherdmod
	double precision objr(p_ncoltot)
 
	call tfbsub(p_nrow,p_a,p_lavec,0, &
		objr,p_vc,wslu1,lwsll1,p_apu,.false.)
	if(p_isn16)write(p_n16,'(a,(15f7.2))')'vc in getvc',p_vc
 
	! if(p_feasible)then
	! !	write(21,*)p_vc,p_kier,p_unit,p_pivot,p_objf
	! if(p_fast)then
	! if(maxval(abs(p_vc-p_vcold)).gt.0.1d0)then
 
	! !			write(21,*)'max ',maxval(abs(p_vc-p_vcold)),'kier,unit',p_kier,p_unit
	! call startfast()
	! endif !if(maxval(abs(p_vc-p_vcold)).gt.0.1d0)   5787
	! p_vcold=p_vc
	! endif !if(p_fast)   5786
	! endif !if(p_feasible)   5784
	!	call rint('aftgetvc',p_n16)
 
	!	p_priced=.true.
	! subroutine tfbsub(n,a,la,p,b,x,aa,ll,ep,save)
	! implicit REAL*8 (a-h,r-z), integer (i-q)
	! logical save
	! dimension a(*),la(*),b(*),x(*),aa(*),ll(*)
 
	! c  solves a system  Bt.x=b
 
	! c  Parameter list
	! c  **************
	! c   n   number of variables (as for bqpd)
	! c   a,la   specification of QP problem data (as for bqpd)
	! c   p    an integer which, if in the range 1:n+m, specifies that the rhs vector
	! c        b is a unit vector appropriate to the position of p in the current
	! c        ordering. In this case b is not referenced by tfbsub.
	! c   b(n+m)  If p=0, this must be set to the r.h.s. vector b. Only the components
	! c        of b need be set, according to the index number of each component (in
	! c        the range 1:n for a simple bound and n+1:n+m for a general constraint)
	! c   x(n)  contains the solution x (in natural ordering)
	! c   aa(*)  real storage used by the basis matrix code (supply the vector
	! c       ws(lu1) with ws as in the call of bqpd and lu1 as in common/bqpdc/...)
	! c   ll(*)  integer storage used by the basis matrix code (supply the vector
	! c       lws(ll1) with lws as in the call of bqpd and ll1 as in common/bqpdc/...)
	! c   ep  if p.ne.0 and save is true, ep contains the l_2 length of x on exit
	! c   save  indicates if tfbsub is to save its copy of the solution for possible
	! c       future use. We suggest that the user only sets save = .false.
end subroutine

subroutine leftcol(icol,rx)
	use jmod, only: p_nrow
	use jmod, only: p_a
	use jmod, only: p_lavec
	use jmod, only: p_ls
	use fletdmod
	use fletdmod2
	use fletcherdmod
	integer ::icol
	double precision temp(1)
	double precision,dimension(1:p_nrow):: rx(p_nrow)
 
	call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,icol,temp,rx,& !!!!
		p_ls,wslu1,lwsll1,.false.)   !!!!!
 
end subroutine

subroutine leftvec(rhs,rx)  !output p_x
	use jmod, only: p_nrow
	use jmod, only: p_a
	use jmod, only: p_lavec
	use jmod, only: p_ls
	use fletdmod
	use fletdmod2
	use fletcherdmod
	double precision, dimension(1:p_nrow)::rhs
	double precision,dimension(1:p_nrow):: rx
 
	call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,0,rhs,rx,& !!!!
		p_ls,wslu1,lwsll1,.false.)   !!!!!
 
 
end subroutine

! subroutine clearnup()
! common/refactorc/nup,nfreq
! nup=0
! end subroutine

subroutine remove(id)
	use jmod, only: p_next
	use jmod, only: p_prev
	use jmod, only: p_nextlkm
	!  1 2 3  remove 2   => 1 3
	!write(16,*)'*next remove',' icol',id,'next',p_next,'ipre',p_prev
	p_next(p_prev(id))=p_next(id)   ! next(1)=3
	p_prev(p_next(id))=p_prev(id)  ! prev(3)=1
	p_next(id)=0
	p_prev(id)=0
	p_nextlkm=p_nextlkm-1
	!	write(16,*)'*next,aft',p_next,'lkm',p_nextlkm
	!	call loopnext()
end subroutine



subroutine putafter(idnew,id)
	use jmod, only: p_next
	use jmod, only: p_prev
	use jmod, only: p_nextlkm
	!write(16,*)'*next putafter',' idnew id',idnew,id,'next',p_next,'ipre',p_prev
	! 1 2 3  put 4 after 2   => 1 2 4 3
	! if(id.gt.0)then
	! if(inloop(id).eq.0)then
	! write(16,*)'inloop0'
	! write(16,*)'next',p_next
	! write(6,*)'inloop0'
	! call printd()
	! read(5,*)jdjjdjd
	! endif !if(inloop(id).eq.0)   5777
 
	! endif !if(id.gt.0)   5776
	! if(inloop(idnew).gt.0)then
	! write(16,*)'inloop0puididi'
	! write(6,*)'inloop0putiisss'
	! read(5,*)jdjjdjd
	! endif !if(inloop(idnew).gt.0)   5786
 
	p_next(idnew)=p_next(id) !next(4)=3
	p_prev( p_next(id)) = idnew     ! prev(3)=
	p_next(id)=idnew   !next(2)=4
	p_prev(idnew)=id !prev(4)=2
 
	p_nextlkm=p_nextlkm+1
	!write(16,*)'*next ,aft',p_next,'lkm',p_nextlkm
	!call loopnext()
end subroutine

function inloop(id)
	use jmod, only: p_next
	integer,dimension(10)::loop
	lkm=0
	nex=p_next(0)
	inloop=0
	do while(nex.ne.0)
		lkm=lkm+1
		if(id.eq.nex)then
			inloop=lkm
			return
		endif !if(id.eq.nex)   5914
		nex=p_next(nex)
	enddo !while(nex.ne.0)   5912
	return
end function

subroutine printd()
	use jmod, only: j_yes
	use jmod, only: p_next
	use jmod, only: p_lunit
	use jmod, only: p_keys
	use jmod, only: p_isch
	use jmod, only: p_x
	use jmod, only: p_nrowz
	integer,dimension(100)::loop
	lkm=0
	j_yes=.false.
	nex=p_next(0)
	do while(nex.ne.0)
		lkm=lkm+1
		loop(lkm)=nex
		! if(p_keys(p_lunit(nex)).eq.p_isch(nex))then
		! write(6,*)'**kopt',nex,p_aopt(p_abas1(nex)+1:p_abas1(nex)+p_nrowtot)
		! write(6,*)'**key',p_akey(p_abas1(nex)+1:p_abas1(nex)+p_nrowtot)
		! j_yes=.true.
		! endif !if(p_keys(p_lunit(nex)).eq.p_isch(nex))   5593
		nex=p_next(nex)
 
	enddo !while(nex.ne.0)   5928
	write(16,*)'cols',loop(1:lkm)
	write(16,*)'lunits',p_lunit(loop(1:lkm))
	write(16,*)'keys',p_keys(p_lunit(loop(1:lkm)))
 
	write(16,*)'isch',p_isch(loop(1:lkm))
	write(16,*)'x',p_x(p_nrowz+loop(1:lkm))
	! if(j_yes)then
	! write(6,*)'isch=key'
	! read(5,*)udfui
 
 
	! endif !if(j_yes)   5607
 
 
 
 
end subroutine


subroutine loopnext()
	use jmod, only: p_next
	use jmod, only: p_nextlkm
	use jmod, only: p_prev
	integer,dimension(100)::loop
	lkm=0
	nex=p_next(0)
	do while(nex.ne.0)
		lkm=lkm+1
		if(lkm.gt.10)then
			write(16,*)'loopper',loop
			write(6,*)'loopper',loop
			read(5,*)fdjfdj
 
		endif !if(lkm.gt.10)   5964
		loop(lkm)=nex
		nex=p_next(nex)
	enddo !while(nex.ne.0)   5962
	write(16,*)'lkm ',lkm,'next9',p_nextlkm,'loop',loop(1:lkm)
	do i=lkm,2,-1
		if(loop(i-1).ne.p_prev(loop(i)))then
			write(16,*)'pexet',p_next
			write(16,*)'pipre',p_prev
			write(6,*)'lkm ',lkm,'next9',p_nextlkm,'loop',loop(1:lkm)
			write(6,*)'pexet',p_next
			write(6,*)'pipre',p_prev
			read(5,*)djdjj
		endif !if(loop(i-1).ne.p_prev(loop(i)))   5975
 
	enddo !i=lkm,2,-1   5974
	if(lkm.ne.p_nextlkm)then
		write(16,*)'***next',p_next
		write(16,*)'***iprev',p_prev
		write(6,*)'lkm ',lkm,'next9',p_nextlkm,'loop',loop(1:lkm)
		read(5,*)djjdj
	endif !if(lkm.ne.p_nextlkm)   5985
 
end subroutine

! p_next(p_prev(0))=p_id
! p_next(p_id)=0
! p_prev(p_id)=p_prev(0)
! p_prev(0)=p_id
! if(p_p)write(6,*)'next p_id iopt iunit',p_id,p_iopt,p_unit

subroutine putbefore(idnew,id)
	use jmod, only: p_next
	use jmod, only: p_prev
	use jmod, only: p_nextlkm
	!	! 1 2 3  put 4 before 2   => 1 4 2 3
	!	write(16,*)'*next putbefore',' idnew id',idnew,id,'next',p_next,'ipre',p_prev
	if(id.gt.0)then
		if(inloop(id).eq.0)then
			write(16,*)'inloop0put'
			write(6,*)'inloop0put'
			read(5,*)jdjjdjd
		endif !if(inloop(id).eq.0)   6004
 
	endif !if(id.gt.0)   6003
	if(inloop(idnew).gt.0)then
		write(16,*)'inloop0puididi'
		write(6,*)'inloop0putiisss'
		read(5,*)jdjjdjd
 
 
	endif !if(inloop(idnew).gt.0)   6011
	p_next(p_prev(id))=idnew  ! next(1)=4
	p_prev(idnew)=p_prev(id) ! prev(4)= 1
	p_prev(id)=idnew  ! prev(2)=4
	p_next(idnew)=id  !next(4)=2
 
	p_nextlkm=p_nextlkm+1
	!	write(16,*)'*next putbeforeaft',p_next,'ipre',p_prev
 
	!	call loopnext()
end subroutine


subroutine jlpstart()
 
 
 
end subroutine

! subroutine changekey(icol,newkey,newcol)
! iunit=p_colkey(icol)
! ibas=p_
! do i=1,p_nrow
! a=x-key
! x=a+key
! a2=x-newkey=a+key-newkey
! if(p_ix(i).ne.0)then
! p_a(p_abas(p_ia)+i)=p_a(i+p_abas(nex+p_nz))-p_a(i+p_abas(p_leavek+p_nz))

! endif

! enddo********************ij ',i




! end subroutine

subroutine pivotnow(newc)  !p_oldc is leaving
	use jmod, only: p_nrowz
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: j_err
	use jmod, only: p_pivot
	use jmod, only: p_oldc
	use jmod, only: p_objf
	use jmod, only: p_newc
	use jmod, only: p_nrow
	use jmod, only: p_lr0
	use jmod, only: p_nrow2z
	use jmod, only: p_a
	use jmod, only: p_lavec
	use jmod, only: p_ifail
	use jmod, only: p_info
	use jmod, only: p_idebug1
	use jmod, only: p_abas
	use jmod, only: p_aopt
	use jmod, only: p_abas1
	use jmod, only: p_nrowtot
	use jmod, only: p_akey
	use jmod, only: p_nz
	use jmod, only: p_ls
	use jmod, only: p_lsi
	use jmod, only: p_lr
	use jmod, only: p_lri
	use jmod, only: p_isres
	use jmod, only: p_lz
	use jmod, only: p_lzi
	use jmod, only: p_lz0
	use jmod, only: p_leaved
	use jmod, only: p_lx
	use jmod, only: p_lxi
	use jmod, only: p_lx0
	use jmod, only: p_next
	use jmod, only: p_lunit
	use jmod, only: p_feasible
	use jmod, only: p_objr
	use jmod, only: p_tmax
	use jmod, only: p_muutosb
	use fletdmod
	use fletdmod2
	use fletcherdmod
	common/refactorc/nup,nfreq
	!	integer p_oldc
	!	logical computex,computevc
 
	if(newc.gt.p_nrowz)mqjj=-1  ! mqjj=-1   !new not  something known
	!	endif !if(sparse)  21409
	! if(p_p)write(p_n16,*)'***pivot***',p_pivot+1
	! if(p_p)then
	! write(p_n16,*)'p_leave',p_leave,'pivot cols',p_ls(p_leave),newc,'mp,mq',mpjj,mqjj
	! !			if(sparse)write(p_n16,*)'p_leave',p_leave,'pivot cols',p_ls(p_leave),newc,'mp,mq',mpjjsp,mqjjsp
	!endif !if(p_p)  12853
	! pivot uses
	! p_leave if number in ls
	! newc is absolute column number
	!	p_oldc=p_ls(p_leave)
	! if(.not.any(p_ls(1:p_nrow).eq.p_oldc))then
	! write(6,*)'icolold',p_oldc,'notinher',p_ls
	! endif !if(.not.any(p_ls(1:p_nrow).eq.p_oldc))   9044
	if(p_isn16)write(p_n16,*)' '
 
	if(p_isn16)	write(p_n16,*)j_err,'*********pivot ',p_pivot,'old ',p_oldc,' newc ',newc,p_objf
 
	p_newc=newc
	! if(nup.ge.nfreq-2)then
	! if((newc.le.p_nrow.and.p_lr0.eq.p_nrow-1).or.p_lr0.eq.p_nrow)then
	! nup=0
	! write(6,*)'clearnup',p_pivot
	! else
	! p_refac=p_refac+1
	! if(p_isn16)write(p_n16,*)'refactoring ',p_refac,nup,nfreq
	! endif !if((newc.le.p_nrow.and.p_lr0.eq.p_nrow-1).or.p_lr0.eq.p_nr   6021
 
	! !	if(p_p)write(p_n16,*)'*refact'
	! endif !if(nup.ge.nfreq-2)   6020
	ido=p_oldc-p_nrow
	!	if(ido.gt.0)write(6,*)'  old',p_oldc,p_a(p_abas(ido)+1:p_abas(ido)+p_nrow)
	idn=newc-p_nrow
	!	write(6,*)'newc,idn',newc,idn
	!	if(idn.gt.0)write(6,*)'   new idn',id,'newc',newc,p_a(p_abas(idn)+1:p_abas(idn)+p_nrow)
	! if(newc.eq.0)then  !get from
	! p_newa=p_nz+p_lx(p_lx0+1)
	! p_a(p_abas(p_newa)+1:p_abas(p_newa)+p_nrow)=p_anew0(1:p_nrow)
	! newc=p_nrowz+newa
	! endif !if(newc.eq.0)   8742
	if(j_err)write(6,*)'error before pivot'
	!	write(6,*)'pnenewc,p_leave,p_leavec,p_oldc,newc',newc,p_oldc
	!	if(.not.any(p_ls(1:p_nrow).eq.p_oldc))then
	!		write(6,*)'icolold',p_oldc,'notin',p_ls
	!	endif !if(.not.any(p_ls(1:p_nrow).eq.p_oldc))   8854
	!if(newc.gt.p_nrowz)write(16,*)'acolnew befpivot',p_a(p_abas(newc-p_nrowz)+1:p_abas(newc-p_nrow)+p_nrow)
	if(p_isn16)write(p_n16,*)'p_pivot,nup,nfreq,p_lr0',p_pivot,nup,nfreq,p_lr0,'newc old',newc,p_oldc
	call pivot(p_oldc,newc,p_nrow,p_nrow2z,p_a,p_lavec,e,wslu1,lwsll1,p_ifail,p_info)
	p_pivot=p_pivot+1
	if(p_pivot.eq.p_idebug1)then
		p_isn16=.true.
		write(6,*)'setting debug on at pivot ',p_pivot
	endif !if(p_pivot.eq.p_idebug1)   6112
	!	if(p_pivot.gt.325400)p_isn16=.true.
	! if(p_pivot.gt.325400)then
	! write(21,*)'old,new,pivot',p_oldc,newc,p_pivot,'objr',p_objr(p_oldc),p_objr(newc)
	! write(21,*)'kier,unit,',p_kier,p_unit
	! lxx=p_oldc-p_nrowz
	! ia=p_oldc-p_nrow
	! !!!write(16,*)'lx(),lunit,key,isch',p_lx(ij),p_lunit(p_lx(ij)),p_keys(p_lunit(p_lx(ij))),p_isch(p_lx(ij))
	! if(p_oldc.gt.p_nrow)write(21,'(20f11.1)')p_a(p_abas(ia)+1:p_abas(ia)+p_nrow)
	! if(lxx.gt.0)then
	! write(21,'(20f11.1)')p_aopt(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
	! write(21,'(20f11.1)')p_akey(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
	! write(21,'(20f11.1)')p_a(p_abas(lxx+p_nz)+1:p_abas(lxx+p_nz)+p_nrow)-&
	! p_aopt(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)+p_akey(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
	! endif !if(lxx.gt.0)   6115
 
	! lxx=newc-p_nrowz
	! ia=newc-p_nrow
	! if(ia.gt.0)write(21,'(20f11.1)')p_a(p_abas(ia)+1:p_abas(ia)+p_nrow)
	! if(lxx.gt.0)then
	! write(21,'(20f11.1)')p_aopt(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
	! write(21,'(20f11.1)')p_akey(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
	! write(21,'(20f11.1)')p_a(p_abas(lxx+p_nz)+1:p_abas(lxx+p_nz)+p_nrow)-&
	! p_aopt(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)+p_akey(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
	! endif !if(lxx.gt.0)   6125
	! !		call rint('pivoterror',6)
 
 
 
 
	! endif !if(p_pivot.gt.325400)   6108
 
	if(j_err)then
		if(p_isn16)write(p_n16,*)'pivoterror oldcol',p_oldc,' pivot',p_pivot
		lxx=p_oldc-p_nrowz
		ia=p_oldc-p_nrow
		!!!write(16,*)'lx(),lunit,key,isch',p_lx(ij),p_lunit(p_lx(ij)),p_keys(p_lunit(p_lx(ij))),p_isch(p_lx(ij))
		if(p_oldc.gt.p_nrow)write(16,'(20f11.1)')p_a(p_abas(ia)+1:p_abas(ia)+p_nrow)
		if(lxx.gt.0)Then
			write(6,'(20f11.1)')p_aopt(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
			write(6,'(20f11.1)')p_akey(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
			write(6,'(20f11.1)')p_a(p_abas(lxx+p_nz)+1:p_abas(lxx+p_nz)+p_nrow)-&
				p_aopt(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)+p_akey(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
		endif !if(lxx.gt.0)   6153
		write(6,*)'newc',newc,'pivoterror oldcol',p_oldc,' pivot',p_pivot
		lxx=newc-p_nrowz
		ia=newc-p_nrow
		if(ia.gt.0)write(6,'(20f11.1)')p_a(p_abas(ia)+1:p_abas(ia)+p_nrow)
		if(lxx.gt.0)then
			write(6,'(20f11.1)')p_aopt(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
			write(6,'(20f11.1)')p_akey(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
			write(6,'(20f11.1)')p_a(p_abas(lxx+p_nz)+1:p_abas(lxx+p_nz)+p_nrow)-&
				p_aopt(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)+p_akey(p_abas1(lxx)+2:p_abas1(lxx)+p_nrowtot)
		endif !if(lxx.gt.0)   6163
		call rint('pivoterror',6)
 
	endif !if(j_err)   6147
 
	call jlplex(p_ls,p_lsi(newc),p_lsi(p_oldc),p_lsi)
 
 
	if(p_oldc.le.p_nrow)then
		!	write(16,*)'   jcj',p_oldc,p_lr0
		!call jlplex(p_lr,p_oldc,p_lr0+1,p_lri)
		!	if(p_bbou(p_oldc))	call fromli(p_oldc,p_lr,p_lri,p_lr0)
		call fromli(p_oldc,p_lr,p_lri,p_lr0)
		p_isres(p_oldc)=.false.
 
		if(p_isn16)write(p_n16,*)'   lr aft pivot res leaves',p_lr(1:p_lr0)
	elseif(p_oldc.le.p_nrowz)then
 
		call fromli(p_oldc-p_nrow,p_lz,p_lzi,p_lz0)
		if(p_isn16)write(p_n16,*)'   lz aft zeleaves ',p_lz(1:p_lz0)
		!	call jlplex(p_lz,p_oldc-p_nrow,p_lz0+1,p_lzi)
	else
		p_leaved=p_oldc-p_nrowz
		!		p_xps(0:p_nrow)=p_xps(0:p_nrow)-p_akey(p_abas1(p_leaved)+1:p_abas1(p_leaved)+p_nrowtot)
		call remove(p_leaved)
		call fromli(p_leaved,p_lx,p_lxi,p_lx0)
		if(p_isn16)write(p_n16,*)'   lx aft sleaves ',p_lx(1:p_lx0)
		!	if(p_leaved.eq.p_linktofirst)p_linktofirst=0
		!		call jlplex(p_lx,p_leaved,p_lx0+1,p_lxi)
	endif !if(p_oldc.le.p_nrow)   6176
 
	if(newc.le.p_nrow)then
		call toli(newc,p_lr,p_lri,p_lr0)
		p_isres(newc)=.true.
		!		if(p_bbou(newc))
		if(p_isn16)write(p_n16,*)'   res enters ,lr aft',p_lr(1:p_lr0)
	elseif(newc.le.p_nrowz)then
 
		call toli(newc-p_nrow,p_lz,p_lzi,p_lz0)
		write(p_n16,*)'zenterslz aft,','lz0',p_lz0,newc-p_nrow,'lz',p_lz,'lzi',p_lzi
	else
		id=newc-p_nrowz
		lcur=0
		do j=1,p_lx0
			lcurnext=p_next(lcur)
			if(p_lunit(id).le.p_lunit(lcurnext))exit
			lcur=lcurnext
		enddo !j=1,p_lx0   6211
		call putafter(id,lcur)
 
 
 
 
		call toli(id,p_lx,p_lxi,p_lx0)
		if(p_isn16)write(p_n16,*)'   senter lxafter',p_lx(1:p_lx0)
		!subroutine putafter(icolnew,icol)
		!	write(16,*)'nextherebef',p_next,'id',id,'p_xl',p_lx(1:p_lx0),'p_lx0',p_lx0,'p_linktofirst',p_linktofirst
 
 
 
		!	p_xps(0:p_nrow)=p_xps(0:p_nrow)+p_akey(p_abas1(id)+1:p_abas1(id)+p_nrowtot)
		if(p_feasible)p_objr(newc)=p_aopt(p_abas1(id)+1)-p_akey(p_abas1(id)+1)
		if(p_isn16.and.p_feasible)write(p_n16,*)'  after senter objr ',p_objr(newc)
		! do j=1,p_nrow
		! if(abs(p_a(p_abas(id)+j)-p_aopt(p_abas1(id)+j+1)+p_akey(p_abas1(id)+j+1)).gt.0.1)then
		! write(16,*)'pa',99.d0,p_a(p_abas(id)+1:p_abas(id)+p_nrow)
		! write(16,*)'popt',p_aopt(p_abas1(id)+1:p_abas1(id)+p_nrowtot)
		! write(16,*)'pkey',p_akey(p_abas1(id)+1:p_abas1(id)+p_nrowtot)
		! write(6,*)'asekas'
		! read(5,*)kdkdfk
		! endif !if(abs(p_a(p_abas(id)+j)-p_aopt(p_abas1(id)+j+1)+p_akey(p_   8870
 
 
		! enddo !j=1,p_nrow   8869
 
	endif !if(newc.le.p_nrow)   6199
 
	! p_pivotcase=5
	! j_o(p_ivpivotcases)%i2(5)=j_o(p_ivpivotcases)%i2(5)+1
	!linux
	!	if(p_p8)write(p_n16,*)'<75aftivot',wslu1
	if(p_ifail.ne.0)then
		write(6,*)'*fail in pivot: p_ifail,tmax,Pivots ',&
			p_ifail, p_tmax,p_pivot,'*,p_oldc,newc ',p_oldc,newc
		call getout()
		j_err=.true.;return
 
 
	endif !if(p_ifail.ne.0)   6249
	p_muutosb=p_muutosb+1
 
 
	! if(p_xpresent)then
	! call rint(16)
	! call testxps('pivotxps')
	! write(16,*)'pivotnow ends unit ',p_unit,'ienter',p_enter0,p_enter1,p_enter2,p_enter3,'tmax',p_tmax
	! endif !if(p_xpresent)   5852
 
end subroutine pivotnow

subroutine testl()
	use jmod, only: p_lr0
	use jmod, only: p_lz0
	use jmod, only: p_lx0
	use jmod, only: p_nrow
	use jmod, only: j_err
	use jmod, only: p_ls
	use jmod, only: p_lr
	use jmod, only: p_nz
	use jmod, only: p_lz
	use jmod, only: p_lx
	use jmod, only: p_nrowz
	use jmod, only: p_nrow2z
	use jmod, only: p_isz
	isum=p_lr0+p_lz0+p_lx0
	if(isum.ne.p_nrow)then
		write(6,*)'p_lr0,p_lz0,p_lx0,sum,p_nrow',p_lr0,p_lz0,p_lx0,isum,p_nrow
		j_err=.true.
	endif !if(isum.ne.p_nrow)   6270
loop: do i=1,p_lr0
		do j=1,p_nrow
			if(p_ls(j).eq.p_lr(i))cycle loop
		enddo !j=1,p_nrow   6275
		write(6,*)'p_lr(',i,')=',p_lr(i),' not in p_ls'
		write(6,*)'lr',p_lr
 
		j_err=.true.
	enddo loop !p: do i=1,p_lr0   6274
loopz: do i=1,p_lz0
		do j=p_nrow+1,p_nrow+p_nz
			if(p_ls(j).eq.p_lz(i))cycle loopz
		enddo !j=p_nrow+1,p_nrow+p_nz   6284
		write(6,*)'p_lz(',i,')=',p_lx(i),' not in p_ls'
		write(6,*)'lz',p_lz
		j_err=.true.
	enddo loopz !pz: do i=1,p_lz0   6283
loopx: do i=1,p_lx0
		do j=p_nrowz+1,p_nrow2z
			if(p_ls(j).eq.p_lx(i))cycle loopx
		enddo !j=p_nrowz+1,p_nrow2z   6292
		write(6,*)'p_lx(',i,')=',p_lx(i),' not in p_ls'
		write(6,*)'lx',p_lx
		j_err=.true.
	enddo loopx !px: do i=1,p_lx0   6291
	if(j_err)then
		write(6,*)'*****'
		write(6,*)'lr0',p_lr0,'lz0',p_lz0,'lx0',p_lx0
		write(6,*)'*************ls',p_ls
		write(6,*)'*******lr',p_lr
		if(p_isz)write(6,*)'*******lr',p_lz
		write(6,*)'*******lx',p_lx
	endif !if(j_err)   6299
 
end subroutine

! subroutine updatel()
! ! if(p_leavec.le.p_nrow)then
! ! ! resiadual leaves
! ! ! put leaving as first nonbasic

! ! write(16,*)'residual leAVES'
! ! call jlplex(p_lr,p_lr0,p_lri(p_leavec),p_lri)
! ! p_lr0=p_lr0-1
!
! !		p_objr,p_vc,wslu1,lwsll1,p_apu,.false.)
subroutine getout()
	use jmod, only: p_isn16
	use jmod, only: p_objf
	use jmod, only: p_nnf
	use jmod, only: p_coefmax
	use jmod, only: j_v
	use jmod, only: j_0
	use jmod, only: p_ivfeasible
	use jmod, only: j_1
	use jmod, only: p_feasible
	use jmod, only: p_ivoptimal
	use jmod, only: p_optimal
	use jmod, only: p_ivpivots
	use jmod, only: p_pivot
	use jmod, only: p_ivobj
	use jmod, only: p_xpresent
	use jmod, only: j_divobsup
	use jmod, only: p_unit
	use jmod, only: p_ivtmax
	use jmod, only: p_tmax
	use jmod, only: p_ivlx0
	use jmod, only: p_lx0
	use jmod, only: p_ivvaluek
	use jmod, only: p_valuek
	use jmod, only: p_ivvalueopt
	use jmod, only: p_valueopt
	use jmod, only: p_ivlr0
	use jmod, only: p_lr0
	use jmod, only: p_isz
	use jmod, only: p_ivlz0
	use jmod, only: p_lz0
	use jmod, only: p_ivrefac
	use jmod, only: p_refac
	use jmod, only: j_ivcpu
	use jmod, only: p_time00
	if(p_isn16)write(16,*)'getout obj ',p_objf,' p_nnf ',p_nnf, 'fin obj ',p_coefmax*p_objf
	j_v(p_ivfeasible)=j_0
	if(p_feasible)j_v(p_ivfeasible)=j_1
	j_v(p_ivoptimal)=j_0
	if(p_optimal)j_v(p_ivoptimal)=j_1
 
	j_v(p_ivpivots)=p_pivot
	j_v(p_ivobj)=p_coefmax*p_objf
	! if(p_isz)then
	! iel=0
	! do ir=1,p_nrow
	! do iz=1,p_nz
	! iel=iel+1
	! j_o(p_ivmatrix)%d(iba)=p_a(p_abas(iz)+ir)
	! enddo !iz=1,p_nz   5891
	! enddo !ir=1,p_nrow   5890
	! endif !if(p_isz)   5888
 
	if(p_xpresent)then
		j_v(j_divobsup)=p_unit
		j_v(p_ivtmax)=p_tmax
		j_v(p_ivlx0)=p_lx0
		j_v(p_ivvaluek)=p_valuek
		j_v(p_ivvalueopt)=p_valueopt
	endif !if(p_xpresent)   6339
	!j_v(p_ivcolold)=p_oldc
	!j_v(p_ivcolnew)=newc
	!j_v(p_ivpivotcase)=p_pivotcase
	j_v(p_ivlr0)=p_lr0
	if(p_isz)j_v(p_ivlz0)=p_lz0
 
 
	j_v(p_ivrefac)=p_refac
 
	call  cpu_time(time0)
	!p_time0-p_time00
	j_v(j_ivcpu)=time0-p_time00
 
end subroutine



subroutine repoepilog(nureport,isx)
	use jmod, only: j_v
	use jmod, only: p_ivunbounded
	use jmod, only: p_buf
	use jmod, only: p_pivot
	use jmod, only: p_refac
	use jmod, only: p_kier
	use jmod, only: p_feasible
	use jmod, only: p_coefmax
	use jmod, only: p_objf
	use jmod, only: p_ivoptimal
	use jmod, only: p_maxrounds
	logical isx
	write(nureport,*)('_',kk=1,79)
 
	if(j_v(p_ivunbounded)>0) then
		p_buf='Unbounded problem'
		write(nureport,'(a)')p_buf(1:79)
	endif !if(j_v(p_ivunbounded)>0)   6367
 
	write(nureport,"('Pivots: ',I10,' refactorizations ',i5,' rounds ',i5)")&
		p_pivot,max(0,p_refac-1),p_kier
 
	if(p_feasible) then
			p_buf='Value of the objective function:  '
	else !if(p_feasible) then
			p_buf='Value of the temporary objective: '
	endif !if(p_feasible)   6375
	write(p_buf(35:),*)p_coefmax*p_objf
	!	p_buf(35:)=j_chr10(p_coefmax*p_objf)
	write(nureport,'(a)')p_buf(1:79)
 
	if(p_feasible) then
		p_buf='Solution is feasible'
	else !if(p_feasible) then
		p_buf='Solution is infeasible'
	endif !if(p_feasible)   6384
	write(nureport,'(a)')p_buf(1:79)
	!write(6,*)'OBJECTIVE&&&&& ',p_objf,j_v(p_ivobj2)
	!	call j_getname(p_ivobj2)
	!write(6,*)'%%%',j_oname(1:j_loname)
	if(j_v(p_ivoptimal)>0) then
		if(p_kier.ge.p_maxrounds.and.isx)then
			p_buf='Solution may not be optimal, maximum number of iterations reached'
			! elseif(isslow)then !if(kier.ge.p_mxiter)then
			! j_buf='Solution is close to optimal (slow improvement)'
		else !if(kier.ge.p_mxiter)then
			p_buf='Solution is optimal'
		endif !if(p_kier.ge.p_maxrounds.and.isx)   6394
	else !if(j_v(p_ivoptimal)>0) then
		p_buf='Solution is not optimal'
	endif !if(j_v(p_ivoptimal)>0)   6393
	write(nureport,'(a)')p_buf(1:79)
	if(nureport.ne.6)write(6,*)'** report-> file remains open'
	return
 
 
end subroutine repoepilog

subroutine repoz(nureport,injlp)
	use jmod, only: j_v
	use jmod, only: p_ivobj
	use jmod, only: p_coefmax
	use jmod, only: p_objf
	use jmod, only: p_issolution
	use jmod, only: p_nrow
	use jmod, only: p_ncol
	use jmod, only: j_0
	use jmod, only: p_x
	use jmod, only: p_ls
	use jmod, only: p_redcost
	use jmod, only: p_lz0
	use jmod, only: p_nz
	use jmod, only: p_newa
	use jmod, only: p_lz
	use jmod, only: p_feasible
	use jmod, only: p_val
	use jmod, only: p_objr
	use jmod, only: p_vc
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_isn16
	use jmod, only: j_defmatrix
	use jmod, only: j_ivout
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: j_o
	use jmod, only: p_rhscur
	use jmod, only: p_zmatrix
	use jmod, only: j_getname
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: j_oname3
	use jmod, only: j_loname3
	use jmod, only: p_iprint
	use jmod, only: p_buf
	use jmod, only: j_vname
	use jmod, only: p_zvars
	use jmod, only: j_lename
	use jmod, only: p_dots
	use jmod, only: j_chr10
	logical :: injlp
	!write(6,*)'<67>p_nrow',p_nrow
	!if(p_maxo) then
	j_v(p_ivobj)=p_coefmax*p_objf
	! else !if(j_maxo) then
	! j_v(p_ivobj)=-p_objf
	! p_vc=-p_vc                        !;objf=-objf ei oltu
	! !	write(6,*)'objrbef',p_objr0
	! p_objr0=-p_objr0
	! endif !if(p_maxo)   9524
	if(.not.injlp)then
		write(nureport,*)' '
		! if(j_ivout.ne.j_ivresult) then
		! call j_getobject(j_ivout,'%obj',j_ipreal,p_ivobj2)
		! j_v(p_ivobj2)=j_v(p_ivobj)
		write(6,*)'OBJECTIVE ',j_v(p_ivobj)
		! endif !if(j_ivout.ne.j_ivresult)   9530
901	continue
		write(nureport,*)' '
		p_issolution=.true.
		!if(p_nrow.gt.0)then  !there are constraints
		do i=p_nrow+1,p_ncol+p_nrow
			p_x(p_ls(i))=j_0
			!	if(p_ls(i).eq.2)write(6,*)'<4774putzero'
		end do !i=p_nrow+1,p_ncol+p_nrow   6433
		!endif !if(p_nrow.gt.0)   9538
 
		!	p_vx=-p_vx
	endif !if(.not.injlp)   6422
 
877 continue !nrow=0
 
!	if(p_isz) then
		! ivredcost=j_defmatrix(j_ivout,'%redcost',j_18,int8(p_nz),j_matreg)
 
		! j_o(ivredcost)%d(1:p_nz)=p_redcost(1:p_nz) !done earlier
	p_redcost=j_0 !
 
	do i=p_lz0+1,p_nz
 
		p_newa=p_lz(i)
		! write(6,*)'i ',i,p_newa,' objr0 ',p_objr0, 'pnewa',p_newa
		! write(6,*)'vc',p_vc
 
		if(p_feasible)then
			p_val=p_objr(p_newa+p_nrow)  ! in objr all cols are counted
		else !if(j_feasible)then
			p_val=j_0
		endif !if(p_feasible)   6456
		!	write(6,*)'val ',p_val
 
		do  j=1,p_nrow
 
			p_val=p_val-p_vc(j)*p_a(j+p_abas(p_newa))  !p_a(j,p_newa)
			if(p_isn16)write(16,*)'redcost col row ',p_newa,j,p_vc(j),p_a(j+p_abas(p_newa)),p_val
		enddo ! j=1,p_nrow   6463
 
		p_redcost(p_newa)=abs(p_val)
		if(p_redcost(p_newa).lt.1.d-70)p_redcost(p_newa)=j_0
 
	enddo !i=p_lz0+1,p_nz   6450
	!	endif !if(p_isz)   6291
	!if(j_err)return
 
	!	if(j_ivout.ne.j_ivresult) then
	!	if (p_nz>0) then
	!write(6,*)'<p_nz',p_nz
	ivzval=j_defmatrix(j_ivout,'%zvalues',j_18,int8(p_nz),j_matreg)
	j_o(ivzval)%d(p_lz(1:p_lz0))=p_x(p_nrow+p_lz(1:p_lz0))
 
	if(.not.injlp)then
		! endif !if (p_nz>0)   7883
 
		!	if(p_nrow>0) then
		!write(6,*)'<nrow',j_nrow
		ivrows=j_defmatrix(j_ivout,'%rows',int8(p_nrow),j_18,j_matreg)
		j_o(ivrows)%d(1:p_nrow)=p_rhscur(1:p_nrow)-p_x(1:p_nrow)
 
		ivshp=j_defmatrix(j_ivout,'%shprice',int8(p_nrow),j_18,j_matreg)
		j_o(ivshp)%d(1:p_nrow)=p_coefmax*p_vc(1:p_nrow)
		!	endif !if(p_nrow>0)   7893
		!	endif !if(j_ivout.ne.j_ivresult)   9583
		!write(6,*)'<47p_zmatrix',p_zmatrix
		if (p_zmatrix)then
			write(6,*)' '
			write(6,*)'***because there was zmatrix->, results are given in:***'
			call j_getname(ivrows,ivzval,ivshp)
			write(6,*)j_oname2(1:j_loname2),' = vector of values of variables'
			write(6,*)j_oname(1:j_loname),' = vector of rows'
			write(6,*)j_oname3(1:j_loname3),' = vector of shadow prices of rows'
			call j_getname(ivredcost,p_ivobj)
			write(6,*)j_oname(1:j_loname),' = vector of reduces costs of variables'
			write(6,*)j_oname2(1:j_loname2),' = value of the objective'
			write(6,*)' '
			goto 8000
		endif !if (p_zmatrix)   6495
 
		!	irow0=0
		irow=0
 
		if(p_iprint.lt.1)goto 8000 !repoepilo
		!title
 
 
		!		call j_printtitle(nureport, p_ivprob)
		!	write(6,*)'p_row0',p_row0,p_rowdomnum,'units',p_domainunits
		!i0=1
		!if(p_isobjective)i0=0
		!	i0=0
		!	ist=0
		!if(p_isobjective)ist=1
		write(nureport,'(6x,a,27x,a,5x,a,2x,a,7x,a,2x,a)')'row','value','shadow price','RHS','RHS2','binding'
		!			domloop: do j=1,p_nsetd(i)
		!write(6,*)'<5553>',j_nsetd(i)
		write(nureport,*)'    ',('_',kk=1,79)
		write(nureport,*)' '
		do irow=0,p_nrow
			!	irowtot=irow+1
			!	write(6,*)'row7 ',irow
			call writerow(nureport,irow) !jlpz
 
			!	write(nureport,*)' ',('_',kk=1,78)
			! write(6,*)'rowdomnum',p_rowdomnum
			! write(6,*)'ibatemp',p_ibatemp
			! write(6,*)'ix',p_ix
			! !	if(p_isdomain)idom=1
			! write(6,*)'idom ',idom
 
 
			!		end do !jx=1,p_nxvartot+p_ncvar+p_noutsubtrans   8083
			!		endif !if(p_iprint.ge.1)   8069
			!	end if !if(idom2.ne.idom.or.irow.eq.p_nrow)   8067
 
 
			!		end do domloop !domloop: do j=1,j_nsetd(i)
			!	idomv=idom
		end do !irow=0,p_nrow   6528
 
		!	if(j_ivout.ne.j_ivresult)then
		!call j_defmatrix(j_ivout,'%problemrows',p_nrow,1,j_matreg,ivout)
 
		!	j_o(ivout)%d = p_irowrow(1:p_nrow)
		!	if(p_nrow.gt.0)then
		! call j_defmatrix(j_ivout,'%rhs',p_nrow,1,j_matreg,ivout)
 
		! j_o(ivout)%d = p_rhs
		! call j_defmatrix(j_ivout,'%rhs2',p_nrow,1,j_matreg,ivout2_)
 
		! j_o(ivout2_)%d = p_rhs2
		! do i_=1,p_nrow
		! if(p_rhs(i_).eq.j_ninf)j_o(ivout)%d(i_) =j_ninf
		! if(p_rhs2(i_).eq.j_inf)j_o(ivout2_)%d(i_) =j_inf
		! enddo !i_=1,p_nrow   8382
		!	call j_defmatrix(j_ivout,'%shprice',p_nrow,1,j_matreg,ivout2_)
		!	j_o(ivout2_)%d = p_vc(1:p_nrow)
		!		endif !if(p_nrow.gt.0)   9656
 
		!	endif !if(j_ivout.ne.j_ivresult)   9652
 
	endif !if(.not.injlp)   6482
	!if(p_isz.and.p_iprint.ge.1)then
	write(nureport,*)' '
	write(nureport,*)'    ',('_',kk=1,46)
	p_buf='z-variable'
	p_buf(25:30)='value'
	p_buf(36:48)='reduced cost'
	write(nureport,'(a)')'     '//p_buf(1:79)
	write(nureport,*)'    ',('_',kk=1,46)
	DO 66778 I=1,p_nz
		p_buf=j_vname(p_zvars(i))
		le=j_lename(p_zvars(i))
		if(le.lt.20)p_buf(le+2:21)=p_dots
		p_buf(23:33)=j_chr10(dble(p_x(p_nrow+i)))
		j_v(p_zvars(i))=p_x(p_nrow+i)
		p_buf(36:46)=j_chr10(p_redcost(i))
		write(nureport,'(a)')'     '//p_buf(1:46)
66778 continue !66778 I=1,p_nz   6580
	!endif !if(p_isz.and.p_iprint.ge.1)   9674
	write(nureport,*)' '
8000 continue  !end printing
 
	call repoepilog(nureport,.false.)
 
 
end subroutine repoz !subroutine repo(nureport)

subroutine writerow(nureport,irow)
	use jmod, only: j_getline
	use jmod, only: p_ivrow
	use jmod, only: p_buf
	use jmod, only: j_nextlim
	use jmod, only: j_chi5
	use jmod, only: p_apubuf
	use jmod, only: p_feasible
	use jmod, only: p_dots
	use jmod, only: j_yes2
	use jmod, only: j_0
	use jmod, only: p_x
	use jmod, only: p_value
	use jmod, only: p_rhscur
	use jmod, only: j_chr10
	use jmod, only: p_coefmax
	use jmod, only: p_vc
	use jmod, only: p_maxo
	use jmod, only: p_rhs
	use jmod, only: p_rhs2
	use jmod, only: p_lbou
	use jmod, only: p_ubou
	use jmod, only: p_objf
	!	write(6,*)'nure ',irow
	irowtot=irow+1
	call j_getline(p_ivrow,irowtot,p_buf,le)
	ip=j_nextlim(p_buf,1,le,'=<>')
	! p_buf=p_buf(1:ip-1)//'+'//p_buf(1:ip-1)//'-'//p_buf(1:ip-1)//p_buf(ip:le)
	! le=len_trim(p_buf)
	! !	write(6,*)p_buf(1:le)
	! ip=j_nextlim(p_buf,1,le,'=<>')
	!p_buf(6+le:)=p_dots
	!	write(6,*)'%',p_buf(1:le)
	if(irow.gt.0)then
		p_apubuf=j_chi5(irow,0)
		!	*    5*
		!	write(6,*)'*',p_apubuf,'*'
		p_buf=p_apubuf(2:5)//') '//p_buf(1:ip-1)
		le=ip+5
	else !if(j.eq.1) then !if(irow0.ne.irowobj)then
 
		if(p_feasible)then   !p_feasible)then
			!write(16,*)'bufnu',p_buf
			!!write(16,*)'ip ',ip
			p_buf=p_buf(ip+2:ip+5)//' '//p_buf(1:ip-1)
			le=ip+4
 
		else !if(p_feasible)then
			p_buf(1:5)=' '
			p_buf(6:33)='Infeasible, temporary objective'
			!	if(p_isn16)call rint('nonfeas',p_n16)
 
			! call leftvec(p_rhscur,p_x)
			! write(6,*)'pxny',p_x
 
			le=33
			!	le=
		endif !if(p_feasible)   6617
 
	endif !if(irow.gt.0)   6609
	!lyhennetään ylipitkät rivit etteivät sotkeennu
	iplusv=0
	!	write(6,*)'le ',le
	do while(le.gt.34)
		!	j_buf(34:35)=j_dots !chnaged by JL 10.9.2018
		iplus=0
		!		write(6,*)'iplus',iplus
		do while(iplus.le.34.and.iplus.le.le)
			iplusv=iplus
			iplus=j_nextlim(p_buf,iplus+1,le,'+-')
		enddo !while(iplus.le.34.and.iplus.le.le)   6643
		if(iplus.gt.le)then
 
			exit
		else
			write(nureport,'(a)')p_buf(1:iplusv)
			p_buf=p_buf(iplusv+1:le)
			le=le-iplusv
 
		endif !if(iplus.gt.le)   6647
	enddo !while(le.gt.34)   6639
	p_buf(le+1:34)=p_dots
 
	if(irow.ge.1)then
		!constraint row
		j_yes2=p_x(irow).eq.j_0
		!			irow=irow+1
		!		j_apubuf=j_chi5(irow,0); j_buf(1:3)=j_apubuf(3:5);j_buf(4:5)=') '
		!	write(6,*)'irow,p_rhscur(irow),p_x(irow)',irow,p_rhscur(irow),p_x(irow)
		p_value=p_rhscur(irow)-p_x(irow)
		p_buf(35:35)=' '
		p_buf(36:)=j_chr10(p_value)
		p_buf(47:57)=j_chr10(p_coefmax*p_vc(irow))
		!	write(6,*)'shp',p_vc(irow)
		if(p_vc(irow).ne.0..and.j_yes2)then
			p_buf(78:78)='L'
			if(p_maxo)then
 
				if(p_vc(irow).gt.j_0.and.j_yes2)p_buf(78:78)='U'
			else
 
				if(p_vc(irow).lt.j_0.and.j_yes2)p_buf(78:78)='U'
			end if !if(p_maxo)   6672
		endif !if(p_vc(irow).ne.0..and.j_yes2)   6670
		!	write(6,*)'jsdj'
		if(p_rhs(irow).eq.p_rhs2(irow))then
			p_buf(65:72)= j_chr10(dble(p_rhs(irow)))
		else !if(p_rhs(irow).eq.p_rhs2(irow))then
			if(p_lbou(irow))p_buf(60:67)= j_chr10(dble(p_rhs(irow)))
			if(p_ubou(irow))p_buf(69:76)= j_chr10(dble(p_rhs2(irow)))
 
		end if !if(p_rhs(irow).eq.p_rhs2(irow))   6681
 
	else
		! for maximization rhs1 =huge  rhs2=0
		! for minimization  rhs2=-huge
		p_buf(34:35)=' '
		!	write(6,*)'pcoefm',p_coefmax,p_objf
		write(p_buf(36:),*)p_coefmax*p_objf
		!	write(16,*)' obj finaö',p_objf,p_xps(0)+dot_product(p_x(p_ls(1:p_nrow)),p_objr(p_ls(1:p_nrow)))
		!	p_buf(36:)=j_chr10(p_coefmax*p_objf)
 
		! if(p_feasible)then
		! if(j_maxo)then
		! j_buf(1:5)=' max'
		! else !if(j_maxo)then
		! j_buf(1:5)=' min'
		! end if !if(j_maxo)then
		! else !if(p_feasible)then
		! j_buf(1:5)=' '
		! j_buf(6:33)='Infeasible, temporary object'
		! endif !if(p_feasible)then
	end if !if(irow.ge.1)   6659
	if(le.lt.35)p_buf(le+1:34)=p_dots
	write(nureport,'(a)')p_buf(1:79)
	if(irow.eq.0)	write(nureport,'(a)')' '
 
	!	write(nureport,'(a)')'ssw'//p_buf(1:79)
 
end subroutine writerow

! subroutine pack()
! p_nxbas=0
! p_vx=j_0
! doloop:	do j=1,p_nrowcurx
! iro=p_rowcurx(j)
! if(p_vx(iro).ne.j_0)then
! do ip=1,p_nxbas-1
! if( p_ix(iro).eq.p_ixpack(ip))then
! p_vxpack(ip)=p_vxpack(ip)+p_vx(iro)
! cycle doloop
! endif !if( p_ix(iro).eq.p_ixpack(ip))   6355
! enddo !ip=1,p_nxbas-1   6354
! p_nxbas=p_nxbas+1
! p_vxpack(p_nxbas)=p_vx(iro)
! p_ixpack(p_nxbas)=p_ix(iro)   ! result (1*ind1,2*ind2,....)

! endif !if(p_vx(iro).ne.j_0)   6353
! enddo doloop !oop:	do j=1,p_nrowcurx   6351


! end subroutine
! subroutine pack()
! double precision vciro
! p_nxbas=0
! doloop:	do j=1,p_nrowcurx
! iro=p_rowcurx(j)
! if(iro.eq.1.and.p_ix0.ne.0)then
! if(p_feasible)then
! vciro=j_onen

! else
! cycle
! endif !if(p_feasible)  10096
! vciro=p_vc(iro)
! endif !if(iro.eq.1.and.p_ix0.ne.0)  10095
! if(vciro.ne.j_0)then
! do ip=1,p_nxbas-1
! if( p_ix(iro).eq.p_ixpack(ip))then
! p_vcpack(ip)=p_vcpack(ip)+vciro
! cycle doloop
! endif !if( p_ix(iro).eq.p_ixpack(ip))  10106
! enddo !ip=1,p_nxbas-1  10105

! p_nxbas=p_nxbas+1
! p_vcpack(p_nxbas)=p_vc(iro)
! p_ixpack(p_nxbas)=p_ix(iro)   ! result (1*ind1,2*ind2,....)

! endif !if(vciro.ne.j_0)  10104
! enddo doloop !oop:	do j=1,p_nrowcurx  10093


! end subroutine


subroutine senterf()
	use jmod, only: p_post
	use jmod, only: p_valueopt
	use jmod, only: p_small
	use jmod, only: p_iopt
	use jmod, only: p_isdomain
	use jmod, only: p_unit
	use jmod, only: p_ibaunit
	use jmod, only: p_keys
	use jmod, only: p_fastvaluemin
	use jmod, only: p_basreject
	use jmod, only: p_ns
	use jmod, only: p_lx0
	use jmod, only: p_lunit
	use jmod, only: p_lx
	use jmod, only: p_isch
	use jmod, only: j_inf
	use jmod, only: p_fast
	use jmod, only: p_fastnow
	use jmod, only: p_fastvalues
	use jmod, only: j_dnkeep
	use jmod, only: p_ntemp0
	use jmod, only: p_nfact
	use jmod, only: p_kntot
	use jmod, only: j_ninf
	use jmod, only: p_fastreject
	use jmod, only: p_filre
	use jmod, only: p_rejects
	use jmod, only: j_dapu2
	use jmod, only: j_0
	use jmod, only: p_xpresent
	use jmod, only: p_xmatrow
	use jmod, only: p_xmat
	use jmod, only: p_vx
	use jmod, only: p_valuek
	use jmod, only: p_logval
	use jmod, only: p_optfact0
	use jmod, only: p_nlog
	use jmod, only: j_dmat
	use jmod, only: p_keeplog
	use jmod, only: p_logtable
	use jmod, only: p_logtablei
	use jmod, only: j_o
	use jmod, only: p_neigbas
	use jmod, only: p_ibafact
	use jmod, only: p_factw
	use jmod, only: p_logfactterm
	use jmod, only: p_rowofterm
	use jmod, only: j_dapu3
	use jmod, only: p_vc
	use jmod, only: p_optfact
	use jmod, only: p_optfactkey
	use jmod, only: p_fastmake
	use jmod, only: p_ivvaluedif
	use jmod, only: p_nimp
	use jmod, only: p_fastcut
	use jmod, only: p_fastpros2
	use jmod, only: p_activeunit
	use jmod, only: p_tolecur
	use jmod, only: p_newd
	use jmod, only: p_newa
	use jmod, only: p_nz
	use jmod, only: p_nrow
	use jmod, only: p_ix
	use jmod, only: p_objr
	use jmod, only: p_i1
	use jmod, only: p_ispiece
	use jmod, only: j_dapu
	use jmod, only: p_pieceprice
	use jmod, only: p_npiece
	use jmod, only: p_piecekeep
	use jmod, only: p_isarea
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_row0
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: j_one
	use jmod, only: p_tmax
	use jmod, only: p_enter
	use jmod, only: p_logvolkey
	use jmod, only: p_logvol
	!	use fletdmod
	!	use fletdmod2
	!	use fletcherdmod
	double precision area,logvol,logvolkey,factopt
 
	!p_enter0=.true.
	p_post=.true.
 
 
	p_valueopt=p_small  !j_ninf
	!p_secondb=p_small
 
	p_iopt=0        ! the optimal schedule
 
	! start computing shadow pricees of schedules <B333>
	!	write(6,*)'idom4',p_unit,p_idomba
	if(p_isdomain)write(16,*)'nytr'
 
	if(p_isdomain)call jlpcurix(p_unit)  !(p_unit)
 
 
	!	nxbas=count(p_ixpack.ne.0)
	!	call pack()
	! Jos tehtaita, ei ohiteta
	! if(p_nxbas.le.0.and..not.p_fpresent)then
 
	! endif !if(p_nxbas.le.0.and..not.p_fpresent)  10047
 
	ikey_ = p_ibaunit(p_unit)+p_keys(p_unit)
 
 
 
	p_fastvaluemin=1.7d37
	!is(p_p.and.p_fpresent)write(p_n16,*)'**fact** <5586> unit,key',p_unit,p_keys(p_unit),p_xpresent2
	p_fastvaluemin=1.7d37
	!!!!kannassa olevat vaihtoehdot ohitetaan
	p_basreject(1:p_ns(p_unit))=.false.
	do k_=1,p_lx0
		if(p_lunit(p_lx(k_)).ne.p_unit) cycle
		i=p_isch(p_lx(k_))
		p_basreject(i)=.true. !if(p_isch(p_lx(k_)).ne.i) cycle
		! if(istree)then add if tree added
		! ibaxmat=xmatiba(iobs) !,1)
		! do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
		! valuesums(jj_)=valuesums(jj_-1)+&
		! vxpack3(jj_)*p_xmat(ixpack3(jj_)+ibaxmat) !dprod(vxpack3(jj_),p_xmat(ixpack3(jj_)+ibaxmat))
		! enddo !do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
		! endif !if(istree)then
		if(p_fast.and..not.p_fastnow)p_fastvalues(i)=j_inf
		!	p_basreject(cycle nschloop
	enddo !k_=1,p_lx0   6809
	ibadat=(p_ibaunit(p_unit)-1)*j_dnkeep
	ibaxmat=(p_ibaunit(p_unit)-1)*p_ntemp0
	ibanei=(p_unit-1)*p_nfact
	ibakeyf=(p_unit-1)*p_kntot
	!write(17,*)'nytpå',kier
	!  -fopenmp
	p_valueopt=j_ninf
	!$OMP PARALLEL DO
	!		j_dapu=j_ninf   ! best
	p_valueopt=j_ninf
		nschloop:	do i=1,p_ns(p_unit)   !mainloop
		ibaxmat=ibaxmat+p_ntemp0
		ibadat=ibadat+j_dnkeep
 
		if(p_basreject(i))cycle
		! p_svalue(i)=j_ninf
		! cycle
		! endif !if(p_basreject(i))  10386
		!write(17,*)'nytpot',p_kier,p_unit,i
		iobs=p_ibaunit(p_unit)+i
		if(p_fastnow)then
			if(p_fastreject(iobs).and.i.ne.p_keys(p_unit))then
				!		p_svalue(i)=j_ninf
				cycle  !i.ne.j_keys(p_unit) added 20.8.2018 JL
			endif !if(p_fastreject(iobs).and.i.ne.p_keys(p_unit))   6844
		endif !if(p_fastnow)   6843
		if(p_filre)then
			if(p_rejects(iobs))then
				! if(istree)then
				! ibaxmat=xmatiba(iobs) !,1)
				! do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
				!valuesums(jj_)=valuesums(jj_-1)+dprod(vxpack3(jj_),p_xmat(ixpack3(jj_)+ibaxmat))
				! enddo !do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
				! endif !if(istree)then
				!			p_svalue(i)=j_ninf
				cycle
			endif !if(p_rejects(iobs))   6850
		endif !if(p_filre)   6849
 
		j_dapu2=j_0
 
 
		if (p_xpresent) then
			! if(istree)then
			! ibaxmat=xmatiba(iobs) !,1)
			! do jj_=idiff3(idiff(iobs)),p_nxbas
			! valuesums(jj_)=valuesums(jj_-1)+vxpack3(jj_)*p_xmat(ixpack3(jj_)+ibaxmat) !dprod(vxpack3(jj_),p_xmat(ixpack3(jj_)+ibaxmat))
 
			! enddo !do jj_=idiff3(idiff(iobs)),p_nxbas
			! p_value=valuesums(p_nxbas)
 
			! else !if(istree)then
			!			ibaxmat=xmatiba(iobs) !,1)xmatiba(iobs)=(iobs-1)*p_ntemp0
			!		p_value=dot_product(p_vxpack2(1:p_nxbas),p_xmat(p_ixpack2(1:p_nxbas)+ibaxmat) )   !!!!
			!		p_svalue(i)=dot_product(p_vxpack(1:p_nxbas),p_xmat(p_ixpack(1:p_nxbas)+ibaxmat) )   !!!!
			p_xmatrow=p_xmat(ibaxmat+1:ibaxmat+p_ntemp0)
			j_dapu2=dot_product(p_vx,p_xmatrow)
			!	j_dapu2=dot_product(p_vxpack(1:p_nxbas),p_xmat(p_ixpack(1:p_nxbas)+ibaxmat) )
			if(i.eq.p_keys(p_unit))then
				p_valuek=j_dapu2
				ibaxmatkey=ibaxmat
			elseif(j_dapu2.gt.p_valueopt)then
				p_valueopt=j_dapu2
				p_iopt=i
				ibaxmatopt=ibaxmat
 
			endif !if(i.eq.p_keys(p_unit))   6881
			!	if(p_p)write(6,*)'i,val',i,p_vxpack(1:p_nxbas),p_xmat(p_ixpack(1:p_nxbas)+ibaxmat) ,p_svalue(i)
			!pitäsi testata, kumpi on nopeampi. Näyttävät PC:llä tuottavan saman tuloksen
			!	  j_value=j_0
			!  do jj_=1,nxbas
			!  j_value=j_value+dprod(j_vxpack2(jj_),j_xmat(j_ixpack2(jj_),iobs))
			!  enddo
			!		endif !if(istree)then
			!		if(p_ispiece)p_svalue(i)=p_svalue(i)+pieceval()
			!	dot_product(j_o(p_ivpieceprice)%d,&
			!		j_o(j_divmat)%d((iobs-1)*j_dnkeep+j_o(p_ivpiecekeep)%i2 ))
 
			!	else !if (j_xpresent2) then
			!		p_svalue(i)=j_0   !p_value = j_0
		endif !if (p_xpresent)   6865
		!is(p_p)write(p_n16,*)'<6712>,nxbas,p_unit,i,initval',p_nxbas,p_unit,i,p_value
		!!!jos tehtaita, valuen arvoa kasvatetaan
		!	if (p_fpresent) then !!!!
 
 
 
		!		if(j.lt.10)write(6,*)'i,j,',i,j,j_dapu2
		!		j_dapu3=j_0
		p_logval=j_0
		p_optfact0=0
		do ilog=1,p_nlog
			!			write(6,*)'ilo,',ilog,j_dmat(ibadat+p_keeplog(ilog))
			logvol=j_dmat(ibadat+p_keeplog(ilog))
			if(logvol.le.j_0)cycle
			ifactopt=0
			factopt=j_ninf
			!		p_factval=j_0  !value for different factories
			!	p_xps0=j_0
			ivtable=p_logtable(ilog)
			itab=p_logtablei(ilog)
 
			knn=j_o(ivtable)%i(10)
			do k=1,knn
				ifact=j_o(ivtable)%i2(p_neigbas(i)+k)  !p_neig(ibanei+k)
				if(ifact.le.0)exit
				inde=p_ibafact(ilog)+p_factw(ifact)   !index of the logfact
 
				iterm=p_logfactterm(inde)
				if(iterm.le.0)exit
				!	write(6,*)'k,ifact,inde,iterm ',k,ifact,inde,iterm
				irow=p_rowofterm(iterm)
 
 
				j_dapu3=p_vc(irow)*logvol
 
 
				if(j_dapu3.gt.factopt)then
					ifactopt=ifact
					factopt=j_dapu3
				endif !if(j_dapu3.gt.factopt)   6940
 
			enddo !k=1,knn   6926
 
			p_optfact0(ilog)=ifactopt
			if(ifactopt.gt.0)p_logval(ilog)=factopt
			!		if(i.le.2.and.j.le.10)write(6,*)'ifactopt,p_logval(ilog)',ifactopt,p_logval(ilog)
 
		enddo !ilog=1,p_nlog   6914
		!	if(i.le.2)write(6,*)'p_logval',p_logval
		!	if(i.eq.5)read(5,*)ii
 
		j_dapu3=j_dapu2+sum(p_logval)  !sum
		! if(j_dapu3.ne.j_0.or.j.eq.p_keys(p_unit))then
		! write(6,*)'i,j,j_dapu3 ,optfact0,logval',&
		! i,j,j_dapu3,j.eq.p_keys(p_unit)
		! write(6,'(25i6)')p_optfact0
		! write(60,*)'i,j,j_dapu3 ',i,j,j_dapu3
		! write(60,'(25i6)')p_optfact0
		! write(6,*)'keyf'
		! write(6,'(25i6)')p_keyfact(ibakeyf+1:ibakeyf+p_nlog)
		! !		write(6,'(25f6.2)')p_factval
		! write(6,'(25f6.2)')p_logval
		! endif !if(j_dapu3.ne.j_0.or.j.eq.p_keys(p_unit))  10995
 
		if(j_dapu3.gt.p_valueopt)then
			write(6,*)'imp',i,j_dapu3,j_dapu2,p_keys(p_unit),p_valueopt
			p_valueopt=j_dapu3
			p_optfact=p_optfact0
			p_iopt=i
			ibadatopt=ibadat
			ibaxmatopt=ibaxmat
 
		endif !if(j_dapu3.gt.p_valueopt)   6968
		if(i.eq.p_keys(p_unit))then
			p_optfactkey=p_optfact
 
			ibaxmatkey=ibaxmat
			ibadatkey=ibadat
			p_valuek=j_dapu3
			!		write(6,*)'key',i,j_dapu3
		endif !if(i.eq.p_keys(p_unit))   6977
 
		if(p_fastmake)then
			p_fastvalues(i)=j_dapu3  !p_svalue(i)  !p_value
			if(j_dapu3.lt.p_fastvaluemin)p_fastvaluemin=j_dapu3 !p_value
		endif !if(p_fastmake)   6986
 
	enddo nschloop !hloop:	do i=1,p_ns(p_unit)   6833
 
	!$OMP END PARALLEL DO
	!	p_valuek=p_svalue(p_keys(p_unit))
	!	p_loco=maxloc(p_svalue(1:p_ns(p_unit))) !output must be array
	!	p_iopt=p_loco(1)
	!	p_valueopt=p_svalue(p_iopt)
	j_o(p_ivvaluedif)%d(p_unit)=p_valueopt-p_valuek
 
	!is(p_p)write(p_n16,*)'p_unitetc',p_unit,p_valuek,p_iopt,p_valueopt
	!read(5,*)iiii
	if(p_fastmake)then
		if(p_fastreject(p_ibaunit(p_unit)+p_iopt))p_nimp=p_nimp+1
		!		p_fastcut=min(p_fastvaluemin+p_fastpros2*(p_valueopt-p_fastvaluemin),p_valuek)-p_tiny78
		p_fastcut=p_fastpros2*p_valuek
		p_fastreject(p_ibaunit(p_unit)+1:p_ibaunit(p_unit)+p_ns(p_unit))=.false.
		nac=0
		do i=1,p_ns(p_unit)
			if(p_fastvalues(i).lt.p_fastcut.and.i.ne.p_keys(p_unit))then
				p_fastreject(p_ibaunit(p_unit)+i)=.true.
			else
				nac=nac+1
			endif !if(p_fastvalues(i).lt.p_fastcut.and.i.ne.p_keys(p_unit))   7009
			!	if(fastvalues(i).lt.fastcut)write(16,*)'reject',p_unit,i
		enddo !i=1,p_ns(p_unit)   7008
		p_activeunit(p_unit)=nac.gt.1
		!	write(17,*)p_unit,p_ns(p_unit),nac,p_fastcut,p_valuek
 
 
	endif !if(p_fastmake)   7002
 
	!	write(16,*)'p_unit,p_iopt,p_valueopt,p_valuek,p_valueopt.lt.p_valuek+p_tolecur',&
	!	p_unit,p_iopt,p_valueopt,p_valuek,p_valueopt.lt.p_valuek+p_tolecur
	if(p_iopt.eq.p_keys(p_unit).or.p_valueopt.lt.p_valuek+p_tolecur)return
	!subroutine entercol(newc)
	!	double precision piecexmat0,area
	!when a schedule enters this computes the entering  column of the a-matrix
	! and computes also the objective row element
	! with factories j_valueopt_af computed with fenter0 is used both for the
	! objective row and for the entire column
	!käyttä p_iopt mikä globaaliksi
 
	! j_valueopt_af computed with fenter0 is needed here toupdate the object row
 
	ld01=p_lx0+1  !!!! get next free column in a,
	! ld0 is the number of used (basic) cols in D-part i.e. after z-cols
	p_newd=p_lx(ld01)
	p_newa=p_newd+p_nz   ! after p_nz
	newc=p_newa+p_nrow  !in Fletecher cols, I part (residuals) are counted also
	!	if(p_p) write(p_n16,*)'ent sched: p_newd',p_newd,'newc',newc, 'tol=',p_tolecur
	! get key schdeule
	iobs= p_ibaunit(p_unit)+p_keys(p_unit)
	iobsopt=p_ibaunit(p_unit)+p_iopt
	ibaxmat=ibamatx(iobs) !,1)
	ibaxmat2=ibamatx(iobsopt) !,2)
	p_lunit(p_newd)=p_unit
	p_isch(p_newd)=p_iopt
 
	!p_colkey(p_newd)=p_keys(p_unit)
	! 6.42  a0'D , here we are prepared that the same x can be in differen rows
	! how does the object variable change
	!!!! put key schedule first int objr and a matrix
 
	! update objr0
	if(p_ix(0).ne.0)then  !then p_ixcur(0))then
		p_objr(newc)=p_xmat(p_ix(0)+ibaxmat2)-p_xmat(p_ix(0)+ibaxmat) !v(ix(0))
 
		p_i1=2
	else !if(j_ixcur(0).ne.0)then
		p_i1=1
		p_objr(newc)=j_0
	endif !if(p_ix(0).ne.0)   7055
 
	!write(6,*)'objr ', p_objr0(newc)
	if(p_ispiece)then !update objr0
		j_dapu=dot_product(p_pieceprice(1:p_npiece),j_dmat((iobsopt-1)*j_dnkeep+p_piecekeep) )
		!	write(6,*)'dapu',j_dapu,'price ',p_pieceprice(1:p_npiece),' damat',j_dmat((iobs-1)*j_dnkeep+p_piecekeep),'iobs',iobs
		if(p_isarea)j_dapu=j_dapu*area(p_unit)
		!	write(6,*)'afdapu',j_dapu
		j_dapu2=dot_product(p_pieceprice(1:p_npiece),j_dmat((iobs-1)*j_dnkeep+p_piecekeep) )
		!write(6,*)'dapu2',j_dapu2,'price  damat',j_dmat((iobs-1)*j_dnkeep+p_piecekeep),'iobs',iobs
		p_objr(newc)=p_objr(newc)+j_dapu-j_dapu2
		!	write(6,*)'objr now ', p_objr0(newc)
	endif !if(p_ispiece)   7065
 
	p_a(p_abas(p_newa)+1:p_abas(p_newa)+p_nrow)=j_0 !p_a(1:p_nrow,p_newa)=j_0
	!if(p_p)write(p_n16,*)p_row0,p_nrowcurx,p_rowcurx(p_row0:p_nrowcurx),'pix',p_ix
	!if(p_p)write(p_n16,*)'p_ntemp0',p_ntemp0,'iba',ibaxmat2,'iobsop',iobsopt,'iopt',p_iopt,'iunit',p_unit
	!if(p_p)write(p_n16,*)p_xmat(ibaxmat2+1:ibaxmat2+p_ntemp0),p_xmat(ibaxmat2+1:ibaxmat2+p_ntemp0)
	do iro=p_row0,p_nrowcurx
		!	if(p_p)write(p_n16,*)iro,p_rowcurx(iro),p_ix(p_rowcurx(iro)),p_xmat(ibaxmat2+p_ix(p_rowcurx(iro)))
		!	p_a(p_rowcurx(iro),p_newa)=p_xmat(ibaxmat2+p_ix(p_rowcurx(iro)))-p_xmat(ibaxmat+p_ix(p_rowcurx(iro)))
		p_a(p_abas(p_newa)+p_rowcurx(iro))=p_xmat(ibaxmat2+p_ix(p_rowcurx(iro)))-p_xmat(ibaxmat+p_ix(p_rowcurx(iro)))
	enddo !iro=p_row0,p_nrowcurx   7080
 
 
 
	p_tmax=j_one	!;p_rcur=p_tmax	! myöhemmin pintala
	p_enter(3)=.true.	!!!! scedule enters
 
	!is(p_p.and.p_fpresent) write(p_n16,*)'**fact** KANTAAN tulossa >> vaihtoehto, p_enter = 3; unit, sch' , &
	!p_unit,p_iopt
	p_logvolkey=j_0
	p_logvol=j_0
	do ilog=1,p_nlog
		logvol=j_dmat(ibadatopt+p_keeplog(ilog))
		logvolkey=j_dmat(ibadatkey+p_keeplog(ilog))
		if(p_optfact(ilog).le.0)then
			write(16,*)'optfact0ilog,logvol,logvolkey,ibadatopt,ibadatkey',ilog,logvol,logvolkey,ibadatopt,ibadatkey
			cycle
		endif !if(p_optfact(ilog).le.0)   7098
		!				if(logvol.le.j_0)cycle
		inde=p_ibafact(ilog)+p_factw(p_optfact(ilog))   !index of the logfact
 
		iterm=p_logfactterm(inde)
 
		irow=p_rowofterm(iterm)
		p_a(p_abas(p_newa)+irow)=logvolkey-logvol
		p_logvolkey(irow)=logvolkey
		p_logvol(irow)=logvol
		write(16,*)'ilog,irow',ilog,irow,p_a(p_abas(p_newa)+irow)
 
	enddo !ilog=1,p_nlog   7095
 
 
 
	!end subroutine entercol !subroutine entercol(newc)
 
 
 
	!	if()call entercol
 
 
	!write(17,*)'senterinlopus',kier
 
end subroutine senterf !subroutine senter()


subroutine entercol(newc)  !normal case
	use jmod, only: p_lx
	use jmod, only: p_lx0
	use jmod, only: p_lunit
	use jmod, only: p_unit
	use jmod, only: p_isch
	use jmod, only: p_iopt
	use jmod, only: p_nz
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_nrow
	use jmod, only: p_anew0
	use jmod, only: p_akey
	use jmod, only: p_abas1
	use jmod, only: p_nrowtot
	use jmod, only: p_akey0
	use jmod, only: p_aopt
	use jmod, only: p_aopt0
	use jmod, only: j_err
	use jmod, only: p_nrowz
	use jmod, only: p_isn16
	use jmod, only: p_feasible
	use jmod, only: p_objr
	!	write(16,*)'identercol',p_lx0,p_lx,'punit',p_unit,'newa',newa
	id=p_lx(p_lx0+1)
	p_lunit(id)=p_unit
	p_isch(id)=p_iopt
	!	write(30,*)p_kier,p_unit,p_iopt,p_valuek,p_valueopt,p_vc
	!p_colkey(p_id)=p_keys(p_unit)
	!	p_enter=3
	!call updatel()
	!	id=p_lx(p_lx0+1)
	newa=p_nz+id
 
	p_a(p_abas(newa)+1:p_abas(newa)+p_nrow)=p_anew0(1:p_nrow)
	p_akey(p_abas1(id)+1:p_abas1(id)+p_nrowtot)=p_akey0(0:p_nrow)
	p_aopt(p_abas1(id)+1:p_abas1(id)+p_nrowtot)=p_aopt0(0:p_nrow)
 
	do j=1,p_nrow
		if(abs(p_anew0(j)-p_aopt0(j)+p_akey0(j)).gt.0.01)then
			write(16,*)'parror,p_unit,p_iopt,id',p_unit,p_iopt,id
			write(16,*)p_anew0
			write(16,*)p_aopt0
			write(16,*)p_akey0
			write(16,*)p_anew0(1:p_nrow)-p_aopt0(1:p_nrow)+p_akey0(1:p_nrow)
			write(6,*)'parror,p_unit,p_iopt,id',p_unit,p_iopt,id
			j_err=.true.
			return  !read(5,*)jdjhd
		endif !if(abs(p_anew0(j)-p_aopt0(j)+p_akey0(j)).gt.0.01)   7146
 
	enddo !j=1,p_nrow   7145
	newc=id+p_nrowz
 
	if(p_isn16)write(16,*)'after entercol ',p_anew0(1:p_nrow)
	! a=opt-key
	! do j=1,p_nrow
	! if(abs(p_a(p_abas(newc)+j)-p_aopt(p_abas1(p_id)+j+1)+p_akey(p_abas1(p_id)+j+1)).gt.0.1)then
	! write(16,*)p_a(p_abas(newc)+j),p_akey(p_abas1(p_id)+j+1),p_aopt(p_abas1(p_id)+j+1),&
	! p_a(p_abas(newc)+j)-p_akey(p_abas1(p_id)+j+1)+p_aopt(p_abas1(p_id)+j+1)
	! write(16,*)'pa',99.d0,p_a(p_abas(newc)+1:p_abas(newc)+p_nrow)
	! write(16,*)'popt',p_aopt(p_abas1(p_id)+1:p_abas1(p_id)+p_nrowtot)
	! write(16,*)'pkey',p_akey(p_abas1(p_id)+1:p_abas1(p_id)+p_nrowtot)
	! write(16,*)'dif',p_aopt(p_abas1(p_id)+1:p_abas1(p_id)+p_nrowtot)-p_akey(p_abas1(p_id)+1:p_abas1(p_id)+p_nrowtot)
	! write(6,*)'asekashere'
	! read(5,*)kdkdfk
	! endif !if(abs(p_a(p_abas(newc)+j)-p_aopt(p_abas1(p_id)+j+1)+p_ake  10042
	! enddo !j=1,p_nrow  10041
 
 
	if(p_feasible)p_objr(newc)=p_aopt0(0)-p_akey0(0)
end subroutine entercol

! subroutine entercolminus(idin,idout) !p_id i
! idout=p_lx(p_lx0+1)
! p_lunit(idout)=p_lunit(idin)

! p_a(p_abas(p_nz+idout)+1:p_abas(p_nz+idout)+p_nrow)=-p_a(p_abas(p_nz+idin)+1:p_abas(p_nz+idin)+p_nrow)
! p_objr(p_nrowz+idout)=-p_objr(p_nrowz+idin)

! p_xps(0:p_nrow)=p_xps(0:p_nrow)-p_akey(p_abas1(p_nz+idin)+1:p_abas1(p_nz+idin)+p_nrowtot)+ &
! p_aopt(p_abas1(p_nz+idin)+1:p_abas1(p_nz+idin)+p_nrowtot)
! p_isch(idout)=p_keys(p_lunit(idin))
! p_keys(p_lunit(idin))=p_isch(idin)

! p_rhsw=p_rhscur-p_xps(1:p_nrow)
! end subroutine

subroutine entercol0(newc,iunit,iopt,aopt,akey)  !nonnormal
	use jmod, only: p_nrowtot
	use jmod, only: p_lx
	use jmod, only: p_lx0
	use jmod, only: p_lunit
	use jmod, only: p_isch
	use jmod, only: p_nz
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_nrow
	use jmod, only: p_akey
	use jmod, only: p_abas1
	use jmod, only: p_aopt
	use jmod, only: p_nrowz
	use jmod, only: j_err
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_feasible
	use jmod, only: p_objr
	double precision,dimension(1:p_nrowtot)::aopt,akey
	id=p_lx(p_lx0+1)
	!column new is generated for unit iunit and scheduel iopt using
	!optimal x-vector aopt and 	key vector akey updates lunit and isch
	p_lunit(id)=iunit
	p_isch(id)=iopt
	!	write(30,*)'col0 ',p_kier,p_unit,p_iopt,p_valuek,p_valueopt,p_vc
	!	p_colkey(p_id)=key
	!	p_enter=3
	!call updatel()
	!	id=p_lx(p_lx0+1)
	newa=p_nz+id
	p_a(p_abas(newa)+1:p_abas(newa)+p_nrow)=aopt(2:p_nrowtot)-akey(2:p_nrowtot)
	p_akey(p_abas1(id)+1:p_abas1(id)+p_nrowtot)=akey
	p_aopt(p_abas1(id)+1:p_abas1(id)+p_nrowtot)=aopt
 
 
	newc=id+p_nrowz
	if(p_isn16)write(p_n16,*)j_err,'entercol0 newc,id,iopt',newc,id,iopt
	if(p_isn16)write(p_n16,*)'a',99.d0,p_a(p_abas(newa)+1:p_abas(newa)+p_nrow)
	if(p_isn16)write(p_n16,'(a,(15f12.2))')'aopt',aopt
	if(p_isn16)write(p_n16,'(a,(15f12.2))')'akey',akey
 
	if(p_feasible)p_objr(newc)=aopt(1)-akey(1)
end subroutine


! subroutine keyfenter()

! write(6,*)'keyfenter p_ilog,p_inebas',p_ilog,p_inebas,'p_unit',p_unit,'p_lcur ',p_lcur
! p_ilog is first log tried to move to factory
! change keyfactories when it does change the basis
! this happens when there is no log in keyschedule or in schedules in basis
! lcur=p_lcur  ! link to first column in current unit

! p_nolog=.false.
! ibas=p_ibadat+(p_ns(p_unit)-1)*j_dnkeep
! loop:	do ilog=1,p_nlog
! if(j_dmat(ibas+p_keeplog(ilog)).ne.j_0)cycle

! do j=1,p_lx0
! iuni=p_lunit(lcur)
! if(iuni.ne.p_unit)cycle
! is1=p_isch(p_lx(j))-1

! if(j_dmat(ibadat+is1*j_dnkeep+p_keeplog(ilog)).ne.j_0)cycle loop

! lcur=p_next(lcur)
! enddo !j=1,p_lx0  11039
! p_nolog(ilog)=.true.
! iba=p_keyfactbas
! keyf=p_keyfact(p_keyfactbas+ilog)
! p_valueopt=p_small

!column enters

! if(p_feasible)then
! do k=1,p_knn
! ifact=p_neig(p_inebas+k)
! if(ifact.le.0)exit
! inde=p_ibafact(ilog)+p_factw(ifact)
! iterm=p_logfactterm0(inde)
! if(iterm.le.0)cycle

! j_dapu=p_neigu(p_inebas+k) !p_coef(iterm)*
! iterm=p_logfactterm(inde)
! if(iterm.le.0)cycle
! j_dapu=p_vc(p_rowofterm(iterm))* p_neigu(p_inebas+k) !*p_coef(iterm)

! if(ifact.eq.keyf)then
! p_keyfval=j_dapu
! elseif(j_dapu.gt.p_valueopt)then
! p_valueopt=j_dapu
! ifactopt=ifact
! endif !if(ifact.eq.keyf)  11068
! enddo !k=1,p_knn  11056
! if(p_valueopt.gt.p_keyfval)then
! p_keyfact(p_keyfactbas+ilog)=ifactopt

! endif !if(p_valueopt.gt.p_keyfval)  11075

! else
!not feasible
! do k=1,p_knn
! ifact=p_neig(p_inebas+k)
! if(ifact.le.0)exit
! inde=(ilog-1)*p_nfact+ifact
! iterm=p_logfactterm(inde)
! if(iterm.le.0)cycle


! j_dapu= p_neigu(p_inebas+k) !p_coef(iterm)*


! if(ifact.eq.keyf)then
! p_keyfval=j_dapu
! else
! if(p_objr2(irow).gt.0)then
! if(j_dapu.gt.p_valueopt)then
! p_valueopt=j_dapu
! ifactopt=ifact
! endif !if(j_dapu.gt.p_valueopt)  11097
! else
! if(j_dapu.lt.p_valueopt)then
! p_valueopt=j_dapu
! ifactopt=ifact
! endif !if(j_dapu.lt.p_valueopt)  11102

! endif !if(p_objr2(irow).gt.0)  11096
! endif !if(ifact.eq.keyf)  11093
! enddo !k=1,p_knn  11082
! if(irow.gt.0)then
! if(p_objr2(irow).gt.0)then
! if(p_valueopt.gt.p_keyfval)then
! p_keyfact(p_keyfactbas+ilog)=ifactopt

! endif !if(p_valueopt.gt.p_keyfval)  11112
! else
! if(p_valueopt.lt.p_keyfval)then
! p_keyfact(p_keyfactbas+ilog)=ifactopt

! endif !if(p_valueopt.lt.p_keyfval)  11117

! endif !if(p_objr2(irow).gt.0)  11111
! endif !if(irow.gt.0)  11110



! endif !if(p_feasible)  11055
! enddo loop !p:	do ilog=1,p_nlog  11036


! end subroutine keyfenter !subroutine fenter()tlepr
subroutine prin(title)
	use jmod, only: p_feasible
	use jmod, only: p_pivot
	use jmod, only: p_objf
	use jmod, only: p_objfv
	use jmod, only: p_enter
	use jmod, only: p_valueopt
	use jmod, only: p_valuek
	use jmod, only: p_unit
	use jmod, only: p_kier
	use jmod, only: p_keys
	use jmod, only: p_lr0
	use jmod, only: p_lr
	use jmod, only: p_xps
	use jmod, only: p_x
	use jmod, only: p_ls
	use jmod, only: p_nrow
	use jmod, only: p_lx0
	use jmod, only: p_lx
	use jmod, only: p_nrowz
	use jmod, only: p_lunit
	use jmod, only: p_isch
	use jmod, only: p_akey
	use jmod, only: p_abas1
	use jmod, only: p_nrowtot
	use jmod, only: p_aopt
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_objr
	use jmod, only: p_r
	use jmod, only: p_rhsw
	use jmod, only: p_lower
	use jmod, only: p_vc
	use jmod, only: p_vx
	use jmod, only: p_leavec
	use jmod, only: p_tmax
	use jmod, only: p_leavetype
	character*(*)title
	write(16,*)' '
	write(16,*)'**** ',title,' feas',p_feasible
	!	call testxps()
 
	write(16,*)'p_pivot',p_pivot,'obj',p_objf,'p_objfv',p_objfv,'ienter',&
		p_enter,'opt',p_valueopt,'key',p_valuek
	write(16,*)'unit ',p_unit,'kier',p_kier
	if(p_unit.gt.0)write(16,*)'key',p_keys(p_unit)
	write(16,*)'p_lr',p_lr0,' val ',p_lr(1:p_lr0)
	write(16,*)'xps ',p_xps(0:5)
	write(16,*)'x/ls',p_x(p_ls(1:p_nrow)),p_ls(1:p_nrow),' p_lx0',p_lx0
	if(p_lx0.gt.0)write(16,*)'x/lx',p_x(p_lx(1:p_lx0)+p_nrowz),p_lx(1:p_lx0),' p_lx0',p_lx0
	do ii=1,p_lx0
		jj=p_lx(ii)
		!	write(6,*)'x/lx',p_x(p_lx(1:p_lx0)+p_nrowz),p_lx(1:p_lx0),' p_lx0',p_lx0
		write(16,*)p_lunit(jj),p_isch(jj),'key',p_akey(p_abas1(jj)+1:p_abas1(jj)+p_nrowtot)
		write(16,*)'opt',p_aopt(p_abas1(jj)+1:p_abas1(jj)+p_nrowtot)
		write(16,*)'a',p_a(p_abas(jj)+1:p_abas(jj)+p_nrow)
		write(16,*)'objr',p_objr(p_nrowz+jj),p_aopt(p_abas1(jj)+1)-p_akey(p_abas1(jj)+1)
 
		if(p_x(p_lx(ii)+p_nrowz).gt.1)read(5,*)fjkjfj
	enddo !ii=1,p_lx0   7342
	write(16,*)'objrkok',p_objr
	write(16,*)'p_r ',p_r(p_ls(1:p_nrow))
	write(16,*)'objr ',p_objr(p_ls(1:p_nrow)),p_ls(1:p_nrow)
 
	!	write(16,*)'objr0 ',p_objr0(p_ls(1:p_nrow)),p_ls(1:p_nrow)
	write(16,*)'resa ',p_x(p_lr(1:p_lr0)),p_lr(1:p_lr0)
	if(p_lr0.gt.1)write(16,*)'sum',sum(abs(p_x(p_lr(1:p_lr0-1)))),'p_lr0 ',p_lr0
	write(16,*)'rhsw ',p_rhsw(1:5),'lower',p_lower
	write(16,*)'p_vc',0,p_vc
	write(16,*)'p_vx',p_vx
	!	write(16,*)'p_ix',p_ix
	write(16,*)'tmax',p_leavec,'p_tmax ',p_tmax,'p_enter',&
		p_enter,' p_leavetype ',p_leavetype,' p_leavec',p_leavec
 
 
 
 
 
 
end subroutine

subroutine unf()
	use jmod, only: j_0
	use jmod, only: p_infsum
	use jmod, only: p_nnf
	use jmod, only: p_lower
	use jmod, only: p_nrow
	use jmod, only: p_ubou
	use jmod, only: p_xps
	use jmod, only: p_rhs2
	use jmod, only: p_rhscur
	use jmod, only: p_rhs
	use jmod, only: p_lbou
	use jmod, only: p_feasible
	use jmod, only: p_nunits
	use jmod, only: j_dnkeep
	use jmod, only: p_keys
	use jmod, only: p_ntemp0
	use jmod, only: p_nterminrow
	use jmod, only: p_tablepart
	use jmod, only: p_istermx
	use jmod, only: p_xmat
	use jmod, only: p_ix
	use jmod, only: p_factpart
	use jmod, only: p_keyfact
	use jmod, only: p_logpart
	use jmod, only: j_dmat
	use jmod, only: p_keeplog
	use jmod, only: p_table
	use jmod, only: p_tablenlog
	use jmod, only: j_o
	use jmod, only: p_ns
	use jmod, only: p_nlog
	double precision infsum
	p_infsum=j_0
	p_nnf=0
	p_lower=.true.
	do irow=1,p_nrow
 
		if(p_ubou(irow))then
			if(p_xps(irow).gt.p_rhs2(irow))then
				p_infsum=p_infsum+p_xps(irow)-p_rhs2(irow)
				!		p_vc(irow)=-1.d0
				p_nnf=p_nnf+1
				p_rhscur(irow)=p_rhs(irow)
			endif !if(p_xps(irow).gt.p_rhs2(irow))   7381
			if(.not.p_lbou(irow))p_lower(irow)=.false.
		endif !if(p_ubou(irow))   7380
		! j_dapu3 infeasibility for best
		if(p_lbou(irow))then
			if(p_xps(irow).lt.p_rhs(irow))then
				p_infsum=p_infsum+p_rhs(irow)-p_xps(irow)
				p_nnf=p_nnf+1
				!		p_vc(irow)=1.0d0
				p_rhscur(irow)=p_rhs2(irow)
				p_lower(irow)=.false.
			endif !if(p_xps(irow).lt.p_rhs(irow))   7391
		endif !if(p_lbou(irow))   7390
	enddo !irow=1,p_nrow   7378
	p_feasible=p_nnf.eq.0
 
	ibaxmat0=0
	ibadat=0
	ibakeyf=0
	p_xps(0)=j_0
	do i=1,p_nunits
		!ibatablemat=(i-1)*nlog*nfact
		!j_yes=.true.
		!	do ir=0,p_nrow
		ibakey=ibadat+(p_keys(i)-1)*j_dnkeep
		ibaxmat=ibaxmat0+(p_keys(i)-1)*p_ntemp0
 
		! keyfact log1%fact1 log1%fact2 ..knn
		! for each unit p_nlogfact elements in p_keyfact
		!initially nearest factory is keyfact ibakeyf+(ilog-1)*p_knn
		!	do k=1,p_ns(i)
		!		irv=0
		do it=1,p_nterminrow(0)
			!		ir=p_rowofterm(it)
			itable=p_tablepart(it)
			!	if(i.eq.6)write(6,*)'i,it,ir,itable,p_isxvar(it)', i,it,ir,itable,p_istermx(it)
			if(p_istermx(it))then
				!		if(i8.le.2)write(6,*)'p_coef(it)*p_xmat(ibaxmat+1)',p_coef(it),p_xmat(ibaxmat+1)
				p_xps(0)=p_xps(0)+p_xmat(ibaxmat+p_ix(0))
				!			j_yes=.false.  ! xmat only once
				!	irv=ir
			elseif(p_factpart(it).ne.0)then
				!		if(i.eq.6)write(6,*)'it,p_factpart(it),ibakey,p_logpart(it),ibakey+p_logpart(it),keyf',&
				!		it,p_factpart(it),ibakey,p_logpart(it),ibakey+p_logpart(it)
				!	if(ibakeyf+(p_logpart(it)-1)*p_knn+1.gt.3120)write(6,*)'i,p_logpart(it),ibakeyf,(i-1)*p_nlogneig,kkk',&
				!		i,p_logpart(it),ibakeyf,(i-1)*p_nlogneig,ibakeyf+(p_logpart(it)-1)*p_knn+1
				if(p_factpart(it).eq.p_keyfact(ibakeyf+p_logpart(it)))&
					p_xps(0)=p_xps(0)+j_dmat(ibakey+p_keeplog(p_logpart(it)))
				!	if(ir.eq.70)write(17,*)i,p_logpart(it),ibakey,p_keeplog(p_logpart(it)),j_dmat(ibakey+p_keeplog(p_logpart(it))),p_xpsf(ir)
				!		if(p_logpart(it).eq.1)j_dapu5=j_dapu5+j_dmat(ibakey+p_keeplog(p_logpart(it)))
				!	if(i8.eq.6)write(6,*)'iseq',p_factpart(it),p_keyfact(ibakey+p_logpart(it)),ibakey,p_logpart(it)
				!	if(i.eq.6)write(6,*)'p_logpart(it),p_keeplog(p_logpart(it))',&
				!		p_logpart(it),p_keeplog(p_logpart(it)),ibakey+p_keeplog(p_logpart(it)),j_dmat(ibakey+p_keeplog(p_logpart(it)))
			elseif(itable.ne.0)then
				ivtable=p_table(itable)
				!	write(6,*)'i8,it,p_table(it),p_knn,ibaneigu,p_knn,p_kntot',i8,it,p_table(it),p_knn,ibaneigu,p_knn,p_kntot
				!	if(i8.le.2_8)write(6,*)'i8,k,it,p_coef(it),ibaneigu,ind,neigu ',&
				!		i8,k,it,p_coef(it),ibaneigu,ibaneigu+(p_table(it)-1)*p_knn+1,p_neigu(ibaneigu+(p_table(it)-1)*p_knn+1)
				!	write(6,*)i8,it,p_table(it)
				!	itabas=ibaneigu+(itable-1)*p_knn
				do ilog=1,p_tablenlog(itable)
 
					p_xps(0)=p_xps(0)+j_dmat(ibakey+j_o(ivtable)%i2(ilog))
					!	if(i.eq.6)write(6,*)'ilog,j_o(ivtable)%i2(ilog),ibadatamatkey,dmat',&
					!		ilog,j_o(ivtable)%i2(ilog),ibakey,j_dmat(ibakey+j_o(ivtable)%i2(ilog))
					!			write(6,*)i8,it,ilog,p_value,p_coef(it),ibaneigu,p_tableneibas(itable),&
					!				p_neigu(ibaneigu+p_tableneibas(itable)+1),&
					!				j_dmat(ibadatamat+j_o(ivtable)%i2(ilog)),ibadatamax
 
				enddo !ilog=1,p_tablenlog(itable)   7446
			endif !if(p_istermx(it))   7422
 
		enddo !it=1,p_nterminrow(0)   7418
 
		!	enddo !k=1,p_ns(i)  15360
		!ibaxmat=ibaxmat+p_ntemp0
 
		!	ibadatamat=ibadatamat+p_ns(i8)*j_dnkeep
		!	ibatable=ibatable+p_nlogfact
		!	if(i.eq.6)write(6,*)'ibaxmat,ibaneigu,ibatablecoeftot ',ibaxmat,ibaneigu,ibatablecoeftot
		ibadat=ibadat+p_ns(i)*j_dnkeep
		ibaxmat0=ibaxmat0+p_ns(i)*p_ntemp0
		ibakeyf=ibakeyf+p_nlog
	enddo !i=1,p_nunits   7406
	write(6,*)'lower',p_lower(1:p_nrow)
	write(6,*)'xps0',p_xps(0)
	do ir=1,p_nrow
		write(6,*)ir,p_rhs(ir),p_xps(ir),p_rhs2(ir)
	enddo !ir=1,p_nrow   7472
 
	!	write(16,*)'noneqconst ',nr, 'nonfrow',nnf,'infsum',infsum
 
end subroutine

subroutine unf0()
	use jmod, only: j_0
	use jmod, only: p_nrow
	use jmod, only: p_ebou
	use jmod, only: p_ubou
	use jmod, only: p_xps
	use jmod, only: p_rhs2
	use jmod, only: p_lbou
	use jmod, only: p_rhs
	double precision infsum
	infsum=j_0
	nnf=0
	nr=0
	do irow=1,p_nrow
		if(p_ebou(irow))cycle
		nr=nr+1
		if(p_ubou(irow))then
			if(p_xps(irow).gt.p_rhs2(irow))infsum=infsum+p_xps(irow)-p_rhs2(irow)
			if(p_xps(irow).gt.p_rhs2(irow))nnf=nnf+1
		endif !if(p_ubou(irow))   7488
		! j_dapu3 infeasibility for best
		if(p_lbou(irow))then
			if(p_xps(irow).lt.p_rhs(irow))infsum=infsum+p_rhs(irow)-p_xps(irow)
			if(p_xps(irow).lt.p_rhs(irow))nnf=nnf+1
		endif !if(p_lbou(irow))   7493
	enddo !irow=1,p_nrow   7485
	write(6,*)'noneqconst ',nr, 'nonfrow',nnf,'infsum',infsum
 
 
end subroutine


subroutine keyf(isopt)
	use jmod, only: j_dnkeep
	use jmod, only: p_ibadat
	use jmod, only: p_ibaxmat
	use jmod, only: p_ntemp0
	use jmod, only: p_keys
	use jmod, only: p_unit
	use jmod, only: p_nrow
	use jmod, only: p_ix
	use jmod, only: p_xps
	use jmod, only: p_xmat
	use jmod, only: p_nterm
	use jmod, only: p_rowofterm
	use jmod, only: p_logpart
	use jmod, only: j_dmat
	use jmod, only: p_keeplog
	!change key schedule update xps anfd xpsf
	ibanewdat=p_ibadat+(isopt-1)*j_dnkeep
	ibanewxmat=p_ibaxmat+(isopt-1)*p_ntemp0
	ibakey=p_ibadat+(p_keys(p_unit)-1)*j_dnkeep
	ibaxmat=p_ibaxmat+(p_keys(p_unit)-1)*p_ntemp0
	itot=0
	write(6,*)'p_unit,p_ibadat,ibanewdat,ibakey,p_keys(p_unit),isopt',p_unit,p_ibadat,ibanewdat,ibakey,p_keys(p_unit),isopt
	do ir=0,p_nrow
		!write(6,*)'ibanewdat,ibanewxmat,ibakey,ibaxmat',ibanewdat,ibanewxmat,ibakey,ibaxmat,'ix',p_ix(ir)
 
 
		if(p_ix(ir).gt.0)p_xps(ir)=p_xps(ir)+p_xmat(ibanewxmat+p_ix(ir))-p_xmat(ibaxmat+p_ix(ir))
 
 
	enddo !ir=0,p_nrow   7512
 
	do iterm=1,p_nterm
		ir=p_rowofterm(iterm)
		ilog=p_logpart(iterm)
 
		if(ilog.ne.0)then
			p_xps(ir)=p_xps(ir)+j_dmat(ibanewdat+p_keeplog(ilog))-j_dmat(ibakey+p_keeplog(ilog))
		endif !if(ilog.ne.0)   7525
	enddo !iterm=1,p_nterm   7521
 
 
	p_keys(p_unit)=isopt
end subroutine keyf


subroutine zeroacol(icol) !make column of a zero
	use jmod, only: p_nrow
	use jmod, only: j_0
	use jmod, only: p_a
	iel=icol
	do i=1,p_nrow
		p_a(iel)=j_0
		iel=iel+p_nrow
	enddo !i=1,p_nrow   7537
 
 
end subroutine

! subroutine entercol(newc)
! double precision piecexmat0,area
! !when a schedule enters this computes the entering  column of the a-matrix
! ! and computes also the objective row element
! ! with factories j_valueopt_af computed with fenter0 is used both for the
! ! objective row and for the entire column
! !käyttä p_iopt mikä globaaliksi

! ! j_valueopt_af computed with fenter0 is needed here toupdate the object row
! !	ld01=p_lx0+1  !!!! get next free column in a,
! ! ld0 is the number of used (basic) cols in D-part i.e. after z-cols
! !	p_newd=p_lx(ld01)
! !	p_newa=p_newd+p_nz   ! after p_nz
! !	newc=p_newa+p_nrow  !in Fletecher cols, I part (residuals) are counted also
! !	if(p_p) write(p_n16,*)'ent sched: p_newd',p_newd,'newc',newc, 'tol=',p_tolecur
! ! get key schdeule
! iobs= p_ibaunit(p_unit)+p_keys(p_unit)
! iobsopt=p_ibaunit(p_unit)+p_iopt
! ibaxmat=ibamatx(iobs) !,1)
! ibaxmat2=ibamatx(iobsopt) !,2)
! !	p_lunit(p_newd)=p_unit
! !	p_isch(p_newd)=p_iopt
! !	p_colkey(p_newd)=p_keys(p_unit)
! ! 6.42  a0'D , here we are prepared that the same x can be in differen rows
! ! how does the object variable change
! !!!! put key schedule first int objr and a matrix

! ! update objr0
! if(p_ix(0).ne.0)then  !then p_ixcur(0))then
! !	p_objr(newc)=p_xmat(p_ix(0)+ibaxmat2)-p_xmat(p_ix(0)+ibaxmat) !v(ix(0))
! p_objrnewc=p_xmat(p_ix(0)+ibaxmat2)-p_xmat(p_ix(0)+ibaxmat)
! p_i1=2
! else !if(j_ixcur(0).ne.0)then
! p_i1=1
! p_objrnewc=j_0
! p_objrnewc=j_0
! endif !if(p_ix(0).ne.0)  11828
! !write(6,*)'objr ', p_objr0(newc)
! if(p_ispiece)then !update objr0
! j_dapu=dot_product(p_pieceprice(1:p_npiece),j_dmat((iobsopt-1)*j_dnkeep+p_piecekeep) )
! !	write(6,*)'dapu',j_dapu,'price ',p_pieceprice(1:p_npiece),' damat',j_dmat((iobs-1)*j_dnkeep+p_piecekeep),'iobs',iobs
! if(p_isarea)j_dapu=j_dapu*area(p_unit)
! !	write(6,*)'afdapu',j_dapu
! j_dapu2=dot_product(p_pieceprice(1:p_npiece),j_dmat((iobs-1)*j_dnkeep+p_piecekeep) )
! !write(6,*)'dapu2',j_dapu2,'price  damat',j_dmat((iobs-1)*j_dnkeep+p_piecekeep),'iobs',iobs
! p_objr(newc)=p_objr(newc)+j_dapu-j_dapu2
! !	write(6,*)'objr now ', p_objr0(newc)
! endif !if(p_ispiece)  11837

! !	p_a(p_abas(p_newa)+1:p_abas(p_newa)+p_nrow)=j_0 !p_a(1:p_nrow,p_newa)=j_0
! p_anew=j_0
! !if(p_p)write(p_n16,*)p_row0,p_nrowcurx,p_rowcurx(p_row0:p_nrowcurx),'pix',p_ix
! !if(p_p)write(p_n16,*)'p_ntemp0',p_ntemp0,'iba',ibaxmat2,'iobsop',iobsopt,'iopt',p_iopt,'iunit',p_unit
! !if(p_p)write(p_n16,*)p_xmat(ibaxmat2+1:ibaxmat2+p_ntemp0),p_xmat(ibaxmat2+1:ibaxmat2+p_ntemp0)
! do iro=p_row0,p_nrowcurx
! if(p_p)write(p_n16,*)'iro',iro,p_rowcurx(iro),p_ix(p_rowcurx(iro)),p_xmat(ibaxmat2+p_ix(p_rowcurx(iro)))
! !	p_a(p_rowcurx(iro),p_newa)=p_xmat(ibaxmat2+p_ix(p_rowcurx(iro)))-p_xmat(ibaxmat+p_ix(p_rowcurx(iro)))
! !	p_a(p_abas(p_newa)+p_rowcurx(iro))=p_xmat(ibaxmat2+p_ix(p_rowcurx(iro)))-p_xmat(ibaxmat+p_ix(p_rowcurx(iro)))
! p_anewa(p_rowcurx(iro))=p_xmat(ibaxmat2+p_ix(p_rowcurx(iro)))-p_xmat(ibaxmat+p_ix(p_rowcurx(iro)))
! enddo !iro=p_row0,p_nrowcurx  11852

! p_tmax=j_one	;p_rcur=p_tmax	! myöhemmin pintala
! !	p_enter=3	!!!! scedule enters




! end subroutine entercol !subroutine entercol(newc)

subroutine leavenew()
	use jmod, only: p_leavek
	use jmod, only: p_oldc
	use jmod, only: p_leavetype
	use jmod, only: p_post
	use jmod, only: p_r
	use jmod, only: p_ls
	use jmod, only: p_nrow
	use jmod, only: j_err
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_enter
	use jmod, only: p_isz
	use jmod, only: p_tmax
	use jmod, only: p_xpresent
	use jmod, only: p_lx0
	use jmod, only: p_lx
	use jmod, only: p_tmaxmin
	! at this point p_r must be updated
	! at output if p-leavetype.eq.0 then no pivoting is done
	! otherwise the calling program does pivoting
	! if residual or z-vaaible is entering then newc tells the entering column
	! if it is necessary to do pivoting due to changes of keyschduels these are done here
	! if schdules is entering the senter() needs to
	!	p_leave=0  !!!!position in ls
	!key
	!	p_p=.true.
	p_leavek=0  !!!! leaving key schedule
	p_oldc=0
	!	p_leavec=0 !!!! leaving column
	p_leavetype=0
	!	call leftvec(p_anew,p_r)
	if(.not.p_post)p_r(p_ls(1:p_nrow))=-p_r(p_ls(1:p_nrow))
	if(p_isn16)write(p_n16,*)j_err,'ienter in leavenew',p_enter
	if(p_isz)then
		if(p_isn16)write(p_n16,*)j_err,'  bef zleavtmax',p_tmax
		call zleavtmax()   !p_oldc)
		if(p_isn16)write(p_n16,*)'  aft zleavtmax',p_tmax,p_oldc
	endif !if(p_isz)   7631
	if(p_isn16.and.p_xpresent)write(p_n16,*)'   bef sleavtmax',p_tmax,'p_lx0,p_lx',p_lx0,p_lx(1:p_lx0)
	if(p_xpresent)call sleavtmax()  !p_oldc)
	if(p_isn16.and.p_xpresent)write(p_n16,*)j_err,'  aft sleav tmax ',p_tmax,'p_tmaxmin ',p_tmaxmin
 
	call rleavtmax()  !p_oldc)
	! if(p_rmustleave)then
	! if(p_isn16)write(p_n16,*)'***rmustleave oldc',p_oldc,'p_r(irow)',p_r(p_oldc)
 
	! p_leavetype=1
 
	! endif !if(p_rmustleave)   7438
	!write(p_n16,*)'    tmax aft rleav ',p_tmax,p_leavetype,'p_oldc',p_oldc
 
	if(p_xpresent)call keyleavtmax()   !p_oldc and p_oldx
	! leavek is column whose key must be cahnged
	! there are following cases
	! 1 there is only one column with the same unit and the unit is not the same unit as the the unit
	!    of the entering schdule. Then this case can be seen as the case where the role of key and opt is
	!changed and the column is dropped. Thus this column is just pivoted out
	!!
	!write(p_n16,*)'coming from leavenew',p_oldc
	return
	! integer,parameter::p_re=1
 
	!	call pivotnow(p_oldc,newc)   !leavenew
 
 
end subroutine leavenew


! subroutine leaving()

! !

subroutine zleavtmax()   !p_oldc)
	use jmod, only: j_yes
	use jmod, only: j_err
	use jmod, only: p_n16
	use jmod, only: p_tmax
	use jmod, only: p_lz0
	use jmod, only: p_nrow
	use jmod, only: p_lz
	use jmod, only: p_x
	use jmod, only: p_r
	use jmod, only: p_tiny78
	use jmod, only: p_oldc
	use jmod, only: p_leavetype
	!computes
	!	use fletdmod
	j_yes=.false.
	write(p_n16,*)j_err,'**zleavtmax old',p_tmax
	!write(6,*)'lz',p_lz
	!write(6,*)'x',p_x,'p_nrow'
	!	write(6,*)'r',p_r
	do j=1,p_lz0  !!!! z leaving
		!same as p_lr0 but now x cannot be negative
		ico=p_nrow+p_lz(j)
		write(p_n16,*)'ico ',ico,'x',p_x(ico),'r',p_r(ico),'p_tmax*p_r(ico)',p_tmax*p_r(ico)
		if(p_r(ico).gt.p_tiny78)then
			if(p_x(ico).lt.p_tmax*p_r(ico))then
				p_tmax=p_x(ico)/p_r(ico) !  ;p_rcur=p_r(ico)
				p_oldc=ico
				j_yes=.true.
				write(p_n16,*)'tmax aft zleav ',p_tmax,'icold ',p_oldc
			endif !if(p_x(ico).lt.p_tmax*p_r(ico))   7683
		endif !if(p_r(ico).gt.p_tiny78)   7682
	enddo !j=1,p_lz0   7678
	if(j_yes)p_leavetype=2   !p_ze
 
end subroutine zleavtmax

subroutine sleavtmax()  !p_oldc and p_oldx
	use jmod, only: p_isn16
	use jmod, only: j_err
	use jmod, only: p_enter
	use jmod, only: p_tmax
	use jmod, only: p_lx
	use jmod, only: p_lx0
	use jmod, only: p_r
	use jmod, only: p_lunit
	use jmod, only: p_x
	use jmod, only: p_nrowz
	use jmod, only: p_vc
	use jmod, only: p_xps
	use jmod, only: p_tiny78
	use jmod, only: j_yes
	use jmod, only: p_oldc
	use jmod, only: p_n16
	use jmod, only: j_0
	use jmod, only: p_leavetype
	use jmod, only: p_oldx
	!	use fletdmod
	!	j_dapu5=p_tmax
	if(p_isn16)then
		write(16,*)' '
		write(16,*)j_err,'sleavtmax ienter',p_enter,' p_tmax',p_tmax
		write(16,'(a,(15i7))')'p_lx',p_lx(1:p_lx0)
		write(16,'(a,(15f12.3))')'p_r',p_r(1:p_lx0)
		write(16,'(a,(15i7))')'p_lunit',p_lunit(p_lx(1:p_lx0))
		write(16,*)'p_x(lx)            ',p_x(p_lx(1:p_lx0)+p_nrowz)
		write(16,*)'p_vc          ',p_vc
		write(16,*)'xps',p_xps
		write(16,*)'p_tiny78',p_tiny78
	endif !if(p_isn16)   7698
	j_yes=.false.
	do j=1,p_lx0  !!!!sched leaving
		!same as lz
		!		if(p_pivot.gt.76652)write(6,*)'pi vot ',p_pivot
		ico=p_nrowz+p_lx(j)
		!	if(p_pivot.gt.76652)write(6,*)'pivot ',p_pivot ,'j lead ',j,p_lead,r(p_lead),p_tiny78
		if(p_r(ico).gt.p_tiny78)then     !
			!		if(p_pivot.gt.76652)write(6,*)'tas ',p_x(p_lead),p_tmax,r(p_lead),p_tmax*r(p_lead),p_x(p_lead).lt.p_tmax*r(p_lead)
			if(p_x(ico).lt.p_tmax*p_r(ico))then
				p_tmax=p_x(ico)/p_r(ico)  !;p_rcur=p_r(p_lead)
				j_yes=.true.
				p_oldc=ico !!!!
				if(p_isn16)write(p_n16,*)'sleavtmax',p_tmax,'p*lead ',p_oldc
				if(p_tmax.lt.j_0)then
					write(6,*)'sleavtmax<0 ',p_tmax,'p*lead ',p_oldc
					!		j_err=.true.;return !if(p_tmax.lt.j_0)read(5,*)djjd
				endif !if(p_tmax.lt.j_0)   7722
				!			if(p_pivot.gt.76652)write(6,*)'tmax ',p_tmax,p_rcur,p_leavec,'piunit',p_unit
			endif !if(p_x(ico).lt.p_tmax*p_r(ico))   7717
 
 
 
		endif !if(p_r(ico).gt.p_tiny78)   7715
	enddo !j=1,p_lx0   7710
	if(j_yes)then
		p_leavetype=3  !p_se
		p_oldx=p_oldc-p_nrowz
	endif !if(j_yes)   7733
	if(p_isn16.and.j_yes)write(16,*)j_err,'sleaves,tmax,p_oldc',p_tmax,p_oldc
 
end subroutine sleavtmax


subroutine rleavtmax()
	use jmod, only: j_yes
	use jmod, only: p_rmustleave
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: j_err
	use jmod, only: p_lower
	use jmod, only: p_lr
	use jmod, only: p_lr0
	use jmod, only: p_r
	use jmod, only: p_x
	use jmod, only: p_tmax
	use jmod, only: j_1
	use jmod, only: p_bbou
	use jmod, only: p_oldc
	use jmod, only: p_rhsdif
	use jmod, only: p_rhscur
	use jmod, only: p_rhs2
	use jmod, only: p_rhsw
	use jmod, only: p_rhs
	use jmod, only: p_leavetype
	double precision x2,sr,sx,sx2,ar,ax
 
	!	use fletdmod
	!	j_dapu5=p_tmax
	!	write(16,*)'rleavtmax bef',p_tmax,p_lr0,'P-r',p_r
	!write(6,*)'rleavtmax',p_tmax,p_lr0
	!if(.not.p_rdone)call leftvec(p_anew0(1:p_nrow),p_r)
	j_yes=.false.
	p_rmustleave=.false.
	if(p_isn16)then
		write(p_n16,*)' '
		write(p_n16,*)j_err,'*rleavtmax p_lower',p_lower(p_lr(1:p_lr0))
		write(p_n16,*)'*rleavtmax p_lr',p_lr(1:p_lr0)
		write(p_n16,*)'*rleavtmax p_r',p_r(p_lr(1:p_lr0))
		write(p_n16,*)'*rleavtmax p_x',p_x(p_lr(1:p_lr0))
		!	write(16,*)'*rleavtmax p_ubou',p_ubou
		write(p_n16,*)'rleavtmax p_tmax',p_tmax
	endif !if(p_isn16)   7752
	do j=1,p_lr0 !!!!
		! t>0 ==post   r>0 x decreases, r<0 x increases
		! x'=x-t*r >= 0
		! if r>0  and t >0 and x>0
		! x'=x-t*r >0  -> t< x/r
		! current max tmax, thus this new will become binding if x/r<tmax
		! that is x<tmax*r  (in testing division by r is not good becasue it can
		! p_lead to overflow)
		! if r>=0  and t >0 and x<=0 then x becomes more negative, or remains
		! if r<=0  and t >0 and x>=0 then x becomes more positive,or remains
		! if r<0  and t >0 and x<=0 then x becomes zero if t=x/r <tmax
		! if r>0  and t >0 and x>=0 then x becomes zero if t=x/r <tmax
		!this can be tested old
		! abs(x)<tmax*abs(r)
 
		irow=p_lr(j)
 
		!	write(16,*)j,p_r(irow)
		!		if(r(p_lr(j)).gt.p_tiny78)then !tole(lr(j)))then
		ar=abs(p_r(irow))
		ax=abs(p_x(irow))
 
 
		! if(p_ebou(irow))then
		! if(ax.lt.p_tiny78)then
		! p_rmustleave=.true.
		! p_tmax=-1.d0
		! p_oldc=irow
		! if(p_isn16)write(p_n16,*)'res ',irow,' must leave'
		! return
		! endif !if(ax.lt.p_tiny78)   7576
 
 
		! endif !if(p_ebou(irow))   7575
		sr=sign(j_1,p_r(irow))
		sx=sign(j_1,p_x(irow))
		if((sx.ne.sr.and.ax.gt.1.d-10).or.ar.lt.1.d-11)cycle
		if(.not.p_bbou(irow))then
			if(ax.lt.ar*p_tmax)then
				p_tmax=p_x(irow)/p_r(irow)
				p_oldc=irow
				j_yes=.true.
 
			endif !if(ax.lt.ar*p_tmax)   7799
 
			cycle
		endif !if(.not.p_bbou(irow))   7798
 
 
		if(p_lower(irow))then
			x2=p_x(irow)-p_rhsdif(irow)
			!	p_tmax=p_x(irow)/p_r(irow)  The greater tmax the better
 
		else
			x2=p_x(irow)+p_rhsdif(irow)
 
		endif !if(p_lower(irow))   7810
		sx2=sign(j_1,x2)
 
 
 
		if(sx2.ne.sr)then
			if(p_lower(irow))then
				p_rhscur(irow)=p_rhs2(irow)
				p_rhsw(irow)=p_rhsw(irow)+p_rhsdif(irow)
			else
				p_rhscur(irow)=p_rhs(irow)
				p_rhsw(irow)=p_rhsw(irow)-p_rhsdif(irow)
 
			endif !if(p_lower(irow))   7823
			p_x(irow)=x2
			cycle
		endif !if(sx2.ne.sr)   7822
 
		if(ax.lt.abs(x2))then
			if(ax.lt.ar*p_tmax)then
				p_tmax=p_x(irow)/p_r(irow)
				p_oldc=irow
				j_yes=.true.
 
			endif !if(ax.lt.ar*p_tmax)   7836
 
			cycle
 
		else
			if(abs(x2).lt.ar*p_tmax)then
				p_tmax=x2/p_r(irow)
				p_oldc=irow
				j_yes=.true.
 
			endif !if(abs(x2).lt.ar*p_tmax)   7846
			if(p_lower(irow))then
				p_rhscur(irow)=p_rhs2(irow)
				p_rhsw(irow)=p_rhsw(irow)+p_rhsdif(irow)
 
			else
				p_rhscur(irow)=p_rhs(irow)
				p_rhsw(irow)=p_rhsw(irow)-p_rhsdif(irow)
			endif !if(p_lower(irow))   7852
			p_x(irow)=x2
 
 
 
		endif !if(ax.lt.abs(x2))   7835
 
 
 
 
 
 
 
	enddo !j=1,p_lr0   7761
 
 
	!if(p_isn16)write(p_n16,*)j_err,'RHSCUR',p_rhscur
	!residual increases
	!uusi yritys
	! residuaali siis pienenee
	! tarkastellaan ensin tilannetta, että on yläraja olemassa ja
	! se on aktiivinen (lower(lr(j)).eq.False ja rhscur=rhs2
	! row+resid=rhs2
	!jos resid<0 row>rhs2 , tehtävä on nonfeasible ja residuaalin pieneneminen
	!tekee siitä enemmän nonfeasiblen, mutta tämä on vain optimaalista eikä uutta
	! tmaxin:n arvoa saada tätä kautta
 
	! jos resid>0 niin row<rhs2.
	! jos resid>0 ja resid pienee (resid'=resid-t*r)
	! yläraja tulee sitovaksi kun resid'=0, eli t=resid/r
 
	! tarkastellaan sitten tilannetta että yläraja on olemassa mutta alaraja on aktiivinen
	! rhscur=rhs,
 
	! row +resid=rhs
 
	! Jos resid<0 row>rhs. Jos residuaali pienenee eli tulee negatiivisemmaksi,
	! niin row kasvaa ja voi tulla ylärajan suuruiseksi. Tämä tapahtuu kun,
	! resid'=rhs-rhs2 eli resid-t*r=rhs-rhs2, eli t=(resid+rhs2-rhs)/r
	!testataan siis onko (resid+rhs2-rhs)/r<tmax eli (resid+rhs2-rhs)<r*tmax
	!( jos (resid+rhs2-rhs)<0 ollaan jo valmiiksi ylärajan yläpuolella.
	! Tätä tilannetta ei pitäisi tulla.)
	! vaihdetaan yläraja aktiiviseksi eli lasketaan resid=resid+rhs2-rhs ja
	!laitetaan rhscur=rhs2
 
	! jos resid>0 niin row<rhs eli kyseessä on infeasible ja tehtävä tulee feasibleksi kun
	! resid menee nollaksi eli resid-t*r=0, eli t=resid/r
	!mutta jos tämä tilanne tulee vastaan voidaan antaa residuaalin pienetä aina arvoon
	! rhs-rhs2 jolloin tulee yläraja vastaan
	! resid'=resid-t*r=rhs-rhs2 eli t=(resid+rhs2-rhs)/r
	!eli tämä tilanne menee samoin kuin edellinen
 
	!jos alaraja on olemassa ja ylärajaa ei ole olemassa lower=T ja rhscur=rhs
	! row+resid=rhs
	! jos resid<0 niin row>rhs ja tulee vielä suuremmaksi, ei tarvitse välittää
	!jos resid>0 row<rhs row tulee ensin rhs:n suuruiseksi ja jatkaa sitten
	!tällöinkään ei tarvitse pysäyttää
 
 
	if(j_yes)p_leavetype=1  !p_re
 
	! if(j_yes.and.p_isn16)then
	! write(p_n16,*)'p_tmax,resleaves',p_tmax,p_leavetype,'p_oldc',p_oldc
	! call rint('*renter ',p_n16)
	! endif !if(j_yes.and.p_isn16)   7763
 
	!	write(6,*)'p_tmax,p_leavetype',p_tmax,p_leavetype
	!read(5,*)rr
	if(p_isn16)write(p_n16,*)'return from rleavtmax ',p_tmax,'leavetype',p_leavetype
 
end subroutine rleavtmax

subroutine keyleavtmax()
	use jmod, only: j_err
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_tmax
	use jmod, only: p_enter
	use jmod, only: p_unit
	use jmod, only: j_onen
	use jmod, only: p_wsu
	use jmod, only: p_rs
	use jmod, only: p_zero
	use jmod, only: j_dapu5
	use jmod, only: p_next
	use jmod, only: p_nsame
	use jmod, only: j_yes2
	use jmod, only: p_lx0
	use jmod, only: p_lunit
	use jmod, only: p_r
	use jmod, only: p_nrowz
	use jmod, only: p_x
	use jmod, only: j_one
	use jmod, only: p_tiny78n
	use jmod, only: p_tiny78
	use jmod, only: p_oldx
	use jmod, only: p_leavetype
	use jmod, only: p_oldc
	integer oldx
	!	use fletdmod
 
 
	!	p_linktofirst=0 !!!! place where to put new entering schdelu if the same unit
	! newc -is coming column , p_newa in A, p_newd in D
	! tmax gives max values for enetering var.
 
	! update step
	! determine leaving variable  c:new column, direct effect
	! Bx=b  ,  Bx+tc=b
	! x'=x- t*inv(B)*c=x-t*r
	! key p_leaves, see p. 115-116
	! if entering unit is not the same,
	! sum(x')=1,  sum(x)-t*sum(inv(b)*c) = 1  ->t=(sum(x)-1)/sum(r)  * if (sum(x)-1<0
	! -> only negative  sum(r) gives possible values, summation over cols in the same unit
 
	! if the unit is the same as entering unit then
	! sum(x')+t=1, sum(x)-t*sum(inv(b)*c)+t = 1->t=(sum(x)-1)/(sum(r)-1.)
	! ei varauduttu vielä negatii viseen t:
 
	! note elements of r have opposite sign as in old JLP
	!	write(61,*)'**leavekeytmax bef',p_tmax
	!write(16,*)'**leavekeytmax bef',p_tmax,'p_lcur', plcur,'p_unit',p_unit,'p_lx',p_lx(1:p_lx0),'ienter',p_enter
	! link to first column where key schdele is cahnged
	if(p_isn16)write(p_n16,*)j_err,'into keyleavtmax ',p_tmax,'enter',p_enter,'unit',p_unit
	p_wsu=j_onen
	p_rs=p_zero
	j_dapu5=p_tmax
	lcur=p_next(0)  !column
	!leavek=lcur
	!	p_linkent=0
	p_nsame=0
	nsame=0
	!_yes=p_enter(3)
	j_yes2=.false.
 
	! lunit etc column number in A
	!if(p_pp)write(p_n16,*)'units',j_lunit(j_next(0)),j_lunit(j_next(j_next(0))),j_lunit(j_next(j_next(j_next(0)))),&
	!j_lunit( j_next(j_next(j_next(j_next(0)))))
	! p_nsameunit=0
	! go through units in next order
 
	!if(p_isn16)write(16,*)'**leavekeytmax bef ',p_tmax,'lcur', lcur,'p_unit',p_unit,'p_lx',p_lx(1:p_lx0)
	!	if(p_isn16)write(16,*)'ienter',p_enter
	!	write(16,*)'  next*',p_next
	!write(16,*)'lunit',p_lunit,'lcur',lcur
	lunitold=0
	do i=1,p_lx0
		lunitcur=p_lunit(lcur)
		if(lunitcur.ne.lunitold)then
			nsame=0
			oldx=lcur
		endif !if(lunitcur.ne.lunitold)   7982
 
		!	lunitold=lunitcur
		!write(16,*)'  i,lcur',i,lcur,'lunitlcur',lunitcur,'unit now',p_unit,'lunitold',lunitold
		nex=p_next(lcur)        ! next  follows numbering in D
 
		p_rs=p_rs+p_r(lcur+p_nrowz)  ! has opposite sign than in old JLP
		p_wsu=p_wsu+p_x(lcur+p_nrowz)
 
		nsame=nsame+1
		!	if(p_isn16)write(16,*)'  nex',nex,'rs',p_rs,p_wsu,p_r(lcur+p_nrowz),'nsame',nsame
		!f(j_yes)then
		!	if(lunitcur.eq.p_unit)p_linkent=lcur
		!_yes=.false.
		!endif !if(j_yes)   7878
		!	if(p_lunit(p_lcur).eq.p_unit.and.p_linktofirst.eq.0.and.p_enter.eq.3)p_linktofirst=p_lcur
		lunitnex=0
		if(nex.ne.0)lunitnex=p_lunit(nex)
		if(lunitnex.ne.lunitcur)then
			! the next col has differen t unit, do final checking
			! formulas  6.59  6.60
			if(lunitcur.eq.p_unit.and.p_enter(3))p_rs=p_rs-j_one
			!	if(p_isn16)write(16,*)'  p_rs,p_wsu,p_tmax,p_rs,tmax*rs',p_rs,p_wsu,p_tmax,p_tmax*p_rs,p_wsu.gt.p_tmax*p_rs
			!	if(p_rs.ne.j_0)write(16,*)'pottmax',p_wsu/p_rs
			if(p_rs.lt.p_tiny78n.and.p_wsu+p_tiny78.ge.p_tmax*p_rs)then !!!!
				! wsu negat  wsu/rs< tmax  rs<0 =>wsu>tmax*rs
				!					t=wsu/rs
				p_tmax=p_wsu/p_rs   ! ;p_rcur=p_rs!!!!
				!	p_leavek=leavek
				p_oldx=oldx
				p_nsame=nsame
				j_yes2=.true.
				!if(p_pp)write(p_n16,*)'**tmax/key,wsu,rs',j_tmax,j_wsu,j_rs,' unit, p_lcur',j_lunit(p_lcur),p_lcur
				! p_leavek is the first D-column in unit for whcik key is leaving
				!					end if
			endif !if(p_rs.lt.p_tiny78n.and.p_wsu+p_tiny78.ge.p_tmax*p_rs)   8009
 
			p_wsu=j_onen   !opposite sign of ws as in old JLP
			p_rs=p_zero
			! p_lcur0 is again the first col in the next new unit
 
			!		leavek=nex
			! last
		endif !if(lunitnex.ne.lunitcur)   8003
		!p_leavek is the column whose key is elaving
		lunitold=lunitcur
		lcur=nex
		!	write(16,*)' lcur,lunitold,',lcur,lunitold
	enddo !i=1,p_lx0   7980
	!	if(j_yes2)write(16,*)'key leaves,p_leavek ',p_oldx,'tmax',p_tmax
	if(j_yes2)then
		p_leavetype=4  !p_sk
		p_oldc=p_oldx+p_nrowz
	endif !if(j_yes2)   8035
	if(p_isn16)write(p_n16,*)j_err,'return keyleavtmax** ',p_leavetype,'p_tmax',p_tmax,'p_nsame',p_nsame,'oldc',p_oldc
	if(p_isn16)write(16,*)'  '
 
 
end subroutine keyleavtmax





subroutine leavekey()  ! p_oldx
	use jmod, only: j_err
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_oldx
	use jmod, only: p_nsame
	use jmod, only: p_lunit
	use jmod, only: j_dapu3
	use jmod, only: j_ninf
	use jmod, only: j_dapu4
	use jmod, only: p_x
	use jmod, only: p_nrowz
	use jmod, only: p_tmax
	use jmod, only: p_r
	use jmod, only: p_next
	use jmod, only: p_xps
	use jmod, only: p_nrow
	use jmod, only: p_akey
	use jmod, only: p_abas1
	use jmod, only: p_nrowtot
	use jmod, only: p_aopt
	use jmod, only: p_rhsw
	use jmod, only: p_rhscur
	use jmod, only: p_keys
	use jmod, only: p_isch
	use jmod, only: p_unit
	use jmod, only: p_akey00
	use jmod, only: p_oldc
	use jmod, only: p_lx
	use jmod, only: p_lx0
	use jmod, only: p_enter
	use jmod, only: p_akey0
	use jmod, only: p_anew0
	use jmod, only: p_aopt0
	! keyschedule leaves
	! there are two cases: there are no explicit basic schedules in the unit and there are
 
	use fletdmod
	use fletdmod2
	use fletcherdmod
	double precision area
	!	double precision,dimension(:),pointer::akey
	common/refactorc/nup,nfreq
 
	! write(16,*)' '
	! write(16,*)'leavekey************** oldc ',p_oldc
	! write(16,*)'p_oldx,p_unit,p_enter',p_oldx,p_unit,p_enter0,p_enter1,p_enter2,p_enter3
 
	! write(16,*)'nsame',p_nsame,'tmax',p_tmax,'biwcol'
	! write(16,*)'lunit',p_lunit(p_oldx)
 
	!there following cases when weight of key schedule becomes 	zero
	!the new key will become the optimal scedule in column ibig
	!if the schdeule entering and the entering schedule has the same unit
	! as the leaving column
	! then the key schdules neeeds to be changed first in the entering schedule also
	!theafter first the column p_bigwcol is pivoted out
	! thereafter p_nsame-1 columns are pivoted out so that in the entering column the
	! optimal schedule is as before but the key is cahnged
	if(p_isn16)write(p_n16,*)j_err,'*into: leavekey ,oldx',p_oldx,'nsame',p_nsame
	! write(p_n16,*)'*sub leavekey, p-oldx,p_nsame',p_oldx,p_nsame
	! write(p_n16,*)'next',p_next
	! write(p_n16,*)'unit ',p_unit,'key',p_keys(p_unit)
	! write(p_n16,*)'lunit ',p_lunit,'anew,opt,ke'
	! write(p_n16,'(20f11.1)')p_anew0
	! write(p_n16,'(20f11.1)')p_aopt0
	! write(p_n16,'(20f11.1)')p_akey0
	! call rint('leavkeystart',p_n16)
	! endif !if(p_isn16)   7910
 
 
 
	if(p_nsame.gt.1)then
		lcur=p_oldx
		lunold=p_lunit(p_oldx)
		j_dapu3=j_ninf
 
		! the schedule with largest weight becomes new key
 
		do i=1,p_nsame
			if(p_lunit(lcur).ne.lunold)then
				write(6,*)'**lunold,luncur',lunold,p_lunit(lcur)
				J_err=.true.
 
			endif !if(p_lunit(lcur).ne.lunold)   8096
			j_dapu4=p_x(lcur+p_nrowz)+p_tmax*p_r(lcur+p_nrowz)
			!		if(p_isn16)write(p_n16,*)'isame',i,'val ',j_dapu4,'curmax',j_dapu3,'lcur',lcur
			if(j_dapu4.gt.j_dapu3)then
				j_dapu3=j_dapu4
				ibig=lcur
			endif !if(j_dapu4.gt.j_dapu3)   8103
			lcur=p_next(lcur)
 
		enddo !i=1,p_nsame   8095
	else
		ibig=p_oldx
	endif !if(p_nsame.gt.1)   8088
	!iunit=p_lunit(p_oldx)
	!update here because 	these may change later
 
	p_xps(0:p_nrow)=p_xps(0:p_nrow)-p_akey(p_abas1(p_oldx)+1:p_abas1(p_oldx)+p_nrowtot)+ &
		p_aopt(p_abas1(ibig)+1:p_abas1(ibig)+p_nrowtot)
	p_rhsw=p_rhscur-p_xps(1:p_nrow)
	p_keys(p_lunit(p_oldx))=p_isch(ibig)
 
	if(p_isn16)write(p_n16,*)'ibig',ibig,'lunitibig/old',p_lunit(ibig),p_lunit(p_oldx),'p_unit',p_unit
 
	!the schedule with largest weight becomes new key
	! all other columns are pivoted out so that new column is equal to the old
	! schedule - the new key
	!	p_akey0=p_aopt(p_abas1(ibig)+1:p_abas1(ibig)+p_nrowtot)
 
	!!!write(16,*)'ibig',ibig
	lcur=p_oldx
	!	if(p_isn16)write(16,*)'lcur,ibig',lcur,ibig,'newc',newc,'nsame ',p_nsame
	! it must be taken into account that p_aopt(ibig) will change
	! if ibig comes newc -column
	p_akey00=p_aopt(p_abas1(ibig)+1:p_abas1(ibig)+p_nrowtot)
	do i=1,p_nsame
		lcurnext=p_next(lcur)
		!	if(p_isn16)write(p_n16,*)'i ,lcur ',i,lcur
 
		!	if(p_isn16)write(p_n16,*)'isame,lcur,nex,ibig',i,lcur,nex,ibig
		if(lcur.eq.ibig)then
 
			call entercol0(newc,p_lunit(lcur),p_isch(lcur),p_akey(p_abas1(lcur)+1:p_abas1(lcur)+p_nrowtot),&
				p_akey00)
			if(p_isn16)write(p_n16,*)'isame ,lcur ',i,lcur,'eq ibig iopt',p_isch(lcur),'newc',newc
		else
 
			call entercol0(newc,p_lunit(lcur),p_isch(lcur),p_aopt(p_abas1(lcur)+1:p_abas1(lcur)+p_nrowtot),&
				p_akey00)
			if(p_isn16)write(p_n16,*)'isame ,lcur ',i,lcur,'ne ibig ',ibig,'iopt',p_isch(lcur),'newc',newc
 
 
		endif !if(lcur.eq.ibig)   8139
		!	if(i.eq.1)akey=>p_akey(p_abas1(newc-p_nrowz)+1:p_abas1(newc-p_nrowz)+p_nrowtot)
		!column new is generated for unit iunit and scheduel iopt using
		!optimal x-vector aopt and 	key vector akey updates lunit and isch
		!!!write(16,*)' '
		!!!write(16,*)'#pivotnowinsaem i,pnsame',i,p_nsame,'old,*newc*',p_oldx,newc
		p_oldc=lcur+p_nrowz
 
		if(p_isn16)write(p_n16,*)j_err,'*into pivot in leavekey'
		call pivotnow(newc)
 
 
		!	write(16,*)'nextny',p_next,'lcur',lcur
		lcur=lcurnext ! p_next(lcur)
 
	enddo !i=1,p_nsame   8134
 
	! column ibig will be dropped if ibig=p_oldx then the first column to be dropped later
	! is p_next(p_oldx) otherwise p_oldx will be the first column
	!write(16,*)'pxef',p_lx(1:p_lx0)
	!write(16,*)'pxbef',p_x(p_nrowz+p_lx(1:p_lx0))
	call leftvec(p_rhsw,p_x)
	call testweight()
	if(j_err)return
 
	if(p_isn16)write(p_n16,'(a,(15i6))')'leavekey,p_x',p_lx(1:p_lx0)
	if(p_isn16)write(p_n16,'(a,(15f6.2))')'p_x',p_x(p_nrowz+p_lx(1:p_lx0))
 
	if(p_enter(3))then
		if(p_unit.eq.p_lunit(p_oldx))then
 
			p_akey0=p_akey00 !p_aopt(p_abas1(ibig)+1:p_abas1(ibig)+p_nrowtot)
			p_anew0=p_aopt0-p_akey0
			if(p_isn16)write(p_n16,'(a,(15f7.1))')'unit=lunit,key',p_akey0
			if(p_isn16)write(p_n16,'(a,(15f7.1))')'unit=lunit,anew0',p_anew0
		endif !if(p_unit.eq.p_lunit(p_oldx))   8180
	endif !if(p_enter(3))   8179
 
 
 
	if(p_isn16)write(16,*)'*return from leavekey'
	call testxps('inleavekey')
	! write(16,*)' '
	! call testx()
 
	! return
 
end subroutine leavekey


subroutine leave0ienter1(newc)
	use jmod, only: p_lower
	use jmod, only: p_ubou
	use jmod, only: p_lbou
	use jmod, only: j_err
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: j_v
	use jmod, only: p_feasible
	use jmod, only: p_ivfeasible
	use jmod, only: p_ivunbounded
	use jmod, only: p_maxo
	use jmod, only: j_inf
	use jmod, only: p_ivobj
	use jmod, only: j_ninf
	use jmod, only: p_rhscur
	use jmod, only: p_rhs2
	use jmod, only: p_rhs
	use jmod, only: p_xpsrow
	use jmod, only: p_rhsw
	use jmod, only: p_xps
 
	!else if(p_enter.eq.1)then !if(p_enter.eq.3)then
 
 
	if((p_lower(newc).and..not.p_ubou(newc)).or.(.not.p_lower(newc).and..not.p_lbou(newc))) then
		write(6,*)'jlp() unbounded problem (1)'
		if(p_isn16)write(p_n16,*)j_err,'newc', newc,p_lower(newc),p_lbou(newc)
		!	read(5,*)ksdkk
		j_err=.true.;return
 
		if(p_feasible)j_v(p_ivfeasible)=1.
		j_v(p_ivunbounded)=1.
		if(p_maxo)then
			j_v(p_ivobj)=j_inf
		else !if(j_maxo)then
			j_v(p_ivobj)=j_ninf
		endif !if(p_maxo)   8214
		j_err=.true. ;return
	endif !if((p_lower(newc).and..not.p_ubou(newc)).or.(.not.p_lower(   8206
	if(p_lower(newc))then
		p_lower(newc)=.false.
		p_rhscur(newc)=p_rhs2(newc)
	else !if(j_lower(newc))then
		p_lower(newc)=.true.
		p_rhscur(newc)=p_rhs(newc)
	endif !if(p_lower(newc))   8221
	if(p_xpsrow(newc))then
		p_rhsw(newc)=p_rhscur(newc)-p_xps(newc)
	else !if((j_ix(newc).ne.0).or.j_fpresent)then
		p_rhsw(newc)=p_rhscur(newc)
 
	endif !if(p_xpsrow(newc))   8228
	if(p_isn16)write(p_n16,*)j_err,'lower',p_lower,'cur',p_rhscur
	!	if(p_debug) write(6,*)'goto 55/täältä'
 
	!vain avaintehdas vaihtuu
	!silmukoiden toteutus, goto 55
 
	!if(p_enter.eq.3)then
 
 
end subroutine leave0ienter1



subroutine printproblem()
	use jmod, only: p_nrow
	use jmod, only: p_xpresent
	use jmod, only: p_isdomain
	use jmod, only: j_tempchar
	use jmod, only: p_rowdomnum
	use jmod, only: j_getline
	use jmod, only: p_ivdomain
	use jmod, only: j_chi5
	use jmod, only: p_domainunits
	use jmod, only: p_ivrow
	use jmod, only: p_apubuf
	use jmod, only: p_rhs
	use jmod, only: p_rhs2
	use jmod, only: j_chr10
	use jmod, only: p_lbou
	use jmod, only: p_ubou
	use jmod, only: p_maxo
	integer irow,ido,i,j,k
 
	ido=0
	do irow=1,p_nrow
		!	domloopp: do j=1,p_nsetd(i)
 
		if(p_xpresent.and.p_isdomain)then
					j_tempchar='DOMAIN:'
			idom=p_rowdomnum(irow)  !domain number
			call j_getline(p_ivdomain,idom,j_tempchar,le)
 
			j_tempchar(le+8:le+12)='units'
			j_tempchar(le+1:le+6)=j_chi5(p_domainunits(idom),0)
			!	write(6,*)' ',('_',kk=1,78)
			write(nureport,'(a)')j_tempchar(1:le+12)
		end if !if(p_xpresent.and.p_isdomain)   8254
		!write(nureport,*)' ',('_',kk=1,78)
		!		write(j_buf,66061)
		!66061 format('row',t38,'value',t50,'shadow',t61,  'lower',t70,'upper')
		!	write(nureport,'(a)')j_buf
		!		write(j_buf,6606)
		!6606 format(t50,'price', t61,'bound',t70, 'bound')
		!!	if(p_intapp)j_buf(35:44)='int. app.'
		!	write(nureport,'(a)')j_buf
		!	write(nureport,*)' ',('_',kk=1,78)
		!	do k=1,p_nsetr(i)
		!					if((k==1).and.(j>1)) irow0 = irow0 - p_nsetr(i)
		!		irow0 = irow0+1
 
		call j_getline(p_ivrow,irow,j_tempchar(6:),le)  ;j_tempchar(6+le:le+28)=' '
 
		!lyhennetään ylipitkät rivit etteivät sotkeennu
		!	if((6+le)>=34) then
		!		j_buf(34:35)=j_dots
		!	endif !if((6+le)>=34) then
 
		if(irow.gt.1)then
			!constraint row
 
			p_apubuf=j_chi5(irow,0); j_tempchar(1:3)=p_apubuf(3:5);j_tempchar(4:5)=') '
 
			!j_value=j_rhscur(irow)-j_x(irow)
			!j_buf(36:)=j_chr10(dble(j_value))
			!j_buf(47:57)=j_chr10(dble(j_vc(irow)))
			!if(j_vc(irow).ne.0.)then
			!	if(j_maxo.eqv.j_vc(irow).gt.0.)j_buf(78:78)='U'
			!	if(j_maxo.eqv.j_vc(irow).lt.0.)j_buf(78:78)='L'
			!end if !if(j_vc(irow).ne.0.)then
 
			if(p_rhs(irow).eq.p_rhs2(irow))then
				j_tempchar(le+10:le+22)= j_chr10(dble(p_rhs(irow)))
			else !if(p_rhs(irow).eq.p_rhs2(irow))then
				if(p_lbou(irow))j_tempchar(le+7:le+17)= j_chr10(dble(p_rhs(irow)))
				if(p_ubou(irow))j_tempchar(le+18:le+28)= j_chr10(dble(p_rhs2(irow)))
 
			end if !if(p_rhs(irow).eq.p_rhs2(irow))   8297
 
		else ! if(j.eq.1) then !if(irow0.ne.irowobj)then
			! for maximization rhs1 =huge  rhs2=0
			! for minimization  rhs2=-huge
			!j_buf(36:)=j_chr10(dble(j_objf))
 
			!if(p_feasible)then
			if(p_maxo)then
				j_tempchar(1:5)=' max'
			else !if(j_maxo)then
				j_tempchar(1:5)=' min'
			end if !if(p_maxo)   8311
			!else !if(p_feasible)then
			!	j_buf(1:5)=' '
			!	j_buf(6:33)='Infeasible, temporary object'
			!endif !if(p_feasible)then
		end if !if(irow.gt.1)   8284
		write(nureport,'(a)')j_tempchar(1:le+28)
		!	if(p_intapp)then
		!		j_buf=' '
		!		j_buf(36:)=j_chr10(j_solx(irow))
		!	write(6,'(a)')j_buf
		!	endif !if(p_intapp)then
		!	end do !k=1,p_nsetr(i)   9299
		!			enddo domloopp !domloopp: do j=1,j_nsetd(i)
	enddo !irow=1,p_nrow   8251
 
end subroutine !subroutine printproblem()

subroutine printcur()
	!write(17,*)'p_kier,iter',p_kier,iter
 
end subroutine !subroutine printcur()

logical function isxkzero(ixk)
	use jmod, only: p_ixkkeep
	use jmod, only: p_ibaunit
	use jmod, only: p_unit
	use jmod, only: p_keys
	use jmod, only: j_o
	use jmod, only: j_divmat
	use jmod, only: p_lx0
	use jmod, only: p_lunit
	use jmod, only: p_lx
	use jmod, only: p_isch
	integer ikey_,ikeepxk_,k_
	isxkzero=.true.
	ikeepxk_ = p_ixkkeep(ixk)
	ikey_=p_ibaunit(p_unit) + p_keys(p_unit)
	ibxdatkey_=ibaxdat(ikey_) !,2)
	if(j_o(j_divmat)%d(ibxdatkey_+ikeepxk_).ne.0.)then
		isxkzero=.false.
		return
	endif !if(j_o(j_divmat)%d(ibxdatkey_+ikeepxk_).ne.0.)   8344
	do k_=1,p_lx0
		if(p_lunit(p_lx(k_)).ne.p_unit) cycle
		ikey_=p_ibaunit(p_unit) + p_isch(p_lx(k_))
		ibxdatkey_=ibaxdat(ikey_) !,2)
		if(j_o(j_divmat)%d(ibxdatkey_+ikeepxk_).ne.0.)then
			isxkzero=.false.
			return
		endif !if(j_o(j_divmat)%d(ibxdatkey_+ikeepxk_).ne.0.)   8352
	enddo !k_=1,p_lx0   8348
end function !logical function isxkzero(ixk)

!funktio palauttaa vaihtoehdon arvon tehdasosuuden
! Sama laskenta oli aiemmin sellaisenaan vaihtoehtosilmukassa, nyt kaksi vaihtoehtosilmukkaa
! (normaali / slow improvment jälkitila), joista funktiota kutsutaan.
! iobs: vaihtoehdon indeksi (kaikkien yksiköiden vaihtoehtojen joukossa)

function value_f(iobs)
	use jmod, only: p_nrowxkfkey
	use jmod, only: p_irowfkeep
	use jmod, only: p_rowxkfkey
	use jmod, only: p_feasible
	use jmod, only: p_coeffx
	use jmod, only: j_o
	use jmod, only: j_divmat
	use jmod, only: p_p
	use jmod, only: p_n16
	use jmod, only: p_vc
	use jmod, only: p_nrowykfkey
	use jmod, only: p_rowykfkey
	use jmod, only: j_v
 
	real*8              :: value_f
	integer, intent(in) :: iobs
	integer ::jjj,ibxdatobs,jj
 
	value_f = 0.
	ibxdatobs=ibaxdat(iobs) !,1)
	do jjj = 1,p_nrowxkfkey
		irowfkeep_ = p_irowfkeep(p_rowxkfkey(jjj)%irowfx)
		if (p_rowxkfkey(jjj)%jcurix == 0) then
			if(p_feasible) then
				value_f = value_f + p_coeffx(p_rowxkfkey(jjj)%irowfx)*&
					j_o(j_divmat)%d(ibxdatobs+irowfkeep_)
				if(p_p)write(p_n16,*)'value_f, coeff, xkf ',&
					value_f, p_coeffx(p_rowxkfkey(jjj)%irowfx),j_o(j_divmat)%d(ibxdatobs+irowfkeep_)
			endif !if(p_feasible)   8375
		else !if (j_rowxkfkey(j_)%jcurix == 0) then
			value_f = value_f - p_vc(p_rowxkfkey(jjj)%jcurix)*p_coeffx(p_rowxkfkey(jjj)%irowfx)*&
				j_o(j_divmat)%d(ibxdatobs+irowfkeep_)
			!     if(p_p)write(p_n16,*)'value_f,vc,coeff,xkf',&
			!        value_f,j_vc(j_rowxkfkey(j_)%jcurix),j_coeffx(j_rowxkfkey(j_)%irowfx),&
			!          j_o(j_divmat)%d(ibaxdat(iobs)+irowfkeep_)
		endif !if (p_rowxkfkey(jjj)%jcurix == 0)   8374
	enddo !jjj = 1,p_nrowxkfkey   8372
 
	do jj=1,p_nrowykfkey
		iv2elpos_ = p_rowykfkey(jj)%iv2elpos
		if (p_rowykfkey(jj)%jcurix.eq.0) then ! 0-rivi, ei v-kerrointa
			if(p_feasible) then
				value_f = value_f + j_v(p_rowykfkey(jj)%ivfout)*&
					j_o(j_divmat)%d(ibxdatobs+iv2elpos_)
				if(p_p)write(p_n16,*)'value_f, gamma, ykf',value_f, j_v(p_rowykfkey(jj)%ivfout),&
					j_o(j_divmat)%d(ibxdatobs+iv2elpos_)
			endif !if(p_feasible)   8393
		else !if (j_rowykfkey(j_)%jcurix.eq.0) then
			value_f = value_f - p_vc(p_rowykfkey(jj)%jcurix)* j_v(p_rowykfkey(jj)%ivfout)*&
				j_o(j_divmat)%d(ibxdatobs+iv2elpos_)
			if(p_p)write(p_n16,*)'value_f,vc,gamma,ykf',value_f,p_vc(p_rowykfkey(jj)%jcurix),&
				j_v(p_rowykfkey(jj)%ivfout),j_o(j_divmat)%d(ibxdatobs+iv2elpos_)
		endif !if (p_rowykfkey(jj)%jcurix.eq.0)   8392
	enddo !jj=1,p_nrowykfkey   8390
 
end function value_f !function value_f(iobs)


function linkunit()
	use jmod, only: p_next
	use jmod, only: p_lx0
	use jmod, only: p_lunit
	use jmod, only: p_unit
 
	linkunit=0
 
	lcur=p_next(0)
	do i=1,p_lx0
 
		if(p_lunit(lcur).eq.p_unit)then
			linkunit=lcur
			return
		end if !if(p_lunit(lcur).eq.p_unit)   8417
		lcur=p_next(lcur)
	end do !i=1,p_lx0   8415
	return
end function !subroutine p_lcursam()

subroutine testld()
	use jmod, only: p_xpresent
	use jmod, only: p_lx0
	use jmod, only: p_next
	use jmod, only: p_lx
	use jmod, only: p_n16
	use jmod, only: p_prev
	use jmod, only: j_err
	if(.not.p_xpresent)return
	do i=1,p_lx0
		! löytyykö jokainen nextin kautta
		nex=0
		jj=1
766		nex=p_next(nex)
		if(nex.eq.p_lx(i))goto 767   ! löyty
		if(jj.gt.p_lx0.or.nex.eq.0)then
			write(p_n16,*)'saa  **','ld',(p_lx(j),j=1,p_lx0)
			write(p_n16,*)'next',p_next(0),p_next(p_next(0)),p_next(p_next(p_next(0)))&
				,	 p_next(p_next(p_next(p_next(0))))
			write(p_n16,*)'*s* iprev',p_prev(0),p_prev(p_prev(0)),&
				p_prev(p_prev(p_prev(0))),p_prev(p_prev(p_prev(p_prev(0))))
			write(6,*)'*s* -return'
			!		close(16)
			j_err=.true.
			return
		end if !if(jj.gt.p_lx0.or.nex.eq.0)   8434
		jj=jj+1
		goto 766
767		continue
	end do !i=1,p_lx0   8428
	! end logical testing
end subroutine !subroutine testld()



!
!p_unit_: yksikkö, joka käsittelyssä kutsuhetkellä -> palautetaan curixit oikeaan yksikköön
subroutine xps0sub(iunit_)
	use jmod, only: j_0
	use jmod, only: p_xps0
	use jmod, only: p_nunits
	use jmod, only: p_isdomain
	use jmod, only: p_ibaunit
	use jmod, only: p_keys
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: p_xmat
	use jmod, only: p_ix
	use jmod, only: p_fpresent
	use jmod, only: p_nrow
	use jmod, only: p_xps
	use jmod, only: p_n16
 
	p_xps0=j_0
	iunitrans=0
	!p_idomba=0
	do i_=1,p_nunits  !***********************
		if(p_isdomain)call jlpcurix(i_)
		! determines for each row if the unit iunit belonggs to the domain of the row
		! units can be in random order
		! p_nrowcurx
		! p_rowcurx
		iobs_=p_ibaunit(i_)+p_keys(i_)
		ibaxmat=ibamatx(iobs_) !,1)
		ibxdatobs_=ibaxdat(iobs_) !,1)
		do jj_=1,p_nrowcurx
			jj=p_rowcurx(jj_)
			!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
			p_xps0(jj)=p_xps0(jj)+p_xmat(p_ix(jj)+ibaxmat) !(ix(j)=0 -> no x in row
			!                if(ixcur(j).ne.0)xps(j)=xps(j)+xmat(ibxdatkey+ix(j))
		enddo !jj_=1,p_nrowcurx   8470
 
		!tehdasmuuttujien xps-laskenta
		if(p_fpresent) then
 
 
		endif !if(p_fpresent)   8478
 
 
	enddo !i_=1,p_nunits   8461
 
	!palautetaan käsiteltävänä olevan yksikön curixit ja tehdasmuunnokset
	if(iunit_>0) then
		iunitrans = iunit_
		if(p_isdomain)call jlpcurix(iunit_)
		! determines for each row if the unit iunit belonggs to the domain of the row
		! units can be in random order
		! p_nrowcurx
		! p_rowcurx
 
 
	endif !if(iunit_>0)   8487
 
	!write(p_n16,*)'**fact** xps0',(p_xps0(j),j=0,j_nrow)
	nero=0
	do iii=1,p_nrow
		if(abs(p_xps0(iii)-p_xps(iii)).gt.0.001)then
			nero=nero+1
			if(nero.lt.10)write(p_n16,*)'row,test,xps', iii,p_xps0(iii),p_xps(iii)
		endif !if(abs(p_xps0(iii)-p_xps(iii)).gt.0.001)   8501
	enddo !iii=1,p_nrow   8500
	write(p_n16,*)'xps0', nero
 
end subroutine !subroutine xps0sub(iunit_)

subroutine testobjr0()
	use jmod, only: p_lx0
	use jmod, only: j_0
	use jmod, only: p_test
	use jmod, only: p_lunit
	use jmod, only: p_lx
	use jmod, only: p_ibaunit
	use jmod, only: p_keys
	use jmod, only: p_isch
	use jmod, only: p_nrow
	use jmod, only: p_ixcur
	use jmod, only: p_xmat
	use jmod, only: p_ix
	use jmod, only: p_fpresent
	use jmod, only: p_n16
	integer pvars_
	!	write(p_n16,*)'**fact** testobjr0/w'
	iunitrans = 0
 
	do ldj_=1,p_lx0
 
		p_test = j_0
 
		iunit_=p_lunit(p_lx(ldj_))
		ikey_=p_ibaunit(p_lunit(p_lx(ldj_)))+ p_keys(p_lunit(p_lx(ldj_)))
		iobs_=p_ibaunit(p_lunit(p_lx(ldj_)))+ p_isch(p_lx(ldj_))
		ibaxmat=ibamatx(iobs_) !,1)
		ibxmatkey_=ibamatx(ikey_) !,2)
		ibxdatobs_=ibaxdat(iobs_) !,1)
		ibxdatkey_=ibaxdat(ikey_) !,2)
		!	write(p_n16,*)'ld(j),iunit,iobs,ikey',p_lx(ldj_),iunit_,iobs_,ikey_
 
		do it_=0,p_nrow
			if(p_ixcur(it_)) then
				p_test(it_) = p_xmat(p_ix(it_)+ibaxmat)- p_xmat(p_ix(it_)+ibxmatkey_)
			endif !if(p_ixcur(it_))   8529
		enddo !it_=0,p_nrow   8528
 
		if (p_fpresent) then
 
 
		endif !if (p_fpresent)   8534
 
		write(p_n16,*)'ld(j)',p_lx(ldj_),'test(0:nrow)',(p_test(jjj),jjj=0,p_nrow)
 
	enddo !ldj_=1,p_lx0   8515
end subroutine !subroutine testobjr0()



! end subroutine !subroutine testobjr0f()






subroutine defsoluf()
	write(6,*)'defsoluf'
 
end subroutine


! !

subroutine factxps(i,key)
	write(6,*)'factxps'
end subroutine
!

! end subroutine

function ibamatx(iobs)
	use jmod, only: p_ntemp0
 
	ibamatx=(iobs-1)*p_ntemp0
end function

function ibaxdat(iobs)
	use jmod, only: j_dnkeep
	ibaxdat=(iobs-1)*j_dnkeep
end function

! logical function indomain(ido)
! if(p_domvars(ido).eq.j_ivall)then
! indomain=.true.

! elseif(btest(p_domainbits(p_idomba+p_icurint(ido)),p_icurbit(ido)))then
! indomain=.true.
! else
! indomain=.false.
! endif !if(p_domvars(ido).eq.j_ivall)  13977
! p_idomba=p_idomba+p_ndomv
! return
! end function

double precision function area(iunit)
	use jmod, only: p_isunit
	use jmod, only: j_o
	use jmod, only: j_divmat
	use jmod, only: j_dnkeep
	use jmod, only: p_ibaunit
	use jmod, only: p_areakeep
	use jmod, only: j_divmatup
	use jmod, only: j_dnkeepup
	if(p_isunit)then
		area=j_o(j_divmat)%d(p_ibaunit(iunit)*j_dnkeep+p_areakeep)
 
	else
		area=j_o(j_divmatup)%d(iunit*j_dnkeepup+p_areakeep)
	endif !if(p_isunit)   8591
end function

subroutine piecesum()
	use jmod, only: p_piecesum
	use jmod, only: p_npiece
	use jmod, only: p_piecexps
	use jmod, only: p_lx0
	use jmod, only: p_x
	use jmod, only: p_lx
	use jmod, only: p_nrowz
	use jmod, only: p_lunit
	use jmod, only: p_isarea
	use jmod, only: p_ibaunit
	use jmod, only: p_keys
	use jmod, only: p_isch
	use jmod, only: j_dnkeep
	use jmod, only: j_o
	use jmod, only: j_divmat
	use jmod, only: p_piecekeep
	double precision area
	p_piecesum(1:p_npiece)=p_piecexps
	do i=1,p_lx0
 
		wei=p_x(p_lx(i)+p_nrowz)
		!	write(6,*)'weiny ',wei
 
		iuni=p_lunit(p_lx(i))
		if(p_isarea)wei=wei*area(iuni)
		do koo=1,2
 
			if(koo.eq.1)then
				iobs=p_ibaunit(iuni)+p_keys(iuni) !update for keyshced and explicit basic
 
			else
 
				iobs=p_ibaunit(iuni)+p_isch(p_lx(i))
 
			endif !if(koo.eq.1)   8611
			iba=(iobs-1)*j_dnkeep
			wei=-wei  !first key
 
			p_piecesum(1:p_npiece)=p_piecesum(1:p_npiece)+wei*j_o(j_divmat)%d(iba+p_piecekeep)
 
		end do !koo=1,2   8609
	enddo !i=1,p_lx0   8602
 
 
end subroutine
subroutine pieceprice()
	use jmod, only: p_npiece
	use jmod, only: j_o
	use jmod, only: p_ivpiececoef
	use jmod, only: p_piecesum
	use jmod, only: p_pieceprice
ploop:	do i=1,p_npiece
		ivmatrix=j_o(p_ivpiececoef)%i2(i)
		! write(6,*)p_ivpiececoef,ivmatrix
		! write(6,*)j_o(p_ivpiececoef)%i2
		! call j_getname(p_ivpiececoef,j_o(p_ivpiececoef)%i2(i),ivmatrix)
		! write(6,*)j_oname(1:j_loname),'*',j_oname2(1:j_loname2),'*',j_oname3(1:j_loname3)
 
		! write(6,*)j_o(j_o(p_ivpiececoef)%i2(i))%d
		npoints=j_o(ivmatrix)%i(1)
		if(p_piecesum(i).le.j_o(ivmatrix)%d(1))then
			p_pieceprice(i)=j_o(ivmatrix)%d(2)
			cycle ploop
		endif !if(p_piecesum(i).le.j_o(ivmatrix)%d(1))   8639
		do j=2,npoints-1,2
			if(p_piecesum(i).ge.j_o(ivmatrix)%d(j).and.&
					p_piecesum(i).le.j_o(ivmatrix)%d(j+2))then
 
				p_pieceprice(i)=j_o(ivmatrix)%d(npoints+j)
				cycle ploop
			endif !if(p_piecesum(i).ge.j_o(ivmatrix)%d(j).a   8644
 
		enddo !j=2,npoints-1,2   8643
		p_pieceprice(i)=j_o(ivmatrix)%d(j_o(ivmatrix)%i(1))
 
 
	enddo  ploop !op:	do i=1,p_npiece   8630
 
 
end subroutine
double precision function piecexmat0(iobs)
	use jmod, only: j_0
	use jmod, only: p_npiece
	use jmod, only: p_piececoef
	use jmod, only: j_o
	use jmod, only: p_piecesum
	use jmod, only: p_pieceint
	use jmod, only: p_ivpieceint
	use jmod, only: j_dapu
	double precision area,wei
	ibaint=0
	piecexmat0=j_0
ploop:	do i=1,p_npiece
 
		ivmatrix=p_piececoef(i)
		if(p_piecesum(i).le.j_o(ivmatrix)%d(1))then
			piecexmat0=piecexmat0+p_piecesum(i)*j_o(ivmatrix)%d(2)
 
		else
			iba=2
			do j=1,j_o(ivmatrix)%i(1)
				! write(6,*)p_ivpiececoef,ivmatrix
				! write(6,*)j_o(p_ivpiececoef)%i2
				! call j_getname(p_ivpiececoef,j_o(p_ivpiececoef)%i2(i),ivmatrix)
				! write(6,*)j_oname(1:j_loname),'*',j_oname2(1:j_loname2),'*',j_oname3(1:j_loname3)
				if(p_piecesum(i).le.j_o(ivmatrix)%d(iba+1).and.  &
						p_piecesum(i).le.j_o(ivmatrix)%d(iba+3))then
					piecexmat0=p_pieceint(ibaint+j)+j_o(ivmatrix)%d(iba+2)*p_piecesum(i)
					cycle ploop
					iba=iba+2
				endif !if(p_piecesum(i).le.j_o(ivmatrix)%d(iba+1).and   8676
				piecexmat0=p_pieceint(ibaint+iba)
				! write(6,*)j_o(j_o(p_ivpiececoef)%i2(i))%d
 
				!		j_dapu=j_dapu+j_rlinterm(ivmatrix,p_piecesum(i))
			enddo !j=1,j_o(ivmatrix)%i(1)   8671
		endif !if(p_piecesum(i).le.j_o(ivmatrix)%d(1))   8666
		ibaint=ibaint+j_o(p_ivpieceint)%i(2)
	enddo ploop !op:	do i=1,p_npiece   8663
	piecexmat0=j_dapu
end function



logical function indomain(ido,iuni)
	use jmod, only: j_ivall
	use jmod, only: p_domvars
	use jmod, only: p_ndomv
	use jmod, only: p_domainbits
	use jmod, only: p_icurint
	use jmod, only: p_icurbit
 
	if(p_domvars(ido).eq.j_ivall)then
		indomain=.true.
		return
	endif !if(p_domvars(ido).eq.j_ivall)   8697
	idomba=(iuni-1)*p_ndomv
	if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido)))then
		indomain=.true.
	else
		indomain=.false.
	endif !if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido   8702
	return
end function


!zerocapacity		   !!!!
! subroutine zerocap()

! do irowj_ = 2,p_nrow
! !		write(6,*)'row ',irowj_,p_rhs2(irowj_-1),p_nloginrow(irowj_),p_nfyinrow(irowj_),&
! !	p_ix(irowj_-1)
! if(p_nloginrow(irowj_).eq.1.and. p_nfyinrow(irowj_).eq.0.and. &
! p_ix(irowj_-1).eq.0)then !.and.j_ix(irowj_).eq.0)then !
! !z-muuttujat tsekattva a-matriisista)

! jxk=p_irowfxvar(p_ibafx(irowj_)+1)
! jf = p_irowffact(p_ibafx(irowj_)+1)
! p_zeroc=p_rhs2(irowj_-1).eq.j_0

! !		if(rh_.eq.0.)p_zeroc=.true.  !(jxk,jf)=.true.

! !20181116 #p_zeroc_z
! do iz=1,p_nz
! if(p_a(irowj_-1+p_abas(iz)).ne.0.)p_zeroc=.false.  !(jxk,jf)=.false.
! enddo !iz=1,p_nz  14092
! if(p_zeroc)then  !J-err=.true.
! write(6,*)'at row' ,irowj_, 'there is zero capacity for timber ',jxk,&
! 'and factory ',jf
! write(6,*)'remove the row and put the price or utility negative'
! j_err=.true.

! endif !if(p_zeroc)  14095

! endif !if(p_nloginrow(irowj_).eq.1.and. p_nfyinrow(irowj_).eq.0.an  14081
! enddo !irowj_ = 2,p_nrow  14078
! return
! end subroutine

subroutine fterms(iob,io)
	use jmod, only: j_err
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivlogpart
	use jmod, only: p_nterm
	use jmod, only: p_logpart
	use jmod, only: p_ivfactpart
	use jmod, only: p_factpart
	use jmod, only: p_ivtablepart
	use jmod, only: p_tablepart
	use jmod, only: p_isvarf
	use jmod, only: p_istermf
	use jmod, only: p_nvar
	use jmod, only: p_ivtable
	use jmod, only: p_ntable0
	use jmod, only: p_ntable
	use jmod, only: p_varofterm
	use jmod, only: j_otype
	use jmod, only: j_ipneigtable
	use jmod, only: j_o
	use jmod, only: j_getneigtablerow
	use jmod, only: j_getneigtablecol
	use jmod, only: j_putlistobject
	use jmod, only: p_ivlog
	use jmod, only: p_ivfact
	use jmod, only: p_ivlogfact
	use jmod, only: j_getname
	use jmod, only: j_getobject
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	use jmod, only: j_ipreal
	use jmod, only: p_ivkeeplog
	use jmod, only: p_nlog
	use jmod, only: p_keeplog
	use jmod, only: p_nfact
	use jmod, only: p_fact
	use jmod, only: p_log
	use jmod, only: p_table
	use jmod, only: p_nlogfact
	use jmod, only: p_nfactmax
	use jmod, only: j_allocatei
	use jmod, only: j_itempvector
	use jmod, only: p_ivneig
	use jmod, only: p_ivneigu
	use jmod, only: p_nlognfact
	use jmod, only: p_nolog
	use jmod, only: p_ivfactw
	use jmod, only: p_factw
	use jmod, only: p_ivlogfactlog
	use jmod, only: p_logfactlog
	use jmod, only: p_ivlogfactfact
	use jmod, only: p_logfactfact
	use jmod, only: p_ivnfactinlog
	use jmod, only: p_nfactinlog
	use jmod, only: p_ivibafact
	use jmod, only: p_ibafact
	use jmod, only: j_defmatrix
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: p_ivfkeyval
	use jmod, only: p_fkeyval
	use jmod, only: p_ivtablenlog
	use jmod, only: p_tablenlog
	use jmod, only: p_ivtableneibas
	use jmod, only: p_tableneibas
	use jmod, only: p_nlogneig
	use jmod, only: p_knn
	use jmod, only: p_ivlogmax
	use jmod, only: p_logmax
	use jmod, only: p_ivlogsum
	use jmod, only: p_logsum
	use jmod, only: j_tempvectors
	use jmod, only: p_ivkeyfact
	use jmod, only: p_nunits
	use jmod, only: p_keyfact
	use jmod, only: p_ivsfact
	use jmod, only: p_nrow
	use jmod, only: p_sfact
	use jmod, only: p_ivslog
	use jmod, only: p_slog
	use jmod, only: p_ivoptfact
	use jmod, only: p_optfact
	use jmod, only: p_logvol
	use jmod, only: p_logvolkey
	use jmod, only: p_ivoptfactkey
	use jmod, only: p_optfactkey
	use jmod, only: p_ivoptfact0
	use jmod, only: p_optfact0
	use jmod, only: p_ivlogval
	use jmod, only: p_logval
	use jmod, only: p_logtable
	use jmod, only: p_logtablei
	use jmod, only: p_neigbas
	use jmod, only: p_ivnloginrow
	use jmod, only: p_nrowtot
	use jmod, only: p_nloginrow
	use jmod, only: p_rowofterm
	use jmod, only: p_ix
	use jmod, only: p_xpsrow
	use jmod, only: p_ivnrowinlog
	use jmod, only: p_ivrowinlog
	use jmod, only: p_ivrowinlogterm
	use jmod, only: p_nrowinlog
	use jmod, only: p_rowinlog
	use jmod, only: p_rowinlogterm
	use jmod, only: p_keyfact0
	use jmod, only: j_inlist
	use jmod, only: j_dnkeep
	use jmod, only: j_dkeep
	use jmod, only: p_logfact
	use jmod, only: j_inlistobject
	use jmod, only: p_ivvars
	use jmod, only: p_tableutil
	use jmod, only: p_ndupmax
	use jmod, only: p_ndupmax1
	use jmod, only: p_logfactstep
	use jmod, only: p_ivlogfactterm0
	use jmod, only: p_logfactterm0
	use jmod, only: p_ivlogfactterm
	use jmod, only: p_logfactterm
	use jmod, only: j_oname3
	use jmod, only: j_loname3
	use jmod, only: j_1
	use jmod, only: p_coef
	use jmod, only: p_nvarf
	use jmod, only: p_ntermf
	use jmod, only: j_getoption
	use jmod, only: j_mutiltrans
	use jmod, only: j_iptrans
	use jmod, only: j_optarg0
	use jmod, only: p_ivutiltrans
	write(6,*)'fterms starts'
	!input p_nterm p_varofterm
	!p_ntable=
	!p_nlog
 
	!lists are expanded now
	!p_ntable=0
	!p_nlog=0
	!p_nfact=0
	!p_nlogfact=0
	!p_nfact=
	!p_ntable0   occurences
	!p_nfactmax
	!p_istermf  term
	!p_isvarf  fvars
 
	!	call j_getoption(iob,io,j_mknn,1,1,j_ipreal,.true.,narg,j_optarg0)
	if(j_err)return
	!	p_knn=j_v(j_optarg0(1))
 
	!	write(6,*)'p_knn',p_knn
 
	p_ivlogpart=j_deflist(j_ivout,'%logpart',list0=p_nterm,ilist=.true.,point=p_logpart)
	p_ivfactpart=j_deflist(j_ivout,'%factpart',list0=p_nterm,ilist=.true.,point=p_factpart)
	p_ivtablepart=j_deflist(j_ivout,'%tablepart',list0=p_nterm,ilist=.true.,point=p_tablepart)
 
	if(allocated(p_isvarf))deallocate(p_isvarf,p_istermf)
	allocate(p_isvarf(1:p_nvar),p_istermf(1:p_nterm))
	p_isvarf=.false. ;p_istermf=.false.
	! do i=1,p_nterm
	! if(j_otype(p_varofterm(i)).eq.j_ipneigtable)p_ntable0=p_ntable0+1
	! end do !i=1,p_nterm   8430
	!	p_ivtable0=j_deflist(j_ivout,'%tables0',nres=p_ntable0)
	p_ivtable=j_deflist(j_ivout,'%table',nres=p_ntable0)
 
	write(6,*)'p_ntable0',p_ntable0,p_ntable ! 0 occurence p_ntable diff
 
 
	nlog=0  !potential number of
	nfact=0 !potential number of factoriesa
	nlogfact=0
	do i=1,p_nterm
		ivterm=p_varofterm(i)
		if(j_otype(ivterm).ne.j_ipneigtable)cycle
		j_o(p_ivtablepart)%i2(i)=ivterm
		ivlog=j_getneigtablerow(ivterm)  !j_o(ivterm)%i(4)
		nlog=nlog+j_o(ivlog)%i(1)
		ivfact=j_getneigtablecol(ivterm)
		nfact=nfact+j_o(ivfact)%i(1)
		nlogfact=nlogfact+j_o(ivlog)%i(1)*j_o(ivfact)%i(1)
 
		iper=j_putlistobject(p_ivtable,single=ivterm)
		p_istermf(i)=.true.
	enddo !i=1,p_nterm   8786
 
	p_ivlog=j_deflist(j_ivout,'%log',list0=nlog)
	p_ivfact=j_deflist(j_ivout,'%fact',list0=nfact)
	p_ivlogfact=j_deflist(j_ivout,'%logfact',list0=nlogfact)
 
	do i=1,p_nterm
		ivterm=p_varofterm(i)
		if(j_otype(ivterm).ne.j_ipneigtable)cycle
		ivlog=j_getneigtablerow(ivterm)  !j_o(ivterm)%i(4)
		iper=j_putlistobject(p_ivlog,ivin=ivlog)
 
		ivfact=j_getneigtablecol(ivterm)
		iper=j_putlistobject(p_ivfact,ivin=ivfact)
 
		do ilo=1,j_o(ivlog)%i(1)
			call j_getname(j_o(ivlog)%i2(ilo))
 
			do ifa=1,j_o(ivfact)%i(1)
				call j_getname(-1,j_o(ivfact)%i2(ifa))
				logfact=j_getobject(0,j_oname(1:j_loname)//j_oname2(1:j_loname2),j_ipreal);if(j_err)return
				iper=j_putlistobject(p_ivlogfact,single=logfact)
			enddo !ifa=1,j_o(ivfact)%i(1)   8816
		enddo !ilo=1,j_o(ivlog)%i(1)   8813
	enddo !i=1,p_nterm   8804
 
	p_ivkeeplog=j_deflist(j_ivout,'%keeplog',list0=p_nlog,ilist=.true.,point=p_keeplog)
 
 
 
 
 
 
 
	p_ivkeeplog=j_deflist(j_ivout,'%keeplog',list0=p_nlog,ilist=.true.,point=p_keeplog)
 
	p_ivfact=j_deflist(j_ivout,'%fact',list0=p_nfact,point=p_fact)
 
	p_log=>j_o(p_ivlog)%i2(1:p_nlog)
	p_ntable=j_o(p_ivtable)%i(1)
	p_table=>j_o(p_ivtable)%i2(1:p_ntable)
	p_nlog=0
	p_nfact=0
	p_nlogfact=0
	p_nfactmax=0
	ibaw=0
	call j_allocatei(j_itempvector,p_nterm)
	if(allocated(p_ivneig))deallocate(p_ivneig,p_ivneigu);allocate(p_ivneig(1:p_ntable),p_ivneigu(1:p_ntable))
 
	do i=1,p_ntable
		p_ivneig(i)=j_o(p_table(i))%i(8)
		p_ivneigu(i)=j_o(p_table(i))%i(9)
		ivlog=j_getneigtablerow(p_table(i))  !j_o(ivterm)%i(4)
		call j_getname(ivlog)
 
		if(j_err)return
		nlog=j_o(ivlog)%i(1)
		write(6,*)'ivlog ',j_oname(1:j_loname),' nlog',nlog
 
		ivfact=j_getneigtablecol(p_table(i));if(j_err)return  !j_o(ivterm)%i(5)
		nfact=j_o(ivfact)%i(1)
		p_nlog=p_nlog+nlog
		do iw=1,nfact
			j_itempvector(ibaw+iw)=iw   !p_factw(ibaw+iw)=iw
		enddo !iw=1,nfact   8860
		ibaw=ibaw+nfact
		p_nfact=p_nfact+nfact
		p_nlogfact=p_nlogfact+nlog*nfact
		p_nfactmax=max(p_nfactmax,nfact)
	enddo !i=1,p_ntable   8847
	p_nlognfact=p_nlog*p_nfact
	write(6,*)'pnlog',p_nlog
	if(allocated(p_nolog))deallocate(p_nolog)
	allocate(p_nolog(1:p_nlog))
 
 
	p_ivfactw=j_deflist(j_ivout,'%factw',list0=p_nfact,list=j_itempvector(1:p_nfact),point=p_factw,ilist=.true.)
 
 
	p_ivlogfactlog=j_deflist(j_ivout,'%logfactlog',list0=p_nlogfact,point=p_logfactlog)
	p_ivlogfactfact=j_deflist(j_ivout,'%logfact',list0=p_nlogfact,point=p_logfactfact)
 
	!used to give the index in logfact for given ilog and ifact to
	!p_iv2logfact=j_deflist(j_ivout,'%2logfact',list0=p_nlog*p_nfact,point=p_2logfact)
 
	!how many factories for each log
	p_ivnfactinlog=j_deflist(j_ivout,'%nfactinlog',list0=p_nlog,ilist=.true.,point=p_nfactinlog)
	!how many factories for each log
	p_ivibafact=j_deflist(j_ivout,'%ibafact',list0=p_nlog,ilist=.true.,point=p_ibafact)
	!factories for each log
	!p_ivfactinlog=j_deflist(j_ivout,'%factinlog',list0=p_nlog,ilist=.true.,point=p_factinlog)
	p_ivfkeyval=j_defmatrix(j_ivout,'%fkeyval',int8(p_nlogfact),j_18,j_matreg,point=p_fkeyval)
	lkm=0
	!	p_ivtableutil=j_deflist(j_ivout,'%tableutil',list0=p_ntable,point=p_tableutil)
	p_ivtablenlog=j_deflist(j_ivout,'%tablenlog',list0=p_ntable,point=p_tablenlog,ilist=.true.)
	p_ivtableneibas=j_deflist(j_ivout,'%tableneibas',list0=p_ntable,point=p_tableneibas,ilist=.true.)
 
 
	write(6,*)'p_nlogfact ',p_nlogfact,p_nlog,p_nfact
 
	p_nlogneig=p_nlog*p_knn
	nlog2=0
	nfact2=0
	nlogfact2=0
	ibaf=0
	do i=1,p_ntable
		ivtable=p_table(i)
		!		call j_getname(-1,-1,ivtable)  !oname3
		ivlog=j_getneigtablerow(p_table(i))  !j_o(ivterm)%i(4)
 
		if(j_err)return
 
		nlog=j_o(ivlog)%i(1)
		p_tablenlog(i)=nlog
		p_log(nlog2+1:nlog2+nlog)=j_o(ivlog)%i2(1:nlog)
		!	write(6,*)'nlog,knn,ibas',nlog,p_nn,p_tableneibas(i)
		if(i.lt.p_ntable)p_tableneibas(i+1)=p_tableneibas(i)+nlog*p_knn
		!	write(6,*)'nlog,k_nn,p_tableneibas(i+1)',nlog,k_nn,p_tableneibas(i+1)
 
		ivfact=j_getneigtablecol(p_table(i));if(j_err)return  !j_o(ivterm)%i(5)
		nfact=j_o(ivfact)%i(1)
 
		p_fact(nfact2+1:nfact2+nfact)=j_o(ivfact)%i2(1:nfact)
 
		p_ivlogmax=j_defmatrix(j_ivout,'%logmax',int8(p_nlog),j_18,j_matreg,point=p_logmax) !ivob)
 
		p_ivlogsum=j_defmatrix(j_ivout,'%logsum',int8(p_nlog),j_18,j_matreg,point=p_logsum)
		!	p_ivtablecoeftot=j_defmatrix(j_ivout,'%tablecoeftot',p_nunits,p_nlogfact,j_matreg,point=p_tablecoeftot)
		!	p_neigu=>j_o(p_ivneigu)%d(1:p_kntot2)
		! if(allocated(p_neig))deallocate(p_neig,p_neigu)
		! allocate(p_neig(1:kntot*p_nunits),p_neigu(1:p_kntot*p_nunits))
		if(allocated(j_itempvector))deallocate(j_itempvector)
		allocate(j_itempvector(1:p_nfactmax))
		if(allocated(j_tempvectors))deallocate(j_tempvectors)
		allocate(j_tempvectors(1:p_nfactmax))
		p_ivkeyfact=j_deflist(j_ivout,'%keyfact',list0=p_nunits*p_nlog,ilist=.true.,ncol=p_nlog)
		p_keyfact=>j_o(p_ivkeyfact)%i2(1:p_nunits*p_nlog)
 
		!	write(6,*)'keyfactinit','keepx',j_dnkeep,'nunits',p_nunits
 
		p_ivsfact=j_deflist(j_ivout,'%sfact',list0=(p_nrow+1)*p_nlogfact,ilist=.true.)
		p_sfact=>j_o(p_ivsfact)%i2(1:(p_nrow+1)*p_nlogfact)
 
		p_ivslog=j_deflist(j_ivout,'%slog',list0=(p_nrow+1)*p_nlogfact,ilist=.true.)
		p_slog=>j_o(p_ivslog)%i2(1:(p_nrow+1)*p_nlogfact)
 
		p_ivoptfact=j_deflist(j_ivout,'%optfact',list0=p_nlog,ilist=.true.)
		p_optfact=>j_o(p_ivoptfact)%i2(1:p_nlog)
 
		if(allocated(p_logvol))deallocate(p_logvol,p_logvolkey)
		allocate(p_logvol(0:p_nrow),p_logvolkey(0:p_nrow))
 
		p_ivoptfactkey=j_deflist(j_ivout,'%optfactkey',list0=p_nlog,ilist=.true.)
		p_optfactkey=>j_o(p_ivoptfactkey)%i2(1:p_nlog)
 
		p_ivoptfact0=j_deflist(j_ivout,'%optfact0',list0=p_nlog,ilist=.true.)
		p_optfact0=>j_o(p_ivoptfact0)%i2(1:p_nlog)
 
		p_ivlogval=j_defmatrix(j_ivout,'%logval',int8(p_nlog),j_18,j_matreg)
		p_logval=>j_o(p_ivlogval)%d(1:p_nlog)
 
		if(allocated(p_logtable))deallocate(p_logtable,p_logtablei,p_neigbas)
		allocate(p_logtable(1:p_nlog),p_logtablei(1:p_nlog),p_neigbas(1:p_ntable))
 
		p_ivnloginrow=j_deflist(j_ivout,'%nloginrow',list0=p_nrowtot,ilist=.true.)
		p_nloginrow(0:)=>j_o(p_ivnloginrow)%i2(1:p_nrowtot)
		do it=1,p_nterm
			ir=p_rowofterm(it)
			if(p_logpart(it).ne.0)p_nloginrow(ir)=p_nloginrow(ir)+1
			if(p_logpart(it).ne.0.or.p_ix(ir).ne.0)p_xpsrow(ir)=.true.
		enddo !it=1,p_nterm   8964
 
		p_ivnrowinlog=j_deflist(j_ivout,'%nrowinlog',list0=p_nlog,ilist=.true.)
		p_ivrowinlog=j_deflist(j_ivout,'%rowinlog',list0=p_nlogfact,ilist=.true.)
		p_ivrowinlogterm=j_deflist(j_ivout,'%rowinlogterm',list0=p_nlogfact,ilist=.true.)
 
		p_nrowinlog=>j_o(p_ivnrowinlog)%i2(1:p_nlog)
		p_rowinlog=>j_o(p_ivrowinlog)%i2(1:p_nlogfact)
		p_rowinlogterm=>j_o(p_ivrowinlogterm)%i2(1:p_nlogfact)
		irv=0
 
		do it=1,p_nterm
			ir=p_rowofterm(it)
			ilog=p_logpart(it)
 
			if(ilog.ne.0.and.ir.ne.irv)p_nrowinlog(ilog)=p_nrowinlog(ilog)+1
			irv=ir
		enddo !it=1,p_nterm   8979
		il=0
		do ilog=1,p_nlog
			do it=1,p_nterm
				if(p_logpart(it).eq.ilog)then
					il=il+1
					p_rowinlog(il)=p_rowofterm(it)
					p_rowinlogterm(il)=it
				endif !if(p_logpart(it).eq.ilog)   8989
 
			enddo !it=1,p_nterm   8988
		enddo !ilog=1,p_nlog   8987
 
		if(allocated(p_keyfact0))deallocate(p_keyfact0);allocate(p_keyfact0(1:p_nlog))
 
 
 
 
		do ilog=1,nlog
			ilog2=nlog2+ilog
			p_ibafact(ilog2)=ibaf
			ik=j_inlist(p_log(ilog2),j_dnkeep,j_dkeep)
			p_nfactinlog(ilog2)=nfact
			p_logtablei(ilog2)=i
			p_logtable(ilog2)=p_table(i)
			ibaf=ibaf+nfact
 
 
			call j_getname(p_log(ilog2))
			if(ik.le.0)then
				write(6,*)j_oname(1:j_loname),' is not in xdata'
				j_err=.true.
				cycle
			endif !if(ik.le.0)   9014
 
			p_keeplog(ilog2)=ik
			j_o(ivtable)%i2(ilog)=ik
			!	write(6,*)'***',j_oname(1:j_loname)
			do ifact=1,nfact
				ifact2=nfact2+ifact
				write(6,*)'djjdjjd',j_oname(1:j_loname)
				call j_getname(-1,p_fact(ifact2))
				ilofa=j_getobject(0,j_oname(1:j_loname)//'%2%'//j_oname2(1:j_loname2),j_ipreal)
				nlogfact2=nlogfact2+1
				p_logfact(nlogfact2)=ilofa
				p_logfactlog(nlogfact2)=ilog2
				p_logfactfact(nlogfact2)=ifact2
				!			p_factinlog(nlogfact2)=ifact2
				!		p_2logfact((ilog2-1)*p_nfact+ifact2)=nlogfact2
				!		write(6,*)'i,ifact,ifact2,ilog,ilog2,iel,nlogfact2',i,ifact,ifact2,ilog,ilog2,(ifact2-1)*p_nfact+ilog2,nlogfact2
				!		write(6,*)
				do j=1,p_nterm
					if(ilofa.eq.p_varofterm(j))then
						p_logpart(j)=ilog2
						p_factpart(j)=ifact2
						p_tablepart(j)=i
						jif=j_inlistobject(p_varofterm(j),p_ivvars)
						p_isvarf(jif)=.true.
						p_istermf(j)=.true.
					endif !if(ilofa.eq.p_varofterm(j))   9037
					if(ivtable.eq.p_varofterm(j))then
						p_tablepart(j)=i
						p_istermf(j)=.true.
						jif=j_inlistobject(p_varofterm(j),p_ivvars)
						p_isvarf(jif)=.true.
					endif !if(ivtable.eq.p_varofterm(j))   9045
				enddo !j=1,p_nterm   9036
 
			enddo !ifact=1,nfact   9023
		enddo !ilog=1,nlog   9003
		if(j_err)return
		nlog2=nlog2+nlog
		write(6,*)'nlog2',nlog2
		nfact2=nfact2+nfact
 
		p_tableutil(i)=j_defmatrix(j_ivout,'%'//j_oname(1:j_loname)//'%util',int8(nlog),int8(nfact),j_matreg) !ivob)
 
	enddo !i=1,p_ntable   8903
 
	write(6,*)'log',p_log
	write(6,*)'keeplog',p_keeplog
	write(6,*)'p_tableneibas',p_tableneibas
	!write(6,*)'factory development  is at this point'
	!j_err=.true.
	!return
	if(j_err)return
 
 
	! ivlog=j_getneigtablecol(ivtable,len=nlog,point=j_templist2)  !j_o(ivtable)%i(4)
	! ivfac=j_getneigtablerow(ivtable,len=nfac,point=j_templist3)  !j_o(ivtable)%i(5)
	! call j_getname(ivtable,ivlog,ivfac)
	! ivdata=j_getobject(j_ivout,'%'//j_oname(1:j_loname)//'%data',j_ipreal)
	! ivcoefmat=j_defmatrix(j_ivout,'%'//j_oname(1:j_loname)//'%data%matrix',p_nunits,nlog*nfac,j_matreg)
 
	! ivkeep=j_deflist(j_ivout,'%'//j_oname(1:j_loname)//'%data%keep',list0=nlog*nfac,point=j_templist)
	! call j_defdata(ivdata,ivcoefmat,ivkeep)
	! !	call j_getname(ivdata)
	! write(6,*)'TABLEDATA ',j_oname(1:j_loname)
	! p_tabledata(j)=ivdata
 
	! enddo !j=1,p_ntable  14088
	p_ndupmax=0
	do i=1,p_nterm
		ivterm=p_varofterm(i)
		ndup=0
		if(j_otype(ivterm).eq.j_ipneigtable)then
			do i2=1,p_nterm-1
				if(p_varofterm(i2).eq.ivterm)ndup=ndup+1
			enddo !i2=1,p_nterm-1   9091
 
		endif !if(j_otype(ivterm).eq.j_ipneigtable)   9090
		p_ndupmax=max(p_ndupmax,ndup)
	enddo !i=1,p_nterm   9087
 
	p_ndupmax1=p_ndupmax+1
	p_logfactstep =p_ndupmax1*p_nlog
	!all tableterms
	!	p_ivtableterm=j_deflist(j_ivout,'%tableterm',list0=p_ndupmax1*p_ntable0,ilist=.true.)
	p_ivlogfactterm0=j_deflist(j_ivout,'%logfactterm0',list0=p_nlogfact, &
		ilist=.true.,point=p_logfactterm0)
 
	p_ivlogfactterm=j_deflist(j_ivout,'%logfactterm',list0=p_ndupmax1*p_nlogfact, &
		ilist=.true.,point=p_logfactterm)
 
	!	p_ivlogfactrow=j_deflist(j_ivout,'%logfactrow',list0=p_nlogfact, &
	!		ilist=.true.,point=p_logfactrow)
 
	!	write(6,'(8i4)')p_2logfact
 
 
	write(6,*)'bef making logfactterm'
	!	call printfact()
	!	iel=0
	!index in logfact list
	do lofa=1,p_nlogfact
 
		!	do ifact=1,p_nfact
		!	iel=p_2logfact((ilog-1)*p_nfact+ifact)
		!	iel=iel+1
		!	if(iel.eq.0)cycle
		ilog=p_logfactlog(lofa)
		ifact=p_logfactfact(lofa)
		!		inde=p_ibafact(ilog)+ifa
 
 
 
 
		ndup=0
tloop:		do  iterm=1,p_nterm
			irow=p_rowofterm(iterm)
			if(p_tablepart(iterm).ne.0.and.p_logpart(iterm).eq.0)then
				ivtable=p_table(p_tablepart(iterm))
				ivfact=j_getneigtablecol(ivtable);if(j_err)return !j_o(ivtable)%i(4)
				ivlog=j_getneigtablerow(ivtable);if(j_err)return  !j_o(ivtable)%i(5)
 
				ilo=j_inlistobject(p_log(ilog),ivlog)
				!			write(6,*)'iterm,ivfact,ivlog,ilo',iterm,ivfact,ivlog,ilo,p_log(ilog)
				!		write(6,*)'iterm,ik',iterm,ik
				if(ilo.le.0)cycle
				ik=j_inlistobject(p_fact(ifact),ivfact)
				!			write(6,*)'ik',ik,'inde',inde
				if(ik.le.0)cycle
 
 
				if(irow.eq.0)then
					if(p_logfactterm0(lofa).ne.0)then
						call j_getname(ivtable,ivlog,ivfact)
						write(6,*)'log ',j_oname2(1:j_loname2),' to ',j_oname3(1:j_loname3),' in table ', &
							j_oname(1:j_loname),' already in objject row'
						j_err=.true.;return
 
					endif !if(p_logfactterm0(lofa).ne.0)   9150
					p_logfactterm0(lofa)=iterm
				else
					do id=0,p_ndupmax
						if(p_logfactterm(id*p_nlogfact+lofa).eq.0)then
 
							p_logfactterm(id*p_nlogfact+lofa)=iterm
							!			write(6,*)'ilog,ifact,iterm,inde',ilog,ifact,iterm,inde
							cycle tloop
						endif !if(p_logfactterm(id*p_nlogfact+lofa).eq.0)   9160
					enddo !id=0,p_ndupmax   9159
 
 
				endif !if(irow.eq.0)   9149
 
			else
				if(p_logpart(iterm).ne.ilog.or.p_logpart(iterm).eq.0)cycle
				if(p_factpart(iterm).ne.ifact)cycle tloop
				if(irow.eq.0)then
					p_logfactterm0(lofa)=iterm
 
				else
					do id=0,p_ndupmax
						if(p_logfactterm(id*p_nlogfact+lofa).eq.0)then
							p_logfactterm(id*p_nlogfact+lofa)=iterm
							!			write(6,*)'ilog,ifact,iterm2222 ',ilog,ifact,iterm
							cycle tloop
						endif !if(p_logfactterm(id*p_nlogfact+lofa).eq.0)   9179
					enddo !id=0,p_ndupmax   9178
				endif !if(irow.eq.0)   9174
			endif !if(p_tablepart(iterm).ne.0.and.p_logpart(iterm).eq.0)   9135
		enddo tloop !op:		do  iterm=1,p_nterm   9133
		!	enddo !ifa=1,p_nfactinlog(ilog)  15340
		!	ibafact=ibafact+p_nfactinlog(ilog)
	enddo !lofa=1,p_nlogfact   9119
 
	!call printfact()
 
	do i=1,p_nlogfact
		iterm=p_logfactterm0(i)
		if(p_tablepart(iterm).ne.0.and.p_logpart(iterm).eq.0)then
			if(p_coef(iterm).ne.j_1)then
				write(6,*)'ask J.Lappi to add coef not equal to 1'
				j_err=.true.
				return
 
			endif !if(p_coef(iterm).ne.j_1)   9197
 
		endif !if(p_tablepart(iterm).ne.0.and.p_logpart(iterm).eq.0)   9196
		do j=1,p_ndupmax
			iterm=p_logfactterm(i)
			if(iterm.le.0)cycle
			if(p_tablepart(iterm).ne.0.and.p_logpart(iterm).eq.0)then
				if(p_coef(iterm).ne.j_1)then
					write(6,*)'ask J.Lappi to add coef not equal to 1'
					j_err=.true.
					return
 
				endif !if(p_coef(iterm).ne.j_1)   9209
 
			endif !if(p_tablepart(iterm).ne.0.and.p_logpart(iterm).eq.0)   9208
 
 
		enddo !j=1,p_ndupmax   9205
 
	enddo !i=1,p_nlogfact   9194
	write(6,*)'aft making logfactterm'
	!	call printfact()
	if(j_err)return
	p_nvarf=count(p_isvarf)
	p_ntermf=count(p_istermf)
 
	do i=1,p_nlogfact
		if(p_logfactterm(i).gt.0)then
			call j_getname(p_varofterm(p_logfactterm(i)))
			write(6,*)'in row0',i,j_oname(1:j_loname)
 
		endif !if(p_logfactterm(i).gt.0)   9229
	enddo !i=1,p_nlogfact   9228
 
 
	do i=1,p_ndupmax*p_nlogfact
		if(p_logfactterm(i).gt.0)then
			call j_getname(p_varofterm(p_logfactterm(i)))
			write(6,*)'i',i,j_oname(1:j_loname)
 
		endif !if(p_logfactterm(i).gt.0)   9238
	enddo !i=1,p_ndupmax*p_nlogfact   9237
 
	write(6,*)'found ',p_nvarf,' f-variables ',p_ntable,' tables ',p_nvarf-p_ntable,' %2% variables '
	write(6,*)p_ntermf, 'f-terms'
 
	call j_getoption(iob,io,j_mutiltrans,1,1,j_iptrans,.true.,narg,j_optarg0)
	if(j_err)return
	p_ivutiltrans=j_optarg0(1)
 
end subroutine fterms



! subroutine testlower()
! do i=1,p_lbb0
! irow=p_lbb(i)
! if(p_lower(irow))then
! if(p_r(irow).gt.j_0)then
! p_lower(irow)=.false.
! p_rhscur(irow)=p_rhs2(irow)
! if(p_ix(irow).ne.0)p_rhsw(irow)=p_rhsw(irow)+p_rhsdif(irow)
! if(p_isres(irow))p_x(irow)=p_x(irow)+p_rhsdif(irow)
! endif
! else
! if(p_r(irow).lt.j_0)then
! p_lower(irow)=.true.
! p_rhscur(irow)=p_rhs(irow)
! if(p_ix(irow).ne.0)p_rhsw(irow)=p_rhsw(irow)-p_rhsdif(irow)
! if(p_isres(irow))p_x(irow)=p_x(irow)-p_rhsdif(irow)
! endif

! endif
! end do
! !p_rhsw=p_rhscur-p_xps(1:p_nrow)(irow
!end subroutine

subroutine testx()
	use jmod, only: j_yes2
	use jmod, only: p_lx0
	use jmod, only: p_lx
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_nrow
	use jmod, only: p_aopt
	use jmod, only: p_abas1
	use jmod, only: p_nrowtot
	use jmod, only: j_one
	use jmod, only: p_akey
	use jmod, only: j_0
	use jmod, only: j_1
	use jmod, only: p_x
	use jmod, only: p_nrowz
	use jmod, only: p_isn16
	use jmod, only: p_n16
 
 
 
	j_yes2=.false.
	do ij=1,p_lx0
		lx=p_lx(ij)
		if(sum(abs(p_a(p_abas(lx)+1:p_abas(lx)+p_nrow)-p_aopt(p_abas1(lx)+2:p_abas1(lx)+p_nrowtot)+&
				p_akey(p_abas1(lx)+2:p_abas1(lx)+p_nrowtot))).gt.j_one)then
			write(16,*)'*************************ij ',ij,'lx',lx
			write(16,*)' a',p_a(p_abas(lx)+1:p_abas(lx)+p_nrow)
			write(16,*)' aopt',p_aopt(p_abas1(lx)+2:p_abas1(lx)+p_nrowtot)
			write(16,*)' akey',p_aopt(p_abas1(lx)+2:p_abas1(lx)+p_nrowtot)
		endif !if(sum(abs(p_a(p_abas(lx)+1:p_abas(lx)+p_nrow)-p_aopt(p_ab   9286
 
		if(p_x(p_lx(ij)+p_nrowz).lt.j_0.or.p_x(lx+p_nrowz).gt.j_1)then
			write(16,*)'*************************ij ',ij,p_x(lx+p_nrowz)
			write(6,*)'*************************ij ',ij,p_x(lx+p_nrowz)
			j_yes2=.true.
			!	read(5,*)fjkfkj
		endif !if(p_x(p_lx(ij)+p_nrowz).lt.j_0.or.p_x(lx+p_nrowz).gt.j_1)   9294
 
	enddo !ij=1,p_lx0   9284
 
	if(p_isn16.and.j_yes2)call printd()
	if(p_isn16.and.j_yes2)call rint('*testx',p_n16)
	!if(j_yes2)	read(5,*)fjkfkj
 
end subroutine


subroutine startf(iob,io)
	use jmod, only: p_fpresent
	use jmod, only: j_err
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivlogpart
	use jmod, only: p_nterm
	use jmod, only: p_logpart
	use jmod, only: p_ivfactpart
	use jmod, only: p_factpart
	use jmod, only: p_ivtablepart
	use jmod, only: p_tablepart
 
	if(p_fpresent)then
		call fterms(iob,io)  !knn i checked here
		if(j_err)return
	else
		p_ivlogpart=j_deflist(j_ivout,'%logpart',list0=p_nterm,ilist=.true.,point=p_logpart)
		p_ivfactpart=j_deflist(j_ivout,'%factpart',list0=p_nterm,ilist=.true.,point=p_factpart)
		p_ivtablepart=j_deflist(j_ivout,'%tablepart',list0=p_nterm,ilist=.true.,point=p_tablepart)
 
	endif !if(p_fpresent)   9312
 
end subroutine

subroutine zmatrix(iob,io)
	use jmod, only: p_n16
	use jmod, only: j_getoption
	use jmod, only: j_mzmatrix
	use jmod, only: j_ipmatrix
	use jmod, only: j_optarg0
	use jmod, only: j_err
	use jmod, only: p_zmatrix
	use jmod, only: p_ivzmatrix
	use jmod, only: j_o
	use jmod, only: p_nrow
	use jmod, only: p_nrowtot
	use jmod, only: p_ncol
	use jmod, only: p_nz
	use jmod, only: j_mrhs
	use jmod, only: p_ivrhs
	use jmod, only: j_printname
	use jmod, only: p_rhs
	use jmod, only: j_mrhs2
	use jmod, only: p_ivrhs2
	use jmod, only: p_rhs2
	use jmod, only: j_mmax
	use jmod, only: p_maxo
	use jmod, only: j_1
	use jmod, only: p_coefmax
	use jmod, only: j_mmin
	use jmod, only: p_ivzobj
	use jmod, only: j_getname
	use jmod, only: p_ivmatrix
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	use jmod, only: p_objrz
	write(6,*)'*zmatrix'
	write(p_n16,*)'*zmatrix'
	call j_getoption(iob,io,j_mzmatrix,-1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
	if(j_err)then
		write(6,*)'jlpz() there must be either problem-> or zmatrix->'
		return
	endif !if(j_err)   9328
	p_zmatrix=.true.
	p_ivzmatrix=j_optarg0(1)
	p_nrow=j_o(p_ivzmatrix)%i(1)
	p_nrowtot=p_nrow+1
	p_ncol=j_o(p_ivzmatrix)%i(2)
	p_nz=p_ncol
	call j_getoption(iob,io,j_mrhs,-1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_ivrhs=j_optarg0(1)
 
	if(j_o(p_ivrhs)%i(3).ne.p_nrow) then
		call j_printname('*jlp: rhs ',p_ivrhs,' should have same number elements as there are rows in p_zmatrix->')
		write(6,*)'Rows in zmatrix->:',p_nrow
		write(6,*)'Elements in rhs->:',j_o(p_ivrhs)%i(3)
		j_err=.true.;return
	endif !if(j_o(p_ivrhs)%i(3).ne.p_nrow)   9342
	p_rhs=>j_o(p_ivrhs)%d(1:p_nrow)
 
	call j_getoption(iob,io,j_mrhs2,1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_ivrhs2=j_optarg0(1)
	nrhsoptdim1=j_o(p_ivrhs)%i(1);nrhsoptdim2 =j_o(p_ivrhs)%i(2)
	if(j_o(p_ivrhs2)%i(3).ne.p_nrow) then
		call j_printname('*jlp: rhs2 ',p_ivrhs2,' should have same number elements as there are rows in p_zmatrix->')
		write(6,*)'Rows in zmatrix->:',p_nrow
		write(6,*)'Elements in rhs->:',j_o(p_ivrhs2)%i(3)
		j_err=.true.;return
	endif !if(j_o(p_ivrhs2)%i(3).ne.p_nrow)   9354
	p_rhs2=>j_o(p_ivrhs2)%d(1:p_nrow)
	call j_getoption(iob,io,j_mmax,-1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.gt.0)then
		p_maxo =.true.
 
		p_coefmax=j_1
	else
		call j_getoption(iob,io,j_mmin,1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
		if(j_err)return
		if(noptarg.gt.0)then
			p_maxo =.false.
			p_coefmax=-j_1
		endif !if(noptarg.gt.0)   9370
	endif !if(noptarg.gt.0)   9363
	p_ivzobj=j_optarg0(1)
	!	write(6,*)'p_ivzobj ',p_ivzobj
	if(j_o(p_ivzobj)%i(3).ne.p_ncol)then
		call j_getname(p_ivzobj,p_ivmatrix)
		write(6,*)'objective ',j_oname(1:j_loname), ' has ',j_o(p_ivzobj)%i(3),' elements but zmatrix ',&
			j_oname2(1:j_loname2),' has ',p_ncol,' columns'
		j_err=.true.;return
	endif !if(j_o(p_ivzobj)%i(3).ne.p_ncol)   9377
	if(allocated(p_objrz))deallocate(p_objrz)
	allocate(p_objrz(1:p_ncol))
	p_objrz=j_o(p_ivzobj)%d(1:p_ncol)
	write(16,*)'objrz here',p_objrz
	!	p_objrz=>j_o(p_ivzobj)%d(1:p_ncol)
 
 
end subroutine zmatrix


subroutine start(iob,io) !p_ivproblem
	use jmod, only: j_getoption
	use jmod, only: j_mproblem
	use jmod, only: j_ipproblem
	use jmod, only: j_optarg0
	use jmod, only: j_err
	use jmod, only: p_ivproblem
	use jmod, only: j_o
	use jmod, only: p_ivrow
	use jmod, only: p_nrow
	use jmod, only: p_nrowtot
	use jmod, only: j_ivout
	use jmod, only: p_zmatrix
	use jmod, only: j_getobject
	use jmod, only: j_ipreal
	use jmod, only: p_ivfeasible
	use jmod, only: j_v
	use jmod, only: j_0
	use jmod, only: p_ivobj
	use jmod, only: j_igetopt
	use jmod, only: j_mdpivot
	use jmod, only: p_pivotstep
	use jmod, only: j_ivcpu
	call j_getoption(iob,io,j_mproblem,-1,1,j_ipproblem,.true.,noptarg,j_optarg0)
	if(j_err)return
	!write(6,*)'hellurei'
 
	if(noptarg.gt.0)then
		p_ivproblem=j_optarg0(1)
		p_ivrow=j_o(p_ivproblem)%i(4)  !text for rows
		p_nrow=j_o(p_ivrow)%i(0)-1
		! if(allocated(p_feasrow)) deallocate(p_feasrow)
		! allocate(p_feasrow(1:p_nrow))
		! p_feasrow=.false.
		p_nrowtot=p_nrow+1
		call jlpcoef_(p_ivproblem,j_ivout)   !jlpz(
		!	write(6,*)p_ivproblem,ivobjects
		! p_coefvars
		! p_varofterm
		!	p_nterminrow
		! p_rowofterm
		! p_coefplus
		! p_vars
 
		if(j_err)return
 
 
 
		p_zmatrix=.false.
 
 
	else
		p_ivproblem=0
 
	endif !if(noptarg.gt.0)   9398
 
	p_ivfeasible=j_getobject(j_ivout,'%feasible',j_ipreal)
	j_v(p_ivfeasible)=j_0
 
	p_ivobj=j_getobject(j_ivout,'%obj',j_ipreal)
	!	p_nrowtot=p_nrow+1
	!	write(6,*)' tas '
	iv=j_igetopt(iob,io,j_mdpivot)
	if(j_err)return
	if(iv.gt.0)then
		p_pivotstep=j_v(iv)
	else
		p_pivotstep=100
	endif !if(iv.gt.0)   9435
	j_ivcpu=j_getobject(j_ivout,'%cpu',j_ipreal)
	j_v(j_ivcpu)=j_0
 
 
end subroutine


subroutine initdata0(iob,io)
	use jmod, only: p_isn16
	use jmod, only: j_getoption
	use jmod, only: j_munit
	use jmod, only: j_ipreal
	use jmod, only: j_optarg0
	use jmod, only: j_err
	use jmod, only: p_isunit
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivunit
	use jmod, only: j_divobsup
	use jmod, only: p_unitvars
	use jmod, only: p_unitkeep
	use jmod, only: j_getdataobject
	use jmod, only: j_inlistobject
	use jmod, only: j_divnobsw
	use jmod, only: j_divkeepup
	use jmod, only: j_printname
	use jmod, only: p_isxvar
	use jmod, only: p_nvar
	use jmod, only: p_istermx
	use jmod, only: p_nterm
	use jmod, only: p_ivnxinrow
	use jmod, only: p_nrowtot
	use jmod, only: j_o
	use jmod, only: p_ninrowx
	use jmod, only: p_nrowcurx
	use jmod, only: p_nxval
	use jmod, only: p_ntermf
	use jmod, only: p_ntable0
	use jmod, only: p_ntable
	use jmod, only: p_varofterm
	use jmod, only: j_divkeep
	use jmod, only: j_yes
	use jmod, only: p_ispiece
	use jmod, only: p_istermpiece
	use jmod, only: p_ivvars
	use jmod, only: p_rowofterm
	use jmod, only: j_otype
	use jmod, only: j_ipneigtable
	use jmod, only: j_getname
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: p_nxvar
	use jmod, only: p_nstot
	use jmod, only: p_ibaunitbas
	use jmod, only: p_nrejtot
	use jmod, only: j_divmat
	use jmod, only: j_itempvector
	use jmod, only: j_divdata
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	use jmod, only: p_nunits
	use jmod, only: j_dnkeep
	use jmod, only: p_maxns
	use jmod, only: j_dnobs
	use jmod, only: j_dnobsup
	use jmod, only: j_divdataup
	use jmod, only: p_xpresent
	use jmod, only: p_ivxvar
	use jmod, only: p_xvar
	use jmod, only: p_vars
	use jmod, only: p_ivibaunit
	use jmod, only: p_ibaunit
	use jmod, only: p_warm
	use jmod, only: p_ivkeys
	use jmod, only: p_keys
	use jmod, only: p_ivns
	use jmod, only: p_ns
	use jmod, only: j_dmatup
	use jmod, only: j_dnkeepup
	! p_ivarea,p_isunit,j_divdataup,p_ivdatax,j_divobsup,j_divkeep,j_dnkeep,j_divmatup
	! j_divkeepup,j_divobsup,j_divmat,j_dnobs,j_dmat,j_divobs,j_ivns,j_filterlink
	!p_filter,j_rejectlink,p_reject,p_filre,p_isxvar,p_istermx,p_ivnxinrow,p_ninrowx(0:)
	!p_nrowcurx,p_nxvar,p_nstot,p_ibaunitbas,p_nrejtot,,p_maxns,p_nunits,p_ivxvar
	!p_xvar,p_ivibaunit,p_ibaunit,p_ivkeys,p_keys,	p_ivns,p_ns
	!p_isunit,p_divobsup,p_unitvars,p_unitkeep,
	! integer::j_divdata,j_dfilterlink,j_drejectlink,j_divtrans,j_divvars,j_divmat,j_divvars2
	! integer::j_divdataup,j_dfilterlinkup,j_drejectlinkup,j_divtransup,j_divvarsup,j_divmatup,j_divvars2up
	!p_istermx,p_nxvar etc
	write(6,*)'*initdata0'
	if(p_isn16)write(16,*)'*initdata0'
 
	! init data also when there are no constraints
 
 
	call j_getoption(iob,io,j_munit,-1,100,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_isunit=.false.
 
 
	if(noptarg.gt.0)then
		p_ivunit=j_deflist(j_ivout,'%unit',list0=noptarg,list=j_optarg0(1:noptarg))
		j_divobsup=j_optarg0(1)
		p_isunit=.true.
		if(allocated(p_unitvars))deallocate(p_unitvars)
		allocate(p_unitvars(1:noptarg))
		if(allocated(p_unitkeep))deallocate(p_unitkeep)
		allocate(p_unitkeep(1:noptarg))
 
 
	endif !if(noptarg.gt.0)   9468
 
 
 
	!		endif
	! integer::j_divdata,j_dfilterlink,j_drejectlink,j_divtrans,j_divvars,j_divmat,j_divvars2
	! integer::j_divdataup,j_dfilterlinkup,j_drejectlinkup,j_divtransup,j_divvarsup,j_divmatup,j_divvars2up
	write(6,*)
	if(p_isunit)then
		!	p_ivdatax=ivdata
		!	call j_getdataobject(iob,io,ivdata=p_ivdatax,pointmat=j_dmat,pointkeep=j_dkeep)
		call j_getdataobject(iob,io)
		if(j_err)return
 
		!	j_dnkeep=j_dnkeep		!	ivxdatmat=
	else
		call j_getdataobject(iob,io,needsup=.true.)
		!	j_divdataup=j_getupdata(ivdata,needed=.true.)
		if(j_err)return
 
		!	write(6,*)'UNIT ',j_oname(1:j_loname) j_divnobsw
		!	j_divnobsw=j_o(j_divdataup)%i(4)   !variable 'Ns' telli ng the number of schedules in each unit (nobsw variable)
		nsindex=j_inlistobject(j_divnobsw,j_divkeepup)
		!	write(6,*)'nsindex ',nsindex
		!	write(6,*)'cdatai ',j_o(j_divdataup)%i(1:10)
 
		if(nsindex.le.0)then
			call j_printname('**nobsw variable ',j_divnobsw,' not in the cdata')
			j_err=.true.;return
		endif !if(nsindex.le.0)   9504
 
		!is(p_p8)write(6,*)'<4075,j_divdataup,p_xdata ',j_divdataup,p_ivdatax, 'keepx ',j_dnkeep
	endif !if(p_isunit)   9486
	if(allocated(p_isxvar))deallocate(p_isxvar);allocate(p_isxvar(1:p_nvar))
	p_isxvar=.false.
 
 
 
	if(allocated(p_istermx))deallocate(p_istermx);allocate(p_istermx(1:p_nterm))
	p_istermx=.false.
 
	!if(allocated(p_ninrowx))deallocate(p_ninrowx);allocate(p_ninrowx(0:p_nrow)) ;p_ninrowx=0
	p_ivnxinrow=j_deflist(j_ivout,'%nxinrow',list0=p_nrowtot,ilist=.true.)
	p_ninrowx(0:)=>j_o(p_ivnxinrow)%i2(1:p_nrowtot)
 
	p_nrowcurx=0  !number of rows having x
 
	p_nxval=0
 
	write(6,*)'p_nterm,p_ntermf',p_nterm,p_ntermf
	p_ntable0=0
	p_ntable=0
 
	! read(5,*)hdfh
vloop:	do ival=1,p_nterm
		i=p_varofterm(ival)
		ikeep=j_inlistobject(i,j_divkeep)
		!call j_getname(i)
		!write(6,*)ival,j_oname(1:j_loname+1),ikeep
		iout=0
		!if(j_divobsubtrans.gt.0)iout=j_inlistobject(i,j_trans_output(j_divobsubtrans))
		j_yes=.true.
		if(p_ispiece)j_yes=.not.p_istermpiece(ival)
		!	if(p_ispiece)write(6,*)'ival',ival,p_nterm,j_oname(1:j_loname),p_istermpiece(ival),j_yes
		if(ikeep.gt.0)then
 
			if(j_yes)then
				p_istermx(ival)=.true.  ! is x variaböe
				!write(6,*)'**',p_varofterm(ival),p_ivvars
				jix=j_inlistobject(i,p_ivvars)
				!
				! if(jix.le.0)then
				! write(6,*)'var ',i,' not among ',p_vars
				! stop
				! endif !if(jix.le.0)   8955
				p_isxvar(jix)=.true.
				p_nxval=p_nxval+1
				!write(6,*)'jix ',jix,p_nxval
				iro=p_rowofterm(ival)
				p_ninrowx(iro)=p_ninrowx(iro)+1
				if(p_ninrowx(iro).eq.1)p_nrowcurx=p_nrowcurx+1
			endif !if(j_yes)   9544
		elseif(j_otype(i).eq.j_ipneigtable)then
			p_ntable0=p_ntable0+1
 
			do ival2=1,ival
				if(p_varofterm(ival2).eq.i)cycle vloop
			enddo !ival2=1,ival   9563
			p_ntable=p_ntable+1
 
		elseif(.not.j_yes)then
			call j_getname(i)
			write(6,*)'piece-wise term ',j_oname(1:j_loname),' is not in xdata'
			j_err=.true.;return
 
		endif !if(ikeep.gt.0)   9542
	enddo vloop !op:	do ival=1,p_nterm   9532
 
	! take logs and factories from tables
 
 
 
 
	!	write(6,*)'istermx',p_istermx,'fpresernt',p_fpresent
 
	p_nxvar=count(p_isxvar)
 
	p_nstot=0  !total number of schedules
	!ibasclass=0  given earlier
	isc=0  ! have we starter the given class if class option is given
	p_ibaunitbas=0  ! basis for schedules, remains 0 if option class-> not given
	p_nrejtot=0  !number of rejected schdules
 
 
	!!!!!**************setting up xdata
	!write(6,*)'<147 hui',size(j_xmat),allocated(j_xmat)
 
	!	write(6,*)'6666pisunit',p_isunit
	if(p_isunit)then
		nntem=j_o(j_divmat)%i(1)/2
		if(allocated(j_itempvector))then
			if(size(j_itempvector).lt.nntem)deallocate(j_itempvector)
		endif !if(allocated(j_itempvector))   9598
		if(.not.allocated(j_itempvector))allocate(j_itempvector(1:nntem))
		do j=1,j_o(p_ivunit)%i(1)
			!	inde=j_inlistobject(j_divobsup,j_divkeep)
			p_unitkeep(j)=j_inlistobject(j_o(p_ivunit)%i2(j),j_divkeep)
			if(p_unitkeep(j).le.0)then
				call j_getname(j_o(p_ivunit)%i2(j),j_divdata)
				write(6,*)'unit-> variable ',j_oname(1:j_loname),&
					' not in keep vars of data ',j_oname2(1:j_loname2)
				j_err=.true.;return
			endif !if(p_unitkeep(j).le.0)   9605
		enddo !j=1,j_o(p_ivunit)%i(1)   9602
		!	write(6,*)'inde',inde
		p_nunits=1
		!		j_dapu=j_o(j_divmat)%d(inde)
		p_unitvars=j_o(j_divmat)%d(p_unitkeep)
		!write(6,*)'unitvars ', p_unitvars
		ibas=j_dnkeep
		iprev=1
		iprev=1
		p_maxns=1
		!	write(6,*)'indet',inde,old tasa5
		do i=2,j_dnobs
			!	if(i.lt.200)write(6,*)i,j_o(j_divmat)%d(ibas+inde),old,ibas
			!	if(j_o(j_divmat)%d(ibas+inde).ne.j_dapu)then
			if(.not.all(j_o(j_divmat)%d(ibas+p_unitkeep).eq.p_unitvars))then
				j_itempvector(p_nunits)=i-iprev
				p_maxns=max(p_maxns,j_itempvector(p_nunits))
				p_nunits=p_nunits+1
				!		j_dapu=j_o(j_divmat)%d(ibas+inde)
				p_unitvars=j_o(j_divmat)%d(ibas+p_unitkeep)
				!write(6,*)i,p_unitvars
 
				iprev=i
			endif !if(.not.all(j_o(j_divmat)%d(ibas+p_unitkeep).eq.p_unitvars   9625
			ibas=ibas+j_dnkeep
		enddo !i=2,j_dnobs   9622
 
		j_itempvector(p_nunits)=j_o(j_divmat)%i(1)-iprev+1
		p_maxns=max(p_maxns,j_itempvector(p_nunits))
 
		!	write(6,*)'p_nunits ',p_nunits
		!	write(6,*)'shed',p_ns(1:p_nunits)
	else
		p_nunits=j_dnobsup !j_o(j_divmatup)%i(1)
	endif !if(p_isunit)   9596
	!	write(6,*)'pnunits',p_nunits
	! if(allocated(p_valuedif))deallocate(p_valuedif,p_objdif)
	! allocate(p_valuedif(1:p_nunits),p_objdif(1:p_nun
	if(.not.p_isunit)p_maxns=j_o(j_divdataup)%i(9)
	!is(p_p8)write(6,*)'<pmaxns ',p_maxns
 
	!	call j_getdat(p_ivdatax,j_dnobs,j_divmat,j_divkeep)
	if(j_err)return
	if(p_xpresent)then
		p_ivxvar=j_deflist(j_ivout,'%xvar',list0=p_nxvar)
		p_xvar=>j_o(p_ivxvar)%i2(1:p_nxvar)
		p_xvar=pack(p_vars(1:p_nvar),p_isxvar)
 
	end if !if(p_xpresent)   9654
	p_ivibaunit=j_deflist(j_ivout,'%ibaunit',list0=p_nunits+1,ilist=.true.)
	p_ibaunit=>j_o(p_ivibaunit)%i2(1:p_nunits+1)
 
	!if(allocated(p_ibaunit))deallocate(p_ibaunit)
	if(.not.p_warm)then
		p_ivkeys=j_deflist(j_ivout,'%keys',list0=p_nunits,ilist=.true.)
		p_keys=>j_o(p_ivkeys)%i2(1:p_nunits)
		!	allocate(p_keys(1:p_nunits))  !;p_keys=0
	endif !if(.not.p_warm)   9664
	p_ivns= j_deflist(j_ivout,'%ns',list0=p_nunits,ilist=.true.,point=p_ns)
	!	p_ns=>j_o(p_ivns)%i2(1:p_nunits)
	!	if(allocated(p_ns))deallocate(p_ns)
	!write(6,*)'<356nunits ',p_nunits
	!	allocate(p_ns(1:p_nunits))
 
	if(p_isunit)then
		p_ns=j_itempvector(1:p_nunits)
		deallocate(j_itempvector)
	endif !if(p_isunit)   9675
	! if(p_nxvar.eq.0.and..not.p_fpresent)then
	! write(6,*)'there are no x-variables even if data->'
	! j_err=.true.;return
 
	! endif !if(p_nxvar.eq.0.and..not.p_fpresent)   9671
 
	p_ibaunit(1)=0 ! ibaunit, basis for schedules for each unit
	iba=0
	do i=1,p_nunits  !***********************
		!	write(6,*)'in',i,p_nunits
		if(.not.p_isunit)p_ns(i)=j_dmatup(iba+nsindex)
		p_ibaunit(i+1)=p_ibaunit(i)+p_ns(i)
		iba=iba+j_dnkeepup
		!	write(6,*)'here',i,p_isunit,p_ns(i),iba,nsindex,p_ibaunit(i+1),j_dnkeepup
	enddo !i=1,p_nunits   9687
 
	!	write(6,*)'ibaunit',p_ibaunit
 
end subroutine initdata0


subroutine commonopt(iob,io)
	use jmod, only: j_getoption
	use jmod, only: j_mpullout
	use jmod, only: j_optarg0
	use jmod, only: j_err
	use jmod, only: p_ispullout
	use jmod, only: j_v
	use jmod, only: j_mdebug
	use jmod, only: p_idebug1
	use jmod, only: p_idebug2
	use jmod, only: j_ivdebug
	use jmod, only: j_igetopt
	use jmod, only: p_idebug
	use jmod, only: p_ivpopar
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: j_o
	use jmod, only: p_ivfeasible
	use jmod, only: p_ivunbounded
	use jmod, only: p_ivobj
	use jmod, only: p_ivobj0
	use jmod, only: p_ivoptimal
	use jmod, only: p_ivpivots
	use jmod, only: p_ivtmax
	use jmod, only: p_ivlr0
	use jmod, only: p_ivlz0
	use jmod, only: p_ivlx0
	use jmod, only: p_ivrefac
	use jmod, only: j_getobject
	use jmod, only: j_ipreal
	use jmod, only: p_nrow
	use jmod, only: p_ivstartedjlp
	use jmod, only: p_ivvaluek
	use jmod, only: p_ivvalueopt
	use jmod, only: j_ninf
	use jmod, only: p_refac
	use jmod, only: j_mprint
	use jmod, only: p_nrecover
	use jmod, only: j_codelink
	use jmod, only: j_mstop
	use jmod, only: p_stoplink
	use jmod, only: p_isstop
	use jmod, only: j_mfastp
	use jmod, only: p_fastplink
	use jmod, only: p_isfastp
	use jmod, only: p_again
	use jmod, only: p_nagain
	use jmod, only: p_ilrmax
	use jmod, only: j_mslow
	use jmod, only: j_mecho
	use jmod, only: p_echo
	use jmod, only: j_0
	use jmod, only: p_feasible
	use jmod, only: p_oldc
	use jmod, only: j_mreport
	use jmod, only: j_ipchar
	use jmod, only: p_ivreport
	use jmod, only: j_iounit
	use jmod, only: p_nureport
	use jmod, only: j_getfile
	use jmod, only: p_p2
	use jmod, only: j_mrefac
	use jmod, only: p_nrefac
	use jmod, only: j_mtole
	use jmod, only: p_tolep
	use jmod, only: j_one
	use jmod, only: j_defmatrix
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: p_ivtole
	use jmod, only: p_nrowtot
	use jmod, only: p_tole
	use jmod, only: p_nnf
	use jmod, only: p_nnfold2
	use jmod, only: p_epsj
	use jmod, only: p_pivot
	use jmod, only: p_lbou
	use jmod, only: p_ubou
	use jmod, only: p_ebou
	use jmod, only: p_bbou
	use jmod, only: p_isres
	use jmod, only: p_lnf
	use jmod, only: p_ivubou
	use jmod, only: p_ivlbou
	use jmod, only: p_rhs2
	use jmod, only: p_p
	use jmod, only: p_rhs
	use jmod, only: p_lbb00
	use jmod, only: p_isbbou
	use jmod, only: p_rhsdif
	use jmod, only: p_lower
	use jmod, only: p_ivrhscur
	use jmod, only: p_rhscur
	integer, dimension(:),allocatable,target::varofterm,vars,rowofterm !,nterminrow
	double precision, dimension(:),allocatable,target::coef
	!	write(6,*)'*commonopt,iob,io',iob,io
 
	!write(6,*)'p_ivproblem',p_ivproblem
 
	call j_getoption(iob,io,j_mpullout,-1,1,0,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_ispullout=noptarg.ge.0
 
	if(noptarg.ge.1)then
 
		if(j_v(j_optarg0(1)).eq.0)p_ispullout=.false.
	endif !if(noptarg.ge.1)   9711
 
	call j_getoption(iob,io,j_mdebug,-1,2,0,.false.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.lt.0)then
		p_idebug1=-1
		p_idebug2=-1
	elseif(noptarg.eq.0)then
		p_idebug1=0
		p_idebug2=100000
	elseif(noptarg.eq.1)then
		p_idebug1=j_v(j_optarg0(1))
		p_idebug2=100000
	else
		p_idebug1=j_v(j_optarg0(1))
		p_idebug2=j_v(j_optarg0(2))
	endif !if(noptarg.lt.0)   9718
	j_v(j_ivdebug)=-1.
	idebu=j_igetopt(iob,io,j_mdebug)
	if(idebu.lt.0)then
		p_idebug=-1
	elseif(idebu.eq.0)then
		p_idebug=0
	else
 
		p_idebug=j_v(j_ivdebug)
	endif !if(idebu.lt.0)   9733
	! endif !if(nad.gt.0)   5072
	! call j_defmatrix(j_ivout,'%coef',p_nterm,1,j_matreg,iv)
	!write(6,*)'tasapopar ',p_ivpopar,j_ivout
	if(p_ivpopar.eq.0)then
		p_ivpopar=j_deflist(j_ivout,'%jlppar',list0=11)
		if(j_err)return
		j_o(p_ivpopar)%i2(1:11)= (/ p_ivfeasible, p_ivunbounded, p_ivobj, p_ivobj0, &
			p_ivoptimal,p_ivpivots,p_ivtmax,&
			p_ivlr0,p_ivlz0,p_ivlx0,p_ivrefac/)
	endif !if(p_ivpopar.eq.0)   9744
	p_ivunbounded=j_getobject(j_ivout,'%unbounded',j_ipreal)
	p_ivoptimal=j_getobject(j_ivout,'%optimal',j_ipreal)
	if(p_nrow.gt.0)then
		p_ivpivots=j_getobject(j_ivout,'%pivots',j_ipreal)
		p_ivstartedjlp=j_getobject(j_ivout,'%started_jlp',j_ipreal)
		p_ivtmax=j_getobject(j_ivout,'%tmax',j_ipreal)
		!	p_ivcolold=j_getobject(j_ivout,'%colold',j_ipreal)
		!	p_ivcolnew=j_getobject(j_ivout,'%colnew',j_ipreal)
		!	p_ivpivotcase=j_getobject(j_ivout,'%pivotcase',j_ipreal)
		!	p_ivpivotcases=j_deflist(j_ivout,'%pivotcases',list0=5,ilist=.true.)
 
		p_ivlr0=j_getobject(j_ivout,'%lr0',j_ipreal)
		p_ivlz0=j_getobject(j_ivout,'%lz0',j_ipreal)
		p_ivlx0=j_getobject(j_ivout,'%lx0',j_ipreal)
		!	p_ivlf0=j_getobject(j_ivout,'%lf0',j_ipreal)
		p_ivrefac=j_getobject(j_ivout,'%refac',j_ipreal)
		p_ivvaluek=j_getobject(j_ivout,'%valuek',j_ipreal)
		p_ivvalueopt=j_getobject(j_ivout,'%valueopt',j_ipreal)
		j_v(p_ivtmax)=j_ninf
	endif !if(p_nrow.gt.0)   9753
 
	p_refac=0 !number of refactorizations
	!	printlf=.false.
	ivprint7=j_igetopt(iob,io,j_mprint)
 
	!p_p=.true.
	p_nrecover=0
	!isslow=.false.
	!cycle2
 
	p_stoplink=j_codelink(iob,io,j_mstop); if(j_err)return
	p_isstop=p_stoplink.ne.0
 
	p_fastplink=j_codelink(iob,io,j_mfastp); if(j_err)return
	p_isfastp=p_fastplink.ne.0
 
	p_again=.false.
	p_nagain=0
	!	p_n16=17
	!		simobug=.true.
	!	write(6,*)'heppp'
	p_ilrmax=1  !to avoi array bounds exeeded situation in point !assshshdhs
	nout=0
	call j_getoption(iob,io,j_mslow,-1,100,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	!	call j_getoption(iob,io,j_mslow,-1,1,j_ipreal,.true.,nslow,slowopt)
	if(noptarg.ge.0)write(6,*)'slow-> is obsolete, use stop->'
	!write(6,*)'<56',iob
 
	call j_getoption(iob,io,j_mecho,-1,1,j_ipreal,.false.,noptarg,j_optarg0)
	if(j_err)return
	!	call j_getoption(iob,j_mslow,-1,1,j_ipreal,.true.,nslow,slowopt)
	p_echo=.true.
	if(noptarg.gt.0)then
		if(j_v(j_optarg0(1)).le.j_0)p_echo=.false.
	endif !if(noptarg.gt.0)   9804
	!write(6,*)'huppp'
	p_feasible=.false.
	p_oldc=-1
	call j_getoption(iob,io,j_mreport,-1,1,j_ipchar,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.eq.1)then
		p_ivreport=j_optarg0(1) ! j_o(iob)%i(j_linkoption(iob,io,j_mreport)+1)
 
		!	if(j_otype(p_ivreport).eq.j_ipchar)then
		p_nureport=j_iounit(p_ivreport) !j_o(p_ivreport)%i(4)  ! file unit when character object is a file name
		if(p_nureport.le.0)then
			!subroutine j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout)
			call j_getfile(nuu,'w',p_ivreport)
			!	write(6,*)'<545>',nuu,p_ivreport
			!call j_getwritefile(p_ivreport,j_ivdollar,bin)
			if(j_err)return
			p_nureport=nuu  !j_iounit(p_ivreport) !j_o(p_ivreport)%i(4)
		endif !if(p_nureport.le.0)   9817
 
	else !if(j_nargopt(iob,io,j_mreport).eq.1)then
		p_nureport=6
	endif !if(noptarg.eq.1)   9812
 
	!p_p=.false.
	p_p2=.false.
 
 
	!data *******************************
	!	j_ivout =j_o(iob)%i(io+2+j_o(iob)%i(io+1))
	!	if(j_otype(j_ivout).ne.j_ipreal)call j_del(j_ivout)
	call j_getoption(iob,io,j_mrefac,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.gt.0)then
		p_nrefac=j_v(j_optarg0(1))
	else !if(j_linkoption(iob,io,j_mrefac).gt.0)then
		p_nrefac=1000  !
	endif !if(noptarg.gt.0)   9839
	!	write(6,*)'refac=',p_nrefac
 
	! tole->
	call j_getoption(iob,io,j_mtole,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.gt.0)then
 
		p_tolep=j_v(j_optarg0(1))
	else !if(j_linkoption(iob,io,j_mtole).gt.0)then
		p_tolep=j_one
	endif !if(noptarg.gt.0)   9849
	!write(6,*)'tole=',j_tolep
	!write(6,*)'tassanyt&& nrow ',p_nrow
 
	! nrow tell the total number of rows when domain sets are expanded
7777 format(a16,i5)
	!if(p_xpresent)write(6,7777)'domains',max(p_ndom,1)
 
	!	endif !if(.not.p_zmatrix)  13009
 
	p_ivtole=j_defmatrix(j_ivout,'%tole',j_18,int8(p_nrowtot),j_matreg)
	p_tole(0:)=>j_o(p_ivtole)%d(1:p_nrowtot)
	! if(allocated(p_tole))deallocate(p_tole)
	! allocate(p_tole(0:p_nrow))
 
	! p_nnf = number of infeasible rows
	p_nnf=p_nrow  !initially all infeasible
 
	!write(6,*)'<7575 p_nnf',p_nnf
	p_nnfold2=p_nrow ! old number of infeasible rows is initially the same
	p_tole=p_epsj*p_tolep
	p_pivot=0
 
	!if (.not.p_zmatrix) then    !!!! initilaization continues
	if(p_nrow.eq.0)return
 
	if(allocated(p_lbou))deallocate(p_lbou);if(allocated(p_ubou))deallocate(p_ubou)
	allocate( p_lbou(1:p_nrow));allocate( p_ubou(1:p_nrow)) !deallo at 950
	if(allocated(p_ebou))deallocate(p_ebou);allocate(p_ebou(1:p_nrow))
	if(allocated(p_bbou))deallocate(p_bbou);allocate(p_bbou(1:p_nrow))
	! if(allocated(p_ln))deallocate(p_ln);allocate(p_ln(1:p_nrow))
	! if(allocated(p_lni))deallocate(p_lni);allocate(p_lni(1:p_nrow))
	if(allocated(p_isres))deallocate(p_isres);allocate(p_isres(1:p_nrow))
	if(allocated(p_lnf))deallocate(p_lnf);allocate(p_lnf(1:p_nrow))
	p_isres=.true.
	!write(6,*)'pnrddow',p_nrow
 
	p_ivubou=j_deflist(j_ivout,'%ubou',list0=p_nrow,ilist=.true.) !p-variables
	p_ivlbou=j_deflist(j_ivout,'%lbou',list0=p_nrow,ilist=.true.) !p-variables
	!write(6,*)'tassan5i rhs2,p_nrow ',p_nrow,p_ivlbou,p_ivubou
	!write(6,*)'ubou',p_ubou,p_rhs2
 
	p_ubou=p_rhs2.lt.1.d20  !j_inf   ! is there upper bound
	if(p_p)write(6,*)'ubou',p_ubou
	!if(p_p)write(6,*)'lbou',p_ubou
	p_lbou=p_rhs.gt.-1.d19 !_ninf
 
	p_ebou=p_rhs.eq.p_rhs2
	p_bbou=p_lbou.and.p_ubou.and..not.p_ebou
	p_lbb00=count(p_bbou)
	p_isbbou=p_lbb00.gt.0
	! if(p_isbbou)then
	! if(allocated(p_lbb))deallocate(p_lbb)
	! allocate(p_lbb(1:p_lbb00))
	! lkm=0
	! do i=1,p_nrow
	! if(p_bbou(i))then
	! lkm=lkm+1
	! p_lbb(lkm)=i
	! endif !if(p_bbou(i))   9281
 
	! enddo !i=1,p_nrow   9280
	! p_lbb0=p_lbb00
 
	! endif !if(p_isbbou)   9276
	if(allocated(p_rhsdif))deallocate(p_rhsdif)
	allocate(p_rhsdif(1:p_nrow))
 
	do i=1,p_nrow
		if(p_rhs(i).gt.p_rhs2(i))then
			write(6,*)'row ',i,' rhs ',p_rhs(i),' greater than rhs2 ' ,p_rhs2
			j_err=.true.
 
		endif !if(p_rhs(i).gt.p_rhs2(i))   9923
		if(p_bbou(i))p_rhsdif(i)=p_rhs2(i)-p_rhs(i)
 
	enddo !i=1,p_nrow   9922
	if(j_err)return
	if(p_p)write(6,*)'lbou',p_lbou
	if(p_p)write(6,*)'***rhs2',p_rhs2
	if(p_p)write(6,*)'***ubou',p_ubou
	!write(6,*)'allolow'
	!	read(5,*)u
	if(allocated(p_lower))deallocate(p_lower)  ! is lower bound active
	allocate(p_lower(1:p_nrow))
 
	! write(6,*)'***rhs',p_rhs
	! write(6,*)'***bou',p_lbou
 
	j_o(p_ivlbou)%i2(1:p_nrow)=p_lbou
	j_o(p_ivubou)%i2(1:p_nrow)=p_ubou
	!endif !if (.not.p_zmatrix)   4371
	if(p_p)write(6,*)'<768'
	!!!! intilization continues
 
	!is(p_p.and.p_nrow.le.50)p_p2=.true.
	!	if(p_p2)then
	! write(6,*)'rhs',p_rhs,j_ninf,j_o(p_ivrhsvars)%i2,j_ivninf
	! write(6,*)'rhs2',p_rhs2
	! write(6,*)'maxo',p_maxo
	! write(6,*)'lbou',p_lbou
	! write(6,*)'ubou',p_ubou
	!	endif !if(p_p2)   5022
	if(p_p)write(6,*)'tassanythhs nrow ',p_nrow
	3096 continue
 
	p_ivrhscur=j_defmatrix(j_ivout,'%rhscur',int8(p_nrow),j_18,j_matreg)
	p_rhscur=>j_o(p_ivrhscur)%d(1:p_nrow)
	!	if(allocated(p_rhscur))deallocate(p_rhscur);allocate( p_rhscur(1:p_nrow))
 
	! rhsw: the working rhs where the sums over key schedules are subtracted
	!	if(p_xpresent)then !this is allocated even if it will be equalt rhscur when there is no data
	!if(allocated(p_rhsw))deallocate(p_rhsw);allocate( p_rhsw(1:p_nrow))
	!endif !if(p_xpresent)   4396
 
end subroutine commonopt



subroutine initjlp2(iob,io)
	use jmod, only: p_isn16
	use jmod, only: j_getoption
	use jmod, only: j_mmaxrounds
	use jmod, only: j_ipreal
	use jmod, only: j_optarg0
	use jmod, only: j_err
	use jmod, only: j_v
	use jmod, only: p_maxrounds
	use jmod, only: j_mz
	use jmod, only: j_ipmatrix
	use jmod, only: p_zopt
	use jmod, only: j_mdx
	use jmod, only: p_tolecur
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivix
	use jmod, only: p_nrowtot
	use jmod, only: j_o
	use jmod, only: p_ix
	use jmod, only: p_tried
	use jmod, only: p_fastusedsame
	use jmod, only: p_fastnow
	use jmod, only: p_intapp
	use jmod, only: p_fastmake
	use jmod, only: j_minteger
	use jmod, only: j_isoption
	use jmod, only: j_mfast
	use jmod, only: p_fast
	use jmod, only: j_mfastdif
	use jmod, only: p_fastdif
	use jmod, only: p_fastusesame
	use jmod, only: j_igetopt
	use jmod, only: j_mfastrounds
	use jmod, only: p_fastpros
	use jmod, only: j_ivfastp
	use jmod, only: j_mfinterval
	use jmod, only: p_fastpros2
	use jmod, only: p_nkeys
	use jmod, only: p_nresw
	use jmod, only: p_npivotw
	use jmod, only: p_nkeyfactw
	use jmod, only: j_mwarm
	use jmod, only: p_warm
	use jmod, only: p_warmf
	write(6,*)'*initjlp2'
	if(p_isn16)write(16,*)'*initjlp2'
	!get options and when xpresent
	! p_ivtrans
	! p_maxrounds
	! p_zopt
	! p_ivix
	! p_ix
	! p_nnotareavars
	! p_tried
	! p_fastusedsame
	! p_intapp
	! p_fastmake
	! p_fastpros p_fastpros2  /100
	! p_fast
	! p_warm p_warmf
 
	!p_ivtrans=j_igetopt(iob,io,j_mtrans) !object given in trans-option
	!write(6,*)'<187>p_ivtrans',p_ivtrans
 
	call j_getoption(iob,io,j_mmaxrounds,-1,999999,j_ipreal,.true.,nargopt,j_optarg0)
	if(j_err)return
	if(nargopt.gt.0)then
		p_maxrounds=j_v(j_optarg0(1))
 
	else !if(j_linkoption(iob,io,j_mmaxiter).gt.0)then
		p_maxrounds=3000
	endif !if(nargopt.gt.0)   9996
	!	write(6,*)'djjdp_nvar ',p_nvar
	write(6,*)'maxrounds ',p_maxrounds
	call j_getoption(iob,io,j_mz,-1,0,j_ipmatrix,.false.,noptarg,j_optarg0)
	if(j_err)return
	p_zopt=noptarg.ge.0
 
	call j_getoption(iob,io,j_mdx,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_tolecur=1.d-6
	if(noptarg.gt.0)p_tolecur=j_v(j_optarg0(1))
 
	!write(6,*'<556> p_nrow',p_nrow
	!if(allocated(p_ix))deallocate(p_ix)
	p_ivix=j_deflist(j_ivout,'%ix',list0=p_nrowtot,ilist=.true.)
	p_ix(0:)=>j_o(p_ivix)%i2(1:p_nrowtot)
 
	!write(6,*)'pix ',p_ix
	!	allocate(p_ix(0:p_nrow));p_ix=0
 
 
 
	p_tried=.false.
	p_fastusedsame=0
	p_fastnow=.false.
	p_intapp=.false.  ! added by JL 112018
	p_fastmake=.false.
	! optioksi
	! fastrounds-> how many rounds the same active set is used, 1 indicates that no active set is generated but
	! all schedules are always used
	! default value if 10
	! fastpercent -> a chedules is included in the active set if the shadow value is at lest fastpercetn* the value
	! of the optimal schedule
	! default is 90
 
	! finterval->   it is checked wether new factories can enter if the round number is jaollinen with finterval
	! default is 3. his option applies only after finding feasible
 
	! p_fastusesame= value of fastround
	! faspros= fastpercent
	! iterxkf=finterval
	call j_getoption(iob,io,j_minteger,-1,0,0,.false.,noptarg,j_optarg0)
	if(j_err)return
	!	call j_getoption(iob,j_mslow,-1,1,j_ipreal,.true.,nslow,slowopt)
	p_intapp=noptarg.ge.0
 
	p_fast=j_isoption(iob,io,j_mfast,.true.)
	if(p_fast)then
		call j_getoption(iob,io,j_mfastdif,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(j_err)return
		if(noptarg.ge.1)p_fastdif=j_v(j_optarg0(1))
 
		! fast-optiot
		p_fastusesame = 10
		i = j_igetopt(iob,io,j_mfastrounds)
		if(i > 0) p_fastusesame = j_v(i)
 
 
		p_fastpros = 95
		j_v(j_ivfastp)=p_fastpros
 
		iterxkf = 3
		i = j_igetopt(iob,io,j_mfinterval)
		if(i>0) iterxkf = j_v(i)
 
		p_fastpros2=p_fastpros/100.
	endif !if(p_fast)  10048
	!	p_fast=p_fastusesame.ne.1
	!write(6,*)'<2345kukuu'
	p_nkeys=0
 
	!p_ntote=0
	!	p_ilres=0
	p_nresw=0
	p_npivotw=0
	p_nkeyfactw=0
	i=j_igetopt(iob,io,j_mwarm)
	p_warm=.false.
	if(i.gt.0)then
		p_warm=j_v(i).ne.0.
	elseif(i.eq.0)then !if(i.gt.0)then
		p_warm=.true.
	endif !if(i.gt.0)  10079
	!keys key schedules for each unit
	p_warm=.false.
 
	p_warmf=p_warm
 
 
end subroutine initjlp2


subroutine initxdatawx()
	use jmod, only: j_defmatrix
	use jmod, only: j_ivout
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: p_ivxcoef
	use jmod, only: p_nxval
	use jmod, only: j_o
	use jmod, only: p_xcoef
	use jmod, only: j_deflist
	use jmod, only: p_ivxvarofterm
	use jmod, only: p_xvarofterm
	use jmod, only: p_nterm
	use jmod, only: p_istermx
	use jmod, only: p_coef
	use jmod, only: p_varofterm
	use jmod, only: p_ivrowcurx
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: p_rowofterm
	use jmod, only: p_ntemp0
	use jmod, only: p_md
	use jmod, only: p_ibatemp
	use jmod, only: p_nrowxtemp
	use jmod, only: p_nrowtot
	use jmod, only: p_xpsrow
	use jmod, only: p_nrow
	use jmod, only: p_ninrowx
	use jmod, only: p_ix
	use jmod, only: p_row0
	use jmod, only: p_xvar1
	use jmod, only: p_xvar2
	use jmod, only: p_nxvar
	use jmod, only: p_ivirowxvar
	use jmod, only: p_irowxvar
	use jmod, only: p_ivnrowx2
	use jmod, only: p_nrowx2
	use jmod, only: j_dnobs
	use jmod, only: p_ivxmat
	use jmod, only: p_xmat
	use jmod, only: p_vx
	use jmod, only: p_xmatrow
	use jmod, only: p_ivxvar
	use jmod, only: p_xvar
	use jmod, only: p_vars
	use jmod, only: p_nvar
	use jmod, only: p_isxvar
	write(6,*)'*initxdataw'  !,'ixxx',p_ix
	! inits something of x-data used also with factories and without ordinary x-variables
	!initilizes also some fact issues
 
	p_ivxcoef=j_defmatrix(j_ivout,'%xcoef',int8(p_nxval),j_18,j_matreg)
	p_xcoef=>j_o(p_ivxcoef)%d(1:p_nxval)
	p_ivxvarofterm=j_deflist(j_ivout,'%xvarofterm',list0=p_nxval)
	p_xvarofterm=>j_o(p_ivxvarofterm)%i2(1:p_nxval)
 
	nxval=0
	do i=1,p_nterm
		if(p_istermx(i))then
			nxval=nxval+1
			p_xcoef(nxval)=p_coef(i)
			p_xvarofterm(nxval)=p_varofterm(i)
		endif !if(p_istermx(i))  10105
	enddo !i=1,p_nterm  10104
	!	write(6,*)'nrowcurx',p_nrowcurx
 
	p_ivrowcurx=j_deflist(j_ivout,'%rowcurx',list0=p_nrowcurx,ilist=.true.)
	p_rowcurx=>j_o(p_ivrowcurx)%i2(1:p_nrowcurx)
	nrowx=0
	irov=-1
	! write(6,*)'p_nterm ',p_nterm
	! write(6,*)'p_isxva ',p_istermx
	! write(6,*)'row of ter ',p_rowofterm
	do ival=1,p_nterm
		iro=p_rowofterm(ival)
		!	write(6,*)'ival,iro,irov',ival,iro,irov
		if(p_istermx(ival).and.iro.ne.irov)then
			nrowx=nrowx+1
			p_rowcurx(nrowx)=iro
			!	write(6,*)'iro nrowx ',iro,nrowx
		endif !if(p_istermx(ival).and.iro.ne.irov)  10123
		irov=iro
	enddo !ival=1,p_nterm  10120
 
	!	write(6,*)'rowcurxtas ',p_rowcurx
 
	!	stop
	p_ntemp0=0 !the number of the temp-varaible
 
	p_md=0 !number of x-rows in the problem
 
	!	write(6,*)'p_nrow',p_nrow
	!*************   here it is checked if the same coefficients and
	! xvariables  appears in different rows , make then only one temporary	x variable
 
	!	p_ix=0
	!	p_nrowx=0
	!	!is(p_p8)write(6,*)'<4624,p_nrow',p_nrow,'p_nrowcurx ',p_nrowcurx,'*',p_rowcurx
	!	p_nrowcurx=0
	!	!is(p_p8)write(6,*)'<4624,p_nrow',p_nrow,'p_nrowcurx ',p_nrowcurx
	!is(p_p8)write(6,*)'<varofterm',p_varofterm(1:p_nterm)
	!write(6,*)'coeftas',p_xcoef
	!write(6,*)'nxinrow',p_ninrowx(iro)
	if(allocated(p_ibatemp))deallocate(p_ibatemp)
	if(allocated(p_nrowxtemp))deallocate(p_nrowxtemp)
 
	allocate(p_ibatemp(1:p_nrowtot),p_nrowxtemp(1:p_nrowtot))
	p_ibatemp=0;p_nrowxtemp=0
	iba=0
	if(allocated(p_xpsrow))deallocate(p_xpsrow);allocate(p_xpsrow(1:p_nrow))
	write(6,*)'rows with x ',p_ninrowx
	write(6,*)'x-terms ',p_xvarofterm
	!write(6,*)'p_coef',p_coef
rowloop:	do iro=0,p_nrow
		if(iro.gt.0)iba=iba+p_ninrowx(iro-1)
		if(p_ninrowx(iro).gt.0)then
			!		p_nrowcurx=p_nrowcurx+1
			!		p_rowcurx(p_nrowcurx)=iro
			!check previous rows
			iba2=0
			irof=0
			!		if(p_ispiece)irof=1  !??
	row2:		do iro2=irof,iro-1
				if(iro2.gt.0)iba2=iba2+p_ninrowx(iro2-1)
				if(p_ninrowx(iro).eq.p_ninrowx(iro2))then
 
			outer:	do jj=1,p_ninrowx(iro)
						do jj2=1,p_ninrowx(iro)
 
 
							if(p_xvarofterm(iba+jj).ne.p_xvarofterm(iba2+jj2).or.&
								p_xcoef(iba+jj).ne.p_xcoef(iba2+jj2))cycle row2
 
							! if(p_xvarofterm(iba+jj).eq.p_xvarofterm(iba2+jj2))then
							! if(p_xcoef(iba+jj).eq.p_xcoef(iba2+jj2))then
							! cycle outer
							! else
							! cycle row2
							! endif !if(p_xcoef(iba+jj).eq.p_xcoef(iba2+jj2))   9961
							! else
							! cycle row2
							! endif !if(p_xvarofterm(iba+jj).eq.p_xvarofterm(iba2+jj2))   9960
 
						enddo !jj2=1,p_ninrowx(iro)  10174
 
					enddo outer !er:	do jj=1,p_ninrowx(iro)  10173
					!!! now all variables and coefficients are equal
					p_ix(iro)=p_ix(iro2)
					cycle rowloop !write(6,*)'samma x',iro,p_irow0,'ix',p_itemp0
				endif !if(p_ninrowx(iro).eq.p_ninrowx(iro2))  10171
				!	iba2=iba2+p_ninrowx(iro2)
			enddo row2 !2:		do iro2=irof,iro-1  10169
			p_ntemp0=p_ntemp0+1
			!		p_ixprow(iro)=p_itemp0
			p_ix(iro)=p_ntemp0 !tells for each row what is teporary
			!	p_rowcurx(nrowx)=iro
			!number of rows with x
			p_ibatemp(p_ntemp0)=iba
			p_nrowxtemp(p_ntemp0)=p_ninrowx(iro)
			!	write(6,*)' iro ',iro,' p_ntemp0 ',p_ntemp0
 
		endif !if(p_ninrowx(iro).gt.0)  10162
 
		!	iba=iba+p_ninrowx(iro)
	enddo rowloop !loop:	do iro=0,p_nrow  10160
 
	!rowcurx is row where the x -variable is and ix is the
	! nrowcurx is the number of rows having x-variables
	! do jj=1,p_nrowcurx
	! j=p_rowcurx(jj)
	! !j_value=j_value+j_vx(j)*j_xmat(j_ix(j),is) !,is) !v(ix(j))
	! p_value=p_value+p_vx(j)*p_xmat(ibaxmat+p_ix(j)) !,is) !v(ix(j))
	! enddo !jj=1,p_nrowcurx   1964
 
 
	p_row0=1
	if(p_rowcurx(1).eq.0)p_row0=2
 
	if(allocated(p_xvar1))deallocate(p_xvar1,p_xvar2)  !xvar1 and xvar2 used to chech duplicated sched
	allocate(p_xvar1(1:p_nxvar),p_xvar2(1:p_nxvar)) !used
 
 
 
 
	p_ivirowxvar=j_deflist(j_ivout,'%irowxvar',list0=p_nxval)
	p_irowxvar=>j_o(p_ivirowxvar)%i2(1:p_nxval)
	p_ivnrowx2=j_deflist(j_ivout,'%nrowx2',list0=p_nrowcurx,ilist=.true.)
	p_nrowx2=>j_o(p_ivnrowx2)%i2(1:p_nrowcurx)
	!take coefficients of x, and the correponding variables
 
 
	!	write(6,*)'xcoefaft',p_xcoef
	!is(p_p8)write(6,*)'<999pack'  !,p_varofterm,'*',p_istermx
	!	write(6,*)'varofterm',p_varofterm
	!	write(6,*)'varofterm',p_istermx
 
	p_irowxvar=pack(p_varofterm,p_istermx)
	!take
	!is(p_p8)write(6,*)'<4779pack'  !,p_nrowx,'*',p_nrowx.gt.0
	p_nrowx2=pack(p_ninrowx,p_ninrowx.gt.0) !numbers of x-variables in each
	!	if(p_nrow.gt.0)then
	p_ivxmat=j_defmatrix(j_ivout,'%xmat',int8(j_dnobs),int8(p_ntemp0),j_matreg,single=.true.)
	p_xmat=>j_o(p_ivxmat)%r(1:p_ntemp0*j_dnobs)
	write(6,*)'allocate xmat:',j_dnobs,' rows ',p_ntemp0,' columns ',p_ntemp0*j_dnobs/1.e6,' million elements'
	!	endif !if(p_nrow.gt.0)   7075
	!	if(allocated(p_xmat))deallocate(p_xmat)
	if(allocated(p_vx))deallocate(p_vx,p_xmatrow);allocate(p_vx(1:p_ntemp0),p_xmatrow(1:p_ntemp0))
 
	!	allocate(p_xmat(1:p_ntemp0*j_dnobs))
	!	p_ivvxpack=j_defmatrix(j_ivout,'%vxpack',int8(p_ntemp0),j_18,j_matreg,single=.true.)
	!	p_vxpack=>j_o(p_ivvxpack)%r(1:p_ntemp0)
 
 
	!	p_ivixpack=j_deflist(j_ivout,'%ixpack',list0=p_ntemp0,ilist=.true.)
	!	p_ixpack=>j_o(p_ivixpack)%i2(1:p_ntemp0)
 
	p_ivxvar=j_deflist(j_ivout,'%xvar',list0=p_nxvar)
	p_xvar=>j_o(p_ivxvar)%i2(1:p_nxvar)
	!write(6,*)'pnxvar',p_nxvar
 
	!	write(6,*)'<56pack,,p_nvar',p_nvar,p_nxvar
	p_xvar=pack(p_vars(1:p_nvar),p_isxvar)   ! c-variables are here included as x -variables
 
end subroutine initxdatawx


subroutine initxdata()
	use jmod, only: p_isn16
	use jmod, only: p_fast
	use jmod, only: p_fastreject
	use jmod, only: j_dnobs
	use jmod, only: p_rejects
	use jmod, only: p_xsmin
	use jmod, only: p_xsmax
	use jmod, only: p_xmin
	use jmod, only: p_xmax
	use jmod, only: j_defmatrix
	use jmod, only: j_ivout
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: p_ivxps
	use jmod, only: p_nrowtot
	use jmod, only: j_o
	use jmod, only: p_xps
	use jmod, only: p_nrow
	use jmod, only: p_ivvaluedif
	use jmod, only: p_nunits
	use jmod, only: p_ivobjdif
	write(6,*)'*initxdata'
	if(p_isn16)write(16,*)'*initxdata'
	! p_fastreject
	! p_rejects
	! p_xsmin p_xsmax
	! p_xmin p_xmax
	! p_ivxps
	! p_xps
	! p_ivvaluedif   j_defmatrix(j_ivout,'%valuedif',p_nunits,1,j_matreg)
	!	p_ivobjdif       j_defmatrix(j_ivout,'%objdif',p_nunits,1,j_matreg)
 
	if(p_fast)then
		if(allocated(p_fastreject))deallocate(p_fastreject)
		allocate(p_fastreject(1:j_dnobs))
		p_fastreject=.false.
 
	endif !if(p_fast)  10285
 
 
	if(allocated(p_rejects))deallocate(p_rejects)
	! subreject-> option was given
	!	if(subfilre)then;allocate(p_rejects(1:j_dnobs));p_rejects=.false.;endif
	allocate(p_rejects(1:j_dnobs));p_rejects=.false.
	!	allocate(p_nunitsrow(0:p_nrow));p_nunitsrow=0
 
 
 
	!if(allocated(p_xps))deallocate(p_xps)  ! sums of x-variables  over key schedules
	if(allocated(p_xsmin))deallocate(p_xsmin) ! smallest possible sum
	if(allocated(p_xsmax))deallocate(p_xsmax)   !largest possible sum
	if(allocated(p_xmin))deallocate(p_xmin)
	if(allocated(p_xmax))deallocate(p_xmax)
 
	p_ivxps=j_defmatrix(j_ivout,'%xps',int8(p_nrowtot),j_18,j_matreg)
	p_xps(0:)=>j_o(p_ivxps)%d(1:p_nrowtot)
 
	allocate(p_xsmin(0:p_nrow),p_xsmax(0:p_nrow))
	allocate(p_xmin(0:p_nrow),p_xmax(0:p_nrow))
 
	p_ivvaluedif=j_defmatrix(j_ivout,'%valuedif',int8(p_nunits),j_18,j_matreg)
	p_ivobjdif=j_defmatrix(j_ivout,'%objdif',int8(p_nunits),j_18,j_matreg)
	! endif !if(size(p_keys).ne.p_nunits)   6203
	! endif !if(p_warm)   6202
	! if(.not.p_warm)deallocate(p_keys)
	! endif !if(allocated(p_keys))   6201
 
 
	!allocate(p_ibaunit(1:p_nunits+1))
 
	!if(p_p8)write(6,*)'<5029 allocating p_ibaunit ',p_nunits+1
 
end subroutine initxdata

subroutine startlist()
	use jmod, only: p_xmi
	use jmod, only: p_xma
	use jmod, only: p_nrow2z
	use jmod, only: j_0
	use jmod, only: p_aopt
	use jmod, only: p_nrowtot
	use jmod, only: p_akey
	use jmod, only: p_aopt0
	use jmod, only: p_akey0
	use jmod, only: p_anew0
	use jmod, only: p_akey00
	use jmod, only: p_nrow
	use jmod, only: p_abas1
	use jmod, only: p_ncol
	use jmod, only: p_xpresent
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivlx
	use jmod, only: j_o
	use jmod, only: p_lx
	use jmod, only: p_ivlxi
	use jmod, only: p_lxi
	! p_ivls p_ls
	! p_ivlsi p_lsi
	! p_ivlr p_lr
	! p_ivlri p_lri
	! p_ivobjr0
	! p_objr0
	! p_ivobjr2
	! p_objr2
	! p_ivlz
	! p_lz
	! p_ivlx
	! p_lx
	! p_ivlxi
	! p_lxi
	!
 
	! p_lfi
	! p_ivvc
	! p_vc
	! p_ivx
	! p_x
	! p_ivb
	! p_b
	! p_xmi
	! p_xma
	! p_iva
	! p_a
 
 
	!write(6,*)'startlist',p_nrow2z,p_nrow2z
	!p_nrowz=p_nrow+p_nz  ! basis (first element -1) of D part, when I is included
	! p_nrow2z=p_ncol+p_nrow
	! p_nrowz=p_nz
	! p_nrow2z=p_nrowz+p_nrow
	! p_zopt=.true.
 
 
	! z),j_18,j_matreg)
	! p_b=>j_o(p_ivb)%d(1:p_nrowz)
 
 
	! if(allocated(p_objr0))deallocate(p_objr0)
	! if(allocated(p_objr2))deallocate(p_objr2)
	! allocate( p_objr0(1:p_nrow2z));allocate( p_objr2(1:p_nrow2z));;p_objr0=j_0;p_objr2=j_0
	if(allocated(p_xmi))deallocate(p_xmi,p_xma);if(allocated(p_xma))deallocate(p_xma)
	allocate( p_xmi(1:p_nrow2z),p_xma(1:p_nrow2z))
	p_xmi=j_0
	p_xma=j_0
 
	if(allocated(p_aopt))deallocate(p_aopt)
	allocate(p_aopt(1:p_nrowtot*p_nrowtot))
 
	if(allocated(p_akey))deallocate(p_akey)
	allocate(p_akey(1:p_nrowtot*p_nrowtot))
 
	if(allocated(p_aopt0))deallocate(p_aopt0,p_akey0,p_anew0,p_akey00)
	allocate(p_aopt0(0:p_nrow),p_akey0(0:p_nrow),p_anew0(0:p_nrow),p_akey00(0:p_nrow))
 
	if(allocated(p_abas1))deallocate(p_abas1)
	allocate(p_abas1(1:p_ncol))
	p_abas1(1)=0
	do j=2,p_ncol
 
		p_abas1(j)=p_abas1(j-1)+p_nrowtot
	enddo !j=2,p_ncol  10389
 
 
	!	call j_defmatrix(0,'po%lws'//ch,1,p_mxlws,j_matreg,ivls)
	!	j_o(ivls)%d(1:p_mxlws)=LWSLL1(1:p_mxlws)
 
	if(p_xpresent)then
		p_ivlx=j_deflist(j_ivout,'%lx',list0=p_nrowtot,ilist=.true.)
		p_lx=>j_o(p_ivlx)%i2(1:p_nrowtot)
 
		p_ivlxi=j_deflist(j_ivout,'%lxi',list0=p_nrowtot,ilist=.true.)
		p_lxi=>j_o(p_ivlxi)%i2(1:p_nrowtot)
 
		do j=1,p_nrowtot
			p_lx(j)=j
			p_lxi(j)=j
 
		enddo !j=1,p_nrowtot  10405
 
 
 
	endif !if(p_xpresent)  10398
 
 
	call startlist0()
 
 
	return
 
end subroutine startlist



subroutine initarea(iob,io)
	use jmod, only: j_getoption
	use jmod, only: j_marea
	use jmod, only: j_ipreal
	use jmod, only: j_optarg0
	use jmod, only: j_err
	use jmod, only: p_ivarea
	use jmod, only: p_isarea
	use jmod, only: p_nnotareavars
	use jmod, only: j_mnotareavars
	use jmod, only: p_notareavars
	use jmod, only: j_ndiffer
	use jmod, only: j_o
	use jmod, only: j_divkeep
	use jmod, only: p_nxvararea
	use jmod, only: p_nxvartot
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivxvararea
	use jmod, only: p_xvararea
	use jmod, only: j_differ
	use jmod, only: p_isunit
	use jmod, only: j_inlistobject
	use jmod, only: p_areakeep
	use jmod, only: j_getname
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: j_divkeepup
	write(6,*)'*initarea'
	call 	j_getoption(iob,io,j_marea,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_ivarea=0
	p_isarea=.false.
	p_nnotareavars=0
	if(noptarg.gt.0)then
		p_ivarea=j_optarg0(1)
		call j_getoption(iob,io,j_mnotareavars,-1,999999,j_ipreal,.true.,p_nnotareavars,p_notareavars)
		!	write(6,*)'notareavars ',p_nnotareavars
		if(j_err)return
		p_isarea=.true.
		!	notareav=j_nargopt(iob,io,j_mnotareavars) !number of p_notareavars-> -variables
		!	if(allocated(p_xvararea))deallocate(p_xvararea)
		if(p_nnotareavars.gt.0)then
			p_nxvararea=j_ndiffer(j_o(j_divkeep)%i2,p_nxvartot,p_notareavars,p_nnotareavars)
			!	write(6,*)'hh ',p_nxvararea,p_nxvartot
			!	notc=j_ndiffer(p_cvarl,p_ncvar,p_notareavars,p_nnotareavars) !j_o(iob)%i(linknotareavars+1:linknotareavars+p_notareavars),p_notareavars)
			if(p_nxvararea.lt.p_nxvartot)then !  .or.notc.gt.0)then
				p_ivxvararea=j_deflist(j_ivout,'%xvararea',list0=p_nxvararea)
				p_xvararea=>j_o(p_ivxvararea)%i2(1:p_nxvararea)
				!	allocate(p_xvararea(1:p_nxvararea)) !+notc))
				!subroutine j_differ(list1,n1,list2,n2,list3,n3)
				!picks from %%list1 elements which are not in list2 to list3
				!list1 and list3 can be the same as well n1 and n3
				call j_differ(j_o(j_divkeep)%i2(1:p_nxvartot),p_nxvartot,p_notareavars,p_nnotareavars,&
					p_xvararea,p_nxvararea)
				!	call j_uniondif(j_o(j_divkeep)%i2,p_nxvartot,p_cvarl,p_ncvar,&
				!	p_notareavars,p_nnotareavars,&
				!		j_o(iob)%i(linknotareavars+1:linknotareavars+p_notareavars),p_notareavars,&
				!p_xvararea,p_nxvararea)
				!		p_ipart=.true.
 
 
 
			endif !if(p_nxvararea.lt.p_nxvartot)  10444
 
		endif !if(p_nnotareavars.gt.0)  10440
 
 
		if(p_isunit)then
			!		if(p_isarea)then
			p_areakeep=j_inlistobject(p_ivarea,j_divkeep)
			if(p_areakeep.le.0)then
				call j_getname(p_ivarea)
				write(6,*)'area-> variable ',j_oname(1:j_loname),' is not in the data'
				j_err=.true.;return
			endif !if(p_areakeep.le.0)  10469
			!	endif !if(p_isarea)  13899
 
		else
			!	if(p_isarea)then
			p_areakeep=j_inlistobject(p_ivarea,j_divkeepup)
			if(p_areakeep.le.0)then
				call j_getname(p_ivarea)
				write(6,*)'area-> variable ',j_oname(1:j_loname),' is not in the up-data'
				j_err=.true.;return
			endif !if(p_areakeep.le.0)  10479
			!	endif !if(p_isarea)  13912
 
 
 
		endif !if(p_isunit)  10466
		!	write(6,*)'yyy ',p_nxvararea,p_xvararea
 
	endif !if(noptarg.gt.0)  10432
	if(noptarg.le.0.or.p_nnotareavars.eq.0)then !if(p_isarea.and.p_notareavars.gt.0)then
		p_nxvararea = p_nxvartot !+ p_noutsubtrans
		!		allocate(p_xvararea(1:p_nxvararea))
		p_xvararea=>j_o(j_divkeep)%i2(1:p_nxvartot)
		!	p_xvararea(1:p_nxvartot)= j_o(j_divkeep)%i2(1:p_nxvartot)
		!	p_ipart=.false.
		!	if(p_noutsubtrans>0) p_xvararea(p_nxvartot+1:p_nxvararea)=j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans)
	endif !if(noptarg.le.0.or.p_nnotareavars.eq.0)  10492
 
end subroutine initarea



subroutine initfact()
	use jmod, only: p_nunits
	use jmod, only: p_nlog
	use jmod, only: p_logpart
	use jmod, only: p_ncaprow
	use jmod, only: p_nterminrow
	use jmod, only: p_nterm
	use jmod, only: p_rowofterm
	use jmod, only: p_factpart
	use jmod, only: j_0
	use jmod, only: p_rhs2
	use jmod, only: j_err
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivrowlog
	use jmod, only: j_o
	use jmod, only: p_rowlog
	use jmod, only: p_ivrowfact
	use jmod, only: p_rowfact
	use jmod, only: p_ivcaprow
	use jmod, only: p_caprow
	use jmod, only: p_ivcaprowinf
	use jmod, only: p_caprowinf
	use jmod, only: j_getname
	use jmod, only: p_log
	use jmod, only: p_fact
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	use jmod, only: p_logmax
	use jmod, only: p_fpresent
	use jmod, only: p_xps
	use jmod, only: p_n16
	use jmod, only: p_nnfcap
	use jmod, only: j_itempvector
	use jmod, only: p_objf0
	use jmod, only: p_ivobj0
	use jmod, only: j_v
	use jmod, only: p_feasible
	use jmod, only: p_nnf
	use jmod, only: p_ivcolunit
	use jmod, only: p_nrow
	use jmod, only: p_colunit
	use jmod, only: p_ivcolfact
	use jmod, only: p_colfact
	use jmod, only: p_ivcollog
	use jmod, only: p_collog
	use jmod, only: p_colbasa
	use jmod, only: p_nrowz
	use jmod, only: p_nextf
	use jmod, only: p_prevf
	use jmod, only: p_nxkfact
	integer*8 ::i8,ibadatamat0,ibadatamat,ibadatamatkey,ibaxmat
 
	!p_knn
	!p_utiltrans
	!p_ivtablecoef
	!p_tablecoef
	! p_knn p_ivutiltrans p_neig p_neigu p_keyfact
 
	write(6,*)'*initfact'  !,p_nrow
	!	write(16,*)'*initfact'
 
 
 
	write(6,*)p_nunits,',p_nunits,p_nlog',p_nlog
 
	!p_kntot=p_knn*p_nlog
	!	p_kntot2=p_kntot*p_nunits
	!	p_ivneig=j_deflist(j_ivout,'%neig',list0=p_kntot2,ilist=.true.,ncol=p_kntot)
	!	p_neig=>j_o(p_ivneig)%i2(1:p_kntot2)
	!	write(6,*)'knnhere,p_nunits,p_kntot,p_kntot2',p_nunits,p_kntot,p_kntot2
 
	!	p_ivneigu=j_defmatrix(j_ivout,'%neigu',int8(p_nunits),int8(p_kntot),j_matreg,point=p_neigu) !ivob)
 
 
 
 
 
 
	write(6,*)'logpart',p_logpart
	write(6,*)'it,irow,p_nterminrow(irow),p_logpart(it),p_factpart(it)'
 
	p_ncaprow=0   !number of capacity rows
 
	do it=p_nterminrow(0)+1,p_nterm
		irow=p_rowofterm(it)
		!		call j_getname(p_log(p_logpart(it)),p_fact(p_factpart(it)))
		!		write(6,*)it,irow,p_nterminrow(irow),p_logpart(it),j_oname(1:j_loname),p_factpart(it),j_oname2(1:j_loname2)
		if(p_nterminrow(irow).eq.1.and.p_factpart(it).ne.0)then
			if(p_rhs2(irow).le.j_0)then
				write(6,*)'zero capacity in row ',irow,' remove the constraint'
				j_err=.true.;return
			endif !if(p_rhs2(irow).le.j_0)  10544
			p_ncaprow=p_ncaprow+1
		endif !if(p_nterminrow(irow).eq.1.and.p_factpart(it).ne.0)  10543
	enddo !it=p_nterminrow(0)+1,p_nterm  10539
 
	write(6,*)'capacity rows ',p_ncaprow
 
	p_ivrowlog=j_deflist(j_ivout,'%rowlog',list0=p_ncaprow,ilist=.true.)
	p_rowlog=>j_o(p_ivrowlog)%i2(1:p_ncaprow)
 
	p_ivrowfact=j_deflist(j_ivout,'%rowfact',list0=p_ncaprow,ilist=.true.)
	p_rowfact=>j_o(p_ivrowfact)%i2(1:p_ncaprow)
	p_ivcaprow=j_deflist(j_ivout,'%caprow',list0=p_ncaprow,ilist=.true.)
	p_caprow=>j_o(p_ivcaprow)%i2(1:p_ncaprow)
	p_ivcaprowinf=j_deflist(j_ivout,'%caprowinf',nres=p_ncaprow,ilist=.true.)
	p_caprowinf=>j_o(p_ivcaprowinf)%i2(1:p_ncaprow)
 
	ncaprow=0
	do it=p_nterminrow(0)+1,p_nterm
		irow=p_rowofterm(it)
 
		if(p_nterminrow(irow).eq.1.and.p_factpart(it).ne.0)then
			call j_getname(p_log(p_logpart(it)),p_fact(p_factpart(it)))
			write(6,*)it,irow,p_nterminrow(irow),p_logpart(it),j_oname(1:j_loname),p_factpart(it),j_oname2(1:j_loname2)
 
			p_logmax(p_logpart(it))=&
				p_logmax(p_logpart(it))+p_rhs2(irow)
			!	p_logsum(p_logpart(it))=&
			!		p_logsum(p_logpart(it))+p_xpsf(irow)
			ncaprow=ncaprow+1
			p_rowlog(ncaprow)=p_logpart(it)
			p_rowfact(ncaprow)=p_factpart(it)
			p_caprow(ncaprow)=irow
 
		endif !if(p_nterminrow(irow).eq.1.and.p_factpart(it).ne.0)  10568
	enddo !it=p_nterminrow(0)+1,p_nterm  10565
 
	! irv=-1
	! do it=1,p_nterm
	! irow=p_rowofterm(it)
	!	write(6,*)'p_xps her,'
 
	if(p_fpresent)write(6,'(25f6.1)')p_xps
 
	!	write(6,*)'p_xpsf her,'
 
	!	if(p_fpresent)write(6,'(25f6.1)')p_xpsf
 
	!stop
 
	!	call initfeas0()
	write(p_n16,*)'coming from initfeas0,p_nnf,p_feasible'
	!call nnfcap()
	!write(6,*)'p_nnfcap ',p_nnfcap
 
	!if(p_nnfcap.gt.0)call initfeas()
	!	stop
	! enddo
	write(6,*)'p_rowlog',p_rowlog
	write(6,*)'p_rowfact',p_rowfact
	write(6,*)'p_caprow',p_caprow
 
 
 
 
	!ibatable=0
	!	write(6,*)'xps',p_xps(1:20)
	!	write(6,*)'xpsf',p_xpsf(1:20)
	!j_dapu5=j_0
 
 
	!	return
	!	call nnfcap()
 
 
	!	write(6,*)p_nnfcap,' of ',p_ncaprow,' capacity rows were infeasible',j_dapu,j_dapu2
	!	write(6,*)
	!	write(6,*)'p_lf here,',p_lf(1:10)
	!	write(6,*)'xpsfinit '
	!	write(6,'(20f6.1)')p_xpsf
 
	! write(6,*)'xmat'
	! write(6,'(20f6.1)')p_xmat(1:300)
	! write(6,*)'p_ix',p_ix(1:15)
	!	return
 
 
	! floop:	do kier=1,2
	! !j_o(p_ivcaprowinf)%i(1)=p_nnfcap
 
	! ibadat=0
	! neibas=0
	! imp=0
	! noo=0
 
	! ibakeyf=0
 
	! do iunit=1,p_nunits
 
	! ibakey=ibadat+(p_keys(iunit)-1)*j_dnkeep
	! !		write(6,*)'iunit,neibas,neibas/p_kntot ',iunit,neibas,neibas/p_kntot
	! nimp=0
	! do ir2=1,p_nnfcap
	! ir=p_caprowinf(ir2)
	! irow=p_caprow(ir)  !nonfeasible row
	! !		write(6,*)'ir2,irow,p_xpsf(irow),p_rhs2(irow)',ir2,irow,p_xpsf(irow),p_rhs2(irow)
	! ilog=p_rowlog(ir)   !
	! ifact=p_rowfact(ir)
	! !	write(6,*)'irow,ilog,p_nlog
	! ifact2=p_neig(neibas+(ilog-1)*p_knn+kier+1)
	! j_dapu=j_dmat(ibakey+p_keeplog(ilog))
	! if(j_dapu.gt.j_0)then
	! do ir3=1,p_ncaprow
	! irow3=p_caprow(ir3)
	! if(irow3.eq.irow)cycle
	! if(j_inlist(ir3,p_nnfcap,p_caprowinf).gt.0)cycle
 
	! if(ilog.eq.p_rowlog(ir3).and.p_rowfact(ir3).eq.ifact2.and. &
	! ifact.eq.p_keyfact(ibakeyf+ilog))then
	! !		write(6,*)'irow3,',irow3,p_xpsf(irow3),j_dapu,p_rhs2(irow3),p_xpsf(irow)
	! if(p_xps(irow3)+j_dapu.le.p_rhs2(irow3))then
	! !			if(irow.eq.70)write(17,*)'from',iunit,ilog,ibakey,p_keeplog(ilog),j_dapu,ifact
	! p_xps(irow)=p_xps(irow)-j_dapu
	! p_xps(irow3)=p_xps(irow3)+j_dapu
	! p_keyfact(ibakeyf+ilog)=ifact2
	! if(p_xps(irow).lt.j_0.or.p_xps(irow3).lt.j_0)then
	! write(6,*)'ilog,irow,ifact,ifact2,j_dapu,irow3',ilog,irow,ifact,ifact2,j_dapu,irow3
	! write(6,*)p_rhs2(irow),p_rhs2(irow3),p_xps(irow),p_xps(irow3)
	! call j_getname(p_log(ilog),p_fact(ifact),p_fact(ifact2))
	! write(6,*)j_oname(1:j_loname+1),j_oname2(1:j_loname2+1),j_oname3(1:j_loname3)
	! return
	! endif !if(p_xps(irow).lt.j_0.or.p_xps(irow3).lt.j_0)  10538
 
	! !	write(6,*)'irow,irow3,p_xpsf(irow),p_xpsf(irow3),j_dapu',&
	! !irow,irow3,p_xpsf(irow),p_xpsf(irow3),j_dapu,p_rhs2(irow),p_rhs2(irow3)
	! if(p_xps(irow).le.p_rhs2(irow))then
	! p_caprowinf(ir2)=-1
	! nimp=nimp+1
 
	! if(nimp.ge.p_nnfcap)then
	! p_nnfcap=0
	! exit floop
	! endif !if(nimp.ge.p_nnfcap)  10552
	! endif !if(p_xps(irow).le.p_rhs2(irow))  10548
	! endif !if(p_xps(irow3)+j_dapu.le.p_rhs2(irow3))  10533
	! endif !if(ilog.eq.p_rowlog(ir3).and.p_rowfact(ir3).eq.ifact2.an  10530
	! enddo !ir3=1,p_ncaprow  10525
	! endif !if(j_dapu.gt.j_0)  10524
	! enddo !ir2=1,p_nnfcap  10515
	! if(nimp.gt.0)then
 
	! lop=p_nnfcap
	! p_nnfcap=0
	! do ir2=1,lop
	! if(p_caprowinf(ir2).gt.0)then
	! p_nnfcap=p_nnfcap+1
	! p_caprowinf(p_nnfcap)=p_caprowinf(ir2)
	! else
	! p_feasrow( p_caprow(ir2))=.true.
	! endif !if(p_caprowinf(ir2).gt.0)  10567
 
	! enddo !ir2=1,lop  10566
	! write(6,*)nimp,'nimp',p_caprowinf(1:p_nnfcap)
	! endif !if(nimp.gt.0)  10562
	! ibadat=ibadat+j_dnkeep*p_ns(iunit)
	! neibas=neibas+p_kntot
	! ibakeyf=ibakeyf+p_nlog
	! enddo !iunit=1,p_nunits  10510
	! enddo floop !op:	do kier=1,2  10500
 
 
 
 
	! write(6,*)'bef fenter0 ',p_nnfcap,' infeasible capacity constraints'
 
	!	if(p_nnfcap.gt.0)then
	write(6,*)'bef initfeas printfact'
	call printfact()
	! write(6,*)'bef initfeas'
	! call initfeas()   !fenter0()
	! write(6,*)'after inifeas ',p_nnfcap,' infeasible capacity constraints'
	! call printfact()
	! read(5,*)hdhdh
	!	endif !if(p_nnfcap.gt.0)  10588
 
 
 
	write(6,*)'p_caprow'
	write(6,'(40i4)')p_caprow
	write(6,*)'caprowinf'
	write(6,'(40i4)')p_caprowinf(1:p_nnfcap)
 
	write(6,*)'caprows'
	write(6,'(40i4)')p_caprow(p_caprowinf(1:p_nnfcap))
 
 
	!	write(6,*)'ibaneigu,p_kntot2',ibaneigu,p_kntot2
	deallocate(j_itempvector)
 
	write(6,*)'*objective ',p_objf0,p_ivobj0
	j_v(p_ivobj0)=p_objf0
	!	write(6,*)'associate',associated(p_objr)
 
	!	write(6,*)'there were ',p_nnf,' infeasible rows,exess ',j_dapu,' slack ',j_dapu2
	!	write(6,*)'vc '
	!	write(6,'(20f5.1)')p_vc
	write(6,*)'p_nnfcap',p_nnfcap
	write(6,'(40i3)')p_caprow
	write(6,*)'caprowinf'
	write(6,'(40i4)')p_caprowinf(1:p_nnfcap)
	write(6,*)'caprows inf'
	write(6,'(40i4)')p_caprow(p_caprowinf(1:p_nnfcap))
 
	p_feasible=p_nnf.eq.0
 
 
	p_ivcolunit=j_deflist(j_ivout,'%colunit',list0=p_nrow,ilist=.true.,point=p_colunit)
	p_ivcolfact=j_deflist(j_ivout,'%colfact',list0=p_nrow,ilist=.true.,point=p_colfact)
	p_ivcollog=j_deflist(j_ivout,'%collog',list0=p_nrow,ilist=.true.,point=p_collog)
 
	p_colbasa=p_nrow+p_nrowz
	write(61,*)'pcolbasa',p_colbasa
 
	if(allocated(p_nextf))deallocate(p_nextf)
	allocate(p_nextf(p_nrow:2*p_nrow,1:p_nlog))
	if(allocated(p_prevf))deallocate(p_prevf)
	allocate(p_prevf(p_nrow:2*p_nrow,1:p_nlog))
	if(allocated(p_nxkfact))deallocate(p_nxkfact)
	allocate(p_nxkfact(1:p_nlog))
 
	p_nextf=0
	p_nextf(p_nrow,1:p_nlog)=p_nrow
	p_prevf(p_nrow,1:p_nlog)=p_nrow
 
end subroutine initfact




subroutine startx(iob,io)
	use jmod, only: j_o
	use jmod, only: p_isdomain
	use jmod, only: p_ivproblem
	use jmod, only: p_nvarf
	use jmod, only: p_ivdomain
	use jmod, only: p_ndom
	use jmod, only: j_getname
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: j_getobject
	use jmod, only: j_ivout
	use jmod, only: j_ipreal
	use jmod, only: p_ivobj0
	use jmod, only: j_getoption
	use jmod, only: j_mshowdomain
	use jmod, only: j_ipchar
	use jmod, only: p_nshow
	use jmod, only: p_show
	use jmod, only: j_err
	use jmod, only: j_deflist
	use jmod, only: p_ivshowunits
	use jmod, only: j_deftext
	use jmod, only: p_ivshowtext
	use jmod, only: j_defmatrix
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: p_ivshowmatrix
	use jmod, only: j_getchar
	use jmod, only: p_domainname
	use jmod, only: j_puttext
	use jmod, only: j_command
	use jmod, only: j_object
	use jmod, only: p_ivshowtrans
	use jmod, only: p_ivshowin
	use jmod, only: p_nshowin
	use jmod, only: p_p
	use jmod, only: p_xpresent
	use jmod, only: p_ivrowx
	use jmod, only: p_nrowx
	use jmod, only: p_rowx
	use jmod, only: p_rowcurx
	use jmod, only: p_sumx
	use jmod, only: p_nsumx
	use jmod, only: j_0
	use jmod, only: j_inlistobject
	use jmod, only: j_divkeepup
	use jmod, only: p_isunit
	use jmod, only: j_oname2
	use jmod, only: j_loname2
	character*2 chint
	write(6,*)'*startx'
	p_isdomain=j_o(p_ivproblem)%i(10).gt.0
	!p_ndom=1   !All
	p_nvarf=0
	if(p_isdomain)then
		p_ivdomain=j_o(p_ivproblem)%i(11)
		p_ndom=j_o(p_ivdomain)%i(0)
		call j_getname(p_ivdomain)
		write(6,*)'ivdomain,ndom',p_ivdomain,p_ndom,j_oname(1:j_loname)
 
	endif !if(p_isdomain)  10792
	!write(6,*)'p_ndom',p_ndom
	!	p_ivobj=j_getobject(j_ivout,'%obj',j_ipreal)
	p_ivobj0=j_getobject(j_ivout,'%obj0',j_ipreal)
	!j_o(ivproblem)%i(4)=p_ivrow  !text for rows
 
	!	write(6,*)'jlp1'
	call j_getoption(iob,io,j_mshowdomain,-1,99,j_ipchar,.true.,p_nshow,p_show)
	!	write(6,*)'jlp2',p_nshow
	if(j_err)return
	!	p_row0=1   !updated for domainprob
	if(p_nshow.gt.0)then
		p_ivshowunits=j_deflist(j_ivout,'%showunits',list0=p_nshow,ilist=.true.)
		les=0
		do j=1,p_nshow
			les=les+j_o(p_show(j))%i(2)-j_o(p_show(j))%i(1)+1
		enddo !j=1,p_nshow  10812
 
		p_ivshowtext=j_deftext(0,'$showtext',p_nshow,les+10*p_nshow)
		p_ivshowmatrix=j_defmatrix(0,'$showdom',int8(p_nshow),j_18,j_matreg)
		do j=1,p_nshow
			write(chint,'(i2)')j
			j1=2
			if(j.ge.10)j1=1
			call j_getchar(p_show(j),p_domainname,le)
			call j_puttext(p_ivshowtext,'$showdom('//chint(j1:2)//')='//p_domainname(1:le))
		enddo !j=1,p_nshow  10818
 
		call j_command('$showtrans=trans(in->$showtext)')
 
		if(j_err)then
			write(6,*)'*error in interpreting $showdomaintext'
			return
		endif !if(j_err)  10828
		p_ivshowtrans=j_object('$showtrans')
		!j_o(ivout)%i2(1)=ivinl
		!	write(6,*)'jlp2'
		p_ivshowin=j_o(p_ivshowtrans)%i2(1)
		p_nshowin=j_o(p_ivshowin)%i(1)
	else
		p_nshow=0
	endif !if(p_nshow.gt.0)  10809
 
	write(6,*)'showdomains',p_nshow
	if(p_isdomain)then
		if(p_p)write(6,*)'start initdomain'
		if(p_p)write(6,*)' '
		if(.not.p_xpresent)then
			write(6,*)'there cannot be domains without constraints for x-variables'
			j_err=.true.; return
		endif !if(.not.p_xpresent)  10845
 
		call initdomain()
		!	if(allocated(p_rowcurx))write(6,*)'i9rowx cur ',p_rowcurx(1:p_nrowcurx)
		if(j_err)return
	else
		p_ndom=1
		p_ivrowx= j_deflist(j_ivout,'%rowx',list0=p_nrowx,ilist=.true.)
		p_rowx=>j_o(p_ivrowx)%i2(1:p_nrowx)
		!	if(allocated(p_rowx))deallocate(p_rowx)
		!p_nrowx=p_nrowcurx
		!allocate(p_rowx(1:p_nrowx))
		p_rowx=p_rowcurx(1:p_nrowx)
 
	endif !if(p_isdomain)  10842
 
	if(allocated(p_sumx))deallocate(p_sumx)
	write(6,*)'domains ',p_ndom,' showdomains ',p_nshow,' show variables',p_nsumx
	!p_isdomain=p_ndom.gt.1 done
	!	write(6,*)'ndom2,p_nshow',ndom2,p_nshow,' p_nsumx ',p_nsumx,p_nsumx*(ndom2+p_nshow)
	allocate(p_sumx(1:p_nsumx*(p_ndom+p_nshow)))
	p_sumx=j_0
	!	write(6,*)'p_ndom ',p_ndom
	!write(6,*)'p_domv ',p_ndomv
 
	if(p_nshow.gt.0)then
		!if(allocated(p_showin))deallocate(p_showin)
		!	allocate(p_showin(1:p_nshowin)
		do j=1,p_nshowin
			iiv=j_o(p_ivshowin)%i2(j)
			ini=0
			if(.not.p_isunit)ini=j_inlistobject(iiv,j_divkeepup)
			!			p_showin(j)=ini
			if(ini.gt.0)cycle
 
			if(iiv.eq.ivunit)cycle
 
			call j_getname(iiv,ivunit)
			write(6,*)' '
			write(6,*)'*showdomain variable ',j_oname(1:j_loname),&
				' is not in c-data and is not the unit variable ',j_oname2(1:j_loname2)
			j_err=.true.
 
		enddo !j=1,p_nshowin  10876
		if(j_err)return
 
	endif !if(p_nshow.gt.0)  10873
 
 
 
end subroutine startx




subroutine initxdatjlp()
	use jmod, only: p_isn16
	use jmod, only: j_err
	use jmod, only: p_activeunit
	use jmod, only: p_nunits
	use jmod, only: p_ivdomaintrans
	use jmod, only: p_unit
	use jmod, only: p_isunit
	use jmod, only: j_v
	use jmod, only: j_dkeep
	use jmod, only: j_dmat
	use jmod, only: j_dnkeep
	use jmod, only: j_dkeepup
	use jmod, only: j_dmatup
	use jmod, only: j_dnkeepup
	use jmod, only: j_divobsup
	use jmod, only: p_isdomain
	use jmod, only: p_ido1
	use jmod, only: p_ndomvars
	use jmod, only: j_0
	use jmod, only: p_domvars
	use jmod, only: p_domainunits
	use jmod, only: p_domainbits
	use jmod, only: p_icurint
	use jmod, only: p_icurbit
	use jmod, only: p_ns
	use jmod, only: p_nstot
	use jmod, only: j_getobs
	use jmod, only: p_filter_
	use jmod, only: j_codevalue
	use jmod, only: j_filterlink
	use jmod, only: p_rejects
	use jmod, only: p_nrejtot
	use jmod, only: p_reject
	use jmod, only: j_rejectlink
	use jmod, only: p_xpresent
	use jmod, only: p_ntemp0
	use jmod, only: p_isarea
	use jmod, only: p_xvararea
	use jmod, only: p_ivarea
	use jmod, only: p_ibatemp
	use jmod, only: p_xmat
	use jmod, only: p_xcoef
	use jmod, only: p_nrowxtemp
	use jmod, only: p_irowxvar
	use jmod, only: p_maxo
	use jmod, only: p_ix
	use jmod, only: p_filre
	use jmod, only: p_nrejdupl
	use jmod, only: p_fpresent
	use jmod, only: p_nrow
	use jmod, only: p_nlog
	use jmod, only: j_o
	use jmod, only: p_ivkeeplog
	use jmod, only: p_fast
	use jmod, only: p_fastvalues
	use jmod, only: p_maxns
	use jmod, only: p_svalue
	use jmod, only: p_basreject
	use jmod, only: p_solx
	use jmod, only: p_nz
	use jmod, only: j_dnobs
	use jmod, only: p_time00
	use jmod, only: p_xvar1
	use jmod, only: p_xvar2
	use jmod, only: p_ndom
	use jmod, only: j_gettext
	use jmod, only: p_ivdomain
	use jmod, only: j_gottext
	use jmod, only: j_lgottext
	use jmod, only: p_xps
	use jmod, only: p_xsmin
	use jmod, only: p_zero
	use jmod, only: p_xsmax
	use jmod, only: p_rejectnow
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivlunit
	use jmod, only: p_nrowtot
	use jmod, only: p_lunit
	use jmod, only: p_ivisch
	use jmod, only: p_isch
	use jmod, only: p_xps0
	use jmod, only: p_test
	use jmod, only: p_nrowz
	use jmod, only: p_next
	use jmod, only: p_prev
	!double precision area
	logical ::isdomtrans
	write(6,*)'*initxdatjlp'  ! ,p_nshow,p_ivshowtrans,p_isdomain,p_ivdomaintrans,p_ndomvars',&
	if(p_isn16)write(16,*)'*initxdatjlp'
	!		p_nshow,p_ivshowtrans,p_isdomain,p_ivdomaintrans,p_ndomvars
	! p_domainunits
	! p_domainbits
	! p_ns(p_unit)
	! p_nstot
	! p_rejects
	! p_nrejtot
	! p_fastvalues
	! p_svalue
	! p_solx (0:p_nrow)
	! p_ndom
	! p_domainunits
 
 
	!20181116 #zeroc block moved from #z-commented ends here
 
 
	!muuttuja-tehdas-taulukko
 
	!if(j_err)goto 990
 
 
 
	iiro=0
	ndd=1
	! now isx tell for all variables in v ars%problem if they are x variable
	! nrowx tell for each row the number of x-variables in the row
	! isxval tells for each element in coef and p_varofterm if it is x-variable
 
	! starts acces to c-data: !!!!
	!if(.not.p_isunit)call j_getdat(j_divdataup,p_nunits,j_divmatup,j_divkeepup) !,p_ivtransc,p_ivvarsc)
	if(j_err)return
	!if(p_nfx.gt.0.or.p_nfy>0) call initkeyfact()
 
 
	ibasc=0
 
	iwar=0   ! number of units where all schedules are rejected
 
	!p_idomba=0
	ibas=0
	!write(6,*)'#####nunits,',p_nunits
 
	if(allocated(p_activeunit))deallocate(p_activeunit)
	allocate(p_activeunit(1:p_nunits))
	p_activeunit=.true.
	isdomtrans=p_ivdomaintrans.gt.0
	!	write(6,*)'p_ivdomaintrans,p_isdomain,isdomtrans ',p_ivdomaintrans,p_isdomain,isdomtrans
	!isdom=p_ndomvars.gt.0
	do p_unit=1,p_nunits
 
		if(p_isunit)then
 
			!		j_v(j_o(j_divkeep)%i2(1:j_dnkeep))=j_o(j_divmat)%d(ibas+1:ibas+j_dnkeep)
			j_v(j_dkeep)=j_dmat(ibas+1:ibas+j_dnkeep)
			ibas=ibas+j_dnkeep
		else
			!	write(6,*)'sh',j_divkeepup,keepc,j_divmatup
			!	call j_getname(j_divkeepup,j_divmatup)
			!	write(6,*)'keepc ',j_oname(1:j_loname),' * ',j_oname2(1:j_loname2)
			!	write(6,*)j_dnkeepup,j_o(j_divkeepup)%i(1)
			j_v(j_dkeepup)=j_dmatup(ibasc+1:ibasc+j_dnkeepup)
			ibasc=ibasc+j_dnkeepup
			!		call j_getobsiv(p_unit,j_divmatup,j_divkeepup,p_j_divobsup)
 
		endif !if(p_isunit)  10959
		! if(p_ivtrans.gt.0)then
		! call dotrans(p_ivtrans,1)  ! trans option given
		! if(j_err)then
		! write(6,*)'error for unit ',p_unit
		! stop 815
		! endif !if(j_err)   8347
 
		! endif !if(p_ivtrans.gt.0)   8345
		if(isdomtrans)then
			!		write(6,*)'<456doing,p_needc,p_ndom',p_ndom
			!	write(6,*)'p_ivdomaintrans',p_ivdomaintrans
			j_v(j_divobsup)=p_unit
			call dotrans(p_ivdomaintrans,1)  !there are domain trannsformations
			if(j_err)then
				write(6,*)'error in domain transformations for unit ',p_unit
				stop 879
			endif !if(j_err)  10987
 
		endif !if(isdomtrans)  10982
		!if(p_needc.gt.0)p_cvar=j_v(p_cvarl(1:p_ncvar))
 
		if(p_isdomain)then  ! ndom is number of domains
			!domainbits(1:p_ndomv,p_unit)=0 done at allocation
			! käydään läpi domainmuuttujat
 
			!		ibit=0
			! !	write(6,*)'<45p_ndomv,idomb,p_ndomvars',p_ndomv,p_idomba,p_ndomvars
			! istep=0
			! if(p_domvars(1).eq.j_ivall)then
			! istep=1
			! p_domainunits(1)=p_nunits
			! endif !if(p_domvars(1).eq.j_ivall)  11418
			!	write(6,*)'<33ido1 ',p_ido1,p_ndomvars
			idomba=p_unit-1  !set p_mmatrix
			do ido=p_ido1,p_ndomvars
 
				!		write(6,*)j_oname(1:j_loname),j_v(domvars(j)),ibit,p_idomba
				if(j_v(p_domvars(ido)).ne.j_0)then
					!	write(6,*)'<45set',j
 
					p_domainunits(ido)=p_domainunits(ido)+1
					! set not
					!		if(p_p8)write(6,*)'ido,pidomba,icurint,bit',ido,p_idomba,p_icurint(ido),p_icurbit(ido)
					p_domainbits(idomba+p_icurint(ido))= ibset(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido))
				endif !if(j_v(p_domvars(ido)).ne.j_0)  11011
 
 
			enddo !ido=p_ido1,p_ndomvars  11008
 
			!	if(p_p8)write(6,*)'<11446 domainbits',p_domainbits
			!		p_idomba=p_idomba+1
 
 
		endif !if(p_isdomain)  10995
		! if(p_unit.gt.10)then
 
		! write(6,*)'ddhhd'
		! j_err=.true.;return
		! endif
		!	if(.not.p_isunit)p_ns(p_unit)=j_v(j_ivns)   ! number of schedules
		nrej=0   ! number of rejected schedules in this unit
		!	inde=1
		!	if(p_unit.eq.1)write(6,*)'%%%%¤¤',p_isunit,p_ns(p_unit)
		if(p_ns(p_unit).le.0)then
			write(6,*)'*unit ',p_unit,' has no schedules ,remove such units'
			j_err=.true.;return
		endif !if(p_ns(p_unit).le.0)  11037
		!	write(6,*)'**unit,nobsw ',p_unit,p_ns(p_unit)
		do is=1,p_ns(p_unit)
			!			write(6,*)'<6565'
			p_nstot=p_nstot+1
			!		write(17,*)p_unit,p_nstot
			call j_getobs(p_nstot)
 
			if(p_filter_)then         !subfilter->
				testcode=j_codevalue(iob,j_filterlink)
				!call dotrans(iob,j_iosubfilter)
				if(j_err)then
					write(6,*)'error for observation ',iob
					stop 337
 
				endif !if(j_err)  11051
				if(testcode.eq.0.)then   ! filter->False, reject
					p_rejects(p_nstot)=.true.
					nrej=nrej+1
					p_nrejtot=p_nrejtot+1  !total number of rejected schedules
					goto 776
				endif !if(testcode.eq.0.)  11056
			endif !if(p_filter_)  11048
			if(p_reject)then            ! reject->
				testcode=j_codevalue(iob,j_rejectlink) !call dotrans(iob,j_iosubreject)
				if(j_err)then
					write(6,*)'error for obs ',iob
				endif !if(j_err)  11065
				if(testcode.ne.0.)then    !reject->True
					p_rejects(p_nstot)=.true.
					nrej=nrej+1
					p_nrejtot=p_nrejtot+1
					goto 776
				endif !if(testcode.ne.0.)  11068
			endif !if(p_reject)  11063
			776     continue
			!		write(6,*)'<8767'
		!	if(j_xmatinmemory)then
 
 
			if(p_xpresent)then
 
				ibaxmat=(p_nstot-1)*p_ntemp0
 
				if(p_isarea)then
					! xvararea is a vector
					!		write(6,*)'area',j_v(p_ivarea)
					j_v(p_xvararea)=j_v(p_ivarea)*j_v(p_xvararea)   ! multiply area-variables by area
				endif !if(p_isarea)  11084
 
				!		if(p_unit.lt.5)write(6,*)'uni ,p_ntemp0',p_unit,p_ntemp0
				do i=1,p_ntemp0
					iba=p_ibatemp(i)
					!	if(p_unit.lt.10)write(6,*)iba,p_ntemp0,iba,ibaxmat,p_nrowxtemp(i),iba+p_nrowxtemp(i)
					!	if(ibaxmat.eq.0)write(6,*)'***',i,p_nrowxtemp(i),p_irowxvar(iba+1),j_v(p_irowxvar(iba+1))
					p_xmat(i+ibaxmat)= &       ! make temporary variables
						dot_product(p_xcoef(iba+1:iba+p_nrowxtemp(i)), &
						j_v(p_irowxvar(iba+1:iba+p_nrowxtemp(i))))
 
					! if(i.eq.1)then
					! !	write(6,*)'***unit',p_unit,p_nstot,i,p_xmat(i+ibaxmat)
					! do kk=1,p_nrowxtemp(i)
					! call j_getname(p_irowxvar(iba+kk))
					! write(6,*)j_oname(1:j_loname),p_xcoef(iba+kk),j_v(p_irowxvar(iba+kk))
 
 
					! enddo !kk=1,p_nrowxtemp(i)   8130
					! endif !if(i.eq.1)   8128
 
 
					!	if(p_unit.lt.5)write(6,*)'uni xcoefmp0',p_xcoef(iba+1:iba+p_nrowxtemp(i))
					!		write(i18,*)p_nstot,p_unit,p_ns(p_unit),is,i,j_v(p_irowxvar(iba+1:iba+p_nrowxtemp(i)))
					! if(p_unit.le.5)then
					! write(6,*)'j_nrowxtemp(i)',p_nrowxtemp(i),p_xmat(i+ibaxmat),&
					! p_xcoef(iba+1:iba+p_nrowxtemp(i)),j_v(p_irowxvar(iba+1:iba+p_nrowxtemp(i)))
					! do jj=1,p_nrowxtemp(i)
					! call j_getname(p_irowxvar(iba+jj))
					! write(6,*)jj,p_xcoef(iba+jj),p_irowxvar(iba+jj),j_oname(1:j_loname),j_v(p_irowxvar(iba+jj))
					! enddo
					! endif
					!	if(p_p2.and.p_nstot.eq.1)write(6,*)'p_itemp',i,' iba=',iba,' coef=',j_xcoef(iba+1:iba+j_nrowxtemp(i)),&
					!		' vars',j_irowxvar(iba+1:iba+j_nrowxtemp(i))
				enddo !i=1,p_ntemp0  11091
				!if(p_unit.gt.1)stop 'per'
				!	write(17,*)p_unit,is,p_xmat(ibaxmat+1:ibaxmat+p_ntemp0)
				!j_ix(0),j_ntemp0
				if(.not.p_maxo.and.p_ix(0).ne.0) &
					p_xmat(p_ix(0)+ibaxmat)=-p_xmat(p_ix(0)+ibaxmat)
 
			endif !if(p_xpresent)  11080
			! endif !if(istree)then
			!		if(p_needc.gt.0)j_v(p_cvarl(1:p_ncvar))=p_cvar
		enddo !is=1,p_ns(p_unit)  11042
 
 
 
 
		if(p_filre)then
			if(nrej.ge.p_ns(p_unit))then
				iwar=iwar+1
				if(iwar.le.10)write(6,*)'*err all schedules were rejected for unit ',p_unit
			endif !if(nrej.ge.p_ns(p_unit))  11138
		endif !if(p_filre)  11137
		if(j_err)return
		!	write(6,*)'nuni ',p_nunits,p_unit,ibaxmat,p_changed
	enddo !p_unit=1,p_nunits  10957
 
	p_nrejdupl=0
	iba=0
	nstot=0
	nstot=0
	ibadat=0
	ibadat2=0
	do i=1,p_nunits
 
		do j2=2,p_ns(i)
 
			if(p_rejects(nstot+j2))cycle
			if(p_xpresent)iba2=iba+(j2-1)*p_ntemp0
			if(p_fpresent)ibadat2=ibadat+(j2-1)*j_dnkeep
j1loop:	 do j1=1,j2-1
				if(p_rejects(nstot+j1))cycle
 
				if(p_xpresent.and.p_nrow.gt.0)then
					iba1=iba+(j1-1)*p_ntemp0
					do k=1,p_ntemp0
 
						if(abs(p_xmat(iba2+k)-p_xmat(iba1+k)).gt.1.d-12)cycle j1loop
 
					enddo !k=1,p_ntemp0  11165
				endif !if(p_xpresent.and.p_nrow.gt.0)  11163
				if(p_fpresent)then
 
					iba1=ibadat+(j1-1)*j_dnkeep
					do k=1,p_nlog
						if(abs(j_dmat(ibadat2+j_o(p_ivkeeplog)%i2(k)) -&
							j_dmat(iba1+j_o(p_ivkeeplog)%i2(k))).gt.1.d-12)cycle j1loop
 
					enddo !k=1,p_nlog  11174
 
				endif !if(p_fpresent)  11171
 
				p_rejects(nstot+j2)=.true.
				p_nrejdupl=p_nrejdupl+1
 
 
			enddo j1loop !oop:	 do j1=1,j2-1  11160
		enddo !j2=2,p_ns(i)  11155
		nstot=nstot+p_ns(i)
		if(p_xpresent)iba=iba+p_ns(i)*p_ntemp0
		if(p_fpresent)ibadat=ibadat+p_ns(i)*j_dnkeep
	enddo !i=1,p_nunits  11153
 
	p_nrejtot=p_nrejtot+p_nrejdupl
 
 
	!	write(6,*)'<56domainbits',p_domainbits
	!	write(6,*)'nunits ',p_nunits,p_ntemp0
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	if(p_xpresent.and.p_nrow.gt.0)then
		iba=0
		write(6,*)' '
		write(6,*)'2 first rows of xmat'
		do i=1,2
			write(6,'(i3,(5g18.10))')i,p_xmat(iba+1:iba+p_ntemp0)
			iba=iba+p_ntemp0
		enddo !i=1,2  11203
 
	endif !if(p_xpresent.and.p_nrow.gt.0)  11199
 
	!is(p_p8)write(6,*)'p_fast ',p_fast,' p_maxns',p_maxns
 
	!	write(6,*)'shed2',p_ns(1:p_nunits)
	if(p_fast)then
		!	maxnsch2=maxval(j_nsch)
		!	write(6,*)'maxns',maxnsch,maxnsch2
		if(allocated(p_fastvalues))deallocate(p_fastvalues)
		!		write(6,*)'<44>',p_maxns
		allocate(p_fastvalues(1:p_maxns))
		!	if(allocated(fastclass))deallocate(fastclass)
		!	allocate(fastclass(1:201))
 
	endif !if(p_fast)  11213
	if(.not.allocated(p_svalue))then
		allocate(p_svalue(1:p_maxns),p_basreject(1:p_maxns))
 
	elseif(size(p_svalue).lt.p_maxns)then
		deallocate(p_svalue,p_basreject)
		allocate(p_svalue(1:p_maxns),p_basreject(1:p_maxns))
	endif !if(.not.allocated(p_svalue))  11223
 
 
	if(iwar.gt.0)then
		write(6,*)'**all schedules rejected for ',iwar, 'units'
		j_err=.true.
		return
	endif !if(iwar.gt.0)  11232
 
 
 
	if(allocated(p_solx))deallocate(p_solx)
	if(p_nz.le.0)allocate(p_solx(0:p_nrow))
	!p_nstot adds up the 'ns' variables, lopp is the number of observations in xdata
	if(j_divobsup.le.0.and.p_nstot.ne.j_dnobs)then
		write(6,*)'**number of sched in cdat, and xdat do not match',p_nstot,j_dnobs
		j_err=.true.
		return
	endif !if(j_divobsup.le.0.and.p_nstot.ne.j_dnobs)  11243
	call cpu_time(p_time00)
	!	write(6,*)'time00al',p_time00
	write(*,*)'found ',p_nunits,' units, ',j_dnobs,' schedules'
	write(6,*)'from which ',p_nrejtot,' schedules were rejected'
	write(6,*)'from which ',p_nrejdupl,' were duplicated schedules'
	if(p_xpresent)then
		!if(p_nrow.eq.0)return
		write(6,*)'* filling xmat'
 
 
		deallocate (p_xvar1,p_xvar2)
 
	endif !if(p_xpresent)  11253
	if(p_isdomain)then
		! empty domains?
		write(6,*)' '
		write(6,*)'found ',p_ndom,' domains'
		nempt=0
		write(6,*)'index  number of units  definition'
		do i=1,min(p_ndom,20)
			call j_gettext(p_ivdomain,i)
			write(6,*)i,p_domainunits(i),j_gottext(1:j_lgottext)
 
			if(p_domainunits(i).le.0) nempt=nempt+1
			! else
 
 
			if(i.eq.20.and.p_ndom.gt.20)write(6,*)'etc ..'
		enddo !i=1,min(p_ndom,20)  11267
		if(nempt.gt.0)then
			j_err=.true.;write(6,*)'**empty domains:',nempt; return
		end if !if(nempt.gt.0)  11277
	endif !if(p_isdomain)  11261
 
	! write(p_n16,*)'<44starti',i
	nrej=0
	!	write(6,*)'idom2',i,p_idomba
	!	if(p_isdomain)call jlpcurix() !determines for each row if the unit p_unit belonggs to the domain of the row
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units must be accessed one by one
	! p_nrowcurx
	! p_rowcurx
	! p_idomba
	!	write(6,*)'p_nrowcurx ',p_nrowcurx,'*',p_rowcurx(1:p_nrowcurx)
 
	!returns nrowp=number of rows in this domain,
	! rowcurx= for each row in this domain tells the original (expanded) row
	!  ixcur for each original expanded row tells the index of temporary x-variable
	! facory tehtävissä tämä on kait sama
	!if(p_p8.and.i.lt.10)write(6,*)'i ,p_nrowcurx,p_rowcurx',i ,p_nrowcurx,p_rowcurx(1:p_nrowcurx)
 
 
	! calculate sums over key schedules !!!!
	!if(j_o(j_divmat)%r(10261*keepx).eq.0.)stop 179
	p_xps=j_0
	if(p_xpresent)then
 
		p_xsmin=p_zero     !minimum for the sum
		p_xsmax=p_zero     !maximum for the sum
		! initial key schedules, rotate number
		key=0
	endif !if(p_xpresent)  11303
 
 
	p_rejectnow=.false.
 
	p_ivlunit=j_deflist(j_ivout,'%lunit',list0=p_nrowtot,ilist=.true.)
	p_lunit=>j_o(p_ivlunit)%i2(1:p_nrowtot)
 
	!units for both schedules and
 
	p_ivisch=j_deflist(j_ivout,'%isch',list0=p_nrowtot,ilist=.true.)
	p_isch=>j_o(p_ivisch)%i2(1:p_nrowtot)
 
	!	p_ivvx=j_defmatrix(j_ivout,'%vx',int8(p_nrowtot),j_18,j_matreg)
	!	p_vx(0:)=>j_o(p_ivvx)%d(1:p_nrowtot)
 
 
	if(allocated(p_xps0))deallocate(p_xps0,p_test)
	if(allocated(p_test))deallocate(p_test)
	allocate(p_xps0(0:p_nrow),p_test(0:p_nrowz))
 
	!		integer next(0:p_p_nrowz) !  is used to travel through columns of D so that
	! the columns correponding to same unit are after each other.
	! if last is the last column in the sequence, then next(last)=0
	! next(0) first column
	if(allocated(p_next))deallocate(p_next)
	if(allocated(p_prev))deallocate(p_prev)
	allocate(p_next(0:p_nrowtot),p_prev(0:p_nrowtot))
	p_next=0;p_prev=0
end subroutine initxdatjlp




subroutine initzjlp()
	use jmod, only: p_nz
	use jmod, only: p_zmatrix
	use jmod, only: p_isn16
	use jmod, only: p_objrz
	use jmod, only: j_deflist
	use jmod, only: j_ivout
	use jmod, only: p_ivzvars
	use jmod, only: j_o
	use jmod, only: p_zvars
	use jmod, only: p_fpresent
	use jmod, only: p_vars
	use jmod, only: p_nvar
	use jmod, only: p_isxvar
	use jmod, only: p_isvarf
	use jmod, only: p_nrow
	use jmod, only: p_varofterm
	use jmod, only: p_nterminrow
	use jmod, only: j_inlist
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_coef
	use jmod, only: p_nterm
	use jmod, only: p_nxval
	use jmod, only: j_0
	use jmod, only: p_istermx
	use jmod, only: p_maxo
	use jmod, only: p_n16
 
	write(6,*)'*initzjlp',p_nz,p_zmatrix
	if(p_isn16)write(16,*)'*initzjlp',p_nz,p_zmatrix
	if(allocated(p_objrz))deallocate(p_objrz);allocate(p_objrz(1:p_nz))
 
 
 
 
	p_ivzvars=j_deflist(j_ivout,'%zvars',list0=p_nz)
	p_zvars=>j_o(p_ivzvars)%i2(1:p_nz)
	! if(allocated(p_zvars))deallocate(p_zvars)
	! allocate(p_zvars(1:p_nz))
	!	write(6,*)'p_fpresent,p_zopt,p_isxvar',p_fpresent,p_zopt,p_isxvar
	if (p_fpresent) then
 
 
		p_zvars(1:p_nz)=pack(p_vars(1:p_nvar),.not.p_isxvar.and..not.p_isvarf)
	else
		p_zvars(1:p_nz)=pack(p_vars(1:p_nvar),.not.p_isxvar)
		!	write(6,*)'p_is%%%%%%%%%%%%%%%',p_isxvar
		!write(6,*)'vars ',p_vars(1:p_nvar)
		!write(6,*)'pisxvar',p_isxvar
		!write(6,*)'zvr2',p_zvars(1:p_nz)
		!read(5,*)jj
	endif !if (p_fpresent)  11357
	!		write(6,*)'num55es '
	!	ivzvar=j_deflist(j_ivout,'%zvars',list0=p_nz,list=p_zvars(1:p_nz))
	ival=0
	iba=0
	do irow=1,p_nrow
 
		!constraint row
		!		p_irow=p_irow+1
		write(16,*)'p_nrow',p_nrow,'varofterm',p_varofterm,'p_nz',p_nz,'kk',p_zvars
		do ii=1,p_nterminrow(irow)
			ival=ival+1   ! the number of variable with coefficient
			write(16,*)'irow,ival',irow,ival
			iz=j_inlist(p_varofterm(ival),p_nz,p_zvars)  !is it z-variable A
			!  function j_inlist(i,list0,list)
 
			! put coefficients of z variables into A matrix
			if(iz.gt.0)p_a(p_abas(iz)+irow)=p_coef(ival)  !	p_a(irow,iz)=p_coef(ival)
			! if(p_ispullout)then
			! write(6,*)'p_varofterm(ival)',p_varofterm(ival),iz,iba
			! j_o(ivconstr)%d(iba+iz)=p_coef(ival)
			! endif !if(p_ispullout)   6812
 
		enddo !ii=1,p_nterminrow(irow)  11378
		iba=iba+p_nz
	enddo !irow=1,p_nrow  11373
 
	!endif !if(p_zmatrix)  10912
 
	!write(6,*)'zvarhere'
	!	p_zvars0=p_nz
	! p_nterm= number of coefficients in the
	nzval=p_nterm-p_nxval   !muuta kun fact
	!		p_idomba=0  ! basis for values
	!		p_irow0=0
	!		p_irow=0
	ival=0
	p_objrz=j_0
 
	if(p_isn16)write(16,*)'p_istermx',p_istermx
	do ii=1,p_nterminrow(0)
		ival=ival+1
		if(.not.p_istermx(ival))then
			!!write(16,*) 'zzz',ival,p_varofterm(ival),p_nz,'zv',p_zvars
			iz=j_inlist(p_varofterm(ival),p_nz,p_zvars)
			if(p_isn16)write(16,*)'izhere',iz
			if(p_maxo)then  ! objective maximized
				!		p_objr(p_nrow+iz)=p_coef(ival) !object row
				p_objrz(iz)=p_coef(ival) !object row
			else !if(j_maxo)then
				!		p_objr(p_nrow+iz)=-p_coef(ival) ! -objective maximized
				p_objrz(iz)=-p_coef(ival)
			endif !if(p_maxo)  11414
 
		endif !if(.not.p_istermx(ival))  11410
	enddo !ii=1,p_nterminrow(0)  11408
	if(p_isn16)write(p_n16,*)'zzobjr',p_objrz
	p_a=j_0
	do irow=1,p_nrow
		!constraint row
		!		p_irow=p_irow+1
		do ii=1,p_nterminrow(irow)
			ival=ival+1   ! the number of variable with coefficient
			if(.not.p_istermx(ival))then        !is the variable not x-variable
				iz=j_inlist(p_varofterm(ival),p_nz,p_zvars)  !is it z-variable B
				if(p_isn16)write(p_n16,*)'ival',iz,ival,p_varofterm(ival),p_coef(ival),p_nz,p_zvars
 
				! put coefficients of z variables into A matrix
				p_a(p_abas(iz)+irow)=p_coef(ival)  !p_a(irow,iz)=p_coef(ival)
				write(16,*)'iz,ival,',iz,p_coef(ival)
			endif !if(.not.p_istermx(ival))  11431
		enddo !ii=1,p_nterminrow(irow)  11429
 
	enddo !irow=1,p_nrow  11426
 
	if(p_isn16)write(p_n16,*)'zpa',p_a(p_abas(1)+1:p_abas(1)+p_nz*p_nrow)
 
 
 
end subroutine initzjlp




subroutine initfeas0()
	use jmod, only: p_isn16
	use jmod, only: j_0
	use jmod, only: p_xps
	use jmod, only: p_nunits
	use jmod, only: p_keys
	use jmod, only: p_ntemp0
	use jmod, only: p_nrow
	use jmod, only: p_ix
	use jmod, only: p_xmat
	use jmod, only: j_dnkeep
	use jmod, only: p_nterm
	use jmod, only: p_logpart
	use jmod, only: p_rowofterm
	use jmod, only: p_factpart
	use jmod, only: p_keyfact
	use jmod, only: j_dmat
	use jmod, only: p_keeplog
	use jmod, only: p_ns
	use jmod, only: p_nlog
	use jmod, only: p_ebou
	use jmod, only: p_ubou
	use jmod, only: p_rhs2
	use jmod, only: p_lbou
	use jmod, only: p_rhs
	use jmod, only: p_neigbas
	use jmod, only: j_dapu
	use jmod, only: j_ninf
	use jmod, only: j_dapu2
	use jmod, only: j_dapu3
	use jmod, only: p_logval
	use jmod, only: p_optfact0
	use jmod, only: j_o
	use jmod, only: p_table
	use jmod, only: p_logtablei
	use jmod, only: p_ivneig
	use jmod, only: p_ibafact
	use jmod, only: p_factw
	use jmod, only: p_logfactterm
	use jmod, only: p_optfact
	use jmod, only: p_optfactkey
	use jmod, only: p_nrowinlog
	use jmod, only: p_rowinlog
	use jmod, only: p_rowinlogterm
	use jmod, only: p_nnf
	use jmod, only: p_infsum
	double precision:: infsum,logvol,factopt
	logical ::keyfactdif
	!	do kier=1,7
	!j_dapu3=j_dapu4
	write(6,*)'*initfeas0'
	if(p_isn16)write(16,*)'*initfeas0'
	!	read(5,*)jhfgjg
	ibadat=0
	ibaxmat=0
	inebas=0
	ibakeyf=0
	p_xps=j_0
	do i=1,p_nunits
		ibaxkey=ibaxmat+(p_keys(i)-1)*p_ntemp0
		do ir=1,p_nrow
			if(p_ix(ir).ne.0)p_xps(ir)=p_xps(ir)+p_xmat(ibaxkey+p_ix(ir))
 
		enddo !ir=1,p_nrow  11467
		ibakey=ibadat+(p_keys(i)-1)*j_dnkeep
		do it=1,p_nterm
			ilog=p_logpart(it)
			if(ilog.le.0)cycle
			irow=p_rowofterm(it)
			if(p_factpart(it).eq.p_keyfact(ibakeyf+ilog))then
 
				p_xps(irow)=p_xps(irow)+j_dmat(ibakey+p_keeplog(ilog))
			endif !if(p_factpart(it).eq.p_keyfact(ibakeyf+ilog))  11476
		enddo !it=1,p_nterm  11472
		ibadat=ibadat+p_ns(i)*j_dnkeep
		ibaxmat=ibaxmat+p_ns(i)*p_ntemp0
		ibakeyf=ibakeyf+p_nlog
	enddo !i=1,p_nunits  11465
	!	write(6,*)'xps'
	do ir=1,p_nrow
		!write(6,*)p_rhs(ir),p_xps(ir),p_rhs2(ir)
	enddo !ir=1,p_nrow  11486
 
	infsum=j_0
	nnf=0
	nr=0
	do irow=1,p_nrow
		if(p_ebou(irow))cycle
		nr=nr+1
		if(p_ubou(irow))then
			if(p_xps(irow).gt.p_rhs2(irow))infsum=infsum+p_xps(irow)-p_rhs2(irow)
			if(p_xps(irow).gt.p_rhs2(irow))nnf=nnf+1
		endif !if(p_ubou(irow))  11496
		! j_dapu3 infeasibility for best
		if(p_lbou(irow))then
			if(p_xps(irow).lt.p_rhs(irow))infsum=infsum+p_rhs(irow)-p_xps(irow)
			if(p_xps(irow).lt.p_rhs(irow))nnf=nnf+1
		endif !if(p_lbou(irow))  11501
	enddo !irow=1,p_nrow  11493
	!write(6,*)'noneqconst ',nr, 'nonfrow',nnf,'infsum',infsum
	!	stop
	!	do kier=1,2
	ibadat=0
	ibaxmat=0
	p_neigbas=0
	ibakeyf=0
	!	p_xps=j_0
 
unitloop:	do i=1,p_nunits
		j_dapu=j_ninf !best value
		!	isopt=p_keys(i)
		keyfactdif=.false. !is optimal factory for some log differetn than keyfactrory
		!	write(6,*)'************i,keys',i,p_keys(i)
sloop:		do j=1,p_ns(i)
			j_dapu2=j_0  !current schedule
			if(j.eq.p_keys(i))then
				ibaxmatkey=ibaxmat
				ibadatkey=ibadat
			endif !if(j.eq.p_keys(i))  11522
			do ir=1,p_nrow
				if(p_ebou(ir))cycle  !equality constraints ignored
				if(p_ix(ir).ne.0)then
					if(p_ubou(ir))then
						if(p_xps(ir).gt.p_rhs2(ir))j_dapu2=j_dapu2-p_xmat(ibaxmat+p_ix(ir))  !large value bad
					endif !if(p_ubou(ir))  11529
					! j_dapu3 infeasibility for best
					if(p_lbou(ir))then
						if(p_xps(ir).lt.p_rhs(ir))j_dapu2=j_dapu2+p_xmat(ibaxmat+p_ix(ir)) !large value good
					endif !if(p_lbou(ir))  11533
				endif !if(p_ix(ir).ne.0)  11528
			enddo !ir=1,p_nrow  11526
			!		if(j.lt.10)write(6,*)'i,j,',i,j,j_dapu2
			j_dapu3=j_0
			p_logval=j_0
			p_optfact0=0
 
			do ilog=1,p_nlog
				!			write(6,*)'ilo,',ilog,j_dmat(ibadat+p_keeplog(ilog))
				logvol=j_dmat(ibadat+p_keeplog(ilog))
				if(logvol.le.j_0)cycle
				ifactopt=0
				factopt=j_ninf
				!		p_factval=j_0  !value for different factories
				knn=j_o(p_table(ilog))%i(10)
				itab=p_logtablei(ilog)
 
				!	p_xps0=j_0
				do k=1,knn
					ifact=j_o(p_ivneig(itab))%i2(p_neigbas(itab)+k)
					if(ifact.le.0)exit
					inde=p_ibafact(ilog)+p_factw(ifact)   !index of the logfact
					iterm=p_logfactterm(inde)
					!	write(6,*)'k,ifact,inde,iterm ',k,ifact,inde,iterm
					if(iterm.le.0)exit
					irow=p_rowofterm(iterm)
 
					if(p_ubou(irow))then
						if(p_xps(irow).gt.p_rhs2(irow))j_dapu2=-logvol
					endif !if(p_ubou(irow))  11563
					! j_dapu3 infeasibility for best
					if(p_lbou(irow))then
						if(p_xps(irow).lt.p_rhs(irow))j_dapu2=logvol
					endif !if(p_lbou(irow))  11567
 
					if(j_dapu2.gt.factopt)then
						ifactopt=ifact
						factopt=j_dapu2
					endif !if(j_dapu2.gt.factopt)  11571
 
				enddo !k=1,knn  11554
 
				p_optfact0(ilog)=ifactopt
				if(ifactopt.gt.0)p_logval(ilog)=factopt
				!		if(i.le.2.and.j.le.10)write(6,*)'ifactopt,p_logval(ilog)',ifactopt,p_logval(ilog)
				if(j.eq.p_keys(i))then
					if(ifactopt.ne.p_keyfact(ibakeyf+ilog))keyfactdif=.true.
				endif !if(j.eq.p_keys(i))  11581
			enddo !ilog=1,p_nlog  11543
			p_neigbas(itab)=p_neigbas(itab)+knn
			!	if(i.le.2)write(6,*)'p_logval',p_logval
			!	if(i.eq.5)read(5,*)ii
 
			j_dapu3=sum(p_logval)  !sum
 
			if(j_dapu3+j_dapu2.gt.j_dapu)then
				!write(6,*)'imp',j,j_dapu,j_dapu2,j_dapu3,j.eq.p_keys(i)
				j_dapu=j_dapu3+j_dapu2
				p_optfact=p_optfact0
				isopt=j
				ibadatopt=ibadat
				ibaxmatopt=ibaxmat
 
			endif !if(j_dapu3+j_dapu2.gt.j_dapu)  11591
			if(j.eq.p_keys(i))then
				p_optfactkey=p_optfact
			endif !if(j.eq.p_keys(i))  11600
			ibadat=ibadat+j_dnkeep
			ibaxmat=ibaxmat+p_ntemp0
		enddo sloop !op:		do j=1,p_ns(i)  11520
 
		!write(6,*)'***i,isopt,p_keys(i)',i,isopt,p_keys(i),isopt.eq.p_keys(i)
		!write(6,'(25i5)')p_optfact
		!write(6,'(25i5)')p_keyfact(ibakeyf+1:ibakeyf+p_nlog)
		if(isopt.eq.p_keys(i).and..not.keyfactdif)goto 778
 
		!write(6,*)'i,isopt,j_dapu,',i,isopt,p_keys(i),j_dapu,ibaxmatopt,ibaxmatkey,'ibadatopt,lye',ibadatopt,ibadatkey
		p_keys(i)=isopt
 
		do ir=1,p_nrow
			!		if(p_ix(ir).ne.0)write(6,*)'ir,xps,xmat+,-',ir,p_xps(ir),p_rhs2(ir),p_xmat(ibaxmatopt+p_ix(ir)),p_xmat(ibaxmatkey+p_ix(ir))
			if(p_ix(ir).ne.0)p_xps(ir)=p_xps(ir)+p_xmat(ibaxmatopt+p_ix(ir))-p_xmat(ibaxmatkey+p_ix(ir))
 
		enddo !ir=1,p_nrow  11615
		il=0
		do ilog=1,p_nlog
			!		write(6,*)'ilog,p_keyfact(ibakeyf+ilog),p_optfact(ilog)',ilog,p_keyfact(ibakeyf+ilog),p_optfact(ilog),&
			!			'logvolopt',j_dmat(ibadatopt+p_keeplog(ilog)),'logvolkey',j_dmat(ibadatkey+p_keeplog(ilog))
			do ir=1,p_nrowinlog(ilog)
 
				il=il+1
				irow=p_rowinlog(il)
				iterm=p_rowinlogterm(il)
				ifact=p_factpart(iterm)
				!	write(6,*)'ilog,ifact,p_keyfact(ibakeyf+ilog),p_optfact(ilog)',ilog,ifact,p_keyfact(ibakeyf+ilog),p_optfact(ilog)
				!	if(ifact.eq.p_keyfact(ibakeyf+ilog))write(6,*)'ilog,irow,xps,dmat-',ilog,irow,p_xps(irow),p_rhs2(irow),&
				!		j_dmat(ibadatkey+p_keeplog(ilog))
 
				if(ifact.eq.p_keyfact(ibakeyf+ilog))p_xps(irow)=p_xps(irow)-j_dmat(ibadatkey+p_keeplog(ilog))
				!	if(ifact.eq.p_optfact(ilog))write(6,*)'ilog,irow,ifact,xps,dmat,+',ilog,irow,ifact,p_xps(irow),p_rhs2(irow),&
				!		j_dmat(ibadatopt+p_keeplog(ilog))
 
				if(ifact.eq.p_optfact(ilog))p_xps(irow)=p_xps(irow)+j_dmat(ibadatopt+p_keeplog(ilog))
 
			enddo !ir=1,p_nrowinlog(ilog)  11624
		enddo !ilog=1,p_nlog  11621
		p_keyfact(ibakeyf+1:ibakeyf+p_nlog)=p_optfact
		!		write(6,*)'????????????????'
 
		p_nnf=0
		do ir=1,p_nrow
			!		write(6,*)p_rhs(ir),p_xps(ir),p_rhs2(ir),p_ebou(ir)
			if(p_ebou(ir))cycle
			if(p_ubou(ir))then
				if(p_xps(ir).gt.p_rhs2(ir))p_nnf=p_nnf+1
			endif !if(p_ubou(ir))  11649
			! j_dapu3 infeasibility for best
			if(p_lbou(ir))then
				if(p_xps(ir).lt.p_rhs(ir))p_nnf=p_nnf+1
			endif !if(p_lbou(ir))  11653
		enddo !ir=1,p_nrow  11646
		if(p_nnf.eq.0)then
			write(6,*)'no infeasible rows, unit ',i
			exit
		endif !if(p_nnf.eq.0)  11657
778		ibakeyf=ibakeyf+p_nlog
		ibanei=ibanei+p_nlog
		write(6,*)'i',i
		call unf0()
	enddo unitloop !tloop:	do i=1,p_nunits  11515
	!	enddo !kier=1,2  10905
	write(6,*)'unf'
	call unf()
	write(6,*)'p_infsum,p_nnf,p_nunits',p_infsum,p_nnf,p_nunits
	!	read(5,*)djjd
 
 
 
end subroutine initfeas0



subroutine printfact()
	use jmod, only: p_nlog
	use jmod, only: p_nfact
	use jmod, only: p_nlogfact
	use jmod, only: p_lower
	use jmod, only: p_keeplog
	use jmod, only: p_ndupmax1
	use jmod, only: p_nterm
	use jmod, only: p_logfactterm0
	use jmod, only: p_logfactterm
	use jmod, only: p_nloginrow
	use jmod, only: p_nrowinlog
	use jmod, only: p_rowinlog
	use jmod, only: p_rowinlogterm
	use jmod, only: p_factw
	use jmod, only: p_logpart
	use jmod, only: p_tablepart
	use jmod, only: p_factpart
	use jmod, only: p_ibafact
	use jmod, only: p_keyfactbas
	use jmod, only: p_nfactinlog
	use jmod, only: p_logfactlog
	use jmod, only: p_logfactfact
	use jmod, only: p_keyfact
	use jmod, only: p_keys
	use jmod, only: p_vc
	use jmod, only: p_xmat
	use jmod, only: p_xps
	use jmod, only: p_rhs2
	use jmod, only: p_nnfcap
	use jmod, only: p_caprow
	use jmod, only: p_caprowinf
	use jmod, only: p_ivneig
	use jmod, only: p_ivneigu
	use jmod, only: p_logtablei
	use jmod, only: p_logtable
	write(6,*)' '
	write(6,*)'************printfact'
	write(6,*)'p_nlog,p_nfact,p_nlogfact',p_nlog,p_nfact,p_nlogfact
 
	write(8,*)'lower',p_lower
 
	write(6,*)'p_keeplog,  locations of log variables in keep'
	write(6,'(25i4)')p_keeplog
 
	write(6,*)'p_ndupmax1',p_ndupmax1,  'maximum number of occurences of logfact in constraints'
 
	write(6,*)'p_nterm',p_nterm
 
	write(6,*)'p_logfactterm0, logfactterms in the objective'
	write(6,'(25i4)')p_logfactterm0
 
	write(6,*)'p_logfactterm '
	write(6,'(25i4)')p_logfactterm
 
	write(6,*)'p_nloginrow '
	write(6,'(25i4)')p_nloginrow
 
	write(6,*)'p_nrowinlog '
	write(6,'(25i4)')p_nrowinlog
 
	write(6,*)'p_rowinlog '
	write(6,'(25i4)')p_rowinlog
 
	write(6,*)'p_rowinlogtern '
	write(6,'(25i4)')p_rowinlogterm
 
 
	write(6,*)'p_factw all factories '
	write(6,'(25i4)')p_factw
 
	write(6,*)'logpart of each term'
	write(6,'(25i4)')p_logpart
 
	write(6,*)'tablepart of each term'
 
	write(6,'(25i4)')p_tablepart
 
	write(6,*)'factpart of each term'
 
	write(6,'(25i4)')p_factpart
 
	write(6,*)'ibafact'
 
	write(6,'(25i4)')p_ibafact
	write(6,*)'p_keyfactbas bef',p_keyfactbas
 
	write(6,*)'p_nfactinlog number of factories for each log'
	write(6,'(25i4)')p_nfactinlog
 
	write(6,*)'p_logfactlog log of each logfact'
	write(6,'(25i4)')p_logfactlog
 
	write(6,*)'p_logfactfact  fact of each logfact'
	write(6,'(25i4)')p_logfactfact
 
	write(6,*)'p_keyfact begin'
	write(6,'(25i4)')p_keyfact(1:200)
 
	write(6,*)'p_keys begin'
	write(6,'(25i4)')p_keys(1:100)
 
 
 
	write(6,*)'vc shaow prices of rows'
	write(6,'(25f6.1)')p_vc
 
	!write(6,*)'xpsf sum of key factory terms of each row'
	!write(6,'(25f6.1)')p_xpsf
	write(6,*)'initial part of xmat'
	write(6,'(25f6.1)')p_xmat(1:200)
 
	write(6,*)'xps x -varaibles in key schdules'
	write(6,'(25f8.1)')p_xps
 
	write(6,*)'rhs2'
	write(6,'(25f6.1)')p_rhs2
 
	write(6,*)'rhs2-xps'
	write(6,'(25f6.1)')p_rhs2-p_xps(1:)   !-p_xps(1:)
 
	! write(6,*)'initial part of neig '
	! write(6,'(25i4)')p_neig(1:100)
 
	! write(6,*)'initial part of neigu '
	! write(6,'(25f8.1)')p_neigu(1:100)
 
 
	write(6,*)'currently ',p_nnfcap,' infeasible capacity constraints'
	write(6,*)'p_caprow'
	write(6,'(40i4)')p_caprow
	write(6,*)'caprowinf'
	write(6,'(40i4)')p_caprowinf(1:p_nnfcap)
 
	write(6,*)'caprows'
	write(6,'(40i4)')p_caprow(p_caprowinf(1:p_nnfcap))
 
	!	write(6,*)
	!	write(6,*)' ir  irow  nnfcap  p_caprowinf(p_nnfcap) p_xps(irow),p_rhs2(irow)'
 
	write(6,*)'p_ivneig'
	write(6,*)p_ivneig
 
	write(6,*)'p_ivneigu'
	write(6,*)p_ivneigu
 
	write(6,*)'p_logtablei'
	write(6,*)p_logtablei
 
	write(6,*)'p_logtable'
	write(6,*)p_logtable
	! nnfcap=0
	! do ir=1,p_ncaprow
	! irow=p_caprow(ir)
	! j_dapu=j_dapu+p_xps(irow)
	! j_dapu2=j_dapu2+p_rhs2(irow)
 
	! if(p_xps(irow).gt.p_rhs2(irow))then
	! nnfcap=nnfcap+1
	! icapp=ir !p_icap
	! else
	! p_feasrow(irow)=.true.
	! !	write(6,*)'p_nnfcap,ir,irow ',p_nnfcap,ir,irow
	! endif !if(p_xps(irow).gt.p_rhs2(irow))  11647
	! write(6,'(4i5,2f12.4)')ir,irow,nnfcap,icapp,p_xps(irow),p_rhs2(irow)
 
	! enddo !ir=1,p_ncaprow  11642
 
 
	write(6,*)'***endprintfact'
 
 
end subroutine printfact



subroutine initxmatxps()
	use jmod, only: p_isn16
	use jmod, only: p_xinobj
	use jmod, only: p_rowcurx
	use jmod, only: p_nunits
	use jmod, only: p_isdomain
	use jmod, only: p_filre
	use jmod, only: p_rejects
	use jmod, only: p_ibaunit
	use jmod, only: p_nrowcurx
	use jmod, only: j_inf
	use jmod, only: p_xmin
	use jmod, only: j_ninf
	use jmod, only: p_xmax
	use jmod, only: p_xmat
	use jmod, only: p_ix
	use jmod, only: p_ns
	use jmod, only: p_keys
	use jmod, only: p_xsmin
	use jmod, only: p_xsmax
	use jmod, only: p_warm
	use jmod, only: p_nrow
	use jmod, only: p_rejectnow
	use jmod, only: p_warmf
	use jmod, only: p_xpresent
	use jmod, only: p_xps
	use jmod, only: p_ispiece
	use jmod, only: j_dnkeep
	use jmod, only: p_isarea
	use jmod, only: j_dmat
	use jmod, only: p_piecexps
	use jmod, only: p_npiece
	use jmod, only: p_piecekeep
	use jmod, only: p_piecexps0
	use jmod, only: p_pieceprice
	use jmod, only: p_iprint
	use jmod, only: p_tole
	use jmod, only: p_tolep
	double precision area
	write(6,*)'*initxmatxps'
	if(p_isn16)write(16,*)'*initxmatxps'
	key=0
 
	p_xinobj=.false.
	p_xinobj=p_rowcurx(1).eq.0
	write(6,*)'sums over key schedules'
	!	p_idomba=0
	do i=1,p_nunits  !***********************
		! write(p_n16,*)'<44starti',i
		nrej=0
		!	write(6,*)'idom2',i,p_idomba
		!	if(p_isdomain)write(16,*)'bb unit',i
		if(p_isdomain)call jlpcurix(i) !determines for each row if the unit p_unit belonggs to the domain of the row
		! determines for each row if the unit iunit belonggs to the domain of the row
		! units must be accessed one by one
		! p_nrowcurx
		! p_rowcurx
		! p_idomba
		!	write(6,*)'p_nrowcurx ',p_nrowcurx,'*',p_rowcurx(1:p_nrowcurx)
 
		!returns nrowp=number of rows in this domain,
		! rowcurx= for each row in this domain tells the original (expanded) row
		!  ixcur for each original expanded row tells the index of temporary x-variable
		! facory tehtävissä tämä on kait sama
		!if(p_p8.and.i.lt.10)write(6,*)'i ,p_nrowcurx,p_rowcurx',i ,p_nrowcurx,p_rowcurx(1:p_nrowcurx)
		!		p_ibaunit(i+1)=p_ibaunit(i)+p_ns(i)
		if(p_filre)then
			if(p_rejects(p_ibaunit(i)+1))then
				! if first schedule is rejected, then p_xmin=large and p_xmax=-large
				! if first schedules is not rejcted these are obtained from the first schedule
				nrej=nrej+1
				! earlier: allocate(nunitsrow(0:nrow));nunitsrow=0
 
				do jj=1,p_nrowcurx
					j=p_rowcurx(jj)
					!		p_nunitsrow(j)=p_nunitsrow(j)+1
					p_xmin(j)=j_inf
					p_xmax(j)=j_ninf
				enddo !jj=1,p_nrowcurx  11855
				goto 745
			endif !if(p_rejects(p_ibaunit(i)+1))  11849
		endif !if(p_filre)  11848
 
		! get p_xmin and p_xmax from the first schedule
		ibaxmat=ibamatx(p_ibaunit(i)+1) !,1)
		!write(6,*)'p_nrowcurx ',p_nrowcurx,'*',p_rowcurx(1:p_nrowcurx)
		!write(6,*)'p_ix ',p_ix
		do jj=1,p_nrowcurx
			j=p_rowcurx(jj)
			!write(6,*)'j,p_ix(j),ibaxmat',j,p_ix(j),ibaxmat,' unit ',i
			!	p_nunitsrow(j)=p_nunitsrow(j)+1
			!	if(p_ix(j).ne.0)then
			p_xmin(j)=p_xmat(p_ix(j)+ibaxmat) ! v(ix(j))
			p_xmax(j)=p_xmat(p_ix(j)+ibaxmat)  ! v(ix(j))
			!	endif !if(p_ix(j).ne.0)   6706
		enddo !jj=1,p_nrowcurx  11869
		!	write(6,*)'p_xmin',p_xmin
 
745   keyopt=1     !works also if first schedule is rejected and we come from  'goto 745'
		do k=2,p_ns(i)
			if(p_filre)then
				if(p_rejects(p_ibaunit(i)+k))then
					nrej=nrej+1
					cycle  !do k=2,nsch(i)
				endif !if(p_rejects(p_ibaunit(i)+k))  11883
			endif !if(p_filre)  11882
			!if(j_o(j_divmat)%r(10261*keepx).eq.0.)stop 149
			! compute for rows containing x-variables the min and max values
 
			!		if(.not.p_fpresent)then
 
			ibaxmat=ibamatx(p_ibaunit(i)+k) !,1)
			do jj=1,p_nrowcurx
				j=p_rowcurx(jj)
				curx=p_xmat(p_ix(j)+ibaxmat)
				!	if(i.eq.1.and.k.eq.2)write(6,*)'jj,j,curx,pmax',jj,j,curx,p_xmax(j)
				if(curx.lt.p_xmin(j))then
					p_xmin(j)=curx
				endif !if(curx.lt.p_xmin(j))  11898
				if(curx.gt.p_xmax(j))then
					p_xmax(j)=curx
					if(j.eq.0)keyopt=k   ! for the case where there are no constraints
				endif !if(curx.gt.p_xmax(j))  11901
			enddo !jj=1,p_nrowcurx  11894
			!		else
			!			keyopt=p_keys(i)
 
			!		endif !if(.not.p_fpresent)   8313
 
		enddo !k=2,p_ns(i)  11881
		!	if(i.eq.1)write(6,*)'keyoptx',keyopt
		p_keys(i)=keyopt
		!	write(6,*)'p_xmintas ',p_xmin
		do jj=1,p_nrowcurx
			j=p_rowcurx(jj)
			p_xsmin(j)=p_xsmin(j)+p_xmin(j)
			p_xsmax(j)=p_xsmax(j)+p_xmax(j)
		enddo !jj=1,p_nrowcurx  11915
 
		! if(p_xinobj)then  !p_nrow.eq.0.or.j_v(j_ivdollar2).eq.237.d0)then
		! key=keyopt   ! no constraints, can be maximized directly
		! else !if(j_nrow.eq.0)then
		! 4689    key=key+1
		! if(key.gt.p_ns(i)) key=1
		! if(p_filre)then
		! if(p_rejects(p_ibaunit(i)+key))goto 4689
		! endif !if(p_filre)   8347
 
		! endif !if(p_xinobj)   8342
 
 
 
		if(p_warm.and.p_nrow.gt.0)then
 
			if(p_filre)p_rejectnow=p_rejects(p_ibaunit(i)+p_keys(i))
			if(p_keys(i).gt.p_ns(i).or.p_rejectnow)then
				p_warm=.false.
				p_warmf=.false.
				write(6,*)'*data does not agree, warm-> ignored'
				!		else !if(j_keys(i).gt.j_nsch(i).or.p_rejectnow)then
				!			key=p_keys(i)
			endif !if(p_keys(i).gt.p_ns(i).or.p_rejectnow)  11937
		endif !if(p_warm.and.p_nrow.gt.0)  11934
 
 
		key=p_keys(i)
		if(p_xpresent)then
			!		write(19,*)'p_nrowcurx( ',p_nrowcurx,p_rowcurx
			!		write(19,*)'i ',i,'key',p_keys(i),'ibaxmat',ibaxmat
			ibaxmat=ibamatx(p_ibaunit(i)+key) !,1)
 
			!write(6,*)'p_nrowcurx( ',p_nrowcurx,p_rowcurx
			!summat nyt
			do jj=1,p_nrowcurx
				j=p_rowcurx(jj)
				!			write(19,*)p_xmat(p_ix(j)+ibaxmat)
				!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
				p_xps(j)=p_xps(j)+p_xmat(p_ix(j)+ibaxmat) !v(ix(j)) !(ix(j)=0 -> no x in row
				!		if(j.eq.0.and.i.ge.6.and.i.le.10)write(p_n16,*)'<555 ',i,ibaxmat,p_ix(j),p_xmat(p_ix(j)+ibaxmat),p_xps(j)
				!	if(j.eq.0)write(6,*)'xps0here',p_xps(0)
			enddo !jj=1,p_nrowcurx  11955
 
			if(p_ispiece)then  !updtaing piecexps inixdat
				iba=(p_ibaunit(i)+key-1)*j_dnkeep
				!		write(6,*)'specesum,bef',p_piecexps
				if(p_isarea)then  !update xps
					p_piecexps(1:p_npiece)=p_piecexps+area(i)*j_dmat(iba+p_piecekeep)
 
					!		p_ivpiecexps(1:p_npiece)=p_piecexps+area(i)*j_o(j_divmat)%d(iba+p_piecekeep)
 
				else
					p_piecexps(1:p_npiece)=p_piecexps+j_dmat(iba+p_piecekeep)
 
 
					!		j_o(p_ivpiecexps)%d(1:p_npiece)=j_o(p_ivpiecesum)%d(1:p_npiece)+j_o(j_divmat)%d(iba+p_piecekeep)
				endif !if(p_isarea)  11967
				!		write(6,*)'specesum,af',p_piecexps
			endif !if(p_ispiece)  11964
 
		endif !if(p_xpresent)  11948
 
		!		if(p_fpresent)call factxps(i,p_keys(i))
 
 
	enddo !i=1,p_nunits  11829
 
	if(p_ispiece)then
		p_piecexps0=dot_product(p_pieceprice,p_piecexps)
		!	write(6,*)'p_pieceprice,p_piecexps,',p_pieceprice,p_piecexps,p_piecexps0
 
	endif !if(p_ispiece)  11988
 
	p_iprint=2
	!	p=.true.
 
	!	call testxps('ihanalus')
 
	!	write(6,*)'onko ok'
	!	read(5,*)kfjkfjk
 
	! if(ivprint7.gt.0)then
	! if(j_v(ivprint7).eq.6.)call printproblem()
	! endif !if(ivprint7.gt.0)   6459
	!	write(p_n16,*)'xps bef preopt',i,(p_xps(jj7),jj7=0,min(p_nrow,10)) !!!!
	!is(p_p)write(p_n16,*)'p_nrow',p_nrow
	if(p_iprint.gt.1)then
		!write(6,*)' ',j_err
		write(6,*)' '
		write(6,*)'***row, min,max,initial value, tolerance'
	endif !if(p_iprint.gt.1)  12007
 
	if(p_iprint.gt.0)write(6,*)'min and max sums of x-variable rows, sum in key-schedules and tolerance'
	do j=0,p_nrow  !nrowp = number of rows with x-variables
		!	j=p_rowcurx(jj) !j= number of the row
		!1.d-5 changed into 1.d-6 21.12.2009
		! it must be studied if this tolerance is reasonable
		!	p_tole(j)= max(p_tolep*1.d-6* (p_xsmax(j)-p_xsmin(j))/ &
		!		p_nunitsrow(j),p_tolep*1.d-7)
		if(p_ix(j).ne.0)then
			p_tole(j)= max(p_tolep*1.d-6* (p_xsmax(j)-p_xsmin(j))/ &
				p_nunits,p_tolep*1.d-7)
			if(p_iprint.gt.0)&
				write(6,'(i5,1x,4g18.12)')j,p_xsmin(j),p_xsmax(j),p_xps(j),p_tole(j)
		endif !if(p_ix(j).ne.0)  12020
	enddo !j=0,p_nrow  12014
 
	! write(6,*)'xpshere',p_xps
	! write(6,*)'ixhere',p_ix
	! write(6,*)'xmt',p_xmat(1:30)
 
end subroutine initxmatxps



subroutine printz()
	use jmod, only: p_xpresent
	use jmod, only: p_nvar
	use jmod, only: j_printname
	use jmod, only: p_vars
	use jmod, only: j_yes
	use jmod, only: p_fpresent
	use jmod, only: p_isvarf
	use jmod, only: p_isxvar
	use jmod, only: p_nz
	nnz=0
	! printing z-variables
	if(.not.p_xpresent)then
		do i=1,p_nvar
			call j_printname(' ',p_vars(i),' ')
		enddo !i=1,p_nvar  12040
		return
	endif !if(.not.p_xpresent)  12039
	do i=1,p_nvar
		!fact
		!isfx ja isfy tarkistetaan vain, jos on tehdastehtävä
		j_yes=.true.
		if(p_fpresent)j_yes=.not.p_isvarf(i)
 
 
 
		if(.not.p_isxvar(i).and.j_yes)then  !.not.p_isvarf(i))then
			nnz=nnz+1
			call j_printname(' ',p_vars(i),' ')
			if(nnz.ge.10.and.nnz.lt.p_nz)then
				write(6,*)' ...etc...'
				exit
			endif !if(nnz.ge.10.and.nnz.lt.p_nz)  12056
		endif !if(.not.p_isxvar(i).and.j_yes)  12053
 
	enddo !i=1,p_nvar  12045
 
	return
 
 
end subroutine printz



! subroutine jlp00(iob,io)
! ! no constraints
! integer,dimension(:),allocatable::ikeepc,ikeepx,itermc,itermx
! integer*8 ibax,nkeepx,isbas
! !without
! !	call jlpcoef_(p_ivproblem,j_ivout,ivobjects)
! call initjlp2(iob,io)   ! jlp00(    no constraints
! !kahteen kertaan
! if(allocated(p_sumx))deallocate(p_sumx)
! write(6,*)'domains ',p_ndom,' showdomains ',p_nshow,' show variables',p_nsumx
! !	write(6,*)'ndom2,p_nshow',ndom2,p_nshow,' p_nsumx ',p_nsumx,p_nsumx*(ndom2+p_nshow)
! allocate(p_sumx(1:p_nsumx*(p_ndom+p_nshow)))
! p_sumx=j_0

! !get options and when xpresent
! ! p_ivtrans
! ! p_maxrounds
! ! p_zopt
! ! p_ivix
! ! p_ix
! ! p_nnotareavars
! ! p_tried
! ! p_fastusedsame
! ! p_intapp
! ! p_fastmake
! ! p_fastpros p_fastpros2  /100
! ! p_fast
! ! p_warm p_warmf

! nc=0
! nx=0
! ikeepc0=0
! !if(allocated(ikeepc))deallocate(ikeepc)
! !	write(6,*)'p_nterm',p_nterm
! ivw=j_defmatrix(j_ivout,'%weights',int8(j_dnobs),j_18,j_matreg)
! allocate(ikeepc(1:p_nterm),ikeepx(1:p_nterm),itermc(1:p_nterm),itermx(1:p_nterm))
! do ival=1,p_nterm
! i=p_varofterm(ival)
! !	write(6,*)'i',i,j_otype(i),j_iptable,j_divkeepup,j_divkeep
! if(j_otype(i).eq.j_ipneigtable)cycle
! if(.not.p_isunit)then
! ikeepc0=j_inlistobject(i,j_divkeepup)
! if(ikeepc0.gt.0)then
! nc=nc+1
! ikeepc(nc)=ikeepc0
! itermc(nc)=ival
! endif !if(ikeepc0.gt.0)  11875
! endif !if(.not.p_isunit)  11873
! ikeepx0=j_inlistobject(i,j_divkeep)
! if(j_err)return
! if(ikeepx0.gt.0)then
! if(ikeepc0.gt.0)nc=nc-1
! nx=nx+1
! ikeepx(nx)=ikeepx0
! itermx(nx)=ival
! endif !if(ikeepx0.gt.0)  11883
! if(ikeepx0.le.0.and.ikeepc0.le.0)then
! call j_getname(i)
! write(6,*)j_oname(1:j_loname),' is not in cdata or in xdata'
! j_err=.true.
! endif !if(ikeepx0.le.0.and.ikeepc0.le.0)  11889
! enddo !ival=1,p_nterm  11869

! nkeepx=j_o(j_divkeep)%i(1)
! nkeepc=0
! if(.not.p_isunit)nkeepc=j_o(j_divkeepup)%i(1)
! if(j_err)goto 90
! p_ivkeys=j_deflist(j_ivout,'%keys',list0=p_nunits,ilist=.true.) ! jlp00
! p_keys=>j_o(p_ivkeys)%i2(1:p_nunits)
! ibac=0
! ibax=j_08
! isbas=j_08
! p_objf=j_0

! !	write(6,*)'jfff,p_nsumx,p_ndom,p_nshow',p_nsumx,p_ndom,p_nshow
! !	write(6,*)'nc,nkeepx,nkeepc,p_nunits,',nc,nkeepx,nkeepc,p_nunits,'nx',nx
! !	write(6,*)'coef', p_coef(1:p_nterm)
! !	write(6,*)'HHHHH ',p_nunits,p_ns(1:5),nc,'ikeepx',ikeepx
! !	write(6,*)'coef ',p_coef(1:p_nterm)

! if(nc.eq.0)then  !there are upper level variables in the prob
! do i=1,p_nunits
! ns=p_ns(i)   !j_o(j_divmatup)%d(ibac+1)
! !		write(6,*)'iunit,ns',i,ns,p_nterm,j_divmat,ibax,ikeepx(1:nx),ibax+ikeepx

! j_dapu=dot_product(p_coef(1:p_nterm),j_o(j_divmat)%d(ibax+ikeepx))
! !			write(18,*)i,ns,ibax,j_o(j_divmat)%d(ibax+ikeepx),j_dapu
! if(.not.p_maxo)j_dapu=-j_dapu
! p_keys(i)=1
! ibax=ibax+nkeepx
! do j=2,ns
! j_dapu2=dot_product(p_coef(1:p_nterm),j_o(j_divmat)%d(ibax+ikeepx))
! !			write(18,*)j,ibax,j_o(j_divmat)%d(ibax+ikeepx),j_dapu2,j_dapu
! if(.not.p_maxo)j_dapu2=-j_dapu2
! if(j_dapu2.gt.j_dapu)then
! p_keys(i)=j
! j_dapu=j_dapu2
! endif !if(j_dapu2.gt.j_dapu)  11927
! ibax=ibax+nkeepx

! enddo !j=2,ns  11923
! ibac=ibac+nkeepcp
! p_objf=p_objf+j_dapu

! if(p_isn16)write(16,*)' obj bys',i,p_objf,j_dapu,ns
! j_o(ivw)%d(isbas+p_keys(i))=j_1
! isbas=isbas+p_ns(i)
! enddo !i=1,p_nunits  11914
! else
! do i=1,p_nunits
! ns=p_ns(i)   !j_o(j_divmatup)%d(ibac+1)
! j_dapu=j_ninf

! do j=1,ns
! j_dapu2=j_0
! do j2=1,nx
! j_dapu2=j_dapu2+p_coef(itermx(j2))*j_o(j_divmat)%d(ibax+ikeepx(j2))

! enddo !j2=1,nx  11948
! if(.not.p_maxo)j_dapu2=-j_dapu2
! if(j_dapu2.gt.j_dapu)then
! p_keys(i)=j
! j_dapu=j_dapu2
! endif !if(j_dapu2.gt.j_dapu)  11953
! ibax=ibax+nkeepx

! p_objf=p_objf+j_dapu
! if(p_isn16)write(16,*)' obj fhfhhf',p_objf
! enddo !j=1,ns  11946
! j_dapu2=j_0
! do j=1,nc
! j_dapu2=j_dapu2+p_coef(itermc(j))*j_o(j_divmatup)%d(ibac+ikeepc(j))

! enddo !j=1,nc  11963
! if(.not.p_maxo)j_dapu2=-j_dapu2

! p_objf=p_objf+j_dapu2
! if(p_isn16)write(16,*)' obj ffuuf',p_objf
! ibac=ibac+nkeepc
! j_o(ivw)%d(isbas+p_keys(i))=j_1
! isbas=isbas+p_ns(i)
! enddo !i=1,p_nunits  11942

! endif !if(nc.eq.0)  11913
! write(6,*)' obj hhy',p_objf
! 90 deallocate(ikeepc,ikeepx,itermc,itermx)
! end subroutine




subroutine getsumxkey(ivo)
	use jmod, only: j_0
	use jmod, only: p_sumx
	use jmod, only: p_nunits
	use jmod, only: p_isdomain
	use jmod, only: p_ibaunit
	use jmod, only: p_keys
	use jmod, only: p_ntemp0
	use jmod, only: p_xmatrow
	use jmod, only: p_xmat
	use jmod, only: j_o
	use jmod, only: p_vx
	use jmod, only: p_isunit
	use jmod, only: j_getobsiv
	use jmod, only: j_divmatup
	use jmod, only: j_divkeepup
	use jmod, only: j_divobsup
	use jmod, only: j_err
	use jmod, only: p_nshow
	use jmod, only: p_ivshowtrans
	use jmod, only: p_ivtrans
	use jmod, only: j_divmat
	use jmod, only: j_divkeep
	use jmod, only: p_isarea
	use jmod, only: j_v
	use jmod, only: p_xvararea
	use jmod, only: p_ivarea
	use jmod, only: p_ndom
	use jmod, only: p_nxvararea
	use jmod, only: p_ivshowmatrix
	use jmod, only: p_ivshowunits
	logical indomain,price
	close(18)
	p_sumx=j_0
	!	p_idomba=0
	!if(p_isunit)j_divobsubtrans=p_ivtrans
	price=ivo.gt.0
 
 
	do iuni=1,p_nunits
		!p_iunit=iuni
		if(price)then !same as in senter
			! write(6,*)'iuni',iuni,'xmat','key',p_keys(iuni)
 
			if(p_isdomain)then   !then pack must be updated
				call jlpcurix(iuni)  !this updates p_idomba thus indomain2 must tbe used
				!			call pack()
 
			endif !if(p_isdomain)  12236
			call getvx()
			! do i=1,p_ns(iuni)
			! j_dapu=dot_product(p_vxpack(1:p_nxbas),p_xmat(p_ixpack(1:p_nxbas)+iib) )
			! write(6,*)iuni,i,j_dapu,p_xmat(iib+1:iib+p_ntemp0)
			! iib=iib+p_ntemp0
			! enddo !i=1,p_ns(iuni)  13102
 
 
			ikey_ = p_ibaunit(iuni)+p_keys(iuni)
			!	ibaxmat=(p_ibaunit(p_unit)-1)*p_ntemp0
			ibaxmat=(ikey_-1)*p_ntemp0
			!	write(6,*)'ibaxmat',ibaxmat
			!	write(6,*)p_vxpack(1:p_nxbas),'*',p_xmat(p_ixpack(1:p_nxbas)+ibaxmat)
			p_xmatrow=p_xmat(ibaxmat+1:ibaxmat+p_ntemp0)
			j_o(ivo)%d(iuni)=dot_product(p_vx,p_xmatrow)
			!	j_o(ivo)%d(iuni)=dot_product(p_vxpack(1:p_nxbas),p_xmat(p_ixpack(1:p_nxbas)+ibaxmat) )
			!	write(6,*)'res',j_o(ivo)%d(iuni)
			!this handles shadoew prices
		endif !if(price)  12233
 
		if(.not.p_isunit)then
			call j_getobsiv(iuni,j_divmatup,j_divkeepup,j_divobsup) !,p_ivtransc,j_ivunit)
 
			if(j_err)then
				write(6,*)'error (66) for unit ',iuni
				return
			endif !if(j_err)  12264
			if(p_nshow.gt.0)then
				! do isi=1,p_nshowin
				! isv=j_o(p_ivshowin)%i2(isi)
				! call j_getname(isv)
				! write(6,*)'uni ',iuni,isi,j_oname(1:j_loname),j_v(isv)
 
				! enddo !isi=1,p_nshowin  13041
				call dotrans(p_ivshowtrans,1)
				!		write(6,*)'showw',j_o(p_ivshowmatrix)%d
				if(j_err)return
			endif !if(p_nshow.gt.0)  12268
			if(p_ivtrans.gt.0)then
				call dotrans(p_ivtrans,1)
				if(j_err)stop 770
			endif !if(p_ivtrans.gt.0)  12279
		endif !if(.not.p_isunit)  12261
		! now sumx is updated
		iobs=p_ibaunit(iuni)+p_keys(iuni)
		call j_getobsiv(iobs,j_divmat,j_divkeep,0)! ,p_ivtransx,0)  !getsolx
		if(j_err)then
			write(6,*)'error for observation ',iobs
			return
		endif !if(j_err)  12287
		if(p_isarea)then
 
			!write(6,'(20f6.1)')j_v(j_o(j_divkeep)%i2(1:p_nxvartot))
			!	j_dapu=j_v(p_ivarea)
			!	write(6,*)'iuni ',iuni,',area,p_nxvartot,x',j_v(p_ivarea)
			!if(p_ipart)then
			j_v(p_xvararea)=j_v(p_ivarea)*j_v(p_xvararea)
 
			!	write(6,'(20f6.0)')j_v(j_o(j_divkeep)%i2(1:p_nxvartot))
		endif !if(p_isarea)  12291
 
 
 
		if(p_isdomain)then
			iba= 0  !p_ndom*p_nxvararea   !0
			!	p_idomba=(iuni-1)*p_ndomv
			!	write(6,*)'ibanyt ' ,iba
			do id=1,p_ndom
				!			icurint=(id-1)/32+1;icurbit=p_id-(icurint-1)*32-1
				!					if(.not.btest(p_domainbits(p_idomba+icurint),icurbit))then
				if(indomain(id,iuni))then  !indomain
					!	write(16,*)'keuiuni,',iuni,id,'yes'
					!         ndomsol(p_id)=ndomsol(p_id)+1
					!	if(id.eq.3)write(6,*)'dom,iuni,iba',iuni,iba,'p_ndom',p_ndom
					!	if(p_ivarea.gt.j_0)then
					p_sumx(iba+1:iba+p_nxvararea)=p_sumx(iba+1:iba+p_nxvararea)+ j_v(p_xvararea)
					!	write(18,'(3i5,10f8.2)')iuni,p_keys(iuni),iobs,j_v(p_xvararea)
					!	if(id.eq.3)write(6,*)'sumdom,iba',iba,p_sumx(iba+1:iba+3)
					!	do jx=1,p_nxvartot
					!		p_sumx(iba+jx)=p_sumx(iba+jx)+ j_v(j_o(j_divkeep)%i2(jx))*j_dapu
					!	end do !jx=1,p_nxvartot  13097
					!else
					!	do jx=1,p_nxvartot
					!		p_sumx(iba+jx)=p_sumx(iba+jx)+ j_v(j_o(j_divkeep)%i2(jx))
					!	end do !jx=1,p_nxvartot  13101
					!	endif !if(p_ivarea.gt.j_0)  13096
					!	if(p_ncvar.gt.0)p_sumx(iba+p_nxvartot+1:iba+p_nxvartot+p_ncvar)= &
					!		p_sumx(iba+p_nxvartot+1:iba+p_nxvartot+p_ncvar)+j_v(p_cvarl(1:p_ncvar))
 
					! if(j_divobsubtrans>0) p_sumx(iba+p_nxvartot+p_ncvar+1:iba+p_nxvartot+p_ncvar+p_noutsubtrans) = &
					! p_sumx(iba+p_nxvartot+p_ncvar+1:iba+p_nxvartot+p_ncvar+p_noutsubtrans) + &
					! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
					! if(j_divobsubtrans>0) p_sumx(iba+p_nxvartot+1:iba+p_nxvartot+p_noutsubtrans) = &
					! p_sumx(iba+p_nxvartot+1:iba+p_nxvartot+p_noutsubtrans) + &
					! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
 
 
				end if !if(indomain(id,iuni))  12311
				iba=iba+p_nxvararea !+p_noutsubtrans !+p_ncvar
			enddo !id=1,p_ndom  12308
 
 
 
		else !if(j_ndom.gt.0)then
			!	write(6,*)'iunibef ',iuni,p_sumx(1),j_v(p_xvararea(1))
			p_sumx(1:p_nxvararea)= p_sumx(1:p_nxvararea)+j_v(p_xvararea)
			!	write(6,*)'iuniaf,sum,v',p_sumx(1),j_v(p_xvararea(1))
			!	write(6,*)'ny ',j_v(p_xvararea(3))
			!			p_sumx(1:p_nxvararea)= p_sumx(1:p_nxvararea)+j_v(j_o(j_divkeep)%i2(1:p_nxvartot))
			!		endif !if(p_isarea)  13114
			!	if(p_ncvar.gt.0)p_sumx(p_nxvartot+1:p_nxvartot+p_ncvar)= &
			!		p_sumx(p_nxvartot+1:p_nxvartot+p_ncvar)+j_v(p_cvarl(1:p_ncvar))
 
			! if(j_divobsubtrans>0) p_sumx(p_nxvartot+p_ncvar+1:p_nxvartot+p_ncvar+p_noutsubtrans) = &
			! p_sumx(p_nxvartot+p_ncvar+1:p_nxvartot+p_ncvar+p_noutsubtrans) + &
			! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
			! if(j_divobsubtrans>0) p_sumx(p_nxvartot+1:p_nxvartot+p_noutsubtrans) = &
			! p_sumx(p_nxvartot+1:p_nxvartot+p_noutsubtrans) + &
			! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
			!		iba=p_nxvararea
 
		end if !if(p_isdomain)  12304
 
		if(p_ndom.eq.0)then
			iba=p_nxvararea
		else
			iba=p_ndom*p_nxvararea
		endif !if(p_ndom.eq.0)  12364
		!iba=p_ndom*p_nxvararea
		!	write(6,*)'pndom',p_ndom,'iba show',iba
 
		if(p_nshow.gt.0)then
 
			!write(6,*)'idshow ',j_o(p_ivshowmatrix)%d,'iba ',iba
			do id=1,p_nshow
				!	write(6,*)'idshow id iba ',id,iba
				if(j_o(p_ivshowmatrix)%d(id).ne.j_0)then
					j_o(p_ivshowunits)%i2(id)=j_o(p_ivshowunits)%i2(id)+1
					!	if(p_isarea)then
					p_sumx(iba+1:iba+p_nxvararea)=p_sumx(iba+1:iba+p_nxvararea)+j_v(p_xvararea)
 
					!	write(6,*)'sumso,iuni',iuni,p_sumx(iba+1:iba+3)
					! do jx=1,p_nxvartot
					! p_sumx(iba+jx)=p_sumx(iba+jx)+ j_v(j_o(j_divkeep)%i2(jx))*j_dapu
					! end do !jx=1,p_nxvartot  13145
 
					! else
					! do jx=1,p_nxvartot
					! p_sumx(iba+jx)=p_sumx(iba+jx)+ j_v(j_o(j_divkeep)%i2(jx))
					! end do !jx=1,p_nxvartot  13150
					! endif !if(p_isarea)  13144
				endif !if(j_o(p_ivshowmatrix)%d(id).ne.j_0)  12377
				iba=iba+p_nxvararea
			enddo !id=1,p_nshow  12375
		endif !if(p_nshow.gt.0)  12372
 
	end do !iuni=1,p_nunits  12231
 
end subroutine getsumxkey



subroutine checkinfeas()
	use jmod, only: p_rowcurx
	use jmod, only: p_i1
	use jmod, only: p_nrowcurx
	use jmod, only: p_nterminrow
	use jmod, only: j_getline
	use jmod, only: p_ivrow
	use jmod, only: p_buf
	use jmod, only: p_xsmin
	use jmod, only: p_xsmax
	use jmod, only: p_rhs
	use jmod, only: p_rhs2
	use jmod, only: j_v
	use jmod, only: p_ivfeasible
	!	write(6,*)'jerrher',j_err
	!infeasiblity at start tarkastus siirretty tähän
	! test if problem is immediately infeasible !!!!
	iunf=0
	if(p_rowcurx(1).eq.0)then
		p_i1=2  ! there is x variable in the objective row
	else !if(j_rowcurx(1).eq.0)then
		p_i1=1   ! there is no  "
	endif !if(p_rowcurx(1).eq.0)  12408
	nunf=0 ! number of infeasible rows
	!write(6,*)'<66p_nz ',p_nz
	if(p_nrowcurx.gt.0)write(6,*)'row_index, row, smallest posssible value, largest possible value, rsh,rhs2'
		rowloop:			do jj=1,p_nrowcurx
		j=p_rowcurx(jj)
		!	if(j.eq.0)cycle rowloop
		!	call j_printname(' tas22 ',j,' ')
 
 
		if(p_nterminrow(j).ne.1)cycle rowloop
		call j_getline(p_ivrow,j+1,p_buf,le)
		if(j.eq.0)then
			write(6,*)j,p_buf(1:le),p_xsmin(j),p_xsmax(j)
		else
			write(6,*)j,p_buf(1:le),p_xsmin(j),p_xsmax(j),p_rhs(j),p_rhs2(j)
			if(p_xsmin(j).gt.p_rhs2(j).or.p_xsmax(j).lt.p_rhs(j))then
				nunf=nunf+1
				write(6,*)'***nonfeasible row ',nunf
			endif !if(p_xsmin(j).gt.p_rhs2(j).or.p_xsmax(j).lt.p_rhs(j))  12428
		endif !if(j.eq.0)  12424
 
	enddo rowloop !loop:			do jj=1,p_nrowcurx  12416
	if(iunf.ne.0)j_v(p_ivfeasible)=0
	!	write(6,*)'jerHER',j_err
 
end subroutine checkinfeas

subroutine initfltot()
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_nrow
	use jmod, only: p_ncol
	use jmod, only: p_a
	use jmod, only: p_lavec
	use jmod, only: p_ls
	use jmod, only: p_ifail
	use jmod, only: p_nrefac
	use jmod, only: j_err
	use fletdmod !nopre!   !file jlpd.f90
	use fletdmod2 !nopre!
	!	use jlpdmod !nopre!
 
	use fletcherdmod  !nopre!
	use lastmod !nopre!
	integer ::nup
	logical indomain  !,indomain2
	common/noutc/nout
	common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj  !Fletcher parameters
	!dense common/factorc/m0,m1,mm0,mm,mp,mq
	!sparse common/factorc/m1,m2,mp,mq,lastr,p_irow
 
	!this means that when in dense mpjj and mqjj are changed
	! then in sparse mpjj is the same as
 
	common/ipivotc/ipivot99  !used only in fltcherd.for to print pivot at malfunctio
	common/refactorc/nup,nfreq
	! write(6,*)'initfltot',p_nrow,p_ncol,p_nrow,nrefac
	! write(6,*)'p_a',p_a
	! write(6,*)'p_lavec',p_lavec
	! write(6,*)'p_ls',p_ls
	!	p_a,p_nrow,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac
	!	call initflet(p_nrow,p_ncol,p_a,la,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac)
	! write(6,*)'lu1',lu1
	! write(6,*)'ll1',ll1
	! write(6,*)'pifail',p_ifail
	write(6,*)'*initfltot'
	if(p_isn16)write(p_n16,*)'*initfltot'
	call initflet(p_nrow,p_ncol,p_a,p_nrow,p_lavec,p_ls,lu1,ll1,p_ifail,p_nrefac)
	!	write(6,*)'afdti',p_ifail
	!	write(6,*)'LS ',p_ls
	if(p_ifail.gt.0)then
		write(6,*)'**at startup p_ifail=',p_ifail, 'tell J. Lappi'
		j_err=.true.;return
	endif !if(p_ifail.gt.0)  12473
 
 
 
 
end subroutine





! subroutine defsolu()
! write(6,*)'defsolu'
! end subroutine

subroutine getsolx() !
	use jmod, only: p_nz
	use jmod, only: j_0
	use jmod, only: p_solx
	use jmod, only: p_nunits
	use jmod, only: p_isdomain
	use jmod, only: p_ibaunit
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: p_xmat
	use jmod, only: p_ix
	use jmod, only: p_ndom
	use jmod, only: p_sumxi
	use jmod, only: p_shpx
	use jmod, only: p_nsumx
	use jmod, only: p_intapp
	use jmod, only: p_nshow
	use jmod, only: j_defmatrix
	use jmod, only: j_ivout
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: j_getname
	use jmod, only: p_xvararea
	use jmod, only: p_lx0
	use jmod, only: p_x
	use jmod, only: p_lx
	use jmod, only: p_nrowz
	use jmod, only: p_lunit
	use jmod, only: p_isunit
	use jmod, only: j_getobsiv
	use jmod, only: j_divmatup
	use jmod, only: j_divkeepup
	use jmod, only: j_divobsup
	use jmod, only: p_ivtrans
	use jmod, only: p_ivshowtrans
	use jmod, only: p_keys
	use jmod, only: j_divmat
	use jmod, only: j_divkeep
	use jmod, only: j_v
	use jmod, only: p_isarea
	use jmod, only: p_ivarea
	use jmod, only: p_isch
	use jmod, only: p_sumx
	use jmod, only: p_nxvararea
	use jmod, only: j_o
	use jmod, only: p_ivshowmatrix
	logical indomain  !,indomain2
	double precision weisum,wei
	!computes the sums of all x-variables in all domains
 
	call testxps('getsolx')
	if(p_nz.eq.0)then
		!!compute integer approximation for rows
		p_solx=j_0
		!	p_idomba=0
 
		do i=1,p_nunits  !***********************
			!write(6,*)'idom9'
			!	if(p_isdomain)write(16,*)'hoo ',i
			if(p_isdomain)call jlpcurix(i)
			! determines for each row if the unit iunit belonggs to the domain of the row
			! units must be accessed one by one
			! p_nrowcurx
			! p_rowcurx
			! p_idomba
			!returns nrowp=number of rows in this domain,
			! rowcurx= for each row in this domain tells the original (expanded) row
			!  ixcur for each original expanded row tells the index of temporary x-variable
			ibaxmat=ibamatx(p_ibaunit(i)+integerschedw(i))  !,1)
			do jj=1,p_nrowcurx
				j=p_rowcurx(jj)
				p_solx(j)=p_solx(j)+p_xmat(p_ix(j)+ibaxmat)
			enddo !jj=1,p_nrowcurx  12515
		enddo !i=1,p_nunits  12502
	endif !if(p_nz.eq.0)  12497
 
 
	! if(j_divobsubtrans>0) then
	! p_ivoutsubtrans = j_trans_output(j_divobsubtrans)
	! p_noutsubtrans = j_o(p_ivoutsubtrans)%i2(1)
	! !write(6,*)'<55>',p_ivoutsubtrans,p_noutsubtrans
	! else !if(j_divobsubtrans>0) then
	!	p_noutsubtrans = 0
	!endif !if(j_divobsubtrans>0)  14165
 
	!p_nxvararea=0
 
	!p_ipart=.false. in initarea
	ndom2=max(p_ndom,1)
 
	!if(allocated(p_shpx))deallocate(p_shpx)
 
	if(allocated(p_sumxi))deallocate(p_sumxi)
 
	allocate(p_shpx(1:p_nsumx*ndom2))
	!	p_sumx=j_0
	p_shpx=j_0
	!	write(6,*)'pnshow',p_nshow
	if(p_intapp)allocate(p_sumxi(1:p_nsumx*(ndom2+p_nshow)))
	!close(17)
	!write(17,*)'getsumxhere',p_isdomain
	!	write(6,*)'dommmbbef',p_sumx(16:18),p_sumx(24:26)
	ivo=j_defmatrix(j_ivout,'%priceunit',int8(p_nunits),j_18,j_matreg)
	!write(6,*)'j_ivout,ivo ',j_ivout,ivo
	call getsumxkey(ivo) !sums over key scheds if ivo ten aslo prices
 
	!write(17,*)'sumsaftkey'
	!write(17,'(9f10.1)')p_sumx
	!write(17,*)'yht'
	!write(17,'(9f10.1)')p_sumx(10:18)+p_sumx(19:27)
	!write(17,*)'lr',p_lr(1:p_lr0)
	!write(17,*)'res',p_x(p_lr(1:p_lr0))
 
	!write(6,*)'dommm',p_sumx(16:18),p_sumx(24:26)
	! write(18,*)j_v(j_ivdollar)
	! write(18,'(10f8.1)')p_sumx(1:p_nxvararea)
	! write(18,'(10f8.1)')p_sumx(p_nxvararea+1:p_nxvararea+p_nxvararea)
	!	close(19)
 
	! lcur=p_next(0)
	! nex=p_next(lcur)
	! lunitnext=p_lunit(nex)
	! do i=1,p_lx0
 
	! if(lunitnext.ne.lunit(lcur))then
	! iobs=p_ibaunit(iuni)+p_keys(lunit(lcur))
	! call j_getobsiv(iobs,j_divmat,j_divkeep,0) !,p_ivtransx,0)  !getsolx
	! if(p_isarea)wei=wei*j_v(p_ivarea)
 
	! endif !if(lunitcur
	! iuni=p_lunit(lcur)
	! wei=p_x(lcur+p_nrowz)
	! weisum=weisum+wei
	! iobs=p_ibaunit(iuni)+p_isch(lcur)
	! call j_getobsiv(iobs,j_divmat,j_divkeep,0) !,p_ivtransx,0)  !getsolx
	! if(p_isarea)wei=wei*j_v(p_ivarea)
	! if(p_isdomain)then
	! iba=0
	! do id=1,p_ndom
	! !		icurint=(id-1)/32+1;icurbit=p_id-(icurint-1)*32-1
	! !		if(.not.btest(p_domainbits(p_idomba+icurint),icurbit))then
	! write(17,*)'id,j_ivall,p_domvars',id,j_ivall,p_domvars
	! write(17,*)'iuni,id,indomain',iuni,id,indomain(id,iuni),j_v(j_o(j_divkeepup)%i2(2)),wei
	! if(indomain(id,iuni))then
 
	! p_sumx(iba+1:iba+p_nxvararea)=p_sumx(iba+1:iba+p_nxvararea)+wei* j_v(p_xvararea)
	! write(17,*)'koo,iba,dom,wei ',koo,iba,id,wei
	! end if !if(indomain(id,iuni))  12494
 
	! iba=iba+p_nxvararea  !+p_noutsubtrans
	! end do !id=1,p_ndom  12489
	! else !if(j_ndom.gt.0)then
 
	! p_sumx(1:p_nxvararea)=p_sumx(1:p_nxvararea)+wei*j_v(p_xvararea)
 
	! end if !if(p_isdomain)  12486
 
	! iobs=p_ibaunit(iuni)+p_keys(iuni) !update for keyshced and explicit basic
	! call j_getobsiv(iobs,j_divmat,j_divkeep,0) !,p_ivtransx,0)  !getsolx
	! if(p_isarea)wei=wei*j_v(p_ivarea)
 
 
 
 
	! enddo
	!write(17,*)'isarea',p_isarea,'nrow',p_nrow,p_nrowtot
	call j_getname(p_xvararea(1))
	!write(17,*)'oname',j_oname(1:j_loname),p_xps(0)
	!write(17,*)'lx',p_lx0
	!write(17,*)p_lx(1:p_lx0)
	! do ij=1,p_lx0
	! p_akey00=p_a(p_abas(p_lx(ij)+p_nz)+1:p_abas(p_lx(ij)+p_nz)+p_nrow)
	! !write(17,'(20f11.1)')p_akey00  !(p_lx(1:p_lx0))
	! enddo !ij=1,p_lx0  12497
 
	!write(17,*)p_rhsw
	!write(17,*)p_lunit(p_lx(1:p_lx0))
	!write(17,*)p_isch(p_lx(1:p_lx0))
	! write(17,*)p_keys(p_lunit(p_lx(1:p_lx0)))
	! do ij=1,p_lx0
	! write(17,'(20f11.1)')p_aopt(p_abas1(p_lx(ij))+2:p_abas1(p_lx(ij))+p_nrowtot)
	! write(17,'(20f11.1)')p_akey(p_abas1(p_lx(ij))+2:p_abas1(p_lx(ij))+p_nrowtot)
	! write(17,'(20f11.1)')p_aopt(p_abas1(p_lx(ij))+2:p_abas1(p_lx(ij))+p_nrowtot)- &
	! p_akey(p_abas1(p_lx(ij))+2:p_abas1(p_lx(ij))+p_nrowtot)
	! write(17,*)' '
	! enddo !ij=1,p_lx0  12506
	! write(17,*)p_x(p_lx(1:p_lx0)+p_nrowz)
 
 
	do i=1,p_lx0
 
		wei=p_x(p_lx(i)+p_nrowz)
		!	write(6,*)'weiny ',wei
		iuni=p_lunit(p_lx(i))
		!write(17,*)'i,iuni',i,iuni,wei,'key',p_keys(iuni),'isch',p_isch(p_lx(i))
		!		write(6,*)'ilx',i,'wei ',wei,'iuni',iuni
		if(.not.p_isunit)then
			call j_getobsiv(iuni,j_divmatup,j_divkeepup,j_divobsup) !,p_ivtransc,j_ivunit)
			if(p_ivtrans.gt.0)call dotrans(p_ivtrans,1)
			!		write(17,*)'iuni,wei',iuni,wei
			!		write(17,*)j_v(j_o(j_divkeepup)%i2)
		endif !if(.not.p_isunit)  12642
 
 
		!	iba2=0
		if(p_nshow.gt.0)call dotrans(p_ivshowtrans,1)
		!	if(p_nshow.gt.0)write(17,*)'*show*',p_nshow
		do koo=1,2
 
			if(koo.eq.1)then
				iobs=p_ibaunit(iuni)+p_keys(iuni) !update for keyshced and explicit basic
				call j_getobsiv(iobs,j_divmat,j_divkeep,0) !,p_ivtransx,0)  !getsolx
				!write(17,'(a,2i5,9f10.1)') 'koo,iobs00',koo,iobs, j_v(p_xvararea)
				if(p_isarea)wei=wei*j_v(p_ivarea)
				!		write(17,'(2i5,10f8.2)')iuni,p_keys(iuni),j_v(p_xvararea)
				!				iobs=p_ibaunit(iuni)+p_keys(iuni)
				!		call j_getobsiv(iobs,j_divmat,j_divkeep,0)! ,p_ivtransx,0)  !getsolx
 
			else
 
				iobs=p_ibaunit(iuni)+p_isch(p_lx(i))
				call j_getobsiv(iobs,j_divmat,j_divkeep,0) !,p_ivtransx,0)  !getsolx
 
			endif !if(koo.eq.1)  12655
			if(p_isarea)then
 
 
				j_v(p_xvararea)=j_v(p_ivarea)*j_v(p_xvararea)
 
			endif !if(p_isarea)  12670
 
			!write(17,'(a,2i5,9f10.1)') 'koo,iobs',koo,iobs, j_v(p_xvararea)
			wei=-wei  !first key
			if(p_isdomain)then
 
				!		p_idomba=(iuni-1)*p_ndomv
				iba=0
				do id=1,p_ndom
					!		icurint=(id-1)/32+1;icurbit=p_id-(icurint-1)*32-1
					!		if(.not.btest(p_domainbits(p_idomba+icurint),icurbit))then
					!write(17,*)'domain',id,'site',j_v(j_o(j_divkeepup)%i2(2)),indomain(id,iuni),'iba',iba
 
					if(indomain(id,iuni))then
 
 
						p_sumx(iba+1:iba+p_nxvararea)=p_sumx(iba+1:iba+p_nxvararea)+wei* j_v(p_xvararea)
						!		write(17,'(a,2i5,9f10.1)') 'koo,xiobs',koo,iobs, j_v(p_xvararea)
					end if !if(indomain(id,iuni))  12688
 
					iba=iba+p_nxvararea  !+p_noutsubtrans
				end do !id=1,p_ndom  12683
			else !if(j_ndom.gt.0)then
 
				!	do jx=1,p_nxvararea
				!		p_sumx(jx)=p_sumx(jx)+wei* j_v(p_))
				!	end do !jx=1,p_nxvartot  13413
 
				p_sumx(1:p_nxvararea)=p_sumx(1:p_nxvararea)+wei*j_v(p_xvararea)
 
				!	write(6,*)'wei ',wei,p_sumx(3)
				! do jx=1,p_noutsubtrans
				! !	p_sumx(p_nxvartot+p_ncvar+jx)=p_sumx(p_nxvartot+p_ncvar+jx) &
				! p_sumx(p_nxvartot+jx)=p_sumx(p_nxvartot+jx) &
				! + wei*j_v(j_o(p_ivoutsubtrans)%i2(jx))
				! enddo !jx=1,p_noutsubtrans  14446
 
			end if !if(p_isdomain)  12679
			!write(17,*)'isdomain',p_isdomain,p_ndom
 
			if(p_nshow.gt.0)then
				if(.not.p_isdomain)iba=p_nxvararea !otherwise comes from above
				!write(17,*)'showbashere ',iba
 
				do id=1,p_nshow
					!		write(6,*)'idny ',id,j_o(p_ivshowmatrix)%d(id)
					if(j_o(p_ivshowmatrix)%d(id).ne.j_0)then
						!write(17,*)'showbasinlo ',iba
						!	write(6,*)'showyes ',iuni,'wei ',wei,'iba ',iba
						!	j_o(p_ivshowunits)%i2(id)=j_o(p_ivshowunits)%i2(id)+1
						p_sumx(iba+1:iba+p_nxvararea)=p_sumx(iba+1:iba+p_nxvararea)+wei* j_v(p_xvararea)
 
					endif !if(j_o(p_ivshowmatrix)%d(id).ne.j_0)  12721
					iba=iba+p_nxvararea
 
				enddo !id=1,p_nshow  12719
			endif !if(p_nshow.gt.0)  12715
 
 
		end do !koo=1,2  12653
 
 
	end do !i=1,p_lx0  12635
 
	!write(17,'(9f10.1)')p_sumx
	!write(17,*)'yht'
	!write(17,'(9f10.1)')p_sumx(10:18)+p_sumx(19:27)
	!	write(6,*)'weinnsnns ',wei,p_sumx(3)
	return
end subroutine !subroutine getsolx()
subroutine testweight()
	use jmod, only: p_lx0
	use jmod, only: p_x
	use jmod, only: p_nrowz
	use jmod, only: p_lx
	use jmod, only: j_err
 
	do ir=1,p_lx0
		if(p_x(p_nrowz+p_lx(ir)).lt.-0.00001d0.or.p_x(p_nrowz+p_lx(ir)).gt.1.000001d0)then
			write(6,*)'illegal weight ',p_x(p_nrowz+p_lx(ir))
			j_err=.true.
 
		endif !if(p_x(p_nrowz+p_lx(ir)).lt.-0.00001d0.or.p_x(p_nrowz+p_lx  12748
 
 
	enddo !ir=1,p_lx0  12747
	if(j_err)then
		write(6,'(a,(15i7))')'p_lx ',p_lx(1:p_lx0)
		write(6,'(a,(15f7.4))')'weights ',p_x(p_nrowz+p_lx(1:p_lx0))
		return
	endif !if(j_err)  12756
 
 
 
 
end subroutine


subroutine aftpivot() !p_x,p_vc
	use jmod, only: p_rhsw
	use jmod, only: p_x
	use jmod, only: p_xpresent
	use jmod, only: j_err
	use jmod, only: p_newc
	use jmod, only: j_dapu
	use jmod, only: p_tmax
	use jmod, only: p_post
	use jmod, only: p_feasible
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_objr
	use jmod, only: p_vc
	use jmod, only: p_objfold
	use jmod, only: p_objf
	use jmod, only: p_ls
	use jmod, only: p_nrow
	use jmod, only: p_xps
	use jmod, only: p_pivot
	call leftvec(p_rhsw,p_x)
	if(p_xpresent)then
		call testweight()
		if(j_err)return
	endif !if(p_xpresent)  12770
	!	if(p_isn16)call rint('aft leftvecpx',p_n16)
	if(p_newc.gt.0)then
		j_dapu=p_tmax
		if(.not.p_post)j_dapu=-j_dapu
		!	if(abs(p_x(p_newc)-j_dapu).gt.0.001)then
		!write(p_n16,*)'aftpivot,tmax,x',p_tmax,p_x(p_newc),'oldnew',p_oldc,p_newc,'post',p_post
		!		read(5,*)jmdjdj
		!	endif !if(abs(p_x(p_newc)-j_dapu).gt.0.001)  12552
 
	endif !if(p_newc.gt.0)  12775
	if(p_feasible)then
		if(p_isn16)write(p_n16,*)'intogetvc in aftpivoty'
		!	write(21,*)'afpiv'
		call getvc(p_objr)
		if(p_isn16)write(p_n16,'(a,(15f7.3))')'vc*aftpivot ',p_vc
 
		p_objfold=p_objf-1.d-7
 
		!		j_dapu=p_objf
		p_objf=dot_product(p_x(p_ls(1:p_nrow)),p_objr(p_ls(1:p_nrow)))
		if(p_xpresent)p_objf=p_objf+p_xps(0)
 
 
 
	else
		!	if(.not.p_feasible)then
		if(p_isn16)	write(p_n16,*)'calling isfeas in aftpivot, obj bef',p_objf
		call isfeasible()  !updates vc
 
 
	endif !if(p_feasible)  12784
 
 
	if(p_isn16)write(p_n16,*)'objf in aftpivot',p_objf
 
	if(p_objf.lt.p_objfold)then
		write(6,*)'*getting worse obj< oldobjf',p_objf,p_objfold,p_pivot
		!	call testxps('getwor')
		if(p_isn16)write(16,*)'*obj< oldobjf',p_objf,p_objfold,'pivot',p_pivot
		call rint('getworse',p_n16)
		if(p_objfold-p_objf.lt.0.01)then
			write(6,*)'worsening is small, lets continue'
		else
			j_err=.true.;return
		endif !if(p_objfold-p_objf.lt.0.01)  12814
 
	endif !if(p_objf.lt.p_objfold)  12809
 
	!	if(.not.p_xpresent)return
 
	!!!write(16,*)'***aftpivot rhsw',p_rhsw
	!!!write(16,*)'aftpivot vc',p_vc
	!!!write(16,*)'aftpivot lr0',p_lr0 ,'lr',p_lr(1:p_lr0)
	!!!write(16,*)'aftpivot lz0',p_lz0 ,'lz',p_lz(1:p_lz0)
	!!!write(16,*)'aftpivot ls',p_ls(1:p_nrow)
	!!!write(16,*)'aftpivot objr',p_objr(p_ls(1:p_nrow))
	!!!write(16,*)'aftpivot x/ls',p_x(p_ls(1:p_nrow))
	!!!write(16,*)'aftpivot lx',p_lx(1:p_lx0)
	!!!write(16,*)'aftpivot xps ',p_xps
	!call testx()
 
end subroutine




subroutine isfeasible()
	use jmod, only: p_objfold
	use jmod, only: p_objf
	use jmod, only: p_nnf
	use jmod, only: j_0
	use jmod, only: p_objr
	use jmod, only: j_yes
	use jmod, only: p_lr0
	use jmod, only: p_lr
	use jmod, only: p_x
	use jmod, only: p_tiny78
	use jmod, only: p_ebou
	use jmod, only: p_lnf
	use jmod, only: j_onen
	use jmod, only: j_one
	use jmod, only: p_lbou
	use jmod, only: j_dapu3
	use jmod, only: p_ubou
	use jmod, only: p_feasible
	use jmod, only: p_pivot
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_isz
	use jmod, only: p_nrow
	use jmod, only: p_nz
	use jmod, only: p_objrz
	use jmod, only: p_xpresent
	use jmod, only: p_ix
	use jmod, only: p_lx0
	use jmod, only: p_lx
	use jmod, only: p_nrowz
	use jmod, only: p_aopt
	use jmod, only: p_abas1
	use jmod, only: p_akey
	use jmod, only: p_ls
	use jmod, only: p_xps
	use jmod, only: p_xps00
	use jmod, only: p_vc
	use jmod, only: p_tmax
	!checks feasiblity and updates shadow prices and omputes p_x and obj
	!return objr and p_nnf
 
	implicit none
	integer i,irow,irowinf,j
	integer iobskey,iobsopt,lun,ibamatx,lxi
 
	!call prin('befisfeasible')
	!!!write(16,*)' '
	!!!write(16,*)'*isfeasible'
	!!!write(16,*)'objher in isfeasible obj ',p_objf,'lr0',p_lr0
 
	p_objfold=p_objf-1.d-7
 
	! write(6,*)'startinf ,p_objf ',p_objf,'p_nnf ',p_nnf,p_lower(1:min(7,p_nrow)),'p_maxo',p_maxo,'cur',p_rhscur(1:min(7,p_nrow))
	! write(6,*)'x',p_x(1:8)
	! !!!write(16,*)'rhsw',p_rhsw(1:min(8,p_nrow))
	! write(6,*)'xps',p_xps(1:min(8,p_nrow))
	! write(6,*)'rhscur',p_rhscur(1:min(8,p_nrow))
	! !!!write(16,*)'rhscw',p_rhsw(1:min(8,p_nrow))
 
	!	p_wasfeasible=p_feasible
	!	if(.not.p_feasible)then
	!		p_nnfold=p_nnf
	!if(p_isn16)call rint('*rint befisfeas',p_n16)
	p_nnf=0
	!		p_feasrow=.true.
	!		write(6,*)'<48848 p_nnf',p_nnf,p_lr0,p_x(2),'*',p_lr(1:p_lr0)
	!	p_objr(p_nrowz+1:)=p_zero
	p_objfold=p_objf-1.d-9
	p_objf=j_0
	p_objr=j_0
	!	p_tolecur=p_zero
	! write(6,*)'isfeasible p_x'
	! write(6,'(10g10.2)')p_x
	! write(6,*)'p_rhscur'
	! write(6,'(10g10.2)')p_rhscur
 
	!	if(.true.)then !.not.j_feasible.or.p_unit.eq.1)then !
	j_yes=.false.   ! is rhsw updated
	!!!write(16,*)'infeasible plr0 ny',p_lr0
	do i=1,p_lr0 !p_lr0 = number of basic residuals
		!rhscur =rhs or rhs2
		irow=p_lr(i)
		!!!write(16,*)' '
		!!!write(16,*)'irow,p_xirow ***',irow,p_x(irow),'ebou,bbou,lbou,ubou',&
		!	p_ebou(irow),p_bbou(irow),p_lbou(irow),p_ubou(irow)
 
		! a'x + resid=rhscur  => a'x = rhscur-resid
		! the problem is infeasible if
		!a'x= rhscur-resid <rhs1    <=>resid>rhscur-rhs1
		! in that case resid should be decreased (and row increased)
		! if rhscur=rhs2 then it can be cahnged first that rhscur=rhs1
 
		! the problem is also infeasible if
		! or a'x =rhscur-resid>rhs2 <=>resid<rhscur-rhs2
		! i.e. resis should be increased (and row decreased)
		! if rhscur=rhs1 then it can be cahnged first that rhscur=rhs2
		!should build how to treat huge
		!	write(6,*)'<355353 p_nnf,irow',p_nnf,irow,p_x(irow),p_tole(irow),p_rhscur(irow),&
		!					p_rhs2(irow),p_ubou(irow),p_lbou(irow),p_lower(irow),p_rhs(irow)
 
		! p_rhsw(i)=p_rhscur(i)-p_xps(i)
 
		! xrow+p_x(irow)=rhsw   = p_rhscur(irow)-p_xps(irow)
		! is xrow>rhs2   !if   rhscur=rhs2  xrow>
		!efective row = rhsw+xps= rhscur
		! x=-1700    xps=1700  rhsw= 0-1700  rhscur=0
		!	row=p_rhsw(irow)-p_x(irow)
		! for equality constraints all residuals indicate infeability
		if(abs(p_x(irow)).lt.p_tiny78)cycle
		if(p_ebou(irow))then
 
			p_nnf=p_nnf+1
			p_lnf(p_nnf)=irow
			!	if(p_nnf.eq.1)p_objr=j_0
			!!!write(16,*)'irowx ebou',irow,p_x(irow),' toinf',abs(p_x(irow))
			if(p_x(irow).gt.j_0)then  !small is beatiful pvalue is deviation
				p_objr(irow)=j_onen
				p_objf=p_objf-p_x(irow)
			else
				p_objr(irow)=j_one
				p_objf=p_objf+p_x(irow)
			endif !if(p_x(irow).gt.j_0)  12918
		else
 
			!	j_dapu=p_rhscur(irow)-p_x(irow)  !j_dapu is rows
			! row+x=rhscur   !row=rhscur-x
			!	call rint(16)
			if(p_lbou(irow))then  ! is  row< rhscur   rhscur-x  <rhscur == -x<0 == x>0
				if(p_x(irow).gt.j_0)then
					p_nnf=p_nnf+1
					p_lnf(p_nnf)=irow
					p_objr(irow)=j_onen
					p_objf=p_objf-p_x(irow)
					j_dapu3=-p_x(irow)
 
				endif !if(p_x(irow).gt.j_0)  12931
 
			endif !if(p_lbou(irow))  12930
			j_dapu3=j_0
			if(p_ubou(irow))then  !  row>rhs2   rhscur-x > rhscur  == -x>0  x<0
				if(p_x(irow).lt.j_0)then
					p_nnf=p_nnf+1
					p_lnf(p_nnf)=irow
					p_objr(irow)=j_one
					p_objf=p_objf+p_x(irow)
					j_dapu3=p_x(irow)
				end if !if(p_x(irow).lt.j_0)  12943
 
			endif !if(p_ubou(irow))  12942
			!!!write(16,*)'irowdapu',irow,'lbub',p_lbou(irow),p_ubou(irow),'objnow',p_objf,'added',j_dapu3
		endif !if(p_ebou(irow))  12912
	enddo !i=1,p_lr0  12882
 
 
 
	!!!write(16,*)'*isfeasend obj ',p_objf
	!!!write(16,*)' '
	!
	! row= effect x and z variables in current solution
	! it holds row + res = rhsw
	! if lower      rhsw= rhs -xps   i.e. row+res =rhs- xps   i.e. row+xps= rhs-res
	! if not lower                        row+res =rhs2-xps        row+xps=rhs2-res
 
	!	It should be   rhs <=  xps+row  <=   rhs2
	! if lower xps+row >=rhs   == rhs-res>= rhs  == res <= 0
	! if.not.lower xps+row >=rhs   == rhs2-res>= rhs  == res <= rhs2-rhs
 
	!	It should be   rhs <=  xps+row  <=   rhs2
	! if lower xps+row <=rhs2   == rhs-res<= rhs2  == res >= rhs-rhs2
	! if.not.lower xps+row <= rhs2   == rhs2-res<= rhs2  == res >= rhs2-rhs
 
	! if rhs=rhs2 any value of res means infeasiblity, positive res needs to be
	! decreased and negative increased
 
	! if rhs2>rhs
	! if xps+row < rhs  then
	! if ,not.lower rhs must be made first active
	! in both lower cases  res should be decreased
 
	! if xps+row> rhs2 then
	! if lower rhs2 must first be made active
	! in both lower cases  res should be increased
	!	if(p_isn16)call rint('*mid feas ',p_n16)
 
	p_feasible=p_nnf.eq.0
 
 
	!	write(6,*)'p_nnf',p_nnf,p_feasible,'p_lr0 ',p_lr0
	!	!!!write(16,*)'p_nnf',p_nnf,p_feasible,'p_lr0 ',p_lr0
	!!!write(16,*)'inisfeasaft p_nnf',p_nnf,' obj ',p_objf,'pisz',p_isz
	if(p_feasible)then
		write(6,*)'*FEASIBLe pivots ',p_pivot
		if(p_isn16)write(p_n16,*)'**************FEASIBLE*'
		! !	return
		! if(j_yes)call leftvec(p_rhsw,p_x)
 
		! !!!write(16,*)'here obj ',p_objf,'nnf',p_nnf
		! else
		! ! iobs= p_ibaunit(p_unit)+p_keys(p_unit)
		! iobsopt=p_ibaunit(p_unit)+p_iopt
		! ibaxmat=ibamatx(iobs) !,1)
		! ibaxmat2=ibamatx(iobsopt) !,2)
		! p_lunit(p_newd)=p_unit
		! p_isch(p_newd)=p_iopt
		p_objr=j_0 !z sit
		if(p_isz)p_objr(p_nrow+1:p_nrow+p_nz)=p_objrz
		!	write(p_n16,'(15f7.3)')'p_objrz',p_objrz
		if(p_xpresent)then
			if(p_ix(0).ne.0)then
				do i=1,p_lx0
					lxi=p_lx(i)
					! lun=p_lunit(lxi)
					! iobskey=p_ibaunit(lun)+p_keys(lun)
 
					! z-objr
					! iobsopt=p_ibaunit(lun)+p_isch(lxi)
					!			p_objr(lxi)=p_xmat(p_ix(0)+ibamatx(iobsopt)) -p_xmat(p_ix(0)+ibamatx(iobskey))
					p_objr(p_nrowz+lxi)=p_aopt(p_abas1(lxi)+1)-p_akey(p_abas1(lxi)+1)
 
				enddo !i=1,p_lx0  13012
			endif !if(p_ix(0).ne.0)  13011
			!		p_objr(newc)=p_xmat(p_ix(0)+ibaxmat2)-p_xmat(p_ix(0)+ibaxmat) !v(ix(0))
		endif !if(p_xpresent)  13010
 
		p_objf=dot_product(p_x(p_ls(1:p_nrow)),p_objr(p_ls(1:p_nrow)))
		if(p_xpresent)then
			if(p_ix(0).ne.0)then
				p_objf=p_objf+p_xps(0)
				if(p_objf.gt.1.d6)then
					p_xps00=p_objf  !p_xps(0)
					p_xps(0)=p_xps(0)-p_xps00
					p_objf=j_0
					write(6,*)'because the objective is large, the current value' ,p_xps00, ' is used as reference'
					write(6,*)'change% is computed with respect to the true value'
				endif !if(p_objf.gt.1.d6)  13031
			endif !if(p_ix(0).ne.0)  13029
 
		endif !if(p_xpresent)  13028
 
 
		p_objfold=p_objf-1.d-4
		!		call getvc(p_objr)
		if(p_isn16)write(p_n16,'(a,(15f7.3))')'vc*in feasible ',p_vc
 
		!	if(p_isn16)call rint('getting feas',p_n16)
		!	j_dapu=j_0
		! do j=1,p_nrow
 
		! j_dapu=j_dapu+p_objr(p_ls(j))*p_x(p_ls(j))
		! write(p_n16,*)j,p_objr(p_ls(j)),p_x(p_ls(j)),j_dapu
 
		! enddo !j=1,p_nrow  12977
 
 
		!	p_objfold=j_ninf
		!!!write(16,*)'FEASIBLE'
		!	!!!write(16,*)'objr',
		!		write(6,*)'FEASIBLE, xps(0) ',p_xps(0)
 
		if(p_isn16)call rint('*tasa*',p_n16)
 
		!	p_objf=p_xps(0)+dot_product(p_x(p_ls(1:p_nrow)),p_objr(p_ls(1:p_nrow)))
		!	if(p_ix(0).ne.0)p_vx(0)=j_one in senter
		!	!!!write(16,*)'OBJF',p_objf
	else
		if(p_objf.lt.p_objfold)then
			!!!write(16,*)'obj,prev in isfeasible',p_objf,p_objfold
			call rint('getting worse here',p_n16)
			write(6,*)'obj,old B',p_objf,p_objfold,p_objf-p_objfold,p_tmax,(p_objf-p_objfold)/p_tmax,'pivot',p_pivot
			if(p_feasible)read(5,*)j
		endif !if(p_objf.lt.p_objfold)  13068
	endif !if(p_feasible)  12993
 
 
	!endif !if(.not.p_feasible)   7781
 
 
	!		if(.not.p_wasfeasible)p_objfv=j_ninf
	!	p_objf=p_xps(0)+dot_product(p_x(p_ls(1:p_nrow)),p_objr(p_ls(1:p_nrow)))
	!	if(p_ix(0).ne.0)p_vx(0)=j_one
	!	!!!write(16,*)'OBJF',p_objf
 
	!	p_priced=.true.	write(16,'(a,25f10.4)')'in feasible vcny',p_vc
 
	!!!write(16,*)'in isfeasible p_objr'
	!	write(21,*)'isfeas'
	call getvc(p_objr)
 
	!	if(p_isn16)call rint('**aft feas',p_n16)
	!!!write(16,*)'from is feasible,',p_nnf,'vc',p_vc
	!p_xdone=.true.
	!	if(p_feasible)p_objf=p_xps(0)+dot_product(p_x(p_ls(1:p_nrow)),p_objr(p_ls(1:p_nrow)))
	!if(.not.p_feasible)!!!write(16,*)'obj in isferasible',p_objf
	!call prin('aftisfeasible')
	return
 
end subroutine isfeasible !subroutine isfeasible()



subroutine tulostele2(iob) !prints and checkes if finished
	use jmod, only: p_exitkier
	use jmod, only: p_feasible
	use jmod, only: p_kier
	use jmod, only: p_as
	use jmod, only: j_dnobs
	use jmod, only: p_fast
	use jmod, only: p_fastreject
	use jmod, only: p_coefmax
	use jmod, only: p_objf
	use jmod, only: p_objfprev
	use jmod, only: p_xps00
	use jmod, only: p_nimp2
	use jmod, only: p_asv
	use jmod, only: p_nimp
	use jmod, only: p_time0
	use jmod, only: p_time00
	use jmod, only: p_fastmakes
	use jmod, only: p_fastpros
	use jmod, only: p_small
	use jmod, only: p_pivot
	use jmod, only: p_lr0
	use jmod, only: p_lz0
	use jmod, only: p_lx0
	use jmod, only: p_nnf
	use jmod, only: j_o
	use jmod, only: p_loco
	use jmod, only: p_ivvaluedif
	use jmod, only: p_nunits
	use jmod, only: p_iopt
	use jmod, only: j_dapu
	use jmod, only: j_0
	use jmod, only: j_dapu2
	use jmod, only: p_ivobjdif
	use jmod, only: p_isstop
	use jmod, only: p_isfastp
	use jmod, only: j_v
	use jmod, only: j_ivimp
	use jmod, only: j_ivchangep
	use jmod, only: j_ivround
	use jmod, only: j_ivactivep
	use jmod, only: j_codevalue
	use jmod, only: p_stoplink
	use jmod, only: p_pivot9
	use jmod, only: p_fastusedsame
	use jmod, only: p_fastusesame
	use jmod, only: p_fastnow
	use jmod, only: p_fastmake
	use jmod, only: p_value
	use jmod, only: p_zero
	use jmod, only: p_lower
	use jmod, only: p_lr
	use jmod, only: p_x
	use jmod, only: p_tole
	integer i
	save asv
	!	logical::exitkier
	p_exitkier=.false.
	!goto785=.false.
 
	if(p_feasible)then
		!	write(23,*)'kier fastusesame,fastusedsame',p_kier,p_fastusesame,p_fastusedsame
		if(p_kier.le.10.or.int(p_kier/10)*10.eq.p_kier)then
			p_as=100.
			if(p_fast)p_as=100.*(1.-1.*count(p_fastreject)/j_dnobs)
 
			!	pros=j_0
			!		call cpu_time(time)
			!		write(6,*)'objprev',j_objfprev
			! if(p_coefmax*p_objfprev.le.j_0)then
			! !write(6,*)'tassa3'
			! write(6,'(i5,i8,g19.12,7x,f8.2,5i5,f6.2,f7.2,i5)')&
			! p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0,p_lf0,p_nnf,(time-p_time0)/60., &
			! (time-p_time00)/60.
			! p_time0=time
			! ncyc=0
			! kier0=p_kier
			! p_objfprev=p_objf
			! else !if(p_coefmax*j_objfprev.le.j_0)then
			pros=abs(100.*p_coefmax*(p_objf-p_objfprev)/(p_objfprev+p_xps00))
			if(p_kier.le.10)pros=10.*pros
			change=p_objf-p_objfprev
			p_nimp2=-1
			if(p_as.ne.p_asv.and.p_asv.ne.100.)p_nimp2=p_nimp
 
			p_asv=p_as
			call cpu_time(time)
			!	write(6,*)'time,0,00',time,P_time0,p_time00
			iminc=(time-p_time0)/60.
			imint=(time-p_time00)/60.
			secd=time-p_time0-iminc*60.
			sect=time-p_time00-imint*60.
			p_time0=time
			!	j_buf(1:12)=' ';j_buf(2:11)=j_chr10(j_objf-j_objfprev)
 
			!write(6,*)'tassa4'
			fastpros=0
			if(p_fastmakes.gt.0)fastpros=p_fastpros
			if(p_objfprev.ne.p_small)then
				write(6,'(i5,i8,g19.12,f8.5,f7.2,4i5,i3,a,f5.2,i5,a,f5.2,i5,f6.2)')&
					p_kier,p_pivot,p_coefmax*p_objf,pros,p_as,p_lr0,p_lz0,p_lx0, &
					p_nnf,iminc,':',secd,imint,':',sect,p_nimp2,fastpros
 
			else
				write(6,'(i5,i8,g19.12,8x,f7.2,4i5,i3,a,f5.2,i5,a,f5.2,i5,f6.2)')&
					p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0, &
					p_nnf,iminc,':',secd,imint,':',sect,p_nimp2,fastpros
 
			endif !if(p_objfprev.ne.p_small)  13148
 
			!	write(6,*)'epsilon ',fraction(p_coefmax*p_objf),exponent(p_coefmax*p_objf),epsilon(p_coefmax*p_objf)
 
			p_objfprev=p_objf
			p_loco=maxloc(j_o(p_ivvaluedif)%d(1:p_nunits)) !output must be array
			p_iopt=p_loco(1)
			j_dapu=j_0
			j_dapu2=j_0
			!	ndi=0
			do i=1,p_nunits
				if(j_o(p_ivvaluedif)%d(i).gt.j_0)then
					j_dapu=j_dapu+j_o(p_ivvaluedif)%d(i)
					j_dapu2=j_dapu2+j_o(p_ivobjdif)%d(i)
					!	ndi=ndi+1
				endif !if(j_o(p_ivvaluedif)%d(i).gt.j_0)  13169
			enddo !i=1,p_nunits  13168
 
			ncyc=0
 
			if(p_isstop.or.p_isfastp)then
				j_v(j_ivimp)=p_nimp
				j_v(j_ivchangep)=pros
				j_v(j_ivround)=p_kier
				j_v(j_ivactivep)=p_as
				! call dotrans(iob,p_iostop)
				! if(j_v(j_divobstop).ne.0)then
			endif !if(p_isstop.or.p_isfastp)  13178
 
			if(p_isstop)then
				if(j_codevalue(iob,p_stoplink).ne.j_0)then
					write(6,*)'pros,p_kier',pros,kier
					write(6,*)'iteration stops due to stop->'
					p_exitkier=.true.
					return
 
				endif !if(j_codevalue(iob,p_stoplink).ne.j_0)  13188
			else !if(p_isstop.and.p_kier.gt.10)then
				if(pros.lt.0.01.and.p_kier.ge.10)then
					write(6,*)'iteration stops due to assumed  stop->(Change%.lt.0.01.and.Round.ge.10)'
					j_v(j_ivimp)=p_nimp
					j_v(j_ivchangep)=pros
					j_v(j_ivround)=p_kier
					p_exitkier=.true.
					return
				endif !if(pros.lt.0.01.and.p_kier.ge.10)  13196
 
			endif !if(p_isstop)  13187
 
			p_pivot9=p_pivot !last pivot printed
 
		endif !if(p_kier.le.10.or.int(p_kier/10)*10.eq.p_kier)  13112
		if(p_fast.and.p_feasible.and.p_kier.ge.20)then
			re=count(p_fastreject)
			!if(pp)!!!write(16,*)'rejpros',re/j_lopp,p_fastusesame,p_fastusedsameFround,non
			p_fastusedsame=p_fastusedsame+10
			if(p_fastusedsame.gt.p_fastusesame)then
				!		write(21,*)'fastusedsame,kier,unit',p_kier,p_unit
				call startfast()
				! p_fastnow=.false.
				! p_fastusedsame=0
				! p_fastmake=.true.
				! if(p_isfastp)then
 
				! p_fastpros=j_codevalue(iob,p_fastplink)
				! j_v(j_ivfastp)=p_fastpros
				! p_fastpros2=p_fastpros/100.d0
				! !	write(6,*)'fastprosaft ',p_fastpros
 
				! endif !if(p_isfastp)  13118
				! p_nimp=0  !fastmake alkaa
				!	write(21,*)'fastused,fastpros,fastpros2,kier',p_fastpros,p_fastpros2,p_kier,p_vc,p_vcold
 
			else !if(p_fastusedsame.gt.p_fastusesame)then
 
				p_fastnow=.true.
				p_fastmake=.false.
 
 
			endif !if(p_fastusedsame.gt.p_fastusesame)  13214
 
		endif !if(p_fast.and.p_feasible.and.p_kier.ge.20)  13210
	else !if(j_feasible)then
		!!!!! compute the sum of infeasiblity after each round (is this necessary?)
		numn=0
		p_value=p_zero
		do i=1,p_lr0 !p_lr0 = number of basic residuals
			!rhscur =rhs or rhs2
			if(p_lower(p_lr(i)).and.p_x(p_lr(i)).gt.p_tole(p_lr(i))) then
				numn=numn+1
				p_value=p_value-p_x(p_lr(i))
			elseif(.not.p_lower(p_lr(i)).and.p_x(p_lr(i)).lt.-p_tole(p_lr(i))) then !if(j_lower(j_lr(i)).and.j_x(j_lr(i)).gt.j_tole(j_lr(i))) then
				numn=numn+1
				p_value=p_value+p_x(p_lr(i))
			endif !if(p_lower(p_lr(i)).and.p_x(p_lr(i)).gt.p_tole(p_lr(i)))  13246
		enddo !i=1,p_lr0  13244
		if(p_kier.le.10.or.int(p_kier/10)*10.eq.p_kier)then
			p_as=100.
			if(p_fast)p_as=100.*(1.-1.*count(p_fastreject)/j_dnobs) !laske muualla
			pros=j_0
			call cpu_time(time)
			iminc=(time-p_time0)/60.
			imint=(time-p_time00)/60.
			secd=time-p_time0-iminc*60.
			sect=time-p_time00-imint*60.
			p_time0=time
			write(6,'(i5,i8,g19.12,7x,f8.2,4i5,i3,a,f5.2,i5,a,f5.2)')&
				p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0,p_nnf, &
				iminc,':',secd,imint,':',sect
			p_loco=maxloc(j_o(p_ivvaluedif)%d(1:p_nunits)) !output must be array
			p_iopt=p_loco(1)
			!		write(6,*)'optdif',j_o(p_ivvaluedif)%d(p_iopt)
 
		endif !if(p_kier.le.10.or.int(p_kier/10)*10.eq.p_kier)  13254
	endif !if(p_feasible)  13110
 
 
end subroutine tulostele2 !subroutine tulostele2()
subroutine getvx()
	use jmod, only: j_0
	use jmod, only: p_vx
	use jmod, only: p_feasible
	use jmod, only: j_1
	use jmod, only: p_ix
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: p_vc
	p_vx=j_0
 
	if(p_feasible)then
		if(p_ix(0).ne.0)p_vx(p_ix(0))=j_1
	endif !if(p_feasible)  13279
	do j=1,p_nrowcurx
		i=p_rowcurx(j)
		if(i.gt.0)p_vx(p_ix(i))=p_vx(p_ix(i))-p_vc(i)
 
	enddo !j=1,p_nrowcurx  13282
 
 
 
end subroutine


subroutine senter(dorz)
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_fast
	use jmod, only: p_post
	use jmod, only: p_isdomain
	use jmod, only: p_unit
	use jmod, only: p_enter
	use jmod, only: p_valueopt
	use jmod, only: p_small
	use jmod, only: p_iopt
	use jmod, only: p_vx
	use jmod, only: p_ibaunit
	use jmod, only: p_keys
	use jmod, only: p_p
	use jmod, only: p_fastvaluemin
	use jmod, only: p_basreject
	use jmod, only: p_ns
	use jmod, only: p_lx0
	use jmod, only: p_lunit
	use jmod, only: p_lx
	use jmod, only: p_isch
	use jmod, only: j_inf
	use jmod, only: p_fastnow
	use jmod, only: p_fastvalues
	use jmod, only: p_ntemp0
	use jmod, only: j_ninf
	use jmod, only: p_fpresent
	use jmod, only: p_nfact
	use jmod, only: p_kntot
	use jmod, only: p_fastreject
	use jmod, only: p_svalue
	use jmod, only: p_filre
	use jmod, only: p_rejects
	use jmod, only: p_xpresent
	use jmod, only: p_xmatrow
	use jmod, only: p_xmat
	use jmod, only: j_dapu2
	use jmod, only: j_0
	use jmod, only: p_logval
	use jmod, only: p_optfact0
	use jmod, only: p_nlog
	use jmod, only: j_dmat
	use jmod, only: p_keeplog
	use jmod, only: p_logtable
	use jmod, only: p_logtablei
	use jmod, only: j_o
	use jmod, only: p_ivneig
	use jmod, only: p_neigbas
	use jmod, only: p_ibafact
	use jmod, only: p_factw
	use jmod, only: p_logfactterm
	use jmod, only: p_rowofterm
	use jmod, only: j_dapu3
	use jmod, only: p_vc
	use jmod, only: p_optfact
	use jmod, only: p_optfactkey
	use jmod, only: p_valuek
	use jmod, only: p_xmatkeyrow
	use jmod, only: p_fastmake
	use jmod, only: p_ivvaluedif
	use jmod, only: p_nimp
	use jmod, only: p_fastcut
	use jmod, only: p_fastpros2
	use jmod, only: p_activeunit
	use jmod, only: p_faststart
	use jmod, only: p_tolecur
	use jmod, only: p_objfz
	use jmod, only: p_objf
	use jmod, only: p_pivotrz
	use jmod, only: p_akey0
	use jmod, only: p_aopt0
	use jmod, only: p_anew0
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: p_ix
	use jmod, only: p_logvolkey
	use jmod, only: p_logvol
	use jmod, only: p_aopt
	use jmod, only: j_1
	use jmod, only: p_tmax
	use jmod, only: p_nrow
	use jmod, only: p_r
	use jmod, only: p_leavetype
	use jmod, only: p_oldx
	use jmod, only: p_xps
	use jmod, only: p_rhsw
	use jmod, only: p_rhscur
	use jmod, only: p_x
	use jmod, only: j_err
	use jmod, only: p_feasible
	use jmod, only: p_ls
	use jmod, only: p_objr
	use jmod, only: p_nnf
	double precision logvol,logvolkey,factopt
	logical dorz
	if(p_isn16)write(p_n16,*)' '
	if(p_isn16)write(p_n16,*)'*sub senter*****************''',p_fast
	!	p_fast=.false.
	!	p_p=p_fastmake
	! if(p_p)then
	! !		close(p_n16)
	! p_isn16=.true.
	! endif !if(p_p)  13291
 
	!	p_p=.true.
 
	p_post=.true.
 
	! if(p_feasible.and.p_ix(0).ne.0)then !ix(0).ne.0.and.feasible
	! p_vx(0)=j_1
	! else !if(p_ix0.ne.0)then
	! p_vx(0)=j_0
	! endif !if(p_feasible.and.p_ix(0).ne.0)  13055
 
	! do i=1,p_nrow
	! if(p_ix(i).ne.0)then
	! p_vx(i)=-p_vc(i)   ! formula 6.41, voitasiko kenties tästä merkinkäännöstä
	! ! luopua ja pelata suoraan vc:n avulla
	! endif !if(p_ix(i).ne.0)  13062
	!	enddo !i=1,p_nrow  13061
 
	! start computing shadow pricees of schedules <B333>
	!	write(6,*)'idom4',p_unit,p_idomba
	!if(p_isdomain)!!!write(16,*)'senter unit',p_unit
	if(p_isdomain)call jlpcurix(p_unit)  !(p_unit)
 
 
	p_enter=.false.
	!	!!!write(16,*)'vxhere',p_vx(0:p_nrow),'obj',p_objf
	p_valueopt=p_small  !j_ninf
	p_iopt=0        ! the optimal schedule
 
	call getvx()
	if(p_isn16)write(p_n16,*)'vx ',p_vx
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units must be accessed one by one
	! p_nrowcurx
	! p_rowcurx
	! p_idomba
 
	!determines for each row if the unit p_unit belonggs to the domain of the row
	!matrix  domainbits(number,mxunit) contains bits for telling if unit belongs to certain domain
	! returns nrowp
	! rowcurx  the x-variable of the
	!	p_vxpack=j_0
	!	p_ixpack=0
 
	!	nxbas=count(p_ixpack.ne.0)
	!	call pack()
	!	!!!write(16,*)'pack',p_vxpack(1:p_nxbas),p_ixpack(1:p_nxbas)
 
	ikey_ = p_ibaunit(p_unit)+p_keys(p_unit)
	if(p_p)write(16,*)'iba,key',p_ibaunit(p_unit),p_keys(p_unit)
	p_fastvaluemin=1.7d37
	!is(p_p.and.p_fpresent)write(p_n16,*)'**fact** <5586> unit,key',p_unit,p_keys(p_unit),p_xpresent2
	!	p_fastvaluemin=1.7d37
	!!!!kannassa olevat vaihtoehdot ohitetaan
	p_basreject(1:p_ns(p_unit))=.false.
	do k_=1,p_lx0
		if(p_lunit(p_lx(k_)).ne.p_unit) cycle
		i=p_isch(p_lx(k_))
		p_basreject(i)=.true. !if(p_isch(p_lx(k_)).ne.i) cycle
		if(p_p)write(16,*)'k,i',k,i,'fastnow'
		if(p_fast.and..not.p_fastnow)p_fastvalues(i)=j_inf
		!	p_basreject(cycle nschloop
	enddo !k_=1,p_lx0  13359
	ibaxmat=(p_ibaunit(p_unit)-1)*p_ntemp0
	!	!!!write(16,*)'nytp',p_kier
	p_valueopt=j_ninf
	!  -fopenmp
	if(p_fpresent)then
		ibanei=(p_unit-1)*p_nfact
		ibakeyf=(p_unit-1)*p_kntot
 
 
 
	endif !if(p_fpresent)  13371
 
 
 
	!$OMP PARALLEL DO
		nschloop:	do i=1,p_ns(p_unit)   !mainloop
		ibaxmat=ibaxmat+p_ntemp0
		if(p_basreject(i))then
			!		p_svalue(i)=j_ninf
			cycle
		endif !if(p_basreject(i))  13384
		!	!!!write(16,*)'nytpot',p_kier,p_unit,i
		iobs=p_ibaunit(p_unit)+i
		if(p_p)write(16,*)i,i.ne.p_keys(p_unit)
		if(p_fastnow)then
			if(p_fastreject(iobs).and.i.ne.p_keys(p_unit))then
				if(p_p)write(16,*)'cycle'
				p_svalue(i)=j_ninf
				cycle  !i.ne.j_keys(p_unit) added 20.8.2018 JL
			endif !if(p_fastreject(iobs).and.i.ne.p_keys(p_unit))  13392
		endif !if(p_fastnow)  13391
		if(p_filre)then
			if(p_rejects(iobs))then
 
				cycle
			endif !if(p_rejects(iobs))  13399
		endif !if(p_filre)  13398
 
		if (p_xpresent) then
 
			p_xmatrow=p_xmat(ibaxmat+1:ibaxmat+p_ntemp0)
			j_dapu2=dot_product(p_vx,p_xmatrow)
		else
			j_dapu2=j_0
		endif !if (p_xpresent)  13405
		if(p_p)write(16,*)'dapu2',j_dapu2
		if(p_fpresent)then
			p_logval=j_0
			p_optfact0=0
			do ilog=1,p_nlog
				!			write(6,*)'ilo,',ilog,j_dmat(ibadat+p_keeplog(ilog))
				logvol=j_dmat(ibadat+p_keeplog(ilog))
				if(logvol.le.j_0)cycle
				ifactopt=0
				factopt=j_ninf
				!		p_factval=j_0  !value for different factories
				ivtable=p_logtable(ilog)
				itab=p_logtablei(ilog)
 
				knn=j_o(ivtable)%i(10)
				!	p_xps0=j_0
				do k=1,knn
					ifact=j_o(p_ivneig(itab))%i2(p_neigbas(itab)+k)   !p_neig(ibanei+k)
					if(ifact.le.0)exit
					inde=p_ibafact(ilog)+p_factw(ifact)   !index of the logfact
 
					iterm=p_logfactterm(inde)
					if(iterm.le.0)exit
					!	write(6,*)'k,ifact,inde,iterm ',k,ifact,inde,iterm
					irow=p_rowofterm(iterm)
 
 
					j_dapu3=p_vc(irow)*logvol
 
 
					if(j_dapu3.gt.factopt)then
						ifactopt=ifact
						factopt=j_dapu3
					endif !if(j_dapu3.gt.factopt)  13442
 
				enddo !k=1,knn  13428
 
				p_optfact0(ilog)=ifactopt
				if(ifactopt.gt.0)p_logval(ilog)=factopt
				!		if(i.le.2.and.j.le.10)write(6,*)'ifactopt,p_logval(ilog)',ifactopt,p_logval(ilog)
 
			enddo !ilog=1,p_nlog  13416
			!	if(i.le.2)write(6,*)'p_logval',p_logval
			!	if(i.eq.5)read(5,*)ii
 
			j_dapu3=j_dapu2+sum(p_logval)  !sum
			if(j_dapu3.gt.p_valueopt)then
				write(6,*)'imp',i,j_dapu3,j_dapu2,p_keys(p_unit),p_valueopt
				p_valueopt=j_dapu3
				p_optfact=p_optfact0
				p_iopt=i
				ibadatopt=ibadat
				ibaxmatopt=ibaxmat
 
			endif !if(j_dapu3.gt.p_valueopt)  13458
			if(i.eq.p_keys(p_unit))then
				p_optfactkey=p_optfact
 
				ibaxmatkey=ibaxmat
				ibadatkey=ibadat
				p_valuek=j_dapu3
				!		write(6,*)'key',i,j_dapu3
			endif !if(i.eq.p_keys(p_unit))  13467
		else
			if(i.eq.p_keys(p_unit))then
				p_valuek=j_dapu2
				p_xmatkeyrow=p_xmatrow
				if(p_p)write(16,*)'valuek',p_valuek
				!	ibaxmatkey=ibaxmat
				!	if(p_unit.eq.4963)!!!write(16,*)'VALK',p_valuek,'vx',p_vxpack(1:p_nxbas),'x',p_xmat(p_ixpack(1:p_nxbas)+ibaxmat)
			elseif(j_dapu2.gt.p_valueopt)then
				p_valueopt=j_dapu2
				p_iopt=i
				ibaxmatopt=ibaxmat
				if(p_p)write(16,*)'iopt',p_iopt
				!	if(p_unit.eq.4963)!!!write(16,*)'VALopt',p_valueopt,'vx',p_vxpack(1:p_nxbas),'x',p_xmat(p_ixpack(1:p_nxbas)+ibaxmat)
 
			endif !if(i.eq.p_keys(p_unit))  13476
 
 
 
		endif !if(p_fpresent)  13413
		!		j_dapu2=dot_product(p_vxpack(1:p_nxbas),p_xmat(p_ixpack(1:p_nxbas)+ibaxmat) )   !!!!
 
		!	endif !if (p_xpresent)  10773
 
		if(p_fastmake)then
			p_fastvalues(i)=j_dapu2  !p_svalue(i)  !p_value
			!	if(p_p)write(16,*)'fatval',j_dapu2
			if(j_dapu2.lt.p_fastvaluemin)p_fastvaluemin=j_dapu2 !p_svalue(i) !p_value
		endif !if(p_fastmake)  13498
	enddo nschloop !hloop:	do i=1,p_ns(p_unit)  13382
	!write(16,'(25f7.1)')p_svalue
	!$OMP END PARALLEL DO
	!	if(p_fastmake.and.p_unit.eq.17383)write(18,*)'fastval',p_fastvalues
	!	!!!write(16,*)'p_unit,p_iopt,p_valueopt,p_valuek,p_keys',p_unit,p_iopt,p_valueopt,p_valuek,p_keys(p_unit)
	j_o(p_ivvaluedif)%d(p_unit)=p_valueopt-p_valuek
	!is(p_p)write(p_n16,*)'p_unitetc',p_unit,p_valuek,p_iopt,p_valueopt
 
	if(p_fastmake)then
		!	p_p=p_unit.eq.17383
		!	if(p_p)write(18,*)'p_iopt,p_ns(p_unit,p_keys(p_unit)',p_iopt,p_ns(p_unit),p_keys(p_unit)
 
		if(p_iopt.gt.0.and.p_fastreject(p_ibaunit(p_unit)+p_iopt))p_nimp=p_nimp+1
 
		!		p_fastcut=min(p_fastvaluemin+p_fastpros2*(p_valueopt-p_fastvaluemin),p_valuek)-p_tiny78
		p_fastcut=p_fastpros2*p_valuek !min(p_valueopt,p_fastpros2*p_valuek)
		p_fastreject(p_ibaunit(p_unit)+1:p_ibaunit(p_unit)+p_ns(p_unit))=.false.
		!	if(p_p)write(18,*)'fastcut',p_fastcut
		nac=0
		do i=1,p_ns(p_unit)
			!if(p_p)write(18,*)'i,fastval',i,p_fastvalues(i)
			if(p_fastvalues(i).lt.p_fastcut)then  !.and.i.ne.p_keys(p_unit))then
				p_fastreject(p_ibaunit(p_unit)+i)=.true.
			else
				nac=nac+1
			endif !if(p_fastvalues(i).lt.p_fastcut)  13524
			!	if(fastvalues(i).lt.fastcut)!!!write(16,*)'reject',p_unit,i
		enddo !i=1,p_ns(p_unit)  13522
		do i=1,p_lx0
			if(p_lunit(p_lx(i)).eq.p_unit)then
				if(p_fastreject(p_ibaunit(p_unit)+p_isch(p_lx(i))))then
					p_fastreject(p_ibaunit(p_unit)+p_isch(p_lx(i)))=.false.
					nac=nac+1
					!		write(6,*)'nofasty',p_unit,p_isch(p_lx(i))
				endif !if(p_fastreject(p_ibaunit(p_unit)+p_isch(p_lx(i))))  13533
			endif !if(p_lunit(p_lx(i)).eq.p_unit)  13532
 
		enddo !i=1,p_lx0  13531
		!	if(p_p)write(6,*)'nac',nac
 
		p_activeunit(p_unit)=nac.gt.1
	endif !if(p_fastmake)  13511
	!	write(17,*)p_unit,p_ns(p_unit),nac,p_fastcut,p_valuek
	!	call testxps('senter')
	if(p_fastmake)then
		if(p_unit.eq.p_faststart)then
			p_fastnow=.true.
			p_fastmake=.false.
			!	write(6,*)'unity',p_unit,'closefastmake'
		endif !if(p_unit.eq.p_faststart)  13548
	endif !if(p_fastmake)  13547
	if(p_valueopt.lt.p_valuek+p_tolecur)return
	dorz=.true.
	p_objfz=p_objf
	p_pivotrz=0
	!p_unitchange=p_unit
	!p_kierchange=p_kier
	!	p_akey=j_0
	!	p_aopt=j_0
	p_akey0=j_0
	p_aopt0=j_0
	p_anew0=j_0
	if(p_xpresent)then
		if(p_isn16)write(p_n16,*)'vxhetre',p_vx
		do j=1,p_nrowcurx
			ir=p_rowcurx(j)
			p_akey0(ir)=p_xmatkeyrow(p_ix(ir))
			p_aopt0(ir)=p_xmat(ibaxmatopt+p_ix(ir))
			if(p_isn16)write(p_n16,*)'ir,key,opt',ir,p_akey0(ir),p_aopt0(ir)
 
			!		p_anew0(ir)=p_aopt0(ir)-p_akey0(ir)
		enddo !j=1,p_nrowcurx  13567
	endif !if(p_xpresent)  13565
	if(p_fpresent)then
		iobs= p_ibaunit(p_unit)+p_keys(p_unit)
		iobsopt=p_ibaunit(p_unit)+p_iopt
		ibaxmat=ibamatx(iobs) !,1)
		ibaxmat2=ibamatx(iobsopt) !,2)
		p_logvolkey=j_0
		p_logvol=j_0
		do ilog=1,p_nlog
			logvol=j_dmat(ibadatopt+p_keeplog(ilog))
			logvolkey=j_dmat(ibadatkey+p_keeplog(ilog))
			if(p_optfact(ilog).le.0)then
				write(16,*)'optfact0ilog,logvol,logvolkey,ibadatopt,ibadatkey',ilog,logvol,logvolkey,ibadatopt,ibadatkey
				cycle
			endif !if(p_optfact(ilog).le.0)  13586
			!				if(logvol.le.j_0)cycle
			inde=p_ibafact(ilog)+p_factw(p_optfact(ilog))   !index of the logfact
 
			iterm=p_logfactterm(inde)
 
			irow=p_rowofterm(iterm)
			p_aopt0(irow)=p_aopt(irow)+logvol
			p_akey0(irow)=logvolkey
 
			! p_akey0(irow)=p_xmatkeyrow(p_ix(irow))
			! p_aopt0(ir)=p_xmat(ibaxmatopt+p_ix(ir))
			! p_anew0(ir)=p_aopt0(ir)-p_akey0(ir)
 
			! p_a(p_abas(p_newa)+irow)=logvolkey-logvol
			! p_logvolkey(irow)=logvolkey
			! p_logvol(irow)=logvol
			!	write(16,*)'ilog,irow',ilog,irow,p_a(p_abas(p_newa)+irow)
 
		enddo !ilog=1,p_nlog  13583
 
	endif !if(p_fpresent)  13576
	p_anew0=p_aopt0-p_akey0
 
 
 
 
	!if(p_isn16)write(p_n16,*)'unit,r70',p_r
	p_enter(3)=.true.
	if(p_isn16)write(16,*)'**ent s ,unit,shed',p_unit,p_iopt,' key ',p_keys(p_unit),&
		'opt',p_valueopt,'key',p_valuek,'ient',p_enter,'ob',p_objf
	!	if(p_isn16)call rint('aft ent',p_n16)
	!!!write(16,*)''
	if(p_isn16)write(p_n16,*)'*levaenew in senter'
100	p_tmax=j_1
	call leftvec(p_anew0(1:p_nrow),p_r)
	call leavenew() !senter
 
	!if(p_isn16)call rint('aftleve',p_n16)
	!!!write(16,*)' '
	if(p_isn16)write(p_n16,*)'leavetype aft leavenew in *sent',p_leavetype,'tmax',p_tmax,p_tmax.lt.1.d-6,'just key = 0'
	if(p_isn16)write(p_n16,*)'p_oldx',p_oldx
	!	if(p_pivot.gt.325400)write(21,*)'leavetype aft leavenew in *sent',p_leavetype,'tmax',p_tmax,p_tmax.lt.1.d-6,'just key = 0'
	! if(p_tmax.lt.1.d-6)then
	! p_enter(3)=.false.
	! if(p_isn16)write(p_n16,*)'RETURN HERE'
	! return
	! endif !if(p_tmax.lt.1.d-6)  13418
	select case(p_leavetype)
	case(0)
 
	!!!write(16,*) '*just key oldk newk',p_keys(p_unit),p_iopt,' obj ',p_objf,'p_unit',p_unit
	!!!write(16,*)'xps bef',p_xps
	p_xps(0:p_nrow)=p_xps(0:p_nrow)-p_akey0(0:p_nrow)+p_aopt0(0:p_nrow)
	!!!write(16,*)'xps aft',p_xps
	p_keys(p_unit)=p_iopt
	p_rhsw=p_rhscur-p_xps(1:p_nrow)
	!!!write(16,*)'pxbef ls',p_x(p_ls(1:p_nrow))
	!!!write(16,*)'pxbef lx',p_x(p_lx(1:p_lx0)+p_nrowz)
	call leftvec(p_rhsw,p_x)
	call testweight()
	if(j_err)return
	!!!write(16,*)'pxaft ls',p_x(p_ls(1:p_nrow))
	!!!write(16,*)'pxaft lx',p_x(p_lx(1:p_lx0)+p_nrowz)
 
	!!!write(16,*)' obj bef',p_objf
	!	!!!write(16,*)'x',p_x(p_ls(1:p_nrow))
	!!!write(16,*)'ls',p_ls(1:p_nrow)
	!	call testxps('justkey')
	if(p_feasible)then
		p_objf=p_xps(0)+dot_product(p_x(p_ls(1:p_nrow)),p_objr(p_ls(1:p_nrow)))
		!!!write(16,*)' obj new',p_objf
	else
		!!!write(16,*)'calling isfeasible in justkey'
		call isfeasible()
 
	endif !if(p_feasible)  13658
	if(p_isn16)write(p_n16,*)'obj after just key ',p_objf,'p_nnf',p_nnf
	call testxps('xps after')
	!!!write(16,*)' '
 
 
 
	case(1)    !p_re) !resdual
	!!!write(16,*)'entcolhere'
	call entercol(newc)
	!!!write(16,*)'resleav',p_oldc,'*newc*',newc
	!write(6,*)'resleav',p_oldc,newc
	!!!write(16,*)' '
	!!!write(16,*)'#pivotnowreasleav bef',p_oldc,newc
	if(p_isn16)write(p_n16,*)'pivotin senter resleav'
	call pivotnow(newc)  !renter
	call aftpivot() ! computes p_x, p_vc and p_objf
 
	!	if(p_isn16)call rint(' aftpivotresleav ',p_n16)
 
 
	!	read(5,*)ll
	case(2)   !p_ze)  !z
	call entercol(newc)
	!!!write(16,*)'zleav',p_oldc,'*newc*',newc
	!	write(6,*)'resleav',p_oldc,newc
	!!!write(16,*)' '
	!!!write(16,*)'#pivotnowzleav',p_oldc,newc
	if(p_isn16)write(p_n16,*)'pivotin senter zleav'
	call pivotnow(newc)
 
	call aftpivot() ! computes p_x, p_vc and p_objf
 
 
	case(3)   !p_se)  !se
	if(p_isn16)write(p_n16,*)'entercol senter sched leavs,newc ',newc
	call entercol(newc)
	!!!write(16,*)' '
	!!!write(16,*)'#pivotnow s leaves,old,*newc*',p_oldc,newc
	if(p_isn16)write(p_n16,*)'pivot senter sleaves'
	call pivotnow(newc)
	call aftpivot() ! computes p_x, p_vc and p_objf
 
	case(4)   !p_sk)
	!!!write(16,*)' '
	!!!write(16,*)'keyleav,same,p_leavek',p_nsame,p_oldc,'lunit',p_lunit(p_oldx),&
	! 'ienter',p_enter0,p_enter1,p_enter2,p_enter3,'p_lx0',p_lx0,'p_lx',p_lx
	if(p_isn16)write(p_n16,*)'**leavekey in senter'
	call leavekey()  ! senter actuall p_oldx -column in D
 
	if(p_isn16)write(p_n16,*)'into aftpivot in senter after leqavekey'
	call aftpivot()
	! call entercol(newc)
	! !!!write(16,*)' '
	! !!!write(16,*)'#pivotnow s leaves,old,*newc*',p_oldc,newc
	! if(p_isn16)write(p_n16,*)'pivotin lsenter sleav'
	! call pivotnow(newc)
	! call aftpivot() ! computes p_x, p_vc and p_objf
 
 
	!!!write(16,*)'p_oldcnow',p_oldc,'oldx',p_oldx
	!!!write(16,*)'entercol leavekey'
	if(j_err)return
	if(p_isn16)write(p_n16,*)'**goto100 afterleavekey in senter'
	!	if(p_pivot.gt.325400)write(21,*)'goto100ger'
	goto 100
 
	end select
	if(p_p)then
		j_err=.true.
		write(6,*)'tasnu'
		return
 
	endif !if(p_p)  13733
 
	return
 
end subroutine senter !subroutine senter()



subroutine zenter(dorz)
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_post
	use jmod, only: p_enter
	use jmod, only: j_0
	use jmod, only: p_vcmax
	use jmod, only: p_lz0
	use jmod, only: p_nz
	use jmod, only: p_lz
	use jmod, only: p_nrow
	use jmod, only: p_val
	use jmod, only: p_objr
	use jmod, only: p_vc
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_ilzmax
	use jmod, only: p_objf
	use jmod, only: j_inf
	use jmod, only: p_tmax
	use jmod, only: p_r
	use jmod, only: p_ls
	use jmod, only: p_oldc
	use jmod, only: p_leavetype
	use jmod, only: p_pivotrz
	use jmod, only: p_objfz
	use jmod, only: p_tiny6
	use jmod, only: p_pivot
	logical dorz
	if(p_isn16)write(p_n16,*)' '
	if(p_isn16)write(p_n16,*)'*sub zenter **************'
	!	use fletdmod
	!	use fletdmod2
	!	use fletcherdmod
	!	write(16,*)'zenter objr',p_objr, ',vc',p_vc
	p_post=.true.
	!j_yes=.false.
	!	p_enter2=.false.
	p_enter=.false.
 
	p_vcmax=j_0  !j_ninf
	!if(.not.p_priced)call getvc(p_objr)
	!	p_dapu=p_vcmax
	!	if(p_p)write(p_n16,*)'startzenter,p_lz0,p_nz',p_lz0,p_nz
	!	if(p_p)write(p_n16,*)'objr',p_objr
	!	if(p_p)write(p_n16,*)'pvc',p_vc(1:p_nrow)
	!!!write(16,*)' '
	!!!write(16,*)'*zenter objrz',p_objrz,p_feasible,'plz0',p_lz0
	!!!write(16,*)'  objr',p_objr,p_feasible
	do ilz=p_lz0+1,p_nz
		newa0=p_lz(ilz)	! col number in A
		newc0=newa0+p_nrow
		!is(p_p)write(p_n16,*)'newa0',newa0
		!	newc0=newa0+p_nrow  ! col number taking into account the I part tarvitaanko p_newd
		!	if(newc0.eq.p_bad)cycle
		! ol ico pit olla newa
		!	p_val=p_objr(newc0) !zero if .not.p_feasible
		!!!write(16,*)'ilz newa0,objr',ilz,newa0,p_val,p_feasible
 
		!!!write(16,*)'a',p_a(p_abas(newa0)+1:p_abas(newa0)+p_nrow)
 
		!!!write(16,*)'newa0,abas',newa0,p_abas
		!!!write(16,*)'vc',p_vc
		!!!write(16,*)'pa',p_a(p_abas(newa0)+1:p_abas(newa0)+p_nrow)
		p_val=p_objr(newc0)-dot_product(p_vc,p_a(p_abas(newa0)+1:p_abas(newa0)+p_nrow))
		! do  j=1,p_nrow
		! p_val=p_val-p_vc(j)*p_a(p_abas(newa0)+j)  !p_a(j,newa0)
		! enddo ! j=1,p_nrow  13135
		!!!write(16,*)'p_val,pvcmax',p_val,p_vcmax,'newa',newa0,p_val.gt.p_vcmax
		!	endif !if(sparse)  18218
		! ei ny otettu huomioon mahdollisuutta, että z voisi olla nyt ylärajalla
		! ja jos se tulee kantaan negatiivisena, niin tavoite voiis kasvaa
		!		if(p_val.gt.epsj)then
		if(p_val.gt.p_vcmax)then
			!	write(p_n16,*)'zenterpvcmaxpval',p_vcmax,p_val,newc0,newa0
 
			newa=newa0
			newc=newc0
			p_enter(2)=.true.	 ! ;p_rcur=p_tmax
			p_ilzmax=ilz  !	do ilz=p_lz0+1,p_nz
			p_vcmax=p_val
			if(p_isn16)write(p_n16,*)'*zenter  pvcmaxpval,',p_vcmax,'newa',newa
		endif !if(p_val.gt.p_vcmax)  13792
	enddo !ilz=p_lz0+1,p_nz  13768
 
	!if(p_enter2)!!!write(16,*)'*ent zenter p_vcmax tmax zmax ',p_vcmax,p_tmax,p_ilzmax,'newc ',newc
	! if(p_p)then
	! if(p_enter.eq.2)then
	! write(p_n16,*)'z could enter',p_newa,'tmax',j_inf,'newc',newc
	! if(p_fpresent) write(p_n16,*)'**fact** KANTAAN >> z, p_enter = 2'
	! else
	! write(p_n16,*)'z could not enter enter, p_enter', p_enter
	! endif !if(p_enter.eq.2)   9912
	! endif !if(p_p)   9911
 
	if(.not.p_enter(2))return
	if(p_isn16)write(16,*)'**ent ze newa',newa,'p_enter',p_enter,'newc',newc,' obj',p_objf
	p_tmax=j_inf
 
 
	!!!write(16,*)' '
	!!!write(16,*)' '
	call leftcol(newc,p_r)
	if(p_isn16)write(p_n16,'(a,(15i5))')'inzenter p_ls',p_ls(1:p_nrow)
	if(p_isn16)write(p_n16,'(a,(15f10.2))')'inzenter p_r',p_r(p_ls(1:p_nrow))
	!if(p_isn16)write(p_n16)'newchere'
	!	p_enter3=.false.
	if(p_isn16)write(p_n16,*)'*zenter into leavenew'
	call leavenew() !p_oldc
	!!!write(16,*)'aftzentertmax,p_oldc',p_tmax,p_oldc
 
	if(p_oldc.eq.0)then
		if(p_isn16)write(p_n16,*)'newc',newc,'leave0ienter1'
		call leave0ienter1(newc)
		return
	endif !if(p_oldc.eq.0)  13830
	if(p_isn16)write(p_n16,*)'aftleav p_ioldc',p_oldc,'newc',newc,'leaetype',p_leavetype
	if(p_isn16.and.p_leavetype.eq.4)write(p_n16,*)'into leavekey as type=4'
	if(p_leavetype.eq.4)call leavekey()   !zenter
	!		if(newc.gt.p_nrowz)mqjj=-1
	!p_oldc=p_oldc+p_nrowz
	!!!write(16,*)' '
	if(p_isn16)write(p_n16,*)'intopivot in zenter p_old',p_oldc
	call pivotnow(newc)  !zenter
	if(p_isn16)write(p_n16,*)'intoaftpivot in zenter p_old',p_oldc
	call aftpivot() ! computes p_x, p_vc and p_objf
 
 
	dorz=.true.
	p_pivotrz=p_pivotrz+1
	if(p_pivotrz.eq.10)then
		if(p_objf.lt.p_objfz+p_tiny6)then
			dorz=.false.
			write(6,*)'cycling detected'
			if(p_isn16)write(p_n16,*)'cycling obj ',p_objf,' pivot ',p_pivot
		endif !if(p_objf.lt.p_objfz+p_tiny6)  13850
		p_objfz=p_objf
		p_pivotrz=0
 
	endif !if(p_pivotrz.eq.10)  13849
	!call jlplex(p_lz,p_lz0,p_lzi(p_leavec-p_nrow),p_lzi)
	!	call updatel()
	if(p_isn16)write(p_n16,*)'return from zenter'
end subroutine zenter !subroutine zenter()



subroutine renter(dorz)
	use jmod, only: p_enter
	use jmod, only: j_0
	use jmod, only: p_feasible
	use jmod, only: p_lr0
	use jmod, only: p_nrow
	use jmod, only: p_lr
	use jmod, only: p_ebou
	use jmod, only: p_isn16
	use jmod, only: p_n16
	use jmod, only: p_lower
	use jmod, only: p_vc
	use jmod, only: p_post
	use jmod, only: p_r0
	use jmod, only: p_objr
	use jmod, only: p_r
	use jmod, only: j_inf
	use jmod, only: p_tmax
	use jmod, only: p_oldc
	use jmod, only: p_leavetype
	use jmod, only: p_pivotrz
	use jmod, only: p_objf
	use jmod, only: p_objfz
	use jmod, only: p_tiny6
	use jmod, only: p_pivot
	logical::dorz
	double precision::vcmax,vcmin,vccur,maxtmax,mintmax
	!	logical ismin,ismax
	!	if(p_isn16)write(p_n16,*)'*sub renter *******************'
 
	!ismin=.false.
	!ismax=.false.
	p_enter=.false.
	!	p_post=.true.
 
 
	! new objective is larger if t>0 and d0-vc'*d >0 or t<0 and d0-vc'*d <0
	! can residual enter
	! if residual enters, d is a vector having one at some row and zero elsewhere
	! the new value of residual is t, which can be positive or negative
	! d0 for residual is zero (for nonfeasible row, d0 is 1 or -1, but is in the phase
	! where residual is already in the basis, here we consider that we should enter a
	! residual)
	! constraint is:  row+resid=rhs
	! resid can be nonzero if
	! current constraint is:  row>rhs  then resid<0 and obj increases if -vc<0 i.e. vc>0
	! or constraint is row<rhs2 then resid>0 and obj increases if -vc>0 i.e. vc<0
	! if constraint is row=rhs then resid cannot be nonzero
	! but it may be useful to put the residual into basis in case its price is close to ze
	! because the residual part of the basis is easier to handle and it will reduce
	! unnecessary changes in the d-part of the basis
	!  siis pistetään kaikki itseisarvoltaan pienet
	! price<0 and rhs active and rhs2-rhs>0
	! testi
	!	vcmin=j_0
	!write(p_n16,*)'*renter,objr ',p_objr,'vc',p_vc
 
100 	vcmax=j_0
	!	p_enter1=.false.
	!!!write(16,*)'renter',p_vcmax
	!if(.not.p_feasible)!!!write(16,*)'objr',p_objr(1:p_nrow)
 
 
 
	! nonbasic residuals are zero
 
	if(p_feasible)then
		!residuals are not in the objective
 
		do ilr=p_lr0+1,p_nrow   !!!!! nonbasic residuals <B1>
			ico=p_lr(ilr)  !lri(lr(ilr))  ! row=col for residual varaibles, note lr
 
 
 
			if(.not.p_ebou(ico))then  !p_rhs2(ico).gt.p_rhs(ico))then
				!	p_r0=99.
				!	call leftcol(ico,p_r0)
				if(p_isn16)write(p_n16,*)'p_r0,ico',ico
				!	write(p_n16,'(15f7.3)')p_r0
 
 
				! endif !if(.not.p_feasible)  13615
				if(p_lower(ico))then
 
					!		write(p_n16,*)'herevcmax',vcmax,'vc',p_vc(ico),'obr',p_objr(ico)
					vccur=p_vc(ico)
					!	vccur=p_objr(ico)-p_vc(ico)
					if(vccur.gt.vcmax)then
						vcmax=vccur
						ilrmax=ilr
						p_enter(1)=.true.
						newc=ico
						p_post=.false.
 
						!		p_r=p_r0
					endif !if(vccur.gt.vcmax)  13929
 
 
 
				else
					!			write(p_n16,*)'herevcmax',vcmax,'vc',p_vc(ico),'obr',p_objr(ico)
					vccur=-p_vc(ico)  !-p_objr(ico)
					if(vccur.gt.vcmax)then
						vcmax=vccur
						ilrmax=ilr
						p_enter(1)=.true.
						newc=ico
						p_post=.true.
						!	if(vccur.gt.vcmax.and.p_lower(ico)) then  !p_lower(ico))then
						!			if(p_isn16)write(p_n16,*)'renter,ico,vcmax,lower',ico,vcmax,p_lower(ico),'post',p_post
					endif !if(vccur.gt.vcmax)  13944
 
 
				endif !if(p_lower(ico))  13924
 
			endif !if(.not.p_ebou(ico))  13916
 
		end do !ilr=p_lr0+1,p_nrow  13911
		if(p_isn16.and.p_enter(1))	write(p_n16,*)'renter,FEASIBLE ico,vcmax,lower',ico,vcmax,p_lower(ico),'post',p_post
 
	else
		! nonfeasible residuals are inte objective
		do ilr=p_lr0+1,p_nrow   !!!!! nonbasic residuals <B1>
			ico=p_lr(ilr)  !lri(lr(ilr))  ! row=col for residual varaibles, note lr
 
			if(.not.p_ebou(ico))then  !p_rhs2(ico).gt.p_rhs(ico))then
				!	p_r0=99.
				!	call leftcol(ico,p_r0)
				if(p_isn16)write(p_n16,*)'p_r0,ico',ico
				!	write(p_n16,'(15f7.3)')p_r0
 
				! endif !if(.not.p_feasible)  13615
				if(p_lower(ico))then
 
					!			write(p_n16,*)'herevcmax',vcmax,'vc',p_vc(ico),'obr',p_objr(ico)
					vccur=j_0  !-p_vc(ico)
					call leftcol(ico,p_r0)
					if(p_isn16)write(p_n16,'(a,(12f12.2))')'p_r0 lowe',p_r0(p_lr(1:p_lr0))
					do ilr2=1,p_lr0
						vccur=vccur+p_objr(p_lr(ilr2))*p_r0(p_lr(ilr2))
					enddo !ilr2=1,p_lr0  13980
					!		if(p_isn16)write(p_n16,*)'vccurhere',vccur
 
					!	vccur=p_objr(ico)-p_vc(ico)
					if(vccur.gt.vcmax)then
						vcmax=vccur
						ilrmax=ilr
						p_enter(1)=.true.
						newc=ico
 
						p_post=.false.
						!		if(p_isn16)write(p_n16,*)'renter,ico,vcmax,lower',ico,vcmax,p_lower(ico),'post',p_post
 
						!		p_r=p_r0
					endif !if(vccur.gt.vcmax)  13986
 
				else
					!			write(p_n16,*)'herevcmax',vcmax,'vc',p_vc(ico),'obr',p_objr(ico)
					vccur=j_0  !p_vc(ico)  !-p_objr(ico)
					call leftcol(ico,p_r0)
					if(p_isn16)write(p_n16,'(a,(12f12.2))')'p_r0 nonlow ',p_r0(p_lr(1:p_lr0))
 
					do ilr2=1,p_lr0
						vccur=vccur-p_objr(p_lr(ilr2))*p_r0(p_lr(ilr2))
					enddo !ilr2=1,p_lr0  14004
					!		if(p_isn16)write(p_n16,*)'vccurhere',vccur,'ico',ico
 
					if(vccur.gt.vcmax)then
						vcmax=vccur
						ilrmax=ilr
						p_enter(1)=.true.
						newc=ico
						p_post=.true.
						!	if(vccur.gt.vcmax.and.p_lower(ico)) then  !p_lower(ico))then
						!					if(p_isn16)write(p_n16,*)'renter,ico,vcmax,lower',ico,vcmax,p_lower(ico),'post',p_post
					endif !if(vccur.gt.vcmax)  14009
 
 
				endif !if(p_lower(ico))  13974
 
 
			endif !if(.not.p_ebou(ico))  13967
 
		end do !ilr=p_lr0+1,p_nrow  13964
 
 
 
 
	endif !if(p_feasible)  13908
 
	!!!write(16,*)'vcmax,ilrmax iente ',vcmax,ilrmax,p_enter1
	if(p_isn16)write(p_n16,*)'inrenter,p_enter ',p_enter,'newc',newc,'dorz',dorz
	if(.not.p_enter(1))return
 
	if(p_isn16)write(p_n16,*)'**ent residual ',newc, 'post',p_post
	!		call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,newc,p_a(p_abas(p_newa)+1:),r, & !!!!
	!			p_ls,wslu1,lwsll1,.false.)   !!!!
 
	!!!write(16,*)' '
	!	if(p_isn16) write(16,*)'**ent *rent ',newc, 'p_lower ',p_lower(newc),'p_vc ',p_vc(newc),'pos',p_post,'tmax',p_tmax,' ob',p_objf
	dorz=.true.
	call leftcol(newc,p_r)
	if(p_isn16)write(p_n16,'(a,(12i7))')'p_lr ny',p_lr(1:p_lr0)
	if(p_isn16)write(p_n16,'(a,(12f12.2))')'p_r ny',p_r(p_lr(1:p_lr0))
	!!!write(16,*)'pr222',p_r
	!	p_enter3=.false.
	!!!write(16,*)''
	!!!write(16,*)'leavenew in renter'
	p_tmax=j_inf
	if(p_isn16)write(p_n16,*)'into leavenew in renter,tmax',p_tmax,'post',p_post
	call leavenew()     !renter
	if(p_isn16)write(p_n16,*)'**renter tmax',p_tmax,'p_oldc',p_oldc,'leavdtype',p_leavetype,'newc ',newc
	if(p_oldc.eq.0)then
		write(16,*)'poldc',p_oldc,'new',newc,'leave0ienter1'
		!	call leave0ienter1(newc)
		!return
		goto 200
	endif !if(p_oldc.eq.0)  14054
 
	if(p_leavetype.eq.4)then
		if(p_isn16)write(p_n16,*)'into leavekey in renter',p_oldc
		call leavekey()   !renter
		if(p_isn16)write(p_n16,*)'into aftpivot aft leavekey in renter',p_oldc
		call aftpivot()
		if(p_isn16)write(p_n16,*)'**goto100'
		p_enter(1)=.false.
		goto 100
	endif !if(p_leavetype.eq.4)  14061
	!p_oldc=p_oldc+p_nrowz
	!!!write(16,*)' '
	if(p_isn16)write(p_n16,*)'pivotin renter p_old',p_oldc
	call pivotnow(newc)  !renter
	if(p_isn16)write(p_n16,*)'into aftpivot in renter'
200	call aftpivot() ! computes p_x, p_vc and p_objf
	!!!write(16,*)'renter,tmax,x',p_x(newc),p_tmax,'oldnew',p_oldc,newc
 
 
 
!	if(p_lr0.ne.p_nrow)call clearnup()   !nup=0   !clear refactorizing counter
	p_pivotrz=p_pivotrz+1
	if(p_pivotrz.eq.10)then
		if(p_objf.lt.p_objfz+p_tiny6)then
			dorz=.false.
			write(6,*)'cycling detected in renter, objf',p_objf,' pivot ',p_pivot
			if(p_isn16)write(p_n16,*)'cycling detected in renter,objf ',p_objf,'pivot ',p_pivot
 
		endif !if(p_objf.lt.p_objfz+p_tiny6)  14083
		p_objfz=p_objf
		p_pivotrz=0
 
	endif !if(p_pivotrz.eq.10)  14082
	!if(.not.p_feasible)  10109
 
 
end subroutine renter !subroutine renter()





subroutine tulostele()
	use jmod, only: p_feasible
	use jmod, only: p_iprint
	use jmod, only: p_nresw
	use jmod, only: p_npivotw
	use jmod, only: p_nkeys
	use jmod, only: p_fpresent
	use jmod, only: p_nkeyf
	use jmod, only: p_zerob
	use jmod, only: j_v
	use jmod, only: j_1
	use jmod, only: p_ivfeasible
	use jmod, only: p_ivoptimal
	use jmod, only: p_xpresent
 
	!end kierloop
 
	if(p_feasible)then
		!	If(.not.p_maxo)p_objf=-p_objf
		write(6,*) 'solution is feasible'!!!!
	else !if(j_feasible)then
		write(6,*) 'SOLUTION IS INFEASIBLE' !!!!
		!	write(6,*)'nofeasible rows ',p_nnf
		if(p_iprint.lt.2)p_iprint=2
	endif !if(p_feasible)  14106
 
	!write(6,*)'pivots ',p_pivot,' rounds ',p_kier  !!!!
 
	if(p_nresw.gt.7)write(6,*)'total number of residual cannot enter conditions ',p_nresw
	if(p_npivotw.gt.7)write(6,*)'total number of pivot cannot be made conditions ',p_npivotw
	!	if(p_nkeyfactw.gt.7)write(6,*)'total number of key factory cannot be changed conditions ',p_nkeyfactw
	write(6,*)'key schedule changed ',p_nkeys, ' times'  !!!!
	if(p_fpresent)write(6,*)'key factory  changed ',p_nkeyf, ' times'  !!!!
	!write(6,*)'basic variables computed ',p_ntote,' times' !!!!
	if(p_zerob.gt.0)write(6,*)'zero objective vector encountered ',p_zerob,' times'
 
	if(p_feasible)then
		j_v(p_ivfeasible)=j_1
		j_v(p_ivoptimal)=j_1
 
	endif !if(p_feasible)  14125
 
	!call defsolu()
	if(p_xpresent)then
 
		call getsolx() !p_nunits,ibaunit,keys,lunit,nsch,isch,
		!	write(6,*)'wei3334 ',wei,p_sumx(3)
	endif !if(p_xpresent)  14132
 
 
	if (p_fpresent) call defsoluf()
 
 
 
end subroutine tulostele !subroutine tulostele()


subroutine repo(nureport)
	use jmod, only: j_v
	use jmod, only: p_ivobj
	use jmod, only: p_coefmax
	use jmod, only: p_objf
	use jmod, only: p_issolution
	use jmod, only: p_nrow
	use jmod, only: p_ncol
	use jmod, only: j_0
	use jmod, only: p_x
	use jmod, only: p_ls
	use jmod, only: p_maxo
	use jmod, only: p_objr
	use jmod, only: p_nz
	use jmod, only: j_defmatrix
	use jmod, only: j_ivout
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: j_o
	use jmod, only: p_rhscur
	use jmod, only: p_vc
	use jmod, only: p_iprint
	use jmod, only: j_getname
	use jmod, only: p_xpresent
	use jmod, only: p_isdomain
	use jmod, only: p_rowdomnum
	use jmod, only: p_buf
	use jmod, only: j_getline
	use jmod, only: p_ivdomain
	use jmod, only: p_domainname
	use jmod, only: p_ldomainname
	use jmod, only: j_chi5
	use jmod, only: p_domainunits
	use jmod, only: p_nunits
	use jmod, only: p_intapp
	use jmod, only: j_chr10
	use jmod, only: p_solx
	use jmod, only: p_nxvar
	use jmod, only: p_ivxvar
	use jmod, only: j_inlist
	use jmod, only: p_nxvararea
	use jmod, only: p_xvararea
	use jmod, only: j_oname
	use jmod, only: j_loname
	use jmod, only: j_yes
	use jmod, only: p_ix
	use jmod, only: p_ibatemp
	use jmod, only: p_nrowx2
	use jmod, only: p_irowxvar
	use jmod, only: p_xcoef
	use jmod, only: p_sumx
	use jmod, only: p_dots
	use jmod, only: p_nsumx
	use jmod, only: p_shpx
	use jmod, only: p_sumxi
	use jmod, only: j_inlistobject
	use jmod, only: j_getobject
	use jmod, only: j_oname3
	use jmod, only: j_loname3
	use jmod, only: j_ipreal
	use jmod, only: p_nshow
	use jmod, only: p_ndom
	use jmod, only: j_getchar
	use jmod, only: p_show
	use jmod, only: p_ivshowunits
	use jmod, only: p_isz
	use jmod, only: p_nureport
	!	write(6,*)'<67>j_nshow',p_nrow,p_nshow
	j_v(p_ivobj)=p_coefmax*p_objf
 
901	continue
 
	p_issolution=.true.
	if(p_nrow.gt.0)then  !there are constraints
		do i=p_nrow+1,p_ncol+p_nrow
			p_x(p_ls(i))=j_0
			!	if(p_ls(i).eq.2)write(6,*)'<4774putzero'
		end do !i=p_nrow+1,p_ncol+p_nrow  14154
		! if(.not.p_maxo)then  ! poistettu bugia metsästettäessä
		! p_vc=-p_vc                        !;objf=-objf done earlier
		! !		p_vx=-p_vx
 
		! end if !if(.not.p_maxo)   9127
 
	endif !if(p_nrow.gt.0)  14153
	if(.not.p_maxo)p_objr=-p_objr
 
 
877 continue !nrow=0
 
 
	! endif !if (p_nz>0)   9201
 
	if(p_nrow>0.and.p_nz.eq.0) then
		!write(6,*)'<nrow',j_nrow
		ivout=j_defmatrix(j_ivout,'%rows',int8(p_nrow),j_18,j_matreg)
		j_o(ivout)%d(1:p_nrow)=p_rhscur(1:p_nrow)-p_x(1:p_nrow)
 
		ivout=j_defmatrix(j_ivout,'%shprice',int8(p_nrow),j_18,j_matreg)
		j_o(ivout)%d(1:p_nrow)=p_coefmax*p_vc(1:p_nrow)
	endif !if(p_nrow>0.and.p_nz.eq.0)  14173
 
	irow=0
	idom=1 !counter for domain statements
	idomv=-1
	!write(6,*)'<467p_iprint',p_iprint
	if(p_iprint.lt.1)goto 8000
 
	call j_getname(-1,-1,j_ivout)
	do irow=0,p_nrow
		!			domloop: do j=1,p_nsetd(i)
		!	write(6,*)'<5553>',jirow
		irowtot=irow+1
		if(p_xpresent.and.p_isdomain)then
			idom=p_rowdomnum(irow)
 
		endif !if(p_xpresent.and.p_isdomain)  14193
		p_buf=' '
		if(idom.ne.idomv)then
			!unpublished
			!	p_buf='DOMAIN:      All'
			if(p_isdomain)then
 
				call j_getline(p_ivdomain,idom,p_domainname,p_ldomainname)
 
				!	call j_getline(p_ivdomain,idom,p_buf(8:),le)
				p_buf(8:)=p_domainname(1:p_ldomainname)
				p_buf(74:78)='units'
				p_buf(68:72)=j_chi5(p_domainunits(idom),0)
			else
				p_domainname='All'
				p_ldomainname=3
				p_buf(1:4)='All:'
				p_buf(74:78)='units'
				p_buf(68:72)=j_chi5(p_nunits,0)
 
			endif !if(p_isdomain)  14201
			write(nureport,*)' ',('_',kk=1,78)
			write(nureport,'(a)')p_buf(1:79)
 
		endif !if(idom.ne.idomv)  14198
		!	end if !if(p_xpresent.and.p_isdomain)   6551
		if(idom.ne.idomv)then
			write(nureport,*)' ',('_',kk=1,78)
			write(p_buf,66061)
66061 format('row',t38,'value',t50,'shadow',t61,  'lower',t70,'upper')
			write(nureport,'(a)')p_buf(1:79)
			write(p_buf,6606)
6606 format(t50,'price', t61,'bound',t70, 'bound')
			if(p_intapp)p_buf(35:44)='int. app.'
			write(nureport,'(a)')p_buf(1:79)
			write(nureport,*)' ',('_',kk=1,78)
		endif !if(idom.ne.idomv)  14222
 
		call writerow(nureport,irow)
 
		if(p_intapp)then
			p_buf=' '
 
			p_buf(36:)=j_chr10(p_solx(irow))
			write(nureport,'(a)')p_buf(1:79)
		endif !if(p_intapp)  14236
		!	end do !k=1,p_nsetr(i)   5561
		idom2=idom
		if(p_isdomain)idom2=p_rowdomnum(min(irow+1,p_nrow))
		if(idom2.ne.idom.or.irow.eq.p_nrow)then
 
			if(p_iprint.ge.1)then
				write(nureport,*)' ',('_',kk=1,78)
				p_buf='     x-variable'  ! 'x-variable'
 
				p_buf(38:50)='value'; p_buf(47:58)='shadow price'
				if(p_intapp)p_buf(62:79)='integer appr.-opt.'
 
				write(nureport,'(a)')p_buf(1:79)
				write(nureport,*)' ',('_',kk=1,78)
 
 
				do jx=1,p_nxvar  !p_nxvartot !+p_noutsubtrans !p_ncvar,available variables
 
					ivxvar=j_o(p_ivxvar)%i2(jx)
 
					ixkeep=j_inlist(ivxvar,p_nxvararea,p_xvararea) ! ivxvararea)
 
					call j_getname(ivxvar)
					!	write(6,*)'startinf ,p_objf ',p_objf,'p_nnf ',p_nnf,p_lower(1:min(7,p_nrow)),'p_maxo',p_maxo
					p_buf =' '
					p_buf(6:)=j_oname(1:j_loname)
					shp=j_0
					indi=0
					item=1
					do iro=0,p_nrow
 
						!	irotot=iro+1
						j_yes=.true.
						if(p_isdomain)j_yes=idom.eq.p_rowdomnum(iro)
						!			write(6,*)'iroisdomain ',iro,j_yes,p_ix(iro)
						!			write(6,*)'iro,j_yes',iro,j_yes
						if(j_yes.and.p_ix(iro).ne.0)then
 
							item=p_ix(iro)
							iba=p_ibatemp(item)
 
							!				write(6,*)'iro,item,iba,nrowx',iro,item,iba,p_nrowx2(item)
							do ii=1,p_nrowx2(item)
 
 
								if(ivxvar.eq.p_irowxvar(iba+ii))then
									if(iro.eq.0)then
 
 
										shp=p_xcoef(iba+ii)
										!				write(6,*)' shp0 ',shp,'iba ',iba,ii,item
										indi=1
 
									else !if(iro.eq.0)then
										!						write(6,*)'iro,ii,p_xcoef(iba+ii),p_vc(iro) ',iro,ii,p_xcoef(iba+ii),p_vc(iro)
 
										shp=shp-p_xcoef(iba+ii)*p_vc(iro)
										indi=1
									endif !if(iro.eq.0)  14288
								endif !if(ivxvar.eq.p_irowxvar(iba+ii))  14287
								!endif !if(p_yes)   9419
							enddo !ii=1,p_nrowx2(item)  14284
 
						endif !if(j_yes.and.p_ix(iro).ne.0)  14278
 
					enddo !iro=0,p_nrow  14271
 
					if(.not.p_isdomain) j_v(ivxvar)=p_sumx(ixkeep)
 
 
					if(j_loname.lt.26)p_buf(6+j_loname:34)=p_dots
					p_buf(36:46)=j_chr10(p_sumx((idom-1)*p_nsumx+ixkeep))
					if(p_nrow.gt.0)then
						if(indi.ne.0)then
							p_buf(47:56)=j_chr10(dble(shp))
							p_shpx((idom-1)*p_nxvar+jx)=shp
						endif !if(indi.ne.0)  14315
						if(p_intapp)	p_buf(67:76)=j_chr10(p_sumxi((idom-1)*p_nsumx+ixkeep)-p_sumx((idom-1)* &
							p_nsumx+ixkeep))
					endif !if(p_nrow.gt.0)  14314
					write(nureport,'(a)')p_buf(1:79)
 
				end do !jx=1,p_nxvar  14258
				write(nureport,*)' '
				!		write(6,*)'p_nxvararea ', p_nxvararea
				do jx=1,p_nxvararea
					jxv=p_xvararea(jx)  !j_o(j_divkeep)%i2(jx)
					call j_getname(jxv)
 
					ipe=j_inlistobject(jxv,p_ivxvar)
					!	write(6,*)'hff ',jx,jxv,ipe
					!	write(6,*)'jx',jx,jxv,ipe,j_oname(1:j_loname)
					if(p_domainname(1:p_ldomainname).eq.'All')then
 
						ivoo=j_getobject(0,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname),j_ipreal)
 
						!	write(6,*)'bb ',jx,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname)
						!    call j_getobject(jxv,'['//p_domainname(1:p_ldomainname)//']',j_ipreal,ivoo)
						!		j_v(jxv)=p_sumx((idom-1)*p_nsumx+jx)
					else
						ivoo=j_getobject(0,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname)// &
							'['//p_domainname(1:p_ldomainname)//']',j_ipreal)
 
						!	write(6,*)'bbc ',jx,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname)// &
						!			'['//p_domainname(1:p_ldomainname)//']'
 
						!	call j_getobject(jxv,'['//p_domainname(1:p_ldomainname)//']',j_ipreal,ivoo)
						!	j_v(ivoo)=p_sumx((idom-1)*p_nsumx+jx)
						!	call j_getname(ivoo)
						!	write(6,*)j_oname(1:j_loname),' =',j_v(ivoo)
					endif !if(p_domainname(1:p_ldomainname).eq.'All')  14334
 
					j_v(ivoo)=p_sumx((idom-1)*p_nsumx+jx)
 
					if(ipe.gt.0)cycle
					call j_getname(jxv)
					p_buf=' '
 
					p_buf(6:)=j_oname(1:j_loname)
					if(j_loname.lt.26)p_buf(6+j_loname:34)=p_dots
					p_buf(36:46)=j_chr10(j_v(ivoo))  ! p_sumx((idom-1)*p_nsumx+jx))
					write(nureport,'(a)')p_buf(1:79)
 
				enddo !jx=1,p_nxvararea  14327
 
 
			endif !if(p_iprint.ge.1)  14247
		end if !if(idom2.ne.idom.or.irow.eq.p_nrow)  14245
 
 
		!		end do domloop !domloop: do j=1,j_nsetd(i)
		idomv=idom
	end do !irow=0,p_nrow  14189
	if(p_nshow.gt.0)then
		write(6,*)' '
		write(6,*)'showdomains'
		!	write(6,*)'p_ndom',p_ndom,p_nsumx,p_nxvartot,p_sumx
 
		do j=1,p_nshow
			idom=max(p_ndom,1)+j
			call j_getchar(p_show(j),p_domainname,p_ldomainname)
			write(nureport,*)' ',('_',kk=1,78)
			p_buf='  '//p_domainname(1:p_ldomainname)
			p_buf(74:78)='units'
			p_buf(68:72)=j_chi5(j_o(p_ivshowunits)%i2(j),0)
			write(nureport,*)p_buf(1:79)
			write(nureport,*)' ',('_',kk=1,78)
			!	write(6,*)'idomhere',idom
			do jx=1,p_nxvararea
				jxv=p_xvararea(jx) !j_o(j_divkeep)%i2(jx)
				ipe=j_inlist(jxv,p_nxvararea,p_xvararea)
				call j_getname(jxv)
				if(p_domainname(1:p_ldomainname).eq.'All')then
					ivoo=j_getobject(0,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname),j_ipreal)
 
					!		write(6,*)'sss ',jx,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname)
					!	j_v(jxv)=p_sumx((idom-1)*p_nsumx+jx)
				else
					ivoo=j_getobject(0,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname)// &
						'['//p_domainname(1:p_ldomainname)//']',j_ipreal)
 
					!	write(6,*)'SSSS ',jx,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname)// &
					!		'['//p_domainname(1:p_ldomainname)//']'
					!	call j_getobject(jxv,'['//p_domainname(1:p_ldomainname)//']',j_ipreal,ivoo)
					!	j_v(ivoo)=p_sumx(idom*p_nsumx+jx)
					!	call j_getname(ivoo)
					!	write(6,*)j_oname(1:j_loname),' =',j_v(ivoo)
				endif !if(p_domainname(1:p_ldomainname).eq.'All')  14394
				!	if(ipe.gt.0)cycle
				j_v(ivoo)=p_sumx((idom-1)*p_nsumx+jx)
 
				p_buf=' '
 
				p_buf(6:)=j_oname(1:j_loname)
				if(j_loname.lt.26)p_buf(6+j_loname:34)=p_dots
				p_buf(36:46)=j_chr10(j_v(ivoo))  !p_sumx((idom-1)*p_nsumx+jx))
				write(nureport,'(a)')p_buf(1:79)
 
			enddo !jx=1,p_nxvararea  14390
 
 
 
 
		enddo !j=1,p_nshow  14380
	endif !if(p_nshow.gt.0)  14375
	!	p_nxvartot=j_o(j_divkeep)%i(1)
	!p_nsumx=p_nxvartot+p_ncvar+p_noutsubtrans
	!xvar%, xsum%, xprice%,xvarproblem%
	!if(p_xpresent.and.j_ivout.ne.j_ivresult) then
	!	call j_deflist(j_ivout,'%xvar',ivxvartot,nres=p_nsumx)
	! call j_deflist(j_ivout,'%xvar',ivxvartot,list0=p_nxvartot, &
	! list=j_o(j_divkeep)%i2(1:p_nxvartot),nres=p_noutsubtrans)  !p_ncvar+p_noutsubtrans)
 
	!j_o(ivxvartot)%i(1:p_nxvartot)=j_o(j_divkeep)%i(1:p_nxvartot)
	!	if (p_ncvar > 0) j_o(ivxvartot)%i2(p_nxvartot+1:p_nxvartot+p_ncvar) =  &
	!		p_cvarl(1:p_ncvar)
	! if (p_noutsubtrans > 0) &
	! !	j_o(ivxvartot)%i2(p_nxvartot+p_ncvar+1:p_nxvartot+p_ncvar+p_noutsubtrans) =  &
	! j_o(ivxvartot)%i2(p_nxvartot+1:p_nxvartot+p_noutsubtrans) =  &
	! j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans)
	! j_o(ivxvartot)%i(1) = 	p_nsumx
	! call j_deflist(j_ivout,'%xvarproblem',ivout,list0=p_nxvar,list=p_xvar)
 
	! j_o(ivout)%i(1:nxvar)=p_xvar
	! j_o(ivout)%i(1)=nxvar
	ndom_=max(p_ndom,1)
	!write(6,*)'j_nsumx',j_nsumx
	ivout=j_defmatrix(j_ivout,'%xsum',int8(ndom_),int8(p_nsumx),j_matreg)
 
	j_o(ivout)%d(1:ndom_*p_nsumx)=p_sumx(1:ndom_*p_nsumx)
	if(p_intapp)then
		ivout=j_defmatrix(j_ivout,'%xsumint',int8(ndom_),int8(p_nsumx),j_matreg)
		j_o(ivout)%d(1:ndom_*p_nsumx)=p_sumxi(1:ndom_*p_nsumx)
	endif !if(p_intapp)  14452
 
	if(p_nrow.gt.0)then
		ivout=j_defmatrix(j_ivout,'%xprice',int8(ndom_),int8(p_nxvar),j_matreg) !p_nsumx,j_matreg)
 
		j_o(ivout)%d(1:ndom_*p_nxvar)=p_shpx(1:ndom_*p_nxvar)
	endif !if(p_nrow.gt.0)  14457
 
	if(p_nrow.gt.0.and.p_isdomain)then
		ivout=j_defmatrix(j_ivout,'%domains',int8(p_nrow),j_18,j_matreg)
 
		!			j_o(ivout)%d = p_irowdomain(1:p_nrow)
 
	endif !if(p_nrow.gt.0.and.p_isdomain)  14463
 
 
	if(p_isz)call repoz(p_nureport,.true.)
 
 
	write(nureport,*)' '
8000 continue  !end printing
 
 
	!if(nureport.ne.6)call j_closeunit(nureport)
end subroutine repo !subroutine repo(nureport)



subroutine weightstot()
	use jmod, only: j_defmatrix
	use jmod, only: j_ivout
	use jmod, only: j_dnobs
	use jmod, only: j_18
	use jmod, only: j_matreg
	use jmod, only: p_nunits
	use jmod, only: p_ibaunit
	use jmod, only: p_keys
	use jmod, only: p_isarea
	use jmod, only: j_o
	use jmod, only: j_1
	use jmod, only: p_lx0
	use jmod, only: p_lunit
	use jmod, only: p_lx
	use jmod, only: p_isch
	use jmod, only: j_dapu
	use jmod, only: p_x
	use jmod, only: p_nrowz
	double precision area
	ivw=j_defmatrix(j_ivout,'%weights',int8(j_dnobs),j_18,j_matreg)
	do iuni=1,p_nunits
		ii=p_ibaunit(iuni)+p_keys(iuni)  ! index of key sched
		if(p_isarea)then
			j_o(ivw)%d(ii)=area(iuni)
		else
			! if(p_isunit)then
 
			! j_o(ivw)%d(ii)=j_o(j_divmat)%d((ii-1)*j_dnkeep+p_areakeep)
			! !		write(6,*)'ii,p_areakeep,j_o(ivw)%d(ii)',ii,p_areakeep,j_o(ivw)%d(ii)
			! else
			! j_o(ivw)%d(ii)=j_o(j_divmatup)%d((iuni-1)*j_dnkeepup+p_areakeep)
			! endif !if(p_isunit)   6104
			! else
			j_o(ivw)%d(ii)=j_1
		endif !if(p_isarea)  14488
 
 
	enddo !iuni=1,p_nunits  14486
 
	do j=1,p_lx0
		iuni=p_lunit(p_lx(j))
		is=p_isch(p_lx(j))
		j_dapu=p_x(p_nrowz+p_lx(j))
		!		write(6,*)'j,w',j,j_dapu,'iuni',iuni
		if(p_isarea)j_dapu=j_dapu*area(iuni)
 
		! if(p_isunit)then
		! ii=p_ibaunit(iuni)+is
		! !	write(6,*)'ii,j_dnkeep,p_areakeep,',ii,j_dnkeep,p_areakeep
		! j_dapu=j_o(j_divmat)%d((ii-1)*j_dnkeep+p_areakeep)*j_dapu
		! !		write(6,*)'ii,p_areakeep,j_o(ivw)%d(ii)',ii,p_areakeep,j_o(ivw)%d(ii),' j_dapu ',j_dapu
		! else
		! j_dapu=j_o(j_divmatup)%d((iuni-1)*j_dnkeepup+p_areakeep)*j_dapu
		! endif !if(p_isunit)   6124
		! endif !if(p_isarea)   6123
 
		j_o(ivw)%d(p_ibaunit(iuni)+is)=j_dapu
 
		j_o(ivw)%d(p_ibaunit(iuni)+p_keys(iuni))=j_o(ivw)%d(p_ibaunit(iuni)+p_keys(iuni))-j_dapu
		!	write(6,*)'fin ',j_o(ivw)%d(p_ibaunit(iuni)+p_keys(iuni)),j_dapu
 
	enddo !j=1,p_lx0  14505
 
end subroutine weightstot



subroutine testxps(title)
	use jmod, only: j_0
	use jmod, only: p_nunits
	use jmod, only: p_isdomain
	use jmod, only: p_ibaunit
	use jmod, only: p_keys
	use jmod, only: p_nrowcurx
	use jmod, only: p_rowcurx
	use jmod, only: p_xmat
	use jmod, only: p_ix
	use jmod, only: p_nrow
	use jmod, only: p_xps
	character*(*)title
	double precision xps(0:200)
	!	idomba=p_idomba
	!	p_idomba=0
	xps=j_0
	do i=1,p_nunits
		if(p_isdomain)call jlpcurix(i)
		ibaxmat=ibamatx(p_ibaunit(i)+p_keys(i)) !,1)
 
		!		write(18,*)'p_nrowcurx( ',p_nrowcurx,p_rowcurx
		!		write(18,*)'i ',i,'key',p_keys(i),'ibaxmat',ibaxmat
		!summat nyt
		do jj=1,p_nrowcurx
			j=p_rowcurx(jj)
 
			!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
			!		write(18,*)p_xmat(p_ix(j)+ibaxmat)
			xps(j)=xps(j)+p_xmat(p_ix(j)+ibaxmat) !v(ix(j)) !(ix(j)=0 -> no x in row
			!		if(j.eq.0.and.i.ge.6.and.i.le.10)write(p_n16,*)'<555 ',i,ibaxmat,p_ix(j),p_xmat(p_ix(j)+ibaxmat),p_xps(j)
			!	if(j.eq.0)write(6,*)'xps0here',p_xps(0)
		enddo !jj=1,p_nrowcurx  14546
 
	enddo !i=1,p_nunits  14539
	! p_idomba=idomba
	do j=1,p_nrow
		if(abs(xps(j)-p_xps(j)).gt.0.6)then
			write(6,*)title
			!!!write(16,*)title
			write(6,*)'p_xpsny',p_xps
			write(6,*)'BUT calc',xps(0:p_nrow)
			!!!write(16,*)'p_xpsny',p_xps
			!!!write(16,*)'BUTy',xps(0:p_nrow)
			!!!write(16,*)'unit ',p_unit
 
			read(5,*)fjfjd
 
		endif !if(abs(xps(j)-p_xps(j)).gt.0.6)  14559
 
	enddo !j=1,p_nrow  14558
end subroutine testxps

subroutine rint(title,nu)
	use jmod, only: j_err
	use jmod, only: p_objf
	use jmod, only: p_nnf
	use jmod, only: p_objfold
	use jmod, only: p_objr
	use jmod, only: p_ls
	use jmod, only: p_nrow
	use jmod, only: p_lnf
	use jmod, only: p_lr0
	use jmod, only: p_lr
	use jmod, only: p_x
	use jmod, only: p_lz0
	use jmod, only: p_lz
	use jmod, only: p_lx0
	use jmod, only: p_lx
	use jmod, only: p_nrowz
	use jmod, only: p_lunit
	use jmod, only: p_keys
	use jmod, only: p_vc
	use jmod, only: p_r
	use jmod, only: p_lower
	use jmod, only: p_rhscur
	use jmod, only: p_rhsw
	use jmod, only: p_oldx
	use jmod, only: p_nrowx
	use jmod, only: p_aopt
	use jmod, only: p_abas1
	use jmod, only: p_nrowtot
	use jmod, only: p_akey
	use jmod, only: p_xpresent
	use jmod, only: p_a
	use jmod, only: p_abas
	use jmod, only: p_nz
	use jmod, only: j_dapu3
	use jmod, only: j_0
	use jmod, only: j_1
	character*(*) ::title
	write(nu,*)' '
 
	write(nu,*)'**rint ',title
	if(j_err)write(nu,*)'********error'
	write(nu,*)' obj ',p_objf,' p_nnf',p_nnf,'p_objfold',p_objfold
	write(nu,'(a,(15f7.3/))')'pobjr',p_objr
	write(nu,*)'p_ls'
	write(nu,'(15i7)')p_ls(1:p_nrow)
	write(nu,*)'p_lnf',p_nnf
	write(nu,'(15i7)')p_lnf(1:p_nnf)
	if(p_lr0.gt.0)then
		write(nu,*)'p_lr',p_lr0
		write(nu,'(15i10)')p_lr(1:p_lr0)
		write(nu,'(15f10.1)')p_x(p_lr(1:p_lr0))
	endif !if(p_lr0.gt.0)  14587
 
	if(p_lz0.gt.0)then
		write(nu,*)'p_lz',p_lz0
		write(nu,'(15i8)')p_lz(1:p_lz0)
		write(nu,'(15f8.3)')p_x(p_lz(1:p_lz0)+p_nrow)
	endif !if(p_lz0.gt.0)  14593
 
	if(p_lx0.gt.0)then
		write(nu,*)'p_lx',p_lx0
		write(nu,'(15i9)')p_lx(1:p_lx0)
		write(nu,'(15f9.3)')p_x(p_lx(1:p_lx0)+p_nrowz)
		write(nu,*)'lunits,kkeys'
		write(nu,'(15i9)')p_lunit(p_lx(1:p_lx0))
		write(nu,'(15i9)')p_keys(p_lunit(p_lx(1:p_lx0)))
 
	endif !if(p_lx0.gt.0)  14599
 
 
	write(nu,'(a,(15f10.2))')'vc*',p_vc
 
	write(nu,*)'p_r'
	write(nu,*)p_r
	write(nu,*)'plower',p_lower
	!	write(nu,*)'rhs'
	!	write(nu,'(15f7.3)')p_rhs
	!	write(nu,*)'rhs2'
	!	write(nu,'(15f7.3)')p_rhs2
	write(nu,*)'rhscur'
	write(nu,'(15f10.1)')p_rhscur
 
	write(nu,*)'rhsw'
	write(nu,'(15f10.1)')p_rhsw
 
	!	write(nu,*)'rhsdif'
	!	write(nu,'(15f7.3)')p_rhsdif
	if(p_oldx.gt.p_nrowx)then
		write(nu,*)'oldoptkey'
		write(nu,'(20f11.1)')p_aopt(p_abas1(p_oldx)+2:p_abas1(p_oldx)+p_nrowtot)
		write(nu,'(20f11.1)')p_akey(p_abas1(p_oldx)+2:p_abas1(p_oldx)+p_nrowtot)
 
 
	endif !if(p_oldx.gt.p_nrowx)  14627
 
 
	if(p_xpresent.and.p_lx0.gt.0)then
		write(nu,*)
		do ij=1,p_lx0
			write(nu,*)'lxij',p_lx(ij)
			!!!write(16,*)'lx(),lunit,key,isch',p_lx(ij),p_lunit(p_lx(ij)),p_keys(p_lunit(p_lx(ij))),p_isch(p_lx(ij))
			write(nu,'(20f11.1)')p_a(p_abas(p_lx(ij)+p_nz)+1:p_abas(p_lx(ij)+p_nz)+p_nrow)
			write(nu,'(20f11.1)')p_aopt(p_abas1(p_lx(ij))+2:p_abas1(p_lx(ij))+p_nrowtot)
			write(nu,'(20f11.1)')p_akey(p_abas1(p_lx(ij))+2:p_abas1(p_lx(ij))+p_nrowtot)
			write(nu,'(20f11.1)')p_a(p_abas(p_lx(ij)+p_nz)+1:p_abas(p_lx(ij)+p_nz)+p_nrow)-&
				p_aopt(p_abas1(p_lx(ij))+2:p_abas1(p_lx(ij))+p_nrowtot)+p_akey(p_abas1(p_lx(ij))+2:p_abas1(p_lx(ij))+p_nrowtot)
			do j=1,p_nrow
				j_dapu3=p_a(p_abas(p_lx(ij)+p_nz)+j)-&
					p_aopt(p_abas1(p_lx(ij))+j+1)+p_akey(p_abas1(p_lx(ij))+j+1)
				if(abs(j_dapu3).gt.0.0001)then
					write(6,*)'pa-error in rint'
 
					j_err=.true.
				endif !if(abs(j_dapu3).gt.0.0001)  14649
 
			enddo !j=1,p_nrow  14646
		enddo !ij=1,p_lx0  14638
	endif !if(p_xpresent.and.p_lx0.gt.0)  14636
	if(j_err)return
	if(p_lx0.gt.0)then
 
		do ij=1,p_lx0
			if(p_x(p_lx(ij)+p_nrowz).lt.j_0.or.p_x(p_lx(ij)+p_nrowz).gt.j_1)then
				call printd()
				!!!write(16,*)'****px x',p_x(p_nrowz+p_lx(1:p_lx0)),p_lx(1:p_lx0)
				write(6,*)'****px x',p_x(p_nrowz+p_lx(1:p_lx0)),p_lx(1:p_lx0)
				j_err=.true.
				exit
			endif !if(p_x(p_lx(ij)+p_nrowz).lt.j_0.or.p_x(p_lx(ij)+p_nrowz).g  14662
		enddo !ij=1,p_lx0  14661
 
	endif !if(p_lx0.gt.0)  14659
	!if(j_err)read(5,*)fjjf
 
end subroutine













