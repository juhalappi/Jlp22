!
! J Software
!
! Copyright (C) 2022 Juha Lappi and Natural Resources Institute Finland (Luke)
! Author  Juha Lappi and Reetta Lempinen (in factory optimization)
!
!
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
 
		eps=1111.D-19;tol=1.D-13; emin=0.d0    ! was emin= 1.D0 corrected 4.5.2015 folowing the advice of Fletcher
		sgnf=1.D-4;nrep=2;npiv=3;nres=2;nfreq=nrefac;nup=0
 
		kk=0;ll=0
		kmax=0
		nm=nrow+ncol ;nmi=nm
		call  stmap(nrow,nm,kmax)
		mode=0;ifail=0
		mxm1=min(ncol+1,nrow)
		mx1=mxm1
		mxws= mxm1*(mxm1+1)/2+3*nrow+mxm1   !required by dense
		mxws=mxws+2*nrow+ncol
		if(allocated(ws))deallocate(ws)  !linux
		if(allocated(wslu1))deallocate(wslu1)  !linux
		lu1=lu1fl
		allocate( ws(1:lu1-1),wslu1(1:mxws-lu1+1))  !linux
		mxlws= nrow+mxm1+nm  !     n+mxm1+n+m
		if(allocated(lws))deallocate(lws)         !    lo
		if(allocated(lwsll1))deallocate(lwsll1)
		if(allocated(e))deallocate(e)
		if(allocated(g))deallocate(g)
		if(allocated(r))deallocate(r)
 
		ll1=ll1fl          !get stmap
		allocate(lws(1:ll1-1),lwsll1(1:mxlws-ll1+1),e(1:nm),g(1:nrow),r(1:nm))           !linux
 
		nk=0
		call start_up(nrow,nm,nmi,a,lav,nk,e,ls,wslu1,lwsll1,&
			mode,ifail)
 
		return
	end subroutine !subroutine initflet(nrow,ncol,a,la,lav,ls,lu1,ll1,ifail,nrefac)
 
	subroutine closeflet()
		use lastmod !nopre!
		if (allocated(ws))deallocate(ws)
		if(allocated(lws))deallocate(lws)
		if(allocated(e))deallocate(e)
		if(allocated(g))deallocate(g)
		if(allocated(r))deallocate(r)
		if(allocated(last))deallocate(last)
		return
	end subroutine !subroutine closeflet()
 
end module !module fletdmod2
 
!global j-variables
!module jlpdmod    !!!!
!contains   !!!!
	subroutine jlpgetcol(icol)
		use lastmod !nopre!
		jlp_acol=jlp_zero
		iel=0
		do i=jlp_lavecsp(jlp_lavecsp(0)+icol),last(icol)
			iel=iel+1
			jlp_acol(jlp_lavecsp(i))=jlp_a(iel,icol)
		end do !do i=j_lavecsp(j_lavecsp(0)+icol),last(icol)
		return
	end subroutine !subroutine j_getcol(icol)
	
	subroutine jlpsubcol(ic1,ic2,icr)   !!!!
		use lastmod !nopre!
		logical viel1,viel2

		i1=jlp_lavecsp(jlp_lavecsp(0)+ic1)
		i2=jlp_lavecsp(jlp_lavecsp(0)+ic2)
		if(i1.gt.last(ic1))then
			viel1=.false.
		else !if(i1.gt.last(ic1))then
			viel1=.true.
		end if !if(i1.gt.last(ic1))then
		if(i2.gt.last(ic2))then
			viel2=.false.
		else !if(i2.gt.last(ic2))then
			viel2=.true.
		end if !if(i2.gt.last(ic2))then
		ie=0
		ie1=0
		ie2=0

		100 if(viel2.and.(jlp_lavecsp(i2).lt.jlp_lavecsp(i1).or..not.viel1))then
			ie=ie+1
			ie2=ie2+1
			jlp_acolapu(ie)=-jlp_a(ie2,ic2)
			jlp_icolapu(ie)=jlp_lavecsp(i2)
			if(i2.eq.last(ic2))then
				viel2=.false.
				goto 200
			else !if(i2.eq.last(ic2))then
				i2=i2+1
				goto 100
			end if !if(i2.eq.last(ic2))then
		end if !if(viel2.and.(j_lavecsp(i2).lt.j_lavecsp(i1).or..not.viel1))then

	200 if(viel1.and.(jlp_lavecsp(i1).lt.jlp_lavecsp(i2).or..not.viel2))then
			ie=ie+1
			ie1=ie1+1
			jlp_acolapu(ie)=jlp_a(ie1,ic1)
			jlp_icolapu(ie)=jlp_lavecsp(i1)
			if(i1.eq.last(ic1))then
				viel1=.false.
			else !if(i1.eq.last(ic1))then
				i1=i1+1
			end if !if(i1.eq.last(ic1))then
			goto 100
		end if !if(viel1.and.(j_lavecsp(i1).lt.j_lavecsp(i2).or..not.viel2))then

300 if(viel1.and.viel2.and.jlp_lavecsp(i1).eq.jlp_lavecsp(i2))then
			ie=ie+1
			ie1=ie1+1
			ie2=ie2+1
			jlp_acolapu(ie)=jlp_a(ie1,ic1)-jlp_a(ie2,ic2)
			jlp_icolapu(ie)=jlp_lavecsp(i1)
			if(abs(jlp_acolapu(ie)).lt.jlp_tiny78)ie=ie-1
			if(i1.eq.last(ic1))then
				viel1=.false.
			else !if(i1.eq.last(ic1))then
				i1=i1+1
			end if !if(i1.eq.last(ic1))then
			if(i2.eq.last(ic2))then
				viel2=.false.
			else !if(i2.eq.last(ic2))then
				i2=i2+1
			end if !if(i2.eq.last(ic2))then
			goto 100
		end if !if(viel1.and.viel2.and.j_lavecsp(i1).eq.j_lavecsp(i2))then

		jlp_a(1:ie,icr)=jlp_acolapu(1:ie)
		last(icr)=jlp_lavecsp(jlp_lavecsp(0)+icr)+ie-1

		jlp_lavecsp(jlp_lavecsp(jlp_lavecsp(0)+icr):last(icr))=jlp_icolapu(1:ie)

	end subroutine !subroutine j_subcol(ic1,ic2,icr)

!end module !module jlpdmod
 
subroutine problem(iob,io)   !new version old version is problem2  %%jlp  !!!!

!Section problem problem() defines a Lp-problem
! An LP-problem is defined in similar way as a TEXT object//
! problem([repeatdomains->])//
! … 
! /
! endheader
! Listing
! Define a lp problem for jlp() function.
! Output:
! a problem definition object
! OOption:
! repeatdomains
! if this option is given then the same domain definition can be in several places of
! the problem definition, otherwise having the same domain in different places
! causes an error (as this is usually not what was intended). If the same domain
! definition is in several places is slightly inefficient in computations, e.g. jlp()
! function computes and prints the values of x-variables for each domain definition
! even if the same values have been computed and printed for earlier occurrence
! of the domain definition.
! The problem definition paragraph can have two types of lines: problem rows and domain rows.
! Examples of problem definitions showing the syntax.
! pr=problem() !ordinary lp-problem
! 7*z2+6*z3-z4==min
! 2*z1+6.1*z2 >2 <8+ !both lower and upper bound is possible
! (a+log(b))*z5-z8=0
! -z7+z1>8
! /
! prx=problem() ! timber management planning problem
! All:
! npv.0==max
! sitetype.eq.2: domain7:
! income.2-income.1>0
! /
! In the above example domain7 is a data variable. Unit belongs to domain if the value of the
! variable domain7 is anything else than zero.
! The objective row must be the first row. The objective must always be present. If the purpose
! is to just get a feasible solution without objective, this can be obtained by minimizing a zvariable which does not otherwise appear in the problem (remember z-> option in the jlp()
! function.
! In problems having large number of variables in a row it is possible to give the coefficients as
! a vector and variables as a list e.g.
! ESIMERKKI
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
! NNote that ‘<’ means less or equal, and ‘>’ means greater or equal. The equality is always
! part of linear programming constraints.
! The logic of jlp() function is the same as in the old JLP software. There is one difference
! which makes the life a little easier with J. In J the problem definition can use c-variables which
! are defined in the stand data. These are used similarly as if they would become from the xdata. It does not make any sense to have on a problem row only c-variables, but there can be
! constraints like
! vol#1-vol#0>0
! where vol#0 is the initial volume, i.e. a c-variable, and vol#1 is the volume during first period.
! In old JLP these initial values had to be put into the x-data.
! NNote also that problem definition rows are not in one-to-one relation to the constraint rows in
! the final lp problem. A problem definition row may belong to several domains, thus several lpconstraint rows may be generated from one problem definition row. The problem obtained by
! taking multiple domains in domain definition rows into account is called ‘expanded problem’.
! Domain definitions describe logical or arithmetic statements indicating for what management
! units the following rows apply. Problem will generate problem definition object, which is
! described below.
! Starting from J3.0 it is also possible to specify the period of the row for each row containing xvariables. The period is given between two ‘#’ signs at the beginning of the row, e.g.
! #2# income.2-income.1 >0
! If the row contains x variables from several periods, the period of the row is the last period of
! the x variables. If the period is given for some rows containing x variables, it must be given for
! all except for the objective row. The period of the objective is assumed to be the last period as
! having any other period for the objective would not make any sense. If wrong period is given
! for a row, J computes the correct solution but not as efficiently as with correct periods.
! If periods are given for rows, J is utilizing the tree structure of schedules in the optimization.
! This leads to smaller amount of additions and multiplications as the computation of the valu
! of a branch of the tree can for each node utilize the value of branch before that node.
! Unfortunately this was not more efficient e.g. in test problems with five periods.
! NNote 1: Only maximization is allowed in problems including factories. To change a
! minimization problem to a maximization problem, multiply the objective function by -1.
! *** We may later add the possibility to define also minimization problems.
! NNote 2: If optimization problem includes factories (see chapter 11.2 Optimization problem
! including factories), there have to be variables in the objective function or at least in one
! constraint row. Example of problem definition including factories can be found in chapter 11.12
! JLP Examples.
! NNote 3: An ordinary linear programming problem contains only z-variables.
! NNote 4: It is not necessary to define problem() function if the problem includes only zvariables. In jlp() function you can use zmatrix-> option instead of problem-> option.
! For more information see chapter 11.8 Solving a large problem with z-variables: jlp( ).
! NNote 5: If the problem contains harvest/area constraints for several domains, it saves memory,
! if the constraints are written in form
! harvest < area_of_domain*constant
! instead of
! (1/area_of_domain)*harvest < constant.
! The latter formulation takes the number of domains times more memory than the former
! formulation.
! endlisting
! endsection


	! domainvars%.. added as a part of the problem object Feb. 2011
	!ivdomain text for domain definitions
	!ivrow text for rows
	!irow number of rows in initial problem definition
	!nset number of rowsets
	!nsetr number of rows in each domainset
	!nsetd=nubber of domains in each domainset
	!nval number of coefficients
	!nvars number of vars in each row
	!ndom is number of different domains
	! ndoms= number of all domain sattements in the problem,
	!isetd list of all domain statements
 
	double precision, dimension(:),allocatable::rhs_   !lower bounds
	double precision, dimension(:),allocatable::rhs2_  !upper bounds
	integer, dimension(:), allocatable:: nsetd,nsetr,isetd
	integer, dimension(:), allocatable:: ivars,irowvars
	integer, dimension(:),allocatable :: nvars
	double precision, dimension(:),allocatable::coef
	double precision coe
	integer ::ivdefrow=0,ndefrow
	integer ::ivdefdomain=0,ndefdomain=0
	logical repeatdom,newprice

	!tree structure
	integer ,dimension(:), allocatable :: iperiods
	logical isperiod

	!coefficients in vector, variablese in list
	logical :: coevec_ = .false.
	logical :: varlist_ = .false.

	! nsetd tells the number of domains in each domain set
	! nsetr tells the number of rows in each row set (between two domain definitions)
	allocate(nsetd(1:500),nsetr(0:500),isetd(1:1000))
	allocate(ivars(0:1000))
	allocate(irowvars(1:5000),coef(1:5000),nvars(1:500))

	newprice=.true.
	newc=0
	inprint=0
!	write(6,*)'PROBLEM'
!	j_linkoption(iob,io,j_mdiag,clear=.true.).ge.0
	if(j_linkoption(iob,io,j_mprint).ge.0.or.j_v(j_ivprintinput).ge.3)inprint=1
	isperiod=.false.  !tree
	! ivtitle_ = 0
	! if(j_nargopt(iob,j_mtitle).eq.1)then
		! ivtitle_ = j_o(iob)%i(j_linkoption(iob,io,j_mtitle)+1)
		! if(j_otype(ivtitle_).ne.j_ipchar) then
			! j_err=.true.
			! write(6,*)'***problem: title not character variable or character constant'
			! return
		! endif !if(j_otype(ivtitle_).ne.j_ipchar) then
	! endif !if(j_nargopt(iob,j_mtitle).eq.1)then

	repeatdom=j_linkoption(iob,io,j_mrepeatdomains).ge.0
	call j_clearoption(iob,io)  ! subroutine
	!how many domains are in one set, or in one row set
	iout=j_o(iob)%i(io+2+j_o(iob)%i(io+1))
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(iout.eq.j_ivresult)then ;write(*,*)'problem must have output';goto 900 ;end if

	call j_deflistobject(iout,'%domainvars',ivdomainvars,nres=100)

	if(ivdefrow.le.0)ivdefrow=j_object('DefRows')   ! poista
	if(ivdefrow.gt.0)then
		ndefrow=j_v(ivdefrow)
	else !if(ivdefrow.gt.0)then
		ndefrow=200
	end if !if(ivdefrow.gt.0)then
	if(ivdefdomain.le.0)ivdefdomain=j_object('DefDomains') !poista
	if(ivdefdomain.gt.0)then
		ndefdomain=j_v(ivdefdomain)
	else !if(ivdefdomain.gt.0)then
		ndefdomain=100
	end if !if(ivdefdomain.gt.0)then

	if(allocated(rhs_))deallocate(rhs_,rhs2_)
	allocate(rhs_(1:ndefrow),rhs2_(1:ndefrow))
	call j_deftext(iout,'%rows',ndefrow,60*ndefrow,ivrow)   ! rows of the problem, text object
	call j_deftext(iout,'%domains',ndefdomain,80*ndefdomain,ivdomain) !domains of the problem,
	!                                                               text object
	irow=0   !number of row in the initial problem definition
	nset=0  !number of domain sets, note that on each domain row there can be several domains
	nsetr(0)=0 !number of rows in each domainset
	ndom=0  !counting separate domains, All not counted?
	ndoms=0  !counting all occurences
	ivars(0)=0
	nval=0
	nval0=0
	irowobj=0 !row where objective is
	iall=0
	1 call j_getinput('prob>',78) !)  !nprint)
! write(6,*)'<77inp:',j_linp,j_inp(1:j_linp)
	ip=j_nextlim0(j_inp,1,j_linp,':')  !Mela
	!if(index(j_inp(1:j_linp),'/').gt.0)write(6,*)'****/',j_linp,j_inp(1:j_linp),nset
	if(nset.gt.0)then
		if((ip.gt.0.or.j_inp(1:j_linp).eq.'/').and.nsetd(nset).gt.1)then   !domains change or end of problem
			!problem2
			nset0=nset
			irowbas=irow-nsetr(nset)
			do id_=2,nsetd(nset)
				nset=nset+1
				call j_puti(nsetr,nset,nsetr(nset0))
				!nsetr(nset)=nsetr(nset0)
				call j_puti(nsetd,nset,1)
				!nsetd(nset)=1
				do ir_=1,nsetr(nset0)
					call j_getline(ivrow,irowbas+ir_,j_tempchar,le_)
					irow=irow+1
					call j_puttext(ivrow,j_tempchar(1:le_))
					call j_putd(rhs_,irow,rhs_(irowbas+ir_))
					call j_putd(rhs2_,irow,rhs2_(irowbas+ir_))
					call j_puti(nvars,irow,nvars(irowbas+ir_))
				end do !do ir_=1,nsetr(nset0)
				do iv_=1,nval-nval0
					nval=nval+1
					call j_putd(coef,nval,coef(nval0+iv_))
					call j_puti(irowvars,nval,irowvars(nval0+iv_))
				enddo !do iv_=1,nval-nval0

			end do !do id_=2,nsetd(nset)
			!nsetd(nset0)=1
			call j_puti(nsetd,nset0,1)

		endif !if((ip.gt.0.or.j_inp(1:j_linp).eq.'/').and.nsetd(nset).gt.1)then

	endif !if(nset.gt.0)then

	if(j_inp(1:j_linp).ne.'/')then

		if(ip.gt.0)then   !domains
			nval0=nval
			nset=nset+1
			call j_puti( nsetr,nset,0)
			call j_puti(nsetd,nset,0)
			ial=1

			17    call j_putnewcleantext(ivdomain,j_inp(ial:ip-1),iline)  !cleantext
			if(j_err)return
	!!N		call j_compiler(j_inp(ial:ip-1),j_ivcursor2,.true.,ivdomainvars,0, &
		!		j_matrix0,j_matrix0,j_matrix0,.false.)
			if(j_err)then
				write(6,*)'*problem: illegal domain definition'
				return
			endif !if(j_err)then
				! return
			! endif !if(j_err)then
			! using same transform as used in jcompil to work with loops/if then
			if(iline.gt.ndom)then  ! new domain definition ?
				ndom=iline
			else if(.not.repeatdom)then !if(iline.gt.ndom)then
				write(6,*)'**problem: domain ',j_inp(ial:ip-1), ' repeated without repeatdomains->'
				j_err=.true.
				return
			endif !if(iline.gt.ndom)then
			ndoms=ndoms+1  !domain occurences
			call j_puti(isetd,ndoms,iline)
			call j_puti(nsetd,nset,nsetd(nset)+1)
			ial=ip+1
			if(ial.gt.j_linp)goto 1
			ip=j_nextlim0(j_inp,ial,j_linp,':') !Mela
			if(ip.gt.ial)then !
				goto 17
			else !if(ip.gt.ial)then
				write(*,*)'not legal domainrow:',j_inp(1:j_linp)
				j_err=.true.
				goto 900
			end if !if(ip.gt.ial)then
		else !if(ip.gt.0)then
			! starts without domains
		!	write(6,*)'<545'
			if(irow.le.0)then
				nset=1;   nsetr(nset)=0 ;nsetd(nset)=1
				call j_puttext(ivdomain,'All') !cleantext caannot be used
				call j_getobject(0,'All',j_ipreal,ivall)
				j_v(ivall)=1.
				ndoms=1
				isetd(ndoms)=1
				iall=1          !

			end if !if(irow.le.0)then
			irow=irow+1
			call j_puti(nsetr,nset,nsetr(nset)+1)
			ipe=j_nextlim0(j_inp,1,j_linp,'=')
			if(j_inp(ipe+1:ipe+1).eq.'=')ipe=0
			ip=ipe
			if(ipe.gt.0)then
				call j_putd(rhs_,irow,j_val(j_inp(ipe+1:j_linp)) ) ! j_err
				call j_putd(rhs2_,irow,rhs_(irow) )

			else !if(ipe.gt.0)then
				ipg=j_nextlim0(j_inp,1,j_linp,'>')
				ipl=j_nextlim0(j_inp,1,j_linp,'<')
				if(ipg+ipl.gt.0)then
					! this section is rather comlicated, but it read the rhs_ or rhs2_ (upper bound) when either or
					!  both are given, and if both are given they can (hopefully) be in any order
					if(ipg.gt.0)then
						lu=j_linp;if(ipl.gt.ipg)lu=ipl-1
						call j_putd( rhs_,irow,j_val(j_inp(ipg+1:lu)) )     !val gives numeric value of a expression
						if(j_err)then  ! j_err coming from val
							write(6,*)'*problem: error in interpreting rhs ',j_inp(ipg+1:lu)
							return
						endif !if(j_err)then
					else !if(ipg.gt.0)then
						call j_putd(rhs_,irow,-huge(1.d0) )
					end if !if(ipg.gt.0)then
					if(ipl.gt.0)then
						lu=j_linp;if(ipg.gt.ipl)lu=ipg-1

						call j_putd(rhs2_,irow,j_val(j_inp(ipl+1:lu)) )
				!		write(6,*)'<rhs2',ipl,ipg,j_inp(ipl+1:lu),j_val(j_inp(ipl+1:lu))
						if(j_err)then  ! j_err coming from val
							write(6,*)'*problem: error in interpreting rhs2 ',j_inp(ipl+1:lu)
							return
						endif !if(j_err)then

					else !if(ipl.gt.0)then
						call j_putd(rhs2_,irow,huge(1.d0))
					end if !if(ipl.gt.0)then
					if(ipg.gt.0.and.ipl.gt.0)then
						ip=min(ipg,ipl)
					else !if(ipg.gt.0.and.ipl.gt.0)then
						ip=ipg+ipl
					end if !if(ipg.gt.0.and.ipl.gt.0)then

				else !if(ipg+ipl.gt.0)then
					ipmin=index(j_inp(1:j_linp+1),'==min')
					if(ipmin.gt.0)then
						call j_putd( rhs_,irow,0.d0)
						call j_putd(rhs2_,irow,-huge(1.d0))
						ip=ipmin
						irowobj=irow
					else !if(ipmin.gt.0)then
						ipmax=index(j_inp(1:j_linp+1),'==max')
						if(ipmax.le.0)then
						write(6,*)
							write(*,*)'***not legal row, < > or = missing '
							write(6,*)j_inp(1:j_linp)
							j_err=.true.
							goto 900
						end if !if(ipmax.le.0)then
						call j_putd( rhs2_,irow,0.d0)
						call j_putd( rhs_,irow,huge(1.d0))
						irowobj=irow
						ip=ipmax
					end if !if(ipmin.gt.0)then

				end if !if(ipg+ipl.gt.0)then
			end if !if(ipe.gt.0)then
			66    call j_clean(j_inp(1:ip-1),le)   !remove blanks etc.
			if(j_err)write(6,*)'<36366ertas'
			if(j_err)return
			!tree
			if(j_inp(1:1).eq.'#')then
				write(6,*)'#rivi:',j_inp(1:le)
				iendp_=j_nextlim0(j_inp,2,le,'#')
				if(iendp_.le.0.or.iendp_.eq.le.or.iendp_.eq.2)then
					write(6,*)'*problem*, illegal row:',j_inp(1:le)
					j_err=.true.
					return
				else !if(iendp_.le.0.or.iendp_.eq.le.or.iendp_.eq.2)then
					iperiod_=j_val(j_inp(2:iendp_-1))
					if(iperiod_.le.0.or.j_err)then
						write(6,*)'*problem*, illegal period in ',j_inp(1:le)
						j_err=.true.
						return
					endif !if(iperiod_.le.0.or.j_err)then

				endif !if(iendp_.le.0.or.iendp_.eq.le.or.iendp_.eq.2)then

				ialrow=iendp_+1
				if(.not.allocated(iperiods))then
					allocate(iperiods(1:500))
					iperiods=0
				endif !if(.not.allocated(iperiods))then
				If(.not.isperiod)iperiods=0
				isperiod=.true.
				call j_puti(iperiods,irow,iperiod_)
			else !if(j_inp(1:1).eq.'#')then
				ialrow=1
			endif !if(j_inp(1:1).eq.'#')then

			call j_puttext(ivrow,j_inp(ialrow:le)) ! put row text into the 'rows%'...  object.
			! interpret:
			call j_puti(nvars,irow,0)
			ial=1
			!  -x+3*x+z-3*y  x+4*c-   -2*g   !ial is at + or -1 or intially
			18    continue
			! this section is reading the coefficient, which can start with '-' or '+' or the first
			! cofficient can be without sign. And if coefficient is not given, it get value 1.

			coevec_ = .false.
			ial7=ial
			if(j_inp(ial:ial).eq.'-')ial7=ial+1
			if(j_inp(ial7:ial7).eq.'(')then
				irp= j_nextrp(j_inp,ial7,le)
				if(irp.eq.ial7+1.or.irp.ge.le.or.j_inp(irp+1:irp+1).ne.'*')then
					write(6,*)'**illegal syntax in problem' ;j_err=.true.  ;return
				endif !if(irp.eq.ial7+1.or.irp.ge.le.or.j_inp(irp+1:irp+1).ne.'*')then
				!#20181108 check if the coefficient is vector
				ivcoe_ = j_object(j_inp(ial7+1:irp-1))
				if(ivcoe_.gt.0)then

					if(j_otype(ivcoe_).eq.j_ipreal)then
						coe=j_v(ivcoe_)
					elseif(j_otype(ivcoe_).eq.j_ipmatrix)then !if(j_otype(ivcoe_).eq.j_ipreal)then
						coevec_=.true.
					else !if(j_otype(ivcoe_).eq.j_ipreal)then
						call j_printname('coefficient ',ivcoe_,&
							'is not REAL or MATRIX')
						j_err=.true.
						return
					endif !if(j_otype(ivcoe_).eq.j_ipreal)then

				else !if(ivcoe_.gt.0)then
					write(6,*)'<444>',j_inp(ial1:ine-1)
					coe=j_val(j_inp(ial1:ine-1))
					if(j_err)then
						write(6,*)'*problem, error in interpreting coefficient: ',j_inp(ial1:ine-1)
						return
					endif !if(j_err)then
				endif !if(ivcoe_.gt.0)then
				if(ial7.ne.ial)coe=-coe
				ial=irp+2
				ine2=j_nextlim(j_inp,ial+1,le,'+-')
				if(j_err)return
			else !if(j_inp(ial7:ial7).eq.'(')then
				ine=j_nextlim(j_inp,ial,le,'*')
				if(j_err)return
				ine2=j_nextlim(j_inp,ial+1,le,'+-')
				if(j_err)return

				if(ine.lt.ine2)then   !explicit coefficient
					ial1=ial;if(j_inp(ial:ial).eq.'+')ial1=ial+1  ! bypass '+'
					!#20181108 check if the coefficient is vector
					ivcoe_ = j_object(j_inp(ial1:ine-1))

					if(ivcoe_.gt.0)then

						if(j_otype(ivcoe_).eq.j_ipreal)then
							coe=j_v(ivcoe_)
						elseif(j_otype(ivcoe_).eq.j_ipmatrix)then !if(j_otype(ivcoe_).eq.j_ipreal)then
							coevec_=.true.
						else !if(j_otype(ivcoe_).eq.j_ipreal)then
							call j_printname('coefficient ',ivcoe_,&
								'is not REAL or MATRIX')
							j_err=.true.
							return
						endif !if(j_otype(ivcoe_).eq.j_ipreal)then

					else !if(ivcoe_.gt.0)then

						coe=j_val(j_inp(ial1:ine-1))
						if(j_err)then
							write(6,*)'*problem, error in interpreting coefficient: ',j_inp(ial1:ine-1)
							return
						endif !if(j_err)then
					endif !if(ivcoe_.gt.0)then


					ial=ine+1
				else if(j_inp(ial:ial).eq.'-')then !if(ine.lt.ine2)then
					coe=-1.
					ial=ial+1
				else !if(ine.lt.ine2)then
					coe=1.
					if(j_inp(ial:ial).eq.'+') ial=ial+1
				end if !if(ine.lt.ine2)then
			endif !if(j_inp(ial7:ial7).eq.'(')then

			nexli=j_nextlim(j_inp,ial,ine2-1,'!"/()]?*<>:,;')
			if(j_err)return
			if(nexli.le.ine2-1)then
				call j_printname('*illegal syntax in: ',iout,'=problem()')
				write(6,*)'**illegal variable name:',j_inp(ial:ine2-1)
				j_err=.true.
				return

			endif !if(nexli.le.ine2-1)then

			varlist_ = .false.
			lenlist_ = 1
			!#20181109 variables in list
			ivlist_ = j_object(j_inp(ial:ine2-1))
			if(ivlist_ > 0) varlist_ = j_otype(ivlist_) == j_iplist
			if(varlist_) then
				lenlist_ = j_lenlist(ivlist_)
				if(lenlist_ == 0) then
					write(6,*)'**illegal  empty list in problem' ;j_err=.true.  ;return
				endif !if(lenlist_ == 0) then
				! check the compability of the coefficient vector and  the variable list
				if(coevec_) then
					nrows_ = j_o(ivcoe_)%i(1)
					ncols_ = j_o(ivcoe_)%i(2)
					if(lenlist_ /= nrows_ .or. ncols_ /= 1) then
						write(6,*)'**illegal vector*list in problem' ;j_err=.true.  ;return
					endif !if(lenlist_ /= nrows_ .or. ncols_ /= 1) then
				endif !if(coevec_) then
			endif !if(varlist_) then
			! Add varibles from list and coefficients from vector to the problem
			do i_=1,lenlist_
				if (varlist_) then
					ivout = j_o(ivlist_)%i(i_)
					if(j_otype(ivout) /= j_ipreal) then
						write(6,*)'**illegal variable type found in list in problem' ;j_err=.true.  ;return
					endif !if(j_otype(ivout) /= j_ipreal) then
					if (coevec_) coe = j_o(ivcoe_)%d(i_)
				else !if (varlist_) then
					call j_getobject(0,j_inp(ial:ine2-1),j_ipreal,ivout);if(j_err)return
				endif !if (varlist_) then
				iperk= j_putlist(ivout,ivars)
				nval=nval+1
				call j_putd(coef,nval,coe);call j_puti(irowvars,nval,ivout)
				call j_puti(nvars,irow,nvars(irow)+1)
			enddo !do i_=1,lenlist_

			!#20181109 commented
			!call j_getobject(0,j_inp(ial:ine2-1),j_ipreal,ivout);if(j_err)return
			!iperk= j_putlist(ivout,ivars)
			!nval=nval+1
			!call j_putr(coef,nval,coe);call j_puti(irowvars,nval,ivout)
			!call j_puti(nvars,irow,nvars(irow)+1)

			ial=ine2
			!write(6,*)'<23454ial,le',ial,le
			if(ial.lt.le)goto 18

			goto 1
		end if !if(ip.gt.0)then
	end if !if(j_inp(1:j_linp).ne.'/')then
	if(irowobj.ne.1)then
		write(6,*)'Object row must be first row'
		write(6,*)'irowobj',irowobj
		j_err=.true.
		return

	endif !if(irowobj.ne.1)then
	call j_defmatrix(iout,'%rhs',irow,1,j_matreg,ivrhs)
	call j_defmatrix(iout,'%rhs2',irow,1,j_matreg,ivrhs2)
	j_o(ivrhs)%d(1:irow)=rhs_(1:irow);j_o(ivrhs2)%d(1:irow)=rhs2_(1:irow)
	call j_deflistobject(iout,'%vars',ivvars,list0=ivars(0),list=ivars(1:ivars(0)))
!	j_o(ivvars)%i(0:ivars(0))=ivars(0:ivars(0))

	!tree
	if(isperiod)then

		call j_defmatrix(iout,'%period',irow,1,j_matreg,j_ivperiod)
		j_o(j_ivperiod)%d(1:irow)=iperiods(1:irow)
		deallocate(iperiods)
	else !if(isperiod)then
		j_ivperiod=0

	endif !if(isperiod)then

	call j_getobject(iout,' ',j_ipproblem,ivout)   !miksei iout käy suoraan, kunhan typen määrittelsi?
	ih= 14                  !6+nset+1+nset+1+ndoms+irow+nval+ivdomainvars
	ih2=2*nset+ndoms+irow+nval+nval
	allocate(j_o(ivout)%i(1:ih))
	allocate(j_o(ivout)%i2(1:ih2))
	allocate(j_o(ivout)%d(1:nval))
	!ivdomain text for all domain definitions
	!ivrow text for rows
	!irow number of rows in initila problem definition
	!nset number of rowsets
	!nsetr number of rows in each domainset
	!nsetd=nubber of domains in each domainset
	!nval number of coefficients
	!nvars number of vars in each row
	!ndom is number of different domains
	! ndoms= number of all domain sattements in the problem,
	!isetd list of all domain statements
	! ivdomainvars  list object containg all variables in domain definitions

	!Problem-object
	! - h(1) : alarojen vektorin indeksi
	! - h(2) : ylärajojen vektorin indeksi
	! - h(3) : domain-listan indeksi
	! - h(4) : (?) tehtävän rivien tekstiolion indeksi
	! - h(5), h(6),h(7), : kts yllä kommenteista
	! - h(8) : tehtävässä olevien muuttujien listan indeksi
	! - h(9), h(10) : kts yllä kommenteista
	! - h(12) : tavoitefunktion rivi tehtävämäärittelyssä
	! - h(13) : kts yllä kommenteista
	j_o(ivout)%i(1)=ivrhs;j_o(ivout)%i(2)=ivrhs2;j_o(ivout)%i(3)=ivdomain;j_o(ivout)%i(4)=ivrow
	j_o(ivout)%i(5)=irow; j_o(ivout)%i(6)=nset;j_o(ivout)%i(7)=ndoms;j_o(ivout)%i(8)=ivvars
	j_o(ivout)%i(9)=nval ;j_o(ivout)%i(10)=ndom  !o(ivout)%i(11)=nrow
	j_o(ivout)%i(12)=irowobj
	j_o(ivout)%i(13)=ivdomainvars

	j_o(ivout)%i(14)=j_ivperiod
!	j_otitle(ivout)=ivtitle_

	!nsetr number of rows for each domain, domain 0 is nothing
	! nsetd =number of doamins in each set
	j_o(ivout)%i2(1:nset)=nsetr(1:nset)
	j_o(ivout)%i2(nset+1:2*nset)=nsetd(1:nset)
	j_o(ivout)%i2(2*nset+1:2*nset+ndoms)=isetd(1:ndoms)
	j_o(ivout)%i2(2*nset+ndoms+1:2*nset+ndoms+irow)=nvars(1:irow)
	j_o(ivout)%i2(2*nset+ndoms+irow+1:2*nset+ndoms+irow+nval)=irowvars(1:nval)
	j_o(ivout)%d(1:nval)=coef(1:nval)

	nrow=nsetr(0)
	do i=1,nset
		nrow=nrow+nsetd(i)*nsetr(i)
	end do !do i=1,nset
	j_o(ivout)%i(11)=nrow
	!write(6,*)'<prob22>,nsetd',nsetd(1:nset),nsetr(1:nset)
	!stop 666
	!now each set contains only one domain

	write(6,*)'total number of rows',nrow
	write(6,*)' '

	900 continue  !after rhs

	deallocate(nsetd,nsetr,isetd,ivars,irowvars,coef,nvars)
	deallocate(rhs_,stat=istat)
	deallocate(rhs2_,stat=istat)
!	io=io+j_o(iob)%i(io+1)+3
	call j_clearoption(iob,io)  ! subroutine
!	write(6,*)'proble,ret*******************'
	return
end subroutine !subroutine problem(iob,io)
 
 
 
 
!module jlpmod        !!!!

!	use lastmod    !nopre!   is in fletecher

!	integer,dimension(:),allocatable:: j_memobs ! mobs(iobs) tells the place of


!	contains    !!!!
		subroutine jlpcurix(iuni)   !!!!
		!determines for each row if the unit iunit belonggs to the domain of the row
		!matrix  domainbits(number,mxunit) contains bits for telling if unit belongs to certain domain
		jlp_nrowp=0
		if(jlp_ndom.le.0)then
			do jcurix=0,jlp_nrow;jlp_ixcur(jcurix)=jlp_ix(jcurix)
				if(jlp_ix(jcurix).ne.0)then
					jlp_nrowp=jlp_nrowp+1
					jlp_ixcurrows(jlp_nrowp)=jcurix
				endif !if(j_ix(jcurix).ne.0)then
			enddo
			return !do jcurix=0,j_nrow;j_ixcur(jcurix)=j_ix(jcurix)
		endif !if(j_ndom.le.0)then

		do jcurix=0,jlp_nrow
			if(jlp_ix(jcurix).gt.0)then
				icurint=(jlp_irowdomain(jcurix)-1)/32+1  !integer part
				icurbit=jlp_irowdomain(jcurix)-(icurint-1)*32-1  !bit part
				if(btest(jlp_domainbits(icurint,iuni),icurbit))then
					jlp_ixcur(jcurix)=jlp_ix(jcurix)
					jlp_nrowp=jlp_nrowp+1
					jlp_ixcurrows(jlp_nrowp)=jcurix
				else !if(btest(j_domainbits(icurint,iuni),icurbit))then
					jlp_ixcur(jcurix)=0
				endif !if(btest(j_domainbits(icurint,iuni),icurbit))then
			else !if(j_ix(jcurix).gt.0)then
				jlp_ixcur(jcurix)=0
			endif !if(j_ix(jcurix).gt.0)then
		enddo !do jcurix=0,j_nrow
	end subroutine !subroutine j_curix(iuni)
	
	subroutine jlpfcurix(iuni)   !!!!

		! integer function jxmatiba(iobs,isaa)  !subroutine
		! integer isaa,iobs
		! if(j_memobs(iobs).le.j_xmatlast)then
			! jxmatiba=(j_memobs(iobs)-1)*j_ntemp0
			! ! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,3,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
			! ! if(j_feasible.or.feas)jxmatiba=100000000
			! return
		! endif
		! if(j_memobs(iobs).le.j_xmatlast2.and..false.)then  !from upper buffer
			! jxmatiba=j_xmatibas2+(j_memobs(iobs)-j_xmatlopp1)*j_ntemp0
			! ! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,3,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
			! ! if(j_feasible.or.feas)jxmatiba=100000000
			! return
		! endif
		! if(isaa.eq.1)then
			! jxmatiba=j_xmatekabas
			! if(j_xmatekaobs.eq.iobs.and..false.)return
			! j_xmatekaobs=iobs
		! else
			! jxmatiba=j_xmattokabas
			! if(j_xmattokaobs.eq.iobs.and..false.)return
			! j_xmattokaobs=iobs
		! endif
		! read(j_xmatnu,rec=iobs)j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
		! ! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,isaa,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
		! ! if(j_feasible)feas=.true.

		! return
	! end function   !end subroutine


		! integer function jxmatiba(iobs,isaa)
		! integer isaa,iobs

		!read(j_xmatnu,rec=iobs)xr1,xr2
		! if(j_memobs(iobs).le.j_xmatlast)then
			! jxmatiba=(j_memobs(iobs)-1)*j_ntemp0
			! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,3,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
			! if(j_feasible.or.feas)jxmatiba=100000000
			! goto 754
			! ! return
		! ! endif
		! ! if(iobs.ge.j_xmatfirst2.and.iobs.le.j_xmatlast2)then  !from upper buffer note
			! ! jxmatiba=j_xmatibas2+(iobs-j_xmatfirst2)*j_ntemp0
			! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,3,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
			! if(j_feasible.or.feas)jxmatiba=100000000
			! goto 754
			! return
		! endif
		! if(isaa.eq.1)then
			! jxmatiba=j_xmatekabas
			!if(j_xmatekaobs.eq.iobs)goto 754
			! if(j_xmatekaobs.eq.iobs)return
			! j_xmatekaobs=iobs

		! else
			! jxmatiba=j_xmattokabas
			!if(j_xmattokaobs.eq.iobs)goto 754
			! if(j_xmattokaobs.eq.iobs)return
			! j_xmattokaobs=iobs

		! endif
		! read(j_xmatnu,rec=iobs)j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
		! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,isaa,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
		! if(j_feasible)feas=.true.
		! ! return
! 754    if(xr1.eq.j_xmat(jxmatiba+1).and.xr2.eq.j_xmat(jxmatiba+2))return
! !write(6,*)iobs,isaa,j_memobs(iobs),j_xmatekaobs,j_xmattokaobs,xr1,j_xmat(jxmatiba+1),xr2,j_xmat(jxmatiba+2)
! stop 765
		! return
	! end function





		!determines for each row if the unit iunit belonggs to the domain of the row
		!matrix  domainbits(number,mxunit) contains bits for telling if unit belongs to certain domain
 
		! nfxfrow : montako tehdasmuuttujaa rivillä on
		! ifxcurrows : millä riveillä tehdasmuuttujia on
		jlp_nrowpf=0
		if(jlp_ndom.le.0)then
			do jcurix=0,jlp_nrow
				jlp_ixcurfact(jcurix)=.false.
				if(jlp_nfxrow(jlp_irowrow(jcurix)).ne.0)then  !j_irowrow(jcurix)=jcurix+-1
					jlp_nrowpf=jlp_nrowpf+1
					jlp_ifxcurrows(jlp_nrowpf)=jcurix
					jlp_ixcurfact(jcurix)=.true.
				endif !if(j_nfxrow(j_irowrow(jcurix)).ne.0)then
			enddo;return !do jcurix=0,j_nrow
		endif !if(j_ndom.le.0)then

		! fdomain
		do jcurix=0,jlp_nrow
			jlp_ixcurfact(jcurix)=.false.
			if(jlp_nfxrow(jlp_irowrow(jcurix)).ne.0)then
				icurint=(jlp_irowdomain(jcurix)-1)/32+1  !integer part
				icurbit=jlp_irowdomain(jcurix)-(icurint-1)*32-1  !bit part
				if(btest(jlp_domainbits(icurint,iuni),icurbit))then
					jlp_nrowpf=jlp_nrowpf+1
					jlp_ifxcurrows(jlp_nrowpf)=jcurix
					jlp_ixcurfact(jcurix)=.true.
				endif !if(btest(j_domainbits(icurint,iuni),icurbit))then
			endif !if(j_nfxrow(j_irowrow(jcurix)).ne.0)then
		enddo !do jcurix=0,j_nrow
	end subroutine !subroutine j_fcurix(iuni)
	subroutine jlpfcurixy(iuni)  !!!!

		!determines for each row if the unit iunit belonggs to the domain of the row
		!matrix  domainbits(number,mxunit) contains bits for telling if unit belongs to certain domain
 
		! nfyfrow : montako tehdasmuuttujaa rivillä on
		! ifycurrows : millä riveillä tehdasmuuttujia on
		jlp_nrowpfy=0
		if(jlp_ndom.le.0)then
			do jcurix=0,jlp_nrow
				if(jlp_nfyrow(jlp_irowrow(jcurix)).ne.0)then
					jlp_nrowpfy=jlp_nrowpfy+1
					jlp_ifycurrows(jlp_nrowpfy)=jcurix
					jlp_ixcurfact(jcurix)=.true.
				endif !if(j_nfyrow(j_irowrow(jcurix)).ne.0)then
			enddo
			return !do jcurix=0,j_nrow
		endif !if(j_ndom.le.0)then
!		write(6,*)'<88888 ',jlp_nrowpfy
		! fdomain
		do jcurix=0,jlp_nrow
			if(jlp_nfyrow(jlp_irowrow(jcurix)).ne.0)then
				icurint=(jlp_irowdomain(jcurix)-1)/32+1  !integer part
				icurbit=jlp_irowdomain(jcurix)-(icurint-1)*32-1  !bit part
				if(btest(jlp_domainbits(icurint,iuni),icurbit))then
					jlp_nrowpfy=jlp_nrowpfy+1
					jlp_ifycurrows(jlp_nrowpfy)=jcurix
					jlp_ixcurfact(jcurix)=.true.
				endif !if(btest(j_domainbits(icurint,iuni),icurbit))then
			endif !if(j_nfyrow(j_irowrow(jcurix)).ne.0)then
		enddo !do jcurix=0,j_nrow
	end subroutine !subroutine j_fcurixy(iuni)
	
	subroutine jlpfirow2curix(iuni)   !!!!

		! muodostaa taulukon, jonka perusteella tiedetään mitkä tehdasmuuttujia sisältävät
		! lavennetut tehtävärivit vastaavat kutakin alkuperäistä tehtäväriviä
		! irow2curix(0, alkup_rivi,)= #lavennetut rivit
		! irow2curix(i, alkup_rivi)= i:nnen lavennetun rivin numero
 
		jlp_irow2curix(0,:) = 0
		if(jlp_ndom.le.0)then
			do jcurix=0,jlp_nrow
				irow_ = jlp_irowrow(jcurix)  !irow_jcurix+1
				if((jlp_nfxrow(irow_).ne.0).or.(jlp_nfyrow(irow_).ne.0))then
					jlp_irow2curix(0,irow_)=jlp_irow2curix(0,irow_)+1
					jlp_irow2curix(jlp_irow2curix(0,irow_),irow_)=jcurix
				endif !if((j_nfxrow(irow_).ne.0).or.(j_nfyrow(irow_).ne.0))then
			enddo !do jcurix=0,j_nrow
			return !do jcurix=0,j_nrow
		endif !if(j_ndom.le.0)then


		!	 fdomain
		do jcurix=0,jlp_nrow
			irow_ = jlp_irowrow(jcurix)
		!	write(6,*)'<33>',jcurix,irow_
			if((jlp_nfxrow(irow_).ne.0).or.(jlp_nfyrow(irow_).ne.0))then
				icurint=(jlp_irowdomain(jcurix)-1)/32+1  !integer part
				icurbit=jlp_irowdomain(jcurix)-(icurint-1)*32-1  !bit part
				if(btest(jlp_domainbits(icurint,iuni),icurbit))then
					jlp_irow2curix(0,irow_)=jlp_irow2curix(0,irow_)+1
					jlp_irow2curix(jlp_irow2curix(0,irow_),irow_)=jcurix
				endif !if(btest(j_domainbits(icurint,iuni),icurbit))then
			endif !if((j_nfxrow(irow_).ne.0).or.(j_nfyrow(irow_).ne.0))then
		enddo !do jcurix=0,j_nrow

	end subroutine !subroutine j_firow2curix(iuni)

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
end !subroutine jlplex(list,i1,i2,listi)
 
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
end !subroutine lexf(list,i1,i2,listi,imin,imax)
 
subroutine printrowinfo(ir)   !!!!
	!character*5 chi5 comes now from getmod
	if(jlp_ndom.gt.0)then
		jlp_buf='DOMAIN:'
		idom=jlp_irowdomain(ir)
		call j_getline(jlp_ivdomain,idom,jlp_buf(8:),le)
		jlp_buf(74:78)='units'
		jlp_buf(68:72)=j_chi5(jlp_domainunits(idom),0)
		write(6,'(a)')jlp_buf(1:72)
	endif !if(j_ndom.gt.0)then
	!for each expanded problem
	! irowrow tells the the initial row in the nonexpanded problem
	call j_getline(jlp_ivrow,jlp_irowrow(ir),jlp_buf,le)
	write(6,'(a)')jlp_buf(1:le)

	return
end !subroutine printrowinfo(ir)
 
 
 
subroutine domain(iob,io)   !!!!
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	j_v(iout)= j_line2(jlp_ivdomain,j_o(iob)%i(io+2))
!	io=io+narg+3
	return
end !subroutine domain(iob,io)
 
subroutine priceunit(iob,io)  !!!!
!jxmatiba(iobs)=(iobs-1)*j_ntemp0
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**priceunit: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then

	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iunit=j_v( j_o(iob)%i(io+2) )
	if(iunit.gt.jlp_nunits.or.iunit.le.0)then
		write(6,*)'**illegal unit in price%unit ',iunit,' max is',jlp_nunits
		j_err=.true. ;return
	endif !if(iunit.gt.jlp_nunits.or.iunit.le.0)then
	call jlpcurix(iunit) !!determines for each row if the unit iunit belonggs to the domain of the row                          
	! returns nrowp,ixcurrows
	jlp_value=0.
	iobs=jlp_ibaunit(iunit)+jlp_keys(iunit)
	ibax=(iobs-1)*jlp_ntemp0  !jxmatiba(iobs) !,1)
	do jj=1,jlp_nrowp
		j=jlp_ixcurrows(jj)
		!	j_value=j_value+j_vx(j)*j_xmat(j_ix(j),iobs) !v(ix(j))
		jlp_value=jlp_value+jlp_vx(j)*jlp_xmat(ibax+jlp_ix(j)) !v(ix(j))
	enddo !do jj=1,j_nrowp
	j_v(iout)=jlp_value

!	io=io+narg+3
	return
end subroutine !subroutine priceunit(iob,io)
 
subroutine priceschedw(iob,io)   !!!!
 
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**priceschedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iunit=j_v( j_o(iob)%i(io+2) )
	is=j_v( j_o(iob)%i(io+3) )

	if(iunit.gt.jlp_nunits.or.iunit.le.0)then
		write(6,*)'**illegal unit in price%schedw ',iunit,' max is',jlp_nunits
		j_err=.true. ;return
	endif !if(iunit.gt.jlp_nunits.or.iunit.le.0)then
	if(is.gt.jlp_nsch(iunit).or.is.le.0)then
		write(6,*)'**illegal sched ',is,'  in price%schedw for unit ',iunit, 'max=',jlp_nsch(iunit)
		j_err=.true. ;return
	endif !if(is.gt.j_nsch(iunit).or.is.le.0)then
	call jlpcurix(iunit)
	jlp_value=0.
	iobs=jlp_ibaunit(iunit)+is
	ibax=(iobs-1)*jlp_ntemp0  !jxmatiba(iobs) ! ,1)
	do jj=1,jlp_nrowp
		j=jlp_ixcurrows(jj)
		jlp_value=jlp_value+jlp_vx(j)*jlp_xmat(ibax+jlp_ix(j)) ! ,iobs) !v(ix(j))
	enddo !do jj=1,j_nrowp
	j_v(iout)=jlp_value

!	io=io+narg+3
	return
end !subroutine priceschedw(iob,io)
 
subroutine priceschedcum(iob,io)   !!!!
 
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**priceschedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	is=j_v( j_o(iob)%i(io+2) )
	if(is.gt.jlp_lopp.or.is.le.0)then
		write(6,*)'**illegal sched in price%schedcum: ',is,' max is',jlp_lopp
		j_err=.true. ;return
	endif !if(is.gt.j_lopp.or.is.le.0)then
	ibax=(is-1)*jlp_ntemp0  !jxmatiba(is) !,1)
	call jlpcurix(iunit)
	jlp_value=0.
	do jj=1,jlp_nrowp
		j=jlp_ixcurrows(jj)
		!j_value=j_value+j_vx(j)*j_xmat(j_ix(j),is) !,is) !v(ix(j))
		jlp_value=jlp_value+jlp_vx(j)*jlp_xmat(ibax+jlp_ix(j)) !,is) !v(ix(j))
	enddo !do jj=1,j_nrowp
	j_v(iout)=jlp_value

!	io=io+narg+3
	return
end subroutine !subroutine priceschedcum(iob,io)
 
 
subroutine weights(iob,io)   !!!!
! Section weights weigts() weights of schdules
! TO BE RAPORTED LATER
! endsection
 
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**weights: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	! if narg=1 then total number of weights, if narg=2 then weight in unit
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(narg.eq.0)then
		j_v(iout)=jlp_nunits+jlp_ld0
	else !if(narg.eq.0)then
		iunit=j_o(iob)%i(io+2)
		ns=0
		do i=1,jlp_ndiv
			if(jlp_iunitdiv(i).eq.iunit)then
				ns=ns+1
			elseif(jlp_iunitdiv(i).gt.iunit)then !if(j_iunitdiv(i).eq.iunit)then
				goto 77
			endif !if(j_iunitdiv(i).eq.iunit)then
		end do !do i=1,j_ndiv
		ns=1
		!
		77  j_v(iout)=ns
	end if !if(narg.eq.0)then
!	io=io+j_o(iob)%i(io+1)+3
	return
end !subroutine weights(iob,io)
 
function nweights()  !number of nonzero weights tehty Markulle  !!!!
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**nweights: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then

	nweights=jlp_nunits+jlp_ld0

	return
end !function nweights()
 
 
subroutine partweights(iob,io)   !!!!
! Section partweights partweight() weights of split schedules
! TO BE RAPORTED LATER
! endsection
 
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**partweights: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	! if narg=1 then total number of weights, if narg=2 then weight in unit
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)

	if(narg.eq.0)then
		j_v(iout)=jlp_ndiv
	else !if(narg.eq.0)then
		iunit=j_o(iob)%i(io+2)
		ns=0
		do i=1,jlp_ndiv
			if(jlp_iunitdiv(i).eq.iunit)then
				ns=ns+1
			endif !if(j_iunitdiv(i).eq.iunit)then
		enddo !do i=1,j_ndiv
		77  j_v(iout)=ns
	endif !if(narg.eq.0)then
!	io=io+narg+3
	return
end !subroutine partweights(iob,io)
 
 
!unit
subroutine jlpunit(iob,io)   !!!!
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**unit: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	ibas=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted
	if(ibas.le.0.or.ibas.gt.jlp_nunits+jlp_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access weight',ibas, 'maximum is:',jlp_nunits+jlp_ndiv
	else !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ndiv)then
		! bas numbering: for each unit count the number of parts, subtract one
		! add number of units
		niv=0 ! count of (numb of parts-1)
		iunitv=-1

		do j=1,jlp_ndiv
			! ib=jlp_nunits before
			iunit=jlp_iunitdiv(j) !o(isol)%i(jlp_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			! write(6,*)'j,unit,niv',j,iunit,niv'
			ib=iunit+niv
			if(ib.ge.ibas)then
				iunit=iunit-(ib-ibas)
				goto 77
			endif !if(ib.ge.ibas)then
			iunitv=iunit
		enddo !do j=1,j_ndiv
		! all
		iunit=ibas-niv
		77  j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=iunit

	endif !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ndiv)then
!	90 io=io+j_o(iob)%i(io+1)+3
	return
end !subroutine jlpunit(iob,io)
 
function lunit_(ibas)    !!!!
 
	integer ibas ! for which the unit is wanted
	! bas numbering: for each unit count the number of parts, subtract one
	! add number of units

	niv=0 ! count of (numb of parts-1)
	iunitv=-1

	do j=1,jlp_ndiv
		! ib=jlp_nunits before
		iunit=jlp_iunitdiv(j) !o(isol)%i(jlp_nunits+j)
		if(iunit.eq.iunitv)niv=niv+1
		! write(6,*)'j,unit,niv',j,iunit,niv'
		ib=iunit+niv
		if(ib.ge.ibas)then
			iunit=iunit-(ib-ibas)
			goto 77
		endif !if(ib.ge.ibas)then
		iunitv=iunit
	enddo !do j=1,j_ndiv
	! all
	iunit=ibas-niv
	77 lunit_=iunit

	return
end !function lunit_(ibas)
 
subroutine partunit(iob,io)   !!!!
 
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**partunit: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	idiv=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted
	if(idiv.le.0.or.idiv.gt.jlp_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access partunit',idiv, 'maximum is:',jlp_ndiv
	else !if(idiv.le.0.or.idiv.gt.j_ndiv)then
		j_v(iout)=jlp_iunitdiv(idiv) !o(isol)%i(jlp_nunits+idiv)
		! all
	endif !if(idiv.le.0.or.idiv.gt.j_ndiv)then
!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine !subroutine partunit(iob,io)
 
subroutine weight(iob,io)   !!!!
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**weight: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	ibas=j_v(j_o(iob)%i(io+2))  ! for which the weight is wanted
	if(ibas.le.0.or.ibas.gt.jlp_nunits+jlp_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access weight',ibas, 'maximum is:',jlp_nunits+jlp_ndiv
	else !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ndiv)then

		niv=0 ! count of (numb of parts-1)
		iunitv=-1
		do j=1,jlp_ndiv
			iunit=jlp_iunitdiv(j) !o(isol)%i(jlp_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			ib=iunit+niv
			if(ib.eq.ibas)then
				wei=jlp_wdiv(j) ! o(isol)%r(j)
				goto 77
			else if(ib.gt.ibas)then !if(ib.eq.ibas)then
				wei=1.   !are
				goto 77
			endif !if(ib.eq.ibas)then
			iunitv=iunit
		enddo !do j=1,j_ndiv
		! all
		wei=1.
		77  j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei

	endif !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ndiv)then
!	90 io=io+j_o(iob)%i(io+1)+3
	return
end !subroutine weight(iob,io)
 
! weight -funktion melaoptj versio
function we_(ibas)    !!!!
 
	integer ibas ! for which the weight is wanted
	niv=0 ! count of (numb of parts-1)
	iunitv=-1
	do j=1,jlp_ndiv
		! ib=jlp_nunits before
		iunit=jlp_iunitdiv(j) !o(isol)%i(jlp_nunits+j)
		if(iunit.eq.iunitv)niv=niv+1
		ib=iunit+niv
		if(ib.eq.ibas)then
			wei=jlp_wdiv(j) ! o(isol)%r(j)
			goto 77
		else if(ib.gt.ibas)then !if(ib.eq.ibas)then
			wei=1.   !are
			!  iunit=ibas-nivv
			goto 77
		endif !if(ib.eq.ibas)then
		iunitv=iunit
	enddo !do j=1,j_ndiv
	! all
	wei=1.
	77  we_=wei
	return
end !function we_(ibas)
 
subroutine schedweight(ibas,is,wei) !returns for ibas'th schedule twith nonzero weight the   !!!!
	! schedule number is (cumulative) and the weight w
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**schedweight: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	!  ibas for which the weight is wanted
	if(ibas.le.0.or.ibas.gt.jlp_nunits+jlp_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access weight',ibas, 'maximum is:',jlp_nunits+jlp_ndiv
	else !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ndiv)then

		niv=0 ! count of (numb of parts-1)
		iunitv=-1
		do j=1,jlp_ndiv
			! ib=jlp_nunits before
			iunit=jlp_iunitdiv(j) !o(isol)%i(jlp_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			ib=iunit+niv
			if(ib.eq.ibas)then
				wei=jlp_wdiv(j) ! o(isol)%r(j)
				is=jlp_ibaunit(iunit)+jlp_isdiv(j)
				goto 77
			else if(ib.gt.ibas)then !if(ib.eq.ibas)then
				iunit=iunit-(ib-ibas)
				is=jlp_ibaunit(iunit)+jlp_keys(iunit)
				wei=1.   !are
				goto 77
			end if !if(ib.eq.ibas)then
			iunitv=iunit
		end do !do j=1,j_ndiv
		! all
		wei=1.
		iunit=ibas-niv;is=jlp_ibaunit(iunit)+jlp_keys(iunit)
		77  continue !  v(o(iob)%i(io+o(iob)%i(io+1)+2))=wei

	end if !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ndiv)then

	return
end !subroutine schedweight(ibas,is,wei)
 
 
subroutine weightschedcum(iob,io)   !!!!
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**weightschedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	isc=j_v(j_o(iob)%i(io+2))  ! for which the weight is wanted
	if(isc.le.0.or.isc.gt.jlp_lopp)then
		j_err=.true.
		write(6,*)'**weight%schedcum: trying to access weight',isc, 'maximum is:',jlp_lopp
		return
	endif !if(isc.le.0.or.isc.gt.j_lopp)then

	iunit0=1+jlp_lopp/jlp_nunits;iunit0=min(jlp_nunits,iunit0)
	if(isc.le.jlp_ibaunit(iunit0))then
		do iunit=iunit0-1,1,-1
			if(isc.gt.jlp_ibaunit(iunit))exit
		enddo !do iunit=iunit0-1,1,-1
	else !if(isc.le.j_ibaunit(iunit0))then
		do iunit=iunit0,jlp_nunits
			if(isc.le.jlp_ibaunit(iunit+1))exit
		enddo !do iunit=iunit0,jlp_nunits
	endif !if(isc.le.j_ibaunit(iunit0))then
	is=isc-jlp_ibaunit(iunit)

	if(j_linkoption(iob,io,j_minteger).ge.0)then
		call j_clearoption(iob,io)  ! subroutine
		do j=1,jlp_ndiv
			! ib=jlp_nunits before
			if(jlp_iunitdiv(j).eq.iunit)then
				jmax=j
				do j2=j+1,jlp_ndiv
					if(jlp_iunitdiv(j2).ne.iunit)exit
					if(jlp_wdiv(j2).gt.jlp_wdiv(jmax))jmax=j2
				enddo !do j2=j+1,j_ndiv
				if(is.eq.jlp_isdiv(jmax))then
					wei=1.
				else !if(is.eq.j_isdiv(jmax))then
					wei=0.
					goto 90
				endif !if(is.eq.j_isdiv(jmax))then
			endif !if(j_iunitdiv(j).eq.iunit)then
		enddo !do j=1,j_ndiv

	else !if(j_linkoption(iob,io,j_minteger).gt.0)then

		do j=1,jlp_ndiv
			! ib=jlp_nunits before
			if(jlp_iunitdiv(j).eq.iunit)then
				do j2=j,jlp_ndiv
					if(jlp_iunitdiv(j2).ne.iunit)exit
					if(is.eq.jlp_isdiv(j2))then
						wei=jlp_wdiv(j2)
						goto 90
					endif !if(is.eq.j_isdiv(j2))then
				enddo !do j2=j,j_ndiv
				wei=0.
				goto 90
			endif !if(j_iunitdiv(j).eq.iunit)then
		enddo !do j=1,j_ndiv
	endif !if(j_linkoption(iob,io,j_minteger).gt.0)then

	if(is.eq.jlp_keys(iunit))then
		wei=1.
	else !if(is.eq.j_keys(iunit))then
		wei=0.
	endif !if(is.eq.j_keys(iunit))then
	90 continue

	j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei

!	io=io+j_o(iob)%i(io+1)+3
	return
end !subroutine weightschedcum(iob,io)
 
 
subroutine integersched(iob,io)  !both schedw andf schedcum   !!!! EI TOIMI
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**integersched: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	iunit=j_v(j_o(iob)%i(io+2))
	if(iunit.le.0.or.iunit.gt.jlp_nunits)then
		j_err=.true.
		write(6,*)'**weight%schedw: trying to access unit',iunit, 'maximum is:',jlp_nunits
		return
	endif !if(iunit.le.0.or.iunit.gt.jlp_nunits)then

	do j=1,jlp_ndiv
		! ib=jlp_nunits before
		if(jlp_iunitdiv(j).eq.iunit)then
			jmax=j
			do j2=j+1,jlp_ndiv
				if(jlp_iunitdiv(j2).ne.iunit)exit
				if(jlp_wdiv(j2).gt.jlp_wdiv(jmax))jmax=j2
			enddo !do j2=j+1,j_ndiv
			wei=jlp_isdiv(jmax)
			goto 90
		endif !if(j_iunitdiv(j).eq.iunit)then
	enddo !do j=1,j_ndiv

	wei=jlp_keys(iunit)

	90 continue
	if(j_o(iob)%i(io).eq.jifintegerschedcum)wei=wei+jlp_ibaunit(iunit) ! ?j3

	j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei

!	io=io+j_o(iob)%i(io+1)+3

	return
end subroutine !subroutine integersched(iob,io)
 
 
subroutine weightschedw(iob,io)  !!!!
 
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**weightschedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then

	iunit=j_v(j_o(iob)%i(io+2))
	if(iunit.le.0.or.iunit.gt.jlp_nunits)then
		j_err=.true.
		write(6,*)'**weight%schedw: trying to access unit',iunit, 'maximum is:',jlp_nunits
		return
	endif !if(iunit.le.0.or.iunit.gt.jlp_nunits)then
	is=j_v(j_o(iob)%i(io+3))  ! for which the weight is wanted
	if(is.le.0.or.is.gt.jlp_nsch(iunit))then
		j_err=.true.
		write(6,*)'**weight%schedw: trying to access sched',is, 'maximum is:',jlp_nsch(iunit)
		return
	endif !if(is.le.0.or.is.gt.j_nsch(iunit))then

	if(j_linkoption(iob,io,j_minteger).ge.0)then
		call j_clearoption(iob,io)  ! subroutine
		do j=1,jlp_ndiv
			! ib=jlp_nunits before
			if(jlp_iunitdiv(j).eq.iunit)then
				jmax=j
				do j2=j+1,jlp_ndiv
					if(jlp_iunitdiv(j2).ne.iunit)exit
					if(jlp_wdiv(j2).gt.jlp_wdiv(jmax))jmax=j2
				enddo !do j2=j+1,j_ndiv
				if(is.eq.jlp_isdiv(jmax))then
					wei=1.
				else !if(is.eq.j_isdiv(jmax))then
					wei=0.
					goto 90
				endif !if(is.eq.j_isdiv(jmax))then
			endif !if(j_iunitdiv(j).eq.iunit)then
		enddo !do j=1,j_ndiv

	else !if(j_linkoption(iob,io,j_minteger).gt.0)then

		do j=1,jlp_ndiv
			! ib=jlp_nunits before
			if(jlp_iunitdiv(j).eq.iunit)then
				do j2=j,jlp_ndiv
					if(jlp_iunitdiv(j2).ne.iunit)exit
					if(is.eq.jlp_isdiv(j2))then
						wei=jlp_wdiv(j2)
						goto 90
					endif !if(is.eq.j_isdiv(j2))then
				enddo !do j2=j,j_ndiv
				wei=0.
				goto 90
			endif !if(j_iunitdiv(j).eq.iunit)then
		enddo !do j=1,j_ndiv

	endif !if(j_linkoption(iob,io,j_minteger).gt.0)then

	if(is.eq.jlp_keys(iunit))then
		wei=1.
	else !if(is.eq.j_keys(iunit))then
		wei=0.
	endif !if(is.eq.j_keys(iunit))then
	90 continue

	j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei

!	io=io+j_o(iob)%i(io+1)+3
	return
end !subroutine weightschedw(iob,io)
 
 
subroutine partweight(iob,io)  !!!!
 
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**partweight: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	idiv=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted
	if(idiv.le.0.or.idiv.gt.jlp_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access partunit',idiv, 'maximum is:',jlp_ndiv
	else !if(idiv.le.0.or.idiv.gt.j_ndiv)then
		j_v(iout)=jlp_wdiv(idiv) !o(isol)%r(idiv)
		! all
	endif !if(idiv.le.0.or.idiv.gt.j_ndiv)then
!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine !subroutine partweight(iob,io)
 
subroutine schedw(iob,io)  !!!!
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**schedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	ibas=j_v(j_o(iob)%i(io+2))  ! for which the weight is wanted
	if(ibas.le.0.or.ibas.gt.jlp_nunits+jlp_ld0)then
		j_err=.true.
		write(6,*)'**trying to access schedw',ibas, 'maximum is:',jlp_nunits+jlp_ndiv
	else !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ld0)then
		niv=0 ! count of (numb of parts-1)
		iunitv=-1
		do j=1,jlp_ndiv
			! ib=jlp_nunits before

			iunit=jlp_iunitdiv(j) ! o(isol)%i(jlp_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			ib=iunit+niv
			if(ib.eq.ibas)then
				is=jlp_isdiv(j) ! o(isol)%i(jlp_nunits+ndiv+j)
				goto 77
				!  eivät ole sortattu
			else if(ib.gt.ibas)then !if(ib.eq.ibas)then
				iunit=iunit-(ib-ibas)
				is=jlp_keys(iunit) ! o(isol)%i(iunit)
				goto 77
			end if !if(ib.eq.ibas)then
			iunitv=iunit
		end do !do j=1,j_ndiv
		! all
		iunit=ibas-niv;is=jlp_keys(iunit) ! o(isol)%i(iunit)
		77 j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=is

	end if !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ld0)then

!	90 io=io+j_o(iob)%i(io+1)+3
	return
end !subroutine schedw(iob,io)
 
! schewd melaoptj versio
function ls_(ibas)   !!!!
	! weight()
 
	integer ibas ! for which the weight is wanted
	niv=0 ! count of (numb of parts-1)
	iunitv=-1
	do j=1,jlp_ndiv
		! ib=jlp_nunits before

		iunit=jlp_iunitdiv(j) ! o(isol)%i(jlp_nunits+j)
		if(iunit.eq.iunitv)niv=niv+1
		ib=iunit+niv
		if(ib.eq.ibas)then
			is=jlp_isdiv(j) ! o(isol)%i(jlp_nunits+ndiv+j)
			goto 77
			!  eivät ole sortattu
		else if(ib.gt.ibas)then !if(ib.eq.ibas)then
			iunit=iunit-(ib-ibas)
			is=jlp_keys(iunit) ! o(isol)%i(iunit)
			goto 77
		end if !if(ib.eq.ibas)then
		iunitv=iunit
	end do !do j=1,j_ndiv
	! all
	iunit=ibas-niv;is=jlp_keys(iunit) ! o(isol)%i(iunit)
	77 ls_=is

	return
end !function ls_(ibas)
 
subroutine schedcum(iob,io)  !!!!
 
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**schedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then

	ibas=j_v(j_o(iob)%i(io+2))  ! for which the weight is wanted
	if(ibas.le.0.or.ibas.gt.jlp_nunits+jlp_ld0)then
		j_err=.true.
		write(6,*)'**trying to access schedcum',ibas, 'maximum is:',jlp_nunits+jlp_ld0
	else !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ld0)then
		niv=0 ! count of (numb of parts-1)
		iunitv=-1
		do j=1,jlp_ndiv

			iunit=jlp_iunitdiv(j) !o(isol)%i(jlp_nunits+j)
			if(iunit.eq.iunitv)niv=niv+1
			ib=iunit+niv
			if(ib.eq.ibas)then
				is=jlp_ibaunit(iunit)+jlp_isdiv(j)
				goto 77
			else if(ib.gt.ibas)then !if(ib.eq.ibas)then

				iunit=iunit-(ib-ibas)
				is=jlp_ibaunit(iunit)+jlp_keys(iunit) ! o(isol)%i(iunit)+o(isol)%i2(4+iunit)
				goto 77
			end if !if(ib.eq.ibas)then
			iunitv=iunit
		end do !do j=1,j_ndiv
		! all
		iunit=ibas-niv;is=jlp_ibaunit(iunit)+jlp_keys(iunit) !o(isol)%i(iunit)+ o(isol)%i2(4+iunit)

		77  j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=is

	end if !if(ibas.le.0.or.ibas.gt.jlp_nunits+j_ld0)then

	90 return
!	io=io+j_o(iob)%i(io+1)+3
!	return
end !subroutine schedcum(iob,io)
 
subroutine partschedw(iob,io)   !!!!
 
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**partschedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	narg=j_o(iob)%i(io+1)

	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)

	idiv=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted
	if(idiv.le.0.or.idiv.gt.jlp_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access partsched',idiv, 'maximum is:',jlp_ndiv
	else !if(idiv.le.0.or.idiv.gt.j_ndiv)then

		j_v(iout)=jlp_isdiv(idiv) !o(isol)%i(jlp_nunits+ndiv+idiv)
		! all

	endif !if(idiv.le.0.or.idiv.gt.j_ndiv)then
	return
	!io=io+j_o(iob)%i(io+1)+3
!	return
end !subroutine partschedw(iob,io)
 
subroutine partschedcum(iob,io)   !!!!
	if(j_v(j_ivstartedjlp).eq.0)then

		write(6,*)'**partschedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	narg=j_o(iob)%i(io+1)
	isol=j_o(iob)%i(io+2)

	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)

	idiv=j_v(j_o(iob)%i(io+2))  ! for which the unit is wanted

	if(idiv.le.0.or.idiv.gt.jlp_ndiv)then
		j_err=.true.
		write(6,*)'**trying to access partsched',idiv, 'maximum is:',jlp_ndiv
	else !if(idiv.le.0.or.idiv.gt.j_ndiv)then
		iunit=jlp_iunitdiv(idiv) !o(isol)%i(jlp_nunits+idiv)
		j_v(iout)=jlp_ibaunit(iunit)+jlp_isdiv(idiv) !o(isol)%i(jlp_nunits+ndiv+idiv)+o(isol)%i2(4+iunit)
		! all

	endif !if(idiv.le.0.or.idiv.gt.j_ndiv)then
	return
!	90 io=io+narg+3
!	return
end !subroutine partschedcum(iob,io)
 
 
 
!factory
subroutine xkf(iob,io) !!!!
	logical bin_ , dividedunit_
	double precision, dimension(:,:),allocatable::sumxkf_
	integer,dimension(:), pointer::arg=>null()
	logical textform
	character*10 faction
	double precision ::amount0
	logical p
	integer kier

	narg=j_o(iob)%i(io+1)
	ivfile=j_o(iob)%i(io+2)
	ivout=j_o(iob)%i(io+narg+2)
	if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
!	io=io+narg+3
	nrec=0 !number of records writen
	if(j_v(j_ivstartedjlp).eq.0)then
		write(6,*)'**xkf: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(j_ivstartedjlp).eq.0)then
	if(.not.jlp_fpresent)then
		write(6,*)'**xkf: Not a factory problem,  fpresent=.false.'
		j_err=.true.
		return
	endif !if(.not.j_fpresent)then
	call j_getoption(iob,io,j_mprint,-1,1,0,.false.,.true.,nprint_,arg)
	textform=nprint_.ge.0
	call j_getoption(iob,io,j_mtole,-1,1,0,.false.,.true.,ntole_,arg)
	amount0=0.d0
	if(ntole_.gt.0)amount0=j_v(arg(1))
	call j_clearoption(iob,io)
	! writef:n mukaisesti
	if(ivfile.eq.j_ivdollar)then
		nu=6
	elseif(j_otype(ivfile).eq.j_ipchar)then !if(ivfile.eq.j_ivdollar)then
		nu=j_iounit(ivfile)  !j_o(ivfile)%i(4)
		if(nu.gt.0)then
			inquire(nu,action=faction)
			if(faction(1:4).eq.'READ')then
				call j_closeunit(nu)
				nu=0
			endif !if(faction(1:4).eq.'READ')then
		endif !if(nu.gt.0)then
		if(nu.le.0)then
			bin_ = .false.
			!subroutine j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout)
			call j_getfile(nu,'w',ivfile=ivfile,ivform=j_ivdollar)
			!call j_getwritefile(ivfile,j_ivdollar,bin_)
			if(j_err)return
			!nu=j_iounit(ivfile) !j_o(ivfile)%i(4)
		endif !if(nu.le.0)then
	else !if(ivfile.eq.j_ivdollar)then
		write(6,*)'**illegal file';j_err=.true.;return
	endif !if(ivfile.eq.j_ivdollar)then
	write(6,*)'*xkf: format is: stand, timber assortment,factory,transported amount '
	if(.not.textform)then
		call j_printname('timber assortment is the index in ',jlp_ivxkyk,' ')
		call j_command('print('//j_object_name(jlp_ivxkyk,j_leno(jlp_ivxkyk))//')')
		if(j_err)return
		call j_printname('factory is the index in ',jlp_ivfact,' ')
		call j_command('print('//j_object_name(jlp_ivfact,j_leno(jlp_ivfact))//')')
		if(j_err)return
	else !if(.not.textform)then

		len1=0
		do i=1,j_o(jlp_ivxkyk)%i(1)
			le=j_leno(j_o(jlp_ivxkyk)%i2(i))
			len1=max(len1,le)
		enddo !do i=1,j_o(j_ivxkyk)%i(1)

		len2=0
		do i=1,j_o(jlp_ivfact)%i(1)
			le=j_leno(j_o(jlp_ivfact)%i2(i))
			len2=max(len2,le)
		enddo !do i=1,j_o(j_ivfact)%i(0)
	endif !if(.not.textform)then

	allocate(sumxkf_(1:j_o(jlp_ivxkyk)%i(1),1:j_o(jlp_ivfact)%i(1)))

	do iuni_ = 1,jlp_nunits
		p=iuni_.le.2
		kier=0
		100		sumxkf_= 0.d0
		if(jlp_ivtrans.gt.0)then
			call dotrans(jlp_ivtrans,1)							 		! muunnokset yksikön tiedoilla
			if(j_err)then
				write(6,*)'error for unit',iuni_
				stop 778
			endif !if(j_err)then
		endif !if(jlp_ivtrans.gt.0)then
		dividedunit_ = .false.
		! a) yksiköllä vaihtoehtoja kannassa
		if(jlp_ndiv>0) then
			jj = 1
			! yksikön alkukohta painovektorissa
			do while((jlp_iunitdiv(jj)/=iuni_).and.(jj<jlp_ndiv))
				jj = jj + 1
			enddo !do while((j_iunitdiv(jj)/=iuni_).and.(jj<jjndiv))
			dividedunit_= jlp_iunitdiv(jj) == iuni_
		endif !if(j_ndiv>0) then

		! yksikön painot
		if (dividedunit_) then

			iuniw_ = jlp_iunitdiv(jj)
			do while(iuniw_==iuni_)
				wei_ = jlp_wdiv(jj)
				iobs_=jlp_ibaunit(iuni_)+jlp_isdiv(jj)+jlp_ibaunitbas					! kantavaihtoehdon indeksi
				call j_getobsiv(iobs_,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0)
				if(j_err)then !subr xks
					write(6,*)'error for observation ',iobs_
					stop 635
				endif !if(j_err)then
				if(jlp_ivsubtrans.gt.0)then
					call dotrans(jlp_ivsubtrans,1)
					if(j_err)then
						write(6,*)'error for schedule ',iobs_
						stop 941
					endif !if(j_err)then
				endif !if(jlp_ivsubtrans.gt.0)then
				! viedään painojen mukaiset osuudet avaintehtaisiin
				do ixk_=1,j_o(jlp_ivxkyk)%i(1)	! puutavaralaji-muuttujat
					keyf_ = jlp_keyfact(iuni_,ixk_)
					sumxkf_(ixk_,keyf_) = sumxkf_(ixk_,keyf_) + wei_*j_v(j_o(jlp_ivxkyk)%i2(ixk_))
					if(p.and.keyf_.eq.negf.and.ixk_.eq.negxk)&
						write(6,*)'wei ',wei_,j_v(j_o(jlp_ivxkyk)%i2(ixk_)),sumxkf_(ixk_,keyf_)
				enddo !do ixk_=1,j_o(j_ivxkyk)%i(1)

				if(jj<jlp_ndiv) then
					jj = jj + 1
					iuniw_ = (jlp_iunitdiv(jj))
				else !if(jj<j_ndiv) then
					iuniw_ = -1
				endif !if(jj<j_ndiv) then
			enddo !do while(iuniw_==iuni_)

		else !if (dividedunit_) then

			iobs_=jlp_ibaunit(iuni_)+jlp_keys(iuni_)+jlp_ibaunitbas						! yksikön avainvaihtoehdon indeksi
			call j_getobsiv(iobs_,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0)		!xkf  avainvaihtoehdon tiedot v-vektoriin ?
			if(j_err)then
				write(6,*)'error for obs ',iobs_
				stop 751
			endif !if(j_err)then
			if(jlp_ivsubtrans.gt.0)then
				call dotrans(jlp_ivsubtrans,1)
				if(j_err)then
					write(6,*)'error for observation ',iobs_
					stop 955
				endif !if(j_err)then

			endif !if(jlp_ivsubtrans.gt.0)then
			! Avainve:n ptl-mjan määrät avaintehtaissa
			do ixk_=1,j_o(jlp_ivxkyk)%i(1)	! puutavaralaji-muuttujat
				keyf_ = jlp_keyfact(iuni_,ixk_)
				sumxkf_(ixk_,keyf_) = sumxkf_(ixk_,keyf_) + j_v(j_o(jlp_ivxkyk)%i2(ixk_))
				if(p.and.keyf_.eq.negf.and.ixk_.eq.negxk)write(6,*)&
					'keysched,' ,j_v(j_o(jlp_ivxkyk)%i2(ixk_)),sumxkf_(ixk_,keyf_)
			enddo !do ixk_=1,j_o(j_ivxkyk)%i(1)

		endif !if (dividedunit_) then

		!nämä vain selittävinä kommentteina, vastaava suoritettava koodi aliohjelmassa defsoluf
		!xkfsol koko: allocate(xkfsol(mxd+1:lf0))
		!i0_xkfsol: xkfsol:in 1. indeksi (i0_xkfsol = mxd+1)
		!lf0_xkfsol: xkfsol:in viimeinen  indeksi (lf0_xkfsol = lf0)

		icol_ = jlp_i0_xkfsol
		if(icol_<=jlp_lf0_xkfsol) then
			do while ((icol_<jlp_lf0_xkfsol).and.(jlp_xkfsol(icol_)%iunit/= iuni_))
				icol_=icol_+1
			enddo !do while ((icol_<j_lf0_xkfsol).and.(j_xkfsol(icol_)%iunit/= iuni_))

			! tehdasmja-srkeet
			if (jlp_xkfsol(icol_)%iunit == iuni_) then
				! käydään läpi yksikön tehdaskantasrkeet xkfsolissa
				! saman yksikön sarakkeet (mjasta riippumatta) ketjutettu %next kentän kautta
				! %next == mxd:  yksiköllä  ei ole enää muita tehdaskantasarakkeita
				do while(icol_>jlp_mxd)
					!%ixkyk	! puutavaramuuttujan indeksi xkyky-listalla
					keyf_ = jlp_keyfact(iuni_,jlp_xkfsol(icol_)%ixkyk)	! avaintehdas

					sumxkf_(jlp_xkfsol(icol_)%ixkyk,keyf_) = &
						sumxkf_(jlp_xkfsol(icol_)%ixkyk,keyf_) - jlp_xkfsol(icol_)%xkf
					if(p.and.keyf_.eq.negf.and.	jlp_xkfsol(icol_)%ixkyk.eq.negxk)&
						write(6,*)'-sol',jlp_xkfsol(icol_)%xkf,sumxkf_(jlp_xkfsol(icol_)%ixkyk,keyf_)
					sumxkf_(jlp_xkfsol(icol_)%ixkyk,jlp_xkfsol(icol_)%ifact) = &
						sumxkf_(jlp_xkfsol(icol_)%ixkyk,jlp_xkfsol(icol_)%ifact) +  jlp_xkfsol(icol_)%xkf
					if(p.and.jlp_xkfsol(icol_)%ifact.eq.negf.and. &
						jlp_xkfsol(icol_)%ixkyk.eq.negxk)&
						write(6,*)'+sol',jlp_xkfsol(icol_)%xkf,sumxkf_(jlp_xkfsol(icol_)%ixkyk,jlp_xkfsol(icol_)%ifact)
					icol_ = jlp_xkfsol(icol_)%next
				enddo !do while(icol_>j_mxd)
			endif !if (j_xkfsol(icol_)%iunit == iuni_) then
		endif !if(icol_<=j_lf0_xkfsol) then

		!tulostus
		do if_= 1,j_o(jlp_ivfact)%i(1)
			do  ixk_= 1,j_o(jlp_ivxkyk)%i(1)
				if (sumxkf_(ixk_,if_)> amount0)  then
					if(textform)then
						if(kier.eq.0)write(nu,*) iuni_,j_object_name(j_o(jlp_ivxkyk)%i2(ixk_),len1+1),&
							j_object_name(j_o(jlp_ivfact)%i2(if_),len2+1),sumxkf_(ixk_,if_)
					else !if(textform)then
						if(kier.eq.0)write(nu,*) iuni_,ixk_,if_,sumxkf_(ixk_,if_)
					endif !if(textform)then
					nrec=nrec+1
				elseif(sumxkf_(ixk_,if_)< -0.00001.and..not.p)  then !if (sumxkf_(ixk_,if_)> amount0)  then
					!write(6,*)'***negative timber, unit ',iuni_,j_object_name(j_o(j_ivxkyk)%i2(ixk_),len1+1),&
						!ixk_,j_object_name(j_o(j_ivfact)%i(if_),len2+1),if_,sumxkf_(ixk_,if_)
					write(6,*)'***negative timber, unit ',iuni_,ixk_,if_,sumxkf_(ixk_,if_)
					negf=if_
					negxk=ixk_
					p=.true.

				endif !if (sumxkf_(ixk_,if_)> amount0)  then
			enddo !do  ixk_= 1,j_o(j_ivxkyk)%i(1)
		enddo !do if_= 1,j_o(j_ivfact)%i(0)
		if(p.and.kier.eq.0)then
			kier=1
			goto 100
		endif !if(p.and.kier.eq.0)then
	enddo !do iuni_ = 1,jlp_nunits

	if(ivfile /= j_ivdollar) then
		call j_closeunit(nu) ! JL 29.3. 2016
	endif !if(ivfile /= j_ivdollar) then
	deallocate(sumxkf_)
	j_v(ivout)=nrec
	return
end subroutine !subroutine xkf(iob,io)
 
 
integer function integerschedw(iunit)  ! %%integer solution   !!!!
	integer,intent(in) ::iunit
	!could be used in subroutine integersched(iob,io)
	integer wei
	if(iunit.le.0.or.iunit.gt.jlp_nunits)then
		j_err=.true.
		write(6,*)'*j* integersched trying to access unit',iunit, 'maximum is:',jlp_nunits
		return
	endif !if(iunit.le.0.or.iunit.gt.jlp_nunits)then

	do j=1,jlp_ndiv
		! ib=jlp_nunits before
		if(jlp_iunitdiv(j).eq.iunit)then
			jmax=j
			do j2=j+1,jlp_ndiv
				if(jlp_iunitdiv(j2).ne.iunit)exit
				if(jlp_wdiv(j2).gt.jlp_wdiv(jmax))jmax=j2
			enddo !do j2=j+1,j_ndiv
			wei=jlp_isdiv(jmax)
			goto 90
		endif !if(j_iunitdiv(j).eq.iunit)then
	enddo !do j=1,j_ndiv

	wei=jlp_keys(iunit)

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
! 			(Jos muuttuja kolmiosainen, ensimmäinen osa ei ole olio) - ??
subroutine isfvar(iv,itrans,imat,len1,iv2,iv3)  !!!!
	character*24 name
	integer ivt(4)
	ibas=j_o(j_ivnames)%i(iv)-1; le=j_o(j_ivnames)%i(iv+1)-ibas-1 !le= the length of rest
	np=0
	iv1=0;iv2=0;iv3=0
	itrans=0
	imat=0

	1000 do i=2,le-2
		if((j_o(j_ivnames)%ch(ibas+i).eq.'%'.and.j_o(j_ivnames)%ch(ibas+i+1).eq.'%'))goto 1
	end do !do i=2,le-2
	if(np.eq.0)return

	i=le+1

1 do j=1,i-1 ;name(j:j)=j_o(j_ivnames)%ch(ibas+j) ;end do
	np=np+1
	if(np.eq.1)len1=i-1  ! length of the first part before '%'

	if(np.gt.3)then
		write(6,*)'**too many %% in a name of problem'
		j_err=.true.
		return
	endif !if(np.gt.3)then
	ivt(np)=j_object(name(1:i-1))

	ibas=ibas+i+1
	le=le-(i+1)
	if(le.gt.0)goto 1000

	if(np.eq.2)then
	!  pulp#"j"%%@pulpfactory(i)<pcapacity%@pulpfactory(i)
		iv3=ivt(2);iv2=ivt(1);iv1=0
		if(iv2.le.0)then
			call j_printname('**first part unknown',iv,' ');
			j_err=.true.
		endif !if(iv2.le.0)then
		if(iv3.le.0)then
			call j_printname('**second part unknown',iv,' ');
			j_err=.true.
		endif !if(iv3.le.0)then
		if(j_err)return

	else !if(np.eq.2)then

	! util%%sawlog%%sawmill
		iv2=ivt(2);iv3=ivt(3)

		if(iv2.le.0)then
			call j_printname('**second part unknown',iv,' ');
			j_err=.true.
		endif !if(iv2.le.0)then
		if(iv3.le.0)then
			call j_printname('**third part unknown',iv,' ');
			j_err=.true.
		endif !if(iv3.le.0)then
		if(j_err)return
		if(j_otype(iv2).ne.j_iplist)then
			call j_printname('**second part is not a list ',iv,' ')
			j_err=.true.
		endif !if(j_otype(iv2).ne.j_iplist)then
		if(j_otype(iv2).ne.j_iplist)then
			call j_printname('**third part is not a list ',iv,' ')
			j_err=.true.
		endif !if(j_otype(iv2).ne.j_iplist)then

		itrans=j_object2('trans.',iv)

		if(itrans.le.0)then
			call j_printname('**transformation trans.',iv,' does not exist')
			j_err=.true.
		endif !if(itrans.le.0)then
		if(j_err)return
		if(j_otype(itrans).ne.j_iptrans)then
			call j_printname('**object ',itrans,' is not a transformation')
			j_err=.true.
			return
		endif !if(j_otype(itrans).ne.j_iptrans)then

		return
	endif !if(np.eq.2)then
end subroutine !subroutine isfvar(iv,itrans,imat,len1,iv2,iv3)
 
! pack2
! Korvaa bugisen pack-funktion, joka joskus kaatuu kun palautetaan
! samaan vektoriin joka on parametrina
! nval: vektoreiden itarget ja is koko
subroutine pack2(itarget, is, n)    !!!!
	integer itarget(*)
	logical is(*)
	integer n

	i2_= 0
	do i_= 1,n
		if(is(i_)) then
			i2_=i2_+1
			itarget(i2_)=itarget(i_)
		endif !if(is(i_)) then
	enddo !do i_= 1,n
	return
end subroutine !subroutine pack2(itarget, is, n)
 
subroutine jlp(iob,io)   ! %%jlp  !!!!****************************************************
! Section jlpintro Linear programming functions
! The linear programming (LP) functions are called jlp-functions. The jlp-functions can be used to define
! linear programming problems, solve them and access the results.
! The jlp-problems are defined using problem() function, and the problems can be solved using
! jlp() function. There are several ways to access the results. 
! The main interest in jlp-functions may be in the forest planning problems, where
! a simualator has generate treatment schedules.
! There are four different applications of jlp() function. 
! \begin{itemize}
! \item[\textbf{J}] Small ordinary LP problems with text input.
! \item[\textbf{J}] Large ordinary LP problems with MATRIX input.
! \item[\textbf{J}] Forest planning problems without factories.
! \item[\textbf{J}] Forest palnning problems with factories.
! \end{itemize}
!The problem definition for each case is generated with problem() function.
! These applications are presented in different subsections
! endsection


! Section jlpzp Problem definition for small ordinary LP problems with text input: problem()
! endheader
! The problem is defined in the input paragraph following 
! !Option
!Output& 1& PROBLEM &The generated PROBLEM object.
!Args& 0- &  & named objects or constants. If an argument is LIST it is ex+panded
!first.
!endoption


!endsection








! Section jlp Solving large problems without schedule data. 
! When solving problems including only a large number of z-variables, it is possible to feed the
! coefficients as a matrix with zmatrix-> option. Unit and schedule data (c- and x-variables)
! are not allowed when zmatrix-> is used.
! =jlp(zmatrix->,max->|min->[,rhs->][,rhs2->][,tole->]
! [,print->][,maxiter->][,test->][,debug->])
! Output:
! Function jlp() generates output row vectors output%zvalues, output%redcost and
! output column vectors output%rows, output%shprice output is the name of the output.
! Options:
! zmatrix Matrix containing coefficients of z-variables for each constraint row.
! max Vector containing coefficients of z-variables for the objective row of a
! maximization problem. Either max-> or min-> option has to be defined but not
! both.
! min Vector containing coefficients of z-variables for the objective row of a
! minimization problem. Either max-> or min-> option has to be defined but not
! both.
! rhs Vector containing lower bound for each constraint row. Value 1.7e37 is used to
! indicate the absence of the lower bound in a row. Either or both of the bound
! options (rhs->, rhs2->) has to be defined.
! rhs2 Vector containing upper bound for each constraint row. Value 1.7e37 is used to
! indicate the absence of the upper bound in a row. Either or both of the bound
! options (rhs->, rhs2->) has to be defined.
! Other options described above in chapter Solving a problem: jlp( ).
! NNote. When zmatrix-> option is used, the solution is not automatically printed. Use jlp
! solution objects to access solution. For more information see chapter 11.10 Objects for the JL
!enEdnote


! There are two versions of jlp() function call: one with problems-> option for problems
! defined by problem() function and the other with zmatrix-> option for large ordinary
! linear programming problems with z-variable coefficients defined by matrix.
! A lp problem defined by problem() function can be solved using jlp() function:
! [=]jlp(problem->[,data->][,z->][,trans->][,subtrans->]
! [,tole->][,subfilter->][,subreject->][,class->]
! [,area->][,notareavars->][,print->][,report->]
! [,maxiter->][,refac->][,slow->][,warm->][,finterval->]
! [,fastrounds->][fastpercent->][,swap->][,test->][,debug->]
! [,memory->])
! Output:
! Necessary for factory problems, otherwise optional. If output is given then function generates
! several matrices and lists associated with the solution (e.g. the values of the constraint rows,
! the shadow prices of the rows, the values of the z-variables, the reduced costs of z-variables,
! the sums of all x-variables of the data in all domains and their shadow prices, lists telling how
! problem variables are interpreted. See Objects for the JLP solution for more detailed
! description.
! Options:
! problem problem definition generated by problem() function
! data data set describing the stand (management unit) data or the schedules data. The
! unit data set must be linked to schedule data either using sub-options in the
! data() function or using linkdata() function. Following the JLP terminology,
! the unit data is called cdata, and the schedule data is called xdata. The jlp()
! function tries if it can find a subdata for the data set given. If it finds, it will assume
! that the data set is the unitdata. If subdata is not found, it tries to find the upper
! level data. If it finds it, then it assumes that the data set given is the schedules
! data. If data-> is not given, then the problem describes an ordinary lp-problem,
! and all variables are z-variables. If data-> option is given but no variable found
! in problem is in the schedules data set, then an error occurs.
! z If the data-> option is given then the default is that there are no z-variables in
! the problem. The existence of z-variables must be indicated with z-> option
! (later the user can specify exactly what are the z variables, but now it is not
! possible). The reason for having this option is that the most jlp-problems do not
! have z variables, and variables which J interprets as z-variables are just
! accidentally missing from the data sets.
! trans transformation set which is executed for each unit.
! subtrans transformation set which is executed for each schedule.
! NNote: the subtrans transformations can utilize the variables in the unit data and
! the output variables of trans-> transformations.
! NNote: transformations already associated with cdata and xdata are taken
! automatically into account and they are executed before transformations defined
! in trans-> or subtrans-> options.
! *** Later we may add the possibility to have several data sets (note that several files can be
! read into one data object in the data()function)
! tole the default tolerances are multiplied with the value of the tole-> option (default
! is thus one). Smaller tolerances mean that smaller numerical differences are
! interpreted as significant. If it is suspected that jlp() has not found the
! optimum, use e.g. tole->0.1 ,tole->0.01 or tole->10.
! subfilter logical or arithmetic statement (nonzero value indicating True) describing which
! schedules will be included in the optimization. If all schedules are rejected, an
! error occurs. Examples: filter->(.not.clearcut) , filter-
! >(ncuts.ge.5), filter->harvest (which is equivalent to: filter-
! >(harvest.ne.0)). If the subfilter statement cannot be defined nicely using
! one statement, the procedure can be put into a transformation set which can be
! then executed using value() function.
! subreject logical or arithmetic statement (nonzero value indicating True) describing which
! schedules will not be included in the optimization. If subfilter-> is given then
! test applied only for such schedules which pass the subfilter test. If the subreject
! statement cannot be defined nicely using one statement, the procedure can be
! put into a transformation set which can be then executed using value()
! function.
! Kommentoinut [LR(46]: Repon puolelle
! Kommentoinut [LR(47]: Repon puolelle[Natural resources and bioeconomy studies XX/20XX]
! 95
! class class->(cvar, cval) Only those treatment units where the variable cvar gets
! value cval are accepted into the optimization. The units within the same class
! must be consecutive.
! area gives the variable in cdata which tells for each stand the area of the stand. It is
! then assumed that all variables of cdata or xdata used in the problem rows are
! expressed as per are values. In optimization the proper values of variables are
! obtained by multiplying area and per area values. Variables of cdata used in
! domain definitions are used as they are, i.e. without multiplying with area.
! Variables which are not treated as per area values are given with the
! notareavars-> option.
! notareavars
! If area-> option is given then this option gives variables which will not be
! multiplied with area.
! print of output printed, 1 => summary of optimization steps, 2=> also the problem
! rows are printed, 3=> also the values of x-variables are printed, 9= the pivot
! steps and the point of code where pivoting is done and the value of objective
! function are written to fort.16 file (or similar file depending on the operating
! system). The value 9 can be used in the case where jlp seems to be stuck. From
! fort.16 file one can then infer at what point debugging should be put on. Some
! cycling situations are now detected, so it should be rather unlikely that jlp is stuck.
! report the standard written output is written into the file given in report-> option
! (.e.g. report->'result.txt'). The file remain open and can be written by
! several jlp-functions or by additional write() functions. Use close() function
! to close it explicitly if you want to open it with other program.
! maxiter maximum number of rounds through all units (default 10000).
! refac after refac pivot steps the basis matrix is refactorized. The default value is 1000.
! New option since J3.0. Actually refactorization was present in the first version of
! J but it had to be dropped because the refactorization corrupted some times the
! factors of the basis matrix. The reason was found and corrected for J3.0. The
! reason for misbehavior of the refactorization algorithm of Fletcher was such that
! it never caused problems in ordinary linear programming problems for which
! Fletcher designed his algorithms.
! slow if improvement of the solution in one round through all units is smaller than value
! given in slow-> option, then J terminates under condition ‘slow improvement’.
! New option since J3.0. Earlier slow improvement is computed from the tolerances
! of the problem, and if the slow-> option is not present these tolerances are still
! used. If slow-> option gives negative value, then the absolute value of the
! option indicates per cent change. Note that in large problems the solution is
! often very long time quite close the actual optimum, and hence the optimization
! time can be decreased with rather low loss in accuracy using the slow-> option.
! If slow-> option is give value zero it is equivalent to omitting the option and
! hence the slow improvement is determined from the tolerances.
! warm If option is present in ordinary problems with x-variables then the key schedules
! of previous solution are used as the starting values of the key schedules. In factory[Natural resources and bioeconomy studies XX/20XX]
! 96
! problems also the previous key factories are used as starting values. If there is no
! previous solution available or the dimension of the key schedules vector (number
! of treatment units) or the dimensions of the key factories matrix (number of
! treatment units and number of factories) do not agree with the current problem,
! warm option is ignored. Thus it is usually safe to use the option always, the only
! exception is that the factories and number of units do agree with the previous
! problem even if factories or schedule data are changes. The warm start may
! reduce solution time perhaps 40-80%.
! finterval In factory problems the transportations to new factories are checked if the round
! number is divisible with finterval. The default value is 3. When there have not
! been improvements during the last round, the value is changed into 1.
! fastrounds
! The shadow prices are used to select an active set of schedules which are
! considered as entering schedules. fastrounds gives the number of rounds using
! the same active set. The default is 10. When there have not been improvements
! during the last round, all schedules are used as the active set.
! fastpercent
! A schedule belongs to the active set if its shadow price is at least fastpercent %
! of the shadow price of the best schedule. The default is 90.
! swap If option is present, the schedules data matrix is written to a direct access binary
! file. This option may help if virtual memory overflow occurs. The data needs to
! be used from the file until all inquiry function calls are computed. Thereafter the
! data can be loaded again into memory using function unswap(). If this does not
! happen ordinary functions using this data object work a little slower. If a new
! problem is solved with the swap-> option, there is no need to unswap() before
! that. In an Intel Fortran application the swap-> option is given without
! arguments. In a Gfortran application the option must be given in form swap->4.
! If there is shortage of virtual memory, read note 5 for problem() function before
! starting to use swap->.
! test If option is present then jlp() is checking the consistency of the intermediate
! results after each pivot step of the algorithm. Takes time but helps in debugging.
! debug determines after which pivot steps jlp() starts and stops to print debugging
! information to fort.16 file. If no value given, the debugging starts immediately
! (produces much output, so it may be good to use step number which is close to
! the step where problems started (print variable Pivots at the error return).
! debug->(ip1,ip2,ip3) indicates that debugging is put on at pivot step ip1,
! off at pivot ip2 and the again on at pivot ip3.
! memory gives the amount of memory in millions of real numbers that can be used to store
! data needed in solving the problem. In factory problems also the xdata stored in
! a direct access file are loaded into memory as much as possible. It is not possible
! to figure out how large number memory option can give, so it must be
! determined with experimentation. Using disk-> option in data() function and
! memory-> option in jlp() function makes it in principle possible to solve
! arbitrary large problems. In practise the ability of double precision numbers
! cannot store accurately the needed quantities in very huge problems.
! Kommentoinut [LR(48]: Vain gfortran käännös?
! Kommentoinut [LR(49]: Lisätty. Oliko tämä mukana
! julkaistavassa versiossa?[Natural resources and bioeconomy studies XX/20XX]
! 97
! Function jlp() is generating output (amount is dependent on the print-> option) plus a
! JLP-solution stored in special data structures which can be accessed with special J functions
! described below and if output is given then several output objects are created (see Objects for
! the JLP solution).
!NNote 1: a feasible solution (without an objective) can be found by minimizing a z-variable
! (remember z-> option), or by maximizing a unit variable (which is constant for all schedules in
! a unit).
! NNote 2: If virtual memory overflow occurs, see first Note 5 for problem() function and then
!endsection









 
	use fletdmod !nopre!   !file jlpd.f90
	use fletdmod2 !nopre!
!	use jlpdmod !nopre!

	use fletcherdmod  !nopre!
	use lastmod !nopre!

	! variables in internal subroutines which must be accesse in other subroutines
	integer ilzmax,ienter,leave,leavec,leaved,nrowxkfkey,nrowykfkey,ilrmax
	integer iunitv,iunit,kierv,kier,newa,leavk,ia,iaft
	integer lz0,leaz,lead,iopt,lr0,lcur,lcursame,leavkf,leavkwf,lunit_,isecond
	integer nrecover,ipivot,ntrans,idebug,nout,ifail,jdebug,ixkykenter,ixkyk_,ifopt,id
	integer ixkyk1_,ixkyk_lkf,if_,inf_,infopt,lcursamef,lcur0_unit,lcur_,nz,nfy
	integer krefac,nup,newc,muutosb,iunit_lkf,ikeepxkyk_
	integer iunitrans,nnf,nnfiter,nnfold2,lcur0,ienter1,ix0,j1,newd,nnfold
	integer irowobj,irow,irow0,irow0bas,nset
	integer ncvar,noutsubtrans,irowinf
	integer ileavedi_,jcurix_,newf,lf01
	integer ivoutsubtrans
	integer ipivot9,iunitprev
	
	integer ::ivobjective2,ivoutresult

!	integer iperke

	logical updated,pivoted,supdated
	logical goto900,goto1234,goto1578,goto400,goto36
	logical goto100,goto5316,goto222,goto55,goto112233
	logical goto8888,goto8889,goto700 ! input for leaving
	logical goto401
	logical sentered
	logical echo

	logical ::pp=.false.

	logical ::p=.true.
	real*8 val_

	real*8  rcur
	logical justkey

	logical ::factnow=.false.  !are factories checked
	logical isstop,stopped
	!	logical isper
	logical bin
	logical cycling
	logical p2
	logical newprice
	logical sparse
	logical subfilter_,subreject_,subfilre
	logical zopt
	logical printlf
	integer listapu(1)
	double precision apusum
	!	integer xdatiba
	logical simobug

!	logical isslow

	! apumja monimutkaiseen ehtoon
	logical fvari

	logical zmatrix, zobj, rhsopt, rhs2opt

	!tree
	logical istree  !are there periods for
	integer,dimension(:),allocatable :: iperiod  !for each column of xmat the period
	double precision,dimension(:),allocatable :: rperiod  !for each column of xmat the period as real
	integer,dimension(:),allocatable ::iperiodperm !permutations
	integer,dimension(:),allocatable :: idiff !for each unit the number of first different var
	!   periods as in previous schedule
	real,dimension(:),allocatable ::previous
	double precision,dimension(0:j_maxjlpxvars) ::valuesums
	double precision value_tree
	!	double precision,dimension(:),allocatable ::valuetree
	integer,dimension(j_maxjlpxvars) ::ixpack3
	double precision, dimension(j_maxjlpxvars) ::vxpack3
	integer,dimension(:),allocatable ::idiff3
!	double precision ::slow

	!cycle
	double precision ::oldsol
	integer ::ienterv=-1
	integer ::nsame=0

	!cycle2
	double precision :: againsol
	integer :: nagain
	logical :: again

	!	double precision store
	logical intapp  !is integer approximation computed
!	integer, dimension (:), pointer :: slowopt
	logical p9

	integer, dimension (:), pointer :: swapopt
	logical isswap
	logical yes,yes2
	!zerocapacity
	logical zeroc  !,dimension(:,:),allocatable :: zeroc  !
	logical::warm,warmf

	logical rejectnow_

	integer nkeys,nkeyf,ntote
	! for sorting puposes
	double precision objilres
	logical route67
	! real,dimension(:),allocatable :: tree
	!endtree

	!new properties
	! factories are not checked if the round number (=kier) is not divisible with parameter iterxkf :
	!if(kier.gt.1.and.iterxkf*int(kier/iterxkf).ne.kier)goto 400

	! fast: the same set of active schedules is checked during several rounds

	logical fast  ! is the fast option used
	logical fastnow ! is the use of active schedules on for the current round
	logical fastmake  ! is the active set made
	logical*1,dimension(:),allocatable :: fastreject !tells which schedules are not in the active set
	real,dimension(:),allocatable:: fastvalues  !the values of schedules in the current unit when setting up the
	!	integer*2,dimension(:),allocatable::fastclass
	! fastreject
	integer ::fastusesame ! how many rounds the same set of active schedules will be used
	integer :: fastusedsame !  how many rounds the same set of active schedules has been used
	double precision fastvaluemin ! the minimum value of schedules in the current unit
	real ::fastpros       ! how big share of the range (fastvaluemin,j_valueopt) (in %) is rejected
	real :: fastpros2     ! fastpros/100

	logical tried ! indicates that last improvement is already tried

	! if the algorithm improves slowly it may be reasonable to take the schedule with the second
	! highest value into the basis
	double precision secondb  !the value of the second best schedule
!	double precision ::rh_
	logical tried2    ! are the second bests trie

	logical tabu !will leaving schedule be put on fastreject list
	!logical xdisk
	logical p7

	!memory
	!logical readit,deleted
	logical rz,e1234,w222,doresid
	integer xdatiba
	logical ::isunit



	common/noutc/nout
	common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj  !Fletcher parameters
	!dense common/factorc/m0,m1,mm0,mm,mp,mq
	!sparse common/factorc/m1,m2,mp,mq,lastr,irow

	!this means that when in dense mpjj and mqjj are changed
	! then in sparse mpjj is the same as

	common/ipivotc/ipivot
	common/refactorc/nup,nfreq
	jxmatiba(iobs)=(iobs-1)*jlp_ntemp0  !(j_memobs(iobs)-1)*j_ntemp0
	xdatiba(iobs)=(iobs-1)*keepx
	e1234=.false.   !error
	w222=.false. !
	ileavedi_=0  !testasuta, poisat kun joutaa
	nnread=0
	nnused=0

	!iperke=75
	p7=.false.
!	io=io_  !io must be used
!	io_=io_+j_o(iob)%i(io_+1)+3   !so that we can return anywhere
	!xdisk=.false.





	tried2=.false.
	tried=.false.
	fastusedsame=0
	fastnow=.false.
	intapp=.false.  ! added by JL 112018
	fastmake=.false.
	! optioksi
	! fastrounds-> how many rounds the same active set is used, 1 indicates that no active set is generated but
	! all schedules are always used
	! default value if 10
	! fastpercent -> a chedules is included in the active set if the shadow value is at lest fastpercetn* the value
	! of the optimal schedule
	! default is 90

	! finterval->   it is checked wether new factories can enter if the round number is jaollinen with finterval
	! default is 3. his option applies only after finding feasible

	! fastusesame= value of fastround
	! faspros= fastpercent
	! iterxkf=finterval
	! fast-optiot
	fastusesame = 10
	i = j_igetopt(iob,io,j_mfastrounds)
	if(i > 0) fastusesame = j_v(i)
	
	ivunit=j_igetopt(iob,io,j_munit)
	isunit=ivunit.gt.0
!	write(6,*)'ivunit ',ivunit

	fastpros = 90
	i = j_igetopt(iob,io,j_mfastpercent)
	if(i > 0) fastpros = j_v(i)

	iterxkf = 3
	i = j_igetopt(iob,io,j_mfinterval)
	if(i>0) iterxkf = j_v(i)

	! inmemory=.true.
!	memory=0
!	j_xmatinmemory=.true.
!	j_xdatinmemory=.true.
!	ivmemory=j_igetopt(iob,io,j_mmemory)
!	if(ivmemory>0)memory=j_v(ivmemory)*1000000.
	!memory=memory*1000000
!	write(6,*)'<345>',memory
	tabu=fastpros.lt.0
	fastpros=abs(fastpros)
	fastpros2=fastpros/100.
	fast=fastusesame.ne.1
	!write(6,*)'<2345kukuu'
	nkeys=0
	nkeyf=0
	ntote=0
	ilres=0
	nresw=0
	npivotw=0
	nkeyfactw=0
	i=j_igetopt(iob,io,j_mwarm)
	warm=.false.
	if(i.gt.0)then
		warm=j_v(i).ne.0.
	elseif(i.eq.0)then !if(i.gt.0)then
		warm=.true.
	endif !if(i.gt.0)then
	!keys key schedules for each unit
	if(.not.allocated(jlp_keys).and.warm)then
		write(6,*)'*jlp: old keys not available, warm-> ignored'
		warm=.false.
	endif !if(.not.allocated(j_keys).and.warm)then
	warmf=warm

	call j_getobject(0,'Feasible',j_ipreal,ivfeasible); j_v(ivfeasible)=0.
	call j_getobject(0,'Unbounded',j_ipreal,ivunbounded); j_v(ivunbounded)=0.
	call j_getobject(0,'Objective',j_ipreal,ivobjective); j_v(ivobjective)=-99.99
	call j_getobject(0,'Optimal',j_ipreal,ivoptimal); j_v(ivoptimal)=0.
	call j_getobject(0,'Pivots',j_ipreal,ivpivots); j_v(ivpivots)=0.
	call j_getobject(0,'Started_jlp',j_ipreal,j_ivstartedjlp); j_v(j_ivstartedjlp)=0.

	krefac=0 !number of refactorizations
	printlf=.false.
	ivprint7=j_igetopt(iob,io,j_mprint)
	if(ivprint7.gt.0)then
		if(j_v(ivprint7).eq.7)printlf=.true.
	endif !if(ivprint7.gt.0)then
	p=.false.
	nrecover=0
	!isslow=.false.
	!cycle2

	again=.false.
	nagain=0
	n16=16
	simobug=.true.
	ilrmax=1  !to avoi array bounds exeeded situation in point !assshshdhs
	nout=0

	jlp_fpresent=.false.
	jlp_isxkyk0 = .false.
	jlp_restarted = .false.

	!j_ivvarsx=0
	! nz=0 nz initilized later JL 112018
	!write(6,*)'<55',iob
	justkey=.false.
	call j_getoption_index(iob,io,j_mslow,-1,100,j_ipreal,.true.,.true.,noptarg,j_optarg0)
!	call j_getoption(iob,io,j_mslow,-1,1,j_ipreal,.true.,.true.,nslow,slowopt)
	if(noptarg.ge.0)write(6,*)'slow-> is obsolete, use stop->'
	!write(6,*)'<56',iob

	call j_getoption_index(iob,io,j_mecho,-1,1,j_ipreal,.true.,.false.,noptarg,j_optarg0)
!	call j_getoption(iob,j_mslow,-1,1,j_ipreal,.true.,.true.,nslow,slowopt)
	echo=.true.
	if(noptarg.gt.0)then
		if(j_v(j_optarg0(1)).le.0.d0)echo=.false.
	endif !if(noptarg.gt.0)then
!	j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg,istart) ! %%option
!	write(6,*)'<57',iob
	call j_getoption_index(iob,io,j_mnoint,-1,0,j_ipreal,.true.,.true.,noptarg,j_optarg0)
!	call j_getoption(iob,j_mslow,-1,1,j_ipreal,.true.,.true.,nslow,slowopt)
	intapp=noptarg.lt.0


	! if(nslow.eq.1)then
		! slow=j_v(slowopt(1))
	! else !if(nslow.eq.1)then
		! slow=0.d0
	! endif !if(nslow.eq.1)then

!	call j_getoption(iob,j_mswap,-1,1,j_ipreal,.false.,.false.,noptarg,j_optarg0)
	! if(nswap.eq.1)then
	! irecls=j_v(swapopt(1))
	! else !if(nslow.eq.1)then
	! irecls=1

	! endif !if(nslow.eq.1)then
!	if(noptarg.ge.0)then
!		write(6,*)'**swap: use disk-> or subdisk-> in data function and/or memory->'
		! j_err=.true.
		! return
!	endif
	jlp_feasible=.false.
	zopt=.false.
	if(j_linkoption(iob,io,j_mz).ge.0.or.j_linkoption(iob,io,j_mdata).lt.0)zopt=.true.

	niter=0
	ncover=0
	icolold=-1
	jlp_needcinx=.false.

	jlp_ivtrans=j_igetopt(iob,io,j_mtrans) !object given in trans-option
	!write(6,*)'<187>jlp_ivtrans',jlp_ivtrans
	if(jlp_ivtrans.gt.0)then
		if(j_otype(jlp_ivtrans).ne.j_iptrans)then
			call j_printname('**jlp: trans->',jlp_ivtrans,' is not transformation')
			j_err=.true. ;return
		endif !if(j_otype(jlp_ivtrans).ne.j_iptrans)then
	endif !if(jlp_ivtrans.gt.0)then
	jlp_ivsubtrans=j_igetopt(iob,io,j_msubtrans) !object given in subtrans-option
	!write(6,*)'<5444jlp_ivsubtrans',jlp_ivsubtrans
	if(jlp_ivsubtrans.gt.0)then
		if(j_otype(jlp_ivsubtrans).ne.j_iptrans)then
			call j_printname('**jlp: subtrans->',jlp_ivsubtrans,' is not transformation')
			j_err=.true. ;return
		endif !if(j_otype(jlp_ivsubtrans).ne.j_iptrans)then
	endif !if(jlp_ivsubtrans.gt.0)then

	! return   990 before allocations
	!          900 after general allocations
	!check options
	!test
	ivarea=j_igetopt(iob,io,j_marea)
	if(ivarea.eq.0)then   !option area->,
		ivarea=j_object('area')
		if(ivarea.le.0)then
			write(6,*)'**jlp, area-variable does not exist'
			j_err=.true.
			return
		endif !if(ivarea.le.0)then
	endif !if(ivarea.eq.0)then
	notareav=j_nargopt(iob,io,j_mnotareavars) !number of notareavars-> -variables
	if(notareav.gt.0)then
		linknotarea=j_linkoption(iob,io,j_mnotareavars)
		if(ivarea.lt.0)then
			write(6,*)'**simulate, cannot have notareavars-> without area->'
			j_err=.true.
			return
		endif !if(ivarea.lt.0)then
	endif !if(notareav.gt.0)then
	! outputobject, not used now

	! report->
	call j_getoption(iob,io,j_mreport,-1,1,j_ipchar,.false.,.true.,noptarg,j_optarg0) 
	if(j_err)return
	if(noptarg.eq.1)then
		ivreport=j_optarg0(1) ! j_o(iob)%i(j_linkoption(iob,io,j_mreport)+1)

	!	if(j_otype(ivreport).eq.j_ipchar)then
			nureport=j_iounit(ivreport) !j_o(ivreport)%i(4)  ! file unit when character object is a file name
			if(nureport.le.0)then
				!subroutine j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout)
				call j_getfile(nuu,'w',ivreport)
			!	write(6,*)'<545>',nuu,ivreport
				!call j_getwritefile(ivreport,j_ivdollar,bin)
				if(j_err)return
				nureport=nuu  !j_iounit(ivreport) !j_o(ivreport)%i(4)
			endif !if(nureport.le.0)then
		
	else !if(j_nargopt(iob,io,j_mreport).eq.1)then
		nureport=6
	endif !if(j_nargopt(iob,io,j_mreport).eq.1)then

	nureports=19   !units for writing additional reports
	ivprint=j_igetopt(iob,io,j_mprint)
	if(ivprint.gt.0)then
		iprint=j_v(ivprint)
	elseif(ivprint.eq.0)then !if(ivprint.gt.0)then
		iprint=2 !testauksen ajaksi
	else !if(ivprint.gt.0)then
		iprint=1
	endif !if(ivprint.gt.0)then
	p9=iprint.eq.9
	sparse=.false.
	! sparse-option disabled
	!if(j_linkoption(iob,io,j_msparse).gt.0) sparse=.true.
	if(j_linkoption(iob,io,j_msparse).ge.0) &
		write(6,*) '*jlp: sparse->  not available in current version of J'

	if(j_linkoption(iob,io,j_mtest).ge.0)then
		if(j_o(iob)%i(j_linkoption(iob,io,j_mtest)).eq.0)then   ! test->
			jlp_testl=.true.
		else if( j_v(j_o(iob)%i(j_linkoption(iob,io,j_mtest+1))).gt.0)then !if(j_o(iob)%i(j_linkoption(iob,io,j_mtest)).eq.0)then
			jlp_testl=.true.
		else !if(j_o(iob)%i(j_linkoption(iob,io,j_mtest)).eq.0)then
			jlp_testl=.false.   ! test->0
		end if !if(j_o(iob)%i(j_linkoption(iob,io,j_mtest)).eq.0)then
	else !if(j_linkoption(iob,io,j_mtest).gt.0)then
		jlp_testl=.false.
	end if !if(j_linkoption(iob,io,j_mtest).gt.0)then
	!debug
	p=.false.
	p2=.false.
!	ivclass=0
	! jlp_ibasclass=0
	! if(j_linkoption(iob,io,j_mclass).gt.0)then
		! ! class-> option
		! if(j_o(iob)%i(j_linkoption(iob,io,j_mclass)).ne.2)then
			! write(6,*)'**should be: class->(var,value)';j_err=.true.;return
		! endif !if(j_o(iob)%i(j_linkoption(iob,io,j_mclass)).ne.2)then
		! !ivclass=j_o(iob)%i(j_linkoption(iob,io,j_mclass)+1)
		! !class=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mclass)+2))
		! call j_printname('obtained classs ',ivclass,' ')
	! endif !if(j_linkoption(iob,io,j_mclass).gt.0)then

	! debug->
	! the 'clock' in debugging is the number of pivot operation stored in variable ipivot
	if(j_linkoption(iob,io,j_mdebug).ge.0)then
		ndebug=j_o(iob)%i(j_linkoption(iob,io,j_mdebug))  !number of arguments
		ibasdebug=j_linkoption(iob,io,j_mdebug)   ! basis for arguments
		jdebug=1     ! number of current debugging limit
		if(ndebug.eq.0)then
			p=.true.;idebug=0
			!  option debug->,
			nout=16  !file unit for writing debugging information
		else !if(ndebug.eq.0)then
			idebug=j_v(j_o(iob)%i(ibasdebug+1))   ! debug->idebug,
			if(idebug.le.0)then
				p=.true.
				nout=16
				if(ndebug.ge.2)then           ! pelaakohan tämä niinkuin pitäisi
					idebug=j_v(j_o(iob)%i(ibasdebug+2))
					jdebug=2
				endif !if(ndebug.ge.2)then
			endif !if(idebug.le.0)then

		endif !if(ndebug.eq.0)then

		write(6,*)'start debugging at Pivot=',idebug

		if(p)write(n16,*)'start debugging at start'
	else !if(j_linkoption(iob,io,j_mdebug).gt.0)then
		idebug=huge(1)
	endif !if(j_linkoption(iob,io,j_mdebug).gt.0)then
	ipivot=0

	! zmatrix, zobj optiot
	if(j_linkoption(iob,io,j_mzmatrix).gt.0)then
		if(j_nargopt(iob,io,j_mzmatrix).le.0)then
			write(6,*)'*jlp: zmatrix-> should have argument'
			j_err=.true.;goto 990
		endif !if(j_nargopt(iob,io,j_mzmatrix).le.0)then
		zmatrix=.true.
		if(j_linkoption(iob,io,j_mdata).gt.0.or.j_linkoption(iob,io,j_mproblem).gt.0.or. &
			j_linkoption(iob,io,j_mtrans).gt.0.or.j_linkoption(iob,io,j_msubtrans).gt.0) then
			write(6,*)'*jlp: illegal options with zmatrix->'
			j_err=.true.;goto 990
		endif !j_linkoption(iob,io,j_mtrans).gt.0.or.j_linkoption(iob,io,j_msubtrans).gt.0) then
		ivzmatrix=j_o(iob)%i(j_linkoption(iob,io,j_mzmatrix)+1)
		if(j_otype(ivzmatrix).ne.j_ipmatrix) then
			call j_printname('*jlp: zmatrix ',ivzmatrix,' is not valid matrix')
			j_err=.true.;goto 990
		endif !if(j_otype(ivzmatrix).ne.j_ipmatrix) then
		jlp_nrow=j_o(ivzmatrix)%i(1);nz =j_o(ivzmatrix)%i(2)
	else !if(j_linkoption(iob,io,j_mzmatrix).gt.0)then
		zmatrix=.false.
	endif !if(j_linkoption(iob,io,j_mzmatrix).gt.0)then

	zobj=.false.
	if(j_linkoption(iob,io,j_mmax).gt.0)then
		if(.not.zmatrix)then
			write(6,*)'*jlp: zmatrix-> missing'
			j_err=.true.;goto 990
		endif !if(.not.zmatrix)then
		if(j_nargopt(iob,io,j_mmax).le.0)then
			write(6,*)'*jlp: max-> should have argument'
			j_err=.true.;goto 990
		endif !if(j_nargopt(iob,io,j_mmax).le.0)then
		jlp_maxo =.true.
		zobj=.true.
		ivzobj=j_o(iob)%i(j_linkoption(iob,io,j_mmax)+1)
	endif !if(j_linkoption(iob,io,j_mmax).gt.0)then

	if(j_linkoption(iob,io,j_mmin).gt.0)then
		if(.not.zmatrix)then
			write(6,*)'*jlp: zmatrix-> missing'
			j_err=.true.;goto 990
		endif !if(.not.zmatrix)then
		if(zobj)then
			write(6,*)'*jlp: min'
			j_err=.true.;goto 990
		endif !if(zobj)then
		if(j_nargopt(iob,io,j_mmin).le.0)then
			write(6,*)'*jlp: min-> should have argument'
			j_err=.true.;goto 990
		endif !if(j_nargopt(iob,io,j_mmin).le.0)then
		jlp_maxo=.false.
		zobj=.true.
		ivzobj=j_o(iob)%i(j_linkoption(iob,io,j_mmin)+1)
	endif !if(j_linkoption(iob,io,j_mmin).gt.0)then

	if (zobj) then
		if(j_otype(ivzobj).ne.j_ipmatrix) then
			call j_printname('*jlp: max/min ',ivzobj,' is not valid vector')
			j_err=.true.;goto 990
		endif !if(j_otype(ivzobj).ne.j_ipmatrix) then
		nzodim1=j_o(ivzobj)%i(1);nzodim2 =j_o(ivzobj)%i(2)
		if((nzodim1*nzodim2).ne.nz) then
			call j_printname('*jlp: max->/min-> ',ivzobj,' should have same number elements as there are columns in zmatrix->')
			write(6,*)'Columns in zmatrix->:',nz
			write(6,*)'Elements in max->/min->:',nzodim1*nzodim2
			j_err=.true.;goto 990
		endif !if((nzodim1*nzodim2).ne.nz) then
	endif !if (zobj) then

	if(j_linkoption(iob,io,j_mrhs).gt.0)then
		if(.not.zmatrix)then
			write(6,*)'*jlp: zmatrix-option missing'
			j_err=.true.;goto 990
		endif !if(.not.zmatrix)then
		if(j_nargopt(iob,io,j_mrhs).le.0)then
			write(6,*)'*jlp: rhs-> should have argument'
			j_err=.true.;goto 990
		endif !if(j_nargopt(iob,io,j_mrhs).le.0)then
		rhsopt=.true.
		ivrhs=j_o(iob)%i(j_linkoption(iob,io,j_mrhs)+1)
		if(j_otype(ivrhs).ne.j_ipmatrix) then
			call j_printname('*jlp: rhs ',ivrhs,' is not valid vector')
			j_err=.true.;goto 990
		endif !if(j_otype(ivrhs).ne.j_ipmatrix) then
		nrhsoptdim1=j_o(ivrhs)%i(1);nrhsoptdim2 =j_o(ivrhs)%i(2)
		if((nrhsoptdim1*nrhsoptdim2).ne.jlp_nrow) then
			call j_printname('*jlp: rhs ',ivrhs,' should have same number elements as there are rows in zmatrix->')
			write(6,*)'Rows in zmatrix->:',jlp_nrow
			write(6,*)'Elements in rhs->:',nrhsoptdim1*nrhsoptdim2
			j_err=.true.;goto 990
		endif !if((nrhsoptdim1*nrhsoptdim2).ne.j_nrow) then
	else !if(j_linkoption(iob,io,j_mrhs).gt.0)then
		rhsopt=.false.
	endif !if(j_linkoption(iob,io,j_mrhs).gt.0)then

	if(j_linkoption(iob,io,j_mrhs2).gt.0)then
		if(.not.zmatrix)then
			write(6,*)'*jlp: zmatrix-option missing'
			j_err=.true.;goto 990
		endif !if(.not.zmatrix)then
		if(j_nargopt(iob,io,j_mrhs2).le.0)then
			write(6,*)'*jlp: rhs-> should have argument'
			j_err=.true.;goto 990
		endif !if(j_nargopt(iob,io,j_mrhs2).le.0)then
		rhs2opt=.true.
		ivrhs2=j_o(iob)%i(j_linkoption(iob,io,j_mrhs2)+1)
		if(j_otype(ivrhs2).ne.j_ipmatrix) then
			call j_printname('*jlp: rhs2 ',ivrhs2,' is not valid vector')
			j_err=.true.;goto 990
		endif !if(j_otype(ivrhs2).ne.j_ipmatrix) then
		nrhs2optdim1=j_o(ivrhs2)%i(1);nrhs2optdim2 =j_o(ivrhs2)%i(2)
		if((nrhs2optdim1*nrhs2optdim2).ne.jlp_nrow) then
			call j_printname('*jlp: rhs2 ',ivrhs2,' should have same number elements as there are rows in zmatrix->')
			write(6,*)'Rows in zmatrix->:',jlp_nrow
			write(6,*)'Elements in rhs->:',nrhsoptdim1*nrhsoptdim2
			j_err=.true.;goto 990
		endif !if((nrhs2optdim1*nrhs2optdim2).ne.j_nrow) then
	else !if(j_linkoption(iob,io,j_mrhs2).gt.0)then
		rhs2opt=.false.
	endif !if(j_linkoption(iob,io,j_mrhs2).gt.0)then

	ivoutresult =j_o(iob)%i(io+2+j_o(iob)%i(io+1))
	if(j_otype(ivoutresult).ne.j_ipreal)call j_del(ivoutresult)
	! if(ivoutresult.ne.j_ivresult) then
		! call j_getobject(ivoutresult,'%xvars',j_ipreal,ivout); j_v(ivout)=0.
		! call j_getobject(ivoutresult,'%xsum',j_ipreal,ivout); j_v(ivout)=0.
		! call j_getobject(ivoutresult,'%xprice',j_ipreal,ivout); j_v(ivout)=0.
		! call j_getobject(ivoutresult,'%xvarsproblem',j_ipreal,ivout); j_v(ivout)=0.
		! call j_getobject(ivoutresult,'%domains',j_ipreal,ivout); j_v(ivout)=0.
		! call j_getobject(ivoutresult,'%problemrows',j_ipreal,ivout); j_v(ivout)=0.
		! call j_getobject(ivoutresult,'%rhs',j_ipreal,ivout); j_v(ivout)=0.
		! call j_getobject(ivoutresult,'%rhs2',j_ipreal,ivout); j_v(ivout)=0.
	! endif !if(ivoutresult.ne.j_ivresult) then
	!write(6,*)'<568'
	if (zmatrix) then
		if(ivoutresult.eq.j_ivresult)then
			write(*,*)'*jlp with zmatrix-option must have output'
			j_err=.true.
			goto 990
		end if !if(ivoutresult.eq.j_ivresult)then
		!joko rhs tai rhs2 oltava
		if(.not.(rhsopt.or.rhs2opt)) then
			write(6,*)'*jlp: rhs-> or rhs2-> missing'
			j_err=.true.;goto 990
		endif !if(.not.(rhsopt.or.rhs2opt)) then
		if(allocated(jlp_rhs))deallocate(jlp_rhs,jlp_rhs2)
		allocate(jlp_rhs(1:jlp_nrow),jlp_rhs2(1:jlp_nrow))
		if(allocated(jlp_lbou))deallocate(jlp_lbou);if(allocated(jlp_ubou))deallocate(jlp_ubou)
		allocate(jlp_lbou(1:jlp_nrow),jlp_ubou(1:jlp_nrow))
		! rhs,rhs2 alustus vast. kuten problems-määrittelyssä
		jlp_rhs=-huge(1.)
		jlp_rhs2=huge(1.)
		jlp_lbou=.false.
		jlp_ubou=.false.
		if(rhsopt) then
			jlp_lbou=.true.
			jlp_rhs(1:jlp_nrow) = j_o(ivrhs)%d(1:jlp_nrow)
			do i_=1,jlp_nrow
				if(jlp_rhs(i_).eq.-1.7e37) then
					jlp_rhs(i_)=-huge(1.)
					jlp_lbou(i_)=.false.
				endif !if(j_rhs(i_).eq.-1.7e37) then
			enddo !do i_=1,j_nrow
		endif !if(rhsopt) then
		if(rhs2opt) then
			jlp_ubou=.true.
			jlp_rhs2(1:jlp_nrow) = j_o(ivrhs2)%d(1:jlp_nrow)
			do i_=1,jlp_nrow
				if(jlp_rhs2(i_).eq.1.7e37) then
					jlp_rhs2(i_)=huge(1.)
					jlp_ubou(i_)=.false.
				endif !if(j_rhs2(i_).eq.1.7e37) then
			enddo !do i_=1,j_nrow
		endif !if(rhs2opt) then
	endif !if (zmatrix) then
	
	! if(ivoutresult.ne.j_ivresult)then
	! !	call j_deflistobject(ivoutresult,'%factories',jlp_ivfact,nres=40)
		! call j_defmatrix(iout,'%rhs',jlp_nrow,1,j_matreg,ivoutrhs)
		! call j_defmatrix(iout,'%rhs2',jlp_nrow,1,j_matreg,ivoutrhs2)
		! j_o(ivoutrhs)%d(1:jlp_nrow)= jlp_rhs(1:jlp_nrow)
			! j_o(ivoutrhs2)%d(1:jlp_nrow)=jlp_rhs2(1:jlp_nrow)
		! call j_defmatrix(iout,'%rows',jlp_nrow,1,j_matreg,ivoutrows)
		! call j_defmatrix(iout,'%shprice',jlp_nrow,1,j_matreg,ivoutshprice)
	! endif
	
	
	
!write(6,*)'<569'
	! problem->
	jlp_ivprob=j_igetopt(iob,io,j_mproblem)
	if(jlp_ivprob.gt.0)then
		if(j_otype(jlp_ivprob).ne.j_ipproblem)then
			j_err=.true.
			write(*,*)'**problem-> does not refer to a legal problem'
			goto 990
		endif !if(j_otype(j_ivprob).ne.j_ipproblem)then
	else !if(j_ivprob.gt.0)then
		if(.not.zmatrix) then
			write(6,*)'**no problem given';j_err=.true.;goto 990
		endif !if(.not.zmatrix) then
	endif !if(j_ivprob.gt.0)then

	! data-> can refer to either x-data or c-data
	
	call j_getdataobject(iob,io)
	if(j_err)return
	jlp_xpresent=j_divdata.ne.0
	

	if(jlp_xpresent)then
		if(isunit)then
			ivxdata=j_divdata
			
			ivcdata=j_divdata
			jlp_ivunit=ivunit
		!	ivxmat=
		else
			
			ivcdata=j_divdata
			
			!tähän tarkistusta
			ivxdata= j_o(ivcdata)%i(3)                 !o(ivout)%i(9)=ivsub :check subdata
			if(ivxdata.le.0.or.j_otype(max(1,ivxdata)).ne.j_ipdata)then
				if(j_o(ivcdata)%i(5).ne.0.)then
					! there is up-data
					if(j_otype(j_o(ivcdata)%i(11)).ne.j_ipdata)then
						call  j_printname('**jlp: up-data  ',j_o(ivcdata)%i(11),' is not a data set');j_err=.true.;goto 990
					endif !if(j_otype(j_o(ivcdata)%i(11)).ne.j_ipdata)then
					ivxdata=ivcdata
					ivcdata=j_o(ivcdata)%i(5)

				else !if(j_o(ivcdata)%i(5).ne.0.)then
					call j_printname('**no legal data linked to data',ivcdata,' ');j_err=.true.;return
				endif !if(j_o(ivcdata)%i(5).ne.0.)then
			endif !if(ivxdata.le.0.or.j_otype(max(1,ivxdata)).ne.j_ipdata)then
			!xdisk testaus
			! ivtestmat=0
			! if(j_linkoption(iob,io,j_msubdata).gt.0)then   !

			! ivtest=j_o(iob)%i(j_linkoption(iob,io,j_msubdata)+1)
			! ivtestmat=j_o(ivtest)%i(1)
			! endif
	!write(6,*)'<578'


			jlp_ivunit=j_o(ivcdata)%i(6)  ! index of unit variable
		
			ivs=j_o(ivxdata)%i(6)     ! index of schedule variable
		
		
		endif
		
			ivxmat=j_o(ivxdata)%i(1)
	!	j_xdatinmemory=j_o(ivxmat)%i(4).gt.0

	!	j_xdatnu=-j_o(ivxmat)%i(4) !if xdat is not in file this is not used
!		write(6,*)'ivxmat j_xdatnu',ivxmat,j_xdatnu
		jlp_ivkeepx=j_o(ivxdata)%i(2)
		keepx=j_o(jlp_ivkeepx)%i(1)
		ivcmat=j_o(ivcdata)%i(1)
		jlp_ivkeepc=j_o(ivcdata)%i(2)
		keepc=j_o(jlp_ivkeepc)%i(1)
	!	write(6,*)'dgdgu',ivxmat,jlp_ivkeepx,keepx,ivcmat,jlp_ivkeepc,keepc

	else !if(j_linkoption(iob,io,j_mdata).gt.0)then
		ivcdata=0
	endif !if(j_linkoption(iob,io,j_mdata).gt.0)then


	

	! maxiter->
	if(j_linkoption(iob,io,j_mmaxiter).gt.0)then
		mxiter=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmaxiter)+1))

	else !if(j_linkoption(iob,io,j_mmaxiter).gt.0)then
		mxiter=2000
	endif !if(j_linkoption(iob,io,j_mmaxiter).gt.0)then


	if(j_linkoption(iob,io,j_mrefac).gt.0)then
		nrefac=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mrefac)+1))
	else !if(j_linkoption(iob,io,j_mrefac).gt.0)then
		nrefac=1000  !
	endif !if(j_linkoption(iob,io,j_mrefac).gt.0)then
!	write(6,*)'refac=',nrefac

	! tole->
	if(j_linkoption(iob,io,j_mtole).gt.0)then
		if(j_nargopt(iob,io,j_mtole).le.0)then
			write(6,*)'*jlp: tole-> should have argument'
			j_err=.true.;goto 990
		endif !if(j_nargopt(iob,io,j_mtole).le.0)then
		jlp_tolep=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mtole)+1))
	else !if(j_linkoption(iob,io,j_mtole).gt.0)then
		jlp_tolep=jlp_one
	endif !if(j_linkoption(iob,io,j_mtole).gt.0)then
	!write(6,*)'tole=',j_tolep
	! subfilter->
	j_subfilterlink=j_codelink(iob,io,j_msubfilter)
	subfilter_=j_subfilterlink.ne.0
	! if(j_linkoption(iob,io,j_msubfilter).gt.0)then
		! subfilter_=.true.
		! j_iosubfilter=j_linkopt2(j_msubfilter) !place of the filter inline function
		! j_ivsubfilter=j_o(iob)%i(j_linkoption(iob,io,j_msubfilter)+1)
	! else !if(j_linkoption(iob,io,j_msubfilter).gt.0)then
		! subfilter_=.false.
	! endif !if(j_linkoption(iob,io,j_msubfilter).gt.0)then

	!subreject_->
	j_subrejectlink=j_codelink(iob,io,j_msubreject)
	subreject_=j_subrejectlink.ne.0
	! if(j_linkoption(iob,io,j_msubreject).gt.0)then
		! subreject_=.true.
		! j_iosubreject=j_linkopt2(j_msubreject)
		! j_ivsubreject=j_o(iob)%i(j_linkoption(iob,io,j_msubreject)+1)
	! else !if(j_linkoption(iob,io,j_msubreject).gt.0)then
		! subreject_=.false.
	! endif !if(j_linkoption(iob,io,j_msubreject).gt.0)then
	jlp_stoplink=j_codelink(iob,io,j_mstop)
	isstop=jlp_stoplink.ne.0
	! if(j_linkoption(iob,io,j_mstop).gt.0)then
		! isstop=.true.
		! jlp_iostop=j_linkopt2(j_mstop)
		! jlp_ivstop=j_o(iob)%i(j_linkoption(iob,io,j_mstop)+1)
	! else !if(j_linkoption(iob,io,j_mstop).gt.0)then
		! isstop=.false.
	! endif !if(j_linkoption(iob,io,j_mstop).gt.0)then
	!write(6,*)'<598'

	subfilre=subfilter_.or.subreject_


	!notareavars->    tsekattava mitä yllä on tehty näiden kanssa
	notareavars=j_nargopt(iob,io,j_mnotareavars)
	linknotareavars=j_linkoption(iob,io,j_mnotareavars)

	call j_clearoption(iob,io)  ! subroutine   !!!! options checked

	stopped=.false.
	! get problem links
	! ivrhs lower bounds, if not prsenet -huge
	! ivrhs2 upper bounds
	!ivdomain text object for domain definitions
	!ivrow text object for rows
	!irowp number of rows in nonexpanded problem definition (was irow in problem function)
	!nset number of rowsets and domainsets
	! ndoms= number of all domain statements in the problem,
	! ivvars = list object containing all separate variables in problem rows
	!nval total number of coefficients
	!ndom is number of different domains
	! nrowtot total number of rows after expanding domainsets
	! irowobject object row probably after expanding (now must be first row)
	! ivdomainvars list object containg all variables in domain definitions


	if(.not.zmatrix) then   !!!! setting vectors
		ivrhs=j_o(jlp_ivprob)%i(1)
		ivrhs2=j_o(jlp_ivprob)%i(2)
		jlp_ivdomain=j_o(jlp_ivprob)%i(3)
		jlp_ivrow=j_o(jlp_ivprob)%i(4)
		irowp=j_o(jlp_ivprob)%i(5)
		nset=j_o(jlp_ivprob)%i(6)
		ndoms=j_o(jlp_ivprob)%i(7)
		ivvars=j_o(jlp_ivprob)%i(8)
		nval=j_o(jlp_ivprob)%i(9)
		jlp_ndom=j_o(jlp_ivprob)%i(10)
		nrowtot=j_o(jlp_ivprob)%i(11)
		irowobj=j_o(jlp_ivprob)%i(12)
		j_ivperiod=j_o(jlp_ivprob)%i(14)
		istree=j_ivperiod.ne.0
		if(istree)then
			nperiods=maxval(j_o(j_ivperiod)%d)
			write(6,7777)'periods ',nperiods
		endif !if(istree)then
		jlp_nrow=nrowtot

		! nrow tell the total number of rows when domain sets are expanded
7777 format(a16,i5)
		if(jlp_xpresent)write(6,7777)'domains',max(jlp_ndom,1)
		if(max(jlp_ndom,1).ne.ndoms) write(6,*)'number of domain occurences =',ndoms

		if(irowobj.gt.0)jlp_nrow=jlp_nrow-1

		if(jlp_nrow.le.1.and.sparse)then
			write(6,*)'*note: with ', jlp_nrow, 'rows option sparse-> ignored'
			sparse=.false.
		endif !if(j_nrow.le.1.and.sparse)then
		!nsetr number of rows in each domainset
		!nsetd=nubber of domains in each domainset
		!isetd list of all domain statements (each occurence is counted separately)
		!nvars number of vars in each row
		!irowvars  variables occuring in problem row , each occurence counted separately
		! coef coefficients of the irowvars variables

		jlp_nsetr=>j_o(jlp_ivprob)%i2(1:nset)  !i+1 will match nsetd(i)
		jlp_nsetd=>j_o(jlp_ivprob)%i2(nset+1:2*nset)
		jlp_isetd=>j_o(jlp_ivprob)%i2(2*nset+1:2*nset+ndoms)
		jlp_nvars=>j_o(jlp_ivprob)%i2(2*nset+ndoms+1:2*nset+ndoms+irowp)
		jlp_irowvars=>j_o(jlp_ivprob)%i2(2*nset+ndoms+irowp+1:2*nset+ndoms+irowp+nval)
		jlp_coef=>j_o(jlp_ivprob)%d(1:nval)
		nvartot=j_o(ivvars)%i(1)  !total number of variables in the problem
		nz=nvartot ! added 112018 JL
		write(6,7777)'constraint rows',irowp-1
		if(jlp_nrow.gt.irowp-1) &
			write(6,*)'number of expanded constraint rows=',jlp_nrow
		!1. allocate  -goto 950 to deallocate
		if(allocated(jlp_rhs))deallocate(jlp_rhs);if(allocated(jlp_rhs2))deallocate(jlp_rhs2)
		allocate(jlp_rhs(1:jlp_nrow),jlp_rhs2(1:jlp_nrow))
	endif !if(.not.zmatrix) then

	!!!!initilization continues tole

	if(allocated(jlp_tole))deallocate(jlp_tole)
	allocate(jlp_tole(0:jlp_nrow))

	! nnf = number of infeasible rows
	nnf=jlp_nrow  !initially all infeasible
	nnfold2=jlp_nrow ! old number of infeasible rows is initially the same
	jlp_tole=jlp_epsj*jlp_tolep

	if (.not.zmatrix) then    !!!! initilaization continues
		if(allocated(jlp_lbou))deallocate(jlp_lbou);if(allocated(jlp_ubou))deallocate(jlp_ubou)
		allocate( jlp_lbou(1:jlp_nrow));allocate( jlp_ubou(1:jlp_nrow)) !deallo at 950
		! lbou and ubou tells if there is lower or upper bound for the row
		! they are defien in jlpmod -module in this file

		!generate rhs
		! irow0 goes over the rows in the problem definition not taking into account
		! multiple domains
		! irow goes over the rows when the domainsets are expanded
		!
		irow=0;irow0=0
		do i=1,nset   ! number of sets domains,the rows belonging to these domains
			irow0bas = irow0
			do j=1,jlp_nsetd(i) !number of domains in set i
				irow0 = irow0bas
				do k=1,jlp_nsetr(i) ! number of rows in set i
					irow0=irow0+1
					if(irow0.ne.irowobj)then
						!constraint row
						irow=irow+1
						jlp_rhs(irow)=j_o(ivrhs)%d(irow0)
						jlp_rhs2(irow)=j_o(ivrhs2)%d(irow0)

						! if(j_rhs(irow).gt.-huge(1.).and.j_rhs2(irow).lt.huge(1))then
						! j_tole(irow)=max(j_tolep*0.0001*(j_rhs2(irow)-j_rhs(irow)),j_tole(irow))
						! elseif(j_rhs(irow).gt.-huge(1.))then
						! j_tole(irow)=max(j_tolep*0.0001*abs(j_rhs(irow)),j_tole(irow))
						! else
						! j_tole(irow)=max(j_tolep*0.0001*abs(j_rhs2(irow)),j_tole(irow))
						! endif

					else if(irow0.eq.irowobj.and.j.eq.1)then !if(irow0.ne.irowobj)then
						! for maximization rhs1 =huge  rhs2=0
						! for minimization  rhs2=-huge
						if(j_o(ivrhs2)%d(irow0).eq.0.)then
							jlp_maxo=.true.
							coefmax=1.
						else !if(j_o(ivrhs2)%d(irow0).eq.0.)then
							jlp_maxo=.false.
							coefmax=-1.
						end if !if(j_o(ivrhs2)%d(irow0).eq.0.)then
					end if !if(irow0.ne.irowobj)then
				end do !do k=1,j_nsetr(i)
			end do !do j=1,j_nsetd(i)
		end do !do i=1,nset
		jlp_lbou=jlp_rhs.gt.-huge(1.)   ! is there lower bound
		jlp_ubou=jlp_rhs2.lt.huge(1.)   ! is there upper bound
	endif !if (.not.zmatrix) then
!write(6,*)'<768'
	!!!! intilization continues

	if(p.and.jlp_nrow.le.50)p2=.true.
	if(p2)then
		write(n16,*)'rhs',jlp_rhs
		write(n16,*)'rhs2',jlp_rhs2
		write(n16,*)'maxo',jlp_maxo
		write(n16,*)'lbou',jlp_lbou
		write(n16,*)'ubou',jlp_ubou
	endif !if(p2)then

	if(allocated(jlp_ix))deallocate(jlp_ix)
	allocate(jlp_ix(0:jlp_nrow));jlp_ix=0
	if(.not.zmatrix) then
		! 2. allocata  !goto 940 to deallocate
		! isx: is a variable a x-variable
		if(allocated(jlp_isx))deallocate(jlp_isx);allocate(jlp_isx(1:nvartot)) ;jlp_isx=.false.
		! could make conditional of fpresent
		if(allocated(jlp_itransv))deallocate(jlp_itransv);allocate(jlp_itransv(1:20))
		if(allocated(jlp_isxval))deallocate(jlp_isxval);allocate(jlp_isxval(1:nval)) ;jlp_isxval=.false.
		if(allocated(jlp_nxrow))deallocate(jlp_nxrow);allocate(jlp_nxrow(1:irowp)) ;jlp_nxrow=0
		if(allocated(jlp_ixcur))deallocate(jlp_ixcur)
		if(allocated(jlp_ixcurrows))deallocate(jlp_ixcurrows)
		if(allocated(jlp_irowdomain))deallocate(jlp_irowdomain)
		if(allocated(jlp_irowrow))deallocate(jlp_irowrow);allocate(jlp_irowrow(0:jlp_nrow))
		allocate(jlp_ixcur(0:jlp_nrow),jlp_irowdomain(0:jlp_nrow))
		jlp_ixcur=0;jlp_irowdomain=0

		! ix tells the index of the temporary x variable in each row ix(ir)=0 -> there is no x -varts in row
		allocate(jlp_ixcurrows(1:jlp_nrow+1))
	endif !if(.not.zmatrix) then

	! rhscur: the current rhs (either lower or upper bound)
	3096 continue
	if(allocated(jlp_rhscur))deallocate(jlp_rhscur);allocate( jlp_rhscur(1:jlp_nrow))

	! rhsw: the working rhs where the sums over key schedules are subtracted
	if(allocated(jlp_rhsw))deallocate(jlp_rhsw);allocate( jlp_rhsw(1:jlp_nrow))

	if(jlp_xpresent)then   !xpresent means that data-> , even if all variables  !!!!
		! are factory variables
		ialldomain=j_line(jlp_ivdomain,'All') !which domain is the All-domain
		nxvar=0  ! number of x-variables in the problem definition
		ncvar=0  ! number of c-variables in the definition, Note these are used in the same way
		! as x variables, they just get their values from the c data (unit data)

		!fact
		nfx=0 ;nfy=0  !

		! c-variables
		if(allocated(jlp_cvarl))deallocate(jlp_cvarl)

		isxrow=0

		!	ifykeep : laskuri tehdas-y-muuttujien ifyvarskeep-vektorille (puutavaralajimuuttujien xmatriisi-sarakkeet)
		!	ifyfact : laskuri tehdas-y-muuttujien ifyfactfact-vektorille (tehdasmuuttujien paikka factories-listassa)
		ifykeep = 0
		ifyfact = 0
		!Käydään läpi tehtävän muuttujat, muodostetaan xkyk- ja tehdaslistat
		do i0=1,nvartot
	!	write(6,*)'<756ivvars',ivvars,nvartot
			i=j_o(ivvars)%i2(i0)
		!	write(6,*)'<756ivvarsi ',i
			!iout-mjalle arvo (gfortran korjaus)
			! j_err -käsittelyt funktioista palatessa
			ikeep=j_inlistobject(i,j_o(ivxdata)%i(2)) !call j_isindata(i,ivxdata,ikeep)
			if(j_err) return
			call isfvar(i,itrans,imat,len1,iv2,iv3)
			if(j_err) return
			if(iv2.ne.0)then	! tehdasmuuttuja
				if(.not.jlp_fpresent) then
					!tehdastehtävässä pakotetaan output
					if(ivoutresult.eq.j_ivresult) then
						write(6,*) '**factory problems require output for jlp'
						j_err=.true.
						goto 930
					endif !if(ivoutresult.eq.j_ivresult) then
					call j_deflistobject(ivoutresult,'%xkyk',jlp_ivxkyk,nres=40)
					call j_deflistobject(ivoutresult,'%factories',jlp_ivfact,nres=40)

					if(allocated(jlp_isfx))deallocate(jlp_isfx);allocate(jlp_isfx(1:nvartot)) ;jlp_isfx=.false.
					if(allocated(jlp_isfy))deallocate(jlp_isfy);allocate(jlp_isfy(1:nvartot)) ;jlp_isfy=.false.

					ntrans = 0
					jlp_fpresent=.true.
					if(jlp_nrow.eq.0)then
						write(6,*)'**in factory problems there must be constraints (sorry)'
						write(6,*)'use constraints which require that timber variables transported to factories are at least zero'
						j_err=.true.
						return
					endif !if(j_nrow.eq.0)then

				endif !if(.not.j_fpresent) then

				if(itrans.eq.0) then ! xk
					!iv2 is variable, test if it is in xdata-matrix
					ijo=j_inlistobject( iv2, jlp_ivkeepx)
					if(ijo.le.0)then
						call j_printname('***variable ',iv2,' is not in x-data matrix')
						j_err=.true.
						return
					endif !if(ijo.le.0)then
					iperk=j_putlistobject(jlp_ivxkyk,single=iv2) !j_putlist2(iv2,jlp_ivxkyk)
					jlp_isfx(i0)=.true.
					nfx=nfx+1

					iperk=j_putlistobject(jlp_ivfact,single=iv3) 
					! ii_ = j_inlistobject(iv3,jlp_ivfact)
					! if(ii_.le.0) then
						! iperk= j_putlist2(iv3,jlp_ivfact)
					! endif !if(ii_.le.0) then

				else !if(itrans.eq.0) then
					ntrans = ntrans+1
					jlp_itransv(ntrans)=itrans
					do ij=1,j_o(iv2)%i(1)
						ijo=j_inlistobject( j_o(iv2)%i2(ij), j_o(ivxdata)%i(2))
						if(ijo.le.0)then
							call j_printname('*variable ',j_o(iv2)%i2(ij),' is not in x-data matrix ')
							
							j_err=.true.
							return
						endif !if(ijo.le.0)then
						iperk=j_putlistobject(jlp_ivxkyk,single=j_o(iv2)%i2(ij)) ! ,jlp_ivxkyk)
					enddo !do ij=1,j_o(iv2)%i2(1)
					! do ij=1,j_o(iv3)%i(0)
						! if(j_inlistobject(j_o(iv3)%i(ij),jlp_ivfact).le.0) then
							! iperk= j_putlist2(j_o(iv3)%i(ij),jlp_ivfact)
						! endif !if(j_inlistobject(j_o(iv3)%i(ij),j_ivfact).le.0) then

					! enddo !do ij=1,j_o(iv3)%i(0)
					iperk=j_putlistobject(jlp_ivfact,ivin=iv3)
					
					if(j_err)return

					jlp_isfy(i0)=.true.
					nfy=nfy+1

				endif !if(itrans.eq.0) then
			endif !if(iv2.ne.0)then
		enddo !do i0=1,nvartot

		!tehdas-tilanvaraukset ja alustukset
		if(jlp_fpresent) then   !!!! initilization for factories

			if(allocated(jlp_isfxval))deallocate(jlp_isfxval);allocate(jlp_isfxval(1:nval)) ;jlp_isfxval=.false.
			if(allocated(jlp_isfyval))deallocate(jlp_isfyval);allocate(jlp_isfyval(1:nval)) ;jlp_isfyval=.false.
			if(allocated(jlp_nfxrow))deallocate(jlp_nfxrow);allocate(jlp_nfxrow(1:jlp_nrow+1)) ;jlp_nfxrow=0
			if(allocated(jlp_nfyrow))deallocate(jlp_nfyrow);allocate(jlp_nfyrow(1:jlp_nrow+1)) ;jlp_nfyrow=0
			if(allocated(jlp_ifxcurrows))deallocate(jlp_ifxcurrows);allocate(jlp_ifxcurrows(1:jlp_nrow+1))
			if(allocated(jlp_ifycurrows))deallocate(jlp_ifycurrows);allocate(jlp_ifycurrows(1:jlp_nrow+1))
			jlp_ifycurrows=0
			if(allocated(jlp_ixcurfact))deallocate(jlp_ixcurfact);allocate(jlp_ixcurfact(0:jlp_nrow))

			if(allocated(jlp_irowfxvars))deallocate(jlp_irowfxvars) !corresponding fx-variables
			if(allocated(jlp_irowfyvars))deallocate(jlp_irowfyvars) !corresponding fy-variables
			if(allocated(jlp_irowffact))deallocate(jlp_irowffact) !corresponding fx-variables
			if(allocated(jlp_irowfyfact))deallocate(jlp_irowfyfact) !corresponding fy-variables
			if(allocated(jlp_irowfkeep))deallocate(jlp_irowfkeep) !corresponding fx-variables
			allocate(jlp_irowfxvars(1:nval)); allocate(jlp_irowfyvars(1:nval))
			allocate(jlp_irowffact(1:nval));  allocate(jlp_irowfyfact(1:nval))
			allocate(jlp_irowfkeep(1:nval))

			if(allocated(jlp_ibafykeep))deallocate(jlp_ibafykeep) ; allocate(jlp_ibafykeep(1:nval))
			if(allocated(jlp_ifyvarskeep))deallocate(jlp_ifyvarskeep); allocate(jlp_ifyvarskeep(1:50)) 
			  ! iv2-listojen mjien xmat-sarakkeet
			if(allocated(jlp_ixkykkeep))deallocate(jlp_ixkykkeep)
			allocate(jlp_ixkykkeep(1:j_o(jlp_ivxkyk)%i(1))) ! xkyk-listan mjien xmat-sarakkeet
			if(allocated(jlp_ibafyfact))deallocate(jlp_ibafyfact) 
			allocate(jlp_ibafyfact(1:nval))
			if(allocated(jlp_ifyvarsxkyk))deallocate(jlp_ifyvarsxkyk)
			allocate(jlp_ifyvarsxkyk(1:50)) ! iv2-listojen mjien paikat xkyk-listassa
			if(allocated(jlp_ifyfactfact))deallocate(jlp_ifyfactfact)
			allocate(jlp_ifyfactfact(1:50)) ! iv3-listojen tehtaiden paikat factories-listassa

			if(allocated(jlp_xkfact))then
				if(warmf.and.size(jlp_xkfact,dim=2).ne. j_o(jlp_ivfact)%i(0))then
					write(6,*)'*number of factories is different, warm-> ignored for factories'
					warmf=.false.
				endif !if(warmf.and.size(j_xkfact,dim=2).ne. j_o(j_ivfact)%i(0))then
				deallocate(jlp_xkfact)
			else !if(allocated(j_xkfact))then
				warmf=.false.  !factories not allocated
			endif !if(allocated(j_xkfact))then

			allocate(jlp_xkfact(1:j_o(jlp_ivxkyk)%i(1),1:j_o(jlp_ivfact)%i(1)))

			if(allocated(jlp_nxkfact))deallocate(jlp_nxkfact)
			allocate(jlp_nxkfact(1:j_o(jlp_ivxkyk)%i(1)))
			jlp_nxkfact = 0

			!zerocapacity
			!allocate(zeroc(1:j_o(j_ivxkyk)%i(1),1:j_o(j_ivfact)%i(0)))
			!zeroc=.false.

			!tehdas-yk muuttujiin liittyvien muunnosten puutavaralaji-/tehdasmja -output muuttujien indeksit
			!rivi-indeksi = mjan järjestysnro xkyk-listalla, sarakeindeksi = tehtaan järjestysnro factories-listalla
			if(allocated(jlp_fyfactout))deallocate(jlp_fyfactout)
			allocate(jlp_fyfactout(1:j_o(jlp_ivxkyk)%i(1),1:j_o(jlp_ivfact)%i(1)))
			jlp_fyfactout = 0

			!apuvektori a-matriisin päivitysarvojen laskentaan (xkf-muuttuja kantaan)
			!value_*(0) : tavoiterivi; value_*(1:nrow): a-matriisi
			if(allocated(jlp_value_af))deallocate(jlp_value_af)
			allocate(jlp_value_af(0:jlp_nrow))
			if(allocated(jlp_valueopt_af))deallocate(jlp_valueopt_af)
			allocate(jlp_valueopt_af(0:jlp_nrow))
			if(allocated(jlp_valuek_af))deallocate(jlp_valuek_af)
			allocate(jlp_valuek_af(0:jlp_nrow))

			!xkyk-listan mjien indeksit x-matriisissa
			do i_=1,j_o(jlp_ivxkyk)%i(1)
				jlp_ixkykkeep(i_)=j_inlistobject(j_o(jlp_ivxkyk)%i2(i_),jlp_ivkeepx)
			enddo !do i_=1,j_o(j_ivxkyk)%i(1)

		endif !if(j_fpresent) then

		!Käydään läpi kaikki tehtävässä olevat muuttujat
		!write(6,*)'<5555>',nvartot,i0
		do i0=1,nvartot       !***********************************
			i=j_o(ivvars)%i2(i0)
		!	call j_printname(' dd',i,' ll')
			! testing if a variable is in x data,
			!  ikeep,  positions in keep
			! iout= position in the list of output variables of transformation allocated with data
			ikeep=j_inlistobject(i,j_o(ivxdata)%i(2))  !call j_isindata(i,ivxdata,ikeep)
		!		write(6,*)'<77ikeep',ikeep
			!j_err -käsittelyt funktioista palatessa
			if(j_err) return
			iout=0
			if(jlp_ivsubtrans.gt.0)iout=j_inlistobject(i,j_trans_output(jlp_ivsubtrans))
		!write(6,*)'<77iout',iout
			if(ikeep.le.0.and.iout.le.0)then  !is variable in cdata
				ikeep=j_inlistobject(i,j_o(ivcdata)%i(2)) !call j_isindata(i,ivcdata,ikeep)
				if(jlp_ivtrans.gt.0)iout=j_inlistobject(i,j_trans_output(jlp_ivtrans))
				if(ikeep.gt.0.or.iout.gt.0)then
					ncvar=ncvar+1
					if(ncvar.eq.1)allocate( jlp_cvarl(1:nvartot-i0+1))  !deallocated earlier
					! note that cvarl is not 'standard' list where element 0 tells the number of elements in the list
					jlp_cvarl(ncvar)=i
				endif !if(ikeep.gt.0.or.iout.gt.0)then
			endif !if(ikeep.le.0.and.iout.le.0)then
			!!!??? what happens when variable is both x and c variable???
			!onko x-muuttuja
			nxval=0   !added by JL 23.1.2021

			if(ikeep.gt.0.or.iout.gt.0)then
				!note both c-variables and x-variables are in optimization x-variables
				jlp_isx(i0)=.true.;nxvar=nxvar+1
				ival=0
				do j=1,irowp  !row definitions
					do k=1,jlp_nvars(j) !nvars number of vars in each row
						ival=ival+1
						!irowvars  variables occuring in problem row , each occurence counted separately
						if(jlp_irowvars(ival).eq.i)then
							jlp_isxval(ival)=.true.  ! is x variaböe
							nxval=nxval+1
							jlp_nxrow(j)=jlp_nxrow(j)+1      !number of x-variables in row j
						endif !if(j_irowvars(ival).eq.i)then
					enddo !do k=1,j_nvars(j)

				enddo !do j=1,irowp
				!factory
				!tarkistetaan onko vuorossa oleva muuttuja tehdasmuuttuja	(vai z-muuttuja)
			else !if(ikeep.gt.0.or.iout.gt.0)then

				!fac     ******************************
				!test factory

				ivar=i

				call isfvar(ivar,itrans,imat,len1,iv2,iv3)
				! tells if variable is of type 'sawlog#2%%Keuruu' or
				! of type 'util%%sawlog%%sawmill'
				! if it is of the latter type itrans is tranformation trans%util%%sawlog%%sawmill
				! if cost%%pulp#1
				if(j_err)return

				!iv2 == 0 --> kyseessä muu kuin tehdasmuuttuja (eli z-muuttuja)
				!iv2 != 0 --> kyseessä tehdasmuuttuja
				if(iv2.ne.0)then !************
					! xkyk%: lista, joka sisältää niiden tehdas-x-muuttujien indeksit, jotka ovat joko tehtävässä tai y-muuttujissa
					!if like util%%sawlog%%sawmill itrans is the object trans%util%%sawlog%%sawmill
					! imat is matrix matrix%util%%sawlog%%sawmill  :: NOT USED YET
					!isindata:  ivkeep=o(ivdata)%i(2);jlp_ivtrans=o(ivdata)%i(6);ivoul=ivoutputlistjlp_ivtrans)

					if(itrans.ne.0)then !****!itrans= object trans%util%%sawlog%%sawmill
						! iv2 is list, test if all elements are in xdata matrix
						!	- kustakin iv2 x iv3 listan parista testataan löytyykö itrans-muunnoksen output-arvot
						!	- o(ivnames): kaikkien olioiden nimet (teksti-olio)
						!	- o(ivnames)%i(i) : i:nnen muuttujan nimen ensimmäisen merkin indeksi o(ivname)-tekstioliossa
						!	- o(itrans)%i2(2) : itrans-muunnoksen output-mjien listan indeksi o-vektorissa
						!	- o(iv2)%i(0),o(iv3)%i(0): iv2- & iv3-listojen alkioden määrä
						!	- o(iv2)%i(ii) : iv2-listan ii:nnen alkion indeksi o-vektorissa

						itransoutl=j_o(itrans)%i2(2)  !outputlist
						leni=j_o(j_ivnames)%i(i+1)-j_o(j_ivnames)%i(i)

						do ii=1,j_o(iv2)%i(1)
							iv2el=j_o(iv2)%i2(ii)
							lentrim=j_o(j_ivnames)%i(iv2el+1)-j_o(j_ivnames)%i(iv2el)
							lxk = j_inlistobject(j_o(iv2)%i2(ii),jlp_ivxkyk) !muuttujan järjestysnro xkyk listassa
							if(lxk.eq.0)then
								call j_getname(iv2,j_o(iv2)%i2(ii))
								write(6,*)'4646',j_oname(1:j_loname),' ',j_oname(1:j_loname)
								call j_getname(jlp_ivxkyk)
								write(6,*)'4647',j_oname(1:j_loname)
							endif
							!testattava löytyykö iv2el datasta >> testattu yllä  do ij=1,o(iv2)%i(0) -silmukassa??
							!ivel2 talteen xkyk-listaan

							toka:	   			do jj=1,j_o(iv3)%i(1)
								! test if util%%iv2%%iv3 variable exists and is among the outputvariables of iv1
								iv3el=j_o(iv3)%i2(jj)
								len3=j_o(j_ivnames)%i(iv3el+1)-j_o(j_ivnames)%i(iv3el)

								lff = j_inlistobject(j_o(iv3)%i2(jj),jlp_ivfact)

								! Tarkistetaan, ettei tehdasta ole jo lisätty ko muuttujalle
								do inx=1,jlp_nxkfact(lxk)
									if(jlp_xkfact(lxk,inx)%ifact.eq.lff) goto 728
								enddo !do inx=1,j_nxkfact(lxk)
								! tehdasta ei löytynyt taulukosta
								! kasvatetaan ko muuttujan tehtaiden lkm ja lisätään tehdas taulukkoon
								jlp_nxkfact(lxk) = jlp_nxkfact(lxk)+1
								jlp_xkfact(lxk,jlp_nxkfact(lxk))%ifact=lff

								728							continue

								!Tarkistetaan löytyvätkö iv2- x iv3-listoista muodostuvien parien muuttujanimet kaikkien olioiden nimien joukosta
								!	- käydään läpi kaikkien olioiden nimet, verrataan ylemmissä silmukoissa muodostuvaan iv2 x iv3 parin nimiin
								!	- jos nimi löytyy, pitää tarkistaa löytyykö nimi itrans-muunnoksen output-muuttujien listasta
								nameloop:  			do ivn=1,j_o(j_ivnames)%i(0)
									if(lentrim+len3+len1+4.ne. &
									j_o(j_ivnames)%i(ivn+1)-j_o(j_ivnames)%i(ivn))&
										& cycle nameloop
									ibas1=j_o(j_ivnames)%i(ivn)-1
									ibas2=j_o(j_ivnames)%i(i)-1
									do iel=1,len1+2
										if(j_o(j_ivnames)%ch( ibas1+iel).ne. &
											& j_o(j_ivnames)%ch( ibas2+iel))cycle nameloop
									enddo !do iel=1,len1+2
									!Vastaavilla loopeilla toinen (iv2) ja kolmas (iv3) osa
									ibas1=ibas1+len1+2
									ibas2 = j_o(j_ivnames)%i(iv2el)-1
									do iel = 1,lentrim
										if(j_o(j_ivnames)%ch( ibas1+iel).ne. &
											& j_o(j_ivnames)%ch( ibas2+iel))cycle nameloop
									enddo !do iel = 1,lentrim
									ibas1=ibas1+lentrim+2
									ibas2 = j_o(j_ivnames)%i(iv3el)-1
									do iel = 1,len3
										if(j_o(j_ivnames)%ch( ibas1+iel).ne. &
											& j_o(j_ivnames)%ch( ibas2+iel))cycle nameloop
									enddo !do iel = 1,len3
									!Testataan löytyykö itrans-muunnoksen output-muuttujista
									! ivn otettava talteen (xps:n laskentaa varten)
									ivtemp=j_inlistobject(ivn,j_o(itrans)%i2(2))
									if(ivtemp.gt.0)then
										!otetaan talteen muunnoksen outputmjan indeksi
										jlp_fyfactout(lxk,lff) = ivn
										goto 2578
									else !if(ivtemp.gt.0)then
										call j_printname('variable ',ivn,' ')
										call j_printname('not among outputvariables of',itrans,' ')
										j_err=.true.
										return
									endif !if(ivtemp.gt.0)then
									!Jos vastaavutta ei löydy, pitäisi palautua virheilmoitus
								enddo nameloop !nameloop:  			do ivn=1,j_o(j_ivnames)%i(0)
								call j_printname('not found outputvariable for ',iv2el,' ')
								call j_printname(' and ',iv3el,' ')
								j_err=.true.
								return

								2578						enddo toka !toka:	   			do jj=1,j_o(iv3)%i(0)
						enddo !do ii=1,j_o(iv2)%i(1)

						iiv2_ = 0
						iiv3_ = 0

						ival=0
						do j=1,irowp  !row definitions
							do k=1,jlp_nvars(j) !nvars number of vars in each row
								ival=ival+1
									!irowvars  variables occuring in problem row , each occurence counted separately
								if(jlp_irowvars(ival).eq.i)then
									jlp_isfyval(ival)=.true.  ! is y variable
									jlp_irowfyvars(ival)=iv2
									jlp_irowfyfact(ival)=iv3
									jlp_nfyrow(j)=jlp_nfyrow(j)+1      !number of y-variables in row j

										!iv2-listan muuttujien xmat-sarakkeet
										! Tarkistetaan, onko lista jo käsitelty (= onko iv2 jo aiemmin laitettu irowfyvars:iin)
									if(ival.gt.1) iiv2_ = j_inlist1(iv2,ival-1,jlp_irowfyvars(1:ival-1))
									if(iiv2_.eq.0) then ! iv2-listaa ei ole vielä käsitelty
										jlp_ibafykeep(ival)=ifykeep+1 ! Tästä indeksistä alkaa ifyvarskeep:issä iv2:n muuttujiin liittyvät xmat-sarakkeet
											! Puretaan iv2-lista ja haetaan listan muuttujien xmat-sarakkeet
										do ii=1,j_o(iv2)%i(1)
											iv2elpos=j_inlistobject(j_o(iv2)%i2(ii),jlp_ivkeepx)
											if(iv2elpos.le.0) then
												call j_printname('**variable ',j_o(iv2)%i2(ii),' is not in x-data matrix')
												do ij=1,j_o(jlp_ivkeepx)%i(1)
													write(6,*)j_o(iv2)%i2(ii),j_o(jlp_ivkeepx)%i2(ij),&
													j_o(iv2)%i2(ii).eq.j_o(jlp_ivkeepx)%i2(ij)
												enddo
												j_err=.true.
												return
											endif !if(iv2elpos.le.0) then
											ifykeep = ifykeep+1
											call j_puti(jlp_ifyvarskeep,ifykeep,iv2elpos)
												!ptl-muuttujan paikka xkyk-listassa talteen
											call j_puti(jlp_ifyvarsxkyk,ifykeep,j_inlistobject(j_o(iv2)%i2(ii),jlp_ivxkyk))
										enddo !do ii=1,j_o(iv2)%i(1)
									else !if(iiv2_.eq.0) then
										jlp_ibafykeep(ival) = jlp_ibafykeep(iiv2_)
									endif !if(iiv2_.eq.0) then

										!tehdas-listan tehtaiden paikat factories-listalla talteen
										!Tarkistetaan, onko lista jo käsitelty (= onko iv3 jo aiemmin laitettu irowfyvars:iin)
									if(ival.gt.1) iiv3_ = j_inlist1(iv3,ival-1,jlp_irowfyfact(1:ival-1))
									if(iiv3_.eq.0) then ! iv3-tehdaslistaa ei ole vielä käsitelty -> etsitään paikat factories-listasta
										jlp_ibafyfact(ival)=ifyfact+1 !Tästä indeksistä alkaa ifyfactfact:issä iv3:n tehtaiden paikat factories-listassa
										do ii=1,j_o(iv3)%i(1)
											ifyfact=ifyfact+1
												! Ei tarvinne tarkistaa löytyykö factories-listasta, koska lisäys tehdään heti tehdas-y-mjan käsittelyn aluksi
											call j_puti(jlp_ifyfactfact,ifyfact,j_inlistobject(j_o(iv3)%i2(ii),jlp_ivfact))
										enddo !do ii=1,j_o(iv3)%i(1)
									else !if(iiv3_.eq.0) then
										jlp_ibafyfact(ival) = jlp_ibafyfact(iiv3_)
									endif !if(iiv3_.eq.0) then

									if(jlp_coef(ival).ne.1.) then
										write(6,*)'Betas must be 1'
										j_err=.true.
										return
									endif !if(j_coef(ival).ne.1.) then
								endif !if(j_irowvars(ival).eq.i)then
							enddo !do k=1,j_nvars(j)
						enddo !do j=1,irowp

					else !if(itrans.ne.0)then
						lxk = j_inlistobject(iv2,jlp_ivxkyk)
						lff = j_inlistobject(iv3,jlp_ivfact)
						do inx=1,jlp_nxkfact(lxk)
							if(jlp_xkfact(lxk,inx)%ifact.eq.lff) goto 729
						enddo !do inx=1,j_nxkfact(lxk)
							! tehdasta ei löytynyt taulukosta
							! kasvatetaan ko muuttujan tehtaiden lkm ja lisätään tehdas taulukkoon
						jlp_nxkfact(lxk) = jlp_nxkfact(lxk)+1
						jlp_xkfact(lxk,jlp_nxkfact(lxk))%ifact=lff

							729					continue

						ival=0
						do j=1,irowp  !row definitions
							do k=1,jlp_nvars(j) !nvars number of vars in each row
								ival=ival+1
									!irowvars  variables occuring in problem row , each occurence counted separately
								if(jlp_irowvars(ival).eq.i)then
									jlp_isfxval(ival)=.true.  ! is x variaböe
									jlp_irowfxvars(ival)=iv2
									jlp_irowffact(ival)=iv3
									jlp_irowfkeep(ival)=j_inlistobject(iv2, jlp_ivkeepx)
									jlp_nfxrow(j)=jlp_nfxrow(j)+1      !number of x-variables in row j
								endif !if(j_irowvars(ival).eq.i)then
							enddo !do k=1,j_nvars(j)
						enddo !do j=1,irowp

					endif !if(itrans.ne.0)then
				endif !if(iv2.ne.0)then

			endif !if(ikeep.gt.0.or.iout.gt.0)then
		enddo !do i0=1,nvartot

		if(nxvar+nfx+nfy.le.0)then
			write(6,*)'**all variables are z variables even if data->';j_err=.true.;goto 940
		end if !if(nxvar+nfx+nfy.le.0)then

		!xpresent2: onko tehtävässä tavanomaisia x-mjia
		jlp_xpresent2 = nxvar.ne.0
!write(6,*)'<6665>',nvartot,nxvar
		if(.not.zmatrix) nz=nvartot-nxvar-nfx-nfy
		write(6,7777)'x-variables',nxvar
		if(nz.gt.0)write(6,7777)'z-variables',nz
		if(jlp_fpresent)write(6,7777)'factories',j_o(jlp_ivfact)%i(1)
		if(nfx.gt.0)write(6,7777)'xkf-variables',nfx
		if(nfy.gt.0)write(6,7777)'ykf-variables',nfy
		!write(6,*)'<4446>nz,j_fpresent,intapp',nz,j_fpresent,intapp
		intapp=nz.eq.0.and..not.jlp_fpresent.and.intapp
		!write(6,*)'<4447>nz,j_fpresent,intapp',nz,j_fpresent,intapp
		!tehdasoptimoidaan vain, jos tavoitefunktiossa ykf-mjia
		if(jlp_fpresent.and.(nfy == 0)) then
			write(6,*)'***error*** ykf variables must be present is a factory problem'
			j_err = .true.
			return
		endif !if(j_fpresent.and.(nfy == 0)) then

		!20181116 #zeroc moved here from #z_commented
		!  zeroc with z-variables needs to be determined before keyfactories

		jlp_mxd=jlp_nrow+4  !???max number of d-vectors mx number of
	else !if(j_xpresent)then
		jlp_mxd=0
		intapp=.false.
	endif !if(j_xpresent)then

	! mitkä olivat xvar-muuttujat
	la=jlp_nrow ! number of rows in Fletcher
	jlp_lavec(1)=la
	mxn=jlp_mxd+nz !mx number of columns in A  , I.e. D+ coefficients of z-variables
	if(jlp_fpresent) mxn=mxn+jlp_mxd
	ncol=mxn !!!
	mxnm=mxn+jlp_nrow !mx number of columns (icluding the I part)
	if(allocated(jlp_ls))deallocate(jlp_ls);if(allocated(jlp_lsi))deallocate(jlp_lsi)
	allocate(jlp_ls(1:mxnm),jlp_lsi(1:mxnm))   ! ls= list of columns in A which are in the basis
	! i.e. ls contains columns in ld-list plus columns of z-part of the A matrix
	if(p) write(n16,*)'lsmax',mxnm
	if(allocated(jlp_a))deallocate(jlp_a)
	! sparse*******************************************************************
	if(sparse)then  !should be tested if it works, not commented yer
		! allocate(asp(nrow*(mxn+1))
		!c Fletcher:
		!c   The matrix A contains gradients of the linear terms in the objective
		!c  function (column 0) and the general constraints (columns 1:m).

		!c  In this sparse format, these vectors have dimension  a(1:nnza)  and
		!c   la(0:lamax), where nnza is the number of nonzero elements in A,
		!c  and lamax is at least  nnza+m+2.  The last m+2 elements in la are pointers.
		!c  The vectors a(.) and la(.) must be set as follows:
		!c  a(j) and la(j) for j=1,nnza are set to the values and row indices (resp.)
		!c  of all the nonzero elements of A. Entries for each column are grouped
		!c  together in increasing column order.
		!c  The last m+2 elements of la(.) contain pointers to the first elements in
		!c  the column groupings. Thus la(la(0)+i) for i=0,m is set to the location
		!c  in a(.) containing the first nonzero element for column i of A. Also
		!c  la(la(0)+m+1) is set to nnza+1 (the first unused location in a(.)).
		!c  Finally la(0) is also a pointer which points to the start of the pointer
		!c  information in la. la(0) must be set to nnza+1 (or a larger value if it
		!c  is desired to allow for future increases to nnza).
		nnzj=jlp_nrow*(ncol+1) !reserve for col 0
		if(allocated(jlp_lavecsp))deallocate(jlp_lavecsp)
		allocate(jlp_lavecsp(0:nnzj+ncol+2))
		if(allocated(last))deallocate(last)
		allocate(last(0:ncol))
		if(allocated(jlp_acol))deallocate(jlp_acol);allocate(jlp_acol(1:jlp_nrow));jlp_acol=0.d0
		if(allocated(jlp_acolapu))deallocate(jlp_acolapu);allocate(jlp_acolapu(1:jlp_nrow));jlp_acolapu=0.d0;
		if(allocated(jlp_icolapu))deallocate(jlp_icolapu);allocate(jlp_icolapu(1:jlp_nrow));jlp_icolapu=0.d0

		jlp_lavecsp=0
		!rmemeber column zero
		!c  la(la(0)+m+1) is set to nnza+1
		jlp_lavecsp(0)=nnzj+1
		jlp_lavecsp(nnzj+ncol+2)=nnzj+1 !

		ie=0
		jlp_lavecsp(nnzj+1)=1
		do i=1,ncol
			jlp_lavecsp(nnzj+i+1)=jlp_lavecsp(nnzj+i)+jlp_nrow
		enddo !do i=1,ncol
		! c  The last m+2 elements of la(.) contain pointers to the first elements in
		!c  the column groupings. Thus la(la(0)+i) for i=0,m is set to the location
		!c  in a(.) containing the first nonzero element for column i of A. Also
		!c  la(la(0)+m+1) is set to nnza+1 (the first unused location in a(.)).

		!c  Finally la(0) is also a pointer which points to the start of the pointer
		!c  information in la. la(0) must be set to nnza+1 (or a larger value if it
		!c  is desired to allow for future increases to nnza).
		!  write(6,*)'lavecsp',lavecsp
	endif !if(sparse)then

	allocate(jlp_a(1:jlp_nrow,0:mxn) ) ;jlp_a=0.d0         ! A matrix
	if(allocated(jlp_objr0))deallocate(jlp_objr0)
	if(allocated(jlp_objr2))deallocate(jlp_objr2)
	allocate( jlp_objr0(1:mxnm));allocate( jlp_objr2(1:mxnm));;jlp_objr0=0.d0;jlp_objr2=0.d0
	if(allocated(jlp_xmi))deallocate(jlp_xmi,jlp_xma);if(allocated(jlp_xma))deallocate(jlp_xma)
	allocate( jlp_xmi(1:mxnm),jlp_xma(1:mxnm))
	jlp_xmi=jlp_zero ;jlp_xma=jlp_zero
	jlp_a=jlp_zero
	!*objr0 the 'official' objective row
	!*objr2 the working obective row wehen the problem is infeasible
	jlp_objr0=jlp_zero;jlp_objr2=jlp_zero

	! z variables in the problem *******************
	if(nz.gt.0)then  !nz=number of z-variables
		write(6,*)'number of z-variables ',nz
		!! commented. March 2011  if(iout.ne.ivresult)then
		! jlp-function has output iout
		if(.not.zmatrix) then
			!if(ivoutresult.ne.j_ivresult) then
			!	call j_deflistobject(ivoutresult,'%zvars',ivzvar,nres=nz)
			!	jlp_zvarl=>j_o(ivzvar)%i2
		!		write(6,*)'jlpzv ',jlp_zvarl
			!else !if(ivoutresult.ne.j_ivresult) then
				if(allocated(jlp_zvarl))deallocate(jlp_zvarl)
				allocate(jlp_zvarl(1:nz))
			!endif !if(ivoutresult.ne.j_ivresult) then
			! no output for the jlp-function
			! z-variables: not z-variable, not factory´xvariables or factory y-variables
			if (jlp_fpresent) then
				jlp_zvarl(1:nz)=pack(j_o(ivvars)%i2(1:nvartot),.not.jlp_isx.and..not.jlp_isfx.and..not.jlp_isfy)
			else !if (j_fpresent) then
				jlp_zvarl(1:nz)=pack(j_o(ivvars)%i2(1:nvartot),.not.jlp_isx)
				
		!		write(6,*)'zvr2',jlp_zvarl(1:nz)
			endif !if (j_fpresent) then
			call j_deflistobject(ivoutresult,'%zvars',ivzvar,list0=nz,list=jlp_zvarl(1:nz))
			
			jlp_zvarl0=nz
			! nval= number of coefficients in the
			nzval=nval-nxval   !muuta kun fact
			idomba=0  ! basis for values
			irow0=0
			irow=0
			do i=1,nset  !number of domainset
				do j=1,jlp_nsetd(i) !number of domains in set i
					ival=idomba    !the same values are repeated fro each domain
					do k=1,jlp_nsetr(i) !number of rows in set i
						if(j.eq.1) irow0=irow0+1   !row accoding to intial (nonexpanded) row numbering
						if(irow0.ne.irowobj.or.j.gt.1)then ! k.gt.1)then
							!constraint row
							irow=irow+1
							do ii=1,jlp_nvars(irow0)
								ival=ival+1   ! the number of variable with coefficient
								if(.not.jlp_isxval(ival))then        !is the variable not x-variable
									iz=j_inlist(jlp_irowvars(ival),jlp_zvarl0,jlp_zvarl)  !is it z-variable
									! fact
									! put coefficients of z variables into A matrix
									jlp_a(irow,iz)=jlp_coef(ival)
								endif !if(.not.j_isxval(ival))then
							enddo !do ii=1,j_nvars(irow0)
						else if(irow0.eq.irowobj.and.j.eq.1)then !if(irow0.ne.irowobj.or.j.gt.1)then
							! the objective row, pick coefficients into objr0
							! if the objective row is in a domain set and there are several domain
							! then the objective function can refer only to the first domain
							do ii=1,jlp_nvars(irow0)
								ival=ival+1
								if(.not.jlp_isxval(ival))then
									iz=j_inlist(jlp_irowvars(ival),jlp_zvarl0,jlp_zvarl)
									if(jlp_maxo)then  ! objective maximized
										jlp_objr0(jlp_nrow+iz)=jlp_coef(ival) !object row
									else !if(j_maxo)then
										jlp_objr0(jlp_nrow+iz)=-jlp_coef(ival) ! -objective maximized
									endif !if(j_maxo)then
								endif !if(.not.j_isxval(ival))then
							enddo !do ii=1,j_nvars(irow0)
						endif !if(irow0.ne.irowobj.or.j.gt.1)then
					enddo !do k=1,j_nsetr(i)
				enddo !do j=1,j_nsetd(i)
				idomba=ival
			enddo !do i=1,nset
		else !if(.not.zmatrix) then

			iel=0
			jlp_objr0(jlp_nrow+1:jlp_nrow+nz)=j_o(ivzobj)%d(1:nz)
			do i=1,jlp_nrow
				do j=1,nz
					iel=iel+1
					jlp_a(i,j)=j_o(ivzmatrix)%d(iel)
				enddo !do j=1,nz
			enddo !do i=1,j_nrow
		endif !if(.not.zmatrix) then

		if(p2)then
			write(n16,'(a,(10f8.2/))')'obj row', jlp_objr0
			write(n16,*)'amat'
			do jj7=1,jlp_nrow;write(n16,'(10f8.2)')(jlp_a(jj7,nco7),nco7=1,ncol) ;enddo
		endif !if(p2)then
		!pack no
		if(sparse)then  !sivuutetaan sparse toistaiseksi
			do i=1,nz
				ie=0
				istart1=jlp_lavecsp(jlp_lavecsp(0)+i)-1
				do j=1,jlp_nrow
					if(jlp_a(j,i).ne.jlp_zero)then
						ie=ie+1
						jlp_a(ie,i)=jlp_a(j,i)
						jlp_lavecsp(istart1+ie)=j
					endif !if(j_a(j,i).ne.j_zero)then
				enddo !do j=1,j_nrow
				last(i)=jlp_lavecsp(istart1)+ie
			enddo !do i=1,nz
		endif !if(sparse)then

		!zerocapacity
		!poistetaan zerocista rajoitteet joilla z-muuttujia !!!!
		!20181116 #zeroc moved to #zeroc_z
		!		if(j_fpresent)then
		!			do irowj_ = 2,irowp
		!				if(j_nfxrow(irowj_).eq.1.and. j_nfyrow(irowj_).eq.0.and.j_ix(irowj_-1).eq.0)then !.and.j_ix(irowj_).eq.0)then !
		!					jxk=j_irowfxvars(j_ibafx(irowj_)+1)
		!					jf = j_irowffact(j_ibafx(irowj_)+1)
		!					do iz=1,nz
		!						if(j_a(irowj_-1,iz).ne.0.)zeroc(jxk,jf)=.false.
		!					enddo
		!				endif
		!			enddo
		!		endif !		if(j_fpresent)then

	endif !if(nz.gt.0)then
	!**************************************** z-variables present


	if(jlp_xpresent)then
		!20181116 #zeroc block moved from #z-commented ends here


		!muuttuja-tehdas-taulukko
		if(jlp_fpresent) then  !!!!
			if(.not.jlp_maxo.and.jlp_nfyrow(1)>0) then
				write(6,*)'***error***  Only maximization is allowed if ykf variables in objective row'
				j_err = .true.
				return
			endif !if(.not.j_maxo.and.j_nfyrow(1)>0) then

			!onko tavoiterivillä tehdas-x- ja/tai -y-mjia
			jlp_isxkyk0 = (jlp_nfxrow(1)>0).or.(jlp_nfyrow(1)>0)

			nfxval = count(jlp_isfxval)
			if(allocated(jlp_coeffx))deallocate(jlp_coeffx) !tehdasmuuttujien kertoimet
			allocate(jlp_coeffx(1:nfxval));jlp_coeffx=0.d0 !nfxval=count(isfxval)

			if(allocated(jlp_ifxcurrows))deallocate(jlp_ifxcurrows) !corresponding fx-variables
			if(allocated(jlp_ifycurrows))deallocate(jlp_ifycurrows) !corresponding fx-variables
			if(allocated(jlp_ibafx))deallocate(jlp_ibafx) !corresponding fx-variables
			if(allocated(jlp_ibafy))deallocate(jlp_ibafy) !corresponding fy-variables
			allocate(jlp_ifxcurrows(1:jlp_nrow+1));allocate(jlp_ibafx(1:irowp))
			allocate(jlp_ifycurrows(1:jlp_nrow+1));allocate(jlp_ibafy(1:irowp))

			if(allocated(jlp_irow2curix))deallocate(jlp_irow2curix)
			allocate(jlp_irow2curix(0:jlp_nrow,1:irowp))
			jlp_irow2curix = 0

			jlp_ibafx(1)=0
			do j=2,irowp
				jlp_ibafx(j)=jlp_ibafx(j-1)+jlp_nfxrow(j-1)
			enddo !do j=2,irowp

			jlp_ibafy(1)=0
			do j=2,irowp
				jlp_ibafy(j)=jlp_ibafy(j-1)+jlp_nfyrow(j-1)
			enddo !do j=2,irowp

			jlp_coeffx=pack(jlp_coef,jlp_isfxval)
			if(nfxval>0) then
				call pack2(jlp_irowfxvars,jlp_isfxval,nval)
				call pack2(jlp_irowffact,jlp_isfxval,nval)
				call pack2(jlp_irowfkeep,jlp_isfxval,nval)
			endif !if(nfxval>0) then

			if(count(jlp_isfyval)>0) then
				call pack2(jlp_irowfyvars,jlp_isfyval,nval)
				call pack2(jlp_irowfyfact,jlp_isfyval,nval)
				call pack2(jlp_ibafykeep,jlp_isfyval,nval)
				call pack2(jlp_ibafyfact,jlp_isfyval,nval)
			endif !if(count(j_isfyval)>0) then

			nyxkf_ = 0	! xk/tehdas -yhdistelmien kokonaismäärä yk-esiintymissä
			do i_ = 1, count(jlp_isfyval)
				nyxkf_ = nyxkf_ + j_o(jlp_irowfyvars(i_))%i(1)*j_o(jlp_irowfyfact(i_))%i(1)
			enddo !do i_ = 1, count(j_isfyval)
			nfxfyexp=nfxval+ nyxkf_
			if(allocated(jlp_xkykrowvars))deallocate(jlp_xkykrowvars)
			allocate(jlp_xkykrowvars(1:nfxfyexp))

			! vaihdetaan irowfxvars:in indeksointi ivxkyk-listan mukaiseksi
			do j=1,nfxval
				jj = j_inlistobject(jlp_irowfxvars(j),jlp_ivxkyk)
				jlp_irowfxvars(j)=jj
				jj = j_inlistobject(jlp_irowffact(j),jlp_ivfact)
				jlp_irowffact(j)=jj
			enddo !do j=1,nfxval


			!'xkf-kantaan laskennan' aputaulukoiden muodostaminen
			irowvars_ = 1
			do ixkyk_=1,j_o(jlp_ivxkyk)%i(1)	!	xkyk-lista
				do inf_=1, jlp_nxkfact(ixkyk_) !xkyk-mjiin liittyvät tehtaat
					jlp_xkfact(ixkyk_,inf_)%i1xkykrowvar = irowvars_
					if_ = jlp_xkfact(ixkyk_,inf_)%ifact
					do irowj_ = 1,irowp ! tehtävärivit (alkup. ei lavennetut)
						do k=1,jlp_nfxrow(irowj_) ! rivin xk-muuttujat
							jxk = jlp_irowfxvars(jlp_ibafx(irowj_)+k) 	!paikka xkyk-listassa
							jf = jlp_irowffact(jlp_ibafx(irowj_)+k)		!paikka tehdas-listassa
							if((ixkyk_.eq.jxk).and.(if_.eq.jf)) then
								jlp_xkykrowvars(irowvars_)%isxk = .true.
								jlp_xkykrowvars(irowvars_)%irow = irowj_
								jlp_xkykrowvars(irowvars_)%ind = jlp_ibafx(irowj_)+k
								irowvars_ = irowvars_ + 1
							endif !if((ixkyk_.eq.jxk).and.(if_.eq.jf)) then
						enddo !do k=1,j_nfxrow(irowj_)
						do k=1,jlp_nfyrow(irowj_) !rivin yk-mjat
							listy=jlp_irowfyvars(jlp_ibafy(irowj_)+k) !yk-mjaa vastaava xk-lista
							listf=jlp_irowfyfact(jlp_ibafy(irowj_)+k) !yk-mjaa vastaava tehdas-lista
							do ixk_=1,j_o(listy)%i(1) !xk-muuttujat
								!paikka xkyk-listassa
								!ibafy(irowj): tavoiterivin yk-muuttujien alkukohta -1 ibafykeep:issä
								!ibafykeep(ibafy(irowj)+k): tavoitefunktion k:nnen yk-mjan puretun xk-listan mjien alkukohta ifyvarskeep:ssä ja ifyvarsxksyk:ssa
								jxk = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj_)+k)+ixk_-1) !tavoitefunktion j:nnen yk-mjan puretun xk-listan k:nnen xk-mjan paikka xkyk-listassa
								do iif_=1,j_o(listf)%i(1) !tehtaat
									!paikka tehdas-listassa
									jf = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj_)+k)+iif_-1)
									if ((ixkyk_.eq.jxk).and.(if_.eq.jf)) then
										jlp_xkykrowvars(irowvars_)%isxk = .false.
										jlp_xkykrowvars(irowvars_)%irow = irowj_
										jlp_xkykrowvars(irowvars_)%ind = jlp_fyfactout(jxk,jf)
										irowvars_ = irowvars_ + 1
									endif !if ((ixkyk_.eq.jxk).and.(if_.eq.jf)) then
								enddo !do iif_=1,j_o(listf)%i(1)
							enddo !do ixk_=1,j_o(listy)%i(1)
						enddo !do k=1,j_nfyrow(irowj_)
					enddo !do irowj_ = 1,irowp
					jlp_xkfact(ixkyk_,inf_)%inxkykrowvar = irowvars_-1
				enddo !do inf_=1, j_nxkfact(ixkyk_)
			enddo !do ixkyk_=1,j_o(j_ivxkyk)%i(1)

			!zerocapacity		   !!!!
			do irowj_ = 2,irowp
	!		write(6,*)'row ',irowj_,jlp_rhs2(irowj_-1),jlp_nfxrow(irowj_),jlp_nfyrow(irowj_),&
		!	jlp_ix(irowj_-1)
				if(jlp_nfxrow(irowj_).eq.1.and. jlp_nfyrow(irowj_).eq.0.and. &
				jlp_ix(irowj_-1).eq.0)then !.and.j_ix(irowj_).eq.0)then !
					!z-muuttujat tsekattva a-matriisista)

					jxk=jlp_irowfxvars(jlp_ibafx(irowj_)+1)
					jf = jlp_irowffact(jlp_ibafx(irowj_)+1)
					zeroc=jlp_rhs2(irowj_-1).eq.j_0

			!		if(rh_.eq.0.)zeroc=.true.  !(jxk,jf)=.true.

					!20181116 #zeroc_z
					do iz=1,nz
						if(jlp_a(irowj_-1,iz).ne.0.)zeroc=.false.  !(jxk,jf)=.false.
					enddo !do iz=1,nz
					if(zeroc)then  !J-err=.true.
						write(6,*)'at row' ,irowj_, 'there is zero capacity for timber ',jxk,&
							'and factory ',jf
						write(6,*)'remove the row and put the price or utility negative'
						j_err=.true.

					endif !if(zeroc)then

				endif !if(j_nfxrow(irowj_).eq.1.and. j_nfyrow(irowj_).eq.0.and.j_ix(irowj_-1).eq.0)then
			enddo !do irowj_ = 2,irowp
			if(j_err)return

		endif !if(j_fpresent) then

		if(nz.gt.0.and. .not.zopt)then
			write(6,*)'*jlp: if there are z-variables, there must be z-> option, zvariables:'
			nnz=0
			! printing z-variables
			do i=1,nvartot
				!fact
				!isfx ja isfy tarkistetaan vain, jos on tehdastehtävä
				fvari = .false.
				if (jlp_fpresent) then
					fvari = jlp_isfx(i).or.jlp_isfy(i)
				endif !if (j_fpresent) then
				if(.not.jlp_isx(i).and..not.fvari)then
					nnz=nnz+1
					call j_printname(' ',j_o(ivvars)%i2(i),' ')
					if(nnz.ge.10.and.nnz.lt.nz)then
						write(6,*)' ...etc...'
						exit
					endif !if(nnz.ge.10.and.nnz.lt.nz)then
				endif !if(.not.j_isx(i).and..not.fvari)then
			enddo !do i=1,nvartot
			j_err=.true.
			return
		endif !if(nz.gt.0.and. .not.zopt)then

		iiro=0
		ndd=1
		! now isx tell for all variables in vars%problem if they are x variable
		! nxrow tell for each row the number of x-variables in the row
		! isxval tells for each element in coef and irowvars if it is x-variable

		! starts acces to c-data: !!!!
		call j_getdat(ivcdata,jlp_nunits,jlp_ivmatc,jlp_ivkeepc) !,jlp_ivtransc,ivvarsc)

		if(nfx.gt.0.or.nfy>0) then
			!Avaintehtaiden alustus !!!!
			if(allocated(jlp_keyfact))then
				if(warmf)then
					if(size(jlp_keyfact,dim=1).ne.jlp_nunits.or. &
						size(jlp_keyfact,dim=2).ne.j_o(jlp_ivxkyk)%i(1))then
						write(6,*)'*jlp: dimensions of problem do not agree with the previous problem',&
							' warm-> ignored for factories'
						warmf=.false.
					endif !size(j_keyfact,dim=2).ne.j_o(j_ivxkyk)%i(1))then
				endif !if(warmf)then
				if(.not.warmf)deallocate(jlp_keyfact)
			endif !if(allocated(j_keyfact))then
			if(.not.allocated(jlp_keyfact))allocate(jlp_keyfact(1:jlp_nunits,1:j_o(jlp_ivxkyk)%i(1)))
			if(p9)write(16,*)zeroc
			if(warmf)then
				do i=1,jlp_nunits
					do j=1,j_o(jlp_ivxkyk)%i(1)
						do keyf_=1,jlp_nxkfact(j)
							if(jlp_xkfact(j,keyf_)%ifact.eq.jlp_keyfact(i,j))then
								! if(zeroc(j,j_keyfact(i,j)))then
									! do keyf2_=1,j_nxkfact(j)
										! jf2_=j_xkfact(j,keyf2_)%ifact
										! if(.not.zeroc(j,jf2_))then
											! j_keyfact(i,j)=jf2_
											! if(p)write(16,*)'uusi',i,j,jf2
											! goto 171
										! endif
									! enddo
									! !write(6,*)'zeroc jäi',i,j,j_keyfact(i,j)
								! endif !if(zeroc(j,j_keyfact(i,j)))then
								goto 171
							endif !if(j_xkfact(j,keyf_)%ifact.eq.j_keyfact(i,j))then
						enddo !do keyf_=1,j_nxkfact(j)
						warmf=.false.
						write(6,*)'*factories do not agree warm-> ignored for factories'
						goto 172
						171			continue
					enddo !do j=1,j_o(j_ivxkyk)%i(1)
				enddo !do i=1,jlp_nunits
			endif !if(warmf)then

			172			if(.not.warmf)then
				do j=1,j_o(jlp_ivxkyk)%i(1)
					keyf_ = 1
				179				jf_=jlp_xkfact(j,keyf_)%ifact
				! if(zeroc(j,jf_))then
					! keyf_=keyf_+1
					! if(keyf_.gt.j_nxkfact(j))then
						! keyf_ = 1
						! cycle
					! endif
					! goto 179
				! endif
					jlp_keyfact(1,j) =keyf_
				enddo !do j=1,j_o(j_ivxkyk)%i(1)

				do i=2,jlp_nunits
					do j=1,j_o(jlp_ivxkyk)%i(1)
						if(jlp_keyfact(i-1,j).lt.jlp_nxkfact(j)) then

							keyf_=jlp_keyfact(i-1,j)+1

						else !if(j_keyfact(i-1,j).lt.j_nxkfact(j)) then
							keyf_ = 1
						endif !if(j_keyfact(i-1,j).lt.j_nxkfact(j)) then
						init_=keyf_
					180					jf_=jlp_xkfact(j,keyf_)%ifact
					! if(zeroc(j,jf_))then
						! keyf_=keyf_+1
						! if(keyf_.gt.j_nxkfact(j))then
							! keyf_ = 1
						! endif
						! if(keyf_.eq.init_)cycle
						! goto 180
					! endif
						jlp_keyfact(i,j) =keyf_
					enddo !do j=1,j_o(j_ivxkyk)%i(1)
				enddo !do i=2,jlp_nunits

! yritetään tehdä uusiksi  uuskey

			! do iunit=1,1  !jlp_nunits
			! keepc=j_o(j_ivkeepc)%i(1)   !yleiseen käyttöön
			! iiba=(iunit-1)*keepc
			! !do j=1,j_o(j_ivkeepc)%i(1)
				! j_v(j_o(j_ivkeepc)%i(1:keepc))=j_o(ivcmat)%d(iiba+1:iiba+keepc)
			! !enddo !do j=1,j_o(j_ivkeepc)%i(1)
			! do j=1,ntrans
				! call dotrans(j_itransv(j),1)
				! ivoutl=j_o(j_itransv(j))%i2(2)
			! !	j_o(ivout)%i2(2)=ivoutl
				! do iou=1,j_o(ivoutl)%i(1)
				! call j_printname('tas ',j_o(ivoutl)%i(iou),' oli')
				! write(6,*)j_v(j_o(ivoutl)%i(iou))
				! enddo

			! enddo
			! write(6,*)'<65>n'

			! do j_=1,1  !j_nrowpfy ! rivit, joilla yk-muuttujia vois tehdä alioh
	! jcurix_=j_ifycurrows(j_)
	! write(6,*)'jcurix',jcurix_
	! irowj_ = j_irowrow(jcurix_)
	! write(6,*)'irowj_,j_nfyrow(irowj_)',irowj_,j_nfyrow(irowj_)
	! do k_=1,j_nfyrow(irowj_) ! silmukka : rivin yk-muuttujat
		! listy_=j_irowfyvars(j_ibafy(irowj_)+k_) !yk-mjaa vastaava ptl-lista
		! listf_=j_irowfyfact(j_ibafy(irowj_)+k_) !yk-mjaa vastaava tehdas-lista
		! do ivars_=1,j_o(listy_)%i(1) ! yk-mjan puutavaralistan muuttujat
			! iv2elpos_ = j_ifyvarskeep(j_ibafykeep(j_ibafy(irowj_)+k_)+ivars_-1) !mjan xmat-sarake
			! iv2xykypos_ = j_ifyvarsxkyk(j_ibafykeep(j_ibafy(irowj_)+k_)+ivars_-1) !mjan paikka xkyk-listassa
			! do ifact_=1,j_o(listf_)%i(1) ! yk-mjan tehdaslistan tehtaat
				! iv3factpos_ = j_ifyfactfact(j_ibafyfact(j_ibafy(irowj_)+k_)+ifact_-1) !tehtaan paikka factories-listassa
				! if(j_keyfact(iunit,iv2xykypos_).eq.iv3factpos_) then
					! nrowykfkey = nrowykfkey + 1
					! j_rowykfkey(nrowykfkey)%ivfout = j_fyfactout(iv2xykypos_,iv3factpos_)
					! call j_printname('ny',j_rowykfkey(j_)%ivfout,' =')
					! write(6,*)j_v(j_rowykfkey(j_)%ivfout)

					! j_rowykfkey(nrowykfkey)%iv2elpos = iv2elpos_
					! j_rowykfkey(nrowykfkey)%jcurix = jcurix_
				! endif !if(j_keyfact(iunit,iv2xykypos_).eq.iv3factpos_) then
			! enddo !do ifact_=1,j_o(listf_)%i(1)
		! enddo !do ivars_=1,j_o(listy_)%i(1)
	! enddo !do k_=1,j_nfyrow(irowj_)
! enddo !do j_=1,j_nrowpfy
			! do j_=1,nrowykfkey
	! !	iv2elpos_ = j_rowykfkey(j_)%iv2elpos
	! !	if(j_rowykfkey(j_)%jcurix.eq.3)write(16,*)'tas2 ',j_, &
	! !		j_valueopt_af(j_rowykfkey(j_)%jcurix),j_v(j_rowykfkey(j_)%ivfout),&
	! !		j_o(ivxmat)%d(ibaobs+iv2elpos_),j_o(ivxmat)%d(ibakey_+iv2elpos_)  !xdatiba
		! !j_valueopt_af(j_rowykfkey(j_)%jcurix) = j_valueopt_af(j_rowykfkey(j_)%jcurix) + &
		! write(6,*)'j_',j_,	j_v(j_rowykfkey(j_)%ivfout)  !*&
		! !	(j_o(ivxmat)%d(ibaobs+iv2elpos_) -&
		! !	j_o(ivxmat)%d(ibakey_+iv2elpos_))
	! enddo !do j_=1,nrowykfkey
		! enddo !do i=1,jlp_nunits
		! stop 641






			!muutetaan keyfactin alkiot suoraan factories-listan indekseiksi
				do i=1,jlp_nunits
					do j=1,j_o(jlp_ivxkyk)%i(1)
						jlp_keyfact(i,j) = jlp_xkfact(j,jlp_keyfact(i,j))%ifact
					enddo !do j=1,j_o(j_ivxkyk)%i(1)
				enddo !do i=1,jlp_nunits
			endif !if(.not.warmf)then

		endif !if(nfx.gt.0.or.nfy>0) then


	! start access to x-data:  !!!!
		call j_getdat(ivxdata,jlp_lopp,jlp_ivmatx,jlp_ivkeepx)
	! if(j_xdatinmemory)then
			! j_xdatlast=j_lopp
			! j_xdatlopp=j_lopp

	! endif

	!		write(6,*)'**********ivxmat,j_ivmatx',j_ivmatx,ivxmat,j_lopp

		if(.not.isunit)then
		j_ivns=j_o(ivcdata)%i(4)   !variable 'Ns' telling the number of schedules in each unit (nobsw variable)
		iiv=j_inlistobject(j_ivns,jlp_ivkeepc)
		if(iiv.le.0)then
			call j_printname('**nobsw variable',j_ivns,' not in the cddata')
			j_err=.true.
			goto 940
		endif !if(iiv.le.0)then
	endif


	!!!!???????????  is it really possible that obsw variable is not in cdata
	!  I think that error ocuurrs in data or linkdta functions if this is not the case
	!JL 23.1 20121  nxval is now computed above, it was used earlier even if it was not define
	!nxval=count(j_isxval) !isxval tells if coefficient in problem is a x-variable
		ntemp=count(jlp_nxrow.gt.0) !number of rows with x-variables, # of temporary x-variables

	! ivdomains =ouput variables for domain transformations
	! Note: ivdomains is not good name because it looks like the index of a J object
		if(allocated(jlp_ivdomains))deallocate(jlp_ivdomains)
		allocate(jlp_ivdomains(1:jlp_ndom))

	! nxrowtemp is the number of x-variables when making each of the temporary x-variables
		if(allocated(jlp_ibatemp))deallocate(jlp_ibatemp)
		if(allocated(jlp_nxrowtemp))deallocate(jlp_nxrowtemp)

		allocate(jlp_ibatemp(1:ntemp),jlp_nxrowtemp(1:ntemp))
		jlp_ibatemp=0;jlp_nxrowtemp=0

		if(allocated(jlp_ixprow))deallocate(jlp_ixprow)
		allocate(jlp_ixprow(1:irowp));jlp_ixprow=0  ! for each problem statement row, what is the temporary

		if(allocated(jlp_coefx))deallocate(jlp_coefx) !coefficients of x-variables
		if(allocated(jlp_irowxvars))deallocate(jlp_irowxvars) !corresponding x-variables
		allocate(jlp_coefx(1:nxval));allocate(jlp_irowxvars(1:nxval)) !nxval=count(isxval)
		jlp_coefx=0.d0;jlp_irowxvars=0

	!generate how to compute xmat

		if(allocated(jlp_nxrow2))deallocate(jlp_nxrow2)
		allocate(jlp_nxrow2(1:ntemp))
	!take coefficients of x, and the correponding variables
		jlp_coefx=pack(jlp_coef,jlp_isxval);jlp_irowxvars=pack(jlp_irowvars,jlp_isxval)
	!take
		jlp_nxrow2=pack(jlp_nxrow,jlp_nxrow.gt.0) !numbers of x-variables in each row containg x-variables

	!ongelma: jos eri riveillä sama temporal variable, niin se lasketaan moneen kertaan
	! ratkaisu: käydään rivit läpi, ja katsotaan onko samoja,

	!allocoi a
	!pistä a:han sarakkeet
	!generoi rhs
	!nrow
		irow=0  !new numbering of rows
		irow0=0 !initial numbering of rows
	! irow0 goes over the rows in the problem definition not taking into account
	! multiple domains

		itemp=0 !the number of the temp-varaible
		idomba=0 !basis for domains in isetd
		md=0 !number of x-rows in the problem
		iba=0

	!*************   here it is checked if the same coefficients and
	! xvariables  appears in different rows , make then only one temporary x variable
		do i=1,nset ! number of sets
		! first domain first
			irowini=irow
			kobj=0
			do k=1,jlp_nsetr(i) ! number of rows in set i (before expanding over domains)
				irow0=irow0+1
				if(jlp_nxrow(irow0).gt.0)then
				!check previous rows
					iba2=0
					do iro=1,irow0-1
						if(jlp_nxrow(iro).eq.jlp_nxrow(irow0))then
							do jj=1,jlp_nxrow(iro)
							! check if coefficient or variable is different
								if(jlp_irowvars(iba+jj).ne.jlp_irowvars(iba2+jj).or.&
									jlp_coefx(iba+jj).ne.jlp_coefx(iba2+jj))goto 194
							enddo !do jj=1,j_nxrow(iro)
						!!!!     iro fllows problem statement rows
							itemp0=jlp_ixprow(iro); goto 195 !write(6,*)'samma x',iro,irow0,'ix',itemp0
						endif !if(j_nxrow(iro).eq.j_nxrow(irow0))then
					194    iba2=iba2+jlp_nxrow(iro)
					enddo !do iro=1,irow0-1
					itemp=itemp+1
					itemp0=itemp
					jlp_ibatemp(itemp)=iba
					jlp_nxrowtemp(itemp)=jlp_nxrow(irow0)
				! ibatemp is the basis for accesing irowvars and coefx for temporary variables
				! nxrowtemp is the number of variables/coefficients
				195  continue
					iba=iba+jlp_nxrow(irow0)
					jlp_ixprow(irow0)=itemp0
				else !if(j_nxrow(irow0).gt.0)then
					jlp_ixprow(irow0)=0
				endif !if(j_nxrow(irow0).gt.0)then

				j=1
				if(irow0.ne.irowobj)then ! .or.k.gt.1)then
				!constraint row
					irow=irow+1
					jlp_irowrow(irow)=irow0
					if(jlp_nxrow(irow0).gt.0)then

						jlp_ix(irow)=itemp0
						jlp_irowdomain(irow)=jlp_isetd(idomba+j)
						md=md+1
					elseif(jlp_fpresent) then !if(j_nxrow(irow0).gt.0)then
						if(jlp_nfxrow(irow0)>0.or.jlp_nfyrow(irow0)>0) &
						jlp_irowdomain(irow)=jlp_isetd(idomba+j)
					elseif(jlp_isetd(idomba+j).ne.ialldomain)then !if(j_nxrow(irow0).gt.0)then
						write(6,*)'**there is is a row without x variables in the domain:'
						call j_printtext(jlp_ivdomain,jlp_isetd(idomba+j))
						j_err=.true.
						return
					endif !if(j_nxrow(irow0).gt.0)then
				elseif(irow0.eq.irowobj.and.k.eq.1)then !if(irow0.ne.irowobj)then
					jlp_irowrow(0)=irow0
					if(jlp_nxrow(irow0).gt.0)then
						jlp_ix(0)=itemp0
						jlp_irowdomain(0)=jlp_isetd(idomba+j)
					elseif(jlp_fpresent) then !if(j_nxrow(irow0).gt.0)then
						if(jlp_nfxrow(irow0)>0.or.jlp_nfyrow(irow0)>0) &
						jlp_irowdomain(0)=jlp_isetd(idomba+j)
					endif !if(j_nxrow(irow0).gt.0)then
					kobj=k
				endif !if(irow0.ne.irowobj)then
			enddo !do k=1,j_nsetr(i)

			do j=2,jlp_nsetd(i) ! nsetd tells the number of domains in each domain set
				iii=irowini
				do k=1,jlp_nsetr(i) ! nsetr tells the number of rows in each row set (between two domain definitions)
					if(k.ne.kobj)then
						irow=irow+1
						iii=iii+1
						jlp_ix(irow)=jlp_ix(iii)  ! ix is the index of temporary x variable
						jlp_irowdomain(irow)=jlp_isetd(idomba+j) !irowdomain tells for each row what is the domain definition
					!   isetd list of all domain statements
						jlp_irowrow(irow)=jlp_irowrow(irow-jlp_nsetr(i))
					endif !if(k.ne.kobj)then

				enddo !do k=1,j_nsetr(i)
			enddo !do j=2,j_nsetd(i)
			idomba=idomba+jlp_nsetd(i)
		enddo !do i=1,nset

		if(p2) write(n16,*)'irowdomain',jlp_irowdomain

	!pakotetut 0-alarajat xkf-mja rajoitteille
		if(jlp_fpresent.and..false.) then
			do irowj_=1,jlp_nrow
				irowrowj_=jlp_irowrow(irowj_)
				if (jlp_nfxrow(irowrowj_)==1) then
					if(j_o(ivrhs)%d(irowrowj_)==-huge(1.d0)) then
						jlp_rhs(irowj_)= 0.d0
						jlp_lbou(irowj_)=.true.
						write(6,*)'constraint ', irowj_,' lower bound set to zero'
						if(irowj_+1.ne.irowrowj_) &
						write(6,*) 'corresponds to row ', irowrowj_, ' in original problem definition'
					endif !if(j_o(ivrhs)%d(irowrowj_)==-huge(1.d0)) then
				endif !if (j_nfxrow(irowrowj_)==1) then
			enddo !do irowj_=1,j_nrow
		endif !if(j_fpresent.and..false.) then

	!generate domains
		itran=0
		if(jlp_ndom.gt.0)then  ! ndom is the number of domains
		! ivdomain is the text object containg domain definitions
		! ivdomains is the list of outputvaribales of the domain transformations
		! Note: ivdomains is not good name because it looks like the index of a J object
		!
			do i=1,jlp_ndom
				call j_getline(jlp_ivdomain,i,jlp_domdef,le)
				idom=j_object(jlp_domdef(1:le))   !
			! a domain definition can consist of just a variable name
			! nonzero values of the variable indicate that the domain is in effect
				if(idom.gt.0)then
					jlp_ivdomains(i)=idom
				else !if(idom.gt.0)then
					jlp_domname='$Domain?='
					led=9
					call j_repse(jlp_domname,8,8,led,i) ! replace '?' by the domain number
					call j_getobject(0,jlp_domname(1:led-1),j_ipreal,ivdom)  ! make the object with the domainname
				!virheenkäsittely
					if(j_err) return
					jlp_domdef=jlp_domname(1:led)//jlp_domdef
					if(itran.le.0)then
					! make transformation object for domain transformations !!!!
						call j_deftrans(0,'$DomainTrans$',ivdomtrans,30*jlp_ndom,0,0,iii,iii,iii,0)

						itran=1
					endif !if(itran.le.0)then
				! interpret the domain defintion
				!!N	call j_compiler(jlp_domdef(1:led+le),ivdomtrans,.false.,0,0,&
				!		j_matrix0,j_matrix0,j_matrix0,.false.)
					if(j_err)then
						write(6,*)'*problem: illegal domain definition '
						return
					endif !if(j_err)then

					jlp_ivdomains(i)=ivdom
				endif !if(idom.gt.0)then
			enddo !do i=1,j_ndom
		endif !if(j_ndom.gt.0)then
	! generate domains, xvars
		if(jlp_ndom.gt.0)then
			ndomv=ceiling(real(jlp_ndom)/32.)
		! store domain information in bit form into domainbits
		! one element in domainbits can store 32 bits

			if(allocated(jlp_domainbits))deallocate(jlp_domainbits)
			allocate(jlp_domainbits(1:ndomv,1:jlp_nunits));jlp_domainbits=0

		! domainunits tells the number of units in each domain
			if(allocated(jlp_domainunits))deallocate(jlp_domainunits)
			allocate(jlp_domainunits(1:jlp_ndom));jlp_domainunits=0
			jlp_domainunits=0
			jlp_domainbits=0
		endif !if(j_ndom.gt.0)then

		jlp_ntemp0=itemp

	!j_xmatlast2=0  ! there is nothing in upper buffer


		if(ivunit.le.0)maxnsch=j_o(ivcdata)%i(9)


	! else !if(memory.ne.0)then
		! j_lopp=j_lopp
	! endif

		write(6,*)'allocate xmat,',jlp_lopp,jlp_ntemp0,jlp_ntemp0*jlp_lopp/1.e6,' million'
		if(allocated(jlp_xmat))deallocate(jlp_xmat)
		
		allocate(jlp_xmat(1:jlp_ntemp0*jlp_lopp))

	  ! write(6,*)'data used from disk would need additional ',&
		! ipe, ' million'
	! endif

	! if(j_xmatinmemory)write(6,*)'memory used by xdata ',float(j_lopp*keepx)/1.e6, 'millions'

	! !write(6,*)'<477>j-xmatibas2',j_xmatibas2
!	j_xdatfromdisk=idofa.ne.0.and..not.j_xdatinmemory

	!write(6,*)'<741>xdatfromdisk ',j_xdatfromdisk,ido,j_xdatinmemory
		if(fast)then
			if(allocated(fastreject))deallocate(fastreject)
			allocate(fastreject(1:jlp_lopp))
			fastreject=.false.
		endif !if(fast)then

	! do i=j_lopp-15000,j_lopp
	! iba=jxmatiba(i,1)
	! write(17,*)j_o(ivxmat)%r(iba+1:iba+7),j_o(ivxmat)%r(iba+11:iba+17)

	! enddo


		if(allocated(jlp_vxpack))deallocate(jlp_vxpack)
		if(allocated(jlp_ixpack))deallocate(jlp_ixpack)

		allocate(jlp_vxpack(1:jlp_ntemp0),jlp_ixpack(1:jlp_ntemp0))
		jlp_vxpack=0;jlp_ixpack=0


		if(jlp_ntemp0.gt.j_maxjlpxvars)then
			write(6,*)'*j* maximum number of xvariables in jlp problem is ',j_maxjlpxvars
			write(6,*)'*j* your problem needs ',jlp_ntemp0
			write(6,*)'ask j-team to increase maxjlpxvars in j_modules.f90'
			j_err=.true.
			return

		endif !if(j_ntemp0.gt.j_maxjlpxvars)then
	!	write(6,*)'<87777'
	!tree
		if(istree)then
			if(allocated(iperiod))deallocate(iperiod)
			allocate(rperiod(1:jlp_ntemp0))
			if(allocated(iperiodperm))deallocate(iperiodperm)
			allocate(iperiodperm(1:jlp_ntemp0))
			valuesums(0)=0.d0

			if(allocated(idiff3))deallocate(idiff3)
			allocate(idiff3(1:jlp_ntemp0))
			ipri_=0 ;ipri2_=0

			nx_=0
			do i_=0,jlp_nrow
				if(jlp_ix(i_).ne.0)then
					nx_=nx_+1
					if(i_.eq.0)then
						rperiod(1)=nperiods
					else !if(i_.eq.0)then
						rperiod(nx_)=j_o(j_ivperiod)%d(i_+1)  !the numbering of rows start from 1 in problem

					endif !if(i_.eq.0)then
				endif !if(j_ix(i_).ne.0)then
			enddo !do i_=0,j_nrow
			call j_quick_sort(rperiod,iperiodperm)

			if(allocated(idiff))deallocate(idiff)
			allocate(idiff(1:jlp_lopp))
			idiff=jlp_ntemp0
			if(allocated(previous))deallocate(previous)
			allocate(previous(1:jlp_ntemp0))
			itreesize=0
		endif !if(istree)then
	! number of schedules of each unit
		if(allocated(jlp_nsch))deallocate(jlp_nsch)
		allocate(jlp_nsch(1:jlp_nunits))
		if(allocated(jlp_rejects))deallocate(jlp_rejects)
	! subreject-> option was given
		if(subfilre)then;allocate(jlp_rejects(1:jlp_lopp));jlp_rejects=.false.;endif

		if(allocated(jlp_nunitsrow))deallocate(jlp_nunitsrow)
		allocate(jlp_nunitsrow(0:jlp_nrow));jlp_nunitsrow=0

		if(allocated(jlp_keys))then
			if(warm)then
				if(size(jlp_keys).ne.jlp_nunits)then
					write(6,*)'*jlp: number of units is not same as in previous problem',&
						' warm-> ignored'
					warm=.false.
					warmf=.false.

				endif !if(size(j_keys).ne.jlp_nunits)then
			endif !if(warm)then
			if(.not.warm)deallocate(jlp_keys)
		endif !if(allocated(j_keys))then
		if(.not.warm)then
			allocate(jlp_keys(1:jlp_nunits));jlp_keys=0
		endif !if(.not.warm)then

		if(allocated(jlp_ibaunit))deallocate(jlp_ibaunit)
		allocate(jlp_ibaunit(1:jlp_nunits+1))

		if(allocated(jlp_xps))deallocate(jlp_xps)  ! sums of x-variables  over key schedules
		if(allocated(jlp_xsmin))deallocate(jlp_xsmin) ! smallest possible sum
		if(allocated(jlp_xsmax))deallocate(jlp_xsmax)   !largest possible sum
		if(allocated(jlp_xmin))deallocate(jlp_xmin)
		if(allocated(jlp_xmax))deallocate(jlp_xmax)

		allocate(jlp_xps(0:jlp_nrow),jlp_xsmin(0:jlp_nrow),jlp_xsmax(0:jlp_nrow))
		allocate(jlp_xmin(0:jlp_nrow),jlp_xmax(0:jlp_nrow))
	!xvarl list of all x variables
		if(allocated(jlp_xvarl))deallocate(jlp_xvarl);allocate(jlp_xvarl(1:nxvar))
		jlp_xvarl=pack(j_o(ivvars)%i2(1:nvartot),jlp_isx)   ! c-variables are here included as x -variables
		if(allocated(jlp_xvarlarea))deallocate(jlp_xvarlarea)
		if(allocated(jlp_cvar))deallocate(jlp_cvar)
	! number of x and c variables which are expressed /area
		nxvararea=0
		ncvararea=0
		needc=0
	!			write(6,*)'<8797'
		if(ivarea.gt.0)then   ! area-> option present

			if(notareavars.gt.0)then
				nxvararea=j_ndiffer(jlp_xvarl,nxvar, &
				j_o(iob)%i(linknotareavars+1:linknotareavars+notareavars),notareavars)
				if(nxvararea.gt.0)then
					allocate(jlp_xvarlarea(1:nxvararea))
				! x area variables = variables - notareavariables
					call j_differ(jlp_xvarl,nxvar, &
					j_o(iob)%i(linknotareavars+1:linknotareavars+notareavars),notareavars,jlp_xvarlarea,nxvararea)

				else !if(nxvararea.gt.0)then

					write(6,*)'*jlp: no variable remains area-variable'

				endif !if(nxvararea.gt.0)then

			else !if(notareavars.gt.0)then
			! all variables are areavariables
				nxvararea=nxvar
				allocate(jlp_xvarlarea(1:nxvar))
				jlp_xvarlarea(1:nxvar)=jlp_xvarl

			endif !if(notareavars.gt.0)then
			if(ncvar.gt.0)then
				allocate(jlp_cvar(1:ncvar))
				needc=1 !make logical
			endif !if(ncvar.gt.0)then

		endif !if(ivarea.gt.0)then
		nstot=0  !total number of schedules
	!ibasclass=0  given earlier
		isc=0  ! have we starter the given class if class option is given
		jlp_ibaunitbas=0  ! basis for schedules, remains 0 if option class-> not given
		nrejtot=0  !number of rejected schdules
		iwar=0   ! number of units where all schedules are rejected
	!!!!!**************setting up xdata
	!write(6,*)'<147 hui',size(j_xmat),allocated(j_xmat)
	if(isunit)then
		inde=j_inlistobject(ivunit,jlp_ivkeepx)
		
		jlp_nunits=1
		old=j_o(ivxmat)%d(inde)
		ibas=keepx
		iprev=1
		maxnsch=1
	!	write(6,*)'indet',inde,old
		do i=2,j_o(ivxmat)%i(1)
	!	if(i.lt.200)write(6,*)i,j_o(ivxmat)%d(ibas+inde),old,ibas
			if(j_o(ivxmat)%d(ibas+inde).ne.old)then
					jlp_nsch(jlp_nunits)=i-iprev
					maxnsch=max(maxnch,jlp_nsch(jlp_nunits))
					jlp_nunits=jlp_nunits+1
					old=j_o(ivxmat)%d(ibas+inde)
				
					iprev=i
			endif
				ibas=ibas+keepx		
		enddo
		jlp_nsch(jlp_nunits)=j_o(ivxmat)%i(1)-iprev
		maxnsch=max(maxnch,jlp_nsch(jlp_nunits))
!	write(6,*)'shed',jlp_nsch(1:jlp_nunits)
	
	endif
		ibasc=0
	!	write(6,*)'jlp_nunits',jlp_nunits,jlp_ntemp0,jlp_ibatemp
		
		
				ibas=0
		do iik=1,keepx
	!write(6,*)'477474',iik,j_o(ivxmat)%d(iik),j_o(ivxmat)%d(keepx+iik)
		enddo		
		do iunit=1,jlp_nunits
	
			if(ivunit.gt.0)then
			
				j_v(j_o(jlp_ivkeepx)%i2(1:keepx))=j_o(ivxmat)%d(ibas+1:ibas+keepx)
				ibas=ibas+keepx
			else
			!	write(6,*)'sh',jlp_ivkeepc,keepc,ivcmat
				j_v(j_o(jlp_ivkeepc)%i2(1:keepc))=j_o(ivcmat)%d(ibasc+1:ibasc+keepc)
				ibasc=ibasc+keepc
	!		call j_getobsiv(iunit,jlp_ivmatc,jlp_ivkeepc,jlp_ivunit)

			endif

			if(jlp_ivtrans.gt.0)then
				call dotrans(jlp_ivtrans,1)  ! trans option given
				if(j_err)then
					write(6,*)'error for unit ',iunit
					stop 815
				endif !if(j_err)then

			endif !if(jlp_ivtrans.gt.0)then
			if(itran.gt.0)then
				call dotrans(ivdomtrans,1)  !there are domain transformations
				if(j_err)then
					write(6,*)'error in domain transformations for unit ',iunit
					stop 879
				endif !if(j_err)then
			endif !if(itran.gt.0)then
			if(needc.gt.0)jlp_cvar=j_v(jlp_cvarl(1:ncvar))
	
			if(jlp_ndom.ne.0)then  ! ndom is number of domains
			!domainbits(1:ndomv,iunit)=0 done at allocation
				do j=1,jlp_ndom
					ii=(j-1)/32+1
					ibit=j-(ii-1)*32-1  ! bit numbering starts from zero
					if(j_v(jlp_ivdomains(j)).ne.0. )then
						jlp_domainbits(ii,iunit)= ibset(jlp_domainbits(ii,iunit),ibit)
					! domainunits tells the number of units in each domain
						jlp_domainunits(j)=jlp_domainunits(j)+1
					endif !if(j_v(j_ivdomains(j)).ne.0. )then
				enddo !do j=1,j_ndom
			endif !if(j_ndom.ne.0)then
			if(.not.isunit)jlp_nsch(iunit)=j_v(j_ivns)   ! number of schedules
			nrej=0   ! number of rejected schedules in this unit
			do is=1,jlp_nsch(iunit)
			!			write(6,*)'<6565'
				nstot=nstot+1
				       ! ivs index of schedule variable
			! get observation from x-data
				if(.not.isunit.or.is.gt.1)then
			!		write(6,*)jlp_ivkeepx,j_o(jlp_ivkeepx)%i2,keepx,ivxmat,ibas
				j_v(j_o(jlp_ivkeepx)%i2(1:keepx))=j_o(ivxmat)%d(ibas+1:ibas+keepx)
				ibas=ibas+keepx
				
				endif
	!			call j_getobsiv(jlp_ibaunitbas+nstot,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0) !making			xmat

				
				if(jlp_ivsubtrans.gt.0)then
				j_v(ivs)=is  
					call dotrans(jlp_ivsubtrans,1)  ! subtrans-> was given
					if(j_err)then
						write(6,*)'error for schedule ',nstot
						stop 447
					endif !if(j_err)then
				endif !if(jlp_ivsubtrans.gt.0)then
				if(subfilter_)then         !subfilter->
					testcode=j_codevalue(iob,j_subfilterlink)
					!call dotrans(iob,j_iosubfilter)
					if(j_err)then
						write(6,*)'error for observation ',iob
						stop 337

					endif !if(j_err)then
					if(testcode.eq.0.)then   ! filter->False, reject
						jlp_rejects(nstot)=.true.
						nrej=nrej+1
						nrejtot=nrejtot+1  !total number of rejected schedules
						goto 776
					endif !if(j_v(j_ivsubfilter).eq.0.)then
				endif !if(subfilter_)then
				if(subreject_)then            ! reject->
					testcode=j_codevalue(iob,j_subrejectlink) !call dotrans(iob,j_iosubreject)
					if(j_err)then
						write(6,*)'error for obs ',iob
					endif !if(j_err)then
					if(testcode.ne.0.)then    !reject->True
						jlp_rejects(nstot)=.true.
						nrej=nrej+1
						nrejtot=nrejtot+1
					endif !if(j_v(j_ivsubreject).ne.0.)then
				endif !if(subreject_)then
			776     continue
			!		write(6,*)'<8767'
		!	if(j_xmatinmemory)then
				ibax=(nstot-1)*jlp_ntemp0
	
				if(nxvararea.gt.0)then
					! xvarlarea is a vector
					j_v(jlp_xvarlarea)=j_v(ivarea)*j_v(jlp_xvarlarea)   ! multiply area-variables by area
				endif !if(nxvararea.gt.0)then
				do i=1,jlp_ntemp0
					iba=jlp_ibatemp(i)
				!	if(iunit.lt.10)write(6,*)iba,jlp_ntemp0,iba,ibax,jlp_nxrowtemp(i),iba+jlp_nxrowtemp(i)

					jlp_xmat(i+ibax)= &       ! make temporary variables
						dot_product(jlp_coefx(iba+1:iba+jlp_nxrowtemp(i)), &
						j_v(jlp_irowxvars(iba+1:iba+jlp_nxrowtemp(i))))
						if(iunit.le.-1.and.i.eq.1)then
							write(6,*)'j_nxrowtemp(i)',jlp_nxrowtemp(i)
						do jj=1,jlp_nxrowtemp(i)
						call j_getname(jlp_irowxvars(iba+jj))
						write(6,*)jj,jlp_coefx(iba+jj),jlp_irowxvars(iba+jj),j_oname(1:j_loname),j_v(jlp_irowxvars(iba+jj))
						enddo
						endif
					!	if(p2.and.nstot.eq.1)write(6,*)'itemp',i,' iba=',iba,' coef=',j_coefx(iba+1:iba+j_nxrowtemp(i)),&
						!		' vars',j_irowxvars(iba+1:iba+j_nxrowtemp(i))
				enddo !do i=1,j_ntemp0
!if(iunit.gt.1)stop 'per'
				!j_ix(0),j_ntemp0
				if(.not.jlp_maxo.and.jlp_ix(0).ne.0) &
				jlp_xmat(jlp_ix(0)+ibax)=-jlp_xmat(jlp_ix(0)+ibax)
		

				if(istree)then
					if(is.eq.1)then
						do i=1,jlp_ntemp0
							previous(i)=jlp_xmat(ibax+iperiodperm(i))!previous(i)=j_xmat(iperiodperm(i),nstot)
						enddo !do i=1,j_ntemp0
						idiff(nstot)=1
						itreesize=itreesize+jlp_ntemp0
					else !if(is.eq.1)then
						do i=1,jlp_ntemp0
						!if(j_xmat(iperiodperm(i),nstot).ne.previous(i))then
							if(jlp_xmat(iperiodperm(i)+ibax).ne.previous(i))then
								idiff(nstot)=i
								do i2_=i,jlp_ntemp0
									previous(i2_)=jlp_xmat(iperiodperm(i2_)+ibax)
								!	previous(i2_)=j_xmat(iperiodperm(i2_),nstot)
								enddo !do i2_=i,j_ntemp0
								itreesize=itreesize+jlp_ntemp0-i+1
								exit
							endif !if(j_xmat(iperiodperm(i)+ibax).ne.previous(i))then
						enddo !do i=1,j_ntemp0

					endif !if(is.eq.1)then

				endif !if(istree)then
				if(needc.gt.0)j_v(jlp_cvarl(1:ncvar))=jlp_cvar
			enddo !do is=1,j_nsch(iunit2)
			if(subfilre)then
				if(nrej.ge.jlp_nsch(iunit))then
					iwar=iwar+1
					if(iwar.le.10)write(6,*)'*err all schedules were rejected for unit ',iunit
				endif !if(nrej.ge.j_nsch(iunit))then
			endif !if(subfilre)then
			if(j_err)return
		!		write(6,*)'nuni ',jlp_nunits,iunit,ibax
		enddo !do iunit=1,jlp_nunits
	!	write(6,*)'nunits ',jlp_nunits,jlp_ntemp0
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	 iba=0
	 do i=1,20
	 write(6,*)i,jlp_xmat(iba+1:iba+jlp_ntemp0)
	 iba=iba+jlp_ntemp0
	 enddo
	
	
	
	!	write(6,*)'shed2',jlp_nsch(1:jlp_nunits)
		if(fast)then
		!	maxnsch2=maxval(j_nsch)
		!	write(6,*)'maxns',maxnsch,maxnsch2
			if(allocated(fastvalues))deallocate(fastvalues)
!		write(6,*)'<44>',maxnsch
			allocate(fastvalues(1:maxnsch))
		!	if(allocated(fastclass))deallocate(fastclass)
		!	allocate(fastclass(1:201))

		endif !if(fast)then



		if(iwar.gt.0)then
			write(6,*)'**all schedules rejected for ',iwar, 'units'
			j_err=.true.
			return
		endif !if(iwar.gt.0)then



		if(allocated(jlp_solx))deallocate(jlp_solx)
		if(nz.le.0)allocate(jlp_solx(0:jlp_nrow))
	!nstot adds up the 'ns' variables, lopp is the number of observations in xdata
		if(ivunit.le.0.and.nstot.ne.jlp_lopp)then
			write(6,*)'**number of sched in cdat, and xdat do not match',nstot,jlp_lopp
			j_err=.true.
			return
		endif !if(nstot.ne.j_lopp.and.ivclass.eq.0)then

		if(jlp_xpresent)then
			write(*,*)'found ',jlp_nunits,' units, ',jlp_lopp,' schedules, filling xmat'
			call cpu_time(time00)
			if(subfilre)write(6,*)'from which ',nrejtot,' schedules were rejected'
		endif !if(j_xpresent.and.ivclass.eq.0)then
	! empty domains?
		nempt=0
		do i=1,jlp_ndom
			if(jlp_domainunits(i).le.0)then    !empty domain
				if(nempt.le.0)write(6,*)'*empty domains:'
				if(nempt.le.10)then
					call j_printtext(jlp_ivdomain,i)
				elseif(nempt.eq.11)then !if(nempt.le.10)then
					write(6,*)'...'
				endif !if(nempt.le.10)then
				nempt=nempt+1
			endif !if(j_domainunits(i).le.0)then
		enddo !do i=1,j_ndom
		if(nempt.gt.0)then 
			j_err=.true.;write(6,*)'**empty domains:',nempt; return 
		end if
!if(j_o(ivxmat)%r(10261*keepx).eq.0.)stop 11
	!tehdas xk-mjien ei-negatiivisuus-tarkistus !!!!
		if(jlp_fpresent) then
			ibaobs=0
		!	write(6,*)'<6662 ',ivxmat,j_o(ivxmat)%i(1),j_o(jlp_ivxkyk)%i(1),jlp_ivkeepx
			do iobs=1,j_o(ivxmat)%i(1)
	!			ibaobs=jxmatiba(iobs)  !,1)
	
				! if(iobs.le.2)then
					! do ixk = 1,j_o(jlp_ivxkyk)%i(1) !tehdas xk-mjat
						! ivxk = j_o(jlp_ivxkyk)%i2(ixk)
						! kk = j_inlistobject(ivxk,jlp_ivkeepx)
						k_ = j_inlistobject(ivxk_,jlp_ivkeepx)
					! call j_getname(ivxk)
					! write(6,*)iobs,ivxk,kk,j_oname(1:j_loname),j_o(ivxmat)%d(ibaobs+k_)
				! enddo
				
				!endif

				do ixk_ = 1,j_o(jlp_ivxkyk)%i(1) !tehdas xk-mjat
					ivxk_ = j_o(jlp_ivxkyk)%i2(ixk_)
					k_ = j_inlistobject(ivxk_,jlp_ivkeepx)
				!	iba_=0
				!		do while(iba_ < (j_o(ivxmat)%i(1)*j_o(ivxmat)%i(2)))
					if (j_o(ivxmat)%d(ibaobs+k_) < 0) then   !jxmatiba
					! datassa negat. tehdas-xk-mja
						call j_getname(ivxk_)
						write(6,*)'**Negative fact xk-variable ',j_oname(1:j_loname), ' in observation ',iobs ,j_o(ivxmat)%d(ibaobs+k_)
						!in data: ixkyk,ivxk,iba,k, xk',&
						!	ixk_,ivxk_,ibaobs,k_,j_o(ivxmat)%d(ibaobs+k_) !jxmatiba
						j_err=.true.
						return
					endif !if (j_o(ivxmat)%r(ibaobs+k_) < 0) then
				!		iba_=iba_+j_o(ivxmat)%i(2)
				!		enddo !do while(iba_ < (j_o(ivxmat)%i(1)*j_o(ivxmat)%i(2)))
				enddo !do ixk_ = 1,j_o(j_ivxkyk)%i(1)
				ibaobs=ibaobs+keepx  !jlp_ntemp0
			enddo !do iobs=1,j_o(ivxmat)%i(1)
		!endif  !xdisk
		endif !if(j_fpresent) then


	! calculate sums over key schedules !!!!
!if(j_o(ivxmat)%r(10261*keepx).eq.0.)stop 179
		jlp_xps=0.d0
	! initial key schedules, rotate number
		key=0
		jlp_ibaunit(1)=0 ! ibaunit, basis for schedules for each unit
		jlp_xsmin=jlp_zero     !minimum for the sum
		jlp_xsmax=jlp_zero     !maximum for the sum
		rejectnow_=.false.
		do i=1,jlp_nunits  !***********************
			nrej=0
			call jlpcurix(i) !determines for each row if the unit iunit belonggs to the domain of the row
		!returns nrowp=number of rows in this domain,
		! ixcurrows= for each row in this domain tells the original (expanded) row
		!  ixcur for each original expanded row tells the index of temporary x-variable
		! facory tehtävissä tämä on kait sama
			jlp_ibaunit(i+1)=jlp_ibaunit(i)+jlp_nsch(i)
			if(subfilre)then
				if(jlp_rejects(jlp_ibaunit(i)+1))then
				! if first schedule is rejected, then jlp_xmin=large and jlp_xmax=-large
				! if first schedules is not rejcted these are obtained from the first schedule
					nrej=nrej+1
				! earlier: allocate(nunitsrow(0:nrow));nunitsrow=0
					do jj=1,jlp_nrowp ;j=jlp_ixcurrows(jj)
					jlp_nunitsrow(j)=jlp_nunitsrow(j)+1
						jlp_xmin(j)=1.7e37;jlp_xmax(j)=-1.7e37
					enddo !do jj=1,j_nrowp ;j=j_ixcurrows(jj); j_nunitsrow(j)=j_nunitsrow(j)+1
					goto 745
				endif !if(j_rejects(j_ibaunit(i)+1))then
			endif !if(subfilre)then

! get jlp_xmin and jlp_xmax from the first schedule
			ibax=jxmatiba(jlp_ibaunit(i)+1) !,1)
			do jj=1,jlp_nrowp
				j=jlp_ixcurrows(jj)
				jlp_nunitsrow(j)=jlp_nunitsrow(j)+1
				jlp_xmin(j)=jlp_xmat(jlp_ix(j)+ibax) ! v(ix(j))
				jlp_xmax(j)=jlp_xmat(jlp_ix(j)+ibax)  ! v(ix(j))
			enddo !do jj=1,j_nrowp

745   keyopt=1     !works also if first schedule is rejected and we come from  'goto 745'
			do k=2,jlp_nsch(i)
				if(subfilre)then
					if(jlp_rejects(jlp_ibaunit(i)+k))then
						nrej=nrej+1
						cycle  !do k=2,nsch(i)
					endif !if(j_rejects(j_ibaunit(i)+k))then
				endif !if(subfilre)then
!if(j_o(ivxmat)%r(10261*keepx).eq.0.)stop 149
! compute for rows containing x-variables the min and max values
				ibax=jxmatiba(jlp_ibaunit(i)+k) !,1)
				do jj=1,jlp_nrowp
					j=jlp_ixcurrows(jj)
					curx=jlp_xmat(jlp_ix(j)+ibax)
					if(curx.lt.jlp_xmin(j))then
						jlp_xmin(j)=curx
					endif !if(curx.lt.jlp_xmin(j))then
					if(curx.gt.jlp_xmax(j))then
						jlp_xmax(j)=curx
						keyopt=k   ! for the case where there are no constraints
					endif !if(curx.gt.jlp_xmax(j))then
				enddo !do jj=1,j_nrowp

			enddo !do k=2,j_nsch(i)

			do jj=1,jlp_nrowp
				j=jlp_ixcurrows(jj)
				jlp_xsmin(j)=jlp_xsmin(j)+jlp_xmin(j)
				jlp_xsmax(j)=jlp_xsmax(j)+jlp_xmax(j)
			enddo !do jj=1,j_nrowp

			if(jlp_nrow.eq.0)then
				key=keyopt   ! no constraints, can be maximized directly
			else !if(j_nrow.eq.0)then
4689    key=key+1
				if(key.gt.jlp_nsch(i)) key=1
				if(subfilre)then
					if(jlp_rejects(jlp_ibaunit(i)+key))goto 4689
				endif !if(subfilre)then

			endif !if(j_nrow.eq.0)then

			if(warm.and.jlp_nrow.gt.0)then

				if(subfilre)rejectnow_=jlp_rejects(jlp_ibaunit(i)+jlp_keys(i))
				if(jlp_keys(i).gt.jlp_nsch(i).or.rejectnow_)then
					warm=.false.
					warmf=.false.
					write(6,*)'*data does not agree, warm-> ignored'
				else !if(j_keys(i).gt.j_nsch(i).or.rejectnow_)then
					key=jlp_keys(i)
				endif !if(j_keys(i).gt.j_nsch(i).or.rejectnow_)then
			endif !if(warm.and.j_nrow.gt.0)then
			jlp_keys(i)=key
			ibax=jxmatiba(jlp_ibaunit(i)+key) !,1)
			do jj=1,jlp_nrowp
				j=jlp_ixcurrows(jj)
!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
				jlp_xps(j)=jlp_xps(j)+jlp_xmat(jlp_ix(j)+ibax) !v(ix(j)) !(ix(j)=0 -> no x in row
			!	if(i.le.2.and.j.eq.0)write(6,*)'<555 ',i,ibax,jlp_ix(j),jlp_xmat(jlp_ix(j)+ibax),jlp_xps(j)
			enddo !do jj=1,j_nrowp

!tehdasmuuttujien xps-laskenta
			if(i.eq.jlp_nunits)write(6,*)'<474774>',jlp_fpresent
			if(jlp_fpresent) then
!fdomain fcurix ja fcurixy täytyy päivittää
				call jlpfcurix(i)
				call jlpfcurixy(i)
!if(j_o(ivxmat)%r(10261*keepx).eq.0.)stop 33
!write(16,*)j_xdattokaobs,j_ibaunit(i)+key,'j_xdatlast',j_xdatlast

				ibakey=jxmatiba(jlp_ibaunit(i)+key) !,2)

if(i.le.2)write(16,*)'ibakey i',ibakey,i

!if(j_o(ivxmat)%r(10261*keepx).eq.0.)then
!write(16,*)ibakey,ibakey/keepx,j_ibaunit(i)+key
!write(16,*)'j_xdatlast',j_xdatlast,j_xdatfirst2,j_xdatlast2
!stop 34
!endif
!	write(19,*)ivxmat,j_ibaunit(i),key,ibakey,j_lopp
!nunitsrow tehdasmja-riveille
!?? subfilre,rejects vrt. x-mja tehtävärivit
				do j=0, jlp_nrow
					if (jlp_ix(j)==0) then
						if (jlp_ixcurfact(j)) then !ehdosta puuttuu rivit, joilla y-tehdasmjia
							jlp_nunitsrow(j) = jlp_nunitsrow(j) + 1
						endif !if (j_ixcurfact(j)) then
					endif !if (j_ix(j)==0) then
				enddo !do j=0, j_nrow

				if(i.eq.jlp_nunits) then
					write(n16,*)'jlp_nrowpfy',jlp_nrowpfy
					write(n16,*)'jlp_nrowp',jlp_nrowp
					write(n16,*)'nfxrow',jlp_nfxrow
					write(n16,*)'irowfxvars: ',jlp_irowfxvars
					write(n16,*)'irowffact: ',jlp_irowffact
					write(n16,*)'irowfkeep: ',jlp_irowfkeep
					write(n16,*)'ibafx: ',jlp_ibafx
					write(n16,*)'ibaunit: ',jlp_ibaunit(1:min(100,jlp_nunits))
					write(n16,*)'key: ',key
					write(n16,*)'o(ivkeepx)%i(1): ',j_o(jlp_ivkeepx)%i(1)
					write(n16,*)'irowrow',jlp_irowrow
					write(n16,*)'ifxcurrows',jlp_ifxcurrows
					write(n16,*)'nfyrow',jlp_nfyrow
					write(n16,*)'ifycurrows',jlp_ifycurrows(1:jlp_nrowpfy)
				endif !if(p.and.i.eq.jlp_nunits) then
!if(j_o(ivxmat)%r(10261*keepx).eq.0.)stop 49
!xk-tehdasmuuttujat

				do jj=1,jlp_nrowpf
					j=jlp_ifxcurrows(jj) !domainissa olevat rivit
					irowj = jlp_irowrow(j)

	!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
					do k=1,jlp_nfxrow(irowj)
		! onko kerrointa vastaava tehdas sama kuin laskentayksikön avaintehdas kerrointa vastaavalla muuttujalla
		! irowfxvars, irowffact : tehdasmuuttujat & tehtaat esiintymisjärjestyksessä
		!if(p) then
		!endif !if(p) then
						if(jlp_keyfact(i,jlp_irowfxvars(jlp_ibafx(irowj)+k)).eq. &
						jlp_irowffact(jlp_ibafx(irowj)+k))then
						

	
							jlp_xps(j)=jlp_xps(j)+ jlp_coeffx(jlp_ibafx(irowj)+k)*&
								j_o(ivxmat)%d(ibakey+jlp_irowfkeep(jlp_ibafx(irowj)+k)) !jxmatiba
								
!if(i.eq.1.and.j.eq.0)write(6,*)'<##',nfy,irowj,jlp_ibafx(irowj),k,jlp_ibafx(irowj)+k,jlp_coeffx(jlp_ibafx(irowj)+k),&
! j_o(ivxmat)%d(ibakey+jlp_irowfkeep(jlp_ibafx(irowj)+k)),jlp_xps(j)
						endif !if(j_keyfact(i,j_irowfxvars(j_ibafx(irowj)+k)).eq.j_irowffact(j_ibafx(irowj)+k))then
					enddo !do k=1,j_nfxrow(irowj)
				enddo !do jj=1,j_nrowpf

				if (nfy.gt.0) then
					do j=1,j_o(jlp_ivkeepc)%i(1)
						j_v(j_o(jlp_ivkeepc)%i2(j))=j_o(ivcmat)%d((i-1)*j_o(jlp_ivkeepc)%i(1)+j)
					enddo !do j=1,j_o(j_ivkeepc)%i(1)
					do j=1,ntrans
						call dotrans(jlp_itransv(j),1)
						if(j_err)then
							write(6,*)'error for unit ',i
							stop 771
						endif !if(j_err)then
					enddo !do j=1,ntrans
				endif !if (nfy.gt.0) then
!if(j_o(ivxmat)%r(10261*keepx).eq.0.)stop 314
				do jj=1,jlp_nrowpfy
	!ivn-laskurin käsittely ei toimi, jos mukana on domaineja
					j=jlp_ifycurrows(jj) !domainissa olevat rivit
					irowj = jlp_irowrow(j)
	!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
					do k=1,jlp_nfyrow(irowj)
		! irowfyvars, irowfyfact : tehdasmuuttujat & tehtaat esiintymisjärjestyksessä
		! coeffx:n tilalle muunnosten output-muuttujat => haaetaan %%puuljit%%tehtaat -listoista siäkkäisillä silmukoilla
		! koko alla oleva if* silmukoiden sisälle
		! irowfxvars-> irowfyvars (huom! lista), vast irowffact->irowfyfact (huom! lista) , ibafx->ibafy
						listy=jlp_irowfyvars(jlp_ibafy(irowj)+k)
						listf=jlp_irowfyfact(jlp_ibafy(irowj)+k)
						call j_getname(listy)
if(i.le.2)write(6,*)'<88899 ',k,j_o(listy)%i(1),j_oname(1:j_loname)
						do ivars_=1,j_o(listy)%i(1)
				!		write(6,*)'<333 ',ivars_
			!mjan xmat-sarake
							iv2elpos = jlp_ifyvarskeep(jlp_ibafykeep(jlp_ibafy(irowj)+k)+ivars_-1)
			!listy-listan ivars_:innen ptl-mjan paikka (järjestysnumero) xkyk-listassa
							iv2xykypos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj)+k)+ivars_-1)
	call j_getname(listf)
!	if(i.le.2)write(6,*)'<688 ',j_oname(1:j_loname),' ',j_o(listf)%i(1)
							do ifact_=1,j_o(listf)%i(1)
				!onko tehdas (ifact_) puutavaralaji-muuttujan (ivars_) avaintehdas
				!tehdas-y-mjaan liittyvänn muunnoksen outputmjien eli gamma-kertoimien poiminta v-vektorista
				!indeksi, josta ibafyfact-vektorissa alkaa irowj:nnen tehtävärivin tehdaslista-esiintymät
				!indeksi, josta ifyfactfact-vektorissa alkaa irowj:nnen tehtävärivin k:nnen tehdaslista-esiintymän
				!(ibafy(irowj)+k) tehtaiden järjestysnumerot factories-listassa
				! iv3factpos_ : tehdaslistan list ifact_:innen tehtaan paikka (järjestysnumero) factories-listassa
								iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj)+k)+ifact_-1)
		!						if(i.le.2)write(6,*)'<22 ',i,' iv3factpos_ ',iv3factpos_,jlp_keyfact(i,iv2xykypos_)
		!						if(i.le.2)write(6,*)'<23 ',i,' ; ',jlp_keyfact(i,1:j_o(jlp_ivxkyk)%i(1))
								
								if(jlp_keyfact(i,iv2xykypos_).eq.iv3factpos_) then
					!if(p)then
					!endif !if(p)then
									jlp_xps(j)=jlp_xps(j)+ j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_))*&
										j_o(ivxmat)%d(ibakey+iv2elpos)
										
										! if(j.eq.0.and.i.le.2)then
										! call j_getname(j_o(jlp_ivkeepx)%i2(iv2elpos),jlp_fyfactout(iv2xykypos_,iv3factpos_))
										
										! write(6,*)'##& ',i, ibakey,j_oname(1:j_loname),'  ',j_oname2(1:j_loname2),&
										! iv2elpos,j_o(ivxmat)%d(ibakey+iv2elpos),&
										! j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_)),jlp_xps(j)
										! endif

								endif !if(j_keyfact(i,iv2xykypos_).eq.iv3factpos_) then

							enddo !do ifact_=1,j_o(listf)%i(1)
						enddo !do ivars_=1,j_o(listy)%i(1)

					enddo !do k=1,j_nfyrow(irowj)
				enddo !do jj=1,j_nrowpfy

				if(p.or..true.) then
					write(n16,*)'**fact** xps <38xx>',jlp_xps
				endif !if(p) then

			endif !if(j_fpresent) then
		enddo !do i=1,jlp_nunits
		
		
		if(ivprint7.gt.0)then
			if(j_v(ivprint7).eq.6.)call printproblem()
		endif !if(ivprint7.gt.0)then
		if(p) write(n16,*)'xps bef preopt',(jlp_xps(jj7),jj7=0,min(jlp_nrow,50)) !!!!
		if(p)write(n16,*)'nrowp',jlp_nrowp
		if(iprint.gt.1)write(6,*)'***row, min,max,initial value, tolerance'

	! do i=j_lopp-15000,j_lopp
	! iba=jxmatiba(i,1)
	! write(17,*)j_o(ivxmat)%r(iba+1:iba+7),j_o(ivxmat)%r(iba+11:iba+17)
	! iba=jxmatiba(i,1)
	! write(18,*)j_xmat(iba+1:iba+j_ntemp0)
	! enddo


		do jj=1,jlp_nrowp   !nrowp = number of rows with x-variables
			j=jlp_ixcurrows(jj) !j= number of the row
		!1.d-5 changed into 1.d-6 21.12.2009
		! it must be studied if this tolerance is reasonable
			jlp_tole(j)= max(jlp_tolep*1.d-6* (jlp_xsmax(j)-jlp_xsmin(j))/ &
			jlp_nunitsrow(j),jlp_tolep*1.d-7)
			if(iprint.gt.1)& 
			write(6,'(i5,1x,4g18.12)')j,jlp_xsmin(j),jlp_xsmax(j),jlp_xps(j),jlp_tole(j)
		enddo !do jj=1,j_nrowp

	!toleranssi tehdasmja-riveille  !!!!
		if (jlp_fpresent) then
			do j=0, jlp_nrow
				if (jlp_ix(j)==0) then
					if ((jlp_nfxrow(jlp_irowrow(j))>0).or.(jlp_nfyrow(jlp_irowrow(j))>0)) then
					!if(iprint.gt.1)  write(6,'(i5,1x,2g18.12)')j,j_xps(j),j_tole(j)
						if(p) write(n16,*)'***fact j,nunits,xps,tole', j,jlp_nunitsrow(j), &
						jlp_xps(j),jlp_tole(j)
					endif !if ((j_nfxrow(j_irowrow(j))>0).or.(j_nfyrow(j_irowrow(j))>0)) then
				endif !if (j_ix(j)==0) then
			enddo !do j=0, j_nrow
		endif !if (j_fpresent) then

	!preoptimoinnin ohitus jos tehtaita mukana (jatketaan vain jos !fpresent)
		if(jlp_nrow.ne.0.and.jlp_xpresent.and..not.jlp_fpresent.and.nz.eq.0)then
		!preoptimization  !!!!
			do kief=1,1
				apusum=jlp_zero
				do i=1,jlp_nunits
					call jlpcurix(i) !determines for each row if the unit iunit belonggs to the domain of the row
				! returns nrowp and ixcurrows
					jlp_valueopt=j_big
					do k=1,jlp_nsch(i)
						if(subfilre)then
							if(jlp_rejects(jlp_ibaunit(i)+k))cycle
						endif !if(subfilre)then
						jlp_value=jlp_zero  !value is the sum of infeasibility
						if(jlp_ixcurrows(1).eq.0)then
							j1=2
						else !if(j_ixcurrows(1).eq.0)then
							j1=1
						endif !if(j_ixcurrows(1).eq.0)then
						ibax=jxmatiba(jlp_ibaunit(i)+k) !,1)
						ibax2=jxmatiba(jlp_ibaunit(i)+jlp_keys(i)) !,2)
						do jj=j1,jlp_nrowp
							j=jlp_ixcurrows(jj)
							curx=jlp_xmat(jlp_ix(j)+ibax)
							curk=jlp_xmat(jlp_ix(j)+ibax2)
							if(jlp_xps(j)+curx-curk.gt.jlp_rhs2(j))then
								jlp_value=jlp_value+jlp_xps(j)+curx-curk-jlp_rhs2(j)
							elseif(jlp_xps(j)+curx-curk.lt.jlp_rhs(j))then !if(j_xps(j)+curx-curk.gt.j_rhs2(j))then
								jlp_value=jlp_value+jlp_rhs(j)-jlp_xps(j)-curx+curk
							endif !if(j_xps(j)+curx-curk.gt.j_rhs2(j))then

						enddo !do jj=j1,j_nrowp
						if(jlp_value.lt.jlp_valueopt)then
							jlp_valueopt=jlp_value
							kopt=k

						endif !if(j_value.lt.j_valueopt)then
						if(k.eq.jlp_keys(i))jlp_valuek=jlp_value
					enddo !do k=1,j_nsch(i)
					if(jlp_valuek.gt.jlp_valueopt)then
						ibax=jxmatiba(jlp_ibaunit(i)+kopt) !,1)
					! ibax2=j_ibaunit(i)+j_keys(i) put earlier

						do jj=1,jlp_nrowp
							j=jlp_ixcurrows(jj)
							curx=jlp_xmat(jlp_ix(j)+ibax)
							
							
							jlp_xps(j)=jlp_xps(j)+curx-jlp_xmat(jlp_ix(j)+ibax2)
							
							! if(i.le.2.and.j.eq.0)write(6,*)'<47747',curx,ibax2,jlp_ix(j)+ibax2,&
							! jlp_xmat(jlp_ix(j)+ibax2),jlp_xps(j)

						enddo !do jj=1,j_nrowp
						jlp_keys(i)=kopt
						apusum=apusum+jlp_valuek-jlp_valueopt

					endif !if(j_valuek.gt.j_valueopt)then
				enddo !do i=1,jlp_nunits
				write(6,*)'preoptimization round ',kief,' improved infeasibility sum by ',apusum
				write(6,*)' '
			enddo !do kief=1,1
			if(iprint.gt.1)then
				write(6,*)'initial values'
				do jj=1,jlp_nrowp   !nrowp = number of rows with x-variables
					j=jlp_ixcurrows(jj) !j= number of the row
					write(6,*)j,jlp_xps(j)
				enddo !do jj=1,j_nrowp
			endif !if(iprint.gt.1)then
			jlp_objf=jlp_small
		endif !if(j_nrow.ne.0.and.j_xpresent.and..not.j_fpresent.and.nz.eq.0)then

	!20181116 #z_commented	moved to #zeroc
	!		j_mxd=j_nrow+4  !???max number of d-vectors mx number of

	! constaints (la) + 1 or 2 or somethi
	!		integer lunit(0:mxd)	! unit of columns of D,
	! lunit(0)=0 is for termination purposes
		if(allocated(jlp_lunit))deallocate(jlp_lunit)
	!	schedule numbers for columns of D
		if(allocated(jlp_isch))deallocate(jlp_isch)

		if(allocated(jlp_ld))deallocate(jlp_ld)
		mxd2_ = jlp_mxd
		if (jlp_fpresent) mxd2_=2*jlp_mxd
		allocate(jlp_lunit(0:mxd2_),jlp_isch(1:jlp_mxd));jlp_lunit=0;jlp_isch=0

	! lunit tells for each column of D the corresponding unit
	! isch tells for each column of D the corresponding schedule
		if(allocated(jlp_ldi))deallocate(jlp_ldi)
		allocate( jlp_ld(1:jlp_mxd),jlp_ldi(1:jlp_mxd))
	!ld is list of all columns of A which are in D (i.e. in the x-part of the basis)
	! first ld0 are in basis
	!ldi is inverse list, D can be traversed in inverse order
		if (jlp_fpresent) then
			if(allocated(jlp_lf))deallocate(jlp_lf)
			if(allocated(jlp_lfi))deallocate(jlp_lfi)
			allocate(jlp_lf(jlp_mxd+1 :2*jlp_mxd),jlp_lfi(jlp_mxd+1 :2*jlp_mxd))

			if(allocated(jlp_nextf))deallocate(jlp_nextf)
			allocate(jlp_nextf(jlp_mxd:2*jlp_mxd,1:j_o(jlp_ivxkyk)%i(1)))
			if(allocated(jlp_iprevf))deallocate(jlp_iprevf)
			allocate(jlp_iprevf(jlp_mxd:2*jlp_mxd,1:j_o(jlp_ivxkyk)%i(1)))
			jlp_nextf=0
			jlp_nextf(jlp_mxd,1:j_o(jlp_ivxkyk)%i(1))=jlp_mxd;jlp_iprevf(jlp_mxd,1:j_o(jlp_ivxkyk)%i(1))=jlp_mxd
			if(allocated(jlp_lunxkf))deallocate(jlp_lunxkf)
			allocate(jlp_lunxkf(1:jlp_mxd))
			if(allocated(jlp_lunw))deallocate(jlp_lunw)
			allocate(jlp_lunw(1:jlp_mxd))
			jlp_lunxkf=jlp_mxd; jlp_lunw=0;jlp_lunits0=0

		!kannan saraketta vastaavan xkyk-muuttujan indeksi xkyk-listassa ja tehdas-listassa
			if(allocated(jlp_ixkf))deallocate(jlp_ixkf)
			allocate(jlp_ixkf(jlp_mxd+1:2*jlp_mxd))
			if(allocated(jlp_ixkffact))deallocate(jlp_ixkffact)
			allocate(jlp_ixkffact(jlp_mxd+1:2*jlp_mxd))
			jlp_ixkf=0;jlp_ixkffact=0

		endif !if (j_fpresent) then

		if(allocated(jlp_vx))deallocate(jlp_vx)
		allocate(jlp_vx(0:jlp_nrow))  !shadow prices of x-variables
	else !if(j_xpresent)then
	!20181116 #z_commented moved to	#zeroc
	!		j_mxd=0

		if(.not.zmatrix) nz=nvartot
	!*ix0 the x-variable of the objective function
		ix0=0
		jlp_nunits=0
		jlp_xpresent=.false.
	end if !if(j_xpresent)then

!kier=0  !number of iterations through data !!!!

!20181116 #z_commented	moved to #zeroc

!	! mitkä olivat xvar-muuttujat
!	la=j_nrow ! number of rows in Fletcher
!	j_lavec(1)=la
!	mxn=j_mxd+nz !mx number of columns in A  , I.e. D+ coefficients of z-variables
!	if(j_fpresent) mxn=mxn+j_mxd
!	ncol=mxn !!!
!	mxnm=mxn+j_nrow !mx number of columns (icluding the I part)
!	if(allocated(j_ls))deallocate(j_ls);if(allocated(j_lsi))deallocate(j_lsi)
!	allocate(j_ls(1:mxnm),j_lsi(1:mxnm))   ! ls= list of columns in A which are in the basis
!	! i.e. ls contains columns in ld-list plus columns of z-part of the A matrix
!	if(p) write(n16,*)'lsmax',mxnm
!	if(allocated(j_a))deallocate(j_a)
!	! sparse*******************************************************************
!	if(sparse)then  !should be tested if it works, not commented yer
!		! allocate(asp(nrow*(mxn+1))
!		!c Fletcher:
!		!c   The matrix A contains gradients of the linear terms in the objective
!		!c  function (column 0) and the general constraints (columns 1:m).
!
!		!c  In this sparse format, these vectors have dimension  a(1:nnza)  and
!		!c   la(0:lamax), where nnza is the number of nonzero elements in A,
!		!c  and lamax is at least  nnza+m+2.  The last m+2 elements in la are pointers.
!		!c  The vectors a(.) and la(.) must be set as follows:
!		!c  a(j) and la(j) for j=1,nnza are set to the values and row indices (resp.)
!		!c  of all the nonzero elements of A. Entries for each column are grouped
!		!c  together in increasing column order.
!		!c  The last m+2 elements of la(.) contain pointers to the first elements in
!		!c  the column groupings. Thus la(la(0)+i) for i=0,m is set to the location
!		!c  in a(.) containing the first nonzero element for column i of A. Also
!		!c  la(la(0)+m+1) is set to nnza+1 (the first unused location in a(.)).
!		!c  Finally la(0) is also a pointer which points to the start of the pointer
!		!c  information in la. la(0) must be set to nnza+1 (or a larger value if it
!		!c  is desired to allow for future increases to nnza).
!		nnzj=j_nrow*(ncol+1) !reserve for col 0
!		if(allocated(j_lavecsp))deallocate(j_lavecsp)
!		allocate(j_lavecsp(0:nnzj+ncol+2))
!		if(allocated(last))deallocate(last)
!		allocate(last(0:ncol))
!		if(allocated(j_acol))deallocate(j_acol);allocate(j_acol(1:j_nrow))
!		if(allocated(j_acolapu))deallocate(j_acolapu);allocate(j_acolapu(1:j_nrow))
!		if(allocated(j_icolapu))deallocate(j_icolapu);allocate(j_icolapu(1:j_nrow))
!
!		j_lavecsp=0
!		!rmemeber column zero
!		!c  la(la(0)+m+1) is set to nnza+1
!		j_lavecsp(0)=nnzj+1
!		j_lavecsp(nnzj+ncol+2)=nnzj+1
!
!		ie=0
!		j_lavecsp(nnzj+1)=1
!		do i=1,ncol
!			j_lavecsp(nnzj+i+1)=j_lavecsp(nnzj+i)+j_nrow
!		enddo !do i=1,ncol
!		! c  The last m+2 elements of la(.) contain pointers to the first elements in
!		!c  the column groupings. Thus la(la(0)+i) for i=0,m is set to the location
!		!c  in a(.) containing the first nonzero element for column i of A. Also
!		!c  la(la(0)+m+1) is set to nnza+1 (the first unused location in a(.)).
!
!		!c  Finally la(0) is also a pointer which points to the start of the pointer
!		!c  information in la. la(0) must be set to nnza+1 (or a larger value if it
!		!c  is desired to allow for future increases to nnza).
!		!  write(6,*)'lavecsp',lavecsp
!	endif !if(sparse)then
!	! if(sparse) *******************************************************************
!
!	allocate(j_a(1:j_nrow,0:mxn) )          ! A matrix
!	if(allocated(j_objr0))deallocate(j_objr0)
!	if(allocated(j_objr2))deallocate(j_objr2)
!	allocate( j_objr0(1:mxnm));allocate( j_objr2(1:mxnm))
!	if(allocated(j_xmi))deallocate(j_xmi,j_xma);if(allocated(j_xma))deallocate(j_xma)
!	allocate( j_xmi(1:mxnm),j_xma(1:mxnm))
!	j_xmi=j_zero ;j_xma=j_zero
!	j_a=j_zero
!	!*objr0 the 'official' objective row
!	!*objr2 the working obective row wehen the problem is infeasible
!	j_objr0=j_zero;j_objr2=j_zero
!
!	! z variables in the problem *******************
!	if(nz.gt.0)then  !nz=number of z-variables
!
!		!! commented. March 2011  if(iout.ne.ivresult)then
!		! jlp-function has output iout
!		if(.not.zmatrix) then
!			if(ivoutresult.ne.j_ivresult) then
!				call j_deflist(ivoutresult,'%zvars',nz,ivzvar)
!				j_zvarl=>j_o(ivzvar)%i
!			else !if(ivoutresult.ne.j_ivresult) then
!				if(associated(j_zvarl))deallocate(j_zvarl)
!				allocate(j_zvarl(0:nz))
!			endif !if(ivoutresult.ne.j_ivresult) then
!			! no output for the jlp-function
!			! z-variables: not z-variable, not factory´xvariables or factory y-variables
!			if (j_fpresent) then
!				j_zvarl(1:nz)=pack(j_o(ivvars)%i2(1:nvartot),.not.j_isx.and..not.j_isfx.and..not.j_isfy)
!			else !if (j_fpresent) then
!				j_zvarl(1:nz)=pack(j_o(ivvars)%i2(1:nvartot),.not.j_isx)
!			endif !if (j_fpresent) then
!			j_zvarl(0)=nz
!			! nval= number of coefficients in the
!			nzval=nval-nxval   !muuta kun fact
!			idomba=0  ! basis for values
!			irow0=0
!			irow=0
!			do i=1,nset  !number of domainset
!				do j=1,j_nsetd(i) !number of domains in set i
!					ival=idomba    !the same values are repeated fro each domain
!					do k=1,j_nsetr(i) !number of rows in set i
!						if(j.eq.1) irow0=irow0+1   !row accoding to intial (nonexpanded) row numbering
!						if(irow0.ne.irowobj.or.j.gt.1)then ! k.gt.1)then
!							!constraint row
!							irow=irow+1
!							do ii=1,j_nvars(irow0)
!								ival=ival+1   ! the number of variable with coefficient
!								if(.not.j_isxval(ival))then        !is the variable not x-variable
!									iz=j_inlist(j_irowvars(ival),j_zvarl)  !is it z-variable
!									! fact
!									! put coefficients of z variables into A matrix
!									j_a(irow,iz)=j_coef(ival)
!								endif !if(.not.j_isxval(ival))then
!							enddo !do ii=1,j_nvars(irow0)
!						else if(irow0.eq.irowobj.and.j.eq.1)then !if(irow0.ne.irowobj.or.j.gt.1)then
!							! the objective row, pick coefficients into objr0
!							! if the objective row is in a domain set and there are several domain
!							! then the objective function can refer only to the first domain
!							do ii=1,j_nvars(irow0)
!								ival=ival+1
!								if(.not.j_isxval(ival))then
!									iz=j_inlist(j_irowvars(ival),j_zvarl)
!									if(j_maxo)then  ! objective maximized
!										j_objr0(j_nrow+iz)=j_coef(ival) !object row
!									else !if(j_maxo)then
!										j_objr0(j_nrow+iz)=-j_coef(ival) ! -objective maximized
!									endif !if(j_maxo)then
!								endif !if(.not.j_isxval(ival))then
!							enddo !do ii=1,j_nvars(irow0)
!						endif !if(irow0.ne.irowobj.or.j.gt.1)then
!					enddo !do k=1,j_nsetr(i)
!				enddo !do j=1,j_nsetd(i)
!				idomba=ival
!			enddo !do i=1,nset
!		else !if(.not.zmatrix) then
!
!			iel=0
!			j_objr0(j_nrow+1:j_nrow+nz)=j_o(ivzobj)%r(1:nz)
!			do i=1,j_nrow
!				do j=1,nz
!					iel=iel+1
!					j_a(i,j)=j_o(ivzmatrix)%r(iel)
!				enddo !do j=1,nz
!			enddo !do i=1,j_nrow
!		endif !if(.not.zmatrix) then
!
!		if(p2)then
!			write(n16,'(a,(10f8.2/))')'obj row', j_objr0
!			write(n16,*)'amat'
!			do jj7=1,j_nrow;write(n16,'(10f8.2)')(j_a(jj7,nco7),nco7=1,ncol) ;enddo
!		endif !if(p2)then
!		!pack no
!		if(sparse)then  !sivuutetaan sparse toistaiseksi
!			do i=1,nz
!				ie=0
!				istart1=j_lavecsp(j_lavecsp(0)+i)-1
!				do j=1,j_nrow
!					if(j_a(j,i).ne.j_zero)then
!						ie=ie+1
!						j_a(ie,i)=j_a(j,i)
!						j_lavecsp(istart1+ie)=j
!					endif !if(j_a(j,i).ne.j_zero)then
!				enddo !do j=1,j_nrow
!				last(i)=j_lavecsp(istart1)+ie
!			enddo !do i=1,nz
!		endif !if(sparse)then
!
!			!zerocapacity
!			!poistetaan zerocista rajoitteet joilla z-muuttujia !!!!
!		if(j_fpresent)then
!			do irowj_ = 2,irowp
!				if(j_nfxrow(irowj_).eq.1.and. j_nfyrow(irowj_).eq.0.and.j_ix(irowj_-1).eq.0)then !.and.j_ix(irowj_).eq.0)then !
!					jxk=j_irowfxvars(j_ibafx(irowj_)+1)
!					jf = j_irowffact(j_ibafx(irowj_)+1)
!					do iz=1,nz
!						if(j_a(irowj_-1,iz).ne.0.)zeroc(jxk,jf)=.false.
!					enddo
!				endif
!			enddo
!		endif !		if(j_fpresent)then
!
!	endif !if(nz.gt.0)then
!	!**************************************** z-variables present
!20181116 #z_commented	moved to #zeroc ends

!fact

	if(jlp_xpresent)then

	!infeasiblity at start tarkastus siirretty tähän
	! test if problem is immediately infeasible !!!!
		iunf=0
		if(jlp_ixcurrows(1).eq.0)then
			j1=2  ! there is x variable in the objective row
		else !if(j_ixcurrows(1).eq.0)then
			j1=1   ! there is no  "
		endif !if(j_ixcurrows(1).eq.0)then
		nunf=0 ! number of infeasible rows
	!write(6,*)'<66nz ',nz
		rowloop:			do jj=j1,jlp_nrowp
			j=jlp_ixcurrows(jj)
	!	call j_printname(' tas22 ',j,' ')
			if(nz>0) then
				do inz_=1,nz
					if(jlp_a(j,inz_).ne.0.) cycle rowloop
				enddo !do inz_=1,nz
			endif !if(nz>0) then
			if(jlp_fpresent) then
				if(jlp_nfxrow(jlp_irowrow(j))>0.or.jlp_nfyrow(jlp_irowrow(j))>0) &
				cycle rowloop
			endif !if(j_fpresent) then
			if(jlp_xsmin(j).gt.jlp_rhs2(j).or.jlp_xsmax(j).lt.jlp_rhs(j))then
				nunf=nunf+1
				iunf=j
				if(nunf.eq.1)then
					write(6,*)'***the problem is infeasible at start, nonfeasible rows: '
					write(6,*)'<xsmin',jlp_xsmin
					if(nureport.ne.6)&
						write(nureport,*)'***the problem is infeasible at start, nonfeasible rows: '
				endif !if(nunf.eq.1)then
				call printrowinfo(iunf)
				if(jlp_xsmin(iunf).gt.jlp_rhs2(iunf))then
					write(6,*) &
					'*smallest possible value ',jlp_xsmin(iunf),' rhs2=',jlp_rhs2(iunf)
					if(nureport.ne.6)write(nureport,*)'row ',iunf,'*smallest possible value ',&
						jlp_xsmin(iunf),' rhs2=',jlp_rhs2(iunf)
				endif !if(j_xsmin(iunf).gt.j_rhs2(iunf))then
				if(jlp_xsmax(iunf).lt.jlp_rhs(iunf))then
					write(6,*)'*greatest possible value ', &
					jlp_xsmax(iunf),' rhs=',jlp_rhs(iunf)
					if(nureport.ne.6)write(nureport,*)'row ',iunf,&
						'*greatest possible value ',jlp_xsmax(iunf),' rhs=',jlp_rhs(iunf)
				endif !if(j_xsmax(iunf).lt.j_rhs(iunf))then
			endif !if(j_xsmin(j).gt.j_rhs2(j).or.j_xsmax(j).lt.j_rhs(j))then
		enddo rowloop !rowloop:			do jj=j1,j_nrowp
		if(iunf.ne.0)then
			return   ! pitäis tehdä jotain goto 8000
		endif !if(iunf.ne.0)then

		if(allocated(jlp_testxps))deallocate(jlp_testxps,jlp_test)
		if(allocated(jlp_test))deallocate(jlp_test)
		allocate(jlp_testxps(0:jlp_nrow),jlp_test(0:mxn))

	!		integer next(0:mxn) !  is used to travel through columns of D so that
	! the columns correponding to same unit are after each other.
	! if last is the last column in the sequence, then next(last)=0
	! next(0) first column
		if(allocated(jlp_next))deallocate(jlp_next)
		if(allocated(jlp_iprev))deallocate(jlp_iprev)
		allocate(jlp_next(0:mxn),jlp_iprev(0:mxn))

	endif !if(j_xpresent)then

	if(allocated(jlp_lower))deallocate(jlp_lower)  ! is lower bound active
	allocate( jlp_lower(1:jlp_nrow))
! lr links to I-part (slack/surplus) part of matrix, lri =inverse list
	if(allocated(jlp_lr))deallocate(jlp_lr);if(allocated(jlp_lri))deallocate(jlp_lri)
	allocate( jlp_lr(1:jlp_nrow),jlp_lri(1:jlp_nrow))
! lz links to z-part of the A matrix lzi inverse list
	if(allocated(jlp_lz))deallocate(jlp_lz,jlp_lzi,jlp_redcost)
! redcost = reduced costs of z-variables
	allocate( jlp_lz(1:nz),jlp_lzi(1:nz),jlp_redcost(1:nz))  !miksei testata onko nz.eq.0  ??
	if(allocated(jlp_vc))deallocate(jlp_vc)
	allocate(jlp_vc(1:jlp_nrow) )  ! eq. 6.42 in JLP manual  (price vector)

	if(allocated(jlp_x))deallocate(jlp_x,jlp_b)  !j_x2 added 201608
	allocate( jlp_x(1:mxnm) ,jlp_b(1:mxn) ) ! x:n dimensio +
! Fletcher routines solve a system  B.x=b
	if(p)write(n16,*)'X:n dimensio,b:n dimensio ',mxnm,mxn

!  nrow =number of constraints=number of variables for Fletcher's LP
! first index is for row number second for column

	iunitv=-1
	kierv=1
1234 continue  !we come here after failure,   !jl 20160510 siirretty tähän !!!!
!write(6,*)'newstart'
!write(16,*)'newstart'
	jlp_xirowold2=jlp_small

	if(jlp_xpresent)then
		jlp_next(0)=0;jlp_iprev(0)=0
	!tehdasosuden alustukset
		if(jlp_fpresent) then
			jlp_lunxkf=jlp_mxd; jlp_lunw=0;jlp_lunits0=0
			jlp_nextf=0
			jlp_nextf(jlp_mxd,1:j_o(jlp_ivxkyk)%i(1))=jlp_mxd
			jlp_iprevf(jlp_mxd,1:j_o(jlp_ivxkyk)%i(1))=jlp_mxd
		endif !if(j_fpresent) then
	endif !if(j_xpresent)then


! get currenet RHS and working rhs
	do i=1,jlp_nrow
	! lower-logiikka epävarma vielä
		if((jlp_ix(i).ne.0).or.jlp_fpresent)then   ! there is a x-variable in the row
		! x-variable present -xps goes to the rhs
			if(jlp_xps(i).gt.jlp_rhs2(i).or..not.jlp_lbou(i))then
				jlp_rhscur(i)=jlp_rhs2(i) ! select either lower or upper bound
			! rhscur = current bound
				jlp_lower(i)=.false.
			else !if(j_xps(i).gt.j_rhs2(i).or..not.j_lbou(i))then
				jlp_rhscur(i)=jlp_rhs(i)
				jlp_lower(i)=.true.
			endif !if(j_xps(i).gt.j_rhs2(i).or..not.j_lbou(i))then
		!xps is the sum over key schedules
			jlp_rhsw(i)=jlp_rhscur(i)-jlp_xps(i)  ! 6.27 and 6.28 p. 110 , get working rhs
		else !if((j_ix(i).ne.0).or.j_fpresent)then
			if(jlp_lbou(i))then
				jlp_rhscur(i)=jlp_rhs(i)
				jlp_lower(i)=.true.
			else !if(j_lbou(i))then
				jlp_rhscur(i)=jlp_rhs2(i)
				jlp_lower(i)=.false.
			endif !if(j_lbou(i))then
			jlp_rhsw(i)=jlp_rhscur(i)
		endif !if((j_ix(i).ne.0).or.j_fpresent)then

	enddo !do i=1,j_nrow
	if(p2)write(n16,*)'rhscur',jlp_rhscur
	if(p2)write(n16,*)'rhsw',jlp_rhsw

! Fletcher subroutines initilize data structures for taht many cols
!*nrowz
	jlp_nrowz=jlp_nrow+nz  ! basis (first element -1) of D part, when I is included
! coefficients for residuals
! coefficients for d columns
	do j=jlp_nrowz+1,mxnm !!!ncol ol liian vähän
		jlp_xma(j)=1.  ! maximum value for w, later area of the unit
	enddo !do j=j_nrowz+1,mxnm

	nm=ncol+jlp_nrow    ! number of columns in (I A)
! pitäs olla sama kuin
! for fletcher m= ncol
!              n= nrow vai pitäiskö nrowt ??
!		nmi=nm
!		nk=0
! kmax = maximum value of k (set kmax=0 iff the problem is an LP problem)
!		kmax=0   ! LP

!FLS     FLS= flag for subroutines of Fletcher
! initilize
!		call  stmap(nrow,nm,kmax)  !for flethcer first argument is number of cols

! set storage map for workspace in bqpd and auxiliary routines
!		mode=0
!		ifail=mode
! m0de  mode of operation (larger numbers imply extra information):
! 0 = cold start (no other information available, takes simple
! bounds for the initial active set)
! FLS

	if(jlp_nrow.eq.0.and..not.jlp_fpresent)then   ! no constraints, was optimized directly
		if(nz.gt.0)then
			write(6,*)'**no constraints and z variables=> illegal'
			j_v(ivfeasible)=0.
			j_v(ivoptimal)=0.
			j_v(ivobjective)=-9.9
			j_err=.true.
			return
		endif !if(nz.gt.0)then
		jlp_objf=jlp_xps(0)
		jlp_vx(0)=1.

		jlp_ld0=0
		lz0=0
		lr0=0
		jlp_lf0=0
		if(jlp_xpresent2)then
			call defsolu()
			call getsolx()
		endif !if(j_xpresent2)then
		j_v(ivfeasible)=jlp_one
		j_v(ivoptimal)=jlp_one
		j_v(ivobjective)=jlp_objf
		j_v(j_ivstartedjlp)=1.
		write(6,*)'no constraints, objective ',jlp_objf
		if(nureport.ne.6)call j_printname( &
		'*report written into:',ivreport,' which remains open')
		call repo(nureport)
!	write(6,*)'77 ',nureport,echo
		if(nureport.ne.6.and.echo)call repo(6)

		return
	endif !if(j_nrow.eq.0.and..not.j_fpresent)then
	j_v(j_ivstartedjlp)=1.  !!!!

! milloin refactoroidaa, Fletcherin refactoroinnissa on bugi

! initilize Fletcher routines
	if(sparse)then
		call initfletsp(jlp_nrow,ncol,jlp_a,la,jlp_lavecsp,jlp_ls,lu1,ll1,ifail,nrefac)
	else !if(sparse)then
	!*nrow = n for Flet

		call initflet(jlp_nrow,ncol,jlp_a,la,jlp_lavec,jlp_ls,lu1,ll1,ifail,nrefac)
	endif !if(sparse)then
	if(p)     write(n16,*)'startup,nrow,nm,la,lu1,ll1',jlp_nrow,nm,la,&
		lu1,ll1
! testi
	if(p)write(n16,*)'startup,nrow,ncol',jlp_nrow,ncol,' ifail :',ifail
	if(ifail.gt.0)then
		write(6,*)'**at startup ifail=',ifail, 'tell J. Lappi'
		j_err=.true.
		return

	endif !if(ifail.gt.0)then
	do j=1,nm !nm=ncol+nrow    ! number of columns in (I A)
		jlp_lsi(j)=j    !intially ls is in order, residuasl are in the basis
	enddo !do j=1,nm
	ll0=ll1-1

! residuals are initially in the basis
! lr =list columns of I, i.e. residuals
	do i=1,jlp_nrow
		jlp_lr(i)=i
		jlp_lri(i)=i	!inverse list
	enddo !do i=1,j_nrow
	lr0=jlp_nrow		! all residuals are in the basis
	do i=1,nz
		jlp_lz(i)=i		! list allz-cols in order
		jlp_lzi(i)=i	! nrow not included
	enddo !do i=1,nz
	lz0=0			! no z-variable is in the basis
!käytetään mxd
!mxd2=ncol-nz	! actual max num of colums for D, sotkua???? parempi luku
!      write(6,*)'mxd,ncol,nz,mxd2',mxd,ncol,nz,mxd2
	do i=1,jlp_mxd
		jlp_ld(i)=i          !lists columns of D
		jlp_ldi(i)=i        !inverse list
	enddo !do i=1,j_mxd
	jlp_ld0=0	! number of d -variables in the basis

	if (jlp_fpresent) then
		do i=jlp_mxd+1,2*jlp_mxd
			jlp_lf(i)=i          !lists columns of D
			jlp_lfi(i)=i       !inverse list
		enddo !do i=j_mxd+1,2*j_mxd
		jlp_lf0=jlp_mxd
	!jatketaanko seuraavasta tehdasmjasta tarkastelu/ienter, ixkykenter alustukset
		ienter= 0
		ixkykenter = 0
		iunitrans = 0	! yksikkö, jolle viimeksi laskettu tehdasmja muunnokset
	endif !if (j_fpresent) then

	iunit=0  !!!!!current unit
	jlp_lunit0=0 ! last unit with improvement
	newc=0

	muutosb=0  !changes in the basis
	jlp_objfv=jlp_small    !for checking if there has not been improvemne tsince last !!!!
	jlp_objf = jlp_small   !!!!

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
! go trough all possible cominations when a basic variable can leave
! residual (and later z variable) can leave by reaching upper or lower limit
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
	if(.not.jlp_maxo)write(6,*)'**minimization: maximize -objective'
	iunit55=0;niter=0
	nkier=mxiter
	nunits=jlp_nunits
	if(.not.jlp_xpresent2)then
		nkier=1
		nunits=1
	endif !if(.not.j_xpresent2)then
!if(j_o(ivxmat)%r(10261*keepx).eq.0.)stop 90
! iba=jxmatiba(6841,1)
! iba2=jxmatiba(6842,2)
! write(18,*)'<129>'
! write(18,*)j_o(ivxmat)%r(iba+1:iba+keepx)
! write(18,*)j_o(ivxmat)%r(iba2+1:iba2+keepx)
!if(pp)write(16,*)'***initialupdate'
	call updatejlp()
!write(16,*)'ALKU updgoto900,goto1234,goto8888,goto8889',goto900,goto1234,goto8888,goto8889,&
!iunit,ipivot,j_objf
	goto900=.false.;goto1234=.false.;goto1578=.false.
	goto400=.false.;goto36=.false.
	goto100=.false.;goto5316=.false.;goto222=.false.;goto55=.false.
	goto8888=.false.;goto8889=.false.;goto700=.false. 	! input for leaving
	goto401=.false.
	!note all other gotos tell where to go next in the upper level
	! except goto8888 and goto8889 are set in updatejlp
	!and they tell where to go in leaving()
	!all gotos are cleared in each subroutine if they are set in the subroutine

!if(kier.gt.1.and.iterxkf*int(kier/iterxkf).ne.kier)cycle unitloop !goto 400
!		iterxkf number of rounds without cheking factories
	nofact=0  !current number of rounds without factories
	factnow=jlp_fpresent

!write(6,*)'alus',j_xmatlopp,j_xdatlopp,j_xmatibas2,j_xdatibas2,j_xmatlast,j_xdatlast
!write(16,*)'ienteralsu',ienter,'nnf',nnf
!write(6,*)'br, bz, bs,bxkf =number of basic residuals, z-vars, scheds, xkf-vars'
	call cpu_time(time0)
	write(6,*)'it took ',time0-time00,' seconds'
	write(6,*)'*optimization options:'

	write(6,*)'refac->',nrefac,' tole->',jlp_tolep, &
	' warm->',warm,'refac->',nrefac, 'maxiter->',mxiter

	if(jlp_xpresent2)then
		write(6,*)'fastrounds->',fastusesame,'fastpercent->',fastpros

		if(jlp_fpresent)write(6,*)'finterval->',iterxkf

		if(isstop)then
			write(6,*)'stop->   :see jlp(stop->'
		else !if(isstop)then
			write(6,*)'stop->(Change%.lt.0.01.and.Round.ge.30)'
		endif !if(isstop)then
	endif !if(j_xpresent2)then
	write(6,*)'printing option, print->',iprint
	!if(memory.eq.0.and.j_xdatfromdisk)write(6,*)'without memory->, xmat is put into memory, but xdata is used from disk'

	!if(memory.eq.0.and..not.j_xdatfromdisk)write(6,*)'without memory-> all data is in memory'
	if(jlp_xpresent2)then
		write(6,*)'** Resid = # of residuals                z = # of basic z-variables'
		write(6,*)'   sched = # of explicit basic scheds  xkf = # of basic factory transportations'
		write(6,*)'      NF = # of nonfeafible rows'


	endif !if(j_xpresent2)then
	iobs=0
!do ii=1,nunits
!do jj=1,6844

! iba=jxmatiba(6841,1)
! iba2=jxmatiba(6842,2)
! write(18,*)'<19>'
! write(18,*)j_o(ivxmat)%r(iba+1:iba+keepx)
! write(18,*)j_o(ivxmat)%r(iba2+1:iba2+keepx)
! !write(18,*)iobs,j_o(ivxmat)%r(iba+1),j_o(ivxmat)%r(iba+keepx),j_xmat(iba2+1),j_xmat(iba2+j_ntemp0)

!stop 967

	nimp=0  !number of improvements outside active set
!	nimpr=0   !number of rounds with fastmake between printing
	asv=100.
	jlp_objfprev=0.d0
	call cpu_time(time0)
	time00=time0  ;ncyc=0
	write(6,*)' '
!	write(6,*)'Round  Pivots      Objective                           active% resid   z   sched  xkf  NF-rows'
	if(jlp_xpresent2)write(6,*) &
	'Round  Pivots      Objective      Change% active% resid     z  sched  xkf    NF  dCPU    CPU   imp'
!write(6,*)'xmatfromdisk',j_xdatformdisk
!if(j_o(ivxmat)%r(10261*keepx).eq.0.)stop 100
	goto401=.false.
	kierloop: do kier=1,nkier
!write(6,*)'kier ',kier

		iunit=1
!	iunit=1
	!if(pp)write(16,*)'kier,fast,fastmake',kier,fast,fastmake
	! if(fastmake)then
		! !if(pp)write(16,*)'fastmake alkaa',j_xmatinmemory,j_xdatinmemory,j_xdatfromdisk
		! if(.not.j_xmatinmemory)j_xmatlast=0
		! if(.not.j_xdatinmemory)j_xdatlast=0
	! endif
	! how many round through all units

	!kier=kier+1 !!!! <B332> new round completed
	! write(16,*)'tas'
		if(jlp_xpresent2)then
			call tulostele2()
		!if(goto900)write(6,*)'<55>goto00nyt'
			if(goto900)exit kierloop !goto 900
		!if(goto785)goto 785  !maxiter
		!if(goto699)goto 699

			jlp_xirowold2=jlp_objf
			if(nnf.ne.nnfold2)jlp_xirowold2=jlp_small
			nnfold2=nnf
		endif !if(j_xpresent2)then
!write(6,*)'<55nunits',nunits

		unitloop: do iunit=1,nunits
!if(pp)write(16,*)'kier,iunit',kier,iunit
	! if(j_fpresent)then !make transformations
		! if ((nfy.gt.0)) then
			! do j=1,j_o(j_ivkeepc)%i(1)
				! j_v(j_o(j_ivkeepc)%i(j))=j_o(ivcmat)%r((iunit-1)*j_o(j_ivkeepc)%i(1)+j)
			! enddo !do j=1,j_o(j_ivkeepc)%i(1)
			! do j=1,ntrans
				! call dotrans(j_itransv(j),1)
			! enddo !do j=1,ntrans
			! iunitrans=iunit
		! endif !if ((nfy.gt.0).and.(iunit/=iunitrans)) then
	! endif

! if(fastmake)then
	! if(.not.j_xmatinmemory)then
		! iba=j_xmatibas2
		! do i=1,j_nsch(iunit)
			! iobs=j_ibaunit(iunit)+i
			! read(j_xmatnu,rec=iobs)j_xmat(iba+1:iba+j_ntemp0)
		 	! j_memobs(iobs)=j_xmatlopp+i   !katsotaan myöei ole väliä
! !			write(16,*)'eka',iobs,j_memobs(iobs),kier,iunit,i,j_nsch(iunit),iba+j_ntemp0,size(j_xmat)
			! iba=iba+j_ntemp0
		! enddo
		! j_xmatfirst2=j_ibaunit(iunit)+1
		! j_xmatlast2=j_ibaunit(iunit)+j_nsch(iunit)

	! endif
	! if(j_xdatfromdisk)then  !only in factory
		! iba=j_xdatibas2
		! do i=1,j_nsch(iunit)
			! iobs=j_ibaunit(iunit)+i
			! read(j_xdatnu,rec=iobs)j_o(ivxmat)%r(iba+1:iba+keepx)
			! iba=iba+keepx
		! enddo
		! j_xdatfirst2=j_ibaunit(iunit)+1
		! j_xdatlast2=j_ibaunit(iunit)+j_nsch(iunit)

	! endif
! endif


!if(kier.gt.1.or.iunit.gt.1)
!write(6,*)'<57hei',goto401,j_xpresent2
			goto401=.false.
			if(jlp_xpresent2)goto401=.true.  !tai 700
			sentered=.false.   ! is a schchedule entered
!write(6,*)'<57hei',goto401
			inunitloop: do while(.true.)
 !write(17,*)'inunit',kier,goto401
				if(goto401)goto 401  ! ei tarvi päivittää
55 continue !!!!********************************** main cycle

!if(pp)write(16,*)'inunit,update alkaa**',fast,fastemake,kier,goto900,goto1234,goto8888,goto8889,j_objf,ienter
!write(6,*)'<57hei2',goto401
				call updatejlp()
!write(6,*)'<57hei3',goto401,goto900,goto1234,goto8888,goto8889
!write(16,*)'updatenjälkeen***',kier,goto900,goto1234,goto8888,goto8889,j_objf,ienter
!write(16,*)'updgoto900,goto1234,goto8888,goto8889',goto900,goto1234,goto8888,goto8889,&
!iunit,ipivot,j_objf
!if(goto900)write(6,*)'<27>goto900tas'
				if(goto900)exit kierloop;
				if(goto1234)goto 1234  !virhe
				if(goto8888.or.goto8889)goto 100;  !leaving

				ienter=0  ! type of entering
				call renter()
!write(16,*)'renter,ienter,goto35,goto36',ienter,goto35,goto36
!if(goto35)goto 35
				if(goto36) goto 36 !bypass renter and zenter because the is no improvement how this relates to cycling


!if(ienter.ne.0.and.p)write(n16,*)' maxcol ',newc,' vcmax',j_vcmax
				if(ienter.ne.0)goto 100  !!!! select leaving variable    !<b1>

! z enters *****************
! loop over nonbasic z:s
!35 continue   !!!! start z-enters ******************* ! <B2>
				call zenter()

!write(16,*)'zenter,ienter,goto35,goto36',ienter,goto35,goto36

				if(ienter.ne.0)goto 100  !!!!! get leaving variable   !<b2>

! vc always of correct sign for maximization
 36 continue   ! kun sivuutetaan residual enters ja z
!*************************************************************
!!!! can new schedule enter ********************************** <B3>
!!!! get first the prices of x-variables


! testiä , tähän tullaan myös jos w:t sai laittomat arvot
				if(jlp_testl)call testcol()

!jatketaanko seuraavasta tehdasmjasta !!!!
				if (jlp_fpresent) then
					if(jlp_nextxkf) then   !!!!
		!tarkistetaan pitääkö laskea muunnokset !!!! <A1> tehdaan alussa
		! iunitrans yksikkö, jolle viimeksi laskettu tehdasmuunnokset
						if ((nfy.gt.0).and.(iunit/=iunitrans)) then
							do j=1,j_o(jlp_ivkeepc)%i(1)
								j_v(j_o(jlp_ivkeepc)%i2(j))= &
								j_o(ivcmat)%d((iunit-1)*j_o(jlp_ivkeepc)%i(1)+j)
							enddo !do j=1,j_o(j_ivkeepc)%i(1)
							do j=1,ntrans
								call dotrans(jlp_itransv(j),1)
								if(j_err)then
									write(6,*)'error for transformation ',j
								endif !if(j_err)then
							enddo !do j=1,ntrans
							iunitrans=iunit
						endif !if ((nfy.gt.0).and.(iunit/=iunitrans)) then
						if(p) write(n16,*)'<5836> nextxkf true'
						goto 5316   !!!! jatketaan seuraavasta tehdasmuuttujasta
		!!!! varmistetaan tehdasmjien läpikäynti alusta tilanteessa,
		!!!! jossa yksikön tehdasmja-listan läpikäyntiä ei jatketa
					else !if(j_nextxkf) then
						ixkykenter = 0  !!!!
					endif !if(j_nextxkf) then
				endif !if (j_fpresent) then

!***********

400	continue  !!!!! goto next unit etc
!write(17,*)'400kier',kier
				cycle unitloop

401 continue
				iunitprev=iunit  !!!!
				goto401=.false. !we jump here when entering inunitloop first time
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!	call ispe(isper,1) !testing negative amounts
!write(20,*)'iunitv,kier,kierv,iunitprev',iunitv,kier,kierv,iunitprev
				if((iunitprev.eq.iunitv.and.kier.gt.kierv).or.(iunitv==-1.and.kier.gt.kierv))then
!!!!! there was no change since last visit to the same unit , optimum found,return !<B331>
	!maxiter
!	write(16,*)'tasny,goto700',goto700
	!call tulostele()
					if(.not.goto700)then
						if(ipivot9.ne.ipivot)then  !vois katsoa muutkin muutokset
							call cpu_time(time)
							if(jlp_feasible)then
								if(jlp_xpresent2)then
									if(jlp_objfprev.gt.0.d0)pros= &
									100.d0*coefmax*(jlp_objf-jlp_objfprev)/jlp_objfprev
									change=jlp_objf-jlp_objfprev
									nimp2=-1
									if(as.ne.asv.and.asv.ne.100.)nimp2=nimp


									asv=as
			!	j_buf(1:12)=' ';j_buf(2:11)=j_chr10(j_objf-j_objfprev)
			!	j_objfprev=j_objf
									write(6,'(i5,i8,g22.12,f8.4,f7.2,5i6,f6.2,f7.2,i6)')&
										kier,ipivot,coefmax*jlp_objf,pros,as,lr0,lz0,jlp_ld0,jlp_lf0,nnf,(time-time0)/60.,(time-time00)/60.,nimp2
								else !if(j_xpresent2)then
									write(6,*)'Feasible, objective:', &
									coefmax*jlp_objf,'basic residuals ', lr0, &
									' basic z-vars ',lz0, ' cpu ',(time-time00)/60.
								endif !if(j_xpresent2)then
				!time0=time
							endif !if(j_feasible)then
						else !if(ipivot9.ne.ipivot)then
							if(jlp_xpresent2)then
								write(6,'(i5,i8,g22.12,7x,f8.2,5i6,f6.2,f7.2)')&
									kier,ipivot,coefmax*jlp_objf,as,lr0,lz0,jlp_ld0, &
									jlp_lf0,nnf,(time-time0)/60.,(time-time00)/60.
							else !if(j_xpresent2)then
								write(6,*)'Optimal, objective:', &
								coefmax*jlp_objf,'basic residuals ', lr0, ' basic z-vars ',lz0, ' cpu ',(time-time00)/60.
							endif !if(j_xpresent2)then
						endif !if(ipivot9.ne.ipivot)then
!write(6,*)'<654exitkier'
						if(jlp_xpresent2)write(6,*)'no changes during last round'
						if(jlp_feasible)write(6,*)'SOLUTION FOUND'

						exit kierloop
					endif !if(.not.goto700)then
				endif !if((iunitprev.eq.iunitv.and.kier.gt.kierv).or.(iunitv==-1.and.kier.gt.kierv))then

! 700   continue
!***************************************************************

!!!! the most time consuming loop
!!!! compute the value of each schedule, take the largest + the value of the key schdeule
!!!! if the largest valuie is greater than the vlaue of the key schedule this schedule can enter
				if(.not.jlp_xpresent)then  !!!check if optimal with only z-problem   <R1>
					if(ienter.ne.0)goto 100
					write(6,*)'*** solution,obj',jlp_objf

					j_v(ivoptimal)=1.
					exit kierloop !goto 900          !   <r1>
				endif !if(.not.j_xpresent)then
!if(pp)write(16,*)'beforesenter,iente',ienter,iunit,j_objf,nnf,ipivot,kier,goto400

				call senter()  !note ienter=3 is not yet set it is set in entercol
!if(pp)write(16,*)'senter,iente',ienter,iunit,j_objf,nnf,ipivot,kier,goto400
				if(goto400)exit inunitloop !cycle unitloop !goto 400
				sentered=.true.
!write(17,*)'senter2,iente',ienter,iunit,j_objf,nnf,ipivot,kier
! end tärkein looppi
!********************end: can a schedule enter
222	continue !secondb


!if(pp)write(16,*)'unit,ns,valopt,valk,key,iopt',iunit,j_nsch(iunit),&
!j_valueopt,j_valuek,j_keys(iunit),iopt
! if finding feasible voisi suoraan katsoa, että valuoptin ja valuek:n eroa

				if(iopt.eq.jlp_keys(iunit))then  !!!!
					if (.not.jlp_fpresent)then   !!!!
						exit inunitloop !cycle unitloop !goto 400   !goto next unit , 400 is earlier !!!!
					else !if (.not.j_fpresent)then
!	write(16,*)'goto5316tas'
						goto 5316 ! fpresent &&  (iopt == key)  !!!!
					endif !if (.not.j_fpresent)then
				endif !if(iopt.eq.j_keys(iunit))then
				if(jlp_valueopt.le.jlp_valuek+jlp_tolecur) then  !!!!
					if (.not.jlp_fpresent)then   !!!!
						exit inunitloop !cycle unitloop !goto 400   !goto next unit , 400 is earlier !!!!
					else !if (.not.j_fpresent)then
!	write(16,*)'goto5316tas'
						goto 5316  ! fpresent &&  (valueopt <= valuek+tolecur) !!!!
					endif !if (.not.j_fpresent)then
				else !if(j_valueopt.le.j_valuek+j_tolecur) then
					if(jlp_fpresent)call fenter0() !;if (j_fpresent)call fenter0()
	!if(pp)write(16,*)'befentcol'
					call entercol()
	!if(pp)write(16,*)'aftentcol',ienter,goto100
!	endif !if (j_fpresent) then
!	write(16,*)'goto5381'

					goto 100 !goto 5381 ! (iopt != key) && (valueopt > valuek+tolecur) !!!!
				endif !if(j_valueopt.le.j_valuek+j_tolecur) then

! <b34>
5316		continue  !!!!!testataan voiko xkf tulla kantaan <C2>

				if(kier.gt.1.and.iterxkf*int(kier/iterxkf).ne.kier.and..not.sentered)exit inunitloop !cycle unitloop !goto 400

				if(factnow)call fenter() !j_fpresent)call fenter() ! ;if(ienter.ne.0)write(16,*)ienter,goto100,goto400,goto5316
!write(17,*)'fenter',ienter,goto100,goto400,goto5316,kier
				if(goto100)goto 100  !leaving
				if(goto400)exit inunitloop  !cycle unitloop !goto 400
				if(goto5316)goto 5316
!endif
! there is change, not yet quarantee againts degenerate loop, because

! 5381 call entercol()


100 continue  !!!! select leaving variable
!goto8888 is now input which may be set in updatejlp
!goto2244 and goto 8889 are input
!goto8883 input
!if(pp)write(16,*)'beleav,goto222,goto900,goto55,goto1234',goto222,goto900,goto55,goto1234,j_objf,ipivot,kier
!if(pp)write(16,*)'before leaving,ienter',ienter
				call leaving()
 !if(pp)write(16,*)'after leaving,ienter',ienter,newa,j_tmax
 !if(pp)write(16,*)'after leaving,ienter',ienter,newa,j_tmax
 !if(pp)write(16,*)'alleav,goto222,goto900,goto55,goto1234',goto222,goto900,goto55,goto1234,j_objf,ipivot,kier
 !write(17,*)'leav,goto222,goto900,goto55,goto1234',goto222,goto900,goto55,goto1234,j_objf,ipivot,kier
				if(goto222)goto 222
 !if(goto900)write(6,*)'<88>goto900aftleaving'
				if(goto900)exit kierloop
				if(goto1234)goto 1234
! if(goto55)goto 55
				if(goto112233)exit inunitloop
!goto 55      !!!!computations after normal pivot are done, goto computing basic vars
			enddo inunitloop !inunitloop: do while(.true.)

! if((fastusesame.ne.1.or.iterxkf.ne.1).and..not.tried)then          !fast
	! write(6,*)'no improvement, pivots ',ipivot,' rounds ',kier, 'obj ',j_objf
! !	write(6,*)'let us continue without heuristics'
	! ! fast=.false.
	! ! fastnow=.false.
	! ! iterxkf=1
	! ! tried=.true.
	! ! j_xirowold2=j_small
	! ! iunitv=iunitprev
	! ! kierv=kier
	! ! goto700=.true.;return
	! write(6,*)'regular return'  !!!!
	! exit kierloop

! endif
!tähän voi laittaa unitin jälkeisiä tarkastuksia
!goto700=.false.


!write(17,*)'aftendinunitloop',kier,iunit
		enddo unitloop !unitloop: do iunit=1,nunits
		if(jlp_fpresent.and.jlp_feasible)then
			if(.not.factnow)then
				nofact=nofact+1
				if(nofact.gt.iterxkf)then
					write(6,*)'factories will now be checked,round,ipivot,obj', kier,ipivot,jlp_objf
					factnow=.true.
					nofact=0
				endif !if(nofact.gt.iterxkf)then
			else !if(.not.factnow)then
!	write(6,*)'round,factnow put to false',kier
!		factnow=.false.
			endif !if(.not.factnow)then
		endif !if(j_fpresent.and.j_feasible)then

!write(17,*)'aftunitloop',kier,iunit
	enddo kierloop !kierloop: do kier=1,nkier



900 continue
	call tulostele()

	if(nureport.ne.6)call j_printname('*report written into:',ivreport,' which remains open ')

	call repo(nureport)
	if(echo.and.nureport.ne.6)call repo(6)


	call closeflet()
	if(allocated(jlp_ls)) deallocate(jlp_ls)   !xvarl;
	if(allocated(jlp_lsi)) deallocate(jlp_lsi)   !xvarl;
	if(allocated(jlp_a)) deallocate(jlp_a)   !xvarl;
	if(allocated(jlp_objr0)) deallocate(jlp_objr0)   !xvarl;
	if(allocated(jlp_objr2)) deallocate(jlp_objr2)   !xvarl;
	if(allocated(jlp_xmi)) deallocate(jlp_xmi)   !xvarl;
	if(allocated(jlp_xma)) deallocate(jlp_xma)   !xvarl;
	if(allocated(jlp_x)) deallocate(jlp_x)   !xvarl;
	if(allocated(jlp_b)) deallocate(jlp_b)   !xvarl;
	nullify(jlp_objr)

930 	continue

	if(jlp_xpresent)then
		if(allocated(jlp_ivdomains)) deallocate(jlp_ivdomains)
		if(allocated(jlp_ibatemp)) deallocate(jlp_ibatemp)
		if(allocated(jlp_nxrowtemp)) deallocate(jlp_nxrowtemp)
		if(allocated(jlp_ixprow)) deallocate(jlp_ixprow)
		if(allocated(jlp_coefx)) deallocate(jlp_coefx)
		if(allocated(jlp_irowxvars)) deallocate(jlp_irowxvars)
		if(allocated(jlp_nxrow2)) deallocate(jlp_nxrow2)
		if(allocated(jlp_nunitsrow)) deallocate(jlp_nunitsrow)
		if(allocated(jlp_xps)) deallocate(jlp_xps)  !nsch,keys,ibaunit
		if(allocated(jlp_xsmin)) deallocate(jlp_xsmin)  !nsch,keys,ibaunit
		if(allocated(jlp_xsmax)) deallocate(jlp_xsmax)  !nsch,keys,ibaunit
		if(allocated(jlp_xmin)) deallocate(jlp_xmin)  !nsch,keys,ibaunit
		if(allocated(jlp_xmax)) deallocate(jlp_xmax)  !nsch,keys,ibaunit
		if(allocated(jlp_rejects))deallocate(jlp_rejects)
		if(allocated(jlp_testxps)) deallocate(jlp_testxps)
		if(allocated(jlp_test)) deallocate(jlp_test)
		if(allocated(jlp_isch)) deallocate(jlp_isch)
		if(allocated(jlp_next)) deallocate(jlp_next)
		if(allocated(jlp_iprev)) deallocate(jlp_iprev)
		if(allocated(jlp_solx)) deallocate(jlp_solx)
		if(allocated(jlp_ldi)) deallocate(jlp_ldi)  !lunit,ld ,vx
	end if !if(j_xpresent)then
	if(sparse)then
		if(allocated(jlp_lavecsp)) deallocate(jlp_lavecsp)
		if(allocated(jlp_acol)) deallocate(jlp_acol)
		if(allocated(jlp_acolapu)) deallocate(jlp_acolapu)
		if(allocated(jlp_icolapu)) deallocate(jlp_icolapu)

	end if !if(sparse)then


	if(nz.gt.0)then
		if(allocated(jlp_lz)) deallocate(jlp_lz)
		if(allocated(jlp_lzi)) deallocate(jlp_lzi)
		if(allocated(jlp_redcost)) deallocate(jlp_redcost)
!zvarl deallocate, jos ei osoita o-vektoriin
	!	if(ivoutresult.eq.j_ivresult) deallocate(jlp_zvarl)
	!	nullify(jlp_zvarl)
	end if !if(nz.gt.0)then
	if(allocated(jlp_vc)) deallocate(jlp_vc)
	if(allocated(jlp_lr)) deallocate(jlp_lr)
	if(allocated(jlp_lri)) deallocate(jlp_lri)
	if(allocated(jlp_lower)) deallocate(jlp_lower)

940 continue
	if(.not.zmatrix) then
		if(allocated(jlp_isx)) deallocate(jlp_isx) !ix,ixcur
		if(allocated(jlp_isxval)) deallocate(jlp_isxval) !ix,ixcur
		if(allocated(jlp_nxrow)) deallocate(jlp_nxrow) !ix,ixcur
		nullify(jlp_nsetr,jlp_nsetd,jlp_isetd,jlp_nvars,jlp_irowvars,jlp_coef)
	endif !if(.not.zmatrix) then
	if(allocated(jlp_rhscur)) deallocate(jlp_rhscur) !ix,ixcur
	if(allocated(jlp_rhsw)) deallocate(jlp_rhsw) !ix,ixcur
	if(allocated(jlp_tole)) deallocate(jlp_tole) !ix,ixcur

950 if(allocated(jlp_lbou)) deallocate(jlp_lbou) !rhs,rhs2,
	if(allocated(jlp_ubou)) deallocate(jlp_ubou) !rhs,rhs2,

990  continue
	if(j_err) then
		jlp_buf='jlp error exit'
	else !if(j_err) then
		jlp_buf='jlp normal exit'
	endif !if(j_err) then
	write(nureport,'(a)')jlp_buf(1:79)
	write(nureport,*)' '
! write(6,*)'hep'
! close(16)
! write(6,*)'ivpivots',ivpivots,'io ',io


	j_v(ivpivots)=ipivot


!	io=io+j_o(iob)%i(io+1)+3
	return

! tfbsub palauttaa x:n alkuun arvot, ei

! subroutine tfbsub(n,a,la,p,b,x,aa,ll,ep,save)
!  solves a system  Bt.x=b

!  Parameter list
!  **************
!   n   number of variables (as for bqpd)
!   a,la   specification of QP problem data (as for bqpd)
!   p    an integer which, if in the range 1:n+m, specifies that the rhs vector
!        b is a unit vector appropriate to the position of p in the current
!        ordering. In this case b is not referenced by tfbsub.
!   b(n+m)  If p=0, this must be set to the r.h.s. vector b. Only the components
!        of b need be set, according to the index number of each component (in
!        the range 1:n for a simple bound and n+1:n+m for a general constraint)
!   x(n)  contains the solution x (in natural ordering)
!   aa(*)  real storage used by the basis matrix code (supply the vector
!       ws(lu1) with ws as in the call of bqpd and lu1 as in common/bqpdc/...)
!   ll(*)  integer storage used by the basis matrix code (supply the vector
!       lws(ll1) with lws as in the call of bqpd and ll1 as in common/bqpdc/...)
!   ep  if p.ne.0 and save is true, ep contains the l_2 length of x on exit
!   save  indicates if tfbsub is to save its copy of the solution for possible
!       future use. We suggest that the user only sets save = .false.
	contains   ! subroutine

	subroutine isfeasible()
 
!write(6,*)'<413>iperke',iperke
!stop 975
		nnfold=nnf
		nnf=0
		jlp_objr2=jlp_zero
		jlp_value=jlp_zero
		jlp_tolecur=jlp_zero
		if(.true.)then !.not.j_feasible.or.iunit.eq.1)then !
			do i=1,lr0 !lr0 = number of basic residuals
		!rhscur =rhs or rhs2
				irow=jlp_lr(i)

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
				if(jlp_ubou(irow)) then
					if(jlp_x(irow)+jlp_tole(irow).lt.jlp_rhscur(irow)-jlp_rhs2(irow))then
						jlp_objr2(irow)=jlp_one
						irowinf=irow
						nnf=nnf+1
						if(jlp_lower(irow))then
							jlp_rhscur(irow)=jlp_rhs2(irow)
							jlp_lower(irow)=.false.
							jlp_x(irow)=jlp_x(irow)+jlp_rhs2(irow)-jlp_rhs(irow)
							if(p)write(n16,*)'irow',irow,'vaihdetaan ylärajaan'
							if((jlp_ix(irow).ne.0).or.jlp_fpresent)jlp_rhsw(irow)=jlp_rhscur(irow)-jlp_xps(irow)
						endif !if(j_lower(irow))then
						jlp_value=jlp_value+jlp_x(irow)
						if(p.and.nnf.le.50)write(n16,*)'infer',irow,jlp_x(irow)
						jlp_tolecur=jlp_tolecur+jlp_tole(irow)
					endif !if(j_x(irow)+j_tole(irow).lt.j_rhscur(irow)-j_rhs2(irow))then
				endif !if(j_ubou(irow)) then
				if(jlp_lbou(irow)) then
					if(jlp_x(irow)-jlp_tole(irow).gt.jlp_rhscur(irow)-jlp_rhs(irow))then
				! illegal surplus, decrease it
						jlp_objr2(irow)=jlp_onen
						nnf=nnf+1
						irowinf=irow  !corrected by JL 9.9.2018 was irowind
						if(.not.jlp_lower(irow))then
							jlp_rhscur(irow)=jlp_rhs(irow)
							jlp_lower(irow)=.true.
							jlp_x(irow)=jlp_x(irow)+jlp_rhs(irow)-jlp_rhs2(irow)
							if(p)write(n16,*)'irow',irow,'vaihdetaan alarajaan'
							if((jlp_ix(irow).ne.0).or.jlp_fpresent) &
							jlp_rhsw(irow)=jlp_rhscur(irow)-jlp_xps(irow)  !jos xps =00 for nonzero ix
					! could be done without condition

						endif !if(.not.j_lower(irow))then
						if(p.and.nnf.le.50)write(n16,*)'infer',irow,jlp_x(irow)
						jlp_value=jlp_value-jlp_x(irow)
						jlp_tolecur=jlp_tolecur+jlp_tole(irow)
					endif !if(j_x(irow)-j_tole(irow).gt.j_rhscur(irow)-j_rhs(irow))then
				endif !if(j_lbou(irow)) then

		! common part of the nonfeasible
		! compute prices of basic variables
		! irow<=nrow -> objr:tä ei käytetä
		! test decreasing
			enddo !do i=1,lr0

			jlp_objfv=jlp_objf

			if(nnf.gt.0)then
				if(p)write(n16,*)'nonfeas rows',nnf,'Pivots=',ipivot,'objf ',jlp_value
				if(jlp_feasible)then
					if(nnf.gt.1)then
						write(6,*)'again ',nnf, ' infeasible rows infeasibility  ',jlp_value,'Pivots=',ipivot !!!!
						if(p.or.p9)write(n16,*)'again infeasible rows ',nnf,'infeasibility  ', &
						jlp_value,'Pivot=',ipivot,&
							'ienter=' ,ienter,' leavec ',leavec, &
							'newc ',newc,'emptmax,j_tmax', jlp_x(max(newc,1)),jlp_tmax
					else !if(nnf.gt.1)then
						write(6,*)'again infeasible row ',irowinf, &
						' infeasibility  ',jlp_value, &
						'Pivots=',ipivot
						if(p.or.p9)write(n16,*)'again infeasible row ',irowinf,'infeasibility  ', &
						jlp_value,'Pivot=',ipivot,&
							'ienter=' ,ienter,' leavec ',leavec, 'newc ', &
							newc,'emptmax,j_tmax', jlp_x(max(newc,1)),jlp_tmax
					endif !if(nnf.gt.1)then
					again=.true.

					nagain=nagain+1
				endif !if(j_feasible)then
				jlp_feasible=.false.
				jlp_objr=>jlp_objr2
				if(p)write(n16,*)'objr',jlp_objr(1:min(jlp_nrow,40)),'+++0'
				jlp_objf=jlp_value
				jlp_xirowold2=jlp_small
				if(nnf.ne.nnfold)then
					jlp_objfv=jlp_small
					justkey=.false.
				endif !if(nnf.ne.nnfold)then
				ix0=0
			else !if(nnf.gt.0)then

				jlp_objr=>jlp_objr0
				if(p)write(n16,*)'***tole(0)',jlp_tole(0)
				jlp_tolecur=jlp_tole(0)

				if(jlp_xpresent)ix0=jlp_ix(0)   !use true objective x-variable
				jlp_objf=jlp_zero
				do j=1,jlp_nrow

					jlp_objf=jlp_objf+jlp_x(jlp_ls(j))*jlp_objr(jlp_ls(j))
					if(p)write(n16,*)'j',jlp_ls(j),jlp_x(jlp_ls(j)), &
					jlp_objr(jlp_ls(j)),' objf',jlp_objf
				enddo !do j=1,j_nrow
				if(ix0.ne.0.or.jlp_isxkyk0)jlp_objf=jlp_objf+jlp_xps(0)
				if(p.and.jlp_xpresent)write(n16,*)'+xp0',jlp_xps(0)
				if(p)write(n16,*)'FEASIBLE,objf,objfv,objf-objfv,ipivot', &
				jlp_objf,jlp_objfv, &
				jlp_objf-jlp_objfv,ipivot, 'was feas?',jlp_feasible

				if(.not.jlp_feasible)then
		!	write(6,'(60x,a)')'*FEASIBLE,round,pivots,obj',kier,ipivot,coefmax*j_objf  !!!!
					if(jlp_xpresent2)write(6,'(74x,a)')'*FEASIBLE'
			!kier,ipivot
		!	write(6,*)'B=basic, NF=NonFeasible'
		!	write(6,*)'round   pivots    objective    active set(%) B-res  B-z   B-sched  B-xkf  NF-rows'
			!             5    3876     2578655.36949    100.00    456      0      56      564
			!	write(6,*)'nnused,nnread',nnused,nnread
					fastusedsame=fastusesame-1  ! to force collecting active set after the next round
					jlp_objfv=jlp_small
					justkey=.false.
					if(again)then
						if(nagain.le.1)then
							againsol=jlp_objf
						else !if(nagain.le.1)then
							if(jlp_objf.gt.againsol+jlp_tolecur)then
								nagain=0
								again=.false.
							else !if(j_objf.gt.againsol+j_tolecur)then
								if(nagain.gt.7)then
									again=.false.
									nagain=0
									jlp_feasible=.false.
									nrecover=nrecover+1
									if(nrecover.ge.20)then
										write(6,*)'*jlp* is mixed up (7), try different tole (e.g.10,100,1000)(or consult J. Lappi)'
										j_err=.true.
										goto900=.true.;return
									endif !if(nrecover.ge.20)then

									write(6,*)'***cycling (2), trying to recover'
									kierv=kier
									iunitv=iunit
									goto1234=.true.;return

								endif !if(nagain.gt.7)then

							endif !if(j_objf.gt.againsol+j_tolecur)then

						endif !if(nagain.le.1)then

					endif !if(again)then
				endif !if(.not.j_feasible)then

				jlp_feasible=.true.
				j_v(ivfeasible)=1.
			endif !if(nnf.gt.0)then


			if(justkey)then   !only key schedule has been changed
				if(p)write(n16,*)'just key-> prev prices'
				newprice=.false.
			else !if(justkey)then

				if(sparse)then
					call tfbsubsp(jlp_nrow,jlp_a,jlp_lavecsp,0, &
					jlp_objr,jlp_vc,wslu1,lwsll1,jlp_apu,.false.) !linux
				else !if(sparse)then
			! vc is 6.42 in JLP manual

					call tfbsub(jlp_nrow,jlp_a,jlp_lavec,0, &
					jlp_objr,jlp_vc,wslu1,lwsll1,jlp_apu,.false.)
				endif !if(sparse)then
			endif !if(justkey)then

			if(p)write(n16,*)'vc:',jlp_vc(1:min(jlp_nrow,50))
			if(p)write(n16,*)'objf,objfv,tolecur:',jlp_objf,jlp_objfv,jlp_tolecur
			if(jlp_objf.le.jlp_objfv-jlp_tolecur)then !1000.d0*j_tolecur)then  ! 100 added Feb. 2011 cnhaged by JL 9.9.2018
				write(6,*)'***getting worse,  trying to recover'  !!!!
				write(6,*)'ipivot,objf,oldobjf: ', &
				ipivot,jlp_objf,jlp_objfv,' tolecur=',jlp_tolecur
				if(p) then
					write(n16,*)'***getting worse, trying to recover'
					write(n16,*)'ipivot,objf,oldobjf: ', &
					ipivot,jlp_objf,jlp_objfv,' tolecur=',jlp_tolecur
				endif !if(p) then
				kierv=kier
				iunitv=iunit
				jlp_feasible=.false.
				nrecover=nrecover+1
				if(nrecover.ge.20)then !10 changed into 20 by J.L 21.2.2019
					write(6,*) &
					'*jlp* is mixed up, try different tole (e.g.10,100,100)(or consult J. Lappi)'
					j_err=.true.
					goto900=.true.;return
				endif !if(nrecover.ge.20)then
				goto1234=.true.;return
			endif !if(j_objf.le.j_objfv-j_tolecur)then

		endif !if(.true.)then

		return
	end subroutine isfeasible !subroutine isfeasible()

	subroutine updatejlp()
		goto900=.false.;goto1234=.false.;goto8888=.false.;goto8889=.false.
! note 8888 and 8889 are in leaving(
!if(pp)write(16,*)'ienter,newc,newa',ienter,newa,newc,j_tmax
		if(p9)then
			write(16,*)'55'
		endif !if(p9)then
		route67=.false.

		if(iunit.eq.iunit55)then
			niter=niter+1
			if(p)write(n16,*)'Niter,iunit',niter,iunit
		else !if(iunit.eq.iunit55)then
			iunit55=iunit
			niter=0
			jlp_valiter=jlp_objf
			nnfiter=nnf

		endif !if(iunit.eq.iunit55)then

!!!! compute values of basic variables *********************************
		if(sparse)then
			call fbsubsp(jlp_nrow,1,jlp_nrow,jlp_a,jlp_lavecsp,0,jlp_rhsw,jlp_x,jlp_ls,wslu1,&
				lwsll1,.false.)   !linux

		else !if(sparse)then

			call fbsub(jlp_nrow,1,jlp_nrow,jlp_a,jlp_lavec,0,jlp_rhsw,jlp_x,jlp_ls,wslu1,&  !!!!
				lwsll1,.false.)    !linux
	!c  solves a system  B.x=b  here b=rhsw  B=a,  x=x
		endif !if(sparse)then
		ntote=ntote+1  !!!!
!new 20.8.2018 JL ******************
		if(jlp_fpresent)then
			do ii_=jlp_mxd+1,jlp_lf0
		!write(n16,*) ii_,j_lf(ii_),j_lunit(j_lf(ii_)) ,j_ixkf(j_lf(ii_)),j_ixkffact(j_lf(ii_)),j_x(j_lf(ii_)+j_nrowz)
				if(jlp_x(jlp_lf(ii_)+jlp_nrowz).le.-0.0001)&
					write(6,*)'**negative amount ,pivot',ipivot, &
					' unit ',iunit, ' ixk ',jlp_ixkf(jlp_lf(ii_)),&
					' ifact ',jlp_ixkffact(jlp_lf(ii_)),' amount ',jlp_x(jlp_lf(ii_)+jlp_nrowz)
		!	write(16,*)'**negative amount ,pivot',ipivot,' unit ',iunit, ' ixk ',j_ixkf(j_lf(ii_)),&
		!	' ifact ',j_ixkffact(j_lf(ii_)),' amount ',j_x(j_lf(ii_)+j_nrowz)
			enddo !do ii_=j_mxd+1,j_lf0


		endif !if(j_fpresent)then
!end new

		if(p)then
			if(newc.ne.0)write(n16,*)'emptmax,tmax,dif',jlp_x(newc),jlp_tmax,jlp_x(newc)-jlp_tmax

			if(jlp_ld0.eq.1) then
				if(jlp_next(0).ne.jlp_ld(1).or.jlp_iprev(0).ne.jlp_ld(1))&
					write(n16,*)'***perk,ld(1),next(0),iprev(0)',jlp_ld(1),jlp_next(0),jlp_iprev(0)
			endif !if(j_ld0.eq.1) then
			if(p2)write(n16,*)'rhsw',jlp_rhsw
			write(n16,'(i2,a,(10i5))')lr0,' basic resid(NO)=',(jlp_lr(jj7),jj7=1,min(lr0,20))
			write(n16,*)'nonbasres',(jlp_lr(jj7),jj7=lr0+1,min(jlp_nrow,50))
			write(n16,'(a,(8f10.1))')'resval(LR)',(jlp_x(jlp_lr(jj7)),jj7=1,min(lr0,50))

			if(nz.gt.0)then
				write(n16,'(i2,a,(10i3))')lz0,'basic z:',(jlp_lz(jj7),jj7=1,lz0)
		! write(n16,*)'xpresent',xpresent
				write(n16,*)'zval',(jlp_x(jlp_nrow+jlp_lz(jj7)),jj7=1,lz0)
			endif !if(nz.gt.0)then
			if(jlp_xpresent)then
				write(n16,'(a,(1x,10i5))')'ls:',(jlp_ls(jj7),jj7=1,jlp_nrow)
				write(n16,'(i4,a,(1x,10i5))') &
				jlp_ld0,'basic d:',(jlp_ld(jj7),jj7=1,min(jlp_ld0,50))
				write(n16,'(a,(1x,10i6))') &
				'units:',(jlp_lunit(jlp_ld(jj7)),jj7=1,min(jlp_ld0,50))
				write(n16,'(a,(1x,10i6))') &
				'scheds:',(jlp_isch(jlp_ld(jj7)),jj7=1,min(jlp_ld0,50))
				write(n16,'(a,(10f7.3))') &
				'w-val',(jlp_x(jlp_nrowz+jlp_ld(jj7)),jj7=1,min(jlp_ld0,50))
			endif !if(j_xpresent)then

			if(jlp_fpresent) then
		! a
				write(n16,*)'a/d-osa'
				do jj=1,jlp_ld0
					write(n16,*)'ld(j) ',jlp_ld(jj)
					write(n16,*)(jlp_a(i_,jlp_ld(jj)),i_=1,jlp_nrow)
				enddo !do jj=1,j_ld0

		! tehdaskannan sarakkeet
				if(jlp_lf0>jlp_mxd) then
					write(n16,*) & 
					'**fact** kannan xkf muuttujat: ii_,lf(ii_),unit,ixkyk,ifact,x(lf(ii_)+nrowz)'
					do ii_=jlp_mxd+1,jlp_lf0
						write(n16,*) ii_,jlp_lf(ii_),jlp_lunit(jlp_lf(ii_)) ,&
						jlp_ixkf(jlp_lf(ii_)),jlp_ixkffact(jlp_lf(ii_)), &
						jlp_x(jlp_lf(ii_)+jlp_nrowz)
					enddo !do ii_=j_mxd+1,j_lf0
				endif !if(j_lf0>j_mxd) then

		!xps
		!write(n16,*)'**fact** xps ',(j_xps(j_),j_=0,j_nrow)
		!call testxpssub(iunit)

		!call printsumxkyk(1)
		!call printxkf()

		! objr0
				write(n16,*)'**fact** objr0/w ', &
				(jlp_objr0(jlp_ld(ii_)+ jlp_nrowz),ii_=1,jlp_ld0)
				call testobjr0()
				write(n16,*)'**fact** objr0/xkf ', &
				(jlp_objr0(jlp_lf(ii_)+jlp_nrowz),ii_=jlp_mxd+1,jlp_lf0)
			endif !if(j_fpresent) then

		endif !if(p)then

		if(jlp_xpresent)then
			lcur=jlp_next(0)
			jlp_wsu=jlp_zero
			lcur0=lcur
	! go through in unit order
			do j=1,jlp_ld0   !nrow
				nex=jlp_next(lcur)
		!!!!! sum of weights in the same unit
				jlp_wsu=jlp_wsu+jlp_x(lcur+jlp_nrowz) !nrowz=nrow+nz  ! basis (first element -1) of D part, when I is included
		! negative value for weight 'noticeably' negative
				if(jlp_x(lcur+jlp_nrowz).lt.jlp_wminwrn)then  ! jl 201606033 j_zeroneg)then    !negative weight, zeroneg=-0.000000001d0
			!***********
					if(p.or.p9)	write(n16,*)'**illegal weight**',&
						jlp_x(lcur+jlp_nrowz),lcur+jlp_nrowz,' pivot=',ipivot

					if(p)then

						write(n16,*)'kukkuuW',jlp_x(lcur+jlp_nrowz),lcur+jlp_nrowz,' pivot=',ipivot
						write(n16,*)'lr0',lr0
						write(n16,*)'lrrest',(jlp_lr(jj),jj=lr0+1,min(jlp_nrow,lr0+50))
						write(n16,*)'acol',(jlp_a(ilr,lcur),ilr=1,min(jlp_nrow,40))
					endif !if(p)then
					if(jlp_x(lcur+jlp_nrowz).lt.jlp_wminerr)then  !wminerr=-0.02d0 !!!!
				! very negative
						write(6,*)'**illegal weight**',&
							jlp_x(lcur+jlp_nrowz),lcur+jlp_nrowz,' pivot=',ipivot,' ienter=',ienter
						write(16,*)'**illegal weight**',&
							jlp_x(lcur+jlp_nrowz),lcur+jlp_nrowz,' pivot=',ipivot,' ienter=',ienter
						jlp_feasible=.false.
						nrecover=nrecover+1
						if(nrecover.ge.10)then
							write(6,*) &
							'*jlp* is mixed up (2), try different tole (e.g.10,100,1000)(or consult J. Lappi)'
							j_err=.true.
							goto900=.true. ;return
						endif !if(nrecover.ge.10)then

						write(6,*)'***trying to recover'
						kierv=kier
						iunitv=iunit
						goto1234=.true.;return
					endif !if(j_x(lcur+j_nrowz).lt.j_wminerr)then


			!***********
					goto 1578 !!!! trying weak rocovery toimii huonosti, ohitetaan nyt 20160603
					write(6,*)'***trying weak recovery' !!!!
					listapu(1)=lcur+jlp_nrowz
					jlp_vcmax=jlp_zero
					cycling=.false.
			!search for which entering residual  w changes most rapidly
					do ilr=lr0+1,jlp_nrow
				!  solves a system  B.x=b
				!      subroutine fbsub(n,jmin,jmax,a,la,q,b,x,ls,aa,ll,save)
				!
				!   ls(*)  an index vector, listing the components of x that are required.
				!       Only the absolute value of the elements of ls are used (this allows
				!       the possibility of using of the contents of the ls parameter of bqpd).
				!       Elements of x in the range abs(ls(j)), j=jmin:jmax are set by fbsub.
				!       These contortions allow bqpd to be independent of the basis matrix code.
				!       call fbsub(n,1,3,a,la,0,b,x,ls,ws(lu1),lws(ll1),.false.)
				! pitää testata voiko koko homman tehdä tällä, a(1,newa ei tarvita)
				!			call fbsub(nrow,1,nrow,a,lavec,newc,a(1,newa),r, &
					!    			 ls,ws(lu1),lws(ll1),.false.)
				!c   q   an integer which, if in the range 1:n+m, specifies that the rhs vector
				!c       b is to be column q of the matrix A of general constraint normals.
				!c       In this case the parameter b is not referenced by fbsub.
						if(sparse)then
							call jlpgetcol(newa)
							call fbsubsp(jlp_nrow,1,1,jlp_a,jlp_lavecsp,jlp_lr(ilr),jlp_acol,r, &
								listapu,wslu1,lwsll1,.false.)   !linux
						else !if(sparse)then
							call fbsub(jlp_nrow,1,1,jlp_a,jlp_lavec,jlp_lr(ilr),jlp_a(1:,newa),r, &
								listapu,wslu1,lwsll1,.false.)   !linux

						endif !if(sparse)then
						if(abs(r(lcur+jlp_nrowz)).gt.jlp_vcmax)then !cycling
							if(jlp_lr(ilr).ne.icolold)then
								jlp_vcmax=abs(r(lcur+jlp_nrowz))
								ilrmax=ilr
								cycling=.false.
							else !if(j_lr(ilr).ne.icolold)then
								cycling=.true.
							endif !if(j_lr(ilr).ne.icolold)then
						endif !if(abs(r(lcur+j_nrowz)).gt.j_vcmax)then
					enddo !do ilr=lr0+1,j_nrow
					if(jlp_vcmax.gt.jlp_tole(jlp_lr(ilrmax)))then

						newc=jlp_lr(ilrmax)
						leavec=lcur+jlp_nrowz
						leave=jlp_lsi(leavec)
						ienter=1

						if(p) write(n16,*)'*nyt mennään,newc,leavec,vcmax',newc,leavec,jlp_vcmax
						if(p.and.jlp_fpresent) write(n16,*)'**fact** KANTAAN >> residual, ienter = 1'

						goto8888=.true.;return
					else !if(j_vcmax.gt.j_tole(j_lr(ilrmax)))then

						write(6,*)'did not remove w, was this due to cycling: ', &
						cycling,' vcmax ',jlp_vcmax,&
							' tole ',jlp_tole(jlp_lr(ilrmax))
					endif !if(j_vcmax.gt.j_tole(j_lr(ilrmax)))then
				endif !if(j_x(lcur+j_nrowz).lt.j_wminwrn)then

		1578	continue ! weak rocoveryn ohitus
				if(jlp_lunit(nex).ne.jlp_lunit(lcur))then
					if(jlp_wsu.gt.jlp_oneps)then
						write(6,*)'**WSUM**',jlp_wsu,jlp_lunit(lcur),' pivot=',ipivot  !!!!
						if(p)write(n16,*)'**WSUM**',jlp_wsu,jlp_lunit(lcur),' pivot=',ipivot
				!		  write(6,*)'prev',iprev78,iprev(lcur),next(0),
						if(jlp_wsu.gt.jlp_wmaxerr)then
							if(sparse)then
								j_err=.true.
								return
							else !if(sparse)then
								if(jlp_wsu.gt.jlp_wmaxwrn)then
									nrecover=nrecover+1
									if(nrecover.ge.20)then
										write(6,*) &
										'*jlp* is mixed up, try different tole (e.g. 10,100,1000) (or consult J. Lappi)'
										j_err=.true.
										goto900=.true.;return

									endif !if(nrecover.ge.20)then
									write(6,*)'***trying to recover'
									if(p)write(n16,*)'***trying to recover'
									kierv=kier
									iunitv=iunit
									jlp_feasible=.false.
									goto1234=.true.;return
								endif !if(j_wsu.gt.j_wmaxwrn)then
							endif !if(sparse)then
						endif !if(j_wsu.gt.j_wmaxerr)then
						listapu(1)=lcur0+jlp_nrowz
						jlp_vcmax=jlp_zero
						do ilr=lr0+1,jlp_nrow
							if(sparse)then
								call jlpgetcol(newa)

								call fbsubsp(jlp_nrow,1,1,jlp_a,jlp_lavecsp,jlp_lr(ilr),jlp_acol,r, &
									listapu,wslu1,lwsll1,.false.)   !linux

							else !if(sparse)then
								call fbsub(jlp_nrow,1,1,jlp_a,jlp_lavec,jlp_lr(ilr),jlp_a(1:,newa),r, &
									listapu,wslu1,lwsll1,.false.)   !linux

							endif !if(sparse)then
							if(abs(r(lcur0+jlp_nrowz)).gt.jlp_vcmax)then
								if(jlp_lr(ilr).ne.icolold)then
									jlp_vcmax=abs(r(lcur0+jlp_nrowz))
									ilrmax=ilr
									cycling=.false.
								else !if(j_lr(ilr).ne.icolold)then
									cycling=.true.
								endif !if(j_lr(ilr).ne.icolold)then
							endif !if(abs(r(lcur0+j_nrowz)).gt.j_vcmax)then

						enddo !do ilr=lr0+1,j_nrow
						if(jlp_vcmax.gt.jlp_tole(jlp_lr(ilrmax)))then
							newc=jlp_lr(ilrmax)
							leavec=lcur0+jlp_nrowz
							leave=jlp_lsi(leavec)
							leavk=lcur0
							ienter=1

							if(p.or.p9) write(n16,*) &
							'*nyt mennään,newc,leavec,vcmax',newc,leavec,jlp_vcmax,ipivot
							if(p.and.jlp_fpresent) write(n16,*)'**fact** KANTAAN >> residual, ienter = 1'

							goto8889=.true.;return
						else !if(j_vcmax.gt.j_tole(j_lr(ilrmax)))then
							write(6,*)'did not remove w, was this due to cycling: ',cycling
						endif !if(j_vcmax.gt.j_tole(j_lr(ilrmax)))then

					endif !if(j_wsu.gt.j_oneps)then

					jlp_wsu=jlp_zero

					lcur0=jlp_next(lcur)

				endif !if(j_lunit(nex).ne.j_lunit(lcur))then

				lcur=nex
			enddo !do j=1,j_ld0

		endif !if(j_xpresent)then

!!!!! check feasiblity , i.e. if residuals get legal values
! note as opposite in old JLp, we concider here if residual should be maximized or minimized
! as in old jlp the row was maximized or minimize
! it may happen that an feasible solution becomes again infeasible /due to rounding errors)
! thus infeasibility must be checked repeatedly
! now this is done after each pivot operation, earlier once in each round
! over units


		call isfeasible()
		if(goto900)return
		if(goto1234)return

! all rows feasible

! lbres0=0 opr all feasible
! B is current basis x current values of basic variables b is RHS
! ie. B*x=b
! let the coefficients of the current basic variables in the objective function be
! objr, thus the current value of the objective is
! objr'x
! if new column say d enters with weight t, let the new value of basic
! variables be xn      then
! B*xn+t*d=b
! xn=inv(B)*b-t*inv(B)*d=x-t*inv(B)*d
! let the coefficients of the current basic variables in the objective function be
! objr      and let the coefficient of the entering column be d0
! then the value of the new objective function is
! objr'xn+t*d0=objr'x-t*objr'*inv(B)*d+t*d0
! vc= objr'*inv(B) is the price vector for rows
! note vc is the solution of B'*vc=objr
! new value is: old value+ t*(d0-vc'*d)>0
! new objective is larger if t>0 and d0-vc'*d >0 or t<0 and d0-vc'*d <0
! entering variable can be negative for residaul and later for z variables at upper bo
! for x-varaibles, if the same x-variable is on several rows
! then  thes prices could be combined

!		maxim=.true.   ! for min problem signs are reversed

! **    entering variable

! ienter=1 residual eneters
! ienter=2   z enters
! ienter=3 schedules enters
!*********************************************************

!jatketaanko seuraavasta tehdasmjasta tarkastelu  !!!!
		if (jlp_fpresent) then
			jlp_nextxkf = ((ienter==4).and.(ixkykenter<j_o(jlp_ivxkyk)%i(1))) !<A>  <a>
	! pakotetaan käymään läpi yksikön tehdasmjat sen jälkeen kun kantaan on tullut vaihtoehto tai avainvaihtoehto vaihtunut
			jlp_nextxkf = jlp_nextxkf.or.(ienter==3)
		endif !if (j_fpresent) then

	end subroutine updatejlp !subroutine updatejlp()

	subroutine repo(nureport)
!write(6,*)'<67>j_nrow',j_nrow
		if(jlp_maxo) then
			j_v(ivobjective)=jlp_objf
		else !if(j_maxo) then
			j_v(ivobjective)=-jlp_objf
		endif !if(j_maxo) then
		if(ivoutresult.ne.j_ivresult) then
		call j_getobject(ivoutresult,'%objective',j_ipreal,ivobjective2)
		j_v(ivobjective2)=j_v(ivobjective)
		write(6,*)'OBJECTIVE ',jlp_objf,j_v(ivobjective2)
		endif
901	continue

		jlp_issolution=.true.
		if(jlp_nrow.gt.0)then  !there are constraints
			do i=jlp_nrow+1,ncol+jlp_nrow;jlp_x(jlp_ls(i))=0.;end do
		endif !if(j_nrow.gt.0)then
		if(.not.jlp_maxo)then  ! poistettu bugia metsästettäessä
			jlp_vc=-jlp_vc                        !;objf=-objf done earlier
			jlp_vx=-jlp_vx
		end if !if(.not.j_maxo)then

877 continue !nrow=0

		if(nz.gt.0) then
			jlp_redcost=0.  !vois allokoida täs
			do i=lz0+1,nz

				newa=jlp_lz(i)
				if(jlp_feasible)then
					val_=jlp_objr0(newa+jlp_nrow)  ! in objr all cols are counted
				else !if(j_feasible)then
					val_=0.
				endif !if(j_feasible)then
				do  j=1,jlp_nrow
					val_=val_-jlp_vc(j)*jlp_a(j,newa)
				enddo !do  j=1,j_nrow
				jlp_redcost(newa)=abs(val_)
			enddo !do i=lz0+1,nz
		endif !if(nz.gt.0) then
		if(j_err)return

		if(ivoutresult.ne.j_ivresult) then
			if (nz>0) then
	!write(6,*)'<nz',nz
				call j_defmatrix(ivoutresult,'%zvalues',1,nz,j_matreg,ivout)
				j_o(ivout)%d(jlp_lz(1:lz0))=jlp_x(jlp_nrow+jlp_lz(1:lz0))

				call j_defmatrix(ivoutresult,'%redcost',1,nz,j_matreg,ivout)

				j_o(ivout)%d(1:nz)=jlp_redcost(1:nz)
			endif !if (nz>0) then

			if(jlp_nrow>0) then
	!write(6,*)'<nrow',j_nrow
				call j_defmatrix(ivoutresult,'%rows',jlp_nrow,1,j_matreg,ivout)
				j_o(ivout)%d(1:jlp_nrow)=jlp_rhscur(1:jlp_nrow)-jlp_x(1:jlp_nrow)

				call j_defmatrix(ivoutresult,'%shprice',jlp_nrow,1,j_matreg,ivout)
				j_o(ivout)%d(1:jlp_nrow)=jlp_vc(1:jlp_nrow)
			endif !if(j_nrow>0) then
		endif !if(ivoutresult.ne.j_ivresult) then
!write(6,*)'<47zmatrix',zmatrix
		if (zmatrix)then
			write(6,*)' '
			write(6,*)'***because there was zmatrix->, results are given in matrices***'
			write(6,*)' '
			goto 8000
		endif !if (zmatrix)then

		irow0=0
		irow=0
		ido=0 !counter for domain statements
!write(6,*)'<467iprint',iprint
		if(iprint.lt.1)goto 8000
!title


!		call j_printtitle(nureport, jlp_ivprob)
! write(6,*)'<567nset',nset
		do i=1,nset
			domloop: do j=1,jlp_nsetd(i)
!write(6,*)'<5553>',j_nsetd(i)
				ido=ido+1
				if(jlp_xpresent.and.jlp_ndom.gt.0)then
					jlp_buf='DOMAIN:'
					idom=jlp_isetd(ido)  !domain number
					call j_getline(jlp_ivdomain,idom,jlp_buf(8:),le)

					jlp_buf(74:78)='units'
					jlp_buf(68:72)=j_chi5(jlp_domainunits(idom),0)
					write(nureport,*)' ',('_',kk=1,78)
					write(nureport,'(a)')jlp_buf(1:79)
				end if !if(j_xpresent.and.j_ndom.gt.0)then
				write(nureport,*)' ',('_',kk=1,78)
				write(jlp_buf,66061)
66061 format('row',t38,'value',t50,'shadow',t61,  'lower',t70,'upper')
				write(nureport,'(a)')jlp_buf(1:79)
				write(jlp_buf,6606)
6606 format(t50,'price', t61,'bound',t70, 'bound')
				if(intapp)jlp_buf(35:44)='int. app.'
				write(nureport,'(a)')jlp_buf(1:79)
				write(nureport,*)' ',('_',kk=1,78)

				do k=1,jlp_nsetr(i)
 !write(6,*)'<458 k,nsetr(i) ',k,j_nsetr(i)
					if((k==1).and.(j>1)) irow0 = irow0 - jlp_nsetr(i)
					irow0 = irow0+1

					call j_getline(jlp_ivrow,irow0,jlp_buf(6:),le);jlp_buf(6+le:)=jlp_dots
	!write(16,*)le,j_buf(1:5+le)
					if(irow0.ne.irowobj)then
						jlp_apubuf=j_chi5(irow+1,0); jlp_buf(1:3)=jlp_apubuf(3:5);jlp_buf(4:5)=') '
					else if(j.eq.1) then !if(irow0.ne.irowobj)then
						if(j_v(ivfeasible)>0)then
							if(jlp_maxo)then
								jlp_buf(1:5)=' max'
							else !if(j_maxo)then
								jlp_buf(1:5)=' min'
							end if !if(j_maxo)then
						else !if(j_v(ivfeasible)>0)then
							jlp_buf(1:5)=' '
							jlp_buf(6:33)='Infeasible, temporary object'
			!	le=
						endif !if(j_v(ivfeasible)>0)then

					endif !if(irow0.ne.irowobj)then
	!lyhennetään ylipitkät rivit etteivät sotkeennu
					le5=le+5
					iplus0=0
	!	istart00_=6
	! if((5+le)>=34) then
	7531		if((le5-iplus0)>=34) then
	!	j_buf(34:35)=j_dots !chnaged by JL 10.9.2018
						if(le5.gt.iplus0+80)then
							do jj=iplus0+81,iplus0+1,-1
								if(jlp_buf(jj:jj).eq.'+'.or.jlp_buf(jj:jj).eq.'-')then
									iplus=jj-1
									exit
								endif !if(j_buf(j_:j_).eq.'+'.or.j_buf(j_:j_).eq.'-')then
							enddo !do jj=iplus0+81,iplus0+1,-1
						else !if(le5.gt.iplus0+80)then
							iplus=le5
						endif !if(le5.gt.iplus0+80)then
						write(nureport,'(a)')jlp_buf(iplus0+1:iplus)
						iplus0=iplus

						goto 7531
					elseif(iplus0.gt.0)then !if((le5-iplus0)>=34) then
						jlp_buf(1:le5-iplus0)=jlp_buf(iplus0+1:le5)
						jlp_buf(le5-iplus0+1:34)=jlp_dots
					endif !if((le5-iplus0)>=34) then


					if(irow0.ne.irowobj)then
	!constraint row
						irow=irow+1
	!		j_apubuf=j_chi5(irow,0); j_buf(1:3)=j_apubuf(3:5);j_buf(4:5)=') '

						jlp_value=jlp_rhscur(irow)-jlp_x(irow)
						jlp_buf(35:35)=' '
						jlp_buf(36:)=j_chr10(jlp_value)
						jlp_buf(47:57)=j_chr10(jlp_vc(irow))
!	write(6,*)'shp',j_vc(irow)
						if(jlp_vc(irow).ne.0.)then
							if(jlp_maxo.eqv.jlp_vc(irow).gt.0.)jlp_buf(78:78)='U'
							if(jlp_maxo.eqv.jlp_vc(irow).lt.0.)jlp_buf(78:78)='L'
						end if !if(j_vc(irow).ne.0.)then

						if(jlp_rhs(irow).eq.jlp_rhs2(irow))then
							jlp_buf(65:72)= j_chr10(dble(jlp_rhs(irow)))
						else !if(jlp_rhs(irow).eq.jlp_rhs2(irow))then
							if(jlp_lbou(irow))jlp_buf(60:67)= j_chr10(dble(jlp_rhs(irow)))
							if(jlp_ubou(irow))jlp_buf(69:76)= j_chr10(dble(jlp_rhs2(irow)))

						end if !if(jlp_rhs(irow).eq.jlp_rhs2(irow))then

					else if(j.eq.1) then !if(irow0.ne.irowobj)then
	! for maximization rhs1 =huge  rhs2=0
	! for minimization  rhs2=-huge
						jlp_buf(35:35)=' '
						jlp_buf(36:)=j_chr10(dble(jlp_objf))

	! if(j_v(ivfeasible)>0)then
	! if(j_maxo)then
	! j_buf(1:5)=' max'
	! else !if(j_maxo)then
	! j_buf(1:5)=' min'
	! end if !if(j_maxo)then
	! else !if(j_v(ivfeasible)>0)then
	! j_buf(1:5)=' '
	! j_buf(6:33)='Infeasible, temporary object'
	! endif !if(j_v(ivfeasible)>0)then
					end if !if(irow0.ne.irowobj)then
					write(nureport,'(a)')jlp_buf(1:79)
					if(intapp)then
						jlp_buf=' '

						jlp_buf(36:)=j_chr10(jlp_solx(irow))
						write(nureport,'(a)')jlp_buf(1:79)
					endif !if(intapp)then
				end do !do k=1,j_nsetr(i)

				if(jlp_xpresent.and.iprint.ge.1)then
					write(nureport,*)' ',('_',kk=1,78)
					jlp_buf='     x-variable'

					jlp_buf(38:50)='value'; jlp_buf(47:58)='shadow price'
					jlp_buf(62:79)='integer appr.-opt.'

					write(nureport,'(a)')jlp_buf(1:79)
					write(nureport,*)' ',('_',kk=1,78)
					if(jlp_ndom.le.0)idom=1
					do jx=1,jlp_nxvartot+ncvar+noutsubtrans
						jlp_buf =' '
						shp=0.
						indi=0
						item=1
						do iro=0,jlp_nrow

							if((jlp_ndom.le.0.or.idom.eq.jlp_irowdomain(iro)).and.jlp_ix(iro).ne.0)then

								item=jlp_ix(iro)
								iba=jlp_ibatemp(item)
								do ii=1,jlp_nxrow2(item)

									if(jx.le.jlp_nxvartot)then
										jlp_yes=jlp_irowxvars(iba+ii).eq.j_o(jlp_ivkeepx)%i2(jx)
									elseif(jx.le.jlp_nxvartot+ncvar) then !if(jx.le.j_nxvartot)then
										jlp_yes=jlp_irowxvars(iba+ii).eq.jlp_cvarl(jx-jlp_nxvartot)
									else !if(jx.le.j_nxvartot)then
										jlp_yes=jlp_irowxvars(iba+ii).eq. &
										j_o(ivoutsubtrans)%i2(jx-jlp_nxvartot-ncvar)
									endif !if(jx.le.j_nxvartot)then
									if(jlp_yes)then   !häär
										if(iro.eq.0)then
											shp=jlp_coefx(iba+ii)

											indi=1
										else !if(iro.eq.0)then
											shp=shp-jlp_coefx(iba+ii)*jlp_vc(iro);indi=1
										endif ;exit !if(iro.eq.0)then
									endif !if(j_yes)then
								enddo !do ii=1,j_nxrow2(item)

							endif !if((j_ndom.le.0.or.idom.eq.j_irowdomain(iro)).and.j_ix(iro).ne.0)then

						enddo !do iro=0,j_nrow
						if(jx.le.jlp_nxvartot)then
							le=j_lename(j_o(jlp_ivkeepx)%i2(jx))
							jlp_buf(6:)=j_vname(j_o(jlp_ivkeepx)%i2(jx))
			!tulos muuttujiin
							if(jlp_ndom.le.1) j_v(j_o(jlp_ivkeepx)%i2(jx))=jlp_sumx(jx)

						elseif(jx.le.jlp_nxvartot+ncvar) then !if(jx.le.j_nxvartot)then
							le=j_lename(jlp_cvarl(jx-jlp_nxvartot))
							jlp_buf(6:)=j_vname(jlp_cvarl(jx-jlp_nxvartot))
							jlp_buf(1:1)='C'

							if(jlp_ndom.le.1) j_v(jlp_cvarl(jx-jlp_nxvartot))=jlp_sumx(jx)
						else !if(jx.le.j_nxvartot)then
							le=j_lename(j_o(ivoutsubtrans)%i2(jx-jlp_nxvartot-ncvar))
							jlp_buf(6:) = j_vname(j_o(ivoutsubtrans)%i2(jx-jlp_nxvartot-ncvar))
			!tulos muuttujiin
							if(jlp_ndom.le.1) j_v(j_o(ivoutsubtrans)%i2(jx-jlp_nxvartot-ncvar))= &
							jlp_sumx(jx)
						endif !if(jx.le.j_nxvartot)then

						if(le.lt.26)jlp_buf(6+le:34)=jlp_dots
						jlp_buf(36:46)=j_chr10(dble(jlp_sumx((idom-1)*jlp_nsumx+jx)))
						if(indi.ne.0)jlp_buf(47:56)=j_chr10(dble(shp))
						jlp_shpx((idom-1)*jlp_nsumx+jx)=shp
						jlp_buf(67:76)=j_chr10(dble(jlp_sumxi((idom-1)*jlp_nsumx+jx)-jlp_sumx((idom-1)* &
						jlp_nsumx+jx)))

						write(nureport,'(a)')jlp_buf(1:79)

					end do !do jx=1,j_nxvartot+ncvar+noutsubtrans
				end if !if(j_xpresent.and.iprint.ge.1)then
			end do domloop !domloop: do j=1,j_nsetd(i)
		end do !do i=1,nset
!	jlp_nxvartot=j_o(jlp_ivkeepx)%i(1)
!jlp_nsumx=jlp_nxvartot+ncvar+noutsubtrans
!xvars%, xsum%, xprice%,xvarsproblem%
		if(jlp_xpresent.and.ivoutresult.ne.j_ivresult) then
		!	call j_deflistobject(ivoutresult,'%xvars',ivxvarstot,nres=jlp_nsumx)
			call j_deflistobject(ivoutresult,'%xvars',ivxvarstot,list0=jlp_nxvartot, &
			list=j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot),nres=ncvar+noutsubtrans)

			!j_o(ivxvarstot)%i(1:jlp_nxvartot)=j_o(jlp_ivkeepx)%i(1:jlp_nxvartot)
			if (ncvar > 0) j_o(ivxvarstot)%i2(jlp_nxvartot+1:jlp_nxvartot+ncvar) =  &
			jlp_cvarl(1:ncvar)
			if (noutsubtrans > 0) &
				j_o(ivxvarstot)%i2(jlp_nxvartot+ncvar+1:jlp_nxvartot+ncvar+noutsubtrans) =  &
				j_o(ivoutsubtrans)%i2(1:noutsubtrans)
			j_o(ivxvarstot)%i(1) = 	jlp_nsumx
			call j_deflistobject(ivoutresult,'%xvarsproblem',ivout,list0=nxvar,list=jlp_xvarl)

			! j_o(ivout)%i(1:nxvar)=jlp_xvarl
			! j_o(ivout)%i(1)=nxvar
			ndom_=max(jlp_ndom,1)
!write(6,*)'j_nsumx',j_nsumx
			call j_defmatrix(ivoutresult,'%xsum',ndom_,jlp_nsumx,j_matreg,ivout)

			j_o(ivout)%d(1:ndom_*jlp_nsumx)=jlp_sumx(1:ndom_*jlp_nsumx)

			call j_defmatrix(ivoutresult,'%xsumint',ndom_,jlp_nsumx,j_matreg,ivout)
			j_o(ivout)%d(1:ndom_*jlp_nsumx)=jlp_sumxi(1:ndom_*jlp_nsumx)
			call j_defmatrix(ivoutresult,'%xprice',ndom_,jlp_nsumx,j_matreg,ivout)

			j_o(ivout)%d(1:ndom_*jlp_nsumx)=jlp_shpx(1:ndom_*jlp_nsumx)
			if(jlp_nrow.gt.0)then
				call j_defmatrix(ivoutresult,'%domains',jlp_nrow,1,j_matreg,ivout)

				j_o(ivout)%d = jlp_irowdomain(1:jlp_nrow)
				
			endif !if(j_nrow.gt.0)then
		endif
		if(ivoutresult.ne.j_ivresult)then
			!call j_defmatrix(ivoutresult,'%problemrows',jlp_nrow,1,j_matreg,ivout)

			!	j_o(ivout)%d = jlp_irowrow(1:jlp_nrow)
			if(jlp_nrow.gt.0)then
				call j_defmatrix(ivoutresult,'%rhs',jlp_nrow,1,j_matreg,ivout)

				j_o(ivout)%d = jlp_rhs
				call j_defmatrix(ivoutresult,'%rhs2',jlp_nrow,1,j_matreg,ivout2_)

				j_o(ivout2_)%d = jlp_rhs2
				do i_=1,jlp_nrow
					if(jlp_rhs(i_).eq.-huge(1.d0))j_o(ivout)%d(i_) =-1.7e37
					if(jlp_rhs2(i_).eq.huge(1.d0))j_o(ivout2_)%d(i_) =1.7e37
				enddo !do i_=1,j_nrow
				call j_defmatrix(ivoutresult,'%shprice',jlp_nrow,1,j_matreg,ivout2_)
				j_o(ivout2_)%d = jlp_vc(1:jlp_nrow)
			endif
			
		endif !if(j_xpresent.and.ivoutresult.ne.j_ivresult) then

		if(nz.gt.0.and.iprint.ge.1)then
			write(nureport,*)' '
			write(nureport,*)'    ',('_',kk=1,46)
			jlp_buf='z-variable'
			jlp_buf(25:30)='value'
			jlp_buf(36:48)='reduced cost'
			write(nureport,'(a)')'     '//jlp_buf(1:79)
			write(nureport,*)'    ',('_',kk=1,46)
			DO 66778 I=1,NZ
				jlp_buf=j_vname(jlp_zvarl(i))
				le=j_lename(jlp_zvarl(i))
				if(le.lt.20)jlp_buf(le+2:21)=jlp_dots
				jlp_buf(23:33)=j_chr10(dble(jlp_x(jlp_nrow+i)))
				j_v(jlp_zvarl(i))=jlp_x(jlp_nrow+i)
				jlp_buf(36:46)=j_chr10(jlp_redcost(i))
				write(nureport,'(a)')'     '//jlp_buf(1:46)
66778 continue !DO 66778 I=1,NZ
		endif !if(nz.gt.0.and.iprint.ge.1)then

8000 continue  !end printing

		write(nureport,*)('_',kk=1,79)

		if(j_v(ivunbounded)>0) then
			jlp_buf='Unbounded problem'
			write(nureport,'(a)')jlp_buf(1:79)
		endif !if(j_v(ivunbounded)>0) then

		write(nureport,"('Pivots: ',I10,' refactorizations ',i5,' rounds ',i5)")&
			ipivot,max(0,krefac-1),kier

		if(j_v(ivfeasible)>0) then
			jlp_buf='Value of the objective function:  '
		else !if(j_v(ivfeasible)>0) then
			jlp_buf='Value of the temporary objective: '
		endif !if(j_v(ivfeasible)>0) then
		jlp_buf(35:)=j_chr10(dble(jlp_objf))
		write(nureport,'(a)')jlp_buf(1:79)

		if(j_v(ivfeasible)>0) then
			jlp_buf='Solution is feasible'
		else !if(j_v(ivfeasible)>0) then
			jlp_buf='Solution is infeasible'
		endif !if(j_v(ivfeasible)>0) then
		write(nureport,'(a)')jlp_buf(1:79)
!write(6,*)'OBJECTIVE&&&&& ',jlp_objf,j_v(ivobjective2)
call j_getname(ivobjective2)
!write(6,*)'%%%',j_oname(1:j_loname)
		if(j_v(ivoptimal)>0) then
			if(kier.ge.mxiter)then
				jlp_buf='Solution may not be optimal, maximum number of iterations reached'
! elseif(isslow)then !if(kier.ge.mxiter)then
! j_buf='Solution is close to optimal (slow improvement)'
			else !if(kier.ge.mxiter)then
				jlp_buf='Solution is optimal'
			endif !if(kier.ge.mxiter)then
		else !if(j_v(ivoptimal)>0) then
			jlp_buf='Solution is not optimal'
		endif !if(j_v(ivoptimal)>0) then
		write(nureport,'(a)')jlp_buf(1:79)
		if(nureport.ne.6)write(6,*)'** report-> file remains open'
		return
!if(nureport.ne.6)call j_closeunit(nureport)
	end subroutine repo !subroutine repo(nureport)


	subroutine renter()
!goto35=.false.;
		goto36=.false.
		jlp_post=.true.
		if(p)write(n16,*)'start entering'  !!!! <B>
		if(niter.ge.10)then  !!!!
	!after addrees 55:niter=0

			if(nnf.eq.nnfiter.and.jlp_objf-jlp_valiter.le.jlp_tolecur)then
				if(p.or.p9)write(n16,*)'Niter,iunit,ipivot,objf,old', &
				Niter,iunit,ipivot,jlp_objf,jlp_valiter
				goto36=.true.;return   !!!! bypassresidual enters and z enters beacause there is no improvement
			else !if(nnf.eq.nnfiter.and.j_objf-j_valiter.le.j_tolecur)then
				if(p)write(n16,*)'*niter with improvement,objf,old',jlp_objf,jlp_valiter
				niter=0
				jlp_valiter=jlp_objf
			endif !if(nnf.eq.nnfiter.and.j_objf-j_valiter.le.j_tolecur)then
		endif !if(niter.ge.10)then

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
		jlp_vcmax=jlp_tolep*jlp_epsj

!cycle
		if(ienterv.eq.1.and.nsame.gt.5.and.jlp_objf.lt.oldsol+jlp_tolecur)then
			if(iprint.gt.1)write(6,*)'cycling, Unit',iunit,'Pivot ',ipivot, 'Objective ',jlp_objf
			return    ! we check if z can enter z
		endif !if(ienterv.eq.1.and.nsame.gt.5.and.j_objf.lt.oldsol+j_tolecur)then

		do ilr=lr0+1,jlp_nrow   !!!!! nonbasic residuals <B1>
			ico=jlp_lr(ilr)  !lri(lr(ilr))  ! row=col for residual varaibles, note lr
			if(ico.eq.ilres)then
				if(jlp_objf.lt.objilres+jlp_tolecur)cycle
				ilres=0
			endif !if(ico.eq.ilres)then
			if(jlp_rhs2(ico).gt.jlp_rhs(ico))then
		! lowerilla ei merkitystä jos rhs=rhs2,
		! voidaan tehdä lista jossa mukana vain
		! mahdolliset rajoitukset
		! could increase vc<0
				if(jlp_vc(ico).gt.jlp_vcmax.and.jlp_lower(ico).and.ico.ne.icolold)then
			! make rhs2 active
					jlp_vcmax=jlp_vc(ico)
					ienter=1
					ilrmax=ilr
					newc=ico      !laita suoraan
					if(p) write(n16,*)'negat. resid could enter', ico,'ste',e(ico),sqrt(e(ico))  !!!!
					if(p.and.jlp_fpresent) write(n16,*)'**fact** KANTAAN >> residual, ienter = 1'
					if(jlp_ubou(ico).and.jlp_lbou(ico)) then
						jlp_tmax=jlp_rhs2(ico)-jlp_rhs(ico) ;rcur=jlp_tmax
					else !if(j_ubou(ico).and.j_lbou(ico)) then
						jlp_tmax=huge(1.)  ;rcur=jlp_tmax
					endif !if(j_ubou(ico).and.j_lbou(ico)) then
					jlp_post=.false.  !post=positive residual could enter
			! xma ei käy koska voidaan ???
			! tmin=xmi(newc) !min value for the entering
			!					goto 100
			! ajatus kesken
			! lower aina yhteensopiva sen kanssa onko
			! rhs vai rhs2 voimassa
			! lower/rhs päivitetään vain silloin kun
			! resid poistuu kannasta

			! vc
				else if(jlp_vc(ico).lt.-jlp_vcmax.and..not.jlp_lower(ico).and.ico.ne.icolold)then !if(j_vc(ico).gt.j_vcmax.and.j_lower(ico).and.ico.ne.icolold)then
			! come from rhs2 downwards
					ienter=1  !pitäis olla eri
					ilrmax=ilr
					newc=ico !laita suoraan
					if(jlp_ubou(ico).and.jlp_lbou(ico)) then
						jlp_tmax=jlp_rhs2(ico)-jlp_rhs(ico);rcur=jlp_tmax
					else !if(j_ubou(ico).and.j_lbou(ico)) then
						jlp_tmax=huge(1.) ;rcur=jlp_tmax
					endif !if(j_ubou(ico).and.j_lbou(ico)) then
					jlp_vcmax=-jlp_vc(ico)
					jlp_post=.true.
					if(p) write(n16,*)'posit resid could enter',ico  !!!!
					if(p.and.jlp_fpresent) write(n16,*)'**fact** KANTAAN >> residual, ienter = 1'
				end if !if(j_vc(ico).gt.j_vcmax.and.j_lower(ico).and.ico.ne.icolold)then
			end if !if(j_rhs2(ico).gt.j_rhs(ico))then
		end do !do ilr=lr0+1,j_nrow


	end subroutine renter !subroutine renter()

	subroutine zenter()
		jlp_post=.true.
		do ilz=lz0+1,nz
			newa0=jlp_lz(ilz)	! col number in A
			newc0=newa0+jlp_nrow  ! col number taking into account the I part tarvitaanko newd
	! ol ico pit olla newa
			if(jlp_feasible)then
				val_=jlp_objr0(newc0)  ! in objr all cols are counted
			else !if(j_feasible)then
				val_=0.
			endif !if(j_feasible)then
	!could take maximum
	! place to refer zcoef
			if(sparse)then
				iel=0
				do jj=jlp_lavecsp(jlp_lavecsp(0)+newa0),last(newa0)
					iel=iel+1
					val_=val_-jlp_vc(jlp_lavecsp(jj))*jlp_a(iel,newa0)
				enddo !do jj=j_lavecsp(j_lavecsp(0)+newa0),last(newa0)
			else !if(sparse)then
				do  j=1,jlp_nrow
					val_=val_-jlp_vc(j)*jlp_a(j,newa0)
				enddo !do  j=1,j_nrow
			endif !if(sparse)then
	! ei ny otettu huomioon mahdollisuutta, että z voisi olla nyt ylärajalla
	! ja jos se tulee kantaan negatiivisena, niin tavoite voiis kasvaa
	!		if(val_.gt.epsj)then
			if(val_.gt.jlp_vcmax)then
				newa=newa0
				newc=newc0
				if(p)write(n16,*)'z could enter',newa,'tmax',huge(1.),'newc',newc
				if(p.and.jlp_fpresent) write(n16,*)'**fact** KANTAAN >> z, ienter = 2'
				ienter=2
				jlp_tmax=huge(jlp_one) ;rcur=jlp_tmax
				ilzmax=ilz  !	do ilz=lz0+1,nz
			endif !if(val_.gt.j_vcmax)then
		enddo !do ilz=lz0+1,nz

	end subroutine zenter !subroutine zenter()

	subroutine senter()
		goto400=.false.  !subroutine
		ienter=0
		jlp_post=.true.
		if(jlp_xpresent)then  !newprice   !B31>

			if(ix0.ne.0)then !ix(0).ne.0.and.feasible
				jlp_vx(0)=1.
			else !if(ix0.ne.0)then
				jlp_vx(0)=0.
			endif !if(ix0.ne.0)then
			do i=1,jlp_nrow
				if(jlp_ix(i).ne.0)then
					jlp_vx(i)=-jlp_vc(i)   ! formula 6.41, voitasiko kenties tästä merkinkäännöstä
			! luopua ja pelata suoraan vc:n avulla
				endif !if(j_ix(i).ne.0)then
			enddo !do i=1,j_nrow
			if(p)  write(n16,'(a,(1x,5g19.7))')'vx',(jlp_vx(i),i=0,min(jlp_nrow,50))
		endif !if(j_xpresent)then

		jlp_valueopt=jlp_small  !-1.7e37
		secondb=jlp_small

		iopt=0        ! the optimal schedule

! start computing shadow pricees of schedules <B333>

		call jlpcurix(iunit)
		if (jlp_fpresent) then
			call jlpfcurix(iunit)
			call jlpfcurixy(iunit)
			call jlpfirow2curix(iunit)
		endif !if (j_fpresent) then
!determines for each row if the unit iunit belonggs to the domain of the row
!matrix  domainbits(number,mxunit) contains bits for telling if unit belongs to certain domain
! returns nrowp
! ixcurrows  the x-variable of the
		jlp_vxpack=0.
		jlp_ixpack=0
		do jj=1,jlp_nrowp
			j=jlp_ixcurrows(jj)
			if(jlp_vx(j).ne.0.)then
				jlp_vxpack(jlp_ix(j))=jlp_vxpack(jlp_ix(j))+jlp_vx(j)
				jlp_ixpack(jlp_ix(j))=jlp_ix(j)   ! result (1*ind1,2*ind2,....)
			endif !if(j_vx(j).ne.0.)then
		enddo !do jj=1,j_nrowp
		nxbas=count(jlp_ixpack.ne.0)
! Jos tehtaita, ei ohiteta
		if(nxbas.le.0.and..not.jlp_fpresent)then
			goto400=.true.;return   !!!! next unit subroutine
		endif !if(nxbas.le.0.and..not.j_fpresent)then
		if(p)then
			write(n16,*)'nonzero prices:', nxbas, 'ix0,feasible,vx(0)',ix0,jlp_feasible,jlp_vx(0)
			write(n16,*)'ixpack',jlp_ixpack,jlp_ixpack.ne.0
			write(n16,*)'vxpack',jlp_vxpack
		endif !if(p)then
		icout=0
		do icin=1,jlp_ntemp0                !remove zero-elements
			if(jlp_ixpack(icin).ne.0)then
				icout=icout+1
				jlp_vxpack2(icout)=jlp_vxpack(icin)
				jlp_ixpack2(icout)=jlp_ixpack(icin)
			endif !if(j_ixpack(icin).ne.0)then
		enddo !do icin=1,j_ntemp0

		if(istree)then !!!!

			ixtree_=0
			ulloop:	do jj_=1,jlp_ntemp0
				do jj2_=1,icout
					if(jlp_ixpack2(jj2_).eq.iperiodperm(jj_))then
						ixtree_=ixtree_+1
						ixpack3(ixtree_)=jlp_ixpack2(jj2_)
						vxpack3(ixtree_)=jlp_vxpack2(jj2_)

						idiff3(jj_)=ixtree_
						cycle ulloop
					endif !if(j_ixpack2(jj2_).eq.iperiodperm(jj_))then
				enddo !do jj2_=1,icout
				idiff3(jj_)=ixtree_+1
			enddo ulloop !ulloop:	do jj_=1,j_ntemp0

		endif !if(istree)then

!!!!tehtaat/ laskennan valmistelu  !<C1>

!!!!katsotaan tehtäväriveillä olevat yksikön avaintehtaat
		if (jlp_fpresent) then
			nrowxkfkey = 0
			nrowykfkey = 0

!xk & yk mjien lkmt nvarxk, nvaryk
			nvarxk=0
			nvaryk=0
			do jj=1,jlp_nrowpf !!!! rivit, joilla xk-muuttujia
				jcurix_=jlp_ifxcurrows(jj)
				irowj_ = jlp_irowrow(jcurix_)
				do k_=1,jlp_nfxrow(irowj_) ! silmukka : rivin xk-muuttujat
					if(jlp_keyfact(iunit,jlp_irowfxvars(jlp_ibafx(irowj_)+k_)).eq. &
					jlp_irowffact(jlp_ibafx(irowj_)+k_)) then
						nvarxk = nvarxk + 1
					endif !if(j_keyfact(iunit,j_irowfxvars(j_ibafx(irowj_)+k_)).eq.j_irowffact(j_ibafx(irowj_)+k_)) then
				enddo !do k_=1,j_nfxrow(irowj_)
			enddo !do jj=1,j_nrowpf

			do jj=1,jlp_nrowpfy !!!!!  rivit, joilla yk-muuttujia
				jcurix_=jlp_ifycurrows(jj)
				irowj_ = jlp_irowrow(jcurix_)
				do k_=1,jlp_nfyrow(irowj_) ! silmukka : rivin yk-muuttujat
					listy_=jlp_irowfyvars(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava ptl-lista
					listf_=jlp_irowfyfact(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava tehdas-lista
					do ivars_=1,j_o(listy_)%i(1) ! yk-mjan puutavaralistan muuttujat
						iv2elpos_ = jlp_ifyvarskeep(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan xmat-sarake
						iv2xykypos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan paikka xkyk-listassa
						do ifact_=1,j_o(listf_)%i(1) ! yk-mjan tehdaslistan tehtaat
							iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj_)+k_)+ifact_-1) !tehtaan paikka factories-listassa
				!value = value+gamma*_xdata_(keepx,iobs)
							if(jlp_keyfact(iunit,iv2xykypos_).eq.iv3factpos_) then
								nvaryk = nvaryk + 1
							endif !if(j_keyfact(iunit,iv2xykypos_).eq.iv3factpos_) then
						enddo !do ifact_=1,j_o(listf_)%i(1)
					enddo !do ivars_=1,j_o(listy_)%i(1)
				enddo !do k_=1,j_nfyrow(irowj_)
			enddo !do jj=1,j_nrowpfy

			if(allocated(jlp_rowxkfkey))then
				if(size(jlp_rowxkfkey).lt.nvarxk)deallocate(jlp_rowxkfkey)
			endif !if(allocated(j_rowxkfkey))then
			if(.not.allocated(jlp_rowxkfkey))allocate(jlp_rowxkfkey(1:nvarxk))

			if(allocated(jlp_rowykfkey))then
				if(size(jlp_rowykfkey).lt.nvaryk)deallocate(jlp_rowykfkey)
			endif !if(allocated(j_rowykfkey))then
			if(.not.allocated(jlp_rowykfkey))allocate(jlp_rowykfkey(1:nvaryk))

			nrowxkfkey=0
			nrowykfkey=0
!write(17,*)'kierny',kier
			do jj=1,jlp_nrowpf ! rivit, joilla xk-muuttujia
				jcurix_=jlp_ifxcurrows(jj)
				irowj_ = jlp_irowrow(jcurix_)
				do k_=1,jlp_nfxrow(irowj_) ! silmukka : rivin xk-muuttujat
					if(jlp_keyfact(iunit,jlp_irowfxvars(jlp_ibafx(irowj_)+k_)).eq. &
					jlp_irowffact(jlp_ibafx(irowj_)+k_)) then
						nrowxkfkey = nrowxkfkey + 1
						jlp_rowxkfkey(nrowxkfkey)%irowfx = jlp_ibafx(irowj_)+k_
						jlp_rowxkfkey(nrowxkfkey)%jcurix = jcurix_
					endif !if(j_keyfact(iunit,j_irowfxvars(j_ibafx(irowj_)+k_)).eq.j_irowffact(j_ibafx(irowj_)+k_)) then
				enddo !do k_=1,j_nfxrow(irowj_)
			enddo !do jj=1,j_nrowpf

			do jj=1,jlp_nrowpfy ! rivit, joilla yk-muuttujia
				jcurix_=jlp_ifycurrows(jj)
				irowj_ = jlp_irowrow(jcurix_)
				do k_=1,jlp_nfyrow(irowj_) ! silmukka : rivin yk-muuttujat
					listy_=jlp_irowfyvars(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava ptl-lista
					listf_=jlp_irowfyfact(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava tehdas-lista
					do ivars_=1,j_o(listy_)%i(1) ! yk-mjan puutavaralistan muuttujat
						iv2elpos_ = jlp_ifyvarskeep(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan xmat-sarake
						iv2xykypos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan paikka xkyk-listassa
						do ifact_=1,j_o(listf_)%i(1) ! yk-mjan tehdaslistan tehtaat
							iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj_)+k_)+ifact_-1) !tehtaan paikka factories-listassa
							if(jlp_keyfact(iunit,iv2xykypos_).eq.iv3factpos_) then
								nrowykfkey = nrowykfkey + 1
								jlp_rowykfkey(nrowykfkey)%ivfout = jlp_fyfactout(iv2xykypos_,iv3factpos_)
								jlp_rowykfkey(nrowykfkey)%iv2elpos = iv2elpos_
								jlp_rowykfkey(nrowykfkey)%jcurix = jcurix_
							endif !if(j_keyfact(iunit,iv2xykypos_).eq.iv3factpos_) then
						enddo !do ifact_=1,j_o(listf_)%i(1)
					enddo !do ivars_=1,j_o(listy_)%i(1)
				enddo !do k_=1,j_nfyrow(irowj_)
			enddo !do jj=1,j_nrowpfy

		endif !if (j_fpresent) then

!!!!laskentayksikkö vaihtunut -> lasketaan tehdas-y-mjien muunnokset
		if (nfy.gt.0.and.iunit.ne.iunitrans) then
			do j=1,j_o(jlp_ivkeepc)%i(1)
				j_v(j_o(jlp_ivkeepc)%i2(j))=j_o(ivcmat)%d((iunit-1)*j_o(jlp_ivkeepc)%i(1)+j)
			enddo !do j=1,j_o(j_ivkeepc)%i(1)
			do j=1,ntrans
				call dotrans(jlp_itransv(j),1)
				if(j_err)then
					write(6,*)'err for trans ',j
					stop 975
				endif !if(j_err)then
			enddo !do j=1,ntrans
			iunitrans = iunit
		endif !if (nfy.gt.0.and.iunit.ne.iunitrans) then
! end preparing for factories

!!!!!******************** can a schedule enter <B34>
! avainvaihtoehdon alkukohta xmatissa
		ikey_ = jlp_ibaunit(iunit)+jlp_keys(iunit)

! kaksi strategiaa, 1: nykyinen, jossa yksiköstä otetaan aina vain paras vaihtoehto
! uusi strategia: käydään yksikköä läpi ja otetaan vaihtoehto heti kantaan jos sen varjohinta on suurempi kuin avain-
! vaihtoehdon hinta+ tolerannssi
! seuraavassa vaiheessa jatketaan vaihtoehdoilla ssamasta yksiköstä

!tätä varten tarvitaan aliohjelma joka laskee vaihtoehdon arvon

! aluksi sovelletaan nykyistä strategiaa, kun tulee slow improvement siirrytään
! toiseen strategiaan

! secondt=.true. jos käytetään toista strategiaa

		if(p.and.jlp_fpresent)write(n16,*)'**fact** <5586> unit,key',iunit,jlp_keys(iunit)
		fastvaluemin=1.7d37
!write(17,*)'nytpå',kier
		nschloop:	do i=1,jlp_nsch(iunit)
!write(17,*)'nytpot',kier,iunit,i
			iobs=jlp_ibaunit(iunit)+i
			if(fastnow)then
				if(fastreject(iobs).and.i.ne.jlp_keys(iunit))cycle  !i.ne.j_keys(iunit) added 20.8.2018 JL
			endif !if(fastnow)then
			if(subfilre)then
				if(jlp_rejects(iobs))then
					if(istree)then
						ibax=jxmatiba(iobs) !,1)
						do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
			!				valuesums(jj_)=valuesums(jj_-1)+dprod(vxpack3(jj_),jlp_xmat(ixpack3(jj_)+ibax))
						enddo !do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
					endif !if(istree)then
					cycle
				endif !if(j_rejects(iobs))then
			endif !if(subfilre)then

!!!!kannassa olevat vaihtoehdot ohitetaan
			do k_=1,jlp_ld0
				if(jlp_lunit(jlp_ld(k_)).ne.iunit) cycle
				if(jlp_isch(jlp_ld(k_)).ne.i) cycle
				if(istree)then
					ibax=jxmatiba(iobs) !,1)
					do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
						valuesums(jj_)=valuesums(jj_-1)+&
						vxpack3(jj_)*jlp_xmat(ixpack3(jj_)+ibax) !dprod(vxpack3(jj_),jlp_xmat(ixpack3(jj_)+ibax))
					enddo !do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
				endif !if(istree)then
				if(fast.and..not.fastnow)fastvalues(i)=1.7e37
				cycle nschloop
			enddo !do k_=1,j_ld0

			if (jlp_xpresent2) then
				if(istree)then
					ibax=jxmatiba(iobs) !,1)
					do jj_=idiff3(idiff(iobs)),nxbas
						valuesums(jj_)=valuesums(jj_-1)+vxpack3(jj_)*jlp_xmat(ixpack3(jj_)+ibax) !dprod(vxpack3(jj_),jlp_xmat(ixpack3(jj_)+ibax))

					enddo !do jj_=idiff3(idiff(iobs)),nxbas
					jlp_value=valuesums(nxbas)

				else !if(istree)then
					ibax=jxmatiba(iobs) !,1)
					jlp_value=dot_product(jlp_vxpack2(1:nxbas),jlp_xmat(jlp_ixpack2(1:nxbas)+ibax) )   !!!!
		!pitäsi testata, kumpi on nopeampi. Näyttävät PC:llä tuottavan saman tuloksen
		!	  j_value=0.d0
		!  do jj_=1,nxbas
		!  j_value=j_value+dprod(j_vxpack2(jj_),j_xmat(j_ixpack2(jj_),iobs))
		!  enddo
				endif !if(istree)then

			else !if (j_xpresent2) then
				jlp_value = 0.d0
			endif !if (j_xpresent2) then
			if(p)write(16,*)'<6712>,nxbas,iunit,i,initval',nxbas,iunit,i,jlp_value
!!!jos tehtaita, valuen arvoa kasvatetaan
			if (jlp_fpresent) then !!!!
				jlp_value = jlp_value + value_f(iobs)

			endif !if (j_fpresent) then

			if(jlp_value.gt.jlp_valueopt)then  !!!!
				secondb=jlp_valueopt
				isecond=iopt

				iopt=i
	! valueopt2=valueopt

				jlp_valueopt=jlp_value
			endif !if(j_value.gt.j_valueopt)then
			if(jlp_value.gt.secondb.and.abs(jlp_value-jlp_valueopt).gt.0.d-6)then
				secondb=jlp_value
				isecond=i
			endif !if(j_value.gt.secondb.and.abs(j_value-j_valueopt).gt.0.d-6)then

			if(i.eq.jlp_keys(iunit)) jlp_valuek=jlp_value !

			if(fastmake)then
				fastvalues(i)=jlp_value
				if(jlp_value.lt.fastvaluemin)fastvaluemin=jlp_value
			endif !if(fastmake)then

		enddo nschloop !nschloop:	do i=1,j_nsch(iunit)

		if(fastmake)then
			if(fastreject(jlp_ibaunit(iunit)+iopt))nimp=nimp+1
			fastcut=min(fastvaluemin+fastpros2*(jlp_valueopt-fastvaluemin),jlp_valuek)-jlp_tiny78
			fastreject(jlp_ibaunit(iunit)+1:jlp_ibaunit(iunit)+jlp_nsch(iunit))=.false.
			do i=1,jlp_nsch(iunit)
				if(fastvalues(i).lt.fastcut.and.i.ne.jlp_keys(iunit))fastreject(jlp_ibaunit(iunit)+i)=.true.
!	if(fastvalues(i).lt.fastcut)write(16,*)'reject',iunit,i
			enddo !do i=1,j_nsch(iunit)
! if(.not.j_xmatinmemory)then !pack used observations in memory
	! iba=j_xmatlast*j_ntemp0 !to which place are putted
! !	write(16,*)'alkuiba',iba,j_xmatlopp,j_xmatlast
	! do i=1,j_nsch(iunit)
		! if((fastvalues(i).ge.fastcut.or.i.eq.j_keys(iunit)).and.j_xmatlast.lt.j_xmatlopp)then
			! iba2=j_xmatibas2+(i-1)*j_ntemp0   !from upper buffer
			! j_xmat(iba+1:iba+j_ntemp0)=j_xmat(iba2+1:iba2+j_ntemp0)
			! j_xmatlast=j_xmatlast+1
			! j_memobs(j_ibaunit(iunit)+i)=j_xmatlast
			! iba=iba+j_ntemp0
		! else
			! j_memobs(j_ibaunit(iunit)+i)=j_lopp+1
		! endif
		! !write(17,*)'toka',j_ibaunit(iunit)+i,j_memobs(j_ibaunit(iunit)+i),kier,iunit,i
		! !iba=iba+j_ntemp0
	! enddo
	! j_xmatlast2=0
	! j_xmatfirst2=0
	! ! do i=1,j_nsch(iunit)
	  ! ! ioob=j_ibaunit(iunit)+i
		! ! ibax=jxmatiba(ioob,1)
		! ! !if(pp)write(16,*)ioob,j_memobs(ioob),fastreject(ioob),j_xmat(ibax+1:ibax+j_ntemp0)
	! ! enddo
! !	write(16,*)'j_xmatlast,j_xmatlast2',j_xmatlast,j_xmatlast2
! !	stop 776
! ! ELSE
	! ! do i=1,j_nsch(iunit)
	  ! ! ioob=j_ibaunit(iunit)+i
		! ! ibax=jxmatiba(ioob,1)
		! ! write(16,*)ioob,j_memobs(ioob),fastreject(ioob),j_xmat(ibax+1:ibax+j_ntemp0)
	! ! enddo
	! ! write(16,*)'j_xmatlast,j_xmatlast2',j_xmatlast,j_xmatlast2
	! ! stop 775

! endif
! if(j_xdatfromdisk)then !pack used observations in memory
	! iba=j_xdatlast*keepx
	! do i=1,j_nsch(iunit)
		! if((fastvalues(i).ge.fastcut.or.i.eq.j_keys(iunit)).and.j_xdatlast.lt.j_xdatlopp)then

			! iba2=j_xdatibas2+(i-1)*keepx
			! j_o(ivxmat)%r(iba+1:iba+keepx)=j_o(ivxmat)%r(iba2+1:iba2+keepx)
			! j_xdatlast=j_xdatlast+1
			! iba=iba+keepx
		! endif
	! enddo
	! j_xdatlast2=0
	! j_xdatfirst2=0
! endif

		endif !if(fastmake)then

!write(17,*)'senterinlopus',kier

	end subroutine senter !subroutine senter()

	subroutine tulostele()
 
!end kierloop
 
		if(jlp_feasible)then
			If(.not.jlp_maxo)jlp_objf=-jlp_objf
			write(6,*) 'solution is feasible'!!!!
		else !if(j_feasible)then
			write(6,*) 'SOLUTION IS INFEASIBLE' !!!!
!	write(6,*)'nofeasible rows ',nnf
			if(iprint.lt.2)iprint=2
		endif !if(j_feasible)then

!write(6,*)'pivots ',ipivot,' rounds ',kier  !!!!
		if(nresw.gt.7)write(6,*)'total number of residual cannot enter conditions ',nresw
		if(npivotw.gt.7)write(6,*)'total number of pivot cannot be made conditions ',npivotw
		if(nkeyfactw.gt.7)write(6,*)'total number of key factory cannot be changed conditions ',nkeyfactw
		if(jlp_xpresent)write(6,*)'key schedule changed ',nkeys, ' times'  !!!!
		if(jlp_fpresent)write(6,*)'key factory  changed ',nkeyf, ' times'  !!!!
		write(6,*)'basic variables computed ',ntote,' times' !!!!

! if(p9)then
! write(16,*)'xps'
! call testxpssub(1)
! !write(16,*)'**fact9* Testxps',(j_xps(j),j=0,j_nrow)
! endif
		if(printlf.or.p9)then
			write(6,*)'xkf variables in the basis'
			write(6,*)'unit,index in xkyk%-list, factory index in factories%-list,keyfactory, amount'
			do lfj_=jlp_mxd+1,jlp_lf0
				write(6,*) jlp_lunit(jlp_lf(lfj_)),jlp_ixkf(jlp_lf(lfj_)), &
				jlp_ixkffact(jlp_lf(lfj_)),jlp_keyfact(jlp_lunit(jlp_lf(lfj_)),&
					jlp_ixkf(jlp_lf(lfj_))),jlp_x(jlp_nrowz+jlp_lf(lfj_))
			enddo !do lfj_=j_mxd+1,j_lf0
		endif !if(printlf.or.p9)then

!close(16)

		if(jlp_feasible)then
			j_v(ivfeasible)=1.
			j_v(ivoptimal)=1.

		endif !if(j_feasible)then

		call defsolu()
		if(jlp_xpresent)then

			call getsolx() !jlp_nunits,ibaunit,keys,lunit,nsch,isch,
		endif !if(j_xpresent)then
! get the solution ready to be accesed by the inquiry routines

		if(printlf.or.p9)then
			write(6,*)'xkf variables in the basis'
			write(6,*)'unit,index in xkyk%-list, factory index in factories%-list,keyfactory, amount'
			do lfj_=jlp_mxd+1,jlp_lf0
				write(6,*) jlp_lunit(jlp_lf(lfj_)),jlp_ixkf(jlp_lf(lfj_)), &
				jlp_ixkffact(jlp_lf(lfj_)),jlp_keyfact(jlp_lunit(jlp_lf(lfj_)),&
					jlp_ixkf(jlp_lf(lfj_))),jlp_x(jlp_nrowz+jlp_lf(lfj_))
			enddo !do lfj_=j_mxd+1,j_lf0
		endif !if(printlf.or.p9)then

		if (jlp_fpresent) call defsoluf()



	end subroutine tulostele !subroutine tulostele()

	subroutine tulostele2() !prints and checkes if finished
		integer i
		goto900=.false.
!goto785=.false.

		if(jlp_feasible)then
			if(kier.le.10.or.int(kier/10)*10.eq.kier)then
				as=100.
				if(fast)as=100.*(1.-1.*count(fastreject)/jlp_lopp)

	!	pros=0.
				call cpu_time(time)
	!		write(6,*)'objprev',j_objfprev
				if(coefmax*jlp_objfprev.le.0.d0)then
					write(6,'(i5,i8,g22.12,7x,f8.2,5i6,f6.2,f7.2,i5)')&
						kier,ipivot,coefmax*jlp_objf,as,lr0,lz0,jlp_ld0,jlp_lf0,nnf,(time-time0)/60., &
						(time-time00)/60.
					time0=time;ncyc=0
					kier0=kier
					jlp_objfprev=jlp_objf
				else !if(coefmax*j_objfprev.le.0.d0)then
					if(coefmax*jlp_objfprev.gt.0.d0)pros=100.*coefmax*(jlp_objf-jlp_objfprev)/jlp_objfprev
					change=jlp_objf-jlp_objfprev
					nimp2=-1
					if(as.ne.asv.and.asv.ne.100.)nimp2=nimp

					asv=as
					call cpu_time(time)
		!	j_buf(1:12)=' ';j_buf(2:11)=j_chr10(j_objf-j_objfprev)
					jlp_objfprev=jlp_objf
					write(6,'(i5,i8,g22.12,f8.4,f7.2,5i6,f6.2,f7.2,i5)')&
						kier,ipivot,coefmax*jlp_objf,pros,as,lr0,lz0,jlp_ld0, &
						jlp_lf0,nnf,(time-time0)/60.,(time-time00)/60.,nimp2

					ipivot9=ipivot !last pivot printed
					time0=time;ncyc=0
					if(isstop.and.kier.gt.10)then
						j_v(j_ivimp)=nimp
						j_v(j_ivchangep)=pros
						j_v(j_ivround)=kier
						! call dotrans(iob,jlp_iostop)
						! if(j_v(jlp_ivstop).ne.0)then
						if(j_codevalue(iob,jlp_stoplink).ne.j_0)then
							write(6,*)'iteration stops due to stop->'
							goto900=.true.
							return
						endif !if(j_v(j_ivstop).ne.0)then

					else !if(isstop.and.kier.gt.10)then
						if(pros.lt.0.01.and.kier.ge.30)then
							write(6,*)'iteration stops due to assumed  stop->(Change%.lt.0.01.and.Round.ge.30)'
							goto900=.true.
							return
						endif !if(pros.lt.0.01.and.kier.ge.30)then

					endif !if(isstop.and.kier.gt.10)then
				endif !if(coefmax*j_objfprev.le.0.d0)then



	 ! if(kier.eq.30)then
		! do iio=1,j_lopp
	!Round  Pivots      Objective         Change    Change% active% resid    z  sched  xkf  NF-rows
	!   5    4820     2695025.15284     2695025.15   0.000  100.00   439     0    97   540     0

 ! endif

			endif !if(kier.le.10.or.int(kier/10)*10.eq.kier)then
			if(fast)then
				re=count(fastreject)
		!if(pp)write(16,*)'rejpros',re/j_lopp,fastusesame,fastusedsameFround,non
				fastusedsame=fastusedsame+1
				if(fastusedsame.gt.fastusesame)then
					fastnow=.false.
					fastusedsame=0
					fastmake=.true.
					nimp=0  !fastmake alkaa
		!	write(6,*)'fastmake,kier,nimp,nimpr',kier,nimp
		!	nnnn=0   ! number of
			! if(j_xdatfromdisk)j_xdatlast=0
			! !fastreject=.false.
			! if(.not.j_xmatinmemory)then
			! j_xmatlast=0
			! j_memobs=j_lopp+1
			! endif
			!if(.not.j_xdatinmemory)j_xdatlast=0

				else !if(fastusedsame.gt.fastusesame)then
	!		write(6,*)'hep'
			!if(fastmake)then
				!if(pp)write(16,*)'j_xmatlast,j_xdatlast',j_xmatlast,j_xdatlast
				! iero=0
				! iero2=0
				! do ii=1,j_lopp
					! if(fastreject(ii).and.j_memobs(ii).le.j_lopp)iero=iero+1
					! if(.not.fastreject(ii).and.j_memobs(ii).gt.j_lopp)iero2=iero2+1
			!	write(6,*)'fastmakeloppuu',kier,nimpr,nimp

				! enddo
			!	write(6,*)'iero,iero2',iero,iero2
			!endif

					fastnow=.true.
					fastmake=.false.
			! j_xmatlast2=0   !
			! j_xdatlast2=0   !not from upper buffer
	!				write(6,*)'<457>',fastmake,j_xdatlast,j_xmatlast,j_xmatlast2,&
			!	j_xdatlast2,j_xmatibas2,j_xdatibas2
			! ierr=0
			! do i=1,j_lopp
				! if(fastreject(i).and.j_memobs(i).le.j_lopp)ierr=ierr+1

			! enddo
			! write(6,*)'ierr ',ierr
			! if(ierr.ne.0)then
				! write(6,*)'ierr ',ierr
				! stop 888
			! endif

				endif !if(fastusedsame.gt.fastusesame)then

			endif !if(fast)then
		else !if(j_feasible)then
	!!!!! compute the sum of infeasiblity after each round (is this necessary?)
			numn=0
			jlp_value=jlp_zero
			do i=1,lr0 !lr0 = number of basic residuals
		!rhscur =rhs or rhs2
				if(jlp_lower(jlp_lr(i)).and.jlp_x(jlp_lr(i)).gt.jlp_tole(jlp_lr(i))) then
					numn=numn+1
					jlp_value=jlp_value-jlp_x(jlp_lr(i))
				elseif(.not.jlp_lower(jlp_lr(i)).and.jlp_x(jlp_lr(i)).lt.-jlp_tole(jlp_lr(i))) then !if(j_lower(j_lr(i)).and.j_x(j_lr(i)).gt.j_tole(j_lr(i))) then
					numn=numn+1
					jlp_value=jlp_value+jlp_x(jlp_lr(i))
				endif !if(j_lower(j_lr(i)).and.j_x(j_lr(i)).gt.j_tole(j_lr(i))) then
			enddo !do i=1,lr0
			if(kier.le.10.or.int(kier/10)*10.eq.kier)then
				as=100.
				if(fast)as=100.*(1.-1.*count(fastreject)/jlp_lopp) !laske muualla
				pros=0.
				call cpu_time(time)
				write(6,'(i5,i8,g22.12,7x,f8.2,5i6,f6.2,f7.2,i5)')&
					kier,ipivot,coefmax*jlp_objf,as,lr0,lz0,jlp_ld0,jlp_lf0,nnf,(time-time0)/60., &
					(time-time00)/60.
				time0=time

			endif !if(kier.le.10.or.int(kier/10)*10.eq.kier)then
		endif !if(j_feasible)then
! if(kier.ge.mxiter)then
	! write(6,*)'**maxiter reached'
	! iunitv=iunitprev
	! goto785=.true.;return
! endif !if(kier.ge.mxiter)then
		if(p)write(n16,*)'kier,iunit1,obj**',kier,jlp_objf
!!!! slow improvement?
		if(((jlp_objf.le.jlp_xirowold2+jlp_tolecur).or.(slow.gt.0.d0.and.&
			jlp_objf.le.jlp_xirowold2+slow).or.(slow.lt.0d0.and. &
			100.*(jlp_objf/jlp_xirowold2-1.).le.-slow)) .and. &
			kier.gt.200)then  !!!!  ???
	! on mahdollista että kanta on vaihtunut mutta ei
	! ole tapahtunut parannusta, toisaalta ei voida katsoa
	! pelkkää parannusta, sillä on mahdollista, että jos vaihtohetoja
	! on vähän ja paljon rajoituksia, niin probleeman käynnistymisvaiheessa
	! ei tapahdu tavoitefunktion parannusta vaikka käydään muutama kierros
	! laskentayksiköiden läpi
	! tähän vähän sofistukoidumpi ehto

	!restart
			jlp_restarted = .true.

			if((fastusesame.ne.1.or.iterxkf.ne.1).and..not.tried)then          !fast
				write(6,*)'slow improvement, pivots ',ipivot,' rounds ',kier, 'obj ',jlp_objf
				write(6,*)'let us continue without heuristics'
				fast=.false.
				fastnow=.false.
				iterxkf=1
				tried=.true.
				iunitv=iunit   ! =1 prev  !???????? 7.2.2017
				kierv=kier
				goto900=.true.
				return

			endif !if((fastusesame.ne.1.or.iterxkf.ne.1).and..not.tried)then

			write(6,*)' return because of slow improvement '  !!!!
	!isslow=.true.
			if (jlp_feasible) then
				If(.not.jlp_maxo)jlp_objf=-jlp_objf
				write(6,*) 'solution is feasible, objective',jlp_objf
			else !if (j_feasible) then
				write(6,*) 'SOLUTION IS INFEASIBLE, temporary objective',jlp_objf
				write(6,*)'nofeasible rows ',nnf
			endif !if (j_feasible) then
			write(6,*)'pivots ',ipivot,' rounds ',kier
			if(nresw.gt.7)write(6,*)'total number of residual cannot enter conditions ',nresw
			if(npivotw.gt.7)write(6,*)'total number of pivot ccannot be made conditions ',npivotw
			if(nkeyfactw.gt.7)write(6,*)'total number of key factory cannot be changed conditions ', &
			nkeyfactw
			if(nkeys.gt.0)write(6,*)'key schedule changed ',nkeys, ' times'
			if(jlp_fpresent)write(6,*)'key factory  changed ',nkeyf, ' times'
			write(6,*)'values of basic variables computed ',ntote,' times'

			if(p9)then
		!write(16,*)'xps'
				call testxpssub(1)
		!write(16,*)'**fact2* Testxps',(j_xps(j),j=0,j_nrow)
			endif !if(p9)then
			if(printlf.or.p9)then
				write(6,*)'xkf variables in the basis'
				write(6,*)'unit,index in xkyk%-list, factory index in factories%-list,keyfactory, amount'
				do lfj_=jlp_mxd+1,jlp_lf0
					write(6,*) jlp_lunit(jlp_lf(lfj_)),jlp_ixkf(jlp_lf(lfj_)), &
					jlp_ixkffact(jlp_lf(lfj_)),jlp_keyfact(jlp_lunit(jlp_lf(lfj_)),&
						jlp_ixkf(jlp_lf(lfj_))),jlp_x(jlp_nrowz+jlp_lf(lfj_))
				enddo !do lfj_=j_mxd+1,j_lf0
			endif !if(printlf.or.p9)then
!	close(16)
			call defsolu() !!!!

	! get sums of x-variables
			if(jlp_xpresent)then
				call getsolx() !jlp_nunits,ibaunit,keys,lunit,nsch,isch,& !!!!

		! get the solution ready to be accesed by the inquiry routines
			endif !if(j_xpresent)then

			if (jlp_fpresent) call defsoluf()  !!!!

			if(jlp_feasible)then
				j_v(ivfeasible)=1.
				j_v(ivoptimal)=1.
			endif !if(j_feasible)then

			goto900=.true.;return   ! <b332>
		endif !kier.gt.200)then


	end subroutine tulostele2 !subroutine tulostele2()

	subroutine fenter0()
!computes j_valueopt_af     which is the difference between value of keyfactory and optimal
!globaaliksi iopt
		jlp_valueopt_af = jlp_zero
		iobs=jlp_ibaunit(iunit)+iopt
		ikey_ = jlp_ibaunit(iunit)+jlp_keys(iunit)  !secondb needs this
		ibaobs=jxmatiba(iobs)  !,1)
		ibakey_=jxmatiba(ikey_)  !,2)
		if(p)write(16,*)'valueopt_af0',jlp_valueopt_af
		do jj = 1,nrowxkfkey
			irowfkeep_ = jlp_irowfkeep(jlp_rowxkfkey(jj)%irowfx)
			if(p)write(16,*)'tat ', jlp_rowxkfkey(jj)%jcurix,irowfkeep_,&
				jlp_rowxkfkey(jj)%irowfx,	jlp_coeffx(jlp_rowxkfkey(jj)%irowfx),irowfkeep_	,iobs,ikey_

		!if(p.and.j_rowxkfkey(jj)%jcurix.eq.2)then


			jlp_valueopt_af(jlp_rowxkfkey(jj)%jcurix) = jlp_valueopt_af(jlp_rowxkfkey(jj)%jcurix) +&
				jlp_coeffx(jlp_rowxkfkey(jj)%irowfx)*&
				(j_o(ivxmat)%d(ibaobs+irowfkeep_) -&   !jxmatiba
				j_o(ivxmat)%d(ibakey_+irowfkeep_))      !jxmatiba
	!	if(p.and.j_rowxkfkey(jj)%jcurix.eq.2)write(	16,*)j_valueopt_af(j_rowxkfkey(jj)%jcurix)
		enddo !do jj = 1,nrowxkfkey
		if(p)write(16,*)'valueopt_af1',jlp_valueopt_af
		if(p)write(16,*)'iobs,ikey_',iobs,ikey_
		do jj=1,nrowykfkey
			iv2elpos_ = jlp_rowykfkey(jj)%iv2elpos
	!	if(j_rowykfkey(jj)%jcurix.eq.3)write(16,*)'tas2 ',jj, &
	!		j_valueopt_af(j_rowykfkey(j_)%jcurix),j_v(j_rowykfkey(j_)%ivfout),&
	!		j_o(ivxmat)%r(ibaobs+iv2elpos_),j_o(ivxmat)%r(ibakey_+iv2elpos_)  !jxmatiba
			jlp_valueopt_af(jlp_rowykfkey(jj)%jcurix) = jlp_valueopt_af(jlp_rowykfkey(jj)%jcurix) + &
				j_v(jlp_rowykfkey(jj)%ivfout)*&
				(j_o(ivxmat)%d(ibaobs+iv2elpos_) -&
				j_o(ivxmat)%d(ibakey_+iv2elpos_))
		enddo !do j_=1,nrowykfkey
		if(p)write(16,*)'valueopt_af',jlp_valueopt_af




	end subroutine fenter0 !subroutine fenter0()



	subroutine fenter()
 
		goto100=.false.;goto400=.false.;goto5316=.false.

		ienter=0
		jlp_post=.true.

!if (j_fpresent) then

		if(p) then
			write(n16,*)'<6619> Avaintehtaat: '
			write(n16,*)jlp_keyfact(iunit,1:j_o(jlp_ivxkyk)%i(1))
		endif !if(p) then

!!!!2jatketaan saman yksikön seuraavasta tehdasmjasta
!!!! ixkykenter != 0, jos edellisellä kierroksella saman yksikön xkf tuli kantaan
		ixkyk1_ = ixkykenter + 1
		ixkykenter = 0

		do ixkyk_=ixkyk1_,j_o(jlp_ivxkyk)%i(1)	!!!!	xkyk-lista
			jlp_valueopt = jlp_small
	!tavoitefunktion ja a-matriisin päivitystä varten
			jlp_valueopt_af = jlp_zero
			jlp_valuek_af = jlp_zero

	!käydään läpi vain ptl-muuttujaan liittyvät tehtaat  TAHAN KIINNI
			do inf_=1, jlp_nxkfact(ixkyk_)
				if_ = jlp_xkfact(ixkyk_,inf_)%ifact

		!zerocapacity
		!if(zeroc(ixkyk_,if_))cycle  !!!!
				jlp_value = 0.d0
		!tavoitefunktion ja a-matriisin päivitystä varten
				jlp_value_af = jlp_zero

		!Testaus (estetään kantaan tulo  kun value==0 > valueopt)
				junk_ = 0
	!	if(j_v(j_xkykrowvars(j)%ind).lt.0.)cycle
		!käydään läpi vain plt-tehdas -yhdistelmään liittyvät tehtävärivien mjat
				do j=jlp_xkfact(ixkyk_,inf_)%i1xkykrowvar,jlp_xkfact(ixkyk_,inf_)%inxkykrowvar

		!write(16,*)'<fenter11>',j_irow2curix(0,j_xkykrowvars(j)%irow)
					if(jlp_irow2curix(0,jlp_xkykrowvars(j)%irow).ne.1)then
						write(6,*)'<fe33>,j',j
						stop 987
					endif !if(j_irow2curix(0,j_xkykrowvars(j)%irow).ne.1)then
			!do jc_ = 1,j_irow2curix(0,j_xkykrowvars(j)%irow)	! alkup. riviä vastaavat lavennetut rivit

					jc_=1
					junk_ = 1 !tarvitaanko tätä enää?
					jcurix_ = jlp_irow2curix(jc_,jlp_xkykrowvars(j)%irow)  !jc=1 onko joskus 0
					jcurix2=jlp_xkykrowvars(j)%irow-1
					if(jcurix_.ne.jcurix2)then
						write(6,*)'jc',jcurix_,jcurix2
						stop 871

					endif !if(jcurix_.ne.jcurix2)then
				!if(j_feasible)write(16,*)'<fenter13>jcurix_,', jcurix_ ,j_xkykrowvars(j)%irow,j_irow2curix(jc_,j_xkykrowvars(j)%irow)

					if (jcurix_ == 0) then
				! onko tavoiterivi
						if (jlp_feasible) then
							if (jlp_xkykrowvars(j)%isxk) then
								jlp_value = jlp_value + jlp_coeffx(jlp_xkykrowvars(j)%ind)
						!	 write(n16,*)'<6659> coeffx(xkykrowvars(j)%ind)', j_coeffx(j_xkykrowvars(j)%ind)
							else !if (j_xkykrowvars(j)%isxk) then
								if(p) call j_printname('<6655> yk mja ', &
								jlp_xkykrowvars(j)%ind,' jlpdebugging?')
						!	if(j_feasible)write(n16,*)'v(xkykrowvars(j)%ind)',j_v(j_xkykrowvars(j)%ind)
								jlp_value = jlp_value +  j_v(jlp_xkykrowvars(j)%ind)
							endif !if (j_xkykrowvars(j)%isxk) then
						else !if (j_feasible) then
						!meidän pitää laskea miten temporary tavoitefunktio riippuu xkf-muuttujan arvosta. On käytävä läpi kaikki rajoiterivit
						! ja katsottava onko rajoite infeasible ja jos on on poimitava xkf-muuttujan kerroint

						endif !if (j_feasible) then
					else !if (jcurix_ == 0) then
						if (jlp_xkykrowvars(j)%isxk) then
			!if(j_feasible)write(16,*)'<fent22>',j_value,j_vc(jcurix_),j_coeffx(j_xkykrowvars(j)%ind)
							jlp_value = jlp_value - jlp_vc(jcurix_)*jlp_coeffx(jlp_xkykrowvars(j)%ind)


						else !if (j_xkykrowvars(j)%isxk) then
			!	if(j_feasible)	write(16,*)'<fent33>',j_value,j_vc(jcurix_),j_v(j_xkykrowvars(j)%ind)
							jlp_value = jlp_value - jlp_vc(jcurix_)*j_v(jlp_xkykrowvars(j)%ind)
						endif !if (j_xkykrowvars(j)%isxk) then

					! if(.not.j_feasible)then   !this section was commented by JL 9.9.2018
					! if(j_objr2( jcurix_).ne.0.d0)then
					! if (j_xkykrowvars(j)%isxk) then
					! j_value=j_value-j_objr2( jcurix_)*j_coeffx(j_xkykrowvars(j)%ind)
					! else
					! j_value = j_value - j_objr2( jcurix_)*j_v(j_xkykrowvars(j)%ind)
					! endif
					! endif
					! endif !if(.not.feasible)then

					endif !if (jcurix_ == 0) then

			!enddo !do jc_ = 1,j_irow2curix(0,j_xkykrowvars(j)%irow)
				enddo !do j=j_xkfact(ixkyk_,inf_)%i1xkykrowvar,j_xkfact(ixkyk_,inf_)%inxkykrowvar

				if(jlp_value>jlp_valueopt.and.junk_>0) then  !!!!
					jlp_valueopt=jlp_value
					ifopt = if_
					infopt=inf_
				endif !if(j_value>j_valueopt.and.junk_>0) then

				if (if_.eq.jlp_keyfact(iunit,ixkyk_)) then
					jlp_valuek = jlp_value
					infkey=inf_
				endif !if (if_.eq.j_keyfact(iunit,ixkyk_)) then

			enddo !do inf_=1, j_nxkfact(ixkyk_)
	!	- jos ifopt == avaintehdas => seuraava xkyk-alkio
	! - jos valueopt.le.valuek+tolecur => seuraava xkyk-alkio
			if(p) write(n16,*)'<6682> ifopt,keyfact(iunit,ixkyk_),valueopt,valuek,tolecur',&
				ifopt,jlp_keyfact(iunit,ixkyk_),jlp_valueopt,jlp_valuek,jlp_tolecur
			if((ifopt/=jlp_keyfact(iunit,ixkyk_)).and.(jlp_valueopt>(jlp_valuek+0.01*jlp_tolecur))) then
		! xkyk-muuttuja voi tulla kantaan
				ienter=4 !!!!! xkyk-muuttuja kantaan

				ixkykenter=ixkyk_

				if(isxkzero(ixkykenter))then !entering variable is zero in all schedules of the solution
					jlp_keyfact(iunit,ixkykenter) = ifopt
					nkeyf=nkeyf+1
					goto5316=.true.;return ! changed by JL 11.9.2018 was goto 55
				endif !if(isxkzero(ixkykenter))then

				jlp_tmax=huge(jlp_one) ;rcur=jlp_tmax

				jlp_valueopt_af=0.d0
				do j=jlp_xkfact(ixkyk_,infopt)%i1xkykrowvar,jlp_xkfact(ixkyk_,infopt)%inxkykrowvar
					do jc_ = 1,jlp_irow2curix(0,jlp_xkykrowvars(j)%irow)	! alkup. riviä vastaavat lavennetut rivit
				! tavoitefunktion ja a-matriisin päivitystä varten
						jcurix_ = jlp_irow2curix(jc_,jlp_xkykrowvars(j)%irow)
						if (jlp_xkykrowvars(j)%isxk) then
				!	if(jcurix_.eq.3)write(16,*)'hier',j_valueopt_af(jcurix_),j_coeffx(j_xkykrowvars(j)%ind)
							jlp_valueopt_af(jcurix_) = jlp_valueopt_af(jcurix_) + jlp_coeffx(jlp_xkykrowvars(j)%ind)

						else !if (j_xkykrowvars(j)%isxk) then
				!	if(jcurix_.eq.3)write(16,*)'hier2',j_valueopt_af(jcurix_), j_v(j_xkykrowvars(j)%ind)
							jlp_valueopt_af(jcurix_) = jlp_valueopt_af(jcurix_) + j_v(jlp_xkykrowvars(j)%ind)
						endif !if (j_xkykrowvars(j)%isxk) then
					end do !do jc_ = 1,j_irow2curix(0,j_xkykrowvars(j)%irow)
				enddo !do j=j_xkfact(ixkyk_,infopt)%i1xkykrowvar,j_xkfact(ixkyk_,infopt)%inxkykrowvar

				jlp_valuek_af=0.d0

				do j=jlp_xkfact(ixkyk_,infkey)%i1xkykrowvar,jlp_xkfact(ixkyk_,infkey)%inxkykrowvar
					do jc_ = 1,jlp_irow2curix(0,jlp_xkykrowvars(j)%irow)	! alkup. riviä vastaavat lavennetut rivit
				! tavoitefunktion ja a-matriisin päivitystä varten
						jcurix_ = jlp_irow2curix(jc_,jlp_xkykrowvars(j)%irow)
						if (jlp_xkykrowvars(j)%isxk) then
							jlp_valuek_af(jcurix_) = jlp_valuek_af(jcurix_) + jlp_coeffx(jlp_xkykrowvars(j)%ind)
						else !if (j_xkykrowvars(j)%isxk) then
							jlp_valuek_af(jcurix_) = jlp_valuek_af(jcurix_) + j_v(jlp_xkykrowvars(j)%ind)
						endif !if (j_xkykrowvars(j)%isxk) then
					end do !do jc_ = 1,j_irow2curix(0,j_xkykrowvars(j)%irow)
				enddo !do j=j_xkfact(ixkyk_,infkey)%i1xkykrowvar,j_xkfact(ixkyk_,infkey)%inxkykrowvar

				ikeepxkyk_ = jlp_ixkykkeep(ixkyk_)

		! a-matriisin täydennys
				lf01 = jlp_lf0+1
				newf=jlp_lf(lf01)
				newa=newf+nz   ! after nz
				newc=newa+jlp_nrow  !in Fletecher cols, I part (residuals) are counted also
		!tavoiterivin päivitys
				jlp_objr0(newc) = jlp_valueopt_af(0) - jlp_valuek_af(0)
		!a-matriisin päivitys
				jlp_a(1:jlp_nrow,newa) = jlp_valueopt_af(1:jlp_nrow) - jlp_valuek_af(1:jlp_nrow)
		!otetaan talteen kantaan tulevan xkyk-muuttujan indeksi xkyk-listassa
		!ja tehtaan indeksi facroties-listassa
				jlp_ixkf(newf) = ixkyk_
				jlp_ixkffact(newf) = ifopt

				if(p) then
					write(n16,*)'**fact** KANTAAN tulossa>>  xkf, ienter = 4 : iunit, ixkyk, ifact ', &
					iunit, ixkykenter,ifopt
					write(n16,*) '**fact** kantaan tulossa xkf : valueopt, valuek, valueopt-valuek: ',  &
					jlp_valueopt, jlp_valuek, jlp_valueopt-jlp_valuek
					write(n16,*) '**fact** kantaan tulossa xkf : xk ,fact: ',&
						j_vname(j_o(jlp_ivxkyk)%i2(ixkykenter)), j_vname(j_o(jlp_ivfact)%i2(ifopt))
					write(n16,*) '**fact** kantaan tulossa xkf : lf01 ,newf,newa,newc: ', lf01 ,newf,newa,newc !!!!
					write(n16,*) '**fact** kantaan tulossa xkf : objr0(newc) ', jlp_objr0(newc)
					write(n16,*) '**fact** kantaan tulossa xkf : a(1:nrow,newa) ', (jlp_a(jj,newa),jj=1,jlp_nrow)
				endif !if(p) then

				goto100=.true.;return !!!! poistutaan xkyk-silmukasta & siirrytään tutkimaan leaving variablea
			endif !if((ifopt/=j_keyfact(iunit,ixkyk_)).and.(j_valueopt>(j_valuek+0.01*j_tolecur))) then

		enddo !do ixkyk_=ixkyk1_,j_o(j_ivxkyk)%i(1)
		goto400=.true. !!!!siirrytään seuraavaan laskentayksikköön
!endif !if (j_fpresent) then

	end subroutine fenter !subroutine fenter()

	subroutine entercol()
!when a schedule enters this computes the entering  column of the a-matrix
! and computes also the objective row element
! with factories j_valueopt_af computed with fenter0 is used both for the
! objective row and for the entire column
!käyttä iopt mikä globaaliksi
 
! j_valueopt_af computed with fenter0 is needed here toupdate the object row
		ld01=jlp_ld0+1  !!!! get next free column in a,
! ld0 is the number of used (basic) cols in D-part i.e. after z-cols
		newd=jlp_ld(ld01)
		newa=newd+nz   ! after nz
		newc=newa+jlp_nrow  !in Fletecher cols, I part (residuals) are counted also
		if(p) write(n16,*)'ent sched: newd',newd,'newc',newc, 'tol=',jlp_tolecur
! get key schdeule
		iobs= jlp_ibaunit(iunit)+jlp_keys(iunit)
		iobsopt=jlp_ibaunit(iunit)+iopt
		ibax=jxmatiba(iobs) !,1)
		ibax2=jxmatiba(iobsopt) !,2)
		jlp_lunit(newd)=iunit
		jlp_isch(newd)=iopt
! 6.42  a0'D , here we are prepared that the same x can be in differen rows
! how does the object variable change
!!!! put key schedule first int objr and a matrix
		if(jlp_ixcur(0).ne.0)then
			jlp_objr0(newc)=jlp_xmat(jlp_ix(0)+ibax2)-jlp_xmat(jlp_ix(0)+ibax) !v(ix(0))
			j1=2
		else !if(j_ixcur(0).ne.0)then
			j1=1
			jlp_objr0(newc)=0.
		endif !if(j_ixcur(0).ne.0)then

!objr:n päivitys
		if(jlp_fpresent) then
! nykyisessä domainissa 0-rivillä xk-muuttujia
			if((jlp_nrowpf.gt.0.and.jlp_ifxcurrows(1).eq.0).or.(jlp_nrowpfy.gt.0.and. jlp_ifycurrows(1).eq.0)) then
	!objr0:n päivitys
				jlp_objr0(newc)=jlp_objr0(newc)+jlp_valueopt_af(0)
			endif !if((j_nrowpf.gt.0.and.j_ifxcurrows(1).eq.0).or.(j_nrowpfy.gt.0.and. j_ifycurrows(1).eq.0)) then
		endif !if(j_fpresent) then

		if(sparse)then
! kannattaa varmaan tehdä vektori johon kerätään rivit joilla x-muuttujia

			iel=0
			istart1=jlp_lavecsp(jlp_lavecsp(0)+newa)-1

			do jj=j1,jlp_nrowp
				j=jlp_ixcurrows(jj)
				jlp_apu=jlp_xmat(jlp_ix(j)+ibax2)-jlp_xmat(jlp_ix(j)+ibax)  !a( ,newa ilamn pakkausta

				if(abs(jlp_apu).gt.jlp_tiny78)then
					iel=iel+1
					jlp_a(iel,newa)=jlp_apu
					jlp_lavecsp(istart1+iel)=j
				endif !if(abs(j_apu).gt.j_tiny78)then

	!robleema, jos pitää vaihtaa
			enddo !do jj=j1,j_nrowp

			last(newa)=istart1+iel
			call jlpgetcol(newa)

		else !if(sparse)then
			do j=1,jlp_nrow
				if(jlp_ixcur(j).ne.0)then
					jlp_a(j,newa)=jlp_xmat(jlp_ix(j)+ibax2)-jlp_xmat(jlp_ix(j)+ibax)  !v(ix(j))
				else !if(j_ixcur(j).ne.0)then
					jlp_a(j,newa)=0.
				endif !if(j_ixcur(j).ne.0)then
			enddo !do j=1,j_nrow

!a:n päivitys
			if(jlp_fpresent) then
				jlp_a(1:jlp_nrow,newa) = jlp_a(1:jlp_nrow,newa)+ jlp_valueopt_af(1:jlp_nrow)
			endif !if(j_fpresent) then

		endif !if(sparse)then
! get optimal schedule, put differences into objr and a

		jlp_tmax=jlp_one	;rcur=jlp_tmax	! myöhemmin pintala
		ienter=3	!!!! scedule enters

		if(p.and.jlp_fpresent) write(n16,*)'**fact** KANTAAN tulossa >> vaihtoehto, ienter = 3; unit, sch' , &
		iunit,iopt




	end subroutine entercol !subroutine entercol()


	subroutine leaving()
		goto112233=.false.
		goto222=.false.
		goto900=.false.;goto1234=.false.;goto55=.false.
!goto 55 go to next unit
		if(goto8888)goto 8888 !this comes directly from update
		if(goto8889)goto 8889 ! this also
!if(pp)write(16,*)'leaving,ienter,tmax,ienter,j_tmax',ienter,ienter,j_tmax
!cycle
		if(ienter.eq.ienterv.and.	jlp_objf.lt.oldsol+jlp_tolecur)then
			nsame=nsame+1
		else !if(ienter.eq.ienterv.and.	j_objf.lt.oldsol+j_tolecur)then
			ienterv=ienter
			oldsol=jlp_objf
			nsame=0
		endif !if(ienter.eq.ienterv.and.	j_objf.lt.oldsol+j_tolecur)then


		iunitv=iunit !!!!
		kierv=kier  !!!!
		justkey=.false.  !!!!
! determine leaving variable
! newc -is coming column , newa in A, newd in D
! tmax gives max values for eneterin var.

! update step
! determine leaving variable  c:new column, direct effect
! Bx=b  ,  Bx+tc=b
! xn=x- t*inv(B)*c =x-t*r  where r=inv(B)*c, i.e. solution of B*r=c

! we  make one call using ls,
! testi
! write(6,*)'100,ls,newc,a(1,newa)',ls(1),ls(2),newc,a(1,newa)

! subroutine fbsub(n,jmin,jmax,a,la,q,b,x,ls,aa,ll,save)
!  solves a system  B.x=b

!  Parameter list
!  **************
!   n   number of variables (as for bqpd)
!   a,la   specification of QP problem data (as for bqpd)
!   jmin,jmax  (see description of ls below)
!   q   an integer which, if in the range 1:n+m, specifies that the rhs vector
!       b is to be column q of the matrix A of general constraint normals.
!       In this case the parameter b is not referenced by fbsub.
!       If q=0 then b is taken as the vector given in the parameter
		if(sparse)then
			call jlpgetcol(newa)
			if(ienter.gt.1)then

				call fbsubsp(jlp_nrow,1,jlp_nrow,jlp_a,jlp_lavecsp,0,jlp_acol,r,&
					jlp_ls,wslu1,lwsll1,.false.)   !linux
			else !if(ienter.gt.1)then
	! residual enters
	! newc=resid , mq=newc
	! pitää testata voiko koko homman tehdä tällä, a(1,newa ei tarvita)

				call fbsubsp(jlp_nrow,1,jlp_nrow,jlp_a,jlp_lavecsp,newc,jlp_acol,r, &
					jlp_ls,wslu1,lwsll1,.false.)
			endif !if(ienter.gt.1)then

		else !if(sparse)then

			if(ienter.gt.1)then  !!!!
				if(p)write(n16,*)'newa',newa,'a:',jlp_a(1:jlp_nrow,newa)
				call fbsub(jlp_nrow,1,jlp_nrow,jlp_a,jlp_lavec,0,jlp_a(1:,newa),r,& !!!!
					jlp_ls,wslu1,lwsll1,.false.)   !!!!!

			else !if(ienter.gt.1)then
	! residual enters
	! newc=resid , mq=newc
				call fbsub(jlp_nrow,1,jlp_nrow,jlp_a,jlp_lavec,newc,jlp_a(1:,newa),r, & !!!!
					jlp_ls,wslu1,lwsll1,.false.)   !!!!
			endif !if(ienter.gt.1)then
		endif !if(sparse)then

		leave=0  !!!!position in ls
		leavk=0  !!!! leaving key schedule
		leavec=0 !!!! leaving columns

		if(.not.jlp_post)then
			do jj=1,jlp_nrow
				r(jlp_ls(jj))=-r(jlp_ls(jj))
			enddo !do jj=1,j_nrow
			jlp_post=.true.   !!!!
		endif !if(.not.j_post)then

		if(p)then
			write(n16,*)'start leaving*, ls:'
			write(n16,'(1x,15i5)')jlp_ls(1:min(jlp_nrow,100))

789	write(n16,*)'r:ls-order'
			write(n16,'(5g17.10)')(r(jlp_ls(jj7)),jj7=1,min(jlp_nrow,100))
! r=inv(B)*c
! tmax =absilute value for entering variable
! could do leaving in 3 different phases, residual, z, x

! if r>0 then x decreases
! if r<0 then x increases
			write(n16,*)'x,ls-order'
			write(n16,'(5g17.10)')(jlp_x(jlp_ls(jj7)),jj7=1,min(jlp_nrow,50))
			write(n16,*)'tmax',jlp_tmax, ' post',jlp_post
		endif !if(p)then

!if(j_post)then  !!!! t > 0  20160603
! case residual leaving:
! rhs1< a'x+b'z <rhs2    residual in basis  =>
! a'x+b'z+res=rhs2    or
! a'x+b'z+res=rhs1   res<0
! residual is leaving => residual becomes zero, i.e. xmi=xma=0
! for z-variables now also xmi=xma=0 ,later these can be different
!  but in case res>0, rhs2 is active and rhs exists and res is increasing, then
! it may happen that rhs1 becomes active

!    (((((((
! t>0 ==post
! x'=x-t*r >xmi    !tässä x tarkoitta mitä tahansa kantamuuttujaa
! if r>0  and t >0
! x'=x-t*r >xmi  -> t<(x-xmi)/r
! current max tmax, thus this new will become binding if (x-xmi)/r<tmax
! that is x-xmi<tmax*r  (in testing division by r is not good becasue it can
! lead to overflow)
! x'=x-t*r<xma
! r<0 and t>0  x-t*r<xma ->t<(x-xma)/r
! this will become binding if (x-xma)/r<tmax  i.e. (x-xma)>tmax*r

! initially when the solution is not feasible we must be prepared
! to the fact that x<xmi or x>xma ,
! if x<xmi then x can reach the lower limit xmi (i.e. increase) only if
! t>0 and r<0 or t<0 and r>0 , but then it is unnecessary to stop at lower limit xmi
! but continue up to xma i.e. use xma to test tmax
! similarly if x >xma then x will decrease (approach both limits xmi and xma) if
! t>0 and r>0 or t<0 and r<0 , then   ))))
! new, when z is positive

		do j=1,lz0  !!!! z leaving
!same as lr0 but now x cannot be negative
			leaz=jlp_nrow+jlp_lz(j)
			if(r(leaz).gt.jlp_tiny78)then
				if(jlp_x(leaz).lt.jlp_tmax*r(leaz))then
					jlp_tmax=jlp_x(leaz)/r(leaz) ;rcur=r(leaz)
					leavec=leaz
				endif !if(j_x(leaz).lt.j_tmax*r(leaz))then
			endif !if(r(leaz).gt.j_tiny78)then
		enddo !do j=1,lz0

		if(jlp_xpresent2)then
			do j=1,jlp_ld0  !!!!sched leaving
!same as lz
				lead=jlp_nrowz+jlp_ld(j)
				if(r(lead).gt.jlp_tiny78)then     !
					if(jlp_x(lead).lt.jlp_tmax*r(lead))then
						jlp_tmax=jlp_x(lead)/r(lead)  ;rcur=r(lead)
						leavec=lead  !!!!
					endif !if(j_x(lead).lt.j_tmax*r(lead))then

				endif !if(r(lead).gt.j_tiny78)then
			enddo !do j=1,j_ld0

!!!! voiko xkf poistua kannasta
			if (jlp_fpresent) then
!
				do j=jlp_mxd+1,jlp_lf0
	!same as lz
					leaf=jlp_nrowz+jlp_lf(j)
					if(r(leaf).gt.jlp_tiny78)then
						if(jlp_x(leaf).lt.jlp_tmax*r(leaf))then
							jlp_tmax=jlp_x(leaf)/r(leaf) ;rcur=r(leaf)
							leavec=leaf
							if(p) write(n16,*)'**fact** <6151> tmax päivitetty', jlp_tmax
						endif !if(j_x(leaf).lt.j_tmax*r(leaf))then

					endif !if(r(leaf).gt.j_tiny78)then
				enddo !do j=j_mxd+1,j_lf0
			endif !if (j_fpresent) then
		endif !if(j_xpresent2)then

		if(p)write(n16,*)'tmaxbefore r',jlp_tmax  !!!!!
		if(p9.and.jlp_tmax.lt.0.d0)write(n16,*)'negat tmax,ienter,leavec',jlp_tmax,ienter,leavec
2244	if(jlp_tmax.le.jlp_tmaxmin)then  !!!!! jos mukaan, pitää tsekata keyt ensin
			leave=jlp_lsi(leavec) !samakuin myöh
			if(p.or.p9.or.jlp_tmax.eq.-1.d0)then
				write(n16,*)'jump88,tmax, post,pivot=',jlp_tmax,jlp_post,ipivot,' ienter ',ienter
			endif !if(p.or.p9.or.j_tmax.eq.-1.d0)then
			if(ienter.eq.3.and.secondb.gt.jlp_valuek+jlp_tolecur)then
				if(p.or.jlp_tmax.eq.-1.d0)write(16,*)'secondb0 ', &
				secondb,jlp_valueopt,jlp_valuek,ipivot,iunit
				iopt=isecond
				jlp_valueopt=secondb
				secondb=jlp_small
				goto222=.true.;return
			endif !if(ienter.eq.3.and.secondb.gt.j_valuek+j_tolecur)then
			if(ienter.eq.3)call lcursam()
			goto 8883 !!!!! jump88 if j_tmax.le.j_tmaxmin before checking f residual can leave
		endif !if(j_tmax.le.j_tmaxmin)then


!j_tmax2=j_tmax   !-j_tiny78  !!!!!do not drop residual too easily

		do j=1,lr0 !!!!
! t>0 ==post   r>0 x decreases, r<0 x increases
! x'=x-t*r >= 0
! if r>0  and t >0 and x>0
! x'=x-t*r >0  -> t< x/r
! current max tmax, thus this new will become binding if x/r<tmax
! that is x<tmax*r  (in testing division by r is not good becasue it can
! lead to overflow)
! if r>=0  and t >0 and x<=0 then x becomes more negative, or remains
! if r<=0  and t >0 and x>=0 then x becomes more positive,or remains
! if r<0  and t >0 and x<=0 then x becomes zero if t=x/r <tmax
! if r>0  and t >0 and x>=0 then x becomes zero if t=x/r <tmax
!this can be tested by
! abs(x)<tmax*abs(r)

			if(r(jlp_lr(j)).gt.jlp_tiny78)then !tole(lr(j)))then
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

				if(jlp_ubou(jlp_lr(j)))then
	!we are in the attraction zone of the constraint
	!  if(ubou(lr(j)).and.x(lr(j))+rhs2(lr(j))-rhscur(lr(j))-tmax*r(lr(j)).lt.-tole(lr(j)))then
	! if upper bound is not active make it first
	! if there is no upper boun then residual can freely become negative
	!mutta jos resid<0
	!  row +resid=rhs2

	!jos resid<0 muutta yläraja aktiivinen tämä on epäloogista ja kantaan tuleva muuttuja
	! lisää ratkaisun epäloogisuutta ja vaihtoehtoja on kaksi luovutaan koko yrityksestä tain
	! otetaan residuaali pois kannasta

					if(jlp_lower(jlp_lr(j)).and.jlp_x(jlp_lr(j))+ &
					jlp_rhs2(jlp_lr(j))-jlp_rhscur(jlp_lr(j)).lt.r(jlp_lr(j))*jlp_tmax)then

						jlp_rhscur(jlp_lr(j))=jlp_rhs2(jlp_lr(j))
						if(jlp_xpresent)jlp_rhsw(jlp_lr(j))=jlp_rhscur(jlp_lr(j))-jlp_xps(jlp_lr(j))
						jlp_x(jlp_lr(j))=jlp_x(jlp_lr(j))+jlp_rhs2(jlp_lr(j))-jlp_rhs(jlp_lr(j)) !
						jlp_lower(jlp_lr(j))=.false.
						jlp_tmax=jlp_x(jlp_lr(j))/r(jlp_lr(j)) ;rcur=r(jlp_lr(j)) !toisin päin
						if(p)write(n16,*)'post,posr,ubou,lower,lr(j),tmax',jlp_lr(j),jlp_tmax
						leavec=jlp_lr(j)
					else if( jlp_x(jlp_lr(j)).ge.0.and..not.jlp_lower(jlp_lr(j)))then !if(j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs2(j_lr(j))-j_rhscur(j_lr(j)).lt.r(j_lr(j))*j_tmax)then
	!if(j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs2(j_lr(j))-j_rhscur(j_lr(j)).lt.r(j_lr(j))*j_tmax)then
     ! x+row=rhs2 and if r is positive row increases and x decreases until t=x/r
						if(jlp_x(jlp_lr(j)).lt.jlp_tmax*r(jlp_lr(j)) )then
							jlp_tmax=jlp_x(jlp_lr(j))/r(jlp_lr(j)) ;rcur=r(jlp_lr(j))
							if(p)write(n16,*)'post,posr,ubou,NOTlower,lr(j),tmax',jlp_lr(j),jlp_tmax
							leavec=jlp_lr(j)
						end if !if(j_x(j_lr(j)).lt.j_tmax*r(j_lr(j)) )then
					elseif(jlp_x(jlp_lr(j)).lt.0.d0.and..not.jlp_lower(jlp_lr(j)).and.jlp_feasible)then !if(j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs2(j_lr(j))-j_rhscur(j_lr(j)).lt.r(j_lr(j))*j_tmax)then

	!there is no lower bound thus if residual is in basis it should be postitive
	!but because of the rounding errors it is negative
	! if r is positive residuasl becomes more negative
	! if residual
	! x+row=rhs2 so it should
	!	if(j_x(j_lr(j)).lt.j_tiny78n)write(6,*)'row ',j_lr(j),' illegal residual(-)',j_x(j_lr(j)),' pivot=',ipivot,&
	!		'r=',r(j_lr(j)),' ienter=',ienter
						if(p.or.p9)write(n16,*)'row ',jlp_lr(j),' illegal residual(-)', &
						jlp_x(jlp_lr(j)),' pivot=',ipivot,&
							'r=',r(jlp_lr(j)),' ienter=',ienter

						leavec=jlp_lr(j)
						jlp_tmax=-1.d0 ;rcur=0.d0
		!p=.true.
						if(p.or.p9)write(n16,*)'negat resid jump*****r,x**',jlp_lr(j),r(jlp_lr(j)), &
						jlp_x(jlp_lr(j))
		!call isfeasible()
		!stop 871
						goto112233=.true.
						return
		!goto 2244

					end if !if(j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs2(j_lr(j))-j_rhscur(j_lr(j)).lt.r(j_lr(j))*j_tmax)then
				end if !if(j_ubou(j_lr(j)))then

			elseif(r(jlp_lr(j)).lt.jlp_tiny78n) then !if(r(j_lr(j)).gt.j_tiny78)then
!will lower become active
!negative resid  surplus
! row+resid =rhscur
! residuaali siis kasvaa
! tarkastellaan ensi tilannetta, jossa alaraja on olemassa ja se on aktiivinen
! rhscur=rhs
! row+resid=rhs
! jos resid<0 row>rhs ja residuaalin kasvaminen tarkoittaa että residuaali lähestyy nollaa
!jolloin alaraja tulee vastaan, tämä tapahtuu kun resid'=0 eli t=resid/r
!jos resid>0 row<rhs ja residuaalin kasvaminen kasvattaa nonfeasibilityä mutta siitä ei
! tarvitse välittää  PAITSI KUN OLLAAN JO FEASIBLESSÄ

! sitten tilanne että yläraja on aktiivinen eli rhscur=rhs2
! row+resid=rhs2
! jos resid>0 niin row<rhs2 residuaalin kasvaminen tarkoittaa, että row pienenee
! edelleeen kunnes alaraja tulee vastaan. Tämä tapahtuu kun
! resid'=resid-t*r=rhs2-rhs eli kun t=(resid-rhs2+rhs)/r
!testataan (resid-rhs2+rhs)/r<tmax eli (resid-rhs2+rhs) >r*tmax
! tilannetta (resid-rhs2+rhs)>0 ei pitäsi tulla vastaan
! jos resid<0 niin row>rhs2 eli infeasible, mutta tällöinkin pittä tarkastella
! alarajan tuloa vastaan kuten edellä

!if(pp)write(n16,*)'**hep**,x,sum',j_x(j_lr(j)),j_x(j_lr(j))+j_rhs(j_lr(j))-j_rhscur(j_lr(j))

				if(jlp_lbou(jlp_lr(j)))then
	! if the upper bound if active, can make
					if(.not.jlp_lower(jlp_lr(j)).and. &
					jlp_x(jlp_lr(j))+jlp_rhs(jlp_lr(j))-jlp_rhscur(jlp_lr(j)).gt. &
					r(jlp_lr(j))*jlp_tmax)then
						jlp_rhscur(jlp_lr(j))=jlp_rhs(jlp_lr(j))
						if(jlp_xpresent) jlp_rhsw(jlp_lr(j))=jlp_rhscur(jlp_lr(j))-jlp_xps(jlp_lr(j))
						jlp_x(jlp_lr(j))=jlp_x(jlp_lr(j))+jlp_rhs(jlp_lr(j))-jlp_rhs2(jlp_lr(j))
						jlp_tmax=jlp_x(jlp_lr(j))/r(jlp_lr(j)) ;rcur=r(jlp_lr(j))
						jlp_lower(jlp_lr(j))=.true.
		!if(pp)write(n16,*)'post,negr,lbou,NOTlower,lr(j),tmax',j_lr(j),j_tmax
						leavec=jlp_lr(j)
					elseif(jlp_x(jlp_lr(j)).le.0.d0.and.jlp_lower(jlp_lr(j)))then !if(.not.j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs(j_lr(j))-j_rhscur(j_lr(j)).gt.r(j_lr(j))*j_tmax)then

						if(jlp_x(jlp_lr(j)).gt.jlp_tmax*r(jlp_lr(j)) )then  !-x/-r <tmax-tole -x<-r*(tmax-tole)
							jlp_tmax=jlp_x(jlp_lr(j))/r(jlp_lr(j)) ;rcur=r(jlp_lr(j))
			!if(pp)write(n16,*)'post,negr,ubou,lower,lr(j),tmax',j_lr(j),j_tmax
							leavec=jlp_lr(j)
						end if !if(j_x(j_lr(j)).gt.j_tmax*r(j_lr(j)) )then

					elseif(jlp_x(jlp_lr(j)).gt.0.d0.and.jlp_lower(jlp_lr(j)).and.jlp_feasible)then !if(.not.j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs(j_lr(j))-j_rhscur(j_lr(j)).gt.r(j_lr(j))*j_tmax)then

						if(jlp_x(jlp_lr(j)).gt.jlp_tiny78)write(6,*)'row ',jlp_lr(j),' illegal residual (+) ',jlp_x(jlp_lr(j)), &
						' pivot=',ipivot,&
							'r=',r(jlp_lr(j)),' ienter=',ienter
						if(p.or.p9)write(n16,*)'row ',jlp_lr(j),' illegal residual (+) ', &
						jlp_x(jlp_lr(j)),' pivot=',ipivot,&
							'r=',r(jlp_lr(j)),' ienter=',ienter
						leavec=jlp_lr(j)
		!p=.true.
						jlp_tmax=-1.d0 ;rcur=0.d0
						if(p.or.p9)write(n16,*)'posit resid jump*****r,x**', &
						jlp_lr(j),r(jlp_lr(j)),jlp_x(jlp_lr(j))
		!call isfeasible()
		!stop 873
						goto112233=.true.
						return
	!	goto 2244
					endif !if(.not.j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs(j_lr(j))-j_rhscur(j_lr(j)).gt.r(j_lr(j))*j_tmax)then

				end if !if(j_lbou(j_lr(j)))then

			end if !if(r(j_lr(j)).gt.j_tiny78)then

		end do !do j=1,lr0

! if(leaveres.ne.0)leaving=1
! leaving= 1  residaul leaves
! leaving= 2 z leaves
! leaving=3  w leaves
! leaving=4 key leaves
! leaving=5 xkf leaves

		if(leavec.gt.0)then  !!!!!
			leave=jlp_lsi(leavec)  ! is this needed
!if(leavec.le.j_nrow)j_tmax=j_tmax   !!!!
			if(p.and.jlp_fpresent)write(n16,*)'**fact** KANNASTA lähdössä <6344> >> srk'
!if(pp)write(n16,*)'leavec,leave,tmax,x,r',leavec,leave,j_tmax,j_x(leavec), &
!r(j_ls(leave)),j_ls(j),j_xmi(j_ls(leave)),j_xma(j_ls(leave))
			if(p)	write(n16,*)'xma,xmi',jlp_xma(jlp_ls(leave)),jlp_xmi(jlp_ls(leave))
			if (p.and.jlp_fpresent.and.(leavec > (jlp_nrowz+jlp_mxd))) then
				write(n16,'(a50,i3,i3,i3,a25,a25)')'**fact** kannasta lähdössä <6349> ' ,&
				'>> xkf: unit,ixkyk,ifact,xkf,fact', &
					jlp_lunit(leavec-jlp_nrowz),jlp_ixkf(leavec-jlp_nrowz),jlp_ixkffact(leavec-jlp_nrowz), &
					j_vname(j_o(jlp_ivxkyk)%i2(jlp_ixkf(leavec-jlp_nrowz))), &
					j_vname(j_o(jlp_ivfact)%i2(jlp_ixkffact(leavec-jlp_nrowz)))
			endif !if (p.and.j_fpresent.and.(leavec > (j_nrowz+j_mxd))) then

		else !if(leavec.gt.0)then
			if(p)write(n16,*)'*no basic variable leaving'  !!!!
		endif !if(leavec.gt.0)then
!if(pp)write(16,*)'kkkoi'
8883	if(.not.jlp_xpresent)goto 8888  !!!!! check if key schedule is leaving

! *********************************************'

		lcur=jlp_next(0)   !link to first

		lcursame=0 !!!! place where to put new entering schdelu if the same unit
! newc -is coming column , newa in A, newd in D
! tmax gives max values for enetering var.

! update step
! determine leaving variable  c:new column, direct effect
! Bx=b  ,  Bx+tc=b
! x'=x- t*inv(B)*c=x-t*r
! key leaves, see p. 115-116
! if entering unit is not the same,
! sum(x')=1,  sum(x)-t*sum(inv(b)*c) = 1  ->t=(sum(x)-1)/sum(r)  * if (sum(x)-1<0
! -> only negative  sum(r) gives possible values, summation over cols in the same unit

! if the unit is the same as entering unit then
! sum(x')+t=1, sum(x)-t*sum(inv(b)*c)+t = 1->t=(sum(x)-1)/(sum(r)-1.)
! ei varauduttu vielä negatii viseen t:

! note elements of r have opposite sign as in old JLP
		jlp_wsu=jlp_onen
		jlp_rs=jlp_zero
		lcur0=lcur  ! link to first column in current unit
! lunit etc column number in A
!if(pp)write(n16,*)'units',j_lunit(j_next(0)),j_lunit(j_next(j_next(0))),j_lunit(j_next(j_next(j_next(0)))),&
!j_lunit( j_next(j_next(j_next(j_next(0)))))

		do i=1,jlp_ld0
			nex=jlp_next(lcur)        ! next  follows numbering in D

			jlp_rs=jlp_rs+r(lcur+jlp_nrowz)  ! has opposite sign than in old JLP
			jlp_wsu=jlp_wsu+jlp_x(lcur+jlp_nrowz)

			if(jlp_lunit(lcur).eq.iunit.and.lcursame.eq.0)lcursame=lcur
			if(jlp_lunit(nex).ne.jlp_lunit(lcur))then
! the next col has different unit, do final checking
! formulas  6.59  6.60
				if(jlp_lunit(lcur).eq.iunit.and.ienter.eq.3)then
	! the unit is the same as the unit of the enetering schedule
	!if(pp)	write(n16,*)'rs-1',j_rs,j_rs-1.
					jlp_rs=jlp_rs-jlp_one
	! store link to be used so that we can put the entering
	! column together with other columns isn the next-sequence
				endif !if(j_lunit(lcur).eq.iunit.and.ienter.eq.3)then

				if(jlp_rs.lt.jlp_tiny78n.and.jlp_wsu.gt.jlp_tmax*jlp_rs)then !!!!
	! wsu negat  wsu/rs<tmax  rs<0 =>wsu>tmax*rs
	!					t=wsu/rs
					jlp_tmax=jlp_wsu/jlp_rs    ;rcur=jlp_rs!!!!
					leavk=lcur0
	!if(pp)write(n16,*)'**tmax/key,wsu,rs',j_tmax,j_wsu,j_rs,' unit, lcur',j_lunit(lcur),lcur
	! leavk is the first D-column in unit for whcik key is leaving
	!					end if
				endif !if(j_rs.lt.j_tiny78n.and.j_wsu.gt.j_tmax*j_rs)then

				jlp_wsu=jlp_onen   !opposite sign of ws as in old JLP
				jlp_rs=jlp_zero
				lcur0=jlp_next(lcur)   ! lcur0 is again the first col in the next new unit
! last
			endif !if(j_lunit(nex).ne.j_lunit(lcur))then
			lcur=nex
		enddo !do i=1,j_ld0
! ! end checking if key-schedule is leaving , if key is leving leavk.gt.
8889 continue   !!!!! we come here if w.gt.1

!!!!!Avaintehdas vaihtuu, versio 2 (tarkastellaan vaihtuuko avaintehdas)
		if(jlp_fpresent) then
			leavkf=0

			leavkwf = 0
			lcursamef = 0

			iunit_lkf = 0
			ixkyk_lkf = 0

!käydään läpi vain kannassa olevat xkf-mjat

!tehdaskantasrkeiden läpikäynti nextf:n avulla
			do ixkyk_=1,j_o(jlp_ivxkyk)%i(1)

				lcur0_unit = jlp_nextf(jlp_mxd,ixkyk_)
				do while (lcur0_unit > jlp_mxd)

					lunit_ = jlp_lunit(lcur0_unit)

	!xkyk-mjan paikka x-matriisissa
					ikeepxkyk_ = jlp_ixkykkeep(ixkyk_)
	!if(pp)write(16,*)'ioo',lunit_,j_ibaunit(lunit_)+ j_keys(lunit_)
					iba1=jxmatiba(jlp_ibaunit(lunit_)+ jlp_keys(lunit_)) !,1)

					jlp_wsu=j_o(ivxmat)%d(iba1+ikeepxkyk_) !jxmatiba
					if(p)write(16,*)'h1',jlp_wsu
					jlp_rs=jlp_zero

					if ((ienter==4).and.(iunit==lunit_).and.(ixkyk_==ixkykenter)) then
						jlp_rs=jlp_rs+jlp_one
						lcursamef = lcur0_unit

						if(p) write(n16,*)'**fact** <6623> kantasrk:een xkf tulossa kantaan: iunit,ixkyk,lcursamef',&
							lunit_,ixkyk_,lcursamef
					endif !if ((ienter==4).and.(iunit==lunit_).and.(ixkyk_==ixkykenter)) then

					lcur_=lcur0_unit

					do while (jlp_lunit(lcur_) == jlp_lunit(lcur0_unit))
						jlp_rs = jlp_rs - r(lcur_+jlp_nrowz)  ! has opposite sign than in old JLP
						jlp_wsu = jlp_wsu - jlp_x(lcur_+jlp_nrowz)
						if(p)write(16,*)'h12',jlp_wsu
						lcur_ = jlp_nextf(lcur_,ixkyk_)
					enddo !do while (j_lunit(lcur_) == j_lunit(lcur0_unit))

	! s.12 kaava (wij enters)
					if ((ienter==3).and.(iunit==lunit_)) then
						ikey_ = jlp_ibaunit(lunit_) + jlp_keys(lunit_)
						iopt_ = jlp_ibaunit(lunit_) + iopt

						ibakey_=jxmatiba(ikey_) !,2)
						ibaopt_=jxmatiba(iopt_) !,1)
						if(p.or.jlp_tmax.eq.-1.d0) write(n16,*)'**fact** <6741> wij tulossa kantaan: iunit,iopt,ikey',&
							iunit,iopt,ikey_
		!
		!		j_o(ivxmat)%r(jxmatiba(ikey_)+ikeepxkyk_)

						jlp_rs=jlp_rs - &
							(j_o(ivxmat)%d(ibaopt_+ikeepxkyk_) -  & !jxmatiba
							j_o(ivxmat)%d(ibakey_+ikeepxkyk_))          !!jxmatiba

					endif !if ((ienter==3).and.(iunit==lunit_)) then

	! tehdaskantasarakketta vastaavan yksikön 1. ve-kantasrk
					lcur_w= 0
					lun_= 1
					do while((jlp_lunit(jlp_lunw(lun_))/=lunit_).and.(lun_<=jlp_lunits0))
						lun_=lun_+1
					enddo !do while((j_lunit(j_lunw(lun_))/=lunit_).and.(lun_<=j_lunits0))
	!kannassa yksikön ve-srkeita
					lcur0_w = 0
					if(lun_ <= jlp_lunits0) then

						lcur0_w = jlp_lunw(lun_)
						ikey_	= jlp_ibaunit(jlp_lunit(lcur0_w)) + jlp_keys(jlp_lunit(lcur0_w))

						ibakey_=jxmatiba(ikey_)! ,2)

						lcur_w = lcur0_w
						do while (jlp_lunit(lcur_w) == jlp_lunit(lcur0_w))
							iobs_ = jlp_ibaunit(jlp_lunit(lcur0_w)) + jlp_isch(lcur_w)

							ibaobs_=jxmatiba(iobs_) !,1)
							if(p.or.jlp_tmax.eq.-1.d0)write(16,*)'tas',ikey_, &
							j_o(ivxmat)%d(ibakey_+1:ibakey_+keepx)
							if(p)write(16,*)'ta2',iobs_,j_o(ivxmat)%d(ibaobs_+1:ibaobs_+keepx)
							xkij_=j_o(ivxmat)%d(ibaobs_+ikeepxkyk_)   !jxmatiba


							jlp_rs=jlp_rs+r(lcur_w+jlp_nrowz)*&
								(xkij_ - j_o(ivxmat)%d(ibakey_+ikeepxkyk_))
							jlp_wsu=jlp_wsu+jlp_x(lcur_w+jlp_nrowz)*&
								(xkij_ - j_o(ivxmat)%d(ibakey_+ikeepxkyk_))
							if(p)write(16,*)'h3',jlp_wsu
			!write(nu20,*)'hep2',j_o(ivxmat)%r(ibakey_+1:ibakey_+10)
			!write(nu20,*)j_o(ivxmat)%r(ibakey_+ikeepxkyk_)
							lcur_w = jlp_next(lcur_w)
						enddo !do while (j_lunit(lcur_w) == j_lunit(lcur0_w))

					endif !if(lun_ <= j_lunits0) then

					if (p) then
						if(jlp_wsu<0.d0)write(n16,*)'**fact** <6738> negat. wsu'
					endif !if (p) then
	!wsu-rs kombinaatiot
	! wsu >0, rs >0 -> t = wsu/rs
	! wsu >0, rs <0 -> t < 0 --> ei käsitellä
	! wsu <0, rs >0 -> t < 0 --> ei käsitellä,  käsitelläänpä JL 27.8.2018
	! wsu <0, rs <0 -> t = wsu/rs
	! rs = 0 -> ei käsitellä
	! wsu = 0, rs >0 -> t = wsu/rs = 0 --> tmax = 0
	! wsu = 0, rs <0 -> t = wsu/rs = 0 --> ei käsitellä
	!	if(j_lunit(lcur0_unit).eq.121.and.ixkyk_.eq.16)write(26,*)&
		!	j_wsu,j_rs,j_wsu/j_rs,j_tmax,j_tiny78,j_rs*j_wsu>j_zero,&
		!	(Abs(j_rs)>j_tiny78).and.(((j_rs*j_wsu)>j_zero).or.(j_wsu.eq.j_zero.and.j_rs>j_zero)).and.&
		!		Abs(j_wsu).lt.j_tmax*Abs(j_rs)
					if(p.or.jlp_tmax.eq.-1.d0) then
						write(n16,*)'<7681> wsu,rs,wsu/rs,tmax,lcur0_unit,lcur0_w,lunit(lcur0_unit),ixkyk_',&
							jlp_wsu,jlp_rs,jlp_wsu/jlp_rs,jlp_tmax,lcur0_unit, &
							lcur0_w,jlp_lunit(lcur0_unit),ixkyk_
					endif !if(p.or.j_tmax.eq.-1.d0) then
	!	if((Abs(j_rs)>j_tiny78).and.(((j_rs*j_wsu)>j_zero).or.(j_wsu.eq.j_zero.and.j_rs>j_zero)).and.&
		!		Abs(j_wsu).lt.j_tmax*Abs(j_rs))then
					if(jlp_rs>jlp_tiny78.and.jlp_wsu.lt.jlp_tmax*jlp_rs)then !changed 27.8.2018 JL
						jlp_tmax=jlp_wsu/jlp_rs ;rcur=jlp_rs
						leavkf = lcur0_unit    ! ehkä kannassa olevat xkf-muuttujat ko yksikölle

						leavkwf = lcur0_w      ! kannassa olevat vaihtoehtosarakkeet ko yksikölle
		!	write(nu20,*)'<22>',leavkwf
						leave=0
						leavk=0
						iunit_lkf = jlp_lunit(lcur0_unit)
						ixkyk_lkf = ixkyk_
						ikeepx_lkf = ikeepxkyk_
						if(p.or.jlp_tmax.eq.-1.d0) write(n16,*)'**fact** <6678> **tmax/key,wsu,rs',&
							jlp_tmax,jlp_wsu,jlp_rs,'leavkf, unit, ixkyk',leavkf, &
							jlp_lunit(leavkf), ixkyk_
					endif !if(j_rs>j_tiny78.and.j_wsu.lt.j_tmax*j_rs)then

					lcur0_unit = lcur_
				enddo !do while (lcur0_unit > j_mxd)
			enddo !do ixkyk_=1,j_o(j_ivxkyk)%i(1)

			if(jlp_tmax.le.jlp_tmaxmin)then ! eq.0.d0)then
				if(ienter.eq.3.and.secondb.gt.jlp_valuek+jlp_tolecur)then
					if(pp.or.jlp_tmax.eq.-1.d0)write(16,*)'secondb2 ',secondb,jlp_valueopt, &
					jlp_valuek,ipivot,iunit
					iopt=isecond
					jlp_valueopt=secondb
					secondb=jlp_small
					goto222=.true.;return
				endif !if(ienter.eq.3.and.secondb.gt.j_valuek+j_tolecur)then
			endif !if(j_tmax.le.j_tmaxmin)then

			if ((ienter==4).and.(lcursamef==0)) then
				ikeepxkykenter_ = jlp_ixkykkeep(ixkykenter)

				ibas1=jxmatiba(jlp_ibaunit(iunit)+ jlp_keys(iunit)) !,1)
				jlp_wsu=j_o(ivxmat)%d(ibas1+ikeepxkykenter_)  !jxmatiba
				if(p.or.jlp_tmax.eq.-1.d0)write(16,*)'h4',jlp_wsu
				jlp_rs=jlp_one

! kantaan tulevan yksikön 1. ve-srk
				lcur_= 0
				lun_= 1
				do while((jlp_lunit(jlp_lunw(lun_))/=iunit).and.(lun_<=jlp_lunits0))
					lun_=lun_+1
				enddo !do while((j_lunit(j_lunw(lun_))/=iunit).and.(lun_<=j_lunits0))

				lcur0_w = 0
!kannassa yksikön ve-srkeita
				if(lun_ <= jlp_lunits0) then

					lcur0_w = jlp_lunw(lun_)
					ikey_	= jlp_ibaunit(jlp_lunit(lcur0_w)) + jlp_keys(jlp_lunit(lcur0_w))
					ibaxkey_=jxmatiba(ikey_) !,1)

					ibakey_=jxmatiba(ikey_) !,2)
					lcur_ = lcur0_w
					do while (jlp_lunit(lcur_) == jlp_lunit(lcur0_w))
						iobs_ = jlp_ibaunit(jlp_lunit(lcur0_w)) + jlp_isch(lcur_)
						ibaobs_=jxmatiba(iobs_) !,1)
						xkij_=j_o(ivxmat)%d(ibaobs_+ikeepxkykenter_) !jxmatiba
						jlp_rs=jlp_rs+r(lcur_+jlp_nrowz)*&
							(xkij_ - j_o(ivxmat)%d(ibakey_+ikeepxkykenter_))  !jxmatiba
						jlp_wsu=jlp_wsu+jlp_x(lcur_+jlp_nrowz)*&
							(xkij_ - j_o(ivxmat)%d(ibakey_+ikeepxkykenter_))  !jxmatiba
						if(p)write(16,*)'h5',jlp_wsu
						lcur_ = jlp_next(lcur_)
					enddo !do while (j_lunit(lcur_) == j_lunit(lcur0_w))

				endif !if(lun_ <= j_lunits0) then

				if(p)write(n16,*)'**fact** <6695> (lcursamef==0) wsu,rs', jlp_wsu,jlp_rs
				jlp_degeneratef=(jlp_wsu==jlp_zero)
				if (p.and.jlp_degeneratef) write(n16,*)'**fact** <6733> degenerate = true'

				if((jlp_rs>jlp_tiny78).and.(jlp_wsu.lt.jlp_tmax*jlp_rs))then
					jlp_tmax=jlp_wsu/jlp_rs  ;rcur=jlp_rs
					leavkf = 0
					leavkwf = 0
					leave=0
					leavk=0
					if(p.or.jlp_tmax.eq.-1.d0) write(n16,*)'**fact** <6743> **tmax/ei_xkf,wsu,rs',&
						jlp_tmax,jlp_wsu,jlp_rs,'leavkf, unit, ixkyk',leavkf, iunit, ixkykenter
				endif !if((j_rs>j_tiny78).and.(j_wsu.lt.j_tmax*j_rs))then

			endif !if ((ienter==4).and.(lcursamef==0)) then

			if ((ienter==4).and.(leavkf==0).and.(leave==0).and.(leavk==0)) then
				lun_= 1
				do while((jlp_lunit(jlp_lunw(lun_))/=iunit).and.(lun_<=jlp_lunits0))
					lun_=lun_+1
				enddo !do while((j_lunit(j_lunw(lun_))/=iunit).and.(lun_<=j_lunits0))
				if (lun_<=jlp_lunits0) then
					leavkwf = jlp_lunw(lun_)
					iunit_lkf=iunit
					ixkyk_lkf = ixkykenter

					ikeepx_lkf = jlp_ixkykkeep(ixkykenter)
					if(p) then
						write(n16,*) '<7325> Muuta ei näytä tapahtuvan, vaihdetaan avaintehdas'
						if (leavk >0) write(n16,*) '<7325> ....paitsi että  myös avainve vaihtuu...'
					endif !if(p) then
				endif !if (lun_<=j_lunits0) then
			endif !if ((ienter==4).and.(leavkf==0).and.(leave==0).and.(leavk==0)) then
		endif !if(j_fpresent) then

		if(leavk.gt.0)then
			nkeys=nkeys+1
			if(p.and.jlp_fpresent) write(n16,*)'**fact** AVAINVE VAIHTUU>> '
	!if(pp)write(n16,*)&
	!'**leaving key,d-column,unit,neun',leavk,j_lunit(leavk),iunit

	! put to next free column
	! old d= x-xkey, x=d+xkey
	! new d= x-xnewkey=d+xoldkey-xnewkey

	! lcur0 column of the
	!  change first xps:

	! subtract old key add new key
	! leyvk-columd in D is newkey-oldkey, the corresponding element in objr contains
	! also newkey-oldkey
	! xps is sum of x:s of keys
	! newxps=oldxps-oldkey+newkey= oldxps+(newkey-oldkey)
	! and newkey-oldkey is already stored in a and objr

			jlp_xps(0)=jlp_xps(0)+jlp_objr0(leavk+jlp_nrowz)
	! leavk column contains the inforamtion
			if(sparse)then
				icol=leavk+nz
				iel=0
				do i=jlp_lavecsp(jlp_lavecsp(0)+icol),last(icol)
					iel=iel+1
					j=jlp_lavecsp(i)
					jlp_xps(j)=jlp_xps(j)+jlp_a(iel,icol)
					jlp_rhsw(j)=jlp_rhscur(j)-jlp_xps(j)
				enddo !do i=j_lavecsp(j_lavecsp(0)+icol),last(icol)
			else !if(sparse)then
				do j=1,jlp_nrow
					if((jlp_ix(j).ne.0).or.jlp_fpresent)jlp_xps(j)=jlp_xps(j)+jlp_a(j,leavk+nz) !leavk col in D
					jlp_rhsw(j)=jlp_rhscur(j)-jlp_xps(j)
				enddo !do j=1,j_nrow

			endif !if(sparse)then

			if(p2)write(n16,*)(jlp_xps(j),j=0,jlp_nrow)

	! if the entering schedule is in the same unit then also the entering
			if(iunit.eq.jlp_lunit(leavk).and.ienter.eq.3)then
	! in objr also the residual section and z section are included
				jlp_objr0(newc)=jlp_objr0(newc)-jlp_objr0(leavk+jlp_nrowz)
				if(sparse)then
					call jlpgetcol(newa)
					call jlpgetcol(leavk+nz)
					call jlpsubcol(newa,leavk+nz,newa)

				else !if(sparse)then

					do j=1,jlp_nrow
						jlp_a(j,newa)=jlp_a(j,newa)-jlp_a(j,leavk+nz)
					enddo !do j=1,j_nrow
				endif !if(sparse)then
			endif !if(iunit.eq.j_lunit(leavk).and.ienter.eq.3)then

	! check

	! if there are more columns in the same unit these must be changed with
	! columns expressed in the new key

	! old d= x-key, x=d+key
	! new d= x-newkey=d+key-newkey
	! column leave is actually : newkey-key, thus new d is:
	! d- column leave
			leavec=leavk+jlp_nrowz  ! in col numbering
			lcur=leavk
			if(p)write(n16,*)'yks old key,new ', jlp_keys(jlp_lunit(lcur)),&
				jlp_isch(leavk)
			jlp_keys(jlp_lunit(lcur))=jlp_isch(leavk)
			nkeys=nkeys+1
			if(jlp_testl)then
				if(jlp_keys(jlp_lunit(lcur)).gt.jlp_nsch(jlp_lunit(lcur)))then
					write(n16,*)'*u*',jlp_lunit(lcur),jlp_nsch(jlp_lunit(lcur)), &
					'yrität',jlp_keys(jlp_lunit(lcur))
					close(16)
					write(6,*)'*u* return'
					goto900=.true.;return
				endif !if(j_keys(j_lunit(lcur)).gt.j_nsch(j_lunit(lcur)))then
			endif !if(j_testl)then

	! intitially leavk is first column (in nex-order) in the unit

	17 nex=jlp_next(lcur)    ! in d columns
	! we jump here from below to go through all columns in the same unit
	! is=nrow
	! first free column is reserved for newc
			if(jlp_lunit(lcur).eq.jlp_lunit(nex))then
				if(p) then
					write(n16,*)'samma ,nex,',nex,jlp_lunit(lcur)
					write(n16,*)'nex',(jlp_next(jj7),jj7=0,5),'lunit',(jlp_lunit(jj7),jj7=0,5)
					write(n16,*)'prev',(jlp_iprev(jj7),jj7=0,5)
				endif !if(p) then
	! is=is+1
	! change the old additional  columns to correspond
	! to the new key
				id=jlp_ld(jlp_ld0+2) !take new D-column for this
	! note ld0+1 may be used already if schedule is entering
				ia=id+nz  ! ld0 nrow+1 already reserved for entering, ia=col in A
				newc2=ia+jlp_nrow  !newc2 col number in (I A)
				if(p)write(n16,*)'newc,newc2',newc,newc2,'newid,newa',id,ia

				jlp_objr0(newc2)=jlp_objr0(nex+jlp_nrowz)-jlp_objr0(leavec)
				if(sparse)then
					call jlpsubcol(nex+nz,leavk+nz,ia)
					if(p)then
						call jlpgetcol(ia);write(n16,*)'**iacol',jlp_acol(1:min(40,jlp_nrow))

					endif !if(p)then
				else !if(sparse)then

					do i=1,jlp_nrow
						jlp_a(i,ia)=jlp_a(i,nex+nz)-jlp_a(i,leavk+nz)
					enddo !do i=1,j_nrow
					if(p)write(n16,*)'iacol',jlp_a(1:min(jlp_nrow,50),ia)
				endif !if(sparse)then
				if(p)write(n16,*)'pivot7',nex+jlp_nrowz,newc2, 'in d',nex,id
				jlp_lunit(id)=jlp_lunit(leavk)
				jlp_isch(id)=jlp_isch(nex)
				icolold=nex+jlp_nrowz
	!!Fletcher ??????????

	! if leaving colums is the same as mp it is utilizing old computation
	! leaving column is stored into  mp
	! if entering column is mq, pivot is also utilizing old computations
	! entering column is getting the new value

				if(sparse)then
					mqjjsp=-1   !new not  something known
					call pivotsp(nex+jlp_nrowz,newc2,jlp_nrow, &
						nm,jlp_a,jlp_lavecsp,e,wslu1,lwsll1,ifail,jlp_info)  !unix

				else !if(sparse)then
					mqjj=-1   !new not  something known

					if(p) then
						write(n16,*) '**Pivot <7484>  nex, nex+nrowz,newc2,nrow', nex, nex+jlp_nrowz,newc2,jlp_nrow
					endif !if(p) then
					if(nup.ge.nfreq)then
						krefac=krefac+1
						if(p)write(19,*)'*refact'
					endif !if(nup.ge.nfreq)then
					call pivot(nex+jlp_nrowz,newc2,jlp_nrow, &
						nm,jlp_a,jlp_lavec,e,wslu1,lwsll1,ifail,jlp_info)
				endif !if(sparse)then
				ipivot=ipivot+1
				if(p.or.p9)write(n16,*)'pivot<9646>',ipivot,1,jlp_objf,jlp_tmax
				if(p)write(77,*)1,ipivot,jlp_objf
				if(ifail.ne.0)then
					write(6,*)'****pivot failure (7) ,ifail=',ifail,' info=',jlp_info
					nrecover=nrecover+1
					if(nrecover.ge.10)then
						write(6,*)'*jlp* is mixed up, try different tole (e.g.10,100,1000)(or consult J. Lappi)'
						j_err=.true.
						goto900=.true.;return

					endif !if(nrecover.ge.10)then
					write(6,*)'***trying to recover'
					if(p)write(n16,*)'***trying to recover'
					kierv=kier
					iunitv=iunit
					jlp_feasible=.false.
					goto1234=.true.;return

				endif !if(ifail.ne.0)then
				if(sparse)then
					mpjjsp=-1
				else !if(sparse)then
		! mpjj=-1   ! old not something to remember
				endif !if(sparse)then

				if(zmatrix)then
					ipivoti_=ipivot/5000
					if(5000*ipivoti_.eq.ipivot)write(6,*)'**pivots, objf ',ipivot, jlp_objf

				endif !if(zmatrix)then
				if(ipivot.eq.idebug)then
					p=.not.p

					if(p)then

						nout=16
					else !if(p)then
						nout=0
					endif !if(p)then
					write(6,*)'<1>changing debuggging at pivot=',ipivot,' into ',p
					if(p.and.jlp_nrow.le.50)p2=.true.
					write(n16,*)'*debug*'
					jdebug=jdebug+1
					if(jdebug.le.ndebug)idebug=j_v(j_o(iob)%i(ibasdebug+jdebug))

				endif !if(ipivot.eq.idebug)then

				if(p)then
					write(n16,*)'**pivot=',ipivot
					if(sparse)then
						call jlpgetcol(nex+nz);write(n16,*)'leaving col',jlp_acol
						call jlpgetcol(ia);write(n16,*)'ent col',jlp_acol
					else !if(sparse)then
						write(n16,*)'lea col',(jlp_a(jj7,nex+nz),jj7=1,jlp_nrow)
						write(n16,*)'ent col',(jlp_a(jj7,ia),jj7=1,jlp_nrow)
					endif !if(sparse)then
				endif !if(p)then
	! update ls and ld lists
				call jlplex(jlp_ls,jlp_lsi(nex+jlp_nrowz),jlp_lsi(newc2),jlp_lsi)
				call jlplex(jlp_ld,jlp_ld0+2,jlp_ldi(nex),jlp_ldi)
				if(p2)then
					write(n16,*)'ldaf ',(jlp_ld(jj7),jj7=1,jlp_ld0)
					write(n16,*)'lsaf ',(jlp_ls(jj7),jj7=1,5)
				endif !if(p2)then
	! nex leaving id entering
	! nex           3           4           0           4           2           0
	! lunit           0          22          22          20          22           0

	! leavk
	! nex           3           4           0           4           2           0
	! lunit           0          22          22          20          22           0
				if(p)then
					write(n16,*)'before inserting ', id ,'next',jlp_next(0),jlp_next(jlp_next(0)),&
						jlp_next(jlp_next(jlp_next(0))),  jlp_next(jlp_next(jlp_next(jlp_next(0))))
					write(n16,*)'units',jlp_lunit(jlp_next(0)),jlp_lunit(jlp_next(jlp_next(0))),&
						jlp_lunit(jlp_next(jlp_next(jlp_next(0)))), jlp_lunit( jlp_next(jlp_next(jlp_next(jlp_next(0)))))
					write(n16,*)'iprev',jlp_iprev(0),jlp_iprev(jlp_iprev(0)),jlp_iprev(jlp_iprev(jlp_iprev(0))),&
						jlp_iprev(jlp_iprev(jlp_iprev(jlp_iprev(0))))
				endif !if(p)then
				jlp_next(jlp_iprev(nex))=id ! insert the new col in next-
				jlp_next(id)=jlp_next(nex)
				jlp_iprev(id)=jlp_iprev(nex)
				jlp_iprev(jlp_next(id))=id

				if(jlp_fpresent)then
		! Päivitetään laskentayksikön vaihtoehtojen alkamissarake lunw:hen
					do ilu_ = 1,jlp_lunits0
						if (jlp_lunw(ilu_)==nex) then
							jlp_lunw(ilu_)=id
						endif !if (j_lunw(ilu_)==nex) then
					enddo !do ilu_ = 1,j_lunits0
				endif !if(j_fpresent)then

				if(p)then
					write(n16,*)'aft next',jlp_next(0),jlp_next(jlp_next(0)), &
					jlp_next(jlp_next(jlp_next(0))),  jlp_next(jlp_next(jlp_next(jlp_next(0))))
					write(n16,*)'iprev',jlp_iprev(0),jlp_iprev(jlp_iprev(0)), &
					jlp_iprev(jlp_iprev(jlp_iprev(0))),jlp_iprev(jlp_iprev(jlp_iprev(jlp_iprev(0))))
				endif !if(p)then
				lcur=id
				goto 17
			endif !if(j_lunit(lcur).eq.j_lunit(nex))then

	! now just proceed as if leaving a-col leavk
	! leave is index for ls
			leave=jlp_lsi(leavk+jlp_nrowz)
			if(p)write(n16,*)'siis leaving, ind, inc ',leavk,leave
			leaved=leavk

		endif !if(leavk.gt.0)then

!Avaintehdas vaihtuu
		if (jlp_fpresent) then

!avainvaihtoehtovaihtuu, kannassa yksikön ve
!versio 2, a lasketaan suoraan s.9 kaavalla
!leavkwf= yksikön 1. vaihtoehtosarake kannassa
			if(leavkwf>0) then
				if(p) write(n16,*)'**fact** AVAINTEHDAS VAIHTUU, leavkwf > 0 >> '
				nkeyf=nkeyf+1

				nex=leavkwf ! yksikön ensimmäinen ve-kantasarake
				ikey_ = jlp_ibaunit(jlp_lunit(leavkwf))+jlp_keys(jlp_lunit(leavkwf))
				ibakey_=jxmatiba(ikey_) !,2)
				ibaxkey_=jxmatiba(ikey_) !,1)
! muunnokset vain tarvittaessa
				if ((nfy.gt.0).and.(jlp_lunit(leavkwf)/=iunitrans)) then
					do j=1,j_o(jlp_ivkeepc)%i(1)
						j_v(j_o(jlp_ivkeepc)%i2(j))=j_o(ivcmat)%d((jlp_lunit(leavkwf)-1)*j_o(jlp_ivkeepc)%i(1)+j)
					enddo !do j=1,j_o(j_ivkeepc)%i(1)
					do j=1,ntrans
						call dotrans(jlp_itransv(j),1)
						if(j_err)then
							write(6,*)'err for trans ',j
							stop 761
						endif !if(j_err)then
					enddo !do j=1,ntrans
					iunitrans=jlp_lunit(leavkwf)
				endif !if ((nfy.gt.0).and.(j_lunit(leavkwf)/=iunitrans)) then

! uusi avaintehdas
!leavkf=yksikön xkf sarkkeiden alku, =0 jos ei ole xkf muuttjia
				if (leavkf>jlp_mxd) then
	!kannasta lähtevästä
					ifnew_ = jlp_ixkffact(leavkf)
				else !if (leavkf>j_mxd) then
					ifnew_ = ifopt   !ifnew= uusi avaintehdas
				endif !if (leavkf>j_mxd) then

				if(p)then
					write(n16,*)'**fact** leavkwf,leavkf,unit,ikey_,',leavkwf,leavkf,jlp_lunit(leavkwf),ikey_
					write(n16,*)'**fact** ixkyk_lkf, ifnew',ixkyk_lkf, ifnew_
					if (leavkf > jlp_mxd ) then
						write(n16,*)'**fact** avaintehdas vaihtuu, leavkf >0'
						write(n16,*)'**fact** nykyinen avaintehdas iunit,ixkyk,if ', jlp_lunit(leavkf), &
							jlp_ixkf(leavkf), jlp_keyfact(jlp_lunit(leavkf),jlp_ixkf(leavkf))
						write(n16,*)'**fact** tuleva avaintehdas iunit,ixkyk,if ', jlp_lunit(leavkf), & 
						jlp_ixkf(leavkf), jlp_ixkffact(leavkf)
					else !if (leavkf > j_mxd ) then
						write(n16,*)'**fact** vain avaintehdas vaihtuu, leavkf == 0 '
						write(n16,*)'**fact** nykyinen avaintehdas iunit,ixkyk,if ', &
						iunit,ixkykenter,jlp_keyfact(iunit,ixkykenter)
						write(n16,*)'**fact** tuleva avaintehdas iunit,ixkyk,if ', &
						iunit,ixkykenter,ifopt
					endif !if (leavkf > j_mxd ) then
					write(n16,*)'**fact** ixkyk_lkf,ikeepx_lkf,ifnew_',ixkyk_lkf,ikeepx_lkf,ifnew_
					write(n16,*)'**fact** ',j_vname(j_o(jlp_ivxkyk)%i2(ixkyk_lkf)),&
						j_vname(j_o(jlp_ivkeepx)%i2(ikeepx_lkf)),j_vname(j_o(jlp_ivfact)%i2(ifnew_))
				endif !if(p)then

!yksikön ve-kantasarakkeet    ks kaava 12
				do while(jlp_lunit(nex)==jlp_lunit(leavkwf))
					id=jlp_ld(jlp_ld0+2) !take new D-column for this
					ia=id+nz  ! ld0 nrow+1 already reserved for entering, ia=col in A
					newc2=ia+jlp_nrow  !newc2 col number in (I A)
					if(p)write(n16,*)'**fact** leavkwf <6966> newc2',newc2,'newid,newa',id,ia

					iobs_ = jlp_ibaunit(jlp_lunit(leavkwf))+jlp_isch(nex) !saraketta vastaavan ve
					ibax=jxmatiba(iobs_) !,2)
					ibaobs_=jxmatiba(iobs_) !,1)
	!!x_ij_t = xmat(ix(j_),iobs_)
	!!x_iJ(i)_t = xmat(ix(j_),ikey_)
	!!alfa = coeffx(ibafx(irowj)+k)
	!!gamma = v(fyfactout(iv2xykypos_,iv3factpos_))

	!objr0:n päivitys
					if(jlp_ixcur(0).ne.0)then
						jlp_objr0(newc2)=jlp_xmat(jlp_ix(0)+ibax)-jlp_xmat(jlp_ix(0)+ibaxkey_)
					else !if(j_ixcur(0).ne.0)then
						jlp_objr0(newc2)=0.
					endif !if(j_ixcur(0).ne.0)then

	!j_ : lavennetut tehtävärivit
					do jj=1,jlp_nrow
						if(jlp_ixcur(jj).ne.0)then
							jlp_a(jj,ia)=jlp_xmat(jlp_ix(jj)+ibax)-jlp_xmat(jlp_ix(jj)+ibaxkey_)
						else !if(j_ixcur(j_).ne.0)then
							jlp_a(jj,ia)=0.
						endif !if(j_ixcur(jj).ne.0)then
					enddo !do j_=1,j_nrow

	!tehtävän tehdas xk-muuttujat
	!jcurix : lavennetut tehtävärivit
	!irowj_ : alkup. tehtävärivit
					do jj=1,jlp_nrowpf ! #tehdasmjarivit
						jcurix=jlp_ifxcurrows(jj)
						irowj_=jlp_irowrow(jcurix)

		! tehtävärivin xk-mjat
						do k_=1,jlp_nfxrow(irowj_)
							ixkyk_=jlp_irowfxvars(jlp_ibafx(irowj_)+k_)
							if (p) then
							endif !if (p) then

							if ((jlp_lunit(leavkwf)==iunit_lkf).and.(ixkyk_==ixkyk_lkf)) then
				! xk-mja, jonka avaintehdas vaihtumassa yksikössä
								keyf_ = ifnew_
							else !if ((j_lunit(leavkwf)==iunit_lkf).and.(ixkyk_==ixkyk_lkf)) then
				! avaintehdas ei vaihdu
								keyf_ = jlp_keyfact(iunit_lkf,ixkyk_)
							endif !if ((j_lunit(leavkwf)==iunit_lkf).and.(ixkyk_==ixkyk_lkf)) then

			! lisätään  avaintehtaan (alfa*(x_ij_k - x_iJ(i)_k))
							if(keyf_==jlp_irowffact(jlp_ibafx(irowj_)+k_)) then
				! jcurix 0 / muut rivit
								if (jcurix==0) then
									jlp_objr0(newc2)=jlp_objr0(newc2) + &
									jlp_coeffx(jlp_ibafx(irowj_)+k_)* &
										(j_o(ivxmat)%d(ibaobs_ &
										+jlp_irowfkeep(jlp_ibafx(irowj_)+k_)) - &
										j_o(ivxmat)%d(ibakey_+&   !jxmatiba
										jlp_irowfkeep(jlp_ibafx(irowj_)+ k_)))
								else !if (jcurix==0) then
									jlp_a(jcurix,ia)=jlp_a(jcurix,ia) + &
									jlp_coeffx(jlp_ibafx(irowj_)+k_)* &
										(j_o(ivxmat)%d(ibaobs_&
										+jlp_irowfkeep(jlp_ibafx(irowj_)+k_)) -&
										j_o(ivxmat)%d(ibakey_+&   !jxmatiba
										jlp_irowfkeep(jlp_ibafx(irowj_)+k_)))

								endif !if (jcurix==0) then
							endif !if(keyf_==j_irowffact(j_ibafx(irowj_)+k_)) then

						enddo !do k_=1,j_nfxrow(irowj_)

					enddo !do jj=1,j_nrowpf

	!tehtävän tehdas yk-muuttujat
	!jcurix : lavennetut tehtävärivit
	!irowj_ : alkup. tehtävärivit
					do jj=1,jlp_nrowpfy
						jcurix=jlp_ifycurrows(jj)
						irowj_ = jlp_irowrow(jcurix)
						do k_=1,jlp_nfyrow(irowj_)
							listy=jlp_irowfyvars(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava ptl-lista
							listf=jlp_irowfyfact(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava tehdas-lista
							do ivars_=1,j_o(listy)%i(1) ! yk-mjan puutavaralistan muuttujat

								iv2elpos_ = jlp_ifyvarskeep(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan xmat-sarake
								iv2xkykpos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan paikka xkyk-listassa

								if ((jlp_lunit(leavkwf)==iunit_lkf).and.(iv2xkykpos_==ixkyk_lkf)) then
									keyf_ = ifnew_
								else !if ((j_lunit(leavkwf)==iunit_lkf).and.(iv2xkykpos_==ixkyk_lkf)) then
									keyf_ = jlp_keyfact(iunit_lkf,iv2xkykpos_)
								endif !if ((j_lunit(leavkwf)==iunit_lkf).and.(iv2xkykpos_==ixkyk_lkf)) then

								do ifact_=1,j_o(listf)%i(1) ! yk-mjan tehdaslistan tehtaat
									iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj_)+k_)+ifact_-1) !tehtaan paikka factories-listassa

					! lisätään  avaintehtaan (gamma*(x_ij_k - x_iJ(i)_k))
									if(keyf_.eq.iv3factpos_) then
						!jcurix 0 / muut rivit
										if (jcurix==0) then

											jlp_objr0(newc2)=jlp_objr0(newc2) + j_v(jlp_fyfactout(iv2xkykpos_,iv3factpos_)) *  &
												(j_o(ivxmat)%d(ibaobs_+iv2elpos_) - &   !jxmatiba
												j_o(ivxmat)%d(ibakey_+iv2elpos_))          !jxmatiba
										else !if (jcurix==0) then
											jlp_a(jcurix,ia)=jlp_a(jcurix,ia) + j_v(jlp_fyfactout(iv2xkykpos_,iv3factpos_)) *  &
												(j_o(ivxmat)%d(ibaobs_+iv2elpos_) - &  !jxmatiba
												j_o(ivxmat)%d(ibakey_+iv2elpos_))  !jxmatiba
										endif !if (jcurix==0) then
									endif !if(keyf_.eq.iv3factpos_) then

								enddo !do ifact_=1,j_o(listf)%i(1)
							enddo !do ivars_=1,j_o(listy)%i(1)
						enddo !do k_=1,j_nfyrow(irowj_)
					enddo !do jj=1,j_nrowpfy

					jlp_lunit(id)=jlp_lunit(leavkwf)
					jlp_isch(id)=jlp_isch(nex)
					icolold=nex+jlp_nrowz

					if(p)then
						write(n16,*)'**fact** leavkwf <7063> **ennen pivot=',ipivot
						write(n16,*)'lea col',(jlp_a(jj7,nex+nz),jj7=1,jlp_nrow)
						write(n16,*)'ent col',(jlp_a(jj7,ia),jj7=1,jlp_nrow)
						write(n16,*)'ld',(jlp_ld(jj7),jj7=1,jlp_ld0)
						write(n16,*)'lf',(jlp_lf(jj7),jj7=jlp_mxd+1,jlp_lf0)
						write(n16,*)'lr',(jlp_lr(jj7),jj7=1,lr0)
						write(n16,*)'nex',nex
						write(n16,*)'newc2',newc2  !newc2=colnum in (I A)
					endif !if(p)then

					mqjj=-1   !new not  something known

					if(p) then
						write(n16,*) '**Pivot <7778> nex,ia, nex+nrowz,newc2,nrow', &
						nex,ia,nex+jlp_nrowz,newc2,jlp_nrow
					endif !if(p) then
					if(nup.ge.nfreq)then
						krefac=krefac+1
						if(p)write(19,*)'*refact'
					endif !if(nup.ge.nfreq)then
	! tarkistetaan onko tuleva vektori riippuvainen jäävistä, jottei tulee lin riippuva systeemi !!!!!

					listapu(1)=nex+jlp_nrowz

					call fbsub(jlp_nrow,1,1,jlp_a,jlp_lavec,0,jlp_a(1:,ia),jlp_x,listapu,wslu1,lwsll1,.false.)

					if(abs(jlp_x(nex+jlp_nrowz)).lt.jlp_tiny78)then
						nkeyfactw=nkeyfactw+1
						if(nkeyfactw.le.7)write(6,*)'key factory cannot be changed, pivot=',ipivot

						if(p9)then
							write(16,*)'key factory cannot be changed',iunit,ixkykenter,leavkf,ifopt,justkey,jlp_valueopt,&
								jlp_valuek
							call fbsub(jlp_nrow,1,jlp_nrow,jlp_a,jlp_lavec,0,jlp_a(1:,ia),jlp_x,jlp_ls,wslu1,lwsll1,.false.)
							write(n16,*)(jlp_ls(jj),jlp_x(jlp_ls(jj)),jj=1,jlp_nrow)
						endif !if(p9)then
						goto55=.true.;return
					endif !if(abs(j_x(nex+j_nrowz)).lt.j_tiny78)then

					call pivot(nex+jlp_nrowz,newc2,jlp_nrow, &
						nm,jlp_a,jlp_lavec,e,wslu1,lwsll1,ifail,jlp_info)
					ipivot=ipivot+1
					nkeyf=nkeyf+1
					route67=.true.
					if(p7)write(77,*)2,ipivot,jlp_objf
					if(p.or.p9)write(n16,*)'<67>',ipivot,jlp_objf,ienter,iunit,jlp_tmax,&
						jlp_valueopt,jlp_valuek,jlp_x(nex+jlp_nrowz),r(nex+jlp_nrowz),ixkykenter,leavkf,ifopt,newc2-jlp_nrow
					if(p9)write(16,*)jlp_keyfact(iunit,max(ixkykenter,1))

					if(ipivot.eq.idebug)then
						p=.not.p
						if(p)then

							nout=16
						else !if(p)then
							nout=0
						endif !if(p)then
						write(6,*)'<2>changing debuggging at pivot=',ipivot,' into ',p
						if(p.and.jlp_nrow.le.50)p2=.true.
						write(n16,*)'*debug*'
						jdebug=jdebug+1
						if(jdebug.le.ndebug)idebug=j_v(j_o(iob)%i(ibasdebug+jdebug))
					endif !if(ipivot.eq.idebug)then

					if(ifail.ne.0)then
						write(6,*)'**failure in pivot (fact) ,ifail=',ifail,' info=',jlp_info
						write(6,*)'pivots',ipivot,' r '

						if(ifail.eq.0)then
							goto55=.true.;return
						endif !if(ifail.eq.0)then
						if(p9)then
			!						call testkeyfact()
							write(n16,*)'leaving col ',jlp_a(1:jlp_nrow,nex+jlp_nrowz)
							write(n16,*)'enterin col ',jlp_a(1:jlp_nrow,newc2)
							write(n16,*)'**failure in pivot (fact) ,ifail=',ifail,' info=',jlp_info
							write(n16,*)'**fact** leavkwf,leavkf,unit,ikey_,',leavkwf,leavkf,jlp_lunit(leavkwf),ikey_
							write(n16,*)'**fact** ixkyk_lkf, ifnew',ixkyk_lkf, ifnew_
							if (leavkf > jlp_mxd ) then
								write(n16,*)'**fact** avaintehdas vaihtuu, leavkf >0'
								write(n16,*)'**fact** nykyinen avaintehdas iunit,ixkyk,if ', &
								jlp_lunit(leavkf), &
									jlp_ixkf(leavkf), jlp_keyfact(jlp_lunit(leavkf),jlp_ixkf(leavkf))
								write(n16,*)'**fact** tuleva avaintehdas iunit,ixkyk,if ', jlp_lunit(leavkf), &
								jlp_ixkf(leavkf), jlp_ixkffact(leavkf)
							else !if (leavkf > j_mxd ) then
								write(n16,*)'**fact** vain avaintehdas vaihtuu, leavkf == 0 '
								write(n16,*)'**fact** nykyinen avaintehdas iunit,ixkyk,if ', iunit,ixkykenter,jlp_keyfact(iunit,ixkykenter)
								write(n16,*)'**fact** tuleva avaintehdas iunit,ixkyk,if ', iunit,ixkykenter,ifopt
							endif !if (leavkf > j_mxd ) then
							write(n16,*)'**fact** ixkyk_lkf,ikeepx_lkf,ifnew_',ixkyk_lkf,ikeepx_lkf,ifnew_
							write(n16,*)'**fact** ',j_vname(j_o(jlp_ivxkyk)%i2(ixkyk_lkf)),&
								j_vname(j_o(jlp_ivkeepx)%i2(ikeepx_lkf)),j_vname(j_o(jlp_ivfact)%i2(ifnew_))
						endif !if(p9)then
						nrecover=nrecover+1
						if(nrecover.ge.10)then
							write(6,*)'*jlp* is mixed up, try different tole (e.g.10,100,1000)(or consult J. Lappi)'
							j_err=.true.
							goto900=.true.;return

						endif !if(nrecover.ge.10)then
						write(6,*)'***trying to recover'
						if(p)write(n16,*)'***trying to recover'
						kierv=kier
						iunitv=iunit
						jlp_feasible=.false.

						if(p.or.p9)then
							write(n16,*)'**fact** pivot fail: kannan xkf -sarakkeet '
							do jj=jlp_mxd+1,jlp_lf0
								write(n16,*) jj,' : ',(jlp_a(jj7,jj),jj7=1,jlp_nrow)
							enddo !do jj=j_mxd+1,j_lf0

							write(n16,*)'**fact** pivot fail: kannan lf() -sarakkeet '
							do jj=jlp_mxd+1,jlp_lf0
								write(n16,*) jlp_lf(jj),' : ',(jlp_a(jj7,jlp_lf(jj)),jj7=1,jlp_nrow)
							enddo !do jj=j_mxd+1,j_lf0
							write(n16,*)'**fact** pivot fail: kannan ld() -sarakkeet '
							do jj=1,jlp_ld0
								write(n16,*) jlp_ld(jj),' : ',(jlp_a(jj7,jlp_ld(jj)),jj7=1,jlp_nrow)
							enddo !do jj=1,j_ld0
						endif !if(p.or.p9)then
						goto1234=.true.;return

					endif !if(ifail.ne.0)then

	! mpjj=-1   ! old not something to remember

	! update ls and ld lists
					call jlplex(jlp_ls,jlp_lsi(nex+jlp_nrowz),jlp_lsi(newc2),jlp_lsi)
					call jlplex(jlp_ld,jlp_ld0+2,jlp_ldi(nex),jlp_ldi)
					if(p2)then
						write(n16,*)'ldaf ',(jlp_ld(jj7),jj7=1,jlp_ld0)
						write(n16,*)'lsaf ',(jlp_ls(jj7),jj7=1,5)
					endif !if(p2)then
	! nex leaving id entering
	! nex           3           4           0           4           2           0
	! lunit           0          22          22          20          22           0

	! leavk
	! nex           3           4           0           4           2           0
	! lunit           0          22          22          20          22           0
					if(p)then
		!					write(n16,*)'**fact** leavkwf <7082> before inserting ', id ,'next',next(0),next(next(0)),next(next(next(0))),  next(next(next(next(0))))
						write(n16,*)'**fact** units',jlp_lunit(jlp_next(0)),jlp_lunit(jlp_next(jlp_next(0))),&
							jlp_lunit(jlp_next(jlp_next(jlp_next(0)))), jlp_lunit( jlp_next(jlp_next(jlp_next(jlp_next(0)))))
						write(n16,*)'**fact** iprev',jlp_iprev(0),jlp_iprev(jlp_iprev(0)),jlp_iprev(jlp_iprev(jlp_iprev(0))),&
							jlp_iprev(jlp_iprev(jlp_iprev(jlp_iprev(0))))
					endif !if(p)then

					jlp_next(jlp_iprev(nex))=id ! insert the new col in next-
					jlp_next(id)=jlp_next(nex)
					jlp_iprev(id)=jlp_iprev(nex)
					jlp_iprev(jlp_next(id))=id

	! Päivitetään laskentayksikön vaihtoehtojen alkamissarake lunw:hen
					do ilu_ = 1,jlp_lunits0
						if (jlp_lunw(ilu_)==nex) then
							jlp_lunw(ilu_)=id
						endif !if (j_lunw(ilu_)==nex) then
					enddo !do ilu_ = 1,j_lunits0

					if(p)then
						write(n16,*)'aft next',jlp_next(0),jlp_next(jlp_next(0)), &
						jlp_next(jlp_next(jlp_next(0))),  jlp_next(jlp_next(jlp_next(jlp_next(0))))
						write(n16,*)'iprev',jlp_iprev(0),jlp_iprev(jlp_iprev(0)), &
						jlp_iprev(jlp_iprev(jlp_iprev(0))),jlp_iprev(jlp_iprev(jlp_iprev(jlp_iprev(0))))
					endif !if(p)then

					nex=jlp_next(nex)
				enddo !do while(j_lunit(nex)==j_lunit(leavkwf))

			endif !if(leavkwf>0) then

			if (leavkf.gt.jlp_mxd) then
! put to next free column
! old d= x-xkey, x=d+xkey
! new d= x-xnewkey=d+xoldkey-xnewkey

! lcur0 column of the
!  change first xps:

! subtract old key add new key
! leyvk-columd in D is newkey-oldkey, the corresponding element in objr contains
! also newkey-oldkey
! xps is sum of x:s of keys
! newxps=oldxps-oldkey+newkey= oldxps+(newkey-oldkey)
! and newkey-oldkey is already stored in a and objr

! xps:n laskenta (s.12 kaava): vähennä vanhan avaintehtaan alfat ja gammat, lisää uuden avaintehtaan alfat ja gammat
! leavk column contains the inforamtion

!xps laskenta, kuun avaintehdas vaihtuu
				iunit_ = jlp_lunit(leavkf)


				if(p.or.p9) then
					write(n16,*) 'AT VAI ', &
						leavkf, iunit_, jlp_ixkf(leavkf), jlp_ixkffact(leavkf)
				endif !if(p.or.p9) then
				ibas1=jxmatiba(jlp_ibaunit(iunit_)+jlp_keys(iunit_)) !,1)
				valtas=j_o(ivxmat)%d(ibas1+ikeepx_lkf)   !jxmatiba
				jlp_xps(0)=jlp_xps(0)+jlp_objr0(leavkf+nz+jlp_nrow)*valtas  !&

				do j=1,jlp_nrow
					jlp_xps(j)=jlp_xps(j)+jlp_a(j,leavkf+nz)*valtas !&
		!
					jlp_rhsw(j)=jlp_rhscur(j)-jlp_xps(j)
				enddo !do j=1,j_nrow

				if(p2)write(n16,*)(jlp_xps(j),j=0,jlp_nrow)

!objr0, a päivitys kun xkyk-muuttuja tulee kantaan
! if the entering schedule is in the same unit then also the entering
				if(iunit.eq.jlp_lunit(leavkf).and.ienter.eq.4.and.(ixkykenter==ixkyk_lkf)) then
	! in objr also the residual section and z section are included
					if(p)write(n16,*)'**fact** <7429> objr0(newc),objr0(leavkf+nrowz),objr0(newc)-objr0(leavkf+nrowz)',&
						jlp_objr0(newc),jlp_objr0(leavkf+jlp_nrowz),jlp_objr0(newc)-jlp_objr0(leavkf+jlp_nrowz)
					jlp_objr0(newc)=jlp_objr0(newc)-jlp_objr0(leavkf+jlp_nrowz)

					do j=1,jlp_nrow
		! newcol= x-oldkey  leavk:  newkey-oldkey  x-newkey= x-oldkey-(newkey-oldkey)
						jlp_a(j,newa)=jlp_a(j,newa)-jlp_a(j,leavkf+nz)
					enddo !do j=1,j_nrow

				endif !if(iunit.eq.j_lunit(leavkf).and.ienter.eq.4.and.(ixkykenter==ixkyk_lkf)) then

!objr0, a päivitys kun vaihtoehto tulee kantaan
! if the entering schedule is in the same unit then also the entering
				if(iunit.eq.jlp_lunit(leavkf).and.ienter.eq.3)then
	! in objr also the residual section and z section are included
					if(p)write(n16,*)'**fact** <7497> <wij> objr0(newc) alussa :', jlp_objr0(newc)

					ikey_ = jlp_ibaunit(iunit) + jlp_keys(iunit)
					iopt_ = jlp_ibaunit(iunit) + iopt
					ibaopt_=jxmatiba(iopt_)! ,1)
					ibakey_=jxmatiba(ikey_)! ,2)
					ibaxopt_=jxmatiba(iopt_)! ,1)
					ibaxkey_=jxmatiba(ikey_) !,2)
	!newc, newa asetettu aiemmin (ienter = 3 paikkeilla)
					if(jlp_ixcur(0).ne.0)then
						jlp_objr0(newc)=jlp_xmat(jlp_ix(0)+ibaxopt_)-jlp_xmat(jlp_ix(0)+ibaxkey_)
					else !if(j_ixcur(0).ne.0)then
						jlp_objr0(newc)=0.
					endif !if(j_ixcur(0).ne.0)then

					do jj=1,jlp_nrow
						if(jlp_ixcur(jj).ne.0) then
							jlp_a(jj,newa) = jlp_xmat(jlp_ix(jj)+ibaxopt_)- jlp_xmat(jlp_ix(jj)+ibaxkey_)
						else !if(j_ixcur(jj).ne.0) then
							jlp_a(jj,newa) = 0
						endif !if(j_ixcur(jj).ne.0) then
					enddo !do jj=1,j_nrow

					do jj=1,jlp_nrowpf ! #tehdasmjarivit
						jcurix=jlp_ifxcurrows(jj)
						irowj_=jlp_irowrow(jcurix)

		! tehtävärivin xk-mjat
						do k_=1,jlp_nfxrow(irowj_)
							ixkyk_=jlp_irowfxvars(jlp_ibafx(irowj_)+k_)

							if (ixkyk_==ixkyk_lkf) then
				! xk-mja, jonka avaintehdas vaihtumassa yksikössä
								keyf_ = jlp_ixkffact(leavkf)
							else !if (ixkyk_==ixkyk_lkf) then
				! muut xk-mjat (avaintehdas ei vaihdu)
								keyf_ = jlp_keyfact(iunit,ixkyk_)
							endif !if (ixkyk_==ixkyk_lkf) then

			! lisätään  avaintehtaan (alfa*(x_ij_k - x_iJ(i)_k))
							if(keyf_==jlp_irowffact(jlp_ibafx(irowj_)+k_)) then
				! jcurix 0 / muut rivit
								if (jcurix==0) then
									jlp_objr0(newc)=jlp_objr0(newc) + jlp_coeffx(jlp_ibafx(irowj_)+k_)* &
										(j_o(ivxmat)%d(ibaopt_+jlp_irowfkeep(jlp_ibafx(irowj_)+k_)) - & !jxmatiba
										j_o(ivxmat)%d(ibakey_+jlp_irowfkeep(jlp_ibafx(irowj_)+k_)))   !jxmatiba
								else !if (jcurix==0) then
									jlp_a(jcurix,newa)=jlp_a(jcurix,newa) + jlp_coeffx(jlp_ibafx(irowj_)+k_)* &
										(j_o(ivxmat)%d(ibaopt_+jlp_irowfkeep(jlp_ibafx(irowj_)+k_)) - & !jxmatiba
										j_o(ivxmat)%d(ibakey_+jlp_irowfkeep(jlp_ibafx(irowj_)+k_)))    !jxmatiba
								endif !if (jcurix==0) then
							endif !if(keyf_==j_irowffact(j_ibafx(irowj_)+k_)) then

						enddo !do k_=1,j_nfxrow(irowj_)
					enddo !do jj=1,j_nrowpf

					do jj=1,jlp_nrowpfy	!tehdas-yk-mjia sisältävät tehtävärivit
						jcurix=jlp_ifycurrows(jj)
						irowj_ = jlp_irowrow(jcurix)
						do k_=1,jlp_nfyrow(irowj_)
							listy=jlp_irowfyvars(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava ptl-lista
							listf=jlp_irowfyfact(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava tehdas-lista
							do ivars_=1,j_o(listy)%i(1) ! yk-mjan puutavaralistan muuttujat
								iv2elpos_ = jlp_ifyvarskeep(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan xmat-sarake
								iv2xkykpos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan paikka xkyk-listassa

								if (iv2xkykpos_==ixkyk_lkf) then
					! xk-mja, jonka avaintehdas vaihtumassa yksikössä
									keyf_ = jlp_ixkffact(leavkf)
								else !if (iv2xkykpos_==ixkyk_lkf) then
									keyf_ = jlp_keyfact(iunit,iv2xkykpos_)
								endif !if (iv2xkykpos_==ixkyk_lkf) then
								valuetas=j_o(ivxmat)%d(ibaopt_+iv2elpos_)-j_o(ivxmat)%d(ibakey_+iv2elpos_)
								do ifact_=1,j_o(listf)%i(1) ! yk-mjan tehdaslistan tehtaat
									iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj_)+k_)+ifact_-1) !tehtaan paikka factories-listassa
					! lisätään  avaintehtaan (gamma*(x_ij_k - x_iJ(i)_k))
									if(keyf_.eq.iv3factpos_) then
						!jcurix 0 / muut rivit
										if (jcurix==0) then
											jlp_objr0(newc)=jlp_objr0(newc) + j_v(jlp_fyfactout(iv2xkykpos_,iv3factpos_)) *  &
												valuetas

										else !if (jcurix==0) then
											jlp_a(jcurix,ia)=jlp_a(jcurix,ia) + j_v(jlp_fyfactout(iv2xkykpos_,iv3factpos_)) *  &
												valuetas

										endif !if (jcurix==0) then
									endif !if(keyf_.eq.iv3factpos_) then

								enddo !do ifact_=1,j_o(listf)%i(1)
							enddo !do ivars_=1,j_o(listy)%i(1)
						enddo !do k_=1,j_nfyrow(irowj_)
					enddo !do j_=1,j_nrowpfy

					if(p)write(n16,*)'**fact**  objr0(newc) päivitetty :', jlp_objr0(newc)
				endif !if(iunit.eq.j_lunit(leavkf).and.ienter.eq.3)then

! check

! if there are more columns in the same unit these must be changed with
! columns expressed in the new key

! old d= x-key, x=d+key
! new d= x-newkey=d+key-newkey
! column leave is actually : newkey-key, thus new d is:
! d- column leave
				leavec=leavkf+jlp_nrowz  ! in col numbering
				lcur=leavkf

				if(p) then
					write(n16,*)'**fact** unit, old keyfact,new ', jlp_lunit(lcur), &
					jlp_keyfact(jlp_lunit(lcur),jlp_ixkf(leavkf)),&
						jlp_ixkffact(leavkf)
				endif !if(p) then
				jlp_keyfact(jlp_lunit(lcur),jlp_ixkf(leavkf))=jlp_ixkffact(leavkf)

				if (p) then
					write(n16,*)'**fact** leavkf > 0, avaintehdas päivitetty/ leavkf, unit, ixkyk, ifact:',&
						leavkf, jlp_lunit(leavkf),jlp_ixkf(leavkf),jlp_ixkffact(leavkf)
					write(n16,*)'**fact** xk, tehdas;', j_vname(j_o(jlp_ivxkyk)%i2(jlp_ixkf(leavkf))),&
						j_vname(j_o(jlp_ivfact)%i2(jlp_ixkffact(leavkf)))
				endif !if (p) then

! intitially leavk is first column (in nex-order) in the unit
19 		nex=jlp_nextf(lcur,jlp_ixkf(leavkf))    ! in d columns
! we jump here from below to go through all columns in the same unit
! first free column is reserved for newc

				if(jlp_lunit(lcur).eq.jlp_lunit(nex))then
					if(p) then
						write(n16,*)'samma ,nex,',nex,jlp_lunit(lcur)
					endif !if(p) then
	! change the old additional  columns to correspond
	! to the new key
					if_=jlp_lf(jlp_lf0+2) !take new D-column for this
	! note ld0+1 may be used already if schedule is entering
					ia=if_+nz  ! ld0 nrow+1 already reserved for entering, ia=col in A
					newc2=ia+jlp_nrow  !newc2 col number in (I A)
					if(p)write(n16,*)'newc,newc2',newc,newc2,'newid,newa',if_,ia

					jlp_objr0(newc2)=jlp_objr0(nex+jlp_nrowz)-jlp_objr0(leavec)

					do i=1,jlp_nrow
						jlp_a(i,ia)=jlp_a(i,nex+nz)-jlp_a(i,leavkf+nz)
					enddo !do i=1,j_nrow
					if(p)write(n16,*)'iacol',jlp_a(1:min(jlp_nrow,50),ia)
					if(p)write(n16,*)'pivot7',nex+jlp_nrowz,newc2, 'in d',nex,if_
					jlp_lunit(if_)=jlp_lunit(leavkf)
					jlp_ixkf(if_)=jlp_ixkf(nex)
					jlp_ixkffact(if_)=jlp_ixkffact(nex)
					icolold=nex+jlp_nrowz
	!!!Fletcher ??????????

	! if leaving colums is the same as mp it is utilizing old computation
	! leaving column is stored into  mp
	! if entering column is mq, pivot is also utilizing old computations
	! entering column is getting the new value

					mqjj=-1   !new not  something known
					if(p) then
						write(n16,*) '**Pivot <8086> nex,nex+nrowz,newc2,nrow', nex+jlp_nrowz,newc2,jlp_nrow
					endif !if(p) then
					if(nup.ge.nfreq)then
						krefac=krefac+1
						if(p)write(19,*)'*refact'
					endif !if(nup.ge.nfreq)then
					call pivot(nex+jlp_nrowz,newc2,jlp_nrow, &
						nm,jlp_a,jlp_lavec,e,wslu1,lwsll1,ifail,jlp_info) !! nex+nrowz :: onko nex:issä
					ipivot=ipivot+1
					if(p7)write(77,*)3,ipivot,jlp_objf
					if(p.or.p9)write(n16,*)'pivot<84>',ipivot,3,jlp_objf,jlp_tmax
					if(ifail.ne.0)then
						write(6,*)'****pivot failure (6) ,ifail=',ifail,' info=',jlp_info

						nrecover=nrecover+1
						if(nrecover.ge.20)then
							write(6,*)'*jlp* is mixed up, try different tole (e.g.10,100,1000)(or consult J. Lappi))'
							j_err=.true.
							goto900=.true.;return

						endif !if(nrecover.ge.20)then
						write(6,*)'***trying to recover'
						if(p)write(n16,*)'***trying to recover'
						kierv=kier
						iunitv=iunit
						jlp_feasible=.false.
						goto1234=.true.;return

					endif !if(ifail.ne.0)then

					if(ipivot.eq.idebug)then
						p=.not.p
						if(p)then

							nout=16
						else !if(p)then
							nout=0
						endif !if(p)then
						write(6,*)'<3>changing debuggging at pivot=',ipivot,' into ',p
						if(p.and.jlp_nrow.le.50)p2=.true.
						write(n16,*)'*debug*'
						jdebug=jdebug+1
						if(jdebug.le.ndebug)idebug=j_v(j_o(iob)%i(ibasdebug+jdebug))

					endif !if(ipivot.eq.idebug)then

					if(p)then
						write(n16,*)'**pivot=',ipivot
						write(n16,*)'leaving col',(jlp_a(jj7,nex+nz),jj7=1,jlp_nrow)
						write(n16,*)'ent col',(jlp_a(jj7,ia),jj7=1,jlp_nrow)
					endif !if(p)then
	! update ls and ld lists
					call jlplex(jlp_ls,jlp_lsi(nex+jlp_nrowz),jlp_lsi(newc2),jlp_lsi)
					call lexf(jlp_lf,jlp_lf0+2,jlp_lfi(nex),jlp_lfi,jlp_mxd+1,2*jlp_mxd)
					if(p2)then
						write(n16,*)'lfaf ',(jlp_lf(jj7),jj7= jlp_mxd+1,jlp_lf0)
						write(n16,*)'lsaf ',(jlp_ls(jj7),jj7=1,5)
					endif !if(p2)then
	! nex leaving id entering
	! nex           3           4           0           4           2           0
	! lunit           0          22          22          20          22           0

	! leavk
	! nex           3           4           0           4           2           0
	! lunit           0          22          22          20          22           0
					if(p)then
					endif !if(p)then

					jlp_nextf(jlp_iprevf(nex, jlp_ixkf(leavkf)),jlp_ixkf(leavkf))=if_ ! insert the new col in nextf
					jlp_nextf(if_, jlp_ixkf(leavkf))=jlp_nextf(nex, jlp_ixkf(leavkf))
					jlp_iprevf(if_, jlp_ixkf(leavkf))=jlp_iprevf(nex, jlp_ixkf(leavkf))
					jlp_iprevf(jlp_nextf(if_,jlp_ixkf(leavkf)),jlp_ixkf(leavkf))=if_

					lcur=if_
					goto 19
				endif !if(j_lunit(lcur).eq.j_lunit(nex))then

! now just proceed as if leaving a-col leavk
! leave is index for ls
				leave=jlp_lsi(leavkf+jlp_nrowz)
				if(p)write(n16,*)'siis leaving, ind, inc ',leavkf,leave
				leaved=leavkf  !!leavkf,mahdollisesti+mxd (jos alkaa tehdasosuudesta indeksointi ykkösestä)

			endif !if (leavkf.gt.j_mxd) then

		endif !if (j_fpresent) then

! testi
8888 continue !we come here direrectly (through checking keys) if tmax is sufficiently small
		if(p)	write(n16,*)' leave, ienter ',leave,ienter

		if(leave.eq.0)then
			if(ienter.eq.3)then
				if(p.and.jlp_fpresent)	write(n16,*)'**fact** VAIN AVAINVE VAIHTUU>> '
! change just key
				iobs=jlp_ibaunit(iunit)+jlp_keys(iunit)
				ibax=jxmatiba(iobs) !,1)

				do jj=1,jlp_nrowp
					j=jlp_ixcurrows(jj)
					jlp_xps(j)=jlp_xps(j)-jlp_xmat(jlp_ix(j)+ibax) ! v(ix(j))
				enddo !do jj=1,j_nrowp

!xps:n päivitys, ienter = 3 (vain avainvaihtoehto vaihtuu)
				if (jlp_fpresent) then
					ibaobs=jxmatiba(iobs) !,1)
					iobsopt=jlp_ibaunit(iunit)+iopt	!uuden avainvaihtoehdon indeksi
					ibaopt=jxmatiba(iobsopt) !,2)
					do jj=1,jlp_nrowpf ! xk-muuttujat
						j=jlp_ifxcurrows(jj)
		! silmukka: rivin xk-muuttujat
						irowj = jlp_irowrow(j)
						do k=1,jlp_nfxrow(irowj)
			! onko kerrointa vastaava tehdas sama kuin laskentayksikön avaintehdas kerrointa vastaavalla muuttujalla
							if(jlp_keyfact(iunit,jlp_irowfxvars(jlp_ibafx(irowj)+k)).eq.jlp_irowffact(jlp_ibafx(irowj)+k))then
								jlp_xps(j)=jlp_xps(j) &
									- jlp_coeffx(jlp_ibafx(irowj)+k)*&
									j_o(ivxmat)%d(ibaobs+jlp_irowfkeep(jlp_ibafx(irowj)+k))& !jxmatiba
									+ jlp_coeffx(jlp_ibafx(irowj)+k)*&
									j_o(ivxmat)%d(ibaopt+jlp_irowfkeep(jlp_ibafx(irowj)+k))  !jxmatiba
							endif !if(j_keyfact(iunit,j_irowfxvars(j_ibafx(irowj)+k)).eq.j_irowffact(j_ibafx(irowj)+k))then
						enddo !do k=1,j_nfxrow(irowj)
					enddo !do jj=1,j_nrowpf

					do jj=1,jlp_nrowpfy ! yk-muuttujat
						j=jlp_ifycurrows(jj)
		! silmukka: rivin yk-muuttujat
		!		silmukka: puultavaralajilista
						irowj = jlp_irowrow(j)
						do k=1,jlp_nfyrow(irowj)
							listy=jlp_irowfyvars(jlp_ibafy(irowj)+k)
							listf=jlp_irowfyfact(jlp_ibafy(irowj)+k)
							do ivars_=1,j_o(listy)%i(1)
				!listy-listan ivars_:innen ptl-mjan paikka (järjestysnumero) xkyk-listassa
								iv2xykypos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj)+k)+ivars_-1)
								iv2elpos_ = jlp_ifyvarskeep(jlp_ibafykeep(jlp_ibafy(irowj)+k)+ivars_-1)
								valuetas=j_o(ivxmat)%d(ibaobs+iv2elpos_)
								valuetas2=j_o(ivxmat)%d(ibaopt+iv2elpos_)
								do ifact_=1,j_o(listf)%i(1)
					! iv3factpos_ : tehdaslistan list ifact_:innen tehtaan paikka (järjestysnumero) factories-listassa
									iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj)+k)+ifact_-1)
									if(jlp_keyfact(iunit,iv2xykypos_).eq.iv3factpos_) then
										jlp_xps(j)=jlp_xps(j) &
											- j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_))*&
											valuetas & !j_o(ivxmat)%d(jxmatiba(iobs)+iv2elpos_)
											+ j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_))*&
											valuetas2
						!	j_o(ivxmat)%d(jxmatiba(iobsopt)+iv2elpos_)
									endif !if(j_keyfact(iunit,iv2xykypos_).eq.iv3factpos_) then
								enddo !do ifact_=1,j_o(listf)%i(1)
							enddo !do ivars_=1,j_o(listy)%i(1)
						enddo !do k=1,j_nfyrow(irowj)
					enddo !do jj=1,j_nrowpfy

				endif !if (j_fpresent) then

				iobs=jlp_ibaunit(iunit)+iopt
				ibax=jxmatiba(iobs) !,1)
				do jj=1,jlp_nrowp
					j=jlp_ixcurrows(jj)
					jlp_xps(j)=jlp_xps(j)+jlp_xmat(jlp_ix(j)+ibax) ! v(ix(j))

				enddo !do jj=1,j_nrowp

				do jj=1,jlp_nrow
					jlp_rhsw(jj)=jlp_rhscur(jj)-jlp_xps(jj)
				enddo !do jj=1,j_nrow

				if(p)write(n16,*)'change just key,ne xps', jlp_xps(0:min(jlp_nrow,20))
				if(p)write(n16,*)'*yks old key, new ', iunit,jlp_keys(iunit),iopt
				jlp_keys(iunit)=iopt
				nkeys=nkeys+1
				justkey=.true.
				goto55=.true.;return    !!!! just key scedule changed
			else if(ienter.eq.1)then !if(ienter.eq.3)then
				if((jlp_lower(newc).and..not.jlp_ubou(newc)).or.(.not.jlp_lower(newc).and..not.jlp_lbou(newc))) then
					write(6,*)'jlp() unbounded problem (1)'

					if(ivoutresult.ne.j_ivresult) write(6,*)'jlp() output objects not created '

					if(jlp_feasible)j_v(ivfeasible)=1.
					j_v(ivunbounded)=1.
					if(jlp_maxo)then
						j_v(ivobjective)=1.7e37
					else !if(j_maxo)then
						j_v(ivobjective)=-1.7e37
					endif !if(j_maxo)then
					j_err=.true.

					return
				endif !if((j_lower(newc).and..not.j_ubou(newc)).or.(.not.j_lower(newc).and..not.j_lbou(newc))) then
				if(jlp_lower(newc))then
					jlp_lower(newc)=.false.
					jlp_rhscur(newc)=jlp_rhs2(newc)
				else !if(j_lower(newc))then
					jlp_lower(newc)=.true.
					jlp_rhscur(newc)=jlp_rhs(newc)
				endif !if(j_lower(newc))then
				if((jlp_ix(newc).ne.0).or.jlp_fpresent)then
					jlp_rhsw(newc)=jlp_rhscur(newc)-jlp_xps(newc)
				else !if((j_ix(newc).ne.0).or.j_fpresent)then
					jlp_rhsw(newc)=jlp_rhscur(newc)

				endif !if((j_ix(newc).ne.0).or.j_fpresent)then

				if(p) write(n16,*)'goto 55/täältä'
				goto55=.true.;return  !!!!active rhs changed goto main loop
!vain avaintehdas vaihtuu
!silmukoiden toteutus, goto 55
			else if (ienter.eq.4) then !if(ienter.eq.3)then
				if(p)	then
					write(n16,*)'**fact** VAIN AVAINTEHDAS VAIHTUU>> unit, ixkyk, vanha, uusi',&
						iunit, ixkykenter,jlp_keyfact(iunit,ixkykenter), ifopt
					write(n16,*)'**fact** keyfact xk, vanha, uusi', j_vname(j_o(jlp_ivxkyk)%i2(ixkykenter)),&
						j_vname(j_o(jlp_ivfact)%i2(jlp_keyfact(iunit,ixkykenter))), j_vname(j_o(jlp_ivfact)%i2(ifopt))
				endif !if(p)	then
				iunit_lkf = iunit
				iobs=jlp_ibaunit(iunit_lkf)+jlp_keys(iunit_lkf)
				ibaobs=jxmatiba(iobs)! ,1)
				do jj=1,jlp_nrowpf ! xk-muuttujarivit
					j=jlp_ifxcurrows(jj)
					irowj = jlp_irowrow(j)
	! silmukka: rivin xk-muuttujat
	!s 12 korjatun kaavan mukainen

					do k=1,jlp_nfxrow(irowj)
						if((jlp_irowfxvars(jlp_ibafx(irowj)+k)==ixkykenter).and.&
							(jlp_keyfact(iunit_lkf,jlp_irowfxvars(jlp_ibafx(irowj)+k)).eq.jlp_irowffact(jlp_ibafx(irowj)+k))) then
							jlp_xps(j)=jlp_xps(j) &
								- jlp_coeffx(jlp_ibafx(irowj)+k)*&
								j_o(ivxmat)%d(ibaobs+jlp_irowfkeep(jlp_ibafx(irowj)+k))  !jxmatiba
						endif !j_keyfact(iunit_lkf,j_irowfxvars(j_ibafx(irowj)+k)).eq.j_irowffact(j_ibafx(irowj)+k))) then
						if ((jlp_irowfxvars(jlp_ibafx(irowj)+k)==ixkykenter).and.(ifopt.eq.jlp_irowffact(jlp_ibafx(irowj)+k))) then
							jlp_xps(j)=jlp_xps(j) &
								+ jlp_coeffx(jlp_ibafx(irowj)+k)*&
								j_o(ivxmat)%d(ibaobs+jlp_irowfkeep(jlp_ibafx(irowj)+k))  !jxmatiba
						endif !if ((j_irowfxvars(j_ibafx(irowj)+k)==ixkykenter).and.(ifopt.eq.j_irowffact(j_ibafx(irowj)+k))) then
					enddo !do k=1,j_nfxrow(irowj)

					if(j>0) jlp_rhsw(j)=jlp_rhscur(j)-jlp_xps(j)
				enddo !do jj=1,j_nrowpf
				do jj=1,jlp_nrowpfy ! yk-muuttujarivit
					j=jlp_ifycurrows(jj)
	! silmukka: rivin yk-muuttujat
	!		silmukka: puultavaralajilista
					irowj = jlp_irowrow(j)
					do k=1,jlp_nfyrow(irowj)
						listy=jlp_irowfyvars(jlp_ibafy(irowj)+k)
						listf=jlp_irowfyfact(jlp_ibafy(irowj)+k)
						do ivars_=1,j_o(listy)%i(1)
			!listy-listan ivars_:innen ptl-mjan paikka (järjestysnumero) xkyk-listassa
							iv2xykypos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj)+k)+ivars_-1)
							iv2elpos_ = jlp_ifyvarskeep(jlp_ibafykeep(jlp_ibafy(irowj)+k)+ivars_-1)

							do ifact_=1,j_o(listf)%i(1)
				! iv3factpos_ : tehdaslistan list ifact_:innen tehtaan paikka (järjestysnumero) factories-listassa
								iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj)+k)+ifact_-1)

								if((iv2xykypos_==ixkykenter).and.(jlp_keyfact(iunit_lkf,iv2xykypos_).eq.iv3factpos_)) then
									jlp_xps(j)=jlp_xps(j) &
										- j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_))*&
										j_o(ivxmat)%d(ibaobs+iv2elpos_)  !jxmatiba
								endif !if((iv2xykypos_==ixkykenter).and.(j_keyfact(iunit_lkf,iv2xykypos_).eq.iv3factpos_)) then
								if((iv2xykypos_==ixkykenter).and.(iv3factpos_==ifopt)) then
									jlp_xps(j)=jlp_xps(j) &
										+ j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_))*&
										j_o(ivxmat)%d(ibaobs+iv2elpos_)  !jxmatiba
								endif !if((iv2xykypos_==ixkykenter).and.(iv3factpos_==ifopt)) then

							enddo !do ifact_=1,j_o(listf)%i(1)
						enddo !do ivars_=1,j_o(listy)%i(1)
					enddo !do k=1,j_nfyrow(irowj)
					if(j.gt.0) jlp_rhsw(j)=jlp_rhscur(j)-jlp_xps(j)
				enddo !do jj=1,j_nrowpfy
! avaintehdas taulukon  (keyfact) päivitys
				jlp_keyfact(iunit_lkf,ixkykenter) = ifopt
				if(p9)write(16,*)'vain at',iunit_lkf,ixkykenter, ifopt
				if(p) then
					write(n16,*) '**fact** vain avaintehdas vaihtuu, avaintehtaat päivitetty'
					write(n16,*)'**fact** xps',jlp_xps
					write(n16,*)'**fact** rhsw',jlp_rhsw
				endif !if(p) then
				nkeyf=nkeyf+1
				goto55=.true.;return    !!!!only key factory changed
			else !if(ienter.eq.3)then

				if(.not.jlp_feasible)then
					do jj=1,lr0
						if(r(jlp_lr(jj)).gt.jlp_tiny78)then
							if(.not.jlp_lbou(jlp_lr(jj)).or..not.jlp_lower(jlp_lr(jj))) then
				!virheilmoitus ja häivytään
							endif !if(.not.j_lbou(j_lr(j_)).or..not.j_lower(j_lr(jj))) then
							jlp_tmax=jlp_x(jlp_lr(jj))/r(jlp_lr(jj))  ;rcur=r(jlp_lr(jj))
							leavec=jlp_lr(jj)
							leave=jlp_lsi(leavec)
							goto 8888
						endif !if(r(j_lr(j_)).gt.j_tiny78)then
						if(r(jlp_lr(jj)).lt.jlp_tiny78n)then
							if(.not.jlp_ubou(jlp_lr(jj)).or.jlp_lower(jlp_lr(jj))) then
				!virheilmoitus ja häivytään
							endif !if(.not.j_ubou(j_lr(j_)).or.j_lower(j_lr(j_))) then
							jlp_tmax=jlp_x(jlp_lr(jj))/r(jlp_lr(jj)) ;rcur=r(jlp_lr(jj))
							leavec=jlp_lr(jj)
							leave=jlp_lsi(leavec)
							goto 8888
						endif !if(r(j_lr(j_)).lt.j_tiny78n)then
					enddo !do jj=1,lr0
				endif !if(.not.j_feasible)then

				write(6,*)'jlp() unbounded problem (2)'

				if(ivoutresult.ne.j_ivresult) write(6,*)'jlp() output objects not created '

				if(jlp_feasible)j_v(ivfeasible)=1.
				j_v(ivunbounded)=1.
				if(jlp_maxo)then
					j_v(ivobjective)=1.7e37
				else !if(j_maxo)then
					j_v(ivobjective)=-1.7e37
				endif !if(j_maxo)then
				j_err = .true.
				return
			endif !if(ienter.eq.3)then

		else !if(leave.eq.0)then
! lbres(leaveres) leaves
! update
			if(ipivot+1.eq.idebug)then
				p=.not.p
				if(p)then

					nout=16
				else !if(p)then
					nout=0
				endif !if(p)then
				write(6,*)'<4>changing debuggging at pivot=',ipivot,' into ',p
				write(n16,*)'*debug*'
				jdebug=jdebug+1
				if(jdebug.le.ndebug)idebug=j_v(j_o(iob)%i(ibasdebug+jdebug))

			endif !if(ipivot+1.eq.idebug)then
			if(sparse)then
				if(newc.gt.jlp_nrowz)mqjjsp=-1  ! mqjj=-1   !new not  something known
			else !if(sparse)then
				if(newc.gt.jlp_nrowz)mqjj=-1  ! mqjj=-1   !new not  something known
			endif !if(sparse)then
			if(p)write(n16,*)'***pivot***',ipivot+1
			if(p)then
				if(.not.sparse)write(n16,*)'leave',leave,'pivot cols',jlp_ls(leave),newc,'mp,mq',mpjj,mqjj
				if(sparse)write(n16,*)'leave',leave,'pivot cols',jlp_ls(leave),newc,'mp,mq',mpjjsp,mqjjsp
			endif !if(p)then
! pivot uses
! leave if number in ls
! newc is absolute column number
			icolold=jlp_ls(leave)

! my dear Fletcher

			if(sparse)then

				call pivotsp(jlp_ls(leave),newc,jlp_nrow,nm,jlp_a, &
				jlp_lavecsp,e,wslu1,lwsll1,ifail,jlp_info) !linux
			else !if(sparse)then

				if(p) then
					write(n16,*) '**Pivot <8482> ls(leave),newc,nrow', jlp_ls(leave),newc,jlp_nrow
					if(jlp_ls(leave)>jlp_nrow) write(n16,*)'leaving ', &
					(jlp_a(j,jlp_ls(leave)-jlp_nrow),j=1,jlp_nrow)
					if(newc>jlp_nrow)write(n16,*)'entering ', &
					(jlp_a(j,newc-jlp_nrow),j=1,jlp_nrow)
				endif !if(p) then
				if(nup.ge.nfreq)then
					krefac=krefac+1
					if(p)write(19,*)'*refact'
				endif !if(nup.ge.nfreq)then

				if(jlp_xpresent.and.abs(rcur).le.1.d-9)then
!!write(19,*)'uus,ipivot,ienter,leave,j_tmax',ipivot,ienter,leave,j_tmax,rcur
					goto112233=.true.
					return

				endif !if(j_xpresent.and.abs(rcur).le.1.d-9)then

				if(route67)then
					if(newc.gt.jlp_nrow)then    !jl 20160608
		! If equation Basis matrix*x= entering column is solved i.e. the entering column is
		! expressed as a linear combination of basic columns and the coefficient of the leaving column is
		! close to zero this means that the entering column is linearly dependent on the all non-leaving
		! columns and the new basis would be singular
						call fbsub(jlp_nrow,leave,leave,jlp_a,jlp_lavec,0,jlp_a(1:,newc-jlp_nrow),jlp_x,jlp_ls,wslu1,lwsll1,.false.)

						if(abs(jlp_x(jlp_ls(leave))).lt.jlp_tiny6)then
							npivotw=npivotw+1
							if(npivotw.le.7)write(6,*)'pivot cannot be done, pivot=',ipivot,' ienter=',ienter

							if(p9)then
				!if(pp)write(16,*)'ienter,leavec',ienter,leavec,store,j_tmax,newa
								write(n16,*)'<12>pivot_cannot',ipivot,jlp_objf,ienter,iunit,jlp_tmax,&
									jlp_valueopt,jlp_valuek,ixkykenter,leavkf,ifopt,justkey,jlp_keyfact(iunit,max(ixkykenter,1))
								call fbsub(jlp_nrow,1,jlp_nrow,jlp_a,jlp_lavec,0,jlp_a(1:,newc-jlp_nrow),jlp_x,jlp_ls,wslu1,lwsll1,.false.)
								write(n16,*)(jlp_ls(jjj),jlp_x(jlp_ls(jjj)),jjj=1,jlp_nrow)
							endif !if(p9)then
							goto55=.true.;return
						endif !if(abs(j_x(j_ls(leave))).lt.j_tiny6)then
					else !if(newc.gt.j_nrow)then
		! residual enters
						call fbsub(jlp_nrow,leave,leave,jlp_a,jlp_lavec,newc,jlp_a(1:,1),jlp_x,jlp_ls,wslu1,lwsll1,.false.)

						if(abs(jlp_x(jlp_ls(leave))).lt.jlp_tiny6)then
			! ! If equation Basis matrix*x= entering column is solved i.e. the entering column is
			! expressed as a linear combination of basic columns and the coefficient of the leaving column is
			! close to zero this means that the entering column is linearly dependent on the all non-leaving
			! columns and the new basis would be singular
							nresw=nresw+1
							if(nresw.le.7)write(6,*)'residual ',newc,' cannot enter, pivot=',ipivot !&


							if(p9)then
								write(16,*)'<44>residual ',newc,' cannot enter, pivot=',ipivot,' x=',jlp_x(jlp_ls(leave)),r(jlp_ls(leave))
								write(16,*)'ienter,leavec',ienter,leavec,jlp_ls(leave)-jlp_nrow,jlp_tmax
								write(16,*)'vcmax ',jlp_vcmax,jlp_lower(newc)
								call fbsub(jlp_nrow,1,jlp_nrow,jlp_a,jlp_lavec,newc,jlp_a(1:,1),jlp_x,jlp_ls,wslu1,lwsll1,.false.)
								write(n16,*)(jlp_ls(jjj),jlp_x(jlp_ls(jjj)),jjj=1,jlp_nrow)
							endif !if(p9)then
							ilres=newc
							objilres=jlp_objf

							goto55=.true.;return
						endif !if(abs(j_x(j_ls(leave))).lt.j_tiny6)then

					endif !if(newc.gt.j_nrow)then
				endif !if(route67)then

				call pivot(jlp_ls(leave),newc,jlp_nrow,nm,jlp_a,jlp_lavec,e,wslu1,lwsll1,ifail,jlp_info) !linux

				if(tabu)then
					if(fast)then
						if(leavec.gt.jlp_nrowz.and.leavec.le.jlp_nrowz+jlp_mxd)then
							iuni_= jlp_lunit(leavec-jlp_nrowz)

							fastreject(jlp_ibaunit(iuni_)+jlp_isch(leavec-jlp_nrowz))=.true.
							if(p)write(16,*)'tabu ',iuni_,jlp_isch(leavec-jlp_nrowz)
						endif !if(leavec.gt.j_nrowz.and.leavec.le.j_nrowz+j_mxd)then
					endif !if(fast)then
				endif !if(tabu)then

			endif !if(sparse)then
			ipivot=ipivot+1
			if(p7)write(77,*)4,ipivot,jlp_objf
			if(p.or.p9.or.pp)write(n16,*)'<75>',ipivot,jlp_objf,ienter,iunit,ienterv,nsame,jlp_tmax,&
				jlp_x(jlp_ls(leave)),r(jlp_ls(leave))

			if(p)write(n16,*)'e:',e

			if(ifail.ne.0)then
				write(6,*)'*fail in pivot: ifail,tmax,x,Pivots ',ifail, jlp_tmax,jlp_x(jlp_ls(leave)),ipivot,'*trying to recover'
				if(p.or.p9)write(16,*) &
				'*fail in pivot: ifail,tmax,x,Pivots ',ifail, jlp_tmax,jlp_x(jlp_ls(leave)),ipivot,'*trying to recover'

				kierv=kier
				iunitv=iunit
				jlp_feasible=.false.
				nrecover=nrecover+1
				if(nrecover.ge.20)then
					write(6,*)'*jlp* is mixed up,try different tole (e.g.10,100,1000)(or consult J. Lappi)'
					j_err=.true.
					goto900=.true.;return
				endif !if(nrecover.ge.20)then
				goto1234=.true.;return

			endif !if(ifail.ne.0)then
			if(sparse)then
				if(jlp_ls(leave).gt.jlp_nrowz) mpjjsp=-1
			else !if(sparse)then
!	if(ls(leave).gt.nrowz) mpjj=-1
			endif !if(sparse)then

			iunitp=iunit

			muutosb=muutosb+1
! update ls
			leavec=jlp_ls(leave)  ! column of leaving
			leaved=leavec-jlp_nrowz
! update ls and lsi lists
			call jlplex(jlp_ls,leave,jlp_lsi(newc),jlp_lsi)

		endif !if(leave.eq.0)then

		if(ienter.eq.1)then
! ilrmax idex in lr
! residual enters
			call jlplex(jlp_lr,ilrmax,lr0+1,jlp_lri)

			lr0=lr0+1
			if(lr0.eq.jlp_nrow)nup=0   !clear refactorizing counter
		else if(ienter.eq.2)then !if(ienter.eq.1)then
!  z enters
			call jlplex(jlp_lz,ilzmax,lz0+1,jlp_lzi)
			lz0=lz0+1

		else if(ienter.eq.3)then !if(ienter.eq.1)then
			if(p)write(n16,*)'d enters,new ld0',jlp_ld0+1
! when searchin if key is leaving we have stored the
! column were we have link to this unit in lcursame
			id=newc-jlp_nrowz
			ia=newc-jlp_nrow
! leaved is leaving in d
			if(lcursame.gt.0)then  ! d-eaves
! the unit was among basic
! check that units will be consecutive in the next
! *****************
! tallenna kun käydään läpi,mistä alkaa sama yksikkö lcursame
! even if lcursame, or other
				if(p)write(n16,*)'lcursame',lcursame
				iaft=jlp_iprev(lcursame)  ! put after iaft
				jlp_next(id)=jlp_next(iaft)
				jlp_next(iaft)=id
				jlp_iprev(id)=iaft
				jlp_iprev(jlp_next(id))=id

				if(jlp_fpresent)then
					do ilu_ = 1,jlp_lunits0
		! Päivitetään laskentayksikön vaihtoehtojen alkamissarake lunw:hen
						if (jlp_lunw(ilu_)==lcursame) then
							jlp_lunw(ilu_)=id
						endif !if (j_lunw(ilu_)==lcursame) then
					enddo !do ilu_ = 1,j_lunits0
				endif !if(j_fpresent)then

				if(p) then
					write(n16,*)'aftl,next',jlp_next(0),jlp_next(jlp_next(0)), &
					jlp_next(jlp_next(jlp_next(0))),jlp_next(jlp_next(jlp_next(jlp_next(0))))
					write(n16,*)'iprev',jlp_iprev(0),jlp_iprev(jlp_iprev(0)), &
					jlp_iprev(jlp_iprev(jlp_iprev(0))),jlp_iprev(jlp_iprev(jlp_iprev(jlp_iprev(0))))
				endif !if(p) then
			else !if(lcursame.gt.0)then
! put to the end
				jlp_next(jlp_iprev(0))=id
				jlp_next(id)=0
				jlp_iprev(id)=jlp_iprev(0)
				jlp_iprev(0)=id

				if(jlp_fpresent)then
	! Jos lunit(id):tä ei löydy lunxkf:ää vastaavista laskentayksiköistä
	! --> lunw(lunits0++) = id
	! --> lunxkf(lunits0) = 0

					ilu_=1
	!löytyykö kannan saraketta id vastaavan laskentaksikön xkf-muuttujaa kannasta
					do while(jlp_lunit(jlp_lunxkf(ilu_)) /= iunit)
						ilu_=ilu_+1
						if (ilu_>jlp_lunits0) exit
					enddo !do while(j_lunit(j_lunxkf(ilu_)) /= iunit)
					if (ilu_<=jlp_lunits0) then
						jlp_lunw(ilu_)=id
					else !if (ilu_<=j_lunits0) then
		!laskentayksikköön ei liittynyt aiemmin kannan kannan vaihtoehto- tai tehdasmuuttujasarakkeita
						jlp_lunits0 = jlp_lunits0+1
						jlp_lunw(jlp_lunits0) = id
						jlp_lunxkf(jlp_lunits0) = jlp_mxd
					endif !if (ilu_<=j_lunits0) then
				endif !if(j_fpresent)then

			endif !if(lcursame.gt.0)then
! edelleed d-enters
			jlp_ld0=jlp_ld0+1
			jlp_lunit(id)=iunit
			jlp_isch(id)=iopt

! update leavelists
! ei toimi jos joku muu enters
!********************************
! if(ienter.eq.1)then loppuu tässä

		else if(ienter==4) then !if(ienter.eq.1)then
! nextf yms päivitys
			if(p)write(n16,*)'**fact** xkf kantaan (75xx) nextf,lunit,lunxkf,lunw päivitykset, new lf0',jlp_lf0+1
! when searchin if key is leaving we have stored the
! column were we have link to this unit in lcursame
			id=newc-jlp_nrowz
			ia=newc-jlp_nrow
! leaved is leaving in d

			if(lcursamef.gt.0)then  ! xk-eaves
! the unit was among basic
! check that units will be consecutive in the next
! *****************
! tallenna kun käydään läpi,mistä alkaa sama yksikkö lcursame
! even if lcursame, or other
				if(p)write(n16,*)'lcursamef',lcursamef
				iaft=jlp_iprevf(lcursamef, ixkykenter)  ! put after iaft
				jlp_nextf(id, ixkykenter)=jlp_nextf(iaft, ixkykenter)
				jlp_nextf(iaft, ixkykenter)=id
				jlp_iprevf(id, ixkykenter)=iaft
				jlp_iprevf(jlp_nextf(id, ixkykenter), ixkykenter)=id

				do ilu_ = 1,jlp_lunits0
					if (jlp_lunxkf(ilu_)==lcursamef) then
						jlp_lunxkf(ilu_)=id
						if (p) write(n16,*)"**fact** lunxkf päivitetty <8184>",(jlp_lunxkf(jj7),jj7=1,jlp_lunits0)
					endif !if (j_lunxkf(ilu_)==lcursamef) then
				enddo !do ilu_ = 1,j_lunits0

			else !if(lcursamef.gt.0)then
! put to the end
				jlp_nextf(jlp_iprevf(jlp_mxd,ixkykenter), ixkykenter)=id
				jlp_nextf(id, ixkykenter)=jlp_mxd
				jlp_iprevf(id,ixkykenter)=jlp_iprevf(jlp_mxd,ixkykenter)
				jlp_iprevf(jlp_mxd,ixkykenter)=id

! Jos lunit(id):tä ei löydy lunw:tä vastaavista laskentayksiköistä
! --> lunxkf(lunits0++) = id
! --> lunw(lunits0) = 0

				ilu_=1
!löytyykö kannan saraketta id vastaavan laskentayksikön vaihtoehtoa kannasta

				do while(jlp_lunit(jlp_lunw(ilu_))/=iunit)
					ilu_=ilu_+1
					if (ilu_>jlp_lunits0) exit
				enddo !do while(j_lunit(j_lunw(ilu_))/=iunit)
				if (ilu_<=jlp_lunits0) then
					jlp_lunxkf(ilu_)=id
				else !if (ilu_<=j_lunits0) then
	!laskentayksikköön ei liittynyt aiemmin kannan vaihtoehto- tai tehdasmuuttujasarakkeita
					jlp_lunits0 = jlp_lunits0+1
					jlp_lunxkf(jlp_lunits0) = id
					jlp_lunw(jlp_lunits0) = 0
				endif !if (ilu_<=j_lunits0) then

			endif !if(lcursamef.gt.0)then
! edelleed d-enters
! indeksointi muutettu alkamaan mxd+1:stä (vastaa suoraan kannan saraketta)
			jlp_lf0=jlp_lf0+1
			jlp_lunit(id)=iunit
! tallennetaan ixkykenter
! tallennetaan tehdas f´

! update leavelists
! ei toimi jos joku muu enters
!********************************

		endif !if(ienter.eq.1)then

		if(p)    write(n16,*)'leavec,nrow',leavec,jlp_nrow
		if(leavec.le.jlp_nrow)then
! resiadual leaves
! put leaving as first nonbasic
			call jlplex(jlp_lr,lr0,jlp_lri(leavec),jlp_lri)
			lr0=lr0-1
		else if(leavec.le.jlp_nrowz)then !if(leavec.le.j_nrow)then
			call jlplex(jlp_lz,lz0,jlp_lzi(leavec-jlp_nrow),jlp_lzi)
			lz0=lz0-1
		else if(leavec.le.(jlp_nrowz+jlp_mxd)) then !if(leavec.le.j_nrow)then
! on jo vaihdettu
			if(p)write(n16,*)'d leaves,	col in d=',leaved,' next', &
			jlp_next(0),jlp_next(jlp_next(0)),jlp_next(jlp_next(jlp_next(0)))&
				,'prev',jlp_iprev(0),jlp_iprev(jlp_iprev(0)), &
				jlp_iprev(jlp_iprev(jlp_iprev(0))),'unit=',jlp_lunit(leaved)
			call jlplex(jlp_ld,jlp_ld0,jlp_ldi(leaved),jlp_ldi)
			jlp_ld0=jlp_ld0-1
! drop
			jlp_next(jlp_iprev(leaved))=jlp_next(leaved)
			jlp_iprev(jlp_next(leaved))=jlp_iprev(leaved)
			if(p)then
				write(n16,*)'leaved,id,next',leaved,id
				write(n16,*)'next',jlp_next(0),jlp_next(jlp_next(0)), &
				jlp_next(jlp_next(jlp_next(0))), jlp_next(jlp_next(jlp_next(jlp_next(0))))
				write(n16,*)'iprev',jlp_iprev(0),jlp_iprev(jlp_iprev(0)),  &
				jlp_iprev(jlp_iprev(jlp_iprev(0))),jlp_iprev(jlp_iprev(jlp_iprev(jlp_iprev(0))))
			endif !if(p)then

!lunw:n päivitys
			if(jlp_fpresent)then
				do ilu_ = 1,jlp_lunits0
					if (jlp_lunw(ilu_)==leaved) then
						if (jlp_lunit(leaved) == jlp_lunit(jlp_next(leaved))) then
							jlp_lunw(ilu_)= jlp_next(leaved)
						else !if (j_lunit(leaved) == j_lunit(j_next(leaved))) then
							if (jlp_lunxkf(ilu_)==jlp_mxd) then
								do i_ = ilu_+1,jlp_lunits0
									jlp_lunw(i_-1) = jlp_lunw(i_)
									jlp_lunxkf(i_-1) = jlp_lunxkf(i_)
								enddo !do i_ = ilu_+1,j_lunits0
								jlp_lunw(jlp_lunits0) = 0
								jlp_lunxkf(jlp_lunits0) = jlp_mxd
								jlp_lunits0 = jlp_lunits0-1
							else !if (j_lunxkf(ilu_)==j_mxd) then
								jlp_lunw(ilu_)=0
							endif !if (j_lunxkf(ilu_)==j_mxd) then
						endif !if (j_lunit(leaved) == j_lunit(j_next(leaved))) then
					endif !if (j_lunw(ilu_)==leaved) then
				enddo !do ilu_ = 1,j_lunits0
			endif !if(j_fpresent)then

!!!test
			if(jlp_testl)then
				do i=1,jlp_nrow
					if(jlp_ls(i).gt.jlp_nrowz)then
						do j=1,jlp_ld0
							if(jlp_ls(i)-jlp_nrowz.eq.jlp_ld(j))goto 7188
						enddo !do j=1,j_ld0
						write(n16,*)'*colnot*',jlp_ls(i),'not in',	  (jlp_ld(j),j=1,jlp_ld0)
						close(16)
						write(6,*)'*colnot* return'
						return
		7188 	    continue
					endif !if(j_ls(i).gt.j_nrowz)then
				enddo !do i=1,j_nrow
			endif !if(j_testl)then
! lista uniteista

!leavec tehdassrk??
		else !if(leavec.le.j_nrow)then
			call lexf(jlp_lf,jlp_lf0,jlp_lfi(leaved),jlp_lfi,jlp_mxd+1,2*jlp_mxd)
!!ld0=ld0-1
			jlp_lf0=jlp_lf0-1
! drop
			jlp_nextf(jlp_iprevf(leaved,jlp_ixkf(leaved)),jlp_ixkf(leaved))=jlp_nextf(leaved,jlp_ixkf(leaved))
			jlp_iprevf(jlp_nextf(leaved,jlp_ixkf(leaved)),jlp_ixkf(leaved))=jlp_iprevf(leaved,jlp_ixkf(leaved))

!lunxkf päivitys
			do ilu_ = 1,jlp_lunits0
				if (jlp_lunxkf(ilu_)==leaved) then
					if (jlp_lunit(leaved) == jlp_lunit(jlp_nextf(leaved,jlp_ixkf(leaved)))) then
						jlp_lunxkf(ilu_)= jlp_nextf(leaved,jlp_ixkf(leaved))
					else !if (j_lunit(leaved) == j_lunit(j_nextf(leaved,j_ixkf(leaved)))) then
						if (jlp_lunw(ilu_)==0) then
							do i_ = ilu_+1,jlp_lunits0
								jlp_lunw(i_-1) = jlp_lunw(i_)
								jlp_lunxkf(i_-1) = jlp_lunxkf(i_)
							enddo !do i_ = ilu_+1,j_lunits0
							jlp_lunw(jlp_lunits0) = 0
							jlp_lunxkf(jlp_lunits0) = jlp_mxd
							jlp_lunits0 = jlp_lunits0-1
						else !if (j_lunw(ilu_)==0) then
							jlp_lunxkf(ilu_)=jlp_mxd
						endif !if (j_lunw(ilu_)==0) then
					endif !if (j_lunit(leaved) == j_lunit(j_nextf(leaved,j_ixkf(leaved)))) then
				endif !if (j_lunxkf(ilu_)==leaved) then
			enddo !do ilu_ = 1,j_lunits0
			if (pp) write(n16,*)"**fact** lunxkf päivitetty <8088>",(jlp_lunw(jj7),jj7=1,jlp_lunits0)

		endif !if(leavec.le.j_nrow)then



	end subroutine leaving !subroutine leaving()

! subroutine ispe(isper,ipi) This subroutine was used to test why there were negative amounts
! integer iobs_,ndi,i,ii_,lfj_,keyf_,ipivotv
! double precision :: res,resv,otherv=-1000.
! double precision :: other
! logical isper
! integer :: ndiv,keyfv,iotherv=-1
! save
! isper=.false.
! iobs_=j_ibaunit(121)+j_keys(121)
! !if(ipi.ne.0)write(6,*)'hep '
! call j_getobsiv(iobs_,j_ivmatx,j_ivkeepx,jlp_ivtransx,0)
! ! function valuexkfact(ixk,ifact)

! res=j_v(j_o(j_ivxkyk)%i(16))
! !	if(ipi.ne.0)write(6,*)'res',res
! ndi=0
! do i=1,j_ld0
! if(j_lunit(j_ld(i)).eq.121)ndi=ndi+1
! enddo
! !	if(ipi.ne.0)write(6,*)'ndi ',ndi
! !if(ndi.gt.0)return
! ndi=0
! other=0.
! nother=0
! keyf_ = j_keyfact(121,16)
! do lfj_=j_mxd+1,j_lf0
! if(j_lunit(j_lf(lfj_)).ne.121)cycle
! if(j_ixkf(j_lf(lfj_)).ne.16)cycle
! if(j_ixkffact(j_lf(lfj_)).ne.keyf_)then
! other=other+j_x(j_nrowz+j_lf(lfj_))
! nother=nother+1
! iother=j_ixkffact(j_lf(lfj_))
! !if(ipi.ne.0)write(26,*)'other ',other,res,ipivot
! endif
! ! if(other.gt.res+0.0001)then
! ! isper=.true.
! ! write(6,*)'***negat',other-res,'pivot',ipivot
! ! endif
! enddo
! if(ndi.ne.ndiv.or.keyf_.ne.keyfv.or.res.ne.resv.or.&
! other.ne.otherv.or.nother.ne.notherv.or.&
! iother.ne.iotherv)write(26,*)ipivot,res,keyf_,res-other,iother,&
! other,ndi,nother,ipivotv
! ! write(27,*)ipivot,res,keyf_,res-other,iother,&
! !other,ndi,nother,ipivotv
! ndiv=ndi
! iotherv=iother
! resv=res
! keyfv=keyf_
! notherv=nother
! otherv=other
! ipivotv=ipivot
! return
! ! double precision su_
! ! integer iix_,i_
! ! su=0.d0
! ! do i_,j_mxd+1,j_lf0
! ! iix_=j_ixkf(j_lf(i_))
! ! if(iix_.eq.ixk)then

! ! endif
! ! enddo
! ! return
! ! end
! end subroutine
! function ibass(iobs)  ! basis
! integer ::ixdisk1=0
! integer :: ixdisk2=0
! integer ::last
! integer ::iba2
! integer :: i
! save ixdisk1,ixdisk2,last
! if(xdisk)then
! if(iobs.eq.ixdisk1)then
! ibass=0
! elseif(iobs.eq.ixdisk2)then
! ibass=keepx
! elseif(last.eq.2)then
! read(nuxdisk,rec=iobs)j_o(ivxmat)%d(1:keepx)
! ixdisk1=iobs
! last=1
! ibass=0
! else
! read(nuxdisk,rec=iobs)j_o(ivxmat)%d(keepx+1:2*keepx)
! ixdisk2=iobs
! ibass=keepx
! last=2
! endif
! if(ivtestmat.ne.0)then
! iba2=(iobs-1)*keepx
! do i=1,keepx
! if(j_o(ivxmat)%d(ibass+i).ne.j_o(ivtestmat)%d(iba2+i))then
! write(6,*)'iobs',iobs,j_o(ivxmat)%d(ibass+1:ibass+keepx)
! write(6,*)'iob2',iobs,j_o(ivtestmat)%d(iba2+1:iba2+keepx)
! read(5,*)iii_
! if(iii_.eq.99)stop
! exit
! endif
! enddo
! endif
! ! if(i.ne.99)then
! ! write(6,*)'iobs,ixdisk1,ixdisk2,ibass,last',iobs,ixdisk1,ixdisk2,ibass,last,'an joku,99 stop wrinting'
! ! read(5,*)i
! ! endif
! else
! ibass=(iobs-1)*keepx
! endif

! return
! end function
! integer function jxmatiba(iobs,isaa)  !subroutine
	! ! used to get ivxmat in jlp function

	! !	integer jxmatiba
	! integer,intent(in)::iobs
	! integer,intent(in)::isaa  !1 or 2 indicating which buffer row is used

	! if(j_memobs(iobs).le.j_xdatlast)then
		! jxmatiba=(j_memobs(iobs)-1)*keepx
		! ! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,3,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
		! ! if(j_feasible.or.feas)jxmatiba=100000000
		! return
	! endif
	! if(j_memobs(iobs).le.j_xdatlast2.and..false.)then  !from upper buffer note
		! jxmatiba=j_j_xmatibas2+(j_memobs(iobs)-j_xmatlopp1)*keepx !note
		! ! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,3,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
		! ! if(j_feasible.or.feas)jxmatiba=100000000
		! return
	! endif


	! if(isaa.eq.1)then
		! jxmatiba=j_xdatekabas
		! if(j_xdatekaobs.eq.iobs.and..false.)return
		! j_xdatekaobs=iobs
	! else
		! jxmatiba=j_xdattokabas
		! if(j_xdattokaobs.eq.iobs.and..false.)return
		! j_xdattokaobs=iobs
	! endif
	! read(j_xdatnu,rec=iobs)j_o(ivxmat)%d(jxmatiba+1:jxmatiba+keepx)
	! ! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,isaa,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
	! ! if(j_feasible

	! return
! end function  !end subroutine



! integer function xdatiba(iobs,isaa)   ! subroutine

! !dimension rivi(170)
! ! used to get ivxmat in jlp function

! !	integer xdatiba
! integer,intent(in)::iobs
! integer,intent(in)::isaa  !1 or 2 indicating which buffer row is used

! !read(j_xdatnu,rec=iobs)rivi

! if(j_memobs(iobs).le.j_xdatlast)then
! xdatiba=(j_memobs(iobs)-1)*keepx
! ! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,3,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
! ! if(j_feasible.or.feas)jxmatiba=100000000
! !goto 8541
! return
! endif
! if(iobs.ge.j_xdatfirst2.and.iobs.le.j_xdatlast2)then  !from upper buffer note
! xdatiba=j_xdatibas2+(iobs-j_xdatfirst2)*keepx !note
! ! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,3,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
! ! if(j_feasible.or.feas)jxmatiba=100000000
! !goto 8541
! return
! endif


! if(isaa.eq.1)then
! xdatiba=j_xdatekabas
! !if(j_xdatekaobs.eq.iobs)goto 8541
! if(j_xdatekaobs.eq.iobs)return
! j_xdatekaobs=iobs
! else
! xdatiba=j_xdattokabas
! !if(j_xdattokaobs.eq.iobs)goto 8541
! if(j_xdattokaobs.eq.iobs)return
! j_xdattokaobs=iobs
! endif
! read(j_xdatnu,rec=iobs)j_o(ivxmat)%d(xdatiba+1:xdatiba+keepx)
! ! if(j_feasible.or.feas)write(17,*)iobs,jxmatiba,isaa,j_xmat(jxmatiba+1:jxmatiba+j_ntemp0)
! ! if(j_feasible
! return


! ! 8541 if(all(rivi.eq.j_o(ivxmat)%d(xdatiba+1:xdatiba+keepx)))return
	! ! write(6,*)'dat',iobs,isaa,j_memobs(iobs),j_xdatlast,j_xdatfirst2,j_xdatlast2
	! ! write(6,*)'fastmake,iunit ',fastmake,iunit,j_nsch(iunit),'ipivot',ipivot
	! ! write(16,*)rivi
	! ! write(16,*)j_o(ivxmat)%d(xdatiba+1:xdatiba+keepx)
	! ! stop 1257
!end function  !subroutine



	subroutine printproblem()
		integer irow0,irow,ido,i,j,k
		irow0=0
		irow=0
		ido=0
		do i=1,nset
			domloopp: do j=1,jlp_nsetd(i)
				ido=ido+1
				if(jlp_xpresent.and.jlp_ndom.gt.0)then
					j_tempchar='DOMAIN:'
					idom=jlp_isetd(ido)  !domain number
					call j_getline(jlp_ivdomain,idom,j_tempchar,le)

					j_tempchar(le+8:le+12)='units'
					j_tempchar(le+1:le+6)=j_chi5(jlp_domainunits(idom),0)
		!	write(6,*)' ',('_',kk=1,78)
					write(nureport,'(a)')j_tempchar(1:le+12)
				end if !if(j_xpresent.and.j_ndom.gt.0)then
	!write(nureport,*)' ',('_',kk=1,78)
	!		write(j_buf,66061)
	!66061 format('row',t38,'value',t50,'shadow',t61,  'lower',t70,'upper')
	!	write(nureport,'(a)')j_buf
	!		write(j_buf,6606)
	!6606 format(t50,'price', t61,'bound',t70, 'bound')
	!!	if(intapp)j_buf(35:44)='int. app.'
	!	write(nureport,'(a)')j_buf
	!	write(nureport,*)' ',('_',kk=1,78)
				do k=1,jlp_nsetr(i)
					if((k==1).and.(j>1)) irow0 = irow0 - jlp_nsetr(i)
					irow0 = irow0+1

					call j_getline(jlp_ivrow,irow0,j_tempchar(6:),le)  ;j_tempchar(6+le:le+28)=' '

		!lyhennetään ylipitkät rivit etteivät sotkeennu
		!	if((6+le)>=34) then
		!		j_buf(34:35)=j_dots
		!	endif !if((6+le)>=34) then

					if(irow0.ne.irowobj)then
			!constraint row
						irow=irow+1
						jlp_apubuf=j_chi5(irow,0); j_tempchar(1:3)=jlp_apubuf(3:5);j_tempchar(4:5)=') '

			!j_value=j_rhscur(irow)-j_x(irow)
			!j_buf(36:)=j_chr10(dble(j_value))
			!j_buf(47:57)=j_chr10(dble(j_vc(irow)))
			!if(j_vc(irow).ne.0.)then
			!	if(j_maxo.eqv.j_vc(irow).gt.0.)j_buf(78:78)='U'
			!	if(j_maxo.eqv.j_vc(irow).lt.0.)j_buf(78:78)='L'
			!end if !if(j_vc(irow).ne.0.)then

						if(jlp_rhs(irow).eq.jlp_rhs2(irow))then
							j_tempchar(le+10:le+22)= j_chr10(dble(jlp_rhs(irow)))
						else !if(jlp_rhs(irow).eq.jlp_rhs2(irow))then
							if(jlp_lbou(irow))j_tempchar(le+7:le+17)= j_chr10(dble(jlp_rhs(irow)))
							if(jlp_ubou(irow))j_tempchar(le+18:le+28)= j_chr10(dble(jlp_rhs2(irow)))

						end if !if(jlp_rhs(irow).eq.jlp_rhs2(irow))then

					else if(j.eq.1) then !if(irow0.ne.irowobj)then
			! for maximization rhs1 =huge  rhs2=0
			! for minimization  rhs2=-huge
			!j_buf(36:)=j_chr10(dble(j_objf))

			!if(j_v(ivfeasible)>0)then
						if(jlp_maxo)then
							j_tempchar(1:5)=' max'
						else !if(j_maxo)then
							j_tempchar(1:5)=' min'
						end if !if(j_maxo)then
			!else !if(j_v(ivfeasible)>0)then
			!	j_buf(1:5)=' '
			!	j_buf(6:33)='Infeasible, temporary object'
			!endif !if(j_v(ivfeasible)>0)then
					end if !if(irow0.ne.irowobj)then
					write(nureport,'(a)')j_tempchar(1:le+28)
		!	if(intapp)then
		!		j_buf=' '
		!		j_buf(36:)=j_chr10(j_solx(irow))
		!	write(6,'(a)')j_buf
		!	endif !if(intapp)then
				end do !do k=1,j_nsetr(i)
			enddo domloopp !domloopp: do j=1,j_nsetd(i)
		enddo !do i=1,nset

	end subroutine !subroutine printproblem()
	subroutine printcur()
!write(17,*)'kier,iter',kier,iter
 
	end subroutine !subroutine printcur()
	logical function isxkzero(ixkyk)
		integer ikey_,ikeepxkyk_,k_
		isxkzero=.true.
		ikeepxkyk_ = jlp_ixkykkeep(ixkyk)
		ikey_=jlp_ibaunit(iunit) + jlp_keys(iunit)
		ibakey_=xdatiba(ikey_) !,2)
		if(j_o(ivxmat)%d(ibakey_+ikeepxkyk_).ne.0.)then
			isxkzero=.false.
			return
		endif !if(j_o(ivxmat)%d(ibakey_+ikeepxkyk_).ne.0.)then
		do k_=1,jlp_ld0
			if(jlp_lunit(jlp_ld(k_)).ne.iunit) cycle
			ikey_=jlp_ibaunit(iunit) + jlp_isch(jlp_ld(k_))
			ibakey_=xdatiba(ikey_) !,2)
			if(j_o(ivxmat)%d(ibakey_+ikeepxkyk_).ne.0.)then
				isxkzero=.false.
				return
			endif !if(j_o(ivxmat)%d(ibakey_+ikeepxkyk_).ne.0.)then
		enddo !do k_=1,j_ld0
	end function !logical function isxkzero(ixkyk)

!funktio palauttaa vaihtoehdon arvon tehdasosuuden
! Sama laskenta oli aiemmin sellaisenaan vaihtoehtosilmukassa, nyt kaksi vaihtoehtosilmukkaa
! (normaali / slow improvment jälkitila), joista funktiota kutsutaan.
! iobs: vaihtoehdon indeksi (kaikkien yksiköiden vaihtoehtojen joukossa)
	function value_f(iobs)
 
		real*8              :: value_f
		integer, intent(in) :: iobs

		value_f = 0.
		ibaobs=xdatiba(iobs) !,1)
		do jjj = 1,nrowxkfkey
			irowfkeep_ = jlp_irowfkeep(jlp_rowxkfkey(jjj)%irowfx)
			if (jlp_rowxkfkey(jjj)%jcurix == 0) then
				if(jlp_feasible) then
					value_f = value_f + jlp_coeffx(jlp_rowxkfkey(jjj)%irowfx)*&
						j_o(ivxmat)%d(ibaobs+irowfkeep_)
					if(p)write(n16,*)'value_f, coeff, xkf ',&
						value_f, jlp_coeffx(jlp_rowxkfkey(jjj)%irowfx),j_o(ivxmat)%d(ibaobs+irowfkeep_)
				endif !if(j_feasible) then
			else !if (j_rowxkfkey(j_)%jcurix == 0) then
				value_f = value_f - jlp_vc(jlp_rowxkfkey(jjj)%jcurix)*jlp_coeffx(jlp_rowxkfkey(jjj)%irowfx)*&
					j_o(ivxmat)%d(ibaobs+irowfkeep_)
	!     if(p)write(n16,*)'value_f,vc,coeff,xkf',&
		!        value_f,j_vc(j_rowxkfkey(j_)%jcurix),j_coeffx(j_rowxkfkey(j_)%irowfx),&
		!          j_o(ivxmat)%d(xdatiba(iobs)+irowfkeep_)
			endif !if (j_rowxkfkey(j_)%jcurix == 0) then
		enddo !do j_ = 1,nrowxkfkey

		do jj=1,nrowykfkey
			iv2elpos_ = jlp_rowykfkey(jj)%iv2elpos
			if (jlp_rowykfkey(jj)%jcurix.eq.0) then ! 0-rivi, ei v-kerrointa
				if(jlp_feasible) then
					value_f = value_f + j_v(jlp_rowykfkey(jj)%ivfout)*&
						j_o(ivxmat)%d(ibaobs+iv2elpos_)
					if(p)write(n16,*)'value_f, gamma, ykf',value_f, j_v(jlp_rowykfkey(jj)%ivfout),&
						j_o(ivxmat)%d(ibaobs+iv2elpos_)
				endif !if(j_feasible) then
			else !if (j_rowykfkey(j_)%jcurix.eq.0) then
				value_f = value_f - jlp_vc(jlp_rowykfkey(jj)%jcurix)* j_v(jlp_rowykfkey(jj)%ivfout)*&
					j_o(ivxmat)%d(ibaobs+iv2elpos_)
				if(p)write(n16,*)'value_f,vc,gamma,ykf',value_f,jlp_vc(jlp_rowykfkey(jj)%jcurix),&
					j_v(jlp_rowykfkey(jj)%ivfout),j_o(ivxmat)%d(ibaobs+iv2elpos_)
			endif !if (j_rowykfkey(j_)%jcurix.eq.0) then
		enddo !do jj=1,nrowykfkey

	end function value_f !function value_f(iobs)


	subroutine lcursam()
 
		lcursame=0
		lcur=jlp_next(0)
		lcur0=lcur
		do i=1,jlp_ld0
			nex=jlp_next(lcur)
			if(jlp_lunit(lcur).eq.iunit)then
				lcursame=lcur
				return
			end if !if(j_lunit(lcur).eq.iunit)then
		end do !do i=1,j_ld0
		return
	end subroutine !subroutine lcursam()

	subroutine testld()
		if(.not.jlp_xpresent)return
		do i=1,jlp_ld0
! löytyykö jokainen nextin kautta
			nex=0
			jj=1
766		nex=jlp_next(nex)
			if(nex.eq.jlp_ld(i))goto 767   ! löyty
			if(jj.gt.jlp_ld0.or.nex.eq.0)then
				write(n16,*)'saa  **','ld',(jlp_ld(j),j=1,jlp_ld0)
				write(n16,*)'next',jlp_next(0),jlp_next(jlp_next(0)),jlp_next(jlp_next(jlp_next(0)))&
					,	 jlp_next(jlp_next(jlp_next(jlp_next(0))))
				write(n16,*)'*s* iprev',jlp_iprev(0),jlp_iprev(jlp_iprev(0)),&
					jlp_iprev(jlp_iprev(jlp_iprev(0))),jlp_iprev(jlp_iprev(jlp_iprev(jlp_iprev(0))))
				write(6,*)'*s* -return'
				close(16)
				j_err=.true.
				return
			end if !if(jj.gt.j_ld0.or.nex.eq.0)then
			jj=jj+1
			goto 766
767		continue
		end do !do i=1,j_ld0
! end logical testing
	end subroutine !subroutine testld()

	subroutine testcol()
		write(6,*)'testvcol',jlp_xpresent,jlp_nrow,jlp_ld0

		if(.not.jlp_xpresent)return
		do i=0,jlp_nrow
			jlp_testxps(i)=0.
			if(i.gt.0.and.jlp_ls(i).gt.jlp_nrowz)then
	! ollaan D-osassa
				do j=1,jlp_ld0
					if(jlp_ls(i)-jlp_nrowz.eq.jlp_ld(j))goto 788 !col löytyi ld:stä
				end do !do j=1,j_ld0
				write(n16,*)'*col*',jlp_ls(i),'not in',(jlp_ld(j),j=1,jlp_ld0)
	!				close(16)
				j_err=.true.
				write(6,*)'*col* -return'
	!        return
	788		  continue
			end if !if(i.gt.0.and.j_ls(i).gt.j_nrowz)then
		end do !do i=0,j_nrow

		do i=1,jlp_nunits
			call jlpcurix(i)
			iobs=jlp_ibaunit(i)+jlp_keys(i)
			ibax=jxmatiba(iobs) !,1)
			if(jlp_keys(i).gt.jlp_nsch(i))write(n16,*)'***perk',i,jlp_keys(i),jlp_nsch(i) !laiton key
			do j=0,jlp_nrow
				if(jlp_ixcur(j).ne.0)jlp_testxps(j)=jlp_testxps(j)+jlp_xmat(jlp_ix(j)+ibax) !v(ix(j)) !lasketaan keysumma
			end do !do j=0,j_nrow
		end do !do i=1,jlp_nunits
		write(n16,*)'   ***xps',(jlp_xps(j),j=0,jlp_nrow)
		write(n16,*)'   ***test',(jlp_testxps(j),j=0,jlp_nrow) !pitäs olla sama

		do i=1,jlp_ld0
			write(n16,*)'  dcol',jlp_ld(i),'unit=',jlp_lunit(jlp_ld(i)),'sch=',&
				jlp_isch(jlp_ld(i)),'weight',jlp_x(jlp_ld(i)+jlp_nrowz)
			write(n16,*)'  val',jlp_objr(jlp_ld(i)+jlp_nrowz),(jlp_a(j,jlp_ld(i)+nz),j=1,jlp_nrow)

			iobs=jlp_ibaunit(jlp_lunit(jlp_ld(i)))+jlp_keys(jlp_lunit(jlp_ld(i)))
			ibax=jxmatiba(iobs) !,1)
			call jlpcurix(jlp_lunit(jlp_ld(i)))
			do j=0,jlp_nrow

				if(jlp_ixcur(j).ne.0) jlp_test(j)=&
					jlp_xmat(jlp_ix(j)+ibax ) !v(ix(j))
			end do !do j=0,j_nrow
			iobs=jlp_ibaunit(jlp_lunit(jlp_ld(i)))+jlp_isch(jlp_ld(i))
			ibax=jxmatiba(iobs) !,1)
			do j=0,jlp_nrow
				if(jlp_ixcur(j).ne.0) jlp_test(j)=jlp_xmat(jlp_ix(j)+ibax) -jlp_test(j) !v(ix(j))-test(j)
				jlp_testxps(j)=jlp_testxps(j)+jlp_x(jlp_ld(i)+jlp_nrowz)*jlp_test(j)
			end do !do j=0,j_nrow
			write(n16,*)'  dcoltest',(jlp_test(j),j=0,jlp_nrow)
			if(jlp_x(jlp_ld(i)+jlp_nrowz).LT.0.OR.jlp_x(jlp_ld(i)+jlp_nrowz).GT.1.)then
				write(6,*)'w:',jlp_x(jlp_ld(i)+jlp_nrowz)
				write(6,*)'*dcoltest* return'
				j_err=.true.
			end if !if(j_x(j_ld(i)+j_nrowz).LT.0.OR.j_x(j_ld(i)+j_nrowz).GT.1.)then
		end do !do i=1,j_ld0
		write(n16,*)'*first nonbasic column*:',icolold,'*in a matrix',icolold.gt.jlp_nrow
		write(n16,*)'  col',(jlp_a(j,icolold-jlp_nrowz),j=1,jlp_nrow)

		write(n16,*)'  ***rowtest',(jlp_testxps(j),j=0,jlp_nrow)
		write(n16,*)'  next',jlp_next(0),jlp_next(jlp_next(0)),jlp_next(jlp_next(jlp_next(0))),&
			jlp_next(jlp_next(jlp_next(jlp_next(0))))
		write(n16,*)'  iprev',jlp_iprev(0),jlp_iprev(jlp_iprev(0)),&
			jlp_iprev(jlp_iprev(jlp_iprev(0))),jlp_iprev(jlp_iprev(jlp_iprev(jlp_iprev(0))))
		write(n16,*)'*end test*'
		nex=0
		lunv=-1
! testataan w:iden summautumista smamassa yksikössä
555	continue
!   3  0   1    5    5  2

!      22       20      22
		nex=jlp_next(nex)
		if(nex.eq.0)goto 556
		if(jlp_lunit(nex).ne.lunv)then
			wss=jlp_x(nex+jlp_nrowz)
			lunv=jlp_lunit(nex)
		else !if(j_lunit(nex).ne.lunv)then
			wss=wss+jlp_x(nex+jlp_nrowz)
			if(wss.gt.1.)then
				write(n16,*)'*nex*',nex,wss
				close(16)
				write(6,*)'*nex* return'
				j_err=.true. ;return
			end if !if(wss.gt.1.)then
		end if !if(j_lunit(nex).ne.lunv)then
		goto 555  !next column in the ordering
556 write(n16,*)'muutosb',muutosb
! testaus lopui
	end subroutine !subroutine testcol()

!
!iunit_: yksikkö, joka käsittelyssä kutsuhetkellä -> palautetaan curixit oikeaan yksikköön
	subroutine testxpssub(iunit_)
 
		jlp_testxps=0.d0
		iunitrans=0

		do i_=1,jlp_nunits  !***********************
			call jlpcurix(i_) !determines for each row if the unit iunit belonggs to the domain of the row
			iobs_=jlp_ibaunit(i_)+jlp_keys(i_)
			ibax=jxmatiba(iobs_) !,1)
			ibaobs_=xdatiba(iobs_) !,1)
			do jj_=1,jlp_nrowp
				jj=jlp_ixcurrows(jj_)
	!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
				jlp_testxps(jj)=jlp_testxps(jj)+jlp_xmat(jlp_ix(jj)+ibax) !(ix(j)=0 -> no x in row
	!                if(ixcur(j).ne.0)xps(j)=xps(j)+xmat(ibakey+ix(j))
			enddo !do jj_=1,j_nrowp

!tehdasmuuttujien xps-laskenta
			if(jlp_fpresent) then
				call jlpfcurix(i_)
				call jlpfcurixy(i_)

	!xk-tehdasmuuttujat
				do jj_=1,jlp_nrowpf
					jjj=jlp_ifxcurrows(jj_) !domainissa olevat rivit
					irowj_ = jlp_irowrow(jjj)
		!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
					do k_=1,jlp_nfxrow(irowj_)
						if(jlp_keyfact(i_,jlp_irowfxvars(jlp_ibafx(irowj_)+k_)).eq.jlp_irowffact(jlp_ibafx(irowj_)+k_))then
							jlp_testxps(jjj)=jlp_testxps(jjj)+ jlp_coeffx(jlp_ibafx(irowj_)+k_)*&
								j_o(ivxmat)%d(ibaobs_+jlp_irowfkeep(jlp_ibafx(irowj_)+k_))
						endif !if(j_keyfact(i_,j_irowfxvars(j_ibafx(irowj_)+k_)).eq.j_irowffact(j_ibafx(irowj_)+k_))then
					enddo !do k_=1,j_nfxrow(irowj_)
				enddo !do jj_=1,j_nrowpf

				if (nfy.gt.0) then
					do jjj=1,j_o(jlp_ivkeepc)%i(1)
						j_v(j_o(jlp_ivkeepc)%i2(jjj))=j_o(ivcmat)%d((i_-1)*j_o(jlp_ivkeepc)%i(1)+jjj)
					enddo !do jjj=1,j_o(j_ivkeepc)%i(1)
					do jjj=1,ntrans
						call dotrans(jlp_itransv(jjj),1)
						if(j_err)then
							write(6,*)'err for trans ',j
							stop 741
						endif !if(j_err)then
					enddo !do j_=1,ntrans
				endif !if (nfy.gt.0) then


				do jj_=1,jlp_nrowpfy
		!ivn-laskurin käsittely ei toimi, jos mukana on domaineja
					jjj=jlp_ifycurrows(jj_) !domainissa olevat rivit
					irowj_ = jlp_irowrow(jjj)
		!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
					do k_=1,jlp_nfyrow(irowj_)
						listy_=jlp_irowfyvars(jlp_ibafy(irowj_)+k_)
						listf_=jlp_irowfyfact(jlp_ibafy(irowj_)+k_)

						do ivars_=1,j_o(listy_)%i(1)
				!mjan xmat-sarake
							iv2elpos_ = jlp_ifyvarskeep(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1)
				!listy-listan ivars_:innen ptl-mjan paikka (järjestysnumero) xkyk-listassa
							iv2xykypos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1)

							do ifact_=1,j_o(listf_)%i(1)
								iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj_)+k_)+ifact_-1)

								if(jlp_keyfact(i_,iv2xykypos_).eq.iv3factpos_) then
									jlp_testxps(jjj)=jlp_testxps(jjj)+ j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_))*&
										j_o(ivxmat)%d(ibaobs_+iv2elpos_) !xdatiba
								endif !if(j_keyfact(i_,iv2xykypos_).eq.iv3factpos_) then

							enddo !do ifact_=1,j_o(listf_)%i(1)
						enddo !do ivars_=1,j_o(listy_)%i(1)

					enddo !do k_=1,j_nfyrow(irowj_)
				enddo !do jj_=1,j_nrowpfy

			endif !if(j_fpresent) then
		enddo !do i_=1,jlp_nunits

!palautetaan käsiteltävänä olevan yksikön curixit ja tehdasmuunnokset
		if(iunit_>0) then
			iunitrans = iunit_
			call jlpcurix(iunit_)
			if(jlp_fpresent) then
				call jlpfcurix(iunit_)
				call jlpfcurixy(iunit_)
				if (nfy.gt.0) then
					do jjj=1,j_o(jlp_ivkeepc)%i(1)
						j_v(j_o(jlp_ivkeepc)%i2(jjj))=j_o(ivcmat)%d((iunit_-1)*j_o(jlp_ivkeepc)%i(1)+jjj)
					enddo !do jjj=1,j_o(j_ivkeepc)%i(1)
					do jjj=1,ntrans
						call dotrans(jlp_itransv(jjj),1)
						if(j_err)then
							write(6,*)'error for trans ',j
							stop 741
						endif !if(j_err)then
					enddo !do jjj=1,ntrans
				endif !if (nfy.gt.0) then
			endif !if(j_fpresent) then
		endif !if(iunit_>0) then

!write(n16,*)'**fact** testxps',(j_testxps(j),j=0,j_nrow)
		nero=0
		do iii=1,jlp_nrow
			if(abs(jlp_testxps(iii)-jlp_xps(iii)).gt.0.001)then
				nero=nero+1
				if(nero.lt.10)write(n16,*)'row,test,xps', iii,jlp_testxps(iii),jlp_xps(iii)
			endif !if(abs(j_testxps(iii)-j_xps(iii)).gt.0.001)then
		enddo !do iii=1,j_nrow
		write(n16,*)'testxps', nero

	end subroutine !subroutine testxpssub(iunit_)

	subroutine testobjr0()
		write(n16,*)'**fact** testobjr0/w'
		iunitrans = 0

		do ldj_=1,jlp_ld0

			jlp_test = 0.d0

			iunit_=jlp_lunit(jlp_ld(ldj_))
			ikey_=jlp_ibaunit(jlp_lunit(jlp_ld(ldj_)))+ jlp_keys(jlp_lunit(jlp_ld(ldj_)))
			iobs_=jlp_ibaunit(jlp_lunit(jlp_ld(ldj_)))+ jlp_isch(jlp_ld(ldj_))
			ibax=jxmatiba(iobs_) !,1)
			ibaxkey_=jxmatiba(ikey_) !,2)
			ibaobs_=xdatiba(iobs_) !,1)
			ibakey_=xdatiba(ikey_) !,2)
			write(n16,*)'ld(j),iunit,iobs,ikey',jlp_ld(ldj_),iunit_,iobs_,ikey_

			do it_=0,jlp_nrow
				if(jlp_ixcur(it_).ne.0) then
					jlp_test(it_) = jlp_xmat(jlp_ix(it_)+ibax)- jlp_xmat(jlp_ix(it_)+ibaxkey_)
				endif !if(j_ixcur(it_).ne.0) then
			enddo !do it_=0,j_nrow

			if (jlp_fpresent) then
	!iperk=xdatiba(-4)
				do jjj=1,jlp_nrowpf ! rivit, joilla xk-muuttujia
					jcurix_=jlp_ifxcurrows(jjj)
					irowj_ = jlp_irowrow(jcurix_)
					do k_=1,jlp_nfxrow(irowj_) ! silmukka : rivin xk-muuttujat
			!value = value+alfa*_xdata_(keepx,iobs)
						if(jlp_keyfact(iunit_,jlp_irowfxvars(jlp_ibafx(irowj_)+k_)).eq.jlp_irowffact(jlp_ibafx(irowj_)+k_)) then
							jlp_test(jcurix_) = jlp_test(jcurix_) + jlp_coeffx(jlp_ibafx(irowj_)+k_)*&
								(j_o(ivxmat)%d(ibaobs_+jlp_irowfkeep(jlp_ibafx(irowj_)+k_)) -&
								j_o(ivxmat)%d(ibakey_+jlp_irowfkeep(jlp_ibafx(irowj_)+k_)))
						endif !if(j_keyfact(iunit_,j_irowfxvars(j_ibafx(irowj_)+k_)).eq.j_irowffact(j_ibafx(irowj_)+k_)) then
					enddo !do k_=1,j_nfxrow(irowj_)
				enddo !do jjj=1,j_nrowpf

	!y-mjien muunnokset
				if (nfy.gt.0) then
					do jjj=1,j_o(jlp_ivkeepc)%i(1)
						j_v(j_o(jlp_ivkeepc)%i2(jjj))=j_o(ivcmat)%d((iunit_-1)*j_o(jlp_ivkeepc)%i(1)+jjj)
					enddo !do jjj=1,j_o(j_ivkeepc)%i(1)
					do jjj=1,ntrans
						call dotrans(jlp_itransv(jjj),1)
						if(j_err)then
							write(6,*)' err in tr ',j
							stop 614
						endif !if(j_err)then
					enddo !do jjj=1,ntrans
				endif !if (nfy.gt.0) then

				do jjj=1,jlp_nrowpfy ! yk-muuttujat
					jcurix_=jlp_ifycurrows(jjj)
					irowj_ = jlp_irowrow(jcurix_)
					do k_=1,jlp_nfyrow(irowj_) ! rivin yk-muuttujat
						listy_=jlp_irowfyvars(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava ptl-lista
						listf_=jlp_irowfyfact(jlp_ibafy(irowj_)+k_) !yk-mjaa vastaava tehdas-lista
						do ivars_=1,j_o(listy_)%i(1) ! yk-mjan puutavaralistan muuttujat
							iv2elpos_ = jlp_ifyvarskeep(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan xmat-sarake
							iv2xykypos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1) !mjan paikka xkyk-listassa
							do ifact_=1,j_o(listf_)%i(1) ! yk-mjan tehdaslistan tehtaat
								iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj_)+k_)+ifact_-1) !tehtaan paikka factories-listassa
					!value = value+gamma*_xdata_(keepx,iobs)
								if(jlp_keyfact(iunit_,iv2xykypos_).eq.iv3factpos_) then
									jlp_test(jcurix_) = jlp_test(jcurix_) +j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_))*&
										(j_o(ivxmat)%d(ibaobs_+iv2elpos_) -&
										j_o(ivxmat)%d(ibakey_+iv2elpos_))
								endif !if(j_keyfact(iunit_,iv2xykypos_).eq.iv3factpos_) then
							enddo !do ifact_=1,j_o(listf_)%i(1)
						enddo !do ivars_=1,j_o(listy_)%i(1)
					enddo !do k_=1,j_nfyrow(irowj_)
				enddo !do jjj=1,j_nrowpfy

			endif !if (j_fpresent) then

			write(n16,*)'ld(j)',jlp_ld(ldj_),'test(0:nrow)',(jlp_test(jjj),jjj=0,jlp_nrow)

		enddo !do ldj_=1,j_ld0
	end subroutine !subroutine testobjr0()


	subroutine testobjr0f()
		write(n16,*)'**fact** testobjr0/w'
		iunitrans=0

		do lfj_=1,jlp_lf0
			jlp_test = 0.d0

			iunit_=jlp_lunit(jlp_lf(lfj_))
			ixkyk_=jlp_ixkf(jlp_lf(lfj_))
			ifact_=jlp_ixkffact(jlp_lf(lfj_))
			ikeyf_=jlp_keyfact(iunit_,ixkyk_)

!? c-muunnokset
			write(n16,*)'lf(j),iunit,ixkyk,ifact,ikeyfact',jlp_lf(lfj_),iunit_,ixkyk_,ifact_,ikeyf_

			do jjj=1,jlp_nrowpf ! rivit, joilla xk-muuttujia
				jcurix_=jlp_ifxcurrows(jjj)
				irowj_ = jlp_irowrow(jcurix_)
				do k_=1,jlp_nfxrow(irowj_) ! silmukka : rivin xk-muuttujat
					if((jlp_irowfxvars(jlp_ibafx(irowj_)+k_)==ixkyk_).and.(ifact_.eq.jlp_irowffact(jlp_ibafx(irowj_)+k_))) then
						jlp_test(jcurix_)=jlp_test(jcurix_) + jlp_coeffx(jlp_ibafx(irowj_)+k_)
					endif !if((j_irowfxvars(j_ibafx(irowj_)+k_)==ixkyk_).and.(ifact_.eq.j_irowffact(j_ibafx(irowj_)+k_))) then
					if((jlp_irowfxvars(jlp_ibafx(irowj_)+k_)==ixkyk_).and.(ifact_.eq.jlp_keyfact(iunit_,ixkyk_))) then
						jlp_test(jcurix_)=jlp_test(jcurix_) - jlp_coeffx(jlp_ibafx(irowj_)+k_)
					endif !if((j_irowfxvars(j_ibafx(irowj_)+k_)==ixkyk_).and.(ifact_.eq.j_keyfact(iunit_,ixkyk_))) then
				enddo !do k_=1,j_nfxrow(irowj_)
			enddo !do jjj=1,j_nrowpf

			if (nfy.gt.0) then
				do jjj=1,j_o(jlp_ivkeepc)%i(1)
					j_v(j_o(jlp_ivkeepc)%i2(jjj))=j_o(ivcmat)%d((iunit_-1)*j_o(jlp_ivkeepc)%i(1)+jjj)
				enddo !do jjj=1,j_o(j_ivkeepc)%i(1)
				do jjj=1,ntrans
					call dotrans(jlp_itransv(jjj),1)
					if(j_err)then
						write(6,*)'err in tr ',j
						stop 711

					endif !if(j_err)then
				enddo !do jjj=1,ntrans
			endif !if (nfy.gt.0) then

			do jjj=1,jlp_nrowpfy ! yk-muuttujarivit
				jcurix_=jlp_ifycurrows(jjj)
				irowj_ = jlp_irowrow(jcurix_)
				do k_=1,jlp_nfyrow(irowj_)
					listy_=jlp_irowfyvars(jlp_ibafy(irowj_)+k_)
					listf_=jlp_irowfyfact(jlp_ibafy(irowj_)+k_)

					do ivars_=1,j_o(listy_)%i(1)
						iv2xykypos_ = jlp_ifyvarsxkyk(jlp_ibafykeep(jlp_ibafy(irowj_)+k_)+ivars_-1)
						do ifact_=1,j_o(listf_)%i(1)
							iv3factpos_ = jlp_ifyfactfact(jlp_ibafyfact(jlp_ibafy(irowj_)+k_)+ifact_-1)
							if((iv2xykypos_==ixkyk_).and.(ifact_.eq.iv3factpos_)) then
								jlp_test(jcurix_)=jlp_test(jcurix_) + j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_))
							endif !if((iv2xykypos_==ixkyk_).and.(ifact_.eq.iv3factpos_)) then
							if((iv2xykypos_==ixkyk_).and.(ifact_.eq.jlp_keyfact(iunit_,ixkyk_))) then
								jlp_test(jcurix_)=jlp_test(jcurix_) - j_v(jlp_fyfactout(iv2xykypos_,iv3factpos_))
							endif !if((iv2xykypos_==ixkyk_).and.(ifact_.eq.j_keyfact(iunit_,ixkyk_))) then
						enddo !do ifact_=1,j_o(listf_)%i(1)
					enddo !do ivars_=1,j_o(listy_)%i(1)

				enddo !do k_=1,j_nfyrow(irowj_)
			enddo !do jjj=1,j_nrowpfy

			write(n16,*)'lf(j)',jlp_lf(lfj_),'test(0:nrow)',(jlp_test(jjj),jjj=0,jlp_nrow)

		enddo !do lfj_=1,j_lf0

	end subroutine !subroutine testobjr0f()


	subroutine getsolx() !
!computes the sums of all x-variables in all domains
 
 
		if(nz.eq.0)then
!!compute integer approximation for rows
			jlp_solx=0.d0
			do i=1,jlp_nunits  !***********************

				call jlpcurix(i)  !!determines for each row if the unit iunit belonggs to the domain of the row
	!returns nrowp=number of rows in this domain,
	! ixcurrows= for each row in this domain tells the original (expanded) row
	!  ixcur for each original expanded row tells the index of temporary x-variable
				ibax=jxmatiba(jlp_ibaunit(i)+integerschedw(i))  !,1)
				do jj=1,jlp_nrowp
					j=jlp_ixcurrows(jj)
					jlp_solx(j)=jlp_solx(j)+jlp_xmat(jlp_ix(j)+ibax)
				enddo !do jj=1,j_nrowp
			enddo !do i=1,jlp_nunits
		endif !if(nz.eq.0)then

		jlp_nxvartot=j_o(jlp_ivkeepx)%i(1)
		if(jlp_ivsubtrans>0) then
			ivoutsubtrans = j_trans_output(jlp_ivsubtrans)
			noutsubtrans = j_o(ivoutsubtrans)%i2(1)
!write(6,*)'<55>',ivoutsubtrans,noutsubtrans
		else !if(jlp_ivsubtrans>0) then
			noutsubtrans = 0
		endif !if(jlp_ivsubtrans>0) then

		nxvarareatot=0
		jlp_nsumx=jlp_nxvartot+ncvar+noutsubtrans
		ipart=0
		ndom2=max(jlp_ndom,1)
		if(allocated(jlp_sumx))deallocate(jlp_sumx,jlp_shpx,jlp_sumxi)
		allocate( jlp_sumx(1:jlp_nsumx*ndom2),jlp_shpx(1:jlp_nsumx*ndom2),jlp_sumxi(1:jlp_nsumx*ndom2))
		if(allocated(jlp_xvarlareatot))deallocate(jlp_xvarlareatot)
		if(ivarea.gt.0.and.notareavars.gt.0)then
			nxvarareatot=j_ndiffer(j_o(jlp_ivkeepx)%i2,jlp_nxvartot,j_o(iob)%i(linknotareavars+1:linknotareavars+notareavars), &
			notareavars)
			notc=j_ndiffer(jlp_cvarl,ncvar,j_o(iob)%i(linknotareavars+1:linknotareavars+notareavars),notareavars)
			if(nxvarareatot.lt.jlp_nxvartot.or.notc.gt.0)then
				allocate(jlp_xvarlareatot(1:nxvarareatot+notc))
				call j_uniondif(j_o(jlp_ivkeepx)%i2,jlp_nxvartot,jlp_cvarl,ncvar,&
					j_o(iob)%i(linknotareavars+1:linknotareavars+notareavars),notareavars,&
					jlp_xvarlareatot,nxvarareatot)
				ipart=1
			endif !if(nxvarareatot.lt.j_nxvartot.or.notc.gt.0)then
		elseif(ivarea.gt.0)then !if(ivarea.gt.0.and.notareavars.gt.0)then
			nxvarareatot = jlp_nxvartot + noutsubtrans
			allocate(jlp_xvarlareatot(1:nxvarareatot))
			jlp_xvarlareatot(1:jlp_nxvartot)= j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot)
			if(noutsubtrans>0) jlp_xvarlareatot(jlp_nxvartot+1:nxvarareatot)=j_o(ivoutsubtrans)%i2(1:noutsubtrans)
		endif !if(ivarea.gt.0.and.notareavars.gt.0)then
		jlp_sumx=0.d0
		do iuni=1,jlp_nunits
			call j_getobsiv(iuni,jlp_ivmatc,jlp_ivkeepc,jlp_ivunit) !,jlp_ivtransc,j_ivunit)
			if(j_err)then
				write(6,*)'error for unit ',iuni
			endif !if(j_err)then
			if(jlp_ivtrans.gt.0)then
				call dotrans(jlp_ivtrans,1)
				if(j_err)stop 770
			endif !if(jlp_ivtrans.gt.0)then
			iobs=jlp_ibaunit(iuni)+jlp_keys(iuni)+jlp_ibaunitbas
			call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0)! ,jlp_ivtransx,0)  !getsolx
			if(j_err)then
				write(6,*)'error for observation ',iobs
				stop 4
			endif !if(j_err)then
			if(jlp_ivsubtrans.gt.0)call dotrans(jlp_ivsubtrans,1)
			if(ivarea.gt.0)then
				if(ipart.eq.0)then

					j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))=j_v(ivarea)*j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))
					if(ncvar.gt.0)j_v(jlp_cvarl(1:ncvar))=j_v(ivarea)*j_v(jlp_cvarl(1:ncvar))
				else !if(ipart.eq.0)then
					j_v(jlp_xvarlareatot)=j_v(ivarea)*j_v(jlp_xvarlareatot)
				endif !if(ipart.eq.0)then
			endif !if(ivarea.gt.0)then
			if(jlp_ndom.gt.0)then
				iba=0
				do id=1,jlp_ndom
					icurint=(id-1)/32+1;icurbit=id-(icurint-1)*32-1
					if(btest(jlp_domainbits(icurint,iuni),icurbit))then
			!         ndomsol(id)=ndomsol(id)+1

						do jx=1,jlp_nxvartot
							jlp_sumx(iba+jx)=jlp_sumx(iba+jx)+ j_v(j_o(jlp_ivkeepx)%i2(jx))
						end do !do jx=1,j_nxvartot

						if(ncvar.gt.0)jlp_sumx(iba+jlp_nxvartot+1:iba+jlp_nxvartot+ncvar)= &
							jlp_sumx(iba+jlp_nxvartot+1:iba+jlp_nxvartot+ncvar)+j_v(jlp_cvarl(1:ncvar))

						if(jlp_ivsubtrans>0) jlp_sumx(iba+jlp_nxvartot+ncvar+1:iba+jlp_nxvartot+ncvar+noutsubtrans) = &
							jlp_sumx(iba+jlp_nxvartot+ncvar+1:iba+jlp_nxvartot+ncvar+noutsubtrans) + &
							j_v(j_o(ivoutsubtrans)%i2(1:noutsubtrans))

					end if !if(btest(j_domainbits(icurint,iuni),icurbit))then
					iba=iba+jlp_nxvartot+ncvar+noutsubtrans
				end do !do id=1,j_ndom
			else !if(j_ndom.gt.0)then

				jlp_sumx(1:jlp_nxvartot)= jlp_sumx(1:jlp_nxvartot)+j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))
				if(ncvar.gt.0)jlp_sumx(jlp_nxvartot+1:jlp_nxvartot+ncvar)= &
				jlp_sumx(jlp_nxvartot+1:jlp_nxvartot+ncvar)+j_v(jlp_cvarl(1:ncvar))

				if(jlp_ivsubtrans>0) jlp_sumx(jlp_nxvartot+ncvar+1:jlp_nxvartot+ncvar+noutsubtrans) = &
					jlp_sumx(jlp_nxvartot+ncvar+1:jlp_nxvartot+ncvar+noutsubtrans) + &
					j_v(j_o(ivoutsubtrans)%i2(1:noutsubtrans))
			end if !if(j_ndom.gt.0)then
		end do !do iuni=1,jlp_nunits

		jlp_sumxi=0.d0
		do iuni=1,jlp_nunits
			call j_getobsiv(iuni,jlp_ivmatc,jlp_ivkeepc,jlp_ivunit)! ,jlp_ivtransc,j_ivunit)
			if(j_err)then
				write(6,*)'error for unit ',iuni
				stop 512
			endif !if(j_err)then
			if(jlp_ivtrans.gt.0)then
				call dotrans(jlp_ivtrans,1)
				if(j_err)stop 651

			endif !if(jlp_ivtrans.gt.0)then
			iobs=jlp_ibaunit(iuni)+integerschedw(iuni)   !+j_ibaunitbas
			call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0)! ,jlp_ivtransx,0)  !getsolx
			if(j_err)then
				write(6,*)'error for observation ',iobs
			endif !if(j_err)then
			if(jlp_ivsubtrans.gt.0)then
				call dotrans(jlp_ivsubtrans,1)
				if(j_err)stop 719
			endif !if(jlp_ivsubtrans.gt.0)then
			if(ivarea.gt.0)then
				if(ipart.eq.0)then

					j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))=j_v(ivarea)*j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))
					if(ncvar.gt.0)j_v(jlp_cvarl(1:ncvar))=j_v(ivarea)*j_v(jlp_cvarl(1:ncvar))
				else !if(ipart.eq.0)then
					j_v(jlp_xvarlareatot)=j_v(ivarea)*j_v(jlp_xvarlareatot)
				endif !if(ipart.eq.0)then
			endif !if(ivarea.gt.0)then

			if(jlp_ndom.gt.0)then
				iba=0
				do id=1,jlp_ndom
					icurint=(id-1)/32+1;icurbit=id-(icurint-1)*32-1
					if(btest(jlp_domainbits(icurint,iuni),icurbit))then
			!         ndomsol(id)=ndomsol(id)+1

						do jx=1,jlp_nxvartot
							jlp_sumxi(iba+jx)=jlp_sumxi(iba+jx)+ j_v(j_o(jlp_ivkeepx)%i2(jx))
						end do !do jx=1,j_nxvartot

						if(ncvar.gt.0)jlp_sumxi(iba+jlp_nxvartot+1:iba+jlp_nxvartot+ncvar)= &
							jlp_sumxi(iba+jlp_nxvartot+1:iba+jlp_nxvartot+ncvar)+j_v(jlp_cvarl(1:ncvar))

						if(jlp_ivsubtrans>0) jlp_sumxi(iba+jlp_nxvartot+ncvar+1:iba+jlp_nxvartot+ncvar+noutsubtrans) = &
							jlp_sumxi(iba+jlp_nxvartot+ncvar+1:iba+jlp_nxvartot+ncvar+noutsubtrans) + j_v(j_o(ivoutsubtrans)%i2(1:noutsubtrans))

					end if !if(btest(j_domainbits(icurint,iuni),icurbit))then

					iba=iba+jlp_nxvartot+ncvar+noutsubtrans
				end do !do id=1,j_ndom
			else !if(j_ndom.gt.0)then

				jlp_sumxi(1:jlp_nxvartot)= jlp_sumxi(1:jlp_nxvartot)+j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))

				if(ncvar.gt.0)jlp_sumxi(jlp_nxvartot+1:jlp_nxvartot+ncvar)= &
				jlp_sumxi(jlp_nxvartot+1:jlp_nxvartot+ncvar)+j_v(jlp_cvarl(1:ncvar))

				if(jlp_ivsubtrans>0) jlp_sumxi(jlp_nxvartot+ncvar+1:jlp_nxvartot+ncvar+noutsubtrans) = &
					jlp_sumxi(jlp_nxvartot+ncvar+1:jlp_nxvartot+ncvar+noutsubtrans) + j_v(j_o(ivoutsubtrans)%i2(1:noutsubtrans))
			end if !if(j_ndom.gt.0)then
		end do !do iuni=1,jlp_nunits

		do i=1,jlp_ld0

			wei=jlp_x(jlp_ld(i)+jlp_nrowz)
			iuni=jlp_lunit(jlp_ld(i))
			call j_getobsiv(iuni,jlp_ivmatc,jlp_ivkeepc,jlp_ivunit) !,jlp_ivtransc,j_ivunit)
			if(jlp_ivtrans.gt.0)call dotrans(jlp_ivtrans,1)
			iobs=jlp_ibaunit(iuni)+jlp_keys(iuni)+jlp_ibaunitbas
			call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0) !getsolx
			if(jlp_ivsubtrans.gt.0)call dotrans(jlp_ivsubtrans,1)
			if(j_err)return

			if(ivarea.gt.0)then
				if(ipart.eq.0)then
					j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))=j_v(ivarea)*j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))
				else !if(ipart.eq.0)then
					j_v(jlp_xvarlareatot)=j_v(ivarea)*j_v(jlp_xvarlareatot)
				endif !if(ipart.eq.0)then
			endif !if(ivarea.gt.0)then
			if(jlp_ndom.gt.0)then
				iba=0
				do id=1,jlp_ndom
					icurint=(id-1)/32+1;icurbit=id-(icurint-1)*32-1
					if(btest(jlp_domainbits(icurint,iuni),icurbit))then
						do jx=1,jlp_nxvartot
							jlp_sumx(iba+jx)=jlp_sumx(iba+jx)-wei* j_v(j_o(jlp_ivkeepx)%i2(jx))
						end do !do jx=1,j_nxvartot

						do jx=1,noutsubtrans
							jlp_sumx(iba+jlp_nxvartot+ncvar+jx)=jlp_sumx(iba+jlp_nxvartot+ncvar+jx) &
								- wei*j_v(j_o(ivoutsubtrans)%i2(jx))
						enddo !do jx=1,noutsubtrans
					end if !if(btest(j_domainbits(icurint,iuni),icurbit))then

					iba=iba+jlp_nxvartot+noutsubtrans
				end do !do id=1,j_ndom
			else !if(j_ndom.gt.0)then
				do jx=1,jlp_nxvartot
					jlp_sumx(jx)=jlp_sumx(jx)-wei* j_v(j_o(jlp_ivkeepx)%i2(jx))
				end do !do jx=1,j_nxvartot

				do jx=1,noutsubtrans
					jlp_sumx(jlp_nxvartot+ncvar+jx)=jlp_sumx(jlp_nxvartot+ncvar+jx) &
						- wei*j_v(j_o(ivoutsubtrans)%i2(jx))
				enddo !do jx=1,noutsubtrans
			end if !if(j_ndom.gt.0)then

			iobs=jlp_ibaunit(iuni)+jlp_isch(jlp_ld(i))+jlp_ibaunitbas
			call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0)  !getsolx
			if(jlp_ivsubtrans.gt.0)call dotrans(jlp_ivsubtrans,1)

			if(ivarea.gt.0)then
				if(ipart.eq.0)then  !if(nxvarareatot.eq.nxvartot)then
					j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))=j_v(ivarea)*j_v(j_o(jlp_ivkeepx)%i2(1:jlp_nxvartot))
				else !if(ipart.eq.0)then
					j_v(jlp_xvarlareatot)=j_v(ivarea)*j_v(jlp_xvarlareatot)
				endif !if(ipart.eq.0)then
			endif !if(ivarea.gt.0)then

			if(jlp_ndom.gt.0)then
				iba=0
				do id=1,jlp_ndom
					icurint=(id-1)/32+1;icurbit=id-(icurint-1)*32-1
					if(btest(jlp_domainbits(icurint,iuni),icurbit))then
						do jx=1,jlp_nxvartot
							jlp_sumx(iba+jx)=jlp_sumx(iba+jx)+wei* j_v(j_o(jlp_ivkeepx)%i2(jx))
						end do !do jx=1,j_nxvartot

						do jx=1,noutsubtrans
							jlp_sumx(iba+jlp_nxvartot+ncvar+jx)=jlp_sumx(iba+jlp_nxvartot+ncvar+jx) &
								+ wei*j_v(j_o(ivoutsubtrans)%i2(jx))
						enddo !do jx=1,noutsubtrans
					end if !if(btest(j_domainbits(icurint,iuni),icurbit))then

					iba=iba+jlp_nxvartot+noutsubtrans
				end do !do id=1,j_ndom
			else !if(j_ndom.gt.0)then
				do jx=1,jlp_nxvartot
					jlp_sumx(jx)=jlp_sumx(jx)+wei* j_v(j_o(jlp_ivkeepx)%i2(jx))
				end do !do jx=1,j_nxvartot

				do jx=1,noutsubtrans
					jlp_sumx(jlp_nxvartot+ncvar+jx)=jlp_sumx(jlp_nxvartot+ncvar+jx) &
						+ wei*j_v(j_o(ivoutsubtrans)%i2(jx))
				enddo !do jx=1,noutsubtrans

			end if !if(j_ndom.gt.0)then
		end do !do i=1,j_ld0

		return
	end subroutine !subroutine getsolx()

	subroutine defsolu()
 
		integer, dimension(:),allocatable::iperm,isdiv0
		double precision, dimension(:),allocatable::sortkey

		id=0
		nunv=-1
		nn=0
		do i=1,jlp_ld0
			id=jlp_next(id)
			if(jlp_lunit(id).ne.nunv)nn=nn+1
			nunv=jlp_lunit(id)
		end do !do i=1,j_ld0
!write(6,*)'number of basic residuals (=nonbinding constraints)',lr0
		if(jlp_xpresent2)write(6,*)'split units ',nn
!write(6,*)'number of explicit basic schedules ',j_ld0
		jlp_ndiv=jlp_ld0+nn
		if(allocated(jlp_iunitdiv))deallocate(jlp_iunitdiv);allocate( jlp_iunitdiv(1:jlp_ndiv))
		if(allocated(jlp_isdiv))deallocate(jlp_isdiv);allocate( jlp_isdiv(1:jlp_ndiv))
		if(allocated(jlp_wdiv))deallocate(jlp_wdiv);allocate( jlp_wdiv(1:jlp_ndiv))
		allocate(iperm(1:jlp_ndiv),isdiv0(1:jlp_ndiv),sortkey(1:jlp_ndiv))

		if(jlp_ld0.eq.0)return
		id=0
		sum=0.
		idiv=0

! keys 2 3 5 6
! div  2,1 4,7
		nunv=jlp_lunit(jlp_next(0))
		iplace=0
		do i=1,jlp_ld0
			id=jlp_next(id)
!	write(6,'(a,i6,a,i4,a,i4,a,f7.5)')'unit=',j_lunit(id),' key=',j_keys(j_lunit(id)),&
	!		' basic sched=',j_isch(id),' weight=',j_x(j_nrowz+id)
			if(jlp_lunit(id).ne.nunv)then
				if(iplace.eq.0)then   ! key is largest
					idiv=idiv+1;iplace=idiv
				end if !if(iplace.eq.0)then

				jlp_wdiv(iplace)=1.-sum     ! area

				isdiv0(iplace)=jlp_keys(nunv)
				jlp_iunitdiv(iplace)=nunv

				sum=0.
				iplace=0
			end if !if(j_lunit(id).ne.nunv)then
!
			if(jlp_isch(id).gt.jlp_keys(jlp_lunit(id)).and.iplace.eq.0)then   !reserve place for
				idiv=idiv+1
				iplace=idiv
			end if !if(j_isch(id).gt.j_keys(j_lunit(id)).and.iplace.eq.0)then
			idiv=idiv+1

			jlp_iunitdiv(idiv)=jlp_lunit(id)
			isdiv0(idiv)=jlp_isch(id)
			jlp_wdiv(idiv)=jlp_x(jlp_nrowz+id)
			sum=sum+jlp_x(jlp_nrowz+id)
			nunv=jlp_lunit(id)
		end do !do i=1,j_ld0

		if(iplace.eq.0)then   ! key is largest
			idiv=idiv+1;iplace=idiv
		end if !if(iplace.eq.0)then
		jlp_wdiv(iplace)=1.-sum
		jlp_iunitdiv(iplace)=nunv
		isdiv0(iplace)=jlp_keys(nunv)
		smax=maxval(isdiv0)
		do i=1,jlp_ndiv;iperm(i)=i;sortkey(i)=jlp_iunitdiv(i)+isdiv0(i)/smax;enddo

!jr  from Numerical Recipes
		call j_quick_sort(sortkey(1:jlp_ndiv),iperm)
		sortkey=jlp_wdiv
		do i=1,jlp_ndiv
			jlp_wdiv(i)=sortkey(iperm(i))
		enddo !do i=1,j_ndiv

		do i=1,jlp_ndiv
			jlp_isdiv(i)=isdiv0(iperm(i))
		end do !do i=1,j_ndiv
		isdiv0=jlp_iunitdiv
		do i=1,jlp_ndiv
			jlp_iunitdiv(i)=isdiv0(iperm(i))
		end do !do i=1,j_ndiv

		deallocate(iperm,isdiv0,sortkey)

		if(jlp_fpresent)write(6,*)'number of split transportations ',jlp_lf0-jlp_mxd

		return

	end  subroutine !subroutine defsolu()

! tehdasratkaisujen talteenotto
	subroutine defsoluf()
! prevcol_(i) = yksikön viimeksi silmukassa vastaantullut tehdaskantasrk
		integer, dimension(:), allocatable :: prevcol_
		if (allocated(prevcol_)) deallocate(prevcol_)
		allocate(prevcol_(1:jlp_nunits))
		prevcol_=jlp_mxd

		if (allocated(jlp_xkfsol)) deallocate(jlp_xkfsol)
		allocate(jlp_xkfsol(jlp_mxd+1:jlp_lf0))
		jlp_i0_xkfsol = jlp_mxd+1
		jlp_lf0_xkfsol = jlp_lf0
		do i_= jlp_i0_xkfsol,jlp_lf0_xkfsol	! kannan tehdasmuuttujat
			jlp_xkfsol(i_)%xkf= jlp_x(jlp_lf(i_)+jlp_nrowz)
			jlp_xkfsol(i_)%ixkyk= jlp_ixkf(jlp_lf(i_))
			jlp_xkfsol(i_)%ifact= jlp_ixkffact(jlp_lf(i_))
			jlp_xkfsol(i_)%iunit= jlp_lunit(jlp_lf(i_))
!yksikön tehdassrkeisen ketjutus
			jlp_xkfsol(i_)%next = jlp_mxd
			if(prevcol_(jlp_xkfsol(i_)%iunit)>jlp_mxd) &
				jlp_xkfsol(prevcol_(jlp_xkfsol(i_)%iunit))%next = i_
			prevcol_(jlp_xkfsol(i_)%iunit) = i_
		enddo !do i_= j_i0_xkfsol,j_lf0_xkfsol

		deallocate(prevcol_)
		return
	end subroutine !subroutine defsoluf()

! käyttöpaikkoihin kuljetettavien puutavaralajien (määrien) tulostus ratkaisussa
! Domainien käsittely puuttuu
	subroutine printxkf()
 
		double precision, dimension(:,:),allocatable::sumxkf
		double precision, dimension(:,:),allocatable::sumxkf2
		allocate(sumxkf(1:j_o(jlp_ivxkyk)%i(1),1:j_o(jlp_ivfact)%i(1)))
		sumxkf=0.d0

		jlp_nxvartot=j_o(jlp_ivkeepx)%i(1) ! #vaihtoehdon muuttujat
		iunitrans=0

!Avaintehtaisiin avainvaihtoehdoista vietävät määrät
		do iuni=1,jlp_nunits	! laskentayksiköt
			call j_getobsiv(iuni,jlp_ivmatc,jlp_ivkeepc,jlp_ivunit) !,jlp_ivtransc,j_ivunit) 	! yksikön tiedot v-vektoriin
			if(jlp_ivtrans.gt.0)call dotrans(jlp_ivtrans,1)							 	! muunnokset yksikön tiedoilla
			iobs=jlp_ibaunit(iuni)+jlp_keys(iuni)+jlp_ibaunitbas						! yksikön avainvaihtoehdon indeksi

			call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0)		!printxkf		! avainvaihtoehdon tiedot v-vektoriin ?
			if(jlp_ivsubtrans.gt.0)call dotrans(jlp_ivsubtrans,1)

! Ptl-mjan summa avaintehtaissa
			do ixk_=1,j_o(jlp_ivxkyk)%i(1)	! puutavaralaji-muuttujat
				keyf_ = jlp_keyfact(iuni,ixk_)
				sumxkf(ixk_,keyf_) = sumxkf(ixk_,keyf_) + j_v(j_o(jlp_ivxkyk)%i(ixk_))
			enddo !do ixk_=1,j_o(j_ivxkyk)%i(1)
		enddo !do iuni=1,jlp_nunits

!Korjataan kantavaihtoehdoista avaintehtaisiin vietävillä määrilla
		do i=1,jlp_ld0	!kannan ve-sarakkeet
			wei=jlp_x(jlp_ld(i)+jlp_nrowz)	! vaihtoehdon paino
			iuni=jlp_lunit(jlp_ld(i))		! vaihtoehdon yksikkö
			call j_getobsiv(iuni,jlp_ivmatc,jlp_ivkeepc,jlp_ivunit) !,jlp_ivtransc,j_ivunit)	! yksikön tiedot v-vektoriin
			if(jlp_ivtrans.gt.0)call dotrans(jlp_ivtrans,1)								! muunnokset yksikön tiedoilla

			iobs=jlp_ibaunit(iuni)+jlp_keys(iuni)+jlp_ibaunitbas 						! yksikön avainvaihtoehdon indeksi
			call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0)	!printxkf			!	avainvaihtoehto v-vektoriin ?
			if(jlp_ivsubtrans.gt.0)call dotrans(jlp_ivsubtrans,1)
			if(j_err)return

! Vähennetään painon osuus yksikön avainvaihtoehdosta avaintehtaalle vietävää määrää
			do ixk_=1,j_o(jlp_ivxkyk)%i(1)	! puutavaralaji-muuttujat
				keyf_ = jlp_keyfact(iuni,ixk_)
				sumxkf(ixk_,keyf_) = sumxkf(ixk_,keyf_) - wei*j_v(j_o(jlp_ivxkyk)%i2(ixk_))
			enddo !do ixk_=1,j_o(j_ivxkyk)%i(1)

			iobs=jlp_ibaunit(iuni)+jlp_isch(jlp_ld(i))+jlp_ibaunitbas					! kantavaihtoehdon indeksi
			call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0) !printxkf
			if(jlp_ivsubtrans.gt.0)call dotrans(jlp_ivsubtrans,1)

! Lisätään painon osuus yksikön avainvaihtoehdosta avaintehtaalle vietävää määrää
			do ixk_=1,j_o(jlp_ivxkyk)%i(1)	! puutavaralaji-muuttujat
				keyf_ = jlp_keyfact(iuni,ixk_)
				sumxkf(ixk_,keyf_) = sumxkf(ixk_,keyf_) + wei*j_v(j_o(jlp_ivxkyk)%i2(ixk_))
			enddo !do ixk_=1,j_o(j_ivxkyk)%i(1)
		enddo !do i=1,j_ld0

		do i= jlp_mxd+1,jlp_lf0	! kannan tehdasmuuttujat
			iuni=jlp_lunit(jlp_lf(i))		! tehdasmuuttujan yksikkö
			ixk_ = jlp_ixkf(jlp_lf(i))	! puutavaramuuttujan indeksi xkyky-listalla
			ifact_ = jlp_ixkffact(jlp_lf(i))		! tehtaan indeksi factories-listalla
			keyf_ = jlp_keyfact(iuni,ixk_)	! avaintehdas

			sumxkf(ixk_,keyf_) = sumxkf(ixk_,keyf_) - jlp_x(jlp_lf(i)+jlp_nrowz)
			sumxkf(ixk_,ifact_) = sumxkf(ixk_,ifact_) + jlp_x(jlp_lf(i)+jlp_nrowz)
		enddo !do i= j_mxd+1,j_lf0

		do if_= 1,j_o(jlp_ivfact)%i(1)
			do  ixk_= 1,j_o(jlp_ivxkyk)%i(1)
				do ixkf_=1,jlp_nxkfact(ixk_)
					if (if_ == jlp_xkfact(ixk_,ixkf_)%ifact) then
						write(n16,*) j_vname(j_o(jlp_ivxkyk)%i2(ixk_)),j_vname(j_o(jlp_ivfact)%i2(if_)),sumxkf(ixk_,if_)
					endif !if (if_ == j_xkfact(ixk_,ixkf_)%ifact) then
				enddo !do ixkf_=1,j_nxkfact(ixk_)
			enddo !do  ixk_= 1,j_o(j_ivxkyk)%i(1)
		enddo !do if_= 1,j_o(j_ivfact)%i(1)

! tarkempi tulostus (valitettavasti uudelleen laskien) jos jokin arvo negatiivinen
		if(p.and.any(sumxkf<0.0)) then

			write(n16,*)'Negat. ptl/tehdas-arvot: '
			if(allocated(sumxkf2))deallocate(sumxkf2)
			allocate(sumxkf2(1:j_o(jlp_ivxkyk)%i(1),1:j_o(jlp_ivfact)%i(1)))
			sumxkf2=0.d0

!Avaintehtaisiin avainvaihtoehdoista vietävät määrät
			do iuni=1,jlp_nunits	! laskentayksiköt
				call j_getobsiv(iuni,jlp_ivmatc,jlp_ivkeepc,jlp_ivunit) !,jlp_ivtransc,j_ivunit) ! printxkf yksikön tiedot v-vektoriin
				if(jlp_ivtrans.gt.0)call dotrans(jlp_ivtrans,1)							 	! muunnokset yksikön tiedoilla
				iobs=jlp_ibaunit(iuni)+jlp_keys(iuni)+jlp_ibaunitbas						! yksikön avainvaihtoehdon indeksi

				call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0)				! avainvaihtoehdon tiedot v-vektoriin ?
				if(jlp_ivsubtrans.gt.0)call dotrans(jlp_ivsubtrans,1)

	! Ptl-mjan summa avaintehtaissa
				do ixk_=1,j_o(jlp_ivxkyk)%i(1)	! puutavaralaji-muuttujat
					keyf_ = jlp_keyfact(iuni,ixk_)
					sumxkf2(ixk_,keyf_) = sumxkf2(ixk_,keyf_) + &
					j_v(j_o(jlp_ivxkyk)%i2(ixk_))
					if(sumxkf(ixk_,keyf_)<0.0) then
						write(n16,*)'Avainve avaintehtaaseen: unit,schd, ixk,ifact, +, yht : ',&
							iuni,jlp_keys(iuni), ixk_,keyf_, &
							j_v(j_o(jlp_ivxkyk)%i2(ixk_)), sumxkf2(ixk_,keyf_)
						write(n16,*)'  xk, fact : ', &
						j_vname(j_o(jlp_ivxkyk)%i2(ixk_)), j_vname(j_o(jlp_ivfact)%i2(keyf_))
					endif !if(sumxkf(ixk_,keyf_)<0.0) then
				enddo !do ixk_=1,j_o(j_ivxkyk)%i(1)
			enddo !do iuni=1,jlp_nunits

!Korjataan kantavaihtoehdoista avaintehtaisiin vietävillä määrilla
			do i=1,jlp_ld0	!kannan ve-sarakkeet
				wei=jlp_x(jlp_ld(i)+jlp_nrowz)	! vaihtoehdon paino
				iuni=jlp_lunit(jlp_ld(i))		! vaihtoehdon yksikkö
				call j_getobsiv(iuni,jlp_ivmatc,jlp_ivkeepc,jlp_ivunit) !,jlp_ivtransc,j_ivunit) ! printxkf yksikön tiedot v-vektoriin
				if(jlp_ivtrans.gt.0)call dotrans(jlp_ivtrans,1)								! muunnokset yksikön tiedoilla

				iobs=jlp_ibaunit(iuni)+jlp_isch(jlp_ld(i))+jlp_ibaunitbas					! kantavaihtoehdon indeksi
				call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0) !,jlp_ivtransx,0)
				if(jlp_ivsubtrans.gt.0)call dotrans(jlp_ivsubtrans,1)

	! Lisätään painon osuus yksikön kantavaihtoehdosta avaintehtaalle vietävää määrää
				do ixk_=1,j_o(jlp_ivxkyk)%i(1)	! puutavaralaji-muuttujat
					keyf_ = jlp_keyfact(iuni,ixk_)
					sumxkf2(ixk_,keyf_) = sumxkf2(ixk_,keyf_) +  &
					wei*j_v(j_o(jlp_ivxkyk)%i2(ixk_))
					if(sumxkf(ixk_,keyf_)<0.0) then
					endif !if(sumxkf(ixk_,keyf_)<0.0) then
				enddo !do ixk_=1,j_o(j_ivxkyk)%i(1)

				iobs=jlp_ibaunit(iuni)+jlp_keys(iuni)+jlp_ibaunitbas 						! yksikön avainvaihtoehdon indeksi
				call j_getobsiv(iobs,jlp_ivmatx,jlp_ivkeepx,0)! ,jlp_ivtransx,0)				!	avainvaihtoehto v-vektoriin ?
				if(jlp_ivsubtrans.gt.0)call dotrans(jlp_ivsubtrans,1)
				if(j_err)return

	! Vähennetään painon osuus yksikön avainvaihtoehdosta avaintehtaalle vietävää määrää
				do ixk_=1,j_o(jlp_ivxkyk)%i(1)	! puutavaralaji-muuttujat
					keyf_ = jlp_keyfact(iuni,ixk_)
					sumxkf2(ixk_,keyf_) = sumxkf2(ixk_,keyf_) -  &
					wei*j_v(j_o(jlp_ivxkyk)%i2(ixk_))
					if(sumxkf(ixk_,keyf_)<0.0) then
						write(n16,*)'Avainve -: unit, ve, paino, ixk,ifact, -, yht : ' ,&
							iuni, jlp_keys(iuni),wei, ixk_,keyf_, &
							wei*j_v(j_o(jlp_ivxkyk)%i2(ixk_)),&
							sumxkf2(ixk_,keyf_)
					endif !if(sumxkf(ixk_,keyf_)<0.0) then
				enddo !do ixk_=1,j_o(j_ivxkyk)%i(1)

			enddo !do i=1,j_ld0

			do i= jlp_mxd+1,jlp_lf0	! kannan tehdasmuuttujat
				iuni=jlp_lunit(jlp_lf(i))		! tehdasmuuttujan yksikkö
				if(i==lf01) iuni=iunit
				ixk_ = jlp_ixkf(jlp_lf(i))	! puutavaramuuttujan indeksi xkyky-listalla
				ifact_ = jlp_ixkffact(jlp_lf(i))		! tehtaan indeksi factories-listalla
				keyf_ = jlp_keyfact(iuni,ixk_)	! avaintehdas

				sumxkf2(ixk_,keyf_) = sumxkf2(ixk_,keyf_) - jlp_x(jlp_lf(i)+jlp_nrowz)
				sumxkf2(ixk_,ifact_) = sumxkf2(ixk_,ifact_) + jlp_x(jlp_lf(i)+jlp_nrowz)
				if (sumxkf(ixk_,ifact_)<0.0) then
					write(n16,*)'Kantaxkf + : srk, unit,ixk,ifact, +, yht : ', &
					jlp_lf(i), iuni,ixk_,ifact_, jlp_x(jlp_lf(i)+jlp_nrowz), &
					sumxkf2(ixk_,ifact_)
					write(n16,*)'  xk, fact : ',j_vname(j_o(jlp_ivxkyk)%i2(ixk_)), j_vname(j_o(jlp_ivfact)%i2(ifact_))               
				endif !if (sumxkf(ixk_,ifact_)<0.0) then
				if(sumxkf(ixk_,keyf_)<0.0) then
					write(n16,*)'Avaint. - : srk, unit, ixk,ikeyf, -, yht : ', &
					jlp_lf(i),iuni, ixk_,keyf_, jlp_x(jlp_lf(i)+jlp_nrowz), sumxkf2(ixk_,keyf_)
					write(n16,*)'  xk, fact : ',j_vname(j_o(jlp_ivxkyk)%i(ixk_)), &
					j_vname(j_o(jlp_ivfact)%i2(keyf_))
				endif !if(sumxkf(ixk_,keyf_)<0.0) then
			enddo !do i= j_mxd+1,j_lf0

			deallocate(sumxkf2)
		endif !if(p.and.any(sumxkf<0.0)) then

		deallocate(sumxkf)
		return
	end subroutine !subroutine printxkf()


!Puutavaralaji-muuttujien (määrien) tulostus ratkaisussa
	subroutine printsumxkyk(idom_)
		iunitrans = 0

		call getsolx()
		do ixk_ = 1,j_o(jlp_ivxkyk)%i(1)
			jx=j_inlistobject(j_o(jlp_ivxkyk)%i2(ixk_),jlp_ivkeepx)
			write(n16,*) j_vname(j_o(jlp_ivxkyk)%i2(ixk_)),jlp_sumx((idom_-1)*jlp_nxvartot+jx)
		enddo !do ixk_ = 1,j_o(j_ivxkyk)%i(1)
		return

	end subroutine !subroutine printsumxkyk(idom_)
end subroutine jlp !subroutine jlp(iob,io)
 
subroutine jlps(iob,io)
 
end subroutine !subroutine jlps(iob,io)
 
subroutine initfletsp(nrow,ncol,a,la,lav,ls,lu1,ll1,ifail,nrefac)  !nopre!
	use fletchersparse
	use fletdmod
	implicit REAL*8 (a-h,o-z)
	integer lav(0:*)
	!nm=n+m
	! flletcher m =ncol
	!           n=nrow /  ei nrow+1, Fletcher ei näe objetitriviä ?
	integer ls(1:*)
	real*8 a(la,*)
	common /wsc/kk,ll,kkk,lll,mxws,mxlws  !?? piti poistaaa bqpd:stä
	common /mxm1c/mxm1
	common/bqpdc/irh1,na,na1,nb,nb1,ka1,kb1,kc1,irg1,lu1fl,lv,lv1,ll1fl
	common/densec/ns,ns1,nt,nt1,nu,nu1,mx1,lc,lc1,li,li1
	common/noutc/nout
	common/factorc/m0,m1,mm0,mm,mp,mq
	common/epsc/eps,tol,emin
	common/repc/sgnf,nrep,npiv,nres
	common/refactorc/nup,nfreq
 
	eps=1111.D-19;tol=1.D-13; emin=0.d0   ! corrected 4.5.2015 following Fletchers advice emin= 1.D0
	sgnf=1.D-4;nrep=2;npiv=3;nres=2;nfreq=nrefac;nup=0
 
	kk=0;ll=0
	kmax=0
	!C  n     number of variables  here number of rows
	!C  m     number of general constraints (columns of A)
	!c  k     dimension of the 'reduced space' obtained by eliminating the active
	!c        constraints (only to be set if mode>=2). The number of constraints in
	!c        the active set is n-k
	!C  kmax  maximum value of k (set kmax=0 iff the problem is an LP problem)
	nm=nrow+ncol ;nmi=nm
	call  stmap(nrow,nm,kmax)
	mode=0;ifail=0
	mxm1=min(ncol+1,nrow+ncol);mx1=mxm1
 
	!c  sparse.f requires
	!c     5*n+nprof          locations of real workspace, and
	!c     9*n+m              locations of integer workspace
	!c  where nprof is the space required for storing the row spikes of the L matrix.
	!c  Storage for sparseL.f is situated at the end of the workspace arrays ws
	!c  and lws in bqpd.
	!c  Allow as much space for nprof as you can afford: the routine will report if
	!c  there is not enough. So far 10^6 locations has proved adequate for problems
	!c  of up to 5000 variables.
	mxws=5*nrow+ 4*nrow*nrow  ! (nrow+1)              !5*nrow+4*nrow*nrow
	mxlws=9*nrow+ncol
 
	allocate( ws(1:mxws))
	allocate(lws(1:mxlws),e(1:nm),g(1:nrow),r(1:nm))
	lu1=lu1fl;ll1=ll1fl          !get stmap
	nk=0
 
	call start_up(nrow,nm,nmi,a,lav,nk,e,ls,wslu1,lwsll1,&
		mode,ifail)   !linux
	return
end subroutine !subroutine initfletsp(nrow,ncol,a,la,lav,ls,lu1,ll1,ifail,nrefac)
 
subroutine fbsubsp(nrow,ione,nrow2,a,lavec,iq,rhsw,x,ls,ws,&     !nopre!
		lws,save)
 
	use fletchersparse
	real*8 rhsw(*),x(*),a(*),ws(*)
	integer lws(*),ls(*),lavec(0:*)
	logical save
	call fbsub(nrow,ione,nrow2,a,lavec,iq,rhsw,x,ls,ws,&
		lws,.false.)
 
	return
end subroutine !subroutine fbsubsp(nrow,ione,nrow2,a,lavec,iq,rhsw,x,ls,ws,&
 
subroutine tfbsubsp(nrow,a,lavec,irow,objr,vc,ws,lws,apu,save)  !nopre!
	use fletchersparse
	real*8 a(*),ws(*),apu,objr(*),vc(*)
	integer lws(*),lavec(0:*)
	logical save
 
	call tfbsub(nrow,a,lavec,irow,objr,vc,ws,lws,apu,.false.)
 
	return
end subroutine !subroutine tfbsubsp(nrow,a,lavec,irow,objr,vc,ws,lws,apu,save)
 
subroutine pivotsp(nexnrowz,newc2,nrow, &       !nopre!
		nm,a,lavec,e,ws,lws,ifail,info)
	use fletchersparse
	integer lws(*),lavec(0:*),info(*)
	real*8 a(*),ws(*),apu,e(*)
 
	call pivot(nexnrowz,newc2,nrow, &
		nm,a,lavec,e,ws,lws,ifail,info)
	return
end subroutine !subroutine pivotsp(nexnrowz,newc2,nrow, &
 
