!
! Jlp22 Software
!
! Copyright (C) 2022 Juha Lappi and Natural Resources Institute Finland (Luke)
! Author  Juha Lappi and Reetta Lempinen (in factory optimization)
 
! Juha Lappi has rewritten the most part after his retirement. Only the key steps
! of the simplex algorithm are as previously but the huge subroutine files are
! divided into reasonable subroutines so that the flow of control can be managed. All variables needed for the communication
! between subroutines are put into modules from where te necessary use statements are
! generated with the precompiler jpre. The prefix for the variables needed
! have prefix p_ as compared to prefix j_ in other part of the software.
! The output has been made more reasonable. It is now possible to stop iterations using stop->
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
		use jmod, only: p_mxlws
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
		!		write(6,*)'hep'
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
		!	write(6,*)'heptas',nrow,nm,nmi,nk
		nk=0
		call start_up(nrow,nm,nmi,a,lav,nk,e,ls,wslu1,lwsll1,&   !LWSLL1
			mode,ifail)
		!		write(6,*)'heptasnyy',nrow,nm,nmi,nk
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
!module jlpdmod    !!!!
!contains   !!!!
subroutine pullout(LWSLL1)
	! logical er
	! character*1 ch
	! integer ipivotv
	integer,dimension(*)::LWSLL1
	! !	write(6,*)'inbasis,p_pivot',p_pivot
	! if(p_pivot.eq.ipivotv.and..not.j_err)return
	! if(j_err.and..not.p_ispullout.or.p_ispullout.and.p_pivot.lt.p_pullout1)then
	! write(6,*)'pullout with j_err=.true.'
	! ch='1'
	! else
	! ch=char(49+p_pivot-p_pullout1)
	! endif !if(j_err.and..not.p_ispullout.or.p_ispullout.and.p_pivot.    116
	j_v(p_ivpivots)=p_pivot
	j_v(p_ivobjective)=p_coefmax*p_objf
	j_v(p_ivfeasible)=j_0
	if(p_feasible)j_v(p_ivfeasible)=j_1
	j_v(p_ivobjective0)=p_objfv
	if(p_xpresent)j_v(p_ivunit)=p_unit
	j_v(p_ivtmax)=p_tmax
	j_v(p_ivcolold)=p_icolold
	j_v(p_ivcolnew)=p_icolnew
	j_v(p_ivpivotcase)=p_pivotcase
	j_v(p_ivlr0)=p_lr0
	j_v(p_ivlz0)=p_lz0
	if(p_xpresent)j_v(p_ivlx0)=p_lx0
	if(p_fpresent)j_v(p_ivlf0)=p_lf0
	j_v(p_ivrefac)=p_refac
	j_v(p_ivvaluek)=p_valuek
	j_v(p_ivvalueopt)=p_valueopt
	! !write(6,*)'pullout pivot del ',p_pivot,p_pullout1,p_pivot-p_pullout1+1,ch,' feas ',p_feasible
	! er=j_err
	! j_err=.false.
	! call j_command("po%misctitle"//ch//"='Pivot,objf,objfv,unit,lr0,lz0,ld0,tmax,icolold,icolnew,pivotcase,FEAS,nkeys'")
	! call j_defmatrix(0,'po%misc'//ch,1,13,j_matreg,iv)
	! j_o(iv)%d(1)=p_pivot
	! j_o(iv)%d(2)=p_objf
	! j_o(iv)%d(3)=p_objfv
	! j_o(iv)%d(4)=p_unit
	! j_o(iv)%d(5)=p_lr0
	! j_o(iv)%d(6)=p_lz0
	! j_o(iv)%d(7)=p_lx0
	! j_o(iv)%d(8)=p_tmax
	! j_o(iv)%d(9)=p_icolold
	! j_o(iv)%d(10)=p_icolnew
	! j_o(iv)%d(11)=p_pivotcase
	! ifeas=0
	! if(p_feasible)ifeas=1
	! j_o(iv)%d(12)=ifeas
	! j_o(iv)%d(13)=p_nkeys
	!j_yes=j_err
	j_err=.false.
	if(p_nrow.gt.0)then
		p_ivpomatrix=j_defmatrix(j_ivout,'%basis',p_nrow,p_nrow,j_matreg)
		! call j_defmatrix(0,'po%xps'//ch,1,p_nrow+1,j_matreg,iv)
		! j_o(iv)%d(1:p_nrow+1)=p_xps
		! call j_defmatrix(0,'po%rowx'//ch,1,p_nrow+1,j_matreg,ivrow)
		! j_o(ivrow)%d(1:p_nrow+1)=p_xps
		! call j_defmatrix(0,'po%rhsw'//ch,1,p_nrow,j_matreg,ivrhs)
		p_ivpox=j_defmatrix(j_ivout,'%x',1,p_nrow,j_matreg)
	endif !if(p_nrow.gt.0)    162
	! call j_defmatrix(0,'po%vc'//ch,1,p_nrow,j_matreg,ivc)
	p_ivpoobj=j_defmatrix(j_ivout,'%obj',1,p_mxnm,j_matreg) !ivob)
	if(p_xpresent.and.p_nrow.gt.0)then
		p_ivpounit=j_deflist(j_ivout,'%unit',list0=p_nrow,ilist=.true.)
		!		p_ivpokey=j_deflist(j_ivout,'%key',list0=p_nrow,ilist=.true.)
		p_ivpos=j_deflist(j_ivout,'%s',list0=p_nrow,ilist=.true.)
		!	call j_deflist(j_ivout,'%baunit',p_ivpoibaunit,list0=p_nrow,ilist=.true.)
 
		ivpolist=j_deflist(j_ivout,'%list',list0=5)
		j_o(ivpolist)%i2(1:5)=(/p_ivpomatrix,p_ivpox,p_ivpoobj,p_ivpounit,p_ivpoibaunit/)
	else
		ivpolist=j_deflist(j_ivout,'%list',list0=3)
		j_o(ivpolist)%i2(1:3)=(/p_ivpomatrix,p_ivpox,p_ivpoobj/)
	endif !if(p_xpresent.and.p_nrow.gt.0)    173
	! call j_defmatrix(0,'po%a'//ch,p_nrow,p_mxn+1,j_matreg,iv)
	! ibas=0
	! do i=1,p_nrow
	! j_o(iv)%d(ibas+1:ibas+p_mxn+1)=p_a(i,0:p_mxn)
	! ibas=ibas+p_mxn+1
	! enddo !i=1,p_nrow    165
 
	! ! j_o(iv)%d((i-1)*p_nrow+ls(i))=j_1
	! ! j_o(ivx)%d(i)=p_x(p_ls(i))
	! ! else
	! ! do j=1,p_nrow
	! ! j_o(iv)%d((j-1)*p_nrow+ls(i))=p_a(i,ld(i))
	! ! enddo
	! ! j_o(ivx)%d(i)=p_x(p_ls(i))
	! ! endif
 
	! ! enddo
 
	do i=1,p_nrow
		j_o(p_ivpox)%d(i)=p_x(p_ls(i))
		if(p_ls(i).le.p_nrow)then
			j_o(p_ivpomatrix)%d((p_ls(i)-1)*p_nrow+i)=j_1
 
 
		else
			do j=1,p_nrow
				j_o(p_ivpomatrix)%d((j-1)*p_nrow+i)=p_a(j+p_abas(p_ls(i)-p_nrow))
 
			enddo !j=1,p_nrow    210
			if(p_xpresent)then
				if(p_ls(i).gt.p_nrowz)then
 
					!	j_o(ivob)%d(p_lr0+p_lz0+i)=p_objr(p_nrowz+p_lx(i))
					iunit=p_lunit(p_ls(i)-p_nrowz)
					j_o(p_ivpounit)%i2(i)=iunit
					!j_o(p_ivpokey)%i2(i)=p_keys( iunit ) !p_keys(p_lunit(p_lcur))
					j_o(p_ivpos)%i2(i)=p_isch(p_ls(i)-p_nrowz)
					j_o(p_ivpoibaunit)%i2(i)=p_ibaunit( iunit)
					!		j_o(p_ivpoobj)%d(1)=j_o(p_ivpoobj)%d(1)+p_x(p_ls(i))*p_objr(p_ls(i))
 
					! do j=1,p_nrow
					! j_o(p_ivpoobj)%d(j)=j_o(p_ivpoobj)%d(j)+p_x(p_ls(i))*p_a(j+p_abas(p_ls(i)-p_nrow))
 
					! enddo !j=1,p_nrow    223
 
 
 
					!ivrow
 
				endif !if(p_ls(i).gt.p_nrowz)    215
			endif !if(p_xpresent)    214
		endif !if(p_ls(i).le.p_nrow)    205
 
		!	j_o(p_ivpoobj)%d(i)=p_objr(p_ls(i))
		j_o(p_ivpox)%d(i)=p_x(p_ls(i))
 
	enddo !i=1,p_nrow    203
 
	j_o(p_ivpoobj)%d=p_objr
	!	j_err=j_yes
	! do i=1,p_lr0
	! j_o(p_ivpomatrix)%d((p_lr(i)-1)*p_nrow+i)=j_1
	! j_o(p_ivpox)%d(i)=p_x(p_lr(i))
	! j_o(ivob)%d(i)=p_objr(p_lr(i))
	! enddo !i=1,p_lr0    164
	! do i=1,p_lz0
	! do j=1,p_nrow
	! j_o(p_ivpomatrix)%d((j-1)*p_nrow+i+p_lr0)=p_a(j,p_lz(i))
	! enddo !j=1,p_nrow    170
	! j_o(ivx)%d(i+p_lr0)=p_x(p_nrow+p_lz(i))
	! j_o(ivob)%d(i+p_lr0)=p_objr(p_nrow+p_lz(i))
	! enddo !i=1,p_lz0    169
 
	! do i=1,p_lx0
	! do j=1,p_nrow
	! j_o(p_ivpomatrix)%d((j-1)*p_nrow+p_lr0+p_nz+i)=p_a(j,p_lx(i))
	! enddo !j=1,p_nrow    178
	! j_o(ivx)%d(i+p_lr0+p_lz0)=p_x(p_nrowz+p_lx(i))
	! j_o(ivob)%d(p_lr0+p_lz0+i)=p_objr(p_nrowz+p_lx(i))
	! j_o(ivunit)%d(p_lr0+p_lz0+i)=p_lunit(p_lx(i))
	! j_o(ivkey)%d(p_lr0+p_lz0+i)=p_keys( p_lunit(p_lx(i)))  !p_keys(p_lunit(p_lcur))
	! j_o(ivs)%d(p_lr0+p_lz0+i)=p_isch(p_lx(i))
	! j_o(iviba)%d(p_lr0+p_lz0+i)=p_ibaunit( p_lunit(p_lx(i)))
	! enddo !i=1,p_lx0    177
 
	! j_o(ivrhs)%d(1:p_nrow)=p_rhsw(1:p_nrow)
 
 
	! j_o(ivc)%d(1:p_nrow)=p_vc(1:p_nrow)
 
 
 
	! call j_defmatrix(0,'po%lower'//ch,1,p_nrow,j_matreg,iv)
	! call j_defmatrix(0,'po%lbou'//ch,1,p_nrow,j_matreg,ivb)
	! call j_defmatrix(0,'po%ubou'//ch,1,p_nrow,j_matreg,iv2)
	! do i=1,p_nrow
	! if(p_lower(i))j_o(iv)%d(i)=j_1
	! if(p_lbou(i))j_o(ivb)%d(i)=j_1
	! if(p_ubou(i))j_o(iv2)%d(i)=j_1
	! enddo !i=1,p_nrow    249
 
 
 
 
 
	! ipivotv=p_pivot
 
 
	! if(p_pivot.ge.p_pullout2)then
	! write(6,*)'error return from pullout-> at pivot ',p_pivot
	! write(6,*)'get generated matrices using polis=;list(po%?);'
	! j_err=.true.;return
 
	! endif !if(p_pivot.ge.p_pullout2)    262
	! j_err=er
 
 
end subroutine
 
 
! subroutine jlpgetcol(icol)
! use lastmod !nopre!
! p_acol=p_zero
! iel=0
! do i=p_lavecsp(p_lavecsp(0)+icol),last(icol)
! iel=iel+1
! p_acol(p_lavecsp(i))=p_a(iel,icol)
! end do !i=p_lavecsp(p_lavecsp(0)+icol),last(icol)    278
! return
! end subroutine jlpgetcol!subroutine j_getcol(icol)
 
! subroutine jlpsubcol(ic1,ic2,icr)   !!!!
! use lastmod !nopre!
! logical viel1,viel2
 
! i1=p_lavecsp(p_lavecsp(0)+ic1)
! i2=p_lavecsp(p_lavecsp(0)+ic2)
! if(i1.gt.last(ic1))then
! viel1=.false.
! else !if(i1.gt.last(ic1))then
! viel1=.true.
! end if !if(i1.gt.last(ic1))    291
! if(i2.gt.last(ic2))then
! viel2=.false.
! else !if(i2.gt.last(ic2))then
! viel2=.true.
! end if !if(i2.gt.last(ic2))    296
! ie=0
! ie1=0
! ie2=0
 
! 100 if(viel2.and.(p_lavecsp(i2).lt.p_lavecsp(i1).or..not.viel1))then
! ie=ie+1
! ie2=ie2+1
! p_acolapu(ie)=-p_a(ie2,ic2)
! p_icolapu(ie)=p_lavecsp(i2)
! if(i2.eq.last(ic2))then
! viel2=.false.
! goto 200
! else !if(i2.eq.last(ic2))then
! i2=i2+1
! goto 100
! end if !if(i2.eq.last(ic2))    310
! end if !100 if(viel2.and.(p_lavecsp(i2).lt.p_lavecsp(i1).or..not.v    305
 
! 200 if(viel1.and.(p_lavecsp(i1).lt.p_lavecsp(i2).or..not.viel2))then
! ie=ie+1
! ie1=ie1+1
! p_acolapu(ie)=p_a(ie1,ic1)
! p_icolapu(ie)=p_lavecsp(i1)
! if(i1.eq.last(ic1))then
! viel1=.false.
! else !if(i1.eq.last(ic1))then
! i1=i1+1
! end if !if(i1.eq.last(ic1))    324
! goto 100
! end if !200 if(viel1.and.(p_lavecsp(i1).lt.p_lavecsp(i2).or..not.v    319
 
! 300 if(viel1.and.viel2.and.p_lavecsp(i1).eq.p_lavecsp(i2))then
! ie=ie+1
! ie1=ie1+1
! ie2=ie2+1
! p_acolapu(ie)=p_a(ie1,ic1)-p_a(ie2,ic2)
! p_icolapu(ie)=p_lavecsp(i1)
! if(abs(p_acolapu(ie)).lt.p_tiny78)ie=ie-1
! if(i1.eq.last(ic1))then
! viel1=.false.
! else !if(i1.eq.last(ic1))then
! i1=i1+1
! end if !if(i1.eq.last(ic1))    339
! if(i2.eq.last(ic2))then
! viel2=.false.
! else !if(i2.eq.last(ic2))then
! i2=i2+1
! end if !if(i2.eq.last(ic2))    344
! goto 100
! end if !300 if(viel1.and.viel2.and.p_lavecsp(i1).eq.p_lavecsp(i2))    332
 
! p_a(1:ie,icr)=p_acolapu(1:ie)
! last(icr)=p_lavecsp(p_lavecsp(0)+icr)+ie-1
 
! p_lavecsp(p_lavecsp(p_lavecsp(0)+icr):last(icr))=p_icolapu(1:ie)
 
! end subroutine !subroutine j_supcol(ic1,ic2,icr)
 
!end module !module jlpdmod
 
subroutine problem(iob,io)   !new version old version is problem2  %%jlp  !!!!
 
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
 
 
	! !Exx jlpex1 Example of ordinary Lp-problem
	! !prob1=problem()
	! !2*x1+x2+3*x3-2*x4+10*x5==min
	! ! x1+x3-x4+2*x5=5
	! !x2+2*x3+2*x4+x5=9
	! !x1<7
	! !x2<10
	! !x3<1
	! !x4<5
	! !x5<3
	! ! /
	! !prob1list=;list(prob1%?); !subobject created
	! !@prob1list; !printing the subobjects
	! !jlp1=jlpz(problem->prob1)
	! !enxdex
 
 
	! !Exx jfjfj
	! !xd=data(read->(unit,X),in->)
	! !1,2
	! !1,0
	! !2,5
	! !2,5
	! !2,0
	! !3,0
	! !3,3
	! !3,5
	! !/
 
 
	! !jlp1list=;list(jlp1%?);
	! !@jlp1list;
	! !endex
	! !Ex jlpex2 example of problem() and jlp() in forest data
	! !xd=data(in->'test.xda',read->(d2...d6,i1))
	! ! **see all sub objects created
	! ! xd%?;
 
	! ! Maxlines=4
	! ! ** print all subobjects created up to line 4
	! ! @xd%?;
 
	! ! jlp2list=;list(xd%?);
	! ! @jlp2list;
 
	! ! stat()
	! ! ** variables are:
	! ! ** i1= income in first period
	! ! ** d2= income in second period - income in first period
	! ! ** d3= income in third period - income in first (sic!) period
	! ! ** d4,d5,d6= income in periods 4 5 6 - income in first period
 
	! ! **make indicator variable for a domain
	! ! doma=0
	! ! ct=trans()
	! ! doma=Unit.gt.20
	! ! /
 
	! ! cd=data(in->'test.cda',read->(Ns),maketrans->ct,obs->Unit)
	! ! cd%?;
	! ! stat()
 
	! ! ** link data sets, Ns is the number of schedules in each unit
 
	! ! linkdata(data->cd,subdata->xd,nobsw->Ns)
 
	! ! prob2=problem() ! timber management planning problem
	! ! npv#0==max
	! !;do(i,1,3)
	! !i"i+1"-i"i"=0
	! !;enddo
	! ! /
	! !problist2=;list(prob2%?);
	! !@problist2;
	! !jlp2=jlp(data->cd,problem->prob2)
	! !jlp2list=;list(jlp2%?)
	! !@jlp2list;
 
	! ! prob3=problem() ! timber management planning problem with domains
	! ! npv#0==max
	! !Unit.le.10:doma:
	! !;do(i,1,3)
	! !i"i+1"-i"i">0
	! !;enddo
	! !All:
	! !i4-i1=0
	! ! /
	! !problist3=;list(prob3%?);
	! !@problist3;
	! !jlp3=jlp(data->cd,problem->prob2)
	! !jlp3list=;list(jlp2%?)
	! !@jlp3list;
	! !endex
	! !endsection
 
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
	integer, dimension(:), allocatable:: pvars,termvar,coefvars,isplus
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
	endif !if(ivproblem.eq.j_ivresult)    684
	!if(.not.allocated(isetd))allocate(isetd(1:1000))
	if(.not.allocated(pvars))allocate(pvars(0:1000))  !problem variables
	pvars(0)=0
	allocate(termvar(1:5000),nterminrow(1:500),isplus(1:5000),coefvars(1:5000))
	! write(6,*)'allo termvar'
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
	!	j_linkoption(iob,io,j_mdiag,clear=.true.).ge.0
	if(j_linkoption(iob,io,j_mprint).ge.0.or.j_v(j_ivprintinput).ge.3)inprint=1
 
	!	call j_getoption(iob,io,j_mfactgroup,-1,999,j_ipdata,.true.,nfactgroup,p_factgroup)
	!	if(j_err)return
	!	p_fpresent=nfactgroup.gt.0
	!	write(6,*)'<444fpresent',p_fpresent,nfactgroup
	! if(p_fpresent)then
	! call j_deflist(ivproblem,'%factgroup',ivfactgroup,list0=nfactgroup,list=p_factgroup)
	! nfact=0
	! p_factgroup=>j_o(ivfactgroup)%i2(1:nfactgroup)
	! do i=1,nfactgroup
	! nfact=nfact+j_nobsdata(p_factgroup(i))
	! enddo !i=1,nfactgroup    449
	! write(6,*)'factgroup-> gave ',nfactgroup,' factory groups containing ',nfact ,' factories'
	! write(6,*)' '
	! call j_deflist(ivproblem,'%fact',ivfact,nres=nfact)
	! p_fact=>j_o(ivfact)%i2(1:nfact)
	! nfact=0
	! do i=1,nfactgroup
	! ivcase=j_o(p_factgroup(i))%i(10)
	! if(ivcase.le.0)then
	! call j_printname('**factory group  ',p_factgroup(i),' is not data with case')
	! j_err=.true.
	! endif !if(ivcase.le.0)    459
	! no=j_nobsdata(p_factgroup(i))
	! p_fact(nfact+1:nfact+no)=j_o(ivcase)%i2(1:no)
	! nfact=nfact+no
	! enddo !i=1,nfactgroup    457
	! j_o(ivfact)%i(1)=nfact  !nonstdard way to update list
	! !	write(6,*)'<4455nfact,nfact,p_fact',nfact,nfact,p_fact
 
	! if(j_err)return
 
 
	! call j_getoption(iob,io,j_mutiltrans,1,1,j_iptrans,.true.,narg,j_optarg0)
	! if(j_err)return
	! ivutiltrans=j_optarg0(1)
	! ivutilout=j_o(ivutiltrans)%i2(2)
	! call j_deflist(ivproblem,'%uxkf',ivuxkf,nres=1000)
	! call j_deflist(ivproblem,'%utillist',ivutillist,nres=20)
	! call j_deflist(ivproblem,'%xk',ivxk,nres=40)
	! call j_deflist(ivproblem,'%xkf',ivxkf,nres=1000)
	! write(6,*)'<%uxkf',ivuxkf,'%utillist',ivutillist,'%xk',ivxk
 
 
	! ! call j_deflist(ivproblem,'%logpart',ivlogpart,nres=1000) later
	! ! call j_deflist(ivproblem,'%factpart',ivfactpart,nres=1000)
	! if(allocated(j_itempvector))deallocate(j_itempvector) !logpart
 
	! if(allocated(j_itempvector2))deallocate(j_itempvector2) !factpart
 
	! if(allocated(j_itempvector3))deallocate(j_itempvector3) !upart
 
	! allocate(j_itempvector(1:5000),j_itempvector2(1:5000),j_itempvector3(1:5000))
	! j_itempvector=0
	! j_itempvector2=0
	! j_itempvector3=0
	! endif !if(p_fpresent)    445
 
	!	write(6,*)'<445rhs',p_rhs
	!if(j_err)return
 
	call j_clearoption(iob,io)  ! problem(iob,io)
 
	call j_getname(ivproblem)
	probname=j_oname
	lprobname=j_loname
 
 
	p_ndoms=0  !counting all occurences
 
	if(j_ninc.eq.1)then
		write(6,*)'*problem must be in incl-file'
		j_err=.true.
		return
	endif !if(j_ninc.eq.1)    777
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
	endif !if(ic0.gt.1)    801
	linptot=j_linp
	!		ial=max(linpr-4,1)
	if(j_inp(j_linp-4:j_linp).eq.'==min')then
		iobjtype=-1
	elseif(j_inp(j_linp-4:j_linp).eq.'==max')then
		iobjtype=1
	else
		write(6,*)'*no objective, you can find feasible using artificial objective: anything==max'
		j_err=.true.;return
	endif !if(j_inp(j_linp-4:j_linp).eq.'==min')    814
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
		endif !if(ic0.gt.0)    835
		!write(6,*)'nrow ',nrow
		!write(6,*)'ic0,ic,nrows,lendom ',ic0,ic,nrows,lendom
		call j_getinput('prob> ',single=.true.)
		!write(6,*)'<55>',j_inp(1:j_linp)
		if(j_err)return
		!	write(6,*)'<2>',j_inp(1:j_linp)
	enddo !while(j_inp(1:j_linp).ne.'/')    825
 
 
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
	endif !if(nrow.gt.0)    862
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
		!		ndefdomain=100
 
		p_ivdomain=j_deftext(ivproblem,'%domains',ndefdomain,lendom) !domains of the problem,
		if(.not.j_yes)call j_puttext(p_ivdomain,'All') !cleantext cannot be used
		ivdomaintext=j_deftext(0,'$domaintext',ndefdomain,lendom+10*p_ndoms)
		!write(6,*)'<4747 ivdomaintext,p_ndoms ',ivdomaintext,p_ndoms
		!	call j_putnewcleantext(ivdomain,'All',ilin)
	endif !if(p_isdomain)    879
 
	ipv=0
	ndom=0  !counting separate domains, All not counted?
	npvar=0
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
					iald=ip+1
					do ili=1,nlin
						call j_getinput('prob> ',0,single=.true.)
						!write(6,*)'<388>',j_inp(1:j_linp)
						if(j_err)return
						!write(6,*)'nyttul ',j_inp(1:j_linp),' idom',idom
						call j_puttext(p_ivrow,j_inp(1:j_linp))
						ipe=j_putlistobject(ivrowdomvar,single=idom,append=.true.)
						ipe2=j_putlistobject(ivrowdomnum,single=idomnum,append=.true.)
 
					enddo !ili=1,nlin    958
					!write(6,*)'nterm1,nterm2,ntermd,nterm',nterm1,nterm2,ntermd,nterm
 
					!write(6,*)
 
					nterminrow(p_nrowtot+1:p_nrowtot+nlin)=nterminrow(nrow0+1:nrow0+nlin)
 
					!		write(6,*)'nrow',p_nrow,j_inp(1:j_linp)
					p_nrowtot=p_nrowtot+nlin
					p_nrow=p_nrowtot-1
					!	call j_checkd(coef,nterm2)
					call j_checki(termvar,nterm2)
					call j_checki(coefvars,nterm2)
					call j_checki(isplus,nterm2)
					!	coef(nterm+1:nterm+ntermd)=coef(nterm1:nterm2)
					termvar(nterm+1:nterm+ntermd)=termvar(nterm1:nterm2)
					coefvars(nterm+1:nterm+ntermd)=coefvars(nterm1:nterm2)
					isplus(nterm+1:nterm+ntermd)=isplus(nterm1:nterm2)
 
					!			write(6,*)'ic,nlin,irow0',ic,nlin,irow0,irow0+(ic-1)*nlin+1,irow0+ic*nlin,irow0+1,irow0+nlin
					j_o(p_ivrhsvars)%i2(irow0+(ic-1)*nlin+1:irow0+ic*nlin)=j_o(p_ivrhsvars)%i2(irow0+1:irow0+nlin)
					j_o(p_ivrhs2vars)%i2(irow0+(ic-1)*nlin+1:irow0+ic*nlin)=j_o(p_ivrhs2vars)%i2(irow0+1:irow0+nlin)
				enddo !ic=2,ic0    947
 
 
 
				nterm=nterm+ntermd
				!		j_o(p_ivrhs)%d(irow0+(ic-1)*nlin+1:irow0+ic*nlin)=j_o(p_ivrhs)%d(irow0+1:irow0+nlin)
				!	j_o(p_ivrhs2)%d(irow0+(ic-1)*nlin+1:irow0+ic*nlin)=j_o(p_ivrhs2)%d(irow0+1:irow0+nlin)
 
 
 
				call j_getinput('prob> ',0,single=.true.)
				!	write(6,*)'<399>',j_inp(1:j_linp)
				if(j_err)return
				!write(6,*)'gotagain ',j_inp(1:j_linp)
				!if(j_inp(1:j_linp).eq.'/')exit
				!endif !if(ic.gt.1)    675
 
			endif !if((ic00.gt.0.or.j_inp(1:j_linp).eq.'/').and.ic0.gt.1)    942
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
				endif !if(ic00.gt.1)   1041
				cycle mainloop
 
			endif !if(ic00.eq.0.and.j_yes2)   1008
 
			!write(6,*)'ic00,ic0 tas ',ic00,ic0
			!	call getobjective(iobjtype)
			!if(j_err)return
			! 	call j_puttext(p_ivrow,j_inp(1:j_linp)) ! put row text into the 'rows%'...  object.
			!domainrow can be either end of previous domainsection or start of a new domainsection
			! isdom tlees that domainrow starts new domain
 
 
			!800		iald=1
			!write(6,*)'<800ic0',ic0,ip,j_inp(1:j_linp)
			!	nterm1=nterm
			!icloop:	do ic=1,ic0
			!write(6,*)'<6756 ic,ic0,ip ',ic,ic0,j_inp(1:j_linp)
 
 
			!nterm1=nterm
			!nlin=0
		end if !if(p_isdomain)    937
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
		endif !if(p_isdomain)   1076
 
		!write(6,*)'nterm,nterm1 ,p_nrow',nterm,nterm1,p_nrow,j_inp(1:j_linp)
 
		le=j_nextlim(j_inp,1,j_linp,'=<>')-1
		if(le.ge.j_linp)then
			write(6,*)'*rhs missing in:',j_inp(1:j_linp)
			j_err=.true.;return
		endif !if(le.ge.j_linp)   1084
		if(p_nrow.gt.0)call getrhs(le)
 
 
 
		!	write(6,*)'<344343>'
		call p_getrow(le,nterminrow,nterm,termvar,pvars,coefvars,isplus)
		!	write(6,*)'<34434399>'
		!write(6,*)'<aftergetrow,888,nterm ',nterm,j_inp(1:j_linp),'p_nrowtot ',p_nrowtot
		if(j_err)return
 
 
 
		! if(.not.p_isdomain)cycle mainloop
 
		! nterm2=nterm
 
		! ntermd=nterm2-nterm1+1
		! ipe=j_putlistobject(ivrowdomvar,single=idom,append=.true.)
		! ipe2=j_putlistobject(ivrowdomnum,single=idomnum,append=.true.)
 
 
		! call j_getinput('prob> ',10,single=.true.)
		! write(6,*)'tasa ',j_inp(1:j_linp)
		! ip0=j_nextlim(j_inp,1,j_linp,':/')
		! write(6,*)'here ic0',ic0,ip0,j_inp(1:j_linp)
 
 
 
		! !	if(ip0.le.j_linp)then  ! end section several
 
 
		! !	endif !if(ip0.le.j_linp)    771
 
		! exit
		! !	enddo !while(.true.)    727
		! if(ic0.eq.1)j_yes=.true.  !next line obtained
	enddo  mainloop !nloop:	do while(.true.)    927
	!enddo icloop !oop:	do ic=1,ic0    678
	!write(6,*)'<5555'
 
	!write(6,*)'ivproblem',ivproblem
	ivoout=j_getobject(ivproblem,' ',j_ipproblem)   !miksei ivproblem käy suoraan, kunhan typen määrittelsi?
	!	write(6,*)'ivout,ivproblem',ivout,ivproblem
	ih=  18         !
	allocate(j_o(ivproblem)%i(1:ih))  !
	j_o(ivproblem)%i=0
 
	!	write(6,*)'prob3'
 
	npvar=pvars(0)
 
 
 
	j_o(ivproblem)%i(1)=p_ivrhsvars
	!	j_o(iv)%d(1:nrowtot)=rhs_(1:nrowtot)
	!	deallocate(rhs_)
 
 
 
	j_o(ivproblem)%i(2)=p_ivrhs2vars
 
	!	write(6,*)'p_ivrhsvars,p_ivrhs2vars',p_ivrhsvars,p_ivrhs2vars
	!	j_o(iv)%d(1:nrowtot)=rhs2_(1:nrowtot)
	!	deallocate(rhs2_)
	!write(6,*)'npvar ',npvar
	iv=j_deflist(ivproblem,'%vars',list0=npvar,list=pvars(1:npvar)) !p-variables
	j_o(ivproblem)%i(3)=iv
	deallocate(pvars) !all variables of the problem
 
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
			endif !if(p_isdomain)   1173
			do j=1,nterminrow(iro)
				if(termvar(ibas+j).ne.termvar(ibas1+j))goto 200
				if(coefvars(ibas+j).ne.coefvars(ibas1+j))goto 200
				!	if(coef(ibas+j).ne.coef(ibas1+j))goto 200
			enddo !j=1,nterminrow(iro)   1176
			call j_getline(p_ivrow,iro,j_tempchar,le)
			ir=j_nextlim(j_tempchar,1,le,'=><')
			write(6,*)j_tempchar(1:ir-1)
			write(6,*)'constraint rows ',iro1-1,iro-1,' are equal, put >low <up to same row'
			write(6,*)' '
			j_err=.true.
200			ibas1=ibas1+nterminrow(iro1)
		enddo !iro1=2,iro-1   1171
		ibas=ibas+nterminrow(iro)
	enddo !iro=3,p_nrowtot   1167
	if(j_err)return
	write(6,*)'number of constraints ',p_nrow,' total number of elements ',nterm
	iv=j_deflist(ivproblem,'%rowofterm',list0=nterm,ilist=.true.) !row of each term
	it=0
	do ir=1,p_nrowtot
		do j=1,nterminrow(ir)
			it=it+1
			j_o(iv)%i2(it)=ir-1
 
		enddo !j=1,nterminrow(ir)   1196
	enddo !ir=1,p_nrowtot   1195
	j_o(ivproblem)%i(6)=iv
 
	!write(6,*)'%%%%%%%%%%% ',p_nrowtot,p_nrowtot
	iv=j_deflist(ivproblem,'%nterminrow',list0=p_nrowtot,list=nterminrow(1:p_nrowtot),ilist=.true.)
	j_o(ivproblem)%i(7)=iv  !number of terms for each row
	deallocate(nterminrow)
 
 
	iv=j_deflist(ivproblem,'%termvar',list0=nterm,list=termvar(1:nterm))
	j_o(ivproblem)%i(8)=iv
	deallocate(termvar)
 
 
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
 
	endif !if(p_isdomain)   1224
 
	iv=j_deflist(ivproblem,'%coefvars',list0=nterm,list=coefvars(1:nterm))
	iv2=j_deflist(ivproblem,'%coefplus',list0=nterm,list=isplus(1:nterm))
	j_otype(iv2)=j_ipilist
	j_o(ivproblem)%i(17)=iv
	j_o(ivproblem)%i(18)=iv2
	!	j_o(ivproblem)%i(19)=p_ivrhsvars
	!	j_o(ivproblem)%i(20)=p_ivrhs2vars
 
	if(p_p8)write(6,*)'<33probi',j_o(ivproblem)%i
 
 
 
 
	! if(p_fpresent)then
	! if(p_nrowtot.eq.1)then
	! write(6,*)'**in factory problems there must be constraints (sorry)'
	! write(6,*)'use constraints which require that timber variables transported to factories are at least zero'
	! j_err=.true.
	! goto 900
	! endif !if(p_nrowtot.eq.1)    880
	! j_o(ivproblem)%i(17)=ivfactgroup
	! j_o(ivproblem)%i(18)=ivfact
	! j_o(ivproblem)%i(19)=ivxk
	! j_o(ivproblem)%i(20)=ivutiltrans
 
	! call j_deflist(ivproblem,'%logpart',iv,list0=npvar,list=j_itempvector(1:npvar),ilist=.true.)
	! j_o(ivproblem)%i(21)=iv  !ivlogpart
	! nxkf=count(j_itempvector(1:nterm).ne.0)
	! deallocate(j_itempvector)
 
	! call j_deflist(ivproblem,'%factpart',iv,list0=npvar,list=j_itempvector2(1:npvar),ilist=.true.)
	! j_o(ivproblem)%i(22)=iv
	! deallocate(j_itempvector2)
 
	! call j_deflist(ivproblem,'%upart',iv,list0=npvar,list=j_itempvector3(1:npvar),ilist=.true.)
	! j_o(ivproblem)%i(23)=iv
	! deallocate(j_itempvector3)
 
 
 
	! !	deallocate(j_itempvector,j_itempvector2)
	! j_o(ivproblem)%i(22)=iv
	! j_o(ivproblem)%i(23)=ivuxkf
	! j_o(ivproblem)%i(24)=ivutillist
 
 
	! !		j_o(ivproblem)%i2(p_nrowtot+nterm+ndom+1:p_nrowtot+nterm+ndom+npvar)=j_itempvector(ibasy+1:ibasy+npvar)
	! !		j_o(ivproblem)%i2(p_nrowtot+nterm+ndom+npvar+1:p_nrowtot+nterm+ndom+2*npvar)=j_itempvector(ibasx+1:ibasx+npvar)
	! !	deallocate(j_itempvector,j_itempvector2)
	! endif !if(p_fpresent)    879
 
 
	!	write(6,*)'proble,ret*******************'
 
	900	j_v(j_ivprintinput)=printold
	return
end subroutine problem !subroutine problem(iob,io)
 
subroutine getdomain(domdef,ivdomaintext,ndom,idom,idomnum)
	character*(*)::domdef
	character*4 domnum
	p_ndoms=p_ndoms+1
	idom=j_object(domdef)
 
	call j_putnewcleantext(p_ivdomain,domdef,iline)
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
		endif !if(idom.le.0)   1346
	endif !if(iline.gt.ndom)   1337
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
 
 
 
subroutine p_getrow(le,nterminrow,nterm,termvar,pvars,coefvars,isplus)
	integer::le !length os the left side
	integer, dimension(:), allocatable :: nterminrow,termvar,pvars,coefvars,isplus
 
	!	double precision,allocatable, dimension(:)::coef
 
	!	double precision :: coe
	logical isminus,iscoef
	integer isplus0
	!	logical :: coevec = .false.
	!	logical :: listvar = .false.
	!	p_nrowtot=p_nrowtot+1
	!write(6,*)'<477474,iip',iip,j_inp(1:j_linp),' p_ivrow',p_ivrow, 'interpret coef','idomain ',p_isdomain
 
	!	write(6,*)'getrow',le,nterm,allocated(nterminrow),allocated(termvar),allocated(pvars),allocated(coefvars),allocated(isplus)
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
	endif !if(isminus)   1425
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
				endif !if(irp.gt.le)   1449
				if(nex.ne.icoef1)then
					write(6,*)'cannot understand what is ',j_inp(icoef1:nex)
					j_err=.true.;return
				endif !if(nex.ne.icoef1)   1453
				if(j_inp(irp+1:irp+1).ne.'*')then
					write(6,*)j_inp(nex:irp),' must be followed with *'
					j_err=.true.;return
				endif !if(j_inp(irp+1:irp+1).ne.'*')   1457
				icoef1=nex  !+1
				icoef2=irp  !-1
				ivar1=irp+2
			else  !'*
				icoef2=nex-1
				ivar1=nex+1
				!write(6,*)'ivar1 ',j_inp(1:ivar1)
			endif !if(j_inp(nex:nex).eq.'(')   1447
 
 
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
		endif !if(j_inp(nex:nex).eq.'*'.or.j_inp(nex:nex).eq.'(')   1446
		if(isminus)then
			!		coe=-coe
			isplus0=0
		endif !if(isminus)   1510
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
		call j_puti(termvar,nterm,ivvar)  !termvar variables of terms
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
		! call j_puti(termvar,nterm,j_o(ivvar)%i2(j))  !termvar variables of terms
		! ipo= j_putlist(j_o(ivvar)%i2(j),pvars)
		! call j_puti(nterminrow,p_nrowtot,nterminrow(p_nrowtot)+1)  !increase number of variables
		! enddo !j=1,j_o(ivvar)%i(1)   1390
		! else
		! do j=1,j_o(ivvar)%i(1)
		! nterm=nterm+1
		! call j_putd(coef,nterm,coe)  !coeffcients of terms
		! call j_puti(termvar,nterm,j_o(ivvar)%i2(j))  !termvar variables of terms
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
		! call j_puti(termvar,nterm,ivvar)  !termvar variables of terms
		! call j_puti(nterminrow,p_nrowtot,nterminrow(p_nrowtot)+1)  !increase number of variables
		! ipo= j_putlist(ivvar,pvars)
		! endif !if(j_otype(ivvar).eq.j_ipreal)   1370
 
		if(ivar2.eq.le)exit
		isminus=j_inp(ivar2+1:ivar2+1).eq.'-'
 
		icoef1=ivar2+2
		!write(6,*)'ivar2,le ',ivar2,le,j_inp(1:ivar2)
 
	enddo termloop !mloop: do while(icoef1.le.le)   1438
	return
	!	end if !if(j_inp(1:j_linp).ne.'/')    532
end subroutine
 
subroutine getrhs(le)
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
		endif !if(j_err)   1607
		!	j_o(p_ivrhs)%d(p_nrow)=j_val(j_inp(ip+1:j_linp))
 
 
		! j_o(p_ivrhs2)%d(p_nrow)=j_o(p_ivrhs)%d(p_nrow)
 
		! j_o(p_ivrhsvars)%i2(p_nrow)=j_tex2iv(j_inp(ip+1:j_linp),isplus0)
		! j_o(p_ivrhs2vars)%i2(p_nrow)=j_o(p_ivrhsvars)%i2(p_nrow)
 
 
	else !if(ipe.gt.0)then
		ipg=j_nextlim0(j_inp,1,j_linp,'>')
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
				endif !if(j_err)   1637
				!		j_o(p_ivrhsvars)%i2(p_nrow)=j_tex2iv(j_inp(ipg+1:lu),isplus0)
 
			else !if(ipg.gt.0)then
				iv=j_ivninf
				! j_o(p_ivrhs)%d(p_nrow)=j_ninf
				! j_o(p_ivrhsvars)%i2(p_nrow)=j_ivtolast
				!	call j_putd(rhs_,p_nrowtot,j_ninf )
			end if !if(ipg.gt.0)   1627
			if(ipl.gt.0)then
				lu=j_linp;if(ipg.gt.ipl)lu=ipg-1
				iv2=j_tex2iv(j_inp(ipl+1:lu),isplus2)
 
				!j_o(p_ivrhs2)%d(p_nrow)=j_v(iv)  !j_val(j_inp(ipl+1:lu))
				!	call j_putd(rhs2_,p_nrowtot,j_val(j_inp(ipl+1:lu)) )
				!		write(6,*)'<rhs2',ipl,ipg,j_inp(ipl+1:lu),j_val(j_inp(ipl+1:lu))
				if(j_err)then  ! j_err coming from val
					write(6,*)'*problem: error in interpreting rhs2 ',j_inp(ipl+1:lu)
					return
				endif !if(j_err)   1656
				!	j_o(p_ivrhs2vars)%i2(p_nrow)=iv !j_tex2iv(j_inp(ipl+1:lu),isplus0)
 
			else !if(ipl.gt.0)then
				iv2=j_ivinf
				! j_o(p_ivrhs2)%d(p_nrow)=j_inf
				! j_o(p_ivrhs2vars)%i2(p_nrow)=j_ivinf
				!		call j_putd(rhs2_,p_nrowtot,j_inf)
			end if !if(ipl.gt.0)   1649
			if(ipg.gt.0.and.ipl.gt.0)then
				ip=min(ipg,ipl)
			else !if(ipg.gt.0.and.ipl.gt.0)then
				ip=ipg+ipl
			end if !if(ipg.gt.0.and.ipl.gt.0)   1668
 
		end if !if(ipg+ipl.gt.0)   1624
	end if !if(ip.gt.0)   1603
	!	j_o(p_ivrhs)%d(p_nrow)=j_v(iv)
	!	j_o(p_ivrhs2)%d(p_nrow)=j_v(iv2)
	!write(6,*)'rhs ',p_ivrhsvars,p_ivrhs2vars
	j_o(p_ivrhsvars)%i2(p_nrow)=iv
	j_o(p_ivrhs2vars)%i2(p_nrow)=iv2
	j_o(p_ivrhsplus)%i2(p_nrow)=isplus
	j_o(p_ivrhs2plus)%i2(p_nrow)=isplus2
end subroutine getrhs
 
 
!	contains    !!!!
subroutine jlpcurix()   !!!! p_idomba tells unit
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units are accessed one by one
	! p_nrowcurx
	! p_rowcurx
	! p_idomba
 
 
	! endif !if(p_ndom.le.0)    911
 
	!write(6,*)'<45 ',iunit,idomba
 
 
 
	p_nrowcurx=p_nrowx0
	!p_ixcur=p_ixcur0
	!	p_ixcur=.false.
	write(6,*)'p_unit,p_domba,p_ndomvars,p_ido1',p_unit,p_idomba,p_ndomvars,p_ido1
	!	write(6,*)'p_ido1,p_domvars',p_ido1,p_ndomvars,p_nrowx0,'*',p_rowcurx
	do ido=p_ido1,p_ndomvars
		!icurint=(id-1)/32+1
		!	icurbit=p_id-(icurint-1)*32-1
		!	write(6,*)'ido',p_icurint(ido),p_icurbit(ido)
		!		if(p_p8)write(6,*)'curix,iuni,p_idomba,p_icurint(ido)',iuni,ido,p_idomba,p_icurint(ido)
		if(btest(p_domainbits(p_idomba+p_icurint(ido)),p_icurbit(ido)))then
			!	write(6,*)'idook ',ido,'nixcu',p_nixcu(ido),'bas',p_ixcubas(ido)
			do j=1,p_nixcu(ido)
				p_nrowcurx=p_nrowcurx+1
				p_rowcurx(p_nrowcurx)=p_ixcurow(p_ixcubas(ido)+j)
				! p_ixcur(p_rowcurx(p_nrowcurx))=.true.
				!		p_ixcur(p_ixcurow(p_ixcubas(ido)+j))=.true.
 
			enddo !j=1,p_nixcu(ido)   1713
		endif !if(btest(p_domainbits(p_idomba+p_icurint(ido)),p_icurbit(i   1711
	enddo !ido=p_ido1,p_ndomvars   1706
	!	p_nrowcurx=p_nrowx0
	! p_nrowcurx=0
	! do j=0,p_nrow
	! if(p_ixcur(j))then
	! p_nrowcurx=p_nrowcurx+1
	! p_rowcurx(p_nrowcurx)=j
	! endif !if(p_ixcur(j))   1257
	! enddo !j=0,p_nrow   1256
	! !if(p_p)write(p_n16,*)'p_ixcur0',p_ixcur0,p_idomba
	!if(p_p)write(p_n16,*)'p_ixcur',p_ixcur
	!	p_row0=1
	!	if(p_rowcurx(1).eq.0)p_row0=2
	p_idomba=p_idomba+p_ndomv
	!write(6,*)'p_nrowcurx ',p_nrowcurx
end subroutine !subroutine j_curix(iuni)
 
subroutine jlpcurix2(iuni)   !!!!
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units can be in random order
	! p_nrowcurx
	! p_rowcurx
 
	!determines for each row if the unit iunit belonggs to the domain of the row
 
	idomba=(iuni-1)*p_ndomv
	p_nrowcurx=p_nrowx0
	!p_ixcur=p_ixcur0
	!	p_ixcur=.false.
	!	write(6,*)'p_unitcurix3,p_domba',iuni,idomba
	!	write(6,*)'p_ido1,p_domvars',p_ido1,p_ndomvars,p_nrowx0,'*',p_rowcurx
	do ido=p_ido1,p_ndomvars
		!icurint=(id-1)/32+1
		!	icurbit=p_id-(icurint-1)*32-1
		!		if(p_p8)write(6,*)'curix,iuni,p_idomba,p_icurint(ido)',iuni,ido,p_idomba,p_icurint(ido)
		if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido)))then
			!	write(6,*)'idook ',ido,'nixcu',p_nixcu(ido),'bas',p_ixcubas(ido)
			do j=1,p_nixcu(ido)
				p_nrowcurx=p_nrowcurx+1
				p_rowcurx(p_nrowcurx)=p_ixcurow(p_ixcubas(ido)+j)
				! p_ixcur(p_rowcurx(p_nrowcurx))=.true.
				!		p_ixcur(p_ixcurow(p_ixcubas(ido)+j))=.true.
 
			enddo !j=1,p_nixcu(ido)   1758
		endif !if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido   1756
	enddo !ido=p_ido1,p_ndomvars   1752
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
	!character*5 chi5 comes now from getmod
	if(p_isdomain)then
 
p_buf='DOMAIN:'
		idom=p_rowdomnum(ir)
		!	call j_getline(p_ivdomain,idom,p_buf,le)
 
		call j_getline(p_ivdomain,idom,p_buf(8:),le)
		p_buf(74:78)='units'
		p_buf(68:72)=j_chi5(p_domainunits(idom),0)
		write(6,'(a)')p_buf(1:72)
	else
		!for each expanded problem
		! irowrow tells the the initial row in the nonexpanded problem
		call j_getline(p_ivrow,ir+1,p_buf,le)
		write(6,'(a)')p_buf(1:le)
	endif !if(p_isdomain)   1922
	return
end subroutine!subroutine printrowinfo(ir)
 
 
 
subroutine domain(iob,io)   !!!!
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	j_v(iout)= j_line2(p_ivdomain,j_o(iob)%i(io+2))
	!	io=io+narg+3
	return
end subroutine!subroutine domain(iob,io)
 
subroutine priceunit(iob,io)  !!!!
	!xmatiba(iobs)=(iobs-1)*j_ntemp0
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**priceunit: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   1954
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iunit=j_v( j_o(iob)%i(io+2) )
	if(iunit.gt.p_nunits.or.iunit.le.0)then
		write(6,*)'**illegal unit in price%unit ',iunit,' max is',p_nunits
		j_err=.true. ;return
	endif !if(iunit.gt.p_nunits.or.iunit.le.0)   1964
	if(p_isdomain)call jlpcurix2(iunit) !
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units can be in random order
	! p_nrowcurx
	! p_rowcurx
 
	p_value=j_0
	iobs=p_ibaunit(iunit)+p_keys(iunit)
	ibxmatx=(iobs-1)*p_ntemp0  !xmatiba(iobs) !,1)
	do jj=1,p_nrowcurx
		j=p_rowcurx(jj)
		!	j_value=j_value+j_vx(j)*j_xmat(j_ix(j),iobs) !v(ix(j))
		p_value=p_value+p_vx(j)*p_xmat(ibxmatx+p_ix(j)) !v(ix(j))
	enddo !jj=1,p_nrowcurx   1977
	j_v(iout)=p_value
 
	!	io=io+narg+3
	return
end subroutine !subroutine priceunit(iob,io)
 
subroutine priceschedw(iob,io)   !!!!
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**priceschedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   1990
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iunit=j_v( j_o(iob)%i(io+2) )
	is=j_v( j_o(iob)%i(io+3) )
 
	if(iunit.gt.p_nunits.or.iunit.le.0)then
		write(6,*)'**illegal unit in price%schedw ',iunit,' max is',p_nunits
		j_err=.true. ;return
	endif !if(iunit.gt.p_nunits.or.iunit.le.0)   2001
	if(is.gt.p_ns(iunit).or.is.le.0)then
		write(6,*)'**illegal sched ',is,'  in price%schedw for unit ',iunit, 'max=',p_ns(iunit)
		j_err=.true. ;return
	endif !if(is.gt.p_ns(iunit).or.is.le.0)   2005
	if(p_isdomain)call jlpcurix2(iunit)
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units can be in random order
	! p_nrowcurx
	! p_rowcurx
 
	p_value=j_0
	iobs=p_ibaunit(iunit)+is
	ibxmatx=(iobs-1)*p_ntemp0  !xmatiba(iobs) ! ,1)
	do jj=1,p_nrowcurx
		j=p_rowcurx(jj)
		p_value=p_value+p_vx(j)*p_xmat(ibxmatx+p_ix(j)) ! ,iobs) !v(ix(j))
	enddo !jj=1,p_nrowcurx   2018
	j_v(iout)=p_value
 
	!	io=io+narg+3
	return
end subroutine!subroutine priceschedw(iob,io)
 
subroutine priceschedcum(iob,io)   !!!!
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**priceschedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2030
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	is=j_v( j_o(iob)%i(io+2) )
	if(is.gt.p_lopp.or.is.le.0)then
		write(6,*)'**illegal sched in price%schedcum: ',is,' max is',p_lopp
		j_err=.true. ;return
	endif !if(is.gt.p_lopp.or.is.le.0)   2039
	ibxmatx=(is-1)*p_ntemp0  !xmatiba(is) !,1)
	if(p_isdomain)call jlpcurix2(iunit)
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units can be in random order
	! p_nrowcurx
	! p_rowcurx
	p_value=j_0
	do jj=1,p_nrowcurx
		j=p_rowcurx(jj)
		!j_value=j_value+j_vx(j)*j_xmat(j_ix(j),is) !,is) !v(ix(j))
		p_value=p_value+p_vx(j)*p_xmat(ibxmatx+p_ix(j)) !,is) !v(ix(j))
	enddo !jj=1,p_nrowcurx   2050
	j_v(iout)=p_value
 
	!	io=io+narg+3
	return
end subroutine !subroutine priceschedcum(iob,io)
 
 
subroutine weights(iob,io)   !!!!
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
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**nweights: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2098
 
	nweights=p_nunits+p_lx0
 
	return
end function!function nweights()
 
 
subroutine partweights(iob,io)   !!!!
	! Section partweights partweight() weights of split schedules
	! TO BE RAPORTED LATER
	! endsection
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**partweights: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2115
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
			endif !if(p_unitdiv(i).eq.iunit)   2131
		enddo !i=1,p_ndiv   2130
		77  j_v(iout)=ns
	endif !if(narg.eq.0)   2125
	!	io=io+narg+3
	return
end subroutine!subroutine partweights(iob,io)
 
 
!unit
subroutine jlpunit(iob,io)   !!!!
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**unit: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2144
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
			endif !if(ib.ge.ibas)   2165
			iunitv=iunit
		enddo !j=1,p_ndiv   2159
		! all
		iunit=ibas-niv
		77  j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=iunit

	endif !if(ibas.le.0.or.ibas.gt.p_nunits+p_ndiv)   2150
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine jlpunit(iob,io)
 
function lunit_(ibas)    !!!!
 
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
		endif !if(ib.ge.ibas)   2195
		iunitv=iunit
	enddo !j=1,p_ndiv   2189
	! all
	iunit=ibas-niv
	77 lunit_=iunit

	return
end function!function lunit_(ibas)
 
subroutine partunit(iob,io)   !!!!
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**partunit: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2210
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
	endif !if(idiv.le.0.or.idiv.gt.p_ndiv)   2219
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine !subroutine partunit(iob,io)
 
subroutine weight(iob,io)   !!!!
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**weight: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2231
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
			endif !if(ib.eq.ibas)   2248
			iunitv=iunit
		enddo !j=1,p_ndiv   2244
		! all
		wei=1.
		77  j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei

	endif !if(ibas.le.0.or.ibas.gt.p_nunits+p_ndiv)   2237
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine weight(iob,io)
 
! weight -funktion melaoptj versio
function we_(ibas)    !!!!
 
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
		endif !if(ib.eq.ibas)   2277
		iunitv=iunit
	enddo !j=1,p_ndiv   2272
	! all
	wei=1.
	77  we_=wei
	return
end function!function we_(ibas)
 
subroutine schedweight(ibas,is,wei) !returns for ibas'th schedule twith nonzero weight the   !!!!
	! schedule number is (cumulative) and the weight w
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**schedweight: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2295
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
			end if !if(ib.eq.ibas)   2313
			iunitv=iunit
		end do !j=1,p_ndiv   2308
		! all
		wei=1.
		iunit=ibas-niv;is=p_ibaunit(iunit)+p_keys(iunit)
		77  continue !  v(o(iob)%i(io+o(iob)%i(io+1)+2))=wei

	end if !if(ibas.le.0.or.ibas.gt.p_nunits+p_ndiv)   2301
 
	return
end subroutine!subroutine schedweight(ibas,is,wei)
 
 
subroutine weightschedcum(iob,io)   !!!!
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**weightschedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2337
	isc=j_v(j_o(iob)%i(io+2))  ! for which the weight is wanted
	if(isc.le.0.or.isc.gt.p_lopp)then
		j_err=.true.
		write(6,*)'**weight%schedcum: trying to access weight',isc, 'maximum is:',p_lopp
		return
	endif !if(isc.le.0.or.isc.gt.p_lopp)   2343
 
	iunit0=1+p_lopp/p_nunits;iunit0=min(p_nunits,iunit0)
	if(isc.le.p_ibaunit(iunit0))then
		do iunit=iunit0-1,1,-1
			if(isc.gt.p_ibaunit(iunit))exit
		enddo !iunit=iunit0-1,1,-1   2351
	else !if(isc.le.j_ibaunit(iunit0))then
		do iunit=iunit0,p_nunits
			if(isc.le.p_ibaunit(iunit+1))exit
		enddo !iunit=iunit0,p_nunits   2355
	endif !if(isc.le.p_ibaunit(iunit0))   2350
	is=isc-p_ibaunit(iunit)
 
	if(j_linkoption(iob,io,j_minteger).ge.0)then
		call j_clearoption(iob,io)  ! weightschedcum(iob,io)
		do j=1,p_ndiv
			! ib=p_nunits before
			if(p_unitdiv(j).eq.iunit)then
				jmax=j
				do j2=j+1,p_ndiv
					if(p_unitdiv(j2).ne.iunit)exit
					if(p_wdiv(j2).gt.p_wdiv(jmax))jmax=j2
				enddo !j2=j+1,p_ndiv   2367
				if(is.eq.p_isdiv(jmax))then
					wei=1.
				else !if(is.eq.j_isdiv(jmax))then
					wei=0.
					goto 90
				endif !if(is.eq.p_isdiv(jmax))   2371
			endif !if(p_unitdiv(j).eq.iunit)   2365
		enddo !j=1,p_ndiv   2363
 
	else !if(j_linkoption(iob,io,j_minteger).gt.0)then
 
		do j=1,p_ndiv
			! ib=p_nunits before
			if(p_unitdiv(j).eq.iunit)then
				do j2=j,p_ndiv
					if(p_unitdiv(j2).ne.iunit)exit
					if(is.eq.p_isdiv(j2))then
						wei=p_wdiv(j2)
						goto 90
					endif !if(is.eq.p_isdiv(j2))   2387
				enddo !j2=j,p_ndiv   2385
				wei=0.
				goto 90
			endif !if(p_unitdiv(j).eq.iunit)   2384
		enddo !j=1,p_ndiv   2382
	endif !if(j_linkoption(iob,io,j_minteger).ge.0)   2361
 
	if(is.eq.p_keys(iunit))then
		wei=1.
	else !if(is.eq.j_keys(iunit))then
		wei=0.
	endif !if(is.eq.p_keys(iunit))   2398
	90 continue

	j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei
 
	!	io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine weightschedcum(iob,io)
 
 
subroutine integersched(iob,io)  !both schedw andf schedcum   !!!! EI TOIMI
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**integersched: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2413
	iunit=j_v(j_o(iob)%i(io+2))
	if(iunit.le.0.or.iunit.gt.p_nunits)then
		j_err=.true.
		write(6,*)'**weight%schedw: trying to access unit',iunit, 'maximum is:',p_nunits
		return
	endif !if(iunit.le.0.or.iunit.gt.p_nunits)   2419
 
	do j=1,p_ndiv
		! ib=p_nunits before
		if(p_unitdiv(j).eq.iunit)then
			jmax=j
			do j2=j+1,p_ndiv
				if(p_unitdiv(j2).ne.iunit)exit
				if(p_wdiv(j2).gt.p_wdiv(jmax))jmax=j2
			enddo !j2=j+1,p_ndiv   2429
			wei=p_isdiv(jmax)
			goto 90
		endif !if(p_unitdiv(j).eq.iunit)   2427
	enddo !j=1,p_ndiv   2425
 
	wei=p_keys(iunit)
 
	90 continue
	if(j_o(iob)%i(io).eq.jifintegerschedcum)wei=wei+p_ibaunit(iunit) ! ?j3
 
	j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei
 
	!	io=io+j_o(iob)%i(io+1)+3
 
	return
end subroutine !subroutine integersched(iob,io)
 
 
subroutine weightschedw(iob,io)  !!!!
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**weightschedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2453
 
	iunit=j_v(j_o(iob)%i(io+2))
	if(iunit.le.0.or.iunit.gt.p_nunits)then
		j_err=.true.
		write(6,*)'**weight%schedw: trying to access unit',iunit, 'maximum is:',p_nunits
		return
	endif !if(iunit.le.0.or.iunit.gt.p_nunits)   2460
	is=j_v(j_o(iob)%i(io+3))  ! for which the weight is wanted
	if(is.le.0.or.is.gt.p_ns(iunit))then
		j_err=.true.
		write(6,*)'**weight%schedw: trying to access sched',is, 'maximum is:',p_ns(iunit)
		return
	endif !if(is.le.0.or.is.gt.p_ns(iunit))   2466
 
	if(j_linkoption(iob,io,j_minteger).ge.0)then
		call j_clearoption(iob,io)  ! ubroutine weightschedw(iob,io)
		do j=1,p_ndiv
			! ib=p_nunits before
			if(p_unitdiv(j).eq.iunit)then
				jmax=j
				do j2=j+1,p_ndiv
					if(p_unitdiv(j2).ne.iunit)exit
					if(p_wdiv(j2).gt.p_wdiv(jmax))jmax=j2
				enddo !j2=j+1,p_ndiv   2478
				if(is.eq.p_isdiv(jmax))then
					wei=1.
				else !if(is.eq.j_isdiv(jmax))then
					wei=0.
					goto 90
				endif !if(is.eq.p_isdiv(jmax))   2482
			endif !if(p_unitdiv(j).eq.iunit)   2476
		enddo !j=1,p_ndiv   2474
 
	else !if(j_linkoption(iob,io,j_minteger).gt.0)then
 
		do j=1,p_ndiv
			! ib=p_nunits before
			if(p_unitdiv(j).eq.iunit)then
				do j2=j,p_ndiv
					if(p_unitdiv(j2).ne.iunit)exit
					if(is.eq.p_isdiv(j2))then
						wei=p_wdiv(j2)
						goto 90
					endif !if(is.eq.p_isdiv(j2))   2498
				enddo !j2=j,p_ndiv   2496
				wei=0.
				goto 90
			endif !if(p_unitdiv(j).eq.iunit)   2495
		enddo !j=1,p_ndiv   2493
 
	endif !if(j_linkoption(iob,io,j_minteger).ge.0)   2472
 
	if(is.eq.p_keys(iunit))then
		wei=1.
	else !if(is.eq.j_keys(iunit))then
		wei=0.
	endif !if(is.eq.p_keys(iunit))   2510
	90 continue

	j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=wei
 
	!	io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine weightschedw(iob,io)
 
 
subroutine partweight(iob,io)  !!!!
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**partweight: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2526
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
	endif !if(idiv.le.0.or.idiv.gt.p_ndiv)   2535
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine !subroutine partweight(iob,io)
 
subroutine schedw(iob,io)  !!!!
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**schedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2547
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
			end if !if(ib.eq.ibas)   2565
			iunitv=iunit
		end do !j=1,p_ndiv   2559
		! all
		iunit=ibas-niv;is=p_keys(iunit) ! o(isol)%i(iunit)
		77 j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=is

	end if !if(ibas.le.0.or.ibas.gt.p_nunits+p_lx0)   2553
 
	!	90 io=io+j_o(iob)%i(io+1)+3
	return
end subroutine!subroutine schedw(iob,io)
 
! schewd melaoptj versio
function ls_(ibas)   !!!!
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
		end if !if(ib.eq.ibas)   2599
		iunitv=iunit
	end do !j=1,p_ndiv   2593
	! all
	iunit=ibas-niv;is=p_keys(iunit) ! o(isol)%i(iunit)
	77 ls_=is

	return
end function!function ls_(ibas)
 
subroutine schedcum(iob,io)  !!!!
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**schedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2619
 
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
			end if !if(ib.eq.ibas)   2637
			iunitv=iunit
		end do !j=1,p_ndiv   2632
		! all
		iunit=ibas-niv;is=p_ibaunit(iunit)+p_keys(iunit) !o(isol)%i(iunit)+ o(isol)%i2(4+iunit)
 
		77  j_v(j_o(iob)%i(io+j_o(iob)%i(io+1)+2))=is

	end if !if(ibas.le.0.or.ibas.gt.p_nunits+p_lx0)   2626
 
	90 return
!	io=io+j_o(iob)%i(io+1)+3
!	return
end subroutine!subroutine schedcum(iob,io)
 
subroutine partschedw(iob,io)   !!!!
 
	if(j_v(p_ivstartedjlp).eq.0)then
		write(6,*)'**partschedw: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2662
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
 
	endif !if(idiv.le.0.or.idiv.gt.p_ndiv)   2673
	return
	!io=io+j_o(iob)%i(io+1)+3
	!	return
end subroutine!subroutine partschedw(iob,io)
 
subroutine partschedcum(iob,io)   !!!!
	if(j_v(p_ivstartedjlp).eq.0)then
 
		write(6,*)'**partschedcum: Started_jlp=0'
		j_err=.true.
		return
	endif !if(j_v(p_ivstartedjlp).eq.0)   2688
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
 
	endif !if(idiv.le.0.or.idiv.gt.p_ndiv)   2702
	return
	!	90 io=io+narg+3
	!	return
end subroutine!subroutine partschedcum(iob,io)
 
 
 
 
integer function integerschedw(iunit)  ! %%integer solution   !!!!
	integer,intent(in) ::iunit
	!could be used in subroutine integersched(iob,io)
	integer wei
	if(iunit.le.0.or.iunit.gt.p_nunits)then
		j_err=.true.
		write(6,*)'*j* integersched trying to access unit',iunit, 'maximum is:',p_nunits
		return
	endif !if(iunit.le.0.or.iunit.gt.p_nunits)   2723
 
	do j=1,p_ndiv
		! ib=p_nunits before
		if(p_unitdiv(j).eq.iunit)then
			jmax=j
			do j2=j+1,p_ndiv
				if(p_unitdiv(j2).ne.iunit)exit
				if(p_wdiv(j2).gt.p_wdiv(jmax))jmax=j2
			enddo !j2=j+1,p_ndiv   2733
			wei=p_isdiv(jmax)
			goto 90
		endif !if(p_unitdiv(j).eq.iunit)   2731
	enddo !j=1,p_ndiv   2729
 
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
		endif !if(is(i_))   2860
	enddo !i_= 1,n   2859
	return
end subroutine !subroutine pack2(itarget, is, n)
 
subroutine usejlpcoef(ivobjects)
	!call j_getname(ivobjects)
	!	write(6,*)'ivobjects',j_oname(1:j_loname)
	p_ivrhs=j_o(ivobjects)%i2(2)
	p_ivrhs2=j_o(ivobjects)%i2(3)
	p_ivnterminrow=j_o(ivobjects)%i2(4)
	p_ivvars=j_o(ivobjects)%i2(5)
	p_npvar=j_o(p_ivvars)%i(1)
	! 	p_ivcoefvars=j_o(ivobjects)%i2(6)
	!	p_ivcoefplus=j_o(ivobjects)%i2(7)
	p_ivcoef=j_o(ivobjects)%i2(8)
	p_ivtermvar=j_o(ivobjects)%i2(9)
	p_ivrowofterm=j_o(ivobjects)%i2(10)
 
	!	p_nrow=j_o(p_ivrhs)%i(3)
	!	p_nrowtot=p_nrow+1
	if(p_nrow.gt.0)then
		!	write(6,*)'pnrow',p_nrow
		p_rhs=>j_o(p_ivrhs)%d(1:p_nrow)
		p_rhs2=>j_o(p_ivrhs2)%d(1:p_nrow)  !userhscoef
	endif !if(p_nrow.gt.0)   2884
	p_ncol=j_o(p_ivvars)%i(1)
	p_vars=>j_o(p_ivvars)%i2(1:p_ncol)
	p_nterm=j_o(p_ivtermvar)%i(1)
	p_termvar=>j_o(p_ivtermvar)%i2(1:p_nterm)
	p_fpresent=any(j_otype(p_termvar).eq.j_iptable)
	p_coef=>j_o(p_ivcoef)%d(1:p_nterm)
	!	write(6,*)'pcoefhere',p_coef,p_nterm
	p_rowofterm=>j_o(p_ivrowofterm)%i2(1:p_nterm)
	p_nterminrow(0:)=>j_o(p_ivnterminrow)%i2(1:p_nrowtot)
 
	p_objr00=>j_o( j_o(ivobjects)%i2(11))%d(1:p_ncol)
	if(p_nrow.gt.0)p_matrix=>j_o( j_o(ivobjects)%i2(12))%d(1:p_npvar*p_nrow)
 
	if( j_v(j_o(ivobjects)%i2(13)).gt.j_0)then
		p_maxo=.true.
		p_coefmax=j_1
	else
		p_maxo=.false.
		p_coefmax=j_m1
	endif !if( j_v(j_o(ivobjects)%i2(13)).gt.j_0)   2902
 
 
 
end subroutine
 
subroutine jlpz(iob,io)
	use fletdmod
	use fletdmod2
	use fletcherdmod
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
	! 	rows in the matrix given in zmatrix->.
	!rhs2 &-1|1&MATRIX& Upper bounds as row or column vector having as many elements as there are
	! 	rows  in the matrix given in zmatrix->.
	!max&-1|1&MATRIX& The objective vector for a maximization problem. It must have as many elements as the
	! constraint matrix has columns.
	!min&-1|1&MATRIX& The objective vector for a minimization problem.
	!dpivot&-1|1&REAL& The objective function etc are printed after  dpivot pivots.
	!debug&-1|0|1&REAL& Gives the value of Pivot at which a pause is generated. During the pause all essential
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
	! x34c=matrix(2,values->(3,-2))
	!i10=10
	!zero=0
	!jlpzb=jlpz(problem->probzb,dpivot->1)
	!**Now different problem is obtained
	!x34c=matrix(2,values->(3,-3))
	!zero=1
	!jlpzb=jlpz(problem->probzb,dpivot->1)
	!**
	! **The matrices needed to use jlpz without problem-> can be obtained from a problem as follows
	! jlpcoefa=jlpcoef(probza)
	!jlpcoefalist=;list(jlpcoefa%?);
	!jlpza=jlpz(zmatrix->jlpcoefa%matrix,rhs->jlpcoefa%rhs,rhs2->jlpcoefa%rhs2,min->jlpcoefa%objective)
	!endex
	!endsection
 
 
	! ** The matrices can be obtained from the problem using jlpcoef() and then used as an input to jlpz()
 
 
	! zcoef=jlpcoef(probz)
	! zcoefs=;list(zcoef%?);
 
	! jz2=jlpz(min->
 
 
 
	! use fletcherdmod  !nopre!
	! use lastmod !nopre!
	! common/noutc/nout
	! common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj  !Fletcher parameters
	! !dense common/factorc/m0,m1,mm0,mm,mp,mq
	! !sparse common/factorc/m1,m2,mp,mq,lastr,p_irow
 
	! !this means that when in dense mpjj and mqjj are changed
	! ! then in sparse mpjj is the same as
 
	! common/ipivotc/ipivot99  !used only in fltcherd.for to print pivot at malfunctio
	! common/refactorc/nup,nfreq
 
	! p_p8=.true.
	! p_n16=6
	! call cpu_time(time00)
	! call j_getoption_index(iob,io,j_mproblem,-1,1,j_ipproblem,.true.,noptarg,j_optarg0)
	! if(j_err)return
	! !	p_row0=1   !updated for domainprob
	! p_ivproblem=j_optarg0(1)
	! p_isdomain=j_o(p_ivproblem)%i(10).gt.0
 
	! call initjlp(iob,io)  !checks all options needed without x-var
 
	! if (p_zmatrix) then
	! if(j_ivout.eq.j_ivresult)then
	! write(*,*)'*jlp with zmatrix-option must have output'
	! j_err=.true.
	! return
	! end if !if(j_ivout.eq.j_ivresult)   2637
	! !joko rhs tai rhs2 oltava
	! if(.not.(p_rhsopt.or.p_rhs2opt)) then
	! write(6,*)'*jlp: rhs-> or rhs2-> missing'
	! j_err=.true.;return
	! endif !if(.not.(p_rhsopt.or.p_rhs2opt))   2643
	! !	if(allocated(p_rhs))deallocate(p_rhs,p_rhs2)
	! !	allocate(p_rhs(1:p_nrow),p_rhs2(1:p_nrow))
	! if(allocated(p_lbou))deallocate(p_lbou);if(allocated(p_ubou))deallocate(p_ubou)
	! allocate(p_lbou(1:p_nrow),p_ubou(1:p_nrow))
	! ! rhs,rhs2 alustus vast. kuten problems-määrittelyssä
	! p_rhs=j_ninf
	! p_rhs2=j_inf
	! p_lbou=.false.
	! p_ubou=.false.
	! if(p_rhsopt) then
	! p_lbou=.true.
	! p_rhs(1:p_nrow) = j_o(p_ivrhs)%d(1:p_nrow)
	! do i_=1,p_nrow
	! if(p_rhs(i_).eq.j_ninf) then
	! p_rhs(i_)=j_ninf
	! p_lbou(i_)=.false.
	! endif !if(p_rhs(i_).eq.j_ninf)   2660
	! enddo !i_=1,p_nrow   2659
	! endif !if(p_rhsopt)   2656
	! if(p_rhs2opt) then
	! p_ubou=.true.
	! p_rhs2(1:p_nrow) = j_o(p_ivrhs2)%d(1:p_nrow)
	! do i_=1,p_nrow
	! if(p_rhs2(i_).eq.j_inf) then
	! p_rhs2(i_)=j_inf
	! p_ubou(i_)=.false.
	! endif !if(p_rhs2(i_).eq.j_inf)   2670
	! enddo !i_=1,p_nrow   2669
	! endif !if(p_rhs2opt)   2666
	! endif !if (p_zmatrix)   2636
	p_p=j_v(j_ivdollar2).eq.8.d0
	p_xpresent=.false.
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
	endif !if(ivprint.gt.0)   3085
 
	call j_getoption_index(iob,io,j_mproblem,-1,1,j_ipproblem,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_ivfeasible=j_getobject(j_ivout,'%feasible',j_ipreal)
	p_ivobjective=j_getobject(j_ivout,'%objective',j_ipreal)
	!	p_row0=1   !updated for domainprob
	p_ivproblem=j_optarg0(1)
	if(noptarg.gt.0)then
		p_ivrow=j_o(p_ivproblem)%i(4)  !text for rows
		p_nrow=j_o(p_ivrow)%i(0)-1
		p_nrowtot=p_nrow+1
		call jlpcoef_(p_ivproblem,j_ivout,ivobjects)   !jlpz(
		! p_coefvars
		! p_termvar
		!	p_nterminrow
		! p_rowofterm
		! p_coefplus
		! p_vars
 
		if(j_err)return
		call usejlpcoef(ivobjects)  !jlpz
		p_zmatrix=.false.
 
	else
		call j_getoption_index(iob,io,j_mzmatrix,1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
		if(j_err)then
			write(6,*)'jlpz() there must be either problem-> or zmatrix->'
			return
		endif !if(j_err)   3118
		p_zmatrix=.true.
		p_ivzmatrix=j_optarg0(1)
		p_nrow=j_o(p_ivzmatrix)%i(1)
		p_nrowtot=p_nrow+1
		p_ncol=j_o(p_ivzmatrix)%i(2)
		call j_getoption_index(iob,io,j_mrhs,1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
		if(j_err)return
		p_ivrhs=j_optarg0(1)
 
		if(j_o(p_ivrhs)%i(3).ne.p_nrow) then
			call j_printname('*jlp: rhs ',p_ivrhs,' should have same number elements as there are rows in p_zmatrix->')
			write(6,*)'Rows in zmatrix->:',p_nrow
			write(6,*)'Elements in rhs->:',j_o(p_ivrhs)%i(3)
			j_err=.true.;return
		endif !if(j_o(p_ivrhs)%i(3).ne.p_nrow)   3131
		p_rhs=>j_o(p_ivrhs)%d(1:p_nrow)
 
		call j_getoption_index(iob,io,j_mrhs2,1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
		if(j_err)return
		p_ivrhs2=j_optarg0(1)
		nrhsoptdim1=j_o(p_ivrhs)%i(1);nrhsoptdim2 =j_o(p_ivrhs)%i(2)
		if(j_o(p_ivrhs2)%i(3).ne.p_nrow) then
			call j_printname('*jlp: rhs2 ',p_ivrhs2,' should have same number elements as there are rows in p_zmatrix->')
			write(6,*)'Rows in zmatrix->:',p_nrow
			write(6,*)'Elements in rhs->:',j_o(p_ivrhs2)%i(3)
			j_err=.true.;return
		endif !if(j_o(p_ivrhs2)%i(3).ne.p_nrow)   3143
		p_rhs2=>j_o(p_ivrhs2)%d(1:p_nrow)
		call j_getoption_index(iob,io,j_mmax,-1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
		if(j_err)return
		if(noptarg.gt.0)then
			p_maxo =.true.
 
			p_coefmax=j_1
		else
			call j_getoption_index(iob,io,j_mmin,1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
			if(j_err)return
			if(noptarg.gt.0)then
				p_maxo =.false.
				p_coefmax=-j_1
			endif !if(noptarg.gt.0)   3159
		endif !if(noptarg.gt.0)   3152
		p_ivzobj=j_optarg0(1)
		write(6,*)'p_ivzobj ',p_ivzobj
		if(j_o(p_ivzobj)%i(3).ne.p_ncol)then
			call j_getname(p_ivzobj,ivmatrix)
			write(6,*)'objective ',j_oname(1:j_loname), 'has ',j_o(ivob)%i(3),' elements but zmatrix ',&
				j_oname2(1:j_loname2),' has ',p_ncol,' columns'
			j_err=.true.;return
		endif !if(j_o(p_ivzobj)%i(3).ne.p_ncol)   3166
		p_objr00=>j_o(p_ivzobj)%d(1:p_ncol)
 
	endif !if(noptarg.gt.0)   3100
	p_nz=p_ncol
	write(6,*)'p_nz',p_nz
	!	p_nrowtot=p_nrow+1
	!	write(6,*)' tas '
	iv=j_igetopt(iob,io,j_mdpivot)
	if(j_err)return
	if(iv.gt.0)then
		p_pivotstep=j_v(iv)
	else
		p_pivotstep=100
	endif !if(iv.gt.0)   3181
	!	write(6,*)'p_pivotstep',p_pivotstep
	!	p_nz=p_npvar
	!	p_ncol=p_nz
	p_nrowz=p_nrow+p_nz  ! basis (first element -1) of D part, when I is included
	p_nm=p_ncol+p_nrow
	p_mxn=p_nz
	p_mxnm=p_mxn+p_nrow
	p_zopt=.true.
 
	call commonopt(iob,io)
	if(j_err)return
	!	write(6,*)'after commonopt'
	if(p_nrow.eq.0)then
		write(6,*)'jlpz() needs constraints'
		j_err=.true.;return
	endif !if(p_nrow.eq.0)   3198
 
 
 
	call startlist()   !jlpz()
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
	if(p_p)write(6,*)'after startlis '
	call initoptz()
	if(p_p)write(6,*)'after initoptz'
 
 
	call initfltot()  !jlpz(
	!write(6,*)'after initflopt'
 
	ival=0
	!write(6,*)'p_nterminrow',p_nterminrow(0:p_nrow)
	!write(6,*)'vars',p_termvar
	!write(6,*)'coef',p_coef
	!write(6,*)'zvarsf',p_zvars(1:p_nz)
	call cpu_time(p_time0)
	p_time00=p_time0
 
	p_x=j_0
 
	j_v(p_ivoptimal)=j_0
	j_v(p_ivfeasible)=j_0
 
	j_v(p_ivstartedjlp)=j_1 !!!
	!	write(6,*)p_nrow,p_ncol,p_a,'la',la,p_lavec
	!write(6,*)'p_ls',p_ls,'lu1',lu1,ll1,p_ifail,nrefac
 
	!	p_objf=j_0
	p_objfv=p_small
	p_objf = p_small
	if(.not.p_maxo)write(6,*)'**minimization: maximize -objective'
	!	write(6,*)'NROW',p_nrow,p_nrowtot
	! p_goto900=.false.;p_goto1234=.false.;p_goto1578=.false.
	! p_goto400=.false.;p_goto36=.false.
	! p_goto100=.false.;p_goto5316=.false.;p_goto222=.false.;p_goto55=.false.
	! p_goto8888=.false.;p_goto8889=.false.;p_goto700=.false. 	! input for leaving
	! p_goto401=.false.
	write(6,*)' '
	write(6,*)'** Resid = # of basic residuals       z = # of basic z-variables'
	!	write(6,*)'   sched = # of explicit basic scheds  xkf = # of basic factory transportations'
	write(6,*)'      NF = # of nonfeafible rows      When NF>0 objective is a temporal objective'
	write(6,*)' '
	write(6,*) 'Pivots      Objective     Change%  resid   z     NF     dCPU       CPU'
	write(6,*)' ',('_',kk=1,80)
 
 
 
	do while(.true.)
		!	write(6,*)p_nrow
		!	write(6,*)p_nrow,'ku'
		!	write(6,*)'rhscur',p_rhscur
		!p_x,p_ls
		call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,0,p_rhscur,p_x,p_ls,wslu1,lwsll1,.false.)
		call isfeasible()
		if(p_p)write(6,*)'non feas ',p_nnf,p_feasible,p_objf
		!	if(p_p)write(6,*)
		if(p_pivot.eq.p_idebug)then
 
			call pullout(LWSLL1)
			idebug1=j_v(j_ivdebug)
			write(6,*)'pivot ',p_pivot
			call j_pause('A_debug>',do=.true.)
			if(j_err)return
			idebug2=j_v(j_ivdebug)
			if(idebug1.ne.idebug2)then
				p_idebug=idebug2
			else
				p_idebug=p_idebug+1
			endif !if(idebug1.ne.idebug2)   3296
		endif !if(p_pivot.eq.p_idebug)   3288
		if(j_err)then
			!call pullout(LWSLL1)
 
			return
 
		endif !if(j_err)   3302
		!		if(p_p)write(6,*)'aftupdate'
		iround=p_pivot/p_pivotstep
		!	write(6,*)'dpiv',p_pivot,iround,p_pivotstep
		if(iround*p_pivotstep.eq.p_pivot)then
 
			call cpu_time(time)
			!write(6,*)'timetas',time,p_time0,p_time00
			iminc=(time-p_time0)/60.
			imint=(time-p_time00)/60.
			secd=time-p_time0-iminc*60.
			sect=time-p_time00-imint*60.
			p_time0=time
 
			if(p_feasible.and.p_objfv.ne.p_small)then
				pros=abs(100.*(p_objf-p_objfv)/p_objfv)
				write(6,'(i8,g19.12,1x,f5.2,3i6,4x,i3,a,f5.2,i5,a,f5.2)')&
					p_pivot,p_coefmax*p_objf,pros,p_lr0,p_lz0,p_nnf,&
					iminc,':',secd,imint,':',sect
 
			else
				if(p_feasible)write(6,'(80x,a)')'*FEASIBLE'
				write(6,'(i8,g19.12,6x,3i6,4x,i3,a,f5.2,i5,a,f5.2)')&
					p_pivot,p_coefmax*p_objf,p_lr0,p_lz0,p_nnf,iminc,':',secd,imint,':',sect
			endif !if(p_feasible.and.p_objfv.ne.p_small)   3321
 
 
			p_objfv=p_objf
 
 
		endif !if(iround*p_pivotstep.eq.p_pivot)   3311
		! if(p_goto8888.or.p_goto8889)then
		! if(p_p)write(6,*)'befleaving'
		! call leaving()
		! cycle
		! if(p_p)write(6,*)'aftleav,pgoto123 ',p_goto1234
		! if(p_goto1234)exit
		! endif !if(p_goto8888.or.p_goto8889)   3364
		p_ienter=0  ! type of entering
		call renter()  !p_ienter=1
		if(p_p)write(6,*)'after renter ',p_ienter,'new c',p_newc
		call zenter()
		if(p_p)write(6,*)'after zenter',p_ienter,p_newc
		!	write(16,*)'renter,ienter,goto35,goto36',p_ienter  !,p_goto35,p_goto36
		!if(goto35)goto 35
		!		if(p_goto36) goto 36 !bypass renter and zenter because the is no improvement how this relates to cycling
		!	p_leave=0  !!!!position in ls
		!	p_leavec=0 !!!! leaving columns
		if(p_ienter.eq.0)exit
		if(p_ienter.gt.1)then  !!!!
 
			! compute r-vector
			!	if(p_p)write(p_n16,*)'p_newa',p_newa,'a:',p_a(1:p_nrow,p_newa)
			!	call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,0,p_a(1:,p_newa),r,&
			call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,0,p_a(p_abas(p_newa)+1:),r,& !!!!
				p_ls,wslu1,lwsll1,.false.)   !!!!!
			if(p_p)write(6,*)'aftfbsub p_post ',p_post
		else !if(p_ienter.gt.1)then
			! residual enters
			! p_newc=resid , mq=p_newc
			!call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,p_newc,p_a(1:,p_newa),r, & !!!!
			call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,p_newc,p_a(p_abas(p_newa)+1:),r, & !!!!
				p_ls,wslu1,lwsll1,.false.)   !!!!
		endif !if(p_ienter.gt.1)   3356
 
 
		if(.not.p_post)then
			do jj=1,p_nrow
				r(p_ls(jj))=-r(p_ls(jj))
			enddo !jj=1,p_nrow   3374
			p_post=.true.   !!!!
		endif !if(.not.p_post)   3373
		p_tmax=j_inf
		call zleavtmax()  !computes Ptmax if z leaves computes p_leavec leaving column (including p_nrow)
		if(p_p)write(6,*)'leavec aft zleav ',p_leavec
		if(p_leavec.gt.0)p_leave=p_lsi(p_leavec)
		if(p_p)write(6,*)'leave here ',p_leave
		call rleavtmax()
		if(p_leavec.gt.0)p_leave=p_lsi(p_leavec)
		if(p_p)write(6,*)'leavec aft rleav ',p_leavec,p_leave,'ls',p_ls,'lsi',p_lsi
		! if(p_p)write(6,*)'entleav ',p_ienter,p_leave,' P_newc ',p_newc,' p_leavk ',p_leavk
		! if(p_p)write(6,*)'#pivot10 ',p_ls(p_leave),p_newc,p_nrow,p_nm
		! if(p_p)write(6,*)
		p_icolold=p_ls(p_leave)   !tasbug
		p_icolnew=p_newc
		call pivot(p_icolold,p_icolnew,p_nrow,p_nm,p_a,p_lavec,e,wslu1,lwsll1,p_ifail,p_info)
		p_pivotcase=4
		j_o(p_ivpivotcases)%i2(4)=j_o(p_ivpivotcases)%i2(4)+1
		p_pivot=p_pivot+1
		!p_ienter=1  residual enter
		!	p_leavec=p_ls(p_leave)  ! column of leaving
		call jlplex(p_ls,p_leave,p_lsi(p_newc),p_lsi)
		if(p_ienter.eq.1)then
			! p_ilrmax idex in lr
			! residual enters
			call jlplex(p_lr,p_ilrmax,p_lr0+1,p_lri)
 
			p_lr0=p_lr0+1
			!	if(p_lr0.eq.p_nrow)nup=0   !clear refactorizing counter
		else if(p_ienter.eq.2)then !if(p_ienter.eq.1)then
			!  z enters
			call jlplex(p_lz,p_ilzmax,p_lz0+1,p_lzi)
			p_lz0=p_lz0+1
		endif !if(p_ienter.eq.1)   3399
		if(p_leavec.le.p_nrow)then
			! resiadual leaves
			! put leaving as first nonbasic
			call jlplex(p_lr,p_lr0,p_lri(p_leavec),p_lri)
			p_lr0=p_lr0-1
		else if(p_leavec.le.p_nrowz)then !if(p_leavec.le.j_nrow)then
			call jlplex(p_lz,p_lz0,p_lzi(p_leavec-p_nrow),p_lzi)
			p_lz0=p_lz0-1
		endif !if(p_leavec.le.p_nrow)   3411
		!		call zenter()
 
 
 
	enddo !while(.true.)   3279
 
 
	call cpu_time(time)
	!write(6,*)'timetas',time,p_time0,p_time00
	iminc=(time-p_time0)/60.
	imint=(time-p_time00)/60.
	secd=time-p_time0-iminc*60.
	sect=time-p_time00-imint*60.
	p_time0=time
	if(p_feasible)then
		j_v(p_ivoptimal)=j_1
 
		pros=abs(100.*(p_objf-p_objfv)/p_objfv)
		write(6,'(i8,g19.12,1x,f5.2,3i6,4x,i3,a,f5.2,i5,a,f5.2,a)')&
			p_pivot,p_coefmax*p_objf,pros,p_lr0,p_lz0,p_nnf,&
			iminc,':',secd,imint,':',sect,' *OPTIMAL*'
	else
		j_v(p_ivoptimal)=j_1
 
		pros=abs(100.*(p_objf-p_objfv)/p_objfv)
		write(6,'(i8,g19.12,1x,f5.2,3i6,4x,i3,a,f5.2,i5,a,f5.2,a)')&
			p_pivot,p_coefmax*p_objf,pros,p_lr0,p_lz0,p_nnf,&
			iminc,':',secd,imint,':',sect,' *NONFEASIBLE*'
 
 
	endif !if(p_feasible)   3434
 
	!	write(6,*)'sol',p_nureport,p_ivreport
	!call tulostele()
 
	if(p_nureport.ne.6)call j_printname('*report written into:',p_ivreport,' which remains open ')
 
	call repoz(p_nureport)
	if(p_echo.and.p_nureport.ne.6)call repoz(6)
	! if(allocated(p_ls)) deallocate(p_ls)   !varx;
	! if(allocated(p_lsi)) deallocate(p_lsi)   !varx;
	! if(allocated(p_a)) deallocate(p_a)   !varx;
 
	!	if(allocated(p_objr0)) deallocate(p_objr0)   !varx;
	!if(allocated(p_objr2)) deallocate(p_objr2)   !varx;
	if(allocated(p_xmi)) deallocate(p_xmi)   !varx;
	if(allocated(p_xma)) deallocate(p_xma)   !varx;
 
	nullify(p_objr)
	! if(allocated(p_lz)) deallocate(p_lz)
	! if(allocated(p_lzi)) deallocate(p_lzi)
	! if(allocated(p_redcost)) deallocate(p_redcost)
	! if(allocated(p_vc)) deallocate(p_vc)
	! if(allocated(p_lr)) deallocate(p_lr)
	! if(allocated(p_lri)) deallocate(p_lri)
	! if(allocated(p_lower)) deallocate(p_lower)
	! if(allocated(p_rhscur)) deallocate(p_rhscur) !ix,ixcur
	! if(allocated(p_tole)) deallocate(p_tole) !ix,ixcur
	! if(allocated(p_lbou)) deallocate(p_lbou) !rhs,rhs2,
	! if(allocated(p_ubou)) deallocate(p_ubou) !rhs,rhs2,
	if(j_err) then
		p_buf='jlp error exit'
	else !if(j_err) then
		p_buf='jlp normal exit'
	endif !if(j_err)   3480
	j_v(p_ivpivots)=p_pivot
	call  cpu_time(p_time0)
	write(p_nureport,*)'total cpu-time in jlp() ',p_time0-p_time00
	write(p_nureport,*)' '
	! write(6,*)'hep'
 
 
end subroutine jlpz
 
subroutine initfltot()
	use fletdmod !nopre!   !file jlpd.f90
	use fletdmod2 !nopre!
	!	use jlpdmod !nopre!
 
	use fletcherdmod  !nopre!
	use lastmod !nopre!
	integer ::nup
	logical indomain,indomain2
	common/noutc/nout
	common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj  !Fletcher parameters
	!dense common/factorc/m0,m1,mm0,mm,mp,mq
	!sparse common/factorc/m1,m2,mp,mq,lastr,p_irow
 
	!this means that when in dense mpjj and mqjj are changed
	! then in sparse mpjj is the same as
 
	common/ipivotc/ipivot99  !used only in fltcherd.for to print pivot at malfunctio
	common/refactorc/nup,nfreq
	!	write(6,*)'initfltot',p_nrow,p_ncol,p_nrow,nrefac
	!	write(6,*)'p_a',p_a
	!write(6,*)'p_lavec',p_lavec
	!	write(6,*)'p_ls',p_ls
	!	p_a,p_nrow,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac
	!	call initflet(p_nrow,p_ncol,p_a,la,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac)
	call initflet(p_nrow,p_ncol,p_a,p_nrow,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac)
	!	write(6,*)'afdti',p_ifail
	!write(6,*)'LS ',p_ls
	if(p_ifail.gt.0)then
		write(6,*)'**at startup p_ifail=',p_ifail, 'tell J. Lappi'
		j_err=.true.
		return
	endif !if(p_ifail.gt.0)   3522
	!	write(6,*)'afdtrr',p_ifail
	! if(allocated(p_lr))deallocate(p_lr);if(allocated(p_lri))deallocate(p_lri)
	!allocate( p_lr(1:p_nrow),p_lri(1:p_nrow))
 
 
 
 
	!if(allocated(p_x))deallocate(p_x,p_b)  !j_x2 added 201608
	!allocate( p_x(1:p_mxnm) ,p_b(1:p_mxn) ) ! x:n dimensio +
	!	p_x=j_0
	!	p_b=j_0
 
	! do j=1,p_nm !nm=ncol+nrow    ! number of columns in (I A)
	! p_lsi(j)=j    !intially ls is in order, residuasl are in the basis
	! enddo !j=1,p_nm   3431
	! ll0=ll1-1
 
	! ! residuals are initially in the basis
	! ! lr =list columns of I, i.e. residuals
	! do i=1,p_nrow
	! p_lr(i)=i
	! p_lri(i)=i	!inverse list
	! enddo !i=1,p_nrow   3438
	! p_lr0=p_nrow		! all residuals are in the basis
 
	! do i=1,p_nz
	! p_lz(i)=i		! list allz-cols in order
	! p_lzi(i)=i	! nrow not included
	! enddo !i=1,p_nz   3444
	! p_lz0=0			! no z-variable is in the basis
 
 
 
end subroutine
 
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
 
! subroutine pivotsp(nexnrowz,p_icolnew,nrow, &       !nopre!
! nm,a,lavec,e,ws,lws,ifail,info)
! use fletchersparse
! integer lws(*),lavec(0:*),info(*)
! real*8 a(*),ws(*),apu,e(*)
 
! call pivot(nexnrowz,p_icolnew,nrow, &
! nm,a,lavec,e,ws,lws,ifail,info)
! return
! end subroutine !subroutine pivotsp(nexnrowz,p_icolnew,nrow, &
subroutine jlpcoef(iob,io)
	!Section jlpcoef  jlpcoef() PROB into numeric form
	! This function is used at the beginning of jlp() and jlpz().
	!endheader
	!Note see jlpz() for how jlpcoef() can be used to demonstate problem defintion using matrices only.
	!endnote
	!endsection
 
	call j_startfunction(iob,io,j_ipproblem,narg,j_arg,ivout)
	if(j_err)return
	call jlpcoef_(j_arg(1),ivout,ivobjects)  ! jlpcoef(iob,io)
	return
 
end subroutine
 
 
 
subroutine jlpcoef_(ivproblem,ivout,ivobjects)
	! p_coefvars
	! p_termvar
	!	p_nterminrow
	! p_rowofterm
	! p_coefplus
	! p_vars
 
	integer,intent(in)::ivproblem,ivout
	integer,intent(out)::ivobjects
 
	integer ,dimension(:), pointer::nterminrow !=>null()
	integer ,dimension(:), pointer::vars !=>null()
	integer ,dimension(:), pointer::coefvars  !=>null(
	integer ,dimension(:), pointer::coefplus  !=>null(
	double precision, dimension(:),pointer ::coef
	integer ,dimension(:), pointer::termvar !=>null()
	integer  ,dimension(:), pointer::rowofterm  !=>null()
	!	write(6,*)'jlpcoef_',ivproblem,ivout,ivobjects
	! global pointer refer to the original
	if(j_otype(ivproblem).ne.j_ipproblem)then
		call j_getname(ivproblem)
		write(6,*)'*jlpcoef: ',j_oname(1:j_loname),' is not PROB'
		j_err=.true.;return
	endif !if(j_otype(ivproblem).ne.j_ipproblem)   3695
	if(ivout.eq.j_ivresult.or.ivout.le.20.or.ivout.gt.j_named)then
		write(6,*)'*jlpcoef: illegal ivout ',ivout
		j_err=.true.;return
	endif !if(ivout.eq.j_ivresult.or.ivout.le.20.or.ivout.gt.j_named)   3700
 
	ivcoefvars= j_o(ivproblem)%i(17)
	nterm=j_o(ivcoefvars)%i(1)  !local initial
	p_coefvars=>j_o(ivcoefvars)%i2(1:nterm)
 
	ivtermvar=j_o(ivproblem)%i(8)
	p_termvar=>j_o(ivtermvar)%i2(1:nterm)
 
	ivnterminrow=j_o(ivproblem)%i(7)
 
	!nrowtot=p_nrow+1  !j_o(ivnterminrow)%i(1)
	!	nrow=nrowtot-1
	p_nterminrow(0:)=>j_o(ivnterminrow)%i2(1:p_nrowtot)
 
	ivrowofterm=j_o(ivproblem)%i(6)  !rowofterms
	p_rowofterm=>j_o(ivrowofterm)%i2(1:nterm)
 
 
	ivcoefplus= j_o(ivproblem)%i(18)
	p_coefplus=>j_o(ivcoefplus)%i2(1:nterm)
 
	ivvars=j_o(ivproblem)%i(3)
	npvar=j_o(ivvars)%i(1)
	p_vars=>j_o(ivvars)%i2(1:npvar)
 
	nad=0 ! how many additonal
 
	do i=1,nterm
 
		ivterm=p_termvar(i)
 
		!	write(6,*)'ii',i,ivterm
		if(j_otype(ivterm).eq.j_iptable)then
			cycle
 
 
			!write(6,*)'iterm',i,ivterm
		elseif(j_otype(ivterm).eq.j_iplist)then
			ivcoe=j_o(ivcoefvars)%i2(i)
			if(j_otype(ivcoe).eq.j_ipmatrix)then
				if(j_o(ivcoe)%i(3).ne.j_o(ivterm)%i(1))then
					call j_getname(ivcoe,ivterm)
					write(6,*)'coefficient ',j_oname(1:j_loname),' of ',j_oname2(1:j_loname2), ' on row ',&
						p_rowofterm(i),' have not same size'
					j_err=.true. ;return
 
				endif !if(j_o(ivcoe)%i(3).ne.j_o(ivterm)%i(1))   3744
			elseif(j_otype(ivcoe).ne.j_ipreal.and.j_otype(ivcoe).ne.j_ipchar)then
				call j_getname(ivcoe)
				write(6,*)'coefficient ',j_oname(1:j_loname), ' is not REAL or MATRIX'
				j_err=.true. ;return
			endif !if(j_otype(ivcoe).eq.j_ipmatrix)   3743
			nad=nad+j_o(ivterm)%i(1)-1
 
		endif !if(j_otype(ivterm).eq.j_iptable)   3736
	enddo !i=1,nterm   3731
 
	nterm2=nterm+nad
	npvar2=npvar+nad !may have some extra
 
	ivnterminrow=j_deflist(ivout,'%nterminrow',list0=p_nrowtot, &
		list=p_nterminrow(0:p_nrow),ilist=.true.)
	nterminrow(0:)=>j_o(ivnterminrow)%i2(1:p_nrowtot)
 
	ivcoef=j_defmatrix(ivout,'%coef',1,nterm2,j_matreg)
	coef=>j_o(ivcoef)%d(1:nterm2)
	! p_pointer refer to original
	p_nterm=nterm2
 
	!	write(6,*)'nad',nad,nterm2
	if(nad.gt.0)then
		!reserve only space
 
 
		ivvars=j_deflist(ivout,'%vars',nres=npvar2) !p-variables
		vars=>j_o(ivvars)%i2(1:npvar2)
 
		ivcoefvars=j_deflist(ivout,'%coefvars',list0=nterm2)
		coefvars=>j_o(ivcoefvars)%i2(1:nterm2)
 
		ivcoefplus=j_deflist(ivout,'%coefplus',list0=nterm2,ilist=.true.) !row of each term
		coefplus=>j_o(ivcoefplus)%i2(1:nterm2)
 
 
 
		ivtermvar=j_deflist(ivout,'%termvar',list0=nterm2)
		termvar=>j_o(ivtermvar)%i2(1:nterm2)
 
 
		ivrowofterm=j_deflist(ivout,'%rowofterm',list0=nterm2,ilist=.true.) !row of each term
		rowofterm=>j_o(ivrowofterm)%i2(1:nterm2)
 
		!	nterminrow=p_nterminrow
		nterm2=0
		npvar2=0
		!write(6,*)'pulusinit',p_coefplus
		do i=1,nterm
 
			iro=p_rowofterm(i)
 
			ivterm=p_termvar(i)
			!	write(6,*)'ihere,iro',i,iro,' ivterm ',ivterm
			if(j_otype(ivterm).eq.j_iplist)then
				ivcoe=p_coefvars(i)
				!	call j_getname(ivcoe)
				!write(6,*)'coe ',j_oname(1:j_loname),j_otype(ivcoe),j_ipmatrix
				isiz=j_o(ivterm)%i(1)
				call j_getname(ivcoe)
				!write(6,*)'coef ',ivcoe,j_oname(1:j_loname),isiz,nterm2
				termvar(nterm2+1:nterm2+isiz)=j_o(ivterm)%i2(1:isiz)
				if(j_otype(ivcoe).eq.j_ipmatrix)then
					!write(6,*)'matrixcoef', j_o(ivcoe)%d(1:isiz)
					if(p_coefplus(i).ne.0)then
 
						coef(nterm2+1:nterm2+isiz)=j_o(ivcoe)%d(1:isiz)
					else
						coef(nterm2+1:nterm2+isiz)=-j_o(ivcoe)%d(1:isiz)
					endif !if(p_coefplus(i).ne.0)   3816
					do j=1,isiz
						coefvars(nterm2+j)=j_num2iv(j_o(ivcoe)%d(j))
					enddo !j=1,isiz   3822
 
				else
					if(p_coefplus(i).ne.0)then
 
						coef(nterm2+1:nterm2+isiz)=j_iv2val(ivcoe)
					else
						coef(nterm2+1:nterm2+isiz)=-j_iv2val(ivcoe)
					endif !if(p_coefplus(i).ne.0)   3827
				endif !if(j_otype(ivcoe).eq.j_ipmatrix)   3814
				rowofterm(nterm2+1:nterm2+isiz)=iro
				coefplus(nterm2+1:nterm2+isiz)=p_coefplus(i)
				nterm2=nterm2+isiz
				nterminrow(iro)=nterminrow(iro)+isiz-1
 
	loop:		do j=1,isiz
					do k=1,npvar2
						if(j_otype(vars(k)).eq.j_iplist) cycle loop
						if(vars(k).eq.j_o(ivterm)%i2(j))cycle loop
					enddo !k=1,npvar2   3840
					npvar2=npvar2+1
					vars(npvar2)=j_o(ivterm)%i2(j)
				enddo loop !p:		do j=1,isiz   3839
 
			else
				do k=1,npvar2
 
					if(vars(k).eq.ivterm)goto 700
				enddo !k=1,npvar2   3849
				npvar2=npvar2+1
				vars(npvar2)=ivterm
	700			nterm2=nterm2+1
				rowofterm(nterm2)=iro
 
				coef(nterm2)=j_iv2val(p_coefvars(i)) !p_coef(i)
				call j_getname(p_coefvars(i))
				!write(6,*)'coeny ',j_oname(1:j_loname),' ',nterm2,p_coefvars(i),j_v(p_coefvars(i)),coef(nterm2)
 
				if(p_coefplus(i).eq.0)coef(nterm2)=-coef(nterm2)
				termvar(nterm2)=p_termvar(i)
				coefplus(nterm2)=p_coefplus(i)
				coefvars(nterm2)=p_coefvars(i)
			endif !if(j_otype(ivterm).eq.j_iplist)   3806
 
			!write(6,*)'nterm ',nterm
		enddo !i=1,nterm   3800
 
 
		!write(6,*)'nterm,npvar ',nterm,npvar
		!write(6,*)'p_befvars ',p_vars
		!j_o(ivvars)%i2(1:npvar2)=vars(1:npvar2)
		j_o(ivvars)%i(1)=npvar2
		!write(6,*)'p_vars ',p_vars
		!write(6,*)'rofofebef',p_rowofterm
		!	j_o(ivrowofterm)%i2(1:nterm2)=rowofterm(1:nterm2)
		!write(6,*)p_rowofterm
		!write(6,*)'termarsbef', p_termvar(1:p_nterm)
		!		j_o(ivtermvar)%i2(1:nterm2)=termvar(1:nterm2)
 
		!write(6,*)p_termvar(1:nterm)
		!write(6,*)'coefbef',p_coef
		!		j_o(ivcoef)%i2(1:nterm)=coef(1:nterm)
		!write(6,*)p_coef
		! if(j_ivout.ne.j_ivresult) then
		! call j_deflist(j_ivout,'%vars',iv,list0=npvar,list=vars(1:npvar)) !p-variables
		! call j_deflist(j_ivout,'%rowofterm',iv,list0=nterm,list=rowofterm(1:nterm),ilist=.true.) !row of each term
		! call j_deflist(j_ivout,'%nterminrow',iv,list0=p_nrowtot,list=p_nterminrow(0:p_nrow),ilist=.true.)
		! call j_deflist(j_ivout,'%termvar',iv,list0=nterm,list=termvar(1:nterm))
		! !
		! !		j_o(iv)%d(1:nterm)=coef(1:nterm)
		! endif !if(j_ivout.ne.j_ivresult)   4889
 
	else
		do i=1,nterm
			!		if(j_otype(p_termvar(i)).eq.j_iptable)cycle
			j_o(ivcoef)%d(i)=j_iv2val(p_coefvars(i))
			if(p_coefplus(i).eq.0)j_o(ivcoef)%d(i)=-j_o(ivcoef)%d(i)
		enddo !i=1,nterm   3897
 
		j_o(ivcoefplus)%i2(1:nterm)=p_coefplus(1:nterm)
		ivtermvar=j_deflist(ivout,'%termvar',list0=nterm,list=p_termvar(1:nterm))
		ivnterminrow=j_deflist(ivout,'%nterminrow',list0=p_nrowtot,list=p_nterminrow(0:p_nrow),ilist=.true.)
		ivvars=j_deflist(ivout,'%vars',list0=npvar,list=p_vars(1:npvar)) !p-variables
		ivrowofterm=j_deflist(ivout,'%rowofterm',list0=nterm,list=p_rowofterm(1:nterm),ilist=.true.) !row of each term
 
	endif !if(nad.gt.0)   3774
 
 
	!	write(6,*)'hellurei'
	! RHS
	ivrhsvars=j_o(ivproblem)%i(1)
	ivrhs2vars=j_o(ivproblem)%i(2)
	if(p_nrow.gt.0)then
		ivrhs=j_defmatrix(ivout,'%rhs',1,p_nrow,j_matreg)
 
		!write(6,*)'NROW ',nrow
 
		ivrhs2= j_defmatrix(ivout,'%rhs2',1,p_nrow,j_matreg)
		do i=1,p_nrow
			j_o(ivrhs)%d(i)=j_iv2val(j_o(ivrhsvars)%i2(i))
			j_o(ivrhs2)%d(i)=j_iv2val(j_o(ivrhs2vars)%i2(i))
		enddo !i=1,p_nrow   3922
	endif !if(p_nrow.gt.0)   3916
 
	ivobjective=j_defmatrix(ivout,'%objective',1,npvar,j_matreg)
	do i=1,j_o(ivnterminrow)%i2(1)
		ico=j_inlistobject(j_o(ivtermvar)%i2(i),ivvars)
		j_o(ivobjective)%d(ico)=j_o(ivcoef)%d(i)
	enddo !i=1,j_o(ivnterminrow)%i2(1)   3929
 
	if(p_nrow.gt.0)ivmatrix=j_defmatrix(ivout,'%matrix',p_nrow,npvar,j_matreg)
	do i=j_o(ivnterminrow)%i2(1)+1,nterm
		iro=j_o(ivrowofterm)%i2(i)
		ico=j_inlistobject(j_o(ivtermvar)%i2(i),ivvars)
		j_o(ivmatrix)%d((iro-1)*npvar+ico)=j_o(ivcoef)%d(i)
	enddo !i=j_o(ivnterminrow)%i2(1)+1,nterm   3935
 
	ivobjects=j_deflist(ivout,'%objects',list0=13)
	j_o(ivobjects)%i2(1)=ivproblem
	j_o(ivobjects)%i2(2)=ivrhs
	j_o(ivobjects)%i2(3)=ivrhs2
	call j_getname(ivrhs)
 
	!	write(6,*)j_oname(1:j_loname),' nrow ',j_o(ivrhs)%i(3)
	!write(6,*)'heelurei2'
	j_o(ivobjects)%i2(4)=ivnterminrow
	j_o(ivobjects)%i2(5)=ivvars
 
	j_o(ivobjects)%i2(6)=ivcoefvars
	j_o(ivobjects)%i2(7)=ivcoefplus
	j_o(ivobjects)%i2(8)=ivcoef
	j_o(ivobjects)%i2(9)=ivtermvar
	j_o(ivobjects)%i2(10)=ivrowofterm
	j_o(ivobjects)%i2(11)=ivobjective
	j_o(ivobjects)%i2(12)=ivmatrix
 
 
	iv=j_getobject(ivout,'%objtype',j_ipreal)
	j_v(iv)=j_o(ivproblem)%i(5)
	j_o(ivobjects)%i2(13)=iv
 
	!	write(6,*)'coef',j_o(ivcoef)%d
	!	write(6,*)'coefssh',j_o(ivobjects)%i2
 
 
 
end subroutine
 
subroutine jlp(iob,io)   ! %%jlp  !!!!****************************************************
 
	use fletdmod2   !closflet
	!	logical zpresent
	character*2 chint
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
	!\item  Output%objective= value of the objective function
	!\item  	Output%rows= the vector the valuef of the constraint rows.
	!\item   Output%shprice = vector of shadow prices of the rows
	!\item  Output%varx= LIST of xvariables in the schedules data
	!Output%varxproblem= LIST of x-variables in the PROB
	!\item  Output%xsum= Vector of sums of variables in Output%varxproblem
	!\item  Output%xprice Shadow prices of the variables in Output%varxproblem
	!\item   Output%xsumint The sums of x-variables in the integer approximation, generated if integer-> is present
	!\end{itemize}
	!problem&1&PROB &Problem object produced with problem()
	!data&1&DATA& Schedules data (Sic!) linked to it with unitdata if ubit-> is not given
	!unit-> gives the  unit variable which changes when unit changes.
	!
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
	! the computations.
	!stop&-1|1&CODE& codeoption telling when iterations over units stop. The variables
	! Round (current number of rounds over units), Change% (change of objective during the last 10
	! rounds), Imp (number of entering schedules outside the active set when updating the active set),
	! 	Active% ( %-size of the active set) and all global variables in JLP22.
	!default is stop->(Change%.lt.0.01.and.Round.ge.10).
	! fast%&-1|1&CODE & codeoption computing Fast%. All schedules whose price is larger than Fast%
	!of the current key schedule. Same variables can be used as for stop-> and also current Fast%.
	! A possible rule is fast%->(min(Fast%+5-(Imp.gt.0)*10,98))). The default is Fast%=85.
	! maxrounds%&-1|1&REAL & maximum number of rounds over all units. Default maxrounds->3000
	!report&-1|1& CHAR & the results are written to the file spesified.
	!echo|-1|0& & &When results are printed to a file, echo-> implies that they are written alo to the terminal.
	!refac&-1|1|&REAL& refac->value tells that the factors of the basis matrix are recomputed after value pivot operations.
	!The default is refac->1000.
	!tole&-1|1&REAL& tole->value tells that the default tolerances are multiplied with the value.
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
	! ** sums of x-variables are stored into the same variables with jlpa%prefix
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
	!**    a logical statement in terms of stand variable
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
	! sawdata=datawcase(read->(X,Y,capacity1...capacity5,util1...util5))
	! S1,20,30,5,6,7,8,6,10,10,11,10,10,12
	! S2,70,60,6,6,7,8,6,10,12,13,13,11,15
	! S3,10,70,5,6,7,8,6,10,13,14,14,11,13
	! /
 
	! **pulpmill properties
	! pulpdata=datawcase(read->(X,Y,capacity1...capacity5,util1...util5))
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
 
	! *sawdata("i",util"j") is value of variable util"j" for observation i in saw mill data
	! sutil%coef("i","j")=sawdata("i",util"j")-
	! scost*sqrt((sawdata("i",X)-x)***2+(sawdata("i",Y)-y)***2)
 
	! ;enddo
	! ;enddo
 
	! ;do(i,1,len(pulpdata%case))
	! ;do(j,1,len(pulplogs))
 
	! putil%coef("i","j")=pulpdata("i",util"j")-
	! pcost*sqrt((pulpdata("i",X)-x)***2+(pulpdata("i",Y)-y)***2)
 
	! ;enddo
	! ;enddo
	! /
 
	! fprob=problem()
	! ** utility from sawmills + utility from pulpmills
	! sutil+putil==max
 
	! ;do(j,1,len(sawlogs))
	! ;do(i,1,len(sawdata%case))
 
	! * capacity constraints
	! ;if(sawdata("i",capacity"j").gt.0)@sawlogs("j")%2%@sawdata%case("i")<sawdata("i",capacity"j")
 
	! ;enddo
	! ;enddo
 
	! ;do(j,1,len(pulplogs))
	! ;do(i,1,len(pulpdata%case))
 
	! ;if(pulpdata("i",capacity"j").gt.0)@pulplogs("j")%2%@pulpdata%case("i")<pulpdata("i",capacity"j")
 
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
	! p_ivobjective REAL for the objective
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
	! p_npvar number of variables in the problem
	! p_vars point to variables
	! p_nterm number of terms
	! p_termvar variable of each term
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
	! p_ivstarted is optimization started
	! p_newc
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
	! p_ienter entering
	!        = 1 residual enters
	!        =2
	!        =3
	!        4
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
	! p_lopp number of schedules
	! p_ivkeepc
	! p_ivdatac p_ivdatax
	! p_ivdatamatx
	!
	! Fletcher
	! p_mxd max number of
	! p_lavec
	! p_mxn mx number of columns of A , D + z-var
	! p_ncol =p_mxn
	! p_nm=p_ncol+p_nrow
	! p_nrowz=p_nrow+p_nz  ! basis (first element -1) of D part, when I is included
	! p_xma maximum number of w
	! p_lx0 number of basic w
	! p_lz0 number of basix z
	! p_lr0 number of basic residuasl
	! p_lf0 number og basci factory vars
 
 
	call j_startfunction(iob,io,0,narg,j_arg,j_ivout,needsout=.true.)
	if(j_err)return
 
	call j_getoption_index(iob,io,j_mproblem,1,1,j_ipproblem,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_ivproblem=j_optarg0(1)
	p_ivrow=j_o(p_ivproblem)%i(4)  !text for rows
	p_nrow=j_o(p_ivrow)%i(0)-1
 
	if(p_nrow.eq.0)then
		write(6,*)'jlp() does not now work without constraints, sorry'
		j_err=.true.
		return
	endif !if(p_nrow.eq.0)   4398
	p_nrowtot=p_nrow+1
	p_isdomain=j_o(p_ivproblem)%i(10).gt.0
	p_ndom=0
	p_nvarf=0
	if(p_isdomain)then
		p_ivdomain=j_o(p_ivproblem)%i(11)
		p_ndom=j_o(p_ivdomain)%i(0)
	endif !if(p_isdomain)   4407
 
	p_ivobjective=j_getobject(j_ivout,'%objective',j_ipreal)
	!j_o(ivproblem)%i(4)=p_ivrow  !text for rows
 
	!	write(6,*)'jlp1'
	call j_getoption_index(iob,io,j_mshowdomain,-1,99,j_ipchar,.true.,p_nshow,p_show)
	write(6,*)'jlp2',p_nshow
	if(j_err)return
	!	p_row0=1   !updated for domainprob
	if(p_nshow.gt.0)then
		p_ivshowunits=j_deflist(j_ivout,'%showunits',list0=p_nshow,ilist=.true.)
		les=0
		do j=1,p_nshow
			les=les+j_o(p_show(j))%i(2)-j_o(p_show(j))%i(1)+1
		enddo !j=1,p_nshow   4423
 
		p_ivshowtext=j_deftext(0,'$showtext',p_nshow,les+10*p_nshow)
		p_ivshowmatrix=j_defmatrix(0,'$showdom',p_nshow,1,j_matreg)
		do j=1,p_nshow
			write(chint,'(i2)')j
			j1=2
			if(j.ge.10)j1=1
			call j_getchar(p_show(j),p_domainname,le)
			call j_puttext(p_ivshowtext,'$showdom('//chint(j1:2)//')='//p_domainname(1:le))
		enddo !j=1,p_nshow   4429
 
		call j_command('$showtrans=trans(in->$showtext)')
 
		if(j_err)then
			write(6,*)'*error in interpreting $showdomaintext'
			return
		endif !if(j_err)   4439
		p_ivshowtrans=j_object('$showtrans')
		!j_o(ivout)%i2(1)=ivinl
		write(6,*)'jlp2'
		p_ivshowin=j_o(p_ivshowtrans)%i2(1)
		p_nshowin=j_o(p_ivshowin)%i(1)
	else
		p_nshow=0
	endif !if(p_nshow.gt.0)   4420
 
	write(6,*)'jlp3',p_nshow
 
	p_ivfeasible=j_getobject(j_ivout,'%feasible',j_ipreal)
	j_v(p_ivfeasible)=j_0
 
	p_isobj0=.false.
	p_isobj2=.false.
	!	write(6,*)'heer'
 
	call jlpcoef_(p_ivproblem,j_ivout,ivobjects)  !jlp(
	! p_coefvars
	! p_termvar
	!	p_nterminrow
	! p_rowofterm
	! p_coefplus
	! p_vars
	!	write(6,*)'heer2,fpresent',p_fpresent,p_ntable
 
 
 
	if(j_err)return
	!	call j_getname(ivobjects)
	!	write(6,*)'ivobjectstas ',j_oname(1:j_loname)
	call usejlpcoef(ivobjects)  !fpresent comes from here
	if(j_err)return
	call initdata0(iob,io)
	if(j_err)return
	write(6,*)'problem has ',p_nterm,' terms'
	! p_ivarea=
	! p_isunit
	!p_ivdatac=ivdata
	! p_ivdatax=j_o(ivdata)%i(3)
	! p_ivunit
	!		p_ivkeepx=
	! p_nkeepx=
	! p_ivdatamatc=
	! p_ivkeepc=
	! p_ivunit=
	! p_ivdatamatx=
	! p_lopp=
	! p_datamatx
	! p_ivs
	!	j_ivns
	!j_filterlink
	!p_filter_
	!j_rejectlink=
	!p_reject=
	!p_filre
	!p_isvarx
	!p_istermx
	!p_ivnxinrow=
	!p_ninrowx(0:)
	!p_nrowcurx
	!p_nxvar
	!p_nstot
	!p_ibaunitbas
	!p_nrejtot=
	!p_maxns
	!p_nunits
	!p_ivxvar
	!p_varx
	!p_ivibaunit
	!p_ibaunit
	!p_ivkeys
	!p_keys
	!	p_ivns
	!	p_ns=
 
	if(p_fpresent)call fterms(iob,io)  !knn i checked here
	call j_clearoption(iob,io)  !jlp()
	if(p_fpresent)write(6,*)'aftfterms'
	write(6,*)'tabpart',p_tabpart
	write(6,*)'logpart',p_logpart
	write(6,*)'factpart',p_factpart
	if(j_err)return
	!	call j_getname(ivobjects)
	!write(6,*)'ivobjectstas ',j_oname(1:j_loname)
 
	if(p_fpresent)then
 
		call initfact()
		! else
 
		! call initkeysx()
	endif !if(p_fpresent)   4530
	write(6,*)' '
	write(6,*)'x-variables',p_nxvar
	p_xpresent=p_nxvar.gt.0
	p_nxvartot=p_nkeepx  !j_o(p_ivkeepx)%i(1)
	p_nsumx=p_nxvartot  !+p_noutsubtrans !p_ncvar+
	ndom2=max(p_ndom,1)
	if(allocated(p_sumx))deallocate(p_sumx)
	allocate( p_sumx(1:p_nsumx*(ndom2+p_nshow)))
	ivprint=j_igetopt(iob,io,j_mprint)
 
	if(ivprint.gt.0)then
 
		p_iprint=j_v(ivprint)
	elseif(ivprint.eq.0)then !if(ivprint.gt.0)then
		p_iprint=2 !testauksen ajaksi
	else !if(ivprint.gt.0)then
		p_iprint=1
	endif !if(ivprint.gt.0)   4547
	if(p_nrow.eq.0)then
		if(p_npvar.ne.p_nxvar.and..not.p_fpresent)then
			write(6,*)p_npvar-p_nxvar,' variables were not in data'
			j_err=.true.
 
		endif !if(p_npvar.ne.p_nxvar.and..not.p_fpresent)   4556
		j_v(p_ivfeasible)=j_1
		call jlp00(iob,io)
		if(j_err)return
		!	write(6,*)'obj',p_objf
		call getsumxkey()
		!	write(6,*)'sumx',p_sumx
		call repo(p_nureport)
 
		return
		!		call tulostele
 
	endif !if(p_nrow.eq.0)   4555
	! if(ivout.eq.j_ivresult)then
	! write(6,*)'jlp() needs now output'
	! j_err=.true. ;return
 
	! endif !if(ivout.eq.j_ivresult)   3909
	call j_getoption_index(iob,io,j_mfastdif,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.ge.1)p_fastdif=j_v(j_optarg0(1))
	!	p_row0=1   !updated for domainprob
 
 
	!	ibaxdat(iobs)=(iobs-1)*p_nkeepx
	p_p8=.false.  !.true. !.true. !j_v(j_ivdollar2).eq.8.d0
	p_p=j_v(j_ivdollar2).eq.8.d0   !.false.  !.true.
	p_p9=.false.  !.true.
	p_n16=16
	p_zerob=0
	p_feasible1=.true.
	call cpu_time(p_time00)
	!write(6,*)'time00tas',time,p_time0,p_time00
 
	!	p_fpresent=j_o(ivout)%i(17).ne.0
	if(p_p8)write(p_n16,*)'<33startinitjlp'
	if(p_p8)write(6,*)' '
	!write(6,*)'idomain0',p_isdomain
 
	! endif !if(j_linkoption(iob,io,j_mdata).lt.0)   3032
 
	! call initjlp(iob,io)  !checks all options needed without x-var
	! write(6,*)'idomain5',p_isdomain,p_xpresent
	!	j_o(ivobjects)%i2(1)=ivproblem
	p_ivrhs=j_o(ivobjects)%i2(2)
	p_ivrhs2=j_o(ivobjects)%i2(3)
	ivnterminrow=j_o(ivobjects)%i2(4)
	ivvars=j_o(ivobjects)%i2(5)
 
	!call j_getname(p_ivrhs)
	!write(6,*)'rhs :',j_oname(1:j_loname)
 
 
 
	! 	p_ivcoefvars=j_o(ivobjects)%i2(6)
	!	p_ivcoefplus=j_o(ivobjects)%i2(7)
	ivcoef=j_o(ivobjects)%i2(8)
	ivtermvar=j_o(ivobjects)%i2(9)
	ivrowofterm=j_o(ivobjects)%i2(10)
 
	!	p_nrow=j_o(p_ivrhs)%i(3)
 
 
	!	p_nrowtot=p_nrow+1
	if(p_nrow.gt.0)then
		p_rhs=>j_o(p_ivrhs)%d(1:p_nrow)
		!	write(6,*)'pnrow',p_nrow,p_rhs
		!	return
		p_rhs2=>j_o(p_ivrhs2)%d(1:p_nrow)
	endif !if(p_nrow.gt.0)   4624
	p_npvar=j_o(ivvars)%i(1)
	p_vars=>j_o(ivvars)%i2(1:p_npvar)
	p_nterm=j_o(ivtermvar)%i(1)
	p_termvar=>j_o(ivtermvar)%i2(1:p_nterm)
	p_coef=>j_o(ivcoef)%d(1:p_nterm)
	p_rowofterm=>j_o(ivrowofterm)%i2(1:p_nterm)
	p_nterminrow(0:)=>j_o(ivnterminrow)%i2(1:p_nrowtot)
 
	!	write(6,*)'xphny',p_xpresent
 
	call commonopt(iob,io)
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
	p_mxd=p_nrowtot+4  !???max number of d-vectors mx number of
 
	if(.not.p_isunit)p_maxns=j_o(p_ivdatac)%i(9)
	!is(p_p8)write(6,*)'<pmaxns ',p_maxns
 
	!	call j_getdat(p_ivdatax,p_lopp,p_ivdatamatx,p_ivkeepx)
 
	if(j_err)return
	!	p_nkeepx=j_o(p_ivkeepx)%i(1)
	!	p_datamatx=>j_o(p_ivdatamatx)%d(1:p_nkeepx*p_lopp)
	write(6,*)'xpresent ',p_xpresent
	if(p_xpresent)call initxdatawx()
	! inits something of x-data used also with factories and without ordinary x-variables
	!initilizes also some fact issues
	! uses p_ninrowx (0p_nrow)
	! p_ivxcoef
	! p_xcoef  (1:p_nxval)
	! p_ivxtermvar
	! p_xtermvar x-term variables
	! p_ivrowcurx  !current rowxs if there are now domaisn these are not updated
	! p_rowcurx
	! p_ibatemp  (1:p_nrowtot)
	! p_nrowxtemp (1:p_nrowtot)
	! p_ntemp0
	! p_nrowxtemp (1:p_ntemp0) number of x- for each temp
	! p_ivirowvarx= (j_ivout,'%irowvarx',list0=p_nxval)
	!	p_irowvarx=>
	!	p_ivnrowx2=j_deflist(j_ivout,'%nrowx2',list0=p_nrowcurx,ilist=.true.)
	!	p_nrowx2=>j_o(p_ivnrowx2)%i2(1:p_nrowcurx)
	!	p_irowvarx=pack(p_termvar,p_istermx)
	! p_nrowx2=pack(p_ninrowx,p_ninrowx.gt.0) !numbers of x-variables in each
	! p_ivxmat=j_defmatrix(j_ivout,'%xmat',p_lopp,p_ntemp0,j_matreg,single=.true.)
	! p_xmat=>j_o(p_ivxmat)%r(1:p_ntemp0*p_lopp)
	!	allocate(p_xmat(1:p_ntemp0*p_lopp))
	! p_ivvxpack=  (j_ivout,'%vxpack',p_ntemp0,1,j_matreg,single=.true.)
	! p_vxpack=>
	! p_ivixpack=j_deflist(j_ivout,'%ixpack',list0=p_ntemp0,ilist=.true.)
	! p_ixpack=>j_o(p_ivixpack)%i2(1:p_ntemp0)
	! p_ivvarx=j_deflist(j_ivout,'%varx',list0=p_nxvar)
	! p_varx=>j_o(p_ivvarx)%i2(1:p_nxvar)
	! p_varx= pack(p_vars(1:p_npvar),p_isvarx)   ! c-variables are here included as x -variables
	! p_ivibaunit= j_deflist(j_ivout,'%ibaunit',list0=p_nunits+1,ilist=.true.)
	! p_ibaunit=>j_o(p_ivibaunit)%i2(1:p_nunits+1)
 
 
	!	call initxdata()
	! p_fastreject
	! p_rejects
	! p_xsmin p_xsmax
	! p_xmin p_xmax
	! p_ivxps
	! p_xps
	! p_ivvaluedif   j_defmatrix(j_ivout,'%valuedif',p_nunits,1,j_matreg)
	!	p_ivobjdif       j_defmatrix(j_ivout,'%objdif',p_nunits,1,j_matreg)
 
 
	p_nz=p_npvar-p_nxvar-p_nvarf
	!	write(6,*)'nzhere',p_nz
	if(p_intapp.and.p_nz.gt.0)then
		write(6,*)' '
		write(6,*)'*there were ',p_nz," z-variables, can't use integer->"
		j_err=.true.;return
	endif !if(p_intapp.and.p_nz.gt.0)   4733
	if(p_nz.gt.0)call initzvar()
	!	write(6,*)'nshere',p_ns(1:5)
	!if(allocated(p_rowcurx))write(6,*)'i8rowx cur ',p_rowcurx(1:p_nrowcurx)
	!write(6,*)'allo ',allocated(p_rowcurx)
	if(j_err)return
 
	!	zpresent=p_nz.gt.0
	write(6,7777)'z-variables',p_nz
 
	write(6,*)' '
	!if(p_nz.gt.0)write(6,*)'zvars',p_zvars(1:p_nz)
	if(p_p)write(6,*)'<444datainitilized'
	if(p_p)write(6,*)' '
	!	write(6,*)'<9995rhs',p_rhs
 
 
	! mitkä olivat xvar-muuttujat
	la=p_nrow ! number of rows in Fletcher
	p_lavec(1)=la
	p_mxd=p_nrowtot+4
	p_mxn=p_mxd+p_nz !mx number of columns in A  , I.e. D+ coefficients of z-variables
	if(p_fpresent) p_mxn=p_mxn+p_mxd
	p_ncol=p_mxn !!!
	p_mxnm=p_mxn+p_nrow !mx number of columns (icluding the I part)
	p_nm=p_ncol+p_nrow
 
	call startlist()
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
 
 
 
 
	if(p_isdomain)then
		if(p_p)write(6,*)'start initdomain'
		if(p_p)write(6,*)' '
		if(.not.p_xpresent)then
			write(6,*)'there cannot be domains without constraints for x-variables'
			j_err=.true.; return
		endif !if(.not.p_xpresent)   4804
 
		call initdomain()
		!	if(allocated(p_rowcurx))write(6,*)'i9rowx cur ',p_rowcurx(1:p_nrowcurx)
		if(j_err)return
	else
		p_ndom=0
		p_ivrowx= j_deflist(j_ivout,'%rowx',list0=p_nrowx,ilist=.true.)
		p_rowx=>j_o(p_ivrowx)%i2(1:p_nrowx)
		!	if(allocated(p_rowx))deallocate(p_rowx)
		!p_nrowx=p_nrowcurx
		!allocate(p_rowx(1:p_nrowx))
		p_rowx=p_rowcurx(1:p_nrowx)
 
	endif !if(p_isdomain)   4801
	!write(6,*)'p_domv ',p_ndomv
 
	if(p_nshow.gt.0)then
		!if(allocated(p_showin))deallocate(p_showin)
		!	allocate(p_showin(1:p_nshowin)
		do j=1,p_nshowin
			iiv=j_o(p_ivshowin)%i2(j)
			ini=j_inlistobject(iiv,p_ivkeepc)
			!			p_showin(j)=ini
			if(ini.gt.0)cycle
 
			if(iiv.eq.ivunit)cycle
 
			call j_getname(iiv,ivunit)
			write(6,*)' '
			write(6,*)'*showdomain variable ',j_oname(1:j_loname),&
				' is not in c-data and is not the unit variable ',j_oname2(1:j_loname2)
			j_err=.true.
 
		enddo !j=1,p_nshowin   4827
		if(j_err)return
 
	endif !if(p_nshow.gt.0)   4824
 
	!	write(6,*)'inttt',p_intapp
 
 		 7777 format(a17,i5)


	if(p_fpresent.or..true.)then
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
 
		if(.not.p_zmatrix) p_nz=p_npvar-p_nxvar-p_nlogfact-p_ntable
		!write(6,*)'nztas',p_nz
	else
		if(.not.p_zmatrix) p_nz=p_npvar-p_nxvar
 
 
	endif !if(p_fpresent.or..true.)   4851
 
	call initxdata()
 
 
	if(p_p)write(6,*)'start initopt'
 
	!	write(p_n16,*)'p_nm ',p_nm
	call initopt()
	!	write(6,*)'nshtase',p_ns(1:5)
	!	write(p_n16,*)'p_nmaftinitop ',p_nm
	if(j_err)return
	if(p_p)write(6,*)'startt initxdatjlp'
	!	if(allocated(p_rowcurx))write(6,*)'i66rowx cur ',p_rowcurx(1:p_nrowcurx)
	write(6,*)'rethere'
	if(p_fpresent)return
	call initxdatjlp() !needs fact    updates p_ns
	!write(6,*)'after',j_err
	!write(6,*)'nshNYT',p_ns(1:5)
	!	write(p_n16,*)'p_nmaftinitop ',p_nm
	if(j_err)return
	!	if(allocated(p_rowcurx))write(6,*)'i77rowx cur ',p_rowcurx(1:p_nrowcurx)
	!	if(p_fpresent)call factxps(i),p_zopt
	write(6,*)p_nz,' z-variables:'
	!write(6,*)'p_zopt,j_err',p_zopt,j_err
	if(p_nz.gt.0)	call printz()
	if(p_nz.gt.0.and. .not.p_zopt)then
		write(6,*)'*jlp: if there are z-variables, there must be z-> option, zvariables:'
		call printz()
		j_err=.true.
		return
	endif !if(p_nz.gt.0.and. .not.p_zopt)   4896
 
	call cpu_time(time1)
	write(6,*)' '
	write(6,*)'*jlp initialization took ',time1-p_time00,' sec',j_err
 
	!p_n16=16
	!p_p=.true.
	!	write(6,*)'p_ivdatamatc,p_ivdatamatx ',p_ivdatamatc,p_ivdatamatx
	!xpresent2: are there ordinary x-variables
	!	p_xpresent= p_nxvar.ne.0
 
	! if(.not.p_xpresent)then
	! if(.not.p_zmatrix) p_nz=p_npvar
	! !*p_ix0 the x-variable of the objective function
	! p_ix0=0
	! p_nunits=0
	! end if !if(.not.p_xpresent)   2856
	!if(p_nrow.ne.0.and..not.p_fpresent.and.p_nz.eq.0)call preopt()
	!p_kier=0  !number of iterations through data !!!!
	!if(allocated(p_rowcurx))write(6,*)'i555rowx cur ',p_rowcurx(1:p_nrowcurx)
	!write(6,*)'j_err bef feas ',j_err
	p_objf=p_small
	if(p_xpresent)call checkinfeas()
	!if(allocated(p_rowcurx))write(6,*)'i5333rowx cur ',p_rowcurx(1:p_nrowcurx)
	!	call checkinfeas()
	!write(6,*)'j_err feas ',j_err
	if(j_err)return
 
	!  nrow =number of constraints=number of variables for Fletcher's LP
	! first index is for row number second for column
 
	call initfltot()
	if(p_fpresent)then
		write(6,*)'factory optimization works only this far'
 
		!	if(j_ierr.gt.0)write(6,*)'**NOTE THERE WERE TRANSPORTATION PROBLEMS'
		return
		!	j_err=.true.;return
	endif !if(p_fpresent)   4934
	p_unitv=-1
	p_kierv=1
1234 continue  !we come here after failure,   !jl 20160510 siirretty tähän !!!!
!write(6,*)'newstart'
!write(16,*)'newstart'
	p_xirowold2=p_small
 
	!	if(p_xpresent)then
	p_next(0)=0;p_iprev(0)=0
	!tehdasosuden alustukset
	if(p_fpresent) then
		p_lunxkf=p_mxd; p_lunw=0;p_lunits0=0
		p_nextf=0
		p_nextf(p_mxd,1:p_nlog)=p_mxd
		p_iprevf(p_mxd,1:p_nlog)=p_mxd
	endif !if(p_fpresent)   4951
	!	endif !if(p_xpresent)   2904
	if(allocated(p_lower)) deallocate(p_lower)
	allocate(p_lower(1:p_nrow))
	p_ivrhsw=j_defmatrix(j_ivout,'%rhsw',p_nrow,1,j_matreg)
	p_rhsw=>j_o(p_ivrhsw)%d(1:p_nrow)
	! get currenet RHS and working rhs
	do i=1,p_nrow
		! lower-logiikka epävarma vielä
		if((p_ix(i).ne.0).or.p_fpresent)then   ! there is a x-variable in the row
			! x-variable present -xps goes to the rhs
			if(p_xps(i).gt.p_rhs2(i).or.p_rhs(i).eq.j_ninf)then
				p_rhscur(i)=p_rhs2(i) ! select either lower or upper bound
				! rhscur = current bound
				p_lower(i)=.false.
			else !if(j_xps(i).gt.j_rhs2(i).or..not.j_lbou(i))then
				p_rhscur(i)=p_rhs(i)
				p_lower(i)=.true.
			endif !if(p_xps(i).gt.p_rhs2(i).or.p_rhs(i).eq.j_ninf)   4967
			!xps is the sum over key schedules
			p_rhsw(i)=p_rhscur(i)-p_xps(i)  ! 6.27 and 6.28 p. 110 , get working rhs
			!	if(i.eq.2)	write(27,*)'<353553rhs ',p_rhscur(i),p_xps(i)
		else !if((j_ix(i).ne.0).or.j_fpresent)then
			if(p_lbou(i))then
				p_rhscur(i)=p_rhs(i)
				p_lower(i)=.true.
			else !if(j_lbou(i))then
				p_rhscur(i)=p_rhs2(i)
				p_lower(i)=.false.
			endif !if(p_lbou(i))   4979
			p_rhsw(i)=p_rhscur(i)
		endif !if((p_ix(i).ne.0).or.p_fpresent)   4965
		if(p_rhs2(i).lt.p_rhs(i))then
			write(6,*)'*constraint ',i,' upper limit ',p_rhs2(i),' less than lower limit ',p_rhs(i)
			j_err=.true.
		endif !if(p_rhs2(i).lt.p_rhs(i))   4988
	enddo !i=1,p_nrow   4963
	if(j_err)return
	!	write(6,*)'rhscur',p_rhscur
	!	write(6,*)'rhsw',p_rhsw
	!	write(6,*)'lower',p_lower
 
	! Fletcher subroutines initilize data structures for taht many cols
	!*nrowz
	p_nrowz=p_nrow+p_nz  ! basis (first element -1) of D part, when I is included
	! coefficients for residuals
	! coefficients for d columns
	do j=p_nrowz+1,p_mxnm !!!ncol ol liian vähän
		p_xma(j)=1.  ! maximum value for w, later area of the unit
	enddo !j=p_nrowz+1,p_mxnm   5003
 
 
	! pitäs olla sama kuin
	! for fletcher m= p_ncol
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
	!		p_ifail=mode
	! m0de  mode of operation (larger numbers imply extra information):
	! 0 = cold start (no other information available, takes simple
	! bounds for the initial active set)
	! FLS
 
	if(p_nrow.eq.0.and..not.p_fpresent)then   ! no constraints, was optimized directly
		if(p_nz.gt.0)then
			write(6,*)'**no constraints and z variables=> illegal'
			j_v(p_ivfeasible)=j_0
			j_v(p_ivoptimal)=j_0
			j_v(p_ivobjective)=-9.9
			j_err=.true.
			return
		endif !if(p_nz.gt.0)   5029
		p_objf=p_xps(0)
		p_vx(0)=1.
 
		p_lx0=0
		p_lz0=0
		p_lr0=0
		p_lf0=0
		if(p_xpresent)then
			call defsolu()
			call getsolx()
		endif !if(p_xpresent)   5044
		j_v(p_ivfeasible)=p_one
		j_v(p_ivoptimal)=p_one
		j_v(p_ivobjective)=p_coefmax*p_objf
		j_v(p_ivstartedjlp)=1.
		write(6,*)'no constraints, objective ',p_coefmax*p_objf
		if(p_nureport.ne.6)call j_printname( &
		'*report written into:',ivreport,' which remains open')
		call repo(p_nureport)
		!	write(6,*)'77 ',p_nureport,p_echo
		if(p_nureport.ne.6.and.p_echo)call repo(6)
 
		return
	endif !if(p_nrow.eq.0.and..not.p_fpresent)   5028
	j_v(p_ivstartedjlp)=j_1 !!!!
 
	! milloin refactoroidaa, Fletcherin refactoroinnissa on bugi
 
	! initilize Fletcher routines
	!if(sparse)then
	!	call initfletsp(p_nrow,p_ncol,p_a,la,p_lavecsp,p_ls,lu1,ll1,p_ifail,nrefac)
	!else !if(sparse)then
	!*nrow = n for Flet
	! write(6,*)p_nrow,p_ncol,p_a,'la',p_nrow,p_lavec,p_ls,'lu1',lu1,ll1,p_ifail,nrefac
	! call initflet(p_nrow,p_ncol,p_a,p_nrow,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac)
	! !endif !if(sparse)  15090
	! if(p_p)write(6,*)'startup,nrow,nm,la,lu1,ll1',p_nrow,nm,la,p_ncol,&
	! lu1,ll1
	! testi
 
	p_newc=0
 
	p_muutosb=0  !changes in the basis
	p_objfv=p_small    !for checking if there has not been improvemne tsince last !!!!
	p_objf = p_small   !!!!
 
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
	p_unit55=0;p_niter=0
	!nkier=p_mxiter
	nunits=p_nunits
	! if(.not.p_xpresent2)then
	! nkier=1
	! nunits=1
	! endif !if(.not.p_xpresent2)   4635
	!if(j_o(ivxdatmat)%r(10261*keepx).eq.0.)stop 90
	! iba=ibamatx(6841,1)
	! iba2=ibamatx(6842,2)
	! write(18,*)'<129>'
	! write(18,*)j_o(ivxdatmat)%r(iba+1:iba+keepx)
	! write(18,*)j_o(ivxdatmat)%r(iba2+1:iba2+keepx)
	!if(p_pp)write(16,*)'***initialupdate'
	call updatejlp()
	!	write(6,*)'firstupdtaejlp ',p_p
	if(p_idebug.eq.p_pivot.or.j_err)then
		idebug1=j_v(j_ivdebug)
		call pullout(LWSLL1)
		write(6,*)'pivot ',p_pivot
		j_yes=j_err
		call j_pause('Bdebug>',do=.true.)
		if(j_yes)then
			j_err=.true.
			return
		endif !if(j_yes)   5198
		!if(p_pivot.ge.p_pullout1.or.j_err)call pullout(LWSLL1)
		idebug2=j_v(j_ivdebug)
		if(idebug1.ne.idebug2)then
			p_idebug=idebug2
		else
			p_idebug=p_idebug+1
		endif !if(idebug1.ne.idebug2)   5204
 
	endif !if(p_idebug.eq.p_pivot.or.j_err)   5192
	if(j_err)return
 
 
	!write(16,*)'ALKU updgoto900,goto1234,goto8888,goto8889',goto900,goto1234,goto8888,goto8889,&
	!p_unit,p_pivot,j_objf
	p_goto900=.false.;p_goto1234=.false.;p_goto1578=.false.
	p_goto400=.false.;p_goto36=.false.
	p_goto100=.false.;p_goto5316=.false.;p_goto222=.false.;p_goto55=.false.
	p_goto8888=.false.;p_goto8889=.false.;p_goto700=.false. 	! input for leaving
	p_goto401=.false.
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
	endif !if(p_isfastp)   5248
 
	if(p_fpresent)write(6,*)'finterval->',iterxkf
 
	if(p_isstop)then
		write(6,*)'stop->   :see jlp(stop->'
	else !if(p_isstop)then
		write(6,*)'stop->(Change%.lt.0.01.and.Round.ge.10)'
	endif !if(p_isstop)   5258
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
 
	!	endif !if(p_xpresent2)   4932
 
	!write(6,*)'objinit ',p_objr(1:p_nrow)
 
 
	iobs=0
	!do ii=1,nunits
	!do jj=1,6844
 
	! iba=ibamatx(6841,1)
	! iba2=ibamatx(6842,2)
	! write(18,*)'<19>'
	! write(18,*)j_o(ivxdatmat)%r(iba+1:iba+keepx)
	! write(18,*)j_o(ivxdatmat)%r(iba2+1:iba2+keepx)
	! !write(18,*)iobs,j_o(ivxdatmat)%r(iba+1),j_o(ivxdatmat)%r(iba+keepx),j_xmat(iba2+1),j_xmat(iba2+j_ntemp0)
 
	!stop 967
 
	p_nimp=0  !number of improvements outside active set
	!	nimpr=0   !number of rounds with fastmake between printing
	p_asv=100.
	p_objfprev=p_small
	!pros=p_small
 
	call cpu_time(p_time0)
 
	p_time00=p_time0
	!	write(6,*)'p_time0,p_time00',p_time0,p_time00
	ncyc=0
	write(6,*)' '
	!	write(6,*)'Round  Pivots      Objective                           active% resid   z   sched  xkf  NF-rows'
	write(6,*) &
		'Round  Pivots    Objective      Change% active%  resid z  sched xkf  NF   dCPU      CPU      Imp  Fast%'
	!   2           19  15489.5951070      5.6924 100.00    1    0    4    0    0  0: 0.14    0: 0.14    0
	!	write(6,'(i5,i8,g19.12,7x,f8.2,5i5,f6.2,f7.2,i5)')&  written later
	!		0,0,p_coefmax*p_objf,100.,p_lr0,p_lz0,p_lx0,p_lf0,p_nnf,j_0, &
	!		j_0
 
	!write(6,*)'xmatfrodisk',j_xdatformdisk
	!if(j_o(ivxdatmat)%r(10261*keepx).eq.0.)stop 100
	p_goto401=.false.
	ipivotstart=-1
 
	! ckeck first can z-variables enter this looks like optimization in jlpz()
	call updatejlp()
	!	write(6,*)'firstupdtaejlp ',p_p
	if(j_err)then
		idebug1=j_v(j_ivdebug)
		call pullout(LWSLL1)
		write(6,*)'pivot ',p_pivot
		j_yes=j_err
		call j_pause('Cdebug>',do=.true.)
		if(j_yes)then
			j_err=.true.
			return
		endif !if(j_yes)   5328
		!if(p_pivot.ge.p_pullout1.or.j_err)call pullout(LWSLL1)
		idebug2=j_v(j_ivdebug)
		if(idebug1.ne.idebug2)then
			p_idebug=idebug2
		else
			p_idebug=p_idebug+1
		endif !if(idebug1.ne.idebug2)   5334
 
	endif !if(j_err)   5322
 
	if(p_nz.gt.0)then
		p_ienter=0
580	call zenter()
		if(p_ienter.ne.0)then  !!!! select leaving variable    !<b1>
			if(p_p)	write(6,*)'resid p_inter ',p_ienter
			call leaving()
			!	if(.not.cancel)then
 
			call updatejlp()
 
 
			if(j_err)then
				!	write(6,*)'firstupdtaejlp ',p_p
 
				!idebug1=j_v(j_ivdebug)
				write(6,*)'pivot #2 ',p_pivot
				call pullout(LWSLL1)
 
 
				call j_pause('Ddebug>',do=.true.)
 
				!if(p_pivot.ge.p_pullout1.or.j_err)call pullout(LWSLL1)
 
			endif !if(j_err)   5353
 
			!	endif !if(.not.cancel)   4802
		endif !if(p_ienter.ne.0)   5345
 
	endif !if(p_nz.gt.0)   5342
 
	!	p_testl=.true.
	p_kierstep=1
	p_kierout=0
	kierloop: do p_kier=0,p_maxrounds
	!	write(6,*)'kier ',p_kier

		p_unit=1
		!	p_unit=1
		!if(p_pp)write(16,*)'p_kier,fast,fastmake',p_kier,fast,fastmake
		! if(fastmake)then
		! !if(p_pp)write(16,*)'fastmake alkaa',j_xmatinmemory,j_xdatinmemory,j_xdatfrodisk
		! if(.not.j_xmatinfmemory)j_xmatlast=0
		! if(.not.j_xdatinmemory)j_xdatlast=0
		! endif
		! how many round through all units
		!	p_p=.true.
 
		!p_kier=p_kier+1 !!!! <B332> new round completed
		! write(16,*)'tas'
		!		if(p_xpresent2)then
 
		if(p_kier.eq.p_kierout)then
			call tulostele2(iob)
			if(p_goto900)goto 900 !exit kierloop
			!	write(6,*)'*********'
			!	call tulostele3()
			if(kier.eq.10)p_kierstep=10
			p_kierout=p_kierout+p_kierstep
 
		endif !if(p_kier.eq.p_kierout)   5393
		!if(goto900)write(6,*)'<55>goto00nyt'
		!	if(p_goto900)exit kierloop !return
		!if(goto785)goto 785  !maxiter
		!if(goto699)goto 699
 
		p_xirowold2=p_objf
		if(p_nnf.ne.p_nnfold2)p_xirowold2=p_small
		p_nnfold2=p_nnf
		!		endif !if(p_xpresent2)   4511
		if(p_p)write(6,*)'<unitloop,p_nunits',p_nunits
		if(p_fastmake)p_fastmakes=p_fastmakes+1
		!	if(p_fastmake)write(6,*)'kier fastamke',p_kier,p_fastmakes
		if(p_fpresent)p_logfactbas=0
 
		unitloop: do p_unit=1,p_nunits
			if(p_pivot.ne.ipivotstart)then
				iunitchange=p_unit
 
			elseif(p_unit.eq.iunitchange)then
				write(6,*)'*no change during the last Round'
 
				if(p_feasible)write(6,*)'SOLUTION FOUND'
				goto 900
			endif !if(p_pivot.ne.ipivotstart)   5417
			ipivotstart=p_pivot
 
			if(p_unit.eq.1)p_idomba=0
 
			!if(p_kier.gt.1.or.iunit.gt.1)
			!write(6,*)'<57hei',goto401,j_xpresent2
 
			p_goto401=.true.  !tai 700
			p_sentered=.false.   ! is a schchedule entered
			!write(6,*)'<57hei',p_goto401
			ipivotstart=p_pivot
 
			!		inunitloop: do while(.true.)
 
			!		write(6,*)'inunit',p_kier,p_goto401
			!	if(p_goto401)goto 401  ! ei tarvi päivittää
55 continue !!!!********************************** main cycle

!if(p_pp)write(16,*)'inunit,update alkaa**',fast,fastemake,p_kier,goto900,goto1234,goto8888,goto8889,j_objf,ienter
!write(6,*)'<57hei2',goto401

			if(p_feasible.and.p_feasible1)then
 
				! if(p_mustupdate)call updatejlp()
				! write(6,*)'aft updatejlp ',p_p
 
				write(6,'(72x,a)')'*FEASIBLE*'
				write(6,'(i5,i8,g19.12,7x,f8.2,5i5,i3,a,f5.2,i5,a,f5.2)')&
					p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0, &
					p_lf0,p_nnf  !,iminc,':',secd,imint,':',sect
				p_feasible1=.false.
			endif !if(p_feasible.and.p_feasible1)   5447
			! if(p_ispullout.or.j_err)then
			! if(p_pivot.ge.p_pullout1.or.j_err)call pullout(LWSLL1)
 
			! endif !if(p_ispullout.or.j_err)   4542
			!if(j_err)return
			!write(6,*)'<57hei3',goto401,goto900,goto1234,goto8888,goto8889
			!write(16,*)'updatenjälkeen***',p_kier,goto900,goto1234,goto8888,goto8889,j_objf,ienter
			!write(16,*)'updgoto900,goto1234,goto8888,goto8889',goto900,goto1234,goto8888,goto8889,&
			!iunit,p_pivot,j_objf
			!if(goto900)write(6,*)'<27>goto900tas'
			!		write(6,*)'p_goto900,p_goto1234,p_goto8888,p_goto8889',p_goto900,p_goto1234,p_goto8888,p_goto8889
			! if(p_goto900)exit kierloop;
			! if(p_goto1234)goto 1234  !virhe
			! if(p_goto8888.or.p_goto8889)goto 100;  !leaving
 
 
 
			!write(16,*)'renter,ienter,goto35,goto36',ienter,goto35,goto36
			!if(goto35)goto 35
			!			if(p_goto36) goto 36 !bypass renter and zenter because the is no improvement how this relates to cycling
 
 
			!if(ienter.ne.0.and.p)write(p_n16,*)' maxcol ',p_newc,' vcmax',j_vcmax
			!		if(p_ienter.ne.0)goto 100  !!!! select leaving variable    !<b1>
 
			! z enters *****************
			! loop over nonbasic z:s
			!35 continue   !!!! start z-enters ******************* ! <B2>
			!			call zenter()
 
			!if(p_ienter.ne.0)goto 100  !!!!! get leaving variable   !<b2>
 
			! vc always of correct sign for maximization
 
 
 
 
			if(p_activeunit(p_unit))then
				if(p_fpresent)then
					p_ilog=1
					p_inebas=0
					p_logfactbas=0
 
 
	778				p_ienter=0
					call fenter()
					if(p_ienter.ne.0)then
						call leaving()
						goto 778
					endif !if(p_ienter.ne.0)   5504
 
 
 
 
				endif !if(p_fpresent)   5496
 
				call senter()
 
 
			endif !if(p_activeunit(p_unit))   5495
			!		write(6,*)'senter ',p_ienter,p_goto36
 
			if(p_ienter.eq.0)then
				if(p_unit.eq.p_unitprev)then
					write(6,*)' '
					write(6,*)'no change during last round'
					write(6,*)'solution is optimal'
					j_v(p_ivoptimal)=j_1
 
					goto 900
				endif !if(p_unit.eq.p_unitprev)   5521
 
				cycle unitloop
 
			endif !if(p_ienter.eq.0)   5520
			!			if(p_ienter.ne.0)then
			if(p_p)write(6,*)'s can enter ,unit ',p_unit, 'pivot ',p_pivot
			call leaving()
			!		if(.not.cancel)then
			if(p_lr0.eq.p_nrow.and.p_pivot.gt.p_pivotold)then !due to a bug in Fletcher
				write(6,*)'here888 startlist0',p_pivot,p_pivotold
				call startlist0()  !jlp()
				! initilize lists defined in startlist(
				call initflet(p_nrow,p_ncol,p_a,p_nrow,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac)
				!	write(6,*)'here999'
 
				p_pivotold=p_pivot
			endif !if(p_lr0.eq.p_nrow.and.p_pivot.gt.p_pivotold)   5537
	380				call updatejlp()
			if(j_err)then
				idebug1=j_v(j_ivdebug)
				call pullout(LWSLL1)
				write(6,*)'pivot ',p_pivot
				close(27)
				j_yes=j_err
				call j_pause('Edebug>',do=.true.)
				if(j_yes)then
					j_err=.true.
					return
				endif !if(j_yes)   5554
				!if(p_pivot.ge.p_pullout1.or.j_err)call pullout(LWSLL1)
				idebug2=j_v(j_ivdebug)
				if(idebug1.ne.idebug2)then
					p_idebug=idebug2
				else
					p_idebug=p_idebug+1
				endif !if(idebug1.ne.idebug2)   5560
 
			endif !if(j_err)   5547
 
 
			j_o(p_ivobjdif)%d(p_unit)=p_objf-p_objfv
			!		endif !if(.not.cancel)   4964
			p_ienter=0  ! type of entering
 
			call renter()
			if(p_nz.gt.0)call zenter()
			if(p_ienter.gt.0)then
				if(p_p)write(6,*)'rzenter ',p_ienter,p_goto36
				call leaving()
				!		if(.not.cancel)then
				if(p_lr0.eq.p_nrow.and.p_pivot.gt.p_pivotold)then !due to a bug in Fletcher
					write(6,*)'herestartlist0',p_lr0,p_nrow,p_pivot,p_pivotold
					call startlist0()  !jlp()
					call initflet(p_nrow,p_ncol,p_a,p_nrow,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac)
					!	write(6,*)'here2'
 
					p_pivotold=p_pivot
				endif !if(p_lr0.eq.p_nrow.and.p_pivot.gt.p_pivotold)   5579
				!		endif !if(.not.cancel)   5005
				goto 380
				!	cycle inunitloop
 
			endif !if(p_ienter.gt.0)   5575
			!				cycle unitloop
 
			!			endif !if(p_ienter.ne.0)   4613
 
			!			cycle unitloop
 
 
			!36 continue   ! kun sivuutetaan residual enters ja z
			!*************************************************************
			!!!! can new schedule enter ********************************** <B3>
			!!!! get first the prices of x-variables
 
 
			! testiä , tähän tullaan myös jos w:t sai laittomat arvot
			!			if(p_testl)call testcol()
 
			!jatketaanko seuraavasta tehdasmjasta !!!!
			! if (p_fpresent) then
			! if(p_nextxkf) then   !!!!
			!	tarkistetaan pitääkö laskea muunnokset !!!! <A1> tehdaan alussa
			!	iunitrans yksikkö, jolle viimeksi laskettu tehdasmuunnokset
			! if ((p_nfy.gt.0).and.(p_unit/=p_unitrans)) then
			! do j=1,j_o(p_ivkeepc)%i(1)
			! j_v(j_o(p_ivkeepc)%i2(j))= &
			! j_o(p_ivdatamatc)%d((p_unit-1)*j_o(p_ivkeepc)%i(1)+j)
			! enddo !j=1,j_o(p_ivkeepc)%i(1)   3494
			! do j=1,p_nutiltrans
			! call dotrans(p_utiltrans(j),1)
			! if(j_err)then
			! write(6,*)'error for transformation ',j
			! endif !if(j_err)   3500
			! enddo !j=1,p_nutiltrans   3498
			! p_unitrans=p_unit
			! endif !if ((p_nfy.gt.0).and.(p_unit/=p_unitrans))   3493
			! if(p_p) write(p_n16,*)'<5836> nextxkf true'
			! goto 5316   !!!! jatketaan seuraavasta tehdasmuuttujasta
			!!! varmistetaan tehdasmjien läpikäynti alusta tilanteessa,
			!!! jossa yksikön tehdasmja-listan läpikäyntiä ei jatketa
			! else !if(j_nextxkf) then
			! ixkenter = 0  !!!!
			! endif !if(p_nextxkf)   3490
			! endif !if (p_fpresent)   3489
 
			!***********
 
			!400	continue  !!!!! goto next unit etc
			!write(17,*)'400kier',p_kier
			!			cycle unitloop
 
			!401 continue
			p_unitprev=p_unit  !!!!
			!			p_goto401=.false. !we jump here when entering inunitloop first time
			!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			!	call ispe(isper,1) !testing negative amounts
			!write(20,*)'p_unitv,p_kier,p_kierv,p_unitprev',p_unitv,p_kier,p_kierv,p_unitprev
 
 
			! 700   continue
			!***************************************************************
 
			!!!! the most time consuming loop
			!!!! compute the value of each schedule, take the largest + the value of the key schdeule
			!!!! if the largest valuie is greater than the vlaue of the key schedule this schedule can enter
			! if(.not.p_xpresent)then  !!!check if optimal with only z-problem   <R1>
			! if(p_ienter.ne.0)goto 100
			! write(6,*)'*** solution,obj',p_objf
 
			! j_v(p_ivoptimal)=j_1
			! exit kierloop !goto 900          !   <r1>
			!			endif !if(.not.p_xpresent)   3449
			!if(p_pp)write(16,*)'beforesenter,iente',p_ienter,p_unit,j_objf,p_nnf,p_pivot,kier,goto400
 
			!			call senter()  !note p_ienter=3 is not yet set it is set in entercol
			!if(p_pp)write(16,*)'senter,iente',p_ienter,p_unit,j_objf,p_nnf,p_pivot,kier,goto400
			!			if(p_goto400)exit inunitloop !cycle unitloop !goto 400
			!			p_sentered=.true.
			!write(17,*)'senter2,iente',p_ienter,p_unit,j_objf,p_nnf,p_pivot,p_kier
			! end tärkein looppi
			!********************end: can a schedule enter
			!222	continue !secondb
 
 
			!if(p_pp)write(16,*)'unit,ns,valopt,valk,key,p_iopt',p_unit,j_nsch(p_unit),&
			!j_valueopt,j_valuek,j_keys(p_unit),p_iopt
			! if finding feasible voisi suoraan katsoa, että valuoptin ja valuek:n eroa
 
			! if(p_iopt.eq.p_keys(p_unit))then  !!!!
			! if (.not.p_fpresent)then   !!!!
			! !				exit inunitloop !cycle unitloop !goto 400   !goto next unit , 400 is earlier !!!!
			! else !if (.not.j_fpresent)then
			! !	write(16,*)'goto5316tas'
			! goto 5316 ! fpresent &&  (p_iopt == key)  !!!!
			! endif !if (.not.p_fpresent)   4763
			! endif !if(p_iopt.eq.p_keys(p_unit))   4762
			! if(p_valueopt.le.p_valuek+p_tolecur) then  !!!!
			! if (.not.p_fpresent)then   !!!!
			! exit inunitloop !cycle unitloop !goto 400   !goto next unit , 400 is earlier !!!!
			! else !if (.not.j_fpresent)then
			! !	write(16,*)'goto5316tas'
			! goto 5316  ! fpresent &&  (valueopt <= valuek+tolecur) !!!!
			! endif !if (.not.p_fpresent)   4771
			! else !if(j_valueopt.le.j_valuek+j_tolecur) then
			! if(p_fpresent)call fenter0() !;if (j_fpresent)call fenter0()
			! !if(p_pp)write(16,*)'befentcol'
			! call entercol()
			! !if(p_pp)write(16,*)'aftentcol',p_ienter,goto100 jfjfjf
			! !	endif !if (j_fpresent) then
			! !	write(16,*)'goto5381'
 
			! goto 100 !goto 5381 ! (p_iopt != key) && (valueopt > valuek+tolecur) !!!!
			! endif !if(p_valueopt.le.p_valuek+p_tolecur)   4770
 
			! ! <b34>
			! 5316		continue  !!!!!testataan voiko xkf tulla kantaan <C2>
 
			! if(p_kier.gt.1.and.iterxkf*int(p_kier/iterxkf).ne.p_kier.and..not.p_sentered)exit inunitloop !cycle unitloop !goto 400
 
			! if(p_factnow)call fenter() !j_fpresent)call fenter() ! ;if(p_ienter.ne.0)write(16,*)p_ienter,goto100,goto400,goto5316
			! !write(17,*)'fenter',p_ienter,goto100,goto400,goto5316,p_kier
			! if(p_goto100)goto 100  !leaving
			! if(p_goto400)exit inunitloop  !cycle unitloop !goto 400
			! if(p_goto5316)goto 5316
			! !endif
			! ! there is change, not yet quarantee againts degenerate loop, because
 
			! ! 5381 call entercol()
 
 
			! 100 continue  !!!! select leaving variable
			! !goto8888 is now input which may be set in updatejlp
			! !goto2244 and goto 8889 are input
			! !goto8883 input
			! !if(p_pp)write(16,*)'beleav,goto222,goto900,goto55,goto1234',goto222,goto900,goto55,goto1234,j_objf,p_pivot,p_kier
			! !if(p_pp)write(16,*)'before leaving,p_ienter',p_ienter
			! call leaving()
			! !if(p_pp)write(16,*)'after leaving,p_ienter',p_ienter,p_newa,j_tmax
			! !if(p_pp)write(16,*)'after leaving,p_ienter',p_ienter,p_newa,j_tmax
			! !if(p_pp)write(16,*)'alleav,goto222,goto900,goto55,goto1234',goto222,goto900,goto55,goto1234,j_objf,p_pivot,p_kier
			! !write(17,*)'leav,goto222,goto900,goto55,goto1234',goto222,goto900,goto55,goto1234,j_objf,p_pivot,p_kier
			! cycle inunitloop
			! write(6,*)'aftleavin'
			! if(p_goto222)write(6,*)'goto 222'
			! if(p_goto222)goto 222
			! !if(goto900)write(6,*)'<88>goto900aftleaving'
 
			! if(p_goto900)exit kierloop
			! if(p_goto1234)write(6,*)'goto1234'
			! if(p_goto1234)goto 1234
			! ! if(goto55)goto 55
			! if(p_goto112233)exit inunitloop
			! !goto 55      !!!!computations after normal pivot are done, goto computing basic vars
			!		enddo inunitloop !nitloop: do while(.true.)   4544
 
 
			! endif
			!tähän voi laittaa unitin jälkeisiä tarkastuksia
			!goto700=.false.
 
 
			!write(17,*)'aftendinunitloop',kier,iunit
		enddo unitloop !tloop: do p_unit=1,p_nunits   5416
 
		if(p_fpresent.and.p_feasible)then
			if(.not.p_factnow)then
				nofact=nofact+1
				if(nofact.gt.iterxkf)then
					write(6,*)'factories will now be checked,round,p_pivot,obj', p_kier,p_pivot,p_objf
					p_factnow=.true.
					nofact=0
				endif !if(nofact.gt.iterxkf)   5757
			else !if(.not.p_factnow)then
				!	write(6,*)'round,p_factnow put to false',p_kier
				!		p_factnow=.false.
			endif !if(.not.p_factnow)   5755
		endif !if(p_fpresent.and.p_feasible)   5754
 
		!write(17,*)'aftunitloop',kier,p_unit
	enddo kierloop !rloop: do p_kier=0,p_maxrounds   5375
	write(6,*)'iteration stops, maximum number of rounds through units ',p_maxrounds,' reached'
	write(6,*)'note you can increase this using maxrounds-> option'
 
900 continue
	call tulostele()
 
	if(p_nureport.ne.6)call j_printname('*report written into:',p_ivreport,' which remains open ')
 
	call repo(p_nureport)
	if(p_echo.and.p_nureport.ne.6)call repo(6)
 
	if(p_ispullout)call pullout(LWSLL1)
 
	call weightstot()
 
	call closeflet()
	! if(allocated(p_ls)) deallocate(p_ls)   !varx;
	! if(allocated(p_lsi)) deallocate(p_lsi)   !varx;
	! if(allocated(p_a)) deallocate(p_a)   !varx;
	! !	if(allocated(p_objr0)) deallocate(p_objr0)   !varx;
	! if(allocated(p_objr2)) deallocate(p_objr2)   !varx;
	! if(allocated(p_xmi)) deallocate(p_xmi)   !varx;
	! if(allocated(p_xma)) deallocate(p_xma)   !varx;
	! if(allocated(p_x)) deallocate(p_x)   !varx;
	! if(allocated(p_b)) deallocate(p_b)   !varx;
	nullify(p_objr)
 
930 	continue

!	if(p_xpresent)then
		!		if(allocated(p_ivdomains)) deallocate(p_ivdomains)
	! if(allocated(p_ibatemp)) deallocate(p_ibatemp)
	! if(allocated(p_nrowxtemp)) deallocate(p_nrowxtemp)
	! !	if(allocated(p_ixprow)) deallocate(p_ixprow)
	! if(allocated(p_xcoef)) deallocate(p_xcoef)
	! if(allocated(p_irowvarx)) deallocate(p_irowvarx)
	! if(allocated(p_nrowx2)) deallocate(p_nrowx2)
	! !	if(allocated(p_nunitsrow)) deallocate(p_nunitsrow)
	! if(allocated(p_xps)) deallocate(p_xps)  !nsch,keys,ibaunit
	! if(allocated(p_xsmin)) deallocate(p_xsmin)  !nsch,keys,ibaunit
	! if(allocated(p_xsmax)) deallocate(p_xsmax)  !nsch,keys,ibaunit
	! if(allocated(p_xmin)) deallocate(p_xmin)  !nsch,keys,ibaunit
	! if(allocated(p_xmax)) deallocate(p_xmax)  !nsch,keys,ibaunit
	! if(allocated(p_rejects))deallocate(p_rejects)
	! if(allocated(p_testxps)) deallocate(p_testxps)
	! if(allocated(p_test)) deallocate(p_test)
	! if(allocated(p_isch)) deallocate(p_isch)
	! if(allocated(p_next)) deallocate(p_next)
	! if(allocated(p_iprev)) deallocate(p_iprev)
	! if(allocated(p_solx)) deallocate(p_solx)
	! if(allocated(p_lxi)) deallocate(p_lxi)  !lunit,ld ,vx
	! !	end if !if(p_xpresent)   3597
	! ! if(sparse)then
	! ! if(allocated(p_lavecsp)) deallocate(p_lavecsp)
	! if(allocated(p_acol)) deallocate(p_acol)
	! if(allocated(p_acolapu)) deallocate(p_acolapu)
	! if(allocated(p_icolapu)) deallocate(p_icolapu)

	! end if !if(sparse)  15728



990  continue
	if(j_err) then
		p_buf='jlp error exit'
	else !if(j_err) then
		p_buf='jlp normal exit'
	endif !if(j_err)   5833
	write(p_nureport,'(a)')p_buf(1:79)
	call  cpu_time(p_time0)
	write(p_nureport,*)'total cpu-time in jlp() ',p_time0-p_time00
	write(p_nureport,*)' '
	! write(6,*)'hep'
	close(17)
	! write(6,*)'p_ivpivots',p_ivpivots,'io ',io
 
 
	j_v(p_ivpivots)=p_pivot
 
 
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
end subroutine jlp   ! jlp(iob
 
subroutine weightstot()
	ivw=j_defmatrix(j_ivout,'%weights',p_lopp,1,j_matreg)
	do iuni=1,p_nunits
		j_o(ivw)%d(p_ibaunit(iuni)+p_keys(iuni))=j_1
	enddo !iuni=1,p_nunits   5880
 
	do j=1,p_lx0
		iuni=p_lunit(p_lx(j))
		is=p_isch(p_lx(j))
		j_dapu=p_x(p_nrowz+p_lx(j))
		j_o(ivw)%d(p_ibaunit(iuni)+is)=j_dapu
		j_o(ivw)%d(p_ibaunit(iuni)+p_keys(iuni))=j_o(ivw)%d(p_ibaunit(iuni)+p_keys(iuni))-j_dapu
 
	enddo !j=1,p_lx0   5884
 
end subroutine
! subroutine initfl()
! use fletdmod !nopre!   !file jlpd.f90
! use fletdmod2 !nopre!
! !	use jlpdmod !nopre!
 
! use fletcherdmod  !nopre!
! use lastmod !nopre!
! integer ::nup
! logical indomain,indomain2
! common/noutc/nout
! common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj  !Fletcher parameters
! !dense common/factorc/m0,m1,mm0,mm,mp,mq
! !sparse common/factorc/m1,m2,mp,mq,lastr,p_irow
 
! !this means that when in dense mpjj and mqjj are changed
! ! then in sparse mpjj is the same as
 
! common/ipivotc/ipivot99  !used only in fltcherd.for to print pivot at malfunctio
! common/refactorc/nup,nfreq
! write(6,*)'initfl'
! !	call initflet(p_nrow,p_ncol,p_a,la,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac)
 
 
 
! call initflet(p_nrow,p_ncol,p_a,p_nrow,p_lavec,p_ls,lu1,ll1,p_ifail,nrefac)
 
! if(p_p)write(p_n16,*)'p_ilasaftinitfl ',p_ls
! !if(allocated(p_lower))deallocate(p_lower)  ! is lower bound active
! !allocate( p_lower(1:p_nrow))
! ! lr links to I-part (slack/surplus) part of matrix, lri =inverse list
 
! ! lz links to z-part of the A matrix lzi inverse list
 
! !if(allocated(p_vc))deallocate(p_vc)
! !	call j_defmatrix(0,'po%obj'//ch,1,p_nrow,j_matreg,ivob)
 
! allocate(p_vc(1:p_nrow) )  ! eq. 6.42 in JLP manual  (price vector)
 
! if(allocated(p_x))deallocate(p_x,p_b)  !j_x2 added 201608
! allocate( p_x(1:p_mxnm) ,p_b(1:p_mxn) ) ! x:n dimensio +
! p_x=j_0
! p_b=j_0
! !write(6,*)'<44 alloc p_x'
! ! Fletcher routines solve a system  B.x=b
! if(p_p)write(p_n16,*)'X:n dimensio,b:n dimensio ',p_mxnm,p_mxn
 
 
 
! end subroutine
!subroutine getnvar()
!	write(6,*)'getnvar'
 
 
 
!end subroutine
subroutine tulostele3()
 
	!				if((p_unitprev.eq.p_unitv.and.p_kier.gt.p_kierv).or.(p_unitv==-1.and.p_kier.gt.p_kierv))then
	!!!!! there was no change since last visit to the same unit , optimum found,return !<B331>
	!maxiter
	!	write(16,*)'tasny,goto700',goto700
	!call tulostele()
	!	if(.not.p_goto700)then
	if(ipivot9.ne.p_pivot)then  !vois katsoa muutkin muutokset
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
				write(6,'(i5,i8,g19.12,f8.4,f7.2,5i5,i3,a,f5.2,i5,a,f5.2,i6)')&
					p_kier,p_pivot,p_coefmax*p_objf,pros,p_as,p_lr0,p_lz0,p_lx0,&
					p_lf0,p_nnf,iminc,':',secd,imint,':',sect,p_nimp2
			else
				write(6,'(i5,i8,g19.12,7x,f8.2,5i5,i3,a,f5.2,i5,a,f5.2)')&
					p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0, &
					p_lf0,p_nnf,iminc,':',secd,imint,':',sect
 
			endif !if(p_objfprev.ne.p_small)   5978
			! else !if(j_xpresent2)then
			! write(6,*)'Feasible, objective:', &
			! p_coefmax*p_objf,'basic residuals ', p_lr0, &
			! ' basic z-vars ',p_lz0, ' cpu ',(time-p_time00)/60.
			! endif !if(p_xpresent2)   5633
			!time0=time
		endif !if(p_feasible)   5965
	else !if(ipivot9.ne.p_pivot)then
		!		if(p_xpresent2)then
		!write(6,*)'tassa2'
		write(6,'(i5,i8,g19.12,7x,f8.2,5i5,i3,a,f5.2,i5,a,f5.2)')&
			p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0, &
			p_lf0,p_nnf,iminc,':',secd,imint,':',sect
		!	else !if(j_xpresent2)then
		!	write(6,*)'Optimal, objective:', &
		!		p_coefmax*p_objf,'basic residuals ', p_lr0, ' basic z-vars ',p_lz0, ' cpu ',(time-p_time00)/60.
		!	endif !if(p_xpresent2)   4715
	endif !if(ipivot9.ne.p_pivot)   5957
	!write(6,*)'<654exitkier'
 
 
	!			exit kierloop
	!				endif !if(.not.p_goto700)   4675
	!			endif !if((p_unitprev.eq.p_unitv.and.p_kier.gt.p_kierv).or.(p_i   4670
 
 
 
 
end subroutine tulostele3
 
subroutine commonopt(iob,io)
	integer, dimension(:),allocatable,target::termvar,vars,rowofterm !,nterminrow
	double precision, dimension(:),allocatable,target::coef
	write(6,*)'commonopt'
 
	!write(6,*)'p_ivproblem',p_ivproblem
 
 
 
	call j_getoption_index(iob,io,j_mpullout,-1,1,0,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_ispullout=noptarg.ge.0
 
	if(noptarg.ge.1)then
 
		if(j_v(j_optarg0(1)).eq.0)p_ispullout=.false.
	endif !if(noptarg.ge.1)   6031
 
 
	!	write(6,*)'ispullout',p_ispullout,p_pullout1,p_pullout2
 
	! if(j_linkoption(iob,io,j_mtest).ge.0)then
	! if(j_o(iob)%i(j_linkoption(iob,io,j_mtest)).eq.0)then   ! test->
	! p_testl=.true.
	! else if( j_v(j_o(iob)%i(j_linkoption(iob,io,j_mtest+1))).gt.0)then !if(j_o(iob)%i(j_linkoption(iob,io,j_mtest)).eq.0)then
	! p_testl=.true.
	! else !if(j_o(iob)%i(j_linkoption(iob,io,j_mtest)).eq.0)then
	! p_testl=.false.   ! test->0
	! end if !if(j_o(iob)%i(j_linkoption(iob,io,j_mtest)).eq.0)   5192
	! else !if(j_linkoption(iob,io,j_mtest).gt.0)then
	! p_testl=.false.
	! end if !if(j_linkoption(iob,io,j_mtest).ge.0)   5191
	j_v(j_ivdebug)=-1.
	idebu=j_igetopt(iob,io,j_mdebug)
	if(idebu.lt.0)then
		p_idebug=-1
	elseif(idebu.eq.0)then
		p_idebug=0
	else
 
		p_idebug=j_v(j_ivdebug)
	endif !if(idebu.lt.0)   6052
	! endif !if(nad.gt.0)   5072
	! call j_defmatrix(j_ivout,'%coef',p_nterm,1,j_matreg,iv)
	if(p_p)write(6,*)'tasapopar ',p_ivpopar
	if(p_ivpopar.eq.0)then
		p_ivpopar=j_deflist(j_ivout,'%jlppar',list0=15)
		j_o(p_ivpopar)%i2(1:15)= (/ p_ivfeasible, p_ivunbounded, p_ivobjective, p_ivobjective0, &
			p_ivoptimal,p_ivpivots,p_ivtmax,p_ivcolold,p_ivcolnew,p_ivpivotcase,&
			p_ivlr0,p_ivlz0,p_ivlx0,p_ivlf0,p_ivrefac/)
 
		p_ivunbounded=j_getobject(j_ivout,'%unbounded',j_ipreal)
 
		p_ivobjective0=j_getobject(j_ivout,'%objective0',j_ipreal)
		p_ivoptimal=j_getobject(j_ivout,'%optimal',j_ipreal)
		if(p_nrow.gt.0)then
			p_ivpivots=j_getobject(j_ivout,'%pivots',j_ipreal)
			p_ivstartedjlp=j_getobject(j_ivout,'%started_jlp',j_ipreal)
			p_ivtmax=j_getobject(j_ivout,'%tmax',j_ipreal)
			p_ivcolold=j_getobject(j_ivout,'%colold',j_ipreal)
			p_ivcolnew=j_getobject(j_ivout,'%colnew',j_ipreal)
			p_ivpivotcase=j_getobject(j_ivout,'%pivotcase',j_ipreal)
			p_ivpivotcases=j_deflist(j_ivout,'%pivotcases',list0=5,ilist=.true.)
			p_ivroute67=j_getobject(j_ivout,'%route67',j_ipreal)
			p_ivlr0=j_getobject(j_ivout,'%lr0',j_ipreal)
			p_ivlz0=j_getobject(j_ivout,'%lz0',j_ipreal)
			p_ivlx0=j_getobject(j_ivout,'%lx0',j_ipreal)
			p_ivlf0=j_getobject(j_ivout,'%lf0',j_ipreal)
			p_ivrefac=j_getobject(j_ivout,'%refac',j_ipreal)
			p_ivvaluek=j_getobject(j_ivout,'%valuek',j_ipreal)
			p_ivvalueopt=j_getobject(j_ivout,'%valueopt',j_ipreal)
		endif !if(p_nrow.gt.0)   6073
		! else
		! j_v(j_o(p_ivpopar)%i2)=j0
	endif !if(p_ivpopar.eq.0)   6063
	if(p_nrow.gt.0)j_v(p_ivtmax)=j_ninf
	p_refac=0 !number of refactorizations
	!	printlf=.false.
	ivprint7=j_igetopt(iob,io,j_mprint)
 
	!p_p=.true.
	p_nrecover=0
	!isslow=.false.
	!cycle2
 
	p_stoplink=j_codelink(iob,io,j_mstop)
	p_isstop=p_stoplink.ne.0
 
	p_fastplink=j_codelink(iob,io,j_mfastp)
	p_isfastp=p_fastplink.ne.0
 
	p_again=.false.
	p_nagain=0
	!	p_n16=17
	!		simobug=.true.
	p_ilrmax=1  !to avoi array bounds exeeded situation in point !assshshdhs
	nout=0
	call j_getoption_index(iob,io,j_mslow,-1,100,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	!	call j_getoption(iob,io,j_mslow,-1,1,j_ipreal,.true.,nslow,slowopt)
	if(noptarg.ge.0)write(6,*)'slow-> is obsolete, use stop->'
	!write(6,*)'<56',iob
 
	call j_getoption_index(iob,io,j_mecho,-1,1,j_ipreal,.false.,noptarg,j_optarg0)
	if(j_err)return
	!	call j_getoption(iob,j_mslow,-1,1,j_ipreal,.true.,nslow,slowopt)
	p_echo=.true.
	if(noptarg.gt.0)then
		if(j_v(j_optarg0(1)).le.j_0)p_echo=.false.
	endif !if(noptarg.gt.0)   6125
 
	p_feasible=.false.
	p_icolold=-1
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
		endif !if(p_nureport.le.0)   6138
 
	else !if(j_nargopt(iob,io,j_mreport).eq.1)then
		p_nureport=6
	endif !if(noptarg.eq.1)   6133
	!	write(6,*)'tassanyt'
	!p_p9=p_iprint.eq.9
	!	sparse=.false.
	! sparse-option disabled
	!if(j_linkoption(iob,io,j_msparse).gt.0) sparse=.true.
	! if(j_linkoption(iob,io,j_msparse).ge.0) &
	! write(6,*) '*jlp: sparse->  not available in current version of J'
 
 
	!debug
	!p_p=.false.
	p_p2=.false.
 
 
	!data *******************************
	!	j_ivout =j_o(iob)%i(io+2+j_o(iob)%i(io+1))
	!	if(j_otype(j_ivout).ne.j_ipreal)call j_del(j_ivout)
 
	if(j_linkoption(iob,io,j_mrefac).gt.0)then
		p_nrefac=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mrefac)+1))
	else !if(j_linkoption(iob,io,j_mrefac).gt.0)then
		p_nrefac=1000  !
	endif !if(j_linkoption(iob,io,j_mrefac).gt.0)   6168
	!	write(6,*)'refac=',p_nrefac
 
	! tole->
	call j_getoption(iob,io,j_mtole,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.gt.0)then
 
		p_tolep=j_v(j_optarg0(1))
	else !if(j_linkoption(iob,io,j_mtole).gt.0)then
		p_tolep=p_one
	endif !if(noptarg.gt.0)   6178
	!write(6,*)'tole=',j_tolep
	if(p_p)write(6,*)'tassanyt&& nrow ',p_nrow
 
	! nrow tell the total number of rows when domain sets are expanded
7777 format(a16,i5)
	!if(p_xpresent)write(6,7777)'domains',max(p_ndom,1)

	!	endif !if(.not.p_zmatrix)  13009

	p_ivtole=j_defmatrix(j_ivout,'%tole',1,p_nrowtot,j_matreg)
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
 
 
 
	p_ivubou=j_deflist(j_ivout,'%ubou',list0=p_nrow,ilist=.true.) !p-variables
	p_ivlbou=j_deflist(j_ivout,'%lbou',list0=p_nrow,ilist=.true.) !p-variables
	if(p_p)write(6,*)'tassan5i rhs2,p_nrow ',p_nrow,p_ivlbou,p_ivubou
	if(p_p)write(6,*)'ubou',p_ubou,p_rhs2
 
	p_ubou=p_rhs2.lt.j_inf   ! is there upper bound
	if(p_p)write(6,*)'ubou',p_ubou
	if(p_p)write(6,*)'lbou',p_ubou
	p_lbou=p_rhs.gt.j_ninf
	if(p_p)write(6,*)'lbou',p_lbou
	if(p_p)write(6,*)'***rhs2',p_rhs2
	if(p_p)write(6,*)'***ubou',p_ubou
 
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

	p_ivrhscur=j_defmatrix(j_ivout,'%rhscur',p_nrow,1,j_matreg)
	p_rhscur=>j_o(p_ivrhscur)%d(1:p_nrow)
	!	if(allocated(p_rhscur))deallocate(p_rhscur);allocate( p_rhscur(1:p_nrow))
 
	! rhsw: the working rhs where the sums over key schedules are subtracted
	!	if(p_xpresent)then !this is allocated even if it will be equalt rhscur when there is no data
	!if(allocated(p_rhsw))deallocate(p_rhsw);allocate( p_rhsw(1:p_nrow))
	!endif !if(p_xpresent)   4396
 
 
 
 
end subroutine
 
 
subroutine getnvarz()
	if(j_linkoption(iob,io,j_mdata).ge.0)then
		write(6,*)'jlpz() cannot use data->, problems with schedules are solved with jlp()'
		j_err=.true.;return
	endif !if(j_linkoption(iob,io,j_mdata).ge.0)   6263
 
 
end subroutine
subroutine startlist0()
	! initilize lists defined in startlist(
	if(p_nz.gt.0)then
		do i=1,p_nz
			p_lz(i)=i		! list allz-cols in order
			p_lzi(i)=i	! nrow not included
		enddo !i=1,p_nz   6273
		p_lz0=0
		!		p_redcost=j_0
	endif !if(p_nz.gt.0)   6272
	p_x=j_0
	p_b=j_0
	p_vc=j_0
	p_a=j_0
	p_abas(1)=p_nrow
	do j=2,p_mxn
		p_abas(j)=p_abas(j-1)+p_nrow
	enddo !j=2,p_mxn   6285
 
	! if(allocated(p_a))deallocate(p_a)
	! allocate(p_a(1:p_nrow,0:p_mxn) ) ;p_a=j_0
 
 
	p_xmi=p_zero ;p_xma=p_zero
 
	do j=1,p_nm !nm=ncol+nrow    ! number of columns in (I A)
		p_lsi(j)=j    !intially ls is in order, residuasl are in the basis
	enddo !j=1,p_nm   6295
	!	write(6,*)p_nm,'lsi',p_lsi
	do i=1,p_nrow
		p_lr(i)=i
		p_lri(i)=i	!inverse list
	enddo !i=1,p_nrow   6299
	p_lr0=p_nrow
	if(p_xpresent)then
		do i=1,p_mxd
			p_lx(i)=i          !lists columns of D
			p_lxi(i)=i        !inverse list
		enddo !i=1,p_mxd   6305
	endif !if(p_xpresent)   6304
	if (p_fpresent) then
		do i=p_mxd+1,2*p_mxd
			p_lf(i-p_mxd)=i          !lists columns of D
			p_lfi(i-p_mxd)=i       !inverse list
		enddo !i=p_mxd+1,2*p_mxd   6311
		p_lf0=p_mxd
		!jatketaanko seuraavasta tehdasmjasta tarkastelu/ienter, ixkenter alustukset
 
		ixkenter = 0
		p_unitrans = 0	! yksikkö, jolle viimeksi laskettu tehdasmja muunnokset
	endif !if (p_fpresent)   6310
	p_ienter= 0
 
end subroutine startlist0
 
subroutine startlist()
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
 
 
	write(6,*)'startlist',p_mxnm,p_nm
 
 
	p_ivls=j_deflist(j_ivout,'%ls',list0=p_mxnm,ilist=.true.)
	p_ls=>j_o(p_ivls)%i2(1:p_mxnm)
 
	p_ivlsi=j_deflist(j_ivout,'%lsi',list0=p_mxnm,ilist=.true.)
	p_lsi=>j_o(p_ivlsi)%i2(1:p_mxnm)
 
	p_ivlr=j_deflist(j_ivout,'%lr',list0=p_mxnm,ilist=.true.)
	p_lr=>j_o(p_ivlr)%i2(1:p_mxnm)
 
	p_ivlri=j_deflist(j_ivout,'%lri',list0=p_mxnm,ilist=.true.)
	p_lri=>j_o(p_ivlri)%i2(1:p_mxnm)
 
	p_ivobjr0=j_defmatrix(j_ivout,'%objr0',1,p_mxnm,j_matreg)
	p_objr0=>j_o(p_ivobjr0)%d(1:p_mxnm)
 
	p_ivobjr2=j_defmatrix(j_ivout,'%objr2',1,p_mxnm,j_matreg)
	p_objr2=>j_o(p_ivobjr2)%d(1:p_mxnm)
 
	if(p_nz.gt.0)then
		p_ivlz=j_deflist(j_ivout,'%lz',list0=p_nz,ilist=.true.)
		p_lz=>j_o(p_ivlz)%i2(1:p_nz)
		p_ivlzi=j_deflist(j_ivout,'%lzi',list0=p_nz,ilist=.true.)
		p_lzi=>j_o(p_ivlzi)%i2(1:p_nz)
		p_ivredcost=j_defmatrix(j_ivout,'%redcost',p_nz,1,j_matreg)
		p_redcost=>j_o(p_ivredcost)%d(1:p_nz)
		!	p_redcost=j_0
		!if(allocated(p_lz))deallocate(p_lz,p_lzi,p_redcost)
		! redcost = reduced costs of z-variables
 
		!allocate( p_lz(1:p_nz),p_lzi(1:p_nz),p_redcost(1:p_nz))  !miksei testata onko nz.eq.0  ??
	endif !if(p_nz.gt.0)   6377
 
 
	p_ivvc=j_defmatrix(j_ivout,'%vc',p_nrow,1,j_matreg)
	p_vc=>j_o(p_ivvc)%d(1:p_nrow)
 
	! if(allocated(p_vc))deallocate(p_vc)
	! allocate(p_vc(1:p_nrow) )  ! eq. 6.42 in JLP manual  (price vector
	p_ivx=j_defmatrix(j_ivout,'%x',p_mxnm,1,j_matreg)
	p_x=>j_o(p_ivx)%d(1:p_mxnm)
 
	p_ivb=j_defmatrix(j_ivout,'%b',p_mxn,1,j_matreg)
	p_b=>j_o(p_ivb)%d(1:p_mxn)
 
 
	! if(allocated(p_objr0))deallocate(p_objr0)
	! if(allocated(p_objr2))deallocate(p_objr2)
	! allocate( p_objr0(1:p_mxnm));allocate( p_objr2(1:p_mxnm));;p_objr0=j_0;p_objr2=j_0
	if(allocated(p_xmi))deallocate(p_xmi,p_xma);if(allocated(p_xma))deallocate(p_xma)
	allocate( p_xmi(1:p_mxnm),p_xma(1:p_mxnm))
 
	p_iva=j_defmatrix(j_ivout,'%a',p_mxn+1,p_nrow,j_matreg)
	p_a=>j_o(p_iva)%d(1:p_nrow*(p_mxn+1))
 
	if(allocated(p_abas))deallocate(p_abas)
	allocate(p_abas(1:p_mxn))
 
	!	p_a=p_zero
 
	!	call j_defmatrix(0,'po%lws'//ch,1,p_mxlws,j_matreg,ivls)
	!	j_o(ivls)%d(1:p_mxlws)=LWSLL1(1:p_mxlws)
 
 
 
 
 
	if(p_xpresent)then
		p_ivlx=j_deflist(j_ivout,'%lx',list0=p_mxd,ilist=.true.)
		p_lx=>j_o(p_ivlx)%i2(1:p_mxd)
 
		p_ivlxi=j_deflist(j_ivout,'%lxi',list0=p_mxd,ilist=.true.)
		p_lxi=>j_o(p_ivlxi)%i2(1:p_mxd)
 
 
	endif !if(p_xpresent)   6425
 
	if(p_fpresent)then
		p_ivlf=j_deflist(j_ivout,'%lf',list0=p_mxd,ilist=.true.)
		p_lf=>j_o(p_ivlf)%i2(1:p_mxd)
 
		p_ivlfi=j_deflist(j_ivout,'%lfi',list0=p_mxd,ilist=.true.)
		p_lfi=>j_o(p_ivlfi)%i2(1:p_mxd)
 
 
	endif !if(p_fpresent)   6435
	!	write(6,*)'startlist0 from startlist'
	call startlist0()
 
 
end subroutine startlist
 
! subroutine initjlp(iob,io)
! !p_nonlintrans
! !p_nonlin
! ! p_fpresent
! ! p_intapp
! ! p_justkey
 
! integer,intent(in)::iob,io
! write(6,*)'initjlp'
! ! this subroutine basically interprets the function call jlp() line
! ! Also xdata and cdata objects are accessed but not used
! ! because options are accessed iob and io myst be as arguments
 
! call j_getoption_index(iob,io,j_mnonlintrans,-1,1,j_iptrans,.true.,noptarg,j_optarg0)
! if(j_err)return
 
! if(noptarg.gt.0)then
! p_nonlintrans=j_optarg0(1)
! p_nonlin=.true.
! endif !if(noptarg.gt.0)   6494
 
! p_fpresent=.false.
! p_isxk0 = .false.
! p_restarted = .false.
 
! p_mxd=0
! p_intapp=.false.
 
! !j_ivvarsx=0
! ! p_nz=0 p_nz initilized later JL 112018
! !write(6,*)'<55',iob
! p_justkey=.false.
! ! report->
! !	write(6,*)'hep8',p_zmatrix
! !	write(6,*)'tassaaa',p_zmatrix
 
! !if(p_p)write(6,*)'<88 ',p_nterminrow(0)
! !nrow3=3*p_nrowtot
! p_nz=p_npvar ! added 112018 JL
! p_niter=0
! !p_needcinx=.false.
 
! end subroutine initjlp
 
subroutine initjlp2(iob,io)
	write(6,*)'initjlp2'
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
	endif !if(nargopt.gt.0)   6516
	!	write(6,*)'djjdp_npvar ',p_npvar
 
	call j_getoption_index(iob,io,j_mz,-1,0,j_ipmatrix,.false.,noptarg,j_optarg0)
	if(j_err)return
	p_zopt=noptarg.ge.0
 
	!write(6,*'<556> p_nrow',p_nrow
	!if(allocated(p_ix))deallocate(p_ix)
	p_ivix=j_deflist(j_ivout,'%ix',list0=p_nrowtot,ilist=.true.)
	p_ix(0:)=>j_o(p_ivix)%i2(1:p_nrowtot)
 
	!write(6,*)'pix ',p_ix
	!	allocate(p_ix(0:p_nrow));p_ix=0
 
	call j_getoption(iob,io,j_mnotareavars,-1,999999,j_ipreal,.true.,p_nnotareavars,p_notareavars)
	if(j_err)return
	!	notareav=j_nargopt(iob,io,j_mnotareavars) !number of p_notareavars-> -variables
	if(p_nnotareavars.gt.0)then
		!	linknotarea=j_linkoption(iob,io,j_mnotareavars)
		if(p_ivarea.lt.0)then
			write(6,*)'**cannot have notareavars-> without area->'
			j_err=.true.
			return
		endif !if(p_ivarea.lt.0)   6541
	endif !if(p_nnotareavars.gt.0)   6539
 
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
	call j_getoption_index(iob,io,j_minteger,-1,0,0,.false.,noptarg,j_optarg0)
	if(j_err)return
	!	call j_getoption(iob,j_mslow,-1,1,j_ipreal,.true.,nslow,slowopt)
	p_intapp=noptarg.ge.0
	! fast-optiot
	p_fastusesame = 10
	i = j_igetopt(iob,io,j_mfastrounds)
	if(i > 0) p_fastusesame = j_v(i)
 
 
	p_fastpros = 85
	j_v(j_ivfastp)=p_fastpros
 
	iterxkf = 3
	i = j_igetopt(iob,io,j_mfinterval)
	if(i>0) iterxkf = j_v(i)
 
	p_fastpros2=p_fastpros/100.
	p_fast=p_fastusesame.ne.1
	!write(6,*)'<2345kukuu'
	p_nkeys=0
 
	p_ntote=0
	p_ilres=0
	p_nresw=0
	p_npivotw=0
	p_nkeyfactw=0
	i=j_igetopt(iob,io,j_mwarm)
	p_warm=.false.
	if(i.gt.0)then
		p_warm=j_v(i).ne.0.
	elseif(i.eq.0)then !if(i.gt.0)then
		p_warm=.true.
	endif !if(i.gt.0)   6596
	!keys key schedules for each unit
	p_warm=.false.
 
	p_warmf=p_warm
 
 
end subroutine initjlp2
 
subroutine initxdatawx()
	write(6,*)'initxdataw'  !,'ixxx',p_ix
	! inits something of x-data used also with factories and without ordinary x-variables
	!initilizes also some fact issues
	! uses p_ninrowx (0p_nrow)
	! p_ivxcoef
	! p_xcoef  (1:p_nxval)
	! p_ivxtermvar
	! p_xtermvar x-term variables
	! p_ivrowcurx  !current rowxs if there are now domaisn these are not updated
	! p_rowcurx
	! p_ibatemp  (1:p_nrowtot)
	! p_nrowxtemp (1:p_nrowtot)
	! p_ntemp0
	! p_nrowxtemp (1:p_ntemp0) number of x- for each temp
	! p_ivirowvarx= (j_ivout,'%irowvarx',list0=p_nxval)
	!	p_irowvarx=>
	!	p_ivnrowx2=j_deflist(j_ivout,'%nrowx2',list0=p_nrowcurx,ilist=.true.)
	!	p_nrowx2=>j_o(p_ivnrowx2)%i2(1:p_nrowcurx)
	!	p_irowvarx=pack(p_termvar,p_istermx)
	! p_nrowx2=pack(p_ninrowx,p_ninrowx.gt.0) !numbers of x-variables in each
	! p_ivxmat=j_defmatrix(j_ivout,'%xmat',p_lopp,p_ntemp0,j_matreg,single=.true.)
	! p_xmat=>j_o(p_ivxmat)%r(1:p_ntemp0*p_lopp)
	!	allocate(p_xmat(1:p_ntemp0*p_lopp))
	! p_ivvxpack=  (j_ivout,'%vxpack',p_ntemp0,1,j_matreg,single=.true.)
	! p_vxpack=>
	! p_ivixpack=j_deflist(j_ivout,'%ixpack',list0=p_ntemp0,ilist=.true.)
	! p_ixpack=>j_o(p_ivixpack)%i2(1:p_ntemp0)
	! p_ivvarx=j_deflist(j_ivout,'%varx',list0=p_nxvar)
	! p_varx=>j_o(p_ivvarx)%i2(1:p_nxvar)
	! p_varx= pack(p_vars(1:p_npvar),p_isvarx)   ! c-variables are here included as x -variables
	! p_ivibaunit= j_deflist(j_ivout,'%ibaunit',list0=p_nunits+1,ilist=.true.)
	! p_ibaunit=>j_o(p_ivibaunit)%i2(1:p_nunits+1)
 
 
 
 
 
	p_ivxcoef=j_defmatrix(j_ivout,'%xcoef',p_nxval,1,j_matreg)
	p_xcoef=>j_o(p_ivxcoef)%d(1:p_nxval)
	p_ivxtermvar=j_deflist(j_ivout,'%xtermvar',list0=p_nxval)
	p_xtermvar=>j_o(p_ivxtermvar)%i2(1:p_nxval)
 
	nxval=0
	do i=1,p_nterm
		if(p_istermx(i))then
			nxval=nxval+1
			p_xcoef(nxval)=p_coef(i)
			p_xtermvar(nxval)=p_termvar(i)
		endif !if(p_istermx(i))   6654
	enddo !i=1,p_nterm   6653
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
		endif !if(p_istermx(ival).and.iro.ne.irov)   6672
		irov=iro
	enddo !ival=1,p_nterm   6669
 
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
	!is(p_p8)write(6,*)'<termvar',p_termvar(1:p_nterm)
	!write(6,*)'coeftas',p_xcoef
	!write(6,*)'nxinrow',p_ninrowx(iro)
	if(allocated(p_ibatemp))deallocate(p_ibatemp)
	if(allocated(p_nrowxtemp))deallocate(p_nrowxtemp)
 
	allocate(p_ibatemp(1:p_nrowtot),p_nrowxtemp(1:p_nrowtot))
	p_ibatemp=0;p_nrowxtemp=0
	iba=0
 
rowloop:	do iro=0,p_nrow
	!	write(6,*)'iro,p_ninrowx(iro)',iro,p_ninrowx(iro)
		if(p_ninrowx(iro).gt.0)then
			!		p_nrowcurx=p_nrowcurx+1
			!		p_rowcurx(p_nrowcurx)=iro
			!check previous rows
			iba2=0
	row2:		do iro2=0,iro-1
				if(p_ninrowx(iro).eq.p_ninrowx(iro2))then
 
			outer:	do jj=1,p_ninrowx(iro)
						do jj2=1,p_ninrowx(iro)
							! check if coefficient or variable is different
							if(p_xtermvar(iba+jj).eq.p_xtermvar(iba2+jj2))then
								if(p_xcoef(iba+jj).eq.p_xcoef(iba2+jj2))then
									cycle outer
								else
									cycle row2
								endif !if(p_xcoef(iba+jj).eq.p_xcoef(iba2+jj2))   6720
							else
								cycle row2
							endif !if(p_xtermvar(iba+jj).eq.p_xtermvar(iba2+jj2))   6719
 
						enddo !jj2=1,p_ninrowx(iro)   6717
 
					enddo outer !er:	do jj=1,p_ninrowx(iro)   6716
					!!! now all variables and coefficients are equal
					p_ix(iro)=p_ix(iro2)
					cycle rowloop !write(6,*)'samma x',iro,p_irow0,'ix',p_itemp0
				endif !if(p_ninrowx(iro).eq.p_ninrowx(iro2))   6714
				iba2=iba2+p_ninrowx(iro2)
			enddo row2 !2:		do iro2=0,iro-1   6713
			p_ntemp0=p_ntemp0+1
			!		p_ixprow(iro)=p_itemp0
			p_ix(iro)=p_ntemp0 !tells for each row what is teporary
			!	p_rowcurx(nrowx)=iro
			!number of rows with x
			p_ibatemp(p_ntemp0)=iba
			p_nrowxtemp(p_ntemp0)=p_ninrowx(iro)
			!	write(6,*)' iro ',iro,' p_ntemp0 ',p_ntemp0
 
		endif !if(p_ninrowx(iro).gt.0)   6708
 
		iba=iba+p_ninrowx(iro)
	enddo rowloop !loop:	do iro=0,p_nrow   6706
 
	!rowcurx is row where the x -variable is and ix is the
	! nrowcurx is the number of rows having x-variables
	! do jj=1,p_nrowcurx
	! j=p_rowcurx(jj)
	! !j_value=j_value+j_vx(j)*j_xmat(j_ix(j),is) !,is) !v(ix(j))
	! p_value=p_value+p_vx(j)*p_xmat(ibxmatx+p_ix(j)) !,is) !v(ix(j))
	! enddo !jj=1,p_nrowcurx   1964
 
 
	p_row0=1
	if(p_rowcurx(1).eq.0)p_row0=2
 
	if(allocated(p_varx1))deallocate(p_varx1,p_varx2)  !varx1 and varx2 used to chech duplicated sched
	allocate(p_varx1(1:p_nxvar),p_varx2(1:p_nxvar)) !used
 
 
 
 
	p_ivirowvarx=j_deflist(j_ivout,'%irowvarx',list0=p_nxval)
	p_irowvarx=>j_o(p_ivirowvarx)%i2(1:p_nxval)
	p_ivnrowx2=j_deflist(j_ivout,'%nrowx2',list0=p_nrowcurx,ilist=.true.)
	p_nrowx2=>j_o(p_ivnrowx2)%i2(1:p_nrowcurx)
	!take coefficients of x, and the correponding variables
 
 
	!	write(6,*)'xcoefaft',p_xcoef
	!is(p_p8)write(6,*)'<999pack'  !,p_termvar,'*',p_istermx
	p_irowvarx=pack(p_termvar,p_istermx)
	!take
	!is(p_p8)write(6,*)'<4779pack'  !,p_nrowx,'*',p_nrowx.gt.0
	p_nrowx2=pack(p_ninrowx,p_ninrowx.gt.0) !numbers of x-variables in each
 
	p_ivxmat=j_defmatrix(j_ivout,'%xmat',p_lopp,p_ntemp0,j_matreg,single=.true.)
	p_xmat=>j_o(p_ivxmat)%r(1:p_ntemp0*p_lopp)
	write(6,*)'allocate xmat:',p_lopp,' rows ',p_ntemp0,' columns ',p_ntemp0*p_lopp/1.e6,' million elements'
	!	if(allocated(p_xmat))deallocate(p_xmat)
 
	!	allocate(p_xmat(1:p_ntemp0*p_lopp))
	p_ivvxpack=j_defmatrix(j_ivout,'%vxpack',p_ntemp0,1,j_matreg,single=.true.)
	p_vxpack=>j_o(p_ivvxpack)%r(1:p_ntemp0)
 
	p_ivixpack=j_deflist(j_ivout,'%ixpack',list0=p_ntemp0,ilist=.true.)
	p_ixpack=>j_o(p_ivixpack)%i2(1:p_ntemp0)
 
 
	p_ivvarx=j_deflist(j_ivout,'%varx',list0=p_nxvar)
	p_varx=>j_o(p_ivvarx)%i2(1:p_nxvar)
	write(6,*)'pnxvar',p_nxvar
 
	!	write(6,*)'<56pack,,p_npvar',p_npvar,p_nxvar
	p_varx=pack(p_vars(1:p_npvar),p_isvarx)   ! c-variables are here included as x -variables
	!write(6,*)'p_varx************************ ',p_varx
	!if(allocated(p_varxarea))deallocate(p_varxarea)
	!if(allocated(p_cvar))deallocate(p_cvar)
	! number of x and c variables which are expressed /area
	p_ivibaunit=j_deflist(j_ivout,'%ibaunit',list0=p_nunits+1,ilist=.true.)
	p_ibaunit=>j_o(p_ivibaunit)%i2(1:p_nunits+1)
 
	p_nxvararea=0
	!	p_ncvararea=0
	!p_needc=0
	!			write(6,*)'<8797'
	if(p_ivarea.gt.0)then   ! area-> option present
 
		if(p_nnotareavars.gt.0)then
			p_nxvararea=j_ndiffer(p_varx,p_nxvar,p_notareavars,p_nnotareavars)  ! &
			!		j_o(iob)%i(linknotareavars+1:linknotareavars+p_notareavars),p_notareavars)
			if(p_nxvararea.gt.0)then
				p_ivvarxarea=j_deflist(j_ivout,'%varxarea',list0=p_nxvararea)
				p_varxarea=>j_o(p_ivvarxarea)%i2(1:p_nxvararea)
				!	allocate(p_varxarea(1:p_nxvararea))
				! x area variables = variables - notareavariables
				call j_differ(p_varx,p_nxvar, &
					p_notareavars,p_nnotareavars,p_varxarea,p_nxvararea)
				!	j_o(iob)%i(linknotareavars+1:linknotareavars+p_notareavars),p_notareavars,p_varxarea,p_nxvararea)
 
			else !if(nxvararea.gt.0)then
 
				write(6,*)'*jlp: no variable remains area-variable'
 
			endif !if(p_nxvararea.gt.0)   6819
 
		else
			! all variables are areavariables
			nxvararea=p_nxvar
			allocate(p_varxarea(1:p_nxvar))
			p_varxarea(1:p_nxvar)=p_varx
 
		endif !if(p_nnotareavars.gt.0)   6816
		! if(p_ncvar.gt.0)then
		! allocate(p_cvar(1:p_ncvar))
		! p_needc=1 !make logical
		! endif !if(p_ncvar.gt.0)   6253
 
	endif !if(p_ivarea.gt.0)   6814
 
 
 
end subroutine initxdatawx
 
subroutine initxdata()
	write(6,*)'initxdata'
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
		allocate(p_fastreject(1:p_lopp))
		p_fastreject=.false.
 
	endif !if(p_fast)   6863
 
 
	if(allocated(p_rejects))deallocate(p_rejects)
	! subreject-> option was given
	!	if(subfilre)then;allocate(p_rejects(1:p_lopp));p_rejects=.false.;endif
	allocate(p_rejects(1:p_lopp));p_rejects=.false.
	!	allocate(p_nunitsrow(0:p_nrow));p_nunitsrow=0
 
 
 
	!if(allocated(p_xps))deallocate(p_xps)  ! sums of x-variables  over key schedules
	if(allocated(p_xsmin))deallocate(p_xsmin) ! smallest possible sum
	if(allocated(p_xsmax))deallocate(p_xsmax)   !largest possible sum
	if(allocated(p_xmin))deallocate(p_xmin)
	if(allocated(p_xmax))deallocate(p_xmax)
 
	p_ivxps=j_defmatrix(j_ivout,'%xps',p_nrowtot,1,j_matreg)
	p_xps(0:)=>j_o(p_ivxps)%d(1:p_nrowtot)
 
	allocate(p_xsmin(0:p_nrow),p_xsmax(0:p_nrow))
	allocate(p_xmin(0:p_nrow),p_xmax(0:p_nrow))
 
	p_ivvaluedif=j_defmatrix(j_ivout,'%valuedif',p_nunits,1,j_matreg)
	p_ivobjdif=j_defmatrix(j_ivout,'%objdif',p_nunits,1,j_matreg)
	! endif !if(size(p_keys).ne.p_nunits)   6203
	! endif !if(p_warm)   6202
	! if(.not.p_warm)deallocate(p_keys)
	! endif !if(allocated(p_keys))   6201
 
 
	!allocate(p_ibaunit(1:p_nunits+1))
 
	!if(p_p8)write(6,*)'<5029 allocating p_ibaunit ',p_nunits+1
 
end subroutine initxdata
 
subroutine initdomain()
	! 	if(p_isdomain)then
	write(6,*)'initdomain'
	p_ndomvars=j_o(p_ivdomvars)%i(1)
	p_domvars=>j_o(p_ivdomvars)%i2(1:p_ndomvars)
 
	!write(6,*)'p_ndom ',p_ndom
	!	p_ndoms=j_o(p_ivproblem)%i(11)
 
	p_ivdomaintrans=j_o(p_ivproblem)%i(12)
 
	!j_o(ivout)%i2(1)=ivinl
	if(p_ivdomaintrans.gt.0)then
		ivinl=j_o(p_ivdomaintrans)%i2(1)
		ivunit=j_o(p_ivdatac)%i(6)
		if(p_isunit)ivunit=p_ivunit
		call j_getname(ivunit,p_ivdatac)
		write(6,*)'%domain ',j_oname(1:j_loname),' ',j_oname(1:j_loname)
 
		do j=1,j_o(ivinl)%i(1)
			iiv=j_o(ivinl)%i2(j)
			if(j_inlistobject(iiv,p_ivkeepc).gt.0)cycle
 
			if(iiv.eq.ivunit)cycle
			call j_getname(iiv,ivunit)
			write(6,*)' '
			write(6,*)'*domain variable ',j_oname(1:j_loname),&
				' is not in c-data and is not the unit variable ',j_oname2(1:j_loname2)
			j_err=.true.
 
		enddo !j=1,j_o(ivinl)%i(1)   6924
	endif !if(p_ivdomaintrans.gt.0)   6917
	if(j_err)return
	! iv=j_o(p_ivproblem)%i(13)
	! p_ndomvars=j_o(iv)%i(1)
	! p_domvars=>j_o(iv)%i2(1:p_ndomvars)
	! if(p_p8)write(6,*)'<p_domvars',p_domvars
 
 
	p_ndomv=j_o(p_ivproblem)%i(14)
 
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
 
 
	endif !if(p_domvars(1).eq.j_ivall)   6984
 
 
 
 
 
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
				endif !if(idomv.eq.p_domvars(ido))   7003
			endif !if(p_ninrowx(iro).gt.0)   7001
 
		enddo !iro=0,p_nrow   7000
		lkm=lkm+p_nixcu(ido)
	enddo !ido=1,p_ndomvars   6998
 
 
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
 
 
		enddo !j=1,p_nixcu(ido)   7024
		if(ido.lt.p_ndomvars)p_ixcubas(ido+1)=p_ixcubas(ido)+p_nixcu(ido)
	enddo !ido=p_ido1,p_ndomvars   7022
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
	enddo !ido=p_ido1,p_ndomvars   7045
	!is(p_p8)write(6,*)'<66icurint',p_icurint,'bit',p_icurbit
 
 
 
	!	endif !if(p_isdomain)   4962
	!if(p_isdomain)then
 
	!is(p_p8)write(6,*)'<4996 p_rowx',p_rowx
	!only if there are domains it  must be made difference between xror ans rowcurx
	! otherwise only cur is needed
	do iro=0,p_nrow
		if(p_ix(iro).ne.0.and.p_rowdomvar(iro).eq.j_ivall)p_ixcur0(iro)=.true.
 
	enddo !iro=0,p_nrow   7059
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
		endif !if(p_domvars(p_rowdomnum(j)).eq.j_ivall.and.p_ix(j).ne.0)   7097
	enddo !j=0,p_nrow   7096
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
	use fletdmod !nopre!   !file jlpd.f90
	use fletdmod2 !nopre!
	!	use jlpdmod !nopre!
 
	use fletcherdmod  !nopre!
	use lastmod !nopre!
	integer ::nup
	logical indomain,indomain2
	common/noutc/nout
	common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj  !Fletcher parameters
	!dense common/factorc/m0,m1,mm0,mm,mp,mq
	!sparse common/factorc/m1,m2,mp,mq,lastr,p_irow
 
	!this means that when in dense mpjj and mqjj are changed
	! then in sparse mpjj is the same as
 
	common/ipivotc/ipivot99  !used only in fltcherd.for to print pivot at malfunctio
	common/refactorc/nup,nfreq
 
	write(6,*)'initopt'  !,pmxd,p_nz',p_mxd,p_nz
	!is(p_p)write(p_n16,*)'initopt'  ,p_nm!
 
 
 
	! if(allocated(p_ls))deallocate(p_ls)
	! allocate(p_ls(1:p_mxnm))   ! ls= list of columns in A which are in the basis
	! i.e. ls contains columns in ld-list plus columns of z-part of the A matrix
	!!is(p_p) write(p_n16,*)'lsmax****',p_mxnm,p_nm
	! A matrix
 
	!*objr0 the 'official' objective row
	!*objr2 the working obective row wehen the problem is infeasible
	!	p_objr0=p_zero;p_objr2=p_zero
 
	! z variables in the problem *******************
	if(p_nz.gt.0)then  !p_nz=number of z-variables
		!	write(6,*)'number of z-variables ',p_nz,p_zmatrix,'p_npvar',p_npvar
		!	write(6,*)'p_isvarx',p_isvarx
		!if(p_ivoutresu
		!	p_p2=.true.
		!	p_p8=.true.
		!! commented. March 2011  if(iout.ne.ivresult)then
		! jlp-function has output iout
		! if(p_zmatrix)then
 
		! iel=0
		! p_objr0(p_nrow+1:p_nrow+p_nz)=j_o(p_ivzobj)%d(1:p_nz)
		! do i=1,p_nrow
		! do j=1,p_nz
		! iel=iel+1
		! p_a(i,j)=j_o(p_ivzmatrix)%d(iel)
		! enddo !j=1,p_nz   5190
		! enddo !i=1,p_nrow   5189
 
 
		!	if(p_xpresent)then
		!if(j_ivout.ne.j_ivresult) then
		!	call j_deflist(j_ivout,'%zvars',ivzvar,nres=p_nz)
		!	p_zvars=>j_o(ivzvar)%i2
		!		write(6,*)'jlpzv ',p_zvars
		!else !if(j_ivout.ne.j_ivresult) then
		! if(allocated(p_zvars))deallocate(p_zvars)
		! allocate(p_zvars(1:p_nz))
		!endif !if(j_ivout.ne.j_ivresult) then
		! no output for the jlp-function
		! z-variables: not z-variable, not factory´xvariables or factory y-variables
		! !is(p_p8)then
		! write(6,*)'3456pack,p_fpresent,p_nz,p_npvar',p_fpresent,p_nz,p_npvar
		! write(6,*)'<6189 allocated(p_isvarx),allocated(p_isfx),allocated(p_isfy)',&
		! allocated(p_isvarx),allocated(p_isfx),allocated(p_isfy),allocated(p_zvars)
		! !		write(6,*)size(p_isvarx),size(p_isfx),size(p_isfy)
		! write(6,*)size(p_zvars)
		! endif !!is(p_p8)   5287
 
 
		p_zvars0=p_nz
		! p_nterm= number of coefficients in the
		nzval=p_nterm-p_nxval   !muuta kun fact
		!		p_idomba=0  ! basis for values
		!		p_irow0=0
		!		p_irow=0
		ival=0
		do ii=1,p_nterminrow(0)
			ival=ival+1
			if(.not.p_istermx(ival))then
				iz=j_inlist(p_termvar(ival),p_zvars0,p_zvars)
				if(p_maxo)then  ! objective maximized
					p_objr0(p_nrow+iz)=p_coef(ival) !object row
				else !if(j_maxo)then
					p_objr0(p_nrow+iz)=-p_coef(ival) ! -objective maximized
				endif !if(p_maxo)   7209
			endif !if(.not.p_istermx(ival))   7207
		enddo !ii=1,p_nterminrow(0)   7205
 
 
 
 
		do irow=1,p_nrow
 
			!constraint row
			!		p_irow=p_irow+1
			do ii=1,p_nterminrow(irow)
				ival=ival+1   ! the number of variable with coefficient
				if(.not.p_istermx(ival))then        !is the variable not x-variable
 
					iz=j_inlist(p_termvar(ival),p_zvars0,p_zvars)  !is it z-variable
					!	write(6,*)'ival',iz,ival,p_termvar(ival),p_coef(ival),p_zvars0,p_zvars
 
					! put coefficients of z variables into A matrix
					p_a(p_abas(iz)+irow)=p_coef(ival)  !p_a(irow,iz)=p_coef(ival)
				endif !if(.not.p_istermx(ival))   7226
			enddo !ii=1,p_nterminrow(irow)   7224
 
		enddo !irow=1,p_nrow   7220
		!	else !P_xpreset
 
 
 
		!	endif !if(p_xpresent)   5225
 
		! !is(p_p2)then
		! write(6,'(a,(10f8.2/))')'obj row', p_objr0
		! write(6,*)'amat'
		! do jj7=1,p_nrow;write(p_n16,'(10f8.2)')(p_a(jj7,nco7),nco7=1,p_ncol) ;enddo
		! endif !!is(p_p2)   6535
	endif !if(p_nz.gt.0)   7158
	!	!is(p_p8)write(6,*)'startup,nrow,p_ncol',p_nrow,p_ncol,' p_ifail :',p_ifail
	if(p_ifail.gt.0)then
		write(6,*)'**at startup p_ifail=',p_ifail, 'tell J. Lappi'
		j_err=.true.
		return
 
	endif !if(p_ifail.gt.0)   7250
 
	! if(allocated(p_lsi))deallocate(p_lsi)
	! allocate(p_lsi(1:p_mxnm))
 
	! do j=1,p_nm !nm=ncol+nrow    ! number of columns in (I A)
	! p_lsi(j)=j    !intially ls is in order, residuasl are in the basis
	! enddo !j=1,p_nm   5888
	!write(p_n16,*)'slsi,p_mxnm,p_nm',p_mxnm,p_nm
	ll0=ll1-1
 
	! residuals are initially in the basis
	! lr =list columns of I, i.e. residuals
	! if(allocated(p_lr))deallocate(p_lr);if(allocated(p_lri))deallocate(p_lri)
	! allocate( p_lr(1:p_nrow),p_lri(1:p_nrow))
 
	! do i=1,p_nrow
	! p_lr(i)=i
	! p_lri(i)=i	!inverse list
	! enddo !i=1,p_nrow   5899
	p_lr0=p_nrow		! all residuals are in the basis
 
	! if(p_nz.gt.0)then
	! if(allocated(p_lz))deallocate(p_lz,p_lzi,p_redcost)
	! ! redcost = reduced costs of z-variables
	! allocate( p_lz(1:p_nz),p_lzi(1:p_nz),p_redcost(1:p_nz))  !miksei testata onko nz.eq.0  ??
	! endif !if(p_nz.gt.0)   6569
 
	! do i=1,p_nz
	! p_lz(i)=i		! list allz-cols in order
	! p_lzi(i)=i	! nrow not included
	! enddo !i=1,p_nz   5911
	p_lz0=0			! no z-variable is in the basis
	!käytetään mxd
	!mxd2=ncol-p_nz	! actual max num of colums for D, sotkua???? parempi luku
	!      write(6,*)'mxd,ncol,p_nz,mxd2',mxd,ncol,p_nz,mxd2
 
	! if(allocated(p_lx))deallocate(p_lx)
	! if(allocated(p_lxi))deallocate(p_lxi)
	! allocate( p_lx(1:p_mxd),p_lxi(1:p_mxd))
 
	! do i=1,p_mxd
	! p_lx(i)=i          !lists columns of D
	! p_lxi(i)=i        !inverse list
	! enddo !i=1,p_mxd   5924
	p_lx0=0	! number of d -variables in the basis
 
	! if(allocated(p_lf))deallocate(p_lf)
	! if(allocated(p_lfi))deallocate(p_lfi)
	! allocate(p_lf(p_mxd+1 :2*p_mxd),p_lfi(p_mxd+1 :2*p_mxd))
 
 
	! if (p_fpresent) then
	! do i=p_mxd+1,2*p_mxd
	! p_lf(i)=i          !lists columns of D
	! p_lfi(i)=i       !inverse list
	! enddo !i=p_mxd+1,2*p_mxd   5936
	! p_lf0=p_mxd
	! !jatketaanko seuraavasta tehdasmjasta tarkastelu/ienter, ixkenter alustukset
	! p_ienter= 0
	! ixkenter = 0
	! p_unitrans = 0	! yksikkö, jolle viimeksi laskettu tehdasmja muunnokset
	! endif !if (p_fpresent)   5935
 
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
			j_err=.true.
			return
		endif !if(p_nonlin)   7330
		p_maxo=.false.
		p_coefmax=-j_1
 
	endif !if(iobjtype.gt.0)   7326
 
 
	!is(p_p8)write(6,*)'p_ndoms,p_nrowtot,p_nterm,p_ndom',p_ndoms,p_nrowtot,p_nterm,p_ndom
 
	p_ivdomvars=j_o(p_ivproblem)%i(10)
 
 
	!write(6,*)'end initopt'
 
 
end subroutine initopt
 
subroutine initoptz()
	use fletdmod !nopre!   !file jlpd.f90
	use fletdmod2 !nopre!
	!	use jlpdmod !nopre!
 
	use fletcherdmod  !nopre!
	use lastmod !nopre!
	integer ::nup
	logical indomain,indomain2
	common/noutc/nout
	common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj  !Fletcher parameters
	!dense common/factorc/m0,m1,mm0,mm,mp,mq
	!sparse common/factorc/m1,m2,mp,mq,lastr,p_irow
 
	!this means that when in dense mpjj and mqjj are changed
	! then in sparse mpjj is the same as
 
	common/ipivotc/ipivot99  !used only in fltcherd.for to print pivot at malfunctio
	common/refactorc/nup,nfreq
 
	write(6,*)'initoptz'  !,p_mxd,p_nz
	! mitkä olivat xvar-muuttujat
	la=p_nrow ! number of rows in Fletcher
	p_lavec(1)=la
	!p_nz=p_npvar
	!	p_ncol=p_nz
	!	p_nrowz=p_nrow+p_nz  ! basis (first element -1) of D part, when I is included
	!	p_nm=p_ncol+p_nrow
	!p_mxn=p_nz
	!p_mxnm=p_mxn+p_nrow
	!p_zopt=.true.
	!p_mxn=p_mxd+p_nz !mx number of columns in A  , I.e. D+ coefficients of z-variables
	!if(p_fpresent) p_mxn=p_mxn+p_mxd
	!p_ncol=p_mxn !!!
	!	p_mxnm=p_mxn+p_nrow !mx number of columns (icluding the I part)
	! if(allocated(p_ls))deallocate(p_ls);if(allocated(p_lsi))deallocate(p_lsi)
	! allocate(p_ls(1:p_mxnm),p_lsi(1:p_mxnm))   ! ls= list of columns in A which are in the basis
	! ! i.e. ls contains columns in ld-list plus columns of z-part of the A matrix
	! if(p_p) write(p_n16,*)'lsmax',p_mxnm
	! if(allocated(p_a))deallocate(p_a)
	! allocate(p_a(1:p_nrow,0:p_mxn) ) ;p_a=j_0         ! A matrix
	! if(allocated(p_objr0))deallocate(p_objr0)
	! if(allocated(p_objr2))deallocate(p_objr2)
	! allocate( p_objr0(1:p_mxnm));allocate( p_objr2(1:p_mxnm));;p_objr0=j_0;p_objr2=j_0
	! if(allocated(p_xmi))deallocate(p_xmi,p_xma);if(allocated(p_xma))deallocate(p_xma)
	! allocate( p_xmi(1:p_mxnm),p_xma(1:p_mxnm))
	! p_xmi=p_zero ;p_xma=p_zero
	! p_a=p_zero
	!*objr0 the 'official' objective row
	!*objr2 the working obective row wehen the problem is infeasible
	!	p_objr0=p_zero;p_objr2=p_zero
 
	! z variables in the problem *******************
	!if(p_nz.gt.0)then  !p_nz=number of z-variables
	!write(6,*)'number of z-variables ',p_nz,p_zmatrix,'p_npvar',p_npvar
	!		write(6,*)'p_isx',p_isx
	!if(p_ivoutresu
	!	p_p2=.true.
	!	p_p8=.true.
	!! commented. March 2011  if(iout.ne.ivresult)then
	! jlp-function has output iout
	! if(p_zmatrix)then
 
	! iel=0
	! p_objr0(p_nrow+1:p_nrow+p_nz)=j_o(p_ivzobj)%d(1:p_nz)
	! do i=1,p_nrow
	! do j=1,p_nz
	! iel=iel+1
	! p_a(i,j)=j_o(p_ivzmatrix)%d(iel)
	! enddo !j=1,p_nz   5190
	! enddo !i=1,p_nrow   5189
 
 
	!	if(p_xpresent)then
	!if(j_ivout.ne.j_ivresult) then
	!	call j_deflist(j_ivout,'%zvars',ivzvar,nres=p_nz)
	!	p_zvars=>j_o(ivzvar)%i2
	!		write(6,*)'jlpzv ',p_zvars
	!else !if(j_ivout.ne.j_ivresult) then
	!	write(6,*)'pzm',p_zmatrix
	if(p_zmatrix)then
		!		write(6,*)'dghgfgfgdh'
		iel=0
		if(p_maxo)then
			p_objr0(p_nrow+1:p_nrow+p_nz)=j_o(p_ivzobj)%d(1:p_nz)
		else
			p_objr0(p_nrow+1:p_nrow+p_nz)=-j_o(p_ivzobj)%d(1:p_nz)
		endif !if(p_maxo)   7434
 
		do i=1,p_nrow
			do j=1,p_nz
				iel=iel+1
				p_a(p_abas(j)+i)=j_o(p_ivzmatrix)%d(iel) !p_a(i,j)=j_o(p_ivzmatrix)%d(iel)
			enddo !j=1,p_nz   7441
		enddo !i=1,p_nrow   7440
		!		write(6,*)'hdhdh'
	else
 
 
		p_ivzvars=j_deflist(j_ivout,'%zvars',list0=p_nz)
		p_zvars=>j_o(p_ivzvars)%i2(1:p_nz)
 
		p_zvars=p_vars(1:p_npvar)
 
 
		p_zvars0=p_nz
		! p_nterm= number of coefficients in the
		nzval=p_nterm-p_nxval   !muuta kun fact
		!		p_idomba=0  ! basis for values
		!		p_irow0=0
		!		p_irow=0
		ival=0
 
		! if(p_ispullout)then
		! call j_defmatrix(j_ivout,'%constraint',p_nrow,p_nz,j_matreg,ivconstr)
		! call j_defmatrix(j_ivout,'%obj',p_nz,1,j_matreg,ivobj)
		! write(6,*)'ivconstr ',ivconstr,ivobj,p_nz
		! endif !if(p_ispullout)   6777
 
		do ii=1,p_nterminrow(0)
			ival=ival+1
			! if(p_ispullout)then
			! iz=j_inlist(p_termvar(ival),p_nz,p_zvars)
			! j_o(ivobj)%d(iz)=p_coef(ival)
			! endif !if(p_ispullout)   6785
			!if(.not.p_istermx(ival))then
			!iz=j_inlist(p_termvar(ival),p_zvars0,p_zvars)
			if(p_maxo)then  ! objective maximized
				p_objr0(p_nrow+ii)=p_coef(ival) !object row
			else !if(j_maxo)then
				p_objr0(p_nrow+ii)=-p_coef(ival) ! -objective maximized
			endif !if(p_maxo)   7478
			!endif !if(.not.p_istermx(ival))   5316
		enddo !ii=1,p_nterminrow(0)   7470
 
		iba=0
		do irow=1,p_nrow
 
			!constraint row
			!		p_irow=p_irow+1
			do ii=1,p_nterminrow(irow)
				ival=ival+1   ! the number of variable with coefficient
 
				iz=j_inlist(p_termvar(ival),p_zvars0,p_zvars)  !is it z-variable
 
 
				! put coefficients of z variables into A matrix
				p_a(p_abas(iz)+irow)=p_coef(ival)  !	p_a(irow,iz)=p_coef(ival)
				! if(p_ispullout)then
				! write(6,*)'p_termvar(ival)',p_termvar(ival),iz,iba
				! j_o(ivconstr)%d(iba+iz)=p_coef(ival)
				! endif !if(p_ispullout)   6812
 
			enddo !ii=1,p_nterminrow(irow)   7491
			iba=iba+p_nz
		enddo !irow=1,p_nrow   7487
	endif !if(p_zmatrix)   7431
 
	if(allocated(p_lower))deallocate(p_lower)  ! is lower bound active
	allocate(p_lower(1:p_nrow))
	p_ivlower=j_deflist(j_ivout,'%lower',list0=p_nrow,ilist=.true.)
	p_lowerJ=>j_o(p_ivlower)%i2(1:p_nrow)
	do i=1,p_nrow
		if(p_lbou(i))then
			p_rhscur(i)=p_rhs(i)
			p_lower(i)=.true.
		else !if(j_lbou(i))then
			p_rhscur(i)=p_rhs2(i)
			p_lower(i)=.false.
		endif !if(p_lbou(i))   7514
		!	p_rhsw(i)=p_rhscur(i)
	enddo !i=1,p_nrow   7513
	!		p_rhsw(i)=p_rhscur(i)
	p_lowerJ=p_lower
 
	!	else !P_xpreset
 
 
 
	!	endif !if(p_xpresent)   5225
 
	! if(p_p2)then
	! write(p_n16,'(a,(10f8.2/))')'obj row', p_objr0
	! write(p_n16,*)'amat'
	! do jj7=1,p_nrow;write(p_n16,'(10f8.2)')(p_a(jj7,nco7),nco7=1,p_ncol) ;enddo
	! endif !if(p_p2)   6844
	!	endif !if(p_nz.gt.0)   5256
 
	!write(6,*)'end initopt'
 
end subroutine initoptz
 
subroutine preopt()
	!preoptimization  !!!!
	write(6,*)'preopt'
	do kief=1,1
		p_apusum=p_zero
		p_idomba=0
		do i=1,p_nunits
			write(6,*)'preop'
			if(p_isdomain)call jlpcurix()
			! determines for each row if the unit iunit belonggs to the domain of the row
			! units must be accessed one by one
			! p_nrowcurx
			! p_rowcurx
			! p_idomba
			! returns nrowp and rowcurx
 
			!	if(p_p8)write(6,*)'unit',i,'p_nrowcurx',p_nrowcurx,'<rowcurx',p_rowcurx
			p_valueopt=j_big
			do k=1,p_ns(i)
				if(p_filre)then
					if(p_rejects(p_ibaunit(i)+k))cycle
				endif !if(p_filre)   7562
				p_value=p_zero  !value is the sum of infeasibility
				if(p_rowcurx(1).eq.0)then
					p_i1=2
				else !if(j_rowcurx(1).eq.0)then
					p_i1=1
				endif !if(p_rowcurx(1).eq.0)   7566
				ibxmatx=ibamatx(p_ibaunit(i)+k) !,1)
				ibxmatx2=ibamatx(p_ibaunit(i)+p_keys(i)) !,2)
				do jj=p_i1,p_nrowcurx
					j=p_rowcurx(jj)
					curx=p_xmat(p_ix(j)+ibxmatx)
					curk=p_xmat(p_ix(j)+ibxmatx2)
					if(p_xps(j)+curx-curk.gt.p_rhs2(j))then
						p_value=p_value+p_xps(j)+curx-curk-p_rhs2(j)
					elseif(p_xps(j)+curx-curk.lt.p_rhs(j))then !if(j_xps(j)+curx-curk.gt.j_rhs2(j))then
						p_value=p_value+p_rhs(j)-p_xps(j)-curx+curk
					endif !if(p_xps(j)+curx-curk.gt.p_rhs2(j))   7577
 
				enddo !jj=p_i1,p_nrowcurx   7573
				if(p_value.lt.p_valueopt)then
					p_valueopt=p_value
					kopt=k
 
				endif !if(p_value.lt.p_valueopt)   7584
				if(k.eq.p_keys(i))p_valuek=p_value
			enddo !k=1,p_ns(i)   7561
			if(p_valuek.gt.p_valueopt)then
				ibxmatx=ibamatx(p_ibaunit(i)+kopt) !,1)
				! ibxmatx2=j_ibaunit(i)+j_keys(i) put earlier
 
				do jj=1,p_nrowcurx
					j=p_rowcurx(jj)
					curx=p_xmat(p_ix(j)+ibxmatx)
 
 
 
					p_xps(j)=p_xps(j)+curx-p_xmat(p_ix(j)+ibxmatx2)
 
					!	if(j.eq.2)write(27,*)'<47747',curx,ibxmatx2,p_ix(j)+ibxmatx2,&
					!		p_xmat(p_ix(j)+ibxmatx2),p_xps(j)
 
					! if(i.le.2.and.j.eq.0)write(6,*)'<47747',curx,ibxmatx2,p_ix(j)+ibxmatx2,&
					! p_xmat(p_ix(j)+ibxmatx2),p_xps(j)
 
				enddo !jj=1,p_nrowcurx   7595
				p_keys(i)=kopt
				p_apusum=p_apusum+p_valuek-p_valueopt
 
			endif !if(p_valuek.gt.p_valueopt)   7591
		enddo !i=1,p_nunits   7549
		write(6,*)'preoptimization round ',kief,' improved infeasibility sum by ',p_apusum
		write(6,*)' '
	enddo !kief=1,1   7546
 
	if(p_iprint.ge.0)then
		write(6,*)'x-sums over keyschedules after preopimization'
		do jj=1,p_nrowcurx   !nrowp = number of rows with x-variables
			j=p_rowcurx(jj) !j= number of the row
			write(6,*)j,p_xps(j)
			!	if(j.eq.2)write(27,*)'<init',p_xps(j)
		enddo !jj=1,p_nrowcurx   7621
	endif !if(p_iprint.ge.0)   7619
	p_objf=p_small
end subroutine preopt
 
subroutine initxdatjlp()
	write(6,*)'initxdatjlp '
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
	! now isx tell for all variables in vars%problem if they are x variable
	! nrowx tell for each row the number of x-variables in the row
	! isxval tells for each element in coef and p_termvar if it is x-variable
 
	! starts acces to c-data: !!!!
	!if(.not.p_isunit)call j_getdat(p_ivdatac,p_nunits,p_ivdatamatc,p_ivkeepc) !,p_ivtransc,p_ivvarsc)
	if(j_err)return
	!if(p_nfx.gt.0.or.p_nfy>0) call initkeyfact()
 
 
	ibasc=0
 
	iwar=0   ! number of units where all schedules are rejected
 
	p_idomba=0
	ibas=0
	!write(6,*)'#####nunits,',p_nunits
 
	if(allocated(p_activeunit))deallocate(p_activeunit)
	allocate(p_activeunit(1:p_nunits))
	p_activeunit=.true.
 
	do p_unit=1,p_nunits
		!	write(6,*)'unit5 ',p_unit
		if(p_isunit)then
 
			!		j_v(j_o(p_ivkeepx)%i2(1:p_nkeepx))=j_o(p_ivdatamatx)%d(ibas+1:ibas+p_nkeepx)
			j_v(p_keepx)=p_datamatx(ibas+1:ibas+p_nkeepx)
			ibas=ibas+p_nkeepx
		else
			!	write(6,*)'sh',p_ivkeepc,keepc,p_ivdatamatc
			!	call j_getname(p_ivkeepc,p_ivdatamatc)
			!	write(6,*)'keepc ',j_oname(1:j_loname),' * ',j_oname2(1:j_loname2)
			!	write(6,*)p_nkeepc,j_o(p_ivkeepc)%i(1)
			j_v(p_keepc)=p_datamatc(ibasc+1:ibasc+p_nkeepc)
			ibasc=ibasc+p_nkeepc
			!		call j_getobsiv(p_unit,p_ivdatamatc,p_ivkeepc,p_p_ivunit)
 
		endif !if(p_isunit)   7680
		! if(p_ivtrans.gt.0)then
		! call dotrans(p_ivtrans,1)  ! trans option given
		! if(j_err)then
		! write(6,*)'error for unit ',p_unit
		! stop 815
		! endif !if(j_err)   8347
 
		! endif !if(p_ivtrans.gt.0)   8345
		if(p_isdomain)then
			!			write(6,*)'<456doing,p_needc,p_ndom',p_needc,p_ndom
			j_v(p_ivunit)=p_unit
			call dotrans(p_ivdomaintrans,1)  !there are domain transformations
			if(j_err)then
				write(6,*)'error in domain transformations for unit ',p_unit
				stop 879
			endif !if(j_err)   7707
 
		endif !if(p_isdomain)   7703
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
			!	if(p_p8)write(6,*)'<33ido1 ',p_ido1,p_ndomvars
			do ido=p_ido1,p_ndomvars
 
				!		write(6,*)j_oname(1:j_loname),j_v(domvars(j)),ibit,p_idomba
				if(j_v(p_domvars(ido)).ne.j_0)then
					!	write(6,*)'<45set',j
 
					p_domainunits(ido)=p_domainunits(ido)+1
					! set not
					!		if(p_p8)write(6,*)'ido,pidomba,icurint,bit',ido,p_idomba,p_icurint(ido),p_icurbit(ido)
					p_domainbits(p_idomba+p_icurint(ido))= ibset(p_domainbits(p_idomba+p_icurint(ido)),p_icurbit(ido))
				endif !if(j_v(p_domvars(ido)).ne.j_0)   7730
 
 
			enddo !ido=p_ido1,p_ndomvars   7727
 
			!	if(p_p8)write(6,*)'<11446 domainbits',p_domainbits
			p_idomba=p_idomba+1
 
 
		endif !if(p_isdomain)   7715
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
		endif !if(p_ns(p_unit).le.0)   7756
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
 
				endif !if(j_err)   7770
				if(testcode.eq.0.)then   ! filter->False, reject
					p_rejects(p_nstot)=.true.
					nrej=nrej+1
					p_nrejtot=p_nrejtot+1  !total number of rejected schedules
					goto 776
				endif !if(testcode.eq.0.)   7775
			endif !if(p_filter_)   7767
			if(p_reject)then            ! reject->
				testcode=j_codevalue(iob,j_rejectlink) !call dotrans(iob,j_iosubreject)
				if(j_err)then
					write(6,*)'error for obs ',iob
				endif !if(j_err)   7784
				if(testcode.ne.0.)then    !reject->True
					p_rejects(p_nstot)=.true.
					nrej=nrej+1
					p_nrejtot=p_nrejtot+1
					goto 776
				endif !if(testcode.ne.0.)   7787
			endif !if(p_reject)   7782
			776     continue
			!		write(6,*)'<8767'
		!	if(j_xmatinmemory)then


			if(p_xpresent)then
 
				ibxmatx=(p_nstot-1)*p_ntemp0
 
				if(p_nxvararea.gt.0)then
					! varxarea is a vector
					j_v(p_varxarea)=j_v(p_ivarea)*j_v(p_varxarea)   ! multiply area-variables by area
				endif !if(p_nxvararea.gt.0)   7803
 
				!		if(p_unit.lt.5)write(6,*)'uni ,p_ntemp0',p_unit,p_ntemp0
				do i=1,p_ntemp0
					iba=p_ibatemp(i)
					!	if(p_unit.lt.10)write(6,*)iba,p_ntemp0,iba,ibxmatx,p_nrowxtemp(i),iba+p_nrowxtemp(i)
					!	if(ibxmatx.eq.0)write(6,*)'***',i,p_nrowxtemp(i),p_irowvarx(iba+1),j_v(p_irowvarx(iba+1))
					p_xmat(i+ibxmatx)= &       ! make temporary variables
						dot_product(p_xcoef(iba+1:iba+p_nrowxtemp(i)), &
						j_v(p_irowvarx(iba+1:iba+p_nrowxtemp(i))))
					!	if(p_unit.lt.5)write(6,*)'uni xcoefmp0',p_xcoef(iba+1:iba+p_nrowxtemp(i))
					!		write(i18,*)p_nstot,p_unit,p_ns(p_unit),is,i,j_v(p_irowvarx(iba+1:iba+p_nrowxtemp(i)))
					! if(p_unit.le.5)then
					! write(6,*)'j_nrowxtemp(i)',p_nrowxtemp(i),p_xmat(i+ibxmatx),&
					! p_xcoef(iba+1:iba+p_nrowxtemp(i)),j_v(p_irowvarx(iba+1:iba+p_nrowxtemp(i)))
					! do jj=1,p_nrowxtemp(i)
					! call j_getname(p_irowvarx(iba+jj))
					! write(6,*)jj,p_xcoef(iba+jj),p_irowvarx(iba+jj),j_oname(1:j_loname),j_v(p_irowvarx(iba+jj))
					! enddo
					! endif
					!	if(p_p2.and.p_nstot.eq.1)write(6,*)'p_itemp',i,' iba=',iba,' coef=',j_xcoef(iba+1:iba+j_nrowxtemp(i)),&
					!		' vars',j_irowvarx(iba+1:iba+j_nrowxtemp(i))
				enddo !i=1,p_ntemp0   7809
				!if(p_unit.gt.1)stop 'per'
				!	write(17,*)p_unit,is,p_xmat(ibxmatx+1:ibxmatx+p_ntemp0)
				!j_ix(0),j_ntemp0
				if(.not.p_maxo.and.p_ix(0).ne.0) &
					p_xmat(p_ix(0)+ibxmatx)=-p_xmat(p_ix(0)+ibxmatx)
 
			endif !if(p_xpresent)   7799
			! endif !if(istree)then
			!		if(p_needc.gt.0)j_v(p_cvarl(1:p_ncvar))=p_cvar
		enddo !is=1,p_ns(p_unit)   7761
 
 
 
 
		if(p_filre)then
			if(nrej.ge.p_ns(p_unit))then
				iwar=iwar+1
				if(iwar.le.10)write(6,*)'*err all schedules were rejected for unit ',p_unit
			endif !if(nrej.ge.p_ns(p_unit))   7844
		endif !if(p_filre)   7843
		if(j_err)return
		!		write(6,*)'nuni ',p_nunits,p_unit,ibxmatx
	enddo !p_unit=1,p_nunits   7678
 
	p_nrejdupl=0
	iba=0
	nstot=0
	nstot=0
	ibadat=0
	do i=1,p_nunits
 
		do j2=2,p_ns(i)
 
			if(p_rejects(nstot+j2))cycle
			if(p_xpresent)iba2=iba+(j2-1)*p_ntemp0
			if(p_fpresent)ibadat2=ibadat+(j2-1)*p_nkeepx
j1loop:	 do j1=1,j2-1
				if(p_rejects(nstot+j1))cycle
 
				if(p_xpresent)then
					iba1=iba+(j1-1)*p_ntemp0
					do k=1,p_ntemp0
 
						if(abs(p_xmat(iba2+k)-p_xmat(iba1+k)).gt.1.d-12)cycle j1loop
 
					enddo !k=1,p_ntemp0   7870
				endif !if(p_xpresent)   7868
				if(p_fpresent)then
 
					iba1=ibadat+(j1-1)*p_nkeepx
					do k=1,p_nlog
						if(abs(p_datamatx(ibadat2+j_o(p_ivkeeplog)%i2(k)) -&
							p_datamatx(iba1+j_o(p_ivkeeplog)%i2(k))).gt.1.d-12)cycle j1loop
 
					enddo !k=1,p_nlog   7879
 
				endif !if(p_fpresent)   7876
 
				p_rejects(nstot+j2)=.true.
				p_nrejdupl=p_nrejdupl+1
 
 
			enddo j1loop !oop:	 do j1=1,j2-1   7865
		enddo !j2=2,p_ns(i)   7860
		nstot=nstot+p_ns(i)
		if(p_xpresent)iba=iba+p_ns(i)*p_ntemp0
		if(p_fpresent)ibadat=ibadat+p_ns(i)*p_ntemp0
	enddo !i=1,p_nunits   7858
 
	p_nrejtot=p_nrejtot+p_nrejdupl
 
 
	!	write(6,*)'<56domainbits',p_domainbits
	!	write(6,*)'nunits ',p_nunits,p_ntemp0
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	if(p_xpresent)then
		iba=0
		write(6,*)' '
		write(6,*)'2 first rows of xmat'
		do i=1,2
			write(6,'(i3,(5g18.10))')i,p_xmat(iba+1:iba+p_ntemp0)
			iba=iba+p_ntemp0
		enddo !i=1,2   7908
 
	endif !if(p_xpresent)   7904
 
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
 
	endif !if(p_fast)   7918
	if(.not.allocated(p_svalue))then
		allocate(p_svalue(1:p_maxns),p_basreject(1:p_maxns))
 
	elseif(size(p_svalue).lt.p_maxns)then
		deallocate(p_svalue,p_basreject)
		allocate(p_svalue(1:p_maxns),p_basreject(1:p_maxns))
	endif !if(.not.allocated(p_svalue))   7928
 
 
	if(iwar.gt.0)then
		write(6,*)'**all schedules rejected for ',iwar, 'units'
		j_err=.true.
		return
	endif !if(iwar.gt.0)   7937
 
 
 
	if(allocated(p_solx))deallocate(p_solx)
	if(p_nz.le.0)allocate(p_solx(0:p_nrow))
	!p_nstot adds up the 'ns' variables, lopp is the number of observations in xdata
	if(p_ivunit.le.0.and.p_nstot.ne.p_lopp)then
		write(6,*)'**number of sched in cdat, and xdat do not match',p_nstot,p_lopp
		j_err=.true.
		return
	endif !if(p_ivunit.le.0.and.p_nstot.ne.p_lopp)   7948
	call cpu_time(p_time00)
	!	write(6,*)'time00al',p_time00
	write(*,*)'found ',p_nunits,' units, ',p_lopp,' schedules'
	write(6,*)'from which ',p_nrejtot,' schedules were rejected'
	write(6,*)'from which ',p_nrejdupl,' were duplicated schedules'
	if(p_xpresent)then
		write(6,*)' filling xmat'
 
 
		deallocate (p_varx1,p_varx2)
 
	endif !if(p_xpresent)   7958
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
		enddo !i=1,min(p_ndom,20)   7971
		if(nempt.gt.0)then
			j_err=.true.;write(6,*)'**empty domains:',nempt; return
		end if !if(nempt.gt.0)   7981
	endif !if(p_isdomain)   7965
 
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
	!if(j_o(p_ivdatamatx)%r(10261*keepx).eq.0.)stop 179
	if(p_xpresent)then
		p_xps=j_0
		p_xsmin=p_zero     !minimum for the sum
		p_xsmax=p_zero     !maximum for the sum
		! initial key schedules, rotate number
		key=0
	endif !if(p_xpresent)   8006
 
 
	p_rejectnow=.false.
	!	write(p_n16,*)'<44starti',p_nfunits
	!is(p_p8)write(6,*)'<11773 ,p_nrow,p_ix ',p_nrow,p_ix(1:p_nrow)
	p_idomba=0
	do i=1,p_nunits  !***********************
		! write(p_n16,*)'<44starti',i
		nrej=0
		!	write(6,*)'idom2',i,p_idomba
		if(p_isdomain)call jlpcurix() !determines for each row if the unit p_unit belonggs to the domain of the row
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
				enddo !jj=1,p_nrowcurx   8044
				goto 745
			endif !if(p_rejects(p_ibaunit(i)+1))   8038
		endif !if(p_filre)   8037
 
		! get p_xmin and p_xmax from the first schedule
		ibxmatx=ibamatx(p_ibaunit(i)+1) !,1)
		!write(6,*)'p_nrowcurx ',p_nrowcurx,'*',p_rowcurx(1:p_nrowcurx)
		!write(6,*)'p_ix ',p_ix
		do jj=1,p_nrowcurx
			j=p_rowcurx(jj)
			!write(6,*)'j,p_ix(j),ibmatx',j,p_ix(j),ibxmatx,' unit ',i
			!	p_nunitsrow(j)=p_nunitsrow(j)+1
			!	if(p_ix(j).ne.0)then
			p_xmin(j)=p_xmat(p_ix(j)+ibxmatx) ! v(ix(j))
			p_xmax(j)=p_xmat(p_ix(j)+ibxmatx)  ! v(ix(j))
			!	endif !if(p_ix(j).ne.0)   6706
		enddo !jj=1,p_nrowcurx   8058
 
745   keyopt=1     !works also if first schedule is rejected and we come from  'goto 745'
		do k=2,p_ns(i)
			if(p_filre)then
				if(p_rejects(p_ibaunit(i)+k))then
					nrej=nrej+1
					cycle  !do k=2,nsch(i)
				endif !if(p_rejects(p_ibaunit(i)+k))   8071
			endif !if(p_filre)   8070
			!if(j_o(p_ivdatamatx)%r(10261*keepx).eq.0.)stop 149
			! compute for rows containing x-variables the min and max values
 
			if(.not.p_fpresent)then
				ibxmatx=ibamatx(p_ibaunit(i)+k) !,1)
				do jj=1,p_nrowcurx
					j=p_rowcurx(jj)
					curx=p_xmat(p_ix(j)+ibxmatx)
					if(curx.lt.p_xmin(j))then
						p_xmin(j)=curx
					endif !if(curx.lt.p_xmin(j))   8084
					if(curx.gt.p_xmax(j))then
						p_xmax(j)=curx
						keyopt=k   ! for the case where there are no constraints
					endif !if(curx.gt.p_xmax(j))   8087
				enddo !jj=1,p_nrowcurx   8081
			else
				keyopt=p_keys(i)
 
			endif !if(.not.p_fpresent)   8079
 
		enddo !k=2,p_ns(i)   8069
 
		do jj=1,p_nrowcurx
			j=p_rowcurx(jj)
			p_xsmin(j)=p_xsmin(j)+p_xmin(j)
			p_xsmax(j)=p_xsmax(j)+p_xmax(j)
		enddo !jj=1,p_nrowcurx   8099
 
		if(p_nrow.eq.0.or.j_v(j_ivdollar2).eq.237.d0)then
			key=keyopt   ! no constraints, can be maximized directly
		else !if(j_nrow.eq.0)then
4689    key=key+1
			if(key.gt.p_ns(i)) key=1
			if(p_filre)then
				if(p_rejects(p_ibaunit(i)+key))goto 4689
			endif !if(p_filre)   8110
 
		endif !if(p_nrow.eq.0.or.j_v(j_ivdollar2).eq.237.d0)   8105
 
 
 
		if(p_warm.and.p_nrow.gt.0)then
 
			if(p_filre)p_rejectnow=p_rejects(p_ibaunit(i)+p_keys(i))
			if(p_keys(i).gt.p_ns(i).or.p_rejectnow)then
				p_warm=.false.
				p_warmf=.false.
				write(6,*)'*data does not agree, warm-> ignored'
			else !if(j_keys(i).gt.j_nsch(i).or.p_rejectnow)then
				key=p_keys(i)
			endif !if(p_keys(i).gt.p_ns(i).or.p_rejectnow)   8121
		endif !if(p_warm.and.p_nrow.gt.0)   8118
		p_keys(i)=key
		ibxmatx=ibamatx(p_ibaunit(i)+key) !,1)
 
		!write(6,*)'p_nrowcurx( ',p_nrowcurx,p_rowcurx
		do jj=1,p_nrowcurx
			j=p_rowcurx(jj)
			!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
			p_xps(j)=p_xps(j)+p_xmat(p_ix(j)+ibxmatx) !v(ix(j)) !(ix(j)=0 -> no x in row
			!		if(j.eq.0.and.i.ge.6.and.i.le.10)write(p_n16,*)'<555 ',i,ibxmatx,p_ix(j),p_xmat(p_ix(j)+ibxmatx),p_xps(j)
		enddo !jj=1,p_nrowcurx   8133
 
		if(p_fpresent)call factxps(i,p_keys(i))
 
 
	enddo !i=1,p_nunits   8019
 
	! if(p_p8.and.p_fpresent)then
	! do ir=0,10
	! write(6,*)'<44 ir,xps,p_ixcurfact,p_nfyinrow ',ir,p_xps(ir),p_ixcurfact(ir),p_nfyinrow(ir)
 
	! enddo !ir=0,10   8155
 
 
	! endif !if(p_p8.and.p_fpresent)   8154
 
	! do jj=1,p_nfrowx
	! irowj=p_frowx(jj) !domainissa olevat rivit
	!irowj =j+1 ! p_irowrow(j)
	!	write(6,*)'<379>',p_nfrowx,jj,irowj,p_nfxinrow(irowj),p_ibafx(irowj),p_irowfvarx(p_ibafx(irowj)+1:p_ibafx(irowj)+1)
	!	xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
	!	xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
	! do k=1,p_nfxinrow(irowj)
 
 
 
	! enddo
	!	enddo
	!call initdata2() !1MAKE TRASNFORMATIONS AND DOMAINS
 
	!	stop 'here'
 
	p_iprint=2
	!	p=.true.
 
	! if(ivprint7.gt.0)then
	! if(j_v(ivprint7).eq.6.)call printproblem()
	! endif !if(ivprint7.gt.0)   6459
	!	write(p_n16,*)'xps bef preopt',i,(p_xps(jj7),jj7=0,min(p_nrow,10)) !!!!
	!is(p_p)write(p_n16,*)'p_nrow',p_nrow
	if(p_iprint.gt.1)then
		!write(6,*)' ',j_err
		write(6,*)' '
		write(6,*)'***row, min,max,initial value, tolerance'
	endif !if(p_iprint.gt.1)   8178
	!	write(6,*)'<loub',p_lbou
	!write(6,*)'<u',p_ubou
	!	write(6,*)'lower',p_lower
	!	write(6,*)'<5454',p_x(2)
	! do i=j_lopp-15000,j_lopp
	! iba=ibamatx(i,1)
	! write(17,*)j_o(ivxdatmat)%r(iba+1:iba+7),j_o(ivxdatmat)%r(iba+11:iba+17)
	! iba=ibamatx(i,1)
	! write(18,*)j_xmat(iba+1:iba+j_ntemp0)
	! enddo
	!	write(6,*)'<2492 p_nrowx',p_nrowcurx,p_rowcurx,'p_ntemp0',p_ntemp0
 
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
		endif !if(p_ix(j).ne.0)   8202
	enddo !j=0,p_nrow   8196
 
	!toleranssi tehdasmja-riveille  !!!!
	!write(6,*)'<556',p,p_nrow,p_fpresent
	! if (p_fpresent) then
	! do j=0, p_nrow
	! !	write(27,*)'***fact j,nunits,xps,tole', j,p_nunitsrow(j),p_xps(j),p_tole(j)
	! ! if (p_ix(j)==0) then
	! ! if ((p_nfxinrow(p_irowrow(j))>0).or.(p_nfyrow(p_irowrow(j))>0)) then
	! ! !if(p_iprint.gt.1)  write(6,'(i5,1x,2g18.12)')j,j_xps(j),j_tole(j)
	! ! if(p_p) write(p_n16,*)'***fact j,nunits,xps,tole', j,p_nunitsrow(j), &
	! ! p_xps(j),p_tole(j)
	! ! endif !if ((j_nfxinrow(j_irowrow(j))>0).or.(j_nfyrow(j_irowrow(j))>0)) then
	! ! endif !if (j_ix(j)==0) then
	! enddo !j=0, p_nrow   2829
	! endif !if (p_fpresent)   2828
 
 
 
 
	!preoptimoinnin ohitus jos tehtaita mukana (jatketaan vain jos !fpresent)
 
 
 
 
	!20181116 #z_commented	moved to #zeroc
	!		j_mxd=j_nrow+4  !???max number of d-vectors mx number of
 
	! constaints (la) + 1 or 2 or somethi
	!		integer lunit(0:mxd)	! unit of columns of D,
	! lunit(0)=0 is for termination purposes
 
 
	! if(allocated(p_lunit))deallocate(p_lunit)
	! !	schedule numbers for columns of D
	! if(allocated(p_isch))deallocate(p_isch)
 
 
	mxd2_ = p_mxd
	if (p_fpresent) mxd2_=2*p_mxd
 
	p_ivlunit=j_deflist(j_ivout,'%lunit',list0=mxd2_+1,ilist=.true.)
	p_lunit(0:)=>j_o(p_ivlunit)%i2(1:mxd2_+1)
	p_ivisch=j_deflist(j_ivout,'%isch',list0=p_mxd,ilist=.true.)
	p_isch=>j_o(p_ivisch)%i2(1:p_mxd)
 
	!allocate(p_lunit(0:mxd2_),p_isch(1:p_mxd));p_lunit=0;p_isch=0
 
	! lunit tells for each column of D the corresponding unit
	! isch tells for each column of D the corresponding schedule
 
 
	!ld is list of all columns of A which are in D (i.e. in the x-part of the basis)
	! first ld0 are in basis
	!ldi is inverse list, D can be traversed in inverse order
	p_ivvx=j_defmatrix(j_ivout,'%vx',p_nrowtot,1,j_matreg)
	p_vx(0:)=>j_o(p_ivvx)%d(1:p_nrowtot)
	!	if(allocated(p_vx))deallocate(p_vx)
	!	allocate(p_vx(0:p_nrow))  !shadow prices of x-variables
 
	if(allocated(p_testxps))deallocate(p_testxps,p_test)
	if(allocated(p_test))deallocate(p_test)
	allocate(p_testxps(0:p_nrow),p_test(0:p_mxn))
 
	!		integer next(0:p_p_mxn) !  is used to travel through columns of D so that
	! the columns correponding to same unit are after each other.
	! if last is the last column in the sequence, then next(last)=0
	! next(0) first column
	if(allocated(p_next))deallocate(p_next)
	if(allocated(p_iprev))deallocate(p_iprev)
	allocate(p_next(0:p_mxn),p_iprev(0:p_mxn))
 
	!write(6,*)'<6363636 ',p_mxn
 
end subroutine initxdatjlp
 
subroutine checkinfeas()
	!	write(6,*)'jerrher',j_err
	!infeasiblity at start tarkastus siirretty tähän
	! test if problem is immediately infeasible !!!!
	iunf=0
	if(p_rowcurx(1).eq.0)then
		p_i1=2  ! there is x variable in the objective row
	else !if(j_rowcurx(1).eq.0)then
		p_i1=1   ! there is no  "
	endif !if(p_rowcurx(1).eq.0)   8289
	nunf=0 ! number of infeasible rows
	!write(6,*)'<66p_nz ',p_nz
		rowloop:			do jj=1,p_nrowcurx
		j=p_rowcurx(jj)
		if(j.eq.0)cycle rowloop
		!	call j_printname(' tas22 ',j,' ')
		if(p_nz>0) then
			do inz_=1,p_nz
				!		write(6,*)'inz_',inz_,p_a(j,inz_)
				if(p_a(p_abas(inz_)+j).ne.j_0)cycle rowloop !p_a(j,inz_).ne.j_0) cycle rowloop
			enddo !inz_=1,p_nz   8301
		endif !if(p_nz>0)   8300
		if(p_fpresent) then
			if(p_nfxinrow(j)>0.or.p_nfyinrow(j)>0) &
				cycle rowloop
		endif !if(p_fpresent)   8306
		if(p_xsmin(j).gt.p_rhs2(j).or.p_xsmax(j).lt.p_rhs(j))then
			nunf=nunf+1
			iunf=j
			if(nunf.eq.1)then
				write(6,*)'***the problem is infeasible at start, nonfeasible rows: '
				!		write(6,*)'<xsmin',p_xsmin
				if(p_nureport.ne.6)&
					write(p_nureport,*)'***the problem is infeasible at start, nonfeasible rows: '
			endif !if(nunf.eq.1)   8313
			call printrowinfo(iunf)
			if(p_xsmin(iunf).gt.p_rhs2(iunf))then
				write(6,*) &
					'*smallest possible value ',p_xsmin(iunf),' rhs2=',p_rhs2(iunf)
				if(p_nureport.ne.6)write(p_nureport,*)'row ',iunf,'*smallest possible value ',&
					p_xsmin(iunf),' rhs2=',p_rhs2(iunf)
			endif !if(p_xsmin(iunf).gt.p_rhs2(iunf))   8320
			if(p_xsmax(iunf).lt.p_rhs(iunf))then
				write(6,*)'*greatest possible value ', &
					p_xsmax(iunf),' rhs=',p_rhs(iunf)
				if(p_nureport.ne.6)write(p_nureport,*)'row ',iunf,&
					'*greatest possible value ',p_xsmax(iunf),' rhs=',p_rhs(iunf)
			endif !if(p_xsmax(iunf).lt.p_rhs(iunf))   8326
		endif !if(p_xsmin(j).gt.p_rhs2(j).or.p_xsmax(j).lt.p_rhs(j))   8310
	enddo rowloop !loop:			do jj=1,p_nrowcurx   8296
	if(iunf.ne.0)j_err=.true.
	!	write(6,*)'jerHER',j_err
 
end subroutine checkinfeas
subroutine printz()
	nnz=0
	! printing z-variables
	if(.not.p_xpresent)then
		do i=1,p_npvar
			call j_printname(' ',p_vars(i),' ')
		enddo !i=1,p_npvar   8342
		return
	endif !if(.not.p_xpresent)   8341
	do i=1,p_npvar
		!fact
		!isfx ja isfy tarkistetaan vain, jos on tehdastehtävä
 
 
		if(.not.p_isvarx(i).and..not.p_isvarf(i))then
			nnz=nnz+1
			call j_printname(' ',p_vars(i),' ')
			if(nnz.ge.10.and.nnz.lt.p_nz)then
				write(6,*)' ...etc...'
				exit
			endif !if(nnz.ge.10.and.nnz.lt.p_nz)   8355
		endif !if(.not.p_isvarx(i).and..not.p_isvarf(i))   8352
 
	enddo !i=1,p_npvar   8347
 
	return
 
 
end subroutine printz
 
 
 
 
subroutine isfeasible()
	!checks feasiblity and updates shadow prices
	use fletdmod
	use fletdmod2
	use fletcherdmod
	implicit none
	integer nout,i,irow,irowinf,j
	common/noutc/nout
	!write(6,*)'<413>iperke',iperke
	!stop 975
	!p=.true.
	p_nnfold=p_nnf
	p_nnf=0
 
	!		write(6,*)'<48848 p_nnf',p_nnf,p_lr0,p_x(2),'*',p_lr(1:p_lr0)
	p_objr2=p_zero
	p_value=j_0
	p_tolecur=p_zero
	if(.true.)then !.not.j_feasible.or.p_unit.eq.1)then !
		do i=1,p_lr0 !p_lr0 = number of basic residuals
			!rhscur =rhs or rhs2
			irow=p_lr(i)
 
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
 
 
			if(p_ubou(irow)) then
				if(p_x(irow)+p_tole(irow).lt.p_rhscur(irow)-p_rhs2(irow))then
					p_objr2(irow)=p_one
					irowinf=irow
					p_nnf=p_nnf+1
 
					!				stop 'tas'
 
					if(p_lower(irow))then
						p_rhscur(irow)=p_rhs2(irow)
						p_lower(irow)=.false.
						p_x(irow)=p_x(irow)+p_rhs2(irow)-p_rhs(irow)
 
						!		if(irow.eq.2)write(6,*)'irow',irow,'vaihdetaan ylärajaan',p_x(irow)
						if(p_xpresent)then
							if((p_ix(irow).ne.0).or.p_fpresent)p_rhsw(irow)=p_rhscur(irow)-p_xps(irow)
						endif !if(p_xpresent)   8423
					endif !if(p_lower(irow))   8417
					p_value=p_value+p_x(irow)
					if(p_p)write(6,*)'infer',irow,p_x(irow)
					p_tolecur=p_tolecur+p_tole(irow)
				endif !if(p_x(irow)+p_tole(irow).lt.p_rhscur(irow)-p_rhs2(irow))   8410
			endif !if(p_ubou(irow))   8409
			if(p_lbou(irow)) then
				if(p_x(irow)-p_tole(irow).gt.p_rhscur(irow)-p_rhs(irow))then
					! illegal surplus, decrease it
					p_objr2(irow)=p_onen
					p_nnf=p_nnf+1
					!				write(6,*)'<35535399 p_nnf,irow',p_nnf,irow
					!			stop 'here'
					irowinf=irow  !corrected by JL 9.9.2018 was irowind
					if(.not.p_lower(irow))then
						p_rhscur(irow)=p_rhs(irow)
						p_lower(irow)=.true.
						p_x(irow)=p_x(irow)+p_rhs(irow)-p_rhs2(irow)
						!			if(irow.eq.2)write(6,*)'44 xuus ',p_x(2)
						!is(p_p)write(p_n16,*)'irow',irow,'vaihdetaan alarajaan'
						if(p_xpresent)then
							if((p_ix(irow).ne.0).or.p_fpresent) &
								p_rhsw(irow)=p_rhscur(irow)-p_xps(irow)  !jos xps =00 for nonzero ix
							! could be done without condition
						endif !if(p_xpresent)   8446
					endif !if(.not.p_lower(irow))   8440
					!is(p_p.and.p_nnf.le.50)write(p_n16,*)'infer',irow,p_x(irow)
					if(p_p)write(6,*)'pvalunow',p_value,irow,p_x(irow)
					p_value=p_value-p_x(irow)
					if(p_p)write(6,*)'pvaluetas',p_value
					p_tolecur=p_tolecur+p_tole(irow)
				endif !if(p_x(irow)-p_tole(irow).gt.p_rhscur(irow)-p_rhs(irow))   8433
			endif !if(p_lbou(irow))   8432
 
			! common part of the nonfeasible
			! compute prices of basic variables
			! irow<=nrow -> objr:tä ei käytetä
			! test decreasing
		enddo !i=1,p_lr0   8390
 
		p_objfv=p_objf
		if(p_p)write(6,*)'pnnf',p_nnf,p_feasible,p_objf
		if(p_nnf.gt.0)then
			!is(p_p)write(p_n16,*)'nonfeas rows',p_nnf,'Pivots=',p_pivot,'objf ',p_value
			if(p_feasible)then
				if(p_nnf.gt.1)then
					write(6,*)'again ',p_nnf, ' infeasible rows infeasibility  ',p_value,'Pivots=',p_pivot !!!!
					!is(p_p.or.p_p9)write(p_n16,*)'again infeasible rows ',p_nnf,'infeasibility  ', &
					!	p_value,'Pivot=',p_pivot,&
					!		'p_ienter=' ,p_ienter,' p_leavec ',p_leavec, &
					!		'p_newc ',p_newc,'emptmax,j_tmax', p_x(max(p_newc,1)),p_tmax
 
				else !if(p_nnf.gt.1)then
					write(6,*)'again infeasible row ',irowinf, &
						' infeasibility  ',p_value, &
						'Pivots=',p_pivot
					!is(p_p.or.p_p9)write(p_n16,*)'again infeasible row ',irowinf,'infeasibility ', &
					!		p_value,'Pivot=',p_pivot,&
					!		'p_ienter=' ,p_ienter,' p_leavec ',p_leavec, 'p_newc ', &
					!		p_newc,'emptmax,j_tmax', p_x(max(p_newc,1)),p_tmax
 
				endif !if(p_nnf.gt.1)   8471
				!	p_again=.true.
				j_err=.true.;return
				!p_nagain=p_nagain+1
			endif !if(p_feasible)   8470
			p_feasible=.false.
			if(.not.p_isobj2)then
				p_objr=>p_objr2
				p_isobj2=.true.
				p_isobj0=.false.
			endif !if(.not.p_isobj2)   8493
			if(p_p)write(6,*)'objr',p_objr(1:min(p_nrow,40)),'+++0,p_value',p_value
			p_objf=p_value
			p_xirowold2=p_small
			if(p_nnf.ne.p_nnfold)then
				p_objfv=p_small
				p_justkey=.false.
			endif !if(p_nnf.ne.p_nnfold)   8501
			p_ix0=0
		else !if(p_nnf.gt.0)then
			if(.not.p_isobj0)then
				p_objr=>p_objr0
				p_isobj0=.true.
				p_isobj2=.false.
			endif !if(.not.p_isobj0)   8507
			!is(p_p)write(p_n16,*)'***tole(0)',p_tole(0)
			p_tolecur=p_tole(0)
 
			if(p_xpresent)p_ix0=p_ix(0)   !use true objective x-variable
			p_objf=dot_product(p_x(p_ls(1:p_nrow)),p_objr(p_ls(1:p_nrow)))
			! p_objf=p_zero
 
			! do j=1,p_nrow
 
			! p_objf=p_objf+p_x(p_ls(j))*p_objr(p_ls(j))
			! if(p_p)write(6,*)'j',p_ls(j),p_x(p_ls(j)), &
			! p_objr(p_ls(j)),p_x(p_ls(j))*p_objr(p_ls(j)),' objf ',p_objf
			! enddo !j=1,p_nrow   9372
 
			if(p_xpresent)then
 
				if(p_ix0.ne.0.or.p_isxk0)p_objf=p_objf+p_xps(0)
				if(p_p)write(6,*)'xps ',p_xps(0),'objfin',p_objf
			endif !if(p_xpresent)   8526
			!is(p_p.and.p_xpresent)write(p_n16,*)'+xp0',p_xps(0)
 
			!is(p_p)write(p_n16,*)'FEASIBLE,objf,objfv,objf-objfv,p_pivot', &
			!	p_objf,p_objfv, &ic
			!	p_objf-p_objfv,p_pivot, 'was feas?',p_feasible
			!write(6,*)'FEASIBLE,objf,objfv,objf-objfv,p_pivot', &
			!	p_objf,p_objfv, &
			!	p_objf-p_objfv,p_pivot, 'was feas?',p_feasible
 
			if(.not.p_feasible)then
				!	write(6,'(60x,a)')'*FEASIBLE,round,pivots,obj',p_kier,p_pivot,p_coefmax*j_objf  !!!!
				!	if(p_xpresent2)write(6,'(2i5,g22.7,f8.2,5i5)')&
				!		p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0, &
				!		p_lf0,p_nnf
				!p_kier,p_pivot
				!	write(6,*)'B=basic, NF=NonFeasible'
				!	write(6,*)'round   pivots    objective    active set(%) B-res  B-z   B-sched  B-xkf  NF-rows'
				!             5    3876     2578655.36949    100.00    456      0      56      564
				!	write(6,*)'nnused,nnread',nnused,nnread
				p_fastusedsame=p_fastusesame-1  ! to force collecting active set after the next round
				p_objfv=p_small
				p_justkey=.false.
				if(p_again)then
					if(p_nagain.le.1)then
						p_againsol=p_objf
					else !if(p_nagain.le.1)then
						if(p_objf.gt.p_againsol+p_tolecur)then
							p_nagain=0
							p_again=.false.
						else !if(j_objf.gt.p_againsol+j_tolecur)then
							if(p_nagain.gt.7)then
								p_again=.false.
								p_nagain=0
								p_feasible=.false.
								p_nrecover=p_nrecover+1
								if(p_nrecover.ge.20)then
									write(6,*)'*jlp* is mixed up (7), try different tole (e.g.10,100,1000)(or consult J. Lappi)'
									j_err=.true.
									p_goto900=.true.;return
								endif !if(p_nrecover.ge.20)   8566
 
								write(6,*)'***cycling (2), trying to recover'
								p_kierv=p_kier
								p_unitv=p_unit
								p_goto1234=.true.;return
 
							endif !if(p_nagain.gt.7)   8561
 
						endif !if(p_objf.gt.p_againsol+p_tolecur)   8557
 
					endif !if(p_nagain.le.1)   8554
 
				endif !if(p_again)   8553
			endif !if(.not.p_feasible)   8540
 
			p_feasible=.true.
			j_v(p_ivfeasible)=j_1
			if(p_xpresent)then
				if(p_lx0.eq.0.and.p_lz0.eq.0)then
					p_vc=j_0
					p_objf=p_xps(0)
					p_objfv=p_objf
					!	write(6,*)'tasa FEASIBLE',p_objf
					return
				endif !if(p_lx0.eq.0.and.p_lz0.eq.0)   8589
			endif !if(p_xpresent)   8588
			! write(6,*)'FEASIBLe justkey p_lx0',p_pivot,p_justkey,p_lx0
			! write(6,*)'LS ',p_ls(1:p_nrow),p_maxo
			! write(6,*)'p_vc ',p_vc
			! write(6,*)'objr ',p_objr(p_ls(1:p_nrow))
			! write(6,*)'lower',p_lower
			! write(6,*)'rhscur',p_rhscur
			! write(6,*)'xps',p_xps
			! write(6,*)'p_maxo ',p_maxo
		endif !if(p_nnf.gt.0)   8468
 
 
		! if(p_justkey)then   !only key schedule has been changed
		! !	!is(p_p)write(p_n16,*)'just key-> prev prices'
		! !newprice=.false.
		! else !if(p_justkey)then
 
 
		! j_dapu=j_0
		! do i=1,p_lr0
		! j_dapu=max(abs(p_objr(p_lr(i))),j_dapu)
		! enddo !i=1,p_lr0   7840
		! !	write(17,*)
		! do i=1,p_lz0
 
		! j_dapu=max(abs(p_objr(p_nrow+p_lz(i))),j_dapu)
		! enddo !i=1,p_lz0   7844
		! if(j_dapu.le.p_epsj)then
		! do i=1,p_lx0
 
		! j_dapu=max(abs(p_objr(p_nrowz+p_lx(i))),j_dapu)
 
		! enddo !i=1,p_lx0   7849
 
		! if(j_dapu.le.p_tiny78)then
		! p_zerob=p_zerob+1
		! write(6,*)'zeroob',j_dapu,p_pivot
		! endif !if(j_dapu.le.p_tiny78)   7855
		! endif !if(j_dapu.le.p_epsj)   7848
		! if(p_pivot.gt.6)then
		! write(6,*)'p_pivot,p_vcbef',p_pivot,p_vc
		! write(6,*)'obj',p_objr
		! endif !if(p_pivot.gt.6)   7860
		! if(p_pivot.ge.8)then
		! nout=6
		! write(6,*)'nout ',nout
		! else
		! nout=0
		! endif !if(p_pivot.ge.8)   7973
		if(p_p)write(6,*)'p_objftas',p_objf
		call tfbsub(p_nrow,p_a,p_lavec,0, &
			p_objr,p_vc,wslu1,lwsll1,p_apu,.false.)
		! if(p_pivot.eq.8)then
		! do i=1,p_nrow
		! write(18,*)'pivot ',p_pivot,'p_lr0',p_lr0,'ls',p_ls(i),' obj ',p_objr(p_ls(i))
		! if(p_ls(i).le.p_nrow)then
		! !j_o(ivmat)%d((p_ls(i)-1)*p_nrow+i)=j_1
		! write(18,*)'i',p_ls(i)
 
		! else
		! !	do j=1,p_nrow
		! write(18,*)(	p_a(j,p_ls(i)-p_nrow),j=1,p_nrow)
 
		! !	enddo !j=1,p_nrow    174
 
 
		! endif !if(p_ls(i).le.p_nrow)   8640
 
 
 
 
 
		! enddo !i=1,p_nrow   8638
		! write(18,*)'p_pivot,p_vce ',p_pivot,p_vc
		! call tfbsub(p_nrow,p_a,p_lavec,0, &
		! p_objr,p_vc,wslu1,lwsll1,p_apu,.false.)
		! write(6,*)'p_pivot,p_vctoka',p_pivot,p_vc
		! endif !if(p_pivot.eq.8)   7866
 
		!	endif !if(p_lr0.eq.p_nrow.and.p_feasible)   7849
		!	endif !if(maxval(abs(p_objr)).lt.p_epsj)   7822
		!	endif !if(j_dapu.le.p_tiny78)   7843
 
 
 
		!	endif !if(sparse)  17275
	endif !if(.true.)   8389
 
	! !is(p_p)write(p_n16,*)'vc:',p_vc(1:min(p_nrow,50))
	! !is(p_p)write(p_n16,*)'objf,objfv,tolecur:',p_objf,p_objfv,p_tolecur
	if(p_objf.le.p_objfv-100*p_tolecur.and.p_tmax.gt.j_0)then !100j_0*j_tolecur)then  ! 100 added Feb. 2011 cnhaged by JL 9.9.2018
		write(6,*)'***getting worse old ' ,p_objfv,' new ',p_objf,' tole ',p_tolecur,'tmax',p_tmax
		! write(6,*)'***rhs2',p_rhs2
		! write(6,*)'***ubou',p_ubou
 
		! write(6,*)'***rhs',p_rhs
		! write(6,*)'***lbou',p_lbou
		call pullout(LWSLL1)
		j_err=.true.
 
		return
	endif !if(p_objf.le.p_objfv-100*p_tolecur.and.p_tmax.gt.j_0)   8686
	! write(6,*)'round, p_pivot,objf,oldobjf: ', &
	! p_kier,p_pivot,p_objf,p_objfv,' tolecur=',p_tolecur
 
	! if(p_p) then
	! write(p_n16,*)'***getting worse'
 
	! write(p_n16,*)'round, p_pivot,objf,oldobjf: ', &
	! p_kier,p_pivot,p_objf,p_objfv,' tolecur=',p_tolecur
	! endif !if(p_p)   8681
 
	! j_err=.true.
	! return
	! p_kierv=p_kier
	! p_unitv=p_unit
	! p_feasible=.false.
	! p_nrecover=p_nrecover+1
	! if(p_nrecover.ge.20)then !10 changed into 20 by J.L 21.2.2019
	! write(6,*) &
	! '*jlp* is mixed up, try different tole (e.g.10,100,100)(or consult J. Lappi)'
	! j_err=.true.
	! p_goto900=.true.;return
	! endif !if(p_nrecover.ge.20)   8694
	! p_goto1234=.true.;return
	!endif !if(p_objf.le.p_objfv-p_tolecur)   9073
 
	!	endif !if(.true.)   8792
 
	return
end subroutine isfeasible !subroutine isfeasible()
 
subroutine updatejlp()
 
	use fletdmod
	use fletdmod2
	use fletcherdmod
	implicit none
	integer ii_,ilr,j,jj,jj7,nex
 
	p_goto900=.false.;p_goto1234=.false.;p_goto8888=.false.;p_goto8889=.false.
	! note 8888 and 8889 are in leaving(
	!if(p_pp)write(16,*)'p_ienter,p_newc,p_newa',p_ienter,p_newa,p_newc,j_tmax
 
 
 
	p_route67=.false.
 
	if(p_unit.eq.p_unit55)then
		p_niter=p_niter+1
		!is(p_p)write(p_n16,*)'Niter,p_unit',p_niter,p_unit
	else !if(p_unit.eq.p_unit55)then
		p_unit55=p_unit
		p_niter=0
		p_valiter=p_objf
		p_nnfiter=p_nnf
 
	endif !if(p_unit.eq.p_unit55)   8744
 
	!!!! compute values of basic variables *********************************
	!	if(sparse)then
	!		call fbsubsp(p_nrow,1,p_nrow,p_a,p_lavecsp,0,p_rhsw,p_x,p_ls,wslu1,&
	!			lwsll1,.false.)   !linux
 
	!	else !if(sparse)then
	! write(61,*)'nrow ',p_nrow
	! write(61,*)'a'
	! write(61,'(20f8.2/)')p_a
	! write(61,*)'lavec '
	! write(61,'(20i8/)')lavec
	! write(61,*)'rhsw'
	! write(61,'(20f8.2/)') p_rhsw
	! write(61,*) 'rx '
	! write(61,'(20f8.2/)')p_x
	! write(61,*)' ls '
	! write(61,'(20i8/)')p_ls
	! write(61,*)' wslu1 '
	! write(61,'(20f8.2/)')wslu1
	! write(61,*)' lws '
	if(p_p) write(6,*) 'besub ',lwsll1
	if(p_p) write(6,*) 'rhsw ',p_rhsw
 
	!write(17,*)p_pivot,p_pivotold
	call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,0,p_rhsw,p_x,p_ls,wslu1,&  !!!!
		lwsll1,.false.)    !linux
 
	if(p_p) write(6,*) 'rhsw ',p_rhsw
 
	!c  solves a system  B.x=b  here b=rhsw  B=a,  x=x
	!	endif !if(sparse)  17337
	p_ntote=p_ntote+1  !!!!
	!new 20.8.2018 JL ******************
	if(p_fpresent)then
		do ii_=p_mxd+1,p_lf0
			!write(p_n16,*) ii_,j_lf(ii_),j_lunit(j_lf(ii_)) ,j_ixkf(j_lf(ii_)),j_ixkffact(j_lf(ii_)),j_x(j_lf(ii_)+j_nrowz)
			if(p_x(p_lf(ii_)+p_nrowz).le.-0.0001)&
				write(6,*)'**negative amount ,pivot',p_pivot, &
				' unit ',p_unit, ' ixk ',p_ixkf(p_lf(ii_)),&
				' ifact ',p_ixkffact(p_lf(ii_)),' amount ',p_x(p_lf(ii_)+p_nrowz)
			!	write(16,*)'**negative amount ,pivot',p_pivot,' unit ',p_unit, ' ixk ',j_ixkf(j_lf(ii_)),&
			!	' ifact ',j_ixkffact(j_lf(ii_)),' amount ',j_x(j_lf(ii_)+j_nrowz)
		enddo !ii_=p_mxd+1,p_lf0   8789
 
 
	endif !if(p_fpresent)   8788
	!end new
 
 
 
 
	if(p_xpresent)then
		p_lcur=p_next(0)
		p_wsu=p_zero
		p_lcur0=p_lcur
		! go through in unit order
		do j=1,p_lx0   !nrow
			nex=p_next(p_lcur)
			!!!!! sum of weights in the same unit
			p_wsu=p_wsu+p_x(p_lcur+p_nrowz) !nrowz=nrow+nz  ! basis (first element -1) of D part, when I is included
			! negative value for weight 'noticeably' negative
			if(p_x(p_lcur+p_nrowz).lt.p_wminerr)then  ! jl 201606033 j_zeroneg)then    !negative weight, zeroneg=-0.000000001d0
				!***********
				write(6,*)'**illegal weight** ',p_x(p_lcur+p_nrowz),' unit ',p_unit
				close(17)
				j_err=.true. ;return
 
 
			endif !if(p_x(p_lcur+p_nrowz).lt.p_wminerr)   8816
 
 
			!***********
			! goto 1578 !!!! trying weak rocovery toimii huonosti, ohitetaan nyt 20160603
			! write(6,*)'***trying weak recovery' !!!!
			! p_listapu(1)=p_lcur+p_nrowz
			! p_vcmax=p_zero
			! p_cycling=.false.
			! !search for which entering residual  w changes most rapidly
			! do ilr=p_lr0+1,p_nrow
			! !  solves a system  B.x=b
			! !      subroutine fbsub(n,jmin,jmax,a,la,q,b,x,ls,aa,ll,save)
			! !
			! !   ls(*)  an index vector, listing the components of x that are required.
			! !       Only the absolute value of the elements of ls are used (this allows
			! !       the possibility of using of the contents of the ls parameter of bqpd).
			! !       Elements of x in the range abs(ls(j)), j=jmin:jmax are set by fbsub.
			! !       These contortions allow bqpd to be independent of the basis matrix code.
			! !       call fbsub(n,1,3,a,la,0,b,x,ls,ws(lu1),lws(ll1),.false.)
			! ! pitää testata voiko koko homman tehdä tällä, a(1,p_newa ei tarvita)
			! !			call fbsub(nrow,1,nrow,a,lavec,p_newc,a(1,p_newa),r, &
			! !    			 ls,ws(lu1),lws(ll1),.false.)
			! !c   q   an integer which, if in the range 1:n+m, specifies that the rhs vector
			! !c       b is to be column q of the matrix A of general constraint normals.
			! !c       In this case the parameter b is not referenced by fbsub.
			! ! if(sparse)then
			! ! call jlpgetcol(p_newa)
			! ! call fbsubsp(p_nrow,1,1,p_a,p_lavecsp,p_lr(ilr),p_acol,r, &
			! ! p_listapu,wslu1,lwsll1,.false.)   !linux
			! ! else !if(sparse)then
			! call fbsub(p_nrow,1,1,p_a,p_lavec,p_lr(ilr),p_a(1:,p_newa),r, &
			! p_listapu,wslu1,lwsll1,.false.)   !linux
 
			! !			endif !if(sparse)  17513
			! if(abs(r(p_lcur+p_nrowz)).gt.p_vcmax)then !cycling
			! if(p_lr(ilr).ne.p_icolold)then
			! p_vcmax=abs(r(p_lcur+p_nrowz))
			! p_ilrmax=ilr
			! p_cycling=.false.
			! else !if(j_lr(ilr).ne.p_icolold)then
			! p_cycling=.true.
			! endif !if(p_lr(ilr).ne.p_icolold)   8894
			! endif !if(abs(r(p_lcur+p_nrowz)).gt.p_vcmax)   8893
			! enddo !ilr=p_lr0+1,p_nrow   8868
			! if(p_vcmax.gt.p_tole(p_lr(p_ilrmax)))then
 
			! p_newc=p_lr(p_ilrmax)
			! p_leavec=p_lcur+p_nrowz
			! p_leave=p_lsi(p_leavec)
			! p_ienter=1
 
 
 
			! 1578	continue ! weak rocoveryn ohitus
			if(p_lunit(nex).ne.p_lunit(p_lcur))then
				if(p_wsu.gt.p_oneps)then
					if(p_wsu.gt.p_wmaxerr)then !p_oneps)then
						write(6,*)'**WSUM**',p_wsu,p_lunit(p_lcur),' pivot=',p_pivot  !!!!
						!if(p_p)write(p_n16,*)'**WSUM**',p_wsu,p_lunit(p_lcur),' pivot=',p_pivot
						j_err=.true.
						return
						!		  write(6,*)'prev',iprev78,iprev(p_lcur),next(0),
						! if(p_wsu.gt.p_wmaxerr)then
						! ! if(sparse)then
						! ! j_err=.true.
						! ! return
						! ! else !if(sparse)then
						! if(p_wsu.gt.p_wmaxwrn)then
						! p_nrecover=p_nrecover+1
						! if(p_nrecover.ge.20)then
						! write(6,*) &
						! '*jlp* is mixed up, try different tole (e.g. 10,100,1000) (or consult J. Lappi)'
						! j_err=.true.
						! p_goto900=.true.;return
 
						! endif !if(p_nrecover.ge.20)   8937
						! write(6,*)'***trying to recover'
						! if(p_p)write(p_n16,*)'***trying to recover'
						! p_kierv=p_kier
						! p_unitv=p_unit
						! p_feasible=.false.
						! p_goto1234=.true.;return
						! endif !if(p_wsu.gt.p_wmaxwrn)   8935
						!		endif !if(sparse)  17558
					endif !if(p_wsu.gt.p_wmaxerr)   8880
					p_listapu(1)=p_lcur0+p_nrowz
					p_vcmax=p_zero
					do ilr=p_lr0+1,p_nrow
						! if(sparse)then
						! call jlpgetcol(p_newa)
 
						! call fbsubsp(p_nrow,1,1,p_a,p_lavecsp,p_lr(ilr),p_acol,r, &
						! p_listapu,wslu1,lwsll1,.false.)   !linux
 
						! else !if(sparse)then
						!	call fbsub(p_nrow,1,1,p_a,p_lavec,p_lr(ilr),p_a(1:,p_newa),r, &
						call fbsub(p_nrow,1,1,p_a,p_lavec,p_lr(ilr),p_a(p_abas(p_newa)+1:),r, &
							p_listapu,wslu1,lwsll1,.false.)   !linux
 
						!	endif !if(sparse)  17583
						if(abs(r(p_lcur0+p_nrowz)).gt.p_vcmax)then
							if(p_lr(ilr).ne.p_icolold)then
								p_vcmax=abs(r(p_lcur0+p_nrowz))
								p_ilrmax=ilr
								p_cycling=.false.
							else !if(j_lr(ilr).ne.p_icolold)then
								p_cycling=.true.
							endif !if(p_lr(ilr).ne.p_icolold)   8925
						endif !if(abs(r(p_lcur0+p_nrowz)).gt.p_vcmax)   8924
 
					enddo !ilr=p_lr0+1,p_nrow   8911
					if(p_vcmax.gt.p_tole(p_lr(p_ilrmax)))then
						p_newc=p_lr(p_ilrmax)
						p_leavec=p_lcur0+p_nrowz
						p_leave=p_lsi(p_leavec)
						p_leavk=p_lcur0
						p_ienter=1
 
						! if(p_p.or.p_p9) write(p_n16,*) &
						! '*nyt mennään,p_newc,p_leavec,vcmax',p_newc,p_leavec,p_vcmax,p_pivot
						! if(p_p.and.p_fpresent) write(p_n16,*)'**fact** KANTAAN >> residual, p_ienter = 1'
 
						p_goto8889=.true.;return
					else !if(j_vcmax.gt.j_tole(j_lr(p_ilrmax)))then
						write(6,*)'did not remove w, was this due to p_cycling: ',p_cycling
					endif !if(p_vcmax.gt.p_tole(p_lr(p_ilrmax)))   8935
 
				endif !if(p_wsu.gt.p_oneps)   8879
 
				p_wsu=p_zero
 
				p_lcur0=p_next(p_lcur)
 
			endif !if(p_lunit(nex).ne.p_lunit(p_lcur))   8878
 
			p_lcur=nex
		enddo !j=1,p_lx0   8811
 
	endif !if(p_xpresent)   8806
 
	!!!!! check feasiblity , i.e. if residuals get legal values
	! note as opposite in old JLp, we concider here if residual should be maximized or minimized
	! as in old jlp the row was maximized or minimize
	! it may happen that an feasible solution becomes again infeasible /due to rounding errors)
	! thus infeasibility must be checked repeatedly
	! now this is done after each pivot operation, earlier once in each round
	! over units
 
 
	call isfeasible()
	if(j_err)return
	if(p_goto900)return
	if(p_goto1234)return
 
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
 
	! p_ienter=1 residual eneters
	! p_ienter=2   z enters
	! p_ienter=3 schedules enters
	!*********************************************************
 
	!jatketaanko seuraavasta tehdasmjasta tarkastelu  !!!!
	if (p_fpresent) then
		!	p_nextxkf = ((p_ienter==4).and.(ixkenter<p_nlog)) !<A>  <a>
		! pakotetaan käymään läpi yksikön tehdasmjat sen jälkeen kun kantaan on tullut vaihtoehto tai avainvaihtoehto vaihtunut
		p_nextxkf = p_nextxkf.or.(p_ienter==3)
	endif !if (p_fpresent)   9012
 
end subroutine updatejlp !subroutine updatejlp()
 
subroutine repo(nureport)
	!	write(6,*)'<67>j_nshow',p_nrow,p_nshow
	j_v(p_ivobjective)=p_coefmax*p_objf
	! else !if(j_maxo) the
	! if(p_maxo) then
	! j_v(p_ivobjective)=p_objf
	! else !if(j_maxo) then
	! j_v(p_ivobjective)=-p_objf
	! endif !if(p_maxo)   7868
	! if(j_ivout.ne.j_ivresult) then
	! call j_getobject(j_ivout,'%objective',j_ipreal,p_ivobjective2)
	! j_v(p_ivobjective2)=j_v(p_ivobjective)
	! call j_getname(p_ivobjective2)
	! write(6,*)j_oname(1:j_loname),'=',j_v(p_ivobjective2)
	! endif !if(j_ivout.ne.j_ivresult)   9095
901	continue

	p_issolution=.true.
	if(p_nrow.gt.0)then  !there are constraints
		do i=p_nrow+1,p_ncol+p_nrow
			p_x(p_ls(i))=j_0
			!	if(p_ls(i).eq.2)write(6,*)'<4774putzero'
		end do !i=p_nrow+1,p_ncol+p_nrow   9039
		if(.not.p_maxo)then  ! poistettu bugia metsästettäessä
			p_vc=-p_vc                        !;objf=-objf done earlier
			p_vx=-p_vx
 
		end if !if(.not.p_maxo)   9043
 
	endif !if(p_nrow.gt.0)   9038
	if(.not.p_maxo)p_objr0=-p_objr0
 
 
877 continue !nrow=0

	! if(p_nz.gt.0) then
		! p_redcost=j_0  !vois allokoida täs
		! do i=p_lz0+1,p_nz

			! p_newa=p_lz(i)

			! if(p_feasible)then
				! p_val_=p_objr0(p_newa+p_nrow)  ! in objr all cols are counted
			! else !if(j_feasible)then
				! p_val_=j_0
			! endif !if(p_feasible)   9185

			! do  j=1,p_nrow
				! p_val_=p_val_-p_vc(j)*p_a(j+p_abas(p_newa))  !p_a(j,p_newa)

			! enddo ! j=1,p_nrow   9191
			! p_redcost(p_newa)=abs(p_val_)
		! enddo !i=p_lz0+1,p_nz   9181
	! endif !if(p_nz.gt.0)   9179
	!if(j_err)return

	!if(j_ivout.ne.j_ivresult) then
	! if (p_nz>0) then
	! !write(6,*)'<p_nz',p_nz
	! call j_defmatrix(j_ivout,'%zvalues',1,p_nz,j_matreg,ivout)
	! j_o(ivout)%d(p_lz(1:p_lz0))=p_x(p_nrow+p_lz(1:p_lz0))

	! call j_defmatrix(j_ivout,'%redcost',1,p_nz,j_matreg,ivout)

	! j_o(ivout)%d(1:p_nz)=p_redcost(1:p_nz)
	! endif !if (p_nz>0)   9201

	if(p_nrow>0.and.p_nz.eq.0) then
		!write(6,*)'<nrow',j_nrow
		ivout=j_defmatrix(j_ivout,'%rows',p_nrow,1,j_matreg)
		j_o(ivout)%d(1:p_nrow)=p_rhscur(1:p_nrow)-p_x(1:p_nrow)
 
		ivout=j_defmatrix(j_ivout,'%shprice',p_nrow,1,j_matreg)
		j_o(ivout)%d(1:p_nrow)=p_vc(1:p_nrow)
	endif !if(p_nrow>0.and.p_nz.eq.0)   9087
	!	endif !if(j_ivout.ne.j_ivresult)   9200
	!write(6,*)'<47p_zmatrix',p_zmatrix
	! if (p_zmatrix)then
	! write(6,*)' '
	! write(6,*)'***because there was zmatrix->, results are given in matrices***'
	! write(6,*)' '
	! goto 8000
	! endif !if (p_zmatrix)   9221
 
	!	irow0=0
	irow=0
	idom=1 !counter for domain statements
	idomv=-1
	!write(6,*)'<467p_iprint',p_iprint
	if(p_iprint.lt.1)goto 8000
	!title
 
 
	!		call j_printtitle(nureport, p_ivprob)
	!	write(6,*)'p_row0',p_row0,p_rowdomnum,'units',p_domainunits
	!i0=1
	!if(p_isobjective)i0=0
	!	i0=0
	!	ist=0
	!if(p_isobjective)ist=1
	call j_getname(-1,-1,j_ivout)
	do irow=0,p_nrow
		!			domloop: do j=1,p_nsetd(i)
		!	write(6,*)'<5553>',jirow
		irowtot=irow+1
		if(p_xpresent.and.p_isdomain)then
			idom=p_rowdomnum(irow)
 
 
 
 
		endif !if(p_xpresent.and.p_isdomain)   9125
		if(idom.ne.idomv)then
			!unpublished
			!	p_buf='DOMAIN:      All'
			if(p_isdomain)then
				p_buf=' '
				call j_getline(p_ivdomain,idom,p_domainname,p_ldomainname)
 
				!	call j_getline(p_ivdomain,idom,p_buf(8:),le)
				p_buf(8:)=p_domainname(1:p_ldomainname)
				p_buf(74:78)='units'
				p_buf(68:72)=j_chi5(p_domainunits(idom),0)
			else
				p_domainname='All'
				p_ldomainname=3
				p_buf(74:78)='units'
				p_buf(68:72)=j_chi5(p_nunits,0)
 
			endif !if(p_isdomain)   9135
			write(nureport,*)' ',('_',kk=1,78)
			write(nureport,'(a)')p_buf(1:79)
 
		endif !if(idom.ne.idomv)   9132
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
		endif !if(idom.ne.idomv)   9155
		!	do k=1,p_nsetr(i)
		!write(6,*)'<458 k,nsetr(i) ',k,j_nsetr(i)
		!		if((k==1).and.(j>1)) irow0 = irow0 - p_nsetr(i)
		!			irow0 = irow0+1
		call writerow(nureport,irow)
 
		if(p_intapp)then
			p_buf=' '
 
			p_buf(36:)=j_chr10(p_solx(irow))
			write(nureport,'(a)')p_buf(1:79)
		endif !if(p_intapp)   9172
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
				! write(6,*)'rowdomnum',p_rowdomnum
				! write(6,*)'ibatemp',p_ibatemp
				! write(6,*)'ix',p_ix
				! !	if(p_isdomain)idom=1
				!	write(6,*)'idom ',idom,p_intapp,p_sumxi
 
				!		write(6,*)'ibatemp',p_ibatemp
				!		write(6,*)'xcoef',p_xcoef
				!		write(6,*)'termvar',termvar
				call j_getname(p_ivvarx)
 
				do jx=1,p_nxvar  !p_nxvartot !+p_noutsubtrans !p_ncvar,available variables
					!			write(6,*)'jx ',jx,p_nxvartot+p_ncvar+p_noutsubtrans
					!	jx=j_inlistobject(j_o(p_ivvarx)%i2(jx0),p_ivkeepx)
 
					ivxvar=j_o(p_ivvarx)%i2(jx)
 
					ixkeep=j_inlistobject(ivxvar,p_ivkeepx)
 
					call j_getname(ivxvar)
					!		write(6,*)'**',j_oname(1:j_loname)
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
 
								! if(jx.le.p_nxvartot)then
								! p_yes=p_irowvarx(iba+ii).eq.j_o(p_ivkeepx)%i2(jx)
								! !x-variable
								! ! elseif(jx.le.p_nxvartot+p_ncvar) then !if(jx.le.j_nxvartot)then
								! ! !c-variable
								! ! p_yes=p_irowvarx(iba+ii).eq.p_cvarl(jx-p_nxvartot)
								! else !if(jx.le.j_nxvartot)then
								! p_yes=p_irowvarx(iba+ii).eq. &
								! j_o(p_ivoutsubtrans)%i2(jx-p_nxvartot)  !-p_ncvar)
								! !output from transformation
								! endif !if(jx.le.p_nxvartot)   9408
								!	if(p_yes)then   !häär
								!			write(6,*)'ivxvar p_irowvarx(iba+ii)',ivxvar,p_irowvarx(iba+ii)
								if(ivxvar.eq.p_irowvarx(iba+ii))then
									if(iro.eq.0)then
										!		p_xmat(i+ibxmatx)= &       ! make temporary variables
										!	dot_product(p_xcoef(iba+1:iba+p_nrowxtemp(i)), &
										!	j_v(p_irowvarx(iba+1:iba+p_nrowxtemp(i))))
 
 
 
										shp=p_xcoef(iba+ii)
										!				write(6,*)' shp0 ',shp,'iba ',iba,ii,item
										indi=1
 
									else !if(iro.eq.0)then
										!						write(6,*)'iro,ii,p_xcoef(iba+ii),p_vc(iro) ',iro,ii,p_xcoef(iba+ii),p_vc(iro)
 
										shp=shp-p_xcoef(iba+ii)*p_vc(iro)
										indi=1
									endif !if(iro.eq.0)   9247
								endif !if(ivxvar.eq.p_irowvarx(iba+ii))   9246
								!endif !if(p_yes)   9419
							enddo !ii=1,p_nrowx2(item)   9231
 
						endif !if(j_yes.and.p_ix(iro).ne.0)   9225
 
					enddo !iro=0,p_nrow   9218
					!	if(jx.le.p_nxvartot)then
 
					!	le=j_lename(j_o(p_ivkeepx)%i2(jx))
 
					!j_vname(j_o(p_ivkeepx)%i2(jx))
					!tulos muuttujiin
					if(.not.p_isdomain) j_v(ivxvar)=p_sumx(ixkeep)
 
					! elseif(jx.le.p_nxvartot+p_ncvar) then !if(jx.le.j_nxvartot)then
					! le=j_lename(p_cvarl(jx-p_nxvartot))
					! p_buf(6:)=j_vname(p_cvarl(jx-p_nxvartot))
					! p_buf(1:1)='C'
 
					! if(.not.p_isdomain) j_v(p_cvarl(jx-p_nxvartot))=p_sumx(jx)
					! else !if(jx.le.j_nxvartot)then
					! le=j_lename(j_o(p_ivoutsubtrans)%i2(jx-p_nxvartot)) !-p_ncvar))
					! p_buf(6:) = j_vname(j_o(p_ivoutsubtrans)%i2(jx-p_nxvartot)) !-p_ncvar))
					! !tulos muuttujiin
					! if(.not.p_isdomain) j_v(j_o(p_ivoutsubtrans)%i2(jx-p_nxvartot))= & !-p_ncvar))= &
					! p_sumx(jx)
					! endif !if(jx.le.p_nxvartot)   9434
 
					if(j_loname.lt.26)p_buf(6+j_loname:34)=p_dots
					p_buf(36:46)=j_chr10(p_sumx((idom-1)*p_nsumx+ixkeep))
					if(p_nrow.gt.0)then
						if(indi.ne.0)p_buf(47:56)=j_chr10(dble(shp))
						p_shpx((idom-1)*p_nsumx+ixkeep)=shp
						if(p_intapp)	p_buf(67:76)=j_chr10(p_sumxi((idom-1)*p_nsumx+ixkeep)-p_sumx((idom-1)* &
							p_nsumx+ixkeep))
					endif !if(p_nrow.gt.0)   9295
					write(nureport,'(a)')p_buf(1:79)
 
				end do !jx=1,p_nxvar   9203
				write(nureport,*)' '
				do jx=1,p_nxvartot
					jxv=j_o(p_ivkeepx)%i2(jx)
					call j_getname(jxv)
 
					ipe=j_inlistobject(jxv,p_ivvarx)
					!	write(6,*)'jx',jx,jxv,ipe,j_oname(1:j_loname)
					if(p_domainname(1:p_ldomainname).eq.'All')then
 
						ivoo=j_getobject(0,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname),j_ipreal)
						!    call j_getobject(jxv,'['//p_domainname(1:p_ldomainname)//']',j_ipreal,ivoo)
						!		j_v(jxv)=p_sumx((idom-1)*p_nsumx+jx)
					else
						ivoo=j_getobject(0,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname)// &
							'['//p_domainname(1:p_ldomainname)//']',j_ipreal)
						!	call j_getobject(jxv,'['//p_domainname(1:p_ldomainname)//']',j_ipreal,ivoo)
						!	j_v(ivoo)=p_sumx((idom-1)*p_nsumx+jx)
						!	call j_getname(ivoo)
						!	write(6,*)j_oname(1:j_loname),' =',j_v(ivoo)
					endif !if(p_domainname(1:p_ldomainname).eq.'All')   9311
					j_v(ivoo)=p_sumx((idom-1)*p_nsumx+jx)
 
					if(ipe.gt.0)cycle
					call j_getname(jxv)
					p_buf=' '
 
					p_buf(6:)=j_oname(1:j_loname)
					if(j_loname.lt.26)p_buf(6+j_loname:34)=p_dots
					p_buf(36:46)=j_chr10(j_v(ivoo))  ! p_sumx((idom-1)*p_nsumx+jx))
					write(nureport,'(a)')p_buf(1:79)
 
				enddo !jx=1,p_nxvartot   9305
 
 
			endif !if(p_iprint.ge.1)   9183
		end if !if(idom2.ne.idom.or.irow.eq.p_nrow)   9181
 
 
		!		end do domloop !domloop: do j=1,j_nsetd(i)
		idomv=idom
	end do !irow=0,p_nrow   9121
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
			do jx=1,p_nxvartot
				jxv=j_o(p_ivkeepx)%i2(jx)
				ipe=j_inlistobject(jxv,p_ivvarx)
				call j_getname(jxv)
				if(p_domainname(1:p_ldomainname).eq.'All')then
					ivoo=j_getobject(0,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname),j_ipreal)
					!	j_v(jxv)=p_sumx((idom-1)*p_nsumx+jx)
				else
					ivoo=j_getobject(0,j_oname3(1:j_loname3)//'%'//j_oname(1:j_loname)// &
						'['//p_domainname(1:p_ldomainname)//']',j_ipreal)
					!	call j_getobject(jxv,'['//p_domainname(1:p_ldomainname)//']',j_ipreal,ivoo)
					j_v(ivoo)=p_sumx((idom-1)*p_nsumx+jx)
					!	call j_getname(ivoo)
					!	write(6,*)j_oname(1:j_loname),' =',j_v(ivoo)
				endif !if(p_domainname(1:p_ldomainname).eq.'All')   9363
				!	if(ipe.gt.0)cycle
				j_v(ivoo)=p_sumx((idom-1)*p_nsumx+jx)
 
				p_buf=' '
 
				p_buf(6:)=j_oname(1:j_loname)
				if(j_loname.lt.26)p_buf(6+j_loname:34)=p_dots
				p_buf(36:46)=j_chr10(j_v(ivoo))  !p_sumx((idom-1)*p_nsumx+jx))
				write(nureport,'(a)')p_buf(1:79)
 
			enddo !jx=1,p_nxvartot   9359
 
 
 
 
		enddo !j=1,p_nshow   9350
	endif !if(p_nshow.gt.0)   9345
	!	p_nxvartot=j_o(p_ivkeepx)%i(1)
	!p_nsumx=p_nxvartot+p_ncvar+p_noutsubtrans
	!varx%, xsum%, xprice%,varxproblem%
	!if(p_xpresent.and.j_ivout.ne.j_ivresult) then
	!	call j_deflist(j_ivout,'%varx',ivvarxtot,nres=p_nsumx)
	! call j_deflist(j_ivout,'%varx',ivvarxtot,list0=p_nxvartot, &
	! list=j_o(p_ivkeepx)%i2(1:p_nxvartot),nres=p_noutsubtrans)  !p_ncvar+p_noutsubtrans)
 
	!j_o(ivvarxtot)%i(1:p_nxvartot)=j_o(p_ivkeepx)%i(1:p_nxvartot)
	!	if (p_ncvar > 0) j_o(ivvarxtot)%i2(p_nxvartot+1:p_nxvartot+p_ncvar) =  &
	!		p_cvarl(1:p_ncvar)
	! if (p_noutsubtrans > 0) &
	! !	j_o(ivvarxtot)%i2(p_nxvartot+p_ncvar+1:p_nxvartot+p_ncvar+p_noutsubtrans) =  &
	! j_o(ivvarxtot)%i2(p_nxvartot+1:p_nxvartot+p_noutsubtrans) =  &
	! j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans)
	! j_o(ivvarxtot)%i(1) = 	p_nsumx
	! call j_deflist(j_ivout,'%varxproblem',ivout,list0=p_nxvar,list=p_varx)
 
	! j_o(ivout)%i(1:nxvar)=p_varx
	! j_o(ivout)%i(1)=nxvar
	ndom_=max(p_ndom,1)
	!write(6,*)'j_nsumx',j_nsumx
	ivout=j_defmatrix(j_ivout,'%xsum',ndom_,p_nsumx,j_matreg)
 
	j_o(ivout)%d(1:ndom_*p_nsumx)=p_sumx(1:ndom_*p_nsumx)
	if(p_intapp)then
		ivout=j_defmatrix(j_ivout,'%xsumint',ndom_,p_nsumx,j_matreg)
		j_o(ivout)%d(1:ndom_*p_nsumx)=p_sumxi(1:ndom_*p_nsumx)
	endif !if(p_intapp)   9416
 
	if(p_nrow.gt.0)then
		ivout=j_defmatrix(j_ivout,'%xprice',ndom_,p_nsumx,j_matreg)
 
		j_o(ivout)%d(1:ndom_*p_nsumx)=p_shpx(1:ndom_*p_nsumx)
	endif !if(p_nrow.gt.0)   9421
 
	if(p_nrow.gt.0.and.p_isdomain)then
		ivout=j_defmatrix(j_ivout,'%domains',p_nrow,1,j_matreg)
 
		!			j_o(ivout)%d = p_irowdomain(1:p_nrow)
 
	endif !if(p_nrow.gt.0.and.p_isdomain)   9427
	!	endif !if(p_xpresent.and.j_ivout.ne.j_ivresult)   9402
	!	if(j_ivout.ne.j_ivresult)then
	!call j_defmatrix(j_ivout,'%problemrows',p_nrow,1,j_matreg,ivout)
 
	!	j_o(ivout)%d = p_irowrow(1:p_nrow)
	! if(p_nrow.gt.0)then
	! ! call j_defmatrix(j_ivout,'%rhs',p_nrow,1,j_matreg,ivout)
 
	! ! j_o(ivout)%d = p_rhs
	! ! call j_defmatrix(j_ivout,'%rhs2',p_nrow,1,j_matreg,ivout2_)
 
	! ! j_o(ivout2_)%d = p_rhs2
	! ! do i_=1,p_nrow
	! ! if(p_rhs(i_).eq.j_ninf)j_o(ivout)%d(i_) =j_ninf
	! ! if(p_rhs2(i_).eq.j_inf)j_o(ivout2_)%d(i_) =j_inf
	! ! enddo !i_=1,p_nrow   8170
 
	! call j_defmatrix(j_ivout,'%shprice',p_nrow,1,j_matreg,ivout2_)
	! j_o(ivout2_)%d = p_vc(1:p_nrow)
	! endif !if(p_nrow.gt.0)   9443
 
	!	endif !if(j_ivout.ne.j_ivresult)   9439
 
	if(p_nz.gt.0)call repoz(p_nureport)
 
 
	! if(p_nz.gt.0.and.p_iprint.ge.1)then
	! write(nureport,*)' '
	! write(nureport,*)'    ',('_',kk=1,46)
	! p_buf='z-variable'
	! p_buf(25:30)='value'
	! p_buf(36:48)='reduced cost'
	! write(nureport,'(a)')'     '//p_buf(1:79)
	! write(nureport,*)'    ',('_',kk=1,46)
	! DO 66778 I=1,p_nz
	! p_buf=j_vname(p_zvars(i))
	! le=j_lename(p_zvars(i))
	! if(le.lt.20)p_buf(le+2:21)=p_dots
	! p_buf(23:33)=j_chr10(dble(p_x(p_nrow+i)))
	! j_v(p_zvars(i))=p_x(p_nrow+i)
	! p_buf(36:46)=j_chr10(p_redcost(i))
	! write(nureport,'(a)')'     '//p_buf(1:46)
	! 66778 continue !66778 I=1,p_nz   9471
	! endif !if(p_nz.gt.0.and.p_iprint.ge.1)   9463
	write(nureport,*)' '
8000 continue  !end printing


	!if(nureport.ne.6)call j_closeunit(nureport)
end subroutine repo !subroutine repo(nureport)
 
subroutine repoepilog(nureport,isx)
	logical isx
	write(nureport,*)('_',kk=1,79)
 
	if(j_v(p_ivunbounded)>0) then
		p_buf='Unbounded problem'
		write(nureport,'(a)')p_buf(1:79)
	endif !if(j_v(p_ivunbounded)>0)   9488
 
	write(nureport,"('Pivots: ',I10,' refactorizations ',i5,' rounds ',i5)")&
		p_pivot,max(0,p_refac-1),p_kier
 
	if(j_v(p_ivfeasible)>0) then
			p_buf='Value of the objective function:  '
	else !if(j_v(p_ivfeasible)>0) then
			p_buf='Value of the temporary objective: '
	endif !if(j_v(p_ivfeasible)>0)   9496
	write(p_buf(35:),*)p_coefmax*p_objf
	!	p_buf(35:)=j_chr10(p_coefmax*p_objf)
	write(nureport,'(a)')p_buf(1:79)
 
	if(j_v(p_ivfeasible)>0) then
		p_buf='Solution is feasible'
	else !if(j_v(p_ivfeasible)>0) then
		p_buf='Solution is infeasible'
	endif !if(j_v(p_ivfeasible)>0)   9505
	write(nureport,'(a)')p_buf(1:79)
	!write(6,*)'OBJECTIVE&&&&& ',p_objf,j_v(p_ivobjective2)
	!	call j_getname(p_ivobjective2)
	!write(6,*)'%%%',j_oname(1:j_loname)
	if(j_v(p_ivoptimal)>0) then
		if(p_kier.ge.p_maxrounds.and.isx)then
			p_buf='Solution may not be optimal, maximum number of iterations reached'
			! elseif(isslow)then !if(kier.ge.p_mxiter)then
			! j_buf='Solution is close to optimal (slow improvement)'
		else !if(kier.ge.p_mxiter)then
			p_buf='Solution is optimal'
		endif !if(p_kier.ge.p_maxrounds.and.isx)   9515
	else !if(j_v(p_ivoptimal)>0) then
		p_buf='Solution is not optimal'
	endif !if(j_v(p_ivoptimal)>0)   9514
	write(nureport,'(a)')p_buf(1:79)
	if(nureport.ne.6)write(6,*)'** report-> file remains open'
	return
 
 
end subroutine repoepilog
 
subroutine repoz(nureport)
	!write(6,*)'<67>p_nrow',p_nrow
	!if(p_maxo) then
	j_v(p_ivobjective)=p_coefmax*p_objf
	! else !if(j_maxo) then
	! j_v(p_ivobjective)=-p_objf
	! p_vc=-p_vc                        !;objf=-objf ei oltu
	! !	write(6,*)'objrbef',p_objr0
	! p_objr0=-p_objr0
	! endif !if(p_maxo)   9524
	write(nureport,*)' '
	! if(j_ivout.ne.j_ivresult) then
	! call j_getobject(j_ivout,'%objective',j_ipreal,p_ivobjective2)
	! j_v(p_ivobjective2)=j_v(p_ivobjective)
	write(6,*)'OBJECTIVE ',j_v(p_ivobjective)
	! endif !if(j_ivout.ne.j_ivresult)   9530
901	continue
	write(nureport,*)' '
	p_issolution=.true.
	!if(p_nrow.gt.0)then  !there are constraints
	do i=p_nrow+1,p_ncol+p_nrow
		p_x(p_ls(i))=j_0
		!	if(p_ls(i).eq.2)write(6,*)'<4774putzero'
	end do !i=p_nrow+1,p_ncol+p_nrow   9552
	!endif !if(p_nrow.gt.0)   9538
 
	!	p_vx=-p_vx
 
 
877 continue !nrow=0

!	if(p_nz.gt.0) then
	p_redcost=j_0 !
 
	do i=p_lz0+1,p_nz
 
		p_newa=p_lz(i)
		! write(6,*)'i ',i,p_newa,' objr0 ',p_objr0, 'pnewa',p_newa
		! write(6,*)'vc',p_vc
 
		if(p_feasible)then
			p_val_=p_objr0(p_newa+p_nrow)  ! in objr all cols are counted
		else !if(j_feasible)then
			p_val_=j_0
		endif !if(p_feasible)   9572
		!	write(6,*)'val ',p_val
 
		do  j=1,p_nrow
			!		write(6,*)'row ',j,p_vc(j),p_a(j+p_abas(p_newa))
			p_val_=p_val_-p_vc(j)*p_a(j+p_abas(p_newa))  !p_a(j,p_newa)
 
		enddo ! j=1,p_nrow   9579
 
		p_redcost(p_newa)=abs(p_val_)
		if(p_redcost(p_newa).lt.1.d-70)p_redcost(p_newa)=j_0
 
	enddo !i=p_lz0+1,p_nz   9566
	!endif !if(p_nz.gt.0)   7864
	!if(j_err)return
 
	!	if(j_ivout.ne.j_ivresult) then
	!	if (p_nz>0) then
	!write(6,*)'<p_nz',p_nz
	ivzval=j_defmatrix(j_ivout,'%zvalues',1,p_nz,j_matreg)
	j_o(ivzval)%d(p_lz(1:p_lz0))=p_x(p_nrow+p_lz(1:p_lz0))
 
	ivredcost=j_defmatrix(j_ivout,'%redcost',1,p_nz,j_matreg)
 
	j_o(ivredcost)%d(1:p_nz)=p_redcost(1:p_nz)
	! endif !if (p_nz>0)   7883
 
	!	if(p_nrow>0) then
	!write(6,*)'<nrow',j_nrow
	ivrows=j_defmatrix(j_ivout,'%rows',p_nrow,1,j_matreg)
	j_o(ivrows)%d(1:p_nrow)=p_rhscur(1:p_nrow)-p_x(1:p_nrow)
 
	ivshp=j_defmatrix(j_ivout,'%shprice',p_nrow,1,j_matreg)
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
		call j_getname(ivredcost,p_ivobjective)
		write(6,*)j_oname(1:j_loname),' = vector of reduces costs of variables'
		write(6,*)j_oname2(1:j_loname2),' = value of the objective'
		write(6,*)' '
		goto 8000
	endif !if (p_zmatrix)   9613
 
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
		call writerow(nureport,irow)
 
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
	end do !irow=0,p_nrow   9646
 
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
 
 
	!if(p_nz.gt.0.and.p_iprint.ge.1)then
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
66778 continue !66778 I=1,p_nz   9698
	!endif !if(p_nz.gt.0.and.p_iprint.ge.1)   9674
	write(nureport,*)' '
8000 continue  !end printing

	call repoepilog(nureport,.false.)
 
	! write(nureport,*)('_',kk=1,79)
 
	! if(j_v(p_ivunbounded)>0) then
	! p_buf='Unbounded problem'
	! write(nureport,'(a)')p_buf(1:79)
	! endif !if(j_v(p_ivunbounded)>0)   8588
 
	! write(nureport,"('Pivots: ',I10,' refactorizations ',i5,' rounds ',i5)")&
	! p_pivot,max(0,p_refac-1),p_kier
 
	! if(j_v(p_ivfeasible)>0) then
	! p_buf='Value of the objective function:  '
	! else !if(j_v(p_ivfeasible)>0) then
	! p_buf='Value of the temporary objective: '
	! endif !if(j_v(p_ivfeasible)>0)   8596
	! p_buf(35:)=j_chr10(j_v(p_ivobjective))
	! write(nureport,'(a)')p_buf(1:79)
 
	! if(j_v(p_ivfeasible)>0) then
	! p_buf='Solution is feasible'
	! else !if(j_v(p_ivfeasible)>0) then
	! p_buf='Solution is infeasible'
	! endif !if(j_v(p_ivfeasible)>0)   8604
	! write(nureport,'(a)')p_buf(1:79)
	! !write(6,*)'OBJECTIVE&&&&& ',p_objf,j_v(p_ivobjective2)
	! !	call j_getname(p_ivobjective2)
	! !write(6,*)'%%%',j_oname(1:j_loname)
	! if(j_v(p_ivoptimal)>0) then
	! !	if(p_kier.ge.p_mxiter)then
	! !		p_buf='Solution may not be optimal, maximum number of iterations reached'
	! ! elseif(isslow)then !if(kier.ge.p_mxiter)then
	! ! j_buf='Solution is close to optimal (slow improvement)'
	! !	else !if(kier.ge.p_mxiter)then
	! p_buf='Solution is optimal'
	! !endif !if(p_kier.ge.p_mxiter)   8618
	! else !if(j_v(p_ivoptimal)>0) then
	! p_buf='Solution is not optimal'
	! endif !if(j_v(p_ivoptimal)>0)   8613
	! write(nureport,'(a)')p_buf(1:79)
	! if(nureport.ne.6)write(6,*)'** report-> file remains open'
	! return
	!if(nureport.ne.6)call j_closeunit(nureport)
end subroutine repoz !subroutine repo(nureport)
 
subroutine writerow(nureport,irow)
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
 
 
		if(j_v(p_ivfeasible)>0)then
 
			p_buf=p_buf(ip+2:ip+5)//' '//p_buf(1:ip-1)
			le=ip+4
			!	write(nureport,*)p_buf(1:le)
			!		p_buf=' '
			! if(p_maxo)then
			! p_buf(1:5)=' max'
			! else !if(j_maxo)then
			! p_buf(1:5)=' min'
			! end if !if(p_maxo)   8392
		else !if(j_v(p_ivfeasible)>0)then
			p_buf(1:5)=' '
			p_buf(6:33)='Infeasible, temporary objective'
			le=33
			!	le=
		endif !if(j_v(p_ivfeasible)>0)   9777
 
	endif !if(irow.gt.0)   9768
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
		enddo !while(iplus.le.34.and.iplus.le.le)   9803
		if(iplus.gt.le)then
 
			exit
		else
			write(nureport,'(a)')p_buf(1:iplusv)
			p_buf=p_buf(iplusv+1:le)
			le=le-iplusv
 
		endif !if(iplus.gt.le)   9807
	enddo !while(le.gt.34)   9799
	p_buf(le+1:34)=p_dots
 
	if(irow.ge.1)then
		!constraint row
		!			irow=irow+1
		!		j_apubuf=j_chi5(irow,0); j_buf(1:3)=j_apubuf(3:5);j_buf(4:5)=') '
 
		p_value=p_rhscur(irow)-p_x(irow)
		p_buf(35:35)=' '
		p_buf(36:)=j_chr10(p_value)
		p_buf(47:57)=j_chr10(p_coefmax*p_vc(irow))
		!	write(6,*)'shp',j_vc(irow)
		if(p_vc(irow).ne.0.)then
			if(p_maxo.eqv.p_vc(irow).gt.j_0)p_buf(78:78)='L'
			if(p_maxo.eqv.p_vc(irow).lt.j_0)p_buf(78:78)='U'
		end if !if(p_vc(irow).ne.0.)   9829
 
		if(p_rhs(irow).eq.p_rhs2(irow))then
			p_buf(65:72)= j_chr10(dble(p_rhs(irow)))
		else !if(p_rhs(irow).eq.p_rhs2(irow))then
			if(p_lbou(irow))p_buf(60:67)= j_chr10(dble(p_rhs(irow)))
			if(p_ubou(irow))p_buf(69:76)= j_chr10(dble(p_rhs2(irow)))
 
		end if !if(p_rhs(irow).eq.p_rhs2(irow))   9834
 
	else
		! for maximization rhs1 =huge  rhs2=0
		! for minimization  rhs2=-huge
		p_buf(34:35)=' '
		!	write(6,*)'pcoefm',p_coefmax,p_objf
		write(p_buf(36:),*)p_coefmax*p_objf
		!	p_buf(36:)=j_chr10(p_coefmax*p_objf)
 
		! if(j_v(p_ivfeasible)>0)then
		! if(j_maxo)then
		! j_buf(1:5)=' max'
		! else !if(j_maxo)then
		! j_buf(1:5)=' min'
		! end if !if(j_maxo)then
		! else !if(j_v(p_ivfeasible)>0)then
		! j_buf(1:5)=' '
		! j_buf(6:33)='Infeasible, temporary object'
		! endif !if(j_v(p_ivfeasible)>0)then
	end if !if(irow.ge.1)   9819
	if(le.lt.35)p_buf(le+1:34)=p_dots
	write(nureport,'(a)')p_buf(1:79)
	if(irow.eq.0)	write(nureport,'(a)')' '
 
	!	write(nureport,'(a)')'ssw'//p_buf(1:79)
 
end subroutine writerow
 
subroutine renter()
	use fletdmod
	use fletdmod2
	use fletcherdmod
	!goto35=.false.;
	p_goto36=.false.
	p_post=.true.
	!is(p_p)write(p_n16,*)'start entering'  !!!! <B>
	if(p_niter.ge.10)then  !!!!
		!after addrees 55:p_niter=0
 
		if(p_nnf.eq.p_nnfiter.and.p_objf-p_valiter.le.p_tolecur)then
			!is(p_p.or.p_p9)write(p_n16,*)'Niter,p_unit,p_pivot,objf,old', &
			!p_niter,p_unit,p_pivot,p_objf,p_valiter
			p_goto36=.true.;return   !!!! bypassresidual enters and z enters beacause there is no improvement
		else !if(p_nnf.eq.p_nnfiter.and.j_objf-j_valiter.le.j_tolecur)then
			!is(p_p)write(p_n16,*)'*niter with improvement,objf,old',p_objf,p_valiter
			p_niter=0
			p_valiter=p_objf
		endif !if(p_nnf.eq.p_nnfiter.and.p_objf-p_valiter.le.p_tolecur)   9880
	endif !if(p_niter.ge.10)   9877
 
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
	p_vcmax=p_tolep*p_epsj
 
	!cycle
	if(p_ienterv.eq.1.and.p_nsame.gt.5.and.p_objf.lt.p_oldsol+p_tolecur)then
		if(p_iprint.gt.1)write(6,*)'cycling, Unit',p_unit,'Pivot ',p_pivot, 'Objective ',p_objf
		return    ! we check if z can enter z
	endif !if(p_ienterv.eq.1.and.p_nsame.gt.5.and.p_objf.lt.p_oldsol+   9912
 
	do ilr=p_lr0+1,p_nrow   !!!!! nonbasic residuals <B1>
		ico=p_lr(ilr)  !lri(lr(ilr))  ! row=col for residual varaibles, note lr
		if(ico.eq.p_ilres)then
			if(p_objf.lt.p_objilres+p_tolecur)cycle
			p_ilres=0
		endif !if(ico.eq.p_ilres)   9919
		if(p_rhs2(ico).gt.p_rhs(ico))then
			! lowerilla ei merkitystä jos rhs=rhs2,
			! voidaan tehdä lista jossa mukana vain
			! mahdolliset rajoitukset
			! could increase vc<0
			if(p_vc(ico).gt.p_vcmax.and.p_lower(ico).and.ico.ne.p_icolold)then
				! make rhs2 active
				p_vcmax=p_vc(ico)
				p_ienter=1
				p_ilrmax=ilr
				p_newc=ico      !laita suoraan
				!is(p_p) write(p_n16,*)'negat. resid could enter', ico,'ste',e(ico),sqrt(e(ico))  !!!!
				!is(p_p.and.p_fpresent) write(p_n16,*)'**fact** KANTAAN >> residual, p_ienter = 1'
				if(p_ubou(ico).and.p_lbou(ico)) then
					p_tmax=p_rhs2(ico)-p_rhs(ico) ;p_rcur=p_tmax
				else !if(j_ubou(ico).and.j_lbou(ico)) then
					p_tmax=j_inf  ;p_rcur=p_tmax
				endif !if(p_ubou(ico).and.p_lbou(ico))   9936
				p_post=.false.  !post=positive residual could enter
				! xma ei käy koska voidaan ???
				! tmin=xmi(p_newc) !min value for the entering
				!					goto 100
				! ajatus kesken
				! lower aina yhteensopiva sen kanssa onko
				! rhs vai rhs2 voimassa
				! lower/rhs päivitetään vain silloin kun
				! resid poistuu kannasta
 
				! vc
			else if(p_vc(ico).lt.-p_vcmax.and..not.p_lower(ico).and.ico.ne.p_icolold)then !if(j_vc(ico).gt.j_vcmax.and.j_lower(ico).and.ico.ne.p_icolold)then
				! come from rhs2 downwards
				p_ienter=1  !pitäis olla eri
				p_ilrmax=ilr
				p_newc=ico !laita suoraan
				if(p_ubou(ico).and.p_lbou(ico)) then
					p_tmax=p_rhs2(ico)-p_rhs(ico);p_rcur=p_tmax
				else !if(j_ubou(ico).and.j_lbou(ico)) then
					p_tmax=j_inf ;p_rcur=p_tmax
				endif !if(p_ubou(ico).and.p_lbou(ico))   9957
				p_vcmax=-p_vc(ico)
				p_post=.true.
				!is(p_p) write(p_n16,*)'posit resid could enter',ico  !!!!
				!is(p_p.and.p_fpresent) write(p_n16,*)'**fact** KANTAAN >> residual, p_ienter = 1'
			end if !if(p_vc(ico).gt.p_vcmax.and.p_lower(ico).and.ico.ne.p_icol   9928
		end if !if(p_rhs2(ico).gt.p_rhs(ico))   9923
	end do !ilr=p_lr0+1,p_nrow   9917
 
 
end subroutine renter !subroutine renter()
 
subroutine zenter()
	use fletdmod
	use fletdmod2
	use fletcherdmod
	p_post=.true.
	!	p_dapu=p_vcmax
	!	if(p_p)write(p_n16,*)'startzenter,p_lz0,p_nz',p_lz0,p_nz
	!	if(p_p)write(p_n16,*)'objr',p_objr
	!	if(p_p)write(p_n16,*)'pvc',p_vc(1:p_nrow)
	do ilz=p_lz0+1,p_nz
		newa0=p_lz(ilz)	! col number in A
		!is(p_p)write(p_n16,*)'newa0',newa0
		newc0=newa0+p_nrow  ! col number taking into account the I part tarvitaanko p_newd
		! ol ico pit olla newa
		if(p_feasible)then
			p_val_=p_objr0(newc0)  ! in objr all cols are counted
		else !if(j_feasible)then
			p_val_=0.
		endif !if(p_feasible)   9987
		!could take maximum
		! place to refer zcoef
		!if(p_p)write(p_n16,*)'pa',p_a(1:p_nrow,newa0)
		! if(sparse)then
		! iel=0
		! do jj=p_lavecsp(p_lavecsp(0)+newa0),last(newa0)
		! iel=iel+1
		! p_val_=p_val_-p_vc(p_lavecsp(jj))*p_a(iel,newa0)
		! enddo !jj=p_lavecsp(p_lavecsp(0)+newa0),last(newa0)  18220
		! else !if(sparse)then
		do  j=1,p_nrow
			p_val_=p_val_-p_vc(j)*p_a(p_abas(newa0)+j)  !p_a(j,newa0)
		enddo ! j=1,p_nrow  10002
		!	endif !if(sparse)  18218
		! ei ny otettu huomioon mahdollisuutta, että z voisi olla nyt ylärajalla
		! ja jos se tulee kantaan negatiivisena, niin tavoite voiis kasvaa
		!		if(p_val_.gt.epsj)then
		if(p_val_.gt.p_vcmax)then
			p_newa=newa0
			p_newc=newc0
			p_ienter=2
			p_tmax=j_inf ;p_rcur=p_tmax
			p_ilzmax=ilz  !	do ilz=p_lz0+1,p_nz
			p_vcmax=p_val
		endif !if(p_val_.gt.p_vcmax)  10009
	enddo !ilz=p_lz0+1,p_nz   9982
	! if(p_p)then
	! if(p_ienter.eq.2)then
	! write(p_n16,*)'z could enter',p_newa,'tmax',j_inf,'p_newc',p_newc
	! if(p_fpresent) write(p_n16,*)'**fact** KANTAAN >> z, p_ienter = 2'
	! else
	! write(p_n16,*)'z could not enter enter, p_ienter', p_ienter
	! endif !if(p_ienter.eq.2)   9912
	! endif !if(p_p)   9911
end subroutine zenter !subroutine zenter()
 
subroutine senter()
	use fletdmod
	use fletdmod2
	use fletcherdmod
	double precision value_f
	p_goto400=.false.  !subroutine
	p_ienter=0
	p_post=.true.
	!if(p_p)write(p_n16,*)'senter'
	!	if(p_xpresent)then  !newprice   !B31>
 
	if(p_ix0.ne.0)then !ix(0).ne.0.and.feasible
		p_vx(0)=j_1
	else !if(p_ix0.ne.0)then
		p_vx(0)=j_0
	endif !if(p_ix0.ne.0)  10039
	do i=1,p_nrow
		if(p_ix(i).ne.0)then
			p_vx(i)=-p_vc(i)   ! formula 6.41, voitasiko kenties tästä merkinkäännöstä
			! luopua ja pelata suoraan vc:n avulla
		endif !if(p_ix(i).ne.0)  10045
	enddo !i=1,p_nrow  10044
	!if(p_p)  write(p_n16,'(a,(1x,5g19.7))')'vx',(p_vx(i),i=0,min(p_nrow,50))
	!	endif !if(p_xpresent)   8374
 
	p_valueopt=p_small  !j_ninf
	!p_secondb=p_small
 
	p_iopt=0        ! the optimal schedule
 
	! start computing shadow pricees of schedules <B333>
	!	write(6,*)'idom4',p_unit,p_idomba
	if(p_isdomain)call jlpcurix()  !(p_unit)
	! determines for each row if the unit iunit belonggs to the domain of the row
	! units must be accessed one by one
	! p_nrowcurx
	! p_rowcurx
	! p_idomba
 
	! if (p_fpresent) then
	! call jlpfcurix(p_unit)
	! call jlpfcurixy(p_unit)
	! call jlpfirow2curix(p_unit)
	! endif !if (p_fpresent)   6047
	!determines for each row if the unit p_unit belonggs to the domain of the row
	!matrix  domainbits(number,mxunit) contains bits for telling if unit belongs to certain domain
	! returns nrowp
	! rowcurx  the x-variable of the
	!	p_vxpack=0.
	!	p_ixpack=0
	p_nxbas=0
doloop:	do j=1,p_nrowcurx
		iro=p_rowcurx(j)
		if(p_vx(iro).ne.j_0)then
			do ip=1,p_nxbas-1
				if( p_ix(iro).eq.p_ixpack(ip))then
					p_vxpack(ip)=p_vxpack(ip)+p_vx(iro)
					cycle doloop
				endif !if( p_ix(iro).eq.p_ixpack(ip))  10083
			enddo !ip=1,p_nxbas-1  10082
			p_nxbas=p_nxbas+1
			p_vxpack(p_nxbas)=p_vx(iro)
			p_ixpack(p_nxbas)=p_ix(iro)   ! result (1*ind1,2*ind2,....)
 
		endif !if(p_vx(iro).ne.j_0)  10081
	enddo doloop !oop:	do j=1,p_nrowcurx  10079
	!	nxbas=count(p_ixpack.ne.0)
 
	! Jos tehtaita, ei ohiteta
	if(p_nxbas.le.0.and..not.p_fpresent)then
		p_goto400=.true.;return   !!!! next unit subroutine
	endif !if(p_nxbas.le.0.and..not.p_fpresent)  10097
 
	ikey_ = p_ibaunit(p_unit)+p_keys(p_unit)
 
	! kaksi strategiaa, 1: nykyinen, jossa yksiköstä otetaan aina vain paras vaihtoehto
	! uusi strategia: käydään yksikköä läpi ja otetaan vaihtoehto heti kantaan jos sen varjohinta on suurempi kuin avain-
	! vaihtoehdon hinta+ tolerannssi
	! seuraavassa vaiheessa jatketaan vaihtoehdoilla ssamasta yksiköstä
 
	!tätä varten tarvitaan aliohjelma joka laskee vaihtoehdon arvon
 
	! aluksi sovelletaan nykyistä strategiaa, kun tulee slow improvement siirrytään
	! toiseen strategiaan
 
	! secondt=.true. jos käytetään toista strategiaa
	!is(p_p)write(p_n16,*)'unit,key,ld0,ns',p_unit,p_keys(p_unit),p_lx0,p_ns(p_unit)
 
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
		! ibxmatx=xmatiba(iobs) !,1)
		! do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
		! valuesums(jj_)=valuesums(jj_-1)+&
		! vxpack3(jj_)*p_xmat(ixpack3(jj_)+ibxmatx) !dprod(vxpack3(jj_),p_xmat(ixpack3(jj_)+ibxmatx))
		! enddo !do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
		! endif !if(istree)then
		if(p_fast.and..not.p_fastnow)p_fastvalues(i)=j_inf
		!	p_basreject(cycle nschloop
	enddo !k_=1,p_lx0  10121
	ibxmatx=(p_ibaunit(p_unit)-1)*p_ntemp0
	!write(17,*)'nytpå',kier
	!  -fopenmp
	!$OMP PARALLEL DO
		nschloop:	do i=1,p_ns(p_unit)   !mainloop
		ibxmatx=ibxmatx+p_ntemp0
		if(p_basreject(i))then
			p_svalue(i)=j_ninf
			cycle
		endif !if(p_basreject(i))  10141
		!write(17,*)'nytpot',p_kier,p_unit,i
		iobs=p_ibaunit(p_unit)+i
		if(p_fastnow)then
			if(p_fastreject(iobs).and.i.ne.p_keys(p_unit))then
				p_svalue(i)=j_ninf
				cycle  !i.ne.j_keys(p_unit) added 20.8.2018 JL
			endif !if(p_fastreject(iobs).and.i.ne.p_keys(p_unit))  10148
		endif !if(p_fastnow)  10147
		if(p_filre)then
			if(p_rejects(iobs))then
				! if(istree)then
				! ibxmatx=xmatiba(iobs) !,1)
				! do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
				!valuesums(jj_)=valuesums(jj_-1)+dprod(vxpack3(jj_),p_xmat(ixpack3(jj_)+ibxmatx))
				! enddo !do jj_=idiff3(idiff(iobs)),idiff3(idiff(iobs+1))-1,1
				! endif !if(istree)then
				p_svalue(i)=j_ninf
				cycle
			endif !if(p_rejects(iobs))  10154
		endif !if(p_filre)  10153
 
 
 
		if (p_xpresent) then
			! if(istree)then
			! ibxmatx=xmatiba(iobs) !,1)
			! do jj_=idiff3(idiff(iobs)),p_nxbas
			! valuesums(jj_)=valuesums(jj_-1)+vxpack3(jj_)*p_xmat(ixpack3(jj_)+ibxmatx) !dprod(vxpack3(jj_),p_xmat(ixpack3(jj_)+ibxmatx))
 
			! enddo !do jj_=idiff3(idiff(iobs)),p_nxbas
			! p_value=valuesums(p_nxbas)
 
			! else !if(istree)then
			!			ibxmatx=xmatiba(iobs) !,1)xmatiba(iobs)=(iobs-1)*p_ntemp0
			!		p_value=dot_product(p_vxpack2(1:p_nxbas),p_xmat(p_ixpack2(1:p_nxbas)+ibxmatx) )   !!!!
			p_svalue(i)=dot_product(p_vxpack(1:p_nxbas),p_xmat(p_ixpack(1:p_nxbas)+ibxmatx) )   !!!!
			!	if(p_p)write(6,*)'i,val',i,p_vxpack(1:p_nxbas),p_xmat(p_ixpack(1:p_nxbas)+ibxmatx) ,p_svalue(i)
			!pitäsi testata, kumpi on nopeampi. Näyttävät PC:llä tuottavan saman tuloksen
			!	  j_value=j_0
			!  do jj_=1,nxbas
			!  j_value=j_value+dprod(j_vxpack2(jj_),j_xmat(j_ixpack2(jj_),iobs))
			!  enddo
			!		endif !if(istree)then
 
		else !if (j_xpresent2) then
			p_svalue(i)=j_0   !p_value = j_0
		endif !if (p_xpresent)  10168
		!is(p_p)write(p_n16,*)'<6712>,nxbas,p_unit,i,initval',p_nxbas,p_unit,i,p_value
		!!!jos tehtaita, valuen arvoa kasvatetaan
		if (p_fpresent) then !!!!
			!		p_value = p_value + value_f(iobs)
			p_svalue(i) = p_svalue(i)+ value_f(iobs)
		endif !if (p_fpresent)  10194
 
		! if(p_value.gt.p_valueopt)then  !!!!
		! secondb=p_valueopt
		! p_isecond=p_iopt
 
		! p_iopt=i
		! ! valueopt2=valueopt
 
		! p_valueopt=p_value
		! endif !if(j_value.gt.j_valueopt)then
		! if(p_value.gt.secondb.and.abs(p_value-p_valueopt).gt.0.d-6)then
		! secondb=p_value
		! p_isecond=i
		! endif !if(j_value.gt.secondb.and.abs(j_value-j_valueopt).gt.0.d-6)then
 
		!		if(i.eq.p_keys(p_unit)) p_valuek=p_value !
 
		if(p_fastmake)then
			p_fastvalues(i)=p_svalue(i)  !p_value
			if(p_svalue(i).lt.p_fastvaluemin)p_fastvaluemin=p_svalue(i) !p_value
		endif !if(p_fastmake)  10215
 
	enddo nschloop !hloop:	do i=1,p_ns(p_unit)  10139
	!$OMP END PARALLEL DO
	p_valuek=p_svalue(p_keys(p_unit))
	p_loco=maxloc(p_svalue(1:p_ns(p_unit))) !output must be array
	p_iopt=p_loco(1)
	p_valueopt=p_svalue(p_iopt)
	j_o(p_ivvaluedif)%d(p_unit)=p_valueopt-p_valuek
 
	!is(p_p)write(p_n16,*)'p_unitetc',p_unit,p_valuek,p_iopt,p_valueopt
 
	if(p_fastmake)then
		if(p_fastreject(p_ibaunit(p_unit)+p_iopt))p_nimp=p_nimp+1
		!		p_fastcut=min(p_fastvaluemin+p_fastpros2*(p_valueopt-p_fastvaluemin),p_valuek)-p_tiny78
		p_fastcut=p_fastpros2*p_valuek
		p_fastreject(p_ibaunit(p_unit)+1:p_ibaunit(p_unit)+p_ns(p_unit))=.false.
		nac=0
		do i=1,p_ns(p_unit)
			if(p_fastvalues(i).lt.p_fastcut.and.i.ne.p_keys(p_unit))p_fastreject(p_ibaunit(p_unit)+i)=.true.
			if(.not.p_fastreject(p_ibaunit(p_unit)+i))nac=nac+1
			!	if(fastvalues(i).lt.fastcut)write(16,*)'reject',p_unit,i
		enddo !i=1,p_ns(p_unit)  10236
		p_activeunit(p_unit)=nac.gt.1
		!	write(17,*)p_unit,p_ns(p_unit),nac,p_fastcut,p_valuek
 
 
	endif !if(p_fastmake)  10230
	if(p_iopt.eq.p_keys(p_unit))return
	if(p_valueopt.gt.p_valuek+p_tolecur)call entercol
 
 
	!write(17,*)'senterinlopus',kier
 
end subroutine senter !subroutine senter()
 
subroutine tulostele()
 
	!end kierloop
 
	if(p_feasible)then
		!	If(.not.p_maxo)p_objf=-p_objf
		write(6,*) 'solution is feasible'!!!!
	else !if(j_feasible)then
		write(6,*) 'SOLUTION IS INFEASIBLE' !!!!
		!	write(6,*)'nofeasible rows ',p_nnf
		if(p_iprint.lt.2)p_iprint=2
	endif !if(p_feasible)  10258
 
	!write(6,*)'pivots ',p_pivot,' rounds ',p_kier  !!!!
	if(p_nresw.gt.7)write(6,*)'total number of residual cannot enter conditions ',p_nresw
	if(p_npivotw.gt.7)write(6,*)'total number of pivot cannot be made conditions ',p_npivotw
	if(p_nkeyfactw.gt.7)write(6,*)'total number of key factory cannot be changed conditions ',p_nkeyfactw
	if(p_xpresent)write(6,*)'key schedule changed ',p_nkeys, ' times'  !!!!
	if(p_fpresent)write(6,*)'key factory  changed ',p_nkeyf, ' times'  !!!!
	write(6,*)'basic variables computed ',p_ntote,' times' !!!!
	if(p_zerob.gt.0)write(6,*)'zero objective vector encountered ',p_zerob,' times'
 
	if(p_feasible)then
		j_v(p_ivfeasible)=j_1
		j_v(p_ivoptimal)=j_1
 
	endif !if(p_feasible)  10276
 
	call defsolu()
	if(p_xpresent)then
 
		call getsolx() !p_nunits,ibaunit,keys,lunit,nsch,isch,
	endif !if(p_xpresent)  10283
	! get the solution ready to be accesed by the inquiry routines
 
	if(p_p9)then
		write(6,*)'xkf variables in the basis'
		write(6,*)'unit,index in xk%-list, factory index in factories%-list,keyfactory, amount'
		! do lfj_=p_mxd+1,p_lf0
		! write(6,*) p_lunit(p_lf(lfj_)),p_ixkf(p_lf(lfj_)), &
		! p_ixkffact(p_lf(lfj_)),p_keyfact(p_lunit(p_lf(lfj_)),&
		! p_ixkf(p_lf(lfj_))),p_x(p_nrowz+p_lf(lfj_))
		! enddo !lfj_=p_mxd+1,p_lf0  10394
	endif !if(p_p9)  10289
 
	if (p_fpresent) call defsoluf()
 
 
 
end subroutine tulostele !subroutine tulostele()
 
subroutine tulostele2(iob) !prints and checkes if finished
	integer i
	save asv
	p_goto900=.false.
	!goto785=.false.
 
	if(p_feasible)then
		if(p_kier.le.10.or.int(p_kier/10)*10.eq.p_kier)then
			p_as=100.
			if(p_fast)p_as=100.*(1.-1.*count(p_fastreject)/p_lopp)
 
			!	pros=0.
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
			pros=abs(100.*p_coefmax*(p_objf-p_objfprev)/p_objfprev)
			if(p_kier.le.10)pros=10.*pros
			change=p_objf-p_objfprev
			p_nimp2=-1
			if(as.ne.asv.and.asv.ne.100.)p_nimp2=p_nimp
 
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
				write(6,'(i5,i8,g19.12,f8.4,f7.2,5i5,i3,a,f5.2,i5,a,f5.2,i5,f6.2)')&
					p_kier,p_pivot,p_coefmax*p_objf,pros,p_as,p_lr0,p_lz0,p_lx0, &
					p_lf0,p_nnf,iminc,':',secd,imint,':',sect,p_nimp2,fastpros
 
			else
				write(6,'(i5,i8,g19.12,8x,f7.2,5i5,i3,a,f5.2,i5,a,f5.2,i5,f6.2)')&
					p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0, &
					p_lf0,p_nnf,iminc,':',secd,imint,':',sect,p_nimp2,fastpros
 
			endif !if(p_objfprev.ne.p_small)  10348
 
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
				endif !if(j_o(p_ivvaluedif)%d(i).gt.j_0)  10369
			enddo !i=1,p_nunits  10368
 
			!		write(6,*)'optdif',j_o(p_ivvaluedif)%d(p_iopt),j_o(p_ivvaluedif)%d(p_iopt)/p_objf,j_o(p_ivvaluedif)%d(p_iopt)/p_nunits,&
			!			j_o(p_ivvaluedif)%d(p_iopt)/(p_nunits*p_objf),j_dapu2/j_dapu
 
 
 
 
			ncyc=0
 
			if(p_isstop.or.p_isfastp)then
				j_v(j_ivimp)=p_nimp
				j_v(j_ivchangep)=pros
				j_v(j_ivround)=p_kier
				j_v(j_ivactivep)=p_as
				! call dotrans(iob,p_iostop)
				! if(j_v(p_ivstop).ne.0)then
			endif !if(p_isstop.or.p_isfastp)  10384
			if(p_isstop)then
				if(j_codevalue(iob,p_stoplink).ne.j_0)then
					write(6,*)'iteration stops due to stop->'
					p_goto900=.true.
					return
				endif !if(j_codevalue(iob,p_stoplink).ne.j_0)  10393
 
 
			else !if(p_isstop.and.p_kier.gt.10)then
				if(pros.lt.0.01.and.p_kier.ge.10)then
					write(6,*)'iteration stops due to assumed  stop->(Change%.lt.0.01.and.Round.ge.10)'
					j_v(j_ivimp)=p_nimp
					j_v(j_ivchangep)=pros
					j_v(j_ivround)=p_kier
					p_goto900=.true.
					return
				endif !if(pros.lt.0.01.and.p_kier.ge.10)  10401
 
			endif !if(p_isstop)  10392
 
 
			!	endif !if(p_coefmax*p_objfprev.le.j_0)   9159
			ipivot9=p_pivot !last pivot printed
 
 
			! if(p_kier.eq.30)then
			! do iio=1,j_lopp
			!Round  Pivots      Objective         Change    Change% active% resid    z  sched  xkf  NF-rows
			!   5    4820     2695025.15284     2695025.15   0.000  100.00   439     0    97   540     0
 
			! endif
 
		endif !if(p_kier.le.10.or.int(p_kier/10)*10.eq.p_kier)  10312
		if(p_fast.and.p_feasible.and.p_kier.ge.20)then
			re=count(p_fastreject)
			!if(pp)write(16,*)'rejpros',re/j_lopp,p_fastusesame,p_fastusedsameFround,non
			p_fastusedsame=p_fastusedsame+1
			if(p_fastusedsame.gt.p_fastusesame)then
				p_fastnow=.false.
				p_fastusedsame=0
				p_fastmake=.true.
				if(p_isfastp)then
					! call j_getname(j_ivfastp)
					! write(6,*)'fast p',j_oname(1:j_loname)
					! write(6,*)'fastp*2',p_fastpros,j_v(j_ivfastp),'imp',j_v(j_ivimp),&
					! 'change%',	j_v(j_ivchangep),&
					! 'kier',	j_v(j_ivround),&
					! 'activep',	j_v(j_ivactivep)
					p_fastpros=j_codevalue(iob,p_fastplink)
					j_v(j_ivfastp)=p_fastpros
					p_fastpros2=p_fastpros/100.d0
					!	write(6,*)'fastprosaft ',p_fastpros
 
				endif !if(p_isfastp)  10433
				p_nimp=0  !fastmake alkaa
				!	write(6,*)'fastmake,p_kier,nimp,nimpr',p_kier,nimp
				!	nnnn=0   ! number of
				! if(j_xdatfrodisk)j_xdatlast=0
				! !fastreject=.false.
				! if(.not.j_xmatinmemory)then
				! j_xmatlast=0
				! j_memobs=j_lopp+1
				! endif
				!if(.not.j_xdatinmemory)j_xdatlast=0
 
			else !if(p_fastusedsame.gt.p_fastusesame)then
				!		write(6,*)'hep'
				!if(fastmake)then
				!if(pp)write(16,*)'j_xmatlast,j_xdatlast',j_xmatlast,j_xdatlast
				! iero=0
				! iero2=0
				! do ii=1,j_lopp
				! if(fastreject(ii).and.j_memobs(ii).le.j_lopp)iero=iero+1
				! if(.not.fastreject(ii).and.j_memobs(ii).gt.j_lopp)iero2=iero2+1
				!	write(6,*)'fastmakeloppuu',p_kier,nimpr,nimp
 
				! enddo
				!	write(6,*)'iero,iero2',iero,iero2
				!endif
 
				p_fastnow=.true.
				p_fastmake=.false.
 
 
			endif !if(p_fastusedsame.gt.p_fastusesame)  10429
 
		endif !if(p_fast.and.p_feasible.and.p_kier.ge.20)  10425
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
			endif !if(p_lower(p_lr(i)).and.p_x(p_lr(i)).gt.p_tole(p_lr(i)))  10485
		enddo !i=1,p_lr0  10483
		if(p_kier.le.10.or.int(p_kier/10)*10.eq.p_kier)then
			p_as=100.
			if(p_fast)p_as=100.*(1.-1.*count(p_fastreject)/p_lopp) !laske muualla
			pros=0.
			call cpu_time(time)
			iminc=(time-p_time0)/60.
			imint=(time-p_time00)/60.
			secd=time-p_time0-iminc*60.
			sect=time-p_time00-imint*60.
			p_time0=time
			write(6,'(i5,i8,g19.12,7x,f8.2,5i5,i3,a,f5.2,i5,a,f5.2)')&
				p_kier,p_pivot,p_coefmax*p_objf,p_as,p_lr0,p_lz0,p_lx0,p_lf0,p_nnf, &
				iminc,':',secd,imint,':',sect
			p_loco=maxloc(j_o(p_ivvaluedif)%d(1:p_nunits)) !output must be array
			p_iopt=p_loco(1)
			!		write(6,*)'optdif',j_o(p_ivvaluedif)%d(p_iopt)
 
		endif !if(p_kier.le.10.or.int(p_kier/10)*10.eq.p_kier)  10493
	endif !if(p_feasible)  10311
 
 
 
end subroutine tulostele2 !subroutine tulostele2()
 
 
 
 
 
subroutine fenter()
 
 
logloop: do while(p_ilog.le.p_nlog)
		p_fopt=j_ninf
		jne=1
		ine=p_neig(p_inebas+jne)
neigloop:	do while(ine.gt.0)
			ifact=p_neig(p_inebas+jne)
 
			ilogfactbas=(ifact-1)*p_logfactstep !p_ndupmax1*p_nlog
			jterm=1
			iterm=p_logfactterm(ilogfactbas+jterm)
			j_dapu=j_0
termloop:		do while(iterm.gt.0)

				irow=p_rowofterm(iterm)
				itable=j_o(p_ivtable)%i2(iterm)
				! is term table or not
				if(itable.gt.0)then
 
					if(irow.eq.0)then
						!			j_dapu=j_dapu+ j_o(p_ivtablecoef)%i2(itable)*p_neigu(p_inebas+jne)
						j_dapu=j_dapu+p_coef(iterm)* p_neigu(p_inebas+jne)
					else
						j_dapu=j_dapu+ p_coef(iterm)*p_vc(irow)*p_neigu(p_inebas+jne)
					endif !if(irow.eq.0)  10542
 
				else
 
					if(irow.eq.0)then
						j_dapu=j_dapu+ p_coef(iterm)
					else
						j_dapu=j_dapu+ p_vc(irow)*p_coef(iterm)
					endif !if(irow.eq.0)  10551
 
				endif !if(itable.gt.0)  10540
				jterm=jterm+1
				iterm=p_logfactterm(ilogfactbas+jterm)
			enddo termloop !mloop:		do while(iterm.gt.0)  10535
			if(ifact.eq.p_keyfact(p_keyfactbas+ifact))then
				p_keyfval=j_dapu
			elseif(j_dapu.gt.p_fopt)then
				p_optfact=ifact
				p_fopt=j_dapu
			endif !if(ifact.eq.p_keyfact(p_keyfactbas+ifact))  10561
 
			jne=jne+1
			ine=p_neig(p_inebas+jne)
		enddo neigloop !gloop:	do while(ine.gt.0)  10528
		if(p_fopt.gt.p_keyfval)then
			p_ienter=4
			return
		endif !if(p_fopt.gt.p_keyfval)  10571
 
 
		p_ilog=p_ilog+1
		p_logfactbas=p_logfactbas+p_nlogfact
	enddo logloop !loop: do while(p_ilog.le.p_nlog)  10524
	return
 
 
end subroutine fenter !subroutine fenter()
 
subroutine entercol()
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
	p_newc=p_newa+p_nrow  !in Fletecher cols, I part (residuals) are counted also
	!	if(p_p) write(p_n16,*)'ent sched: p_newd',p_newd,'p_newc',p_newc, 'tol=',p_tolecur
	! get key schdeule
	iobs= p_ibaunit(p_unit)+p_keys(p_unit)
	iobsopt=p_ibaunit(p_unit)+p_iopt
	ibxmatx=ibamatx(iobs) !,1)
	ibxmatx2=ibamatx(iobsopt) !,2)
	p_lunit(p_newd)=p_unit
	p_isch(p_newd)=p_iopt
	! 6.42  a0'D , here we are prepared that the same x can be in differen rows
	! how does the object variable change
	!!!! put key schedule first int objr and a matrix
	if(p_ix(0).ne.0)then  !then p_ixcur(0))then
		p_objr0(p_newc)=p_xmat(p_ix(0)+ibxmatx2)-p_xmat(p_ix(0)+ibxmatx) !v(ix(0))
		p_i1=2
	else !if(j_ixcur(0).ne.0)then
		p_i1=1
		p_objr0(p_newc)=0.
	endif !if(p_ix(0).ne.0)  10609
 
	!objr:n päivitys
	if(p_fpresent) then
		! nykyisessä domainissa 0-rivillä xk-muuttujia
		if((p_nfrowx.gt.0.and.p_frowx(1).eq.0).or.(p_nfyrow.gt.0.and. p_fyrow(1).eq.0)) then
			!objr0:n päivitys
			p_objr0(p_newc)=p_objr0(p_newc)+p_valueopt_af(0)
		endif !if((p_nfrowx.gt.0.and.p_frowx(1).eq.0).or.(p_nfyrow.gt.0.a  10620
	endif !if(p_fpresent)  10618
 
	! if(sparse)then
	! ! kannattaa varmaan tehdä vektori johon kerätään rivit joilla x-muuttujia
 
	! iel=0
	! istart1=p_lavecsp(p_lavecsp(0)+p_newa)-1
 
	! do jj=p_i1,p_nrowx
	! j=p_rowcurx(jj)
	! p_apu=p_xmat(p_ix(j)+ibxmatx2)-p_xmat(p_ix(j)+ibxmatx)  !a( ,p_newa ilamn pakkausta
 
	! if(abs(p_apu).gt.p_tiny78)then
	! iel=iel+1
	! p_a(iel,p_newa)=p_apu
	! p_lavecsp(istart1+iel)=j
	! endif !if(abs(p_apu).gt.p_tiny78)  19229
 
	! !robleema, jos pitää vaihtaa
	! enddo !jj=p_i1,p_nrowx  19225
 
	! last(p_newa)=istart1+iel
	! call jlpgetcol(p_newa)
 
	! else !if(sparse)then
	!TASSA
	p_a(p_abas(p_newa)+1:p_abas(p_newa)+p_nrow)=j_0 !p_a(1:p_nrow,p_newa)=j_0
	!if(p_p)write(p_n16,*)p_row0,p_nrowcurx,p_rowcurx(p_row0:p_nrowcurx),'pix',p_ix
	!if(p_p)write(p_n16,*)'p_ntemp0',p_ntemp0,'iba',ibxmatx2,'iobsop',iobsopt,'iopt',p_iopt,'iunit',p_unit
	!if(p_p)write(p_n16,*)p_xmat(ibxmatx2+1:ibxmatx2+p_ntemp0),p_xmat(ibxmatx2+1:ibxmatx2+p_ntemp0)
	do iro=p_row0,p_nrowcurx
		!	if(p_p)write(p_n16,*)iro,p_rowcurx(iro),p_ix(p_rowcurx(iro)),p_xmat(ibxmatx2+p_ix(p_rowcurx(iro)))
		!	p_a(p_rowcurx(iro),p_newa)=p_xmat(ibxmatx2+p_ix(p_rowcurx(iro)))-p_xmat(ibxmatx+p_ix(p_rowcurx(iro)))
		p_a(p_abas(p_newa)+p_rowcurx(iro))=p_xmat(ibxmatx2+p_ix(p_rowcurx(iro)))-p_xmat(ibxmatx+p_ix(p_rowcurx(iro)))
	enddo !iro=p_row0,p_nrowcurx  10654
	!do iro=1,p_nrow
	!	if(p_ixcur(iro))p_a(iro,p_newa)=p_xmat(ibxmatx2+p_ix(p_rowcurx(iro)))
	!enddo !iro=1,p_nrow   7976
	!	if(p_p8)write(17,*)'p_ixcur',p_ixcur
 
	!p_a(p_rowcurx(p_row0:p_nrowcurx),p_newa)=p_xmat(ibxmatx2+p_rowcurx(p_row0:p_nrowcurx))
	! do j=1,p_nrow
	! if(p_ixcur(j))then
	! p_a(j,p_newa)=p_xmat(p_ix(j)+ibxmatx2)-p_xmat(p_ix(j)+ibxmatx)  !v(ix(j))
	! else !if(j_ixcur(j).ne.0)then
	! p_a(j,p_newa)=j_0
	! endif !if(p_ixcur(j))   7938
	! enddo !j=1,p_nrow   7937
	! p_a(1:p_nrow)=j_0
 
 
	!a:n päivitys
	if(p_fpresent) then
		!p_a(1:p_nrow,p_newa) = p_a(1:p_nrow,p_newa)+ p_valueopt_af(1:p_nrow)
		iba=p_abas(p_newa)
		!	p_a(iba+1:iba+p_nrow) = p_a(iba+1:iba+p_nrow)+ p_valueopt_af(1:p_nrow)
 
		p_a(iba+1:iba+p_nrow) = p_a(iba+1:iba+p_nrow)+ p_valueopt_af(1:p_nrow)
	endif !if(p_fpresent)  10676
 
	!		endif !if(sparse)  19219
	! get optimal schedule, put differences into objr and a
 
	p_tmax=p_one	;p_rcur=p_tmax	! myöhemmin pintala
	p_ienter=3	!!!! scedule enters
 
	!is(p_p.and.p_fpresent) write(p_n16,*)'**fact** KANTAAN tulossa >> vaihtoehto, p_ienter = 3; unit, sch' , &
	!p_unit,p_iopt
 
 
 
 
end subroutine entercol !subroutine entercol()
 
 
subroutine leaving()
 
	use fletdmod
	use fletdmod2
	use fletcherdmod
	!	logical cancel
	!implicit none
	integer m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj,nup,nfreq
	integer i,i_,iba1,ibas1,ibxdatkey_,ikey_,ibxdatobs,ibxdatobs_,iobs_,ibxdatopt,iobsopt
	common/factorc/m0jj,m1jj,mpjjsp,mqjjsp,mpjj,mqjj
	common/refactorc/nup,nfreq
	integer pvars_
	!	cancel=.false.
	p_goto112233=.false.
	p_goto222=.false.
	p_goto900=.false.;p_goto1234=.false.;p_goto55=.false.
	!p_goto 55 go to next unit
	if(p_goto8888)goto 8888 !this comes directly from update
	if(p_goto8889)goto 8889 ! this also
	!if(p_pp)write(16,*)'leaving,p_ienter,tmax,p_ienter,j_tmax',p_ienter,p_ienter,j_tmax
	!cycle
	if(p_ienter.eq.p_ienterv.and.	p_objf.lt.p_oldsol+p_tolecur)then
		p_nsame=p_nsame+1
	else !if(p_ienter.eq.p_ienterv.and.	j_objf.lt.p_oldsol+j_tolecur)then
		p_ienterv=p_ienter
		p_oldsol=p_objf
		p_nsame=0
	endif !if(p_ienter.eq.p_ienterv.and.	p_objf.lt.p_oldsol+p_tolecur  10720
 
	!is(p_p)write(p_n16,*)'p_inleaving  ',p_ienter,p_ls,' * ',p_lsi
	p_unitv=p_unit !!!!
	p_kierv=p_kier  !!!!
	p_justkey=.false.  !!!!
	! determine leaving variable
	! p_newc -is coming column , p_newa in A, p_newd in D
	! tmax gives max values for enterin var.
 
	! update step
	! determine leaving variable  c:new column, direct effect
	! Bx=b  ,  Bx+tc=b
	! xn=x- t*inv(B)*c =x-t*r  where r=inv(B)*c, i.e. solution of B*r=c
 
	! we  make one call using ls,
	! testi
	! write(6,*)'100,ls,p_newc,a(1,p_newa)',ls(1),ls(2),p_newc,a(1,p_newa)
 
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
	!
 
	if(p_ienter.gt.1)then  !!!!
		!	if(p_p)write(p_n16,*)'p_newa',p_newa,'a:',p_a(1:p_nrow,p_newa)
		!	call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,0,p_a(1:,p_newa),r,&
		call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,0,p_a(p_abas(p_newa)+1:),r,& !!!!
			p_ls,wslu1,lwsll1,.false.)   !!!!!
		if(p_p)write(6,*)'aftfbsub p_post ',p_post
	else !if(p_ienter.gt.1)then
		! residual enters
		! p_newc=resid , mq=p_newc
		!call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,p_newc,p_a(1:,p_newa),r, & !!!!
		call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,p_newc,p_a(p_abas(p_newa)+1:),r, & !!!!
			p_ls,wslu1,lwsll1,.false.)   !!!!
	endif !if(p_ienter.gt.1)  10759
	!	endif !if(sparse)  19316
 
	p_leave=0  !!!!position in ls
	p_leavk=0  !!!! leaving key schedule
	p_leavec=0 !!!! leaving columns
 
	if(.not.p_post)then
		do jj=1,p_nrow
			r(p_ls(jj))=-r(p_ls(jj))
		enddo !jj=1,p_nrow  10779
		p_post=.true.   !!!!
	endif !if(.not.p_post)  10778
 
	! if(p_p)then
	! write(p_n16,*)'start leaving*, ls,lsi:'
	! write(p_n16,'(1x,15i5)')p_ls(1:min(p_nrow,100))
	! write(p_n16,'(1x,15i5)')p_lsi(1:min(p_nrow,100))
	! 789	write(p_n16,*)'r:ls-order, p_lr0',p_lr0
	! write(p_n16,'(5g17.10)')(r(p_ls(jj7)),jj7=1,min(p_nrow,100))
	! ! r=inv(B)*c
	! ! tmax =absilute value for entering variable
	! ! could do leaving in 3 different phases, residual, z, x
 
	! ! if r>0 then x decreases
	! ! if r<0 then x increases
	! write(p_n16,*)'x,ls-order'
	! write(p_n16,'(5g17.10)')(p_x(p_ls(jj7)),jj7=1,min(p_nrow,50))
	! write(p_n16,*)'tmax',p_tmax, ' post',p_post
	! endif !if(p_p)  11159
 
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
	! p_lead to overflow)
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
	if(p_nz.gt.0)then
		if(p_p)write(6,*)'bef zleavtmax'
		call zleavtmax()
		if(p_p)write(6,*)'aft zleavtmax'
	endif !if(p_nz.gt.0)  10832
 
	!if(p_xpresent2)then
	if(p_p)write(6,*)'bef sleavtmax'
	call sleavtmax()
	if(p_p)write(6,*)'aft sleavtmax ',p_tmax,'p_tmaxmin ',p_tmaxmin
	! if(t_max.lt.j_0)then
	! cancel=.true.
	! p_ienter=0
	! return
	! endif !if(t_max.lt.j_0)  12019
	!!!! voiko xkf poistua kannasta
	if (p_fpresent) call fleavtmax()
	!
	!	endif !if(p_xpresent2)  11468
 
	!if(p_)write(p_n16,*)'tmaxbefore r',p_tmax  !!!!!
	!if(p_9.and.p_tmax.lt.j_0)write(p_n16,*)'negat tmax,p_ienter,p_leavec',p_tmax,p_ienter,p_leavec
2244	if(p_tmax.lt.p_tmaxmin)then  !!!!! jos mukaan, pitää tsekata keyt ensin
		p_leave=p_lsi(p_leavec) !samakuin myöh
		!if(p_.or.p_p9.or.p_tmax.eq.-1.d0)then
		!	write(p_n16,*)'jump88,tmax, post,pivot=',p_tmax,p_post,p_pivot,' p_ienter ',p_ienter
		!endif !if(p_p.or.p_p9.or.p_tmax.eq.-1.d0)  11253
		! if(p_ienter.eq.3.and.p_secondb.gt.p_valuek+p_tolecur)then
		! !if(p_p.or.p_tmax.eq.-1.d0)write(16,*)'secondb0 ', &
		! !	p_secondb,p_valueopt,p_valuek,p_pivot,p_unit
		! p_iopt=p_isecond
		! p_valueopt=p_secondb
		! p_secondb=p_small
		! p_goto222=.true.;return
		! endif !if(p_ienter.eq.3.and.p_secondb.gt.p_valuek+p_tolecur)  11251
		if(p_ienter.eq.3)call lcursam()
		write(6,*)'jump8883 ',j_err,'ienter, p_tmax,p_tmaxmin p_leave ',p_ienter,p_tmax,p_tmaxmin, p_leave
		!	j_err=.true. ;return
		!		goto 8883 !!!!! jump88 if j_tmax.le.j_tmaxmin before checking f residual can p_leave
	endif !2244	if(p_tmax.lt.p_tmaxmin)  10854
	!	if(p_p)write(6,*)'bef rleavtmax'
	call rleavtmax()
	if(p_p)write(6,*)'aft rleavtmax tmax, p_leavec ',p_tmax,  p_leavec
	!j_tmax2=j_tmax   !-j_tiny78  !!!!!do not drop residual too easily
 
 
 
	! if(p_leaveres.ne.0)leaving=1
	! leaving= 1  residaul leaves
	! leaving= 2 z leaves
	! leaving=3  w leaves
	! leaving=4 key leaves
	! leaving=5 xkf leaves
	!if(p_.or.p_p9)write(p_n16,*)'pleavec',p_leavec
	if(p_leavec.gt.0)then  !!!!!
		p_leave=p_lsi(p_leavec)  ! is this needed
 
		if(p_p)write(6,*)'p_leave ',p_leave
		!if(p_.or.p_p9)write(p_n16,*)'pleave',p_leave
		!if(p_leavec.le.j_nrow)j_tmax=j_tmax   !!!!
		!if(p_.and.p_fpresent)write(p_n16,*)'**fact** KANNASTA lähdössä <6344> >> srk'
		!if(p_pp)write(p_n16,*)'p_leavec,p_leave,tmax,x,r',p_leavec,p_leave,j_tmax,j_x(p_leavec), &
		!r(j_ls(p_leave)),j_ls(j),j_xmi(j_ls(p_leave)),j_xma(j_ls(p_leave))
		!if(p_.and.p_xpresent)	write(p_n16,*)'xma,xmi',p_xma(p_ls(p_leave)),p_xmi(p_ls(p_leave))
		! if (p_p.and.p_fpresent.and.(p_leavec > (p_nrowz+p_mxd))) then
		! write(p_n16,'(a50,i3,i3,i3,a25,a25)')'**fact** kannasta lähdössä <6349> ' //&
		! '>> xkf: unit,p_ixk,ifact,xkf,fact', &
		! p_lunit(p_leavec-p_nrowz),p_ixkf(p_leavec-p_nrowz),p_ixkffact(p_leavec-p_nrowz), &
		! j_vname(p_log(p_ixkf(p_leavec-p_nrowz))), &
		! j_vname(p_fact(p_ixkffact(p_leavec-p_nrowz)))
		! endif !if (p_p.and.p_fpresent.and.(p_leavec > (p_nrowz+p_mxd)))  11476
 
	else !if(p_leavec.gt.0)then
		!if(p_)write(p_n16,*)'*no basic variable leaving'  !!!!
	endif !if(p_leavec.gt.0)  10886
	!if(p_pp)write(16,*)'kkkoi'
	! 8883	if(.not.p_xpresent)goto 8888  !!!!! check if key schedule is leaving
 
	! *********************************************'
	if(p_xpresent)then
		if(p_p)write(6,*)'bef skeyleav ','pivot ',p_pivot
		call skeyleav()
 
		if(p_p)write(6,*)'aft skeyleav p_leavk ',p_leavk
	endif !if(p_xpresent)  10911
 
	! ! end checking if key-schedule is leaving , if key is leving p_leavk.gt.
	8889 continue   !!!!! we come here if w.gt.1

	!!!!!Avaintehdas vaihtuu, versio 2 (tarkastellaan vaihtuuko avaintehdas)
	!if(p_fpresent)call fkeyleav()

	if(p_leavk.gt.0)then
		if(p_p)write(6,*)'leaveskeys ',p_leavk,p_p
		call leaveskeys()
	endif !if(p_leavk.gt.0)  10924
 
 
	! !Avaintehdas vaihtuu
	! if (p_fpresent) then
 
	! !avainvaihtoehtovaihtuu, kannassa yksikön ve
	! !versio 2, a lasketaan suoraan s.9 kaavalla
	! !p_leavkwf= yksikön 1. vaihtoehtosarake kannassa
	! if(p_leavkwf>0) then
 
	! call leaveskeyf()
 
	! endif !if(p_leavkwf>0)  11383
 
	! if (p_leavkf.gt.p_mxd) then
 
	! call leafa()
 
	! endif !if (p_leavkf.gt.p_mxd)  11389
 
	! endif !if (p_fpresent)  11378
 
	! testi
8888 continue !we come here direrectly (through checking keys) if tmax is sufficiently small
!	if(p_p)	write(p_n16,*)' p_leave, p_ienter ',p_leave,p_ienter

	if(p_pivot.eq.p_idebug)then
		!	p_p=.true.
 
		write(6,*)'*debugON*',p_pivot
		p_debug=.true.
 
		! elseif(p_pivot.eq.p_jdebug)then
		! p_p=.false.
		! write(6,*)'*debugOFF*',p_pivot
		!			close(p_n16)
	endif !if(p_pivot.eq.p_idebug)  10954
	if(p_p)write(6,*)'entleav ',p_ienter,p_leave,' P_newc ',p_newc,' p_leavk ',p_leavk,' p_tmax ',p_tmax
 
	if(p_leave.eq.0)then
		if(p_ienter.eq.3)then
			if(p_p)write(6,*)'bef leave0ienter3'
			call leave0ienter3()
			if(p_p)write(6,*)'aft leave0ienter3 goto8888',p_goto8888,p_goto55
			if(p_goto55)return
		elseif(p_ienter.eq.1)then
			call leave0ienter1()
		elseif(p_ienter.eq.4)then
			call leave0ienter4()
		else
			call leave0else()
			if(p_goto8888)goto 8888
		endif !if(p_ienter.eq.3)  10968
 
	else !if(p_leave.eq.0)then
		! lbres(p_leaveres) p_leaves
		! update
 
		! if(sparse)then
		! if(p_newc.gt.p_nrowz)mqjjsp=-1  ! mqjj=-1   !new not  something known
		! else !if(sparse)then
		if(p_newc.gt.p_nrowz)mqjj=-1  ! mqjj=-1   !new not  something known
		!	endif !if(sparse)  21409
		! if(p_p)write(p_n16,*)'***pivot***',p_pivot+1
		! if(p_p)then
		! write(p_n16,*)'p_leave',p_leave,'pivot cols',p_ls(p_leave),p_newc,'mp,mq',mpjj,mqjj
		! !			if(sparse)write(p_n16,*)'p_leave',p_leave,'pivot cols',p_ls(p_leave),p_newc,'mp,mq',mpjjsp,mqjjsp
		!endif !if(p_p)  12853
		! pivot uses
		! p_leave if number in ls
		! p_newc is absolute column number
		p_icolold=p_ls(p_leave)
		if(p_p)write(6,*)'icolold ',p_icolold
		if(nup.ge.nfreq)then
 
			p_refac=p_refac+1
			!	if(p_p)write(p_n16,*)'*refact'
		endif !if(nup.ge.nfreq)  11001
 
		if(p_xpresent.and.abs(p_rcur).le.1.d-9)then
			!!write(19,*)'uus,p_pivot,p_ienter,p_leave,j_tmax',p_pivot,p_ienter,p_leave,j_tmax,p_rcur
			p_goto112233=.true.
			return
 
		endif !if(p_xpresent.and.abs(p_rcur).le.1.d-9)  11007
		!if(p_route67.and.p_p)write(6,*)
		if(p_route67)then
			if(p_p)write(6,*)'route67'
			call route67()
			j_v(p_ivroute67)=j_v(p_ivroute67)+1
			if(j_err)return
 
		endif !if(p_route67)  11014
 
		!	if(p_p8)write(6,*)'<7575pivot',wslu1
		if(p_p)write(6,*)'#pivot10 ',p_ls(p_leave),p_newc,p_nrow,p_nm
		p_icolold=p_ls(p_leave)
		p_icolnew=p_newc
 
 
		call pivot(p_icolold,p_icolnew,p_nrow,p_nm,p_a,p_lavec,e,wslu1,lwsll1,p_ifail,p_info)
		p_pivot=p_pivot+1
		! if(p_pivot.gt.76651)then
		! p_p=.true.
		! p_p8=.true.
		! !	write(17,*)p_pivot,p_icolold,p_a(1:p_nrow,icolold),'new',p_icolnew,p_a(1:p_nrow,icolold)
		! write(27,*)p_pivot,p_icolold,p_a((p_icolold-1)*p_nrow+1:p_icolold*p_nrow),'obj ',p_objr(p_icolold),&
		! 'new ',p_icolnew,p_a((p_icolnew-1)*p_nrow+1:p_icolnew*p_nrow),' ob',p_objr(p_icolnew)
		! endif !if(p_pivot.gt.76651)  12222
		! p_a(1:p_nrow,p_newa)
		p_pivotcase=5
		j_o(p_ivpivotcases)%i2(5)=j_o(p_ivpivotcases)%i2(5)+1
		!linux
		!	if(p_p8)write(p_n16,*)'<75aftivot',wslu1
		if(p_ifail.ne.0)then
			write(6,*)'*fail in pivot: p_ifail,tmax,x,Pivots ',&
				p_ifail, p_tmax,p_x(p_ls(p_leave)),p_pivot,'*'
			j_err=.true.
			! if(p_p.or.p_p9)write(16,*) &
			! '*fail in pivot: p_ifail,tmax,x,Pivots ',&
			!	p_ifail, p_tmax,p_x(p_ls(p_leave)),p_pivot,'*trying to recover'
 
			! p_kierv=p_kier
			! p_unitv=p_unit
			! p_feasible=.false.
			! p_nrecover=p_nrecover+1
			! if(p_nrecover.ge.20)then
			! write(6,*)'*jlp* is mixed up,try different tole (e.g.10,100,1000)(or consult J. Lappi)'
			! j_err=.true.
			! p_goto900=.true.;return
			! endif !if(p_nrecover.ge.20)  12979
			! p_goto1234=.true.;return
 
		endif !if(p_ifail.ne.0)  11042
 
		! if(p_tabu)then
		! if(p_fast)then
		! if(p_leavec.gt.p_nrowz.and.p_leavec.le.p_nrowz+p_mxd)then
		! iuni_= p_lunit(p_leavec-p_nrowz)
 
		! p_fastreject(p_ibaunit(iuni_)+p_isch(p_leavec-p_nrowz))=.true.
		! !		if(p_p)write(16,*)'tabu ',iuni_,p_isch(p_leavec-p_nrowz)
		! endif !if(p_leavec.gt.p_nrowz.and.p_leavec.le.p_nrowz+p_mxd)  12215
		! endif !if(p_fast)  12214
		! endif !if(p_tabu)  12213
 
 
		!	endif !if(sparse)  21155
 
		!	if(p_p7)write(77,*)4,p_pivot,p_objf
		!	if(p_p.or.p_p9.or.p_p8)write(p_n16,*)p_p,'<75>',p_pivot,p_objf,p_ienter,p_unit,p_ienterv,p_nsame,p_tmax,&
		!		p_x(p_ls(p_leave)),r(p_ls(p_leave))
 
		!	if(p_p)write(p_n16,*)'e:',e
 
 
		! if(sparse)then
		! if(p_ls(p_leave).gt.p_nrowz) mpjjsp=-1
		! else !if(sparse)then
		!	if(ls(p_leave).gt.nrowz) mpjj=-1
		!	endif !if(sparse)  21272
 
		iunitp=p_unit
 
		p_muutosb=p_muutosb+1
		! update ls
		p_leavec=p_ls(p_leave)  ! column of leaving
		p_leaved=p_leavec-p_nrowz
		! update ls and lsi lists
		! if(p_p)write(p_n16,*)'<33herels',p_ls
		! if(p_p)write(p_n16,*)'<337herelsi',p_ls
		! if(p_p)write(p_n16,*)'<337heleavr',p_leave,p_newc
 
		call jlplex(p_ls,p_leave,p_lsi(p_newc),p_lsi)
 
	endif !if(p_leave.eq.0)  10967
 
	if(p_p)write(6,*)'ientertas ',p_ienter,' p_lcursame ',p_lcursame, 'p_leavec ',p_leavec
 
	if(p_ienter.eq.1)then
		! p_ilrmax idex in lr
		! residual enters
		call jlplex(p_lr,p_ilrmax,p_lr0+1,p_lri)
 
		p_lr0=p_lr0+1
		if(p_lr0.eq.p_nrow)nup=0   !clear refactorizing counter
	else if(p_ienter.eq.2)then !if(p_ienter.eq.1)then
		!  z enters
		call jlplex(p_lz,p_ilzmax,p_lz0+1,p_lzi)
		p_lz0=p_lz0+1
 
	else if(p_ienter.eq.3)then !if(p_ienter.eq.1)then
		!if(p_)write(p_n16,*)'d enters,new ld0',p_lx0+1
		! when searchin if key is leaving we have stored the
		! column were we have link to this unit in p_lcursame
		p_id=p_newc-p_nrowz
		p_ia=p_newc-p_nrow
		! p_leaved is leaving in d
		if(p_lcursame.gt.0)then  ! d-eaves
			! the unit was among basic
			! check that units will be consecutive in the next
			! *****************
			! tallenna kun käydään läpi,mistä alkaa sama yksikkö p_lcursame
			! even if p_lcursame, or other
			!if(p_)write(p_n16,*)'p_lcursame',p_lcursame
			p_iaft=p_iprev(p_lcursame)  ! put after p_iaft
			p_next(p_id)=p_next(p_iaft)
			p_next(p_iaft)=p_id
			p_iprev(p_id)=p_iaft
			p_iprev(p_next(p_id))=p_id
 
			if(p_fpresent)then
				do ilu_ = 1,p_lunits0
					! Päivitetään laskentayksikön vaihtoehtojen alkamissarake lunw:hen
					if (p_lunw(ilu_)==p_lcursame) then
						p_lunw(ilu_)=p_id
					endif !if (p_lunw(ilu_)==p_lcursame)  11142
				enddo !ilu_ = 1,p_lunits0  11140
			endif !if(p_fpresent)  11139
 
			! if(p_p) then
			! write(p_n16,*)'aftl,next',p_next(0),p_next(p_next(0)), &
			! p_next(p_next(p_next(0))),p_next(p_next(p_next(p_next(0))))
			! write(p_n16,*)'iprev',p_iprev(0),p_iprev(p_iprev(0)), &
			! p_iprev(p_iprev(p_iprev(0))),p_iprev(p_iprev(p_iprev(p_iprev(0))))
			! endif !if(p_p)  12003
		else !if(p_lcursame.gt.0)then
 
			p_next(p_iprev(0))=p_id
			p_next(p_id)=0
			p_iprev(p_id)=p_iprev(0)
			p_iprev(0)=p_id
			if(p_p)write(6,*)'next p_id iopt iunit',p_id,p_iopt,p_unit
 
			! if(p_fpresent)then
			! ! Jos lunit(p_id):tä ei löydy lunxkf:ää vastaavista laskentayksiköistä
			! ! --> lunw(lunits0++) = id
			! ! --> lunxkf(lunits0) = 0
 
			! ilu_=1
			! !löytyykö kannan saraketta id vastaavan laskentaksikön xkf-muuttujaa kannasta
			! do while(p_lunit(p_lunxkf(ilu_)) /= p_unit)
			! ilu_=ilu_+1
			! if (ilu_>p_lunits0) exit
			! enddo !while(p_lunit(p_lunxkf(ilu_)) /= p_unit)  11615
			! if (ilu_<=p_lunits0) then
			! p_lunw(ilu_)=p_id
			! else !if (ilu_<=j_lunits0) then
			! !laskentayksikköön ei liittynyt aiemmin kannan kannan vaihtoehto- tai tehdasmuuttujasarakkeita
			! p_lunits0 = p_lunits0+1
			! p_lunw(p_lunits0) = p_id
			! p_lunxkf(p_lunits0) = p_mxd
			! endif !if (ilu_<=p_lunits0)  11619
			! endif !if(p_fpresent)  11608
 
		endif !if(p_lcursame.gt.0)  11126
		! edelleed d-enters
		p_lx0=p_lx0+1
		p_lunit(p_id)=p_unit
		p_isch(p_id)=p_iopt
 
		! update leavelists
		! ei toimi jos joku muu enters
		!********************************
		! if(p_ienter.eq.1)then loppuu tässä
 
	else if(p_ienter==4) then !if(p_ienter.eq.1)then
		! nextf yms päivitys
		!if(p_)write(p_n16,*)'**fact** xkf kantaan (75xx) nextf,lunit,lunxkf,lunw päivitykset, new lf0',p_lf0+1
		! when searchin if key is leaving we have stored the
		! column were we have link to this unit in p_lcursame
		p_id=p_newc-p_nrowz
		p_ia=p_newc-p_nrow
		! p_leaved is leaving in d
 
		! if(p_lcursamef.gt.0)then  ! xk-eaves
		! ! the unit was among basic
		! ! check that units will be consecutive in the next
		! ! *****************
		! ! tallenna kun käydään läpi,mistä alkaa sama yksikkö p_lcursame
		! ! even if p_lcursame, or other
		! !if(p_)write(p_n16,*)'p_lcursamef',p_lcursamef
		! p_iaft=p_iprevf(p_lcursamef, ixkenter)  ! put after p_iaft
		! p_nextf(p_id, ixkenter)=p_nextf(p_iaft, ixkenter)
		! p_nextf(p_iaft, ixkenter)=p_id
		! p_iprevf(p_id, ixkenter)=p_iaft
		! p_iprevf(p_nextf(p_id, ixkenter), ixkenter)=p_id
 
		! do ilu_ = 1,p_lunits0
		! if (p_lunxkf(ilu_)==p_lcursamef) then
		! p_lunxkf(ilu_)=p_id
		! if (p_p) write(p_n16,*)"**fact** lunxkf päivitetty <8184>",(p_lunxkf(jj7),jj7=1,p_lunits0)
		! endif !if (p_lunxkf(ilu_)==p_lcursamef)  11555
		! enddo !ilu_ = 1,p_lunits0  11554
 
		! else !if(p_lcursamef.gt.0)then
		! ! put to the end
		! p_nextf(p_iprevf(p_mxd,ixkenter), ixkenter)=p_id
		! p_nextf(p_id, ixkenter)=p_mxd
		! p_iprevf(p_id,ixkenter)=p_iprevf(p_mxd,ixkenter)
		! p_iprevf(p_mxd,ixkenter)=p_id
 
		! ! Jos lunit(p_id):tä ei löydy lunw:tä vastaavista laskentayksiköistä
		! ! --> lunxkf(lunits0++) = id
		! ! --> lunw(lunits0) = 0
 
		! ilu_=1
		! !löytyykö kannan saraketta id vastaavan laskentayksikön vaihtoehtoa kannasta
 
		! do while(p_lunit(p_lunw(ilu_))/=p_unit)
		! ilu_=ilu_+1
		! if (ilu_>p_lunits0) exit
		! enddo !while(p_lunit(p_lunw(ilu_))/=p_unit)  11575
		! if (ilu_<=p_lunits0) then
		! p_lunxkf(ilu_)=p_id
		! else !if (ilu_<=j_lunits0) then
		! !laskentayksikköön ei liittynyt aiemmin kannan vaihtoehto- tai tehdasmuuttujasarakkeita
		! p_lunits0 = p_lunits0+1
		! p_lunxkf(p_lunits0) = p_id
		! p_lunw(p_lunits0) = 0
		! endif !if (ilu_<=p_lunits0)  11579
 
		! endif !if(p_lcursamef.gt.0)  11541
		! ! edelleed d-enters
		! ! indeksointi muutettu alkamaan mxd+1:stä (vastaa suoraan kannan saraketta)
		! p_lf0=p_lf0+1
		! p_lunit(p_id)=p_unit
		! ! tallennetaan ixkenter
		! ! tallennetaan tehdas f´
 
		! ! update leavelists
		! ! ei toimi jos joku muu enters
		!********************************
 
	endif !if(p_ienter.eq.1)  11107
 
	if(p_p)write(6,*)'p_leavec,nrow',p_leavec,p_nrow
 
	if(p_leavec.le.p_nrow)then
		! resiadual leaves
		! put leaving as first nonbasic
		call jlplex(p_lr,p_lr0,p_lri(p_leavec),p_lri)
		p_lr0=p_lr0-1
	else if(p_leavec.le.p_nrowz)then !if(p_leavec.le.j_nrow)then
		call jlplex(p_lz,p_lz0,p_lzi(p_leavec-p_nrow),p_lzi)
		p_lz0=p_lz0-1
	else if(p_leavec.le.(p_nrowz+p_mxd)) then !if(p_leavec.le.j_nrow)then
		! on jo vaihdettu
		! if(p_p)write(p_n16,*)'d leaves,	col in d=',p_leaved,' next', &
		! p_next(0),p_next(p_next(0)),p_next(p_next(p_next(0)))&
		! ,'prev',p_iprev(0),p_iprev(p_iprev(0)), &
		! p_iprev(p_iprev(p_iprev(0))),'unit=',p_lunit(p_leaved)
		call jlplex(p_lx,p_lx0,p_lxi(p_leaved),p_lxi)
		p_lx0=p_lx0-1
		! drop
		p_next(p_iprev(p_leaved))=p_next(p_leaved)
		p_iprev(p_next(p_leaved))=p_iprev(p_leaved)
		! if(p_p)then
		! write(p_n16,*)'p_leaved,id,next',p_leaved,id
		! write(p_n16,*)'next',p_next(0),p_next(p_next(0)), &
		! p_next(p_next(p_next(0))), p_next(p_next(p_next(p_next(0))))
		! write(p_n16,*)'iprev',p_iprev(0),p_iprev(p_iprev(0)),  &
		! p_iprev(p_iprev(p_iprev(0))),p_iprev(p_iprev(p_iprev(p_iprev(0))))
		! endif !if(p_p)  12138
 
		! !lunw:n päivitys
		! if(p_fpresent)then
		! do ilu_ = 1,p_lunits0
		! if (p_lunw(ilu_)==p_leaved) then
		! if (p_lunit(p_leaved) == p_lunit(p_next(p_leaved))) then
		! p_lunw(ilu_)= p_next(p_leaved)
		! else !if (j_lunit(p_leaved) == j_lunit(j_next(p_leaved))) then
		! if (p_lunxkf(ilu_)==p_mxd) then
		! do i_ = ilu_+1,p_lunits0
		! p_lunw(i_-1) = p_lunw(i_)
		! p_lunxkf(i_-1) = p_lunxkf(i_)
		! enddo !i_ = ilu_+1,p_lunits0  11747
		! p_lunw(p_lunits0) = 0
		! p_lunxkf(p_lunits0) = p_mxd
		! p_lunits0 = p_lunits0-1
		! else !if (j_lunxkf(ilu_)==j_mxd) then
		! p_lunw(ilu_)=0
		! endif !if (p_lunxkf(ilu_)==p_mxd)  11746
		! endif !if (p_lunit(p_leaved) == p_lunit(p_next(p_leaved)))  11743
		! endif !if (p_lunw(ilu_)==p_leaved)  11742
		! enddo !ilu_ = 1,p_lunits0  11741
		! endif !if(p_fpresent)  11740
 
		! ! !!!test
		! if(p_testl)then
		! do i=1,p_nrow
		! if(p_ls(i).gt.p_nrowz)then
		! do j=1,p_lx0
		! if(p_ls(i)-p_nrowz.eq.p_lx(j))goto 7188
		! enddo !j=1,p_lx0  11766
		! write(p_n16,*)'*colnot*',p_ls(i),'not in',	  (p_lx(j),j=1,p_lx0)
		! !		close(16)
		! write(6,*)'*colnot* return'
		! return
		! 7188 	    continue
		! endif !if(p_ls(i).gt.p_nrowz)  11765
		! enddo !i=1,p_nrow  11764
		! endif !if(p_testl)  11763
		! lista uniteista
 
		!p_leavec tehdassrk??
	else !if(p_leavec.le.j_nrow)then
		! call lexf(p_lf,p_lf0,p_lfi(p_leaved),p_lfi,p_mxd+1,2*p_mxd)
		! !!ld0=ld0-1
		! p_lf0=p_lf0-1
		! ! drop
		! p_nextf(p_iprevf(p_leaved,p_ixkf(p_leaved)),p_ixkf(p_leaved))=p_nextf(p_leaved,p_ixkf(p_leaved))
		! p_iprevf(p_nextf(p_leaved,p_ixkf(p_leaved)),p_ixkf(p_leaved))=p_iprevf(p_leaved,p_ixkf(p_leaved))
 
		! !lunxkf päivitys
		! do ilu_ = 1,p_lunits0
		! if (p_lunxkf(ilu_)==p_leaved) then
		! if (p_lunit(p_leaved) == p_lunit(p_nextf(p_leaved,p_ixkf(p_leaved)))) then
		! p_lunxkf(ilu_)= p_nextf(p_leaved,p_ixkf(p_leaved))
		! else !if (j_lunit(p_leaved) == j_lunit(j_nextf(p_leaved,j_ixkf(p_leaved)))) then
		! if (p_lunw(ilu_)==0) then
		! do i_ = ilu_+1,p_lunits0
		! p_lunw(i_-1) = p_lunw(i_)
		! p_lunxkf(i_-1) = p_lunxkf(i_)
		! enddo !i_ = ilu_+1,p_lunits0  11795
		! p_lunw(p_lunits0) = 0
		! p_lunxkf(p_lunits0) = p_mxd
		! p_lunits0 = p_lunits0-1
		! else !if (j_lunw(ilu_)==0) then
		! p_lunxkf(ilu_)=p_mxd
		! endif !if (p_lunw(ilu_)==0)  11794
		! endif !if (p_lunit(p_leaved) == p_lunit(p_nextf(p_leaved,p_ixkf(p  11791
		! endif !if (p_lunxkf(ilu_)==p_leaved)  11790
		! enddo !ilu_ = 1,p_lunits0  11789
		! !	if (pp) write(p_n16,*)"**fact** lunxkf päivitetty <8088>",(p_lunw(jj7),jj7=1,p_lunits0)
 
	endif !if(p_leavec.le.p_nrow)  11266
 
	if(p_p)write(6,*)'retrun from leaving'
 
end subroutine leaving !subroutine leaving()
 
subroutine zleavtmax()
	!computes
	use fletdmod
	do j=1,p_lz0  !!!! z leaving
		!same as p_lr0 but now x cannot be negative
		p_leaz=p_nrow+p_lz(j)
		if(r(p_leaz).gt.p_tiny78)then
			if(p_x(p_leaz).lt.p_tmax*r(p_leaz))then
				p_tmax=p_x(p_leaz)/r(p_leaz) ;p_rcur=r(p_leaz)
				p_leavec=p_leaz
			endif !if(p_x(p_leaz).lt.p_tmax*r(p_leaz))  11377
		endif !if(r(p_leaz).gt.p_tiny78)  11376
	enddo !j=1,p_lz0  11373
end subroutine zleavtmax
 
subroutine sleavtmax()
	use fletdmod
	do j=1,p_lx0  !!!!sched leaving
		!same as lz
		!		if(p_pivot.gt.76652)write(6,*)'pi vot ',p_pivot
		p_lead=p_nrowz+p_lx(j)
		!	if(p_pivot.gt.76652)write(6,*)'pivot ',p_pivot ,'j lead ',j,p_lead,r(p_lead),p_tiny78
		if(r(p_lead).gt.p_tiny78)then     !
			!		if(p_pivot.gt.76652)write(6,*)'tas ',p_x(p_lead),p_tmax,r(p_lead),p_tmax*r(p_lead),p_x(p_lead).lt.p_tmax*r(p_lead)
			if(p_x(p_lead).lt.p_tmax*r(p_lead))then
				p_tmax=p_x(p_lead)/r(p_lead)  ;p_rcur=r(p_lead)
				p_leavec=p_lead  !!!!
				!			if(p_pivot.gt.76652)write(6,*)'tmax ',p_tmax,p_rcur,p_leavec,'piunit',p_unit
			endif !if(p_x(p_lead).lt.p_tmax*r(p_lead))  11394
 
		endif !if(r(p_lead).gt.p_tiny78)  11392
	enddo !j=1,p_lx0  11387
end subroutine sleavtmax
 
subroutine fleavtmax()
	use fletdmod
	do j=p_mxd+1,p_lf0
		!same as lz
		leaf=p_nrowz+p_lf(j)
		if(r(leaf).gt.p_tiny78)then
			if(p_x(leaf).lt.p_tmax*r(leaf))then
				p_tmax=p_x(leaf)/r(leaf) ;p_rcur=r(leaf)
				p_leavec=leaf
				!if(p_) write(p_n16,*)'**fact** <6151> tmax päivitetty', p_tmax
			endif !if(p_x(leaf).lt.p_tmax*r(leaf))  11410
 
		endif !if(r(leaf).gt.p_tiny78)  11409
	enddo !j=p_mxd+1,p_lf0  11406
 
end subroutine fleavtmax
subroutine rleavtmax()
	use fletdmod
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
		!this can be tested by
		! abs(x)<tmax*abs(r)
 
		if(r(p_lr(j)).gt.p_tiny78)then !tole(lr(j)))then
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
 
			if(p_ubou(p_lr(j)))then
				!we are in the attraction zone of the constraint
				!  if(ubou(lr(j)).and.x(lr(j))+rhs2(lr(j))-rhscur(lr(j))-tmax*r(lr(j)).lt.-tole(lr(j)))then
				! if upper bound is not active make it first
				! if there is no upper boun then residual can freely become negative
				!mutta jos resid<0
				!  row +resid=rhs2
 
				!jos resid<0 muutta yläraja aktiivinen tämä on epäloogista ja kantaan tuleva muuttuja
				! lisää ratkaisun epäloogisuutta ja vaihtoehtoja on kaksi luovutaan koko yrityksestä tain
				! otetaan residuaali pois kannasta
 
				if(p_lower(p_lr(j)).and.p_x(p_lr(j))+ &
						p_rhs2(p_lr(j))-p_rhscur(p_lr(j)).lt.r(p_lr(j))*p_tmax)then
 
					p_rhscur(p_lr(j))=p_rhs2(p_lr(j))
					if(p_xpresent)p_rhsw(p_lr(j))=p_rhscur(p_lr(j))-p_xps(p_lr(j))
					p_x(p_lr(j))=p_x(p_lr(j))+p_rhs2(p_lr(j))-p_rhs(p_lr(j)) !
 
					!	if(p_lr(j).eq.2)write(6,*)'<777tas ',p_x(p_lr(j))
 
					p_lower(p_lr(j))=.false.
					p_tmax=p_x(p_lr(j))/r(p_lr(j)) ;p_rcur=r(p_lr(j)) !toisin päin
					!if(p_)write(p_n16,*)'post,posr,ubou,lower,lr(j),tmax',p_lr(j),p_tmax
					p_leavec=p_lr(j)
				else if( p_x(p_lr(j)).ge.0.and..not.p_lower(p_lr(j)))then !if(j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs2(j_lr(j))-j_rhscur(j_lr(j)).lt.r(j_lr(j))*j_t
					!if(j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs2(j_lr(j))-j_rhscur(j_lr(j)).lt.r(j_lr(j))*j_tmax)then
					! x+row=rhs2 and if r is positive row increases and x decreases until t=x/r
					if(p_x(p_lr(j)).lt.p_tmax*r(p_lr(j)) )then
						p_tmax=p_x(p_lr(j))/r(p_lr(j)) ;p_rcur=r(p_lr(j))
						!if(p_)write(p_n16,*)'post,posr,ubou,NOTlower,lr(j),tmax',p_lr(j),p_tmax
						p_leavec=p_lr(j)
					end if !if(p_x(p_lr(j)).lt.p_tmax*r(p_lr(j)) )  11506
				elseif(p_x(p_lr(j)).lt.j_0.and..not.p_lower(p_lr(j)).and.p_feasible)then !if(j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs2(j_lr(j))-j_rhscur(j_lr(j)).
 
					!there is no lower bound thus if residual is in basis it should be postitive
					!but because of the rounding errors it is negative
					! if r is positive residuasl becomes more negative
					! if residual
					! x+row=rhs2 so it should
					!	if(j_x(j_lr(j)).lt.j_tiny78n)write(6,*)'row ',j_lr(j),' illegal residual(-)',j_x(j_lr(j)),' pivot=',p_pivot,&
					!		'r=',r(j_lr(j)),' p_ienter=',p_ienter
					! if(p_p.or.p_p9.or.p_p8)write(p_n16,*)'row ',p_lr(j),' illegal residual(-)', &
					! p_x(p_lr(j)),' pivot=',p_pivot,&
					! 'r=',r(p_lr(j)),' p_ienter=',p_ienter
 
					p_leavec=p_lr(j)
					p_tmax=-1.d0 ;p_rcur=j_0
					!p=.true.
					! if(p_p.or.p_p9.or.p_p8)write(p_n16,*)'negat resid jump*****r,x**',p_lr(j),r(p_lr(j)), &
					! p_x(p_lr(j))
					!call isfeasible()
					!stop 871
					p_goto112233=.true.
					return
					!goto 2244
 
				end if !if(p_lower(p_lr(j)).and.p_x(p_lr(j)  11490
			end if !if(p_ubou(p_lr(j)))  11478
 
		elseif(r(p_lr(j)).lt.p_tiny78n) then !if(r(j_lr(j)).gt.j_tiny78)then
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
 
			!if(p_pp)write(p_n16,*)'**hep**,x,sum',j_x(j_lr(j)),j_x(j_lr(j))+j_rhs(j_lr(j))-j_rhscur(j_lr(j))
 
			if(p_lbou(p_lr(j)))then
				! if the upper bound if active, can make
				if(.not.p_lower(p_lr(j)).and. &
					p_x(p_lr(j))+p_rhs(p_lr(j))-p_rhscur(p_lr(j)).gt. &
						r(p_lr(j))*p_tmax)then
					p_rhscur(p_lr(j))=p_rhs(p_lr(j))
					if(p_xpresent) p_rhsw(p_lr(j))=p_rhscur(p_lr(j))-p_xps(p_lr(j))
					p_x(p_lr(j))=p_x(p_lr(j))+p_rhs(p_lr(j))-p_rhs2(p_lr(j))
 
 
 
					p_tmax=p_x(p_lr(j))/r(p_lr(j)) ;p_rcur=r(p_lr(j))
					p_lower(p_lr(j))=.true.
					!if(p_pp)write(p_n16,*)'post,negr,lbou,NOTlower,lr(j),tmax',j_lr(j),j_tmax
					p_leavec=p_lr(j)
				elseif(p_x(p_lr(j)).le.j_0.and.p_lower(p_lr(j)))then !if(.not.j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs(j_lr(j))-j_rhscur(j_lr(j)).gt.r(j_lr(j))*j_tm
 
					if(p_x(p_lr(j)).gt.p_tmax*r(p_lr(j)) )then  !-x/-r <tmax-tole -x<-r*(tmax-tole)
						p_tmax=p_x(p_lr(j))/r(p_lr(j)) ;p_rcur=r(p_lr(j))
						!if(p_pp)write(p_n16,*)'post,negr,ubou,lower,lr(j),tmax',j_lr(j),j_tmax
						p_leavec=p_lr(j)
					end if !if(p_x(p_lr(j)).gt.p_tmax*r(p_lr(j)) )  11580
 
				elseif(p_x(p_lr(j)).gt.j_0.and.p_lower(p_lr(j)).and.p_feasible)then !if(.not.j_lower(j_lr(j)).and.j_x(j_lr(j))+j_rhs(j_lr(j))-j_rhscur(j_lr(j)).g
 
					if(p_x(p_lr(j)).gt.p_tiny78)write(6,*)'row ',p_lr(j),' illegal residual (+) ',p_x(p_lr(j)), &
						' pivot=',p_pivot,&
						'r=',r(p_lr(j)),' p_ienter=',p_ienter
					! if(p_p.or.p_p9)write(p_n16,*)'row ',p_lr(j),' illegal residual (+) ', &
					! p_x(p_lr(j)),' pivot=',p_pivot,&
					! 'r=',r(p_lr(j)),' p_ienter=',p_ienter
					p_leavec=p_lr(j)
					!p=.true.
					p_tmax=-1.d0 ;p_rcur=j_0
					! if(p_p.or.p_p9)write(p_n16,*)'posit resid jump*****r,x**', &
					! p_lr(j),r(p_lr(j)),p_x(p_lr(j))
					!call isfeasible()
					!stop 873
					p_goto112233=.true.
					return
					!	goto 2244
				endif !if(.not.p_lower(p_lr(j)).an  11565
 
			end if !if(p_lbou(p_lr(j)))  11563
 
		end if !if(r(p_lr(j)).gt.p_tiny78)  11437
 
	end do !j=1,p_lr0  11422
 
end subroutine rleavtmax
 
subroutine skeyleav()
	use fletdmod
	p_lcur=p_next(0)   !link to first
 
	p_lcursame=0 !!!! place where to put new entering schdelu if the same unit
	! p_newc -is coming column , p_newa in A, p_newd in D
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
	p_wsu=p_onen
	p_rs=p_zero
	p_lcur0=p_lcur  ! link to first column in current unit
	! lunit etc column number in A
	!if(p_pp)write(p_n16,*)'units',j_lunit(j_next(0)),j_lunit(j_next(j_next(0))),j_lunit(j_next(j_next(j_next(0)))),&
	!j_lunit( j_next(j_next(j_next(j_next(0)))))
 
	do i=1,p_lx0
		nex=p_next(p_lcur)        ! next  follows numbering in D
 
		p_rs=p_rs+r(p_lcur+p_nrowz)  ! has opposite sign than in old JLP
		p_wsu=p_wsu+p_x(p_lcur+p_nrowz)
 
		if(p_lunit(p_lcur).eq.p_unit.and.p_lcursame.eq.0)p_lcursame=p_lcur
		if(p_lunit(nex).ne.p_lunit(p_lcur))then
			! the next col has different unit, do final checking
			! formulas  6.59  6.60
			if(p_lunit(p_lcur).eq.p_unit.and.p_ienter.eq.3)then
				! the unit is the same as the unit of the enetering schedule
				!if(p_pp)	write(p_n16,*)'rs-1',j_rs,j_rs-1.
				p_rs=p_rs-p_one
				! store link to be used so that we can put the entering
				! column together with other columns isn the next-sequence
			endif !if(p_lunit(p_lcur).eq.p_unit.and.p_ienter.eq.3)  11653
 
			if(p_rs.lt.p_tiny78n.and.p_wsu.gt.p_tmax*p_rs)then !!!!
				! wsu negat  wsu/rs<tmax  rs<0 =>wsu>tmax*rs
				!					t=wsu/rs
				p_tmax=p_wsu/p_rs    ;p_rcur=p_rs!!!!
				p_leavk=p_lcur0
				!if(p_pp)write(p_n16,*)'**tmax/key,wsu,rs',j_tmax,j_wsu,j_rs,' unit, p_lcur',j_lunit(p_lcur),p_lcur
				! p_leavk is the first D-column in unit for whcik key is leaving
				!					end if
			endif !if(p_rs.lt.p_tiny78n.and.p_wsu.gt.p_tmax*p_rs)  11661
 
			p_wsu=p_onen   !opposite sign of ws as in old JLP
			p_rs=p_zero
			p_lcur0=p_next(p_lcur)   ! p_lcur0 is again the first col in the next new unit
			! last
		endif !if(p_lunit(nex).ne.p_lunit(p_lcur))  11650
		p_lcur=nex
	enddo !i=1,p_lx0  11643
 
 
end subroutine skeyleav
 
subroutine fkeyleav()
	use fletdmod
	! if(p_fpresent) then
	p_leavkf=0
 
	p_leavkwf = 0
	p_lcursamef = 0
 
	p_unit_lkf = 0
	p_ixk_lkf = 0
 
	!käydään läpi vain kannassa olevat xkf-mjat
 
	!tehdaskantasrkeiden läpikäynti nextf:n avulla
	do p_ixk_=1,p_nlog
 
		p_lcur0_unit = p_nextf(p_mxd,p_ixk_)
		do while (p_lcur0_unit > p_mxd)
 
			p_lunit_ = p_lunit(p_lcur0_unit)
 
			!xk-mjan paikka x-matriisissa
			p_ikeepxk_ = p_ixkkeep(p_ixk_)
			!if(p_pp)write(16,*)'ioo',p_lunit_,j_ibaunit(p_lunit_)+ j_keys(p_lunit_)
			iba1=ibaxdat(p_ibaunit(p_lunit_)+ p_keys(p_lunit_)) !,1)
 
			p_wsu=j_o(p_ivdatamatx)%d(iba1+p_ikeepxk_) !ibamatx
			!if(p_)write(16,*)'h1',p_wsu
			p_rs=p_zero
 
			if ((p_ienter==4).and.(p_unit==p_lunit_).and.(ixk_==ixkenter)) then
				p_rs=p_rs+p_one
				p_lcursamef = p_lcur0_unit
 
				!if(p_p) write(p_n16,*)'**fact** <6623> kantasrk:een xkf tulossa kantaan: p_unit,ixk,p_lcursamef',&
				!	p_lunit_,ixk_,p_lcursamef
			endif !if ((p_ienter==4).and.(p_unit==p_lunit_).and.(ixk_==ixkent  11712
 
			p_lcur_=p_lcur0_unit
 
			do while (p_lunit(p_lcur_) == p_lunit(p_lcur0_unit))
				p_rs = p_rs - r(p_lcur_+p_nrowz)  ! has opposite sign than in old JLP
				p_wsu = p_wsu - p_x(p_lcur_+p_nrowz)
				!if(p_)write(16,*)'h12',p_wsu
				p_lcur_ = p_nextf(p_lcur_,ixk_)
			enddo !while (p_lunit(p_lcur_) == p_lunit(p_lcur0_unit))  11722
 
			! s.12 kaava (wij enters)
			if ((p_ienter==3).and.(p_unit==p_lunit_)) then
				ikey_ = p_ibaunit(p_lunit_) + p_keys(p_lunit_)
				iopt_ = p_ibaunit(p_lunit_) + p_iopt
 
				ibxdatkey_=ibaxdat(ikey_) !,2)
				ibxdatopt_=ibaxdat(iopt_) !,1)
				!	if(p_p.or.p_tmax.eq.-1.d0) write(p_n16,*)'**fact** <6741> wij tulossa kantaan: p_unit,p_iopt,ikey',&
				!		p_unit,p_iopt,ikey_
				!
				!		j_o(p_ivdatamatx)%r(ibamatx(ikey_)+p_ikeepxk_)
 
				p_rs=p_rs - &
					(j_o(p_ivdatamatx)%d(ibxdatopt_+p_ikeepxk_) -  & !ibamatx
					j_o(p_ivdatamatx)%d(ibxdatkey_+p_ikeepxk_))          !!ibamatx
 
			endif !if ((p_ienter==3).and.(p_unit==p_lunit_))  11730
 
			! tehdaskantasarakketta vastaavan yksikön 1. ve-kantasrk
			lcur_w= 0
			lun_= 1
			do while((p_lunit(p_lunw(lun_))/=p_lunit_).and.(lun_<=p_lunits0))
				lun_=lun_+1
			enddo !while((p_lunit(p_lunw(lun_))/=p_lunit_).and.(lun_<=p_lunit  11750
			!kannassa yksikön ve-srkeita
			lcur0_w = 0
			if(lun_ <= p_lunits0) then
 
				lcur0_w = p_lunw(lun_)
				ikey_	= p_ibaunit(p_lunit(lcur0_w)) + p_keys(p_lunit(lcur0_w))
 
				ibxdatkey_=ibaxdat(ikey_)! ,2)
 
				lcur_w = lcur0_w
				do while (p_lunit(lcur_w) == p_lunit(lcur0_w))
					iobs_ = p_ibaunit(p_lunit(lcur0_w)) + p_isch(lcur_w)
 
					ibxdatobs_=ibaxdat(iobs_) !,1)
					! if(p_p.or.p_tmax.eq.-1.d0)write(16,*)'tas',ikey_, &
					! j_o(p_ivdatamatx)%d(ibxdatkey_+1:ibxdatkey_+p_nkeepx)
					! if(p_p)write(16,*)'ta2',iobs_,j_o(p_ivdatamatx)%d(ibxdatobs_+1:ibxdatobs_+p_nkeepx)
					xkij_=j_o(p_ivdatamatx)%d(ibxdatobs_+p_ikeepxk_)   !ibamatx
 
 
					p_rs=p_rs+r(lcur_w+p_nrowz)*&
						(xkij_ - j_o(p_ivdatamatx)%d(ibxdatkey_+p_ikeepxk_))
					p_wsu=p_wsu+p_x(lcur_w+p_nrowz)*&
						(xkij_ - j_o(p_ivdatamatx)%d(ibxdatkey_+p_ikeepxk_))
					!if(p_)write(16,*)'h3',p_wsu
					!write(nu20,*)'hep2',j_o(p_ivdatamatx)%r(ibxdatkey_+1:ibxdatkey_+10)
					!write(nu20,*)j_o(p_ivdatamatx)%r(ibxdatkey_+p_ikeepxk_)
					lcur_w = p_next(lcur_w)
				enddo !while (p_lunit(lcur_w) == p_lunit(lcur0_w))  11763
 
			endif !if(lun_ <= p_lunits0)  11755
 
			! if (p_p) then
			! if(p_wsu<j_0)write(p_n16,*)'**fact** <6738> negat. wsu'
			! endif !if (p_p)  11659
			!wsu-rs kombinaatiot
			! wsu >0, rs >0 -> t = wsu/rs
			! wsu >0, rs <0 -> t < 0 --> ei käsitellä
			! wsu <0, rs >0 -> t < 0 --> ei käsitellä,  käsitelläänpä JL 27.8.2018
			! wsu <0, rs <0 -> t = wsu/rs
			! rs = 0 -> ei käsitellä
			! wsu = 0, rs >0 -> t = wsu/rs = 0 --> tmax = 0
			! wsu = 0, rs <0 -> t = wsu/rs = 0 --> ei käsitellä
			!	if(j_lunit(p_lcur0_unit).eq.121.and.ixk_.eq.16)write(26,*)&
			!	j_wsu,j_rs,j_wsu/j_rs,j_tmax,j_tiny78,j_rs*j_wsu>j_zero,&
			!	(Abs(j_rs)>j_tiny78).and.(((j_rs*j_wsu)>j_zero).or.(j_wsu.eq.j_zero.and.j_rs>j_zero)).and.&
			!		Abs(j_wsu).lt.j_tmax*Abs(j_rs)
			if(p_tmax.eq.-1.d0) then
				write(6,*)'<7681> wsu,rs,wsu/rs,tmax,p_lcur0_unit,lcur0_w,lunit(p_lcur0_unit),ixk_',&
					p_wsu,p_rs,p_wsu/p_rs,p_tmax,p_lcur0_unit, &
					lcur0_w,p_lunit(p_lcur0_unit),ixk_
			endif !if(p_tmax.eq.-1.d0)  11800
			!	if((Abs(j_rs)>j_tiny78).and.(((j_rs*j_wsu)>j_zero).or.(j_wsu.eq.j_zero.and.j_rs>j_zero)).and.&
			!		Abs(j_wsu).lt.j_tmax*Abs(j_rs))then
			if(p_rs>p_tiny78.and.p_wsu.lt.p_tmax*p_rs)then !changed 27.8.2018 JL
				p_tmax=p_wsu/p_rs ;p_rcur=p_rs
				p_leavkf = p_lcur0_unit    ! ehkä kannassa olevat xkf-muuttujat ko yksikölle
 
				p_leavkwf = lcur0_w      ! kannassa olevat vaihtoehtosarakkeet ko yksikölle
				!	write(nu20,*)'<22>',p_leavkwf
				p_leave=0
				p_leavk=0
				p_unit_lkf = p_lunit(p_lcur0_unit)
				ixk_lkf = ixk_
				ikeepx_lkf = p_ikeepxk_
				! if(p_p.or.p_tmax.eq.-1.d0) write(p_n16,*)'**fact** <6678> **tmax/key,wsu,rs',&
				! p_tmax,p_wsu,p_rs,'p_leavkf, unit, ixk',p_leavkf, &
				! p_lunit(p_leavkf), ixk_
			endif !if(p_rs>p_tiny78.and.p_wsu.lt.p_tmax*p_rs)  11807
 
			p_lcur0_unit = p_lcur_
		enddo !while (p_lcur0_unit > p_mxd)  11699
	enddo !p_ixk_=1,p_nlog  11696
 
end subroutine fkeyleav
 
subroutine jotainf()
 
	! if(p_tmax.le.p_tmaxmin)then ! eq.j_0)then
	! ! if(p_ienter.eq.3.and.p_secondb.gt.p_valuek+p_tolecur)then
	! ! if(p_pp.or.p_tmax.eq.-1.d0)write(16,*)'secondb2 ',secondb,p_valueopt, &
	! ! p_valuek,p_pivot,p_unit
	! ! p_iopt=p_isecond
	! ! p_valueopt=secondb
	! ! secondb=p_small
	! ! goto222=.true.;return
	! ! endif !if(p_ienter.eq.3.and.secondb.gt.j_valuek+j_tolecur)then
	! endif !if(p_tmax.le.p_tmaxmin)  12149
 
	if ((p_ienter==4).and.(p_lcursamef==0)) then
		ikeepxkenter_ = p_ixkkeep(ixkenter)
 
		ibas1=ibaxdat(p_ibaunit(p_unit)+ p_keys(p_unit)) !,1)
		p_wsu=j_o(p_ivdatamatx)%d(ibas1+ikeepxkenter_)  !ibamatx
		if(p_tmax.eq.-1.d0)write(6,*)'h4*',p_wsu
		p_rs=p_one
 
		! kantaan tulevan yksikön 1. ve-srk
		p_lcur_= 0
		lun_= 1
		do while((p_lunit(p_lunw(lun_))/=p_unit).and.(lun_<=p_lunits0))
			lun_=lun_+1
		enddo !while((p_lunit(p_lunw(lun_))/=p_unit).and.(lun_<=p_lunits0  11853
 
		lcur0_w = 0
		!kannassa yksikön ve-srkeita
		if(lun_ <= p_lunits0) then
 
			lcur0_w = p_lunw(lun_)
			ikey_	= p_ibaunit(p_lunit(lcur0_w)) + p_keys(p_lunit(lcur0_w))
			ibxmatkey_=ibamatx(ikey_) !,1)
 
			ibxdatkey_=ibaxdat(ikey_) !,2)
			p_lcur_ = lcur0_w
			do while (p_lunit(p_lcur_) == p_lunit(lcur0_w))
				iobs_ = p_ibaunit(p_lunit(lcur0_w)) + p_isch(p_lcur_)
				ibxdatobs_=ibaxdat(iobs_) !,1)
				xkij_=j_o(p_ivdatamatx)%d(ibxdatobs_+ikeepxkenter_) !ibamatx
				p_rs=p_rs+r(p_lcur_+p_nrowz)*&
					(xkij_ - j_o(p_ivdatamatx)%d(ibxdatkey_+ikeepxkenter_))  !ibamatx
				p_wsu=p_wsu+p_x(p_lcur_+p_nrowz)*&
					(xkij_ - j_o(p_ivdatamatx)%d(ibxdatkey_+ikeepxkenter_))  !ibamatx
				!if(p_)write(16,*)'h5',p_wsu
				p_lcur_ = p_next(p_lcur_)
			enddo !while (p_lunit(p_lcur_) == p_lunit(lcur0_w))  11867
 
		endif !if(lun_ <= p_lunits0)  11859
 
		!if(p_)write(p_n16,*)'**fact** <6695> (p_lcursamef==0) wsu,rs', p_wsu,p_rs
		p_degeneratef=(p_wsu==p_zero)
		if (p_p.and.p_degeneratef) write(p_n16,*)'**fact** <6733> degenerate = true'
 
		if((p_rs>p_tiny78).and.(p_wsu.lt.p_tmax*p_rs))then
			p_tmax=p_wsu/p_rs  ;p_rcur=p_rs
			p_leavkf = 0
			p_leavkwf = 0
			p_leave=0
			p_leavk=0
			if(p_tmax.eq.-1.d0) write(6,*)'**fact** <6743> **tmax/ei_xkf,wsu,rs',&
				p_tmax,p_wsu,p_rs,'p_leavkf, unit, ixk',p_leavkf, p_unit, ixkenter
		endif !if((p_rs>p_tiny78).and.(p_wsu.lt.p_tmax*p_rs))  11885
 
	endif !if ((p_ienter==4).and.(p_lcursamef==0))  11842
 
	if ((p_ienter==4).and.(p_leavkf==0).and.(p_leave==0).and.(p_leavk==0)) then
		! lun_= 1
		! do while((p_lunit(p_lunw(lun_))/=p_unit).and.(lun_<=p_lunits0))
		! lun_=lun_+1
		! enddo !while((p_lunit(p_lunw(lun_))/=p_unit).and.(lun_<=p_lunits  12237
		! if (lun_<=p_lunits0) then
		! p_leavkwf = p_lunw(lun_)
		! p_unit_lkf=p_unit
		! ixk_lkf = ixkenter
 
		! ikeepx_lkf = p_ixkkeep(ixkenter)
		! if(p_p) then
		! write(p_n16,*) '<7325> Muuta ei näytä tapahtuvan, vaihdetaan avaintehdas'
		! if (p_leavk >0) write(p_n16,*) '<7325> ....paitsi että  myös avainve vaihtuu...'
		! endif !if(p_p)  11778
		!endif !if (lun_<=p_lunits0)  12240
	endif !if ((p_ienter==4).and.(p_leavkf==0).and.(p_leave==0).and.(  11897
	!endif !if(p_fpresent)  12006
end subroutine
 
 
subroutine leaveskeys()
	! keyschedule leaves
	! there are two cases: there are no explicit basic schedules in the unit
	use fletdmod
	use fletdmod2
	use fletcherdmod
	common/refactorc/nup,nfreq
	!if(p_leavk.gt.0)then
	p_nkeys=p_nkeys+1
	!if(p_.and.p_fpresent) write(p_n16,*)'**fact** AVAINVE VAIHTUU>> '
	!if(p_pp)write(p_n16,*)&
	!'**leaving key,d-column,unit,neun',p_leavk,j_lunit(p_leavk),p_unit
 
	! put to next free column
	! old d= x-xkey, x=d+xkey
	! new d= x-xnewkey=d+xoldkey-xnewkey
 
	! p_lcur0 column of the
	!  change first xps:
 
	! subtract old key add new key
	! leyvk-columd in D is newkey-oldkey, the corresponding element in objr contains
	! also newkey-oldkey
	! xps is sum of x:s of keys
	! newxps=oldxps-oldkey+newkey= oldxps+(newkey-oldkey)
	! and newkey-oldkey is already stored in a and objr
 
	p_xps(0)=p_xps(0)+p_objr0(p_leavk+p_nrowz)
	! p_leavk column contains the inforamtion
	! if(sparse)then
	! icol=p_leavk+p_nz
	! iel=0
	! do i=p_lavecsp(p_lavecsp(0)+icol),last(icol)
	! iel=iel+1
	! j=p_lavecsp(i)
	! p_xps(j)=p_xps(j)+p_a(iel,icol)
	! p_rhsw(j)=p_rhscur(j)-p_xps(j)
	! enddo !i=p_lavecsp(p_lavecsp(0)+icol),last(icol)  20008
	! else !if(sparse)then
	do j=1,p_nrow
		!	if((p_ix(j).ne.0).or.p_fpresent)p_xps(j)=p_xps(j)+p_a(j,p_leavk+p_nz) !
 
		!		write(6,*)'tasp_leavk ',p_leavk
 
 
		if((p_ix(j).ne.0).or.p_fpresent)p_xps(j)=p_xps(j)+p_a(p_abas(p_leavk+p_nz)+j)
		p_rhsw(j)=p_rhscur(j)-p_xps(j)
	enddo !j=1,p_nrow  11957
 
	!	endif !if(sparse)  20005
 
	!	if(p_p2)write(p_n16,*)'xpsher',(p_xps(j),j=0,p_nrow)
 
	! if the entering schedule is in the same unit then also the entering
	if(p_unit.eq.p_lunit(p_leavk).and.p_ienter.eq.3)then
		! in objr also the residual section and z section are included
		p_objr0(p_newc)=p_objr0(p_newc)-p_objr0(p_leavk+p_nrowz)
		! if(sparse)then
		! call jlpgetcol(p_newa)
		! call jlpgetcol(p_leavk+p_nz)
		! call jlpsubcol(p_newa,p_leavk+p_nz,p_newa)
 
		! else !if(sparse)then
 
		do j=1,p_nrow
			!p_a(j,p_newa)=p_a(j,p_newa)-p_a(j,p_leavk+p_nz)
			iba=p_abas(p_newa)
			p_a(iba+j)=p_a(iba+j)-p_a(p_abas(p_leavk+p_nz)+j)
		enddo !j=1,p_nrow  11982
		!		endif !if(sparse)  20028
	endif !if(p_unit.eq.p_lunit(p_leavk).and.p_ienter.eq.3)  11972
 
	! check
 
	! if there are more columns in the same unit these must be changed with
	! columns expressed in the new key
 
	! old d= x-key, x=d+key
	! new d= x-newkey=d+key-newkey
	! column p_leave is actually : newkey-key, thus new d is:
	! d- column p_leave
	p_leavec=p_leavk+p_nrowz  ! in col numbering
	p_lcur=p_leavk
	! if(p_p)write(p_n16,*)'yks old key,new (1)', p_keys(p_lunit(p_lcur)),&
	! p_isch(p_leavk)
	p_keys(p_lunit(p_lcur))=p_isch(p_leavk)
	p_nkeys=p_nkeys+1
	! if(p_testl)then
	! if(p_keys(p_lunit(p_lcur)).gt.p_ns(p_lunit(p_lcur)))then
	! write(p_n16,*)'*u*',p_lunit(p_lcur),p_ns(p_lunit(p_lcur)), &
	! 'yrität',p_keys(p_lunit(p_lcur))
	! !		close(16)
	! write(6,*)'*u* return'
	! p_goto900=.true.;return
	! endif !if(p_keys(p_lunit(p_lcur)).gt.p_ns(p_lunit(p_lcur)))  12426
	! endif !if(p_testl)  12425
 
	! intitially p_leavk is first column (in nex-order) in the unit
	! is there ex0plicit schedules in the same unit
 
 
	17 nex=p_next(p_lcur)    ! in d columns
	! we jump here from below to go through all columns in the same unit
	! is=nrow
	! first free column is reserved for p_newc
	if(p_lunit(p_lcur).eq.p_lunit(nex))then
		! if(p_p) then
		! write(p_n16,*)'samma ,nex,',nex,p_lunit(p_lcur)
		! write(p_n16,*)'nex',(p_next(jj7),jj7=0,5),'lunit',(p_lunit(jj7),jj7=0,5)
		! write(p_n16,*)'prev',(p_iprev(jj7),jj7=0,5)
		! endif !if(p_p)  12320
		! is=is+1
		! change the old additional  columns to correspond
		! to the new key
		p_id=p_lx(p_lx0+2) !take new D-column for this
		! note ld0+1 may be used already if schedule is entering
		p_ia=p_id+p_nz  ! ld0 nrow+1 already reserved for entering, p_ia=col in A
		p_icolnew=p_ia+p_nrow  !p_icolnew col number in (I A)
		!if(p_)write(p_n16,*)'newc,p_icolnew',p_newc,p_icolnew,'newid,p_newa',p_id,p_ia,p_nz,p_leavk,nex
		!if(p_)write(p_n16,*)'nex,nex+,nex,p_nrowz,pÄleavec',nex,p_nrowz,p_leavec,p_leavk,p_nz,p_leavk+p_nz
		p_icolold=nex+p_nrowz
		!nex,nex+,nex,p_nrowz,pIeavec           2           5           6           1           0           1
		!if(p_)write(p_n16,*)'p_mxnm',p_mxnm,p_mxn
		p_objr0(p_icolnew)=p_objr0(p_icolold)-p_objr0(p_leavec)
		! if(sparse)then
		! call jlpsubcol(nex+p_nz,p_leavk+p_nz,ia)
		! if(p_p)then
		! call jlpgetcol(p_ia);write(p_n16,*)'**iacol',p_acol(1:min(40,p_nrow))
 
		! endif !if(p_p)  20090
		! else !if(sparse)then
 
		do i=1,p_nrow
			!	if(p_p)write(p_n16,*)'nex+p_nz,p_mxn',nex+p_nz,p_mxn
			!	enddo !i=1,p_nrow  10213
			!		p_a(i,p_ia)=p_a(i,nex+p_nz)-p_a(i,p_leavk+p_nz)
			p_a(p_abas(p_ia)+i)=p_a(i+p_abas(nex+p_nz))-p_a(i+p_abas(p_leavk+p_nz))
		enddo !i=1,p_nrow  12050
		!if(p_p)write(p_n16,*)'iacol',p_a(1:min(p_nrow,50),p_ia)
		!		endif !if(sparse)  20088
		!if(p_p)write(p_n16,*)'pivot7',p_icolold,p_icolnew, 'in d',nex,p_id
		p_lunit(p_id)=p_lunit(p_leavk)
		p_isch(p_id)=p_isch(nex)
		!p_icolold=nex+p_nrowz
		!!Fletcher ??????????
 
		! if leaving colums is the same as mp it is utilizing old computation
		! leaving column is stored into  mp
		! if entering column is mq, pivot is also utilizing old computations
		! entering column is getting the new value
 
		! if(sparse)then
		! mqjjsp=-1   !new not  something known
		! call pivotsp(p_icolold,p_icolnew,p_nrow, &
		! nm,p_a,p_lavecsp,e,wslu1,lwsll1,p_ifail,p_info)  !unix
 
		! else !if(sparse)then
		mqjj=-1   !new not  something known
 
		! if(p_p) then
		! write(p_n16,*) '**Pivot <7484>  nex, nex+nrowz,p_icolnew,nrow', nex, p_icolold,p_icolnew,p_nrow
		! endif !if(p_p)  12373
		if(nup.ge.nfreq)then
			p_refac=p_refac+1
			!if(p_)write(p_n16,*)'*refact',p_refac,nup,nfreq
		endif !if(nup.ge.nfreq)  12080
		!if(p_8)write(6,*)'<464pivot',wslu1
		!if(p_)write(p_n16,*)'#pivot7 ',p_icolold,p_icolnew,p_nrow,p_nm
		call pivot(p_icolold,p_icolnew,p_nrow, &
			p_nm,p_a,p_lavec,e,wslu1,lwsll1,p_ifail,p_info)
		p_pivotcase=1
		j_o(p_ivpivotcases)%i2(1)=j_o(p_ivpivotcases)%i2(1)+1
		!		endif !if(sparse)  20112
		p_pivot=p_pivot+1
		!if(p_.or.p_p9.or.p_p8)write(p_n16,*)'pivot<9646>',p_pivot,1,p_objf,p_tmax
		!if(p_)write(77,*)1,p_pivot,p_objf
		if(p_ifail.ne.0)then
			write(6,*)'****pivot failure (7) ,p_ifail=',p_ifail,' info=',p_info
			j_err=.true.
			return
			! p_nrecover=p_nrecover+1
			! if(p_nrecover.ge.10)then
			! write(6,*)'*jlp* is mixed up, try different tole (e.g.10,100,1000)(or consult J. Lappi)'
			! j_err=.true.
			! p_goto900=.true.;return
 
			! endif !if(p_nrecover.ge.10)  12393
			! write(6,*)'***trying to recover'
			! if(p_p)write(p_n16,*)'***trying to recover'
			! p_kierv=p_kier
			! p_unitv=p_unit
			! p_feasible=.false.
			!p_goto1234=.true.;return
 
		endif !if(p_ifail.ne.0)  12094
		!	if(sparse)then
		!	mpjjsp=-1
		!	else !if(sparse)then
		! mpjj=-1   ! old not something to remember
		!	endif !if(sparse)  20150
 
		! if(p_zmatrix)then
		! ipivoti_=p_pivot/5000
		! if(5000*ipivoti_.eq.p_pivot)write(6,*)'**pivots, objf ',p_pivot, p_objf
 
		! endif !if(p_zmatrix)  12413
		if(p_pivot.eq.p_idebug)then
			p_debug=.true.
			write(6,*)'<1>changing debuggging at pivot=',p_pivot,' into ',p
 
		endif !if(p_pivot.eq.p_idebug)  12124
 
		! if(p_p)then
		! write(p_n16,*)'**pivot=',p_pivot
		! ! if(sparse)then
		! ! call jlpgetcol(nex+p_nz);write(p_n16,*)'leaving col',p_acol
		! ! call jlpgetcol(p_ia);write(p_n16,*)'ent col',p_acol
		! ! else !if(sparse)then
		! !	write(p_n16,*)'lea col',(p_a(jj7,nex+p_nz),jj7=1,p_nrow)
		! !	write(p_n16,*)'ent col',(p_a(jj7,p_ia),jj7=1,p_nrow)
		! !	endif !if(sparse)  20174
		! endif !if(p_p)  11875
		! update ls and ld lists
		call jlplex(p_ls,p_lsi(p_icolold),p_lsi(p_icolnew),p_lsi)
		call jlplex(p_lx,p_lx0+2,p_lxi(nex),p_lxi)
		! if(p_p2)then
		! write(p_n16,*)'ldaf ',(p_lx(jj7),jj7=1,p_lx0)
		! write(p_n16,*)'lsaf ',(p_ls(jj7),jj7=1,5)
		! endif !if(p_p2)  11888
		! nex leaving id entering
		! nex           3           4           0           4           2           0
		! lunit           0          22          22          20          22           0
 
		! p_leavk
		! nex           3           4           0           4           2           0
		! lunit           0          22          22          20          22           0
		! if(p_p)then
		! write(p_n16,*)'before inserting ', p_id ,'next',p_next(0),p_next(p_next(0)),&
		! p_next(p_next(p_next(0))),  p_next(p_next(p_next(p_next(0))))
		! write(p_n16,*)'units',p_lunit(p_next(0)),p_lunit(p_next(p_next(0))),&
		! p_lunit(p_next(p_next(p_next(0)))), p_lunit( p_next(p_next(p_next(p_next(0)))))
		! write(p_n16,*)'iprev',p_iprev(0),p_iprev(p_iprev(0)),p_iprev(p_iprev(p_iprev(0))),&
		! p_iprev(p_iprev(p_iprev(p_iprev(0))))
		! endif !if(p_p)  11899
		p_next(p_iprev(nex))=p_id ! insert the new col in next-
		p_next(p_id)=p_next(nex)
		p_iprev(p_id)=p_iprev(nex)
		p_iprev(p_next(p_id))=p_id
 
		if(p_fpresent)then
			! Päivitetään laskentayksikön vaihtoehtojen alkamissarake lunw:hen
			do ilu_ = 1,p_lunits0
				if (p_lunw(ilu_)==nex) then
					p_lunw(ilu_)=p_id
				endif !if (p_lunw(ilu_)==nex)  12170
			enddo !ilu_ = 1,p_lunits0  12169
		endif !if(p_fpresent)  12167
 
		! if(p_p)then
		! write(p_n16,*)'aft next',p_next(0),p_next(p_next(0)), &
		! p_next(p_next(p_next(0))),  p_next(p_next(p_next(p_next(0))))
		! write(p_n16,*)'iprev',p_iprev(0),p_iprev(p_iprev(0)), &
		! p_iprev(p_iprev(p_iprev(0))),p_iprev(p_iprev(p_iprev(p_iprev(0))))
		! endif !if(p_p)  12470
		p_lcur=p_id
		goto 17
	endif !if(p_lunit(p_lcur).eq.p_lunit(nex))  12023
 
	! now just proceed as if leaving a-col p_leavk
	! p_leave is index for ls
	p_leave=p_lsi(p_leavk+p_nrowz)
	!if(p_)write(p_n16,*)'siis leaving, ind, inc ',p_leavk,p_leave
	p_leaved=p_leavk
 
	!	endif !if(p_leavk.gt.0)  11677
end subroutine leaveskeys
 
subroutine leaveskeyf()
	write(6,*)'leaveskeyf'
end subroutine
 
 
subroutine leafa()
	! use fletdmod
	! use fletdmod2
	! use fletcherdmod
	! if (p_leavkf.gt.p_mxd) then
	! ! put to next free column
	! ! old d= x-xkey, x=d+xkey
	! ! new d= x-xnewkey=d+xoldkey-xnewkey
 
	! ! p_lcur0 column of the
	! !  change first xps:
 
	! ! subtract old key add new key
	! ! leyvk-columd in D is newkey-oldkey, the corresponding element in objr contains
	! ! also newkey-oldkey
	! ! xps is sum of x:s of keys
	! ! newxps=oldxps-oldkey+newkey= oldxps+(newkey-oldkey)
	! ! and newkey-oldkey is already stored in a and objr
 
	! ! xps:n laskenta (s.12 kaava): vähennä vanhan avaintehtaan alfat ja gammat, lisää uuden avaintehtaan alfat ja gammat
	! ! p_leavk column contains the inforamtion
 
	! !xps laskenta, kuun avaintehdas vaihtuu
	! iunit_ = p_lunit(p_leavkf)
 
 
	! ! if(p_p.or.p_p9) then
	! ! write(p_n16,*) 'AT VAI ', &
	! ! p_leavkf, iunit_, p_ixkf(p_leavkf), p_ixkffact(p_leavkf)
	! ! endif !if(p_p.or.p_p9)  12315
	! ibas1=ibaxdat(p_ibaunit(iunit_)+p_keys(iunit_)) !,1)
	! valtas=j_o(p_ivdatamatx)%d(ibas1+ikeepx_lkf)   !ibamatx
	! p_xps(0)=p_xps(0)+p_objr0(p_leavkf+p_nz+p_nrow)*valtas  !&
 
	! do j=1,p_nrow
	! p_xps(j)=p_xps(j)+p_a(j+p_abas(p_leavkf+p_nz))*valtas !&
	! !
	! p_rhsw(j)=p_rhscur(j)-p_xps(j)
	! enddo !j=1,p_nrow  12852
 
	! !	if(p_p2)write(p_n16,*)'xps',(p_xps(j),j=0,p_nrow)
 
	! !objr0, a päivitys kun xk-muuttuja tulee kantaan
	! ! if the entering schedule is in the same unit then also the entering
	! if(p_unit.eq.p_lunit(p_leavkf).and.p_ienter.eq.4.and.(ixkenter==ixk_lkf)) then
	! ! in objr also the residual section and z section are included
	! !	if(p_p)write(p_n16,*)'**fact** <7429> objr0(p_newc),objr0(p_leavkf+nrowz),objr0(p_newc)-objr0(p_leavkf+nrowz)',&
	! !		p_objr0(p_newc),p_objr0(p_leavkf+p_nrowz),p_objr0(p_newc)-p_objr0(p_leavkf+p_nrowz)
	! p_objr0(p_newc)=p_objr0(p_newc)-p_objr0(p_leavkf+p_nrowz)
 
 
 
 
end subroutine leafa
 
subroutine leave0ienter3()
	!	if(p_ienter.eq.3)then
	integer pvars_
	if(p_debug.and.p_fpresent)	write(p_n16,*)'**fact** VAIN AVAINVE VAIHTUU>> '
	! change just key
	iobs=p_ibaunit(p_unit)+p_keys(p_unit)
	ibxmatx=ibamatx(iobs) !,1)
 
	do jj=1,p_nrowcurx
		j=p_rowcurx(jj)
		p_xps(j)=p_xps(j)-p_xmat(p_ix(j)+ibxmatx) ! v(ix(j))
	enddo !jj=1,p_nrowcurx  12263
 
 
 
	iobs=p_ibaunit(p_unit)+p_iopt
	ibxmatx=ibamatx(iobs) !,1)
	do jj=1,p_nrowcurx
		j=p_rowcurx(jj)
		p_xps(j)=p_xps(j)+p_xmat(p_ix(j)+ibxmatx) ! v(ix(j))
 
	enddo !jj=1,p_nrowcurx  12272
 
	do jj=1,p_nrow
		p_rhsw(jj)=p_rhscur(jj)-p_xps(jj)
	enddo !jj=1,p_nrow  12278
 
	if(p_p)write(p_n16,*)'change just key,ne xps', p_xps(0:min(p_nrow,20))
	if(p_p)write(p_n16,*)'*yks old key, new ', p_unit,p_keys(p_unit),p_iopt
	p_keys(p_unit)=p_iopt
	p_nkeys=p_nkeys+1
	p_justkey=.true.
	p_goto55=.true.;return    !!!! just key scedule changed
 
 
end subroutine leave0ienter3
 
subroutine leave0ienter1()
 
	!else if(p_ienter.eq.1)then !if(p_ienter.eq.3)then
 
 
	if((p_lower(p_newc).and..not.p_ubou(p_newc)).or.(.not.p_lower(p_newc).and..not.p_lbou(p_newc))) then
		write(6,*)'jlp() unbounded problem (1)'
 
 
		if(p_feasible)j_v(p_ivfeasible)=1.
		j_v(p_ivunbounded)=1.
		if(p_maxo)then
			j_v(p_ivobjective)=j_inf
		else !if(j_maxo)then
			j_v(p_ivobjective)=j_ninf
		endif !if(p_maxo)  12303
		j_err=.true.
 
		return
	endif !if((p_lower(p_newc).and..not.p_ubou(p_newc)).or.(.not.p_lo  12297
	if(p_lower(p_newc))then
		p_lower(p_newc)=.false.
		p_rhscur(p_newc)=p_rhs2(p_newc)
	else !if(j_lower(p_newc))then
		p_lower(p_newc)=.true.
		p_rhscur(p_newc)=p_rhs(p_newc)
	endif !if(p_lower(p_newc))  12312
	if((p_ix(p_newc).ne.0).or.p_fpresent)then
		p_rhsw(p_newc)=p_rhscur(p_newc)-p_xps(p_newc)
	else !if((j_ix(p_newc).ne.0).or.j_fpresent)then
		p_rhsw(p_newc)=p_rhscur(p_newc)
 
	endif !if((p_ix(p_newc).ne.0).or.p_fpresent)  12319
 
	if(p_debug) write(6,*)'goto 55/täältä'
	p_goto55=.true.;return  !!!!active rhs changed goto main loop
	!vain avaintehdas vaihtuu
	!silmukoiden toteutus, goto 55
 
	!if(p_ienter.eq.3)then
 
 
end subroutine leave0ienter1
 
subroutine leave0ienter4()
	! integer pvars_
	! !else if (p_ienter.eq.4) then
 
	! ! if(p_p)	then
	! ! write(p_n16,*)'**fact** VAIN AVAINTEHDAS VAIHTUU>> unit, ixk, vanha, uusi',&
 
	! p_goto55=.true.;return    !!!!only key factory changed
 
 
 
end subroutine leave0ienter4
 
subroutine leave0else()
 
	if(.not.p_feasible)then
		do jj=1,p_lr0
			if(r(p_lr(jj)).gt.p_tiny78)then
				if(.not.p_lbou(p_lr(jj)).or..not.p_lower(p_lr(jj))) then
					!virheilmoitus ja häivytään
				endif !if(.not.p_lbou(p_lr(jj)).or..not.p_lower(p_lr(jj)))  12354
				p_tmax=p_x(p_lr(jj))/r(p_lr(jj))
				p_rcur=r(p_lr(jj))
				p_leavec=p_lr(jj)
				p_leave=p_lsi(p_leavec)
				p_goto8888=.true.
			endif !if(r(p_lr(jj)).gt.p_tiny78)  12353
			if(r(p_lr(jj)).lt.p_tiny78n)then
				if(.not.p_ubou(p_lr(jj)).or.p_lower(p_lr(jj))) then
					!virheilmoitus ja häivytään
				endif !if(.not.p_ubou(p_lr(jj)).or.p_lower(p_lr(jj)))  12364
				p_tmax=p_x(p_lr(jj))/r(p_lr(jj)) ;p_rcur=r(p_lr(jj))
				p_leavec=p_lr(jj)
				p_leave=p_lsi(p_leavec)
				p_goto8888=.true.
			endif !if(r(p_lr(jj)).lt.p_tiny78n)  12363
		enddo !jj=1,p_lr0  12352
	endif !if(.not.p_feasible)  12351
 
	write(6,*)'jlp() unbounded problem (2)'
 
 
 
	if(p_feasible)j_v(p_ivfeasible)=1.
	j_v(p_ivunbounded)=1.
	if(p_maxo)then
		j_v(p_ivobjective)=j_inf
	else !if(j_maxo)then
		j_v(p_ivobjective)=j_ninf
	endif !if(p_maxo)  12381
	j_err = .true.
	return
	!	endif !if(p_ienter.eq.3)  12601
 
end subroutine leave0else
 
subroutine route67()
	use fletdmod
	use fletdmod2
	use fletcherdmod
	if(p_route67)then
		if(p_newc.gt.p_nrow)then    !jl 20160608
			! If equation Basis matrix*x= entering column is solved i.e. the entering column is
			! expressed as a linear combination of basic columns and the coefficient of the leaving column is
			! close to zero this means that the entering column is linearly dependent on the all non-leaving
			! columns and the new basis would be singular
			call fbsub(p_nrow,p_leave,p_leave,p_a,p_lavec,0,p_a(1+p_abas(p_newc-p_nrow):) &
				,p_x,p_ls,wslu1,lwsll1,.false.)
			!write(6,*)'<67 ',p_x(2)
			if(abs(p_x(p_ls(p_leave))).lt.p_tiny6)then
				! p_npivotw=p_npivotw+1
				! if(p_npivotw.le.7)write(6,*)'pivot cannot be done, pivot=',p_pivot,' p_ienter=',p_ienter
 
				!	if(p_p9)then
				!if(p_pp)write(16,*)'p_ienter,p_leavec',p_ienter,p_leavec,store,j_tmax,p_newa
				write(6,*)'<12>pivot_cannot',p_pivot,p_objf,p_ienter,p_unit,p_tmax
				! p_valueopt,p_valuek,ixkenter,p_leavkf,p_ifopt,p_justkey,p_keyfact(p_unit,max(ixkenter,1))
				! call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,0,p_a(1+p_abas(p_newc-p_nrow)),p_x,p_ls,wslu1,lwsll1,.false.)
				! write(6,*)'<6788',p_x(2)
				! write(p_n16,*)(p_ls(jjj),p_x(p_ls(jjj)),jjj=1,p_nrow)
				! !endif !if(p_p9)  12901
				!	p_goto55=.true.;return
				j_err=.true. ;return
			endif !if(abs(p_x(p_ls(p_leave))).lt.p_tiny6)  12405
		else !if(p_newc.gt.j_nrow)then
			! residual enters
			call fbsub(p_nrow,p_leave,p_leave,p_a,p_lavec,p_newc,p_a(1+p_abas(1):),p_x,p_ls,wslu1,lwsll1,.false.)
			!				write(6,*)'<679',p_x(2)
			if(abs(p_x(p_ls(p_leave))).lt.p_tiny6)then
				! ! If equation Basis matrix*x= entering column is solved i.e. the entering column is
				! expressed as a linear combination of basic columns and the coefficient of the leaving column is
				! close to zero this means that the entering column is linearly dependent on the all non-leaving
				! columns and the new basis would be singular
				!	p_nresw=p_nresw+1
				write(6,*)'residual ',p_newc,' cannot enter, pivot=',p_pivot !&
				j_err=.true.;return
 
				! !if(p_p9)then
				! write(6,*)'<44>residual ',p_newc,' cannot enter, pivot=',p_pivot,' x=',p_x(p_ls(p_leave)),r(p_ls(p_leave))
				! write(6,*)'p_ienter,p_leavec',p_ienter,p_leavec,p_ls(p_leave)-p_nrow,p_tmax
				! write(6,*)'vcmax ',p_vcmax,p_lower(p_newc)
				! !		call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,p_newc,p_a(1:,1),p_x,p_ls,wslu1,lwsll1,.false.)
				! call fbsub(p_nrow,1,p_nrow,p_a,p_lavec,p_newc,p_a(p_abas(1)+1),p_x,p_ls,wslu1,lwsll1,.false.)
 
				! !	write(p_n16,*)(p_ls(jjj),p_x(p_ls(jjj)),jjj=1,p_nrow)
				! !	endif !if(p_p9)  12924
				! p_ilres=p_newc
				! p_objilres=p_objf
 
				! j_err=.true.;return
			endif !if(abs(p_x(p_ls(p_leave))).lt.p_tiny6)  12424
 
		endif !if(p_newc.gt.p_nrow)  12397
	endif !if(p_route67)  12396
 
endsubroutine route67
 
subroutine printproblem()
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
		end if !if(p_xpresent.and.p_isdomain)  12460
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
 
			end if !if(p_rhs(irow).eq.p_rhs2(irow))  12503
 
		else ! if(j.eq.1) then !if(irow0.ne.irowobj)then
			! for maximization rhs1 =huge  rhs2=0
			! for minimization  rhs2=-huge
			!j_buf(36:)=j_chr10(dble(j_objf))
 
			!if(j_v(p_ivfeasible)>0)then
			if(p_maxo)then
				j_tempchar(1:5)=' max'
			else !if(j_maxo)then
				j_tempchar(1:5)=' min'
			end if !if(p_maxo)  12517
			!else !if(j_v(p_ivfeasible)>0)then
			!	j_buf(1:5)=' '
			!	j_buf(6:33)='Infeasible, temporary object'
			!endif !if(j_v(p_ivfeasible)>0)then
		end if !if(irow.gt.1)  12490
		write(nureport,'(a)')j_tempchar(1:le+28)
		!	if(p_intapp)then
		!		j_buf=' '
		!		j_buf(36:)=j_chr10(j_solx(irow))
		!	write(6,'(a)')j_buf
		!	endif !if(p_intapp)then
		!	end do !k=1,p_nsetr(i)   9299
		!			enddo domloopp !domloopp: do j=1,j_nsetd(i)
	enddo !irow=1,p_nrow  12457
 
end subroutine !subroutine printproblem()
 
subroutine printcur()
	!write(17,*)'p_kier,iter',p_kier,iter
 
end subroutine !subroutine printcur()
 
logical function isxkzero(ixk)
	integer ikey_,ikeepxk_,k_
	isxkzero=.true.
	ikeepxk_ = p_ixkkeep(ixk)
	ikey_=p_ibaunit(p_unit) + p_keys(p_unit)
	ibxdatkey_=ibaxdat(ikey_) !,2)
	if(j_o(p_ivdatamatx)%d(ibxdatkey_+ikeepxk_).ne.0.)then
		isxkzero=.false.
		return
	endif !if(j_o(p_ivdatamatx)%d(ibxdatkey_+ikeepxk_).ne.0.)  12550
	do k_=1,p_lx0
		if(p_lunit(p_lx(k_)).ne.p_unit) cycle
		ikey_=p_ibaunit(p_unit) + p_isch(p_lx(k_))
		ibxdatkey_=ibaxdat(ikey_) !,2)
		if(j_o(p_ivdatamatx)%d(ibxdatkey_+ikeepxk_).ne.0.)then
			isxkzero=.false.
			return
		endif !if(j_o(p_ivdatamatx)%d(ibxdatkey_+ikeepxk_).ne.0.)  12558
	enddo !k_=1,p_lx0  12554
end function !logical function isxkzero(ixk)
 
!funktio palauttaa vaihtoehdon arvon tehdasosuuden
! Sama laskenta oli aiemmin sellaisenaan vaihtoehtosilmukassa, nyt kaksi vaihtoehtosilmukkaa
! (normaali / slow improvment jälkitila), joista funktiota kutsutaan.
! iobs: vaihtoehdon indeksi (kaikkien yksiköiden vaihtoehtojen joukossa)
 
function value_f(iobs)
 
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
					j_o(p_ivdatamatx)%d(ibxdatobs+irowfkeep_)
				if(p_p)write(p_n16,*)'value_f, coeff, xkf ',&
					value_f, p_coeffx(p_rowxkfkey(jjj)%irowfx),j_o(p_ivdatamatx)%d(ibxdatobs+irowfkeep_)
			endif !if(p_feasible)  12581
		else !if (j_rowxkfkey(j_)%jcurix == 0) then
			value_f = value_f - p_vc(p_rowxkfkey(jjj)%jcurix)*p_coeffx(p_rowxkfkey(jjj)%irowfx)*&
				j_o(p_ivdatamatx)%d(ibxdatobs+irowfkeep_)
			!     if(p_p)write(p_n16,*)'value_f,vc,coeff,xkf',&
			!        value_f,j_vc(j_rowxkfkey(j_)%jcurix),j_coeffx(j_rowxkfkey(j_)%irowfx),&
			!          j_o(p_ivdatamatx)%d(ibaxdat(iobs)+irowfkeep_)
		endif !if (p_rowxkfkey(jjj)%jcurix == 0)  12580
	enddo !jjj = 1,p_nrowxkfkey  12578
 
	do jj=1,p_nrowykfkey
		iv2elpos_ = p_rowykfkey(jj)%iv2elpos
		if (p_rowykfkey(jj)%jcurix.eq.0) then ! 0-rivi, ei v-kerrointa
			if(p_feasible) then
				value_f = value_f + j_v(p_rowykfkey(jj)%ivfout)*&
					j_o(p_ivdatamatx)%d(ibxdatobs+iv2elpos_)
				if(p_p)write(p_n16,*)'value_f, gamma, ykf',value_f, j_v(p_rowykfkey(jj)%ivfout),&
					j_o(p_ivdatamatx)%d(ibxdatobs+iv2elpos_)
			endif !if(p_feasible)  12599
		else !if (j_rowykfkey(j_)%jcurix.eq.0) then
			value_f = value_f - p_vc(p_rowykfkey(jj)%jcurix)* j_v(p_rowykfkey(jj)%ivfout)*&
				j_o(p_ivdatamatx)%d(ibxdatobs+iv2elpos_)
			if(p_p)write(p_n16,*)'value_f,vc,gamma,ykf',value_f,p_vc(p_rowykfkey(jj)%jcurix),&
				j_v(p_rowykfkey(jj)%ivfout),j_o(p_ivdatamatx)%d(ibxdatobs+iv2elpos_)
		endif !if (p_rowykfkey(jj)%jcurix.eq.0)  12598
	enddo !jj=1,p_nrowykfkey  12596
 
end function value_f !function value_f(iobs)
 
 
subroutine lcursam()
 
	p_lcursame=0
	p_lcur=p_next(0)
	p_lcur0=p_lcur
	do i=1,p_lx0
		nex=p_next(p_lcur)
		if(p_lunit(p_lcur).eq.p_unit)then
			p_lcursame=p_lcur
			return
		end if !if(p_lunit(p_lcur).eq.p_unit)  12623
	end do !i=1,p_lx0  12621
	return
end subroutine !subroutine p_lcursam()
 
subroutine testld()
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
			write(p_n16,*)'*s* iprev',p_iprev(0),p_iprev(p_iprev(0)),&
				p_iprev(p_iprev(p_iprev(0))),p_iprev(p_iprev(p_iprev(p_iprev(0))))
			write(6,*)'*s* -return'
			!		close(16)
			j_err=.true.
			return
		end if !if(jj.gt.p_lx0.or.nex.eq.0)  12639
		jj=jj+1
		goto 766
767		continue
	end do !i=1,p_lx0  12633
	! end logical testing
end subroutine !subroutine testld()
 
subroutine testcol()
	write(6,*)'testvcol',p_xpresent,p_nrow,p_lx0
 
	if(.not.p_xpresent)return
	do i=0,p_nrow
		p_testxps(i)=0.
		if(i.gt.0)then
			if(p_ls(i).gt.p_nrowz)then
				! ollaan D-osassa
				do j=1,p_lx0
					if(p_ls(i)-p_nrowz.eq.p_lx(j))goto 788 !col löytyi ld:stä
				end do !j=1,p_lx0  12666
				write(p_n16,*)'*col*',p_ls(i),'not in',(p_lx(j),j=1,p_lx0)
				!				close(16)
				j_err=.true.
				write(6,*)'*col* -return'
				!        return
	788		  continue
			endif !if(p_ls(i).gt.p_nrowz)  12664
		end if !if(i.gt.0)  12663
	end do !i=0,p_nrow  12661
	p_idomba=0
	do i=1,p_nunits
		!	write(6,*)'idom9',i,p_idomba
		if(p_isdomain)call jlpcurix()
		! determines for each row if the unit iunit belonggs to the domain of the row
		! units must be accessed one by one
		! p_nrowcurx
		! p_rowcurx
		! p_idomba
		iobs=p_ibaunit(i)+p_keys(i)
		ibxmatx=ibamatx(iobs) !,1)
		if(p_keys(i).gt.p_ns(i))write(6,*)'***perk',i,p_keys(i),p_ns(i) !laiton key
		j_err=.true.
		do j=0,p_nrow
			if(p_ixcur(j))p_testxps(j)=p_testxps(j)+p_xmat(p_ix(j)+ibxmatx) !v(ix(j)) !lasketaan keysumma
		end do !j=0,p_nrow  12691
	end do !i=1,p_nunits  12679
	!	write(p_n16,*)'   ***xps',(p_xps(j),j=0,p_nrow)
	!	write(p_n16,*)'   ***test',(p_testxps(j),j=0,p_nrow) !pitäs olla sama
 
	do i=1,p_lx0
		!	write(p_n16,*)'  dcol',p_lx(i),'unit=',p_lunit(p_lx(i)),'sch=',&
		!p_isch(p_lx(i)),'weight',p_x(p_lx(i)+p_nrowz)
		!	write(p_n16,*)'  val',p_objr(p_lx(i)+p_nrowz),(p_a(j,p_lx(i)+p_nz),j=1,p_nrow)
 
		iobs=p_ibaunit(p_lunit(p_lx(i)))+p_keys(p_lunit(p_lx(i)))
		ibxmatx=ibamatx(iobs) !,1)
 
		if(p_isdomain)call jlpcurix2(p_lunit(p_lx(i)))
		! determines for each row if the unit iunit belonggs to the domain of the row
		! units can be in random order
		! p_nrowcurx
		! p_rowcurx
		do j=0,p_nrow
 
			if(p_ixcur(j)) p_test(j)=&
				p_xmat(p_ix(j)+ibxmatx ) !v(ix(j))
		end do !j=0,p_nrow  12711
		iobs=p_ibaunit(p_lunit(p_lx(i)))+p_isch(p_lx(i))
		ibxmatx=ibamatx(iobs) !,1)
		do j=0,p_nrow
			if(p_ixcur(j)) p_test(j)=p_xmat(p_ix(j)+ibxmatx) -p_test(j) !v(ix(j))-test(j)
			p_testxps(j)=p_testxps(j)+p_x(p_lx(i)+p_nrowz)*p_test(j)
		end do !j=0,p_nrow  12718
		!	write(p_n16,*)'  dcoltest',(p_test(j),j=0,p_nrow)
		if(p_x(p_lx(i)+p_nrowz).LT.0.OR.p_x(p_lx(i)+p_nrowz).GT.1.)then
			write(6,*)'illgal w:',p_x(p_lx(i)+p_nrowz)
			write(6,*)'*dcoltest* return'
			j_err=.true.
		end if !if(p_x(p_lx(i)+p_nrowz).LT.0.OR.p_x(p_lx(i)+p_nrowz).GT.1.  12723
	end do !i=1,p_lx0  12698
	!write(p_n16,*)'*first nonbasic column*:',p_icolold,'*in a matrix',p_icolold.gt.p_nrow
	!write(p_n16,*)'  col',(p_a(j,p_icolold-p_nrowz),j=1,p_nrow)
 
	!write(p_n16,*)'  ***rowtest',(p_testxps(j),j=0,p_nrow)
	!write(p_n16,*)'  next',p_next(0),p_next(p_next(0)),p_next(p_next(p_next(0))),&
	!p_next(p_next(p_next(p_next(0))))
	!	write(p_n16,*)'  iprev',p_iprev(0),p_iprev(p_iprev(0)),&
	!	p_iprev(p_iprev(p_iprev(0))),p_iprev(p_iprev(p_iprev(p_iprev(0))))
	!	write(p_n16,*)'*end test*'
	nex=0
	lunv=-1
	! testataan w:iden summautumista smamassa yksikössä
555	continue
!   3  0   1    5    5  2

!      22       20      22
	nex=p_next(nex)
	if(nex.eq.0)goto 556
	if(p_lunit(nex).ne.lunv)then
		wss=p_x(nex+p_nrowz)
		lunv=p_lunit(nex)
	else !if(j_lunit(nex).ne.lunv)then
		wss=wss+p_x(nex+p_nrowz)
		if(wss.gt.1.)then
			!		write(p_n16,*)'*nex*',nex,wss
			!		close(16)
			write(6,*)'*nex* return'
			j_err=.true. ;return
		end if !if(wss.gt.1.)  12752
	end if !if(p_lunit(nex).ne.lunv)  12747
	goto 555  !next column in the ordering
556 write(6,*)'p_muutosb',p_muutosb
! testaus lopui
end subroutine !subroutine testcol()
 
!
!p_unit_: yksikkö, joka käsittelyssä kutsuhetkellä -> palautetaan curixit oikeaan yksikköön
subroutine testxpssub(iunit_)
 
	p_testxps=j_0
	iunitrans=0
	!p_idomba=0
	do i_=1,p_nunits  !***********************
		if(p_isdomain)call jlpcurix2(i_)
		! determines for each row if the unit iunit belonggs to the domain of the row
		! units can be in random order
		! p_nrowcurx
		! p_rowcurx
		iobs_=p_ibaunit(i_)+p_keys(i_)
		ibxmatx=ibamatx(iobs_) !,1)
		ibxdatobs_=ibaxdat(iobs_) !,1)
		do jj_=1,p_nrowcurx
			jj=p_rowcurx(jj_)
			!xps = sums over the keyschedules vector s in Eq. (6.14) in JLP-manual
			p_testxps(jj)=p_testxps(jj)+p_xmat(p_ix(jj)+ibxmatx) !(ix(j)=0 -> no x in row
			!                if(ixcur(j).ne.0)xps(j)=xps(j)+xmat(ibxdatkey+ix(j))
		enddo !jj_=1,p_nrowcurx  12780
 
		!tehdasmuuttujien xps-laskenta
		if(p_fpresent) then
 
 
		endif !if(p_fpresent)  12788
 
 
	enddo !i_=1,p_nunits  12771
 
	!palautetaan käsiteltävänä olevan yksikön curixit ja tehdasmuunnokset
	if(iunit_>0) then
		iunitrans = iunit_
		if(p_isdomain)call jlpcurix2(iunit_)
		! determines for each row if the unit iunit belonggs to the domain of the row
		! units can be in random order
		! p_nrowcurx
		! p_rowcurx
 
		if(p_fpresent) then
			!	call jlpfcurix(iunit_)
			!	call jlpfcurixy(iunit_)
 
			if (p_nfy.gt.0) then
				do jjj=1,j_o(p_ivkeepc)%i(1)
					j_v(j_o(p_ivkeepc)%i2(jjj))=j_o(p_ivdatamatc)%d((iunit_-1)*j_o(p_ivkeepc)%i(1)+jjj)
				enddo !jjj=1,j_o(p_ivkeepc)%i(1)  12810
				!	do jjj=1,p_nutiltrans
				call dotrans(p_ivutiltrans,1)
				if(j_err)then
					write(6,*)'error for trans ',j
					stop 741
				endif !if(j_err)  12815
				!	enddo !jjj=1,p_nutiltrans  11758
			endif !if (p_nfy.gt.0)  12809
		endif !if(p_fpresent)  12805
	endif !if(iunit_>0)  12797
 
	!write(p_n16,*)'**fact** testxps',(j_testxps(j),j=0,j_nrow)
	nero=0
	do iii=1,p_nrow
		if(abs(p_testxps(iii)-p_xps(iii)).gt.0.001)then
			nero=nero+1
			if(nero.lt.10)write(p_n16,*)'row,test,xps', iii,p_testxps(iii),p_xps(iii)
		endif !if(abs(p_testxps(iii)-p_xps(iii)).gt.0.001)  12827
	enddo !iii=1,p_nrow  12826
	write(p_n16,*)'testxps', nero
 
end subroutine !subroutine testxpssub(iunit_)
 
subroutine testobjr0()
	integer pvars_
	write(p_n16,*)'**fact** testobjr0/w'
	iunitrans = 0
 
	do ldj_=1,p_lx0
 
		p_test = j_0
 
		iunit_=p_lunit(p_lx(ldj_))
		ikey_=p_ibaunit(p_lunit(p_lx(ldj_)))+ p_keys(p_lunit(p_lx(ldj_)))
		iobs_=p_ibaunit(p_lunit(p_lx(ldj_)))+ p_isch(p_lx(ldj_))
		ibxmatx=ibamatx(iobs_) !,1)
		ibxmatkey_=ibamatx(ikey_) !,2)
		ibxdatobs_=ibaxdat(iobs_) !,1)
		ibxdatkey_=ibaxdat(ikey_) !,2)
		write(p_n16,*)'ld(j),iunit,iobs,ikey',p_lx(ldj_),iunit_,iobs_,ikey_
 
		do it_=0,p_nrow
			if(p_ixcur(it_)) then
				p_test(it_) = p_xmat(p_ix(it_)+ibxmatx)- p_xmat(p_ix(it_)+ibxmatkey_)
			endif !if(p_ixcur(it_))  12855
		enddo !it_=0,p_nrow  12854
 
		if (p_fpresent) then
 
 
		endif !if (p_fpresent)  12860
 
		write(p_n16,*)'ld(j)',p_lx(ldj_),'test(0:nrow)',(p_test(jjj),jjj=0,p_nrow)
 
	enddo !ldj_=1,p_lx0  12841
end subroutine !subroutine testobjr0()
 
 
 
! end subroutine !subroutine testobjr0f()
 
 
subroutine getsumxkey()
	logical indomain2
	p_sumx=j_0
	p_idomba=0
	!if(p_isunit)p_ivsubtrans=p_ivtrans
 
	do iuni=1,p_nunits
		if(.not.p_isunit)then
			call j_getobsiv(iuni,p_ivdatamatc,p_ivkeepc,p_ivunit) !,p_ivtransc,j_ivunit)
 
			if(j_err)then
				write(6,*)'error (66) for unit ',iuni
				return
			endif !if(j_err)  12885
			if(p_nshow.gt.0)then
				call dotrans(p_ivshowtrans,1)
				if(j_err)return
			endif !if(p_nshow.gt.0)  12889
			if(p_ivtrans.gt.0)then
				call dotrans(p_ivtrans,1)
				if(j_err)stop 770
			endif !if(p_ivtrans.gt.0)  12893
		endif !if(.not.p_isunit)  12882
		iobs=p_ibaunit(iuni)+p_keys(iuni)
		call j_getobsiv(iobs,p_ivdatamatx,p_ivkeepx,0)! ,p_ivtransx,0)  !getsolx
		if(j_err)then
			write(6,*)'error for observation ',iobs
			return
		endif !if(j_err)  12900
		!if(p_ivsubtrans.gt.0)call dotrans(p_ivsubtrans,1)
		! if(p_ivarea.gt.0)then
		! if(ipart.eq.0)then
		! j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))=j_v(p_ivarea)*j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))
		! !if(p_ncvar.gt.0)j_v(p_cvarl(1:p_ncvar))=j_v(p_ivarea)*j_v(p_cvarl(1:p_ncvar))
		! else !if(ipart.eq.0)then
		! j_v(p_varxareatot)=j_v(p_ivarea)*j_v(p_varxareatot)
		! endif !if(ipart.eq.0)  15088
		! endif !if(p_ivarea.gt.0)  15087
 
		if(p_isdomain)then
			iba=0
			!	p_idomba=(iuni-1)*p_ndomv
 
			do id=1,p_ndom
				!			icurint=(id-1)/32+1;icurbit=p_id-(icurint-1)*32-1
				!					if(.not.btest(p_domainbits(p_idomba+icurint),icurbit))then
				if(indomain2(id,iuni))then
					!         ndomsol(p_id)=ndomsol(p_id)+1
 
					do jx=1,p_nxvartot
						p_sumx(iba+jx)=p_sumx(iba+jx)+ j_v(j_o(p_ivkeepx)%i2(jx))
					end do !jx=1,p_nxvartot  12924
 
					!	if(p_ncvar.gt.0)p_sumx(iba+p_nxvartot+1:iba+p_nxvartot+p_ncvar)= &
					!		p_sumx(iba+p_nxvartot+1:iba+p_nxvartot+p_ncvar)+j_v(p_cvarl(1:p_ncvar))
 
					! if(p_ivsubtrans>0) p_sumx(iba+p_nxvartot+p_ncvar+1:iba+p_nxvartot+p_ncvar+p_noutsubtrans) = &
					! p_sumx(iba+p_nxvartot+p_ncvar+1:iba+p_nxvartot+p_ncvar+p_noutsubtrans) + &
					! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
					! if(p_ivsubtrans>0) p_sumx(iba+p_nxvartot+1:iba+p_nxvartot+p_noutsubtrans) = &
					! p_sumx(iba+p_nxvartot+1:iba+p_nxvartot+p_noutsubtrans) + &
					! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
 
 
				end if !if(indomain2(id,iuni))  12921
				iba=iba+p_nxvartot !+p_noutsubtrans !+p_ncvar
			enddo !id=1,p_ndom  12918
 
 
 
		else !if(j_ndom.gt.0)then
 
			p_sumx(1:p_nxvartot)= p_sumx(1:p_nxvartot)+j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))
			!	if(p_ncvar.gt.0)p_sumx(p_nxvartot+1:p_nxvartot+p_ncvar)= &
			!		p_sumx(p_nxvartot+1:p_nxvartot+p_ncvar)+j_v(p_cvarl(1:p_ncvar))
 
			! if(p_ivsubtrans>0) p_sumx(p_nxvartot+p_ncvar+1:p_nxvartot+p_ncvar+p_noutsubtrans) = &
			! p_sumx(p_nxvartot+p_ncvar+1:p_nxvartot+p_ncvar+p_noutsubtrans) + &
			! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
			! if(p_ivsubtrans>0) p_sumx(p_nxvartot+1:p_nxvartot+p_noutsubtrans) = &
			! p_sumx(p_nxvartot+1:p_nxvartot+p_noutsubtrans) + &
			! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
 
		end if !if(p_isdomain)  12914
 
		if(p_nshow.gt.0)then
			if(.not.p_isdomain)iba=p_nxvartot
			do id=1,p_nshow
				if(j_o(p_ivshowmatrix)%d(id).ne.j_0)then
					j_o(p_ivshowunits)%i2(id)=j_o(p_ivshowunits)%i2(id)+1
					do jx=1,p_nxvartot
						p_sumx(iba+jx)=p_sumx(iba+jx)+ j_v(j_o(p_ivkeepx)%i2(jx))
					end do !jx=1,p_nxvartot  12965
				endif !if(j_o(p_ivshowmatrix)%d(id).ne.j_0)  12963
				iba=iba+p_nxvartot
			enddo !id=1,p_nshow  12962
		endif !if(p_nshow.gt.0)  12960
 
	end do !iuni=1,p_nunits  12881
 
end subroutine getsumxkey
 
 
 
subroutine getsolx() !
	logical indomain,indomain2
	!computes the sums of all x-variables in all domains
 
 
	if(p_nz.eq.0)then
		!!compute integer approximation for rows
		p_solx=j_0
		p_idomba=0
 
		do i=1,p_nunits  !***********************
			!write(6,*)'idom9'
			if(p_isdomain)call jlpcurix()
			! determines for each row if the unit iunit belonggs to the domain of the row
			! units must be accessed one by one
			! p_nrowcurx
			! p_rowcurx
			! p_idomba
			!returns nrowp=number of rows in this domain,
			! rowcurx= for each row in this domain tells the original (expanded) row
			!  ixcur for each original expanded row tells the index of temporary x-variable
			ibxmatx=ibamatx(p_ibaunit(i)+integerschedw(i))  !,1)
			do jj=1,p_nrowcurx
				j=p_rowcurx(jj)
				p_solx(j)=p_solx(j)+p_xmat(p_ix(j)+ibxmatx)
			enddo !jj=1,p_nrowcurx  13001
		enddo !i=1,p_nunits  12989
	endif !if(p_nz.eq.0)  12984
 
 
	! if(p_ivsubtrans>0) then
	! p_ivoutsubtrans = j_trans_output(p_ivsubtrans)
	! p_noutsubtrans = j_o(p_ivoutsubtrans)%i2(1)
	! !write(6,*)'<55>',p_ivoutsubtrans,p_noutsubtrans
	! else !if(p_ivsubtrans>0) then
	!	p_noutsubtrans = 0
	!endif !if(p_ivsubtrans>0)  14165
 
	p_nxvarareatot=0
 
	ipart=0
	ndom2=max(p_ndom,1)
 
	!if(allocated(p_shpx))deallocate(p_shpx)
 
	if(allocated(p_sumxi))deallocate(p_sumxi)
 
	allocate(p_shpx(1:p_nsumx*ndom2))
	!	p_sumx=j_0
	p_shpx=j_0
	if(p_intapp)allocate(p_sumxi(1:p_nsumx*ndom2))
	if(allocated(p_varxareatot))deallocate(p_varxareatot)
	if(p_ivarea.gt.0.and.p_nnotareavars.gt.0)then
		p_nxvarareatot=j_ndiffer(j_o(p_ivkeepx)%i2,p_nxvartot,p_notareavars,p_nnotareavars)
 
		!	notc=j_ndiffer(p_cvarl,p_ncvar,p_notareavars,p_nnotareavars) !j_o(iob)%i(linknotareavars+1:linknotareavars+p_notareavars),p_notareavars)
		if(p_nxvarareatot.lt.p_nxvartot)then !  .or.notc.gt.0)then
			allocate(p_varxareatot(1:p_nxvarareatot)) !+notc))
			!	call j_uniondif(j_o(p_ivkeepx)%i2,p_nxvartot,p_cvarl,p_ncvar,&
			!	p_notareavars,p_nnotareavars,&
			!		j_o(iob)%i(linknotareavars+1:linknotareavars+p_notareavars),p_notareavars,&
			!p_varxareatot,p_nxvarareatot)
			ipart=1
		endif !if(p_nxvarareatot.lt.p_nxvartot)  13035
	elseif(p_ivarea.gt.0)then !if(p_ivarea.gt.0.and.p_notareavars.gt.0)then
		p_nxvarareatot = p_nxvartot !+ p_noutsubtrans
		allocate(p_varxareatot(1:p_nxvarareatot))
		p_varxareatot(1:p_nxvartot)= j_o(p_ivkeepx)%i2(1:p_nxvartot)
		!	if(p_noutsubtrans>0) p_varxareatot(p_nxvartot+1:p_nxvarareatot)=j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans)
	endif !if(p_ivarea.gt.0.and.p_nnotareavars.gt.0)  13031
 
	call getsumxkey()
 
 
	if(p_intapp)p_sumxi=j_0
	p_idomba=0
	do iuni=1,p_nunits
		if(.not.p_isunit)then
			call j_getobsiv(iuni,p_ivdatamatc,p_ivkeepc,p_ivunit)! ,p_ivtransc,j_ivunit)
			if(j_err)then
				write(6,*)'error for unit ',iuni
				stop 512
			endif !if(j_err)  13058
			if(p_ivtrans.gt.0)then
				call dotrans(p_ivtrans,1)
				if(j_err)stop 651
 
			endif !if(p_ivtrans.gt.0)  13062
		endif !if(.not.p_isunit)  13056
		iobs=p_ibaunit(iuni)+integerschedw(iuni)   !+j_ibaunitbas
		call j_getobsiv(iobs,p_ivdatamatx,p_ivkeepx,0)! ,p_ivtransx,0)  !getsolx
		if(j_err)then
			write(6,*)'error for observation ',iobs
		endif !if(j_err)  13070
		! if(p_ivsubtrans.gt.0)then
		! call dotrans(p_ivsubtrans,1)
		! if(j_err)stop 719
		! endif !if(p_ivsubtrans.gt.0)  14296
		if(p_ivarea.gt.0)then
			if(ipart.eq.0)then
 
				j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))=j_v(p_ivarea)*j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))
				!if(p_ncvar.gt.0)j_v(p_cvarl(1:p_ncvar))=j_v(p_ivarea)*j_v(p_cvarl(1:p_ncvar))
			else !if(ipart.eq.0)then
				j_v(p_varxareatot)=j_v(p_ivarea)*j_v(p_varxareatot)
			endif !if(ipart.eq.0)  13078
		endif !if(p_ivarea.gt.0)  13077
		if(p_intapp)then
			if(p_isdomain)then
				iba=0
				!	p_idomba=(iuni-1)*p_ndomv
				do id=1,p_ndom
					!		icurint=(id-1)/32+1;icurbit=p_id-(icurint-1)*32-1
					!	if(.not.btest(p_domainbits(p_idomba+icurint),icurbit))then
					!         ndomsol(p_id)=ndomsol(p_id)+1
					if(indomain2(id,iuni))then
 
						do jx=1,p_nxvartot
							p_sumxi(iba+jx)=p_sumxi(iba+jx)+ j_v(j_o(p_ivkeepx)%i2(jx))
						end do !jx=1,p_nxvartot  13096
 
						!if(p_ncvar.gt.0)p_sumxi(iba+p_nxvartot+1:iba+p_nxvartot+p_ncvar)= &
						!	p_sumxi(iba+p_nxvartot+1:iba+p_nxvartot+p_ncvar)+j_v(p_cvarl(1:p_ncvar))
 
						! if(p_ivsubtrans>0) p_sumxi(iba+p_nxvartot+p_ncvar+1:iba+p_nxvartot+p_ncvar+p_noutsubtrans) = &
						! p_sumxi(iba+p_nxvartot+p_ncvar+1:iba+p_nxvartot+p_ncvar+p_noutsubtrans) + &
						! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
 
						! if(p_ivsubtrans>0) p_sumxi(iba+p_nxvartot+1:iba+p_nxvartot+p_noutsubtrans) = &
						! p_sumxi(iba+p_nxvartot+1:iba+p_nxvartot+p_noutsubtrans) + &
						! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
 
					end if !if(indomain2(id,iuni))  13094
 
					iba=iba+p_nxvartot !+p_noutsubtrans !+p_ncvar+
				end do !id=1,p_ndom  13090
			else !if(j_ndom.gt.0)then
 
				p_sumxi(1:p_nxvartot)= p_sumxi(1:p_nxvartot)+j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))
 
				!	if(p_ncvar.gt.0)p_sumxi(p_nxvartot+1:p_nxvartot+p_ncvar)= &
				!	p_sumxi(p_nxvartot+1:p_nxvartot+p_ncvar)+j_v(p_cvarl(1:p_ncvar))
 
				! if(p_ivsubtrans>0) p_sumxi(p_nxvartot+p_ncvar+1:p_nxvartot+p_ncvar+p_noutsubtrans) = &
				! p_sumxi(p_nxvartot+p_ncvar+1:p_nxvartot+p_ncvar+p_noutsubtrans) + &
				! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
 
				! if(p_ivsubtrans>0) p_sumxi(p_nxvartot+1:p_nxvartot+p_noutsubtrans) = &
				! p_sumxi(p_nxvartot+1:p_nxvartot+p_noutsubtrans) + &
				! j_v(j_o(p_ivoutsubtrans)%i2(1:p_noutsubtrans))
 
			end if !if(p_isdomain)  13087
		endif !if(p_intapp)  13086
	end do !iuni=1,p_nunits  13055
 
	do i=1,p_lx0
 
		wei=p_x(p_lx(i)+p_nrowz)
		iuni=p_lunit(p_lx(i))
		if(.not.p_isunit)then
			call j_getobsiv(iuni,p_ivdatamatc,p_ivkeepc,p_ivunit) !,p_ivtransc,j_ivunit)
			if(p_ivtrans.gt.0)call dotrans(p_ivtrans,1)
		endif !if(.not.p_isunit)  13138
		if(p_nshow.gt.0)then
			call dotrans(p_ivshowtrans,1)
			if(j_err)return
		endif !if(p_nshow.gt.0)  13142
		iobs=p_ibaunit(iuni)+p_keys(iuni)
		call j_getobsiv(iobs,p_ivdatamatx,p_ivkeepx,0) !,p_ivtransx,0) !getsolx
		!	if(p_ivsubtrans.gt.0)call dotrans(p_ivsubtrans,1)
		if(j_err)return
 
		if(p_ivarea.gt.0)then
			if(ipart.eq.0)then
				j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))=j_v(p_ivarea)*j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))
			else !if(ipart.eq.0)then
				j_v(p_varxareatot)=j_v(p_ivarea)*j_v(p_varxareatot)
			endif !if(ipart.eq.0)  13152
		endif !if(p_ivarea.gt.0)  13151
		if(p_isdomain)then
			iba=0
			!	p_idomba=(iuni-1)*p_ndomv
			do id=1,p_ndom
				!	icurint=(id-1)/32+1;icurbit=p_id-(icurint-1)*32-1
				!	if(.not.btest(p_domainbits(p_idomba+icurint),icurbit))then
				if(indomain2(id,iuni))then
					do jx=1,p_nxvartot
						p_sumx(iba+jx)=p_sumx(iba+jx)-wei* j_v(j_o(p_ivkeepx)%i2(jx))
					end do !jx=1,p_nxvartot  13165
 
					! do jx=1,p_noutsubtrans
					! p_sumx(iba+p_nxvartot+p_ncvar+jx)=p_sumx(iba+p_nxvartot+p_ncvar+jx) &
					! p_sumx(iba+p_nxvartot+jx)=p_sumx(iba+p_nxvartot+jx) &
					! - wei*j_v(j_o(p_ivoutsubtrans)%i2(jx))
					! enddo !jx=1,p_noutsubtrans  14388
				end if !if(indomain2(id,iuni))  13164
 
				iba=iba+p_nxvartot  !+p_noutsubtrans
			end do !id=1,p_ndom  13161
		else !if(j_ndom.gt.0)then
			do jx=1,p_nxvartot
				p_sumx(jx)=p_sumx(jx)-wei* j_v(j_o(p_ivkeepx)%i2(jx))
			end do !jx=1,p_nxvartot  13179
 
			! do jx=1,p_noutsubtrans
			! !	p_sumx(p_nxvartot+p_ncvar+jx)=p_sumx(p_nxvartot+jx) & !p_ncvar+jx) &
			! p_sumx(p_nxvartot+jx)=p_sumx(p_nxvartot+jx) & !p_ncvar+jx) &
			! - wei*j_v(j_o(p_ivoutsubtrans)%i2(jx))
			! enddo !jx=1,p_noutsubtrans  14402
		end if !if(p_isdomain)  13158
 
		if(p_nshow.gt.0)then
			if(.not.p_isdomain)iba=p_nxvartot
			do id=1,p_nshow
				if(j_o(p_ivshowmatrix)%d(id).ne.j_0)then
					!		j_o(p_ivshowunits)%i2(id)=j_o(p_ivshowunits)%i2(id)+1
					do jx=1,p_nxvartot
						p_sumx(iba+jx)=p_sumx(iba+jx)-wei* j_v(j_o(p_ivkeepx)%i2(jx))
						!	p_sumx(iba+jx)=p_sumx(iba+jx)+ j_v(j_o(p_ivkeepx)%i2(jx))
					end do !jx=1,p_nxvartot  13195
				endif !if(j_o(p_ivshowmatrix)%d(id).ne.j_0)  13193
				iba=iba+p_nxvartot
			enddo !id=1,p_nshow  13192
 
		endif !if(p_nshow.gt.0)  13190
 
 
		iobs=p_ibaunit(iuni)+p_isch(p_lx(i))
		call j_getobsiv(iobs,p_ivdatamatx,p_ivkeepx,0) !,p_ivtransx,0)  !getsolx
		!	if(p_ivsubtrans.gt.0)call dotrans(p_ivsubtrans,1)
 
		if(p_ivarea.gt.0)then
			if(ipart.eq.0)then  !if(p_nxvarareatot.eq.nxvartot)then
				j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))=j_v(p_ivarea)*j_v(j_o(p_ivkeepx)%i2(1:p_nxvartot))
			else !if(ipart.eq.0)then
				j_v(p_varxareatot)=j_v(p_ivarea)*j_v(p_varxareatot)
			endif !if(ipart.eq.0)  13211
		endif !if(p_ivarea.gt.0)  13210
 
		if(p_isdomain)then
			iba=0
			!		p_idomba=(iuni-1)*p_ndomv
			do id=1,p_ndom
				!		icurint=(id-1)/32+1;icurbit=p_id-(icurint-1)*32-1
				!		if(.not.btest(p_domainbits(p_idomba+icurint),icurbit))then
				if(indomain2(id,iuni))then
					do jx=1,p_nxvartot
						p_sumx(iba+jx)=p_sumx(iba+jx)+wei* j_v(j_o(p_ivkeepx)%i2(jx))
					end do !jx=1,p_nxvartot  13225
 
					! do jx=1,p_noutsubtrans
					! !			p_sumx(iba+p_nxvartot+p_ncvar+jx)=p_sumx(iba+p_nxvartot+p_ncvar+jx) &
					! p_sumx(iba+p_nxvartot+jx)=p_sumx(iba+p_nxvartot+jx) &
					! + wei*j_v(j_o(p_ivoutsubtrans)%i2(jx))
					! enddo !jx=1,p_noutsubtrans  14432
				end if !if(indomain2(id,iuni))  13224
 
				iba=iba+p_nxvartot+p_noutsubtrans
			end do !id=1,p_ndom  13221
		else !if(j_ndom.gt.0)then
			do jx=1,p_nxvartot
				p_sumx(jx)=p_sumx(jx)+wei* j_v(j_o(p_ivkeepx)%i2(jx))
			end do !jx=1,p_nxvartot  13239
 
			! do jx=1,p_noutsubtrans
			! !	p_sumx(p_nxvartot+p_ncvar+jx)=p_sumx(p_nxvartot+p_ncvar+jx) &
			! p_sumx(p_nxvartot+jx)=p_sumx(p_nxvartot+jx) &
			! + wei*j_v(j_o(p_ivoutsubtrans)%i2(jx))
			! enddo !jx=1,p_noutsubtrans  14446
 
		end if !if(p_isdomain)  13218
		if(p_nshow.gt.0)then
			if(.not.p_isdomain)iba=p_nxvartot
			do id=1,p_nshow
				if(j_o(p_ivshowmatrix)%d(id).ne.j_0)then
					!	j_o(p_ivshowunits)%i2(id)=j_o(p_ivshowunits)%i2(id)+1
					do jx=1,p_nxvartot
						p_sumx(iba+jx)=p_sumx(iba+jx)+wei* j_v(j_o(p_ivkeepx)%i2(jx))
						!			p_sumx(iba+jx)=p_sumx(iba+jx)+ j_v(j_o(p_ivkeepx)%i2(jx))
					end do !jx=1,p_nxvartot  13255
				endif !if(j_o(p_ivshowmatrix)%d(id).ne.j_0)  13253
				iba=iba+p_nxvartot
			enddo !id=1,p_nshow  13252
		endif !if(p_nshow.gt.0)  13250
 
 
 
	end do !i=1,p_lx0  13134
 
	return
end subroutine !subroutine getsolx()
 
 
 
 
subroutine defsolu()
	write(6,*)'defsolu'
end subroutine
! integer, dimension(:),allocatable::iperm,isdiv0
! double precision, dimension(:),allocatable::sortkey
 
! p_id=0
! nunv=-1
! nn=0
! do i=1,p_lx0
! p_id=p_next(p_id)
! if(p_lunit(p_id).ne.nunv)nn=nn+1
! nunv=p_lunit(p_id)
! end do !i=1,p_lx0  14789
! !write(6,*)'number of basic residuals (=nonbinding constraints)',p_lr0
! write(6,*)'split units ',nn
! !write(6,*)'number of explicit basic schedules ',j_ld0
! p_ndiv=p_lx0+nn
! if(allocated(p_unitdiv))deallocate(p_unitdiv);allocate( p_unitdiv(1:p_ndiv))
! if(allocated(p_isdiv))deallocate(p_isdiv);allocate( p_isdiv(1:p_ndiv))
! if(allocated(p_wdiv))deallocate(p_wdiv);allocate( p_wdiv(1:p_ndiv))
! allocate(iperm(1:p_ndiv),isdiv0(1:p_ndiv),sortkey(1:p_ndiv))
 
! if(p_lx0.eq.0)return
! p_id=0
! sum=0.
! idiv=0
 
! ! keys 2 3 5 6
! ! div  2,1 4,7
! nunv=p_lunit(p_next(0))
! iplace=0
! do i=1,p_lx0
! p_id=p_next(p_id)
! !	write(6,'(a,i6,a,i4,a,i4,a,f7.5)')'unit=',j_lunit(p_id),' key=',j_keys(j_lunit(p_id)),&
! !		' basic sched=',j_isch(p_id),' weight=',j_x(j_nrowz+id)
! if(p_lunit(p_id).ne.nunv)then
! if(iplace.eq.0)then   ! key is largest
! idiv=idiv+1;iplace=idiv
! end if !if(iplace.eq.0)  14817
 
! p_wdiv(iplace)=1.-sum     ! area
 
! isdiv0(iplace)=p_keys(nunv)
! p_unitdiv(iplace)=nunv
 
! sum=0.
! iplace=0
! end if !if(p_lunit(p_id).ne.nunv)  14816
! !
! if(p_isch(p_id).gt.p_keys(p_lunit(p_id)).and.iplace.eq.0)then   !reserve place for
! idiv=idiv+1
! iplace=idiv
! end if !if(p_isch(p_id).gt.p_keys(p_lunit(p_id)).and.iplace.eq.0)  14830
! idiv=idiv+1
 
! p_unitdiv(idiv)=p_lunit(p_id)
! isdiv0(idiv)=p_isch(p_id)
! p_wdiv(idiv)=p_x(p_nrowz+p_id)
! sum=sum+p_x(p_nrowz+p_id)
! nunv=p_lunit(p_id)
! end do !i=1,p_lx0  14812
 
! if(iplace.eq.0)then   ! key is largest
! idiv=idiv+1;iplace=idiv
! end if !if(iplace.eq.0)  14843
! p_wdiv(iplace)=1.-sum
! p_unitdiv(iplace)=nunv
! isdiv0(iplace)=p_keys(nunv)
! smax=maxval(isdiv0)
! do i=1,p_ndiv;iperm(i)=i;sortkey(i)=p_unitdiv(i)+isdiv0(i)/smax;enddo
 
! !jr  from Numerical Recipes
! call j_quick_sort(sortkey(1:p_ndiv),iperm)
! sortkey=p_wdiv
! do i=1,p_ndiv
! p_wdiv(i)=sortkey(iperm(i))
! enddo !i=1,p_ndiv  14855
 
! do i=1,p_ndiv
! p_isdiv(i)=isdiv0(iperm(i))
! end do !i=1,p_ndiv  14859
! isdiv0=p_unitdiv
! do i=1,p_ndiv
! p_unitdiv(i)=isdiv0(iperm(i))
! end do !i=1,p_ndiv  14863
 
! deallocate(iperm,isdiv0,sortkey)
 
! if(p_fpresent)write(6,*)'number of split transportations ',p_lf0-p_mxd
 
! return
 
! end  subroutine !subroutine defsolu()
 
! ! tehdasratkaisujen talteenotto
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
 
	ibamatx=(iobs-1)*p_ntemp0
end function
 
function ibaxdat(iobs)
	ibaxdat=(iobs-1)*p_nkeepx
end function
 
logical function indomain(ido)
	if(p_domvars(ido).eq.j_ivall)then
		indomain=.true.
 
	elseif(btest(p_domainbits(p_idomba+p_icurint(ido)),p_icurbit(ido)))then
		indomain=.true.
	else
		indomain=.false.
	endif !if(p_domvars(ido).eq.j_ivall)  13395
	p_idomba=p_idomba+p_ndomv
	return
end function
 
logical function indomain2(ido,iuni)
	if(p_domvars(ido).eq.j_ivall)then
		indomain2=.true.
		return
	endif !if(p_domvars(ido).eq.j_ivall)  13408
	idomba=(iuni-1)*p_ndomv
	if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido)))then
		indomain2=.true.
	else
		indomain2=.false.
	endif !if(btest(p_domainbits(idomba+p_icurint(ido)),p_icurbit(ido  13413
	return
end function
 
 
!zerocapacity		   !!!!
subroutine zerocap()
 
	do irowj_ = 2,p_nrow
		!		write(6,*)'row ',irowj_,p_rhs2(irowj_-1),p_nfxinrow(irowj_),p_nfyinrow(irowj_),&
		!	p_ix(irowj_-1)
		if(p_nfxinrow(irowj_).eq.1.and. p_nfyinrow(irowj_).eq.0.and. &
				p_ix(irowj_-1).eq.0)then !.and.j_ix(irowj_).eq.0)then !
			!z-muuttujat tsekattva a-matriisista)
 
			jxk=p_irowfvarx(p_ibafx(irowj_)+1)
			jf = p_irowffact(p_ibafx(irowj_)+1)
			p_zeroc=p_rhs2(irowj_-1).eq.j_0
 
			!		if(rh_.eq.0.)p_zeroc=.true.  !(jxk,jf)=.true.
 
			!20181116 #p_zeroc_z
			do iz=1,p_nz
				if(p_a(irowj_-1+p_abas(iz)).ne.0.)p_zeroc=.false.  !(jxk,jf)=.false.
			enddo !iz=1,p_nz  13439
			if(p_zeroc)then  !J-err=.true.
				write(6,*)'at row' ,irowj_, 'there is zero capacity for timber ',jxk,&
					'and factory ',jf
				write(6,*)'remove the row and put the price or utility negative'
				j_err=.true.
 
			endif !if(p_zeroc)  13442
 
		endif !if(p_nfxinrow(irowj_).eq.1.and. p_nfyinrow(irowj_).eq.0.an  13428
	enddo !irowj_ = 2,p_nrow  13425
	return
end subroutine
 
subroutine jlp00(iob,io)
	! no constraints
	integer,dimension(:),allocatable::ikeepc,ikeepx,itermc,itermx
	integer*8 ibax,nkeepx
	!without
	!	call jlpcoef_(p_ivproblem,j_ivout,ivobjects)
	call initjlp2(iob,io)   ! jlp00(    no constraints
 
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
 
	nc=0
	nx=0
	!if(allocated(ikeepc))deallocate(ikeepc)
	!	write(6,*)'p_nterm',p_nterm
	allocate(ikeepc(1:p_nterm),ikeepx(1:p_nterm),itermc(1:p_nterm),itermx(1:p_nterm))
	do ival=1,p_nterm
		i=p_termvar(ival)
		write(6,*)'i',i,j_otype(i),j_iptable
		if(j_otype(i).eq.j_iptable)cycle
		ikeepc0=j_inlistobject(i,p_ivkeepc)
		if(ikeepc0.gt.0)then
			nc=nc+1
			ikeepc(nc)=ikeepc0
			itermc(nc)=ival
		endif !if(ikeepc0.gt.0)  13488
		ikeepx0=j_inlistobject(i,p_ivkeepx)
		if(ikeepx0.gt.0)then
			if(ikeepc0.gt.0)nc=nc-1
			nx=nx+1
			ikeepx(nx)=ikeepx0
			itermx(nx)=ival
		endif !if(ikeepx0.gt.0)  13494
		if(ikeepx0.le.0.and.ikeepc0.le.0)then
			call j_getname(i)
			write(6,*)j_oname(1:j_loname),' is not in cdata or in xdata'
			j_err=.true.
		endif !if(ikeepx0.le.0.and.ikeepc0.le.0)  13500
	enddo !ival=1,p_nterm  13483
 
	nkeepx=j_o(p_ivkeepx)%i(1)
	nkeepc=j_o(p_ivkeepc)%i(1)
	if(j_err)goto 90
	p_ivkeys=j_deflist(j_ivout,'%keys',list0=p_nunits,ilist=.true.) ! jlp00
	p_keys=>j_o(p_ivkeys)%i2(1:p_nunits)
	ibac=0
	ibax=0
	p_objf=j_0
	!	write(6,*)'nc,nkeepx,nkeepc,p_nunits,',nc,nkeepx,nkeepc,p_nunits,'nx',nx
	!	write(6,*)'coef', p_coef(1:p_nterm)
	if(nc.eq.0)then  !there are
		do i=1,p_nunits
			ns=j_o(p_ivdatamatc)%d(ibac+1)
			!		write(6,*)'iunit,ns',i,ns,p_nterm,p_ivdatamatx,ibax,ikeepx(1:nx),ibax+ikeepx
 
			j_dapu=dot_product(p_coef(1:p_nterm),j_o(p_ivdatamatx)%d(ibax+ikeepx))
			!			write(18,*)i,ns,ibax,j_o(p_ivdatamatx)%d(ibax+ikeepx),j_dapu
			if(.not.p_maxo)j_dapu=-j_dapu
			p_keys(i)=1
			ibax=ibax+nkeepx
			do j=2,ns
				j_dapu2=dot_product(p_coef(1:p_nterm),j_o(p_ivdatamatx)%d(ibax+ikeepx))
				write(18,*)j,ibax,j_o(p_ivdatamatx)%d(ibax+ikeepx),j_dapu2,j_dapu
				if(.not.p_maxo)j_dapu2=-j_dapu2
				if(j_dapu2.gt.j_dapu)then
					p_keys(i)=j
					j_dapu=j_dapu2
				endif !if(j_dapu2.gt.j_dapu)  13531
				ibax=ibax+nkeepx
 
			enddo !j=2,ns  13527
			ibac=ibac+nkeepc
			p_objf=p_objf+j_dapu
			!		write(6,*)'i,daspu,j_objf',i,p_objf,j_dapu,ns
		enddo !i=1,p_nunits  13518
	else
		do i=1,p_nunits
			ns=j_o(p_ivdatamatc)%d(ibac+1)
			j_dapu=j_ninf
 
			do j=1,ns
				j_dapu2=j_0
				do j2=1,nx
					j_dapu2=j_dapu2+p_coef(itermx(j2))*j_o(p_ivdatamatx)%d(ibax+ikeepx(j2))
 
				enddo !j2=1,nx  13549
				if(.not.p_maxo)j_dapu2=-j_dapu2
				if(j_dapu2.gt.j_dapu)then
					p_keys(i)=j
					j_dapu=j_dapu2
				endif !if(j_dapu2.gt.j_dapu)  13554
				ibax=ibax+nkeepx
 
				p_objf=p_objf+j_dapu
			enddo !j=1,ns  13547
			j_dapu2=j_0
			do j=1,nc
				j_dapu2=j_dapu2+p_coef(itermc(j))*j_o(p_ivdatamatc)%d(ibac+ikeepc(j))
 
			enddo !j=1,nc  13563
			if(.not.p_maxo)j_dapu2=-j_dapu2
 
			p_objf=p_objf+j_dapu2
			ibac=ibac+nkeepc
		enddo !i=1,p_nunits  13543
 
	endif !if(nc.eq.0)  13517
	write(6,*)'objf',p_objf
 90 deallocate(ikeepc,ikeepx,itermc,itermx)
end subroutine
 
subroutine initdata0(iob,io)
	write(6,*)'initdata0'
	! p_ivarea=
	! p_isunit
	!p_ivdatac=ivdata
	! p_ivdatax=j_o(ivdata)%i(3)
	! p_ivunit
	!		p_ivkeepx=
	! p_nkeepx=
	! p_ivdatamatc=
	! p_ivkeepc=
	! p_ivunit=
	! p_ivdatamatx=
	! p_lopp=
	! p_datamatx
	! p_ivs
	!	j_ivns
	!j_filterlink
	!p_filter_
	!j_rejectlink=
	!p_reject=
	!p_filre
	!p_isvarx
	!p_istermx
	!p_ivnxinrow=
	!p_ninrowx(0:)
	!p_nrowcurx
	!p_nxvar
	!p_nstot
	!p_ibaunitbas
	!p_nrejtot=
	!p_maxns
	!p_nunits
	!p_ivxvar
	!p_varx
	!p_ivibaunit
	!p_ibaunit
	!p_ivkeys
	!p_keys
	!	p_ivns
	!	p_ns=
 
 
	! init data also when there are no constraints
	p_ivarea=j_igetopt(iob,io,j_marea)
	if(p_ivarea.eq.0)then   !option area->,
		p_ivarea=j_object('area')
		if(p_ivarea.le.0)then
			write(6,*)'**jlp, area-variable does not exist'
			j_err=.true.
			return
		endif !if(p_ivarea.le.0)  13625
	endif !if(p_ivarea.eq.0)  13623
 
 
	call j_getoption_index(iob,io,j_munit,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	p_isunit=.false.
 
 
	if(noptarg.gt.0)then
		p_ivunit=j_optarg0(1)
		p_isunit=.true.
	endif !if(noptarg.gt.0)  13638
 
	!	write(6,*)'here'
	call j_getoption_index(iob,io,j_mdata,1,1,j_ipdata,.true.,noptarg,j_optarg0)
	if(j_err)then
		write(6,*)'jlp() neeeds data->, problems without schedules are solved with jlpz()'
		return
	endif !if(j_err)  13645
 
	p_ivdatax=j_optarg0(1)
	! level=j_o(ivdata)%i(12)
	! p_ivdatac=ivdata  !j_topdata(ivdata)
	! !	j_o(iv)%i(14)=j_o(ivmat)%i(1)  !number of observa
	! !j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	! !	j_o(iv)%i(5)=ivup
 
	! p_ivdatax=j_o(ivdata)%i(3)  !(ivdata)
	! !	levelxdata=j_o(p_ivdatax)%i(12)
	! call j_getname(p_ivdatax)
 
	! write(6,*)'xdata ',j_oname(1:j_loname)  !,' cdata ',j_oname2(1:j_loname2),' xdata ',j_oname3(1:j_loname3)
	! !	write(6,*)'level ',level ,p_ivdatac,p_ivdatax
 
	! call j_getdataobject(iob,io,ivdata=p_ivdatax,pointmat=>p_datamatx,)
 
	!		endif
	write(6,*)
	if(p_isunit)then
		!	p_ivdatax=ivdata
		call j_getdataobject(iob,io,ivdata=p_ivdatax,pointmat=p_datamatx,pointkeep=p_keepx)
		p_nkeepx=j_dnkeep		!	ivxdatmat=
	else
		!	p_ivdatac=j_getupdata(ivdata,needed=.true.)
		if(j_err)return
		!	call j_getdataobject(iob,io,ivdata=p_ivdatax,pointmat=p_datamatx,pointmatup=p_datamatc,&
		!		pointkeep=p_keepx,pointkeepup=p_keepc)
		!	if(j_err)return
 
		p_ivdatamatx=j_getmatrix(p_ivdatax,point=p_datamatx);if(j_err)return
		p_ivkeepx=j_getkeep(p_ivdatax,point=p_keepx,len=p_nkeepx);if(j_err)return
		p_ivdatac=j_getupdata(p_ivdatax,needed=.true.)
		if(j_err)write(6,*)'*** data-> must now refer to xdata for consistency with other functions with hierarchical data'
		if(j_err)return
		p_ivdatamatc=j_getmatrix(p_ivdatac,point=p_datamatc);if(j_err)return
		p_ivkeepc=j_getkeep(p_ivdatac,point=p_keepc,len=p_nkeepc);if(j_err)return
 
		!		write(6,*)'xx',p_datamatx(1:20)
		!		write(6,*)'cc',p_datamatc(1:20)
 
		p_ivunit=j_o(p_ivdatac)%i(6)
		p_ivs=j_o(p_ivdatax)%i(6)     ! index of Obs
 
		!write(6,*)'xmat',p_ivs,p_datamatx(1:20)
		!	stop
		p_lopp=j_nrows(p_ivdatamatx)
		!		p_datamatx=>j_o(p_ivdatamatx)%d(1:p_nkeepx*p_lopp)
		!		call j_getname(p_ivunit)
		!	write(6,*)'UNIT ',j_oname(1:j_loname)
		p_ivnsvar=j_o(p_ivdatac)%i(4)   !variable 'Ns' telling the number of schedules in each unit (nobsw variable)
		nsindex=j_inlistobject(p_ivnsvar,p_ivkeepc)
		write(6,*)'nsindex ',nsindex
		!	write(6,*)'cdatai ',j_o(p_ivdatac)%i(1:10)
 
		if(nsindex.le.0)then
			call j_printname('**nobsw variable ',p_ivnsvar,' not in the cdata')
			j_err=.true.;return
		endif !if(nsindex.le.0)  13704
		!write(6,*)'p_ivdatac ',p_ivdatac
		! ! subfilter->
		! j_filterlink=j_codelink(iob,io,j_msubfilter)
		! p_filter_=j_filterlink.ne.0
 
		! j_rejectlink=j_codelink(iob,io,j_msubreject)
		! p_reject=j_rejectlink.ne.0
		! p_filre=p_filter_.or.p_reject
 
		!is(p_p8)write(6,*)'<4075,p_ivdatac,p_xdata ',p_ivdatac,p_ivdatax, 'keepx ',p_nkeepx
	endif !if(p_isunit)  13668
	if(allocated(p_isvarx))deallocate(p_isvarx);allocate(p_isvarx(1:p_npvar))
	p_isvarx=.false.
 
 
 
	if(allocated(p_istermx))deallocate(p_istermx);allocate(p_istermx(1:p_nterm))
	p_istermx=.false.
 
	!if(allocated(p_ninrowx))deallocate(p_ninrowx);allocate(p_ninrowx(0:p_nrow)) ;p_ninrowx=0
	p_ivnxinrow=j_deflist(j_ivout,'%nxinrow',list0=p_nrowtot,ilist=.true.)
	p_ninrowx(0:)=>j_o(p_ivnxinrow)%i2(1:p_nrowtot)
 
	p_nrowcurx=0  !number of rows having x
	!p rowcurx
	ivkeep=j_o(p_ivdatax)%i(11)    !2)
	call j_getname(ivkeep)
	!	write(6,*)'ivkeep',ivkeep,j_oname(1:j_loname)
	do ival=1,p_nterm
		i=p_termvar(ival)
		ikeep=j_inlistobject(i,ivkeep)
 
		iout=0
		!if(p_ivsubtrans.gt.0)iout=j_inlistobject(i,j_trans_output(p_ivsubtrans))
		if(ikeep.gt.0)then
			p_istermx(ival)=.true.  ! is x variaböe
			!write(6,*)'**',p_termvar(ival),p_ivvars
			jix=j_inlistobject(p_termvar(ival),p_ivvars)
			if(jix.gt.0)p_isvarx(jix)=.true.
			p_nxval=p_nxval+1
			iro=p_rowofterm(ival)
			p_ninrowx(iro)=p_ninrowx(iro)+1
			if(p_ninrowx(iro).eq.1)p_nrowcurx=p_nrowcurx+1
 
		endif !if(ikeep.gt.0)  13742
	enddo !ival=1,p_nterm  13736
	p_nxvar=count(p_isvarx)
 
	p_nstot=0  !total number of schedules
	!ibasclass=0  given earlier
	isc=0  ! have we starter the given class if class option is given
	p_ibaunitbas=0  ! basis for schedules, remains 0 if option class-> not given
	p_nrejtot=0  !number of rejected schdules
 
 
	!!!!!**************setting up xdata
	!write(6,*)'<147 hui',size(j_xmat),allocated(j_xmat)
 
	!write(6,*)'6666pisunit',p_isunit
	if(p_isunit)then
		nntem=j_o(p_ivdatamatx)%i(1)/2
		if(allocated(j_itempvector))then
			if(size(j_itempvector).lt.nntem)deallocate(j_itempvector)
		endif !if(allocated(j_itempvector))  13769
		if(.not.allocated(j_itempvector))allocate(j_itempvector(1:nntem))
		inde=j_inlistobject(p_ivunit,p_ivkeepx)
		!	write(6,*)'inde',inde
		p_nunits=1
		j_dapu=j_o(p_ivdatamatx)%d(inde)
		ibas=p_nkeepx
		iprev=1
		iprev=1
		p_maxns=1
		!	write(6,*)'indet',inde,old
		do i=2,j_o(p_ivdatamatx)%i(1)
			!	if(i.lt.200)write(6,*)i,j_o(p_ivdatamatx)%d(ibas+inde),old,ibas
			if(j_o(p_ivdatamatx)%d(ibas+inde).ne.j_dapu)then
				j_itempvector(p_nunits)=i-iprev
				p_maxns=max(p_maxns,j_itempvector(p_nunits))
				p_nunits=p_nunits+1
				j_dapu=j_o(p_ivdatamatx)%d(ibas+inde)
 
				iprev=i
			endif !if(j_o(p_ivdatamatx)%d(ibas+inde).ne.j_dapu)  13784
			ibas=ibas+p_nkeepx
		enddo !i=2,j_o(p_ivdatamatx)%i(1)  13782
		j_itempvector(p_nunits)=j_o(p_ivdatamatx)%i(1)-iprev
		p_maxns=max(p_maxns,j_itempvector(p_nunits))
		!	write(6,*)'shed',p_ns(1:p_nunits)
	else
		p_nunits=j_o(p_ivdatamatc)%i(1)
	endif !if(p_isunit)  13767
 
	! if(allocated(p_valuedif))deallocate(p_valuedif,p_objdif)
	! allocate(p_valuedif(1:p_nunits),p_objdif(1:p_nun
	if(.not.p_isunit)p_maxns=j_o(p_ivdatac)%i(9)
	!is(p_p8)write(6,*)'<pmaxns ',p_maxns
 
	!	call j_getdat(p_ivdatax,p_lopp,p_ivdatamatx,p_ivkeepx)
	if(j_err)return
	if(p_xpresent)then
		p_ivvarx=j_deflist(j_ivout,'%varx',list0=p_nxvar)
		p_varx=>j_o(p_ivvarx)%i2(1:p_nxvar)
		p_varx=pack(p_vars(1:p_npvar),p_isvarx)
 
	end if !if(p_xpresent)  13808
	p_ivibaunit=j_deflist(j_ivout,'%ibaunit',list0=p_nunits+1,ilist=.true.)
	p_ibaunit=>j_o(p_ivibaunit)%i2(1:p_nunits+1)
	!if(allocated(p_ibaunit))deallocate(p_ibaunit)
	if(.not.p_warm)then
		p_ivkeys=j_deflist(j_ivout,'%keys',list0=p_nunits,ilist=.true.)
		p_keys=>j_o(p_ivkeys)%i2(1:p_nunits)
		!	allocate(p_keys(1:p_nunits))  !;p_keys=0
	endif !if(.not.p_warm)  13817
	p_ivns= j_deflist(j_ivout,'%ns',list0=p_nunits,ilist=.true.,point=p_ns)
	!	p_ns=>j_o(p_ivns)%i2(1:p_nunits)
	!	if(allocated(p_ns))deallocate(p_ns)
	!write(6,*)'<356nunits ',p_nunits
	!	allocate(p_ns(1:p_nunits))
 
	if(p_isunit)then
		p_ns=j_itempvector(1:p_nunits)
		deallocate(j_itempvector)
	endif !if(p_isunit)  13828
	if(p_nxvar.eq.0.and..not.p_fpresent)then
		write(6,*)'there are no x-variables even if data->'
		j_err=.true.;return
 
	endif !if(p_nxvar.eq.0.and..not.p_fpresent)  13832
 
	p_ibaunit(1)=0 ! ibaunit, basis for schedules for each unit
	iba=0
	do i=1,p_nunits  !***********************
		!	write(6,*)'in',i,p_nunits
		if(.not.p_isunit)p_ns(i)=p_datamatc(iba+nsindex)
		p_ibaunit(i+1)=p_ibaunit(i)+p_nkeepx
		iba=iba+p_nkeepc
	enddo !i=1,p_nunits  13840
 
 
 
 
 
 
end subroutine initdata0
 
subroutine fterms(iob,io)
	write(6,*)'fterms'
	!input p_nterm p_termvar
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
 
	call j_getoption(iob,io,j_mknn,1,1,j_ipreal,.true.,narg,j_optarg0)
	if(j_err)return
	p_knn=j_v(j_optarg0(1))
 
	write(6,*)'p_knn',p_knn
 
	p_ivlogpart=j_deflist(j_ivout,'%logpart',list0=p_nterm,ilist=.true.,point=p_logpart)
	p_ivfactpart=j_deflist(j_ivout,'%factpart',list0=p_nterm,ilist=.true.,point=p_factpart)
	p_ivtabpart=j_deflist(j_ivout,'%tabpart',list0=p_nterm,ilist=.true.,point=p_tabpart)
 
 
	p_ntable0=0
	if(allocated(p_isvarf))deallocate(p_isvarf,p_istermf)
	allocate(p_isvarf(1:p_npvar),p_istermf(1:p_nterm))
	p_isvarf=.false. ;p_istermf=.false.
	do i=1,p_nterm
		if(j_otype(p_termvar(i)).eq.j_iptable)p_ntable0=p_ntable0+1
	end do !i=1,p_nterm  13886
	!	p_ivtable0=j_deflist(j_ivout,'%tables0',nres=p_ntable0)
	p_ivtable=j_deflist(j_ivout,'%table',nres=p_ntable0)
	write(6,*)'p_ntable0',p_ntable0
	do i=1,p_nterm
		ivterm=p_termvar(i)
		if(j_otype(ivterm).ne.j_iptable)cycle
		iper=j_putlistobject(p_ivtable,single=ivterm)
		p_istermf(i)=.true.
	enddo !i=1,p_nterm  13892
	p_ntable=j_o(p_ivtable)%i(1)
	p_table=>j_o(p_ivtable)%i2(1:p_ntable)
	p_nlog=0
	p_nfact=0
	p_nlogfact=0
	p_nfactmax=0
	do i=1,p_ntable
 
		ivlog=j_gettablecol(p_table(i))  !j_o(ivterm)%i(4)
		if(j_err)return
		nlog=j_o(ivlog)%i(1)
 
		ivfact=j_gettablerow(p_table(i));if(j_err)return  !j_o(ivterm)%i(5)
		nfact=j_o(ivfact)%i(1)
		p_nlog=p_nlog+nlog
		p_nfact=p_nfact+nfact
		p_nlogfact=p_nlogfact+nlog*nfact
		p_nfactmax=max(p_nfactmax,nfact)
	enddo !i=1,p_ntable  13904
 
	p_ivlog=j_deflist(j_ivout,'%log',nres=p_nlog)
	p_log=>j_o(p_ivlog)%i2(1:p_nlog)
	p_ivkeeplog=j_deflist(j_ivout,'%keeplog',list0=p_nlog,ilist=.true.,point=p_keeplog)
 
	p_ivfact=j_deflist(j_ivout,'%fact',nres=p_nfact,point=p_fact)
 
 
	p_ivlogfact=j_deflist(j_ivout,'%logfact',list0=p_nlogfact,point=p_logfact)
	!	p_ivtabledata=j_deflist(j_ivout,'%tabledata',list0=p_ntable,point=p_tabledata)
	lkm=0
	p_ivtableutil=j_deflist(j_ivout,'%tableutil',list0=p_ntable,point=p_tableutil)
	p_ivtablenlog=j_deflist(j_ivout,'%tablenlog',list0=p_ntable,point=p_tablenlog,ilist=.true.)
	p_ivtableneibas=j_deflist(j_ivout,'%tableneibas',list0=p_ntable,point=p_tableneibas,ilist=.true.)
 
 
	write(6,*)'p_nlogfact ',p_nlogfact
	nlog2=0
	nfact2=0
	nlogfact2=0
	do i=1,p_ntable
		ivtable=p_table(i)
		call j_getname(-1,-1,ivtable)  !oname3
		ivlog=j_gettablecol(p_table(i))  !j_o(ivterm)%i(4)
		if(j_err)return
 
		nlog=j_o(ivlog)%i(1)
		p_tablenlog(i)=nlog
		p_log(nlog2+1:nlog2+nlog)=j_o(ivlog)%i2(1:nlog)
		!	write(6,*)'nlog,knn,ibas',nlog,p_nn,p_tableneibas(i)
		if(i.lt.p_ntable)p_tableneibas(i+1)=p_tableneibas(i)+nlog*p_knn
		!	write(6,*)'nlog,k_nn,p_tableneibas(i+1)',nlog,k_nn,p_tableneibas(i+1)
 
		ivfact=j_gettablerow(p_table(i));if(j_err)return  !j_o(ivterm)%i(5)
		nfact=j_o(ivfact)%i(1)
 
		p_fact(nfact2+1:nfact2+nfact)=j_o(ivfact)%i2(1:nfact)
 
 
 
		do ilog=1,nlog
			ilog2=nlog2+ilog
 
			ik=j_inlist(p_log(ilog2),p_nkeepx,p_keepx)
 
 
			call j_getname(p_log(ilog2))
			if(ik.le.0)then
				write(6,*)j_oname(1:j_loname),' is not in xdata'
				j_err=.true.
				cycle
			endif !if(ik.le.0)  13964
			p_keeplog(ilog2)=ik
			j_o(ivtable)%i2(ilog)=ik
 
			do ifact=1,nfact
				ifact2=nfact2+ifact
				call j_getname(-1,p_fact(ifact2))
				ilofa=j_getobject(0,j_oname(1:j_loname)//'%2%'//j_oname2(1:j_loname2),j_ipreal)
				nlogfact2=nlogfact2+1
				p_logfact(nlogfact2)=ilofa
 
				do j=1,p_nterm
					if(ilofa.eq.p_termvar(j))then
						p_logpart(j)=ilog
						p_factpart(j)=ifact
						p_tabpart(j)=i
						jif=j_inlistobject(p_termvar(j),p_ivvars)
						p_isvarf(jif)=.true.
						p_istermf(j)=.true.
					endif !if(ilofa.eq.p_termvar(j))  13980
					if(ivtable.eq.p_termvar(j))then
						p_tabpart(j)=i
						p_istermf(j)=.true.
						jif=j_inlistobject(p_termvar(j),p_ivvars)
						p_isvarf(jif)=.true.
					endif !if(ivtable.eq.p_termvar(j))  13988
				enddo !j=1,p_nterm  13979
 
			enddo !ifact=1,nfact  13972
		enddo !ilog=1,nlog  13957
		if(j_err)return
		nlog2=nlog2+nlog
		nfact2=nfact2+nfact
 
		p_tableutil(i)=j_defmatrix(j_ivout,'%'//j_oname(1:j_loname)//'%util',nlog,nfact,j_matreg) !ivob)
 
	enddo !i=1,p_ntable  13937
	write(6,*)'log',p_log
	write(6,*)'keeplog',p_keeplog
	write(6,*)'p_tableneibas',p_tableneibas
	if(j_err)return
 
 
	! ivlog=j_gettablecol(ivtable,len=nlog,point=j_templist2)  !j_o(ivtable)%i(4)
	! ivfac=j_gettablerow(ivtable,len=nfac,point=j_templist3)  !j_o(ivtable)%i(5)
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
		ivterm=p_termvar(i)
		ndup=0
		if(j_otype(ivterm).eq.j_iptable)then
			do i2=1,p_nterm-1
				if(p_termvar(i2).eq.ivterm)ndup=ndup+1
			enddo !i2=1,p_nterm-1  14029
 
		endif !if(j_otype(ivterm).eq.j_iptable)  14028
		p_ndupmax=max(p_ndupmax,ndup)
	enddo !i=1,p_nterm  14025
 
	p_ndupmax1=p_ndupmax+1
	p_logfactstep =p_ndupmax1*p_nlog
	!all tableterms
	!	p_ivtableterm=j_deflist(j_ivout,'%tableterm',list0=p_ndupmax1*p_ntable0,ilist=.true.)
 
	p_ivlogfactterm=j_deflist(j_ivout,'%logfactterm',list0=p_ndupmax1*p_nlogfact, &
		ilist=.true.,point=p_logfactterm)
 
	! nxf=0
	! ibas=0
	! nlogbas=0
	! do j=1,p_ntable
	! !	write(6,*)'jtab',j,p_ntable
	! ivtable=p_table(j)
	! ivxt=j_gettablecol(ivtable);if(j_err)return !j_o(ivtable)%i(4)
	! ivft=j_gettablerow(ivtable);if(j_err)return  !j_o(ivtable)%i(5)
 
	! nlog=j_o(ivxt)%i(1)
 
 
	! do ilog=1,nlog
	! ivlo=j_o(ivxt)%i2(ilog)
	! ik=j_inlistobject(ivlo,p_ivkeepx)
 
	! !			j_o(p_ivkeeplog)%i2(nlogbas+ilog)=ik
	! p_keeplog(nlogbas+ilog)=ik
	! do ifact=1,nfact
	! nd=0
	! do i=1,p_nterm
	! if(j_o(p_ivtab)%i2(i).ne.j)cycle
	! if(j_o(p_ivlogpart)%i2(i).ne.ilog)cycle
	! if(j_o(p_ivfactpart)%i2(i).ne.ifact)cycle
	! nd=nd+1
	! p_logfactterm(ibas+nd)=i    !
	! enddo !i=1,p_nterm  14262
	! ibas=ibas+p_ndupmax1  !zero
	! enddo !ifact=1,nfact  14260
	! enddo !ilog=1,nlog  14249
	! nlogbas=nlogbas+nlog
	! enddo !j=1,p_ntable  14240
	if(j_err)return
	p_nvarf=count(p_isvarf)
	p_ntermf=count(p_istermf)
 
	write(6,*)'found ',p_nvarf,' f-variables ',p_ntable,' tables ',p_nvarf-p_ntable,' %2% variables '
	write(6,*)p_ntermf, 'f-terms'
 
	call j_getoption(iob,io,j_mutiltrans,1,1,j_iptrans,.true.,narg,j_optarg0)
	if(j_err)return
	p_ivutiltrans=j_optarg0(1)
 
end subroutine fterms
 
subroutine initfact()
	integer*8 ::i8
 
	!p_knn
	!p_utiltrans
	!p_ivtablecoef
	!p_tablecoef
	! p_knn p_ivutiltrans p_neig p_neigu p_keyfact
 
	write(6,*)'initfact'
 
 
	write(6,*)'p_nlogfact,p_nunits,',p_nlogfact,p_nunits
	!	p_ivtablecoef=j_defmatrix(j_ivout,'%tablecoef',p_nlogfact,p_nunits,j_matreg,point=p_tablecoef)
	!	p_tablecoef=j_o(p_ivtablecoef)%d(1:p_nlogfact*p_nunits)
 
	! do i=1,p_nterm
	! ivx=ivxofterm(i)
 
	! if(ivx.ne.0)then
	! iperk=j_inlistobject(ivx,p_ivkeepx)
	! if(iperk.le.0)then
	! call j_getname(ivx,p_termvar(i),p_ivkeepx)
	! write(6,*)j_oname(1:j_loname),' in ',j_oname2(1:j_loname2),' is not in ',j_oname3(1:j_loname3)
	! j_err=.true.
	! endif !if(iperk.le.0)  14230
	! endif !if(ivx.ne.0)  14228
	! enddo !i=1,p_nterm  14225
	! if(j_err)return
 
	p_kntot=p_knn*p_nlog
	write(6,*)'knnhere',p_knn
	p_kntot2=p_kntot*p_nunits
	p_ivneig=j_deflist(j_ivout,'%neig',list0=p_kntot2,ilist=.true.,ncol=p_kntot)
	p_neig=>j_o(p_ivneig)%i2(1:p_kntot2)
	p_ivneigu=j_defmatrix(j_ivout,'%neigu',p_nunits,p_kntot,j_matreg,point=p_neigu) !ivob)
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
	write(6,*)'keyfactinit'
	ibasn=0
	jkey=0
	ibaxmat=0
	ibadatamatx=0
	ibaneigu=0
	ibatablecoeftot=0
 
	p_xps(0:p_nrow)=j_0
	!ibadatamatx0=0
	!	write(6,*)'nfactmax',p_nfactmax
	do i8=1_8,p_nunits
		ibadatamatx0=ibadatamatx
		ibasnold=ibasn
		call j_getobs0(p_ivdatac,i8)
		call dotrans(p_ivutiltrans,1)
		if(j_err)then
			write(6,*)'error occurred for unit ',i8
			return
		endif !if(j_err)  14152
		write(6,*)'i8',i8
		!write(6,*)ibatablecoef,ibatablecoef+p_nlogfact
		!	write(6,*)'p_logfact',p_logfact
		!	write(6,*)'ptablecoef',p_tablecoef
		!	p_tablecoef(ibatablecoef+1:ibatablecoef+p_nlogfact)=j_v(p_logfact)
 
		!j_ierr=0
		do ip=1,p_ntable
			ibatablecoefcur=0
			!	write(6,*)'ip ',ip,p_ivtable
			ibas=0
			ivtable=p_table(ip)
 
			ivcoe=j_gettablecoef(ivtable,point=p_tablecoefcur)
 
			if(j_err)return
 
			!	call j_getname(ivtable,ivcoe)
			!	write(6,*)'djjd ',j_oname(1:j_loname),' ** ',j_oname2(1:j_loname2)
			!	write(6,*) p_tablecoefcur
			!	stop
 
			if(j_err)return
			ivlog=j_gettablerow(ivtable,point=p_tablelog,len=nlog);if(j_err)return
			ivfact=j_gettablecol(ivtable,len=nfact);if(j_err)return
 
			!		ivfact=j_gettablecol(ivtable,point=p_tablefact,len=nfact);if(j_err)return
			ibatablemat=(i8-j_18)*nlog*nfact
			!		write(6,*)'nlog,nfac',nlog,nfact,' ivlog ',ivlog,' ivfact ',ivfact
			!		ivmat=j_getmatrix( p_tabledata(ip),point=j_tempdpoint)
			!	j_tempdpoint(ibatablemat+1:ibatablemat+nlog*nfact)=p_tablecoefcur(1:nlog*nfact)
			!		write(6,*)'ivtable,ivcoe,ivlog,nlog,ivfact,nfact',ivtable,ivcoe,ivlog,nlog,ivfact,nfact
			do j=1,nlog
 
				j_tempvectors(1:nfact)=-p_tablecoefcur(ibas+1:ibas+nfact)
				call ssortp(j_tempvectors,1,nfact,j_itempvector)
				!		if(i8.le.2.and.j.le.2)write(6,*)' index', j_itempvector(1:nfact)
				do k=1,p_knn
 
					j_dapu=p_tablecoefcur(ibas+j_itempvector(k))
					if(j_dapu.gt.j_0)then
						p_neig(ibasn+k)=j_itempvector(k)
						p_neigu(ibasn+k)=j_dapu
 
					endif !if(j_dapu.gt.j_0)  14196
				enddo !k=1,p_knn  14193
				write(6,*)'i8,log',i8,j,p_tablecoefcur(ibas+1:ibas+nfact),j_itempvector(1:nfact)
				write(6,*)p_neig(ibasn+1:ibasn+p_knn)
				jkey=jkey+1
				p_keyfact(jkey)=p_neig(ibasn+1)
				!	if(i8.le.2_8.and.j.le.2)write(6,*)'i8,ip,ilog,jkey,keyfact ',i8,ip,j,jkey,p_keyfact(jkey)
				!	if(i8.le.2_8.and.j.le.2)write(6,*)p_neig(ibasn+1:ibasn+p_knn),p_neigu(ibasn+1:ibasn+p_knn)
				ibas=ibas+nfact
				ibasn=ibasn+p_knn
				ibatablecoefcur=ibatablecoefcur+nfact
			enddo !j=1,nlog  14188
 
		enddo !ip=1,p_ntable  14163
 
		!		write(6,'(14i4)')p_neig(ibasnold+1:ibasn)
 
		if(j_err)return
		!	write(6,*)'i8,p_nlog p_ns(i8),p_nterminrow(0)',i8,p_nlog,p_ns(i8),p_nterminrow(0)
		!		do ilog=1,p_nlog
 
		p_valueopt=j_ninf
		do k=1,p_ns(i8)
 
			p_value=j_0
			j_yes=.true.
			do it=1,p_nterminrow(0)
				itable=p_tabpart(it)
				if(p_isvarx(it).and.j_yes)then
					!		if(i8.le.2)write(6,*)'p_coef(it)*p_xmat(ibaxmat+1)',p_coef(it),p_xmat(ibaxmat+1)
					p_value=p_value+p_xmat(ibaxmat+1)
					j_yes=.false.  ! xmat only once
				elseif(itable.ne.0)then
					ivtable=p_table(itable)
					!	write(6,*)'i8,it,p_table(it),p_knn,ibaneigu,p_knn,p_kntot',i8,it,p_table(it),p_knn,ibaneigu,p_knn,p_kntot
					!	if(i8.le.2_8)write(6,*)'i8,k,it,p_coef(it),ibaneigu,ind,neigu ',&
					!		i8,k,it,p_coef(it),ibaneigu,ibaneigu+(p_table(it)-1)*p_knn+1,p_neigu(ibaneigu+(p_table(it)-1)*p_knn+1)
					!	write(6,*)i8,it,p_table(it)
					itabas=ibaneigu+(itable-1)*p_knn
					do ilog=1,p_tablenlog(itable)
 
						p_value=p_value+p_coef(it)*p_neigu(ibaneigu+p_tableneibas(itable)+1)* &
							p_datamatx(ibadatamatx+j_o(ivtable)%i2(ilog))
 
						!			write(6,*)i8,it,ilog,p_value,p_coef(it),ibaneigu,p_tableneibas(itable),&
						!				p_neigu(ibaneigu+p_tableneibas(itable)+1),&
						!				p_datamatx(ibadatamatx+j_o(ivtable)%i2(ilog)),ibadatamax
 
					enddo !ilog=1,p_tablenlog(itable)  14239
 
				elseif(p_factpart(it).ne.0)then
					p_value=p_value+p_coef(it)*p_datamatx(ibadatamatx+p_keeplog(ilog))
				endif !if(p_isvarx(it).and.j_yes)  14228
 
			enddo !it=1,p_nterminrow(0)  14226
			write(6,*)p_value
			if(p_value.gt.p_valueopt)then
				p_keys(i8)=k
				p_valueopt=p_value
			endif !if(p_value.gt.p_valueopt)  14256
			!		write(6,*)'k,value,valueop',k,p_value,p_valueopt,p_keys(i8)
			ibadatamatx=ibadatamatx+p_nkeepx
		enddo !k=1,p_ns(i8)  14222
 
		do ir=0,p_nrow
 
 
		enddo !ir=0,p_nrow  14264
		!	j_err=.true.
		!	return
		!		enddo !ilog=1,p_nlog  14682
		ibaxmat=ibaxmat+p_ntemp0
		ibaneigu=ibaneigu+p_kntot
 
 
		!	ibadatamatx=ibadatamatx+p_ns(i8)*p_nkeepx
		ibatablecoeftot=ibatablecoeftot+p_nlogfact
 
		!jj=ibadatamax/p_nkeepx
		!	write(6,*)'jj',jj
 
 
	enddo !i8=1_8,p_nunits  14147
	!	write(6,*)'ibaneigu,p_kntot2',ibaneigu,p_kntot2
	deallocate(j_itempvector)
 
 
 
 
end subroutine initfact
 
! subroutine initkeysx()
! integer*8 ::i8
! jkey=0
! ibaxmat=0
 
! p_xps(0:p_nrow)=j_0
 
! do i8=1_8,p_nunits
! p_valueopt=j_ninf
! do k=1,p_ns(i8)
! p_value=p_xmat(ibaxmat+1)
! if(p_value.gt.p_valueopt)then
! p_keys(i8)=k
! p_valueopt=p_value
! endif !if(p_value.gt.p_valueopt)  14397
! ibaxmat=ibaxmat+p_ntemp0
! enddo
! end subroutine
 
subroutine initzvar()
 
 
 
	p_ivzvars=j_deflist(j_ivout,'%zvars',list0=p_nz)
	p_zvars=>j_o(p_ivzvars)%i2(1:p_nz)
	! if(allocated(p_zvars))deallocate(p_zvars)
	! allocate(p_zvars(1:p_nz))
	!	write(6,*)'p_fpresent,p_zopt,p_isvarx',p_fpresent,p_zopt,p_isvarx
	if (p_fpresent) then
 
 
		p_zvars(1:p_nz)=pack(p_vars(1:p_npvar),.not.p_isvarx.and..not.p_isvarf)
	else
		p_zvars(1:p_nz)=pack(p_vars(1:p_npvar),.not.p_isvarx)
		!	write(6,*)'p_is%%%%%%%%%%%%%%%',p_isvarx
		!	write(6,*)'zvr2',p_zvars(1:p_nz)
	endif !if (p_fpresent)  14319
	!		write(6,*)'num55es '
	ivzvar=j_deflist(j_ivout,'%zvars',list0=p_nz,list=p_zvars(1:p_nz))
 
 
 
end subroutine initzvar
 
! function ivxofterm(iterm)
! itable=p_table(iterm) !j_o(p_ivtab)%i2(iterm)
! if(itable.le.0)then
! ivxofterm=0
! else
! ivtable=p_table(itable) !j_o(p_ivtable)%i2(itable)
! ivcol=j_gettablecol(ivtable)
! ikx=p_logpart(iterm)  !j_o(p_ivlogpart)%i2(iterm)
! ivxofterm=j_o(ivcol)%i2(ikx)
! endif !if(itable.le.0)  14408
! end function ivxofterm
