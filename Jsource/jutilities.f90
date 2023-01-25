! Jlp22 Software
!
! Copyright (C) 2021 Juha Lappi and Natural Resources Institute Finland (Luke)
! Author  Juha Lappi
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as
! published by the Free Software Foundation, either version 3 of the
! License, or (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Affero General Public License for more details.
!
! You should have received a copy of the GNU Affero General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!
!-----------------------------------------------------------------------
!
! j_.f90	general subroutines used in J-functions
!
!-----------------------------------------------------------------------
 
 
 
 
 
 
integer function j_function_index(func)  ! %%function get function index for function func
 
	character*(*),intent(in):: func
 
	le=len_trim(func)
	j_function_index=j_isin(func(1:le),j_functions,j_nfunctions)
	!		write(6,*)'ifuncs ,func ,le ',func, le
	!###TESTAUS%%#%%##
	!write(6,*)'jcompil <3557> i=isin(...): input(icur:ipos1)', input(icur:ipos1)
	!20141219 if(i==0)
	if(j_function_index==0) then
 
		io_ = j_isin(func(1:le),o1_functions,o1_nfunctions)
		!				write(6,*)'o_nfunctions, o1_functions ,io_', o1_nfunctions,  o1_functions ,io_
		if(io_ >0) then
			j_function_index = j_nfunctions_+ io_ !1000+io_
		else !if(io_ >0) then
			io_ = j_isin(func(1:le),o2_functions,o2_nfunctions)
			if(io_ > 0) then
				j_function_index = j_nfuncs1+io_ !2000+io_
			else !if(io_ > 0) then
				io_ = j_isin(func(1:le),o3_functions,o3_nfunctions)
				if(io_ > 0) then
					j_function_index = j_nfuncs2+ io_ !  3000+io_
				endif !if(io_ > 0)     52
			endif !if(io_ > 0)     48
		endif !if(io_ >0)     44
	endif !if(j_function_index==0)     40
 
end function !integer function j_function_index(func)
 
subroutine j_print(iv,ofile,oformat,maxlines,debug)
	integer,intent(in)::iv,maxlines
	character*(*),intent(in)::ofile,oformat
	logical,intent(in)::debug
	character*80 name
	logical bin_,isrow
	character*5 apubuf
	integer*8::iobs
	bin_ = .false.
	nu_ = 6
	if(j_err)then
		write(6,*)'*j* calling j_print with j_err=.true.'
		return
	endif !if(j_err)     71
 
	if(ofile.ne.'*')call j_getfile(nu_,'w',ext=ofile)
 
	write(nu_,*)' '
 
	if(j_otype(iv).eq.j_ipdata)then
 
		!	call j_getdataobject(iv,nobs)
		imat=j_getmatrix(iv);if(j_err)return !j_o( iv)%i(1)
		nobs=j_nrows(imat)  !j_o(imat)%i(1)
		do iobs=1,nobs
			call j_getobs0(iv,iobs); if(j_err)return
			if(j_rejected)cycle
			ntot=ntot+1
			if(oformat.eq.'*')then
				write(nu_,*)(j_v(j_o(iob)%i(io+1+j)),j=1,narg)
			else !if(oformat.eq.'*')then
				write(nu_,oformat)(j_v(j_o(iob)%i(io+1+j)),j=1,narg)
			endif !if(oformat.eq.'*')     89
			if(ntot.ge.maxlines)exit
		enddo !iobs=1,nobs     85
		!enddo !do k=1,jndatasetss
		return
	endif !if(j_otype(iv).eq.j_ipdata)     80
 
 
 
	if(debug) then
		write(nu_,*)'variable part:',j_v(iv)
		write(nu_,*)'object type:',j_otype(iv)
		if(allocated(j_o(iv)%i)) write(nu_,*)'i: size',size(j_o(iv)%i), &
			j_o(iv)%i(1:min(100,size(j_o(iv)%i)))
		if(allocated(j_o(iv)%i2)) write(nu_,*)'i2: size',size(j_o(iv)%i2),&
			j_o(iv)%i2(1:min(100,size(j_o(iv)%i2)))
		if(allocated(j_o(iv)%r)) write(nu_,*)'r: size',size(j_o(iv)%r),&
			j_o(iv)%r(1:min(100,size(j_o(iv)%r)))
		if(allocated(j_o(iv)%d)) write(nu_,*)'d: size',size(j_o(iv)%d),&
			j_o(iv)%d(1:min(100,size(j_o(iv)%d)))
		return
	endif !if(debug)    102
 
	if(iv.le.j_namedv)then
		call j_getline(j_ivnames,iv,name,le)
	else !if(iv.le.j_namedv)then
		name(1:1)=' ';le=1
		!		write(6,*)'<55>',iv,j_namedv,j_nv
	end if !if(iv.le.j_namedv)    116
 
	write(6,*)'tasiv,otype',iv,j_otype(iv)
	if(iv.le.j_nv)then
		iotype=j_otype(iv)
	else !if(iv.le.j_nv)then
		iotype=j_ipreal
	end if !if(iv.le.j_nv)    124
	if(iotype.gt.j_ipreal)then  !vois muuttaa otypet
		if(iotype.eq.j_ipmatrix)then
			!	write(nu_,*)' '
			iba=0
			write(nu_,*)' '
			write(nu_,*)name(1:le),' is matrix(',j_o(iv)%i(1),',',j_o(iv)%i(2),' )'
			write(nu_,*)' '
			do i=1,min(maxlines,j_o(iv)%i(1))
				if(oformat.ne.'*')then
					write(nu_,oformat,err=950)(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
					!			write(nu_,j_form_(1:lef),err=950)(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
 
				else !if(oformat.ne.'*')then
 
					write(nu_,'(i5,1x,7(g14.7,1x))')i,(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
				endif !if(oformat.ne.'*')    137
				iba=iba+j_o(iv)%i(2)
 
			end do !i=1,min(maxlines,j_o(iv)%i(1))    136
 
			!		endif !if(iotype.eq.j_ipmatrix0)then
 
 
			if(maxlines.lt.j_o(iv)%i(1))write(nu_,*)'*printing limited by maxlines'
		else if(j_otype(iv).eq.j_iptext)then !if(iotype.eq.j_ipmatrix)then
			if(nu_.eq.6)then
				write(6,*)' '
				write(nu_,*)name(1:le),' is text object with ',j_o(iv)%i(0),' lines:'
			endif !if(nu_.eq.6)    154
			if(iv.eq.j_ivnames)then
				do i=1,j_namedv
					call j_getline(j_ivnames,i,name,le)
					if(j_err)return
					if(le.lt.34)name(le+1:34)=' '
					isiz=0
					if(allocated(j_o(i)%i))isiz=isiz+size(j_o(i)%i)
					if(allocated(j_o(i)%i2))isiz=isiz+size(j_o(i)%i2)
					if(allocated(j_o(i)%r))isiz=isiz+size(j_o(i)%r)
					if(allocated(j_o(i)%ch))isiz=isiz+size(j_o(i)%ch)/4
					if(allocated(j_o(i)%d))isiz=isiz+2*size(j_o(i)%d)
					write(nu_,*)int2(i),' ',name(1:max(34,le)),' type= ',j_objecttypes(j_otype(i))(1:8),' size=',isiz
				end do !i=1,j_namedv    159
			else !if(iv.eq.j_ivnames)then
				if(maxlines.gt.j_o(iv)%i(0))then
					call j_writetext(nu_,iv,0)
					if(nu_.eq.6)write(nu_,*)'///end of text object '
				else !if(maxlines.gt.j_o(iv)%i(0))then
 
					ifi=1
					las=min(maxlines,j_o(iv)%i(0))
					!		endif !if(lirow.gt.0)then
					do i=ifi,las
						call j_writetext(nu_,iv,i)
					enddo !i=ifi,las    180
					write(nu_,*)'*lines ',ifi,'-',las,' printed out of ', j_o(iv)%i(0)
				endif !if(maxlines.gt.j_o(iv)%i(0))    172
			end if !if(iv.eq.j_ivnames)    158
		elseif(j_otype(iv).eq.j_ipstemspline)then !if(iotype.eq.j_ipmatrix)then
			npo=j_o(iv)%i(1);npo2=j_o(iv)%i(2)
			write(nu_,*) name(1:le),' is a stemspline with ',npo2, ' points'
			write(nu_,*)'heights:',0.01*j_o(iv)%r(npo+1:npo+npo2)
			write(nu_,*)'diam.:  ',j_o(iv)%r(1:npo2)
			if(j_o(iv)%i(3).gt.0)call j_printname('generated from stecurves ',j_o(iv)%i(3),' ')
		else if(j_otype(iv).eq.j_ipsmooth)then !if(iotype.eq.j_ipmatrix)then
			write(nu_,*) name(1:le),' is a smoothing spline'
		else if(j_otype(iv).eq.j_ipbitmatrix)then !if(iotype.eq.j_ipmatrix)then
			write(nu_,*) name(1:le),' is ',j_o(iv)%i(1),' x (', j_o(iv)%i(3),':', j_o(iv)%i(2),') bitmatrix: '
			if(j_o(iv)%i(1).gt.maxlines.or.j_o(iv)%i(2).gt.80)&
				write(nu_,*)'showing ',min(j_o(iv)%i(1),maxlines) ,'rows ',min(j_o(iv)%i(2),80),' cols'
			do jj=1,min(j_o(iv)%i(1),maxlines)
				ic=0
				do kk=j_o(iv)%i(3),min(j_o(iv)%i(2),79+j_o(iv)%i(3))
					ic=ic+1
					if(j_ibittest(iv,jj,kk).ne.0)then
						j_cline(ic:ic)='1'
					else !if(j_ibittest(iv,jj,kk).ne.0)then
						j_cline(ic:ic)='0'
					end if !if(j_ibittest(iv,jj,kk).ne.0)    202
				end do !kk=j_o(iv)%i(3),min(j_o(iv)%i(2),79+j_o(iv)%i(3))    200
				write(nu_,'(a)')j_cline(1:ic)
			enddo !jj=1,min(j_o(iv)%i(1),maxlines)    198
		else if(j_otype(iv).eq.j_iplist)then !if(iotype.eq.j_ipmatrix)then
			call j_printlist(nu_,iv)
		elseif(j_otype(iv).eq.j_iptable)then
			call j_getname(j_o(iv)%i(4),j_o(iv)%i(5))
			write(nu_,*)name(1:le),' is table of column LIST ',j_oname(1:j_loname), ' with ',j_o(iv)%i(1),&
				' elements and row LIST ',j_oname2(1:j_loname2),' with ',j_o(iv)%i(2),' elements'
 
		else if(j_otype(iv).eq.j_ipfigure)then !if(iotype.eq.j_ipmatrix)then
			write(nu_,*)name(1:le),' is a figure object with ',j_o(iv)%i(0)-1, ' subfigures '
		else if(j_otype(iv).eq.j_ipregr)then !if(iotype.eq.j_ipmatrix)then
			write(nu_,*)name(1:le),' is a regression object with ',j_o(iv)%i(0), ' indep. variables '
		elseif(j_otype(iv).eq.j_ipproblem)then !if(iotype.eq.j_ipmatrix)then
			nset=j_o(iv)%i(6)
			!j_nsetr=>j_o(j_ivprob)%i2(1:nset)  !i+1 will match nsetd(i)
			!j_nsetd=>j_o(j_ivprob)%i2(nset+1:2*nset)
			!j_isetd=>j_o(j_ivprob)%i2(2*nset+1:2*nset+ndoms)
			!;j_ivdomain=j_o(j_ivprob)%i(3)
			!ivrhs=j_o(j_ivprob)%i(1);ivrhs2=j_o(j_ivprob)%i(2);
			! j_ivdomain=j_o(j_ivprob)%i(3);j_ivrow=j_o(j_ivprob)%i(4)
			ivrhs=j_o(iv)%i(1)
			ivrhs2=j_o(iv)%i(2)
			!irow0=0
			irow=0
			ido=0
			!j_buf=' '
			! do iset=1,nset
 
			! do jj=1,j_o(iv)%i2(nset+iset)
			! ido=ido+1
			! idom=j_o(iv)%i2(2*nset+ido)
			! call j_getline(j_o(iv)%i(3),idom,j_inp,le)
			! if(j_err)return
			! write(6,*)j_inp(1:le)//':'
			! do k=1,j_o(iv)%i2(iset)
			! irow=irow+1
			! call j_getline(j_o(iv)%i(4),irow,j_inp(6:),le)
			! if(j_err)return
			! le=le+5
			! !	write(6,*)'le',le
			! if(irow.eq.1)then
			! ! for minimization  rhs2=-huge
			! if(j_o(ivrhs2)%d(1).eq.0.)then
			! j_inp(1:5)=' max'
 
			! else !if(j_o(ivrhs2)%d(1).eq.0.)then
			! j_inp(1:5)=' min'
 
			! end if !if(j_o(ivrhs2)%d(1).eq.0.)    246
 
			! else !if(irow.eq.1)then
			! apubuf=j_chi5(irow,0); j_inp(1:3)=apubuf(3:5);j_inp(4:5)=') '
			! !	write(6,*)j_o(ivrhs)%r(irow),j_o(ivrhs2)%r(irow),huge(1.)
			! if(j_o(ivrhs)%d(irow).eq.j_o(ivrhs2)%d(irow))then
			! j_inp(le+1:le+1)='='
 
			! j_inp(le+2:le+9)= j_chr8(j_o(ivrhs)%d(irow))
			! le=le+9
			! else !if(j_o(ivrhs)%d(irow).eq.j_o(ivrhs2)%d(irow))then
			! if(j_o(ivrhs)%d(irow).ne.-huge(1.d0))then
			! j_inp(le+1:le+9)='>'//j_chr8(j_o(ivrhs)%d(irow))
			! le=le+9
			! endif !if(j_o(ivrhs)%d(irow).ne.-huge(1.d0))    263
			! if(j_o(ivrhs2)%d(irow).ne.huge(1.d0))then
			! j_inp(le+1:le+10)=' <'//j_chr8(j_o(ivrhs2)%d(irow))
			! le=le+10
			! endif !if(j_o(ivrhs2)%d(irow).ne.huge(1.d0))    267
			! end if !if(j_o(ivrhs)%d(irow).eq.j_o(ivrhs2)%d(irow))    257
 
			! endif !if(irow.eq.1)    244
 
 
 
			!			write(6,*)'le2',le
			write(6,'(a)')j_inp(1:le)
 
 
			!		enddo !k=1,j_o(iv)%i2(iset)    238
			!		enddo !jj=1,j_o(iv)%i2(nset+iset)    232
			!	enddo !iset=1,nset    230
 
 
 
 
		else if(j_otype(iv).eq.j_ipdata)then !if(iotype.eq.j_ipmatrix)then
			ivmat=j_o(iv)%i(1)
			write(nu_,*)' '
			write(nu_,*)name(1:le),' is data object with ',j_o(ivmat)%i(1), 'obs and ',j_o(ivmat)%i(2),&
				'keep vars'
			write(nu_,*)'data matrix: ', j_vname(ivmat)
			!  write(6,*) o(ivmat)%i(1), ' obs', o(ivmat)%i(2), ' vars'
			write(nu_,*)'keep-list: ', j_vname(j_o(iv)%i(2))
			call j_printlist(nu_,j_o(iv)%i(2))
			!write(nu_,*)'vars-list: ', j_vname(j_o(iv)%i(8))
			write(nu_,*)'obs-variable: ', j_vname(j_o(iv)%i(6))
			!if(j_o(iv)%i(6).ne.0)write(nu_,*)'trans: ', j_vname(j_o(iv)%i(6))
			if(j_o(iv)%i(3).ne.0)then
				write(nu_,*)'sub-data: ', j_vname(j_o(iv)%i(3))
				write(nu_,*)'nobsw-variable: ', j_vname(j_o(iv)%i(4))
			end if !if(j_o(iv)%i(3).ne.0)    305
			if(j_o(iv)%i(5).ne.0)then
				write(nu_,*)'up-data: ', j_vname(j_o(iv)%i(5))
				write(nu_,*)'obsw-variable: ', j_vname(j_o(iv)%i(7))
			end if !if(j_o(iv)%i(5).ne.0)    309
			if(j_o(iv)%i(8).ne.0)write(nu_,*)'nobswcum-variable: ', j_vname(j_o(iv)%i(7))
			!if(j_o(iv)%i(11).ne.0)then
		else if(j_otype(iv).eq.j_ipchar)then !if(iotype.eq.j_ipmatrix)then
			write(nu_,*)(j_o(j_ivnames)%ch(jj),jj=j_o(iv)%i(1),j_o(iv)%i(2))
		else if(j_ipc(iv).ne.0)then !if(iotype.eq.j_ipmatrix)then
			call j_getchar(iv,name(le+2:),le2)
			write(nu_,*)name(1:le),"='",name(le+2:le+le2+1),"'"
		end if !if(iotype.eq.j_ipmatrix)    130
	else !if(iotype.gt.j_ipreal)then
		if(j_otype(iv).eq.j_ipreal)then
			!iv_ = j_o(iob)%i(io+1+j)
			if(iv.le.j_namedv)then
 
				write(nu_,*)name(1:le),'=',j_v(iv)
			else !if(iv.le.j_namedv)then
				write(nu_,*)j_v(iv), 'is constant'
			endif !if(iv.le.j_namedv)    324
		end if !if(j_otype(iv).eq.j_ipreal)    322
	end if !if(iotype.gt.j_ipreal)    129
	!	end do !do j=1,narg
 
	return
	950 write(6,*)'*error in format'
	j_err=.true.
	return
end subroutine !subroutine j_print(iv,ofile,oformat,maxlines,debug)
 
subroutine  j_getcurline(iob,io)
	integer,intent(in)::iob,io
	ivsource=j_o(iob)%i2(11)
	ili=1
	do i=1,io-1
		if(j_o(iob)%i(i).lt.0)then
			ili=ili-j_o(iob)%i(i)
		endif !if(j_o(iob)%i(i).lt.0)    345
	enddo !i=1,io-1    344
	j_curline(j_recursion)=ili
end subroutine
 
 
 
 
subroutine j_errexit() ! error exit from nested ;incl -files
 
	!module inpmod
	!end module
 
	!use j_globalfuncsmod, only: printname
 
	!module vmod
	!end module vmod
 
	! use j_omod, only : oinp
	integer,allocatable :: closed(:)
	integer ::nclosed
	logical notsame
	character*10 act
	character*1 ch
	if(j_nused.gt.0)allocate(closed(1:j_nused))
	nclosed=0
	!
	call j_clearoption(iob,i)
77 format(1x,a79/)
!	write(6,*)'errexit ',j_ninc
	if(j_ninc.gt.1)then
		!	write(6,*)
 
 
 
 
 
		n1=j_o(j_ivinput0)%i( j_o(j_ivinput0)%i(0)+1) -1
		n2=j_o(j_ivinput1)%i( j_o(j_ivinput1)%i(0)+1) -1
		n3=j_o(j_ivinput2)%i( j_o(j_ivinput2)%i(0)+1) -1
		! if(n1.ne.n2)then
		! write(6,*)'**input after removing comments and spaces: '
		! call j_printtext(j_ivinput1,0)
		! endif
		notsame=.false.
		if(n1.eq.n2)then
			notsame=any(j_o(j_ivinput1)%ch(1:n1).ne.j_o(j_ivinput2)%ch(1:n2))
		else !if(n1.eq.n2)then
			notsame=.true.
		endif !if(n1.eq.n2)    392
		if(notsame)then
			write(6,*)'***cleaned input'
			call j_printtext(j_ivinput1,0)
		else
			write(6,*)'**input: '
			!			write(6,*)'original'
			call j_printtext(j_ivinput0,0)
 
		endif !if(notsame)    397
		notsame=.false.
		if(n2.eq.n3)then
			notsame=any(j_o(j_ivinput1)%ch(1:n2).ne.j_o(j_ivinput2)%ch(1:n3))
		else !if(n2.eq.n3)then
			notsame=.true.
		endif !if(n2.eq.n3)    407
		if(notsame)then
			!	write(6,*)'**input after interpreting input programming: '
			write(6,*)'interpreted'
			call j_printtext(j_ivinput2,0)
		endif !if(notsame)    412
 
		! write(6,77)inp(1:linp)
		! write(6,*)'**previous line:',inpold(1:linpold)
		!20140812 J2.2
		!elseif(oinp) then
		!	write(6,*)'*j error ',inp(1:linp)
	endif !if(j_ninc.gt.1)    377
 
	!write(6,*)'nul0',nul(0)
	nul0_=j_ninc
	ial=2
	if(j_v(j_ivcontinue).ne.0.d0)ial=3
	do i=ial,j_ninc
		!	write(6,*)'nul',nul0_,i,j_nul(i)
 
		!400
		iiv=j_inciv(i)
		write(6,*) ' after using ', j_o(iiv)%i(6),' lines from ',j_vname(iiv),' in ;incl nesting ', i
		!	call j_closeunit(j_nul(i))
		! write(6,*)' ifiout_trans', nuliv(nul(i))
		!	endif !if(j_nul(i).gt.0)then
	enddo !i=ial,j_ninc    429
 
	!this may not work if we  are in loops
	jninc=j_ninc
	j_ninc=1
	j_ndo=0
	j_niifs=0  ! open ifthens
	j_bypa(0)=.false.
 
	call j_zerondo()
 
	if(j_v(j_ivcontinue).ne.0.d0)then
		write(6,*)'*Continue even if error has occured'
		j_err=.false.
		j_v(j_iverr)=1.d0
		j_ninc=min(jninc,2)
		!		write(6,*)'j_nincnow',j_ninc
		return
	endif !if(j_v(j_ivcontinue).ne.0.d0)    449
 
	! if(j_commandbuf(0)>0) then
	! write(6,*)'closing ',j_commandbuf(0),' command buffers'
	! j_commandbuf(0) = 0
	! endif
	if(nul0_.eq.1)return
	do i_=j_nused,1,-1
		inquire(j_nunits(i_),NAME=j_tempchar2,ACTION=act)
		lenact=len_trim(act)
		!	write(6,*)'unit', j_nunits(i_),act
		!		write(6,*)'<112>',j_nunits(i_),j_tempchar2(1:20),act
 
		if(act(1:lenact)=='READ') then
			write(6,*)'closing read file ', j_tempchar2(1:len_trim(j_tempchar2))
			!call j_closeunit(j_nunits(i_))
			nclosed=nclosed+1
			closed(nclosed)=j_nunits(i_)
		else !if(act(1:lenact)=='READ') then
			write(6,*)'write file ', j_tempchar2(1:len_trim(j_tempchar2)),' remains open'
		endif !if(act(1:lenact)=='READ')    469
	enddo !i_=j_nused,1,-1    463
	!		write(6,*)j_nused,'+',j_nunits(j_nused+1:6)
	do i_=1,nclosed
		call j_closeunit(closed(i_))
	enddo !i_=1,nclosed    479
	! j_err=.false.
	return
end subroutine j_errexit !subroutine j_errexit()
 
 
!%%functionj_checkout
subroutine j_checkoutput(iob,io)
	! subroutine checks that no argument is the same object as the output
	! or any of the option arguments
	! if an argument is a transformation then it is also checked that no of the input or output variables is the same as the output
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	do i=1,narg
		iarg=j_o(iob)%i(io+i+1)
		if(iarg.eq.iout)then
			write(6,*)'*argument ',i, 'iob,io,narg',iob,io,narg,' ifunc ',j_o(iob)%i(io)
			call j_printname('*is the same as the output ',iout,' ')
			j_err=.true.
		endif !if(iarg.eq.iout)    496
		if(j_otype(iarg).eq.j_iptrans)then
			do ili=1,2
				list=j_o(iarg)%i2(ili)
				il=j_inlistobject(iout,list)
				if(il.gt.0)then
					write(6,*)'*argument ',il
					call j_printname('*has the same input or output as the output ',iout, &
						' of the function ')
					j_err=.true.
				endif !if(il.gt.0)    505
			enddo !ili=1,2    502
		endif !if(j_otype(iarg).eq.j_iptrans)    501
	enddo !i=1,narg    494
	!integer, dimension(j_maxopenopt) :: j_optioniob
	!	integer*2, dimension(2,j_maxopenopt) :: j_optionmoptio
 
	optloop:	do i=1,j_nopt
		iopt=j_optionmoptio(1,i)
		if(j_optioniob(i).ne.iob.or.j_optionmoptio(2,i).ne.io)cycle optloop
		lin= j_optionlink(i)
		nargo=j_o(iob)%i(lin)
		if(nargo.le.0)cycle optloop  !no arguments
 
 
 
		do j=1,nargo
			!	write(6,*)'nargo',nargo,j_curropt(i),j_nopt
			iarg=j_o(iob)%i(lin+j)
			!		write(6,*)'<22>iarg',iarg,iout
			!		call j_printname('hui ',iarg,' ')
			if(iout.eq.iarg)then
				write(6,*)'*argument ',j,' of option ',j_option_name(iop,&
					j_lenoptions(iopt))
				call j_printname('*is the same as the output ',iout,' ')
				j_err=.true.
			endif !if(iout.eq.iarg)    531
			if(j_otype(iarg).eq.j_iptrans)then
				do ili=1,2
					list=j_o(iarg)%i2(ili)
					il=j_inlistobject(iout,list)
					if(il.gt.0)then
						write(6,*)'*argument ',j,' of option ',&
							j_option_name(iopt,j_lenoptions(iopt))
						call j_printname('*has the same input or output as the output ',iout, &
							' of the function ')
						j_err=.true.
					endif !if(il.gt.0)    541
				enddo !ili=1,2    538
			endif !if(j_otype(iarg).eq.j_iptrans)    537
		enddo !j=1,nargo    526
	end do optloop !loop:	do i=1,j_nopt    517
	!	if(j_err)io=io+narg+3
end subroutine j_checkoutput !subroutine j_checkoutput(iob,io)
 
subroutine j_checkoutput0(iob,io)
	! subroutine checks that no argument is the same object as output
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	do i=1,narg
		iarg=j_o(iob)%i(io+i+1)
		if(iarg.eq.iout)then
			write(6,*)'*argument ',i, 'of ',narg
			call j_printname('*is the same as the output ',iout,' ')
			j_err=.true.
		endif !if(iarg.eq.iout)    562
	enddo !i=1,narg    560
 
	!	if(j_err)io=io+narg+3
end subroutine j_checkoutput0 !subroutine j_checkoutput0(iob,io)
 
logical function j_isdollar(iv)
	integer,intent(in)::iv
	j_isdollar=.false.
 
 
	if(iv.le.j_namedv)then
		j_isdollar=j_o(j_ivnames)%ch(j_o(ivnames)%i(iv)).eq.'$'
 
 
	endif !if(iv.le.j_namedv)    577
 
	! endif
	return
end function !logical function j_isdollar(iv)
 
subroutine j_startfunction(iob,io,iptype,narg,arg,ivout,delout,iptype2,minarg,maxarg,needsout) ! %%function
	! iob,io current compiled object and the place there
	! iptype type of arguments for j-function, look at j_modules.f90 for available types
	! if arguments must be of o type then use ?? to get the object type
	! if arguments can be of different type then give value 0 and check
	! argument types individually
 
	! narg number of arguments (output)
	! arg integer pointer to argumensts, if there are no arguments it points to j_arglist0 which
	! has one argument whose value is zero
 
 
 
	implicit none
	integer, intent(in):: iob,iptype
	integer, intent(in),optional:: iptype2,minarg,maxarg
	logical, intent(in),optional:: delout
	logical,optional,intent(in)::needsout
	integer,intent(in) :: io
	!	logical, intent(in) :: expand
	integer, intent(out):: narg, ivout
	integer, dimension (:), pointer :: arg
	integer:: ifunc0,irgtype
 
	logical yes
	character*60 name
	character*16 functions, objecttype
	integer :: ifunc,iarg1,i_,le,noptarg
 
	!	write(6,*)'check,iob,io ',iob,io
	!	call j_checkoutput(iob,io)
	!	write(6,*)'check2,iob,io ',iob,io
	!	if(j_err)return    ! checkoutput updates io
 
	ifunc = j_o(iob)%i(io)
	!ifunc=ifunc-j_nspecialtot
	narg=j_o(iob)%i(io+1)
	ivout=j_o(iob)%i(io+2+narg)
	if(ivout.eq.j_ivresult)then
		if(present(needsout))then
			write(6,*)j_function_name(ifunc,j_lenfunctions(ifunc)),'() should have output'
			j_err=.true.;return
		endif !if(present(needsout))    626
	endif !if(ivout.eq.j_ivresult)    625
	if(present(delout))then
 
		if(delout.and.j_otype(ivout).ne.j_ipreal)call j_del(ivout)
		!	else
		!		if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
	endif !if(present(delout))    631
	if(narg>0) then
		iarg1=j_o(iob)%i(io+2)
		! if(expand.and.j_otype(iarg1).eq.j_iplist)then
		! if(narg==1) then
		! narg=j_o(iarg1)%i(1)
		! arg=>j_o(iarg1)%i(1:narg)
		! else !if(narg==1) then
		! arg => j_o(iob)%i(io+2:io+1+narg)
		! write(6,*)'first argument of ',j_function_name(ifunc,j_lenfunctions(ifunc)),' is list and should not have other arguments'
		! j_err=.true.
		! endif !if(narg==1)    640
		! else !if(expand.and.j_otype(iarg1).eq.j_iplist)then
		arg => j_o(iob)%i(io+2:io+1+narg)
		!		endif !if(expand.and.j_otype(iarg1).eq.j_iplist)    639
		if(iptype/=0) then
			yes=.true.
			!allocate(j_v(1:j_mxv))  !
			!allocate(j_o(1:j_nv))
			do i_=1,narg
				if(j_otype(arg(i_))/=iptype) then
					irgtype=j_otype(arg(i_))
					!	write(6,*)'iarg',i_,irgtype,iptype
					if(arg(i_).le.j_namedv)then
						call j_getline(j_ivnames,arg(i_),name,le)
					elseif(arg(i_).le.j_nv)then !if(arg(i_).le.j_namedv)then
						name='temporary variable'
						le=18
					else !if(arg(i_).le.j_namedv)then
						name='constant'
						le=8
					endif !if(arg(i_).le.j_namedv)    659
					if(j_err)return
					if(yes) write(6,*)j_function_name(ifunc,j_lenfunctions(ifunc)),' should have ',&
						j_objecttype_name(iptype,j_lenobjecttypes(iptype)),' arguments '
					write(6,*)name(1:le),' is ',j_objecttype_name(irgtype,j_lenobjecttypes(irgtype))
 
 
					yes=.false.
					j_err=.true.
				endif !if(j_otype(arg(i_))/=iptype)    656
			enddo !i_=1,narg    655
		endif !if(iptype/=0)    651
	else !if(narg>0) then
		arg => j_arglist0
	endif !if(narg>0)    637
	if(present(minarg))then
		if(narg.lt.minarg)then
 
			write(6,*)'*function ',j_function_name(ifunc,j_lenfunctions(ifunc)),'should have ', minarg ,' arguments',&
				'and it has ',narg
 
			j_err=.true.
		endif !if(narg.lt.minarg)    683
	endif !if(present(minarg))    682
	if(present(maxarg))then
		if(narg.gt.maxarg)then
 
			write(6,*)'*w* : function ',j_function_name(ifunc,j_lenfunctions(ifunc)),'has ', narg,' arguments',&
				' maximum is ',maxarg
			j_err=.true.
			!		call j_addwarning(iob,io,ifunc)
		endif !if(narg.gt.maxarg)    692
	endif !if(present(maxarg))    691
	! call j_getoption_index(iob,io,j_mtitle,-1,1,j_ipchar,.true.,noptarg,j_optarg0)
	! if(j_optarg0(1).gt.0.and.ivout.le.j_namedv.and.ivout.ne.j_ivresult)then
	! j_otitle(ivout)=j_optarg0(1)
	! else !if(j_optarg0(1).gt.0.and.ivout.le.j_namedv.and.ivout.ne.j_ivresult)then
	! if(ivout.le.j_mxnamedv)j_otitle(ivout)=0
	! endif !if(j_optarg0(1).gt.0.and.ivout.le.j_namedv.and.ivout.ne.j_ivresult)then
 
	j_iob(ivout)=iob
	j_io(ivout)=io
	!90	continue !io=io+narg+3
 
	return
end subroutine j_startfunction !subroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout,delout,iptype2,minarg,maxarg)
 
!subroutine j_getoption2_index(iob,io,moption,minarg,maxarg,iptype,needsarg,narg,optarg) ! %%option
!INPUT:
! This can be called using the generic name j_getoption2
!The difference beween getoption and getoption2 is that here optarg
!is an allocatable vector, so that this can be used when option argument is lis and is
!then expanded
! iob is the current compiled transformation, as in the function input
! moption the option inxed= m//option_name
! minarg minimum number of arguments,
!  =-1 is option is not necessary
! =  0 if option is necessary but arguments are not
! iptype necessary argument type, if possible several types give value 0  %%object_type
! expand :is list argument expanded
! needsarg .true. if there must be arguments if option is present
!OUTPUT:
!noptarg = number of arguments, -1 if option is not present
!optarg pointer to argument objects
!     if option not present optarg(1)=-1
! if noptarg=0 then optarg(1)=0
! for code options the optional argument istart returns the starting point for the code and
!optarg(1) returns the index of the result variable in vector j_v
! if a code option is not present istart=0 and noptarg=0
 
 
! integer, intent(in):: iob
! integer, intent(in):: io
! integer, intent(in):: moption
! integer, intent(in):: minarg
! integer, intent(in)::maxarg
! integer, intent(in):: iptype
! !	logical, intent(in):: expand
! logical, intent(in):: needsarg
! integer,intent(out) :: narg
! integer, dimension (:), allocatable :: optarg
! !integer,optional,intent(out) :: istart
! !	save optarg,maxn
! !character*14 options
 
! if(moption<=0.or.moption>j_noptions) then
! write(6,*)'*j* getoption illegal moption  ',moption,iptype,needsarg,narg
! j_err=.true.
! !	j_err = .true.
! return
! endif !if(moption<=0.or.moption>j_noptions.or.iptype.lt.0.or.iptype.gt.j_nobjecttypes) then
 
 
! !	noptarg = -1
! nopv=j_nopt
! ! write(6,*)'in getoption,iob,io,',iob,io,' moption ',moption,'j_nopt',j_nopt
! ! write(6,*)'option1,mopt,io',j_optionmoptio(1:2,1),'iob',j_optioniob(1)
! ili=j_linkoption(iob,io,moption,clear=.true.)
! ! write(6,*)'gaft,j_nopt',j_nopt, 'mopt,io',j_optionmoptio(1:2,1),'iob',j_optioniob(1)
! ! if(ili.ge.0.and.j_nopt.eq.nopv)stop
! !	noptarg=ili
! ! write(6,*)'<666',j_options(moption),'ili',ili,'io ',io,'iob',iob
! ! do i=1,j_nopt
! ! write(6,*)j_optionmoptio(1:2,i),j_optioniob(i)
! ! enddo
! !write(6,*)'hep 6'
! if(ili.lt.0.and.minarg.ge.0)then
! write(6,*)'** ',j_option_name(moption,j_lenoptions(moption)),'-> is missing '
! j_err=.true.
! return
! endif
! if(ili>0) then
 
! narg0 = j_o(iob)%i(ili)
! !		write(6,*)'hep 6 narg0',nrg0
 
 
! if(.not.allocated(optarg))then
! maxn=max(2*narg0,50)
! allocate(optarg(1:maxn))
! else
! maxn=size(optarg)
! endif
! !	write(6,*)'hep 68 a',allocated(optarg)
! goto 20
! 10		deallocate(optarg)
! allocate(optarg(1:2*maxn))
! maxn=2*maxn
 
! !	write(6,*)'hep 68 b'
! 20	narg=0
! !	write(6,*)narg0,'??'
! do i=1,narg0
! !	write(6,*)'hep 68',i
! iv=j_o(iob)%i(ili+i)
! if(j_otype(iv).eq.j_iplist)then
 
! do j=1,j_o(iv)%i(1)
! ik=j_o(iv)%i2(j)
! if(j_otype(ik).eq.j_iplist)then
! do k=1,j_o(ik)%i(1)
! narg=narg+1
! if(narg.gt.maxn)goto 10
! optarg(narg)=j_o(ik)%i2(k)
! enddo
! else
! narg=narg+1
! if(narg.gt.maxn)goto 10
! optarg(narg)=ik
! endif
 
! enddo
! else
 
! narg=narg+1
! if(narg.gt.maxn)goto 10
! !			write(6,*)'tas',narg,iv,maxn
! optarg(narg)=iv
! endif
 
 
! enddo
 
! elseif(ili.eq.0)then !if(ili>0) then
! if(needsarg) then
! write(6,*)'option ',j_option_name(moption,j_lenoptions(moption)),' must have arguments'
! j_err = .true.
! endif !if(needsarg) then
! optarg(1)=0
! endif !if(ili>0) then
 
! !write(6,*)'hep 69',i
! if(narg.lt.minarg)then
 
! write(6,*)'option ',j_option_name(moption,j_lenoptions(moption)),'-> should have ', minarg ,' arguments',&
! ' and it has ',narg
! j_err=.true.
! endif !if(noptarg.lt.minarg)then
! if(narg.gt.maxarg)then
 
! write(6,*)'*w* : option ',j_option_name(moption,j_lenoptions(moption)),' has ', narg,'arguments',&
! ' but can have only ',maxarg
! j_err=.true.
! endif !if(noptarg.gt.maxarg)then
! !	j_linkoption(iob,io,moption)=0
 
! return
! end subroutine j_getoption2_index !subroutine j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
 
 
 
subroutine j_getoption_index(iob,io,moption,minarg,maxarg,iptype,needsarg,noptarg,optarg) ! %%option
	!INPUT:
	! This can be called using the generic name j_getoption
	! iob is the current compiled transformation, as in the function input
	! moption the option inxed= m//option_name
	! minarg minimum number of arguments,
	!  =-1 is option is not necessary
	! =  0 if option is necessary but arguments are not
	! iptype necessary argument type, if possible several types give value 0  %%object_type
	! needsarg .true. if there must be arguments if option is present
	!OUTPUT:
	!noptarg = number of arguments, -1 if option is not present
	!optarg pointer to argument objects
	!     if option not present optarg(1)=-1
	! if noptarg=0 then optarg(1)=0
	! for code options the optional argument istart returns the starting point for the code and
	!optarg(1) returns the index of the result variable in vector j_v
	! if a code option is not present istart=0 and noptarg=0
 
 
	integer, intent(in):: iob
	integer, intent(in):: io
	integer, intent(in):: moption
	integer, intent(in):: minarg
	integer, intent(in)::maxarg
	integer, intent(in):: iptype
	!logical, intent(in):: expand
	logical, intent(in):: needsarg
	integer,intent(out) :: noptarg
	integer, dimension (:), pointer :: optarg
	!integer,optional,intent(out) :: istart
 
	!character*14 options
	!write(6,*)'minarg,maxarag',minarg,maxarg
 
	if(moption<=0.or.moption>j_noptions.or.iptype.lt.-1.or.iptype.gt.j_nobjecttypes) then
		write(6,*)'*j* getoption illegal moption or iptype ',moption,iptype
		j_err=.true.
		!	j_err = .true.
		return
	endif !if(moption<=0.or.moption>j_noptions.or.iptype.lt.-1.or.ipt    893
 
 
	!	noptarg = -1
	nopv=j_nopt
	! write(6,*)'in getoption,iob,io,',iob,io,' moption ',moption,'j_nopt',j_nopt
	! write(6,*)'option1,mopt,io',j_optionmoptio(1:2,1),'iob',j_optioniob(1)
	ili=j_linkoption(iob,io,moption,clear=.true.)
	! write(6,*)'gaft,j_nopt',j_nopt, 'mopt,io',j_optionmoptio(1:2,1),'iob',j_optioniob(1)
	! if(ili.ge.0.and.j_nopt.eq.nopv)stop
	noptarg=ili
	! write(6,*)'<666',j_options(moption),'ili',ili,'io ',io,'iob',iob
	! do i=1,j_nopt
	! write(6,*)j_optionmoptio(1:2,i),j_optioniob(i)
	! enddo
 
	if(ili>0) then
		noptarg = j_o(iob)%i(ili)
 
		! if(moption.eq.j_mread)then
		! write(6,*)'ili,noptarg',ili,'*',j_o(iob)%i(ili-2:ili+5)
		! write(6,'(20i5/)')j_o(iob)%i(1:j_o(iob)%i(0))
		! endif
 
		!	write(6,*)'ili,noptarg',ili,noptargi
		!	write(6,'(20i5/)')j_o(iob)%i(1:j_o(iob)%i(0))
		!if(present(istart))istart=j_linkoption(iob,io,moption)
		optarg => j_o(iob)%i(ili+1:ili+noptarg)
		if(iptype.gt.0.or.iptype.eq.-1)then
			! if(iptype.gt.j_nobjecttypes_)then
			! write(6,*)'*j* argument iptype has illegal value ',iptype,' in option ',j_options( moption)
			! j_err=.true.;return
			! endif !if(iptype.lt.0.or.iptype.gt.j_nobjecttypes_)    927
			do i=1,noptarg
				if(iptype.eq.-1)then
					if(j_otype(optarg(i)).ne.j_ipreal)then
						call j_getname(optarg(i))
						write(6,*)'*w* ',j_oname(1:j_loname),' is made REAL it was ',j_objecttypes(j_otype(optarg(i)))
						call j_del(optarg(i))
					endif !if(j_otype(optarg(i)).ne.j_ipreal)    933
				elseif(j_otype(optarg(i)).ne.iptype)then
					call j_getname(optarg(i))
					leno=len_trim(j_options( moption))
					write(6,*)'*arguments of ',j_options( moption)(1:leno),'-> must be ',&
						j_objecttypes(iptype)
					write(6,*)j_oname(1:j_loname),' is ',j_objecttypes(j_otype(optarg(i)))
					if(iptype.eq.j_ipreal)write(6,*)'*note objects can be made REAL using delete_o() function'
					j_err=.true.
				endif !if(iptype.eq.-1)    932
			enddo !i=1,noptarg    931
 
		endif !if(iptype.gt.0.or.iptype.eq.-1)    926
		!write(6,*)'<668',noptarg,optarg
	elseif(ili.eq.0.and.minarg.gt.0)then !if(ili>0) then
		if(needsarg) then
			write(6,*)'option ',j_option_name(moption,j_lenoptions(moption)),' must have arguments'
			j_err = .true.
		endif !if(needsarg)    952
		optarg => j_arglist0
	else !if(ili>0) then
		optarg=>j_arglist1
	endif !if(ili>0)    914
 
 
	if(noptarg.lt.minarg)then
		if(noptarg.lt.0)then
			write(6,*)'option ',j_option_name(moption,j_lenoptions(moption)),'-> should be present with ', minarg ,' arguments'
 
		else
			write(6,*)'option ',j_option_name(moption,j_lenoptions(moption)),'-> should have ', minarg ,' arguments',&
				' and it has ',noptarg
		endif !if(noptarg.lt.0)    963
		j_err=.true.
	endif !if(noptarg.lt.minarg)    962
	if(noptarg.gt.maxarg)then
 
		write(6,*)'*w* : option ',j_option_name(moption,j_lenoptions(moption)),' has ', noptarg,'arguments',&
			' but can have only ',maxarg
		j_err=.true.
	endif !if(noptarg.gt.maxarg)    972
	!	j_linkoption(iob,io,moption)=0
 
	return
end subroutine j_getoption_index !subroutine j_getoption_index(iob,io,moption,minarg,maxarg,iptype,needsarg,noptarg,optarg)
 
 
subroutine j_getoption_name(iob,io,option,minarg,maxarg,iptype,needsarg,noptarg,optarg) !%%option
 
	! This can be also called using the generic name j_getoption
	! option is the option name
	! see j_getoption_index for other arguments
 
 
	integer, intent(in):: iob
	integer, intent(in):: io
	character*(*) option
	integer, intent(in):: minarg
	integer, intent(in)::maxarg
	integer, intent(in):: iptype
	!logical, intent(in):: expand
	logical, intent(in):: needsarg
	integer,intent(out) :: noptarg
	integer, dimension (:), pointer :: optarg
 
 
	!function iopts(opt)
	moption = j_iopts(option)
 
	call j_getoption_index(iob,io,moption,minarg,maxarg,iptype,needsarg,noptarg,optarg)
 
	return
end subroutine j_getoption_name !subroutine j_getoption_name(iob,io,option,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
 
! subroutine j_getoption2_name(iob,io,option,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) !%%option
 
! ! This can be also called using the generic name j_getoption
! ! option is the option name
! ! see j_getoption_index for other arguments
 
 
! integer, intent(in):: iob
! integer, intent(in):: io
! character*(*) option
! integer, intent(in):: minarg
! integer, intent(in)::maxarg
! integer, intent(in):: iptype
! logical, intent(in):: expand
! logical, intent(in):: needsarg
! integer,intent(out) :: noptarg
! integer, dimension (:), allocatable :: optarg
 
 
! !function iopts(opt)
! moption = j_iopts(option)
 
! call j_getoption2_index(iob,io,moption,minarg,maxarg,iptype,needsarg,noptarg,optarg)
 
! return
! end subroutine j_getoption2_name !subroutine j_getoption_name(iob,io,option,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
 
 
! function j_linkcodeoption_index(moption)  ! %%option %%codeoption
! use j_omod, only: j_noptions
! use getmod, only: j_crash
! use j_omod, only: j_linkopt
! use j_omod, only: j_linkopt2   !used to run options
 
! integer, intent(IN) :: moption
 
! if(moption<=0.or.moption>j_noptions) then
! write(6,*)'*j* linkcodeoption: illegal argument ',moption
! j_err=.true. !	j_err = .true.
! return
! endif
! j_linkcodeoption_index=0
! if(j_linkoption(iob,io,moption).gt.0)then
! j_linkcodeoption_index=j_linkopt2(moption)
! j_linkoption(iob,io,moption)=0
! endif
! return
! end function j_linkcodeoption_index
 
 
! function j_linkcodeoption_name(option)
! use getmod, only: j_iopts
! use getmod, only: j_linkcodeoption_index
 
! character*(*), intent(in):: option
 
! moption = j_iopts(option)
! j_linkcodeoption_name=j_linkcodeoption_index(moption)
! return
! end function j_linkcodeoption_name
 
 
!20150203 jcommand
recursive subroutine j_command(commandline,passoptions) !execute single %%function from within other function (%%basic)
 
	character*(*), intent(in):: commandline
	logical, optional :: passoptions
	integer,dimension(3) ::ivjcommand=0
	!	integer ::recursion=0
	logical::p=.false.
	!	integer :: ivjcommand=0
	save ivjcommand   !,recursion
	if(j_err)then
 
		write(6,*)'*j_command: error detected at start, recursion level ',recursion,' command:'
		!write(6,*)commandline
		stop
	endif !if(j_err)   1083
	! if(.not.present(passoptions))then
	! if(j_nopt.gt.0)then
	! write(6,*)'*j* ?? j_command started without clearing options first with clearopt or clearopt, command:'
	! !	write(6,*)commandline
	! j_err=.true.
	! return
	! endif !if(j_nopt.gt.0)then
	! endif !if(.not.present(passoptions))then
	j_recursion=j_recursion+1
	if(j_recursion.gt.3)then
		write(6,*)'*recursion in j_command is too deep ',j_recursion
		j_err=.true.
		goto 900
	endif !if(j_recursion.gt.3)   1098
	if(ivjcommand(j_recursion).eq.0)then
		!20150812(arg1<->arg2) oli: 		!subroutine deftrans(name,iv,leng,ivout,lenin,lenout,ivinl,ivoutl,linsource)
		!subroutine deftrans(iv,name,leng,ivout,lenin,lenout,ivinl,ivoutl,linsource)
		!20150812(arg1<->arg2) oli: 		call deftrans('$jcommand',0,100,ivjcommand,0,0,ii,ii,0)
		ivjcommand(j_recursion)=j_deftrans(0,'$jcommand'//char(48+j_recursion),200,0,0,ii,ii,ii,ii,0)
	endif !if(ivjcommand(j_recursion).eq.0)   1103
	!subroutine jcompil(input,ivteku,oneline,newin,ivinl,ivoutl,matrix,localin,localout,jhaka) !trasnforamtion interpreter
	!write(6,*)'<337>',commandline
	!call j_compiler(commandline,ivjcommand(recursion),.true.,0,0,&
	!	j_matrix0,j_matrix0,j_matrix0,.false.)
	! 167	write(6,*)'sit>'
	! read(5,'(a)')j_filename
	! linp=len_trim(j_filename)
	! if(j_filename(1:linp).eq.'e')stop
	! write(6,*)'*got:',j_filename(1:linp)
	! call j_parser(j_filename(1:linp),ivjcommand(recursion))
	! goto 167
	!	write(6,*)'<55tas:',commandline
	j_tempchar=commandline
	le=len_trim(commandline)
	call j_clean(j_tempchar,le)
	if(j_err)return
	!	write(6,*)'jcommand ',j_tempchar(1:le)
	!	j_v(j_ivdebug)=300
	call j_parser(j_tempchar(1:le),ivjcommand(j_recursion))
 
	!	if(j_v(j_ivdollar).eq.783.d0)write(6,*)'cl:',commandline,j_recursion
	!	write(6,*)'from interpret:',j_o(ivjcommand(recursion))%i(1:j_o(ivjcommand(recursion))%i(0))
	if(j_err)goto 900
	write(6,*)commandline
 
	!	write(6,*)'aft33 ',j_o(ivjcommand(j_recursion))%i(0:20)
	call dotrans(ivjcommand(j_recursion),1)
	!	write(6,*)'{{',1
	!	if(j_v(j_ivdollar).eq.783.d0)write(6,*)'claft:',commandline,j_recursion
	if(j_err)goto 900
	lec=len(commandline)
	if(lec.gt.7)then
		if(commandline(1:6).eq.';incl(')then
			! write(6,*)'<366>j_ninc',j_ninc
			nul0t=j_ninc-1
			! nu=j_nul(nul0)
			!	call j_command('sit()')
1 inprint=j_v(j_ivprintinput)
	!		write(6,*)'inprint',inprint
			call j_getinput('# ',inprint,nul0t)
			!	if(j_err)write(6,*)'errhere'
			if(j_err)return
			!		write(6,*)'<33309>',j_inp(1:j_linp),j_ninc,nul0t
			if(j_stop)return
			if(j_ninc.eq.nul0t)then
				write(6,*)'*j* this error',j_ninc,nul0t
				goto 900
			endif !if(j_ninc.eq.nul0t)   1153
			! write(6,*)'got:',inp(1:linp)
 
			!0140814 paluu melaopt:iin (tai muuhun pääohjelmaan)
			!if(j_inp(1:j_linp).eq.'return') return
			! write(6,*)'<379>got:',j_inp(1:j_linp)
			if(j_inp(1:j_linp).eq.'end')then
				j_stop=.true.
				return
			endif !if(j_inp(1:j_linp).eq.'end')   1162
			!  write(6,'(a)')'got:',inp(1:len_trim(inp))
			!call jcompil(inp(1:linp),o(ivcursor)%i,.true.,.false.,0,0)
			!20141203 oli: if(.not.err)call jcompil(inp(1:linp),ivcursor,.true.,.false.,0,0,matrix0,matrix0,matrix0)
			!if(.not.j_err)write(6,*)'<618',j_inp(1:j_linp)
			!	if(.not.j_err)call j_compiler(j_inp(1:j_linp),ivjcommand(recursion),.true.,0,0,j_matrix0,&
			!		j_matrix0,j_matrix0,.false.)
			!subroutine jcompil(input,          ivteku, oneline, newin, ivinl,ivoutl,matrix,localin,localout)
			if(j_err)goto 900
 
 
			!io=1
			!	j_njump=0
			!err=.false.
			!	write(6,*)'hep2',err
			call dotrans(ivjcommand(j_recursion),1)
			!	write(6,*)'{{',2
			! write(6,*)'<395>j_ninc,nul',j_ninc,j_nul(1:j_ninc)
			! write(6,*)'<397>icurl',j_icurl(j_ninc)
			!write(6,*)'<397>j_incline(j_ninc),j_o(-nu)%i(0))',j_incline(j_ninc),j_o(-nu)%i(0)
 
			! 	if(j_incline(j_ninc).gt.j_o(-nu)%i(0))goto 90
 
			if(j_err)goto 900
 
			!write(6,*)'hep3',j_err
 
			!20140812 J2.2
			! if(oinp) then
			! if(lineobuf(ivobuf(0)).ge.o(ivobuf(ivobuf(0)))%i(0)) return
 
			goto 1
 
		endif !if(commandline(1:6).eq.';incl(')   1141
	endif !if(lec.gt.7)   1140
	900 j_recursion=j_recursion-1
	if(j_err)write(6,*)'*error doing j_command:',commandline
 
	return
end subroutine j_command !recursive subroutine j_command(commandline,passoptions)
 
 
!20150202 getcommnadbuf
! subroutine j_getcommandbuf(ivcommandbuf)
! use j_globalsmod, only: j_nusedcom
! use j_globalsmod, only: j_maxcommandbuf
! use getmod, only: j_crash
! use j_globalsmod, only: j_commandbuf
! use getmod, only: j_deftext
! use j_globalsmod, only: j_usedcom
! use getmod, only: j_cleartext
 
! integer, intent(out) :: ivcommandbuf
 
! if(j_nusedcom>=j_maxcommandbuf) then
! write(6,*)'*j* too many command buffers'
! j_err=.true. !j_err = .true.
! return
! endif
 
! j_nusedcom=j_nusedcom+1
! if(j_commandbuf(j_nusedcom)==0) then
! !20150812(arg1<->arg2) oli: 		call deftext('commandbuffer%'//char(i0+48),0,15,200,commandbuf(usedcom(nusedcom)))
! call j_deftext(0,'commandbuffer%'//char(i0+48),15,200,j_commandbuf(j_usedcom(j_nusedcom)))
! endif
! ivcommandbuf = j_commandbuf(j_usedcom(j_nusedcom))
! call j_cleartext(ivcommandbuf)
! return
! end subroutine  j_getcommandbuf
 
 
! subroutine j_closecommandbuf(ivcommandbuf)
! use j_globalsmod, only: j_nusedcom
! use j_globalsmod, only: j_usedcom
 
! implicit none
! integer, intent(in) ::ivcommandbuf
 
! integer i_
 
! do i_=1,j_nusedcom
! if(ivcommandbuf.eq.j_usedcom(i_))then
! j_usedcom(i_)=j_usedcom(j_nusedcom)
! !nusedcom
! endif
! enddo
! return
! end subroutine j_closecommandbuf
 
 
! !20150202 gettitleopt
! subroutine j_gettitleopt(iob,iout)   !obsolete when using startfunction
! use j_omod, only: j_linkopt
! use j_globalsmod, only: j_mtitle
! use j_globalsmod, only: j_o
! use errmod, only: j_err
! use j_globalsmod, only: j_otype
! use j_globalsmod, only: j_IPCHAR
! use j_globalsmod, only: j_otitle
 
! integer, intent(in):: iob, iout
 
! if(j_linkoption(iob,io,j_mtitle)>0) then
! if(j_o(iob)%i(j_linkoption(iob,io,j_mtitle)).ne.1) then
! write(6,*)'title-> should have one argument'
! j_err=.true.
! return
! endif
! iarg = j_o(iob)%i(j_linkoption(iob,io,j_mtitle)+1)
! if(j_otype(iarg)==j_ipchar) then
! ! ivtitle(iout) = iarg
! ! elseif(otype(iarg)==ipcharv) then
! ! ial=o(iarg)%i(1)
! ! do i=1,namedv
! ! if(o(ivnames)%i(i)>=ial) then
! ! ivtitle(iout) = i
! ! exit
! ! endif
! ! enddo
! j_otitle(iout)=j_o(iarg)%i(3)
! else
! write(6,*)'title-> should have character argument'
! j_err=.true.
! return
! endif
! endif
! return
! end subroutine j_gettitleopt
 
!20150202 gettitle
! subroutine j_gettitle(iv,title,lentitle)  !get %%title into char. var title, le is the length
 
! integer, intent(in):: iv
! integer, intent(out):: lentitle
! character*(*), intent(out)::title
 
! lentitle = 0
! if(j_otitle(iv)>0) call j_getchar2(iv,title,lentitle)
! return
! end subroutine j_gettitle !subroutine j_gettitle(iv,title,lentitle)
 
 
 
function j_getobject(ivin,name,itype,useold,silent) ! %%object get old or new object, UUSI
	!                  %%object is  ready to use only for real variables or character constants
	! if the object exist and it is not of type real, it will be deleted
	! if name=' ' then get as object ivin
	! if name & ivin=0 then get old or new with the name
	! if name & ivin>0 then get variable which has prefix name of ivin and then come the name
	!if ivin=0 and name starts wit ' the character contant is created even ityp is j_ipreal
 
	integer, intent(in):: ivin, itype
	!	integer, intent(out):: ivout
	character*(*), intent(in):: name
	logical,optional,intent(in)::useold,silent
	character(len=j_lenobject) name2
	logical old,del
	del=.true.
	if(present(useold))del=.not.useold
 
	!write(6,*)'getobj:ivin,name,type',ivin,name,itype
	!write(6,*)'>23getob', ivin,name,itype
	!logical isletter
	!	write(6,*)'>641/'//name//'/',ivin,itype,j_ipchar,j_otype(ivin),name.ne.' '
	if(j_err)then
		write(6,*)'*j* getobject called with j_err , ivin=',ivin,' name was ',name
		return
	endif !if(j_err)   1329
	goto 2
99 if(.not.present(silent))write(6,*)'*illegal object name /'//name//'/'
	j_err=.true. ;return
 
2	lena=j_lentrim(name)
	if(lena.eq.0)then
		if(ivin.le.0)then
			write(6,*)'*j* getobject,both name and ivin are empty'
			j_err=.true. ;return
		endif !if(ivin.le.0)   1339
		j_getobject=ivin
		!	write(6,*)'ivinhere',ivin,itype,del
		if(j_otype(ivin).ne.itype.or.del)then
			call j_del(ivin)
			j_otype(j_getobject)=itype
		endif !if(j_otype(ivin).ne.itype.or.del)   1345
		return
	endif !if(lena.eq.0)   1338
 
	!	write(6,*)'lena ',lena
	ihaka2=0
 
	if(name(1:1).ne.'@')then
		li1=1
	else
		li1=2
	end if !if(name(1:1).ne.'@')   1355
 
	if(name(1:1).ne."'")then
		ihaka1=index(name,'[')
		!	if(ihaka1.eq.1.and.iv.eq.0)write(6,*)'haka1'
		if(ihaka1.eq.1.and.ivin.eq.0)goto 99
		if(ihaka1.gt.0)ihaka2=j_nextlim(name,ihaka1+1,lena,']')
		if(ihaka1.gt.0)then
			!	if(ihaka2.gt.lena)write(6,*)'haka2'
			if(ihaka2.gt.lena)goto 99
			istart=li1
			iend=ihaka1-1
		else
			iend=lena
		endif !if(ihaka1.gt.0)   1366
		istart=li1
		!if(ivin.le.0.and.name(li1:li1).ge.'0'.and.name(li1:li1).le.'9')write(6,*)'haka3'
		if(ivin.le.0.and.name(li1:li1).ge.'0'.and.name(li1:li1).le.'9')goto 99
		!	write(6,*)'<44istart,iend',istart,iend
700	continue
		do i=istart,iend
			!		write(6,*)'iiii',i,name(i:i),ichar(name(i:i))
			if(ichar(name(i:i)).lt.35)goto 99 !35=#
			if(ichar(name(i:i)).ge.39.and.ichar(name(i:i)).le.47)goto 99
			if(ichar(name(i:i)).ge.58.and.ichar(name(i:i)).le.64)goto 99
			if(name(i:i).eq.']'.or.name(i:i).eq.'^')goto 99
			if(ichar(name(i:i)).eq.96)goto 99
			if(ichar(name(i:i)).eq.123)goto 99  !124=|
			if(ichar(name(i:i)).ge.125)goto 99
		enddo !i=istart,iend   1379
		! if(ihaka1.gt.0)then
		! ihaka1=0
		! istart=ihaka2+1
		! iend=lena
		! goto 700
		! endif !if(ihaka1.gt.0)   1510
 
	endif !if(name(1:1).ne."'")   1361
 
 
	le=lena-li1+1  !len(name(li1:))
 
	if(ivin.gt.0)then
		if(j_otype(ivin).eq.j_ipchar.and.itype.eq.j_ipchar)then
			!			write(6,*)'>33',ivin,itype
			call j_getchar(ivin,name2(2:),len1)
			!			write(6,*)name(2:1+len1)
			name2(1:1)="'"
			name2(len1+2:len1+2+le)=name(li1:le)
			le=len1+3+le
			name2(le:le)="'"
			!			write(6,*)'>33/',name2(1:le)
		elseif(itype.eq.j_ipchar)then !if(j_otype(ivin).eq.j_ipchar.and.itype.eq.j_ipchar)then
			call j_getline(j_ivnames,ivin,name2(2:),le2)
			if(j_err)return
			name2(1:1)="'"
			name2(le2+2:le2+2+le)=name(li1:le)
			le=le2+2+le
			name2(le:le)="'"
		else !if(j_otype(ivin).eq.j_ipchar.and.itype.eq.j_ipchar)then
 
			call j_getline(j_ivnames,ivin,name2,le2)
			if(j_err)return
			!	 write(6,*)'name2',name2(1:le2),' le2',name,' le ',le
 
			name2(le2+1:le2+le)=name(li1:)
 
			le=le+le2
			!	write(6,*)'le,le2',le,le2,name2(1:le)
		endif !if(j_otype(ivin).eq.j_ipchar.and.itype.eq.j_ipchar)   1402
 
 
	else
		name2=name(li1:lena)
		le=lena-li1+1
	endif !if(ivin.gt.0)   1401
 
	ivout=j_object(name2(1:le))
	if(ivout.gt.0)then
		!write(6,*)'<33getobject ivout itype,j_otype(ivout),',ivout,itype,j_otype(ivout)
		if(j_otype(ivout).ne.itype.or.del)then
			call j_del(ivout)
			j_otype(ivout)=itype
		endif !if(j_otype(ivout).ne.itype.or.del)   1439
		j_getobject=ivout
		return
	endif !if(ivout.gt.0)   1437
 
 
	if(j_namedv.ge.j_mxnamedv) then
		!21040522 oli: stop 'increase max number of named vars in j.par'
		write(6,*)'****increase max number of named vars ',j_mxnamedv,' in j.par and restart j'
		j_err=.true.
		return
	endif !if(j_namedv.ge.j_mxnamedv)   1448
	j_namedv=j_namedv+1
	ivout=j_namedv
	!	if(ivin.le.0)then
	! if(.not.(j_isletter(name(li1:li1)).or.name(li1:li1).eq."'"))then
	! write(6,*)'**(2)illegal variable name (2): ',name(li1:);j_err=.true.
 
	! j_namedv=j_namedv-1
	! return
	! end if !if(.not.(j_isletter(name(li1:li1)).or.name(li1:li1).eq."'"   1576
	!20140522 tarkistetaan, ettei mja == jokin funktio
	i_=j_function_index(name2(1:le))   ! isin(name(li1:),j_functions,j_nfunctions)
	if(i_>0) then
		write(6,*)'**error: ',name2(1:le),' is reserved for function'
		j_err=.true.
		j_namedv=j_namedv-1
		return
	endif !if(i_>0)   1465
	if(itype.eq.j_ipchar.and.name2(1:1).ne."'")then
		call j_puttext(j_ivnames,"'"//name2(l:le)//"'") ! name can already contain ' or cannot contain it
		if(j_err)then
			write(6,*)'*detected in j_getobject'
			return
		endif !if(j_err)   1473
	else !if(itype.eq.j_ipchar.and.name(li1:li1).ne."'")then
		call j_puttext(j_ivnames,name2(1:le))
	endif !if(itype.eq.j_ipchar.and.name2(1:1).ne."'")   1471
 
	i_=j_function_index(name2(1:le))  ! isin(name2(1:le),j_functions,j_nfunctions)
	if(i_>0) then
		write(6,*)'**error: ',name2(1:le),' is reserved for function'
		j_err=.true.
		j_namedv=j_namedv-1
		return
	endif !if(i_>0)   1482
	! call j_puttext(j_ivnames,name2(1:le))
	! if(j_err)then
	! write(6,*)'*detected in j_getobject'
	! return
	! endif !if(j_err)   1666
	!	end if !if(ivin.le.0)   1614
	j_v(ivout)=j_0
	! else  if(j_otype(ivout).gt.j_ipreal.and..not.j_ischarconst(ivout))then !if(ivout.le.0)then
	! ! write(6,*)ivout, ' ivout'
	! ! call printname('fele ',ivout,' ll')
	! !20141120 if.. (oli call del(ivout))
	! !	write(6,*)'<570> j_otype(ivout)',j_otype(ivout)
	! call j_del(ivout);if(j_err)return
	! j_v(ivout)=j_0
	!	end if !if(ivout.le.0)   1603
	!	write(6,*)'getobject,itype,ivout ',itype,ivout
	j_otype(ivout)=itype
	j_getobject=ivout
	return
end function j_getobject !subroutine j_getobject(ivin,name,itype,ivout)
 
subroutine j_getobjectnam(name,itype,ivout) ! %%object get old or new object, UUSI
	!                  %%object is  ready to use only for real variables or character constants
	! if the object exist and it is not of type real, it will be deleted
	! if name=' ' then get as object ivin
	! if name & ivin=0 then get old or new with the name
	! if name & ivin>0 then get variable which has prefix name of ivin and then come the name
	!if ivin=0 and name starts wit  ' the character contant is created even ityp is j_ipreal
	integer,intent(in)::itype
	integer, intent(out):: ivout
	character*(*), intent(in):: name
	logical ::ishipsu
	!name contains "'" for ipchar or not
 
	if(index(name,':').gt.0.and.name(1:1).ne."'")then
		write(6,*)'object name cannot contain : as in ',name
		j_err=.true.
		return
	endif !if(index(name,':').gt.0.and.name(1:1).ne."'")   1522
	if(index(name,"'").gt.0.and.name(1:1).ne."'")then
		write(6,*)"object name cannot contain ' as in ",name
		j_err=.true.
		return
	endif !if(index(name,"'").gt.0.and.name(1:1).ne."'")   1527
	ishipsu=name(1:1).eq."'"
	if(itype.eq.j_ipchar.and.ishipsu)then
		ivout=j_line(j_ivnames,name)
	elseif(itype.eq.j_ipchar)then
		ivout=j_line(j_ivnames,"'"//name//"'")
	endif !if(itype.eq.j_ipchar.and.ishipsu)   1533
	if(ivout.gt.0)return
 
	!write(6,*)'<522> ivout ',ivout
 
	!20140522 oli: if(namedv.ge.mxnamedv)stop 'increase max number of named vars in j.par'
	if(j_namedv.ge.j_mxnamedv) then
		!21040522 oli: stop 'increase max number of named vars in j.par'
		write(6,*)'****increase max number of named vars ',j_mxnamedv,' in j.par and restart j'
		j_err=.true.
		return
	endif !if(j_namedv.ge.j_mxnamedv)   1543
	j_namedv=j_namedv+1
	ivout=j_namedv
	le=len(name)
	if(itype.eq.j_ipchar)then
		if(ishipsu)then
			call j_puttext(j_ivnames,name)
			if(j_err)then
				write(6,*)'*detected in j_getobjenam'
				return
			endif !if(j_err)   1555
		else
			call j_puttext(j_ivnames,"'"//name//"'") !
		endif !if(ishipsu)   1553
	else
		i_=j_function_index(name)   ! isin(name(li1:),j_functions,j_nfunctions)
		if(i_>0) then
			write(6,*)'**error: ',name,' is reserved for function'
			j_err=.true.
			j_namedv=j_namedv-1
			return
		endif !if(i_>0)   1564
		do li1=1,le
			if(.not.(j_isletter(name(li1:li1)).or.name(li1:li1).eq."'"))then
				write(6,*)'*illegal symbol ',name(li1:li1),' in variable name (3): /',name//'/';j_err=.true.
 
				j_namedv=j_namedv-1
				return
			end if !if(.not.(j_isletter(name(li1:li1)).or.name(li1:li1).eq."'"   1571
		enddo !li1=1,le   1570
		call j_puttext(j_ivnames,name)
	endif !if(itype.eq.j_ipchar)   1552
 
	!20140522 tarkistetaan, ettei mja == jokin funktio
 
 
 
	j_v(ivout)=j_0
 
	j_otype(ivout)=itype
 
	return
end subroutine j_getobjectnam !subroutine j_getobject(ivin,name,itype,ivout)
 
subroutine j_getder(ivfunc,args,ders,nder,old)
	integer,intent(in)::ivfunc
	integer,intent(in)::nder
	integer,intent(in),dimension(nder) :: args
	integer,intent(out),dimension(nder) :: ders
	logical,intent(in),optional::old
	call j_getname(ivfunc)
	if(nder.gt.j_mxder)then
		j_mxder=j_mxder*2
		do iv=j_mxnamedv+j_mxtemporalv0+1,j_mxnamedv+j_mxtemporalv !%dervative
			deallocate(j_o(iv)%d)
			allocate(j_o(iv)%d(1:j_mxder))
		enddo !iv=j_mxnamedv+j_mxtemporalv0+1,j_mxnamedv+j_mxtemporalv   1601
 
 
	endif !if(nder.gt.j_mxder)   1599
	do ina=1,nder
		!	nteku=nteku+1
		call j_getname(-1,args(ina))
		!	call j_getline(j_ivnames,args(ina),j_tempchar2,le)
		ivo=j_object(j_oname(1:j_loname)//'\'//j_oname2(1:j_loname2))
		if(ivo.le.0)then
 
			if(present(old))then
				if(old)then
					write(6,*)j_oname(1:j_loname)//'\'//j_oname2(1:j_loname2),' not found'
					j_err=.true.;return
				endif !if(old)   1616
			endif !if(present(old))   1615
			ivo=j_getobject(0,j_oname(1:j_loname)//'\'//j_oname2(1:j_loname2),j_ipreal)
 
		endif !if(ivo.le.0)   1613
		ders(ina)=ivo
		!		call j_putoutput(ivo,ivinl,ivoutl)
	enddo !ina=1,nder   1608
 
 
 
end subroutine !subroutine j_getder(args,ders,nder,old)
! object handling
! olist(
subroutine j_getobject2(iv,itype)  !%%object just get new object with existing name
	! if the object is not of real type, it is deleted
	!module vmod
	!end module vmod
 
	!module typemod
	!ipobj , & first objec ????
 
	!end module
 
	integer, intent(in):: iv, itype
 
	!20141219 oli: if(otype(iv).ne.0)call del(iv)
	if(j_otype(iv).ne.j_ipreal)call j_del(iv)
	j_otype(iv)=itype
	j_v(iv)=0.
	return
end subroutine  j_getobject2 !subroutine j_getobject2(iv,itype)
 
 
recursive subroutine j_del(iv) ! %%object delete object , subobects are also deleted,
	! if the object is of o object type then the oX_del is called
	! the onX_del needs to delete only subobjects
 
	!end module
 
	! use getmod, only: del
	integer, intent(in):: iv
 
	logical bfexist_
	if(iv.le.0)return !delete can be called for nonexistent subobjects
	if(j_otype(iv).eq.j_ipreal)return
	if(iv.le.j_locked)then
		call j_printname('*cannot delete locked object ',iv,' ')
		j_err=.true.
	endif !if(iv.le.j_locked)   1665
 
	select case (j_otype(iv))
 
	case (j_ipchar) !select case (j_otype(iv))
	! if(j_o(iv)%i(3).ne.0)then    !charcter variable
	! j_iounit=j_o(j_o(iv)%i(3))%i(4)
	! else !if(j_o(iv)%i(3).ne.0)then
	! j_iounit=j_o(iv)%i(4)
	if(j_o(iv)%i(3).eq.0)then
 
		call j_getname(iv)
 
		write(6,*)'**cannot delete a character constant',j_oname(1:j_loname)
		j_err=.true.
		return
	endif !if(j_o(iv)%i(3).eq.0)   1677
	! endif !if(j_o(iv)%i
 
	if(j_iounit(iv).ne.0)then !if(j_o(iv)%i(3).eq.0)then
		!write(6,*)'<77>',j_o(iv)%i
 
		call j_printname('**character variable ',iv, ' is associated with file, it cannot be deleted before closing the file')
		call j_getname(iv)
		write(6,*)'name',j_oname(1:j_loname)
		do iiv=1,j_mxnamedv
			if(j_otype(iiv).eq.j_ipchar)then
				call j_getname(iiv)
				write(6,*)'iiv ',j_oname(1:j_loname),j_o(iiv)%i(3:4)
 
			endif !if(j_otype(iiv).eq.j_ipchar)   1694
 
		enddo !iiv=1,j_mxnamedv   1693
 
 
 
		j_err=.true.
		return
	endif !if(j_iounit(iv).ne.0)   1687
 
	case (j_ipdata) !select case (j_otype(iv))
	!	write(6,*)j_o(iv)%i(1:14)
	! call j_getname(iv,j_o(iv)%i(1))
	! write(6,*)'del,',iv,j_o(iv)%i(1),j_oname(1:j_loname),j_oname2(1:j_loname2)
	call j_del(j_o(iv)%i(1)) !matrix
	!	call j_getname(j_o(iv)%i(2),j_o(iv)%i(3),j_o(iv)%i(8))
	!	write(6,*)j_oname(1:j_loname),j_oname2(1:j_loname2),j_oname3(1:j_loname3)
	call j_del(j_o(iv)%i(2)) !keep
	!call j_del(j_o(iv)%i(3)) !cases
	!call j_del(j_o(iv)%i(8)) !vars
	! call j_del(j_o(iv)%i(9)) ! subdata
	! call j_del(j_o(iv)%i(11)) !updata
	!case (j_iptrans)
	! call j_del(j_o(iv)%i2(1)) ! %input
	! call j_del(j_o(iv)%i2(1)) ! %input
	! call j_del(j_o(iv)%i2(2)) ! %output
	! call j_del(j_o(iv)%i2(3)) ! datavars
	! call j_del(j_o(iv)%i2(4)) ! nodes
	! call j_del(j_o(iv)%i2(5)) ! periods
	! call j_del(j_o(iv)%i2(7)) ! treevars
	! call j_del(j_o(iv)%i2(8)) ! plotvars
	! call j_del(j_o(iv)%i2(11)) ! %output
	case (j_ipregr) !select case (j_otype(iv))
	ncoef=j_o(iv)%i(0)
	ivvar=j_o(iv)%i(ncoef+2)
	ivcor=j_o(iv)%i(ncoef+3)
	if(ivvar.ne.0)call j_del(ivvar)
	if(ivcor.ne.0)call j_del(ivcor)
	end select !select case (j_otype(iv))
	if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nobjecttypes1)then
		call o1_del(iv,j_otype(iv)-j_nobjecttypes)
	elseif(j_otype(iv).gt.j_nobjecttypes1.and.j_otype(iv).le.j_nobjecttypes2)then !if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nobjecttypes1)then
		call o2_del(iv,j_otype(iv)-j_nobjecttypes1)
	elseif(j_otype(iv).gt.j_nobjecttypes2.and.j_otype(iv).le.j_nobjecttypes3)then !if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nobjecttypes1)then
		call o3_del(iv,j_otype(iv)-j_nobjecttypes2)
	elseif(j_otype(iv).gt.j_nobjecttypes)then !if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nobjecttypes1)then
		write(6,*)'*j* illegal object type in delete ',j_otype(iv),' max is ',j_nobjecttypes
		j_err=.true.
		return
	endif !if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nob   1737
 
	!write(6,*)'del,iv,(iv)',iv,iv
	if(allocated(j_o(iv)%r))then;deallocate(j_o(iv)%r);end if
	if(allocated(j_o(iv)%i))then; deallocate(j_o(iv)%i);end if
	if(allocated(j_o(iv)%i2))then;deallocate(j_o(iv)%i2);end if
	!	if(allocated(j_o(iv)%i8))then;deallocate(j_o(iv)%i8);end if
	if(allocated(j_o(iv)%d))then;deallocate(j_o(iv)%d);end if
	if(allocated(j_o(iv)%ch))then;deallocate(j_o(iv)%ch);;end if
	if(allocated(j_o(iv)%txt))then;deallocate(j_o(iv)%txt);;end if
	j_otype(iv)=j_ipreal
	!if(iv.le.j_mxnamedv)j_otitle(iv)=0
	! write(6,*)'deleteob','no',no,'iv',iv,'ioreal',ioreal
	!write(6,*)'aft',(olist(jj),jj=1,15)
	return
end subroutine j_del !recursive subroutine j_del(iv)
 
recursive subroutine j_del0(iv) !delete %%object , subobects are not deleted
 
	integer, intent(in):: iv
	if(j_otype(iv).eq.j_ipreal)return
	if(j_otype(iv).eq.j_ipchar)then
		if(j_o(iv)%i(3).eq.0)then   !character constants cannot be deleted
			j_err=.true.
			write(6,*)'**trying to use character constant as output',iv
			return
		endif !if(j_o(iv)%i(3).eq.0)   1769
	endif !if(j_otype(iv).eq.j_ipchar)   1768
 
	if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nobjecttypes1)then
		call o1_del(iv,j_otype(iv)-j_nobjecttypes)
	elseif(j_otype(iv).gt.j_nobjecttypes1.and.j_otype(iv).le.j_nobjecttypes2)then !if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nobjecttypes1)then
		call o2_del(iv,j_otype(iv)-j_nobjecttypes1)
	elseif(j_otype(iv).gt.j_nobjecttypes2.and.j_otype(iv).le.j_nobjecttypes3)then !if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nobjecttypes1)then
		call o3_del(iv,j_otype(iv)-j_nobjecttypes2)
	elseif(j_otype(iv).gt.j_nobjecttypes)then !if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nobjecttypes1)then
		write(6,*)'*j* illegal object type in del0 ',j_otype(iv),' max is ',j_nobjecttypes
		j_err=.true.
		return
	endif !if(j_otype(iv).gt.j_nobjecttypes.and. j_otype(iv).le.j_nob   1776
 
	!write(6,*)'del,iv,(iv)',iv,iv
	if(allocated(j_o(iv)%r))then;deallocate(j_o(iv)%r);end if
	if(allocated(j_o(iv)%i))then; deallocate(j_o(iv)%i);end if
	if(allocated(j_o(iv)%i2))then;deallocate(j_o(iv)%i2);end if
	if(allocated(j_o(iv)%d))then;deallocate(j_o(iv)%d);end if
	if(allocated(j_o(iv)%ch))then;deallocate(j_o(iv)%ch);;end if
 
	j_otype(iv)=j_ipreal
	! write(6,*)'deleteob','no',no,'iv',iv,'ioreal',ioreal
	!write(6,*)'aft',(olist(jj),jj=1,15)
	return
end subroutine j_del0 !recursive subroutine j_del0(iv)
 
 
 
subroutine j_fromutf8(line)
	character*(*),intent(inout):: line
	le=len(line)
	do i=1,j_nutf8
		do j=1,le
			if(line(j:j).eq.j_utf8(i))line(j:j)=j_ocode(i)
		end do !j=1,le   1807
	enddo !i=1,j_nutf8   1806
end subroutine !subroutine j_fromutf8(line)
 
subroutine j_toutf8(line,le)
	character*(*),intent(inout):: line
	integer,intent(inout)::le
	!	le=len(line)
	ial=1
	lop=le
10 	continue
	do i=ial,lop
 
		if(line(i:i).eq.'\')then
			!	write(6,*)ial,lop,line(ial:lop)
			if(line(i+1:i+1).eq.'#')then
				if(line(i+2:i+2).eq.'a')then
					line(i+1:i+1)=char(164)
				elseif(line(i+2:i+2).eq.'A')then
					line(i+1:i+1)=char(132)
				elseif(line(i+2:i+2).eq.'o')then
					line(i+1:i+1)=char(182)
				elseif(line(i+2:i+2).eq.'O')then
					line(i+1:i+1)=char(150)
 
 
				endif !if(line(i+2:i+2).eq.'a')   1825
				ial=i+2
				line(i+2:lop-1)=line(i+3:lop)
				lop=lop-1
				goto 100
			elseif(line(i+1:i+6).eq.'greek_')then
				!	subroutine j_repl(jono1,i1,i2,linp,jono2,le2)
				le2=20
				call j_repl(line,i,i+7,lop,'{/Symbol:Plain=12 '//line(i+7:i+7)//'}',le2)
 
				ial=i+8
				goto 10
				!				{/Symbol:Plain=12 l
			endif !if(line(i+1:i+1).eq.'#')   1824
		endif !if(line(i:i).eq.'\')   1822
	end do !i=ial,lop   1820
	le=lop
	return
100		line(i:i)=char(195)
	goto 10
 
end subroutine !subroutine j_toutf8(line)
 
!20150812(arg1<->arg2) oli: subroutine deftext(name,iv,lines,leng,ivout) ! define a text object
function j_deftext(iv,name,lines,leng) ! define a %%text %%object
	! note h(0) is number of lines but there is one more element in h
 
	integer, intent(in):: iv, lines,leng
	!	integer, intent(out):: ivout
	character*(*), intent(in):: name
	! write(6,*)'deftext',iv,name,lines,
	!20150812(arg1<->arg2) oli: 	call getv(name,iv,iptext,ivout);if(j_err)return
	j_deftext=j_getobject(iv,name,j_iptext);if(j_err)return
	!	write(6,*)'ccd',name,lines,leng,j_deftext,ior
	allocate( j_o(j_deftext)%i(0:lines+1),j_o(j_deftext)%ch(1:leng))
	! o(j_deftext)%i(-1)=lines+1;
	j_o(j_deftext)%i(0)=0;j_o(j_deftext)%i(1)=1
	return
end function j_deftext !
 
 
!20150812(arg1<->arg2) oli: subroutine deftext2(name,iv,lines,leng,lines2,j_deftext) ! as deftext, but h2 allocated for adresses
function j_deftext2(iv,name,lines,leng,lines2) ! as deftext, but h2 allocated for adresses
	! lines2 linenumbers for labels
	! used by puttext2
 
	!module vmod
	!end mod
 
	!module typemod
	!ipobj , & first objec ????
 
	!end module
 
	integer, intent(in):: iv, lines,leng,lines2
	!	integer, intent(out):: ivout
	character*(*), intent(in):: name
 
	!20150812(arg1<->arg2) oli: 	call getv(name,iv,iptext,ivout)
	j_deftext2=j_getobject(iv,name,j_iptext)
	!20140522 virheenkäsittely
	if(j_err) return
 
	! write(6,*)name,lines,leng,j_deftext2,ior,
	allocate( j_o(j_deftext2)%i(0:lines+1),j_o(j_deftext2)%ch(1:leng))
	allocate( j_o(j_deftext2)%i2(0:lines2))
	! o()%i(-1)=lines+1;
	j_o(j_deftext2)%i(0)=0;j_o(j_deftext2)%i(1)=1;j_o(j_deftext2)%i2(0)=0
	!write(6,*)'otype',otype()
	return
end function j_deftext2 !subroutine j_deftext2(iv,name,lines,leng,lines2,ivout)
 
integer*8 function  j_i4i8(i4)
	integer,dimension(2),intent(in)::i4
	integer*8::ii8
	integer,dimension(2)::ii4
	equivalence(ii4,ii8)
	ii4=i4
	j_i4i8=ii8
end function
 
subroutine j_i8i4(i8,i4)
	integer,dimension(2),intent(out)::i4
	integer*8,intent(in)::i8
	integer*8::ii8
	integer,dimension(2)::ii4
	equivalence(ii4,ii8)
	ii8=i8
	i4=ii4
endsubroutine
 
!20150812(arg1<->arg2) oli: subroutine defmatrix(name,iv,ndim1,ndim2,itype,ivout) ! defines a matrix object
integer function j_defmatrix(iv,name,ndim1,ndim2,itype,single,nod,rowtot) ! defines a matrix object %%matrix
	integer, intent(in):: iv,itype
	integer,intent(in) ::ndim1,ndim2
	character*(*), intent(in):: name
	logical,intent(in),optional::single
	logical,intent(in),optional::nod
	integer,intent(in),optional::rowtot
	integer*8::ndim18,ndim28,rowtot8
	logical sing,no
	ndim18=ndim1
	ndim28=ndim2
 
	!	write(6,*)'name',name
	!	write(6,*)'iv,name,ndim18,ndim28,itype,single ',iv,name,ndim18,ndim28,itype,single
	sing=.false.
	if(present(single))sing=single
	no=.false.
	if(present(nod))no=nod
	rowtot8=ndim18
	if(present(rowtot))rowtot8=rowtot
	!	write(6,*)'rowtot',rowtot8
	j_defmatrix=j_defmatrix8(iv,name,ndim18,ndim28,itype,single=sing,nod=no,rowtot=rowtot8)
 
end function
 
integer function j_defmatrix8(iv,name,ndim1,ndim2,itype,single,nod,rowtot) ! defines a matrix object %%matrix
	integer, intent(in):: iv,itype
	integer*8,intent(in) ::ndim1,ndim2
	integer*8:: nel
	character*(*), intent(in):: name
	logical,intent(in),optional::single
	logical,intent(in),optional::nod
	integer*8,intent(in),optional::rowtot
	integer*8 ::rowtot8
	!	logical expand !can the matrix be expanded
	logical sing,no
	!	write(6,*)'iv,name,ndim18,ndim28,itype,single ',iv,name,ndim18,ndim28,itype,single
	!20141113 if(j_err)
	if(j_err) then
		write(6,*)'*j* defmatrix: j_err=true at start '
		j_err=.true.
		return
	endif !if(j_err)   1965
	no=.false.
	if(present(nod))no=nod
	sing=.false.
	if(present(single))sing=single
	rowtot8=ndim1
	!write(6,*)'defmatr8',iv,name,ndim1,ndim2
	if(present(rowtot))rowtot8=rowtot
	!	write(6,*)'rowtot',rowtot8
	!	write(6,*)'matreg',matreg,'sing',sing
	ivout=j_getobject(iv,name,j_ipmatrix) !call getv2(ipmatrix,iv,ior)
	!	write(6,*)'hereivout,',ivout,j_otype(ivout),j_ipmatrix
	if(j_err)return
	if(ndim1.le.0.or.ndim2.le.0)then
		call j_getname(ivout)
		write(6,*)'*j* defmatrix: illegal dimensions: ',ndim1,ndim2,' for ',j_oname(1:j_loname)
		j_err=.true.
		return
	endif !if(ndim1.le.0.or.ndim2.le.0)   1982
	!if(ndim1.eq.1.and.ndim2.eq.1)call j_getobject(iv,name,j_ipreal,ivout)
	if(itype.eq.j_matdiag.and.ndim1.ne.ndim2)then
		write(6,*)'*j* diagonal-> and nrows ',ndim1,' .ne. ncols ',ndim2
		j_err=.true.;return
 
	endif !if(itype.eq.j_matdiag.and.ndim1.ne.ndim2)   1989
	!20150812(arg1<->arg2) oli: 	call getv(name,iv,ipmatrix,ivout) !call getv2(ipmatrix,iv,ior)
	! if(sing)then
	! call j_getobject(iv,name,j_ipmatrix0,ivout)
	! else !if(sing)then
	!write(6,*)'<34defm,j_namedv',j_namedv
	!	write(6,*)'defm ',iv,name,'_je_err ',j_err
 
	!	write(6,*)'deaft ',iv,name,'_je_err ',j_err,'ivout',ivout
	!	write(6,*)'defm ',iv,name,'_je_err ',j_err,ivout
	!write(6,*)'<34defmivout,namedv',ivout,j_namedv
	!endif !if(sing)then
	!20140522 virheenkäsittely
	!20141113 j_err täydennys
	!write(6,*)'<556',allocated(j_o(ivout)%r),allocated(j_o(ivout)%i),allocated(j_o(ivout)%i2),allocated(j_o(ivout)%ch)
	if(j_err) then
		write(6,*) '*j* defmatrix j_err=true in getv'
		j_err=.true.
		return
	endif !if(j_err)   2008
	if(sing)j_otype(ivout)=j_ipmatrixs
	! write(6,*)name,lines,leng,ivout,ior
	!ndim1,ndim2,ndim1*ndim2
	allocate( j_o(ivout)%i(1:21))  !+6 (20 21=rowtot
	!	write(6,*)'here'
	!14 - nrows and ncols , nel
	!	allocate( j_o(ivout)%i8(1:13))
	j_o(ivout)%i=0
	! i(6) the number of rows in the intial part
	! i(7) first row in the initial part
	! i(8) last row in intial part , note there may be an unused are
	! i(9) the row which is stored after i(10)
	! i(11) the row which is stored after i(12)
 
	!	j_o(ivout)%i=0
	nel=ndim1*ndim2
	j_o(ivout)%i(1)=ndim1;   j_o(ivout)%i(2)=ndim2;j_o(ivout)%i(3)=nel
 
	j_o(ivout)%i(4)=itype
 
	!	write(6,*)'hui',ndim1,ndim2,nel
	!subroutine j_i8i4(i8,i4)
	! 	integer*8 function  j_i4i8(i4)
	call j_i8i4(ndim1,j_o(ivout)%i(14:15))
	call j_i8i4(ndim2,j_o(ivout)%i(16:17))
	call j_i8i4(nel,j_o(ivout)%i(18:19))
	call j_i8i4(rowtot8,j_o(ivout)%i(20:21))
	!write(6,*)'hool nod',no
	!	if(sing)j_o(ivout)%i8(13)=1
	! negative itype the unit number associate with the direct access file storing the data
	! iexpand=0
	! if(expand)iexpand=1
	! j_o(ivout)%i8(5)=iexpand REMOVED
	! if(sing)then
	! j_otype(ivout)=j_ipmatrix0
	! j_o(ivout)%i8(13)=1 !obsolte may be deleted
	! endif !if(sing)then
	if(.not.no)then
		!	write(6,*)'itype.eq.j_matreg.or.itype.eq.j_matdiag.or.itype.eq.j_matfreq'
		!	write(6,*)itype,j_matreg,itype,j_matdiag,itype,j_matfreq
		if(itype.eq.j_matreg.or.itype.eq.j_matdiag.or.itype.eq.j_matfreq)then
			if(sing)then
				allocate( j_o(ivout)%r(1:rowtot8*ndim2))
				!20140416 lisätty alustus
				j_o(ivout)%r=0.
			else !if(sing)then
				!			write(6,*)'ndim1*ndim2',ndim1*ndim2
				allocate( j_o(ivout)%d(1:rowtot8*ndim2))
				j_o(ivout)%d=j_0
			endif !if(sing)   2054
			!write(6,*)'<5477',ndim1,ndim2,ivout,j_o(ivout)%d
		else if(itype.eq.j_matclass)then !if(itype.eq.j_matreg.or.itype.eq.j_matdiag)then
			! if(sing)then
			! allocate( j_o(ivout)%r(1:ndim1*ndim2+4))  !xmin,dx,zmin,dz
			! j_o(ivout)%r=0.
 
			! else !if(sing)then
			allocate( j_o(ivout)%d(1:ndim1*ndim2+4))  !xmin,dx,zmin,dz
			j_o(ivout)%d=0.
			!	endif !if(sing)then
			allocate( j_o(ivout)%i2(1:3))
		else if (itype.gt.0)then !if(itype.eq.j_matreg.or.itype.eq.j_matdiag)then
			write(6,*)'**not yet other matrix types,was',itype
			j_err=.true.
			!elseif (itype.lt.0)then
			!allocate( j_o(ivout)%r(1:2*ndim2))  !two rows !disk
			!	j_o(ivout)%i8(10)=0
 
			!	j_o(ivout)%i(6)=0
			!	j_o(ivout)%i(7)=0
			!	j_o(ivout)%i(8)=0
			!	j_o(ivout)%i(9)=0
			!	j_o(ivout)%i(6)=0
 
			!(6) the number of rows in the intial part
			! i(7) first row in the initial part
			! i(8) last row in intial part , note there may be an unused are
			! i(9) the row which is stored after i(10)
			! i(11) the row which is stored after i(12)
 
		end if !if(itype.eq.j_matreg.or.itype.eq.j_matdiag.or.itype.eq.j_m   2053
	endif !if(.not.no)   2050
	j_defmatrix8=ivout
	!write(6,*)'hdhdhd',j_defmatrix8,j_otype(ivout)
	return
end function j_defmatrix8 !
 
! logical function j_expandable(iv)  ! %%matrix is matrix expandable
! if(j_otype(iv).ne.j_ipmatrix)then
! call j_printname('*j* expandable: ',iv, 'not a matrix')
! j_expandable=.false.
! j_err=.true.
! else !if(j_otype(iv).ne.j_ipmatrix)then
! j_expandable=j_o(iv)%i(5).ne.0
! endif !if(j_otype(iv).ne.j_ipmatrix)then
! return
! end function !logical function j_expandable(iv)
 
 
 
!20150812(arg1<->arg2) oli: subroutine defdata(name,iv,ivmat,ivkeep,ivcases,ivprolog,ivmaketrans,ivtrans,&
!subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivprolog,ivmaketrans,ivtrans,& ! %%data
!		ivepilog,ivvars,ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum) !%%data
subroutine j_defdata(iv,ivmat,ivkeep,ivcases) !%%data
	!module vmod
 
	integer, intent(in):: iv,ivmat,ivkeep
	!integer, intent(in):: ivsub,ivnobsw,ivup,ivobsw,ivnobswcum
	integer, intent(in),optional:: ivcases
	if(j_otype(iv).ne.j_ipreal)call j_del0(iv)  !this should not happemn
	j_otype(iv)=j_ipdata
	j_v(iv)=0.
 
	allocate( j_o(iv)%i(1:14))
	j_o(iv)%i=0
	j_o(iv)%i(1)=ivmat;j_o(iv)%i(2)=ivkeep
 
	ivobs=j_getobject(iv,'%obs',j_ipreal)
	j_o(iv)%i(6)=ivobs
	!	j_o(iv)%i(7)=ivobsw
	!	if(ivup.eq.0)j_o(iv)%i(7)=ivobs
	!	j_o(iv)%i(8)=ivnobswcum
	ivvars=j_deflistobject(iv,'%vars',ivin=ivkeep)
	ivvars2=j_deflistobject(iv,'%vars2',ivin=ivkeep)
	!	j_o(iv)%i(14)=j_o(ivmat)%i(1)  !number of observa
	!j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	!	j_o(iv)%i(5)=ivup
	!i(14)
	!i(12)=level
	!i(13) index of nobsw variable
	j_o(iv)%i(11)=ivvars
	j_o(iv)%i(14)=ivvars2
 
	!	 %i(11) is %vars getting upper level variables fro
	if(present(ivcases))j_o(iv)%i(10)=ivcases
	! j_o(iv)%i(9)=ivsub;j_o(iv)%i(10)=ivnobsw;j_o(iv)%i(11)=ivup
	! j_o(iv)%i(12)=ivobs;j_o(iv)%i(13)=ivobsw;j_o(iv)%i(14)=ivnobswcum
	! j_o(iv)%i(15)=0
	!this reserves place for the maximum number of subobservations for one observation
 
	!	if(ivobsw.le.0)j_o(iv)%i(7)=ivobs
	!write(6,*)'<947>,',j_o(iv)%i(1:14)
	return
end subroutine j_defdata !subroutine j_defdata(iv,ivmat,ivkeep,ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum)
 
!the following functions can be used to access subobjects  of %%data object
! integer function j_datamatrix(iv)  ! %%data
! integer,intent(in) ::iv
! j_datamatrix=	j_o(iv)%i(1)
! return
! end function !integer function j_datamatrix(iv)
 
! integer function j_datakeep(iv)  ! %%data
! integer,intent(in) ::iv
! j_datakeep=	j_o(iv)%i(2)
! return
! end function !integer function j_datakeep(iv)
! integer function j_dataprolog(iv)  ! %%data
! integer,intent(in) ::iv
! j_dataprolog=	j_o(iv)%i(4)
! return
! end function !integer function j_dataprolog(iv)
! integer function j_datamaketrans(iv)  ! %%data
! integer,intent(in) ::iv
! j_datamaketrans=	j_o(iv)%i(5)
! return
! end function !integer function j_datamaketrans(iv)
! integer function j_datatrans(iv)  ! %%data
! integer,intent(in) ::iv
! j_datatrans=	j_o(iv)%i(6)
! return
! end function !integer function j_datatrans(iv)
! integer function j_dataepilog(iv)  ! %%data
! integer,intent(in) ::iv
! j_dataepilog=	j_o(iv)%i(7)
! return
! end function !integer function j_dataepilog(iv)
! integer function j_datavars(iv)  ! %%data
! integer,intent(in) ::iv
! j_datavars=	j_o(iv)%i(8)
! return
! end function !integer function j_datavars(iv)
integer function j_datasub(iv)  ! %%data
	integer,intent(in) ::iv
	j_datasub=	j_o(iv)%i(3)
	return
end function !integer function j_datasub(iv)
integer function j_datanobsw(iv)  ! %%data
	integer,intent(in) ::iv
	j_datanobsw=	j_o(iv)%i(4)
	return
end function !integer function j_datanobsw(iv)
 
integer function j_dataup(iv)  ! %%data
	integer,intent(in) ::iv
	j_dataup=j_o(iv)%i(5)
	return
end function !integer function j_dataup(iv)
integer*8 function j_nrows(iv)
	integer,intent(in)::iv
	if(j_otype(iv).eq.j_ipmatrix)then
		j_nrows=j_i4i8(j_o(iv)%i(14:15))
	elseif(j_otype(iv).eq.j_ipreal)then
		j_nrows=j_18
	else
		call j_getname(iv)
		write(6,*)j_oname(1:j_loname),' is not MATRIX or REAL'
		j_err=.true.;return
	endif !if(j_otype(iv).eq.j_ipmatrix)   2213
 
end function
 
integer*8 function j_nrowtot(iv)
	integer,intent(in)::iv
	if(j_otype(iv).eq.j_ipmatrix)then
		j_nrowtot=j_i4i8(j_o(iv)%i(20:21))
	else
		call j_getname(iv)
		write(6,*)j_oname(1:j_loname),' is not MATRIX or REAL'
		j_err=.true.;return
	endif !if(j_otype(iv).eq.j_ipmatrix)   2227
 
end function
 
integer*8 function j_ncols(iv)
	integer,intent(in)::iv
	if(j_otype(iv).eq.j_ipmatrix)then
		j_ncols=j_i4i8(j_o(iv)%i(16:17))
	elseif(j_otype(iv).eq.j_ipreal)then
		j_ncols=j_18
	else
		call j_getname(iv)
		write(6,*)j_oname(1:j_loname),' is not MATRIX or REAL'
		j_err=.true.;return
	endif !if(j_otype(iv).eq.j_ipmatrix)   2239
 
end function
 
integer*8 function j_nelem(iv)
	integer,intent(in)::iv
	if(j_otype(iv).eq.j_ipmatrix)then
		j_nelem=j_i4i8(j_o(iv)%i(18:19))
	elseif(j_otype(iv).eq.j_ipreal)then
		j_nelem=j_18
	else
		call j_getname(iv)
		write(6,*)j_oname(1:j_loname),' is not MATRIX or REAL'
		j_err=.true.;return
	endif !if(j_otype(iv).eq.j_ipmatrix)   2253
 
end function
 
integer function j_dataobs(iv)  ! %%data
	integer,intent(in) ::iv
	j_dataobs=j_o(iv)%i(6)
	return
end function !integer function j_dataobs(iv)
 
integer function j_dataobsw(iv)  ! %%data
	integer,intent(in) ::iv
	j_dataobsw=j_o(iv)%i(7)
	return
end function !integer function j_dataobsw(iv)
 
integer function j_datanobswcum(iv)  ! %%data
	integer,intent(in) ::iv
	j_datanobswcum=j_o(iv)%i(8)
	return
end function !integer function j_datanobswcum(iv)
 
 
 
 
 
 
function j_deftrans(iv,name,leng,lenin,lenout,ivinl,ivoutl, &
		ivlocal,linsource,ivarg,istrans) ! %%trans %%object
		! defines a transformation
	! arguments
	! iv 		: first part of the name is the name of object iv if iv>0
	! name 	: the second part of the name
	! leng	: the length of interpreted transfomations, will be increased if too short
	! ivout	: output-objektin indeksi
	! lenin	: input-mjien listan (alku)koko, 0 = listaa ei tehdä
	! lenout 	: output-mjien listan (alku)koko
	! ivinl	: input-mjien listaolion indeksi
	! ivoutl	: output-mjien listaolion indeksi
	! linsource	: source-tekstiolion rivien (alku)määrä, 0 = source-oliota ei tehdä
	!20150812(arg1<->arg2) oli: subroutine deftrans(name,iv,leng,ivout,lenin,lenout,ivinl,ivoutl,linsource) !defines a transformation
 
	!end module vmod
 
 
	!module typemod
	!ipobj , & first objec ????
 
	!end module
 
	integer, intent(in):: iv,leng,lenin,lenout,linsource
	integer, intent(out):: ivinl,ivoutl,ivlocal
	integer,optional, intent(in):: ivarg
	logical,optional,intent(in)::istrans
	character*(*), intent(in):: name
 
	!20150812(arg1<->arg2) oli: 	call getv(name,iv,iptrans,ivout)
	!write(6,*)'<44deftrans iv name',iv,name
 
	j_deftrans=j_getobject(iv,name,j_iptrans)
	!write(6,*)'<33deftrivout,namedv,otyp*2',ivout,j_namedv,j_otype(ivout),j_iptrans
	!20140522 virheenkäsittely
	if(j_err) return
 
	! write(6,*)name,leng,j_deftrans,ior
	allocate( j_o(j_deftrans)%i(0:leng))
	j_o(j_deftrans)%i=0
	allocate( j_o(j_deftrans)%i2(1:13))
 
	if(lenin.gt.0)then
		!20150812(arg1<->arg2) oli: 		call deflist('input%',j_deftrans,lenin,ivinl)
		!call j_deflist(j_deftrans,'%input',lenin,ivinl)
		ivinl=j_deflistobject(j_deftrans,'%input',nres=lenin)
 
	else !if(lenin.gt.0)then
		ivinl=0
	end if !if(lenin.gt.0)   2330
	if(lenout.gt.0)then
		!20150812(arg1<->arg2) oli: 		call deflist('output%',j_deftrans,lenout,ivoutl)
		ivoutl=j_deflistobject(j_deftrans,'%output',nres=lenout) !,ivoutl)
 
	else !if(lenout.gt.0)then
		ivoutl=0
	end if !if(lenout.gt.0)   2338
	if(lenin.gt.0.or.lenout.gt.0)then
		ivlocal=j_deflistobject(j_deftrans,'%local',nres=40) !,ivoutl)
	else
		ivlocal=0
	endif !if(lenin.gt.0.or.lenout.gt.0)   2345
	if(linsource.gt.0)then
		!20150812(arg1<->arg2) oli: 		call deftext2('source%',ivout,linsource,40*linsource,linsource,ivsource)
		ivsource=j_deftext2(j_deftrans,'%source',linsource,40*linsource,linsource)
	else !if(linsource.gt.0)then
		ivsource=0
	endif !if(linsource.gt.0)   2350
	ivarg_=0
	if(present(ivarg))ivarg_=ivarg
	! write(6,*)'linsource,ivsource ',linsource,ivsource
 
	j_o(j_deftrans)%i(0)=0
	j_o(j_deftrans)%i(1)=0
	j_o(j_deftrans)%i2(1)=ivinl
	j_o(j_deftrans)%i2(2)=ivoutl
	j_o(j_deftrans)%i2(3)=ivarg_ !
	j_o(j_deftrans)%i2(4)=ivlocal !          reserved for nodes%
	j_o(j_deftrans)%i2(5)=0 !periods
	j_o(j_deftrans)%i2(6)=0 !period
	j_o(j_deftrans)%i2(7)=0 !ivtreevars
	j_o(j_deftrans)%i2(8)=0 !ivplotvars simulator
	j_o(j_deftrans)%i2(9)=j_ivarg !ivplotvars
	j_o(j_deftrans)%i2(10)=j_ivresult
	j_o(j_deftrans)%i2(11)=ivsource
	j_o(j_deftrans)%i2(12)=j_deftrans  ! store identity, needed in the simulator
	j_o(j_deftrans)%i2(13)=0
	if(present(istrans))j_o(j_deftrans)%i2(13)=1
	return
end function j_deftrans !subroutine j_deftrans(iv,name,ivout,leng,lenin,lenout,ivinl,ivoutl,linsource,ivarg,istrans)
 
!%%trans the following functions can be used to get subobjects of a trandformation
integer function j_trans_input(iv) ! get the input variable list for a transformation %%trans
	integer, intent(in):: iv
	if(iv.gt.0)then
		j_trans_input=j_o(iv)%i2(1)
	else !if(iv.gt.0)then
		j_trans_input=0
	endif !if(iv.gt.0)   2382
	return
end function j_trans_input !integer function j_trans_input(iv)
 
integer function j_trans_output(iv) ! %%trans outputlist; if there is no then return zero, also when iv=0
	integer, intent(in):: iv
	if(iv.le.0)then
		j_trans_output=0
	else !if(iv.le.0)then
		!write(6,*)'<547iv',iv
		!call j_printname('tr ',iv,' ')
		j_trans_output=j_o(iv)%i2(2)
	endif !if(iv.le.0)   2392
	return
end function j_trans_output !integer function j_trans_output(iv)
 
integer function j_trans_arg(iv) !%%trans gives argument; if there is no then return zero
	integer, intent(in):: iv
	j_trans_arg=j_o(iv)%i2(9)
	return
end function j_trans_arg !integer function j_trans_arg(iv)
 
integer function j_trans_result(iv)   !%%trans if there is no then return zero
	integer, intent(in):: iv
	j_trans_result=j_o(iv)%i2(10)
	return
end function j_trans_result !integer function j_trans_result(iv)
 
integer function j_trans_source(iv) !%%trans
	integer, intent(in):: iv
	j_trans_source=j_o(iv)%i2(11)
	return
end function j_trans_source !integer function j_trans_source(iv)
 
 
subroutine j_cleartext(ivtext) !clear %%text %%object without deleting it
 
	integer, intent(in):: ivtext
 
	if(ivtext.le.0.or.ivtext.gt.j_namedv)then
		write(6,*)'*j* cleartext: illegal ivtext ',ivtext
		j_err=.true.
		return
	endif !if(ivtext.le.0.or.ivtext.gt.j_namedv)   2425
	if(j_otype(ivtext).ne.j_iptext)then
		write(6,*)'*j* cleartext: ivtext= ',ivtext ,' illegal  type ',j_otype(ivtext)
		j_err=.true.
	endif !if(j_otype(ivtext).ne.j_iptext)   2430
	j_o(ivtext)%i(0)=0
end subroutine j_cleartext !subroutine j_cleartext(ivtext)
 
 
!120141208  cleartrans
subroutine j_cleartrans(ivtrans) !clears %%trans %%object without deleting it
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: ivtrans
	integer ::ivinl_
	integer ::ivoutl_
 
	if(j_otype(ivtrans)==j_iptrans) then
		j_o(ivtrans)%i(0)=0
		j_o(ivtrans)%i(1)=0
		ivinl_ = j_o(ivtrans)%i2(1)
		if(ivinl_>0) j_o(ivinl_)%i(0)=0
		ivoutl_ = j_o(ivtrans)%i2(2)
		if(ivoutl_>0) j_o(ivoutl_)%i(0)=0
		ivsource=j_o(ivtrans)%i2(11)
		if(ivsource>0) j_o(ivsource)%i(0)=0
	else !if(j_otype(ivtrans)==j_iptrans) then
		j_err= .true.
		call j_printname('**cleartrans: object ',ivtrans,' is not transformation')
		return
	endif !if(j_otype(ivtrans)==j_iptrans)   2446
	return
end subroutine j_cleartrans !subroutine j_cleartrans(ivtrans)
 
 
! subroutine j_gettrans(ivtrans,ivinput,ivoutput) ! %%trans get a trans-%%object
! !module typemod
! use j_globalsmod, only: j_namedv   ! current namber of named objects
! use errmod, only: j_err
! use j_globalsmod, only: j_otype
! use j_globalsmod, only: j_IPTRANS   ! free named transformations
! use getmod, only: j_printname
! use j_globalsmod, only: j_o
! !ipobj , & first objec ????
 
! !end module
 
! !module vmod
! !end module vmod
 
! !use j_globalfuncsmod, only: printname
 
! integer, intent(in):: ivtrans
! integer, intent(out):: ivinput, ivoutput
 
! if(ivtrans.le.0.or.ivtrans.gt.j_namedv)then
! write(6,*)'*j* getrans, illegal value for ivtrans ',ivtrans
! j_err=.true.
! return
! endif
! if(j_otype(ivtrans).ne.j_iptrans)then
! call j_printname('**not legal transforamtion:',ivtrans,' ');j_err=.true.;return
! end if
 
! ivinput=j_o(ivtrans)%i2(1)
! ivoutput=j_o(ivtrans)%i2(2)
! return
! end subroutine j_gettrans
 
subroutine j_printsource(iob,io)
	integer,intent(in)::iob,io
	ivsource=j_o(iob)%i2(11)
 
	!	write(6,*)'io ',ivsource,io
	if(ivsource.ne.0)then
 
		do i=1,j_o(ivsource)%i(0)
			!	write(6,*)'i ',i
			if(j_o(ivsource)%i2(i).ge.io-2)then
				ili=i-1
				if(j_o(iob)%i(ili).lt.0)ili=ili+1
				write(6,*)'source row ',ili !,j_o(ivsource)%i2(i),io-1
				call j_printtext(ivsource,ili)
				return
			endif !if(j_o(ivsource)%i2(i).ge.io-2)   2507
		enddo !i=1,j_o(ivsource)%i(0)   2505
 
	else !if(ivsource.ne.0)then
		call j_printname('transformation set=',iob,' ')
	endif !if(ivsource.ne.0)   2503
 
 
 
end subroutine !subroutine j_printsource(iob,io)
 
function j_linenumber(iob,io)
	integer,intent(in)::iob,io
 
 
 
	!	call closeallunits()
	ivsource=j_o(iob)%i2(11)
 
 
 
end function !function j_linenumber(iob,io)
 
recursive subroutine j_debug(iob)  !writes what is the origin of an error %%error
 
	integer, intent(in):: iob
	ivsource=j_o(iob)%i2(11)
 
	!	write(6,*)'io ',io
	if(ivsource.ne.0)then
		lin=j_curline(j_recursion)
		write(6,*)(j_o(ivsource)%ch(k),k=j_o(ivsource)%i(lin),&
			j_o(ivsource)%i(lin+1)-1 )
		call j_pause('<t'//j_chi5(lin,0)//'>',do=.true.)
 
	endif !if(ivsource.ne.0)   2542
 
 
 
end subroutine
 
 
subroutine j_debugerr(iob,io)  !writes waht is the origin of an error %%error
	!module vmod
	!end mod
	!use j_globalfuncsmod, only: printname
	!use j_omod, only : lineobuf, ivobuf
 
	integer, intent(in):: iob,io
 
 
	!	call closeallunits()
	j_inpara=.false.
	ivsource=j_o(iob)%i2(11)
	!write(6,*)'<66,debugerr,ivsource,j_ninc',ivsource,j_ninc, ' err ',j_err
	!	write(6,*)'io ',io
	if(ivsource.ne.0)then
		! call j_printname('*err* transformation set=',iob,', *source= ',ivsource)
		! do i=1,j_o(ivsource)%i(0)
		! !	write(6,*)'i ',i
		! if(j_o(ivsource)%i2(i).ge.io-2)then
		! ili=i-1
		! if(j_o(iob)%i(ili).lt.0)ili=ili+1
		! call j_getname(ivsource)
		! write(6,*)'*****error on row ',ili,' in ',j_oname(1:j_loname) !,j_o(ivsource)%i2(i),io-1
		! call j_printtext(ivsource,i-1)
		! !	return
		! endif !if(j_o(ivsource)%i2(i).ge.io-2)then
		! enddo !do i=1,j_o(ivsource)%i(0)
 
		ili=1
		do i=1,io-1
			if(j_o(iob)%i(i).lt.0)then
				ili=ili-j_o(iob)%i(i)
 
			endif !if(j_o(iob)%i(i).lt.0)   2585
 
 
		enddo !i=1,io-1   2584
		call j_getname(ivsource)
		write(6,*)'*****error on row ',ili,' in ',j_oname(1:j_loname) !,j_o(ivsource)%i2(i),io-1
		call j_printtext(ivsource,ili)
 
		return
		write(6,*)'* ask J. Lappi where is the error line'
	else !if(ivsource.ne.0)then
		call j_printname('*err* transformation set=',iob,' ')
	endif !if(ivsource.ne.0)   2569
 
	do loc=1,j_nopt
		if(j_optioniob(loc).eq.iob)then
			if(loc.lt.j_nopt)then
				j_optioniob(loc)=j_optioniob(j_nopt)
				j_optiontot(loc)=j_optiontot(j_nopt)
				j_optionlink(loc)=j_optionlink(j_nopt)
			endif !if(loc.lt.j_nopt)   2604
			j_nopt=j_nopt-1
		endif !if(j_optioniob(loc).eq.iob)   2603
	enddo !loc=1,j_nopt   2602
 
 
 
 
	return
end subroutine j_debugerr !subroutine j_debugerr(iob,io)
 
subroutine j_where(iob,io)
	logical :: notsame
	! ivsource=j_o(iob)%i2(11)
 
	! !	write(6,*)'io ',io
	! if(ivsource.ne.0)then
	! !	call j_printname('*err* transformation set=',iob,', *source= ',ivsource)
	! do i=1,j_o(ivsource)%i(0)
	! !	write(6,*)'i ',i
	! if(j_o(ivsource)%i2(i).ge.io-2)then
	! write(6,*)' row ',i-1 !,j_o(ivsource)%i2(i),io-1
	! call j_printtext(ivsource,i-2)
	! exit
	! endif !if(j_o(ivsource)%i2(i).ge.io-2)then
	! enddo !do i=1,j_o(ivsource)%i(0)
	! lkm=0
	! do i=1,j_o(iob)%i(0)
	! if(j_o(iob)%i(i).lt.0)lkm=lkm-j_o(iob)%i(i)
	! if(i.gt.io)then
	! write(6,*)'row', lkm-1
	! return
	! endif !if(i.gt.io)then
 
	! enddo !do i=1,j_o(iob)%i(0)
	! write(6,*)'row', lkm
	! endif !if(ivsource.ne.0)then
	if(j_ninc.eq.1)then
		!	write(6,*)
		write(6,*)'You are just at sit> prompt, my dear'
		return
	endif !if(j_ninc.eq.1)   2645
 
 
	n1=j_o(j_ivinput0)%i( j_o(j_ivinput0)%i(0)+1) -1
	n2=j_o(j_ivinput1)%i( j_o(j_ivinput1)%i(0)+1) -1
	n3=j_o(j_ivinput2)%i( j_o(j_ivinput2)%i(0)+1) -1
	! if(n1.ne.n2)then
	! write(6,*)'**input after removing comments and spaces: '
	! call j_printtext(j_ivinput1,0)
	! endif
	notsame=.false.
	if(n1.eq.n2)then
		notsame=any(j_o(j_ivinput1)%ch(1:n1).ne.j_o(j_ivinput2)%ch(1:n2))
	else !if(n1.eq.n2)then
		notsame=.true.
	endif !if(n1.eq.n2)   2660
	if(notsame)then
		write(6,*)'****cleaned input'
		call j_printtext(j_ivinput1,0)
	else
		write(6,*)'**input: '
		!			write(6,*)'original'
		call j_printtext(j_ivinput0,0)
 
	endif !if(notsame)   2665
	notsame=.false.
	if(n2.eq.n3)then
		notsame=any(j_o(j_ivinput1)%ch(1:n2).ne.j_o(j_ivinput2)%ch(1:n3))
	else !if(n2.eq.n3)then
		notsame=.true.
	endif !if(n2.eq.n3)   2675
	if(notsame)then
		!	write(6,*)'**input after interpreting input programming: '
		write(6,*)'interpreted'
		call j_printtext(j_ivinput2,0)
	endif !if(notsame)   2680
 
	! write(6,77)inp(1:linp)
	! write(6,*)'**previous line:',inpold(1:linpold)
	!20140812 J2.2
	!elseif(oinp) then
	!	write(6,*)'*j error ',inp(1:linp)
	!	endif !if(j_ninc.gt.1)then
 
	!write(6,*)'nul0',nul(0)
	nul0_=j_ninc
	ial=2
	if(j_v(j_ivcontinue).ne.0.d0)ial=3
	do i=ial,j_ninc
		!	write(6,*)'nul',nul0_,i,j_nul(i)
 
		!400
		iiv=j_inciv(i)
		write(6,*) 'at line ', j_o(iiv)%i(6),' in ',j_vname(iiv)
		!	call j_closeunit(j_nul(i))
		! write(6,*)' ifiout_trans', nuliv(nul(i))
		!	endif !if(j_nul(i).gt.0)then
	enddo !i=ial,j_ninc   2697
 
 
 
end subroutine !subroutine j_where(iob,io)
 
 
subroutine j_puttext(iv,text) ! put %%text into text %%object
	!module vmod
	!end module vmod
 
	integer, intent(in):: iv
	character*(*), intent(in):: text
 
	if(j_otype(iv).ne.j_iptext.and.j_otype(iv).ne.j_ipfigure)then !this ipfig sounds bad
		!  but figure is used aslo as if it were a text object
		call j_printname('*j* j_puttext: ',iv,' is not text object')
		j_err=.true.
		return
 
	endif !if(j_otype(iv).ne.j_iptext.and.j_otype(iv).ne.j_ipfigure)   2720
	if(j_err)then
		write(6,*)'*j* j_putttext called with j_err=.true.'
		return
 
	endif !if(j_err)   2727
	! write(6,*)'obj',iv
	!  write(6,*)'00:',o(io)%i(0)
	ibas=j_o(iv)%i( j_o(iv)%i(0)+1) -1
	le=len(text)
	leb=le+ibas
	!	call j_printname('puut ',iv,' ')
	!	write(6,*)'<33put ',iv,'/',text
	!	write(6,*)ibas,leb
 
	if(leb.gt.ubound(j_o(iv)%ch,dim=1))call j_incch(iv,leb)
	jj=0
	do j=1,le
		j_o(iv)%ch(ibas+j)=text(j:j)
	end do !j=1,le   2743
	j_o(iv)%i(0)=j_o(iv)%i(0)+1
	!write(6,*)'<66',j_o(iv)%i(0),size(j_o(iv)%i),ubound(j_o(iv)%i,dim=1)
	!	if( j_o(iv)%i(0) .ge.ubound(j_o(iv)%i,dim=1))write(6,*)'<66565>',j_o(iv)%i(0),ubound(j_o(iv)%i,dim=1)
	if( j_o(iv)%i(0) .ge.ubound(j_o(iv)%i,dim=1))then
		!		write(6,*)'<57incr j_o(iv)%i(0)',j_o(iv)%i(0),ubound(j_o(iv)%i,dim=1)
		call j_inci(iv,j_o(iv)%i(0))
		if(j_err)then
			call j_getname(iv)
			write(6,*)'*j* error detected in j_puttext when increasing ',j_oname(1:j_loname)
			write(6,*)'j_o(iv)%i(0)',j_o(iv)%i(0),ubound(j_o(iv)%i,dim=1)
			return
		endif !if(j_err)   2752
	endif !if( j_o(iv)%i(0) .ge.ubound(j_o(iv)%i,dim=1))   2749
	j_o(iv)%i( j_o(iv)%i(0)+1 )=j_o(iv)%i( j_o(iv)%i(0))+len(text)
	return
end subroutine j_puttext !subroutine j_puttext(iv,text)
 
 
 
 
subroutine j_puttext2(iv,text) !%%text store also the line number in %i2
	! see deftext2
 
	!module vmod
	!end module vmod
 
	integer, intent(in):: iv
	character*(*), intent(in):: text
 
	!integer,dimension(:),pointer::hh=>null()
 
	call j_puttext(iv,text)
	j_o(iv)%i2(0)=j_o(iv)%i2(0)+1   !allocoinnin tarkastus
	if(j_o(iv)%i2(0).gt.ubound(j_o(iv)%i2,dim=1))call j_inci2(iv,j_o(iv)%i2(0))
	j_o(iv)%i2( j_o(iv)%i2(0) )=j_o(iv)%i(0)
	return
end subroutine j_puttext2 !subroutine j_puttext2(iv,text)
 
 
integer function j_nlines(iv) !number of lines in a %%text object
 
	!module vmod
	!end module vmod
 
	integer, intent(in):: iv
	if(iv.lt.0.or.iv.gt.j_namedv)then
		write(6,*)'*j* j_nlines: argument ',iv,' is not named object'
		j_err=.true.
		return
 
	endif !if(iv.lt.0.or.iv.gt.j_namedv)   2791
	if(j_otype(iv).ne.j_iptext)then
		call j_printname('*j* j_nlines: argument ',iv,' is not a text object')
		j_err=.true.
		return
 
	endif !if(j_otype(iv).ne.j_iptext)   2797
 
	j_nlines=j_o(iv)%i(0)
	return
end function j_nlines !integer function j_nlines(iv)
 
 
subroutine j_putcleantext(iv,text) ! %%text as puttext but remove blanks etc
	!module vmod
	!end module vmod
	!	logical ischar,haka,comment
	integer, intent(in):: iv
	character*(*), intent(inout):: text
	!	ischar=.false.
	!	haka=.false.
 
	! write(6,*)'obj',iv
	!  write(6,*)'00:',o(iv)%i(0)
	call j_clean(text,le)
	if(j_err)return
	call j_puttext(iv,text(1:le))
	!	ibas=j_o(iv)%i( j_o(iv)%i(0)+1) -1
	! jj=0
	! le=len(text)
	! !if(index(text(1:le),'figure(1/0)').gt.0)write(6,*)'putcleanbef',text(1:le)
	! leb=le+ibas
	! if(leb.gt.ubound(j_o(iv)%ch,dim=1))call j_incch(iv,leb)
	! comment=.false.
	! do j=1,le
	! if(text(j:j).eq."'")ischar=.not.ischar
	! if(text(j:j).eq."[")haka=.true.
	! if(text(j:j).eq."]")haka=.false.
	! if(text(j:j).le.' '.and..not.ischar.and..not.haka)cycle
	! if(text(j:j).eq.'!'.and..not.ischar.and..not.haka.and.jj.gt.0)exit
	! if(text(j:j).gt.'~'.and..not.comment)then
	! write(6,*)'ichar ',(ichar(text(jj:jj)),jj=1,le)
	! write(6,*)text(1:le),' contains illegal non-ascii character'
	! write(6,*)'change the text file encoding to ANSI or remove non-ascii'
	! j_err=.true.
	! return
	! endif !if(text(j:j).gt.'~')then
	! if(text(j:j).le.' '.and..not.ischar.and..not.haka)cycle
	! jj=jj+1
	! if(jj.eq.1.and.(text(j:j).eq.'!'.or.text(j:j).eq.'*'))comment=.true.
	! if(jj.ne.j)j_o(iv)%ch(ibas+jj)=text(j:j)
	! end do !do j=1,le
	!	j_o(iv)%i(0)=j_o(iv)%i(0)+1
	!	j_o(iv)%i( j_o(iv)%i(0)+1 )=j_o(iv)%i( j_o(iv)%i(0))+jj
	return
end subroutine j_putcleantext !subroutine j_putcleantext(iv,text)
 
 
subroutine j_putnewcleantext(iv,text,iline) ! clean %%text and put in the text object if not there
	!remove balnks etc
 
	!module vmod
	!end module vmod
 
	integer, intent(in):: iv  !text object
	character*(*), intent(inout):: text
	integer, intent(out):: iline !the in which the text will be
 
	! write(6,*)'obj',iv
	!  write(6,*)'00:',o(iv)%i(0)
	!if(index(text(1:le),'figure(1/0)').gt.0)write(6,*)'putcleanbef',text(1:le)
	call j_clean(text,le)
	!if(index(text(1:le),'figure(1/0)').gt.0)write(6,*)'putcleanaf',text(1:le)
	if(j_err)return
	iline=j_line(iv,text(1:le))
	if(iline.gt.0)return
	call j_puttext(iv,text(1:le))
	iline=j_o(iv)%i(0)
	return
end subroutine j_putnewcleantext !subroutine j_putnewcleantext(iv,text,iline)
 
 
subroutine j_printtext(iob,line) ! print line of %%text object, if line=0 -> print all  (%%io)
	!module vmod
	!end module vmod
 
	integer, intent(in):: iob, line
	integer ::line2_
 
	! write(6,*)'printtext,iob',iob
	if(line.eq.0)then
		line1=1;line2_=j_o(iob)%i(0)
		! write(6,*)'lines',line2_
	else !if(line.eq.0)then
		if(line.gt.j_o(iob)%i(0).or.line.le.0)then
			!call j_getname(iob)
 
			write(6,*)'*illegal line ',line, ' for text object ',iv,&
				' having ',j_o(iob)%i(0), ' lines'
			j_err=.true.;return
		endif !if(line.gt.j_o(iob)%i(0).or.line.le.0)   2890
 
		line1=line;line2_=line
	end if !if(line.eq.0)   2886
 
	do j=line1,line2_
		!write(6,*)j,o(iob)%i(j),o(iob)%i(j+1)-1
		write(6,*)(j_o(iob)%ch(k),k=j_o(iob)%i(j),j_o(iob)%i(j+1)-1 )
	end do !j=line1,line2_   2901
	return
end subroutine j_printtext !subroutine j_printtext(iob,line)
 
subroutine j_gettext(iob,line) ! print line of %%text object, if line=0 -> print all  (%%io)
	!module vmod
	!end module vmod
 
	integer, intent(in):: iob, line
	if(line.gt.j_o(iob)%i(0).or.line.le.0)then
		!call j_getname(iob)
 
		write(6,*)'*illegal line ',line, ' for text object ',iv,&
			' having ',j_o(iob)%i(0), ' lines'
		j_err=.true.;return
	endif !if(line.gt.j_o(iob)%i(0).or.line.le.0)   2913
	!	integer ::line2_
 
	! write(6,*)'printtext,iob',iob
	! if(line.eq.0)then
	! line1=1;line2_=j_o(iob)%i(0)
	! ! write(6,*)'lines',line2_
	! else !if(line.eq.0)then
	! line1=line;line2_=line
	! end if !if(line.eq.0)then
 
	!	do j=line1,line2_
	!write(6,*)j,o(iob)%i(j),o(iob)%i(j+1)-1
	j_lgottext=j_o(iob)%i(line+1)-j_o(iob)%i(line)
	do j=1,j_lgottext
		j_gottext(j:j)=j_o(iob)%ch(j_o(iob)%i(line)+j-1)
	enddo !j=1,j_lgottext   2933
	!		write(6,*)(j_o(iob)%ch(k),k=j_o(iob)%i(j),j_o(iob)%i(j+1)-1 )
	!	end do !do j=line1,line2_
	return
end subroutine j_gettext !subroutine j_printtext(iob,line)
 
 
 
subroutine j_writetext(nu,iob,line,nonum) ! write line of %%text object to unit, if line=0 -> write all
	!module vmod
	!end module vmod
 
	integer, intent(in):: nu,iob, line
	integer ::line2_
	logical, intent(in),optional ::nonum
 
 
	! write(6,*)'printtext,iob',iob
	if(line.eq.0)then
		call j_getname(iob)
		!		write(6,*)'shhs ',j_oname(1:j_loname)
		line1=1;line2_=j_o(iob)%i(0)
		! write(6,*)'lines',line2_
	else !if(line.eq.0)then
		line1=line;line2_=line
	end if !if(line.eq.0)   2953
 
	do j=line1,line2_
		!write(6,*)j,o(iob)%i(j),o(iob)%i(j+1)-1
 
		!		write(6,*)'>cline',j_cline
		if(nu.ne.6)then
 
			i2=j_o(iob)%i(j+1)-1
			i1=j_o(iob)%i(j)
			lkm=i2-i1+1
			do jj=1,lkm
				j_cline(jj:jj)=j_o(iob)%ch(jj+i1-1)
			enddo !jj=1,lkm   2971
			call j_toutf8(j_cline(1:lkm),lkm)
			if(present(nonum))then
 
				write(nu,'(a)')j_cline(1:lkm)
 
 
 
			else
				if(j.le.99)then
					write(nu,'(i2,(a78))')j,' '//j_cline(1:lkm)
				else !if(j.le.99)then
					write(nu,'(i4,(a76))')j,' '//j_cline(1:lkm)
				endif !if(j.le.99)   2982
			endif !if(present(nonum))   2975
		else !if(nu.ne.6)then
			if(present(nonum))then
				write(nu,'(160a1)')(j_o(iob)%ch(k),k=j_o(iob)%i(j),j_o(iob)%i(j+1)-1 )
			else
				if(j.le.99)then
					write(nu,'(i2,(78a1))')j,' ',(j_o(iob)%ch(k),k=j_o(iob)%i(j),j_o(iob)%i(j+1)-1 )
				else !if(j.le.99)then
					write(nu,'(i4,(76a1))')j,' ',(j_o(iob)%ch(k),k=j_o(iob)%i(j),j_o(iob)%i(j+1)-1 )
				endif !if(j.le.99)   2992
			endif !if(present(nonum))   2989
		endif !if(nu.ne.6)   2966
	end do !j=line1,line2_   2962
	return
end subroutine j_writetext !subroutine j_writetext(nu,iob,line)
 
subroutine j_getobjectname(ivin,name,name2,le2)  ! !!object
	integer, intent(in):: ivin
	character*(*), intent(in):: name
	character*(*), intent(inout):: name2
	integer, intent(out):: le2
	logical old
	!write(6,*)'>23getob', ivin,name,itype
	!logical isletter
	!	write(6,*)'>641/'//name//'/',ivin,itype,j_ipchar,j_otype(ivin),name.ne.' '
	le=len_trim(name)
	le2=0
	if(ivin.gt.0)then
		if(j_otype(ivin).eq.j_ipchar)then
			!			write(6,*)'>33',ivin,itype
			call j_getchar(ivin,name2,le2)
		else !if(j_otype(ivin).eq.j_ipchar)then
			call j_objectname(ivin,name2,le2)
		endif !if(j_otype(ivin).eq.j_ipchar)   3015
	else !if(ivin.gt.0)then
		name2(1:le)=name(1:le)
		le2=le
		goto 90
	endif !if(ivin.gt.0)   3014
	!	if(name(1:1).ne.'@')then; li1=1;else;li1=2;end if
 
 
	if(le.gt.0)then
		name2(le2+1:le2+le)=name(1:le)
		le2=le2+le
	endif !if(le.gt.0)   3029
90	continue
	if(.not.(j_isletter(name2(1:1)).or.name2(1:1).eq."'"))then
		write(6,*)'**illegal object name: ',name2(1:le2)
		j_err=.true.
	end if !if(.not.(j_isletter(name2(1:1)).or.name2(1:1).eq."'"))   3034
	return
 
end subroutine !subroutine j_getobjectname(ivin,name,name2,le2)
 
subroutine j_objectname(iv,name,le)
	integer,intent(in):: iv  !object index
	character*(*),intent(out) ::name  !object name
	integer,intent(out):: le  !length of the name
	call j_getline(j_ivnames,iv,name,le)
end subroutine !subroutine j_objectname(iv,name,le)
 
 
subroutine j_getline(iv,line,buffer,le) ! get line from %%text object iv into buffer
	!module vmod
	!end module vmod
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: iv, line
	integer, intent(out):: le  !the length of the line
	character*(*), intent(out) :: buffer
 
	le=0
	lebu=len(buffer)
	if(line.gt.j_o(iv)%i(0).or.line.le.0)then
		!call j_printname('*J:  text obj ',iv, ' ')
		!	call j_getname(iv)
		write(6,*)'**trying to get illegal text obj line ',line, ' from ',j_o(iv)%i(0),&
			' lines in object ', iv
		write(6,*)' use print(Names) to see the object'
		j_err=.true.
		return
	endif !if(line.gt.j_o(iv)%i(0).or.line.le.0)   3061
 
	do k=j_o(iv)%i(line),j_o(iv)%i(line+1)-1
		le=le+1
		if(le.gt.lebu)then
			write(6,*)'*j* trying to get text object line to too small buffer'
			j_err=.true.
			le=le-1  !beginning is however corrrect
			return
		endif !if(le.gt.lebu)   3073
		buffer(le:le)=j_o(iv)%ch(k)
		if(le.ge.lebu)return
	end do !k=j_o(iv)%i(line),j_o(iv)%i(line+1)-1   3071
	return
end subroutine j_getline !subroutine j_getline(iv,line,buffer,le)
 
subroutine j_getname(iv,iv2,iv3) ! get line from %%text object iv into buffer
	!module vmod
	!end module vmod
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: iv
	integer,intent(in),optional::iv2,iv3
 
	if(iv.ne.-1)then
		call j_getname0(iv,j_oname,j_loname)
 
	endif !if(iv.ne.-1)   3093
 
	if(present(iv2))then
		!	write(6,*)'getnaiv2',iv2,j_namedv,
		if(iv2.ne.-1)call j_getname0(iv2,j_oname2,j_loname2)
 
 
	endif !if(present(iv2))   3098
	if(present(iv3))then
		!	write(6,*)'getnaiv2',iv2,j_namedv,
		if(iv3.ne.-1)call j_getname0(iv3,j_oname3,j_loname3)
 
 
	endif !if(present(iv3))   3104
 
 
	return
 
end subroutine j_getname !subroutine j_getname(iv,iv2)
 
subroutine j_getname0(iv,name,lname)
	integer, intent(in):: iv
	integer,intent(out) ::lname
	character*(*),intent(out) ::name
	character*6 chr
	if(iv.le.j_namedv.and.iv.gt.0)then
		call j_getline(j_ivnames,iv,name,lname)
 
 
	elseif(iv.gt.j_mxnamedv.and.iv.le.j_nv)then !if(iv.le.j_namedv)then
		write(chr,'(i6)')iv
		do j=1,4
			if(chr(j:j).ne.' ')exit
		enddo !j=1,4   3127
		name='TEMP'//chr(j:6)
		lname=len_trim(name)
	elseif(iv.gt.0.and.iv.le.j_mxv)then !if(iv.le.j_namedv)then
		write(name,*)j_v(iv)
		lname=len_trim(name)
	else
		write(name,*)iv
		!	j_oname(1:3)='iv='
		lname=len_trim(name)
 
	endif !if(iv.le.j_namedv.and.iv.gt.0)   3121
end subroutine !subroutine j_getline(iv,line,buffer,le)
 
 
subroutine j_getline2(iv,line,buffer,le) !%%text as getline but fills end of buffer with blanks
	!module vmod
	!end module vmod
 
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: iv, line
	integer, intent(out):: le
	character*(*), intent(out) :: buffer
 
	le=0
	lenb=len(buffer)
	if(line.gt.j_o(iv)%i(0).or.line.le.0)then
		call j_getname(iv)
		call j_printname('*J:  text obj ',iv, ' ')
		write(6,*)'**trying to get illegal text obj line ',line, ' from ',j_o(iv)%i(0),' available in ',&
			j_oname(1:j_loname)
		j_err=.true.
		return
	endif !if(line.gt.j_o(iv)%i(0).or.line.le.0)   3156
	! write(17,*)line,o(iv)%i(line+1)-o(iv)%i(line),len(buffer)
	! le=le+1
	do k=j_o(iv)%i(line),j_o(iv)%i(line+1)-1
		le=le+1
		buffer(le:le)=j_o(iv)%ch(k)
		if(le.ge.lenb)return
	end do !k=j_o(iv)%i(line),j_o(iv)%i(line+1)-1   3166
	buffer(le+1:)=' '
 
	return
end subroutine j_getline2 !subroutine j_getline2(iv,line,buffer,le)
 
 
subroutine j_getchar(iv,buffer,le) !%%io get %%char constant or char variable or real var as char*8
	! if iv is $ -variable then this returns '*'
 
 
	integer, intent(in):: iv
	integer, intent(out):: le
	character*(*), intent(out) :: buffer
 
	!20141112 j_err + paluu
	!	if(j_err) return
	lem=len(buffer)
	!write(6,*)'lem ',lem
	if(j_otype(iv).eq.j_ipchar)then
		le=0
		!iv2=iv
		!	write(6,*)'<777 ',j_o(iv)%i(1),j_o(iv)%i(2),j_o(iv)%i(2)-j_o(iv)%i(1)
		do i=j_o(iv)%i(1),j_o(iv)%i(2)
			le=le+1
			if(le.gt.lem)then
				write(6,*)'*j* j_getchar called with too small buffer lentgth ',lem
				j_err=.true.
				return
			endif !if(le.gt.lem)   3195
			buffer(le:le)=j_o(j_ivnames)%ch(i)
			if(buffer(le:le).eq.'~')buffer(le:le)="'"
		end do !i=j_o(iv)%i(1),j_o(iv)%i(2)   3193
		!	write(6,*)'le ',le
		!	write(6,*)buffer(1:le)
		!write(6,*)'/'//j_o(j_ivnames)%ch(j_o(iv)%i(1)-1)//'/'//j_o(j_ivnames)%ch(j_o(iv)%i(2)+1)
	elseif(iv.eq.j_ivdollar)then !if(j_otype(iv).eq.j_ipchar)then
		le=1
		buffer(1:1)='*'
	else !if(j_otype(iv).eq.j_ipchar)then
		le=8
		call j_reps2(buffer,1,8,le,j_v(iv))
	endif !if(j_otype(iv).eq.j_ipchar)   3189
	return
end subroutine j_getchar !subroutine j_getchar(iv,buffer,le)
 
 
!20141112 getchar2
subroutine j_getchar2(iv,buffer,le) !get %%char constant or char varaible into buffer
	! of real
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: iv
	integer, intent(out):: le
	character*(*), intent(out) :: buffer
 
	if(j_err) return
	lem=len(buffer)
	if(j_otype(iv).eq.j_ipchar)then
		le=0
		!iv2=iv
		do i=j_o(iv)%i(1),j_o(iv)%i(2)
			le=le+1
			if(le.gt.lem)then
				write(6,*)'*j* j_getchar called with too small buffer lentgth ',lem
				j_err=.true.
				return
			endif !if(le.gt.lem)   3233
			buffer(le:le)=j_o(j_ivnames)%ch(i)
		end do !i=j_o(iv)%i(1),j_o(iv)%i(2)   3231
	elseif(iv.eq.j_ivdollar)then !if(j_otype(iv).eq.j_ipchar)then
		le=1
		buffer(1:1)='*'
	else !if(j_otype(iv).eq.j_ipchar)then
		call j_printname('*Object ',iv,' is not character constant or character variable')
		j_err = .true.
	endif !if(j_otype(iv).eq.j_ipchar)   3228
	return
end subroutine j_getchar2 !subroutine j_getchar2(iv,buffer,le)
 
subroutine j_getchar3(iv,buffer,le,ext) !get character
	integer, intent(in),optional:: iv
	integer, intent(out):: le
	character*(*), intent(out) :: buffer
	character*(*),intent(in),optional::ext
	!    iv: if iv is character variable or constant, then the initial part of buffer get that
	! otherwise the name of the object is first put tu tbuffer
	le=0
	if(present(iv))then
		if(j_otype(iv).eq.j_ipchar)then
			call j_getchar(iv,buffer,le)
		else !if(j_otype(iv).eq.j_ipchar)then
			call j_getline(j_ivnames,ivin,buffer,le)
		endif !if(j_otype(iv).eq.j_ipchar)   3259
	endif !if(present(iv))   3258
	if(present(ext))then
		le2=j_lentrim(ext)
		buffer(le+1:le+le2)=ext
		le=le+le2
	endif !if(present(ext))   3265
	return
end subroutine j_getchar3 !subroutine j_getchar3(iv,buffer,le,ext)
 
function j_object(name) ! %%object get the index of object with name, if not defined return 0
	!parmod
	! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
	!end module parmod
 
	character*(*), intent(in) :: name
 
	!bypass initila @
	if(len(name).le.0)then
		j_object=0
	elseif(name(1:1).ne.'@')then !if(len(name).le.0)then
		j_object=j_line(j_ivnames,name)
	else !if(len(name).le.0)then
		j_object=j_line(j_ivnames,name(2:))
	end if !if(len(name).le.0)   3281
	!write(6,*)'<6661j_object ',name,j_object
	return
end function j_object !function j_object(name)
 
 
! function j_mtjach1(name,lename) ! get the index of variable with name, if not defined return 0
! !parmod
! use getmod, only: j_linech1
! use j_globalsmod, only: j_ivnames   ! Names-object, text object containing names of all named ob
! ! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
! !end module parmod
 
! character*(*), intent(in) :: name
 
! !bypass initila @
! if(name(1:1).ne.'@')then
! j_mtjach1=j_linech1(j_ivnames,name,lename)
! else
! j_mtjach1=j_linech1(j_ivnames,name(2:),lename-1)
! end if
! return
! end function j_mtjach1
 
 
function j_object2(name,iv)! get index of %%object with name name//name_of_iv if not defined return 0
 
	!module vmod
	!end module vmod
 
	!parmod
	! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
	!end module parmod
 
	integer, intent(in):: iv
	character*(*), intent(in) :: name
 
	le=len(name)
	!bypass initila @
	!if(name(1:1).ne.'@')then
	! mtja=line(ivnames,name)
	!else
	le2=0
	if(iv.gt.0)le2=j_o(j_ivnames)%i(iv+1)-j_o(j_ivnames)%i(iv)
 
	names: do j=1,j_o(j_ivnames)%i(0)
		if(j_o(j_ivnames)%i(j+1)-j_o(j_ivnames)%i(j).eq.le+le2)then
			kk=0
			do k=j_o(j_ivnames)%i(j),j_o(j_ivnames)%i(j)+le-1 ;kk=kk+1
				if(j_o(j_ivnames)%ch(k).ne.name(kk:kk))cycle names
			enddo !k=j_o(j_ivnames)%i(j),j_o(j_ivnames)%i(j)+le-1 ;kk=kk+1   3335
			kk=0
			do k=j_o(j_ivnames)%i(j)+le,j_o(j_ivnames)%i(j+1)-1
				ivc=j_o(j_ivnames)%i(iv)+kk
				if(j_o(j_ivnames)%ch(k).ne.j_o(j_ivnames)%ch(ivc ))cycle names
				kk=kk+1
			enddo !k=j_o(j_ivnames)%i(j)+le,j_o(j_ivnames)%i(j+1)-1   3339
			j_object2=j;return
		endif !if(j_o(j_ivnames)%i(j+1)-j_o(j_ivnames)%i(j).eq.le+le2)   3333
	enddo names !es: do j=1,j_o(j_ivnames)%i(0)   3332
	j_object2=0
	return
end function j_object2 !function j_object2(name,iv)
 
function j_object3(iv,name,char)
 
	! get index of %%object with name name_of_ivname//name if not defined return 0
 
	!module vmod
	!end module vmod
 
	!parmod
	! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
	!end module parmod
 
	integer, intent(in):: iv
	character*(*), intent(in) :: name
	logical,intent(in),optional::char  !object created
	logical ischar
	logical ishipsu
	le=len(name)
	ischar=.false.
	if(present(char))ischar=char
	!bypass initila @
	!if(name(1:1).ne.'@')then
	! mtja=line(ivnames,name)
	!else
	if(iv.gt.j_namedv)then
		write(6,*)'trying to get name of unnamed object ',iv,' maximum is ', j_namedv
		j_err=.true.
		return
	endif !if(iv.gt.j_namedv)   3374
	if(iv.gt.0)then
		call j_getname(iv)
		if(le.gt.0)then
			j_oname(j_loname+1:j_loname+le)=name
			j_loname=j_loname+le
		endif !if(le.gt.0)   3381
	else
		j_oname=name
		j_loname=le
	endif !if(iv.gt.0)   3379
 
	! name can contain ' or not
	!write(6,*)'ischar,iv,le,j_oname(1:j_loname)',ischar,iv,le,j_oname(1:j_loname)
	if(ischar)then
		ishipsu=name(1:1).eq."'"
		if(ishipsu)then
			j_object3=j_object(j_oname(1:j_loname))
		else
			j_object3=j_object("'"//j_oname(1:j_loname)//"'")
		endif !if(ishipsu)   3394
	else
 
		j_object3=j_object(j_oname(1:j_loname))
	endif !if(ischar)   3392
	if(j_object3.gt.0)then
		if(ischar)j_o(j_object3)%i(3:4)=0
		return
 
	endif !if(j_object3.gt.0)   3403
	call j_getobjectnam(j_oname(1:j_loname),j_ipchar,j_object3)
 
	if(ischar)then
		allocate( j_o(j_object3)%i(1:10))
		! do ii=j_o(j_ivnames)%i(ivout)-1,j_o(j_ivnames)%i(ivout+1)+1
		! write(6,*)ii,j_o(j_ivnames)%ch(ii)
		! enddo
		j_o(j_object3)%i(1)=j_o(j_ivnames)%i(j_object3)+1
		j_o(j_object3)%i(2)=j_o(j_ivnames)%i(j_object3+1)-2
	endif !if(ischar)   3410
	!if(p)write(6,*)'<6669',j_o(ivout)%i(1:2)
 
 
	return
end function j_object3 !function j_object3(iv,name)
 
 
function j_line(iv,name)   !get line number of %%text object consisting of name, not found =>0
	!module vmod
	!end module vmod
 
	integer, intent(in):: iv
	character*(*), intent(in) :: name
 
	le=len(name)
 
 
	do j=1,j_o(iv)%i(0)
		!	if(j_v(j_ivdebug).ge.2.and.j.eq.j_o(iv)%i(0))write(6,*)'isle',j_o(iv)%i(j+1)-j_o(iv)%i(j).eq.le
		if(j_o(iv)%i(j+1)-j_o(iv)%i(j).eq.le)then
			kk=0
			do k=j_o(iv)%i(j),j_o(iv)%i(j+1)-1
				kk=kk+1
 
				if(j_o(iv)%ch(k).ne.name(kk:kk))goto 5
			end do !k=j_o(iv)%i(j),j_o(iv)%i(j+1)-1   3439
			j_line=j
			return
		endif !if(j_o(iv)%i(j+1)-j_o(iv)%i(j).eq.le)   3437
5 	continue
	enddo !j=1,j_o(iv)%i(0)   3435
	j_line=0
	return
end function j_line !function j_line(iv,name)
 
 
function j_line2(ivtext,ivchar) ! as j_line but now the input %%text is a charvar or charconst
	!module vmod
	!end module vmod
 
	!parmod
	! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
	!end module parmod
 
	!module typemod
	!ipobj , & first objec ????
 
	!end module
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: ivtext, ivchar
 
	if(j_otype(ivtext).ne.j_iptext)then
		call j_printname('**not legal text',ivtext,' '); j_err=.true.;return
	endif !if(j_otype(ivtext).ne.j_iptext)   3470
	if(j_otype(ivchar).ne.j_ipchar)then
		call j_printname('**Not legal charv',ivchar,' ') ;j_err=.true. ;return
	endif !if(j_otype(ivchar).ne.j_ipchar)   3473
	le=j_o(ivchar)%i(2)-j_o(ivchar)%i(1)+1
	do j=1,j_o(ivtext)%i(0)
		if(j_o(ivtext)%i(j+1)-j_o(ivtext)%i(j).eq.le)then
			! write(6,*)'line',j,o(io)%i(j),o(io)%i(j+1)
			kk=j_o(ivchar)%i(1)
			do k=j_o(ivtext)%i(j),j_o(ivtext)%i(j+1)-1
				if(j_o(ivtext)%ch(k).ne.j_o(j_ivnames)%ch(kk))goto 5
				kk=kk+1
			enddo !k=j_o(ivtext)%i(j),j_o(ivtext)%i(j+1)-1   3481
			j_line2=j;return
		endif !if(j_o(ivtext)%i(j+1)-j_o(ivtext)%i(j).eq.le)   3478
5 	continue
	enddo !j=1,j_o(ivtext)%i(0)   3477
	j_line2=0
 
	return
end function j_line2 !function j_line2(ivtext,ivchar)
 
 
! function j_linech1(ivtext,name,le) ! as j_line but now the input text length is given explicitly NOTneeded
! !module vmod
! use j_globalsmod, only: j_otype
! use j_globalsmod, only: j_IPTEXT
! use getmod, only: j_printname
! use errmod, only: j_err
! use j_globalsmod, only: j_o
! !end module vmod
 
! !parmod
! ! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
! !end module parmod
 
! !module typemod
! !ipobj , & first objec ????
 
! !end module
! !use j_globalfuncsmod, only: printname
 
! integer, intent(in):: ivtext, le
! character*1, intent(in) :: name(le)
 
! if(j_otype(ivtext).ne.j_iptext)then
! call j_printname('**not legal text',ivtext,' '); j_err=.true.;return
! end if
 
! do j=1,j_o(ivtext)%i(0)
! if(j_o(ivtext)%i(j+1)-j_o(ivtext)%i(j).eq.le)then
! ! write(6,*)'line',j,o(io)%i(j),o(io)%i(j+1)
! kk=1  !o(ivchar)%i(1)
! do k=j_o(ivtext)%i(j),j_o(ivtext)%i(j+1)-1
! if(j_o(ivtext)%ch(k).ne.name(kk))goto 5
! kk=kk+1
! enddo
! j_linech1=j;return
! endif
! 5 	continue
! enddo
! j_linech1=0
 
! return
! end function j_linech1
 
 
 
 
 
subroutine j_putor(iv,iel,val) !puts value val into element iel of the double fork of %%object iv
	! if the length of o(iv)&r is not large enough it is increased
	!end module vmod
 
	integer, intent(in):: iv,iel
	real, intent(in):: val
 
	if(iel.gt.ubound(j_o(iv)%r,dim=1))call j_incr(iv,iel)
	j_o(iv)%r(iel)=val
	return
end subroutine j_putor !subroutine j_putor(iv,iel,val)
 
subroutine j_putod(iv,iel,val) !puts value val into element iel of the double fork of %%object iv
	! if the length of o(iv)&r is not large enough it is increased
	!end module vmod
 
	integer, intent(in):: iv,iel
	double precision, intent(in):: val
 
	if(iel.gt.ubound(j_o(iv)%d,dim=1))call j_incr(iv,iel)
	j_o(iv)%d(iel)=val
	return
end subroutine j_putod !subroutine j_putod(iv,iel,val)
 
 
subroutine j_putoi2(iv,iel,ival) !putting value ival into element iel the integer fork i2 of %%object iv
	! if the length of o(iv)&i2 is not large enough it is increased
	!end module vmod
 
	integer, intent(in):: iv,iel,ival
 
	if(iel.gt.ubound(j_o(iv)%i2,dim=1))then
		! iel0=iel !iel may contain reference to h2
		call j_inci2(iv,iel)
		! iel=iel0
	endif !if(iel.gt.ubound(j_o(iv)%i2,dim=1))   3573
	j_o(iv)%i2(iel)=ival
	return
end subroutine j_putoi2 !subroutine j_putoi2(iv,iel,ival)
 
 
subroutine j_putoi(iv,iel,ival) !putting value ival into element iel the integer fork of iv
	! if the length of o(iv)&i is not large enough it is increased
	!end module vmod
 
	integer, intent(in):: iv,iel,ival
 
	! if iel is zero then put also the zero after the element ival
	iup=ubound(j_o(iv)%i,dim=1)
	if(iel.gt.iup)then
		call j_inci(iv,iel)
		if(j_err)then
			call j_getname(iv)
			write(6,*)'*j* error detected in j_putoi when increasing ',j_oname(1:j_loname)
			write(6,*)j_o(iv)%i(0),iel,size(j_o(iv)%i)
			return
		endif !if(j_err)   3593
	endif !if(iel.gt.iup)   3591
	j_o(iv)%i(iel)=ival
	! if(iel.eq.0)then
	! if(ival.ge.iup)call j_inci(iv)   !putoizero
	! j_o(iv)%i(ival+1)=0
	! endif
	return
end subroutine j_putoi !subroutine j_putoi(iv,iel,ival)
 
subroutine j_putoizero(iv,ival) !put ival to o(iv)%i(0) and 0 to o(iv)%i(ival+1) (j_compiler)
	! if the length of o(iv)&i is not large enough it is increased
	!end module vmod
 
	integer, intent(in):: iv,ival
 
	! if iel is zero then put also the zero after the element ival
	iup=ubound(j_o(iv)%i,dim=1)
	j_o(iv)%i(0)=ival
	if(ival.ge.iup)then
		call j_inci(iv,iup)
		if(j_err)then
			call j_getname(iv)
			write(6,*)'*j* error detected in j_puttext when increasing ',j_oname(1:j_loname)
			return
		endif !if(j_err)   3619
	endif !if(ival.ge.iup)   3617
	j_o(iv)%i(ival+1)=0
	return
end subroutine j_putoizero !subroutine j_putoizero(iv,ival)
 
 
subroutine j_incch(iv,mins) !increase size of %%char fork of an %%object iv i.e. o(iv)%ch
	!module vmod
	!end module vmod
 
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: iv,mins
 
	character*1,dimension(:),allocatable::ch2
 
	if(.not.allocated(j_o(iv)%ch))then
		write(6,*)'*j* j_incch trying to expand character fork which is not allocated'
		j_err=.true.
		return
	endif !if(.not.allocated(j_o(iv)%ch))   3640
 
	isi=size(j_o(iv)%ch)
	if(isi.le.0)then
		write(6,*)'*j* j_incch, trying to increas charcater vector which has zero size'
		j_err=.true.
		return
	endif !if(isi.le.0)   3647
	allocate(ch2(1:isi))
	!call j_printname('*doubling the  size of text fork of ',iv, ' ')
	ch2=j_o(iv)%ch
	deallocate(j_o(iv)%ch)
	allocate(j_o(iv)%ch(1:max(2*isi,mins)))
	j_o(iv)%ch(1:isi)=ch2
	deallocate(ch2)
	return
end subroutine j_incch !subroutine j_incch(iv,mins)
 
 
subroutine j_inci(iv,mins) !increase size of the integer fork of an %%object iv ,i.e. o(iv)%i
	!module vmod
	!end module vmod
 
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: iv,mins
 
	integer,dimension(:),allocatable::hh
 
	if(.not.allocated(j_o(iv)%i))then
		write(6,*)'*j* j_inci trying to expand %i fork which is not allocated'
		j_err=.true.
		return
	endif !if(.not.allocated(j_o(iv)%i))   3673
 
	lb=lbound(j_o(iv)%i,dim=1)
	iup=ubound(j_o(iv)%i,dim=1)
	allocate(hh(lb:iup))
	!	call j_printname('*doubling the integer fork of ',iv, ' ')
	!write(6,*)'new lower/upper bound ', lb, 2*iup
	hh=j_o(iv)%i
	!	write(6,*)'<355353535 ',lb,iup,mins
	new=max(2*iup,mins)
	deallocate(j_o(iv)%i)
	! if(max(2*iup,mins).gt.100000000)then
	! write(6,*)'*j* trying to allocate huge integer vector iup=',iup,' mins=',mins
	! j_err=.true.
	! return
	! endif
	allocate(j_o(iv)%i(lb:new))
	j_o(iv)%i(lb:iup)=hh
	deallocate(hh)
	return
end subroutine j_inci !subroutine j_inci(iv,mins)
 
 
subroutine j_incr(iv,mins) !increase size of the real fork of  %%object iv ,i.e. o(iv)%r
	!module vmod
	!end module vmod
 
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: iv,mins
 
	real,dimension(:),allocatable::hh
 
	if(.not.allocated(j_o(iv)%r))then
		write(6,*)'*j* j_incr trying to expand %r fork which is not allocated'
		j_err=.true.
		return
	endif !if(.not.allocated(j_o(iv)%r))   3710
 
	lb=lbound(j_o(iv)%r,dim=1)
	iup=ubound(j_o(iv)%r,dim=1)
	allocate(hh(lb:iup))
	!	call j_printname('*doubling the real fork of ',iv, ' ')
	hh=j_o(iv)%r
	deallocate(j_o(iv)%r)
	allocate(j_o(iv)%r(lb:max(2*iup,mins)))
	j_o(iv)%r(lb:iup)=hh
	deallocate(hh)
	return
end subroutine j_incr !subroutine j_incr(iv,mins)
 
subroutine j_inctxt(iv,mins) !increase size of the real fork of  %%object iv ,i.e. o(iv)%r
	!module vmod
	!end module vmod
 
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: iv,mins
	character*(j_txtlen),allocatable,dimension(:)::temp
	isize=j_o(j_gpiout)%i(2)
 
	if(allocated(temp))deallocate(temp)
	isize2=max(2*isize,mins)
	allocate(temp(1:isize2))
	temp(1:isize)=j_o(j_gpiout)%txt(1:isize)
	deallocate(j_o(j_gpiout)%txt,j_o(j_gpiout)%i2)
	allocate(j_o(j_gpiout)%txt(1:isize2),j_o(j_gpiout)%i2(1:isize2))
	j_o(j_gpiout)%txt(1:isize)=temp(1:isize)
	j_o(j_gpiout)%txt(isize+1:isize2)=' '
	do i=1,isize
		j_o(j_gpiout)%i2(i)=len_trim(j_o(j_gpiout)%txt(i))
	enddo !i=1,isize   3746
	j_o(j_gpiout)%i2(isize+1:isize2)=0
	j_o(j_gpiout)%i(2)=isize2
	deallocate(temp)
	return
end subroutine j_inctxt !subroutine j_inctxt(iv,mins)
 
 
 
 
subroutine j_inci2(iv,mins) !increase size of the 2. integer fork of %%object iv, i.e. o(iv)%i2
	!module vmod
	!end module vmod
 
	!use j_globalfuncsmod, only: printname
 
	integer, intent(in):: iv,mins
 
	integer,dimension(:),pointer::hh=>null()
	if(.not.allocated(j_o(iv)%i2))then
		write(6,*)'*j* j_inci2 trying to expand %i2 fork which is not allocated'
		j_err=.true.
		return
	endif !if(.not.allocated(j_o(iv)%i2))   3767
 
	lb=lbound(j_o(iv)%i2,dim=1)
	iup=ubound(j_o(iv)%i2,dim=1)
	allocate(hh(lb:iup))
	!	call j_printname('*doubling the 2. integer fork of ',iv, ' ')
	hh=j_o(iv)%i2
	deallocate(j_o(iv)%i2)
	allocate(j_o(iv)%i2(lb:max(2*iup,mins)))
	j_o(iv)%i2(lb:iup)=hh
	deallocate(hh)
	return
end subroutine j_inci2 !subroutine j_inci2(iv,mins)
 
 
function j_isin(text,vector,n) !location of text in character vector vector with size n
	integer, intent(in):: n
	character*(*), intent(in)::  text, vector(n)
 
	!20141217 oli: le1=len(text)
	le1=len_trim(text)
	do i=1,n
		le2=len_trim(vector(i))
		if(le1.eq.le2) then
			!20141217 oli: if(text.eq.vector(i)(1:le2))then
			if(text(1:le1).eq.vector(i)(1:le2))then
				j_isin=i
				return
			endif !if(text(1:le1).eq.vector(i)(1:le2))   3796
		endif !if(le1.eq.le2)   3794
	enddo !i=1,n   3792
	j_isin=0
	return
end function j_isin !function j_isin(text,vector,n)
 
 
subroutine j_clean(text,le) ! remove blanks, tabs etc , le is the length of the cleaned text
	integer, intent(out):: le
	character (len=*),intent(inout):: text
	logical ischar,haka,p
	p=.false.
	ischar=.false.
	haka=.false.
	jj=0
	!if line starts with ! it is returned as a comment
	le1=j_lentrim(text)
	!	p=index(text(1:le1),'figure(1/0)').gt.0
	le=0
	if(p)write(6,*)'<559>',text(1:le1)
	do j=1,le1
		if(text(j:j).eq."'")ischar=.not.ischar
		if(text(j:j).eq."[")haka=.true.
		if(text(j:j).eq."]")haka=.false.
		if(text(j:j).le.' '.and..not.ischar.and..not.haka)cycle
		if(text(j:j).eq.'!'.and..not.ischar.and..not.haka.and.jj.gt.0)exit
		if(text(j:j).gt.'~')then
			if(ichar(text(j:j)).eq.194)then
				write(6,*)'illegal use of "'
				j_err=.true.;return
			endif !if(ichar(text(j:j)).eq.194)   3827
			write(6,*)'ichar ',(ichar(text(jj:jj)),jj=1,le1)
			write(6,*)text(1:le1),' contains illegal non-ascii character'
			write(6,*)'change the text file encoding to ANSI or remove non-ascii'
			j_err=.true.
			return
		endif !if(text(j:j).gt.'~')   3826
		if(p)write(6,*)'j,jj',j,jj,ischar,text(j:j)
		jj=jj+1
 
		if(jj.eq.1.and.(text(jj:jj).eq.'!'.or.text(jj:jj).eq.'*'))then
			if(j.eq.le1)return
			if(text(j+1:j+1).ne.'!'.and.text(j+1:j+1).ne.'*')return
			le=le1-j
			text(1:le)=text(j+1:le1)
			return
		endif !if(jj.eq.1.and.(text(jj:jj).eq.'!'.or.text(jj:jj).eq.'*'))   3840
		if(jj.ne.j)text(jj:jj)=text(j:j) !this can now be used for clean character constants also
	enddo !j=1,le1   3820
	le=jj
	if(le.lt.le1)then
		text(jj+1:le1)=' '
	endif !if(le.lt.le1)   3850
	if(p)write(6,*)text(1:le),le
	!	if(p)stop
	return
end subroutine j_clean !subroutine j_clean(text,le)
 
 
!
subroutine j_differ(list1,n1,list2,n2,list3,n3) !picks from %%list1 elements which are not in list2 to list3
	!list1 and list3 can be the same as well n1 and n3
	integer, intent(in):: n1, n2
	integer, intent(out):: n3
	integer, intent(in):: list1(*),list2(*)
	integer, intent(out):: list3(*)
 
	no=0
	do i=1,n1
		if(any(list1(i).eq.list2(1:n2)))cycle
		no=no+1
		list3(no)=list1(i)
	enddo !i=1,n1   3868
	n3=no
	return
end subroutine j_differ !subroutine j_differ(list1,n1,list2,n2,list3,n3)
 
 
function j_ndiffer(list1,n1,list2,n2) !number of elements of %%list1 which are not in list2
	integer, intent(in):: n1,n2
	integer, intent(in):: list1(*),list2(*)
 
	j_ndiffer=0
	do i=1,n1
		if(any(list1(i).eq.list2(1:n2)))cycle
		j_ndiffer=j_ndiffer+1
	enddo !i=1,n1   3883
	return
end function j_ndiffer !function j_ndiffer(list1,n1,list2,n2)
 
 
subroutine j_union(list1,n1,list2,n2,list3,n3) !the union of %%list1 and list2 put to list3
	!list1 and list3 can be the same as well n1 and n3
	integer, intent(in):: n1,n2
	integer, intent(in):: list1(*),list2(*)
	integer, intent(out):: list3(*)
	integer, intent(out):: n3
 
	no=n1
	list3(1:n1)=list1(1:n1)
	do i=1,n2
		if(any(list2(i).eq.list1(1:n1)))cycle
		no=no+1
		list3(no)=list2(i)
	enddo !i=1,n2   3900
	n3=no
	return
end subroutine j_union !subroutine j_union(list1,n1,list2,n2,list3,n3)
 
function j_nunion(list1,n1,list2,n2) !size of union of %%list1 and list2
	integer, intent(in):: n1,n2
	integer, intent(in):: list1(*),list2(*)
 
	j_nunion=n1
	do i=1,n2
		if(any(list2(i).eq.list1(1:n1)))cycle
		j_nunion=j_nunion+1
	enddo !i=1,n2   3914
	return
end function j_nunion !function j_nunion(list1,n1,list2,n2)
 
 
subroutine j_uniondif(list1,n1,list2,n2,list3,n3,list4,n4)  !%%list1+list2-list3
	!+ = union
	!- =intersection
	!list1 and list3 can be the same as well n1 and n3
	integer, intent(in):: n1,n2,n3
	integer, intent(out):: n4
	integer, intent(in):: list1(*),list2(*),list3(*)
	integer, intent(out):: list4(*)
 
	no=0
	do i=1,n1
		if(any(list1(i).eq.list3(1:n3)))cycle
		no=no+1
		list4(no)=list1(i)
	enddo !i=1,n1   3932
 
	no1=no
	do i=1,n2
		if(any(list2(i).eq.list4(1:no1)).or.any(list3(1:n3).eq.list2(i)))cycle
		no=no+1
		list4(no)=list2(i)
	enddo !i=1,n2   3939
	n4=no
	return
end subroutine j_uniondif !subroutine j_uniondif(list1,n1,list2,n2,list3,n3,list4,n4)
 
 
 
 
subroutine j_msd2(nvar,x,xm,ss,wt,sumwt,summa)
	integer, intent(in):: nvar
	double precision, intent(in):: wt
	double precision, intent(out):: xm(nvar),ss(nvar),summa(nvar),sumwt
	double precision, intent(in):: x(nvar)
 
	double precision b,c,apu
 
	! if(sumwt.lt.0.)then
	! xm=j_0
	! ss=j_0
	! summ=j_0
	! ! do i=1,nvar
	! ! xm(i)=0.d0
	! ! ss(i)=0.d0
	! ! end do !do i=1,nvar
	! sumwt=j_0
	! summa=j_0
	!	end if !if(sumwt.lt.0.)then
	sumwt=sumwt+wt
	b=wt/sumwt
	c=wt-b*wt
	do i=1,nvar
		apu=x(i)-xm(i)
		xm(i)=xm(i)+b*apu
		ss(i)=ss(i)+c*apu**2
	end do !i=1,nvar   3973
	summa=summa+wt*x
	!	write(6,*)'<176>x(1),xm(1),ss(1),wt,sumwt',x(1),xm(1),ss(1),wt,sumwt
	return
end subroutine j_msd2 !subroutine j_msd2(nvar,x,xm,ss,wt,sumwt)
 
subroutine j_msd21(x,xm,ss,wt,sumwt,summa)
	double precision, intent(in):: wt
	double precision, intent(out):: xm,ss,sumwt
	double precision,intent(out),optional:: summa
	double precision, intent(in):: x
	double precision b,c,apu
 
	if(sumwt.lt.j_0)then
		!	do i=1,nvar
		xm=j_0
		ss=j_0
		!	end do !do i=1,nvar
		sumwt=j_0
 
	end if !if(sumwt.lt.j_0)   3990
	if(present(summa))summa=summa+x
	sumwt=sumwt+wt
	b=wt/sumwt
	c=wt-b*wt
	!	do i=1,nvar
	apu=x-xm
	xm=xm+b*apu
	ss=ss+c*apu**2
	!	end do !do i=1,nvar
	!	write(6,*)'<176>x(1),xm(1),ss(1),wt,sumwt',x(1),xm(1),ss(1),wt,sumwt
	return
end subroutine j_msd21 !subroutine j_msd21(x,xm,ss,wt,sumwt)
 
 
function j_nonblank(inp,ial,lop)
	integer, intent(in):: ial,lop
	character*(*), intent(in):: inp
 
	do i=ial,lop
		if(ichar(inp(i:i)).gt.32)then
			j_nonblank=i
			return
		endif !if(ichar(inp(i:i)).gt.32)   4017
	enddo !i=ial,lop   4016
	j_nonblank=lop+1
	return
end function j_nonblank !function j_nonblank(inp,ial,lop)
 
 
!20141208 lastblank
function j_lastblank(inp,ial,lop)
	integer, intent(in):: ial,lop
	character*(*), intent(in):: inp
 
	do i=lop,ial,-1
		if(inp(i:i)<=' ') then
			j_lastblank=i
			return
		endif !if(inp(i:i)<=' ')   4033
	enddo !i=lop,ial,-1   4032
	j_lastblank = 0
	return
end function j_lastblank !function j_lastblank(inp,ial,lop)
 
 
subroutine j_reps2(inp,i1,i2,lop,aa)
	! replaces segement i1:ii2 with the character value of aa
	! if aa seems to be integer then integer representation is made,lop updated
 
	integer, intent(in):: i1,i2
	integer, intent(out):: lop
	double precision, intent(in):: aa
	character*(*), intent(inout):: inp
 
	character*10 chv    ! ,chr8
	!
	ii1=aa-0.0000001
	ii2=aa+0.0000001
	if(ii2.eq.ii1+1.or.aa.eq.0.)then
		! integer
		call j_repse(inp,i1,i2,lop,ii2)
	else !if(ii2.eq.ii1+1.or.aa.eq.0.)then
		chv=j_chr10(aa)
		ie=j_nextlim(chv,1,10,'eE')
		if(j_err)return
		nz=0
		do i=ie-1,2,-1
			if(chv(i:i).ne.'0') goto 2
			nz=nz+1
		enddo !i=ie-1,2,-1   4064
2   if(nz.ne.0.and.ie.lt.8)then
		!        write(6,*)nz,i,':',chv
			do  j=i+1,i+nz
				chv(j:j)=chv(j+nz:j+nz)
				!        write(6,*)j,':',chv
			enddo ! j=i+1,i+nz   4070
		endif !2   if(nz.ne.0.and.ie.lt.8)   4068
		!        write(6,*)i1,i2,lop,chv,nz
		call j_repl(inp,i1,i2,lop,chv,8-nz)
		!        write(6,*)lop,':',inp
	endif !if(ii2.eq.ii1+1.or.aa.eq.0.)   4056
	return
end subroutine j_reps2 !subroutine j_reps2(inp,i1,i2,lop,aa)
 
subroutine j_repl(jono1,i1,i2,linp,jono2,le2) !replaces the substring jono1(i1:i2) by string jono2(1:le2)
	!*
	!*linp is the initial and final length of jono1
	!*if string becomes shorter blanks are added
	!*if jono2 is prepended then i1 should be 1 and i2 should be 0
	!*if le2 is zero then section jono1(i1:i2) is removed
	integer, intent(in):: i1,i2,le2
	integer, intent(inout):: linp
	character*(*), intent(inout):: jono1
	character*(*), intent(in):: jono2
 
	!***********************************************
	!nc is the number of net change of the lenght of jono1
	nc=le2-(i2-i1+1)
	if(nc.lt.0)then
		j=i1+le2
		! shift left
		do  i=i2+1,linp
			jono1(j:j)=jono1(i:i)
			j=j+1 !do 1 i=i2+1,linp
		enddo ! i=i2+1,linp   4099
		jono1(linp+nc+1:linp)=' '
	else if(nc.gt.0) then !if(nc.lt.0)then
		! shift right
		linp2=len(jono1)
		j=linp+nc
		if(j.gt.linp2)then
			!     call jout(1,'call ambulance')
			stop 'per..'
			!         return
		endif !if(j.gt.linp2)   4108
		do  i=linp,i2+1,-1
			jono1(j:j)=jono1(i:i)
			j=j-1 !do 2 i=linp,i2+1,-1
		enddo ! i=linp,i2+1,-1   4113
	endif !if(nc.lt.0)   4096
	if(le2.gt.0)jono1(i1:i1+le2-1)=jono2(1:le2)
	linp=linp+nc
	return
end subroutine j_repl !subroutine j_repl(jono1,i1,i2,linp,jono2,le2)
 
 
subroutine j_repse(inp,i1,i2,lop,iii,ial2)
	! replace segement i1:i2 by shortest presentation of integer iii
	integer, intent(in):: i1,i2,iii
	integer, intent(inout):: lop
	integer,optional,intent(out)::ial2 !the next charcter after replacement
	character*(*), intent(inout):: inp
 
	character*8 buf
 
1 format(i8)
	write(buf,1)iii
	do j=1,8
		if(buf(j:j).ne.' ')exit !do 20 j=1,8
	enddo !j=1,8   4135
	!21 continue
	!         if(buf(j:j).eq.'-')buf(j:j)='_'
	li=9-j
	jero=li-(i2-i1+1)
	if(jero.lt.0)then
		! move left
		do  k=i1+li,lop+jero
			inp(k:k)=inp(k-jero:k-jero) !do 30 k=i1+li,lop+jero
		enddo ! k=i1+li,lop+jero   4144
		inp(lop+jero+1:lop)=' '
	else if(jero.gt.0)then !if(jero.lt.0)then
		! move right
		!        if(inp(1:1).eq.'$')write(6,*)'jero',jero, 'i1,li',i1,li
		do  k=lop,i2+1,-1
			!      write(6,*)'k,ik',k,inp(k:k)
			inp(k+jero:k+jero)=inp(k:k) !do 31 k=lop,i2+1,-1
		enddo ! k=lop,i2+1,-1   4151
		!     if(inp(1:1).eq.'$')write(6,*)'häär',inp
	end if !if(jero.lt.0)   4142
	inp(i1:i1+li-1)=buf(j:8)
	if(present(ial2))ial2=i1+li
	!           if(inp(1:1).eq.'$')write(6,*)'nyt',inp
	lop=lop+jero
	!           write(6,*)inp(1:lop)
	return
end subroutine j_repse !subroutine j_repse(inp,i1,i2,lop,iii,ial2)
 
 
 
 
 
!=chr8=== file jlpsub.src ==============================================
! character*8 function j_chr8_s(a)
! ! Returns real value as a character*8 variable.
! !*
! ! Because the function uses WRITE statement, it cannot be used in
! ! WRITE statements (recursive WRITEs causes trouble).
! real, intent(in):: a
 
! !***********************************************
! real zero
! character*16 buf
! data zero/0./
 
! if(abs(a).ge.1.e-4.or.a.eq.zero)then
! write(buf,1)a
! else !if(abs(a).ge.1.e-4.or.a.eq.zero)then
! write(buf,2)a
! end if !if(abs(a).ge.1.e-4.or.a.eq.zero)   4209
! 1 format(f16.7)
! 2 format(e8.1)
! call j_adjul2(buf)
! j_chr8_s=buf
! return
! end function j_chr8_s !character*8 function j_chr8_s(a)
 
! character*8 function j_chr8_d(a)
! ! Returns real value as a character*8 variable.
! !*
! ! Because the function uses WRITE statement, it cannot be used in
! ! WRITE statements (recursive WRITEs causes trouble).
! double precision, intent(in):: a
 
! !***********************************************
! real zero
! character*16 buf
! data zero/0./
 
! if(abs(a).ge.1.e-4.or.a.eq.zero)then
! write(buf,1)a
! else !if(abs(a).ge.1.e-4.or.a.eq.zero)then
! write(buf,2)a
! end if !if(abs(a).ge.1.e-4.or.a.eq.zero)   4233
! 1 format(f16.7)
! 2 format(e8.1)
! call j_adjul2(buf)
! j_chr8_d=buf
! return
! end function j_chr8_d !character*8 function j_chr8_d(a)
 
 
!=chr8=== file jlpsub.src ==============================================
character*8 function j_chr8b(a,le)
	! Returns real value as a character*8 variable.
	!* drops ending zeros and possibly also decimal point
	! Because the function uses WRITE statement, it cannot be used in
	! WRITE statements (recursive WRITEs causes trouble).
	real, intent(in):: a
	integer, intent(out):: le
 
	!***********************************************
	real zero
	character*16 buf
	data zero/0./
 
	if(a.eq.zero)then
		j_chr8b='0';le=1; return
	elseif(a.eq.1)then !if(a.eq.zero)then
		j_chr8b='1';le=1; return
	endif !if(a.eq.zero)   4233
	!      write(6,*)a
	last=16
	if(a.gt.0.)then
		if(a.lt.1.e-10)then
			! 0.999e-10
			write(buf(1:16),10)a
10    format(g16.3)
			!          write(6,*)'#1#:',buf
		else if(a.le.1.e-4)then !if(a.lt.1.e-10)then
			! 0.9999e-03  ->.9999e-3
			write(buf,11)a
			!           write(6,*)'#2#:',buf
			buf(15:15)=buf(16:16)
			last=15
11    format(g16.4)
		else if(a.lt.1.)then !if(a.lt.1.e-10)then
			write(buf(1:12),111)a
			buf(13:16)=' '
111   format(f12.7)
		else if(a.le.10000000.)then !if(a.lt.1.e-10)then
			! 0.0111112   999999.9
			write(buf(1:16),12)a
			!           write(6,*)'#3#:',buf
12 		format(g16.7)
		else if(a.lt.100000000.)then !if(a.lt.1.e-10)then
			! 99999999
			write(buf,14)a
			!         write(6,*)'#4#:',buf
14  	format(g16.8)
		else if(a.lt.1.e9)then !if(a.lt.1.e-10)then
			write(buf,141)a
141 	format(g16.5)
			buf(14:14)=buf(16:16)
			last=14
		else !if(a.lt.1.e-10)then
			write(buf,15)a
			!         write(6,*)'#5#:',buf
15  	format(g16.4)
			buf(14:15)=buf(15:16)
			last=15
		endif !if(a.lt.1.e-10)   4241
		!      write(6,*)'last',last,'buf',buf,'/'
		do i=12,6,-1
			if((buf(i:i).ne.'0'.and.buf(i:i).ne.'.').or.buf(i+1:i+1).eq.'.')exit
		enddo !i=12,6,-1   4280
		do j=1,8
			if(buf(j:j).ne.' ')exit
		enddo !j=1,8   4283
 
		if(buf(13:16).eq.' ')then
			le=i-j+1
			if(le.gt.8.and.buf(j:j).eq.'0')then
				j=j+1;le=le-1
			endif !if(le.gt.8.and.buf(j:j).eq.'0')   4289
			j_chr8b=buf(j:i)
		else !if(buf(13:16).eq.' ')then
			le=i-j+1+ last-13+1
			if(le.gt.8.and.buf(j:j).eq.'0')then
				j=j+1;le=le-1
			endif !if(le.gt.8.and.buf(j:j).eq.'0')   4295
			j_chr8b=buf(j:i)//buf(13:last)
		endif !if(buf(13:16).eq.' ')   4287
		!     if(le.gt.8)  write(6,*)'chr8b:',chr8b,' le:',le
		return
	else !if(a.gt.0.)then
		aa=-a
		if(aa.lt.1.e-10)then
			! 0.999e-10
			write(buf(1:16),20)a
20    format(g16.2)
			!          write(6,*)'#1#:',buf
		else if(aa.le.1.e-4)then !if(aa.lt.1.e-10)then
			! 0.9999e-03  ->.9999e-3
			write(buf,21)a
			!           write(6,*)'#2#:',buf
			buf(15:15)=buf(16:16)
			last=15
21    format(g16.3)
		else if(aa.lt.1.)then !if(aa.lt.1.e-10)then
			write(buf(1:12),211)a
			buf(13:16)=' '
211   format(f12.6)
		else if(aa.le.1000000.)then !if(aa.lt.1.e-10)then
			! 0.0111112   999999.9
			write(buf(1:16),22)a
			!           write(6,*)'#3#:',buf
22 		format(g16.6)
		else if(aa.lt.10000000.)then !if(aa.lt.1.e-10)then
			! 99999999
			write(buf,24)a
			!         write(6,*)'#4#:',buf
24    format(g16.7)
		else if(aa.lt.1.e8)then !if(aa.lt.1.e-10)then
			write(buf,241)a
241   format(g16.4)
			buf(14:14)=buf(16:16)
			last=14
		else !if(aa.lt.1.e-10)then
			write(buf,25)a
			!        write(6,*)'#5#:',buf
25    format(g16.3)
			buf(14:15)=buf(15:16)
			last=15
		endif !if(aa.lt.1.e-10)   4304
		!      write(6,*)'last',last,'buf',buf,'/'
 
		do i=12,6,-1
			if((buf(i:i).ne.'0'.and.buf(i:i).ne.'.').or.buf(i+1:i+1).eq.'.')exit
		enddo !i=12,6,-1   4344
		do j=1,8
			if(buf(j:j).ne.' ')exit
		enddo !j=1,8   4347
		if(buf(j+1:j+1).eq.'0')then
			buf(j+1:j+1)=buf(j:j);j=j+1
		endif !if(buf(j+1:j+1).eq.'0')   4350
 
		if(buf(13:16).eq.' ')then
			le=i-j+1
			if(le.gt.8.and.buf(j:j).eq.'0')then
				j=j+1;le=le-1
			endif !if(le.gt.8.and.buf(j:j).eq.'0')   4356
			j_chr8b=buf(j:i)
		else !if(buf(13:16).eq.' ')then
			le=i-j+1+ last-13+1
			if(le.gt.8.and.buf(j:j).eq.'0')then
				j=j+1;le=le-1
			endif !if(le.gt.8.and.buf(j:j).eq.'0')   4362
			j_chr8b=buf(j:i)//buf(13:last)
		endif !if(buf(13:16).eq.' ')   4354
		!      if(le.gt.8)  write(6,*)'chr8b:',chr8b,' le:',le
		return
	endif !if(a.gt.0.)   4240
 
1 format(f16.7)
2 format(e8.1)
3 format(g13.6)
	!      call adjul2(buf)
	do i=16,1,-1
		if(buf(i:i).eq.'0')then
			buf(i:i)=' '
		else !if(buf(i:i).eq.'0')then
			exit
		endif !if(buf(i:i).eq.'0')   4376
	enddo !i=16,1,-1   4375
 
	if(buf(i:i).eq.'.')then
		buf(i:i)=' '
		i=i-1
	endif !if(buf(i:i).eq.'.')   4383
	le=i
	j_chr8b=buf(1:8)
	return
end function j_chr8b !character*8 function j_chr8  b(a,le)
 
integer function j_countlim(inp,linp,lim)
	character*(*)::inp
	integer ::linp
	character*1 ::lim
	logical ::hipsu,haka
	j_countlim=0
	if(linp.le.0)return
	hipsu=.false.
	haka=.false.
	do j=1,linp
		if(inp(j:j).eq."'")hipsu=.not.hipsu
		if(inp(j:j).eq."[")haka=.true.
		if(inp(j:j).eq."]")haka=.false.
		if(inp(j:j).eq.lim.and..not.haka.and..not.hipsu)j_countlim=j_countlim+1
	enddo !j=1,linp   4401
	return
end function
 
 
function j_nextlim0(inp,ial,lop,limit)
	! like nextlim but returns 0 if limiter not found'
	integer,intent(in):: ial,lop
	character*(*), intent(in):: inp, limit
 
	j_nextlim0=j_nextlim(inp,ial,lop,limit)
	if(j_nextlim0.gt.lop)j_nextlim0=0
	return
end function j_nextlim0 !function j_nextlim0(inp,ial,lop,limit)
 
function j_nextlimset(inp,ial,lop,limset)
	character*(*), intent(in):: inp, limset
	integer, intent(in):: ial,lop
 
	ik=index(inp(ial:lop),limset)
	if(ik.le.0)then
		j_nextlimset=lop+1
	else
		j_nextlimset=ial+ik-1
 
	endif !if(ik.le.0)   4426
 
end function !function j_nextlim(inp,ial,lop,limit)
 
 
 
 
function j_nextlim(inp,ial,lop,limit,inhipsu)
	! Finds the next limiter.
	!*
	! inp   = string to be searched
	! ial   = first character of inp looked at
	! lop   = last character to consider
	! limit = string containing limiters
	! If no limiter character is found function returns lop+1
	! sections between [ and ] are ignored
 
	character*(*), intent(in):: inp, limit
	integer, intent(in):: ial,lop
	logical,optional, intent(in):: inhipsu
 
	logical haka,hipsu,ishipsu
	!************************************************
	ishipsu=.false.
	if(present(inhipsu))ishipsu=inhipsu
	le=len(limit)
	linp=len(inp)
 
	if(ishipsu)then
		do  i=ial,lop
			do  j=1,le
				if(inp(i:i).eq.limit(j:j))then
					j_nextlim=i
					return
				endif !if(inp(i:i).eq.limit(j:j))   4462
			enddo ! j=1,le   4461
 
		enddo ! i=ial,lop   4460
		j_nextlim=lop+1
		return
	endif !if(ishipsu)   4459
 
 
	haka=.false.
	hipsu=.false.
 
	if(ial.le.0.or.lop.gt.linp)then
		write(6,*)'*j* j_nextlim ial should be >=0 and lop<=linp, ial=',ial,' lop=',lop,' linp=',linp
		write(6,*)'inp:',inp(1:linp)
		j_err=.true.
		j_nextlim=lop+1
		return
 
	endif !if(ial.le.0.or.lop.gt.linp)   4477
	do  i=ial,lop
		if(inp(i:i).eq."'")then
			hipsu=.not.hipsu
			cycle
		endif !if(inp(i:i).eq."'")   4486
		if(inp(i:i).eq.'['.and..not.hipsu)then
			haka=.true.
			cycle
		endif !if(inp(i:i).eq.'['.and..not.hipsu)   4490
		if(haka)then
			if(inp(i:i).eq.']')haka=.false.
			cycle
		endif !if(haka)   4494
		if(haka.or.hipsu)cycle
		do  j=1,le
			if(inp(i:i).eq.limit(j:j)) goto 3
		enddo ! j=1,le   4499
	enddo ! i=ial,lop   4485
	!1 continue !do 1 i=ial,lop
	i=lop+1
3 j_nextlim=i
	if(haka)then
		write(6,*)ial,lop,'*unclosed [ in ', inp(ial:lop),(ichar(inp(j:j)),j=ial,lop)
		j_err=.true.
	endif !if(haka)   4506
	return
end function j_nextlim !function j_nextlim(inp,ial,lop,limit)
 
function j_prevlim(inp,ial,limit)
	! Finds the previous limiter.
	!*
	! inp   = string to be searched
	! ial   = first character of inp looked at
	! lop   = last character to consider
	! limit = string containing limiters
	! If no limiter character is found function returns lop+1
	! sections between [ and ] are ignored
 
	character*(*), intent(in):: inp, limit
	integer, intent(in):: ial
 
	logical haka,hipsu
	!************************************************
	le=len(limit)
	haka=.false.
	hipsu=.false.
	do  i=ial,1,-1
		if(inp(i:i).eq."'")then
			hipsu=.not.hipsu
			cycle
		endif !if(inp(i:i).eq."'")   4532
 
		if(inp(i:i).eq.']'.and..not.hipsu)then
			haka=.true.
			cycle
		endif !if(inp(i:i).eq.']'.and..not.hipsu)   4537
		if(haka)then
			if(inp(i:i).eq.'[')haka=.false.
			cycle
		endif !if(haka)   4541
		if(haka.or.hipsu)cycle
		do  j=1,le
			if(inp(i:i).eq.limit(j:j)) goto 3
		enddo ! j=1,le   4546
		!2   continue !do 2 j=1,le
	enddo ! i=ial,1,-1   4531
	! 1 continue !do 1 i=ial,1,-1
	i=0
3 j_prevlim=i
	if(haka)then
		write(6,*)ial,lop,'*unclosed [ in ', inp(ial:lop),(ichar(inp(j:j)),j=ial,lop)
		j_err=.true.
	endif !if(haka)   4554
	return
end function j_prevlim !function j_prevlim(inp,ial,limit)
 
! function j_nextlimword(inp,ial,lop,limit)  j_enextword
! character*(*), intent(in):: inp, limit
! integer, intent(in):: ial,lop
 
! ! Finds the next limiter.
! !*
! ! inp   = string to be searched
! ! ial   = first character of inp looked at
! ! lop   = last character to consider
! ! limit = string containing limiter word
! ! If no limiter character is found function returns lop+1
! ! sections between [ and ] are ignored
 
 
! logical haka
! !************************************************
 
! haka=.false.
! le=len(limit)
! le1=le-1
! do 1 i=ial,lop
! if(inp(i:i).eq.'[')then
! haka=.true.
! goto 1
! endif !if(inp(i:i).eq.'[')then
! if(haka)then
! if(inp(i:i).eq.']')haka=.false.
! goto 1
! endif !if(haka)then
! do 2 j=1,le
! if(inp(i:i+le1).eq.limit) goto 3
! 2   continue !do 2 j=1,le
! 1 continue !do 1 i=ial,lop
! i=lop+1
! 3 j_nextlim=i
! if(haka)then
! write(6,*)ial,lop,'*unclosed [ in ', inp(ial:lop),(ichar(inp(j:j)),j=ial,lop)
! j_err=.true.
! endif !if(haka)then
! return
! end function j_nextlimword !function j_nextlim(inp,ial,lop,limit)
 
function j_nextlim2(inp,ial,lop,limit) !likem nextlim but [] sequences are not ignored
	! Finds the next limiter.
	!*
	! inp   = string to be searched
	! ial   = first character of inp looked at
	! lop   = last character to consider
	! limit = string containing limiters
	! If no limiter character is found function returns lop+1
	! sections between [ and ] are NOT ignored
 
	character*(*), intent(in):: inp, limit
	integer, intent(in):: ial,lop
 
	!  logical haka
	!************************************************
 
	!	haka=.false.
	le=len(limit)
	do  i=ial,lop
		! if(inp(i:i).eq.'[')then
		! haka=.true.
		! goto 1
		! endif
		! if(haka)then
		! if(inp(i:i).eq.']')haka=.false.
		! goto 1
		! endif
		do  j=1,le
			if(inp(i:i).eq.limit(j:j)) goto 3
		enddo ! j=1,le   4630
		!2   continue !do 2 j=1,le
	enddo ! i=ial,lop   4621
	! 1 continue !do 1 i=ial,lop
	i=lop+1
3 j_nextlim2=i
	! if(haka)then
		! write(6,*)'*unclosed [ in ', inp(ial:lop)
		! j_err=.true.
	! endif
	return
end function j_nextlim2 !function j_nextlim2(inp,ial,lop,limit)
 
 
!20141208 nextword
function j_nextword(inp,ial,lop,limit)
	! Finds the next limiter.
	!*
	! inp   = string to be searched
	! ial   = first character of inp looked at
	! lop   = last character to consider
	! limit = string containing limiters
	! If no limiter character is found function returns lop+1
	! sections between [ and ] are ignored
	!	use errmod
	character*(*), intent(in):: inp
	integer, intent(in):: ial,lop
	character*(*),intent(in):: limit
 
	! logical haka
	!************************************************
	!haka=.false.
	le=len_trim(limit)
	!###TESTAUS###
	!	write(6,*)'nextword <526> ial,le,lop,limit ', ial,le,lop,'/',limit(1:le),'/'
	if(ial+le-1>lop) then
		j_nextword = lop+1
		return
	endif !if(ial+le-1>lop)   4667
 
	i = index(inp(ial:lop),limit(1:le))
	!###TESTAUS###
	!write(6,*)'nextword <534> i, inp(ial:lop),limit ',inp(ial:lop),' ',limit
	if(i>0) then
		j_nextword = ial+i-1
	else !if(i>0) then
		j_nextword = lop+1
	endif !if(i>0)   4675
	return
end function j_nextword !function j_nextword(inp,ial,lop,limit)
 
 
!20141208 jreplace
subroutine j_jreplace(inp,ial,lop,cout,lcout,word1,lword1,word2,lword2)
	character*(*), intent(in):: inp,word1,word2
	character*(*), intent(out):: cout
	integer, intent(in):: ial, lop,lword1,lword2
	integer, intent(out):: lcout
 
	!###TESTAUS###
	!write(6,*)'jreplace <549> ial, lop, inp(ial:lop) ', ial, lop, inp(ial:lop)
	!write(6,*)'jreplace <549> inp(1:lop) ',inp(1:lop)
 
	ialin = ial
	ialout = ialin
	if(ialin>1) cout(1:ialin-1)=inp(1:ialin-1)
 
1	nextw = j_nextword(inp,ialin,lop,word1(1:lword1))
	npit = nextw - ialin
	cout(ialout:ialout+npit-1)=inp(ialin:nextw-1)
	!###TESTAUS###
	!write(6,*)'jreplace <552> nextw, npit, cout ', nextw, npit, cout(ialout:ialout+npit-1)
	ialout = ialout+npit
	if(nextw<lop) then
		cout(ialout:ialout+lword2-1)=word2(1:lword2)
		!###TESTAUS###
		!write(6,*)'jreplace <557> ialout, cout ', ialout, cout(ialout:ialout+lword2-1)
		ialout = ialout + lword2
		ialin = ialin + lword1 + npit
		!if(nextw<lop) goto 1
		goto 1
	endif !if(nextw<lop)   4705
	lcout = ialout - 1
	return
end subroutine j_jreplace !subroutine j_jreplace(inp,ial,lop,cout,lcout,word1,lword1,word2,lword2)
 
 
function j_nextrp(inp,ial,lop)
	!* Finds the next balanced parenthesis,including []
	!**
	!* inp   = string to be searched
	!* ial   = first character of inp containing either ( or [
	!* lop   = last character to consider
	!* If no comma match is found function returns lop+1
	character*(*), intent(in):: inp
	integer, intent(in):: ial,lop
	!************************************************
	isulv=0
	isulr=0
	markkuv=0
	markkur=0
	do  i=ial,lop
		if(inp(i:i).eq.'(')isulv=isulv+1
		if(inp(i:i).eq.')')isulr=isulr+1
		if(inp(i:i).eq.'[')markkuv=markkuv+1
		if(inp(i:i).eq.']')markkur=markkur+1
		if(isulv.eq.isulr.and.markkur.eq.markkuv) goto 3
	enddo ! i=ial,lop   4733
	!1 continue !do 1 i=ial,lop
	i=lop+1
3 j_nextrp=i
	return
end function j_nextrp !function j_nextrp(inp,ial,lop)
 
 
subroutine j_adjul2(inp)
	! Adjusts a character variable to the left, i.e. removes initial blanks
	!*
	! and tabs.
	character*(*), intent(inout):: inp
	!***********************************************
 
	lop=len(inp)
	do  i=1,lop
		! inp(i:i).ne.' '.and.inp(i:i).ne.char(9) changed for version Dec/1995
		if(ichar(inp(i:i)).gt.32)then
			if(i.gt.1)  inp=inp(i:)
			return
		endif !if(ichar(inp(i:i)).gt.32)   4757
	enddo ! i=1,lop   4755
	!1 continue !do 1 i=1,lop
	return
end subroutine j_adjul2 !subroutine j_adjul2(inp)
 
 
character*5 function j_chi5(i,il)
	!* Returns integer i as character*5.
	!**
	!* If il=1, then the output is justified to the left.
	!* Because the function uses WRITE statement, it cannot be used in
	!* WRITE statements (recursive WRITEs causes trouble).
	integer, intent(in):: i,il
	!************************************************
 
	character*5 buf
 
	write(buf,1)i
1 format(i5)
	if(il.eq.1)call j_adjul2(buf)
	j_chi5=buf
	return
end function j_chi5 !character*5 function j_chi5(i,il)
 
double precision function j_cotan(x)  !%%math cotan not available in all systems
	double precision,intent(in)::x
	j_cotan=dtan(1.570796327d0-x)
	return
end function !double precision function j_cotan(x)
 
 
!*=chr10=== file jlpsub.src =============================================
character*10 function j_chr10(a)
	!* Returns double precision a as character*10 variable.
	!**
	!* Because the function uses WRITE statement, it cannot be used in
	!* WRITE statements (recursive WRITEs causes trouble).
	!*IF DOUBLE REPLACE DOUBLE PRECISION
	DOUBLE PRECISION, intent(in)::  a
	!*END
	!************************************************
 
	real zero
	character*20 buf
	data zero/0./
 
	if(abs(a).gt.1.e-5.or.a.eq.zero)then
		if(a.gt.1.d13)then
			j_chr10='   INF'
			return
		else if(a.lt.-1.d13)then !if(a.gt.1.d13)then
			j_chr10='  -INF'
			return
		else !if(a.gt.1.d13)then
			write(buf,1)a
			if(buf(1:3).eq.'***')write(buf,2)a
		endif !if(a.gt.1.d13)   4808
	else !if(abs(a).gt.1.e-5.or.a.eq.zero)then
		write(buf,2)a
		if(buf(1:3).eq.'***')write(buf,3)a
	endif !if(abs(a).gt.1.e-5.or.a.eq.zero)   4807
1 format(f20.9)
2 format(e10.5)
3 format(e10.4)

	if(buf(1:1).eq.'0')buf(1:1)=' '
 
	call j_adjul2(buf)
	j_chr10=buf
	return
end function j_chr10 !character*10 function j_chr10(a)
 
 
!******************************************************************
 
 
subroutine j_gayainit(iob,io)
 
	integer, intent(in):: iob,io
 
	npar=j_nargopt(iob,io,j_mpar) !number of parameters given in par-> option
	!	j_g_maxvar=j_nargopt(iob,j_msubread) !!n umber of variables given in subread-> option
	j_g_maxvar=j_nread(2)  !datansubread_
 
	!	write(6,*)'<47477 ',j_g_maxvar,j_nread(2),npar
 
	if(npar.ge.1)then
		if(npar.eq.1)then
			! it is assumed that if ngpar is given tehn also g_npvar must be give
			write(6,*)'*there must be two par-parameters'
			j_err=.true.
			return
		endif !if(npar.eq.1)   4848
		lip=j_linkoption(iob,io,j_mpar)
		j_g_ngvar=j_v(j_o(iob)%i(lip+1) ) ! value of first parameter in par->
		j_g_npvar=j_v(j_o(iob)%i(lip+2) ) !value of second
 
	else !if(npar.ge.1)then
		! default values
		j_g_ngvar=8
		j_g_npvar=93
	endif !if(npar.ge.1)   4847
	j_g_nvar=j_g_npvar+j_g_ngvar
	j_nper=(j_g_maxvar-j_g_ngvar)/j_g_npvar
	j_g_nvarre=j_g_maxvar-j_g_nvar   ! parameter (g_nvarre=g_maxvar-g_nvar)
 
	! g_maxvar=g_npvar*nper+g_ngvar     !parameter (g_maxvar=g_npvar*maxper+g_ngvar)
	write(6,*)'gaya: j_g_ngvar,j_g_npvar, nper ',j_g_ngvar,j_g_npvar,j_nper
	!	write(6,*)'g_ngvar,g_npvar,nper,j_g_maxvar=',j_g_ngvar,j_g_npvar,j_nper,j_g_maxvar
 
	if(j_g_maxvarold.lt.j_g_maxvar)then
		if(allocated(j_g_xx))deallocate(j_g_xx,j_g_p,j_g_var,j_g_ixl)
		allocate (j_g_xx(1:j_g_maxvar),j_g_p(1:j_g_maxvar),j_g_var(1:j_g_maxvar))
		allocate (j_g_ixl(1:j_g_maxvar))
		j_g_maxvarold=j_g_maxvar
	endif !if(j_g_maxvarold.lt.j_g_maxvar)   4871
end subroutine !subroutine j_gayainit(iob)
 
 
!     subroutine gayainit(readxl)
! entry gayax(nuo2,is,readxl,v)
subroutine j_gayax(nuo2,is)  !,readxl,v)
	!use vmod
 
	integer, intent(in):: nuo2  ! unit used for reading
	!integer, intent(in):: readxl(0:*)  !this is the variable list given in subread-> option
	integer, intent(in):: is  ! the number of schedule we are now reading
	!real, dimension(*), intent(out):: v ! read varaibles should be put into v so that
	!          v(readxl(1)) gets value of variable readxl(1)
 
	!there can be additional arguments e.g. there can be additional variable lists
	! which are given similarly as subread-> variables
	! also the varaibles obtained with 'readfirst->' can be given as arguments
	! there can be additiona options which give numeric values
	! e.g. if we agree on gayapar->(par1,par2,par3) option, I can transmit
	! par1,par2 and par3 as integer or real constants
 
	save
 
	!* g_npvar = number of variables for each period
	!*maxper = maximum number of periods
	!* g_ngvar = number of general variables not defined for each period
	!*g_nvar =g_ngvar+g_npvar
	!     parameter (maxper=30)
	!    parameter (g_ngvar=8)
	!      parameter (g_npvar=49)    ! full modell old
	!   parameter (g_npvar=93)    ! full modell
	!     dimension g_xx(g_npvar*maxper+g_ngvar),p(g_npvar*maxper+g_ngvar)! Data for
	!* all periods on one line
	!      parameter (g_nvar=g_npvar+g_ngvar)
	!      parameter (g_maxvar=g_npvar*maxper+g_ngvar)
	!    parameter (g_nvarre=g_maxvar-g_nvar)
	!* xnam names of variables
	!*xnam1 names of periodic variables without period extension
	!        character*32 xnam(g_maxvar),xnam1(g_npvar)
	!* lenam = lengths of the names of periodic variables without period extension
	!      integer lenam(g_npvar)
	!*g_ixl link list to the variale numbers in GAYA, nonperiodic variables come first
	!      integer g_ixl(g_maxvar)
	data rt,ft,tl,rt2,ft2,tl2/0.,0.,0.,0.,0.,0./
	!*areav tells if variable is /ha-variable
	!      logical areav(g_maxvar)
 
	!C entry mopen varten:
	!C      character*(*) path,name
	! C      character*5 chi5
	!     character*80 fil
	!     character*5 chi
	!      logical yes,lxnam1,lxnam2
	!* entry mgetc and mgetx:
	!*variables for JLP
	!C      dimension x(*)
 
	!* entry mgetx:
	!      dimension var(g_maxvar)
 
	!     return
	!     entry gayax(nuo2,is,readxl,v)
	t1=secnds(0.)
	call cpu_time(c1)
	read(NUo2)nsper,ip,(j_g_p(ii),ii=1,ip)
	!	  write(6,*)nsper,ip,(g_p(ii),ii=1,ip)
	t2=secnds(0.)
	call cpu_time(c2)
	call j_f_pca_gaya(j_g_xx,ixx,j_g_p,ip)
	t3=secnds(0.)
	call cpu_time(c3)
	rt=rt+t2-t1
	ft=ft+t3-t2
	rt2=rt2+c2-c1
	ft2=ft2+c3-c2
	If(nsper.eq.1)slnr=is   !v(3)   !variable 's'
	j_g_var(1)=slnr
	j_g_var(2)=nsper*1.
	Do  j=3,j_g_ngvar
		j_g_var(j)=j_g_xx(j-2) !Do 110 j=3,j_g_ngvar
	enddo ! j=3,j_g_ngvar   4955
	!C Variable slnr and nsper is not in g_xx-array (out-packed version of p)
	!C This means that g_ngvar-2 out of g_ngvar non-periodic x-variables
	!C is read 'directly'. Thus, the 111 loop has to start from
	!C g_ngvar-1 (not g_ngvar+1)
	ij=j_g_ngvar
	do  j=j_g_ngvar-1,ixx
		ij=ij+1
		j_g_var(ij+((nsper-1)*j_g_npvar))=j_g_xx(j) !Do 111 j=j_g_ngvar-1,ixx
	enddo ! j=j_g_ngvar-1,ixx   4963
 
	!Chfh      ij=nper*g_npvar+g_ngvar
	!Chfh      read(NUo2)(var(j),j=g_ngvar+1,ij),(var(j),j=1,g_ngvar)
	!CC
	!        area=v(ivarea)
	!     if(is.eq.1)write(6,*)'area',area,'nverdi',g_var(4)
	! do 5 i=1,readxl(0)
	! ij=  i  !      g_ixl(i)
	! apu=j_g_var(ij)
	! !         if(areav(ij) )apu=area*apu
	! 5 V(readxl(i))=apu !do 5 i=1,readxl(0)
	j_v(j_d(2)%readv)=j_g_var(1:j_nread(2))
	!       if(is.eq.1)write(6,*)'area2',area,'nverdi',v(readxl(4))
	!*         if(iperk.ne.12456)write(6,*)(v(readxl(jjj)),jjj=1,
	!*     2   readxl(0) )
	!*        iperk=12456
	t4=secnds(0.)
	call cpu_time(c4)
	tl=tl+t4-t3
	tl2=tl2+c4-c3
	return
 
	entry timegaya()
	write(6,*)'reading time ',rt,' f_p time ',ft, 'rest ',tl
	write(6,*)'reading ctime ',rt2,' f_p ctime ',ft2, 'crest ',tl2
	return
 
end subroutine j_gayax !subroutine j_gayax(nuo2,is)
 
 
subroutine j_f_pca_gaya(x,ix,p,ipp)
	!*       ----------------
	save
 
	real,dimension(*), intent(out):: x
	real,dimension(*), intent(in):: p
	integer, intent(out):: ix
	integer, intent(in):: ipp
 
	real xx
	character*4 cc
	character*3 cw(5)
	equivalence (xx,cc)
 
	data cw/'***','*+*','+*+','<*>','>*<'/
 
	jz=0
	if(ipp.eq.1)goto 31
 
	!       get id (default = '***')
	do 3 i=1,5
		xx=p(ipp)
		if(cc(1:3).eq.cw(i))then
			!           omit last word of compressed record, if id found
			ip=ipp-1
			ji=i
			!           compress number
			jz=ichar(cc(4:4))
			goto 4
		endif !if(cc(1:3).eq.cw(i))   5019
3 continue !3 i=1,5   5017
31 continue
	ip=ipp
	ji=1
	!      here unkno compress number
	jz=-1
4 continue

	kz=0
	ix=0
	do 1 i=1,ip
		xx=p(i)
		if(jz.ne.0.and.cc(1:3).eq.cw(ji))then
			nz=ichar(cc(4:4))
			do  j=1,nz
				ix=ix+1
				x(ix)=0. !do 2 j=1,nz
			enddo ! j=1,nz   5041
			!           decompress number
			kz=kz+1
		else !if(jz.ne.0.and.cc(1:3).eq.cw(ji))then
			ix=ix+1
			x(ix)=p(i)
		endif !if(jz.ne.0.and.cc(1:3).eq.cw(ji))   5039
1 continue !1 i=1,ip   5037

	!       compare decompress and compress number, if possible
	if(jz.gt.0.and.jz.lt.255)then
		if(jz.ne.kz)then
			write(*,*)' F_PCA decompress error: jz .ne. kz',jz,kz
			stop 'F_PCA'
		endif !if(jz.ne.kz)   5055
	endif !if(jz.gt.0.and.jz.lt.255)   5054
	return
end subroutine j_f_pca_gaya !subroutine j_f_pca_gaya(x,ix,p,ipp)
!end of gaya sub *****************************************
 
 
!**********mela
subroutine melainit()   !minit in jlp
	return
end subroutine !subroutine melainit()
 
 
subroutine melax(nuo2,is,readxl,v)
	!use vmod
 
	integer nuo2  ! unit used for reading
	integer readxl(0:*)  !this is the variable list given in subread-> option
	integer is  ! the number of schedule we are now reading
	dimension v(*) ! read varaibles should be put into v so that
	!          v(readxl(1)) gets value of variable readxl(1)
 
	!there can be additional arguments e.g. there can be additional variable lists
	! which are given similarly as subread-> variables
	! also the varaibles obtained with 'readfirst->' can be given as arguments
	! there can be additiona options which give numeric values
	! e.g. if we agree on gayapar->(par1,par2,par3) option, I can transmit
	! par1,par2 and par3 as integer or real constants
 
	save
 
	return
end subroutine melax !subroutine melax(nuo2,is,readxl,v)
 
 
 
double precision function j_rlinter(x0,x1,y0,y1,x)  !linear interpolation
	double precision, intent(in):: x0,x1,y0,y1,x
	if(x1.eq.x0)then
		write(6,*)'x0==x1 in interpolation'
		j_err=.true.
		return
	endif !if(x1.eq.x0)   5096
 
	j_rlinter=y0+(x-x0)*(y1-y0)/(x1-x0)
	return
end function j_rlinter !double precision function j_rlinter(x0,x1,y0,y1,x)
 
 
double precision function j_bilin(xa,xy,za,zy,aa,ay,ya,yy,x,z)
	double precision, intent(in):: xa,xy,za,zy,aa,ay,ya,yy,x,z
	double precision t,u
	t=(x-xa)/(xy-xa)
	u=(z-za)/(zy-za)
	!	j_bilin=(1.-t)*(1.-u)*aa+t*(1-u)*ya+t*u*ay+(1-t)*u*yy
	j_bilin=(1.-t)*(1.-u)*aa+t*(1.-u)*ya+t*u*yy+(1.-t)*u*ay
	return
end function j_bilin !double precision function j_bilin(xa,xy,za,zy,aa,ay,ya,yy,x,z)
 
 
double precision function j_sqrtt(x)
	double precision, intent(in):: x
 
	if(x.ge.0)then
		j_sqrtt=sqrt(x)
	else !if(x.ge.0)then
		j_sqrtt=-sqrt(-x)
	endif !if(x.ge.0)   5121
	return
end function j_sqrtt !double precision function j_sqrtt(x)
 
 
!* numeric linear integration
double precision function j_flini(np,x,y,r1,r2)
	integer, intent(in):: np
	double precision, intent(in):: r1,r2
	double precision,dimension(*), intent(in):: x,y
 
	double precision sum,ya,yy
	!      write(6,*)'rajat,n,',r1,r2,np
	!    write(6,*)'x',x(1:np)
	!     write(6,*)'y',y(1:np)
	!*       pause
 
	if(r1.ge.r2)then
		j_flini=0.
		return
	endif !if(r1.ge.r2)   5142
	if(np.le.1)then
		j_flini=y(1)*(r2-r1)
		return
	end if !if(np.le.1)   5146
	sum=0.d0
	!search first x.ge. r1
	do  i=1,np
		if(x(i).gt.r1)goto 2 !do 1 i=1,np
	enddo ! i=1,np   5152
	!*       write(6,*)'satana'
	j_flini=0.
	return
2 if(i.eq.1)i=2

	ya=y(i-1)+(r1-x(i-1))*(y(i)-y(i-1))/(x(i)-x(i-1))
	if(r2.le.x(i))then
		yy=y(i-1)+(r2-x(i-1))*(y(i)-y(i-1))/(x(i)-x(i-1))
		j_flini=(r2-r1)*0.5*(ya+yy)
		!   write(6,*)'flini ',flini
		return
	endif !if(r2.le.x(i))   5161
	sum=(x(i)-r1)*0.5*(ya+y(i))
	ia=i
	!*         write(6,*)'**sum,ia',sum,ia
	!    end if
	do i=ia,np-1
		if(x(i+1).ge.r2.or.i.eq.np-1)then
			yy=y(i)+(r2-x(i))*(y(i+1)-y(i))/(x(i+1)-x(i))
			sum=sum+(r2-x(i))*0.5*(yy+y(i))
			goto 8
		else !if(x(i+1).ge.r2.or.i.eq.np-1)then
			sum=sum+(x(i+1)-x(i))*0.5*(y(i+1)+y(i))
		endif !if(x(i+1).ge.r2.or.i.eq.np-1)   5172
	enddo !i=ia,np-1   5171
 
8 j_flini=sum
	! write(6,*)'flini ',flini
end function j_flini !double precision function j_flini(np,x,y,r1,r2)
 
 
subroutine j_startfig(iob,io,update) !defines a figure object
	integer, intent(in) ::iob
	integer, intent(in) ::io
	logical,intent(in),optional::update  !in show the options can be updated
	logical::nupdate
	logical ::pp=.false.
	nupdate=.true.
	if(present(update))then
		nupdate=.not.update
	endif !if(present(update))   5193
	!	p=j_v(j_ivdebug).gt.j_0
	!write(6,*)'<666nupdate ',nupdate
	if(nupdate)then
		call j_startfunction(iob,io,0,j_gpnarg,j_gparg,j_gpiout)
		!	write(6,*)'gpiout',j_gpiout
		if(j_gpiout.gt.j_namedv)then
			!io=io+narg+3
			j_gpnewio=io+j_gpnarg+3
			!		write(6,*)'newio',j_o(iob)%i(j_gpnewio:j_gpnewio+8)
			if(j_o(iob)%i(j_gpnewio).eq.3)then !setelem
				ili=j_o(iob)%i(j_gpnewio+2)
				if(j_otype(ili).ne.j_iplist)then
					write(6,*)'figure can be stored only in named object'
					j_err=.true.;return
				endif !if(j_otype(ili).ne.j_iplist)   5207
				if(j_o(iob)%i(j_gpnewio+1).ne.2)then
					write(6,*)'LIST() can have here only one argument'
					j_err=.true.;return
				endif !if(j_o(iob)%i(j_gpnewio+1).ne.2)   5211
				iel=j_v( j_o(iob)%i(j_gpnewio+3))
				j_gpiout=j_o(ili)%i2(iel)
				!			write(6,*)'out',j_gpiout
			else !if(j_o(iob)%i(j_gpnewio).eq.3)then
				write(6,*)'figure cannot be temporary object'
				j_err=.true.;return
			endif !if(j_o(iob)%i(j_gpnewio).eq.3)   5205
		endif !if(j_gpiout.gt.j_namedv)   5201
 
		!		write(6,*)'<66iout,append,j_otype(iout)',iout,append,j_otype(iout)
 
		j_gpshow=j_isopt(iob,io,j_mshow,.true.)
 
		!	write(6,*)'show',j_gpshow
		!	endif
		! call j_getoption_index(iob,io,j_mshow,-1,1,j_ipreal,.false.,.false.,noptarg,j_optarg0)
		! if(noptarg.le.0)then
		! j_gpshow=.true.
		! else !if(noptarg.le.0)then
		! j_gpshow=j_v(j_optarg0(1)).gt.j_0
		! endif !if(noptarg.le.0)then
 
		call j_getoption_index(iob,io,j_mmarksize,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(j_err)return
		if(noptarg.le.0)then
			j_gpps(5:5)='1'
		else
			isi=j_v(j_optarg0(1))
			if(isi.le.0.or.isi.ge.10)then
				write(6,*)'illegal marksize->',isi
				j_err=.true.
				return
			endif !if(isi.le.0.or.isi.ge.10)   5243
			j_gpps(5:5)=char(48+isi)
		endif !if(noptarg.le.0)   5239
		call j_getoption_index(iob,io,j_maxes,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(j_err)return
		if(noptarg.gt.0)then
 
			j_gpaxes=j_v(j_optarg0(1))
		else
			j_gpaxes=11
		endif !if(noptarg.gt.0)   5252
 
		j_gpappend=j_linkoption(iob,io,j_mappend,clear=.true.).ge.0
		!	call j_defchar(j_gpiout,'.jfig',ivout)
		if(j_otype(j_gpiout).ne.j_ipfigure)then
			if(j_otype(j_gpiout).ne.j_ipreal)call j_del(j_gpiout)
			allocate(j_o(j_gpiout)%txt(1:j_gplines))
			allocate(j_o(j_gpiout)%i2(1:j_gplines))
			j_o(j_gpiout)%i2(1:j_gplines)=0
			allocate(j_o(j_gpiout)%i(1:11))
			j_o(j_gpiout)%i(2)=j_gplines
			j_o(j_gpiout)%i(4)=j_gpplots
			!number of functions
			! i(5) number of set lines
			allocate(j_o(j_gpiout)%d(1:10))  ! given xmin xmax xmin2 xmax2 ymin ymax obtained
			j_otype(j_gpiout)=j_ipfigure
			j_gpappend=.false.
		end if !if(j_otype(j_gpiout).ne.j_ipfigure)   5261
		if(.not.j_gpappend)then
			j_o(j_gpiout)%d(1:6)=1.7d20
			j_o(j_gpiout)%d(7)=1.7d20
			j_o(j_gpiout)%d(8)=-1.7d20
			j_o(j_gpiout)%d(9)=1.7d20
			j_o(j_gpiout)%d(10)=-1.7d20
			!		j_o(j_gpiout)%i(1)=0  !number of lines
			!	j_o(j_gpiout)%i(2)=j_gplines !number of allocated lines
			j_o(j_gpiout)%i(3)=0  !number plot commands
			!	j_o(j_gpiout)%i(4)=j_gpplots  !number of availabl plotcommands
			j_o(j_gpiout)%i(5:11)=0  !number of datasets in the jfig file and number of data blocks in different datasets
			!	j_o(j_gpiout)%i(11) number of pt in plot commant
			!	j_otype(j_gpiout)=j_ipfigure
			!		j_o(j_gpiout)%i(5:10)=0  !number of subfigures first set (set=0) reserved for plotyx and draw
			j_o(j_gpiout)%i2(1:j_o(j_gpiout)%i(2))=0  !line lengths
			j_o(j_gpiout)%txt(1:j_o(j_gpiout)%i(2))=' '
			j_gpletitle=0
			j_gplexlabel=0
 
			j_gpleylabel=0
			j_gppoints=150
			if(j_gpletitle.eq.0.and.j_gpaxes.gt.0)&
				call j_putfig0(1,'set title "'//j_buffer(1:j_lentrim(j_buffer))//'"')
 
			if(j_gplexlabel.eq.0.and.j_gpaxes.gt.0)call j_putfig0(2,'set xlabel "x-var"')
			!
			if(j_gpleylabel.eq.0.and.j_gpaxes.gt.0)call j_putfig0(3,'set ylabel "y-var"')
			call j_putfig0(4,'#set xrange [min:max]')
			call j_putfig0(5,'#set yrange [min:max]')
			nres=(j_o(j_gpiout)%i(4)+1)/2 !i4 reserved plot command rows
			j_o(j_gpiout)%i(1)=j_gpbas+nres
			!		write(6,*)'<888hhd55',j_o(j_gpiout)%i(1)
		endif !if(.not.j_gpappend)   5275
 
		!call j_startfunction(iob,io,iptype,narg,arg,ivout)
 
		call j_getoption_index(iob,io,j_mpoints,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(noptarg.gt.0)j_gppoints=j_v(j_optarg0(1))
 
	endif !if(nupdate)   5198
	j_gpcontinue=j_isopt(iob,io,j_mcontinue)
	!	write(6,*)'<49 gpcontinue ',j_gpcontinue
	! call j_getoption_index(iob,io,j_mcontinue,-1,1,j_ipreal,.false.,.false.,noptarg,j_optarg0)
	! if(noptarg.lt.0)then
	! j_gpcontinue=.false.
	! elseif(noptarg.eq.0)then !if(noptarg.lt.0)then
	! j_gpcontinue=.true.
	! else !if(noptarg.lt.0)then
	! j_gpcontinue=j_v(j_optarg0(1)).gt.j_0
	! endif !if(noptarg.lt.0)then
	if(nupdate)then
		call j_getoption_index(iob,io,j_mset,-1,1,j_ipreal,.false.,noptarg,j_optarg0)
 
		if(noptarg.lt.0)then
			j_gpset=0
		elseif(noptarg.eq.0)then !if(noptarg.lt.0)then
			j_gpset=1
		else !if(noptarg.lt.0)then
			j_gpset=j_v(j_optarg0(1))
			if(j_gpset.lt.0.or.j_gpset.gt.5)then
				write(6,*)'set-> should be between 1 and 5'
				j_err=.true.;return
			endif !if(j_gpset.lt.0.or.j_gpset.gt.5)   5334
		endif !if(noptarg.lt.0)   5328
 
		call j_getoption_index(iob,io,j_mmark,-1,1,0,.true.,noptarg,j_optarg0)
		if(noptarg.le.0)then
			j_gpmark=j_ivzero
 
		else !if(noptarg.le.0)then
			j_gpmark=j_optarg0(1)
			if(j_otype(j_gpmark).ne.j_ipreal.and.j_otype(j_gpmark).ne.j_ipchar)then
				write(6,*)'mark-> must be REAL or CHAR'
				j_err=.true.;return
			endif !if(j_otype(j_gpmark).ne.j_ipreal.and.j_otype(j_gpmark).ne.   5346
		endif !if(noptarg.le.0)   5341
 
		call j_getoption_index(iob,io,j_mwidth,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(noptarg.le.0)then
			j_gpwidth=1
		else !if(noptarg.le.0)then
			j_gpwidth=j_v(j_optarg0(1))
		endif !if(noptarg.le.0)   5353
		j_gplw(5:5)=char(48+j_gpwidth)
 
		call j_getoption_index(iob,io,j_mmark,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(noptarg.le.0)then
 
		else !if(noptarg.le.0)then
			j_gpmark=j_optarg0(1)  !0  no mark
		endif !if(noptarg.le.0)   5361
 
		call j_getoption_index(iob,io,j_mcolor,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(noptarg.le.0)then
			j_gpicolor=1
		else !if(noptarg.le.0)then
			j_gpicolor=j_v(j_optarg0(1))
			if(j_gpicolor.gt.8)j_gpicolor=1
		endif !if(noptarg.le.0)   5368
 
		call j_getoption_index(iob,io,j_mstyle,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(noptarg.le.0)then
			j_gpstyle=1
		else !if(noptarg.le.0)then
			j_gpstyle=j_v(j_optarg0(1))
		endif !if(noptarg.le.0)   5376
		j_gplt(5:5)=char(48+j_gpstyle)
 
		call j_getoption_index(iob,io,j_mlabel,-1,1,j_ipchar,.true.,noptarg,j_optarg0)
		if(noptarg.le.0)then
			j_gplelabel=0
		else !if(noptarg.le.0)then
			call j_getchar(j_optarg0(1),j_gplabel,j_gplelabel)
		endif !if(noptarg.le.0)   5384
	endif !if(nupdate)   5325
 
	call j_getoption_index(iob,io,j_mtitle,-1,1,j_ipchar,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.gt.0)&
		call j_getchar(j_optarg0(1),j_gptitle,j_gpletitle)
 
 
	call j_getoption_index(iob,io,j_mxlabel,-1,1,j_ipchar,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.gt.0)&
		call j_getchar(j_optarg0(1),j_gpxlabel,j_gplexlabel)
	!	write(6,*)'<77xlabel',j_gpxlabel(1:j_gplexlabel)
 
 
	call j_getoption_index(iob,io,j_mylabel,-1,1,j_ipchar,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.gt.0)&
		call j_getchar(j_optarg0(1),j_gpylabel,j_gpleylabel)
 
 
 
	j_gpix=0
	call j_getoption_index(iob,io,j_mx,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	!write(6,*)noptarg,j_optarg0(1)
	if(noptarg.gt.0)j_gpix=j_optarg0(1)
 
	j_gpiy=0
	call j_getoption_index(iob,io,j_my,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(noptarg.gt.0)j_gpix=j_optarg0(1)
 
 
 
 
 
	!	call j_clearoption(iob,i)
	!	write(6,*)'j_gpiout,append,j_otype(j_gpiout)',showfig2j_gpiout,append,j_otype(j_gpiout)
	!write(6,*)'j_gpletitle,j_gplexlabel,j_gpleylabel',j_gpletitle,j_gplexlabel,j_gpleylabel
	if(j_gpletitle.gt.0)then
 
		ifo=j_font(j_gptitle,j_gpletitle)
		!	write(6,*)'title:'//j_gptitle(1:j_gpletitle)//'/',ifo
		if(j_err)return
		if(ifo.gt.0)then
			call j_putfig0(1,'set title "'//j_gptitle(1:ifo-1)//'" font "'//j_gptitle(ifo+1:j_gpletitle-1)//'"')
		else
			call j_putfig0(1,'set title "'//j_gptitle(1:j_gpletitle)//'"')
		endif !if(ifo.gt.0)   5432
	endif !if(j_gpletitle.gt.0)   5427
	!
	if(j_gplexlabel.gt.0)then
		ifo=j_font(j_gpxlabel,j_gplexlabel)
		!	write(6,*)'xla:'//j_gpxlabel(1:j_gplexlabel)//'/',ifo
		if(j_err)return
		if(ifo.gt.0)then
			call j_putfig0(2,'set xlabel "'//j_gpxlabel(1:ifo-1)//&
				'" font "'//j_gpxlabel(ifo+1:j_gplexlabel-1)//'"')
		else
			call j_putfig0(2,'set xlabel "'//j_gpxlabel(1:j_gplexlabel)//'"')
		endif !if(ifo.gt.0)   5443
	endif !if(j_gplexlabel.gt.0)   5439
	if(j_gpleylabel.gt.0)then
		ifo=j_font(j_gpylabel,j_gpleylabel)
		if(j_err)return
		if(ifo.gt.0)then
			call j_putfig0(3,'set ylabel "'//j_gpylabel(1:ifo-1)//&
				'" font "'//j_gpylabel(ifo+1:j_gpleylabel-1)//'"')
		else
 
 
			call j_putfig0(3,'set ylabel "'//j_gpylabel(1:j_gpleylabel)//'"')
 
		endif !if(ifo.gt.0)   5453
	endif !if(j_gpleylabel.gt.0)   5450
	!	call j_putfig0(3,'set ylabel "'//j_gpylabel(1:j_gpleylabel)//'"')
 
	!subroutine j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) !
	call j_getoption_index(iob,io,j_mxrange,-1,4,j_ipreal,.false.,noptarg,j_optarg0)
	if(j_err)return
	j_o(j_gpiout)%d(1:4)=j_inf
	if(noptarg.eq.0.and.nupdate)then
		if(j_gpix.le.0)then
			write(6,*)'xrange-> requires x->'
			j_err=.true.;return
		endif !if(j_gpix.le.0)   5470
		ii1=j_object3(j_gpix,'%min')
		ii2=j_object3(j_gpix,'%max')
		!		write(6,*)'ii1,ii2',ii1,ii2
		if(ii1.le.0.or.ii2.le.0)then
			write(6,*)'...%min  or ...%max does not exist'
			j_err=.true. ;return
 
		endif !if(ii1.le.0.or.ii2.le.0)   5477
		!xmin=j_v(ii1)
		!xmax=j_v(ii2)
		write(6,*)'using xrange->(',j_chr10(j_v(ii1)),',',j_chr10(j_v(ii2)),') given by ',j_vname(ii1),&
			' and ',j_vname(ii2)
		j_o(j_gpiout)%d(1)=j_v(ii1)
		j_o(j_gpiout)%d(2)=j_v(ii2)
	elseif(noptarg.gt.0)then
 
		j_o(j_gpiout)%d(1)=j_v(j_optarg0(1))
		if(noptarg.gt.1)j_o(j_gpiout)%d(2)=j_v(j_optarg0(2))
		if(noptarg.gt.2)j_o(j_gpiout)%d(3)=j_v(j_optarg0(3))
		if(noptarg.gt.3)j_o(j_gpiout)%d(4)=j_v(j_optarg0(4))
		if(noptarg.eq.1.or.noptarg.eq.3)then
			j_o(j_gpiout)%txt(4)='set xrange['//j_chr10(j_v(j_optarg0(1)))//':]'
		elseif(noptarg.eq.2.or.noptarg.eq.4)then !if(noptarg.eq.1)then
			j_o(j_gpiout)%txt(4)='set xrange['//j_chr10(j_v(j_optarg0(1)))//':'//&
				j_chr10(j_v(j_optarg0(2)))//']'
 
		endif !if(noptarg.eq.1.or.noptarg.eq.3)   5494
		j_o(j_gpiout)%i2(4)=len_trim(j_o(j_gpiout)%txt(4))
 
	endif !if(noptarg.eq.0.and.nupdate)   5469
 
 
	call j_getoption_index(iob,io,j_myrange,-1,2,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.gt.0)j_o(j_gpiout)%d(5)=j_v(j_optarg0(1))
	if(noptarg.gt.1)j_o(j_gpiout)%d(6)=j_v(j_optarg0(2))
 
	if(noptarg.eq.1)then
		j_o(j_gpiout)%txt(5)='set yrange['//j_chr10(j_v(j_optarg0(1)))//':]'
	elseif(noptarg.eq.2)then !if(noptarg.eq.1)then
		j_o(j_gpiout)%txt(5)='set yrange['//j_chr10(j_v(j_optarg0(1)))//':'//&
			j_chr10(j_v(j_optarg0(2)))//']'
	endif !if(noptarg.eq.1)   5511
	if(noptarg.gt.0)j_o(j_gpiout)%i2(5)=len_trim(j_o(j_gpiout)%txt(5))
 
	!	if(p)write(6,*)'<736736 nres,j_gpbas,j_o(j_gpiout)%i(1)',nres,j_gpbas,j_o(j_gpiout)%i(1)
	!	call j_defchar(iv,'.jfig',ivout)
 
end subroutine !subroutine j_startfig(iob,io,update)
 
 
 
!20150812(arg1<->arg2) oli: subroutine deffig(name,iv,mxfigs,mxpoints,xmin,ymin,xmax,ymax,dx,dy,title,ivout) !defines a figure object
! subroutine j_deffig(iv,name,mxfigs,mxpoints,xmin,ymin,xmax,ymax,dx,dy,title,ivout) !defines a figure object
 
 
! integer, intent(in):: iv, mxfigs, mxpoints
! real, intent(in) ::xmin,ymin,xmax,ymax,dx,dy
! integer ,intent(out) :: ivout
! character(len=*), intent(in):: name, title
 
! !parameter (n4=8)
 
! !20150812(arg1<->arg2) oli: 	call getv(name,iv,ipfigure,ivout)
 
! call j_del(iv)
 
! call j_getobject(iv,name,j_ipfigure,ivout)
! !20140522 virheenkäsittely
! if(j_err) return
! allocate (j_o(ivout)%i(0:mxfigs+2))
! j_o(ivout)%i(0:2)=(/1,1,101/) !first subfig is header, chracters as text buffer
! allocate (j_o(ivout)%i2(1:j_fig_n4*mxfigs))  !(number of points,basis subfig,itype)
! ! fig header, first subfig: xminreal,yminreal,xmaxreal, ymaxreal,
! ! points: xmingiven,yming,xmaxg,ymaxg,dx,dy,ticx,ticy
! ! subfigs: xmin,xmax,ymin,ymax,points
! ! number of points, lastt used eleemt of r, figtype
! j_o(ivout)%i2(1:4)=(/4,12,-1,0/)
! allocate (j_o(ivout)%r(1:12+j_fig_n4*mxfigs+mxpoints*2))
! j_o(ivout)%r(1:4)=(/1.7e37,-1.7e37,1.7e37,-1.7e37/)
! j_o(ivout)%r(5:12)=(/xmin,ymin,xmax,ymax,dx,dy,0.,0./)
! allocate (j_o(ivout)%ch(1:mxfigs+100)) !xlegend, ylegend,legend + some text
! j_o(ivout)%ch=' '
! !do i=1,40;o(ivout)%ch(i)=' ';end subroutine do
! do i=41,40+len(title)
! j_o(ivout)%ch(i)=title(i-40:i-40)
! enddo !do i=41,40+len(title)
! !write(6,*)'ch',o(ivout)%ch
! return
! end subroutine j_deffig !subroutine j_deffig(iv,name,mxfigs,mxpoints,xmin,ymin,xmax,ymax,dx,dy,title,ivout)
 
 
 
 
 
subroutine j_openread(fil,for,nu,irecl) ! open file for reading
	!module filemod
	!end module
 
 
	character*(*), intent(in):: fil,for
	integer, intent(out) :: nu
	integer,optional,intent(in) ::irecl
	write(6,*)'OPENREADHERE'
	lefo=len(for)
	! write(6,*)'j_util <4680> openread, lefo,fil,for ', lefo,fil,for
	!nused=nused+1
	!20140523 oli: if(nused.ge.mxunits)stop '*j* mxunits'
	if(j_nused.ge.j_mxunits) then
		write(6,*)'*J* too many open files, ask R. Lempinen to increase mxunits'
		j_err = .true.
		return
	endif !if(j_nused.ge.j_mxunits)   5582
	nu=j_nunits(j_nused+1)
	!write(6,*)'file:',fil,' ofr',for,nu
	if(for(1:1).eq.'b'.or.for(1:1).eq.'B')then
		ise=min(lefo,2)
		! if(for(ise:ise).eq.'i')then
		! open(nu,err=90,file=fil,form='binary',status='old',iostat=ios)
		! ! write(6,*)'open: nu,ios',nu,ios
		! write(6,*)'binary not available, ask J. Lappi'
		! j_err=.true.;return
		if(for(ise:ise).eq.'s')then
			open(nu,err=90,file=fil,form='unformatted',status='old',&
				access='stream',iostat=ios,action='READ')
		else !if(for(ise:ise).eq.'s')then
			open(nu,err=90,file=fil,form='unformatted',status='old',iostat=ios,action='READ')
		endif !if(for(ise:ise).eq.'s')   5596
	elseif(for(1:1).eq.'d')then !if(for(1:1).eq.'b'.or.for(1:1).eq.'B')then
		if(.not.present(irecl))then
			write(6,*)'*j* j_openread: argument irecl not presetn'
			j_err=.true.;return
		endif !if(.not.present(irecl))   5603
		isc=1
		if(for(2:2).eq.'4')isc=4
		open(nu,err=90,file=fil,form='unformatted',access='direct',recl=isc*irecl,&
			status='old',iostat=ios,action='READ')
	else !if(for(1:1).eq.'b'.or.for(1:1).eq.'B')then
		!if(for(1:1).eq.'*'.or.for(1:1).eq.'(')then
		open(nu,err=90,file=fil,form='formatted',status='old',iostat=ios,action='READ')
	end if !if(for(1:1).eq.'b'.or.for(1:1).eq.'B')   5589
	j_nused=j_nused+1
 
	!write(6,*)'<<opening ',nu,fil
	return
90 	write(6,*)'error opening file for reading: ',fil,' iostat=',ios,' unit=',nu
	if(j_nused.gt.0)write(6,*)'open units: ',j_nunits(1:j_nused)
	write(6,*)'format: ',for
	if(ios.eq.29)write(*,*)'file not found'
	if(ios.eq.30)write(6,*)'file opened by other application?'
	!20140523 oli : write(*,*)'open units:',(nunits(jj),jj=1,nused)
	j_err=.true.
	nu=0
	return
end subroutine j_openread !subroutine j_openread(fil,for,nu,irecl)
 
 
subroutine j_openreadiv(ivi,for,nu,ext,irecl) !get file for reading when file name is stored in character constant iv
 
	integer, intent(in) :: ivi
	integer, intent(out) :: nu
	character*(*), intent(in):: for
	character*(*),optional,intent(in)::ext
	integer,intent(in),optional::irecl
	! if iv is text object then nu will be -iv
 
	iv=abs(ivi)
 
	if(iv.eq.j_ivdollar)then
		nu=5
		return
	endif !if(iv.eq.j_ivdollar)   5642
	if(j_otype(iv).eq.j_iptext)then
		nu=-iv
		return
	endif !if(j_otype(iv).eq.j_iptext)   5646
	ifi=iv
	if(j_otype(iv).ne.j_ipchar)then
		if(present(ext))then
			ifi=j_defchar(ivi,ext)
			!		write(6,*)ifi,j_otype(ifi)
			!	%	ivout=ifi
			if(j_err)return
		else !if(present(ext))then
			call j_printname('*trying to open file ',iv,' which is not character variable or constant')
			j_err=.true.
			return
		endif !if(present(ext))   5652
	endif !if(j_otype(iv).ne.j_ipchar)   5651
	!file from filemod
	!combines getchar and openread
	call j_getchar(ifi,j_filename,le)
	!le=0
	!do k=o(ivnames)%i(iv)+1,o(ivnames)%i(iv+1)-2
	!le=le+1
	!filename(le:le)=o(ivnames)%ch(k)
	!end do
	if(j_nused.ge.j_mxunits)then
		write(6,*) '*j* mxunits'
		j_err=.true.
	endif !if(j_nused.ge.j_mxunits)   5671
	nu=j_nunits(j_nused+1)
	!write(6,*)'file:',fil,' ofr',for,nu
	if(for(1:1).eq.'*'.or.for(1:1).eq.'(')then
		if(ivi.gt.0)then
			open(nu,err=90,file=j_filename(1:le),form='formatted',status='old',&
				iostat=ios,action='READ')
		else !if(ivi.gt.0)then
908			open(nu,err=908,file=j_filename(1:le),form='formatted',status='old',&
				iostat=ios,action='READWRITE')
		endif !if(ivi.gt.0)   5678
		j_nused=j_nused+1
		j_unitchar(nu)=-iv     ! readfile
		call j_putiounit(nu,iv) ! j_o(iv)%i(4)=nu
		return
	else if(for(1:1).eq.'b')then !if(for(1:1).eq.'*'.or.for(1:1).eq.'(')then
		if(ivi.gt.0)then
			open(nu,err=90,file=j_filename(1:le),form='unformatted',status='old',&
				iostat=ios,action='READ')
		else !if(ivi.gt.0)then
904			open(nu,err=904,file=j_filename(1:le),form='unformatted',status='old',&
				iostat=ios,action='READWRITE')
		endif !if(ivi.gt.0)   5690
		j_nused=j_nused+1
		j_unitchar(nu)=-iv
		call j_putiounit(nu,iv) !j_o(iv)%i(4)=nu
		return
	else if (for(1:1).eq.'d')then !if(for(1:1).eq.'*'.or.for(1:1).eq.'(')then
		if(present(irecl))then
			open(nu,err=90,file=j_filename(1:le),access='direct',status='old',&
				iostat=ios,action='READWRITE',recl=irecl)
		else !if(present(irecl))then
			write(6,*)'*j* j_openreadiv, missing irecl'
			j_err=.true.
			return
		endif !if(present(irecl))   5702
		j_nused=j_nused+1
		j_unitchar(nu)=-iv
		call j_putiounit(nu,iv) !j_o(iv)%i(4)=nu
		return
		!	endif
 
		!if(for(1:1).eq.'*'.or.for(1:1).eq.'(')then
		write(6,*)'*illegal format:',for,', cannot open file'
		goto 91
	end if !if(for(1:1).eq.'*'.or.for(1:1).eq.'(')   5677
90 write(6,*)'**error opening file:',j_filename(1:le),' iostat=',ios
	if(ios.eq.29)write(*,*)'file not found'
	if(ios.eq.30)write(6,*)'file opened by other application?'
91	nu=0;j_err=.true.
	return
end subroutine j_openreadiv !subroutine j_openreadiv(ivi,for,nu,ext,irecl)
 
subroutine j_getwritefile(ivfile,ivform,bin,del) !get file for writing, name is stored in character constant iv
	! bin must be logical variable , not .true. or .fasle.
	!module filemod
	!end module
 
	!module vmod
	!end module vmod
 
	!parmod
	! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
	!end module parmod
 
	!module typemod
	!ipobj  ! & first objec ????
	! !typec
	!end module
 
 
	integer, intent(in):: ivfile, ivform
	logical, intent(in),optional::del
	logical, intent(out):: bin
 
	logical yes,replace
	character*1 ans,form1
 
	replace=.false.
	if(present(del))replace=del
	!file from filemod
	!combines getchar and openread
	if(j_otype(ivform).eq.j_ipchar)then
		bin=j_o(j_ivnames)%ch(j_o(ivform)%i(1)).eq.'b'
	else if(ivform.eq.j_ivdollar.or.ivform.eq.j_ivbuffer)then !if(j_otype(ivform).eq.j_ipchar)then
		bin=.false.
	else !if(j_otype(ivform).eq.j_ipchar)then
		write(6,*)'**illegal format in write'
		j_err=.true. ;return
	endif !if(j_otype(ivform).eq.j_ipchar)   5756
	if(ivfile.eq.j_ivdollar)return
	call j_getchar(ivfile,j_filename,le)
	!le=0
	!do k=o(ivnames)%i(iv)+1,o(ivnames)%i(iv+1)-2
	!le=le+1
	!filename(le:le)=o(ivnames)%ch(k)
	!end do
	if(j_nused.ge.j_mxunits)then
		write(6,*)'*j* maxunits (2)'
		j_err=.true.
		return
	endif !if(j_nused.ge.j_mxunits)   5771
	nu=j_nunits(j_nused+1)
	!write(6,*)'file:',filename(1:le)
	inquire(file=j_filename(1:le),exist=yes)
	if(yes.and..not.replace)then
1000	format(1x,a,a,a,$)
		write(6,1000)'file: ',j_filename(1:le),' exists delete old? (y/n)>'
		read(5,*)ans
		if(ans.ne.'y'.and.ans.ne.'Y')then
			write(6,*)'**was not allowed replace file:';j_err=.true.
			return
		endif !if(ans.ne.'y'.and.ans.ne.'Y')   5783
	endif !if(yes.and..not.replace)   5779
	if (bin)then
		open(unit=nu,iostat=ier,err=90,file=j_filename(1:le),&
			ACCESS='SEQUENTIAL',status='REPLACE',form='UNFORMATTED',action='WRITE')
	else !if (bin)then
		open(unit=nu,  err=90,file=j_filename(1:le),&
			ACCESS='SEQUENTIAL',status='REPLACE',form='FORMATTED',action='WRITE', &
			iostat=ier)
	endif !if (bin)   5788
	!	j_nused=j_nused+1; j_unitchar(nu)=ivfile; j_o(ivfile)%i(4)=nu;return
	j_nused=j_nused+1; j_unitchar(nu)=ivfile; call j_putiounit(nu,ivfile);return
90  write(6,*)'error opening file for writing: ',j_filename(1:le),&
		' iostat:',ier,' unit',nu
	if(ier.eq.30)write(6,*)'file opened by other application?'
	nu=0; j_err=.true.;   return
end subroutine j_getwritefile !subroutine j_getwritefile(ivfile,ivform,bin,del)
 
 
subroutine j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout,readit,deleted) !get file for writing, name is stored in character constant ivfile
	!  this subroutine open a new file for writing
	! the unit which is opened, if, there is error nu=0, if the terminal is 'opened' then nu=6, thus this subroutine can be used
	! ivfile: if ivfile is a chracter variable and there is no ext then the opened file is associated with ivfile
	! ivform: if ivform is a character variable, then the first character determines how the file is opended
	!         if ivform=j_ivdollar, then the file is opened as sequential formatted file
	! forma   if present the first character determines  the type of the file
	! if neither ivform nor forma is presnet then the file is opened as sequential formatted file
	! ext : if ivfile is aLSo present then the file is opened then ext is put after ivfile
	!        if ivfile is not present the filename and the corresponding character variable will be ext
	! replace if rw='w' and the file exists then the file is replaced without asking
	! irecl in direct access file the recod size in terms of integer or real variables
	! with Gfortran the variable $dac must have been given value 4 ?????
	! ivout (output) If a new character variable is generated for the filename, the variable is ivout and the argument must be present
	! readit (output) if rw='r' and the file does not exist and readit is present the readit=.false and no error is generatated
	!           if rw='w' and the file exists and the user answers 'u' when asked if the file is used or deleted (d),
	!              then the files is opened for reading, an open sequatial file is rewinded
	! if readit option is not present the
	! In callin the function, the arguments must be give with keywords e.g. call j_getfile(nu,'w',forma='d',ext='kukuuu.dat',irecl=100,ivout=ofile
	! direct acces files are alway openen for readwrite, even
	integer,intent(out)::nu
	character*1,intent(in):: rw
	integer, intent(in),optional:: ivfile, ivform
	character*(*),intent(in),optional::forma
	character*(*),intent(in),optional::ext
	logical,intent(in),optional::replace
	integer, intent(in),optional::irecl
	integer, intent(out),optional::ivout
	logical, intent(out),optional::readit
	logical, intent(out),optional::deleted
	logical yes,replace2,bin,dir,ope,useit
	logical p
	character*1 ans
	integer ::ivform_
	p=.true.
	!if(present(ivfile))write(6,*)ivfile
	!if(present(ext))write(6,*)ext
	if(rw.ne.'r'.and.rw.ne.'w'.and.rw.ne.'a')then
		write(6,*)'*j* getfile, rw must be r, w or a'
		j_err=.true.
 
		return
	endif !if(rw.ne.'r'.and.rw.ne.'w'.and.rw.ne.'a')   5842
	if(rw.eq.'r'.and.present(ivfile))then
		if(abs(ivfile).eq.j_ivdollar)then
			nu=5
			return
		endif !if(abs(ivfile).eq.j_ivdollar)   5849
	endif !if(rw.eq.'r'.and.present(ivfile))   5848
	!write(6,*)'<555>',ivfile
	replace2=.false.
	useit=.false.
	if(present(replace))replace2=replace
	if(present(readit))readit=.false.
	if(present(deleted))deleted=.false.
	!	bin=.false.
	!	dir=.false.
	ivform_=0
	if(present(ivform))then
		ivform_=ivform
		!	write(6,*)'545445ut',ivform
		if(j_otype(ivform).eq.j_ipchar.and.ivform.gt.32.and.ivform.ne.j_ivbgaya)then
			call j_getchar(ivform,j_form_,lef)
			if(j_form_(1:1).eq.'(')then
				j_form_(1:1)='*'
				lef=1
			endif !if(j_form_(1:1).eq.'(')   5868
		elseif(ivform.eq.j_ivdollar)then !if(j_otype(ivform).eq.j_ipchar.and.ivform.gt.32)then
			j_form_='*'
			lef=1
		endif !if(j_otype(ivform).eq.j_ipchar.and.ivform.gt.32.and.ivform   5866
	elseif(present(forma))then !if(present(ivform))then
		j_form_=forma
		lef=len_trim(j_form_)
	else !if(present(ivform))then
		j_form_='*'
		lef=1
	endif !if(present(ivform))   5863
 
	!write(6,*)'<778', ivform_
 
	lefi=0
	if(present(ivfile))then
		if(present(ext))then
			ifi=j_defchar(ivfile,ext)
			if(j_err)return
			if(present(ivout))ivout=ifi
			!call j_getchar3(iv=ivfile,buffer=j_filename,le=lefi,ext=ext)
		elseif(j_otype(ivfile).eq.j_ipchar)then !if(present(ext))then
			ifi=ivfile
			!write(6,*)'<773 ifi',ifi
		else !if(present(ext))then
			call j_printname('*j* getfile: cannot make ',ivfile,' character')
			j_err=.true.
		endif !if(present(ext))   5888
	else !if(present(ivfile))then
		if(present(ext))then
			ifi=j_defchar(0,ext)
			if(j_err)return
			if(present(ivout))ivout=ifi
		else !if(present(ext))then
			write(6,*)'*j* j_getfile, NO FILE'
			j_err=.true.
			return
		endif !if(present(ext))   5901
	endif !if(present(ivfile))   5887
	call j_getchar(ifi,j_filename,lefi)
	!	write(6,*)'<77getf44',j_filename(1:lefi),ifi,j_otype(ifi)
	if(j_nused.ge.j_mxunits)then
		write(6,*)'*j* maxunits (2) in j_getfile'
		j_err=.true.
		return
	endif !if(j_nused.ge.j_mxunits)   5913
	!	write(6,*)'lefi',lefi
	!	write(6,*)j_filename
	nu=j_nunits(j_nused+1)
	idac=0
 
	if(ivform_.eq.j_ivdi)then
		idac=1;isize=1
	elseif(ivform_.eq.j_ivdi2)then !if(ivform_.eq.j_ivdi)then
		!if(j_form_(2:2).eq.'1')then
		idac=1	;isize=2		!intel
	elseif(ivform_.eq.j_ivdg)then !if(ivform_.eq.j_ivdi)then
 
		idac=4;isize=1
	elseif(ivform_.eq.j_ivdg2)then !if(ivform_.eq.j_ivdi)then
		idac=4;isize=2
	endif !if(ivform_.eq.j_ivdi)   5923
	! if(j_v(j_ivdac).le.0)then
	! write(6,*)'$dac=0, should be 4 for Gfortran and 1 for Intel, use j.par, or the current include file'
	! j_err=.true.
	! return
	! endif
	if(idac.ne.0.and..not.present(irecl))then
		write(6,*)'*j*, irecl missing in j_getfile for direct access'
		j_err=.true.
	endif !if(idac.ne.0.and..not.present(irecl))   5939
 
	ope=.false.
	inquire(file=j_filename(1:lefi),exist=yes,opened=ope)
 
	if(rw.eq.'a')then
		open(nu,err=90,file=j_filename(1:lefi),form='formatted',status='old',iostat=ios,access='APPEND')
		goto 700
	endif !if(rw.eq.'a')   5947
 
	if(ope)nu=j_iounit(ifi)
777			if(yes.and..not.replace2.and.rw.eq.'w')then
	1000		format(1x,a,a,a,$)
		if(present(readit))then
			write(6,*)' '
			write(6,1000)'file: ',j_filename(1:lefi),' exists, delete it (d) or use it (u)? (d/u)>'
			read(5,*)ans
			if(ans.eq.'u'.or.ans.eq.'U')then
				readit=.true.
				if(ope.and.j_form_(1:1).ne.'d')then
					write(6,*)'the file is rewinded'
					rewind(nu,err=981)
				endif !if(ope.and.j_form_(1:1).ne.'d')   5961
				if(ope)return
				useit=.true.
			elseif(ans.ne.'d'.and.ans.ne.'D')then !if(ans.eq.'u'.or.ans.eq.'U')then
				goto 777
			else !if(ans.eq.'u'.or.ans.eq.'U')then
				if(present(deleted))deleted=.true.
				if(present(readit))readit=.false.
				replace2=.true.
			endif !if(ans.eq.'u'.or.ans.eq.'U')   5959
 
		else !if(present(readit))then
			write(6,*)' '
			write(6,1000)'file: ',j_filename(1:lefi),' exists, delete it (y/n)?>'
			read(5,*)ans
			if(ans.eq.'d'.or.ans.eq.'D')then
				write(6,*)'you mean probably y'
				ans='y'
			endif !if(ans.eq.'d'.or.ans.eq.'D')   5979
			if(ans.ne.'y'.and.ans.ne.'Y')then
				write(6,*)'**was not allowed replace file:';j_err=.true.
				return
			endif !if(ans.ne.'y'.and.ans.ne.'Y')   5983
			if(present(deleted))deleted=.true.
		endif !if(present(readit))   5955
 
	endif !777			if(yes.and..not.replace2.and.rw.eq.'w')   5953
	!if(ope)write(6,*)'old ',j_nused
	if(ope)close(nu,err=93)
	if(j_err)return
 
 
	if(rw.eq.'r'.or.useit)then
		!	write(6,*)'<6665 ',lefi,ivform_,j_filename(1:60),' idac ',idac
		if(.not.yes)then
			write(6,*)'*file ',j_filename(1:lefi),' does not exist'
			j_err=.true.
			return
		endif !if(.not.yes)   5998
		if(ope)then
			write(6,*)'*file '//j_filename(1:lefi)//' was already open, it is closed first'
			close(nu,err=93)
		endif !if(ope)   6003
		!write(6,*)'<776,nu,form',nu,j_form(1:1)
		!		if(j_form_(1:1).eq.'b'.or.j_form_(1:1).eq.'B')then
		if(ivform_.eq.j_ivb.or.ivform_.eq.j_ivb2.or.ivform_.eq.j_ivbgaya)then
			!if(j_form_(2:2).eq.'s')then
			!	open(nu,err=90,file=j_filename(1:lefi),form='unformatted',status='old',&
			!		access='stream',iostat=ios,action='READ')
			!else !if(j_form_(2:2).eq.'s')then
			!	write(6,*)'<55555>',nu,j_filename(1:lefi)
			open(nu,err=90,file=j_filename(1:lefi),form='unformatted',status='old',iostat=ios,action='READ')
			!	write(6,*)'<5566>'
			!endif !if(j_form_(2:2).eq.'s')then
			!		elseif(j_form_(1:1).eq.'d')then !if(j_form_(1:1).eq.'b'.or.j_form_(1:1).eq.'B')then
		elseif(idac.ne.0)then !if(iform_.eq.j_ivb.or.iform_.eq.j_ivb2)then
			irec2=idac*irecl*isize
			!write(6,*)'<345irec2,nu',irec2,nu,lefi,j_v(j_ivdac)
			!	write(6,*)'<8485',j_filename(1:lefi)
			open(nu,err=90,file=j_filename(1:lefi),form='unformatted',access='direct',recl=irec2,&
				status='old',iostat=ios,action='READWRITE')
		else !if(iform_.eq.j_ivb.or.iform_.eq.j_ivb2)then
			!if(for(1:1).eq.'*'.or.for(1:1).eq.'(')then
			!open(nu,err=90,file=j_filename(1:lefi),form='formatted',status='old',iostat=ios,action='READ',encoding='UTF-8')
			!	write(6,*)'<191lefi'
			open(nu,err=90,file=j_filename(1:lefi),form='formatted',status='old',iostat=ios,action='READ')
			!	write(6,*)'<19efi'
		end if !if(ivform_.eq.j_ivb.or.ivform_.eq.j_ivb2.or.ivform_.eq.j_i   6009
 
	else !if(rw.eq.'r'.or.useit)then
 
		!inquire(file=j_filename(1:lefi),exist=yes,opened=ope)
 
		!if(ope)nu=j_iounit(ifi)
		!write(6,*)'nu',nu
 
 
		!if(ope)write(6,*)'new ',j_nused
		!	write(6,*)'<424util',ivform_,j_ivb,j_ivb2
		if(ivform_.eq.j_ivb.or.ivform_.eq.j_ivb2)then
			open(unit=nu,iostat=ier,err=91,file=j_filename(1:lefi),&
				ACCESS='SEQUENTIAL',status='REPLACE',form='UNFORMATTED',action='WRITE')
 
		elseif(idac.ne.0)then !if(iform_.eq.j_ivb.or.iform_.eq.j_ivb2)then
			irec2=idac*irecl*isize
			!		write(6,*)'<345irecl,irec2,lefi,nu,dac,ope',irecl,irec2,lefi,nu,j_v(j_ivdac),ope
			if(p)write(6,*)j_filename(1:lefi)
			open(unit=nu,iostat=ier,err=91,file=j_filename(1:lefi),form='unformatted',access='direct',recl=irec2,&
				status='REPLACE',action='READWRITE')
			if(p)write(6,*)'<23ok,ope',ope
		else !if(iform_.eq.j_ivb.or.iform_.eq.j_ivb2)then
			!write(6,*)'avataan ', j_filename(1:lefi),nu,ope
			open(unit=nu,  err=91,file=j_filename(1:lefi),&
				ACCESS='SEQUENTIAL',status='REPLACE',form='FORMATTED',action='WRITE', &
				iostat=ier)
		endif !if(ivform_.eq.j_ivb.or.ivform_.eq.j_ivb2)   6043
	endif !if(rw.eq.'r'.or.useit)   5996
 
	!p=.true.
	!write(6,*)'<44ope,nu,ifi',ope,nu,ifi
 
	if(ope)return
	!if(p)write(6,*)'<77>',j_o(ifi)%i
	!if(p)write(6,*)'<78unitcar>',j_unitchar
700	j_nused=j_nused+1  !we come here from append
	if(rw.eq.'r')then
		!	write(6,*)'<888'
		j_unitchar(nu)=-ifi
	else !if(rw.eq.'r')then
		j_unitchar(nu)=ifi
	endif !if(rw.eq.'r')   6069
	!	write(6,*)'<66bef ',j_iounit(ifi)
	call j_putiounit(nu,ifi)  !; if(p)write(6,*)'<hihuraa',nu,ifi
	!write(6,*)'<66af ',j_iounit(ifi)
	return
90  write(6,*)'error opening file for reading: ',j_filename(1:lefi),&
		' iostat:',ier,' unit',nu
	if(ier.eq.30)write(6,*)'file opened by other application?'
	nu=0; j_err=.true.;   return
91  write(6,*)'error opening file for writing: ',j_filename(1:lefi),&
		' iostat:',ier,' unit',nu
		!if(present(deleted))deleted=.false. at the beginning
	if(ier.eq.30)write(6,*)'file opened by other application?'
	nu=0; j_err=.true.;   return
93  write(6,*)'getfile: error closing unit ',nu	;j_err=.true.;return
981 write(6,*)'getfile: error rwinding unit ',nu	;j_err=.true.;return

end subroutine j_getfile !subroutine j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout,readit,deleted)
 
logical function j_exist(ivfile,ext) !check if file exist ivfile can be character or other
	integer, intent(in)::ivfile
	character*(*), intent(in),optional::ext
	ifi=ivfile
	if(j_otype(ivfile).ne.j_ipchar)then
		if(present(ext))then
			!	write(6,*)'perk'
			ifi=j_defchar(ivfile,ext)
			if(j_err)return
			!		write(6,*)ifi,j_otype(ifi)
		endif !if(present(ext))   6098
	endif !if(j_otype(ivfile).ne.j_ipchar)   6097
	!	write(6,*)ifi,j_otype(ifi)
	call j_getchar(ifi,j_filename,le)
	inquire(file=j_filename(1:le),exist=j_exist)
 
end function j_exist !logical function j_exist(ivfile,ext)
subroutine j_getwritefilebin(ivfile,ext,irecl,ivout) !get file for writing, name is stored in character constant iv
 
 
	integer, intent(in):: ivfile
	character*(*), intent(in),optional::ext
	integer, intent(in),optional::irecl
	integer, intent(out),optional::ivout
	logical yes,replace,bin,dir
	character*1 ans,form1
 
	replace=.false.
	!file from filemod
	!combines getchar and openread
	ifi=ivfile
	dir=.false.
	!	write(6,*)ivfile,j_otype(ivfile)
	if(j_otype(ivfile).ne.j_ipchar)then
		if(present(ext))then
			!	write(6,*)'perk'
			ifi=j_defchar(ivfile,ext)
			if(j_err)return
			!		write(6,*)ifi,j_otype(ifi)
			ivout=ifi
			dir=.true.
		else !if(present(ext))then
 
			write(6,*)'*j* j_getwritefilebin error'
			j_err=.true.
			return
		endif !if(present(ext))   6127
	endif !if(j_otype(ivfile).ne.j_ipchar)   6126
	!	write(6,*)ifi,j_otype(ifi)
	call j_getchar(ifi,j_filename,le)
	!write(6,*)ifi,j_otype(ifi)
	if(j_nused.ge.j_mxunits)then
		write(6,*) '*j* mxunits must be increased'
		j_err=.true.
		return
	endif !if(j_nused.ge.j_mxunits)   6144
	nu=j_nunits(j_nused+1)
	!write(6,*)'file:',filename(1:le)
	inquire(file=j_filename(1:le),exist=yes)
	if(yes.and..not.replace)then
1000 	format(1x,a,a,a,$)
		write(6,1000)'file: ',j_filename(1:le),' exists delete old? (y/n)>'
		read(5,*)ans
		if(ans.ne.'y'.and.ans.ne.'Y')then
			write(6,*)'**was not allowed replace file:';j_err=.true.
			return
		end if !if(ans.ne.'y'.and.ans.ne.'Y')   6156
	end if !if(yes.and..not.replace)   6152
	if(dir)then
		open(unit=nu,iostat=ier,err=90,file=j_filename(1:le),form='unformatted',access='direct',recl=irecl,&
			status='REPLACE',action='READWRITE')
	else !if(dir)then
		open(unit=nu,iostat=ier,err=90,file=j_filename(1:le),&
			ACCESS='SEQUENTIAL',status='REPLACE',form='UNFORMATTED',action='WRITE')
	endif !if(dir)   6161
	!j_nused=j_nused+1;j_unitchar(nu)=ivfile; j_o(ivfile)%i(4)=nu;return
	j_nused=j_nused+1
	!	write(6,*)ifi,j_otype(ifi)
	j_unitchar(nu)=ifi; write(6,*)ifi,j_otype(ifi)
	!	write(6,*)'nu,ifi',nu,ifi
	call j_putiounit(nu,ifi)
	!		write(6,*)'nu,ifi',nu,ifi
	return
90  write(6,*)'error opening file for writing: ',j_filename(1:le),&
		' iostat:',ier,' unit',nu
	if(ier.eq.30)write(6,*)'file opened by other application?'
	nu=0;  j_err=.true.;  return
end subroutine j_getwritefilebin !subroutine j_getwritefilebin(ivfile,ext,irecl,ivout)
 
 
subroutine j_clearoption(iob,io) !clear options  %%option
	integer,intent(in)::iob,io
	!write(6,*)'nopt',nopt
	nopc=0 !number of dropped options
 
	do i=1,j_nopt
		if(j_optionmoptio(2,i).eq.io)then
 
			if(j_optioniob(i).ne.iob)then
				write(6,*)'clearoption wrong iob, ',iob,j_optioniob(i)
				j_err=.true.;return
			endif !if(j_optioniob(i).ne.iob)   6191
			nopc=nopc+1
		else !if(j_optionmoptio(2,i).eq.io)then
			!do not drop
 
			if(nopc.gt.0)then
				j_optioniob(i-nopc)=iob
				j_optiontot(i-nopc)=j_optiontot(i)
 
			endif !if(nopc.gt.0)   6199
 
		endif !if(j_optionmoptio(2,i).eq.io)   6189
 
	end do !i=1,j_nopt   6188
	if(nopc.gt.0)j_nopt=j_nopt-nopc
 
	return
end subroutine j_clearoption !subroutine j_clearoption(iob,io)
 
! subroutine j_clearoption(iob,i) !check if there are options not checked by getoption  %%option
! !character*14 options
! !end module
 
! !write(6,*)'nopt',nopt
! do i=1,j_nopt
! !write(6,*)'clearing opt',curropt(i)
! if(j_linkoption(iob,io,j_curropt(i)).ne.0)then
 
! write(6,*)'*w* there was illegal option ', &
! j_option_name(j_curropt(i),j_lenoptions(j_curropt(i)))
! j_err=.true.
! j_linkoption(iob,io,j_curropt(i))=0
! endif !if(j_linkoption(iob,io,j_curropt(i)).ne.0)then
! end do !do i=1,j_nopt
! j_nopt=0
! return
! end subroutine j_clearopt !subroutine j_clearoption(iob,i)
 
 
function j_igetopt(iob,io,mopt) ! -1 ==not  0== opt->,  otherwise iv for first argument %%option obsolete
 
	!module vmod
	!end module vmod
 
	integer, intent(in):: iob,io, mopt
	j_igetopt=j_linkoption(iob,io,mopt)
 
	if(j_igetopt.gt.0)j_igetopt=j_o(iob)%i(j_igetopt+1)
	!if(j_linkoption(iob,io,mopt).eq.0)then
	return
end function j_igetopt !function j_igetopt(iob,io,mopt)
 
logical function j_isopt(iob,io,mopt,default) ! -1 ==not  0== opt->,  otherwise iv for first argument %%option obsolete
	integer, intent(in):: iob,io, mopt
	logical,intent(in),optional::default
	iopt=j_linkoption(iob,io,mopt,clear=.true.)
	!	write(6,*)'<55 ',iopt
	if(iopt.lt.0)then
		if(present(default))then
			j_isopt=default
		else
			j_isopt=.false.
		endif !if(present(default))   6252
	elseif(iopt.eq.0)then
		j_isopt=.true.
		!	write(6,*)'<44 tas'
	elseif(j_v(j_o(iob)%i(iopt+1)).ne.j_0)then
		j_isopt=.true.
		!write(6,*)'6464',j_v(j_o(iob)%i(iopt+1))
	else
		j_isopt=.false.
	endif !if(iopt.lt.0)   6251
 
	!if(j_linkoption(iob,io,mopt).eq.0)then
	return
end function j_isopt !function j_igetopt(iob,io,mopt)
 
 
! function j_igetoptval(iob,mopt,idef,idef0) ! -1 ==not  0== opt->,  otherwise iv for first argument
 
! !module vmod
! !end module vmod
 
! integer, intent(in):: iob,mopt,idef,idef0
 
! if(j_linkoption(iob,io,mopt).eq.0)then
! j_igetoptval=idef
! else if(j_o(iob)%i(j_linkoption(iob,io,mopt)).eq.0)then !if(j_linkoption(iob,io,mopt).eq.0)then
! j_igetoptval=idef0
! else !if(j_linkoption(iob,io,mopt).eq.0)then
! j_igetoptval=j_v(j_o(iob)%i(j_linkoption(iob,io,mopt)+1))
! endif !if(j_linkoption(iob,io,mopt).eq.0)then
! return
! end function j_igetoptval !function j_igetoptval(iob,mopt,idef,idef0)
 
 
! function j_igetoptout(iob,mopt) ! -1 ==not  0== opt->,  otherwise iv for first argument
! ! the output must be legal named object if it exist delete
 
! !module vmod
! !end module vmod
 
 
! integer, intent(in):: iob, mopt
 
! if(j_linkoption(iob,io,mopt).eq.0)then
! j_igetoptout=-1
! else if(j_o(iob)%i(j_linkoption(iob,io,mopt)).eq.0)then !if(j_linkoption(iob,io,mopt).eq.0)then
! j_igetoptout=0
! else !if(j_linkoption(iob,io,mopt).eq.0)then
! j_igetoptout=j_o(iob)%i(j_linkoption(iob,io,mopt)+1)
! if(j_igetoptout.gt.j_namedv)then
! write(6,*)'*option ',j_options(mopt),' must refer to object'
! j_err=.true.
! return
! endif !if(j_igetoptout.gt.j_namedv)then
! endif !if(j_linkoption(iob,io,mopt).eq.0)then
! return
! end function j_igetoptout !function j_igetoptout(iob,mopt)
 
 
! function j_igetopt2(iob,io,mopt,itype) ! -1 ==not  0== opt->,  otherwise iv for first argument
! ! return iv for first
 
! !module vmod
! !end module vmod
 
! integer, intent(in):: iob, mopt
! integer, intent(out):: itype
 
! if(j_linkoption(iob,io,mopt).eq.0)then
! j_igetopt2=-1
! itype=-2
! elseif(j_o(iob)%i(j_linkoption(iob,io,mopt)).eq.0)then !if(j_linkoption(iob,io,mopt).eq.0)then
! j_igetopt2=0
! itype=-1
! else !if(j_linkoption(iob,io,mopt).eq.0)then
! j_igetopt2=j_o(iob)%i(j_linkoption(iob,io,mopt)+1)
! itype=j_otype(j_igetopt2)
! endif !if(j_linkoption(iob,io,mopt).eq.0)then
! return
! end function j_igetopt2 !function j_igetopt2(iob,io,mopt,itype)
 
 
subroutine j_putoptv(iob,li,j,name,iv,value)
	double precision value
	!put values for option variables
	! li =link to the option in current transforasmtion iob
	! j:th place
	! if name.ne.' '  get var name using both name and iv  e.g. 'mean%' //nameofiv
	! if there are arguments for the option which
 
 
 
	integer, intent(in):: iob,li,j,iv
	character*(*), intent(in):: name
 
	character*16 pref
 
	!	if(li.le.0)return
	nargo=0
	if(li.gt.0)nargo=j_o(iob)%i(li)
	if(nargo.eq.0)then
		!20150812(arg1<->arg2) oli: 		call getv(name,iv,ipreal,ivout)
		ivout=j_getobject(iv,name,j_ipreal)
		!20140522 virheenkäsittely
		if(j_err) return
		j_v(ivout)=value
		return
	endif !if(nargo.eq.0)   6356
	ivf=j_o(iob)%i(li+1)
	if(j_otype(ivf).eq.j_ipchar)then
		if(nargo.gt.1)then
			write(6,*)'*option cannot have several arguments if first is character'
			j_err=.true.
			return
		endif !if(nargo.gt.1)   6366
		call j_getchar(ivf,pref,le)
		! write(6,*)'pref',pref,'ive',ive,'le,iv',le,iv,'single',single
		!20150812(arg1<->arg2) oli: 		call getv(pref(1:le),iv,ipreal,ivout)
		ivout=j_getobject(iv,pref(1:le),j_ipreal)
		!20140522 virheenkäsittely
		if(j_err) return
		j_v(ivout)=value ;return
	endif !if(j_otype(ivf).eq.j_ipchar)   6365
	if(j.gt.nargo)return
	iarg=j_o(iob)%i(li+j)
	if(iarg.gt.j_namedv)then
		write(6,*)'*argument for option is not an object'
		j_err=.true.
		return
	endif !if(iarg.gt.j_namedv)   6381
	!20141219 oli: if(otype(iarg).ne.0)call del(iarg)
	if(j_otype(iarg).ne.j_ipreal)call j_del(iarg)
	j_v(iarg)=value
	return
end subroutine j_putoptv !subroutine j_putoptv(iob,li,j,name,iv,value)
 
 
subroutine j_igetoptv(igeto,name,iv,single,ivout) !get output option varaible e.g. stat(mean->
	! if igeto=0 then get var name using both name and iv  e.g. 'mean%' //nameofiv
	!if igeto.gt.0 the it contains the chracter constant
	!  if last character is not '%' and single is true then use the name as such
 
	!module typemod
	!ipobj  ! & first objec ????
	! !typec
	!end module
 
	!module vmod
	!end module vmod
 
	!parmod
	! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
	!end module parmod
 
 
	integer, intent(in):: igeto	, iv
	character*(*), intent(in):: name
	logical, intent(in):: single
	integer, intent(out):: ivout
 
	character*16 pref
 
	if(igeto.lt.0)then
		write(*,*)'*j* illegal use of igetoptv '
		ivout=0
		j_err=.true.; return
		return
	endif !if(igeto.lt.0)   6418
	if(igeto.eq.0)then
		!20150812(arg1<->arg2) oli: 		call getv(name,iv,ipreal,ivout)
		ivout=j_getobject(iv,name,j_ipreal)
		!20140522 virheenkäsittely
		if(j_err) return
		return
	endif !if(igeto.eq.0)   6424
	if(j_otype(igeto).eq.j_ipchar)then
		call j_getchar(igeto,pref,le)
		if(pref(le:le).eq.'%'.or..not.single)then ;ive=iv;else;ive=0; end if
		! write(6,*)'pref',pref,'ive',ive,'le,iv',le,iv,'single',single
		!20150812(arg1<->arg2) oli: 		call getv(pref(1:le),ive,ipreal,ivout)
		ivout=j_getobject(ive,pref(1:le),j_ipreal)
		!20140522 virheenkäsittely
		if(j_err) return
	else !if(j_otype(igeto).eq.j_ipchar)then
		if(.not.single)write(6,*)'*wrn* putting many values to same variable'
		ivout=igeto
	endif !if(j_otype(igeto).eq.j_ipchar)   6431
	return
end subroutine j_igetoptv !subroutine j_igetoptv(igeto,name,iv,single,ivout)
 
 
subroutine j_getdataobject2(iob,io)  ! initilization for data-> option  %%data
	integer,intent(in)::iob,io
 
 
	!j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
	!  ivdata,,nobs,filterlink,rejectlink,ivtrans,ivvars
	call j_getoption(iob,io,j_mprint,-1,1,j_ipreal,.true.,nopt,j_optarg0)
	j_disprint=.false.
	j_dprint=j_v(j_ivprintoutput)
	if(nopt.ge.0)then
		j_disprint=.true.
		j_dprint=-1
		if(nopt.gt.0)j_dprint=j_v(j_optarg0(1))
	endif !if(nopt.ge.0)   6456
 
 
 
	j_diob=iob
	call j_getoption(iob,io,j_mdata,-1,1,j_ipdata,.true.,nopt,j_optarg0)
	id=0
	if(j_err)return
	if(nopt.eq.1)then
		j_divdata=j_optarg0(1)
		if(j_otype(j_divdata).ne.j_ipdata)then
			call j_printname('*Object ',j_divdata, ' given in data-> is not DATA')
			j_err=.true.
			write(6,*)'Available data objects:'
			!	ial=1
			!j_tempchar2=' '
			do ii=1,j_namedv
				if(j_otype(ii).eq.j_ipdata)then
					!		write(6,*)'/'//j_vname(ii)//'/'
					call j_getname(ii)
					write(6,*)j_oname(1:j_loname)
					!			j_tempchar2(ial:ial+23)=j_vname(ii)
					!			ial=len_trim(j_tempchar2)+2
				endif !if(j_otype(ii).eq.j_ipdata)   6477
 
			enddo !ii=1,j_namedv   6476
			!	write(6,*)j_tempchar2(1:ial)
			return
		endif !if(j_otype(j_divdata).ne.j_ipdata)   6470
	elseif(j_otype(j_ivdata).eq.j_iplist)then !if(nopt.eq.1)then
		if(j_o(j_ivdata)%i(1).ne.1)then
			write(6,*)'Data=list( ) can give only one data'
			j_err=.true.
		endif !if(j_o(j_ivdata)%i(1).ne.1)   6490
		j_divdata=j_o(j_ivdata)%i2(1)
		if(j_otype(j_divdata).ne.j_ipdata)then
			call j_printname('Data=list(',j_divdata,') does not point to DATA')
			j_err=.true.
			j_divdata=0
		endif !if(j_otype(j_divdata).ne.j_ipdata)   6495
		if(j_err)return
		if(j_dprint.gt.0)call j_printname('using data given in Data=list(',j_divdata ,')')
	else if(j_dlastdata.gt.0)then !if(nopt.eq.1)then
		! last data had explicit output, then
		j_divdata=j_dlastdata   !j_o(j_ivlastdata)%i2(1)
		if(j_dprint.gt.0)call j_printname('*using last data ',j_divdata,' ')
	else !if(nopt.eq.1)then
		write(*,*)'no data available'
		j_err=.true.
		return
	endif !if(nopt.eq.1)   6468
 
	!	write(6,*)'<1po',i
 
	j_dimat=j_o(j_divdata)%i(1)
	j_divkeep=j_o(j_divdata)%i(2)
	!	call j_getname(j_dimat,j_divkeep)
 
	j_dnkeep=j_o(j_divkeep)%i(1)
	!	write(6,*)'<87',j_oname(1:j_loname),' ',j_oname2(1:j_loname2),' ',j_dnkeep
	j_dnobs8=j_nrows(j_dimat)
	j_dnobs=j_dnobs8
	write(6,*)'dnobs',j_dnobs
	j_divobs=j_o(j_divdata )%i(6)
	!	write(6,*)'<1po32',j_dnobs
 
 
	j_dfilterlink=j_codelink(iob,io,j_mfilter)
	j_disfilter=j_dfilterlink.ne.0
 
	j_drejectlink=j_codelink(iob,io,j_mreject)
	j_disreject=j_drejectlink.ne.0
	call j_getoption(iob,io,j_mtrans,-1,1,j_iptrans,.true.,nopt,j_optarg0)
	if(j_err)return
 
	j_distrans=.false.
	j_divvars= j_divkeep
	! write(6,*)'<77 ',nopt
	! if(nopt.gt.0)write(6,*)'<774',j_optarg0(1)
	ivup=j_o(j_divdata)%i(5)
	j_disup=ivup.ne.0
	if(j_disup)j_disup=j_isopt(iob,io,j_mup,.false.)
 
	naddv=0
	if(.not.j_disup)then
		j_dimat=j_o(ivdata)%i(1)
		j_divkeep=j_o(ivdata)%i(2)
		j_dnkeep=j_o(j_divkeepup)%i(3)
		!	j_divnobsw=j_o(ivdata)%i(4)
		!	j_dnextobs=1
		!	j_divobsw=j_o(divdata)%i(7)
		!	nadvv=j_o(j_divkeepup)%i(1)+1
		!	j_divobsup=j_o(ivup)%i(6)
	endif !if(.not.j_disup)   6544
 
 
	if(nopt.eq.1)then
		j_divtrans=j_optarg0(1)
		if(j_otype(j_divtrans).ne.j_iptrans)then
			call j_printname('trans-> does not refer to a transformation: ',j_divtrans,' ')
			j_err=.true. ;return
		endif !if(j_otype(j_divtrans).ne.j_iptrans)   6558
		ivoul=j_trans_output(j_divtrans)
		noutv=j_o(ivoul)%i(1)
 
		if(noutv.gt.0)then
			j_divvars=j_deflistobject(j_divdata,'%vars',ivin=j_divkeep,nres=noutv+naddv)
			iper=j_putlistobject(j_divvars,ivin=ivoul)
			!function j_putlistobject(ivlist,single,list0,list,ivin,ignored)
		endif !if(noutv.gt.0)   6565
		j_distrans=.true.
 
	endif !if(nopt.eq.1)   6556
	if(j_disup)then
		if(nopt.lt.1)j_divvars=j_deflistobject(j_divdata,'%vars',ivin=j_divkeep,nres=naddv)
 
		iper=j_putlistobject(j_divvars,ivin=j_divkeepup)
		iper=j_putlistobject(j_divvars,single=j_o(ivup)%i(6))  !Unit variable
 
 
	endif !if(j_disup)   6573
 
 
 
 
	call j_getoption(iob,io,j_mfrom,-1,1,j_ipreal,.true.,nopt,j_optarg0)
	if(nopt.gt.0)then
		ifirst=j_v(j_optarg0(1))
		if(ifirst.lt.1.or.ifirst.gt.j_dnobs)then
			write(6,*)'from->  should be between 1 and ',j_dnobs,' it is ',ifirst
			j_err=.true.;return
		endif !if(ifirst.lt.1.or.ifirst.gt.j_dnobs)   6588
		j_dfrom=ifirst
	else !if(nopt.gt.0)then
		j_dfrom=1
	endif !if(nopt.gt.0)   6586
	call j_getoption(iob,io,j_muntil,-1,1,j_ipreal,.true.,nopt,j_optarg0)
	if(nopt.gt.0)then
		ifirst=j_v(j_optarg0(1))
		if(ifirst.lt.1.or.ifirst.gt.j_dnobs8)then
			write(6,*)'last->  should be between 1 and ',j_dnobs8,' it is ',ifirst
			j_err=.true.;return
		endif !if(ifirst.lt.1.or.ifirst.gt.j_dnobs8)   6599
		j_duntil=ifirst
	else !if(nopt.gt.0)then
		j_duntil=j_dnobs8
	endif !if(nopt.gt.0)   6597
 
	call j_getoption(iob,io,j_mprolog,-1,1,j_iptrans,.true.,nopt,j_optarg0)
	if(j_err)return
 
	if(nopt.eq.1)then
 
		if(j_otype(j_optarg0(1)).ne.j_iptrans)then
			call j_printname('prolog-> does not refer to a transformation: ',j_optarg0(1),' ')
			j_err=.true. ;return
		endif !if(j_otype(j_optarg0(1)).ne.j_iptrans)   6613
		call dotrans(j_optarg0(1),1)
		!	write(6,*)'{{',3
		if(j_err)return
	endif !if(nopt.eq.1)   6611
 
	call j_getoption(iob,io,j_mepilog,-1,1,j_iptrans,.true.,nopt,j_optarg0)
	if(j_err)return
	j_depilog=0
	if(nopt.eq.1)then
		j_depilog=j_optarg0(1)
		if(j_otype(j_depilog).ne.j_iptrans)then
			call j_printname('epilog-> does not refer to a transformation: ',j_depilog,' ')
			j_err=.true. ;return
		endif !if(j_otype(j_depilog).ne.j_iptrans)   6627
	endif !if(nopt.eq.1)   6625
	!		id(j_divobsup)=0 !how may upper observations done
	!when this is reached update upper level
	!	write(6,*)'<1po3444'
	return
end subroutine j_getdataobject2 !subroutine j_getdataobject(iob,io)
 
 
subroutine j_getobs2(iobs)
	integer,intent(in)::iobs
	!	write(6,*)'iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat',iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat
	!	write(6,*)j_o(j_dimat)%d(1:6)
	if(iobs.eq.j_dfrom)then
		j_diba=(j_dfrom-1)*j_dnkeep
 
		if(j_disup)then
			j_dibaup=0
			nsub=1
			j_dobsup=1
			do while(nsub.lt.j_dfrom)
				nsub=nsub+j_v(j_divnobsw)
				j_dibaup=j_dibaup+j_dnkeepup
				j_dobsup=j_dobsup+1
			enddo !while(nsub.lt.j_dfrom)   6650
			j_v(j_divobsup)=j_dobsup
			j_dnextobs=nsub-j_dfrom+1  !testattava
		endif !if(j_disup)   6646
	endif !if(iobs.eq.j_dfrom)   6643
	if(j_disup)then
 
		if(iobs.eq.j_dnextobs)then
			j_v(j_o(j_divkeepup)%i2(1:j_dnkeepup))=j_o(j_dimatup)%d(j_dibaup+1:j_dibaup+ j_dnkeepup)
			j_dnextobs=j_dnextobs+j_v(j_divnobsw)
			j_dibaup=j_dibaup+j_dnkeepup
			j_v( j_divobsw)=1
			j_v(j_divobsup)=j_dobsup
			j_dobsup=j_dobsup+1
 
		else !if(iobs.eq.j_dnextobs)then
			j_v( j_divobsw)=j_v( j_divobsw)+1
		endif !if(iobs.eq.j_dnextobs)   6661
 
 
	endif !if(j_disup)   6659
	! write(6,*)'<55',j_diba,j_dnkeep,j_o(j_divkeep)%i2(1:j_dnkeep)
	j_v( j_o(j_divkeep)%i2(1:j_dnkeep))= &
		j_o(j_dimat)%d(j_diba+1:j_diba+ j_dnkeep)
	j_diba=j_diba+j_dnkeep
	if(j_distrans)then
		j_v(j_divobs)=iobs
		call dotrans(j_divtrans,1)
		if(j_err)write(6,*)'error in transformation for Obs ',iobs
	endif !if(j_distrans)   6679
	j_rejected=.false.
	!	j_dapu=j_codevalue(j_diob,j_dfilterlink)
	if( j_disfilter)j_rejected=j_codevalue(j_diob,j_dfilterlink).eq.j_0
	!if(iobs.le.5.and.j_disfilter)write(6,*)'<455',j_disfilter,j_dfilterlink,j_codevalue(j_diob,j_dfilterlink)
 
	if(j_disreject)j_rejected=j_rejected.or.j_codevalue(j_diob,j_drejectlink).ne.j_0
	return
end subroutine !subroutine j_getobs(iobs)
 
subroutine j_getdataobject(iob,io,ivdata)  ! initilization for data-> option  %%data
	integer,intent(in)::iob,io
	integer,intent(in),optional::ivdata
 
	!j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
	!  ivdata,,nobs,filterlink,rejectlink,ivtrans,ivvars
	call j_getoption(iob,io,j_mprint,-1,1,j_ipreal,.true.,nopt,j_optarg0)
	j_disprint=.false.
	j_dprint=j_v(j_ivprintoutput)
	if(nopt.ge.0)then
		j_disprint=.true.
		j_dprint=-1
		if(nopt.gt.0)j_dprint=j_v(j_optarg0(1))
	endif !if(nopt.ge.0)   6702
 
 
 
	j_diob=iob
	if(.not.present(ivdata))then
		call j_getoption(iob,io,j_mdata,-1,1,j_ipdata,.true.,nopt,j_optarg0)
		id=0
		if(j_err)return
		if(nopt.eq.1)then
			j_divdata=j_optarg0(1)
 
		elseif(j_otype(j_ivdata).eq.j_iplist)then !if(nopt.eq.1)then
			if(j_o(j_ivdata)%i(1).ne.1)then
				write(6,*)'Data=list( ) can give only one data'
				j_err=.true.
			endif !if(j_o(j_ivdata)%i(1).ne.1)   6719
			j_divdata=j_o(j_ivdata)%i2(1)
 
			if(j_dprint.gt.0)call j_printname('using data given in Data=list(',j_divdata ,')')
		else if(j_dlastdata.gt.0)then !if(nopt.eq.1)then
			! last data had explicit output, then
			j_divdata=j_dlastdata   !j_o(j_ivlastdata)%i2(1)
			if(j_dprint.gt.0)call j_printname('*using last data ',j_divdata,' ')
		else !if(nopt.eq.1)then
			write(*,*)'no data available'
			j_err=.true.
			return
		endif !if(nopt.eq.1)   6715
	else
		j_divdata=ivdata
 
	endif !if(.not.present(ivdata))   6711
	if(j_otype(j_divdata).ne.j_ipdata)then
		call j_printname('*Object ',j_divdata, ' given in data-> is not DATA')
		j_err=.true.
		write(6,*)'Available data objects:'
		!	ial=1
		!j_tempchar2=' '
		do ii=1,j_namedv
			if(j_otype(ii).eq.j_ipdata)then
				!		write(6,*)'/'//j_vname(ii)//'/'
				call j_getname(ii)
				write(6,*)j_oname(1:j_loname)
				!			j_tempchar2(ial:ial+23)=j_vname(ii)
				!			ial=len_trim(j_tempchar2)+2
			endif !if(j_otype(ii).eq.j_ipdata)   6746
 
		enddo !ii=1,j_namedv   6745
		!	write(6,*)j_tempchar2(1:ial)
		return
	endif !if(j_otype(j_divdata).ne.j_ipdata)   6739
 
	!	write(6,*)'<1po',i
 
	j_dimat=j_o(j_divdata)%i(1)
	j_divkeep=j_o(j_divdata)%i(2)
	!	call j_getname(j_dimat,j_divkeep)
 
	j_dnkeep=j_o(j_divkeep)%i(1)
	!	write(6,*)'<87',j_oname(1:j_loname),' ',j_oname2(1:j_loname2),' ',j_dnkeep
 
	j_dnobs8=j_nrows(j_dimat)  !=j_o(j_dimat )%i(1)
	j_dnobs=j_dnobs8
	j_divobs=j_o(j_divdata )%i(6)
	!	write(6,*)'<1po32',j_dnobs
 
 
	j_dfilterlink=j_codelink(iob,io,j_mfilter)
	j_disfilter=j_dfilterlink.ne.0
 
	j_drejectlink=j_codelink(iob,io,j_mreject)
	j_disreject=j_drejectlink.ne.0
	call j_getoption(iob,io,j_mtrans,-1,1,j_iptrans,.true.,nopt,j_optarg0)
	if(j_err)return
 
	j_distrans=.false.
	j_divvars= j_divkeep
	! write(6,*)'<77 ',nopt
	! if(nopt.gt.0)write(6,*)'<774',j_optarg0(1)
 
	j_disup=j_isopt(iob,io,j_mup,.false.)
 
	naddv=0
	if(j_disup)then
		ivup=j_o(j_divdata)%i(5)
		if(ivup.le.0)then
			write(6,*)'there is no up data, up-> ignored'
			j_disup=.false.
		else
			j_dimatup=j_o(ivup)%i(1)
			j_divkeepup=j_o(ivup)%i(2)
			j_dnkeepup=j_o(j_divkeepup)%i(3)
			j_divnobsw=j_o(ivup)%i(4)
			j_dnextobs=1
			j_divobsw=j_o(j_divdata)%i(7)
			nadvv=j_o(j_divkeepup)%i(1)+1
			j_divobsup=j_o(ivup)%i(6)
		endif !if(ivup.le.0)   6792
	endif !if(j_disup)   6790
 
 
	if(nopt.eq.1)then
		j_divtrans=j_optarg0(1)
		if(j_otype(j_divtrans).ne.j_iptrans)then
			call j_printname('trans-> does not refer to a transformation: ',j_divtrans,' ')
			j_err=.true. ;return
		endif !if(j_otype(j_divtrans).ne.j_iptrans)   6810
		ivoul=j_trans_output(j_divtrans)
		noutv=j_o(ivoul)%i(1)
 
		if(noutv.gt.0)then
			j_divvars=j_deflistobject(j_divdata,'%vars',ivin=j_divkeep,nres=noutv+naddv)
			iper=j_putlistobject(j_divvars,ivin=ivoul)
			!function j_putlistobject(ivlist,single,list0,list,ivin,ignored)
		endif !if(noutv.gt.0)   6817
		j_distrans=.true.
 
	endif !if(nopt.eq.1)   6808
	if(j_disup)then
		if(nopt.lt.1)j_divvars=j_deflistobject(j_divdata,'%vars',ivin=j_divkeep,nres=naddv)
 
		iper=j_putlistobject(j_divvars,ivin=j_divkeepup)
		iper=j_putlistobject(j_divvars,single=j_o(ivup)%i(6))  !Unit variable
 
 
	endif !if(j_disup)   6825
 
 
 
 
	call j_getoption(iob,io,j_mfrom,-1,1,j_ipreal,.true.,nopt,j_optarg0)
	if(nopt.gt.0)then
		ifirst=j_v(j_optarg0(1))
		if(ifirst.lt.1.or.ifirst.gt.j_dnobs)then
			write(6,*)'from->  should be between 1 and ',j_dnobs,' it is ',ifirst
			j_err=.true.;return
		endif !if(ifirst.lt.1.or.ifirst.gt.j_dnobs)   6840
		j_dfrom=ifirst
	else !if(nopt.gt.0)then
		j_dfrom=1
	endif !if(nopt.gt.0)   6838
	call j_getoption(iob,io,j_muntil,-1,1,j_ipreal,.true.,nopt,j_optarg0)
	if(nopt.gt.0)then
		ifirst=j_v(j_optarg0(1))
		if(ifirst.lt.1.or.ifirst.gt.j_dnobs)then
			write(6,*)'last->  should be between 1 and ',j_dnobs,' it is ',ifirst
			j_err=.true.;return
		endif !if(ifirst.lt.1.or.ifirst.gt.j_dnobs)   6851
		j_duntil=ifirst
	else !if(nopt.gt.0)then
		j_duntil=j_dnobs
	endif !if(nopt.gt.0)   6849
 
	call j_getoption(iob,io,j_mprolog,-1,1,j_iptrans,.true.,nopt,j_optarg0)
	if(j_err)return
 
	if(nopt.eq.1)then
 
		if(j_otype(j_optarg0(1)).ne.j_iptrans)then
			call j_printname('prolog-> does not refer to a transformation: ',j_optarg0(1),' ')
			j_err=.true. ;return
		endif !if(j_otype(j_optarg0(1)).ne.j_iptrans)   6865
		call dotrans(j_optarg0(1),1)
		!	write(6,*)'{{',3
		if(j_err)return
	endif !if(nopt.eq.1)   6863
 
	call j_getoption(iob,io,j_mepilog,-1,1,j_iptrans,.true.,nopt,j_optarg0)
	if(j_err)return
	j_depilog=0
	if(nopt.eq.1)then
		j_depilog=j_optarg0(1)
		if(j_otype(j_depilog).ne.j_iptrans)then
			call j_printname('epilog-> does not refer to a transformation: ',j_depilog,' ')
			j_err=.true. ;return
		endif !if(j_otype(j_depilog).ne.j_iptrans)   6879
	endif !if(nopt.eq.1)   6877
	!		id(j_divobsup)=0 !how may upper observations done
	!when this is reached update upper level
	!	write(6,*)'<1po3444'
	return
end subroutine j_getdataobject !subroutine j_getdataobject(iob,io)
 
subroutine j_linkdata(ivup,ivsub)
	integer, intent(in)::ivup,ivsub
 
	double precision,dimension(:),allocatable ::newdata
	integer,dimension(:),allocatable ::newkeep,oldkeep,oldkeepiv
	integer, dimension(:), pointer :: keepup
	!	logical isupup
	ivkeepup=j_o(ivup)%i(2)
	nkeepup=j_o(ivkeepup)%i(1)
	ivobsup=j_o(ivup)%i(6)
	ivmatup=j_o(ivup)%i(1)
	ivnobswup=j_getobject(ivup,'%nobsw',j_ipreal)
	linknobswup=j_inlistobject(ivnobswup,ivkeepup)
 
	!	write(6,*)'linknobswup',linknobswup
	!	j_dnobswindex(level)=j_o(ivdatanow)%i(13)
	!j_o(j_divdata)%i(12)! level
	if(linknobswup.le.0)then
		call j_getname(ivnobswup,ivup)
		write(6,*)j_oname(1:j_loname),' is not in ',j_oname2(1:j_loname2)
		j_err=.true. ;return
	endif !if(linknobswup.le.0)   6907
	j_o(ivup)%i(4)=ivnobswup
	call j_getname(ivup,ivnobswup)
 
	!	write(6,*)'hellurei',j_oname(1:j_loname),ivup,j_oname2(1:j_loname2),ivnobswup
	nobsup=j_o(ivmatup)%i(1)
	!	write(6,*)'iiup',j_o(ivup)%i
	ivtop=j_topdata(ivup)
 
	!	call j_getname(ivup,ivsub,ivtop)
	!	write(6,*)'*********linkdata ',ivup,j_oname(1:j_loname),ivsub,j_oname2(1:j_loname2),&
	!	ivtop,j_oname3(1:j_loname3)
	!j_dnobswindex(level)=j_o(ivdatanow)%i(13)
	!	write(6,*)'ivtop,ivup',ivtop,ivup
 
	ivkeeptop=j_o(ivtop)%i(2)
 
	nkeeptop=j_o(ivkeeptop)%i(1)
 
	ivmattop=j_o(ivtop)%i(1)
	nobstop=j_o(ivmattop)%i(1)
	ivmatsub=j_o(ivsub)%i(1)
	nobssub=j_o(ivmatsub)%i(1)
	ivkeepsub=j_o(ivsub)%i(2)
	nkeepsub=j_o(ivkeepsub)%i(1)
	levelup=j_o(ivup)%i(12)
	!	call j_getobject(ivtop,'%nobswtot',j_ipreal,ivnobswtot)
	if(levelup.le.1)then
		if(allocated(j_o(ivup)%i2))deallocate(j_o(ivup)%i2)
		allocate(j_o(ivup)%i2(1:nobstop))
 
		call j_getname(ivup)
		!	write(6,*)'allo0cate ',nobstop,ivup,j_oname(1:j_loname)
		j_o(ivup)%i2=0
	endif !if(levelup.le.1)   6938
	!write(6,*)'ivnobswtot ,ivtopkeep ',ivnobswtot,ivkeeptop
	!	liwtot=j_inlistobject(ivnobswtot,ivkeeptop)
	!write(6,*)'liwtot** ',liwtot
	! if(liwtot.le.0)then  !now ivtop is ivup
	! liwtot=nkeepup  !old
	! nkeepup=liwtot+1  !
	! !	write(6,*)'nkeepup&',nkeepup
	!allocate(newdata(1:nobstop*nkeepup))
	!	write(6,*)'new ',nobstop,nkeep2,nobstop*nkeepup
	ibas=0
	ibas2=0
 
	!	do i=1,nobstop
	!	write(6,*)'i,nobstop,ntopkeep,nobstot*liwtot,linknobswup ', &
	!		i,nobstop,ntopkeep,nobstop*liwtot,liwtot
 
 
	! newdata(ibas2+1:ibas2+liwtot)
	!	j_o(ivtop)%i2(i)=j_o(ivmattop)%d(ibas+1:ibas+liwtot)
	! if(i.le.1)then
	! write(6,*)'*********************'
	! call j_getname(ivmattop)
	! write(6,*)'newdata,ibas,ibas2',newdata(ibas2+1:ibas2+liwtot)
 
	! endif !if(i.le.1)   6950
	!	if(ivtop.eq.ivup)then
	! !	write(6,*)'here ',ibas2,liwtot,ibas2+liwtot,ivtopmat,ibas,linknobswup,ibas+linknobswup
	! newdata(ibas2+nkeepup)=j_o(ivmattop)%d(ibas+linknobswup)
	! !else
	! !	stop 'here'
	! !newdata(ibas2+liwtot)=j_0
	! !endif !if(ivtop.eq.ivup)   6959
	! ibas=ibas+liwtot
	! ibas2=ibas2+nkeepup
	! enddo !i=1,nobstop   6947
	! call move_alloc(from=newdata, to=j_o(ivmattop)%d)
	! !	if(i.eq.1)write(6,*)'dmat',j_o(ivmattop)%d(1:nkeepup)
	! !	write(6,*)'ivtopkeep',ivkeeptop
	! iper=j_putlistobject(ivkeeptop,single=ivnobswtot)
	! nkeeptot=liwtot+1
	! elseif(liwtot.ne.nkeeptop)then
	! call j_getname(ivnobswtot,ivtopkeep)
	! write(6,*)' variable ',j_oname(1:j_loname),' should be the last variable in ',&
	! j_oname2(1:j_loname2), ' (i.e. made with linkdata)'
	! j_err=.true.;return
	! endif !if(liwtot.le.0)   6938
	nobstot=0
	ibas=0
	nobswmax=0
	if(ivtop.ne.ivup)then
		call j_getname(ivtop,ivup)
		write(6,*)'ivtop,ivup ',j_oname(1:j_loname),' * ',j_oname2(1:j_loname2)
		!		write(6,*)'nobstop ',nobstop,linknobswup,nkeeptop,nkeepup
		ibas=0
		ibas2=0
		do i=1,nobstop
			nobsw=j_o(ivmatup)%d(ibas2+linknobswup)
			!		if(i.le.10)write(6,*)'i nobsw ',i,nobsw
			nobto=j_o(ivmattop)%d(ibas+nkeeptop)*nobsw
			j_o(ivtop)%i2(i)=nobto
			nobswmax=max(nobto,nobswmax)
			nobstot=nobstot+j_o(ivtop)%i2(i)
			ibas=ibas+nkeeptop
			ibas2=ibas2+nkeepup
		enddo !i=1,nobstop   7001
 
	else
		!write(6,*)'nobstop,linknobswup,liwtot,ivupmat,nkeepup,j_o(ivupmat)%d',&
		!	nobstop,linknobswup,liwtot,ivupmat,nkeepup,j_o(ivupmat)%d
		do i=1,nobstop
			!		write(6,*)'ii',i,ibas,linknobswup
			nobsw=j_o(ivmatup)%d(ibas+linknobswup)
			!	j_o(ivdattop)%i2(i)= nobsw
			nobswmax=max(nobsw,nobswmax)
 
			nobstot=nobstot+nobsw
 
			ibas=	ibas+nkeepup
		enddo !i=1,nobstop   7015
		! if(nobstot.ne.nobssub)then
		! write(6,*)'*j* nobstot ',nobstot,' does not agree with nobssub ',nobssub
		! j_err=.true. ;return
		! endif !if(nobstot.ne.nobssub)   6991
 
 
	endif !if(ivtop.ne.ivup)   6995
	j_o(ivtop)%i(9)=nobswmax
	!	write(6,*)'nobswmax, nobstot',nobswmax,nobstot
	if(nobssub.ne.nobstot)then
		write(6,*)'the sum of %nobsw variable is ',nobstot,' but subdata has ',nobssub,' observations'
		j_err=.true.;return
 
 
	endif !if(nobssub.ne.nobstot)   7034
 
	!	call j_getname(ivsub)
	!	write(6,*)'sub ',j_oname(1:j_loname)
	!j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	!	j_o(iv)%i(5)=ivup
	!i(14)
	!i(12)=level
	!i(13) index of nobsw variable
 
	! nobssub=j_o(j_o(ivsub)%i(1))%i(1)
 
	! else !if(iilink.le.0)then
	! ntot=0
	! maxnobsw=0
	! ibas=0
	! do i=1,nobsup
	! !	call j_getobs0(ivup,i)
	! !	nobo=j_v(ivnobsw)
	! nobo=j_o(ivupmat)%d(ibas+linknobswup)
	! maxnobsw=max(maxnobsw,nobo)
	! ntot=ntot+nobo  ! int added 19.3.2019 by J.L.
	! ibas=ibas+nkeep
	! enddo !i=1,nobsup   6918
	! if(ntot.ne.nobssub)then
	! write(6,*)'**j_linkdata *sum of nobsw variable ',ntot
	! write(6,*)'does not agree with the number of observations in the subdata ',nobssub
	! j_err=.true.
	! return
	! endif !if(ntot.ne.nobssub)   6926
	! j_o(ivup)%i(9)=maxnobsw
	! !	write(6,*)'<418>maxnobsw',maxnobsw
	! endif !if(iilink.le.0)   6904
	!	ivobsw=j_igetopt(iob,io,j_mobsw)
	!if(ivobsw.le.0)then
 
	!	end if !if(ivobsw.le.0)  15442
	!end if !if(j_nargopt(iob,io,j_mnobsw).le.0)  15143
 
 
 
	!	call j_getoption(iob,io,j_mnobsw,-1,1,j_ipreal,.true.,narg,j_optarg0)
 
	!	call j_getobject(ivup,'%nobsw',j_ipreal,ivnobsw)
 
 
	!nobswindex=j_inlistobject(ivnobsw,ivkeepup)
	! if(nobswindex.le.0)then
	! call j_getname(-1,ivnobsw)
	! write(6,*)'nobsw variable ',j_oname2(1:j_loname2),' not in DATA ',j_oname(1:j_loname)
	! j_err=.true.;return
	! endif !if(nobswindex.le.0)   6952
	!	if(j_o(ivup)%i(12).eq.0)then
	!		keepup=>j_o(ivkeepup)%i2(1:nkeepup)
	ivobswup=j_getobject(ivup,'%obsw',j_ipreal)
	!	call j_getname(-1,ivobsw)
	!write(6,*)j_oname(1:j_loname),' is the # of node within ',j_oname(1:j_loname)
 
	!if(j_o(ivup))%i(12).eq.0)call j_getobject(ivup,'%obsw',j_ipreal,ivupobsw)
	ivobswsub=j_getobject(ivsub,'%obsw',j_ipreal)
	call j_getname(ivsub,ivobswsub)
	write(6,*)'obsw variable in ',j_oname(1:j_loname),' is ',j_oname2(1:j_loname2)
	j_o(ivup)%i(3)=ivsub;j_o(ivsub)%i(5)=ivup
	!j_o(ivdatanow)%i(13)
	j_o(ivsub)%i(7)=ivobswsub
	j_o(ivup)%i(7)=ivobswup
	!ivvars=j_o(ivsub)%i(11)
	ivvarsup=j_o(ivup)%i(11)
	iper=j_putlistobject(j_o(ivsub)%i(11),ivin=ivvarsup)
	iper=j_putlistobject(j_o(ivsub)%i(14),ivin=ivvarsup)
	!	call  j_deflistobject(ivsub,'%vars',iout,list0=newk,list=newkeep(1:newk))
	!j_o(ivsub)%i(11)=iout
	if(j_o(ivup)%i(12).eq.0)j_o(ivup)%i(12)=1  !12 level
	j_o(ivsub)%i(12)=j_o(ivup)%i(12)+1
	j_o(ivup)%i(13)=linknobswup
	j_o(ivsub)%i(13)=1
 
 
	!	write(6,*)'link2 ,nobswindex',linknobswup
 
	!	j_o(iv)%i(14)=j_o(ivmat)%i(1)  !number of observa
	!j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	!	j_o(iv)%i(5)=ivup
 
	!i(13) index of nobsw variable
 
	!	j_o(iv)%i(11)=ivvars
	!	j_o(iv)%i(14)=ivvars2
 
	!	 %i(11) is %vars getting upper level variables fro
	!if(present(ivcases))j_o(iv)%i(10)=ivcases
	! j_o(iv)%i(9)=ivsub;j_o(iv)%i(10)=ivnobsw;j_o(iv)%i(11)=ivup
	! j_o(iv)%i(12)=ivobs;j_o(iv)%i(13)=ivobsw;j_o(iv)%i(14)=ivnobswc
	!j_o(ivsub)%i(14)=j_o(ivup)%i(14)+j_o(ivmat)%i(1)
 
end subroutine
 
function j_datalevels(ivdata)
	integer,intent(in)::ivdata
	!j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	!	j_o(iv)%i(5)=ivup
	j_datalevels=j_o(ivdata)%i(12)
	ivdatac=ivdata
	do while (j_o(ivdatac)%i(3).ne.0)
		j_datalevels=j_datalevels+1
		ivdatac=j_o(ivdatac)%i(3)
	enddo !while (j_o(ivdatac)%i(3).ne.0)   7142
 
end function
 
function j_bottomdata(ivdata)
	integer,intent(in)::ivdata
	!j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	!	j_o(iv)%i(5)=ivup
 
	j_bottomdata=ivdata
	do while (j_o(j_bottomdata)%i(3).ne.0)
		j_bottomdata=j_o(j_bottomdata)%i(3)
	enddo !while (j_o(j_bottomdata)%i(3).ne.0)   7155
 
end function
 
function j_topdata(ivdata)
	integer,intent(in)::ivdata
	!j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	!	j_o(iv)%i(5)=ivup
	!	write(6,*)'ivdata ',ivdata,j_o(ivdata)%i
	j_topdata=ivdata
	do while (j_o(j_topdata)%i(5).ne.0)
		j_topdata=j_o(j_topdata)%i(5)
		!		write(6,*)'topdata ',j_topdata,j_o(j_topdata)%i
	enddo !while (j_o(j_topdata)%i(5).ne.0)   7167
	!	write(6,*)'topdata ',j_topdata,j_o(j_topdata)%i
end function
 
! subroutine j_getobsup(iobs)
! integer,intent(in)::iobs
! logical p
! p=iobs.le.2
! !	write(6,*)'iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat',iobs,j_disup
! !	write(6,*)j_o(j_dimat)%d(1:6)
 
 
 
! !	write(6,*)'iobs ',iobs,'obswinit',j_dobsw2(1:j_level),' nobsw ',j_dnobsw2(1:j_level)
 
! level0=j_level
! !	do level0=j_level,1,-1
! !	write(6,*)'level0  ',level0
! !	j_dobsw2(level0)=j_dobsw2(level0)+1
! !	write(6,*)'obsw nobsw',j_dobsw2(level0),j_dnobsw2(level0)
! !		write(6,*)'level,j_dimat2(level),j_diba2(level),j_dnobswindex(level)',&
! !	level0,j_dimat2(level0),j_diba2(level0),j_dnobswindex(level0)
! !		j_dnobsw2(level0)=j_o(j_dimat2(level0))%d(j_diba2(level0)+j_dnobswindex(level0))
! if(p)write(6,*)'*******************************'
! if(p)write(6,*)'iobs,level0 init',iobs,level0,j_dobsw2(level0),j_dnobsw2(level0)
! 700		if(j_dobsw2(level0).eq.j_dnobsw2(level0))then
! !		write(6,*)'level0',level0,j_dobsw2(level0),j_dnobsw2(level0)
! j_dobsw2(level0)=0
 
! !			write(6,*)j_oname(1:j_loname),j_diba2(level0-1)+ j_dnobswindex(level0-1),&
! !			j_o(j_dimat2(level0-1))%i(1),j_o(j_dimat2(level0-1))%i(2),j_o(j_dimat2(level0-1))%i(3)
 
! j_dnobsw2(level0)=j_o(j_dimat2(level0-1))%d(j_diba2(level0-1)+ j_dnobswindex(level0-1))
! call j_getname(j_dimat2(level0-1))
! if(p)write(6,*)'level0',level0,j_dnobsw2(level0),j_dimat2(level0-1),j_oname(1:j_loname)
! !		write(6,*)'level0here dnobs',level0, j_dnobsw2(level0),' from ',j_diba2(level0-1)+ j_dnobswindex(level0-1)
! level0=level0-1
! if(p)write(6,*)'uus level0 ,j_dnobsw2(level0)',level0,j_dnobsw2(level0)
! !	if(level0.gt.0)goto 800
 
! !		write(6,*)'here ',level0,j_dnobsw2(level0)
! if(level0.ge.j_dlevel0)goto 700
! endif !700		if(j_dobsw2(level0).eq.j_dnobsw2(level0))   7107
! !		write(6,*)'level0 ',level0
! 800		do level=level0,j_level
! if(p)write(6,*)'level,inloop',level
! j_dobsw2(level)=j_dobsw2(level)+1
! j_dobs2(level)=j_dobs2(level)+1
! j_v(j_divobs2(level))=j_dobs2(level)
 
! if(p)call j_getname(j_divobsw2(level))
! if(p)write(6,*)'level,obswiv',level,j_oname(1:j_loname),j_dobs2(level)
! j_v(j_divobsw2(level))=j_dobsw2(level)
! call j_getname(j_dimat2(level))
! if(p)write(6,*)'level matrix',level,j_oname(1:j_loname),j_diba2(level)+1
! if(p)write(6,*)j_o(j_dimat2(level))%d(1:20)
! j_v(j_o(j_divkeep2(level))%i2(1:j_dnkeep2(level)))= &
! j_o(j_dimat2(level))%d(j_diba2(level)+1:j_diba2(level)+ j_dnkeep2(level))
! j_diba2(level)=j_diba2(level)+j_dnkeep2(level)
! if(p)then
! do j=1,j_dnkeep2(level)
! call j_getname(j_o(j_divkeep2(level))%i2(j))
! write(6,*)j_oname(1:j_loname),j_v(j_o(j_divkeep2(level))%i2(j))
 
! enddo !j=1,j_dnkeep2(level)   7142
! !	write(6,*)'level,iba ',level, j_diba2(level)
! endif !if(p)   7141
! !		write(6,*)'level,diba2 ',	j_v(j_o(j_divkeep2(level))%i2(1:j_dnkeep2(level)))
! enddo !		do level=level0,j_level   7126
 
! ! if(level0.lt.0)level0=0
! ! !going down
! ! write(6,*)'level iba,',level0,j_diba2(0:1)
 
! ! do level=level0-1,j_level
 
! ! j_dobs2(level)=j_dobs2(level)+1
! ! j_dobsw2(level)=j_dobsw2(level)+1
 
! ! write(6,*)'levle,obs,obsw',level,j_dobs2(level),j_dobsw2(level)
 
! ! call j_getname(j_divobs2(level),j_divobsw2(level))
! ! write(6,*)level,j_oname(1:j_loname),j_dobs2(level), j_oname2(1:j_loname2),j_dobsw2(level)
! ! call j_getname(j_dimat2(level))
 
 
 
! end subroutine j_getobsup
 
! subroutine j_getnobswtot(ivdata)
! integer,intent(in)::ivdata
! logical p
 
! !	write(6,*)'iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat',iobs,j_disup
! !	write(6,*)j_o(j_dimat)%d(1:6)
! ivbot=j_bottomdata(ivdata)
! ivmat=j_o(ivbot)%i(1)
! nobs=j_o(ivmat)%i(1)
! j_dobsw2=0
! write(6,*)'nobs ',nobs
! !	write(6,*)'iobs ',iobs,'obswinit',j_dobsw2(1:j_level),' nobsw ',j_dnobsw2(1:j_level)
! do iobs=1,nobs
! p=iobs.le.3
! level0=j_level
! !	do level0=j_level,1,-1
! !	write(6,*)'level0  ',level0
! !	j_dobsw2(level0)=j_dobsw2(level0)+1
! !	write(6,*)'obsw nobsw',j_dobsw2(level0),j_dnobsw2(level0)
! !		write(6,*)'level,j_dimat2(level),j_diba2(level),j_dnobswindex(level)',&
! !	level0,j_dimat2(level0),j_diba2(level0),j_dnobswindex(level0)
! !		j_dnobsw2(level0)=j_o(j_dimat2(level0))%d(j_diba2(level0)+j_dnobswindex(level0))
! if(p)write(6,*)'*******************************'
! if(p)write(6,*)'iobs,level0 init',iobs,level0,j_dobsw2(level0),j_dnobsw2(level0)
! 700		if(j_dobsw2(level0).eq.j_dnobsw2(level0))then
! !		write(6,*)'level0',level0,j_dobsw2(level0),j_dnobsw2(level0)
! j_dobsw2(level0)=0
 
! !			write(6,*)j_oname(1:j_loname),j_diba2(level0-1)+ j_dnobswindex(level0-1),&
! !			j_o(j_dimat2(level0-1))%i(1),j_o(j_dimat2(level0-1))%i(2),j_o(j_dimat2(level0-1))%i(3)
! if(level0.gt.1)then
! if(j_dimat2(level0-1).le.0)write(6,*)'dimat ',j_dimat2(1:j_level)
! j_dnobsw2(level0)=j_o(j_dimat2(level0-1))%d(j_diba2(level0-1)+ j_dnobswindex(level0-1))
! call j_getname(j_dimat2(level0-1))
! if(p)write(6,*)'level0',level0,j_dnobsw2(level0),j_dimat2(level0-1),j_oname(1:j_loname)
! !		write(6,*)'level0here dnobs',level0, j_dnobsw2(level0),' from ',j_diba2(level0-1)+ j_dnobswindex(level0-1)
! level0=level0-1
! goto 700
! if(p)write(6,*)'uus level0 ,j_dnobsw2(level0)',level0,j_dnobsw2(level0)
! endif !if(level0.gt.1)   7201
! endif !700		if(j_dobsw2(level0).eq.j_dnobsw2(level0))   7195
! !		write(6,*)'level0 ',level0
! 800	do level=level0,j_level
! if(p)write(6,*)'level,inloop',level
! j_dobsw2(level)=j_dobsw2(level)+1
! !	j_dobs2(level)=j_dobs2(level)+1
! !	j_v(j_divobs2(level))=j_dobs2(level)
 
! if(p)call j_getname(j_divobsw2(level))
! if(p)write(6,*)'level,obswiv',level,j_oname(1:j_loname),j_dobs2(level)
! !	j_v(j_divobsw2(level))=j_dobsw2(level)
! !	call j_getname(j_dimat2(level))
! if(p)write(6,*)'level matrix',level,j_oname(1:j_loname),j_diba2(level)+1
! if(p)write(6,*)j_o(j_dimat2(level))%d(1:20)
! !	j_v(j_o(j_divkeep2(level))%i2(1:j_dnkeep2(level)))= &
! !		j_o(j_dimat2(level))%d(j_diba2(level)+1:j_diba2(level)+ j_dnkeep2(level))
! j_diba2(level)=j_diba2(level)+j_dnkeep2(level)
 
! !		write(6,*)'level,diba2 ',	j_v(j_o(j_divkeep2(level))%i2(1:j_dnkeep2(level)))
! enddo !	do level=level0,j_level   7213
! j_dobs2(1)=j_dobs2(level)+1
! !	write(6,*)'iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat',iobs,j_disup
! !	write(6,*)j_o(j_dimat)%d(1:6)
 
 
 
! !	write(6,*)'iobs ',iobs,'obswinit',j_dobsw2(1:j_level),' nobsw ',j_dnobsw2(1:j_level)
 
! iunit=j_dobs2(1)
! j_o(ivdata)%i2(iunit)=j_o(ivdata)%i2(iunit)+1
! ! if(level0.lt.0)level0=0
! ! if(level0.lt.0)level0=0
! if(p)write(6,*)'inutit',iunit,j_o(ivdata)%i2(iunit)
! enddo !iobs=1,nobs   7183
! write(6,*)j_o(ivdata)%i2
 
 
 
! end subroutine
 
subroutine j_getobs(iobs)
	integer*8,intent(in)::iobs
	integer*8 ::nsub
	!	write(6,*)'iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat',iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat
	!	write(6,*)j_o(j_dimat)%d(1:6)
	if(iobs.eq.j_dfrom)then
		j_diba=(j_dfrom-1)*j_dnkeep
 
		if(j_disup)then
			j_dibaup=0
			nsub=1
			j_dobsup=1
			do while(nsub.lt.j_dfrom)
				nsub=nsub+j_o(j_dimatup)%d(j_dibaup+1)
				j_dibaup=j_dibaup+j_dnkeepup
				j_dobsup=j_dobsup+1
			enddo !while(nsub.lt.j_dfrom)   7351
			j_v(j_divobsup)=j_dobsup
			j_dnextobs=nsub-j_dfrom+1  !testattava
		endif !if(j_disup)   7347
		!	write(6,*)'iobsfro',iobs,j_dfrom,j_dnkeep,j_nkeepup,j_dobsup,j_dnextobs
	endif !if(iobs.eq.j_dfrom)   7344
	if(j_disup)then
 
		if(iobs.eq.j_dnextobs)then
			! if(j_dibaup+ j_dnkeepup.gt.392987)then
			! call j_getname(j_dimatup)
			! write(6,*)iobs,j_dnkeepup,j_oname(1:j_loname),' ',j_o(j_dimatup)%i(1:3), &
			! ' obsup ',j_dobsup
 
			!endif !if(j_dibaup+ j_dnkeepup.gt.392987)   7282
			j_v(j_o(j_divkeepup)%i2(1:j_dnkeepup))=j_o(j_dimatup)%d(j_dibaup+1:j_dibaup+ j_dnkeepup)
			j_dnextobs=j_dnextobs+ j_o(j_dimatup)%d(j_dibaup+1)  !  j_v(j_divnobsw)
			j_dibaup=j_dibaup+j_dnkeepup
			j_v( j_divobsw)=1
			j_v(j_divobsup)=j_dobsup
			j_dobsup=j_dobsup+1
 
		else !if(iobs.eq.j_dnextobs)then
			j_v( j_divobsw)=j_v( j_divobsw)+1
		endif !if(iobs.eq.j_dnextobs)   7363
 
 
	endif !if(j_disup)   7361
	! write(6,*)'<55',j_diba,j_dnkeep,j_o(j_divkeep)%i2(1:j_dnkeep)
	!	write(17,*)iobs,j_diba,j_dnobs8,iobs*j_dnkeep,j_o(j_dimat)%i(3)
	j_v( j_o(j_divkeep)%i2(1:j_dnkeep))= &
		j_o(j_dimat)%d(j_diba+1:j_diba+ j_dnkeep)
	j_diba=j_diba+j_dnkeep
	if(j_distrans)then
		j_v(j_divobs)=iobs
		call dotrans(j_divtrans,1)
		if(j_err)write(6,*)'error in transformation for Obs ',iobs
	endif !if(j_distrans)   7388
	j_rejected=.false.
	!	j_dapu=j_codevalue(j_diob,j_dfilterlink)
	if( j_disfilter)j_rejected=j_codevalue(j_diob,j_dfilterlink).eq.j_0
	!if(iobs.le.5.and.j_disfilter)write(6,*)'<455',j_disfilter,j_dfilterlink,j_codevalue(j_diob,j_dfilterlink)
 
	if(j_disreject)j_rejected=j_rejected.or.j_codevalue(j_diob,j_drejectlink).ne.j_0
	return
end subroutine !subroutine j_getobs(iobs)
 
! subroutine j_getobs20()
 
! !	write(6,*)'iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat',iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat
! !	write(6,*)j_o(j_dimat)%d(1:6)
! do iobs=1,j_dfrom-1
! level0=j_level
! !	do level0=j_level,1,-1
! !	write(6,*)'level0  ',level0
! !	j_dobsw2(level0)=j_dobsw2(level0)+1
! !	write(6,*)'obsw nobsw',j_dobsw2(level0),j_dnobsw2(level0)
! !		write(6,*)'level,j_dimat2(level),j_diba2(level),j_dnobswindex(level)',&
! !	level0,j_dimat2(level0),j_diba2(level0),j_dnobswindex(level0)
! !		j_dnobsw2(level0)=j_o(j_dimat2(level0))%d(j_diba2(level0)+j_dnobswindex(level0))
! 700		if(j_dobsw2(level0).eq.j_dnobsw2(level0))then
! j_dobsw2(level0)=0
! j_dnobsw2(level0)=j_o(j_dimat2(level0-1))%d(j_diba2(level0-1)+ &
! j_dnobswindex(level0-1))
! level0=level0-1
 
! goto 700
! endif !700		if(j_dobsw2(level0).eq.j_dnobsw2(level0))   7318
! !		write(6,*)'level0 ',level0
! 800		do level=level0,j_level
! j_dobsw2(level)=j_dobsw2(level)+1
! j_dobs2(level)=j_dobs2(level)+1
 
! j_diba2(level)=j_diba2(level)+j_dnkeep2(level)
 
! enddo !		do level=level0,j_level   7327
! enddo !iobs=1,j_dfrom-1   7309
 
! return
! end subroutine !subroutine j_getobs(iobs)
 
 
 
subroutine j_getobswup(iobs,ivdata)
	integer,intent(in)::iobs,ivdata
	logical isup
 
	!	write(6,*)'iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat',iobs,j_dfrom,j_dnkeep,j_disup,j_dnextobs,j_dimat
	!	write(6,*)j_o(j_dimat)%d(1:6)
	save
 
 
	if(iobs.eq.1)then
		!j_o(iv)%i(5)=ivup;
		imat=j_o(ivdata)%i(1)
		ivkeep=j_o(ivdata)%i(2)
		nkeep=j_o(ivkeep)%i(1)
 
		ivup=j_o(ivdata)%i(5)
		isup=ivup.ne.0
 
 
		if(isup)then
			nsub=1
			nup=0
			nextobs=nsub
			ivkeepup=j_o(ivup)%i(2)
			nkeepup=j_o(ivkeepup)%i(1)
			ivobsup=j_o(ivup)%i(6)
			imatup=j_o(ivup)%i(1)
			ivnobsw=j_o(ivup)%i(4)
			ivobsw=j_o(ivdata)%i(7)
			call j_getname(ivobsw)
			!		write(6,*)'ivobsw ',j_oname(1:j_loname)
 
			ibaup=0
		endif !if(isup)   7457
		!	write(6,*)'wup imat,ivkeep,isup,nsub,ibau ',imat,ivkeep,isup,nsub,ibaup
	endif !if(iobs.eq.1)   7447
	if(isup)then
		if(iobs.eq.nextobs)then
			j_v(j_o(ivkeepup)%i2(1:nkeepup))=j_o(imatup)%d(ibaup+1:ibaup+ nkeepup)
			nextobs=nextobs+j_v(ivnobsw)
			ibaup=ibaup+nkeepup
			j_v( ivobsw)=1
			nup=nup+1
		else !if(iobs.eq.j_dnextobs)then
			j_v( ivobsw)=j_v( ivobsw)+1
		endif !if(iobs.eq.nextobs)   7475
		j_v(ivobsup)=nup
 
	endif !if(isup)   7474
 
	j_v( j_o(ivkeep)%i2(1:nkeep))= &
		j_o(imat)%d(iba+1:iba+ nkeep)
	iba=iba+nkeep
 
 
	return
end subroutine j_getobswup!subroutine j_getobs(iobs)
 
 
function j_nobsdata(ivdata) !number of obs in data
	integer, intent(in):: ivdata
	if(ivdata.le.0.or.ivdata.gt.j_namedv)then
		write(6,*)'j* illegal argument ',ivdata,' for nobbsdata'
		j_err=.true.;return
	endif !if(ivdata.le.0.or.ivdata.gt.j_namedv)   7499
	if(j_otype(ivdata).ne.j_ipdata)then
		call j_printname(' ',ivdata, ' is not data object')
		j_err=.true.
		return
	endif !if(j_otype(ivdata).ne.j_ipdata)   7503
	j_nobsdata=j_o( j_o(ivdata)%i(1))%i(1)
	return
end function j_nobsdata !function j_nobsdata(ivdata)
 
 
subroutine j_getdots(i1,name2,n) ! get varaible list from ... , if(new) can generate vars
	! i1 is not included but i2 is
	integer, intent(in):: i1
	character*(*),intent(in)::name2
	integer, intent(out):: n
	character(len=j_lenobject) :: name1,name
 
	integer,parameter::nmaxtot=1000
	!	integer, allocatable,dimension(:),intent(inout):: list
	integer, allocatable,dimension(:)::templist
	character*4 num
	le2=len(name2)
	if(.not.allocated(j_dotlist))allocate(j_dotlist(1:100))
	!	write(6,*)'getdots,i1,i2,n',i1,i2,n
 
	n=0
	if(i1.gt.j_namedv)then
		write(6,*)'**illegal ...';j_err=.true.;return
	end if !if(i1.gt.j_namedv)   7529
	call j_getline(j_ivnames,i1,name1,le1)
	if(name1(1:le1).eq.name2)then
		!write(6,*)'n=-1'
		n=0
		return
	endif !if(name1(1:le1).eq.name2)   7533
 
	!	call j_getline(j_ivnames,i2,name2,le2)
	ibu=1
	!	write(6,*)'<765>i1,le1,le2,name1(1:le1),name2(1:le2)', &
	!i1,le1,le2,name1(1:le1),name2(1:le2)
	!	if(le2.lt.le1)goto 99
	ial0=1
	!write(6,*)'name1,name2',name1(1:le1),'*',name2(1:le2)
17	continue
	do ial=ial0,le1
		if(ial.gt.le2)then
			write(6,*)'**illegal ...';j_err=.true.;return
		endif !if(ial.gt.le2)   7548
 
		if(name1(ial:ial).eq.name2(ial:ial).and.(name1(ial:ial).lt.'0'.or. &
			name1(ial:ial).gt.'9'))cycle !bypass initial letters
		if(name1(ial:ial).ne.name2(ial:ial).and.(name1(ial:ial).lt.'0'.or.name1(ial:ial).gt.'9'.or.&
			name2(ial:ial).lt.'0'.or.name2(ial:ial).gt.'9'))goto 99 !then letter
 
		exit
	enddo !ial=ial0,le1   7547
 
	!	write(6,*)'ial ',ial
 
 
	if(ial.le.le1)then
		do ilo=ial,le1
			if(name1(ilo:ilo).ge.'0'.and.name1(ilo:ilo).le.'9')cycle
			exit
		enddo !ilo=ial,le1   7564
		ilop1=ilo-1
	endif !if(ial.le.le1)   7563
 
 
	if(ial.le.le2)then
		do ilo=ial,le2
			if(name2(ilo:ilo).ge.'0'.and.name2(ilo:ilo).le.'9')cycle
			exit
		enddo !ilo=ial,le2   7573
		ilop2=ilo-1
	endif !if(ial.le.le2)   7572
	if(ilop1.gt.le1)then
		write(6,*)'illegal ... '
		j_err=.true.
		return
	endif !if(ilop1.gt.le1)   7579
	if(name1(ial:ilop1).eq.name2(ial:ilop2))then
		ial0=ilop1+1
		goto 17
	endif !if(name1(ial:ilop1).eq.name2(ial:ilop2))   7584
 
	if(ilop1.lt.le1.and.ilop2.lt.le2)then
		if(name1(ilop1+1:le1).ne.name2(ilop2+1:le2))then
			write(6,*)'illegal ... ',name1(ilop1+1:le1), ' is not equal to ',&
				name2(ilop2:le2)
			j_err=.true.
			return
		endif !if(name1(ilop1+1:le1).ne.name2(ilop2+1:le2))   7590
	elseif(ilop1.lt.le1.or.ilop2.lt.le2)then
		write(6,*)'illegal ... '
		j_err=.true.
		return
 
	endif !if(ilop1.lt.le1.and.ilop2.lt.le2)   7589
 
 
77	read(name1(ial:ilop1),'(i4)',err=88)ii1
	goto 78
88	write(6,*)'error when reading ',name1(ial:ilop1)
	j_err=.true.;return
 
78	read(name2(ial:ilop2),'(i4)',err=89)ii2
	goto 79
89	write(6,*)'error when reading ',name2(ial:ilop2)
	j_err=.true.;return
79	continue

	!	write(6,*)'dot:ii1,ii2,0',ii1,ii2

	nn=abs(ii2-ii1-1)
	if(nn.gt.nmaxtot)then
		write(6,*)'trying to generate ',nn,' in ... ',nmaxtot,' allowed'
		j_err=.true.
		return
	endif !if(nn.gt.nmaxtot)   7618
	!list(1)=i1
 
	!	write(6,*)'<567>',ii1,ii2
	istep=1
	if(ii1.gt.ii2)istep=-1
	do ili=ii1+istep,ii2-istep,istep
		write(num,'(i4)')ili
		do lu=1,4
			if(num(lu:lu).ne.' ')exit
		enddo !lu=1,4   7630
		!write(6,*)'<47',lu,num
		n=n+1
		call testdots(n)
		if(ilop1.lt.le1)then
			name=name1(1:ial-1)//num(lu:4)//name1(ilop1+1:le1)
		else !if(ilop1.lt.le1)then
			name=name1(1:ial-1)//num(lu:4)
		endif !if(ilop1.lt.le1)   7636
		lena=len_trim(name)
		!write(6,*)'name(1:lena) ',name(1:lena)
		iv=j_object(name(1:lena))
		if(iv.le.0)iv=j_getobject(0,name(1:lena),j_ipreal)
		j_dotlist(n)=iv
		!write(6,*)'<577>ili,n,list(n)',ili,n,list(n)
	enddo !ili=ii1+istep,ii2-istep,istep   7628
	n=n+1
	call testdots(n)
	i2=j_object(name2)
	if(i2.le.0)i2=j_getobject(0,name2,j_ipreal)
	j_dotlist(n)=i2
	!write(6,*)'dot:n,list',n,j_dotlist(1:n)
	!write(6,*)'<779> n',n,iv
	return
 
 
99 if(le1.ne.le2)then
		write(6,*)'*dots for letters must be equal size  ',name1(1:le1),'...',name2(1:le2)
		j_err=.true.
		return
	endif !99 if(le1.ne.le2)   7658
 
	if(ial.lt.le1)then
		if(name1(ial+1:le1).ne.name2(ial+1:le1))then
			write(6,*)'end ',name1(ial+1:le1),' differs from ',name2(ial+1:le1)
			j_err=.true. ;return
		endif !if(name1(ial+1:le1).ne.name2(ial+1:le1))   7665
	endif !if(ial.lt.le1)   7664
	if(.not.(name1(ial:ial).ge.'A'.and.name2(ial:ial).le.'Z'.or.&
			name1(ial:ial).ge.'a'.and.name2(ial:ial).le.'z'))then
		write(6,*)'if letters change in ... they mus be either between A and Z or between a and z'
		j_err=.true.
		return
	endif !if(.not.(name1(ial:ial).ge.'A'.and.name2(ial:ial).le.'Z'.   7670
	istep=1
	if(name1(ial:ial).gt.name2(ial:ial))istep=-1
 
	!first is obtained earlier
 
 
	do i=ichar(name1(ial:ial))+istep,ichar(name2(ial:ial))-istep,istep
		!	write(6,*)'i ',i,' ',char(i)
		if(ial.lt.le1)then
			name=name1(1:ial-1)//char(i)//name1(ial+1:le1)
		else
			name=name1(1:ial-1)//char(i)
		endif !if(ial.lt.le1)   7684
		iv=j_object(name(1:le1))
		!	write(6,*)'name(1:le1) ',name(1:le1)
		if(iv.le.0)iv=j_getobject(0,name(1:le1),j_ipreal)
		n=n+1
		j_dotlist(n)=iv
		!	write(6,*)'n,name,iv',n,name(1:le1),iv
 
	enddo !i=ichar(name1(ial:ial))+istep,ichar(name2(ial:ial))-istep,   7682
	n=n+1
	iv=j_object(name2(1:le1))
	iv=j_getobject(0,name2(1:le1),j_ipreal)
	!write(6,*)'n,name2(1:le1),iv',n,name2(1:le1),iv
	j_dotlist(n)=iv
	return
 
	contains
	subroutine testdots(nn)
		if(nn.gt.size(j_dotlist))then
			nmax=size(j_dotlist)
			if(allocated(templist))deallocate(templist)
			allocate(templist(1:nmax))
			templist=j_dotlist
			deallocate(j_dotlist)
			allocate(j_dotlist(1:2*nmax))
			j_dotlist(1:nmax)=templist
			deallocate(templist)
 
		endif !if(nn.gt.size(j_dotlist))   7706
 
	end subroutine
end subroutine j_getdots !subroutine j_getdots(i1,i2,list,n,nmax)
 
 
subroutine j_zerondo()
 
	j_ndo_loop=0
	return
end subroutine j_zerondo !subroutine j_zerondo()
 
!20141203 oli: subroutine jcompil(input,ivteku,oneline,newin,ivinl,ivoutl,matrix,localin,localout) !trasnforamtion interpreter
 
 
subroutine j_tracecheck(iv)
	!module tracemod
	! end module
 
	!module putmod
	!use j_globalfuncsmod, only: &
	! puti, &
	! putl, &
	! putim, &
	! putv
	!end module
 
	!use j_globalfuncsmod, only : printname
 
	!module vmod
	!end module vmod
 
	!!use printnamemod
 
	integer, intent(in):: iv
 
	! call printname('tracecheck ',iv,' ')
	if(j_ivtracevars.le.0)return
	!integer ::ntraced
	!integer, dimension(:),pointer::traceline,traceiv
	ii=j_inlistobject(iv,j_ivtracevars)
	! write(6,*)'ii,',ii
	if(ii.gt.0)then
		if(j_o(j_ivtracestatus)%d(ii).gt.0)then
			j_ntraced=j_ntraced+1
			call j_puti(j_traceii,j_ntraced,ii)
		endif !if(j_o(j_ivtracestatus)%d(ii).gt.0)   7759
	endif !if(ii.gt.0)   7758
	!write(6,*)'ntraced ',ntraced
	return
end subroutine j_tracecheck !subroutine j_tracecheck(iv)
 
 
!20141210 oli: subroutine nextop(inp,icur,last,oper,noper,ipos,ipos1,ioper,isq)
subroutine j_nextop(inp,icur,last,oper,noper,ipos,ipos1,ioper)
	! inp =string
	! icur = initiallyu where to satrt, in output initial bvalnks are ignored
	! last= last charcater
	! oper = list of operations
	! noper =amount of oper
	! ipos =position of the operation
	! ipos1= the end of preceding string i.e. the can be spaces between string and operation
	! ioper= index of operation
	! isq is there '?' within the string
 
	character*(*), intent(inout):: inp
	character*(*), intent(in):: oper(*)
	integer, intent(inout):: icur
	integer, intent(in):: last, noper
	integer, intent(out):: ipos,ipos1,ioper
 
	!write(6,*)'icur,last',icur,last,'inp',inp
	!write(6,*)'inpicur;',inp(icur:icur)
 
	!20141210 oli: isq=0  ! question mark
	!write(6,*)'hep'
	!do i=icur,last
	!chacter constants bypassed
	!bypass spaces etc initially
	!write(6,*)'icur',icur
77 if(inp(icur:icur).le.' ')then
		if(icur.ge.last)return
		icur=icur+1
		goto 77
	end if !77 if(inp(icur:icur).le.' ')   7796
	i=icur
	!20140618 (,  ,) virheet
	!if(inp(i:i)=='('.and.inp(i+1:i+1)==','.or.inp(i:i)==','.and.inp(i+1:i+1)==')') then
	!  write(6,*)'***syntax error 7'
	!	j_err=.true.
	!	return
	!endif
	!write(6,*)'i=icur',i
	ipos1=icur
	!
 
100 continue
	!write(6,*)'i=',i
	if(inp(i:i).eq.'[')then
		!write(6,*)'[',i
		iclos=index(inp(i+1:last),']')
		if(iclos.le.0)then
			write(6,*)'*no closing ]'
			j_err=.true.
			return
		endif !if(iclos.le.0)   7817
		i=iclos+i+1
		ipos1=i-1
		!write(6,*)'nyt:',inp(i:last),'i=',i,' last=',last
		if(i.gt.last)goto 70
		goto 100
	endif !if(inp(i:i).eq.'[')   7814
 
	!20141210 oli: if(inp(i:i).eq.'?')then; isq=i;i=i+1;goto 100; end if
 
	if(inp(i:i).eq."'")then
		!write(6,*)'<358i,last',i,last
		!write(6,*)'inp ',inp
		!write(6,*)'last ',last
		do j=i+1,last
			!	if(inp(j:j).eq."'")write(6,*)'<48j',j
			if(inp(j:j).eq."'")goto 17
			!    write(6,*)inp(j:j),ichar(inp(j:j)),ichar('¤'),iachar(inp(j:j)),iachar('¤')
			if(inp(j:j).eq.'~'.OR.inp(j:j).eq.'|')inp(j:j)="'" ! ´´´´`´´ <<<||||||||||
		enddo !j=i+1,last   7835
		goto 70  !termination
17 i=j+1   !;goto 100
 !  write(6,*)'inp ',inp
 !  write(6,*)'i,last',i,last
		if(i.gt.last)goto 70   ! no ope found
		ipos1=j
	endif !if(inp(i:i).eq."'")   7831
	!write(6,*)'icur,ipos1,i,j',icur,ipos1,i,j
	if(inp(i:i).le.' ')then
		i=i+1
		goto 100
	endif !if(inp(i:i).le.' ')   7849
 
	! special mark
	if(inp(i:i).lt.'0'.or.(inp(i:i).gt.'9'.and.inp(i:i).lt.'A').or. &
			(inp(i:i).gt.'Z'.and.inp(i:i).lt.'a').or.inp(i:i).gt.'z')then
		!write(6,*)'inp:',inp,len(inp),'i=',i
		do j=1,noper
			le2=len_trim(oper(j))
			! if(len(inp).lt.last)write(16,*)'last,en',last,len(inp),':',inp(1:len(inp))
			! j,i,le2,last          19          26           6          31
			if(i+le2-1.gt.last)cycle
			if(inp(i:i+le2-1).eq.oper(j)(1:le2))then
				! 2.e-1  or  2e-7
				if(i.ge.3.and.(oper(j)(1:le2).eq.'-'.or.oper(j)(1:le2).eq.'+'))then
					if(i+1.gt.len(inp))then
						!write(6,*)'<2245>',inp
						return
					endif !if(i+1.gt.len(inp))   7866
					if((inp(i-1:i-1).eq.'e'.or.inp(i-1:i-1).eq.'E').and. &
						inp(i+1:i+1).ge.'0'.and.inp(i+1:i+1).le.'9'.and.&
						((inp(i-2:i-2).ge.'0'.and.inp(i-2:i-2).le.'9').or.inp(i-2:i-2).eq.'.'))cycle
				endif !if(i.ge.3.and.(oper(j)(1:le2).eq.'-'.or.oper(j)(1:le2).eq.   7865
				ipos=i
				ioper=j
				! to treat ==
				if(ioper.eq.20)then
					if(last.ge.i+1.and.inp(i+1:i+1).eq.'=')ioper=25
				endif !if(ioper.eq.20)   7877
				! if(ioper.eq.iops)write(6,*)'iops',i,i+le2-1,input
				return
			endif !if(inp(i:i+le2-1).eq.oper(j)(1:le2))   7863
		enddo !j=1,noper   7858
		if(i.gt.ipos1+1)then
			write(6,*)'**illegal special marks after:',inp(1:ipos1)
			j_err=.true.
			!write(6,*)'ipos1,i,icur',ipos1,i,icur
			return
		endif !if(i.gt.ipos1+1)   7884
	endif !if(inp(i:i).lt.'0'.or.(inp(i:i).gt.'9'.and.inp(i:i).lt.'A'   7855
 
	ipos1=i
	i=i+1
 
	if(i.le.last)goto 100
	70 ipos=last+1
	ipos1=last
	ioper=0
	! write(6,*)'return,icur',icur
	return
end subroutine j_nextop !subroutine j_nextop(inp,icur,last,oper,noper,ipos,ipos1,ioper)
 
 
!20150812(arg1<->arg2) oli: subroutine deflist2(name,iv,list,ivout)  ! define a list object, and put list to it
! ! subroutine j_deflist2(iv,name,list,ivout)  ! define a list object, and put list to it
! module vmod
! end module vmod
 
! module typemod
! ipobj  ! & first objec ????
! !typec
! end module
 
 
! ! character*(*), intent(in):: name
! ! integer, intent(in):: iv
! ! integer, intent(in):: list(0:*)  !starts from io
! ! integer, intent(out):: ivout
 
! 20150812(arg1<->arg2) oli: 	call getv(name,iv,iplist,ivout) !call getv2(iplist,ivout(teku),ior,)
! call deflist(iv, !call getv2(iplist,ivout(teku),ior,)
! 20140522 virheenkäsittely
! if(j_err) return
 
! ! call j_deflist(iv,name,ivout,nres=list(0))
! ! write(6,*)'3773iv',iv,name,list(0)
! ! if(j_err) return
! o(ivout)%i(0)=list(0)
! do i=1,list(0);o(ivout)%i(i)=list(i);end do
! ! j_o(ivout)%i2(1:list(0))=list(1:list(0))
! ! return
! ! end subroutine j_deflist2 !subroutine j_deflist2(iv,name,list,ivout)
 
 
!20150812(arg1<->arg2) oli: subroutine deflist(name,iv,list0,ivout)
! subroutine j_deflist(iv,name,list0,ivout,list2) !allocates list object with size list0,
! ! but put it as empty
 
 
! !module vmod
! !end module vmod
 
! !module typemod
! !ipobj  ! & first objec ????
! ! !typec
! !end module
 
 
! character*(*), intent(in):: name
! integer, intent(in):: iv
! integer, intent(in):: list0
! integer, intent(out):: ivout
! logical,intent(in),optional::list2
 
! !20150812(arg1<->arg2) oli: 	call getv(name,iv,iplist,ivout) !call getv2(iplist,ivout(teku),ior,)
! call j_getobject(iv,name,j_iplist,ivout) !call getv2(iplist,ivout(teku),ior,)
! !20140522 virheenkäsittely
 
! if(j_err) return
! if(present(list2))then
! if(list2)j_otype(ivout)=j_iplist2
! endif !if(present(list2))then
! allocate( j_o(ivout)%i2(1:list0))
! allocate( j_o(ivout)%d(1:list0))
! allocate( j_o(ivout)%i(1:13)
! j_o(ivout)%i=0
! j_o(ivout)%i2=0
! j_o(ivout)%d=j_0
! !o(ivout)%i(-1)=list0;
! j_o(ivout)%i(1)=list0
! j_o(ivout)%i(2)=1
! j_o(ivout)%i(3)=list0
 
! return
! end subroutine j_deflist !subroutine j_deflist(iv,name,list0,ivout,list2)
 
function j_deflistobjectinp(iv,name,inp)
	!makes a list from character variable inp which contains the names of objects separated with commas
	character*(*), intent(in):: inp,name
	integer, intent(in):: iv
	!	integer, intent(out):: ivout
	integer,dimension(:),allocatable :: list
	lop=len(inp)
	!20150812(arg1<->arg2) oli: 	call getv(name,iv,iplist,ivout) !call getv2(iplist,ivout(teku),ior,)
	!	call j_getobject(iv,name,j_ipreal,ivout) !call getv2(iplist,ivout(teku),ior,)
	!20140522 virheenkäsittely
 
	nl=1+lop/2
	allocate(list(1:nl))
	nv=0
	ial=1
	ipil=j_nextlim(inp,ial,lop,',')
	!write(6,*)'<3553',ipil,lop,inp(1:lop)
	do while(.true.)
		iv0=j_object(inp(ial:ipil-1))
		if(iv0.le.0)iv0=j_getobject(0,inp(ial:ipil-1),j_ipreal)
		if(j_err)goto 99
		write(6,*)ipil,nv,ipil,ial
		nv=nv+1
		list(nv)=iv0
		if(ipil.gt.lop)exit
		ial=ipil+1
		ipil=j_nextlim(inp,ial,lop,',')
	enddo !while(.true.)   7994
 
	!	call j_command(j_object_name(ivout,j_leno(ivout))//'=list('//inp//')',.true.)  !options are passed through
	!write(6,*)'nv',nv,list
	j_deflistobjectinp=j_deflistobject(iv,name,list0=nv,list=list(1:nv))
	!write(6,*)j_o(iv)%i(1),j_o(iv)%i2
99	deallocate(list)

	return
end function j_deflistobjectinp !subroutine j_deflistobjectinp(iv,name,ivout,inp)
 
 
!20150812(arg1<->arg2) oli: subroutine deflistopt(name,iv,list0,list,ivout)
! subroutine j_deflistopt(iv,name,list0,list,ivout)
! !allocates list object with size list0, and put list (strating from 1)
! !module vmod
! !end module vmod
 
! !module typemod
 
! character*(*), intent(in):: name
! integer, intent(in):: list(1:list0)
! integer, intent(in):: iv,list0
! integer, intent(out):: ivout
 
! !20150812(arg1<->arg2) oli: 	call getv(name,iv,iplist,ivout) !call getv2(iplist,ivout(teku),ior,)
 
 
! !o(ivout)%i(-1)=list0;
! j_o(ivout)%i(0)=list0
! j_o(ivout)%i(1:list0)=list
! return
! end subroutine j_deflistopt !subroutine j_deflistopt(iv,name,list0,list,ivout)
 
 
! subroutine j_defmergelist(iv,name,list,list2,single,ivout) ! merging two lists+object single
! ! output list can be same as the second list
! ! single single object to be addied to the output list if it is not in the input lists
 
! integer, intent(in):: iv
! character*(*), intent(in):: name
! integer, intent(in)::  single
! integer, intent(in):: list(0:*)  !starts from io
! integer, intent(in):: list2(0:*)  !starts from io
! integer, intent(out):: ivout
 
! integer, dimension(:),pointer::temp=>null()
 
! allocate(temp(1:list2(0)))
! new=0
! do i=1,list2(0)
! j=j_inlist(list2(i),list)
! if(j.le.0)then ;new=new+1;temp(new)=list2(i) ;end if
! end do !do i=1,list2(0)
! if(single.ne.0)then
! js=j_inlist(single,list)
! if(js.le.0)js=j_inlist(single,list2)
! if(js.eq.0)js=1
! else !if(single.ne.0)then
! js=0
! end if !if(single.ne.0)then
! !20150812(arg1<->arg2) oli: 	call getv(name,iv,iplist,ivout) !call getv2(iplist,ivout(teku),ior,)
! !call j_getobject(iv,name,j_iplist,ivout) !call getv2(iplist,ivout(teku),ior,)
! !20140522 virheenkäsittely
! !	if(j_err) return
 
! call j_deflist(iv,name,new+list(0)+js,ivout)
! !allocate( j_o(ivout)%i(0:new+list(0)+js))
! !o(ivout)%i(0)=list(0)
! !do i=1,list(0);o(ivout)%i(i)=list(i);end do
! j_o(ivout)%i2(1:list(0))=list(1:list(0))
! j_o(ivout)%i2(list(0)+1:list(0)+new)=temp(1:new)
! if(js.gt.0)j_o(ivout)%i2(list(0)+new+1)=single
! !j_o(ivout)%i(0)=new+list(0)+js
! deallocate(temp)
! return
! end subroutine j_defmergelist !subroutine j_defmergelist(iv,name,list,list2,single,ivout)
 
 
logical function j_isletter(ch) ! is letter ?
	character*1, intent(in):: ch
	j_isletter=(ch.ge.'A'.and.ch.le.'Z').or.(ch.ge.'a'.and.ch.le.'z').or.&
		ch.eq.'%'.or.ch.eq.'$'.or.ch.eq.'#'.or.ch.eq.'_'
	return
end function j_isletter !logical function j_isletter(ch)
 
 
logical function j_isnumber(ch) ! is number or decimal point followdd by number and not preced by letter
	character*3, intent(in):: ch
	j_isnumber=ch(2:2).ge.'0'.and.ch(2:2).le.'9'.or.(ch(2:2).eq.'.'.and.ch(3:3).ge.'0'.and.ch(3:3).le.'9'  &
		.and..not.j_isletter(ch(1:1)))
end function j_isnumber !logical function j_isnumber(ch)
 
 
 
logical function j_istrans(iv) ! is iv a transformation  !ei ehkä ole tar
 
	!module vmod
	!end module vmod
 
	integer, intent(in):: iv
 
	if(iv.le.0.or.iv.gt.j_namedv)then
		j_istrans=.false.
	else if(j_otype(iv).ne.j_iptrans)then !if(iv.le.0.or.iv.gt.j_namedv)then
		j_istrans=.false.
	else !if(iv.le.0.or.iv.gt.j_namedv)then
		j_istrans=.true.
	endif !if(iv.le.0.or.iv.gt.j_namedv)   8106
	return
end function j_istrans !logical function j_istrans(iv)
 
integer function j_outputlist(ivtrans) !outputlist of transformation set ivtrans
 
	integer,intent(in):: ivtrans
	j_outputlist=0
	if(ivtrans.le.0.or.ivtrans.gt.j_namedv)then
		write(6,*)'*j* outputlist, argument not object index ',ivtrans
		j_err=.true.
		return
	endif !if(ivtrans.le.0.or.ivtrans.gt.j_namedv)   8120
	if(j_otype(ivtrans).ne.j_iptrans)then
		call j_printname('*Object ',ivtrans,' is not a transformation set')
		j_err=.true.
		return
	endif !if(j_otype(ivtrans).ne.j_iptrans)   8125
	j_outputlist=j_o(ivtrans)%i2(2)
 
end function j_outputlist !integer function j_outputlist(ivtrans)
 
 
character*24 function j_vname(iv) ! name of object iv
 
	integer, intent(in):: iv
	!j_vname=' '
	!call j_getline(j_ivnames,iv,j_vname,le)
	call j_getname(iv)
	j_vname=j_oname(1:j_loname)
	return
end function j_vname !character*24 function j_vname(iv)
 
 
function j_lename(iv)
	!module vmod
	!end module vmod
 
 
	integer, intent(in):: iv
 
	j_lename=j_o(j_ivnames)%i(iv+1)-j_o(j_ivnames)%i(iv)
	return
end function j_lename !function j_lename(iv)
 
 
function j_lenlist(iob) ! length of list, -1 if not a list
 
	!module vmod
	!end module vmod
 
	integer, intent(in):: iob
 
	if(j_otype(iob).ne.j_iplist)then
		j_lenlist=-1
	else !if(j_otype(iob).ne.j_iplist)then
		j_lenlist=j_o(iob)%i(1)
	endif !if(j_otype(iob).ne.j_iplist)   8165
	return
end function j_lenlist !function j_lenlist(iob)
 
 
function j_inlist(i,list0,list)  ! is i in list NO=>0
	integer, intent(in):: i
	integer,dimension(:), intent(in):: list
 
	!write(6,*)list(0)
	do j_inlist=1,list0
		if(i.eq.list(j_inlist))return
	enddo !j_inlist=1,list0   8179
	j_inlist=0
	return
end function j_inlist !function j_inlist(i,list0,list)
 
!20141211 crash
 
 
integer function j_nextio(iob,io)
	select case (j_o(iob)%i(io))
	case (1) !select case (j_o(iob)%i(io))
	j_nextio=io+j_o(iob)%i(io+2)+3
	case (2) !select case (j_o(iob)%i(io))
	j_nextio=io+3
	case (3:14) !select case (j_o(iob)%i(io))
	j_nextio=io+4
	case (15) !select case (j_o(iob)%i(io))
	j_nextio=io+3
	case (16:19) !select case (j_o(iob)%i(io))
	j_nextio=io+4
	case (20) !select case (j_o(iob)%i(io))
	j_nextio=io+3
	case ( 21) !select case (j_o(iob)%i(io))
	j_nextio=io+2*j_o(iob)%i(io+1)+2
	case (22) !select case (j_o(iob)%i(io))
	j_nextio=io+j_o(iob)%i(io+1)+2
	case (23:24) !select case (j_o(iob)%i(io))
	j_nextio=io+5
	case (25) !select case (j_o(iob)%i(io))
	io=io+3
	case (26) !select case (j_o(iob)%i(io))
	j_nextio=j_o(iob)%i(io+2)
	case (27:29) !select case (j_o(iob)%i(io))
	j_nextio=-1
	case (30) !select case (j_o(iob)%i(io))
	j_nextio=io+3
	case (31:33) !select case (j_o(iob)%i(io))
	j_nextio=io+4
	case (34:37) !select case (j_o(iob)%i(io))
	j_nextio=-1
	case (38:62) !select case (j_o(iob)%i(io))
	j_nextio=io+3
	case (78:79) !select case (j_o(iob)%i(io))
	j_nextio=-1
	case default !select case (j_o(iob)%i(io))
	j_nextio=io+j_o(iob)%i(io+1)+3
	end select !select case (j_o(iob)%i(io))
	return
end function !integer function j_nextio(iob,io)
 
 
 
 
function j_inlistobject(i,ivlist) !is i in a list object ivlist
	!20141211 err
 
	integer, intent(in):: i,ivlist
	!	write(6,*)'i,ivlist ',iv,ivlist
	if(ivlist.le.0.or.ivlist.gt.j_namedv)then
 
		write(6,*)'*j* illegal list j_inlistobject ',ivlist
		j_err = .true.;return
	else !if(ivlist.le.0.or.ivlist.gt.j_namedv)then
 
		if(j_otype(ivlist).ne.j_iplist.and.j_otype(ivlist).ne.j_ipilist)then
			call j_getname(i,ivlist)
			write(6,*)'*j* not a list ',j_oname2(1:j_loname2), ' trying ', j_oname(1:j_loname)
 
			j_err=.true.;return
		endif !if(j_otype(ivlist).ne.j_iplist.and.j_otype(ivlist).ne.j_ip   8245
	endif !if(ivlist.le.0.or.ivlist.gt.j_namedv)   8239
	j_inlistobject=0
	!	write(6,*)'j_o(ivlist)%i(1) ',i,j_o(ivlist)%i(1),j_o(ivlist)%i2, ' in ',j_inlistobject
	do j=1,j_o(ivlist)%i(1)
		if(i.eq.j_o(ivlist)%i2(j))then
			j_inlistobject=j
			return
		endif !if(i.eq.j_o(ivlist)%i2(j))   8255
	enddo !j=1,j_o(ivlist)%i(1)   8254
 
	return
end function j_inlistobject !function j_inlistobject(i,ivlist)
 
 
 
 
 
 
function j_inlist1(i,list0,list) ! is i in list, length given in list0 , not in list(0)
	integer, intent(in):: i, list0
	integer, intent(in):: list(list0)
 
	!write(6,*)list(0)
	do j_inlist1=1,list0
		if(i.eq.list(j_inlist1))return
	enddo !j_inlist1=1,list0   8274
	j_inlist1=0
	return
end function j_inlist1 !function j_inlist1(i,list0,list)
 
 
! function j_putlist0(i,list) ! put i into list, no bound checking %%list
! !put into list
! integer, intent(in):: i
! integer, intent(inout):: list(0:*)
 
! j_putlist0=j_inlist(i,list(0),list(1:list(0))
! if(j_putlist0.gt.0)return
! list(0)=list(0)+1
! list(list(0))=i
! j_putlist0=list(0)
! return
! end function j_putlist0 !function j_putlist0(i,list)
 
function j_putlist(i,list) ! put i into allocated list, bound checking %%list
	!put into list
	integer, intent(in):: i
	integer, dimension(:),allocatable, intent(inout):: list  !list(0:*)
	integer,dimension(:), allocatable::ivec2
	!	write(6,*)'<66i,list(0),list(1:10)',i,list(0),list(1:10)
	j_putlist=j_inlist(i,list(0),list(1:list(0)))
	if(j_putlist.gt.0)return
	iubound_=ubound(list,dim=1)
	if(list(0).ge.iubound_)then
		allocate(ivec2(0:2*iubound_))
		!		write(6,*)'*doubling a allocatable integer vector'
		ivec2(0:iubound_)=list
		call move_alloc( from=ivec2, to=list )
		! deallocate(list)
		! allocate(list(0:2*iubound_))
		! list(0:iubound_)=ivec2
		! deallocate(ivec2)
	end if !if(list(0).ge.iubound_)   8304
	list(0)=list(0)+1
	list(list(0))=i
	j_putlist=list(0)
	return
end function j_putlist !function j_putlist(i,list)
 
 
 
! function j_putlist2(i,ivlist) ! put i into %%list object, size increased if needed
! !module vmod
! !end module vmod
 
! !use j_globalfuncsmod	, only: printname
! !put into list
 
! integer, intent(in):: i,ivlist
 
! if(ivlist.le.0)then
! write(6,*)'*j* illegal putlist2';j_err=.true. ;return
! else if(j_otype(ivlist).ne.j_iplist)then !if(ivlist.le.0)then
! call j_printname('**not a legal list: ',ivlist,' ');j_err=.true. ;return
! end if !if(ivlist.le.0)then
! j_putlist2=j_inlist(i,j_o(ivlist)%i2)
! if(j_putlist2.gt.0)return
! le=size(j_o(ivlist)%i)-1
! write(6,*)'TEE putlist2'
! j_err=.true.;return
! if(j_o(ivlist)%i(1).ge.le)call j_inci(ivlist,j_o(ivlist)%i(1))
! j_o(ivlist)%i(1)=j_o(ivlist)%i(1)+1
! j_o(ivlist)%i2(j_o(ivlist)%i(1))=i
! j_putlist2=j_o(ivlist)%i(1)
! return
! end function j_putlist2 !function j_putlist2(i,ivlist)
 
! subroutine j_fromlist2(i,ivlist) ! put i into %%list object, size increased if needed
! !module vmod
! !end module vmod
 
! !use j_globalfuncsmod	, only: printname
! !put into list
 
! integer, intent(in):: i,ivlist
 
! if(ivlist.le.0)then
! write(6,*)'*j* illegal putlist2';j_err=.true. ;return
! else if(j_otype(ivlist).ne.j_iplist)then !if(ivlist.le.0)then
! call j_printname('**not a legal list: ',ivlist,' ');j_err=.true. ;return
! end if !if(ivlist.le.0)then
! iup=j_o(ivlist)%i(1)
! do k=1,iup
! if(i.eq.j_o(ivlist)%i2(k))then
! if(k.lt.iup)j_o(ivlist)%i2(k)=j_o(ivlist)%i2(iup)
! j_o(ivlist)%i(1)=iup-1
! return
 
! endif
! enddo
! call j_printname('*j* object ',i,' not in list ',ivlist)
! j_err=.true.
! return
! end subroutine j_fromlist2 !function j_putlist2(i,ivlist)
 
 
! function j_putlist2plus(i,ivlist) ! put i into list object and also after the list
 
 
! integer, intent(in):: i,ivlist
 
! if(ivlist.le.0)then
! write(6,*)'*j* illegal putlist2plus';j_err=.true. ;return
! else if(j_otype(ivlist).ne.j_iplist)then !if(ivlist.le.0)then
! call j_printname('**not a legal list: ',ivlist,' ');j_err=.true. ;return
! endif !if(ivlist.le.0)then
! j_putlist2plus=j_inlist(i,j_o(ivlist)%i)
! ! if(putlist2.gt.0)return
! le=size(j_o(ivlist)%i)-2  ! -1
! if(j_o(ivlist)%i(0).ge.le)call j_inci(ivlist,j_o(ivlist)%i(1))
! if(j_putlist2plus.le.0)then
! j_o(ivlist)%i(1)=j_o(ivlist)%i(1)+1
! j_o(ivlist)%i2(j_o(ivlist)%i(1))=i
! j_putlist2plus=j_o(ivlist)%i(1)
! endif !if(j_putlist2plus.le.0)then
! j_o(ivlist)%i(j_o(ivlist)%i(0)+1)=i  !put to end
! return
! end function j_putlist2plus !function j_putlist2plus(i,ivlist)
 
 
! function j_putlist2b(i,ivlist) ! put i into list object, if i is list expand it, and put all
 
! !module vmod
! !end module vmod
 
! !use j_globalfuncsmod	, only: printname
 
! integer, intent(in):: i,ivlist
 
! !put into list
! if(ivlist.le.0)then
! write(6,*)'*j* illegal putlist2';j_err=.true. ;return
! else if(j_otype(ivlist).ne.j_iplist)then !if(ivlist.le.0)then
! call j_printname('**not a legal list: ',ivlist,' ');j_err=.true. ;return
! endif !if(ivlist.le.0)then
! if(j_otype(i).eq.j_iplist)then
! do j=1,j_o(i)%i(1)
! ii=j_o(i)%i(j)
! j_putlist2b=j_inlist(ii,j_o(ivlist)%i2)
! if(j_putlist2b.gt.0)cycle
! le=size(j_o(ivlist)%i2)-1
! if(j_o(ivlist)%i(1).ge.le)call j_inci(ivlist,j_o(ivlist)%i(1))
! j_o(ivlist)%i(1)=j_o(ivlist)%i(1)+1
! j_o(ivlist)%i2(j_o(ivlist)%i(1))=ii
! enddo !do j=1,j_o(i)%i(0)
! j_putlist2b=j_o(ivlist)%i(1)
! else !if(j_otype(i).eq.j_iplist)then
! j_putlist2b=j_inlist(i,j_o(ivlist)%i2)
! if(j_putlist2b.gt.0)return
! le=size(j_o(ivlist)%i2)-1
! if(j_o(ivlist)%i(1).ge.le)call j_inci(ivlist,j_o(ivlist)%i(1))
! j_o(ivlist)%i(1)=j_o(ivlist)%i(1)+1
! j_o(ivlist)%i2(j_o(ivlist)%i(1))=i
! j_putlist2b=j_o(ivlist)%i(1)
! endif !if(j_otype(i).eq.j_iplist)then
! return
! end function j_putlist2b !function j_putlist2b(i,ivlist)
 
 
! !20141208 putlist3
! function j_putlist3(i,ivlist) !append i into list object
! !module vmod
! !end module vmod
 
! !use j_globalfuncsmod	, only: printname
 
! integer, intent(in):: i,ivlist
 
! !put into list
! if(ivlist.le.0)then
! write(6,*)'*j* illegal putlist2';j_err=.true. ;return
! else if(j_otype(ivlist).ne.j_iplist)then !if(ivlist.le.0)then
! call j_printname('**not a legal list: ',ivlist,' ');j_err=.true. ;return
! endif !if(ivlist.le.0)then
! !putlist2=inlist(i,o(ivlist)%i)
! !if(putlist2.gt.0)return
! le=size(j_o(ivlist)%i2)-1
! if(j_o(ivlist)%i(1).ge.le)call j_inci(ivlist,j_o(ivlist)%i(0))
! j_o(ivlist)%i(0)=j_o(ivlist)%i(0)+1
! j_o(ivlist)%i(j_o(ivlist)%i(0))=i
! j_putlist3=j_o(ivlist)%i(0)
! return
! end function j_putlist3 !function j_putlist3(i,ivlist)
 
 
subroutine j_putinput(iv,ivinl,ivoutl,ivarg) !put variable into inputlist if not in the outputlist
	!module vmod
	!end module vmod
 
 
	integer, intent(in):: iv,ivinl,ivoutl,ivarg
 
	if(ivinl.le.0.or.iv.gt.j_namedv)return
	if(ivarg.ne.0)then
		if(j_inlistobject(iv,ivarg).le.0)return
	endif !if(ivarg.ne.0)   8474
	if(ivoutl.gt.0)then
		if(j_inlistobject(iv,ivoutl).gt.0)return
	endif !if(ivoutl.gt.0)   8477
	!20140627   JL
	!	if(o(ivnames)%ch( o(ivnames)%i(iv) ).eq.'$')return
	if(j_o(j_ivnames)%ch( j_o(j_ivnames)%i(iv) ).eq.'$.or.iv.eq.ivresult')return
	ii=j_putlistobject(ivinl,single=iv)  !j_putlist2(iv,ivinl)
	return
end subroutine j_putinput !subroutine j_putinput(iv,ivinl,ivoutl)
 
 
subroutine j_putoutput(iv,ivinl,ivoutl,ivarg) ! put varaible in the outputlist, ignore $-varaibles
	integer, intent(in):: iv,ivinl,ivoutl,ivarg
 
	!getline(ivnames,iv,vname,le)
	if(ivoutl.le.0.or.iv.gt.j_namedv)return
	if(ivarg.ne.0)then
		if(j_inlistobject(iv,ivarg).le.0)return
	endif !if(ivarg.ne.0)   8493
	!20140627  JL
	!	if(o(ivnames)%ch( o(ivnames)%i(iv) ).ne.'$')ii=putlist2plus(iv,ivoutl)
	if(j_o(j_ivnames)%ch( j_o(j_ivnames)%i(iv) ).ne.'$'.and.iv.ne.j_ivresult) &
		ii=j_putlistobject(ivoutl,single=iv)   !ii=j_putlist2plus(iv,ivoutl)
	return
end subroutine j_putoutput !subroutine j_putoutput(iv,ivinl,ivoutl)
 
 
subroutine j_xt(ivmat,ivkeep,iobs)  ! get all keep-variables for observation iobs for data matrix ivmat
 
	integer, intent(in):: ivmat,ivkeep,iobs
 
	do i=1,j_o(ivkeep)%i(1)
		j_v(j_o(ivkeep)%i2(i))=j_o(ivmat)%d((iobs-1)*j_o(ivkeep)%i(1)+i)
	enddo !i=1,j_o(ivkeep)%i(1)   8508
	return
end subroutine j_xt !subroutine j_xt(ivmat,ivkeep,iobs)
 
 
 
 
 
 
! subroutine j_getdataobject(ivdata,nobs) ! initilization for a given data object
! integer, intent(in) ::ivdata
! integer,intent(out) ::nobs
! !module vmod
! !end module vmod
 
 
! !module typemod
! !ipobj  ! & first objec ????
! ! !typec
! !end module
 
 
! !parmod
! ! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
! !end module parmod
 
 
! !module datamod
! !end module
 
 
! ! integer, dimension(:),pointer::dataup,datado
! ! see defdata
! !write(6,*)'>%%',ivdata
! j_level=1
! ivcur=ivdata
! 1 if(j_o(ivcur)%i(5).ne.0)then
! ivcur=j_o(ivcur)%i(5);j_level=j_level+1
! if(j_level.gt.100)then
! write(6,*)'**too many data levels (up)';j_err=.true.;return
! end if !if(j_level.gt.100)then
! goto 1
! end if !if(j_o(ivcur)%i(5).ne.0)then
! j_levels=j_level
! ivcur2=ivdata
! !write(6,*)'levels',levels,ivdata
! 2 	if(j_o(ivcur2)%i(3).ne.0)then
! ivcur2=j_o(ivcur2)%i(3);j_levels=j_levels+1
! if(j_levels.gt.100)then
! write(6,*)'**too many data levels (do)';j_err=.true.;return
! end if !if(j_levels.gt.100)then
! goto 2
! end if !if(j_o(ivcur2)%i(3).ne.0)then
! !   write(6,*)'<levelsb',levels
! if(allocated(j_curdatasets))deallocate(j_curdatasets,j_iobcur,j_nobcur,j_iobcum)
! allocate(j_curdatasets(1:j_levels),j_iobcur(1:j_levels),j_nobcur(1:j_levels),j_iobcum(1:j_levels))
! j_iobcur=0
! j_iobcum=0
! j_nobcur=0
! j_curdatasets(1)=ivcur
! j_nobcur(1)=j_nobs_data(ivcur)
! ! if(j_o(ivcur)%i(4).gt.0)then
! ! write(6,*)'hep,ivcur',ivcur
! ! call dotrans(j_o(ivcur)%i(4),1)  prolog
! ! write(6,*)'hep2',j_err
! ! if(j_err)return
! ! end if !if(j_o(ivcur)%i(4).gt.0)then
! do i=2,j_levels
! j_curdatasets(i)=j_o(j_curdatasets(i-1))%i(3)
! ! nobcur(i)=nobs_data(curdatasets(i))1415
! ! if(j_o(j_curdatasets(i) )%i(4).gt.0)then
! ! !write(6,*)'hep3',i
! ! call dotrans(j_o(j_curdatasets(i))%i(4),1)  prolog
! ! if(j_err)return
! ! end if !if(j_o(j_curdatasets(i) )%i(4).gt.0)then
! ! nobcur(i)=v(
! end do !do i=2,j_levels
! !write(6,*)'curdtasets',(curdatasets(i),nobcur(i),i=1,levels)
! !ivobs=o(ivdata)%i(12)
! !   write(6,*)'<24031>ivdata',ivdata
! nobs=j_nobs_data(ivdata)
! !   write(6,*)'<24033>nobs',nobs
! !iobcur=
! return
! end subroutine !subroutine j_getdataobject(ivdata,nobs)
 
! subroutine j_alldatavars(vars,nvar) !all variables in data +trans variables
! ! if trans variables are already in data there will be space for them in vars
! ! but they are not stored twice
! integer,dimension(:),allocatable,intent(inout) :: vars
! integer,intent(out) ::nvar
! !write(6,*)'>5445',jdatasets(1)
 
! ivvars=j_datakeep(jdatasets(1)) !variables in the data set
! !write(6,*)'ivvars',ivvars
! narg0=j_o(ivvars)%i(1)
! !write(6,*)'<12poi',ivvars,narg0,j_transopt
 
! narg2=narg0
! if(j_transopt)then   !trans option is present, j_transopt set by j_getdataobject
! ivoutl2_=j_trans_output(j_ivtransopt) ! j_ivtransopt set by j_getdataobject
! ntransout=j_o(ivoutl2_)%i(1)
! narg2=narg2+ntransout
! endif !if(j_transopt)then
! if(allocated(vars))deallocate(vars)
! allocate(vars(1:narg2))
! vars(1:narg0)=j_o(ivvars)%i2(1:narg0)
! if(j_transopt)then
! nvar=narg0
! do j=1,ntransout
! if(j_inlistobject(j_o(ivoutl2_)%i(j),ivvars).le.0)then
! nvar=nvar+1
! vars(nvar)=j_o(ivoutl2_)%i2(j)
! endif !if(j_inlistobject(j_o(ivoutl2_)%i(j),ivvars).le.0)then
! if(nvar.lt.narg2)vars(nvar+1:narg2)=0
! enddo !do j=1,ntransout
! else !if(j_transopt)then
! nvar=narg0
! endif !if(j_transopt)then
 
 
! end subroutine !subroutine j_alldatavars(vars,nvar)
 
 
 
! subroutine j_nextobs() !get next observation for the current data set
 
! !module vmod
! !end module vmod
 
 
! !module datamod
! !end module
 
 
 
! !module typemod
! !ipobj  ! & first objec ????
! ! !typec
! !end module
 
! !level0=j_level
! !write(6,*)'level0',level0,j_iobcur(level0),j_nobcur(level0)
! 100 if(j_iobcur(level0).ge.j_nobcur(level0))then
! if(level0.gt.1)then
! j_iobcur(level0)=0
! level0=level0-1
! goto 100
! else !if(level0.gt.1)then
! ! now all done
! write(6,*)'*j* too many obs, '
! j_err=.true.
! return
! end if !if(level0.gt.1)then
! end if !if(j_iobcur(level0).ge.j_nobcur(level0))then
! !if(level0.le.0)write(6,*)'level0',level0,iobcur,nobcur
! !write(6,*)'level0,level',level0,level
! do lev=level0,j_level
! j_iobcur(lev)=j_iobcur(lev)+1
! j_iobcum(lev)=j_iobcum(lev)+1
! !write(17,*)lev,curdatasets(lev)
! !if(iobcur(lev).le.0.or.iobcum(lev).le.0)&
! ! write(6,*)'curcum',iobcur(lev),iobcum(lev)
! ! if(write(17,*)o(curdatasets(lev))%i(13)
! j_v( j_o(j_curdatasets(lev))%i(7) )=j_iobcur(lev)  !if uppermost level, then there is no obsw
! j_v( j_o(j_curdatasets(lev))%i(6) )=j_iobcum(lev)
! !write(6,*)'lev,iobcur(lev),iobcum(lev)',lev,iobcur(lev),iobcum(lev)
! ! write(6,*)'getting obs ',iobcum(lev), 'from level ',lev
! !	write(6,*)'cum',j_iobcum(lev)
! call j_getob(j_curdatasets(lev),j_iobcum(lev))
! if(j_err)return
! if(lev.lt.j_levels)j_nobcur(lev+1)=j_v( j_o(j_curdatasets(lev))%i(4) )
! if(lev.lt.j_levels)write(6,*)'cum',j_iobcum(lev+1)
! !if(lev.lt.levels)write(6,*)'lev,nobcur(lev+1)',lev,nobcur(lev+1)
! end do !do lev=level0,j_level
! if(j_transopt)then
! ! if(v(ivdebug).ne.0.)write(6,*)'h8'
! call dotrans(j_ivtransopt,1)
! if(j_err)return
! endif !if(j_transopt)then
! j_rejected=.false.
! if(j_filter)then
! !write(6,*)'hip',iofilter,ivfilter,reject,ioreject,ivreject
! if(j_codevalue(iob,j_filterlink).eq.j_0)j_rejected=.true.
! ! call dotrans(j_iobdata,j_iofilter)
! ! if(j_v(j_ivfilter).eq.0.)then
! ! j_rejected=.true.
! ! return
! ! end if !if(j_v(j_ivfilter).eq.0.)then
! end if !if(j_filter)then
! if(j_reject)then
! !if(v(ivdebug).ne.0.)write(6,*)'hk'
! if(j_codevalue(iob,j_rejectlink).ne.j_0)j_rejected=.true.
! ! call dotrans(j_iobdata,j_ioreject)
! ! !  if(v(ivdebug).ne.0.)write(6,*)'hjjj'
! ! if(j_v(j_ivreject).ne.0)then
! ! j_rejected=.true.
! ! return
! ! end if !if(j_v(j_ivreject).ne.0)then
! end if !if(j_reject)then
! if(j_transoptafter)then
! call dotrans(j_ivtransoptafter,1)
! endif !if(j_transoptafter)then
! ! write(6,*)'ennextob'
! return
! end subroutine !subroutine j_nextobs()
 
 
subroutine j_getobs0(ivdata,iobs)  !get observation iob in dta set ivdata (upper levels not used) %%data
	integer, intent(in):: ivdata !data object
	integer*8, intent(in) ::iobs
	integer*8::nkeep
	!usej_globalsmod, only:
	!o(ivout)%i(1)=ivmat;o(ivout)%i(2)=ivkeep;o(ivout)%i(3)=ivcases;o(ivout)%i(4)=ivprolog
	!o(ivout)%i(5)=ivmaketrans;o(ivout)%i(6)=ivtrans;
	!o(ivout)%i(7)=ivepilog;o(ivout)%i(8)=ivvars
	!o(ivout)%i(9)=ivsub;o(ivout)%i(10)=ivnobsw;o(ivout)%i(11)=ivup
	!o(ivout)%i(12)=ivobs;o(ivout)%i(13)=ivobsw
	ivkeep=j_o(ivdata)%i(2)
	nkeep=j_o( ivkeep )%i(1)
	ivmat=j_o(ivdata)%i(1)
	!if(j_o(ivmat)%i(4).gt.0)then
	ibas=(iobs-1)*nkeep
	!write(6,*)'iob,nkeep,ivkeep,ivmat',iob,nkeep,ivkeep,ivmat
	!write(6,*)j_o(ivmat)%d
	j_v(j_o(ivkeep)%i2(1:nkeep))=j_o(ivmat )%d(ibas+1:ibas+nkeep)
 
 
	!endif
	return
end subroutine !subroutine j_getobs0(ivdata,iobs)
 
 
! subroutine j_isindata(iv,ivdata,ikeep) ! is iv in data ivdata,
! !  ikeep, iout= positions in keep or output of trans
 
! !module vmod
! !end module vmod
 
! !use j_globalfuncsmod, only:printname
 
! integer, intent(in):: iv, ivdata
! !integer, intent(out):: ikeep,iout
 
! if(j_otype(ivdata).ne.j_ipdata)then
! call j_printname('*j* not a data object ',ivdata,' ')
! j_err=.true.;return
! endif !if(j_otype(ivdata).ne.j_ipdata)then
! !keep
! !	ivkeep=j_o(ivdata)%i(2)
! !	iout=j_inlistobject(iv,ivoul)
! ikeep=j_inlistobject(iv,j_o(ivdata)%i(2))
! return
! end subroutine j_isindata !subroutine j_isindata(iv,ivdata,ikeep,iout)
 
 
function j_ipc(iv)  !is iv character, YES=>1 NO=>0
 
	!module vmod
	!end module vmod
 
	integer, intent(in):: iv
 
	if(j_otype(iv).eq.j_ipchar)then
		j_ipc=1
	else !if(j_otype(iv).eq.j_ipchar)then
		j_ipc=0
	endif !if(j_otype(iv).eq.j_ipchar)   8773
	return
end function j_ipc !function j_ipc(iv)
 
 
subroutine j_andlist(list1,list2,olist)
	integer, intent(in):: list1(0:*),list2(0:*)
	integer, intent(out):: olist(0:*)
 
	iout=0
	do i=1,list1(0)
		if(any(list1(i).eq.list2(1:list2(0)) ))then
			iout=iout+1
			olist(iout)=list1(i)
		endif !if(any(list1(i).eq.list2(1:list2(0)) ))   8788
	enddo !i=1,list1(0)   8787
	olist(0)=iout
	return
end subroutine j_andlist !subroutine j_andlist(list1,list2,olist)
 
subroutine j_clear()
 
 
	!	write(6,*)'max named objects  ',j_mxnamedv
	!	17 j_nv=j_mxnamedv+j_mxtemporalv
 
	if(allocated(j_v))deallocate(j_v)   !allocate(j_v(1:j_mxv))  !
	if(allocated(j_o))deallocate(j_o)    !allocate(j_o(1:j_nv))
 
	if(allocated(j_otype))deallocate(j_otype)
	!allocate(j_otitle(1:j_mxnamedv))
	if(allocated(j_iob))deallocate(j_iob)   !possibly j_maxnamedv would be enough
	if(allocated(j_io))deallocate(j_io)   !possibly j_maxnamedv would be enough
	!	if(allocated(j_optarg2))deallocate(j_optarg2)
	!	allocate(j_locked(1:j_nv))   !possibly j_maxnamedv would be enough
	!
 
	!		call o1_init() clear subroutine needed
 
 
	!	call o2_init()
 
 
 
 
	if(allocated(j_utf8))deallocate(j_utf8)
	!   'ä','å','ö','Ä','Ö','Å'/)
	!	write(6,*)'>45/',j_utf8
	if(allocated(j_ocode))deallocate(j_ocode)
	!	close(6)
	j_inited=.false.
 
 
 
end subroutine j_clear
 
 
subroutine j_stopj()
	character*10 act
	integer,allocatable :: closed(:)
	if(j_nused.gt.0)allocate(closed(1:j_nused))
	!dimension a(1)
	!   USE DFWIN
	!logical status
	!status = FreeConsole()
	write(6,*)'stopj'
	nclosed=0
	do i_=j_nused,1,-1
		inquire(j_nunits(i_),NAME=j_tempchar2,ACTION=act)
		lenact=len_trim(act)
		!	write(6,*)'unit', j_nunits(i_),act
		!		write(6,*)'<112>',j_nunits(i_),j_tempcha r2(1:20),act
 
		if(act(1:lenact)=='READ') then
			write(6,*)'closing read file ', j_tempchar2(1:len_trim(j_tempchar2))
			!call j_closeunit(j_nunits(i_))
			nclosed=nclosed+1
			closed(nclosed)=j_nunits(i_)
		else !if(act(1:lenact)=='READ') then
			write(6,*)'write file ', j_tempchar2(1:len_trim(j_tempchar2)),' remains open'
		endif !if(act(1:lenact)=='READ')   8850
	enddo !i_=j_nused,1,-1   8844
	!		write(6,*)j_nused,'+',j_nunits(j_nused+1:6)
	do i_=1,nclosed
		call j_closeunit(closed(i_))
	enddo !i_=1,nclosed   8860
	! j_err=.false.
 
 
	!write(6,*)a(100000000000)
	j_stop=.true.
end subroutine !subroutine j_stopj()
 
 
subroutine j_closeunit(nu_)   ! %%io
	integer, intent(in)::nu_
	nu=nu_
	!	write(6,*)'<65>nu_,j_nused',nu_,j_nused ,j_nunits(1:j_nused)
	if(nu.lt.0)return   !text object
	close(nu,err=99)
	!write(6,*)'nu,nused',nu,nused ,nunits
	! write(6,*)'nunits',nunits
	!write(6,*)j_nunits(1:j_nused)
	!write(6,*)'<8647> closing ',nu
	do i=1,j_nused
		if(j_nunits(i).eq.nu)then
			nunitvar_=abs(j_unitchar(nu))
			if(nunitvar_.ne.0)then
				call j_putiounit(0,nunitvar_) !j_o(nunitvar_)%i(4)=0
				j_unitchar(nu)=0
			endif !if(nunitvar_.ne.0)   8884
			j_nunits(i)=j_nunits(j_nused)
			j_nunits(j_nused)=nu
			j_nused=j_nused-1
			return
		end if !if(j_nunits(i).eq.nu)   8882
	end do !i=1,j_nused   8881
99	write(6,*)'*j* closing illegal unit',nu
	write(6,*)'open units ',j_nunits(1:j_nused)
	j_err=.true.
	return
end subroutine j_closeunit !subroutine j_closeunit(nu_)
 
 
 
 
 
!20150812(arg1<->arg2) oli: subroutine defchar(name,iv,ivout) !define a character variable
function j_defchar(iv,name) !define a character constant
	character(len=*), intent(in):: name
	integer, intent(in):: iv
	!	integer, intent(out):: ivout
	logical::p=.false.
	!j_o(ivout)%i(1) first character of the constant in the text buffer containg
	!  all the names of objects
 
	if(p)write(6,*)'456',iv,name
	le=len_trim(name)
	if(iv.gt.0)then
		call j_getname(iv)
		if(le.le.0)then
 
			write(6,*)'*j* defchar name missing'
			j_err=.true.
			return
 
		endif !if(le.le.0)   8917
		if(name(1:1).eq."'")then
			j_defchar=j_object("'"//j_oname(1:j_loname)//name(2:le))
			if(j_defchar.gt.0)return
			call j_getobjectnam("'"//j_oname(1:j_loname)//name(2:le),j_ipchar,j_defchar)
		else
			j_defchar=j_object("'"//j_oname(1:j_loname)//name(1:le)//"'")
			if(j_defchar.gt.0)return
			call j_getobjectnam("'"//j_oname(1:j_loname)//name(1:le)//"'",j_ipchar,j_defchar)
		endif !if(name(1:1).eq."'")   8924
 
	else
		if(le.eq.0)then
 
			write(6,*)' defchar name empty'
			j_err=.true.
			return
		endif !if(le.eq.0)   8935
		if(name(1:1).eq."'")then
			j_defchar=j_object(name)
			if(j_defchar.gt.0)return
			call j_getobjectnam(name,j_ipchar,j_defchar)
		else
			j_defchar=j_object("'"//name//"'")
			if(j_defchar.gt.0)return
			call j_getobjectnam("'"//name//"'",j_ipchar,j_defchar)
		endif !if(name(1:1).eq."'")   8941
 
 
 
	endif !if(iv.gt.0)   8915
 
 
	! !	ivout=j_object3(iv,name,.true.)
	! if(p)write(6,*)'<748',ivout,name
 
	! if(ivout.gt.0)then
	! write(6,*)j_otype(ivout),j_otype(ivout).eq.j_ipchar
	! call j_getname(ivout)
	! write(6,*)j_oname(1:j_loname)
 
	! if(j_otype(ivout).eq.j_ipchar)return
	! endif !if(ivout.gt.0)then
	! call j_getobject(iv,name,j_ipchar,ivout)
	!20140522 virheenkäsittely
	if(j_err) return
	!	if(allocated(j_o(ivout)%i))return  !ivout was character constant
	allocate( j_o(j_defchar)%i(1:10))
	! do ii=j_o(j_ivnames)%i(ivout)-1,j_o(j_ivnames)%i(ivout+1)+1
	! write(6,*)ii,j_o(j_ivnames)%ch(ii)
	! enddo
	j_o(j_defchar)%i(1)=j_o(j_ivnames)%i(j_defchar)+1
	j_o(j_defchar)%i(2)=j_o(j_ivnames)%i(j_defchar+1)-2
	!if(p)write(6,*)'<6669',j_o(j_defchar)%i(1:2)
	j_o(j_defchar)%i(3:8)=0
	! for character
 
	!5 number of lines
	!6 lines used
	!7	last modified
	!8 lines allocated
 
	! write(6,*)'ivout,h',o(ivout)%i
	!call j_getchar(ivout,j_filename,lefi)
	!write(6,*)'>34>',j_filename(1:lefi)
	return
end function j_defchar !subroutine j_defchar(iv,name,ivout)
 
 
 
 
 
logical function j_ischarconst(iv)   ! %%char
	integer, intent(in)::iv
	j_ischarconst=.false.
	if(j_otype(iv).eq.j_ipchar)then
		if(j_o(iv)%i(3).eq.0)j_ischarconst=.true.
	endif !if(j_otype(iv).eq.j_ipchar)   8998
	return
 
end function !logical function j_ischarconst(iv)
 
function j_isconst(name)   !0 =is not, -1 looks like but is not
	double precision ::r
	!	logical,optional::silent
	!
	! tests if name refers to a constant
	! if name is numeric constant it is put to list constants if it is not there
	character*(*), intent(in):: name
 
	double precision,dimension(:),allocatable::vv
	!integer,dimension(:),pointer::otype2_=>null()
	j_isconst=-1
	if(.not.(name(1:1).eq.'.'.or.(name(1:1).ge.'0'.and.name(1:1).le.'9').or.name(1:1).eq.'-'.or. &
		name(1:1).eq.'+'))return
	read(name,*,err=99)r   !'(f12.0)',err=99)r
	!		write(6,*)'isconst r',r
	goto 100
99  return
100	continue
		! nv mxnamedv+mxtemporalv
	if(r.eq.j_1)then
		j_isconst=j_ivone
		!if(r.eq.1.d0)write(6,*)'<3663636uno',j_1,j_v(j_ivone)
		return
	elseif(r.eq.j_0)then !if(r.eq.j_1)then
		j_isconst=j_ivzero
		return
	endif !if(r.eq.j_1)   9024
	!write(6,*)'v(nv+1:nv+nconstantv)',v(nv+1:nv+nconstantv)
	do i=j_nv+1,j_nv+j_nconstantv
		if(j_v(i).eq.r)then
			j_isconst=i
			return
		endif !if(j_v(i).eq.r)   9034
	enddo !i=j_nv+1,j_nv+j_nconstantv   9033
 
	j_nconstantv=j_nconstantv+1
	j_isconst=j_nv+j_nconstantv
	call j_checkd(j_v,j_isconst)
	call j_checki(j_otype,j_isconst)
	j_v(j_isconst)=r
	! write(6,*)'const',nv+nconstantv,r
 
	j_otype(j_isconst)=j_ipreal
 
	return
end function j_isconst !function j_isconst(name)
 
function j_getconst(name)   !makes a constant from coed
	double precision ::r
	!	logical,optional::silent
	!
	! tests if name refers to a constant
	! if name is numeric constant it is put to list constants if it is not there
	character*(*), intent(in):: name
 
	double precision,dimension(:),allocatable::vv
	!integer,dimension(:),pointer::otype2_=>null()
	j_getconst=-1
	if(name(1:1).eq.'.'.or.(name(1:1).ge.'0'.and.name(1:1).le.'9').or.name(1:1).eq.'-'.or. &
		name(1:1).eq.'+')&
		!	 write(6,*)'reading',name,name(1:1).eq.'-'
		!	write(6,*)'isconst ',name
		read(name,*,err=99)r   !'(f12.0)',err=99)r
	!	write(6,*)'isconst r',r
	goto 100
99  r=j_val(name)
	if(j_err)return
100	continue
		! nv mxnamedv+mxtemporalv
	if(r.eq.j_1)then
		j_getconst=j_ivone
		!if(r.eq.1.d0)write(6,*)'<3663636uno',j_1,j_v(j_ivone)
		return
	elseif(r.eq.j_0)then !if(r.eq.j_1)then
		j_getconst=j_ivzero
		return
	endif !if(r.eq.j_1)   9074
	!write(6,*)'v(nv+1:nv+nconstantv)',v(nv+1:nv+nconstantv)
	do i=j_nv+1,j_nv+j_nconstantv
		if(j_v(i).eq.r)then
			j_getconst=i
			return
		endif !if(j_v(i).eq.r)   9084
	enddo !i=j_nv+1,j_nv+j_nconstantv   9083
 
 
 
 
	j_nconstantv=j_nconstantv+1
	j_getconst=j_nv+j_nconstantv
	call j_checkd(j_v,j_getconst)
	call j_checki(j_otype,j_getconst)
	j_v(j_getconst)=r
	! write(6,*)'const',nv+nconstantv,r
 
	j_otype(j_getconst)=j_ipreal
 
	return
end function j_getconst !function j_getconst(name)
 
function j_getkeep(ivdata)
	integer,intent(in)::ivdata
	if(j_otype(ivdata).ne.j_ipdata)then
		call j_getname(ivdata)
		write(6,*)j_oname(1:j_loname),' is not DATA'
		j_err=.true.; return
	endif !if(j_otype(ivdata).ne.j_ipdata)   9107
	j_getkeep=j_o(ivdata)%i(2)
	if(j_otype(j_getkeep).ne.j_iplist)then
		call j_getname(j_getkeep)
		write(6,*)j_oname(1:j_loname),' is not LIST'
		j_err=.true.; return
	endif !if(j_otype(j_getkeep).ne.j_iplist)   9113
 
end function
 
function j_getmatrix(ivdata)
	integer,intent(in)::ivdata
	if(j_otype(ivdata).ne.j_ipdata)then
		call j_getname(ivdata)
		write(6,*)j_oname(1:j_loname),' is not DATA'
		j_err=.true.; return
	endif !if(j_otype(ivdata).ne.j_ipdata)   9123
	!	write(6,*)'7575 ',ivdata
	j_getmatrix=j_o(ivdata)%i(1)
	!	write(6,*)'75799 ',j_getmatrix
	if(j_otype(j_getmatrix).ne.j_ipmatrix)then
		call j_getname(j_getmatrix)
		write(6,*)j_oname(1:j_loname),' is not MATRIX'
		j_err=.true.; return
	endif !if(j_otype(j_getmatrix).ne.j_ipmatrix)   9131
 
end function
 
function j_getcases(ivdata)
	integer,intent(in)::ivdata
	if(j_otype(ivdata).ne.j_ipdata)then
		call j_getname(ivdata)
		write(6,*)j_oname(1:j_loname),' is not DATA'
		j_err=.true.; return
	endif !if(j_otype(ivdata).ne.j_ipdata)   9141
	j_getcases=j_o(ivdata)%i(10)
	if(j_getcases.gt.0)then
		if(j_otype(j_getcases).ne.j_iplist)then
			call j_getname(j_getcases)
			write(6,*)j_oname(1:j_loname),' is not LIST'
			j_err=.true.; return
		endif !if(j_otype(j_getcases).ne.j_iplist)   9148
	endif !if(j_getcases.gt.0)   9147
end function
 
 
function j_getinputvar(ivtrans)
	integer,intent(in)::ivtrans
 
	if(j_otype(ivtrans).ne.j_iptrans)then
		call j_getname(ivtrans)
		write(6,*)j_oname(1:j_loname),' is not TRANS'
		j_err=.true.; return
	endif !if(j_otype(ivtrans).ne.j_iptrans)   9160
	j_getinputvar=j_o(ivtrans)%i(1)
	if(j_otype(j_getinputvar).ne.j_iplist)then
		call j_getname(j_getinputvar)
		write(6,*)j_oname(1:j_loname),' is not LIST'
		j_err=.true.; return
	endif !if(j_otype(j_getinputvar).ne.j_iplist)   9166
 
end function
 
function j_getoutputvar(ivtrans)
	integer,intent(in)::ivtrans
	if(j_otype(ivtrans).ne.j_iptrans)then
		call j_getname(ivtrans)
		write(6,*)j_oname(1:j_loname),' is not TRANS'
		j_err=.true.; return
	endif !if(j_otype(ivtrans).ne.j_iptrans)   9176
	j_getoutputvar=j_o(ivtrans)%i(2)
	if(j_otype(j_getoutputvar).ne.j_iplist)then
		call j_getname(j_getoutputvar)
		write(6,*)j_oname(1:j_loname),' is not LIST'
		j_err=.true.; return
	endif !if(j_otype(j_getoutputvar).ne.j_iplist)   9182
end function
 
function j_gettablerow(ivtable)
	integer,intent(in)::ivtable
	if(j_otype(ivtable).ne.j_iptable)then
		call j_getname(ivtable)
		write(6,*)j_oname(1:j_loname),' is not TABLE'
		j_err=.true.; return
	endif !if(j_otype(ivtable).ne.j_iptable)   9191
	j_gettablerow=j_o(ivtable)%i(4)
	if(j_otype(j_gettablerow).ne.j_iplist)then
		call j_getname(j_gettablerow)
		write(6,*)j_oname(1:j_loname),' is not LIST'
		j_err=.true.
	endif !if(j_otype(j_gettablerow).ne.j_iplist)   9197
	return
end function
 
function j_gettablecol(ivtable)
	integer,intent(in)::ivtable
	if(j_otype(ivtable).ne.j_iptable)then
		call j_getname(ivtable)
		write(6,*)j_oname(1:j_loname),' is not TABLE'
		j_err=.true.; return
	endif !if(j_otype(ivtable).ne.j_iptable)   9207
	j_gettablecol=j_o(ivtable)%i(5)
	if(j_otype(j_gettablecol).ne.j_iplist)then
		call j_getname(j_gettablecol)
		write(6,*)j_oname(1:j_loname),' is not LIST'
		j_err=.true.
	endif !if(j_otype(j_gettablecol).ne.j_iplist)   9213
	return
end function
 
 
 
 
function j_num2iv(r) !puts const into list of constants, if not there, and output is the indec in j_v
	double precision,intent(in) ::r
	!
	! tests if name refers to a constant
	! if name is numeric constant it is put to list constants if it is not there
 
 
	double precision,dimension(:),allocatable::vv
	integer,dimension(:),allocatable::otyp
	!integer,dimension(:),pointer::otype2_=>null()
 
	if(r.eq.j_1)then
		j_num2iv=j_ivone
		!if(r.eq.1.d0)write(6,*)'<3663636uno',j_1,j_v(j_ivone)
		return
	elseif(r.eq.j_0)then !if(r.eq.j_1)then
		j_num2iv=j_ivzero
		return
	endif !if(r.eq.j_1)   9235
	!write(6,*)'v(nv+1:nv+nconstantv)',v(nv+1:nv+nconstantv)
	do i=j_nv+1,j_nv+j_nconstantv
		if(j_v(i).eq.r)then
			j_num2iv=i
			return
		endif !if(j_v(i).eq.r)   9245
	enddo !i=j_nv+1,j_nv+j_nconstantv   9244
	if(j_nconstantv.ge.j_mxconstantv)then
		!	allocate(vv(1:nv+2*mxconstantv) )
		allocate(vv(1:j_nv+2*j_mxconstantv))
		vv(1:j_mxconstantv)=j_v
		call move_alloc(from=vv,to=j_v)
		allocate(otyp(1:j_nv+2*j_mxconstantv) )
		otyp(1:j_mxconstantv)=j_otype
		call move_alloc(from=otyp,to=j_otype)
		! deallocate(j_v)
		! allocate(j_v(1:j_nv+2*j_mxconstantv) )
		! j_v(1:j_nv+j_mxconstantv)=vv
 
		! vv=j_otype
		! deallocate(j_otype)
		! allocate(j_otype(1:j_nv+2*j_mxconstantv) )
		! j_otype(1:j_nv+j_mxconstantv)=vv
		! vv=j_otype2
		! deallocate(j_otype2)
		! allocate(j_otype2(1:j_nv+2*j_mxconstantv) )
		! j_otype2(1:j_nv+j_mxconstantv)=vv
		! deallocate(vv)
		j_mxconstantv=2*j_mxconstantv
		!	write(6,*)'*doubling the number of constants into ',j_mxconstantv
	endif !if(j_nconstantv.ge.j_mxconstantv)   9250
	j_nconstantv=j_nconstantv+1
	j_v(j_nv+j_nconstantv)=r
	! write(6,*)'const',nv+nconstantv,r
	j_num2iv=j_nv+j_nconstantv
	!	write(6,*)'<5isconst>',j_isconst
 
	return
 
end function j_num2iv
 
 
subroutine j_printuntil(nu,inp,le,tag) !print file until tag, but not it
 
	integer, intent(in):: nu
	character*(*), intent(in):: tag
	character*(*), intent(out):: inp
	integer, intent(out):: le
 
	let=len_trim(tag)
1 read(nu,'(a)',end=90,err=99)inp
	le=j_lentrim(inp)
 
	if(inp(1:max(1,le)).ne.tag(1:let))then
		write(6,*)inp(1:max(le,1))
		goto 1
	endif !if(inp(1:max(1,le)).ne.tag(1:let))   9296
	return
99 write(6,*)'*printuntil: error reading'
	j_err=.true.
90 return
end subroutine j_printuntil !subroutine j_printuntil(nu,inp,le,tag)
 
 
subroutine j_readuntil(nu,inp,le,tag)
 
	integer, intent(in):: nu
	character*(*), intent(in):: tag
	character*(*), intent(out):: inp
	integer, intent(out):: le
 
	let=len_trim(tag)
 
1 read(nu,'(a)',end=99,err=99)inp
	if(inp(1:let).ne.tag(1:let))goto 1
	le=j_lentrim(inp)
	write(6,*)inp(1:le)
	return
99 write(6,*)'* cannot find ',tag(1:let)
	j_err=.true.
	call j_closeunit(nu)
	return
end subroutine j_readuntil !subroutine j_readuntil(nu,inp,le,tag)
 
 
!melacharline ei ole j_modules/interface määrittelyissä
subroutine melacharline(inp,le)
	character*(*) inp
	inp='puppua'
	le=6
	return
end subroutine melacharline !subroutine melacharline(inp,le)
 
 
!20141217 isanyin
!  Testing if any element of text-vector text is in text-vector vector
subroutine j_isanyin(vector1,nvector1, vector2,nvector2, cvector1, cvector2)
 
	character*(*), intent(in):: vector1(nvector1), vector2(nvector2), cvector1, cvector2
	integer, intent(in):: nvector1, nvector2
 
	do it_ = 1, nvector1
		le1=len_trim(vector1(it_))
		do i=1,nvector2
			le2=len_trim(vector2(i))
			if(le1.eq.le2) then
				!20141217 oli: if(text.eq.vector(i)(1:le2))then
				if(vector1(it_)(1:le1).eq.vector2(i)(1:le2))then
					write(6,*)'name ',vector1(it_)(1:le1),' in ', cvector1 ,' is already in ',cvector2
					j_err = .true.
				endif !if(vector1(it_)(1:le1).eq.vector2(i)(1:le2))   9350
			endif !if(le1.eq.le2)   9348
		enddo !i=1,nvector2   9346
	enddo !it_ = 1, nvector1   9344
	return
end subroutine j_isanyin !subroutine j_isanyin(vector1,nvector1, vector2,nvector2, cvector1, cvector2)
 
 
function j_iopts(opt)  ! returns the index of option, but this is in j_utilities.f90 becasue
 
	character*(*), intent(in):: opt
 
	le=len_trim(opt)
	j_iopts=j_isin(opt(1:le),j_options,j_noptions)
	!###TESTAUS###
	!write(6,*)'jcompil <3557> i=isin(...): input(icur:ipos1)', input(icur:ipos1)
	!20141219 if(i==0)
	if(j_iopts==0) then
		io_ = j_isin(opt(1:le),o1_options,o1_noptions)
		if(io_ >0) then
			j_iopts = j_noptions+ io_ !1000+io_
		else !if(io_ >0) then
			io_ = j_isin(opt(1:le),o2_options,o2_noptions)
			if(io_ > 0) then
				j_iopts = j_nopts1+io_ !2000+io_
			else !if(io_ > 0) then
				io_ = j_isin(opt(1:le),o3_options,o3_noptions)
				if(io_ > 0) then
					j_iopts = j_nopts2+ io_ !  3000+io_
				endif !if(io_ > 0)   9380
			endif !if(io_ > 0)   9376
		endif !if(io_ >0)   9372
	endif !if(j_iopts==0)   9370
	return
end function !function j_iopts(opt)
 
 
subroutine j_getvalues(ix,value9,nval,iz,value2,nval2)
	integer*8::ial,i
	!module datamod
	!end module
 
 
	!module vmod
	!end module vmod
 
	integer, intent(in):: ix, iz
	real, dimension(*), intent(out):: value9,value2
	integer*8, intent(out):: nval, nval2
 
	!	do k=1,jndatasetss
	!call j_getdataobject(jdatasets(k),nobs)
	!write(6,*)'levels,level,nobs',levels,level,nobs
	!write(6,*)'h',o(datasets(k))%i
 
	ial=1
	!if(k.eq.1)then
 
700   call j_getobs(ial)
	if(j_rejected) then
		ial=ial+1
		if(ial.eq.j_dnobs8)then
			write(6,*)'**all observations rejected '
			j_err=.true.
			return
		endif !if(ial.eq.j_dnobs8)   9414
		goto 700
	endif !if(j_rejected)   9412
 
	value9(1)=j_v(ix);nval=1;ial=ial+1
	if(iz.gt.0)then
		value2(1)=j_v(iz);nval2=1
	endif !if(iz.gt.0)   9423
	!write(6,*)'k,ial,nobs',k,ial,nobs
	do i=ial,nobs
		call j_getobs(i)
		if(j_rejected)cycle
		!do j=1,nval
		!if(value9(j).eq.v(ix))then
		!freq(j)=freq(j)+1
		!goto 800
		!endif
		!enddo
		if(any(value9(1:nval).eq.j_v(ix)))goto 800
		nval=nval+1;value9(nval)=j_v(ix)
800   continue
		if(iz.gt.0)then
			if(any(value2(1:nval2).eq.j_v(iz)))goto 900
			nval2=nval2+1;value2(nval2)=j_v(iz)
900     continue
		endif !if(iz.gt.0)   9439
	enddo !i=ial,nobs   9427
	!	enddo !do k=1,jndatasetss
	return
end subroutine j_getvalues !subroutine j_getvalues(ix,value9,nval,iz,value2,nval2)
 
 
subroutine j_bitset(ifunc,irow,icol,val)
	!module vmod
	!end module vmod
 
	integer, intent(in):: ifunc,irow,icol
	real, intent(in):: val
 
	j=(irow-1)*j_o(ifunc)%i(4)+icol-j_o(ifunc)%i(5)
	ii=(j-1)/32+1
	ibit=j-(ii-1)*32-1  ! bit numbering starts from zero
	!  rw=o(ifunc)%r(ii)
	if(val.ne.0.)then
		j_o(ifunc)%i2(ii)= ibset(j_o(ifunc)%i2(ii),ibit)
	else !if(val.ne.0.)then
		j_o(ifunc)%i2(ii)= ibclr(j_o(ifunc)%i2(ii),ibit)
	endif !if(val.ne.0.)   9461
	return
end subroutine j_bitset !subroutine j_bitset(ifunc,irow,icol,val)
 
 
subroutine j_putmatrix_s(ivmat,irow,icol,val)
	!module vmod
	!end module vmod
 
	integer, intent(in):: ivmat
	integer*4,intent(in)::irow,icol
	real, intent(in):: val
	!write(6,*)'<66>,irow,icol,val,ivmat,j_otype(ivmat),j_iplist',&
	!	irow,icol,val,ivmat,j_otype(ivmat),j_iplist
	if(j_otype(ivmat).eq.j_iplist)then
		j_v(j_o(ivmat)%i(irow))=val
		return
 
	endif !if(j_otype(ivmat).eq.j_iplist)   9479
	if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.1.and.icol.eq.1)then
		j_v(ivmat)=val
	else !if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.1.and.icol.eq.1)then
		j_o(ivmat)%d((irow-1)*j_o(ivmat)%i(2)+icol)=val
		! else !if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.1.and.icol.eq.1)then
 
		! read(-j_o(ivmat)%i(4),rec=irow,err=90)j_o(ivmat)%r(1:j_o(ivmat)%i(2))
		! j_o(ivmat)%r(icol)=val
		! write(-j_o(ivmat)%i(4),rec=irow,err=90)j_o(ivmat)%r(1:j_o(ivmat)%i(2))
		! j_o(ivmat)%i(6)=irow
 
	endif !if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.1.and.icol.eq.1)   9484
	return
90 write(6,*)'*j* read/write error in j_putmatrix'
	j_err=.true.
 
	return
end subroutine j_putmatrix_s !subroutine j_putmatrix_s(ivmat,irow,icol,val)
 
 
 
subroutine j_func(iob,io,func,minv,maxv,coef,coef2,reci)
	integer, intent(in):: iob,io
	double precision ::func
	double precision,optional,intent(in)::minv,maxv,coef,coef2,reci
	double precision,dimension(1:2)::per,per2
	double precision:: val
	integer ::loc
	!	external ::func
	irg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2)
	per(1)=2
	per(2)=5
	per2=dsin(per)
 
	!	write(6,*)'<33>',per2
	per2=func(per)
	!	write(6,*)'<33>',per2
 
 
 
	if(j_otype(irg).eq.j_ipreal)then
		val=j_v(irg)
		if(present(minv))then
			if(j_v(irg).lt.minv)then
				write(6,*)'*too small argument ',j_v(irg)
				j_err=.true.
				return
			endif !if(j_v(irg).lt.minv)   9528
		endif !if(present(minv))   9527
		if(present(maxv))then
			if(val.gt.maxv)then
				write(6,*)'*too larg argument ',j_v(irg)
				j_err=.true.
				return
			endif !if(val.gt.maxv)   9535
		endif !if(present(maxv))   9534
		if(present(coef))val=val*coef
		if(present(reci))val=1.d0/val
 
		j_v(iout)=func(val)
 
		if(present(coef2))j_v(iout)=coef2*j_v(iout)
 
 
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		if(present(minv))then
			val=minval(j_o(irg)%d)
			if(val.lt.minv)then
				! loc=minloc(j_o(irg)%d)
				! irow=loc/j_o(irg)%i(2)
				! icol=loc-(irow-1)*j_o(irg)%i(2)
				! write(6,*)'*row ',irow,' column ',icol,' too small value ',&
				! j_o(irg)%d(loc)
				write(6,*)'*too small element ',val
				j_err=.true.
				return
			endif !if(val.lt.minv)   9552
		endif !if(present(minv))   9550
		if(present(maxv))then
			val=maxval(j_o(irg)%d)
			if(val.gt.maxv)then
				! loc=maxloc(j_o(irg)%d)
				! irow=loc/j_o(irg)%i(2)
				! icol=loc-(irow-1)*j_o(irg)%i(2)
				! write(6,*)'*row ',irow,' column ',icol,' too large value ',val
				j_err=.true.
				return
			endif !if(val.gt.maxv)   9565
		endif !if(present(maxv))   9563
 
 
		!as in j_defmatrix
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		!		write(6,*)'<34>',irg,iout
		!		write(6,*)'hep',func(3.d0)
		!		write(6,*)'hep2',func(3.)
		if(present(reci))then
			if(present(coef))then
				j_o(iout)%d=func(coef/j_o(irg)%d)
			else !if(present(coef))then
				j_o(iout)%d=func(1.d0/j_o(irg)%d)
			endif !if(present(coef))   9586
		else !if(present(reci))then
			if(present(coef))then
				j_o(iout)%d=func(coef*j_o(irg)%d)
			else !if(present(coef))then
				j_o(iout)%d=dsin(j_o(irg)%d(1:j_o(irg)%i(3)))
 
			endif !if(present(coef))   9592
 
		endif !if(present(reci))   9585
		if(present(coef2))j_o(iout)%d=coef2*j_o(iout)%d
		!		write(6,*)'irg',irg,iout,j_o(iout)%i,j_o(iout)%d
	else !if(j_otype(irg).eq.j_ipreal)then
		call j_printname('argument ',irg, ' is not scalar or matrix ')
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   9525
end subroutine !subroutine j_func(iob,io,func,minv,maxv,coef,coef2,reci)
 
subroutine j_funci(iob,io,ifunc)   !integer function
	integer,intent(in)::iob,io
	integer ::ifunc
	!	external ifunc
 
	integer ::iresult
	real, allocatable,dimension(:)::mat
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+3) !narg is present but it should be 1
	if(j_otype(irg).eq.j_ipreal)then
		iresult=ifunc(real(j_v(irg)))
 
		j_v(iout)=iresult
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		!	allocate(mat(1:j_o(irg)%i(3)))
		!	iresultmat=ifunc(j_o(irg)%d)
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		!	write(6,*)'<33>',real(j_o(irg)%d)
		j_o(iout)%d=ifunc(real(j_o(irg)%d))
		!	deallocate(iresultmat)
 
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'*argument is not real or matrix'
		j_err=.true.
 
	endif !if(j_otype(irg).eq.j_ipreal)   9617
	return
 
 
end subroutine !subroutine j_funci(iob,io,ifunc)
 
subroutine j_func2(iob,io,func)   !integer function
	integer,intent(in)::iob,io
	double precision ::func
	!	external func
 
	!integer ::iresult
	!	real, allocatable,dimension(:)::mat
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+3) !narg is present but it should be 1
	if(j_otype(irg).eq.j_ipreal)then
		!	iresult=ifunc(real(j_v(irg)))
		write(6,*)'<2>',func(real(j_v(irg)))
		j_v(iout)=func(j_v(irg))  !iresult
		!	write(6,*)'<22',dnint(j_v(irg)),anint(j_v(irg)),anint(real(j_v(irg)))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		!	allocate(mat(1:j_o(irg)%i(3)))
		!	iresultmat=ifunc(j_o(irg)%d)
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		write(6,*)'<33>',real(j_o(irg)%d)
		!	j_o(iout)%d=func(real(j_o(irg)%d))
		j_o(iout)%d=func(j_o(irg)%d)
		!	deallocate(iresultmat)
 
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'*argument is not real or matrix'
		j_err=.true.
 
	endif !if(j_otype(irg).eq.j_ipreal)   9652
	return
 
 
end subroutine !subroutine j_func2(iob,io,func)
 
! subroutine j_func2(iob,io,func) !as j_func but with positivie argument
! integer, intent(in):: iob,io
! double precision ::func
! !	external ::func
! irg=j_o(iob)%i(io+1)
! iout=j_o(iob)%i(io+2)
! if(j_otype(irg).eq.j_ipreal)then
! if(j_v(irg).ge.0.d0)then
! j_v(iout)=func(j_v(irg))
! else
! write(6,*)'*negative argument ,j_v(irg)
! j_err=.true.
! endif
! elseif(j_otype(irg).eq.j_ipmatrix)then
! if(minval(j_o(irg)%d).lt.0.d0)then
! loc=minloc(j_o(irg)%d)
! irow=loc/j_o(irg)%i(2)
! icol=loc-(irow-1)*j_o(irg)%i(2)
! write(6,*)'*row ',irow,' column ',icol,' negative value ',&
! j_o(irg)%d(loc)
! j_err=.true.
! else
! !		write(6,*)'irg',irg,iout,j_
! !as in j_defmatrix
! call j_del(iout)
! j_otype(iout)=j_ipmatrix
! allocate( j_o(iout)%i(1:13))
! j_o(iout)%i=j_o(irg)%i
! allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
! !		write(6,*)'<34>',irg,iout
! !		write(6,*)'hep',func(3.d0)
! !		write(6,*)'hep2',func(3.)
! j_o(iout)%d=func(j_o(irg)%d)
! endif
! !		write(6,*)'irg',irg,iout,j_o(iout)%i,j_o(iout)%d
! else
! call j_printname('argument ',irg, ' is not scalar or matrix ')
! j_err=.true.
! endif
! end subroutine
 
! subroutine j_exp(iob,io) !as j_func but with positivie argument
! integer, intent(in):: iob,io
! double precision ::func
! !	external ::func
! irg=j_o(iob)%i(io+1)
! iout=j_o(iob)%i(io+2)
! if(j_otype(irg).eq.j_ipreal)then
! if(j_v(irg).ge.0.d0)then
! j_v(iout)=func(j_v(irg))
! else
! write(6,*)'*negative argument ,j_v(irg)
! j_err=.true.
! endif
! elseif(j_otype(irg).eq.j_ipmatrix)then
! if(minval(j_o(irg)%d).lt.0.d0)then
! loc=minloc(j_o(irg)%d)
! irow=loc/j_o(irg)%i(2)
! icol=loc-(irow-1)*j_o(irg)%i(2)
! write(6,*)'*row ',irow,' column ',icol,' negative value ',&
! j_o(irg)%d(loc)
! j_err=.true.
! else
! !		write(6,*)'irg',irg,iout,j_
! !as in j_defmatrix
! call j_del(iout)
! j_otype(iout)=j_ipmatrix
! allocate( j_o(iout)%i(1:13))
! j_o(iout)%i=j_o(irg)%i
! allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
! !		write(6,*)'<34>',irg,iout
! !		write(6,*)'hep',func(3.d0)
! !		write(6,*)'hep2',func(3.)
! j_o(iout)%d=func(j_o(irg)%d)
! endif
! !		write(6,*)'irg',irg,iout,j_o(iout)%i,j_o(iout)%d
! else
! call j_printname('argument ',irg, ' is not scalar or matrix ')
! j_err=.true.
! endif
! end subroutine
 
subroutine j_putmatrix(ivmat,irow,icol,val)
	!module vmod
	!end module vmod
	integer,intent(in)::ivmat
	integer*4,intent(in):: irow,icol
	double precision, intent(in):: val
 
	if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.1.and.icol.eq.1)then
		j_v(ivmat)=val
	elseif(j_o(ivmat)%i(4).ge.0)then !if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.1.and.icol.eq.1)then
		j_o(ivmat)%d((irow-1)*j_o(ivmat)%i(2)+icol)=val
		! else double precision matrices are not stored on disk
 
		! read(-j_o(ivmat)%i(4),rec=irow,err=90)j_o(ivmat)%r(1:j_o(ivmat)%i(2))
		! j_o(ivmat)%r(icol)=val
		! write(-j_o(ivmat)%i(4),rec=irow,err=90)j_o(ivmat)%r(1:j_o(ivmat)%i(2))
		! j_o(ivmat)%i(6)=irow
 
	endif !if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.1.and.icol.eq.1)   9769
	return
	! 90 write(6,*)'*j* read/write error in j_putmatrix'
	! j_err=.true.
 
	return
end subroutine j_putmatrix!subroutine j_putmatrix_d(ivmat,irow,icol,val)
 
subroutine j_putmatrix8(ivmat,irow,icol,val)
	!module vmod
	!end module vmod
	integer,intent(in)::ivmat
	integer*8,intent(in):: irow,icol
	double precision, intent(in):: val
 
	if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.j_18.and.icol.eq.j_18)then
		j_v(ivmat)=val
		!i(4) is type
	elseif(j_o(ivmat)%i(4).ge.0)then !if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.1.and.icol.eq.1)then
 
		!	j_o(ivmat)%d((irow-1)*j_o(ivmat)%i(2)+icol)=val
 
		j_o(ivmat)%d((irow-1)*j_ncols(ivmat)+icol)=val
		! else double precision matrices are not stored on disk
 
		! read(-j_o(ivmat)%i(4),rec=irow,err=90)j_o(ivmat)%r(1:j_o(ivmat)%i(2))
		! j_o(ivmat)%r(icol)=val
		! write(-j_o(ivmat)%i(4),rec=irow,err=90)j_o(ivmat)%r(1:j_o(ivmat)%i(2))
		! j_o(ivmat)%i(6)=irow
 
	endif !if(j_otype(ivmat).eq.j_ipreal.and.irow.eq.j_18.and.icol.eq   9795
	return
	! 90 write(6,*)'*j* read/write error in j_putmatrix'
	! j_err=.true.
 
	return
end subroutine j_putmatrix8!subroutine j_putmatrix_d(ivmat,irow,icol,val)
 
 
double precision function j_getmatel(ivmat,irow,icol) !function
	!module vmod
	!end module vmod
 
	integer, intent(in):: ivmat,irow,icol
 
 
	!	if(j_o(ivmat)%i(4).ge.0)then
	if(j_otype(ivmat).eq.j_ipmatrix)then !j_o(ivmat)%i(13).ne.0)then
		if(irow.le.0.or.irow.gt.j_o(ivmat)%i(1).or.icol.le.0.or.icol.gt.j_o(ivmat)%i(2))then
			call j_getname(ivmat)
			write(6,*)'*j* trying to get elem ',irow,icol,'from ',j_o(ivmat)%i(1:2),' matrix ',j_oname(1:j_loname)
			j_err=.true.
			return
		endif !if(irow.le.0.or.irow.gt.j_o(ivmat)%i(1).or.icol.le.0.or.ic   9828
		j_getmatel=j_o(ivmat)%d((irow-1)*j_o(ivmat)%i(2)+icol)
 
	else !if(j_otype(ivmat).eq.j_ipmatrix)then
		call j_printname('*j* not a MATRIX',ivamt,' ')
	endif !if(j_otype(ivmat).eq.j_ipmatrix)   9827
 
 
	! else
	! if(j_o(ivmat)%i(6).eq.irow)then
	! j_getmatel=j_o(ivmat)%r(icol)
 
	! else
	! read(-j_o(ivmat)%i(4),rec=irow,err=90)j_o(ivmat)%r(1:j_o(ivmat)%i(2))
	! j_getmatel=j_o(ivmat)%r(icol)
	! j_o(ivmat)%i(6)=irow
	! endif
 
	! endif
	return
	! 90	write(6,*)'*j* error in reading disk matrix'
	! j_err=.true.
	! return
end function j_getmatel !double precision function j_getmatel(ivmat,irow,icol)
 
 
subroutine j_asschar2(ivin,ivout) !! assign character constant to charvariable ?
 
	!	write(6,*)'<5423535',ivout,j_otype(ivout)
	if(j_otype(ivout).ne.j_ipchar)then
		call j_del(ivout)
		!link to first char, linkt to last char, link to charconst, unit in files
		allocate( j_o(ivout)%i(1:8)) ! 4))
	endif !if(j_otype(ivout).ne.j_ipchar)   9862
	j_o(ivout)%i(1:2)=j_o(ivin)%i(1:2)
	j_o(ivout)%i(3)=ivin   !  ; o(ivin)%i(3)=ivout character constant
	j_o(ivout)%i(4:8)=0
	j_otype(ivout)=j_ipchar
	return
end subroutine j_asschar2 !subroutine j_asschar2(ivin,ivout)
 
subroutine j_sit() !sit -prompt
	integer ::recursion=0
	!	integer,dimension(3)::ivcursor=(/j_ivcursor,0,0/)
	!	save recursion,ivcursor
 
	!	integer, dimension (:), pointer :: optarg_
 
	!		recursion=recursion+1
	! if(recursion.gt.3)then
	! write(6,*)'* sit() cannot be called at recursion level ',recursion
	! j_err=.true.
	! goto 900
	! endif !if(recursion.gt.3)then
	!if(ivcursor(recursion).eq.0)
	!call j_deftrans(0,'$Cursor$'//char(48+recursion), &
	!		ivcursor(recursion),200,0,0,iii,iii,iii,0)
	!			call j_deftrans(0,'$Cursor$', &
	!		ivcursor,200,0,0,iii,iii,iii,0)
 
 
	!		call j_startfunction(iob,io,0,narg,optarg_,ivout)
	!	inprint=j_v(j_ivprintinput)
 
	1 inprint=j_v(j_ivprintinput)
	!	if(recursion.eq.1)then
	if(j_ndo.le.0.and.j_ninc.eq.1)write(6,*)' '
 
	!	write(6,*)'<63777',j_ninc,j_remain,j_err,j_inp(1:j_linp)
	!	if(j_ninc.eq.1)then
	!		call j_getinput('sit>',inprint)
	!	else
	!	write(6,*)'inprint2',inprint
	call j_getinput('# ',inprint)
 
 
	!	endif !if(j_ninc.eq.1)   8664
	!	write(6,*)'<63636',j_ninc,j_remain,j_err,j_inp(1:j_linp)
	!		write(6,*)'hhdhd'
	!write(6,*)'<per j_ninc remain',j_ninc,j_remain
	if(j_ninc.eq.1.and..not.j_remain)return
 
	!	write(6,*)'<545',j_inp(1:j_linp),j_ninc
	!	else !if(recursion.eq.1)then
	!		call j_getinput('sit'//char(48+recursion)//'>',inprint)
	!endif !if(recursion.eq.1)then
	if(j_err.and..not.j_remain)return
	!	write(6,*)'<431>recursion,',recursion,j_inp(1:j_linp)
	if(j_inp(1:j_linp).eq.'RETURN') return
	!	if(j_linp.eq.0.and.recursion.gt.1)goto 900
	if(j_inp(1:j_linp).eq.'end')then
		j_stop=.true.
		write(6,*)'stop'
		return
	endif !if(j_inp(1:j_linp).eq.'end')   9923
	!	write(6,*)'<7337',j_inp(1:j_linp)
	iiv=j_inciv(j_ninc)
	!	write(6,*) '  after using  ', j_o(iiv)%i(6),' lines from ',j_vname(iiv)
	if(j_ninc.gt.1.and.j_v(j_ivdebugconsole).ne.j_0)&
		call j_pause('<c'//j_chi5(j_o(iiv)%i(6),0)//'>',do=.true.)
	!	write(6,*)'<74774 ',j_err,j_inp(1:j_linp)
	if(.not.j_err)call j_parser(j_inp(1:j_linp),j_ivcursor)
	!		write(6,*)j_o(j_ivcursor)%i(0:20)
	!	write(6,*)'tas,recursion',recursion,j_err
	if(j_err)then
		call j_errexit()
		if(.not.j_remain)return
		!if(recursion.gt.1)goto 900
		j_err=.false.
		goto 1
	endif !if(j_err)   9937
 
	j_njump=0
	!	write(6,*)'<7e7e74994',j_o(j_ivcursor)%i(0:20)
	!	if(j_inp(1:4).eq.'p=5;')j_v(j_ivdebug)=300
	call dotrans(j_ivcursor,1)
	!	write(6,*)'{{',5,'j_err',j_err,'rec',j_recursion
	!write(6,*)'<22sit, err',j_err
	if(j_err)then
		!	write(6,*)'<22sit, err,calling errexit',j_err,'r ecursion ',recursion
		call j_errexit()
		!			write(6,*)'<22sit, errAFT,calling errexit',j_err,'recursion ',recursion
		if(.not.j_remain)return
		!		if(recursion.gt.1)goto 900
		j_err=.false.
	endif !if(j_err)   9951
 
 
	!write(6,*)'<44>',recursion
	!900 recursion=recursion-1
 
	goto 1
 
end subroutine j_sit !recursive subroutine sit(iob,io)
 
 
 
subroutine j_getinput(prompt,inprint,nul0t,single)  ! gets next input line
	implicit none
	character*(*), intent(in):: prompt
	integer, intent(in),optional:: inprint
	integer,intent(in),optional ::nul0t  ! at waht level we return
	logical,intent(in),optional::single
	integer inprint0
	logical printline
	logical isad
	logical edited  !,fromread  !frombuf
	logical inpuif
	!logical bypass
	character*24 name
	character*8 pauseprompt
	character*8 :: inpup='%       '
	character*30::ch
	!	character*16 valuec
	!20140623 contline
	!	logical contline  !is coniation line
	!	logical continues !continues
	!	logical yess !for testing
	integer,dimension(4)::iargs
	logical::p=.false.
	integer iii,nargs,i,ilie,lip
	logical done,doublesp
 
	if(present(inprint))then
		inprint0=inprint
	else
		inprint0=j_v(j_ivprintinput)
	endif !if(present(inprint))   9996
 
	lip=len(prompt)-1
	!	write(6,*)'lip ',lip,' /',inpup(1:lip),'/'
	doublesp=.true.
	if(present(single))doublesp=.not.single
	!	1 continue
	if(present(nul0t))then
 
		if(j_ninc.eq.nul0t)return
	endif !if(present(nul0t))  10007
1	j_linp=0

2	call initinput(prompt) !if we come here with j_linp>0  then continuation lines are mixed up

	if(j_err)return
 
	edited=.false.
	!write(6,*)'<159nollasu'
3 	continue  !we come here from below if we are getting continuation
	done=.false.
	!***
	call getinpr(pauseprompt)
	!	write(6,*)'inpr0',j_inpr(1:j_linpr)
	if(j_inpr(1:1).eq.'!'.or.j_inpr(1:1).eq.'*'.and..not.j_bypa(j_niifs))then
		write(6,*)j_inpr(1:j_linpr)
		goto  3
 
	endif !if(j_inpr(1:1).eq.'!'.or.j_inpr(1:1).eq.'*'.and..not.j_byp  10024
 
 4	continue
	!if(j_ninc.eq.1.and..not.j_remain)return

	!write(6,*)'<5454linpr,inpr',j_linpr,j_inpr(1:j_linpr),' j_linp ',j_linp,j_inp(1:j_linp)
	if(j_inpr(1:1).ne.';')then
		iii=j_object(j_inpr(1:j_linpr)) ! is just object name
		!	write(6,*)'<7776 888iii',iii
		if(iii.gt.0)then
			write(6,*)'% ',j_inpr(1:j_linpr)
			call shortcut(iii)
			if(j_err)return
			!write(6,*)'<6646inpt',j_inpr(1:j_linpr)
		endif !if(iii.gt.0)  10037
	endif !if(j_inpr(1:1).ne.';')  10034
 
 
	!	if(j_linpr.le.0)goto 161
	!	write(6,*)'<56 ',done
	call printstar(done)
	!	write(6,*)'<57 ',done
	if(done)goto 1
 
 
	if(j_ispause.and.j_inpr(1:j_linpr).eq.'e')then
		j_ispause=.false.
		goto 1
	endif !if(j_ispause.and.j_inpr(1:j_linpr).eq.'e')  10053
 
	if(j_inpr(1:6).eq.';pause')then
		if(j_linpr.eq.6)then
			pauseprompt=';pause>'
		else
			pauseprompt=j_inpr(8:j_linpr)
		endif !if(j_linpr.eq.6)  10059
		j_ispause=.true.
		goto 1
	endif !if(j_inpr(1:6).eq.';pause')  10058
 
	if(j_inpr(1:6).eq.';where')then
		write(6,*)'% ;where'
		if(j_ninc.eq.1)write(6,*)'just here, my dear'
		do i=j_ninc,2,-1
			iii=j_inciv(i)
			call j_getname( iii)
			write(6,*)'file ',j_oname(1:j_loname),' line ',j_o(iii)%i(6)
		enddo !i=j_ninc,2,-1  10071
		goto 1
	endif !if(j_inpr(1:6).eq.';where')  10068
	!
	!write(6,*)'<344befinpr',j_inpr(1:j_linpr)
	call sumdif()
 
 
 
	call j_puttext(j_ivinput1,j_inpr(1:j_linpr))
	!	if(j_err)return
	!	if(printline())call printlinesub()
 
 
	!write(6,*)'<a>',j_linp;
	!	write(6,*)'inpr1',j_inpr(1:j_linpr),.not.j_bypa(j_niifs)
 
 
 
 
	if(.not.j_bypa(j_niifs) )then
		!write(6,*)'tasbef:',j_inpr,j_linpr
		call inputinterpret2(j_inpr,j_linpr,edited)    !"sequences
		!		write(6,*)'tasaft:',j_inpr(1:j_linpr)
		if(j_err)return
		!	write(6,*)'liste ',j_inpr(1:j_linpr)
		call liste(edited)   !get liste elements @lis(ele)
		!	write(6,*)'list2 ',j_inpr(1:j_linpr)
		if(j_err)return
	endif !if(.not.j_bypa(j_niifs) )  10095
 
 
	call j_puttext(j_ivinput2,j_inpr(1:j_linpr))
 
	!	write(6,*)'<bypa ',j_niifs,j_bypa(j_niifs)
 
	if(.not.j_bypa(j_niifs))then
 
		call getreturn(done,doublesp,inpup(1:lip))  !get ;if();return
		if(j_err)return
		!	if(done)write(6,*)'<75775'
		if(done)goto 1
	endif !if(.not.j_bypa(j_niifs))  10111
 
	!	write(6,*)1,linp,'/',inp(1:linp)
	if(j_inpr(1:1).eq.';'.and.j_inpr(j_linpr:j_linpr).eq.':')goto 2
 
 
 
	if(j_inpr(1:3).eq.'if('.and.(index(j_inpr(1:j_linpr),';goto').gt.0.or. &
			index(j_inpr(1:j_linpr),';incl').gt.0))then
		write(6,*)'*****;goto and ;incl must be used after ;if( '
		j_err=.true.;	return
	endif !if(j_inpr(1:3).eq.'if('.and.(index(j_inpr(1:j_linpr),';got  10124
 
	if(j_inpr(1:1).eq.';')then  !input programming
		!	write(6,*)'<88888 ',j_inp(1:6)
		!	write(6,*)'<34printline()',printline()
		inpuif=.false.
		if(j_inpr(1:j_linpr).eq.';enddo'.or.j_inpr(1:j_linpr).eq.';endo')then
			call ienddo()
			if(j_err)return
			!write(6,*)'<enddo'
			if(j_ndo.eq.0.and.inprint0.ge.2) &
				write(6,*)inpup(1:lip)//j_inpr(1:j_linpr),'  !!DONE'
 
			goto 700 !nothing to add
 
		endif !if(j_inpr(1:j_linpr).eq.';enddo'.or.j_inpr(1:j_linpr).eq.'  10134
		if(inprint0.gt.0)then    !printline(inprint))then
			!		if(doublesp)write(6,*)' '
			!		write(6,*)'% '//j_inpr(1:j_linpr)
 
			write(6,*)inpup(1:lip)//j_inpr(1:j_linpr)
		endif !if(inprint0.gt.0)  10144
		!	if(j_inp(1:j_linp).eq.';enddo')write(6,*)'j_niifs ',j_niifs,' j_ndo ',j_ndo, 'j_niifsindo ',&
		!	j_niifsindo(j_ndo),' dostart ',j_dostart(j_ndo),'j_niifs,',j_niifs,' j_bypa(j_niifs) ', &
		!	j_bypa(j_niifs)
		if(j_inpr(1:6).eq.';incl(')then
			!			write(6,*)' '
			call j_incl(j_inpr(1:j_linpr))
			!	write(6,*)'>5977/j_ninc,j_incline(1:3)/',j_ninc,j_incline(1:3),j_increturn(1:3)
			!		write(6,*)' '
			if(j_err)return
			goto 1
			!	endif !if(j_inpr(1:6).eq.';incl('.or.j_inpr(1:6).eq.';goto(')   8815
		elseif(j_inpr(1:6).eq.';goto(')then
			call j_inpugoto(doublesp,inpup(1:lip))
			goto 1
 
			!	endif !if(j_inpr(1:j_linpr).eq.';enddo'.or.j_inpr(1:j_linpr).eq.'   8824
 
			!     write(6,*)'enddo,ndo,iido',ndo,(iido(kk,ndo),kk=1,5)
			!         iido(1,ndo)=iido(1,ndo)+iido(3,ndo)
 
			!	write(6,*)'<6784 iido ',j_ndo,j_iido(1:7,j_ndo)
 
		elseif(j_inpr(1:4 ).eq.';if(')then !if(j_inp(1:6).eq.';incl('.or.j_inp(1:6).eq.';goto(')then
			i=j_nextrp(j_inpr,4,j_linpr)
			if(i.gt.j_linpr)then
				write(6,*)'right parenthesis missing '
				j_err=.true.
				return
			else
				j_dapu=j_val(j_inpr(5:i-1))
			endif !if(i.gt.j_linpr)  10174
			if(j_err)return
			j_yes=j_dapu.eq.j_0
			if(j_inpr(i+1:j_linpr).eq.';then')then
				call getif(j_yes)
				goto 3
			else
				if(j_yes)then
					goto 3
				else
					j_inpr(1:j_linpr-i)=j_inpr(i+1:j_linpr) !may be dangerous in optimized code
					j_linpr=j_linpr-i
					!write(6,*)'<59inpr',j_inpr(1:j_linpr)
					inpuif=.true.
					goto 4
				endif !if(j_yes)  10187
 
			endif !if(j_inpr(i+1:j_linpr).eq.';then')  10183
 
			!	endif !if(j_inpr(1:4 ).eq.';if(')   8837
 
 
 
 
		elseif(j_inpr(1:7).eq.';elseif')then !if(j_inp(1:6).eq.';incl('.or.j_inp(1:6).eq.';goto(')then
			call ielseif()
			if(j_err)return
			goto 3
			!		endif !if(j_inpr(1:7).eq.';elseif')   8866
 
 
 
		elseif(j_inpr(1:j_linpr).eq.';else')then !if(j_inp(1:6).eq.';incl('.or.j_inp(1:6).eq.';goto(')then
			call ielse()
			if(j_err)return
			goto 3
			!		endif !if(j_inpr(1:j_linpr).eq.';else')   8874
 
 
		elseif(j_inpr(1:j_linpr).eq.';endif')then !if(j_inp(1:6).eq.';incl('.or.j_inp(1:6).eq.';goto(')then
			j_niifs=j_niifs-1
			!write(6,*)';endif ',j_niifs
			goto 700  !nothing added from inpr
 
 
		elseif(j_inpr(1:4).eq.';do(')then !if(j_inp(1:6).eq.';incl('.or.j_inp(1:6).eq.';goto(')then
			!	write(6,*)'<6668',j_inpr(1:j_linpr),' contline ',contline
			call j_getdos(j_inpr,j_linpr,iargs,nargs)
			!		write(6,*)'<588585'
			if(j_err)return
			!	write(6,*)'cbbc'
			goto 3
		endif !if(j_inpr(1:6).eq.';incl(')  10153
		if(.not.inpuif)then
			write(6,*)'illegal input programming line'
			j_err=.true. ;return
		endif !if(.not.inpuif)  10233
 
	endif !if(j_inpr(1:1).eq.';')  10130
 
 
	!	if(j_bypa(j_niifs))write(6,*)'bypassniifs ',j_niifs
	if(j_bypa(j_niifs))goto 1
 
 
	j_inp(j_linp+1:j_linp+j_linpr)=j_inpr(1:j_linpr)
	j_linp=j_linp+j_linpr
	!write(6,*)'<4477 j_linp,j_inp',j_linp,j_inp(1:j_linp)
 
 700 continue
	! if(j_linp.gt.0)then
	if(j_linp.eq.0)goto 3
 
	if(any(j_inp(j_linp:j_linp).eq.(/'>',',','+','*','(','/','=','-'/)))then
		if(j_linp.gt.2)then
			if(j_inp(j_linp-1:j_linp).eq.'>>')j_linp=j_linp-2
		endif !if(j_linp.gt.2)  10254
		!	write(6,*)'<cont ',j_inp(1:j_linp)
		if((j_linp.gt.1.and.j_inp(1:j_linp).ne.'//').or.j_inp(1:1).ne.'/')goto 3
	endif !if(any(j_inp(j_linp:j_linp).eq.(/'>',',','+','*','(','/','  10253
	!
	if(j_ispause)then
		if(j_inp(1:j_linp).eq.'e'.or.j_inp(1:j_linp).eq.'/')then
			j_ispause=.false.
			goto 1
		endif !if(j_inp(1:j_linp).eq.'e'.or.j_inp(1:j_linp).eq.'/')  10262
		call j_command(j_inp(1:j_linp))
		j_err=.false.
		goto 1
 
	endif !if(j_ispause)  10261
	j_incline2=j_incline()
	if(j_ninc.gt.1.and.inprint0.gt.0)then !printline(inprint))then
		!	if(j_ndo.gt.0)write(6,*)'iido ',j_iido(1:7,1)
		if(doublesp)write(6,*)' '
		!	write(6,*)'doublesp ',doublesp
		!	write(6,*)'# '//j_inp(1:j_linp)
		write(6,*)prompt//j_inp(1:j_linp)
	endif !if(j_ninc.gt.1.and.inprint0.gt.0)  10272
	if(j_inp(1:j_linp).eq.'end')then
		j_stop=.true.;return
 
	endif !if(j_inp(1:j_linp).eq.'end')  10279
	!
	!if(j_ninc.gt.1)write(6,*)'<88'//j_inp(1:j_linp)
	!	if(j_inp(1:j_linp).eq.'/'.and..not.j_inpara2)j_inpara=.false.
 
	return
 
 
 
	! 99 write(6,*)'**error reading input'
	! j_err=.true.;return
	!write(6,*)'got',inp
	!	return
 
end subroutine j_getinput
 
subroutine initinput(prompt)
	character*(*) prompt
	!	integer lenp
	if(j_linp.gt.0)then
		write(6,*)'was expecting continuation for input: ',j_inp(1:j_linp)
		j_err=.true.
 
	endif !if(j_linp.gt.0)  10301
	!		j_linp=0  !output collected to j_inp
 
	!write(6,*)'getinput <1506> nul(0)',j_nul(0:j_ninc),nu!   oinp, lineobuf ',nul(0), oinp, lineobuf
	!if(p)write(6,*)'<66ivin',j_ivinput0,j_ivinput1,j_ivinput2
	!if(p)write(6,*)'<66ivin',allocated(j_o(j_ivinput0)%i)
	j_o(j_ivinput0)%i(0)=0 !original
	j_o(j_ivinput1)%i(0)=0 !cleaned
	j_o(j_ivinput2)%i(0)=0 !interpreted
 
	lenp=len(prompt)
	!	write(6,*)'<73474tas',lenp,prompt
	!if(p)write(6,*)'<487',j_ninc
 
	j_incline1=j_incline()+1
 
 
	!77 if(prompt.ne.' '.and.j_ninc.eq.1.and.j_ndo.eq.0) write(6,'(a,$)')prompt    !write!
	if(j_ninc.eq.1) write(6,'(a,$)')'sit>'
	!	contline=.false.
	!		continues=.false.
	!write(6,*)'<59z>contline FALSE uusikutsu'
	!	j_linpr=0
	return
 
 
end subroutine initinput
 
subroutine getinpr(pauseprompt)
	character*(*) pauseprompt
	integer iiv,iline
77		j_linpr=0
	if(j_ispause)then
		jninc=1
		write(6,'(a,$)')pauseprompt(1:len_trim(pauseprompt))
	else
		jninc=j_ninc
	endif !if(j_ispause)  10337
 
	do while(j_linpr.le.0)
		! if(j_ispause)then
		! write(6,'(a,$)')pauseprompt(1:len_trim(pauseprompt))
		! !		call j_pause(pauseprompt(1:len_trim(pauseprompt)))
		!		if(.not.j_ispause)goto 77
		if(jninc.eq.1)then !if(j_ispause)then
			!****************************************************************************************
			!  printed=.false.
			!** read input line   !** key points with respect to  read/print
 
			! if(nu.gt.0)then
			!    read(nu,100,end=90,err=99)inp(ial:ial+LLINE-1)
			!inquire(unit=nu,opened=yess )
 100 format(a)
500			read(5,100)j_inpr

			!	j_incline(j_ninc)=j_incline(j_ninc)+1
			!###TESTAUS###
			!write(6,*)'getinput <1571> read: nu, inpr ',nu, j_inpr(1:20)
			!  if(.true.)write(16,*)inp(ial:ial+LLINE-1)
			j_linpr=j_lentrim(j_inpr)
			if(j_linpr.eq.0)then
				write(6,'(a,$)')pauseprompt
				goto 500
			endif !if(j_linpr.eq.0)  10365
 
			! endif !if(j_linpr.eq.0)   9305
 
			!	write(6,*)'<3773inpr',j_inpr(1:j_linpr)
			!if(index(j_inpr(1:j_linpr),'figure(1/0)').gt.0)write(6,*)'puttasbef',j_inpr(1:j_linpr)
			call j_clean(j_inpr(1:j_linpr),j_linpr)
			!	if(j_inpr(1:j_linpr).eq.'/'.and..not.j_ispause)j_inpara=.false.
			!if(index(j_inpr(1:j_linpr),'figure(1/0)').gt.0)write(6,*)'puttaaff',j_inpr(1:j_linpr)
			if(j_inpr(1:j_linpr).eq.'end')then
				if(j_remain)stop 'FINITO'
				j_stop=.true.
				!		write(6,*)'dhhd'
				return
			endif !if(j_inpr(1:j_linpr).eq.'end')  10377
			if(j_err)return
 
			j_printed=.true.
			call j_puttext(j_ivinput0,j_inpr(1:j_linpr))
 
		else !if(j_ispause)then
			iiv=j_inciv( j_ninc)
			j_o(iiv)%i(6)=j_o(iiv)%i(6)+1
			!	j_incline(j_ninc)=j_incline(j_ninc)+1
			call j_getname(iiv)
 
			iline=j_o(iiv)%i(6)
			!		write(6,*)'iline ',iline,j_oname(1:j_loname)
			if(iline.gt.j_o(iiv)%i(5))then
				j_o(iiv)%i(6)=0
				if(inprint0.gt.0)write(6,*)'<end of txt>'
				call goto90()
			endif !if(iline.gt.j_o(iiv)%i(5))  10396
 
			! write(6,*)'233 ',j_nul,'iv ',j_inc iv, 'line ',j_incline,' iiv',iiv
			! write(6,*)'<7787',j_inciv
			!	call j_printname(' sjjsjs ',iiv,' ')
			j_linpr=j_o(iiv)%i2(iline)
 
			j_inpr(1:j_linpr)=j_o( iiv)%txt(iline)(1:j_linpr)
			!	write(6,*)'<73berf',j_linpr,j_inpr(1:j_linpr)
			!if(index(j_inpr(1:j_linpr),'figure(1/0)').gt.0)write(6,*)'putcleanbef',j_inpr(1:j_linpr)
			call j_clean(j_inpr(1:j_linpr),j_linpr)
			!	if(index(j_inpr(1:j_linpr),'figure(1/0)').gt.0)write(6,*)'putcafef',j_inpr(1:j_linpr)
			!	write(6,*)'<73aff',j_linpr,j_inpr(1:j_linpr)
			!	write(6,*)'<7779 j_ninc,j_incline(1:3)' ,j_ninc,j_incline(1:3)
			!	call j_getline(j_ivbuf(j_ninc),j_icurl(j_ninc),j_inpr,j_linpr)
			!write(6,*)'<10745>gotinpr buf:',j_inpr(1:j_linpr)
			if(j_err)return
			!   if(inp(1:1).eq.'/')write(6,*)'got/,icurl',icurl(nul(0))
			!		frombuf=.true.
 
		endif !if(jninc.eq.1)  10349
		!**********************************
	end do !while(j_linpr.le.0)  10344
 
end subroutine getinpr
 
subroutine printstar(done)
	logical done
	integer ist,ie,ial2,ivmaxlines,maxlines,iv,le1,linpr,iper,ivlist
	!	write(6,*)'<<',j_inpr(1:j_linpr)
	ist=index(j_inpr(1:j_linpr),'?')
	ie=index(j_inpr(1:j_linpr),'=;list(')
 
	ial=1
	if(j_inpr(1:1).eq.'@')ial=2
	!	write(6,*)'<66 ',ist,ie
	np=0
	ial2=ist+1 !character after ?
	!	write(6,*)'ist,ie,ial,np', ist,ie,ial,np,j_inpr(j_linpr:j_linpr),j_linpr,j_inpr(j_linpr:j_linpr).eq.';'
	if(ist.gt.0.and.(j_inpr(j_linpr:j_linpr).eq.';'.or.ie.gt.1))then
		!		write(6,*)'here'
		write(6,*)' '
		if(j_ninc.ne.1)write(6,*)'% ',j_inpr(1:j_linpr)
 
		if(j_inpr(j_linpr:j_linpr).eq.';'.and.ie.eq.0)then
 
			ivmaxlines=j_object('Maxlines')
			maxlines=100
			if(ivmaxlines.gt.0)maxlines=j_v(ivmaxlines)
			if(maxlines.le.0)then
				write(6,*)'Maxlines=',maxlines,' illegal!'
				j_err=.true.
				return
			endif !if(maxlines.le.0)  10449
			j_yes=ial2.le.j_linpr-1  !there is somthing after * in the name
			!		write(6,*)'j_yes,ial2 ',j_yes,ial2
			nf=0
			do iv=1,j_namedv
				call j_getname(iv)
				if(j_inpr(ial:ist-1).ne.j_oname(1:ist-ial))cycle
				if(j_yes)then
					le1=j_linpr-ist-1
					!		write(6,*)'j_linpr,ist,le1,ist+1',j_linpr,ist,le1,ist+1
					if(j_loname.le.le1)cycle
					if(j_inpr(ist+1:ist+le1).ne.j_oname(j_loname-le1+1:j_loname))cycle
				endif !if(j_yes)  10460
				if(ial.eq.2)then
					!				write(6,*)'<391',iv
					call j_print(iv,'*','*',maxlines,j_v(j_ivdebug).ne.j_0)
					write(6,*)' '
					if(j_err)return
				else
					write(6,*)'  ',j_oname(1:j_loname)
				endif !if(ial.eq.2)  10466
				nf=nf+1
			enddo !iv=1,j_namedv  10457
			write(6,'(15x,i4,a)')nf,' objects found '
			!	write(6,*)' '
			if(nf.eq.0)write(6,*)'no matching object names found'
		else  !list
			!write(6,*)'<88>',j_inpr(1:j_linpr),'<'
			!		write(6,*)'herePAS'
			if(j_inpr(j_linpr:j_linpr).eq.')')then
				linpr=j_linpr-1
				j_yes=.false.
			elseif(j_inpr(j_linpr-1:j_linpr).eq.');')then
				linpr=j_linpr-2
				j_yes=.true.   !print
			else
				write(6,*)'line should end with ) or );'
				j_err=.true. ;return
			endif !if(j_inpr(j_linpr:j_linpr).eq.')')  10482
			j_yes2=ial2.le.linpr  !is there ending
			!	write(6,*)'j_yes2 ',j_yes2
			!	write(6,*)'<44>',j_inpr(1:ie-1)
			ivlist=j_deflistobject(0,j_inpr(1:ie-1),nres=20)
			le1=ist-ie-7  !ist-1-ie-6+1
			!		if(j_yes2)write(6,*)j_oname(j_loname-le1:j_loname)
			!	write(6,*)'<55>',j_inpr(ie+6:ist-1),le1,'ial2 ',ial2,j_yes2
			!	if(ist+1.le.j_linpr)write(6,*)'aft',j_inpr(ist+1:j_linpr)
			!		write(6,*)'vcvc',j_inpr(ie+7:ist-1),' le1 ',le1
			do iv=1,j_namedv
				call j_getname(iv)
 
				if(j_inpr(ie+7:ist-1).ne.j_oname(1:le1))cycle
				if(j_yes2)then
					if(j_loname-le1.le.0)cycle
					if(j_inpr(ist+1:linpr).ne.j_oname(j_loname-le1+1:j_loname))cycle
				endif !if(j_yes2)  10505
				iper=j_putlistobject(ivlist,single=iv)
 
			enddo !iv=1,j_namedv  10501
			if(j_yes)call j_printlist(6,ivlist)
 
 
		endif !if(j_inpr(j_linpr:j_linpr).eq.';'.and.ie.eq.0)  10444
 
		done=.true.
	endif !if(ist.gt.0.and.(j_inpr(j_linpr:j_linpr).eq.';'.or.ie.gt.1  10439
 
 
end subroutine
 
 
subroutine sumdif()
	integer isum
	192	isum=index(j_inpr(1:j_linpr),';sum(')
	if(isum.gt.0)then
		!	write(6,*)'sum:',j_inpr(1:j_linpr)
		call j_getsum(j_inpr,j_linpr,.true.)
		!	if(isum.gt.0)write(6,*)'<8out',j_inpr(1:j_linpr),j_linpr
		if(j_err)return
		goto 192
	endif !if(isum.gt.0)  10527
193	isum=index(j_inpr(1:j_linpr),';dif(')
	if(isum.gt.0)then
		call j_getsum(j_inpr,j_linpr,.false.)
		!	if(isum.gt.0)write(6,*)'<8out',j_inpr(1:j_linpr),j_linpr
		if(j_err)return
		goto 193
	endif !if(isum.gt.0)  10535
 
 
end subroutine
 
! subroutine printlinesub()
! integer lenp
! if(j_niifs.ge.1.and.j_bypa(max(1,j_niifs)))then
! if(j_ninc.gt.1) write(6,'(a)')j_inpr(1:j_linpr)    !write!
! else !if(j_niifs.ge.1.and.j_bypa(max(1,j_niifs)))then
! if(contline)then  !reading j_inpr
! if(j_ninc.gt.1)write(6,'(a)')j_inpr(1:j_linpr)
! else !if(contline)then
! if(inprint.ge.2.and.j_ninc.gt.1)write(6,*)' '
! if(j_inp(1:1).ne.';')then
! if(j_ninc.gt.1) write(6,'(a)')prompt(1:max(lenp-1,1))//'< '//j_inpr(1:j_linpr)
! else !if(j_inp(1:1).ne.';')then
! if(j_ninc.gt.1) write(6,'(a)')j_inpr(1:j_linpr)
! endif !if(j_inp(1:1).ne.';')   9156
! endif !if(contline)   9152
! endif !if(j_niifs.ge.1.and.j_bypa(max(1,j_niifs)))   9149
! end subroutine
 
subroutine shortcut(iii)
	!	write(6,*)'<33util iii',iii
	integer iaa,ihi
	if(j_otype(iii).eq.j_ipchar)then
		if(j_o(iii)%i(3).ne.iii)then   !character variable
			!		write(6,*)'<66 ',j_o(iii)%i
			call j_getchar(iii,j_inpr,j_linpr)
			iaa=1
 
			ihi=j_nextlim(j_inpr,iaa,j_linpr,'~')
			do while (ihi.le.j_linpr)
				j_inpr(ihi:ihi)="'"
				iaa=ihi+1
				ihi=j_nextlim(j_inpr,iaa,j_linpr,'~')
			end do !while (ihi.le.j_linpr)  10573
			! ial=1
			!** key shortcut
			!			edited=.true.
			!	write(6,*)'<64333 ',j_linpr,j_ninc,j_incline(j_ninc),j_inpr(1:j_linpr)
			!call debugi('!<a+>')
 
			! if(j_inpr(1:6).eq.';incl('.and.j_ninc.gt.1)then
			! !		j_ninc2(j_ninc)=j_ninc2(j_ninc)+1
			! j_increturn(j_ninc,j_ninc2(j_ninc))=j_o(iii)%i(6) !j_incline(j_ninc)
 
			! write(6,*)'ninc2 after shortcut ninc ,ninc2 ' ,j_ninc,j_ninc2(j_ninc)
			! endif !if(j_inpr(1:6).eq.';incl('.and.j_ninc.gt.1)   9250
 
			j_linp=0
			return
		endif !if(j_o(iii)%i(3).ne.iii)  10567
		!if(reacom(nul(0)).and.inprint.gt.1)write(6,*)'->',inp(ial:linp)
	else
		write(6,*)'<',j_inpr(1:j_linpr)
		write(6,*)'JLP22 did not understand'
		j_err=.true.;return
	endif !if(j_otype(iii).eq.j_ipchar)  10566
 
 
end subroutine
 
subroutine liste(edited)
	logical edited
	!get list element using @list(ele
	logical islist
	integer iald,ilie,lef,i,nexr,inde,lesn
	iald=1
 
	!	write(6,*)'ILIE',ilie
	ilie=j_nextlim(j_inpr,iald,j_linpr,'@',inhipsu=.true.)
 
	!	write(6,*)'ILIE',ilie,j_linpr
	do while(ilie.lt.j_linpr)
		!		write(6,*)'ILIE',ilie,j_linpr
		!	if(ilie.le.j_linpr)then
		lef=j_nextlim(j_inpr,ilie+1,j_linpr,'(,=+-/*')
		!		write(6,*)'lef ',j_inpr(lef:lef)
		if(lef.gt.j_linpr)return  !goto 881
		if(j_inpr(lef:lef).ne.'(')then
			iald=lef+1
			goto 8800
		endif !if(j_inpr(lef:lef).ne.'(')  10621
 
		i=j_object(j_inpr(ilie+1:lef-1))
 
		!	write(6,*)j_inpr(ilie+1:lef-1),i
		if(i.le.0)then
			write(6,*)j_inpr(ilie+1:lef-1), ' after @ is not an object '
			goto 900
			! write(16,*)'lef,iald,linp,',lef,iald,linp,inp(1:linp)
		endif !if(i.le.0)  10629
		!20140519 oli: if(otype(i).ne.iplist)then
		islist=j_otype(i).eq.j_iplist
		!	write(6,*)'islist',islist
		if(.not.islist.and.j_otype(i).ne.j_iptext)then
			call j_printname('**not list or text object: ',i, ' ')
			!!   write(6,*)'#input:',inp(1:linp7)
			goto 900
		endif !if(.not.islist.and.j_otype(i).ne.j_iptext)  10637
		nexr=j_nextrp(j_inpr,lef,j_linpr)
		if(nexr.gt.j_linpr.or.nexr.eq.lef+1)then
			write(6,*)'**no legal matching ) after ',j_inpr(1:lef)
			! write(6,*)'#input:',inp(1:linp)
			goto 900
		endif !if(nexr.gt.j_linpr.or.nexr.eq.lef+1)  10643
		!write(6,*)'<234>',j_inp(lef+1:nexr-1)
		inde=j_val(j_inpr(lef+1:nexr-1))
		if(j_err)then
			write(6,*)'error in ',j_inpr(lef+1:nexr-1)
			goto 900
		endif !if(j_err)  10650
		!write(6,*)'input: ',inp(1:linp)
		!return
		! end if
		!	write(6,*)'inde',inde
		if(islist)then
			if(inde.le.0.or.inde.gt.j_o(i)%i(1))then
				! 20140519 virheilmoituksen täydennys (tekstiobjekti)
				call j_printname('**not legal index for ',i,' ')
				write(6,*)'#index: ',j_inpr(lef+1:nexr-1),'=',inde,' should be between 1 and ', j_o(i)%i(1)
				goto 900
			endif !if(inde.le.0.or.inde.gt.j_o(i)%i(1))  10659
			!	write(6,*)'i,inde,j_o(i)%i2(inde)',i,inde,j_o(i)%i2(inde)
			call j_getname(j_o(i)%i2(inde))
			!		write(6,*)'ilie,nexr,j_linpr,j_oname(1:j_loname)',ilie,nexr,j_linpr,j_oname(1:j_loname)
 
			nh=0
			if(j_oname(1:1).eq."'")then
				nh=0
				do j=1,ilie
					if(j_inpr(j:j).eq."'")then
						if(nh.eq.1)then
							nh=0
						else
							nh=1
						endif !if(nh.eq.1)  10674
					endif !if(j_inpr(j:j).eq."'")  10673
				enddo !j=1,ilie  10672
			endif !if(j_oname(1:1).eq."'")  10670
			if(nh.eq.0)then
				call j_repl(j_inpr,ilie,nexr,j_linpr,j_oname,j_loname)
 
				lesn=j_loname
 
			else
				call j_repl(j_inpr,ilie,nexr,j_linpr,j_oname(2:),j_loname-2)
				lesn=j_loname-2
 
			endif !if(nh.eq.0)  10682
			!		else
			! if(inde.le.0.or.inde.gt.j_o(i)%i(0))then
			! write(6,*)'#index: ',j_inp(lef+1:nexr-1),'=',inde,' should be between 1 and ', j_o(i)%i(0)
			! j_err=.true.;return
			! endif !if(inde.le.0.or.inde.gt.j_o(i)%i(0))   9374
			! call j_getline(i,inde,j_tempchar,lesn)
			! call j_repl(j_inpr,ilie,nexr,j_linpr,j_tempchar,lesn)
		endif !if(islist)  10658
 
		! write(16,*)'after',inp(1:linp)
		! @a(1)  ->z
		!     ilie=1  nexr=5  lesn=1
		! z
		edited=.true.
		iald=ilie+lesn  ! -(nexr-ilie)+4 ! could get better using difference between current and previous linp
		!write(6,*)'iald,ilie,lesn,nexr',iald,ilie,lesn,nexr
		!read(5,*)ihep
		! write(16,*)'iald,ilie,lesn,linp,nexr',iald,ilie,lesn,linp,nexr,inp(1:linp)
8800		ilie=j_nextlim(j_inpr,iald,j_linpr,'@',inhipsu=.true.)
		!if(iald.lt.j_linp)goto 8800
	end do !while(ilie.lt.j_linpr)  10615
	return
 900 j_err=.true.	 !write(6,*)'% ',j_inpr(1:j_linpr)
	return
 
end subroutine liste !subroutine liste()
 
 
 
!end subroutine !subroutine j_getinput(prompt,inprint,nul0t)
 
 
! subroutine startdo()
 
! integer ivout_
 
 
 
! if(j_ndo.ge.j_mxndo)then
! write(6,*)j_ndo,j_mxndo
! write(6,*)'**too many ;do -loops'
! j_err=.true.
! j_ndo=0
! return
! endif !if(j_ndo.ge.j_mxndo)then
! if(j_ndo.eq.j_ndoinc(j_ninc))then
! if(j_reacom(j_ninc))then
! ndotemp=1
! if(.not.j_savcom(j_ninc))then
! call j_deftext2(0,'Buffer'//char(47+j_ninc),500,10000,50,j_ivbuf(j_ninc))
! j_savcom(j_ninc)=.true.
! call j_puttext(j_ivbuf(j_ninc), j_inp(1:j_linp))
! istart=1
! else !if(.not.j_savcom(j_ninc))then
! istart=j_o(j_ivbuf(j_ninc))%i(0)  !cuuren line in text buffer
! endif !if(.not.j_savcom(j_ninc))then
! icu=istart
! !      write(6,*)'istart',istart
! 100	 	format(a)
! 10 		continue
! if(j_nul(j_ninc).gt.0)then
! read(j_nul(j_ninc),100,end=90,err=90)j_inp
! j_linp=j_lentrim(j_inp)tatrdo
! else !if(j_nul(j_ninc).gt.0)then
! ifi=-j_nul(j_ninc)
! if(j_incline(j_ninc).ge.j_o(ifi)%i(0))goto 90
! j_incline(j_ninc)=j_incline(j_ninc)+1
! call j_getline(ifi,j_incline(j_ninc),j_inp(1:),j_linp)
! if(j_err)return
! endif !if(j_nul(j_ninc).gt.0)then
 
! j_ialb=j_nonblank(j_inp,1,j_linp)
! ialb2=j_nonblank(j_inp,j_ialb+3,j_linp)
! if(j_inp(1:j_linp).eq.'endsimu')then
! j_inp=';enddo';j_linp=6 !;ialb2=1
! endif !if(j_inp(1:j_linp).eq.'endsimu')then
! call j_puttext(j_ivbuf(j_ninc), j_inp(1:j_linp))
 
! icu=icu+1
 
! if(j_inp(j_ialb:j_ialb+2).eq.';do'.and.j_inp(ialb2:ialb2).eq.'(')then
! ndotemp=ndotemp+1
! endif !if(j_inp(j_ialb:j_ialb+2).eq.';do'.and.j_inp(ialb2:ialb2).eq.'(')then
! if(j_inp(j_ialb:j_ialb+5).eq.';enddo'.or.j_inp(j_ialb:j_ialb+6).eq.';end do')then
! ndotemp=ndotemp-1
! endif !if(j_inp(j_ialb:j_ialb+5).eq.';enddo'.or.j_inp(j_ialb:j_ialb+6).eq.';end do')then
 
! if(ndotemp.eq.0)goto 17
! goto 10
! 90 		write(6,*)'unclosed ;do loop'
! j_err=.true.
 
! goto 900
! else !if(j_reacom(j_ninc))then
! istart=j_o(j_ivbuf(j_ninc))%i(0)
 
! endif !if(j_reacom(j_ninc))then
! else !if(j_ndo.eq.j_ndoinc(j_ninc))then
! istart=j_icurl(j_ninc)-1 ! this is set to next already
! endif !if(j_ndo.eq.j_ndoinc(j_ninc))then
 
! 17 ilow=j_v(j_o(iob)%i(io+3)); iup=j_v(j_o(iob)%i(io+4))
! if(narg.le.3)then
! istep=1
! else !if(narg.le.3)then
! istep=j_v(j_o(iob)%i(io+5))
! endif !if(narg.le.3)then
 
! if(istep.eq.0)then
! write(6,*)'**illegal ;do loop with low,up,step:',ilow,iup,istep
! j_err=.true.
 
! goto 900
! endif !if(istep.eq.0)then
 
! nstep=(iup-ilow+istep)/istep
! if(nstep.le.0)then
 
! if(j_ndo.eq.0)j_icurl(j_ninc)=istart+1
! 567		call j_getline(j_ivbuf(j_ninc),j_icurl(j_ninc),j_inp(1:j_lline),j_linp)
! if(j_err)return
! ialb2=4
! ndotemp=1
! if(j_inp(1:3).eq.';do'.and.j_inp(4:4).eq.'(')ndotemp=ndotemp+1
! if(j_inp(1:6).eq.';enddo')ndotemp=ndotemp-1
! j_icurl(j_ninc)=j_icurl(j_ninc)+1
! if(ndotemp.gt.0)goto 567
 
! return
! endif !if(nstep.le.0)then
! j_ndo=j_ndo+1
! j_niifsindo(j_ndo)=j_niifs
 
! j_iido(1,j_ndo)=ilow
! j_iido(2,j_ndo)=iup  !ilow+(nstep-1)*istep !up
! j_iido(3,j_ndo)=istep
! j_iido(4,j_ndo)=j_o(iob)%i(io+2)
! j_v(j_o(iob)%i(io+2))=ilow
! j_iido(5,j_ndo)=istart
! j_iido(6,j_ndo)=j_ninc
! j_iido(7,j_ndo)=ilow !  iido(1 is updated, thsi stores the intial value
! if(j_ndo.eq.1)j_printdo=j_linkoption(iob,io,j_mprint).gt.0
! call j_clearoption(iob,i)  ! subroutine
 
! if(j_ndo.eq.1)then
! j_icurl(j_ninc)=istart+1
! endif !if(j_ndo.eq.1)then
! j_reacom(j_ninc)=.false.
! 900 continue
! call j_clearoption(iob,i)  ! subroutine
! return
! end subroutine startdo !subroutine dos(iob,io_)
 
 
 
 
!	end subroutine
 
subroutine expandlist(inp,linp,edited)  !@
	character*(*) inp
	character*60 name
	logical edited
	!	iald=2
	iald=1
	edited=.false.
	!	write(6,*)'<644>iald,inp',iald,linp,inp(1:linp)
	8800 ilie=j_nextlim(inp,iald,linp,'@')
	!write(6,*)'<6456',ilie,ilie
	if(j_err)return
	if(ilie.gt.linp)return
	lop=j_nextlim(inp,ilie+1,linp,',)')
	if(j_err)return
	!	if(lop.gt.linp)return
 
	!write(6,*)'<aa>',lef-1-(ilie+1)+1;		i=j_object(inp(ilie+1:lef-1))
	iv=j_object(inp(ilie+1:lop-1))
	itype=0
	if(iv.gt.0)itype=j_otype(iv)
	if(itype.eq.0.or.j_otype(iv).ne.j_iplist)then
		write(6,*)inp(ilie+1:lop-1),' is not a list'
		j_err=.true.
		return
	endif !if(itype.eq.0.or.j_otype(iv).ne.j_iplist)  10870
	le2=0
	!write(6,*)'<6346346>nele',j_o(iv)%i(0)
	do j=1,j_o(iv)%i(1)
		call j_objectname(j_o(iv)%i2(j),j_tempchar(le2+1:),lena)
		if(j_err)then
			write(6,*)'*j* size of j_tempchar should be increased'
			j_err=.true.
			return
		endif !if(j_err)  10879
 
		le2=le2+lena
		if(j.lt.j_o(iv)%i(1))then
			j_tempchar(le2+1:le2+1)=','
			le2=le2+1
		endif !if(j.lt.j_o(iv)%i(1))  10886
		!	write(6,*)'<466464',j,le2,j_tempchar(1:le2)
	enddo !j=1,j_o(iv)%i(1)  10877
	!write(6,*)'<443>ilie,linp,j_linp,le2,inp,tempchar',ilie,linp,j_linp,le2,inp(1:linp)
	write(6,*)j_tempchar(1:le2)
	call j_repl(inp,ilie,lop-1,linp,j_tempchar,le2)
	!replaces the substring jono1(i1:i2) by string jono2(1:le2)
	!	call j_insert(inp,ilie-1,linp,j_tempchar,le2)
	iald=iald+le2-1
	!write(6,*)'<52525',inp(1:linp)
	if(j_err)return
	edited=.true.
	goto 8800
 
	!20140618 siirretty kohdasta <1648> --loppuu
end subroutine !subroutine expandlist(inp,linp,edited)
 
subroutine j_insert(line,leline0,leline,line2,leline2)
	character*(*),intent(inout) ::line  !object name
	integer,intent(in):: leline0  !lengt of initial part
	integer,intent(inout):: leline  !current and final end of line
	character*(*),intent(in) ::line2  !inserted text
	integer,intent(in)::leline2 ! length of the inserted text
 
 
	if(leline+leline2.gt.len(line))then
		write(6,*)'*j* j_insert: len(line)=',len(line),' is too small'
 
		j_err=.true.
		return
	endif !if(leline+leline2.gt.len(line))  10914
 
	leline3=leline+leline2
	do j=0,leline2-1
		line(leline3-j:leline3-j)=line(leline-j:leline-j)
	enddo !j=0,leline2-1  10922
	line(leline0+1:leline0+leline2)=line2(1:leline2)
	leline=leline3
 
end subroutine !subroutine j_insert(line,leline0,leline,line2,leline2)
 
subroutine inputinterpret2(inp,linp,edited) ! interpreting "
	character*(*)inp
	integer linp
	logical edited
	character*16::valuec
	double precision::value
 
	iald=1
88  ido=index(inp(iald:linp),'"')+iald-1
!	write(6,*)'<6667 ',inp(1:linp),ido,iald
	if(ido.ge.iald)then
		!write(6,*)'iald,linp',iald,linp,inp(iald:linp)
		ido2=index(inp(ido+1:linp),'"')+ido
		!	write(6,*)'<6667ido2',inp(1:linp),ido2
		!write(6,*)'ido,ido2,linp',ido,ido2,linp
		if(ido2.le.ido+1)then
			write(*,*)'**no proper " sequence in:'// &
				inp(iald:linp);goto 90
		end if !if(ido2.le.ido+1)  10945
		iva=j_object(inp(ido+1:ido2-1))
		if(iva.gt.0)then
			!       subroutine jrepl(jono1,i1,i2,linp,jono2,le2)
			!replaces the substring jono1(i1:i2) by string jono2(1:le2)
			call j_getchar(iva,j_adr,les)
			call j_repl(inp,ido,ido2,linp,j_adr,les)
 
			!write(6,*)'aft,ido,ido2,linp,',ido,ido2,linp,':',inp(1:linp)
		else !if(iva.gt.0)then
 
			if(inp(ido+1:ido+1).eq.'[')then
				lopvak=j_nextlim2(inp,ido+1,ido2-1,']')  ! [] not ignored
				if(lopvak.ge.ido2-1)then
 
					write(*,*)'**no proper [] sequence in ',inp(ido+1:ido2-1);j_err=.true.;return
				endif !if(lopvak.ge.ido2-1)  10961
				!	write(6,*)'<6832>',inp(lopvak+1:ido2-1)
				value=j_val(inp(lopvak+1:ido2-1)) ;if(j_err)goto 90
				write(valuec,'('//inp(ido+2:lopvak-1)//')',err=1356)value
				call j_repl(inp,ido,ido2,linp,valuec,len_trim(valuec))
				goto 1357
1356			 write(6,*)'*error reading [] sequence: ',inp(ido+1:lopvak)
				goto 90
				!	return
		1357 continue
			else !if(inp(ido+1:ido+1).eq.'[')then
				!	write(6,*)'<6891',inp(ido+1:ido2-1)
				value=j_val(inp(ido+1:ido2-1)) ;if(j_err)return
				!	write(6,*)'value ',value
				!	write(6,*)inp(1:linp)
				call j_reps2(inp,ido,ido2,linp,value)
				!	write(6,*)inp(1:linp)
			endif !if(inp(ido+1:ido+1).eq.'[')  10959
			!write(6,*)'replace:',inp(ido+1:ido2-1)
			!write(6,*)'aft,ido,ido2,linp,',ido,ido2,linp,':',inp(1:linp)
		endif !if(iva.gt.0)  10950
		!write(6,*)'after:',inp(ial:linp),ido+1,index(inp(iald:linp),'"')
		iald=ido+1  ! could get better using difference between current and previous linp
		edited=.true.
		if(iald.lt.linp)goto 88
	endif !if(ido.ge.iald)  10940
	return
	!20140618 <1685> siirretty kohdasta <1648> --alkaa
90 write(6,*)'*interpreting:','"'//j_inpr(1:j_linpr)//'"'
	j_err=.true.
	return
 
 
end subroutine inputinterpret2 !subroutine inputinterpret2(inp,linp,edited)
 
subroutine ienddo()
	logical done
	!.and..not.j_bypa(j_niifs))then !if(j_inp(1:6).eq.';incl('.or.j_inp(1:6).eq.';goto(')then
	!	write(6,*)'ienddo ',j_ndo
	if(j_ndo.le.0)then
		write(6,*)'** ;enddo without ;do'
		j_err=.true.;return
	endif !if(j_ndo.le.0)  11003
	if(j_niifs.ne.j_niifsindo(j_ndo))then
 
		write(6,*)'** ;enddo cannot be before ;endif in ;if();then structure'
		write(6,*)'the ;do loop started at line ',j_dostart(j_ndo)
		write(6,*)'j_niifs ',j_niifs,' j_ndo ',j_ndo, 'j_niifsindo ',j_niifsindo
		!		write(6,*)j_niifsindo(j_ndo),j_niifs.ne.j_niifsindo(j_ndo)
		j_err=.true. ;return
 
	endif !if(j_niifs.ne.j_niifsindo(j_ndo))  11007
	if(.not.j_bypa(j_niifs))then
		!	write(6,*)'1ndo ä',j_iido(1,j_ndo),j_iido(3,j_ndo)
		j_iido(1,j_ndo)=j_iido(1,j_ndo)+j_iido(3,j_ndo)
		!	write(6,*)'ndo1again ',j_iido(1,j_ndo),j_iido(2,j_ndo)
 
		if(j_iido(1,j_ndo).gt.j_iido(2,j_ndo).and.j_iido(3,j_ndo).gt.0)then
 
			j_ndo=j_ndo-1
 
			!if(continues)goto 161
			done=.true.
			!		write(6,*)'donehere'
			return
		elseif(j_iido(1,j_ndo).lt.j_iido(2,j_ndo).and.j_iido(3,j_ndo).lt.0)then
 
			j_ndo=j_ndo-1
 
			!if(continues)goto 161
			done=.true.
			!		write(6,*)'donehtaasere'
			return
 
		else !if(j_iido(1,j_ndo).gt.j_iido(2,j_ndo))then
			j_v(j_iido(4,j_ndo))=j_iido(1,j_ndo)
			j_o( j_inciv(j_ninc))%i(6)=j_iido(5,j_ndo) !j_incline(j_ninc)=
 
			!	write(6,*)'ndo46', j_v(j_iido(4,j_ndo)),	j_o( j_inciv(j_ninc))%i(6)
			return
 
		endif !if(j_iido(1,j_ndo).gt.j_iido(2,j_ndo).and.j_iido(3,j_ndo).  11021
	else
		j_ndo=j_ndo-1
	endif !if(.not.j_bypa(j_niifs))  11016
end subroutine
 
subroutine getif(iszero)
	logical	iszero
	logical printline
 
 
	!	if(.not.j_bypa(j_niifs)) yes=j_val(j_inpr(5:ir-1))
	if(j_err)return
	! write(6,*)'yes',yes,'inp2 ',inp(ir+1:linp7)
	!	if(j_inpr(ir+1:j_linpr).eq.';then')then
	j_niifs=j_niifs+1
	j_bypa(j_niifs)=iszero.or.j_bypa(j_niifs-1)
	!bypass=bypa(niifs)
	j_ifdone(j_niifs)=.not.j_bypa(j_niifs)
 
	! write(6,*)'hoo,niifs ',niifs,bypass
	!write(6,*)'niifs',niifs,bypa(niifs),bypass
	!**
	!	if(inprint.ge.2)write(6,*) j_inp(1:j_linp),' :: ',yes.ne.0.
	if(iszero.and.inprint0.gt.0)write(6,*)'*following section is bypassed until ;else or ;endif'   !write
 
	return
	!	else !if(j_inp(ir+1:j_linp).eq.';then')then
 
 
end subroutine
 
subroutine ielseif()
	logical printline
	if(j_inp(8:8).ne.'(')then
		write(6,*)'**illegal ;elseif('
 
		j_err=.true.;return
		!return;
	endif !if(j_inp(8:8).ne.'(')  11079
	ir=j_nextrp(j_inpr,8,j_linpr)
	if(ir.gt.j_linpr)then
		write(6,*)'**illegal ;elseif('
		j_err=.true.;return
		!return
	end if !if(ir.gt.j_linpr)  11086
 
	if(j_inpr(ir+1:j_linpr).eq.';then')then
		!	write(6,*)'<457',j_inp(9:ir-1)
		yes=j_val(j_inpr(9:ir-1))
		if(j_err)return
		if(j_niifs.le.0)then
			write(6,*)'**;elseif( without ;if'
			j_err=.true.;return
			!return
		endif !if(j_niifs.le.0)  11096
		j_bypa(j_niifs)=yes.eq.0..or.j_bypa(j_niifs-1).or.j_ifdone(j_niifs)
		j_ifdone(j_niifs)=.not.j_bypa(j_niifs).or.j_ifdone(j_niifs)
		!**
		!	if(inprint.ge.2)write(6,*)j_inp(1:j_linp),' :: ',yes.ne.0.
		if(yes.eq.0.and.inprint0.gt.0)write(6,*)'*following section is bypassed until ;else or ;endif'  !write!
 
		return  !goto 1
	else !if(j_inp(ir+1:j_linp).eq.';then')then
		write(6,*)'**illegal ;elseif();then'
		j_err=.true. ;return
	endif !if(j_inpr(ir+1:j_linpr).eq.';then')  11092
 
 
end subroutine ielseif
 
subroutine ielse()
	logical printline
	! write(6,*)'tas,by',bypass,'niifs',niifs
	j_bypa(j_niifs)=j_bypa(j_niifs-1).or.j_ifdone(j_niifs)
 
	!	if(inprint.ge.2) write(6,*)j_inp(1:j_linp), '::',.not.j_bypa(j_niifs)
	if(j_bypa(j_niifs).and.inprint0.gt.0)write(6,*)'*following section until ;endif is bypassed '  !write!
	return  !goto 1
	!
	!else if(j_inpr(1:j_linpr).eq.';endif')then !if(j_inp(1:6).eq.';incl('.or.j_inp(1:6).eq.';goto(')then
	!write(6,*)'jooby',bypass
	j_niifs=j_niifs-1
	if(j_niifs.lt.0)then
		write(6,*)'**  ;endif without ;if();then'
		j_err=.true.;return
		!return
	endif !if(j_niifs.lt.0)  11128
 
	!	if(inprint.ge.2)write(6,*)'<123>',j_inp(1:j_linp)
	return
 
 
end subroutine
 
subroutine goto90() !return from an include file
	! 90 continue
	write(6,*)' '
	j_ndo=j_ndoinc(j_ninc)
	j_niifs=j_niifsinc(j_ninc)
	j_ninc=j_ninc-1
end subroutine
 
subroutine getreturn(done,doublesp,inpup)
 
	logical done
	logical doublesp
	character*(*):: inpup
	logical ifret
	!	write(6,*)'getret ',j_inpr(1:j_linpr),j_ninc
	done=.false.
	if(j_ninc.eq.1)return
	ifret=.false.
	if(j_linpr.gt.12)then
		if(j_inpr(1:4).eq.";if(".and.(j_inpr(j_linpr-7:j_linpr)==');return'.or. &
				j_inpr(j_linpr-7:j_linpr)==');Return')) then
			!	write(6,*)'<458',j_err,j_inp(5:max(5,j_linp-8))
			j_dapu = j_val(j_inpr(5:j_linpr-8))
			done=.true.
			if(j_err) return
 
			if(j_dapu.eq.j_0)return
			ifret=.true.
			write(6,*)j_inpr(1:j_linpr),' => ',j_inpr(j_linpr-6:j_linpr)
			if(j_inpr(j_linpr-5:j_linpr-5).eq.'R')then
				call goto90()
				return
			endif !if(j_inpr(j_linpr-5:j_linpr-5).eq.'R')  11169
 
		endif !if(j_inpr(1:4).eq.";if(".and.(j_inpr(j_linpr-7:j_linpr)=='  11159
	endif !if(j_linpr.gt.12)  11158
 
	if(j_inpr(1:j_linpr).eq.';return'.or.done.or.j_inpr(1:j_linpr).eq.';Return'.or.ifret)then
 
		if(.not.ifret)then
			if(doublesp)write(6,*)' '
			!		write(6,*)'% '//j_inpr(1:j_linpr)
			write(6,*)inpup//j_inpr(1:j_linpr)
		endif !if(.not.ifret)  11179
		if(j_inpara)then
			write(6,*)'*cannot ;return while in paragraph'
			j_err=.true.;return
		endif !if(j_inpara)  11184
		if(j_inpara2)then
			!		write(6,*)j_inpr(1:j_linpr), ' done ',done,' ifret ',ifret
			write(6,*)'*cannot ;return while in text or txt paragraph'
			j_err=.true.;return
		endif !if(j_inpara2)  11188
		!		write(6,*)' '
		!	write(6,*)'<73@j_ninc',j_ninc,'j_inciv',j_inciv,' ret ',j_increturn,' nulline ',j_incline
		!	write(6,*)'incret',j_ninc,j_increturn( j_ninc)
		done=.true.
 
		if(j_ninc2(j_ninc).gt.0)then
			!		if(j_increturn( j_ninc,j_ninc2).gt.0)then
			!	rewind(j_nul( j_ninc))
			j_o( j_inciv(j_ninc))%i(6)=j_increturn( j_ninc,j_ninc2(j_ninc))
			!	j_incline(j_ninc)=j_increturn( j_ninc)
			j_ninc2(j_ninc)=j_ninc2(j_ninc)-1
			!			j_increturn( j_ninc,j_ninc2)=0
			!		write(6,*)'zeroinc,j_ninc2 reduced',j_ninc,j_ninc2(j_ninc),'j_inpr(1:j_linpr).eq.;return',j_inpr(1:j_linpr).eq.';return'
			return
		endif !if(j_ninc2(j_ninc).gt.0)  11198
 
		call goto90()
		!		done=.true.
	endif !if(j_inpr(1:j_linpr).eq.';return'.or.done.or.j_inpr(1:j_li  11177
 
 
end subroutine getreturn
subroutine inputinterpret(inpr,linpr,inp,linp,contline) ! interpretin utf
	character*(*)inpr,inp
	integer linpr,linp
	logical contline
	linp=0
	do i0=1,linpr
		if(ichar(inpr(i0:i0)).le.32.and.nq.eq.0)cycle
 
		if(inpr(i0:i0).eq."'")then
			nq=mod(nq+1,2)
		endif !if(inpr(i0:i0).eq."'")  11223
		if(inpr(i0:i0).eq.'!'.and.nq.eq.0)exit
		linp=linp+1
 
 
		!	write(6,*)'<1239nu',nu
		if(nu.ne.5.and.nu.gt.0)then
			do jj=1,j_nutf8
				if(inpr(i0:i0).eq.j_utf8(jj))then
					inpr(i0:i0)=j_ocode(jj)
					!	any
					exit
				endif !if(inpr(i0:i0).eq.j_utf8(jj))  11233
			enddo !jj=1,j_nutf8  11232
		endif !if(nu.ne.5.and.nu.gt.0)  11231
 
		inp(linp:linp)=inpr(i0:i0)
 
	enddo !i0=1,linpr  11220
	!write(6,*) 'interpret,putting from inpr /',inpr(1:linpr)
	!write(6,*)'> to inp: ',inp(1:linp)
 
	! write(6,*)'cleaninp',inp(1:linp)
	if(nu.ne.5.and.nu.gt.0)then
 
		call j_fromutf8(inp(1:linp))
 
	endif !if(nu.ne.5.and.nu.gt.0)  11248
	!write(6,*)'<87 put inp toinpu1, inp ',inp(1:linp)
 
	return
end subroutine !subroutine inputinterpret(inpr,linpr,inp,linp,contline)
 
! logical function printline(inprint)
! printline=.true.
! !	inprint=j_v(j_ivprintinput)
! !	write(6,*)'printdo',j_printdo
! !	write(6,*)'inprint,promt',inprint,prompt
! if(inprint.le.2)then
! !		if(.not.j_printdo)then
! do jj=1,j_ndo
! !	write(6,*)'iido',j_iido(1,jj),j_iido(7,jj)
! if(j_iido(1,jj).eq.j_iido(7,jj))then
! printline=.false.
! return
! endif !if(j_iido(1,jj).eq.j_iido(7,jj))  10713
! enddo !jj=1,j_ndo  10711
! !			if(j_iido(1,jj).ne.j_iido(7,jj))printline=.false.
! !enddo !jj=1,j_ndo  10698
! !		endif !if(.not.j_printdo)  10698
 
 
! elseif(inprint.lt.1)then
! printline=.false.
! endif !if(inprint.le.2)  10709
 
! return
! end function !logical function printline()
 
subroutine debugi(tag)
	integer j,k
	character*(*)tag
	if(tag(1:1).eq.'!')return
	write(6,*)tag,'***********'
	write(6,*)'inpr /',j_inpr(1:max(1,j_linpr))
	write(6,*)'inp  /',j_inp(1:max(1,j_linp))
	write(6,*)'inpr  /',j_inpr(1:max(1,j_linpr))
	write(6,*)'input0 ',j_o(j_ivinput0)%i(0)
	do j=1,j_o(j_ivinput0)%i(0)
		!write(6,*)j,o(iob)%i(j),o(iob)%i(j+1)-1
		write(6,*)(j_o(j_ivinput0)%ch(k),k=j_o(j_ivinput0)%i(j),j_o(j_ivinput0)%i(j+1)-1 )
	end do !j=1,j_o(j_ivinput0)%i(0)  11293
	write(6,*)'input1 ',j_o(j_ivinput1)%i(0)
	do j=1,j_o(j_ivinput0)%i(0)
		!write(6,*)j,o(iob)%i(j),o(iob)%i(j+1)-1
		write(6,*)(j_o(j_ivinput1)%ch(k),k=j_o(j_ivinput1)%i(j),j_o(j_ivinput1)%i(j+1)-1 )
	end do !j=1,j_o(j_ivinput0)%i(0)  11298
	write(6,*)'input2 ',j_o(j_ivinput2)%i(0)
	do j=1,j_o(j_ivinput2)%i(0)
		!write(6,*)j,o(iob)%i(j),o(iob)%i(j+1)-1
		write(6,*)(j_o(j_ivinput2)%ch(k),k=j_o(j_ivinput2)%i(j),j_o(j_ivinput2)%i(j+1)-1 )
	end do !j=1,j_o(j_ivinput2)%i(0)  11303
 
end subroutine !subroutine debugi(tag)
 
subroutine j_incl(line) !  ;incl(  )  ;goto(
	!!Inpuf ;incl
	!!Inpuf ;goto
	!!Inpuf ;return
	!Section incl ;incl() lines from a file
	! Includes lines from a file or from a text object. Using the from->
	!option the include file can contain sections which start with adresses like
	! ;ad: \\
	!and end with \\
	!;return
	!endheader
	!Option
	! Args & 0|1 & Ch|Tx &  file name. Default: the same file is used as in the previous ;incl().
	!from & N|1 & Ch &gives the starting label for the inclusion, label is given without starting ';'
	! and ending ':'.
	!wait& N|0 & & Jlp22 waits until the include file can be opened. Useful in client server applications.
	! See chapter Jlp22 as a server.
	!endoption
	! Note  Include files can be nested up to 4 levels.
	!endnote
	! Note  In the current version of Jlp22, the file name and the adress can be without apostrophes ' ',
	! but the previous names with apostrophes are allowed.
	!endnote
	!Note It is possible to start reading the script from the same file. In that case ;return returns the reading of
	! the script after the ;incl- line.
	!endnote
	!Note ;goto(adr) and ;incl(from->adr) go to the same line in the include file but after ;goto the ;return-command
	! closes the include file but after ;incl() the ;return-command returns the control to the calling point.
	!endnote
	! Note  See Chapter Defining a text object with text function and using it in ;incl how to include
	! commands from a text object.
	!endnote
	! Note  When editing the include file with Notepad ++, it is reasonable to set the language as Fortran (free form).
	!endnote
	!Ex inpuincl Example of ;incl()
	!file=text()
	!** File start
	!i=1;
	!;goto(ad1)
	!** Never here
	!i=2;
	!;ad1:i=66;
	!**After ad1
	!;goto(ad2,ad3,2)  !select label from a label list
	!;ad2:
	!** After ad2
	!i=3;
	!;ad3:
	!** After ad3
	!i=4;
	!;ad4:
	!** After ad4
	!i=5;
	!;ad5:
	!** After ad5
	!i=6;
	!//
	!file;
	!;if(exist_f('file.txt'))delete_f('file.txt')
	!write('file.txt',$,file)
	!close('file.txt')
	! ;incl(file.txt)
	! ;incl(file.txt,from->ad2)
	!endex
	!Note The adress line can contain comment starting with '!'.
	!endnote
	!endsection
 
	!Section inpugoto ;goto()
	!Go to different adress in ;incl() file.
	!endheader
	!Option
	!Args&1&CHAR& The label from which the reading continues. With ;goto(adr1)
	! the adress line starts ;adr1:
	!endoption
	!Ex inpugotoex Example of ;goto() and ;incl()
	!gototxt=text()
	!!! Start jumping
	!;goto(ad2)
	!;ad1:
	!!!Greetings from ad1
	!;return
	!;ad2:
	!!!Greetings from ad2
	!;goto(ad1)
	!//
	!print(gototxt)
	!if(exist_f('goto.txt'))delete_f('goto.txt')
	!write('goto.txt',gototxt)
	!close('goto.txt')
	!print('goto.txt')
	!;incl(goto.txt)
	!;incl(goto.txt,from->ad1)
	!delete_f('goto.txt')
	!endex
	!Note In the previous versions the adress had to be withing apostrophes ' ', but now tis is not necessatry
	! even if it is possible.
	!endnote
	!endsection
 
 
 
 
 
 
	character*(*)line
	logical::p=.false.
	logical wait
	logical::contold
	! integer::ifiold
	! save ifiold
	!	p=j_v(j_ivdebug).ne.0
	write(6,*)' '
	goto 100
	99	write(6,*)'*illegal: ',line
	j_err=.true.;return
 
100	le=len(line)
	if(line(le:le).ne.')')goto 99
 
 
	ifi=99999
	!	write(6,*)'<84incl84',line(1:le)
 
 
	!	write(6,*)'<888>le',le,line(1:le)
 
	if(p)write(6,*)'tas ollaan ',line
 
	ifrom=index(line,'from->')
	iprint=index(line,'print->')
 
 
	if(ifrom.gt.0)then
		if(le.lt.ifrom+6)goto 99
		if(line(ifrom+6:ifrom+6).eq."'")then
			ifrom1=ifrom+7
			ifrom2=ifrom+index(line(ifrom1:),"'")+5
 
			if(ifrom2.lt.ifrom1)goto 99
 
		else !if(line(ifrom+6:ifrom+6).eq."'")then
			ifrom1=ifrom+6
			ifrom2=ifrom+index(line(ifrom1:),")")+4
 
			if(ifrom2.lt.ifrom1)goto 99
		endif !if(line(ifrom+6:ifrom+6).eq."'")  11445
	endif !if(ifrom.gt.0)  11443
 
	if(index(line,'form->').gt.0)then
		write(6,*)'form-> in not option in ;incl you probably mean from->'// &
			' but you must anyhow correct it'
		j_err=.true.
		return
	endif !if(index(line,'form->').gt.0)  11459
 
	wait=index(line,'wait->').gt.0
	ileft=index(line,'(')
	ipil=index(line,',')
	iright=ipil
	if(iright.eq.0)iright=index(line,')')
	if(iright.le.0)then
		write(6,*)'illegal syntax ',line
		j_err=.true.
		return
	endif !if(iright.le.0)  11471
	!write(6,*)'<333>',ileft,iright,lafi,line(ileft+1:lafi-1)
 
	!get file
	if(ipil.gt.0.or..not.ifrom.gt.0)then  !file name given
 
		j_filename=line(ileft+1:iright-1)
		leno=len_trim(j_filename)
		ifi=j_object(j_filename(1:leno))
		if(p)write(6,*)'<78>',leno,j_filename(1:leno),'ifi ',ifi
 
		if(ifi.gt.0)then
			!if(p)write(6,*)'<767',j_otype(ifi)
			if(j_otype(ifi).eq.j_iptxt)goto 700
			if(j_otype(ifi).ne.j_ipchar)then
				write(6,*)'illegal argument in ;incl'
				j_err=.true.;return
			endif !if(j_otype(ifi).ne.j_ipchar)  11489
			!		write(6,*)'<77>',j_o(ifi)%i
			if(j_o(ifi)%i(3).ne.0)ifi=j_o(ifi)%i(3)    !charcter variable
 
 
			!5 number of lines
			!6 last modified
			!7 lines allocated
 
		else !if(ifi.gt.0)then
			!		if(line(ileft+1:ileft+1).eq."'".or.line(ileft+1:ileft+1).eq.'~')then
			if(p)write(6,*)'6666'
			ifi=j_defchar(0,j_filename(1:leno))
			if(j_err)return
 
		endif !if(ifi.gt.0)  11486
		!	p=.true.
		if(p)then
			write(6,*)'<7e77e',ifi,j_otype(ifi)
			call j_getname(ifi)
			write(6,*)j_oname(1:j_loname)
			write(6,*)'ibef ',j_o(ifi)%i
		endif !if(p)  11509
 
		call j_readtext(ifi)  !does not read if not needed
		if(j_err)return
		!	if(p)write(6,*)'hshsh'
		!remove spaces before ; and /
		! if(p)write(6,*)'<7e88e',ifi,j_otype(ifi)
		!		write(6,*)'i',j_o(ifi)%i
		! write(6,*)'i2',j_o(ifi)%i2
		! write(6,*)'txt',size(j_o(ifi)%txt)
 
		do il=1,j_o(ifi)%i(5)
			!	write(6,*)ifi,j_o(ifi)%txt
			!	write(6,*)j_o(ifi)%i2
			!le=j_lentrim(j_o(ifi)%txt(il))
			!	write(6,*)il,j_o(ifi)%txt(il)(1:j_o(ifi)%i2(il))
			call j_cleanstart(j_o(ifi)%txt(il)(1:j_o(ifi)%i2(il)),le)
			j_o(ifi)%i2(il)=le
			!			write(6,*)'aft',il,j_o(ifi)%txt(il)(1:j_o(ifi)%i2(il))
 
		enddo !il=1,j_o(ifi)%i(5)  11525
 
		if(j_err)return
		call j_getname(ifi)
		!	write(6,*)'<33 ',j_oname(1:j_loname),' type ',j_otype(ifi)
		!	write(6,*)'<445>i ',j_o(ifi)%i
		if(j_o(ifi)%txt( j_o(ifi)%i(5))(1:7).ne.';return')then
			j_o(ifi)%i(5)=j_o(ifi)%i(5)+1
			j_o(ifi)%txt( j_o(ifi)%i(5))=';return'
			j_o(ifi)%i2(j_o(ifi)%i(5))=7
		endif !if(j_o(ifi)%txt( j_o(ifi)%i(5))(1:7).ne.';return')  11540
	else !if(ipil.gt.0.or..not.ifrom.gt.0)then
		if(j_inciv(j_ninc+1).eq.0)then
			write(6,*)';incl without file, but there is no previous file'
			j_err=.true.;return
		endif !if(j_inciv(j_ninc+1).eq.0)  11546
 
	endif !if(ipil.gt.0.or..not.ifrom.gt.0)  11479
 
700	continue
!	write(6,*)'<55% ',j_ninc,j_inciv,'  ifi ',ifi
!	if(ifi.eq.99999)then
!		write(6,*)'include file not given
!	endif
	if(ifi.eq.99999)then
 
		write(6,*)'include file not given',j_ninc,j_inciv(j_ninc:j_ninc+1)
		!ifi=j_inciv(j_ninc+1)
 
		j_err=.true.
		return
 
	endif !if(ifi.eq.99999)  11558
	iiv=j_inciv(j_ninc)
	if(iiv.eq.ifi)then
 
 
		!it is possible to use include shortcut which is in the include file
		do ic=1,j_ninc2(j_ninc)
			if(j_increturn(j_ninc,ic).eq.j_o(iiv)%i(6))then
				if(j_ivgo_on.eq.0)j_ivgo_on=j_object('Go_on')
				if(j_ivgo_on.ne.0)j_dapu=j_v(j_ivgo_on)
				if(j_dapu.eq.j_0)then
					call j_getname(ifi)
					write(6,*)'perhaps an eternal loop implied in file ',j_oname(1:j_loname), ' on lines ',&
						j_increturn(j_ninc,1:j_ninc2(j_ninc))
					write(6,*)'if you want anyhow go on define Go_on=1 and restart'
					j_err=.true.
 
				else
					exit
				endif !if(j_dapu.eq.j_0)  11576
 
			endif !if(j_increturn(j_ninc,ic).eq.j_o(iiv)%i(6))  11573
 
		enddo !ic=1,j_ninc2(j_ninc)  11572
		if(j_ninc2(j_ninc).ge.j_mxinc2)then
			call j_getname(ifi)
			write(6,*)'maximum number of jumps j_mxinc2=', j_mxinc2, ' reached in an include file ',j_oname(1:j_loname)
			j_err=.true. ;return
		endif !if(j_ninc2(j_ninc).ge.j_mxinc2)  11590
		j_ninc2(j_ninc)=j_ninc2(j_ninc)+1
		!		write(6,*)'newinc2',j_ninc,j_ninc2(j_ninc),' line ',line
 
 
		j_increturn( j_ninc,j_ninc2(j_ninc))=j_o(iiv)%i(6)  !j_incline(j_ninc)
 
		!	write(6,*)'putinc',j_ninc, j_ninc2(j_ninc),j_increturn( j_ninc,j_ninc2(j_ninc)),' line ',line
		!	rewind(nu)
		!	j_incline(j_ninc)=0
		contold=.true.
	else !if(iiv.eq.ifi)then
		if(j_ninc.ge.j_mxinc)then
			write(6,*)'maximum nesting of ,incl ',j_mxinc,' reached'
			j_err=.true.
			return
		endif !if(j_ninc.ge.j_mxinc)  11606
		j_ninc=j_ninc+1
 
		j_ninc2(j_ninc)=0
		j_inciv(j_ninc)=ifi
		j_o(ifi)%i(6)=0   !j_incline(j_ninc)=0
		!	j_increturn(j_ninc,j_ninc2)=0
		!	write(6,*)'zeroinchere inc',j_ninc,j_ninc2(j_ninc)
		contold=.false.
	endif !if(iiv.eq.ifi)  11568
 
 
 
	if(p)write(6,*)'<273 nul0 ',j_ninc
	if(ifrom.gt.0)call findad()
	!write(6,*)'<z55znul0,j_err',nul0,j_err,j_tempchar(1:20)
	if(j_err)return !findad closes nu
	!	write(6,*)j_o(ifi)%txt(ili)(1:j_o(ifi)%i2(ili))
 
	if(contold)return
 
 
	j_ndoinc(j_ninc)=j_ndo
	j_niifsinc(j_ninc)=j_niifs  !
	j_inciv(j_ninc)=ifi
	!	j_ivbuf(nul0)=0
	!	write(6,*)' '
 
	return
 
	contains
 
	subroutine findad()
		!	170 	read(nu,'(a)',end=172,err=99)j_tempchar2
 
		do ili=1,j_o(ifi)%i(5)
			!	write(6,*)'ili ',ili,ifrom1,ifrom2,j_o(ifi)%txt(ili)(1:60)
 
			if(j_o(ifi)%txt(ili)(1:1).ne.';')cycle
 
 
			!		call j_clean(j_tempchar2,lin)
 
			if(line(ifrom1:ifrom2)//':'.eq.j_o(ifi)%txt(ili)(2:ifrom2-ifrom1+3))then
				!	write(6,*)'<found:,j_ninc',j_ninc,ili,j_o(ifi)%txt(ili)(1: j_o(ifi)%i2(ili))
				j_o(j_inciv(j_ninc))%i(6)=ili  !j_incline(j_ninc)=ili
				write(6,*)'%',j_o(ifi)%txt(ili)
				return
			endif !if(line(ifrom1:ifrom2)//':'.eq.j_o(ifi)%txt(ili)(2:ifrom2-  11653
			cycle
		enddo !ili=1,j_o(ifi)%i(5)  11645
172 	write(6,*)'label ;'//line(ifrom1:ifrom2)//': not found'
		!call j_closeunit(nu)
		j_err=.true.
		return
 
 
 
	end subroutine !subroutine findad()
 
 
 
	!endcd kommentoitucd ..
end subroutine j_incl !subroutine j_incl(line)
 
subroutine j_inpugoto(doublesp,inpup)  !;goto
	logical doublesp
	character*(*) ::inpup
	!	170 	read(nu,'(a)',end=172,err=99)j_tempchar2
 
 
	if(j_ninc.le.1)then
		write(6,*)';goto not legal at command level'
		j_err=.true.
		return
	endif !if(j_ninc.le.1)  11681
	!nu=j_nul(j_ninc)
	!rewind(nu)
	!j_incline(j_ninc)=0
	if(j_inpr(7:7).eq."'".or.j_inpr(7:7).eq."~")then
		ifrom1=8
		ifrom2=j_linpr-2
	else
		ifrom1=7
		ifrom2=j_linpr-1
	endif !if(j_inpr(7:7).eq."'".or.j_inpr(7:7).eq."~")  11689
	ihere=0
	ipil=j_nextlim(j_inpr,ifrom1,ifrom2,',')
	!	write(6,*)j_inpr(1:j_linpr)
	!	write(6,*)'ifrom1,ifrom2,ipil',ifrom1,ifrom2,ipil
	if(ipil.lt.ifrom2)then
		ihere=1
		ipre=j_prevlim(j_inpr,ifrom2,',')
		ival=j_val(j_inpr(ipre+1:ifrom2))
		!	write(6,*)'<445 ',ipre,ival,ipil,j_inpr(ipre+1:ifrom2)
		if(j_err)return
		if(ival.le.0)then
			write(6,*)'illegal value ',ival,' obtained from ',j_inpr(ipre:ifrom2+1)
			j_err=.true.  ;return
		endif !if(ival.le.0)  11706
 
		ic=1
		ipil1=ifrom1
		do while(ipil.le.ipre)
			!			write(6,*)'ic,ival,ipil',ic,ival,ipil
			if(ic.eq.ival)then
				ifrom1=ipil1+1
				ifrom2=ipil-1
			endif !if(ic.eq.ival)  11715
			ipil1=ipil
			ipil=j_nextlim(j_inpr,ipil+1,ipre,',')
			!	write(6,*)'ipilhere ',ipil,ipre
			if(ipil.gt.ipre)then
 
			endif !if(ipil.gt.ipre)  11722
			ic=ic+1
 
			!			write(6,*)'ic ',ic
		enddo !while(ipil.le.ipre)  11713
	endif !if(ipil.lt.ifrom2)  11700
 
	if(ival.ge.ic.and.ihere.ne.0)then
		write(6,*)'there are not ',ival,' addresses in ',j_inpr(ifrom1:ifrom2),'ic',ic
		j_err=.true.;return
	endif !if(ival.ge.ic.and.ihere.ne.0)  11731
 
	iiv=j_inciv( j_ninc)
 
	liloop:	do ili=1,j_o(iiv)%i(5)
			!	write(6,*)'ili ',ili,ifrom1,ifrom2,j_o(ifi)%txt(ili)(1:60)
			! or ;goto(ad1,ad2,index)

		if(j_o(iiv)%txt(ili)(1:1).ne.';')cycle
 
		!		call j_clean(j_tempchar2,lin)
		j_txtp=>j_o(iiv)%txt(ili)
		!similarly as in findad
		!		if(j_inpr(ifrom1:ifrom2)//':'.eq.j_o(ifi)%txt(ili)(2:ifrom2-ifrom1+3))then
		if(j_inpr(ifrom1:ifrom2)//':'.eq.j_txtp(2:ifrom2-ifrom1+3))then
			!	write(6,*)'<found:,j_ninc',j_ninc,ili,j_o(ifi)%txt(ili)(1: j_o(ifi)%i2(ili))
			if(doublesp)write(6,*)' '
			write(6,*)inpup,j_txtp(2:ifrom2-ifrom1+3)
			!		write(6,*)'% ',j_txtp(2:ifrom2-ifrom1+3)
			j_o(j_inciv(j_ninc))%i(6)=ili  !j_incj_inpr(j_ninc)=ili
			return
		endif !if(j_inpr(ifrom1:ifrom2)//':'.eq.j_txtp(2:ifrom2-ifrom1+3)  11748
 
	enddo liloop !oop:	do ili=1,j_o(iiv)%i(5)  11738
172 	write(6,*)'adress ;'//j_inpr(ifrom1:ifrom2)//': not found'
		!call j_closeunit(nu)
	j_err=.true.
	return
end subroutine !subroutine findad()
 
subroutine j_bypassinc(nu)
	ivinc=j_inciv(j_ninc)
	il=j_o( ivinc)%i(6)
	nl=j_o( ivinc)%i(5)
	!	write(6,*)'il,nu ',il,nu,nl
	do while(j_o(nu)%txt(il)(1:1).ne.'/')
 
		il=il+1
		!	write(6,*)'ilgg ',il
		if(il.gt.nl)goto 99
		le=len_trim(j_o(nu)%txt(il))
		!			write(6,*)j_o(nu(id))%txt(j_o( j_inciv(j_ninc))%i(6))
		write(6,*)'bypass>',j_o(nu)%txt(il)(1:le)
	enddo !while(j_o(nu)%txt(il)(1:1).ne.'/')  11769
	!	write(6,*)'bypass>/'
99	j_o( ivinc)%i(6)=il
!	write(6,*)'last',il

end subroutine
 
!end subroutine j_getinput !subroutine j_getinput(prompt,inprint,nul0t)
 
 
 
 
!20150812(arg1<->arg2) oli: subroutine defconst(varname,ivin,text) !compute the numeric value of a text string
subroutine j_defconst(ivin,varname,text) !compute the numeric value of a text string
	! and to put it into real variable object with name varname//name_of_ivin
 
	!module vmod
	!end module vmod
 
 
	character (len=*), intent(in):: text,varname
	integer, intent(in):: ivin
 
 
	if(j_err)return
	!20150812(arg1<->arg2) oli: 	call getv(varname,ivin,ipreal,iout)
	iout=j_getobject(ivin,varname,j_ipreal)
	!20140522 virheenkäsittely
	if(j_err) return
	call dotrans(j_ivvalc,1)
	!	write(*)'{{',7
	j_v(iout)=j_v(j_ivresult)
	return
end subroutine j_defconst !subroutine j_defconst(ivin,varname,text)
 
 
double precision function j_val(text) !subroutine compute the numeric value of a text string, used in getinput
 
	character (len=*), intent(in):: text
	logical p
	le=len(text)
	!	p=text.eq.'3-2'
	!	write(6,*)'val in j_val',text
	if(le.le.0)then
		write(6,*)'*j* j-val called with nul string'
		j_err=.true.
		return
		! elseif(text.eq.'1')then
		! j_val=j_1
		! !	write(6,*)j_1
		! return
	endif !if(le.le.0)  11820
	iv=j_isconst(text)
	!	write(6,*)'iv ',iv
	if(iv.gt.0)then
		j_val=j_v(iv)
		return
	endif !if(iv.gt.0)  11831
	iv=j_object(text)
	!write(6,*)'iv2 ',iv
	!	i2=min(2,le)
	if(iv.gt.0)then
		if(j_otype(iv).eq.j_ipreal)then
			j_val=j_v(iv)
		else !if(j_otype(iv).eq.j_ipreal)then
			write(6,*)text,' is not REAL but ',j_objecttypes(j_otype(iv))
			j_err=.true.
		endif !if(j_otype(iv).eq.j_ipreal)  11839
		return
		! elseif(iv.le.0.and.(text(1:1).eq.'-'.or.text(1:1).eq.'+').and.&
		! (text(i2:i2).eq.'.'.or.(text(i2:i2).ge.'0'.and.text(i2:i2).le.'9')))then
		! iv=j_isconst(text)
		! if(iv.gt.0)then
		! j_val=j_v(iv)
		! return
		! endif !if(iv.gt.0)  10514
		! elseif(iv.le.0.and.(text(1:1).eq.'.'.or.(text(1:1).ge.'0'.and.text(1:1).le.'9')))then !if(iv.gt.0)then
		! iv=j_isconst(text)
		! if(iv.gt.0)then
		! j_val=j_v(iv)
		! return
		! endif !if(iv.gt.0)  10520
	endif !if(iv.gt.0)  11838
	!	write(6,*)'inter'
	call j_parser(text,j_ivvalc)
 
	if(j_err)return
	call dotrans(j_ivvalc,1)
	! write(6,*)'{{',8
	!write(6,*)'<4444:',j_v(j_ivresult)
	j_val=j_v(j_ivresult)
	return
end function j_val !double precision function j_val(text)
 
integer function j_tex2iv(text,isplus) !subroutine compute the numeric value of a text string, used in getinput
 
	character (len=*), intent(in):: text
	integer,intent(out)::isplus
	!	write(6,*)'tex2 ',text
	le=len(text)
	if(le.le.0)then
		write(6,*)'*j* j_tex2iv called with null string'
		j_err=.true.
		return
	endif !if(le.le.0)  11877
	i1=1
	if(text(1:1).eq.'-')then
		i1=2
		isplus=0
		if(i2.eq.le)then
			write(6,*)' - sign is not number'
			j_err=.true.
			return
		endif !if(i2.eq.le)  11886
	elseif(text(1:1).eq.'+')then
		i1=2
		isplus=1
	else
		isplus=1
	endif !if(text(1:1).eq.'-')  11883
	if(text(i1:i1).eq."'")then
		j_tex2iv=j_defchar(0,text(i1:le))
		return
	endif !if(text(i1:i1).eq."'")  11897
	j_tex2iv=j_object(text(i1:le))
	if(j_tex2iv.gt.0)return
 
	if(text(i1:i1).eq.'(')then
 
		j_dapu=j_val(text(i1+1:le-1))
		if(j_err)return
		j_tex2iv=j_num2iv(j_dapu)
		!write(6,*)'here ',text(i1+1:le-1),j_dapu,j_tex2iv
		return
	endif !if(text(i1:i1).eq.'(')  11904
 
	j_tex2iv=j_getobject(0,text(i1:le),j_ipreal,silent=.true.)
	if(.not.j_err)return
	if(j_err)j_err=.false.
	!write(6,*)'<444>',text(i1:le)
	j_dapu=j_val(text(i1:le))
	if(j_err)return
	j_tex2iv=j_num2iv(j_dapu)
	!write(6,*)'here ',text(i1+1:le-1),j_dapu,j_tex2iv
	return
 
end function j_tex2iv !
 
double precision function j_iv2val(iv)
	integer,intent(in)::iv
	if(j_otype(iv).eq.j_ipreal)then
		j_iv2val=j_v(iv)
	elseif(j_otype(iv).eq.j_ipchar)then
		call j_getchar(iv,j_tempchar3,le)
		if(le.le.0)then
			write(6,*)'* j_iv2val called with empty char'
			j_err=.true.
		endif !if(le.le.0)  11931
		j_iv2val=j_val(j_tempchar3(1:le))
	else
		j_iv2val=j_inf
	endif !if(j_otype(iv).eq.j_ipreal)  11927
	return
end function j_iv2val !real function j_val(text)
 
!tassa
!!Inpuf ;do
!!Inpuf ;enddo
!Generates new input records and replaces text with other text
!using " "  to generate numbers, @list to generate lists of object names
! and @list(elem) to pick the names of the elements of a list, or
!;sum() to generate sums and ;dif() to generate differences.
subroutine j_getdos(inp,linp,iargs,nargs)!  ;do !enddo
	!Section inpudo ;do() input records in a loop.
	!endheader
	!Option
	!Args&3|4&Var,Num..&Arguments are: iteration index, starting limit,
	! final limit and step. First argument must be a variable name and others
	!can be REAL variables or numeric constants.
	!endoption
	!Ex inpudoex Examples of ;do()
	!;do(i,1,2)
	!x"i"="i"*10
	!print('Greetings from iteration "i"')
	!;enddo
	!print(x1,x2)
	!varlist=list(x0,y0,
	!;do(i,1,3)
	!x"i",y"i",
	!;enddo
	!x4,y4);
	!endex
 
	!Listing
	!!After dropping out extra text about the processing we get:
	!<print('Greetings from iteration 1')
	!'Greetings from iteration 1'
	!<print('Greetings from iteration 2')
	!'Greetings from iteration 2'
	!sit< print(x1,x2)
	!<print(x1,x2)
	!x1=   10.000000000000000
	!x2=   20.000000000000000
	!endlisting
	!endsection
 
	!2    2   75 5202 5001    4    3   76    4 5001   65
	!2    2   75 5202   65    0
 
 
	integer,intent(in)::linp
	integer,intent(out),dimension(:) :: iargs
	character(len=linp),intent(in)::inp
	integer,intent(out)::nargs
	!function j_nextword(inp,ial,lop,limit)
	ial=1
 
	nargs=0
	il=j_nextlim2(inp,ial,linp,'(')
	!write(6,*)'ial ',ial,linp,il
 
 
 
	if(il.gt.linp.or.inp(1:4).ne.';do(')then
		write(6,*)'illegal ',inp(1:linp)
		!	write(6,*)'il',il,inp(1:il)
		j_err=.true.
		return
	endif !if(il.gt.linp.or.inp(1:4).ne.';do(')  12000
	!	ial=il+1
	call j_parser('list2'//inp(il:linp),j_ivcursori)
	if(j_err)return
	!	write(6,'(20i5)')j_o(j_ivcursor)%i(0:16)
	call dotrans(j_ivcursori,1)
	!	write(6,*)'{{',9
	if(j_err)return
	nargs=j_o(j_ivresult)%i(1)
	!write(6,*)'<22>',nargs,inp(il:linp)
	if(nargs.lt.3.or.nargs.gt.4)then
		j_err=.true.
		write(6,*)'illegal number of arguments ',nargs
		return
	endif !if(nargs.lt.3.or.nargs.gt.4)  12015
	iargs(1)=j_o(j_ivresult)%i2(1)
	if(iargs(1).gt.j_namedv)then
		write(6,*)'**first argument must be variable'
		j_err=.true. ;return
	endif !if(iargs(1).gt.j_namedv)  12021
	do i=2,nargs
		iargs(i)=j_v(j_o(j_ivresult)%i2(i))
		if(dble(iargs(i)).ne.j_v(j_o(j_ivresult)%i2(i)))then
			write(6,*)'argument ',i,' not an integer'
			j_err=.true. ;return
		endif !if(dble(iargs(i)).ne.j_v(j_o(j_ivresult)%i2(i)))  12027
	enddo !i=2,nargs  12025
 
 
	if(j_ndo.ge.j_mxndo)then
		write(6,*)j_ndo,j_mxndo
		write(6,*)'**too many ;do -loops'
		j_err=.true.
		j_ndo=0
		return
	endif !if(j_ndo.ge.j_mxndo)  12034
 
 
	17 ilow=iargs(2); iup=iargs(3)
	if(nargs.le.3)then
		istep=1
	else !if(nargs.le.3)then
		istep=iargs(4)
	endif !if(nargs.le.3)  12044
 
	if(istep.eq.0)then
		write(6,*)'**illegal ;do loop with low,up,step:',ilow,iup,istep
		j_err=.true.
 
		return
	endif !if(istep.eq.0)  12050
 
	nstep=(iup-ilow+istep)/istep
 
	!write(6,*)'ilow,iup,istep',ilow,iup,istep,' nstep ',nstep
	!write(6,*)'<555',nstep
	if(nstep.le.0)then
		!5 number of lines
		!6 lines used
		write(6,*)'negative count ',nstep,' obtained from ;do:'
		call j_getname(iargs(1))
		if(nargs.eq.3)then
			write(6,*)';do(',j_oname(1:j_loname),',',ilow,',',iup,')'
		else
			write(6,*)';do(',j_oname(1:j_loname),',',ilow,',',iup,',',istep,')'
		endif !if(nargs.eq.3)  12066
		j_err=.true.
		;return
		ifi=j_inciv(j_ninc)  !incl file
		!j_o(ifi)%i(5) number of lines
		nde=0
		!	write(6,*)'<77 ',j_o(ifi)%i(6),j_o(ifi)%i(5),j_o(ifi)%txt(j_o(ifi)%i(6))
		do ili=j_o(ifi)%i(6)+1,j_o(ifi)%i(5)
			!	write(6,*)'<771',ili,j_o(ifi)%txt(ili)
			if(j_o(ifi)%txt(ili)(1:4).eq.';do(')then
				nde=nde+1
			elseif(j_o(ifi)%txt(ili)(1:6).eq.';enddo'.or.j_o(ifi)%txt(ili)(1:5).eq.';endo')then !if(j_o(ifi)%txt(ili)(1:4).eq.';do(')then
				if(nde.eq.0)exit
				nde=nde-1
			endif !if(j_o(ifi)%txt(ili)(1:4).eq.';do(')  12079
		enddo !ili=j_o(ifi)%i(6)+1,j_o(ifi)%i(5)  12077
		if(nde.gt.0)then
			write(6,*)';do starting at line ',j_o(ifi)%i(6)+1,' never ends'
			j_err=.true.;return
		endif !if(nde.gt.0)  12086
		j_o(ifi)%i(6)=ili
 
 
		! if(j_ndo.eq.0)j_icurl(j_ninc)=istart+1
		! 567		call j_getline(j_ivbuf(j_ninc),j_icurl(j_ninc),j_inp(1:j_lline),j_linp)
		! !write(6,*)'<77puskurista rivi j_icurl(j_ninc)',j_inp(1:j_linp)
		! if(j_err)return
		! ialb2=4
		! ndotemp=1
		! if(j_inp(1:3).eq.';do'.and.j_inp(4:4).eq.'(')ndotemp=ndotemp+1
		! if(j_inp(1:6).eq.';enddo')ndotemp=ndotemp -1
		! j_icurl(j_ninc)=j_icurl(j_ninc)+1
		! if(ndotemp.gt.0)goto 567
		!	write(6,*)'getdosreturn ',inp(1:30)
		return
	endif !if(nstep.le.0)  12061
	do jdo=1,j_ndo
		call j_getname(iargs(1))
		if(j_iido(4,jdo).eq.iargs(1))then
			write(6,*)';do(  ',jdo,' had the same index variable ',j_oname(1:j_loname),' as current ;do( ',j_ndo+1
			j_err=.true.
		endif !if(j_iido(4,jdo).eq.iargs(1))  12108
	enddo !jdo=1,j_ndo  12106
	if(j_err)return
	j_ndo=j_ndo+1
 
	iiv=j_inciv(j_ninc)
	j_dostart(j_ndo)=j_o(iiv)%i(6)
 
	j_niifsindo(j_ndo)=j_niifs
	!	write(6,*)'<777 j_ndo,',j_ndo,' j_niifs ',j_niifs
	j_iido(1,j_ndo)=ilow
	!write(6,*)'<558> j_iido(1,j_ndo)',ilow
	j_iido(2,j_ndo)=iup  !ilow+(nstep-1)*istep !up
	j_iido(3,j_ndo)=istep
	j_iido(4,j_ndo)=iargs(1)  ! j_o(iob)%i(io+2)
	j_v(iargs(1))=ilow
	j_iido(5,j_ndo)=j_o( j_inciv(j_ninc))%i(6)   !j_incline(j_ninc)
	j_iido(6,j_ndo)=j_ninc
	j_iido(7,j_ndo)=ilow !  iido(1 is updated, thsi stores the intial value
 
	!	if(j_ndo.eq.1)j_printdo=j_linkoption(iob,io,j_mprint).gt.0
 
 
	! if(j_ndo.eq.1)then
	! j_icurl(j_ninc)=istart+1
	! endif !if(j_ndo.eq.1)then
	!	j_reacom(j_ninc)=.false.
	!	write(6,*)'<337>paluu getdos ',j_iido(1:7,j_ndo)
 
	900 continue
!	call j_clearoption(iob,i)  ! subroutine

end subroutine j_getdos !subroutine j_getdos(inp,linp,iargs,nargs)
 
! subroutine j_getdot(inp,linp)
! character*(*),intent(inout)::inp
! integer,intent(inout)::linp
! integer,dimension(2)::ivar,ilen,le
! character(len=j_lenobject),dimension(2) :: name
 
! ial00=index(inp,';dot(')
! ial=ial00+4
! ipil=j_nextlim2(inp,ial,linp,',')
! do kier=1,2
 
! if(ipil.gt.linp)goto 900
! ivar(kier)=j_object(inp(ial+1,ipil-1))
! if(ivar(kier).le.0)then
!write(6,*)';dot '//inp(ial+1,ipil-1)//' is not an object'
! goto 901
! endif
! if(j_otype(ivar(kier)).ne.j_ipmatrix.or.j_otype(ivar(kier)).ne.j_iplist)then
! call j_printname(';dot: ',ivar(kier),' is not LIST or MATRIX')
! goto 901
! elseif(j_otype(kier).eq.j_ipmatrix)then
! ilen(kier)=j_o(ivar(kier))%i(3)
! else
! ilen(kier)=j_o(ivar(kier))%i(0)
! endif
! ial=ipil+1
! if(kier.eq.1)ipil=j_nextlim2(inp,ial,linp,')')
! enddo
! if(ilen(1).ne.ilen(2))then
!write(6,*)';dot:first argument has length ',ilen(1), &
! ' but second argument has length ',ilen(2)
! goto 901
! endif
 
! do i=1,ilen(1)
! do iar=1,2
! if(j_otype(ivar(1)).eq.j_iplist)then
 
! iv=j_o(ivar(iar))%i(i)
 
! call j_getline(j_ivnames,iv,name(iar),le)
! else
! j_buf(36:)=j_chr10(j_value)
 
! endif
! enddo
! enddo
 
 
 
 
! return
! 900 write(6,*)'illegal ;dot '
! 901 continue
! j_err=.true.
! return
! end subroutine
!!!Inpuf ;sum
!!!Inpuf ;dif
subroutine j_getsum(inp,linp,plus)   ! ;sum  ;dif
	!Section inpusum ;sum() sums into input
	!Jlp22 an generate text of form part1+part2+...partn into input line using
	! input programming function ;isum(). The syntax of the function is as follows:\\
	! ;sum(i,low,up,step)(text)\\
	!or \\
	!;sum(i,low,up)\\
	!Arguments low, up and step must be integers (actually from nonintger values, the
	! integer part is used) or REAL variables. Thus te valuse cannot be obtained
	! from arithmetic operations. Sum is useful at least in problem() function.
	!endheader
	!Ex inpusumex Example of ;sum()
	!prob=problem()
	!;sum(i,1,5)(a"i"*x"i")==max
	!;sum(i,1,3)(a"i"*x"i")<8
	!/
	!endex
	!Note ;dif() works similarly for minus
	!endnote
	!endsection
 
	!Section inpudif ;dif() differences into input
	!Jlp22 can generate text of form part1-part2-...partn into input line using
	! input programming function ;dif(). The syntax of the function is as follows:\\
	! ;dif(i,low,up,step)(text)\\
	!or \\
	!;dif(i,low,up)\\
	!Arguments low, up and step must be integers (actually from nonintger values, the
	! integer part is used) or REAL variables. Thus te valuse cannot be obtained
	! from arithmetic operations. ;dif() is useful at least in problem() function.
	!endheader
 
	!Note ;sum()() works similarly for plus. See ;sum() for examples.
	!endnote
	!endsection
	!!Inpuf ;where
 
	!Section inpuwhere ;where the current line in ;incl -files
	!endsection
 
	!Section inpupause ;pause in script processing
	!Including input from an include file can be interrupted using an input programming
	! command ;pause promt or the Jlp22 function pause('<prompt>'). In both cases
	! the user can give Jlp22 commands, e.g., print objects, change the value of Printdebug etc.
	! The difference is that  pause('<prompt>') goes first through the interpreted and the interptreted
	! code is transmitted to the Jlp22 function driver. In the ;pause- pause it is possible to
	! use input programming commands while in pause()- pause it is not possible. In both cases, when
	!an error occurs, the control remains at the pause prompt. If the user is pressing
	! <return> Jlp22 continues in the include file. If pause() is part of a transformation object,
	! pressing <return>, the function driver continues in the transformation object.
	!If the user gives command 'e' or 'end', then Jlp22 procees similarly as if an error had occured,
	! i.e. print error messages and returns control to sit> -promt.
	!endsection
 
 
	character*(*),intent(inout)::inp
	integer,intent(inout)::linp
	logical, intent(in)::plus
	integer,dimension(4)::iargs
	integer,dimension(5)::li
	logical ::contline=.false.
	character*1 oper
	if(plus)then
		oper='+'
		ial00=index(inp,';sum(')
	else !if(plus)then
		oper='-'
		ial00=index(inp,';dif(')
	endif !if(plus)  12266
	!write(6,*)plus,'ial00',ial00,linp,inp(1:linp)
	! do i=1,linp
	! write(6,*)i,ichar(inp(i:i))
	! enddo
	ial=ial00+4
	ial0=ial
	!write(6,*)'<3ial',ial
	!	write(6,*)'<33>','/',inp(1:linp),'/'
	do nargs=1,4
		li(nargs)=j_nextlim2(inp,ial,linp,',)')
		ial=li(nargs)+1
		!	write(6,*)'<3ial',nargs,ial,inp(li(nargs):li(nargs))
 
 
		if(inp(li(nargs):li(nargs)).eq.')')exit
	enddo !nargs=1,4  12281
	if(nargs.lt.3)then
		write(6,*)';sum should have at least three arguments'
		j_err=.true.;return
	endif !if(nargs.lt.3)  12289
 
	last=li(nargs)
	!	write(6,*)'<45last,nargs,li',last,nargs,li,inp(last+1:last+1),ial0,ial
	if(inp(last+1:last+1).ne.'(')then
		write(6,*)';sum() should be follofed by (...)'
		j_err=.true.
		return
	endif !if(inp(last+1:last+1).ne.'(')  12296
 
	!write(6,*)'let ',inp(ial:ial)
	if(j_isletter(inp(ial0+1:ial0+1)))then
		!write(6,*)'<44>',inp(ial0+1:li(1)-1)
		iv=j_object(inp(ial0+1:li(1)-1))
		if(iv.le.0)iv=j_getobject(0,inp(ial0+1:li(1)-1),j_ipreal)
		if(j_err)return
	else !if(j_isletter(inp(ial0+1:ial0+1)))then
		write(6,*)';sum(,first argument must be a variable'
		j_err=.true.
		return
	endif !if(j_isletter(inp(ial0+1:ial0+1)))  12303
	! call j_printname('eka ',iv,' ')
	do j=2,nargs
		!write(6,*)'<55>',inp(li(j-1)+1:li(j)-1)
		if(j_isletter(inp(li(j-1)+1:li(j)-1)))then
			iv2=j_object(inp(li(j-1)+1:li(j)-1))
			if(iv2.le.0)then
				write(6,*)';sum argument '//inp(li(j-1)+1:li(j)-1)//' is not variable or constant'
				j_err=.true.
				return
			endif !if(iv2.le.0)  12318
		else !if(j_isletter(inp(li(j-1)+1:li(j)-1)))then
 
			iv2=j_isconst(inp(li(j-1)+1:li(j)-1))
			!		write(6,*)'iv2 ',iv2
			if(iv2.le.0)then
				write(6,*)';sum argument '//inp(li(j-1)+1:li(j)-1)//' is not variable or constant'
				j_err=.true.
				return
			endif !if(iv2.le.0)  12327
 
		endif !if(j_isletter(inp(li(j-1)+1:li(j)-1)))  12316
		iargs(j)=j_v(iv2)
	enddo !j=2,nargs  12314
	istep=1
	if(nargs.gt.3)istep=iargs(4)
	if(istep.eq.0.or. (istep.gt.0.and.iargs(3).lt.iargs(2)).or.&
			(istep.lt.0.and.iargs(3).gt.iargs(2)))then
		write(6,*)';sum illegal arguments ',inp(ial:last)
		j_err=.true.
		return
	endif !if(istep.eq.0.or. (istep.gt.0.and.iargs(3).lt.iargs(2)).  12338
	lop=j_nextrp(inp,last+1,linp)
	!write(6,*)'<44lop',lop,inp(lop:lop)
 
	luus=	ial0
	lu=0
	!write(6,*)'iargs ',iargs
	do i=iargs(2),iargs(3),istep
		j_v(iv)=i
 
		!call interpret(inp(last+2:lop-1),lop-last-2,j_tempchar(luus+1:),lec,contline)
		!subroutine inputinterpret2(inp,linp,edited) ! interpreting "
		linp2=lop-1
		j_tempchar=inp(last+2:linp2)
		!	write(6,*)'<567>',inp(last+2:lop-1),lop-last-2
		call inputinterpret2(j_tempchar,linp2-last-1,contline)
		if(j_err)return
		lenf=j_lentrim(j_tempchar)
		!write(6,*)'<23>',j_tempchar(1:lenf)
		if(i.lt.iargs(3))then
			j_tempchar(lenf+1:lenf+1)=oper
			lenf=lenf+1
		endif !if(i.lt.iargs(3))  12362
		j_tempchar2(lu+1:lu+lenf)=j_tempchar(1:lenf)
		lu=lu+lenf
	enddo !i=iargs(2),iargs(3),istep  12350
 
 
	!write(6,*)'lu',lu,lop,ial00,j_tempchar(1:lu),'?',inp(lop+1:linp),'?'
	!	j_tempchar2(lu+1:)=inp(lop+1:linp)
	inp(ial00:)=j_tempchar2(1:lu)//inp(lop+1:linp)
	lis=lu-(lop-ial00+1)
	linp=linp+lis
	!rite(6,*)'<666linp',linp,inp(1:linp)
	return
 
 
end subroutine !subroutine j_getsum(inp,linp,plus)
 
!option_name(iopt,le) : returns the index of option, but this is in j_utilities.f90 becasue
function j_option_name(iopt,le)
 
	integer,intent(in)::iopt
	integer, intent(in):: le
	character(len=le) j_option_name
 
	if(iopt.gt.0.and.iopt.le.j_noptions_)then
		j_option_name=j_options(iopt)
	elseif(iopt.le.j_nopts1)then !if(iopt.gt.0.and.iopt.le.j_noptions_)then
		j_option_name=o1_options(iopt-j_noptions)
	elseif(iopt.le.j_nopts2)then !if(iopt.gt.0.and.iopt.le.j_noptions_)then
		j_option_name=o2_options(iopt-j_nopts1)
	elseif(iopt.le.j_noptions)then !if(iopt.gt.0.and.iopt.le.j_noptions_)then
		j_option_name=o3_options(iopt-j_nopts2)
	else !if(iopt.gt.0.and.iopt.le.j_noptions_)then
		j_option_name='*illegal option*'
		write(6,*)'*j* illegal value for iopt in options ',iopt
		j_err=.true.
	endif !if(iopt.gt.0.and.iopt.le.j_noptions_)  12389
	return
end function j_option_name !function j_option_name(iopt,le)
 
 
function j_function_name(ifunc,le)
 
	integer, intent(in) :: ifunc
	integer, intent(in) :: le
	character(len=le) j_function_name
 
	if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)then
		j_function_name=j_functions(ifunc)
	elseif(ifunc.le.j_nfuncs1)then !if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)then
		j_function_name=o1_functions(ifunc-j_nfunctions_)
	elseif(ifunc.le.j_nfuncs2)then !if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)then
		j_function_name=o2_functions(ifunc-j_nfuncs1)
	elseif(ifunc.le.j_nfunctions)then !if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)then
		j_function_name=o3_functions(ifunc-j_nfuncs2)
	else !if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)then
		j_function_name='*illegal function *'
		!write(6,*)'*j* illegal value for ifunc in function ',ifunc
		j_err=.true.
	endif !if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)  12412
	return
end function j_function_name !function j_function_name(ifunc,le)
 
 
function j_objecttype_index(objecttype)
 
	character*16, intent(in):: objecttype
 
	le=len_trim(objecttype)
	j_objecttype_index=j_isin(objecttype(1:le),j_objecttypes,j_nobjecttypes_)
	!###TESTAUS###
	!write(6,*)'jcompil <3557> i=isin(...): input(icur:ipos1)', input(icur:ipos1)
	!20141219 if(i==0)
	if(j_objecttype_index==0) then
		io_ = j_isin(objecttype(1:le),o1_objecttypes,o1_nobjecttypes)
		if(io_ >0) then
			j_objecttype_index = j_nobjecttypes+ io_ !1000+io_
		else !if(io_ >0) then
			io_ = j_isin(objecttype(1:le),o2_objecttypes,o2_nobjecttypes)
			if(io_ > 0) then
				j_objecttype_index = j_nobjecttypes1+io_ !2000+io_
			else !if(io_ > 0) then
				io_ = j_isin(objecttype(1:le),o3_objecttypes,o3_nobjecttypes)
				if(io_ > 0) then
					j_objecttype_index = j_nobjecttypes2+ io_ !  3000+io_
				endif !if(io_ > 0)  12448
			endif !if(io_ > 0)  12444
		endif !if(io_ >0)  12440
	endif !if(j_objecttype_index==0)  12438
end function j_objecttype_index !function j_objecttype_index(objecttype)
 
 
function j_objecttype_name(ipobjecttype,le)
 
	implicit none
 
	integer, intent(in) :: ipobjecttype
	integer, intent(in) :: le
	character(len=le) j_objecttype_name
 
 
	if(ipobjecttype.gt.0.and.ipobjecttype.le.j_nobjecttypes)then
		j_objecttype_name=j_objecttypes(ipobjecttype)
	elseif(ipobjecttype.le.j_nobjecttypes1)then !if(ipobjecttype.gt.0.and.ipobjecttype.le.j_nobjecttypes)then
		j_objecttype_name=o1_objecttypes(ipobjecttype-j_nobjecttypes)
	elseif(ipobjecttype.le.j_nobjecttypes2)then !if(ipobjecttype.gt.0.and.ipobjecttype.le.j_nobjecttypes)then
		j_objecttype_name=o2_objecttypes(ipobjecttype-j_nobjecttypes1)
	elseif(ipobjecttype.le.j_nobjecttypes)then !if(ipobjecttype.gt.0.and.ipobjecttype.le.j_nobjecttypes)then
		j_objecttype_name=o3_objecttypes(ipobjecttype-j_nobjecttypes2)
	else !if(ipobjecttype.gt.0.and.ipobjecttype.le.j_nobjecttypes)then
		!write(6,*)'*j* function objectype, illegal argument ',ipobjecttype
		j_objecttype_name='*illegal objecttype'
		j_err=.true.
 
	endif !if(ipobjecttype.gt.0.and.ipobjecttype.le.j_nobjecttypes)  12466
	return
end function j_objecttype_name !function j_objecttype_name(ipobjecttype,le)
 
 
 
 
 
function j_chr_integer(ival,le,left,fill,le2) !the purpose is to make a general character function to
	! present integer values. Problem is that the function should not use write statement, because if it is using write
	!statement it cannot be used in write statement
 
	implicit none
 
	integer, intent(in) :: ival
	integer, intent(in) :: le
	character(len=le) j_chr_integer
	logical, intent(in) :: left
	logical, intent(in) ::fill
	integer, intent(out)::le2
 
	integer len_,i1,lenout
 
	len_=len(j_chrchr)
	write(j_chrchr,'(i13)')ival
	i1=j_nonblank(j_chrchr,1,len_)
	lenout=len_-i1+1
	if(le.lt.lenout)then
		write(6,*)'*j* chr_integer, available length ',le, 'needed length ',lenout
		j_chr_integer='?????'
		j_err=.true.
		return
	endif !if(le.lt.lenout)  12506
	if(left)then
		j_chr_integer=j_chrchr(i1:len_)
		le2=le-i1+1
	else !if(left)then
		if(fill)then
			!if(i1.eq.
		else !if(fill)then
 
 
		endif !if(fill)  12516
 
 
	endif !if(left)  12512
 
end function j_chr_integer !function j_chr_integer(ival,le,left,fill,le2)
 
 
function j_chr_real(rval,le,left,fill,le2) !!the purpose is to make a general character function to
	! present real values. Problem is that the function should not use write statement, because if it is using write
	!statement it cannot be used in write statement
	real, intent(in) :: rval
	integer, intent(in) :: le
	!character(len=le) chr_real
	logical, intent(in) :: left
	logical, intent(in) ::fill
	integer, intent(out)::le2
	return
end function j_chr_real !function j_chr_real(rval,le,left,fill,le2)
 
 
function j_chr_double(dval,le,left,fill,le2) !!the purpose is to make a general character function to
	! present double precision values. Problem is that the function should not use write statement, because if it is using write
	!statement it cannot be used in write statement
	double precision, intent(in) :: dval
	integer, intent(in) :: le
	!	character(len=le) chr_real
	logical, intent(in) :: left
	logical, intent(in) ::fill
	integer, intent(out)::le2
	return
end function j_chr_double !function j_chr_double(dval,le,left,fill,le2)
 
 
function lenobjects(iv)
 
	implicit none
	integer lenobjects
	integer, intent(in) :: iv
 
	character*3 temp
 
	!	 nv=mxnamedv+mxtemporalv
	!	mxv=nv+mxconstantv
	if(iv.gt.0.and.iv.le.j_namedv)then
		lenobjects=j_o(j_ivnames)%i(iv+1)-j_o(j_ivnames)%i(iv)
	elseif(iv.gt.j_mxnamedv.and.iv.le.j_nv)then !if(iv.gt.0.and.iv.le.j_namedv)then
		lenobjects=8
	elseif(iv.gt.j_nv.and.iv.le.j_mxv)then !if(iv.gt.0.and.iv.le.j_namedv)then
		lenobjects=8
	else !if(iv.gt.0.and.iv.le.j_namedv)then
		write(6,*)'*j* lenobject, illegal arument ',iv
		j_err=.true.
	endif !if(iv.gt.0.and.iv.le.j_namedv)  12565
end function lenobjects !function lenobjects(iv)
 
 
function j_object_name(iv,le)
 
	implicit none
	integer, intent(in) :: le
	character(len=le)::j_object_name
	integer, intent(in) :: iv
 
 
	!	character(len=le) objecttype_name
	integer len,i_,itemp,ibas_
	!write(6,*)'iv,le,namedv',iv,le,namedv
	if(iv.gt.0.and.iv.le.j_namedv)then
		len=j_o(j_ivnames)%i(iv+1)-j_o(j_ivnames)%i(iv)
		!	write(6,*)'len',len
		!	if(len.ne.le)then
		!		write(6,*)'*j* object_name, illegal argument le ',le,' it should be ',len
		!		call crash()
		!		return
		ibas_=j_o(j_ivnames)%i(iv)-1 ! h sotores links to first element in name
 
		do i_=1,min(len,le)
			j_object_name0(i_:i_)=j_o(j_ivnames)%ch(ibas_+i_)
		enddo !i_=1,min(len,le)  12598
		if(len.lt.le)j_object_name0(len+1:le)=' '
 
	elseif(iv.gt.j_mxnamedv.and.iv.le.j_nv)then !if(iv.gt.0.and.iv.le.j_namedv)then
		itemp=iv-j_mxnamedv
		j_object_name0='$$Temp'
		j_object_name0(8:8)=char(48+mod(itemp,10))
		itemp=itemp/10
		j_object_name0(7:7)=char(48+mod(itemp,10))
	elseif(iv.gt.j_nv.and.iv.le.j_mxv)then !if(iv.gt.0.and.iv.le.j_namedv)then
		j_object_name0 = 'constant' !lenobject=7
	else !if(iv.gt.0.and.iv.le.j_namedv)then
		!	write(6,*)'*j* lenobject, illegal arument ',iv
		j_object_name0 = '*j* wrong object index'
	endif !if(iv.gt.0.and.iv.le.j_namedv)  12589
	j_object_name = j_object_name0
	return
end function j_object_name !function j_object_name(iv,le)
 
real function j_vlog(iff)
	logical iff
	if(iff)then;j_vlog=1.;else;j_vlog=0.;end if
end function j_vlog !real function j_vlog(iff)
 
!20150202 function eqf
real function j_eqf(iv1,iv2)
	integer, intent(in) ::iv1
	integer, intent(in) ::iv2
 
 
	j_eqf = 0.
	if(j_otype(iv1)==j_ipreal.and.j_otype(iv2)==j_ipreal) then
		j_eqf = j_vlog(j_v(iv1)==j_v(iv2))
	elseif((j_otype(iv1)==j_ipchar).and.(j_otype(iv2)==j_ipchar)) then !if(j_otype(iv1)==j_ipreal.and.j_otype(iv2)==j_ipreal) then
		call j_getchar(iv1,j_tempchar,leniv1)
		call j_getchar(iv2,j_tempchar2,leniv2)
		if(j_tempchar(1:leniv1)==j_tempchar2(1:leniv2)) j_eqf = 1.
	else !if(j_otype(iv1)==j_ipreal.and.j_otype(iv2)==j_ipreal) then
		write(6,*)'illegal component types in .eq.'
		j_err = .true.
	endif !if(j_otype(iv1)==j_ipreal.and.j_otype(iv2)==j_ipreal)  12631
	return
end function j_eqf !real function j_eqf(iv1,iv2)
 
subroutine j_printname(text1,iv,text2,iv2) !print variable name with text
 
	character*24 name,name2
	character(len=*),intent(in):: text1,text2
	integer, intent(in)::iv
	integer, intent(in),optional:: iv2
 
	logical:: jerr
	jerr=j_err
	j_err=.false.
	n6=6
	if(text2.eq.' jlpdebugging?') n6=16
	if(iv.le.0.or.iv.gt.j_nv)then
		write(6,*)'*j* illegal iv in printname ',iv
		j_err=.true.
 
		return
	endif !if(iv.le.0.or.iv.gt.j_nv)  12656
 
	if(present(iv2))then
		call j_getname(iv,iv2)
		write(n6,*)text1,j_oname(1:j_loname),text2,j_oname2(1:j_loname2)
	else
 
		call j_getname(iv)
		write(n6,*)text1,j_oname(1:j_loname),text2
 
	endif !if(present(iv2))  12663
	j_err=jerr
	return
end subroutine j_printname !subroutine j_printname(text1,iv,text2,iv2)
 
! subroutine puti(ivec,iel,ival)       !ei tod näk tarvita
! integer,dimension(:),intent(inout), pointer::ivec    ! HHir. 8.3/2011  =>null()
! integer,intent(in) :: iel
! integer,intent(in) :: ival
 
! integer,dimension(:), pointer::ivec2 !! HHir. 8.3/2011  =>null()
! if(ubound(ivec,dim=1).lt.iel)then
! allocate(ivec2(lbound(ivec,dim=1):2*ubound(ivec,dim=1)))
!write(6,*)'*doubling an integer vector'
! ivec2(lbound(ivec,dim=1):ubound(ivec,dim=1))=ivec
! ivec2(iel)=ival
! deallocate(ivec)
! ivec=>ivec2
! nullify(ivec2)
! return
! end if
! ivec(iel)=ival
! end subroutine puti
 
subroutine j_puti(ivec,iel,ival)
	integer,dimension(:),intent(inout), allocatable::ivec
	integer,intent(in) :: iel
	integer, intent(in) ::ival
 
	call j_checki(ivec,iel)
 
	ivec(iel)=ival
end subroutine j_puti !subroutine j_puti(ivec,iel,ival)
 
subroutine j_checki(ivec,iel)
	integer,dimension(:),intent(inout), allocatable::ivec
	integer,intent(in) :: iel
 
	integer,dimension(:), allocatable::ivec2
	iubound=ubound(ivec,dim=1)
	if(iubound.lt.iel)then
		lbound0=lbound(ivec,dim=1)
		!		allocate(ivec2(lbound_:iubound_))
		!		write(6,*)'*doubling a allocatable integer vector'
		!		ivec2(lbound_:iubound_)=ivec
 
 
		!		deallocate(ivec)
		allocate(ivec2(lbound0:2*iel))
		ivec2(1:iubound) = ivec      ! copy of data
		ivec2(iubound+1:2*iel)=0
		call move_alloc( from=ivec2, to=ivec )
		!	ivec(lbound_:iubound_)=ivec2
		!	deallocate(ivec2)
	end if !if(iubound.lt.iel)  12711
 
end subroutine j_checki !subroutine j_puti(ivec,iel,ival)
 
! subroutine j_checkl(ivec,iel)
! logical,dimension(:),intent(inout), allocatable::ivec
! integer,intent(in) :: iel
 
! logical dimension(:), allocatable::ivec2
! iubound_=ubound(ivec,dim=1)
! if(iubound_.lt.iel)then
! lbound_=lbound(ivec,dim=1)
! !		allocate(ivec2(lbound_:iubound_))
! !		write(6,*)'*doubling a allocatable integer vector'
! !		ivec2(lbound_:iubound_)=ivec
 
 
! !		deallocate(ivec)
! allocate(ivec2(lbound_:2*iel))
! ivec2(1:iubound) = ivec      ! copy of data
! call move_alloc( from=ivec2, to=ivec )
! !	ivec(lbound_:iubound_)=ivec2
! !	deallocate(ivec2)
! end if !if(iubound_.lt.iel)  11353
 
! end subroutine j_checkl !subroutine j_puti(ivec,iel,ival)
 
subroutine j_checkd(dvec,iel,lenn)
	double precision,dimension(:),intent(inout), allocatable::dvec
	integer,intent(in) :: iel
	integer,optional,intent(out) :: lenn
 
	double precision,dimension(:), allocatable::dvec2
	iubound=ubound(dvec,dim=1)
	if(iubound.lt.iel)then
		lbound0=lbound(dvec,dim=1)
		!		allocate(ivec2(lbound_:iubound_))
		!		write(6,*)'*doubling a allocatable integer vector'
		!		ivec2(lbound_:iubound_)=ivec
 
 
		!		deallocate(ivec)
		iubound2=2*iel
		allocate(dvec2(lbound0:iubound2))
		dvec2(1:iubound) = ivec      ! copy of data
		dvec2(iubound+1:iubound2)=j_0
		call move_alloc( from=dvec2, to=dvec )
		if(present(lenn))lenn=iubound2
		!	ivec(lbound_:iubound_)=ivec2
		!	deallocate(ivec2
	else
		if(present(lenn))lenn=iubound
	end if !if(iubound.lt.iel)  12759
 
end subroutine j_checkd !subroutine j_puti(ivec,iel,ival)
 
subroutine j_checkr(dvec,iel,lenn)
	real,dimension(:),intent(inout), allocatable::dvec
	integer,intent(in) :: iel
	integer,optional,intent(out) :: lenn
 
	real,dimension(:), allocatable::dvec2
	iubound=ubound(dvec,dim=1)
	if(iubound.lt.iel)then
		lbound0=lbound(dvec,dim=1)
		!		allocate(ivec2(lbound_:iubound_))
		!		write(6,*)'*doubling a allocatable integer vector'
		!		ivec2(lbound_:iubound_)=ivec
 
 
		!		deallocate(ivec)
		iubound2=2*iel
		allocate(dvec2(lbound0:iubound2))
		dvec2(1:iubound) = ivec      ! copy of data
		dvec2(iubound+1:iubound2)=j_0
		call move_alloc( from=dvec2, to=dvec )
		!	ivec(lbound_:iubound_)=ivec2
		if(present(lenn))lenn=iubound2
	else
		if(present(lenn))lenn=iubound
		!	deallocate(ivec2)
	end if !if(iubound.lt.iel)  12788
 
end subroutine j_checkr !subroutine j_puti(ivec,iel,ival)
! logical,dimension(:), intent(inout), pointer::ivec  ! HHir. 8.3/2011    =>null()
! integer :: iel
! logical ::ival
 
! logical,dimension(:), pointer::ivec2  ! HHir. 8.3/2011  =>null()
! if(ubound(ivec,dim=1).lt.iel)then
! allocate(ivec2(lbound(ivec,dim=1):2*ubound(ivec,dim=1)))
! !write(6,*)'*doubling an logical vector'
! ivec2(lbound(ivec,dim=1):ubound(ivec,dim=1))=ivec
! call move_alloc( from=ivec2, to=ivec )
! !		deallocate(ivec)
! !		ivec=>ivec2
! !		nullify(ivec2)
! end if !if(ubound(ivec,dim=1).lt.iel)  11213
! ivec(iel)=ival
! end subroutine j_putl !subroutine j_putl(ivec,iel,ival)
 
subroutine j_putim(ivec,iel1,iel2,ival)
	integer,dimension(:,:),intent(inout), allocatable::ivec
	integer,intent(in) :: iel1
	integer,intent(in) :: iel2
	integer,intent(in) :: ival
	integer ub1,ub2
	integer,dimension(:,:), allocatable::ivec2
	logical allo
	allo=.false.
	ub1=ubound(ivec,dim=1)
	ub2=ubound(ivec,dim=2)
	if(ub1.lt.iel1)then
		allo=.true.;new1=2*ub1;new2=ub2
	end if !if(ub1.lt.iel1)  12837
	if(ub2.lt.iel2)then
		allo=.true.;new2=2*ub2
	end if !if(ub2.lt.iel2)  12840
	if(allo)then
		lb1=lbound(ivec,dim=1) ; lb2=lbound(ivec,dim=2)
		allocate(ivec2(lb1:ub1,lb2:ub2))
		!write(6,*)'*doubling an integer matrix'
		ivec2=ivec
		call move_alloc( from=ivec2, to=ivec )
		! deallocate(ivec)
		! allocate(ivec(lb1:new1,lb2:new2))
		! ivec=0
		! ivec(lb1:ub1,lb2:ub1)=ivec2
		! deallocate(ivec2)
	end if !if(allo)  12843
	ivec(iel1,iel2)=ival
end subroutine !subroutine j_putim(ivec,iel1,iel2,ival)
 
! subroutine putim(ivec,iel1,iel2,ival)
! integer,dimension(:,:),intent(inout), pointer::ivec
! integer,intent(in) :: iel1
! integer,intent(in) :: iel2
! integer,intent(in) :: ival
 
! integer,dimension(:,:), pointer::ivec2  ! HHir. 8.3/2011  =>null()
! logical allo
! allo=.false.
! if(ubound(ivec,dim=1).lt.iel1)then
! allo=.true.;new1=2*ubound(ivec,dim=1);new2=ubound(ivec,dim=2)
! end if
! if(ubound(ivec,dim=2).lt.iel2)then
! allo=.true.;new2=2*ubound(ivec,dim=2)
! end if
! if(allo)then
! lb1=lbound(ivec,dim=1) ; lb2=lbound(ivec,dim=2)
! allocate(ivec2(lb1:new1,lb2:new2))
!write(6,*)'*doubling an integer matrix'
! ivec2(lb1:ubound(ivec,dim=1),lb2:ubound(ivec,dim=2))=ivec
! deallocate(ivec)
! ivec=>ivec2
! nullify(ivec2)
! end if
! ivec(iel1,iel2)=ival
! end subroutine
 
! subroutine putv(rvec,iel,val)
! real,dimension(:), pointer::rvec
! integer,intent(in) :: iel
! real, intent(in) ::val
 
! real,dimension(:), pointer::rvec2
! if(ubound(rvec,dim=1).lt.iel)then
! allocate(rvec2(lbound(rvec,dim=1):2*ubound(rvec,dim=1)))
!write(6,*)'*doubling a real vector'
! rvec2(lbound(rvec,dim=1):ubound(rvec,dim=1))=rvec
! deallocate(rvec)
! rvec=>rvec2
! nullify(rvec2)
! end if
! rvec(iel)=val
! end subroutine
 
subroutine j_putr(rvec,iel,val,lenn)
	real,dimension(:), allocatable::rvec
	integer,intent(in) :: iel
	real, intent(in) ::val
	integer,optional,intent(out) :: lenn
	if(present(lenn))then
		call j_checkr(rvec,iel,lenn)
	else
		call j_checkr(rvec,iel)
	endif !if(present(lenn))  12907
	! real,dimension(:), allocatable::rvec2
	! iubound_=ubound(rvec,dim=1)
	! if(iubound_.lt.iel)then
	! lbound_=lbound(rvec,dim=1)
	! allocate(rvec2(lbound_:iubound_))
	! !		write(6,*)'*doubling a allocatable real vector'
	! rvec2(lbound_:iubound_)=rvec
	! call move_alloc( from=rvec2, to=rvec )
	! !	deallocate(rvec)
	! !	allocate(rvec(lbound_:2*iubound_))
	! !	rvec(lbound_:iubound_)=rvec2
	! !	deallocate(rvec2)
	! end if !if(iubound_.lt.iel)  12821
	rvec(iel)=val
end subroutine !subroutine j_putr(rvec,iel,val)
 
subroutine j_putd(rvec,iel,val,lenn)
	double precision,dimension(:), allocatable::rvec
	integer,intent(in) :: iel
	double precision, intent(in) ::val
	integer,optional,intent(out) :: lenn
	if(present(lenn))then
		call j_checkd(rvec,iel,lenn)
	else
		call j_checkd(rvec,iel)
	endif !if(present(lenn))  12933
	! double precision,dimension(:), allocatable::rvec2
	! iubound_=ubound(rvec,dim=1)
	! if(iubound_.lt.iel)then
	! lbound_=lbound(rvec,dim=1)
	! allocate(rvec2(lbound_:iubound_))
	! !		write(6,*)'*doubling a allocatable real vector'
	! rvec2(lbound_:iubound_)=rvec
	! call move_alloc( from=rvec2, to=rvec )
	! ! deallocate(rvec)
	! ! allocate(rvec(lbound_:2*iubound_))
	! ! rvec(lbound_:iubound_)=rvec2
	! ! deallocate(rvec2)
	! end if !if(iubound_.lt.iel)  11442
	rvec(iel)=val
end subroutine !subroutine j_putd(rvec,iel,val)
 
 
integer function j_leno(iv)  ! the length of object name
	integer, intent(in)::iv
	j_leno= j_o(j_ivnames)%i(iv+1)-j_o(j_ivnames)%i(iv)
end function !integer function j_leno(iv)
 
 
! integer function j_lentitles(iv)
! implicit none
! integer, intent(in)::iv
! j_lentitles=1
! if(iv.le.j_namedv)then
! if(j_otitle(iv).gt.0)then
! j_lentitles=j_o(j_otitle(iv))%i(2)-j_o(j_otitle(iv))%i(1)+1
! endif !if(j_otitle(iv).gt.0)then
! endif !if(iv.le.j_namedv)then
! end function j_lentitles !integer function j_lentitles(iv)
 
! function object_title(iv,le)    ! %%title character function alternative for gettitle
! implicit none
! integer, intent(in) :: iv
! integer, intent(in) :: le
! character(len=le) object_title
 
! integer len,i_,itemp,ibas
 
! if(iv.gt.0.and.iv.le.j_namedv)then
! if(j_otitle(iv).gt.0)then
! ibas=j_o(j_otitle(iv))%i(1)-1
! do i_=1,j_lentitles(iv)
! j_object_name0(i_:i_)=j_o(j_ivnames)%ch(ibas+i_)
! enddo !do i_=1,j_lentitles(iv)
! object_title=j_object_name0
! else !if(j_otitle(iv).gt.0)then
! object_title=' '
! endif !if(j_otitle(iv).gt.0)then
! else !if(iv.gt.0.and.iv.le.j_namedv)then
! object_title=' '
! endif !if(iv.gt.0.and.iv.le.j_namedv)then
 
! return
! end function object_title !function object_title(iv,le)
 
real function j_valuesspl(ifunc,arg) !compute the value of a smoothing %%spline
 
	!module vmod
	!end module vmod
 
 
	integer nob_,nob2,nwarn
	!use j_globalfuncsmod	,only: printname
	double precision ,dimension(:), pointer::c=>null()
	double precision splder,argd,wrk(4)
	argd=arg
	m=j_o(ifunc)%i(1);nob_=j_o(ifunc)%i(2);nob2=j_o(ifunc)%i(3)  !efficient dimension
	goto 80
	if(argd.lt.j_o(ifunc)%d(1).or.argd.gt.j_o(ifunc)%d(nob2))then
		call j_printname('argument of smoothing spline ',ifunc,' out of range')
		write(6,*)'arg ',arg,' range:',j_o(ifunc)%d(1),j_o(ifunc)%d(nob2)
		nwarn=nwarn+1
		valuef=1.7e37
		if(nwarn.gt.mxwarn)then
			j_err=.true. ;nwarn=0
		end if !if(nwarn.gt.mxwarn)  13015
		return
	end if !if(argd.lt.j_o(ifunc)%d(1).or.argd.gt.j_o(ifunc)%d(nob2))  13010
	!C       L       (I/O)   L contains an integer such that:
	!C                       X(L).le.T and T.lt.X(L+1) if T is within
	!C                       the range X(1).le.T and T.lt.X(N). If
	!C                       T.lt.X(1), L is set to 0, and if T.ge.X(N),
	!C                       L is set to N. The search for L is facili-
	!C                       tated if L has approximately the right
	!C                       value on entry.
80 L=nob_*(argd-j_o(ifunc)%d(1))/(j_o(ifunc)%d(nob2)-j_o(ifunc)%d(1))
	L=max(1,L);L=min(nob_,L)
	!write(6,*)'m,nob_,l',m,nob_,l
	c=>j_o(ifunc)%d(nob_+1:nob_+nob2) !2*nob_)
	j_valuesspl=splder(0,m,nob2,argd,j_o(ifunc)%d,c,L,wrk)
	return
end function j_valuesspl !real function j_valuesspl(ifunc,arg)
 
integer function j_nargopt(iob,io,mopt) !number of option values, option not given => -1
	integer,intent(in) ::iob,io
	integer, intent(in)::mopt
 
	!module transmod  !teku ko
 
	!module vmod
	!end module vmod
	li=j_linkoption(iob,io,mopt)
	j_nargopt=li
 
	if(li.gt.0)j_nargopt=j_o(iob)%i(li)
 
	return
end function j_nargopt !integer function j_nargopt(iob,mopt)
 
 
integer function j_ibittest(ifunc,irow,icol)
	integer,intent(in) ::ifunc
	integer,intent(in) ::irow
	integer,intent(in) ::icol
 
	!module vmod
	!end module vmod
 
	j=(irow-1)*j_o(ifunc)%i(4)+icol-j_o(ifunc)%i(5)
	ii=(j-1)/32+1
	ibit=j-(ii-1)*32-1  ! bit numbering starts from zero
	!  rw=o(ifunc)%r(ii)
	if(btest(j_o(ifunc)%i2(ii),ibit))then
		j_ibittest=1
	else !if(btest(j_o(ifunc)%i2(ii),ibit))then
		j_ibittest=0
	endif !if(btest(j_o(ifunc)%i2(ii),ibit))  13064
	return
end function j_ibittest !integer function j_ibittest(ifunc,irow,icol)
 
integer function j_lentrim(inp)   !like len-trim but returns zero aslo when line consists of tabs
	character(len=*),intent(in):: inp
	le=len(inp)
 
	if(le.le.0)then
		j_lentrim=0
		return
	endif !if(le.le.0)  13076
	do j_lentrim=le,1,-1
		if(ichar(inp(j_lentrim:j_lentrim)).gt.32)return
	enddo !j_lentrim=le,1,-1  13080
	j_lentrim=0
	return
end function j_lentrim !integer function j_lentrim(inp)
 
subroutine j_getdat(ivdat,nobs,ivmat,ivkeep) !get links to data elements, used getobsiv, used in JLP
	integer,intent(in)::ivdat
	integer,intent(out)::nobs
	integer,intent(out)::ivmat
	integer,intent(out)::ivkeep
	!	integer,intent(out)::ivtrans
	!	integer,intent(out)::ivvars
	!module vmod
	!end module vmod
 
	ivmat=j_o(ivdat)%i(1)
	ivkeep=j_o(ivdat)%i(2)
	nobs=j_o(ivmat)%i(1)
	! if(j_o(ivdat)%i(6).gt.0)then    !trans
	! ivtrans=j_o(ivdat)%i(6)
	! else !if(j_o(ivdat)%i(6).gt.0)then
	! ivtrans=0
	! end if !if(j_o(ivdat)%i(6).gt.0)then
	! ivvars=j_o(ivdat)%i(8)
	return
end subroutine j_getdat !subroutine j_getdat(ivdat,nobs,ivmat,ivkeep)
 
! subroutine j_freeunit(nu) ! free unit nu : use only j_closeunit
! integer,intent(in) ::nu
! !module filemod
! !end module
 
! !j_err=.true.
! do i=1,j_nused
! if(j_nunits(i).eq.nu)then
! j_nunits(i)=j_nunits(j_nused)
! j_nunits(j_nused)=nu
! j_nused=j_nused-1
! return
! end if !if(j_nunits(i).eq.nu)then
! end do !do i=1,j_nused
!write(6,*)'**trying to free unit which was not reserved'
! j_err=.true.
! return
! end subroutine j_freeunit !subroutine j_freeunit(nu)
 
integer function j_iounit(iv)
	integer,intent(in) ::iv
 
	! if(iv.eq.j_ivdollar)then
	! j_iounitr=5
	! return
	! endif
	if(iv.eq.j_ivdollar)then
		j_iounit=0
		return
	endif !if(iv.eq.j_ivdollar)  13135
	if (j_otype(iv).ne.j_ipchar)then
		call j_printname('*j* object ',iv,' is not of character type, it cannot be associate with a file')
		j_err=.true.;j_iounit=0
		return
	end if !if (j_otype(iv).ne.j_ipchar)  13139
	if(j_o(iv)%i(3).ne.0)then    !charcter variable
		j_iounit=j_o(j_o(iv)%i(3))%i(4)
	else !if(j_o(iv)%i(3).ne.0)then
		j_iounit=j_o(iv)%i(4)
	endif !if(j_o(iv)%i(3).ne.0)  13144
	return
end function j_iounit !integer function j_iounit(iv)
 
! integer function j_iounit(iv)
! integer,intent(in) ::iv
! if(iv.eq.j_ivdollar)then
! j_iounit(=6
! return
! endif
 
! if (j_otype(iv).ne.j_ipchar)then
! call j_printname('** object ',iv,' is not of character type, it cannot be associate with a file')
! j_err=.true.;j_iounit(=0
! return
! end if !if (j_otype(iv).ne.j_ipchar)then
! if(j_o(iv)%i(3).ne.0)then
! j_iounit(=j_o(j_o(iv)%i(3))%i(4)
! else
! j_iounit(=j_o(iv)%i(4)
! endif
! return
! end function j_iounit( !integer function j_iounit(iv)
 
subroutine j_putiounit(nu,iv)
	integer,intent(in)::nu
	integer, intent(in):: iv
 
	if (j_otype(iv).ne.j_ipchar)then
		call j_printname('*j* object ',iv,' is not of character type, it cannot be associate with a file')
		j_err=.true.
		return
	end if !if (j_otype(iv).ne.j_ipchar)  13176
	if(j_o(iv)%i(3).ne.0)then
		j_o(j_o(iv)%i(3))%i(4)=nu
	else !if(j_o(iv)%i(3).ne.0)then
		j_o(iv)%i(4)=nu
	endif !if(j_o(iv)%i(3).ne.0)  13181
	return
end subroutine j_putiounit !subroutine j_putiounit(nu,iv)
 
integer function j_ibass(ivmatrix,iobs,isaa)
 
	integer,intent(in)::ivmatrix
	integer,intent(in)::iobs
	integer,intent(in)::isaa  !1 or 2 indicating which buffer row is used
 
	! i(6) the number of rows in the intial part
	! i(7) first row in the initial part
	! i(8) last row in intial part , note there may be an unused are
	! i(9) the row which is stored after i(10)
	! i(11) the row which is stored after i(12)
 
	! if(j_o(ivmatrix)%i(4).lt.0)then !i(4) is the matrix type
	! if(iobs.ge.j_o(ivmatrix)%i(7).and.iobs.le.j_o(ivmatrix)%i(8))then
	! j_ibass=(iobs-j_o(ivmatrix)%i(7))*j_o(ivmatrix)%i(2)
	! return
	! endif !if(iobs.ge.j_o(ivmatrix)%i(7).and.iobs.le.j_o(ivmatrix)%i(8))then
	! if(isaa.eq.1)then
	! j_ibass=j_o(ivmatrix)%i(10)
	! if(iobs.ne.j_o(ivmatrix)%i(9))then
	! read(-j_o(ivmatrix)%i(4),rec=iobs)j_o(ivmatrix)%r(j_ibass+1:j_ibass+j_o(ivmatrix)%i(2))
	! j_o(ivmatrix)%i(9)=iobs
	! endif !if(iobs.ne.j_o(ivmatrix)%i(9))then
	! else !if(isaa.eq.1)then
	! j_ibass=j_o(ivmatrix)%i(12)
	! if(iobs.ne.j_o(ivmatrix)%i(11))then
	! read(-j_o(ivmatrix)%i(4),rec=iobs)&
	! j_o(ivmatrix)%r(j_ibass+1:j_ibass+j_o(ivmatrix)%i(2))
	! j_o(ivmatrix)%i(11)=iobs
	! endif !if(iobs.ne.j_o(ivmatrix)%i(11))then
	! endif !if(isaa.eq.1)then
	! else !if(j_o(ivmatrix)%i(4).lt.0)then
	j_ibass=(iobs-1)*j_o(ivmatrix)%i(2)
	!	endif !if(j_o(ivmatrix)%i(4).lt.0)then
 
	return
end function !integer function j_ibass(ivmatrix,iobs,isaa)
 
! subroutine j_ibass2(ivmatrix,iobs1,iobs2,ibas1,ibas2)
! integer,intent(in)::ivmatrix,iobs1,iobs2
! integer,intent(out)::ibas1,ibas2
! logical ::notyet
! !allocate( j_o(ivout)%i(1:10))    !i(6:7) the last matrix  rows read from direct access file, i(8) i(4)=type
! ! i(2)=number of columns
! ! which one to read next 9:10 which rows are above the two rows
! !	save ixdisk1,ixdisk2,last
! if(j_o(ivmatrix)%i(4).lt.0)then
! notyet=.true.
! if(iobs1.eq.j_o(ivmatrix)%i(6))then
! ibas1=0
! j_o(ivmatrix)%i(8)=1  !read second
! notyet=.false.
! elseif(iobs1.eq.j_o(ivmatrix)%i(7))then
! ibas1=j_o(ivmatrix)%i(2)
! j_o(ivmatrix)%i(8)=2  !read first
! notyet=.false.
! endif
! if(iobs2.eq.j_o(ivmatrix)%i(6))then
! ibas2=0
! elseif(iobs1.eq.j_o(ivmatrix)%i(7))then
! ibas2=j_o(ivmatrix)%i(2)
! elseif(j_o(ivmatrix)%i(8).eq.2)then !read now the first
! read(-j_o(ivmatrix)%i(4),rec=iobs2)j_o(ivmatrix)%r(1:j_o(ivmatrix)%i(2))
! j_o(ivmatrix)%i(6)=iobs2
! j_o(ivmatrix)%i(8)=1
! ibas2=0
! else
! read(-j_o(ivmatrix)%i(4),rec=iobs2)j_o(ivmatrix)%r(j_o(ivmatrix)%i(2)+1:j_o(ivmatrix)%i(2)+j_o(ivmatrix)%i(2))
! j_o(ivmatrix)%i(7)=iobs
! j_o(ivmatrix)%i(8)=2
! ibas2=j_o(ivmatrix)%i(2)
! endif
! if(notyet)then
! if(j_o(ivmatrix)%i(8).eq.2)then !read now the first
! read(-j_o(ivmatrix)%i(4),rec=iobs1)j_o(ivmatrix)%r(1:j_o(ivmatrix)%i(2))
! j_o(ivmatrix)%i(6)=iobs1
! j_o(ivmatrix)%i(8)=1
! ibas1=0
! else
! read(-j_o(ivmatrix)%i(4),rec=iobs1)j_o(ivmatrix)%r(j_o(ivmatrix)%i(2)+1:j_o(ivmatrix)%i(2)+j_o(ivmatrix)%i(2))
! j_o(ivmatrix)%i(7)=iobs1
! j_o(ivmatrix)%i(8)=2
! ibas2=j_o(ivmatrix)%i(2)
! endif
 
! endif
 
! else
! ibas1=(iobs1-1)*j_o(ivmatrix)%i(2)
! ibas2=(iobs2-1)*j_o(ivmatrix)%i(2)
! endif
 
! return
! end subroutine
 
 
subroutine j_copy(iob,io)   !makes a copy of an object j-function
	integer,intent(in)::iob
	integer,intent(in)::io
	!module vmod
	!end module vmod
	!se typemod
	! a=b b general object
	! narg=o(iob)%i(io+1) not arg
	irg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2)
 
	if(irg.ne.iout)then
		!20141219 oli: if(otype(iout).ne.0)call del(iout)
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		!20141219 oli: if(otype(irg).ne.0)then
		if(j_otype(irg).ne.j_ipreal)then
 
			if(allocated(j_o(irg)%r))then
				allocate(j_o(iout)%r( lbound(j_o(irg)%r,1):ubound(j_o(irg)%r,1) ));j_o(iout)%r=j_o(irg)%r
			end if !if(allocated(j_o(irg)%r))  13302
			if(allocated(j_o(irg)%i))then
				allocate(j_o(iout)%i( lbound(j_o(irg)%i,1):ubound(j_o(irg)%i,1)));j_o(iout)%i=j_o(irg)%i
			end if !if(allocated(j_o(irg)%i))  13305
			if(allocated(j_o(irg)%i2))then
				allocate(j_o(iout)%i2( lbound(j_o(irg)%i2,1):ubound(j_o(irg)%i2,1)));j_o(iout)%i2=j_o(irg)%i2
			end if !if(allocated(j_o(irg)%i2))  13308
			if(allocated(j_o(irg)%d))then
				allocate(j_o(iout)%d( lbound(j_o(irg)%d,1):ubound(j_o(irg)%d,1)));j_o(iout)%d=j_o(irg)%d
			end if !if(allocated(j_o(irg)%d))  13311
			if(allocated(j_o(irg)%ch))then
				allocate(j_o(iout)%ch( lbound(j_o(irg)%ch,1):ubound(j_o(irg)%ch,1)));j_o(iout)%ch=j_o(irg)%ch
			end if !if(allocated(j_o(irg)%ch))  13314
			j_otype(iout)=j_otype(irg)
 
		else !if(j_otype(irg).ne.j_ipreal)then
			j_v(iout)=j_v(irg)
 
		endif !if(j_otype(irg).ne.j_ipreal)  13300
 
	endif !if(irg.ne.iout)  13296
	!io=io+narg+3
	return
end subroutine j_copy !subroutine j_copy(iob,io)
 
subroutine j_copy2(irg,iout)   !makes a copy of an object utility function
	integer,intent(in)::irg
	integer,intent(out)::iout
 
 
	if(irg.ne.iout)then
		!20141219 oli: if(otype(iout).ne.0)call del(iout)
		if(j_otype(iout).ne.j_ipreal)call j_del(iout) !
		if(j_err)return
		if(j_otype(irg).eq.j_ipchar)then
			allocate(j_o(iout)%i( 1:ubound(j_o(irg)%i,1)))
			j_o(iout)%i=j_o(irg)%i
			j_o(iout)%i(3)=irg   !links to char constant
			j_otype(iout)=j_ipchar
			return
		endif !if(j_otype(irg).eq.j_ipchar)  13338
		!20141219 oli: if(otype(irg).ne.0)then
		if(j_otype(irg).ne.j_ipreal)then
 
			if(allocated(j_o(irg)%r))then
				allocate(j_o(iout)%r( lbound(j_o(irg)%r,1):ubound(j_o(irg)%r,1) ));j_o(iout)%r=j_o(irg)%r
			end if !if(allocated(j_o(irg)%r))  13348
			if(allocated(j_o(irg)%i))then
				allocate(j_o(iout)%i( lbound(j_o(irg)%i,1):ubound(j_o(irg)%i,1)));j_o(iout)%i=j_o(irg)%i
			end if !if(allocated(j_o(irg)%i))  13351
			if(allocated(j_o(irg)%i2))then
				allocate(j_o(iout)%i2( lbound(j_o(irg)%i2,1):ubound(j_o(irg)%i2,1)));j_o(iout)%i2=j_o(irg)%i2
			end if !if(allocated(j_o(irg)%i2))  13354
			if(allocated(j_o(irg)%d))then
				allocate(j_o(iout)%d( lbound(j_o(irg)%d,1):ubound(j_o(irg)%d,1)));j_o(iout)%d=j_o(irg)%d
			end if !if(allocated(j_o(irg)%d))  13357
			if(allocated(j_o(irg)%ch))then
				allocate(j_o(iout)%ch( lbound(j_o(irg)%ch,1):ubound(j_o(irg)%ch,1)));j_o(iout)%ch=j_o(irg)%ch
			end if !if(allocated(j_o(irg)%ch))  13360
			j_otype(iout)=j_otype(irg)
 
		else !if(j_otype(irg).ne.j_ipreal)then
			!	call j_getname(iout,irg)
			!	write(6,*)j_oname(1:j_loname),' ',j_oname2(1:j_loname2)
			j_v(iout)=j_v(irg)
 
		endif !if(j_otype(irg).ne.j_ipreal)  13346
 
	endif !if(irg.ne.iout)  13334
	!io=io+narg+3
	return
end subroutine j_copy2 !subroutine j_copy2(irg,iout)
 
 
 
subroutine j_getobsiv(iobs,iomat,ivkeep,iviobs) !,iotrans,iviobs)  !getobs used in JLP, faster than getob
	integer,intent(in):: iobs
	integer,intent(in):: iomat
	integer,intent(in):: ivkeep
	!	integer,intent(in):: iotrans
	integer,intent(in):: iviobs
	integer*8:: iba
 
	!j_o(ivout)%i(1)=ndim1;   j_o(ivout)%i(2)=ndim2;j_o(ivout)%i(3)=ndim1*ndim2;j_o(ivout)%i(4)=itype
	ityp=j_o(iomat)%i(4)
	nkeep=j_o(ivkeep)%i(1)
	if(j_o(iomat)%i(4).eq.j_matreg)then
		iba=(iobs-1)*nkeep
		j_v(j_o(ivkeep)%i2(1:nkeep))=j_o(iomat)%d(iba+1:iba+nkeep)
		! elseif(j_o(iomat)%i(4).lt.0)then  !from da file
		! ! i(6) the number of rows in the intial part
		! ! i(7) first row in the initial part
		! ! i(8) last row in intial part , note there may be an unused are
		! ! i(9) the row which is stored after i(10)
		! ! i(11) the row which is stored after i(12)
 
		! if(iobs.ge.j_o(iomat)%i(7).and.iobs.le.j_o(iomat)%i(8))then
		! iba=(iobs-j_o(iomat)%i(7))*nkeep
		! j_v(j_o(ivkeep)%i(1:nkeep))=j_o(iomat)%r(iba+1:iba+nkeep)
		! else
		! read(-j_o(iomat)%i(4),rec=iobs,err=90)j_v(j_o(ivkeep)%i(1:nkeep))
		! !		read(-j_o(ivmat)%i(4),rec=iob)j_v(j_o(ivkeep)%i(1:nkeep))
		! endif
 
	else !if(j_o(iomat)%i(4).eq.j_matreg)then
		write(6,*)'*j* illegal matrix type in getobsiv'
		j_err=.true.
	endif !if(j_o(iomat)%i(4).eq.j_matreg)  13390
	if(iviobs.gt.0)j_v(iviobs)=iobs
	!	if(iotrans.gt.0)call dotrans(iotrans,1)
 
 
	return
 
	return
 
end subroutine j_getobsiv !subroutine j_getobsiv(iobs,iomat,ivkeep,iviobs)
 
double precision function j_quad(x,x0,x1,x2,y0,y1,y2) !*qudratic interpolation for point x, Newton forward-form
	double precision, intent(in) :: x,x0,x1,x2,y0,y1,y2
	double precision c1,c2
 
	c1=(y1-y0)/(x1-x0)
	c2=(y2-y0-c1*(x2-x0))/((x2-x0)*(x2-x1))
	j_quad=y0+c1*(x-x0)+c2*(x-x0)*(x-x1)
	return
end function !double precision function j_quad(x,x0,x1,x2,y0,y1,y2)
 
! integer function j_iprintin(iob,idef) !idef default for print->
! integer,intent(in):: iob,idef
! !parmod
! ! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
! !end module parmod
! !use transmod
 
! !module vmod
! !end module vmod
! j_iprintin=j_igetopt(iob,j_mprint)
! if(j_iprintin.le.0)then
 
! j_iprintin=idef
 
! endif !if(j_linkoption(iob,io,j_mprint).gt.0)then
! return
! end function !integer function j_iprintin(iob,idef)
 
integer function j_iprintout(iob,idef)
	integer, intent(in) :: iob,idef
	!parmod
	! integer ionames,ioreal,iochar,iocharv,iocconst,iocursor,ioval    !,iotempdata
	!end module parmod
	!use transmod
 
	!module vmod
	!end module vmod
	if(j_linkoption(iob,io,j_mprint).gt.0)then
		if(j_o(iob)%i( j_linkoption(iob,io,j_mprint) ).gt.0)then
			j_iprintout=j_v( j_o(iob)%i( j_linkoption(iob,io,j_mprint)+1 ) )
		else !if(j_o(iob)%i( j_linkoption(iob,io,j_mprint) ).gt.0)then
			j_iprintout=idef
		endif !if(j_o(iob)%i( j_linkoption(iob,io,j_mprint) ).gt.0)  13460
	else !if(j_linkoption(iob,io,j_mprint).gt.0)then
		j_iprintout=j_v(j_ivprintoutput)
	endif !if(j_linkoption(iob,io,j_mprint).gt.0)  13459
	return
end function !integer function j_iprintout(iob,idef)
 
! subroutine j_printtitle(nu,iv)  !write  %%title of object iv to unit nu
! integer,intent(in) :: nu,iv
 
! if(j_otitle(iv).ne.0) then
! i1=j_o(j_otitle(iv))%i(1)
! i2=j_o(j_otitle(iv))%i(2)
! write(nu,*)(j_o(j_ivnames)%ch(i),i=i1,i2)
! endif !if(j_otitle(iv).ne.0) then
! return
! end subroutine !subroutine j_printtitle(nu,iv)
 
 
subroutine j_objargs(iob,io) !checking if arguments are named objects %%function
	integer,intent(in):: iob,io
	!module vmod
	!end module vmod
 
	do i=1,j_o(iob)%i(io+1)
		if(j_o(iob)%i(io+i+1).gt.j_namedv)then
			write(6,*)'*arguments should be named objects'
			j_err=.true.
			exit
 
 
		endif !if(j_o(iob)%i(io+i+1).gt.j_namedv)  13489
	enddo !i=1,j_o(iob)%i(io+1)  13488
	return
end subroutine !subroutine j_objargs(iob,io)
 
!mathematical subroutines which would belong to matsub.f if they were in fortran 77
recursive subroutine j_quick_sort(array, order)
 
	implicit none
	double precision, dimension(:), intent(inout):: array
	integer, dimension(:), intent(out):: order
 
	! Local variable
	integer i_
 
	do i_ = 1, size(array)
		order(i_) = i_
	enddo !i_ = 1, size(array)  13510
 
	call qsort(1, size(array))
 
	contains
 
	recursive subroutine qsort(il, ir)
 
		integer, intent(in):: il, ir
		integer, parameter:: maxsort = 6
		double precision refval_, tmp_
		integer i_, jj, itmp_
 
		if (ir < il + maxsort) then
			! small arrays
			call swapsort(il, ir)
		else !if (ir < il + maxsort) then
			! qsort recursion
			refval_ = array((il + ir)/2)
			i_ = il - 1
			jj = ir + 1
			do
				! search from the beginning of the search range for element >= refval_
				do
					i_ = i_ + 1
					if (array(i_) >= refval_) exit
				enddo !  13535
				! search from the end of the search range for element <= value_
				do
					jj= jj - 1
					if (array(jj) <= refval_) exit
				enddo !  13540
 
				if (i_ < jj) then
					! Swap
					tmp_ = array(i_)
					array(i_) = array(jj)
					array(jj) = tmp_
					itmp_ = order(i_)
					order(i_) = order(jj)
					order(jj) = itmp_
				else if (i_ == jj) then !if (i_ < jj) then
					i_ = i_ + 1
					exit
				else !if (i_ < jj) then
					exit
				endif !if (i_ < jj)  13545
			enddo !  13533
 
			if (il < jj) call qsort(il, jj)
			if (i_ < ir) call qsort(i_, ir)
		end if !if (ir < il + maxsort)  13525
 
	end subroutine qsort !recursive subroutine qsort(il, ir)
 
	subroutine swapsort(il, ir)
		integer, intent(in):: il, ir
		real tmp_
		integer i_, jj, itmp_
 
		do i_ = il, ir - 1
			do jj = i_+1, ir
				if (array(i_) > array(jj)) then
					!swap
					tmp_ = array(i_)
					array(i_) = array(jj)
					array(jj) = tmp_
					itmp_ = order(i_)
					order(i_) = order(jj)
					order(jj) = itmp_
				endif !if (array(i_) > array(jj))  13574
			enddo !jj = i_+1, ir  13573
		enddo !i_ = il, ir - 1  13572
 
	end subroutine swapsort !subroutine swapsort(il, ir)
 
end subroutine j_quick_sort !recursive subroutine j_quick_sort(array, order)
 
!figure routines
 
 
 
 
 
subroutine j_dbw(mets,y,lkm,n,nmets,bias,sd,sb,sw,pien,suur, &
		bias2,s2out)
		! C laskee variannssikomponentit ym. joukosta muuttujia kun ei vakion lis:
	! C muita selitt:ji:
	! C mets on luokkatunnus, sama luokan sis:ll:
	! C    JOS mets=-1 LASKETAAN bias, sd, sb, sw, ja nollataan sis. vektorit
	! C    MUUTEN LASKETAAN V:LITULOKSIA
	! C y tarkasteltavien muuttujien vektori
	! C lkm muuttujien lukum::r:
	! C n= hvaintojen kokonaismaara (output)
	! C nmets = metsien konaismaara (output)
	! C bias on keskiarvojen vektori, huom. poikkeaa (teoriassa) odotusarvojen
	! C parhaasta estimaatista
	! C sd tavallinen keskihajontojen vektori
	! C sb luokkien v:listen hajontojen vektori
	! C sw luokkien sis. hajontojen vektori HUOM. ei p:de: sd**2=sb**2+sw**2
	! C sb ja sw laketaan Searlen kaavalla s. 478 '(analysis of variance estim
	! C pien minimien vektori
	! C suur maksimien vektori
	! C bias2 keskiarvojen GLS-estimaattivektori (toivottavasti)
	! C apu apuvektori jonka pituus oltava (lkm+1)*(luokkien lukumaara)
	! * mxapu apu-vektorin pituus, jollei tarkpeeksi , bias2 vektoria ei lasketa
	! * s2 -luokkien havaintojen lukumäärän neliöiden summa (tarvitaan, kun
	! * lasketaan artimettisen keskiarvon keksivirhettä)
	! Cccccccccccccccccc
	!      parameter (n150=86736) ! muuttujien maks. m::r:
	! C    t0,sm,st,ta v:litulostaulukoita
	! C    *************************************************************
	! C y-muuttujien lkm  on lkm
	! C      metsik:iden m::r:, hav. kokom::r:   nmets,n
	! C       *****************************************************
	real*8 y(*),bias(*),sd(*),sb(*),sw(*),pien(*),suur(*)
	real*8 bias2(*)
	integer*8 ::n
	!     real*8 t0(n150), ta(n150), sm(n150), st(n150),s2,s2out
	real*8,dimension(:),allocatable:: t0(:), ta(:), sm(:), st(:),apu(:),apu2(:)
	real*8 s2,s2out
	real*8 su,suw
	parameter (mxapu=100000)
	save
	!     data metsv/-1/
	! data t0,ta,sm,st/n150*0.d0,n150*0.d0,n150*0.d0,n150*0.d0/
	! data s2,nm,illu/0.d0,0,1/
 
	if(mets.eq.-2)then !initialization
		if(allocated(t0))deallocate(t0,ta,sm,st,apu)
		allocate(t0(1:lkm),ta(1:lkm),sm(1:lkm),st(1:lkm),apu(1:mxapu))
		t0=0.d0
		ta=0.d0
		sm=0.d0
		st=0.d0
		s2=0.d0
		nm=0
		illu=1
		mxmets=mxapu/(lkm+1)
		napu=mxapu
		metsv=-1
		nmets=1
		n=0
		do  k=1,lkm
			pien(k)=1.7e37
			suur(k)=-1.7e37
		end do ! k=1,lkm  13655
		return
 
	endif !if(mets.eq.-2)  13640
	if(mets.eq.-1)then  !finalization
 
 
		s2=s2+nm**2
		n=n+nm
		!     if(nmets.le.mxmets)then
 
		if(illu+lkm+1.gt.napu)then
			allocate(apu2(1:napu))
			apu2=apu
			deallocate(apu)
			allocate(apu(1:2*napu))
			apu(1:napu)=apu2
			deallocate(apu2)
			napu=2*napu
		endif !if(illu+lkm+1.gt.napu)  13669
 
		apu(illu)=nm
		illu=illu+1
		!     end if
		do k=1,lkm
			!    if(nmets.le.mxmets) then
			apu(illu)=sm(k)/nm
			illu=illu+1
			!     end if
			ta(k)=ta(k)+sm(k)**2/nm
			st(k)=st(k)+sm(k)
			sw(k)=0.
			if(n.gt.nmets)sw(k)=(t0(k)-ta(k))/(n-nmets)
			sb(k)=0.
			if(nmets.gt.1)&
				sb(k)=(ta(k)-st(k)**2/n-(nmets-1.d0)*sw(k))/(n-s2/n)
 
			su=0.d0
			suw=0.d0
			!      if(nmets.le.mxmets)then
			ilpu=1
			do j=1,nmets
				paino=apu(ilpu)/(apu(ilpu)*sb(k)+sw(k))
				suw=suw+paino
				su=su+paino*apu(ilpu+k)
				ilpu=ilpu+lkm+1
			end do !j=1,nmets  13699
			bias2(k)=su/suw
			! !     else
			! bias2(k)=-9999.
			! end if
			if(sw(k).gt.0.)sw(k)=sqrt(sw(k))
			if(sb(k).gt.0.)sb(k)=sqrt(sb(k))
			bias(k)=st(k)/n
			sd(k)=(t0(k)-st(k)**2/n)/(n-1)
			sd(k)=sqrt(sd(k))
			!		write(6,*)' k',sd(k),sw(k),bias(k),sb(k),bias2(k)
			! ta(k)=0.d0
			! sm(k)=0.d0
			! t0(k)=0.d0
			! st(k)=0.d0
 
		end do !k=1,lkm  13682
 
		deallocate(ta,sm,t0,st,apu)
		! illu=1
		! nm=0
		! metsv=-1
		! s2out=s2
		! s2=0.d0
		return
	end if !if(mets.eq.-1)  13662
 
	if(mets.ne.metsv.and.metsv.ne.-1)then
		!     if(nmets.le.mxmets)then
		if(illu+lkm+1.gt.napu)then
			allocate(apu2(1:napu))
			apu2=apu
			deallocate(apu)
			allocate(apu(1:2*napu))
			apu(1:napu)=apu2
			deallocate(apu2)
			napu=2*napu
		endif !if(illu+lkm+1.gt.napu)  13733
		apu(illu)=nm
		illu=illu+1
		do k=1,lkm
			apu(illu)=sm(k)/nm
			illu=illu+1
 
		end do !k=1,lkm  13744
		!    end if
 
		do  k=1,lkm
 
			ta(k)=ta(k)+sm(k)**2/nm
			st(k)=st(k)+sm(k)
			sm(k)=0.d0
		end do ! k=1,lkm  13751
 
		s2=s2+nm**2
		n=n+nm
		nm=0
		!*      if(nmets.gt.mxmets)
		nmets=nmets+1
 
		!*      end if
 
		!   else if(metsv.eq.-1)then
		! n=0
		! nmets=1
		! mxmets=mxapu/(lkm+1)
		! do  k=1,lkm
		! pien(k)=1.7e37
		! suur(k)=-1.7e37
		! end do
 
	end if !if(mets.ne.metsv.and.metsv.ne.-1)  13731
 
	nm=nm+1
	do k=1,lkm
		t0(k)=t0(k)+y(k)**2
		sm(k)=sm(k)+y(k)
		pien(k)=amin1(pien(k),y(k))
		suur(k)=amax1(suur(k),y(k))
	end do !k=1,lkm  13778
	metsv=mets
	return
end subroutine !subroutine j_dbw(mets,y,lkm,n,nmets,bias,sd,sb,sw,pien,suur, &
 
double precision function j_interplane(xi0,x1,xi2,yi0,y1,yi2,zi0,z1,zi2,xa,ya)
	!used to interpolate covariance matrix near digonal
	double precision, intent(in)::xi0,x1,xi2,yi0,y1,yi2,zi0,z1,zi2
	double precision, intent(in)::xa,ya
 
	if(yi0.ne.y1)then !change
		x0=xi0
		! x1=xi1
		x2=xi2
		y0=yi0
		! y1=yi1
		y2=yi2
		z0=zi0
		! z1=zi1
		z2=zi2
	else !if(yi0.ne.y1)then
		x0=xi2
		! x1=xi1
		x2=xi0
		y0=yi2
		! y1=yi1
		y2=yi0
		z0=zi2
		! z1=zi1
		z2=zi0
	endif !if(yi0.ne.y1)  13793
 
	!write(6,*)x0,x1,x2,y0,y1,y2,z0,z1,z2,xa,ya
	!write(6,*)y1-y0,((x2-x0)*(y1-y0)+(x1-x0)*(y2-y0))
	a=((z2-z0)*(y1-y0)-(z1-z0)*(y2-y0))/((x2-x0)*(y1-y0)-(x1-x0)*(y2-y0))
	b=((z1-z0)-a*(x1-x0))/(y1-y0)
	c=z0-a*x0-b*y0
	j_interplane=a*xa+b*ya+c
 
	return
end function !double precision function j_interplane(xi0,x1,xi2,yi0,y1,yi2,zi0,z1,zi2,xa,ya)
subroutine eigen_(matin,n,ndim2,values)
	double precision matin(ndim2,*),values(*)
	!*     .. Parameters ..
	DOUBLE PRECISION   ZERO, ONE
	PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
	!*     ..
	!*     .. Local Scalars ..
	!  LOGICAL            LOWER, LQUERY, WANTZ
	INTEGER            IINFO, IMAX, INDE, INDTAU, INDWRK, ISCALE, &
		LLWORK, LWKOPT, NB
	DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,&
		SMLNUM
	! *     ..
	! *     .. External Functions ..
	!  LOGICAL            LSAME
	INTEGER            ILAENV
	DOUBLE PRECISION   DLAMCH, DLANSY
	!  EXTERNAL           LSAME, ILAENV, DLAMCH, DLANSY
	EXTERNAL           LSAME, ILAENV, DLAMCH, DLANSY
 
	! *     ..
	! *     .. External Subroutines ..
	EXTERNAL           DLASCL, DORGTR, DSCAL, DSTEQR, DSTERF, DSYTRD, &
		XERBLA
	! *     ..
	! *     .. Intrinsic Functions ..
	INTRINSIC          MAX, SQRT
	!integer,dimension(:), pointer::arg=>null()
	double precision ,dimension(:,:), allocatable::a
	double precision ,dimension(:), allocatable::w,work
	CHARACTER          JOBZ, UPLO
	! WANTZ = LSAME( JOBZ, 'V' )  .true. (eigenvalues and vectors)
	! LOWER = LSAME( UPLO, 'L' )  .fasle.
	! LQUERY = ( LWORK.EQ.-1 )  .false.
	! *
	! C logical pr
	! C pr=.false.
 
	! C call j_startfunction(iob,io,j_ipmatrix,narg,arg,ivout)
	! C if(j_err)return
	! C if(ivout.eq.j_ivresult)then
	! C write(6,*)'*eigen must have explicit output'
	! C j_err=.true.
	! C return
 
	! C endif !if(ivout.eq.j_ivresult)then
	! C	n=j_o(arg(1))%i(1)
	! if(n.ne.j_o(arg(1))%i(2))then
	! call j_printname('*eigen: argument ',arg(1),' not a square matrix')
	! j_err=.true.
	! return
	! endif !if(n.ne.j_o(arg(1))%i(2))then
	allocate(a(1:n,1:n),w(1:n))
	LDA=n
	do i=1,n
		do j=i,n
			a(i,j)=matin(i,j)   !j_o(arg(1))%r((i-1)*n+j)
		end do !j=i,n  13879
	end do !i=1,n  13878
	uplo='U'
	jobz='V'
	NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
	LWORK = MAX( 1, ( NB+2 )*N ) !lwkopt
	!write(6,*)'lwkopt,nb',lwk,nb
	allocate( work(1:lwork))
	SAFMIN = DLAMCH( 'Safe minimum' )
	EPS = DLAMCH( 'Precision' )
	SMLNUM = SAFMIN / EPS
	BIGNUM = ONE / SMLNUM
	RMIN = SQRT( SMLNUM )
	RMAX = SQRT( BIGNUM )
	! *
	! *     Scale matrix to allowable range, if necessary.
	! *
	ANRM = DLANSY( 'M', UPLO, N, A, LDA, WORK )
	ISCALE = 0
	IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
		ISCALE = 1
		SIGMA = RMIN / ANRM
	ELSE IF( ANRM.GT.RMAX ) THEN !IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
		ISCALE = 1
		SIGMA = RMAX / ANRM
	END IF !IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN )  13900
	IF( ISCALE.EQ.1 ) &
		CALL DLASCL( UPLO, 0, 0, ONE, SIGMA, N, N, A, LDA, INFO )
	! *
	! *     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
	! *
	INDE = 1
	INDTAU = INDE + N
	INDWRK = INDTAU + N
	LLWORK = LWORK - INDWRK + 1
	!			write(6,*)'llwork',llwork
	CALL DSYTRD( UPLO, N, A, LDA, W, WORK( INDE ), WORK( INDTAU ),&
		WORK( INDWRK ), LLWORK, IINFO )
	! *
	! *     For eigenvalues only, call DSTERF.  For eigenvectors, first call
	! *     DORGTR to generate the orthogonal matrix, then call DSTEQR.
	! *
	! IF( .NOT.WANTZ ) THEN
	! CALL DSTERF( N, W, WORK( INDE ), INFO )
	! ELSE
	CALL DORGTR( UPLO, N, A, LDA, WORK( INDTAU ), WORK( INDWRK ),&
		LLWORK, IINFO )
	CALL DSTEQR( JOBZ, N, W, WORK( INDE ), A, LDA, WORK( INDTAU ),&
		INFO )
	! *      END IF
	! *
	! *     If matrix was scaled, then rescale eigenvalues appropriately.
	! *
	IF( ISCALE.EQ.1 ) THEN
		IF( INFO.EQ.0 ) THEN
			IMAX = N
		ELSE !IF( INFO.EQ.0 ) THEN
			IMAX = INFO - 1
		END IF !IF( INFO.EQ.0 )  13935
		CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
	END IF !IF( ISCALE.EQ.1 )  13934
	! C	call j_defmatrix(ivout,'%matrix',n,n,j_matreg,.false.,ivmat)
	! C	do i=1,n
	! C		do j=1,n
	! C			j_o(ivmat)%r((i-1)*n+j)=a(i,j)
	! C		enddo !do j=1,n
	! C	enddo !do i=1,n
	! C	if(pr)call j_printname('eigenvectors stored in matrix ',ivmat,' ')
	! C	call j_defmatrix(ivout,'%values',1,n,j_matreg,.false.,ivmat)
	! C	j_o(ivmat)%r(1:n)=w(1:n)
	values(1:n)=w(1:n)
	! C	if(pr)call j_printname('eigenvalues stored in matrix (row vector) ',ivmat,' ')
	if(info.gt.0)write(6,*)'*eigen did not converge!!!!!'
	return
 
 
end subroutine eigen_ !subroutine eigen_(matin,n,ndim2,values)
 
!
recursive subroutine j_parser(input,ivteku,ivtext)
	character*(*),intent(in):: input
	integer,intent(in)::ivteku
	integer,intent(in),optional::ivtext
	!	integer, dimension (:), pointer :: teku
	integer:: ntekumax
	integer::ider
	integer:: nteku
	logical ::oneline
	logical ::nexist
	character*8::const
	!	integer,dimension(1000)::teku=0
	!	integer,dimension(3000)::teku0=0
	integer,dimension(30)::tekuout=0
	integer,parameter::maxnode=5000
	integer, dimension(maxnode):: node,mother,child,sister,nchild,lastchild,levels,nodetoteku
	!	logical,dimension(5000)::islabel
	integer,dimension(1000)::optionloc,optionmother
	integer,dimension(20)::optionlocout !locations in output
	integer,dimension(1000)::optionloc2,optionmother2
	integer,dimension(maxnode)::nchildopt,brother !number of optionchildren
	!brother is on the left sister on the right
	!	integer, dimension(500):: node2,mother2,child2,sister2,nchildopt,lastchild2 !,levels2,nodetoteku
	!integer,dimension(500)::mother,child,sister,node,nchild
	!	logical::p,p2
	integer,parameter::lenwinput=4000
	character*(lenwinput) :: winput  !working input
	character*(lenwinput) ::winput2
	!	character*200 ::winput3
	character*40 ::winass
	logical ::oneout,onein,printout,printout2
	integer::ivinl,ivoutl,ivsource
	integer ::ndo,nifthen
	integer,parameter::maxdo=100
	integer,parameter::maxifthen=100
	integer,parameter::maxifthen2=100
	integer,parameter::maxgotos=200
	integer::ngotos
	character*10,dimension(maxdo+maxifthen)::label
	integer,dimension(maxgotos)::gotos
	integer,dimension(maxdo+maxifthen)::ilabel  !place of label
	integer,dimension(maxdo+maxifthen)::ivlabel  !v
	integer,dimension(maxifthen,maxifthen2)::ioifgoto
	integer,dimension(maxifthen)::ioifthen
	integer,dimension(maxifthen)::nifgoto   ! 1+numer of elseifgotos
	logical,dimension(maxifthen)::iselse
	integer,dimension(maxdo)::iodo
	integer,dimension(50,2)::dosec
	integer,dimension(2000)::outlist,inlist  !,outlist2,inlist2
	logical ::isoutput  !onein2, one input but after transformations
	logical isinout,isarg
	!integer,dimension(maxdo)::ioelse
	logical isdo,isif,isifthen,isif2,isder,isder2,issetel  !,isgoto,isgoto2
	logical,dimension(:),allocatable::used
	logical ::istext
	!integer, dimension(:),allocatable::list
	!integer::nmax=200
	integer::ifuif
	!logical::p3
	integer ::ila
 
 
	!logical::inoutput
	!	call j_getname(ivteku)
	!	write(6,*)'<505 ',j_oname(1:j_loname),'  ',input
	!!! SIMULAATTORIIN jispendtrt
	j_p=j_v(j_ivdebug).ge.201.d0
	j_p2=j_v(j_ivdebug).ge.200.d0  !print but less
	j_p3=j_v(j_ivdebug).ge.199.d0
	!	write(6,*)'<464646ivteku ',ivteku
	!	write(6,*)' allocated list',allocated(list)
	!if(.not.allocated(list))allocate(list(1:nmax))
	!write(6,*)'<59959'
	! p=.false.
	! p2=.false.   !print but less
	! p3=.false.
	nexist=.false.
	istext=present(ivtext)
	lop=j_lentrim(input)
	!write(6,*)'<59959 startinterp ',oneline,input ,' ivteku ',ivteku
	if(j_p)write(6,*)'spec functions:',	'setoption','getelem','setelem','list2', 'o1_funcs'
	!write(6,*)'<53535 ',ivteku,size(j_o(ivteku)%i),shape(j_o(ivteku)%i)
	ntekumax=size(j_o(ivteku)%i)-1
	!	teku=>j_o(ivteku)%i(1:ntekus-1)
	!	nteku=>j_o(ivteku)%i(0)
	nteku=0
	!write(6,*)'<74747',j_o(ivteku)%i(0)
	leteku=0
	winput=' '
	ndo=0
	nifthen=0
	!write(6,*)'inpu ',lop,index(input(1:lop),'in->')
	!write(6,*)'**input****',input
	if(j_p2)write(6,'(20i5/)')j_o(ivteku)%i(1:max(nteku,1))
	ivinl=j_o(ivteku)%i2(1)
	ivoutl=j_o(ivteku)%i2(2)
	ivarg=j_o(ivteku)%i2(3)
	ivlocal=j_o(ivteku)%i2(4)
	isarg=ivarg.ne.0
	isder=.false.
	if(j_p)write(6,*)'<66 isarg,ivarg ',isarg,ivarg,input
	isinout=ivinl.gt.0.and.ivoutl.gt.0
	isder2=.false.
	isder=.false.
	nlabel=0
	ngotos=0
	ilabel=0
	ndosec=0
 
	if(input.eq.'trans')then
		write(6,*)' '
		oneline=.false.
		j_inpara=.true.
		ip6=6
		ider=0 !link to Der(
 
		ivsource=j_o(ivteku)%i2(11)
		if(j_p)write(6,*)'<19outputlistl ',ivoutl,' source ',ivsource
		ndo=0
 
		ifthen=0
 
 
 
		call j_getname(ivteku)
		!j_tempchar2(2:25)=j_vname(ivteku)
 
		!letr=len_trim(j_tempchar2(1:25))
		j_varname1(1:j_loname+1)=j_oname(1:j_loname)//'|'
		!j_tempchar2(letr+1:letr+1)='$'
		letr=j_loname+1
		nline=0
		!	nline2=10000
		!	nline=0
		if(j_ninc.gt.1)then
			!search all labels
	1234			continue
	! get first everything into source

			if(istext)then
				call j_getline(ivtext,nline+1,j_inp,j_linp)
				if(j_err)return
				write(6,*)'<',j_inp(1:j_linp)
				call j_clean(j_inp(1:j_linp),j_linp)
				!			write(6,*)'<<',j_inp(1:j_linp)
			else
				call j_getinput('tr> ',inprint,single=.true.)
				if(j_err)return
				!				write(21,*)j_linp,j_inp(1:j_linp)
			endif !if(istext)  14099
			!		write(6,*)'<466464 getinputhere ',j_inp(1:j_linp)
 
			call j_parent(j_inp,j_linp) !call j_parent(input(1:lopw))  !check parenthesis
			if(j_err)goto 999
			call j_puttext(ivsource,j_inp(1:j_linp))
			nline=nline+1
 
 
			ikp=index(j_inp(1:j_linp),':')
			ihipsu=index(j_inp(1:j_linp),"'")
			if(ikp.gt.0)then
				ikp2= j_nextlim(j_inp,ikp+1,j_linp,':')
				if(ikp2.le.j_linp.and.ihipsu.eq.0)then
					write(6,*)'*there cannot be two : in the same line'
					j_err=.true.
					return
				endif !if(ikp2.le.j_linp.and.ihipsu.eq.0)  14122
			endif !if(ikp.gt.0)  14120
 
			if(ikp.gt.0.and..not.(ihipsu.gt.0.and.ihipsu.lt.ikp))then
				ila=j_isin(j_inp(1:ikp-1),label,nlabel)
				if(ila.gt.0)then
					if(ilabel(ila).ne.0)then
						write(6,*)'label ',winput(i:ikp),' already defined'
						j_err=.true.;goto 999
					endif !if(ilabel(ila).ne.0)  14132
 
				else !if(ila.gt.0)then
					nlabel=nlabel+1
					label(nlabel)=j_inp(1:ikp-1)
					!	ilabel(nlabel)=nline
					!		iv=j_object(j_varname1(1:letr)//j_inp(1:ikp-1))
					!		if(iv.le.0)call j_getobject(0,j_varname1(1:letr)//j_inp(1:ikp-1),j_ipreal,iv)
					!		j_v(iv)=nlabel
					!		ivlabel(nlabel)=iv
 
					!	write(6,*)'label ',label(nlabel),' line ',nline
				endif !if(ila.gt.0)  14131
 
 
 
			endif !if(ikp.gt.0.and..not.(ihipsu.gt.0.and.ihipsu.lt.ikp))  14129
 
			!	write(6,*)'<456>',j_inp(1:1)
 
			if(j_inp(1:1).ne.'/')goto 1234 !continue getting source
			j_inpara=.false.
			!		write(6,*)'tas'
		endif !if(j_ninc.gt.1)  14094
 
 
		!start
		if(isarg)then
 
			!write(6,*)'pref',j_tempchar2(1:letr),ivarg,j_iplist
			! write(6,*)j_otype(ivarg)
			! write(6,*)j_o(ivarg)%i
			! write(6,*)j_o(ivarg)%i2
			if(allocated(used))deallocate(used)
			allocate(used(1:j_o(ivarg)%i(1)))
			used=.false.
		endif !if(isarg)  14162
		nline2=100000
	else !if(input.eq.'trans')then
		!		write(6,*)'<65',lop,input
 
 
		oneline=.true.
		nline=1
		nline2=1
		if(j_ninc.eq.1)then
			ip6=3
		else
			ip6=6
		endif !if(j_ninc.eq.1)  14180
		lopw=lop
		!	if(input(lop:lop).eq.';')lopw=lopw-1
 
		!		winput(1:lopw)=input(1:lopw)
 
 
		!		write(6,*)'<58',lopw,winput(1:lopw)
		!	call j_parent(winput,lopw)  !check parenthesis
		!		write(6,*)'<59',lopw,winput(1:lopw)//'/'
 
		!lopw=len(input)
		winput(1:lopw)=input
		call j_parent(winput,lopw)
		if(j_err)goto 999
		!	write(6,*)'<56oneline',oneline,winput(1:lopw)
		if(j_err)goto 999
		if(winput(1:3).eq.'do('.or.winput(1:5).eq.'enddo'.or.winput(1:5).eq.'goto('.or. &
			winput(max(lop-4,1):lopw).eq.')then'.or.winput(1:lop).eq.'else'.or. &
			winput(1:7).eq.'elseif('.or.winput(1:5).eq.'endif'.or.winput(1:6).eq.'while(' &
				.or.winput(1:lop).eq.'endwhile')then
			write(6,*)'illegal at command level'
			j_err=.true.;goto 999
		endif !if(winput(1:3).eq.'do('.or.winput(1:5).eq.'enddo'.or.winpu  14201
 
	endif !if(input.eq.'trans')  14069
 
 
	if(winput(1:1).eq.',')then
		write(6,*)'*transformation cannot start with ,'
		j_err=.true.
		return
 
	endif !if(winput(1:1).eq.',')  14212
	isdo=.false.;isif=.false.;iselse=.false.;isifthen=.false.;isif2=.false.
	nchildopt=0
	issetel=.false.
	!	nline2=nline
	!if(nline2.eq.0)nline2=100000
	mainloop:	do iline=1,nline2

		if(j_err)goto 999
		itemporalv=itemporal
		if(.not.isder)itemporal=j_mxnamedv
		if(.not.oneline)then
			!transformation object
			if(nteku.gt.0)then
				if(j_o(ivteku)%i(nteku).lt.0)then
					call teku(nteku,j_o(ivteku)%i(nteku)-1)
				else !if(teku(nteku).lt.0)then
					nteku=nteku+1
					call teku(nteku,-1)
				endif !if(j_o(ivteku)%i(nteku).lt.0)  14231
			endif !if(nteku.gt.0)  14230
 
			if(isder2)then
				call endder(0)
			endif !if(isder2)  14239
 
			!	if(.not.oneline)write(6,*)'<7w777>befget oneline ',oneline
			if(.not.oneline)then
				if(j_ninc.eq.1)then
					call j_getinput('tr> ',inprint,single=.true.)
					if(j_err)return
				else
					! getting fro source
					call j_getline(ivsource,iline,j_inp,j_linp) ! call j_getinput('trans>',inprint)
 
					!			write(6,*)'*778',iline,j_linp,j_inp(1:j_linp)
					!			j_o(j_inciv( j_ninc))%i(6)
				endif !if(j_ninc.eq.1)  14245
			endif !if(.not.oneline)  14244
 
			!write(6,*)'<545454oneline',oneline,j_linp,j_inp(1:j_linp)
			call j_parent(j_inp,j_linp) !call j_
			if(j_err)return
			!		if(j_ninc.eq.1)then
 
			!write(6,*)'<66666',j_inp(1:j_linp)
			!		write(6,*)'alus,isder,isder2',isder,isder2>
 
			ikp=index(j_inp(1:j_linp),':')
			ihipsu=index(j_inp(1:j_linp),"'")
			if(j_ninc.eq.1.and.ikp.gt.0.and..not.(ihipsu.gt.0.and.ihipsu.lt.ikp))then
				write(6,*)'labels allowed only in include files'
 
				write(6,*)j_inp(1:j_linp)
				j_err=.true. ;return
			endif !if(j_ninc.eq.1.and.ikp.gt.0.and..not.(ihipsu.gt.0.and.ihip  14267
			if(ivsource.ne.0)then
 
				if(j_ninc.eq.1)call j_puttext(ivsource,j_inp(1:j_linp))
				call j_putoi2(ivsource,j_o(ivsource)%i(0),nteku)
			endif !if(ivsource.ne.0)  14273
			!		else
			!		call j_getline(ivsource,iline,j_inp,j_linp)
			!		ikp=index(j_inp(1:j_linp),':')
			!		ihipsu=index(j_inp(1:j_linp),"'")
 
 
			if(j_p)write(6,*)'****GOTINP ',j_inp(1:j_linp)
			if(j_p)write(6,'(a,(20i5/))')'<tekuhere>',j_o(ivteku)%i(1:max(nteku,1))
			!	if(j_p)write(6,*)'   isgoto ',isgoto
			!		islabel=.false.
			!	write(6,*)'<466464isder',isder,isder2
 
			if(isder)then  !previous was der(
 
				nder=j_o(ivteku)%i(ider+1)
				!	write(6,*)'<76ider,nder',ider,nder
 
				call j_getder(ivfunc,j_o(ivteku)%i(ider+2:ider+1+nder),j_o(ivteku)%i(nteku+1:nteku+nder),nder)
				!teku(ider:ider+5)
				do ina=1,nder
					nteku=nteku+1
					!		call j_getline(j_ivnames,teku(ider+ina+1),j_tempchar2,le)
					!		ivo=j_object('d['//j_tempchar2(1:le)//']')
					!		if(ivo.le.0)call  j_getobject(0,'d['//j_tempchar2(1:le)//']',j_ipreal,ivo)
					!		teku(nteku)=ivo
					!		write(6,*)'<777',nteku,ivarg, 'isder2 TRUE'
					call j_putoutput(j_o(ivteku)%i(nteku),ivinl,ivoutl,ivarg)
				enddo !ina=1,nder  14297
 
				!			isder=.false.
				isder2=.true.
				itemporal=j_mxnamedv+j_mxtemporalv0
 
 
			elseif(j_inp(1:4).eq.'der(')then !if(isder)then
				ider=nteku+1
				isder=.true.
				if(oneline)then
					write(6,*)'derivative allowed only in transformations'
					j_err=.true.
					goto 999
 
				endif !if(oneline)  14315
				!look the output from the next line
				call j_getline(ivsource,iline+1,j_inpr2,linpr2)
 
				!		write(6,*)'<??',j_inpr2(1:linpr2)
				call j_clean(j_inpr2(1:linpr2),linpr2)
				ifst=j_nonblank(j_inpr2(1:linpr2),1,linpr2)
				ilo=j_nextlim(j_inpr2(1:linpr2),ifst,linpr2,'=')
				!		write(6,*)'<3#>',iiv,ilnext,linpr2,ifst,ilo,j_o( iiv)%txt(ilnext)(1:20)
				if(ilo.gt.linpr2)then
					write(6,*)'der() should be followed by func='
					j_err=.true. ;return
				endif !if(ilo.gt.linpr2)  14329
				!		write(6,*)'<98? ',j_inpr2(ifst:ilo-1)
				ivfunc=j_getobject(0,j_inpr2(ifst:ilo-1),j_ipreal)
				if(j_err)return
			endif !if(isder)  14290
 
 
			! if(j_p2)write(6,*)' '
			!			write(6,*)'<44got*****nteku ',nteku,' ************ ',j_inp(1:j_linp)
			!			write(6,'(a,(20i5/))')'<teku44ere>',teku(1:nteku)
			! if(j_p2)write(6,'(20i5/)')teku(1:nteku)
			ikp=index(j_inp(1:j_linp),':')
			!write(6,*)'<777ikp',ikp
			ihipsu=index(j_inp(1:j_linp),"'")
			!		write(6,*)'35635>',j_inp(1:j_linp)
 
			if(j_inp(1:j_linp).eq.'/')then
				!	j_inpara=.false.
				!		write(6,*)'<46646464647'
				if(ndo.gt.0)then
					write(6,*)'there were ',ndo,' open do()'
					j_err=.true.;goto 999
				endif !if(ndo.gt.0)  14351
				if(nifthen.gt.0)then
					write(6,*)'there were ',ndo,' open if()then'
					j_err=.true.;goto 999
				endif !if(nifthen.gt.0)  14355
 
				if(isarg)then
					do ii=1,j_o(ivarg)%i(1)
						if(.not.used(ii))call j_printname('*wrn*, object ',j_o(ivarg)%i2(ii), &
							' was in arg-list but was not used')
					end do !ii=1,j_o(ivarg)%i(1)  14361
				endif !if(isarg)  14360
				!		write(6,*)'<66ngotos',ngotos,ila,i
				!	write(6,'(20i5/)')teku(1:nteku)
 
				! do il=1,nlabel
				! write(6,*)'label ',il,label(il)
 
				! enddo
				ngo2=0
				! do i=1,ngotos
				! !	write(6,*)'dosec',dosec(1,1:2)
				! !		write(6,*)'**igoto ',gotos(i),teku(gotos(i)+1),ilabel(teku(gotos(i)+1))
				! ! if(j_o(ivteku)%i(gotos(i)).eq.j_fgoto2)then
				! ! !		write(6,*)'goto2 '
				! ! narg=j_o(ivteku)%i(gotos(i)+1)
				! ! do ig=1,narg-1
				! ! !	iv=j_o(ivteku)%i(gotos(i)+1+ig)
				! ! !		write(6,*)'iv ',iv
				! ! !	iil=j_v(iv)
				! ! call teku(gotos(i)+1+ig,ilabel(iil))
				! ! enddo !ig=1,narg-1  13190
 
				! ! else
				! iv=j_o(ivteku)%i(gotos(i)+1)
				! ilo=j_intloc(ivlabel,nlabel,iv)
 
				! if(ilo.gt.0)then
				! !	iil=j_v(iv)
				! call teku(gotos(i)+1,ilabel(iil))   !teku(gotos(i)+1))
				! else
				! call teku(gotos(i),j_fgoto3)
				! if(j_p)write(6,*)'gsgs6',j_o(ivteku)%i(gotos(i)+1)
				! ngo2=ngo2+1
				! endif !if(ilo.gt.0)  13201
 
				! if(ngo2.gt.0)then
				if(nlabel.gt.0)then
					call teku( nteku+2,nlabel)
					call teku(nteku+2+nlabel,ilabel(nlabel))  !last first
					if(nlabel.gt.1)j_o(ivteku)%i(nteku+3:nteku+1+nlabel)=ilabel(1:nlabel-1)
					!	call teku(nteku+3:nteku+2+nlabel,ilabel(1:nlabel))
				endif !if(nlabel.gt.0)  14401
				! endif !if(ngo2.gt.0)  13210
				! endif !if(j_o(ivteku)%i(gotos(i)).eq.j_fgoto2)  13187
				! enddo !i=1,ngotos  13184
				! !		write(6,*)'exit'
				exit
 
			endif !if(j_inp(1:j_linp).eq.'/')  14348
 
 
			! if then structures:
			! nifthen the number of ifthen
			! if()then  nifthen=nifthen+1  ioifhten(nd) links to the place of if(
			! in if(  ) first io after if( is the place where to jump if condition is not satidfied
			!            then is the index of the variable telling if the conditon is satified
			!            then the place where to jump if conditon is satisfied
			!after if()then there can be elseif() else or endif
			!if there is elseif then ioifhten(nifthen) is used to put first argument of if to link to this plcace
			!if there is else or elseif then ioifgoto gives the place where to put the jump
			! label after endif
			! nifgoto(nifthen) gives the number of thes else or eleseif structures
 
			isdo=.false.
			!		write(6,*)'<5353here',oneline,j_inp(1:j_linp)
 
			if(j_inp(1:j_linp).eq.'endif')then
				if(nifthen.le.0)then
					write(6,*)'endif without open if..then'
					write(6,*)'line ',iline
					j_err=.true.;goto 999
 
				endif !if(nifthen.le.0)  14432
 
				!is goto label needed
				!	write(6,*)'endif ',nifthen,nifgoto(nifthen),iselse(nifthen),ioifthen(1:nifthen)
				!if(.not.iselse(nifthen))teku(ioifthen(nifthen,nifg oto(nifthen))+1)=nteku+1
				! if previous if  then is not satisfied put jump label
				do j=1,nifgoto(nifthen)
					!		write(6,*)'into',ioifgoto(nifthen,j)+1,nteku+1
					call teku(ioifgoto(nifthen,j)+1,nteku+1)   !goto adrress after doing sections
				enddo !j=1,nifgoto(nifthen)  14443
				!		io=j_o(iob)%i(io+1)  !condition not satisfied
				if(ioifthen(nifthen).gt.0)call teku(ioifthen(nifthen)+1,nteku+1)
				if(j_p2)write(6,*)'after endif neku and teku',nteku
				if(j_p2)write(6,'(20i5/)')j_o(ivteku)%i(1:nteku)
				!teku(ifuif+1)=nteku+1	;isif2=.false. !not neede?
				nifthen=nifthen-1
				cycle
 
			elseif(j_inp(1:j_linp).eq.'cycle'.or.j_inp(1:j_linp).eq.'exitdo')then !if(j_inp(1:j_linp).eq.'endif')then
				write(6,*)'exitdo and cycle are deleted features, use goto'
				j_err=.true.;goto 999
 
				! if(j_v(j_o(iob)%i(io+2)).eq.j_0)then
				! io=j_o(iob)%i(io+1)  !condition not satisfied
				! else !if(j_v(j_o(iob)%i(io+2)).eq.j_0)then
				! io=j_o(iob)%i(io+3)   !condition satified
				! endif !if(j_v(j_o(iob)%i(io+2)).eq.j_0)   3881
 
 
			elseif(j_inp(1:j_linp).eq.'else')then !if(j_inp(1:j_linp).eq.'endif')then
				!put goto and the place to put the label of goto to ioifgoto
				!	call tekut(teku,nteku
				!subroutine else()
				!	write(6,*)'else',j_fbloop+7,nteku+1,'nifgoto(nifthen)',nifgoto(nifthen),ioifthen(nifthen)
				!	write(6,*)'befogoto,nifthen,ioifthen ',nifthen,ioifthen(nifthen)
				!	write(6,'(20i5)')teku(1:nteku)
				!	ntek=ntekuf(3)
				if(nifthen.le.0)then
					write(6,*)'*else even if there are no open if-thens'
					write(6,*)'line ',iline
					j_err=.true.
					return
				endif !if(nifthen.le.0)  14474
				call teku(nteku+1,j_fgoto)  !bloop+7 )   !goto  nteku+2 reserved for
				call teku(nteku+2,0)  !no arguments
				!	write(6,*)'goto',nteku+1,j_fbloop+7
 
				!	write(6,*)'nifthen ',nifthen, 'ioifthen',ioifthen(nifthen)
				call teku(ioifthen(nifthen)+1,nteku+4)
				nifgoto(nifthen)=nifgoto(nifthen)+1 !increase counter of needed goto labels
				!	write(6,*)'nifthen, nifgoto(nifthen)',nifgoto(nifthen)
				ioifgoto(nifthen,nifgoto(nifthen))=nteku+2
				ioifthen(nifthen)=0  !no more ifs to be update
				nteku=nteku+3
				!ioelse(nifthen)=nteku from previous if(then or elseif
				!	write(6,'(20i5/)')j_o(ivteku)%i(1:nteku)
 
				if(j_p2)write(6,*)'aftelse nteku',nteku,' putto',ioifthen(nifthen)+1
				if(j_p2)write(6,'(20i5/)')j_o(ivteku)%i(1:nteku)
 
				ioifthen(nifthen)=0  !there is no if8 to be updted at the end
				cycle
			endif !if(j_inp(1:j_linp).eq.'endif')  14431
 
 
			if(j_linp.gt.lenwinput)then
				write(6,*)'*j* increase lenwinput'
				j_err=.true.;return
 
 
			endif !if(j_linp.gt.lenwinput)  14502
			winput(1:j_linp)=j_inp(1:j_linp)
			!		write(6,*)'goto9',winput(1:j_linp)
			lopw=j_linp
 
			!start changing
 
			if(ikp.gt.0.and..not.(ihipsu.gt.0.and.ihipsu.lt.ikp))then
				ila=j_isin(j_inp(1:ikp-1),label,nlabel)
				! if(ila.gt.0)then
				! if(ilabel(ila).ne.0)then
				! write(6,*)'label ',j_inp(i:ikp),' already defined'
				! j_err=.true.;return
				! endif !if(ilabel(ila).ne.0)then
 
				! else !if(ila.gt.0)then
				! nlabel=nlabel+1
				! label(nlabel)=j_inp(1:ikp-1)
				! ila=nlabel
				! endif !if(ila.gt.0)then
				ilabel(ila)=nteku
				!label of the label
				!				write(6,*)'<434putilabel',ila,nteku,'label ',j_inp(1:ikp-1)
				!				write(6,*)'labels ',label(1:nlabel)
				if(ikp.eq.lopw)cycle
				winput2(1:lopw-ikp)=winput(ikp+1:lopw)
				lopw=lopw-ikp
				winput(1:lopw)=winput2(1:lopw)
				!isgoto=j_inp(1:5).eq.'goto('
			endif !if(ikp.gt.0.and..not.(ihipsu.gt.0.and.ihipsu.lt.ikp))  14514
			! isgoto=.false.
			! if(j_inp(1:5).eq.'goto(')isgoto=.true.
			!	isgoto=winput(1:5).eq.'goto('
			! !	write(6,*)'getgoto88'
			! isgoto=.true.
			! !	ialgoto=6
			! !	call getgoto()
			! !	if(j_err)return
			! !	cycle
			! endif !if(j_inp(1:5).eq.'goto(')then
 
			!write(6,*)'tassa'
 
		endif !if(.not.oneline)  14228
		!	write(6,*)'tassapas',winput(1:lopw),oneline
		!write(6,*)'isgototas ',isgoto
		! let us treat all thos command which can be done without further interpretation
		if(j_nextlim(winput(1:lopw),1,lopw,'=').gt.lopw.and.winput(lopw:lopw).eq.';'.and.&
				winput(1:1).ne.'{')then
 
			if(winput(lopw-1:lopw-1).eq.';')then
				winput='printresult2('//winput(1:lopw-2)//')'  !13+1-2
 
			else
				winput='printresult('//winput(1:lopw-1)//')'  !12+1-1
 
			endif !if(winput(lopw-1:lopw-1).eq.';')  14557
			lopw=lopw+12
			!		write(6,*)'<515>',winput(1:lopw)
		endif !if(j_nextlim(winput(1:lopw),1,lopw,'=').gt.lopw.and.winput  14554
		!	write(6,*)'<555 ',winput(1:lopw)
 
		printout=winput(lopw:lopw).eq.';'
		if(printout)lopw=lopw-1
		printout2=winput(lopw:lopw).eq.';'
		if(printout2)lopw=lopw-1
		if(lopw.lt.1)then
			write(6,*)'*illegal ; -line'
			j_err=.true.
			return
		endif !if(lopw.lt.1)  14573
		if(j_p)write(6,*)'**PRINTOUT',printout,printout2,winput(1:lopw)
		!	nnset=0  !node where setelem
		ira=index(winput(1:lopw),'ran')
		ira1=max(ira-1,1)
		if(ira.gt.0.and..not.(j_isletter(winput(ira1:ira1)).or.( winput(ira1:ira1).ge.'0' &
				.and.winput(ira1:ira1).le.'9' )))then
			ipi=j_nextlim(winput,ira,lopw,'(')
			if(ipi.le.lopw)then
				ipi=ipi-1
				if(winput(ira:ipi).eq.'ran'.or.winput(ira:ipi).eq.'rann'.or.winput(ira:ipi).eq.'ranbin'.or.&
						winput(ira:ipi).eq.'rannegbin'.or.winput(ira:ipi).eq.'random')then
					ias=index(winput(1:ira),'=')-1
					if(ias.gt.0)then
						if(index(winput(1:ias),'-').gt.0.or.index(winput(1:ias),'All').gt.0.or. &
								index(winput(1:ias),'Tolast').gt.0)then
							write(6,*)'random numbers can be put only to whole matrices '
							j_err=.true.
							return
						endif !if(index(winput(1:ias),'-').gt.0.or.index(winput(1:ias),'A  14591
					endif !if(ias.gt.0)  14590
 
 
				endif !if(winput(ira:ipi).eq.'ran'.or.winput(ira:ipi).eq.'rann'.o  14587
			endif !if(ipi.le.lopw)  14585
		endif !if(ira.gt.0.and..not.(j_isletter(winput(ira1:ira1)).or.( w  14582
		!		write(6,*)'<6565winput',winput(1:lopw)
		isif=.false.
		isifthen=.false.
		!	iselse=.false.
		if(j_p2)write(6,*)'printout ',printout
		!			call polishall(winput,lopw)
		!		if(j_p2)write(6,*)'<line after polish> ', winput(1:lopw)
		call getconstants()
		if(j_err)goto 999
		!	write(6,*)'<line after constants> ', winput(1:lopw)
 
		call getoptions()
		!write(6,*)'aftop',lopw
		!write(6,*)'aftopt',lopw,winput(maX(lopw-80,1):MAX(1,lopw)),'*'
		if(j_err)goto 999
		if(j_p2)write(6,*)'<line after options> ', winput(1:lopw)
		if(winput(1:3).eq.'if('.or.winput(1:7).eq.'elseif(')then
			isif=.true.
			lo=j_nextrp(winput,3,lopw)
			if(lo.gt.lopw)then
				write(6,*)'unbalanced parenthesis in if('
				j_err=.true.;goto 999
 
			endif !if(lo.gt.lopw)  14622
 
 
			if(winput(lopw-4 :lopw).eq.')then'.or.winput(1:7).eq.'elseif(')then
				if(oneline)then
					write(6,*)'if...then not allowed at command level'
					j_err=.true.;goto 999
 
				endif !if(oneline)  14630
 
				!	isif=.false.
				isifthen=.true.
				if(winput(1:3).eq.'if(')then
					lopw=lopw-4
					nifthen=nifthen+1
					nifgoto(nifthen)=0  !number of gotos
					ioifthen(nifthen)=0
					!		write(6,*)'<568 start if then ',nifthen
 
				else !if(winput(1:3).eq.'if(')then
 
					if(nifthen.le.0)then
						write(6,*)'no previous if( )then'
						write(6,*)j_inp(1:j_linp),'   line ',iline
						j_err=.true.;goto 999
					endif !if(nifthen.le.0)  14647
					if(winput(lopw-4 :lopw).eq.')then')lopw=lopw-4  !if then
					!	call tekut(teku,nteku
					!	subroutine else()
					if(j_p2)write(6,*)'elseif',j_fbloop+7,nteku+1,'nifgoto(nifthen)',nifgoto(nifthen),ioifthen(nifthen)
					!ntek=ntekuf(3)
					call teku(nteku+1,j_fgoto)  !bloop+7 )   !goto
					call teku(ioifthen(nifthen)+1,nteku+4)
					nifgoto(nifthen)=nifgoto(nifthen)+1 !increase counter of needed goto labels
					ioifgoto(nifthen,nifgoto(nifthen))=nteku+2  ! udate goto after endif
					!	ioifthen(nifthen)=
					nteku=nteku+3
					winput(1:lopw-4)=winput(5:lopw)
					lopw=lopw-4
					!ioelse(nifthen)=nteku from previous if(then or elseif
 
 
					if(j_p2)write(6,*)'aftelse nteku',nteku,' putto',ioifthen(nifthen)+1
					if(j_p2)write(6,'(20i5/)')j_o(ivteku)%i(1:nteku)
 
				endif !if(winput(1:3).eq.'if(')  14638
			else !if(winput(lopw-4 :lopw).eq.')then'.or.winput(1:7).eq.'elseif(')then
 
				lopw2=lopw-lo
				winput2(1:lopw2)=winput(lo+1:lopw)
				lopw=lo
				!isgoto=winput2(1:5).eq.'goto('
				!	if(j_p)write(6,*)'ISGOTO',isgoto
				!				write(6,*)'<294 start if().....',winput2(1:lopw2),lopw2
 
			endif !if(winput(lopw-4 :lopw).eq.')then'.or.winput(1:7).eq.'else  14629
 
 
 
 
 
		elseif(winput(1:3).eq.'do(')then !if(winput(1:3).eq.'if('.or.winput(1:7).eq.'elseif(')then
			isdo=.true.
			ndosec=ndosec+1
			dosec(ndosec,1)=nteku  !to prevent goto into do
 
		elseif(winput(1:lopw).eq.'enddo')then !if(winput(1:3).eq.'if('.or.winput(1:7).eq.'elseif(')then
			if(ndo.le.0)then
				write(6,*)'there are no open do() at enddo at line ',iline
				j_err=.true.;goto 999
			endif !if(ndo.le.0)  14693
			!ntek=ntekuf(4)
			call teku(nteku+1,j_fbloop+15)
			call teku(nteku+2,iodo(ndo))   !one argument the io of the corrersponding do
			call teku(iodo(ndo)+1,nteku+3)   !jump label if no do iterations
			nteku=nteku+2
			ndo=ndo-1
			dosec(ndosec,2)=nteku
			cycle
 
		endif !if(winput(1:3).eq.'if('.or.winput(1:7).eq.'elseif(')  14619
		!		write(6,*)'<her ',j_nextlim(winput(1:lopw),1,lopw,'='),lopw,winput(1:lopw)
		isif2=.false.
		!	this is done twice if there is if()...    or if()    then  or elseif()
100		if(winput(1:lopw).eq.'return')then
			nteku=nteku+1
			call teku(nteku,0)
			!			write(6,*)'<100>'
			!if(isif2)call getif2()
			goto 888
		endif !100		if(winput(1:lopw).eq.'return')  14710
		iass=j_nextlim(winput,1,lopw,'=')
		iass0=0
		if(iass.eq.lopw.or.iass.eq.1)then
			write(6,*)'command cannot start or end with ='
			j_err=.true.;goto 999
		endif !if(iass.eq.lopw.or.iass.eq.1)  14719
		if(iass.lt.lopw)iass0=iass
		if(j_p2.and.isif2)write(6,*)'<6363iass,iass0',iass,iass0,lopw
		if(iass.gt.lopw)then
 
			winput='Result='//winput(1:lopw)
			iass=7
			iass0=7
			lopw=lopw+7
		endif !if(iass.gt.lopw)  14725
		!	write(6,*)
		winput='list2('//winput(1:iass-1)//')=list2('//winput(iass+1:lopw)//')'
		lopw=lopw+14
		if(j_p2)write(6,*)'<55>',winput(1:lopw)
		ial=1
		lopw0=lopw
		!	call getoptions()
		if(j_err)goto 999
		if(j_p2)then
			if(lopw.lt.lopw0)then
				write(6,*)'<line after options> ', winput(1:lopw)
			else !if(lopw.lt.lopw0)then
				write(6,*)'<no optionss>'
			endif !if(lopw.lt.lopw0)  14741
		endif !if(j_p2)  14740
		!write(6,*)'<line after options> ',lopw, winput(1:lopw)
		call polishall(winput,lopw)
		if(j_err)goto 999
		!		write(6,*)'<line after polish> ',winput(1:lopw)
		isoutput=.false.
		call jparse(winput(1:lopw),isoutput)
		if(j_err.or.nexist)goto 999
		!	write(6,*)'<line afteparse> ',winput(1:lopw)
		!	nodegoto=0
		!	isgoto=.false.
		do in=1,nn
			!		if(j_p)write(6,*)' in ',in,node(in)
			! if(node(in).eq.j_fgoto)then
			! ngotos=ngotos+1
			! nodegoto=in  !to get the place where to put the jump label
			! isgoto=.true.
			! if(j_p)write(6,*)'ISGOTO',isgoto
			! !		write(6,*)'***ngotos**',ngotos,nodegoto, ' shoudl follow 44isgoto'
			! endif !if(node(in).eq.j_fgoto)  13548
 
			if(node(in).eq.1.or.node(in).eq.8)then
				isist=sister(in)
				!			if(j_p)write(6,*)'isist'
				do while(isist.ne.0)
					if(node(isist).ne.1.and.node(isist).ne.8)then
						write(6,*)'**option can be followed only with options'
						j_err=.true.;return
 
					endif !if(node(isist).ne.1.and.node(isist).ne.8)  14771
					isist=sister(isist)
				enddo !while(isist.ne.0)  14770
			endif !if(node(in).eq.1.or.node(in).eq.8)  14767
 
		enddo !in=1,nn  14757
 
		if(j_err)goto 999
 
 
 
 
		nout=0
		ini=child(2)
		do while(ini.gt.0)
			ivo=-node(ini)
			if(ivo.ne.j_ivresult.and.ivo.gt.0)then
				nout=nout+1
				outlist(nout)=ivo
				if(ivo.le.j_locked)then
					call j_printname('*Object ',ivo,' is locked')
					j_err=.true. ;goto 999
				elseif(ivo.gt.j_nv)then
					write(6,*)'*constant cannot be output'
					j_err=.true.;goto 999
				endif !if(ivo.le.j_locked)  14794
				if(isinout)call j_putoutput(ivo,ivinl,ivoutl,ivarg)
			end if !if(ivo.ne.j_ivresult.and.ivo.gt.0)  14791
			ini=sister(ini)
		enddo !while(ini.gt.0)  14789
 
		nin=0
 
		do ini=child(sister(2)),nn
			ivo=-node(ini)
			if(ivo.gt.0)then
				!		write(6,*)'ini,ivo,',ini,ivo
				nin=nin+1
				inlist(nin)=ivo
				if(ivo.le.j_namedv.and.isinout.and.node(mother(ini)).ne.j_fpause)&
					call j_putinput(ivo,ivinl,ivoutl,ivarg)
			endif !if(ivo.gt.0)  14810
 
		enddo !ini=child(sister(2)),nn  14808
 
		ntekuv=nteku
		noption=0
		noption2=0
		node3=node(3)  !to test setelem
		node4=node(4)
 
		!	if(j_p.and.nodegoto.ne.0)write(6,'(a,(20i5/))')'<gotbefo>',j_o(ivteku)%i(1:nteku)
		call tekut(nteku)
		if(j_err)goto 999
		!	if(nodegoto.ne.0)gotos(ngotos)=nodetoteku(nodegoto)
 
		! if(nodegoto.ne.0)write(6,*)'<44isgoto tas ',isgoto,' nodegoto ',nodegoto,&
		! ' gotos(ngotos) ',gotos(ngotos), 'isgoto ',isgoto
		! if(nodegoto.ne.0)write(6,'(a,(20i5/))')'<goto>',teku(1:nteku)
		!if(j_p)write(6,*)'isgotocycle ',isgoto
		!		if(isgoto)cycle
		if(j_p2)then
			write(6,*)'nin ',nin,inlist(1:nin),' ntekuv ,nteku,', ntekuv,nteku,'iass0',iass0
			write(6,*)'nout ',nout,outlist(1:nout)  !,'ISGOTO',  isgoto
			write(6,'(a,(20i5/))')'<gtasfo>',j_o(ivteku)%i(1:nteku)
			write(6,*)'noption,noption2',noption,noption2,iass0,nteku,ntekuv
 
		end if !if(j_p2)  14836
		if(isdo)then
			call getdo()
			if(j_err)goto 999
			cycle mainloop
		endif !if(isdo)  14843
		!write(6,*)'p3**',j_p3
		if(j_p3)write(6,*)'<777i ,iass0,nargo,nteku,ntekuv',iass0,nargo,nteku,ntekuv
		if(nteku.eq.ntekuv)then
			if(iass0.gt.0)then
				if(nin.eq.1.and.nin.eq.nout)then
					!ntek=ntekuf(4)
					call teku(nteku+1,j_fassone)
					call teku(nteku+2,1)
					call teku(nteku+3,inlist(1))
					call teku(nteku+4,outlist(1))
					nteku=nteku+4
				else
					!ntek=ntekuf(nin+nout+4)
					call teku(nteku+1,j_fassign)
					call teku(nteku+2,nin)
					call teku(nteku+3,nout)
					nteku=nteku+3
					if(nin.le.0)then
						write(6,*)'*syntax error, input list empty'
						j_err=.true.;return
					endif !if(nin.le.0)  14865
					call teku(nteku+nin,inlist(nin))
					if(nin.gt.1)j_o(ivteku)%i(nteku+1:nteku+nin-1)=inlist(1:nin-1)
 
					!call teku(nteku+1:nteku+nin,inlist(1:nin))
					nteku=nteku+nin
					call teku(nteku+nout,outlist(nout))
					if(nout.gt.1)j_o(ivteku)%i(nteku+1:nteku+nout-1)=outlist(1:nout-1)
					!	call teku(nteku+1:nteku+nout,outlist(1:nout))
					nteku=nteku+nout
					if(j_p2)write(6,'(a,(20i5/))')'<finfin>',j_o(ivteku)%i(1:nteku)
					! narin=j_o(iob)%i(io+1)
 
 
				endif !if(nin.eq.1.and.nin.eq.nout)  14852
			else !if(iass0.gt.0)then
				outlist(1:nin)=inlist(1:nin)  !to print correctly
				nout=nin
			endif !if(iass0.gt.0)  14851
		elseif(nout.eq.1)then !  .and..not.isgoto)then !if(nteku.eq.ntekuv)then
			call teku(nteku,outlist(1))
			! nteku position has the temporal
			if(j_p3)write(6,*)'tasteku ',nteku,outlist(1),ntekuv !,' isgoto ',isgoto
		else !if(nteku.eq.ntekuv)then
			! narout=j_o(iob)%i(io+2)
			! argin=>j_o(iob)%i(io+3:io+2+narin)
			! argout=>j_o(iob)%i(io+3+narin:io+2+narin+narout)
			! ion=io+narin+narout+3
 
		endif !if(nteku.eq.ntekuv)  14850
		do iop=1,noption
			ilo=optionloc(iop)
			!		if(j_p.and.ilo+j_o(ivteku)%i(ilo+2)+3.eq.61)stop 'here'
			call teku(ilo+j_o(ivteku)%i(ilo+2)+3,nodetoteku( optionmother(iop)))
			if(j_p2)write(6,*)' optionloc ', ilo, 'teku ',ilo+j_o(ivteku)%i(ilo+2)+3,j_o(ivteku)%i(ilo+j_o(ivteku)%i(ilo+2)+3)
			optionlocout(iop)=optionloc(iop)
		enddo !iop=1,noption  14898
		if(j_p2.and.noption.gt.0)write(6,'(a,(20i5/))')'<finopt>',j_o(ivteku)%i(1:nteku)
		do iop=1,noption2
			ilo=optionloc2(iop)
			call teku(ilo+4,nodetoteku( optionmother2(iop)))
		enddo !iop=1,noption2  14906
		if(j_p2.and.noption2.gt.0)write(6,'(a,(20i5/))')'<finopt2>',j_o(ivteku)%i(1:nteku)
		if(nin.eq.nout.and.nin.eq.1.and.nteku.gt.ntekuv)then ! .and..not.isgoto)then
			if(j_p3)write(6,*)'NIN,nout,nteku,ntekuv',nin,nout,nteku,ntekuv !,' ISGOTO ',isgotoisgoto
			call teku(nteku,outlist(1))
		else !if(nin.eq.nout.and.nin.eq.1.and.nteku.gt.ntekuv)then
			if(.not.(nin.eq.1.and.nout.gt.1.or.nout.eq.1.or.nin.eq.nout))then
				write(6,*)'illegal assignment nin, nout ',nin,nout
				j_err=.true.;goto 999
			endif !if(.not.(nin.eq.1.and.nout.gt.1.or.nout.eq.1.or.nin.eq.nou  14915
 
 
		endif !if(nin.eq.nout.and.nin.eq.1.and.nteku.gt.ntekuv)  14911
 
		if(node3.eq.j_fsetelem)then
			!iout=j_o(iob)%i(io+2+narg)
			li=nodetoteku(node3)
			nar=j_o(ivteku)%i(li+1)
			call teku( li+2+nar,-(node(child(sister(2))))  )  !inlist(nin)
			if(j_p)write(6,*)'setelem li,nar,li+2+nar',li,nar,li+2+nar
		endif !if(node3.eq.j_fsetelem)  14923
		if(printout)then
			if(j_p2)write(6,*)'node(3:4)',node3,node4,j_fsetelem,'j_fbio+ip6',j_fbio,ip6
			!ntek=ntekuf(4)
 
			if(isder2)call endder(1)
			!		write(6,*)'printout',nteku+1,j_fbio+ip6,'isder2',isder2
			call teku(nteku+1,j_fbio+ip6)
			if(.not.oneline.and.printout2)call teku(nteku+1,j_fbio+7)
			if(node3.eq.j_fsetelem)then
 
				call teku(nteku+2,1)
				call teku(nteku+3,-node4)
				call teku(nteku+4,j_ivresult)
				if(j_p)write(6,*)'prinnadprin ',j_o(ivteku)%i(nteku+1:nteku+4),winput(1:lopw)
				nteku=nteku+4
 
			else   !if(.not.isgoto)then !if(node3.eq.j_fsetelem)then
				!			write(6,*)'<33notisgotonteku',nteku
				call teku(nteku+2,nout)
				nteku=nteku+2
				call teku(nteku+nout,outlist(nout))
				if(nout.gt.1)j_o(ivteku)%i(nteku+1:nteku+nout-1)=outlist(1:nout-1)
				!	call teku(nteku+1:nteku+nout,outlist(1:nout))
				nteku=nteku+nout+1
				call teku(nteku,j_ivresult)
				if(j_p3)write(6,*)'<444result ',nteku
				!			write(6,*)'prinnytas,ntekuivres ',nteku,'isgoto ',isgoto,winput(1:lopw)
			endif !if(node3.eq.j_fsetelem)  14938
 
			! if(nin.eq.nout)then
			! nout2=0
			! do ii=1,nin
			! if(outlist(ii).gt.0)then
			! nout2=nout2+1
			! !				outlist2(nout2)=outlist(ii)
			! if(outlist(ii).le.j_locked)then
			! call j_printname('*Object ',outlist(ii),' is locked')
			! j_err=.true.;return
			! endif
			! if(isinout)call j_putoutput(outlist(ii),ivinl,ivoutl)
 
			! if(j_p2)write(6,*)'<38output',outlist(ii)
			! !		inlist2(nout2)=inlist(ii)
			! endif
			! enddo
			! endif
 
		endif !if(printout)  14930
		if(j_p)write(6,*)'bef888 '
888	  continue
		if(j_p)write(6,*)'*isif ifuif,isif2,',isif, ifuif,isif2
		if(isif)then
			ifuif=nodetoteku(sister(2)+1)  !where is the if in teku vector
			!ntek=ntekuf(3)
			call teku(ifuif+3,nteku+1)
			if(j_p)write(6,*)'<66##############ifuif',ifuif,'teku ',j_o(ivteku)%i(1:nteku)
 
			isif=.false.
			! isgoto=.false.
			!	if(winput(1:5).eq.'goto(')isgoto=.true.
			! isgoto=.true.
			! write(6,*)'getototas'
			! ! call getgoto()
			! ! if(j_err)return
			! ! teku(ifuif+1)=nteku+1
			! ! return
			! !the same as with isif2
			! endif !if(winput(1:5).eq.'goto(')then
			if(isifthen)then
				!	iselse(nifthen)=.false. ! nodetotek(2) refers to if(
				!	ioifthen(nifthen)=nteku
				ifuif=nodetoteku(sister(2)+1)  !where is the if in teku vector
				!		write(6,*)'ifuif',ifuif,' nteku ',nteku,' nifthen ',nifthen
				!	write(6,'(20i5/)')teku(1:nteku)
				! if(j_v(j_o(iob)%i(io+2)).eq.j_0)then
				! io=j_o(iob)%i(io+1)  !condition not satisfied
				! else
				! io=j_o(iob)%i(io+3)   !condition satified
				! endif
				!teku(ifuif)=nteku+1
				!	teku(nodetoteku(2)+3)=nteku+1  !if
				!io=j_o(iob)%i(io+3)   !condition satified
				ioifthen(nifthen)=nodetoteku(sister(2)+1)  !place of if
				!	write(6,*)'**nifthen ',ioifthen(nifthen)
				!    if conditon is not satisfied	this is treatedd in else if or endif or in else
				!	write(6,*)'<298 && getifthen ',nifthen,nifgoto(nifthen),nodetoteku(2)
			else !if(isifthen)then
				winput(1:lopw2)=winput2(1:lopw2)
				if(j_p2)write(6,*)'process77 now after if():',winput2(1:lopw2)
				lopw=lopw2
				! isgoto=.false.
				!	isgoto=winput(1:5).eq.'goto('
				! if(winput(1:5).eq.'goto(')then
				! write(6,*)'getoto988888'
				! isgoto=.true.
				!		call getgoto()
				!	write(6,*)'getoto6666 ',ngotos
				! if(j_err)return
				! teku(ifuif+1)=nteku+1
				! cycle mainloop
				!the same as with isif2
				!		if(winput(1:5).eq.'goto(')isgoto2=.true.
				!endif !if(winput(1:5).eq.'goto(')then
				lopw=lopw2
				isif2=.true.
				if(j_p)write(6,*)'goto100 tas ',winput2(1:lopw2),' isif2',isif2 !,' isgoto ',isgoto
				goto 100
			endif !if(isifthen)  14998
		elseif(isif2)then !if(isif)then
			!	write(6,*)'<555isif2**************** ifuif',ifuif,nteku+1,j_inp(1:j_linp)
			!	write(6,'(20i5)')teku(1:nteku)
 
 
			call teku(ifuif+1,nteku+1)
 
			call teku(ifuif+3,ifuif+4)
			isif2=.false.
 
 
		endif !if(isif)  14981
		if(oneline)exit mainloop
		!	if(oneline)exit
	enddo mainloop !nloop:	do iline=1,nline2  14223
 
	!ntek=ntekuf(2)
	call teku(nteku+1,0)
	!	write(6,*)'teku finally ,j_err',j_err
	!	if(isif2)call getif2()
	!	call teku(nteku+1)=0
	if(j_p3)then
		write(6,*)'teku finally'
		write(6,'(20i5/)')j_o(ivteku)%i(1:nteku+1)
 
		if(nlabel.gt.0)write(6,*) 'labelpart ',j_o(ivteku)%i(nteku+2:nteku+nlabel+3)
	endif !if(j_p3)  15059
999	if(j_err)then
!write(6,*)'line ',iline

	!	write(6,*)'linp',j_linp,j_inp(1:j_linp)
		nteku=0
		call teku(1,0)
		call teku(0,0)
	endif !999	if(j_err)  15065
	! if(minval(teku(1:nteku+1)).lt.0)then
	! write(6,*)'syntax errror, vector '
	! write(6,'(20i5/)')teku(1:nteku+1)
	! j_err=.true.
	! endif
	if(j_p2)write(6,*)'<355353ntekureturn',nteku
	call teku(nteku+1,0)
	call teku(0,nteku)
	!	write(6,'(20i5/)')j_o(ivteku)%i(1:nteku+1)
	return
 
	contains            !subroutine
 
	subroutine endder(istep)
		!	write(6,*)'isder2',isder2,ider+j_o(ivteku)%i(ider+1)+2,nteku-1
		!the line after der() is done
		call teku(ider+j_o(ivteku)%i(ider+1)+2,nteku-1+istep) !normal outpu place
		!	write(6,*)'<8886 ',j_mxnamedv+j_mxtemporalv0+1,itemporalv+1
		do it=j_mxnamedv+j_mxtemporalv0+1,itemporalv+1  !one extra
			if(allocated(j_o(it)%d))then
				if(size(j_o(it)%d).lt.nder)then
					deallocate(j_o(it)%d)
				else !if(size(j_o(it)%d).lt.nder)then
					cycle
				endif !if(size(j_o(it)%d).lt.nder)  15093
			endif !if(allocated(j_o(it)%d))  15092
			!	write(6,*)'<888999it',it,nder
			allocate(j_o(it)%d(1:2*nder)) !make a reserve
		enddo !it=j_mxnamedv+j_mxtemporalv0+1,itemporalv+1  15091
		isder=.false.
		isder2=.false.
		!		write(6,*)'isder2 valse here'
 
	end subroutine
 
	subroutine teku(ipos,ival)
		integer, allocatable,dimension(:)::tempteku
		if(ipos.gt.ntekumax)then
			!ntek=nteku !nteku is pointer to i(0)
			i0=j_o(ivteku)%i(0)
			allocate(tempteku(1:ntekumax))
			tempteku(1:ntekumax)=j_o(ivteku)%i(1:ntekumax)
 
			deallocate(j_o(ivteku)%i)
			allocate(j_o(ivteku)%i(0:2*ntekumax))
			j_o(ivteku)%i(1:ntekumax)=tempteku(1:ntekumax)
			j_o(ivteku)%i(0)=max(i0,ipos)
			deallocate(tempteku)
			ntekumax=2*ntekumax
		endif !if(ipos.gt.ntekumax)  15110
		j_o(ivteku)%i(ipos)=ival
 
 
	end subroutine
	subroutine getdo()
		! func,addres to jump if loop is not done, indexvar,iv-low,iv-up,iv-step,
		! current,up,step
 
		niin=nodetoteku(sister(2)+1)
		if(j_o(ivteku)%i(niin+1).eq.3)then !step is missing
			call	teku(niin+5,j_ivone)
 
			nteku=nteku+1
 
			if(nteku.ne.niin+6)stop 'per'
		elseif(j_o(ivteku)%i(niin+1).gt.4.or.j_o(ivteku)%i(niin+1).lt.3)then !if(j_o(ivteku)%i(niin+1).eq.3)then
			write(6,*)'do() illegal number of arguments'
			j_err=.true.
			return
		endif !if(j_o(ivteku)%i(niin+1).eq.3)  15132
		if(j_o(ivteku)%i(niin+2).gt.j_namedv)then
			write(6,*)'do() first argument must be named variable'
			j_err=.true.;return
		endif !if(j_o(ivteku)%i(niin+2).gt.j_namedv)  15143
		nteku=nteku+2    !the outputvariable is not needed
 
		ndo=ndo+1
		iodo(ndo)=niin
		!	write(6,*)'<6636ntekundo',ndo,iodo(ndo),'nteku 'nteku
 
		return
 
	end subroutine !subroutine getdo()
 
	subroutine getif()
		ifuif=nodetoteku(sister(2)+1)
		!	ifuif=nodetoteku(2)  !where is the if in teku vector
		!		write(6,*)'<66###########bef###ifuif',ifuif,'teku ',teku(1:nteku)
		call teku(ifuif+3,nteku+1)
		!	write(6,*)'<66####aft######ifuif',ifuif,'teku ',teku(1:nteku)
		winput(1:lopw2)=winput2(1:lopw2)
		if(j_p2)write(6,*)'process now after if():',winput(1:lopw2)
		lopw=lopw2
		isif=.false.
		!	isgoto=winput(1:5).eq.'goto('
		! if(winput(1:5).eq.'goto(')then
		! write(6,*)'getoto999'
		! !call getgoto()
		! isgoto=.true.
		! !	write(6,*)'getoto6666 ',ngotos
		! if(j_err)return
		! teku(ifuif+1)=nteku+1
		! write(6,*)'<777putifuif+1',ifuif+1,nteku+1,j_inp(1:j_linp)
		! return
		! !the same as with isif2
 
		! endif !if(winput(1:5).eq.'goto(')then
 
		!	isif2=.true.  !treat if
		!	nteku=nteku-1  !we can drop output out
 
 
		!goto 100  !process  if(   ) the last part
		return
	end subroutine !subroutine getif()
 
 
 
	! subroutine getgoto()
 
	! if(winput(lopw:lopw).ne.')')then
	! write(6,*)'illegal goto ';j_err=.true.;return
	! endif !if(winput(lopw:lopw).ne.')')  13976
 
	! ial=6
	! iar=0
	! 178		ipil=j_nextlim(winput,ial,lopw,',')
	! !		write(6,*)'ipil',ipil,ial
	! if(ial.gt.6.and.ipil.gt.lopw)then !was
	! if(ial.eq.lopw-1)then
	! write(6,*)'**illegal goto'
	! j_err=.true.
	! return
 
	! endif !if(ial.eq.lopw-1)  13985
	! ivargperk=j_object(winput(ial:lopw-1))
	! if(ivargperk.le.0)call j_getobject(0,winput(ial:lopw-1),j_ipreal,ivargperk)
	! call	teku(nargpo,iar)
	! nteku=nteku+1
	! call teku(nteku,ivargperk)
	! call j_putinput(ivargperk,ivinl,ivoutl,ivarg)
	! !	write(6,*)'<<34>',teku(1:nteku)
	! return
	! endif !if(ial.gt.6.and.ipil.gt.lopw)  13984
 
	! if(ipil.lt.lopw)then
	! ila=j_isin(winput(ial:ipil-1),label,nlabel)
	! iar=iar+1
 
	! !	write(6,*)'gotoila********************',ila,iar
	! !		ngotos=ngotos+1
	! if(ila.eq.0)then
	! nlabel=nlabel+1
	! label(nlabel)=winput(ial:ipil-1)
	! !	write(6,*)'gotolabel,ngoto ',label(nlabel)
	! ila=nlabel
	! endif !if(ila.eq.0)  14007
	! if(ial.eq.6)then  !first
	! call teku(nteku+1,j_fbloop+17)
	! nargpo=nteku+2 !position of narg
	! call teku(nteku+3,ila)
	! nteku=nteku+3
	! !		write(6,*)'tkthere>',teku(1:nteku)
	! ! else
	! ! nteku=nteku+1
	! ! teku(nteku)=ila
	! ! !			write(6,*)'tktas>',teku(1:nteku)
 
	! endif !if(ial.eq.6)  14013
	! !		gotos(ngotos)=nteku
	! ial=ipil+1
	! goto 178
	! endif !if(ipil.lt.lopw)  14001
 
 
 
	! ila=j_isin(winput(6:lopw-1),label,nlabel)
	! !	write(6,*)'gotoila********************',ila
	! if(ila.eq.0)then
	! nlabel=nlabel+1
	! label(nlabel)=winput(6:lopw-1)
	! !		write(6,*)'gotolabel,ngoto ',label(nlabel)
	! ila=nlabel
	! endif !if(ila.eq.0)  14034
	! call teku(nteku+1,j_fbloop+7)
	! call teku(nteku+2,ila)  !replaced later with ilabel
 
	! !	ngotos=ngotos+1
 
	! nteku=nteku+2
	! !		gotos(ngotos)=nteku
	! !	write(6,*)'ngotos',ngotos,'nteku',nteku,'nalebl',nlabel
	! !	stop
	! return
 
	! end subroutine !subroutine getgoto()
 
	subroutine getconstants()  !+options
		character*4,dimension(11)::loper
 
		logical::waspiste
		data loper/'eq','ne', 'le','lt','ge','gt','not', 'and','eqv','neqv','or'/
		logical :: hipsu,haka,namenum
		ial=1
 
		ial0=1
		waspiste=.false.
		if(winput(1:1).eq.'+'.or.winput(1:1).eq.'-')ial=2
		if(winput(ial:ial).eq.'.')then
 
			waspiste=.true.
			if(j_p)write(6,*)'<44 starting with .'
			goto 17
		elseif(winput(ial:ial).ge.'0'.and.winput(ial:ial).le.'9')then !if(winput(ial:ial).eq.'.')then
			goto 2
		endif !if(winput(ial:ial).eq.'.')  15281
		ial=2
 
 
1		continue !find next number
		hipsu=.false.
		haka=.false.
		namenum=.false.  !number within name
		lis=0
		if(j_p)write(6,*)'<555,ial',ial,lopw,winput(1:max(ial-1,1)),'#',winput(ial:lopw)
		do while (.not.j_isnumber(winput(ial-1:ial+1)).or.hipsu.or.haka.or.namenum.or. &
				winput(ial:ial+2).eq.'...')
			if(winput(ial:ial).eq."'")then
				if(.not.hipsu)then
					ialhipsu=ial
					ial=ial+1
				else !if(.not.hipsu)then
					ivc=j_object(winput(ialhipsu:ial))
					if(ivc.le.0)ivc=j_defchar(0,winput(ialhipsu:ial))
					if(j_err)return
					if(j_p)write(6,*)'befhipsu ',winput(1:lopw)
					call j_repse(winput,ialhipsu,ial,lopw,ivc,ial) !ial uusi aslku
					!		write(6,*)'afthipsu,ial:',ial,winput(1:ial-1),'#',winput(ial:lopw)
					if(ial.lt.lopw)then
						if(winput(ial:ial).ne.','.and.winput(ial:ial).ne.')')then
							write(6,*)"*illegal continuation after '", winput(ial:lopw)
							j_err=.true.;return
						endif !if(winput(ial:ial).ne.','.and.winput(ial:ial).ne.')')  15312
					endif !if(ial.lt.lopw)  15311
 
				endif !if(.not.hipsu)  15301
				hipsu=.not.hipsu
 
				if(ial.gt.lopw)return
				cycle
			endif !if(winput(ial:ial).eq."'")  15300
			if(hipsu)then
				ial=ial+1
				if(ial.gt.lopw)return
				cycle
			endif !if(hipsu)  15324
			if(winput(ial:ial+2).eq.'...')then
				ial=ial+3
				if(ial.gt.lopw)then
					write(6,*)'input cannot end with ...'
					j_err=.true.;return
				endif !if(ial.gt.lopw)  15331
				cycle
			endif !if(winput(ial:ial+2).eq.'...')  15329
			if(winput(ial:ial).eq."[")haka=.true.
			if(winput(ial:ial).eq."]")haka=.false.
			if(winput(ial:ial).lt.'0'.or.winput(ial:ial).gt.'9')namenum=.false.
			if(j_p2.and.ial.ge.lopw)write(6,*)'<558 return',ial,j_isnumber(winput(ial-1:ial+1))
			ial=ial+1
 
			if(ial.gt.lopw)return
 
 
		enddo !while (.not.j_isnumber(winput(ial-1:ial+1)).or.hipsu.or.ha  15298
 
		if(j_isletter(winput(ial-1:ial-1)).and.winput(ial:ial).ge.'0'.and.winput(ial:ial).le.'9')then
			ial=ial+1
			do while (winput(ial:ial).ge.'0'.and.winput(ial:ial).le.'9')
				ial=ial+1
				if(ial.gt.lopw)return
			enddo !while (winput(ial:ial).ge.'0'.and.winput(ial:ial).le.'9')  15350
			goto 1
		endif !if(j_isletter(winput(ial-1:ial-1)).and.winput(ial:ial).ge.  15348
		!	namenum=.false.
		ial0=ial
		if(j_p)write(6,*)'ial here',ial,winput(ial0:ial0)
 
		waspiste=winput(ial:ial).eq.'.'
		ial1=ial-1
		if(winput(ial1:ial1).eq.'+'.or.winput(ial1:ial1).eq.'-')then
			if(ial1.eq.1)then
				ial0=ial1
			elseif(winput(ial-2:ial-2).eq.'('.or.winput(ial-2:ial-2).eq.'='.or.& !if(ial1.eq.1)then
					winput(ial-2:ial-2).eq.',')then
				ial0=ial1
			endif !if(ial1.eq.1)  15363
		endif !if(winput(ial1:ial1).eq.'+'.or.winput(ial1:ial1).eq.'-')  15362
		! ial1=ial1-1
		! ine=j_nextlim(winput,ial1-1,ial1-1,',(=')
		! if(j_p)write(6,*)'<72 ,ial1,ine,ine.eq.ial1-1',ial1,ine,ine.eq.ial1-1
		! if(ine.eq.ial1-1)ial0=ial1
 
 
 
		2	continue !where the number ends ialis after the number
		ial=ial+1
		if(j_p)write(6,*)'ial888',ial,lopw,winput(ial:ial)
		if(ial.gt.lopw)goto 31
 
		do while (winput(ial:ial).ge.'0'.and.winput(ial:ial).le.'9')
 
			ial=ial+1
			if(ial.gt.lopw)goto 3
		enddo !while (winput(ial:ial).ge.'0'.and.winput(ial:ial).le.'9')  15382
 
		if(winput(ial:ial).eq.'e'.or. &
				winput(ial:ial).eq.'E'.or.winput(ial:ial).eq.'d'.or.winput(ial:ial).eq.'D')then
			ial=ial+1
			if(ial.gt.lopw)goto 99
			if(winput(ial:ial).eq.'+'.or.winput(ial:ial).eq.'-')ial=ial+1
			iala=ial
			do while (winput(ial:ial).ge.'0'.and.winput(ial:ial).le.'9')
				ial=ial+1
				if(ial.gt.lopw)goto 3
			enddo !while (winput(ial:ial).ge.'0'.and.winput(ial:ial).le.'9')  15394
			if(ial.eq.iala)goto 99
		endif !if(winput(ial:ial).eq.'e'.o  15388
		lis=0
17		if(winput(ial:ial).eq.'.')then
			iip=j_nextlim(winput,ial+1,ial+5,'.')
			if(j_p2)write(6,*)'iip',iip
			if(iip.eq.1)goto 99
			if(iip.gt.0)then
 
				ilo=j_isin(winput(ial+1:iip-1),loper,11)
				if(j_p2)write(6,*)'iipilo',ilo,winput(ial+1:iip-1)
				if(ilo.gt.0)then
					if(ial.eq.1)then
						ial=iip+1
						goto 1
					endif !if(ial.eq.1)  15410
					lis=iip-ial  !-1 -(ial+1) +2=iip-1-ial-1+2
					goto 31
				endif !if(ilo.gt.0)  15409
			endif !if(iip.gt.0)  15405
 
			if(waspiste)goto 99
			!	write(6,*)'continuenumber,ial',ial,winput(ial:ial)
			goto 2 !continue number
		endif !17		if(winput(ial:ial).eq.'.')  15401
3		continue  !test number element ial is first which does not belong to number
		!number must be followed by some delimiter
		if(j_p)write(6,*)'<587 ial,winput',ial,winput(ial:ial)
		inex=j_nextlim(winput,ial,ial,',+*)/-')
		!if(inex.gt.ial)goto 99
31		continue		!or number is at end
		ial1=ial-1
		if(j_p)write(6,*)'<5252529 ',winput(ial0:ial1)
		idots=index(winput(ial0:ial1),'...')
		if(idots.gt.0)then
			!	write(6,*)'ial0,ial1',ial0,ial1,'first:',winput(ial0:ial0+idots-2),'*',winput(ial0+idots+2:ial1)
			if(idots.gt.1)goto 77
88		write(6,*)'illegal ... in ',	winput(ial0:ial1)
			j_err=.true.;return
 
77		read(winput(ial0:ial0+idots-2),'(i4)',err=88)ii1
			if(ial1-ial0.le.3)goto 88
 
 
78	read(winput(ial0+idots+2:ial1),'(i4)',err=88)ii2
			if(abs(ii2-ii1).gt.40)then
				write(6,*)'only 40 constants can be generated with ...'
				j_err=.true.;return
			endif !if(abs(ii2-ii1).gt.40)  15443
			istep=1
			if(ii2.lt.ii1)istep=-1
			if(lopw.gt.ial1)j_tempchar(1:lopw-ial1)=winput(ial1+1:lopw)
			iial=ial0-1
			!	write(6,*)winput(1:iial),'/#'
			do iij=ii1,ii2,istep
				if(iij.eq.1)then
					winput(iial+1:iial+2)='$1'
					iial=iial+2
					!		write(6,*)winput(1:iial),'/%'
				elseif(iij.eq.0)then
					winput(iial+1:iial+2)='$0'
					iial=iial+2
				else
					const=j_chi5(iij,1)
					lec=len_trim(const)
					iii=j_isconst(const(1:lec))
					const=j_chi5(iii,1)
					lec=len_trim(const)
 
					winput(iial+1:iial+lec)=const(1:lec)
					iial=iial+lec
					!		 write(6,*)winput(1:iial),'/##'
 
				endif !if(iij.eq.1)  15453
				if(iij.ne.ii2)then
					iial=iial+1
					winput(iial:iial)=','
					!		write(6,*)winput(1:iial),'/##&&'
				endif !if(iij.ne.ii2)  15472
			enddo !iij=ii1,ii2,istep  15452
			if(lopw.le.ial1)then
				lopw=iial
				!	write(6,*)'tas:',winput(1:lopw)
				return
			endif !if(lopw.le.ial1)  15478
			winput(iial+1:iial+lopw-ial0)=j_tempchar(1:lopw-ial0)
 
			lopw=iial+lopw-ial1
 
			ial=iial+lopw-ial0+1
			!	write(6,*)'taspa:',ial,lopw,winput(1:lopw)
			if(ial.gt.lopw)return
			goto 1
		endif !if(idots.gt.0)  15432
 
 
 
		iii=j_isconst(winput(ial0:ial1))
		if(iii.le.0)goto 99
		if(iii.eq.j_ivone)then
			!j_repl(jono1,i1,i2,linp,jono2,le2) : replaces the substring jono1(i1:i2) by string jono2(1:le2)
 
			call j_repl(winput,ial0,ial1,lopw,'$1',2) !ial is not updated
			if(j_p)write(6,*)'aftuno:',winput(1:lopw)
			ial=ial+1
		elseif(iii.eq.j_ivzero)then !if(iii.eq.j_ivone)then
			!	write(6,*)'<23befrepl',ial0,lopw,winput(1:lopw)
			call j_repl(winput,ial0,ial1,lopw,'$0',2)
			ial=ial+1
			!	write(6,*)'<23aftzero:',ial,lopw,winput(1:lopw)
 
		else !if(iii.eq.j_ivone)then
			!	subroutine j_repse(inp,i1,i2,lop,iii)
			! replace segement i1:i2
			!		by shortest presentation of integer iii
 
			!	call j_repse(winput,ial0,inex-1,lopw,iii,ial2=ial)
			if(j_p)write(6,*)'<23befrepse',ial0,ial1,lopw,winput(1:lopw),' const ',iii,j_v(iii)
			! if(ial0.eq.1.and.ial1.eq.lopw.and..not.printout.and..not.printout2)then
			! write(6,*)'**what should we do with this number: ',winput(1:lopw)
			! j_err=.true.
			! return
			! endif !if(ial0.eq.1.and.ial1.eq.lopw.and..not.printout.and..not.p  14171
 
			call j_repse(winput,ial0,ial1,lopw,iii,ial) !ial uusi aslku
 
			if(j_p)write(6,*)ialnew,winput(1:ial-1)//'#'//winput(ial:lopw)
		endif !if(iii.eq.j_ivone)  15497
		if(j_p)write(6,*)'<3636 ial,ialnew,lis,lopw',ial,ial+lis+1,lis,lopw
		ial=ial+lis+1
		if(ial.gt.lopw)return
 
		goto 1
99		write(6,*)'illegal syntax (4)'
	!	write(6,*)'<34>',winput(1:lopw)
		j_err=.true.;return
 
	end subroutine !subroutine getconstants()
 
	subroutine getoptions()
		character*5::chii
		ial=j_nextword(winput,1,lopw,'->')
		!write(6,*)'<567lopw',ial,lopw
		!if(ial.gt.lopw-20)write(6,*)winput(ial:lopw)
		do while(ial.lt.lopw)
			do jii=ial-1,1,-1
				if(winput(jii:jii).eq.','.or.winput(jii:jii).eq.'(')goto 178
			enddo !jii=ial-1,1,-1  15543
			write(6,*)'illegal ->',ial,lopw,winput(1:lopw)
			j_err=.true.;return
178				iopt=j_isin(winput(jii+1:ial-1),j_options,j_noptions)
			!	write(6,*)iopt
			if(iopt.le.0)then
				write(6,*)winput(jii+1:ial-1),' is not an option'
				if(winput(jii+1:ial-1).eq.'input')then
					write(6,*)'input-> is deleted option, just drop it, see manual for trans() '
				elseif(winput(jii+1:ial-1).eq.'readfirst')then
					write(6,*)'replace readfirst-> with rfcode->, see manual for data() function'
				elseif(winput(jii+1:ial-1).eq.'result')then
					write(6,*)'transformation tr can provide object Ob by tr(Ob), see manual '
 
				endif !if(winput(jii+1:ial-1).eq.'input')  15552
				j_err=.true.
				return
			endif !if(iopt.le.0)  15550
			chii=j_chi5(iopt,1)
			lec=len_trim(chii)
 
			lop=j_nextlim(winput,ial+2,lopw,',)(')
			if(winput(ial+2:ial+2).eq.'(')then
				ir=j_nextrp(winput,ial+2,lopw)
				winput=winput(1:jii)//'setoption('//chii(1:lec)//','//&
					winput(ial+3:lopw)
			elseif(winput(ial+2:ial+2).eq.',')then !if(winput(ial+2:ial+2).eq.'(')then
				winput=winput(1:jii)//'setoption('//chii(1:lec)//')'//&
					winput(ial+2:lopw)
			elseif(winput(lop:lop).eq.')'.and.lop.eq.ial+2)then !if(winput(ial+2:ial+2).eq.'(')then
				winput=winput(1:jii)//'setoption('//chii(1:lec)//')'//&
					winput(lop:lopw)
			elseif(winput(lop:lop).eq.'(')then !if(winput(ial+2:ial+2).eq.'(')then
				loo=lop-ial-2
				iper=j_nextlim(winput(ial+2:lop-1),1,loo,'+-*/')
				!	write(6,*)winput(1:len_trim(winput))
				!	write(6,*)iper,loo,winput(ial+2:lop-1),loo
				if(iper.lt.loo)then
					write(6,*) 'illegal option ',j_options(iopt)
					j_err=.true.;return
				endif !if(iper.lt.loo)  15582
				ir=j_nextrp(winput,lop,lopw)
				winput=winput(1:jii)//'setoption('//chii(1:lec)//','//&
					winput(ial+2:ir)//')'//winput(ir+1:lopw)
 
			else !if(winput(ial+2:ial+2).eq.'(')then
				winput=winput(1:jii)//'setoption('//chii(1:lec)//','//&
					winput(ial+2:lop-1)//')'//winput(lop:lopw)
 
			endif !if(winput(ial+2:ial+2).eq.'(')  15567
			lopw=len_trim(winput)
			ial=j_nextword(winput,jii+10+lec,lopw,'->')
		enddo !while(ial.lt.lopw)  15542
		!write(6,*)'<222>',lope, winput(1:lopw)
 
	end subroutine !subroutine getoptions()
 
	! end subroutine
 
	subroutine jparse(win,isoutput)
		character*(*),intent(in):: win
		logical,intent(in)::isoutput  !in output getelem is changed into setelem
		logical :: isat
		integer,dimension(200)::mothers  !last mother for each level
		integer,parameter::nmax=200
		integer,dimension(nmax)::list
		integer::le,level,ial,nl,nr,ir,ir1,ir9,iv,ndots,idots,iir,nlist,ial1,ial2
		logical isgotonew
		!	integer,parameter::maxlevels=10
		!	integer,dimens
 
		le=len(win)
		node=0
		mother=0
		child=0 !first child
		lastchild=0
		sister=0  !right
		brother=0 !left
		nn=0
		nchild=0
 
 
		nchildopt=0 !number of option children
 
 
		level=1  !root is at level 1
		mothers(1)=1   !last node at root level is node 1
		node(1)=j_fbloop+3
		mother(1)=-1
		levels(1)=1
		!current node !postive nodes are functions negative object
		ial=1	!if node is object and it does not have childern it is orinnary object otherwie
		if(j_p2)write(6,*)'<324 ',win(1:le)
		!it is MATRIX
		nl=0  !left parentese
		nr=0 !right parnetheses
		ir=j_nextlim(win,ial,le,',()')  !ial is the start of node name
 
 
 
		do while(ial.le.le)
			ir=j_nextlim(win,ial,le,',()')  !ir is the delimiter after next node name
 
			ial1=max(ial-1,1) !the charcter  before start, to avoid reference to character 0
			ial2=max(ial-2,1) ! the character before the previous character
			!if(j_p)write(6,*)'                                                                        ',&
			!win(ial1:ial1),win(ial:ir-1)
			!write(6,*)'tAS:',le,win(1:le)
			!	if(j_p)write(6,*)'<666STARTing node******** ',nn+1,win(ial2:ial1),'%',win(ial:ir-1),'#',ir,win(ir:ir)
			if(ial.eq.le)then
				if(win(le-1:le).eq.'()')exit
			endif !if(ial.eq.le)  15654
			! inde=index(win(ial:le),'()')
			! if(inde.gt.0)write(6,*)'ial,le,inde()',ia,le,inde
 
			nn=nn+1
			if(nn.gt.maxnode)then
				write(6,*)'*j* increase maxnode'
				j_err=.true.;return
			endif !if(nn.gt.maxnode)  15661
			!	write(6,*)'startnode ',nn,' ial,le ',ial,le
			ir1=ir-1
			ir9=ir
			if(ir.gt.le)ir9=ir1
 
			!*************** new level
			!new level
			if(win(ir9:ir9).eq.'(')then  !at the end this is not true but we do not refer outsdie win
				!	if(j_p)write(6,*)'LOPUssa VASEN SULKU'
				!the current node is eiher function or matrix/list and next node is at next level
				isgotonew=win(ial:ir1).eq.'goto'
				iv=j_isin(win(ial:ir1),j_functions,j_nfunctions_)
				if(j_p)write(6,*)'<23 >iv',iv,win(ial:ir1)
				!	if(win(ial:ir-1).eq.'sin')write(6,*)'22 ',jnfunctions,jfunctions(30:40)
				mother(nn)=mothers(level)
 
				if(iv.eq.1)then !setoption
					if(node(mother(nn)).eq.4)then
 
						ipil=index(win(ir1+2:ir1+2+j_lenoption),',')
						!		write(6,*)'<55 ',ipil,win(ir1:ir1+10)
						read(win(ir1+2:ir1+ipil),'(i8)')iopt
						write(6,*)'**illegal location for option ',j_options(iopt)
 
						j_err=.true.
						return
					endif !if(node(mother(nn)).eq.4)  15682
				endif !if(iv.eq.1)  15681
				!		if(nn.eq.10)write(6,*)'tasmutsiee*********%%%%%%%%%%%%',mothers(level+1),&
				!		'level ',level,levels(nn)
 
				mutsi=mother(nn)
 
				if(nchild(mutsi).eq.0)then
					child(mutsi)=nn
				else !if(nchild(mutsi).eq.0)then
					sister(lastchild(mutsi))=nn
					brother(nn)=lastchild(mutsi)
					!			if(nn.eq.10)write(6,*)'tasbro7474747ther10ee*********%%%%%%%%%%%%'
				endif !if(nchild(mutsi).eq.0)  15698
				nchild(mutsi)=nchild(mutsi)+1
				lastchild(mutsi)=nn
 
 
				level=level+1
				mothers(level)=nn
 
				levels(nn)=level
 
 
				!	levels(nn)=level
				if(iv.gt.0)then !functions are stored with positive value
 
					node(nn)=iv
 
					if(iv.eq.1)nchildopt(mother(nn))=nchildopt(mother(nn))+1
					nchild(nn)=0
					nchildopt(nn)=0
 
 
 
 
 
				else !if(iv.gt.0)then
 
					! not function  then looks like matrix
					if(win(ial:ial).ge.'0'.and.win(ial:ial).le.'9')then
						write(6,*)'( at loc ',ir9,' cannot be preceded by a constant'
						j_err=.true.;return
					endif !if(win(ial:ial).ge.'0'.and.win(ial:ial).le.'9')  15731
					!	write(6,*)'djjdjdj'
 
					if(ir1.ge.ial)then
						!	write(6,*)'<ivrr',ial,ir1,win(ial:ir1)
						if(isgotonew.and.(win(ial-1:ial-1).eq.'('.and.win(ir1+1:ir1).eq.')'.or. &
								win(ial-1:ial-1).ne.'('))then
							!goto(igo,ad1,ad2) igo is not address others are
							call getiv(win(ial:ir1),iv,.true.)
							! call j_getname(iv)
							! write(6,*)'ivgototas',j_oname(1:j_loname)
 
						else
							call getiv(win(ial:ir1),iv,.false.)
						endif !if(isgotonew.and.(win(ial-1:ial-1).eq.'('.and.win(ir1+1:ir  15739
 
						!	iv=j_object(win(ial:ir1)) !is it kno object
 
						!  write(6,*)'<25 >',ial,ir1,iv
 
						if(j_err.or.nexist)return
 
						if(j_p)write(6,*)'GETMATRIX NODE',nn
 
						node(nn)=2  !the function getelem
						if(nn.eq.3)node(nn)=3  !in output side setelem
 
						child(nn)=nn+1
						nchild(nn)=1
						if(iv.eq.1)nchildopt(nn)=1
						lastchild(nn)=nn+1
						nn=nn+1
						if(nn.gt.maxnode)then
							write(6,*)'*j* increase maxnode'
							j_err=.true.;return
						endif !if(nn.gt.maxnode)  15766
						levels(nn)=level
						node(nn)=-iv
						!		islabel(nn)=j_intloc(ivlabel,nlabel,iv).gt.0
						mother(nn)=mothers(level)
						!zero arguments
 
						!				if(nn.eq.10)write(6,*)'mude%%%%%level',mothers(level+1)
						if(j_p)write(6,*)'***MAKING NODE ',nn,node(nn), ' mutsi ',mother(nn),nchild(nn)
						if(j_p)write(6,*)'WWWIIN',win(ial:ial+2)
					endif !if(ir1.ge.ial)  15737
					if(j_p.and.iv.eq.0)write(6,*)'ZERO,nn',nn
 
 
				endif !if(iv.gt.0)  15716
				ial=ir+1
				if(ial.gt.le)then
					write(6,*)'illegal syntax at the end (1)'
					j_err=.true.
					return
				endif !if(ial.gt.le)  15785
 
				if(j_p)write(6,*)'<333>',win(1:le), 'looking',win(1:ir+1)
 
				if(win(ir+1:ir+1).eq.')')then  !no arguments in fun or matrix
					ial=ir+3
					if(j_p)write(6,*)'<884 no arguments',ial,win(ial:ial)
					child(nn)=0
					lastchild(mother(nn))=nn
					level=level-1
				endif !if(win(ir+1:ir+1).eq.')')  15793
				if(j_p2)write(6,*)'cyclehere,IV',iv
				!if(iv.eq.0)nn=nn-1
				cycle
			endif !if(win(ir9:ir9).eq.'(')  15672
			!! ***************************************end ( and new level
 
			!not : if(win(ir9:ir9).eq.'(')  nex limiter was , or )  in any casenew child for mother
			! node is ordinary object
			if(win(ir9:min(le,ir9+1)).eq.',,')then
				write(6,*)'consecutive commas not allowed:'
				write(6,*)win(1:min(le,ir9+1))
				j_err=.true.
				return
			endif !if(win(ir9:min(le,ir9+1)).eq.',,')  15808
 
			isat=win(ial:ial).eq.'@'
			if(j_p)write(6,*)'TASSA OLLAAN, node ial, ir1',nn,ial,ir1,win(ial:ir1),win(1:lopw)
			!	if(ial.gt.ir1.and.p2)write(6,*)'<778 ialgt.ir1 ,CYCLe ',win(ial:le)
			if(ial.gt.ir1)then
				nn=nn-1
				if(j_p2)write(6,*)'<<55NNTAS',nn
				lastchild(mother(nn))=nn
				ial=ial+1
				level=level-1
				cycle
			endif !if(ial.gt.ir1)  15818
			ilist=0
			nlist=0
			if(ir1.ge.ial)then
				idots=0
				if(win(ial:ial).ge.'0'.and.win(ial:ial).le.'9')then
					!	if(j_p)write(6,*)'<234 >',ial,ir
					goto 80
99 			write(6,*)'*j* error when reading from ',win(ial:ir1)
					j_err=.true.;return
 
	80			read(win(ial:ir1),'(i8)',err=99)iv  !numeric constant number
				else !if(win(ial:ial).ge.'0'.and.win(ial:ial).le.'9')then
					!	if(j_p)write(6,*)'<234 >',ial,ir
					!	subroutine j_getdots(i1,i2,list,n,nmax) !
					idots=index(win(ial:ir1),'...')
					iir=ir1
					if(idots.gt.0)iir=ial+idots-2
					if(j_p)write(6,*)'ivbef',ial,iir,win(ial:iir)
 
					if(isat)then
 
						iv= j_object(win(ial+1:iir))
						if(iv.le.0)then
							write(6,*)win(ial+1:iir),' is not an object'
							j_err=.true.;return
						endif !if(iv.le.0)  15848
						if(j_otype(iv).ne.j_iplist)then
							call j_printname('* ',iv,' is not a list')
							j_err=.true.;return
						endif !if(j_otype(iv).ne.j_iplist)  15852
						nlist=j_o(iv)%i(1)
						if(.not.allocated(j_dotlist))then
							allocate(j_dotlist(1:max(100,nlist)))
						elseif(size(j_dotlist).lt.nlist)then
							deallocate(j_dotlist)
							allocate(j_dotlist(1:nlist))
						endif !if(.not.allocated(j_dotlist))  15857
 
						j_dotlist(1:nlist)=j_o(iv)%i2(1:nlist)
						if(j_p)write(6,*)'<7377@list',j_dotlist(1:nlist)
						iv=j_dotlist(1)
						ilist=1
					else
						nlist=0
						j_yes=iir.lt.le
						if(j_yes)j_yes=win(ial-1:ial-1).eq.'('.and.win(iir+1:iir+1).eq.')'
						!	if(isgotonew.and.(win(ial-1:ial-1).eq.'('.and.win(iir+1:iir+1).eq.')'.or. &
						if(isgotonew.and.(j_yes.or. &
								win(ial-1:ial-1).ne.'('))then
 
 
							!goto(igo,ad1,ad2) igo is not address others are
							call getiv(win(ial:iir),iv,.true.)
							!				call j_getname(iv)
							!		write(6,*)'gotoivhere ',win(ial:iir),iv,j_inp(1:j_linp),'j_yes',j_yes
							!		if(j_yes
 
						else
							call getiv(win(ial:iir),iv,.false.)
						endif !if(isgotonew.and.(j_yes.o  15873
						!	call getiv(win(ial:iir),iv,isgotonew)
						if(j_err.or.nexist)return
						ilist=0
					endif !if(isat)  15845
 
					! if(isgoto2.and.ila.gt.0.and.idots.gt.0)then
					! write(6,*)'dots .. does not work with labels (yet)'
					! j_err=.true.
					! return
 
					! endif !if(isgoto2.and.ila.gt.0.and.idots.gt.0)  14663
					if(j_p)write(6,*)win(ial+1:iir),' ivtas ',iv,win(ial:iir)
 
 
				endif !if(win(ial:ial).ge.'0'.and.win(ial:ial).le.'9')  15830
 
 
 
				!		write(6,*)'<5454 ',win(ial:ial)
 
 
				if(idots.gt.0)then
					iv0=iv
					!	iv2=j_object(win(iir+4:ir1))
					!write(6,*)'<297',iir+4,ir1,win(iir+4:ir1)
					!		call getiv(win(iir+4:ir1),iv)
					if(j_err)return
					!		iv2=iv
					!		iv=iv0
					!write(6,*)'<befdots iv,iv2
					call j_getname(iv)
 
 
					!	write(6,*)j_oname(1:j_loname),' ',win(iir+4:ir1),'  ',win(ial:iir)
					!		write(6,*)'hep'
					call j_getdots(iv,win(iir+4:ir1),nlist)
 
					!			write(6,*)'nlist ',nlist
					if(j_err)write(6,*)win(max(1,ir1-15):ir1)
					!write(6,*)'<43iv,iv2,nlist',iv,iv2,nlist
				endif !if(idots.gt.0)  15907
				!if(iv.le.0)call j_getobject(0,win(ial:ir1),j_ipreal,iv)
 
				!			endif
				if(j_p)write(6,*)'ilist,nlist,idots',ilist,nlist,idots
				do while(ilist.le.nlist)
					!			write(6,*)'ilist,iv',ilist,nn,iv
					! if(iv.eq.2814)p=.true.
					! if(iv.eq.2814)p2=.true.
					! if(iv.eq.2814)write(6,*)j_inp(1:j_linp)
					! if(iv.eq.2814)j_v(j_ivdollar)=783.d0
 
					node(nn)=-iv
					mother(nn)=mothers(level)
					!		if(nn.eq.10)write(6,*)'mude%shhshshhh%level',mothers(level+1)
					mutsi=mother(nn)
 
					if(nchild(mutsi).eq.0)then
						child(mutsi)=nn
					else !if(nchild(mutsi).eq.0)then
						sister(lastchild(mutsi))=nn
						brother(nn)=lastchild(mutsi)
 
						!				if(nn.eq.10)write(6,*)'tasbrother10&&&&&&&&&&&&&&&&&&&&&&&&&&&'
					endif !if(nchild(mutsi).eq.0)  15943
					nchild(mutsi)=nchild(mutsi)+1
					lastchild(mutsi)=nn
 
					ilist=ilist+1
					if(ilist.le.nlist)then
						iv=j_dotlist(ilist)
						nn=nn+1
						levels(nn)=level
						!				write(6,*)'startnode2 ',nn
					endif !if(ilist.le.nlist)  15955
 
				enddo !while(ilist.le.nlist)  15931
 
				if(ir.lt.le)then
					do while(win(ir:ir).eq.')')
						level=level-1
						ir=ir+1
						if(ir.gt.le)exit
					enddo !while(win(ir:ir).eq.')')  15965
				endif !if(ir.lt.le)  15964
			endif !if(ir1.ge.ial)  15828
			ial=ir+1
			if(ial.gt.le)exit
		enddo !while(ial.le.le)  15645
		if(j_p2)write(6,*)'beftas'
		if(j_p3)call printparse(win)
 
		return
		!WHAT is done here  ??? was it important
		nn2=0
		inode=1
		do while (inode.le.nn)
			nn2=nn2+1
 
			if(node(inode).ne.0)then
				node(nn2)=node(inode)
				child(nn2)=child(inode)
				lastchild(nn2)=lastchild(inode)
				mother(nn2)=mother(inode)
				!		if(nn2.eq.10)write(6,*)'mude%djjdjdjdjd&&el',mothers(level+1)
				sister(nn2)=sister(inode)
				brother(sister(inode))=sister(nn2)
				!	if(sister(inode).eq.10)write(6,*)'tasbrlopusother10***********************'
				inode=inode+1
			else !if(node(inode).ne.0)then
 
				if(j_p2)write(6,*)'tas nolla',inode
				node(nn2)=node(inode+1)
				child(nn2)=child(inode)
				lastchild(nn2)=lastchild(inode)
				mother(nn2)=mother(inode)
				sister(nn2)=sister(inode)
				brother(sister(inode))=nn2
				inode=inode+2
			endif !if(node(inode).ne.0)  15985
 
 
		enddo !while (inode.le.nn)  15982
		nn=nn2
		return
 
	end subroutine !subroutine jparse(win,isoutput)
 
	subroutine getiv(inp,iv,isgoto)
		character*(*)::inp
		integer::ia,iv
		logical isgoto  !must be address
		!	write(6,*)'**getiv ',inp,'/',inp,isgoto
		if(oneline.and.inp.eq.input.or..not.oneline.and.inp.eq.j_inp(1:j_linp))then
			write(6,*)'**nothing to be done with ',inp
			j_err=.true.
			return
		endif !if(oneline.and.inp.eq.input.or..not.oneline.and.inp.eq.j_i  16019
 
		if(isgoto)then
			iv=j_isin(inp,label,nlabel)
			if(iv.le.0)then
				write(6,*)inp,' is not an address'
				j_err=.true.
			endif !if(iv.le.0)  16027
			return
		endif !if(isgoto)  16025
		iv=j_object(inp)
 
		if(iv.gt.0.and.iv.le.j_predefined)return
		if(iv.eq.0.and.oneline.and.(inp//';'.eq.input.or.inp//';'.eq.j_inp(1:j_linp)))then
			write(6,*)inp,' does not exist'
			nexist=.true.
			j_err=.true.
			return
 
		endif !if(iv.eq.0.and.oneline.and.(inp//';'.eq.input.or.inp//';'.  16036
		lenp=len(inp)
		if(j_p)write(6,*)'<65 getiv israg,iv ',isarg,iv, 'isgoto ',isgoto
 
		!		if(isgoto)then
 
		!		endif
		if(isarg.and.iv.gt.0)then
			if(iv.le.j_predefined)return
			if(j_otype(iv).eq.j_ipchar)return
			ia=j_inlistobject(iv,ivarg)
			!	write(6,*)'<7379',iv,ia,isarg
			if(j_err)return
			if(ia.le.0)then
				iv=j_object(j_varname1(1:letr)//inp)
				!	write(6,*)'<7888',iv,ia,j_tempchar2(1:letr)//winput(ial:lop)
 
				if(iv.le.0)iv=j_getobject(0,j_varname1(1:letr)//inp,j_ipreal)
 
			else !if(ia.le.0)then
				used(ia)=.true.
			endif !if(ia.le.0)  16055
		elseif(iv.le.0)then !if(isarg.and.iv.gt.0)then
			!	write(6,*)'<6336:',inp
			if(inp(1:1).eq."'")then
				iv=j_defchar(0,inp)
				if(j_err)return
				!		write(6,*)j_o(iv)%i
				! elseif(inp(1:1).eq.'~'.and.inp(lenp:lenp).eq.'~')then
				! call j_defchar(0,"'"//inp(2:lenp-1)//"'",iv)
			else
				if(isarg)then
					!	write(6,*)'bef:',j_varname1(1:letr),'+',inp
					iv=j_getobject(0,j_varname1(1:letr)//inp,j_ipreal)
				else
					iv=j_getobject(0,inp,j_ipreal)
				endif !if(isarg)  16073
			endif !if(inp(1:1).eq."'")  16066
		endif !if(isarg.and.iv.gt.0)  16049
 
		if(j_p2)write(6,*)'getiv ','/'//inp//'/',iv,' lenp ',lenp
		return
 
 
	end subroutine !subroutine getiv(inp,iv)
 
	subroutine printparse(title)
		character*(*)::title
		write(6,*)title
		write(6,*)'number of nodes ',nn, 'nodefunctions'
		do i=1,nn
			if(node(i).gt.0)then
				write(6,*)i,node(i),j_functions(node(i))
			elseif(i.gt.1.and.node(max(i-1,1)).eq.1)then !if(node(i).gt.0)then
				write(6,*)i,node(i),j_options(-node(i))
			else !if(node(i).gt.0)then
				call j_getname(-node(i))
				write(6,*)i,node(i),j_oname(1:j_loname)
			endif !if(node(i).gt.0)  16093
		enddo !i=1,nn  16092
 
		write(6,'(a,20i6/20i6)')'nodes  ',(j,j=1,nn)
		write(6,'(a,20i6/20i6)')'nodes  ',node(1:nn)
		write(6,'(a,20i6/20i6)')'levels ',levels(1:nn)
		write(6,'(a,20i6/20i6)')'mother ',mother(1:nn)
		write(6,'(a,20i6/20i6)')'child  ',child(1:nn)
		write(6,'(a,20i6/20i6)')'sister ',sister(1:nn)
		write(6,'(a,20i6/20i6)')'brother ',brother(1:nn)
		write(6,'(a,20i6/20i6)')'nchild ',nchild(1:nn)
		write(6,'(a,20i6/20i6)')'lastch ',lastchild(1:nn)
 
		!	w
		write(6,'(a,20i6/20i6)')'nchildopt ',nchildopt(1:nn)
 
	end subroutine !subroutine printparse(title)
 
	subroutine tekut(nteku)
		!	integer,dimension(:),intent(out)::teku
		integer,intent(out)::nteku
		integer::inode,narg,ino,mutsi,nin  !local
		if(j_p)write(6,*)'heihei,starting teku,p nteku ',p,nteku,' ISGOTO',isgoto
 
 
		!inode=child(1)
		inode=lastchild(1)
		!p=.true.
		if(j_p2)write(6,*)'<1>inode ',inode,node(inode)
		!childer of a node are treated from right to left
		! this means that options are treated before arguments
 
		!	nteku=0
		!	itemporal=j_mxnamedv
		!write(6,*)'heihei ',p,nteku,itemporal
		icodenode=0
100	continue
		do while(lastchild(inode).gt.0) !go to bottom of the tree in the right
			inode=lastchild(inode)  !child(inode)
 
			if(node(inode).eq.1)call initcodenode(inode,nteku,icode,icodenode)
 
			if(j_p2)write(6,*)'1 moving down to inode ',inode,' innode ',node(inode),' nteku ',nteku
		enddo !while(lastchild(inode).gt.0)  16137
 
		if(node(inode).gt.0)then !function wihtout arguments, cannot be setoption
			!ntek=ntekuf(3)
			itemporal=itemporal+1
			call teku(nteku+1,node(inode))
			nodetoteku(inode)=nteku+1
			call teku(nteku+2,0)
			!		if(j_minarg(node(inode)).gt.0)write(6,*)'66node ',inode
			!		if(j_minarg(node(inode)).gt.0)call prinTparse('tas')
			call teku(nteku+3,itemporal)
			if(j_minarg(node(inode)).gt.0)goto 99
			if(j_p)write(6,*)'<3773withoutargs',j_o(ivteku)%i(nteku+1:nteku+3),' out temporal',itemporal,nteku+3
			nteku=nteku+3
			node(inode)=-itemporal
 
		endif !if(node(inode).gt.0)  16145
 
		!		if(node(inode).eq.0)write(6,*)' taspanol',inode
 
		if(brother(inode).ne.0)then
			inode=brother(inode)
			if(j_p2)write(6,*)'2 moving left to node',inode,' innode ',node(inode),' nteku ',nteku
			if(node(inode).eq.1)call initcodenode(inode,nteku,icode,icodenode)
			goto 100
		endif !if(brother(inode).ne.0)  16163
 
 
 
		inode=mother(inode) ! mother of the bottom node whose all arguments are cleared
		if(j_p2)write(6,*)'3 moving up to node',inode,' innode ',node(inode),' nteku ',nteku
		if(inode.eq.1.or.inode.lt.0)then
			if(j_p)write(6,'(a,(20i5/))')'<fin>',j_o(ivteku)%i(1:nteku)
			if(j_p)write(6,*)nn,'NODES:'
			if(j_p)write(6,'(20i5)')node(1:nn)
 
			call teku(nteku+1,0)
			return
		endif !if(inode.eq.1.or.inode.lt.0)  16174
 
200	if(node(inode).eq.1)then
			ic=child(inode)
			nargo=nchild(inode)-nchildopt(inode)-1
 
			if(inode.eq.icodenode)then
				!	teku(icode+3)=itemporal
				if(nargo.gt.1.or.nargo.eq.0)then
					write(6,*)'codeoption ',j_options(-node(ic)),' can have only one argument'
					j_err=.true.;return
				endif !if(nargo.gt.1.or.nargo.eq.0)  16189
				!setcodeopt, iopt, jumpadress,outputof code, io of the function
				!icode link to setocode
				!	write(6,*)'icode ',icode
				call teku(icode+3,itemporal)
				!	teku(icode+4)=nodetoteku(mother(inode))
				noption2=noption2+1
				optionmother2(noption2)=mother(inode)
				optionloc2(noption2)=icode
				!write(6,*)'code,indoe,nodetoteku,',inode,icode, teku(icode+4)			!io of the function
				if(nteku.eq.icode+4)then   !single argument without computing anything
					call teku(icode+2,0) !just continue without computing
					call teku(icode+3,-node(sister(ic)))
				else !if(nteku.eq.icode+4)then
					!put return to the end
					!ntek=ntekuf(2)
					nteku=nteku+1
					call teku(nteku,0)
					call teku(icode+2,nteku+1)
 
				endif !if(nteku.eq.icode+4)  16202
				!		if(j_p)write(6,*)'<3883codenode,nteku,icode',nteku,icode,j_o(ivteku)%i(icode:icode+3),itemporal
				icodenode=0
			else !if(inode.eq.icodenode)then
				!ntek=ntekuf(nargo+4)
				call teku(nteku+1,1)  !set option
				nodetoteku(inode)=nteku+1
				noption=noption+1
				optionloc(noption)=nteku+1
				optionmother(noption)=mother(inode)
				call teku(nteku+2,-node(ic) )   !the option index only nonoption arguments are treated
 
				call teku(nteku+3,nargo)
				if(j_p)write(6,*)'<757,inode,nteku,nargo,j_o(ivteku)%i(nteku+1:nteku+3',inode,nteku+3,nargo,j_o(ivteku)%i(nteku+1:nteku+3)
				nteku=nteku+3
				do ii=1,nargo
					ic=sister(ic)
					nteku=nteku+1
					call teku(nteku,-node(ic))
 
				enddo !ii=1,nargo  16227
				nteku=nteku+1 !space for mother
 
 
 
			endif !if(inode.eq.icodenode)  16187
		elseif(node(inode).eq.0)then !if(node(inode).eq.1)then
			node(inode)=node(child(inode))  !-teku(nteku)
			if(brother(inode).ne.0)then
				inode=brother(inode)
				if(j_p2)write(6,*)'4 ZERONODE moving LEFT to node',inode,' innode ',node(inode),' nteku ',nteku
 
			else !if(brother(inode).ne.0)then
 
				inode=mother(inode)
				if(j_p2)write(6,*)'4 ZERONODE moving UP to node',inode,' innode ',node(inode),' nteku ',nteku
 
			endif !if(brother(inode).ne.0)  16240
 
			goto 100
		elseif(node(inode).gt.0)then !if(node(inode).eq.1)then
			ino=nteku  !poisat
			nargo=nchild(inode)-nchildopt(inode)
			if(j_p2)write(6,*)'inode nargo',inode,nargo,nchild(inode)-nchildopt(inode)
			if(mother(inode).eq.1.and.brother(inode).ne.0)then
				!  list2(inputs)
				nin=nargo
				inode2=child(inode)
				do ii=1,nargo
					inlist(ii)=-node(inode2)
					inode2=sister(inode2)
				enddo !ii=1,nargo  16260
			elseif(mother(inode).eq.1)then !if(mother(inode).eq.1.and.brother(inode).ne.0)then
				!  list2(inputs)
				nout=nargo
				inode2=child(inode)
				do ii=1,nargo
					outlist(ii)=-node(inode2)
					if(outlist(ii).gt.j_nv)then
						write(6,*)'*Constant cannot be output'
						j_err=.true.;return
 
					endif !if(outlist(ii).gt.j_nv)  16270
					inode2=sister(inode2)
				enddo !ii=1,nargo  16268
 
 
			else !if(mother(inode).eq.1.and.brother(inode).ne.0)then
 
				call teku(nteku+1,node(inode))
 
				nodetoteku(inode)=nteku+1
				call teku(nteku+2,nargo)
				if(j_p3)write(6,*)'nargo*** ',nargo,' intopos ',nteku+2
				!	if(node(inode).eq.j_fgoto.and.nargo.gt.1)call	teku(nteku,j_fgoto2)
				if(j_minarg(node(inode)).gt.nargo)goto 99
				inode2=child(inode)
				nteku=nteku+2
 
				!		nteku=nteku+2
				do ii=1,nargo
 
					nteku=nteku+1
					call teku(nteku,-node(inode2))
					! if(node(inode).eq.j_fgoto.and.narg.gt.1.and.ii.lt.nargo)then
					! iv=j_o(ivteku)%i(nteku)
					! !		ilo=j_intloc(ivlabel,nlabel,iv)
					! if(ilo.eq.0)then
					! call j_getname(iv)
					! write(6,*)'** argument ',j_oname(1:j_loname), ' not a label'
					! j_err=.true.
					! endif !if(ilo.eq.0)  15062
					!	endif !if(node(inode).eq.j_fgoto.and.narg.gt.1.and.ii.lt.nargo)  15059
					inode2=sister(inode2)
 
				enddo !ii=1,nargo  16292
				!	if(node(inode).ne.j_fgoto.or.nargo.gt.1)then
				!	if(nargo.ge.1)then
				itemporal=itemporal+1
				nteku=nteku+1
				call teku(nteku,itemporal)
 
				!this is replaced in 'tasteku' point
				if(j_p3)write(6,*)'nteku ',nteku,itemporal
				!	endif !if(nargo.ge.1)  15079
 
 
				node(inode)=-itemporal
				if(j_p)write(6,*)'<555,ino,nteku,teku,nargo,',ino,nteku,j_o(ivteku)%i(max(ino,1):nteku),' nargo',nargo
			endif !if(mother(inode).eq.1.and.brother(inode).ne.0)  16256
		else !if(node(inode).eq.1)then
			!	write(6,*)'HUPSISTA'
 
		endif !200	if(node(inode).eq.1)  16183
		if(brother(inode).ne.0)then
			inode=brother(inode)
			if(j_p2)write(6,*)'5 moving LEFT to node',inode,' innode ',node(inode),' nteku ',nteku
			if(node(inode).eq.1)call initcodenode(inode,nteku,icode,icodenode)
			goto 100
		else !if(brother(inode).ne.0)then
			inode=mother(inode)
			if(j_p2)write(6,*)'6 moving UP to node',inode,' innode ',node(inode),' nteku ',nteku
			if(inode.eq.1.or.inode.lt.0)then
				if(j_p)write(6,'(a,(20i5/))')'<finhere>',j_o(ivteku)%i(1:nteku)
				!if(j_p)write(6,*)'**isgoto',isgoto,nn,'NODES:'
				if(j_p)write(6,'(20i5)')node(1:nn)
 
				!ntek=ntekuf(1)
				call teku(nteku+1,0)
				return
			endif !if(inode.eq.1.or.inode.lt.0)  16334
			goto 200
		endif !if(brother(inode).ne.0)  16326
		return
99	write(6,*)'function ',node(inode),' ',j_functions(node(inode)),'needs ',j_minarg_(node(inode)),' arguments'
	!	write(6,*)j_functions(node(inode),' ',j_functions(node(inode)),'needs ',j_minarg_(node(inode)),' arguments'

		j_err=.true.
 
		return
 
	end subroutine tekut !subroutine tekut(teku,nteku)
 
	subroutine initcodenode(inode,nteku,icode,icodenode)
		!		integer,dimension(:)::teku
		!	write(6,*)'inode',inode
		ic=child(inode)
		!		if(inode.eq.3)write(6,*)'***ic',ic,-node(ic),j_codeoptions,j_codeoption_(j_codeoptions)
		if(j_codeoption_(-node(ic)))then !any(j_codeoptions.eq.node(ic)))then
 
			nteku=nteku+1
			call teku(nteku,j_fbspec+8)   !set codeoptionicodenode
			if(j_p)write(6,*)'toneteku',nteku
			call teku(nteku+1,-node(ic))
			!teku(nteku+2)=place where to jump in setcodeoption function
			!teku(nteku+3) =output of the codeoption
			icode=nteku  !link TO SETCODEOPT
			icodenode=inode
			if(j_p2)write(6,*)'<it was codeoption',j_o(ivteku)%i(nteku+1:nteku+3), 'icodenode ',icodenode
			nteku=nteku+4
 
		endif !if(j_codeoption_(-node(ic)))  16360
 
	end subroutine !subroutine initcodenode(inode,teku,nteku,icode,icodenode)
 
 
 
 
end subroutine !subroutine j_parser(input,ivteku)
 
 
subroutine polishall(win,lopw)
	character*(*) win
	integer,dimension(29)::lefts,rights
	integer,parameter ::noper=29
	character(len=6),dimension(noper):: operin,operout
	integer,dimension(noper)::lec0
	character(len=6)::ch,ch2
	integer::i,iop,lec,lec2,ich,kier,nr,nl,j,i1,j1,ich1,ich2
	logical haka2,hipsu2
	! data operin/'***','**','^','*','/','-','+','.eq.','==','.ne.', &
	! '~=','.le.','<=','.lt.','<','.ge.','>=','.gt.','>','.not.', &
	! '~','.and.','&','.eqv.','.neqv.','.or.','*.','/.'/
	! data operout/'IPOWER','POWER','POWER','MULT','DIV','MINUS','PLUS','EQ','EQ','NE', &
	! 'NE','LE','LE','LT','LT','GE','GE','GT','GT','NOT', &
	! 'NOT','AND','OR','EQV','NEQV','OR','HMULT','HDIV'/
 
	data operin/'*.','/.','***','**','^','*','/','-','+','.eq.','==','.ne.', &
		'~=','.le.','<=','.lt.','<','.ge.','>=','.gt.','>','.not.', &
		'~','.and.','&','.eqv.','.neqv.','.or.','='/
	data operout/'HMULT','HDIV','IPOWER','POWER','POWER','MULT','DIV','MINUS','PLUS','EQ','EQ','NE', &
		'NE','LE','LE','LT','LT','GE','GE','GT','GT','NOT', &
		'NOT','AND','OR','EQV','NEQV','OR','ASSIGN'/
 
	!	data opeout/'intpower','power','power','mult','div','plus','minus',
 
	!	/
	!	logical ::p
	!	p=j_v(j_ivdebug).ge.200.d0
	do i=1,noper
		lec0(i)=len_trim(operin(i))
	enddo !i=1,noper  16411
	!	p=.true.
	!	write(6,*)' '
 
	do iop=1,noper
		ial=1
		ch=operin(iop)
		ch2=operout(iop)
		lec2=len_trim(ch2)
		lec=lec0(iop)
10		ich=jindex(win(1:lopw),ial,ch(1:lec))
		if(ich.gt.0.and.index(win(1:lopw),'[').gt.0.and.j_p)write(6,*)'TASA',ch(1:lec),ich,win(1:lopw),'*',win(1:ich)
		!if(j_p)write(6,*)'iop,ich,ial',iop,ich,ial
		if((iop.eq.4.or.iop.eq.5).and.ich.gt.0.and.win(ich+1:ich+1).eq.'.')then
 
			ial=ich+3
			goto 10
		endif !if((iop.eq.4.or.iop.eq.5).and.ich.gt.0.and.win(ich+1:ich+1  16426
 
		kier=0
		do while(ich.gt.0)
			kier=kier+1
			if(kier.gt.100)stop 'hier'
			!if(j_p)write(6,*)'ich ',ich,' ',win(ich:ich)
			!search left argument
			nr=0 !number of )
			nl=0 !number of (
			haka2=.false.
			hipsu2=.false.
			ichloop:do i=ich-1,1,-1
				if(win(i:i).eq."'")then
					hipsu2=.not.hipsu2
					cycle
				endif !if(win(i:i).eq."'")  16443
				if(win(i:i).eq.']'.and..not.hipsu2)then
					haka2=.true.
					cycle
				endif !if(win(i:i).eq.']'.and..not.hipsu2)  16447
				if(win(i:i).eq.'['.and..not.hipsu2)then
					haka2=.false.
					cycle
				endif !if(win(i:i).eq.'['.and..not.hipsu2)  16451
				if(haka2.or.hipsu2)cycle
				if(win(i:i).eq.')')nr=nr+1
				if(win(i:i).eq.'(')nl=nl+1
				if(j_p.and.nl.gt.nr)write(6,*)'left:nl.gt.nr',nl,nr
				if(nl.gt.nr)exit
				!	if(win(i:i).eq.',')exit
				!	write(6,*)'per ',i,nl,nr,win(i:i),win(i:i).eq.','
				if(nl.eq.nr)then
					if(win(i:i).eq.','.or.win(i:i).eq.'=')exit ichloop
					do iop2=iop+1,noper
						if(i-lec0(iop2)+1.le.0)cycle
						! if(j_p)write(6,*)win(i-lec0(iop2)+1:i),' ',operin(iop2)(1:lec0(iop2))
						if(j_p.and.win(i-lec0(iop2)+1:i).eq.operin(iop2)(1:lec0(iop2))) &
							write(6,*)'<567 ',win(i-lec0(iop2)+1:i)
						if(win(i-lec0(iop2)+1:i).eq.operin(iop2)(1:lec0(iop2)))exit ichloop
					enddo !iop2=iop+1,noper  16464
 
				endif !if(nl.eq.nr)  16462
 
			enddo ichloop !loop:do i=ich-1,1,-1  16442
			i=i+1
			!if(j_p)write(6,*)ch(1:lec),' i',i,' ',win(1:i),'  ',win(i+1:lopw)
			nr=0 !number of )
			nl=0 !number of (
			!if(j_p)write(6,*)win(ich+lec:lopw)
 
			haka2=.false.
			hipsu2=.false.
 
			jloop:	do j=ich+lec,lopw  !search right
				!if(j_p)write(6,*)'j  ',j,' ',win(1:j-1),'?',win(j:j),'?',win(j+1:lopw)
				if(win(j:j).eq."'")then
					hipsu2=.not.hipsu2
					cycle
				endif !if(win(j:j).eq."'")  16486
				if(win(j:j).eq.'['.and..not.hipsu2)then
					haka2=.true.
					cycle
				endif !if(win(j:j).eq.'['.and..not.hipsu2)  16490
				if(win(i:i).eq.']'.and..not.hipsu2)then
					haka2=.false.
					cycle
				endif !if(win(i:i).eq.']'.and..not.hipsu2)  16494
				if(haka2.or.hipsu2)cycle
 
				if(win(j:j).eq.')')nr=nr+1
				if(win(j:j).eq.'(')nl=nl+1
				!	if(j_p.and.(nr.eq.nl.and.nl.gt.0.or.nr.gt.nl
				if(j_p.and.nr.eq.nl.and.nl.gt.0)write(6,*)'exit1'
				if(nr.eq.nl.and.nl.gt.0)exit
				if(j_p.and.nr.gt.nl)write(6,*)'exit2'
				if(nr.gt.nl)exit
				if(j_p.and.win(j:j).eq.','.and.nr.eq.nl)write(6,*)'exitcomma'
				if(win(j:j).eq.','.and.nr.eq.nl)exit
 
				if(nl.eq.nr)then
					if(j_p.and.win(j:j).eq.',')write(6,*)'exitcomma here'
					if(win(j:j).eq.',')exit jloop
 
					do iop2=iop,noper
						!if(j_p.and.j+lec0(iop2)-1.gt.0)write(6,*)'cycle operin ',operin(iop2), j,lec0(iop2),j+lec0(iop2)-1
						!if(j+lec0(iop2)-1.gt.0)cycle
						!	if(j_p)write(6,*)win(j:j+lec0(iop2)-1),'=?= ',operin(iop2)(1:lec0(iop2)),' found'
						if(win(j:j+lec0(iop2)-1).eq.operin(iop2)(1:lec0(iop2)))exit jloop
					enddo !iop2=iop,noper  16514
 
				endif !if(nl.eq.nr)  16510
				!		if(((win(j:j).eq.','.or.win(j:j).eq.'+'.or.win(j:j).eq.'-'.or.&
				!		win(j:j).eq.'*').and.nl.eq.nr).or.nr.gt.nl)exit
			enddo jloop !op:	do j=ich+lec,lopw  16484
			j=j-1
			!if(j_p)write(6,*)'j',j,win(j:lopw)
 
			!i1=i+1
			ich1=ich-1
			j1=j+1
			i1=i-1
 
			ich2=ich+lec
 
 
			if(j_p)write(6,*)'<38383 ',i,ich1,ich2,j,lopw,lec
			if(j_p)write(6,*)'<773',i,ich1,j,ich,win(1:lopw),' eka ',win(1:i-1),' toka ', &
				win(i:ich1), ' kol ',win(ich2:j)
			if(ich1.ge.i)then
				j_tempchar7=win(1:i1)//ch2(1:lec2)//'('//win(i:ich1)//','//win(ich2:j)//')'//&
					win(j1:lopw)
				!write(6,*) 'kuis:',win(i:ich1),'%',win(ich2:j),'#', win(j1:lopw)
 
			else !if(ich1.ge.i)then
				j_tempchar7=win(1:i1)//ch2(1:lec2)//'('//win(ich2:j)//')'//&
					win(j1:lopw)
				! write(6,*)'%'//ch2(1:lec2)//'&'//win(j1:lopw)
				!write(6,*) 'kuis',win(i:ich1),'%',win(ich2:j),'#', win(j1:lopw)
 
			endif !if(ich1.ge.i)  16539
			lopw=len_trim(j_tempchar7)
			win=j_tempchar7
20				ich=jindex(win(1:lopw),ial,ch(1:lec))
			!	if(j_p)write(6,*)'iop,ich,ial,',iop,ich,ial,ch(1:lec)
			if(ich.gt.0)then
				if((iop.eq.4.or.iop.eq.5).and.win(ich+1:ich+1).eq.'.')then
					ial=ich+3
					goto 20
				endif !if((iop.eq.4.or.iop.eq.5).and.win(ich+1:ich+1).eq.'.')  16556
			endif !if(ich.gt.0)  16555
			!	ich=index(win(1:lopw),ch(1:lec))
			!	if(j_p)write(6,*)'<555ich',ich,ch2,'  ',win(1:lopw)
		enddo !while(ich.gt.0)  16433
 
 
	enddo !iop=1,noper  16417
	!remove extra parenthsis  function j_nextlim(inp,ial,lop,limit)
	return
	nl=0
	nr=0
	do i=1,lopw
		if(win(i:i).eq.'(')then
			nl=nl+1
			lefts(nl)=i
		elseif(win(i:i).eq.')')then !if(win(i:i).eq.'(')then
			nr=nr+1
			rights(nr)=i
		endif !if(win(i:i).eq.'(')  16572
	enddo !i=1,lopw  16571
	!	ndone=0
	if(nr.le.0)return
	nremove=0
	ir0=1
	if(lefts(1).eq.1.and.rights(nr).eq.lopw)then
		nremove=nremove+1
		win(1:1)=' '
		win(lopw:lopw)=' '
		ir0=2
	endif !if(lefts(1).eq.1.and.rights(nr).eq.lopw)  16584
	!DIV((MINUS(5302,5303)),5301)
	do i=ir0,nr
		if(win(lefts(i)-1:lefts(i)-1).eq.'('.and.win(rights(i)+1:rights(i)+1).eq.')')then
			nremove=nremove+1
			win(lefts(i)-1:lefts(i)-1)=' '
			win(rights(i)+1:rights(i)+1)=' '
		elseif(j_isletter(win(lefts(i)+1:lefts(i)+1)).and.win(rights(i)-1:rights(i)-1).eq.')')then !if(win(lefts(i)-1:lefts(i)-1).eq.'('.and.win(rights(i)+1:rights(i
			iile=j_nextlim(win,lefts(i)+1,lopw,'(),')
			if(win(iile:iile).ne.'(')cycle
			nremove=nremove+1
			win(lefts(i):lefts(i))=' '
			win(rights(i)-1:rights(i)-1)=' '
		endif !if(win(lefts(i)-1:lefts(i)-1).eq.'('.and.win(rights(i)+1:r  16592
	enddo !i=ir0,nr  16591
	if(nremove.gt.0)then
		nn=0
		do ii=1,lopw
			if(win(ii:ii).ne.' ')then
				nn=nn+1
				if(nn.gt.maxnode)then
					write(6,*)'*j* increase maxnode'
					j_err=.true.;return
				endif !if(nn.gt.maxnode)  16609
				win(nn:nn)=win(ii:ii)
			endif !if(win(ii:ii).ne.' ')  16607
		enddo !ii=1,lopw  16606
		lopw=nn
		if(j_p)write(6,*)'removed ', nremove, ' pairs of unnecessary parenthesis'
	endif !if(nremove.gt.0)  16604
	return
 
 
	! enddo
 
 
end subroutine !subroutine polishall(win,lopw)
 
 
integer function jindex(line,ial,word)
	character*(*) line,word
	logical ::in,inhaka
	in=.false.
	inhaka=.false.
	le=len(line)
	lew=len(word)
	if(lew.gt.le)then
		jindex=0
		return
	endif !if(lew.gt.le)  16635
	do i=ial,le-lew+1
		if(line(i:i).eq."'")then
			in=.not.in
			cycle
		endif !if(line(i:i).eq."'")  16640
		if(line(i:i).eq.'['.and..not.in)then
			inhaka=.true.
			cycle
		endif !if(line(i:i).eq.'['.and..not.in)  16644
		if(line(i:i).eq.']'.and..not.in)then
			inhaka=.false.
			cycle
		endif !if(line(i:i).eq.']'.and..not.in)  16648
		if(inhaka.or.in)cycle
		!	if(.not.in.and..not.inhaka)then
		if(line(i:i+lew-1).eq.word)then
			jindex=i
			return
		endif !if(line(i:i+lew-1).eq.word)  16654
		!	endif !if(.not.in.and..not.inhaka)  16559
	enddo !i=ial,le-lew+1  16639
	jindex=0
end function !integer function jindex(line,ial,word)
integer function j_linkoption(iob,io,mopt,clear,link)
	integer,intent(in)::iob,io,mopt
	logical,optional,intent(in)::clear,link
	integer*2, dimension(2) :: optionmoptio
	integer::loc
	integer :: optiontot
	equivalence(optiontot,optionmoptio)
 
	!	write(6,*)'present clear ',present(clear)
	!write(6,*)'j_nopt',j_nopt
	if(j_nopt.eq.0)then
		j_linkoption=-1
		return
	endif !if(j_nopt.eq.0)  16672
 
	optionmoptio(1)=mopt
	optionmoptio(2)=io
	!	write(6,*)'optiontot',optiontot,j_optiontot(1),' io',io
	do loc=1,j_nopt
		if(j_optiontot(loc).eq.optiontot)goto 60
	enddo !loc=1,j_nopt  16680
	loc=0
60	continue
!	loc=findloc( j_optiontot(1:j_nopt),optiontot,dim=1)
	!	if(j_v(j_ivdollar).eq.5.d0)write(6,*)j_nopt,iob,io,mopt,' loc',loc
		!	if(j_v(j_ivdollar).eq.5.d0.and.j_nopt.gt.0)&
		!	write(6,*)'<777',(j_optionmoptio(1:2,jj),jj=1,j_nopt)
	!	write(6,*)'j_nopt,mopt',j_nopt,mopt,j_optionmoptio(1:2,1),' joptili',j_optionlink(1),' loc',loc
	if(loc.gt.0)then
		!	write(6,*)'linkoption,loc',loc,iob,j_optioniob(loc)
		if(j_optioniob(loc).ne.iob)then
			call j_printname('*j* options of ',j_optioniob(loc),' not properly cleared')
			iio=j_optionmoptio(2,loc)
			call j_debugerr(j_optioniob(loc),iio) !last argument must be integer*4
			j_err=.true.
			j_linkoption=-1
			return
		endif !if(j_optioniob(loc).ne.iob)  16692
		if(j_o(iob)%i(j_optionlink(loc)).eq.0.and..not.present(link))then
			j_linkoption=0
		else !if(j_o(iob)%i(j_optionlink(loc)).eq.0)then
			j_linkoption=j_optionlink(loc)
		endif !if(j_o(iob)%i(j_optionlink(loc)).eq.0.and..not.present(lin  16700
		!	write(6,*)'PRSENT CLEAR',present(clear)
		if(present(clear))then
			if(loc.lt.j_nopt)then
				j_optioniob(loc)=j_optioniob(j_nopt)
				j_optiontot(loc)=j_optiontot(j_nopt)
				j_optionlink(loc)=j_optionlink(j_nopt)
			endif !if(loc.lt.j_nopt)  16707
 
			j_nopt=j_nopt-1
			!	write(6,*)'option ',mopt,'lcleard'
		endif !if(present(clear))  16706
		!		write(6,*)'linkoptio',j_linkoption
	else !if(loc.gt.0)then
		j_linkoption=-1
 
	endif !if(loc.gt.0)  16690
	return
 
end function !integer function j_linkoption(iob,io,mopt,clear)
 
integer function j_codelink(iob,io,jmcode)
	integer, intent(in):: iob,io,jmcode
	integer*2, dimension(2) :: optionmoptio
	integer::loc
	integer :: optiontot
	equivalence(optiontot,optionmoptio)
	!	write(6,*)'<777 nopt2,in j_nopt2,jmcode,iob,io',j_nopt2,jmcode,iob,io
	! do kii=1,j_nopt2
 
	! write(6,*)j_optionmoptio2(1:2,kii)
	! enddo
	if(jmcode.le.0.or.jmcode.gt.j_noptions)then
		write(6,*)'*j* illegal argument in j_codelink'
		j_err=.true.;return
	endif !if(jmcode.le.0.or.jmcode.gt.j_noptions)  16736
	!	write(6,*)'j_nopt2',j_nopt2,jmcode,io,j_optioniob2(1),j_optionmoptio2(1:2,1),j_optionlink2(1)
	if(j_nopt2.eq.0)then
		j_codelink=0
		return
	endif !if(j_nopt2.eq.0)  16741
 
	optionmoptio(1)=jmcode
	optionmoptio(2)=io
	do loc=1,j_nopt2
		if(j_optiontot2(loc).eq.optiontot)goto 60
	enddo !loc=1,j_nopt2  16748
	loc=0
60	continue
!	loc=findloc( j_optiontot2(1:j_nopt2),optiontot,dim=1)
	if(loc.gt.0)then
		if(j_optioniob2(loc).ne.iob)then
			call j_printname('illegal code option in ',j_optioniob2(loc),' ')
			iio=j_optionmoptio2(2,loc)
			call j_debugerr(j_optioniob2(loc),iio) !last argument must be integer*4
			j_err=.true.
			j_codelink=0
			return
		endif !if(j_optioniob2(loc).ne.iob)  16755
		j_codelink=j_optionlink2(loc)
		if(loc.lt.j_nopt2)then
			j_optioniob2(loc)=j_optioniob2(j_nopt2)
			j_optiontot2(loc)=j_optiontot2(j_nopt2)
			j_optionlink2(loc)=j_optionlink2(j_nopt2)
		endif !if(loc.lt.j_nopt2)  16764
		j_nopt2=j_nopt2-1
	else !if(loc.gt.0)then
		j_codelink=0
 
	endif !if(loc.gt.0)  16754
	!	write(6,*)'<88 nopt2,out',j_nopt2
	!		write(6,*)'codelink',j_codelink
end function !integer function j_codelink(iob,io,jmcode)
 
double precision function j_codevalue(iob,link)
	integer,intent(in)::iob,link
	if(link.le.0.or.link.gt.j_o(iob)%i(0))then
		write(6,*)'*j* illegal link in j_codevalue ',link,' i(0) ',j_o(iob)%i(0)
		j_err=.true.;return
	endif !if(link.le.0.or.link.gt.j_o(iob)%i(0))  16780
	! iofilter=j_linkopt2(j_mfilter)
	! dofilter=iofilter.ne.0
	! ivfilter=j_o(iob)%i(j_linkopt2(j_mfilter)-1)
	!	j_linkopt2(j_o(iob)%i(io+1) )=io+4  !where to start
 
	!  write(6,*)'setcodeopt,option',j_o(iob)%i(io+1),' jumpto ' ,j_o(iob)%i(io+2),'output ',j_o(iob)%i(io+3)
	!io=j_o(iob)%i(io+2)
	!write(6,*)'jumpnow ',io
	!write(6,*)'<777',link,j_o(iob)%i(link-2),j_o(iob)%i(link-2)
	if(j_o(iob)%i(link-3).ne.0)call dotrans(iob,link)
	!	write(6,*)'{{',10
	j_codevalue=j_v(j_o(iob)%i(link-2))
	!	write(6,*)'iisjump,link,iv',j_o(iob)%i(link-2),link,j_o(iob)%i(link-1)
	!	call j_printname('  ',j_o(iob)%i(link-1),' hellirei')
	return
end function !double precision function j_codevalue(iob,link)
 
 
 
function j_deflistobject(iv,name,list0,list,listold,nres,&
		expand,ivin,ilist)
 
	! list0 number of elements and list list(1:list0)
	! listold list(0:
	! expaand expnd argume tlists
	!ivin put list object
	integer, intent(in):: iv
	character*(*), intent(in):: name
	!integer, intent(out):: ivout
	integer,optional, intent(in):: list0
	integer,optional,dimension(:), intent(in):: list
	integer,optional,dimension(:), intent(in):: listold
	integer,optional, intent(in):: nres
	logical,optional, intent(in):: expand
	integer,optional, intent(in):: ivin
	logical,optional,intent(in)::ilist !make ilist object
	logical::expan,isilist
	!iout=iv
	iplist=j_iplist
	isilist=.false.
	if(present(ilist))isilist=ilist
	if(isilist)iplist=j_ipilist
	list00=0
	expan=.false.
	if(present(expand))expan=expand
	if(present(list0))then
		list00=list0
		if(expan)then
			if(j_otype(list(1)).eq.iplist)then
				if(list0.gt.1)goto 99
				inlist=list(1)
				list00=j_o(listold(1))%i(1)
			endif !if(j_otype(list(1)).eq.iplist)  16832
		endif !if(expan)  16831
	elseif(present(listold))then !if(present(list0))then
 
		if(expan.and.j_otype(listold(2)).eq.iplist)then
			if(listold(1).ne.1)goto 99
			list00=j_o(listold(2))%i(1)
			inlist=listold(2)
 
		else !if(expan.and.j_otype(listold(2)).eq.iplist)then
			expan=.false.
			list00=listold(1)
		endif !if(expan.and.j_otype(listold(2)).eq.iplist)  16840
	elseif(present(ivin))then !if(present(list0))then
		list00=j_o(ivin)%i(1)
	endif !if(present(list0))  16829
	ntot=list00
 
	if(present(nres))ntot=ntot+nres
	ntot=max(ntot,1)
	! if(name.ne.' ')then
	! if(.not.present(ivout))then
	! write(6,*)'*j* J_deflist needs ivout argument'
	! j_err=.true.;return
	! endif
	if(name.eq.' ')then
		ivout=iv
		call j_del(ivout)
	else !if(name.eq.' ')then
		ivout=j_getobject(iv,name,iplist)
	endif !if(name.eq.' ')  16861
	!	write(6,*)'<636',ivout
	! endif
 
	allocate(j_o(ivout)%i(1:4))
	!j_o(iout)%i=0
	j_o(ivout)%i(1)=list00
	j_o(ivout)%i(2)=1
	j_o(ivout)%i(3)=list00
	j_o(ivout)%i(4)=ntot
 
	allocate(j_o(ivout)%i2(1:ntot))
	if(expan)then
		j_o(ivout)%i2(1:list00)=j_o(inlist)%i2(1:list00)
 
	elseif(present(list))then !if(expan)then
 
		j_o(ivout)%i2(1:list00)=list(1:list00)
 
	elseif(present(listold))then !if(expan)then
 
		j_o(ivout)%i2(1:list00)=listold(2:list00+1)
	elseif(present(ivin))then !if(expan)then
		j_o(ivout)%i2(1:list00)=j_o(ivin)%i2(1:list00)
	else
		j_o(ivout)%i2(1:ntot)=j_0
	endif !if(expan)  16878
	!		j_o(ivout)%i2=0
	if(iplist.eq.j_iplist)then
		allocate(j_o(ivout)%d(1:ntot))
		j_o(ivout)%d=j_0
	endif !if(iplist.eq.j_iplist)  16894
	j_otype(ivout)=iplist
	j_deflistobject=ivout
	return
99		write(6,*)'cannot mix lists and with single objects in defing list objects'
	j_err=.true.
 
end function !subroutine j_deflistobject(iv,name,ivout,list0,list,listold,nres,expand,ivin)
 
function j_putlistobject(ivlist,single,list0,list,ivin,ignored,append)		 ! put i into %%list object, size increased if needed
	integer,intent(in)::ivlist
	integer,optional,intent(in)::single
	integer,optional,intent(in)::list0
	integer,dimension(:),optional,target,intent(in)::list
	integer,optional,intent(in)::ivin !list object
	logical,optional,intent(in)::ignored
	logical,optional,intent(in)::append
 
	integer,dimension(:),allocatable::hh,netput
	integer,dimension(:),pointer::listin
	logical ::islist,ignore,isprepend,isappend
	! if(ivlist.le.0)then
	! write(6,*)'*j* illegal putlist2';j_err=.true. ;return
	! else if(j_otype(ivlist).ne.j_iplist)then !if(ivlist.le.0)then
	! call j_printname('**not a legal list: ',ivlist,' ');j_err=.true. ;return
	! end if !if(ivlist.le.0)then
	isappend=.false.
	isprepend=.false.
	if(present(append))then
		isappend=append
		if(.not.isappend)isprepend=.true.
 
	endif !if(present(append))  16925
	ignore=.false.
	if(present(ignored))ignore=ignored
	nadd=0
	j_putlistobject=0
	if(ivlist.le.0.or.ivlist.gt.j_mxv)then
		write(6,*)'*j* j_putlistobject, illegal ivlist ',ivlist
		j_err=.true.;return
	elseif(j_otype(ivlist).ne.j_iplist.and.j_otype(ivlist).ne.j_ipilist)then
		call j_printname('j_putlistobject  ',ivlist,' is not LIST or ILIST')
		j_err=.true.;return
	endif !if(ivlist.le.0.or.ivlist.gt.j_mxv)  16934
 
 
	islist=.false.
	if(present(list0))then
		list00=list0
		listin=>list
		islist=.true.
	elseif(present(ivin))then !if(present(list0))then
		list00=j_o(ivin)%i(1)
		listin=>j_o(ivin)%i2(1:list00)
		islist=.true.
	endif !if(present(list0))  16944
	if(present(single))then
		if(.not.isappend)then
			j_putlistobject=j_inlistobject(single,ivlist)
			if(.not.islist.and.j_putlistobject.gt.0)return
		endif !if(.not.isappend)  16954
		if(ignore)then
			if(.not.j_isdollar(single))nadd=nadd+1
		else !if(ignore)then
			nadd=nadd+1
 
		endif !if(ignore)  16958
	endif !if(present(single))  16953
	if(isprepend)nadd=1
	if(islist)then
		allocate(netput(1:nadd+list00))
		if(nadd.gt.0)netput(1)=single
		k=0
		do i=1,list00
			if(.not.present(append))k=j_inlistobject(listin(i),ivlist)
			if(k.eq.0)then
				if(ignore)then
					if(.not.j_isdollar(listin(i)))then
						nadd=nadd+1
						netput(nadd)=listin(i)
					endif !if(.not.j_isdollar(listin(i)))  16974
				else !if(ignore)then
					nadd=nadd+1
					netput(nadd)=listin(i)
				endif !if(ignore)  16973
			endif !if(k.eq.0)  16972
		enddo !i=1,list00  16970
	endif !if(islist)  16966
	nold=j_o(ivlist)%i(1)
	if(nadd+nold.gt.j_o(ivlist)%i(4))then
		nuus=max(2*nold,nold+2*nadd)
		allocate(hh(1:nuus))
		hh(1:nold)=j_o(ivlist)%i2(1:nold)
		hh(nold+1:nuus)=0
		call move_alloc(from=hh,to=j_o(ivlist)%i2)
		! deallocate(j_o(ivlist)%i2)
 
		! allocate(j_o(ivlist)%i2(1:nuus))
		! j_o(ivlist)%i2(1:nold)=hh
		! deallocate(hh)
		j_o(ivlist)%i(4)=nuus
	endif !if(nadd+nold.gt.j_o(ivlist)%i(4))  16986
	if(islist)then
		j_o(ivlist)%i2(nold+1:nold+nadd)=netput(1:nadd)
		deallocate(netput)
	elseif(isprepend)then
		j_o(ivlist)%i2(2:nold+1)=j_o(ivlist)%i2(1:nold)
		j_o(ivlist)%i2(1)=single
	else !if(islist)then
		j_o(ivlist)%i2(nold+1)=single
		j_putlistobject=nold+1
	endif !if(islist)  16999
	j_o(ivlist)%i(1)=nold+nadd
	j_o(ivlist)%i(3)=j_o(ivlist)%i(1)
 
 
 
	return
end function j_putlistobject !function j_putlistobject(ivlist,single,list0,list,ivin,ignored)
 
subroutine j_printoptions()
	write(6,*)'   #     DEF in  TRANS,     OPT ,   FUNC , defined in line ,  in location'
	loop:		do ii=1,j_nopt
		iobi=j_optioniob(ii)
		ioo=j_optionmoptio(2,ii)
		call j_getname(iobi)
		if(j_o(iobi)%i(ioo).gt.j_nfunctions)then
			write(6,*)ii,j_oname(1:j_loname),'  ',j_options(j_optionmoptio(1,ii)),&
				' option function ',j_o(iobi)%i(ioo), ' is not legal'
			j_err=.true.
		endif !if(j_o(iobi)%i(ioo).gt.j_nfunctions)  17023
		if(j_o(iobi)%i(ioo).gt.j_nfunctions)then
			write(6,*)ii,j_oname(1:j_loname),'  ',j_options(j_optionmoptio(1,ii)),&
				' option function ',j_o(iobi)%i(ioo), ' is not legal'
			j_err=.true.
		else
			write(6,*)ii,j_oname(1:j_loname),'  ',j_options(j_optionmoptio(1,ii)),&
				j_functions(j_o(iobi)%i(ioo)),ioo  !,' link ',j_optionlink(ii)
		endif !if(j_o(iobi)%i(ioo).gt.j_nfunctions)  17028
 
		ivsource=j_o(iobi)%i2(11)
 
		!	write(6,*)'io ',io
		if(ivsource.ne.0)then
 
 
 
			do i=1,j_o(ivsource)%i(0)
				!	write(6,*)'i ',i
				if(j_o(ivsource)%i2(i).ge.ioo-2)then
					call j_getline(ivsource,i-1,j_filename,le)
					write(6,*)'line ',i-1,': ',j_filename(1:le) !,j_o(ivsource)%i2(i),io-1
					!	call j_printtext(ivsource,i-1)
					cycle loop
				endif !if(j_o(ivsource)%i2(i).ge.ioo-2)  17046
			enddo !i=1,j_o(ivsource)%i(0)  17044
			!write(6,*)'* ask J. Lappi where is the error line'
 
		endif !if(ivsource.ne.0)  17040
 
	enddo loop !p:		do ii=1,j_nopt  17019
 
 
end subroutine !subroutine j_printoptions()
 
subroutine j_printlist(nu,iv,head)
	integer,intent(in)::nu,iv
	logical,optional::head
	logical ::head_
	call j_getname(iv)
	if(j_otype(iv).ne.j_iplist)then
		write(6,*)j_oname(1:j_loname), ' is ,not list'
		j_err=.true.
		return
 
	endif !if(j_otype(iv).ne.j_iplist)  17067
 
	head_=.true.
	if(present(head))head_=head
	if(head_)then
		write(nu,*)' '
		write(nu,*)j_oname(1:j_loname),' is list with ', j_o(iv)%i(1), ' elements:' ! ,size(o(iv)%i)
	endif !if(head_)  17076
	nel= j_o(iv)%i(1)
	lenc = 0
	lenct=len(j_cline)
	do i_=1,nel
		iel = j_o(iv)%i2(i_)
		call j_getname(iel)
		if(lenc+j_loname+1.gt.lenct)then
			write(nu,'(1x,a)')j_cline(1:lenc)
			lenc=0
		endif !if(lenc+j_loname+1.gt.lenct)  17086
		j_cline(lenc+1:lenc+j_loname+1) =j_oname(1:j_loname)//' '
		lenc=lenc+j_loname+1
	enddo !i_=1,nel  17083
	write(nu,'(1x,a)')j_cline(1:lenc)
end subroutine !subroutine j_printlist(nu,iv,head)
 
subroutine j_printlist0(nu,list0,list)
	integer,intent(in)::nu,list0
	integer,intent(in),dimension(list0)::list
	character*6 chr
	logical temp
	nel= list0
	j_lencline = 0
	do i_=1,nel
		iel = list(i_)
		temp=.false.
		if(iel.gt.j_mxnamedv.and.iel.le.j_nv)then !if(iv.le.j_namedv)then
			write(chr,'(i6)')iel
			do j=1,4
				if(chr(j:j).ne.' ')exit
			enddo !j=1,4  17108
			ipit=11-j
			temp=.true.
			!		write(6,*)'iv',iv
			!	j_oname='TEMP'//chr(j:6)
			!	j_loname=len_trim(j_oname)
		else
			ipit=j_o(j_ivnames)%i(iel+1)-j_o(j_ivnames)%i(iel)
		endif !if(iel.gt.j_mxnamedv.and.iel.le.j_nv)  17106
		if (j_lencline+ipit>80) then
			write(nu,'(1x,a)')j_cline(1:j_lencline)
			j_lencline=0
		endif !if (j_lencline+ipit>80)  17119
		if(temp)then
			j_cline(j_lencline+1:j_lencline+ipit)='TEMP'//chr(j:6)
		else
			call j_getline2(j_ivnames,iel,j_cline(j_lencline+1:j_lencline+ipit),le)
			if(j_err)return
		endif !if(temp)  17123
		j_lencline = j_lencline+ipit
		if (j_lencline<80) then
			j_lencline = j_lencline+1
			j_cline(j_lencline:j_lencline)=' '
		endif !if (j_lencline<80)  17130
 
	enddo !i_=1,nel  17103
	write(nu,'(1x,a)')j_cline(1:j_lencline)
end subroutine !subroutine j_printlist0(nu,list0,list)
 
subroutine j_parent(inp,le)
	character*(*),intent(inout)::inp
	integer,intent(inout)::le
	logical ::haka, hipsu
	integer nleft,nright,nleft2,nright2,le2
	!	le=len(inp)
	hipsu=.false.
	haka=.false.
	nleft=0
	nright=0
	neq=0
	nleft2=0
	nright2=0
 
 
 
	do i=1,le
		i1=min(i+1,le)
		i2=min(i+1,le)
		if(inp(i:i).eq."'")then
			hipsu=.not.hipsu
			if(hipsu)li=i
			cycle
		else
			if(hipsu)cycle
		endif !if(inp(i:i).eq."'")  17158
		select case(inp(i:i))
		case ("'") !select case(inp(i:i))
		hipsu=.not.hipsu
		case("[") !select case(inp(i:i))
		if(.not.hipsu)haka=.true.
		case("]") !select case(inp(i:i))
		if(.not.hipsu)haka=.false.
		case('(') !select case(inp(i:i))
		if(.not.(haka.or.hipsu))then
			nleft=nleft+1
			if(inp(i+1:i+1).eq.',')then
				write(6,*)'left parenthesis cannot be followed with comma'
				j_err=.true. ;return
			endif !if(inp(i+1:i+1).eq.',')  17175
		endif !if(.not.(haka.or.hipsu))  17173
 
		case(')') !select case(inp(i:i))
		if(.not.(haka.or.hipsu))then
			nright=nright+1
			if(j_isletter(inp(i1:i1)).and.inp(max(le-4,1):le).ne.')then'.and.inp(1:3).ne.'if('.and.&
					index(inp(1:le),':if(').le.0)then
 
				write(6,*)'right parenthesis at ',i,'cannot be followed with letter, input:'
				write(6,*)inp(1:le)
				j_err=.true.;return
			endif !if(j_isletter(inp(i1:i1)).and.inp(max(le-4,1):le).ne.')the  17184
			if(inp(i1:i1).ge.'0'.and.inp(i1:i1).le.'9'.or.(inp(i1:i1).eq.'.'.and.inp(i2:i2).ge.'0' &
					.and.inp(i2:i2).le.'9'))then
 
				write(6,*)'right parenthesis at ',i,'cannot be followed with number, input:'
				write(6,*)inp(1:le)
				j_err=.true.;return
 
 
			endif !if(inp(i1:i1).ge.'0'.and.inp(i1:i1).le.'9'.or.(inp(i1:i1).  17191
			if(nright.gt.nleft)then
				write(6,*)nleft,' ( ',nright,' ) at:'! more ) than ( ,nleft=',nleft,' nright=',nright
				write(6,*)inp(1:i)
				write(6,*)' '
				!write(6,*)'at character ',i ,' more ) than ( ,nleft=',nleft,' nright=',nright
				j_err=.true.;return
			endif !if(nright.gt.nleft)  17200
		endif !if(.not.(haka.or.hipsu))  17182
		case('=') !select case(inp(i:i))
		if(.not.(haka.or.hipsu))then
			neq=neq+1
			if(neq.gt.1)then
				write(6,*)'there can be only one ='
				j_err=.true.;return
 
			endif !if(neq.gt.1)  17211
			if(nleft.ne.nright)then
				write(6,*)nleft,' ( ',nright,' ) nonbalanced parenthesis at:'
				write(6,*)inp(1:i)
				write(6,*)' '
				!	write(6,*)'at  = more ( than ), nleft=',nleft,' nright=',nright
				j_err=.true. ;return
			endif !if(nleft.ne.nright)  17216
		endif !if(.not.(haka.or.hipsu))  17209
 
		case('{')  ! ½ default !select case(inp(i:i))
		if(nleft2.eq.0)then
			if(i.eq.1)then
				j_tempchar(1:6)='ilist('
				le2=6
			else
				j_tempchar(1:i+6)=inp(1:i-1)//'ilist('
				le2=i+5
			endif !if(i.eq.1)  17227
 
 
		else
			lis=i-iv+5
			j_tempchar(le2+1:le2+lis)=inp(iv+1:i-1)//'ilist('
			le2=le2+lis
 
		endif !if(nleft2.eq.0)  17226
		iv=i
		!		 write(6,*)'he1:',i,iv,j_tempchar(1:le2)
		nleft2=nleft2+1
 
 
		case('}')
		nright2=nright2+1
		if(nright2.gt.nleft2)then
			write(6,*)nleft2,' { ',nright2, ' }, not balanced at'
			write(6,*)inp(1:i)
			write(6,*)' '
			j_err=.true.
		endif !if(nright2.gt.nleft2)  17249
		lis=i-iv
		j_tempchar(le2+1:le2+lis)=inp(iv+1:i-1)//')'
		!	 write(6,*)'hui:',le2,i,iv,lis,inp(iv+1:i-1)//')'
		le2=le2+lis
		iv=i
		!	  write(6,*)'he2:',i,iv,j_tempchar(1:le2)
		!	if(hipsu.and.inp(i:i).eq.'~')inp(i:i)="'"
		end select !select case(inp(i:i))
	enddo !i=1,le  17155
	if(hipsu)then
		write(6,*)'apostrophe started at ',li, ' never ends '
		j_err=.true.;return
	endif !if(hipsu)  17264
	if(nleft.ne.nright)then
		write(6,*)'*unbalanced parenthesis ',nleft,' ( ',nright, ' )  at location ', i,':'
		!	write(6,*)inp(1:i)
		!	write(6,*)'total line with length ',le,':'
		!write(6,*)inp(1:le)
		!	write(6,*)' '
		j_err=.true. ;return
	endif !if(nleft.ne.nright)  17268
	if(nleft2.ne.0)then
		if(nleft2.ne.nright2)then
			write(6,*)'*unbalanced parenthesis ',nleft2,' { ',nright2, ' }'
			write(6,*)inp(1:i)
			write(6,*)' '
			j_err=.true.
		else
			if(iv.lt.le)then
				lis=le-iv
				j_tempchar(le2+1:le2+lis)=inp(iv+1:le)
				!		 write(6,*)'hef:',j_tempchar(1:le2)
				le2=le2+lis
			endif !if(iv.lt.le)  17283
			le=le2
			inp(1:le)=j_tempchar(1:le)
			!		write(6,*)'pate:',inp(1:le)
		endif !if(nleft2.ne.nright2)  17277
	endif !if(nleft2.ne.0)  17276
 
 
	return
 
 
end subroutine !subroutine j_parent(inp)
 
logical function j_writevar(nu,narg,arg,ivform)
	integer,intent(in)::nu,narg,ivform
	integer,intent(in),dimension(narg)::arg
	j_writevar=.false.
	nar=0
	do iar=1,narg
		if(j_otype(arg(iar)).ne.j_ipchar.and.j_otype(arg(iar)).ne.j_ipreal)then
 
			if(j_otype(arg(iar)).ne.j_ipmatrix)then
				call j_getname(arg(iar))
				write(6,*)j_oname(1:j_loname),' is not REAL, CHAR , or MATRIX'
				j_err=.true.
				j_writevar=.true.
 
			endif !if(j_otype(arg(iar)).ne.j_ipmatrix)  17309
			return
 
		endif !if(j_otype(arg(iar)).ne.j_ipchar.and.j_otype(arg(iar)).ne.  17307
		if(j_otype(arg(iar)).eq.j_ipreal)nar=nar+1
	enddo !iar=1,narg  17306
	if(j_err)return
	j_writevar=.true.
	if(nar.eq.narg)then
		if(ivform.eq.j_ivdollar)then
			write(nu,*)real(j_v(arg))
		else
			write(nu,*)j_v(arg)
		endif !if(ivform.eq.j_ivdollar)  17324
		return
	endif !if(nar.eq.narg)  17323
 
 
	ial=1
 
	do iar=1,narg
		if(j_otype(arg(iar)).eq.j_ipchar)then
			call j_getchar(arg(iar),j_tempchar2(ial:),le)
			ial=ial+le
		else !if(j_otype(arg(iar)).eq.j_ipchar)then
			j_tempchar2(ial:ial+9)=j_chr10(j_v(arg(iar)))
			ial=ial+10
		endif !if(j_otype(arg(iar)).eq.j_ipchar)  17336
	enddo !iar=1,narg  17335
	write(nu,'(a)')j_tempchar2(1:ial-1)
	return
 
end function !logical function j_writevar(nu,narg,arg)
 
logical function j_printvar(nu,narg,arg)
	integer,intent(in)::nu,narg
	integer,intent(in),dimension(narg)::arg
	j_printvar=.false.
 
	do iar=1,narg
		if(arg(iar).gt.j_mxv)then
			write(6,*)'<6w>',arg(1:narg)
			j_err=.true.
			return
 
		endif !if(arg(iar).gt.j_mxv)  17355
		if(j_otype(arg(iar)).ne.j_ipchar.and.j_otype(arg(iar)).ne.j_ipreal)return
	enddo !iar=1,narg  17354
	j_printvar=.true.
 
	j_tempchar2=' '
	!write(6,*)'<55355 narg,arg ',narg,arg,'type:',j_otype(arg)
	ial=1
	ic=-1
	do iar=1,narg
 
 
		if(arg(iar).gt.j_mxv)then
			write(6,*)'uliu',arg(1:narg)
			j_err=.true.;return
 
		endif !if(arg(iar).gt.j_mxv)  17372
		if(j_otype(arg(iar)).eq.j_ipchar)then
			call j_getchar(arg(iar),j_tempchar2(ial:),le)
			ial=ial+le
			ic=iar
		else !if(j_otype(arg(iar)).eq.j_ipchar)then
			if(iar-1.ne.ic)then
				if(arg(iar).le.j_namedv)then
					j_tempchar2(ial:ial+23)=j_vname(arg(iar))
					le=len_trim(j_tempchar2(ial:ial+23))
					ial=ial+le+1
					j_tempchar2(ial-1:ial-1)='='
 
				else !if(arg(iar).le.j_namedv)then
 
					j_tempchar2(ial:ial)='='
					ial=ial+1
				endif !if(arg(iar).le.j_namedv)  17383
			endif !if(iar-1.ne.ic)  17382
			j_tempchar2(ial:ial+9)=j_chr10(j_v(arg(iar)))
			ial=ial+12
		endif !if(j_otype(arg(iar)).eq.j_ipchar)  17377
	enddo !iar=1,narg  17369
	write(nu,'(a)')j_tempchar2(1:ial)
	write(nu,*)' '
	return
 
end function !logical function j_printvar(nu,narg,arg)
 
subroutine j_readtext(iv)
	!for character variable text is read into the char constant to which charvar is pointing
	integer,intent(in)::iv
 
	integer(4), dimension(13)::buff
	integer(4)::status
	integer,dimension(:),allocatable::lenb
	!	logical ::p
	!	p=.false.
	if(j_otype(iv).ne.j_ipchar)then
		write(6,*)'*j* object ',ivin,' is not CHAR'
		j_err=.true.;return
	endif !if(j_otype(iv).ne.j_ipchar)  17414
 
 
	!buff(10)	Last modification time   %i(5) number of lines i(6) last modified
	!buff(8)	File size (bytes)
	! if(j_o(iv)%i(3).ne.0)then
	! j_o(j_o(iv)%i(3))%i(4)=nu
	! else !if(j_o(iv)%i(3).ne.0)then
	! j_o(iv)%i(4)=nu
	!endif !if(j_o(iv)%i(3).ne.0)then
 
	call j_getchar(iv,j_filename,le)
	!	write(6,*)'<fil>',iv,j_filename(1:le)
	inquire(file=j_filename(1:le),exist=j_yes,opened=j_yes2,number=nu)
	if(.not.j_yes)then
		write(6,*)'file ',j_filename(1:le),' does not exist'
		j_err=.true.;return
	endif !if(.not.j_yes)  17431
 
	if(j_yes2)then
		write(6,*)'file ',j_filename(1:le),' was open, it is closed first'
		call j_closeunit(nu)
	endif !if(j_yes2)  17436
 
	CALL STAT(j_filename(1:le),BUFF,STATUS)
	if(STATUS.ne.0)then
		write(6,*)'cannot open file ',j_filename(1:le),' STATUS=',STATUS
		j_err=.true.;return
	endif !if(STATUS.ne.0)  17442
	!	if(p)write(6,*)'perk',buff(10),j_o(iv)%i(7),iv,allocated(j_o(iv)%txt),&
	!	allocated(j_o(iv)%i),allocated(j_o(iv)%i2)
	!	write(6,*)'buf10',buff(10),j_o(iv)%i(7),' return ',buff(10).eq.j_o(iv)%i(7)
	if(buff(10).eq.j_o(iv)%i(7))return
 
 
	open(j_incnu,file=j_filename(1:le),action='read',err=90)
 
	nlin=buff(8)/2  !min line length average 2
	if(allocated(j_temptxt))then
		if(size(j_temptxt).lt.nlin)then
			deallocate(j_temptxt)
			allocate(j_temptxt(1:nlin))
		endif !if(size(j_temptxt).lt.nlin)  17456
	else !if(allocated(j_temptxt))then
		allocate(j_temptxt(1:nlin))
	endif !if(allocated(j_temptxt))  17455
	if(allocated(lenb))then
		if(size(lenb).lt.nlin)then
			deallocate(lenb)
			allocate(lenb(1:nlin))
		endif !if(size(lenb).lt.nlin)  17464
	else !if(allocated(lenb))then
		allocate(lenb(1:nlin))
	endif !if(allocated(lenb))  17463
	ntrunc=0
	ntrunc2=0
	il=0
	!if(p)WRITE(6,*)'nlin,leb',nlin,leb
	do while(.true.)
		read(j_incnu,'(a)',end=99,err=90)j_tempchar
		leb=j_lentrim(j_tempchar)
		!		write(6,*)'il ',il,j_tempchar(1:leb)
		if(leb.gt.160)then
			!if(ntrunc.eq.0)then
			!	write(6,*)'file ',j_filename(1:le),' line ',il+1, 'truncated to length 132'
 
			!endif !if(ntrunc.eq.0)then
 
			ntrunc=ntrunc+1
			ico=j_nextlim(j_tempchar(1:leb),1,leb,'!')
			if(ico.gt.160)then
				write(6,*)'line ',il+1, ' is ' , leb, ' characters long'
				write(6,*) j_tempchar(1:leb)
				ntrunc2=ntrunc2+1
			endif !if(ico.gt.160)  17487
		endif !if(leb.gt.160)  17479
 
		il=il+1
		j_temptxt(il)=j_tempchar(1:max(leb,1))
		lenb(il)=min(leb,160)
		!	if(p)WRITE(6,*)'leb,il',leb,il
	enddo !while(.true.)  17475
99		close(j_incnu)
!	write(6,*)'ilines ',il,j_incnu
	if(ntrunc2.gt.0)then
		write(6,*)ntrunc ,' lines too long from which ',ntrunc2,' lines the extra part was not comment'
		write(6,*)'the maximum line length is 160'
		j_err=.true.
		return
 
	endif !if(ntrunc2.gt.0)  17501
	if(il.le.0)then
		write(6,*)'file ',j_filename(1:le),' was empty'
		j_err=.true.
		return
	endif !if(il.le.0)  17508
	if(allocated(j_o(iv)%txt))deallocate(j_o(iv)%txt)
	if(allocated(j_o(iv)%i2))deallocate(j_o(iv)%i2)
	allocate(j_o(iv)%txt(1:il+1))   !one line reserve
	allocate(j_o(iv)%i2(1:il+1))  !one line reserve
	j_o(iv)%txt(1:il)=j_temptxt(1:il)
	j_o(iv)%i2(1:il)=lenb(1:il)
	deallocate(j_temptxt,lenb)
 
	j_o(iv)%i(5)=il !number of lines
	j_o(iv)%i(7)=buff(10) !size?
	!	j_inciv(j_ninc)=iv
 
	!	write(6,*)'<777 ',il,iv,nu
	return
 
 
 
90	write(6,*)'error in reading '
	j_err=.true.;return
 
 
end subroutine !subroutine j_readtext(iv)
 
subroutine j_getin(iob,io,nu,ivform)  !get in-> file
	integer,intent(in)::iob,io
	integer,intent(out)::nu,ivform
	logical ::p=.false.
	!	p=j_v(j_ivdebug).ne.j_0
	j_incin=.false.
	li=j_linkoption(iob,io,j_min)
	!	write(6,*)'<777)',li
	if(li.lt.0)then
		nu=0;return
	endif !if(li.lt.0)  17544
 
	lif=j_linkoption(iob,io,j_mform)
	ivform=j_ivdollar
	if(lif.gt.0)then
		if(j_o(iob)%i(lif).ne.1)then
			write(6,*)'form needs one argument'
			j_err=.true.;return
		endif !if(j_o(iob)%i(lif).ne.1)  17551
		ivform=j_o(iob)%i(lif+1)
		!	write(6,*)'<4545subin ifortm',ivform
	endif !if(lif.gt.0)  17550
 
 
	if(li.eq.0)then !if(j_ninc.eq.1)then
		if(j_ninc.eq.1)then
			nu=5
		else
			j_incin=.true.
			nu=j_inciv(j_ninc)
		endif !if(j_ninc.eq.1)  17561
	else !if(j_ninc.eq.1)then
		if(j_o(iob)%i(li).gt.1)then
			write(6,*)'only on in-> file allowed, use newdata to merge datas'
			j_err=.true.
			return
		endif !if(j_o(iob)%i(li).gt.1)  17568
		ifi=j_o(iob)%i(li+1)
		!	write(6,*)'<88bef getfile,ifi,ivform',ifi,ivform
		if(ivform.ne.j_ivdollar)then
			!	write(6,*)'<53535'
			call j_getfile(nu,'r',ivfile=ifi,ivform=ivform)
			!	write(6,*)'<464664 '
		else !if(ivform.ne.j_ivdollar)then
			call j_getfile(nu,'r',ivfile=ifi)
		endif !if(ivform.ne.j_ivdollar)  17575
	endif !if(li.eq.0)  17560
	! j_linkoption(iob,io,j_min)=0
	! j_linkoption(iob,io,j_mform)=0
	return
end subroutine !subroutine j_getin(iob,io,nu,ivform)
 
subroutine j_getsubin(iob,io,nu,ivform)
	integer,intent(in)::iob,io
	integer,intent(out)::nu,ivform
 
	j_incin=.false.
 
	li=j_linkoption(iob,io,j_msubin)
	if(li.lt.0)then
		write(6,*)'subin-> is misssing'
		j_err=.true.;return
	endif !if(li.lt.0)  17595
 
	lif=j_linkoption(iob,io,j_msubform)
	!	write(6,*)'<646subinlinkform',lif
	ivform=j_ivdollar
	if(lif.gt.0)then
		if(j_o(iob)%i(lif).ne.1)then
			write(6,*)'subform needs one argument'
			j_err=.true.;return
		endif !if(j_o(iob)%i(lif).ne.1)  17604
		ivform=j_o(iob)%i(lif+1)
	endif !if(lif.gt.0)  17603
 
	if(j_ninc.eq.1)then
		nu=5
 
	elseif(j_o(iob)%i(li).eq.0)then !if(j_ninc.eq.1)then
		j_incin=.true.
		nu=j_inciv(j_ninc)
	else !if(j_ninc.eq.1)then
		ifi=j_o(iob)%i(li+1)
		if(ivform.ne.j_ivdollar)then
			call j_getfile(nu,'r',ivfile=ifi,ivform=ivform)
		else !if(ivform.ne.j_ivdollar)then
			call j_getfile(nu,'r',ivfile=ifi)
		endif !if(ivform.ne.j_ivdollar)  17619
	endif !if(j_ninc.eq.1)  17611
	!j_linkoption(iob,io,j_msubin)=0
	!j_linkoption(iob,io,j_msubform)=0
	return
end subroutine !subroutine j_getsubin(iob,nu,ivform)
 
integer function j_filesize(ifile,filename,time)
	! buff(1)	Device ID
	! buff(2)	Inode number
	! buff(3)	File mode
	! buff(4)	Number of links
	! buff(5)	Owner's uid
	! buff(6)	Owner's gid
	! buff(7)	ID of device containing directory entry for file (0 if not available)
	! buff(8)	File size (bytes)
	! buff(9)	Last access time
	! buff(10)	Last modification time
	! buff(11)	Last file status change time
	! buff(12)	Preferred I/O block size (-1 if not available)
	!buff(13)	Number of blocks allocated (-1 if not available)uff(13)	Number of blocks allocated (-1 if not available)
	integer,optional,intent(in)::ifile
	character*(*),optional:: filename
	integer,intent(out),optional::time
	integer(4), dimension(13)::buff
	integer(4)::status
 
	if(present(ifile))then
		call j_getchar(ifile,j_filename,le)
		CALL STAT(j_filename(1:le),BUFF,STATUS)
	elseif(present(filename))then !if(present(ifile))then
		CALL STAT(filename,BUFF,STATUS)
	else !if(present(ifile))then
		write(6,*)'*j* j_filesize, either filename or ifile must be present'
		j_err=.true.;return
	endif !if(present(ifile))  17650
	if(STATUS.ne.0)then
		j_filesize=0
		if(present(time))time=0
	else !if(STATUS.ne.0)then
		j_filesize=buff(8)
		if(present(time))time=buff(10)
	endif !if(STATUS.ne.0)  17659
 
 
	return
end function !integer function j_filesize(ifile,filename,time)
 
subroutine j_cleanstart(text,le) ! remove blanks, tabs etc , le is the length of the cleaned text
	integer, intent(out):: le
	character (len=*),intent(inout):: text
	le=len(text)
	do j=1,le
		if(text(j:j).le.' ')cycle
		if(text(j:j).eq.';'.or.text(j:j).eq.'/')then
			if(j.gt.1)then
				text(1:le-j+1)=text(j:le)
				le=le-j+1
			endif !if(j.gt.1)  17678
			return
		else !if(text(j:j).eq.';'.or.text(j:j).eq.'/')then
			return
		end if !if(text(j:j).eq.';'.or.text(j:j).eq.'/')  17677
	enddo !j=1,le  17675
end subroutine !subroutine j_cleanstart(text,le)
 
subroutine j_range(xmin,xmax,xmin2,xmax2)
	real, intent(in)::xmin,xmax
	real, intent(out)::xmin2,xmax2
	real dl
	d1=log10(abs(xmin))
	d2=log10(abs(xmax))
	i1=d1
	i2=d2
	ii=max(i1,i2)
	ii1=ii-1
	coe1=xmin/10.**ii
	coe2=xmax/10.**ii
 
	icoe1=floor(coe1*10.)
	icoe2=ceiling(coe2*10.)
	xmax2=icoe2*10.**ii1
	xmin2=icoe1*10.**ii1
	!		write(6,*)d1,d2,i1,i2,ii,icoe1,icoe2,xmin2,xmax2
	return
end subroutine !subroutine j_range(xmin,xmax,xmin2,xmax2)
 
recursive subroutine j_showfig(ivfig,asis)
 
	!	logical::old=.false.
	integer,intent(in)::ivfig
	logical,intent(in),optional::asis
	logical ::p=.false.
	character*30 ch
	character*6::terminal
	character*12 ::window
	!	set terminal qt size 600.,1000. persist
	!	save old
	if(j_otype(j_ivterminal).eq.j_ipchar)then
		call j_getchar(j_ivterminal,terminal,let)
	else
		terminal='qt'
		let=2
	endif !if(j_otype(j_ivterminal).eq.j_ipchar)  17721
	if(j_otype(j_ivwindow).eq.j_ipchar)then
		call j_getchar(j_ivwindow,window,lew)
	else
		window='700,700'
		lew=7
	endif !if(j_otype(j_ivwindow).eq.j_ipchar)  17727
	!	write(6,*)'showfig,ivfigas, ',ivfig,asis
	!	j_otype(ivfig),
	if(j_otype(ivfig).eq.j_ipchar)then
		!	write(6,*)'shhshs'
		call j_getchar(ivfig,j_filename,le)
		!write(6,*)'shhshs',j_filename(1:le)
 
	else !if(j_otype(ivfig).eq.j_ipchar)then
		!write(6,*)'kdjdjd'
		call j_getfile(nu,rw='w',ivfile=ivfig,ext='.jfig',replace=.true.,ivout=ivfile)
 
		call j_getchar(ivfile,j_filename,le)
		!	write(6,*)'<99ivfig,append,j_otype(ivfig)',ivfig,append,j_otype(ivfig)
		!	if(append)write(nu,*)'set multiplot'
		!	p=j_v(j_ivdebug).gt.j_0
		write(nu,'(a)')'set nokey'
		write(nu,'(a)')'unset title'
		write(nu,'(a)')'unset label'
		write(nu,'(a)')'unset xlabel'
		write(nu,'(a)')'unset ylabel'
		write(nu,'(a)')'unset xrange'
		write(nu,'(a)')'unset yrange'
		! do ili=1,j_o(ivfig)%i(1)
		! write(28,*)j_o(ivfig)%txt(ili)(1:j_o(ivfig)%i2(ili))
		if(j_gpaxes.eq.0)then
			write(nu,'(a)')'unset border'
			write(nu,'(a)')'unset ytics'
			write(nu,'(a)')'unset xtics'
			write(nu,'(a)')'unset raxis'
			write(nu,'(a)')'unset rtics'
		endif !if(j_gpaxes.eq.0)  17757
		write(nu,*)'set terminal ',terminal(1:let),' size ',window(1:lew), ' enhanced'
		! enddo
		write(nu,*)'set encoding utf8'
		do ili=1,j_gpbas
			write(nu,'(a)')j_o(ivfig)%txt(ili)(1:j_o(ivfig)%i2(ili))
		enddo !ili=1,j_gpbas  17767
		xmin=j_o(ivfig)%d(1)
		if(xmin.gt.1.e20)xmin=j_o(ivfig)%d(1)
		nl=(j_o(ivfig)%i(3)+1)/2  !used plot lines
 
		!write plot linse
		!	write(6,*)'<77',nl,j_gpbasstartfig
		do ili=j_gpbas+1,j_gpbas+nl
			if(ili.lt.j_gpbas+nl)then
				write(nu,'(a)')j_o(ivfig)%txt(ili)(1:j_o(ivfig)%i2(ili))//'\'
			else !if(ili.lt.j_gpbas+nl)then
				if(j_o(ivfig)%i2(ili).gt.0)	write(nu,'(a)')j_o(ivfig)%txt(ili)(1:j_o(ivfig)%i2(ili))
			endif !if(ili.lt.j_gpbas+nl)  17777
 
		enddo !ili=j_gpbas+1,j_gpbas+nl  17776
		nres=(j_o(ivfig)%i(4)+1)/2  !reserved plot lines
		nc=0
		nc2=0
		do ili=j_gpbas+nres+1,j_o(ivfig)%i(1)
			nc2=nc2+1
			if(j_o(ivfig)%txt(ili)(1:10).ne.'set label ')then
				write(nu,'(a)')j_o(ivfig)%txt(ili)(1:j_o(ivfig)%i2(ili))
				nc=nc+1
			endif !if(j_o(ivfig)%txt(ili)(1:10).ne.'set label ')  17789
		enddo !ili=j_gpbas+nres+1,j_o(ivfig)%i(1)  17787
 
		if(nc.ne.nc2)then
			do ili=j_gpbas+nres+1,j_o(ivfig)%i(1)
				if(j_o(ivfig)%txt(ili)(1:10).eq.'set label ')&
					write(nu,'(a)')j_o(ivfig)%txt(ili)(1:j_o(ivfig)%i2(ili))
			enddo !ili=j_gpbas+nres+1,j_o(ivfig)%i(1)  17796
 
		endif !if(nc.ne.nc2)  17795
		write(nu,'(a)')'replot'
 
		!	write(nu,'(a)')'e'
 
		!if(append)write(nu,*)'unset multiplot'
		call j_closeunit(nu)
 
	endif !if(j_otype(ivfig).eq.j_ipchar)  17735
	!	if(old) call execute_command_line('gnuplot exit')
	!	write(6,*)'<djjd',j_filename(1:le)
	call execute_command_line('gnuplot --persist '//j_filename(1:le), wait=.false.)
	!	 call execute_command_line('gnuplot --persist gnuclear.txt', wait=.false.)
	!	 old=.true.
	!	 	 call execute_command_line('gnuplot -- '//j_filename(1:le), wait=.false.)
	if(j_gpcontinue.or.j_ispause)return
	!	call j_command('sit()')
	!	write(6,*)'pausetas,',j_cline,' recurs ',j_recursion
	if(j_ninc.gt.1)call j_pause('<fig>',do=.true.)
	!	write(6,*)'paunytsshowfiglopus,',j_err,' recrus ',j_recursion, j_cline
	return
end subroutine !recursive subroutine j_showfig(ivfig,asis)
function j_font(label,le)
	character*(*),intent(in)::label
	integer,intent(in)::le
	if(label(le:le).eq.'[')then
		do i=le-1,1,-1
			if(label(i:i).eq.']')then
 
				j_font=i
				return
			endif !if(label(i:i).eq.']')  17828
 
		enddo !i=le-1,1,-1  17827
		write(6,*)'label ',label(1:le), ' does not have matching ]'
		j_err=.true.
	endif !if(label(le:le).eq.'[')  17826
	j_font=0
	return
 
end function
 
subroutine j_putfig0(line,text)
	integer,intent(in)::line
	character*(*),intent(in)::text
	character*(j_txtlen),allocatable,dimension(:)::temp
	le=len_trim(text)
	j_cline=text
	!	write(6,*)'<77',le,j_cline(1:le)
	call j_toutf8(j_cline,le)
	!write(6,*)'<88',le,j_cline(1:le)
	!call j_putfig0(1,'set title "'//j_buffer(1:j_lentrim(j_buffer))//'"')
 
	j_o(j_gpiout)%txt(line)=j_cline(1:le) !text(1:le)
 
	j_o(j_gpiout)%i2(line)=le
	j_o(j_gpiout)%i(1)=max(line,j_o(j_gpiout)%i(1))
	return
end subroutine !subroutine j_putfig0(line,text)
 
subroutine j_putfig(text)
	character*(*),intent(in)::text
	character*(j_txtlen),allocatable,dimension(:)::temp
	le=len_trim(text)
	line=j_o(j_gpiout)%i(1)+1
	isize=j_o(j_gpiout)%i(2)
	if(line.gt.isize)call j_inctxt(j_gpiout,0)
 
	j_o(j_gpiout)%txt(line)=text(1:le)
	j_o(j_gpiout)%i(1)=line
	j_o(j_gpiout)%i2(line)=le
	return
end subroutine !subroutine j_putfig(text)
 
 
 
subroutine j_gpplot(text,add)
	character*(*),intent(in)::text
	logical,intent(in),optional::add
	character*4::pt !pointtype
	logical ::add_
	logical ::p=.false.
	!	p=j_v(j_ivdebug).gt.j_0
	add_=.false.
	if(present(add))add_=add
	le=len_trim(text)
	if(.not.add_)j_o(j_gpiout)%i(3)=j_o(j_gpiout)%i(3)+1
	iplot=j_o(j_gpiout)%i(3)
	nl=j_gpbas+(iplot+1)/2  !current plot line
 
 
	if(iplot.gt.j_o(j_gpiout)%i(4))then
		nlres=(j_o(j_gpiout)%i(4)+1)/2  !reserved lines
		!	write(6,*)'<843848incr'
		if(nlres+j_o(j_gpiout)%i(1).gt.j_o(j_gpiout)%i(2))call j_inctxt(j_gpiout,0)
		do i=j_o(j_gpiout)%i(1),j_gpbas+nlres+1,-1
			j_o(j_gpiout)%txt(i+nlres)=j_o(j_gpiout)%txt(i)
			j_o(j_gpiout)%i2(i+nlres)=j_o(j_gpiout)%i2(i)
		enddo !i=j_o(j_gpiout)%i(1),j_gpbas+nlres+1,-1  17896
		j_o(j_gpiout)%i2(j_gpbas+nlres+1:j_gpbas+2*nlres)=0
		j_o(j_gpiout)%i(4)=2*j_o(j_gpiout)%i(4)
	endif !if(iplot.gt.j_o(j_gpiout)%i(4))  17892
	ipt=index(text,' pt ')
	i2=j_o(j_gpiout)%i2(nl)
	j_o(j_gpiout)%txt(nl)(i2+1:i2+le)=text(1:le)
	j_o(j_gpiout)%i2(nl)=i2+le
	if(ipt.gt.0)then
		if(j_otype(j_gpmark).eq.j_ipchar)then
			call j_getchar(j_gpmark,j_tempchar2(1:4),lem)
			j_o(j_gpiout)%txt(nl)(i2+ipt+3:i2+ipt+5)='"'//j_tempchar2(1:1)//'"'
		else !if(j_otype(j_gpmark).eq.j_ipchar)then
			imark=j_v(j_gpmark)
			!	write(6,*)'imark',imark
			if(imark.eq.0)then
				j_o(j_gpiout)%i(11)=j_o(j_gpiout)%i(11)+1
				imark=j_o(j_gpiout)%i(11)
			endif !if(imark.eq.0)  17914
			ii1=mod(imark,10)
			j_o(j_gpiout)%txt(nl)(i2+ipt+6:i2+ipt+6)=char(ii1+48)
			ii2=imark-ii1
			if(ii2.ne.0)then
				ii2=imark/10
				j_o(j_gpiout)%txt(nl)(i2+ipt+5:i2+ipt+5)=char(ii2+48)
			endif !if(ii2.ne.0)  17921
 
		endif !if(j_otype(j_gpmark).eq.j_ipchar)  17908
	endif !if(ipt.gt.0)  17907
	nlres=(j_o(j_gpiout)%i(4)+1)/2  !reserved lines
	if(iplot.eq.1)j_o(j_gpiout)%i(1)=j_gpbas+nlres
 
end subroutine !subroutine j_gpplot(text,add)
 
subroutine j_replacefig(line,text)
	integer,intent(in)::line
	character*(*),intent(in)::text
	le=len(text)
	j_o(j_gpiout)%txt(line)=text
	j_o(j_gpiout)%i2(line)=le
end subroutine !subroutine j_replacefig(line,text)
 
subroutine j_putfigbreak(nu)
	integer,intent(in),optional::nu
	if(present(nu))then
		write(nu,*)' '
 
	else !if(present(nu))then
		line=j_o(j_gpiout)%i(1)+1
		j_o(j_gpiout)%txt(line)=' '
		j_o(j_gpiout)%i2(line)=1
		j_o(j_gpiout)%i(1)=line
	endif !if(present(nu))  17943
 
end subroutine
 
subroutine j_putfigxy(x,y,se,nu)
	double precision,intent(in)::x,y
	double precision, intent(in),optional::se
	integer,intent(in),optional::nu
	if(present(nu))then
		write(nu,*)real(x),real(y)
 
	else !if(present(nu))then
		line=j_o(j_gpiout)%i(1)+1
		if(line.gt.j_o(j_gpiout)%i(2))call j_inctxt(j_gpiout,0)
		if(present(se))then
			write(j_o(j_gpiout)%txt(line),*)real(x),real(y),real(se)
		else !if(present(se))then
			write(j_o(j_gpiout)%txt(line),*)real(x),real(y)
 
		endif !if(present(se))  17965
		j_o(j_gpiout)%i2(line)=j_lentrim(j_o(j_gpiout)%txt(line))
		j_o(j_gpiout)%i(1)=line
	endif !if(present(nu))  17959
 
	j_o(j_gpiout)%d(7)=min(j_o(j_gpiout)%d(7),x)
	j_o(j_gpiout)%d(8)=max(j_o(j_gpiout)%d(8),x)
	j_o(j_gpiout)%d(9)=min(j_o(j_gpiout)%d(9),y)
	j_o(j_gpiout)%d(10)=max(j_o(j_gpiout)%d(10),y)
 
end subroutine !subroutine j_putfigxy(x,y,se,nu)
subroutine j_pause(prompt,do)
	character*(*),optional::prompt
	logical,optional::do
	!	write(6,*)'startpasus recurs ',j_recursion
1000	format(a,$)
78		if(present(prompt))then
		write(6,1000)prompt
	else !if(present(prompt))then
		write(6,1000)'<ret>'
	endif !78		if(present(prompt))  17986
	j_ispause=.true.
	!j_cline=' '
	read(5,'(a)')j_cline
	!	write(6,*)j_cline.eq.' ',j_cline.eq.'',j_cline
 
	if(j_cline.eq.' ')goto 90
 
 
	lec=len_trim(j_cline)
	call j_clean(j_cline,lec)
	if(index(j_cline(1:lec),'=;list(').gt.0)then
		j_inpr(1:lec)=j_cline(1:lec)
		j_linpr=lec
		call printstar(j_yes)
		goto 78
	endif !if(index(j_cline(1:lec),'=;list(').gt.0)  18001
 
 
	if(j_cline(1:lec).eq.'e'.or.j_cline(1:lec).eq.'end'.or.j_cline(1:lec).eq.'/')then
		!			if(j_ninc.eq.1)goto 90
		write(6,*)'err return from pause'
		j_err=.true.;goto 90
	endif !if(j_cline(1:lec).eq.'e'.or.j_cline(1:lec).eq.'end'.or.j_c  18009
	if(present(do))then
		write(6,*)'cline ',j_cline(1:lec)
		call j_command(j_cline(1:lec))
		if(j_err)then
			j_err=.false.
			write(6,*)'try again, <ret> to continue e => error return'
		endif !if(j_err)  18017
		goto 78
	endif !if(present(do))  18014
90	j_ispause=.false.
	!	write(6,*)'pause,end ',j_recursion
end subroutine !subroutine j_pause(prompt,do)
integer function j_intloc(ivec,lenvec,i)
	integer,dimension(*),intent(in):: ivec
	integer,intent(in) ::lenvec,i
	do j_intloc=1,lenvec
		if(i.eq.ivec(j_intloc))return
	enddo !j_intloc=1,lenvec  18029
	j_intloc=0
end function
 
integer function j_incline() !what was the last incline
	if(j_ninc.gt.1)then
		iiv=j_inciv( j_ninc)
		j_incline=j_o(iiv)%i(6)
	else
		j_incline=-1
 
	endif !if(j_ninc.gt.1)  18036
 
 
end function
 
subroutine j_inclinegoto(line)
	integer,intent(in)::line
	iiv=j_inciv( j_ninc)
	if(line.gt.j_o(iiv)%i(5).or.line.le.0)then
		write(6,*)'*j* trying to go to incl-line ',line,  'but max is ',j_o(iiv)%i(5)
		j_err=.true.
		return
	endif !if(line.gt.j_o(iiv)%i(5).or.line.le.0)  18050
	j_o(iiv)%i(6)=line-1 !so getinput get correct line
 
end subroutine
 
! subroutine j_mergelist(list,n)
! integer,dimension(*),list
! integer n
! n2=0
! eka:		do i=1,n
! do j=1,n2
! if(list(i).eq.list(j))cycle eka
! enddo !j=1,n2  16421
! n2=n2+1
! list(n2)=list(i)
! enddo eka !:		do i=1,n  16420
! n=n2
 
! end subroutine
