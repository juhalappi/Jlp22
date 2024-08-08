!
! Jlp22 Software
!
! Copyright (C) 2022 Juha Lappi and Natural Resources Institute Finland (Luke)
! Author  Juha Lappi
!
! See LICENSE file
!-----------------------------------------------------------------------
!
! j_.f90	Main program (technically a subroutine), initialization and core functions
!
!-----------------------------------------------------------------------
 
subroutine j_sub(remain,jparf,error)
	logical,intent(in)::remain
	character*32,intent(inout) ::jparf
	logical,intent(out)::error
	logical ::p=.false.
	!	if(.not.j_inited)then
	j_remain=remain
	!	le=len_trim(parf)
	!	write(6,*)j_remain,len_trim(jparf),jparf(1:len_trim(jparf))
	!if(le.gt.0)then
	!	write(6,*)'calling j_init0'
	call j_jinit0(jparf)
	!else
	!	write(6,*)'from j_init0'
	!	call j_jinit0()
	!endif
	if(j_err)return
	! if(p)then
	! do jj=1,j_nfunctions_
	! write(6,*)jj,j_functions(jj)
	! enddo !do jj=1,j_nfunctions_
	! write(6,*)'bas0 ', j_fbspec ,' obj ', j_fbobj,' trans: ', j_fbtrans, ' bloop', j_fbloop, &
	! ' boper ', j_fboper,' barit ', j_fbarit, 'arit2 ',j_fbarit2, 'bdist ', j_fbdist, &
	! ' bran ', j_fbran, 'binter ', j_fbinter,' blist', j_fblist,' btex ',j_fbtex,' bfile ', j_fbfile
	! write(6,*)' bio ', j_fbio,' bmatr ',j_fbmatr,' bdata', j_fbdata,' bstat ', j_fbstat, &
	! ' bjlp ',j_fbjlp,' bsimu ',j_fbsimu,' bfig ', j_fbfig,' bspli', j_fbspli, ' bbit ',j_fbbit, &
	! ' bmisc ',j_fbmisc
	! endif !if(p)then
	!	j_inited=.true.
	!	endif
	!	10 	continue
	!write(6,*)'<878>',hep
	! call j_getinput('SIT>',inprint)
	! !write(6,*)'<33got ',j_inp(1:j_linp)
	! if(j_inp(1:j_linp).eq.'end')stop
	! !write(6,'(a,20i5/)')'<150>teku',j_o(j_ivcursor)%i(0:20)
	! call j_interpret(j_inp(1:j_linp),j_ivcursor)
	! !write(6,'(a,20i5/)')'<151>teku',j_o(j_ivcursor)%i(0:20)
	! call dotrans(j_ivcursor,1)
	! j_o(j_ivcursor)%i(0)=0
	!	j_o(j_ivcursor)%i(1)=0
	!	write(6,*)'nul(0)',j_ninc
	call j_sit()
	!write(6,*)'tas2,remain',remain,'j_nused ',j_nused,' j_recursion ',j_recursion
	!	call j_command('sit()')
	!rite(6,*)'j_nused ',j_nused
	if(.not.j_remain)then
		!	if(j_nused.gt.0)write(6,*)'j_nused ',j_nused
		do while(j_nused.gt.0)
			!	write(6,*)'67765'
			nu=j_nunits(j_nused)
			!		write(6,*)'nu',nu
			iv=j_unitchar(nu)
			!		write(6,*)'<66 nu iv ',nu,iv
			iv2=abs(iv)
			call j_getchar(iv2,j_filename,le);if(j_err)return
 
			!	write(6,*)'<66leaaf  ',le
			!	write(6,*)j_filename
			if(iv.lt.0)then
				write(6,*)'closing read -file ',j_filename(1:le)
 
			else
				write(6,*)'closing write -file ',j_filename(1:le)
 
			endif !if(iv.lt.0)     73
			call j_closeunit(nu)
		enddo !while(j_nused.gt.0)     62
		!		write(6,*)'j_nused ',j_nused
 
	endif !if(.not.j_remain)     60
	!write(6,*)'tassss'
	error=j_err
 
	return
	!	goto 10
 
 
end subroutine !program j
 
subroutine j_jinit0(jparf)
	character*32,intent(inout)::jparf
 
	integer ::nerr=0
	logical :: jpar=.false.
	integer i_, it
	character*(1) ch0
	character*256 cmd
	!	write(6,*)'jinit0',jparf,len_trim(jparf)
	1357 format(a25,' : ',i4,' functions ',i4,' options ',i4,' object types')
	!call get_command(cmd)
	!lencmd=len_trim(cmd)
	!istcmd=j_nextlim(cmd(1:lencmd),1,lencmd,' ')
	!write(6,*)'>44',lencmd,istcmd,cmd
	if(.not.j_inited)then
		!the following is updated with the Jlp22-precompiler,DO NOT CHANGE ANYTHING
		j_title='jlp22    22. 04. ???? (c) Juha Lappi and Natural Resources Institute Finland'
		write(6,*)j_title
		write(6,1357)'jlp22 ',j_nfunctions_,j_noptions_,j_notypes
		write(6,1357)o1_title,o1_nfunctions,o1_noptions,o1_nobjecttypes
		write(6,1357)o2_title,o2_nfunctions,o2_noptions,o2_nobjecttypes
		write(6,1357)o3_title,o3_nfunctions,o3_noptions,o3_nobjecttypes
 
		ib0=index(j_title,' ')
		j_leno1_title=j_nextlim(o1_title,1,len_trim(o1_title),' ')-1
		j_leno2_title=j_nextlim(o2_title,1,len_trim(o2_title),' ')-1
		j_leno3_title=j_nextlim(o3_title,1,len_trim(o3_title),' ')-1
		if(j_title(1:ib0).eq.o1_title(1:j_leno1_title).or.j_title(1:ib0).eq.o2_title(1:j_leno2_title).or. &
			j_title(1:ib0).eq.o3_title(1:j_leno3_title).or.&
			o1_title(1:j_leno1_title).eq.o2_title(1:j_leno2_title).or. &
			o1_title(1:j_leno1_title).eq.o3_title(1:j_leno3_title).or. &
				o2_title(1:j_leno2_title).eq.o3_title(1:j_leno3_title))then
			write(6,*)'*two packages cannot have the same title'
			nerr=1
 
		endif !if(j_title(1:ib0).eq.o1_title(1:j_leno1_title).or.j_title(    121
		!	lef=len_trim(jparf)
		!	if(present(jparf))then
		call j_jinit(jparf)
		jpar=len_trim(jparf).gt.0
 
		!	write(6,*)len_trim(jparf),jparf
		!else
		!	call j_jinit()
 
		!	endif
		if(j_err)return
 
		call o1_init()
		if(j_err)then
			write(6,*)'*o1* error initiliazing ',o1_title
			nerr=nerr+1
			j_err=.false.;j_errprint=.false.
		endif !if(j_err)    143
 
		call o2_init()
		if(j_err)then
			write(6,*)'*o2* error initiliazing ',o2_title
			nerr=nerr+1
			j_err=.false.;j_errprint=.false.
		endif !if(j_err)    150
 
		call o3_init()
		if(j_err)then
			write(6,*)'*o3* error initiliazing ',o3_title
			nerr=nerr+1
			j_err=.false.;j_errprint=.false.
		endif !if(j_err)    157
 
		if(nerr.gt.0)then
			9		write(6,*)'initialization failed, press any key to stop'
			if(j_remain)read(5,'(a)')ch
			j_stop=.true.
			j_err=.true.
			return
		endif !if(nerr.gt.0)    163
 
		do i_=1,j_noptions_
			j_lenoptions(i_)=len_trim(j_options(i_))
		enddo !i_=1,j_noptions_    171
		it=j_noptions_
		do i_=1,o1_noptions
			it=it+1
			j_lenoptions(it)=len_trim(o1_options(i_))
		end do !i_=1,o1_noptions    175
		do i_=1,o2_noptions
			it=it+1
			j_lenoptions(it)=len_trim(o2_options(i_))
		end do !i_=1,o2_noptions    179
		do i_=1,o3_noptions
			it=it+1
			j_lenoptions(it)=len_trim(o3_options(i_))
		end do !i_=1,o3_noptions    183
 
		do i_=1,j_nfunctions_
			j_lenfunctions(i_)=len_trim(j_functions(i_))
		enddo !i_=1,j_nfunctions_    188
		it=j_nfunctions_
		do i_=1,o1_nfunctions
			it=it+1
			j_lenfunctions(it)=len_trim(o1_functions(i_))
		end do !i_=1,o1_nfunctions    192
		do i_=1,o2_nfunctions
			it=it+1
			j_lenfunctions(it)=len_trim(o2_functions(i_))
		end do !i_=1,o2_nfunctions    196
		do i_=1,o3_nfunctions
			it=it+1
			j_lenfunctions(it)=len_trim(o3_functions(i_))
		end do !i_=1,o3_nfunctions    200
 
		do i_=1,j_notypes
			j_lenobjecttypes(i_)=len_trim(j_otypes(i_))
		enddo !i_=1,j_notypes    205
		it=j_notypes
		do i_=1,o1_nobjecttypes
			it=it+1
			j_lenobjecttypes(it)=len_trim(o1_objecttypes(i_))
		end do !i_=1,o1_nobjecttypes    209
		do i_=1,o2_nobjecttypes
			it=it+1
			j_lenobjecttypes(it)=len_trim(o2_objecttypes(i_))
		end do !i_=1,o2_nobjecttypes    213
		do i_=1,o3_nobjecttypes
			it=it+1
			j_lenobjecttypes(it)=len_trim(o3_objecttypes(i_))
		end do !i_=1,o3_nobjecttypes    217
 
		do i_=1,j_notypes
			j_lenobjecttypes(i_)=len_trim(j_otypes(i_))
		enddo !i_=1,j_notypes    222
		it=j_notypes
		do i_=1,o1_nobjecttypes
			it=it+1
			j_lenobjecttypes(it)=len_trim(o1_objecttypes(i_))
		end do !i_=1,o1_nobjecttypes    226
		do i_=1,o2_nobjecttypes
			it=it+1
			j_lenobjecttypes(it)=len_trim(o2_objecttypes(i_))
		end do !i_=1,o2_nobjecttypes    230
		do i_=1,o3_nobjecttypes
			it=it+1
			j_lenobjecttypes(it)=len_trim(o3_objecttypes(i_))
		end do !i_=1,o3_nobjecttypes    234
		j_inited=.true.
 
	endif !if(.not.j_inited)    108
	if(jpar) then
		write(6,*)'getting from ',jparf,' using:'
		write(6,*)' '
		write(6,*)'% ;incl('//jparf(1:len_trim(jparf))//')'
		!write(6,*)'j-namedv ',j_named
		!	write(6,*)'hep:',';incl('//jparf(1:len_trim(jparf))//')'
		call j_incl(';incl('//jparf(1:len_trim(jparf))//')' )   !j_command(";incl('j.par')")%%%
		!write(6,*)'<3633tas ',j_nul(0:1)
		if(j_err)then
			write(6,*)'*error in doing initilization commands in j.par, correct j.par and include it again'
			call j_errexit()
			j_err=.false.;j_errprint=.false.
		endif !if(j_err)    249
	endif !if(jpar)    241
	!	write(6,*)'<552',istcmd,lencmd,cmd(istcmd+1:lencmd)%
	!	read(5,*)n
	!write(6,*)'junit cmd',cmd
	! if(istcmd.lt.lencmd) then
	! write(6,*)'TASSA',";incl('"//cmd(istcmd+1:lencmd)//"')"
	! call j_command(";incl('"//cmd(istcmd+1:lencmd)//"')")
	! if(j_err)then
	! call j_errexit()
	! j_err=.false.
	! endif !if(j_err)then
 
end subroutine j_jinit0 !subroutine jinit0()
 
subroutine j_jinit(jparf)
 
 
	character*32,intent(inout)::jparf
	logical ::jpar
	logical yes_
	character*1 chp
 
	logical :: pak
	integer(1)::iloc
 
	!	write(6,*)'j_init ',jparf,len_trim(jparf)
 
	call j_isanyin(o1_functions, o1_nfunctions, j_functions, j_nfunctions_, 'o1_functions', 'j_functions')
	call j_isanyin(o2_functions, o2_nfunctions, j_functions, j_nfunctions_, 'o2_functions', 'j_functions' )
	call j_isanyin(o3_functions, o3_nfunctions, j_functions, j_nfunctions_, 'o3_functions', 'j_functions')
	call j_isanyin(o2_functions, o2_nfunctions, o1_functions, o1_nfunctions, 'o2_functions', 'o1_functions')
	call j_isanyin(o3_functions, o3_nfunctions, o1_functions, o1_nfunctions, 'o3_functions', 'o1_functions' )
	call j_isanyin(o3_functions, o3_nfunctions, o2_functions, o2_nfunctions, 'o3_functions', 'o2_functions')
 
	call j_isanyin(o1_options, o1_noptions, j_options, j_noptions_, 'o1_options', 'j_options')
	call j_isanyin(o2_options, o2_noptions, j_options, j_noptions_, 'o2_options', 'j_options' )
	call j_isanyin(o3_options, o3_noptions, j_options, j_noptions_, 'o3_options', 'j_options')
	call j_isanyin(o2_options, o2_noptions, o1_options, o1_noptions, 'o2_options', 'o1_options')
	call j_isanyin(o3_options, o3_noptions, o1_options, o1_noptions, 'o3_options', 'o1_options' )
	call j_isanyin(o3_options, o3_noptions, o2_options, o2_noptions, 'o3_options', 'o2_options')
 
	call j_isanyin(o1_objecttypes, o1_nobjecttypes, j_otypes, j_notypes, 'o1_objecttypes', 'j_otypes')
	call j_isanyin(o2_objecttypes, o2_nobjecttypes, j_otypes, j_notypes, 'o2_objecttypes', 'j_otypes' )
	call j_isanyin(o3_objecttypes, o3_nobjecttypes, j_otypes, j_notypes, 'o3_objecttypes', 'j_otypes')
	call j_isanyin(o2_objecttypes, o2_nobjecttypes, o1_objecttypes, o1_nobjecttypes, 'o2_objecttypes', 'o1_objecttypes')
	call j_isanyin(o3_objecttypes, o3_nobjecttypes, o1_objecttypes, o1_nobjecttypes, 'o3_objecttypes', 'o1_objecttypes' )
	call j_isanyin(o3_objecttypes, o3_nobjecttypes, o2_objecttypes, o2_nobjecttypes, 'o3_objecttypes', 'o2_objecttypes')
 
	do i=1,j_nnamedfuncarg
		le=len_trim(j_namedfuncarg(i))
		ifun=j_isin(j_namedfuncarg(i)(1:le),j_functions,j_nfunctions_)
		if(ifun.le.0)then
			write(6,*)'*j* in module jmod element ', j_namedfuncarg(i)(1:le), ' in namedfuncarg not a function'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedfuncarg(ifun)=.true.
 
			!	write(6,*)'named ifun',ifun
		endif !if(ifun.le.0)    305
	enddo !i=1,j_nnamedfuncarg    302
	do i=1,o1_nnamedfuncarg
		le=len_trim(o1_namedfuncarg(i))
		ifun=j_isin(o1_namedfuncarg(i)(1:le),o1_functions,o1_nfunctions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o1mod element ', o1_namedfuncarg(i)(1:le), ' in namedfuncarg not a function'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedfuncarg(j_nfunctions_+ifun)=.true.
		endif !if(ifun.le.0)    317
	enddo !i=1,o1_nnamedfuncarg    314
 
	do i=1,o2_nnamedfuncarg
		le=len_trim(o2_namedfuncarg(i))
		ifun=j_isin(o2_namedfuncarg(i)(1:le),o2_functions,o2_nfunctions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o2mod element ', o2_namedfuncarg(i)(1:le), ' in namedfuncarg not a function'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedfuncarg(j_nfuncs1+ifun)=.true.
		endif !if(ifun.le.0)    328
	enddo !i=1,o2_nnamedfuncarg    325
	!write(6,*)'olo2'
	do i=1,o3_nnamedfuncarg
		le=len_trim(o3_namedfuncarg(i))
		ifun=j_isin(o3_namedfuncarg(i)(1:le),o3_functions,o3_nfunctions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o3mod element ', o3_namedfuncarg(i)(1:le), ' in namedfuncarg not a function'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedfuncarg(j_nfuncs2+ifun)=.true.
		endif !if(ifun.le.0)    339
	enddo !i=1,o3_nnamedfuncarg    336
	!write(6,*)'olo3'
	do i=1,j_nnamedoptarg
		le=len_trim(j_namedoptarg(i))
		ifun=j_isin(j_namedoptarg(i)(1:le),j_options,j_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module j _mod element ', j_namedoptarg(i)(1:le), ' in namedoptarg not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedoptarg(ifun)=.true.
		endif !if(ifun.le.0)    350
	enddo !i=1,j_nnamedoptarg    347
	!write(6,*)'olnam'
	do i=1,o1_nnamedoptarg
		le=len_trim(o1_namedoptarg(i))
		ifun=j_isin(o1_namedoptarg(i)(1:le),o1_options,o1_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o1 _mod element ', o1_namedoptarg(i)(1:le), ' in namedoptarg not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedoptarg(j_noptions_+ifun)=.true.
		endif !if(ifun.le.0)    361
	enddo !i=1,o1_nnamedoptarg    358
	!write(6,*)'olnamo1'
	do i=1,o2_nnamedoptarg
		le=len_trim(o2_namedoptarg(i))
		ifun=j_isin(o2_namedoptarg(i)(1:le),o2_options,o2_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o2_ mod element ', o2_namedoptarg(i)(1:le), ' in namedoptarg not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedoptarg(j_nopts1+ifun)=.true.
		endif !if(ifun.le.0)    372
	enddo !i=1,o2_nnamedoptarg    369
 
	do i=1,o3_nnamedoptarg
		le=len_trim(o3_namedoptarg(i))
		ifun=j_isin(o3_namedoptarg(i)(1:le),o3_options,o3_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o3_ mod element ', o3_namedoptarg(i)(1:le), ' in namedoptarg not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedoptarg(j_nopts2+ifun)=.true.
		endif !if(ifun.le.0)    383
	enddo !i=1,o3_nnamedoptarg    380
	!write(6,*)'olnamo3'
	!newvar
	do i=1,j_nnewvar
		le=len_trim(j_newvar(i))
		ifun=j_isin(j_newvar(i)(1:le),j_options,j_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module j _mod element ', j_newvar(i)(1:le), ' in newvar not an option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnewvar(ifun)=.true.
		endif !if(ifun.le.0)    395
	enddo !i=1,j_nnewvar    392
 
	do i=1,o1_nnewvar
		le=len_trim(o1_newvar(i))
		ifun=j_isin(o1_newvar(i)(1:le),o1_options,o1_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o1 _mod element ', o1_newvar(i)(1:le), ' in newvar not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnewvar(j_noptions_+ifun)=.true.
		endif !if(ifun.le.0)    406
	enddo !i=1,o1_nnewvar    403
 
	do i=1,o2_nnewvar
		le=len_trim(o2_newvar(i))
		ifun=j_isin(o2_newvar(i)(1:le),o2_options,o2_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o2 _ mod element ', o2_newvar(i)(1:le), ' in newvar not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnewvar(j_nopts1+ifun)=.true.
		endif !if(ifun.le.0)    417
	enddo !i=1,o2_nnewvar    414
 
	do i=1,o3_nnewvar
		le=len_trim(o3_newvar(i))
		ifun=j_isin(o3_newvar(i)(1:le),o3_options,o3_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o3_ mod element ', o3_newvar(i)(1:le), ' in newvar not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnewvar(j_nopts2+ifun)=.true.
		endif !if(ifun.le.0)    428
	enddo !i=1,o3_nnewvar    425
	!write(6,*)'olnew'
	!codeoptions
 
	do i=1,j_ncodeoptions_
 
		j_codeoption_(j_codeoptions(i))=.true.
 
	enddo !i=1,j_ncodeoptions_    438
 
	!write(6,*)'olcode'
	do i=1,o1_ncodeoptions,1
		le=len_trim(o1_codeoptions(i))
		ifun=j_isin(o1_codeoptions(i)(1:le),o1_options,o1_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o1 _mod element ', o1_codeoptions(i)(1:le), ' in codeoptions not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_codeoption_(j_noptions+ifun)=.true.
		endif !if(ifun.le.0)    448
	enddo !i=1,o1_ncodeoptions,1    445
 
	!write(6,*)'olcode1'
	do i=1,o2_ncodeoptions,1
		le=len_trim(o2_codeoptions(i))
		ifun=j_isin(o2_codeoptions(i)(1:le),o2_options,o2_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o2 _mod element ', o2_codeoptions(i)(1:le), ' in codeoptions not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_codeoption_(j_nopts1+ifun)=.true.
		endif !if(ifun.le.0)    460
	enddo !i=1,o2_ncodeoptions,1    457
 
	!write(6,*)'olcode2'
 
	do i=1,o3_ncodeoptions,1
		le=len_trim(o3_codeoptions(i))
		ifun=j_isin(o3_codeoptions(i)(1:le),o3_options,o3_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o3 _mod element ', o3_codeoptions(i)(1:le), ' in codeoptions not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_codeoption_(j_nopts2+ifun)=.true.
		endif !if(ifun.le.0)    473
	enddo !i=1,o3_ncodeoptions,1    470
	!write(6,*)'olcode3'
 
	if(j_err) then
		write(6,*)'*j* transformations cannot be interpreted, close the program with end command'
		j_err = .false.
		!return
	else !if(j_err) then
		!	j_checkedo = .true.  ???
	endif !if(j_err)    482
 
	allocate(j_optarg2(1:300))
	!write(6,*)'allcate',allocated(j_optarg2)
	j_mxconstantv=5000  ! %%constant max number of constants this can be increased
	! %constant   if it is too smaal it is increased automatically
	lename_=12
	icontrol=0
	!	write(6,*)'<3663',jparf,len_trim(jparf)
	goto 567
77		write(6,*)'*j* error opening jpar-file ',jparf
	j_err = .true.
	return
 
567		lep=len_trim(jparf)
	if(lep.gt.0)then
		inquire(file=jparf(1:lep),exist=jpar)
		if(.not.jpar)then
			write(6,*)'*j* jpar-file ',jparf(1:lep),' does not exist'
			j_err = .true.
			return
		endif !if(.not.jpar)    505
 
	else
 
		lep=5
		inquire(file='j.par',exist=jpar)
		if(jpar)jparf='j.par'
		!	write(6,*)'<333',jpar
	endif !if(lep.gt.0)    503
	j_mxnamedv=5000
	if(jpar)then
		open (unit=1,file=jparf(1:lep),err=77)
		read(1,'(a)')j_cline
		!	write(6,*)'cline ',j_cline
		close(1)
		le=len_trim(j_cline)
		if(le.gt.1.and.j_cline(1:1).eq.'*')then
			read(j_cline(2:le),*,err=99)j_mxnamedv !read(1,'(a1,i)',err=99)chp,mxnamedv
			if(j_mxnamedv.lt.200)then
				write(6,*)'max named objects should be at least 200'
				j_err=.true.
				return
			endif !if(j_mxnamedv.lt.200)    527
		endif !if(le.gt.1.and.j_cline(1:1).eq.'*')    525
 
	end if !if(jpar)    519
	write(6,*)' '
	write(6,*)'max named objects  ',j_mxnamedv
	write(6,*)' '
	17 j_nv=j_mxnamedv+j_mxtemporalv
	j_mxv=j_nv+j_mxconstantv
	j_nvtot=j_nv
 
 
	allocate(j_v(1:j_mxv))  !
	allocate(j_prot(1:j_mxnamedv))
	j_prot=.false.
	allocate(j_o(1:j_nv))
 
	do iv=j_mxnamedv+j_mxtemporalv0+1,j_mxnamedv+j_mxtemporalv !%dervative
		allocate(j_o(iv)%d(1:j_mxder0))
	enddo !iv=j_mxnamedv+j_mxtemporalv0+1,j_mxnamedv+j_mxtemporalv    548
	j_mxder=j_mxder0
 
	allocate(j_otype(1:j_mxv))
	!allocate(j_otitle(1:j_mxnamedv))
	allocate(j_iob(1:j_nv))   !possibly j_maxnamedv would be enough
	allocate(j_io(1:j_nv))   !possibly j_maxnamedv would be enough
	!	allocate(j_locked(1:j_nv))   !possibly j_maxnamedv would be enough
	!write(6,*)'otas'
	j_v=j_0 ;j_otype=j_ipreal
	!	j_otype(0)=0
	j_iob=0;j_io=0
 
	allocate( j_o(j_ivnames)%i(0:j_mxnamedv+1),j_o(j_ivnames)%ch(1:lename_*j_mxnamedv))
	!	j_temporalbas=j_mxnamedv+j_mxtemporalv0
	j_o(j_ivnames)%i(0)=0;j_o(j_ivnames)%i(1)=1
	! endif !if(istcmd.lt.lencmd) then
	j_nutf8=6; allocate(j_utf8(1:6));j_utf8=(/char(228), char(229),char(246),char(196),char(214),char(197)/)
	!   'ä','å','ö','Ä','Ö','Å'/)
	!	write(6,*)'>45/',j_utf8
	allocate(j_ocode(1:6));j_ocode=(/char(132),char(134),char(148),char(142),char(153),char(143)/)
 
	j_lotypes =len_trim(j_otypes)
 
	!j_ivstartedjlp=j_ivzero
	j_named=1
	j_otype(j_ivnames)=j_iptext
	j_err=.false.;j_errprint=.false.
	!the lines ended with !!! are not put tpo the Latex code
	!Section pref Predefined objects
	! The following objects are generated during the initilaization.
	!endheader
	!
	!Tabular 2 25mm 45mm 70mm
	call j_puttext(j_ivnames,'Names') !!!
	!write(6,*)'hep',j_err
	! Names& Text& Text object containg the names of named objects
 
	ivout_=j_getobject(0,'Pi',j_ipreal)   !!!
	!	write(6,*)'<44	j_named=1',	j_named
 
	j_v(j_ivpi)=j_pi  !3.14159265358979323  !!!
 
	! Pi&REAL&The value of Pi (=3.1415926535897931)
 
	ivout_=j_getobject(0,'$0',j_ipreal)  !!!
 
 
 
	ivout_=j_getobject(0,'$1',j_ipreal) !!!
	j_v(j_ivone)=j_1!!!
 
	ivout_=j_getobject(0,'Inf',j_ipreal)  !!!
 
	j_v(j_ivinf)=j_inf  !10.d300
 
	ivout_=j_getobject(0,'Tolast',j_ipreal) ! ivtempdata)	 !!!
	j_v(ivout_)=j_ninf  !-10.d300
 
	ivout_=j_getobject(0,'Maxnamed',j_ipreal) !!!
	j_v(ivout_)=j_mxnamedv !!!
 
 
 
 
	ivout_=j_getobject(0,'$Recursion',j_ipreal)! ivdebug) !!!
	j_v(j_ivrecursion)=0 !!!
	!	j_recursion=0 doen already
	!write(6,*)'hep2',hep2
 
	ivout_=j_deftrans(0,'$Cursor$',1000,0,0,iii,iii,iii,0) ! ivcursor !!!
	! $Cursor$& TRANS & The transformation object used to run sit> prompt
 
	ivout_=j_deftrans(0,'$Cursor2$',200,0,0,iii,iii,iii,0) !ivcursor2 !!!
	! $Cursor2$& TRANS & Another transformation object used to run >
	!sit> prompt
	!	$Val
	ivot_=j_deftrans(0,'$Val$',200,0,0,iii,iii,iii,0) !ivval !!!
	! Val&TRANS& Transformation object used to extract values of mathematical statements, used,
	! e.g., in input programming.
	!	 call j_deflist(0,'$Data$',ivout_,nres=1) ! ivtempdata)
 
	ivout_=j_getobject(0,'Round',j_ipreal) !!!
	!Round &REAL& jlp(): The current round through treatment units in jlp() function.
	! Can be used to define stopping criterion.
 
	!Change&REAL& jlp(): The change of objective in jlp()  in one round before finding feasible and thereafter
	!the change in 10 rounds.
	ivout_=j_getobject(0,'Change%',j_ipreal) !; j_v(ivchangep)=0 !!!
 
	ivout_=j_getobject(0,'Imp',j_ipreal)  !!!
	!Imp&REAL& jlp(): The number of improvements obtained from schedules outside the current active
	! set when updating the active set.
 
	! $Data$ & List& Default data set name for a new data set created by data()-function.
 
	!call j_deflist(0,'LastData',ivout_,nres=1) ! ivlastdata) !!
	ivout_=j_getobject(0,'LastData',j_ipreal)
 
	ivout_=j_getobject(0,'Accepted',j_ipreal) !!!
 
 
	ivout_=j_getobject(0,'Active%',j_ipreal)  ! ivobsdef) !!!
	! Obs& REAL & The default name of variable obtaining the the number of
	! observation in a data set. Given in the data() function. Newdata ?
	!Maxnamed &REAL& The maximum number of named objects. Determined via j.par in
	! initilaization. default is 5000.
 
	ivout_=j_getobject(0,'Record',j_ipreal) ! ivrecord) !!!
	! Record& REAL & The name of variable obtaining the the number of
	! record when reading data in data() function. Has the same value as Obs variabel
	! if no records are rejected.
 
	ivout_=j_getobject(0,'Subrecord',j_ipreal) ! ivsubrecord) !!!
	! Subecord& REAL & The name of variable obtaining the the number of
	! record when reading subdata in data() function. Has the same value as obs variable
	! if no records are rejected.
 
	ivout_=j_getobject(0,'Duplicate',j_ipreal) ! ivduplicate) !!½!
	!Duplicate& REAL& A special variable used in data() function when duplicating observations
 
	! LastaData &List&	A list object referring to the last data set made, used as default data set.
 
	! $Buffer&Char& A special character object used by the write() function.
	ivout_=j_deftext(0,'$Input0$',20,1500) ! ivinput0) !!!
 
	! $Input$& Text & Text object used for original input line.
	ivout_=j_deftext(0,'$Input1$',20,1400) ! ivinput1) !!!
	!write(6,*)'$Input1$',j_o(ivout_)%i(0),size(j_o(ivout_)%i),ubound(j_o(ivout_)%i,dim=1)
	!and endcommnets are removed
	!1$Input1$ &Text& Text object for input line after removing blanks and comments.
	ivout_=j_deftext(0,'$Input2$',20,1400) ! !!!
	ivout=j_defchar(0,'bis') !define a character constant binary
 
	!Data &List&  List object used to indicate current data setsDat
 
	ivout=j_defchar(0,'b') !define a character constant binary j_ivb
	ivout=j_defchar(0,'B') !define a character constant  j_ivb2 double prec
	ivout=j_defchar(0,'di') !define a character constant  j_ivdi !intel
	ivout=j_defchar(0,'DI') !define a character constant  j_ivdi2 !dbouble
	ivout=j_defchar(0,'dg') !define a character constant   j_ivdg gfortran
	ivout=j_defchar(0,'DG') !define a character constant j_ivdg2
	ivout=j_defchar(0,'bn') !define a character constant
	ivout=j_defchar(0,'BN') !define a character constant
 
 
	ivout_=j_getobject(0,'CHAR',j_ipreal) !!!
	j_v(ivout_)=j_ipchar
 
	ivout_=j_getobject(0,'LIST2',j_ipreal) !!!
	!	j_v(ivout_)=j_iplist2
 
	ivout_=j_getobject(0,'MATRIX',j_ipreal) !!!
	j_v(ivout_)=j_ipmatrix
 
	ivout_=j_getobject(0,'TRANS',j_ipreal) !!!
	j_v(ivout_)=j_iptrans
 
	ivout_ =j_getobject(0,'LIST',j_ipreal) !!!
	j_v(ivout_)=j_iplist
 
	ivout_=j_getobject(0,'TXT',j_ipreal) !!!
	j_v(ivout_)=j_iptxt
 
	ivout_=j_getobject(0,'DATA',j_ipreal) !!!
	j_v(ivout_)=j_ipdata
 
	ivout_=j_getobject(0,'FIG',j_ipreal) !!
	j_v(ivout_)=j_ipfigure
 
	ivout_=j_getobject(0,'REGR',j_ipreal) !!!
	j_v(ivout_)=j_ipregr
 
	ivout_=j_getobject(0,'BITMATRIX',j_ipreal) !!
	j_v(ivout_)=j_ipbitmatrix
 
	ivout_=j_getobject(0,'REAL',j_ipreal) !!
	j_v(ivout_)=j_ipreal
	! set style line 1 lt rgb "red" lw 3
	! set style line 2 lt rgb "orange" lw 2
	! set style line 3 lt rgb "yellow" lw 3
	! set style line 4 lt rgb "green" lw 2
	! set style line 5 lt rgb "cyan" lw 3
	! set style line 6 lt rgb "blue" lw 2
	! set style line 7 lt rgb "violet" lw 3
	!	write(6,*)'hep2',j_err
	ivout=j_defmatrix(0,'Fakematrix',j_18,j_18,j_matreg)
	!write(6,*)'hep3',j_err
	j_otype(j_ivfakematrix)=j_ipreal  !otherwise defmatrix faisl as fakematrix is llocked
 
 
	ivout_=j_getobject(0,'Ninf',j_ipreal)   !was Black
	j_v(j_ivninf)=j_ninf
 
 
	ivout_=j_getobject(0,'Red',j_ipreal)
	j_v(j_ivred)=2.d0
 
	ivout_=j_getobject(0,'Green',j_ipreal)
	j_v(j_ivgreen)=3.d0
 
	ivout_=j_getobject(0,'Blue',j_ipreal)
	j_v(j_ivblue)=4.d0
 
	ivout_=j_getobject(0,'Cyan',j_ipreal)
	j_v(j_ivcyan)=5.d0
 
	ivout_=j_getobject(0,'Violet',j_ipreal)
	j_v(j_ivviolet)=6.d0
 
	ivout_=j_getobject(0,'Yellow',j_ipreal)
	j_v(j_ivyellow)=7.d0
 
	ivout_=j_getobject(0,'Orange',j_ipreal)
	j_v(j_ivorange)=8.d0
 
 
	! $&REAL& Object name used to indicate console and '*' format in reading and writing
	! commands.
	ivout_=j_getobject(0,'Maxlines',j_ipreal)! ivxstar) !!!
	j_v(j_ivmaxlines)=20
 
	!x#&REAL& Variable used when drawing functions.
	ivout_=j_getobject(0,'Selected',j_ipreal) ! ivselected) !!!
	!Selected& REAL &Variable used to indicate the simulator selected in simulations
	ivout_=j_getobject(0,'Printinput',j_ipreal) ! ivprintinput) !!!
	!Printinput& REAL& Variable used to specify how input lines are printed. Not properly used.
	j_v(j_ivprintinput)=2.d0 !!!
	ivout_=j_getobject(0,'Printoutput',j_ipreal) ! ivprintoutput) !!!
	!Prinoutpu& REAL& Variable used to indicate how much output is printed. Not properly used.
	j_v(j_ivprintoutput)=2.d0 !!!
 
	ivout_=j_getobject(0,'$Buffer',j_ipreal) ! ivbuffer) !!!
 
	! $Input2$Text object for input line after interpreting “-sequencies.$
	ivout_ =j_getobject(0,'Debug',j_ipreal)! ivdebug) !!!
 
	!write(6,*)'ivout_,j_ivdebug',ivout_,j_ivdebug
 
 
	! $Debug&REAL& Variable used to put debugging mode on.
 
	ivout_=j_getobject(0,'Regf',j_ipreal)! ivdebug) !!!free
 
	ivout_=j_getobject(0,'Resid',j_ipreal)  !!!
 
	ivout_=j_getobject(0,'Debugconsole',j_ipreal)  !!!  !unnecessar
	j_v(j_ivdebugconsole)=j_0 !!!
 
 
 
 
	! Accepted &REAL& The number of accepted observations in functions using data sets.
 
	ivout_=j_getobject(0,'Arg',j_ipreal) !  ivarg) !!!
	! Arg& REAL & The default argument name when using transformation object as a function.
 
 
	ivout_=j_getobject(0,'Continue',j_ipreal) !!!
	! Continue &REAL& If Continue has nonzero value then the control does not return to the
	! sit> prompt when an error occurs, but computation proceed from the first ;incl() file.
	! Used in the manual examples to demonstrate errors.
 
 
	ivout_=j_getobject(0,'Err',j_ipreal) !!!
	! Err&REAL& If Continue prevents the control from returning to sit> prompt
	! this variable tells whether an error has occured.
 
 
	ivout_=j_getobject(0,'Result',j_ipreal)  !!!
 
 
	ivout_=j_getobject(0,'Data',j_ipreal)  ! ivcurrentdata) !!
 
	ivout_=j_getobject(0,'$',j_ipreal) ! ivdollar) !!!
 
	ivout_=j_getobject(0,'Printresult',j_ipreal) ! ivdollar) !!!
	j_v(j_ivprintresult)=3
	!Result& ? & The default name of output object.
	!   Type varies according to the function
	!endtabular
	!endsection
	ivout_=j_getobject(0,'$$',j_ipreal) ! ivdollar) !!!
 
	ivout_=j_getobject(0,'Terminal',j_ipreal) ! ivdollar) !!!
 
	ivout_=j_getobject(0,'Window',j_ipreal) ! ivdollar) !!!
	ivout_=j_getobject(0,'All',j_ipreal)
	j_v(ivout_)=10000.d0 !not zero in irnages of getelem
	ivout_=j_getobject(0,'Debugtrans',j_ipreal)
	!write(6,*)'<666 ',ivout_
	j_v(ivout_)=j_0 !not zero in irnages of getelem
	ivout_=j_defchar(0,'bgaya')
	!	write(6,*)'<77 ',ivout_
	ivout_=j_deftrans(0,'$Cursori$',200,0,0,iii,iii,iii,0) !ivcursor2 !!!
 
	ivout_=j_getobject(0,'Fast%',j_ipreal)
	ivout_=j_getobject(0,'Obs',j_ipreal)
	ivout_=j_getobject(0,'Black',j_ipreal)
	j_v(j_ivblack)=j_1
	ivout_=j_getobject(0,'TEMPOUT',j_ipreal)
	ivout_=j_getobject(0,'Printpause',j_ipreal)
	ivout_=j_deftxt(0,'Incfilenames',j_mxinc)
	ivout_=j_getobject(0,'$invector',j_ipreal)
	ivout_=j_getobject(0,'$outlist',j_ipreal)
 
	!ivout_=j_deflist(0,'Incfiles',list0=j_mxinc)
	if(j_ivoutlist.ne.ivout_)then
		write(6,*)'predefined variables are mixed up,j_ivoutlist=',j_ivtempout,' but ivout_=',ivout_
		write(6,*)'j_named ',j_named
		j_err=.true.
		do i=1,j_named
			call j_getname(i)
			write(6,*)i,j_oname(1:j_loname)
		enddo !i=1,j_named    861
		j_stop=.true.
		return
	endif !if(j_ivoutlist.ne.ivout_)    857
 
	do j=1,j_mxinc
		iv=j_deftxt(0,'Incfile'//char(j+48),1)
		j_inciv(j)=iv
	enddo !j=1,j_mxinc    869
	!	write(6,*)'incfiles',j_inciv
 
	!	ivout_=j_getobject(0,'Varfor',j_ipreal)  !free
	allocate(j_vector(1:300))
	j_n_vector=300
	j_vector=0.
	allocate(j_dvector(1:300))
	j_n_dvector=300
	j_dvector=j_0
 
	return
	99 write(*,*)'first record in j.par must look: *2000 '
	write(6,*)'where the number gives the max number of named variables'
	write(6,*)'using default value 2000'
	close(1)
 
	!	j_mxnamedv=2000
 
	goto 17
 
end subroutine j_jinit !subroutine jinit(jpar)
 
! j_functions
 
 
recursive subroutine dotrans(iob,ioi)
 
	integer, intent(in) :: iob
	integer, intent(in) :: ioi
	integer ::io
	logical ::p,p4
 
	integer,dimension(j_nfunctions_)::gotos !for testing
	data gotos/&
		11,12,13,14,15,16,17,18, 19,& !special j_nfspec=8
		21,22,23,24,25,26, & !Objects j_nfobj=3
		31,32,33,34,35, & !transformations j_nftrans=3
		41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,&  !Loops and control j_nfloop=16
		61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79, & ! operations  j_nfoper=16
		81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100, & !arithmetic functions
		101,102,103,104,105,106,107,108,109,110,111,112,113, 114, &  ! arith continues j_nfarit=34
		121,122,123,124,125, & ! special artith j_nfarit2=5
		131,132,133,134,135,   &  ! probability distributions
		141,142,143,144,145,146,147, & ! random numbers
		151,152,153, &  !           interpolation
		161,162,163,164,165,166,167,168,169, 170,1701, & !list functions
		171,172,173, &                   ! text object
		181,182,183,184,185,186,187,188, &  ! file handling
		191,192,193,194,195,196,197, &   ! io
		201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218, 219,220,2201,2202,2203,&  ! matrices
		221,222,223,224,225,226,227,228,229,230,2301,2302,2303,2304, & !data functions
		231,232,233,234,235,236,237,238,239,240,241,242,243, & ! statistical functions
		251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272, & ! linear programming
		281,282,283,284,285,286, &    ! simulator
		291,292,293,294,295,296,297,298,& !    Figures
		301,302,303,304,305,306,307,308,309,&  !   splines , stem
		311,312,313,314,315,316,317, & ! bit functions
		321,322,323,324,325,326,327,328,329,330,331/
 
	p=j_v(j_ivdollar).eq.784.d0
	p4=j_v(j_ivdebug).ge.3.d0
	!	p=.true.
 
	! double precision:: deg
	! parameter ( deg=0.017453293d0)  !*deg makes degrees to radians
	! double precision:: todeg
	! parameter (todeg=57.2957795d0) ! makes radians to degrees
	! intrinsic :: dsin,dcos,dtan,dsqrt,dlog,dlog10,dexp,dcotan,dasin,dacos,datan
	! intrinsic::dsinh,dcosh,dtanh,anint,ceiling,nint,dnint
	! double precision coef,coef2,minv,maxv
	! logical ::reci
	!write(6,*)'err0',j_err
	j_recursion=j_recursion+1
	!	write(6,*)'j915recursion ',j_recursion
	if(j_recursion.gt.j_mxrecursion)then
		write(6,*)'recursion>maxrecusion =',j_mxrecursion
		j_err=.true.
		return
	endif !if(j_recursion.gt.j_mxrecursion)    947
	j_v(j_ivrecursion)= j_recursion !j_v(j_ivrecursion) + 1 ! $Recursion=$Recursion+1
 
	j_curline(j_recursion)=1
	io=ioi
	if(j_v(j_ivdebugtrans).ne.j_0)call j_debug(iob)
 
 
	! if(p)then useful when there is mixup
	! write(6,*)'funcs',j_nfunctions_
	! do j=1,j_nfunctions_
	! write(6,*)j,j_functions(j),gotos(j),j_minarg_(j),j_maxarg_(j)
	! enddo !j=1,j_nfunctions_    933
 
	! stop
	if(p)then
		write(6,*)'nteku in dotrans',j_o(iob)%i(0)
		write(6,'(30i5/)')(j_o(iob)%i(j),j=1,j_o(iob)%i(0)+1)
		write(6,'(30i5/)')(j,j=1,j_o(iob)%i(0)+1)
		if(p4.or..true.)then
			do j=1,j_nfunctions_
				write(6,*)j,j_functions(j),gotos(j),j_minarg_(j),j_maxarg_(j)
			enddo !j=1,j_nfunctions_    971
		endif !if(p4.or..true.)    970
	endif !if(p)    966
 
	goto 2
 
	!	1 if(j_nopt.gt.0)!!call j_clearoption(iob,io)
 
	! 1	if(j_nopt.gt.0)write(17,*)'isnnopt',j_isnopt,j_nopt, 'io cur ',io,j_options(j_optionmoptio(1,1))
	! write(17,*)'io,coming func',io,j_functions(j_o(iob)%i(io))
1		if(j_isnopt)call j_clearoption(iob,io)
!	if(j_isnopt.or.j_nopt.gt.0)write(6,*)'clearoptaft',j_isnopt,j_nopt
	io=io+j_o(iob)%i(io+1)+3
 
	if(p)write(6,*)'newio ',io, j_o(iob)%i(io:io+6)
	!write(17,*)' new j_nopt ',j_nopt
	!if(j_nopt.gt.0)write(17,*)j_options(j_optionmoptio(1,1)),'optioio',j_optionmoptio(2,1)
	!	if(j_o(iob)%i(io).gt.0.and.j_o(iob)%i(io).le.j_nfunctions)write(17,*)'next ',j_functions(j_o(iob)%i(io))
 
 
 
	! 2000 io=io+4
 
	2 continue ! if io is updated in nonnormal way

	if(j_err)then
 
		!	write(6,*)'dotrans caaliing debugerr'
		call j_debugerr(iob,io)
		!	write(6,*)'dotrans AFT caaliing debugerr,err',j_err
 
		!	j_err=.false.
		!	call j_pause('<err>',do=.true. )
		!	if(.not.j_err)goto 12345
		j_recursion=j_recursion-1
		!	write(6,*)'j967 recursion ',j_recursion
		j_v(j_ivrecursion) = j_recursion !j_v(j_ivrecursion) - 1
		if(.not.j_errprint)write(6,*)'recursion level set to ',j_recursion !j_v(j_ivrecursion)
		!if(j_recursion.gt.j_1)write(6,*)' '
		!	write(6,*)'return from dotrans'
		return
		!		return
	endif !if(j_err)    998
 
	!	write(16,*)'kou',iob,io
	if(j_o(iob)%i(io).lt.0)then
 
		j_curline(j_recursion)=j_curline(j_recursion)-j_o(iob)%i(io)
		if(j_v(j_ivdebugtrans).ne.j_0)call j_debug(iob)
		io=io+1
	endif !if(j_o(iob)%i(io).lt.0)   1018
	! if(j_nopt.gt.0)then
	! write(6,*)j_nopt,' open options'
	! write(6,*)'io ',io, 'len ',j_o(iob)%i(0)
	! write(6,'(25i5)')j_o(iob)%i(1:min(j_o(iob)%i(0),100))
	! !	write(6,'(25i5)')(j,j=1,max(j_o(iob)%i(0),100))
	! call j_printoptions()
	! j_err=.true.
	! goto 2
	!	endif !if(j_nopt.gt.0)then
	!	io=io+1
	!	if(j_o(iob)%i(io).lt.0)	write(6,*)'iotbefas ',io,j_o(iob)%i(io-1:io+6)
	!	endif !if(j_o(iob)%i(io).lt.0)then
 
	!	write(6,*)'<776',j_v(j_ivdollar),j_oname(1:j_loname),'io ',io, 'in io ',j_o(iob)%i(io)
	!	write(6,'(30i5/)')j_o(iob)%i(0:30)
	if(j_v(j_ivdollar2).eq.179.d0)then
		call j_getname(iob)
		write(6,*)'<776',j_v(j_ivdollar),j_oname(1:j_loname),'io ',io, 'in io ',j_o(iob)%i(io)
		write(6,'(30i5/)')j_o(iob)%i(0:30)
		!	write(6,*)'io ',io
		if(j_o(iob)%i(io).ne.0)then
			iio=j_o(iob)%i(io)
			na=j_o(iob)%i(io+1)
			!	write(6,*)'io ',io,' j_nopt ',j_nopt,j_functions(iio),iio,'teku:',j_o(iob)%i(io:io+na+2)
		else !if(j_o(iob)%i(io).ne.0)then
			write(6,*)io,'finito'
		endif !if(j_o(iob)%i(io).ne.0)   1044
	endif !if(j_v(j_ivdollar2).eq.179.d0)   1039
	!	write(6,*)'io,',io,j_o(iob)%i(io:io+10)
	!	write(6,*)'io,',io,j_o(iob)%i(io)
	! write(6,*)'nollataa'
	! if(j_nopt.gt.0)j_linkoption(iob,io,j_curropt(1:j_nopt))=0
	! j_nopt=0
	!3001 continue
	!	write(6,*)'err before',j_err
 
12345	continue
	! do ii=1,1,j_nfunctions_
		! write(6,*)ii,j_functions(ii),gotos(ii)
	! enddo !ii=1,1,j_nfunctions_   1049
	! stop
	if(p)then
		write(6,*)'io',io,j_o(iob)%i(io)
		if(j_o(iob)%i(io).gt.0)write(6,*)j_functions(j_o(iob)%i(io)),gotos(j_o(iob)%i(io))
	endif !if(p)   1065
 
	goto (11,12,13,14,15,16,17,18,19, & !special j_nfspec=8
		21,22,23,24,25,26, & !Objects j_nfobj=3
		31,32,33,34,35, & !transformations j_nftrans=3
		41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,&  !Loops and control j_nfloop=16
		61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,& ! operations  j_nfoper=17
		81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100, & !arithmetic functions
		101,102,103,104,105,106,107,108,109,110,111,112,113, 114, &  ! arith continues j_nfarit=34
		121,122,123,124,125, & ! special artith j_nfarit2=5
		131,132,133,134,135,   &  ! probability distributions
		141,142,143,144,145,146,147, & ! random numbers
		151,152,153, &  !           interpolation
		161,162,163,164,165,166,167,168,169, 170,1701, & !list functions
		171,172,173, &                   ! text object
		181,182,183,184,185,186,187,188, &  ! file handling
		191,192,193,194,195,196,197, &   ! io
		201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,2201, 2202,2203,&  ! matrices 19
		221,222,223,224,225,226,227,228,229,230,2301,2302,2303, 2304,& !data functions
		231,232,233,234,235,236,237,238,239,240,241,242,243, & ! statistical functions
		251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272, & ! linear programming
		281,282,283,284,285,286, &    ! simulator
		291,292,293,294,295,296,297,298,& !    Figures
		301,302,303,304,305,306,307,308,309, &  !   splines , stem
		311,312,313,314,315,316,317, & ! bit functions
		321,322,323,324,325,326,327,328,329,330,331)   j_o(iob)%i(io)
 
	j_recursion=j_recursion-1
 
	j_v(j_ivrecursion) = j_recursion
	!	write(6,*)'j1053 <recu>',j_v(j_ivrecursion)
	return
 
	!! The following 7 functions are obtained from j_interpret in nonstandard way
 
11	call setoption(iob,io)  ! %%option
	goto 2     !setoption updates io
12	call getelem(iob,io)  !get element or submatrix from matrix or list

	goto 1
 
13	call getelem(iob,io) !put element or submatrix to matrix or list this is actually setelem
	goto 1
 
14	call list2(iob,io) ! NOTHING now make list containg both named objects and numeric constants
	goto 1
 
15	io=io+1; call o1_funcs(iob,io);goto 1
16	io=io+1; call o2_funcs(iob,io);goto 1
17	io=io+1; call o3_funcs(iob,io);goto 1

18	call setcodeopt(iob,io)
	goto 2
	19 call getelem(iob,io)  !setpointer
	goto 1
 
	!! Objects
 
21	call type(iob,io)  !type of object
	goto 1
 
22	call delete_o(iob,io)   !delete object
	goto 1
 
23	call exist_o(iob,io)
	goto 1
 
24	call name(iob,io)
	goto 1
25  call protect(iob,io)
	goto 1
26  call unprotect(iob,io)
	goto 1
 
	!!Transformation objects
 
31	call trans(iob,io)
	goto 1
 
32	call call(iob,io)
	goto 1
 
33	call pause(iob,io)
	goto 1
34	call noptions(iob,io)
	goto 1
 
35	call R(iob,io)
	goto 1
 
 
	!! Loops and control structures, io is updated within functions except in assign
 
41	call do(iob,io) ! enddo, exitdo and cycledo are implemented using goto
	goto 2
 
 
42	call if(iob,io)  !! while, if( )then and  elseif()then are iplemented using if()
	goto 2			!! else is implemented using goto
 
	!! Note j_interpret implements exitdo and cycledo using goto
 
43	call assign(iob,io)  ! Explicit assignment of output
	goto 2
 
44	call bincode(iob,io)   !sit(iob,io)	! ! Go to o prompt
	goto 1
 
45	call which(iob,io)   !! Get value based on condition
	goto 1
 
 
46	call errexit(iob,io)  !! Get j_err return to sit>
	goto 2
 
47	call goto(iob,io)    !! exitdo cycledo exitwhile cyclewhile and else are implement using goto
	goto 2				!!jump and bck are also implemented using goto
 
48	write(6,*) 'itrace not available'  !call itrace(iob,io)
	stop
 
49	write(6,*) 'trace not available'
	stop
 
50	write(6,*)'tracenow not available'
	stop
	!stop call tracenow(iob,io)
	!goto 1
 
51	write(6,*)'itraceoff not available' !call itraceoff(iob,io)
	stop
	!goto 1
 
52	write(6,*)'traceoff not available' !call itraceoff(iob,io)
	stop
	!call traceoff(iob,io)
	!	goto 1
 
53	write(6,*)'tracetest not available' !call itraceoff(iob,io)call tracetest(iob,io)
	stop
 
54 	call assone(iob,io)
	!write(6,*)'<55err',j_err
	! io updated in assone
	goto 1
 
55	call enddo(iob,io)
	goto 2
 
56	call assign(iob,io) !assmany
	goto 2
 
57  call goto2(iob,io)
	goto 2
 
58  call goto3(iob,io)
	goto 2
 
 
	!! Arithmetic and logical operations
	!!**********************************
61	call BINOP(iob,io,14) ! HMULT(iob,io) elementwise producr
	goto 1
62 	call BINOP(iob,io,15) ! HDIV(iob,io) elementwise division
	goto 1
 
63 call POWER(iob,io,1)  !IPOWER(iob,io)
	goto 1
 
64 call MULT(iob,io)
	goto 1
 
65	call DIV(iob,io)
	goto 1
 
66	call BINOP(iob,io,1)  !sum
	goto 1
 
67	call MINUS(iob,io)
	goto 1
	! ioper 1=plus 2=minus 3=eq  4=ne 5 =le 6=lt 7 =ge 8 =gt 10 =and 11=or 12=eqv 13=neqv 14=hmult 17 hdiv
68	call BINOP(iob,io,3)  !EQ(iob,io)
	goto 1
 
69	call BINOP(iob,io,4) ! NE(iob,io)
	goto 1
 
70	call BINOP(iob,io,5) ! LE(iob,io)
	goto 1
 
71	call BINOP(iob,io,6) ! LT(iob,io)
	goto 1
 
72	call BINOP(iob,io,7) ! GE(iob,io)
	goto 1
 
73	call BINOP(iob,io,8) ! GT(iob,io)
	goto 1
 
74	call NOT(iob,io)
	goto 1
 
75	call BINOP(iob,io,10) ! AND(iob,io)
	goto 1
 
76	call BINOP(iob,io,11) ! OR(iob,io)
	goto 1
 
77	call BINOP(iob,io,12) ! EQV(iob,io)
	goto 1
 
78	call BINOP(iob,io,13) ! NEQV(iob,io)
	goto 1
 
79	call POWER(iob,io,2)
	goto 1
 
	!! Arithemetic functions
	!***********************
81	call BINOP(iob,io,16)  !call min_(iob,io)
	goto 1
 
82	call BINOP(iob,io,17)  !call max_(iob,io)
	goto 1
 
83	call BINOP(iob,io,18)  !sign
	goto 1
 
84	call BINOP(iob,io,19)   !mod
	goto 1
 
85	call UNIOP(iob,io,1) !nint_(
	goto 1
 
86	call UNIOP(iob,io,2) !int_
	goto 1
 
87	call UNIOP(iob,io,3) !ceiling_
	goto 1
 
88	call UNIOP(iob,io,4) !floor_
	goto 1
 
89	call UNIOP(iob,io,5) !sqrt_(
	goto 1
 
90	call UNIOP(iob,io,6) ! sqrt2 sqrt2(-4)=-2
	goto 1
 
91	call UNIOP(iob,io,7)  !base e log_
	goto 1
 
92	call UNIOP(iob,io,8) ! log10_
	goto 1
 
93	call UNIOP(iob,io,9) !exp_
	goto 1
 
94	call UNIOP(iob,io,10) !sin_(
	goto 1
 
95	call UNIOP(iob,io,11) !sind_
	goto 1
 
96	call UNIOP(iob,io,12) !cos_
	goto 1
 
97	call UNIOP(iob,io,13) !cosd_
	goto 1
 
98	call UNIOP(iob,io,14) !tan_
	goto 1
 
99	call UNIOP(iob,io,15) !tand_
	goto 1
 
100	call UNIOP(iob,io,16) !cotan_
	goto 1
 
101	call UNIOP(iob,io,17) !cotand_
	goto 1
 
102	call UNIOP(iob,io,18) !asin_
	goto 1
 
103	call UNIOP(iob,io,19) !asind_(
	goto 1
 
104	call UNIOP(iob,io,20) !acos_
	goto 1
 
105	call UNIOP(iob,io,21) ! acosd_
	goto 1
 
106	call UNIOP(iob,io,22) ! atan_
	goto 1
 
107	call UNIOP(iob,io,23) !atand_
	goto 1
 
108	call UNIOP(iob,io,24) !acotan_
	goto 1
 
109	call UNIOP(iob,io,25) !acotand_
	goto 1
 
110	call UNIOP(iob,io,26) !sinh_
	goto 1
 
 
111	call UNIOP(iob,io,27) !cosh_
	goto 1
 
112	call UNIOP(iob,io,28) !tanh_
	goto 1
 
113	call	UNIOP(iob,io,29) ! abs
	goto 1
 
114	call UNIOP(iob,io,30)  !fraction
	goto 1
 
	! Special arithemetic functions
 
121	call der(iob,io)  ! derivatives
	goto 1
 
122	call gamma_(iob,io)
	goto 1
 
123	call loggamma(iob,io)
	goto 1
 
124	call logistic(iob,io) ! 1/(1+exp(-x)) for numerical reasons, logistic function should be used
	goto 1
 
125	call npv(iob,io)
	goto 1
 
	! Probability distributions
 
131	call pdf(iob,io)
	goto 1
 
132	call cdf(iob,io)
	goto 1
 
133	call bin(iob,io)
	goto 1
 
134	call negbin(iob,io)
	goto 1
 
135 call density(iob,io)
	goto 1
	!Random numbers
 
141	call ran(iob,io)  !uniform
	goto 1
 
142	call rann(iob,io)  ! normal

	goto 1
 
143	call ranpoi(iob,io)  ! Poisson
	goto 1
 
144	call ranbin(iob,io)  ! Binomial
	goto 1
 
145	call rannegbin(iob,io)
	goto 1
 
146	call select(iob,io)
	goto 1
 
147	call random(iob,io)
	goto 1
 
 
	!! Interpolation
 
151	call interpolate(iob,io)
	goto 1
 
152	call plane(iob,io)
	goto 1
 
153	call bilin(iob,io)
	goto 1
 
 
 
	!!! List functions
 
161 call list(iob,io)
	goto 1
 
162	call merge(iob,io)
	goto 1
 
163	call difference(iob,io)
	goto 1
 
164	call index(iob,io)
	goto 1
 
165	call index_o(iob,io)
	goto 1
 
166	call len(iob,io)
	goto 1
 
167	call ilist(iob,io)

	goto 1
 
168	call putlist(iob,io)
	goto 1
169	call table(iob,io)
	!write(6,*)'afttab',io,j_o(iob)%i(1:20)

	goto 1
	170 call fromlist(iob,io)
	goto 1
	1701 call list2sym(iob,io)
	goto 1
	!! Creating a text object
 
171	call text(iob,io)
	goto 1
172	call txt(iob,io)
	goto 1
173 call char_(iob,io)  !char(
	goto 1
	!!File handling
 
181	call exist_f(iob,io)
	goto 1
 
182	call delete_f(iob,io)
	goto 1
 
183	call close_(iob,io)
	goto 1
 
184	call showdir(iob,io)
	goto 1
 
185	call setdir(iob,io)
	goto 1
 
186	call thisfile(iob,io)
	goto 1
 
187 call filestat(iob,io)
	goto 1
188	call print_f(iob,io)
	goto 1
 
	!! io
 
191	call read(iob,io)
	goto 1
 
192	call write(iob,io)
	goto 1
 
193	call print(iob,io)

	goto 1
 
194	call ask(iob,io)
	goto 1
 
195	call askc(iob,io)
	goto 1
 
196	call printresult(iob,io,1)
	goto 1
197 call printresult(iob,io,2)
	goto 1
	!! Matrices
 
201	call matrix(iob,io)  !generate matrix
	goto 1
 
202	call nrows(iob,io)
	goto 1
 
203	call ncols(iob,io)
	goto 1
 
204	call t(iob,io)  !transpose
	goto 1
 
205	call inverse(iob,io)
	goto 1
 
206	call solve(iob,io)  !solves A*x=b  faster and more accurate than x=inverse(A)*b
	goto 1
 
207	call qr(iob,io)  !qr decomposition
	goto 1
 
208	call eigen(iob,io)
	goto 1
 
209	call sort(iob,io)
	goto 1
 
210	call envelope(iob,io)
	goto 1
 
211	call find(iob,io)
	!	write(6,*)'from find io ',io,' new io ',io+j_o(iob)%i(io+1)+3, ' inio ',&
	!	j_o(iob)%i(io+j_o(iob)%i(io+1)+3)
	goto 1
 
212	call matrixstat(iob,io,1)  !mean
	goto 1
 
213	 call matrixstat(iob,io,2)  !sum(iob,io)


	goto 1
 
214	call matrixstat(iob,io,3) !call var(iob,io)
	goto 1
 
215	call matrixstat(iob,io,4) !call sd(iob,io)
	goto 1
 
216 call minloc_(iob,io)  !call minloc(
	goto 1
 
217 call maxloc_(iob,io)  !call maxloc(
	goto 1
 
218	call cumsum(iob,io)
	goto 1
219	call corrmatrix(iob,io)
	goto 1
220 call matrixborder(iob,io)
	goto 1
2201 call fill(iob,io)

	goto 1
2202 call vector(iob,io)
	goto 1
2203 call flip(iob,io)
	goto 1
 
 
221	call data(iob,io)

	goto 1
 
222	call newdata(iob,io)
	goto 1
 
223	call exceldata(iob,io)
	goto 1
 
224	call linkdata(iob,io)
	goto 1
 
225	call getobs(iob,io)
	goto 1
 
226	call nobs(iob,io)  ! If dat is a DATA object, nobs(dat)==nrows(dat%matrix)
	goto 1
 
227	call classvector(iob,io)
	goto 1
 
228	call values(iob,io)
	goto 1
 
229	call transdata(iob,io)
	goto 1
230  call datawcase(iob,io)
	goto 1
 
2301 call joindata(iob,io)
	goto 1
 
2302 call splitdata(iob,io)
	goto 1
2303 call partdata(iob,io)
	goto 1
	2304 call find_d(iob,io)
	goto 1
 
	!! Statistical functions
 
231	call stat_(iob,io) ! basic satistics call stat(
	goto 1
 
232	call corr(iob,io,1) !cov(
	goto 1
 
233	call corr(iob,io,0)  !correlation
	goto 1
 
234	call regr(iob,io)  !linear regression
	goto 1
 
	! Inquiry functions for lin. regression
 
235		call regpar(iob,io,2)  !call mse(iob,io)
	goto 1
 
236		call regpar(iob,io,1)  !call rmse(iob,io)
	goto 1
 
237		call coefse(iob,io,0)   !call coef(iob,io) !ity=0 coef, ity=1 se
	goto 1
 
238		 call regpar(iob,io,3)  !call r2(iob,io)
	goto 1
 
239		call coefse(iob,io,1)   !se of param
	goto 1
240	call nonlin(iob,io) !nonlinear regression
	goto 1
 
241	call varcomp(iob,io)  !variance and covariance components
	goto 1
 
242	call classify(iob,io) !groupå satistics
	goto 1
 
243	call class(iob,io)
	goto 1
 
 
	!! Linear programming
 
251	call problem(iob,io)
	goto 1
 
252	call jlp(iob,io)
	goto 1
 
253	call weights(iob,io)
	goto 1
 
254	call jlpunit(iob,io)
	goto 1
 
255	call schedcum(iob,io)
	goto 1
 
256	call schedw(iob,io)
	goto 1
 
257	call weight(iob,io)
	goto 1
 
258	call partweights(iob,io)
	goto 1
 
259	call partunit(iob,io)
	goto 1
 
260	call partschedcum(iob,io)
	goto 1
 
261	call partschedw(iob,io)
	goto 1
 
262	call partweight(iob,io)
	goto 1
 
263	call priceunit(iob,io)  ! price%unit
	goto 1
 
264	call weightschedcum(iob,io)
	goto 1
 
265	call priceschedcum(iob,io)
	goto 1
 
266	call priceschedw(iob,io)
	goto 1
 
267	call weightschedw(iob,io)
	goto 1
 
268	call integerschedw(iob,io)
	goto 1
 
269	call piecewise(iob,io)
	goto 1
 
270 call jlpz(iob,io)
	goto 1
 
271 call jlpcoef(iob,io)
	goto 1
272 call neigtable(iob,io)
	goto 1
 
	!! Simulator
 
281	call simulator(iob,io)
	goto 1
 
282	call next(iob,io)
	goto 1
 
283	call branch(iob,io)
	goto 1
 
284	call simulate(iob,io)
	goto 1
 
285	call cut(iob,io)
	goto 1
 
286	call loadtrees(iob,io)
	goto 1
 
	!! Plotting figures
 
291	call plotyx(iob,io)  ! scatter plot
	! if(j_err)goto 1
	! if(j_gpshow)call j_showfig(ivfig)
	goto 9753  !check pause
 
292	call draw(iob,io)
	! if(j_err)goto 1
	! if(j_gpshow)call j_showfig(ivfig)
	!if(p)write(6,*)'<555ioafterdraw ',io
	goto 9753
 
293	call drawclass(iob,io)  ! plot results obtained with classify
!	if(j_err)goto 1
!	if(j_gpshow)call j_showfig(ivfig)
	goto 9753
 
294	call drawline(iob,io)
	!if(j_err)goto 1
!	if(j_gpshow)call j_showfig(ivfig) !so drawline must not be recursive

	goto 9753
 
295	call show(iob,io)
	goto 9753 !check pause
 
 
296	call plot3d(iob,io)
	goto 9753
	297 call gnuplot(iob,io)
	goto 9753
	298 call tics(iob,io)
	goto 1
 
 
	!! Splines, stem splines,  and volume functions
 
301	call tautspline(iob,io)
	goto 1
 
302	call stemspline(iob,io)
	goto 1
 
303	call stempolar(iob,io)
	goto 1
 
304	call laasvol(iob,io)
	goto 1
 
305	call laaspoly(iob,io)
	goto 1
 
306	call integrate(iob,io) !integrating stem spline
	goto 1
 
307	call stemcurve(iob,io) !integrating stem spline
	goto 1
 
308	call stemopt(iob,io) !
	goto 1
 
	309	call cumvol(iob,io) !
	goto 1
 
	!! Bit function
 
311	call setbits(iob,io)
	goto 1
 
312	call clearbits(iob,io)
	goto 1
 
313	call getbit(iob,io)
	goto 1
 
314	call getbitch(iob,io)
	goto 1
 
315	call bitmatrix(iob,io)
	goto 1
 
316	call setvalue(iob,io)
	goto 1
 
317	call closures(iob,io)
	goto 1
 
 
	!! Misc. utility functions
 
321	call value_(iob,io) !extract information from object
	goto 1
 
322	call properties(iob,io)
	goto 1
 
323	call cpu(iob,io)
	goto 1
 
324	call secnds_(iob,io)
	goto 1
325 call where(iob,io)
	goto 1
326	call batch(iob,io)
	goto 1
327 call systemj(iob,io)
	goto 1
328 call info(iob,io)
	goto 1
329	call namenum(iob,io,1)

	goto 1
330 call namenum(iob,io,2)
	goto 1
331 call macro(iob,io)
	goto 1
	!!! o function
9753 if(j_ninc.gt.1.and..not.j_gpcontinue.and.j_gpwasshown)then
		call j_getname(j_ivfig)
		call j_pause(j_oname(1:j_loname)//'>',do=.true.)
	endif !9753 if(j_ninc.gt.1.and..not.j_gpcontinue.and.j_gpwasshown   1910
	goto 1
 
 
 
end subroutine !call do(iob,io)
 
subroutine setcodeopt(iob,io)
	!	write(6,*)'setcode**','io ',io,j_o(iob)%i(io:io+10)
	j_nopt2=j_nopt2+1
	iopt=j_o(iob)%i(io+1)
	!j_linkopt2(j_o(iob)%i(io+1) )=io+4  !where to start to compute
	!j_nopt2=j_nopt2+1
	if(j_nopt2.gt.j_maxopenopt2)then
		write(6,*)'*j* too many open codeoptions ',j_nopt2
		j_err=.true.;return
 
	endif !if(j_nopt2.gt.j_maxopenopt2)   1926
	j_optionmoptio2(1,j_nopt2)=iopt
	j_optionmoptio2(2,j_nopt2)=j_o(iob)%i(io+4)
	j_optionlink2(j_nopt2)=io+5
	! write(17,*)'<34,j-nopt2,j_optionmoptio2 ',j_nopt2,j_optionmoptio2(1,j_nopt2),j_optionmoptio2(2,j_nopt2)
	! write(17,*)'code,iob,io,iopt,io, ',iob,io,iopt,j_options(iopt),j_o(iob)%i(io+4), 'link ',j_o(iob)%i(io+2)
	! write(17,*)'recur',j_recursion
 
	if(j_o(iob)%i(io+2).eq.0)then    !nothing to be computed
		io=io+5  !in this case j_o(iob)(j_linkopt2(option)-2).eq.0
	else !if(j_o(iob)%i(io+2).eq.0)then
		io=j_o(iob)%i(io+2)
	endif !if(j_o(iob)%i(io+2).eq.0)   1938
	j_optioniob2(j_nopt2)=iob
 
	!	write(6,*)'<38834',io
 
	!
	!setcode,option,jumptonow,output      io of func                                                      ioptjump,ioptjum
	!         +1      +2         +3      +4
	!                                  linkopt2
	! if linktowhencompuyting is zero dont jump anywhere
 
end subroutine !subroutine setcodeopt(iob,io)
! subroutine setpointer(iob,io)
! call j_startfunction(iob,io,0,narg,j_arg,ivout);if(j_err)return
! call j_getname(ivout)
! write(6,*)j_oname(1:j_loname)
! write(6,*)narg,j_arg
! return
 
! end subroutine
 
subroutine setoption(iob,io)
	integer,intent(in)::iob
	integer,intent(inout)::io
	iopt=j_o(iob)%i(io+1)
	narg=j_o(iob)%i(io+2)  !
	!	iout=j_o(iob)%i(io+2+narg)
	!
	if(j_nopt.ge.j_maxopenopt.or.(j_v(j_ivdollar).eq.455.D0.and.j_nopt.gt.0))then
		write(6,*)'there are too many open options ',j_nopt
		call j_printoptions()
		j_err=.true.
		return
	endif !if(j_nopt.ge.j_maxopenopt.or.(j_v(j_ivdollar).eq.455.D0.an   1970
	!	noptv=j_nopt
	j_nopt=j_nopt+1
	j_isnopt=.true.
	j_optionmoptio(1,j_nopt)=iopt !option
	j_optionmoptio(2,j_nopt)=j_o(iob)%i(io+3+narg) ! io of the function
 
	! if(noptv.gt.0)then
	! if(any(j_optiontot(1:noptv).eq.j_optiontot(j_nopt)))then
	! j_nopt=j_nopt-1
	! return
	! endif !if(any(j_optiontot(1:noptv).eq.j_optiontot(j_nopt)))   1948
	! endif !if(noptv.gt.0)   1947
	j_optionlink(j_nopt)=io+2  !link to narg
	j_optioniob(j_nopt)=iob
	!	write(6,*)'setopt',j_nopt,iopt,j_optionmoptio(2,j_nopt),'iob,ionow',iob,io
	! write(17,*)'set ',j_options(iopt)
	! write(17,*)j_nopt,iopt,j_optionmoptio(2,j_nopt),j_optionlink(j_nopt),j_optioniob(j_nopt)
	! write(17,*)'iob,io',iob,io,'recu',j_recursion
	!	write(6,*)'set iob,io, iopt ',iob,io,iopt,' narg',j_o(iob)%i(io+2)
	!	call j_getname(iob)
	!	write(6,*)'iob=',j_oname(1:j_loname)
	!write(6,*)
	!	write(6,*)'****setting option in ',iob, 'in io ',io,' option ',iopt,j_options(iopt),&
	!	' option io', j_optionmoptio(2,j_nopt),' optfunc ',&
	!j_functions(j_o(iob)%i( j_optionmoptio(2,j_nopt)  )),	' option iob ',iob,' link ',io+2
	! if(j_o(iob)%i( j_optionmoptio(2,j_nopt)).eq.1)then !function is setoption
	! !	write(6,*)'KUKULUGUU, io'
 
	! lkm=1
 
	! do i=io,1,-1
	! if(j_o(iob)%i(i).lt.0)then
	! lkm=lkm+1
	! if(lkm.eq.3)ial=i
	! endif
	! enddo
	! do lo=io,j_o(iob)%i(0)
	! if(j_o(iob)%i(lo).lt.0)exit
	! enddo
	! write(6,*)'open optio io ',io,' is in line ', lkm,' second line here'
	! write(6,'(20i5/)')j_o(iob)%i(ial:lo)
	! write(6,'(20i5/)')(j,j=ial,lo)
	! j_err=.true.
	! return
 
	! endif
	!	if(j_v(j_ivdollar).eq.456.D0)	write(6,*)'j_nopt,iob,io,iopt',j_nopt,iob,io,iopt,'iofunc',&
	!		j_optionmoptio(2,j_nopt),'link',j_optionlink(j_nopt)
	io=io+narg+4  !one more than usually
	return
end subroutine setoption !subroutine setoption(iob,io)
!	tr(1,-4,3,-9)=
! re=regr(y,x1,x2,noint->,step->2.5)
! r=re()
 
recursive subroutine getelem(iob,io)  ! matrix(iel) e.g. out=a(irow,icol)  %%matrix
	!	integer, dimension(10):: argv !arguments of the function
 
	! Section getelem Get or set a matrix element or submatrices
	! Matrix elements or submatrices can be accessed using the same syntax as
	! accesing Jlp22 functions.
 
	! One can get or set matrix elements and submatrices as follows. If the expression
	! is on the right side of '=' then Jlp22 gets a REAL value or submatrix, if the expression
	! is on the left side of '=', the Jlp22 sets new values for a matrix element or a submatrix.
	! In the following formulas ]C[ is a column vector, ]R[ a row vector, and ]M[ is
	! a general matrix with m rows and n columns.
	! If ]C[ is actually REAL it can
	! be used as if it would 1 x 1 MATRIX. This can be useful when working with
	! matrices whose dimensions can vary starting from 1 x 1. Symbol ]r[ refers to
	! row index, ]r1[ to first row in a row range, ]r2[ to the last row. The rows and
	! columns can be specifiel using ILIST objects ]il1[ and ]il2[ to specify noncontiguous ranges.
	! ILIST can be
	! specified using ilist() function or using {} construction.
	! Similarly columns are indicated  with ]c[. It is always legal to refer to
	! vectors by using the ]M[ formulation and giving ]c[ with value 1 for column vectors and
	!  and ]r[ with value 1 for row vectors.
	!  endheader
	!Option
	!Args&0-4&REAL | ILIST&row and column range as explained below.
	!diag&N|& &Get or set diagonal elements
	!sum&N|0|1& & When setting elements, the right side is added to the current elements. If the
	!sum-> option has argument, the right side is multiplied with the argument when adding to the curretn elements.
	!endoption
 
	!Latex
	! Row and column ranges can be specified as follows. Both rows and columns can be given
	! in similar way. Assume that submatrix is specified with  ]M[(r,c). Then r can
	! be the following constructions. Columns can be defined similarly:
	! \begin{itemize}
	! \item  r1 : a variable or constant for single row.
	! \item r1,-r2 : a range of rows
	! \item r1,Ninf : Rows from row r1 to the last row
	! \item All : All rows
	! \item ili : Rows sepecfied with a ILIST object ili defined earlier
	! \item {r1,...,rn} : The ILIST object is defined here
	! \item mat : Where mat is a matrix whose all elements (in row order)are made first integers.
	! Nonpositive values are ignored.
	! \end{itemize}
	!endlatex
 
	!\Latex
	! If option diag-> is present then
	! \begin{itemize}
	! \item  ]M[(diag->) Get or set the diagonal. If ]M[ is not square matrix, and error
	!occurs.
	!\item  ]M[(r1,-r2,diag->) (Again -r2 can be ]Tolast[.
	!\end{itemize}
	!endlatex
	! Note
	! When setting values to a submatrix the the the values given in input matrix
	! are put into the outputmatrix in row order, and the shape of the input and output matrices need
	! not be the same. An error occurs only if input and output contain different number of
	! elements.
	! endnote
	! Ex getset Get or set submatrices
	! A=matrix(3,4,do->);
	! B=A(1,-2,3,-4);
	! A(1,-2,3,-4)=B+3;
	! A(1,-2,3,-4,sum->)=-5;
	! A(1,-2,3,-4,sum->2)=A(2,-3,1,-2);
	! C=A({1,3},{4...2});
	! H=matrix(4,4,diag->,do->3);
	! H(3,-4,diag->)=matrix(2,values->(4,7));
	!row=matrix(2,values->(1,3))
	!col=matrix(3,values->(4,3.5,2))
	!C=A(row,col);
	!row(2)=0
	!c=A(row,col);
 
 
	! endex
	! Note When giving range, the lower and upper limit can be equal.
	! endnote
	! Note It was earlier not possible to mix ILIST range and contiguous range. Now they can be mixed.
	!endnote
	! Note Giving rows or columsn with matrices is introduced in version October 1, 2023.
	!endnote
	! endsection
 
	implicit none   !MUST if there are internal subroutines
	integer,intent(in)::iob,io
	!	integer, dimension(:), pointer :: arg !arguments of the funct
	integer, dimension(:), pointer :: ilist1,ilist2 !a
	integer,dimension(4)::irange
	!integer ::nrang
 
	logical diag,simple
 
	logical::p=.false.
	double precision::coef
 
 
	logical :: get,set,issum,iscoef,issumcoef
	integer ::imat,iout,narg1,narg
	integer ::iel,j,iro,ico,neli,j2
	integer ::ninlist,ninlist1,ninlist2
	integer*8 ::ncol,nrow,nel,nrow2,ncol2,nel2,nelout,ibas,ibasin,nelmat,nrowout,ncolout
	integer::ivilist1,ivilist2
	logical :: isreal,islistout,isrealout,ismatrixout,isilist1,isilist2,ismatrix1,ismatrix2,isilist
	logical::ismati
	logical ::nrang1
	!getmatrix
	double precision,dimension(:),allocatable,target ::tem
	double precision,dimension(:),pointer ::d
	logical ::point
	integer,dimension(4)::arg  !arguments
 
	p=j_v(j_ivdollar2).eq.56.d0
 
 
	! if(p)write(6,*)' voimassaolevat optiot'
	! if(p)call j_printoptions()
	get=j_o(iob)%i(io).eq.2    ! if not get then set
	set=.not.get
	call sumcoef()
	if(j_err)return
	!write(6,*)'issum',issum
	!	write(6,*)'issum ',issum
	point=j_o(iob)%i(io).gt.3
 
	!	write(6,*)'set',set,j_o(iob)%i(io:io+10)
	!write(6,*)'gete',j_o(iob)%i(io;io+10),set
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	islistout=j_otype(iout).eq.j_iplist
 
	if(set)then
 
		isrealout=j_otype(iout).eq.j_ipreal
		ismatrixout=j_otype(iout).eq.j_ipmatrix
 
		if(ismatrixout)then
			nelout=j_nelem(iout)
			nrowout=j_nrows(iout)
			ncolout=j_ncols(iout)
			if(j_err)return
		endif !if(ismatrixout)   2168
 
	endif !if(set)   2163
	narg1=narg-1  !number of index arguments
	if(narg1.gt.0)arg(1:narg1)=j_o(iob)%i(io+3:io+1+narg)
	imat=j_o(iob)%i(io+2)  !imat is first
	if(imat.eq.j_ivoutlist)then
		if(j_otype(iout).eq.j_ipreal)then
			do j=1,narg1
				if(j_otype(arg(j)).ne.j_ipreal)call j_del(arg(j))
				j_v(arg(j))=j_v(iout)
			enddo !j=1,narg1   2181
		elseif(j_otype(iout).eq.j_ipmatrix)then
			do j=1,min(narg1,j_o(iout)%i(3))
				if(j_otype(arg(j)).ne.j_ipreal)call j_del(arg(j))
				j_v(arg(j))=j_o(iout)%d(j)
			enddo !j=1,min(narg1,j_o(iout)%i(3))   2186
		elseif(j_otype(iout).eq.j_iplist)then
			do j=1,min(narg1,j_o(iout)%i(3))
				iel=j_o(iout)%i2(j)
				if(j_otype(iel).ne.j_ipreal)then
					call j_getname(iout,iel)
					write(6,*)j_oname2(1:j_loname2),' in LIST ',j_oname(1:j_loname),' is not REAL'
					j_err=.true. ;	return
 
				endif !if(j_otype(iel).ne.j_ipreal)   2193
				if(j_otype(arg(j)).ne.j_ipreal)call j_del(arg(j))
				j_v(arg(j))=j_v(iel)
 
			enddo !j=1,min(narg1,j_o(iout)%i(3))   2191
		else
			call j_getname(iout)
			write(6,*)j_oname(1:j_loname),' is not REAL, MATRIX or LIST'
			j_err=.true.;return
		endif !if(j_otype(iout).eq.j_ipreal)   2180
		return
	endif !if(imat.eq.j_ivoutlist)   2179
 
 
 
	select case (j_otype(imat))
 
	case(j_ipmatrix)
 
	ismati=j_o(imat)%i(4).eq.j_mati
	diag=j_isoption(iob,io,j_mdiag)
	nrow=j_nrows(imat)  !j_o(imat)%i(1)
	ncol=j_ncols(imat)  !j_o(imat)%i(2)
	nel=j_nelem(imat) !j_o(imat)%i
 
	call iranges()
	if(p)write(6,*)'irange',irange,'isrealout ',isrealout,'nrang1 ',nrang1
	! if(p)then
	!call j_getname(imat)
	! write(6,*)j_oname(1:j_loname),imat,'*****irnages',irange,nrow2,ncol2,'nrang1',nrang1,'simple',simple
	! write(6,*)'nrow,ncol,nel',nrow,ncol,nel,'er',j_err
	! endif !if(p)   2147
	if(j_err)return
 
	!write(6,*)'oonrang1,set,narg1,simple',nrang1,set,narg1,simple,irange,isilist1,isilist2
	if(simple)then
		iel=nrow
		if(nrang1)iel=nel
		if(irange(1).le.0.or.irange(1).gt.iel)j_err=.true.
		if(.not.nrang1)then
 
			if(irange(3).le.0.or.irange(3).gt.ncol)j_err=.true.
 
		endif !if(.not.nrang1)   2237
		!		if(j_err)write(6,*)'simple,nrang1,iel,ncol,nel,irange',simple,nrang1,iel,ncol,nel,irange,'nrow',nrow
		if(j_err)goto 66
		if(get)then
 
			if(nrang1)then
				call getmat0(j_v(iout),irange(1))
				!					j_v(iout)=j_o(imat)%d(irange(1))
			else
				iel=(irange(1)-1)*ncol+irange(3)
				call getmat0(j_v(iout),iel)
				!				j_v(iout)=j_o(imat)%d((irange(1)-1)*ncol+irange(3))
			endif !if(nrang1)   2246
 
 
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
		else  !set
			if(j_otype(iout).eq.j_ipreal)then
 
				!	write(6,*)issum,'nrang1',nrang1,'irange',irange,'ncol,nrow,ncol2,nrow2',ncol,nrow,ncol2,nrow2
				! j_err=.true.;return
 
				if(nrang1)then
					if(issum)then
						j_o(imat)%d(irange(1))=j_o(imat)%d(irange(1))+j_v(iout)
					else
						j_o(imat)%d(irange(1))=j_v(iout)
					endif !if(issum)   2265
				else
					iel=(irange(1)-1)*ncol+irange(3)
					if(issum)then
						j_o(imat)%d(iel)=j_o(imat)%d(iel)+j_v(iout)
					else
						j_o(imat)%d(iel)=j_v(iout)
					endif !if(issum)   2272
				endif !if(nrang1)   2264
 
			else
				call j_getname(imat,iout)
				write(6,*)'not possible to put into matrix ',j_oname(1:j_loname), ' ',&
					j_otypes( j_otype(iout))(1:j_lotypes(j_otype(iout))),' object ',j_oname2(1:j_loname2)
				j_err=.true.
			endif !if(j_otype(iout).eq.j_ipreal)   2259
 
		endif !if(get)   2244
		return
 
	endif !if(simple)   2233
 
	if(irange(2).lt.irange(1).or.irange(1).le.0)j_err=.true.
 
	if(.not.nrang1)then
 
		if(irange(4).lt.irange(3).or.irange(4).gt.ncol.or.irange(3).le.0)j_err=.true.
 
	endif !if(.not.nrang1)   2293
 
66	if(j_err)then
		call j_getname(imat)
		j=4
		if(nrang1)j=2
		write(6,*)'illegal ranges ',irange(1:j), ' for ',j_oname(1:j_loname),' with dimensions ',int(nrow),int(ncol)
		!	write(6,*)'nrang1,set,narg1,simple',nrang1,set,narg1,simple
		j_err=.true.;return
	endif !66	if(j_err)   2299
	if(get)then
		call getmatrix()
	else
 
		!	if(p)write(6,*)'set,diag',diag,'outmat ',j_otype(iout).eq.j_ipmatrix,&
		!	iscoef,issumcoef,issum,'ismaout',ismatrixout,nrang1,iscoef,issumcoef
		if(diag)then
			call setdiag()
			return
		endif !if(diag)   2313
		!		write(6,*)'ismatrixout,islistout', ismatrixout,islistout
		if(ismatrixout.or.islistout)then  !j_otype(iout).eq.j_ipmatrix)then
 
			nelout=j_nelem(iout)
			if(islistout)j_o(iout)%d(1:nelout)=j_v( j_o(iout)%i2(1:nelout))
			if(nrang1)then
				nelmat=irange(2)-irange(1)+1
			else
				nelmat=nrow2*ncol2
			endif !if(nrang1)   2322
			if(nelout.ne.nelmat)then
				call j_getname(imat,iout)
				write(6,*)'cannot put into ',j_oname(1:j_loname), ' into box of size ',nelmat,&
					' MATRIX ',j_oname2(1:j_loname2),' with size ',nelout
				j_err=.true.;return
			endif !if(nelout.ne.nelmat)   2327
 
			if(iscoef)then
				call setcoefm()
 
			elseif(issumcoef)then
				call setsumcoef()
			elseif(issum)then
				call setsumm()
			else
				!	if(p)write(6,*)'setmatrixm,isilist1,isilist2',isilist1,isilist2
				call setmatrix()
			endif !if(iscoef)   2334
		elseif(isrealout)then
 
			if(issum)then
				call setsumr()
			else
				!	if(p)write(6,*)'tasa'
				call setmatrix()
			endif !if(issum)   2347
 
 
 
		endif !if(ismatrixout.or.islistout)   2318
 
 
 
	endif !if(get)   2307
	return
 
	case(j_iplist)
	call iranges()
	if(j_err)return
	if(.not.nrang1)then
		call j_getname(imat)
		write(6,*)'LIST ',j_oname(1:j_loname), ' can contain only row range'
		j_err=.true.;return
	endif !if(.not.nrang1)   2366
	if(simple)then
		if(point)then
			j_o(imat)%i2(irange(1))=iout
		elseif(get)then
			call j_copy2(j_o(imat)%i2(irange(1)),iout)
		else
			iel=j_o(imat)%i2(irange(1))
			if(j_otype(iel).eq.j_ipreal.and.j_otype(iout).eq.j_ipreal)then
				j_v(iel)= j_v(iout)
			else
				call j_copy2(iout,iel)
 
			endif !if(j_otype(iel).eq.j_ipreal.and.j_otype(iout).eq.j_ipreal)   2378
 
		endif !if(point)   2372
 
 
	endif !if(simple)   2371
 
	case default
	call j_objfunc(get,iob,io,imat,narg1,j_o(iob)%i(io+3:io+1+narg),iout)
 
 
	end select
 
 
	return
	contains
 
	subroutine putmat0(iel,vali)
		integer*8::iel
		double precision::vali
		if(ismati)then
			j_o(imat)%i2(iel)=vali
		else
			j_o(imat)%d(iel)=vali
		endif !if(ismati)   2403
	end subroutine
 
	subroutine getmat0(outp,iel)
		integer::iel
		double precision::outp
		if(ismati)then
			outp=j_o(imat)%i2(iel)
		else
			outp=j_o(imat)%d(iel)  !=vali
		endif !if(ismati)   2413
	end subroutine
 
	subroutine sumcoef()
 
		call j_getoption(iob,io,j_msum,-1,1,j_ipreal,.false.,j,j_optarg0)
		if(j_err)return
		call j_getoption(iob,io,j_mcoef,-1,1,j_ipreal,.true.,j2,j_optarg)
		issum=j.ge.0
		if(issum.and.get)then
			write(6,*)'sum-> is not in the output'
			j_err=.true.;return
		endif !if(issum.and.get)   2426
		if(j.gt.0)then
			coef=j_v(j_optarg0(1))
			issumcoef=.true.
		else
			issumcoef=.false.
		endif !if(j.gt.0)   2430
		if(j2.gt.0)then
			if(issumcoef)then
 
				write(6,*)'cannot give coefficient both in sum-> and coef->'
				j_err=.true.
				return
			endif !if(issumcoef)   2437
 
			iscoef=.true.
			coef=j_v(j_optarg(1))
		else
			iscoef=.false.
		endif !if(j2.gt.0)   2436
 
 
	end subroutine
 
	! subroutine printmat(imat)
	! integer::imat
	! call j_getname(imat)
	! write(6,*)'matrix ',j_oname(1:j_loname)
	! if(j_otype(imat).ne.j_ipmatrix)then
	! write(6,*)'Was not MATRIX'
	! return
	! endif !if(j_otype(imat).ne.j_ipmatrix)   2345
	! iba=0
	! do j=1,j_o(imat)%i(1)
	! write(6,'(10f4.0)')j_o(imat)%d(iba+1:iba+j_o(imat)%i(2))
	! iba=iba+j_o(imat)%i(2)
	! enddo !j=1,j_o(imat)%i(1)   2350
	! return
	! end subroutine !subroutine printmat(imat)
 
	subroutine iranges()
		integer ::i
		isilist1=.false.
		isilist2=.false.
		isilist=.false.
		ismatrix1=.false.
		ismatrix2=.false.
		irange=0
		!	nrang1=.false.
		do i=1,narg1
			isilist=.false.
			call j_getname(arg(i))
			!		write(17,*)'iii ',i,arg(i),j_oname(1:j_loname),j_otype(arg(i))
			if(j_otype(arg(i)).eq.j_ipmatrix)then
				isilist=.true.
				j2=j_o(arg(i))%i(3)
				call j_allocatei(j_o(arg(i))%i2,j2)
				ninlist=0
				do j=1,j2
					if(j_o(arg(i))%d(j).gt.j_0)then
						ninlist=ninlist+1
						j_o(arg(i))%i2(ninlist)=j_o(arg(i))%d(j)
					endif !if(j_o(arg(i))%d(j).gt.j_0)   2488
				enddo !j=1,j2   2487
				!	write(6,*)'ninlist',ninlist,j2,j_o(arg(i))%i2
			elseif(j_otype(arg(i)).eq.j_ipilist)then
				isilist=.true.
				ninlist=j_o(arg(i))%i(1)
			endif !if(j_otype(arg(i)).eq.j_ipmatrix)   2482
			if(isilist)then
				if(diag.or.((iscoef.or.issum).and.set))then
					call j_getname(imat)
					write(6,*)'ILIST argument does not yet work yet for ',j_oname(1:j_loname),' with option:'
					if(iscoef)	write(6,*)'coef->'
					if(issum)	write(6,*)'sum->'
					if(diag)	write(6,*)'diag->'
 
					j_err=.true.;return
				endif !if(diag.or.((iscoef.or.issum).and.set))   2499
 
				if(i.eq.1)then
					isilist1=.true.
					ivilist1=arg(i)
					arg(i)=j_ivall
					ninlist1=ninlist
 
				elseif(.not.isilist2)then
					isilist2=.true.
					ivilist2=arg(i)
					arg(i)=j_ivall
					ninlist2=ninlist
				else
					call j_getname(imat,arg(i))
					write(6,*)i,'th argument ',j_oname2(1:j_loname2),' of ',j_oname(1:j_loname), ' exess ILIST/MATRIX'
					j_err=.true.
				endif !if(i.eq.1)   2509
 
			elseif(j_otype(arg(i)).ne.j_ipreal)then
				call j_getname(imat,arg(i))
				write(6,*)i,'th argument ',j_oname2(1:j_loname2),' of ',j_oname(1:j_loname), ' is not REAL'
				j_err=.true.
			endif !if(isilist)   2498
 
		enddo !i=1,narg1   2478
		if(j_err)return
		!	if(p)write(6,*)'isilist1,isilist2',isilist1,isilist2,'arg',arg,'ivall',j_ivall
		simple=.false.
		select case (narg1)
		case(1)
		nrang1=.true.
		if(arg(1).eq.j_ivall)then
			! a(All)
			irange(1)=1
			irange(2)=nrow
			nrow2=nrow
			nelmat=nrow
		else
			! a(7)
			irange(1)=j_v(arg(1))
			irange(2)=irange(1)
			!	nelmat=1
			simple=.true.
		endif !if(arg(1).eq.j_ivall)   2539
		if(isilist1)goto 88
 
		return
 
		case(2)
		if(p)write(6,*)'case2 nrow,ncol,nrang1',nrow,ncol,nrang1,arg,diag,'irange ',irange
		if(arg(1).eq.j_ivall)then  !ei toimi
			! a(All,
			irange(1)=1
			irange(2)=nrow
			nrow2=nrow
			if(arg(2).eq.j_ivall)then
				! a(All,All)
				irange(3)=1
				irange(4)=ncol
				ncol2=ncol
			else
				irange(3)=j_v(arg(2))
				irange(4)=irange(3)
				ncol2=j_18
				! a(All,4)
			endif !if(arg(2).eq.j_ivall)   2563
			nrang1=.false.
			if(diag)goto 77
			goto 88
		else
			irange(1)=j_v(arg(1))
		endif !if(arg(1).eq.j_ivall)   2558
 
		if(arg(2).eq.j_ivall)then
			! a(4,All)
			irange(2)=irange(1)
			if(arg(1).ne.j_ivall)nrow2=j_18
			irange(3)=1
			irange(4)=ncol
 
			ncol2=ncol
			nrang1=.false.
		else
			if(j_v(arg(2)).lt.j_0)then
 
				if(arg(2).eq.j_ivninf)then
					! a(2,Ninf)
					irange(2)=nrow
				else
					irange(2)=-j_v(arg(2))
 
					! a(3,-5)
				endif !if(arg(2).eq.j_ivninf)   2593
 
				nrang1=.true.
				nrow2=irange(2)-irange(1)+1
			else
				! a(3,4)
				irange(3)=j_v(arg(2))
 
				simple=.true.
				nrang1=.false.
			endif !if(j_v(arg(2)).lt.j_0)   2591
 
		endif !if(arg(2).eq.j_ivall)   2581
		if(p)write(6,*)'irnage,nrang1,simple',irange,nrang1,simple
 
		case(3)
		nrang1=.false.
		if(arg(1).eq.j_ivall)then
			! a(All,2,
			irange(1)=1
			irange(2)=nrow
			nrow2=nrow
			irange(3)=j_v(arg(2))
 
			if(j_v(arg(3)).lt.j_0)then
				if(arg(3).eq.j_ivninf)then
					! a(All,2,Ninf)
					irange(4)=ncol
 
				else
					irange(4)=-j_v(arg(3))
 
					ncol2=irange(4)-irange(3)+1
					! a(All,2,-7)
				endif !if(arg(3).eq.j_ivninf)   2625
 
			endif !if(j_v(arg(3)).lt.j_0)   2624
 
 
		else !if(arg(1).eq.j_ivall)   2343
			! not: a(All,
			irange(1)=j_v(arg(1))
 
			if(j_v(arg(2)).lt.j_0)then
				if(arg(2).eq.j_ivninf)then
					irange(2)=nrow
					nrow2=nrow-irange(1)+1
					! a(2,Ninf,
				else
					! a(2,-7,
					irange(2)=-j_v(arg(2))
 
					nrow2=irange(2)-irange(1)+1
				endif !if(arg(2).eq.j_ivninf)   2644
				if(arg(3).eq.j_ivall)then
					irange(3)=1
					irange(4)=ncol
					ncol2=ncol
				else
					irange(3)=j_v(arg(3))
					irange(4)=irange(3)
					ncol2=j_18
				endif !if(arg(3).eq.j_ivall)   2654
 
 
 
			else
				! a(2,3,  )
				irange(2)=irange(1)
				nrow2=j_18
				irange(3)=j_v(arg(2))
 
				if(arg(3).eq.j_ivninf)then
					irange(4)=ncol
				else
					irange(4)=-j_v(arg(3))
 
				endif !if(arg(3).eq.j_ivninf)   2672
				ncol2=irange(4)-irange(3)+1
			endif !if(j_v(arg(2)).lt.j_0)   2643
		endif !if(arg(1).eq.j_ivall)   2617
 
		case(4)
		nrang1=.false.
		irange(1)=j_v(arg(1))
 
		if(arg(2).eq.j_ivninf)then
			irange(2)=nrow
		else
			irange(2)=-j_v(arg(2))
 
		endif !if(arg(2).eq.j_ivninf)   2686
		irange(3)=j_v(arg(3))
 
		if(arg(4).eq.j_ivinf)then
			irange(4)=ncol
		else
			irange(4)=-j_v(arg(4))
 
		endif !if(arg(4).eq.j_ivinf)   2694
		nrow2=irange(2)-irange(1)+1
		ncol2=irange(4)-irange(3)+1
 
 
		case default
		if(narg1.eq.0.and.diag)then
			irange(1)=1
			irange(2)=nrow
			nrang1=.true.
			nrow2=nrow
		else
			call j_getname(imat)
			write(6,*)j_oname(1:j_loname),' has ',narg1,' indexes, should have >=1 <=4'
			j_err=.true.;return
		endif !if(narg1.eq.0.and.diag)   2705
		end select
		! 80   		nrow2=irange(2)-irange(1)+1
		! if(nrang.eq.1)then
		! ncol2=j_18
		! else
		! ncol2=irange(4)-irange(3)+1
		! endif
 
88		if(isilist1)then

			irange(2)=ninlist1  !j_o(ivilist1)%i(3)
			do iro=1,irange(2)
				if(j_o(ivilist1)%i2(iro).lt.0.or.j_o(ivilist1)%i2(iro).gt.nrow2)then
					!	write(6,*)'nrow2,ncol2',nrow2,ncol2
					call j_getname(imat)
					write(6,*)'element ',iro,' of first ILIST argument has illegal value ',j_o(ivilist1)%i2(iro)
					j_err=.true.
				endif !if(j_o(ivilist1)%i2(iro).lt.0.or.j_o(ivilist1)%i2(iro).gt.   2727
			enddo !iro=1,irange(2)   2726
			nrow2=ninlist1  !j_o(ivilist1)%i(3)
		endif !88		if(isilist1)   2723
		if(isilist2)then
			irange(4)=ninlist2  !j_o(ivilist2)%i(3)
			do iro=1,irange(4)
				if(j_o(ivilist2)%i2(iro).lt.0.or.j_o(ivilist2)%i2(iro).gt.ncol2)then
					call j_getname(imat)
					write(6,*)'element ',iro,' of  ILIST argument has illegal value ',j_o(ivilist2)%i2(iro)
					j_err=.true.
				endif !if(j_o(ivilist2)%i2(iro).lt.0.or.j_o(ivilist2)%i2(iro).gt.   2739
			enddo !iro=1,irange(4)   2738
			ncol2=ninlist2   !j_o(ivilist2)%i(3)
 
 
		endif !if(isilist2)   2736
		if(j_err)return
		!	write(6,*)'islist ',isilist1,isilist2,irange
77		if(diag)then


			if(.not.nrang1.and.nrow2.ne.ncol2)then
				call j_getname(imat)
				write(6,*)'diag-> can be applied only to square matrix, submatrix of ',j_oname(1:j_loname),' is ',&
					int(irange)
				j_err=.true.
			endif !if(.not.nrang1.and.nrow2.ne.ncol2)   2754
		endif !77		if(diag)   2751
		return
 
 
	end subroutine !subroutine iranges()
 
	subroutine getmatrix()
		!	if(p)write(17,*)'getmatrix,irange',irange
		if(ncol2.eq.j_18.and.nrow2.eq.j_18)then
			if(nrang1)then
				j_v(iout)=j_o(imat)%d((irange(1)-1)*ncol+irange(1))
			else
				j_v(iout)=j_o(imat)%d((irange(1)-1)*ncol+irange(3))
			endif !if(nrang1)   2769
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			return
		endif !if(ncol2.eq.j_18.and.nrow2.eq.j_18)   2768
		if(diag)then
			nrow2=irange(2)-irange(1)+1
			ncol2=j_18
			nel2=nrow2
		elseif(nrang1)then
			nel2=irange(2)-irange(1)+1
			!		write(6,*)'nel2',nel2,nrow,ncol
 
			if(nrow.eq.j_18)then
				nrow2=nrow
				ncol2=nel2
			else
				ncol2=j_18
				nrow2=nel2
 
			endif !if(nrow.eq.j_18)   2785
		else
			nel2=nrow2*ncol2
 
		endif !if(diag)   2777
		!	write(6,*)'nrow2,',nrow2,ncol2,nel2,iout,imat
		if(iout.eq.imat)then
			allocate(tem(1:nel2))
			d=>tem
		else
			iout=j_defmatrix(iout,' ',nrow2,ncol2,j_matreg)
			d=>j_o(iout)%d
		endif !if(iout.eq.imat)   2798
 
		if(diag)then
			if(nrang1)then
				ibasin=(irange(1)-1)*ncol+irange(1)
			else
				ibasin=(irange(1)-1)*ncol+irange(3)
			endif !if(nrang1)   2807
			do iro=1,nrow2
				d(iro)=j_o(imat)%d(ibasin)
				ibasin=ibasin+ncol+j_18
			enddo !iro=1,nrow2   2812
 
 
 
 
		elseif(nrang1)then
			if(isilist1)then
				do iro=1,irange(2)
					d(iro)=j_o(imat)%d( j_o(ivilist1)%i2(iro))
 
				enddo !iro=1,irange(2)   2822
 
			else
				if(irange(2).gt.j_o(imat)%i(3))then
					write(6,*)'trying to get elements up to ',irange(2), ' there are only ',j_o(imat)%i(3)
					j_err=.true.;return
				endif !if(irange(2).gt.j_o(imat)%i(3))   2828
				d(1:irange(2)-irange(1)+1)=j_o(imat)%d(irange(1):irange(2))
			endif !if(isilist1)   2821
		else
 
			if(ncol.eq.ncol2)then
				if(isilist1)then
					ibas=0
					do iro=1,irange(2)
 
						ibasin= (j_o(ivilist1)%i2(iro)-1)*ncol
 
						d(ibas+1:ibas+ncol)=j_o(imat)%d( ibasin+1:ibasin+ncol)
						ibas=ibas+ncol
					enddo !iro=1,irange(2)   2839
 
				else
					!			write(6,*)'ncol2,nrow2,irange,ncol', ncol2,nrow2,irange,ncol,(irange(1)-1)*ncol+1,'*',irange(2)*ncol
					d(1:ncol2*nrow2)=j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)
 
				endif !if(isilist1)   2837
			else
 
				if(isilist1.and.isilist2)then
					ibas=0
					do iro=1,irange(2)
						ibasin= (j_o(ivilist1)%i2(iro)-1)*ncol
 
						do ico=1,irange(4)
							ibas=ibas+1
							d(ibas)=j_o(imat)%d( ibasin+j_o(ivilist2)%i2(ico))
						enddo !ico=1,irange(4)   2859
 
					enddo !iro=1,irange(2)   2856
				elseif(isilist1)then
					ibas=0
					do iro=1,irange(2)
						ibasin= (j_o(ivilist1)%i2(iro)-1)*ncol+irange(3)-1
 
						do ico=irange(3),irange(4)
 
							d(ibas+1:ibas+ncol2)=j_o(imat)%d( ibasin+1:ibasin+ncol2)
						enddo !ico=irange(3),irange(4)   2870
						ibas=ibas+ncol2
					enddo !iro=1,irange(2)   2867
 
 
				elseif(isilist2)then
					ibas=0
					ibasin=(irange(1)-1)*ncol
					do iro=irange(1),irange(2)
 
 
						do ico=1,irange(4)
							ibas=ibas+1
							d(ibas)=j_o(imat)%d( ibasin+j_o(ivilist2)%i2(ico))
						enddo !ico=1,irange(4)   2884
						ibasin=ibasin+ncol
					enddo !iro=irange(1),irange(2)   2881
 
 
				else
 
					ibasin=(irange(1)-1)*ncol+irange(3)-1
					ibas=0
 
					do iro=irange(1),irange(2)
						if(ibasin+ncol2.gt.j_o(imat)%i(3))then
							write(6,*)'error in getmat,irange',irange
							j_err=.true.
							return
						endif !if(ibasin+ncol2.gt.j_o(imat)%i(3))   2898
						d(ibas+1:ibas+ncol2)=j_o(imat)%d(ibasin+1:ibasin+ncol2)
						ibas=ibas+ncol2
						ibasin=ibasin+ncol
 
					enddo !iro=irange(1),irange(2)   2897
				endif !if(isilist1.and.isilist2)   2854
			endif !if(ncol.eq.ncol2)   2836
		endif !if(diag)   2806
		!	if(iout.eq.imat)write(6,*)'tem',tem
		if(iout.eq.imat)iout=j_defmatrix(iout,' ',nrow2,ncol2,j_matreg,temp=tem)
 
	end subroutine !subroutine getmatrix()
 
	subroutine setdiag()
 
		if(nrang1)then
			ibasin=(irange(1)-1)*ncol+irange(1)
		else
			ibasin=(irange(1)-1)*ncol+irange(3)
		endif !if(nrang1)   2918
 
		if(isrealout)then
 
			do iro=irange(1),irange(2)
 
 
				j_o(imat)%d(ibasin)=j_v(iout)
				ibasin=ibasin+ncol+j_18
			enddo !iro=irange(1),irange(2)   2926
		else
 
			if(nelout.ne.nrow2)then
				call j_getname(imat,iout)
				write(6,*)'diagonal of ',j_oname(1:j_loname),' has size ',nrow2,' but ',j_oname2(1:j_loname2),' has size ',nelout
				j_err=.true.;return
 
			endif !if(nelout.ne.nrow2)   2934
			!		write(6,*)'nrow2',nrow2
			do iro=1,nrow2
 
 
				j_o(imat)%d(ibasin)=j_o(iout)%d(iro)
				ibasin=ibasin+ncol+j_18
			enddo !iro=1,nrow2   2941
 
		endif !if(isrealout)   2924
		return
 
	end subroutine
 
	subroutine setsumm()
 
		if(ncol.eq.ncol2)then
			j_o(imat)%d(irange(1):irange(2))=j_o(imat)%d(irange(1):irange(2))+j_o(iout)%d(1:nelout)
		elseif(irange(3).eq.1.and.irange(4).eq.ncol)then
			j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)=&
				j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)+j_o(iout)%d(1:nelout)
		else
			if(nrang1)then
 
				ncol2=irange(2)-irange(1)+1
 
				j_o(imat)%d(irange(1):irange(2))=j_o(imat)%d(irange(1):irange(2))+j_o(iout)%d(1:ncol2)
 
			else
				ibas=(irange(1)-1)*ncol+irange(3)-1
				ncol2=irange(2)-irange(1)+1
				ibasin=0_8
 
				do iro=irange(1),irange(2)
					!		write(6,*)'iro,ibas,ncol2,ibasin,irange,nrang1',iro,ibas,ncol2,ibasin,irange,nrang1
					j_o(imat)%d(ibas+1:ibas+ncol2)=j_o(imat)%d(ibas+1:ibas+ncol2)+j_o(iout)%d(ibasin+1:ibasin+ncol2)
 
					ibas=ibas+ncol
					ibasin=ibasin+ncol2
				enddo !iro=irange(1),irange(2)   2972
 
 
 
 
			endif !if(nrang1)   2961
		endif !if(ncol.eq.ncol2)   2955
 
	end subroutine
 
 
 
 
	subroutine setsumr()
 
		if(nrang1)then
			j_o(imat)%d(irange(1):irange(2))=j_o(imat)%d(irange(1):irange(2))+j_v(iout)
		elseif(irange(3).eq.1.and.irange(4).eq.ncol)then
			j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)=&
				j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)+j_v(iout)
		else
			ibas=(irange(1)-1)*ncol+irange(3)-1
			ncol2=irange(2)-irange(1)+1
			do iro=irange(1),irange(2)
				j_o(imat)%d(ibas+1:ibas+ncol2)=j_o(imat)%d(ibas+1:ibas+ncol2)+j_v(iout)
 
				ibas=ibas+ncol
 
			enddo !iro=irange(1),irange(2)   3001
		endif !if(nrang1)   2993
		return
	end subroutine
 
	subroutine setcoefm()
		if(nrang1)then
 
 
			ncol2=irange(2)-irange(1)+1
 
			j_o(imat)%d(irange(1):irange(2))=j_o(imat)%d(irange(1):irange(2))+coef*j_o(iout)%d(1:ncol2)
 
 
		else
			if(irange(3).eq.1.and.irange(4).eq.ncol)then
				j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)=coef*j_o(iout)%d(1:nelout)
			else
				ibas=(irange(1)-1)*ncol+irange(3)-1
				ncol2=irange(2)-irange(1)+1
				ibasin=0_8
 
				do iro=irange(1),irange(2)
					j_o(imat)%d(ibas+1:ibas+ncol2)=coef*j_o(iout)%d(ibasin+1:ibasin+ncol2)
 
					ibas=ibas+ncol
					ibasin=ibasin+ncol2
				enddo !iro=irange(1),irange(2)   3028
			endif !if(irange(3).eq.1.and.irange(4).eq.ncol)   3021
 
		endif !if(nrang1)   3012
 
		return
	end subroutine
 
 
	subroutine setsumcoef()
		if(ncol.eq.ncol2)then
			j_o(imat)%d(irange(1):irange(2))=j_o(imat)%d(irange(1):irange(2))+coef*j_o(iout)%d(1:nelout)
		elseif(irange(3).eq.1.and.irange(4).eq.ncol)then
			j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)=&
				j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)+coef*j_o(iout)%d(1:nelout)
		else
			if(nrang1)then
 
				ncol2=irange(2)-irange(1)+1
 
				j_o(imat)%d(irange(1):irange(2))=j_o(imat)%d(irange(1):irange(2))+coef*j_o(iout)%d(1:ncol2)
 
			else
				ibas=(irange(1)-1)*ncol+irange(3)-1
				ncol2=irange(2)-irange(1)+1
				ibasin=0_8
 
				do iro=irange(1),irange(2)
					j_o(imat)%d(ibas+1:ibas+ncol2)=j_o(imat)%d(ibas+1:ibas+ncol2)+coef*j_o(iout)%d(ibasin+1:ibasin+ncol2)
 
					ibas=ibas+ncol
					ibasin=ibasin+ncol2
				enddo !iro=irange(1),irange(2)   3060
			endif !if(nrang1)   3049
		endif !if(ncol.eq.ncol2)   3043
 
		return
	end subroutine
 
	! subroutine setsumr()
	! if(nrang1)then
	! j_o(imat)%d(irange(1):irange(2))=j_o(imat)%d(irange(1):irange(2))+j_v(iout)
	! elseif(irange(3).eq.1.and.irange(4).eq.ncol)then
	! j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)=j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)+j_v(iout)
	! else
	! ibas=(irange(1)-1)*ncol+irange(3)-1
	! ncol2=irange(2)-irange(1)+1
	! do iro=irange(1),irange(2)
	! j_o(imat)%d(ibas+1:ibas+ncol2)=j_o(imat)%d(ibas+1:ibas+ncol2)+j_v(iout)
 
	! ibas=ibas+ncol
	! enddo !iro=irange(1),irange(2)   2738
 
	! endif !if(nrang1)   2731
 
 
	! end subroutine
 
	! subroutine setmatrixr()
	! if(nrang1)then
	! if(isilist1)then
	! !		do iro=1,irange(2)
	! j_o(imat)%d( j_o(ivilist1)%i2(1:irange(2)))=j_v(iout)
 
	! !		enddo !iro=1,irange(2)   2999
 
	! else
 
	! j_o(imat)%d(irange(1):irange(2))=j_v(iout)
	! endif
	! elseif(irange(3).eq.1.and.irange(4).eq.ncol)then
	! if(isilist1)then
	! ibas=(irange(1)-1)*ncol
	! !		do ico=1,irange(2)
	! j_o(imat)%d(ibas+ j_o(ivilist1)%i2(1:irange(2)))=j_o(iout)%d(ico)
 
	! !		enddo !ico=1,irange(2)   3012
 
 
	! else
	! j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)=j_v(iout)
	! endif
	! else
 
 
	! ibas=(irange(1)-1)*ncol+irange(3)-1
	! ncol2=irange(2)-irange(1)+1
	! do iro=irange(1),irange(2)
	! j_o(imat)%d(ibas+1:ibas+ncol2)=j_v(iout)
 
	! ibas=ibas+ncol
	! enddo !iro=irange(1),irange(2)   2986
 
	! endif !if(nrang1)   2979
	! end subroutine
 
	subroutine setmatrix()
		!	write(6,*)'set',irange,nrang1,'ncol',ncol,isilist1,isilist2
		if(nrang1)then
			if(isilist1)then
				if(isrealout)then
					j_o(imat)%d( j_o(ivilist1)%i2(1:irange(2)))=j_v(iout)
 
				else
					!	do iro=1,irange(2)
					j_o(imat)%d( j_o(ivilist1)%i2(1:irange(2)))=j_o(iout)%d(1:irange(2))
 
					!	enddo !iro=1,irange(2)   2999
				endif !if(isrealout)   3133
			else
				if(isrealout)then
					j_o(imat)%d(irange(1):irange(2))=j_v(iout)
 
				else
					j_o(imat)%d(irange(1):irange(2))=j_o(iout)%d(1:nelout)
				endif !if(isrealout)   3143
			endif !if(isilist1)   3132
		elseif(irange(3).eq.1.and.irange(4).eq.ncol)then
			if(isilist1)then
				ibas=(irange(1)-1)*ncol
				if(isrealout)then
					j_o(imat)%d(ibas+ j_o(ivilist1)%i2(1:irange(2)))=j_v(iout)
 
				else
					!		do ico=1,irange(2)
					j_o(imat)%d(ibas+ j_o(ivilist1)%i2(1:irange(2)))=j_o(iout)%d(1:irange(2))
 
					!		enddo !ico=1,irange(2)   3012
				endif !if(isrealout)   3153
 
			else
				if(isrealout)then
					j_o(imat)%d((irange(1)-1)*ncol+1:irange(2)*ncol)=j_v(iout)
 
				else
					iel=irange(2)*ncol
					if(iel.gt.j_o(imat)%i(3))then
						call j_getname(imat)
						write(6,*)'illegal index for ',j_oname(1:j_loname)
						j_err=.true.;return
 
					endif !if(iel.gt.j_o(imat)%i(3))   3169
					j_o(imat)%d((irange(1)-1)*ncol+1:iel)=j_o(iout)%d(1:nelout)
				endif !if(isrealout)   3164
			endif !if(isilist1)   3151
		else
			if(isilist1.and.isilist2)then
				ibasin=0
				do iro=1,irange(2)
					ibas= (j_o(ivilist1)%i2(iro)-1)*ncol
					if(isrealout)then
						j_o(imat)%d( ibas+j_o(ivilist2)%i2(1:irange(4)))=j_v(iout)
 
					else
						!		do ico=1,irange(4)
 
						j_o(imat)%d( ibas+j_o(ivilist2)%i2(1:irange(4)))=j_o(iout)%d(ibasin+1:ibasin+irange(4))
						!		enddo !ico=1,irange(4)   3063
						ibasin=ibasin+irange(4)
					endif !if(isrealout)   3183
				enddo !iro=1,irange(2)   3181
 
 
			elseif(isilist1)then
				ibasin=0
				!	write(6,*)'isilist1,ncol,irange,ncol2',ncol,irange,ncol2
				do iro=1,irange(2)
					ibas= (j_o(ivilist1)%i2(iro)-1)*ncol
					!		write(6,*)'iro,ibas',iro,ibas,j_o(ivilist1)%i2(iro),ncol,irange(3)
					if(isrealout)then
						j_o(imat)%d( ibas+irange(3):ibas+irange(4))=j_v(iout)
					elseif(islistout)then
						j_o(imat)%d( ibas+irange(3):ibas+irange(4))=j_v(j_o(iout)%i2(ibasin+1:ibasin+ncol2))
					else
						!	do ico=irange(3),irange(4)
 
						j_o(imat)%d( ibas+irange(3):ibas+irange(4))=j_o(iout)%d(ibasin+1:ibasin+ncol2)
						!		enddo !ico=irange(3),irange(4)   3078
					endif !if(isrealout)   3202
					ibasin=ibasin+ncol2
				enddo !iro=1,irange(2)   3199
 
 
			elseif(isilist2)then
				ibas=(irange(1)-1)*ncol
				ibasin=0
				do iro=irange(1),irange(2)
 
					if(isrealout)then
						j_o(imat)%d( ibas+j_o(ivilist2)%i2(1:irange(4)))=j_v(iout)
 
 
					else
						!	do ico=1,irange(4)
						!	ibasin=ibasin+1
						j_o(imat)%d( ibas+j_o(ivilist2)%i2(1:irange(4)))=j_o(iout)%d(ibasin+1:ibasin+irange(4))
						ibasin=ibasin+irange(4)
						!	enddo !ico=1,irange(4)   3095
					endif !if(isrealout)   3221
					ibas=ibas+ncol
				enddo !iro=irange(1),irange(2)   3219
 
 
			else
 
				ibas=(irange(1)-1)*ncol+irange(3)-1
				ncol2=irange(4)-irange(3)+1
				ibasin=0_8
				!		write(6,*)'irange',irange,'ncol,ncol2',ncol,ncol2
				if(isrealout)then
					do iro=irange(1),irange(2)
						j_o(imat)%d(ibas+1:ibas+ncol2)=j_v(iout)
 
						ibas=ibas+ncol
 
					enddo !iro=irange(1),irange(2)   3243
 
 
				else
					do iro=irange(1),irange(2)
						j_o(imat)%d(ibas+1:ibas+ncol2)=j_o(iout)%d(ibasin+1:ibasin+ncol2)
 
						ibas=ibas+ncol
						ibasin=ibasin+ncol2
					enddo !iro=irange(1),irange(2)   3252
				endif !if(isrealout)   3242
 
			endif !if(isilist1.and.isilist2)   3179
		endif !if(nrang1)   3131
	end subroutine !subroutine setmatrix()
 
end subroutine getelem !recursive subroutine getelem(iob,io)
 
 
 
subroutine list2(iob,io)
	!Section list2 Defining LIST2 object for named object and constants.
	! The LIST2 object is simialr to LIST object but it can contain also
	! indices of REAL constants in addition to named objects.
	!endheader
	!Option
	!Output& 1& LIST2 &The generated LIST2 object.
	!Args& 0- &  & named objects or constants. If an argument is LIST it is ex+panded
	!first.
	!endoption
	!Ex list2ex
	! lis2=list2(x1...x4,3,5);
	! li1=list(x1,x2);
	! li2=list(z,y);
	! lis=list2(@li1,@li2,77);
	!endex
	!endsection
	integer,intent(in)::iob
	integer,intent(in)::io
	integer, dimension(:), pointer :: arg !arguments of the function
	!	logical expand
	!defines from compiled transforamtion list
	!subroutine j_startfunction(iob,io,iptype,narg,arg,ivout) ! %%function
	!	expand=j_linkoption(iob,io,j_mexpand).ge.0
	call j_startfunction(iob,io,0,narg,arg,ivout);if(j_err)return
	!subroutine j_deflist(iv,name,list0,ivout) !allocates list object with size list0,
	! but put it as empty
 
	iout=j_deflist(ivout,' ',list0=narg,list=arg)
	! j_o(iout)%i2(1:narg)=arg
	j_otype(iout)=j_iplist2
	!!call j_clearoption(iob,io)
	return
end subroutine list2 !subroutine list2(iob,io)
 
subroutine ilist(iob,io)
	!Section ilist ilist() ILIST of integers
	!Generates a list of integers which can be used as indexes.
	!endheader
	!Option
	!Output& 1& ILIST &The generated ILIST.
	!Args& 0- &REAL & Values to be put into ILIST, or the dimesion
	! of the ILIST when values are given in values->,  or variables whose indeces
	!in in the data are put into the ILIST.
	!data&N|1& DATA& The DATA from whose variable indeces are obtained.
	!extra & 1&REAL & Extra space reserved for later updates of the ILIST.
	!values &N|1-& REAL& Values to be put into ILIST when dimesnion is determined as the
	!only argument
	!endoption
	! Note ILIST is a new object whose all utilization possiblities are not yet explored.
	! It will be used e.g. when developing factory optimization.
	! endnote
	! Note
	! Using ilist() by giving the dimension as argument and values with values-> option
	! imitates the definition of a matrix (column vector). The structure of ILIST object
	! is the same as LIST object which can be used in matrix computations.
	! endnote
	! Ex ilistex ILIST examples
	! {1,4,5};
	! {4...1};
	! A=matrix(4,4)
	! A({1,4},{3})=7;
	!endex
 
	!endsection
	integer,intent(in)::iob
	integer,intent(in)::io
	integer, dimension(:), pointer :: arg !arguments of the function
	integer,dimension(:),allocatable::temp
	integer::iloc
	nextra=0
 
	!subroutine j_startfunction(iob,io,iptype,narg,arg,ivout) ! %%function
	!	expand=j_linkoption(iob,io,j_mexpand).ge.0
	call j_startfunction(iob,io,0,narg,arg,ivout);if(j_err)return
	!	write(6,*)'arg',arg,' ivout',ivout,'narg',narg
	!j_getoption(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
	call j_getoption(iob,io,j_mextra,-1,1,j_ipreal,.true.,noptarg,j_optarg0) ! %%option
	!write(6,*)'tas'
	if(j_err)return
 
	if(noptarg.eq.1)nextra=j_v(j_optarg0(1))
 
	call j_getoption(iob,io,j_mdata,-1,1,j_ipdata,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.eq.1)then
		ivkeep=j_getkeep(j_optarg0(1));if(j_err)return !j_o(j_optarg0(1))%i(2)
		nkeep=j_o(ivkeep)%i(1)
		ntot=narg+nextra
		iout= j_deflist(ivout,' ',nres=ntot)
		do i=1,narg
			do iloc=1,nkeep
				if(arg(i).eq.j_o(ivkeep)%i2(iloc))goto 134
			enddo !iloc=1,nkeep   3359
			iloc=0
			!			iloc=findloc(j_o(ivkeep)%i2(1:nkeep),arg(i),dim=1)
	134			j_o(ivout)%i2(i)=iloc
		enddo !i=1,narg   3358
		j_o(ivout)%i(1)=narg
	else !if(noptarg.eq.1)then
		call j_getoption(iob,io,j_mvalues,-1,9999,j_ipdata,.true.,noptarg,j_optarg0)
		if(j_err)return
		if(narg.eq.0.and.noptarg.eq.0)then
			write(6,*)'without data-> and values-> there must be argument'
			j_err=.true.;return
		endif !if(narg.eq.0.and.noptarg.eq.0)   3370
		if(noptarg.gt.0)then
			nin=noptarg+nextra
			if(allocated(temp))deallocate(temp)
			allocate(temp(1:noptarg))
			temp=j_v(j_optarg0)
			iout=j_deflist(ivout,' ',list0=noptarg,list=temp,nres=nextra)
			deallocate(temp)
		else !if(noptarg.gt.0)then
			if(nextra.gt.0)then
				write(6,*)'if is size is given as argument there cannot be extra->'
				j_err=.true.;return
			endif !if(nextra.gt.0)   3382
			nin=narg+nextra
			iout=j_deflist(ivout,' ',nres=nin)
 
			j_o(iout)%i2(1:narg)=j_v(arg)
			j_o(iout)%i(1)=narg
			j_o(iout)%i(2)=1
			j_o(iout)%i(3)=narg
 
		endif !if(noptarg.gt.0)   3374
 
	endif !if(noptarg.eq.1)   3353
	!subroutine j_deflist(iv,name,list0,ivout) !allocates list object with size list0,
 
	! j_o(iout)%i2(1:narg)=arg
	j_otype(ivout)=j_ipilist
	!!call j_clearoption(iob,io)
	!	write(6,*)'tasa',j_err
	return
end subroutine ilist !subroutine ilist(iob,io)
 
subroutine putlist(iob,io)
	integer,intent(in)::iob
	integer,intent(in)::io
	! Section putlist putlist() puts into LIST an object
	! Usage:\\
	! putlist(LIST,OBJ)\\
	! put OBJ into LIST
	! endsection
 
	integer, dimension(:), pointer :: arg !arguments of the function
	integer,dimension(:),allocatable::temp
	call j_startfunction(iob,io,0,narg,arg,ivout);if(j_err)return
	if(j_otype(arg(1)).ne.j_iplist.and.j_otype(arg(1)).ne.j_ipilist)then
		write(6,*)'first argument is not LIST or ILIST'
		j_err=.true.;return
	endif !if(j_otype(arg(1)).ne.j_iplist.and.j_otype(arg(1)).ne.j_ip   3418
	nle=j_o(arg(1))%i(1)
	narg1=narg-1
	nle2=nle+narg1
 
	if(nle2.ge.j_o(arg(1))%i(4))then
		if(allocated(temp))deallocate(temp)
		allocate(temp(1:nle))
		temp=j_o(arg(1))%i2(1:nle)
		deallocate(j_o(arg(1))%i2)
		allocate(j_o(arg(1))%i2(1:2*nle2))
		j_o(arg(1))%i2(1:nle)=temp
		deallocate(temp)
	endif !if(nle2.ge.j_o(arg(1))%i(4))   3426
	if(j_otype(arg(1)).eq.j_iplist)then
 
		j_o(arg(1))%i2(nle+1:nle+narg1)=arg(2:narg)
	else !if(j_otype(arg(1)).eq.j_iplist)then
		j_o(arg(1))%i2(nle+1:nle+narg1)=j_v(arg(2:narg))
	endif !if(j_otype(arg(1)).eq.j_iplist)   3435
	j_o(arg(1))%i(1)=j_o(arg(1))%i(1)+narg1
 
	return
end subroutine putlist !subroutine putlist(iob,io)
 
subroutine table(iob,io)
	integer,intent(in)::iob
	integer,intent(in)::io
 
	! Section table table() Crossses two LISTs
	! Usage:\\
	! Output=table(rowlist,collist)\\
	! This can be used in factory optimizations
 
	call j_startfunction(iob,io,j_iplist,narg,j_arg,ivout);if(j_err)return
 
	if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
 
 
	allocate(j_o(ivout)%i(1:6))
 
	nfact=j_o(j_arg(2))%i(1)
	nlog=j_o(j_arg(1))%i(1)
	allocate(j_o(ivout)%i2(1:nlog) ) !keep positions
	j_o(ivout)%i(1)=nlog
	j_o(ivout)%i(2)=nfact
 
 
	j_o(ivout)%i(3)=nlog*nfact
	j_o(ivout)%i(4)=j_arg(1)
	j_o(ivout)%i(5)=j_arg(2)
	!call j_getname(ivout)
	j_o(ivout)%i(6)=j_defmatrix(ivout,'%coef',int8(nlog),&
		int8(nfact),j_matreg)
	!	call j_getname(j_o(ivout)%i(6))
	!	write(6,*)'jdjdj ',j_oname(1:j_loname)
	j_otype(ivout)=j_iptable
	! endsection
end subroutine table
 
subroutine fromlist(iob,io)
	call j_startfunction(iob,io,0,narg,j_arg,ivout); if(j_err)return
	if(j_otype(j_arg(1)).ne.j_iplist)then
		call j_getname(j_arg(1))
		write(6,*)j_oname(1:j_loname),' is not LIST'
		j_err=.true.;return
	endif !if(j_otype(j_arg(1)).ne.j_iplist)   3483
	ind=j_v(j_arg(2))
	if(ind.le.0.or.ind.gt.j_o(j_arg(1))%i(1))then
		call j_getname(j_arg(1))
		write(6,*)'cannot get element ',ind,' from ',j_oname(1:j_loname),&
			' with size ',j_o(j_arg(1))%i(1)
		j_err=.true.;return
 
	endif !if(ind.le.0.or.ind.gt.j_o(j_arg(1))%i(1))   3489
	!func,narg,arg1,arg2,iout
	! write(6,*)'bef',io,'*',j_o(iob)%i(io:io+7)
	! call j_getname(j_o(j_arg(1))%i2(ind))
	! write(6,*)'got ',j_o(j_arg(1))%i2(ind),j_oname(1:j_loname)
	! j_o(iob)%i(io+4)=j_o(j_arg(1))%i2(ind)
	! write(6,*)'aft',io,'*',j_o(iob)%i(io:io+7)
	return
 
end subroutine
 
subroutine list2sym(iob,io)
	!Section list2sym list2sym() makes LIST of symbolic constants from another LIST.
	! List of symbolic constants is a LIST of REAL variables where the value of each variable
	! equals the position of the variable in the list. Applications of this will be presented
	! in the worthcoming project of optimization of sawing blade settings.
	!endheader
	!Option
	! Output &1 & LIST & LIST of symbolic constants generated
	!Args &2& List and REAL& The first argument tells from waht list the output is generated.
	!It is probably the list of cases in a DATA with cases. The second argument tells how many '%'
	! signs precede that part of variable names from which the list is made. The variable names
	! are taken to the next '%' or to the end of name.
	!endoption
	!Note See functions namenum() and namenum%() how to pick numbers from variable names.
	!endnote
	!Ex list2symex Example of list2sym()
	!*** Log boxes in a sawmill where boxes are defined with top diameter, length and species.
	! logdata=data(read->N_log,in->,case->)
	! L15%40%Spruce,200
	! L15%43%Spruce,300
	! L18%40%Pine,100
	! L18%43%Spruce,150
	! /
	! li=;list(logdata%?);
	! @li;
	! logdata%case;
	! ** Pick after two %
	! ili=list2sym(logdata%case,2);
	! @ili;
	!endex
	!endsection
 
	call j_startfunction(iob,io,0,narg,j_arg,ivout); if(j_err)return
	irg=j_arg(1)
	if(j_otype(irg).ne.j_iplist)then
		call j_getname(j_arg(1))
		write(6,*)j_oname(1:j_loname),' is not LIST'
		j_err=.true.;return
	endif !if(j_otype(irg).ne.j_iplist)   3540
	lenin=j_o(irg)%i(1)
	num=j_v(j_arg(2))
	ivout=j_deflist(ivout,'',nres=lenin)
	do i=1,lenin
		call j_getname(j_o(irg)%i2(i))
		call j_namenum2(j_oname(1:j_loname),num,iv=ivo,make=.true.)
		if(j_err)return
		iper=j_putlistobject(ivout,single=ivo)
		j_v(ivo)=iper
	enddo !i=1,lenin   3548
 
	return
 
end subroutine
 
subroutine piecewise(iob,io)
	integer,intent(in)::iob
	integer,intent(in)::io
	call j_startfunction(iob,io,0,narg,j_arg,ivout); if(j_err)return
 
	nrow=j_nrows(j_arg(2),mi=2,ma=2);if(j_err)return
	ncol=j_ncols(j_arg(2),mi=2);if(j_err)return
	if(j_err)return
	if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
	allocate(j_o(ivout)%i(1:3))
	j_o(ivout)%i(1:2)=j_arg(1:2)
 
	j_otype=j_ippiecewise
 
 
end subroutine
subroutine type(iob, io)  !
	integer,intent(in)::iob
	integer,intent(in)::io
	! Section type type() Type of an object or all available types
	!  The type of any object can be access by type(]object[).
	!  If the argument is a character variable or character constant referring to
	!  a character constant, and there is content-> option, and the character is the name of
	! an object , the type() returns the type of the object having the name given in the argument.
	! If there is no object having that name, then type() returns -1 and no error is generated.
	! endheader
	! Ex typeex Example of type
	! ttt=8;   !REAL
	! type(ttt);
	! type('ttt'); !type is CHAR
	! type('ttt',content->);
	! cttt='ttt'
	! type(cttt);
	! type(cttt,content->);
	! endex
	! endsection
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	iarg=j_o(iob)%i(io+2)
	if(narg.eq.0)then
		write(6,*)'there are ',j_notypes,' object types'
		write(6,*)'Note, there is a variable for each type, so if(type(data1).eq.DATA) works'
		do i=1,j_notypes
			if(j_otypes(i)(1:4).ne.'free')write(6,*)j_otypes(i)
 
 
		enddo !i=1,j_notypes   3603
		return
 
	endif !if(narg.eq.0)   3600
 
	if(j_linkoption(iob,io,j_mcontent).ge.0.and.j_otype(iarg)==j_ipchar) then
		call j_getchar(iarg,j_tempchar,le)
		iv_ = j_object(j_tempchar(1:le))
		if(iv_>0) then
			j_v(iout) = j_otype(iv_)
		else !if(iv_>0) then
			j_v(iout) = -1
		endif !if(iv_>0)   3615
	else !if(j_linkoption(iob,io,j_mcontent).ge.0.and.j_otype(iarg)==j_ipchar) then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_otype(iarg)
	endif !if(j_linkoption(iob,io,j_mcontent).ge.0.and.j_otype(iarg)=   3612
 
	!	if(j_linkoption(iob,io,j_mcontent).ge.0) !!call j_clearoption(iob,io)  ! subroutine
 
	return
end subroutine !subroutine type(iob, io)
 
subroutine delete_o(iob,io)
	!Section deleteo delete_o() Deletes objects
	!The function delete_o() deletes all the argument objects, which means that the
	! associated allocated vectors are deallocated, and the object will be REAL.
	!endheader
	!Note Note that delete_o() is actually needed only for matrices, because objects
	!can be deleted with obj1,...,objn=0=0 When Object is a matrix, then Object=0 puts all elements
	!into zero
	!endnote
	!Note One can see how much memory each object is using print(Names).
	!endnote
	!Note Deleting a compound object deletes also such
	! subobjects which have no meaning when the main object is deleted. But e.g. if a
	! data object is deleted then the as-sociated transformation object is not
	! deleted as the transformation can be used independently.
	!endnote
	!Note Files can be deleted with delete_f(file). See IO-functions for
	! details.
	!endnote
	!Note If the user has defined own new compound objects in the open source
	! Jlp22 software she/he needs to define the associated delete function.
	!endnote
	!Ex deleteoex Delete object
	!a=matrix(2,3,do->);
	!b=t(a);
	!delete_o(a,b)
	!a,b;
	!endex
	!endsection
	integer,intent(in)::iob
	integer,intent(in)::io
	narg=j_o(iob)%i(io+1)
	do i=1,narg
		iv=j_o(iob)%i(io+1+i)
		if(j_otype(iv).ne.j_ipreal)call j_del(iv)
	end do !i=1,narg   3662
end subroutine !subroutine delete_o(iob,io)
subroutine protect(iob,io)
	!Section protect protect() protects object from being deleted
	!Function protect() protects argument objects from being deleted. They can be modified
	! but not deleted with j_del() function which is used before object is defined to be non REAL.
	!function macro() protects its arguments.
	!endheader
	!endsection
 
	call j_startfunction(iob,io,0,narg,j_arg,ivout);if(j_err)return
	j_prot(j_arg(1:narg))=.true.
	return
end subroutine
subroutine unprotect(iob,io)
	!Section unprotect unprotect() unprotects ptotected objects.
 
	!endheader
	!endsection
	call j_startfunction(iob,io,0,narg,j_arg,ivout);if(j_err)return
	j_prot(j_arg(1:narg))=.false.
	return
end subroutine
 
subroutine delete_f(iob,io)
	!Section deletef delete_f() Deletes files
	!The function delete_f() deletes all files having the names given in the arguments.
	! The arguments can be character constants or character variables associated with character
	! constants. After deleting a file whose name is given in character variable, the variable still refers to the same
	!chracter constant.
	!endheader
	!Ex deletfex Deletef file
	!write('delete_fex.txt',$,'a=matrix(2,4,do->);')
	!write('delete_fex.txt',$,'delete_o(a)')
	!write('delete_fex.txt',$,'a;')
	!close('delete_fex.txt')
	!;incl(delete_fex.txt)
	!delete_f('delete_fex.txt')
	!endex
	!endsection
	logical exis
	narg=j_o(iob)%i(io+1)
	do i=1,narg
		iv=j_o(iob)%i(io+1+i)
		if(j_otype(iv).ne.j_ipchar)then
			call j_printname('object ',iv ,' is not a character object which could point to a file')
			j_err=.true.
			cycle
		endif !if(j_otype(iv).ne.j_ipchar)   3709
		nu_ = j_nunits(j_nused+1)	! väliaikainen kanava tiedostolle
		if (j_iounit(iv)>0)then !j_o(iv)%i(4)>0) then! aukioleva tiedosto
			nu_ = j_iounit(iv)  !j_o(iv)%i(4)
			call j_closeunit(nu_)
		endif !if (j_iounit(iv)>0)   3715
		call j_getchar(iv,j_filename,le_);if(j_err)return
		inquire(file = j_filename(1:le_) , exist=exis)
		if(exis) then
			open(unit=nu_,err=99, file=j_filename(1:le_), status="old")
			goto 4
	99 		write(6,*)'errot opening ', j_filename(1:le_),' for deleting it'
			j_err=.true.;return
	4		close (unit=nu_, status="delete",err=991,iostat=ios)
		else !if(exis) then
			j_err=.true.
			write(6,*)'*file ', j_filename(1:le_),' does not exist'
		endif !if(exis)   3721
	end do !i=1,narg   3707
	return
991	write(6,*)'file ',j_filename(1:le_),' exists but cannot be deleted, iostat=',ios

end subroutine !subroutine delete_f(iob,io)
 
subroutine exist_f(iob,io)
	integer,intent(in)::iob
	integer,intent(in)::io
	logical exis
	narg=j_o(iob)%i(io+1)
	iv=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+narg+2)
 
	if(j_otype(iv).ne.j_ipchar)then
		call j_printname('object ',iv ,' is not a character object which could point to a file')
		j_err=.true.;return
	endif !if(j_otype(iv).ne.j_ipchar)   3745
	nu_ = j_nunits(j_nused+1)	! väliaikainen kanava tiedostolle
	if (j_iounit(iv)>0)then !j_o(iv)%i(4)>0) then! aukioleva tiedosto
		nu_ = j_iounit(iv)  !j_o(iv)%i(4)
		call j_closeunit(nu_)
	endif !if (j_iounit(iv)>0)   3750
	call j_getchar(iv,j_filename,le_);if(j_err)return
	inquire(file = j_filename(1:le_) , exist=exis)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(exis) then
		j_v(iout)=j_1
	else !if(exis) then
		j_v(iout)=j_0
	endif !if(exis)   3757
	if(narg.ge.2)then
		iv2=j_o(iob)%i(io+2)
		if (j_iounit(iv)>0)then !j_o(iv)%i(4)>0) then! aukioleva tiedosto
			j_v(iv2)=j_iounit(iv)
		else !if (j_iounit(iv)>0)then
			j_v(iv2)=j_0
		endif !if (j_iounit(iv)>0)   3764
	endif !if(narg.ge.2)   3762
	return
 
end subroutine !subroutine exist_f(iob,io)
 
subroutine exist_o(iob,io)
	integer,intent(in)::iob
	integer,intent(in)::io
	narg=j_o(iob)%i(io+1)
	iv=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iv).ne.j_ipchar)then
		call j_printname('object ',iv ,' is not a character object')
		j_err=.true.;return
	endif !if(j_otype(iv).ne.j_ipchar)   3780
	call j_getchar(iv,j_filename,le);if(j_err)return
	iv2=j_object(j_filename(1:le))
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(iv2.le.0)then
		j_v(iout)=0
	else !if(iv2.le.0)then
		j_v(iout)=j_otype(iout)
	endif !if(iv2.le.0)   3787
 
end subroutine !subroutine exist_o(iob,io)
 
subroutine name(iob,io)
	integer,intent(in)::iob
	integer,intent(in)::io
	iv=j_v(j_o(iob)%i(io+2))
	!narg=j_o(iob)%i(io+1)
	!iout=j_o(iob)%i(io+narg+2)
	!	write(6,*)j_o(iob)%i(io:io+5)
	call j_getname(iv)
	if(j_err)then
		write(6,*)'**illegal argument ',iv, ' must be between 1 and ',j_mxv
		return
	endif !if(j_err)   3803
	write(6,*)j_oname(1:j_loname)
	!		call j_defchar(0 ,j_oname(1:j_loname),ivchar)
 
	!		iout=j_o(iob)%i(io+j_o(iob)%i(io+1)+2)
	!	write(6,*)'iout',iout
	!		call j_asschar2(ivchar,iout)
 
end subroutine !subroutine name(iob,io)
 
subroutine do(iob,io)
	integer,intent(in)::iob
	integer,intent(inout)::io
	integer, dimension(:), pointer :: arg  !narg=4
 
 
	!addres where to jump if no iterations ,index variable,iv-low,iv-up,iv-step,
	!current,up,step
	arg=>j_o(iob)%i(io+1:io+8)
	arg(6:8)=j_v(arg(3:5))
	if(arg(8).eq.0)then
		write(6,*)'do(): step is zero'
		j_err=.true.; return
	endif !if(arg(8).eq.0)   3826
	niter=(arg(7)-arg(6))/arg(8)+1
 
	if(j_otype(arg(2)).ne.j_ipreal)call j_del(arg(2))
	j_v(arg(2))=arg(6)
	!write(6,*)'<74',arg,j_v(arg(2))
	if(niter.le.0)then
		io=arg(1)!jump after enddo
	else !if(niter.le.0)then
		io=io+9
	endif !if(niter.le.0)   3835
	return
end subroutine do !subroutine do(iob,io)
 
subroutine enddo(iob,io)
	integer,intent(in)::iob
	integer,intent(inout)::io
	iod=j_o(iob)%i(io+1)   !io of do(
	!write(6,*)'<636iod ',iod
	j_o(iob)%i(iod+6)=j_o(iob)%i(iod+6)+j_o(iob)%i(iod+8)
 
	!	write(6,*)'<37377arg',j_o(iob)%i(iod+6)
 
	if(j_o(iob)%i(iod+8).gt.0)then
		!	if(j_o(iob)%i(iod+6).gt.j_o(iob)%i(iod+7))write(6,*)'<88 io,newio ',io,io+2
		if(j_o(iob)%i(iod+6).gt.j_o(iob)%i(iod+7))then
 
			io=io+2
			return
		endif !if(j_o(iob)%i(iod+6).gt.j_o(iob)%i(iod+7))   3854
	else !if(j_o(iob)%i(iod+8).gt.0)then
		if(j_o(iob)%i(iod+6).lt.j_o(iob)%i(iod+7))then
 
			io=io+2
			return
		endif !if(j_o(iob)%i(iod+6).lt.j_o(iob)%i(iod+7))   3860
	endif !if(j_o(iob)%i(iod+8).gt.0)   3852
	j_v(j_o(iob)%i(iod+2))=j_o(iob)%i(iod+6)  !index is updated always
	io=iod+9   !io=io+1+3  new io after this it should be iod+4+3
	!thus io+4=iod+7 => io=iod+3
	!	write(6,*)'<66enddoio, jump to ',io
	return
 
end subroutine !subroutine enddo(iob,io)
 
subroutine if(iob,io)
	integer,intent(in)::iob
	integer,intent(inout)::io
	! write(6,*)'<256IF',io,'*',j_o(iob)%i(io:io+3),j_v(j_o(iob)%i(io+2))
	! write(6,'(20i5)')j_o(iob)%i(1: j_o(iob)%i(0))
	!ioold=io
	irg=j_o(iob)%i(io+2)
	if(j_otype(irg).ne.j_ipreal)then
		call j_getname(irg)
		write(6,*)'if requires REAL argument ',j_oname(1:j_loname), ' is ',j_otypes(j_otype(irg))
		j_err=.true.;return
	endif !if(j_otype(irg).ne.j_ipreal)   3881
	!	write(6,*)'irg',irg,j_v(irg),j_o(iob)%i(io+1),j_o(iob)%i(io+3)
	if(j_v(irg).eq.j_0)then
		io=j_o(iob)%i(io+1)  !condition not satisfied
	else !if(j_v(j_o(iob)%i(io+2)).eq.j_0)then
		io=j_o(iob)%i(io+3)   !condition satified
	endif !if(j_v(irg).eq.j_0)   3887
	!	write(6,*)'io ',io
	!if(j_o(iob)%i2(13).gt.0)write(26,'(20i5)')iob,ioold,j_o(iob)%i(ioold+1:ioold+3),io,j_o(iob)%i(io:io+5)
	return
end subroutine if !subroutine if(iob,io)
 
! recursive subroutine sit(iob,io) !sit -prompt
! integer ::recursion=0
! integer,dimension(3)::ivcursor=(/j_ivcursor,0,0/)
! save recursion,ivcursor
 
! integer, dimension (:), pointer :: optarg_
 
! recursion=recursion+1
! if(recursion.gt.3)then
! write(6,*)'* sit() cannot be called at recursion level ',recursion
! j_err=.true.
! goto 900
! endif !if(recursion.gt.3)then
! if(ivcursor(recursion).eq.0)call j_deftrans(0,'$Cursor$'//char(48+recursion), &
! ivcursor(recursion),200,0,0,iii,iii,iii,0)
 
! call j_startfunction(iob,io,0,narg,optarg_,ivout)
! inprint=j_v(j_ivprintinput)
 
! 1 inprint=j_v(j_ivprintinput)
! if(recursion.eq.1)then
! if(j_ndo.le.0.and.j_ninc.eq.1)write(6,*)' '
! call j_getinput('sit>',inprint)
! else !if(recursion.eq.1)then
! call j_getinput('sit'//char(48+recursion)//'>',inprint)
! endif !if(recursion.eq.1)then
! if(j_err.and..not.j_remain)goto 900
! !	write(6,*)'<431>recursion,',recursion,j_inp(1:j_linp)
! if(j_inp(1:j_linp).eq.'RETURN') return
! if(j_linp.eq.0.and.recursion.gt.1)goto 900
! if(j_inp(1:j_linp).eq.'end')call j_stopj()
! !	write(6,*)'<7337',j_inp(1:j_linp)
! iiv=j_inciv(j_ninc)
! !	write(6,*) '  after using  ', j_o(iiv)%i(6),' lines from ',j_vname(iiv)
! if(j_ninc.gt.1.and.j_v(j_ivdebugconsole).ne.j_0)&
! call j_pause('<c'//j_chi5(j_o(iiv)%i(6),0)//'>',do=.true.)
! if(.not.j_err)call j_interpret(j_inp(1:j_linp),ivcursor(recursion))
! !	write(6,*)'tas,recursion',recursion,j_err
! if(j_err)then
! call j_errexit()
! if(.not.j_remain)return
! if(recursion.gt.1)goto 900
! j_err=.false.
! goto 1
! endif !if(j_err)then
 
! j_njump=0
! call dotrans(ivcursor(recursion),1)
! !write(6,*)'<22sit, err',j_err
! if(j_err)then
! !	write(6,*)'<22sit, err,calling errexit',j_err,'recursion ',recursion
! call j_errexit()
! !			write(6,*)'<22sit, errAFT,calling errexit',j_err,'recursion ',recursion
! if(.not.j_remain)return
! if(recursion.gt.1)goto 900
! j_err=.false.
! endif !if(j_err)then
 
! goto 1
! !write(6,*)'<44>',recursion
! 900 recursion=recursion-1
 
! return
 
! end subroutine sit !recursive subroutine sit(iob,io)
 
subroutine bincode(iob,io)
	write(6,*)'current io ',io, ' in which ',j_o(iob)%i(io),' maxio ',j_o(iob)%i(0)
	write(6,'(20i5/)')j_o(iob)%i(1:j_o(iob)%i(0))
 
endsubroutine
 
subroutine which(iob,io)  !which()
	! Section which which() Value based on conditions
 
	!Usage:\\
 
	! output=which(condition1,value1,...,conditionn,valuen) \\
	! or\\
	! output=which(condition1,value1,...,conditionn,valuen,valuedefault)
	! Where conditionx is a REAL value, nonzero	value indicating TRUE. Output will get first value for which
	! the condition is TRUE. When the number of arguments is not even, the the last value
	! is the default value.
	! endheader
	! Ex whichex Example of which()
	! delete_o(a,c)
	!a,c=3,9
	! which(a.eq.3.or.c.gt.8,5,a.eq.7,55);
	! a=7
	! which(a.eq.3.or.c.gt.8,5,a.eq.7,55);
	! a=5
	! which(a.eq.3.or.c.gt.8,5,a.eq.7,55);
	! which(a.eq.3.or.c.gt.8,5,a.eq.7,55,108);
	! endex
	! endsection
	integer,intent(in)::iob
	integer,intent(in)::io
 
 
	narg=j_o(iob)%i(io+1)
	imod=mod(narg,2)
	irg2=0
	!	write(6,*)'which narg,imod',narg,imod
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	do i=1,narg-imod,2
		irg=j_o(iob)%i(io+1+i)
		if(j_otype(irg).ne.j_ipreal)then
			call j_getname(irg)
			write(6,*)'which() requires REAL (logical) arguments ',j_oname(1:j_loname), ' is ',j_otypes(j_otype(irg))
			j_err=.true.;return
		endif !if(j_otype(irg).ne.j_ipreal)   4004
		if(j_v(irg).ne.0.)then
			irg2=j_o(iob)%i(io+2+i)
			goto 77
 
		end if !if(j_v(irg).ne.0.)   4009
	end do !i=1,narg-imod,2   4002
	if(imod.ne.0)then
		irg2=j_o(iob)%i(io+1+narg)
		!	j_v(iout)=j_v(j_o(iob)%i(io+1+narg))
	else
		write(6,*)'no condition was satisfied and default value was not given'
		j_err=.true.;return
	endif !if(imod.ne.0)   4015
 77  if(j_otype(irg2).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_v(irg2)
	else
		call j_copy2(irg2,iout)
	endif !77  if(j_otype(irg2).eq.j_ipreal)   4022
	return
end subroutine which !subroutine which(iob,io)
 
subroutine errexit(iob,io)
	integer,intent(in)::iob
	integer,intent(in)::io
 
	!use j_globalfuncsmod	,only: printname
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	call j_printname('*errexit from transformation ',iob,' ')
	do j=1,narg
		iv=j_o(iob)%i(io+1+j)
		if(j_ipc(iv).ne.0)then
			call j_getchar(iv,j_cline,le);if(j_err)return
			write(6,*)j_cline(1:le)
		else !if(j_ipc(iv).ne.0)then
			write(6,*)j_v(iv)
		end if !if(j_ipc(iv).ne.0)   4042
	end do !j=1,narg   4040
	j_err=.true.
	return
end subroutine errexit !subroutine errexit(iob,io)
 
subroutine goto(iob,io)
 
	!	call j_getname(j_o(iob)%i(io+1))
	!	write(6,*)'var ',j_oname(1:j_loname)
	!	write(6,*)'goto',j_o(iob)%i(io:io+15)
	narg=j_o(iob)%i(io+1)
	!	write(6,*)'narg',narg,' io ',io
	if(narg.eq.0)then
 
		io=j_o(iob)%i(io+2)  !simple
		!	write(6,*)'uusio',io
		!	write(6,*)'uusio',io
		return
	endif !if(narg.eq.0)   4060
 
 
	iad=j_o(iob)%i(0)+2					!
	nlabel=j_o(iob)%i(iad)
	!	write(6,*)'iad',iad,nlabel
	!	write(6,*)'narg,iad,nlabel,labels ',narg,iad,nlabel,j_o(iob)%i(iad+1:iad+nlabel)
	!	write(6,*)io,'%',j_o(iob)%i(io+1:io+20)
	!select from all lables
	if(narg.eq.1)then
		igo=j_o(iob)%i(io+2)
 
		io=j_o(iob)%i(iad+igo)+1    !the basis is stored
		!	write(6,*)'uusio ',igo,io
		return
	endif !if(narg.eq.1)   4075
 
 
	igo=j_v(j_o(iob)%i(io+2)) !goto(igo,ad1,ad3,ad5
	!write(6,*)'igo ',igo
 
	if(igo.eq.0)then
		io=io+narg+3
		!	write(6,*)'uusio0 ',io
		return
	endif !if(igo.eq.0)   4087
	if(j_v(j_ivdebugtrans).ne.0)then
		call j_getcurline(iob,io)
		write(6,*)'goto3curl ',j_curline(j_recursion)
	endif !if(j_v(j_ivdebugtrans).ne.0)   4092
 
 
	if(igo.lt.0.or.igo.ge.narg)then
		write(6,*)'cannot goto to label ',igo, 'there are only ',narg-1,' labels in goto()'
		j_err=.true.
		return
	endif !if(igo.lt.0.or.igo.ge.narg)   4098
 
	!	write(6,*)'igo,io+2,io+2+igo,igo2,iad+igo2',igo,io+2,io+2+igo,igo2,iad+igo2
	iad2=j_o(iob)%i(io+2+igo)
	!	write(6,*)'iad2 ',iad2
	io=j_o(iob)%i(iad+iad2)+1
	!	write(6,*)'iad2',iad2,io
	if(j_v(j_ivdebugtrans).ne.0)call j_getcurline(iob,io)
	!	write(6,*)'goto3curl ',j_curline(j_recursion)
 
	!	write(6,*)'uusio',io,j_o(iob)%i(io)
	return
 
end subroutine !subroutine goto(iob,io)
 
subroutine goto2(iob,io) !select from all labels
	integer,intent(in)::iob
	integer,intent(inout)::io
	write(6,'(a,(20i5/))')'<fingoto2>',j_o(iob)%i(0:j_o(iob)%i(0)+7)
	!select from argument list
	write(6,*)'io ',io, 'locio ',j_o(iob)%i(io)
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+1+narg)
	igo=j_v(iout)
	write(6,*)'goto2 narg,iout,igo ',narg,iout,igo
	if(igo.eq.0)then
		io=io+narg+2
	endif !if(igo.eq.0)   4127
	if(igo.lt.0.or.igo.gt.narg)then
		call j_getname(iout)
		write(6,*)j_oname(1:j_loname),' has illegal value ',igo,&
			' in goto(), should be between 1 and ',narg
		j_err=.true.
		return
	endif !if(igo.lt.0.or.igo.gt.narg)   4130
 
	io=j_o(iob)%i(io+igo+1)
	if(j_v(j_ivdebugtrans).ne.0)call j_getcurline(iob,io)
	!	write(6,*)'goto2 to line ',j_curline(j_recursion), ' new io ',io
 
 
	return
 
end subroutine !subroutine goto(iob,io)
 
subroutine goto3(iob,io)
	!write(6,'(a,(20i5/))')'<fingoto3  ',j_o(iob)%i(1:j_o(iob)%i(0)+7)
 
 
end subroutine !subroutine goto3(iob,io)
 
 
 
subroutine IPOWER(iob,io)
	narg=j_o(iob)%i(io+1)
	ipow=j_v( j_o(iob)%i(io+3))  !second argument
	j_v( j_o(iob)%i(io+narg+2))= j_v( j_o(iob)%i(io+2))**ipow
	return
 
end subroutine IPOWER !subroutine IPOWER(iob,io)
 
subroutine POWER(iob,io,ioper)
	integer,intent(in)::iob,io
	double precision minva,pow
	integer::ipow
	logical ipower
	ipower=ioper.eq.1
 
	narg=j_o(iob)%i(io+1)
 
	irg=j_o(iob)%i(io+2)
	pow=j_v( j_o(iob)%i(io+3))  !second argument
	if(ipower)ipow=pow
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		if(ipower)then
			j_v(iout)=j_v(irg)**ipow
		else !if(ipower)then
			if(j_v(irg).lt.j_0)then
				write(6,*)'negative argument in ** ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).lt.j_0)then
				j_v(iout)=j_v(irg)**pow
			endif !if(j_v(irg).lt.j_0)   4181
		endif !if(ipower)   4178
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
 
 
		!if(minva.lt.j_0)then
		nrow=j_o(irg)%i(1)
		ncol=j_o(irg)%i(2)
		if(j_otype(iout).ne.j_ipmatrix)iout=j_defmatrix(iout,' ',int8(nrow),int8(ncol),j_matreg)
		if(j_o(iout)%i(3).ne.j_o(irg)%i(3))then
			ivout=j_defmatrix(iout,' ',int8(nrow),int8(ncol),j_matreg)
		else !if(j_o(iout)%i(3).ne.j_o(irg)%i(3))then
			j_o(iout)%i(1)=nrow
			j_o(iout)%i(2)=ncol
		endif !if(j_o(iout)%i(3).ne.j_o(irg)%i(3))   4195
		if(ipower)then
			j_o(iout)%d=	j_o(irg)%d**ipow
		else !if(ipower)then
			minva=minval(j_o(irg)%d)
			if(minva.lt.j_0)then
				write(6,*)'negative elements,min = ',minva
				j_err=.true.;return
			endif !if(minva.lt.j_0)   4205
			j_o(iout)%d=	j_o(irg)%d**pow
		endif !if(ipower)   4201
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   4176
 
	return
 
end subroutine POWER !subroutine POWER(iob,io,ioper)
 
subroutine HMULT(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg1).ne.j_ipmatrix.or.j_otype(irg2).ne.j_ipmatrix)then
		write(6,*)' *.  requires matrix arguments'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix.or.j_otype(irg2).ne.j_ipmat   4225
	nrow=j_o(irg1)%i(1)
	ncol=j_o(irg1)%i(2)
	nel=j_o(irg1)%i(3)
	if(nrow.ne.j_o(irg2)%i(1).or.ncol.ne.j_o(irg2)%i(2))then
		write(6,*)'arguments of *.  are not compatible'
		j_err=.true.;return
	endif !if(nrow.ne.j_o(irg2)%i(1).or.ncol.ne.j_o(irg2)%i(2))   4232
	if(irg1.ne.iout.and.irg2.ne.iout)ivout= j_defmatrix(iout,' ',int8(nrow),int8(ncol),j_matreg)
	do i=1,nel
		j_o(iout)%d(i)=j_o(irg1)%d(i)*j_o(irg2)%d(i)
	enddo !i=1,nel   4237
	return
 
end subroutine HMULT !subroutine HMULT(iob,io)
 
subroutine HDIV(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+2+narg)
	write(6,*)'ir1,irg2',irg1,irg2
	if(j_otype(irg1).ne.j_ipmatrix.or.j_otype(irg2).ne.j_ipmatrix)then
		write(6,*)' *.  requires matrix arguments'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix.or.j_otype(irg2).ne.j_ipmat   4250
	nrow=j_o(irg1)%i(1)
	ncol=j_o(irg1)%i(2)
	nel=j_o(irg1)%i(3)
	if(nrow.ne.j_o(irg2)%i(1).or.ncol.ne.j_o(irg2)%i(2))then
		write(6,*)'arguments of *.  are not compatible'
		j_err=.true.;return
	endif !if(nrow.ne.j_o(irg2)%i(1).or.ncol.ne.j_o(irg2)%i(2))   4257
	if(irg1.ne.iout.and.irg2.ne.iout)ivout=j_defmatrix(iout,' ',int8(nrow),int8(ncol),j_matreg)
	do i=1,nel
		if(j_o(irg2)%d(i).eq.j_0)then
			write(6,*)' /. :element ',i, 'is zero'
			j_err=.true.;return
 
		endif !if(j_o(irg2)%d(i).eq.j_0)   4263
		j_o(iout)%d(i)=j_o(irg1)%d(i)/j_o(irg2)%d(i)
	enddo !i=1,nel   4262
	return
 
end subroutine HDIV !subroutine HDIV(iob,io)
 
 
subroutine MULT(iob,io) ! mata*matb
	double precision,dimension(:),allocatable ::temp
	integer, dimension (:), pointer :: irg
	!	integer, dimension (:), pointer :: irg
	!	integer irg(2)
	logical ismatrix(2),islist,matrixout
	integer*8 nrow(2),ncol(2),nel,i
	logical ::p=.false.
	!	write(6,*)'MULTio ',j_o(iob)%i(1: j_o(iob)%i(1))
	!write(6,*)'io ',io
	!	call  j_startfunction(iob,io,j_ipreal,narg,arg,ivout,iptype2=j_ipmatrix)
	call  j_startfunction(iob,io,0,narg,irg,iout);if(j_err)return
	!	write(6,*)'irg',irg,j_otype(irg)
	!write(6,*)'<443irg1irg2iout',irg(1),irg(2),iout,j_otype(irg(1)),j_otype(irg(2))
 
	if(j_otype(irg(1)).eq.j_ipreal.and.j_otype(irg(2)).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_v(irg(1))*j_v(irg(2))  !two scalars as fast as possible
		return
	endif !if(j_otype(irg(1)).eq.j_ipreal.and.j_otype(irg(2)).eq.j_ip   4290
	matrixout=j_otype(iout).eq.j_ipmatrix
	!	write(6,*)'multi'
	ireal=0
	imatrix=0
	do i=1,2
		select case(j_otype(irg(i)))
		case(j_ipreal) !select case(j_otype(irg(i)))
		ismatrix(i)=.false.
		ireal=i
		case(j_iplist) !select case(j_otype(irg(i)))
		nrow(i)=j_nrows(irg(i))  !j_o(irg(i))%i(1)
		ncol(i)=j_18
		j_o(irg(i))%d(1:nrow(i))=j_v(j_o(irg(i))%i2(1:nrow(i)))
		ismatrix(i)=.true.
		imatrix=i
		case(j_ipmatrix) !select case(j_otype(irg(i)))
		ismatrix(i)=.true.
		imatrix=i
		nrow(i)=j_nrows(irg(i))  !j_o(irg(i))%i(1)
		ncol(i)=j_ncols(irg(i))  !j_o(irg(i))%i(2)
		! case(j_iplist)
		! if(i.eq.2)then
		! islist=.true.
		! nrow(2)=j_o(irg(2))%i(1)
		! ncol(2)=1
		! else
		! call j_getname(irg(1))
		! write(6,*)'list ',j_oname(1:j_loname),' can be only second argument in MULT'
		! j_err=.true.;return
		! endif !if(i.eq.2)   4404
		case default !select case(j_otype(irg(i)))
		call j_printname('argument ',irg(i),' of MULT is not MATRIX or REAL or LIST')
		j_err=.true.;return
		end select !select case(j_otype(irg(i)))
	enddo !i=1,2   4299
	if(p)write(6,*)'ismatrix ',ismatrix, ' nrow ',nrow,' ncol ',ncol
 
	if(ireal.ne.0.and.imatrix.ne.0)then
		ivmatrix=irg(imatrix)
		ivreal=irg(ireal)
		nel=j_nelem(ivmatrix)  !j_o(irg(imatrix))%i(3)
		if(j_otype(iout).eq.j_iplist)then
 
			if(j_o(iout)%i(1).ne.nel)goto 99
			do i=1,nel
				iv=j_o(iout)%i2(i)
				if(j_otype(iv).ne.j_ipreal)call j_del(iv)
				j_v(iv)=j_v(ivreal)*j_o(ivmatrix)%d(i)
 
			enddo !i=1,nel   4339
			return
		endif !if(j_otype(iout).eq.j_iplist)   4336
		if(j_otype(iout).eq.j_ipmatrix)then
			if(j_o(iout)%i(1).eq.nrow(imatrix).and.j_o(iout)%i(2).eq.ncol(imatrix))then
				j_o(iout)%d(1:nel)=j_v(ivreal)*j_o(ivmatrix)%d(1:nel)
				return
			endif !if(j_o(iout)%i(1).eq.nrow(imatrix).and.j_o(iout)%i(2).eq.n   4348
 
		endif !if(j_otype(iout).eq.j_ipmatrix)   4347
		ivout=j_defmatrix(iout,' ',nrow(imatrix),ncol(imatrix),j_matreg)
		j_o(iout)%d=j_v(ivreal)*j_o(ivmatrix)%d(1:nel)
		return
 
 
	endif !if(ireal.ne.0.and.imatrix.ne.0)   4332
 
	!both are matrices
	if(ncol(1).ne.nrow(2))goto 99
	if(nrow(1).eq.1.and.ncol(2).eq.1)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dot_product(j_o(irg(1))%d(1:ncol(1)),j_o(irg(2))%d(1:ncol(1)))
		return
	endif !if(nrow(1).eq.1.and.ncol(2).eq.1)   4363
	nel=nrow(1)*ncol(2)
	! j_o(iout)%d=reshape(matmul(reshape(j_o(irg(2))%d,(/ndim2(2),ndim1(2)/)),&
	! reshape(j_o(irg(1))%d,(/ndim2(1),ndim1(1)/) ) ), (/nel/) )
	if(nrow(1).eq.ncol(1).and.iout.eq.irg(1).or.iout.eq.irg(2))then
		allocate(temp(1:nrow(1)*nrow(1)))
		temp=reshape(matmul(reshape(j_o(irg(2))%d,(/ncol(2),nrow(2)/)),&
			reshape(j_o(irg(1))%d,(/ncol(1),nrow(1)/) ) ), (/nel/) )
		j_o(iout)%d=temp
		deallocate(temp)
	else !if(nrow(1).eq.ncol(1).and.iout.eq.irg(1).or.iout.eq.irg(2))then
		ivout=j_defmatrix(iout,' ',nrow(1),ncol(2),j_matreg)
		j_o(iout)%d=reshape(matmul(reshape(j_o(irg(2))%d,(/ncol(2),nrow(2)/)),&
			reshape(j_o(irg(1))%d,(/ncol(1),nrow(1)/) ) ), (/nel/) )
 
	endif !if(nrow(1).eq.ncol(1).and.iout.eq.irg(1).or.iout.eq.irg(2)   4371
	return
 
 
 
99		call j_getname(irg(1),irg(2))

	write(6,*)'dimensions do not agree in MULT ',j_oname(1:j_loname),' is ',&
		nrow(1),ncol(1),j_oname2(1:j_loname2),' is ',nrow(2),ncol(2)
	write(6,*)'imatrix',imatrix,'ireal',ireal
	j_err=.true.
	return
end subroutine MULT !subroutine MULT(iob,io)
 
 
subroutine DIV(iob,io)
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	! write(6,*)'type ',irg1,irg2,j_otype(irg1),j_otype(irg2)
	! write(6,'(20i5/)')j_o(iob)%i(1:j_o(iob)%i(1))
	! write(6,*)'veet',j_v(irg1),j_v(irg2)
	if(j_otype(irg2).ne.j_ipreal)then
		write(6,*)'*denominator not real variable'
		j_err=.true.
		return
	endif !if(j_otype(irg2).ne.j_ipreal)   4405
	if(j_v(irg2).eq.j_0)then
		write(6,*)'*division by zero'
		j_err=.true.
		return
	endif !if(j_v(irg2).eq.j_0)   4410
	if(j_otype(irg1).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_v(irg1)/j_v(irg2)
 
	elseif(j_otype(irg1).eq.j_ipmatrix)then !if(j_otype(irg1).eq.j_ipreal)then
		if(iout.ne.irg1)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			allocate( j_o(iout)%i(1:13))
			allocate(j_o(iout)%d(1:j_o(irg1)%i(3)))
			j_otype(iout)=j_ipmatrix
			j_o(iout)%i=j_o(irg1)%i
			j_o(iout)%d=j_o(irg1)%d/j_v(irg2)
		else !if(iout.ne.irg1)then
			j_o(iout)%d=j_o(irg1)%d/j_v(irg2)
		endif !if(iout.ne.irg1)   4420
	else !if(j_otype(irg1).eq.j_ipreal)then
		write(6,*)'*numerator is ',j_otypes(j_otype(irg1)),' not REAL or MATRIX'
		j_err=.true.
 
	end if !if(j_otype(irg1).eq.j_ipreal)   4415
	return
 
end subroutine DIV !subroutine DIV(iob,io)
 
!binop(  ioper 1=plus 2=minus 3=eq  4=ne 5 =le 6=lt 7 =ge 8 =gt 10 =and 11=or 12=eqv 13=neqv 14=hmult 15 hdiv
subroutine BINOP(iob,io,ioper) ! mata+matb !
	integer, dimension (:), pointer :: irg
	double precision::diout
	logical listout
	!	integer, dimension (:), pointer :: irg
	!	integer irg(2)
	!	logical ismatrix(2),is
	integer*8 nrow(2),ncol(2),nco,nro,ibas,nrowout,ncolout,nelem(2),iel,nel
	integer*8::colbas,colbasstep,colstep(2),rowstep(2)
	logical::scalar(2),vector(2),matrix(2),list(2),iseq
	character*6,dimension(19)::opname
	data opname/'+','?','.eq.','.ne.','.le.','.lt.','ge','.gt.','?','.and.',&
		'.or','.eqv.','.neqv','*.','/.','min','max','sign','mod'/
	!	call  j_startfunction(iob,io,j_ipreal,narg,arg,ivout,iptype2=j_ipmatrix)
	call  j_startfunction(iob,io,0,narg,irg,iout);if(j_err)return
 
	!	write(6,*)'binop ',ioper,opname(ioper)
	!j_p=ioper.eq.14
	! if(j_p)then
	! call j_getname(iout,irg(1),irg(2))
	! write(6,*)j_oname(1:j_loname),'=',j_oname2(1:j_loname2),'*.',j_oname3(1:j_loname3)
	! endif !if(j_p)   4316
	if(narg.eq.1)then
		if(j_otype(irg(1)).eq.j_ipmatrix)then
 
			if(ioper.ne.16.and.ioper.ne.17)then
				!if(ioper.eq.16)then
				write(6,*)'illegal operation with one argument'
				j_err=.true.
				return
 
			endif !if(ioper.ne.16.and.ioper.ne.17)   4465
			nco=j_ncols(irg(1)) !j_o(irg(1))%i(2)
			nro=j_nrows(irg(1))  !j_o(irg(1))%i(1)
			if(j_err)return
			if(nco.eq.j_18.or.nro.eq.j_18.or.j_linkoption(iob,io,j_many,clear=.true.).ge.0)then
				if(j_otype(iout).eq.j_ipmatrix.and.iout.le.j_named)then
					if(ioper.eq.16)then
 
						j_o(iout)%d=minval(j_o(irg(1))%d)
					elseif(ioper.eq.17)then !if(ioper.eq.16)then
						j_o(iout)%d=maxval(j_o(irg(1))%d)
 
					endif !if(ioper.eq.16)   4477
 
				else
					if(j_otype(iout).ne.j_ipreal)call j_del(iout)
					!	write(6,*)'<65',iout,j_otype(iout)
					if(ioper.eq.16)then
 
						j_v(iout)=minval(j_o(irg(1))%d)
					elseif(ioper.eq.17)then !if(ioper.eq.16)then
						j_v(iout)=maxval(j_o(irg(1))%d)
 
					endif !if(ioper.eq.16)   4488
				endif !if(j_otype(iout).eq.j_ipmatrix.and.iout.le.j_named)   4476
			else
				iout=j_defmatrix(iout,' ',j_18,nco,j_matreg)
				ibas=0
				if(ioper.eq.16)then
					j_o(iout)%d(1:nco)=j_inf
					do i=1,nro
						do j=1,nco
							j_o(iout)%d(j)=min(j_o(iout)%d(j),j_o(irg(1))%d(ibas+j))
						enddo !j=1,nco   4502
						ibas=ibas+nco
					enddo !i=1,nro   4501
				else
 
					j_o(iout)%d(1:nco)=j_ninf
					do i=1,nro
						do j=1,nco
							j_o(iout)%d(j)=max(j_o(iout)%d(j),j_o(irg(1))%d(ibas+j))
						enddo !j=1,nco   4511
						ibas=ibas+nco
					enddo !i=1,nro   4510
				endif !if(ioper.eq.16)   4499
			endif !if(nco.eq.j_18.or.nro.eq.j_18.or.j_linkoption(iob,io,j_man   4475
 
 
		endif !if(j_otype(irg(1)).eq.j_ipmatrix)   4463
		return
	endif !if(narg.eq.1)   4462
	!	narg=j_o(iob)%i(io+1)
 
	!	irg=j_o(iob)%i(io+2:io+3)
	!	iout=j_o(iob)%i(io+narg+2)
	!	write(6,*)'binop ',ioper,irg,j_otype(irg(1)),j_otype(irg(2))
	!write(6,*)'<7373',narg,irg
	!ordinary summation as fast as possibleselect case
	if(j_otype(irg(1)).eq.j_ipreal.and.j_otype(irg(2)).eq.j_ipreal)then
		if(j_otype(iout).eq.j_ipmatrix.and.iout.le.j_named.and.(ioper.lt.3.or.ioper.gt.13))then
			select case (ioper)
			case(1) !select case (ioper)
			!	write(6,*)'binoplus',j_v(irg(1)),j_v(irg(2))
			j_o(iout)%d=j_v(irg(1))+j_v(irg(2))
			! call j_getname(iout,irg(1),irg(2))
			! write(18,*)'sum ',j_oname(1:j_loname),j_v(iout),j_oname2(1:j_loname2),j_v(irg(1)),&
			! j_oname3(1:j_loname3),j_v(irg(2))
			case (2) !select case (ioper)
			j_o(iout)%d=j_v(irg(1))-j_v(irg(2))
			! case (3) !select case (ioper)
			! j_v(iout)=j_0
			! if(j_v(irg(1)).eq.j_v(irg(2)))j_v(iout)=j_1
			! ! call j_getname(iout,irg(1),irg(2))
			! ! write(18,*)'eq ',j_oname(1:j_loname),j_v(iout),j_oname2(1:j_loname2),j_v(irg(1)),&
			! ! j_oname3(1:j_loname3),j_v(irg(2))
			! case( 4) !select case (ioper)
			! j_v(iout)=j_0
			! if(j_v(irg(1)).ne.j_v(irg(2)))j_v(iout)=j_1
			! case (5) !select case (ioper)
			! j_v(iout)=j_0
			! if(j_v(irg(1)).le.j_v(irg(2)))j_v(iout)=j_1
			! case (6) !select case (ioper)
			! j_v(iout)=j_0
			! if(j_v(irg(1)).lt.j_v(irg(2)))j_v(iout)=j_1
			! !	call j_getname(irg(1),irg(2),iout)
			! !	write(6,*)irg(1),irg(2),iout,j_oname(1:j_loname+1),j_oname2(1:j_loname2+1),j_oname3(1:j_loname3+1)
			! !	write(6,*)j_v(irg(1)),j_v(irg(2)),j_v(iout)
			! case (7) !select case (ioper)
			! j_v(iout)=j_0
			! if(j_v(irg(1)).ge.j_v(irg(2)))j_v(iout)=j_1
			! case( 8) !select case (ioper)
			! j_v(iout)=j_0
			! if(j_v(irg(1)).gt.j_v(irg(2)))j_v(iout)=j_1
			! !	case 9 not
 
			! case (10) !select case (ioper)
			! j_v(iout)=j_0
			! if(j_v(irg(1)).ne.j_0.and.j_v(irg(2)).ne.j_0)j_v(iout)=j_1
 
			! case(11) !select case (ioper)
			! !		write(6,*)'OR ',io,j_o(iob)%i(io-10:io),'*',j_o(iob)%i(io:io+10)
			! j_v(iout)=j_0
			! if(j_v(irg(1)).ne.j_0.or.j_v(irg(2)).ne.j_0)j_v(iout)=j_1
 
 
			! case(12) !select case (ioper)
			! j_v(iout)=j_0
			! if(j_v(irg(1)).ne.j_0.and.j_v(irg(2)).ne.j_0.or. &
			! j_v(irg(1)).eq.j_0.and.j_v(irg(2)).eq.j_0)j_v(iout)=j_1
 
			! case(13) !select case (ioper)
			! j_v(iout)=j_0
			! if(.not.(j_v(irg(1)).ne.j_0.and.j_v(irg(2)).ne.j_0.or. &
			! j_v(irg(1)).eq.j_0.and.j_v(irg(2)).eq.j_0))j_v(iout)=j_1
			case (14 ) !select case (ioper)
			j_o(iout)%d=j_v(irg(1))*j_v(irg(2))
			case( 15) !select case (ioper)
			if(j_v(irg(2)).eq.j_0)then
				write(6,*)'division with zero'
				j_err=.true.
			else !if(j_v(irg(2)).eq.j_0)then
				j_o(iout)%d=j_v(irg(1))/j_v(irg(2))
			endif !if(j_v(irg(2)).eq.j_0)   4589
			case (16) !select case (ioper)
			j_o(iout)%d=min(j_v(irg(1)),j_v(irg(2)))
			case (17) !select case (ioper)
			j_o(iout)%d=max(j_v(irg(1)),j_v(irg(2)))
			case (18) !select case (ioper)
			j_o(iout)%d=dsign(j_v(irg1),j_v(irg2))
			case (19) !select case (ioper)
			ir1=j_v(irg(1))
			ir2=j_v(irg(2))
			j_o(iout)%d=mod(ir1,ir2)
 
			end select !select case (ioper)
 
 
		else
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			select case (ioper)
			case(1) !select case (ioper)
			!	write(6,*)'binoplus',j_v(irg(1)),j_v(irg(2))
			j_v(iout)=j_v(irg(1))+j_v(irg(2))
			! call j_getname(iout,irg(1),irg(2))
			! write(18,*)'sum ',j_oname(1:j_loname),j_v(iout),j_oname2(1:j_loname2),j_v(irg(1)),&
			! j_oname3(1:j_loname3),j_v(irg(2))
			case (2) !select case (ioper)
			j_v(iout)=j_v(irg(1))-j_v(irg(2))
			case (3) !select case (ioper)
			j_v(iout)=j_0
			if(j_v(irg(1)).eq.j_v(irg(2)))j_v(iout)=j_1
			! call j_getname(iout,irg(1),irg(2))
			! write(18,*)'eq ',j_oname(1:j_loname),j_v(iout),j_oname2(1:j_loname2),j_v(irg(1)),&
			! j_oname3(1:j_loname3),j_v(irg(2))
			case( 4) !select case (ioper)
			j_v(iout)=j_0
			if(j_v(irg(1)).ne.j_v(irg(2)))j_v(iout)=j_1
			case (5) !select case (ioper)
			j_v(iout)=j_0
			if(j_v(irg(1)).le.j_v(irg(2)))j_v(iout)=j_1
			case (6) !select case (ioper)
			j_v(iout)=j_0
			if(j_v(irg(1)).lt.j_v(irg(2)))j_v(iout)=j_1
			!	call j_getname(irg(1),irg(2),iout)
			!	write(6,*)irg(1),irg(2),iout,j_oname(1:j_loname+1),j_oname2(1:j_loname2+1),j_oname3(1:j_loname3+1)
			!	write(6,*)j_v(irg(1)),j_v(irg(2)),j_v(iout)
			case (7) !select case (ioper)
			j_v(iout)=j_0
			if(j_v(irg(1)).ge.j_v(irg(2)))j_v(iout)=j_1
			case( 8) !select case (ioper)
			j_v(iout)=j_0
			if(j_v(irg(1)).gt.j_v(irg(2)))j_v(iout)=j_1
			!	case 9 not
 
			case (10) !select case (ioper)
			j_v(iout)=j_0
			if(j_v(irg(1)).ne.j_0.and.j_v(irg(2)).ne.j_0)j_v(iout)=j_1
 
			case(11) !select case (ioper)
			!		write(6,*)'OR ',io,j_o(iob)%i(io-10:io),'*',j_o(iob)%i(io:io+10)
			j_v(iout)=j_0
			if(j_v(irg(1)).ne.j_0.or.j_v(irg(2)).ne.j_0)j_v(iout)=j_1
 
 
			case(12) !select case (ioper)
			j_v(iout)=j_0
			if(j_v(irg(1)).ne.j_0.and.j_v(irg(2)).ne.j_0.or. &
				j_v(irg(1)).eq.j_0.and.j_v(irg(2)).eq.j_0)j_v(iout)=j_1
 
			case(13) !select case (ioper)
			j_v(iout)=j_0
			if(.not.(j_v(irg(1)).ne.j_0.and.j_v(irg(2)).ne.j_0.or. &
				j_v(irg(1)).eq.j_0.and.j_v(irg(2)).eq.j_0))j_v(iout)=j_1
			case (14 ) !select case (ioper)
			j_v(iout)=j_v(irg(1))*j_v(irg(2))
			case( 15) !select case (ioper)
			if(j_v(irg(2)).eq.j_0)then
				write(6,*)'division with zero'
				j_err=.true.
			else !if(j_v(irg(2)).eq.j_0)then
				j_v(iout)=j_v(irg(1))/j_v(irg(2))
			endif !if(j_v(irg(2)).eq.j_0)   4668
			case (16) !select case (ioper)
			j_v(iout)=min(j_v(irg(1)),j_v(irg(2)))
			case (17) !select case (ioper)
			j_v(iout)=max(j_v(irg(1)),j_v(irg(2)))
			case (18) !select case (ioper)
			j_v(iout)=dsign(j_v(irg1),j_v(irg2))
			case (19) !select case (ioper)
			ir1=j_v(irg(1))
			ir2=j_v(irg(2))
			j_v(iout)=mod(ir1,ir2)
 
			end select !select case (ioper)
		endif !if(j_otype(iout).eq.j_ipmatrix.and.iout.le.j_named.and.(io   4531
		return
	endif !if(j_otype(irg(1)).eq.j_ipreal.and.j_otype(irg(2)).eq.j_ip   4530
 
	! return
 
	colstep=0
	rowstep=0
	scalar=.false.
	vector=.false.
	matrix=.false.
	list=.false.
	ivector=0
	imatrix=0
	iscalar=0
 
	do i=1,2
		!	write(6,*)'irg(i),',irg(i),j_otype(irg(i))
		select case(j_otype(irg(i)))
		case(j_ipreal) !select case(j_otype(irg(i)))
		!		ismatrix(i)=.false.
		!		ireal=irg(i)
		scalar(i)=.true.
 
		ivfake=irg(i)
		j_o(j_ivfakematrix)%d(1)=j_v(ivfake)
		iscalar=i
		!		irg(i)=j_ivfakematrix   !must be returned
		!		write(6,*)'i scaalr ',j_v(ivfake)
 
		nelem(i)=j_18
		nrow(i)=j_18
		ncol(i)=j_18
		case(j_iplist) !select case(j_otype(irg(i)))
		nrow(i)=j_o(irg(i))%i(1)
		ncol(i)=j_18
		nelem(i)=nrow(i)
		j_o(irg(i))%d(1:nrow(i))=j_v(j_o(irg(i))%i2(1:nrow(i)))
		rowstep(i)=1
		list(i)=.true.
		vector(i)=.true.
		ivector=i
		!			ismatrix(i)=.true.
		!		imatrix=i
		case(j_ipmatrix) !select case(j_otype(irg(i)))
 
		nrow(i)=j_nrows(irg(i))  !j_o(irg(i))%i(1)
		ncol(i)=j_ncols(irg(i))  !j_o(irg(i))%i(2)
		nelem(i)=j_nelem(irg(i))
		!	write(6,*)'iro',j_o(irg(i))%i(1),nrow(i),'co',j_o(irg(i))%i(2),ncol(i)
		if(nrow(i).eq.j_18)then !j_o(irg(i))%i(2).eq.1)then
			rowstep(i)=-ncol(i)
 
		endif !if(nrow(i).eq.j_18)   4736
 
		if(ncol(i).eq.j_18)then
			rowstep(i)=j_18
		else
			colstep(i)=j_18
		endif !if(ncol(i).eq.j_18)   4741
 
		if(nrow(i).eq.j_18.or.ncol(i).eq.j_18)then
			vector(i)=.true.
			ivector=i
		else !if(j_o(irg(i))%i(1).eq.1.or.j_o(irg(i))%i(2).eq.1)then
			matrix(i)=.true.
			imatrix=i
		endif !if(nrow(i).eq.j_18.or.ncol(i).eq.j_18)   4747
 
		!	imatrix=i
 
		case default !select case(j_otype(irg(i)))
		if(j_otype(irg(1)).eq.j_ipchar.and.j_otype(irg(2)).eq.j_ipchar.and.(ioper.eq. 3.or.ioper.eq.4))then
 
			iseq=j_o(irg(1))%i(2)-j_o(irg(1))%i(1).eq.j_o(irg(2))%i(2)-j_o(irg(2))%i(1)
			if(iseq)then
				le=j_o(irg(2))%i(1)
				do j=j_o(irg(1))%i(1),j_o(irg(1))%i(2)
 
					if(j_o(j_ivnames)%ch(j).ne.j_o(j_ivnames)%ch(le))then
						iseq=.false.
						exit
					endif !if(j_o(j_ivnames)%ch(j).ne.j_o(j_ivnames)%ch(le))   4765
					le=le+1
 
				enddo !j=j_o(irg(1))%i(1),j_o(irg(1))%i(2)   4763
			endif !if(iseq)   4761
			if(ioper.eq.3.and.iseq)then
 
				j_v(iout)=j_1
			else
				j_v(iout)=j_0
 
			endif !if(ioper.eq.3.and.iseq)   4773
			return
 
 
		endif !if(j_otype(irg(1)).eq.j_ipchar.and.j_otype(irg(2)).eq.j_ip   4758
		call j_getname(irg(i))
		write(6,*)'argument ',j_oname(1:j_loname),' of ',opname(ioper),' is not MATRIX or REAL or LIST'
		j_err=.true.;if(iscalar.ne.0)irg(iscalar)=ivfake;return
		end select !select case(j_otype(irg(i)))
	enddo !i=1,2   4702
 
	! if(j_p)write(6,*)'dkjdj ',ioper,',ncol ',ncol,' nrow ',nrow,'ivector',ivector,'imatrix',imatrix,'iscalar',iscalar
	!***********
	!call j_getname(irg(1),irg(2))
	!write(6,*)'hdiv',j_oname(1:j_loname),' ',j_oname2(1:j_loname)
	!write(6,*)'matrix',matrix,'imatrix',imatrix,'vector',vector,'scalr',scalar,&
	!	'nrow',nrow,'ncol',ncol
	!write(6,*)'iscalar,ivector,imatrix,ioper',iscalar,ivector,imatrix,ioper
	if(iscalar.gt.0.and.(ivector.gt.0.or.imatrix.gt.0))then
		irgma=irg(max(ivector,imatrix))
		ivs=irg(iscalar)
		nel=j_nelem(irgma)
		if(j_otype(iout).eq.j_ipmatrix)then
			!		write(6,*)'hep',irgma,j_ncols(irgma).ne.j_ncols(iout),j_nrows(irgma).ne.j_nrows(iout)
			if(j_ncols(irgma).ne.j_ncols(iout).or.j_nrows(irgma).ne.j_nrows(iout))ivout=&
				j_defmatrix(iout,' ',j_nrows(irgma),j_ncols(irgma),j_matreg)
		else
			ivout=j_defmatrix(iout,' ',j_nrows(irgma),j_ncols(irgma),j_matreg)
			! if(j_p)write(6,*)'ivout,iout,j_nrows(irgma),j_ncols(irgma)',ivout,iout,j_nrows(irgma),j_ncols(irgma)
 
		endif !if(j_otype(iout).eq.j_ipmatrix)   4801
		!	if(nel.ne.j_o(iout)%i(3))ivout=j_defmatrix(iout,' ',nrowout,ncolout,j_matreg)
		if(ioper.eq.1)then
			!		write(6,*)'heppp',irgma,irg(iscalar),iout,j_v(irg(iscalar)),'*',j_o(irgma)%d(1:j_nelem(irgma))
			j_o(iout)%d(1:nel)=j_o(irgma)%d(1:nel)+j_v(ivs)
			return
		elseif(ioper.eq.2)then
			if(iscalar.eq.1)then
				j_o(iout)%d(1:nel)=j_v(ivs)-j_o(irgma)%d(1:nel)
			else
				j_o(iout)%d(1:nel)=j_o(irgma)%d(1:nel)-j_v(ivs)
			endif !if(iscalar.eq.1)   4816
			return
		endif !if(ioper.eq.1)   4811
		!	write(6,*)'iophere',ioper
		select case (ioper)
		! case (1 ) !select case (ioper)
		! !	call j_getname(irg(1),irg(2))
		! !	write(6,*)j_oname(1:j_loname),' d',j_o(irg(1))%d,j_oname2(1:j_loname2),j_o(irg(2))%d
		! diout=j_o(irg(1))%d(iel)+j_o(irg(2))%d(iel2)
		! case (2) !select case (ioper)
		! diout=j_o(irg(1))%d(iel)-j_o(irg(2))%d(iel2)
		case (3) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(j_o(irgma)%d(iel).eq.j_v(ivs))diout=j_1
			j_o(iout)%d(iel)=diout
		enddo !iel=1,nel   4832
		case (4) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(j_o(irgma)%d(iel).ne.j_v(ivs))diout=j_1
			j_o(iout)%d(iel)=diout
		enddo !iel=1,nel   4838
		case ( 5) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(j_o(irgma)%d(iel).le.j_v(ivs))diout=j_1
			j_o(iout)%d(iel)=diout
		enddo !iel=1,nel   4844
		case (6) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(j_o(irgma)%d(iel).lt.j_v(ivs))diout=j_1
			j_o(iout)%d(iel)=diout
		enddo !iel=1,nel   4850
		case (7) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(j_o(irgma)%d(iel).ge.j_v(ivs))diout=j_1
			j_o(iout)%d(iel)=diout
		enddo !iel=1,nel   4856
		case (8) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(j_o(irgma)%d(iel).gt.j_v(ivs))diout=j_1
			j_o(iout)%d(iel)=diout
			!	case 9 not
		enddo !iel=1,nel   4862
		case (10) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(j_o(irgma)%d(iel).ne.j_0.and.j_v(ivs).ne.j_0)diout=j_1
			j_o(iout)%d(iel)=diout
		enddo !iel=1,nel   4869
		case (11) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(j_o(irgma)%d(iel).ne.j_0.or.j_v(ivs).ne.j_0)diout=j_1
			j_o(iout)%d(iel)=diout
		enddo !iel=1,nel   4875
 
		case (12) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(j_o(irgma)%d(iel).ne.j_0.and.j_v(ivs).ne.j_0.or.j_o(irgma)%d(iel).eq.j_0.and.&
				j_v(ivs).eq.j_0)diout=j_1
			j_o(iout)%d(iel)=diout
		enddo !iel=1,nel   4882
		case (13) !select case (ioper)
		do iel=1,nel
			diout=j_0
			if(.not.(j_o(irgma)%d(iel).ne.j_0.and.j_v(ivs).ne.j_0.or.&
				j_o(irgma)%d(iel).eq.j_0.and.j_v(ivs).eq.j_0))diout=j_1
 
		enddo !iel=1,nel   4889
		case (14) !select case (ioper)
		!	write(6,*)'nel',nel,irgma,ivs,j_v(ivs)
		do iel=1,nel
			j_o(iout)%d(iel)=j_o(irgma)%d(iel)*j_v(ivs)
		enddo !iel=1,nel   4897
		case (15) !select case (ioper)
		do iel=1,nel
			if(j_v(ivs).eq.j_0)then
				write(6,*)'division with zero'
				j_err=.true.;return
			else !if(j_v(ivs).eq.j_0)then
				j_o(iout)%d(iel)=j_o(irgma)%d(iel)/j_v(ivs)
			endif !if(j_v(ivs).eq.j_0)   4902
		enddo !iel=1,nel   4901
		case (16) !select case (ioper)
		do iel=1,nel
			j_o(iout)%d(iel)=min(j_o(irgma)%d(iel),j_v(ivs))
		enddo !iel=1,nel   4910
		case(17) !select case (ioper)
		do iel=1,nel
			j_o(iout)%d(iel)=max(j_o(irgma)%d(iel),j_v(ivs))
		enddo !iel=1,nel   4914
		case (18) !select case (ioper)
		do iel=1,nel
			j_o(iout)%d(iel)=dsign(j_o(irgma)%d(iel),j_v(ivs))
		enddo !iel=1,nel   4918
		end select !select case (ioper)
 
		! if(listout)then
		! if(j_otype(j_o(iout)%i2(ij)).ne.j_ipreal)call j_del(j_o(iout)%i2(ij))
		! j_v(j_o(iout)%i2(ij))=diout
		! else !if(listout)then
		! j_o(iout)%d(ij)=diout
		! endif !if(listout)   4855
		return
 
	endif !if(iscalar.gt.0.and.(ivector.gt.0.or.imatrix.gt.0))   4797
	!write(6,*)'nrow',nrow,'ncol',ncol,'imatrix,ivector,iscar,scalar',imatrix,ivector,iscalar,scalar
	if(iscalar.gt.0)irg(i)=j_ivfakematrix
 
	!either 1 or 2 is matrix
	!	if(j_v(j_ivdollar).eq.5.d0)write(6,*)'binop',vector,j_o(irg(1))%i(1:3),j_o(irg(2))%i(1:3)
	!write(6,*)
	! if(vector(1).and.vector(2).and.j_o(irg(1))%i(3).ne.j_o(irg(2))%i(3).or. &
	! matrix(1).and.matrix(2).and.(j_o(irg(1))%i(1).ne.j_o(irg(2))%i(1).or. &
	! j_o(irg(1))%i(2).ne.j_o(irg(2))%i(2)))goto 99
	!write(6,*)'vec',vector,'matr',matrix,'scaalr',scalar,'nrow ',nrow,'ncol ',ncol
	! if(vector(1).and.vector(2).and.nelem(1).ne.nelem(2).or. &
	! matrix(1).and.matrix(2).and.(nrow(1).ne.nrow(2).or. &
	! ncol(1).ne.ncol(2)))write(6,*)'vec',vector,nelem,matrix,nrow,ncol
	if(vector(1).and.vector(2).and.nelem(1).ne.nelem(2).or. &
		matrix(1).and.matrix(2).and.(nrow(1).ne.nrow(2).or. &
		ncol(1).ne.ncol(2)))goto 99
	!write(6,*)'vec',vector,'ncol',ncol,'nrow',nrow
	if(vector(1).and.vector(2))then
		ncolout=max(ncol(1),ncol(2))
		nrowout=min(nrow(1),nrow(2))
	else !if(vector(1).and.vector(2))then
		nrowout=max(nrow(1),nrow(2)) !j_o(irg(1))%i(1),j_o(irg(2))%i(1))
		ncolout=max(ncol(1),ncol(2))  !j_o(irg(1))%i(2),j_o(irg(2))%i(2))
	endif !if(vector(1).and.vector(2))   4949
	nel=max(nelem(1),nelem(2))  !j_o(irg(1))%i(3),j_o(irg(2))%i(3))
	! if(j_p)write(6,*)'ncolout,nrowout,nel',ncolout,nrowout,nel
	if(j_otype(iout).eq.j_iplist)then
		listout=.true.
		if(ncolout.ne.1.or.nrowout.ne.j_o(iout)%i(2))goto 99
	else !if(j_otype(iout).eq.j_iplist)then
		listout=.false.
	endif !if(j_otype(iout).eq.j_iplist)   4958
 
	if(j_otype(iout).ne.j_ipmatrix)then
		iout=j_defmatrix(iout,' ',nrowout,ncolout,j_matreg)
		! if(j_p)write(6,*)'nelldeml',j_o(iout)%i(1:3)
		!	if(nel.ne.j_o(iout)%i(3))ivout=j_defmatrix(iout,' ',nrowout,ncolout,j_matreg)
	elseif(nel.ne.j_nelem(iout))then
		iout=j_defmatrix(iout,' ',nrowout,ncolout,j_matreg)
		! if(j_p)write(6,*)'nellhhhhdeml',j_o(iout)%i(1:3)
	elseif(nrowout.ne.nrow(1))then
 
		write(6,*)'defm',ivout,nrowout,ncolout,iout
		call j_defmatdim(iout,nrowout,ncolout)
		! if(j_p)write(6,*)'nelddhgdgdgldeml',j_o(iout)%i(1:3)
	endif !if(j_otype(iout).ne.j_ipmatrix)   4965
	!	write(6,*)'iope ',ioper,matrix,vector,scalar
 
	!write(6,*)'if(matrix(1).and.matrix(2).or.vector(1).and.vector(2))',matrix(1).and.matrix(2).or.vector(1).and.vector(2)
	if(matrix(1).and.matrix(2).or.vector(1).and.vector(2))then
		select case (ioper)
		case (1 ) !select case (ioper)
		j_o(iout)%d=j_o(irg(1))%d+j_o(irg(2))%d
		case (2) !select case (ioper)
		j_o(iout)%d=j_o(irg(1))%d-j_o(irg(2))%d
		case (3) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).eq.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).eq.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).eq.j_o(irg(2))%d(iel))   4989
		enddo !iel=1,nel   4988
 
		case (4) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ne.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).ne.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ne.j_o(irg(2))%d(iel))   4998
		enddo !iel=1,nel   4997
 
		case ( 5) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).le.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).le.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).le.j_o(irg(2))%d(iel))   5007
		enddo !iel=1,nel   5006
 
		case (6) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).lt.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).lt.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).lt.j_o(irg(2))%d(iel))   5016
		enddo !iel=1,nel   5015
 
		case (7) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ge.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).ge.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ge.j_o(irg(2))%d(iel))   5025
		enddo !iel=1,nel   5024
 
		case (8) !select case (ioper)
		do iel=1,nel
			!	write(6,*)i,j_o(irg(1))%d(iel),'>',j_o(irg(2))%d(iel)
			if(j_o(irg(1))%d(iel).gt.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).gt.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).gt.j_o(irg(2))%d(iel))   5035
		enddo !iel=1,nel   5033
 
		case (10) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0)then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0)then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0   5044
		enddo !iel=1,nel   5043
 
		case (11) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ne.j_0.or.j_o(irg(2))%d(iel).ne.j_0)then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).ne.j_0.or.j_o(irg(2))%d(iel).ne.j_0)then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ne.j_0.or.j_o(irg(2))%d(iel).ne.j_0)   5053
		enddo !iel=1,nel   5052
 
		case (12) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0.or.j_o(irg(1))%d(iel).eq.j_0.and.&
					j_o(irg(2))%d(iel).eq.j_0)then
				j_o(iout)%d(iel)=j_1
			else !j_o(irg(2))%d(iel).eq.j_0)then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0   5062
		enddo !iel=1,nel   5061
 
 
		case (13) !select case (ioper)
		do iel=1,nel
			if(.not.(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0.or.j_o(irg(1))%d(iel).eq.j_0.and.&
					j_o(irg(2))%d(iel).eq.j_0))then
				j_o(iout)%d(iel)=j_1
			else !j_o(irg(2))%d(iel).eq.j_0))then
				j_o(iout)%d(iel)=j_0
			endif !if(.not.(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).   5073
		enddo !iel=1,nel   5072
 
		case (14 ) !select case (ioper)
		!	j_o(iout)%d=j_o(irg(1))%d*j_o(irg(2))%d
		nrow1=j_o(irg(1))%i(1)
		nco1=j_o(irg(1))%i(2)
		nrow2=j_o(irg(2))%i(1)
		nco2=j_o(irg(2))%i(2)
		!write(6,*)'nrow1,nco1,nrow2,nco2',nrow1,nco1,nrow2,nco2
		!	write(6,*)'hada,nrow1,ncol1,nrow2,nco2 ',nrow1,ncol1,nrow2,nco2
		if(nrow1.eq.nrow2.and.nco1.eq.nco2)then
			j_o(iout)%d(1:nel)=j_o(irg(1))%d(1:nel)*j_o(irg(2))%d(1:nel)
		elseif(nco2.eq.1)then
			iba=0
			do irow=1,j_o(irg(1))%i(1)
				j_o(iout)%d(iba+1:iba+nco1)=j_o(irg(1))%d(iba+1:iba+nco1)*j_o(irg(2))%d(irow)
			enddo !irow=1,j_o(irg(1))%i(1)   5093
		else
			write(6,*)'illegal dimensions in extended Hadamard product *.'
			j_err=.true.
			!		diout=j_o(irg(1))%d(iel)/j_v(ivs)
		endif !if(nrow1.eq.nrow2.and.nco1.eq.nco2)   5089
 
		case (15) !select case (ioper)
		if(any(j_o(irg(2))%d.eq.j_0))then
			write(6,*)'division with zero'
			j_err=.true.
		else !if(any(j_o(irg(2))%d.eq.j_0))then
 
			nrow1=j_o(irg(1))%i(1)
			nco1=j_o(irg(1))%i(2)
			nrow2=j_o(irg(2))%i(1)
			nco2=j_o(irg(2))%i(2)
			!	write(6,*)'nrow1,nco1,nrow2,nco2',nrow1,nco1,nrow2,nco2
			if(nrow1.eq.nrow2.and.nco1.eq.nco2)then
				j_o(iout)%d(1:nel)=j_o(irg(1))%d(1:nel)/j_o(irg(2))%d(1:nel)
			elseif(nco2.eq.1)then
				iba=0
				do irow=1,j_o(irg(1))%i(1)
					j_o(iout)%d(iba+1:iba+nco1)=j_o(irg(1))%d(iba+1:iba+nco1)/j_o(irg(2))%d(irow)
				enddo !irow=1,j_o(irg(1))%i(1)   5117
			else
				write(6,*)'illegal dimensions in extended Hadamard division /.'
				j_err=.true.
				!		diout=j_o(irg(1))%d(iel)/j_v(ivs)
			endif !if(nrow1.eq.nrow2.and.nco1.eq.nco2)   5113
		endif !if(any(j_o(irg(2))%d.eq.j_0))   5103
		case (16) !select case (ioper)
		do iel=1,nel
			j_o(iout)%d(iel)=min(j_o(irg(1))%d(iel),j_o(irg(2))%d(iel))
		enddo !iel=1,nel   5127
		case(17) !select case (ioper)
		do iel=1,nel
			j_o(iout)%d(iel)=max(j_o(irg(1))%d(iel),j_o(irg(2))%d(iel))
		enddo !iel=1,nel   5131
 
 
		case (18) !select case (ioper)
		do iel=1,nel
			j_o(iout)%d(iel)=dsign(j_o(irg(1))%d(iel),j_o(irg(2))%d(iel))
		enddo !iel=1,nel   5137
 
		end select !select case (ioper)
		! if(listout)then
		! if(j_otype(j_o(iout)%i2(ij)).ne.j_ipreal)call j_del(j_o(iout)%i2(ij))
		! j_v(j_o(iout)%i2(ij))=diout
		! else
		! j_o(iout)%d(ij)=diout
		! endif
		if(iscalar.ne.0)irg(iscalar)=ivfake
		! if(j_p)write(6,*)'outdim',j_o(iout)%i(1:3)
		return
 
	endif !if(matrix(1).and.matrix(2).or.vector(1).and.vector(2))   4981
	! write(16,*)'ioper,nrowout,ncolout,nel,irg(1),irg(2)',ioper,nrowout,ncolout,nel,irg(1),irg(2),&
	!j_o(irg(1))%i,j_o(irg(2))%i
 
	if(ivector.gt.0.and.imatrix.gt.0)then
		!	write(6,*)'ivector,imatrix,ncol(ivector),ncol(imatrix),nrow(ivector),nrow(imatrix)',&
		!ivector,imatrix,ncol(ivector),ncol(imatrix),nrow(ivector),nrow(imatrix)
		if(ncol(ivector).ne.ncol(imatrix).and.nrow(ivector).ne.nrow(imatrix))goto 99
	endif !if(ivector.gt.0.and.imatrix.gt.0)   5156
	!	write(6,*)'rowstep,colstep',rowstep,colstep
	ij=1
	iel=1
	iel2=1
	!write(6,*)'colstep ',colstep,'rowstep',rowstep,'nrowout,ncolout',nrowout,ncolout
	do i=1,nrowout
		do j=1,ncolout
			!		write(6,*)'i',i,'j',j,'iel ',iel,iel2
			select case (ioper)
			case (1 ) !select case (ioper)
			! !	call j_getname(irg(1),irg(2))
			! !	write(6,*)j_oname(1:j_loname),' d',j_o(irg(1))%d,j_oname2(1:j_loname2),j_o(irg(2))%d
			diout=j_o(irg(1))%d(iel)+j_o(irg(2))%d(iel2)
			case (2) !select case (ioper)
			!	call j_getname(irg(1),irg(2))
			!	write(6,*)j_oname(1:j_loname+1),j_oname2(1:j_loname2),ivector,imatrix
			diout=j_o(irg(1))%d(iel)-j_o(irg(2))%d(iel2)
			case (3) !select case (ioper)
			diout=j_0
			if(j_o(irg(1))%d(iel).eq.j_o(irg(2))%d(iel2))diout=j_1
			case (4) !select case (ioper)
			diout=j_0
			if(j_o(irg(1))%d(iel).ne.j_o(irg(2))%d(iel2))diout=j_1
			case ( 5) !select case (ioper)
			diout=j_0
			if(j_o(irg(1))%d(iel).le.j_o(irg(2))%d(iel2))diout=j_1
			case (6) !select case (ioper)
			diout=j_0
			if(j_o(irg(1))%d(iel).lt.j_o(irg(2))%d(iel2))diout=j_1
			case (7) !select case (ioper)
			diout=j_0
			if(j_o(irg(1))%d(iel).ge.j_o(irg(2))%d(iel2))diout=j_1
			case (8) !select case (ioper)
			diout=j_0
			if(j_o(irg(1))%d(iel).gt.j_o(irg(2))%d(iel2))diout=j_1
			!	case 9 not
 
			case (10) !select case (ioper)
			diout=j_0
			if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel2).ne.j_0)diout=j_1
 
			case (11) !select case (ioper)
			diout=j_0
			if(j_o(irg(1))%d(iel).ne.j_0.or.j_o(irg(2))%d(iel2).ne.j_0)diout=j_1
 
 
			case (12) !select case (ioper)
			diout=j_0
			if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel2).ne.j_0.or.j_o(irg(1))%d(iel).eq.j_0.and.&
				j_o(irg(2))%d(iel2).eq.j_0)diout=j_1
 
			case (13) !select case (ioper)
			diout=j_0
			if(.not.(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel2).ne.j_0.or.&
				j_o(irg(1))%d(iel).eq.j_0.and.j_o(irg(2))%d(iel2).eq.j_0))diout=j_1
			case (14 ) !select case (ioper)
 
 
			diout=j_o(irg(1))%d(iel)*j_o(irg(2))%d(iel2)
			! if(j_p)write(6,*)'i,j',i,j,'irg(1),irg(2),iel',irg(1),irg(2),iel,j_o(irg(1))%d(iel),j_o(irg(2))%d(iel2)
			case (15) !select case (ioper)
			if(j_o(irg(2))%d(iel2).eq.j_0)then
				write(6,*)'division with zero'
				j_err=.true.
			else !if(j_o(irg(2))%d(iel2).eq.j_0)then
				!		write(6,*)'hep,iel,iel2,',iel,iel2
				diout=j_o(irg(1))%d(iel)/j_o(irg(2))%d(iel2)
			endif !if(j_o(irg(2))%d(iel2).eq.j_0)   5222
			case (16) !select case (ioper)
			diout=min(j_o(irg(1))%d(iel),j_o(irg(2))%d(iel2))
			case(17) !select case (ioper)
			diout=max(j_o(irg(1))%d(iel),j_o(irg(2))%d(iel2))
 
			case (18) !select case (ioper)
			diout=dsign(j_o(irg(1))%d(iel),j_o(irg(2))%d(iel2))
			end select !select case (ioper)
			if(listout)then
				if(j_otype(j_o(iout)%i2(ij)).ne.j_ipreal)call j_del(j_o(iout)%i2(ij))
				j_v(j_o(iout)%i2(ij))=diout
			else !if(listout)then
				j_o(iout)%d(ij)=diout
			endif !if(listout)   5237
 
			ij=ij+1
			iel=iel+colstep(1)
			iel2=iel2+colstep(2)
			!	write(6,*)'i,j,ij,iel,iel2,',i,j,ij,iel,iel2,'colstep,rowstep',colstep,rowstep
 
 
		enddo !j=1,ncolout   5167
		iel=iel+rowstep(1)
 
		iel2=iel2+rowstep(2)
		!	write(6,*)'ROWi,j,ij,iel,iel2',i,j,ij,iel,iel2,',colstep,rowstep',colstep,rowstep
 
	enddo !i=1,nrowout   5166
	if(iscalar.ne.0)irg(iscalar)=ivfake
	!write(6,*)'scalr',iscalar
	! if(j_p)write(6,*)'outdim',j_o(iout)%i(1:3)
	return
 
 
99	call j_getname(irg(1),irg(2))
	write(6,*)'noncompatible dimensions of ',j_oname(1:j_loname),nrow(1),ncol(1),' and ',&
		j_oname2(1:j_loname2),nrow(2),ncol(2),' in oper',ioper
	j_err=.true.
	return
 
end subroutine BINOP !subroutine BINOP(iob,io,ioper)
 
subroutine MINUS(iob,io) ! mata+matb !
	integer, dimension (:), pointer :: irg
	!	integer, dimension (:), pointer :: irg
	!	integer irg(2)
	logical ismatrix(2)
	integer*8 nrow(2),ncol(2),nrow1,ncol1
 
	!	call  j_startfunction(iob,io,j_ipreal,narg,arg,ivout,iptype2=j_ipmatrix)
	call  j_startfunction(iob,io,0,narg,irg,iout);if(j_err)return
	!if(j_v(j_ivdollar2).eq.234.)write(6,*)'minus',narg,irg
	if(narg.ge.2)then
		call binop(iob,io,2)
		return
	endif !if(narg.ge.2)   5281
 
	!	if(narg.eq.1)then
 
	if(j_otype(irg(1)).eq.j_ipreal)then
		if(j_otype(iout).eq.j_ipmatrix.and.iout.le.j_named)then
			j_o(iout)%d=-j_v(irg(1))
			return
		else
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=-j_v(irg(1))
			return
		endif !if(j_otype(iout).eq.j_ipmatrix.and.iout.le.j_named)   5289
	endif !if(j_otype(irg(1)).eq.j_ipreal)   5288
 
 
	select case(j_otype(irg(1)))
	case(j_ipmatrix) !select case(j_otype(irg(1)))
	nrow1=j_nrows(irg(1))  !j_o(irg(1))%i(1)
	ncol1=j_ncols(irg(1))  !j_o(irg(1))%i(2)
 
	if(irg(1).ne.iout)iout=j_defmatrix(iout,' ',nrow1,ncol1,j_matreg)
	j_o(iout)%d=-j_o(irg(1))%d(1:j_nelem(iout))   !j_o(iout)%i(3))
 
	case(j_iplist) !select case(j_otype(irg(1)))
	nrow=j_nrows(irg(1))  !j_o(irg(1))%i(1)
	if(j_otype(iout).eq.j_iplist)then
		write(6,*)'output cannot be list in output=-list'
		j_err=.true.
		return
	endif !if(j_otype(iout).eq.j_iplist)   5310
	iout=j_defmatrix(iout,' ',nrow1,j_18,j_matreg)
	j_o(iout)%d=j_v(j_o(irg(1))%i2(1:nrow1))
 
	case default !select case(j_otype(irg(1)))
	call j_printname('**object ',irg(1),' has illegal type in -matrix')
	write(6,*)'#type is:',j_otypes( j_otype(irg(1)))
	j_err=.true.
 
	endselect !select case(j_otype(irg(1)))
	return
 
 
 
	! j_err=.true.
	return
 
end subroutine MINUS !subroutine MINUS(iob,io)
 
 
 
subroutine EQ(iob,io)
	double precision val1
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	if(narg.eq.1)then
		if(j_otype(irg1).eq.j_ipmatrix)then
			val1=j_o(irg1)%d(1)
			do j=2,j_o(irg1)%i(3)
				if(j_o(irg1)%d(j).ne.val1)then
 
					j_v(iout)=j_0
					return
				endif !if(j_o(irg1)%d(j).ne.val1)   5346
			enddo !j=2,j_o(irg1)%i(3)   5345
			j_v(iout)=j_1
			return
 
		elseif(j_otype(irg1).eq.j_iplist)then !if(j_otype(irg1).eq.j_ipmatrix)then
			val1=j_v( j_o(irg1)%i(1))
			do j=2,j_o(irg1)%i(1)
				if(j_v( j_o(irg1)%i2(j)).ne.val1)then
					j_v(iout)=j_0
					return
				endif !if(j_v( j_o(irg1)%i2(j)).ne.val1)   5358
			enddo !j=2,j_o(irg1)%i(1)   5357
			j_v(iout)=j_1
			return
		else !if(j_otype(irg1).eq.j_ipmatrix)then
			write(6,*)'If only one argument in EQ it must be matrix ot list'
			j_err=.true.
			return
		endif !if(j_otype(irg1).eq.j_ipmatrix)   5343
	endif !if(narg.eq.1)   5342
	irg2=j_o(iob)%i(io+3)
	if(j_v(irg1).eq.j_v(irg2))then
		j_v(iout)=j_1
	else !if(j_v(irg1).eq.j_v(irg2))then
		j_v(iout)=j_0
	endif !if(j_v(irg1).eq.j_v(irg2))   5372
 
end subroutine !subroutine EQ(iob,io)
 
subroutine NE(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	if(j_v(irg1).ne.j_v(irg2))then
		j_v(iout)=j_1
	else !if(j_v(irg1).ne.j_v(irg2))then
		j_v(iout)=j_0
	endif !if(j_v(irg1).ne.j_v(irg2))   5387
 
end subroutine !subroutine NE(iob,io)
 
 
 
subroutine LE(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_v(irg1).le.j_v(irg2))then
		j_v(iout)=j_1
	else !if(j_v(irg1).le.j_v(irg2))then
		j_v(iout)=j_0
	endif !if(j_v(irg1).le.j_v(irg2))   5403
end subroutine !subroutine LE(iob,io)
 
subroutine LT(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	!	write(6,*)'<666lt',j_v(irg1),j_v(irg2)
	if(j_v(irg1).lt.j_v(irg2))then
		j_v(iout)=j_1
	else !if(j_v(irg1).lt.j_v(irg2))then
		j_v(iout)=j_0
	endif !if(j_v(irg1).lt.j_v(irg2))   5417
end subroutine !subroutine LT(iob,io)
 
subroutine GE(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_v(irg1).ge.j_v(irg2))then
		j_v(iout)=j_1
	else !if(j_v(irg1).ge.j_v(irg2))then
		j_v(iout)=j_0
	endif !if(j_v(irg1).ge.j_v(irg2))   5430
end subroutine !subroutine GE(iob,io)
 
subroutine GT(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_v(irg1).gt.j_v(irg2))then
		j_v(iout)=j_1
	else !if(j_v(irg1).gt.j_v(irg2))then
		j_v(iout)=j_0
	endif !if(j_v(irg1).gt.j_v(irg2))   5443
end subroutine !subroutine GT(iob,io)
 
subroutine NOT(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	!	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_v(irg1).eq.j_0)then
		j_v(iout)=j_1
	else !if(j_v(irg1).eq.j_0)then
		j_v(iout)=j_0
	endif !if(j_v(irg1).eq.j_0)   5456
end subroutine !subroutine NOT(iob,io)
 
subroutine AND(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_v(irg1).ne.j_0.and.j_v(irg2).ne.0)then
		j_v(iout)=j_1
	else !if(j_v(irg1).ne.j_0.and.j_v(irg2).ne.0)then
		j_v(iout)=j_0
	endif !if(j_v(irg1).ne.j_0.and.j_v(irg2).ne.0)   5469
end subroutine !subroutine AND(iob,io)
 
subroutine OR(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_v(irg1).ne.j_0.or.j_v(irg2).ne.j_0)then
		j_v(iout)=j_1
	else !if(j_v(irg1).ne.j_0.or.j_v(irg2).ne.j_0)then
		j_v(iout)=j_0
	endif !if(j_v(irg1).ne.j_0.or.j_v(irg2).ne.j_0)   5482
end subroutine !subroutine OR(iob,io)
 
subroutine EQV(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if((j_v(irg1).ne.j_0.and.j_v(irg2).ne.j_0).or.(j_v(irg1).eq.j_0.and.j_v(irg2).eq.j_0))then
		j_v(iout)=1.d0
	else !if((j_v(irg1).ne.j_0.and.j_v(irg2).ne.j_0).or.(j_v(irg1).eq.j_0.and.j_v(irg2).eq.j_0))then
		j_v(iout)=j_0
	endif !if((j_v(irg1).ne.j_0.and.j_v(irg2).ne.j_0).or.(j_v(irg1).e   5495
end subroutine !subroutine EQV(iob,io)
 
subroutine NEQV(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if((j_v(irg1).ne.j_0.and.j_v(irg2).eq.j_0).or.(j_v(irg1).eq.j_0.and.j_v(irg2).ne.j_0))then
		j_v(iout)=1.d0
	else !if((j_v(irg1).ne.j_0.and.j_v(irg2).eq.j_0).or.(j_v(irg1).eq.j_0.and.j_v(irg2).ne.j_0))then
		j_v(iout)=j_0
	endif !if((j_v(irg1).ne.j_0.and.j_v(irg2).eq.j_0).or.(j_v(irg1).e   5508
end subroutine !subroutine NEQV(iob,io)
 
subroutine min_(iob,io)
	integer*8::ncols
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(narg.eq.2)then
		if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipreal)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=dmin1(j_v(irg1),j_v(irg2))
			return
		endif !if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipreal   5522
		if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipmatrix)then
			if(j_o(irg1)%i(1).ne.j_o(irg2)%i(1).or.j_o(irg1)%i(2).ne.j_o(irg2)%i(2))then
				write(6,*)'incompatibe dimensions ',j_o(irg1)%i(1:2),j_o(irg2)%i(1:2)
				j_err=.true.;return
			endif !if(j_o(irg1)%i(1).ne.j_o(irg2)%i(1).or.j_o(irg1)%i(2).ne.j   5528
			if(irg1.ne.iout.and.irg2.ne.iout)iout=j_defmatrix(iout,' ',int8(j_o(irg2)%i(1)),int8(j_o(irg2)%i(2)),j_matreg)
			do ii=1,j_o(irg2)%i(3)
				j_o(iout)%d(ii)=dmin1(j_o(irg1)%d(ii),j_o(irg2)%d(ii))
			enddo !ii=1,j_o(irg2)%i(3)   5533
			return
		endif !if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipma   5527
		if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipreal)then
			ir=irg1
			irg1=irg2
			irg2=ir
		endif !if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipre   5538
		!now irg2 is matrix
		!	write(6,*)'<66',irg1,irg2,j_otype(irg1),j_otype(irg2),j_ipmatrix
		if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipmatrix)then
 
			if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)then
				if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))then
					j_o(iout)%i(1)=j_o(irg2)%i(1)
					j_o(iout)%i(2)=j_o(irg2)%i(2)
				else !if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))then
					iout=j_defmatrix(iout,' ',int8(j_o(irg2)%i(1)),int8(j_o(irg2)%i(2)),j_matreg)
				endif !if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))   5548
			else !if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)then
				iout=j_defmatrix(iout,' ',int8(j_o(irg2)%i(1)),int8(j_o(irg2)%i(2)),j_matreg) !could do with one def
			endif !if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)   5547
			do ii=1,j_o(irg2)%i(3)
				j_o(iout)%d(ii)=dmin1(j_v(irg1),j_o(irg2)%d(ii))
			enddo !ii=1,j_o(irg2)%i(3)   5557
			return
 
		endif !if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipmatr   5545
 
		write(6,*)'illegal argument types ',j_otypes(j_otype(irg1)),j_otypes(j_otype(irg1))
		j_err=.true.;return
	endif !if(narg.eq.2)   5521
 
 
	if(j_otype(irg1).ne.j_ipmatrix)then
		write(6,*)'one argument must be MATRIX'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5569
	nrows=j_o(irg1)%i(1)
	ncols=j_o(irg1)%i(2)
	if(j_linkoption(iob,io,j_many).ge.0)then
		ncols=1
		!!call j_clearoption(iob,io)
	endif !if(j_linkoption(iob,io,j_many).ge.0)   5575
	if(nrows.eq.1.or.ncols.eq.1)then
 
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=minval(j_o(irg1)%d(1:j_o(irg1)%i(3)))
 
	else !if(nrows.eq.1.or.ncols.eq.1)then
		iout=j_defmatrix(iout,' ',j_18,ncols,j_matreg)
		j_o(iout)%d=j_v(j_ivinf)
		ibas=0  !could use reshape
		do i=1,nrows
			do j=1,ncols
				j_o(iout)%d(j)=min(j_o(iout)%d(j),j_o(irg1)%d(ibas+j))
			enddo !j=1,ncols   5589
			ibas=ibas+ncols
		enddo !i=1,nrows   5588
 
	endif !if(nrows.eq.1.or.ncols.eq.1)   5579
	return
 
end subroutine !subroutine min_(iob,io)
 
subroutine max_(iob,io)
	integer*8::nrows,ncols,ibas
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(narg.eq.2)then
		if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipreal)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=dmax1(j_v(irg1),j_v(irg2))
			return
		endif !if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipreal   5607
		if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipmatrix)then
			if(j_o(irg1)%i(1).ne.j_o(irg2)%i(1).or.j_o(irg1)%i(2).ne.j_o(irg2)%i(2))then
				write(6,*)'incompatibe dimensions ',j_o(irg1)%i(1:2),j_o(irg2)%i(1:2)
				j_err=.true.;return
			endif !if(j_o(irg1)%i(1).ne.j_o(irg2)%i(1).or.j_o(irg1)%i(2).ne.j   5613
			if(irg1.ne.iout.and.irg2.ne.iout)iout=j_defmatrix(iout,' ',int8(j_o(irg2)%i(1)),int8(j_o(irg2)%i(2)),j_matreg)
			do ii=1,j_o(irg2)%i(3)
				j_o(iout)%d(ii)=dmax1(j_o(irg1)%d(ii),j_o(irg2)%d(ii))
			enddo !ii=1,j_o(irg2)%i(3)   5618
			return
		endif !if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipma   5612
		if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipreal)then
			ir=irg1
			irg1=irg2
			irg2=ir
		endif !if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipre   5623
		!now irg2 is matrix
		!	write(6,*)'<66',irg1,irg2,j_otype(irg1),j_otype(irg2),j_ipmatrix
		if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipmatrix)then
 
			if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)then
				if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))then
					j_o(iout)%i(1)=j_o(irg2)%i(1)
					j_o(iout)%i(2)=j_o(irg2)%i(2)
				else !if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))then
					iout=j_defmatrix(iout,' ',int8(j_o(irg2)%i(1)),int8(j_o(irg2)%i(2)),j_matreg)
				endif !if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))   5633
			else !if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)then
				iout=j_defmatrix(iout,' ',int8(j_o(irg2)%i(1)),int8(j_o(irg2)%i(2)),j_matreg) !could do with one def
			endif !if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)   5632
			do ii=1,j_o(irg2)%i(3)
				j_o(iout)%d(ii)=dmax1(j_v(irg1),j_o(irg2)%d(ii))
			enddo !ii=1,j_o(irg2)%i(3)   5642
			return
 
		endif !if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipmatr   5630
 
		write(6,*)'illegal argument types ',j_otypes(j_otype(irg1)),j_otypes(j_otype(irg1))
		j_err=.true.;return
	endif !if(narg.eq.2)   5606
 
	if(j_otype(irg1).ne.j_ipmatrix)then
		write(6,*)'one arguement must be MATRIX'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5653
	nrows=j_o(irg1)%i(1)
	ncols=j_o(irg1)%i(2)
	if(j_linkoption(iob,io,j_many).ge.0)then
		ncols=j_18
		!!call j_clearoption(iob,io)
	endif !if(j_linkoption(iob,io,j_many).ge.0)   5659
	if(nrows.eq.1.or.ncols.eq.1)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=maxval(j_o(irg1)%d(1:j_o(irg1)%i(3)))
 
	else !if(nrows.eq.1.or.ncols.eq.1)then
		iout=j_defmatrix(iout,' ',j_18,ncols,j_matreg)
		j_o(iout)%d=j_v(j_ivtolast)
		ibas=0  !could use reshape
		do i=1,nrows
			do j=1,ncols
				j_o(iout)%d(j)=dmax1(j_o(iout)%d(j),j_o(irg1)%d(ibas+j))
			enddo !j=1,ncols   5672
			ibas=ibas+ncols
		enddo !i=1,nrows   5671
 
	endif !if(nrows.eq.1.or.ncols.eq.1)   5663
	return
end subroutine !subroutine max_(iob,io)
 
subroutine minloc_(iob,io)
	! Section minloc minloc() Locations of the minimum values
	! minloc(MATRIX) generates a row vector containing the locations of the  minimum
	! values in each column. minloc(VECTOR) is the REAL scalar telling
	! the location of the minimum value. Thus the VECTOR can also be a row vector.
	!endheader
	!Note The observation for which a DATA variable gets minimum value can be obtained
	! using minobs-> option in stat().
	!endnote
	! endsection
	integer*8 ncols,nrows,i,ibas
	double precision::minva
	integer loco(1)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	!	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	!	if(narg.eq.2)then
	!	j_v(iout)=dmin1(j_v(irg1),j_v(irg2)) UPDATE MINARG
	!	return
	!	endif
	if(j_otype(irg1).ne.j_ipmatrix)then
		write(6,*)'argument must be MATRIX'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5704
	nrows=j_nrows(irg1)  !j_o(irg1)%i(1)
	ncols=j_ncols(irg1) !j_o(irg1)%i(2)
	if(j_linkoption(iob,io,j_many,clear=.true.).ge.0)then
		ncols=1
		!	!!call j_clearoption(iob,io)
	endif !if(j_linkoption(iob,io,j_many,clear=.true.).ge.0)   5710
	if(nrows.eq.1.or.ncols.eq.1)then
 
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		loco=minloc(j_o(irg1)%d(1:j_o(irg1)%i(3)))
		j_v(iout)=loco(1)
	else !if(nrows.eq.1.or.ncols.eq.1)then
		iout=j_defmatrix(iout,' ',j_18,ncols,j_matreg)
		minva=j_v(j_ivinf)
		loc=0
		!	write(6,*)'<888',nrows,ncols
		do i=1,ncols
			ibas=0  !could use reshape
			do j=1,nrows
 
				if(j_o(irg1)%d(ibas+i).lt.minva)then
					minva=j_o(irg1)%d(ibas+i)
					loc=j
				endif !if(j_o(irg1)%d(ibas+i).lt.minva)   5728
				ibas=ibas+ncols
			enddo !j=1,nrows   5726
			j_o(iout)%d(i)=loc
		enddo !i=1,ncols   5724
 
	endif !if(nrows.eq.1.or.ncols.eq.1)   5714
	return
 
end subroutine !subroutine minloc_(iob,io)
 
subroutine maxloc_(iob,io)
	! Section maxloc maxloc() Locations of the minimum values
	! maxloc(MATRIX) generates a row vector containing the locations of the  minimum
	! values in each column. maxloc(VECTOR) is the REAL scalar telling
	! the location of the maxim value whether VECTOR is a row vector or column vector.
	!endheader
	!Note The observation for which a DATA variable gets the maximum value can be obtained
	! using maxobs-> option in stat()
	!endnote
	! endsection
	integer*8 ::ncols,nrows,ibas,i,j,loc
 
	double precision::maxva
	integer loco(1)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	!	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	!	if(narg.eq.2)then
	!	j_v(iout)=dmin1(j_v(irg1),j_v(irg2)) UPDATE MINARG
	!	return
	!	endif
	if(j_otype(irg1).ne.j_ipmatrix)then
		write(6,*)'argument must be MATRIX'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5765
	nrows=j_nrows(irg1)  !j_o(irg1)%i(1)
	ncols=j_ncols(irg1) !j_o(irg1)%i(2)
	if(j_linkoption(iob,io,j_many).ge.0)then
		ncols=j_18
		!!call j_clearoption(iob,io)
	endif !if(j_linkoption(iob,io,j_many).ge.0)   5771
	if(nrows.eq.1.or.ncols.eq.1)then
 
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		loco=maxloc(j_o(irg1)%d(1:j_o(irg1)%i(3)))
		j_v(iout)=loco(1)
 
	else !if(nrows.eq.1.or.ncols.eq.1)then
		iout=j_defmatrix(iout,' ',j_18,ncols,j_matreg)
		minva=j_v(j_ivinf)
		loc=0
 
		do i=1,ncols
			ibas=0  !could use reshape
			do j=1,nrows
				if(j_o(irg1)%d(ibas+i).gt.maxva)then
					maxva=j_o(irg1)%d(ibas+i)
					loc=j
				endif !if(j_o(irg1)%d(ibas+i).gt.maxva)   5789
				ibas=ibas+ncols
			enddo !j=1,nrows   5788
			j_o(iout)%d(i)=loc
		enddo !i=1,ncols   5786
 
	endif !if(nrows.eq.1.or.ncols.eq.1)   5775
	return
 
end subroutine !subroutine maxloc_(iob,io)
 
subroutine cumsum(iob,io)
	! Section cumsum cumsum() Cumulative sums
	! cumsum(MATRIX) generates a MATRIX with the same dimesnions as the argument,
	! and puts the cumulative sums of the columsn into the output matrix.
	! endheader
	! Note If the argument is vector, the cumsum makes a vector having the same
	! form as the argument.
	! endnote
	! endheader
	!endsection
	integer*8::nrows,ncols,nel,ibas,ibasv
	double precision::cumsu
	logical::inverse
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	!	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
 
 
 
	!	if(narg.eq.2)then
	!	j_v(iout)=dmin1(j_v(irg1),j_v(irg2)) UPDATE MINARG
	!	return
	!	endif
	if(j_otype(irg1).ne.j_ipmatrix)then
		write(6,*)'argument must be MATRIX'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5827
	nrows=j_o(irg1)%i(1)
	ncols=j_o(irg1)%i(2)
	nel=j_o(irg1)%i(3)
 
 
	iout=j_defmatrix(iout,' ',nrows,ncols,j_matreg)
	cumsu=j_0
	if(nrows.eq.1.or.ncols.eq.1)then
 
		do i=1,nel
			cumsu=cumsu+j_o(irg1)%d(i)
			j_o(iout)%d(i)=cumsu
		enddo !i=1,nel   5840
 
	else !if(nrows.eq.1.or.ncols.eq.1)then
 
		j_o(iout)%d(1:ncols)=j_o(irg1)%d(1:ncols)
		ibas=nrows  !could use reshape
		ibasv=0
		do i=2,nrows
			j_o(iout)%d(ibas+1:ibas+ncols)=j_o(iout)%d(ibasv+1:ibasv+ncols)+&
				j_o(irg1)%d(ibas+1:ibas+ncols)
			ibasv=ibas
			ibas=ibas+ncols
 
		enddo !i=2,nrows   5850
	endif !if(nrows.eq.1.or.ncols.eq.1)   5838
	return
 
end subroutine !subroutine cumsum(iob,io)
 
subroutine corrmatrix(iob,io)
	!Section corrmatrix corrmatrix() Correlation matrix from variance-covariance matrix
	!This simple function is sometimes needed. The function does not test wether the input matrix is symmetric.
	!Negative diagonal eleemnt produces error, value zero correaltion 9,99.
	!endheader
	!Option
	!Output& 1& MATRIX& matrix having nondiagonal values \\
	!]Out[(i,j)= ]arg[(i,j)=
	! ]arg[(i,j)/sqrt(]arg[(i,i)*]arg[(j,j)).
	!Args &1& MATRIX& symmetric matrix
	!sd&N|0 & &If sd-> is given, then diagonal elements will be equal to sqrt(]arg[(i,i)
	!endoption
	!endsection)
	integer*8::ndim,ibas,i
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	!	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(irg1).ne.j_ipmatrix)then
		call j_getname(irg1)
		write(6,*)j_oname(1:j_loname),' is not MATRIX but ',j_otypes(j_otype(irg1))
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5880
	if(j_o(irg1)%i(1).ne.j_o(irg1)%i(2))then
		call j_getname(irg1)
		write(6,*)j_oname(1:j_loname),' is not square, it has dimensions ',j_o(irg1)%i(1),j_o(irg1)%i(2)
		j_err=.true.;return
	endif !if(j_o(irg1)%i(1).ne.j_o(irg1)%i(2))   5885
	ndim=j_o(irg1)%i(1)
	if(iout.ne.irg1)iout=j_defmatrix(iout,' ',ndim,ndim,j_matreg)
	ibas=0
	do i=1,ndim
		if(j_o(irg1)%d(ibas+i).lt.j_0)then
			write(6,*)'diagonal ',i, 'has illegal value ',j_o(irg1)%d(ibas+i)
			j_err=.true.;return
 
		endif !if(j_o(irg1)%d(ibas+i).lt.j_0)   5894
		j_o(iout)%d(ibas+i)=sqrt(j_o(irg1)%d(ibas+i))
		ibas=ibas+ndim
	enddo !i=1,ndim   5893
	ibas=0
	!	write(6,*)'tas',j_o(iout)%d
	!	write(6,*)'tirg',j_o(iout)%d
	do i=1,ndim
 
		do j=1,ndim
			!		write(6,*)'i,j',i,j,j_o(iout)%d(ibas+j),j_o(iout)%d(ibas+i),j_o(iout)%d((j-1)*ndim+j)
			if(i.ne.j)then
				j_dapu=j_o(iout)%d(ibas+i)*j_o(iout)%d((j-1)*ndim+j)
				if(j_dapu.ne.j_0)then
					j_o(iout)%d(ibas+j)=&
						j_o(irg1)%d(ibas+j)/j_dapu
				else
					j_o(iout)%d(ibas+j)=9.99d0
 
				endif !if(j_dapu.ne.j_0)   5911
 
			endif !if(i.ne.j)   5909
			!		write(6,*)j_o(iout)%d(ibas+j)
		enddo !j=1,ndim   5907
 
		ibas=ibas+ndim
	enddo !i=1,ndim   5905
	if(j_linkoption(iob,io,j_msd,clear=.true.).lt.0)then
		ibas=0
		do i=1,ndim
			if(j_o(iout)%d(ibas+i).ne.j_0)j_o(iout)%d(ibas+i)=j_1
			ibas=ibas+ndim
 
		enddo !i=1,ndim   5927
	endif !if(j_linkoption(iob,io,j_msd,clear=.true.).lt.0)   5925
 
 
 
end subroutine
 
subroutine matrixborder(iob,io)
	logical collist,rowlist
 
	!Section matrixborder matrixborder) puts row above a MATRIX or column on the left
	!When computing reasults into a MATRIX, often header row is needed on top and/or on left.
	!endheader
	!Option
	!Output& 1& MATRIX& the augmented matrix
	!Args &1& MATRIX& input
	!row &-1 -& LIST|MATRIX|REAL & values put on top
	!column &-1 -& LIST|MATRIX|REAL & values put to left
	!endoption
	!Ex matrixborderex
	! a=matrix(2,3,do->);
	! b=matrix(2,values->(2,5));
	! ab=matrixborder(a,column->b);
	! c=matrix(3,values->(4,2,5));
	! ac=matrixborder(a,row->c);
	! abc=matrixborder(a,row->c,column->b);
	! row=list(x1...x3)
	! @row=4,2,5
	! col=list(c1,c2)
	! @col=2,5
	! ab=matrixborder(a,column->col);
	! ac=matrixborder(a,row->row);
	! abc=matrixborder(a,row->row,column->col);
	! ab=matrixborder(a,column->(2,5));
	! ac=matrixborder(a,row->(4,2,5));
	! abc=matrixborder(a,row->(4,2,5),column->(2,5));
	!endex
	!endsection)
 
	call j_startfunction(iob,io,j_ipmatrix,narg,j_arg,ivout);if(j_err)return
 
	imat=j_arg(1)
	nrow=j_o(imat)%i(1)
	ncol=j_o(imat)%i(2)
	!	call j_getname(imat)
	!	write(6,*)j_oname(1:j_loname),nrow,ncol,j_o(imat)%d
	call j_getoption(iob,io,j_mcolumn,-1,nrow,0,.true.,icol,j_optarg0)
 
	if(j_err)return
	ivcol=0
	collist=.true.
	if(icol.eq.1)then
		ivcol=j_optarg0(1)
		if(j_otype(ivcol).eq.j_iplist)then
			ncol0=j_o(ivcol)%i(1)
			j_optarg0=>j_o(ivcol)%i2(1:ncol0)
 
		elseif(j_otype(ivcol).eq.j_ipmatrix)then
			collist=.false.
			ncol0=j_o(ivcol)%i(1)
		elseif(j_otype(ivcol).eq.j_ipreal)then
			ncol0=1
 
		endif !if(j_otype(ivcol).eq.j_iplist)   5984
	elseif(icol.gt.0)then
		ncol0=icol
		icol=1
	endif !if(icol.eq.1)   5982
	icol=max(icol,0)
	if(icol.gt.0.and.ncol0.ne.nrow)then
 
		write(6,*)'argument of col-> has ',ncol0,' elements, should have ',nrow
		j_err=.true.;return
	endif !if(icol.gt.0.and.ncol0.ne.nrow)   6000
 
 
 
	call j_getoption(iob,io,j_mrow,-1,ncol,0,.true.,irow,j_optarg)
	rowlist=.true.
	if(irow.eq.1)then
		ivrow=j_optarg(1)
		if(j_otype(ivrow).eq.j_iplist)then
			nrow0=j_o(ivrow)%i(1)
			j_optarg=>j_o(ivrow)%i2(1:nrow0)
 
		elseif(j_otype(ivrow).eq.j_ipmatrix)then
			rowlist=.false.
			nrow0=j_o(ivrow)%i(1)
		elseif(j_otype(ivrow).eq.j_ipreal)then
			nrow0=1
 
		endif !if(j_otype(ivrow).eq.j_iplist)   6012
	elseif(irow.gt.0)then
		nrow0=irow
		irow=1
 
	endif !if(irow.eq.1)   6010
	irow=max(irow,0)
	if(irow.gt.0.and.nrow0.ne.ncol)then
 
		write(6,*)'argument of row-> has ',nrow0,' elements, should have ',ncol
		j_err=.true.;return
	endif !if(irow.gt.0.and.nrow0.ne.ncol)   6029
 
 
 
	if(icol+irow.eq.0)then
		write(6,*)'matrixborder needs column-> or row-> or both'
		j_err=.true.;return
 
	endif !if(icol+irow.eq.0)   6037
	ivout=j_defmatrix(ivout,' ',int8(nrow+irow),int8(ncol+icol),j_matreg)
	ibas=0
	if(irow.gt.0)then
		if(rowlist)then
			j_o(ivout)%d(icol+1:icol+ncol)=j_v( j_optarg(1:ncol))
		else
			!
			j_o(ivout)%d(icol+1:icol+ncol)=j_o(ivrow)%d(1:ncol)
 
 
		endif !if(rowlist)   6045
		ibas=icol+ncol
 
	endif !if(irow.gt.0)   6044
	ibasin=0
	do ir=1,nrow
		!	write(6,*)'ivout,icol,ibas,imat,ibasin,ncol',ir,ivout,icol,ibas,imat,ibasin,ncol
		!		write(6,*)icol+ibas+1,icol+ibas+ncol,ibasin+1,ibasin+ncol
		j_o(ivout)%d(icol+ibas+1:icol+ibas+ncol)=j_o(imat)%d(ibasin+1:ibasin+ncol)
		if(icol.ne.0)then
			if(collist)then
				j_o(ivout)%d(ibas+1)=j_v(j_optarg0(ir))
			else
				j_o(ivout)%d(ibas+1)=j_o(ivcol)%d(ir)
 
			endif !if(collist)   6062
 
		endif !if(icol.ne.0)   6061
		ibasin=ibasin+ncol
		ibas=ibas+ncol+icol
	enddo !ir=1,nrow   6057
 
	return
 
 
 
end subroutine
 
subroutine fill(iob,io)
	logical::isrow,iscolumn,islast,isany,p
	!	integer,dimension(1)::loc,loc2
	integer ::outstep
	double precision ::x0,x1,y0,y1,x,val
	val()=y0+(x-x0)*(y1-y0)/(x1-x0)
	!Section fill fill() fills empty (zero) cells in a matrix
	!Currently fill() uses linear interpolation to fill rows or columns. Which ones
	!are interpolated first is indicated with row-> or column->. If all cells
	!could not be filled, the filling direction is changed if any-> option is present.
	!endheader
	!Option
	!Output&0| 1& MATRIX& The filled matrix. If output is not given, argument is filled.
	!Args &1& MATRIX& input
	!row & & &rows are filled first using linear interpolation
	!column & & &columns are filled first using linear interpolation
	!any & & &the direction is changed if there were not at least two nonempty cells
	! in the chosen direction
	!endoption
	!Note It is simple to extent to more complicated methods, e.g. to use bilinear
	!interpolation
	!endnote
	!Note It is implicitly assumed that distances between rows or columns are equal, i.e.,
	!the row or column indexes are used as the x-values.
	!It would be simple to add possiblity to give as arguments vectors which define
	!the distances.
	!endnote
	!Ex fillex
	! a=matrix(4,3,do->)
	! a(2,3)=0
	! a;
	! b=fill(a,row->);
	! c=fill(a,column->);
	! a(2,1)=0
	! a;
	! d=fill(a,row->);
	!d=fill(a,row->,any->);
	! e=fill(a,column->);
	! a(2,2)=0
	! a;
	! f=fill(a,row->);
	!! f=fill(a,row->,any->);
	! g=fill(a,column->);
	!endex
	!endsection)
	p=j_v(j_ivdollar2).eq.235.d0
	call j_startfunction(iob,io,j_ipmatrix,narg,j_arg,ivout);if(j_err)return
	iscolumn=j_isoption(iob,io,j_mcolumn)
	isrow=j_isoption(iob,io,j_mrow)
	!	islast=j_isoption(iob,io,j_mlast)
	if(.not.(isrow.or.iscolumn).or.iscolumn.and.isrow)then
		write(6,*)'fill requires either row-> or column->'
		j_err=.true.;return
	endif !if(.not.(isrow.or.iscolumn).or.iscolumn.and.isrow)   6130
	isany=j_isoption(iob,io,j_many)
 
	if(ivout.ne.j_ivresult)then
		imat=j_defmatrix(ivout,' ',int8(j_o(j_arg(1))%i(1)),int8(j_o(j_arg(1))%i(2)),j_matreg)
		j_o(imat)%d(1:j_o(imat)%i(3))=j_o(j_arg(1))%d(1:j_o(imat)%i(3))
	else
		imat=j_arg(1)
	endif !if(ivout.ne.j_ivresult)   6136
 
	if(isrow)then  !n
		nrow=j_o(imat)%i(1)
		ncol=j_o(imat)%i(2)
	else
		ncol=j_o(imat)%i(1)
		nrow=j_o(imat)%i(2)
	endif !if(isrow)   6143
	iround=0
 
900 not=0
loop0:	do irow=1,nrow

		y0=mat(1)   !   iroj_o(imat)%d(iel)
		iid=0
		if(y0.eq.j_0)then
		loop:		do jj0=2,ncol
				y0=mat(jj0)
				if(y0.eq.j_0)cycle
				x0=jj0
				do jj2=jj0+1,ncol
					y1=mat(jj2)
					if(y1.eq.j_0)cycle
					x1=jj2
					do jj3=1,jj0-1
						x=jj3
						j_o(imat)%d(iel(jj3))=val()
					enddo !jj3=1,jj0-1   6166
					do jj3=jj0+1,jj2-1
						x=jj3
						j_o(imat)%d(iel(jj3))=val()
					enddo !jj3=jj0+1,jj2-1   6170
					icol=jj2+1
					goto 100
				enddo !jj2=jj0+1,ncol   6162
 
				iid=1
				! if(isrow)then
				! write(6,*)'row ',irow, ' has only one nonzero values at column ',jj0
				! else
				! write(6,*)'column ',irow, ' has only one nonzero values at row ',jj0
				! endif !if(isrow)   5868
			enddo loop !p:		do jj0=2,ncol   6158
			not=not+ncol-iid
			cycle loop0
			! if(isrow)then
			! write(6,*)'row ',irow,' zeros, cannot fill yet'
			! else
			! write(6,*)'column ',irow,' zeros, cannot fill yet'
			! endif !if(isrow)   5874
			! j_err=.true.;return
		else
			icol=2
			x0=1
		endif !if(y0.eq.j_0)   6157
100	continue
			!jj how many treated
		do while (icol.le.ncol)
			if(mat(icol).eq.j_0)then
 			loo:		do jj2=icol+1,ncol
					y1=mat(jj2)
					if(y1.ne.j_0)then
						x1=jj2
						do jj3=icol,jj2-1
							x=jj3
							j_o(imat)%d(iel(jj3))=val()
						enddo !jj3=icol,jj2-1   6205
						goto 200
					endif !if(y1.ne.j_0)   6203
				enddo loo !:		do jj2=icol+1,ncol   6201
				!all rest zero
				if(icol.eq.2)then
					not=not+ncol-1
					cycle loop0
				endif !if(icol.eq.2)   6213
				y1=mat(icol-2)
				x1=icol-2
				x0=icol-1
				y0=mat(icol-1)
				do j=icol,ncol
					x=j
 
					j_o(imat)%d(iel(j))=val()
					if(p)write(6,*)'x,x0,x1,y0,y1',x,x0,x1,y0,y1,j_o(imat)%d(iel(j))
				enddo !j=icol,ncol   6221
				cycle loop0
	200				icol=jj2+1
				y0=y1
			else
				y0=mat(icol)
				x0=icol
				icol=icol+1
			endif !if(mat(icol).eq.j_0)   6200
		enddo !while (icol.le.ncol)   6199
	enddo loop0 !p0:	do irow=1,nrow   6153
	if(not.gt.0)then
		iround=iround+1
		if(iround.eq.1)then
			write(6,*)'could not fill ',not, ' cells'
			if(isany)then
				write(6,*)'changing the interpolation direction'
				isrow=.not.isrow
				if(isrow)then  !n
					nrow=j_o(imat)%i(1)
					ncol=j_o(imat)%i(2)
				else
					ncol=j_o(imat)%i(1)
					nrow=j_o(imat)%i(2)
				endif !if(isrow)   6244
				goto 900
			endif !if(isany)   6241
		else
			write(6,*)'could not fill ',not,'cells '
 
		endif !if(iround.eq.1)   6239
 
	endif !if(not.gt.0)   6237
 
 
 
	contains
	double precision function mat(icol)
		if(isrow)then
			mat=j_o(imat)%d((irow-1)*ncol+icol)
		else
			mat=j_o(imat)%d((icol-1)*nrow+irow)
		endif !if(isrow)   6264
	end function
 
	integer function iel(icol)
		if(isrow)then
			iel=(irow-1)*ncol+icol
		else
			iel=(icol-1)*nrow+irow
		endif !if(isrow)   6272
	endfunction
 
end subroutine
 
subroutine vector(iob,io)
	!Section vector Makes a column vector of REAL arguments.
	!endsection
 
	call j_startfunction(iob,io,j_ipreal,narg,j_arg,ivout)
	if(j_err)return
	j_yes=j_isoption(iob,io,j_mrow)
	if(j_yes)then
		nrow=1
		ncol=narg
 
	else
		nrow=narg
		ncol=1
 
 
	endif !if(j_yes)   6288
	ivout=j_defmatrix(ivout,' ',int8(nrow),int8(ncol),j_matreg)
	j_o(ivout)%d(1:narg)=j_v(j_arg)
 
 
end subroutine vector
 
subroutine flip(iob,io)
	!Section flip Reversers order of matrix element
	!Reversers the order matrix elements. If there is no output, the reordering happens in place.
	!Remember that Jlp22 stores matrices in row order.
	!endsection
 
 
	call j_startfunction(iob,io,j_ipmatrix,narg,j_arg,ivout)
	if(j_err)return
	if(ivout.eq.j_arg(1))ivout=j_ivresult
 
	ivout=j_defmatrix(ivout,' ',int8(j_o(j_arg(1))%i(1)),int8(j_o(j_arg(1))%i(2)),j_matreg)
 
	nel=j_o(ivout)%i(3)
	do i=1,nel
		j=nel-i+1
		j_o(ivout)%d(i)=j_o(j_arg(1))%d(j)
	enddo !i=1,nel   6318
	if(ivout.eq.j_ivresult)j_o(j_arg(1))%d=j_o(j_ivresult)%d
 
end subroutine flip
 
subroutine condition(iob,io)
 
 
 
 
end subroutine
 
subroutine abs_(iob,io)
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	!	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
 
 
	narg=j_o(iob)%i(io+1)
 
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dabs(j_v(irg))
 
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		if(iout.ne.irg)then
			if(j_otype(iout).eq.j_ipmatrix)then
				if(j_o(iout)%i(3).eq.j_o(irg)%i(3))then
					j_o(iout)%i(1)=j_o(irg)%i(1)
					j_o(iout)%i(2)=j_o(irg)%i(2)
					goto 80
				endif !if(j_o(iout)%i(3).eq.j_o(irg)%i(3))   6349
			endif !if(j_otype(iout).eq.j_ipmatrix)   6348
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			ivout=j_defmatrix(iout,' ',int8(j_o(irg)%i(1)),int8(j_o(irg)%i(2)),j_matreg) !matreg?
		endif !if(iout.ne.irg)   6347
80		continue
		j_o(iout)%d=dabs(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6342
 
 
end subroutine !subroutine abs_(iob,io)
 
 
 
subroutine sign_(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	j_v(iout)=dsign(j_v(irg1),j_v(irg2))
end subroutine !subroutine sign_(iob,io)
 
subroutine nint_(iob,io)
	integer,intent(in)::iob,io
 
	irg=j_o(iob)%i(io+2)  !narg is present
	iout=j_o(iob)%i(io+3)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dnint(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dnint(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6384
end subroutine !subroutine nint_(iob,io)
 
subroutine int_(iob,io)
	integer,intent(in)::iob,io
 
	irg=j_o(iob)%i(io+2)  !narg is present
	iout=j_o(iob)%i(io+3)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dint(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dint(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6405
end subroutine !subroutine int_(iob,io)
 
subroutine ceiling_(iob,io)
	integer,intent(in)::iob,io
 
	irg=j_o(iob)%i(io+2)  !narg is present
	iout=j_o(iob)%i(io+3)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=ceiling(real(j_v(irg)))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=ceiling(real(j_o(irg)%d))
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6426
end subroutine !subroutine ceiling_(iob,io)
 
subroutine floor_(iob,io)
	integer,intent(in)::iob,io
 
	irg=j_o(iob)%i(io+2)  !narg is present
	iout=j_o(iob)%i(io+3)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=floor(real(j_v(irg)))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=floor(real(j_o(irg)%d))
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6447
end subroutine !subroutine floor_(iob,io)
 
subroutine UNIOP(iob,io,ioper)
	integer,intent(in)::iob,io
	double precision minva
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
 
		if(j_otype(iout).eq.j_ipmatrix.and.iout.le.j_named)then
			goto (82,83,84,85,86,87,88,89,810,811,812,813,814,815,816,817,818,819,&
				920,821,822,823,824,825,826,827,828,829,830,831)ioper
			!abs!NOTE ILLOGICAL ORDER
 
 
82 	j_o(iout)%d=dnint(j_v(irg))
			return
			!abs
83 	j_o(iout)%d=dint(j_v(irg))
			return
 
			!abs
84 	j_o(iout)%d=ceiling(real(j_v(irg)))
			return
			!abs
85 	j_o(iout)%d=floor(real(j_v(irg)))
			return
 
			!sqrt
86 	if(j_v(irg).lt.j_0)then
				write(6,*)'negative argument in sqrt ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).lt.j_0)then
				j_o(iout)%d=dsqrt(j_v(irg))
			endif !86 	if(j_v(irg).lt.j_0)   6492
			return
 
			!sqrt2
87 	if(j_v(irg).eq.j_0)then
				j_o(iout)%d=j_0
			elseif(j_v(irg).gt.j_0)Then !if(j_v(irg).eq.j_0)then
				j_o(iout)%d=sqrt(j_v(irg))
			else !if(j_v(irg).eq.j_0)then
				j_o(iout)%d=-sqrt(-j_v(irg))
			endif !87 	if(j_v(irg).eq.j_0)   6501
			return
			!
88 	if(j_v(irg).lt.j_0)then
				write(6,*)'negative argument in log ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).lt.j_0)then
				j_o(iout)%d=dlog(j_v(irg))
			endif !88 	if(j_v(irg).lt.j_0)   6510
			return
			!abs
89 	if(j_v(irg).lt.j_0)then
				write(6,*)'negative argument in log10 ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).lt.j_0)then
				j_o(iout)%d=dlog10(j_v(irg))
			endif !89 	if(j_v(irg).lt.j_0)   6518
			return
810	if(j_v(irg).gt.88.d0)then
				write(6,*)'too large argument ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).gt.88.d0)then
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				j_o(iout)%d=dexp(j_v(irg))
			endif !810	if(j_v(irg).gt.88.d0)   6525
			return
			!abs
811 	j_o(iout)%d=dsin(j_v(irg))
			return
			!abs
812 	j_o(iout)%d=dsin(j_deg*j_v(irg))
			return
			!abs
813 	j_o(iout)%d=dcos(j_v(irg))
			return
			!abs
814 	j_o(iout)%d=dcos(j_v(irg)*j_deg)
			return
 
			!abs
815 	j_o(iout)%d=dtan(j_v(irg))
			return
			!tand
816 	j_o(iout)%d=dtan(j_deg*j_v(irg))
			return
817		if(dabs(j_v(irg)).lt.d-15)then
				write(6,*)'too close to zero ',j_v(irg)
				j_err=.true.
			endif !817		if(dabs(j_v(irg)).lt.d-15)   6552
 
			j_o(iout)%d=dtan(1.d0/j_v(irg))
			return
818	if(dabs(j_v(irg)).lt.d-15)then
				write(6,*)'too close to zero ',j_v(irg)
				j_err=.true.
			endif !818	if(dabs(j_v(irg)).lt.d-15)   6559
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_o(iout)%d=dtan(1.d0/(j_deg*j_v(irg)))
			return
819	j_o(iout)%d=dasin(j_v(irg))
			return
820	j_o(iout)%d=dasin(j_v(irg))*j_todeg
			return
821	j_o(iout)%d=dacos(j_v(irg))
			return
822	j_o(iout)%d=dacos(j_v(irg))*j_todeg
			return
 
823	j_o(iout)%d=datan(j_v(irg))
			return
 
824	j_o(iout)%d=datan(j_v(irg))*j_todeg
			return
 
825	if(abs(j_v(irg)).gt.0.d-15)then  !cotan
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				j_o(iout)%d=datan(1.d0/j_v(irg))
 
			else !if(abs(j_v(irg)).gt.0.d-15)then
				write(6,*)'too small argument ',j_v(irg)
				j_err=.true.
			endif !825	if(abs(j_v(irg)).gt.0.d-15)   6581
 
826		if(abs(j_v(irg)).gt.0.d-15)then

				j_o(iout)%d=datan(1.d0/j_v(irg))*j_todeg  !cotand
 
			else !if(abs(j_v(irg)).gt.0.d-15)then
				write(6,*)'too small argument ',j_v(irg)
				j_err=.true.
			endif !826		if(abs(j_v(irg)).gt.0.d-15)   6590
			return
 
827	j_o(iout)%d=dsinh(j_v(irg))
			return
 
828 j_o(iout)%d=dcosh(j_v(irg))
			return
829	j_o(iout)%d=tanh(j_v(irg))
			return
830  j_o(iout)%d=fraction(j_v(irg))
			return
831	j_o(iout)%d=dabs(j_v(irg))
			return
 
 
		else
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			!		write(6,*)ioper
 
			goto (2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)ioper
			!abs!NOTE ILLOGICAL ORDER
 
 
2 	j_v(iout)=dnint(j_v(irg))
			return
			!abs
3 	j_v(iout)=dint(j_v(irg))
			return
 
			!abs
4 	j_v(iout)=ceiling(real(j_v(irg)))
			return
			!abs
5 	j_v(iout)=floor(real(j_v(irg)))
			return
 
			!sqrt
6 	if(j_v(irg).lt.j_0)then
				write(6,*)'negative argument in sqrt ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).lt.j_0)then
				j_v(iout)=dsqrt(j_v(irg))
			endif !6 	if(j_v(irg).lt.j_0)   6635
			return
 
			!sqrt2
7 	if(j_v(irg).eq.j_0)then
				j_v(iout)=j_0
			elseif(j_v(irg).gt.j_0)Then !if(j_v(irg).eq.j_0)then
				j_v(iout)=sqrt(j_v(irg))
			else !if(j_v(irg).eq.j_0)then
				j_v(iout)=-sqrt(-j_v(irg))
			endif !7 	if(j_v(irg).eq.j_0)   6644
			return
			!
8 	if(j_v(irg).lt.j_0)then
				write(6,*)'negative argument in log ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).lt.j_0)then
				j_v(iout)=dlog(j_v(irg))
			endif !8 	if(j_v(irg).lt.j_0)   6653
			return
			!abs
9 	if(j_v(irg).lt.j_0)then
				write(6,*)'negative argument in log10 ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).lt.j_0)then
				j_v(iout)=dlog10(j_v(irg))
			endif !9 	if(j_v(irg).lt.j_0)   6661
			return
10	if(j_v(irg).gt.88.d0)then
				write(6,*)'too large argument ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).gt.88.d0)then
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				j_v(iout)=dexp(j_v(irg))
			endif !10	if(j_v(irg).gt.88.d0)   6668
			return
			!abs
11 	j_v(iout)=dsin(j_v(irg))
			return
			!abs
12 	j_v(iout)=dsin(j_deg*j_v(irg))
			return
			!abs
13 	j_v(iout)=dcos(j_v(irg))
			return
			!abs
14 	j_v(iout)=dcos(j_v(irg)*j_deg)
			return
 
			!abs
15 	j_v(iout)=dtan(j_v(irg))
			return
			!tand
16 	j_v(iout)=dtan(j_deg*j_v(irg))
			return
17		if(dabs(j_v(irg)).lt.d-15)then
				write(6,*)'too close to zero ',j_v(irg)
				j_err=.true.
			endif !17		if(dabs(j_v(irg)).lt.d-15)   6695
 
			j_v(iout)=dtan(1.d0/j_v(irg))
			return
18	if(dabs(j_v(irg)).lt.d-15)then
				write(6,*)'too close to zero ',j_v(irg)
				j_err=.true.
			endif !18	if(dabs(j_v(irg)).lt.d-15)   6702
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=dtan(1.d0/(j_deg*j_v(irg)))
			return
19	j_v(iout)=dasin(j_v(irg))
			return
20	j_v(iout)=dasin(j_v(irg))*j_todeg
			return
21	j_v(iout)=dacos(j_v(irg))
			return
22	j_v(iout)=dacos(j_v(irg))*j_todeg
			return
 
23	j_v(iout)=datan(j_v(irg))
			return
 
24	j_v(iout)=datan(j_v(irg))*j_todeg
			return
 
25	if(abs(j_v(irg)).gt.0.d-15)then  !cotan
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				j_v(iout)=datan(1.d0/j_v(irg))
 
			else !if(abs(j_v(irg)).gt.0.d-15)then
				write(6,*)'too small argument ',j_v(irg)
				j_err=.true.
			endif !25	if(abs(j_v(irg)).gt.0.d-15)   6724
 
26		if(abs(j_v(irg)).gt.0.d-15)then

				j_v(iout)=datan(1.d0/j_v(irg))*j_todeg  !cotand
 
			else !if(abs(j_v(irg)).gt.0.d-15)then
				write(6,*)'too small argument ',j_v(irg)
				j_err=.true.
			endif !26		if(abs(j_v(irg)).gt.0.d-15)   6733
			return
 
27	j_v(iout)=dsinh(j_v(irg))
			return
 
28 j_v(iout)=dcosh(j_v(irg))
			return
29	j_v(iout)=tanh(j_v(irg))
			return
30  j_v(iout)=fraction(j_v(irg))
			return
31	j_v(iout)=dabs(j_v(irg))
			return
 
 
 
		endif !if(j_otype(iout).eq.j_ipmatrix.and.iout.le.j_named)   6472
 
	endif !if(j_otype(irg).eq.j_ipreal)   6470
	if(j_otype(irg).ne.j_ipmatrix)then
		write(6,*)'argument is not MATRIX or REAL'
		j_err=.true.;return
 
	endif !if(j_otype(irg).ne.j_ipmatrix)   6760
	if(j_otype(iout).ne.j_ipmatrix)ivout= &
		j_defmatrix(iout,' ',int8(j_o(irg)%i(1)),int8(j_o(irg)%i(2)),j_matreg)
	if(j_o(iout)%i(3).ne.j_o(irg)%i(3))then
		ivout=j_defmatrix(iout,' ',int8(j_o(irg)%i(1)),int8(j_o(irg)%i(2)),j_matreg)
	else !if(j_o(iout)%i(3).ne.j_o(irg)%i(3))then
		j_o(iout)%i(1:2)=j_o(irg)%i(1:2)
	endif !if(j_o(iout)%i(3).ne.j_o(irg)%i(3))   6767
 
	goto (92,93,94,95,96,97,98,99,910,911,912,913,914,915,916,917,918,919,920,&
		921,922,923,924,925,926,927,928,929,930,931)ioper
 
92 	j_o(iout)%d=dnint(j_o(irg)%d)
	return
	!abs
93 	j_o(iout)%d=dint(j_o(irg)%d)
	return
 
	!abs
94 	j_o(iout)%d=ceiling(real(j_o(irg)%d))
	return
	!abs
95 	j_o(iout)%d=floor(real(j_o(irg)%d))
	return
 
	!sqrt
96 	minva=minval(j_o(irg)%d)

	if(minva.lt.j_0)then
		write(6,*)'negative arguments, in sqrt, min= ',minva
		j_err=.true.
	else !if(minva.lt.j_0)then
		j_o(iout)%d=dsqrt(j_o(irg)%d)
	endif !if(minva.lt.j_0)   6792
	return
 
	!sqrt2
97 		minva=minval(j_o(irg)%d)
	do i=1,j_o(irg)%i(3)
		if(j_o(irg)%d(i).eq.j_0)then
			j_o(iout)%d(i)=j_0
		elseif(j_o(irg)%d(i).gt.j_0)Then !if(j_o(irg)%d(i).eq.j_0)then
			j_o(iout)%d(i)=sqrt(j_o(irg)%d(i))
		else !if(j_o(irg)%d(i).eq.j_0)then
			j_o(iout)%d(i)=-sqrt(-j_o(irg)%d(i))
		endif !if(j_o(irg)%d(i).eq.j_0)   6803
	end do !i=1,j_o(irg)%i(3)   6802
	return
	!
98 		minva=minval(j_o(irg)%d)
	if(minva.lt.j_0)then
		write(6,*)'negative arguments, in log, min= ',minva
		j_err=.true.
	else !if(minva.lt.j_0)then
		j_o(iout)%d=dlog(j_o(irg)%d)
	endif !if(minva.lt.j_0)   6814
	return
	!abs
99 		minva=minval(j_o(irg)%d)
	if(minva.lt.j_0)then
		write(6,*)'negative arguments in log10,  min= ',minva
		j_err=.true.
	else !if(minva.lt.j_0)then
		j_o(iout)%d=dlog10(j_o(irg)%d)
	endif !if(minva.lt.j_0)   6823
	return
910		minva=maxval(j_o(irg)%d)
	if(minva.gt.88.d0)then
		write(6,*)'too large arguments, max= ',minva
		j_err=.true.
	else !if(minva.gt.88.d0)then
		j_o(iout)%d=dexp(j_o(irg)%d)
	endif !if(minva.gt.88.d0)   6831
	return
	!abs
911 	j_o(iout)%d=dsin(j_o(irg)%d)
	return
	!abs
912 	j_o(iout)%d=dsin(j_deg*j_o(irg)%d)
	return
	!abs
913 	j_o(iout)%d=dcos(j_o(irg)%d)
	return
	!abs
914 	j_o(iout)%d=dcos(j_o(irg)%d*j_deg)
	return
 
	!abs
915 	j_o(iout)%d=dtan(j_o(irg)%d)
	return
	!tand
916 	j_o(iout)%d=dtan(j_deg*j_o(irg)%d)
	return
917			minva=minval(dabs(j_o(irg)%d))
	if(minva.lt.d-15)then
		write(6,*)'too close to zero, minimum absolute value ',minva
		j_err=.true.
	endif !if(minva.lt.d-15)   6858
	j_o(iout)%d=dtan(1.d0/j_o(irg)%d)
	return
 
918		minva=minval(dabs(j_o(irg)%d))
	if(minva.lt.d-15)then
		write(6,*)'min absolute value too close to zero ',minva
		j_err=.true.
	else !if(minva.lt.d-15)then
		j_o(iout)%d=dtan(1.d0/(j_deg*j_o(irg)%d))
	endif !if(minva.lt.d-15)   6866
 
 
	return
919	j_o(iout)%d=dasin(j_o(irg)%d)
	return
920	j_o(iout)%d=dasin(j_o(irg)%d)*j_todeg
	return
921	j_o(iout)%d=dacos(j_o(irg)%d)
	return
922	j_o(iout)%d=dacos(j_o(irg)%d)*j_todeg
	return
 
923	j_o(iout)%d=datan(j_o(irg)%d)
	return
 
924	j_o(iout)%d=datan(j_o(irg)%d)*j_todeg
	return
 
925		minva=minval(dabs(j_o(irg)%d))
	if(minva.gt.0.d-15)then  !cotan
		j_o(iout)%d=datan(1.d0/j_o(irg)%d)
	else !if(minva.gt.0.d-15)then
		write(6,*)'too small min absolute argument ',minva
		j_err=.true.
	endif !if(minva.gt.0.d-15)   6891
 
926			minva=minval(dabs(j_o(irg)%d))
	if(minva.gt.0.d-15)then
 
		j_o(iout)%d=datan(1.d0/j_o(irg)%d)*j_todeg  !cotand
 
	else !if(minva.gt.0.d-15)then
		write(6,*)'too small min absolute argument ',minva
		j_err=.true.
	endif !if(minva.gt.0.d-15)   6899
	return
 
927	j_o(iout)%d=dsinh(j_o(irg)%d)
	return
 
928 j_o(iout)%d=dcosh(j_o(irg)%d)
	return
929	j_o(iout)%d=tanh(j_o(irg)%d)
	return
930  j_o(iout)%d=fraction(j_o(irg)%d)
	return
931	j_o(iout)%d=dabs(j_o(irg)%d)
	return
 
 
 
 
	return
 
 
 
end subroutine !subroutine UNIOP(iob,io,ioper)
 
 
! subroutine utf8(iob,io)
! !integer, dimension(:),allocatable :: ocoding  utf8
! !subroutine j_startfunction(iob,io,iptype,narg,arg,ivout)
! integer,dimension(:),allocatable::oint
! integer, dimension(:), pointer :: arg !arguments of the function
! !j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout)
! call j_startfunction(iob,io,j_ipchar,narg,arg,ivout)
! if(j_err)return
! call j_getfile(nu,'r',ivfile=arg(1))
! if(j_err)return
 
! read(nu,'(a)',err=90)j_cline
 
! j_nutf8=len_trim(j_cline)
! if(allocated(j_utf8))deallocate(j_utf8)
! allocate(j_utf8(1:j_nutf8))
! if(allocated(j_ocode))deallocate(j_ocode)
! allocate(j_ocode(1:j_nutf8))
 
! do j=1,j_nutf8
! j_utf8(j)=j_cline(j:j)
! end do !do j=1,j_nutf8
 
! if(narg.eq.1)then
! allocate(oint(1:j_nutf8))
! read(nu,*,end=90,err=90)oint
! do jj=1,j_nutf8
! j_ocode(jj)=char(oint(jj))
! enddo !do jj=1,j_nutf8
! deallocate(oint)
 
! else !if(narg.eq.1)then
! call j_getfile(nu2,'w',ivfile=arg(2))
! if(j_err)return
! write(nu2,'(a)',err=90)j_cline(1:j_nutf8)
! call j_printname('paste the first line from file ',arg(1), 'here and press return')
! read(5,'(a)',err=90)j_cline
! do jj=1,j_nutf8 ; j_ocode(jj)=j_cline(jj:jj);enddo
! !	write(nu2,'(a)',err=90)j_ocode(1:j_nutf8)
! write(nu2,*,err=90)(ichar(j_cline(jj:jj)),jj=1,j_nutf8)
! write(nu2,*,err=90)(ichar(j_utf8(jj)),jj=1,j_nutf8)
! call j_closeunit(nu2)
! endif !if(narg.eq.1)then
! call j_closeunit(nu)
! return
! 90 write(6,*)'*utf8: reading error'
! j_err=.true.
! call j_closeunit(nu)
! end subroutine utf8 !subroutine utf8(iob,io)
 
 
subroutine index_o(iob,io)
	!Section indexo index_o() Index of the first argument in the LIST of second argument
	!endsection
	integer,intent(in)::iob
	integer,intent(in)::io
	integer, dimension(:), pointer :: arg !arguments of the function
	!	io_=io
	call j_startfunction(iob,io,0,narg,arg,ivout)
	if(j_err)return
	if(j_otype(arg(2)).ne.j_iplist)then
		call j_printname(' ',arg(2), ' not a list')
		j_err=.true.
		return
	endif !if(j_otype(arg(2)).ne.j_iplist)   6991
	inde=j_v(arg(1))
	if(inde.lt.1.or.inde.gt.j_o(arg(2))%i(1))then
		write(6,*)'*illegal index ',inde, 'for list with len=',j_o(arg(2))%i(1)
		j_err=.true.
	else !if(inde.lt.1.or.inde.gt.j_o(arg(2))%i(1))then
		!narg=j_o(iob)%i(io+1)  !note out put is not put into the output variable
		!ivout=j_o(iob)%i(io+2+narg)
		j_o(iob)%i(io+j_o(iob)%i(io+1)+2)=j_o(arg(2))%i2(inde)
	endif !if(inde.lt.1.or.inde.gt.j_o(arg(2))%i(1))   6997
	return
 
end subroutine !subroutine index_o(iob,io)
 
 
subroutine find(iob,io)
 
	!Section find find() Finds from a MATRIX
	!Function find() can be used to find the first matrix element satisfying a given condition, or
	! all matrix elements satifying the conditon, and in that case the found
	! elements can be put to a vector containg element numbers or to a
	!vector which has equal size as the input matrix and where 1 indicates that
	! the element satifies the condition..
	!Remember that matrices are stored in row order. If a given column or row of matrix A should be seaeched,
	! use A(All,column) or A(row,ALL) to extract that row or column.
	!endheader
	!Option
	!Output& 1& REAL|MATRIX&Without any-> or expand-> the first element found in row order.
	!With any->, the vector of element numbers satisfying the conditon. If nothing found
	! the output will be REAL with value zero.
	!With expand->, the matrix of the same dimensions as the input matrix where
	! hits are marked with 1.
	!Args&1&Matrix& The matrix searched.
	!filter&1&Code&The condition which the matrix element should be satisfied. The
	! values of the matrix elements are put to the variable $.
	!any&-1|0& & The filtered element numbers are put to the output vector.
	!expand&-1|0&& The filtered elements are put the output matrix
	!endoption
	!Ex  findex Finding something from matrix
	! ** Repeating the example, different results will be obtained
	! rm=matrix(500)
	! m,s=2,3
	! rm=rann(m,s)
	! mean(rm),sd(rm),min(rm),max(rm);
	! m+1.96*s;
	! ** index of first row satisfying the condition:
	! first=find(rm,filter->($.ge.m+1.96*s));
	! ** indeces of all rows satisfying the condition
	! large=find(rm,filter->($.ge.m+1.96*s),any->);
	! nrows(large),nrows(large)/nrows(rm),mean(large),sd(large),min(large),max(large);
	! ** vector of equal size as rm containing 1 or 0
	! large2=find(rm,filter->($.ge.m+1.96*s),expand->)
	! mean(large2),min(large2),max(large2);
	!endex
	!endsection
	integer,intent(in)::iob
	integer,intent(in)::io
 
	integer, dimension(:), pointer :: arg !arguments of the function
	integer, dimension(:), pointer :: rows !arguments of the function
	integer, dimension(:), pointer :: cols !arguments of the function
	integer,dimension(:),allocatable :: found
	double precision:: filter
	logical:: isany,isexpand
	integer*8 ::ncols,nrows,nel,nfound
	! j_optarg0
 
	call j_startfunction(iob,io,j_ipmatrix,narg,arg,ivout,delout=.true.)
	if(j_err)return
 
	call j_getoption(iob,io,j_mrow,-1,1,j_ipreal,.true.,noprows,rows)
	if(j_err)return
	call j_getoption(iob,io,j_mcolumn,-1,1,j_ipreal,.true.,nopcols,cols)
	if(j_err)return
	if(noprows.ge.0.or.nopcols.ge.0)then
		write(6,*)'row-> and column-> not yet implemented'
		j_err=.true.
		return
	endif !if(noprows.ge.0.or.nopcols.ge.0)   7069
	!write(6,*)'infind'
	link=j_codelink(iob,io,j_mfilter); if(j_err)return
	! if(j_linkopt2(j_mfilter).gt.0)then
 
	! iofilter=j_linkopt2(j_mfilter)
	! dofilter=iofilter.ne.0
	! ivfilter=j_o(iob)%i(j_linkopt2(j_mfilter)-1)
	! write(6,*)'iofilter,ivfilter',iofilter,ivfilter
	if(link.le.0)then !if(j_linkoption(iob,io,j_mfilter).gt.0)then
		write(6,*)'* filter-> missing'
		j_err=.true. ;return
	end if !if(link.le.0)   7082
	!j_getoption(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
	call j_getoption(iob,io,j_many,-1,0,0,.false.,nany,j_optarg0)
	if(j_err)return
	isany=nany.ge.0
	call j_getoption(iob,io,j_mexpand,-1,0,0,.false.,nexpand,j_optarg0)
	if(j_err)return
	isexpand=nexpand.ge.0
	!!call j_clearoption(iob,io)  ! subroutine
 
 
	nrows=j_o(arg(1))%i(1)
	ncols=j_o(arg(1))%i(2)
	nel=j_o(arg(1))%i(3)
	if(isexpand)then
		ive=j_defmatrix(ivout,' ',nrows,ncols,j_matreg)
	elseif(isany)then !if(isexpand)then
		allocate(found(1:nel))
	else
		if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
	endif !if(isexpand)   7099
	nfound=0
	!	if(nrows.eq.1.or.ncols.eq.1)then
	do i=1,nel
		j_v(j_ivdollar)=j_o(arg(1))%d(i)
 
 
		filter=j_codevalue(iob,link)
		!	write(6,*)'<77 ',j_v(j_ivdollar),filter
		if (j_err)then
			if(allocated(found))deallocate(found)
			return
		endif !if (j_err)   7114
		if(filter.ne.j_0)then
			if(isexpand)then
				j_o(ivout)%d(i)=j_1
			elseif(isany)then !if(isexpand)then
				nfound=nfound+1
				found(nfound)=i
			else !if(isexpand)then
				j_v(ivout)=i
				return
			endif !if(isexpand)   7119
		endif !if(filter.ne.j_0)   7118
	enddo !i=1,nel   7108
	if(isexpand)return
	if(isany)then
		if(nfound.gt.0)then
			ive=j_defmatrix(ivout,' ',nfound,j_18,j_matreg)
			j_o(ivout)%d=found(1:nfound)
		else !if(nfound.gt.0)then
			write(6,*)'*wrn* nothing found, output is REAL zero'
			j_v(ivout)=j_0
		endif !if(nfound.gt.0)   7132
		deallocate(found)
	else !if(isany)then
		j_v(ivout)=j_0
	endif !if(isany)   7131
	return
 
end subroutine find !subroutine find(iob,io)
 
subroutine matrixstat(iob,io,ifunc)
 
	! Section matrixstat Statistical functions for matrices
	! Functions mean(), sd(), var(), sum(), min() and max()
	! can be used used to compute statistics from a matrix. Let mean() here present
	! any of thes functions. The following rules apply:
	! \begin{itemize}
	!   mean(VECTOR) computes the mean of the vector, output is REAL
	!   mean(MATRIX) computes the mean of the each column. Result is row vector.
	!   mean(VECTOR,weight->wvector) computes the
	! weigted mean of the vector,weights being in vector wvector.
	!   mean(MATRIX,weight->wvector) computes the
	! weighted mean of each column, weights being in vector wvector, result is row vector.
	!mean(MATRIX,any->) computes the mean of the whole MATRIX, output is REAL
	! \end{itemize}
	! endsection
 
	! Section mean mean() Means or weighted means
	!
	! endsection
 
	! Section sd sd() Sd's or weighted sd's
	!
	! endsection
 
	! Section var var() Sample variances or weighted variances
	!
	! endsection
 
	! Section sum sum() Sums or weighted sums
	!
	! endsection
	integer,intent(in)::iob
	integer,intent(in)::io
 
	integer, dimension(:), pointer :: arg !arguments of the function
	integer, dimension(:), pointer :: rows !arguments of the function
	integer, dimension(:), pointer :: cols !arguments of the function
	double precision,dimension(:),allocatable:: sum2
	double precision ::suma,suma2,mean,dcoef,dcoef1,wsum
	logical::need2
	integer*8 ::ncols,nrows,nel
	logical::isany
	!ifunc=1  =>mean
	!ifunc=2  =>sum
	!ifunc=3  =>var
	!ifunc=4  =>sd
	! j_optarg0
	call j_startfunction(iob,io,0,narg,arg,ivout)
	if(j_err)return
	!j_getoption(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
	isany=j_isoption(iob,io,j_many)
 
	need2=ifunc.ge.3
	imat=arg(1)
	if(j_otype(imat).eq.j_ipreal)then
		if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
		if(ifunc.le.2)then
			j_v(ivout)=j_v(imat)
		else
			j_v(ivout)=j_0
		endif !if(ifunc.le.2)   7204
		return
	endif !if(j_otype(imat).eq.j_ipreal)   7202
 
	nrows=j_nrows(imat)  !j_o(imat)%i(1)
	ncols=j_ncols(imat)  !j_o(imat)%i(2)
	nel=j_nelem(imat)  !j_o(imat)%i(3)
	!write(17,*)'mstat',narg,arg
	!	return
	call j_getoption(iob,io,j_mfreq,-1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
	if(j_err)return
	!write(6,*)'noptarg',noptarg,j_optarg0(1)
	if(isany)then
		ncols=j_18
 
 
	endif !if(isany)   7220
	!!call j_clearoption(iob,io)
 
	! j_getoption(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
 
 
	if(noptarg.gt.0)then
		iwe=j_optarg0(1)
 
		if(ncols.eq.1.or.nrows.eq.1)then
			if(j_o(iwe)%i(3).ne.nel)then
				call j_getname(imat,iwe)
				write(6,*)'weight ',j_oname2(1:j_loname2), 'does not have same size as ',&
					j_oname(1:j_loname)
				j_err=.true. ; return
			endif !if(j_o(iwe)%i(3).ne.nel)   7234
			if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
			!dcoef=j_1/nel
			!dcoef1=j_1/(nel-j_1)
			suma=dot_product(j_o(imat)%d(1:nel),j_o(iwe)%d(1:nel))
			wsum=sum(j_o(iwe)%d(1:nel))
 
			mean=suma/wsum
			if(need2)then
				suma2=dot_product(j_o(imat)%d(1:nel)-mean,j_o(imat)%d(1:nel)-mean)
 
				if(ifunc.ge.3)then
					j_v(ivout)=suma2/(nel-j_1)
 
					if(ifunc.eq.4)	j_v(ivout)=sqrt(j_v(ivout))
				endif !if(ifunc.ge.3)   7250
				return
			endif !if(need2)   7247
 
			if(ifunc.eq.1)then
				j_v(ivout)=mean
			else !if(ifunc.eq.1)then
				j_v(ivout)=suma
 
			endif !if(ifunc.eq.1)   7258
			return
 
		endif !if(ncols.eq.1.or.nrows.eq.1)   7233
		if(j_o(iwe)%i(1).ne.1.and.j_o(iwe)%i(2).ne.1)then
			call j_getname(iwe)
			write(6,*)'weight ',j_oname(1:j_loname),' is not vector but ',j_o(iwe)%i(1:2)
			j_err=.true.
			return
		endif !if(j_o(iwe)%i(1).ne.1.and.j_o(iwe)%i(2).ne.1)   7267
		ivout=j_defmatrix(ivout,' ',j_18,ncols,j_matreg)
 
 
		ibas=0
		do i=1,nrows
 
			j_o(ivout)%d=j_o(ivout)%d+j_o(iwe)%d(i)*j_o(imat)%d(ibas+1:ibas+ncols)
			ibas=ibas+ncols
		end do !i=1,nrows   7277
		wsum=sum(j_o(iwe)%d(1:j_o(iwe)%i(3)))
		if(ifunc.eq.2)return
		j_o(ivout)%d=j_o(ivout)%d/wsum
		if(ifunc.eq.1)return
		if(allocated(sum2))deallocate(sum2)
		allocate(sum2(1:ncols))
		sum2=j_0
		ibas=0
		do i=1,nrows
 
			sum2=sum2+j_o(iwe)%d(i)*(j_o(imat)%d(ibas+1:ibas+ncols)-j_o(ivout)%d)**2
			ibas=ibas+ncols
		end do !i=1,nrows   7290
		j_o(ivout)%d=sum2/wsum
		deallocate(sum2)
		if(ifunc.eq.3)return
		j_o(ivout)%d=sqrt(j_o(ivout)%d)
		return
 
		!weighted
 
	endif !if(noptarg.gt.0)   7230
	!not weighted
	if(ncols.eq.1.or.nrows.eq.1)then
		if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
		dcoef=j_1/nel
		dcoef1=j_1/(nel-j_1)
		suma=sum(j_o(imat)%d(1:nel))
		mean=dcoef*suma
		if(need2)then
 
			suma2=j_0
			do i=1,nel
				suma2=suma2+(j_o(imat)%d(i)-mean)**2
			enddo !i=1,nel   7314
			suma2=dcoef1*suma2
			if(ifunc.eq.3)then
				j_v(ivout)=suma2
			else !if(ifunc.eq.3)then
				j_v(ivout)=sqrt(suma2)
			endif !if(ifunc.eq.3)   7318
			return
		endif !if(need2)   7311
 
		if(ifunc.eq.1)then
			j_v(ivout)=mean
		else !if(ifunc.eq.1)then
			j_v(ivout)=suma
 
		endif !if(ifunc.eq.1)   7326
		return
 
	endif !if(ncols.eq.1.or.nrows.eq.1)   7305
 
	ivout=j_defmatrix(ivout,' ',j_18,ncols,j_matreg)
	dcoef=j_1/nrows
	dcoef1=j_1/(nrows-j_1)
 
	ibas=0
	do i=1,nrows
 
		j_o(ivout)%d=j_o(ivout)%d+j_o(imat)%d(ibas+1:ibas+ncols)
		ibas=ibas+ncols
	end do !i=1,nrows   7341
	if(ifunc.eq.2)return
	j_o(ivout)%d=dcoef*j_o(ivout)%d
	if(ifunc.eq.1)return
	if(allocated(sum2))deallocate(sum2)
	allocate(sum2(1:ncols))
	sum2=j_0
	ibas=0
	do i=1,nrows
 
		sum2=sum2+(j_o(imat)%d(ibas+1:ibas+ncols)-j_o(ivout)%d)**2
		ibas=ibas+ncols
	end do !i=1,nrows   7353
	j_o(ivout)%d=dcoef1*sum2
	deallocate(sum2)
	if(ifunc.eq.3)return
	j_o(ivout)%d=sqrt(j_o(ivout)%d)
	return
 
end subroutine matrixstat !subroutine matrixstat(iob,io,ifunc)
 
 
 
subroutine class(iob,io)
	!Section class class() Class of a given value
	!Function class() computes the class of given value when classifying values
	!similarly as done in classify().
	!endheader
	!Option
	!Output&1&REAL&The class number.
	!Args&1&Real&The value whose class is determined.
	!xrange&2&Real&The range of values.
	!dx&N|1&Real&The class width.
	!classes&N|1&Real&The number of classes.
	!endoption
	!Note Either dx-> or classes-> must be given. If both are given, dx-> dominates.
	!endnote
	!Note If stat() is used earlier for variables including Var1 and
	! options min-> and max-> are present, then
	!xrange->(Var1%min,Var1%max) is assumed.
	!endnote
	!endsection
	integer,intent(in)::iob
	integer,intent(in)::io
	integer, dimension(:), pointer :: arg
	double precision xmin,xmax,dx,clasn
	call j_startfunction(iob,io,0,narg,arg,ivout)
	if(j_err)return
	call j_getoption(iob,io,j_mxrange,2,2,j_ipreal,.true.,nr,j_optarg0)
	if(j_err)return
 
	xmin=j_v(j_optarg0(1))
	xmax=j_v(j_optarg0(2))
 
 
	if(xmax.le.xmin)then
		write(6,*)'illegal xrange->'
		j_err=.true.
		return
	endif !if(xmax.le.xmin)   7400
	if(j_v(arg(1)).lt.xmin.or.j_v(arg(1)).gt.xmax)then
		j_v(ivout)=j_0
		return
	endif !if(j_v(arg(1)).lt.xmin.or.j_v(arg(1)).gt.xmax)   7405
 
	call j_getoption(iob,io,j_mdx,-1,1,j_ipreal,.true.,ndx,j_optarg0)
	if(j_err)return
	if(ndx.eq.1)then
		dx=j_v(j_optarg0(1))
		if(dx.le.j_0)then
			write(6,*)'illegal dx->'
			j_err=.true.
			return
		endif !if(dx.le.j_0)   7414
	endif !if(ndx.eq.1)   7412
 
	call j_getoption(iob,io,j_mclasses,-1,1,j_ipreal,.true.,nc,j_optarg0)
	if(j_err)return
	if(nc.eq.1)clasn=j_v(j_optarg0(1))
	if(nc.gt.0.and.ndx.gt.0)then
		write(6,*)'both dx-> and classes->, dx dominates'
		nc=0
	endif !if(nc.gt.0.and.ndx.gt.0)   7424
	!!call j_clearoption(iob,io) ! subroutine
	if(nc.gt.0)dx=1.000001*(xmax-xmin)/clasn
	nval=(xmax-xmin)/dx
	j=(j_v(arg(1))-xmin)/dx+1
	j=min(j,nval)
	j_v(ivout)=j
	return
 
end subroutine class !subroutine class(iob,io)
 
subroutine newdata(iob,io)
	!Section newdata newdata() Making a DATA from MATRIXs and/or DATAs
	!Function newdata() generates a new data object from existing data objects and/or
	!matrices possibly using transformations to generate new variables.
	!endheader
	!Option
	!Output&1&Data&The data object generated.
	!Args&1-&Data|Matrix& Input matrices and data objects.
	!read&N|1-&REAL& Variable names for columns of matrices in the order of
	!matrices.
	!maketrans&N|1& TRANS & A predefined ransformation object computed for each observation.
	!time&-1|0& & If time-> is present, the cpu-time and total time in function are printed
	!delete&-1|0|1 &REAL& If present, then the new data matrix is made sequentially so that used data
	! matrices are deleted. This takes more time, but may be needed if there is shortage of
	!memory.
	!endoption
	!Note If a DATA has a link to an upper level DATA obtained with linkdata() without
	!output, the the upper level DATA is not included. You can make a link to an upper level data
	! using linkdata() for the DATA produced with newdata()
	!endnote
	!Note If observations are dropped using filter-> or reject-> and there is maketrans->, then
	! the maketerans trasformation get the initial obsevation from variable Record and the new
	! observation number from varaiable Obs
	!endnote
	!Note It is not yet possible to drop variables.
	!endnote
	!Note An error occurs if the same variable is several times in the variable list obtained
	!by combining variables in data sets and read-> variables.
	!endnote
	!Note An error occurs if the numbers of rows of matrices and observations in data sets
	! are not compatible.
	!endnote
	!Note Output variables in maketrans-> transformations whose name start with $ are not put into the new data object.
	!endnote
	!Ex newdataex newdata() generates a new data object.
	!data1=data(read->(x1...x3),in->)
	!1,2,3
	!4,5,6
	!7,8,9
	!/
	!matrix1=matrix(3,2,in->)
	!10,20
	!30,40
	!50,60
	!/
	! transa=trans()
	! ;do(i,1,3)
	! ;do(j,1,2)
	! x"i"#z"j"=x"i"*z"j"
	! ;enddo
	! ;enddo
	! /
	!new=newdata(data1,matrix1,read->(z1,z2),maketrans->transa)
	!print(new)
	!endex
	!endsection
	integer,intent(in)::iob
	integer,intent(in)::io
	integer, dimension(:), pointer :: arg,readv
	!	integer, dimension(:), pointer :: readv
	integer, dimension(:),allocatable :: arg2 ! matrices of arguments
	!integer, dimension(:),allocatable :: vars !all variables
	integer*8, dimension(:),allocatable :: ibas0 !variables of argument
	integer, dimension(:),allocatable :: nvar0
	integer, dimension(:),pointer :: extra !variables of argument
	double precision,dimension(:),allocatable :: temp
	logical maketran,isappend,ismatrix,isdata,istime,istrans,isup,isdelete
	integer*8::nob,nkeep,ncols,ibas,ibas2,ibas1,iobs1,nel,ntot,nobstot,noutv8
 
	call j_startfunction(iob,io,0,narg,arg,iout)
 
	if(j_err)return
	ivobs=j_getobject(iout,'%obs',j_ipreal);if(j_err)return
	ndata=0
	! isup=.false.
	! do i=1,narg
	! if(j_otype(arg(i)).eq.j_ipdata)then
	! ndata=ndata+1
	! if(j_o(arg(i))%i(5).ne.0)isup=.true.
	! endif !if(j_otype(arg(i)).eq.j_ipdata)   7176
	isdelete=j_isoption(iob,io,j_mdelete,.false.)
	if(isdelete.and.narg.le.2)then
		write(6,*)'with only ',narg, 'arguments delete-> is ignored'
		isdelete=.false.
 
	endif !if(isdelete.and.narg.le.2)   7519
	! enddo !i=1,narg   7175
 
	! isup=.false.
	! if(isup.and.ndata.gt.1)then
	! write(6,*)'newdata cannot combine several DATAs with upper-level data'
	! j_err=.true.;return
 
	! endif !if(isup.and.ndata.gt.1)   7184
 
	call j_getname(iout)
	write(6,*)
	write(6,*)'Making newdata: ',j_oname(1:j_loname)
	allocate(arg2(1:narg),nvar0(1:narg),ibas0(1:narg))
	nextra0=0
	istime=j_isoption(iob,io,j_mtime)
	if(istime)then
		call cpu_time(cpu0)
		time0=secnds(0.)
	endif !if(istime)   7539
	call j_getoption(iob,io,j_mextra,-1,999,j_ipreal,&
		.true.,nextra,extra);if(j_err)return
	if(nextra.lt.0)nextra=0
	call j_getoption(iob,io,j_mmaketrans,-1,1,j_iptrans,.true.,noptarg,j_optarg)
	if(j_err)return
	!	write(6,*)'makenop',noptarg,j_optarg0(1)
	if(noptarg.gt.0)then
		maketran=.true.
		ivmaketrans=j_optarg(1)
		ivoul=j_trans_output(ivmaketrans)
		noutv=j_o(ivoul)%i(1)
		if(noutv.le.0)then
			call j_getname(ivmaketrans)
			write(6,*)'maketrans transforamtion ',j_oname,' does not have output variables'
			j_err=.true.;return
		endif !if(noutv.le.0)   7554
		!	write(6,*)'<4554',noutv,ivoul,j_o(ivoul)%i2(1:noutv)
	else !if(noptarg.gt.0)then
		maketran=.false.
		noutv=0
	end if !if(noptarg.gt.0)   7549
 
	if(narg.le.0)then
		call j_getoption(iob,io,j_mnobs,1,1,j_ipreal,.true.,noptarg,j_optarg)
		if(j_err)return
		nobs=j_v(j_optarg(1))
		if(noutv.le.0)then
 
			write(6,*)'nobs-> has illegal value ',nobs
			j_err=.true.;return
		endif !if(noutv.le.0)   7569
		ivmat=j_defmatrix(iout,'%matrix',int8(nobs),int8(noutv),j_matreg)
		call j_getname(ivmat)
		write(6,*)' matr ',j_oname(1:j_loname)
		ibas=0
		do i=1,nobs
			j_v(ivobs)=i
			j_v(j_ivobs)=i
			call dotrans(ivmaketrans,1)
			if(j_err)then
				write(6,*)'error occured for observation ',i
				j_err=.true.; return
			endif !if(j_err)   7582
			j_o(ivmat)%d(ibas+1:ibas+noutv)=j_v(j_o(ivoul)%i2(1:noutv))
			ibas=ibas+noutv
		enddo !i=1,nobs   7578
		ivkeep=j_deflist(iout,'%keep',ivin=ivoul)
		call j_defdata(iout,ivmat,ivkeep)
		j_dlastdata=iout
		return
	endif !if(narg.le.0)   7565
 
	!there are arguments
	if(isdelete.and.maketran)then
		write(6,*)'with delete-> maketrans-> is not allowed'
		j_err=.true.; return
	endif !if(isdelete.and.maketran)   7596
	call	j_getoption(iob,io,j_mread,-1,999999,-j_ipreal,.true.,nread,readv)
	if(j_err)return
	!	write(6,*)'<55,ivobs,arg',ivobs,arg
	ismatrix=.false.
	isdata=.false.
	nmatrix=0
	nvarmatrix=0
	nvar=0 !nuber of variables from matrices or datas
	nob=0
	isappend=j_linkoption(iob,io,j_mappend,clear=.true.).ge.0
 
	! j_o(iv)%i(1)=ivmat;j_o(iv)%i(2)=ivkeep;j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	! j_o(iv)%i(5)=ivup;j_o(iv)%i(6)=ivobs;
	! j_o(iv)%i(7)=ivobsw ;j_o(iv)%i(8)=ivnobswcum;j_o(iv)%i(9)=0
	!arg2 will be the matrix either obtained directly or from the data
	!write(6,*)'<767narg',narg
	if(isappend)then
		ncols=0
		do i=1,narg
			if(j_otype(arg(i)).eq.j_ipmatrix)then
				ivmat=arg(i)
				if(i.eq.1)then
					ncols=j_o(ivmat)%i(2)
					!					call	j_getoption(iob,io,j_mread,ncols,ncols,j_ipreal,.true.,noptarg,j_optarg0)
					if(nread.ne.ncols)then
						call j_getname(arg(i))
						write(6,*)'*matrix ',j_oname(1:j_loname),' has ',ncols,' columns but read-> has ',nread,' arguments'
						j_err=.true.;return
					endif !if(nread.ne.ncols)   7624
					if(j_err)return
 
				elseif(j_otype(arg(1)).ne.j_ipmatrix)then
					write(6,*)'*the first argument is DATA ',&
						' but arg ', i,' is MATRIX, with append-> all arguments must be either DATA or MATRIX'
					j_err=.true.;return
				endif !if(i.eq.1)   7621
				ismatrix=.true.
				arg2(i)=ivmat		!arg2 is matrix
 
				if(ncols.ne.j_o(ivmat)%i(2))then
					write(6,*)'arg ',i,' has ', ncols, 'columns,but first matrix ',j_o(arg(1))%i(2),&
						' with append-> all matrices must have the same number of columns'
					j_err=.true.;return
				endif !if(ncols.ne.j_o(ivmat)%i(2))   7639
			elseif(j_otype(arg(i)).eq.j_ipdata)then !if(j_otype(arg(i)).eq.j_ipmatrix)then
 
				ivmat=j_getmatrix(arg(i));if(j_err)return !j_o(arg(i))%i(1)
				arg2(i)=ivmat
				if(i.eq.1)then
					ncols=j_o(ivmat)%i(2)
					ivkeep1=j_getkeep(arg(1));if(j_err)return !j_o(arg(1))%i(2)
				else
					if(j_otype(arg(1)).ne.j_ipdata)then
						write(6,*)'*the first argument is MATRIX but argument ',i,&
							' is DATA with append-> all arguments must be either DATA or MATRIX'
						call j_getname(arg(i))
						write(6,*)'hint: take the matrix of data with ',j_oname(1:j_loname),'%matrix'
						j_err=.true.;return
					endif !if(j_otype(arg(1)).ne.j_ipdata)   7652
					if(j_o(ivmat)%i(2).ne.ncols)then
						write(6,*)'arg ',i,' has ',j_o(ivmat)%i(2),' vars but first matrix ',ncols, &
							' with append-> all matrices mus have the same number of colums'
						j_err=.true.;return
					endif !if(j_o(ivmat)%i(2).ne.ncols)   7659
					ivkeep=j_getkeep(arg(1));if(j_err)return  !j_o(arg(1))%i(2)
					if(.not.all(j_o(ivkeep1)%i2(1:ncols).eq.j_o(ivkeep)%i2(1:ncols)))then
						write(6,*)'*arg ',i, ' does not have same keep-variables as the previous args'
						write(6,*)'hint: take matrices using ..%matrix and define names with read->'
						j_err=.true.;return
					endif !if(.not.all(j_o(ivkeep1)%i2(1:ncols).eq.j_o(ivkeep)%i2(1:n   7665
					arg2(i)=ivmat
				endif !if(i.eq.1)   7648
 
				!write(6,*)'77',arg2(i)
			else !if(j_otype(arg(i)).eq.j_ipmatrix)then
				call j_printname('argument ',arg(i), ' is not DATA or MATRIX')
				j_err=.true.
				return
			endif !if(j_otype(arg(i)).eq.j_ipmatrix)   7619
			nob=nob+j_nrows(ivmat)  !j_o(ivmat)%i(1)
 
			!	call j_getname(arg(i))
			!			write(6,*)'<666 ',i,nvar0(i),nvar,j_oname(1:j_loname),nob
		enddo !i=1,narg   7618
		!	write(6,*)'nob',nob
 
		j_filter=j_codelink(iob,io,j_mfilter).ne.0; if(j_err)return
 
		! if(j_linkoption(iob,io,j_mfilter).gt.0)then
		! !	write(6,*)'<1po34',j_linkoption(iob,io,j_mfilter)
		! j_filter=.true.
		! j_iofilter=j_linkopt2(j_mfilter)
		! j_ivfilter=j_o(iob)%i(j_linkoption(iob,io,j_mfilter)+1)
		! !write(6,*)'iofilter,ivfilter',iofilter,ivfilter
		! else !if(j_linkoption(iob,io,j_mfilter).gt.0)then
		! j_filter=.false.
		! endif !if(j_linkoption(iob,io,j_mfilter).gt.0)then
 
		j_reject=j_codelink(iob,io,j_mreject).ne.0; if(j_err)return
		if(j_reject.or.j_filter)then
			write(6,*)'*filter-> and reject-> do not work with append->'
			j_err=.true.;return
 
		endif !if(j_reject.or.j_filter)   7699
 
		!	write(6,*)'<48848',j_reject,j_filter,j_filterlink,j_rejectlink
		! if(j_linkoption(iob,io,j_mreject).gt.0)then
		! !	write(6,*)'<1po37',j_linkoption(iob,io,j_mreject)
		! j_reject=.true.
		! j_ioreject=j_linkopt2(j_mreject)
		! j_ivreject=j_o(iob)%i(j_linkoption(iob,io,j_mreject)+1)
		! !write(6,*)'ioreject,ivreject',ioreject,ivreject
		! else !if(j_linkoption(iob,io,j_mreject).gt.0)then
		! j_reject=.false.
		! endif !if(j_linkoption(iob,io,j_mreject).gt.0)then
 
 
 
		!!call j_clearoption(iob,io)  ! subroutine
 
		nvartot=ncols+noutv+nextra
 
		!	allocate(vars(1:nvar))
		!write(6,*)'<4664,nvartot,nvar,noutv,nextra,ivkeep',nvartot,nvar,noutv,nextra
		ivkeep=j_deflist(iout,'%keep',nres=nvartot)
		!		write(6,*)'<55775 ',ncols,noutv,nextra,nvartot,j_o(ivkeep)%i(1)
		nr=0
		nv=0
		if(ismatrix)then
			j_o(ivkeep)%i2(1:ncols)=j_optarg0(1:ncols)
		else
			j_o(ivkeep)%i2(1:ncols)=j_o( j_o(arg(1))%i(2))%i2(1:ncols)
		endif !if(ismatrix)   7728
		j_o(ivkeep)%i(1)=ncols
		!write(6,*)'nextra',nextra,'?',extra
		if(maketran)iper=j_putlistobject(ivkeep,ivin=ivoul)
		!	write(6,*)'extra ',nextra,extra,nvartot,ivkeep
		if(nextra.gt.0)then
			do ij=1,nextra
				iper=j_inlistobject(extra(ij),ivkeep)
				if(iper.gt.0)then
					nvartot=nvartot-1
				else !if(iper.gt.0)then
					iper=j_putlistobject(ivkeep,single=extra(ij))
				endif !if(iper.gt.0)   7740
			enddo !ij=1,nextra   7738
		endif !if(nextra.gt.0)   7737
		if(j_o(ivkeep)%i(1).lt.nvartot)then
			!write(6,*)'<555 ',ncols,noutv,nextra,nvartot,j_o(ivkeep)%i(1)
			write(6,*)'There are duplicated variable names'
			j_err=.true.; return
		endif !if(j_o(ivkeep)%i(1).lt.nvartot)   7747
 
		nkeep=j_o(ivkeep)%i(1)
		itemp=1  !stored in tempv
		!	write(6,*)'nkeep',nkeep
		!		write(6,*)'<new nkeep,nob,nvartot,nextra,noutv,nvar ',nkeep,nob,nvartot,nextra,noutv,nvar
		!		write(6,*)'ncols ',ncols
 
		!	ivmat=j_defmatrix(iout,'%matrix',nob,nkeep,j_matreg)
 
		!		write(6,*)'iout,nob,nkeep,ivmat ',iout,nob,nkeep,ivmat
		!	call j_getname(ivmat)
		!	write(6,*)j_oname(1:j_loname),nob*nkeep,j_o(ivmat)%i(1:3)
		if(j_err)return
 
		if(.not.maketran.and.nextra.le.0)then
			if(.not.isdelete)then
 
				if(allocated(j_tempvector))deallocate(j_tempvector)
				allocate(j_tempvector(1:nob*nkeep))
				ibas=0
				do i=1,narg
					nel=nkeep*j_nrows(arg2(i))
					j_tempvector(ibas+1:ibas+nel)=j_o(arg2(i))%d(1:nel)
					ibas=ibas+nel
				enddo !i=1,narg   7772
				!			write(6,*)'temp',j_tempvector
			else
				!fast but piecewise
				if(allocated(j_tempvector))deallocate(j_tempvector)
				nobstot=j_nrows(arg2(1))+j_nrows(arg2(2))
				ntot=nobstot*nkeep
				allocate(j_tempvector(1:ntot))
				nel=j_nrows(arg2(1))*nkeep
				j_tempvector(1:nel)=j_o(arg2(1))%d(1:nel)
				call j_del(arg(1))
				ibas=nel
				nel=j_nrows(arg2(2))*nkeep
				j_tempvector(ibas+1:ibas+nel)=j_o(arg2(2))%d(1:nel)
				call j_del(arg(2))
				call j_getname(arg2(1),arg2(2))
				write(6,*)j_oname2(1:j_loname2),' appended to ',j_oname(1:j_loname), ' obs ',nobstot
				itemp=1
				!	write(6,*)'ntot ',ntot,nobstot
				do i=3,narg
					if(itemp.eq.1)then
						if(allocated(j_tempvector2))deallocate(j_tempvector2)
						nobstot=nobstot+j_nrows(arg2(i))
						nel=j_nrows(arg2(i))*nkeep
						allocate(j_tempvector2(1:ntot+nel))
						j_tempvector2(1:ntot)=j_tempvector(1:ntot)
						j_tempvector2(ntot+1:ntot+nel)=j_o(arg2(i))%d(1:nel)
						call j_del(arg(i))
						deallocate(j_tempvector)
 
						ntot=ntot+nel
						!		write(6,*)'tempsize2 ',ntot
						itemp=2
					else
						if(allocated(j_tempvector))deallocate(j_tempvector)
						nel=j_nrows(arg2(i))*nkeep
						nobstot=nobstot+j_nrows(arg2(i))
						allocate(j_tempvector(1:ntot+nel))
						j_tempvector(1:ntot)=j_tempvector2(1:ntot)
						j_tempvector(ntot+1:ntot+nel)=j_o(arg2(i))%d(1:nel)
						call j_del(arg(i))
						deallocate(j_tempvector2)
 
						ntot=ntot+nel
						!		write(6,*)'tempsize ',ntot
						itemp=1
					endif !if(itemp.eq.1)   7796
					call j_getname(arg2(i))
					write(6,*)j_oname(1:j_loname),' appended, obs ',nobstot
				enddo !i=3,narg   7795
			endif !if(.not.isdelete)   7767
		else
 
			if(allocated(j_tempvector))deallocate(j_tempvector)
			allocate(j_tempvector(1:nob*nkeep))
 
			ibas=0
			!		nrejected=0
			!write(6,*)'hep0'
			j_iobs=0
			do i=1,narg
				!write(6,*)'hep0',i,arg2(i)
				ibas2=0
				!	if(j_iobs.eq.0.and.j_otype(arg(i)).eq.j_ipdata)
				do k=1,j_o(arg2(i))%i(1)
					if(maketran)then
						if(arg(i).eq.arg2(i))then
							j_v(j_o(ivkeep)%i2(1:ncols))=j_o(arg2(i))%d(ibas2+1:ibas2+ncols)
 
						else
							j_iobs=j_iobs+j_18
							call j_getobs(j_iobs)
						endif !if(arg(i).eq.arg2(i))   7842
 
						!			j_v(j_ivrecord)=j
						j_v(ivobs)=j_iobs+j_18
						j_v(j_ivobs)=j_iobs+j_18
						!write(6,*)iobs+1,'in',j_o(arg2(i))%d(ibas2+1:ibas2+ncols)
						!	if(maketran)then
						call dotrans(ivmaketrans,1)
						if(j_err)then
							call j_getname(ivmaketrans)
							write(6,*)'error in doing maketrans->'//j_oname(1:j_loname)
							goto 900
							!	endif !if(j_err)then
						endif !if(j_err)   7856
 
						j_tempvector(ibas+1:ibas+nkeep)=j_v( j_o(ivkeep)%i2(1:nkeep))
						!		j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v( j_o(ivkeep)%i2(1:nkeep))
						!write(6,*)j_iobs+1,'out',j_o(ivmat)%d(ibas+1:ibas+nkeep)
						j_iobs=j_iobs+1
						!endif !if(j_otype(arg2(i)).eq.j_ipmatrix0)then
					else
						! write(6,*)'ivmat ,arg2(i) ',ivmat,arg2(i),ibas,ibas2
						! write(6,*)allocated(j_o(ivmat)%d)
						! if(allocated(j_o(ivmat)%d))write(6,*)'size', size(j_o(ivmat)%d)
						! write(6,*)j_o(arg2(i))%d(ibas2+1:ibas2+nkeep)
						! write(6,*)
						!!			!	write(6,*)'i,ibas,ibas2,arg2(i),j_o(arg2(i))%d(ibas2+1:ibas2+ncols)',i,ibas,ibas2,arg2(i),j_o(arg2(i))%d(ibas2+1:ibas2+ncols)
						!		j_o(ivmat)%d(ibas+1:ibas+ncols)=j_o(arg2(i))%d(ibas2+1:ibas2+ncols)
						j_tempvector(ibas+1:ibas+ncols)=j_o(arg2(i))%d(ibas2+1:ibas2+ncols)
					endif !if(maketran)   7841
 
 
					ibas2=ibas2+ncols
					ibas=ibas+nkeep
				enddo !k=1,j_o(arg2(i))%i(1)   7840
 
			enddo !i=1,narg   7836
 
 
 
		endif !if(.not.maketran.and.nextra.le.0)   7766
		!write(6,*)'hellurei',nob,nkeep
		if(nob.gt.0)then
 
			ivmat=j_defmatrix(iout,'%matrix',nob,nkeep,j_matreg,nod=.true.)
			!	write(6,*)'i',j_o(ivmat)%i
			!	write(6,*)'itemp ',itemp
			if(itemp.eq.1)then
				call move_alloc(from=j_tempvector,to=j_o(ivmat)%d)
				!	write(6,*)'dny',j_o(ivmat)%d
				!	call j_getname(ivmat)
				!	write(6,*)'mat',j_oname(1:j_loname)
			else
				call move_alloc(from=j_tempvector2,to=j_o(ivmat)%d)
			endif !if(itemp.eq.1)   7895
		endif !if(nob.gt.0)   7890
		j_iobs=nob
 
	else	!isappend ########################
 
 
		do i=1,narg
 
			call j_getname(arg(i))
			!	write(6,*)'arg ',arg(i),j_oname(1:j_loname),j_otype(arg(i)),j_ipmatrix
 
			if(j_otype(arg(i)).eq.j_ipmatrix.or.j_otype(arg(i)).eq.j_ipmatrixs)then
				arg2(i)=arg(i)
				nmatrix=nmatrix+1
 
 
 
 
			elseif(j_otype(arg(i)).eq.j_ipdata)then !if(j_otype(arg(i)).eq.j_ipmatrix)then
				arg2(i)=j_o(arg(i))%i(1)
				ncols=j_o( arg2(i))%i(2)
				isdata=.true.
			else
				call j_printname('argument ',arg(i), ' is not data or matrix')
				j_err=.true.
				return
 
			endif !if(j_otype(arg(i)).eq.j_ipmatrix.or.j_otype(arg(i)).eq.j_i   7914
			ncols=j_o(arg2(i))%i(2)
			if(i.eq.1)then
				nob=j_o(arg2(i))%i(1)
				write(6,*)'nob ',nob
			elseif(j_o(arg2(i))%i(1).ne.nob)then
				call j_printname('argument ',arg(i), ' has different number of rows than first argument')
				j_err=.true.;return
 
			endif !if(i.eq.1)   7932
			nvar=nvar+ncols
 
 
			nrowstot=nrowstot+j_o(arg2(i))%i(2)
 
			nvar0(i)=j_o(arg2(i))%i(2)
			if(j_otype(arg(i)).eq.j_ipmatrix.or.j_otype(arg(i)).eq.j_ipmatrixs)nvarmatrix=nvarmatrix+nvar0(i)
			!		 nvar=nvar+nvar0(i)
 
 
			!	call j_getname(arg(i))
			!			write(6,*)'<666 ',i,nvar0(i),nvar,j_oname(1:j_loname)
		enddo !i=1,narg   7909
		if(j_err)return
		!	write(6,*)'nvar,nvarmatrix,ncols',nvar,nvarmatrix,ncols
 
		if(nmatrix.gt.0)then
 
			!	call j_getoption(iob,io,j_mread,nvarmatrix,nvarmatrix,j_ipreal,.true.,nread,j_optarg)
			if(nread.ne.nvarmatrix)then
				write(6,*)'read-> should refer to as many variables as there are columns'
				write(6,*)'read-> has ',nread ,' arguments and matrices have ',nvarmatrix,' columns'
				j_err=.true.;return
			endif !if(nread.ne.nvarmatrix)   7959
 
		endif !if(nmatrix.gt.0)   7956
		j_filterlink=j_codelink(iob,io,j_mfilter); if(j_err)return
		j_filter=j_filterlink.ne.0
		!	write(6,*)'filt ',j_filter
		j_rejectlink=j_codelink(iob,io,j_mreject); if(j_err)return
		j_reject=j_rejectlink.ne.0
		istrans=j_filter.or.j_reject.or.maketran  !newdata
		!write(6,*)'istrans ',istrans
 
		!!call j_clearoption(iob,io)  ! subroutine
 
		nvartot=nvar+noutv+nextra
 
		!	allocate(vars(1:nvar))
		!write(6,*)'<4664,nvartot,nvar,noutv,nextra,ivkeep',nvartot,nvar,noutv,nextra
		ivkeep=j_deflist(iout,'%keep',nres=nvartot)
		nr=0
		nv=0
		!write(6,*)'readv ',readv
 
		do i=1,narg
			!write(6,*)'argdhhd',arg(i),arg2(i),j_otype(arg(i)),j_otype(arg2(i))
			if(arg(i).eq.arg2(i))then
				!write(6,*)nvar0(i),'+',readv(nr+1:nr+nvar0(i))
				iper=j_putlistobject(ivkeep,list0=nvar0(i),list=readv(nr+1:nr+nvar0(i)))
				!	vars(nv+1:nv+nvar0(i))=readv(nr+1:nr+nvar0(i))
				nr=nr+nvar0(i)
			else !if(arg(i).eq.arg2(i))then
				!write(6,*)'arg2',arg2(i) !data
				iper=j_putlistobject(ivkeep,ivin=j_o(arg(i))%i(2))
 
			endif !if(arg(i).eq.arg2(i))   7987
		enddo !i=1,narg   7985
		if(j_o(ivkeep)%i(1).lt.nvar)then
			!	write(6,*)'<555 ,nvar,noutv,nextra,j_o(ivkeep)%i(1)',&
			!	nvar,noutv,nextra,j_o(ivkeep)%i(1)
			write(6,*)'There are duplicated variable names'
			j_err=.true.; return
		endif !if(j_o(ivkeep)%i(1).lt.nvar)   7998
 
		!write(6,*)'nextra',nextra,'?',extra
		if(maketran)iper=j_putlistobject(ivkeep,ivin=ivoul)
		!	write(6,*)'extra ',nextra,extra,nvartot,ivkeep
		if(nextra.gt.0)then
			do ij=1,nextra
				iper=j_inlistobject(extra(ij),ivkeep)
				if(iper.gt.0)then
					call j_getname(extra(ij))
					write(6,*)'*extra-> variable ',j_oname(1:j_loname),' was already in data'
					j_err=.true.;return
				else !if(iper.gt.0)then
					iper=j_putlistobject(ivkeep,single=extra(ij))
				endif !if(iper.gt.0)   8011
			enddo !ij=1,nextra   8009
		endif !if(nextra.gt.0)   8008
 
 
		!nkeep takes duplications into account
		nkeep=j_o(ivkeep)%i(1)
		!	write(6,*)'<new ',nkeep,nob
		if(allocated(j_tempvector))deallocate(j_tempvector)
		allocate(j_tempvector(1:nob*nkeep))
		!	ivmat=j_defmatrix(iout,'%matrix',nob,nkeep,j_matreg)
		!write(6,*)'ivmat',ivmat,allocated(j_o(ivmat)%d),size(j_o(ivmat)%d)
 
 
		ibas0=0
 
 
		nrejected=0
		ibas=0
		j_iobs=0
		if(.not.istrans)j_iobs=nob
		!write(6,*)'j_iobs ',j_iobs,nob,istrans
		nkeep0=nkeep
		!	write(6,*)'isup ',isup
		do j=1,nob
			ibas1=ibas
			j_v(j_ivrecord)=j
			!if(isup)then
			!	call j_getobswup(j_iobs+1,arg(1))
			!	else
			do i=1,narg
				!			write(6,*)'j,i,ibas1,nvar0(i),ibas0(i)',j,i,ibas1,nvar0(i),ibas0(i)
				if(j_otype(arg2(i)).eq.j_ipmatrix)then
					!		j_o(ivmat)%d(ibas1+1:ibas1+nvar0(i))=&
					j_tempvector(ibas1+1:ibas1+nvar0(i))= &
						j_o(arg2(i))%d(ibas0(i)+1:ibas0(i)+nvar0(i))
				else
 
					!		j_o(ivmat)%d(ibas1+1:ibas1+nvar0(i))=&
					j_tempvector(ibas1+1:ibas1+nvar0(i))= &
						j_o(arg2(i))%r(ibas0(i)+1:ibas0(i)+nvar0(i))
 
				endif !if(j_otype(arg2(i)).eq.j_ipmatrix)   8049
 
				ibas0(i)=ibas0(i)+nvar0(i)
				ibas1=ibas1+nvar0(i)
			enddo !i=1,narg   8047
			if(.not.istrans)then
				ibas=ibas+nkeep
				cycle  !nobloop
			endif !if(.not.istrans)   8064
			j_v(j_o(ivkeep)%i2(1:nvar))=j_tempvector(ibas+1:ibas+nvar) !j_o(ivmat)%d(ibas+1:ibas+nvar)
			!write(6,*)'<33>',j_o(ivmat)%d(ibas+1:ibas+nkeep)
 
			!	endif !if(isup)   7558
			j_v(ivobs)=j_iobs+1
			j_v(j_ivobs)=j_v(ivobs)
			j_rejected=.false.
 
 
			if(maketran)then
				call dotrans(ivmaketrans,1)
				if(j_err)then
					call j_getname(ivmaketrans)
					write(6,*)'error in doing maketrans->'//j_oname(1:j_loname)
					goto 900
				endif !if(j_err)   8079
			endif !if(maketran)   8077
			if(j_filter)then
				if(j_codevalue(iob,j_filterlink).eq.j_0)j_rejected=.true.
				if(j_err)return
				!		write(6,*)'code',j_codevalue(iob,j_filterlink),j_v(j_o(ivkeep)%i2(1:nvar))
				! call dotrans(iob,j_iofilter)
				! if (j_err) goto 900
				! if(j_v(j_ivfilter).eq.0.)then
				! j_rejected=.true.
				! end if !if(j_v(j_ivfilter).eq.0.)then
			end if !if(j_filter)   8085
			if(j_reject)then
				if(j_codevalue(iob,j_rejectlink).ne.j_0)j_rejected=.true.
 
			end if !if(j_reject)   8095
			if(j_err)return
			if(j_rejected)then
				nrejected=nrejected+1
			else
				j_iobs=j_iobs+1
				j_tempvector(ibas+1:ibas+nkeep)=j_v(j_o(ivkeep)%i2(1:nkeep))
				!		j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_o(ivkeep)%i2(1:nkeep))
				ibas=ibas+nkeep
			endif !if(j_rejected)   8100
 
		enddo !j=1,nob   8041
		if(j_iobs.gt.0)then
			ivmat=j_defmatrix(iout,'%matrix',j_iobs,nkeep,j_matreg,nod=.true.)
			!	write(6,*)'i',j_o(ivmat)%i
			!	write(6,*)'itemp ',itemp,' d ',j_o(ivmat)%d
			!	if(itemp.eq.1)then
			if(j_iobs.eq.nob)then
				call move_alloc(from=j_tempvector,to=j_o(ivmat)%d)
			else
				j_o(ivmat)%d=j_tempvector(1:ibas)
				deallocate(j_tempvector)
			endif !if(j_iobs.eq.nob)   8115
 
		endif !if(j_iobs.gt.0)   8110
		!	write(6,*)'dny',j_o(ivmat)%d
		!	call j_getname(ivmat)
		!	write(6,*)'mat',j_oname(1:j_loname)
		! else
		! call move_alloc(from=j_tempvector2,to=j_o(ivmat)%d)
		! endif !if(itemp.eq.1)   7606
		!	j_iobs=j_iobs
 
 
		!write(6,*)'pooo'
 
	endif !if(isappend)   7616
	!write(6,*)'fkfk'
	call j_getname(iout)
	write(6,*)'Accepted ',j_iobs, ' observations and ',nkeep,&
		' variables, which can be seen with '//j_oname(1:j_loname)//'%keep;'
 
	j_v(j_ivaccepted)=j_iobs
 
 
 
	if(j_iobs.ne.nob)write(6,*)'rejected ',nrejected
	!		allocate(temp(1:nob*nkeep))
	!		temp=j_o(ivmat)%d
	!ivmat=j_defmatrix(iout,'%matrix',j_iobs,nkeep,j_matreg)
	!	j_o(ivmat)%d=j_tempvector(1:j_iobs*nkeep)
	!	deallocate(j_tempvector)
	if(j_iobs.le.0)then
		write(6,*)'All observations rejected, data not created'
		return
 
	endif !if(j_iobs.le.0)   8150
 
 
	!	call j_defdata(ivout,ivmat,ivkeep,ivcases,ivprolog,ivmaketrans,ivtrans,&
	!		ivepilog,ivvars,iout2_,ivnobsw,0,ivobs,ivobs,ivnobswcum)
	!subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivmaketrans,& ! %%data
	!		ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum)
	!subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivmaketrans,& ! %%data
	!		ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum)
	! else
	! ivmat=j_defmatrix(iout,'%matrix',iobs,nkeep,j_matreg,nod=.true.)
	! call move_alloc(from=j_tempvector,to=j_o(ivmat)%d)
 
	!	endif !if(j_iobs.ne.nob)   8206
	!	write(6,*)'ivmat,ivkeep,',
 
	call j_defdata(iout,ivmat,ivkeep)
900		deallocate(arg2,nvar0)
	j_dlastdata=iout
	j_v(j_ivlastdata)=iout
	iv=j_object('bd%matrix')
 
	if(nread.gt.0)then
		do i=1,nread
			itype=j_otype(readv(i))
			if(itype.ne.j_ipreal)then
				call j_getname(readv(i))
				write(6,*)'object ',j_oname(1:j_loname),' is made REAL, it was ',j_otypes(itype)
				call j_del(readv(i))
			endif !if(itype.ne.j_ipreal)   8179
		enddo !i=1,nread   8177
	endif !if(nread.gt.0)   8176
	!	write(6,*)'ivmat',iv,'dati',j_o(iout)%i
 
	!	j_o(j_ivlastdata)%i(1)=1;j_o(j_ivlastdata)%i2(1)=iout
	if(istime)then
		call cpu_time(cpu1)
		time1=secnds(time0)
		write(6,*)'newdata() used cpu ',cpu1-cpu0,' s and total time ',time1,' s'
	endif !if(istime)   8189
	write(6,*)' '
	return
 
end subroutine newdata !subroutine newdata(iob,io)
 
 
 
subroutine exceldata(iob,io)
	!Section exceldata exceldata() DATA from an excel file
	!Generates data object from csv data generated with excel. It is assumed that ';' is used as column separator,
	! and first is the header line generated with excel and containing column names.
	!The second line contains information for Jlp22 how to read the data.
	!First the first line is copied and pasted as the second line. To the beginning of the second line is put
	! '@#'. Then each entry separated by ';' is edited as follows. If the column is just ignored, then
	! put '!' to the beginning of the entry. If all characters in the column are read in as
	! a numeric variable, change the name to accpetable variable name in J.
	! If the column is read in but it is just used as an input variable for
	!maketrans-> trasformations, then start the name with '$' so the variable is not put to
	! the list of keep-> variables. If a contains only character values then it must
	! be ignored using '!'. If the contains numeric values surrounded by characters, the the numeric value can be picked
	! as follows. Put '?' to the end of entry. Put the variable name to the beginning of the entry.
	! then put the the number of characters to be ignored by two digits, inserting
	! aleading zero if needed. The given the length of the numeric field to be read in as a numeric value.
	! For instance, if the header line in the excel file is
	!Listing
	!Block;Contract;Starting time;Name of municipality;Number of stem;Species code
	!endlisting
	! and the first data line could be
	!Listing
	!MG_H100097362501;20111001;7.5.2021 9:37;Akaa;20;103;1;FI2_Spruce
	!endlisting
	! then the second line before the first data line could be
	!Listing
	!##block0808?;!Contract;!Starting time;!Name of municipality;stem;species0201?
	!endlisting
	! therafter the first observation would get values block=97362501,stem=1, and
	!species=2.
 
	!If there are several input files, the header line of later input lines is ignored, and
	! also if the second line of later files starts with '##', then it is ignored.
	! if any later lines in any input files start with 'jcode:', then the code is computed.
	!This way variables decribing the whole input file can be transmitted to the data.
	! Currently jcode-output variables can be transmitted to data matrix only by using the as pseudo
	!outputvariables in maketrans-transformations, e.g., filevar1=filevar1, if
	!filevar1 is generated in jcode transformation.
	! If there are several input files the file number is put into variable In before computing maketrans transformations
	! and this variable is automatically stored in the data matrix.
	!endheader
	!Option
	!Output&1&Data& Data object generated
	!in&1-&Char&Files to read in.
	!maketrans&N|1&trans& Transformations used to compute new variables to be stored
	! in the data.
	!endoption
	!endsection
	integer,intent(in)::iob
	integer,intent(in)::io
	integer, dimension(:), pointer :: in_
	logical maketran
	integer, dimension(:),allocatable:: nobs,nu,readv,keepv,nhead,line
	logical, dimension(:),allocatable:: isread,ispe
	logical isnobs
	character*2 ,dimension(:),allocatable:: x,f
	character(len=6) ch
 
	call j_startfunction(iob,io,0,narg0,j_optarg0,ivout)
	if(j_err)return
	ivobs=j_getobject(ivout,'%obs',j_ipreal);if(j_err)return
	call j_getoption(iob,io,j_min,1,999,j_ipchar,.false.,nin_,in_)
	call j_getoption(iob,io,j_mmaketrans,-1,1,j_iptrans,.true.,noptarg,j_optarg0)
 
 
	if(j_err)return
	if(noptarg.gt.0)then
		ivmaketrans=j_optarg0(1)
		maketran=.true.
		ivoul=j_trans_output(ivmaketrans)
		!		noutv=j_o(ivoul)%i(0)
	else !if(noptarg.gt.0)then
		maketran=.false.
		!		noutv=0
	end if !if(noptarg.gt.0)   8267
	call j_getoption(iob,io,j_mnobs,-1,999,j_ipreal,.true.,noptarg,j_optarg0)
	isnobs=noptarg.gt.0
	if(isnobs)then
		if(noptarg.ne.nin_)then
			write(6,*)'nobs-> needs as many arguments as in->'
			j_err=.true.;return
		endif !if(noptarg.ne.nin_)   8279
 
	endif !if(isnobs)   8278
 
	if(allocated(nobs))then
		if(size(nobs).lt.nin_)deallocate(nobs,nu)
	endif !if(allocated(nobs))   8286
	if(.not.allocated(nobs))allocate(nobs(1:nin_),nu(1:nin_),nhead(1:nin_))
	nobs=0
	! if(nobsarg.gt.0)then
	! nobs=j_v(j_optarg0(1:nobsarg))
	! else
	nobs=0
	if(nin_.gt.1)then
		ivin=j_getobject(0,'In',j_ipreal) !
	else
		ivin=0
 
	endif !if(nin_.gt.1)   8295
	ivstand=j_getobject(0,'Section',j_ipreal)
	!	endif
	nobstot=0
	do ifi=1,nin_
		call j_getfile(nu(ifi),'r',ivfile=in_(ifi))
		if(j_err)return
 
		!character*80 j_cline
		!		 if(nobs(ifi).eq.0)then
		if(ifi.eq.1)then
			read(nu(ifi),'(a)',end=98,err=99)j_cline !first line
			linfi=j_lentrim(j_cline)
			write(6,*)'file ',ifi, ' header:'
			write(6,*)j_cline(1:linfi)
		else !if(ifi.eq.1)then
			read(nu(ifi),'(a)',end=98,err=99)j_inpr
			if(j_inpr(1:linfi).ne.j_cline(1:linfi))then
				write(6,*)'file ',ifi,' header does not agree with the header of first file'
				write(6,*)j_cline(1:linfi)
				j_err=.true.
				return
			endif !if(j_inpr(1:linfi).ne.j_cline(1:linfi))   8317
		endif !if(ifi.eq.1)   8310
		nhead(ifi)=1
		nlin=1
100		read(nu(ifi),'(a)',end=98,err=99)j_inpr !second line
		nlin=nlin+1
		lin=j_lentrim(j_inpr)
		!	write(6,*)'lin ',lin,j_inpr(1:lin)
		nhead(ifi)=nhead(ifi)+1
		if(j_inpr(1:5).eq.'code:')then
			write(6,*)j_inpr(1:lin)
			call j_command(j_inpr(1:lin))
 
			if(j_err)return
			goto 100
		endif !if(j_inpr(1:5).eq.'code:')   8331
		if(nlin.eq.2.and.ifi.eq.1)then
			if(j_inpr(1:2).ne.'##')then
				write(6,*)'the first in-> file second line must start with ##'
				j_err=.true.
				return
			endif !if(j_inpr(1:2).ne.'##')   8339
		endif !if(nlin.eq.2.and.ifi.eq.1)   8338
 
		if(j_inpr(1:2).eq.'##'.and.ifi.eq.1)then
			!interpret
			!	character*132 j_tempchar2, j_tempchar3
			!lin=j_lentrim(j_inpr)
			!			read(nu(1),'(a)',err=99)j_inpr
 
			!			linwrite(
			!	write(6,*)'linalku',lin
			call j_clean0(j_inpr(1:lin),lin)
			!	write(6,*)'linsit',lin
			if(j_err)return
			npp=0  !number of words
 
			nread=0 !nuber variables to read
 
			nkeep=0
			allocate(readv(1:1000),isread(1:1000),x(1:1000),f(1:1000),keepv(0:1000))
			allocate(ispe(1:1000))
			isread=.false.
			x=' '
			f=' '
			ic0=2
			npp=0
 
			!	write(6,*)'linhere ',lin,j_inpr(1:100)
			!		write(6,*)'londuduu ',lin,j_inpr(lin-100:lin)
			do ic=3,lin+1
				!	ic=ic1
				!	if(ic1.eq.lin)ic=lin+1
				!		write(6,*)'ic ',ic,'/',j_inpr(ic-1:ic),'/'
 
				!if(ic.gt.lin-30)write(6,*)'ic',ic,j_inpr(ic:ic),j_inpr(ic:ic).eq.';'.or.ic.eq.lin
				if(j_inpr(ic:ic).eq.';'.or.ic.eq.lin+1)then
					npp=npp+1
					!		write(6,*)'npp ',npp,j_inpr(ic0+1:ic0+1)
					if(j_inpr(ic0+1:ic0+1).ne.'!')then
						nread=nread+1
						if(j_inpr(ic-1:ic-1).eq.'?')then
							x(npp)=j_inpr(ic-5:ic-4)
							f(npp)=j_inpr(ic-3:ic-2)
							ispe(npp)=.true.
 
							iread=j_getobject(0,j_inpr(ic0+1:ic-6),j_ipreal);if(j_err)return ! ivsubrecord)
							!		write(6,*)'npp ',npp,' x ',x(npp),' f ',f(npp),'iread ',iread
						else !if(j_inpr(ic-1:ic-1).eq.'?')then
							ispe(npp)=.false.
							iread=j_getobject(0,j_inpr(ic0+1:ic-1),j_ipreal);if(j_err)return ! ivsubrecord)
						endif !if(j_inpr(ic-1:ic-1).eq.'?')   8383
 
						!	write(6,*)j_inpr(ic0+1:ic-1),iread
						if(j_err)return
						!write(6,*)'iread ',iread
						readv(nread)=iread
						isread(npp)=.true.
						if(j_inpr(ic0+1:ic0+1).ne.'$')nkeep=nkeep+1
						keepv(nkeep)=iread
					endif !if(j_inpr(ic0+1:ic0+1).ne.'!')   8381
					ic0=ic
					!	if(ic.gt.lin-30)write(6,*)ic,ic0
				endif !if(j_inpr(ic:ic).eq.';'.or.ic.eq.lin+1)   8378
			enddo !ic=3,lin+1   8372
			nkeep=nkeep+1
			keepv(nkeep)=j_ivrecord
			nkeep=nkeep+1
			keepv(nkeep)=ivstand
			if(nin_.gt.1)then
				nkeep=nkeep+1
				keepv(nkeep)=ivin
			endif !if(nin_.gt.1)   8411
			!	call j_getobject(0,'File',j_ipreal,ifile)
			! if(nin_.gt.1)then
			! nkeep=nkeep+1
			! keepv(nkeep)=j_ivin
			! endif !if(nin_.gt.1)then
			keepv(0)=nkeep
			if(maketran)then
				!function j_putlistobject(ivlist,single,list0,list,ivin)
				!deflistobject(iv,name,ivout,list0,list,nres)
				ivkeep=j_deflist(ivout,'%keep',list0=nkeep,list=keepv(1:nkeep),nres=j_o(ivoul)%i(1))
				iperk= j_putlistobject(ivkeep,ivin=ivoul)
				!call j_defmergelist(ivout,'%keep',keepv(0:nkeep),j_o(ivoul)%i,0,ivkeep)
			else !if(maketran)then
				ivkeep=j_deflist(ivout,'%keep',listold=keepv(0:nkeep+1))
				!call j_deflist2(ivout,'%keep',keepv(0:nkeep),ivkeep)
			endif !if(maketran)   8421
			nkeep=j_o(ivkeep)%i(1)
			!nhead=nhead+1
			write(6,*)'reading ',nread,' variables, keeping ',nkeep
			!	write(6,*)readv(1:nread)
		endif !if(j_inpr(1:2).eq.'##'.and.ifi.eq.1)   8346
		!	nhead=nhead+1
		if(j_inpr(1:2).eq.'##'.or.j_inpr(1:5).eq.'code:')goto 100
		nhead(ifi)=nhead(ifi)-1
 
		!		write(6,*)'nhead',nhead
 
		if(isnobs)then
			nobs(ifi)=j_v(j_optarg0(ifi))
 
		else
			nobs(ifi)=1
 
			nst=1
200		read(nu(ifi),'(a)',end=201,err=202)j_inpr(1:linfi)
			if(j_inpr(1:linfi).eq.j_cline(1:linfi))nst=nst+1
			if(j_inpr(1:linfi).eq.j_cline(1:linfi))goto 200
			nobs(ifi)=nobs(ifi)+1
			goto 200
 
			!	if(ch(1:5).eq.'code:'.or.ch(1:2).eq.'##')then
			!		nhead=nhead+1
			!	endif !if(ch(1:5).eq.'code:'.or.ch(1:2).eq.'##')   8388
 
202	write(6,*)'error reading file number ',ifi,'line ',nobs0+1
			j_err=.true.;return
201 	rewind(nu(ifi))
			write(6,*)'number of sections ',nst,' variable Section tells the section'
			do ibs=1,nhead(ifi)
				read(nu(ifi),*)ch
			enddo !ibs=1,nhead(ifi)   8463
 
			write(6,*)'in-> file ',ifi, ' has ',nobs(ifi),' observations after header of ',nhead(ifi),' lines'
 
		endif !if(isnobs)   8442
		nobstot=nobstot+nobs(ifi)
 
	enddo !ifi=1,nin_   8304
	! each file at second line
 
 
	iobs=0
	ivmat=j_defmatrix(ivout,'%matrix',int8(nobstot),int8(j_o(ivkeep)%i(1)),j_matreg)
 
	do ifi=1,nin_
 
		!	nzero=0
		!	j_v(j_ivin)=ifi
		iobsw=0
		!		write(6,*)'nhead' ,nhead
		nlin=nhead(ifi)
		nstand=1
		j_v(ivstand)=1
		if(nin_.gt.1)j_v(ivin)=ifi
		write(6,*)'npp ',npp,' nread ',nread
		!	nlinv=0
		do i=1,100000
			if(iobsw.eq.nobs(ifi))exit
			read(nu(ifi),'(a)',end=98,err=99)j_inpr;lin=j_lentrim(j_inpr)
			!	if(i.le.3)write(6,*)i,'tas',iobs,nlin+1,j_inpr(1:lin)
			nlin=nlin+1
 
			if(j_inpr(1:linfi).eq.j_cline(1:linfi))then
				!	write(6,*)'here',iobs,nlin,j_inpr(1:linfi)
				nstand=nstand+1
				j_v(ivstand)=nstand
				!			nlinv=nlin
				cycle
			endif !if(j_inpr(1:linfi).eq.j_cline(1:linfi))   8497
			iobsw=iobsw+1
			npp0=0
			nrea=0
			ic0=0
			!	if(ifi.eq.2.and.i.eq.1089)write(6,*)'isrea',isread
			do ic=1,lin+1
 
				!if(ic1.eq.lin)ic=lin+1
				if(j_inpr(ic:ic).eq.';'.or.ic.eq.lin+1)then
 
					npp0=npp0+1
					!		write(6,*)'ic,ic0,',ic,ic0,j_inpr(ic:ic),npp0,isread(npp0)
					if(npp0.gt.npp)exit
					!		if(ifi.eq.2.and.i.eq.1089)write(6,*)'npp,ic0,ic,isread',npp,ic0,ic,isread(npp)
					if(isread(npp0))then
						nrea=nrea+1
						if(ic.gt.ic0+1)then
							if(ispe(npp0))then
								if(x(npp0).ne.'00')then
									read(j_inpr(ic0+1:ic-1),'('//x(npp0)//'x,f'//f(npp0)//'.0)',err=998)j_v(readv(nrea))
								else !if(x(npp0).ne.'00')then
									read(j_inpr(ic0+1:ic-1),'(f'//f(npp0)//'.0)',err=998)j_v(readv(nrea))
								endif !if(x(npp0).ne.'00')   8522
							else !if(ispe(npp0))then
								read(j_inpr(ic0+1:ic-1),*,err=991)j_v(readv(nrea))
								!		write(6,*)'npp0,ic0,ic,nrea',npp0,ic0,ic,nrea,j_v(readv(nrea))
							endif !if(ispe(npp0))   8521
 
						else !if(ic.gt.ic0+1)then
							j_v(readv(nrea))=j_0
						endif !if(ic.gt.ic0+1)   8520
						! if(i.le.3)then
						! call j_getname(readv(nrea))
						! write(6,*)nrea,j_oname(1:j_loname),ic0+1,ic-1,'/',j_inpr(ic0+1:ic-1),'/',j_v(readv(nrea))
						! endif !if(i.le.3)   8614
					endif !if(isread(npp0))   8518
 
					ic0=ic
				endif !if(j_inpr(ic:ic).eq.';'.or.ic.eq.lin+1)   8512
 
			enddo !ic=1,lin+1   8509
			if(nrea.lt.nread)j_v(readv(nrea+1:nread))=j_0
			j_v(j_ivrecord)=nlin
			if(maketran)then
				j_v(ivobs)=iobs
				call dotrans(ivmaketrans,1)
				if(j_err)return
			endif !if(maketran)   8547
			iobs=iobs+1
			j_o(ivmat)%d((iobs-1)*nkeep+1:iobs*nkeep)=j_v(j_o(ivkeep)%i2(1:nkeep))
			!		write(6,*)j_o(ivmat)%d((iobs-1)*nkeep+1:iobs*nkeep)
			! if(j_o(ivmat)%d((iobs-1)*nkeep+5).le.j_0)then
			! write(6,*)
			! lkm=lin/80+1
			! do jj=1,lkm
			! write(6,*)'zero',nlin,iobs,j_inpr((jj-1)*80+1:jj*80)
			! enddo !jj=1,lkm   8593
			! write(6,*)'i,iobs,lin ',i,iobs,lin,j_o(ivmat)%d((iobs-1)*nkeep+1:(iobs-1)*nkeep+6)
			! !		do jj=1,nread
			! !			call j_getname(readv(jj))
			! !			write(6,*)j_oname(1:j_loname),'=',j_v(readv(jj))
			! !		enddo !jj=1,nread   8563
			! endif !if(j_o(ivmat)%d((iobs-1)*nkeep+5).le.j_0)   8590
			!	if(i.eq.1)stop
		enddo !i=1,100000   8491
		!	write(6,*)'iobs,nobs(ifi)',iobs,nobs(ifi)
		call j_closeunit(nu(ifi))
	enddo !ifi=1,nin_   8479
	! do iobs_=nobstot-60,nobstot
	! write(6,*)iobs_,j_o(ivmat)%d((iobs-1)*nkeep+1:(iobs-1)*nkeep+15)
 
	! enddo !iobs_=nobstot-60,nobstot   8606
	write(6,*)
900 	continue
	!call j_deflist2(ivout,'%vars',j_o(ivkeep)%i(0:nkeep),ivvars)
		!subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivprolog,ivmaketrans,ivtrans,& ! %%data
	!	ivepilog,ivvars,ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum) !%%data
	!subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivmaketrans,& ! %%data
	!	ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum)
	call j_defdata(ivout,ivmat,ivkeep)
	call j_printname('data ',ivout, ' created')
	write(6,*)'Accepted ',nobstot,' out of ',dnobs
	j_v(j_ivaccepted)=nobstot
	j_dlastdata=ivout
	j_v(j_ivlastdata)=ivout
	!j_o(j_ivlastdata)%i(1)=1;j_o(j_ivlastdata)%i2(1)=ivout
800	deallocate(nobs,nu)
	if(allocated(readv))deallocate(readv,keepv,x,f,isread,ispe)
	return
	!character*4096 j_inpr
98   write(6,*)'premature end in reading file ',ifi	,' record ',i,' obs',obs
99	write(6,*)'error reading file ',ifi,' record ',i,' obs ',iobs
	j_err=.true.
	deallocate(nobs,nu)
	return
 
998  write(6,*)' '
	write(6,*)'file ',ifi,' record ',nlin,' lin',lin
	write(6,*)j_inpr(1:lin)
	write(6,*)'error reading number from ',ic0+1, 'to ',ic-1, 'i.e. from ',&
		j_inpr(ic0+1:ic-1),' with format'
	if(x(npp0).ne.'00')then
		write(6,*)'('//x(npp0)//'x,f'//f(npp0)//'.0)'
		write(6,*)'make sure that Enconding is set to ANSI in the editor'
	else !if(x(npp0).ne.'00')then
		write(6,*)'(f'//f(npp0)//'.0)'
	endif !if(x(npp0).ne.'00')   8605
	j_err=.true.
	goto 800
991  write(6,*)'file ',ifi,' record ',i
	write(6,*)j_inpr(1:lin)
	write(6,*)'error reading variable from ',ic0+1, 'to ',ic-1, 'i.e. from ',&
		j_inpr(ic0+1:ic-1)
 
	j_err=.true.
	deallocate(nobs,nu)
end subroutine exceldata !subroutine exceldata(iob,io)
 
subroutine classvector(iob,io)
	!Section classvector classvector() Vectors from grouped DATA
	!Function classvector computes vectors from data which extract information from grouped
	!data. These vectors can be used to generate new data object using newdata() function or
	!new matrices from submatrices using matrix() function with matrix-> option or
	! they can be used in transformation objects to compute class related things.
	!There is no explicit output for the function, but several output vectors can
	! be generated depending on the arguments and first->, last-> and
	! expand-> options. The function creates automatically REAL variable
	! Class%nobs whose value will be the number of classwes. The function prints the names of the output vectors generated.
	!endheader
	!Option
	!Args&0-&REAL& The variables whose class information is computed. Arguments
	! are not necessary if first-> and/or last-> are present.
	!Let Arg be the generic name for arguments.
	!class&1&REAL&The variable indicating the class. The class variable which must be present in the data object or which is
	!an output variable of the trans-> transformations.
	!When the class-> variable, denoted as
	!as Class changes,
	!the class changes.
	!data&0|1&Data&Data object used. Only one data object can be used; extra data-> objects just ignored. The default is the last
	! data object generated.
	!expand&-1|0& &If expand-> is present then the lengths output vectors are equal
	!to the number of observations in the data object and the values of the class variables
	! are repeated as many times as there are observations in each class. If
	!expand-> is not present, the lengths of the output vectors are.
	! equal to the number of classes.
	!first&0& &The the number of first observation in class is stored in vector
	! Class%%first if expand-> is present and Class%first if expand-> is not present.
	!last&0& &The the number of last observation in class is stored in vector
	! Class%%last if expand-> is present and Class%lastobject if expand-> is not present.
	!nobsw&0& &The number of observations in class is stored in vector nobsw%Class if
	! expand is not present and in nobsw%%Class if expand-> is present
	!obsw&0& &If there expand-> option then vector Class%%obsw
	!ext&-1|1&Char&The extension to the names of vectors generated for arguments. Let
	! Ext be denote the extension.
	!mean&-1|0& &The class means are stored in the vectors \newline
	! Arg#Class%%mean with expand-> and without ext->\newline
	! Arg#Class%%meanExt with expand-> and with ext-> are \newline
	! Arg#Class%mean without expand-> and without ext-> \newline
	! Arg#Class%meanExt without expand-> and with ext->
	!sd&-1|0& & Class standard deviations are computed to sd vectors
	!var&-1|0& & Class variances are computed to var vectors
	!min&-1|0& & Class minimums are computed to min vectors
	!max&-1|0& & Class maximums are computed to max vectors.
	!endoption
	!Note Numbers of observations in each class can also be obtained by \\
	!Class%nobs=Class%%last-Class%%first+1 when expand-> is present, and \\
	!Class%nobs=Class%%last-Class%%first+1
	!endnote
	!Ex classdata Hierarchical data
	! nstand=10
	! xm=matrix(nstand)
	! xm=rann(3)
	! ym=0.7*xm+0.1*xm
	! xm;
	! ym;
	! standdata=newdata(xm,ym,read->(X,Y))
	! stat()
	! ntree=6
	! xt=matrix(ntree*nstand)
	! yt=matrix(ntree*nstand)
	! standv=matrix(ntree*nstand)
	! ex=matrix(ntree*nstand)
	! ey=matrix(ntree*nstand)
	! transa=trans()
	! jj=0
	! do(i,1,nstand)
	! do(j,1,ntree)
 
	! jj=jj+1
	! standv(jj)=i
	! ex(jj)=rann()
	! ey(jj)=0.3*ex(jj)+0.3*rann()
	! xt(jj)=xm(i)+ex(jj)
	! yt(jj)=ym(i)+0.3*ex(jj)+0.3*rann()
	! enddo
	! enddo
	! /
	! call(transa)
	! treedata=newdata(standv,xt,yt,read->(stand,x,y))
	! stat()
 
	!!! Making class level data object from treedata
	! classvector(x,y,class->stand,data->treedata,mean->,min->)
	! standdata2=newdata(x[stand]%mean,y[stand]%mean,x[stand]%min,y[stand]%min,
	! read->(x,y,xmin,ymin))
	! stat()
	!classvector(x,y,class->stand,data->treedata,mean->,expand->)
	!ex2=treedata(x)-x
 
 
 
 
	!endex
 
	!/endsection
 
	!call j_defmatrix(ivclass,'%first',int8(nclass),j_18,j_matreg,.false.,ive) nexpand
	!call j_defmatrix(0,j_varname1(1:le)//'%%first',nobs,1,j_matreg,.false.,ive) expand
	!nexpand
	!		if(next.le.0)&
	!call j_defmatrix(arg(ia),'#'//j_varname1(1:le)//'%mean',int8(nclass),j_18,j_matreg,.false.,ive)
	!if(next.gt.0)&
	!call j_defmatrix(arg(ia),'#'//j_varname1(1:le)//'%mean'//j_cline(1:lext),int8(nclass),j_18,j_matreg,.fals
	integer,intent(in)::iob
	integer,intent(in)::io
	integer, dimension(:), pointer :: arg !arguments of the function
	integer, dimension(:), pointer :: ext !arguments of the function
	integer, dimension(:), pointer :: clvar !arguments of row
	integer, dimension(:), pointer :: dat !arguments of column
	integer,dimension(:),allocatable :: makekeep
	double precision sum,sum2,nd,classv
	integer,dimension(:), allocatable:: last0 !,sum,mean,var,sd,mini,maxi
	double precision,dimension(:), allocatable:: sum0,mean0,var0,sd0,mini0,maxi0
	double precision cmean,cvar,cmin,cmax,csd,w
	!	integer*8::iobs
	integer ::nclass
	!	logical maketran
	! call j_checkoutput(iob,io)
	! if(j_err)return
	! j_optarg0
	!	write(6,'(30i5/)')j_o(iob)%i(1: j_o(iob)%i(0) )
	call j_startfunction(iob,io,j_ipreal,narg,arg,ivout);if(j_err)return
	!write(6,*)'<arg ',narg,arg
	! if(j_otype(arg(1)).ne.j_ipreal)then
	!write(6,*)'*classvar needs real argument'
	! j_err=.true.
	call j_getdataobject(iob,io)
 
	! endif
 
	!call j_getdatasets(iob)
	if(j_err)return
	!call j_getdataset(j_datasets(1),nobs)
 
	!	ivkeep=j_o(j_datasets(1))%i(2)
	!j_getoption(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
	call j_getoption(iob,io,j_mclass,1,1,j_ipreal,.true.,nc,clvar)
	if(j_err)return
	ivclass=clvar(1)
	call j_getline(j_ivnames,ivclass,j_varname1,le)
	if(j_err)return
	! j_getoption(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
	!	if(ivout.eq.j_ivresult)ivout=ivclass
	call j_getoption(iob,io,j_mext,-1,1,j_ipchar,.true.,next,j_optarg0)
	if(j_err)return
	if(next.gt.0)call j_getchar(j_optarg0(1),j_cline,lext);if(j_err)return
 
 
	call j_getoption(iob,io,j_mfirst,-1,0,j_ipreal,.false.,ifirst,j_optarg0)
	!	write(6,*)ifirst,'ifirst'
	if(j_err)return
	call j_getoption(iob,io,j_mobsw,-1,0,j_ipreal,.false.,iobsw,j_optarg0)
	if(j_err)return
	call j_getoption(iob,io,j_mlast,-1,0,j_ipreal,.false.,ilast,j_optarg0)
	if(j_err)return
	call j_getoption(iob,io,j_mw,-1,1,j_ipreal,.false.,iw,j_optarg0)
	if(j_err)return
	!	call j_getoption(iob,io,j_mdata,1,1,j_ipdata,.true.,nd,dat)
	! ivar<0 do not make
	!
	! ivar=1 put to data index invar
	! ivar=2 make vector
	! ivar=3 make both index invar
	if(j_err)return
	!	no=0
	call j_getoption(iob,io,j_mvar,-1,1,j_ipreal,.false.,ivar0,j_optarg0)
	if(j_err)return
	if(ivar0.ge.0)allocate(var0(1:j_dnobs))
 
	!	if(var)no=no+1
	call j_getoption(iob,io,j_msd,-1,1,j_ipreal,.false.,isd0,j_optarg0)
	if(j_err)return
	if(isd0.ge.0)allocate(sd0(1:j_dnobs))
 
	call j_getoption(iob,io,j_msum,-1,1,j_ipreal,.false.,isum0,j_optarg0)
	if(j_err)return
	if(isum0.ge.0)allocate(sum0(1:j_dnobs))
 
 
	call j_getoption(iob,io,j_mmean,-1,1,j_ipreal,.false.,imean0,j_optarg0)
	if(j_err)return
	if(imean0.ge.0.or.iw.ge.0)allocate(mean0(1:j_dnobs))
 
 
	call j_getoption(iob,io,j_mmin,-1,1,j_ipreal,.false.,imin0,j_optarg0)
	if(j_err)return
	if(imin0.ge.0)allocate(mini0(1:j_dnobs))
 
	call j_getoption(iob,io,j_mnobsw,-1,1,j_ipreal,.false.,inobsw0,j_optarg0)
	if(j_err)return
 
 
 
	call j_getoption(iob,io,j_mmax,-1,1,j_ipreal,.false.,imax0,j_optarg0)
	if(j_err)return
	if(imax0.ge.0)allocate(maxi0(1:j_dnobs))
	call j_getoption(iob,io,j_mexpand,-1,0,j_ipreal,.false.,nexpand,j_optarg0)
	if(j_err)return
 
 
 
 
	!!call j_clearoption(iob,io)  ! subroutine
	ifi=1
	i1=1
 
 
	n=0
	!	nkier=1
	!write(6,*)ivar0+isd0+isum0+imax0+imin0+imean0,ivar0,isd0,isum0,imax0,imin0,imean0
	if(narg.le.0.and.ilast.lt.0.and.ifirst.lt.0)then
		write(6,*)'*arguments missing'
		j_err=.true.
		return
	endif !if(narg.le.0.and.ilast.lt.0.and.ifirst.lt.0)   8834
	if(iw.ge.0.and.nexpand.lt.0)then
		write(6,*)'option ->w requires expand->'
		j_err=.true.;return
 
	endif !if(iw.ge.0.and.nexpand.lt.0)   8839
 
 
	allocate(last0(1:j_dnobs8))
 
	nlo=max(narg,1)
		vloop: do ia=1,nlo
		!call j_getdataset(j_datasets(1),nobs)
		if(narg.gt.0)then
 
 
			! endif
			sum=j_0
			sum2=j_0
			n=0
 
			cmin=1.d60
			cmax=-1.d60
			if(iw.ge.0)then
 
				if(next.le.0)&
					ivew=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%w',j_dnobs8,j_18,j_matreg)
				if(next.gt.0)&
					ivew=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%w'//j_cline(1:lext),j_dnobs8,j_18,j_matreg)
				call j_printname('   done ',ivew,' ')
			endif !if(iw.ge.0)   8861
		endif !if(narg.gt.0)   8851
		classv=-1.79d37
		ifi=1
		!write(6,*)'ivar,isd,isum,imin,imax,nobs',ivar,isd,isum,imin,imax
		nclass=1
 
		!	n
			ob:	do j_iobs=j_dfrom,j_duntil+1

!	write(6,*)'i',i

			if(j_iobs.le.j_dnobs)then
				!	j_v(j_ivobs)=i
				call j_getobs(j_iobs);
				!			call j_getob(j_datasets(1),i);if(j_err)return
				!call j_nextobs()
 
				if(j_err)return
 
				!			if(j_rejected)cycle
			endif !if(j_iobs.le.j_dnobs)   8880
 
			!	write(6,*)'j_v(clvar(1))',j_v(clvar(1)),ifi
			if((j_v(ivclass).ne.classv.and.j_iobs.gt.1).or.j_iobs.eq.j_dnobs+1)then
				!j_o(iv)%i(1)=ivmat
				!	write(6,*)'<44>'
				last0(nclass)=j_iobs-1
 
				if(narg.gt.0)then
					!		write(6,*)'<45>',ivmat,nkeep,indvar,'j_dnobs',nobs
					nd=n
					!	sum
					cmean=sum/nd
					cvar=j_0
					if(nd.gt.1)cvar=(sum2-sum*sum/nd)/(nd-j_n1)
					csd=j_0
					if(cvar.gt.j_0)csd=sqrt(cvar)
					if(ivar0.ge.0)var0(nclass)=cvar
					if(isd0.ge.0)sd0(nclass)=csd
					if(isum0.ge.0) sum0(nclass)=sum
					if(imean0.ge.0.or.iw.ge.0) mean0(nclass)=cmean
					!write(6,*)'clasmmean var ',ia,'class ',nclass,' mean ',cmean
					if(imin0.ge.0) mini0(nclass)=cmin
					if(imax0.ge.0) maxi0(nclass)=cmax
					!				write(6,*)nclass,sum,sum2,cmean,cvar,csd
				endif !if(narg.gt.0)   8897
				if(j_iobs.eq.j_dnobs+1)exit ob
				nclass=nclass+1
				!		write(6,*)'j_iobs,nclass,j_v(ivclass).ne.classv,j_v(ivclass),classv',&
				!j_iobs,nclass,j_v(ivclass).ne.classv,j_v(ivclass),classv
				!		write(6,*)'<478>',nclass
				sum=j_0
				sum2=j_0
				n=0
				ifi=j_iobs
				cmin=1.d60
				cmax=-1.d60
			endif !if((j_v(ivclass).ne.classv.and.j_iobs.gt.1).or.j_iobs.eq.j   8892
			classv=j_v(ivclass)
			if(j_rejected)then
				nrej=nrej+1
				cycle ob
 
			endif !if(j_rejected)   8928
			n=n+1
			if(narg.gt.0)then
				if(j_v(arg(ia)).ge.1.7d18)cycle
				sum=sum+j_v(arg(ia))
				sum2=sum2+j_v(arg(ia))*j_v(arg(ia))
				cmin=min(cmin,j_v(arg(ia)))
				cmax=max(cmax,j_v(arg(ia)))
				if(iw.ge.0)j_o(ivew)%d(j_iobs)=j_v(arg(ia))
				!if(iw.ge.0)!write(6,*) 'put in w matrix ',i,j_v(arg(ia))
				!	write(6,*)'csum',csum,sum2,cmin
			endif !if(narg.gt.0)   8934
		enddo ob !	do j_iobs=j_dfrom,j_duntil+1   8876
		!	write(6,*)nclass,'last ',last0(1:20)
		if(ifirst.ge.0.and.ia.eq.1)then
			if(nexpand.lt.0)then
				ive=j_defmatrix(ivclass,'%first',int8(nclass),j_18,j_matreg)
				!	write(6,*)'nclass',nclass,size(j
				!	ibas=0
 
				!	call j_putmatrix(ive,1,1,1.d0 )
 
 
				ifi=1
				do i=1,nclass
					j_o(ive)%d(i)=ifi
					ifi=last0(i)+1
					!	call j_putmatrix(ive,i,1,last0(i-1)+1.d0)
				enddo !i=1,nclass   8956
				call j_printname('   done ',ive,' ')
			else !if(nexpand.lt.0)then
				ifi=1
				!call j_defmatrix(arg(ia),'$'//j_varname1(1:le)//'%mean',int8(j_dnobs),j_18,j_matreg,.false.,ive)
				!			call j_defmatrix(ivclass,'%first',j_dnobs,1,j_matreg,.false.,ive)
				ive=j_defmatrix(0,j_varname1(1:le)//'%%first',int8(j_dnobs),j_18,j_matreg)
				!	write(6,*)'nclass',nclass,size(j
				!call j_putmatrix(ive,1,1,1.d0 )
				ij=0
 
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=ifi
						!		call j_putmatrix(ive,i,1,last0(i-1)+1.d0)
					enddo !j=ifi,last0(i)   8972
					ifi=last0(i)+1
				enddo !i=1,nclass   8971
				call j_printname('   done ',ive,' ')
			endif !if(nexpand.lt.0)   8947
		endif !if(ifirst.ge.0.and.ia.eq.1)   8946
 
		ive=j_getobject(0,j_varname1(1:le)//'%nobs',j_ipreal)
		j_v(ive)=nclass
		!write(6,*)j_varname1(1:le)//'%nobs=',nclass
 
		if(inobsw0.ge.0.and.ia.eq.1)then
			if(nexpand.lt.0)then
				ive=j_defmatrix(ivclass,'%nobsw',int8(nclass),j_18,j_matreg)
				!		do i=1,nclass
				ifi=0
				do i=1,nclass
					j_o(ive)%d(i)=last0(i)-ifi
					ifi=last0(i)
					!	call j_putmatrix(ive,i,1,last0(i-1)+1.d0)
				enddo !i=1,nclass   8992
 
				!		call j_putmatrix(ive,i,1,dble(last0(i)) )
				!	enddo !i=1,nclass   8430
				!	write(6,*)'nclass',nclass,last0(1:10)
				call j_printname('   done ',ive,' ')
			else !if(nexpand.lt.0)then
				ifi=1
				ive=j_defmatrix(0,j_varname1(1:le)//'%%last',int8(j_dnobs),j_18,j_matreg)
				!	call j_defmatrix(ivclass,'%last',nobs,1,j_matreg,ive)
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=last0(i)-ifi+1
						!		call j_putmatrix(ive,i,1,dble(last0(i)) )
					enddo !j=ifi,last0(i)   9008
					ifi=last0(i)+1
				enddo !i=1,nclass   9007
				!	write(6,*)'nclass',nclass,last0(1:10)
				call j_printname('   done ',ive,' ')
			endif !if(nexpand.lt.0)   8988
		endif !if(inobsw0.ge.0.and.ia.eq.1)   8987
 
		if(ilast.ge.0.and.ia.eq.1)then
			if(nexpand.lt.0)then
				ive=j_defmatrix(ivclass,'%last',int8(nclass),j_18,j_matreg)
				!		do i=1,nclass
				j_o(ive)%d(1:nclass)=last0(1:nclass)
				!		call j_putmatrix(ive,i,1,dble(last0(i)) )
				!	enddo !i=1,nclass   8430
				call j_printname('   done ',ive,' ')
			else !if(nexpand.lt.0)then
				ifi=1
				ive=j_defmatrix(0,j_varname1(1:le)//'%%last',int8(j_dnobs),j_18,j_matreg)
				!	call j_defmatrix(ivclass,'%last',nobs,1,j_matreg,ive)
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=last0(i)
						!		call j_putmatrix(ive,i,1,dble(last0(i)) )
					enddo !j=ifi,last0(i)   9034
					ifi=last0(i)+1
				enddo !i=1,nclass   9033
 
				call j_printname('   done ',ive,' ')
			endif !if(nexpand.lt.0)   9021
		endif !if(ilast.ge.0.and.ia.eq.1)   9020
 
		if(iobsw.ge.0.and.ia.eq.1)then
			if(nexpand.lt.0)then
				write(6,*)'obsw-> requires expand->'
				j_err=.true.;return
			else !if(nexpand.lt.0)then
				ifi=1
				ive=j_defmatrix(0,j_varname1(1:le)//'%%obsw',int8(j_dnobs),j_18,j_matreg)
				!	call j_defmatrix(ivclass,'%last',nobs,1,j_matreg,ive)
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=j-ifi+1
						!				call j_putmatrix(ive,i,1,dble(j-ifi+1) )
					enddo !j=ifi,last0(i)   9056
					ifi=last0(i)+1
				enddo !i=1,nclass   9055
 
				call j_printname('   done ',ive,' ')
			endif !if(nexpand.lt.0)   9047
 
 
		endif !if(iobsw.ge.0.and.ia.eq.1)   9046
		!	write(6,*)'<454545 imean0',imean0
		!	write(6,*)'ia,arg,narg',ia,arg,narg,'imean0',imean0
		call j_getname(arg(ia))
		!	write(6,*)'var0 ',j_oname(1:j_loname)
		if(imean0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				!			write(6,*)'<66 ',ia,arg
 
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%mean',int8(nclass),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%mean'//j_cline(1:lext),int8(nclass),j_18,j_matreg)
 
 
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=mean0(1:nclass)
				!	call j_putmatrix(ive,i,1,mean0(i) )
				!	enddo !i=1,nclass   8484
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%mean',int8(j_dnobs),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%mean'//j_cline(1:lext),int8(j_dnobs),j_18,j_matreg)
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						!write(6,*)'ijmean',ij,'mean',mean0(i)
						j_o(ive)%d(ij)=mean0(i)
						!		call j_putmatrix(ive,ij,1,mean0(i) )
					enddo !j=ifi,last0(i)   9095
					ifi=last0(i)+1
				enddo !i=1,nclass   9094
			endif !if(nexpand.lt.0)   9075
			call j_printname('   done ',ive,' ')
		endif !if(imean0.ge.0)   9073
 
		if(iw.ge.0)then
			ij=0
			ifi=1
			do i=1,nclass
				do j=ifi,last0(i)
					ij=ij+1
					!write(6,*)'ijol',ij,' invector ',j_o(ivew)%d(ij),'mean',mean0(i)
					j_o(ivew)%d(ij)=j_o(ivew)%d(ij)-mean0(i)
 
				enddo !j=ifi,last0(i)   9111
				ifi=last0(i)+1
			enddo !i=1,nclass   9110
			call j_printname('   done ',ivew,' ')
		endif !if(iw.ge.0)   9107
 
 
 
		if(ivar0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%var',int8(nclass),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%var'//j_cline(1:lext),int8(nclass),j_18,j_matreg)
 
				!			call j_printname('making ',ive,' ')
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=var0(1:nclass)
				!			call j_putmatrix(ive,i,1,var0(i) )
				!	enddo !i=1,nclass   8531
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),j_varname1(1:le)//']%%var',int8(j_dnobs),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),j_varname1(1:le)//']%%var'//j_cline(1:lext),int8(j_dnobs),j_18,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=var0(i)
						!		call j_putmatrix(ive,ij,1,var0(i) )
					enddo !j=ifi,last0(i)   9145
					ifi=last0(i)+1
				enddo !i=1,nclass   9144
			endif !if(nexpand.lt.0)   9126
			call j_printname('   done ',ive,' ')
		endif !if(ivar0.ge.0)   9124
 
		if(isd0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%sd',int8(nclass),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%sd'//j_cline(1:lext),int8(nclass),j_18,j_matreg)
 
				!			call j_printname('making ',ive,' ')
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=sd0(1:nclass)
				!			call j_putmatrix(ive,i,1,sd0(i) )
				!	enddo !i=1,nclass   8561
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%sd',int8(j_dnobs),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%sd'//j_cline(1:lext),int8(j_dnobs),j_18,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=sd0(i)
						!	call j_putmatrix(ive,ij,1,sd0(i) )
					enddo !j=ifi,last0(i)   9177
					ifi=last0(i)+1
				enddo !i=1,nclass   9176
			endif !if(nexpand.lt.0)   9158
			call j_printname('   done ',ive,' ')
		endif !if(isd0.ge.0)   9156
 
		if(isum0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%sum',int8(nclass),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%sum'//j_cline(1:lext),int8(nclass),j_18,j_matreg)
 
				!			call j_printname('making ',ive,' ')
				!		do i=1,nclass
				j_o(ive)%d(1:nclass)=sum0(1:nclass)
				!		call j_putmatrix(ive,i,1,sum0(i) )
				!	enddo !i=1,nclass   8591
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%sum',int8(j_dnobs),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%sum'//j_cline(1:lext),int8(j_dnobs),j_18,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=sum0(i)
						!		call j_putmatrix(ive,ij,1,sum0(i) )
					enddo !j=ifi,last0(i)   9209
					ifi=last0(i)+1
				enddo !i=1,nclass   9208
			endif !if(nexpand.lt.0)   9190
			call j_printname('   done ',ive,' ')
		endif !if(isum0.ge.0)   9188
 
		if(imin0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%min',int8(nclass),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%min'//j_cline(1:lext),int8(nclass),j_18,j_matreg)
 
				!			call j_printname('making ',ive,' ')
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=mini0(1:nclass)
				!	call j_putmatrix(ive,i,1,mini0(i) )
				!enddo !i=1,nclass   8621
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%min',int8(j_dnobs),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%min'//j_cline(1:lext),int8(j_dnobs),j_18,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=mini0(i)
						!	call j_putmatrix(ive,ij,1,mini0(i) )
					enddo !j=ifi,last0(i)   9241
					ifi=last0(i)+1
				enddo !i=1,nclass   9240
			endif !if(nexpand.lt.0)   9222
			call j_printname('   done ',ive,' ')
		endif !if(imin0.ge.0)   9220
 
		if(imax0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%max',int8(nclass),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%max'//j_cline(1:lext),int8(nclass),j_18,j_matreg)
 
				!		call j_printname('making ',ive,' ')
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=maxi0(1:nclass)
				!		call j_putmatrix(ive,i,1,maxi0(i) )
				!	enddo !i=1,nclass   8651
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%max',int8(j_dnobs),j_18,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%max'//j_cline(1:lext),int8(j_dnobs),j_18,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=maxi0(i)
						!			call j_putmatrix(ive,ij,1,maxi0(i) )
					enddo !j=ifi,last0(i)   9273
					ifi=last0(i)+1
				enddo !i=1,nclass   9272
			endif !if(nexpand.lt.0)   9254
			call j_printname('   done ',ive,' ')
		endif !if(imax0.ge.0)   9252
 
 
 
 
	enddo vloop !op: do ia=1,nlo   8849
	!	write(6,*)'tas'
 
	if(allocated(mean0))deallocate(mean0)
	if(allocated(var0))deallocate(var0)
	if(allocated(sd0))deallocate(sd0)
	if(allocated(sum0))deallocate(sum0)
	if(allocated(mini0))deallocate(mini0)
	if(allocated(maxi0))deallocate(maxi0)
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
	if(nrej.gt.0)write(6,*)'rejected ',nrej,' observations'
	write(6,*)' '
	return
 
 
 
end subroutine classvector !subroutine classvector(iob,io)
 
subroutine varcomp(iob,io)
	integer,intent(in)::iob
	integer,intent(in)::io
	! Section varcomp varcomp() Variance and covariance components
	! TO BE RAPORTED LATER, see old manual
	! endsection
 
	integer, dimension(:), pointer :: arg !arguments of the function
	integer, dimension(:), pointer :: classv !arguments of the function
	integer, dimension(:), pointer :: cov !arguments of the function
	double precision,dimension(:),allocatable :: s,xs,xss,sb,sumwt,xmi,xma,xs2
	double precision s2,el
	logical iscov
	logical isfunc
	integer*8::ndim2,narg8,iobsw
	integer ::i,j,j2
	id(i)=i*(i+1)/2
 
	call j_startfunction(iob,io,j_ipreal,narg,arg,ivout);if(j_err)return
	narg8=narg
	if(j_err)return
	linkfunc=j_codelink(iob,io,j_mfunc); if(j_err)return
	isfunc=linkfunc.ne.0
	if(j_err)return
	if(narg.le.0.and..not.isfunc)then
		write(6,*)'*varcomp: there must be arguments or func'
		j_err=.true.
	endif !if(narg.le.0.and..not.isfunc)   9329
	if(isfunc)narg=1
 
	call j_getoption(iob,io,j_mclass,1,1,j_ipreal,.true.,nclassv,classv)
	if(j_err)return
	call j_getoption(iob,io,j_mcov,-1,0,j_ipreal,.false.,icov,cov)
	if(j_err)return
	call j_getoption(iob,io,j_mcorr,-1,0,j_ipreal,.false.,icorr,cov)
	if(j_err)return
	iscov=icov.ge.0.or.icorr.ge.0
	if(ivout.eq.j_ivresult.and.iscov)then
		write(6,*)'*varcomp: there must be output with cov-> and corr->'
		j_err=.true.
	endif !if(ivout.eq.j_ivresult.and.iscov)   9342
	call j_getdataobject(iob,io) ;if(j_err)return
	!call j_getdatasets(iob)
 
	!!call j_clearoption(iob,io)  ! subroutine
	if(j_err)return
 
	lkm=narg
	if(iscov)lkm=narg*(narg+1)/2
	if(iscov.and.isfunc)then
		write(6,*)'there cannot be func-> and corr-> or cov->'
		j_err=.true.;return
	endif !if(iscov.and.isfunc)   9354
	allocate(s(1:lkm),xs(1:lkm),xss(1:lkm),sb(1:lkm),sumwt(1:lkm),xmi(1:lkm),xma(1:lkm),xs2(1:lkm))
	ntot=0
 
	call j_dbw(-2,s,lkm,iobsw,nmets,xs,xss,sb,sumwt,xmi,xma,xs2,s2)
	missing=0
	!do k=1,jndatasetss
 
	!call j_getdataset(j_datasets(k),j_dnobs) !initilize dtaset k, nobs will be the number of observations
	! write(6,*)'from',j_from,j_duntil
 
		obloop:	do j_iobs=j_dfrom,j_duntil

		call j_getobs(j_iobs)   ! get next observation compute transformations
 
		if(j_err)return
 
		!there may be errors in computing transformations
 
		if(j_rejected)cycle ! j_rejected tells if the observation is rejected
		if(isfunc)then
			j_dapu=j_codevalue(iob,linkfunc)
			if(j_err)return
			if(abs(j_dapu).ge.1.7d18)then
				missing=missing+1
				cycle obloop
 
			endif !if(abs(j_dapu).ge.1.7d18)   9380
		else
			do j=1,narg
				if(abs(j_v(arg(j))).ge.1.7d18)then
					missing=missing+1
					cycle obloop
 
				endif !if(abs(j_v(arg(j))).ge.1.7d18)   9387
			enddo !j=1,narg   9386
		endif !if(isfunc)   9377
		ntot=ntot+1
		!	 write(6,*)v(o(iob)%i(io+1+1)),v(o(iob)%i(io+1+2))
		mets=j_v(classv(1))
		jc=1
		do j=1,narg
			if(iscov)then
				do j2=1,j-1
					s(jc)=j_v(arg(j2))+j_v(arg(j))
					jc=jc+1
				end do !j2=1,j-1   9400
			end if !if(iscov)   9399
			if(isfunc)then
				s(jc)=j_dapu
			else
				s(jc)=j_v(arg(j))
			endif !if(isfunc)   9405
			jc=jc+1
		end do !j=1,narg   9398
 
		call j_dbw(mets,s,lkm,iobsw,nmets,xs,xss,sb,sumwt,xmi,xma,xs2,s2)
 
	enddo obloop !oop:	do j_iobs=j_dfrom,j_duntil   9368
	!	end do !do k=1,jndatasetss
 
	call j_dbw(-1,s,lkm,iobsw,nmets,xs,xss,sb,sumwt,xmi,xma,xs2,s2)
 
	write(6,'(a,i5,a,i4)')'Accepted ',ntot,' out of ',j_dnobs,' classes:',nmets
	if(missing.gt.0)write(6,*)'missing ',missing
	j_v(j_ivaccepted)=ntot
	if(ntot.le.4)then
		write(6,*)'*not enough observations'
 
		return
	endif !if(ntot.le.4)   9423
 
	write(6,*)&
		'                average     GLS mean      sb         sw'// &
		'        RMSE'
	if(isfunc)then
		write(6,'(a15,8g13.5e2)')&
			'func->',xs(1),&
			xs2(1), & !xss(j2)
			sb(1),sumwt(1),sqrt(sb(1)**2+sumwt(1)**2)
		j2=1
		write(6,*)xs(1)
	else
		do j=1,narg
			j2=j
			if(iscov)	j2=j*(j+1)/2
			call j_getname(arg(j))
			write(6,'(a15,8g12.5e1)')&
				j_oname(1:min(15,j_loname)),xs(j2),&
				xs2(j2), & !xss(j2)
				sb(j2),sumwt(j2),sqrt(sb(j2)**2+sumwt(j2)**2)  !,&
 
			!     100.*sb(j2)**2/(sumwt(j2)**2+sb(j2)**2)
		enddo !j=1,narg   9440
	endif !if(isfunc)   9432
	if(ivout.eq.j_ivresult)return
	if(narg.eq.1)then
		ivvarb=j_getobject(ivout,'%varb',j_ipreal);if(j_err)return
		ivvarw=j_getobject(ivout,'%varw',j_ipreal);if(j_err)return
		ivglsmean=j_getobject(ivout,'%glsmean',j_ipreal);if(j_err)return
		ivmean=j_getobject(ivout,'%mean',j_ipreal);if(j_err)return
		j_v(ivvarb)=sb(j2)**2
		j_v(ivvarw)=sumwt(j2)**2
		j_v(ivglsmean)=xs2(j2)
		j_v(ivmean)=xs2(j2)
		return
	endif !if(narg.eq.1)   9453
 
	ndim2=1
	if(iscov)ndim2=narg
	if(icov.ge.0)then
		ivvarb=j_defmatrix(ivout,'%varb',narg8,ndim2,j_matreg);if(j_err)return
		call j_printname('matrix ',ivvarb,' generated')
		ivvarw=j_defmatrix(ivout,'%varw',narg8,ndim2,j_matreg)
		call j_printname('matrix ',ivvarw,' generated')
	elseif(icorr.lt.0)then !if(icov.ge.0)then
		ivvarb=j_defmatrix(ivout,'%varb',narg8,ndim2,j_matreg)
		call j_printname('vector ',ivvarb,' generated')
		ivvarw=j_defmatrix(ivout,'%varw',narg8,ndim2,j_matreg)
		call j_printname('vector ',ivvarw,' generated')
 
	endif !if(icov.ge.0)   9467
	if(icorr.ge.0)then
		ivSb=j_defmatrix(ivout,'%corrb',narg8,ndim2,j_matreg)
		call j_printname('matrix ',ivsb,' generated')
		ivSw=j_defmatrix(ivout,'%corrw',narg8,ndim2,j_matreg)
		call j_printname('matrix ',ivsw,' generated')
	endif !if(icorr.ge.0)   9479
	ivglsmean=j_defmatrix(ivout,'%glsmean',narg8,j_18,j_matreg)
	call j_printname('vector ',ivglsmean,' generated')
	ivmean=j_defmatrix(ivout,'%mean',narg8,j_18,j_matreg)
	call j_printname('vector ',ivmean,' generated')
	jc=1
	do j=1,narg
		if(iscov)then
			do j2=1,j-1
				if(icov.ge.0)then
					el=0.5*(sb(jc)**2-sb(id(j))**2-sb(id(j2))**2)
					call j_putmatrix(ivvarb,j,j2,el ) !s(jc)=j_v(arg(j2))+j_v(arg(j))
					call j_putmatrix(ivvarb,j2,j,el ) !s(jc)=j_v(arg(j2))+j_v(arg(j)
					el=0.5*(sumwt(jc)**2-sumwt(id(j))**2-sumwt(id(j2))**2)
					call j_putmatrix(ivvarw,j,j2,el )
					call j_putmatrix(ivvarw,j2,j,el )
				endif !if(icov.ge.0)   9493
				if(icorr.ge.0)then
					el=0.5*(sb(jc)**2-sb(id(j))**2-sb(id(j2))**2)/(sb(id(j))*sb(id(j2)))
					call j_putmatrix(ivsb,j,j2, el) !s(jc)=j_v(arg(j2))+j_v(arg(j))
					call j_putmatrix(ivsb,j2,j,el ) !s(jc)=j_v(arg(j2))+j_v(arg(j))
					el=0.5*(sumwt(jc)**2-sumwt(id(j))**2-sumwt(id(j2))**2)/(sumwt(id(j))*sumwt(id(j2)))
					call j_putmatrix(ivsw,j,j2,el)
					call j_putmatrix(ivsw,j2,j,el)
				endif !if(icorr.ge.0)   9501
				jc=jc+1
			end do !j2=1,j-1   9492
		end if !if(iscov)   9491
		if(icov.ge.0)then
			call j_putmatrix(ivvarb,j,j,sb(jc)**2)
			call j_putmatrix(ivvarw,j,j,sumwt(jc)**2)
		elseif(icorr.lt.0)then !if(icov.ge.0)then
			call j_putmatrix(ivvarb,j,1,sb(jc)**2)
			call j_putmatrix(ivvarw,j,1,sumwt(jc)**2)
 
		endif !if(icov.ge.0)   9512
		if(icorr.ge.0)then
			call j_putmatrix(ivsb,j,j,1.d0)
			call j_putmatrix(ivsw,j,j,1.d0)
		endif !if(icorr.ge.0)   9520
		call j_putmatrix(ivglsmean,1,j,xs2(jc))
		call j_putmatrix(ivmean,1,j,xs(jc))
 
		jc=jc+1
	end do !j=1,narg   9490
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
 
end subroutine !subroutine varcomp(iob,io)
 
subroutine plane(iob,io)
	!note: use lines are generated by j_precompiler
	! Section plane plane() Interpolates an a plane
	! Usage:\\
	! plane(x1,x2,x3,y1,y2,y3,z1,z2,z3,x,y]\\
	! The function computes the equation of plane going through the three points (x1,y1,z1), etc
	! and computes the value of the z-coordinate in point (x,y). The three points defining the plane
	! cannot be on single line.
	! endsection
	integer,intent(in)::iob
	integer,intent(in)::io
 
	integer, dimension(:), pointer :: arg !arguments of the function
 
	!j-function needs to start with startfunction
	!subroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	! iob,io current compiled object and the place there
	! iptype type of arguments for j-function, look at j_modules.f90 for available types
	! expand logical variable telling if a single list argument is expanded
	! narg number of arguments
	! arg integer pointer to argumensts, if there are no arguments it points to j_arglist0 which
	! has one argument whose value is zero
	! limits of number of arguments checked during the interpretation is given j_minarg and j_maxarg
	!  in  j_modules.f90 or  for o functions in oX_mod.f90
	! ivout is the output object
	!for example j_minarg=1 and j_maxarg=999 and if the argument is list it is expanded
 
	!if the function had an output which cannot be the same as any of the arguments or
	! any of the option arguments or any of the arguments of a transformations set arguments
	! then there should be first
 
	!	call j_checkoutput(iob,io)
 
	call j_startfunction(iob,io,j_ipreal,narg,arg,ivout);if(j_err)return
	if(j_err)return
	j_v(ivout)=j_interplane(j_v(arg(1)),j_v(arg(2)),j_v(arg(3)),j_v(arg(4)),j_v(arg(5)),j_v(arg(6)),&
		j_v(arg(7)),j_v(arg(8)),j_v(arg(9)),j_v(arg(10)),j_v(arg(11)))
	return
 
end subroutine !subroutine plane(iob,io)
subroutine logistic(iob,io)
	! Section logistic logistic() Logistic function
	! Returns the value of the logistic function 1/(1+exp(-x)). This can in principle computed by the
	! transformation, but the transformation will produce an error condition when the argument -x
	! of the exp-function is large. Because the logistic function is symmetric, these cases are
	! computed as exp(x)/(1+exp(x)). Because the logistic function can be needed in the nonlinear
	! regression, also the derivatives are implemented.  Note, to utilize derivatives
	! the function needs to be in a TRANS object.
	! Eg when f=logistic(a*(x-x0)), then
	! the derivatives can be obtained with respect to the parameters a and x0 by
	! endheader
	! Ex logisticex Example of logistic function
	! transa=trans()
	! der(a,x0)
	! f=logistic(a*(x-x0));
	! /
 
	! x,x0,a=10,5,0.1
	!call(transa)
	! endex
	! Note In the previous example tr(d[x0] ahs the effect that TRANS tr is first
	! called, which makes that also d[a] and d[x] have been computed. Remember that
	! the parse tree is computed from right to left.
	! endnote
	! endsection
	integer,intent(in)::iob
	integer,intent(in)::io
	double precision::arg
	!note: use lines are generated by j_precompiler
 
	!	integer, dimension(:), pointer :: arg !arguments of the function
 
	!j-function needs to start with startfunction
	!subroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	! iob,io current compiled object and the place there
	! iptype type of arguments for j-function, look at j_modules.f90 for available types
	! expand logical variable telling if a single list argument is expanded
	! narg number of arguments
	! arg integer pointer to argumensts, if there are no arguments it points to j_arglist0 which
	! has one argument whose value is zero
	! limits of number of arguments checked during the interpretation is given j_minarg and j_maxarg
	!  in  j_modules.f90 or  for o functions in oX_mod.f90
	! ivout is the output object
	!for example j_minarg=1 and j_maxarg=999 and if the argument is list it is expanded
 
	!if the function had an output which cannot be the same as any of the arguments or
	! any of the option arguments or any of the arguments of a transformations set arguments
	! then there should be first
 
	!	call j_checkoutput(iob,io)
	narg=j_o(iob)%i(io+1)
	ivout=j_o(iob)%i(io+narg+2)
	arg=j_v( j_o(iob)%i(io+2))
	j_v(ivout)=j_1/(j_1+exp(-arg))
	!	io=io+3    ! this is not a regular function, it does not have the number of arguments, becasue
	! one can get derivatives, which are implemented wfor funtion which do dot have number of arguments explicitly
	!	write(6,*)'arg,j_v(ivout),ivout',arg,j_v(ivout),ivout
	return
 
end subroutine !subroutine logistic(iob,io)
 
 
subroutine bilin(iob,io)
	!note: use lines are generated by j_precompiler
 
	! Section bilin bilin() Bilinear interpolation
	! Usage:\\
	! bilin(x1,x2,y1,y2,z1,z2,z3,z4,x,y]\\
	! z1 is the value of function at point (x1,y1), z2 is the value at point (x1,y2), z3 is the value at
	! (x2,y1) and z4 is the value at (x2,y2): the function is using bilinear interpolation to compute
	! the value of the z-coordinate in point (x,y). The point (x,y) needs not be within the square
	! defined by the corner points, but it is good if it is. See Press et al. ? (or Google) for the principle
	! of bilinear interpolation
	! endsection
	integer,intent(in)::iob
	integer,intent(in)::io
 
	integer, dimension(:), pointer :: arg !arguments of the function
 
	!j-function needs to start with startfunction
	!subroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	! iob,io current compiled object and the place there
	! iptype type of arguments for j-function, look at j_modules.f90 for available types
	! expand logical variable telling if a single list argument is expanded
	! narg number of arguments
	! arg integer pointer to argumensts, if there are no arguments it points to j_arglist0 which
	! has one argument whose value is zero
	! limits of number of arguments checked during the interpretation is given j_minarg and j_maxarg
	!  in  j_modules.f90 or  for o functions in oX_mod.f90
	! ivout is the output object
	!for example j_minarg=1 and j_maxarg=999 and if the argument is list it is expanded
 
	!if the function had an output which cannot be the same as any of the arguments or
	! any of the option arguments or any of the arguments of a transformations set arguments
	! then there should be first
 
	!	call j_checkoutput(iob,io)
 
	call j_startfunction(iob,io,j_ipreal,narg,arg,ivout)
	if(j_err)return
	j_v(ivout)=j_bilin(j_v(arg(1)),j_v(arg(2)),j_v(arg(3)),j_v(arg(4)),j_v(arg(5)),j_v(arg(6)),&
		j_v(arg(7)),j_v(arg(8)),j_v(arg(9)),j_v(arg(10)))
	return
 
end subroutine !subroutine bilin(iob,io)
 
 
 
subroutine dos(iob,io)  !do loops in input programming  ;do()
	integer,intent(in)::iob
	integer,intent(in)::io
	integer ivout_
	integer, dimension (:), pointer :: arg
 
	!	io=io_
	call j_startfunction(iob,io,0,narg,arg,ivout_);if(j_err)return
 
	if(j_ndo.ge.j_mxndo)then
		write(6,*)j_ndo,j_mxndo
		write(6,*)'**too many ;do -loops'
		j_err=.true.
		j_ndo=0
		return
	endif !if(j_ndo.ge.j_mxndo)   9690
	if(j_ndo.eq.j_ndoinc(j_ninc))then
		if(j_reacom(j_ninc))then
			ndotemp=1
			if(.not.j_savcom(j_ninc))then
				j_ivbuf(j_ninc)=j_deftext2(0,'Buffer'//char(47+j_ninc),500,10000,50)
				j_savcom(j_ninc)=.true.
				call j_puttext(j_ivbuf(j_ninc), j_inp(1:j_linp))
				istart=1
			else !if(.not.j_savcom(j_ninc))then
				istart=j_o(j_ivbuf(j_ninc))%i(0)  !cuuren line in text buffer
			endif !if(.not.j_savcom(j_ninc))   9700
			icu=istart
			!      write(6,*)'istart',istart
			100	 	format(a)
			10 		continue
			! if(j_nul(j_ninc).gt.0)then
				! read(j_nul(j_ninc),100,end=90,err=90)j_inp
				! j_linp=j_lentrim(j_inp)
			! else !if(j_nul(j_ninc).gt.0)then
				! ifi=-j_nul(j_ninc)
				! if(j_nulline(j_ninc).ge.j_o(ifi)%i(0))goto 90
				! j_nulline(j_ninc)=j_nulline(j_ninc)+1
				! call j_getline(ifi,j_nulline(j_ninc),j_inp(1:),j_linp)
				! if(j_err)return
			! endif !if(j_nul(j_ninc).gt.0)then

			j_ialb=j_nonblank(j_inp,1,j_linp)
			ialb2=j_nonblank(j_inp,j_ialb+3,j_linp)
			if(j_inp(1:j_linp).eq.'endsimu')then
				j_inp=';enddo';j_linp=6 !;ialb2=1
			endif !if(j_inp(1:j_linp).eq.'endsimu')   9725
			call j_puttext(j_ivbuf(j_ninc), j_inp(1:j_linp))
 
			icu=icu+1
 
			if(j_inp(j_ialb:j_ialb+2).eq.';do'.and.j_inp(ialb2:ialb2).eq.'(')then
				ndotemp=ndotemp+1
			endif !if(j_inp(j_ialb:j_ialb+2).eq.';do'.and.j_inp(ialb2:ialb2).   9732
			if(j_inp(j_ialb:j_ialb+5).eq.';enddo'.or.j_inp(j_ialb:j_ialb+6).eq.';end do')then
				ndotemp=ndotemp-1
			endif !if(j_inp(j_ialb:j_ialb+5).eq.';enddo'.or.j_inp(j_ialb:j_ia   9735
 
			if(ndotemp.eq.0)goto 17
			goto 10
			90 		write(6,*)'unclosed ;do loop'
			j_err=.true.
 
			goto 900
		else !if(j_reacom(j_ninc))then
			istart=j_o(j_ivbuf(j_ninc))%i(0)
 
		endif !if(j_reacom(j_ninc))   9698
	else !if(j_ndo.eq.j_ndoinc(j_ninc))then
		istart=j_icurl(j_ninc)-1 ! this is set to next already
	endif !if(j_ndo.eq.j_ndoinc(j_ninc))   9697
 
	17 ilow=j_v(j_o(iob)%i(io+3)); iup=j_v(j_o(iob)%i(io+4))
	if(narg.le.3)then
		istep=1
	else !if(narg.le.3)then
		istep=j_v(j_o(iob)%i(io+5))
	endif !if(narg.le.3)   9754
 
	if(istep.eq.0)then
		write(6,*)'**illegal ;do loop with low,up,step:',ilow,iup,istep
		j_err=.true.
 
		goto 900
	endif !if(istep.eq.0)   9760
 
	nstep=(iup-ilow+istep)/istep
	if(nstep.le.0)then
 
		if(j_ndo.eq.0)j_icurl(j_ninc)=istart+1
		567		call j_getline(j_ivbuf(j_ninc),j_icurl(j_ninc),j_inp(1:j_lline),j_linp)
		if(j_err)return
		ialb2=4
		ndotemp=1
		if(j_inp(1:3).eq.';do'.and.j_inp(4:4).eq.'(')ndotemp=ndotemp+1
		if(j_inp(1:6).eq.';enddo')ndotemp=ndotemp-1
		j_icurl(j_ninc)=j_icurl(j_ninc)+1
		if(ndotemp.gt.0)goto 567
		!!call j_clearoption(iob,io)  ! subroutine
		return
	endif !if(nstep.le.0)   9768
	j_ndo=j_ndo+1
	j_niifsindo(j_ndo)=j_niifs
 
	j_iido(1,j_ndo)=ilow
	j_iido(2,j_ndo)=iup  !ilow+(nstep-1)*istep !up
	j_iido(3,j_ndo)=istep
	j_iido(4,j_ndo)=j_o(iob)%i(io+2)
	j_v(j_o(iob)%i(io+2))=ilow
	j_iido(5,j_ndo)=istart
	j_iido(6,j_ndo)=j_ninc
	j_iido(7,j_ndo)=ilow !  iido(1 is updated, thsi stores the intial value
	if(j_ndo.eq.1)j_printdo=j_linkoption(iob,io,j_mprint).ge.0
	!!call j_clearoption(iob,io)  ! subroutine
 
	if(j_ndo.eq.1)then
		j_icurl(j_ninc)=istart+1
	endif !if(j_ndo.eq.1)   9796
	j_reacom(j_ninc)=.false.
	900 continue
	!!call j_clearoption(iob,io)  ! subroutine
	return
end subroutine dos !subroutine dos(iob,io)
 
 
subroutine gotos_(iob,io) ! ;goto  in input programming
	integer,intent(in)::iob
	integer,intent(in)::io
	logical sav
 
	!	io=io_
	!	io_=io_+3+j_o(iob)%i(io_+1)
	iva=j_o(iob)%i(io+2)
	if(j_ninc.lt.2.or.j_ipc(iva).eq.0)then
		write(6,*)'**illegal ;goto';j_err=.true.
		return
	end if !if(j_ninc.lt.2.or.j_ipc(iva).eq.0)   9814
 
	if(j_ndo.gt.0)then
		write(6,*)'**open ;do loops closed'
		j_ndo=0
	endif !if(j_ndo.gt.0)   9819
 
	iva1=j_o(iva)%i(1);iva2=j_o(iva)%i(2)
	ivb=j_ivbuf(j_ninc);lena=iva2-iva1+1
	if(ivb.ne.0.and.j_otype(max(ivb,1)).eq.j_iptext)then !search for labels
			adrloop: do i=1,j_o(ivb)%i2(0)
			iil=j_o(ivb)%i2(i)
			j1=j_o(ivb)%i(iil)+1;j2=j_o(ivb)%i(iil+1)-2
			if(j2-j1.ne.iva2-iva1)cycle
			do j=0,iva2-iva1
				if(j_o(j_ivnames)%ch(iva1+j).ne.j_o(ivb)%ch(j1+j))cycle adrloop
			end do !j=0,iva2-iva1   9831
			j_icurl(j_ninc)=iil
			j_reacom(j_ninc)=.false.
			return !adrloop: do i=1,j_o(ivb)%i2(0)
 
		enddo adrloop !loop: do i=1,j_o(ivb)%i2(0)   9827
		sav=.true.
	else !if(ivb.ne.0.and.j_otype(max(ivb,1)).eq.j_iptext)then
		!not saving
		sav=.false.
	end if !if(ivb.ne.0.and.j_otype(max(ivb,1)).eq.j_iptext)   9826
	!like in incl
	17 continue  !read(j_nul(j_ninc),'(a)',end=95,err=99)j_inp
	j_reacom(j_ninc)=.true.
	lin=j_lentrim(j_inp)
 
	if(lin.le.0) goto 17
 
	ialgo=j_nonblank(j_inp,1,lin)
	! from getinput except here nul(0)+1
	if(j_inp(ialgo:ialgo).eq.';'.and.j_inp(lin:lin).eq.':')then
		! satrt saving
		if(.not.sav)then
			j_ivbuf(j_ninc)=j_deftext2(0,'Buffer'//char(47+j_ninc),500,10000,50)
			sav=.true.
			j_savcom(j_ninc)=.true.
		end if !if(.not.sav)   9855
		call j_puttext2(j_ivbuf(j_ninc), j_inp(ialgo:lin))
 
		if(lena.ne.lin-1-ialgo)goto 17
		do j=0,iva2-iva1
			if(j_o(j_ivnames)%ch(iva1+j).ne.j_inp(j+2:j+2))goto 17
		end do !j=0,iva2-iva1   9863
	else !if(j_inp(ialgo:ialgo).eq.';'.and.j_inp(lin:lin).eq.':')then
		! not adr
		if(sav)call j_puttext(j_ivbuf(j_ninc), j_inp(ialgo:lin))
		goto 17
	end if !if(j_inp(ialgo:ialgo).eq.';'.and.j_inp(lin:lin).eq.':')   9853
 
	!	90 continue  !io=io+3+o(iob)%i(io+1)
	return
 
	99 write(6,*)'**error reading commands'
	j_err=.true.
	return
 
	95 write(6,*)'**adress not found';j_err=.true.;return
end subroutine gotos_ !subroutine gotos_(iob,io)
 
 
 
subroutine rannegbin(iob,io)
	!Section rannegbin rannegbin() Negative binomial
	! The  function  returns  random  number  distributed  according to the
	! negative binomial distribution.
	!endheader
	!Option
	! Output&1&REAL | MATRIX & the number of successes in
	! independent Bernoul trials before r’th failure when
	! p is the probability of success. ranbin(r,1)returns 1.7e37 and
	! ranbin(r,0)returns 0.
	! !Args&&REAL& r=]Arg1[ and p=]Arg1[
	!endoption
 
	! Note  there are different ways to define the negative binomial distribution. In this definition
	! a Poisson random variable with mean $\lamda$ is obtained by letting r go
	! to infinity and defining p= $\lamda$/( $\lamda$+r)
	! The mean E(x) of this definition is p*r/(1-p) and the variance is V=p*r/(1-p)2. Thus given
	! E(x) and V, r and p can be obtained as follows: p=1- E(x) /V and r= E(x)**2/(V- E(x)) . This is useful when
	! simulating ‘overdispersed Poisson’ variables. Sorry for the (temporary) inconsistency of parameters with
	! function negbin().
	! endnote
	! Note can also have a noninteger values. This is not in accordance with the above
	! interpretation of the distribution, but it is compatible with interpreting negative binomial
	! distribution as a compound gamma-Poisson distribution and it is useful when simulating
	! overdispersed Poisson distributions.
	! endnote
	! endsection
	integer,intent(in)::iob
	integer,intent(in)::io
 
	real n_
	!!call j_clearoption(iob,io)
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
 
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_named)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)   9921
	n_=j_v(j_o(iob)%i(io+2))
	if(n_.le.0)then
		write(6,*)'*rannegbin: n (=1st arg) must be greater than zero'
		j_err=.true.
 
	endif !if(n_.le.0)   9925
	pp_=j_v(j_o(iob)%i(io+3))
	if(pp_.lt.0.or.pp_.gt.1.)then
		write(6,*)'*rannegbin: p (=2nd arg) must be between 0 and 1, it was ',pp_
		j_err=.true.
	endif !if(pp_.lt.0.or.pp_.gt.1.)   9931
	if(j_err)return
 
	pp_=1.-pp_
 
	if(j_otype(iout).eq.j_ipreal)then
		if(pp_.eq.1.)then
 
			j_v(iout)=j_0
		elseif(pp_.eq.0.)then !if(p_.eq.1.)then
			j_v(iout)=1.7e37
		else !if(p_.eq.1.)then
			j_v(iout)=ignnbn(n_,pp_)
		endif !if(pp_.eq.1.)   9940
 
	elseif(pp_.eq.1.)then
		j_o(iout)%d(1: j_o(iout)%i(3))=j_0
	elseif(pp_.eq.0.)then
		j_o(iout)%d(1: j_o(iout)%i(3))=1.7e37
	else
		do i=1,j_o(iout)%i(3)
 
			j_o(iout)%d(i)=ignnbn(n_,pp_)
 
		enddo !i=1,j_o(iout)%i(3)   9954
	endif !if(j_otype(iout).eq.j_ipreal)   9939
 
	return
 
end subroutine !subroutine rannegbin(iob,io)
 
 
subroutine elementsum(iob,io)   ! %%matrix
 
	call j_checkoutput(iob,io)
	if(j_err)return
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iarg=j_o(iob)%i(io+2)
	if(j_otype(iarg).ne.j_ipmatrix)then
		call j_printname('**elementsum in trans-object',iob,' Not a matrix:', iarg)
		j_err=.true. ;return
	endif !if(j_otype(iarg).ne.j_ipmatrix)   9977
	call j_getoption(iob,io,j_mrow,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	!ivrow_=j_igetopt(iob,io,j_mrow)
	if(noptarg.gt.0)then
		ivrow=j_optarg0(1)
		irow=j_v(ivrow)
		if(irow.le.0.or.irow.gt.j_o(iarg)%i(1))then
			call j_printname('**elementsum in trans-object',iob,' illegal row for matrix ', iarg)
			j_err=.true.  ;return
		endif !if(irow.le.0.or.irow.gt.j_o(iarg)%i(1))   9987
	else
		irow=0
	endif !if(noptarg.gt.0)   9984
	call j_getoption(iob,io,j_mcolumn,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	!ivcolumn=j_igetopt(iob,io,j_mcolumn)
	if(noptarg.gt.0)then
		ivcolumn=j_optarg0(1)
		icolumn=j_v(ivcolumn)
		if(icolumn.le.0.or.icolumn.gt.j_o(iarg)%i(2))then
			call j_printname('**elementsum in trans-object',iob,' illegal column for matrix ', iarg)
			j_err=.true.  ;return
		endif !if(icolumn.le.0.or.icolumn.gt.j_o(iarg)%i(2))  10000
	endif !if(noptarg.gt.0)   9997
 
	select case (narg)
	case(1) !select case (narg)
	if(ivrow_.gt.0)then
		las=irow*j_o(iarg)%i(2)
		ifi=las-j_o(iarg)%i(2)+1
		j_v(iout)=sum(j_o(iarg)%d(ifi:las))
 
	elseif(ivcolumn.gt.0)then !if(ivrow_.gt.0)then
		su=0
		ii=icolumn
		do i=1,j_o(iarg)%i(1)
			su=su+j_o(iarg)%d(ii)
			ii=ii+j_o(iarg)%i(2)
		enddo !i=1,j_o(iarg)%i(1)  10016
		j_v(iout)=su
	else !if(ivrow_.gt.0)then
		j_v(iout)=sum(j_o(iarg)%d(1:j_o(iarg)%i(3)))
	endif !if(ivrow_.gt.0)  10008
 
	case(2) !select case (narg)
	las=j_v( j_o(iob)%i(io+3))
	if(ivrow_.gt.0)then
		if(las.gt.j_o(iarg)%i(2).or.las.le.0)then
			call j_printname('**elementsum in trans-object',iob,' illegal second argument for matrix ', iarg)
			write(6,*)'there are ',j_o(iarg)%i(2), 'columns, you asked ', las
			j_err=.true.
			return
 
		endif !if(las.gt.j_o(iarg)%i(2).or.las.le.0)  10028
 
		ifi=(irow-1)*j_o(iarg)%i(2)
		j_v(iout)=sum(j_o(iarg)%d(ifi+1:ifi+las))
 
	elseif(ivcolumn.gt.0)then !if(ivrow_.gt.0)then
		if(las.le.0.or.las.gt.j_o(iarg)%i(1))then
			call j_printname('**elementsum in trans-object ',iob,' illegal upper bound for matrix:', iarg)
			write(6,*)'#you gave ',las,'maximum is',j_o(iarg)%i(1)
			j_err=.true. ;return
 
		end if !if(las.le.0.or.las.gt.j_o(iarg)%i(1))  10040
		su=0
		ii=icolumn
		do i=1,las
			su=su+j_o(iarg)%d(ii)
			ii=ii+j_o(iarg)%i(2)
		enddo !i=1,las  10048
		j_v(iout)=su
	else !if(ivrow_.gt.0)then
		if(las.le.0.or.las.gt.j_o(iarg)%i(3))then
			call j_printname('**elementsum in trans-object ',iob,' illegal upper bound for matrix:', iarg)
			write(6,*)'#you gave ',las,'maximum is',j_o(iarg)%i(3)
			j_err=.true. ;return
		end if !if(las.le.0.or.las.gt.j_o(iarg)%i(3))  10054
		j_v(iout)=sum(j_o(iarg)%d(1:las))
	endif !if(ivrow_.gt.0)  10027
 
	case(3) !select case (narg)
	ifi=j_v( j_o(iob)%i(io+3) );las=j_v( j_o(iob)%i(io+4) )
	if(ivrow_.gt.0)then
		if(las.gt.j_o(iarg)%i(2).or.ifi.gt.las.or.ifi.le.0)then
			call j_printname('**elementsum in trans-object',iob,' illegal second or third argument for matrix ', iarg)
			write(6,*)'there are ',j_o(iarg)%i(2), 'columns, you asked from ', ifi ,' to ',las
			j_err=.true.
			return
		endif !if(las.gt.j_o(iarg)%i(2).or.ifi.gt.las.or.ifi.le.0)  10065
		ifi=(irow-1)*j_o(iarg)%i(2)
		j_v(iout)=sum(j_o(iarg)%d(ifi+1:ifi+las))
 
 
	elseif(ivcolumn.gt.0)then !if(ivrow_.gt.0)then
		if(las.le.0.or.las.gt.j_o(iarg)%i(2))then
			call j_printname('**elementsum in trans-object ',iob,' illegal upper bound for matrix:', iarg)
			write(6,*)'#you gave ',las,'maximum is',j_o(iarg)%i(2)
			j_err=.true. ;return
		end if !if(las.le.0.or.las.gt.j_o(iarg)%i(2))  10076
		su=0
		ii=icolumn
		do i=1,las
			su=su+j_o(iarg)%d(ii)
			ii=ii+j_o(iarg)%i(2)
		enddo !i=1,las  10083
		j_v(iout)=su
	else !if(ivrow_.gt.0)then
		if(ifi.le.0.or.ifi.gt.j_o(iarg)%i(3).or.las.le.0.or.las.gt.j_o(iarg)%i(3))then
			call j_printname('**vecsum illegal bound for ',iarg,' in trans ', iob)
			write(6,*)'#you gave ',ifi,las,' maximum is',j_o(iarg)%i(3)
			j_err=.true. ;return
		end if !if(ifi.le.0.or.ifi.gt.j_o(iarg)%i(3).or.las.le.0.or.las.gt  10089
		j_v(iout)=sum(j_o(iarg)%d(ifi:las))
	endif !if(ivrow_.gt.0)  10064
	end select !select case (narg)
	!!call j_clearoption(iob,io)  ! subroutine
	return
end subroutine elementsum !subroutine elementsum(iob,io)
 
subroutine dotproduct(iob,io)  !%%matrix
	integer,intent(in)::iob
	integer,intent(in)::io
	!!call j_clearoption(iob,io)
 
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iarg=j_o(iob)%i(io+2)
	iarg2=j_o(iob)%i(io+3)
	if(j_otype(iarg).ne.j_ipmatrix)then
		call j_printname('**dotproduct in trans-object',iob,' *Not a matrix:', iarg)
		j_err=.true. ;return
	endif !if(j_otype(iarg).ne.j_ipmatrix)  10113
	if(j_otype(iarg2).ne.j_ipmatrix)then
		call j_printname('**dotproduct in trans-object',iob,' *Not a matrix:', iarg2)
		j_err=.true. ;return
	endif !if(j_otype(iarg2).ne.j_ipmatrix)  10117
	nele=min(j_o(iarg)%i(3),j_o(iarg2)%i(3))
	select case (narg)
	case(2) !select case (narg)
	ifi=1;las=j_o(iarg)%i(3)
	case(3) !select case (narg)
	ifi=1;las=j_v( j_o(iob)%i(io+4))
	if(las.le.0.or.las.gt.nele)then
		call j_printname('**dotproduct illegal ubound for ',iarg,' or for: ', iarg2)
		write(6,*)'#you gave ',las,' maximum is ',nele
		j_err=.true. ;return
	end if !if(las.le.0.or.las.gt.nele)  10127
	case(4) !select case (narg)
	ifi=j_v( j_o(iob)%i(io+4) );las=j_v( j_o(iob)%i(io+5) )
	if(ifi.le.0.or.ifi.gt.nele.or.las.le.0.or.las.gt.nele)then
		call j_printname('**dotproduct illegal bound for',iarg,' or for:', iarg2)
		write(6,*)'#you gave ',ifi,las,' maximum is',nele
		j_err=.true. ;return
	end if !if(ifi.le.0.or.ifi.gt.nele.or.las.le.0.or.las.gt.nele)  10134
	end select !select case (narg)
	j_v(iout)=dot_product(j_o(iarg)%d(ifi:las),j_o(iarg2)%d(ifi:las))
end subroutine dotproduct !subroutine dotproduct(iob,io)
 
subroutine loadtrees(iob,io)
 
	narg=j_o(iob)%i(io+1)
	itreeout=0
	nkeep=j_o(j_ivkeeptree)%i(1)
	do itre=j_itree1,j_itree2
		itreeout=itreeout+1
		do i=1,j_gettreevars(0)
			j_o(j_gettreevars(i))%d(itreeout)=j_o(j_ivtreemat)%d((itre-1)*nkeep+j_loctreevars(i))
		enddo !i=1,j_gettreevars(0)  10150
	enddo !itre=j_itree1,j_itree2  10148
	!	io=io+narg+3
	return
end subroutine loadtrees !subroutine loadtrees(iob,io)
 
subroutine assone(iob,io)
 
	iout=j_o(iob)%i(io+3)
	irg=j_o(iob)%i(io+2)
	!	write(6,*)irg,j_ipreal,j_otype(irg),j_ipreal,j_otype(iout),j_ipmatrix
	if(j_otype(irg).eq.j_ipreal)then
 
		if(j_otype(iout).eq.j_ipmatrix)then
			j_o(iout)%d(1: j_nelem(iout))=j_v(irg)
			return
		elseif(j_otype(iout).eq.j_iplist)then
			do i=1,j_o(iout)%i(1)
				ivout=j_o(iout)%i2(i)
				if(j_otype(ivout).ne.j_ipreal)then
					call j_getname(iout,ivout)
					write(6,*)'cannot put REAL into object ',j_oname2(1:j_loname2),' in LIST ',j_oname(1:j_loname)
					j_err=.true.;return
				endif !if(j_otype(ivout).ne.j_ipreal)  10171
				j_v(ivout)=j_v(irg)
 
			enddo !i=1,j_o(iout)%i(1)  10169
			return
		endif !if(j_otype(iout).eq.j_ipmatrix)  10165
		!	else !if(j_otype(iout).eq.j_ipmatrix)then
		!	call j_getname(iout)
		!	write(6,*)'del ',j_oname(1:j_loname)
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_v(irg)
		!	endif !if(j_otype(iout).eq.j_ipmatrix)then
	elseif(j_otype(iout).eq.j_iplist.and.j_otype(irg).eq.j_ipmatrix)then
		isi=j_o(irg)%i(3)
		if(isi.eq.j_o(iout)%i(3))then
			j_v( j_o(iout)%i2(1:isi))=j_o(irg)%d(1:isi)
		else
			call j_getname(irg,iout)
			write(6,*)'the size of list ',j_oname2(1:j_loname2),' ',j_o(iout)%i(3),&
				' does not agree with the size of matrix ',j_oname(1:j_loname), j_o(irg)%i(3)
			j_err=.true.;return
		endif !if(isi.eq.j_o(iout)%i(3))  10189
	elseif(j_otype(irg).eq.j_iplist.and.j_otype(iout).eq.j_ipmatrix)then
		isi=j_o(irg)%i(3)
		if(isi.eq.j_o(iout)%i(3))then
			j_o(iout)%d(1:isi)=j_v(j_o(irg)%i2(1:isi))
		else
			call j_getname(irg,iout)
			write(6,*)'the size of list ',j_oname(1:j_loname),' ',j_o(irg)%i(3),&
				' does not agree with the size of list ',j_oname2(1:j_loname2), j_o(iout)%i(3)
			j_err=.true.;return
		endif !if(isi.eq.j_o(iout)%i(3))  10199
 
 
 
	else !if(j_otype(j_o(iob)%i(io+2)).eq.j_ipreal)then
		!	write(6,*)'copy',j_o(iob)%i(io+2),j_o(iob)%i(io+3)
		call j_copy2(irg,iout)
 
	endif !if(j_otype(irg).eq.j_ipreal)  10163
 
end subroutine !subroutine assone(iob,io)
 
subroutine assign(iob,io)
	integer, dimension(:), pointer :: argin,argout !arguments of the funct
	double precision::valu
	integer*8 ::nrow,ncol,ibas,i,iel,narin,j
	logical p
	narin=j_o(iob)%i(io+1)
	narout=j_o(iob)%i(io+2)
	argin=>j_o(iob)%i(io+3:io+2+narin)
	argout=>j_o(iob)%i(io+3+narin:io+2+narin+narout)
	ion=io+narin+narout+3
	p=j_v(j_ivdollar).eq.948
	if(p)then
		write(6,'(27i5)')'assign',narin,narout,argin,argout ,io,ion
		if(j_v(j_ivdollar).eq.948)write(6,'(27i5)')j_o(iob)%i(1:ion+20)
		if(j_v(j_ivdollar).eq.948)write(6,'(27i5)')(j,j=1,ion+20)
	endif !if(p)  10229
	io=io+narin+narout+3
 
	if(narin.eq.1.and.narout.gt.1)then
		!
		if(j_otype(argin(1)).ne.j_ipreal)then
			write(6,*)'only REAL objects can be duplicated'
			j_err=.true.;return
 
		endif !if(j_otype(argin(1)).ne.j_ipreal)  10238
		do i=1,narout
			!	iv=j_o(iob)%i(io+narin+2+i)
			if(j_otype(argout(i)).ne.j_ipreal)call j_del(argout(i))
			j_v(argout(i))=j_v(argin(1))
		enddo !i=1,narout  10243
		return
		!write(6,*)'22assas',io
	elseif(narout.eq.1)then !if(narin.eq.1.and.narout.gt.1)then
 
 
		if(j_otype(argout(1)).eq.j_ipmatrix.and.j_otype(argin(1)).eq.j_ipreal)then
 
			nrow=j_nrows(argout(1))  !j_o(argout(1))%i(1)
			ncol=j_ncols(argout(1))  !j_o(argout(1))%i(2)
			!		j_o(ivout)%i(4)=itype
			ibas=0
 
			if(j_o(argout(1))%i(4).eq.j_matdiag)then
 
 
				do i=1,nrow
					if(i.le.narin)valu=j_v(argin(i))
					j_o(argout(1))%d(ibas+i)=valu
					ibas=ibas+nrow
 
				enddo !i=1,nrow  10263
 
			else !if(j_o(argout(1))%i(4).eq.j_matdiag)then
				iel=0
				!			write(6,*)'nrow',nrow,ncol,narin
				do i=1,nrow
					do j=1,ncol
						iel=iel+1
						if(iel.le.narin)valu=j_v(argin(iel))
						j_o(argout(1))%d(iel)=valu
					enddo !j=1,ncol  10274
				enddo !i=1,nrow  10273
 
			endif !if(j_o(argout(1))%i(4).eq.j_matdiag)  10260
			return
		endif !if(j_otype(argout(1)).eq.j_ipmatrix.and.j_otype(argin(1)).  10253
	endif !if(narin.eq.1.and.narout.gt.1)  10236
 
	if(narin.eq.narout)	then
		do i=1,narin
			if(j_otype(argin(i)).eq.j_ipdata)then
				call j_printname('input ',argin(i),' is DATA and it cannot be copied')
				j_err=.true.;return
 
			endif !if(j_otype(argin(i)).eq.j_ipdata)  10288
			if(j_v(j_ivdebug).ge.2)write(6,*)'making copy of ',argin(i), ' to ',argout(i),j_otype(argin(i))
			call j_copy2(argin(i),argout(i))
		enddo !i=1,narin  10287
		!	io=io+2*narin+3
		!	write(6,*)'22tas  ',io
		return
	endif !if(narin.eq.narout)  10286
	write(6,*)'illegal assignment having ',narout,' outputs and ',narin,' inputs'
	!	write(6,'(20i5/)')j_o(iob)%i(1: j_o(iob)%i(0))
	j_err=.true.
	return
 
end subroutine !subroutine assign(iob,io)
 
 
 
subroutine properties(iob,io)
 
	! Section properties properties() Properties of subjects
	! This function has been used to define properties of factories.
	! It will replaced with othe means in later versions.
	! endsection
 
 
	character*24 name1
	integer,dimension(:),allocatable::linevar
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	allocate(linevar(1:narg))
	inprint=0
	if(j_v(j_ivprintinput).ge.3.)inprint=2
	iprin=j_igetopt(iob,io,j_mprint)
	if(iprin.eq.0)then
		inprint=1
	elseif(iprin.gt.0)then !if(iprin.eq.0)then
		inprint=j_v(iprin)
	endif !if(iprin.eq.0)  10327
	if(j_v(j_ivprintinput).ge.3)inprint=1
	!!call j_clearoption(iob,io)  ! subroutine
	j_inpara=.true.
	1 call j_getinput('prop>',inprint)
	if(j_err.or.j_stop)return
	if(j_inp(1:1).eq.'/')then
		j_inpara=.false.
		goto 90
	endif !if(j_inp(1:1).eq.'/')  10337
	iip=index(j_inp(1:j_linp),',')
	iip2=j_lentrim(j_inp(1:iip-1))
	if(iip2.le.0)then
		write(6,*)'format of properties: name, .. (values)'
		goto 99
	end if !if(iip2.le.0)  10343
	do i=1,narg
		call j_getline(j_ivnames,j_o(iob)%i(io+1+i),name1,le)
		if(j_err)return
		linevar(i)=j_getobject(0,j_inp(1:iip2)//'%'//name1(1:le),j_ipreal)
		if(j_err) goto 99
	end do !i=1,narg  10347
	read(j_inp(iip+1:j_linp),*,err=99,end=99)(j_v(linevar(j)),j=1,narg)
	if(inprint.gt.0)then
		do j=1,narg
			call j_getname(linevar(j))
			write(6,*)j_oname(1:j_loname),j_v(linevar(j))
		enddo !j=1,narg  10355
		write(6,*)' '
		!	write(6,*)j_inp(1:iip),(j_v(linevar(j)),j=1,narg)
	endif !if(inprint.gt.0)  10354
	goto 1
	90 continue !io=io+narg+3
	deallocate(linevar)
	return
	99 write(6,*)'error in properties, reading line:'
	write(6,*)j_inp(1:j_linp)
	j_err=.true.
	deallocate(linevar)
	return
end subroutine properties !subroutine properties(iob,io)
 
subroutine datawcase(iob,io)
	write(6,*)'datawcase is deleted, use case-> option in data()'
	j_err=.true.
	return
 
 
end subroutine
! integer,intent(in)::iob
! integer,intent(in)::io
! logical maketran
!Section datawcase Deleted function for data with cases
! datawcase is a deleted function. Use data() with case->option to get names for cases.
!endheader
!endsection
 
 
 
 
subroutine classify(iob,io)
	!Section classify classify() Group means, variances and standard deviations
	! Classifies data with respect to one or two variables, get class
	!  frequencies,
	!   means and standard deviations of
	! argument variables.
	!endheader
	!Option
	! Output& 1&Matrix&
	! A matrix containing class information (details given below)
	! Args&-1-&REAL&
	! Variables for which class means are computed. If there are no arguments, code option
	! func-> must be present
	!func&-1|1&CODE& code telling how the variable to be classified is computed.
	!xfunc& N | 1 & CODE &  Code option telling how the x variable is computed from the data variables.
	!@@data@
	! x &1&REAL&The first variable defining classes.
	! minobs&-1|1&REAL& minimum number of observation in a class, obtained by merging classes. Does
	! not work if z-> is given
 
	! xrange& -1|0|2&Real& Defines the range of x variable. If xrange-> is given without
	! arguments and Jlp22 variables x%min and x%max exist, they are used, and
	! if they do not exist an error occurs. Note that these variables can be
	! generate with stat(min->,max->). Either xtrange-> or any-> must be presente.
	!any&-1|0& &Indicates that each value of the x-variables foms a separate class.
	!  either xrange-> or nay-> must be present.
	!tailtofirst&-1|0&& If the x-variable is less than the lower xrange, the observation is put to the first class
	!tailtolast&-1|0&& If the x-variable is greater than the upper xrange, the observation is put to the first class
	! classes&-1|1&Real& Number of classes, If dx-> is not given, the default is that range is
	! divided into 7 classes.
	! minobs-> minimum number of observations in one class. Classes are merged so that this can
	! be obtained. Does not work if z-> is present.
	!
	! z&-1|1&REAL& The second variable (z variable) defining classes in two dimensional classification.
	! zrange &-1|0|2&Real& Defines the range and class width for a continuous z
	! variable. If Jlp22 variables x%min and x%max exist,
	!  provided by stat(min->,max->), they are used.
	! dz &-1|1&Real& Defines the class width for a continuous z variable.
	! mean if z variable is given, class means are stored in a matrix given in the mean->
	! option
	! classes number of classes, has effect if dx is not defined in xrangedx->. The default is
	! classes->7. If z is given then, there can be a second argument, which gives the
	! number of classes for z, the default being 7.
	!@@trans@
	!@@filter@
	!@@reject@
 
	! print&-1|1&Real& By setting print->0, the classification matrix is not printed.
	!  The matrix can be utilized directly in drawclass() function.
	!endoption
	!Note If z variable is not given then first column in printed output and the first row in the output
	! matrix (if given) contains class means of the x variable. In the output matrix the last element is
	! zero. Second column an TARKASTA VOISIKO VAIHTAArow shows number of observations in
	! class, and the last element is the total number of observations. Third row shows the class means
	! of the argument variable. The fourth row in the output matrix shows the class standard
	! deviations, and the last element is the overall standard deviation
	!endnote
	!Note Variable Accepted gets the number of accepted obsevations.
	!endnote
	!Note Note the difference of xrange-> in classify() and draw-> when xfunc-> is present.
	!In draw->, x-> and xrange-> refer to the background variable which is stepped in xrange->
	!and the x-variable in figure is then computed using xfunc->. In classify() xrange-> and
	!xfunc refer to the same variable.
	!endnote
	!endsection
 
	double precision,dimension(:), allocatable::value9,value2
	integer, dimension(:),allocatable :: iperm  !for sorting
	integer, dimension (:,:),allocatable:: freqtot
	double precision ,dimension(:,:), allocatable::xs
	double precision ,dimension(:), allocatable::xsx,xssx,sumwtx  ! for class means of x var
	double precision ,dimension(:,:), allocatable::xss
	double precision ,dimension(:,:), allocatable::sumwt
	!	integer, dimension (:),allocatable:: prevc,nextc
	double precision dinobs,dapu,pros,cumpros,xvalue
	double precision dx,dz,xmin,xmax,zmin,zmax,yvalue,xw,bias
	integer :: ix_
	logical xrange,zrange,isfunc,isy,print0,istailtofirst,istailtolast
	logical::p
	integer*8::nval,nval2,i,j,nrow_
	logical isxfunc
	!integer*8::iobs
	p=j_v(j_ivdebug).gt.0
	call j_checkoutput(iob,io)
	if(j_err)return
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	iy=0
	call j_getdataobject(iob,io)
	print0=j_dprint.gt.0
 
	linkfunc=j_codelink(iob,io,j_mfunc); if(j_err)return
	isfunc=linkfunc.ne.0
	if(j_err)return
 
	linkxfunc=j_codelink(iob,io,j_mxfunc); if(j_err)return
	isxfunc=linkxfunc.gt.0
 
	jnobstot=j_dnobs
	isy=isfunc
	if(narg.gt.0)then
		iy=j_o(iob)%i(io+2)
		isy=.true.
		if(print0)call j_printname('y-variable: ',iy,' ')
		if(isfunc)then
			write(6,*)'cannot have y-variable and func-> simulataneously'
			j_err=.true.;return
 
		endif !if(isfunc)  10497
	endif !if(narg.gt.0)  10493
 
	ix_=j_igetopt(iob,io,j_mx)
	if(ix_.le.0.and..not.isxfunc)then
		write(6,*)'**no x-> or xfunc-> given in classify';j_err=.true.;return
	end if !if(ix_.le.0.and..not.isxfunc)  10505
	iz=j_igetopt(iob,io,j_mz)
	iout=j_o(iob)%i(io+2+narg)
	!iprint=2
	!	write(6,*)'<634646',j_linkoption(iob,io,j_mxrange),j_linkoption(iob,io,j_many)
	if(j_linkoption(iob,io,j_mxrange).lt.0.and.j_linkoption(iob,io,j_many).lt.0)then
		write(6,*)'either any-> or xrange-> must be present'
		j_err=.true.;return
	endif !if(j_linkoption(iob,io,j_mxrange).lt.0.and.j_linkoption(io  10512
	!	if(j_linkoption(iob,io,j_mprint).gt.0)iprint=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mprint)+1) )
	!call j_getdatasets(iob)
	!write(6,*)'7NOBSTOTO',jnobstot
	!nr=j_nargopt(iob,j_mxrange)
	call j_getoption(iob,io,j_mxrange,-1,2,j_ipreal,.false.,nr,j_optarg0);if(j_err)return
	xrange=.true.
	if(nr.lt.0)then
		if(j_linkoption(iob,io,j_mdx).gt.0)then
			write(6,*)'dx-> requires xrange->'
			j_err=.true. ;return
 
 
		endif !if(j_linkoption(iob,io,j_mdx).gt.0)  10523
		if(j_linkoption(iob,io,j_mclasses).ge.0)then
			write(6,*)'classes-> requires xrange->'
			j_err=.true.;return
 
		endif !if(j_linkoption(iob,io,j_mclasses).ge.0)  10529
		xrange=.false.
		if(allocated(value9))deallocate(value9)
		allocate(value9(1:jnobstot))
		if(allocated(iperm))deallocate(iperm)
		allocate(iperm(1:jnobstot))
	elseif(nr.eq.0.and..not.isxfunc)then !if(nr.lt.0)then
		ii1=j_object3(ix_,'%min')
		ii2=j_object3(ix_,'%max')
		!	write(6,*)'ii1,ii2',ii1,ii2
		if(ii1.le.0.or.ii2.le.0)then
			write(6,*)'...%min  or ...%max does not exist'
			j_err=.true. ;return
 
		endif !if(ii1.le.0.or.ii2.le.0)  10543
		xmin=j_v(ii1)
		xmax=j_v(ii2)
		write(6,*)'using xrange->(',j_chr10(xmin),',',j_chr10(xmax),') given by ',&
			j_vname(ii1),' and ',j_vname(ii2)
	elseif(nr.eq.2)then !if(nr.lt.0)then
 
		xmin=j_v(j_optarg0(1))
		xmax=j_v(j_optarg0(2))
 
	else !if(nr.lt.0)then
		write(6,*)'xrange-> must have zero or 2 arguments'
		j_err=.true. ;return
 
 
	endif !if(nr.lt.0)  10522
 
 
	if(xrange)then
		if(xmax.le.xmin)then
			write(6,*)'**classify: illegal xrange :',xmin,xmax
			j_err=.true. ;return
 
		endif !if(xmax.le.xmin)  10566
		li=j_linkoption(iob,io,j_mclasses)
		nargoc=0
		if(li.gt.0)nargoc=j_o(iob)%i(li)
		if(nargoc.gt.0)then
			clasn=j_v(j_o(iob)%i( li+1))
			if(clasn.lt.2)then
				write(6,*)'classes-> must be at least 2, it was ',clasn
				j_err=.true.;return
			endif !if(clasn.lt.2)  10576
		else !if(nargoc.gt.0)then
			clasn=7.
		endif !if(nargoc.gt.0)  10574
		if(nargoc.ge.2)then
			clasz=j_v(j_o(iob)%i( li+2))
		else !if(nargoc.ge.2)then
			clasz=7.
		endif !if(nargoc.ge.2)  10583
 
		!ivcl=j_igetopt(iob,io,j_mclasses)
 
		call	j_getoption(iob,io,j_mdx,-1,1,j_ipreal,.true.,nargopt,j_optarg0);if(j_err)return
		if(j_err)return
		!ivdx=j_igetopt(iob,io,j_mdx)
		if(nargopt.gt.0)then
			dx=j_v(j_optarg0(1))
			nval=(xmax-xmin)/dx
			if(nargoc.ge.1)write(6,*)' dx defined by dx-> and classes->, dx-> used:',dx
		else !if(nargopt.gt.0)then
			!	if(ivcl.gt.0)then
			!		clasn=j_v(ivcl)
			!	else !if(ivcl.gt.0)then
			!		clasn=7
			!	endif !if(ivcl.gt.0)then
			dx=1.000001*(xmax-xmin)/clasn
			if(print0) write(6,*)'dx:',dx,' classes =',clasn
			nval=clasn
		end if !if(nargopt.gt.0)  10594
	end if !if(xrange)  10565
	nval2=0
	call	j_getoption(iob,io,j_mzrange,-1,1,j_ipreal,.true.,nargopt,j_optarg0);if(j_err)return
	if(iz.gt.0)then
		if(nargopt.lt.2)then
			zrange=.false.
			allocate(value2(1:jnobstot))
			if(allocated(iperm))then
				if(size(iperm).lt.jnobstot)then
					deallocate(iperm)
					allocate(iperm(1:jnobstot))
				endif !if(size(iperm).lt.jnobstot)  10616
			else !if(allocated(iperm))then
				allocate(iperm(1:jnobstot))
			endif !if(allocated(iperm))  10615
		else !if(nargopt.lt.2)then
			zrange=.true.
			zmin=j_v(j_optarg0(1))
			zmax=j_v(j_optarg0(2))
			call	j_getoption(iob,io,j_mdz,-1,1,j_ipreal,.true.,nargopt,j_optarg0);if(j_err)return
			if(nargopt.ge.1)then
				dz=j_v(j_optarg0(1))
				nval2=(zmax-zmin)/dz
			else !if(nargopt.ge.1)then
				dz=1.00001*(zmax-zmin)/clasz
				if(print0)write(6,*)'dz:',dz,' clasz =',clasz
				nval2=clasz
			end if !if(nargopt.ge.1)  10628
 
		end if !if(nargopt.lt.2)  10612
	endif !if(iz.gt.0)  10611
	call	j_getoption(iob,io,j_mmean,-1,1,j_ipreal,.true.,igmean,j_optarg0);if(j_err)return
	if(igmean.gt.0)igmean=j_optarg0(1)
	!	igmean= j_igetopt(iob,io,j_mmean)
	minobs=j_igetopt(iob,io,j_mminobs)
	if(minobs.gt.0)then
		minobs=j_v(minobs)
		dinobs=minobs ! v(minobs)
		if(iz.gt.0)then
			write(6,*)'classify: minobs-> does not work with z-> (yet)'
		endif !if(iz.gt.0)  10646
	endif !if(minobs.gt.0)  10643
	! ivnobs=j_igetoptout(iob,j_mnobs)
 
	! if(ivnobs.eq.0) then
	! call j_getobject(0,'Nobs',j_ipreal,ivnobs)
	! !	if(j_err) return
	! endif !if(ivnobs.eq.0) then
	istailtofirst=j_isoption(iob,io,j_mtailtofirst)
	istailtolast=j_isoption(iob,io,j_mtailtolast)
	!!call j_clearoption(iob,io)  ! subroutine
	if(.not.xrange.and.iz.gt.0.and..not.zrange)then
		call j_getvalues(ix_,value9,nval,iz,value2,nval2)
	elseif(.not.xrange.and.(iz.le.0.or.zrange))then !if(.not.xrange.and.iz.gt.0.and..not.zrange)then
		call j_getvalues(ix_,value9,nval,0,value9,nval2)
	elseif(xrange.and.(iz.gt.0.and..not.zrange))then !if(.not.xrange.and.iz.gt.0.and..not.zrange)then
		call j_getvalues(iz,value2,nval2,0,value2,nval)
	endif !if(.not.xrange.and.iz.gt.0.and..not.zrange)  10659
	!	write(6,*)'values ',value9(1:nval)
	if(j_err)return
	if(.not.xrange)call j_quick_sort(value9(1:nval),iperm)
	!	write(6,*)'values ',value9(1:nval)
	if(iz.gt.0.and..not.zrange)call j_quick_sort(value2(1:nval2),iperm)
	j2=0
	allocate(freqtot(0:nval2,0:nval))
	if(isy)then
		allocate(xs(0:nval2,0:nval),xss(0:nval2,0:nval),sumwt(0:nval2,0:nval))
		xs=j_0;xss=j_0;sumwt=j_0
	end if !if(isy)  10673
	xw=j_1
	allocate(xsx(0:nval),xssx(0:nval),sumwtx(0:nval))
	xsx=j_0;xssx=j_0;sumwtx=j_0
	freqtot=0
	nobst=0
	missing=0
	!do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
	nlow=0
	nup=0
	do j_iobs=j_dfrom,j_duntil
		call j_getobs(j_iobs)
		if(j_err)return
 
		if(j_rejected)cycle
		if(isxfunc)then
			xvalue=j_codevalue(iob,linkxfunc)
			if(j_err)return
		else
			xvalue=j_v(ix_)
		endif !if(isxfunc)  10692
		if(abs(xvalue).ge.1.7d18)then
			missing=missing+1
			cycle
		endif !if(abs(xvalue).ge.1.7d18)  10698
		if(iz.gt.0)then
			if(abs(j_v(iz)).ge.1.7d18)then
				missing=missing+1
				cycle
			endif !if(abs(j_v(iz)).ge.1.7d18)  10703
 
		endif !if(iz.gt.0)  10702
		if(iy.gt.0)then
			if(abs(j_v(iy)).ge.1.7d18)then
				missing=missing+1
				cycle
			endif !if(abs(j_v(iy)).ge.1.7d18)  10710
		endif !if(iy.gt.0)  10709
 
		j2=0
		if(xrange)then
 
			if(.not.istailtofirst.and.xvalue.lt.xmin-0.000000001d0)then
				nlow=nlow+1
				cycle
 
			endif !if(.not.istailtofirst.and.xvalue.lt.xmin-0.000000001d0)  10719
			if(.not.istailtolast.and.xvalue.gt.xmax+0.000000001d0)then
				nup=nup+1
				cycle
			endif !if(.not.istailtolast.and.xvalue.gt.xmax+0.000000001d0)  10724
			j=(xvalue-xmin)/dx+1
			j=min(j,nval)
			j=max(j,1)
			!if(j.lt.0)cycle
 
 
		else !if(xrange)then
			do j=1,nval
				if(abs(value9(j)-xvalue).lt.1.d-15)exit
			enddo !j=1,nval  10735
 
		endif !if(xrange)  10717
		if(iz.gt.0)then
			if(zrange)then
				if(xvalue.lt.zmin-0.000000001d0)cycle
				if(xvalue.gt.zmax+0.000000001d0)cycle
				j2=(j_v(iz)-zmin)/dz+1
				j2=min(j2,nval2)
				j2=max(1,j2)
 
			else !if(zrange)then
				do j2=1,nval2
					if(abs(value2(j2)-j_v(iz)).lt.1.d-15)exit
				enddo !j2=1,nval2  10749
 
			end if !if(zrange)  10741
		end if !if(iz.gt.0)  10740
		call j_msd21(xvalue,xsx(j),xssx(j),xw,sumwtx(j))
		call j_msd21(xvalue,xsx(0),xssx(0),xw,sumwtx(0))
		nobst=nobst+1
		freqtot(j2,j)=freqtot(j2,j)+1
		if(isy)then
			if(isfunc)then
				yvalue=j_codevalue(iob,linkfunc)
				if(j_err)return
			else !if(isfunc)then
				yvalue=j_v(iy)
			endif !if(isfunc)  10760
			call j_msd21(yvalue,xs(j2,j),xss(j2,j),xw,sumwt(j2,j))
			call j_msd21(yvalue,xs(0,0),xss(0,0),xw,sumwt(0,0))
 
		end if !if(isy)  10759
		if(j2.gt.0)then
			freqtot(0,j)=freqtot(0,j)+1
			freqtot(j2,0)=freqtot(j2,0)+1
 
			if(isy)then
				!	if(i.lt.20)write(6,*)xvalue,j_v(iz),j,j2,j_v(iy)
 
				call j_msd21(yvalue,xs(j2,j),xss(j2,j),xw,sumwt(j2,j))
				call j_msd21(yvalue,xs(0,j),xss(0,j),xw,sumwt(0,j))
				call j_msd21(yvalue,xs(j2,0),xss(j2,0),xw,sumwt(j2,0))
			end if !if(isy)  10774
 
		endif !if(j2.gt.0)  10770
	enddo !j_iobs=j_dfrom,j_duntil  10687
	!	enddo !do k=1,jndatasetss
	!if(ivnobs.gt.0)j_v(ivnobs)=nobst
	write(6,*)'Accepted ',nobst, ' out of ',j_dnobs
	if(nlow.gt.0)write(6,*)nlow, ' x-values smaller than ',xmin
	if(nup.gt.0)write(6,*)nup, ' x-values grater than ',xmax
	j_v(j_ivaccepted)=nobst
	if(nobst.le.0)then
		write(6,*)'Nothing accepted'
		return
 
	endif !if(nobst.le.0)  10790
	if(minobs.gt.0)then
		! if(nobst.lt.minobs)then
		!write(6,*)'*classify: minobs',minobs,' > number of accpeted obs ',nobst
		! if(ivnobs.gt.0)return
		!write(6,*)'if nobs-> is given, then return without error'
		! j_err=.true.
 
		! return
		! endif !if(nobst.lt.minobs)then
 
		!allocate(nextc(0:nval),prevc(0:nval))
		!do i=0,nval
		!	nextc(i)=i+1
		!	prevc(i)=i-1
		!	enddo !i=0,nval  10924
		!	nextc(nval)=0
		!	prevc(0)=nval
		ifim=0
		i0=1
		do i2=2,nval
			!	if(sumwtx(i2).lt.dinobs.and.sumwtx(i2).gt.0)then
			if(sumwtx(i0).lt.dinobs.and.sumwtx(i2).gt.0)then 			!merge into prev
				!		write(6,*)'* sumwtx(i0)sumwtx(i2).lt.dinobs',i0,i2,sumwtx(i0),sumwtx(i2),sumwt(0,i0),sumwt(0,i2)
 
				dapu=(sumwt(0,i0)*xs(0,i0)+sumwt(0,i2)*xs(0,i2))/(sumwt(0,i0)+sumwt(0,i2))
 
				xss(0,i0)=xss(0,i0)+xss(0,i2)+sumwt(0,i0)*xs(0,i0)**2+sumwt(0,i2)*xs(0,i2)**2 &
					-(sumwt(0,i0)+sumwt(0,i2))*dapu*dapu
				xs(0,i0)=dapu
				sumwt(0,i0)=sumwt(0,i0)+sumwt(0,i2)
 
				xsx(i0)=(sumwtx(i0)*xsx(i0)+sumwtx(i2)*xsx(i2))/(sumwtx(i0)+sumwtx(i2))
				sumwtx(i0)=sumwtx(i0)+sumwtx(i2)
 
				freqtot(0,i0)=sumwtx(i0)
				!			write(6,*)'aft umwtx(i0)sumwtx(i2).lt.dinobs',i0,i2,sumwtx(i0),sumwtx(i2),sumwt(0,i0),sumwt(0,i2)
 
			elseif(sumwtx(i2).gt.0)then
				i0=i0+1
				if(i0.lt.i2)then
					if(ifim.eq.0)ifim=i0
 
					sumwtx(i0)=sumwtx(i2)
					xss(0,i0)=xss(0,i2)
					xs(0,i0)=xs(0,i2)
 
					sumwt(0,i0)=sumwt(0,i2)
					xsx(i0)=xsx(i2)
					freqtot(0,i0)=freqtot(0,i2)
					!		write(6,*)'i0,i2',i0,i2,sumwtx(i0),xss(0,i0),xs(0,i0),sumwt(0,i0),xsx(i0),freqtot(0,i0)
				endif !if(i0.lt.i2)  10834
			endif !if(sumwtx(i0).lt.dinobs.and.sumwtx(i2).gt.0)  10816
 
			!		endif
		enddo !i2=2,nval  10814
		if(sumwtx(i0).lt.dinobs)then   !last group not filled
			!	write(6,*)'i0here',i0,sumwtx(i0),sumwt(0,i0)
			i2=i0
			i0=i0-1
 
			dapu=(sumwt(0,i0)*xs(0,i0)+sumwt(0,i2)*xs(0,i2))/(sumwt(0,i0)+sumwt(0,i2))
 
			xss(0,i0)=xss(0,i0)+xss(0,i2)+sumwt(0,i0)*xs(0,i0)**2+sumwt(0,i2)*xs(0,i2)**2 &
				-(sumwt(0,i0)+sumwt(0,i2))*dapu*dapu
			xs(0,i0)=dapu
			sumwt(0,i0)=sumwt(0,i0)+sumwt(0,i2)
 
			xsx(i0)=(sumwtx(i0)*xsx(i0)+sumwtx(i2)*xsx(i2))/(sumwtx(i0)+sumwtx(i2))
			sumwtx(i0)=sumwtx(i0)+sumwtx(i2)
 
			freqtot(0,i0)=sumwtx(i0)
 
 
 
		endif !if(sumwtx(i0).lt.dinobs)  10850
		if(i0.lt.nval)then
			xsx(i0+1:nval)=j_0;xssx(i0+1:nval)=j_0;sumwtx(i0+1:nval)=j_0
 
			write(6,*)nval-i0,' classes merged due to minobs->'
			nval=i0
		endif !if(i0.lt.nval)  10870
 
		!deallocate (nextc,prevc)
	endif !if(minobs.gt.0)  10795
	if(iout.ne.j_ivresult)then
		if(iz.gt.0)then
			ivmat=j_defmatrix(iout,' ',nval2+2_8,nval+2_8,j_matclass)
			if(xrange)then
				j_o(ivmat)%d((nval2+2_8)*(nval+2_8)+j_18)=xmin
				j_o(ivmat)%d((nval2+2_8)*(nval+2)+2_8)=dx
				do i=1,nval
					j_o(ivmat)%d(i+1)=xmin+(i-0.5)*dx
					!	call j_putmatrix(ivmat,1,i+1,xmin+(i-0.5)*dx)
				end do !i=1,nval  10885
			else !if(xrange)then
				j_o(ivmat)%d(2:nval+1)=value9(1:nval)
			end if !if(xrange)  10882
			if(zrange)then
				j_o(ivmat)%d((nval2+2)*(nval+2)+3)=zmin
				j_o(ivmat)%d((nval2+2)*(nval+2)+4)=dz
				do i=1,nval2
					j_o(ivmat)%d(i+1)=zmin+(i-0.5)*dz
					!	call j_putmatrix(ivmat,i+1,1,zmin+(i-0.5)*dz)
				end do !i=1,nval2  10895
			else !if(zrange)then
				do i=1,nval2
					j_o(ivmat)%d(i+1)=value2(i)
					!	call j_putmatrix(ivmat,i+1,1,value2(i))
				end do !i=1,nval2  10900
			end if !if(zrange)  10892
			do i=1,nval
				do j=1,nval2
					j_dapu=freqtot(j,i)
					call j_putmatrix8(ivmat,j+j_18,i+j_18,j_dapu)
				end do !j=1,nval2  10906
				j_dapu=freqtot(0,i)
				call j_putmatrix8(ivmat,nval2+2_8,i+j_18,j_dapu)
			enddo !i=1,nval  10905
			do j=1,nval2
				j_dapu=freqtot(j,0)
				call j_putmatrix8(ivmat,j+j_18,nval+2_8,j_dapu)
			enddo !j=1,nval2  10913
			j_dapu=nobst
			call j_putmatrix8(ivmat,nval2+2_8,nval+2_8,j_dapu)
			if(igmean.gt.0)then
				ivmat=j_defmatrix(igmean,' ',nval2+2_8,nval+2_8,j_matclass)
				if(xrange)then
					do i=1,nval
						j_o(ivmat)%d(i+1)=xmin+(i-0.5)*dx
						!			call j_putmatrix8(ivmat,1,i+1,xmin+(i-0.5)*dx)
					end do !i=1,nval  10922
				else !if(xrange)then
					j_o(ivmat)%d(2:nval+1)=value9(1:nval)
				end if !if(xrange)  10921
				if(zrange)then
					do i=1,nval2
						call j_putmatrix8(ivmat,i+j_18,j_18,zmin+(i-0.5)*dz)
					end do !i=1,nval2  10930
				else !if(zrange)then
					do i=1,nval2
						call j_putmatrix8(ivmat,i+j_18,j_18,value2(i))
					end do !i=1,nval2  10934
				end if !if(zrange)  10929
				do i=1,nval
					do j=1,nval2
						call j_putmatrix8(ivmat,j+j_18,i+j_18,xs(j,i))
					end do !j=1,nval2  10939
					call j_putmatrix8(ivmat,nval2+2_8,i+j_18,xs(0,i))
				enddo !i=1,nval  10938
				do j=1,nval2
					call j_putmatrix8(ivmat,j+j_18,nval+2_8,xs(j,0))
				enddo !j=1,nval2  10944
				call j_putmatrix8(ivmat,nval2+2_8,nval+2_8,xs(0,0))
			end if !if(igmean.gt.0)  10919
		else !if(iz.gt.0)then
			nrow_=2
			if(isy)nrow_=4
			ivmat=j_defmatrix(iout,' ',nrow_,nval+j_18,j_matclass)
			if(p)write(6,*)'allocating matrix ',nrow_,nval+1
			nval1=nval+1
			if(xrange)then
				! extra positions
				j_o(ivmat)%d(nrow_*(nval1)+1)=xmin
				j_o(ivmat)%d(nrow_*(nval1)+2)=dx
				do i=1,nval
					j_o(ivmat)%d(i)=xsx(i)  ! xmin+(i-0.5)*dx
				end do !i=1,nval  10959
				j_o(ivmat)%d(nval+1)=xsx(0)
			else !if(xrange)then
				j_o(ivmat)%d(1:nval)=value9(1:nval)
			end if !if(xrange)  10955
			j_o(ivmat)%d(nval+2:2*nval+1)=freqtot(0,1:nval)
			j_o(ivmat)%d(2*nval+2)=nobst
			if(isy)then
				j_o(ivmat)%d(2*(nval1)+1:3*(nval1)-1)=xs(0,1:nval)
				j_o(ivmat)%d(3*(nval1))=xs(0,0)
				do j=0,nval
					if(sumwt(0,j).gt.j_1.and.xss(0,j).gt.j_0)then
						haj=sqrt(xss(0,j)/(sumwt(0,j)-j_n1))
					else !if(sumwt(0,j).gt.1.d0.and.xss(0,j).gt.j_0)then
						haj=j_0
					end if !if(sumwt(0,j).gt.j_1.and.xss(0,j).gt.j_0)  10972
					j2=j
					if(j.eq.0)j2=nval1
					j_o(ivmat)%d(3*(nval1)+j2)=haj
				end do !j=0,nval  10971
				j_dapu2=j_0
				ivregf=j_object('Cregf')
				ivresid=j_object('Cresid')
				iregf=0
				if(ivregf.gt.0)iregf=j_inlistobject(ivregf,j_divkeep)
				iresid=0
				if(ivresid.gt.0)iresid=j_inlistobject(ivresid,j_divkeep)
				bias=j_0
				ibas=-j_dnkeep+(j_dfrom-1)*j_dnkeep
				do j_iobs=j_dfrom,j_duntil
					ibas=ibas+j_dnkeep
					call j_getobs(j_iobs)
					if(j_err)return
					j_dapu=xvalue
					!	if(j_rejected)cycle
					if(abs(j_dapu).ge.1.7d18.or.j_rejected)then
						if(iregf.gt.0)j_o(j_divmat)%d(ibas+iregf)=j_0
						if(iresid.gt.0)j_o(j_divmat)%d(ibas+iresid)=j_0
						cycle
					endif !if(abs(j_dapu).ge.1.7d18.or.j_rejected)  10996
					ico1=2
					do while(j_dapu.lt.j_o(ivmat)%d(ico1).and.ico1.lt.nval)
						ico1=ico1+1
					enddo !while(j_dapu.lt.j_o(ivmat)%d(ico1).and.ico1.lt.nval)  11002
					ico0=ico1-1
 
					yvalue=j_o(ivmat)%d(2*nval1+ico0)+&
						(j_dapu-j_o(ivmat)%d(ico0))*(j_o(ivmat)%d(2*nval1+ico1)-j_o(ivmat)%d(2*nval1+ico0))/&
						(j_o(ivmat)%d(ico1)-j_o(ivmat)%d(ico0))
					if(isfunc)then
						j_dapu3=j_codevalue(iob,linkfunc)
						if(j_err)return
					else
						j_dapu3=j_v(iy)
					endif !if(isfunc)  11010
					j_dapu2=j_dapu2+(j_dapu3-yvalue)*(j_dapu3-yvalue)
 
					bias=bias+j_dapu3-yvalue
					if(iregf.gt.0)j_o(j_divmat)%d(ibas+iregf)=yvalue
					if(iresid.gt.0)j_o(j_divmat)%d(ibas+iresid)=j_dapu3-yvalue
				enddo !j_iobs=j_dfrom,j_duntil  10990
				ivres=j_getobject(ivmat,'%RMSE',j_ipreal)
				j_v(ivres)=sqrt(j_dapu2/nobst)
 
				ivresa=j_getobject(ivmat,'%RMSEadj',j_ipreal)
 
				j_v(ivresa)=sqrt(j_dapu2/max(nobst-nval,1))
				ivbias=j_getobject(ivmat,'%bias',j_ipreal)
				j_v(ivbias)=bias/nobst
				call j_getname(ivbias,ivresa,ivres)
				write(6,*)j_oname(1:j_loname),'=',j_v(ivbias),'  ',j_oname2(1:j_loname2),'=',&
					j_v(ivresa),'  ',j_oname3(1:j_loname3),'=',j_v(ivres)
				write(6,*)' '
 
			end if !if(isy)  10968
		end if !if(iz.gt.0)  10880
		if(isfunc)iy=j_ivresult
		j_o(ivmat)%i2=(/ix_,iz,iy/)
	end if !if(iout.ne.j_ivresult)  10879
 
	if(.not.print0)goto 88
	if(iz.gt.0)then
		call j_printname('x:',ix_,'      z:    ',iz)
		! if(xrange)then
		!write(6,'(8x,9f8.1)')(xmin+(j+0.5)*dx,j=0,nval-1)
		! else !if(xrange)then
		!write(6,'(8x,9f8.1)')value9(1:nval)
		! endif !if(xrange)then
		if(zrange)then
			! do j2=1,nval2
			write(6,'(9x,15f9.3)')(zmin+(j-0.5)*dz,j=1,nval2)  !(freqtot(j2,j),j=1,nval),freqtot(j2,0)
			! if(iy.gt.0)then
		else !if(zrange)then
			write(6,'(9x,15f9.3)') (value2(j2),j2=1,nval2)
 
		endif !if(zrange)  11049
		write(6,*)('_',ij=1,80)
		!write(6,'(8x,9f8.2/)')(xs(j2,j),j=1,nval),xs(j2,0)
		! end if !if(iy.gt.0)then
		! enddo !do j2=1,nval2
 
		if(isy)then
			do j=1,nval
				if(xrange)then
					!	write(6,*)'xsx',xsx(j)
					if(j.lt.ifim)then
						write(6,'(15f9.3)')xmin+(j-1.)*dx,xmin+j*dx,(xs(j2,j),j2=1,nval2),xs(0,j)
					else
						write(6,'(9x,15f9.3)')xsx(j),(xs(j2,j),j2=1,nval2),xs(0,j)
					endif !if(j.lt.ifim)  11066
				else !if(xrange)then
					write(6,'(15f9.3)')value9(j),(xs(j2,j),j2=1,nval2),xs(0,j)
				endif !if(xrange)  11064
				write(6,'(9x,15I9)')(freqtot(j2,j),j2=1,nval2),freqtot(0,j)
				write(6,*)' '
			enddo !j=1,nval  11063
 
		else !if(isy)then
			do j=1,nval
				if(xrange)then
					!	if(xsx(j).ge.xmin+(j-1)*dx.and.xsx(j).le.xmin+j*dx)then
 
					write(6,'(2f9.3,(15i9/)  )')xmin+(j-1)*dx,xmin+j*dx,(freqtot(j2,j),j2=1,nval2)
					!else
 
					!endif
				else !if(xrange)then
					write(6,'(f9.3,(15i9/)  )')value9(j),(freqtot(j2,j),j2=1,nval2),freqtot(0,j)
				endif !if(xrange)  11080
				write(6,*)' '
			enddo !j=1,nval  11079
		endif !if(isy)  11062
 
		write(6,*)('_',j=1,70)
		write(6,'(9x,15f9.3)')(xs(j2,0),j2=1,nval2)
		write(6,'(9x,15i9)')(freqtot(j2,0),j2=1,nval2),nobst
 
 
		!	else !if(zrange)then
		! do j2=1,nval2
		!write(6,'(f8.1,9i8,(8x,9i8/))')value2(j2),(freqtot(j2,j),j=1,nval),freqtot(j2,0)
		! if(iy.gt.0)then
		!write(6,'(8x,9f8.2/)')(xs(j2,j),j=1,nval),xs(j2,0)
		! end if !if(iy.gt.0)then
		! enddo !do j2=1,nval2
		! !	end if !if(zrange)then
 
		!	write(6,'(8x,9i8)')(freqtot(0,j),j=1,nval),nobst
		!if(iy.gt.0)write(6,'(8x,9f8.2)')(xs(j2,0),j=1,nval2),xs(0,0)
	else !if(iz.gt.0)then
		if(isy)then
			if(.not.isxfunc)call j_printname(' ',ix_,'                             n      y-mean       sd y    se of mean')
		else !if(isy)then
			if(.not.isxfunc)call j_printname(' ',ix_,'                       n      %      cum %')
			cumpros=j_0
		endif !if(isy)  11111
		!	write(6,*)'
		! if(nval.le.9)then
		! if(xrange)then
		!write(6,'(10f8.1)')(xsx(j),j=1,nval),xsx(0)
		! else !if(xrange)then
		!write(6,'(10f8.1)')value9(1:nval)
		! endif !if(xrange)then
		!write(6,'(10i8/)')(freqtot(0,j),j=1,nval),nobst
		! if(iy.gt.0)write(6,'(10f8.2/)')(xs(0,j),j=1,nval),xs(0,0)
		! if(iy.gt.0.and.iprint.ge.2)write(6,'(10f8.2/)')( j_o(ivmat)%d(3*(nval+1)+j),j=1,nval+1)
		! else
456	format(f12.5,a4,f12.5,i8,3f12.5)
4561	format(f12.5,i8,3f12.5)
		if(xrange.and.isy)then  !
			if(j_dprint.ge.2)then
				if(p)write(6,*)'ivmat',ivmat
				if(p)write(6,*)'ivmat,d',j_o(ivmat)%d
				do j=1,nval
 
					if(freqtot(0,j).gt.0)then
						sdy=j_o(ivmat)%d(3*(nval+1)+j)
						!	write(6,*)'xsx',xsx(j),xmin+(j-1.)*dx,xsx(j),xmin+j*dx
						if(j.lt.ifim)then
							!			write(6,456)xsx(j),freqtot(0,j),xs(0,j),sdy,sdy/sqrt(real(freqtot(0,j)))
 
							write(6,456)xmin+(j-1.)*dx,' -  ',xmin+j*dx,freqtot(0,j),xs(0,j),sdy,sdy/sqrt(real(freqtot(0,j)))
						else
							write(6,456)xsx(j),' *  ',j_0,freqtot(0,j),xs(0,j),sdy,sdy/sqrt(real(freqtot(0,j)))
 
						endif !if(j.lt.ifim)  11139
						!	write(6,456)xmin+(j+0.5)*dx,freqtot(0,j),xs(0,j),j_o(ivmat)%d(3*(nval+1)+j)
					endif !if(freqtot(0,j).gt.0)  11136
				enddo !j=1,nval  11134
				write(6,*)'---------------'
				write(6,4561)xsx(0),nobst,xs(0,0),j_o(ivmat)%d(3*(nval+1)+nval+1)
			else !if(j_dprint.ge.2)then
				do j=1,nval
					!		write(6,456)xsx(j),freqtot(0,j),xs(0,j),xs(0,0)
					write(6,456)xmin+(j-1.)*dx,' -  ',xmin+j*dx,freqtot(0,j),xs(0,j),xs(0,0)
				enddo !j=1,nval  11153
				write(6,*)'---------------'
				write(6,4561)xsx(0),nobst,xs(0,0)
			endif !if(j_dprint.ge.2)  11131
 
		elseif(xrange)then !if(xrange.and.isy)then
			do j=1,nval
				!		write(6,456)xsx(j),freqtot(0,j)
				pros=100.d0*freqtot(0,j)/nobst
				cumpros=cumpros+pros
				write(6,456)xmin+(j-1.)*dx,'  - ',xmin+j*dx,freqtot(0,j),pros,cumpros
			enddo !j=1,nval  11162
			write(6,*)'---------------'
			write(6,'(30x,i6)')nobst
 
		elseif(isy)then !if(xrange.and.isy)then
 
			! if(iprint.ge.2)then
			! do j=1,nval
			! sdy=j_o(ivmat)%d(3*(nval+1)+j)
			!write(6,456)value9(j),freqtot(0,j),xs(0,j),sdy,sdy/sqrt(real(freqtot(0,j)))
			! enddo
			!write(6,*)'---------------'
			! sdy=j_o(ivmat)%d(3*(nval+1)+nval+1)
			!write(6,456)xsx(0),nobst,xs(0,0),sdy
			! else
			!		write(6,*)'clash9'
			do j=1,nval
				sdy=j_o(ivmat)%d(3*(nval+1)+j)
				write(6,4561)value9(j),freqtot(0,j),xs(0,j),sdy,sdy/sqrt(real(freqtot(0,j)))
			enddo !j=1,nval  11183
			write(6,*)'---------------'
			sdy=j_o(ivmat)%d(3*(nval+1)+nval+1)
			write(6,4561)xsx(0),nobst,xs(0,0),sdy,&
				sdy/sqrt(real(nobst))
			!endif
		else !if(xrange.and.isy)then
			do j=1,nval
				write(6,4561)value9(j),freqtot(0,j)
			enddo !j=1,nval  11193
			write(6,*)'-----------'
			write(6,*)'       ',nobst
 
		endif !if(xrange.and.isy)  11130
		!	endif
	end if !if(iz.gt.0)  11042
88 deallocate(freqtot)
	if(.not.xrange)deallocate(value9)
	if(allocated(value2))deallocate(value2)
	if(isy)deallocate(xs,xss,sumwt)
	if(allocated(xsx))deallocate(xsx,xssx,sumwtx)
	! io=io+narg+3
	write(6,*)' '
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
	return
end subroutine classify !subroutine classify(iob,io)
 
recursive subroutine show(iob,io)
	! Section show show() Plots FIG object
	! An figure stored in a figure object or in gnuplot file can be plotted with show().
	!By default, plotting functions plot (show) FIGs immediately. When the final figure
	!consists of several subfigures, it is convenient to prevent plotting of different
	! phases of the figure development using show->0 in the graphics functions.
	! In developing scripts, it is convenient to switch with show->. This can be done by giving
	! option in form show->showfig, then the ploting can be controlled with REAL showfig. If the
	! argument is FIGURE, the
	! parameters of the figure can be changed in show() using xlabel->, ylabel->,title->,
	!xrange-> and yrange->. If show() has option show->0,
	! it is possible to change parameters and not yet plot the figure. This is useful
	! e.g. when using multiplot of gnuplot to combine several figures.
	!Jlp22 sends figures to gnupolt using text files.
	!These files can be edited and then sent to gnuplot using show(). As quite many gnuplot properties
	! are implemented in Jlp22, it is not necessary to edit the files often.
	!endheader
	! Option
	!Output&N|1&  &If plot with several subfigures is made, the name of the file of gnuplot commands
	! is the name of output extended with '.jfig'.
	! Args & 1-70& FIGURE | CHAR & With one argumnet, it can be a figure object or the name of the file containg gnuplot commands.
	!With several arguments a multiplot consisting os subplots is made.
	!@@figure
	!multiplot&0|1&CHAR& gnuplot settings defining the multiplot. Several gnuplot commands can be given by
	!separting them with semicolon.
	!xrange&0-&REAL& gives the margins of the subplots, in form left margin,
	! right margin. Value -1 indicates that gnuplot uses its default. Defaults are alo used if
	!there are less than 2*number of subplots  -values.
	!yrange &0- &REAL& Bottom and upper margins.
	! endoption
	! Note You may wish to use show also if you cnange the window size by giving new value for
	! Window CHAR variable. Window and Terminal are not part of figure objects. Their values
	! are looked when showing figures.
	! endnote
	!Note Example of multiplots are given shortly.
	!endnote
	! Ex showex Example of show()
	! fi=draw(func->sqrt2(x),x->x,xrange->(-50,50),continue->fcont)
	! show(fi,xrange->(-60,60),xlabel->'NEWX]Times Roman,16[',ylabel->'NEWY]Courier,10[',continue->fcont)
	! show(fi,axes->10,continue->fcont)
	! show(fi,axes->01,continue->fcont)
	! show(fi,axes->00,continue->fcont)
	! Window='400,800'
	! show(fi,continue->fcont)
	! Window='700,700'
	! fi=drawline(1,10,3,1,color->Red,continue->fcont)
	!*** The line is not visible
	! show(fi,xrange->(1.1,11),continue->fcont)
	! dat=data(read->(x,y),in->)
	! 1,4
	! 2,6
	! 3,2
	! 5,1
	! /
	! stat()
	! *** gnuplot hides points at border
	! fi=plotyx(y,x,continue->fcont)
	! ** The ranges needs to be adjusted manually
	! show(fi,xrange->(0,6),yrange->(0,7),continue->fcont)
	! endex
 
	!endsection
 
 
	logical p
	integer :: nu
 
	call  j_startfunction(iob,io,0,narg,j_arg,ivfig);if(j_err)return
	j_ivfig=ivfig
	j_p=j_v(j_ivdollar2).eq.777.d0
	if(narg.eq.1)then
		ivfig=j_arg(1)
		if(j_err)return
		if(j_otype(ivfig).eq.j_ipchar)then
			call j_getchar(ivfig,j_showfile,j_lenshowfile);if(j_err)return
			inquire(file=j_showfile(1:j_lenshowfile),exist=j_yes,opened=j_yes2)
			if(.not.j_yes)then
				call j_getname(ivfig)
				write(6,*)'file ',j_showfile(1:j_lenshowfile),' pointed by ',j_oname(1:j_loname),' does not exist'
				j_err=.true.;return
			endif !if(.not.j_yes)  11289
			if(j_yes2)then
				write(6,*)'file ',j_showfile(1:j_lenshowfile),' pointed by ',j_oname(1:j_loname),' is closed first'
				nu=j_iounit(ivfile)
				if(nu.gt.0)call j_closeunit(nu)
			endif !if(j_yes2)  11294
			call j_showfigepilog()
		elseif(j_otype(ivfig).eq.j_ipfigure)then !if(j_otype(ivfig).eq.j_ipchar)then
			j_gpi(-1:)=>j_o(ivfig)%i(1:)
			j_gpi2(0:)=>j_o(ivfig)%i2(1:)
			call j_gpfigoption(iob,io,ivfig)
			!	write(6,*)'SHOW gpshow,gpcont',j_gpshow,j_gpcontinue
			if(j_err)return
			if(j_gpshow)call j_showfig(ivfig)
		else !if(j_otype(ivfig).eq.j_ipchar)then
			call j_getname(ivfig)
			write(6,*)j_oname(1:j_loname),' not FIGURE or CHAR for file name'
			j_err=.true.
		endif !if(j_otype(ivfig).eq.j_ipchar)  11286
		!!call j_clearoption(iob,io)
		return
	else
		!subroutine j_getoption(iob,io,moption,minarg,maxarg,iptype,needsarg,noptarg,
		! optarg,link,gotchar,lengot,isfile) ! %%option
		call j_getoption(iob,io,j_mmultiplot,-1,1,j_ipchar,.true.,noptarg,j_optarg0,gotchar=j_gottext,lengot=j_lgottext)
		if(noptarg.le.0)then
			j_gottext='set size 1,1;set origin 0,0;set multiplot layout 1,2 scale 1,1'
			!          12345678901234567890123456789012345678901234567890
			if(narg.gt.2)then
				nr=(narg+1)/2
				j_gottext(50:50)=char(47+nr)
			endif !if(narg.gt.2)  11321
			j_lgottext=len_trim(j_gottext)
			write(6,*)'using default'
			write(6,*)'multiplot->',j_gottext(1:j_lgottext)
		endif !if(noptarg.le.0)  11318
		if(j_err)return
		call j_getoption(iob,io,j_mxrange,-1,2*narg,j_ipreal,.true.,nxrange,j_optarg0)
 
		call j_getoption(iob,io,j_myrange,-1,2*narg,j_ipreal,.true.,nyrange,j_optarg)
		if(j_err)return
		!	call j_showfighead(ivfig,nu)
		call j_showfighead(ivfig,nu)
		!write(6,*)'ivfig nu',ivfig,nu
		write(nu,'(a)')j_gottext(1:j_lgottext)
		iba=0
		do i=1,narg
			if(j_otype(j_arg(i)).ne.j_ipfigure)then
				call j_getname(j_arg(i))
				write(6,*)'*',j_oname(1:j_loname),' is not FIG'
				j_err=.true.;return
			endif !if(j_otype(j_arg(i)).ne.j_ipfigure)  11340
			if(nxrange.ge.iba+1)then
				va=j_v(j_optarg0(iba+1))
				if(va.gt.-1.)write(nu,'(a,f6.3)')'set lmargin at screen ',va
			endif !if(nxrange.ge.iba+1)  11345
			if(nxrange.ge.iba+2)then
				va=j_v(j_optarg0(iba+2))
				if(va.gt.-1.)write(nu,'(a,f6.3)')'set rmargin at screen ',va
			endif !if(nxrange.ge.iba+2)  11349
			if(nyrange.ge.iba+1)then
				va=j_v(j_optarg(iba+1))
				if(va.gt.-1.)write(nu,'(a,f6.3)')'set bmargin at screen ',va
			endif !if(nyrange.ge.iba+1)  11353
			if(nyrange.ge.iba+2)then
				va=j_v(j_optarg(iba+2))
				if(va.gt.-1.)write(nu,'(a,f6.3)')'set tmargin at screen ',va
			endif !if(nyrange.ge.iba+2)  11357
			j_gpi(-1:)=>j_o(j_arg(i))%i(1:)
			j_gpi2(0:)=>j_o(j_arg(i))%i2(1:)
			!	call j_figdump(j_arg(i))
			j_gpcontinue=j_isoption(iob,io,j_mcontinue)
			call j_showfig0(j_arg(i),nu)
			iba=iba+2
		enddo !i=1,narg  11339
		write(nu,'(a)')'unset multiplot'
		call j_closeunit(nu)
		call j_showfigepilog()
 
 
	endif !if(narg.eq.1)  11283
 
 
end subroutine show !subroutine show(iob,io)
 
subroutine plot3d(iob,io)
	! Section plot3d plot3d() 3d-figure.
	! Plot 3d-figure with indicater contours  with colours.
	! endheader
	! Option
	! Output & 1  & & ]fi[=plot3d() generates gnuplot file ]fi[.jfig.
	! No figure object is produced.
	! Args& 1 & MATRIX & The argument is a matrix having 3 columns for x,y and z.
	! sorted & N | 1 & &plot3d() uses the gnuplot function splot, which requires that the data
	! is sorted withe respect to the x-variable. sorted-> indicates that the argument matrix is sorted
	! either natrurally or with sort() function. If sort-> is not presented, plot3
	! sorts the data.
	! endoption
	! Ex plot3dex plot3d() example see p.  328 in Mehtatalo & Lappi 2020
	! mat=matrix(1000000,3)
	! mat2=matrix(1000000,3)
	! transa=trans() !second order response surface
	! x=0
	! x2=0
	! xy=0
	! irow=1
	! do(ix,1,1000)
	! y=0
	! y2=0
	! xy=0
	! do(iy,1,1000)
	! mat(irow,1)=x
	! mat(irow,2)=y
	! mat(irow,3)=12+8*x-7*x2+124*y+8*xy-13*y2
	! mat2(irow,1)=x
	! mat2(irow,2)=y
	! mat2(irow,3)=50+160*x-5*x2-40*y-20*xy+10*y2
	! irow=irow+1
	! y=y+0.01
	! y2=y*y
	! xy=x*y
	! enddo
	! x=x+0.01
	! x2=x*x
	! enddo
	! /
	!*** This example takes some time
	! call(transa)
	! fi=plot3d(mat,sorted->,continue->fcont)
	!** This is commneted because it takes some time
	!** fi=plot3d(mat2,sorted->,continue->fcont)
	! endex
 
	! endsection
 
 
	integer, dimension (:), pointer::arg
	integer, dimension(:), allocatable::p
	real ,dimension(:), allocatable::xi
	double precision ::zmin,zmax
	logical sorted
	zmin=j_v(j_ivinf)
	zmax=j_v(j_ivtolast)
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	!	io_=io_+narg+3
	imat=j_o(iob)%i(io+2)
	j_gpcontinue=j_isoption(iob,io,j_mcontinue)
	!j_gpdelete=j_isoption(iob,io,j_mdelete)
	if(j_otype(imat).ne.j_ipmatrix)then
		call j_getname(imat)
		write(6,*)'plot3d needs a matrix argument ',j_oname(1:j_loname), ' is ',&
			j_otypes(j_otype(imat))
		j_err=.true.;return
	endif !if(j_otype(imat).ne.j_ipmatrix)  11442
	nrow=j_o(imat)%i(1)
	if(j_o(imat)%i(2).ne.3)then
		write(6,*)'argument must have 3 columns'
		j_err=.true.
		return
 
	endif !if(j_o(imat)%i(2).ne.3)  11449
	sorted=j_linkoption(iob,io,j_msorted,clear=.true.).ge.0
 
	if(.not.sorted)then
		allocate(xi(1:nrow),p(1:nrow))
		ibas=1
		do i=1,nrow
			xi(i)=j_o(imat)%d(ibas)
 
			ibas=ibas+3
		end do !i=1,nrow  11460
		call SSORTP (xi, 1, nrow, p)
 
	endif !if(.not.sorted)  11457
	ibas=3
	do i=1,nrow
		zmin=min(zmin,j_o(imat)%d(ibas))
		zmax=max(zmax,j_o(imat)%d(ibas))
		ibas=ibas+3
	enddo !i=1,nrow  11469
	call j_getfile(nu,rw='w',ivout=iv,ivfile=iout,ext='.jfig',replace=.true.)
	if(j_err)return
	call j_getchar(iv,j_filename,le);if(j_err)return
	write(nu,'(a)')
	write(nu,'(a)')'set pm3d map'
	write(nu,'(a)')'unset title'
	!set xrange [0:50]
	!set yrange [0:50]
	!#set format x ' '
	!#set format y ''
	write(nu,*)'set cbrange [',zmin,':',zmax,']'
	write(nu,'(a)')'set palette rgbformulae 22,13,10'
	write(nu,'(a)')"splot '-'"
	if(sorted)then
		ibas=0
		do i=1,nrow
			write(nu,'(3g12.6)')j_o(imat)%d(ibas+1:ibas+3)
			if(i.lt.nrow)then
				if(j_o(imat)%d(ibas+1).ne.j_o(imat)%d(ibas+4))write(nu,'(a)')' '
			endif !if(i.lt.nrow)  11491
			ibas=ibas+3
		enddo !i=1,nrow  11489
	else
		j=(p(1)-1)*3
		do i=1,nrow
			write(nu,'(3g12.6)')j_o(imat)%d(j+1:j+3)
			if(i.lt.nrow)then
				j2=(p(i+1)-1)*3
				if(j_o(imat)%d(j+1).ne.j_o(imat)%d(j2+1))write(nu,'(a)')' '
				j=j2
			endif !if(i.lt.nrow)  11500
		end do !i=1,nrow  11498
	endif !if(sorted)  11487
	if(allocated(xi))deallocate(xi,p)
	call j_closeunit(nu)
	i1=1
	if(j_filename(1:1).eq.'$')i1=2
	call execute_command_line('gnuplot --persist '//j_filename(i1:le), wait=.false.)
	!	 call execute_command_line('gnuplot --persist gnuclear.txt', wait=.false.)
	!	 old=.true.
	!	 	 call execute_command_line('gnuplot -- '//j_filename(1:le), wait=.false.)
	!if(j_gpcontinue.or.j_ispause)return
	!	write(6,*)'pausetassa ',j_cline
	!if(j_ninc.gt.1)call j_pause('<fig>',do=.true.)
end subroutine plot3d
 
subroutine gnuplot(iob,io)
	! Section gnuplot gnuplot() sends command to gnuplot.
	! Sends the character argument to gnuplot as a file.
	! endheader
	! Option
	! Output & 1  & & ]fi[=gnuplot() generates gnuplot file ]fi[.jfig. If no output is given
	!the the file name is gnuplotscript.txt
	! No figure object is produced.
	!endoption
	! Note gnuplot cammands can be put on a same line by separating the with semicolon.
	!Thus quite long scripts can be sent to gnuplot thia way.
	!endnote
	!Ex gnuplotex
	!gnuplot('splot [-2:2] [-2:2] exp(-0.5*(x**2+y**2))*(x**2+y**2)')
	!endex
	! endsection
 
	!getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout,readit,deleted)
	call j_startfunction(iob,io,j_ipchar,narg,j_arg,ivout);if(j_err)return
	j_ivfig=ivout
	if(ivout.eq.j_ivresult)then
		call j_getfile(nu,rw='w',ext='gnuplotscript.jfig',replace=.true.)
	else
		call j_getfile(nu,rw='w',ivfile=ivout,ext='.jfig',replace=.true.)
	endif !if(ivout.eq.j_ivresult)  11540
	if(j_err)return
	call j_getchar(j_arg(1),j_tempchar,le);if(j_err)return
	write(nu,'(a)')j_tempchar(1:le)
	call j_closeunit(nu)
	j_yes=j_isoption(iob,io,j_mcontinue,.false.)
	j_yes2=j_isoption(iob,io,j_mshow,.true.)
	if(j_yes2)&
		call execute_command_line('gnuplot --persist '//j_filename(1:le), wait=.false.)
	!	 call execute_command_line('gnuplot --persist gnuclear.txt', wait=.false.)
	!	 old=.true.
	!	 	 call execute_command_line('gnuplot -- '//j_filename(1:le), wait=.false.)
 
	!	write(6,*)'pausetassa ',j_cline
	!if(j_ninc.gt.1)call j_pause('<gnu>',do=.true.)
end subroutine gnuplot
 
subroutine tics(iob,io)
	character*8::ch
	call j_startfunction(iob,io,j_ipreal,narg,j_arg,ivout,needsout=.true.,argval=j_tempvector,nval=nval);if(j_err)return
	call j_getoption(iob,io,j_mxrange,2,2,j_ipreal,.true.,noptarg,j_optarg0)
	if(j_err)return
	nsub=j_tempvector(1)
	if(nsub.le.0)then
		write(6,*)'first arg in tics must be number of subplots ',nsub,' is illegal'
		j_err=.true.;return
	endif !if(nsub.le.0)  11567
	xmin=j_v(j_optarg0(1))
	xmax=j_v(j_optarg0(2))
	if(xmin.ge.xmax)then
		write(6,*)'illegal xmin ',xmin,' and xmax ', xmax
		j_err=.true.;return
	endif !if(xmin.ge.xmax)  11573
	le=0
	write(6,*)'xmin',xmin,xmax,'nval', nval,j_tempvector
	do is=1,nsub
		do j=2,nval
 
			x=(is-1)*(xmax-xmin)+j_tempvector(j)
			!	write(6,*)'jx',j,x
			ch=j_chr8b(x,lec)
			le2=le+2*lec+4
			j_buffer(le+1:le2)='"'//ch(1:lec)//'" '//ch(1:lec)//','
			le=le2
		enddo !j=2,nval  11580
	enddo !is=1,nsub  11579
	le=le-1
	write(6,*)j_buffer(1:40)
	write(6,*)le,ch
	iv=j_defchar(0,j_buffer(1:le))
	if(j_err)return
	call j_asschar2(iv,ivout)
	return
 
 
end subroutine
subroutine clearbits(iob,io)
	! Section clearbits clearbits() Clears bits
	! To be reported later
	! endsection
 
	real rw
	integer iw
	equivalence(iw,rw)
	!!call j_clearoption(iob,io)
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	ivb=j_o(iob)%i(io+2)
	select case(j_otype(ivb))
	case default !select case(j_otype(ivb))
	call j_printname('**setbits:', ivb,' is not legal object for bits')
	j_err=.true.
	return
	case(j_ipreal) !select case(j_otype(ivb))
	do i=2,narg
		j=j_v(j_o(iob)%i(io+1+i))
		if(j.gt.32.or.j.le.0)then
			call j_printname('**setbits, real variable', ivb,'can store only 32 bits ')
			write(6,*) 'you try to set bit ',j
			j_err=.true.
			return
		end if !if(j.gt.32.or.j.le.0)  11621
		!   ii=(j-1)/32+1
		ibit=j-1  ! bit numbering starts from zero
		rw=j_v(ivb)
		iw= ibclr(iw,ibit)
		j_v(ivb)=rw
	end do !i=2,narg  11619
 
	case(j_iplist) !select case(j_otype(ivb))
	do i=2,narg
		j=j_v(j_o(iob)%i2(io+1+i))
		ii=(j-1)/64 +1  !32+1
		if(ii.gt.j_o(ivb)%i(0).or.j.le.0)then
			call j_printname('*setbits: list', ivb,' ')
			write(6,*)'**can store ',64*j_o(ivb)%i(0), 'bits and you try to set bit',j
			j_err=.true.
			return
		end if !if(ii.gt.j_o(ivb)%i(0).or.j.le.0)  11638
		ibit=j-(ii-1)*64-1   !(ii-1)*32-1  ! bit numbering starts from zero
		rw=j_v(j_o(ivb)%i(ii))
		iw= ibclr(iw,ibit)
		j_v(j_o(ivb)%i(ii))=rw
	end do !i=2,narg  11635
	end select !select case(j_otype(ivb))
	return
end subroutine clearbits !subroutine clearbits(iob,io)
 
subroutine setbits(iob,io)
	! Section setbits setbits() Sets bits
	! To be reported alter
	! endsection
 
	real rw
	integer iw
	equivalence(iw,rw)
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	ivb=j_o(iob)%i(io+2)
	select case(j_otype(ivb))
	case default !select case(j_otype(ivb))
	call j_printname('**setbits:', ivb,' is not legal object for bits')
	j_err=.true.
	return
	case(j_ipreal) !select case(j_otype(ivb))
	do i=2,narg
		j=j_v(j_o(iob)%i(io+1+i))
		if(j.gt.64.or.j.le.0)then
			call j_printname('**setbits, real variable', ivb,'can store only 64 bits ')
			write(6,*) 'you try to set bit ',j
			j_err=.true.
			return
		end if !if(j.gt.64.or.j.le.0)  11673
		!   ii=(j-1)/32+1
		ibit=j-1  ! bit numbering starts from zero
		rw=j_v(ivb)
		iw= ibset(iw,ibit)
		j_v(ivb)=rw
	end do !i=2,narg  11671
	case(j_iplist) !select case(j_otype(ivb))
	do i=2,narg
		j=j_v(j_o(iob)%i2(io+1+i))
		if(j.gt.j_o(ivb)%i(1).or.j.le.0)then
			call j_printname('**bits object', ivb,' ')
			write(6,*)' has ',j_o(ivb)%i(1), 'bits and you try to set bit',j
			j_err=.true.
			return
		end if !if(j.gt.j_o(ivb)%i(1).or.j.le.0)  11688
		ii=(j-1)/64+1  !32+1
		if(ii.gt.j_o(ivb)%i(0).or.j.le.0)then
			call j_printname('**setbits: list', ivb,' ')
			write(6,*)'can store ',64*j_o(ivb)%i(1), 'bits and you try to set bit',j
			j_err=.true.
			return
		end if !if(ii.gt.j_o(ivb)%i(0).or.j.le.0)  11695
		ibit=j-(ii-1)*64-1  !32-1  ! bit numbering starts from zero
		rw=j_v(j_o(ivb)%i(ii))
		iw= ibset(iw,ibit)
		j_v(j_o(ivb)%i(ii))=rw
	end do !i=2,narg  11686
	end select !select case(j_otype(ivb))
	return
end subroutine setbits !subroutine setbits(iob,io)
 
subroutine getbit(iob,io)
	! Section getbit getbit() : Gets bit
	! To be reported later, see old manual
	! endsection
 
	double precision rw
	integer*8 iw
	equivalence(iw,rw)
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	ivb=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	j=j_v(j_o(iob)%i(io+3))
	icurint=(j-1)/64+1  !
	icurbit=j-(icurint-1)*64-1
	select case(j_otype(ivb))
	case default !select case(j_otype(ivb))
	call j_printname('**getbit:', ivb,' is not legal object for bits')
	j_err=.true.
	return
	case(j_ipreal) !select case(j_otype(ivb))
	if(j.gt.64.or.j.le.0)then
		call j_printname('**getbit, real variable', ivb,'has only 64 bits ')
		write(6,*) 'you try to get bit ',j
		j_err=.true.
		return
	end if !if(j.gt.64.or.j.le.0)  11733
	icurbit=j-1
	rw=j_v(ivb)
	if(btest(iw,icurbit))then
		j_v(iout)=1.
	else !if(btest(iw,icurbit))then
		j_v(iout)=0.
	end if !if(btest(iw,icurbit))  11741
	case(j_iplist) !select case(j_otype(ivb))
	ii=(j-1)/64+1
	if(ii.gt.j_o(ivb)%i(0).or.j.le.0)then
		call j_printname('**getbit: list', ivb,' ')
		write(6,*)'#can store ',64*j_o(ivb)%i(1), 'bits and you try to get bit',j
		j_err=.true.
		return
	end if !if(ii.gt.j_o(ivb)%i(0).or.j.le.0)  11748
	icurbit=j-(ii-1)*64-1
	rw=j_v(j_o(ivb)%i(ii))
	if(btest(iw,icurbit))then
		j_v(iout)=j_1
	else !if(btest(iw,icurbit))then
		j_v(iout)=j_0
	end if !if(btest(iw,icurbit))  11756
	end select !select case(j_otype(ivb))
	return
end subroutine getbit !subroutine getbit(iob,io)
 
subroutine getbitch(iob,io)
	! Section getbitch getbitch() : get bit value
	! To be reported later, see old manual
	! endsection
	double precision rw
	integer*8 iw
	integer ::i1,i2
	equivalence(iw,rw)
	!!call j_clearoption(iob,io)
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	ivb=j_o(iob)%i(io+2)
	i1=1
	if(narg.eq.2)then
		i2=j_v(j_o(iob)%i(io+3))
	elseif(narg.eq.3)then !if(narg.eq.2)then
		i1=j_v(j_o(iob)%i(io+3))
		i2=j_v(j_o(iob)%i(io+4))
	end if !if(narg.eq.2)  11781
	select case(j_otype(ivb))
	case default !select case(j_otype(ivb))
	call j_printname('**getbitch:', ivb,' is not legal object for bits')
	j_err=.true.
	return
	case (j_ipreal) !select case(j_otype(ivb))
	if(narg.le.1)i2=64
	i2=min(64,i2)
	ivout_=j_deftext(iout,' ',1,i2-i1+1)
	rw=j_v(ivb)
	ie=0
	do i=i1,i2
		ie=ie+1
		if(btest(iw,i-1))then
			j_cline(ie:ie)='1'
		else !if(btest(iw,i-1))then
			j_cline(ie:ie)='0'
		end if !if(btest(iw,i-1))  11800
	end do !i=i1,i2  11798
	case(j_iplist) !select case(j_otype(ivb))
	if(narg.le.1)i2=j_o(ivb)%i(1)*64
	i2=min(64*j_o(ivb)%i(1),i2)
	ivout_=j_deftext(iout,' ',1,i2-i1+1)
	ie=0
	do i=i1,i2
		ie=ie+1
		ii=(i-1)/64+1
		icurbit=i-(ii-1)*64-1
		rw=j_v(j_o(ivb)%i2(ii))
		if(btest(iw,icurbit))then
			j_cline(ie:ie)='1'
		else !if(btest(iw,icurbit))then
			j_cline(ie:ie)='0'
		end if !if(btest(iw,icurbit))  11816
	end do !i=i1,i2  11811
	end select !select case(j_otype(ivb))
	call j_puttext(ivout_,j_cline(1:ie))
	return
end subroutine getbitch !subroutine getbitch(iob,io)
 
 
 
subroutine sort(iob,io)
	! Section sort sort() Sorts MATRIX
	! Usage:\\
	! sort(a,key->(key1[,key2]))\\
	! Makes a new matrix obtained by sorting all matrix columns of MATRIX a according to one or two columns.
	! Absolute value of key1 and the value of key2 must be legal column numbers.
	! If key1 is
	! positive then the columns are sorted in ascending order,
	! if key1 is negative then the columns
	! are sorted in descending order. If two keys are given, then first key dominates.
	! endheader
	! Note It is currently
	! assumed that if there are two keys then the values in first key column have integer values.
	! endnote
	! Note  If key2 is not given and key1 is positive, then the syntax is: sort(a,key->key1).
	! endnote
	! Note If there is no output, then the argument matrix is sorted in place.
	! endnote
	! Note The argument can be the data matrix of a data object. The data object will remain a
	! valid data object.
	! endnote
	! endsection
 
 
	integer, dimension(:), allocatable::p
	real ,dimension(:), allocatable::xi
	double precision s,xmin,xmax,range
	integer::ncol,nrow_
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	imat=j_o(iob)%i(io+2)
	if(j_otype(imat).ne.j_ipmatrix)then
		call j_printname('**sort: argument ',imat,' not a matrix')
		j_err=.true.
		return
	end if !if(j_otype(imat).ne.j_ipmatrix)  11863
	nrow_=j_o(imat)%i(1)
	ncol=j_o(imat)%i(2)
	call	j_getoption(iob,io,j_mkey,1,2,j_ipreal,.true.,noptarg,j_optarg0) ! %%option
	if(j_err)return
	!	ivkey=j_optarg0(1)
 
	key0=j_v(j_optarg0(1))
	key=abs(key0)
	if(key.le.0.or.key.gt.ncol)then
		write(6,*)'**key in sort is ',key, ' should be in range:',1,ncol
		j_err=.true.
		!	return
	end if !if(key.le.0.or.key.gt.ncol)  11876
 
	! if(j_o(iob)%i(j_linkoption(iob,io,j_mkey)).gt.2)then
	! write(6,*)'**max number of keys is two'
	! j_err=.true.
	! !	return
	! end if !if(j_o(iob)%i(j_linkoption(iob,io,j_mkey)).gt.2)then
	!!call j_clearoption(iob,io)  ! subroutine
	if(j_err)return
	key2=0
	if(noptarg.eq.2)then
		key2=j_v(j_optarg0(2) )
		if(key2.le.0.or.key2.gt.ncol)then
			write(6,*)'** 2. key in sort is ',key2, ' should be in range:',1,ncol
			j_err=.true.
			return
		end if !if(key2.le.0.or.key2.gt.ncol)  11892
		write(6,*)'*wrn* using two keys in sort assumes currently first key to be positive integer'
		smax=-huge(1.)
		smin=huge(1.)
		do i=1,nrow_
			s=j_o(imat)%d((i-1)*ncol+key2)
			smax=max(s,smax)
			smin=min(s,smin)
		end do !i=1,nrow_  11900
		range=smax-smin+1.
	end if !if(noptarg.eq.2)  11890
	allocate(xi(1:nrow_),p(1:nrow_))
	do i=1,nrow_
		xi(i)=j_o(imat)%d((i-1)*ncol+key)
		if(key2.ne.0)xi(i)=xi(i)+(j_o(imat)%d((i-1)*ncol+key2)-smin)/range
	end do !i=1,nrow_  11908
	if(key0.lt.0)xi=-xi
	!write(6,*)'xi',xi
	call SSORTP (xi, 1, nrow_, P)  !matsub
	if(key0.lt.0.and.key2.eq.0)xi=-xi
	if(iout.eq.j_ivresult)then
		iout=imat
	else !if(iout.eq.j_ivresult)then
		ivout_=j_defmatrix(iout,' ',int8(nrow_),int8(ncol),j_matreg)
	end if !if(iout.eq.j_ivresult)  11916
	!c     R is not disturbed.  P is set so that R(P(J)) is the J'th element
	!c     of the sorted sequence.
	if(key2.eq.0)then
		do i=1,nrow_
			j_o(iout)%d((i-1)*ncol+key)=xi(p(i))
		end do !i=1,nrow_  11924
	end if !if(key2.eq.0)  11923
	do j=1,ncol
		if(j.eq.key.and.key2.eq.0)cycle
		do i=1,nrow_
			xi(i)=j_o(imat)%d((i-1)*ncol+j)
		end do !i=1,nrow_  11930
		do i=1,nrow_
			j_o(iout)%d((i-1)*ncol+j)=xi(p(i))
		end do !i=1,nrow_  11933
	end do !j=1,ncol  11928
	deallocate(xi,p)
	return
end subroutine sort !subroutine sort(iob,io)
 
 
! subroutine sumu(iob,io)  !for testing purposes
 
! character*80 line_
! !	io=io_
! narg=j_o(iob)%i(io+1)
! !	io_=io_+narg+3
 
! open(16,file='fort.16')
! nlintot=0
! 1 write(6,*)'monta rivi tulostetaan (neg bypass,99 get number of lines'
! read(5,*)nlines_
! nlin=abs(nlines_)
! if(nlin.eq.99)then
! nl=0
! 2 read(16,*,end=56)
! nl=nl+1
! goto 2
! 56 write(6,*)'rivei was', nl
! rewind (16)
! goto 1
! end if !if(nlin.eq.99)then
! if(nlin.eq.0)goto 90
! write(17,*)'*****section****** starting at line',nlintot+1
! do i=1,nlin
! read(16,'(a)')line_
! if(nlines_.gt.0)write(17,'(a)')line_(1:len_trim(line_))
! end do !do i=1,nlin
! nlintot=nlintot+nlin
! goto 1
! 90 continue !  io=io+narg+3
! close(17)
! return
! end subroutine sumu !subroutine sumu(iob,io)
 
 
 
subroutine thisfile(iob,io) ! returns the character object referring to current include file
	!
	! Section thisfile thisfile() Name of the current ;incl -file
	! The name of the current include file is returned as a character variable by:
	! out=thisfile()
	! This is useful when defining shortcuts for commands that include sections from an include file.
	! Using this function the shortcuts work even if the name of the include file is changed. See file
	! jexamples.inc for an application
	! endsection
 
	character*40 filename
	narg=j_o(iob)%i(io+1)
	ivout=j_o(iob)%i(io+narg+2)
 
	!	io=io+narg+3
	!	!!call j_clearoption(iob,io)  ! subroutine
 
	if(j_ninc.le.1)then
		write(6,*)'*thisfile not allowed at command level'
		j_err=.true.
	else !if(j_ninc.le.1)then
		call j_gettxtfile(j_inciv(j_ninc),j_filename,le)
		call j_asschar(ivout,j_filename(1:le))
 
	endif !if(j_ninc.le.1)  11995
	return
end subroutine !subroutine thisfile(iob,io)
 
subroutine filestat(iob,io)
	! Section filestat filestat() Information of a file
	! Function filestat(filename) prints the size of the
	! file in bytes (if available) and the time the file was last accessed
	! endsection
 
	!ite(6,*)j_o(iob)%i(io:io+4)
	ifile=j_o(iob)%i(io+2)
	if(j_otype(ifile).ne.j_ipchar)then
		write(6,*)'illegal argument'
		j_err=.true. ;return
	endif !if(j_otype(ifile).ne.j_ipchar)  12014
	isize=j_filesize(ifile=ifile,time=itime)
	if(isize.eq.0)then
		write(6,*)'cannot obtain infromation from file'
	else !if(isize.eq.0)then
		write(6,*)'size ',isize,' bytes, last accessed ',itime
	endif !if(isize.eq.0)  12019
	! 6.190 STAT — Get file status
 
	! Description:
	! This function returns information about a file. No permissions are required on the file itself, but execute (search) permission is required on all of the dir
	! The elements that are obtained and stored in the array BUFF:
 
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
	! buff(13)	Number of blocks allocated (-1 if not available)
	! Not all these elements are relevant on all systems. If an element is not relevant, it is returned as 0.
 
	! This intrinsic is provided in both subroutine and function forms; however, only one form can be used in any given program unit.
 
	! Standard:
	! GNU extension
	! Class:
	! Subroutine, function
	! Syntax:
	! CALL STAT(FILE,BUFF[,STATUS])
	! Arguments:
	! FILE	The type shall be CHARACTER(*), a valid path within the file system.
	! BUFF	The type shall be INTEGER(4), DIMENSION(13).
	! STATUS	(Optional) status flag of type INTEGER(4). Returns 0 on success and a system specific error code otherwise.
 
 
end subroutine !subroutine filestat(iob,io)
subroutine print_f(iob,io)
	! Section printf print_f() prints files
	! Prints the files associated with the CHAR arguments\\
	! maxlines->  tells the maximum number of lines to be printed, default the value of Maxlines
	! endsection
 
	logical isopen
	call j_startfunction(iob,io,j_ipchar,narg,j_arg,ivout);if(j_err)return
	call j_getoption(iob,io,j_mmaxlines,-1,1,j_ipreal,.true.,nargo,j_optarg0)
	if(j_err)return
	if(nargo.gt.0)then
		maxlines=j_v(j_optarg0(1))
	else
		maxlines=j_v(j_ivmaxlines)
	endif !if(nargo.gt.0)  12070
	do i=1,narg
		call j_getname(j_arg(i))
		write(6,*)'file ',j_oname(1:j_loname),':'
		write(6,*)'_____________________'
		nuu=j_iounit(j_arg(i))
		!			write(6,*)'<77nuu',nuu
		if(nuu.ne.0)then
			isopen=.true.
			write(6,*)'file was open, it is rewinded'
			rewind(nuu)
		else !if(nuu.ne.0)then
			call j_getchar(j_arg(i),j_filename,le_);if(j_err)return
			inquire(file = j_filename(1:le_) , exist=j_yes)
			if(j_yes)then
				call j_getfile(nuu,'r',ivfile=j_arg(i))
				if(j_err)return
				isopen=.false.
			else
				write(6,*)'file does not exist'
				j_err=.true.
				return
			endif !if(j_yes)  12088
		endif !if(nuu.ne.0)  12081
		nlin=0
500		read(nuu,'(a)',end=888,err=99)j_tempchar2
		nlin=nlin+1
		if(nlin.le.maxlines)then
 
			write(6,*)'  ',j_tempchar2(1:len_trim(j_tempchar2))
			goto 500
 
		endif !if(nlin.le.maxlines)  12101
		if(nlin.ge.maxlines)then
			write(6,*)' '
			write(6,*)'printing limited with Maxlines ',maxlines
			return
		endif !if(nlin.ge.maxlines)  12107
888	 continue
		write(6,*)'______________'
		call j_closeunit(nuu)
	enddo !i=1,narg  12075
	return
	99 write(6,*)'error in reading, is file open for write?'
	j_err=.true.
	return
 
 
 
 
 
end subroutine print_f
 
subroutine nrows(iob,io)  ! %%matrix
	! Section nrows nrows() Number of rows in MATRIX, TEXT or BITMATRIX
	!can be used as:
	! \begin{itemize}
	! \item  nrows(MATRIX)
	! \item  nrows(TEXT)
	! \item  nrows(BITMATRIX)
	! \end{itemize}
	! endheader
	! Note If the argument has another object type, and error occurs
	! endnote
	! endsection
 
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iarg=j_o(iob)%i(io+2)
	select case(j_otype(iarg))
	case(j_ipmatrix) !select case(j_otype(iarg))
	j_v(iout)=j_nrows(iarg) !j_o(iarg)%i(1)
	case(j_iptext) !select case(j_otype(iarg))
	j_v(iout)=j_o(iarg)%i(0)
	case(j_ipbitmatrix) !select case(j_otype(iarg))
	j_v(iout)=j_o(iarg)%i(1)
	case default !select case(j_otype(iarg))
	call j_printname('**argument ',iarg, ' has not a legal type for nrows()')
	j_err=.true.;return
	end select !select case(j_otype(iarg))
	return
end subroutine nrows !subroutine nrows(iob,io)
 
 
subroutine ncols(iob,io) !%%matrix
	! Section ncols ncols() Number of columns in MATRIX or BITMATRIX
	!	can be used as:
	! \begin{itemize}
	! \item  nrows(MATRIX)
	! \item  nrows(BITMATRIX)
	! \end{itemize}
	! endheader
	! Note If the argument has another object type, and error occurs
	! endnote
	! endsection
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iarg=j_o(iob)%i(io+2)
	select case(j_otype(iarg))
	case(j_ipmatrix) !select case(j_otype(iarg))
	j_v(iout)=j_ncols(iarg)  !j_o(iarg)%i(2)
	case(j_ipbitmatrix) !select case(j_otype(iarg))
	j_v(iout)=j_o(iarg)%i(2)
	case default !select case(j_otype(iarg))
	call j_printname('**argument ',iarg, ' has not a legal type for ncols()')
	j_err=.true.;return
	end select !select case(j_otype(iarg))
	return
end subroutine ncols !subroutine ncols(iob,io)
 
subroutine len(iob,io)
 
	! Section len len() Length of LIST, ILIST or MATRIX
	! len(]arg[)  Lenghts for the following argument types
	! \begin{itemize}
	! \item  ]arg[ is MATRIX => len=the size of the matrix, i.e.
	! nrows(]arg[)*ncols(]arg[)
 
	! \item  ]arg[ is TEXT => len=the number of chracter in TEXT object
 
	! \item  ]arg[ is LIST => len=the number of elements in LIST
	! \item  ]arg[ is ILIST => len=the number of elements in ILIST
	! \end{itemize}
	! If ]arg[ does not have a legal type for len(), then len(]arg[)=-1 if len() has
	! option any->, otherwise an error is produced.
	! endsection
 
 
	logical any
	any=j_linkoption(iob,io,j_many).ge.0
	!	if(any)!!call j_clearoption(iob,io)  ! subroutine
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iarg=j_o(iob)%i(io+2)
	select case(j_otype(iarg))
	case(j_ipmatrix) !select case(j_otype(iarg))
	j_v(iout)=j_nelem(iarg)  !j_o(iarg)%i(3)
	case(j_iptext) !select case(j_otype(iarg))
 
	j_v(iout)=j_o(iarg)%i( j_o(iarg)%i(0)+1) -1
	case (j_ipchar)
	j_v(iout)=j_o(iarg)%i(2)-j_o(iarg)%i(1)+1
	case(j_iplist) !select case(j_otype(iarg))
	j_v(iout)=j_lenlist(iarg)
	case(j_ipilist) !select case(j_otype(iarg))
	j_v(iout)=j_o(iarg)%i(1)
	case(j_ipregr) !select case(j_otype(iarg))
	j_v(iout)=j_o(iarg)%i(0)
	case default !select case(j_otype(iarg))
	if(any)then
		j_v(iout)=-1
	else !if(any)then
		call j_printname('**argument ',iarg, ' has not a legal type for len()')
		j_err=.true.;return
	endif !if(any)  12232
	end select !select case(j_otype(iarg))
	return
end subroutine len !subroutine len(iob,io)
 
SUBROUTINE envelope(iob,io)
 
	!Section envelope envelope() Convex hull of point
	!endheader
	!Option
	!Output& 1&MATRIX & (nvertex+1, 2) matrix of the coordinates of the convex hull, where nvertex is the number of
	!verteces. The last point is the same as the first point
	! arg &1&MATRIX & (n,2) matrix of point coordinates
	!nobs&-1|1& REAL& The number of points if not all
	! points of the input matrix are used
	! endoption
	! Note The transpose of the output can be directly used in frawline() function
	!to draw the envelope
	!endnote
	!Note The function is using a subroutine made by Alan Miller and found in Netlib
	!endnote
	!endsection
 
 
 
	!  Find the vertices (in clockwise order) of a polygon enclosing
	!  the points (x(i), y(i), i=1, ..., n.
 
	!  On output, vertex(i), i=1, ..., nvert contains the numbers of the vertices.
	!  iwk() is an integer work array which must have dimension at least n
	!  in the calling program.
 
	!  There is a limit of 100 vertices imposed by the dimension of array next. (500)
	!based on ::::
	!  Programmer: Alan Miller
	!  Latest revision - 12 September 1987
	!  Fortran 90 version - 8 August 1996
	! input is a(n,2) matrix of ploygon mat(1,  = x-value
	!                                   mat(2, =y value
	! in output the last point is the same as the first one
	! so the output can be directly used in drawline
 
	IMPLICIT NONE
	INTEGER :: n
	integer ::	nvert
	INTEGER,dimension(:),allocatable :: vertex, iwk
 
	INTEGER ::  i, i1, i2, j, jp1, jp2, i2save, i3, i2next
	INTEGER,allocatable,dimension(:) :: next
	double precision    :: xmax, xmin, ymax, ymin, dist, dmax, dmin, x1, y1, dx, dy, x2, y2, &
		dx1, dx2, dmax1, dmax2, dy1, dy2, temp, zero = 0.0
	integer,dimension(:),pointer ::arg
	integer narg,ivout,iout,iob,io,istart,ivmat
	double precision x,y
	x(i)=	j_o(ivmat)%d((i-1)*2+1) !j_getmatel(arg(1),i,1)
	y(i)=	j_o(ivmat)%d(i*2) !j_getmatel(arg(1),i,1)
	call j_startfunction( iob,io,j_ipmatrix,narg,arg,ivout)
	if(j_err)return
	ivmat=arg(1)
	if(j_o(ivmat)%i(2).ne.2)then
		write(6,*)'*envelope: currently there must be two columns in the input'
		j_err=.true.
		return
	endif !if(j_o(ivmat)%i(2).ne.2)  12296
	call j_getoption(iob,io,j_mnobs,-1,1,j_ipreal,.true.,narg,arg);if(j_err)return
	if(narg.ge.1)then
		n=j_v(arg(1))
	else !if(narg.ge.1)then
		n=j_o(ivmat)%i(1)
	endif !if(narg.ge.1)  12302
	IF (n < 2) then
		write(6,*)'*envelope: there must be at least two points, there was ',n
		j_err=.true.
		RETURN
	endif !IF (n < 2)  12307
	allocate(vertex(1:n),iwk(1:n),next(1:500))
 
	IF (x(1) > x(n)) THEN
		vertex(1) = n
		vertex(2) = 1
		xmin = x(n)
		xmax = x(1)
	ELSE !IF (x(1) > x(n)) THEN
		vertex(1) = 1
		vertex(2) = n
		xmin = x(1)
		xmax = x(n)
	END IF !IF (x(1) > x(n))  12314
 
	DO i = 2, n-1
		temp = x(i)
		IF (temp < xmin) THEN
			vertex(1) = i
			xmin = temp
		ELSE IF (temp > xmax) THEN !IF (temp < xmin) THEN
			vertex(2) = i
			xmax = temp
		END IF !IF (temp < xmin)  12328
	END DO !i = 2, n-1  12326
 
	IF (xmax == xmin) THEN
		IF (y(1) > y(n)) THEN
			vertex(1) = n
			vertex(2) = 1
			ymin = y(n)
			ymax = y(1)
		ELSE !IF (y(1) > y(n)) THEN
			vertex(1) = 1
			vertex(2) = n
			ymin = y(1)
			ymax = y(n)
		END IF !IF (y(1) > y(n))  12338
 
		DO i = 2, n-1
			temp = y(i)
			IF (temp < ymin) THEN
				vertex(1) = i
				ymin = temp
			ELSE IF (temp > ymax) THEN !IF (temp < ymin) THEN
				vertex(2) = i
				ymax = temp
			END IF !IF (temp < ymin)  12352
		END DO !i = 2, n-1  12350
 
		nvert = 2
		IF (ymax == ymin) nvert = 1
		write(6,*)'*envelope: ymax=ymin=',ymax
		j_err=.true.
		RETURN
	END IF !IF (xmax == xmin)  12337
 
	!  Set up two initial lists of points; those points above & those below the
	!  line joining the first two vertices.    next(i) will hold the pointer to the
	!  point furthest from the line joining vertex(i) to vertex(i+1) on the left
	!  hand side.
 
	i1 = vertex(1)
	i2 = vertex(2)
	iwk(i1) = -1
	iwk(i2) = -1
	dx = xmax - xmin
	y1 = y(i1)
	dy = y(i2) - y1
	dmax = zero
	dmin = zero
	next(1) = -1
	next(2) = -1
 
	DO i = 1, n
		IF (i == vertex(1) .OR. i == vertex(2)) CYCLE
		dist = (y(i) - y1)*dx - (x(i) - xmin)*dy
		IF (dist > zero) THEN
			iwk(i1) = i
			i1 = i
			IF (dist > dmax) THEN
				next(1) = i
				dmax = dist
			END IF !IF (dist > dmax)  12391
		ELSE IF (dist < zero) THEN !IF (dist > zero) THEN
			iwk(i2) = i
			i2 = i
			IF (dist < dmin) THEN
				next(2) = i
				dmin = dist
			END IF !IF (dist < dmin)  12398
		END IF !IF (dist > zero)  12388
	END DO !i = 1, n  12385
 
	!  Ends of lists are indicated by pointers to -ve positions.
 
	iwk(i1) = -1
	iwk(i2) = -1
	nvert = 2
 
	j = 1
 
	!  Start of main process.
 
	!  Introduce new vertex between vertices j & j+1, if one has been found.
	!  Otherwise increase j.   Exit if no more vertices.
 
	40 IF (next(j) < 0) THEN
		IF (j == nvert) goto 99 !RETURN
		j = j + 1
		GO TO 40
	END IF !40 IF (next(j) < 0)  12418
 
	jp1 = j + 1
	DO i = nvert, jp1, -1
		vertex(i+1) = vertex(i)
		next(i+1) = next(i)
	END DO !i = nvert, jp1, -1  12425
	jp2 = jp1 + 1
	nvert = nvert + 1
	IF (jp2 > nvert) jp2 = 1
	i1 = vertex(j)
	i2 = next(j)
	i3 = vertex(jp2)
	vertex(jp1) = i2
 
	!  Process the list of points associated with vertex j.   New list at vertex j
	!  consists of those points to the left of the line joining it to the new
	!  vertex (j+1).   Similarly for the list at the new vertex.
	!  Points on or to the right of these lines are dropped.
 
	x1 = x(i1)
	x2 = x(i2)
	y1 = y(i1)
	y2 = y(i2)
	dx1 = x2 - x1
	dx2 = x(i3) - x2
	dy1 = y2 - y1
	dy2 = y(i3) - y2
	DMAX1 = zero
	dmax2 = zero
	next(j) = -1
	next(jp1) = -1
	i2save = i2
	i2next = iwk(i2)
	i = iwk(i1)
	iwk(i1) = -1
	iwk(i2) = -1
 
60 IF (i /= i2save) THEN
		dist = (y(i) - y1)*dx1 - (x(i) - x1)*dy1
		IF (dist > zero) THEN
			iwk(i1) = i
			i1 = i
			IF (dist > DMAX1) THEN
				next(j) = i
				DMAX1 = dist
			END IF !IF (dist > DMAX1)  12465
		ELSE !IF (dist > zero) THEN
			dist = (y(i) - y2)*dx2 - (x(i) - x2)*dy2
			IF (dist > zero) THEN
				iwk(i2) = i
				i2 = i
				IF (dist > dmax2) THEN
					next(jp1) = i
					dmax2 = dist
				END IF !IF (dist > dmax2)  12474
			END IF !IF (dist > zero)  12471
		END IF !IF (dist > zero)  12462
		i = iwk(i)
	ELSE !IF (i /= i2save) THEN
		i = i2next
	END IF !60 IF (i /= i2save)  12460
 
	!  Get next point from old list at vertex j.
 
	IF (i > 0) GO TO 60
 
	!  End lists with -ve values.
 
	iwk(i1) = -1
	iwk(i2) = -1
 
	GO TO 40
99 iout=j_defmatrix(ivout,' ',int8(nvert+1),2_8,j_matreg)
	do i=1,nvert
		j_o(ivout)%d((i-1)*2+1)=x(vertex(i))
		j_o(ivout)%d(i*2)=y(vertex(i))
	enddo !i=1,nvert  12496
	j_o(ivout)%d(2*nvert+1)=x(vertex(1))
	j_o(ivout)%d(2*nvert+2)=y(vertex(1))
	return
 
END SUBROUTINE envelope !SUBROUTINE envelope(iob,io)
 
 
subroutine values(iob,io)  !doub
	! Section values values() Different values of variables in DATA
	! Extracting values of class variables: values( ).
	! endheader
	! Option
	! Output&1&VECTOR& the vector getting differen values
	! arg&1&REALV& variables whose values obtained
	! data&1&DATA& The data set.
	! endoption
	! Note  The values found will be sorted in an increasing order.
	!endnote
	! Note After getting the values into a vector,
	!the number of different values can be obtained
	! using nrows() function.
	!endnote
 
	! Note values() function can be utilized e.g. in generating domains for all different
	! owners or regions found in data.
	!endnote
	!endsection
 
	double precision,dimension(:), allocatable::value9
	integer,dimension(:),allocatable::iperm  !quick sort
	integer*8::nval,ial,i
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	iv=j_o(iob)%i(io+2)
 
	iout=j_o(iob)%i(io+2+narg)
	call j_getdataobject(iob,io)
 
	if(j_err)return
	ivv=j_inlistobject(iv,j_divvars)
	if(ivv.le.0)then
		call j_getname(iv,j_divdata)
		write(6,*)'variable ',j_oname(1:j_loname),' is not in DATA ',j_oname2(1:j_loname)
		j_err=.true.;return
	endif !if(ivv.le.0)  12542
	!!call j_clearoption(iob,io )
	allocate(value9(1:j_dnobs))
	!		write(6,*)'<33allocat',j_dnobs,size(value9),'j_divmat ',j_divmat
	missing=0
	!	do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
	ial=j_dfrom
	!	write(6,*)'<4664 j_dfrom',j_dfrom,j_duntil
100	continue  !call j_nextobs();if(j_err)return
	call j_getobs(ial)
	if(j_err)then
		write(6,*)'error in Obs ',ial, ' out of ',j_dnobs
		return  !j_err
	endif !if(j_err)  12557
	if(j_rejected)then
		ial=ial+1
		if(ial.gt.j_duntil)then
			write(6,*)'*no observations accepted from Obs-range ',j_dfrom,j_duntil
			j_err=.true.
			return
		endif !if(ial.gt.j_duntil)  12563
		goto 100
	endif !if(j_rejected)  12561
	value9(1)=j_v(iv);nval=1;ial=ial+1
	nacc=1
	!	write(6,*)'<5757ial,j_duntil ',ial,j_duntil
	do i=ial,j_duntil
		call j_getobs(i)
		if(j_err)return  !j_err
 
		if(j_rejected)cycle
		if(abs(j_v(iv)).ge.1.7d18)then
			missining=missing+1
			cycle
		endif !if(abs(j_v(iv)).ge.1.7d18)  12578
		nacc=nacc+1
		if(.not.any(value9(1:nval).eq.j_v(iv)))then
			nval=nval+1;value9(nval)=j_v(iv)
		end if !if(.not.any(value9(1:nval).eq.j_v(iv)))  12583
	end do !i=ial,j_duntil  12573
	!	end do !do k=1,jndatasetss
	j_v(j_ivaccepted)=nacc
	write(6,*)'Accepted ',nacc,'  from ',j_dnobs
	allocate(iperm(1:nval))
	call j_quick_sort(value9(1:nval),iperm)
	ivout_=j_defmatrix(iout,' ',nval,j_18,j_matreg)
	j_o(iout)%d=value9(1:nval)
	deallocate(value9)
	deallocate(iperm)
 
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
	return
end subroutine values !subroutine values(iob,io)
 
! subroutine store(iob,io)
 
! !	io=io_
! narg=j_o(iob)%i(io+1)
! !	io_=io_+narg+3
! iout=j_o(iob)%i(io+2+narg)
! if(j_otype(iout).ne.j_ipreal)call j_del(iout)
! allocate(j_o(iout)%d(1:narg))
! allocate(j_o(iout)%i(0:narg))
! j_o(iout)%i=j_o(iob)%i(io+1:io+1+narg)
! do i=1,narg
! j_o(iout)%d(i)=j_v(j_o(iob)%i(io+1+i))
! end do !do i=1,narg
! j_otype(iout)=j_ipstore
! !io=io+narg+3
! return
! end subroutine store !subroutine store(iob,io)
 
! subroutine load(iob,io)
 
! !	io=io_
! narg=j_o(iob)%i(io+1)
! !	io_=io_+narg+3
! iout=j_o(iob)%i(io+2+narg)
! do i=1,narg
! if(j_otype(j_o(iob)%i(io+1+i)).ne.j_ipstore)then
! call j_printname('**trying to load ',j_o(iob)%i(io+1+i),' which is not a store')
! j_err=.true.
! return
! else !if(j_otype(j_o(iob)%i(io+1+i)).ne.j_ipstore)then
! istore=j_o(iob)%i(io+1+i)
! do j=1,j_o(istore)%i(0)
! j_v( j_o(istore)%i(j))=j_o(istore)%d(j)
! end do !do j=1,j_o(istore)%i(0)
! end if !if(j_otype(j_o(iob)%i(io+1+i)).ne.j_ipstore)then
! end do !do i=1,narg
! return
! end subroutine load !subroutine load(iob,io)
 
subroutine solve(iob,io)
	! Section solve solve() Solves a linear equation A*x=b
	! A linear matrix equation A*x=b can be solved for x with code\\
	! x=solve(A,b)
	! endheader
	! Note x=solve(A,b) is faster and more accurate than x=inverse(A)*b
	! endnote
	! Note solve works also if A and b are scalars. This is useful when
	! working with linear systems which start to grow from scalars.
	! endnote
	! endsection
 
 
	integer,dimension(:), pointer::arg=>null()
	double precision ,dimension(:,:), allocatable::mat,rhs_
	integer ,dimension(:), allocatable::ipiv
	integer::ndim,ndim2
	!	call j_startfunction(iob,io,j_ipmatrix,narg,arg,iout)
	call j_startfunction(iob,io,0,narg,arg,iout)
	if(j_err)return
	if(j_otype(arg(1)).eq.j_ipreal)then
		if(j_v(arg(1)).eq.j_0)then
			write(6,*)'first argument is scalar zero'
			j_err=.true.;return
		endif !if(j_v(arg(1)).eq.j_0)  12661
		if(j_otype(arg(2)).eq.j_ipreal)then
 
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=j_v(arg(2))/j_v(arg(1))
			return
		elseif(j_otype(arg(2)).eq.j_ipmatrix)then
			iout=j_defmatrix(iout,' ',j_nrows(arg(2)),j_nrows(arg(2)),j_matreg)
			j_o(iout)%d=j_o(arg(2))%d/j_v(arg(1))
			return
		end if !if(j_otype(arg(2)).eq.j_ipreal)  12665
	endif !if(j_otype(arg(1)).eq.j_ipreal)  12660
	itarg1=j_otype(arg(1))
	itarg2=j_otype(arg(2))
	if(itarg1.ne.j_ipmatrix.or.itarg2.ne.j_ipmatrix)then
		write(6,*)'arguments have types ',j_otypes(itarg1),j_otypes(itagr2),' should be MATRIX'
		j_err=.true.;return
	endif !if(itarg1.ne.j_ipmatrix.or.itarg2.ne.j_ipmatrix)  12678
	ndim=j_nrows(arg(1))  !j_o(arg(1))%i8(1)
	ndim2=j_ncols(arg(2))  ! j_o(arg(2))%i8(2)
	!		if(ndim.ne.j_o(arg(1))%i8(2).or.j_o(arg(2))%i8(1).ne.ndim)then
	if(ndim.ne.j_ncols(arg(1)).or.j_nrows(arg(2)).ne.ndim)then   !j_o(arg(2))%i8(1).ne.ndim)then
		write(6,*)'illegal dimensions ',j_nrows(arg(1)),j_ncols(arg(1)),j_nrows(arg(2)),j_ncols(arg(2))
		j_err=.true.
		return
	endif !if(ndim.ne.j_ncols(arg(1)).or.j_nrows(arg(2)).ne.ndim)  12685
	allocate (mat(ndim,ndim),rhs_(1:ndim,1:ndim2))
	allocate (ipiv(ndim))
	iel=0
	iel2=0
	do i=1,ndim
		do j=1,ndim
			iel=iel+1
			mat(i,j)=j_o(arg(1))%d(iel)
		enddo !j=1,ndim  12695
 
		do j=1,ndim2
			iel2=iel2+1
			rhs_(i,j)=j_o(arg(2))%d(iel2)
		enddo !j=1,ndim2  12700
	enddo !i=1,ndim  12694
	!  SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
	call dgesv(ndim,ndim2,mat,ndim,ipiv,rhs_,ndim,info_)
	if(info_.ne.0)then
		write(6,*)'info ',info_
		call j_printname('matrix ',arg(1), ' is singular')
		j_err=.true.
 
	else !if(info_.ne.0)then
		ivout=j_defmatrix(iout,' ',int8(ndim),int8(ndim2),j_matreg)
		iel=0
		do i=1,ndim
			do j=1,ndim2
				iel=iel+1
				j_o(iout)%d(iel)=rhs_(i,j)
			enddo !j=1,ndim2  12716
		enddo !i=1,ndim  12715
	endif !if(info_.ne.0)  12707
	deallocate(mat,ipiv,rhs_)
	return
end subroutine solve !subroutine solve(iob,io)
 
subroutine regr(iob,io)   !compute a linear regression model using linpack (%%stat)
	! Section regr regr() Linear regression
	! Ordinary or stepwise linear regrwession can be computed using regr().
	! endheader
 
	! Option
	! output&1&REGR& sRegression object..
	! arg& 1-N&LIST or REALV& y-variable and x-variables variables listing them
	! individually or given as a LIST.
	!@@data@
	! noint&-1|0& & noint-> implies that the model does not include intercept
	!step&-1|1 &REAL& t-value limit for stepwise regression. Regression variables are droped one-by-one
	! until the absolute value of t-value is at least as large as the limit given.
	!intercept is not considered.
	!var&-1|0& & if var-> is present regr() generated matrix ]output%var[ for
	!the variance-covariance matrix of the coeffcient estimates.
	!corr&-1|0& & if vcorr-> is present regr() generated matrix ]output%corr[ for
	!the correlation matrix of the coeffcient estimates. Standard deviations
	!are put to the diagonal.
	!variance&-1|1&CODE&The variance of the residual error is proportional to the function
	!given in this codeoption.
	!endoption
	! Note If the DATA contains variables Regr and Resid, then the values of
	! the regression function and residuals are put into these columns. Space for these e
	! coluns cab reserved with extra-> option in data() or in newdata()
	! endnote
	! Note If ]re[ is the output of the regr() then function re() can be used to compute
	! the value of the regression function. re() can contain from zero arguments up to the
	! total number of arguments as arguments. The rest of arguments get
	! the value they happen to have at the moment when the function is called.
	! endnote
	! Latex
	! Information from the REGR object can be obtained with the following functions.
	! let ]re[ be the name of the REGR object.
	! \begin{itemize}
	! \item   coef(]re[,xvar) = coefficient of variable xvar
	! \item   coef(]re[,xvar,any->) = returns zero if the variable is dropped from
	! the equation in the stepwise procedure of
	! due to linear dependencies.
	! \item  coef(]re[,1) or coef(]re(,$1) returns the intercept
	! \item  se(]re[,xvar) standard error of a coeffcient
	! \item  mse(]re[) MSE of the regression
	! \item  rmse(]re[) RMSE of the regression
	! \item 	r2(]re[) adjusted R2. If the intercept is not present this can be negative.
	! \item 	nobs(]re[) number of observations used
	! \item 	len(]re[) number of independent variables (including intercept) used
	! \end{itemize}
	!endlatex
	! endsection
 
	integer,dimension(:), pointer::arg=>null()
	!	integer,dimension(:), pointer::regrl=>null()
	integer ,dimension(:), allocatable::regl   !,ortonew
	double precision,dimension(:),allocatable::t
	double precision ,dimension(:,:), allocatable::mat
	integer ,dimension(:), allocatable::ipiv
	double precision ,dimension(:,:), allocatable::rhs_
	double precision ::sst,sse,sumy,sum,resid,weight,sumynw,sse0nw,ssenw,sstnw,sumw,vari
	double precision sse0,sst0 ,r2,r20
	double precision dfe,mse_,tmin,slow
	logical var,corr,step,singucur,singutot,getpar,isvariance
	integer*8 ::i
	character*6 form1,form2
 
	call j_startfunction(iob,io,0,narg,arg,iout);if(j_err)return !the argumenst are used here as real
	singutot=.false. ! are there altogether droppings
	getpar=j_linkoption(iob,io,j_mpar).ge.0
	if(j_err)return
	iy=arg(1) !   o(iob)%i(io+2)
	intcep=1
	linkvariance=j_codelink(iob,io,j_mvariance); if(j_err)return
	isvariance=linkvariance.gt.0
 
 
	!		if(isvariance)write(6,*)'hep ',j_o(iob)%i(linkvariance:linkvariance+4)
	call j_getoption(iob,io,j_mnoint,-1,0,0,&
		.false.,nnoint,j_optarg0);if(j_err)return
	if(nnoint.ge.0)intcep=0
	!j_getoption_name(iob,option,minarg,maxarg,iptype,expand,min0,noptarg,optarg)
 
	!	write(6,*)'iprint',iprint
	call j_getdataobject(iob,io);if(j_err)return
 
	call j_getoption(iob,io,j_mrow,-1,3,j_ipreal,.true.,nrow,j_optarg0)
	if(j_err)return
	if(nrow.gt.0)then
		if(nrow.ne.3)then
			write(6,*)'row needs 3 arguments';j_err=.true.;return
		endif !if(nrow.ne.3)  12812
		if1=j_v(j_optarg0(1))
		if2=j_v(j_optarg0(2))
		if3=j_v(j_optarg0(3))
		form1='(f0.0)'
		form1(3:3)=char(48+if1)
		form1(5:5)=char(48+if2)
		form2=form1
		form2(5:5)=char(48+if3)
 
	endif !if(nrow.gt.0)  12811
	!	isrow=j_isoption(iob,io,j_mrow)
	! call j_getoption(iob,io,j_mprint,-1,1,j_ipreal,&
	! .false.,iprint,j_optarg0)
	! if(iprint.eq.0)then
	! iprint=1
	! elseif(iprint.gt.0)then !if(iprint.eq.0)then
	! iprint=j_v(j_optarg0(1))
	! endif !if(iprint.eq.0)then
 
	if(j_err)return
	iregf=j_inlistobject(j_ivregf,j_divkeep)
	iresid=j_inlistobject(j_ivresid,j_divkeep)
	intcep2=intcep+1
	!ixns=j_inlistobject(ivnobsw,ivkeepc_)
 
	!	nindf=nind  !final number of independent variables
 
	if(intcep.eq.1)then
 
		ncoef=narg
 
	else !if(intcep.eq.1)then
		ncoef=narg-1
 
	endif !if(intcep.eq.1)  12842
	allocate(regl(1:ncoef))
	if(intcep.eq.1)then
		regl(1)=j_ivone
		regl(2:ncoef)=arg(2:narg)
	else !if(intcep.eq.1)then
		regl=arg(2:narg)
 
	endif !if(intcep.eq.1)  12851
 
	!	regl=>arg(2:narg) !    o(iob)%i(io+3:io+1+narg)
	!	ncoef=nind+intcep
	!x(iob,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
	call j_getoption(iob,io,j_mstep,-1,1,j_ipreal,&
		.true.,nstep,j_optarg0)
	if(j_err)return
	step=.false.
	tmin=0.
	if(nstep.ge.1)then
		step=.true.
		tmin=j_v(j_optarg0(1))
	endif !if(nstep.ge.1)  12867
 
	call j_getoption(iob,io,j_mvar,-1,0,0,&
		.false.,nvar,j_optarg0);if(j_err)return
	var=nvar.ge.0
	call j_getoption(iob,io,j_mcorr,-1,0,0,&
		.false.,nvar,j_optarg0);if(j_err)return
	corr=nvar.ge.0
	!call j_getdatasets(iob)
	!	write(6,*)'j_noptb',j_nopt,ncoef,ncoef1
	!!call j_clearoption(iob,io)  ! subroutine
	!	write(6,*)'aft',j_nopt
	ntotm=jnobstot  !0
	ncoef1=ncoef+1
	allocate(mat(1:ncoef,1:ncoef),rhs_(1:ncoef,1:ncoef1),ipiv(1:ncoef))
	allocate(t(1:ncoef))
	ncoef0=ncoef
	intcep2=intcep+1
		steploop:	do iste=1,ncoef

		mat=j_0
		rhs_=j_0
		do k=1,ncoef
			rhs_(k,k)=1.d0
		enddo !k=1,ncoef  12892
		ntot=0
		sumy=j_0
		missing=0
 
		!do k=1,jndatasetss
		!call j_getdataset(j_datasets(k),nobs)
		if(isvariance)then
			sumynw=j_0
			sumw=j_0
				obloopw:do i=j_dfrom,j_duntil
					!call j_nextobs()
				call j_getobs(i)
				if(j_err)return  !j_err
 
				if(j_rejected)cycle
				if(abs(j_v(iy)).ge.1.7d18)then
					missing=missing+1
					cycle
				endif !if(abs(j_v(iy)).ge.1.7d18)  12910
 
				do j=intcep2,ncoef
					if(abs(j_v(regl(j))).ge.1.7d18)then
						missing=missing+1
						cycle obloopw
					endif !if(abs(j_v(regl(j))).ge.1.7d18)  12916
				enddo !j=intcep2,ncoef  12915
 
 
				if(j_err)goto 900
				ntot=ntot+1
				! if(intcep.gt.0)then
				! mat(1,1)=mat(1,1)+1.d0
				! rhs_(1,ncoef1)=rhs_(1,ncoef1)+j_v(iy)
				! do j=2,ncoef
				! mat(1,j)=mat(1,j)+j_v(regl(j))
				! enddo !do j=2,ncoef
				! endif !if(intcep.gt.0)then
 
				vari=j_codevalue(iob,linkvariance)
				if(j_err)return
 
				!	if(i.le.3)write(6,*)'<66i',i,j_o(iob)%i(linkvariance:linkvariance+5),weight
				if(vari.le.j_0)then
					write(6,*)'for observation ',i,' variance was not positive'
					j_err=.true. ;return
				endif !if(vari.le.j_0)  12937
				weight=1.d0/sqrt(vari)
				do j=1,ncoef
					rhs_(j,ncoef1)=rhs_(j,ncoef1)+j_v(regl(j))*j_v(iy)/vari
 
					do j2=j,ncoef
 
						mat(j,j2)=mat(j,j2)+j_v(regl(j))*j_v(regl(j2))/vari
					enddo !j2=j,ncoef  12945
				enddo !j=1,ncoef  12942
				sumw=sumw+weight
				sumy=sumy+weight*j_v(iy)
				sumynw=sumynw+j_v(iy)
			enddo obloopw !oopw:do i=j_dfrom,j_duntil  12904
 
 
		else
				obloop1:do i=j_dfrom,j_duntil
					!call j_nextobs()
				call j_getobs(i)
				if(j_err)return  !j_err
 
				if(j_rejected)cycle
				if(abs(j_v(iy)).ge.1.7d18)then
					missing=missing+1
					cycle
				endif !if(abs(j_v(iy)).ge.1.7d18)  12963
 
				do j=intcep2,ncoef
					if(abs(j_v(regl(j))).ge.1.7d18)then
						missing=missing+1
						cycle obloop1
					endif !if(abs(j_v(regl(j))).ge.1.7d18)  12969
				enddo !j=intcep2,ncoef  12968
 
 
				if(j_err)goto 900
				ntot=ntot+1
				! if(intcep.gt.0)then
				! mat(1,1)=mat(1,1)+1.d0
				! rhs_(1,ncoef1)=rhs_(1,ncoef1)+j_v(iy)
				! do j=2,ncoef
				! mat(1,j)=mat(1,j)+j_v(regl(j))
				! enddo !do j=2,ncoef
				! endif !if(intcep.gt.0)then
 
				do j=1,ncoef
					rhs_(j,ncoef1)=rhs_(j,ncoef1)+j_v(regl(j))*j_v(iy)
 
					do j2=j,ncoef
 
						mat(j,j2)=mat(j,j2)+j_v(regl(j))*j_v(regl(j2))
					enddo !j2=j,ncoef  12989
				enddo !j=1,ncoef  12986
 
				sumy=sumy+j_v(iy)
			enddo obloop1 !oop1:do i=j_dfrom,j_duntil  12957
		endif !if(isvariance)  12901
		if(ntot.lt.ncoef)then
			write(6,*)'*only ',ntot,' observations accepted, estimation not possible'
			j_err=.true. ;return
		endif !if(ntot.lt.ncoef)  12998
 
		!	end do !do k=1,jndatasetss
		singucur=.false.
		do j=1,ncoef
			if(mat(j,j).lt.1.d-18)then
				call j_printname('always zero: ',regl(j),',dropping it')
				iimin=j
				singucur=.true.
				singutot=.true. ! dropping bussiness working like in step
				goto 445
			endif !if(mat(j,j).lt.1.d-18)  13006
			do j2=1,j-1
				mat(j,j2)=mat(j2,j)
				if(abs(mat(j,j2)/sqrt(mat(j,j)*mat(j2,j2))).gt.0.99999d0)then
					if(j2.eq.1.and.intcep2.gt.1)then
						call j_printname('no variation in ',regl(j),',dropping it')
					else !if(j2.eq.1.and.intcep2.gt.1)then
						call j_printname('variable ',regl(j),' is dropped,&
							because it is linearly related to ',regl(j2))
					endif !if(j2.eq.1.and.intcep2.gt.1)  13016
					iimin=j
					singutot=.true.
					singucur=.true.
					goto 445
				endif !if(abs(mat(j,j2)/sqrt(mat(j,j)*mat(j2,j2))).gt.0.99999d0)  13015
			enddo !j2=1,j-1  13013
		enddo !j=1,ncoef  13005
		call dgesv(ncoef,ncoef1,mat,ncoef0,ipiv,rhs_,ncoef0,info_)
		sse=j_0
		sst=j_0
		sse0=j_0
		sst0=j_0
		if(isvariance)then
			sumy=sumy/sumw
			sumynw=sumynw/ntot
			sstnw=j_0
			ssenw=j_0
			sse0nw=j_0
			sst0nw=j_0
		else
			sumy=sumy/ntot
		endif !if(isvariance)  13034
		ntot=0
		!	do k=1,jndatasetss
		!call j_getdataset(j_datasets(k),nobs)
		ibas=-j_dnkeep+(j_dfrom-1)*j_dnkeep    !dtas
			obloop2:		do i=j_dfrom,j_duntil
			ibas=ibas+j_dnkeep
			call j_getobs(i)
			if(j_err)return  !j_err
 
 
 
			if(j_rejected.or.abs(j_v(iy)).ge.1.7d18)then
				if(iregf.gt.0)j_o(j_divmat)%d(ibas+iregf)=j_0
				if(iresid.gt.0)j_o(j_divmat)%d(ibas+iresid)=j_0
				cycle
			endif !if(j_rejected.or.abs(j_v(iy)).ge.1.7d18)  13055
 
			do j=intcep2,ncoef
				if(abs(j_v(regl(j))).ge.1.7d18)cycle obloop2
			enddo !j=intcep2,ncoef  13061
 
 
 
			ntot=ntot+1
			sum=j_0
			!			if(intcep.gt.0)sum=rhs_(1,ncoef1)
 
			do j=1,ncoef
 
				sum=sum+rhs_(j,ncoef1)*j_v(regl(j))
			enddo !j=1,ncoef  13071
			resid=j_v(iy)-sum
 
			if(isvariance)then
				vari=j_codevalue(iob,linkvariance)
				sse=sse+resid*resid/vari
				sst=sst+(j_v(iy)-sumy)**2/vari
				!		sse0=sse0+resid*resid/vari
				ssenw=ssenw+resid*resid
 
				!		sst0=sst0+(j_v(iy)-sumy)**2
				sstnw=sstnw+(j_v(iy)-sumynw)**2
			else
				sse=sse+resid*resid
				sst=sst+(j_v(iy)-sumy)**2
			endif !if(isvariance)  13077
			if(iregf.gt.0)j_o(j_divmat)%d(ibas+iregf)=sum
			if(iresid.gt.0)j_o(j_divmat)%d(ibas+iresid)=resid
 
 
		enddo obloop2 !oop2:		do i=j_dfrom,j_duntil  13048
		!		end do !do k=1,jndatasetss
		dfe=ntot-ncoef
		idfe=ntot-ncoef
		mse_=sse/dfe
		rmse=sqrt(mse_)
 
		do j=1,ncoef
			t(j)=real(rhs_(j, ncoef1))/(sqrt(rhs_(j,j))*rmse)
		enddo !j=1,ncoef  13101
		if(step)then
 
			tmin0=10000.
			do ii=intcep2,ncoef
				if(abs(t(ii)).lt.tmin0)then
					iimin=ii !REGL
					tmin0=abs(t(ii))
				endif !if(abs(t(ii)).lt.tmin0)  13108
			enddo !ii=intcep2,ncoef  13107
 
			if(j_dprint.gt.1.and.tmin0.lt.tmin)call j_printname('dropping ',regl(iimin),' ')
 
			if(j_dprint.lt.2.and.tmin0.lt.tmin)goto 445
		endif !if(step)  13104
 
 
		write(6,*)' '
		if(iout.ne.j_ivresult)call j_printname('output regression object ',iout,' ')
 
		write(6,*)'Accepted ',ntot,' from ',j_dnobs
		if(missing.gt.0)write(6,*)'dependent was missing ',missing, ' times'
		j_v(j_ivaccepted)=ntot
		write(6,*)' '
		r2=1.d0-mse_*(ntot-1.)/sst
		if(nrow.gt.0)then
 
			call j_getname(iy)
			j_tempchar(1:j_loname+1)=j_oname(1:j_loname)//'='
			j_tempchar2(1:200)=' '
			lout=j_loname+1
			if(intcep.gt.0)then
				write(j_tempchar(lout+1:lout+if1),form1)rhs_(1,ncoef1)
 
 
				write(j_tempchar2(lout+1:lout+if1),form2)t(1)
				lout=lout+if1
			endif !if(intcep.gt.0)  13134
 
 
			do j=intcep2,ncoef
				!	t(j)=real(rhs_(j,ncoef1))/(sqrt(rhs_(j,j))*rmse)
				lout=lout+1
				if(rhs_(j,ncoef1).lt.j_0)then
					j_tempchar(lout:lout)='-'
				else
					j_tempchar(lout:lout)='+'
				endif !if(rhs_(j,ncoef1).lt.j_0)  13146
 
				write(j_tempchar(lout+1:lout+if1),form1)abs(rhs_(j,ncoef1))
				write(j_tempchar2(lout+1:lout+if1),form2)t(j)
				lout=lout+if1+1
				j_tempchar(lout:lout)='*'
				call j_getname(regl(j))
				j_tempchar(lout+1:lout+j_loname)=j_oname(1:j_loname)
				!write(6,718)j_object_name(regl(j),15),real(rhs_(j,ncoef1)),&
				lout=lout+j_loname
				!	sqrt(rhs_(j,j))*rmse,t(j)
 
			end do !j=intcep2,ncoef  13143
			j_tempchar(lout+1:lout+6)=' RMSE='
			j_tempchar2(lout+1:lout+6)='   R2='
			lout=lout+6
			write(j_tempchar(lout+1:lout+5),'(f5.3)')rmse
			write(j_tempchar2(lout+1:lout+5),'(f5.3)')r2
			lout=lout+5
			write(6,*)j_tempchar(1:lout)
			write(6,*)j_tempchar2(1:lout)
 
		else
			call j_getname(iy)
			write(6,*)'dependent variable ',j_oname(1:j_loname)  !j_object_name(iy,15)
			write(6,*)'     var           coef         se           t'
			if(intcep.gt.0)write(6,718)'Intercept       ',real(rhs_(1,ncoef1)),&
				sqrt(rhs_(1,1))*rmse,t(1)
	718 format(1x,a15,2g16.8e1,f11.2)
			do j=intcep2,ncoef
				!	t(j)=real(rhs_(j,ncoef1))/(sqrt(rhs_(j,j))*rmse)
				call j_getname(regl(j))
				write(6,718)j_oname(1:min(15,j_loname)),real(rhs_(j,ncoef1)),&
					sqrt(rhs_(j,j))*rmse,t(j)
 
			end do !j=intcep2,ncoef  13179
 
			write(6,*)' '
			write(6,'(a,g15.7,a,f8.5,a,i8)')'RMSE= ',rmse,' R2=',r2, ' df ',idfe
		endif !if(nrow.gt.0)  13128
		if(isvariance)then
			write(6,*)'*Note the se of the residual error is RMSE*sqrt(variance), where variance is defined in variance->'
			!	mse_=sse/dfe
			r20=1.d0-ssenw*(ntot-1.)/(sstnw*dfe)
			write(6,*)'rmse of unweighted residuals=',sqrt(ssenw/dfe),' R2 in the original scale =',r20
		endif !if(isvariance)  13190
		if(intcep.gt.0)then
			write(6,*)'F reg (',ncoef-1,idfe,')=',(sst-sse)/(mse_*(ncoef-1.))
		end if !if(intcep.gt.0)  13196
445	if(step.and.tmin0.lt.tmin.or.singucur)then
			if(iimin.lt.ncoef)then
				do ii=iimin,ncoef-1
					regl(ii)=regl(ii+1)
				enddo !ii=iimin,ncoef-1  13201
			endif !if(iimin.lt.ncoef)  13200
 
 
			ncoef=ncoef-1
			ncoef1=ncoef1-1
			cycle steploop
		endif !445	if(step.and.tmin0.lt.tmin.or.singucur)  13199
		exit steploop
 
	enddo steploop !ploop:	do iste=1,ncoef  12888
 
	! needs list which transfers original regressors to ne one
	if(iout.ne.j_ivresult)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		!		nind2=nind+intcep
		!	write(6,*)'ncoef ',ncoef
		allocate(j_o(iout)%i(0:ncoef+4),j_o(iout)%d(1:2*ncoef+4)) !coef_,se,rmse,mse,r2,f !const last
		j_o(iout)%d=0
		ivvar=0
		if(var)then
			ivvar=j_defmatrix(iout,'%var',int8(ncoef),int8(ncoef),j_matreg)
			it=0
			do i=1,ncoef
				do j=1,ncoef
					it=it+1
 
					j_o(ivvar)%d(it)=rhs_(i,j)*mse_  !laske
 
 
				enddo !j=1,ncoef  13227
			enddo !i=1,ncoef  13226
		endif !if(var)  13223
		ivcor=0
		if(corr)then
			ivcor=j_defmatrix(iout,'%corr',int8(ncoef),int8(ncoef),j_matreg)
			it=0
			do i=1,ncoef
				do j=1,ncoef
					it=it+1
					!		if(ortonew(i).ne.0.and.ortonew(j).ne.0)then
					if(i.eq.j)then
						j_o(ivcor)%d(it)=1.d0 !=rmse*sqrt(rhs_(i,j))  !laske
					else !if(i.eq.j)then
						j_o(ivcor)%d(it)=rhs_(i,j)/sqrt(rhs_(i,i)*rhs_(j,j))
					endif !if(i.eq.j)  13244
					!		endif
				enddo !j=1,ncoef  13241
			enddo !i=1,ncoef  13240
		endif !if(corr)  13237
 
		j_o(iout)%i(0)=ncoef
		j_o(iout)%i(1:ncoef)=regl(1:ncoef)
		j_o(iout)%d(1:ncoef)=rhs_(1:ncoef,ncoef1)
		do j=1,ncoef
			j_o(iout)%d(ncoef+j)=sqrt(rhs_(j,j))*rmse
		enddo !j=1,ncoef  13257
 
		j_o(iout)%i(ncoef+1)=ntot
		j_o(iout)%i(ncoef+2)=ivvar  ! reserved for variance covariance matrixntot
		j_o(iout)%i(ncoef+3)=ivcor  ! reserved for correlations
		j_o(iout)%i(ncoef+4)= 0 !reserved for ivtrans in nonlinear regression
 
		j_o(iout)%d(2*ncoef+1)=SQRT(SSE/dfe)
		j_o(iout)%d(2*ncoef+2)=SSE/dfe
		j_o(iout)%d(2*ncoef+3)=r2
		j_o(iout)%d(2*ncoef+4)=9.9  ! p-value
		j_otype(iout)=j_ipregr
		!		write(6,*)'aa',ncoef0,size(j_o(iout)%i)
		!		write(6,*)j_o(iout)%i(0:ncoef0)
		!		write(6,*)j_o(iout)%d
		if(getpar)then
			!call j_deflist2(ivout,'%keep',keepv(0:nkeep),ivkeep)
			! 	call j_deflist2(ivout,'%vars',j_o(ivkeep)%i(0:nkeep),ivvars)
			iv=j_deflist(iout,'%xvars',listold=j_o(iout)%i(0:ncoef))
			!call j_deflist2(iout,'%xvars',j_o(iout)%i(0:ncoef),iv)
			iv=j_defmatrix(iout,'%coef',int8(ncoef),j_18,j_matreg)
			j_o(iv)%d=j_o(iout)%d(1:ncoef)
			iv=j_defmatrix(iout,'%se',int8(ncoef),j_18,j_matreg)
			j_o(iv)%d=j_o(iout)%d(ncoef+1:2*ncoef)
 
		endif !if(getpar)  13274
 
	endif !if(iout.ne.j_ivresult)  13216
 
	!		allocate(mat(1:ncoef,1:ncoef),rhs_(1:ncoef,ncoef1),ipiv(1:ncoef))
	!	allocate(t(1:ncoef))
	deallocate(mat,rhs_,ipiv,t,regl)
	write(6,*)' '
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
	if(step)write(6,*)'********************************'
	return
	900   write(6,*)'*regr: error in transformations'
	deallocate(mat,rhs_,ipiv,t,regl)
	return
end subroutine !subroutine regr(iob,io)
 
subroutine regpar(iob,io,ity) !
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	select case(j_otype(irg))
	case(j_ipregr) !select case(j_otype(irg))
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	j_v(iout)=j_o(irg)%d(2*j_o(irg)%i(0)+ity)
	case default !select case(j_otype(irg))
	call j_printname('**not a regression obj:',irg,' ')
	j_err=.true.
	end select !select case(j_otype(irg))
	!	io=io+narg+3
	return
end subroutine !subroutine regpar(iob,io,ity)
 
subroutine coefse(iob,io,ity)  !ity=0 coef, ity=1 se coef(iob,io)
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	inde=j_o(iob)%i(io+3)  !second argument
	iout=j_o(iob)%i(io+2+narg)
	select case(j_otype(irg))
	case(j_ipregr) !select case(j_otype(irg))
	nvar=j_o(irg)%i(0)
	!write(6,*)'nvar',nvar
	if(inde.eq.j_ivall)then
		iout=j_defmatrix(iout,' ',int8(nvar),j_18,j_matreg)
		j_o(iout)%d(1:nvar)=j_o(irg)%d(1+ity*nvar:nvar+ity*nvar)
		return
	endif !if(inde.eq.j_ivall)  13327
 
 
	do i=1,nvar
		if(j_o(irg)%i(i).eq.inde)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=j_o(irg)%d(i+ity*j_o(irg)%i(0))
			return
 
		endif !if(j_o(irg)%i(i).eq.inde)  13335
	end do !i=1,nvar  13334
	i=j_v(inde)
	if(i.gt.0.and.i.le.nvar)then
		j_v(iout)=j_o(irg)%d(i+ity*j_o(irg)%i(0))
		return
	endif !if(i.gt.0.and.i.le.nvar)  13343
	call j_getname(inde)
	write(6,*)j_oname(1:j_loname),' is not argument or valid index between 1 and ',nvar
 
	j_err=.true.
	case default !select case(j_otype(irg))
	call j_printname('**not legal obj for param(obj, ):',irg,' ')
	j_err=.true.
 
	end select !select case(j_otype(irg))
 
 
	return
end subroutine coefse !subroutine coefse(iob,io,ity)
 
 
 
subroutine nonlin(iob,io)   !compute a linear regression model using linpack (%%stat)
	! Section nonlin nonlin() Nonlinear regression
	! To be raported later, see old manual
	! endsection
 
	!loglike sum y*log(p)+(1-y)*log(1-p)
 
 
	integer,dimension(:), pointer::arg=>null()
	integer,dimension(:), pointer::par=>null()
	integer,dimension(:), pointer::initial=>null()
	integer,dimension(:), allocatable ::der
	double precision,dimension(:), allocatable::parmin
	double precision,dimension(:), allocatable::parmax
	double precision,dimension(:), allocatable::dpar
	double precision ,dimension(:,:), allocatable::mat
	integer ,dimension(:), allocatable::ipiv
	double precision ,dimension(:), allocatable::current
	double precision ,dimension(:,:), allocatable::rhs_
	double precision ::sst,sse,sumy,sse0,sumy2,sse0nw,ssenw
	double precision dfe,mse_,resid,rmse,rmse0
	double precision slow,tole,stepmin
	double precision scale,scalemin1,scalemin2,step,sumynw,sumy2nw,weight
	double precision sumf,sumnf,loglike,sumfnw
	double precision,dimension (:),allocatable ::variance
	logical eka
	logical var,corr
	logical isparmin,isparmax,isdpar,ismissing,isvariance,isloglike
	integer iprint
	integer*8 ::i
	corr=.false.
	eka=.true.
 
	call j_startfunction(iob,io,0,narg,arg,iout)
	if(j_err)return
	call j_getdataobject(iob,io)
	if(j_err)return
	if(.not.j_distrans)then
		write(6,*)'nonlin needs trans->'
		j_err=.true.;return
	endif !if(.not.j_distrans)  13400
	ismissing=j_isoption(iob,io,j_mmissing)
 
	iregf=j_inlistobject(j_ivregf,j_divkeep)
	iresid=j_inlistobject(j_ivresid,j_divkeep)
	!	write(6,*)'iregf,iresid',iregf,iresid
	linkvariance=j_codelink(iob,io,j_mvariance); if(j_err)return
	isvariance=linkvariance.gt.0
 
	linkloglike=j_codelink(iob,io,j_mloglike); if(j_err)return
	isloglike=linkloglike.gt.0
 
	iy=arg(1) !   o(iob)%i(io+2)
	ivfunc=arg(2)
 
	call j_getoption(iob,io,j_mpar,1,9999,j_ipreal,.true.,ncoef,par);if(j_err)return
	call j_getoption(iob,io,j_minitial,-1,9999,j_ipreal,.true.,ncoef1,initial);if(j_err)return
 
 
	call j_getoption(iob,io,j_mmaxiter,-1,1,j_ipreal,.true.,narg,arg);if(j_err)return
	maxiter=100
	if(narg.gt.0)maxiter=j_v(arg(1))
	call j_getoption(iob,io,j_mslow,-1,1,j_ipreal,.true.,narg,arg);if(j_err)return
	slow=-0.1
	if(narg.gt.0)slow=j_v(arg(1))
	call j_getoption(iob,io,j_mtole,-1,1,j_ipreal,.true.,narg,arg);if(j_err)return
	tole=0.1
	if(narg.gt.0)tole=j_v(arg(1))
 
 
 
	! iprint=1
	! call j_getoption(iob,io,j_mprint,-1,1,j_ipreal,.true.,narg,arg)
	! if(narg.gt.0)iprint=j_v(arg(1))
	call j_getoption(iob,io,j_mvar,-1,1,j_ipreal,.false.,narg,arg);if(j_err)return
	var=narg.ge.0
	call j_getoption(iob,io,j_mcorr,-1,1,j_ipreal,.false.,narg,arg);if(j_err)return
	corr=narg.ge.0
	if(j_err)return
 
	if(ncoef1.gt.0)then
		if(ncoef.ne.ncoef1)then
 
			write(6,*)'the number of initial values ',ncoef1, ' should be equal to the number parameters ',ncoef
			j_err=.true. ;return
		endif !if(ncoef.ne.ncoef1)  13444
 
		j_v(par(1:ncoef1))=j_v(initial(1:ncoef1))
	endif !if(ncoef1.gt.0)  13443
	write(6,*)'intial values:'
	if(j_printvar(6,ncoef,par(1:ncoef)))continue
 
 
	nnz=0
	do ii=1,ncoef
		if(j_v(par(ii)).ne.j_0)nnz=nnz+1
	enddo !ii=1,ncoef  13457
	if(nnz.eq.0)then
		write(6,*)'some initial values should be nonzero'
		j_err=.true.
		return
	endif !if(nnz.eq.0)  13460
 
 
	if(allocated(der))deallocate(der)
	allocate(der(1:ncoef))
	call j_getder(ivfunc,par(1:ncoef),der,ncoef,old=.true.)
	if(j_err)return
 
	call j_getoption(iob,io,j_mparmin,-1,ncoef,j_ipreal,.true.,narg,arg);if(j_err)return
 
	if(j_err)return
	isparmin=.false.
	if(narg.gt.0.)then
		if(narg.ne.ncoef)then
			write(6,*)'nonlin: number of minimums should be ',ncoef,' it is ',narg
			j_err=.true.
			return
		else !if(narg.ne.ncoef)then
			allocate(parmin(1:ncoef))
			parmin=j_v(arg(1:ncoef))
			isparmin=.true.
		endif !if(narg.ne.ncoef)  13477
	endif !if(narg.gt.0.)  13476
	call j_getoption(iob,io,j_mparmax,-1,ncoef,j_ipreal,.true.,narg,arg)
	if(j_err)return
	isparmax=.false.
	if(narg.gt.0)then
		if(narg.ne.ncoef)then
			write(6,*)'nonlin: number of maximums should be ',ncoef,' it is ',narg
			j_err=.true.
			return
		else !if(narg.ne.ncoef)then
			allocate(parmax(1:ncoef))
			parmax=j_v(arg(1:ncoef))
			isparmax=.true.
		endif !if(narg.ne.ncoef)  13491
	endif !if(narg.gt.0)  13490
	call j_getoption(iob,io,j_mdpar,-1,ncoef,j_ipreal,.true.,narg,arg)
	if(j_err)return
	isdpar=.false.
	if(narg.gt.0)then
		if(narg.ne.ncoef)then
			write(6,*)'nonlin: length of dpar should be ',ncoef,' it is ',narg
			j_err=.true.
			return
		else !if(narg.ne.ncoef)then
			allocate(dpar(1:ncoef))
			dpar=j_v(arg(1:ncoef))
			isdpar=.true.
		endif !if(narg.ne.ncoef)  13505
	endif !if(narg.gt.0)  13504
	step=1
	call j_getoption(iob,io,j_mstep,-1,1,j_ipreal,.true.,narg,arg);if(j_err)return
	if(narg.gt.0)stepmin=j_v(arg(1))
	ncoef1=ncoef+1
	! litr=j_linkoption(iob,io,j_mtrans)
 
	! ivtrans=j_o(iob)%i(litr+1)  !needed to be stored in the output object
	! endif !if(litr.eq.0)then
	!call j_getdatasets(iob)
	!!call j_clearoption(iob,io)  ! subroutine
	! if(jndatasetss.le.0)then
	! write(6,*)'*nonlin: no data available'
	! j_err=.true.
	! return
 
	! endif !if(jndatasetss.le.0)then
	ntotm=j_dnobs !0
 
	allocate(mat(1:ncoef,1:ncoef),rhs_(1:ncoef,ncoef1),ipiv(1:ncoef),current(1:ncoef))
	loop=0
	sumy=j_0
	sumy2=j_0
	sumynw=j_0
	sumy2nw=j_0
	iter=0
	sumf=j_0
	if(isvariance)then
		if(allocated(variance))then
			if(size(variance).lt.j_duntil)deallocate(variance)
		endif !if(allocated(variance))  13542
		if(.not.allocated(variance))allocate(variance(1:j_duntil))
	endif !if(isvariance)  13541
	100 continue
	if(iter.ge.maxiter)then
		if(j_dprint.gt.1)write(6,*)'maximum number of iterations ',iter
		goto 200
	endif !if(iter.ge.maxiter)  13548
	loop=loop+1
	mat=j_0
	rhs_=j_0
	do k=1,ncoef
		rhs_(k,k)=1.d0
	enddo !k=1,ncoef  13555
 
	ntot=0
	sse0=j_0
	sse0nw=j_0
	missing=0
	loglike=j_0
	!	do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
	if(ismissing.and.eka)then
		ibas=0
		do i=1,j_dfrom-1
			if(iregf.gt.0)j_o(j_divmat)%d(ibas+iregf)=1.7d19
			if(iresid.gt.0)j_o(j_divmat)%d(ibas+iresid)=1.7d19
 
			ibas=ibas+j_dnkeep
 
		enddo !i=1,j_dfrom-1  13568
		ibas=ibas-j_dnkeep
	else
		ibas=(j_dfrom-2)*j_dnkeep
 
	endif !if(ismissing.and.eka)  13566
 
 
 
	do i=j_dfrom,j_duntil
		ibas=ibas+j_dnkeep
		call j_getobs(i)
		if(j_err)return  !j_err
 
		!	if(j_rejected)then
		!if(
		!cycle
		!	endif
		if(abs(j_v(iy)).ge.1.7d18)then
			missing=missing+1
			if(eka)then
				if(iregf.gt.0)j_o(j_divmat)%d(ibas+iregf)=1.7d19
				if(iresid.gt.0)j_o(j_divmat)%d(ibas+iresid)=1.7d19
 
			endif !if(eka)  13594
			cycle
		elseif(j_rejected)then
			missing=missing+1
			if(eka)then
				if(ismissing.and.iregf.gt.0)j_o(j_divmat)%d(ibas+iregf)=1.7d19
				if(ismissing.and.iresid.gt.0)j_o(j_divmat)%d(ibas+iresid)=1.7d19
 
			endif !if(eka)  13602
			cycle
		endif !if(abs(j_v(iy)).ge.1.7d18)  13592
 
		ntot=ntot+1
 
		resid=j_v(iy)-j_v(ivfunc)
 
		if(isvariance)then
			variance(i)=j_codevalue(iob,linkvariance)
			if(j_err)return
			!	if(i.le.5)write(6,*)'<77',i,variance,j_v(ivfunc),j_v(iy),j_v(der(1:ncoef))
			if(variance(i).le.j_0)then
				write(6,*)'for observation ',i,' variance',variance(i),' was not positive'
				j_err=.true. ;return
			endif !if(variance(i).le.j_0)  13618
			weight=j_1/sqrt(variance(i))
			sumy=sumy+weight*j_v(iy)
			sumf=sumf+weight*j_v(ivfunc)
			sumfnw=sumfnw+j_v(ivfunc)
			sumy2=sumy2+j_v(iy)*j_v(iy)/variance(i)
 
			if(eka)then
				sumynw=sumynw+j_v(iy)
				sumy2nw=sumy2nw+j_v(iy)*j_v(iy)
			endif !if(eka)  13628
			do j=1,ncoef
				rhs_(j,ncoef1)=rhs_(j,ncoef1)+j_v(der(j))*resid/variance(i)
 
				do j2=j,ncoef
 
					mat(j,j2)=mat(j,j2)+j_v(der(j))*j_v(der(j2))/variance(i)
				enddo !j2=j,ncoef  13635
			enddo !j=1,ncoef  13632
			sse0nw=sse0+resid*resid
			sse0=sse0+resid*resid/variance(i)
		else
			if(eka)then
				sumy=sumy+j_v(iy)
				sumy2=sumy2+j_v(iy)*j_v(iy)
			endif !if(eka)  13643
			do j=1,ncoef
				rhs_(j,ncoef1)=rhs_(j,ncoef1)+j_v(der(j))*resid !(j_v(iy)-j_v(ivfunc))
 
				do j2=j,ncoef
 
					mat(j,j2)=mat(j,j2)+j_v(der(j))*j_v(der(j2))
				enddo !j2=j,ncoef  13650
			enddo !j=1,ncoef  13647
			sse0=sse0+resid*resid
			sumf=sumf+j_v(ivfunc)
			!	end do !do k=1,jndatasetss
		endif !if(isvariance)  13614
		!	resid=j_v(iy)-j_v(ivfunc)
		!	sse0nw=sse0+resid*resid
		if(isloglike)loglike=loglike+j_codevalue(iob,linkloglike)
		if(j_err)return
		if(iregf.gt.0)j_o(j_divmat)%d(ibas+iregf)=j_v(ivfunc)
		if(iresid.gt.0)j_o(j_divmat)%d(ibas+iresid)=resid
	end do !i=j_dfrom,j_duntil  13583
 
	if(ismissing.and.eka)then
		!write(6,*)'dunt',j_duntil+1,j_o(j_divmat)%i(1)
		do i=j_duntil+1,j_o(j_divmat)%i(1)
			ibas=ibas+j_dnkeep
			if(iregf.gt.0)j_o(j_divmat)%d(ibas+iregf)=1.7d19
			if(iresid.gt.0)j_o(j_divmat)%d(ibas+iresid)=1.7d19
 
			ibas=ibas+j_dnkeep
 
		enddo !i=j_duntil+1,j_o(j_divmat)%i(1)  13669
	endif !if(ismissing.and.eka)  13667
 
	do j=1,ncoef
		do j2=1,j-1
			mat(j,j2)=mat(j2,j)
		enddo !j2=1,j-1  13680
	enddo !j=1,ncoef  13679
	!		write(6,*)'mat',mat
	!	write(6,*)'rhs_',rhs_
	call dgesv(ncoef,ncoef1,mat,ncoef,ipiv,rhs_,ncoef,info_)
	!	write(6,*)'add',rhs_(1:ncoef,ncoef1)
	current=j_v(par(1:ncoef))
	rmse0=sqrt(sse0/(ntot-ncoef))
	if(eka)then
		if(j_dprint.gt.1)then
			!			write(6,*)' '
			!		write(6,*)'par0 ', j_v(par(1:ncoef))
			write(6,*)'Initially:'
			if(isvariance)then
				write(6,*)'Unweighted: rmse',sqrt(sse0nw/(ntot-ncoef)),' mean of y   ',sumynw/ntot,' mean of func ',sumfnw/ntot
 
				write(6,*)'  Weighted: rmse',rmse0,' mean of w-y ',sumy/ntot,' mean of func ',sumf/ntot
			else
				write(6,*)'rmse',rmse0,' mean of y ',sumy/ntot,' mean of func ',sumf/ntot
			endif !if(isvariance)  13695
			if(isloglike)write(6,*)'log likelihood ',loglike
		endif !if(j_dprint.gt.1)  13691
		!	eka=.false.
	endif !if(eka)  13690
	scale=step/0.6     ! j changed  6.10.2020   was 1/0.6
	iter2=0
	10 scale=scale*0.6d0
	iter=iter+1
	iter2=iter2+1
	if(iter.gt.maxiter)then
		j_v(par(1:ncoef))=current
		if(j_dprint.gt.1)write(6,*)'maximum number of iterations (maxiter->) ',iter
		sse=sse0
		j_v(par(1:ncoef))=current
		goto 200
	endif !if(iter.gt.maxiter)  13711
	if(iter2.gt.8)then
		j_v(par(1:ncoef))=current
		if(j_dprint.gt.1)write(6,*)'maximum number of inner looop iterations ',iter2
		sse=sse0
		!		j_v(par(1:ncoef))=current
		goto 200
	endif !if(iter2.gt.8)  13718
	scalemin1=scale
	if(isparmin)then
		do i=1,ncoef
			if(j_v(par(i)).lt.parmin(i))then
				! current+scale*rhs=min ->
				scalemin1=	min((parmin(i) -current(i))/rhs_(i,ncoef1),scalemin1)
				if(j_dprint.gt.1) write(6,*)'parameter ',i,' constrained by minimum ',	parmin(i),' newscale=',scalemin1
			endif !if(j_v(par(i)).lt.parmin(i))  13728
		enddo !i=1,ncoef  13727
 
	endif !if(isparmin)  13726
	scalemin2=scale
	if(isparmax)then
		do i=1,ncoef
			if(j_v(par(i)).gt.parmax(i))then
				! current+scale*rhs=max ->
				scalemin2=	min((parmax(i) -current(i))/rhs_(i,ncoef1),scalemin1,scalemin2)
				if(j_dprint.gt.1) write(6,*)'parameter ',i,' constrained by minimum ',	parmax(i),'newscale=',scalemin2
			endif !if(j_v(par(i)).gt.parmax(i))  13739
		enddo !i=1,ncoef  13738
 
	endif !if(isparmax)  13737
	!	cscale=scale
	if(scalemin1.lt.scale.or.scalemin2.lt.scale)then
		scale=min(scalemin1,scalemin2)
		if(j_dprint.gt.1)write(6,*)'new scale =',scale
	endif !if(scalemin1.lt.scale.or.scalemin2.lt.scale)  13748
 
	do i=1,ncoef
		if(scale*abs(rhs_(i,ncoef1)).gt.tole*sqrt(rhs_(i,i))/100.)goto 19
 
	enddo !i=1,ncoef  13753
	if(j_dprint.gt.1) write(6,*)'potential changes in parameters are smaller than tolerance'
	sse=sse0
	j_v(par(1:ncoef))=current
	goto 200
	19 continue
	if(isdpar)then
		do i=1,ncoef
			if(abs(rhs_(i,ncoef1)).gt.dpar(i))scale=dpar(i)/abs(rhs_(i,ncoef1))
		enddo !i=1,ncoef  13763
 
	endif !if(isdpar)  13762
 
 
	j_v(par(1:ncoef))=current+scale*rhs_(1:ncoef,ncoef1)
	sse=j_0
	ntot=0
	ssenw=j_0
	sumf=j_0
	if(isvariance)then
		sumy=j_0
		sumy2=j_0
	endif !if(isvariance)  13775
	sumfnw=j_0
	!	do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
 
	do i=j_dfrom,j_duntil
 
		call j_getobs(i)
 
		if(j_err)return
		if(j_rejected)then
			if(eka)then
				if(ismissing)then
					iba=(i-1)*j_dnkeep
					if(iregf.gt.0)j_o(j_divmat)%d(iba+iregf)=1.7d19
					if(iresid.gt.0)j_o(j_divmat)%d(iba+iresid)=1.7d19
 
 
				endif !if(ismissing)  13790
 
			endif !if(eka)  13789
			cycle
 
		endif !if(j_rejected)  13788
		if(abs(j_v(iy)).ge.1.7d18)cycle
		ntot=ntot+1
		resid=j_v(iy)-j_v(ivfunc)
		if(isvariance)then
			!if(i.le.15)write(6,*)'<88',i,variance,j_v(ivfunc),j_v(iy)
			!	variance=j_codevalue(iob,linkvariance)
			weight=j_1/sqrt(variance(i))
			! if(variance.le.j_0)then
			! write(6,*)'VARIA',i,variance
			! j_err=.true.
			! return
			! endif
			sse=sse+resid*resid/variance(i)  !(j_v(iy)-j_v(ivfunc))**2/variance
			ssenw=ssenw+resid*resid !(j_v(iy)-j_v(ivfunc))**2
			sumf=sumf+weight*j_v(ivfunc)
			sumfnw=sumfnw+j_v(ivfunc)
			sumy=sumy+weight*j_v(iy)
			sumy2=sumy2+j_v(iy)*j_v(iy)/variance(i)
		else
			sse=sse+resid*resid ! (j_v(iy)-j_v(ivfunc))**2
			sumf=sumf+j_v(ivfunc)
		endif !if(isvariance)  13805
	end do !i=j_dfrom,j_duntil  13783
	eka=.false.
	!	end do !do k=1,jndatasetss
	if(sse.gt.sse0)goto 10   ! no improvement, decrease step
	rmse=sqrt(sse/(ntot-ncoef))
 
	if(j_dprint.gt.1)then
		write(6,*)' '
		write(6,*)'par ',j_v(par(1:ncoef))
		if(isvariance)then
			write(6,*)'Unweighted: rmse ',sqrt(sse0nw/(ntot-ncoef)),' mean of y   ',sumynw/ntot,' mean of func ',sumfnw/ntot
 
			write(6,*)'  Weighted: rmse ',rmse0,' mean of w-y ',sumy/ntot,' mean of func ',sumf/ntot
		else
			write(6,*)'rmse',rmse,' mean of y ',sumy/ntot,' mean of func ',sumf/ntot
		endif !if(isvariance)  13833
		if(isloglike)write(6,*)'loglikelihood ',loglike
		! if(isvariance)then
		! write(6,*)'unweighted: rmse',sqrt(sse0nw/dfe),' mean of y ',sumynw/ntot, 'mean of func ',sumfnw/ntot, &
		! ' Weighted: rmse0 ',rmse,' mean of w-y ',sumy/ntot,' mean of func ',sumf/ntot
		! else
		! write(6,*)'rmse',rmse,' mean of y ',sumy/ntot,' mean of func ',sumf/ntot
		! endif
		write(6,*)' step ',scale
		if(isvariance)write(6,*)' unweighted rmse ',	sqrt(ssenw/(ntot-ncoef))
	endif !if(j_dprint.gt.1)  13830
	if(slow.lt.0.)then
		perc=100.*(sse0-sse)/sse0
		if(perc.gt.abs(slow))goto 100
		if(j_dprint.gt.1)write(6,*)'improvement % in sse ',perc, ' < ',abs(slow), ' def or negative slow->'
	else !if(slow.lt.0.)then
		if(rmse0-rmse.gt.slow)goto 100
		if(j_dprint.gt.1)write(6,*)'improvement of rmse ',rmse0-rmse,' <  ',slow, ' positive slow->'
	endif !if(slow.lt.0.)  13850
 
	200 dfe=ntot-ncoef
	idfe=dfe
	mse_=sse/dfe
	!		write(6,*)'ssehere ',sse,ssew
	rmse=sqrt(mse_)
	if(j_dprint.gt.0)then
		write(6,*)' '
		call j_getname(iy)
		write(6,*)'dependent variable ',j_oname(1:j_loname)  !j_object_name(iy,15)
		write(6,*)'Accepted ',ntot,' out of ',dnobs
		j_v(j_ivaccepted)=ntot
		write(6,*)'     param          value         se           t'
 
		718 format(1x,a15,2g13.5e1,f8.2)
		do j=1,ncoef
			call j_getname(par(j))
			write(6,718)j_oname(1:min(j_loname,15)),j_v(par(j)),&
				sqrt(rhs_(j,j))*rmse,j_v(par(j))/(sqrt(rhs_(j,j))*rmse)
 
		end do !j=1,ncoef  13873
	endif !if(j_dprint.gt.0)  13864
	r2=1.-mse_*(ntot-1.)/((sumy2-ntot*(sumy/ntot)**2))
	if(j_dprint.gt.0)then
		!	mse_=sse/dfe
		write(6,*)' '
589	format(a,g14.6,a,f7.4,a,i7)
		if(isvariance)then
			write(6,589)' Weighted RMSE= ',rmse,' R2=',r2, ' df ',idfe
			write(6,589)'Unweighted RMSE=',sqrt(ssenw/(ntot-ncoef)),&
				' R2=',j_1-ssenw*(ntot-j_1)/(dfe*(sumy2nw-ntot*(sumynw/ntot)**2))
		else
			write(6,589)'RMSE= ',rmse,' R2=',r2, ' df ',idfe
		endif !if(isvariance)  13885
	endif !if(j_dprint.gt.0)  13881
	if(isloglike)then
		write(6,*)'loglikehilihood ',loglike
		write(6,*)' '
	endif !if(isloglike)  13893
	if(iout.ne.j_ivresult)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
		allocate(j_o(iout)%i(0:ncoef1+3),j_o(iout)%d(1:2*ncoef+4)) !coef_,se,rmse,mse,r2,f const last
		j_o(iout)%i(0)=ncoef
		j_o(iout)%d(1:ncoef)=j_v(par(1:ncoef))
		j_o(iout)%i(1:ncoef)=par(1:ncoef)
		do i=1,ncoef
 
			j_o(iout)%d(i+ncoef)=sqrt(rhs_(i,i))*rmse
 
		end do !i=1,ncoef  13904
		ivvar=0
		if(var)then
			ivvar=j_defmatrix(iout,'%var',int8(ncoef),int8(ncoef),j_matreg)
			it=0
			do i=1,ncoef
				do j=1,ncoef
					it=it+1
					j_o(ivvar)%d(it)=rhs_(i,j)*mse_  !laske
				enddo !j=1,ncoef  13914
			enddo !i=1,ncoef  13913
		endif !if(var)  13910
		ivcor=0
		if(corr)then
			ivcor=j_defmatrix(iout,'%corr',int8(ncoef),int8(ncoef),j_matreg)
			it=0
			do i=1,ncoef
				do j=1,ncoef
					it=it+1
					if(i.eq.j)then
						j_o(ivcor)%d(it)=rmse*sqrt(rhs_(i,j))  !laske
					else !if(i.eq.j)then
						j_o(ivcor)%d(it)=rhs_(i,j)/sqrt(rhs_(i,i)*rhs_(j,j))
					endif !if(i.eq.j)  13927
				enddo !j=1,ncoef  13925
			enddo !i=1,ncoef  13924
		endif !if(corr)  13921
		j_o(iout)%i(ncoef1)=ntot
		j_o(iout)%i(ncoef1+1)=ivvar
		j_o(iout)%i(ncoef1+2)=ivcor
		j_o(iout)%i(ncoef1+3)=ivtrans
		j_o(iout)%d(2*ncoef+1)=SQRT(SSE/dfe)
		j_o(iout)%d(2*ncoef+2)=SSE/dfe
		j_o(iout)%d(2*ncoef+3)=r2
		j_o(iout)%d(2*ncoef+4)=9.9  ! p-value
		j_otype(iout)=j_ipregr
 
	endif !if(iout.ne.j_ivresult)  13897
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
	return
	900   write(6,*)'*regr: error in transformations'
	return
end subroutine !subroutine nonlin(iob,io)
 
recursive subroutine value_(iob,io)  ! Jlp22-function value(
 
	double precision dapu,arg,arg2
	real, dimension (:),allocatable:: vout
	logical isz,isprint
	equivalence (rw,iw)
	call j_checkoutput(iob,io)
	if(j_err)return
	isprint=j_linkoption(iob,io,j_mprint).ge.0
 
	!	io=io_
 
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	ifunc=j_o(iob)%i(io+2)  !first argument
	if(j_otype(ifunc).eq.j_iplist)then
		ivindex=j_igetopt(iob,io,j_mindex)
		if(ivindex.le.0)then
			write(6,*)'** value, first argument is list => there must be index->'
			j_err=.true.
			!	!!call j_clearoption(iob,io)  ! subroutine
			return
		endif !if(ivindex.le.0)  13972
		inde=j_v(ivindex)
		if(inde.le.0.or.inde.gt.j_o(ifunc)%i(1))then
			write(6,*)'**value, index-> has illegal value ',inde, 'max is ',j_o(ifunc)%i(1)
			j_err=.true.
			return
		endif !if(inde.le.0.or.inde.gt.j_o(ifunc)%i(1))  13979
		ifunc=j_o(ifunc)%i2(inde)
	endif !if(j_otype(ifunc).eq.j_iplist)  13970
	iarg=j_o(iob)%i(io+3)
	select case(j_otype(ifunc) )
	case (j_ipsmooth) !select case(j_otype(ifunc) )
	! smooth
	!need wrk only for 2*m
	j_v(iout)=j_valuesspl(ifunc,real(j_v(iarg)))
	!!call j_clearoption(iob,io)  ! subroutine
	case(j_ipmatrix) !select case(j_otype(ifunc) )
	nval=j_o(ifunc)%i(2)
	arg=j_v(iarg)
	select case(j_o(ifunc)%i(1) )
	case(1) !select case(j_o(ifunc)%i(1) )
	return
	case(2) !select case(j_o(ifunc)%i(1) )
	irow=2
	case default !select case(j_o(ifunc)%i(1) )
	irow=j_igetopt(iob,io,j_mrow)
	isz=j_linkoption(iob,io,j_mz).ge.0
	!!call j_clearoption(iob,io)  ! subroutine
	! type
	if(isz)then  !bilinear interpolation
		nrows=j_o(ifunc)%i(1)
		ncols=j_o(ifunc)%i(2)
		if(nrows.lt.3.or.ncols.lt.3.or.narg.lt.3)then
			write(6,*)'*value: illegal number of arguments or matrix for bilinear interpolation'
			j_err=.true.
			return
		endif !if(nrows.lt.3.or.ncols.lt.3.or.narg.lt.3)  14009
		arg2=j_v(j_o(iob)%i(io+4))
 
		do icol=3,ncols
			if(arg.le.j_getmatel(ifunc,1,icol))goto 17
 
		enddo !icol=3,ncols  14016
		icol=ncols
						17  continue

		do irow=3,nrows
			if(arg2.le.j_getmatel(ifunc,irow,1))goto 171
 
		enddo !irow=3,nrows  14023
		irow=nrows
						171 continue
		j_v(iout)=j_bilin(j_getmatel(ifunc,1,icol-1),j_getmatel(ifunc,1,icol),&
			j_getmatel(ifunc,irow-1,1),j_getmatel(ifunc,irow,1),&
			j_getmatel(ifunc,irow-1,icol-1),j_getmatel(ifunc,irow-1,icol), &
			j_getmatel(ifunc,irow,icol-1),j_getmatel(ifunc,irow,icol),arg,arg2)
		!!call j_clearoption(iob,io)  ! subroutine
		return
	endif !if(isz)  14006
	if(irow.le.0.and.j_o(ifunc)%i(4).eq.j_matreg)then
		! i(1) number of rows
		! i(2) number of columns
		if(iout.gt.j_named)then
			write(6,*)'*value can generate only named matrix'
			j_err=.true.
			return
		endif !if(iout.gt.j_named)  14039
		ivout_=j_defmatrix(iout,' ',int8(j_o(ifunc)%i(1)-1),j_18,j_matreg)
		do i=2,j_o(ifunc)%i(2)
			if(j_o(ifunc)%d(i).ge.arg.or.i.eq.j_o(ifunc)%i(2))then
				!(irow-1)*o(ifunc)%i(2)  y0+(x-x0)*(y1-y0)/(x1-x0)
				do irow=2,j_o(ifunc)%i(1)
					j_o(iout)%d(irow-1)=j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i-1)+(arg-j_o(ifunc)%d(i-1))* &
						(j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i)-j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i-1))/ &
						(j_o(ifunc)%d(i)-j_o(ifunc)%d(i-1))
				end do !irow=2,j_o(ifunc)%i(1)  14048
				return
			end if !if(j_o(ifunc)%d(i).ge.arg.or.i.eq.j_o(ifunc)%i(2))  14046
 
		end do !i=2,j_o(ifunc)%i(2)  14045
	else if(irow.le.0.and.j_o(ifunc)%i(4).eq.j_matclass)then !if(irow.le.0.and.j_o(ifunc)%i(4).eq.j_matreg)then
		irow=3
		nval=nval-1
	else !if(irow.le.0.and.j_o(ifunc)%i(4).eq.j_matreg)then
		irow=j_v(irow)
		if(irow.le.0.or.irow.gt.j_o(ifunc)%i(1))then
			call j_printname('**value-function::matrix ',ifunc, ' has illegal row->')
			write(6,*)'you gave row->',irow,' there are ',j_o(ifunc)%i(1),' rows'
			j_err=.true.
			return
		end if !if(irow.le.0.or.irow.gt.j_o(ifunc)%i(1))  14062
	end if !if(irow.le.0.and.j_o(ifunc)%i(4).eq.j_matreg)  14036
	end select !select case(j_o(ifunc)%i(1) )
	do i=2,nval
		if(j_o(ifunc)%d(i).ge.arg.or.i.eq.nval)then
			!(irow-1)*o(ifunc)%i(2)  y0+(x-x0)*(y1-y0)/(x1-x0)
			j_v(iout)=j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i-1)+(arg-j_o(ifunc)%d(i-1))* &
				(j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i)-j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i-1))/ &
				(j_o(ifunc)%d(i)-j_o(ifunc)%d(i-1))
			exit
		end if !if(j_o(ifunc)%d(i).ge.arg.or.i.eq.nval)  14071
	end do !i=2,nval  14070
	case(j_iptrans) !select case(j_otype(ifunc) )
	if(narg.ne.2)then
		call j_printname('**value(',ifunc, ', arg)  wrong number of arguments')
		j_err=.true.
		return
	endif !if(narg.ne.2)  14080
	iarg2=j_igetopt(iob,io,j_marg)
	if(iarg2.le.0)iarg2=j_o(ifunc)%i2(9)
	argv=j_v(iarg2)
	!iresu=j_igetopt(iob,io,j_mresult)
	if(iresu.le.0)iresu=j_o(ifunc)%i2(10)
	!!call j_clearoption(iob,io)  ! subroutine
	j_v(iarg2)=j_v(iarg)
	if(iout.gt.j_mxnamedv+1)then
		nsav=iout-j_mxnamedv-1
		allocate(vout(1:nsav)); vout=j_v(j_mxnamedv+1:iout-1)
	else !if(iout.gt.j_mxnamedv+1)then
		nsav=0
	endif !if(iout.gt.j_mxnamedv+1)  14092
	call dotrans(ifunc,1)
	if(j_err)return
	!write(6,*)'<54754'
	j_v(iarg2)=argv
	j_v(iout)=j_v(iresu)
	if(nsav.gt.0)then
		j_v(j_mxnamedv+1:iout-1)=vout ;deallocate(vout)
	endif !if(nsav.gt.0)  14103
	case(j_ipregr) !select case(j_otype(ifunc) )
	write(6,*)'value(regrob.. is delete feature, use directly =regob('
	j_err=.true.
	return
	! nre=j_o(ifunc)%i(0)
	! if(j_o(ifunc)%i(nre).eq.j_ivone)then
	! dapu=j_o(ifunc)%d(nre)
	! if(isprint)write(6,*)'intcep ',dapu
	! nre=nre-1
	! else !if(j_o(ifunc)%i(nre).eq.j_ivone)then
	! dapu=0.
	! end if !if(j_o(ifunc)%i(nre).eq.j_ivone)then
	! dapu=j_0
	! do i=1,min(narg-1,nre)
	! dapu=dapu+j_o(ifunc)%d(i)*j_v( j_o(iob)%i(io+2+i))
	! if(isprint)write(6,*)'+arg',j_o(ifunc)%d(i),j_v( j_o(iob)%i(io+2+i))
	! end do !do i=1,min(narg-1,nre)
	! do i=narg,nre
	! dapu=dapu+j_o(ifunc)%d(i)*j_v( j_o(ifunc)%i(i) )
	! if(isprint)write(6,*)'+v',j_o(ifunc)%d(i),j_v( j_o(ifunc)%i(i) )
	! end do !do i=narg,nre
	! j_v(iout)=dapu
	! if(isprint)write(6,*)'value=',j_v(iout)
	!!!call j_clearoption(iob,io)  ! subroutine
	case(j_iplist) !select case(j_otype(ifunc) )
	if(inde.le.0.or.inde.gt.j_o(ifunc)%i(1))then
		call j_printname('**value-function, illegal index for list ',ifunc, ' :')
		write(6,*)inde
		j_err=.true.
		return
	endif !if(inde.le.0.or.inde.gt.j_o(ifunc)%i(1))  14131
	j_v(iout)=j_v(j_o(ifunc)%i(inde))
 
	case(j_iptautspline) !select case(j_otype(ifunc) )
	arg=j_v(iarg)
	j_v(iout)=ppvalu(j_o(ifunc)%d,j_o(ifunc)%d(j_o(ifunc)%i(2)+1:),j_o(ifunc)%i(3), 4,arg,0)
 
 
	case(j_ipbitmatrix) !select case(j_otype(ifunc) )
	j=j_v(iarg)
	if(j.gt.j_o(ifunc)%i(1).or.j.le.0)then
		if(j_linkoption(iob,io,j_many).ge.0)then
			j_v(iout)=0
			!	!!call j_clearoption(iob,io)  ! subroutine
			return
		endif !if(j_linkoption(iob,io,j_many).ge.0)  14147
		call j_printname('**value: first index has illegal value for bitmatrix ',ifunc, ' ')
		j_err=.true.;return
	endif !if(j.gt.j_o(ifunc)%i(1).or.j.le.0)  14146
	if(narg.gt.2)then
		inde2=j_v(j_o(iob)%i(io+4))
		if(inde2.gt.j_o(ifunc)%i(2).or.inde2.lt.j_o(ifunc)%i(3))then
			if(j_linkoption(iob,io,j_many).ge.0)then
				j_v(iout)=0
				!	!!call j_clearoption(iob,io)  ! subroutine
				return
			endif !if(j_linkoption(iob,io,j_many).ge.0)  14158
			call j_printname('**value: second argument has illegal value for bitmatrix ',ifunc, ' ')
			j_err=.true.;return
		endif !if(inde2.gt.j_o(ifunc)%i(2).or.inde2.lt.j_o(ifunc)%i(3))  14157
	endif !if(narg.gt.2)  14155
	j_v(iout)=j_ibittest(ifunc,j,inde2)
	!	if(j_linkoption(iob,io,j_many).ge.0)!!call j_clearoption(iob,io)  ! subroutine
	case (j_iplaaspoly) !select case(j_otype(ifunc) )
	j_v(iout)=crkd(real(j_v(iarg)),real(j_o(ifunc)%d))
	if(j_err)return
	case default !select case(j_otype(ifunc) )
	call j_printname('**value: first argument has not valid type ',ifunc, ' ')
	j_err=.true.
	return
	end select !select case(j_otype(ifunc) )
	!	900 !!call j_clearoption(iob,io)  ! subroutine !  io=io+narg+3
	return
end subroutine value_ !recursive subroutine value_(iob,io)
 
subroutine ran(iob,io) !rando_loopm uniform number netlib ranlib
	!Section ran ran() Uniform
	! Uniform random numbers between 0 and 1 are generating usig Netlib function ranf.
	!endheader
	!Option
	!Output& 1& REAL |MATRIX  & The generated REAL value or MATRIX.
	!Random matrix cab generated by defining first the matrix with matrix().
	!endoption.
	! Ex ranex
	! ran();
	! ran();
	! cpu0=cpu()
	! A=matrix(10000,5)
	! A=ran()
	! mean(A);
	! mean(A,any->) !mean over all elements
	! mean(A(All,2));
	! sd(A);
	! sd(A,any->);
	! min(A);
	! min(A,any->);
	! max(A);
	! cpu()-cpu0;
 
	! endex
 
	! endsection
	external ranf
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_named)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  14213
	if(narg.gt.0)then
 
		write(6,*)'*ran: seed not yet '
		j_err=.true.
		return
 
	end if !if(narg.gt.0)  14216
	if(j_otype(iout).eq.j_ipmatrix)then
		do j=1,j_o(iout)%i(3)
			j_o(iout)%d(j)=ranf()
		enddo !j=1,j_o(iout)%i(3)  14224
	else !if(j_otype(iout).eq.j_ipmatrix)then
 
		j_v(iout)=ranf()
	endif !if(j_otype(iout).eq.j_ipmatrix)  14223
 
	return
end subroutine ran !subroutine ran(iob,io)
 
subroutine select(iob,io)
	! Section select select() Random selection
	! endheader
	! Option
	! Output&1&MATRIX &column vector with n elements indicating random
	! selection of k
	! elements out of n elements. The selection is without replacement,
	! thus elements of the output are 1 or 0..
	! Args&2&REAL& k=]Arg1[ and n=]Arg2[.
	! endoption
	! Ex selectex Random selection
	!** select 500 numbers without replacement from 10000
	!** output is vector of 10000 rows containing 0 or 1
	! S=select(500,10000)
	! nrows(S),mean(S),sum(S),500/10000;
	! endex
	! endsection
 
 
	external ranf
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(iout.eq.j_ivresult)then
		write(6,*)'*select needs an output'
		j_err=.true.
	endif !if(iout.eq.j_ivresult)  14259
 
	n=j_v(j_o(iob)%i(io+2))
	ntot=j_v(j_o(iob)%i(io+3))
	if(n.lt.0.or.n.gt.ntot.or.ntot.le.0)then
		WRITE(6,*)'*select: illegal arguments ',n,ntot
		j_err=.true.
	endif !if(n.lt.0.or.n.gt.ntot.or.ntot.le.0)  14266
 
	if(j_err)return
	ivout=j_defmatrix(iout,' ',int8(ntot),j_18,j_matreg)
 
	if(n.eq.0)return
	if(n.eq.ntot)then
		j_o(ivout)%d(1:ntot)=1.
		return
	endif !if(n.eq.ntot)  14275
	nleft=n
	do i=1,n
		r=ranf()
		ir=r*nleft+1.
		ir=min(ir,nleft)
		icount=0
		do j=1,ntot
			if(j_o(ivout)%d(j).le.0.)icount=icount+1
			if(ir.eq.icount)then
				j_o(ivout)%d(j)=1.
 
			endif !if(ir.eq.icount)  14287
 
		enddo !j=1,ntot  14285
	enddo !i=1,n  14280
 
end subroutine !subroutine select(iob,io)
 
subroutine random(iob,io)
	! Section random random()  Any distribution
	! usage random(]dist[) where ]dist[ is the density defined in density().
	! See density() for examples.
	! endsection
 
	logical ::diskr
	external ranf
	integer, dimension(:), pointer :: arg
	double precision ::xval
	!	io=io_
 
	call	j_startfunction(iob,io,j_ipmatrix,narg,arg,iout)
	if(j_err)return
 
	iarg=arg(1)
	ncol=j_o(iarg)%i(2)
	diskr=j_o(iarg)%i(4).eq.j_matfreq
	!	io_=io_+narg+3
	!	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_named)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  14317
 
	if(narg.gt.1)then
 
		write(6,*)'*random: seed not yet '
		j_err=.true.
		return
 
	end if !if(narg.gt.1)  14321
	if(j_otype(iout).eq.j_ipmatrix)then
		if(diskr)then
		jloop:		do j=1,j_o(iout)%i(3)
				ra=ranf()
				!write(6,*)'ram',ra
				do i=1,ncol-1
					!y=y0+(y1-y0)*(x-x0)/(x1-x0)  here x is value of rnadom uniform and y is the xavalue
					if(j_o(iarg)%d(ncol+i).ge.ra)then
						!write(6,*)'i ',i,j_o(iarg)%d(i-1),j_o(iarg)%d(i),j_o(iarg)%d(i-1),j_o(iarg)%d(ncol+i-1)
 
						j_o(iout)%d(j)=j_o(iarg)%d(i)
 
						cycle jloop
					endif !if(j_o(iarg)%d(ncol+i).ge.ra)  14335
				enddo !i=1,ncol-1  14333
				j_o(iout)%d(j)=j_o(iarg)%d(ncol)
 
				!		j_o(iout)%d(j)=ranf()
			enddo jloop !op:		do j=1,j_o(iout)%i(3)  14330
 
		else
			do j=1,j_o(iout)%i(3)
				ra=ranf()
				!write(6,*)'ram',ra
				do i=2,ncol
					!y=y0+(y1-y0)*(x-x0)/(x1-x0)  here x is value of rnadom uniform and y is the xavalue
					if(j_o(iarg)%d(ncol+i).ge.ra)then
						!write(6,*)'i ',i,j_o(iarg)%d(i-1),j_o(iarg)%d(i),j_o(iarg)%d(i-1),j_o(iarg)%d(ncol+i-1)
 
 
						j_o(iout)%d(j)=j_o(iarg)%d(i-1)+(j_o(iarg)%d(i)-j_o(iarg)%d(i-1))*(ra-j_o(iarg)%d(ncol+i-1))/&
							(j_o(iarg)%d(ncol+i)-j_o(iarg)%d(ncol+i-1))
 
						exit
 
					endif !if(j_o(iarg)%d(ncol+i).ge.ra)  14354
 
 
				enddo !i=2,ncol  14352
				!		j_o(iout)%d(j)=ranf()
			enddo !j=1,j_o(iout)%i(3)  14349
		endif !if(diskr)  14329
	elseif(diskr)then
		do i=1,ncol-1
			!y=y0+(y1-y0)*(x-x0)/(x1-x0)  here x is value of rnadom uniform and y is the xavalue
			if(j_o(iarg)%d(ncol+i).ge.ra)then
				!write(6,*)'i ',i,j_o(iarg)%d(i-1),j_o(iarg)%d(i),j_o(iarg)%d(i-1),j_o(iarg)%d(ncol+i-1)
 
 
				j_v(iout)=j_o(iarg)%d(i)
 
				return
			endif !if(j_o(iarg)%d(ncol+i).ge.ra)  14373
		enddo !i=1,ncol-1  14371
		j_v(iout)=j_o(iarg)%d(ncol)
 
 
	else !if(j_otype(iout).eq.j_ipmatrix)then
		ra=ranf()
		!	write(6,*)'ra ',ra
		do i=2,ncol
			!y=y0+(y1-y0)*(x-x0)/(x1-x0)  here x is value of rnadom uniform and y is the xavalue
			if(j_o(iarg)%d(ncol+i).ge.ra)then
				j_v(iout)=j_o(iarg)%d(i-1)+(j_o(iarg)%d(i)-j_o(iarg)%d(i-1))*(ra-j_o(iarg)%d(ncol+i-1))/&
					(j_o(iarg)%d(ncol+i)-j_o(iarg)%d(ncol+i-1))
 
				return
 
			endif !if(j_o(iarg)%d(ncol+i).ge.ra)  14390
		enddo !i=2,ncol  14388
		!	j_v(iout)=ranf()
	endif !if(j_otype(iout).eq.j_ipmatrix)  14328
 
	return
 
 
 
end subroutine
 
subroutine rann(iob,io)  !calls function gennor from netlib ranlib
	!Section rann rann() Normal
	!Computes normally distributed pseudo random numbers into a REAL variable or
	!into MATRIX.
	!endheader
	!Option
	!Output&1&REAL|MATRIX&The matrix to be generated must be defined earlier with matrix().
	!Args&0-2&num& rannn() produces N(0,1) variables, rann(mean) will produce
	!N(mean,1) variables and rann(mean,sd) procuses N(mean,sd) variables.
	!endoption
	!Ex rannex Random normal variates, illustrating also find
	!rx=rann();  !Output is REAL
	!rm=matrix(1000)
	!rm=rann()
	!rm(1,-5);
	!print(mean(rm),sd(rm),min(rm),max(rm))
	!Continue=1 !an error
	!large=find(rm,filter->($.ge.2),any)
	!Continue=0
	!large=find(rm,filter->($.ge.2),any->)
	!100*nrows(large)/nrows(rm);
	!cpu0=cpu()
	!rm2=matrix(1000000)
	!rm2=rann(10,2)  !there cannot be arithmetix opreations in the right side
	!cpu()-cpu0;
	!mean(rm2),sd(rm2),min(rm2),max(rm2);
	!large=find(rm2,filter->($.ge.14),any->)
	!100*nrows(large)/nrows(rm2);
	!
	!endex
	!Note When generating a matrix with random numbers, there cannot be
	!arithmetic operations on the right side.That means that code:\newline
	!rm=matrix(100)\newline
	!rm=2*rann()\newline
	!would produce a REAL value rm.
	!endnote
 
	!endsection
	real av,sd
	!	!!call j_clearoption(iob,io)
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	!	write(6,*)'iout,type',iout,j_otype(iout)
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_named)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  14452
	if(narg.ge.3)then
		write(6,*)'*rann: seed not yet '
		j_err=.true.
		return
	end if !if(narg.ge.3)  14455
 
	if(narg.ge.1)then
		av=j_v(j_o(iob)%i(io+2))
		if(narg.ge.2)then
			sd=j_v(j_o(iob)%i(io+3))
		else !if(narg.ge.2)then
			sd=1.
		endif !if(narg.ge.2)  14463
	else !if(narg.ge.1)then
		av=0.
		sd=1.
	endif !if(narg.ge.1)  14461
	if(j_otype(iout).eq.j_ipmatrix)then
		do j=1,j_o(iout)%i(3)
			j_o(iout)%d(j)=gennor(av,sd)
		enddo !j=1,j_o(iout)%i(3)  14473
	else !if(j_otype(iout).eq.j_ipmatrix)then
		j_v(iout)=gennor(av,sd)
	endif !if(j_otype(iout).eq.j_ipmatrix)  14472
 
	return
end subroutine rann !subroutine rann(iob,io)
 
subroutine ranpoi(iob,io)  !calls function ignpoi from netlib ranlib
	! Section ranpoi ranpoi() Poisson
	! ranpoi(]myy[\\
	! returns a random Poisson variable with expected value and variance ]myy[
	! endsection
 
 
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_named)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  14494
	rmu=j_v(j_o(iob)%i(io+2))
	if(rmu.le.0)then
		write(6,*)'*ranpoi* mu is nonpositive ',rmu
		j_err=.true.
		return
 
	endif !if(rmu.le.0)  14498
	if(j_otype(iout).eq.j_ipmatrix)then
		do j=1,j_o(iout)%i(3)
			j_o(iout)%d(j)=ignpoi(rmu)
		enddo !j=1,j_o(iout)%i(3)  14505
	else !if(j_otype(iout).eq.j_ipmatrix)then
 
		j_v(iout)=ignpoi(rmu)
	endif !if(j_otype(iout).eq.j_ipmatrix)  14504
 
	return
end subroutine ranpoi !subroutine ranpoi(iob,io)
 
 
subroutine ranbin(iob,io)
	!Section ranbin ranbin() Binomial
 
	!Binomial random numbers between 0 and n are generating usig Netlib
	! ignbin(n,p). Random matrix can generated by defining first
	! the matrix with matrix().
	!endheader
	!Option
	!Output& 1& REAL |MATRIX  & The generated REAL value or MATRIX with
	! number of successes. (Jlp22 does not have explicit integer type object).
	!Args &2&REAL& ]Arg1[ is the number of trials (n) and ]Arg2[ is the probability
	! of succes in one trial.
	!
	!endoption.
	! Ex ranbinex Random binomial
	! ranbin(10,0.1);
	! ranbin(10,0.1);
	! A=matrix(1000,2)
	! Continue=1
	! A(All,1)=ranbin(20,0.2)
	! Continue=0
	! A=matrix(1000)
	! A=ranbin(20,0.2)
	! B=matrix(1000)
	! B=ranbin(20,0.2)
	! da=newdata(A,B,read->(s1,s2))
	!stat(min->,max->)
	!cl=classify(1,x->s1,xrange->)
	!fi=drawclass(cl,histogram->,color->Blue,continue->fcont)
	!cl=classify(1,x->s2,xrange->)
	!fi=drawclass(cl,histogram->,color->Red,append->,continue->fcont)
	! endex
 
	! endsection
 
	integer n_
	!	double precision:: pitäs tehdä
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
 
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_named)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  14559
	n_=j_v(j_o(iob)%i(io+2))
	if(n_.le.0)then
		write(6,*)'*ranbin: n (=1st arg) must be positive, it was ',n_
		j_err=.true.
 
	endif !if(n_.le.0)  14563
	pp_=j_v(j_o(iob)%i(io+3))
	if(pp_.lt.0.or.pp_.gt.1.)then
		write(6,*)'*ranbin: p (=2nd arg) must be between 0 and 1, it was ',pp_
		j_err=.true.
	endif !if(pp_.lt.0.or.pp_.gt.1.)  14569
	if(j_err)return
	if(j_otype(iout).eq.j_ipmatrix)then
		if(pp_.eq.0.)then
			j_o(iout)%d=0.
		elseif(pp_.eq.1.)then !if(pp_.eq.0.)then
			j_o(iout)%d=n_
		else !if(pp_.eq.0.)then
			do j=1,j_o(iout)%i(3)
				j_o(iout)%d(j)=ignbin(n_,pp_)
			enddo !j=1,j_o(iout)%i(3)  14580
		endif !if(pp_.eq.0.)  14575
 
	else !if(j_otype(iout).eq.j_ipmatrix)then
		if(pp_.eq.0.)then
			j_v(iout)=0.
		elseif(pp_.eq.1.)then !if(pp_.eq.0.)then
			j_v(iout)=n_
		else !if(pp_.eq.0.)then
			j_v(iout)=ignbin(n_,pp_)
		endif !if(pp_.eq.0.)  14586
	endif !if(j_otype(iout).eq.j_ipmatrix)  14574
 
	return
 
end subroutine !subroutine ranbin(iob,io)
 
 
 
 
subroutine cpu(iob,io)  !Jlp22-function cpu()
	real,dimension(:),allocatable ::vc
	double precision,dimension(:),allocatable ::vc2
 
	! Section cpu cpu() Cpu time
	! endheader
	! Ex cpuex Example of cpu-timing
	! cpu0=cpu()
	! a=matrix(100000)
	! a=ran() !uniform
	! mean(a),sd(a),min(a),max(a);
	! cpu1=cpu()
	! elapsed=cpu1-cpu0;
	! endex
	! endsection
 
 
	!	call system('cls')
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	! !testing
	! ndim=j_v(j_o(iob)%i(io+2))
	! call j_defmatrix(iout,' ',ndim,1,j_matreg,iout)
	! call cpu_time(time)
	! j_o(iout)%d(i)=j_1
	! do i=2,ndim
	! j_o(iout)%d(i)=j_o(iout)%d(i-1)*j_o(iout)%d(i)
 
	! enddo !i=2,ndim  13398
	! call cpu_time(time2)
	! write(6,*)'timeobj ',time2-time
 
 
	! p_x=>j_o(iout)%d(1:ndim)
	! call cpu_time(time)
	! do i=2,ndim
	! p_x(i)=p_x(i-1)*p_x(i)
 
	! enddo !i=2,ndim  13408
	! call cpu_time(time2)
	! write(6,*)'timepoint ',time2-time
 
	! allocate(j_tempvector(1:ndim))
	! j_tempvector=j_1
	! call cpu_time(time)
	! do i=2,ndim
	! j_tempvector(i)=j_tempvector(i-1)*j_tempvector(i)
 
	! enddo !i=2,ndim  13418
	! call cpu_time(time2)
	! write(6,*)'timeallo',time2-time
	! return
 
	! allocate(j_o(j_ivdollar)%d(1:10000000))
	! allocate(j_o(j_ivdollar)%r(1:10000000))
	! allocate(j_o(j_ivdollar)%i(1:20))
	! allocate(vc(1:9),vc2(1:18))
	! 19	write(6,*)'anluku'
	! read(5,*)j_dapu
	! write(6,*)fraction(j_dapu),exponent(j_dapu)
	! goto 19
	! if(aluku.eq.0.0)return
	! vc=aluku
	! write(17,*)vc
	! goto 19
	! vc=2.d0/3.d0
	! vc2(1:9)=2.d0/3.d0
	! vc2(10:18)=j_0
	! do i=1,20
	! j_o(j_ivdollar)%i(i)=i
	! enddo !i=1,20  14615
	! j_o(j_ivdollar)%d=j_1/3.d0
	! j_o(j_ivdollar)%r=j_1/3.d0
	! ! j_dapu2=j_o(j_ivdollar)%d(1)*vc2(1)
	! ! j_dapu=j_o(j_ivdollar)%r(1)*vc(1)
	! ! j_dapu=9.d0*j_dapu
	! ! j_dapu2=9.d0*j_dapu2
	! ! write(6,*)'onko 2 ',j_dapu,j_dapu2
 
	! call cpu_time(time0)
	! do k=1,10000
	! ibas=0
	! j_dapu=j_0
	! do i=1,10000
	! j_dapu=j_dapu+dot_product(vc2(1:9),j_o(j_ivdollar)%d(ibas+1:ibas+9))
 
	! ibas=ibas+9
	! enddo !i=1,10000  14630
	! enddo !k=1,10000  14627
	! call cpu_time(time)
 
	! write(6,*)'d2 ',time-time0,j_dapu
 
	! call cpu_time(time0)
	! do k=1,10000
	! ibas=0
	! j_dapu=j_0
	! do i=1,10000
	! j_dapu=j_dapu+dot_product(vc2(1:9),j_o(j_ivdollar)%d(j_o(j_ivdollar)%i(1:9)))
 
	! ibas=ibas+9
	! enddo !i=1,10000  14644
	! enddo !k=1,10000  14641
	! call cpu_time(time)
 
	! write(6,*)'d2 ind',time-time0,j_dapu
 
	! call cpu_time(time0)
	! do k=1,10000
	! ibas=0
	! j_dapu=j_0
	! do i=1,10000
	! j_dapu=j_dapu+dot_product(vc2(1:18),j_o(j_ivdollar)%d(ibas+1:ibas+18))
 
	! ibas=ibas+9
	! enddo !i=1,10000  14658
	! enddo !k=1,10000  14655
	! call cpu_time(time)
 
	! write(6,*)'d2 zero',time-time0,j_dapu
	! vc2(10:18)=j_1/2.45d0
	! call cpu_time(time0)
	! do k=1,10000
	! ibas=0
	! j_dapu=j_0
	! do i=1,10000
	! j_dapu=j_dapu+dot_product(vc2(1:18),j_o(j_ivdollar)%d(ibas+1:ibas+18))
 
	! ibas=ibas+9
	! enddo !i=1,10000  14672
	! enddo !k=1,10000  14669
	! call cpu_time(time)
 
	! write(6,*)'d2 zero',time-time0,j_dapu
 
	! ! call cpu_time(time0)
	! ! do k=1,1000
	! ! ibas=0
	! ! j_dapu=j_0
	! ! do i=1,1000000
	! ! j_dapu=j_dapu+dot(vc2(1:9),j_o(j_ivdollar)%d(ibas+1:ibas+9))
 
	! ! ibas=ibas+9
	! ! enddo !i=1,1000000  14361
	! ! enddo !k=1,1000  14358
	! ! call cpu_time(time)
 
	! ! write(6,*)'d2dot ',time-time0,j_dapu
 
 
	! call cpu_time(time0)
	! do k=1,10000
	! ibas=0
	! j_dapu=j_0
	! do i=1,10000
	! j_dapu=j_dapu+dot_product(vc(1:9),j_o(j_ivdollar)%r(ibas+1:ibas+9))
 
	! ibas=ibas+9
	! enddo !i=1,10000  14701
	! enddo !k=1,10000  14698
	! call cpu_time(time)
 
	! write(6,*)'sing minus ',time-time0,j_dapu
 
	! ! do k=1,1000
	! ! ibas=0
	! ! j_dapu=j_0
	! ! do i=1,1000000
	! ! j_dapu=j_dapu+dot_product(vc(1:9),j_o(j_ivdollar)%r(ibas+j_o(j_ivdollar)%i(1:9)))
 
	! ! ibas=ibas+9
	! ! enddo !i=1,1000000  13533
	! ! enddo !k=1,1000  13530
	! ! call cpu_time(time)
 
	! ! write(6,*)'d2ite ',time-time0,j_dapu
	! ! call cpu_time(time0)
	! ! do k=1,1000
	! ! ibas=0
	! ! j_dapu=j_0
	! ! do i=1,1000000
	! ! j_dapu=j_dapu+dot_product(vc(1:9),j_o(j_ivdollar)%r(ibas+j_o(j_ivdollar)%i(1:9)))
 
	! ! ibas=ibas+9
	! ! enddo !i=1,1000000  13546
	! ! enddo !k=1,1000  13543
	! ! call cpu_time(time)
 
	! ! write(6,*)'sinite',time-time0,j_dapu
 
	! do k2=1,10000
	! ibas=0
	! j_dapu=j_0
	! do i=1,10000
	! do k=1,9
	! j_dapu=j_dapu+vc2(k)*j_o(j_ivdollar)%d(ibas+k)
	! enddo !k=1,9  14741
	! ibas=ibas+9
	! enddo !i=1,10000  14740
	! enddo !k2=1,10000  14737
	! call cpu_time(time)
 
	! write(6,*)'d2iteloop ',time-time0,j_dapu
	! ! call cpu_time(time0)
	! ! do k2=1,1000
	! ! ibas=0
	! ! j_dapu=j_0
	! ! do i=1,1000000
	! ! do k=1,9
	! ! j_dapu=j_dapu+vc(k)*j_o(j_ivdollar)%r(ibas+j_o(j_ivdollar)%i(k))
	! ! enddo !k=1,9  13574
	! ibas=ibas+9
	! enddo !i=1,1000000  13573
	! enddo !k2=1,1000  13570
	! call cpu_time(time)
 
	! write(6,*)'siniteloop',time-time0,j_dapu
 
 
	call cpu_time(time)
	if(narg.le.0)then
		j_v(iout)=time  ! -first
	else !if(narg.le.0)then
		j_v(iout)=time-j_v(j_o(iob)%i(io+2) )
	end if !if(narg.le.0)  14824
	!io=io+narg+3
	return
end subroutine cpu !subroutine cpu(iob,io)
 
subroutine secnds_(iob,io) !Jlp22-function secnds()
	! Section secnds secnds() Clock time
	! endheader
	! Ex secondsex Example of elapsed time
	! cpu0=cpu()
	! sec0=secnds()
	! a=matrix(100000)
	! a=ran() !uniform
	! mean(a),sd(a),min(a),max(a);
	! cpu1=cpu()
	!sec1=secnds()
	! elapsed=cpu1-cpu0;
	!selapsed=sec1-sec0;
	! endex
	! endsection
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	time=secnds(0.)
	if(narg.le.0)then
		j_v(iout)=time ! -first
	else !if(narg.le.0)then
		j_v(iout)=time-j_v(j_o(iob)%i(io+2) )
	end if !if(narg.le.0)  14853
	!io=io+narg+3
	return
end subroutine secnds_ !subroutine seconds(iob,io)
 
subroutine where(iob,io)
 
	call j_where(iob,io)
	return
end subroutine
 
 
subroutine batch(iob,io)
 
	j_remain=.false.
	return
end subroutine
 
subroutine systemj(iob,io)  ! system(iob
	!Section systemcom system() executes Windows commands
	!Windows system commands can be given by\\
	!system('command')\\
	!For instance \\
	! system('cls') clears screen
	! system('dir') prints the directory
	!endsection
	!Section cls cls clears screen
	!command cls clears screen, thus cls is equivalent to system('cls')
	!endsection
 
	call j_startfunction(iob,io,j_ipchar,narg,j_arg,ivout);if(j_err)return
	call j_getchar(j_arg(1),j_tempchar,le);if(j_err)return
	call system(j_tempchar(1:le))
	return
end subroutine
 
 
 
subroutine info(iob,io)
	!Section info info() print some information of current parameters.
	!endheader
	!Note Late addtiona information sis printed.
	!endnote
	!endsection
	write(6,*)'there are ',j_named,' named objects'
	write(6,*)'there is space for ',j_mxtemporalv,' intermediate results'
	write(6,*)'from wchich ',j_mxtemporalv-j_mxtemporalv0, ' are for derivatives'
	write(6,*)'there are ',j_nconstantv,' constants'
	write(6,*)' '
	write(6,*)'the following ',j_nfunctions_, ' functions are available, with min and max # of arguments'
	do j=1,j_nfunctions_
		write(6,*)j,j_functions(j),j_minarg(j),j_maxarg(j)
	enddo !j=1,j_nfunctions_  14907
	write(6,*)' '
	write(6,*)'the following ',j_noptions_, ' options are available (some are obsolete)'
	do j=1,j_noptions_
		write(6,*)j,j_options(j)
	enddo !j=1,j_noptions_  14912
	write(6,*)'The current objects can be seen with Names;'
end subroutine
 
subroutine namenum(iob,io,ind)
	!Section namenum namenum() and namenum%() take numbers from object name
	!Object names often contain some numbers. These can be
	!picked using namenum() and namenum%() functions. Probably the function will be used so that the object is
	! in an object LIST. The are two ways to pick object from LIST, either in input programming or either at sit> prompt or
	! in TRANS object. Usually the numbers are picked from the list of cases in a data
	! with cases.
	! Function namenum() picks the number which is presented with consecutive numbers, while
	! namenum%() picks the numbers from a REAL variable.
	!endheader
	!Option
	!Output& 1& REAL& The number picked from the name
	!Args&1-3&REAL|LIST& There are two cases:\\
	!The first argument is the object from whose name the number is picked. Then the second argument can be
	!  The second argument tells what is the order of the number among numbers (in namenum()) or REAL
	! variables whose name starts after
	! '%' ( in Namenum%() )	of the name. Default is one, i.e. the first
	! number is picked. \\
	! The fist argument is LIST and sewcond argument is the element number and third argument can be the order of number.
	!endoption
	!Ex namenumex Example of namenum() and namenum%() in the data decribing the log storage in a sawmill
	! logdata=data(read->(T_log,N_log),in->,case->)
	! L15%40%Spruce,200
	! L15%43%Spruce,300
	! L18%40%Pine,100
	! L18%43%Spruce,150
	! /
	! D=namenum(logdata%case,3,2);
	! D=namenum(@logdata%case(3),2);
	! logdata%case;
	! ili=list2sym(logdata%case,2);
	! @ili;
 
	! species=namenum%(logdata%case,3,2);
	! species=namenum%(@logdata%case(3),2);
	!endex
	!endsection
 
 
 
	call j_startfunction(iob,io,0,narg,j_arg,ivout);if(j_err)return
	if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
	if(j_otype(j_arg(1)).eq.j_iplist)then
		if(narg.lt.2)then
			write(6,*)'if first argument of namenum(9 is LIST, there must be 2 or the arguments'
			j_err=.true.
 
		endif !if(narg.lt.2)  14961
		nel=j_v(j_arg(2))
		if(nel.le.0.or.nel.gt.j_o(j_arg(1))%i(1))then
			write(6,*)'you asked element ',nel,' from ',j_o(j_arg(1))%i(1), '?'
			j_err=.true.;return
		endif !if(nel.le.0.or.nel.gt.j_o(j_arg(1))%i(1))  14967
		irg=j_o( j_arg(1) )%i2(nel)
		nba=3
	else
 
		irg=j_arg(1)
		nba=2
 
	endif !if(j_otype(j_arg(1)).eq.j_iplist)  14960
	nn=1
	if(narg.eq.nba)nn=j_v(j_arg(nba))
	call j_getname(irg)
	!	write(6,*)j_oname(1:j_loname),'  ',nn
	if(ind.eq.1)then
		j_v(ivout)=j_namenum(j_oname(1:j_loname),nn)
	else
		!	write(6,*)j_oname(1:j_loname),'  ',nn
		call  j_namenum2(j_oname(1:j_loname),nn,val=j_dapu)
		j_v(ivout)=j_dapu
	endif !if(ind.eq.1)  14983
	return
 
end subroutine
 
subroutine macro(iob,io)
	!Section macro macro() makes shortcuts for macros
	!	Sections in an include file starting with label like ;labelA: and
	! ending with ;return are called 'macros'. It is a convenient practice to
	! define a CHAR object with name labelA which can be used as a shortcut for calling the macro.
	! Function macro()  can be used to make such CHAR objects.
	! If the function has output then the function will make a LIST for these CHAR variables.
	! The function tells at what lines the labels are or that such label does not exist.
	! No error is generated when the lable does not exist. An error results if the macro
	! will be called before it is defined.
	!endheader
	!Note The function is reported here in the input programming section, even if the
	! function is in fact an ordinary Jlp22 function. It does not in any way modify
	! the input going to the parser. Thats why the name of the function does not contain
	! semicolon. It can be called within TRANS object, even if I cannot see when ti might be useful.
	!endnote
	!endsection
 
	call j_startfunction(iob,io,0,narg,j_arg,ivout)
	!	write(6,*)'arg',narg,'*',j_arg
	do i=1,narg
		itype=j_otype(j_arg(i))
		if(itype.ne.j_ipreal.and.itype.ne.j_ipchar)then
			call j_getname(j_arg(i))
			if(itype.eq.j_ipmatrix)then
				write(6,*)j_oname(1:j_loname),' is MATRIX, delete it first with delete_o(',j_oname(1:j_loname),')'
			else
				write(6,*)j_oname(1:j_loname),' is not CHAR or REAL, delete it first with ',j_oname(1:j_loname),'=0'
			endif !if(itype.eq.j_ipmatrix)  15018
			return
		endif !if(itype.ne.j_ipreal.and.itype.ne.j_ipchar)  15016
	enddo !i=1,narg  15014
	if(j_ninc.le.1)then
		write(6,*)'macro not allowed at command level'
		j_err=.true.;return
	endif !if(j_ninc.le.1)  15026
	!function j_defchar(iv,name) !define a character constan
	! write(6,*)'j_inciv(j_ninc)',j_inciv(j_ninc)
	!write(6,*)'j_ninc',j_ninc,j_inciv(j_ninc)
	call j_gettxtfile(j_inciv(j_ninc),j_filename,le)
	!write(6,*)'file ',j_filename(1:le)
	! if(j_err)return
 
 
	do i=1,narg
		call j_getname(j_arg(i))
		!	write(6,*)'arg',i,j_arg(i),j_oname(1:j_loname)
		write(6,*)';incl('//j_filename(1:le)//',from->'//j_oname(1:j_loname)//')'
		call j_deftxtchar(j_arg(i),';incl('//j_filename(1:le)//',from->'//j_oname(1:j_loname)//')')
		!	call j_asschar2(ivchar,j_arg(i))
 
		iline=j_findad(j_oname(1:j_loname))
		!	write(6,*)'iline',iline,j_oname(1:j_loname)
		if(iline.eq.0)then
			write(6,*)';',j_oname(1:j_loname),': not found'
		else
			write(6,*)';',j_oname(1:j_loname),': at line ',iline
		endif !if(iline.eq.0)  15047
 
	enddo !i=1,narg  15038
 
 
	if(ivout.ne.j_ivresult)iout=j_deflist(ivout,' ',list0=narg,list=j_arg(1:narg))
	j_prot(j_arg(1:narg))=.true.
 
end subroutine
 
 
 
! subroutine jump(iob,io) ! Jlp22-function jump
 
! j_njump=j_njump+1
! if(j_njump.gt.j_mxjump)then
! write(6,*)'*j*jump:, jump limit ',j_mxjump ,' exceeded'
! j_err=.true.
! return ! error line perhaps not properly printed
! endif !if(j_njump.gt.j_mxjump)  14363
! j_jumpio(j_njump)=io+4
! j_jumpiob(j_njump)=iob
! io=j_o(iob)%i(io+2)
! return
! end subroutine jump !subroutine jump(iob,io)
 
! subroutine back(iob,io) !Jlp22-function back io?
 
! if(j_njump.le.0)then
! write(6,*)'**back without jump'
! j_err=.true.;return !error line not properly printed
! else if(iob.ne.j_jumpiob(j_njump))then !if(j_njump.le.0)then
! write(6,*)'**back in different trans than last jump'
! j_err=.true.;return
! end if !if(j_njump.le.0)  14376
! io=j_jumpio(j_njump)
! j_njump=j_njump-1
! return
! end subroutine back !subroutine back(iob,io)
 
recursive subroutine printresult(iob,io,level)
	!	logical done !
	! Section printresult Printresult controls printing of the result
	! If there is semicolon ; at the end of a line at command level or within a
	!TRANS object line, the the output is	printed with implicit printresult() function if the value
	! of ]Printresult[ has value 1. If the line ends with double semicolon ;; then the
	!output is printed if variable ]Printresult[ has value 2. If ]Printresult[=3, then
	! the output of both ; and ;; -lines are printed. Using ;pause in command level or
	! pause() in TRANS object makes it possible to debug code initially
	! by printing many variables and objects, and the the printing can be stopped during a pause by changing the value
	! by putting during the pause ]Printresult[ into zero. If variable ]Printpause[ has
	! the same value as ]Printresult[, then a pause is generated after printing.
	!In the version 28.6.2023 'printing' a TRANS object means taht the trasnforamtion is computed. This gives
	! still further possibilities for debugging.
	!endheader
	! Note Controlling the printing with ending semicolon is a property stolen from
	! Matlab. In order to avoid possible lawsuits, it works differently: in Matlab
	! ending semicolon prevents printing. In Jlp22 this printing is more powerful as
	! there are two different printing levels, and the the printing can be changed
	!without changing the script.
	! endnote
	!endsection
 
	!	write(6,*)'PR:',io,':',j_o(iob)%i(io:io+10)
 
	ilevel=j_v(j_ivprintresult)
	ilevelpause=j_v(j_ivprintpause)
 
	!	write(6,*)'level,ilevelpause,ilevel',level,ilevelpause,ilevel
	if(ilevel.eq.level.or.ilevel.eq.3)then
 
		if(j_o(iob)%i(io+1).eq.1)then
			irg=j_o(iob)%i(io+2)
			!		call j_getname(irg)
			!		write(6,*)'irg',irg,j_otype(irg),j_oname(1:j_loname)
			if(j_otype(irg).eq.j_ipchar)then
				call j_getname(irg)
				write(6,*)j_oname(1:j_loname),"='",j_o(irg)%txt(1)(1:j_o(irg)%i(1)),"'"
				!	call j_getchar(irg,j_cline,le);if(j_err)return
				!	write(6,'(a)')j_cline(1:le)
				return
			elseif(j_otype(irg).eq.j_iptrans)then
				call dotrans(irg,1)
				if(j_err)return
				if(ilevelpause.eq.level.or.ilevelpause.eq.3)call j_pause('<Printpause>',do=.true.)
				return
			endif !if(j_otype(irg).eq.j_ipchar)  15126
		endif !if(j_o(iob)%i(io+1).eq.1)  15122
		!	write(6,*)'print ',io,' * ',j_o(iob)%i(io:io+7)
		call print(iob,io)
 
		if(ilevelpause.eq.level.or.ilevelpause.eq.3)call j_pause('<Printpause>',do=.true.)
	endif !if(ilevel.eq.level.or.ilevel.eq.3)  15120
 
	return
end subroutine printresult !subroutine printresult(iob,io,level)
 
subroutine simulate(iob,io) ! Jlp22-function simulate
 
 
 
end subroutine simulate !subroutine simulate(iob,io)
 
subroutine simulator(iob,io) !simulator
 
 
	return
end subroutine simulator !subroutine simulator(iob,io)
 
subroutine next(iob,io) ! next-function in a simulator
 
	narg=j_o(iob)%i(io+1)
	do i=1,narg
		j_nextnodes(j_iper)=j_nextnodes(j_iper)+1
		j_istarts(j_iper+1,j_nextnodes(j_iper))=j_o(iob)%i(io+1+i)
	end do !i=1,narg  15163
	!io=io+narg+3
	return
end subroutine next !subroutine next(iob,io)
 
subroutine branch(iob,io) ! next-function in a simulator
 
	narg=j_o(iob)%i(io+1)
	do i=1,narg
		iarg=j_o(iob)%i(io+1+i)
		if(any(iarg.eq.j_istarts(j_iper,1:j_nextnodes(j_iper-1))))cycle
		j_nextnodes(j_iper-1)=j_nextnodes(j_iper-1)+1
		j_istarts(j_iper,j_nextnodes(j_iper-1))=iarg  !o(iob)%i(io+1+i)
	end do !i=1,narg  15174
	!io=io+narg+3
	return
end subroutine branch !subroutine branch(iob,io)
 
subroutine cut(iob,io) !cut-function in a simulator
 
	narg=j_o(iob)%i(io+1)
	j_nextnodes(j_iper)=0
	!io=io+narg+3
	return
end subroutine cut !subroutine cut(iob,io)
 
subroutine endtrt(iob,io)  !endtrt-function generated by simulator function io?
 
	if(j_iper.ge.j_nper)then
		if(j_keepper.lt.j_nper)then
			500  	continue
			if(j_lastpoint(j_iper).ge.j_nextnodes(j_iper-1))then !aa
				j_iper=j_iper-1
				if(j_iper.le.j_keepper)then
					io=j_ioret
					return
				end if !if(j_iper.le.j_keepper)  15199
				goto 500
			else !if(j_lastpoint(j_iper).ge.j_nextnodes(j_iper-1))then
				j_v(j_ivperiod)=j_iper
				j_lastpoint(j_iper)=j_lastpoint(j_iper)+1
				j_nextnodes(j_iper)=0
				io=j_istarts(j_iper,j_lastpoint(j_iper))
				iob=j_ivsimu    !!!!!!!!!!!!1
				return
			end if !if(j_lastpoint(j_iper).ge.j_nextnodes(j_iper-1))  15197
		else !if(j_keepper.lt.j_nper)then
			io=j_ioret
			return
		end if !if(j_keepper.lt.j_nper)  15195
	end if !if(j_iper.ge.j_nper)  15194
	if(j_nextnodes(j_iper).le.0)then   !nextnodes for period whee next is give
		if(j_iper.eq.j_keepper)then
			io=j_ioret;return
		end if !if(j_iper.eq.j_keepper)  15218
		5000  	continue
		iper1=j_iper-1
		if(iper1.le.0)then
			write(6,*)'*there is no next to start simulation'
			j_err=.true.
			return
		endif !if(iper1.le.0)  15223
		if(j_lastpoint(j_iper).ge.j_nextnodes(j_iper-1))then
			j_iper=j_iper-1
			if(j_iper.le.1)then
				j_iper=0
				!    write(6,*)'loppu tul endtrtpppssä'
				io=j_ioret;return
			else !if(j_iper.le.1)then
				goto 5000
			end if !if(j_iper.le.1)  15230
		end if !if(j_lastpoint(j_iper).ge.j_nextnodes(j_iper-1))  15228
	end if !if(j_nextnodes(j_iper).le.0)  15217
	j_iper=j_iper+1
	j_nextnodes(j_iper)=0  !index according to the next period
	io=j_istarts(j_iper,1)
	j_lastpoint(j_iper)=1
	j_v(j_ivperiod)=j_iper
	return
end subroutine endtrt !subroutine endtrt(iob,io)
 
 
 
subroutine draw(iob,io) ! draw()
	! Section draw draw() Draws a function
	! draw() draws a function. There are three types of curves: (1) ordinary y-x curves, (2) plane
	! curves where a variable is stepped and both y and x are computed from the stepped variable, and (3)
	! polar curves where angle is stepped and radius is computed possibly as a function of the angle.
	! endheader
	!Option
	! Output & 1& FIGURE & The FIGURE object created or updated.
	! x&1&REAL&Variable which is stepped before computing the value of func-> in ordinary curves.
	! u&1&REAL&Variable which is stepped in plane and polar curves.
	! func& N | 1 & CODE &  Code option telling how the y-variable is computed in ordinary curves or how
	! y is computed in plane curves or radius is computed in polar curves.
	! xfunc& N | 1 & CDE &  Code option telling how the x-axes variable is computed from the u-variable
	! in plane curves.
	!urange &N|2& REAL& Ranges for u in plane curves and polear curves
	!points&N|1&REAL&Number of equal distance points used in either xrange or urange
 
	!xrange&0-4&REAL& Range for the x-variable. If there are now arguments Jlp22 tries to
	!find variables var%min and var%max which where computed with stat() with options min->
	!and max->. If there is need to define a smaller range for computing the values as wished
	! for the x-axes, then the figure is first made for the range given in xrange-> and then the new
	! range can be given in show() or drawline() without arguments.
	!yrange&0-4&REAL& Range for the y-axis.
	! mark & N | 1 & REAL | CHAR & The mark used in the plot.
	!Numeric values refer to 15
	! mark types of gnuplot. The mark can be given also as CHAR varible or constant.
	!width & 0 | 1 & REAL & the width of the line
	!endoption
	! Ex drawex Example of draw()
	! fi=draw(func->sin(x),x->x,xrange->(0,2*Pi),color->Blue,continue->)
	! fi=draw(func->cos(x),x->x,xrange->(0,2*Pi),continue->fcont,color->Red,append->)
	!** Circle
	!fi=draw(func->3,u->u,urange->(0,2*Pi),color->Red,continue->)
	!fi=draw(func->4,u->u,urange->(0,2*Pi),color->Blue,axes->0,continue->fcont)
 
	! **Spiral with too few points
	! fi=draw(func->(0.1*u*sin(u)),xfunc->(0.1*u*cos(u)),u->u,urange->(0,100),color->Blue,continue->fcont)
	! **Spiral with more points
	! fi=draw(func->(0.1*u*sin(u)),xfunc->(0.1*u*cos(u)),u->u,urange->(0,100),points->500,
	! continue->fcont,color->Orange,width->2,axes->0)
	! ;if(type(figyx).ne.FIGURE)plotyxex
	! show(figyx,continue->fcont)
	! reg0=regr(y,x)
	! stat(data->datyx,min->,max->)
	! figyx=draw(func->reg0(),x->x,xrange->,color->Violet,append->,continue->fcont)
	! transa=trans()
	! x2=x*x
	! /
	! reg2=regr(y,x,x2,data->datyx,trans->transa)
	! transa=trans()
	! x2=x*x
	! fu=reg2()
	! /
	!Continue=1  !Error
	! figyx=draw(func->transa(fu),xrange->,color->Orange,append->,continue->fcont)
	!continue=0
	! figyx=draw(func->transa(fu),x-x,xrange->,color->Orange,append->,continue->fcont)
	! fi=draw(func->sin(u),xfunc->cos(u),x->u,xrange->(0,0.5*Pi),continue->fcont)
	! **there is some problem with xrange
	!Continue=1  !Errors
	!fi=draw(func->sin(x),x->x)
	!fi=draw(xrange->(1,100),func->Sin(x),x->x)
	!Continue=0
	! endex
	!Note When xfunc-> is present the the variable given in x-> is stepped in range given in xrange->
	! and x-axes variable is computed using xfunc->.
	!endnote
	!Note ]fi[=draw() produces or updates file ]fi[.jfig] which contains
	! gnuplot commands and file ]fi[.jfi0 containg data. If scandic characters are needed
	! for labels, the edit these characters into fi.jfig and use show('fi.jfig')
	!endnote
	!Note It is possible to have different range for the x-axes range and range for stepping
	! the x-variable. This is useful e.g. when the function is not defined for zero, which would
	! be nice lower in limit for the axes. I need to check how this works.
	!endnote
	!Note Note the difference of xrange-> in classify() and draw-> when xfunc-> is present.
	!In draw->, x-> and xrange-> refer to the background variable which is stepped in xrange->
	!and the x-variable in figure is then computed using xfunc->. In classify() xrange-> and
	!xfunc refer to the same variable.
	!endnote
	!endsection
 
 
 
	real ::funcval
	character*3 ch
	character*2 ch0
	logical p
	logical ::isu,isxfunc
	double precision::umin,umax
	j_gpwasshown=.false.   !draw
	call j_startfunction(iob,io,0,narg,j_arg,ivfig);if(j_err)return
	j_ivfig=ivfig
	j_gpappend=j_isoption(iob,io,j_mappend)  !draw
	p=j_v(j_ivdebug).gt.j_0
	linkfunc=j_codelink(iob,io,j_mfunc); if(j_err)return
	j_gpline=' '
	j_gplenline=0
	if(p)write(6,*)'linkfunc',linkfunc,'teku:'
	if(p)write(6,'(20i5/)')j_o(iob)%i(0:j_o(iob)%i(0))
	if(linkfunc.eq.0)then
		write(6,*)'**draw: function not defined';goto 99
 
	endif !if(linkfunc.eq.0)  15349
	linkxfunc=j_codelink(iob,io,j_mxfunc); if(j_err)return
	isxfunc=linkxfunc.gt.0
	if(p)write(6,*)'linkxfunc',linkxfunc
	!j_gpline=' '
	!	j_lenline=0
	call j_getoption(iob,io,j_mpoints,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
	npoints=100
	if(noptarg.gt.0)npoints=j_v(j_optarg0(1))
 
 
	call j_deffig(iob,io,ivfig,100,200)
	call j_gpfigoption(iob,io,ivfig)
	if(j_err)return
 
	call j_getoption(iob,io,j_mu,-1,1,0,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.eq.1)then
		ivu=j_optarg0(1)
		isu=.true.
		call j_getoption(iob,io,j_murange,2,2,j_ipreal,.true.,noptarg,j_optarg0)
		if(j_err)return
		umin=j_v(j_optarg0(1))
		umax=j_v(j_optarg0(2))
		if(umax.le.umin)then
			write(6,*)'umin ',umin,' >=  umax ',umax
			j_err=.true.;return
		endif !if(umax.le.umin)  15376
	else
		isu=.false.
		if(isxfunc)then
			write(6,*)'xfunc-> requires that argument for both func-> and xfunc-> is give in u->'
			j_err=.true.;return
 
		endif !if(isxfunc)  15382
		if(j_gpix.eq.0)then
			write(6,*)'**x-> missing'
			j_err=.true.
			return
		endif !if(j_gpix.eq.0)  15387
	endif !if(noptarg.eq.1)  15369
 
	! j_gpline="'"//j_filename(3:le)//' i '//ch(iaa:3)//' w l'//j_gplw)
	! if(j_filename(2:2).eq.'$')then
	! call j_gpputfig(ivfig,j_gpplot,ch0//"'"//j_filename(3:le)//' i '//ch(iaa:3)//' w l'//j_gplw)
	! else
	! call j_gpputfig(ivfig,j_gpplot,ch0//j_filename(1:le)//' i '//ch(iaa:3)//' w l'//j_gplw)
	! endif !if(j_filename(2:2).eq.'$')  14716
	!	write(6,*)'<%%>',j_gpcolors(j_gpicolor)
 
	j_gpline(j_gplenline+1:)="'-' w l"//j_gplw
	j_gplenline=len_trim(j_gpline)
	if(j_gpicolor.ge.1)then
		j_gpline(j_gplenline+1:)=j_gpcolors(j_gpicolor)
		j_gplenline=len_trim(j_gpline)
	endif !if(j_gpicolor.ge.1)  15404
	call j_gpputfig(ivfig,j_gpplot,word=j_gpline)  ! ,add=.true.)
 
 
	!	write(6,*)'draw8io ',io
	!	p=.true.
	if(isu)then
		dxx=(umax-umin)/npoints
		!	write(6,*)'npoints',npoints
		xx=umin
		!	write(6,*)'lix',linkxfunc,'range',j_o(ivfig)%d(1:2)
		!	write(6,*)' d ',j_o(ivfig)%d(5:6), 'xx ',xx,'points, ',j_gppoints,'dxx',dxx
 
		do i=1,npoints
			if(i.eq.npoints)xx=umax
			j_v(ivu)=xx
			!call dotrans(iob,iofunc)
			if(j_err)goto 99
 
			yyy=j_codevalue(iob,linkfunc)
			if(j_err)return
			if(isxfunc)then
				xxf=j_codevalue(iob,linkxfunc)
				if(j_err)return
 
			else
				xxf=cos(xx)*yyy
				yyy=sin(xx)*yyy
			endif !if(isxfunc)  15428
			call j_gpputfigxy(ivfig,xxf,yyy)
 
			xx=xx+dxx
		end do !i=1,npoints  15420
 
 
 
 
	else
 
 
 
		if(j_o(ivfig)%d(5).eq.j_inf.or.j_o(ivfig)%d(6).eq.j_inf)then
			write(6,*)'** xrange-> not given in draw';goto 99
		end if !if(j_o(ivfig)%d(5).eq.j_inf.or.j_o(ivfig)%d(6).eq.j_inf)  15448
 
		! if(j_gpix.le.0)then
		! write(6,*)'x-> not given in draw';goto 99
		! end if !if(j_gpix.le.0)  15280
 
		if(j_o(ivfig)%d(5).ge.j_o(ivfig)%d(6))then
			write(6,*)'**draw: xrange (',j_o(ivfig)%d(5),',',j_o(ivfig)%d(6),') not properly defined';goto 99
		end if !if(j_o(ivfig)%d(5).ge.j_o(ivfig)%d(6))  15456
		if(j_err)goto 99
 
		dxx=(j_o(ivfig)%d(6)-j_o(ivfig)%d(5))/npoints
 
		xx=j_o(ivfig)%d(5)
		!	write(6,*)'lix',linkxfunc,'range',j_o(ivfig)%d(1:2)
		!	write(6,*)' d ',j_o(ivfig)%d(5:6), 'xx ',xx,'points, ',j_gppoints,'dxx',dxx
 
		do i=1,npoints
			if(i.eq.npoints)xx=j_o(ivfig)%d(6)
			j_v(j_gpix)=xx
			!call dotrans(iob,iofunc)
			if(j_err)goto 99
 
			yyy=j_codevalue(iob,linkfunc)
			if(j_err)return
			if(linkxfunc.gt.0)then
				xxf=j_codevalue(iob,linkxfunc)
				call j_gpputfigxy(ivfig,xxf,yyy)
				!	write(nu,*)xxf,yyy
				!		write(6,*)xx,xxf,yyy
			else
				call j_gpputfigxy(ivfig,xx,yyy)
				!	write(nu,*)xxf,yyy
				!write(nu,*)xx,yyy
			endif !if(linkxfunc.gt.0)  15475
			xx=xx+dxx
		end do !i=1,npoints  15467
	endif !if(isu)  15413
	!write(6,*)'bef e'
	!	call j_figdump(ivfig)
	call j_gpputfige(ivfig)  ! draw
	!	write(6,*)'aft e'
	!	call j_figdump(ivfig)
	!	if(p)write(6,*)'<000667io  ',io
	!write(6,*)'<747close ',nu,'nul',nul
	!	call j_closeunit(nu)
 
	if(j_gpshow)call j_showfig(ivfig)
 
	!shown later
	! write(6,*)'<011166aftshowfig7io  ',io,' recursion ',j_recursion
	! write(6,'(20i5)')j_o(iob)%i(1:j_o(iob)%i(0))
	! write(6,'(20i5)')j_o(iob)%i(io:io+19)
 
	900 continue !  if(j_err)return

	return
	99 	j_err=.true.

	!io=io+j_fig_narg+3  !uusi

	return
end subroutine draw !subroutine draw(iob,io)
 
 
 
subroutine drawline(iob,io)
	! Section drawline drawline() Draws a polygon through points.
	! endheader
	!Option
	! Output & 1& FIGURE & The FIGURE object created or updated.
	!Args&& &Numeric values given with any combination of numbers, matrices and lists. If there is
	!x-> option referring to a MATRIX, those values give the x-coordinates, otherwise
	! first half of values give x-values and rest y-values. If number of y-values is a multiple
	! of x-values given in x->, then the x-values are repeated.
 
	!@@figure@
	! label & N | 1 & CHAR & Label written to the end of line. If arguments define only one point,
	! then with label-> option one can write text to any point.
	!step&N|1&REAL& Points are taken at steps given in step->
 
	!points &N|0|1&REAL&In addtion to lines, also points are plotted.
	!pointsonly&N|0&&Only points are drawn.
	! break & N | 0 & & The line is broken when a x-value is smaller than the previous one.
	! filter &0|1 &CODE & Code option defined in terms of $x, $xold,$y and $yold that
	! points not passing the filter are dropped. For first point $xold=$x and $yold=$y
	! reject &0|1 &CODE & Code option defining in terms of $x, $xold,$y and $yold when
	! a point is rejected and a break generated. For first point $xold=$x and $yold=$y.
	! mark & N | 1 & REAL | CHAR & The mark used in the plot.
	!Numeric values refer to gnuplot mark types between 1-75.
	! The mark can be given also as CHAR varible or constant.
	!width & 0 | 1 & REAL & the width of the line. Default: width->1
	!label& N |1 &CHAR & Text plotted to the end of line.
	!in &N|1&CHAR & Name of file from which coordinatee values are read. Each line contains
	!the x-coordinate and y-coordinate. An empty line means that there is a break in the line.
	!endoption
	!Note Both  fi=drawline(x,y,mark->'*',append->) and fi=drawline(x,y,label->'*',append->) can
	! be used to put the mark into a figure, but mark is centered at the point but
	! for a label x and y give the upper-left coordiantes.
	!endnote
	!Note Using drawline() without arguments and with append-> and with xlabel->,ylabel->,
	!xrange-> and/or yrange->
	! can be use to define axes after making the figure.
	!endnote
	!Note A useful tactic to add labels to a figure ]fi[ and fine tune them, is to
	! add	a section into the include file where the figure is copied into a working figure ]fi0[
	! using ]fi0[=]fi[. Then the finetuning the figure can be done using this section
	!without the need to start making the figure several times using the grpahics functions.
	!endnote
	!Note The file given in in-> option is read directly by gnuplot. Binary formats are includes as needed.
	!endnote
	!Note If the label extends the xrange of the figure, the gnuplot does not extend xrange,
	! it must be extended explicitly.
	!endnote
	! of
	! Ex drawlineex Example of drawline()
	! fi=draw(func->sin(x),x->x,xrange->(0,2*Pi),color->Blue,continue->fcont)
	! fi=drawline(Pi,sin(Pi)+0.1,label->'sin()',append->,continue->fcont)
	! xval=matrix(do->(1,10));
	! mat=matrix(values->(xval,xval+1,xval,xval+2,xval,xval+3))
	! fi=drawline(mat,color->Red,continue->fcont)
	! fi=drawline(mat,color->Orange,break->,continue->fcont)
	! xm=matrix(do->(0,100,1))
	! e=matrix(101)
	! e=rann(0,3)
	! ym=2*x+0.3*xm*.xm+0.4+e
	! dat=newdata(xm,ym,read->(x,y),extra->(Regf,Resid))
	! reg=regr(y,x)
	! figyx=plotyx(y,x,continue->fcont)
	! figr=plotyx(Resid,x,continue->fcont)
	! reg0=regr(y,x)
	! stat(min->,max->)
	! figyx=draw(func->reg0(),x->x,xrange->,color->Violet,append->,continue->fcont)
 
	! transa=trans()
	! x2=x*x
	! if(type(reg2).eq.REGR)fu=reg2()
	! /
	! reg2=regr(y,x,x2,trans->transa)
	! figyx=draw(func->transa(fu),x->x,xrange->,color->Orange,append->,continue->fcont)
	! fi=drawline(matrix(6,do->),continue->fcont)
	! fi=drawline(1,2,1,2,3,4,3.4,5,continue->fcont)
	! fi=drawline(1,2,1,2,3,4,3.4,5,break->,continue->fcont)
	! xt=matrix(10,do->)
	! yt=matrix(10,do->)
	! yt(5)=0
	! fi=drawline(xt,yt,show->0)
	! fi=drawline(xt,yt+1,show->0,filter->$x.ne.5,color->Blue,append->)
	! fi=drawline(xt,yt+2,continue->fcont,reject->$x.eq.5,color->Red,append->)
	! ;if(wait);pause
	! ;return
	!
	! endex
	!Note if a line is not visible, this may be caused by the fact that
	!the starting or ending point is outside the range specified by xrange-> or yrange->.
	!endnote
	!endsection
 
 
 
	logical ::p,isbreak,exis !makeplot,
	logical ::ispointsonly,isfilter,isreject,iscode,breaknow,isx,rejected,isdx,isin
	logical ::ispoints,isfile,filtered
	integer ::istep
	character*2 ch
	real ::x,y,xold,yold
 
	!drawline
 
	double precision,dimension(:),allocatable::argval
	j_gpwasshown=.false.  !drawline
	p_p=j_v(j_ivdollar2).eq.777.d0
	if(j_p)write(6,*)'drawlinbe',p_p
	call j_startfunction(iob,io,0,narg,j_arg,ivfig);if(j_err)return
 
	j_ivfig=ivfig
	call j_getoption(iob,io,j_min,-1,1,j_ipchar,.true.,noptarg,j_optarg0,gotchar=j_filename,lengot=le,isfile=isfile)
	if(j_err)return
	if(noptarg.gt.0)then
		j_gpline="'"//j_filename(1:le)//"' "
		j_gplenline=len_trim(j_gpline)+1
		nval=0
		j_gplastx=j_gpdif
	else
		isfile=.false.
		j_gpline="'-'"
		j_gplenline=3
	endif !if(noptarg.gt.0)  15628
	!	call j_startfig(iob,io,xtype=j_ipmatrix)  !j_gpix
	!	write(6,*)'gpshow',j_gpshow
	j_gpappend=j_isoption(iob,io,j_mappend,.false.)  !drawline
	!write(6,*)'gpappend',j_gpappend
	!call j_figdump(ivfig)
	if(j_err)return
 
	! le=le+2
	if(.not.isfile.and.narg.gt.0)then
 
		call j_arg2val(j_arg,narg,j_gpval,nval)
		if(nval.lt.2)then
			write(6,*)'*data points not given'
			j_err=.true. ;return
 
		endif !if(nval.lt.2)  15649
 
		np=nval/2
		j_gplastx=j_gpval(np)
		j_gplasty=j_gpval(nval)
 
		!write(6,*)'nval',nval,j_gpval(1:nval)
		!if(j_gpset.gt.0)then
		! j_filename(1:3)="'-'"
		! le=3
 
		call j_getoption(iob,io,j_mdx,-1,1,j_ipreal,.true.,ndx,j_optarg0)
		if(j_err)return
		if(ndx.gt.0)then
 
			ndx=j_v(j_optarg0(1))
			if(dble(ndx).ne.j_v(j_optarg0(1)))then
				write(6,*)'dx-> in drawline is not integer ',j_v(j_optarg0(1))
				j_err=.true.;return
 
			endif !if(dble(ndx).ne.j_v(j_optarg0(1)))  15669
			isdx=.true.
		else
			isdx=.false.
		endif !if(ndx.gt.0)  15666
 
 
		isbreak=j_isoption(iob,io,j_mbreak)
		call j_getoption(iob,io,j_mstep,-1,1,j_ipreal,.true.,nstep,j_optarg0)
		if(j_err)return
		istep=1
		if(nstep.gt.0)istep=j_v(j_optarg0(1))
		if(istep.le.0)then
			write(6,*)'step must be positive integer, was ',j_v(j_optarg0(1)),  '=> ',istep
			j_err=.true.;return
		endif !if(istep.le.0)  15685
		!	write(6,*)'hep',j_ipmatrix
		!(iob,io,moption,minarg,maxarg,iptype,needsarg,noptarg,optarg,link)
		!call j_getoption(iob,io,j_mx,-1,1,j_ipmatrix,.true.,nx,j_optarg0)
		if(j_err)return
		isx=j_gpix.gt.0
		!
		! if(isx)then
 
		! nxval=j_o(j_gpix)%i(3)
 
		! endif !if(isx)  14983
 
		linkreject=j_codelink(iob,io,j_mreject); if(j_err)return
		isreject=linkreject.gt.0
		linkfilter=j_codelink(iob,io,j_mfilter); if(j_err)return
		isfilter=linkfilter.gt.0
		iscode=.false.
		if(isreject.or.isfilter)then
			ivx=j_object('$x')
			ivxold=j_object('$xold')
			ivy=j_object('$y')
			ivyold=j_object('$yold')
			iscode=.true.
		endif !if(isreject.or.isfilter)  15706
 
	endif !if(.not.isfile.and.narg.gt.0)  15646
	!write(6,*)'filnam', j_filename
 
 
 
	!write(6,*)'append',j_gpappend,'befdef'
	!	call j_figdump(ivfig)
	call j_deffig(iob,io,ivfig)
	!	write(6,*)'ivfig',ivfig
	!	write(6,*)'append',j_gpappend,'aftdef'
	!	call j_figdump(ivfig)
 
	call j_gpfigoption(iob,io,ivfig)
	!	write(6,*)'append',j_gpappend,'aftfigopt'
	!	call j_figdump(ivfig)
 
	!	onlylabel=
 
	! pointsonly  ispoints=lines+points  default lines
 
	if(j_err)return
	!	write(6,*)'***',nval.le.2,j_gpislabel,narg.eq.0,nval.le.2.and.j_gpislabel.or.narg.eq.0
	if((nval.le.2.and..not.j_gpismark).and.j_gpislabel.or.narg.eq.0)then
		if(j_gpshow)call j_showfig(ivfig)
		return
	endif !if((nval.le.2.and..not.j_gpismark).and.j_gpislabel.or.narg  15736
	ispointsonly=j_isoption(iob,io,j_mpointsonly)
	if(np.eq.1.and..not.j_gpislabel)ispointsonly=.true.
	ispoints=j_isoption(iob,io,j_mpoints).or.j_gpismark
 
	p=j_v(j_ivdebug).gt.j_0
 
	!	write(6,*)'ismark,ispoints',j_gpismark,ispoints
 
	!else
	!	write(6,*)'cAAAAb,ispointsonly,j_gpstyle,j_gpmark ,j_gpplot', ispointsonly,j_gpstyle,j_gpplot
	if(ispointsonly) then
		j_gpline(j_gplenline+1:)=' w p'//j_gppt//j_gpps
		j_gplenline=len_trim(j_gpline)
		!	call j_gpputfig(ivfig,j_gpplot,ch//j_filename(1:le)//' w p'//j_gppt//j_gpps)
	elseif(ispoints)then
		!	write(6,*)'**',ch//j_filename(1:le)//' w lp'//j_gppt//j_gpps//j_gplw
		j_gpline(j_gplenline+1:)=' w lp'//j_gppt//j_gpps//j_gplw
		j_gplenline=len_trim(j_gpline)
 
		!		call j_gpputfig(ivfig,j_gpplot,ch//j_filename(1:le)//' w lp'//j_gppt//j_gpps//j_gplw)
 
	else  !if(j_gpstyle.gt.0.and.j_gpmark.ne.j_ivzero)then
		j_gpline(j_gplenline+1:)=' w l'//j_gplw
		j_gplenline=len_trim(j_gpline)
 
		!	call j_gpputfig(ivfig,j_gpplot,ch//j_filename(1:le)//' w l'//j_gplw)
 
	endif !if(ispointsonly)  15750
	!	endif !if(j_filename(2:2).eq.'$')  15061
	!	write(6,*)'<%>',onlylabel,j_gpicolor,j_gpcolors(j_gpicolor)
 
	!	write(6,*)'linehere',onlylabel,j_gpline
	if(j_gpicolor.ge.1)then
		!call j_gpputfig(ivfig,j_gpplot,j_gpcolors(j_gpicolor))  !  ,add=.true.)
		j_gpline(j_gplenline+1:)=j_gpcolors(j_gpicolor)
		j_gplenline=len_trim(j_gpline)
	endif !if(j_gpicolor.ge.1)  15772
	!write(6,*)j_o(ivfig)%txt(6)
 
	call j_gpputfig(ivfig,j_gpplot,word=j_gpline(1:j_gplenline),le=j_gplenline)
	!write(6,*)'word',j_gpline(1:j_gplenline)
	!	endif !if(makeplot)  15020
	!write(6,*)'<464664>',j_gpnarg,exis
	!	if(exis)return
	!	iarg=j_arg(1)
	!write(6,*)'<464664>',j_gpnarg
	if(isfile)then
		if(j_gpshow)call j_showfig(ivfig)
		return
	endif !if(isfile)  15786
 
 
	ili=j_o(ivfig)%i(1)
 
 
	! endif
 
	!j_o(ivfig)%txt(ili)="plot '-'"
	! if(isx)then
	! nset=nval/nxval
 
	! if(nset*nxval.ne.nval)then
	! write(6,*)'number of values ',nval,' is not multiple of values in x->'
	! j_err=.true.;return
 
	! endif !if(nset*nxval.ne.nval)  15080
	! npoints=nval
 
	!	else
	npoints=nval/2
	!	endif !if(isx)  15077
	!	if(.not.onlylabel)then
	!	if(j_gpset.eq.0.and..not.onlylabel)then
	!write(6,*)'npoints',npoints,'istep',istep
	if(isdx)then
		npiece=npoints/ndx
		!write(6,*)'npiece ',npiece
		if(npiece*ndx.ne.npoints)then
			write(6,*)'number of points ',npoints,' is not divisible with dx->',ndx
			j_err=.true.;return
 
		endif !if(npiece*ndx.ne.npoints)  15817
 
	endif !if(isdx)  15814
 
	breaknow=.false.
	!if(.not.onlylabel)then
	ix=0
	nac=0
	ibreak=-1
	nwarn=0
	inpiece=0
	!		write(6,*)'npoints ,istep,isbreak,',npoints,istep,isbreak,'breaknow',breaknow,'isreject',isreject
	!		write(6,*)'iscode',iscode
	!		call j_figdump(ivfig)
 
 
	do ip=1,npoints,istep
		!		write(6,*)'<88ip',ip,'ivfig',ivfig
		if(ip.gt.1)then
			xold=x
			yold=y
		endif !if(ip.gt.1)  15839
		! if(isx)then
		! ix=ix+1
		! if(ix.gt.nxval)ix=1
		! if(ix.eq.1.and..not.breaknow.and.ip.gt.0)call j_gpputfigbreak(ivfig)
		! x=j_o(j_gpix)%d(ix)
		! y=j_gpval(ip)
		! else
		x=j_gpval(ip)
		y=j_gpval(npoints+ip)
		!	endif !if(isx)  15119
		if(ip.eq.1)then
			xold=x
			yold=y
		endif !if(ip.eq.1)  15853
		breaknow=.false.
		if(isbreak.and.ip.gt.1)then
			if(j_gpval(ip).lt.j_gpval(ip-1))breaknow=.true.
		endif !if(isbreak.and.ip.gt.1)  15858
		if(iscode)then
			if(ivx.gt.0)j_v(ivx)=x
			if(ivy.gt.0)j_v(ivy)=y
			if(ivxold.gt.0)j_v(ivxold)=xold
 
			if(ivyold.gt.0)j_v(ivyold)=yold
			!			write(6,*)'ivx,ivy,ivxold,ivyold',ivx,ivy,ivxold,ivyold,x,y,xold,yold
 
		endif !if(iscode)  15861
		rejected=.false.
		if(isreject)then
			j_dapu=j_codevalue(iob,linkreject)
			if(j_err)return
			if(j_dapu.ne.j_0)then
				breaknow=.true.
				rejected=.true.
			endif !if(j_dapu.ne.j_0)  15874
		endif !if(isreject)  15871
		!write(6,*)'ip,rejected,breaknow',ip,rejected,breaknow
		j_yes=.true.
		if(isfilter)j_yes=j_codevalue(iob,linkfilter).ne.j_0
		!		write(6,*)'yes,rejected,breaknow',j_yes,rejected,breaknow,'isreject,isfilter,iscode',isreject,isfilter,iscode
		if(breaknow)then
			if(ip.eq.ibreak+1)then
				if(nwarn.le.4)then
					write(6,*)'consecutive break, point ',ip,'$x,$y,$xold,$yold ',x,y,xold,yold
 
				endif !if(nwarn.le.4)  15885
				nwarn=nwarn+1
			else
				! if(j_gpset.eq.0)then
				call j_gpputfigbreak(ivfig)
				! else
				!call j_gpputfigbreak(nu=nu)
				!endif !if(j_gpset.eq.0)  15182
			endif !if(ip.eq.ibreak+1)  15884
			ibreak=ip
		endif !if(breaknow)  15883
 
		if(j_yes.and..not.rejected)then
			nac=nac+1
			!		write(6,*)'nac',nac,ivfig
			!	call j_figdump(ivfig)
 
			call j_gpputfigxy(ivfig,x,y)
 
			!	write(6,*)'aftdjjdj:,isdx',isdx
			!	call j_figdump(ivfig)
			!	else
			!		call j_gpputfigxy(x,y,nu=nu)
			!	endif !if(j_gpset.eq.0)  15193
			if(isdx)then
				inpiece=inpiece+1
				!write(6,*)'inpiece',inpiece,ndx
				if(inpiece.eq.ndx)then
					!	if(j_gpset.eq.0)then
					call j_gpputfigbreak(ivfig)
					!	else
					!	call j_gpputfigbreak(nu=nu)
					!	endif !if(j_gpset.eq.0)  15202
					inpiece=0
 
				endif !if(inpiece.eq.ndx)  15915
 
			endif !if(isdx)  15912
		endif !if(j_yes.and..not.rejected)  15900
		!	write(6,*)'IPHERE',IP,'dumpfolo'
		!		call j_figdump(ivfig)
 
 
	enddo !ip=1,npoints,istep  15837
	if(nac.le.0)then
		if(nwarn.gt.0)write(6,*)nwarn,' consecutive breaks '
		write(6,*)'nothing accepted'
		j_err=.true.;return
	endif !if(nac.le.0)  15932
	!if(j_gpset.eq.0)then
	call j_gpputfige(ivfig)    !'e')   drawline
	!	else
	!		call j_closeunit(nu)
	!endif !if(j_gpset.eq.0)  15221
	!endif !if(.not.onlylabel)  15200
	!	write(6,*)'******'
	!	call j_figdump(ivfig)
 
	deallocate(j_gpval)
	!	write(6,*)'ii2befsho',ivfig,j_o(ivfig)%i2
	!	call j_figdump(ivfig)
	!	call j_figshowoption(iob,io,ivfig)
	!write(6,*)'aft figshow'
	!call j_figdump(ivfig)
	!	write(6,*)'gpshow,gpcont',j_gpshow,j_gpcontinue,'********'
	!	call j_figdump(ivfig)
 
	if(j_gpshow)call j_showfig(ivfig)
	!	write(6,*)'figaftshow  ',ivfig
	!if(j_gpshow)call j_showfig(ivfig)
	!	io=j_gpnewio
	return
	contains
 
	integer function npoints2()
		npoints2=npoints/istep
		if(npoints2.lt.1)then
			write(6,*)npoints,' values with step ',istep,' leads ',npoints2,' points, illegal'
			j_err=.true.;return
		endif !if(npoints2.lt.1)  15964
		npoints2=npoints2+1
		return
	end function
end subroutine !subroutine drawline(iob,io)
 
 
 
 
 
 
subroutine drawclass(iob,io) ! draw()
	! Section drawclass drawclass() Draws results of classify()
	! drawclass() can plot class means and/or lines connecting class means, with
	! or without standard errors of class means, within class standard deviations,
	! within class variances, frequency histograms, which can be scaled so that
	! density funtions can be drawn in the same figure.
	! endheader
	! Option
	! Output &1 & FIGURE & FIGURE object updated or generated.
	! Arg & 1& MATRIX & A MATRIX generated with classify().
	! se & N | 0 & &Presence of option tells to include that error bars showing standard errors
	! of class means computed as sqrt(sample_within-class_variance)/number_of_obs). If se-> is present,
	!also the mean curve is drawn.
	! sd & N | 0 & & Within-class standard deviations are drawn, if there is no mean->.
	! If there is also mean-> or se-> option then sd-error bars are drawn
	! around mean. If both se-> error and sd-> are present, then the se error bars and
	! sd error bars can have different color and width. If color-> and/or width-> has two
	! arguments, the first argument refers to sd-> error bars, and the second argument refers to
	! se-> error bars. Using width->(1,2) or width->(2,4) gives nice looking error bars.
 
	! mean & N | 0 & & Class mean are drawn. Has effect only together with sd->.
	! var & N | 0 &  &Within-class sample variances are drawn.
	! histogram &  N | 0 &  &Within-class sample variances are drawn.
	! freq & N | 0 &  & Absolute  frequences are drawn in histogram. Default
	!percentage if area-> is not present
	! area & N |0 & &the histogram is scaled so that that it can be overlayed to density function
	! cumulative &N|0 & & cumulativer histogram is drawn. If freq-> is presented then absolute
	! cumulative frequences are drawn, otherwise cumulative percentages are draw, except if also area->
	! is presenet then then cumulative realtive freaquences are drawn.
	! endoption
	! Ex drawclassex Examples of drawclass()
	! X=matrix(do->(1,100,0.1))
	! e=matrix(nrows(X))
	! e=rann()
	! X2=0.01*X*.X !elementwise product
	! Y=2*X+0.01*X2+(1+0.3*X)*.e  !nonequal error variance,quadratic function
	! dat=newdata(X,Y,X2,read->(x,y,x2),extra->(Regf,Resid))
	! stat(min->,max->)
	! reg=regr(y,x) ! Regf and resid are put into the data
	! fi=plotyx(y,x,continue->fcont)
	! fi=drawline(x%min,x%max,reg(x%min),reg(x%max),width->3,color->Cyan,append->,continue->fcont)
	! cl=classify(Resid,x->x,xrange->,classes->5)
	! fi=drawclass(cl,sd->,se->,continue->fcont,color->Red,width->(2,4))
	! fi=drawclass(cl,sd->,continue->fcont)
	! fi=drawclass(cl,var->,continue->fcont)
	!** x-values were equally distributed due to data construction
	! fi=drawclass(cl,histogram->,area->,continue->fcont)
	! fi=draw(func->pdf(0,rmse(reg)),x->x,xrange->,append->,continue->fcont) ! xrange comes from stat()
	! endex
	! Note In previous versions of Jlp22 if se-> and sd-> were both present, the error
	! both bars were plotted. This possibility will be included later.
	! endnote
	! endsection
 
	!drawclass
 
	real ::xx,yy,cumu
	logical histo,area,freq,isse,issd,isvar,iscumu,ismean
	character*2 ch
	j_gpwasshown=.false.  ! drawclass
	call j_startfunction(iob,io,0,narg,j_arg,ivfig);if(j_err)return  !drawclass
	j_ivfig=ivfig
	j_gpappend=j_isoption(iob,io,j_mappend)  !drawclass
	call j_deffig(iob,io,ivfig,100,200)
	call j_gpfigoption(iob,io,ivfig)
	if(j_err)return
	!j_getoption(iob,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) !
	call j_getoption(iob,io,j_mmean,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	ismean=noptarg.ge.0
	call j_getoption(iob,io,j_mse,-1,0,0,.false.,noptarg,j_optarg0,link=linkse);if(j_err)return
	isse=noptarg.ge.0
	call j_getoption(iob,io,j_msd,-1,0,0,.false.,noptarg,j_optarg0,link=linksd);if(j_err)return
 
	issd=noptarg.ge.0
	call j_getoption(iob,io,j_mvar,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	isvar=noptarg.ge.0
	call j_getoption(iob,io,j_mfreq,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	freq=noptarg.ge.0
 
	call j_getoption(iob,io,j_mhistogram,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	histo=noptarg.ge.0
 
	call j_getoption(iob,io,j_marea,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	area=noptarg.ge.0
	iscumu=j_isoption(iob,io,j_mcumulative)
 
	! integer itype_
	! logical getyrange,freq,histo,area
	! call j_checkoutput(iob,io)
	if(j_err)return
	!!call j_clearoption(iob,io)
 
	iarg=j_arg(1)
	if(j_otype(iarg).ne.j_ipmatrix)then
		call j_printname('**drawclass: ',iarg,' not a matrix')
		j_err=.true. ;return
		!goto 900
	end if !if(j_otype(iarg).ne.j_ipmatrix)  16071
	if(j_o(iarg)%i(4).ne.j_matclass)then
		call j_printname('**drawclass: ',iarg,' not a produced by classify')
		j_err=.true. ;return
		!goto 900
	endif !if(j_o(iarg)%i(4).ne.j_matclass)  16076
	if(j_o(iarg)%i(1).eq.2)histo=.true.
 
 
	nrow_=j_o(iarg)%i(1)
	ncol=j_o(iarg)%i(2)
 
	nloop=1
	if(isse.and.issd)then
		ismean=.true.
 
		nloop=2
	endif !if(isse.and.issd)  16088
	!	write(6,*)'linkselinksd',linkse,linksd
 
	do iloop=1,nloop
		j_gpline="'-'"
		j_gplenline=3
		!first
		if(iloop.eq.2)isse=.true.
 
 
		!!		write(6,*)'isse,issd ',isse,issd
		!write(6,*)',iloop,icur,j_gplw, j_gpl2,j_gpicolor,j_gpicolor2',iloop,icur,j_gplw, j_gpl2,j_gpicolor,j_gpicolor2
		if(isse.or.ismean.and.issd)then
			if(iloop.eq.1)then
 
				j_gpline(j_gplenline+1:)= " w errorl"//j_gplw
			else
				j_gpline(j_gplenline+1:)= " w errorl"//j_gplw2
			endif !if(iloop.eq.1)  16105
 
 
		else !if(isse)then
 
			j_gpline(j_gplenline+1:)= " w l"//j_gplw
 
			!	write(6,*)'w ', j_gplenline," w l"//j_gplw,'*',j_gpline
			!	call j_gpputfig(ivfig,j_gpplot,ch//"'-' w l"//j_gplw)
		endif !if(isse.or.ismean.and.issd)  16104
		j_gplenline=len_trim(j_gpline)
		!write(6,*)'<&&>',j_gpcolors(j_gpicolor)
		if(j_gpicolor.ge.1)then
 
			j_gpline(j_gplenline+1:)=j_gpcolors(j_gpicolor)
			j_gplenline=len_trim(j_gpline)
			write(6,*)'color ', j_gpcolors(j_gpicolor),'*',j_gpline
			!				call j_gpputfig(ivfig,j_gpplot,j_gpcolors(j_gpicolor))  ! ,add=.true.)
			!		else
			!				call j_gpputfig(ivfig,j_gpplot,j_gpcolors(j_gpicolor2))  ! )  ! ,add=.true.)
			!			endif !if(iloop.eq.1)  15630
 
		endif !if(j_gpicolor.ge.1)  16122
 
		!	write(6,*)'gpline ',j_gpline(1:j_gplenline)
		call j_gpputfig(ivfig,j_gpplot,word=j_gpline(1:j_gplenline),le=j_gplenline)
		!	write(6,*)'<84848hui'
		! HISTOGRAM
		if(histo)then
			xmin=j_o(iarg)%d(nrow_*ncol+1)
			dd=j_o(iarg)%d(nrow_*ncol+2)
			xx=xmin
 
			if(freq)then
				scale=1.
 
			else !if(freq)then
				if(area)then
					if(iscumu)then
						scale=j_1/j_o(iarg)%d(2*ncol)
					else
						scale=j_1/(j_o(iarg)%d(2*ncol)*dd)
					endif !if(iscumu)  16148
				else !if(area)then
					scale=100.d0/j_o(iarg)%d(2*ncol)
 
				endif !if(area)  16147
			endif !if(freq)  16143
			vlast=0
			!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin)
			! 1.00000000       0.00000000
			! 1.00000000       37.5000000
			! 1.00000000       37.5000000
			! 1.66666722       37.5000000
 
			!	call j_putor(j_fig_ivout,j_fig_iba+2,0.)
			if(iscumu)then
				!	j_fig_iba=j_fig_iba+2
				call j_gpputfigxy(ivfig,xx,0.)
				cumu=j_0
 
				!	write(6,*)'5445',xx-0.5d0*dd,0,xx,0,xx,scale*j_getmatel(iarg,2,i)
				do i=1,ncol-1
					cumu=cumu+j_getmatel(iarg,2,i) !scale*j_getmatel(iarg,2,i)
					!	if(i.eq.1)call j_gpputfigxy(xx,yy)
					call j_gpputfigxy(ivfig,xx,scale*cumu)
					xx=xx+dd
					!				call j_gpputfigxy(xx,yy)
 
					!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+(i-1)*dd)
					!	call j_putor(j_fig_ivout,j_fig_iba+2,scale*real(j_o(iarg)%d(ncol+i)))
					!	j_fig_iba=j_fig_iba+2
					!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+i*dd)
					!	call j_putor(j_fig_ivout,j_fig_iba+2,scale*real(j_o(iarg)%d(ncol+i)))
					!	j_fig_iba=j_fig_iba+2
 
				enddo !i=1,ncol-1  16172
				!		call j_gpputfigxy(xx,j_0)
				!		call j_gpputfigxy(xx+0.5*dd,j_0)
 
 
			else
				!	j_fig_iba=j_fig_iba+2
				call j_gpputfigxy(ivfig,xx-0.5*dd,0.)
				call j_gpputfigxy(ivfig,xx,0.)
				!	write(6,*)'5445',xx-0.5d0*dd,0,xx,0,xx,scale*j_getmatel(iarg,2,i)
				do i=1,ncol-1
					yy=scale*j_getmatel(iarg,2,i)
					!	if(i.eq.1)call j_gpputfigxy(ivfig,xx,yy)
					call j_gpputfigxy(ivfig,xx,yy)
					xx=xx+dd
					call j_gpputfigxy(ivfig,xx,yy)
 
					!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+(i-1)*dd)
					!	call j_putor(j_fig_ivout,j_fig_iba+2,scale*real(j_o(iarg)%d(ncol+i)))
					!	j_fig_iba=j_fig_iba+2
					!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+i*dd)
					!	call j_putor(j_fig_ivout,j_fig_iba+2,scale*real(j_o(iarg)%d(ncol+i)))
					!	j_fig_iba=j_fig_iba+2
 
				enddo !i=1,ncol-1  16196
				call j_gpputfigxy(ivfig,xx,0.)
				call j_gpputfigxy(ivfig,xx+0.5*dd,0.)
				!call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+(ncol-1)*dd)
				!call j_putor(j_fig_ivout,j_fig_iba+2,0.)
				!j_fig_iba=j_fig_iba+2
				!j_fig_np=2*ncol
				!call endfig(2)
				!	return
			endif !if(iscumu)  16166
		else !if(histo)then
			! if(getyrange)then
			! j_fig_ymin0=1.7e37;j_fig_ymax0=-1.7e37
			! end if !if(getyrange)then
			if(j_err)return
 
 
			! j_o(ivfig)%txt(6)(i2+1:)="'-' w l"//j_gplw
			! j_o(ivfig)%i2(6)=j_lentrim(j_o(ivfig)%txt(6))
			! ili=6
			!row 1 classmeans of x, total mean of x
			!row 2 frequencies
			!row 3 means of y
			!row 4 sd:s
			!		write(6,*)'isse,ismean,issd ',isse,ismean,issd
			do ip=1,ncol-1
				!	write(6,*)'377373ip',ip
				fre=j_getmatel(iarg,2,ip)
				if(fre.le.0)cycle
				xx=    j_getmatel(iarg,1,ip)
				!		call j_putor(j_fig_ivout,j_fig_iba+1,xx)
				if(issd.and..not.ismean)then
					yy=j_getmatel(iarg,4,ip)
				elseif(isvar)then !if(issd)then
					yy=j_getmatel(iarg,4,ip)**2
				elseif(ismean.and.issd)then
					yy=j_getmatel(iarg,3,ip)
				else !if(issd)then
					yy=j_getmatel(iarg,3,ip)
				endif !if(issd.and..not.ismean)  16241
				!		call j_putor(j_fig_ivout,j_fig_iba+2,yy)
				!		write(6,*)'ismean,issd ',ismean,issd
 
 
				if(ismean.and.issd)then !after first loop issd=.false.
					call j_gpputfigxy(ivfig,xx,yy,se=real(j_getmatel(iarg,4,ip)))
				elseif(isse)then
					call j_gpputfigxy(ivfig,xx,yy,se=real(j_getmatel(iarg,4,ip)/sqrt(j_getmatel(iarg,2,ip))))
 
				else !if(isse)then
					call j_gpputfigxy(ivfig,xx,yy)
				endif !if(ismean.and.issd)  16254
			end do !ip=1,ncol-1  16235
		end if !if(histo)  16138
		!write(6,*)'377373'
		call j_gpputfige(ivfig)   !'e')  !drawclass
		!	call j_figdump(ivfig)
		issd=.false.
	end do !iloop=1,nloop  16095
	!	call j_figdump(ivfig)
 
	if(j_gpshow)call j_showfig(ivfig)
 
 
 
	!	if(j_gpshow)call j_showfig(ivfig)
 
	!io=io+j_fig_narg+3
	return
end subroutine drawclass !subroutine drawclass(iob,io)
 
 
 
subroutine plotyx(iob,io)   !plotyx()
	! Section plotyx plotyx() Scatterplot
	! plotyx() makes scatterplot.
	! endheader
	!Option
	! Output & 1& FIGURE & The FIGURE object created or updated.
	! Args & 1 | 2 & REAL & y and x-variable, if func-> is not present.
	! In case y-variable is given with func->only,  x-variable is given as argument.
	! data & N | 1 & DATA & Data object used, default the last data object created or the dta given
	! with data=list().
	!@@figure@
	! mark & N | 1 & REAL | CHAR & The mark used in the plot. Numeric values refer to
	! mark types of gnuplot. The mark can be given also as CHAR varible or constant.
	! marksize & N | 1 & REAL & The size of the mark, deafault 1
	! buffer &N |1 &REAL & If a coordinate minimum or maximum is integer, gnplot can make
	! axes so that the border points are not properly visible. If there is buffer->
	! option the border points are shifted slightly so that gnplot puts axes further apart.
	! func& N | 1 & CODE &  Code option telling how the y-variable is computed.
	! xfunc& N | 1 & CODE &  Code option telling how the x-variable is computed.
	! buffer & N|0|1& REAL& If the data contains integer values at the borders of the ranges which gnuplot
	!these may not be visible. Giving buffer->, Jlp22 adds or subtracts a small number to the border
	!points which force gnplot to move borders further apart.
	! endoption
	!Note By default plotyx() uses the names of the x-variable and y-variable
	! in the xlabel and ylabel. If the names contain e.g. underscore _, this would make
	!the next character as under script. To plot the underscore character,
	! use xlabel-> 	and ylabel->, and replace _ with \\\_.
	!endnote
	!Note There can be both func-> and xfunc->
	!endnote
	!Note If there is func-> and not xfunc, the x-variable must be before func->
	!as arguments are always before options.
	!endnote
	!Note The default axes of gnuplot may hide points. This must be solved somehow later,
	!but currently the user should define axes ranges explicitly in such cases, especially with plots with few
	!points.
	!endnote
	! Ex plotyxex plotyx()
	! ** plotyx() (line  16019 file c:/jlp22/j.f90)
	! xmat=matrix(do->(0,10,0.01))
	! transa=trans()
	! y=2+3*x+0.4*x*x+4*rann()
	! /
	! datyx=newdata(xmat,read->x,maketrans->transa,extra->(Regf,Resid))
	! fi=plotyx(y,x,continue->fcont)
	! ** It is not necessary to put the function into the data
	! fi=plotyx(x,func->transa(y),mark->3,color->Orange,continue->fcont)
	! **look other way, rotate axes
	! fi=plotyx(x,xfunc->transa(y),mark->3,color->Orange,continue->fcont)
	! reg=regr(y,x)
	! ** ranges are stored in order to use them in classify()
	! stat(min->,max->)
	! figyx=plotyx(y,x,show->0)
	! **It would be possible to draw the regression function using draw()
	! figyx=plotyx(Regf,x,append->,continue->fcont,color->Blue)
	! fir=plotyx(Resid,x,continue->fcont)
	! ** When there are many observations, it is useful to compute class means of residuals
	! cl=classify(Resid,x->x,xrange->,dx->1)
	! ** Red error bars for standard deviations of residuals in calsees
	! fi22=drawclass(cl,sd->,mean->,continue->fcont,color->Red,show->0)
	! ** Black error bars for standard errors for class means. These can be used to
	! ** study how significantly residuals are nonzero.
	! fi22=drawclass(cl,se->,mean->,color->Black,width->2,append->,continue->fcont,
	! xlabel->'Xvariable, m]Arial,11[',
	! ylabel->'Yvariable, %]Arial,11[')
	!;if(wait);pause
	! endex
	! Note With data with integer values, the default ranges of gnuplot may be hide point at
	! borderlines.
	! endnote
	!Note ]fi[=plotyx() produces or updates file ]fi[.jfig] which contains
	! gnuplot commands and file ]fi[.jfi0 containg data.
	!endnote
	! endsection
 
 
 
	logical::isfunc,isxfunc
	real::yvalue,xvalue
	character*3 ch
	character*2 ch0
	real:: yvalue0
	integer*8 ::i
	!	write(6,*)'<7467er',j_err
	j_gpwasshown=.false.   !plotyx
	call j_startfunction(iob,io,0,narg,j_arg,ivfig);if(j_err)return
	j_ivfig=ivfig
	j_gpappend=j_isoption(iob,io,j_mappend)  !plotyx
 
	call j_deffig(iob,io,ivfig)
	if(j_err)return
	!call j_startfig(iob,io)
	!	write(6,*)'<77er',j_err
	!	call j_checkoutput(iob,io)
 
	j_gpline="'-'"
	j_gplenline=3
 
	if(j_err)return
	!	write(6,*)'shhsh'
	linkfunc=j_codelink(iob,io,j_mfunc); if(j_err)return
	isfunc=linkfunc.gt.0
	ix=0
	iy=0
	linkxfunc=j_codelink(iob,io,j_mxfunc); if(j_err)return
	isxfunc=linkxfunc.gt.0
	!	write(6,*)'isfunc',isfunc,isxfunc
	!narg=j_o(iob)%i(io+1)
	if(isfunc.and..not.isxfunc)then
		if(narg.ne.1)then
			write(6,*)'with func-> and without xfunc-> there can be only x-variable'
			j_err=.true.;return
		endif !if(narg.ne.1)  16392
		!	j_fig_ixx=j_o(iob)%i(io+2)
		!ixy=j_o(iob)%i(io+2)
		ix=j_arg(1)
	elseif(isxfunc.and..not.isfunc)then
		if(narg.ne.1)then
			write(6,*)'with xfunc-> and without func-> there can be only y-variable'
			j_err=.true.;return
		endif !if(narg.ne.1)  16400
		!	j_fig_ixx=j_o(iob)%i(io+2)
		!ixy=j_o(iob)%i(io+2)
		iy=j_arg(1)
		!	write(6,*)'iy',iy
 
	elseif(.not.(isfunc.and.isxfunc))then !if(isfunc)then
		if(narg.ne.2)then
			write(6,*)'without func-> and xfunc-> there must be two arguments, y and x'
			j_err=.true.;return
		endif !if(narg.ne.2)  16410
		!		j_fig_ixx=j_o(iob)%i(io+3)
		!		ixy=j_o(iob)%i(io+2)
		ix=j_arg(2)
		iy=j_arg(1)
	endif !if(isfunc.and..not.isxfunc)  16391
	!	j_fig_istyle=0  !defaulst
	!	j_fig_tex='.';j_fig_lex=1   !default for
	!	call startfig(iob,io)
	if(j_err)return
	call j_getdataobject(iob,io)
	if(j_err)return
	!call j_getdatasets(iob)  !using opt
	!!call j_clearoption(iob,io)  ! subroutine
 
	! if(j_gplexlabel.eq.0)then
	! if(ix.gt.0)then
	! call j_getline(j_ivnames,ix,j_varname1,le)
	! call j_replacefig(2,'set xlabel "'//j_varname1(1:le)//'"')
	! else
	! call j_replacefig(2,'set xlabel "xfunc->"')
	! endif !if(ix.gt.0)  15880
	! !else
	! !	call j_replacefig(2,'set xlabel "'//j_gpxlabel(1:j_gplexlabel)//'"')
	! endif !if(j_gplexlabel.eq.0)  15879
 
	! if(j_gpleylabel.eq.0)then
	! if(iy.gt.0)then
	! call j_getline(j_ivnames,iy,j_varname1,le)
	! call j_replacefig(3,'set ylabel "'//j_varname1(1:le)//'"')
	! else !if(j_gpleylabel.eq.0)then
	! call j_replacefig(3,'set ylabel "func->"')
	! endif !if(iy.gt.0)  15891
	! !	write(6,*)'<888',j_o(ivfig)%i(1)
	! endif !if(j_gpleylabel.eq.0)  15890
 
	! j_o(ivfig)%i(1)=0  !number of lines
	! j_o(ivfig)%i(2)=j_gplines !number of allocated lines
	! j_o(ivfig)%i(3)=0  !number plot commands
	! j_o(ivfig)%i(4)=j_gpplots  !number of availablr plotcommands
	!! j_o(ivfig)%i(5)=number of sets !number of availablr plotcommands
	! if(j_o(ivfig)%i(5).eq.0)then
	! call j_getfile(nu,rw='w',ivout=nuf,ivfile=ivfig,ext='.jfi0',replace=.true.)
	! else !if(j_o(ivfig)%i(5).eq.0)then
	! call j_getfile(nu,rw='a',ivout=nuf,ivfile=ivfig,ext='.jfi0')
	! write(nu,*)' '
	! write(nu,*)' '
	! endif !if(j_o(ivfig)%i(5).eq.0)  15971
	! if(j_err)return
	! j_o(ivfig)%i(5)=j_o(ivfig)%i(5)+1
	call j_deffig(iob,io,ivfig)
	if(j_err)return
	ntot=0
	!	do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
	!	write(6,*)'drom,until',j_dfrom,j_duntil
 
	do i=j_dfrom,j_duntil
		call j_getobs(i); if(j_err)goto 900
		!	write(6,*)'i',i,j_v(j_arg(1:2))
		if(j_rejected)cycle
 
		ntot=ntot+1
		! j_fig_xmin=min(j_fig_xmin,j_v(j_fig_ixx))
		! j_fig_xmax=max(j_fig_xmax,j_v(j_fig_ixx))
		if(isfunc)then
			yvalue=j_codevalue(iob,linkfunc)
			if(j_err)return
		else !if(isfunc)then
			yvalue=j_v(iy)
		endif !if(isfunc)  16478
		if(isxfunc)then
			!		write(6,*)'linkx',linkxfunc
			xvalue=j_codevalue(iob,linkxfunc)
			if(j_err)return
		else !if(isfunc)then
			xvalue=j_v(ix)
		endif !if(isxfunc)  16484
 
		if(abs(xvalue).ge.1.7d18.or.abs(yvalue).ge.1.7d18)cycle
		!	yvalue0=yvalue
		!j_fig_ymin=min(j_fig_ymin,yvalue0)
		!j_fig_ymax=max(j_fig_ymax,yvalue0)
		call j_gpputfigxy(ivfig,xvalue,yvalue)
		! write(nu,*)real(j_v(j_arg(2))),yvalue0
		! j_o(ivfig)%d(7)=min(j_o(ivfig)%d(7),j_v(j_arg(2)))
		! j_o(ivfig)%d(8)=max(j_o(ivfig)%d(8),j_v(j_arg(2)))
		! j_o(ivfig)%d(9)=min(j_o(ivfig)%d(9),yvalue)
		! j_o(ivfig)%d(10)=max(j_o(ivfig)%d(10),yvalue)
 
	end do !i=j_dfrom,j_duntil  16470
 
	call j_gpputfige(ivfig)  !plotyx
 
	call j_gpfigoption(iob,io,ivfig)
 
	! !ch=char(47+j_o(ivfig)%i(5))
	! write(ch,'(i3)')j_o(ivfig)%i(5)-1
	! iaa=1
	! if(ch(1:1).eq.' ')iaa=2
	! if(ch(2:2).eq.' ')iaa=3
	! !	call j_getfile(nu,rw='w',ivout=nuf,ivfile=ivfig,ext='.jfi0',replace=.true.)
	! !	write(6,*)'<8484',nuf
	! call j_getline(j_ivnames,nuf,j_filename,le) !note this takes ' ' into filename
	! if(j_err)return
	!i1=1
	!	if(j_filename(1:1).eq.'$')i1=2
	j_gpline(j_gplenline+1:)=' w p'//j_gppt//j_gpps
	j_gplenline=len_trim(j_gpline)
	if(j_gpicolor.ge.1)then
		!	call j_gpputfig(ivfig,j_gpplot,j_gpcolors(j_gpicolor))  ! ,add=.true.)
		!		le=len_trim(j_gpline)
		j_gpline(j_gplenline+1:)=j_gpcolors(j_gpicolor)
		j_gplenline=len_trim(j_gpline)
	endif !if(j_gpicolor.ge.1)  16522
	!if(j_gpicolor.ge.1)call j_gpputfig(ivfig,j_gpplot,j_gpcolors(j_gpicolor))  ! ,add=.tru
	call j_gpputfig(ivfig,j_gpplot,word=j_gpline)
	!	call j_gpputfig(ivfig,j_gpplot,ch0//j_filename(i1:le)//' i '//ch(iaa:3)//' w p'//j_gppt//j_gpps)  !i=index
	!	write(6,*)'<##',j_gpcolors(j_gpicolor)
 
	!	call j_closeunit(nu)
	!	end do !do k=1,jndatasetss
	!	call j_figdump(ivfig)
	!	call j_figshowoption(iob,io,ivfig)
	!write(6,*)'aft figshow'
	!call j_figdump(ivfig)
	!write(6,*)'gpshow,gpcont',j_gpshow,j_gpcontinue
	if(j_gpshow)call j_showfig(ivfig)
 
 
 
	!	j_fig_np=ntot
	j_v(j_ivaccepted)=ntot
	!	if(j_gpshow)call j_showfig(ivfig)
	900 continue !  if(j_err)return
!	io=io+j_fig_narg+3
!	if(j_depilog.gt.0)call dotrans(j_depilog,1)
!	write(6,*)' '
	return
end subroutine plotyx !subroutine plotyx(iob,io)
 
subroutine nobs(iob,io) !Number of observations in DATA
	! Section nobs nobs() number of observations in DATA or REGR
	! nobs(DATA) returns the number of rows in the data matrix of DATA//
	! nobs(REGR) returns the number of observations used to compute
	! the regression with regr().
	! endsection
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	iv=j_o(iob)%i(io+2)
	if(j_otype(iv).eq.j_ipregr)then
		j_v(iout)=j_o(iv)%i(  j_o(iv)%i(0)+1)
	elseif(j_otype(iv).ne.j_ipdata)then !if(j_otype(iv).eq.j_ipregr)then
		call j_printname('**argument of nobs not a DATA or REGR:',iv,' ')
		j_err=.true.
		return
	else !if(j_otype(iv).eq.j_ipregr)then
 
		ivmat=j_getmatrix(iv);if(j_err)return  !j_o(iv)%i(1)
		j_v(iout)=j_o(ivmat)%i(1)
 
 
	end if !if(j_otype(iv).eq.j_ipregr)  16566
	return
end subroutine nobs !subroutine nobs(iob,io)
 
 
 
 
 
subroutine joindata(iob,io) !
	!! There can be two options:\\
	! !nrowtot-> gives the total number of observations when joindata is used several times to put several DATAs into
	! ! the same DATA.\\
	! !append-> current data are appended to a DATA for which space has been reserved with nrowtot->
 
	!Section joindata joindata() Joins hierarchical DATAs
	!There are two different cases of joindata() \\
	! The call has only two arguments and is like \\
	! xdata2=joindata(cdata,xdata)\\
	!where cdata and xdata are linked and the first variable in cdata is cdata%nobsw. Then xdata2 will
	! contain all variables in cdata, except variable cdata%nosw is replaced by variable
	! cdata%obs which tells into which cdata observation the xdata observation belongs, in practice,
	!into which stand the schedule belongs. The output data will not be linked to any upper level data.\\
	! In the second case the call is like \\
	!xdata2=joindata(cdata,xdata#1...xdata#p)\\
	!DATAs xdata#1...xdata#p contain different periods (levels) in hierarchical data and
	!the will contain whole schedules. This new data is automatically linked to
	! to the first argument DATA (cdata).
 
	!endheader
	!Note In the second case: As the output will be connected to the first argument, the link from the second argument
	! to the first argument will be lost.
	!endnote
	!Note In the second case: The variable cdata%nobsw in cdata is modified to correpond to the new number of children,
	!i.e. the number of schedules in the stands.
	!endnote
	!Note there are examples of the new data format after splitdata() function.
	!endnote
	!Ex joindataex splitting and joining period DATA
	! cdata=data(in->'cdat.txt',read->(cdata%nobsw,site))
	! stat()
	! xdata=data(in->'xdat.txt',read->(npv#0,npv#5,income1...income5),time->)
	! stat()
 
	! linkdata(cdata,xdata)
	! stat(up->)
 
 
	! ;do(i,1,4)
	! pv"i"=list(income"i")
	! ;enddo
	! pv5=list(income5,npv#0,npv#5)
 
	! xdatap=splitdata(xdata,periodvars->(pv1...pv5))
	! stat(data->cdata)
 
	! ;do(i,1,5)
	! stat(data->xdatap#"i")
	! write('xdatap.txt',$,xdatap#"i"%matrix)
	! ;enddo
 
 
	! close('xdatap.txt')
	! contd=0
	! keepo=1
	! stat(data->cdata,sum->)
 
	! nobscur=cdata%nobsw%sum;
 
	! ;do(i,1,5)
	! Pv"i"=list(xdataP#"i"%nobsw,@pv"i")
	! ;enddo
 
	! ;do(i,1,5)
	! ;if(i.eq.5)keepo=0
	! xdataP#"i"=data(in->'xdatap.txt',read->(@Pv"i"),keepopen->keepo,continue->contd,nobs->nobscur)
	! stat(data->xdataP#"i",sum->)
	! nobscur=xdataP#"i"%nobsw%sum;
	! contd=1
	! stat(data->xdataP#"i")
	! ;enddo
 
	! stat(data->cdata)
	! xdatanew=joindata(cdata,xdataP#1...xdataP#5)
	! stat(data->xdatanew)
 
	! ** Did the procedure loose any data
	! stat(data->xdata)
 
	!delete_f('xdatap.txt')
	!endex
	!Latex
	! ** To summarize
	! \begin{itemize}
	!\item First there was cdata and xdata linked to it.
	! \item Then splitdata() was used to pull out different levels of the
	! simulation tree into xdatap#1...xdatap#5 DATAs.
	! \item Eeach data has %nobsw variable, in addition to the original period variables,
	! telling how many children each observation has.
	! \item Also the variable cdata%nobsw is updated.
	! \item Then the %matrix of each DATA was written to 'xdatap.txt'
	!  from the first to last period.
	! \item In practice it is beneficial to use binary files.
	!  Recall that format 'b' reads and writes in single precision.
	!  The matrices are in Jlp22 in double precision but practically never
	!  	double precision is needed.
	! If insisting on double precision data storage, use format 'B'.
	! \item  Data is read in from the file period by period. Initially the sum of variable cdata%nobsw is
	!  computed using stat(data->cdata,sum->). This sum tells the number of period 1 observations.
	! \item After making a period DATA, the number of observations in next period is computed similarly.
	!  It simplifies coding when the data for the last period has also %nobsw variable which is zero.
	!\item keepopen-> keeps the file open making it possible to continue reading from the same file,
	!  which is indicated with the continue-> option.
	!  It would be possible to use keepopen also for the last period and close the file after
	! reading all periods.
	!\item The above procedure can be used to convert tabular form schedules data into this
	! disk saving and fast reading format. But in long run a simulator could generate directly
	! data in this format which is simple to implement: just write different periods to different files
	!  or collect data into different period matrices and write the directly all data into one file.
	!\end{itemize}
	!endlatex
	!endsection
 
	! !** use nrowtot-> and append-> to put more data into the same DATA
	! !** here the same DATA is put twice
	! ! ** the number of observations in the tree format data is the same as
	! !**the number of observations in the data for the last period.
	! ! nobstot=2*nobs(xdataP#5);
	! ! Xdatanew=joindata(cdata,xdataP#1...xdataP#5,nrowtot->nobstot)
	! ! Xdatanew=joindata(cdata,xdataP#1...xdataP#5,append->)
	! ! stat()
	! !** remove the created file
 
 
	integer,dimension(:),allocatable ::newkeep,nkeep,ivkeep,nobsw,obsw,imat
	integer,dimension(:),allocatable ::rowba,nkeep2,nobswup
	integer,dimension(:),pointer::datat
	integer*8,dimension(:),allocatable ::iba
	integer*8::nobstot,nobstop,nkeeptot,ibas2,ndim1,ndim2,nel
	integer*8::nrowtot
	logical istwo,isappend,isnrowtot
	logical p
	p=.false.
 
	!	write(6,*)'hep'
	!	call j_checkoutput(iob,io)
	call j_startfunction(iob,io,j_ipdata,ndata,datat,ivout,needsout=.true.)
	if(j_err)return
 
	!	write(6,*)'ndata',ndata,datat
	isappend=j_isoption(iob,io,j_mappend,.false.)
	if(isappend)then
		call j_getoption(iob,io,j_mnewup,1,1,j_ipreal,.true.,newup,j_optarg0)
		if(j_err)return
		ivnewup=j_optarg0(1)
 
	endif !if(isappend)  16728
	if(ndata.lt.2)then
		write(6,*)'joindata needs at least two DATA arguments'
		j_err=.true. ;return
	endif !if(ndata.lt.2)  16734
	call j_getoption(iob,io,j_mnrowtot,-1,1,j_ipreal,.true.,nn,j_optarg0)
	if(j_err)return
 
	if(nn.gt.0)then
		nrowtot=j_v(j_optarg0(1))
 
		isnrowtot=.true.
	else
		isnrowtot=.false.
	endif !if(nn.gt.0)  16741
 
	istwo=ndata.eq.2
	! call j_getoption(iob,io,j_mdata,2,999,j_ipdata,.true.,ndata,datat)
	! if(j_err)return
	allocate(nkeep(1:ndata),ivkeep(1:ndata),nobsw(1:ndata),obsw(1:ndata))
	allocate(imat(1:ndata),iba(1:ndata),rowba(1:ndata),nkeep2(1:ndata))
	iba=0
 
	rowba(1)=0
	ivmat=j_getmatrix(datat(ndata));if(j_err)return !j_o(datat(ndata))%i(1)
	nobstot=j_o(ivmat)%i(1)
	if(isnrowtot.and.nrowtot.le.nobstot)then
		write(6,*)'illegal nrowtot->',nrowtot,' as current data has ',nobstot,' observations'
		j_err=.true.;return
		j_err=.true.;return
 
	endif !if(isnrowtot.and.nrowtot.le.nobstot)  16759
	!	write(6,*)'ivout ',ivout
	!	call j_getobject(ivout,'%obsw',j_ipreal,ivobsw)
	!	write(6,*)'ivobsw ',ivobsw
	nkeeptot=0
 
	do i=1,ndata
		!		write(6,*)'i',i
		call j_getname(datat(i))
		ipe=j_object(j_oname(1:j_loname)//'%nobsw')
 
		if(ipe.gt.0)ipe=j_inlistobject(ipe,j_o(datat(i))%i(2))
		if(ipe.ne.1.and..not.istwo)then
			write(6,*)'variable ',j_oname(1:j_loname)//'%nobsw  is not first variable in ',j_oname(1:j_loname)
			j_err=.true. ;return
		endif !if(ipe.ne.1.and..not.istwo)  16776
		imat(i)=j_getmatrix(datat(i));if(j_err)return  !j_o(datat(i))%i(1)
		ivkeep(i)=j_getkeep(datat(i));if(j_err)return !;j_o(datat(i))%i(2)
		nkeep(i)=j_o(ivkeep(i))%i(1)
		if(i.eq.2)then
			rowba(i)=0
			!	nkeep2(2)=nkeep(i)
		elseif(i.eq.3)then
			rowba(i)=nkeep2(2)
		elseif(i.gt.1)then
 
			rowba(i)=rowba(i-1)+nkeep2(i-1)
		elseif(istwo.and.i.eq.1)then
			nkeeptot=nkeep(1)
 
		endif !if(i.eq.2)  16783
 
 
		nkeep2(i)=nkeep(i)-1
		if(i.gt.1)then
			if(istwo)then
				nkeeptot=nkeeptot+nkeep(i)
			else
				nkeeptot=nkeeptot+nkeep2(i)
			endif !if(istwo)  16799
		endif !if(i.gt.1)  16798
		!	write(6,*)'i,nkeep,nkeep2,nkeeptot ',i,nkeep(i),nkeep2(i),nkeeptot
	enddo !i=1,ndata  16770
	!	write(6,*)'nkeep',nkeep
	!	write(6,*)'nkeep2 ',nkeep2
	!write(6,*)'nkeeptot',nkeeptot
 
	!stop
	if(allocated(newkeep))deallocate(newkeep)
	if(allocated(j_tempvector))deallocate(j_tempvector)
	allocate(newkeep(1:nkeeptot),j_tempvector(1:nkeeptot))
	!	newkeep(1)=ivobsw
	!	write(6,*)'j_tempvector',j_tempvector
	!	newkeep(1:nkeep2(2))=j_o(ivkeep(2))%i2(2:nkeep(2))
	!	ibas=nkeep2(2)
	!j_o(iv)%i(6)=ivobs
 
	if(istwo)then
		newkeep(1)=j_o(datat(1))%i(6)
		newkeep(2:nkeep(1))=j_o(ivkeep(1))%i2(2:nkeep(1))
 
		newkeep(nkeep(1)+1:nkeep(1)+nkeep(2))=j_o(ivkeep(2))%i2(1:nkeep(2))
 
	else
		ibas=0
		do i=2,ndata
			!	write(6,*)ibas+1,ibas+nkeep2(i),2,nkeep(i),nkeep(i),nkeep2(i)
			newkeep(ibas+1:ibas+nkeep2(i))=j_o(ivkeep(i))%i2(2:nkeep(i))
			ibas=ibas+nkeep2(i)
			!j_tempvector(ibas+1:ibas+nkeep(i))=j_o(imat(i))%d(1:nkeep(i))
		enddo !i=2,ndata  16829
	endif !if(istwo)  16821
	!	write(6,*)'newkeep',newkeep
	!	write(6,*)'rowba ',rowba
	nkeepto=nkeeptot
	ivkeeptot=j_deflist(ivout,'%keep',list0=nkeepto,list=newkeep)
	!	write(6,*)'keep',nkeeptot,'*',j_o(ivkeeptot)%i(1),j_o(ivkeeptot)%i2
	if(isappend)then
		call j_getname(ivout)
		ivmatrix=j_object(j_oname(1:j_loname)//'%matrix')
		if(ivmatrix.le.0)then
			write(6,*)j_oname(1:j_loname)//'%matrix does not exist'
			j_err=.true.;return
		elseif(j_otype(ivmatrix).ne.j_ipmatrix)then
			write(6,*)j_oname(1:j_loname)//'%matrix is not MATRIX'
			j_err=.true.;return
		endif !if(ivmatrix.le.0)  16844
		ibas2=j_nelem(ivmatrix)
	else
		if(isnrowtot)then
			ivmatrix=j_defmatrix(ivout,'%matrix',nobstot,nkeeptot,j_matreg,rowtot=nrowtot)
		else
			ivmatrix=j_defmatrix(ivout,'%matrix',nobstot,nkeeptot,j_matreg)
		endif !if(isnrowtot)  16853
		ibas2=0
	endif !if(isappend)  16841
 
	!	write(6,*)' ivmatrix ',ivmatrix,nkeeptot,nobstot
	!write(6,*) ',nobstot,nkeeptot,j_matreg,ivmatrix',nobstot,nkeeptot,j_matreg,ivmatrix
	nobswtot=0
	obsw=0
	nobsw=0
	!write(6,*)'obsw',obsw
	!write(6,*)'nobsw',nobsw
	nobstop=j_o(imat(1))%i(1)
 
	allocate(nobswup(1:nobstop))
	nobswup=0
	!	write(6,*)'nobstot',nobstot
	if(.not.istwo)then
 
		do i=1,nobstot
			level0=ndata
			!	p=i.le.5
			!		if(p)write(6,*)'*******************************'
			!	if(p)write(6,*)'j_iobs,level0 init',i,level0,obsw(level0),nobsw(level0)
 
700		if(obsw(level0).eq.nobsw(level0))then
		!		if(p)write(6,*)'level0',level0,obsw(level0),nobsw(level0)
				obsw(level0)=0
 
 
				nobsw(level0)=j_o(imat(level0-1))%d(iba(level0-1)+ 1)
				!	call j_getname(j_divmat2(level0-1))
				!	if(p)write(6,*)'level0',level0,nobsw(level0),imat(level0-1),j_oname(1:j_loname)
				!		write(6,*)'level0here dnobs',level0, j_dnobsw2(level0),' from ',j_diba2(level0-1)+ j_dnobswindex(level0-1)
				level0=level0-1
				!	if(p)write(6,*)'uus level0 ,nobsw(level0)',level0,nobsw(level0)
				!	if(level0.gt.0)goto 800
 
				!		write(6,*)'here ',level0,j_dnobsw2(level0)
				if(level0.gt.1)goto 700
			endif !700		if(obsw(level0).eq.nobsw(level0))  16881
			!	if(p)write(6,*)'level0here ',level0,ndata
			obsw(level0:ndata)=obsw(level0:ndata)+1
			nobswtot=nobswtot+1
			!		if(p)write(6,*)'level 0',level0,'nkeeptot ', nkeeptot
			! if(istwo)then
			! j_tempvector(1)=i
			! !		write(6,*)'datat(1),iba(1),nkeep(1)',datat(1),iba(1),nkeep(1)
 
			! j_tempvector(2:nkeep(1))=j_o(imat(1))%d(iba(1)+2:iba(1)+nkeep(1))
			! j_tempvector(nkeep(1)+1:nkeeptot)=j_o(imat(2))%d(iba(2)+1:iba(2)+nkeep(2))
 
			! else
			do ile=level0,ndata
				if(ile.eq.2)then
					!		if(p)write(6,*)'ileowba ',ile,2,1+nkeep2(ile),iba(ile)+2,iba(ile)+nkeep2(ile)
					j_tempvector(1:nkeep2(ile))=j_o(imat(ile))%d(iba(ile)+2:iba(ile)+nkeep(ile))
					!		write(6,*)'ile ',ile,j_tempvector(2:nkeep(ile))
				elseif(ile.gt.1)then
					!	if(p)write(6,*)'ileowba ',ile,rowba(ile)+1,rowba(ile)+nkeep2(ile),iba(ile)+2,iba(ile)+nkeep(ile)
					j_tempvector(rowba(ile)+1:rowba(ile)+nkeep2(ile))=j_o(imat(ile))%d(iba(ile)+2:iba(ile)+nkeep(ile))
				endif !if(ile.eq.2)  16910
				iba(ile)=iba(ile)+nkeep(ile)
			enddo !ile=level0,ndata  16909
			!	endif !if(istwo)  15324
 
			!		write(17,*)i,obsw(1)
			!	if(.not.istwo)
			nobswup(obsw(1))=nobswup(obsw(1))+1
 
			!	write(6,*)'obsw',obsw
			!	j_tempvector(1)=obsw(2)
			!	if(p)write(6,*)'i,',i,ibas2,nkeeptot,j_tempvector
			nobswtot=0
			j_o(ivmatrix)%d(ibas2+1:ibas2+nkeeptot)=j_tempvector
			ibas2=ibas2+nkeeptot
		enddo !i=1,nobstot  16875
 
	else
		nobsup=j_o(imat(1))%i(1)
		ibas=0  ! updata
		ibas2=0  ! subdata
		ibasout=0
		do i=1,nobsup
			nobo=j_o(imat(1))%d(ibas+1)
			!		write(6,*)'i,nobo',i,nobo
			do j=1,nobo
				!write(6,*)'nkeep,nsubkeep,i,j,ibas,ibassub,ibasup,nvar ',&
				!	nkeep,nsubkeep,i,j,ibas,ibassub,ibasup,nvar
				j_o(ivmatrix)%d(ibasout+1)=i
				j_o(ivmatrix)%d(ibasout+2:ibasout+nkeep(1))=j_o(imat(1))%d(ibas+2:ibas+nkeep(1))
				ibasout=ibasout+nkeep(1)
 
				!			write(6,*)ibasout+1,ibasout+nkeep(2),imat(2),ibas2+1,nkeep(2),ibas2+nkeep(2)
				j_o(ivmatrix)%d(ibasout+1:ibasout+nkeep(2))=j_o(imat(2))%d(ibas2+1:ibas2+nkeep(2))
				!write(6,*)newdata(ibas+nsubkeep+nvar+1),newdata(ibas+nsubkeep+nvar+2)
				!write(6,*)'i ',ibas+nsubkeep+nvar+1,i,ibas+nsubkeep+nvar+2,j
				ibasout=ibasout+nkeep(2)
				ibas2=ibas2+nkeep(2)
			enddo !j=1,nobo  16942
			ibas=ibas+nkeep(1)
		enddo !i=1,nobsup  16939
 
 
 
 
	endif !if(.not.istwo)  16873
 
	if(isappend)then
		! call j_i8i4(ndim1,j_o(ivout)%i(14:15))
		! call j_i8i4(ndim2,j_o(ivout)%i(16:17))
		! call j_i8i4(nel,j_o(ivout)%i(18:19))
 
 
		ndim1=j_nrows(ivmatrix)+nobstot
		write(6,*)'nobstot ',nobstot,'ndim1',ndim1,j_nrows(ivmatrix)
		j_o(ivmatrix)%i(1)=ndim1
		call j_i8i4(ndim1,j_o(ivmatrix)%i(14:15))
		write(6,*)'nrowsnew',j_nrows(ivmatrix)
		nel=ndim1*nkeeptot
		!		call j_i8i4(ndim2,j_o(ivout)%i(16:17))
		write(6,*)'nel ',nel,j_nelem(ivmatrix)
		call j_i8i4(nel,j_o(ivmatrix)%i(18:19))
		j_o(ivmatrix)%i(3)=nel
		write(6,*)'nelnew ',j_nelem(ivmatrix)
 
		!	ivupmat=j_defmatrix(ivnewup,' ',nobstop,nrow,j_matreg)
 
 
	else
		call j_defdata(ivout,ivmatrix,ivkeeptot)
 
	endif !if(isappend)  16964
 
 
 
	!	call j_getname(imat(1))
	!	write(6,*)j_oname(1:j_loname),nkeep(1),j_o(imat(1))%i(1:3),nkeep(1)*j_o(imat(1))%i(1),&
	!		nobstot
	!write(6,*)'nobswup ',nobswup(1:10)
	if(.not.istwo)then
		do i=1,nobstop
			j_o(imat(1))%d((i-1)*nkeep(1)+1)=nobswup(i)
		enddo !i=1,nobstop  16997
 
		call j_linkdata(datat(1),ivout)
	endif !if(.not.istwo)  16996
 
	deallocate(newkeep,nkeep,ivkeep,nobsw,obsw,imat)
	deallocate(iba,rowba,nkeep2,nobswup,j_tempvector)
	! if(level0.lt.0)level0=0
	! !going down
	j_dlastdata=ivout
	!!call j_clearoption(iob,io)  ! subroutine
	return
end subroutine !subroutine joindata(iob,io)
 
subroutine splitdata(iob,io)
	!Section splitdata splitdata() splits a schedules DATA into components
 
	! Each component contains variables simulated for a given period. After the function the stand data
	! is linked to DATA of schedules variables during the first period.
	!endheader
	!Option
	!Output&1&?& The variable name determining the names the generated DATAs.
	! If the output is xdatab
	! then the splitdata() generates DATAs xdatab#1...,xdatab#p for the number of periods p.
	!Args&1&DATA& Schedules data linked to an upper level stand data with linkdata().
	!periodvars&2-&LIST& LIST of LISTs, each list telling variables which
	! are simulated for the corresponding period.
	!endoption
	!Note When deciding to which period a variable belongs, the only thing which matters is at
	!what level the variable is put into the tree. Thus variable
	!  NPV0 which is the variable telling the NPV at the beginning
	! of the plannin horizon must put to the last list in periodvars, because it can be computed only
	! after the whole planning horizon is simulated.
	!endnote
	!Note If the stand data has variable nobswold then the initial value of the nobsw variable
	! is put into this variable. The easiest way to add this variable is to use
	! 	extra->nobswold when making the stand data
	!endnote
	!Note All generated matrices files can be written into one (e.g. binary) file starting
	! from the first to the last period. These DATAS can be the read in using
	! keepopen-> and continue-> options of data(). See below in the joindata() section
	!endnote
	!Note The first variable in DATA dataa is dataa%nobsw
	!endnote
 
 
	!endsection
	integer,intent(in)::iob
	integer,intent(in)::io
	integer,dimension(:), allocatable::iperiod,ivmat,link,ivdata,nobswpos
	integer*8,dimension(:), allocatable::nobsp,nobsp2,ibasper,nkeepper
	integer,dimension(:,:), allocatable::ikeep
	logical::isnobswold
	logical p
	call j_startfunction(iob,io,j_ipdata,narg,j_arg,ivout,needsout=.true.)
	if(j_err)return
	ivkeep=j_getkeep(j_arg(1));if(j_err)return  !j_o(j_arg(1))%i(2)
	ivmatrix=j_getmatrix(j_arg(1));if(j_err)return  !j_o(j_arg(1))%i(1)
	nobs=j_o(ivmatrix)%i(1)
	call j_getname(j_arg(1))
	ivup=j_o(j_arg(1))%i(5)
	call j_getname(j_arg(1))
	if(ivup.le.0)then
 
		write(6,*)j_oname(1:j_loname),' does not have upper level data'
		j_err=.true.;return
	endif !if(ivup.le.0)  17061
	!	j_o(iv)%i(14)=j_o(ivmat)%i(1)  !number of observa
	!j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	!	j_o(iv)%i(5)=ivup
 
	call j_getname(-1,ivup)
	write(6,*)'DATA ',j_oname(1:j_loname),' is no more linked to ',j_oname2(1:j_loname2)
	j_o(j_arg(1))%i(5)=0
	! write(6,*)'nobs,ivup,ivmatrix,',nobs,ivup,ivmatrix
	! call j_getname(ivup,ivmatrix)
	! write(6,*)j_oname(1:j_loname),' * ',j_oname2(1:j_loname2)
 
	ivkeepup=j_o(ivup)%i(2)
 
	nobswoldlink=j_object('nobswold')
	!write(6,*)'nobswoldlink ',nobswoldlink
	if(nobswoldlink.gt.0)nobswoldlink=j_inlistobject(nobswoldlink,ivkeepup)
	isnobswold=nobswoldlink.gt.0
 
	ivnobsw=j_o(ivup)%i(4)
 
	nobswlink=j_inlistobject(ivnobsw,ivkeepup)
	if(nobswlink.le.0)then
 
	elseif(nobswlink.ne.1)then
		call j_getname(ivnobsw,ivkeepup)
		write(6,*)'variable ',j_oname(1:j_loname),' must be first variable in ',j_oname2(1:j_loname2)
 
	endif !if(nobswlink.le.0)  17087
	ivmatrixup=j_getmatrix(ivup);if(j_err)return !j_o(ivup)%i(1)
	nobsup=j_o(ivmatrixup)%i(1)
	!if(allocated(j_o(ivmatrixup)%i2))deallocate(j_o(ivmatrixup)
	ndupl=0
 
	!		call j_getname(ivmatrixup, ivobsw,ivkeepup)
	!		write(6,*)'imatrixup ,ivobsw, ivkeepup ',j_oname(1:j_loname),' *',j_oname2(1:j_loname2),' + ',j_oname3(1:j_loname3)
	nkeepup=j_o(ivkeepup)%i(1)
 
	!write(6,*)'ivnobsw,ivkeepup,nobswlink,ivmatrixup,nobsup ',ivnobsw,ivkeepup,nobswlink,ivmatrixup,nobsup
	call j_getoption(iob,io,j_mperiodvars,2,9999,j_iplist,.true.,nperiod,j_optarg)
	if(j_err)return
	nvar=0
	nkeep=j_o(ivkeep)%i(1)
	allocate(nkeepper(1:nperiod),link(1:nperiod+1))
	allocate(ikeep(1:nperiod,1:nkeep))
	link(1)=0
	do i=1,nperiod
		nkeepper(i)=j_o(j_optarg(i))%i(1)
		link(i+1)=link(i)+nkeepper(i)
		do j=1,nkeepper(i)
			ivlisti=j_o(j_optarg(i))%i2(j)
			ikeep(i,j)=j_inlistobject(ivlisti,ivkeep)
			if(ikeep(i,j).le.0)then
				call j_getname(-1,ivlisti)
				write(6,*)j_oname2(1:j_loname2),' is not in ',j_oname(1:j_loname)//'%keep'
				j_err=.true.;return
			endif !if(ikeep(i,j).le.0)  17117
		enddo !j=1,nkeepper(i)  17114
		!write(6,*)'period,keep',i,ikeep(i,1:nkeepper(i))
		nvar=nvar+nkeepper(i)
	enddo !i=1,nperiod  17111
 
 
	!write(6,*)'nkeepper ',nkeepper,' link ',link,' nkeep ',nkeep
 
	if(nvar.ne.nkeep)then
		write(6,*)'there were ',nvar,' periodvars and ',nkeep,' keep-vars'
		do k=1,nkeep
			iv=j_o(ivkeep)%i2(k)
			lkm=0
			do i=1,nperiod
				do j=1,nkeepper(i)
					ivlisti=j_o(j_optarg(i))%i2(j)
					if(ivlisti.eq.iv)lkm=lkm+1
				enddo !j=1,nkeepper(i)  17136
 
			enddo !i=1,nperiod  17135
			if(lkm.ne.1)then
				call j_getname(iv)
				write(6,*)j_oname(1:j_loname),' appears ',lkm, ' times in periodvars'
 
			endif !if(lkm.ne.1)  17142
		enddo !k=1,nkeep  17132
		j_err=.true.
	endif !if(nvar.ne.nkeep)  17130
 
 
	if(j_err)then
		deallocate(ikeep)
		return
	endif !if(j_err)  17152
	allocate(iperiod(1:nobs),nobsp(1:nperiod),nobsp2(1:nperiod),ivmat(1:nperiod),ivdata(1:nperiod))
	!allocate(nobswp(1:nobsup,1:nperiod))
 
	nobsp=0
	ibas=0
	ibas0=0
	nobswcur=0
	ibasup=0
	iobs1=1
	j_iobs=0
	ndupl=0
 
	ntot=0
	!	call j_getname(ivup)
 
	!	write(6,*)'<proess',j_oname(1:j_loname)
	!	nobswp=0
	!write(6,*)'nobsup,ivmatrixup,nobswlink ',nobsup,ivmatrixup,nobswlink
	!write(6,*)'vars ',(j_o(j_optarg(i))%i(1),i=1,nperiod)
	do iunit=1,nobsup
 
		nobsw=j_o(ivmatrixup)%d(ibasup+nobswlink)
		!	write(6,*)' iunit,nobsw',iunit,nobsw
		if(isnobswold)j_o(ivmatrixup)%d(ibasup+nobswoldlink)=nobsw
		ibasup=ibasup+nkeepup
		j_iobs=j_iobs+1
		!	write(6,*)'iunit,nobsw,j_iobs ',iunit,nobsw,j_iobs
		!	if(nobsw.eq.1)then
		nobsp=nobsp+1
		iperiod(j_iobs)=1
		!	nobsp(1:nperiod)=nobsp(1:nperiod)+1
		!	nobswp(iunit,1:nperiod)=nobswp(iunit,1:nperiod)+1
		ibas0=ibas
		ibas=ibas+nkeep
		ntot=ntot+1
		!	else
		!	write(6,*)'hep'
iobloop:		do iobsw=2,nobsw
			j_iobs=j_iobs+1
			nvar=0
			!	write(6,*)'iob ',iob
			!	iperiod(iobs)=1
			!	nobsp(1:nperiod)=nobsp(1:nperiod)+1
			!		nobswp(iunit,nperiod)=nobswp(iunit,nperiod)+1
 
			ntot=ntot+1
ploop:	do i=1,nperiod
					!		write(6,*)'i ',i

				do j=1,nkeepper(i)
 
					!		write(6,*)'i,j, ',i,j,nvar,ibas,ibas0,nperiod
					!	if(j_iobs.le.200)
					!			if(j_iobs.le.200)write(6,*)iunit,iobsw,j,j_o(ivmatrix)%d(ibas+ikeep(i,j)),j_o(ivmatrix)%d(ibas0+ikeep(i,j))
					if(j_o(ivmatrix)%d(ibas+ikeep(i,j)).ne.j_o(ivmatrix)%d(ibas0+ikeep(i,j))) then
						!	iperiod(j_iobs)=i
						nobsp(i:nperiod)=nobsp(i:nperiod)+1
						!		nobswp(iunit,i)=nobswp(iunit,i)+1
						iperiod(j_iobs)=i
 
						!		write(6,*)'eka ',j_o(ivmatrix)%d(ibas+ikeep(nvar))
						!	write(6,*)'toka',j_o(ivmatrix)%d(ibas0+ikeep(nvar))
						goto 567
					endif !if(j_o(ivmatrix)%d(ibas+ikeep(i,j)).ne.j_o(ivmatrix)%d(iba  17210
				enddo !j=1,nkeepper(i)  17205
 
			enddo ploop !op:	do i=1,nperiod  17202
 
			ndupl=ndupl+1
			iperiod(j_iobs)=nperiod
			nobsp(nperiod)=nobsp(nperiod)+1
			!if(j_iobs.le.10)write(6,*)iunit,j_iobsw,iperiod(j_iobs)
			!	nobswp(iunit,i:nperiod)=nobswp(iunit,i:nperiod)+1
567			ibas0=ibas
			ibas=ibas+nkeep
			!		write(6,*)'ibas,nkeep ',ibas,nkeep
		enddo iobloop !loop:		do iobsw=2,nobsw  17193
		!	endif !if(nobsw.eq.1)  15415
	enddo !iunit=1,nobsup  17175
 
	if(ndupl.gt.0)write(6,*)'there were ',ndupl, 'schedules identical with the previous shchedule (they are not dropped)'
	! ibasup=0
	! do iunit=1,nobsup
	! j_o(ivmatrixup)%d(ibasup+nobswlink)=nobswp(iunit,1)
	! ibasup=ibasup+nkeepup
	!enddo !iunit=1,nobsup  15465
	!write(6,*)'ntot',ntot
	! do i=1,ntot
	! write(17,*)i,iperiod(i)
	! enddo !i=1,ntot  15455
 
	!	write(6,*)'nobsp ',nobsp
	!write(6,*)'nkeepper ',nkeepper
	nobsp2=0
 
	!write(6,*)'nobsp ',nobsp
 
	call j_getname(ivout)
	write(6,*)'onnam ',j_oname(1:j_loname)
	ivdatav=ivup
	do i=1,nperiod
		ivdata(i)=j_getobject(0,j_oname(1:j_loname)//'#'//char(48+i),j_ipdata)
		call j_getname(-1,ivdata(i))
		write(6,*)'nam2 ',j_oname2(1:j_loname2),nobsp(i),nkeepper(i)+1
		ivmat(i)=j_defmatrix(0,j_oname2(1:j_loname2)//'%matrix',nobsp(i),nkeepper(i)+j_18,j_matreg)
		if(j_err)return
		!write(6,*)'ivmat(i)',ivmat(i)
		ivkee=j_deflist(0,j_oname2(1:j_loname2)//'%keep',ivin=j_optarg(i),nres=1)
 
		ivnobsw=j_getobject(0,j_oname2(1:j_loname2)//'%nobsw',j_ipreal)
		iper=j_putlistobject(ivkee,single=ivnobsw,append=.false.)
		!	call j_getobject(0,j_oname2(1:j_loname2)//'%obs',j_ipreal,ivnobs) defdata takes care of this
 
 
		ivvars=j_deflist(0,j_oname(1:j_loname)//'%vars',ivin=ivkee)
		ivvars2=j_deflist(0,j_oname(1:j_loname)//'%vars2',ivin=ivkee)
		write(6,*)'onnamenyt ',j_oname(1:j_loname)
		call j_defdata(ivdata(i),ivmat(i),ivkee)
 
 
		if(j_err)return
		!	j_o(ivup)%i(3)=ivdata
		!	ivdatav=ivdata
	enddo !i=1,nperiod  17256
 
	allocate(ibasper(1:nperiod),nobswpos(1:nperiod))
 
 
 
	ibas=0
	ibasup=0
	ibasper=0
	j_iobs=0
	p=.false.
	do iunit=1,nobsup
		nobsw=j_o(ivmatrixup)%d(ibasup+nobswlink)
		j_o(ivmatrixup)%d(ibasup+nobswlink)=0
		!	j_o(ivmatrixup)%d(ibasup+nobswlink)=nobswp(iunit,1)
 
		!write(6,*)'iunit ', iunit,nobsw
 
 
 
		do j=1,nobsw
			j_iobs=j_iobs+1
 
			!	write(17,*)'j_iobs,p',j_iobs,p
			!	if(p)write(6,*)'iunit,j,j_iobs,iperiod(j_iobs)',iunit,j,j_iobs,iperiod(j_iobs)
			!write(17,*)'iunit,j,iobs,iperiod(iobs)',iunit,j,iobs,iperiod(j_iobs)
			do ip=iperiod(j_iobs),nperiod
				!	if(j_iobs.le.100)	write(6,*)iunit,j,ip,iperiod(j_iobs)ibasper(ip),ibas
				j_o(ivmat(ip))%d(ibasper(ip)+2:ibasper(ip)+1+nkeepper(ip))=&
					j_o(ivmatrix)%d(ibas+ikeep(ip,1:nkeepper(ip)))
				nobswpos(ip)=ibasper(ip)+1
				nobsp2(ip)=nobsp2(ip)+1
				!	write(17,*)'iunit,j,j_iobs,iperiod(j_iobs)',iunit,j,j_iobs,iperiod(j_iobs)
				!if(p)write(6,*)j_o(ivmat(ip))%d(ibasper(ip)+2:ibasper(ip)+1+nkeepper(ip))
				!	write(17,*)j_o(ivmat(ip))%d(ibasper(ip)+2:ibasper(ip)+1+nkeepper(ip))
				if(ip.gt.1)then
					j_o(ivmat(ip-1))%d(nobswpos(ip-1))=j_o(ivmat(ip-1))%d(nobswpos(ip-1))+1
				else
					j_o(ivmatrixup)%d(ibasup+nobswlink)=j_o(ivmatrixup)%d(ibasup+nobswlink)+1
				endif !if(ip.gt.1)  17314
				if(p)write(6,*)'ip,nobswpos(ip) ',ip,nobswpos(ip)
				!		if(ip.lt.nperiod)j_o(ivmat(ip))%d(ibasper(ip)+1)=nobswp(iunit,ip+1)
 
				!		if(j_iobs.le.10)	write(6,'(5i5,8f8.2)')iunit,j,ip,ibasper(ip),ibas,&
				!	j_o(ivmat(ip))%d(ibasper(ip)+2:ibasper(ip)+1+nkeepper(ip))
 
				ibasper(ip)=ibasper(ip)+1+nkeepper(ip)
			end do !ip=iperiod(j_iobs),nperiod  17305
			ibas=ibas+nkeep
		enddo !j=1,nobsw  17299
		ibasup=ibasup+nkeepup
	enddo !iunit=1,nobsup  17290
	!	write(6,*)'nobsp2',nobsp2
	!ivdatav=ivup
	! do i=1,nperiod
	! !write(6,*)'linkperiod ',i
	! call j_linkdata(ivdatav,ivdata(i))  !j_o(ivdata)%i(5)=ivup
	! if(j_err)return
	! ivdatav=ivdata(i)
	! enddo !i=1,nperiod  15627
 
	!	j_o(iv)%i(1)=ivmat;j_o(iv)%i(2)=ivkeep !j_o(iv)%i(3)=ivsub;j_o(iv)%i(4)=ivnobsw
	!	j_o(iv)%i(5)=ivup
	!integer,dimension(:), allocatable::iperiod,nobsp,ibasper,ivmat,nkeepper,link,ivdata
	!integer,dimension(:,:), allocatable::nobswp,ikeep
 
	deallocate(iperiod,nobsp,nobsp2,ibasper,ivmat,nkeepper,link,ivdata,nobswpos)
	!deallocate(nobswp,ikeep)
 
 
end subroutine splitdata
 
subroutine partdata(iob,io)
	logical same
	!Section partdata partdata() makes a new DATA by dropping variables and/or
	! observations.
	!endheader
	!Option
	!Output&1&DATA& The new DATA generated.
	!data&0|1&Data&Data object used as the input. Only one data object used.
	!filter&1&Code&The condition used to accept observations into the new data.
	!reject&1&Code&The condition used to reject observations from the new data.
	! If also filter-> is present, only observations passed the filter will be considered.
	!keep&-1-&REAL&Variables kept in the data.
	!drop&-1-&REAL&Variables dropped from the data. It is not possible to have both keep-> and drop->.
	!from&-1|1&REAL& from which observation observations are accepted.
	!until&-1|1&REAL& untilwhich observation are accepted.
	!endoption
	!Note The output can be the same as the input, except when the input is DATA
	! with cases and observations are rejected. The same technique which is
	! used to implement this can used also in other functions, if there is need for
	!this.
	!endnote
	!Ex partdataex
	!da=data(in->,read->(x1,x2,x3))
	! 1,2,3
	! 4,5,6
	! 7,8,9
	! /
	! da1=partdata(data->da,filter->(x3.le.6))
	! da1%matrix;
	! da2=partdata(data->da,reject->(x3.gt.6))
	! da2%matrix;
	! da3=partdata(data->da,from->2)
	! da3%matrix;
	! da4=partdata(data->da,keep->(x1,x2))
	! da4%matrix;
	! da4%keep;
	! da=partdata(data->da,drop->x2,filter->x1.lt.7)
	! da%matrix;
	! da%keep;
	!endex
	!endsection
	integer*8 ::nkeep8,no8,ibas8  !,iobs8
 
	call j_startfunction(iob,io,0,narg,j_arg,ivout);if(j_err)return
	call j_getdataobject(iob,io,pointkeep=j_dkeep)
	if(j_err)return
	same=.false.
	if(ivout.eq.j_divdata)then
		ivout0=ivout
		ivout=j_ivtempout
		call j_del(ivout)
		same=.true.
	endif !if(ivout.eq.j_divdata)  17398
 
	call j_getoption(iob,io,j_mkeep,-1,9999,j_ipreal,.false.,nkeep,j_optarg0)
	if(j_err)return
	call j_getoption(iob,io,j_mdrop,-1,9999,j_ipreal,.false.,ndrop,j_optarg)
	if(j_err)return
	if(nkeep.gt.0.and.ndrop.gt.0)then
		write(6,*)'there cannot be both keep-> and drop->'
		j_err=.true.;return
	endif !if(nkeep.gt.0.and.ndrop.gt.0)  17409
 
	if(nkeep.gt.0)then
		ivkeep=j_deflist(ivout,'%keep',list0=nkeep)
 
		j_o(ivkeep)%i2(1:nkeep)=j_optarg0(1:nkeep)
 
	elseif(ndrop.gt.0)then
		nkeep=j_dnkeep-ndrop
		ivkeep=j_deflist(ivout,'%keep',list0=nkeep)
		nk=0
loop:		do i=1,j_dnkeep
			do j=1,ndrop
				if(j_dkeep(i).eq.j_optarg(j))cycle loop
 
			enddo !j=1,ndrop  17424
			nk=nk+1
			j_o(ivkeep)%i2(nk)=j_dkeep(i)
 
		enddo loop !p:		do i=1,j_dnkeep  17423
	else
		nkeep=j_dnkeep
		ivkeep=j_deflist(ivout,'%keep',ivin=j_divkeep)
 
 
	endif !if(nkeep.gt.0)  17414
 
	!!call j_clearoption(iob,io)  ! subroutine
	!	write(6,*)'af',j_dfrom,j_duntil
	no8=0
	do j_iobs=j_dfrom,j_duntil
		!	write(6,*)'iobs ',iobs
		call j_getobs(j_iobs)
		if(j_err)return
		if(j_rejected)cycle
		no8=no8+1_8
	end do !j_iobs=j_dfrom,j_duntil  17442
	nkeep8=nkeep
	ivmat=j_defmatrix(ivout,'%matrix',no8,nkeep8,j_matreg)
 
 
	ibas8=0
	do j_iobs=j_dfrom,j_duntil
		!	write(6,*)'iobs ',iobs
		call j_getobs(j_iobs)
		if(j_err)return
		if(j_rejected)cycle
 
		j_o(ivmat)%d(ibas8+1:ibas8+nkeep)=j_v(j_o(ivkeep)%i2(1:nkeep))
		ibas8=ibas8+nkeep8
	end do !j_iobs=j_dfrom,j_duntil  17454
 
	call j_defdata(ivout,ivmat,ivkeep)
	write(6,*)'accepted ',j_nobsdata(ivout),' observations from ',j_nobsdata(j_divdata)
	if(j_o(j_divdata)%i(10).ne.0.and.j_nobsdata(ivout).ne.j_nobsdata(j_divdata))&
		write(6,*)'ask J. Lappi to update partdata when data has cases and obsevations are rejected'
	j_dlastdata=ivout
	if(same)then
		call j_move(ivmat,j_o(j_divdata)%i(1))
		ivvars=j_o(j_divdata)%i(11)
		deallocate(j_o(ivvars)%i2);allocate(j_o(ivvars)%i2(1:nkeep))
		j_o(ivvars)%i2(1:nkeep)=j_o(ivkeep)%i2(1:nkeep)
		ivvars=j_o(j_divdata)%i(12)
		if(ivvars.ne.0)then
			deallocate(j_o(ivvars)%i2);allocate(j_o(ivvars)%i2(1:nkeep))
			j_o(ivvars)%i2(1:nkeep)=j_o(ivkeep)%i2(1:nkeep)
		endif !if(ivvars.ne.0)  17475
		call j_move(ivkeep,j_o(j_divdata)%i(2))
		j_dlastdata=ivout0
		j_otype(j_ivtempout)=j_ipreal
 
 
	endif !if(same)  17469
 
 
 
	return
 
 
end subroutine
 
subroutine linkdata(iob,io) !linkdata()
 
	!Section linkdata linkdata() Links or combines hierarchical DATAs
	! linkdata(updata,subdata,...)
	! links hierarchical data sets. Currently linkdata can create also one flat file which can be used in jlp()
	!endheader
	!Option
	!Args&2-999&DATA& DATA objects in up to down order.
	!endoption
	!Note In versions before 20.12.2022 nobsw-> obtion was needed to tell what is the number
	! of subdata -observations under the current upper level observation. When I made the data format for storing
	! schedules having the tree structure efficiently, everything became too complicated when allowing any
	! freedom with respect to the nobsw-variable. Thus the nobsw variable in DATA subdata has
	!name updat%nosw and it must be the first variable among the keep-variables of the DATA.
	!endnote
 
	! Note  In most cases links between data sets can be made be making a DATA with up-> option.
	! If there is need to duplicate lower level observations, then
	! this can be currently made only in data() function.
	! endnote
	! Note  When using linked data in  functions, the values of the upper level variables are
	! automatically obtained when accessing lower level observations, if option up-> is present.
	! endnote
 
	! Note In the current version of Jlp22 it is no more necassary to use linked data sets in
	! jlp() function, as the treatment unit index in data containing both
	! stand and schedule data can be given in unit-> option
	! endnote
 
 
	!Ex linkdataex Example for linkdata.
	! ** make upper level DATA
	! dataa=data(read->(dataa%nobsw,site),in->)
	! 2,4
	! 3,5
	! /
	! dataa%matrix;
 
	! ** make subdata as an ordinary DATA
	! datab=data(read->(x1,y),in->)
	! 1,2
	! 3,4
	! 5,6
	! 7,8
	! 6,9
	! /
	! datab%matrix;
	! datab%keep;
	! **link now DATAs
	!** First varaible in the upper level dataa must be dataa%nobsw
	! linkdata(dataa,datab)
	! listb=;list(datab%?);
	! @listb;
	! ** when working with subdata the upper level data is feeded in for all observations
	! ** if up-> is present
	! **   even if they are not part of the data matrix as seen from datab%keep.
	! stat()
	!stat(up->)
 
	!**
	! ** Note stat() and all functions assume that the last DATA created is used
	! ** Thus when there are several DATAs around it is safer all use data-> option
	! ** i.e. the above could/should be stat(data->datab)
	! **
 
	! **
	! ** The flat file can be created also as follows:
	! ** when dealing with the subdata the upper level data is automatically used
	! transa=trans()
	! Unit=Unit !adds Unit and site variables from up-data to sub-data
	! site=site
	! Unit%obsw=Unit%obsw
	! /
	! ** In TRANS transa, the input variables come from the upper level data, and
	! ** outputvariables go to the new data based on DATA datab.
	! datac=newdata(datab,maketrans->transa)
	! stat()
	! datac%matrix;
	!endex
	!**linkdata() can be used to replace the old subdata DATA
	! datab=linkdata(dataa,datab)
	! stat()
	! datab%matrix;
	! endsection
 
	!	call j_checkoutput(iob,io)
	call j_startfunction(iob,io,j_ipdata,narg,j_arg,ivout)
	if(j_err)return
 
	! call j_getoption(iob,io,j_mdata,1,1,j_ipdata,.true.,noptarg,j_optarg0)
	! if(j_err)return
	! ivup=j_optarg0(1)
	! call j_getoption(iob,io,j_mupdata,1,1,j_ipdata,.true.,noptarg,j_optarg0)
	! ivsub=j_optarg0(1)
	! if(j_err)return
	do i=1,narg-1
		ivup=j_arg(i)
		ivsub=j_arg(i+1)
		call j_linkdata(ivup,ivsub)
		!	write(6,*)'up',j_o(ivup)%i
		!	write(6,*)'sub',j_o(ivsub)%i
		if(j_err)return
	enddo !i=1,narg-1  17588
	return
 
	!io=io+j_o(iob)%i(io+1)+3
	!!call j_clearoption(iob,io)  ! subroutine
	return
end subroutine
 
subroutine index(iob,io)  ! index(data_set,variable)
 
	! Section index index() Index in a LIST or MATRIX
	! index(obj,list) return the index of object obj in LIST list. Fuction returns zero
	! if list is not in LIST list.\\
	! index(valuea,matrixa) returns the location of valuea in MATRIX matrixa when going
	! the matrix through in row order. If valuea is not in matrixa index() returns zero.\\
	!index(valuea,matrixa,any->) returns the location of the first element which is greater or equal
	!to valuea.
	! endsection
 
 
	double precision::valu
	!	io=io_
	narg=j_o(iob)%i(io+1)
	iarg=j_o(iob)%i(io+2)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(narg.eq.1)then
		j_v(iout)=iarg
		return
	endif !if(narg.eq.1)  17622
	ivlist=j_o(iob)%i(io+3) !second arg
	select case(j_otype(ivlist))
	case(j_iplist) !select case(j_otype(ivlist))
	inde=j_inlistobject(j_o(iob)%i(io+2),ivlist)
 
	j_v(iout)=inde
	case (j_ipmatrix) !select case(j_otype(ivlist))
	valu=j_v(iarg)
	if(j_linkoption(iob,io,j_many).gt.0)then
		!!call j_clearoption(iob,io)  ! subroutine
		do j=1,j_o(ivlist)%i(3)
			if(j_o(ivlist)%d(j).ge.valu)then
				j_v(iout)=j
				return
			endif !if(j_o(ivlist)%d(j).ge.valu)  17637
		enddo !j=1,j_o(ivlist)%i(3)  17636
		j_v(iout)=j_0
	else !if(j_linkoption(iob,io,j_many).gt.0)then
		do j=1,j_o(ivlist)%i(3)
			if(valu.eq.j_o(ivlist)%d(j))then
				j_v(iout)=j
				return
			endif !if(valu.eq.j_o(ivlist)%d(j))  17645
		enddo !j=1,j_o(ivlist)%i(3)  17644
		j_v(iout)=j_0
	endif !if(j_linkoption(iob,io,j_many).gt.0)  17634
	case default !select case(j_otype(ivlist))
	call j_printname('**index: object ',ivlist, ' is not a list or matrix')
	j_err=.true.
	endselect !select case(j_otype(ivlist))
 
	return
end subroutine index !subroutine index(iob,io)
 
subroutine askc(iob,io) ! %%io
 
 
	! Section askc askc() Asks CHAR
	! Usage ://
	! askc(chvar1[,default->][,q->][,exit->])
	! Asks values for character variables when reading commands from an include file.
	!endheader
	! Option
 
	! Args&0-4&REAL | ILIST&row and column range as explained below.
 
	! Args & 0|1 &CHAR & character variable (need not exist before)
 
	! default&0|1&CHAR & default character stings
	! q & 0|1 &CHAR &text used in asking
	! exit& -1|0& & if the character constant or variable given in this option is read, then the control
	! return to command level similarly as if an error would occur.
	! endoption
	! Note
	! Response with carriage return indicates that the variable gets the default value. If there is no
	! default-> option, then the variable will be unchanged (i.e. it may remain also as another
	! object type than character variable).
	! endnote
	! Note: If there are no arguments, then the value is asked for the output variable, otherwise for
	! the arguments.
	! endnote
	! endsection
 
	character*40 line_
	character*40 def
	character*40 name
	character*10 exitc
	!	io=io_
	if(.not.j_remain)then
		write(6,*)'*ask does not work in batch()-mode'
		j_err=.true.
		return
	endif !if(.not.j_remain)  17694
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	lindef=j_linkoption(iob,io,j_mdefault)
	ivexit=j_igetopt(iob,io,j_mexit)
	if(ivexit.gt.0)then
		if(j_otype(ivexit).ne.j_ipchar)then
			write(6,*)'**askc, end-> does not refer to character'
			j_err=.true.
			return
		endif !if(j_otype(ivexit).ne.j_ipchar)  17704
		call j_getchar(ivexit,exitc,lex);if(j_err)return
	endif !if(ivexit.gt.0)  17703
 
	do i=1,max(narg,1)
		if(narg.le.0)then
			iv=j_o(iob)%i(io+2+narg)  !iout
		else !if(narg.le.0)then
			iv=j_o(iob)%i(io+1+i)
			if(j_otype(iv).eq.j_ipchar)then
				if(j_o(iv)%i(3).eq.0)then
					write(6,*)'*askc: argument cannot be character constant'
					j_err=.true.
					return
 
				endif !if(j_o(iv)%i(3).eq.0)  17718
 
			endif !if(j_otype(iv).eq.j_ipchar)  17717
		end if !if(narg.le.0)  17713
		if(j_linkoption(iob,io,j_mq).le.0.or.j_ipc(j_o(iob)%i(j_linkoption(iob,io,j_mq)+1)).eq.0)then
			call j_getline(j_ivnames,iv,name(21:),le)
			if(j_err)return
			name(1:20)='give text value for ';le=le+20
		else !if(j_linkoption(iob,io,j_mq).le.0.or.j_ipc(j_o(iob)%i(j_linkoption(iob,io,j_mq)+1)).eq.0)then
			call j_getchar(j_o(iob)%i(j_linkoption(iob,io,j_mq)+1),name,le);if(j_err)return
		end if !if(j_linkoption(iob,io,j_mq).le.0.or.j_ipc(j_o(iob)%i(j_li  17727
		lef=0
		if(lindef.gt.0)then
			ide=j_o(iob)%i(lindef+i)
			if(j_otype(ide).eq.j_ipchar)then
				call j_getchar(ide,def,lef)
			else !if(j_otype(ide).eq.j_ipchar)then
				write(6,*)'** illegal default in askc';j_err=.true.
			end if !if(j_otype(ide).eq.j_ipchar)  17737
		end if !if(lindef.gt.0)  17735
		if(lef.gt.0)then
			if(ivexit.gt.0)then
				write(6,'(1x,a,$)')name(1:le)//'  (default='//def(1:lef)//') >'
			else !if(ivexit.gt.0)then
				write(6,'(1x,a,$)')name(1:le)//'  (default='//def(1:lef)//',exit='//exitc(1:lex)//') >'
			endif !if(ivexit.gt.0)  17744
		else !if(lef.gt.0)then
			if(ivexit.le.0)then
				write(6,'(1x,a,$)')name(1:le)//' >'
			else !if(ivexit.le.0)then
				write(6,'(1x,a,$)')name(1:le)//'(exit='//exitc(1:lex)//') >'
			endif !if(ivexit.le.0)  17750
		end if !if(lef.gt.0)  17743
		read(5,'(a)')line_(2:) ; nc=j_lentrim(line_) !read(5,'(q,a)')nc,line_(2:)
		if(line_(2:).eq.' '.or.nc.le.0)then
			if(lef.gt.0)then
				ivchar=ide
			else !if(lef.gt.0)then
				call j_printname('*w* character variable',iv,' not created/modified');goto 900
			end if !if(lef.gt.0)  17758
		else !if(line_(2:).eq.' '.or.nc.le.0)then
			if(line_(2:2).eq."'")then
				nc1=2
			else !if(line_(2:2).eq."'")then
				line_(1:1)="'"
				nc1=1;nc=nc+1;line_(nc:nc)="'"
			end if !if(line_(2:2).eq."'")  17764
			if(ivexit.gt.0)then
				if(line_(nc1+1:nc-1).eq.exitc(1:lex))then
					write(6,*)'**exit from askc due to exit->'
					j_err=.true.
					return
				endif !if(line_(nc1+1:nc-1).eq.exitc(1:lex))  17771
			endif !if(ivexit.gt.0)  17770
			ivchar=j_defchar(0,line_(nc1:nc))
			if(j_err) return
		end if !if(line_(2:).eq.' '.or.nc.le.0)  17757
		call j_asschar2(ivchar,iv)
	end do !i=1,max(narg,1)  17712
	900 if(j_err)return
	!!call j_clearoption(iob,io)  ! subroutine
	return
end subroutine askc !subroutine askc(iob,io)
 
subroutine ask(iob,io)  !ask()
 
	! Section ask ask() Asks REAL
	! ask([var][,default->][,q->][,exit->])//
	! Ask values for a variable while reading commands from an include file.//
	! Argument://
	! var 0 or one real variable (need not exist before)
	! Options:
	! default default values for the asked variables
	! q text used in asking
	! exit if the value given in this option is read, then the control returns to command level
	! similarly as if an error would occur. If there is no value given in this option, then
	! the exit takes place if the text given as answer is not a number.
	!endheader
	! Note If there are no arguments, then the value is asked for the output variable, otherwise for
	! the argument. The value is interpreted, so it can be defined using transformations.
	! Response with carriage return indicates that the variables get the default values. If there is no
	! default-> option, then the previous value of the variable is maintained (which is also printed
	! as the default-> value in asking)
	! endnote
 
	! Ex askex Examples for ask()
	! a=ask(default->8)
	! ask(a,default->8)
	! print(ask()+ask()) ! ask without argument is a numeric function
	! ask(v,q->'Give v>')
	! endex
	! endsection
 
 
	integer,dimension(:),pointer::arg,argq
	logical::yes
	!	character*40 line_
	!	character*60 name
	double precision vav,exitv
	call j_startfunction(iob,io,0,narg0,arg,ivout);if(j_err)return
	if(narg0.eq.0)then
		narg=1
		arg=>j_o(iob)%i(io+2:io+2)
	else
		narg=narg0
	endif !if(narg0.eq.0)  17823
	call j_getoption(iob,io,j_mdefault,-1,narg,j_ipreal,.true.,noptarg,j_optarg0);if(j_err)return
	if(noptarg.gt.0.and.noptarg.ne.narg)then
		write(6,*)'default-> should have as many arguments as the function has'
		j_err=.true. ;return
 
	endif !if(noptarg.gt.0.and.noptarg.ne.narg)  17830
 
 
	!	io=io_
	!	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	!j_getoption(iob,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
 
	call j_getoption(iob,io,j_mq,-1,1,j_ipchar,.true.,nq,argq)
	if(j_err)return
	!	lindef=j_linkoption(iob,io,j_mdefault)
	ivexit=j_igetopt(iob,io,j_mexit)
	if(ivexit.gt.0)exitv=j_v(ivexit)
	write(6,100)'default values:'
	if(noptarg.gt.0)then
		yes=j_printvar(6,noptarg,j_optarg0)
	else !if(noptarg.gt.0)then
		yes=j_printvar(6,narg,arg)
	endif !if(noptarg.gt.0)  17848
	if(nq.gt.0)then
		call j_getchar(argq(1),j_asktext,leb);if(j_err)return
		write(6,100)j_asktext(1:leb)
	else !if(nq.gt.0)then
100 format(a,$)
		write(6,100)'give values for:'
		call j_printlist0(6,narg,arg)
	endif !if(nq.gt.0)  17853
 
 
	!!call j_clearoption(iob,io)  ! subroutine
	goto 179
178		write(6,100)'ask>'
179	read(5,'(a)')j_tempchar2
	le=j_lentrim(j_tempchar2)
	if(le.le.0)return
	if(j_tempchar2(1:le).eq.'e'.or.j_tempchar2(1:le).eq.'end')then
		write(6,*)'generating error return from ask'
		j_err=.true.;return
 
	endif !if(j_tempchar2(1:le).eq.'e'.or.j_tempchar2(1:le).eq.'end')  17869
	! do j=1,narg-1
	! ipi=le2b=j_nextlim(j_tempchar,1,le,',')
	! if(ipi.gt.le)then
	! write(6,*)'seperate values wit comma'
	! goto 178
 
	! endif
	! enddo
	goto 11
12	write(6,*)'reading error, try again, <return>: give up'
	goto 178
13	write(6,*)'not enough numbers, try again, <return>: give up'
	goto 178
 
11	read(j_tempchar2(1:le),*,err=12,end=13)j_tempv(1:narg) !to avoid warnings
	j_v(arg)=j_tempv(1:narg)
	j_v(ivout)=j_v(arg(1))
	return
 
	return
end subroutine ask !subroutine ask(iob,io)
 
subroutine pdf(iob,io)   !
	! Section pdf pdf() Normal density
	! endheader
	! Option
	! Output & 1 & REAL & the value of the density.
	! Args &0-2 & REAL & ]Arg1[ is the mean (default 0), ]Arg2[ is the standard deviation
	! (default 1). If sd is given, the mean must be given explicitly as teh first argument.
	! endoption
	! Note See example drawclassex for an utilization of pdf()
	! endnote
	! endsection
 
	parameter (coef_=0.39894227485064260)  !
	double precision::arg
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	arg=j_v(j_o(iob)%i(io+2))
	select case(narg)
	case(1) !select case(narg)
	j_v(iout)=coef_*exp(-0.5d0*arg*arg)
	case(2) !select case(narg)
	arg=arg-j_v(j_o(iob)%i(io+3))
	j_v(iout)=coef_*exp(-0.5d0*arg*arg)
	case(3) !select case(narg)
	arg=(arg-j_v(j_o(iob)%i(io+3)))/j_v(j_o(iob)%i(io+4))
	j_v(iout)=coef_*exp(-0.5d0*arg*arg)/j_v(j_o(iob)%i(io+4))
	end select !select case(narg)
	!	io=io+narg+3
	return
end subroutine pdf !subroutine pdf(iob,io)
 
subroutine cdf(iob,io)  !
 
	! Section cdf cdf() Cumulative distribution for normal and chi2
	! endheader
	! Option
	! Output& 1 & REAL & The value of the cdf.
	! Args & 1-3 & REAL& ]Arg1[ the upper limit of the integral. When chi2-> is not present, then
	! ]Arg2[, if present is the mean of the normal distribution (defaul 0), and ]Arg3[, if present,
	! is the sd of the ditribution. If chi2-> is present, then oblicatory ]Arg2[ is
	! ifs the number of degrees of freedom for chi2-distribution.
	! chi2 & N |0 & &
	! endoption
	!endsection
 
	double precision arg,sqr2,arg2
	parameter (sqr2= 0.707106769d0)  !1/sqrt(2.)
	double precision erf
	external erf  ! from netlib dcdflib -library in file matsub.f
 
	double precision df_,pp_,q_  !for chi square
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	arg=j_v(j_o(iob)%i(io+2))
	call j_getoption(iob,io,j_mchi2,-1,0,j_ipreal,.false.,nchi2,j_optarg0)
	if(j_err)return !goto 90
	if(nchi2.ge.0)then
		x_=arg
		if(narg.lt.2)then
			write(6,*)'*cdf(arg,df,chi2->) needs two arguments'
			j_err=.true.
			return
		else !if(narg.lt.2)then
			idf=j_v(j_o(iob)%i(io+3))
			if(idf.le.0)then
				write(6,*)'*cdf(arg,df,chi2->): df not positive integer ',idf
				j_err=.true.
				return
			endif !if(idf.le.0)  17963
			if(arg.lt.0)then
				write(6,*)'*cdf(arg,df,chi2->): arg is negative',arg
				j_err=.true.
				return
 
			endif !if(arg.lt.0)  17968
			arg2=idf
		endif !if(narg.lt.2)  17957
 
		CALL cumchi(arg,arg2,pp_,q_)  !from netlib dcdflib
		j_v(iout)=pp_
 
	else !if(nchi2.ge.0)then
		select case(narg)
		case(2) !select case(narg)
		arg=arg-j_v(j_o(iob)%i(io+3))
		case(3) !select case(narg)
		arg=(arg-j_v(j_o(iob)%i(io+3)))/j_v(j_o(iob)%i(io+4))
		end select !select case(narg)
 
		j_v(iout)=  0.5d0*(j_1+erf(sqr2*arg))
		!	write(6,*)'arg ',arg,sqr2*arg,j_v(iout)
 
 
	endif !if(nchi2.ge.0)  17955
	!	90	io=io+narg+3
	return
end subroutine cdf !subroutine cdf(iob,io)
 
subroutine close_(iob,io)  ! close(iob,io)
	! Section close close() Closes  files
	! close(file1,...filen) close open files where file is either a character constant
	! or character variable associated with a file or REAL.
	!A REAL argument is useful when debuggin information is written directly to a unit indicated
	!with a number. REAL argument is naturally made first integer.
	! endheader
	! Note No open()function is needed. An file is opened when it is first time in write() or
	!print(file->).
	! endnote
	! endsection
	logical ope
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	do i=1,narg
 
		iv=j_o(iob)%i(io+1+i)
		if(j_otype(iv).eq.j_ipreal)then
			nu=j_v(iv)
			close(nu)
 
		else
			nu=j_iounit(iv)
			!	write(6,*)'<554 iv',iv
			if(j_err)return
			if(nu.le.0)then
				call j_printname('close: argument ',iv,' not open file')
				! j_err=.true.
				return
			endif !if(nu.le.0)  18023
			!		write(6,*)'<9'
			call j_closeunit(nu)
			if(j_err)return
		endif !if(j_otype(iv).eq.j_ipreal)  18015
	end do !i=1,narg  18012
	return
end subroutine close_ !subroutine close(iob,io)
 
! subroutine recl_(iob,io)  ! used to study file structure, a trial version not documented
 
! logical exi
! character*10 unf,seq,acc,dir,for,rtype
! narg=j_o(iob)%i(io+1)
! iout=j_o(iob)%i(io+2+narg)
! iv2=j_o(iob)%i(io+2)
! nu=j_iounit(iv2)
! if(j_err)goto 100 !return
! if(nu.le.0)then
! call j_getchar(iv2,j_filename,le)
! inquire(file=j_filename(1:le),exist=exi,recl=nrecl)
! if(.not.exi)then
! write(6,*)'*recl, file does not exist: ',j_filename(1:le)
! j_err=.true.
! goto 100 !return
! endif !if(.not.exi)then
! inquire(unit=25,recl=nrecl,sequential=seq,unformatted=unf,access=acc,direct=dir,&
! form=for)
! write(6,*)'nrecl ',nrecl,'seq ',seq,'unf ',unf, 'acc ',acc, ' dir ',dir
! write(6,*)'form ',for
! open(file=j_filename(1:le),unit=25)
! inquire(unit=25,recl=nrecl,sequential=seq,unformatted=unf,access=acc,direct=dir,&
! form=for)
! write(6,*)'nrecl ',nrecl,'seq ',seq,'unf ',unf, 'acc ',acc, ' dir ',dir
! write(6,*)'form ',for
! close(25)
! open(file=j_filename(1:le),unit=25,form='unformatted')
! inquire(unit=25,recl=nrecl,sequential=seq,unformatted=unf,access=acc,direct=dir,&
! form=for)
! write(6,*)'unform', 'nrecl ',nrecl,'seq ',seq,'unf ',unf, 'acc ',acc, ' dir ',dir
! write(6,*)'form ',for
! close(25)
! open(file=j_filename(1:le),unit=25,access='direct',err=100)
! inquire(unit=25,recl=nrecl,sequential=seq,unformatted=unf,access=acc,direct=dir,&
! form=for) !   ,recordtype=rtype)
! write(6,*)'unform', 'nrecl ',nrecl,'seq ',seq,'unf ',unf, 'acc ',acc, ' dir ',dir
! write(6,*)'form ',for
! close(25)
! goto 100
! call j_getfile(nu,'r',ext=j_filename(1:le),forma='b')
! ! call j_openread(j_filename(1:le),'b',nu) !tee uuus
! inquire(unit=nu,recl=nrecl,unformatted=unf)
! !		write(6,*)'<10'
! call j_closeunit(nu)  !JL 29.3.2016
! else !if(nu.le.0)then
! inquire(unit=nu,recl=nrecl)
! end if !if(nu.le.0)then
! if(j_otype(iout).ne.j_ipreal)call j_del(iout)
! j_v(iout)=nrecl
! 100 continue !io=io+narg+3
! return
! end subroutine recl_ !subroutine recl_(iob,io)
 
subroutine read(iob,io)   !read()   %%io
 
	!Section read read() Reads from file
	! read(file,format[,obj1,…,objn][,eof->var] [,wait->])//
	! Reads real variables or matrices from a file. If there are no objects
	! to be read, then a record is
	! by-passed.//
	! Arguments:
	! file the file name as a character variable or a character constant//
	! format//
	! b' unformatted (binary) data //
	! 'bn' unformatted, but for each record there is integer for the size of the record. Does
	! not work when reading matrices.
	! 'bis' binary data consisting of bytes, each value is converted to real value (the only
	! numeric data type in J). This works only when reading matrices.//
	! '(….)' a Fortran format. Does not work when reading matrices.
	! $ the * format of Fortran//
	! obj1,…,objn
	! Jlp22 objects//
	! Options://
	! eof Defines the variable which indicates the end of file condition of the file. If the end
	! of the file is not reached the variable gets the value 0, and when the end of file is
	! reached then the variable gets value 1 and the file is closed without extra notice.
 
	! When eof-> option is not present and the file ends then an error
	! condition occurs and the file is closed.//
	! wait Jlp22 is waiting until the file can be opened. Useful in client-server applications. See
	! chapter Jlp22 as a server.
	!endheader
	!Note Use ask() or askc() to read values from the terminal when reading lines from an
	! include file.
	! endnote
	! Note When reading matrices, their shapes need to
	! be defined earlier with matrix()
	! hfunction.
	! endnote
	! endsection
 
 
	integer,dimension(:),pointer :: eof  !eofvariable
	integer :: neof
	logical nform
	logical wait,nudollar
	integer*1 ,dimension(:), allocatable::bytes
	save le,lef
	!	io=io_
	call j_startfunction(iob,io,0,narg,j_arg,ivout);if(j_err)return
	!	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iv2=j_arg(1)  !j_o(iob)%i(io+2)
	nudollar=iv2.eq.j_ivdollar
	ivform0=j_arg(2) !j_o(iob)%i(io+3
	call j_getname(ivform0)
	write(6,*)'ivform0',j_oname(1:j_loname),'*',ivform0.eq.j_ivdollar
 
	!ivarg3=j_arg(3) !j_o(iob)%i(io+4)
	if(nudollar)then
		if(ivform0.eq.j_ivb.or.iform0.eq.j_ivb2.or.iform0.eq.j_ivdi2.or.ivform0.eq.j_ivdg.or.&
				ivform0.eq.j_ivdg2)then !if(ivform_.eq.j_ivdi)then
			!if(j_form(2:2).eq.'1')then
			write(6,*)'illegal format for reading from console'
			j_err=.true.;return
		endif !if(ivform0.eq.j_ivb.or.iform0.eq.j_ivb2.or.iform0.eq.j_ivd  18147
		nu=5
	else
		nu=j_iounit(iv2)
	endif !if(nudollar)  18146
	write(6,*)'nu',nu
	if(j_err)return
	wait=j_isoption(iob,io,j_mwait) !j_linkoption(iob,io,j_mwait).ge.0
	call j_getoption(iob,io,j_meof,-1,1,j_ipreal,.true.,neof,eof);if(j_err)return
	!!call j_clearoption(iob,io)  ! subroutine
	if(j_err)return
	if(eof(1).gt.0)then
		j_v(eof(1))=0.
	endif !if(eof(1).gt.0)  18163
	if(nu.le.0.and.nu.ne.5)then
		!	call j_getchar2(ivform,j_form,lef)
		!if(j_err)return
		iv22=iv2
		if(wait)iv22=-iv2
		! pitäis katsoa
		! j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout,readit,deleted) !get file for writing, name is stored in character constant ivfile
		!222	call j_openreadiv(iv22,j_form(1:lef),nu) !openfile
		!	222	call j_getfile(nu,'r',ivfile=iv22,forma=j_form(1:lef)) !openfile
		!		222	call j_getfile(nu,'r',ivfile=iv22,forma=j_form(1:lef)) !openfile
222 call j_getfile(nu,'r',ivfile=iv22,ivform=ivform0)
	!	write(6,*)'iv22,',iv22,'form', j_form(1:lef),' nu',nu
		if(j_err)return
 
	endif !if(nu.le.0.and.nu.ne.5)  18166
	!	else !if(nu.le.0.and.nu.ne.5)then
	! if(j_unitchar(nu).gt.0)then
	! write(6,*)'*read: trying to read write file, file remains open'
	! j_err=.true.
	! return
	! endif !if(j_unitchar(nu).gt.0)  18104
	! if(ivform.ne.j_ivdollar)call j_getchar2(ivform,j_form,lef)
	! if(j_err)return
	! end if !if(nu.le.0.and.nu.ne.5)  18089
	if((ivform0.eq.j_ivb.or.iform0.eq.j_ivb2).and.ivform0.ne.j_unitform(nu))then
		call j_getname(iv2,ivform0,j_unitform(nu))
		write(6,*)j_oname(1:j_loname),' was opened with format ',j_oname3(1:j_loname3), &
			' and you try to read with ',j_oname2(1:j_loname2)
		j_err=.true.;return
	endif !if((ivform0.eq.j_ivb.or.iform0.eq.j_ivb2).and.ivform0.ne.j  18190
	if(narg.eq.2)then
		if(ivform0.eq.j_ivb.or.ivform0.eq.j_ivb2)then
			read(nu,err=990,end=880,iostat=ios)
		else
			read(nu,*,err=990,end=880,iostat=ios)
 
		endif !if(ivform0.eq.j_ivb.or.ivform0.eq.j_ivb2)  18197
		return
		return
 
	endif !if(narg.eq.2)  18196
 
	if(j_otype(j_arg(3)).eq.j_ipmatrix)then
		if(ivform0.eq.j_ivb)then
			write(6,*)'reading binary single precison matrices with sizes ',(j_o( j_arg(j))%i(3),j= 3,narg) !aftee form
 
			do ia=3,narg
				if(j_o(j_arg(ia))%i(1).eq.1.or.j_o(j_arg(ia))%i(1).eq.1)then
					nel=j_o(j_arg(ia))%i(3)
					call j_allocates(j_tempvectors,int8(nel))
					read(nu,err=990,end=880,iostat=ios)j_tempvectors(1:nel)
					j_o(j_arg(ia))%d(1:nel)=j_tempvectors(1:nel)
				else
					ibas=0
					ncol=j_o(j_arg(ia))%i(2)
					call j_allocates(j_tempvectors,int8(ncol))
					do i=1,j_o(j_arg(ia))%i(1)
						read(nu,err=990,end=880,iostat=ios)j_tempvectors(1:ncol)
						j_o(j_arg(ia))%d(ibas+1:ibas+ncol)=j_tempvectors(1:ncol)
						ibas=ibas+ncol
					enddo !i=1,j_o(j_arg(ia))%i(1)  18222
				endif !if(j_o(j_arg(ia))%i(1).eq.1.or.j_o(j_arg(ia))%i(1).eq.1)  18213
			end do !ia=3,narg  18212
 
 
 
			return
 
		elseif(ivform0.eq.j_ivb2)then
 
			write(6,*)'reading binary double precison matrices with sizes ',(j_o( j_arg(j))%i(3),j= 3,narg) !aftee form
 
			do ia=3,narg
				if(j_o(j_arg(ia))%i(1).eq.1.or.j_o(j_arg(ia))%i(1).eq.1)then
					nel=j_o(j_arg(ia))%i(3)
 
					read(nu,err=990,end=880,iostat=ios)j_o(j_arg(ia))%d(1:nel)
 
				else
					ibas=0
					ncol=j_o(j_arg(ia))%i(2)
 
					do i=1,j_o(j_arg(ia))%i(1)
						read(nu,err=990,end=880,iostat=ios)j_o(j_arg(ia))%d(ibas+1:ibas+ncol)
						ibas=ibas+ncol
					enddo !i=1,j_o(j_arg(ia))%i(1)  18248
				endif !if(j_o(j_arg(ia))%i(1).eq.1.or.j_o(j_arg(ia))%i(1).eq.1)  18239
			end do !ia=3,narg  18238
 
 
 
			return
			do ia=3,narg
 
				isiz=j_o(j_arg(ia))%i(3)
				read(nu,err=990,end=880,iostat=ios) j_o(j_arg(ia))%d(1:isiz)
 
 
			enddo !ia=3,narg  18258
 
		elseif(ivform0.eq.j_ivdollar)then
			write(6,*)'dollar'
			do ia=3,narg
 
				isiz=j_o(j_arg(ia))%i(3)
				read(nu,*,err=990,end=880,iostat=ios) j_o(j_arg(ia))%d(1:isiz)
 
 
			enddo !ia=3,narg  18268
 
			return
		else
 
 
		endif !if(ivform0.eq.j_ivb)  18209
	endif !if(j_otype(j_arg(3)).eq.j_ipmatrix)  18208
 
	!if(j_form(lef:lef).eq.'n')then
	!	read(nu,err=990,end=880,iostat=ios)n,(j_v(j_o(iob)%i(io+1+j)),j=4,min(n+3,narg))
	!	j_v(j_o(iob)%i(io+4))=n
	!else !if(j_form(lef:lef).eq.'n')then
 
 
	!	if(j_form(1:lef).eq.'bis')then
	! nel=0
	! do j=3,narg
	! nel=nel+j_o( j_o(iob)%i(io+1+j))%i(3)
	! enddo !j=3,narg  18120
	! allocate(bytes(1:nel))
	! read(nu,err=990,end=880)bytes
	! iel=0
	! do j=3,narg
	! nel=j_o(j_o(iob)%i(io+1+j))%i(3)
	! j_o( j_o(iob)%i(io+1+j) )%d(1: nel)=bytes(iel+1:iel+nel)
	! iel=iel+nel
	! enddo !j=3,narg  18126
	! deallocate(bytes)
	! else !if(j_form(1:lef).eq.'bis')then
 
 
 
 
	! nval=narg-2
	! if(j_n_vector.lt.nval)call j_checkr(j_vector,nval,j_n_vector)
 
	! !j_v(j_o(iob)%i(io+1+j)),j=3,narg)
	! read(nu,err=990,end=880,iostat=ios)j_vector(1:nval)
	! ii1=io+4
	! ii2=io+1+narg
	! j_v(j_o(iob)%i(ii1:ii2))=j_vector(1:nval)
	! return
 
	!endif !if(ivform0.eq.j_ivb)  18112
	if(ivform0.eq.j_ivdollar)then
		read(nu,*,err=990,end=880)j_v(j_arg(3:narg))
		return
 
	endif !if(ivform0.eq.j_ivdollar)  18319
	if(ivform0.eq.j_ivb2)then !if(j_form(1:1).eq.'b')then
		nval=narg-2
		if(j_n_dvector.lt.nval)call j_checkd(j_dvector,nval,j_n_dvector)
 
		!j_v(j_o(iob)%i(io+1+j)),j=3,narg)
		read(nu,err=990,end=880,iostat=ios)j_dvector(1:nval)
		ii1=io+4
		ii2=io+1+narg
		j_v(j_o(iob)%i(ii1:ii2))=j_dvector(1:nval)
		return
	endif !if(ivform0.eq.j_ivb2)  18324
	if(j_form(1:1).eq.'(')then !if(j_form(1:1).eq.'b')then
		if(j_form(lef:lef).eq.'n')then
			read(nu,fmt=j_form(1:lef-1),err=990,end=880,iostat=ios)n,(j_v(j_o(iob)%i(io+1+j)),j=4,min(n+3,narg))
			j_v(j_o(iob)%i(io+4))=n
			if(narg.lt.n+3)goto 770
		else !if(j_form(lef:lef).eq.'n')then
			read(nu,fmt=j_form(1:lef),err=990,end=880,iostat=ios)(j_v(j_o(iob)%i(io+1+j)),j=3,narg)
		endif !if(j_form(lef:lef).eq.'n')  18336
	else !if(j_form(1:1).eq.'b')then
		if(j_form(lef:lef).eq.'n')then
			read(nu,*,err=990,end=880,iostat=ios)n,(j_v(j_o(iob)%i(io+1+j)),j=4,min(n+3,narg))
			j_v(j_o(iob)%i(io+4))=n
			if(narg.lt.n+3)goto 770
			return
		elseif(ivform.eq.j_ivdollar)then !if(j_form(lef:lef).eq.'n')then
 
			if(j_otype(j_arg(3)).eq.j_ipmatrix)then
				read(nu,*,err=990,end=880)(j_o( j_o(iob)%i(io+1+j) )%d(1: j_o( j_o(iob)%i(io+1+j))%i(3)),j=3,narg)
			else !if(j_otype(j_arg(3)).eq.j_ipmatrix)then
				read(nu,*,err=990,end=880,iostat=ios)(j_v(j_o(iob)%i(io+1+j)),j=3,narg)
			endif !if(j_otype(j_arg(3)).eq.j_ipmatrix)  18351
		else !if(j_form(lef:lef).eq.'n')then
			write(6,*)'*read: illegal format ',j_form(1:lef)
			if(j_ninc.eq.1)write(6,*)' file remains open'
			j_err=.true.
			return
			!	endif !if(j_form(lef:lef).eq.'n')  18190
		end if !if(j_form(lef:lef).eq.'n')  18344
	endif !if(j_form(1:1).eq.'(')  18335
	900	continue
	return
	990 write(6,*)'**read: error in reading from file, form=',j_form(1:lef)
	write(6,*)'iostat=',ios
	j_err=.true.
	if(nu.eq.5)return
 
	write(6,*)'**closing file'
	call j_closeunit(nu)
 
	return
	880 continue
	if(eof(1).gt.0)then
		j_v(eof(1))=1
	else !if(eof(1).gt.0)then
		write(6,*)'**eof in file ',j_filename(1:le)
		write(6,*)'closing file'
		j_err=.true.
	endif !if(eof(1).gt.0)  18376
	!	write(6,*)'<12'
	call j_closeunit(nu)
	return
	770 write(6,*)'**n-format in read-function, not enough arguments to read ',n, 'varaibles'
	write(6,*)'closing file'
	call j_closeunit(nu)
	j_err=.true.
	return
end subroutine read !subroutine read(iob,io)
 
subroutine write(iob,io)   !write( ) %%io
	!Section write write() Writes to console or to file
	! write(file,format,arg1,…,argn)\\
	! Writes real values and text to a file or to the console. \\
 
	! Arguments:\\
	! file variable $ (indicating the console), or the name of the file as a character variable
	! or a character constant.\\
	! format
	!\begin{itemize}
	! \item $ indicates the '*' format of Fortran for numeric values. The numeric values are converted to single precision.
	! This format can be used also when there are CHAR and REAL arguments.
	!\item $$ indicates the '*' format of Fortran, works only for numeric values.
	! The numeric values are printed in double precision.
	! \item 'b' The values are converted to single precision and written to a binary file
	!\item 'B' The values are written to a binary file in double precision
	! \item A Fortran format statement, e.g. (~the values were ~,4f6.0) when there are numeric values to be written
	! \item If there are no arguments and the format is a CHAR, then this is written.
	!\end{itemize}
	! For other arguments the following rules apply:
	!\begin{itemize}
	!\item If the format is $ and the arguments are REAL or CHAR in any order, arguments are written
	! \item If there is one MATRIX argument then this matrix is written row after
	! 	row using the format indicated.
	! \item If there are several arguments which are REAL or MATRIX, then the values are put into a vector which is then written. If format is 'b' or  $ then
	!the values are converted to single precision. For matrices, all values written
	! without paying attention to to number of rows or columns. If matrices needs to be written
	! row by row, the matrices can be put into a single matrix with matrix operations
	! and then this matrix can be used as a single argument (after file and format).
	!\end{itemize}
	!endheader
	!Option
	! tab&-1-1& & if format is a Fortran format then, tab-> option indicates that sequences of
	! spaces are replaced by tab character so that written text can be easily converted
	! to Ms Word tables. If there are no decimals after the decimal point also the
	! decimal point is dropped.
	!endoption
	! Note There have been options to describe the format with tab and
	!width parameters but they may not work now. If such formats are needed,
	! 	I can reinstall them.
	!endnote
	!Note Also text objects can be written, but I must check how this works now.
	!endnote
 
	!endsection
 
	!
	! write(file,'t',t1,val1,t2,val2,…,tn,valn[,tab->]) ! case[2/6]
	! Tabulation format. positive tab position values indicate that the value is written starting from
	! that position, negative tab positions indicate that the value is written up to that position. The
	! values can be either numeric expressions or character variables or character constants. Tab
	! positions can be in any order.
	!
	!end gheader
 
	!enndsection
 
	!Variable names from a variable list can be written using @-construction. E.g. if xl is a
	! variable list, then both the name and value of element i can be written:
	! write($,'w',1,'@xl(i)=',8,@xl(i))
	! write(file,'w',w1,val1,w2,val2,…,wn,valn[,tab->]) ! case[3/6]
	! Width format: positive w-value indicates that the value is right-justified into field of that length,
	! negative w-values indicate that the value is left-justified. The value can be either numeric or
	! character expression.
	! In both 't' and 'w' format with integer w-value, numeric values are converted into character
	! expression with 8 characters. This special formatting drops unnecessary decimal points, leading
	! and ending zeros, and will give as much precision as can be obtained using 8 characters. If less
	! than 8 characters are needed, then one can use shorter fields than 8 characters.
	! A decimal w-value works similarly as f-format of Fortran, thus w-value 8.2 is equivalent to f8.2.
	! For technical reasons, the format with zero decimals but with decimal point included must be
	! given with w-value having decimal part .01, e.g. w value 5.01 is equivalent to f5.0. Note that
	! writing with zero decimal using e.g. 5,nint(value) will drop also the decimal point
	! (corresponding to I format of Fortran).
	! Arguments:
	! file variable $ (indicating the console), or the name of the file as a character variable
	! or a character constant, or variable $Buffer
	! 'w' width format
	! w1,val1,w2,val2,…,wn,valn
	! KUVAUS
	! Options:
	! tab option indicates that sequences of spaces are replaced by tab character so that
	! writ-ten text can be easily converted to Ms Word tables.
	! note. Variable names from a variable list can be written using @-construction. E.g. if xl is a
	! variable list, then both the name and value of element i can be written:
	! write($,'w',8,'@xl(i)=',7,@xl(i))
	! When first write to a file is done, then the file will be opened. If the file already exists then J
	! asks if the old file can be deleted. Note that before answering you can rename the file. In that
	! case the old file will be saved even if you answer ‘y’.
	! write(file,text_object) ! case[4/6]
	! A text object can be written into a file using this form of write() function.
	! Arguments:
	! file variable $ (indicating the console), or the name of the file as a character variable
	! or a character constant, or variable $Buffer
	! text_object
	! Jlp22 text object
	! write(file,character) ! case[5/6]
	! When second argument is a character constant or character variable referring to a character
	! constant, then it is written to the file (or to the screen is file is $). Note character constant can
	! contain “”-sequences, and after J3.0 these can contain also format specifier.
	! Arguments:
	! file variable $ (indicating the console), or the name of the file as a character variable
	! or a character constant, or variable $Buffer, or REAL whose value is positive integer
	! character a character constant or a variable referring to character constant
 
	! note: You can put character information into the format (to put apostrophe within character
	! constant use (~),see Character constants and variables).
	! Examples:
	! write(‘out.txt’,’sin(“[f4.4]a” is “[f5.3]sin(a)”, believe or not’)
	! dir='d:\j\'
	! write('"dir"example.out','(~the values were ~,4f4.0)',@values)
	! Writing into $Buffer ! case[6/6]
	! If variable $Buffer is used as the file argument, then different write() function calls can put
	! information on the same line. Writing into $Buffer has the following logic. Other parts of J
	! consider $Buffer as real variable. The output buffer can be initialized by giving value zero to
	! $Buffer (i.e. giving command $Buffer=0), this is the situation initially. One can write onto
	! the buffer using $,'(...)', 't', or 'w' -formats. $ and '(... )' formats will also initialize
	! the buffer first, so only 't', and 'w' formats can be used to collect buffer in several parts.
	! After writing into the buffer, the real variable $Buffer gets the current length of the output.
	! The current output buffer can be written into file using either
	! write(file,$Buffer)
	! or $Buffer can be used similarly as character variables in writing with 'w' or 't' format, e.g.
	! write($,'t',1,$Buffer,$Buffer+2,'kukuu')
	! In the above first $Buffer indicates the current content of the buffer. In the tab value
	! $Buffer+2 indicates that the tab position is two characters past the buffer length.
	! close(file)
	! Closes an open file.
	! Argument:
	! file a character variable or constant telling the name of an open file. The file has been
	! created and opened with write(), print() or save() functio
	!%%endssection
 
	logical bin,wform,bin8,single
	character*60 luku
	character*1000 line_
	character*8 for
	character*1 ff
	logical isopen,istex
	logical ismat,tabform,chr ! single,chr
	logical isvc
	!real, dimension(:),allocatable::temp
	!double precision, dimension(:),allocatable::dtemp
	!	integer,dimension(:),pointer ::arg2
	integer,dimension(:),pointer ::arg
	data for/'(f  .  )'/
	real :: val_
 
	!	io=io_
 
 
	!call j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	call j_startfunction(iob,io,0,narg0,j_optarg0,ivout);if(j_err)return !args of the function
	if(narg0.gt.2)arg=>j_optarg0(3:narg0) !objects to be written
	!write(6,*)'arg2',arg2
	narg=narg0-2
	!read(5,*)diidid
	!	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	ivfile=j_optarg0(1)
	ivform=j_optarg0(2)
	tabform= j_igetopt(iob,io,j_mtab) .ge.0
	!	write(6,*)'narg ',narg,'arg',arg,'narg0 ',narg0
	call j_getoption(iob,io,j_mrows,-1,2,j_ipreal,.true.,nargval,j_optarg0)
	if(j_err)return
	nrow1=0
	nrow2=0
	if(nargval.eq.1)then
		nrow2=j_v(j_optarg0(1))
	elseif(nargval.eq.2)then !if(nargval.eq.1)then
		nrow1=j_v(j_optarg0(1))
		nrow2=-j_v(j_optarg0(2))
		if(nrow1.lt.1.or.nrow2.lt.nrow1)then
			write(6,*)'*write, illegal rows-> ',nrow1,-nrow2
			j_err=.true.;return
		endif !if(nrow1.lt.1.or.nrow2.lt.nrow1)  18563
	endif !if(nargval.eq.1)  18558
 
	!!call j_clearoption(iob,io)  ! subroutine
 
	!write(6,*)'narg,narg0,ivfile,ivform,arg',narg,narg0,ivfile,ivform,arg
 
 
	bin=ivform.eq.j_ivb
	bin8=ivform.eq.j_ivb2
	! if(j_otype(ivform).eq.j_ipchar)then
	! ff=j_o(j_ivnames)%ch(j_o(ivform)%i(1))
	! ! bin=ff.eq.'b'
	! ! bin8=ff.eq.'B'
	! chr=ff.eq.'c'
	! endif !if(j_otype(ivform).eq.j_ipchar)  15571
 
	if(ivfile.eq.j_ivdollar)then
		nu=6
		if(bin.or.bin8)then
			write(6,*)'cannot write with binary format to console'
			j_err=.true.;return
		endif !if(bin.or.bin8)  18585
	else if(j_otype(ivfile).eq.j_ipchar)then !if(ivfile.eq.j_ivdollar)then
		nu=j_iounit(ivfile) !j_o(ivfile)%i(4)
		!	write(6,*)'<555,nu',nu
		if(nu.le.0)then
			! file not yet opened
			if(j_otype(ivform).eq.j_iptext)then
				!subroutine j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout)
				call j_getfile(nu,'w',ivfile=ivfile)
				!write(6,*)'<771',nu
				!call j_getwritefile(ivfile,j_ivdollar,bin)
			else !if(j_otype(ivform).eq.j_iptext)then
 
				call j_getfile(nu,'w',ivfile=ivfile,ivform=ivform)
				!		write(6,*)'<771',nu,bin,ivform
				!call j_getwritefile(ivfile,ivform,bin)
			endif !if(j_otype(ivform).eq.j_iptext)  18594
			if(j_err)return
			!nu=j_iounit(ivfile) !j_o(ivfile)%i(4)
			!j_unitchar(nu)=ivfile
		else !if(nu.le.0)then
			if(j_unitchar(nu).lt.0)then
				write(6,*)'write: trying to write into read file'
				j_err=.true.
				return
			endif !if(j_unitchar(nu).lt.0)  18609
		end if !if(nu.le.0)  18592
 
	else if(ivfile.eq.j_ivbuffer)then !if(ivfile.eq.j_ivdollar)then
		if(j_v(j_ivbuffer).eq.0)j_buffer=' '
	elseif(j_otype(ivfile).eq.j_ipreal)then
		nu=j_v(ivfile)
		if(dble(nu).ne.j_v(ivfile).or.nu.le.0)then
			write(6,*)'REAL file argument in write() ',j_v(ivfile),' should be positive integer '
			j_err=.true.;return
		endif !if(dble(nu).ne.j_v(ivfile).or.nu.le.0)  18620
	else
		write(6,*)'**illegal file in write';j_err=.true.;return
	end if !if(ivfile.eq.j_ivdollar)  18583
 
 
 
	if(ivform.eq.j_ivdollar.or.ivform.eq.j_ivdollar2)then
		if(narg.eq.0)then
			write(nu,*)' '
			return
		endif !if(narg.eq.0)  18631
		if(narg.eq.1.and.j_otype(arg(1)).eq.j_iptext)then
			if(nu.eq.6)then
				call j_writetext(nu,arg(1),0)
			else
				call j_writetext(nu,arg(1),0,.true.)
 
			endif !if(nu.eq.6)  18636
			return
		endif !if(narg.eq.1.and.j_otype(arg(1)).eq.j_iptext)  18635
		!		write(6,*)'hep'
		if(j_writevar(nu,narg,arg,ivform))return
 
	endif !if(ivform.eq.j_ivdollar.or.ivform.eq.j_ivdollar2)  18630
	!	write(6,*)'ivform,ivbuffer',ivform,j_ivbuffer
	single=bin.or.ivform.eq.j_ivdollar
	!	write(6,*)'ivform,singel',ivform,single
	if(narg.gt.1)then
		nle=0
		if(single)then
			j_i8=narg
			call j_allocates(j_vector,j_i8)
			do i=1,narg
				call j_getname(arg(i))
				!		write(6,*)'var ',arg(i),j_oname(1:j_loname)
				if(j_otype(arg(i)).eq.j_ipreal)then
					nle=nle+1
					call j_putr(j_vector,nle,real(j_v(arg(i))))
				elseif(j_otype(arg(i)).eq.j_ipmatrix)then
 
					nel=j_nelem(arg(i))
					!			write(6,*)'nel,nle',nel,nle
					call j_putr(j_vector,nle+nel,real(j_o(arg(i))%d(nel)))
					j_vector(nle+1:nle+nel-1)=j_o(arg(i))%d(1:nel-1)
					nle=nle+nel
				else
					call j_getname(arg(i))
					write(6,*)j_oname(1:j_loname),' is not REAL or MATRIX'
					j_err=.true.;return
 
				endif !if(j_otype(arg(i)).eq.j_ipreal)  18659
 
			enddo !i=1,narg  18656
			if(bin)then
				write(nu)j_vector(1:nle)
			else
				write(nu,*)j_vector(1:nle)
			endif !if(bin)  18677
		else
			do i=1,narg
				call j_getname(arg(i))
				!		write(6,*)'var ',arg(i),j_oname(1:j_loname)
				if(j_otype(arg(i)).eq.j_ipreal)then
					nle=nle+1
					call j_putd(j_dvector,nle,j_v(arg(i)))
				else
					nel=j_nelem(arg(i))
					call j_putd(j_dvector,nle+nel,j_o(arg(i))%d(nel))
					j_dvector(nle+1:nle+nel-1)=j_o(arg(i))%d(1:nel-1)
					nle=nle+nel
				endif !if(j_otype(arg(i)).eq.j_ipreal)  18686
 
			enddo !i=1,narg  18683
			if(bin8)then
				write(nu)j_dvector(1:nle)
			elseif(ivform.eq.j_ivdollar2)then
				write(nu,*)j_dvector(1:nle)
			else
				if(j_otype(ivform).eq.j_ipchar)then
					call j_getchar(ivform,j_form,lef)
				else
					call j_getname(ivform)
					write(6,*)'illegal format ',j_oname(1:j_loname)
					j_err=.true. ;return
				endif !if(j_otype(ivform).eq.j_ipchar)  18702
 
				!		write(6,*)'write1',nu,ivform,j_otype(ivform),lef,j_form(1:lef),j_dvector(1:nle)
				write(nu,j_form(1:lef),err=989)j_dvector(1:nle)
			endif !if(bin8)  18697
 
		endif !if(single)  18653
		return
	endif !if(narg.gt.1)  18651
 
 
800	if(ivform.eq.j_ivbuffer)then
		if(j_v(j_ivbuffer).eq.0.)then
			j_buffer=' ';write(nu,*)' '
		else !if(j_v(j_ivbuffer).eq.0.)then
			lenbu=len_trim(j_buffer)
			if(tabform)then
				line_(1:1)=' '
				j0=0
				do i=1,lenbu
					if(j_buffer(i:i).ne.' ')then
						i1=max(i-1,1)
						if(j0.gt.0.and.j_buffer(i1:i1).eq.' ')then
							if(line_(j0:j0).ne.'.')j0=j0+1
							line_(j0:j0)=char(9)
						endif !if(j0.gt.0.and.j_buffer(i1:i1).eq.' ')  18730
 
						j0=j0+1
						line_(j0:j0)=j_buffer(i:i)
					endif !if(j_buffer(i:i).ne.' ')  18728
				enddo !i=1,lenbu  18727
				if(j0.gt.0)then
					if(line_(j0:j0).eq.'.')j0=j0-1
				endif !if(j0.gt.0)  18739
				!	if(nu.ne.6)call j_toutf8(line_(1:max(j0,1)))
				!write(6,*)'<12 ',line_(1:max(j0,1))
				write(nu,'(a)',err=990)line_(1:max(j0,1))
			else !if(tabform)then
				!if(nu.ne.6)call j_toutf8(j_buffer(1:lenbu))
				!write(6,*)'<13 ',j_buffer(1:lenbu)
				write(nu,'(a)',err=990)j_buffer(1:lenbu)
			endif !if(tabform)  18724
		end if !if(j_v(j_ivbuffer).eq.0.)  18720
		return
	elseif(j_otype(ivform).eq.j_iptext.and.narg.eq.0)then !if(ivform.eq.j_ivbuffer)then
		!	write(6,*)'>128'
		if(nu.eq.6)then
			call j_writetext(nu,ivform,0)
		else
			call j_writetext(nu,ivform,0,.true.)
 
		endif !if(nu.eq.6)  18754
		return
 
		! !		WRITE(6,*)'<J331NU,ITE,0,.TRUE',nu,ite
		! call j_writetext(nu,0,0,.true.)
		! return
	end if !800	if(ivform.eq.j_ivbuffer)  18719
	if(narg.le.0)then
		if(j_otype(ivform).eq.j_ipchar)then
			call j_getchar(ivform,j_form,lef);if(j_err)return
			!		if(nu.ne.6)call j_toutf8(j_form(1:lef))
			!write(6,*)'>129',j_form(1:lef)
			write(nu,'(a)')j_form(1:lef)
 
			return
		else !if(ivform.ne.j_ivdollar.and.ivfile.ne.j_ivbuffer.and.j_otype(ivform).eq.j_ipchar)then
			goto 990
		endif !if(j_otype(ivform).eq.j_ipchar)  18767
	endif !if(narg.le.0)  18766
 
	! if(chr)then
	! le2=0
	! do ii=1,narg
	! iv=arg(ii)
 
	! if(j_otype(iv).ne.j_ipchar)then
	! call j_printname('write ',iv,' is not CHAR')
	! j_err=.true.
	! else !if(j_otype(iv).ne.j_ipchar)then
	! call j_getchar(iv,j_tempchar2,le)
	! if(le.gt.0)then
	! j_tempchar(le2+1:le2+le)=j_tempchar2(1:le)
	! endif !if(le.gt.0)  15691
	! le2=le2+le
	! endif !if(j_otype(iv).ne.j_ipchar)  15686
	! enddo !ii=1,narg  15683
	! if(le2.gt.0)write(nu,'(a)',err=990)j_tempchar(1:le2)
	! return
 
	! endif !if(chr)  15681
	if(j_otype(ivform).eq.j_ipchar)call j_getchar(ivform,j_form,lef);if(j_err)return
 
	irg=arg(1) !first arg
	!write(6,*)'irg',irg,'narg',narg
 
	if(narg.eq.1.and.j_otype(irg).eq.j_ipmatrix)then
 
		! if(nargval.le.0)then
		!single=j_otype(irg).eq.j_ipmatrix0 !j_o(irg)%i(13).ne.0
		! else
		! nval=min(nval,j_o(irg)%i(3))
		! endif
		nrow1c=max(nrow1,1)
		nrows=j_o(irg)%i(1)
		nrow2c=nrow2
		if(nrow2.le.0.or.nrow2.gt.nrows)nrow2c=nrows
 
		ncols=j_o(irg)%i(2)
		nval=ncols*(nrow2c-nrow1c+1)
		if(ivform.eq.j_ivdollar2)then
			if(ncols.eq.1)then
				! if(single)then
				! write(nu,*,err=990)j_o(irg)%d(nrow1c:nrow2c)
				! else !if(single)then
				write(nu,*,err=990)j_o(irg)%d(nrow1c:nrow2c)
				!endif !if(single)then
			else !if(ncols.eq.1)then
 
				do ir=nrow1c,nrow2c
					write(nu,*,err=990)j_o(irg)%d((ir-1)*ncols+1:ir*ncols)
				enddo !ir=nrow1c,nrow2c  18827
				!endif !if(single)then
			endif !if(ncols.eq.1)  18819
			return
		elseif(ivform.eq.j_ivdollar)then
 
			if(ncols.eq.1)then
				! if(single)then
				! write(nu,*,err=990)j_o(irg)%d(nrow1c:nrow2c)
				! else !if(single)then
				write(nu,*,err=990)real(j_o(irg)%d(nrow1c:nrow2c))
				!endif !if(single)then
			else !if(ncols.eq.1)then
 
				do ir=nrow1c,nrow2c
					write(nu,*,err=990)real(j_o(irg)%d((ir-1)*ncols+1:ir*ncols))
				enddo !ir=nrow1c,nrow2c  18843
				!endif !if(single)then
			endif !if(ncols.eq.1)  18835
			return
		elseif(bin)then !if(ivform.eq.j_ivdollar)then
 
			if(ncols.eq.1)then
				write(nu,err=990)real(j_o(irg)%d(nrow1c:nrow2c))
			else !if(ncols.eq.1)then
				do ir=nrow1c,nrow2c
					write(nu,err=990)real(j_o(irg)%d((ir-1)*ncols+1:ir*ncols))
				enddo !ir=nrow1c,nrow2c  18854
			endif !if(ncols.eq.1)  18851
			!endif !if(single)then
			return
		elseif(bin8)then !if(ivform.eq.j_ivdollar)then
			if(ncols.eq.1)then
				write(nu,err=990)j_o(irg)%d(nrow1c:nrow2c)
			else !if(ncols.eq.1)then
				do ir=nrow1c,nrow2c
					write(nu,err=990)j_o(irg)%d((ir-1)*ncols+1:ir*ncols)
				enddo !ir=nrow1c,nrow2c  18864
				return
			endif !if(ncols.eq.1)  18861
 
		elseif(j_otype(ivform).eq.j_ipchar)then !if(ivform.eq.j_ivdollar)then
 
			if(ncols.eq.1)then
				write(nu,j_form(1:lef),err=989)j_o(irg)%d(nrow1c:nrow2c)
			else !if(ncols.eq.1)then
				!			write(6,*)'nrow1c,nrow2c',nrow1c,nrow2c,ncols
				do ir=nrow1c,nrow2c
					!			write(6,*,err=990)ir,j_o(irg)%d((ir-1)*ncols+1:ir*ncols)
					write(nu,j_form(1:lef),err=989)j_o(irg)%d((ir-1)*ncols+1:ir*ncols)
				enddo !ir=nrow1c,nrow2c  18876
			endif !if(ncols.eq.1)  18872
			return
			!endif !if(single)then
		endif !if(ivform.eq.j_ivdollar2)  18818
		ismat=.true.
	else !if(narg.eq.1.and.j_otype(irg).eq.j_ipmatrix)then
		ismat=.false.
		!	nval=narg-2
	endif !if(narg.eq.1.and.j_otype(irg).eq.j_ipmatrix)  18804
 
	!write(6,*)'tassa '
 
	if(bin)then
		if(j_n_vector.lt.narg)call j_checkr(j_vector,narg,j_n_vector)
		! then
		! if(allocated(j_vector))deallocate(j_vector)
		! allocate(j_vector(1:narg))
		! j_n_vector=narg
		! endif !if(j_n_vector.lt.narg)  17230
		!	write(6,*)'<55>',j_v( j_o(iob)%i(io+4:io+3+nval) )
		j_vector(1:narg)=j_v( arg(1:narg) )
		!	write(6,*)'<388383',j_vector(1:nval)
		!write(6,*)'write2',nu,j_form(1:lef),j_dvector(1:nle)
		write(nu,err=990)j_vector(1:narg) !(v(o(iob)%i(io+1+j)),j=3,narg)
		return
	endif !if(bin)  18892
 
 
 
	if(narg.gt.0)then !if(ismat)then
		if(j_n_dvector.lt.narg)call j_checkd(j_dvector,narg,j_n_dvector)
		! then
		! if(allocated(j_dvector))deallocate(j_dvector)
		! allocate(j_dvector(1:narg))
		! j_n_dvector=narg
		! endif !if(j_n_dvector.lt.narg)  17245
		!		write(6,*)'>3>',j_v( j_o(iob)%i(io+4:io+3+nval) )
		j_dvector(1:narg)=j_v( arg(1:narg) )
	endif !if(narg.gt.0)  18909
 
 
	if(bin8.and.narg.gt.0)then
		!	write(6,*)'nva',nval
		write(nu,err=990)j_dvector(1:narg)
		return
	endif !if(bin8.and.narg.gt.0)  18921
 
	if(ivform.eq.j_ivdollar)then !if(bin)then
 
		write(nu,*,err=990)real(j_vector(1:narg))
 
 
 
	elseif(ivform.eq.j_ivdollar2)then
		if(ivfile.ne.j_ivbuffer)then
			write(nu,*,err=990)j_dvector(1:narg) !(v(o(iob)%i(io+1+j)),j=3,narg)
		else !if(ivfile.ne.j_ivbuffer)then
			write(j_buffer,*,err=990)j_dvector(1:narg) !(v(o(iob)%i(io+1+j)),j=3,narg)
			j_v(j_ivbuffer)=len_trim(j_buffer)
		end if !if(ivfile.ne.j_ivbuffer)  18934
 
 
	else !if(ivform.eq.j_ivdollar)then
		call j_getchar(ivform,j_form,lef);if(j_err)return
		!	if(nu.ne.6)call j_toutf8(j_form(1:lef))
		if(j_form(1:1).eq.'(')then
			if(ivfile.ne.j_ivbuffer)then
				if(tabform)then
					write(line_,fmt=j_form(1:lef),err=989)j_dvector(1:narg)
					lenbu=len_trim(line_)
					j0=0
					do i=1,lenbu
						if(line_(i:i).ne.' ')then
							i1=max(i-1,1)
							if(j0.gt.0.and.line_(i1:i1).eq.' ')then
								if(line_(j0:j0).ne.'.')j0=j0+1
								line_(j0:j0)=char(9)
							endif !if(j0.gt.0.and.line_(i1:i1).eq.' ')  18954
							j0=j0+1
							line_(j0:j0)=line_(i:i)
						endif !if(line_(i:i).ne.' ')  18952
					enddo !i=1,lenbu  18951
					if(j0.gt.0)then
						if(line_(j0:j0).eq.'.')j0=j0-1
					endif !if(j0.gt.0)  18962
					!	if(nu.ne.6)call j_toutf8(line_(1:max(j0,1)))
					!	write(6,*)'>12' ,line_(1:max(j0,1))
					write(nu,'(a)',err=990)line_(1:max(j0,1))
				else !if(tabform)then
					!	write(6,*)'>129' ,temp(1:nval)
 
					write(nu,fmt=j_form(1:lef),err=989)j_dvector(1:narg)
				endif !if(tabform)  18947
			else !if(ivfile.ne.j_ivbuffer)then
				!	write(6,*)'>1288'
				write(j_buffer,fmt=j_form(1:lef),err=989)j_dvector(1:narg) ! (v(o(iob)%i(io+1+j)),j=3,narg)
				j_v(j_ivbuffer)=len_trim(j_buffer)
			end if !if(ivfile.ne.j_ivbuffer)  18946
		else if(j_form(1:lef).eq.'t'.or.j_form(1:lef).eq.'w')then !if(j_form(1:1).eq.'(')then
			if(ismat)then
				write(6,*)'write: matrix cannot be written with form->',j_form(1:lef)
				j_err=.true.
				return
			endif !if(ismat)  18979
			if(mod(narg,2).ne.0)then; write(6,*)'**illegal nro of args in write';j_err=.true.;return;end if
			wform=.false.;if(j_form(1:1).eq.'w')then ;wform=.true.;lew=0;end if
			line_=' '
			do j=1,narg,2
				tab=j_v(arg(j))
				itab=tab  !either tab or w
				if(abs(itab).gt.80)then
					write(6,*)'**illegal tab in writing with t-format:',itab;j_err=.true.;return
				end if !if(abs(itab).gt.80)  18990
				iv=arg(j)
				if(j_otype(iv).eq.j_ipreal)then
					val_=j_v(iv)
					if(wform.and.nint(100.*tab).ne.100*itab)then
					33 						format(i2)
						write(for(3:4),33)itab
						write(for(6:7),33)nint(10.*tab)-10*itab
						write(luku,for)val_
						le=itab
					else !if(wform.and.nint(100.*tab).ne.100*itab)then
						luku(1:8)=j_chr8b(val_,le)
					end if !if(wform.and.nint(100.*tab).ne.100*itab)  18996
				else if(j_otype(iv).eq.j_ipchar)then !if(j_otype(iv).eq.j_ipreal)then
					call j_getchar(iv,luku,le);if(j_err)return
					!	write(6,*)'got:',luku(1:le)
				else !if(j_otype(iv).eq.j_ipreal)then
					call j_printname('**illegal variable type in write:',iv,' ')
					j_err=.true.;return
				end if !if(j_otype(iv).eq.j_ipreal)  18994
				if(wform.and.le.gt.itab)le=itab
				if(itab.lt.0)then
					itab=-itab
					if(wform)then   ! lew cumulative lentgth
						i1=lew+1;i2=i1+le-1;lew=lew+itab
					else !if(wform)then
						i1=max(itab-le+1,1);i2=itab
					end if !if(wform)  19015
				else !if(itab.lt.0)then
					if(wform)then
						i2=lew+itab;i1=i2-le+1;lew=lew+itab  !right justified
					else !if(wform)then
						i1=itab;i2=min(itab+le-1,80)
					end if !if(wform)  19021
				end if !if(itab.lt.0)  19013
				line_(i1:i2)=luku(1:le)
			end do !j=1,narg,2  18987
			lel=j_lentrim(line_)
			if(ivfile.ne.j_ivbuffer)then
				if(tabform)then
					line_(1:1)=' '
					j0=0
					do i=1,lel
						if(line_(i:i).ne.' ')then
							i1=max(i-1,1)
							if(j0.gt.0.and.line_(i1:i1).eq.' ')then
								if(line_(j0:j0).ne.'.')j0=j0+1
								line_(j0:j0)=char(9)
							endif !if(j0.gt.0.and.line_(i1:i1).eq.' ')  19037
							j0=j0+1
							line_(j0:j0)=line_(i:i)
						endif !if(line_(i:i).ne.' ')  19035
					enddo !i=1,lel  19034
					if(j0.gt.0)then
						if(line_(j0:j0).eq.'.')j0=j0-1
					endif !if(j0.gt.0)  19045
					write(nu,'(a)',err=990)line_(1:max(j0,1))
				else !if(tabform)then
					write(nu,'(a)',err=990)line_(1:lel)
				endif !if(tabform)  19031
			else !if(ivfile.ne.j_ivbuffer)then
				if(wform)then
					leb=j_v(j_ivbuffer)
					if(leb+lel.gt.256)then
						write(6,*)'**write: $Buffer is only 256 char long'
						j_err=.true.
						return
					endif !if(leb+lel.gt.256)  19055
					j_buffer(leb+1:leb+lel)=line_(1:lel)
					j_v(j_ivbuffer)=leb+lel
				else !if(wform)then
					do j=1,lel
						if(line_(j:j).ne.' ')j_buffer(j:j)=line_(j:j)
					enddo !j=1,lel  19063
					j_v(j_ivbuffer)=j_lentrim(j_buffer)
				endif !if(wform)  19053
			end if !if(ivfile.ne.j_ivbuffer)  19030
		else !if(j_form(1:1).eq.'(')then
			write(6,*)'**illegal format in write';j_err=.true.
		end if !if(j_form(1:1).eq.'(')  18945
	end if !if(ivform.eq.j_ivdollar)  18927
	900 continue !   if(j_err)return
	!write(6,*)'<776nu,iv ',j_iounit(ivfile),ivfile
	return
	989  write(6,*)'format:',j_form(1:lef)
	990  write(6,*)'**error in writing, file remains open'
	j_err=.true. !;return
	return
end subroutine write !subroutine write(iob,io)
 
 
subroutine elementprod(iob,io)
 
	integer irg(2)
	integer*8 ::ndim1(2),ndim2(2)
	narg=j_o(iob)%i(io+1)
 
	do i=1,2
		irg(i)=j_o(iob)%i(io+1+i)
		if(j_otype( irg(i)).eq.j_ipmatrix)then
			!	ndim1(i)=j_o(irg(i))%i8(1);ndim2(i)=j_o(irg(i))%i8(2)
			ndim1(i)=j_nrows(irg(i));ndim2(i)=j_ncols(irg(i))
		else !if(j_otype( irg(i)).eq.j_ipmatrix)then
			if(j_otype( irg(i)).eq.j_ipreal)then
				ndim1(i)=0
			else !if(j_otype( irg(i)).eq.j_ipreal)then
				call j_printname('**object ',irg(i),' has illegal type in elementprod')
				write(6,*)'#type is:',j_otypes( j_otype(irg(i)))
				j_err=.true.
				return
			endif !if(j_otype( irg(i)).eq.j_ipreal)  19095
		end if !if(j_otype( irg(i)).eq.j_ipmatrix)  19091
	end do !i=1,2  19089
 
	iout=j_o(iob)%i(io+2+narg)
	if(ndim1(1).eq.0.and.ndim1(2).eq.0)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_v(irg(1))*j_v(irg(2))
	else if(ndim1(1).ne.0.and.ndim1(2).ne.0)then !if(ndim1(1).eq.0.and.ndim1(2).eq.0)then
		nel=ndim1(1)*ndim2(1)
		if(irg(1).ne.iout.and.irg(2).ne.iout)ivout=j_defmatrix(iout,' ',&
			ndim1(1),ndim2(1),j_matreg)
		j_o(iout)%d=j_o(irg(1))%d(1:nel)*j_o(irg(2))%d(1:nel)
	else !if(ndim1(1).eq.0.and.ndim1(2).eq.0)then
		write(6,*)'**arguments of elementprod must both be matrices or both real numbers'
		j_err=.true.
	end if !if(ndim1(1).eq.0.and.ndim1(2).eq.0)  19107
	900 if(irg(1).gt.j_named)call j_del(irg(1))
	if(irg(2).gt.j_named)call j_del(irg(2))
	return
end subroutine elementprod !subroutine elementprod(iob,io)
 
 
 
 
 
subroutine t(iob,io) ! t(mat)  transpose  %%matrix
	! Section t t() Transpose of a MATRIX or a LIST
	! t(MATRIX) is the transpose of a MATRIX. As LIST objects can now
	! be used in matrix computations, t(LIST) is also available.
	! endheader
	! Note Multiplying a matrix by the transpose of a matrix can be made by
	! making new operation '*.
	! endnote
	! Note The argument matrix can also be a submatrix expression.
	! endnote
	! endsection
 
 
	double precision,dimension(:),allocatable :: copy
 
	integer*8::ndim1,ndim2,i,j,ibain,ibaout
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	irg=j_o(iob)%i(io+2)
	if(j_otype(irg).ne.j_ipmatrix)then
		if(j_otype(irg).eq.j_ipreal)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=j_v(irg)
 
		elseif(j_otype(irg).eq.j_iplist)then !if(j_otype(irg).eq.j_ipreal)then
			ivout=j_defmatrix(iout,' ',j_18,int8(j_o(irg)%i(0)),j_matreg)
			j_o(iout)%d=j_v(j_o(irg)%i(1: j_o(irg)%i(0)))
		else !if(j_otype(irg).eq.j_ipreal)then
			call j_printname('illegal type of ',irg, ' for transpose')
			write(6,*)'type is: ',j_otypes( j_otype(irg))
			j_err=.true.
 
		endif !if(j_otype(irg).eq.j_ipreal)  19148
		return
	end if !if(j_otype(irg).ne.j_ipmatrix)  19147
	!ndim1=j_o(irg)%i8(1);ndim2=j_o(irg)%i8(2)
	ndim1=j_nrows(irg);ndim2=j_ncols(irg)
	allocate(copy(1:ndim1*ndim2))
	ibain=0
	do i=1,ndim1
		ibaout=i
 
		do j=1,ndim2
			!j_o(iout)%d((j-1)*ndim1+i)   =copy((i-1)*ndim2+j)
			copy(ibaout)=j_o(irg)%d(ibain+j)
			ibaout=ibaout+ndim1
		enddo !j=1,ndim2  19170
		ibaout=ibaout+1
		ibain=ibain+ndim2
	enddo !i=1,ndim1  19167
 
	if(irg.eq.iout)then
 
		call j_defmatdim(irg,ndim2,ndim1)
	else
		ivout=j_defmatrix(iout,' ',ndim2,ndim1,j_matreg,nod=.true.)
	endif !if(irg.eq.iout)  19179
	call move_alloc(from=copy,to=j_o(iout)%d)
 
	return
end subroutine t !subroutine t(iob,io)
 
subroutine inverse(iob,io)  !inverse(mat)
	!Section inverse inverse() Inverse and condition of MATRIX
	! inverse(matrixa) computes the inverse of a square MATRIX matrixa. The function utilized dgesv funtion
	! of netlib. If the argument has type REAL, then the reciprocal is computed,
	! and the output will also have type REAL. An error occurs, if matrixa is not
	! a square matrix or REAL, or matrixa is singular according to dgesv.
	!If the output is a named object (i.e. not a temporary object), the condition number is stored in REAL with name Output%condition.
	!endheader
	!Note The condition number is \textbf{not} put into input%conditon which could be more logical.
	!endnote
	!Ex inverseex inverse() and condition number
	! matrixa=matrix(4,4)
	! matrixa=1
	! ***  well conditoned matrix
	! matrixa(diag->)=10
	! matrixa;
	! matrixb=inverse(matrixa);
	! matrixb%condition;
	! ** almost singular matrix
	! matrixa(diag->)=1.05
	! matrixb=inverse(matrixa);
	! matrixb%condition;
	! ** figure of condition number
	! transa=trans()
	! matrixa(diag->)=diag
	! matrixb=inverse(matrixa)
	! /
	! ** Note that the lower bound is equal to the dimension
	! figa=draw(x->diag,xrange->(1.05,50),func->transa(matrixb%condition),
	! color->Blue,continue->fcont)
	!endex
	!
	!Note instead of writing c=inverse(a)*b, it is faster and more accurate to
	!write c=solve(a,b)
	!endnote
	!endsection
 
	real*8,dimension(:,:),allocatable :: mat
	real*8,dimension(:,:),allocatable :: rhs_
	integer,dimension(:),allocatable ::ipiv
	integer,dimension(:), pointer::arg=>null()
	!	integer*8::ndim1,ndim2
 
	call j_startfunction(iob,io,0,narg,arg,iout);if(j_err)return
 
	irg=arg(1)  !o(iob)%i(io+2)
	if(j_otype(irg).ne.j_ipmatrix)then
		if(j_otype(irg).eq.j_ipreal)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=1./j_v(irg)
			return
		else !if(j_otype(irg).eq.j_ipreal)then
			call j_printname('**argument of inverse ',irg,' not a matrix or real var')
			write(6,*)'#type is:',j_otypes( j_otype(irg))
			j_err=.true. ;return
		end if !if(j_otype(irg).eq.j_ipreal)  19237
	end if !if(j_otype(irg).ne.j_ipmatrix)  19236
	ndim1=j_nrows(irg)  !j_o(irg)%i8(1)
	ndim2=j_ncols(irg)   !j_o(irg)%i8(2)
	if(ndim1.ne.ndim2)then
		call j_printname('**argument ',irg,' of inverse is  not a square matrix')
		j_err=.true. ; return
	end if !if(ndim1.ne.ndim2)  19249
	if(j_o(irg)%i(4).eq.j_matdiag)then
		if(iout.ne.irg)ivout=j_defmatrix(iout,' ',int8(ndim1),int8(ndim2),j_matdiag)
		ibas=0
		do i=1,ndim1
			if(j_o(irg)%d(ibas+i).eq.j_0)then
				write(6,*)'diagonal element ',i,' is zero'
				j_err=.true.;return
			endif !if(j_o(irg)%d(ibas+i).eq.j_0)  19257
			j_o(iout)%d(ibas+i)=j_1/j_o(irg)%d(ibas+i)
			ibas=ibas+ndim1
		enddo !i=1,ndim1  19256
		return
	endif !if(j_o(irg)%i(4).eq.j_matdiag)  19253
	! call j_getoption(iob,io,j_mcondition,-1,1,j_ipreal,.true.,no,j_optarg0)
	! !!call j_clearoption(iob,io)
	! if(j_err)return
	!if(no.gt.0)ivnorm=j_optarg0(1)
 
 
	allocate(mat(1:ndim1,1:ndim1),rhs_(1:ndim1,1:ndim1),ipiv(1:ndim1))
	iel=0
	rhs_=j_0  !
	do i=1,ndim1
		do j=1,ndim1
			iel=iel+1
			mat(i,j)=j_o(irg)%d(iel)
		enddo !j=1,ndim1  19276
		rhs_(i,i)=1.d0
	enddo !i=1,ndim1  19275
 
	call dgesv(ndim1,ndim1,mat,ndim1,ipiv,rhs_,ndim1,info_)
	if(info_.ne.0)then
		j_err=.true.
		write(6,*)'inverse: singular matrix'
		return
	endif !if(info_.ne.0)  19284
	ivout=j_defmatrix(iout,' ',int8(ndim2),int8(ndim1),j_matreg)
	iel=0
	do i=1,ndim1
		do j=1,ndim1
			iel=iel+1
			j_o(ivout)%d(iel)=rhs_(i,j)
		enddo !j=1,ndim1  19292
	enddo !i=1,ndim1  19291
 
	if(ivout.le.j_named)then
		ivnorm=j_getobject(iout,'%condition',j_ipreal);if(j_err)return
		j_dapu=j_0
		j_dapu2=j_0
		do i=1,ndim1*ndim1
			j_dapu=j_dapu+j_o(ivout)%d(i)**2
			j_dapu2=j_dapu2+j_o(irg)%d(i)**2
		enddo !i=1,ndim1*ndim1  19302
		j_v(ivnorm)=sqrt(j_dapu)*sqrt(j_dapu2)
	endif !if(ivout.le.j_named)  19298
	deallocate(mat,rhs_,ipiv)
 
	return
end subroutine inverse !subroutine inverse(iob,io)
 
subroutine qr(iob,io)
	!Section qr qr() QR decomposition of MATRIX
	! Makes QR decomposition of a MATRIX This can be used to study if columns of a are linearly
	! dependent. Jlp22 prints a matrix which indicates the structure of the upper diagonal matrix R in the
	! qr decomposition. If column k is linearly dependent on previous columns the k’th diagonal
	! element is zero. If output is given, then it will be the r matrix. Due to rounding errors diagonal
	! elements which are interpreted to be zero are not exactly zero. Explicit r matrix is useful if user
	! thinks that Jlp22 has not properly interpreted which diagonal elements are zero.
	!In Jlp22  qr() may be useful when it is studied why a matrix which shoudl
	! be nonsingular turns out to be singular in inverse() or solve().
	!qr() is using  the subroutine dgeqrf from Netlib.
	!An error occurs if the argument is not MATRIX or if dgeqrf produces
	! error code, which is just printed.
	!Now the function just shows the linear dependencies, as sho in the examples.
	!endheader
	!Option
	!Args&1&MATRIX&A m-by-n MATRIX.
	!endoption
	!endsection
 
 
	!output could be defined later
	!Output&0|1&MATRIX&
 
 
	integer,dimension(:), pointer::arg=>null()
	real*8,dimension(:,:),allocatable :: a_
	real*8,dimension(:),allocatable :: tau
	real*8,dimension(:),allocatable :: work
	integer ::lwork
	call j_startfunction(iob,io,0,narg,arg,iout);if(j_err)return
	irg=arg(1)  !o(iob)%i(io+2)
	if(j_otype(irg).ne.j_ipmatrix)then
 
		call j_printname('**argument of qr ',irg,' not a matrix')
		write(6,*)'#type is:',j_otypes( j_otype(irg))
		j_err=.true. ;return
	end if !if(j_otype(irg).ne.j_ipmatrix)  19345
	ndim1=j_o(irg)%i(1);ndim2=j_o(irg)%i(2)
	allocate(a_(1:ndim1,1:ndim2),tau(1:min(ndim1,ndim2)),work(1:ndim2))
	iel=0
	do i=1,ndim1
		do j=1,ndim2
			iel=iel+1
			a_(i,j)=j_o(irg)%d(iel)
		enddo !j=1,ndim2  19355
	enddo !i=1,ndim1  19354
 
	call dgeqrf(ndim1,ndim2,a_,ndim1,tau,work,ndim2,info_)
 
	if(info_.ne.0)then
		write(6,*)'*j* qr failed, info ',info_
		j_err=.true.
		return
	endif !if(info_.ne.0)  19363
	if(iout.gt.0)iout=j_defmatrix(iout,' ',int8(min(ndim1,ndim2)),int8(ndim2),j_matreg)
	write(6,*)'r -matrix' !, * indicates nonzero'
	do i=1,min(ndim1,ndim2)
 
		do j=1,ndim2
			j_tempchar(2*(j-1)+1:2*(j-1)+2)='0 '
		enddo !j=1,ndim2  19372
		do j=i,ndim2
			j_o(iout)%d((i-1)*ndim2+j)=a_(i,j)
			if(abs(a_(i,j)).gt.1.d-10)then
 
				j_tempchar(2*(j-1)+1:2*(j-1)+2)='* '
			endif !if(abs(a_(i,j)).gt.1.d-10)  19377
		enddo !j=i,ndim2  19375
		write(6,*)j_tempchar(1:2*ndim2)
		!write(6,'(9f8.5/)')a_(i,1:ndim2)
	enddo !i=1,min(ndim1,ndim2)  19370
 
	return
 
end subroutine qr !subroutine qr(iob,io)
 
subroutine merge(iob,io)  ! merge()
	logical isilist
	!Section merge merge() Merges LISTs and ILISTs by dropping duplicates.
	! merge() will produce of a LIST consisting of separate objects or values
	! in the arguments.
	!endheader
	!Option
	!
	!endoption
	!Ex mergex Merging list
	!lista=list(x1...x5,x3);
	! ** LISTs must be expanded with @
	!listb=merge(@lista)
	!ilista=ilist(1...5,3);
	!ilistb=merge(ilista,ilist->);
	!*** without ilist the arguments are interpreted as object indices
	!ilistc=merge(ilista);
	!endex
	!endsection
 
 
	!	integer, dimension(:),allocatable::temp
	!	call j_checkoutput(iob,io)
	if(j_err)return
	call  j_startfunction(iob,io,0,narg,j_arg,iout);if(j_err)return
 
	isilist=j_isoption(iob,io,j_milist,.false.)
	nn=0
	do i=1,narg
		j_yes=.false. !is it already
		do j=1,i-1
			if(j_arg(i).eq.j_arg(j))then
				j_yes=.true.
				exit
			endif !if(j_arg(i).eq.j_arg(j))  19421
 
		enddo !j=1,i-1  19420
		if(.not.j_yes)nn=nn+1
 
	end do !i=1,narg  19418
 
	ivout=j_deflist(iout,' ',nres=nn,ilist=isilist)
	nn=0
	do i=1,narg
		j_yes=.false. !is it already
		do j=1,i-1
			if(j_arg(i).eq.j_arg(j))then
				j_yes=.true.
				exit
			endif !if(j_arg(i).eq.j_arg(j))  19436
 
		enddo !j=1,i-1  19435
		if(.not.j_yes)then
			nn=nn+1
			j_o(iout)%i2(nn)=j_arg(i)
 
		endif !if(.not.j_yes)  19442
	end do !i=1,narg  19433
	j_o(iout)%i(1)=nn
	!90	io=io+j_o(iob)%i(io+1)+3
	return
end subroutine merge !subroutine merge(iob,io)
 
 
 
 
 
! subroutine dot(iob,io)  ! dot()
 
! double precision sum
! sum=j_0
! narg=j_o(iob)%i(io+1)
! iout=j_o(iob)%i(io+2+narg)
! if(j_otype(iout).ne.j_ipreal)call j_del(iout)
! ni=narg/2
! do i=1,ni
! sum=sum+j_v(j_o(iob)%i(io+1+i))*j_v(j_o(iob)%i(io+1+i+ni))
! end do !i=1,ni  16689
! j_v(iout)=sum
! !	io=io+narg+3 ;return
! end subroutine dot !subroutine dot(iob,io)
 
! subroutine do(iob,io)   ! do(i,  , ) io?
 
! !	io=io_
! !	io_=io+j_o(iob)%i(io+1)+3
 
! if(j_ndo_loop.ge.j_mxdo)then
! write(*,*)'**too many do -loops, maximum is ',j_mxdo;j_err=.true.;return
! end if !if(j_ndo_loop.ge.j_mxdo)then
! iout=io+j_o(iob)%i(io+1)+2 !the location after end do, in the output position
! j_ndo_loop=j_ndo_loop+1  ! ; write(6,*)'do',(o(iob)%i(io+j),j=0,5)
! j_iido_loop(7,j_ndo_loop)=iob !new
! j_iido_loop(8,j_ndo_loop)=j_v(j_ivrecursion)!
! j_iido_loop(1,j_ndo_loop)=j_o(iob)%i(io+2)  !variable
! j_iido_loop(2,j_ndo_loop)=j_v(j_o(iob)%i(io+3))  !start !write(6,*)'start,iv',o(iob)%i(io+3),
! j_v(j_iido_loop(1,j_ndo_loop))=j_iido_loop(2,j_ndo_loop) !iter=start
! j_iido_loop(3,j_ndo_loop)=j_v(j_o(iob)%i(io+4))  !upper
! if(j_o(iob)%i(io+1).le.3)then
! j_iido_loop(4,j_ndo_loop)=1
! else !if(j_o(iob)%i(io+1).le.3)then
! j_iido_loop(4,j_ndo_loop)=j_v(j_o(iob)%i(io+5)) !step
! end if !if(j_o(iob)%i(io+1).le.3)then
! if(j_iido_loop(4,j_ndo_loop).gt.0)then
! if(j_iido_loop(2,j_ndo_loop).gt.j_iido_loop(3,j_ndo_loop))then
! !write(6,*)'zero count,newio',o(iob)%i(iout)+1,' posend',iout
! io=j_o(iob)%i(iout)+1;j_ndo_loop=j_ndo_loop-1;return
! end if !if(j_iido_loop(2,j_ndo_loop).gt.j_iido_loop(3,j_ndo_loop))then
! else if(j_iido_loop(4,j_ndo_loop).lt.0)then !if(j_iido_loop(4,j_ndo_loop).gt.0)then
! if(j_iido_loop(2,j_ndo_loop).lt.j_iido_loop(3,j_ndo_loop))then
! !write(6,*)'zero count,newio',o(iob)%i(iout)+1, 'posen',iout
! io=j_o(iob)%i(iout)+1;
! j_ndo_loop=j_ndo_loop-1; return
! end if !if(j_iido_loop(2,j_ndo_loop).lt.j_iido_loop(3,j_ndo_loop))then
! else !if(j_iido_loop(4,j_ndo_loop).gt.0)then
! write(6,*)'**step is zero in loop';j_err=.true.;return
! end if !if(j_iido_loop(4,j_ndo_loop).gt.0)then
! j_iido_loop(6,j_ndo_loop)=j_o(iob)%i(iout)  !for exitdo and cycle
! io=io+j_o(iob)%i(io+1)+3  !onko turha
! j_iido_loop(5,j_ndo_loop)=io !place for next after do
! return
! end subroutine do !subroutine do(iob,io)
 
! subroutine enddo(iob,io) !enddo
 
! j_iido_loop(2,j_ndo_loop)=j_iido_loop(2,j_ndo_loop)+j_iido_loop(4,j_ndo_loop)
! if(j_iido_loop(4,j_ndo_loop).gt.0)then
! if(j_iido_loop(2,j_ndo_loop).gt.j_iido_loop(3,j_ndo_loop))then
! io=io+1;j_ndo_loop=j_ndo_loop-1;return
! else !if(j_iido_loop(2,j_ndo_loop).gt.j_iido_loop(3,j_ndo_loop))then
! io=j_iido_loop(5,j_ndo_loop)
! end if !if(j_iido_loop(2,j_ndo_loop).gt.j_iido_loop(3,j_ndo_loop))then
! else !if(j_iido_loop(4,j_ndo_loop).gt.0)then
! if(j_iido_loop(2,j_ndo_loop).lt.j_iido_loop(3,j_ndo_loop))then
! io=io+1;j_ndo_loop=j_ndo_loop-1;return
! else !if(j_iido_loop(2,j_ndo_loop).lt.j_iido_loop(3,j_ndo_loop))then
! io=j_iido_loop(5,j_ndo_loop)
! end if !if(j_iido_loop(2,j_ndo_loop).lt.j_iido_loop(3,j_ndo_loop))then
! end if !if(j_iido_loop(4,j_ndo_loop).gt.0)then
! j_v(j_iido_loop(1,j_ndo_loop))=j_iido_loop(2,j_ndo_loop)
! return
! end subroutine enddo !subroutine enddo(iob,io)
 
! subroutine exitdo(iob,io)  ! exitdo in do-loop
 
! io=j_iido_loop(6,j_ndo_loop)+1
! j_ndo_loop=j_ndo_loop-1
! return
! end subroutine exitdo !subroutine exitdo(iob,io)
 
! subroutine cycledo(iob,io)  !cycle in do loop
 
! io=j_iido_loop(6,j_ndo_loop)
! return
! end subroutine cycledo !subroutine cycledo(iob,io)
 
subroutine npv(iob,io)  ! npv()
	! Section npv npv() Net present value
 
	! npv(]interest,income1,…,incomen,time1,…,timen[)//
	! Returns net present value for income sequence income1,...,incomen, occurring at times
	! time1,…,timen when the interest percentage is ]interest[.
	! endsection
 
	double precision pv
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	r1=1./( 1. + 0.01*j_v( j_o(iob)%i(io+2)) )
	ni=(narg-1)/2 !how many income,time pairs
	pv=j_0
	do  i=io+3,io+2+ni
		pv=pv+j_v( j_o(iob)%i(i)) * r1**j_v( j_o(iob)%i(i+ni) ) !do 11 i=io+3,io+2+ni
	enddo ! i=io+3,io+2+ni  19562
	j_v(iout)=pv
	!		io=io+narg+3
	return
end subroutine npv !subroutine npv(iob,io)
 
subroutine print(iob,io)  !print-function  %%io  print()
 
	! Section print print() Prints objects to file or console
	! print(arg1,…,argn[,maxlines->][,data->][,row->]
	! [,file->][,func->][,debug->])//
	! Print values of variables or information about objects.//
	! Arguments:
	! arg1,…,argn
	! Options:
	!from&0|1&REAL& this option tells that the argumets give the object indices, not their names.
	! this is useful when debuging problems realted to temporary objects.
	! maxlines&-1|1&REAL& the maximum number of lines printed for matrices, default 100.
	! any& -1|0 && if the argument is the name of a text file, then any-> indicates that
	! the file is read to the end and the number  of lines is put into variable Accepted.
	! data&-1|1 &DATA & data sets. If data-> option is given then arguments must be ordinary real
	! variables obtained from data.
	! row if a text object is printed, then the first value given in the row-> option gives the
	! first line to be printed. If a range of lines is printed, then the second argument
	! must be the negative of the last line to be printed (e.q. row->(10,-15)).Note
	! that nrows() function can be used to get the number of rows.
	! file the file name as a character variable or a character constant. Redirects the output
	! of the print() function to given file. After printing to the file, the file remains
	! open and must be explicitly closed (close(‘file’)) if it should be opened in
	! a different application.
	! form when a matrix is printed, the format for a row can be given as a Fortran format,
	! e.g. form ‘(15f6.2)’ may be useful when printing a correlation matrix.
	! debug the associated real variable part is first printed, and thereafter the tow associated
	! two integer vectors, the real vector and the double precision vector
	! func all functions available are printed
	! endheader
	! Note  For simple objects, all the object content is printed, for complicates objects only
	! summary information is printed. print(Names) will list the names, types and sizes of all
	! named Jlp22 objects. The printing format is dependent on the object type.
	!endnote
	! Note print() function can be executed for the output of a Jlp22 command
	! by writing ';' or ';;' at the end of the line. The excution of implied print()
	! is dependent on the value of ]printresult[. If ]printresult[ =0,
	! then the output is not printed, If ]printresult[ =1, then ';' is
	! causing printing, if ]printresult[ =2 then only ';;'-outputs are
	! printed, and if ]printresult[ =3, then both ';' and ';;' outputs are printed.
	! endnote
	!Note it was earlier to print a file using this function, but now function Print_f must be
	!used fro taht purpose.
	!endnote
 
 
	! endsection
	! file->filename  writen to file
	!print(cgar,file->) prints teh content of file
	!print(cahrvar) print the cahr
 
	character*80 name
	logical bin_,isrow
	logical debug,isopen,isappend,isfile,isobject,isany
	character*16 funcsb
	character ::ch
	integer,dimension(:),pointer::arg,file
	!	io=io_
	call j_startfunction(iob,io,0,narg,arg,ivout);if(j_err)return
 
	j_yes=j_isoption(iob,io,j_mfrom)
	if(j_yes)arg(1:narg)=j_v(arg(1:narg))
 
	if(narg.eq.1)then
		if(j_otype(arg(1)).eq.j_iplist)then
 
 
			if(j_linkoption(iob,io,j_mexpand,clear=.true.).ge.0)then
				narg=j_o(arg(1))%i(1)
				arg=>j_o(arg(1))%i2(1:narg)
 
			endif !if(j_linkoption(iob,io,j_mexpand,clear=.true.).ge.0)  19637
 
		endif !if(j_otype(arg(1)).eq.j_iplist)  19634
	endif !if(narg.eq.1)  19633
 
 
	if(j_linkoption(iob,io,j_moptions,clear=.true.).ge.0)then
		call j_printoptions()
		return
 
	endif !if(j_linkoption(iob,io,j_moptions,clear=.true.).ge.0)  19647
 
 
	isappend=j_linkoption(iob,io,j_mappend).ge.0
	call j_getoption(iob,io,j_mform,-1,100,j_ipchar,.true.,noptargf,j_optarg0);if(j_err)return
 
 
	if(noptargf>0.and.j_optarg0(1).ne.j_ivdollar)then  !format
		call j_getchar(j_optarg0(1),j_form,lef);if(j_err)return
	endif !if(noptargf>0.and.j_optarg0(1).ne.j_ivdollar)  19658
	call j_getoption(iob,io,j_mfile,-1,100,j_ipchar,.true.,nfile,file);if(j_err)return
	!call j_getchar(ivform,j_f orm_,lef)
	isany=j_isoption(iob,io,j_many)
	isrow=j_linkoption(iob,io,j_mrow).ge.0
 
	!	write(6,*)'j_mfunctions',j_mfunctions,j_linkoption(iob,io,j_mfunctions)
	if(j_linkoption(iob,io,j_mfunctions).ge.0)then   !print available functions
		icur=0
		j_cline=' '
		write(6,*)j_title,' -functions ', j_nfunctions
		write(6,*)' '
		write(6,*)'function  # minarg maxarg function     # minarg maxxarg    function # minarg maxarg'
		write(6,*)' '
 
		do i=1,j_nfunctions
 
			j_cline(icur*26+1:icur*26+20)=j_function_name(i,16)
			!"		write(6,*)'icur ',icur*26+1+16,icur*26+1+25
			write(j_cline(icur*26+1+16:icur*26+1+25),'(i3,i2,i4)')i,j_minarg(i),min(j_maxarg(i),999)
			icur=icur+1
			if(icur.eq.3.or.i.eq.j_nfunctions_.or.i.eq.j_nfuncs1.or. &
					i.eq.j_nfuncs2.or.i.eq.j_nfunctions)then
				write(6,'(a)')j_cline(1:79)
				j_cline=' '
				icur=0
			endif !if(icur.eq.3.or.i.eq.j_nfunctions_.or.i.eq.j_nfuncs1.o  19681
			select case(i)
			case(j_nfunctions_) !select case(i)
			write(6,*)' '
			write(6,*)o1_title,' -functions ', o1_nfunctions
			case (j_nfuncs1) !select case(i)
			write(6,*)' '
			write(6,*)o2_title,' -functions', o2_nfunctions
			case (j_nfuncs2) !select case(i)
			write(6,*)' '
			write(6,*)o3_title,' functions',  o3_nfunctions
 
			end select !select case(i)
		enddo !i=1,j_nfunctions  19675
		!!call j_clearoption(iob,io)  ! subroutine
		!goto 900
	endif !if(j_linkoption(iob,io,j_mfunctions).ge.0)  19667
	debug=j_linkoption(iob,io,j_mdebug).ge.0
 
 
 
 
	bin_ = .false.
	nu_ = 6
	if(nfile>0) then
		if (nfile==1) then
			!	write(6,*)'heelurei'
			!ivfile_ = j_o(iob)%i(j_linkoption(iob,io,j_mfile)+1)
			if(j_otype(file(1)).eq.j_ipchar)then
				nu_=j_iounit(file(1)) !j_o(ivfile_)%i(4)
 
				if(nu_.le.0)then
					! subroutine j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout)
					!			write(6,')'<448>',isappend,
					if(isappend)then
						call j_getfile(nu_,'a',file(1))
					else !if(isappend)then
						call j_getfile(nu_,'w',file(1))
					endif !if(isappend)  19720
					!call j_getwritefile(ivfile_,j_ivdollar,bin_)
					if(j_err)goto 900
					!	nu_=j_iounit(file(1)) !j_o(ivfile_)%i(4)
				endif !if(nu_.le.0)  19717
			else !if(j_otype(file(1)).eq.j_ipchar)then
				! virheellinen option arvo
				write(6,*)'**illegal file-> option';j_err=.true.;goto 900
			endif !if(j_otype(file(1)).eq.j_ipchar)  19714
		else !if (nfile==1) then
			write(6,*)'**illegal number of arguments in file-> option';j_err=.true.;goto 900
		endif !if (nfile==1)  19711
	endif !if(nfile>0)  19710
	call j_getoption(iob,io,j_mmaxlines,-1,1,j_ipreal,.true.,nmax,j_optarg0)
	if(j_err)return
	if(nmax.gt.0)then
		maxlines=j_v(j_optarg0(1))
	else !if(j_linkoption(iob,io,j_mmaxlines).gt.0)then
		maxlines=j_v(j_ivmaxlines)
	end if !if(nmax.gt.0)  19739
	lirow=j_linkoption(iob,io,j_mrow)
	linkoptmdata=j_linkoption(iob,io,j_mdata)
 
 
	!!call j_clearoption(iob,io)  ! subroutine
	!	write(6,*)'narg',narg,arg(1),nfile,j_otype(arg(1))
 
 
	if(.not.debug)then
		if(j_printvar(nu_,narg,arg))return
	endif !if(.not.debug)  19752
	!	write(6,*)'<73737narg',narg
	!	write(6,*)'debug',debug
	do j=1,narg  ! o(iob)%i(io+1)
		!iv=j_o(iob)%i(io+1+j)
 
		iv=arg(j)
		!	write(6,*)'<73737narg',iv,j_otype(iv)
		call j_getname(iv)
		!	write(6,*)'<555>',iv,j_otype(iv),j_ipilist,j_oname(1:j_loname)
		if(debug) then
 
			write(nu_,*)'object index ',iv,' v-part: ',j_v(iv)
			write(nu_,*)' object type:',j_otype(iv), j_otypes(j_otype(iv))
			if(allocated(j_o(iv)%i)) write(nu_,*)'i: size',size(j_o(iv)%i), ' elements (max 100) ',&
				j_o(iv)%i(1:min(100,size(j_o(iv)%i)))
			if(allocated(j_o(iv)%i2)) write(nu_,*)'i2: size',size(j_o(iv)%i2),' : ',&
				j_o(iv)%i2(1:min(100,size(j_o(iv)%i2)))
			if(allocated(j_o(iv)%r)) write(nu_,*)'r: size',size(j_o(iv)%r),' : ',&
				j_o(iv)%r(1:min(100,size(j_o(iv)%r)))
			if(allocated(j_o(iv)%d)) write(nu_,*)'d: size',size(j_o(iv)%d),' : ', &
				j_o(iv)%d(1:min(100,size(j_o(iv)%d)))
			if(allocated(j_o(iv)%txt))then
				write(6,*)'txt part ',j_o(iv)%i(1),' lines'
				do i=1,j_o(iv)%i(1)
					write(6,*)j_o(iv)%i2(i),j_o(iv)%txt(i)
				enddo !i=1,j_o(iv)%i(1)  19778
 
			endif !if(allocated(j_o(iv)%txt))  19776
			cycle
		endif !if(debug)  19764
 
 
 
		!	if(iv.le.j_nv)then
		!	iotype=j_otype(iv)
 
		!	end if !if(iv.le.j_nv)  17250
		if(j_otype(iv).gt.j_ipreal)then  !vois muuttaa otypet
			if(j_otype(iv).eq.j_iptxt)then
				write(nu_,*)j_oname(1:j_loname),' is TXT with ',j_o(iv)%i(5), 'lines'
				do i=1,j_o(iv)%i(5)
					let=len_trim(j_o(iv)%txt(i))
					write(nu_,*)j_o(iv)%txt(i)(1:let)
 
				end do !i=1,j_o(iv)%i(5)  19795
				write(6,*)'// end of TXT'
 
			elseif(j_otype(iv).eq.j_ipmatrix)then
				write(nu_,*)' '
				iba=0
 
				!	else !if(iotype.eq.j_ipmatrix0)then
				if(j_o(iv)%i(4).eq.j_mati)then
					write(nu_,*)j_oname(1:j_loname),' is I-MATRIX(',j_o(iv)%i(1),',',j_o(iv)%i(2),' )'
 
					!	write(nu_,'(30i5/)')j_o(iob)%i(io:io+59)
					!	write(nu_,'(30i5/)')(jj,jj=io,io+59)
					do i=1,min(maxlines,j_o(iv)%i(1))
						if(noptargf.gt.0)then
							if(isrow)then
								write(nu_,j_form(1:lef),err=950)i,(j_o(iv)%i2(iba+k),k=1,j_o(iv)%i(2) )
							else !if(isrow)then
								write(nu_,j_form(1:lef),err=950)(j_o(iv)%i2(iba+k),k=1,j_o(iv)%i(2) )
 
							endif !if(isrow)  19814
						else !if(noptargf.gt.0)then
							if(isrow)then
								write(nu_,'(i5,1x,10(g14.7,1x))')i,(j_o(iv)%i2(iba+k),k=1,j_o(iv)%i(2) )
							else !if(isrow)then
								write(nu_,'(10(g14.7,1x))')(j_o(iv)%i2(iba+k),k=1,j_o(iv)%i(2) )
							endif !if(isrow)  19821
						endif !if(noptargf.gt.0)  19813
						iba=iba+j_o(iv)%i(2)
					enddo !i=1,min(maxlines,j_o(iv)%i(1))  19812
 
				else
					write(nu_,*)j_oname(1:j_loname),' is MATRIX(',j_o(iv)%i(1),',',j_o(iv)%i(2),' )'
 
					!	write(nu_,'(30i5/)')j_o(iob)%i(io:io+59)
					!	write(nu_,'(30i5/)')(jj,jj=io,io+59)
					iba=0
					do i=1,min(maxlines,j_o(iv)%i(1))
						if(noptargf.gt.0)then
							if(isrow)then
								write(nu_,j_form(1:lef),err=950)i,(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
							else !if(isrow)then
								write(nu_,j_form(1:lef),err=950)(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
 
							endif !if(isrow)  19838
						else !if(noptargf.gt.0)then
							if(isrow)then
								write(nu_,'(i5,1x,10(g14.7,1x))')i,(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
							else !if(isrow)then
								write(nu_,'(10(g14.7,1x))')(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
							endif !if(isrow)  19845
						endif !if(noptargf.gt.0)  19837
						iba=iba+j_o(iv)%i(2)
 
					end do !i=1,min(maxlines,j_o(iv)%i(1))  19836
 
				endif !if(j_o(iv)%i(4).eq.j_mati)  19807
 
				if(maxlines.lt.j_o(iv)%i(1))write(nu_,*)'*printing limited by maxlines ',maxlines
			elseif(j_otype(iv).eq.j_ipmatrixs)then
				write(nu_,*)' '
				iba=0
 
				!	else !if(iotype.eq.j_ipmatrix0)then
				write(nu_,*)j_oname(1:j_loname),' is single precision MATRIXS (',j_o(iv)%i(1),',',j_o(iv)%i(2),' )'
 
				!	write(nu_,'(30i5/)')j_o(iob)%i(io:io+59)
				!	write(nu_,'(30i5/)')(jj,jj=io,io+59)
				do i=1,min(maxlines,j_o(iv)%i(1))
					if(noptargf.gt.0)then
						if(isrow)then
							write(nu_,j_form(1:lef),err=950)i,(j_o(iv)%r(iba+k),k=1,j_o(iv)%i(2) )
						else !if(isrow)then
							write(nu_,j_form(1:lef),err=950)(j_o(iv)%r(iba+k),k=1,j_o(iv)%i(2) )
 
						endif !if(isrow)  19869
					else !if(noptargf.gt.0)then
						if(isrow)then
							write(nu_,'(i5,1x,10(g14.7,1x))')i,(j_o(iv)%r(iba+k),k=1,j_o(iv)%i(2) )
						else !if(isrow)then
							write(nu_,'(10(g14.7,1x))')(j_o(iv)%r(iba+k),k=1,j_o(iv)%i(2) )
						endif !if(isrow)  19876
					endif !if(noptargf.gt.0)  19868
					iba=iba+j_o(iv)%i(2)
 
				end do !i=1,min(maxlines,j_o(iv)%i(1))  19867
 
				if(maxlines.lt.j_o(iv)%i(1))write(nu_,*)'*printing limited by maxlines'
			else if(j_otype(iv).eq.j_iptext)then !if(iotype.eq.j_ipmatrix)then
				if(nu_.eq.6)then
					write(6,*)' '
					write(nu_,*)j_oname(1:j_loname),' is text object:'
				endif !if(nu_.eq.6)  19888
				if(iv.eq.j_ivnames)then
					do i=1,j_named
						call j_getline(j_ivnames,i,name,le)
						if(j_err)return
						if(le.lt.34)name(le+1:34)=' '
						isiz=0
						if(allocated(j_o(i)%i))isiz=isiz+size(j_o(i)%i)
						if(allocated(j_o(i)%i2))isiz=isiz+size(j_o(i)%i2)
						if(allocated(j_o(i)%r))isiz=isiz+size(j_o(i)%r)
						if(allocated(j_o(i)%ch))isiz=isiz+size(j_o(i)%ch)/4
						if(allocated(j_o(i)%d))isiz=isiz+2*size(j_o(i)%d)
						write(nu_,*)int2(i),' ',name(1:max(34,le)),' type= ',j_otypes(j_otype(i))(1:8),' size=',isiz
					end do !i=1,j_named  19893
				else !if(iv.eq.j_ivnames)then
					if(maxlines.gt.j_o(iv)%i(0).and.lirow.le.0)then
						!				WRITE(6,*)'<J44NU,ITE,0,.TRUE',nu_,iv,0
						if(nu_.eq.6)then
							call j_writetext(nu_,iv,0)
							write(nu_,*)'///end of text object '
						else
							call j_writetext(nu_,iv,0,.true.)
						endif !if(nu_.eq.6)  19908
					else !if(maxlines.gt.j_o(iv)%i(0).and.lirow.le.0)then
						if(lirow.gt.0)then
							ifi=j_v(j_o(iob)%i(lirow+1))
							if(ifi.gt.j_o(iv)%i(0))then
								call j_printname('**text object ',iv, ' does not have that many rows')
								write(6,*)'it has ',j_o(iv)%i(0), ' rows ', 'you asked to print row ',ifi
								j_err=.true. ;return
							endif !if(ifi.gt.j_o(iv)%i(0))  19917
							if(j_o(iob)%i(lirow).ge.2)then
								las=-j_v(j_o(iob)%i(lirow+2))
								if(las.lt.ifi)then
									write(nu_,*)'**print: the range must be given in form row->(5,-7)'
									j_err=.true. ;return
								endif !if(las.lt.ifi)  19924
								las=min(las,j_o(iv)%i(0))
							else !if(j_o(iob)%i(lirow).ge.2)then
								las=ifi  ! min(o(iv)%i(0),ifi+maxlines)
							endif !if(j_o(iob)%i(lirow).ge.2)  19922
						else !if(lirow.gt.0)then
							ifi=1
							las=min(maxlines,j_o(iv)%i(0))
						endif !if(lirow.gt.0)  19915
						if(nu_.eq.6)then
							do i=ifi,las
								!		WRITE(6,*)'<J991NU,ITE,0,.TRUE',nu_,iv,i,ite
								call j_writetext(nu_,iv,i)
							enddo !i=ifi,las  19937
							write(nu_,*)'*lines ',ifi,'-',las,' printed out of ', j_o(iv)%i(0)
						else
							do i=ifi,las
								!		WRITE(6,*)'<J991NU,ITE,0,.TRUE',nu_,iv,i,ite
								call j_writetext(nu_,iv,i,.true.)
							enddo !i=ifi,las  19943
 
						endif !if(nu_.eq.6)  19936
					endif !if(maxlines.gt.j_o(iv)%i(0).and.lirow.le.0)  19906
				end if !if(iv.eq.j_ivnames)  19892
			elseif(j_otype(iv).eq.j_ipstemspline)then !if(iotype.eq.j_ipmatrix)then
				npo=j_o(iv)%i(1);npo2=j_o(iv)%i(2)
				write(nu_,*) j_oname(1:j_loname),' is a stemspline with ',npo2, ' points'
				write(nu_,*)'heights:',0.01*j_o(iv)%r(npo+1:npo+npo2)
				write(nu_,*)'diam.:  ',j_o(iv)%r(1:npo2)
				if(j_o(iv)%i(3).gt.0)call j_printname('generated from stecurves ',j_o(iv)%i(3),' ')
			else if(j_otype(iv).eq.j_ipsmooth)then !if(iotype.eq.j_ipmatrix)then
				write(nu_,*) j_oname(1:j_loname),' is a smoothing spline'
			else if(j_otype(iv).eq.j_ipbitmatrix)then !if(iotype.eq.j_ipmatrix)then
				write(nu_,*) j_oname(1:j_loname),' is ',j_o(iv)%i(1),' x (', j_o(iv)%i(3),':', j_o(iv)%i(2),') bitmatrix: '
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
						end if !if(j_ibittest(iv,jj,kk).ne.0)  19967
					end do !kk=j_o(iv)%i(3),min(j_o(iv)%i(2),79+j_o(iv)%i(3))  19965
					write(nu_,'(a)')j_cline(1:ic)
				enddo !jj=1,min(j_o(iv)%i(1),maxlines)  19963
			else if(j_otype(iv).eq.j_iplist)then !if(iotype.eq.j_ipmatrix)then
				call j_printlist(nu_,iv)
 
			elseif(j_otype(iv).eq.j_iptable)then
				ivrow=j_o(iv)%i(4)   !j_gettablerow(iv)
				ivcol=j_o(iv)%i(5)   !j_gettablecol(iv)
				if(j_err)return
				call j_getname(iv,ivrow,ivcol)
				write(nu_,*)j_oname(1:j_loname),' is TABLE of row LIST ',j_oname2(1:j_loname2), ' with ',j_o(ivrow)%i(1),&
					' elements and column LIST ',j_oname3(1:j_loname3),' with ',j_o(ivcol)%i(1),' elements'
 
			elseif(j_otype(iv).eq.j_ipilist)then !if(iotype.eq.j_ipmatrix)then
				write(6,*)' '
				if(j_o(iv)%i(2).ne.1)then
					ncol=j_o(iv)%i(2)
					nrow=j_o(iv)%i(3)/ncol
					write(6,*)j_oname(1:j_loname),' is ',nrow,'*',ncol,' ILIST with ',&
						'allocated size ', j_o(iv)%i(4)
					iba=0
					do ir=1,nrow
						write(6,'(20i7/)')j_o(iv)%i2(iba+1:iba+ncol)
						iba=iba+ncol
					enddo !ir=1,nrow  19994
				else
					write(6,*)j_oname(1:j_loname),' is ILIST with ', j_o(iv)%i(1),&
						' elements,and allocated size ', j_o(iv)%i(4)
					write(6,'(20i7/)')j_o(iv)%i2(1:j_o(iv)%i(1))
				endif !if(j_o(iv)%i(2).ne.1)  19988
			else if(j_otype(iv).eq.j_ipfigure)then !if(iotype.eq.j_ipmatrix)then
				!	j_o(iv)%i(2)=j_gplines
				!j_o(ivfig)%i(4)=j_gpplots
				write(6,*)' '
				write(nu_,*)j_oname(1:j_loname),' is a figure object '
				call j_figdump(iv)
			else if(j_otype(iv).eq.j_ipregr)then !if(iotype.eq.j_ipmatrix)then
				write(nu_,*)j_oname(1:j_loname),' is a regression object with ',j_o(iv)%i(0), ' indep. variables '
			elseif(j_otype(iv).eq.j_ipproblem)then !if(iotype.eq.j_ipmatrix)then
				call j_getname(iv)
				write(6,*)j_oname(1:j_loname),' is PROBLEM'
				!	call j_getname( j_o(iv)%i(3),j_o(iv)%i(3)
				! write(6,*)'you can see subobjects of ', j_oname(1:j_loname),' with ',j_oname(1:j_loname)//'%?;'
				! write(6,*)' and contents of  subobjects with ', '@'//j_oname(1:j_loname)//'%?;'
				! write(6,*)' giving Maxlines=20 you can limit the number of printed lines'
				! write(6,*)' the subobjects can be put into a list using  plist=list(//',j_oname(1:j_loname)//'%?'
				! write(6,*)' putting ; to the end of above, the list is printed immediately'
				! write(6,*)' the contens can then be printed with @plist;'
 
 
			else if(j_otype(iv).eq.j_ipdata)then !if(iotype.eq.j_ipmatrix)then
				ivmat=j_getmatrix(iv);if(j_err)return  !j_o(iv)%i(1)
				write(nu_,*)' '
				write(nu_,*)j_oname(1:j_loname),' is data object with ',j_o(ivmat)%i(1), 'obs and ',j_o(ivmat)%i(2),&
					'keep vars'
				write(nu_,*)'  data matrix: ', j_vname(ivmat)
				!  write(6,*) o(ivmat)%i(1), ' obs', o(ivmat)%i(2), ' vars'
				write(nu_,*)'  keep-list: ', j_vname(j_o(iv)%i(2))
				call j_printlist(nu_,j_o(iv)%i(2))
				!	write(nu_,*)'vars-list: ', j_vname(j_o(iv)%i(8))
				write(nu_,*)'  obs-variable: ', j_vname(j_o(iv)%i(6))
				!	if(j_o(iv)%i(6).ne.0)write(nu_,*)'trans: ', j_vname(j_o(iv)%i(6))
				if(j_o(iv)%i(3).ne.0)then
					write(nu_,*)'  sub-data: ', j_vname(j_o(iv)%i(3))
					write(nu_,*)'  nobsw-variable: ', j_vname(j_o(iv)%i(4))
				end if !if(j_o(iv)%i(3).ne.0)  20035
				if(j_o(iv)%i(5).ne.0)then
					write(nu_,*)'  up-data: ', j_vname(j_o(iv)%i(5))
					write(nu_,*)'obsw-variable: ', j_vname(j_o(iv)%i(7))
				end if !if(j_o(iv)%i(5).ne.0)  20039
			else if(j_otype(iv).eq.j_ipchar)then !if(iotype.eq.j_ipmatrix)then
				call j_getname(iv)
				write(nu_,*)j_oname(1:j_loname),'=',j_o(iv)%txt(1: j_o(iv)%i(1))
				!write(nu_,*)(j_o(j_ivnames)%ch(jj),jj=j_o(iv)%i(1)-1,j_o(iv)%i(2)+1)
			else if(j_ipc(iv).ne.0)then !if(iotype.eq.j_ipmatrix)then
				call j_getchar(iv,name(le+2:),le2);if(j_err)return
				write(nu_,*)j_oname(1:j_loname),"='",name(le+2:le+le2+1),"'"
			end if !if(j_otype(iv).eq.j_iptxt)  19793
		else !if(iotype.gt.j_ipreal)then
			if(j_otype(iv).eq.j_ipreal)then
				call j_getname(iv)
				write(nu_,*)' '
				write(nu_,*)j_oname(1:j_loname),'=',j_v(arg(j))
			end if !if(j_otype(iv).eq.j_ipreal)  20052
		end if !if(j_otype(iv).gt.j_ipreal)  19792
	end do !j=1,narg  19757
	900 continue ! io=io+narg+3
	!write(6,*)'kui'
	!!call j_clearoption(iob,io)
	!	write(6,*)'hip '
	return
	950 write(6,*)'*error in format'
	j_err=.true.
	return
end subroutine print !subroutine print(iob,io)
 
! subroutine getlist(iob,io)  ! list(iel) e.g. out=a(irow,icol)  %%matrix
! iout=j_o(iob)%i(io+4)
! !	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
! !write(6,*)'ioalus ',io,'i:',j_o(iob)%i(1:j_o(iob)%i(0))
! imat=j_o(iob)%i(io+1)
! !	call j_printname('imat',imat,' ')
! if(j_otype(imat).ne.j_iplist)then
 
! call j_printname('**not a legal list: ',imat,' ')
! j_err=.true. !;return
! return
! end if !if(j_otype(imat).ne.j_iplist)then
! idim1=j_o(iob)%i(io+2)
! inde1=j_v(idim1)  !row
! !	write(6,*)'idim,inde',idim1,inde1
! if(inde1.lt.0.or.inde1.gt.j_o(imat)%i(0))then
! call j_printname('illegal element of  ',imat,':')
! write(6,*)inde1
! return
! endif !if(inde1.lt.0.or.inde1.gt.j_o(imat)%i(0))then
 
! if(iout.le.j_named.and.iout.gt.0)then
! call j_copy2(j_o(imat)%i(inde1),iout)
! !	write(6,*)'out:',j_v(iout)
! else !if(iout.le.j_named.and.iout.gt.0)then
! loc=j_o(iob)%i(io+3)
! j_o(iob)%i(loc)=j_o(imat)%i(inde1)
! !		write(6,*)'loc,iut ',loc,j_o(imat)%i(inde1)
! !	write(6,*)'iolopus ',io,'i:',j_o(iob)%i(1:j_o(iob)%i(0))
! endif !if(iout.le.j_named.and.iout.gt.0)then
! !	loc=j_o(iob)%i(io+3)
! !	write(6,*)'loc ',loc
! !	j_o(iob)%i(loc)=j_o(imat)%i(inde1)
! !write(6,*)'getlist,uusio ',io+5
! !90 io=io+5
! return
! end subroutine getlist !subroutine getlist(iob,io)
 
 
 
 
 
subroutine asschar(iob,io) ! %%char assign character variable
 
	! =in out
	iout=j_o(iob)%i(io+2)
	in=j_o(iob)%i(io+1)
	if(j_otype(iout).ne.j_ipchar)then
 
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		!link to first char, linkt to last char, link to charconst, unit in files
		allocate( j_o(iout)%i(1:4))   ! 4))
		j_otype(iout)=j_ipchar
	endif !if(j_otype(iout).ne.j_ipchar)  20116
 
	j_o(iout)%i(1:2)=j_o(in)%i(1:2)
	j_o(iout)%i(3)=in !  ; o(in)%i(3)=iout
	j_o(iout)%i(4)=0
 
	!	io=io+3
	return
end subroutine asschar !subroutine asschar(iob,io)
 
 
subroutine data(iob,io)  !data(
 
	!Section datahead  DATA functions
	! Data can be analyzed and processed either using matrix computations
	! or using DATAs. A DATA is compound object
	!linked to a data MATRIX and LIST containing variable (column) names,
	! some other information.
	! When data ere used via DATA in statistical or linear programming
	! functions, the data are processed observation by observation.
	! It is possible to work using DATA or using directly the data matrix, whatever is more
	!convenient. It is possible make new data objects or new matrices
	!by to extracting  columns of data matrix, computing matrices with matrix computations.
	!It is possible to use data in hierarcical way, This property is inherited from JLP.
	! There are two Jlp22 functions which create DATAs from files, data() and
	! exceldta(). data() can create hierarchical data objects. Function newdata() creates a DATA from matrices, which temselfs can be
	! picked from data objects. Function linkdata() can link two data sets to make a hierarchical data.
	!endheader
	!Note If a data file contains columns which are referred with variable names and some vectors,
	! the it is practical to read data first into a matrix using matrix() function and then
	! use matrix operations and newdata() to make DATA with variable names and matrices.
 
	!endnote
	!Note transdata() function goes through a DATA similarly as statitical functions, but
	! does not serve a specific purpose, just transformations defined in the TRANS object refreed with
	! trans-> option are computed.
	!endnote
	!Note In earlier versions it was possible to give several data sets as arguments for data-> option.
	! This festure is now deleted as it is possible to stack several data matrices and then use newdata() function to create a single data set.
	!endnote
	!endsection
 
 
	!Section data data() Making a DATA
	! Data objects can be created with the data() function.
	! A data object can created by a data() function when data are read from a file
	!or from the following input paragraph. New data objects can
	!be created with newdata() function from previous data objects and/or matrices.
	!A data object can created using a TRANS object
	! by creating first data matrix with matrix() and then using newdata() to
	! create data object. In the new version data values can be obtained from a variable whose name is given or
	! using computation rule within parenthesis. If the data is given in in-> file, there must be
	! code-> option to indicate that interpretation is needed for getting values. If there is sparse-> or case->
	! then code-> is not needed for a file input either.
	!endheader
	!Option
	! Output &1&DATA&
	! Data object to be created.
	! read &0|1-&REAL|List& Variables read from the input files. If the first variable is %nobsw its
	! name is changed into Output%nobsw which is assumed in hierarchical data.
	! If no arguments are given
	! then the variables to be read in are stored in the first line of the data file separated
	! with commas. Also the … -shortcut can be used to define the variable list.
	! in &0-&Char&input file or list of input files. If no files are given, data is read from the following input
	! paragraph.
	! form &-1|1&Char& Format of the data as follows \newline
	! $  Fortran format '*', the default \newline
	! b    Single precison binary \newline
	! B  Double precison binary.\newline
	! Char giving a Fortran format, e.g. '(4f4.1,1x,f4.3)' \newline
	!case &N|0|1&REAL& The corresponding column in the data gives names of variables used for the cases.
	!	The values for the variables will be the case number. If the case variable is Case, the LIST output%Case
	! will contain all the case variables. The Case variable will be in the data, and will have values 1,2,..etc.
	! maketrans&-1|1&TRANS &Transformations computed for each observation when reading the data
	! keep&-1|1-&REAL& variables kept in the data object, default: all read-> variables plus the output
	! variables of maketrans-> transformations. If tr is the maketrans-> object,
	! then keep->(x,x4,@tr%output) can be used to pick part of read-> variables and
	! all output variables of tr. keep-> can be used to put into data matrix columns which
	! are utilized later. If there the is read->@readv, then keep->(@readv,Regf,Resid)
	! can be used to put Regf and Resid to the data, and these are the utilisez in regressions.
	! filter&-1|1&Code& logical or arithmetic statement (nonzero value indicating True) describing which
	! observations will be accepted to the data object. maketrans->-transformations are
	! computed before using filter. Option filter-> can utilize automatically created
	! variable Record which tells which input record has been just read. If observations
	! are rejected, then the Obs-variable has as its value number of already accepted
	! observations+1.
	!sparse &N|0&& In the input there can be for each observation after variables given in read-> any
	! number of variable,value -pairs. The variables are collected from these line endings and they need
	! not to be specified otherwise. The value part can also be a variable name or it can be obtained with transformations.
	!Also the variables in the read-> part can be given with variables names or computaion rules withi parenthesis.
	! reject &-1|1&Code& Logical or arithmetic statement (nonzero value indicating True) describing which
	! observations will be rejected from the data object. If filter-> option is given then
	! reject statement is checked for observations which have passed the filter. Option
	! reject-> can utilize automatically created variable Record which tells which
	! input record has been just read. If observations are rejected, then the Obsvariable has as its value number of already accepted observations+1.
 
	! keepopen &-1:1&REAL& This option tells that if nobs-> option gives the number of observations
	!then after the current DATA is read in, the file remains open so that other DATAs can be read from the same file.
	!This option is useful when storing schedules having the tree structure and unpacking the structure
	! with joindata(). See also splitdata().
	! continue  &-1:1&REAL& This tells that the reading continues from
	! the file which was left open with the previous keepopen-> option.
 
	!up&-1|1& DATA& Gives the upper level data to which the data is linked. The first keep
	!variable must be updata%nobsw, which tells the number of children observations for
	! each up data observation. If there is up data then the variable Data%obsw will give
	!the number of observation within the upper level observation. The up data
	! is automatically linked to the DATA created.
 
	! duplicate&-1|2& TRANS &
	! The two TRANS object arguments describe how observations
	! will be duplicated. This can be used only with up->. Without up_ the duplcation can be treated
	! with standard matrix operations.The first transformation object should have Duplicates as an
	! output variable so that the value of Duplicates tells how many duplicates ar
	! made (0= no duplication). The second transformation object defines how the values
	! of subdata variables are determined for each duplicate. The number of duplicate
	! is transmitted to the variable Duplicate. These transformations are called also
	! when Duplicate=0. This means that when there is the duplicate-> option,
	! then all transformations for the subdata can be defined in the duplicate
	! transformation object, and submaketrans-> is not necessary. If there are duplications
	! the %nobsw variable of the upper level data is updated.
 
	! oldobs&-1|1& REAL& If there are duplications of observations, then this option gives the variable
	! into which the original observation number is put. This can be stored in the
	! data by putting it into keep-> list, or, if keep-> option is not given
	! then this variable is automatically put into the keep-> list of the data.
	! oldobsw&-1|1&REAL& This works similarly with respect to the
	!initial obsw variable as oldobs->
	! works for initial obs variable.
	! nobs&-1|1&Real& Gives the number of records in the data file.
	!  If there are
	! fewer records in file as given in nobs-> option, an error occurs. There are two
	! reasons for using nobs-> option. First, one can read a small sample
	! from a large file for testing purposes. Second, the reading is slightly faster as the
	! data can be read directly into proper memory area without using linked buffers.
	! Third, if the data file is so large that a memory overflow occurs, then it may
	! be possible to read data in as linked buffers are not needed.
	!If there is up dta, nobs is not needed as the numbers of observations can be computed
	! from the %nobsw variable in the upper level data.
	! buffersize&-1|1&Real&
	! The number of observations put into one temporary working buffer. The default
	! is 10000. Experimentation with different values of buffersize-> in huge data
	! objects may result in more efficient buffersize-> than is the default (or perhaps
	! not). Note that the buffers are not needed if number of observations is given in
	! nobs->.
 
	! par&-1|1-& Real&additional parameters for reading. If subform-> option is 'bgaya' then par
	! option can be given in form par->(ngvar,npvar) where ngvar is the number
	! of nonperiodic x-variables and npvar is the number of period specific x-variables
	! for each period. Default values are par->(8,93).
 
	! rfhead &-1-1& & When reading data from a text file, the first lines can contain a header which is
	!printed but othewise ignored. If the number of header lines is geater than one,
	!the argument gives the number of header lines. The default number of header lines, when
	!rfhead-> is present,	is one.
 
	! rfcode &-1-1& &The data file can contain also Jlp22-code lines which are first executed. Note the code can
	! be like var1,var,x1...x5=1,2,3,4,5,6,7, which give the possibility to
	! define variables which describe the in-> file. The argument of rfcode-> gives the number of code lines.
	!If there is no argument, one code line is read in.
 
 
	!time&-1|0&& If time-> is present, the cpu-time and total time in function are printed
 
	!endoption
	!Note If there is no keep-> option and there is maketrans-> then the outputvariables
	!are put to the keep variables if hte name of output does not start with $. If the output is not a REAL variable, then this causes trouble.
	!This can be handled using keep-> but it is simpler to start the name of output with $.
	! For instance if variables x1...x5 need to be multiplied with 10, the this can be ahndeld as follows.
	! Make before data() list $li=list(x1...x5). Then the maketrans-transformation can have
	! $li=10*$li, and $li will not be among keep variables.
	!endnote
	!Note there are both header and code lines, they are read in the same order as
	! the options are.
	!endnote
	!Note It was earlier possible to create both the upper level data and subdata with the same data().
	!This made the function very complicated. The function is also otherwise greatly
	! simplified.
	!endnote
	!Note Direct access formats and format for reading Pascal binary files are easy
	! to include.
	!endnote
	! Note  data() function will create a data object object, which is a compound object consisting
	! of links to data matrix, etc. see Data object object. If Data is the output
	! of the function, the function creates the list Data%keep telling the
	! variables in the data and
	! Data%matrix containg the data as a double precision matrix. The number of observations can be obtained by nobs(Data) or by
	! nrows(Data%matrix) or nobs(Data).
	!endnote
	! Note See common options section for how data objects used in other Jlp22 functions will be defined.
	!endnote
 
	!Note In ealier versions, the user could select the name for the variable telling the number
	!	of the observation. After version Dec 20 2022, the name for DATA datab is always datab%obs.
	!endnote
 
	!Note The values can be obtained from variables or form code also in matrix(), but case-> or sparse-> are not
	! possible.
	!endnote
 
	!Note In the previous versions DATA with case names was generated with datawcase() function.
	!This function is not available any more as case-> option does the same thing. The difference is that case->
	! varible need not be in the first column and the case column is now part of the DATA as earlier only the list
	! containg case names was made.
	!endnote
 
	!Note when case-> or sparse-> option is present keep-> will probably mix up Jlp22.
	!endnote
 
	! Note  Options nobs->100, reject->(Record.gt.100) and filter->
	! (Record.le.100) result in the same data object, but when reading a large file, the nobs->
	! option is faster as the whole input file is not read.
	!endnote
	! Note If no observations are rejected, obs variable and Record variable get the same values.
	!endnote
	!Note If virtual memory overflow occurs, see nobs-> optio. This should not happen easily with the currrent
	! 64-bit application.
	!endnote
 
 
 
	!Ex dataex data() generates a new data object by reading data.
	!**This example now contains several combinations of options
	!** and this example is not yet cleaned
	!** it may not contain all possible option comination s
	! da=data(read->(x1,x2),in->)
	! 1,2
	! 3,4
	! /
	! da%keep;
	! da%matrix;
 
	! da2=data(read->(x1...x4),in->)
	! 1,2,3,4
	! 5,6,7,8
	! /
	! da2%keep;
	! da2%matrix;
	! write('da2.txt',$,da2%matrix)
	! close('da2.txt')
	! tr=trans()
	! x13=x1+x3
	! /
	! da2=data(in->'da2.txt',read->(x1...x4),keep->(x2,x4,x13),maketrans->tr)
	! da2%keep;
	! da2%matrix;
	! delete_f('da2.txt')
 
	! write('da3.txt',$,'x1...x3')
	! write('da3.txt',$,da2%matrix)
	! close('da3.txt')
	! da3=data(in->'da3.txt',read->)
	! da3%keep;
	! da3%matrix;
	! delete_f('da3.txt')
 
 
	! da=data(read->(x1,x2),keep->(x1,x2,Regf,Resid),in->)
	! 1,2
	! 3,4
	! /
	! da%keep;
	! da%matrix;
 
	! tr=trans()
	! x1#x2=x1*x2
	! x1%x2=x1+x2
	! /
	! dab=data(read->(x1,x2),maketrans->tr,in->)
	! 1,2
	! 3,4
	! /
	! dab%keep;
	! dab%matrix;
 
	! trup=trans()
	! x1#y1=x1*y1
	! x2%y2=x2+y2
	! /
	! **first variable must be %nobsw
	! da%keep(1)=>da%nobsw
 
	! dasub=data(read->(y1,y2),in->,up->da,maketrans->trup)
	! 3,4
	! 6,7
	! 8,9
	! 10,11
	! /
	! dasub%keep;
	! dasub%matrix;
	! stat(data->dasub)
	! **utilize upper level data
	! stat(data->dasub,up->)
 
	! ndupl=trans()
	! Duplicates=0
	! !da%obs;
	! !Obsw;
	! if(da%obs.eq. 2.and.Obsw.eq.2)Duplicates=2
	! /
	! dupl=trans()
	! x1#y1=x1*y1
	! x2%y2=x2+y2
	! if(Duplicate.gt.0)then
	! x1#y1=x1#y1+Duplicate
	! x1%y1=x1#y1+Duplicate
	! endif
 
	! /
 
	! da%keep;
	! da%matrix;
	! dasub3=data(read->(y1,y2),in->,up->da,maketrans->trup) !,duplicate->(ndupl,dupl))
	! 3,4
	! 6,7
	! 8,9
	! 10,11
	! /
	! dasub3%keep;
	! dasub3%matrix;
 
	! da%keep;
	! da%matrix;
	! dasub2=data(read->(y1,y2),in->,up->da,duplicate->(ndupl,dupl))
	! 3,4
	! 6,7
	! 8,9
	! 10,11
	! /
	! da%matrix;
	! dasub2%keep;
	! dasub2%matrix;
 
	! stat(data->dasub)
	! **utilize upper level data
	! stat(data->dasub2,up->)
	! ;return
	! write('dasub.txt',$,dasub%matrix)
	! close('dasub.txt')
	! **this must be redefined because duplication changed nobsw
	! da=data(read->(da%nobsw,x2),keep->(da%nobsw,x2,Regf,Resid),in->)
	! 1,2
	! 3,4
	! /
	! da%keep;
	! da%matrix;
	! dasub=data(read->(y1,y2),in->'dasub.txt',up->da,maketrans->trup,
	! keep->(y1,y2,@trup%output,Regf,Resid))
	! dasub%keep;
	! dasub%matrix;
	! ** x2 is now alwasy zero
	! regr(y1,x2,data->dasub)
	! ** Regf and Resid are put into the data matrix
	! dasub%matrix;
 
	! ** now x2 is picked from the up-data
	! regr(y1,x2,data->dasub,up->)
	! dasub%matrix;
	! delete_f('dasub.txt')
 
 
	! write('data.txt',$,da%matrix)
	! close('data.txt')
	! dc=data(read->(x1,x2),in->'data.txt')
	! dc%matrix;
	! dc=data(read->(x1,x2),in->'data.txt',maketrans->tr)
	! dc%keep;
	! dc%matrix;
	! ** another way to make data
	! damat=matrix(nrows(da%matrix),ncols(da%matrix),in->'data.txt')
	! da2=newdata(damat,read->(x1...x4))
	! da2%matrix;
	! !;return
 
	! delete_f('data.txt')
 
	! write('data.txt','(4f4.0)',da%matrix)
	! close('data.txt')
 
	! dc=data(read->(x1,x2),form->'(2f4.0)',in->'data.txt')
	! delete_f('data.txt')
	! dc%matrix;
 
	! write('data.bin','b',da%matrix)
	! close('data.bin')
	! dc=data(read->(x1,x2),form->'b',in->'data.bin')
	! dc%matrix;
	! dc=data(read->(x1,x2),form->'b',in->'data.bin',maketrans->tr)
	! dc%keep;
	! dc%matrix;
	! delete_f('data.bin')
 
	! write('data.bin','B',da%matrix)
	! close('data.bin')j_mcase
	! dc=data(read->(x1,x2),form->'B',in->'data.bin')
	! dc%keep;
	! dc%matrix;
 
	! dc=data(read->(x1,x2),form->'B',in->'data.bin',maketrans->tr)
	! dc%keep;
	! dc%matrix;
	! delete_f('data.bin')
 
	!setdata=data(read->(set,set_D),in->,sparse->,case->)
	!set1,15,d50x100,2,d22x100,2
	!set2,18,d50x75,4,d20x100,2
	!/
	!stat()
	!setdata%matrix;
	!setdata%set;
 
	! ;return
	! *** sparse data with case-> and makelist->
	! logdata=data(read->(D_log,L_log,T_log,N_log),makelist->T_log,in->)
	! 15,40,Spruce,200
	! 15,43,Spruce,300
	! 18,40,Pine,100
	! 18,43,Spruce,150
	! /
 
	!endex
	!endsection
 
	!	parameter(ivformstar = 1)  ! * or $ format
	!	parameter(ivformb = 2)   ! 'b' = binary
	!parameter(ivformfortran = 43)  !fortran format
	!	parameter(ivformin = 4)   !read directly from the paragraph
	!	parameter(ivformo1 = 6)
	!	parameter(ivformo2 = 7)
	!	parameter(ivformo3 = 8)
	parameter(ivformd = 49) !'d' =direct access singel
	parameter(ivformd2 = 50) !'d' double
	!		parameter(ivformgaya = 45)
	!integer, dimension(:),pointer::keepl=>null(),keepl2=>null()
	type(j_linkr),pointer ::head=>null(),tail=>null(),ptr=>null()
	!type(j_linkr),pointer ::head2=>null(),tail2=>null(),ptr2=>null()   !subdata
	logical isdupl
	integer :: lr_
 
	logical ::p
	!neede only in sub
 
	integer nnobswcum
	integer, dimension(:), pointer :: obswcum
	integer nobsw
	integer, dimension(:), pointer :: obsw
	!integer nmakelist
 
 
	integer noldsubobs
	integer, dimension(:), pointer :: oldsubobs
 
	!	integer, dimension(:), pointer :: makelist
	!	integer noldobsw
	!	integer, dimension(:), pointer :: oldobsw
 
	character*100 :: form
	integer ::ivkeep,ivmat,ivmaketrans,iout,ivobs
	integer*8::iba,nobs,iobc,nkeep8,nvar,nread
	integer nkeep
	integer::ivnobs,filterlink,rejectlink,nu,lenf,irecord,nbuf,nrejected
	integer ::rfcodelink,rfheadlink
	integer ivform
	integer ::lenform
	logical ::ismaketrans,isnobs,isfilter,isreject,single,iskeep,vff,istrans !var from file
	logical :: isreadindex
	logical isrfhead,isrfcode
	!double precision,allocatable,dimension(:)::readvec
	logical ::cyclerobs,goto51,istime
	logical::up,isgaya
	logical::isff !fortran format
 
	logical::keepopen,continue
	integer,dimension(:),pointer::keep
	!	logical notv
	!integer,dimension(:):: readv
	save
	!logical isinpu
	p=j_v(j_ivdollar).eq.278
	if(p)write(6,*)'<221,iob,io',iob,io,j_o(iob)%i(io:io+10)
	nullify(head)
	nullify(tail)
	nullify(ptr)
 
	iout=0
	single=.false.
	call j_startfunction(iob,io,0,narg,j_optarg0,iout);if(j_err)return
 
	keepopen=j_isoption(iob,io,j_mkeepopen,.false.)
	continue=j_isoption(iob,io,j_mcontinue,.false.)
 
	j_iscode=j_isoption(iob,io,j_mcode)
 
	istime=j_isoption(iob,io,j_mtime)
	if(istime)then
		call cpu_time(cpu0)
		time0=secnds(0.)
	endif !if(istime)  20616
 
	ibas=0
	j_iobs=0
	irecord=0
	iobc=0  !number of obs in current buffer
	nbuf=0
	nrejected=0
	iba=0
	time=0.
	nobs=0
	isgaya=.false.
	isnobs=.false.
	iskeep=.false.
	ismaketrans=.false.
	isfilter=.false.
	isreject=.false.
	!write(6,*)'<777inli',j_linkoption(iob,io,j_min)
	ivnobsw=0
	ivnobswcum=0
 
	if(p)write(6,*)'shshh'
	! %%option
	if(.not.continue)then
		if(p)write(6,*)'hert66',' iob',iob
		nu=j_getin(iob,io,lines=nobs,ivform=ivform)  ! data(
		!	write(6,*)'nu,ivform,nobs',nu,ivform,nobs,j_err
		if(nu.lt.0)then
			write(6,*)'in-> is needed'
			j_err=.true.
		endif !if(nu.lt.0)  20646
		if(j_err)return
		!	write(6,*)'nobs',nobs,nu,ivform
		isnobs=nobs.gt.0
	endif !if(.not.continue)  20642
	isgaya=ivform.eq.j_ivbgaya
 
	single=ivform.eq.j_ivb.or.ivform.eq.j_ivdi.or.ivform.eq.j_ivdg.or.isgaya
 
	!if(p)write(6,*)'id ',id,'aft getin nu,ivform', nu,ivform
	isff=.false.
 
	if(ivform.gt.j_predefined)then
		call j_getchar(ivform,form,lenform);if(j_err)return
		isff=.true.
		il=max(lenform,1)
		if(form(1:1).ne.'('.or.form(il:il).ne.')')then
			write(6,*)'format should start with ( and end with )'
			j_err=.true.;return
		endif !if(form(1:1).ne.'('.or.form(il:il).ne.')')  20665
		!		ivform=ivformfortran
	endif !if(ivform.gt.j_predefined)  20661
 
	if(ivform.eq.j_ivdi.or.ivform.eq.j_ivdg)ivform=ivformd
	if(ivform.eq.j_ivdi2.or.ivform.eq.j_ivdg2)ivform=ivformd2
 
	call j_getoption(iob,io,j_mread,0,9999,j_ipreal,.false.,&
		j_nread,j_readv)
	nread=j_nread
	!	write(6,*)'*note: if variables are read from the first line, use read->$'
	!write(6,*)'jnread',j_nread
	if(j_err)return
	!	write(6,*)'nredfd',j_nread,j_readv
	if(j_nread.eq.0)then
		!	write(6,*)'nu+',nu
		if(nu.eq.0)then
			write(6,*)'with in->, there must be arguments in read->'
			j_err=.true.;return
		endif !if(nu.eq.0)  20684
		vff=.true.
 
	elseif(j_nread.eq.1.and.readv.eq.j_ivdollar)then
		vff=.true.
	else
		vff=.false.
		call j_getname(j_readv(1))
		if(j_oname(1:j_loname).eq.'%nobsw')j_readv(1)=j_getobject(iout,'%nobsw',j_ipreal)
 
	endif !if(j_nread.eq.0)  20682
	!if(.not.vff)rfreadlink=0
 
	if(vff)then
		read(nu,'(a)')j_tempchar2
		leco=len_trim(j_tempchar2)
		if(leco.le.0)then
			write(6,*)'read->$ or read-> but first line is empty'
			j_err=.true.
			return
		endif !if(leco.le.0)  20703
		call j_getname(iout)
		call j_command(j_oname(1:j_loname)//'%read=list('//j_tempchar2(1:leco)//')')
		if(j_err)then
			write(6,*)'illegal variable list:',j_tempchar2(1:leco)
			return
		endif !if(j_err)  20710
		call j_getname(iout) !may change in j-command
		ivread=j_object(j_oname(1:j_loname)//'%read')
		write(6,*)'ivread',ivread,j_oname(1:j_loname)//'%read'
		j_nread=j_o(ivread)%i(1)
		j_readv=>j_o(ivread)%i2(1:j_nread)
		!	rfreadlink=0
	endif !if(vff)  20700
 
 
	!	isrfhead=j_isoption(iob,io,j_mrfhead,.false.)
	!	isrfcode=j_isoption(iob,io,j_mrfcode,.false.)
	if(.not.isnobs)then !can come from getin(
		call j_getoption(iob,io,j_mnobs,-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(j_err)return
 
		isnobs=noptarg.gt.0
		if(isnobs)then
			ivnobs = j_optarg0(1)
			nobs = nint(j_v(ivnobs))
			if(nobs.le.0)then
				write(6,*)'**nobs is illegal ',nobs
				j_err=.true. ;return
			endif !if(nobs.le.0)  20733
		else !if(isnobs)then
			nobs=10**9  !integer*8
		endif !if(isnobs)  20730
	endif !if(.not.isnobs)  20725
 
	call j_getoption(iob,io,j_mup,-1,1,j_ipdata,.true.,narg,j_optarg0);if(j_err)return
 
	up=narg.gt.0
	if(narg.gt.0)ivup=j_optarg0(1)
	!write(6,*)'j_mup ',j_options(j_mup),narg,up,ivup
	!	call j_getname(ivup); write(6,*)j_oname(1:j_loname)
	if(.not.up.and.isgaya)then
		write(6,*)'gaya-> requires up->'
		j_err=.true.;return
 
	endif !if(.not.up.and.isgaya)  20748
	!*********************
	isdupl=.false.
	if(up)then
		ivobsup=j_getobject(ivup,'%obs',j_ipreal)
		!	write(6,*)'ivuphere',ivup
		!	if(j_oname(1:j_loname).eq.'%nobsw')j_d(2)%readv(1)=j_getobject(iout(2),'%nobsw',j_ipreal)
		j_yes=j_isupdata(ivup,keep=j_keepup,nkeep=nkeepup,ivobs=ivobsupm,nobsup=nobsup,&
			point=j_dmatup,nobssub=nobs)
		ivobsw=j_getobject(iout,'%obsw',j_ipreal)
		ivobsw0=j_getobject(0,'Obsw',j_ipreal)
		!	write(6,*)'nkeepup',nkeepup
		if(j_err)return
		isgaya=ivform.eq.j_ivbgaya
 
		if(isgaya)then!	ivarea=j_data_read_
			!	write(6,*)'>&&&&&&&&&'
			ivarea=j_readv(2)
			!		write(6,*)'<633336'
			call j_gayainit(iob,io) !uses par->
			!ivform2=ivform2bgaya
			call j_printname('gaya: area variable is ',ivarea,' ')
		endif !if(isgaya)  20767
		isnobs=.true.
		call j_getoption(iob,io,j_mnobswcum,-1,1,j_ipreal,&
			.true.,noptarg,j_optarg0);if(j_err)return
		ivnobswcum=j_optarg0(1) !  igetopt(iob,io,mnobswcum)
 
		call j_getoption(iob,io,j_mduplicate,-1,2,j_iptrans,.true.,narg,j_optarg0)
 
		if(j_err)return
		if(narg.gt.0)then
			if(narg.eq.1)then
				write(6,*)'duplicate-> needs two arguments'
				j_err=.true.;return
			endif !if(narg.eq.1)  20784
			ivndupl=j_object('Duplicates') !tsekaksuonko output mutujissa
			if(ivndupl.le.0)then
				write(6,*)'**Duplicates -variable not defined'
				j_err=.true. ;return
			end if !if(ivndupl.le.0)  20789
			ivtransndupl=j_optarg0(1)
 
			call j_checkobject(j_o(ivtransndupl)%i2(2),j_iplist);if(j_err)return
			iper=j_inlistobject(ivndupl,j_o(ivtransndupl)%i2(2))
			if(iper.le.0)then
				call j_getname(ivtransndupl)
				write(6,*)'Duplicates is not output variable in ',j_oname(1:j_loname)
				j_err=.true.;return
			endif !if(iper.le.0)  20797
			call j_checkobject(ivndupl,j_ipreal);if(j_err)return
 
			ivduplicate=j_object('Duplicate') !tsekaksuonko output mutujissa
			if(ivduplicate.le.0)then
				write(6,*)'**Duplicate -variable not defined'
				j_err=.true. ;return
			end if !if(ivduplicate.le.0)  20805
			ivtransdupl=j_optarg0(2)
 
			call j_checkobject(j_o(ivtransdupl)%i2(1),j_iplist);if(j_err)return
			iper=j_inlistobject(ivduplicate,j_o(ivtransdupl)%i2(1))
			if(iper.le.0)then
				call j_getname(ivtransdupl)
				write(6,*)'Duplicate is not input variable in ',j_oname(1:j_loname)
				j_err=.true.;return
			endif !if(iper.le.0)  20813
			call j_checkobject(ivduplicate,j_ipreal);if(j_err)return
 
 
			!	call j_getoption(iob,io,j_moldupobs,-1,1,j_ipreal,&
			!		.true.,noptarg,j_optarg0);if(j_err)return
			!	ivoldobs=j_optarg0(1)  !  igetopt(iob,io,moldupobs)
			!	call j_getoption(iob,io,j_moldobsw,-1,1,j_ipreal,&
			!		.true.,noptarg,j_optarg0);if(j_err)return
			!	ivoldobsw=j_optarg0(1)  ! igetopt(iob,io,moldobsw)
			ivoldobsw=j_getobject(iout,'%oldobsw',j_ipreal)
			ivoldobs=j_getobject(iout,'%oldobs',j_ipreal)
			isdupl=.true.
			isnobs=.false.
		endif !if(narg.gt.0)  20783
		!		ivobsw=j_getobject(iout,'%obsw',j_ipreal)
 
	endif !if(up)  20755
	if(p)write(6,*)'PPPPPP'
 
	call j_getoption(iob,io,j_mmaketrans,-1,1,j_iptrans,.true.,noptarg,j_optarg0)
	if(j_err)return
	ismaketrans=noptarg.gt.0
	noul=0
	if(ismaketrans)then
		ivmaketrans=j_optarg0(1)
		ivoul=j_trans_output(j_optarg0(1))
		noul=j_o(ivoul)%i(1)
	endif !if(ismaketrans)  20841
	!nextra0=0
	call j_getoption(iob,io,j_mextra,-1,999,j_ipreal,&
		.true.,nextra,j_optarg0);if(j_err)return
	if(nextra.gt.0)then
		write(6,*)'extra-> is deleted option in data(), use keep-> to give extra variables'
		j_err=.true.;return
	endif !if(nextra.gt.0)  20849
 
 
	call j_getoption(iob,io,j_mkeep,-1,999,j_ipreal,&
		.true.,nkeep,j_keep)
	!	write(6,*)'nek',nkeep
	if(j_err)return
	if(nkeep.gt.0)then
		iskeep=.true.
		ivkeep=j_deflist(iout,'%keep',list0=nkeep,list=j_keep(1:nkeep))
 
 
		!	write(6,*)'nkeepff,',nkeep,j_keepindex
 
		!	endif !if(iskeep)  19253
	else
		ndol=0
		do j=1,j_nread
			call j_getname(j_readv(j))
			if(j_oname(1:1).eq.'$')ndol=ndol+1
			!	write(6,*)j,ndol,j_getnamech(j_readv(j))
		enddo !j=1,j_nread  20869
		nexdup=0
		nkeep0=j_nread-ndol
		if(isdupl)nexdup=2
		nobsw=0
		if(up)nobsw=1
		!	write(6,*)'ndo1,noul,nexdup,novsw,j_nread',ndo1,noul,nexdup,novsw,j_nread
		if(ndol.gt.0)then
			ivkeep=j_deflist(iout,'%keep',nres=j_nread-ndol+noul+nexdup+nobsw)
			do j=1,j_nread
				if(j_getnamech(j_readv(j)).ne.'$')iper=j_putlistobject(ivkeep,single=j_readv(j))
			enddo !j=1,j_nread  20882
 
		else
			ivkeep=j_deflist(iout,'%keep',list0=j_nread,&
				list=j_readv(1:j_nread),nres=noul+nexdup+nobsw)!
		endif !if(ndol.gt.0)  20880
		if(noul.gt.0)iper=j_putlistobject(ivkeep,ivin=ivoul)
		!	if(nextra0.gt.0)iper=j_putlistobject(ivkeep,single=ivnobswcum)
		!	if(nextra.gt.0)iper=j_putlistobject(ivkeep,list0=nextra,list=j_optarg0)
		if(up)iper=j_putlistobject(ivkeep,single=ivobsw)
		if(isdupl)then
			iper=j_putlistobject(ivkeep,single=ivoldobsw)
			iper=j_putlistobject(ivkeep,single=ivoldobs)
		endif !if(isdupl)  20894
		nkeep=j_o(ivkeep)%i(1)
		j_keep=>j_o(ivkeep)%i2(1:nkeep)
		!	write(6,*)'nkeep',nkeep
		if(ndol.gt.0)then
			iskeep=.true. !read list does not start keep list
		else
 
			iskeep=.false.
		endif !if(ndol.gt.0)  20901
	endif !if(nkeep.gt.0)  20859
	if(p)write(6,*)'nkeephwer ',nkeep,j_keep,iskeep,j_nread
 
	if(iskeep)then
		nki=0
		do i=1,nkeep
			ik=j_inlist(j_keep(i),j_nread,j_readv)
			if(ik.gt.0)nki=nki+1
		enddo !i=1,nkeep  20912
		!		if(iskeep)then
		if(allocated(j_keepindex))deallocate(j_keepindex)
		allocate(j_keepindex(1:nki))
		if(allocated(j_readindex))deallocate(j_readindex)
		allocate(j_readindex(1:nki))
		if(allocated(j_readindex0))deallocate(j_readindex0)
		allocate(j_readindex0(1:nki))
		nki=0
		nokeep=0
		isreadindex=.false.
 
		do i=1,j_nread
			ik=j_inlist(j_readv(i),nkeep,j_keep)
			if(ik.gt.0)then
				nki=nki+1
				j_keepindex(nki)=i
 
				call j_getname(j_readv(i))
				!		write(6,*)'i,ik ',i,ik,j_oname(1:j_loname)
				if(nokeep.gt.0)isreadindex=.true. ! extras are not at the end
			else
				nokeep=nokeep+1
			endif !if(ik.gt.0)  20929
		enddo !i=1,j_nread  20927
		if(isreadindex)then
			nki2=0
			do i=1,nkeep
				ik=j_inlist(j_keep(i),j_nread,j_readv)
				if(ik.gt.0)then
					nki2=nki2+1
					j_readindex(nki2)=i
 
					!	call j_getname(j_readv(i))
					!	write(6,*)'i,ik ',i,ik,j_oname(1:j_loname)
					!	if(nokeep.gt.0)j_yes=.false. ! extras are not at the end
				endif !if(ik.gt.0)  20944
 
			enddo !i=1,nkeep  20942
 
 
 
		endif !if(isreadindex)  20940
 
		nkeep0=nki
		!	iskeep=j_yes
		!	write(6,*)'nkeep0,iskeep',nkeep0,iskeep
		!	write(6,*)j_keepindex
		! if(j_yes)then
		! iskeep=.true.  !extras are not at the
		! else
		! iskeep=.false.
		! endif !if(j_yes)  19441
 
	endif !if(iskeep)  20910
 
	if(p)write(6,*)'**************'
 
 
	!	end if !if(nkeep.gt.0)  18935
 
	ivobs=j_getobject(iout,'%obs',j_ipreal)
 
	filterlink=j_codelink(iob,io,j_mfilter); if(j_err)return
	isfilter=filterlink.ne.0
 
	rejectlink=j_codelink(iob,io,j_mreject); if(j_err)return
	isreject=rejectlink.ne.0
 
	j_eof=.false.
 
 
	if(j_err)return
	call j_getoption(iob,io,j_mcase,-1,1,j_ipreal,.false.,noptarg,j_optarg0)
	if(j_err)return
	!	j_caseread=0
 
	j_iscase=noptarg.ge.0
	if(noptarg.gt.0)then
 
		j_ivcase=j_optarg0(1)
 
		!	call j_getname(j_ivcase)
		!	write(6,*)'ivcae',j_ivcase,j_oname(1:j_loname)
	elseif(noptarg.eq.0)then
		j_ivcase=j_getobject(0,'Case',j_ipreal)
 
	endif !if(noptarg.gt.0)  20993
 
 
 
	if(nu.gt.0.and.(j_iscase.or.j_iscode))then
 
		nobs=j_nlinesnu(nu)
		isnobs=.true.
 
	endif !if(nu.gt.0.and.(j_iscase.or.j_iscode))  21006
	!	write(6,*)'isno',isnobs,iout,'%'//j_oname(1:j_loname),'nobbs',nobs
	j_ivcaselist=0
	if(j_iscase)then
		nobs4=nobs
		j_ivcaselist=j_deflist(iout,'%case',nres=nobs4)
	endif !if(j_iscase)  21014
	if(p)write(6,*)'HHHHHHHHHHHHHHHHh'
 
	!!!!!!!!!!!!!!!!duplication
	!write(6,*)'isnobs',isnobs,nobsb,nkeep
	if(.not.isnobs)then
		if(.not.associated(head))allocate (head)
		tail=>head ;nullify(tail%pnext)
		!write(6,*)'<87>',nobsb,nvar,nobsb*nvar
		ivdefn=j_igetopt(iob,io,j_mbuffersize)
		if(ivdefn.gt.0)then
			nobsb=j_v(ivdefn)
		else !if(ivdefn.gt.0)then
			nobsb=10000
		end if !if(ivdefn.gt.0)  21027
		allocate(tail%rbuf(1:nobsb*nkeep))
		iba=0
 
	endif !if(.not.isnobs)  21022
 
	if(ivform.eq.j_ivb.or.ivform.eq.j_ivdi.or.ivform.eq.j_ivdg.or.&
			ivform.eq.j_ivbgaya)then
		if(allocated(j_readvecsing))deallocate(j_readvecsing)
		!		write(6,*)'<466464774474 ',j_nread
		allocate(j_readvecsing(1:j_nread))
		!		write(6,*)'<4665555 ',j_nread
	else !if(ivform.eq.j_ivb.or.ivform.eq.j_ivdi.or.ivform.eq.j_ivdg)then
		if(allocated(j_readvec))deallocate(j_readvec)
		allocate(j_readvec(1:j_nread))
	endif !if(ivform.eq.j_ivb.or.ivform.eq.j_ivdi.or.ivform.eq.j_ivdg  21037
	!	write(6,*)'readvec',j_nread,j_readvec
	if(allocated(j_keepvec))deallocate(j_keepvec)
 
	allocate(j_keepvec(1:nkeep))
	j_keepvec=j_0
	!	write(6,*)'nkeptas',nkeep,j_keepvec
	nhead=0
	ncode=0
 
	!rfheadlink=j_linkoption(iob,io,j_mrfhead,link=.true.)  !,clear=.true.,link=.true.)
	!	if(rfheadlink.gt.0)then
	call j_getoption(iob,io,j_mrfhead,-1,1,j_ipreal,.false.,nhead,j_optarg0,link=rfheadlink)
	!	write(6,*)'nhead',nhead,'link',rfheadlink
	if(j_err)return
	if(nhead.eq.1)then
		nhead=j_v(j_optarg0(1))
 
		if(nhead.lt.1)then
			write(6,*)'rfhead-> had illegal argument ',nhead
			j_err=.true.;return
		endif !if(nhead.lt.1)  21064
	elseif(nhead.eq.0)then
		nhead=1
 
 
	endif !if(nhead.eq.1)  21061
	!	else
	!		rfheadlink=999999
	!	endif !if(rfheadlink.gt.0)  20433
 
	!rfcodelink=j_linkoption(iob,io,j_mrfcode,link=.true.) !,clear=.true.,link=.true.)
	!write(6,*)'rfcodelink',rfcodelink
	!if(rfcodelink.gt.0)then
	call j_getoption(iob,io,j_mrfcode,-1,1,j_ipreal,.false.,ncode,j_optarg0,link=rfcodelink)
	!	write(6,*)'rfcodelink,',rfcodelink,'ncode',ncode
	!		write(6,*)'ncode,j_optarg0 ',ncode,j_optarg0
	if(j_err)return
	if(ncode.eq.1)then
		ncode=j_v(j_optarg0(1))
 
		if(ncode.lt.1)then
			write(6,*)'rfcode-> had illegal argument ',ncode
			j_err=.true.;return
		endif !if(ncode.lt.1)  21087
	elseif(ncode.eq.0)then
		ncode=1
	endif !if(ncode.eq.1)  21084
 
	!write(6,*)'ncode,nhead',ncode,nhead,rfheadlink,rfcodelink
	goto 456
987		write(6,*)'premature end/error when reading ',nhead,' rfhead-> lines ',&
		' and ',ncode,' rfcode-> lines'
	j_err=.true.;return
 
456		do ilin=1,ncode+nhead
		if(ivform.eq.ivformd.or.ivform.eq.ivformb)then
			write(6,*)'* rfcode does not work with binary files'
			j_err=.true.;return
		endif !if(ivform.eq.ivformd.or.ivform.eq.ivformb)  21102
 
		read(nu,'(a)',err=987,end=987)j_tempchar3
		leco=j_lentrim(j_tempchar3)
		!	write(6,*)'rfheadlink,rfcodelink',rfheadlink,rfcodelink,ncode,nhead
		if(rfheadlink.gt.rfcodelink.and.nhead.gt.0)then
			write(6,*)'head: ',j_tempchar3(1:leco)
			nhead=nhead-1
		else
			call j_clean(j_tempchar3,leco)
			write(6,*)'from rfcode->: ',j_tempchar3(1:leco)
			call j_command(j_tempchar3(1:leco))
			if(j_err)then
				write(6,*)'error was in rfcode:',j_tempchar3(1:leco)
				return
			endif !if(j_err)  21117
			ncode=ncode-1
			if(ncode.eq.0)rfcodelink=0
		endif !if(rfheadlink.gt.rfcodelink.and.nhead.gt.0)  21110
 
	enddo !		do ilin=1,ncode+nhead  21101
 
	!!call j_clearoption(iob,io)  ! subroutine
	istrans=ismaketrans.or.isreject.or.isfilter.or.isdupl
 
	if(p)write(6,*)'<555rfcode nu',nu,' nobs ',nobs,'isnobs ',isnobs,' iout ',iout ,' nkeep ',nkeep
 
	nkeep8=nkeep
	if(isnobs)ivmat=j_defmatrix(iout,'%matrix',nobs,nkeep8,j_matreg)
 
 
	!taala
	! write(6,*)'keepindex ',j_keepindex
	! write(6,*)'up',up
	! write(6,*)nkeep,'nkeep'
	! write(6,*)j_keep,'keep'
	! write(6,*)'j_keepvec',j_keepvec
	! write(6,*)ivform,'ivform'
	! write(6,*)j_readv,'readv'
	! write(6,*)isnobs,'isnobs'
	! write(6,*)nobs,'nobs'
	! write(6,*)ivmaketrans,'ivmaketrans'
	! write(6,*)istrans,'istrans'
	! write(6,*)iskeep,'iskeep'
	! write(6,*)isreject,'isreject'
	! write(6,*)isfilter,'isfilter'
	! write(6,*)j_filterlink,'j_filterlink'
	! write(6,*)j_rejectlink,'j_rejectlink'
	! write(6,*)isrfhead,'isrfhead'
	! write(6,*)isrfcode,'isrfcode'
	! if(up)write(6,*)rfcodelink,'rfcodelink'
	! if(up)write(6,*)'datamatup,',j_dmatup
	! if(up)write(6,*)'j_keepup',j_keepup
	! write(6,*)'nkeeup',nkeepup
 
	if(ivform.eq.j_ivb)then
		if(allocated(j_readvecsing))deallocate(j_readvecsing);allocate(j_readvecsing(1:j_nread))
		!write(6,*)'j_readvecsing',j_readvecsing
	else
		if(allocated(j_readvec))deallocate(j_readvec);allocate(j_readvec(1:j_nread))
	endif !if(ivform.eq.j_ivb)  21160
 
	!call j_fromvarinit(iob,io)
 
	!write(6,*)'j_readvec',j_readvec
 
	!write(6,*)'nu',nu
	! if(isrfcode)then
	! read(nu,*)j_tempchar3
	! le=j_lentrim(j_tempchar3)
	! write(6,*)'rfcode; ',j_tempchar3(1:le)
	! call j_command(j_tempchar3(1:le))
	! if(j_err)return
	! endif
	nduplsum=0
	ibasup=0  !data
	ibsw=0
	ibs=0
	j_iobs=0
	irecord=0
	nrejected=0
	!j_iobs=0
 
	j_isnobs=isnobs
	j_nu=nu
	j_isreadnu=nu.gt.0
	!j_isgetinp=nu.le.0
	j_isgetinp=nu.le.0
	if(j_isgetinp)ivform=j_ivdollar
 
	!call j_getoption(iob,io,j_mmakelist,-1,1,j_ipreal,.true.,j_makelistnvar,j_makelist)
	! j_ismakelist=noptarg.gt.0
	! if(j_ismakelist)then
	! j_makelistvar=j_optarg0(1)
	! j_makelistnvar=j_inlistobject(j_makelistvar,ivkeep)
	! if(j_makelistnvar.le.0)then
	! call j_getname(j_makelistvar)
	! write(6,*)'makelist-> variable',j_oname(1:j_loname),' not among read->'
	! j_err=.true.; return
	! endif !if(j_makelistnvar.le.0)  21156
	! if(j_isnobs)then
	! nobs4=nobs
	! j_makelist=j_deflist(j_makelistvar,'%list',nres=nobs4)
	! else
	! j_makelist=j_deflist(j_makelistvar,'%list',nres=50)
	! endif !if(j_isnobs)  21161
 
	! endif !if(j_ismakelist)  21153
	j_needsall=.true.
 
	if(allocated(j_readvec))deallocate(j_readvec)
	allocate(j_readvec(1:j_nread))
	j_end=.false.
	!	write(6,*)'istrans,up,',istrans,up
 
 
	j_issparse=j_isoption(iob,io,j_msparse)
	if(j_issparse)then
 
		j_nsparseval=0
		if(allocated(j_sparsen))deallocate(j_sparsen,j_sparsekeep,j_sparseval)
		allocate(j_sparsen(1:nobs),j_sparsekeep(0:20*nobs),j_sparseval(1:20*nobs))
		j_ivtemplist= j_deflist(0,'$templist',nres=200)
 
		j_sparsen(1:nobs)=0
		j_sparseval=j_0
 
 
	endif !if(j_issparse)  21222
 
	if(j_isreadnu.and.j_iscode)j_isreadnu=.false.
	if(j_isgetinp.and.j_isinpu)isnobs=.false.
	!	call readsparse()
	if(.not.istrans.and..not.isgaya.and..not.up)then !.and.isnobs)then
		call fast() !can read directly to the matrix  without v-vector keep/nokeep isnobs/noisnobs
	elseif(up.and.istrans.or.isgaya)then   !
		call upv()  !inclduning gaya
	elseif(istrans.and.isnobs)then
		call subv()
 
	elseif(up)then
		call fastup()
	else
 
		!istarns
		!write(6,*)'up,istrans,isnobs',up,istrans,isobs
		call v()
 
	endif !if(.not.istrans.and..not.isgaya.and..not.up)  21238
	if(nu.le.0)then
		!call j_getinput('rhup>',single=.true.)
		!write(6,*)'dhhd>',j_inp(1:j_linp)
		call j_exitpara()
	else
		if(.not.keepopen)call j_closeunit(nu)
	endif !if(nu.le.0)  21254
	if(j_err)return
	if(j_issparse)then
		write(6,*)'got ',j_o(j_ivtemplist)%i(1),' variables in sparse part'
		ipre=j_o(ivkeep)%i(1)
		nad=j_o(j_ivtemplist)%i(1)
		do i=1,nad
			iper=j_putlistobject(ivkeep,single=j_o(j_ivtemplist)%i2(i))
			if(iper.le.ipre)then
				call j_getname(j_o(j_ivtemplist)%i2(i))
				write(6,*)'sparse variable ',j_oname(1:j_loname),' was in the head part'
				j_err=.true.
				ipre=j_o(ivkeep)%i(1)
			endif !if(iper.le.ipre)  21268
		enddo !i=1,nad  21266
		if(j_err)goto 543
 
		call j_allocated(j_tempvector,(nad+nkeep)*nobs)
		! if(allocated(j_tempvector))deallocate(j_tempvector)
		! allocate(j_tempvector(1:(nad+nkeep)*nobs))
		! j_tempvector=j_0
		iba=0
		iba2=0
		iba3=0
		!	write(6,*)'nkeep,j_sparsekeep,j_sparseval,j_sparsen',j_nsparseval,nkeep,j_sparsekeep(1:j_nsparseval),&
		!	'*',j_sparseval(1:j_nsparseval),'*',j_sparsen
		do i=1,nobs
			j_tempvector(iba2+1:iba2+nkeep)=j_o(ivmat)%d(iba+1:iba+nkeep)
			iba=iba+nkeep
			iba2=iba2+nkeep
			j_tempvector(iba2+j_sparsekeep(iba3+1:iba3+j_sparsen(i)))=j_sparseval(iba3+1:iba3+j_sparsen(i))
			iba3=iba3+j_sparsen(i)
			iba2=iba2+nad
			!		write(6,*)'iba,iba2,iba3',iba,iba2,iba3
		enddo !i=1,nobs  21286
		call move_alloc(from=j_tempvector,to=j_o(ivmat)%d)
		j_o(ivmat)%i(2)=nad+nkeep
		j_o(ivmat)%i(3)=nobs*(nad+nkeep)
 
 
 
 
543 deallocate(j_sparsen,j_sparsekeep,j_sparseval)
		call j_del(j_ivtemplist)
 
	endif !if(j_issparse)  21262
 
 
 
 
 
	!	j_iobs=j_iobs-nrejected
	!write(6,*)'j_iobs',j_iobs,' nrejected ',nrejected
 
	!600 	nob(1)=iobs(1)   !!!!! clean
	!	if(allocated(j_d(1)%readv))deallocate(j_d(1)%readv)
	if(allocated(j_readvec))deallocate(j_readvec)
	!		if(allocated(j_d(2)%readv))deallocate(j_d(2)%readv)
	if(allocated(j_readvecsing))deallocate(j_readvecsing)
	if(isreject.or.isfilter)write(6,*)'*number of rejected observations ',nrejected
	if(nbuf.gt.0.and..not.isnobs)write(6,*)'number of data buffers ',nbuf+1, ' (faster with nobs->)'
	if(j_iobs.le.0)then
		write(6,*)'*no observations, data not created'
		j_err=.true.
	endif !if(j_iobs.le.0)  21321
	if(j_err)call j_del(iout)
 
 
 
	if(.not.isnobs)then
		nkeep8=nkeep
		if(p)write(6,*)'j_iobs,nkeep ',j_iobs,nkeep,iout
		if(.not.j_err)ivmat=j_defmatrix(iout,'%matrix',j_iobs,nkeep8,j_matreg)
		!if(idisk.gt.0)goto 800
		!if(p)write(6,*)'ivmat(1) ',ivmat
 
		ptr=>head
		ibaa=0
		do i=1,nbuf
			!write(6,*)'<77ibuf',i,nobsb,iobc
			if(.not.j_err)j_o(ivmat)%d(ibaa+1:ibaa+nobsb*nkeep)=ptr%rbuf
			!		write(6,*)'<772',i,nobsb,iobc
			deallocate(ptr%rbuf)
			!		write(6,*)'<772',i,nobsb,iobc
			nullify(ptr%rbuf)
			!		write(6,*)'<774f',i,nobsb,iobc
			ptr=>ptr%pnext
			!	write(6,*)associated(ptr),associated(head),associated(tail)
			!	write(6,*)'<775',i,nobsb,iobc
			ibaa=ibaa+nobsb*nkeep
			!		write(6,*)'<776',i,nobsb,iobc
		end do !i=1,nbuf  21338
		!write(6,*)'<77ibufss',i,nobsb,iobc,'ibaa',ibaa,ptr%rbuf(1:iobc*nkeep)
		if(.not.j_err)j_o(ivmat)%d(ibaa+1:ibaa+iobc*nkeep)=ptr%rbuf(1:iobc*nkeep)
		!	write(6,*)'<77ahahha'
		deallocate(ptr%rbuf);nullify(ptr%rbuf)
		tail=>head
		do i=1,nbuf
			!	write(6,*)'<77ahbba'
			ptr=>tail%pnext
			nullify(tail%pnext)
			tail=>ptr
		end do !i=1,nbuf  21357
	endif !if(.not.isnobs)  21329
 
	!write(6,*)'<787e7iobs(1)',j_iobs(1),nrejected(1)
 
	if(isnobs.and.nobs>j_iobs)then
		if(j_iobs.eq.0)then
			write(6,*)'*no observations, data not created'
			j_err=.true.
			j_err=.true.
			call j_del(ivmat)
			!	if(sub)call j_del(ivmat(2))
			return
		else
			write(6,*)'*number of observations ',j_iobs,' is less than given in nobs-> ',nobs
			write(6,*)'the data matrix has now ',(nobs-j_iobs), ' extra rows'
			nkeep8=nkeep
			call j_defmatdim(ivmat,j_iobs,nkeep8)
			!	j_o(ivmat)%i(1)=iobs
			!	j_o(ivmat)%i(3)=iobs*nkeep
		endif !if(j_iobs.eq.0)  21368
 
	endif !if(isnobs.and.nobs>j_iobs)  21367
	!	write(6,*)'ivup',ivup,iout,up
	if(.not.j_err)then
		if(j_iobs.le.0)then
			write(6,*)'All observations rejected, DATA not created'
			return
		endif !if(j_iobs.le.0)  21387
		call j_defdata(iout,ivmat,ivkeep,j_ivcaselist)   !ivout2 for mother
		j_dlastdata=iout
		j_v(j_ivlastdata)=iout
		write(6,*)
		if(up)then
			call j_linkdata(ivup,iout)
			call j_getname(ivup)
			write(6,*)'number of observations ',j_iobs,' keep variables ', nkeep,' up-data ',j_oname(1:j_loname)
		else
			write(6,*)'number of observations ',j_iobs,' keep variables ', nkeep
		endif !if(up)  21395
		!	write(6,*)'<466464lopus',ivmat,ivkeep,j_o(ivkeep)%i
		!	write(6,*)'*####',j_o(ivkeep)%i2,'%%%',j_o(ivmat(1))%i,'*&&&',j_o(ivmat(1))%d
		!	write(6,*)ivnobsw,ivobs(1)
		!if(sub)j_o(iout)%i(9)=maxnobsw   !note it is stored here
 
 
	else
		if(j_dlastdata.eq.iout)then
			j_dlastdata=0
			j_dlastdata=0
		endif !if(j_dlastdata.eq.iout)  21409
	endif !if(.not.j_err)  21386
	!	j_o(j_ivlastdata)%i(1)=1;j_o(j_ivlastdata)%i2(1)=iout
 
 
 
	!	 call j_getoption(iob,io,j_mfromvar,-1,9999,j_ipreal,.true.,nfromvar,fromvar)
	!	isfromvar=nfromvar.gt.0
 
 
 
	if(istime)then
		call cpu_time(cpu1)
		time1=secnds(time0)
		write(6,*)'data() used cpu ',cpu1-cpu0,' s and total time ',time1,' s'
	endif !if(istime)  21423
	write(6,*)' '
	return
	!these variables are used in several subroutines thus they need be imntiated here
 
 
	contains !subroutine more2
 
	subroutine putlinked()
 
		!write(6,*)'putlink,iobc',iobc,'iba',iba,'keepc',j_keepvec
		iobc=iobc+1
		!	write(6,*)'>221iobc',iobc,nobsb,'nkeep',nkeep,j_keepvec
		if(iobc.gt.nobsb)then !store in chained list
			!	if(p)write(6,*)'>22iobc',iobc,associated(tail%pnext),associated(tail%pnext)
			if(associated(tail%pnext))then
				!		write(6,*)'allo:',allocated(tail%pnext)
				deallocate(tail%pnext)
			endif !if(associated(tail%pnext))  21442
			if(.not.associated(tail%pnext))allocate(tail%pnext)
			!		if(p)write(6,*)'>22iobsssc',nobsb,nkeep
			tail=>tail%pnext ;nullify(tail%pnext)
			!if(p)write(6,*)'>22iobsssc',nobsb,nkeep
			allocate(tail%rbuf(1:nobsb*nkeep))
			iobc=1
			iba=0
			nbuf=nbuf+1
			!			if(p)write(6,*)'>22iohere ',nbuf
		end if !if(iobc.gt.nobsb)  21440
		100 continue
!		write(6,*)'putli',iba+1,iba+nkeep,nkeep,j_keepvec
		tail%rbuf(iba+1:iba+nkeep)=j_keepvec !j_v(j_o(ivkeep)%i2(1:nkeep))  !j_v(keepl(1:nvar))
		iba=iba+nkeep
		!		iba=iba+nkeep
	end subroutine putlinked
 
	subroutine fast()
		!.not.istrans  isnobs true or fasle
		write(6,*)'******fast' ! nu,nobs,ivform,iskeep,isnobs',nu,nobs,ivform,iskeep,isnobs,'igaya,istrans',isgaya,istrans
		!	write(6,*)'nobs,nu,ivform,nvar',nobs,nu,ivform,nvar,isnobs,'dol',j_ivdollar,'nread',j_nread
 
		!	write(6,*)'nkeep0',nkeep0,'isreadindex',isreadindex !no l
		!	write(6,*)'keepvec',nkeep,j_keepvec
		!	write(6,*)'readvvec',j_nread,j_readvec
		!	write(6,*)'keepind',j_keepindex
		!	write(6,*)'nkeep0',nkeep0
 
		ibas=0
		!		write(6,*)'isnobs,nobs(1),ivform(1),j_ivdollar',isnobs,nobs(1),ivform(1),j_ivdollar
		!	if(nu.eq.0.and..false.)then
		! !write(6,*)'ismake',j_ismakefrom,iskeep
		! do j_iobs=1,nobs
		! call j_getinput(' ')
		! if(iskeep)then
		! if(j_ismakefrom)then
		! call j_readfromvar0()
		! write(6,*)'vec ',j_readvec
		! else
		! read(j_inp(1:j_linp),*,err=5,end=5)j_readvec
		! endif !if(j_ismakefrom)  21044
		! if(isreadindex)then
		! j_readindex0=ibas+j_readindex
		! j_o(ivmat)%d(j_readindex0)=j_readvec(j_keepindex)
		! else
		! !(j_keepindex)
		! j_o(ivmat)%d(ibas+1:ibas+nkeep0)=j_readvec(j_keepindex)
		! endif !if(isreadindex)  21050
		! else
		! if(j_ismakefrom)then
		! call j_readfromvar0()
		! write(6,*)'vec ',j_readvec
		! j_o(ivmat)%d(ibas+1:ibas+nkeep0)=j_readvec
		! else
 
 
		! read(j_inp(1:j_linp),*,err=5,end=5)j_o(ivmat)%d(ibas+1:ibas+nkeep0)
		! endif !if(j_ismakefrom)  21058
		! endif !if(iskeep)  21043
		! ibas=ibas+nkeep
		! enddo !j_iobs=1,nobs  21041
		! call j_getinput(' ')  !/
		! j_iobs=nobs
		! return
		! 5			call j_exitpara()
 
		if(ivform.eq.j_ivb)then
			if(iskeep)then
				if(isreadindex)then
					if(isnobs)then
						do j_iobs=1,nobs
							read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1))
							j_readindex0=ibas+j_readindex
							j_o(ivmat)%d(j_readindex0)=j_readvecsing(j_keepindex)  !(1:nkeep(1))
							ibas=ibas+nkeep
						enddo !j_iobs=1,nobs  21516
						j_iobs=nobs;return
					else !j_iobs
 
						do j_iobs=1,nobs
							read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1))
							j_keepvec(j_readindex)=j_readvecsing(j_keepindex)
							call putlinked()
						enddo !j_iobs=1,nobs  21525
 
					endif !if(isnobs)  21515
 
				else
					if(isnobs)then
						do j_iobs=1,nobs
							read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1))
							j_o(ivmat)%d(ibas+1:ibas+nkeep0)=j_readvecsing(j_keepindex)  !(1:nkeep(1))
							ibas=ibas+nkeep
						enddo !j_iobs=1,nobs  21535
						j_iobs=nobs;return
					else !j_iobs
 
						do j_iobs=1,nobs
							read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1))
							j_keepvec(1:nkeep0)=j_readvecsing(j_keepindex)
							call putlinked()
						enddo !j_iobs=1,nobs  21543
 
					endif !if(isnobs)  21534
				endif !if(isreadindex)  21514
			else !iskeep
				if(isnobs)then
					do j_iobs=1,nobs
						read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1))
						j_o(ivmat)%d(ibas+1:ibas+nkeep0)=j_readvecsing !(1:nkeep(1))
						ibas=ibas+nkeep
					enddo !j_iobs=1,nobs  21553
					j_iobs=nobs;return
				else !j_iobss
 
					do j_iobs=1,nobs
						read(nu,err=957,end=958)j_readvecsing
						j_keepvec(1:nkeep0) =j_readvecsing !(1:nkeep(1))
						call putlinked()
					enddo !j_iobs=1,nobs  21561
 
				endif !if(isnobs)  21552
 
			endif !if(iskeep)  21513
 
 
 
		elseif(ivform.eq.j_ivb2)then
 
			if(iskeep)then
				if(isreadindex)then
					if(isnobs)then
						do j_iobs=1,nobs
							read(nu,err=957,end=958)j_readvec  !(1:nkeep(1))
							j_readindex0=ibas+j_readindex
							j_o(ivmat)%d(j_readindex0)=j_readvec(j_keepindex)  !(1:nkeep(1))
							ibas=ibas+nkeep
						enddo !j_iobs=1,nobs  21578
						j_iobs=nobs;return
					else !iobss
 
						do j_iobs=1,nobs
							read(nu,err=957,end=958)j_readvec
							j_keepvec(j_readindex)=j_readvec(j_keepindex)  !(1:nkeep(1))
							call putlinked()
						enddo !j_iobs=1,nobs  21587
 
					endif !if(isnobs)  21577
 
				else
					if(isnobs)then
						do j_iobs=1,nobs
							read(nu,err=957,end=958)j_readvec  !(1:nkeep(1))
							j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_readvec(j_keepindex)  !(1:nkeep(1))
							ibas=ibas+nkeep
						enddo !j_iobs=1,nobs  21597
						j_iobs=nobs;return
					else !j_iobss
 
						do j_iobs=1,nobs
							read(nu,err=957,end=958)j_readvec
							j_keepvec(1:nkeep0)=j_readvec(j_keepindex)  !(1:nkeep(1))
							call putlinked()
						enddo !j_iobs=1,nobs  21605
 
					endif !if(isnobs)  21596
				endif !if(isreadindex)  21576
			else !iskeep
				if(isnobs)then
					do j_iobs=1,nobs
						read(nu,err=957,end=958)j_o(ivmat)%d(ibas+1:ibas+nkeep0)
						ibas=ibas+nkeep
					enddo !j_iobs=1,nobs  21615
					j_iobs=nobs;return
				else !j_iobss
 
					do j_iobs=1,nobs
						read(nu,err=957,end=958)j_keepvec(1:nkeep0) !(1:nkeep(1))
						call putlinked()
					enddo !j_iobs=1,nobs  21622
 
				endif !if(isnobs)  21614
 
			endif !if(iskeep)  21575
		else if(ivform.eq.j_ivdollar)then
			!		write(6,*)'herr,iskeep,isnobs,nkeep0',iskeep,isnobs,nkeep0,'nvar',nvar,'nread',nread
			if(iskeep)then
				if(isreadindex)then
					if(isnobs)then
						do j_iobs=1,nobs
							call j_readvector(nu,ivform,j_readvec,nread)
							if(j_err)goto 957
							!			if(j_end)goto 958
							!	read(nu,*,err=957,end=958)j_readvec  !(1:nkeep(1))
							j_readindex0=ibas+j_readindex
							j_o(ivmat)%d(j_readindex0)=j_readvec(j_keepindex)  !(1:nkeep(1))
							ibas=ibas+nkeep
						enddo !j_iobs=1,nobs  21635
						j_iobs=nobs
						!	if(j_isgetinp)call j_exitpara()
						return
					else !j_iobss
 
						do j_iobs=1,nobs  !nobs i huge
							call j_readvector(nu,ivform,j_readvec,nread)
							if(j_err)goto 957
							if(j_end)goto 958
							!	read(nu,*,err=957,end=958)j_readvec
							!		if(j_iobs.eq.1)write(6,*)'rv',j_readvec
							!		if(j_iobs.eq.1)write(6,*)'nkeep,nkeep0',nkeep,nkeep0
							j_keepvec(j_readindex)=j_readvec(j_keepindex)
							!	if(j_iobs.eq.1)write(6,*)'k_vec',j_keepvec(1:nkeep0)
							call putlinked()
						enddo !j_iobs=1,nobs  21649
						!	if(j_isgetinp)call j_exitpara()
					endif !if(isnobs)  21634
 
 
				else
					if(isnobs)then
 
						do j_iobs=1,nobs
							call j_readvector(nu,ivform,j_readvec,nread)
							if(j_err)goto 957
							!	if(j_end)goto 958
							!	read(nu,*,err=957,end=958)j_readvec  !(1:nkeep(1))
							j_o(ivmat)%d(ibas+1:ibas+nkeep0)=j_readvec(j_keepindex)  !(1:nkeep(1))
							ibas=ibas+nkeep
						enddo !j_iobs=1,nobs  21667
						j_iobs=nobs
						!if(j_isgetinp)call j_exitpara()
						return
					else !j_iobss
						!	write(6,*)'nobs',nobs
						do j_iobs=1,nobs
							call j_readvector(nu,ivform,j_readvec,nread)
							!		write(6,*)'iobs',j_err,j_end,j_readvec(1:nvar)
							if(j_err)goto 957
							if(j_end)goto 958
							!read(nu,*,err=957,end=958)j_readvec
							!		if(j_iobs.eq.1)write(6,*)'rv',j_readvec
							!		if(j_iobs.eq.1)write(6,*)'nkeep,nkeep0',nkeep,nkeep0
							j_keepvec(1:nkeep0)=j_readvec(j_keepindex)
							!	if(j_iobs.eq.1)write(6,*)'k_vec',j_keepvec(1:nkeep0)
							call putlinked()
						enddo !j_iobs=1,nobs  21680
						!	if(j_isgetinp)call j_exitpara()
					endif !if(isnobs)  21665
				endif !if(isreadindex)  21633
 
			else !iskeep
				if(isnobs)then
					do j_iobs=1,nobs
						call j_readvector(nu,ivform,j_o(ivmat)%d(ibas+1:ibas+nkeep0),nread)
						if(j_err)goto 957
						!		if(j_end)goto 958
						!	read(nu,*,err=957,end=958)j_o(ivmat)%d(ibas+1:ibas+nkeep0)
						ibas=ibas+nkeep
					enddo !j_iobs=1,nobs  21698
					!if(j_isgetinp)call j_exitpara()
					j_iobs=nobs;return
				else !j_iobss
					!write(6,*)'ncncnc',nobs
					do j_iobs=1,nobs
						call j_readvector(nu,ivform,j_keepvec(1:nkeep0),nread)
						if(j_err)goto 957
						if(j_end)goto 958
						!					read(nu,*,err=957,end=958)j_keepvec(1:nkeep0) !(1:nkeep(1))
						!write(6,*)'j_iobs',j_iobs,j_keepvec
						call putlinked()
					enddo !j_iobs=1,nobs  21709
					!	if(j_isgetinp)call j_exitpara()
				endif !if(isnobs)  21697
				!
			endif !if(iskeep)  21632
		else if(isff)then
			!write(6,*)'herr',iskeep,isnobs
			if(iskeep)then
				if(isreadindex)then
					if(isnobs)then
						do j_iobs=1,nobs
							read(nu,form(1:lenform),err=957,end=958)j_readvec  !(1:nkeep(1))
							j_readindex0=ibas+j_readindex
							j_o(ivmat)%d(j_readindex0)=j_readvec(j_keepindex)  !(1:nkeep(1))
							ibas=ibas+nkeep
						enddo !j_iobs=1,nobs  21726
						j_iobs=nobs;return
					else !j_iobss
 
						do j_iobs=1,nobs
							read(nu,form(1:lenform),err=957,end=958)j_readvec
							! write(6,*)j_readvec
							! write(6,*)'keepi',keepindex
							j_keepvec(j_readindex)=j_readvec(j_keepindex) !(1:nkeep(1))
							call putlinked()
						enddo !j_iobs=1,nobs  21735
 
					endif !if(isnobs)  21725
 
				else
					if(isnobs)then
						do j_iobs=1,nobs
							read(nu,form(1:lenform),err=957,end=958)j_readvec  !(1:nkeep(1))
							j_o(ivmat)%d(ibas+1:ibas+nkeep0)=j_readvec(j_keepindex)  !(1:nkeep(1))
							ibas=ibas+nkeep
						enddo !j_iobs=1,nobs  21747
						j_iobs=nobs;return
					else !j_iobss
 
						do j_iobs=1,nobs
							read(nu,form(1:lenform),err=957,end=958)j_readvec
							! write(6,*)j_readvec
							! write(6,*)'keepi',keepindex
							j_keepvec(1:nkeep0)=j_readvec(j_keepindex) !(1:nkeep(1))
							call putlinked()
						enddo !j_iobs=1,nobs  21755
 
					endif !if(isnobs)  21746
				endif !if(isreadindex)  21724
			else !iskeep
				if(isnobs)then
					do j_iobs=1,nobs
						read(nu,form(1:lenform),err=957,end=958)j_o(ivmat)%d(ibas+1:ibas+nkeep0)
						ibas=ibas+nkeep
					enddo !j_iobs=1,nobs  21767
					j_iobs=nobs;return
				else !j_iobss
					!write(6,*)'ncncnc',nobs
					do j_iobs=1,nobs
 
						read(nu,form(1:lenform),err=957,end=958)j_keepvec(1:nkeep0) !(1:nkeep(1))
						!write(6,*)'j_iobs',j_iobs,j_keepvec
						call putlinked()
					enddo !j_iobs=1,nobs  21774
 
				endif !if(isnobs)  21766
 
			endif !if(iskeep)  21723
 
 
 
		else !update later
			write(6,*)'format not done'
			j_err=.true.;return
 
 
		endif !if(ivform.eq.j_ivb)  21512
		!	j_iobs=iob
 
		return
 
 
	957		write(6,*)'*error reading in-> file after reading ',j_iobs-1,' observations'

		j_err=.true.
		!  958		call j_closeunit(nu)
958 		j_iobs=j_iobs-1
		!if(j_isgetinp)call j_exitpara()
	end subroutine fast
 
 
	subroutine subv()
		!istrans.and.isnobs
		write(6,*)'****subv' !	,isobs,ivform,iskeep,nu',isobs,ivform,iskeep,nu
		ibas=0
		j_iobs=0
		if(nu.eq.0.and..false.)then
 
			do ire=1,nobs
				call j_getinput(' ')
 
				read(j_inp(1:j_linp),*,err=5,end=5)j_readvec
				j_v(j_readv)=j_readvec
				!	write(6,*)'r',j_readvec
				call transu()
				if(j_err)return
				if(j_rejected)cycle
				j_iobs=j_iobs+1
				j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep(1:nkeep))  !(1:nkeep(1))
				!		write(6,*)'aft',j_v(j_keep(1:nkeep0))
 
				ibas=ibas+nkeep
			enddo !ire=1,nobs  21814
			call j_getinput(' ')  !/
			!	j_iobs=nobs
			return
	5 write(6,*)'error in line ',j_iobs,' ',j_inp(1:j_linp)
			j_err=.true.;return
 
		elseif(ivform.eq.j_ivb)then
 
			do ire=1,nobs
				read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1))
				j_v(j_readv)=j_readvecsing
				call transu()
				if(j_err)return
				if(j_rejected)cycle
				j_iobs=j_iobs+1
				j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep(1:nkeep))  !(1:nkeep(1))
				ibas=ibas+nkeep
			enddo !ire=1,nobs  21837
			return
		elseif(ivform.eq.j_ivb2)then
 
			do ire=1,nobs
				read(nu,err=957,end=958)j_readvec  !(1:nkeep(1))
				j_v(j_readv)=j_readvec
				call transu()
				if(j_err)return
				if(j_rejected)cycle
				j_iobs=j_iobs+1
				j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep(1:nkeep))  !(1:nkeep(1))
				ibas=ibas+nkeep
			enddo !ire=1,nobs  21850
			return
 
		else if(ivform.eq.j_ivdollar)then
 
			do ire=1,nobs
				call j_readvector(nu,ivform,j_readvec,nread)
				if(j_err)goto 957
				!		if(j_end)goto 958
				!	read(nu,*,err=957,end=958)j_readvec  !(1:nkeep(1))
				j_v(j_readv)=j_readvec
				call transu()
				if(j_err)return
				if(j_rejected)cycle
				j_iobs=j_iobs+1
				j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep(1:nkeep))   !(1:nkeep(1))
				ibas=ibas+nkeep
			enddo !ire=1,nobs  21864
			!if(j_isgetinp)call j_exitpara()
			return
		else if(isff)then
 
			do ire=1,nobs
				read(nu,form(1:lenform),err=957,end=958)j_readvec  !(1:nkeep(1))
				j_v(j_readv)=j_readvec
				call transu()
				if(j_err)return
				if(j_rejected)cycle
				j_iobs=j_iobs+1
				j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep(1:nkeep))  !(1:nkeep(1))
				ibas=ibas+nkeep
			enddo !ire=1,nobs  21881
			return
 
		else !update later
			write(6,*)'not done'
			j_err=.true.;return
 
 
		endif !if(nu.eq.0.and..false.)  21812
 
 
		return
 
		j_err=.true.;return
	957		write(6,*)'*error reading in-> file after reading ',ire-1,' observations'
		j_err=.true.
958		return  !	if(j_isgetinp)call j_exitpara()
	!	return
	end subroutine subv
 
 
	subroutine upv()
		!up.and.istrans
		write(6,*)'*****upv' !,isobs,ivform,iskeep,nu,istrans',isnobs,ivform,iskeep,nu,istrans,' ivup ',ivup!no linked lists no keep isobs
		ibas=0
 
		j_iobs=0
		irecord=0
		!		write(6,*)'isnobs,nobs(1),ivform(1),j_ivdollar',isnobs,nobs(1),ivform(1),j_ivdollar
		if(nu.eq.0.and..false.)then
			!write(6,*)'obshsh',nobsup,nkeepup
			do iu=1,nobsup
				!			write(6,*)'ibasupta',ibasup
				nobsw=j_dmatup(ibasup+1)
				j_v(j_keepup)=j_dmatup(ibasup+1:ibasup+nkeepup)
				j_v(ivobsup)=iu
				!		write(6,*)'iu,nobsw',iu,nobsw
				ibsw=0
				do ibs=1,nobsw
					j_v(ivobsw)=ibs
					j_v(ivobsw0)=ibs
					call j_getname(ivobsw)
					!	write(6,*)'hhh',j_oname(1:j_loname),ibs,ivobsw
					call j_getinput(' ')
 
 
					!		write(6,*)'inp',ibs, j_inp(1:j_linp),j_iobs
					read(j_inp(1:j_linp),*,err=5,end=5)j_readvec
					j_v(j_readv)=j_readvec
					!write(6,*)'r',j_readvec
					call transu()
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					if(isnobs)then
						j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_keepvec  !j_v(j_keep)  !(1:nkeep(1))
						ibas=ibas+nkeep
					else
						call putlinked()
 
					endif !if(isnobs)  21944
					!write(6,*)'ibas,nkeep,j_iobs,',ibas,nkeep,j_iobs,j_keepvec
				enddo !ibs=1,nobsw  21928
				!write(6,*)'ibasup ibasup+nkeepup,nkeepup',ibasup,ibasup+nkeepup,nkeepup
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  21921
			!write(6,*)'inpfin',ivup,iout,ibs, j_inp(1:j_linp),'j_iobs ',j_iobs
			call j_getinput(' ')  !/
			!call j_linkdata(ivup,ivsub)
			return
	5 write(6,*)'error in line ',j_iobs,' ',j_inp(1:j_linp)
			call j_exitpara()
			j_err=.true.;return
 
		elseif(ivform.eq.j_ivb)then
 
 
			do iu=1,nobsup
				nobsw=j_dmatup(ibasup+1)
				j_v(j_keepup)=j_dmatup(ibasup+1:ibasup+nkeepup)
				j_v(ivobsup)=iu
				ibsw=0
				do ibs=1,nobsw
					j_v(ivobsw)=ibs
					j_v(ivobsw0)=ibs
					read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1))
 
					j_v(j_readv)=j_readvecsing
					call transu()
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					if(isnobs)then
						j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep)  !(1:nkeep(1))
						ibas=ibas+nkeep
					else
						call putlinked()
					endif !if(isnobs)  21982
				enddo !ibs=1,nobsw  21972
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  21967
			!		return
		elseif(ivform.eq.j_ivb2)then
 
			do iu=1,nobsup
				nobsw=j_dmatup(ibasup+1)
				j_v(j_keepup)=j_dmatup(ibasup+1:ibasup+nkeepup)
				j_v(ivobsup)=iu
				ibsw=0
				do ibs=1,nobsw
					j_v(ivobsw)=ibs
					j_v(ivobsw0)=ibs
					if(isdupl)j_v(ivoldobsw)=ibs
					read(nu,err=957,end=958)j_readvec  !(1:nkeep(1))
					j_v(j_readv)=j_readvec
					call transu()
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					if(isnobs)then
						j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep)  !(1:nkeep(1))
						ibas=ibas+nkeep
					else
						call putlinked()
					endif !if(isnobs)  22009
				enddo !ibs=1,nobsw  21999
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  21994
			!		return
		else if(ivform.eq.j_ivdollar)then
 
			do iu=1,nobsup
				nobsw=j_dmatup(ibasup+1)
				!	write(6,*)'iu,nobsup,nobsw',iu,nobsup,nobsw
				j_v(ivobsup)=iu
				ibsw=0
				do ibs=1,nobsw
					!			write(6,*)'ibs',ibs
					if(isdupl)j_v(ivoldobsw)=ibs
					j_v(ivobsw)=ibs
					j_v(ivobsw0)=ibs
					call j_readvector(nu,ivform,j_readvec,nread)
					if(j_err)goto 957
					!		if(j_end)goto 958
					!	read(nu,*,err=957,end=958)j_readvec  !(1:nkeep(1))
					!			write(6,*)'readv ',j_readvec
					j_v(j_readv)=j_readvec
					call transu()
					if(j_err)return
 
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					if(isnobs)then
						j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep)  !(1:nkeep(1))
						ibas=ibas+nkeep
					else
						call putlinked()
					endif !if(isnobs)  22042
				enddo !ibs=1,nobsw  22026
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  22021
			!	if(j_isgetinp)call j_exitpara()
		else if(isff)then
 
			do iu=1,nobsup
				nobsw=j_dmatup(ibasup+1)
				!		write(6,*)'iu,nobsup,nobsw',iu,nobsup,nobsw
				j_v(ivobsup)=iu
				ibsw=0
				do ibs=1,nobsw
					!			write(6,*)'ibs',ibs
					if(isdupl)j_v(ivoldobsw)=ibs
					j_v(ivobsw)=ibs
					j_v(ivobsw0)=ibs
					read(nu,form(1:lenform),err=957,end=958)j_readvec  !(1:nkeep(1))
					!			write(6,*)'readv ',j_readvec
					j_v(j_readv)=j_readvec
					call transu()
					if(j_err)return
 
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					if(isnobs)then
						j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep)  !(1:nkeep(1))
						ibas=ibas+nkeep
					else
						call putlinked()
					endif !if(isnobs)  22072
				enddo !ibs=1,nobsw  22059
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  22054
 
			!		return
		elseif(isgaya)then
 
			do iu=1,nobsup
				nobsw=j_dmatup(ibasup+1)
				!		write(6,*)'iu,nobsup,nobsw',iu,nobsup,nobsw
				j_v(ivobsup)=iu
 
				do ibs=1,nobsw
					!write(6,*)'ibs',ibs
					if(isdupl)j_v(ivoldobsw)=ibs
					j_v(ivobsw)=ibs
					j_v(ivobsw0)=ibs
					!		j_v(j_readv)=j_g_var(1:j_nread)
					call j_gayax(nu,ibs)
					if(istrans)then
						call transu()
						if(j_err)return
 
						if(j_rejected)cycle
					endif !if(istrans)  22097
					j_iobs=j_iobs+1
					if(isnobs)then
						j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep)  !(1:nkeep(1))
						ibas=ibas+nkeep
					else
						if(.not.istrans)j_keepvec=j_v(j_keep)
						call putlinked()  !gaya
					endif !if(isnobs)  22104
				enddo !ibs=1,nobsw  22090
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  22085
			!	write(6,*)'nduplsum',nduplsum
 
		else !update later
			write(6,*)'format not yet '
			j_err=.true.;return
 
 
		endif !if(nu.eq.0.and..false.)  21919
 
		!	j_iobs=
 
		return
 
		j_err=.true.;return
	957		write(6,*)'*error reading in-> file after reading ',j_iobs-1,' observations'
		j_err=.true.
958	 return  !	if(j_isgetinp)call j_exitpara()
	!	return
	end subroutine upv
 
	subroutine fastup()
		!up.and.istrans
		write(6,*)'*****fastup' !,isobs,ivform,iskeep,nu,istrans',isnobs,ivform,iskeep,nu,istrans,' ivup ',ivup!no linked lists no keep isobs
		ibas=0
 
		j_iobs=0
 
		!		write(6,*)'isnobs,nobs(1),ivform(1),j_ivdollar',isnobs,nobs(1),ivform(1),j_ivdollar
		if(nu.eq.0.and..false.)then
			!write(6,*)'obshsh',nobsup,nkeepup
			do iu=1,nobsup
				!			write(6,*)'ibasupta',ibasup
				nobsw=j_dmatup(ibasup+1)
 
				do ibs=1,nobsw
 
					!	write(6,*)'hhh',j_oname(1:j_loname),ibs,ivobsw
					call j_getinput(' ')
 
 
					!		write(6,*)'inp',ibs, j_inp(1:j_linp),j_iobs
					read(j_inp(1:j_linp),*,err=5,end=5)j_o(ivmat)%d(ibas+1:ibas+nkeep0)
					ibas=ibas+nkeep
					j_o(ivmat)%d(ibas)=ibs
					!write(6,*)'ibas,nkeep,j_iobs,',ibas,nkeep,j_iobs,j_keepvec
				enddo !ibs=1,nobsw  22148
				!write(6,*)'ibasup ibasup+nkeepup,nkeepup',ibasup,ibasup+nkeepup,nkeepup
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  22144
			j_iobs=j_iobs+nobsw
			!write(6,*)'inpfin',ivup,iout,ibs, j_inp(1:j_linp),'j_iobs ',j_iobs
			call j_getinput(' ')  !/
			!call j_linkdata(ivup,ivsub)
			return
	5 write(6,*)'error in line ',j_iobs,' ',j_inp(1:j_linp)
	!		call j_exitpara()
			j_err=.true.;return
 
		elseif(ivform.eq.j_ivb)then
 
 
			do iu=1,nobsup
				nobsw=j_dmatup(ibasup+1)
 
				do ibs=1,nobsw
 
					read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1))
 
					j_o(ivmat)%d(ibas+1:ibas+nkeep0)=j_readvecsing  !(1:nkeep(1))
 
					ibas=ibas+nkeep
					j_o(ivmat)%d(ibas)=ibs
 
				enddo !ibs=1,nobsw  22178
				j_iobs=j_iobs+nobsw
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  22175
			!		return
		elseif(ivform.eq.j_ivb2)then
 
			do iu=1,nobsup
				nobsw=j_dmatup(ibasup+1)
 
				do ibs=1,nobsw
 
					read(nu,err=957,end=958)j_o(ivmat)%d(ibas+1:ibas+nkeep0)
 
					ibas=ibas+nkeep
					j_o(ivmat)%d(ibas)=ibs
 
				enddo !ibs=1,nobsw  22197
				j_iobs=j_iobs+nobsw
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  22194
			!		return
		else if(ivform.eq.j_ivdollar)then
 
			do iu=1,nobsup
				nobsw=j_dmatup(ibasup+1)
				!	write(6,*)'iu,nobsup,nobsw',iu,nobsup,nobsw
 
				do ibs=1,nobsw
					!			write(6,*)'ibs',ibs
					call j_readvector(nu,ivform,j_o(ivmat)%d(ibas+1:ibas+nkeep0),nread)
					if(j_err)goto 957
					if(j_end)goto 958
					!		read(nu,*,err=957,end=958)j_o(ivmat)%d(ibas+1:ibas+nkeep0) !(1:nkeep(1))
					!			write(6,*)'readv ',j_readvec
 
					ibas=ibas+nkeep
					j_o(ivmat)%d(ibas)=ibs
 
 
				enddo !ibs=1,nobsw  22215
				j_iobs=j_iobs+nobsw
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  22211
		else if(isff)then
 
			do iu=1,nobsup
				nobsw=j_dmatup(ibasup+1)
				!		write(6,*)'iu,nobsup,nobsw',iu,nobsup,nobsw
 
				do ibs=1,nobsw
					!			write(6,*)'ibs',ibs
 
					read(nu,form(1:lenform),err=957,end=958)j_o(ivmat)%d(ibas+1:ibas+nkeep0)  !(1:nkeep(1))
					!			write(6,*)'readv ',j_readvec
 
					ibas=ibas+nkeep
					j_o(ivmat)%d(ibas)=ibs
 
				enddo !ibs=1,nobsw  22237
				j_iobs=j_iobs+nobsw
				ibasup=ibasup+nkeepup
			enddo !iu=1,nobsup  22233
 
			!		return
 
 
		else !update later
			write(6,*)'format not yet '
			j_err=.true.;return
 
 
		endif !if(nu.eq.0.and..false.)  22142
 
		!	j_iobs=
 
		return
 
		j_err=.true.;return
	957		write(6,*)'*error reading in-> file after reading ',j_iobs-1,' observations'
		j_err=.true.
958		return ! if(j_isgetinp)call j_exitpara()
	!return
	end subroutine fastup
 
	subroutine v()
		write(6,*)'*****v' !,isnobs,nobs,ivform,iskeep,nu',isnobs,nobs,ivform,iskeep,nu,'istrans',istrans,'nkeep',nkeep
		! NOT
		! if(.not.istrans.and..not.isgaya)then !.and.isnobs)then
		! call fast() !can read directly to the matrix  without v-vector keep/nokeep isnobs/noisnobs
		! elseif(up.and.istrans.or.isgaya)then   !
		! call upv()  !inclduning gaya
		! elseif(istrans.and.isnobs)then
		! call subv()
		! else
 
 
		ibas=0
 
 
		if(ivform.eq.j_ivb)then
			if(isnobs)then
				do ibs=1,nobs
					read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1))
					j_v(j_readv)=j_readvecsing
					call transu()
 
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep)  !(1:nkeep(1))
					ibas=ibas+nkeep
				enddo !ibs=1,nobs  22289
				return
			else
				do ibs=1,nobs
					read(nu,err=957,end=958)j_readvecsing  !(1:nkeep(1)
					j_v(j_readv)=j_readvecsing
					call transu()
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					j_keepvec=j_v(j_keep)
					call putlinked()
				enddo !ibs=1,nobs  22302
 
 
 
			endif !if(isnobs)  22288
 
		elseif(ivform.eq.j_ivb2)then
			if(isnobs)then
				do ibs=1,nobs
					read(nu,err=957,end=958)j_readvec
					j_v(j_readv)=j_readvec  !(1:nkeep(1))
					call transu()
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep)  !(1:nkeep(1))
					ibas=ibas+nkeep
				enddo !ibs=1,nobs  22319
				return
			else
				do ibs=1,nobs
					read(nu,err=957,end=958)j_readvec  !(1:nkeep(1)
					j_v(j_readv)=j_readvec
					call transu()
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					j_keepvec=j_v(j_keep)
					call putlinked()
				enddo !ibs=1,nobs  22331
 
 
 
 
			endif !if(isnobs)  22318
 
		else if(ivform.eq.j_ivdollar)then
			if(isnobs)then
				do ibs=1,nobs
					call j_readvector(nu,ivform,j_readvec,nread)
					if(j_err)goto 957
					if(j_end)goto 958
					j_v(j_readv)=j_readvec
					!		read(nu,*,err=957
 
					!	read(nu,*,err=957,end=958)j_v(j_readv)  !(1:nkeep(1))
					call transu()
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep)  !(1:nkeep(1))
					ibas=ibas+nkeep
				enddo !ibs=1,nobs  22349
				return
			else
				do ibs=1,nobs
					call j_readvector(nu,ivform,j_readvec,nread)
					if(j_err)goto 957
					if(j_end)goto 958
 
 
					!	read(nu,*,err=957,end=958)j_readvec  !(1:nkeep(1)
					!	if(j_iobs.eq.1)write(6,*)'rv',j_readvec
					!	if(j_iobs.eq.1)write(6,*)'nkeep,nkeep0',nkeep,nkeep0
					j_v(j_readv)=j_readvec
					call transu()
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					j_keepvec=j_v(j_keep)
					!	if(j_iobs.eq.1)write(6,*)'kvec ',j_keepvec(1:nkeep0)
					call putlinked()
				enddo !ibs=1,nobs  22366
 
 
			endif !if(isnobs)  22348
		else if(isff)then
			if(isnobs)then
				do ibs=1,nobs
					read(nu,form(1:lenform),err=957,end=958)j_v(j_readv)  !(1:nkeep(1))
					call transu()
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_keep)  !(1:nkeep(1))
					ibas=ibas+nkeep
				enddo !ibs=1,nobs  22389
				return
			else
				do ibs=1,nobs
					read(nu,form(1:lenform),err=957,end=958)j_readvec  !(1:nkeep(1)
					j_v(j_readv)=j_readvec
 
					call transu()
					!write(6,*)'ibs',ibs,'rej',j_rejected
					if(j_err)return
					if(j_rejected)cycle
					j_iobs=j_iobs+1
					j_keepvec=j_v(j_keep)
					call putlinked()
				enddo !ibs=1,nobs  22400
 
 
			endif !if(isnobs)  22388
 
		else !update later
			write(6,*)'format not yet '
			j_err=.true.;return
 
 
		endif !if(ivform.eq.j_ivb)  22287
		!	j_iobs=ib
 
		return
 
		j_err=.true.;return
	957		write(6,*)'*error reading in-> file after reading ',ibs-1,' observations'
		j_err=.true.
958		return !	if(j_isgetinp)call j_exitpara()
		!return
	end subroutine v
 
 
 
 
 
	subroutine transu()
		!write(6,*)'transu'
		irecord=irecord+1
		j_v(j_ivrecord)=irecord
		j_v(ivobs)=j_iobs+1
		j_v(j_ivobs)=j_v(ivobs)
		!write(6,*)'ismaketrans,ivmaketrans,isfilter,isreject',ismaketrans,ivmaketrans,isfilter,isreject
		if(ismaketrans)then
			!write(6,*)j_o(ivmaketrans)%i(1:40)
			call dotrans(ivmaketrans,1)
			!	if(id.eq.1)write(6,*)'<9999 ',j_readv(1:j_nread)
			if(j_err)then
				write(6,*)'error in making maketrans for observation ',j_iobs
				if(irecord.ne.j_iobs)write(6,*)'record ',irecord
				return
			endif !if(j_err)  22448
			j_keepvec=j_v(j_keep)
		end if !if(ismaketrans)  22444
		!write(6,*)'<88',isfilter,isreject,filterlink,rejectlink
		j_rejected=.false.
		if(isfilter)then
			!	call dotrans(iobcur_,j_iofilter)
			if(j_codevalue(iob,filterlink).eq.j_0)j_rejected=.true. !j_v(j_ivfilter).eq.0.)then
			!	write(6,*)'filte ',j_codevalue(iob,filterlink)
			if(j_err)then
				write(6,*)'error in filter-> for obs ',j_iobs
				if(irecord.ne.j_iobs)write(6,*)'record ',irecord
				return
			endif !if(j_err)  22461
			!end if !if(j_v(j_ivfilter).eq.0.)then
		end if !if(isfilter)  22457
		if(isreject)then
			!		write(6,*)'firejte ',j_codevalue(iob,rejectlink)
			if(j_codevalue(iob,rejectlink).ne.j_0)j_rejected=.true.
			!	call dotrans(iobcur_,j_ioreject)
			if(j_err)then
				write(6,*)'error in reject-> for obs ',j_iobs
				if(irecord.ne.j_iobs)write(6,*)'*record ',irecord
				return
			endif !if(j_err)  22472
 
		end if !if(isreject)  22468
		!write(6,*)'isreject,isfilter,j_rejected ',isreject,isfilter,j_rejected
		if(j_rejected)then
			nrejected=nrejected+1
			!	write(6,*)'rej,record ',irecord,nrejected
			return
		endif !if(j_rejected)  22480
		if(isdupl)then
 
			j_v(ivoldobsw)=ibs
			j_v(ivoldobs)=irecord
			!	write(6,*)'ibs,irec',ibs,irecord
			call dotrans(ivtransndupl,1)
			if(j_err)then
				write(6,*)'error when computing Duplicates for obs ',j_iobs
				if(irecord.ne.j_iobs)write(6,*)'*record ',irecord
				return
			endif !if(j_err)  22491
			ndupl=j_v(ivndupl)
			!	write(6,*)'ndupl',ndupl
			nduplsum=nduplsum+ndupl
			j_dmatup(ibasup+1)=j_dmatup(ibasup+1)+ndupl
			!	write(6,*)'ndupl ',ndupl,j_dmatup(ibasup+1),ibasup,'updat',j_dmatup
			do id=0,ndupl
				j_v(ivduplicate)=id
				ibsw=ibsw+1
				j_v(ivobsw)=ibsw
				call dotrans(ivtransdupl,1)
				if(j_err)then
					call j_getname(ivtransdupl)
					write(6,*)'error when computing ',j_oname(1:j_loname),' for obs ',j_iobs, &
						' and Duplicate ',id
					if(irecord.ne.j_iobs)write(6,*)'*record ',irecord
					return
				endif !if(j_err)  22506
				if(id.lt.ndupl)then
					!	write(6,*)'dupl ,ndupl,irecord,j_iobs ',ndupl,irecord,j_iobs
 
					j_iobs=j_iobs+1
					j_keepvec=j_v(j_keep)  !(1:nkeep(1))
					call putlinked()
 
					j_v(ivobs)=j_iobs+1
					j_v(j_ivobs)=j_v(ivobs)
 
				endif !if(id.lt.ndupl)  22513
			enddo !id=0,ndupl  22501
 
			j_keepvec=j_v(j_keep)  !(1:nkeep(1))
			!write(6,*)'j_iobs',j_iobs,j_keepvec
		endif !if(isdupl)  22485
 
	end subroutine transu
 
 
 
end subroutine data !subroutine data(iob,io)
 
 
subroutine stat_(iob,io)  !subroutine stat(iob,io)
	!Section stat stat() Basic statistics in DATA
	! Computes and prints basic statistics from data objects.
	!endheader
	!Option
	!Output&0-1&REAL& kokopo
	! Args & 0-99&REAL&variables for which the statistics are computed,
	! the default is all variables in the data (all variables in the data matrix plus the output variables of the associated transformation object) and all output
 
	!@@data@
	! data & -1,99 & Data &	data objects , see section Common options for default! weight 	gives the weight of each observations if weighted means and variances ar
	! transformation or it can be a variable in the data object
	!@@seecom@
	!missing&-1|1&REAL& With missing->, values greater or 1.7e19 are interpred as missing values.
	! min & -1,99&REAL&	defines to which variables the minima are stored.
	!   If the value is character constant or character variable,
	!   then the name is formed by concatenating the character with the name of the argument
	! variable. E.g. stat(x1,x2,min->'%pien') stores minimums into variables
	! x1%pien and x2%pien. The default value for min  is '%min'.
	!If the values of the min-> option are variables,
	! then the minima are stored into these variables.
	!minobs &N|0&& Observation where minimum is obtained is stored in var%minobs
	! max &-1,99&REAL& maxima are stored, works as min->
	!maxobs &N|0&& Observation where maximum is obtained is stored in var%maxobs
	! mean &-1,99&REAL & means are stored
	! var &-1,99&REAL& variances are stored
	! sd &-1,99&REAL& standard deviations are stored
	! sum &-1,99&REAL&	sums are stored, (note that sums are not printed automatically)
	! nobs &-1 | 1&REAL&	gives variable which will get the number of accepted observations, default is variable 'Nnobs'. If all observations are rejected due to fi
	! trans &-1 | 1&TRANS&	transformation object which is executed for each observation. If there is a transformation object associated with the data object, those
	! filter &-1 | 1&Code& logical or arithmetic statement (nonzero value indicating True) describing which observations will be accepted. trans-> transformations
	! reject &-1 | 1&Code&
	! transafter &-1 | 1& TRANS& transformation object which is executed for each observation which has passed the filter and is not rejected by the reject->-optio
	!func&-1|1&CODE& Defines function for whose values the statistics are computed.
	!	Statistics are not computed for other variables in the data.
	!endoption
	! Note  stat() function prints min, max, means, sd and sd of the mean computed
	! as sd/sqrt(number of observations)
	!endnote
	! Note If the value of a variable is greater than or equal to 1.7e19,
	! then that observation is rejected when computing statistics for that variable, if option
	!missing-> is present.
	!endnote
	!Note When making several analyses with several transformations, it is often convenient
	! to define a basic TRANS object trans0, and define for each special analyses
	! a TRANS object transcur, where the first line is call(trans0) and which is used as
	!the argument in trans->. It is easier to handle
	! short TRANS objects.
	!endnote
	!Note If a Jlp22 function used in tranformation generates MATRIX objects, then these vectors
	! can be made ordinary data variables using lists. Defing that veclist=list(e1...e6),
	!then a vector Evec defined inside a Jlp22 function as a row or column vector can be
	! accessed as data variables using veclist=Evec. It is possible to acces also matrices by
	! remembering that matrices are stored in row order.
	!endnote
	!Ex statex stat() computes minimums, maximums, means and std deviatons
	!data1=data(in->,read->(x1,x2,x3))
	!1,2,3
	!4,6,8
	!3,8,20
	!6,8,9
	!/
	! stat()
	! stat(data->data1,sum->x2,mean->,filter->(x3.le.18.5))
	! li=;list(x2%?);
	! @li;
	! stat(x1,data->data1,weight->x2)
	! stat(x1,weight->(x2**1.2))
	!endex
	!endsection
 
 
	integer,dimension(:), allocatable::minobs,maxobs
	double precision ,dimension(:), allocatable::xmin_
	double precision,dimension(:), allocatable::xmax_
	double precision ,dimension(:), allocatable::xs
	double precision ,dimension(:), allocatable::xss
	double precision ,dimension(:), allocatable::sumwt
	double precision ,dimension(:), allocatable::summa,x
	double precision haj,cov,cor,weisu
	double precision xws,xw,xh
	logical weig,weigt,rejo,missing,istime !filter, single
	logical isfunc
 
	integer,dimension(:),pointer::arg
 
	!double precision, dimension(2):: xh
 
	!	integer,dimension(:),pointer :: arg2  !all
	logical::isup,istrans,isprefix
	integer*8 ::nobs !iobs
	!	write(6,*)'stat'
	!ubroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout) ! %%function
	call j_startfunction(iob,io,j_ipreal,narg,arg,iout);if(j_err)return
 
 
 
	if(j_err)return
 
	call j_getdataobject(iob,io)
 
	if(j_v(j_ivprintinput).gt.0.and.j_dprint.ne.0)then
		call j_getname(-1,-1,j_divdata)
		write(6,*)j_oname3(1:j_loname3)
	endif !if(j_v(j_ivprintinput).gt.0.and.j_dprint.ne.0)  22638
	!	write(6,*)'j_divdata,keep,nkeep',j_divdata,j_o(j_divdata)%i(2),j_o( j_o(j_divdata)%i(2))%i(1)
	! write(6,*)'idata1',j_divdata,j_dnobs,j_dfilterlink,j_drejectlink,j_divtrans,j_divvars,j_divmat
	! write(6,*)j_dnkeep,j_divkeep,j_divmatup,j_divkeepup,j_dnkeepup,j_divnobsw
	! write(6,*)j_divobsup,j_dnextobs,j_diba,j_dibaup,j_divobsw,j_divobs,j_diob
	! write(6,*)j_disup,j_distrans,j_disreject,j_disfilter
	isprefix=j_isoption(iob,io,j_mprefix)
	!	isminobs=j_isoption(iob,io,j_mminobs)
	!	isminloc=j_isoption(iob,io,j_mminloc)
	!	ismaxloc=j_isoption(iob,io,j_mmaxloc)
 
	!	ismaxobs=j_isoption(iob,io,j_mmaxobs)
	if(j_err)return
	missing=j_isoption(iob,io,j_mmissing)
	istime=j_isoption(iob,io,j_mtime)
	linkfunc=j_codelink(iob,io,j_mfunc); if(j_err)return
	isfunc=linkfunc.ne.0
	if(istime)then
		call cpu_time(cpu0)
		time0=secnds(0.)
	endif !if(istime)  22658
	nobs=j_dnobs8
	!write(6,*)'nobs ',nobs
	! iprint=1
	! if(j_igetopt(iob,io,j_mprint).gt.0)iprint=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mprint)+1) )
 
 
	! if(ivnobs.eq.0) then
	! call j_getobject(0,'Nobs',j_ipreal,ivnobs)
	! if(j_err) return
	! endif !if(ivnobs.eq.0) then
	liminobs=j_linkoption(iob,io,j_mminobs)
	limaxobs=j_linkoption(iob,io,j_mmaxobs)
	!write(6,*)'liminobs,limaxobs',liminobs,limaxobs
	limean=j_linkoption(iob,io,j_mmean)
	livar=j_linkoption(iob,io,j_mvar)
	lisd=j_linkoption(iob,io,j_msd)
	lisdmean=j_linkoption(iob,io,j_msdmean)
	limin=j_linkoption(iob,io,j_mmin)
	limax=j_linkoption(iob,io,j_mmax)
	lisum=j_linkoption(iob,io,j_msum)
	ligot=j_linkoption(iob,io,j_mgot)
	lirmse=j_linkoption(iob,io,j_mrmse)
	!write(6,*)'ligot',limean,limin,limax
	linkweight=j_codelink(iob,io,j_mweight); if(j_err)return
	weig=linkweight.ne.0
	! if(j_linkoption(iob,io,j_mweight).gt.0)then
	! weig=.true.
	! ioweight=j_linkopt2(j_mweight)
	! ivweight=j_o(iob)%i(j_linkoption(iob,io,j_mweight)+1)
	! weigt=j_o(iob)%i(ioweight).ne.0
	! else !if(j_linkoption(iob,io,j_mweight).gt.0)then
	! weig=.false.
	! end if !if(j_linkoption(iob,io,j_mweight).gt.0)then
	!!call j_clearoption(iob,io)  ! subroutine
	if(narg.eq.0.and..not.isfunc)then
		narg=j_o(j_divvars)%i(1)
		arg=>j_o(j_divvars)%i2(1:narg)
	endif !if(narg.eq.0.and..not.isfunc)  22696
	if(isfunc)narg=1
	if(liminobs.ge.0)allocate(minobs(1:narg))
	if(limaxobs.ge.0)allocate(maxobs(1:narg))
	xw=1.d0  !weight
	xws=j_0
	j_iobs=0
	ntot=0
	! do k=1,jndatasetss
	! !	write(6,*)'<12',k,j_datasets(k)
	! !call j_getdataset(j_datasets(k),nobs)
	! !write(6,*)'<127',nobs,'narg',narg,narg2,arg2,narg2
	! if(k.eq.1)then
	! if(narg.le.0)then
	! call j_alldatavars(arg2,narg2)
	! !write(6,*)'<67>narg,arg2',narg2,arg2
	! else !if(narg.le.0)then
	! narg2=narg
	! allocate(arg2(1:narg2))
	! arg2=j_o(iob)%i(io+2:io+1+narg)
	! endif !if(narg.le.0)then
	allocate(xs(1:narg),xss(1:narg),sumwt(1:narg),xmin_(1:narg),xmax_(1:narg))
	allocate(summa(1:narg))
	if(.not.missing)allocate(x(1:narg))
	xs=j_0;xss=j_0;xmin_=1.7d37;xmax_=-1.7d37;sumwt=j_0
	summa=j_0
	!	write(6,*)'<66666narg,from,until',narg,j_dfrom,j_duntil
	if(missing)then
		!		allocate(x(1:narg))
		do j_iobs=j_dfrom,j_duntil
 
			call j_getobs(j_iobs)
			j_v(j_ivobs)=j_iobs
 
			!	!call j_nextobs()
			!	write(6,*)'<13',i,'*',j_v(arg)
			if(j_err)return
			if(j_rejected)cycle
			ntot=ntot+1
			if(weig)then
				!if(weigt) call dotrans(iob,ioweight)
				xw=j_codevalue(iob,linkweight) !j_v(ivweight)
				if(j_err)return
			endif !if(weig)  22738
			xws=xws+xw
			!			write(6,*)'<334arg',j_v(arg)
 
			do j=1,narg
 
				if(isfunc)then
					xh=j_codevalue(iob,linkfunc)
				else
					xh=j_v(arg(j))  !j_v(j_o(iob)%i(io+1+j))
				endif !if(isfunc)  22748
				!			write(27,*)j_iobs,j,xh
				if(abs(xh).lt.1.7e18)then
					call j_msd21(xh,xs(j),xss(j),xw,sumwt(j),summa(j))
					if(xh.lt.xmin_(j))then
						xmin_(j)=xh
						if(liminobs.ge.0)minobs(j)=j_iobs
					endif !if(xh.lt.xmin_(j))  22756
					!	xmin_(j)=min(xmin_(j),xh)
					!	xmax_(j)=max(xmax_(j),xh)
					if(xh.gt.xmax_(j))then
						xmax_(j)=xh
						if(limaxobs.ge.0)maxobs(j)=j_iobs
					endif !if(xh.gt.xmax_(j))  22762
				end if !if(abs(xh).lt.1.7e18)  22754
			end do !j=1,narg  22746
		end do !j_iobs=j_dfrom,j_duntil  22728
	else !missing
		!write(6,*)'j_dfrom,j_duntil ',j_dfrom,j_duntil
		do j_iobs=j_dfrom,j_duntil
 
			call j_getobs(j_iobs)
			!	write(6,*)'<66',j_iobs,j_rejected
			!	!call j_nextobs()
			!	write(6,*)'<13',i,'*',j_v(arg)
			if(j_err)return
			if(j_rejected)cycle
			ntot=ntot+1
			if(weig)then
				!if(weigt) call dotrans(iob,ioweight)
				xw=j_codevalue(iob,linkweight) !j_v(ivweight)
				if(j_err)return
			endif !if(weig)  22780
			!		xws=xws+xw
			!			write(6,*)'<334arg',j_v(arg)
			if(isfunc)then
 
				x=j_codevalue(iob,linkfunc)
			else
				x=j_v(arg)
			endif !if(isfunc)  22787
			call j_msd2(narg,x,xs,xss,xw,xws,summa)
			xmin_=min(xmin_,x)
			xmax_=max(xmax_,x)
			!write(6,*)'limx',liminobs,limaxobs,xmin_,xmax_,x
			if(liminobs.ge.0)then
				do j=1,narg
					!			write(6,*)'jj',j,x(j),xmin_(j),j,x(j).eq.xmin_(j)
					if(x(j).eq.xmin_(j))minobs(j)=j_iobs
				enddo !j=1,narg  22798
			endif !if(liminobs.ge.0)  22797
			if(limaxobs.ge.0)then
				do j=1,narg
					!			write(6,*)'jj',j,x(j),xmax_(j),j,x(j).eq.xmax_(j)
					if(x(j).eq.xmax_(j))maxobs(j)=j_iobs
				enddo !j=1,narg  22804
			endif !if(limaxobs.ge.0)  22803
			! do j=1,narg
			! xh=j_v(arg(j))  !j_v(j_o(iob)%i(io+1+j))
			! if(abs(xh).lt.1.7e19)then
			! call j_msd21(xh,xs(j),xss(j),xw,sumwt(j),summa(j))
			! xmin_(j)=min(xmin_(j),xh)
			! xmax_(j)=max(xmax_(j),xh)
			! end if !if(abs(xh).lt.1.7e19)then
			!				end do !do j=1,narg
		end do !j_iobs=j_dfrom,j_duntil  22771
	endif !if(missing)  22726
 
 
 
 
	j_v(j_ivaccepted)=ntot
	if(ntot.le.0)then
		write(6,*)'**stat: all observations rejected'
		goto 750
	endif !if(ntot.le.0)  22824
 
	rejo=.false.
	if(iout.ne.j_ivresult)then
		if(limean.ge.0)ivmean=j_defmatrix(iout,'%mean',int8(narg),j_18,j_matreg)
		if(livar.ge.0)ivvar=j_defmatrix(iout,'%var',int8(narg),j_18,j_matreg)
		if(lisd.ge.0)ivsd=j_defmatrix(iout,'%sd',int8(narg),j_18,j_matreg)
		if(lisdmean.ge.0)ivsdmean=j_defmatrix(iout,'%sdmean',int8(narg),j_18,j_matreg)
		if(limin.ge.0)ivmin=j_defmatrix(iout,'%min',int8(narg),j_18,j_matreg)
		if(lirmse.ge.0)ivrmse=j_defmatrix(iout,'%rmse',int8(narg),j_18,j_matreg)
		if(limax.ge.0)ivmax=j_defmatrix(iout,'%max',int8(narg),j_18,j_matreg)
		if(lisum.ge.0)ivsum=j_defmatrix(iout,'%sum',int8(narg),j_18,j_matreg)
		!		if(j_err)return
	endif !if(iout.ne.j_ivresult)  22830
	!write(6,*)'<7474got'.ligot
	weisu=xws
	maxlen=0
	do j=1,narg
		maxlen=max(maxlen,j_lenname(arg(j)))
	enddo !j=1,narg  22844
	!8881 format(1x,a16,1x,6g14.6)
 
	if(j_dprint.ne.0)then
		write(6,*)'Accepted ',ntot,' out of ',dnobs
		if(missing)then
			call j_getname(arg(1))
			write(6,*)'variables ',j_oname(1:j_loname),'%got etc get the number of obs for each variable'
		endif !if(missing)  22851
 
		if(j_dnobs.ne.ntot)write(6,*)'from the total number of obs: ',nobs
		if(weig)write(6,*)'sum of weights ',xws !maxval(sumwt(1:narg))
		if(lirmse.lt.0)then
			j_form='(1x,a16,a8,5a14,a7)'
			write(j_form(6:7),'(i2)')maxlen+1
 
			write(*,j_form(1:19))'var          ','min', 'max','mean','sd','sdmean'
			if(missing)write(*,'(82x,a)')'accepted'
			j_form='(1x,a16,1x,5g14.6,i6)'
		else
			j_form='(1x,a16,a8,6a14,a7)'
			write(j_form(6:7),'(i2)')maxlen+1
			write(*,j_form(1:19))'var          ','min', 'max','mean','sd','sdmean','rmse'
			if(missing)write(*,'(87x,a)')'accepted'
			j_form='(1x,a16,1x,6g14.6,i6)'
		endif !if(lirmse.lt.0)  22858
 
		write(j_form(6:7),'(i2)')maxlen+1
	end if !if(j_dprint.ne.0)  22849
	!	write(6,*)j_form(1:20)
	do j=1,narg
 
		haj=j_0
		if(missing)	weisu=sumwt(j)
 
 
		if(weig)then
			haj=sqrt(xss(j)/weisu)
		else !if(weig)then
			if(xss(j).gt.j_0)then
				if(weisu.gt.j_1)	haj=sqrt(xss(j)/(weisu-j_1))
			endif !if(xss(j).gt.j_0)  22885
		endif !if(weig)  22882
		if(isfunc)then
			j_oname(1:6)='func->'
			j_loname=6
			iv=j_ivdollar
		else
			iv=arg(j)    !j_o(iob)%i(io+1+j)
			call j_getname(iv)
			j_oname(j_loname+1:maxlen)=p_dots
			if(ligot.ne.0)call j_putoptv(iob,ligot,j,'%got',iv,weisu)
		endif !if(isfunc)  22889
		!write(6,*)'<666 ',iv
		if(j_dprint.ne.0)then
			if(lirmse.lt.0)then
				if(missing.and.weisu.lt.xws)then
					rejo=.true.
					if(weig)then
						write(*,*)' sum of weights of rejected observations ',xws-weisu
					else !if(weig)then
						write(*,j_form)&
							j_oname(1:maxlen),xmin_(j),xmax_(j),xs(j),haj,haj/sqrt(weisu),int(weisu)
						!	write(*,'(85x,a,i6)')'got: ',int(weisu)  !xws-sumwt(j))
					endif !if(weig)  22904
				else
					write(*,j_form)j_oname(1:maxlen),xmin_(j),xmax_(j),xs(j),haj,haj/sqrt(weisu)
				endif !if(missing.and.weisu.lt.xws)  22902
			else
 
 
				write(*,j_form)j_oname(1:maxlen),xmin_(j),xmax_(j),xs(j),haj,haj/sqrt(weisu),sqrt(xs(j)*xs(j)+haj*haj)
				!			write(*,8881)j_vname(iv),xmin_(j),xmax_(j),xs(j),haj,haj/sqrt(weisu),sqrt(xs(j)*xs(j)+haj*haj)
			endif !if(lirmse.lt.0)  22901
			if(j_err)then
				write(6,*)'*j* problem with variable ',iv
				return
			endif !if(j_err)  22920
 
		endif !if(j_dprint.ne.0)  22900
		if(iout.eq.j_ivresult)then
			if(limean.ge.0)call j_putoptv(iob,limean,j,'%mean',iv,xs(j))
			if(livar.ge.0)call j_putoptv(iob,livar,j,'%var',iv,haj**2)
			if(lisd.ge.0)call j_putoptv(iob,lisd,j,'%sd',iv,haj)
			if(lirmse.ge.0)call j_putoptv(iob,lirmse,j,'%rmse',iv,sqrt(xs(j)*xs(j)+haj*haj))
			if(lisdmean.ge.0)call j_putoptv(iob,lisdmean,j,'%sdmean',iv,haj/sqrt(sumwt(j)))
 
			if(limin.ge.0)call j_putoptv(iob,limin,j,'%min',iv,xmin_(j))
			if(limax.ge.0)call j_putoptv(iob,limax,j,'%max',iv,xmax_(j))
 
			if(lisum.ge.0)call j_putoptv(iob,lisum,j,'%sum',iv,summa(j))
			if(liminobs.ge.0)then
				j_dapu=minobs(j)
				call j_putoptv(iob,liminobs,j,'%minobs',iv,j_dapu)
			endif !if(liminobs.ge.0)  22937
			if(limaxobs.ge.0)then
				j_dapu=maxobs(j)
				call j_putoptv(iob,limaxobs,j,'%maxobs',iv,j_dapu)
			endif !if(limaxobs.ge.0)  22941
 
 
		else !if(iout.eq.j_ivresult)then
			if(limean.ge.0)call j_putmatrix(ivmean,j,1,xs(j))
			if(livar.ge.0)call j_putmatrix(ivvar,j,1,haj**2)
			if(lisd.ge.0)call j_putmatrix(ivsd,j,1,haj)
			if(lisdmean.ge.0)call j_putmatrix(ivsdmean,j,1,haj/sqrt(sumwt(j)))
			if(limin.ge.0)call j_putmatrix(ivmin,j,1,xmin_(j))
			if(limax.ge.0)call j_putmatrix(ivmax,j,1,xmax_(j))
			if(lisum.ge.0)call j_putmatrix(ivsum,j,1,summa(j))
 
		endif !if(iout.eq.j_ivresult)  22926
	end do !j=1,narg  22876
	if(rejo.and.j_dprint.ne.0)write(*,*)'*** reason for rejecting observations for variables: abs-value.ge.1.7e19'
	750 deallocate(xmin_,xmax_,xs,xss,sumwt,summa)
	if(liminobs.ge.0)deallocate(minobs)
	if(limaxobs.ge.0)deallocate(maxobs)
	if(.not.missing)deallocate(x)
	8881 format(1x,a16,1x,6g14.6)
	!8882 format(1x,a16,a8,5a14,a7)
	!	88822 format(1x,a16,a8,6a14,a7)
	900 continue  ! io=io+narg+3
	if(istime)then
		call cpu_time(cpu1)
		time1=secnds(time0)
		write(6,*)' '
		write(6,*)'stat() used cpu ',cpu1-cpu0,' s and total time ',time1,' s'
	endif !if(istime)  22967
	if(j_dprint.ne.0)write(6,*)' '
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
	return
end subroutine stat_ !subroutine stat_(iob,io)
 
 
subroutine transdata(iob,io)
	integer*8::iobs
	! Section transdata transdata() Own computations for DATA
	! transdata() is useful when all necassy computions are put into a TRANS
	! , and a DATA is gone through obsevation by observation.
	! This is useful e.g. when simulating harvesting schdules using a simulator which is defined
	! as an ordinary TRANS. The whole function is written below to indicate
	!how users' own functions dealing with DATA could be developped.
	! endheader
	! Option
	!  data & N|1- & DATA &	DATA object. Default last DATA defined.
	!from&N|1&REAL& First observation used.
	!until&N|1&REAL& Last observation used.
	!trans&N|1&TRANS& TRANS computed fro each observation
	!epilog&N|1&TRANS &TRANS computed after going through data.
	! endoption
 
	!Listing
	!subroutine transdata(iob,io)
	! call j_getdataobject(iob,io)
	! if(j_err)return
	! !!call j_clearoption(iob,io)  ! subroutine
 
	! do j_iobs=j_dfrom,j_duntil
	! call j_getobs(j_iobs)
	! if(j_err)return
	! end do !do j_iobs=j_dfrom,j_duntil
 
	! if(j_depilog.gt.0)call dotrans(j_depilog,1)
 
	! return
	!endlisting
	!endsection
	!logical is2
	!is2=j_v(j_ivdollar).eq.128
	!if(is2)then
	!write(6,*)'bef'
	call j_getdataobject(iob,io)
	if(j_err)return
	if(.not.j_distrans)then
		write(6,*)'transdata requires trans->'
		j_err=.true. ;return
 
	endif !if(.not.j_distrans)  23018
	!!call j_clearoption(iob,io)  ! subroutine
	!	write(6,*)'af',j_dfrom,j_duntil
	do iobs=j_dfrom,j_duntil
		!	write(6,*)'j_iobs ',j_iobs
		call j_getobs(iobs)
		if(j_err)return
	end do !iobs=j_dfrom,j_duntil  23025
	! else
	! call j_getdataobject(iob,io)
	! if(j_err)return
	! !!call j_clearoption(iob,io)  ! subroutine
 
	! do j_iobs=j_dfrom,j_duntil
	! call j_getobs(j_iobs)
	! if(j_err)return
	! end do !j_iobs=j_dfrom,j_duntil  19383
 
 
	! endif !if(is2)  19369
 
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
 
	return
end subroutine transdata!subroutine stat_(iob,io)
 
subroutine find_d(iob,io)
	integer*8::iobs !,nfind the lengt of vecttor must fit into inetrger*4
	integer, dimension(:), allocatable:: ifind
	! Section findd find_d() makes a ILIST of observations satisfying a condition.
	! endheader
	! Option
	!Output& 1& ILIST &The generated ILIST.
	!  data & N|1- & DATA &	DATA object. Default last DATA defined.
	!from&N|1&REAL& First observation used.
	!until&N|1&REAL& Last observation used.
	!filter&N|1&CODE& Code telling what observations are accepted
	!reject&N|1&CODE &Code telling what observations are rejected.
	! endoption
	!Note  Function find() can be used to search all or first elements of a vector
	! satifying a condition.
	!endnote
	!endsection
 
	call j_startfunction(iob,io,0,narg,j_optarg0,ivout);if(j_err)return
 
	if(j_err)return
	call j_getdataobject(iob,io)
	if(j_err)return
	if(.not.(j_disfilter.or.j_disreject))then
		write(6,*)'find_d requires filter-> or reject->'
		j_err=.true. ;return
 
	endif !if(.not.(j_disfilter.or.j_disreject))  23071
	if(allocated(ifind))deallocate(ifind)
	maxo=j_duntil-j_dfrom+1
	allocate(ifind(1:maxo))
	!!call j_clearoption(iob,io)  ! subroutine
	!	write(6,*)'af',j_dfrom,j_duntil
	nfind=0
	do iobs=j_dfrom,j_duntil
		!	write(6,*)'j_iobs ',j_iobs j_rejected
		call j_getobs(iobs)
		if(j_err)return
		if(.not.j_rejected)then
			nfind=nfind+1
			ifind(nfind)=iobs
 
		endif !if(.not.j_rejected)  23086
	end do !iobs=j_dfrom,j_duntil  23082
 
	iout=j_deflist(ivout,' ',list0=nfind,list=ifind)
	j_otype(ivout)=j_ipilist
	deallocate(ifind)
	! else
	! call j_getdataobject(iob,io)
	! if(j_err)return
	! !!call j_clearoption(iob,io)  ! subroutine
 
	! do j_iobs=j_dfrom,j_duntil
	! call j_getobs(j_iobs)
	! if(j_err)return
	! end do !j_iobs=j_dfrom,j_duntil  19383
 
 
	! endif !if(is2)  19369
 
	!if(j_depilog.gt.0)call dotrans(j_depilog,1)
 
	return
end subroutine find_d !subroutine stat_(iob,io)
 
 
 
 
subroutine corr(iob,io,itype)  !stat()
	! Section cov cov()  Covariance MATRIX
	! cov() computes the covariance matrix of variables in DATA.
	! endheader
	! Option
	! output&1&MATRIX& symmetric aoutput matrix.
	! arg& 1-N&LIST or REALV& variables for which covarianes are computed, listing
	! individually or given as a LIST.
	!@@data@
	! weight&-1|1&CODE& Codeoption for weight of each observation.
	! endoption
	! Note the output is not automaticall printed, but it can be printed using ';'
	! at the end of line.
	! endnote
	! Note The covariance matrix can changed into correaltion matrix with corrmatrix()
	! function.
	! endnote
	! Note If variable ]w[ in the data is used as the weigth, this can be expressed as
	! weight->w
	! endnote
	! Ex covex Example of covariance
	! X1=matrix(200)
	! X1=rann()
	! ;do(i,2,6)
	! ad=matrix(200)
	! ad=rann()
	! X"i"=X"i-1"+0.6*ad
	! ;enddo
	! Continue=1   !error
	! dat=newdata(X1...X6,read->(x1...x5))
	!Continue=0
	! dat=newdata(X1...X6,read->(x1...x6))
	! co=cov(x1...x5);
	! co=cov(dat%keep);
	! endex
	! endsection
 
	! Section corr corr() Correlation MATRIX
	! corr(1) works similarly as cov()
	! endsection
 
 
 
 
 
	integer, dimension(:), pointer :: arg !arguments of the function
	logical weig,weigt !filter, single
	double precision sumwt,b_,c,wt
	double precision, dimension(:),allocatable::cov,xc,xm
	logical ::iscorr
	integer*8::ii
	iscorr=itype.eq.0
	call j_checkoutput(iob,io)
	if(j_err)return
	!	io=io_
	narg=j_o(iob)%i(io+1)
	ivi=j_o(iob)%i(io+2)
	!	io_=io_+narg+3
	if(j_otype(ivi).eq.j_iplist)then
		arg=>j_o(ivi)%i2(1: j_o(ivi)%i(1))
	else
 
		arg=>j_o(iob)%i(io+1+1:io+1+narg)
	endif !if(j_otype(ivi).eq.j_iplist)  23175
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(allocated(cov))deallocate(cov,xc,xm)
	np=narg*(narg+1)/2
	allocate( cov(1:np),xc(1:narg),xm(1:narg))
	cov=j_0;xm=j_0
	call j_objargs(iob,io)
	if(j_err)return
	call j_getdataobject(iob,io)
	if(j_err)return
	! iprint=1
	! if(j_igetopt(iob,io,j_mprint).gt.0)iprint=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mprint)+1) )
	linkweight=j_codelink(iob,io,j_mweight); if(j_err)return
	weig=linkweight.ne.0
	! if(j_linkoption(iob,io,j_mweight).gt.0)then
	! weig=.true.
	! ioweight=j_linkopt2(j_mweight)
	! ivweight=j_o(iob)%i(j_linkoption(iob,io,j_mweight)+1)
	! weigt=j_o(iob)%i(ioweight).ne.0
	! else !if(j_linkoption(iob,io,j_mweight).gt.0)then
	! weig=.false.
	!end if !if(j_linkoption(iob,io,j_mweight).gt.0)then
	!!call j_clearoption(iob,io)  ! subroutine
	j_iobs=0
	iout=j_defmatrix(iout,' ',int8(narg),int8(narg),j_matreg)
	sumwt=0.
	wt=1.  !weight
	ntot=0
	!do kk=1,jndatasetss
	!call j_getdataset(j_datasets(kk),nobs)
		obloop:		do ii=j_dfrom,j_duntil
		call j_getobs(ii)
		if(j_err)return
		if(j_rejected)cycle
 
		ntot=ntot+1
		if(weig)then
			! if(weigt) call dotrans(iob,ioweight)
			! if(j_err)return
			wt=j_codevalue(iob,linkweight)  !j_v(ivweight)
			if(j_err)return
		endif !if(weig)  23217
		if(wt.ge.1.7d18)cycle obloop
		do i=1,narg
			if(abs(j_v(arg(i))).ge.1.7d18)cycle obloop
		enddo !i=1,narg  23224
		k=0
		sumwt=sumwt+wt
		b_=wt/sumwt
		c=wt-b_*wt
		do i=1,narg
			xc(i)= j_v(arg(i))-xm(i) !   v(corrl(i))-xm(i)
			xm(i)=xm(i)+b_*xc(i)
			do 4 j=1,i
				k=k+1
				cov(k)=cov(k)+c*xc(i)*xc(j)
				4	   				continue !4 j=1,i  23234
		enddo !i=1,narg  23231
	enddo obloop !oop:		do ii=j_dfrom,j_duntil  23211
	!	enddo !do kk=1,jndatasetss
	ih=sumwt
	if(ntot.le.1.)then
		write(6,*)'*all observations rejected'
		call j_del(iout)
		write(6,*)' '
		return
	endif !if(ntot.le.1.)  23242
 
 
	!write(6,*)'output matrix can be seen with print-function'
	write(6,*)'Accepted ',ntot,' out of ',dnobs
	j_v(j_ivaccepted)=ntot
	do  i=1,np
		cov(i)=cov(i)/(sumwt-1.) !do 7 i=1,np
	enddo ! i=1,np  23253
	if(.not.iscorr)then  !not correlation
		do i=1,narg
			j_o(iout)%d((i-1)*narg+i)=cov(i*(i+1)/2)
			do j=1,i-1
				j_o(iout)%d((i-1)*narg+j)=cov(i*(i-1)/2+j)
				j_o(iout)%d((j-1)*narg+i)=cov(i*(i-1)/2+j)
			enddo !j=1,i-1  23259
		enddo !i=1,narg  23257
	else !if(.not.iscorr)then
		do i=1,narg
			xc(i)=cov(i*(i+1)/2)
			if(xc(i).gt.j_0)xc(i)=sqrt(xc(i))
			j_o(iout)%d((i-1)*narg+i)=1.
			do j=1,i-1
				if(xc(i).gt.0..and.xc(j).gt.0.)then
					cor=cov(i*(i-1)/2+j)/(xc(i)*xc(j))
				else !if(xc(i).gt.0..and.xc(j).gt.0.)then
					cor=0.
				endif !if(xc(i).gt.0..and.xc(j).gt.0.)  23270
				j_o(iout)%d((i-1)*narg+j)=cor
				j_o(iout)%d((j-1)*narg+i)= cor
			enddo !j=1,i-1  23269
		enddo !i=1,narg  23265
	endif !if(.not.iscorr)  23256
	if(j_dprint.ge.1)then
 
		if(j_o(iob)%i(io) .eq.212)then  !not correlation
 
			!		write(6,8886)(j_object_name(arg(j),7),j=1,narg)
			do i=1,narg
				call j_getname(arg(i))
				write(6,8881)j_oname(1:min(15,j_loname)),j_o(iout)%d((i-1)*narg+1:i*narg)
			enddo !i=1,narg  23285
 
		else !if(j_o(iob)%i(io) .eq.212)then
			!		write(6,8885)(j_object_name(arg(j),12),j=1,narg)
			do i=1,narg
				call j_getname(arg(i))
				write(6,8883)j_oname(1:min(15,j_loname)),j_o(iout)%d((i-1)*narg+1:i*narg)
			enddo !i=1,narg  23292
 
 
 
		endif !if(j_o(iob)%i(io) .eq.212)  23282
 
 
	endif !if(j_dprint.ge.1)  23280
			750 		deallocate(cov,xc,xm)
			8881	format(1x,a16,1x,8g12.4)
		8882	format(1x,a16,a8,5a12,a7)
!			88822	format(1x,a16,a8,6a12,a7)
	8883	format(1x,a16,1x,14f7.3)
	8885 format(18x,14a7)
	8886 format(18x,8a12)
!			900 	continue ! io=io+narg+3
	write(6,*)' '
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
	return
end subroutine corr !subroutine corr(iob,io,itype)
 
subroutine matrix(iob,io) ! matrix()
	!Section matrix matrix() Creates MATRIX
	!Function matrix() creates a matrix and puts REAL values to the elements, zero being the default value
	! for each element.	Elements are actually
	! in double precision. Element values
	!can be read from the input paragraph, file, or the values can be generated
	! using values-> option, or sequential values can be generated
	! using do-> option. Function matrix() can generate a diagonal and block diagonal matrix.
	! A matrix can be generated from submatrices by using matrices as arguments
	! of the  values-> option. It should be noted that matrices are stored in row order.
	!endheader
	!Option
	!Output& 1& MATRIX | REAL& If a 1x1 matrix is defined, the output will be REAL.
	! The output can be a temporary matrix without name, if matrix() is an argument
	! of an arithmetic function  or matrix function.
	!Args&0-2&REAL& The dimension of the matrix. The first argument is the number of rows,
	! the second argument, if present, the number of columns.  If the matrix is generated from submatrices given in values->, then the dimensions
	!refer to the submatrix rows and submatrix columns. If there are no arguments, then the
	!it should be possible to infer the dimensions from values-> option. If the
	! first argument is ]Inf[, the the number of rows is determined by the number
	! number of lines in source determined by in->.
	! in&N|0|1&CHAR& The input for values. in-> means that values are read in from
	! the following input paragraps, in->]file[ means that the values are read from file.
	! in both cases a record must contain one row for each matrix row, except column vectors are
	! read from one input line, and the diagonal of a diagonal matrix is read from one input line.
	! Recall that an input line can consist of sevrl input records, where continuation is
	!indicated with an ending comma.
	!values&N|1-& REAL &values or MATRIX objects put to the matrix. The argumenst of
	! values-> option go in the regular way
	! through the interpreter, so the values can be obtained by computations. If only one REAL value is
	! given then all diagonal elements are put equal to the value (ohers will be zero),
	! if diag-> option is present, otherwise all elements are put equal to this value. If matrix dimensions
	!are given, and there are fewer values than is the size the matrix, matrix is
	! filled row by row using all values given in
	! values->. Giving more values than necessary does not cause an error.
	! Thus matrix(N,N,values->1,diag->) generates the identity matrix.
	! If value-> refers to one MATRIX,and diag-> is present then a block diagonal
	! matrix is generated. Without diag->, a partitioned matrix is generated having all
	! submatrices equal. In the new version also values given in the input paragraph or in the in-> file
	! can have names of variables whose values are put into the matrix. The value can be also computed
	! using transformations if the element is put within parenthesis. If values are read from a file,
	! then code-> option must be present telling Jlp22 that values can not be read directly.
	! code-> option is also needed when getting values from an iput paragraph, if only varaible names
	!are given.
	!	The same method can be used in data() function.
	!diag&N&& The matrix is diagonal. The diagonal is read from one input paragraph line, if in-> is present
	!without an argument.
	! code &N|0&REAL  & The values given in in-> file are interpreted and not read directly.
	!If a line contains ( the the line is interpreted also without code->. If line
	! is interpreted, the value can be given by giving the name of variable.
	!
	!do &N|0-3&REAL  & A matrix of number sequences is generated, as followsws: \newline
	!do-> Values 1,2,...,]arg1[ x ]arg2[ are put into the matrix in the row order. \newline
	!do->5 Values 5,6,...,]arg1[ x ]arg2[+4 are put into the matrix \newline
	!do->
	!endoption
	!Note In the current version the values in the input paragraph or in-> file can be separated with commas or
	! spaces, or tabulators, and there can be several separators between two values.
	!endnote
	!Note Values given for vectors or for the diagonal in a diagonal matrix can be in several input recorsds
	! forming a input line.
	!endnote
	!Note If there are fewer values in the input paragraph or in the file than there are matrix elements,
	! a warning but no error is generated. Extra values are ignored without a notice.
	!endnote
	!Ex matrixex Example of generatig matrices
	!A=matrix(3,values->(sin(2),sqrt(3),6));
	!** The same in the new version
	!h=9
	!**extra values are ignored
	! A=matrix(3,in->);
	!	(sin(2)),(sqrt(3)),6,h
	!/
	! ** No error even if less values are given. Diagonal is read from one line.
	!** extra lines are ignored
	! ma=matrix(5,5,diag->,in->);
	! 15,(sin(4)),43,h
	!77
	! /
	!** The last value is taken by putting it to the same input line
	! ma=matrix(5,5,diag->,in->);
	! 15,(sin(4)),43,h,
	!77
	! /
	!endex
 
	!endsection
 
	double precision,dimension(:),allocatable,target::temp2
	double precision, dimension (:), pointer :: outd
	double precision, dimension (:),allocatable :: valu
	integer, dimension (:), pointer :: doval,arg !do->
	integer, dimension (:), pointer :: values !do->
	character*8 loppu
	logical yes_
	double precision :: val
	logical matmat,inout,diag  !matrix of matrices
	double precision dostep,docur
	logical ::isinf,isany,isreal
	integer::iout
	logical::p
	integer*8:: ndim,ibas,i,ndim1,ncol,nval,ndim2,nel,nrows,ncols,ndim1b,nvar
	integer*8 lines
	integer::nva
	logical iscode,isinteger
	!	p=int(j_v(j_ivdollar2)).eq.j_o(iob)%i(io)
	p=j_v(j_ivdollar2).eq.345.d0
	j_needsall=.false.
	!	j_makelistvar=0
	j_issparse=.false.
	!	j_caseread=0
	! write(6,*)'22>',j_o(iob)%i(0),'**',j_o(iob)%i(1:13)
	! do jj=1,j_noptions_
	! write(6,*)jj,j_options(jj)
	! enddo
	! stop
	!	io=io_
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	diag=j_linkoption(iob,io,j_mdiag,clear=.true.).ge.0
	isinteger=j_isoption(iob,io,j_minteger)
 
	!	matmat=j_linkoption(iob,io,j_mmatrix,clear=.true.).ge.0
	itype=j_matreg
	if(diag)itype=j_matdiag
	if(isinteger)itype=j_mati
	if(p)write(6,*)'matmat',matmat,' nva',nva
	isinf=.false.
	if(narg.eq.0)then
		arg=>j_o(iob)%i(io:io) !just to such small number which does not cause trouble in arg(1)
	elseif(narg.le.2)then !if(narg.eq.0)then
		arg=>j_o(iob)%i(io+1+1:io+1+narg) !i narg.eq.0 arg refers to nonsense
		isinf=arg(1).eq.j_ivinf
		if(.not.isinf)ndim1=j_v(arg(1))
		ndim2=1
		if(narg.eq.2)ndim2=j_v(arg(2))
	else
		write(6,*)'**matrix, illegal number of arguments'
		j_err=.true.; return
	endif !if(narg.eq.0)  23444
	!write(6,*)'ndim1,ndim2',ndim1,ndim2,'isinf',isinf
	if(isinf)then
		nu=j_getin(iob,io,ivform=ivform,lines=lines,needslines=.true.) !matrix
 
		if(nu.lt.0)then
			write(6,*)'if number of rows is Inf, then there must be in->'
			j_err=.true.;goto 70
		else
			ndim1=lines
		endif !if(nu.lt.0)  23460
	else
		!write(6,*)'nu,helluorm',iob,io,ivform,lines
		nu=j_getin(iob,io,ivform=ivform,lines=lines) !matrix
		!write(6,*)'nu,lines,ivform',nu,lines,ivform
 
	endif !if(isinf)  23457
	!	write(6,*)'ndim1b',ndim1b
	if(nu.eq.0)then
		!	write(6,*)'callenpara'
		!	lines=j_ninlines   !j_lenpara(j_isinpu)
		!		write(6,*)'callenpara',lines
		j_isreadnu=.false.
		j_isgetinp=.true.
	elseif(nu.gt.0)then
		iscode=j_isoption(iob,io,j_mcode)
		j_isreadnu=.not.iscode
		!	lines=j_nlinesnu(nu)
		j_isgetinp=.false.
	endif !if(nu.eq.0)  23473
	!	j_isgetinp=nu.eq.0
	!	write(6,*)'nu',nu,io,iob
	call j_getoption(iob,io,j_mvalues,-1,99999,0,.true.,nva,j_optarg0)
 
	if(j_err)return
	matmat=.false.
	if(nva.gt.0)then
		if(nu.ge.0)then
			write(6,*)'there cannon be both in-> and values->'
			j_err=.true.;return
		endif !if(nu.ge.0)  23492
 
		do iva=1,nva
			!		write(6,*)'iva ',iva,j_otype(j_optarg0(iva)),j_optarg0(iva).eq.j_ipmatrix
			if(j_otype(j_optarg0(iva)).eq.j_ipmatrix)then
				matmat=.true.
			elseif(j_otype(j_optarg0(iva)).ne.j_ipreal)then
				call j_getname(j_optarg0(iva))
				write(6,*)'values-> must refer to REAL or MATRIX ',j_oname(1:j_loname),&
					' is ',j_otypes(j_otype(j_optarg0(iva)))
			endif !if(j_otype(j_optarg0(iva)).eq.j_ipmatrix)  23499
		enddo !iva=1,nva  23497
	endif !if(nva.gt.0)  23491
 
 
	if(diag.and.matmat.and.narg.eq.0.and.nva.eq.1)then
		irg=j_optarg0(1)
		ndim=j_o(irg)%i(3)
 
		iout=j_defmatrix(iout,' ',ndim,ndim,j_matreg)
 
		ibas=0
		do i=1,ndim
			j_o(iout)%d(ibas+i)=j_o(irg)%d(i)
			ibas=ibas+ndim
		enddo !i=1,ndim  23517
		!write(6,*)'matret1'
		return
 
	endif !if(diag.and.matmat.and.narg.eq.0.and.nva.eq.1)  23510
 
 
	!	iarg1=j_o(iob)%i(io+2)
	!****
	if(narg.eq.1.and.j_otype(arg(1)).eq.j_iplist)then
 
		ndim1=j_o(arg(1))%i(1)
 
		if(diag)then
 
			iout=j_defmatrix(iout,' ',ndim1,ndim1,itype)
			ibas=0
			do i=1,ndim
				j_o(iout)%d(ibas+i)=j_v(j_o(arg(1))%i2(i))
				ibas=ibas+ndim1
			enddo !i=1,ndim  23537
		else !if(diag)then
			iout=j_defmatrix(iout,' ',ndim1,j_18,j_matreg)
 
			j_o(iout)%d=j_v(j_o(arg(1))%i2(1:ndim1))
		endif !if(diag)  23533
		goto 70
	endif !if(narg.eq.1.and.j_otype(arg(1)).eq.j_iplist)  23529
	!******
 
	if(p)write(6,*)'<88  narg,arg,v(arg)iout,nva,inout', narg,arg,j_v(arg),iout,nva,inout
	if(.not.isinf)ndim1=0
	ndim2=0
	if(matmat.and.narg.eq.0)then
		ndim1=nva
		ndim2=1
	elseif(narg.eq.1)then
		ndim1=j_v(arg(1))
		ndim2=1
		if(diag)ndim2=ndim1
 
	elseif(narg.eq.2)then
		if(.not.isinf)ndim1=j_v(arg(1))
		ndim2=j_v(arg(2))
		if(diag.and.ndim1.ne.ndim2)then
			write(6,*)'with diag-> the number of rows must be equal to number of columns, or drop secon argument'
			j_err=.true.;return
		endif !if(diag.and.ndim1.ne.ndim2)  23564
	endif !if(matmat.and.narg.eq.0)  23553
	isreal=ndim1.eq.1.and.ndim2.eq.1
	isany=j_linkoption(iob,io,j_many).ge.0
	!	iarg2=j_o(iob)%i(io+3)
	call j_getoption(iob,io,j_mdo,-1,99999,j_ipreal,.false.,ndo,doval);if(j_err)return
 
	!		write(6,*)'ndoa:',ndo,doval,j_v(doval)
	!	call getoption(iob,io,option_name,minarg,maxarg,iptype,expand,min0,noptarg,optarg)
 
 
	!write(6,*)'nva ',nva,j_optarg0(1)
	if(nva.gt.0)then
		if(allocated(valu))deallocate(valu)
	endif !if(nva.gt.0)  23579
	if(nva.eq.1)then
		if(j_otype(j_optarg0(1)).eq.j_ipmatrix.and..not.matmat)then
			nva=j_o(j_optarg0(1))%i(3)
			allocate(valu(1:nva))
			valu=j_o(j_optarg0(1))%d(1:nva)
		endif !if(j_otype(j_optarg0(1)).eq.j_ipmatrix.and..not.matmat)  23583
 
	elseif(nva.gt.0.and..not.matmat)then !if(nva.eq.1)then
 
		allocate(valu(1:nva))
		valu=j_v(j_optarg0(1:nva))
	endif !if(nva.eq.1)  23582
	!write(6,*)'<77',nva,j_optarg0
	if(matmat.and.nva.lt.1)then
		write(6,*)'element matrices must be given in values->'
		j_err=.true. ;goto 70
	endif !if(matmat.and.nva.lt.1)  23595
 
	if(nva.gt.0.and.ndo.gt.0)then
		write(6,*)'values-> and do-> cannot be simultaeously'
		j_err=.true. ;goto 70
	endif !if(nva.gt.0.and.ndo.gt.0)  23600
 
	if(narg.eq.0.and..not.matmat)then
		if(nva.le.0.and.ndo.eq.0)then
			write(6,*)'without arguments, values must be given using values-> or do->'
			j_err=.true. ;goto 70
 
		elseif(nva.gt.0)then !if(nva.le.0.and.ndo.eq.0)then
			ncol=1
			if(diag)ncol=nva
			j_i8=nva
			iout=j_defmatrix(iout,' ',j_i8,ncol,itype)
			!	write(6,*)'<77'
			if(diag)then
 
				do i=1,nva
					j_o(iout)%d((i-1)*nva+i)=valu(i) !j_v(j_optarg0(i))
				enddo !i=1,nva  23618
			elseif(isinteger)then
				j_o(iout)%i2=valu
			else !if(diag)then
				!write(6,*)'<765 ',narg,nva,valu,size(j_o(iout)%d)
				j_o(iout)%d=valu  !j_v(j_optarg0)
				!write(6,*)'<766 ',narg,nva,valu,size(j_o(iout)%d)
			endif !if(diag)  23616
 
			goto 70
		elseif(ndo.ge.0)then !if(nva.le.0.and.ndo.eq.0)then
			if(ndo.lt.2)then
				write(6,*)'without dimensions do-> must have atl least two arguments'
				j_err=.true. ;goto 70
			endif !if(ndo.lt.2)  23631
			dostep=1.d0
			if(ndo.eq.3)dostep=j_v(doval(3))
			if(dostep.eq.0)then
				write(6,*)'do-step is zero'
				j_err=.true. ; goto 70
			endif !if(dostep.eq.0)  23637
 
			nval=nint((j_v(doval(2))-j_v(doval(1)))/dostep)+1
			!		write(6,*)'ndo',ndo,doval
 
			if(nval.le.0)then
				write(6,*)'illegal do->'
				j_err=.true. ; goto 70
			endif !if(nval.le.0)  23645
 
			if(diag)then
				iout=j_defmatrix(iout,' ',nval,nval,itype)
				!write(6,*)'<78'
				docur=j_v(doval(1))
				do i=1,nval
					j_o(iout)%d((i-1)*nval+i)=docur
					docur=docur+dostep
 
				enddo !i=1,nval  23654
			else !if(diag)then
				if(p)write(6,*)'ioutbe ',iout,j_otype(iout)
				iout=j_defmatrix(iout,' ',nval,j_18,itype)
				if(p)write(6,*)'ioutaf ',iout,j_otype(iout)
				!write(6,*)'<80'
				docur=j_v(doval(1))
				if(isinteger)then
					do i=1,nval
						j_o(iout)%i2(i)=docur
						docur=docur+dostep
					enddo !i=1,nval  23666
 
				else
					do i=1,nval
						j_o(iout)%d(i)=docur
						docur=docur+dostep
					enddo !i=1,nval  23672
				endif !if(isinteger)  23665
			endif !if(diag)  23650
			goto 70
		endif !if(nva.le.0.and.ndo.eq.0)  23606
 
	endif !if(narg.eq.0.and..not.matmat)  23605
 
 
	if(narg.eq.2)then
		if(j_otype(arg(1)).eq.j_iplist)then
			inde=j_v(arg(2))
			!	if(inde.lt.1.or.inde.gt.j_o(arg(1))%i8(1))then
			if(inde.lt.1.or.inde.gt.j_nrows(arg(1)))then
				write(6,*)'*illegal index ',inde, 'for list with len=',j_o(arg(1))%i(1)
				j_err=.true.;goto 70
			endif !if(inde.lt.1.or.inde.gt.j_nrows(arg(1)))  23688
			irg=j_o(arg(1))%i2(inde)
			if(j_otype(irg).eq.j_ipreal)then
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				j_v(iout)=j_v(irg)
				goto 70
			endif !if(j_otype(irg).eq.j_ipreal)  23693
			if(j_otype(irg).ne.j_ipmatrix)then
				call j_printname('*not a matrix:',irg,' ')
				j_err=.true. ;goto 70
			endif !if(j_otype(irg).ne.j_ipmatrix)  23698
			nel=ndim1*ndim2
			!	if(p)write(6,*)'ioutbe ',iout,j_otype(iout)
			iout=j_defmatrix(iout,' ',ndim1,ndim2,j_matreg)
			if(p)write(6,*)'ioutbe ',iout,j_otype(iout),ndim1,ndim2
			!write(6,*)'<89',ndim1
			j_o(iout)%d=j_o(irg)%d(1:nel)
			!write(6,*)' '
			goto 70
		end if !if(j_otype(arg(1)).eq.j_iplist)  23685
	endif !if(narg.eq.2)  23684
 
	if(.not.matmat)then
		if(diag.and.nva.gt.0.and.nva.ne.ndim1.and.nva.ne.1)then
			write(6,*)nva,' values given for diagonal ',ndim1,' x ', ndim,' matrix, not allowed without any->'
			j_err=.true.
		elseif(.not.diag.and.nva.gt.0.and.ndim1*ndim2.ne.nva.and..not.isany.and.nva.ne.1)then !if(diag.and.nva.gt.0.and.nva.ne.ndim1.and.nva.ne.1)then
			write(6,*)'nrows=',ndim1,' and ncols=',ndim2, 'and there are ',nva, &
				'values and no any->'
			j_err=.true.
		endif !if(diag.and.nva.gt.0.and.nva.ne.ndim1.and.nva.ne.1)  23714
		if(j_err)return
 
		iout=j_defmatrix(iout,' ',ndim1,ndim2,itype)
		if(p)then
			write(6,*)'defm ',ndim1,ndim2,iout
			call j_getname(iout)
			write(6,*)j_oname(1:j_loname),j_otype(iout),'matmat',matmat
		endif !if(p)  23725
 
	endif !if(.not.matmat)  23713
	!write(6,*)'<100',ndim1,ndim2
	if(j_err)goto 70
	!	endif !if(narg.le.0)then
	inout=.false.
 
	if(matmat)then
		call matmatsub()
		goto 70
	endif !if(matmat)  23737
 
	if(p)write(6,*)'nvahere',nva,'nu',nu
	!	write(6,*)'linopt',j_linkoption(iob,io,j_min),nu
	if(nva.ge.1)then
		call readval()
 
	elseif(nu.ge.0)then !if(nva.ge.1)then
 
		!	write(6,*)'nu',nu,ndim1b
		! if(nu.eq.0.and.arg(1).eq.j_ivinf)then
		! ndim1=ndim1b
 
		! endif !if(nu.eq.0.and.arg(1).eq.j_ivinf)  23242
		! ndim1=lin-lin0
		!		write(6,*)'found ',ndim1,' rows'
		if(diag)ndim2=ndim1
 
		!	endif !if(isinf)  20957
		!		character*13000 j_tempchar2
 
		iout=j_defmatrix(iout,' ',ndim1,ndim2,itype)
		if(p)write(6,*)'defmhere ',ndim1,ndim2
 
		!	integer function j_countlim(inp,linp,lim)
		! if(diag)then
 
		! if(allocated(j_tempvector))deallocate(j_tempvector)
		! allocate(j_tempvector(1:ndim1))
		! endif !if(diag)  21260
		if(nu.ge.0)then
			leva=0
			if(diag)then
				! goto 780
				! 770	write(6,*)'there are not ',ndim1b,' elements in line'
				! write(6,*)j_inpr(1:j_linpr)
				! j_err=.true.;return
 
				! 780				call j_getinput0(' ')
				!	read(j_inpr(1:j_linpr),*,end=770,err=90)j_o(iout)%d(1:ndim1b)
 
				call j_readvector(nu,ivform,j_o(iout)%d(1:ndim1),ndim1)
				if(j_err)then
					write(6,*)'error reading diagonal'
					goto 767
				endif !if(j_err)  23782
 
				ibas=ndim1
				do i=2,ndim1
					j_o(iout)%d(ibas+i)=j_o(iout)%d(i)
					j_o(iout)%d(i)=j_0
					ibas=ibas+ndim1
				enddo !i=2,ndim1  23788
			else
 
				ipi=index(j_inp(1:j_linp),',')
 
				!		else
				if(p)write(6,*)'ndim1b,ndim1',ndim1b,ndim1,lines
 
				if(ndim2.eq.1.and.ipi.gt.1)then
					call j_readvector(nu,ivform,j_o(iout)%d(1:ndim1),ndim1)
				else
					do ii=1,ndim1
						!		call j_getinput0(' ')
						!		iva=j_countlim(j_inp,j_linp,',')+1
						!		levaiva=leva+iva
 
						!		iva2=min(levaiva,j_o(iout)%i(3))
						!write(6,*)'leii ',iiia
						!			read(j_inpr(1:j_linpr),*,end=66,err=90)j_o(iout)%d(leva+1:leva+ndim2)
 
						call j_readvector(nu,ivform,j_o(iout)%d(leva+1:leva+ndim2),ndim2)
						if(j_end)then
							write(6,*)'premature end in line  ',ii
							j_err=.true.
						endif !if(j_end)  23813
						if(j_err)then
							write(6,*)'error reading row ',ii
							goto 767
						endif !if(j_err)  23817
						!			if(iva2.ge.leva+1)read(j_inp(1:j_linp),*,err=90)j_o(iout)%d(leva+1:iva2)
						!			if(nvar.lt.ndim2)write(6,*)'*WRN* there were only ',nvar, ' elements in row ',ii
						leva=leva+ndim2
					enddo !ii=1,ndim1  23803
 
				endif !if(ndim2.eq.1.and.ipi.gt.1)  23800
			endif !if(diag)  23772
			!	leva=iva2
 
			! 767		if(p)write(6,*)'<541>nu ',nu
767			if(nu.eq.0)then
				call j_exitpara()
			else
				call j_closeunit(nu)
			endif !767			if(nu.eq.0)  23831
 
			if(p)write(6,*)'767 nu ',nu
			! elseif(nu.gt.0)then
			! goto 80
			! 78 write(6,*)'error reading matrix '
			! j_err=.true.;goto 777
			! 79 write(6,*)'premature end reading matrix ',nu,nva
			! j_err=.true.;goto 777
 
			! 80		if(diag)then
			! read(nu,*,end=79,err=78)j_o(iout)%d(1:ndim1)
 
			! do ii=2,ndim1
			! j_o(iout)%d((ii-1)*ndim1+ii)=j_o(iout)%d(ii)
			! j_o(iout)%d(ii)=j_0
			! enddo !ii=2,ndim1  23513
			! else
			! read(nu,*,end=79,err=78)j_o(iout)%d
			! endif !80		if(diag)  23510
 
 
 
 
			return
		endif !if(nu.ge.0)  23770
 
 
		goto 70
 
 
	endif !if(nva.ge.1)  23744
 
 
 
	ibas=0
 
	!		j_o(iout)%d=j_0
	if(p)write(6,*)'ndohere',ndo
	if(ndo.eq.0)then !if(ndo.lt.0)then
		if(diag)then
			do i=1,ndim1
				j_o(iout)%d(ibas+i)=i
				ibas=ibas+ndim1
			enddo !i=1,ndim1  23876
		elseif(isinteger)then
			do i=1,ndim1*ndim2
				j_o(iout)%i2(i)=i
			enddo !i=1,ndim1*ndim2  23881
 
		else !if(diag)then
			do i=1,ndim1*ndim2
				j_o(iout)%d(i)=i
			enddo !i=1,ndim1*ndim2  23886
 
		endif !if(diag)  23875
	elseif(ndo.eq.1)then !if(ndo.lt.0)then
		if(diag)then
			do i=1,ndim1
				j_o(iout)%d(ibas+i)=i+j_v(doval(1))-1
				ibas=ibas+ndim1
			enddo !i=1,ndim1  23893
		else !if(diag)then
			do i=1,ndim1*ndim2
				j_o(iout)%d(i)=i+j_v(doval(1))-1
			enddo !i=1,ndim1*ndim2  23898
		endif !if(diag)  23892
	elseif(ndo.eq.2)then !if(ndo.lt.0)then
		iup=j_v(doval(2))
		if(diag)then
 
			do i=1,min(ndim1,iup)
				j_o(iout)%d(ibas+i)=i+j_v(doval(1))-1
				ibas=ibas+ndim1
			enddo !i=1,min(ndim1,iup)  23906
 
		else !if(diag)then
			do i=1,min(ndim1*ndim2,iup)
				j_o(iout)%d(i)=i+j_v(doval(1))-1
			enddo !i=1,min(ndim1*ndim2,iup)  23912
		endif !if(diag)  23904
	elseif(ndo.eq.3)then !if(ndo.lt.0)then
		docur=j_v(doval(1))
		if(diag)then
			do i=1,ndim1
				j_o(iout)%d(i)=docur
				docur=docur+j_v(doval(3))
				if(j_v(doval(3)).lt.j_0)then
					if(docur.lt.j_v(doval(2))-1.0d-17)exit
				else
					if(docur.gt.j_v(doval(2))+1.0d-17)exit
				endif !if(j_v(doval(3)).lt.j_0)  23922
				ibas=ibas+ndim1
			enddo !i=1,ndim1  23919
		elseif(isinteger)then
			do i=1,ndim1*ndim2
				j_o(iout)%i2(i)=docur
				docur=docur+j_v(doval(3))
				if(j_v(doval(3)).lt.j_0)then
					if(docur.lt.j_v(doval(2))-1.0d-17)exit
				else
					if(docur.gt.j_v(doval(2))+1.0d-17)exit
				endif !if(j_v(doval(3)).lt.j_0)  23933
 
			enddo !i=1,ndim1*ndim2  23930
 
		else !if(diag)then
			do i=1,ndim1*ndim2
				j_o(iout)%d(i)=docur
				docur=docur+j_v(doval(3))
				if(j_v(doval(3)).lt.j_0)then
					if(docur.lt.j_v(doval(2))-1.0d-17)exit
				else
					if(docur.gt.j_v(doval(2))+1.0d-17)exit
				endif !if(j_v(doval(3)).lt.j_0)  23945
 
			enddo !i=1,ndim1*ndim2  23942
 
		endif !if(diag)  23918
 
	endif !if(ndo.eq.0)  23874
 
	70 	continue ! io=io+narg+3
	!!call j_clearoption(iob,io)  ! subroutine
	if(allocated(valu))deallocate(valu)
	if(j_err)return
	!write(26,*)linkopt(j_mdiag
	if(p)write(6,*)'j_o(iout)%i(3)',j_o(iout)%i(3),'isreal',isreal
	if(j_o(iout)%i(3).eq.1)then
		docur=j_o(iout)%d(1)
		call j_del(iout)
		j_v(iout)=docur
	endif !if(j_o(iout)%i(3).eq.1)  23963
	if(isreal)then
		docur=j_o(iout)%d(1)
		call j_del(iout)
		j_v(iout)=docur
	endif !if(isreal)  23968
	!write(6,*)'metret70'
	return
	90 	write(6,*)'**error in reading matrix values'
	j_err=.true.
	goto 70
	91 	write(6,*)'*matrix error in reading row ',i
	write(6,*)j_inpr(1:le)
	j_err=.true.
	goto 70
	98 	write(6,*)'*premature end of file'
	99 	write(6,*)'*matrix, error reading from binary file, iostat',ios
	j_err=.true.
	call j_closeunit(nu)
	goto 70
	contains
 
	subroutine matmatsub()
 
 
		!	write(6,*)'<7757>here'
 
		if(nva.ne.ndim1*ndim2.and..not.diag)then
			write(6,*)'there should be ', ndim1*ndim2 ,' arguments in values->'
			j_err=.true. ;return
		endif !if(nva.ne.ndim1*ndim2.and..not.diag)  23994
		!***********************
		if(diag)then
			nrows=0
			ivastep=1
			if(nva.eq.1)ivastep=0
			!		write(6,*)'nva ',nva
			iva=1
 
			do iro=1,ndim1
				ii=j_optarg0(iva)
				ii=j_optarg0(iva)
 
				iva=iva+ivastep
				if(ii.eq.iout)then
					write(6,*)'output cannot be same as one of values-> matrices'
					j_err=.true.;return
				endif !if(ii.eq.iout)  24011
				if(j_otype(ii).eq.j_ipreal)then
					niro=1
					ncols=1
				else !if(j_otype(ii).eq.j_ipreal)then
					if(j_otype(ii).ne.j_ipmatrix)then
						call j_getname(ii)
						write(6,*)'values-argument ',j_oname(1:j_loname), ' neither real nor matrix'
						call printvalues()
						j_err=.true. ;return
					endif !if(j_otype(ii).ne.j_ipmatrix)  24019
					niro=j_o(ii)%i(1)
					ncols=j_o(ii)%i(2)
					if(ncols.ne.niro)then
						call j_getname(ii)
						write(6,*)'matrix ',j_oname(1:j_loname),' is not square'
						j_err=.true.
						return
 
					endif !if(ncols.ne.niro)  24027
				endif !if(j_otype(ii).eq.j_ipreal)  24015
				nrows=nrows+niro
			enddo !iro=1,ndim1  24006
			iout=j_defmatrix(iout,' ',nrows,nrows,j_matreg)
			iva=1
 
			ibas=0
 
			do iro=1,ndim1
				ii=j_optarg0(iva)
				iva=iva+ivastep
 
				if(j_otype(ii).eq.j_ipreal)then
					j_o(iout)%d(nrows+ibas+1)=j_v(ii)
					niro=1
				else !if(j_otype(ii).eq.j_ipreal)then
 
					ie=1
					niro=j_o(ii)%i(1)
					do ir0=1,niro
						do ir=1,niro
							j_o(iout)%d(ibas+ir)=j_o(ii)%d(ie)
							ie=ie+1
						enddo !ir=1,niro  24054
						!		write(6,*)'iro, ndim1,nrows,ibas',iro,ndim1,nrows,ibas
						ibas=ibas+nrows
					enddo !ir0=1,niro  24053
					ibas=ibas+niro
				endif !if(j_otype(ii).eq.j_ipreal)  24046
 
			enddo !iro=1,ndim1  24042
 
			return
		endif !if(diag)  23999
		!*****************matmat and diags
 
		nrows=0
 
		do iro=1,ndim1
			ii=j_optarg0((iro-1)*ndim2+1)
			!		write(6,*)'ndim1,iro,ii',ndim1,iro,ii,'iout',iout
			!	write(6,*)'otype',j_otype(ii)
			if(ii.eq.iout)inout=.true.
			if(j_otype(ii).eq.j_ipreal)then
				niro=1
				ncols=1
			else !if(j_otype(ii).eq.j_ipreal)then
				if(j_otype(ii).ne.j_ipmatrix)then
					call j_getname(ii)
					write(6,*)'values-argument ',j_oname(1:j_loname), ' neither real nor matrix'
					call printvalues()
					j_err=.true. ;return
				endif !if(j_otype(ii).ne.j_ipmatrix)  24081
				niro=j_o(ii)%i(1)
				ncols=j_o(ii)%i(2)
			endif !if(j_otype(ii).eq.j_ipreal)  24077
			nrows=nrows+niro
			do j2=2,ndim2
				ii=j_optarg0((iro-1)*ndim2+j2)
				if(ii.eq.iout)inout=.true.
				if(j_otype(ii).eq.j_ipreal)then
					niro2=1
					ncol=1
				else !if(j_otype(ii).eq.j_ipreal)then
					if(j_otype(ii).ne.j_ipmatrix)then
						call j_getname(ii)
						write(6,*)'values-argument ',j_oname(1:j_loname), ' neither real nor matrix'
						call printvalues()
						j_err=.true.;return
					endif !if(j_otype(ii).ne.j_ipmatrix)  24098
					niro2=j_o(ii)%i(1)
					ncol=j_o(ii)%i(2)
				endif !if(j_otype(ii).eq.j_ipreal)  24094
				if(niro2.ne.niro)then
					call j_getname(iout)
 
					write(6,*)'values-argument ',j_oname(1:j_loname),' in row ',iro, &
						' does not have same number of rows as the first in the row'
					call printvalues()
					j_err=.true.;return
				endif !if(niro2.ne.niro)  24107
				ncols=ncols+ncol
 
			enddo !j2=2,ndim2  24091
			if(iro.gt.1.and.ncols.ne.ncolsv)then
				write(6,*)'row ',iro,' does not have the same number of columns as previous '
				call printvalues()
				j_err=.true.;return
			endif !if(iro.gt.1.and.ncols.ne.ncolsv)  24118
			ncolsv=ncols
 
		enddo !iro=1,ndim1  24072
		if(inout)then
			allocate(temp2(1:nrows*ncols))
			outd=>temp2
		else !if(inout)then
 
			iout=j_defmatrix(iout,' ',nrows,ncols,itype)
			if(p)write(6,*)'<890',nrows,ncols,size(j_o(iout)%d)
			outd=>j_o(iout)%d
		endif !if(inout)  24126
		nrows0=0
		do iro=1,ndim1
			ncols0=0
			do j2=1,ndim2
				ii=j_optarg0((iro-1)*ndim2+j2)
 
				if(j_otype(ii).eq.j_ipreal)then
					ncols0=ncols0+1
					outd(nrows0*ncols+ncols0)=j_v(ii)
					nrow0=1
				else !if(j_otype(ii).eq.j_ipreal)then
					nrow0=j_o(ii)%i(1)
					ncol0=j_o(ii)%i(2)
					do iri=1,nrow0
						do jic=1,ncol0
							outd((nrows0+iri-1)*ncols+ncols0+jic)=j_o(ii)%d((iri-1)*ncol0+jic)
 
						enddo !jic=1,ncol0  24149
					enddo !iri=1,nrow0  24148
					ncols0=ncols0+ncol0
				endif !if(j_otype(ii).eq.j_ipreal)  24141
			enddo !j2=1,ndim2  24138
			nrows0=nrows0+nrow0
 
 
		enddo !iro=1,ndim1  24136
		if(inout)then
			!write(6,*)'n55',nrows,ncols
 
			iout=j_defmatrix(iout,' ',nrows,ncols,itype,temp=temp2)
			!write(6,*)'n55',nrows,ncols,size(j_o(iout)%d)
			!	j_o(iout)%d=temp2
			!write(6,*)'n57',nrows,ncols,size(j_o(iout)%d)
			!	deallocate(temp2)
		endif !if(inout)  24161
		!	write(6,*)j_object_name(iout,15),'is ', nrows,' X ',ncols,' matrix'
		!	write(6,*)' ';
		return
 
		!
 
 
 
	end subroutine
	subroutine readval()
		!		nva=j_o(iob)%i(j_linkoption(iob,io,j_mvalues))
		!write(6,*)'tas2'
		if(nva.eq.1)then  !constant value
			val=j_v(j_optarg0(1))
			!	write(6,*)'val',val,' diag',diag, ndim1,ndim2
			if(diag)then
				j_o(iout)%d(1:ndim1*ndim2)=j_0
				iba=1
				do i=1,min(ndim1,ndim2)
					j_o(iout)%d(iba)=val
					iba=iba+ndim2+1
				enddo !i=1,min(ndim1,ndim2)  24188
			else !if(diag)then
				j_o(iout)%d(1:ndim1*ndim2)=val
			endif !if(diag)  24185
			!		write(6,*)'tas3'
		else !if(nva.eq.1)then
			if(diag)then
				j_o(iout)%d(1:ndim1*ndim2)=j_0
				iba=1
				do j=1,min(ndim1,ndim2,nva)
					j_o(iout)%d(iba)=valu(j)  !j_v(j_optarg0(j))
					iba=iba+ndim2+1
				enddo !j=1,min(ndim1,ndim2,nva)  24200
			else !if(diag)then
 
				j_o(iout)%d(1:nva)=valu  !j_v(j_optarg0)   !
 
				!do j=1,min(ndim1*ndim2,j_o(iob)%i(j_linkoption(iob,io,j_mvalues)) )
			endif !if(diag)  24197
		endif !if(nva.eq.1)  24182
 
 
 
 
	endsubroutine
 
	subroutine printvalues()
		write(6,*)'the dimensions of values-> matrices are:'
		do j=1,nva
			ii=j_optarg0(j)
			call j_getname(ii)
			if(j_otype(ii).eq.j_ipmatrix)then
				write(6,*)j_oname(1:j_loname),j_o(ii)%i(1:2)
			elseif(j_otype(ii).eq.j_ipreal)then !if(j_otype(ii).eq.j_ipmatrix)then
				write(6,*)j_oname(1:j_loname),' is REAL with value ',j_v(ii)
			else !if(j_otype(ii).eq.j_ipmatrix)then
				write(6,*)j_oname(1:j_loname), ' has wrong type ',j_otypes(j_otype(ii))
			endif !if(j_otype(ii).eq.j_ipmatrix)  24222
		enddo !j=1,nva  24219
		return
	end subroutine !subroutine printvalues()
 
end subroutine matrix !subroutine matrix(iob,io)
 
subroutine trans(iob,io)  ! define transformation set   output=trans()
	! Section Transformations Transformation objects
	! The code lines generated by the input programming can be either
	! executed directly after interpretation, or the interpreted code lines
	! are packed into a transformation object, which can be excuted with call()
	! which is either in the code generated with the input programming
	! or inside the same or other transformation object. Recursive
	! calling a transformation is thus also possible. Different functions
	! related to transformation object are described in this section.
	! endsection
 
 
	!Section trans trans() Creates a TRANS (transformation) object
	!trans() function interprets lines from input paragraph following the trans() command and puts the
	!  interpreted code into an integer vector, which can be excuted in several places.
	! If there are no arguments in the function, the all objected used within the
	! transforamations are global. This may cause conflicts if there are several recursive
	! functions operating at the same time with same objects. Jlp22 checks some of
	! these conflict situations, but not all.  These conflicts can be avoided by giving
	! intended global arguments  in the list of arguments.
	! Then an object 'ob' created e.g. with transformation object ]tr[ have prefix
	! ]tr/\[ yelding ]tr/\ob[. Actually also these objects are global, but their prefix
	! protects them so that they do not intervene with objects having the same name in the
	! calling transformation objec.
 
	! Each line in the input paragraph is read and interpreted and packed into a transformation
	! object, and associated tr%input and tr%output lists are created for input and output
	! variables. Objects can be in both lists. Objects having names starting
	! with '$' are not put into the input or output lists. The source code is saved in a text object
	! tr%source. List tr%arg contains all arguments.
	!
	!If a semicolon ';'  is at the end of an input line, then
	! the output is printed if REAL variable Prindebug has value 1 or value>2 at
	! the execution time. If the double semicolon ';;' is at the end then the output is
	! printed if printresult>1. If there is no output, but just list of objects, then these
	!objects will be printed with semicolns.
 
	!
	!endheader
	!Option
	!Output&1&Data&The TRANS object generated.
	!Args&N|1-& & Global objects.
	!endoption
	!Note Options input->, local->, matrix->, arg->, result->, source-> of previous
	!versions are obsolte.
	!endnote
	!Note The user can intervene the execution from console if the code calls read($,),
	! ask(), askc() or pause() functions. During the pause one can give any command excepts
	! such input programming command as ;incl.
	!endnote
	!Note The value of printresult can be changed in other parts of the transformation, or
	! in other transforamations called or during execution of pause().
	!endnote
 
	!Note Output variables in maketrans-> transformations whose name start with $ are not put into the new data object.
	!endnote
	!Ex transex Demonstrates also error handling
	!transa=trans()
	!$x3=x1+3
	!x2=2/$x3;
	!/
	!transa%input,transa%output,transa%source;
	!x1=8
	!call(transa)
	!transb=trans(x1,x2)
	!$x3=x1+3
	!x2=2/$x3;
	!x3=x1+x2+$x3;
	!/
	!transb%input,transb%output,transb%source;
	!call(transb)
	!transb|x3;   !x3 is now local
	! transc=trans()
	! x1=-3
	! call(transb) !this is causing division by zero
	! /
	!Continue=1   ! continue after error
	!call(transc)
	!endex
	!Listing
	! sit>transex
	! <;incl(exfile,from->transex)
 
 
	! <transa=trans()
	! <$x3=x1+3
	! <x2=2|$x3;
	! </
	! <transa%input,transa%output,transa%source;
	! transa%input is list with            2  elements:
	! x1 $x3
	! transa%output is list with            1  elements:
	! x2
	! transa%source is text object:
	! 1 $x3=x1+3
	! 2 x2=2/$x3;
	! 3 /
	! ///end of text object
 
	! <x1=8
 
 
	! <call(transa)
	! x2=0.18181818
 
	! <transb=trans(x1,x2)
	! <$x3=x1+3
	! <x2=2/$x3;
	! <x3=x1+x2+$x3;
	! </
	! <transb%?;
 
 
	! <call(transb)
 
 
	! <transc=trans()
	! <x1=-3
	! <call(transb)
	! </
	! <Continue=1
	! <call(transc)
	! *division by zero
	! *****error on row            2  in tr%source
	! x2=2/$x3;
	! recursion level set to    3.0000000000000000
 
	! *****error on row            2  in transc%source
	! call(transa)
	! recursion level set to    2.0000000000000000
 
	! *err* transformation set=$Cursor$
	! recursion level set to    1.0000000000000000
	! ****cleaned input
	! call(transc)
	! *Continue even if error has occured
	! <;return
	!endlisting
	!endsection
 
	logical islocal,iscall
	character*40 tname
	integer,dimension(:),pointer::list=>null(),localin=>null(),localout=>null()
	!	integer,dimension(:),pointer::arg
	logical::p=.false.
	!	p=j_v(j_ivdebug).ge.300
	!	call j_getname(iob)
	!		write(6,*)'<33TRANS ',j_oname(1:j_loname)
	call j_startfunction(iob,io,0,nm,j_optarg0,iout);if(j_err)return
	!write(6,*)'<88trans type ',j_otype(iout)
	!write(6,*)'<88trans type ',allocated(j_o(iout)%i),allocated(j_o(iout)%i2),allocated(j_o(iout)%ch)
 
 
 
	if(j_err)return
	iscall=.false.
	!	subroutine j_getoption_name(iob,option,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
 
	linsource=200
	if(nm.gt.0)then
		ivarg_=j_deflist(iout,'%arg',list0=nm,list=j_optarg0)
		ivtrans=j_deftrans(iout,' ',4000,400,400,ivinputl,ivoutputl,ivlocal,linsource,ivarg=ivarg_,istrans=.true.)
	else !if(nm.gt.0)then
		ivtrans= j_deftrans(iout,' ',4000,400,400,ivinputl,ivoutputl,ivlocal,linsource,istrans=.true.)
		!	write(6,*)'<33ivtrans',ivtrans,j_otype(ivtrans),j_iptrans
	endif !if(nm.gt.0)  24394
	if(j_err)return
	j_o(ivtrans)%i(0)=0
 
	inprint=0
	if(j_v(j_ivprintinput).ge.3.)inprint=1
	iprin=j_igetopt(iob,io,j_mprint)
	if(iprin.eq.0)then
		inprint=1
	elseif(iprin.gt.0)then !if(iprin.eq.0)then
		inprint=j_v(iprin)
	endif !if(iprin.eq.0)  24407
	ivargu=j_igetopt(iob,io,j_marg)
	if(ivargu.gt.0)j_o(iout)%i2(9)=ivargu
	!	ivres=j_igetopt(iob,io,j_mresult)
	if(ivres.gt.0)j_o(iout)%i2(10)=ivres
 
	call j_getoption(iob,io,j_min,-1,1,j_iptext,.true.,nin,j_optarg0)
	if(j_err)return
	!	call j_clear
	!!call j_clearoption(iob,io)  ! subroutine
	!return
	!if(nin.gt.0)write(6,*)'<569nin',nin
	if(nin.gt.0)then
		ivtext=j_optarg0(1)
		if(j_otype(ivtext).ne.j_iptext)then
			call j_getname(ivtext)
			write(6,*)'*object ',j_oname(1:j_loname),' is not TEXT'
			j_err=.true.;return
		endif !if(j_otype(ivtext).ne.j_iptext)  24425
		call j_gettext(ivtext,j_o(ivtext)%i(0))
		if(j_gottext(1:j_lgottext).ne.'/')then
			!	call j_getname(ivtext)
			!				write(6,*)'the last line of ',j_oname(1:j_loname),' should be / but let it be this time'
			call j_puttext(ivtext, '/')
		endif !if(j_gottext(1:j_lgottext).ne.'/')  24431
		!		write(6,*)'<4664ivtext ',ivtext,ivtrans
		call j_getname(ivtrans)
		!	write(6,*)'<7778>',j_oname(1:j_loname)
		call j_parser('trans',ivtrans,ivtext)
	else
		call j_parser('trans',ivtrans)
	endif !if(nin.gt.0)  24423
 
	!	write(6,*)'<54>return from trans,io',io,j_o(iob)%i(0:15)
	!	write(6,*)'<33ivtrans2',ivtrans,j_otype(ivtrans),j_iptrans
 
end subroutine trans !subroutine trans(iob,io)
 
! subroutine list(iob,io)  !%%list
! !defines from compiled transforamtion list
! integer, dimension(:), pointer :: arg !arguments of the function
! integer, dimension(:), pointer :: mask !arguments of the function
! logical expand !,ismask
! logical ok,different
! !defines from compiled transforamtion list
! !subroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout) ! %%function
! expand=j_linkoption(iob,io,j_mexpand).ge.0
! call j_startfunction(iob,io,0,expand,narg,arg,ivout)
! !subroutine j_deflist(iv,name,list0,ivout) !allocates list object with size list0,
! ! but put it as empty
! ! subroutine j_getoption_name(iob,option,minarg,maxarg,iptype,expand,min0,noptarg,optarg)
! call j_getoption(iob,io,j_mmask,-1,9999,j_ipreal,.true.,narg2,mask)
! if(j_err)return
! !ismask=j_linkoption(iob,io,j_mmask).ge.0
! !!call j_clearoption(iob,io)
! !if(narg.eq.1.and.j_otype(ivarg_).eq.j_ipchar)then
! if(narg2.lt.0.and.(narg.gt.1.or.(narg.eq.1.and.j_otype(arg(1).ne.j_ipchar)))then
! do i=1,narg
! if(arg(i).gt.j_named)then
!write(6,*)'argument ',i,' is not named object'
! j_err=.return.
! endif
! enddo
! if(j_err)return
! call  j_deflist(ivout,' ',arg,iout)
! j_o(iout)%i(1:narg)=arg
! return
! elseif(narg2.gt.0)then
 
! nel=0
! nel2=0  ! how many elements specified
! narg2=j_o(iob)%i(j_linkoption(iob,io,j_mmask))
! do i=1,narg2
! nval=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmask)+i))
! if(nval.gt.0)nel=nel+nval
! if(nval.eq.0)nval=-1
! nel2=nel2+abs(nval)
! end do !do i=1,narg2
! if(nel.le.0)then
!write(6,*)'*wrn: all mask elements false, no list generated';goto 900
! end if !if(nel.le.0)then
! if(nel2.lt.narg)write(6,*)'*wrn* mask shorter, rest rejected'
! if(nel2.gt.narg)write(6,*)'*wrn* mask longer, tail ignored'
 
! call j_getobject(iout,' ',j_iplist,iv2) !call getv2(iplist,ivout(teku),ior
! allocate( j_o(iv2)%i(0:nel))
! nel=0
! nel2=0
! do i=1,narg2
! nval=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmask)+i))
! if(nval.gt.0)then
! do j=1,nval
! nel=nel+1
! nel2=nel2+1
! if(nel.lt.narg)j_o(iv2)%i(nel)=j_o(iob)%i(io+1+nel2)
! end do !do j=1,nval
! else if(nval.eq.0)then !if(nval.gt.0)then
! nel2=nel2+1
! else !if(nval.gt.0)then
! nel2=nel2-nval
! end if !if(nval.gt.0)then
! end do !do i=1,narg2
! j_o(iv2)%i(0)=nel
! !!call j_clearoption(iob,io)  ! subroutine
! return
! elseif(narg.eq.1.and.j_otype(ivarg_).eq.j_ipchar)then !if(j_linkoption(iob,io,j_mmask).ne.0)then
 
! call j_getobject(iout,' ',j_iplist,iv2) !call getv2(iplist,ivout(teku),ior
! ivarg_=arg(1)
! !	if(narg.eq.1.and.j_otype(ivarg_).eq.j_ipchar)then
 
! call j_getline(j_ivnames,ivarg_,j_tempchar2,le2)
! isq=index(j_tempchar2(1:le2),'?')   ! first and last character is '
! if(isq.eq.2.and.le2.eq.3) then  ! '?'  -> all named variables
! allocate( j_o(iv2)%i(0:j_named ))
! do i=1,j_named  ;  j_o(iv2)%i(i )=i ;enddo
! j_o(iv2)%i(0) =j_named
! goto 900
! endif !if(isq.eq.2.and.le2.eq.3) then
! different=j_tempchar2(2:2).eq.'-'.and.le2.gt.3
! ledif=le2-3
! isq2=le2+1
! if(isq.gt.0)isq2=j_nextlim(j_tempchar2,isq+1,le2,'?')
! ok=.false.
 
! if(isq.gt.0.or.different)then
! nfound=0
! 17                 do i_=1,j_named
! call j_getline(j_ivnames,i_,j_tempchar3,le3)
! if(j_err)return
! if(different.and.le3.ge.ledif)then  !'!x'
! if(j_tempchar2(3:le2-1).eq.j_tempchar3(1:ledif))cycle
! goto 11
! endif !if(different.and.le3.ge.ledif)then
! if(isq.eq.2.and.isq2.eq.le2-1)then    !   '?x?'
! if(index(j_tempchar3(1:le3),j_tempchar2(3:le2-2)).gt.0 )goto 11
! cycle
! endif !if(isq.eq.2.and.isq2.eq.le2-1)then
! if(isq.gt.2)then  !there is something before ?     'x?'  or 'x?y'   ~
! if(le3.lt.isq-2)cycle
! if(j_tempchar3(1:isq-2).ne.j_tempchar2(2:isq-1))cycle
 
! endif !if(isq.gt.2)then
! loppupit=le2-isq-1
! if(loppupit.gt.0)then  !there is something after ?  '?x'  or 'x?y'
! if(le3.lt.loppupit)cycle
! if(j_tempchar3(le3-loppupit+1:le3).ne.j_tempchar2(isq+1:le2-1))cycle
 
! endif !if(loppupit.gt.0)then
 
! 11                  nfound=nfound+1
! if(ok)j_o(iv2)%i(nfound)=i_
 
! enddo !do i_=1,j_named
 
! if(nfound.le.0)then
! allocate( j_o(iv2)%i(0:0 ))
! j_o(iv2)%i(0)=0
! goto 900
! endif !if(nfound.le.0)then
! if(ok)goto 900
! allocate( j_o(iv2)%i(0:nfound ))
! ok=.true.  !collect now'
! j_o(iv2)%i(0)=nfound
! nfound=0
! goto 17
! endif !if(isq.gt.0.or.different)then
! endif !if(narg.eq.1.and.j_otype(ivarg_).eq.j_ipchar)then
! if(j_err) return
! allocate( j_o(iv2)%i(0:narg ))
! j_o(iv2)%i(0)=narg
! do i=1,narg;j_o(iv2)%i(i)=j_o(iob)%i(io+1+i);end do
! end if !if(j_linkoption(iob,io,j_mmask).ne.0)then
 
! 900 continue !  io=io+narg+3
! return
! end subroutine list !subroutine list(iob,io_)
subroutine list(iob,io)  !%%list
	!Section list list() Creates LIST
	!endheader
	!Option
	!Output& 1& LIST &The generated LIST object.
	!Args& 0- &  & named objects.
	!merge&N& & Duplicates are dropped.
	!do&-1-&REAL& Consecutive values are given to variables in the LIST. If do->
	!has no arguments, values 1,2,.. are used. If do-> has one argument, it gives the starting value.
	!If do-> has two arguments, the first argument gives the starting value and
	! the second argument gives the step.
	!mask& N|1- &REAl& Which object are picked from the list of arguments.
	! value 0 indicates that
	! he object is dropped, positive value indicates how many variables are taken,
	!negative value how many objects are dropped (thus 0 is equivalent to -1). mask-
	!option is useful for creating sublists of long lists.
	!endoption
	!Note The same object may appear several times in the list. (see merge())
	!endnote
	!Note There may be zero arguments, which result in an empty list
	!which can be updated later.
	!endnote
	!Note Also matrix() has option do-> for similar purpose as list(), but
	! it works slightly differently as in matrix(), do-> can determine
	! the dimensions of generated MATRIX and it is possible to fill only part of
	! the MATRIX.
	!endnote
	!Note The index of object in a LIST can be obtained using index().
	!endnote
	!Note The elements of lists can be changed using the new setpointer() function
	!indicated with =>. E.g. li(4)=>x meand that 4th element will be x.
	!endnote
	!Note Versions before March 2023 allowed list arguments which were then expanded.
	!This property is dropped because lists can be included better with @-cosnstruct.
	!endnote
	!Ex  listex
	! li=list(x1...x5);
	!li=list(x1...x5,merge->);
	!li=list(x1...x5,x2...x4,merge->);
	! index(x2,li);
	!***returns zero
	! index(x6,li);
	!***
	! all=list();  ! empty list
	! sub=list();
	! nper=3
	! ;do(i,1,nper)
	! period#"i"=list(ba#"i",vol#"i",age#"i",harv#"i")
	! sub#"i"=list(@period#"i",mask->(-2,1,-1))
	! all=list(@all,@period#"i") !note that all is on both sides
	! sub=list(@sub,@sub#"i")
	! ;end do
	! li=list(x1...x13,do->);
	! li=list(x1...x13,do->4)
	! li=list(x1...x13,do->(3,2));
	!endex
	!endsection
 
	!Section symconst Symbolic constants
	!REAL variables having some fixed value are can be considered as constants. Only
	! some constants created at the initialization are constants in the sense that their value
	! cannot be changed later. For some time I made a possiblity to lock the values of other
	! variables also, but this was not very useful. But in a DATA having symbolic names for cases, the
	! names for cases can be considered as symbolic contants. In DATA with cases data%case is
	! a list having the case names as elements. These case names are actually ordinary REAL
	! variables but with the special property that the value of ith element is equal to i.
	!This way the case name tells what is the observation number, which makes it possible to
	! acces a row in data matrix using the case name. Other lists of symbolic constants can be
	! made with list2sym() function, which can pick from the names of varaibles in the list a new LIST
	! having this property. The purpose of symbolic constants is give a tag which ca be used to tell
	! that subjects occuring in different parts of the code refer to the same subject. When
	! the symbolic constant is in the case-list of 	a DATA, the value of the constant tells in what
	! observation of the DATA further properties are defined. The namenum() and namenum%() functions allow
	! to maintain also numeric variables in the name of the constant. This way it is not necessary
	! to look these quantitative properties from the DATA because this infomation is carried in ther name.
	!This is a handy way to maintain indexes without the need to establish permanent indexes.
	! 	I will use this property in a forthcoming
	! application where Jlp22 is used for optimization of blade settings in saw mills.
	!endheader
	!endsection
 
 
	!defines from compiled transforamtion list
	logical ok,different,ismerge
	call j_startfunction(iob,io,0,narg,j_arg,iout);if(j_err)return
	call j_checkoutput0(iob,io)  !list cannot refer to itself
	if(j_err)return
	!	io=io_
	!	narg=j_o(iob)%i(io+1)
	! io_=io_+narg+3
 
	!	iout=j_o(iob)%i(io+narg+2)
	ismerge=j_isoption(iob,io,j_mmerge)
	if(narg.eq.0)then
		iv2=j_deflist(iout,' ',nres=20)
		return
	endif !if(narg.eq.0)  24681
 
	call j_getoption(iob,io,j_mmask,-1,9999,j_ipreal,.true.,nmask,j_optarg0)
	!!call j_clearoption(iob,io)  ! subroutine
	if(j_err)return
	call j_getoption(iob,io,j_mdo,-1,99999,j_ipreal,.false.,ndo,j_optarg)
	if(j_err)return
 
	!	issub=j_linkoption(iob,io,j_mup,clear=.true.).ge.0
 
	!	write(6,*)'LISTnarg,iout',narg,iout,issub
	if(nmask.gt.0)then
		nel=0
		nel2=0  ! how many elements specified
		!	narg2=j_o(iob)%i(j_linkoption(iob,io,j_mmask))
		do i=1,nmask
			nval=j_v(j_optarg0(i))  !j_o(iob)%i(j_linkoption(iob,io,j_mmask)+i))
			if(nval.gt.0)nel=nel+nval
			if(nval.eq.0)nval=-1
			nel2=nel2+abs(nval)
		end do !i=1,nmask  24699
		if(nel.le.0)then
			write(6,*)'*wrn: all mask elements false, no list generated';return
		end if !if(nel.le.0)  24705
		if(nel2.lt.narg)write(6,*)'*wrn* mask shorter, rest rejected'
		if(nel2.gt.narg)write(6,*)'*wrn* mask longer, tail ignored'
 
		!	call j_getobject(iout,' ',j_iplist,iv2) !call getv2(iplist,ivout(teku),ior
		!	allocate( j_o(iv2)%i(0:nel))
		iv2=j_deflist(iout,' ',nres=nel)
		!if(p)write(6,*)'<aa8',j_o(ivkeep)%i,'*',j_o(ivkeep)%i2
		nel=0
		nel2=0
		do i=1,nmask
			nval=j_v(j_optarg0(i))
			if(nval.gt.0)then
				do j=1,nval
					nel=nel+1
					nel2=nel2+1
					if(nel.lt.narg)j_o(iv2)%i2(nel)=j_arg(nel)
				end do !j=1,nval  24720
			else if(nval.eq.0)then !if(nval.gt.0)then
				nel2=nel2+1
			else !if(nval.gt.0)then
				nel2=nel2-nval
			end if !if(nval.gt.0)  24719
		end do !i=1,nmask  24717
		j_o(iv2)%i(1)=nel
 
	else !if(j_linkoption(iob,io,j_mmask).ge.0)then
 
 
		if(ismerge)then
			iv2=j_deflist(iout,' ',nres=narg)
			j_o(iv2)%i2(1)=j_arg(1)
			n=1
loop:		do i=2,narg
				do j=1,i-1
					if(j_arg(i).eq.j_arg(j))cycle loop
				enddo !j=1,i-1  24741
				n=n+1
				j_o(iv2)%i2(n)=j_arg(i)
			enddo loop !p:		do i=2,narg  24740
			j_o(iv2)%i(1)=n
		else
			iv2=j_deflist(iout,' ',listold=j_o(iob)%i(io+1:io+1+narg))
 
		endif !if(ismerge)  24736
		!	write(6,*)'LIST',j_o(iout)%i,'**#',j_o(iout)%i2
		!allocate( j_o(iv2)%i(0:narg ))
		!j_o(iv2)%i(0)=narg
		!	j_o(iv2)%i2(1:narg)=j_o(iob)%i(io+2:io+1+narg) !2=1+1
		!	do i=1,narg;j_o(iv2)%i(i)=j_o(iob)%i(io+1+i);end do
	end if !if(nmask.gt.0)  24695
 
	!	900 continue !  io=io+narg+3
	if(ndo.ge.0)then !if(nva.le.0.and.ndo.eq.0)then
		nel=j_o(iout)%i(1)
		if(ndo.eq.0)then
			do i=1,nel
				j_v(j_o(iout)%i2(i))=i
			enddo !i=1,nel  24763
			return
		endif !if(ndo.eq.0)  24762
		if(ndo.eq.1)then
			do i=1,nel
				j_v(j_o(iout)%i2(i))=i-1+j_v(j_optarg(1))
			enddo !i=1,nel  24769
			return
		endif !if(ndo.eq.1)  24768
		if(ndo.eq.2)then
 
 
			do i=1,nel
				j_v(j_o(iout)%i2(i))=j_v(j_optarg(1))+(i-1)*j_v(j_optarg(2))
			enddo !i=1,nel  24777
			return
		endif !if(ndo.eq.2)  24774
 
 
		write(6,*)'do-> needs 0 ,1 , or 2 arguments'
		j_err=.true. ;return
	endif !if(ndo.ge.0)  24760
	!		write(6,*)'ndo',ndo,doval
 
	return
end subroutine list !subroutine list(iob,io)
 
 
 
! subroutine param(iob,io)     !
 
! narg=j_o(iob)%i(io+1)
! irg=j_o(iob)%i(io+2)
! arg=j_v(j_o(iob)%i(io+3))
! iout=j_o(iob)%i(io+2+narg)
! select case(j_otype(irg))
! case(j_ipsmooth) !select case(j_otype(irg))
! inde=arg
! if(inde.le.0.or.inde.gt.7)then
! write(6,*)'**param(smoothing_spline,index), index got value ',inde,' max =7'
! j_err=.true.
! end if !if(inde.le.0.or.inde.gt.7)then
! if(j_otype(iout).ne.j_ipreal)call j_del(iout)
! j_v(iout)=j_o(irg)%d(inde)
! case default !select case(j_otype(irg))
! call j_printname('**not legal obj for param(obj, ):',irg,' ')
! j_err=.true.
 
! end select !select case(j_otype(irg))
 
! !io=io+narg+3
! return
! end subroutine !subroutine param(iob,io)
 
subroutine stempolar(iob,io)  !
 
	! Section stempolar stempolar() Puts a stem into polar coordinates
	! To be reported later
	! endsection
 
 
	dimension acal(4),root(3)
	!parameter ( deg=0.017453293)  !*deg makes degrees to radians
	call j_checkoutput(iob,io)
	if(j_err)return
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	ispl=j_o(iob)%i(io+2)
	arg=j_v(j_o(iob)%i(io+3))
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(ispl).ne.j_ipstemspline)then
		call j_printname('**not legal argument for stempolar:',ispl,' ')
		j_err=.true.
		return
	end if !if(j_otype(ispl).ne.j_ipstemspline)  24837
	if(j_linkoption(iob,io,j_morigo).gt.0)then
		if(j_o(iob)%i(j_linkoption(iob,io,j_morigo)).ne.1)then
			write(6,*)'**stempolar: origo->  must give height of the origo'
			j_err=.true.
			return
 
		endif !if(j_o(iob)%i(j_linkoption(iob,io,j_morigo)).ne.1)  24843
		origo=100.*j_v(j_o(iob)%i(j_linkoption(iob,io,j_morigo)+1) )
 
	else !if(j_linkoption(iob,io,j_morigo).gt.0)then
		origo=0.
	endif !if(j_linkoption(iob,io,j_morigo).gt.0)  24842
	iverr=j_igetopt(iob,io,j_merr)
	!!call j_clearoption(iob,io)  ! subroutine
	npo=j_o(ispl)%i(1)
	npo2=j_o(ispl)%i(2)
	lir=1
	lirh=1+npo
	lirsp=lirh+npo
 
	cotang=0.01*j_cotan(dble(j_deg*arg))
	!* for angles height is in meters and diamter in centimeters
	!* in splines they are both in centimeters
	do 441 L=1,npo2-1
		!* h/d>tand ->d/(h-origo) <cotang ->d<h*cotang
		!* d,h.splmat
		!* first point tpo be checked is the second
		dref=cotang*(j_o(ispl)%d(lirh+L)-origo)
		if(abs(j_o(ispl)%d(lir+L)-dref).lt.0.001)then
			j_v(iout)=dref
			goto 90
		endif !if(abs(j_o(ispl)%d(lir+L)-dref).lt.0.001)  24870
 
		if(j_o(ispl)%d(lir+L).le.dref)goto 221
 
		441       continue !441 L=1,npo2-1  24865
		!*UBROUTINE  CUBEQ (A,X,N)
		!C  THE SUBROUTINE DETERMINES THE REAL ROOTS OF THE CUBIC EQUATION
		!C
		!C     A(1) * X**3  +  A(2) * X**2  +  A(3) * X  +  A(4)  =  0
		!*h=hala+r
		!C     D(N)    D(I) IS THE DIAMETER AT POINT X(I)
		!C     SM(4,N1) SPLINE MATRIX , AT (X(I),X(I+1)) THE SPLINE IS
		! C     SM(1,I)*(X-X(I))**3+SM(2,I)*(X-X(I)**2+SM(3,I)*(X-X(I))+SM(4,I)

		! D=a*r**3+b*r**2+c*r+d
		! r=h-alaraja
		! ->
		!* D/(H-origo)=cotan(alfa); D=cotan*(H-origo)
		!cotan*(H-origo)=a*r**3+b*r**2+c*r+d
		!-> =a*r**3+b*r**2+c*r+d-cotan*(H-origo)=0
		! H=knot+r
		!=a*r**3+b*r**2+c*r+d-cotan*(knot+r-origo)=0
		!=a*r**3+b*r**2+(c+cotan)*r+d-cotan*(knot-origo)=0

		!* D/(H-origo)=cotan(alfa); d=cotan*(H-origo),D is cubic in r,H->a*r**3+b*r**2+(c-cotan)H+d=0
		!* ar***2+b*r**2+cr+d=cotan(hala+r)/100
		!*ar***2+b*r**2+cr+d=cotang(hala+r)
		!*where to find spline coefficients
		!*check if close t
		! here the ange goes to the tip of the tree
		!htot=o(ispl)%d(lirh+npo2-1)
		! D/(H-origo)=cotanng  -> d=cotang*(h-origo)
	j_v(iout)=cotang*(j_o(ispl)%d(lirh+npo2-1)-origo)
	goto 90
 
	!check first if very close teh knot
		221  continue

	ii=lirsp+(L-1)*4-1
 
	do  i=1,4
		acal(i)=  j_o(ispl)%d(ii+i) !do 222 i=1,4
	enddo ! i=1,4  24913
	acal(3)=acal(3)-cotang
	acal(4)=acal(4)-cotang*(j_o(ispl)%d(lirh+L-1)-origo)
	!* ONE HEIGHT BETWEEN X(L) AND X(L+1) WHERE THE
	!C     DIAMETER IS Z BUT THERE MIGHT BE MORE. IN THAT CASE THE
	!C     SMALLEST IS CHOSEN:
	CALL CUBEQ(ACAL,ROOT,NROOT)
 
	!* L is index of range
 
	if(nroot.lt.1)then
		write(6,*)'**stempolar,noo roots'
		goto 99
	end if !if(nroot.lt.1)  24925
	R = ROOT(1)
	if(L.eq.1.and.j_o(ispl)%d(1).le.cotang*(j_o(ispl)%d(lirh)-origo))then
		!* before first knot
 
		IF (NROOT.GT.1 .AND.( r.gt.0.01.or.-r.gt.(j_o(ispl)%d(lirh)-origo)))R = ROOT(2)
		IF (NROOT.EQ.3 .AND. (r.gt.0.01.or.-r.gt.(j_o(ispl)%d(lirh)-origo) ))R = ROOT(3)
		if(r.gt.0.01.or.-r.gt.j_o(ispl)%d(lirh)-origo)then
			write(6,*)'**stempolar, err=3',nroot,root,l,j_o(ispl)%d(lirh)
			goto 99
 
		end if !if(r.gt.0.01.or.-r.gt.j_o(ispl)%d(lirh)-origo)  24935
	else !if(L.eq.1.and.j_o(ispl)%d(1).le.cotang*(j_o(ispl)%d(lirh)-origo))then
		!*after first knot
 
		sero=j_o(ispl)%d(lirh+L)-j_o(ispl)%d(lirh+L-1)+0.01
		IF (nroot.gt.1.and.( R.LT.-0.01.or.r.gt.sero))  R = ROOT(2)
		IF (NROOT.EQ.3 .AND.( R.LT.-0.01.or.r.gt.sero))  R = ROOT(3)
 
		IF (R.LT.-0.01 .OR.r.gt.sero)then
 
			write(6,*)'**stempolar, err=1'
			write(6,*)'nroot,root ',nroot,root,' origo ',origo, 'r=',r,'sero',sero
			write(6,*)'L,hlow,hup ',l,j_o(ispl)%d(lirh+L-1),j_o(ispl)%d(lirh+L),'dup= ',j_o(ispl)%d(lir+L)
			write(6,*)'dup.le.cotang*(hup-origo): cotang, * :',cotang,cotang*(j_o(ispl)%d(lirh+L)-origo)
			write(6,*)'reserved, used knots:',npo,npo2
			write(6,*)'heights:',j_o(ispl)%d(lirh:lirh+npo2-1)  !lirh link to first
			write(6,*)'diams:',j_o(ispl)%d(1:npo2)
 
			goto 99
 
		end if !IF (R.LT.-0.01 .OR.r.gt.sero)  24947
	end if !if(L.eq.1.and.j_o(ispl)%d(1).le.cotang*(j_o(ispl)%d(lirh)-  24930
 
	!C height height(L)+root
	r=r+j_o(ispl)%d(lirh+L-1)
 
	j_v(iout)=spld(r,npo, j_o(ispl)%r( lirh:),  j_o(ispl)%r( lirsp:))
	if(j_err)return
			90 continue  ! io=io+narg+3
	return
 
			99 	   if(iverr.gt.0)then
		if(j_otype(iverr).ne.j_iptrans)then
			call j_printname('err-> transforamtion ',iverr,' not a transforamtion set')
		else !if(j_otype(iverr).ne.j_iptrans)then
			call dotrans(iverr,1)
		endif !if(j_otype(iverr).ne.j_iptrans)  24971
 
	endif !99 	   if(iverr.gt.0)  24970
	j_err=.true.
	return
 
end !subroutine stempolar(iob,io)
 
 
 
subroutine integrate(iob,io)  !
	! Section integrate integrate() Integrates volume from STEMSPLINE
	! To be reported later.
	! endsection
 
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(narg.ge.3)then
		xl=j_v(j_o(iob)%i(io+3))
		xu=j_v(j_o(iob)%i(io+4))
		select case(j_otype(irg))
		case(j_ipstemspline) !select case(j_otype(irg))
 
		if(xl.ge.xu)then
			write(6,*)'*integrate, lower limit ',xl,' >,upper limit ',xu
			j_err=.true.
			return
 
		endif !if(xl.ge.xu)  25002
		j_v(iout)=splv(100.*xl,100.*xu,j_o(irg)%i(2),j_o(irg)%r(1+j_o(irg)%i(1):),j_o(irg)%r(1+2*j_o(irg)%i(1):) )
 
		if(j_err)return
		case default !select case(j_otype(irg))
		call j_printname('**not yet legal argument for integrate:',irg,' ')
		j_err=.true.
		return
 
		end select !select case(j_otype(irg))
	else !if(narg.ge.3)then
		!	xl=j_v(irg)
		!	xu=j_v(j_o(iob)%i(io+3))
		write(6,*)'**integrate type not yet implemented'
		j_err=.true.
	end if !if(narg.ge.3)  24996
 
	return
end !subroutine integrate(iob,io)
 
! subroutine valuex(iob,io)   !
 
! ! !	io=io_
! ! narg=j_o(iob)%i(io+1)
! ! !	io_=io_+narg+3
! ! irg=j_o(iob)%i(io+2)
! ! iout=j_o(iob)%i(io+2+narg)
! ! if(j_otype(iout).ne.j_ipreal)call j_del(iout)
! ! select case(j_otype(irg))
 
 
! ! case default !select case(j_otype(irg))
! ! call j_printname('**not legal argument for valuex:',irg,' ')
! ! j_err=.true.
 
! ! end select !select case(j_otype(irg))
! ! !	if(j_err)return
 
! return
! end !subroutine valuex(iob,io)
 
 
 
subroutine stemspline(iob,io)     !
	! Section stemspline stemspline() Creates STEMSPLINE
	!To be reported later, soon if needed.
	! endsection
 
	real,dimension(:), allocatable::dapu,hapu
	integer,dimension(:), allocatable::iperm
	logical sort
	logical vec
	character*1 tx
	logical p,test
	call j_checkoutput(iob,io)
	if(j_err)return
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	p=j_linkoption(iob,io,j_mdebug).ge.0
	test=j_linkoption(iob,io,j_mtest).ge.0
	iprint=j_iprintout(iob,1)  ! ,idef
	sort=j_linkoption(iob,io,j_msort).gt.0
	!distance must be at least 8 cm, optio
	delh=8.
	ndx=j_nargopt(iob,io,j_mdx)
	if(ndx.gt.0)delh=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mdx)+1))
	!!call j_clearoption(iob,io)  ! subroutine
 
	if(narg.eq.2)then
		ivh_=j_o(iob)%i(io+2)
		ivd_=j_o(iob)%i(io+3)
		if(j_otype(ivh_).ne.j_ipmatrix.or.j_otype(ivd_).ne.j_ipmatrix)then
			write(6,*)'*stemspline: if two arguments, they should be column vectors'
			goto 90
		endif !if(j_otype(ivh_).ne.j_ipmatrix.or.j_otype(ivd_).ne.j_ipmat  25083
		npo=j_o(ivh_)%i(1)
		if(j_o(ivd_)%i(1).ne.npo)then
			write(6,*)'*stemspline:arguments have different dimensions'
			goto 90
		endif !if(j_o(ivd_)%i(1).ne.npo)  25088
		vec=.true.
	else !if(narg.eq.2)then
		if(test)then
			npo=0
			npobas=narg/2
			do i=1,npobas
				if(j_v(j_o(iob)%i(io+1+i)).le.0.and.j_v(j_o(iob)%i(io+1+i+npobas)).le.0.)then
					npo=i-1
					goto 756
				endif !if(j_v(j_o(iob)%i(io+1+i)).le.0.and.j_v(j_o(iob)%i(io+1+i+  25098
			enddo !i=1,npobas  25097
			npo=narg/2
			756		continue
			!!write(6,*)'npo',npo,j_v(j_o(iob)%i(io+1+i:io+1+npo))
		else !if(test)then
			npo=narg/2
			npobas=npo
		endif !if(test)  25094
		vec=.false.
 
	endif !if(narg.eq.2)  25080
 
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipstemspline.or.j_o(iout)%i(1).ne.npo)call j_del(iout)
		! defstemspline
	endif !if(j_otype(iout).ne.j_ipreal)  25114
 
	if(j_otype(iout).eq.j_ipreal)then
		allocate( j_o(iout)%i(1:4));j_o(iout)%i(1)=npo;j_o(iout)%i(3:4)=0
		allocate( j_o(iout)%r(1:6*npo))
		j_otype(iout)=j_ipstemspline
	end if !if(j_otype(iout).eq.j_ipreal)  25119
	! in jakta spl(dt,ht) in j(ht,dt), but use interanlly the same order as in jakta
 
	if(sort)then
		ibd=2*npo
		ibh=ibd+npo   !use r to store
		if(allocated(iperm))then
			if(size(iperm).lt.npo)then
				deallocate(iperm)
				allocate(iperm(1:npo+1)) !some reserve
			endif !if(size(iperm).lt.npo)  25130
		else !if(allocated(iperm))then
			allocate(iperm(1:npo+1))
		endif !if(allocated(iperm))  25129
		ii=1
		do i=1,npo
			if(vec)then
				j_o(iout)%d(ibh+ii)=100.*j_o(ivh_)%d(i)
				j_o(iout)%d(ibd+ii)=j_o(ivd_)%d(i)
			else !if(vec)then
				j_o(iout)%d(ibh+ii)=100.*j_v(j_o(iob)%i(io+1+i) ) !h
				j_o(iout)%d(ibd+ii)=j_v(j_o(iob)%i(io+1+npo+i))
 
			endif !if(vec)  25139
			if(j_o(iout)%d(ibh+ii).lt.0..or.j_o(iout)%d(ibd+ii).le.0.)cycle
			iperm(ii)=ii
			ii=ii+1
		enddo !i=1,npo  25138
		npo2=ii-1
 
		j_o(iout)%d(npo+1:npo+npo2)=j_o(iout)%d(ibh+1:ibh+npo2)  !new, copy first to the output plcae
		call j_quick_sort(j_o(iout)%d(npo+1:npo+npo2),iperm)
 
		do i=1,npo2
 
			j_o(iout)%d(i)=j_o(iout)%d(ibd+iperm(i))
		enddo !i=1,npo2  25156
 
		ii=2
		do i=2,npo2
			if(j_o(iout)%d(npo+i)-j_o(iout)%d(npo+ii-1).lt.delh)then
				if(i.lt.npo2)then
					j_o(iout)%d(npo+ii-1)=  &
						0.5*(j_o(iout)%d(npo+ii-1)+j_o(iout)%d(npo+i))
					j_o(iout)%d(ii-1)= &
						0.5*(j_o(iout)%d(ii-1)+j_o(iout)%d(i))
				else !if(i.lt.npo2)then
					j_o(iout)%d(npo+ii-1)=j_o(iout)%d(npo+i)
					j_o(iout)%d(ii-1)=j_o(iout)%d(i)
 
				endif !if(i.lt.npo2)  25164
				ii=ii-1
			elseif(ii.ne.i)then !if(j_o(iout)%d(npo+i)-j_o(iout)%d(npo+ii-1).lt.delh)then
				j_o(iout)%d(npo+ii)=j_o(iout)%d(npo+i)
				j_o(iout)%d(ii)=j_o(iout)%d(i)
			endif !if(j_o(iout)%d(npo+i)-j_o(iout)%d(npo+ii-1).lt.delh)  25163
			ii=ii+1
 
		enddo !i=2,npo2  25162
 
		npo2=ii-1
 
	else !if(sort)then
		ii=1
		do 31 i=1,npo
			! dt, ht , splmat
 
			if(vec)then
				j_o(iout)%r(ii)=j_o(ivd_)%d(i)   !dt
				j_o(iout)%r(ii+npo)=100.*j_o(ivh_)%d(i)  !ht
			else !if(vec)then
				j_o(iout)%r(ii)=j_v(j_o(iob)%i(io+1+npobas+i))  !dt
				j_o(iout)%r(ii+npo)=100.*j_v(j_o(iob)%i(io+1+i) ) !ht
			endif !if(vec)  25190
			if( j_o(iout)%r(ii+npo).lt.0..or.j_o(iout)%r(ii).lt.0.)then
				goto 311
			end if !if( j_o(iout)%r(ii+npo).lt.0..or.j_o(iout)%r(ii).lt.0.)  25197
			if(ii.gt.1)then
				if(j_o(iout)%r(npo+ii).lt.j_o(iout)%r(npo+ii-1))then
 
					write(6,*)'*stemspline: heights not in order'
					if(vec)then
						write(6,*)j_o(ivh_)%r
					else !if(vec)then
						write(6,*)j_v(j_o(iob)%i(io+1+1:io+1+npo))
					endif !if(vec)  25204
					goto 90
				endif !if(j_o(iout)%r(npo+ii).lt.j_o(iout)%r(npo+ii-1))  25201
				if(j_o(iout)%r(npo+ii)-j_o(iout)%r(npo+ii-1).lt.delh)then
					if(i.lt.npo)then
						j_o(iout)%r(npo+ii-1)=  &
							0.5*(j_o(iout)%r(npo+ii-1)+j_o(iout)%r(npo+ii))
						j_o(iout)%r(ii-1)= &
							0.5*(j_o(iout)%r(ii-1)+j_o(iout)%r(ii))
					else !if(i.lt.npo)then
						j_o(iout)%r(npo+ii-1)=j_o(iout)%r(npo+ii)
						j_o(iout)%r(ii-1)=j_o(iout)%r(ii)
					endif !if(i.lt.npo)  25212
					ii=ii-1
				end if !if(j_o(iout)%r(npo+ii)-j_o(iout)%r(npo+ii-1).lt.delh)  25211
			end if !if(ii.gt.1)  25200
			ii=ii+1
 
			31     continue !31 i=1,npo  25187

			311	npo2=ii-1
	endif !if(sort)  25126
 
	j_o(iout)%i(2)=npo2
 
	if(iprint.gt.2.or.p)then
		write(6,*)'ht',j_o(iout)%r(npo+1:npo+npo2)
		write(6,*)'dt',j_o(iout)%r(1:npo2)
 
	endif !if(iprint.gt.2.or.p)  25233
 
	call spltd(npo2,j_o(iout)%r,j_o(iout)%r(npo+1:),j_o(iout)%r(2*npo+1:))
	kie=0
	!check that spline works
	dif0=1.8   !dy
	dtop=0.6*j_o(iout)%r(npo2)
 
	700    do i=1,npo2-1

		arg1=0.67*j_o(iout)%r(npo+i)+0.33*j_o(iout)%r(npo+i+1)
		test1=spld(arg1,npo2,j_o(iout)%r(npo+1:),  j_o(iout)%r(1+2*npo:))
		if(j_err)return
		arg2=0.33*j_o(iout)%r(npo+i)+0.67*j_o(iout)%r(npo+i+1)
		test2=spld(arg2,npo2,j_o(iout)%r(npo+1:),  j_o(iout)%r(1+2*npo:))
		if(j_err)return
		dmi=min(j_o(iout)%r(i),j_o(iout)%r(i+1)) !greater end point
		dma=max(j_o(iout)%r(i),j_o(iout)%r(i+1)) !smaller end point
 
		if(test1.gt.test2)then
			testma=test1  !testma is greater test point
 
			argma=arg1   !corresponding argument
			argmi=arg2
			testmi=test2
		else !if(test1.gt.test2)then
			testma=test2
			argma=arg2
			argmi=arg1
			testmi=test1
		endif !if(test1.gt.test2)  25256
 
		isok=1
		if(testma-dma.gt.dif0)then
			arg=argma
			isok=0
			f=0.7*dma+0.3*dmi
		else if(dmi-testmi.gt.dif0.or.testmi.le.dtop)then !if(testma-dma.gt.dif0)then
			arg=argmi
			f=0.7*dmi+0.3*dma
			isok=0
		endif !if(testma-dma.gt.dif0)  25270
		if(p)then
			write(6,*)i,j_o(iout)%r(i),j_o(iout)%r(i+1),test1,test2
		endif !if(p)  25279
 
		if(isok.le.0.and..false.)then
 
			if(p.or.iprint.ge.2) write(6,*)'illogical values in stemspline ',test1,test2, ' between values ',j_o(iout)%r(i),&
				j_o(iout)%r(i+1) !,' trying to fix' !, press <ret> to continue'
 
			if(npo2.ge.npo)then
				allocate(dapu(1:npo2+1),hapu(1:npo2+1))
				dapu(1:i)=j_o(iout)%r(1:i);dapu(i+2:npo2+1)=j_o(iout)%r(i+1:npo2)
				hapu(1:i)=j_o(iout)%r(npo+1:npo+i);hapu(i+2:npo2+1)=j_o(iout)%r(npo+i+1:npo+npo2)
				dapu(i+1:i+1)=f
				hapu(i+1:i+1)=arg
				deallocate(j_o(iout)%r)
 
 
				npo=npo2+1
				j_o(iout)%i(1)=npo;   !;o(iout)%i(3:4)=0
				allocate( j_o(iout)%r(1:6*npo))
				j_o(iout)%r(1:npo)=dapu
				j_o(iout)%r(npo+1:2*npo)=hapu
				deallocate(dapu,hapu)
			else !if(npo2.ge.npo)then
 
				do j=npo2,i+1,-1
					j_o(iout)%r(j+1)=j_o(iout)%r(j)
					j_o(iout)%r(npo+j+1)=j_o(iout)%r(npo+j)
				enddo !j=npo2,i+1,-1  25305
				j_o(iout)%r(i+1)=f
				j_o(iout)%r(npo+i+1)=arg
			endif !if(npo2.ge.npo)  25288
			npo2=npo2+1
			j_o(iout)%i(2)=npo2
			if(p.or.iprint.gt.2)then
				write(6,*)'new heights ',j_o(iout)%r(npo+1:npo+npo2)
				write(6,*)'new diams   ',j_o(iout)%r(1:npo2)
			endif !if(p.or.iprint.gt.2)  25314
			call spltd(npo2,j_o(iout)%r,j_o(iout)%r(npo+1:),j_o(iout)%r(2*npo+1:))
			kie=kie+1
 
			if(kie.le.7)goto 700
			write(6,*)'stemspline is mixed up'
			goto 90
		endif !if(isok.le.0.and..false.)  25283
 
	enddo !    do i=1,npo2-1  25245
 
	return
90 j_err=.true.

	call j_del(iout)
end subroutine!subroutine stemspline(iob,io)
 
subroutine stemcurve(iob,io)
 
	!
	!Section stemcurve stemcurve() defines a stem curve
	! The function is used with:
	!stem=stemcurve(ar1...argn)\\
	!where the arguments can be REAL variables, matrices, vectors or lists in any combination.
	! The arguments are first expanded to a numeric vector. The first half of the numeric values
	!contain the heights in dm and the second half diameters in cm. Odd number of values
	! cause error. After defining the stemcurve, it can be used as follows:\\
	! stem(h)= diameter in cm at height h in dm.\\
	!stem(-d) = height in dm of diameter d in cm\\
	! stem(h1,h2)= the volume in litres between heights h1 and h2\\
	!For examples of stemcurve, see manual for stemopt().
	!endheader
	!Note In harvester data having diameters at one dm steps, the function can be called as follows.
	!Let the diameters stored in vector dh having length 350. Make first vector of heights:\\
	! hoo=matrix(350,do->)\\
	!Then stem can be defined:\\
	!stem=stemcurve(hoo,dh)\\
	! The location of last diameter in dh can be found:\\
	!lastd=find(dh,filter->$.le.0)-1;
	!endnote
 
	!endsection
 
 
	double precision,dimension(:),allocatable ::argval
	logical vec
	logical new
	integer ::nargval
	integer*8  ::iba
 
 
	logical p,test
 
	call j_checkoutput(iob,io)
	call j_startfunction(iob,io,0,narg,j_arg,iout); if(j_err)return
 
	!	call j_getoption(iob,io,j_mharvester,-1,2,j_ipreal,.false.,nharvest,j_optarg0)
	call j_arg2val(j_arg,narg,argval,nargval)
 
	ncol=nargval/2
 
	if(2*ncol.ne.nargval)then
		write(6,*)'stemcurve has ',nargval,' argument values, needs even number'
		j_err=.true. ;return
	endif !if(2*ncol.ne.nargval)  25378
	test=j_isoption(iob,io,j_mtest)
	p=j_isoption(iob,io,j_mprint)  ! ,idef
	!	harvest=.false.
	i2=2
	ih=0
 
	new=.false.
	!	if(j_otype(iout).ne.j_ipreal)then
	if(j_otype(iout).eq.j_ipstemcurve)then
		if(j_o(iout)%i(1).ne.ncol)then  ! .or.j_o(iout)%i(3).ne.ih)then
			call j_del(iout)
			new=.true.
		endif !if(j_o(iout)%i(1).ne.ncol)  25391
	else
		new=.true.
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	endif !if(j_otype(iout).eq.j_ipstemcurve)  25390
 
	! defstemspline
	!	endif !if(j_otype(iout).ne.j_ipreal)  23142
 
	if(new)then
		allocate( j_o(iout)%i(1:2));j_o(iout)%i(1)=ncol
		allocate( j_o(iout)%d(1:i2*ncol))
		j_otype(iout)=j_ipstemcurve
	end if !if(new)  25403
 
	j_o(iout)%d(1:i2*ncol)=argval(1:nargval)
 
	npo0=0
 
 
	do i=1,ncol
		if(j_o(iout)%d(ncol+i).lt.j_0.or.j_o(iout)%d(i).lt.j_0.or.j_o(iout)%d(ncol+i).gt.10000.d0.or. &
			j_o(iout)%d(i).gt.10000.d0)exit
 
	enddo !i=1,ncol  25414
	j_o(iout)%i(2)=i-1   !npo
	if(i.le.1)then
		write(6,*)'no legal points'
		write(6,*)'ht ',j_o(iout)%d(1:ncol)
		write(6,*)'dt ',j_o(iout)%d(ncol+1:2*ncol)
		j_err=.true.
		return
 
	endif !if(i.le.1)  25420
 
 
 
end subroutine!subroutine stemcurve(iob,io) ncol% npo
 
subroutine stemopt(iob,io)     !new
 
	! Section stemopt stemopt() optimizes bucking or writes bucking schedules
	! The function can be used to compute optimal log lengths when tree stems are cut into sawlogs and
	! and the stem above sawlogs is used for pulp up to a diameter limit dmin.
	!The stem curve is feeded
	! in the form produced by a harvester, i.e., in one dm steps. It is possible that
	! the stem vector does not go up to diameter dmin as can be in harvester data. The function can predict
	! the volume up to dmin. The function is utilized results in paper: Juha Lappi (2024) 'From Finnish Assortment Pricing to Market
	!Economy Using Prices for Sawn Wood and
	!Chips in Reference Bucking' to be  published withid few days in Open Journal of forestry.
	! The github folder stem contains the data and script needed for the paper. The method is described
	! in the paper. Link to the paper is published soon here. This description for the function is not
	! yet checked.
	!The function can have two codeoptions which can tell that certain schedules are not accpeted\\
	!filter-> tells what schedules are accpted. This codeoption is computed when all logs os a schedule
	!has been generated. It can utilize all the output objects of the function.\\
	!reject-> tells what logs are rejected. Its arguments can be Dtop and Len, where Dtop is the
	!top diameter of the current log, used aslo in func-> and weight->, and len is the length of the
	!log, which must be already between lmin and Lmax\\
	! A call of function looks like:\\
	!u=stemopt(Lmin,Lmax,Dmin,dmin,dee,iobs,func->"Func",weight->"Weight")\\
	!where iobs can be missing if dee is vector instead of matrix of several rows\\
	!Lmin = the minimum saw log length in dm\\
	!Lmax = the maximum log length in dm\\
	!Dmin = the minimum top diamter of log\\
	!dee = if argument iobs is missing, then dee is vector (row- or column-) containg diameters.
	! if dee is matrix the iobs'th row contains the diameters.\\
	!func-> code option determining the share of top cross-sectional are going to sawn wood.\\
	! The function requires output. If output is u then the function generates the following subobjects.
	! The current schedule means in the following the schdedule whose value is computed and checked
	! is the schedule the best thus far. The vectors have maxlog elements, currently maxlog=7 \\
	!u%obj = value of the objective\\
	!u%nlogmin = minimum number of potential logs\\
	!u%nlogmax = maximum number of logs\\
	!u%lenpot = potential saw log part up to Dmin\\
	!u%logpot = volume of the potentila log part\\
	!u%com = commercial volume up to dmin\\
	! u%lentot = length of the commercial part\\
	! u%lencur = length of the current log part\\
	! u%dtopcur = top diameter of the current log part\\
	! u%nlog = number of logs in the optimal/current schedule\\
	! u%dtop = top diameter of the log part\\
	! u%logv = total volume of saw logs, the name following the same standard as othe components\\
	! would be u%log, but variable names without u% part are used elsewhere, and log is not a legal variable name.\\
	! u%cyl = total volume of log cylinders\\
	! u%inc = total volume of chips coming from log cylinders.\\
	! u%outc = total volume of chips coming between bark and log cylinder pulp bark\\
 
	! u%saw = total volume of sawn wood computed by multiplying volume of each cylinder multiplied\\
	! with the value of func-> option for the top diameter\\
 
	! u%sawu = value-weighted volume of sawn wood, obtained by multiplying
	! 	 u%saw with the value of weight->\\
 
	! u%chip = u%logv-u%saw=u%inc+u%outc\\
	! u%pulp = u%com-u%log  the volume of the pulp part\\
	!u%comlen = the length of the commercial part\\
	! u%lenn = the length of the log part\\
	! u%pulplen = length of the pulp part u%comlen-u%lenn\\
	!u%totc = u%chip+u%pulp total amount of chips coming both saw logs and pulp wood\\
	! u%lens = vector of loglengts. Elements u%nlog+1...maxlog are zero\\
	!u%lencs = cunulative lentghs of logs\\
	! u%dtops = vector of top diameters\\
	! u%logvs = volumes of logs in litres\\
	! ucyls = volumes of log cylinders\\
	! u%incs = volume of in-chips for each log\\
	! u%outc = volume of in-chips for each log\\
	!u%saws =volumes of sawn wood\\
	! u%chips = volumes chips\\
	! u%vars  = list of all subobjects produces\\
	! u%tree = the value iobs in the function call, with the second argument u%tree=1.
	! if schedules written to disk are used in jlp() function, this variable is used in the
	! unit-> option.\\
	! u%s = number of the optimal schedule. \\
	!u%ns =number of the schedules.
	!endheader
	!Ex stemoptex
	!dmat=matrix(2,180,in->)
	! 43.6 42.4 41.4 40.5 39.7 38.8 38.1 37.6 37.1 36.6 36.2 36.0 35.8 35.6 35.4 35.2 35.1 34.9 34.7 34.6,
	! 34.4 34.2 34.1 33.8 33.6 33.3 33.1 32.8 32.6 32.3 32.1 31.9 31.8 31.7 31.7 31.6 31.5 31.5 31.4 31.3,
	! 31.2 31.2 31.1 31.0 31.0 30.9 30.8 30.7 30.7 30.6 30.5 30.3 30.2 30.1 30.0 29.9 29.8 29.7 29.6 29.5,
	! 29.4 29.3 29.2 29.1 29.0 28.8 28.4 28.1 27.7 27.4 27.0 26.7 26.4 26.0 25.7 25.3 25.0 24.6 24.3 24.0,
	! 23.6 23.3 23.0 22.7 22.5 22.3 22.0 21.8 21.5 21.3 21.0 20.8 20.6 20.3 20.1 19.8 19.6 19.3 19.1 18.8,
	! 18.5 18.2 17.8 17.5 17.2 16.9 16.6 16.3 15.9 15.6 15.3 15.0 14.7 14.3 14.0 13.7 13.4 13.2 12.9 12.7,
	! 12.5 12.2 12.0 11.7 11.5 11.2 11.0 10.8 10.5 10.3 10.0  9.8  9.5  9.2  8.8  8.4  8.0  7.7  7.3  6.9,
	! 6.5  6.1  5.7  5.3  5.0  4.6  4.2  3.8  3.4  3.0  2.9  2.7  2.6  2.4  2.2  2.1  1.9  1.8  1.6  1.5,
	! 1.3  1.2  1.0  0.9  0.7  0.6  0.4  0.2  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0
	! 49.0 47.5 46.3 45.3 44.2 43.2 42.1 41.2 40.2 39.3 38.3 37.8 37.5 37.2 36.9 36.7 36.5 36.3 36.1 35.9,
	! 35.6 35.4 35.2 35.0 34.8 34.6 34.4 34.2 34.0 33.8 33.6 33.4 33.2 33.0 32.9 32.7 32.5 32.4 32.2 32.1,
	! 31.9 31.7 31.6 31.4 31.2 31.1 30.9 30.8 30.6 30.4 30.3 30.1 30.0 29.9 29.8 29.7 29.6 29.5 29.4 29.3,
	! 29.2 29.1 29.0 28.9 28.7 28.6 28.5 28.4 28.3 28.2 28.0 27.9 27.7 27.6 27.5 27.3 27.2 27.0 26.9 26.7,
	! 26.6 26.4 26.3 26.1 26.0 25.8 25.7 25.4 25.1 24.7 24.4 24.1 23.8 23.5 23.1 22.8 22.5 22.2 21.9 21.5,
	! 21.2 20.9 20.6 20.3 20.0 19.7 19.5 19.3 19.1 18.9 18.7 18.5 18.2 18.0 17.8 17.6 17.4 17.2 17.0 16.8,
	! 16.5 16.3 16.1 15.8 15.6 15.3 15.0 14.8 14.5 14.2 14.0 13.7 13.5 13.2 12.9 12.7 12.4 12.1 11.9 11.6,
	! 11.2 10.7 10.3  9.8  9.3  8.9  8.4  7.9  7.4  7.0  6.5  6.0  5.6  5.1  4.6  4.1  3.7  3.2  3.0  2.8,
	! 2.7  2.5  2.4  2.2  2.1  1.9  1.8  1.6  1.5  1.3  1.2  1.0  0.9  0.7  0.6  0.4  0.2  0.0  0.0  0.0
	! /
	! hoo=matrix(180,do->);
	! trstem=trans()
	! do(itree,1,2)
	! fi=0
	! stem=stemcurve(hoo,dmat(itree,All))
	! d13=stem(13);
	! h13=stem(-d13);
	! vol50=stem(0,50);
	! top=find(dmat(1,All),filter->$.le.0)-1;
	! fi=drawline(hoo(1,-top),dmat(itree,1,-top),width->2,color->Red,show->0)
	! u=stemopt(Lmin,Lmax,Dmin,dmin,dmat,itree,func->(1-bf*gam**Dtop),obj->u%saw)
	! u%vars;
	! print(u%vars,expand->)
	! do(ilog,1,u%nlog)
	! fi=drawline(u%lencs(ilog),u%lencs(ilog),0,stem(u%lencs(ilog)),color->Green,append->,show->0)
	! enddo  !ilog
	! fi=drawline(xlabel->'L, dm]Arial,11[',ylabel->'D, cm]Arial,11[',append->,continue->fcont)
	! enddo  !itree
	! /
	! Lmin=40
	! Lmax=55
	! Dmin=15
	! dmin=5
	! bf=0.63217650
	! gam=0.97487124
	! call(trstem)
 
	!endex
 
 
 
 
	! endsection
 
 
	!reported in utilities.f90 in section stemcurve
	integer,parameter ::n350=350
	integer,parameter ::maxlog=7,maxnobs=300,nkeep=33
	integer ::iob,io
	!17+3*maxlog
 
	!	logical harvest
	double precision ::stump,dmin,dminp,gam,b
	!double precision::pi4  !,pi1200
	double precision ::hs,stumpd,h0,h1,dapu,d0,d1
	double precision ::vbas,hbas
	double precision ::hcur,logvol,sawopt,sawopt0,optf,optcur
	double precision,dimension(:),pointer:: s,loglenp,logvp,sawp
	double precision ::veecom !
	double precision,dimension(:),pointer:: dee  !,aa,vee
	double precision,dimension(:),pointer::saw
	integer,dimension(:),pointer:: writev
 
	double precision,dimension(n350)::aaf,uaaf
	double precision,target,dimension(n350)::aa,vee
	double precision ::lam,lam2
	!double precision,dimension(4) ::lamv !saw util,p-util,sawprice,p-price
 
	!	parameter(pi4=0.007853981852531433d0)
	!	parameter(pi12= 0.261799395084d0)
	!parameter(pi1200=   0.0026179939508438d0)  !Pi/1200
	!	integer,dimension(itot):loglenopt
	integer,dimension(maxlog)::loglen,loglenf
	integer,dimension(0:maxlog)::loglenc=0
	double precision,dimension(maxlog)::logv0,saw0,sawcum
	integer nlogf
	integer::is,isopt,ivs,ivns
	integer last
	!double precision ::dobs
 
	integer htop,loglencur,nlog,iopt,ivnobs,nobs,ibasin,ibas,ncol
	integer linkobj
	integer ::ivdtopfunc=0
 
	logical iswrite,isbin,isbin8,isany,ispar,isweight,isreject,isfilter
	logical p
	save nwrite,writev,lenwrite,nu,isbin,isbin8,ivlenlog,ivdtopfunc
	p=j_isoption(iob,io,j_mprint)
 
	! j_yes=j_v(j_!2).eq.51
	!	ismax=.not.j_isoption(iob,io,j_mfast)
	!double precision,dimension(maxlog)::fa
	!	integer*8  ::iba
	isany=j_isoption(iob,io,j_many)
 
 
	!logical p,test
	call j_checkoutput(iob,io)  !calle,stump,lmin,lmax,dminl,dminp,gam,b
	call j_startfunction(iob,io,0,narg,j_arg,iout); if(j_err)return
 
	!	write(6,*)'narg ',narg
	!	if(narg.eq.5)then
	!	n5=4
	if(j_otype(j_arg(5)).ne.j_ipmatrix)then
		call j_getname(j_arg(5))
		write(6,*)j_oname(1:j_loname),' is not MATRIX'
		j_err=.true.;return
	endif !if(j_otype(j_arg(5)).ne.j_ipmatrix)  25624
 
	if(narg.eq.6)then
		nrow=j_o(j_arg(5))%i(1)
		ncol=j_o(j_arg(5))%i(2)
		iobs=j_v(j_arg(6))
		if(iobs.le.0.or.iobs.gt.nrow)then
			call j_getname(j_arg(5),j_arg(6))
			write(6,*)'illogical row argument ',iobs, 'for ',j_oname(1:j_loname+1),'with ',nrow,' rows'
			j_err=.true.;return
		endif !if(iobs.le.0.or.iobs.gt.nrow)  25634
		iba=(iobs-1)*ncol
	else
		iba=0
		ncol=j_o(j_arg(5))%i(3) !can be column vector
	endif !if(narg.eq.6)  25630
	dee=>j_o(j_arg(5))%d(iba+1:iba+ncol)
 
	!	j_dapu1=dee(1)
	!j_dapu2=dee(2)
	!interpolate
	!y=y0+(y1-y0)*(x-x0)/(x1-x0)  !x0=1,x1=2,y0=aa0(1),y1=aa0(2),x=0
	!y=aa0(1)+(aa0(2)-aa0(1))*(-1)/1  =aa0(1)-(aa0(2)-aa0(1))=aa0(1)+aa0(1)-aa0(2)
	!
	j_dapu=dee(1)+dee(1)-dee(2)
	vee(1)=j_pi1200*(j_dapu*j_dapu+dee(1)*dee(1)+j_dapu*dee(1))
 
	i0=1
	aa(1)=dee(1)*dee(1)
	do i=2,ncol
		if(dee(i).le.j_0)exit
		aa(i)=dee(i)*dee(i)
		vee(i)=vee(i0)+j_pi1200*(aa(i0)+aa(i)+dee(i)*dee(i0))  !cone
		i0=i
	enddo !i=2,ncol  25657
	ncol0=i-1
	aa(1:ncol0)=j_pi4*aa(1:ncol0)
 
	!write(6,*)'arg',narg,j_arg,j_otype(iout),j_ipstemopt
	if(j_err)return
	linkfunc=j_codelink(iob,io,j_mfunc); if(j_err)return
 
 
	if(j_err)return
	if(linkfunc.le.0)then
		write(6,*)'there must be func->'
		j_err=.true.;return
	endif !if(linkfunc.le.0)  25672
	linkweight=j_codelink(iob,io,j_mweight); if(j_err)return
	isweight=linkweight.gt.0
 
 
	linkobj=j_codelink(iob,io,j_mobj); if(j_err)return
 
	!	write(6,*)'ivsaw',ivsaw
	if(linkobj.le.0)then
		write(6,*)'there must be obj->'
		j_err=.true.;return
	endif !if(linkobj.le.0)  25683
 
	linkfilter=j_codelink(iob,io,j_mfilter); if(j_err)return
	isfilter=linkfilter.gt.0
	linkreject=j_codelink(iob,io,j_mreject); if(j_err)return
	isreject=linkreject.gt.0
 
 
	!	stop
	lmin=j_v(j_arg(1))
	lmax=j_v(j_arg(2))
	dmin=j_v(j_arg(3))
	dminp=j_v(j_arg(4))
	!dobs=j_v(j_arg(5))
	if(minval(j_v(j_arg(1:4))).le.j_0)then
 
		write(6,*)'illegal arguments lmin,lmax,dmin,dminp,iobs',j_v(j_arg(1:4))
		j_err=.true.;return
	endif !if(minval(j_v(j_arg(1:4))).le.j_0)  25700
 
	if(j_otype(iout).ne.j_ipstemopt)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		ivlenlog=j_getobject(0,'Len',j_ipreal)
		ivobj=j_getobject(iout,'%obj',j_ipreal)
		ivnlogmin=j_getobject(iout,'%nlogmin',j_ipreal)
		ivnlogmax=j_getobject(iout,'%nlogmax',j_ipreal)
 
		ivlenpot=j_getobject(iout,'%lenpot',j_ipreal)
		ivlogpot=j_getobject(iout,'%logpot',j_ipreal)
		ivcom=j_getobject(iout,'%com',j_ipreal)
 
		ivlentot=j_getobject(iout,'%lentot',j_ipreal)
		ivlencur=j_getobject(iout,'%lencur',j_ipreal)
		ivdtopcur=j_getobject(iout,'%dtopcur',j_ipreal)
		!	ivlogh=j_getobject(iout,'%logh',j_ipreal)
 
		!for each obsertot
		!	ivmethod=j_getobject(iout,'%method',j_ipreal)
		ivnlog=j_getobject(iout,'%nlog',j_ipreal)
		if(j_ivilog.eq.0)j_ivilog=j_getobject(0,'Ilog',j_ipreal) !to be used infilter reject
		ivdtop=j_getobject(iout,'%dtop',j_ipreal)
		ivlog=j_getobject(iout,'%logv',j_ipreal)
		ivcyl=j_getobject(iout,'%cyl',j_ipreal)
		ivinc=j_getobject(iout,'%inc',j_ipreal)
		ivoutc=j_getobject(iout,'%outc',j_ipreal)
		ivsaw=j_getobject(iout,'%saw',j_ipreal)
		ivchip=j_getobject(iout,'%chip',j_ipreal)
		ivpulp=j_getobject(iout,'%pulp',j_ipreal)
		!	ivcomlen=j_getobject(iout,'%comlen',j_ipreal)
		ivlen=j_getobject(iout,'%lenn',j_ipreal)
		ivpulplen=j_getobject(iout,'%pulplen',j_ipreal)
 
		ivlens=j_defmatrix(iout,'%lens',j_18,int8(maxlog),j_matreg)
		ivdtops=j_defmatrix(iout,'%dtops',j_18,int8(maxlog),j_matreg)
		ivlogs=j_defmatrix(iout,'%logvs',j_18,int8(maxlog),j_matreg)
		ivcyls=j_defmatrix(iout,'%cyls',j_18,int8(maxlog),j_matreg)
		ivincs=j_defmatrix(iout,'%incs',j_18,int8(maxlog),j_matreg)
		ivoutcs=j_defmatrix(iout,'%outcs',j_18,int8(maxlog),j_matreg)
 
		ivsaws=j_defmatrix(iout,'%saws',j_18,int8(maxlog),j_matreg)
		ivchips=j_defmatrix(iout,'%chips',j_18,int8(maxlog),j_matreg)
		ivkeep=j_deflist(iout,'%vars',list0=nkeep)  !17+3*maxlog)
		ivobs=j_getobject(iout,'%tree',j_ipreal)
		ivtotc=j_getobject(iout,'%totc',j_ipreal)
		ivs=j_getobject(iout,'%s',j_ipreal)
		ivlencs=j_defmatrix(iout,'%lencs',j_18,int8(maxlog),j_matreg)
		ivns=j_getobject(iout,'%ns',j_ipreal)
		ivsawu=j_getobject(iout,'%sawu',j_ipreal)
		ivsawus=j_defmatrix(iout,'%sawus',j_18,int8(maxlog),j_matreg)
		!	write(6,*)'nkeep',nkeep,j_o(ivkeep)%i(1)
		j_o(ivkeep)%i2(1:7)=(/ivobj,ivnlogmin,ivnlogmax,ivlenpot,ivlogpot,&
			ivcom,ivlentot/)
 
		j_o(ivkeep)%i2(8:17)=(/ivnlog,ivdtop,ivlog,ivcyl,&
			ivinc,ivoutc,ivsaw,ivpulp,ivlen,ivpulplen/)
 
		j_o(ivkeep)%i2(18:nkeep)=(/ivlens,ivlogs,ivcyls,&
			ivincs,ivoutcs,ivsaws,ivchips,ivchip,ivobs,ivtotc,ivs,ivlencs,ivdtops,ivns,ivsawu,ivsawus/)
 
		allocate(j_o(iout)%i(1:8))
		j_o(iout)%i(1:8)=(/ivlens,ivlogs,ivcyls,ivincs,ivoutcs,ivsaws,ivchips,ivkeep/)
		!	j_o(iout)%i(1:4)=(/ivkeep,ivlens,ivlogs,ivsaws/)
		! ivlens=j_o(ivkeep)%i2(18)
		! ivlogs=j_o(ivkeep)%i2(20)
		! ivcyls=j_o(ivkeep)%i2(21)
		! ivincs=j_o(ivkeep)%i2(22)
		! ivoutcs=j_o(ivkeep)%i2(23)
		! ivsaws=j_o(ivkeep)%i2(24)
		! ivchips=j_o(ivkeep)%i2(25)
 
		j_otype(iout)=j_ipstemopt
	else
 
		ivkeep=j_o(iout)%i(8)
		!	ivlens=j_o(iout)%i(2)
		!	ivlogs=j_o(iout)%i(3)
		!ivsaws=j_o(iout)%i(4)
		!	j_o(ivkeep)%i2(1:7)=(/ivobj,ivnlogmin,ivnlogmax,ivlenpot,ivlogpot,&
		!		ivcom,ivlentot/)
 
		ivobj=j_o(ivkeep)%i2(1)
		ivnlogmin=j_o(ivkeep)%i2(2)
		ivnlogmax=j_o(ivkeep)%i2(3)
		ivlenpot=j_o(ivkeep)%i2(4)
		ivlogpot=j_o(ivkeep)%i2(5)
		ivcom=j_o(ivkeep)%i2(6)
		ivlentot=j_o(ivkeep)%i2(7)
 
		! j_o(ivkeep)%i2(8:17)=(/ivnlog,ivdtop,ivlog,ivcyl,&
		! ivinc,ivoutc,ivsaw,ivpulp,ivlen,ivpulplen/)
		ivnlog=j_o(ivkeep)%i2(8)
		ivdtop=j_o(ivkeep)%i2(9)
		ivlog=j_o(ivkeep)%i2(10)
		ivcyl=j_o(ivkeep)%i2(11)
		ivinc=j_o(ivkeep)%i2(12)
		ivoutc=j_o(ivkeep)%i2(13)
		ivsaw=j_o(ivkeep)%i2(14)
		ivpulp=j_o(ivkeep)%i2(15)
		ivlen=j_o(ivkeep)%i2(16)
		ivpulplen=j_o(ivkeep)%i2(17)
		!j_o(ivkeep)%i2(18:nkeep)=(/ivlens,ivlogs,ivcyls,&
		!		ivincs,ivoutcs,ivsaws,ivchips,ivchip/)
 
		ivlens=j_o(ivkeep)%i2(18)
		ivlogs=j_o(ivkeep)%i2(19)
		ivcyls=j_o(ivkeep)%i2(20)
		ivincs=j_o(ivkeep)%i2(21)
		ivoutcs=j_o(ivkeep)%i2(22)
		ivsaws=j_o(ivkeep)%i2(23)
		ivchips=j_o(ivkeep)%i2(24)
		ivchip=j_o(ivkeep)%i2(25)
		ivobs=j_o(ivkeep)%i2(26)
		ivtotc=j_o(ivkeep)%i2(27)
		ivs=j_o(ivkeep)%i2(28)
		ivlencs=j_o(ivkeep)%i2(29)
		ivdtops=j_o(ivkeep)%i2(30)
		ivns=j_o(ivkeep)%i2(31)
		ivsawu=j_o(ivkeep)%i2(32)
		ivsawus=j_o(ivkeep)%i2(33)
	endif !if(j_otype(iout).ne.j_ipstemopt)  25706
 
	call j_getoption(iob,io,j_mpar,-1,1,j_ipmatrix,.true.,npar,j_optarg0)
	if(j_err)return
	ispar=npar.gt.0
	if(ispar)then
		ivpar=j_optarg0(1)
		ncolpar=j_o(ivpar)%i(2)
	endif !if(ispar)  25830
 
	call j_getoption(iob,io,j_mwrite,-1,9999,0,.true.,nwri,j_optarg0)
	if(j_err)return
	iswrite=nwri.gt.0
	if(iswrite)then
		if(j_iounit(j_optarg0(1)).eq.0)then
			if(nwri.eq.3.and.j_optarg0(3).eq.j_ivall)then
				nwrite=j_o(ivkeep)%i(1)
				writev=>j_o(ivkeep)%i2(1:nwrite)
			else
				nwrite=nwri-2
				writev=>j_optarg0(3:nwri)
			endif !if(nwri.eq.3.and.j_optarg0(3).eq.j_ivall)  25840
			call j_startwrite(j_optarg0(1),j_optarg0(2),nwrite,writev,isbin,isbin8,nu,lenwrite)
			!	write(6,*)'nwrite,lenwrite',nwrite,lenwrite,writev
			if(j_err)return
		endif !if(j_iounit(j_optarg0(1)).eq.0)  25839
	endif !if(iswrite)  25838
	!	write(6,*)'iswrite,nwritew,writev,nu',iswrite,nwritew,writev,nu
 
	j_v(ivobs)=iobs
	saw=>j_o(ivsaws)%d(1:maxlog)
	!	if(iobs.le.0.or.iobs.gt.j_o(ivmat)%i(1))then
	! if(imet.eq.0)write(6,*)'****imet0'
	! if(imet.eq.0)return  !variables are defiend
	!	endif !if(iobs.le.0.or.iobs.gt.j_o(ivmat)%i(1))  25577
	!	if(p)write(6,*)'aftinit'
	j_v( j_o(ivkeep)%i2(1:17))=j_0
 
	j_v(ivchip)=j_0
	j_v(ivtotc)=j_0
	!	write(6,*)'dmin',dminp
	do j=1,ncol
		if(dee(j).lt.dmin)exit
	enddo !j=1,ncol  25866
	loglentot=j-1
	if(loglentot.gt.n350)then
		write(6,*)'*j* commercial length is',lentot,' increse n350 at least to this'
		j_err=.true.;return
 
	endif !if(loglentot.gt.n350)  25870
	do i=j,ncol
		if(dee(i).lt.dminp)exit
 
	enddo !i=j,ncol  25875
	if(dee(i).le.j_0)then
		!
		lentot=i-1
		j_v(ivend)=lentot
		idmin=dee(i-1)+1
		! write(6,*)'dee',i,idmin,dee(i-7:i+1)
		! write(6,*)'vee',vee(i-7:i+1)
		! write(6,*)'idmin',idmin
		j_dapu=idmin
		if(idmin.gt.14)then
			write(6,*)'stem is measured only up to diameter ',idmin, ' shouls be up to 14cm'
			j_err=.true.;return
		endif !if(idmin.gt.14)  25888
		j_yes=.false.
		do j=i-1,1,-1
			!		write(6,*)'j,dee',j,dee(j)
			if(dee(j).gt.j_dapu.and..not.j_yes)then
 
				j1=j+1
				j0=j
				!predict h where dee.eq.j_dapu  h=h0+(d-d0)*(h1-h0)/(d1-d0)
				!		write(6,*)'rlin',j0,j1,dee(j0),dee(j1),j_dapu
				j_dapu2=j_rlinter(dee(j0),dee(j1),dble(j0),dble(j1),j_dapu)-j0  !h
				!		write(6,*)'rlin2',j0,j1,vee(j0),vee(j1),j_dapu2,'h',j_dapu2+j0
 
				j_dapu3=j_cone(j_dapu,dee(j0),j_dapu2)
 
				!			write(6,*)'cone,j0,j1,h,v,j_dapu,j_yes',j0,j1,j_dapu,dee(j0),j_dapu2,'vol',j_dapu3
				!			write(6,*)'veedif',vee(j1)-vee(j0)
				j00=j0
				j_yes=.true.
				j_dapu=j_dapu+j_1
				j_dapu4=j_dapu3  !store
			elseif(dee(j).gt.j_dapu)then
				j1=j+1
				j0=j
				!		write(6,*)'rlin',j0,j1,dee(j0),dee(j1),j_dapu
				j_dapu2=j_rlinter(dee(j0),dee(j1),dble(j0),dble(j1),j_dapu)  !h
				!		write(6,*)'j_dapu2 (h)',j_dapu2
				!		write(6,*)'cone',j_dapu,dee(j1),j1,j_dapu2,j1-j_dapu2
				j_dapu3=j_cone(j_dapu,dee(j1),dble(j1-j_dapu2))
				!		write(6,*)'j0,j1,h,v,j_dapu,j_yes',j0,j1,j_dapu,dee(j0),j_dapu2,'vol',j_dapu3
				veecom=j_dapu4+vee(j00)-vee(j1)+j_dapu3
 
				! write(6,*)'j00,',j00,j1,j_dapu4,vee(j00),vee(j1),vee(j00)-vee(j1),j_dapu3
				! write(6,*)'jr0,',j00,j1,j_dapu4,dee(j00),dee(j1)
				! write(6,*)'veecom',veecom
				icol=idmin-4
				iel=3*(dminp-4)*ncolpar+icol
				!		write(6,*)'iel ',iel,ncolpar
				veecom=j_o(ivpar)%d(iel)-j_o(ivpar)%d(iel+ncolpar)*j_o(ivpar)%d(iel+2*ncolpar)**veecom
				!		write(6,*)'veecom',icol,iel,veecom,j_o(ivpar)%d(iel),j_o(ivpar)%d(iel+ncolpar),j_o(ivpar)%d(iel+2*ncolpar)
 
				! pinetop6para=matrix(3,10,in->);
				! 0.000000   0.000000   2.907467   5.837656  11.538495  20.689988  28.307899  46.469346  56.151391 104.473485
				! 0.000000   0.000000   3.281787   6.788148  12.042798  20.317932  27.826743  44.171492  53.408341  97.921034
				! 0.000000   0.000000   0.664620   0.674034   0.801409   0.875630   0.890505   0.933906   0.936043   0.969030
				! /
				j_v(ivcom)=veecom+vee(lentot)
 
				exit
			endif !if(dee(j).gt.j_dapu.and..not.j_yes)  25895
 
 
 
		enddo !j=i-1,1,-1  25893
		!	read(5,*)k
 
	else
		lentot=i-1
		j_v(ivcom)=vee(lentot)
		j_v(ivlenpot)=lentot
	endif !if(dee(i).le.j_0)  25879
	!	read(5,*)k
 
 
	if(p)write(6,*)'loglentot',loglentot
 
 
 
	!	j_v(ivcomlen)=lepulp
 
	!	j_v(ivtotcom)=vee(lepulp)
 
	!	write(6,*)'ivtotlog',ivtotlog,ivlenpot,ivlogpot,ivnlogmax,loglentot
 
	if(loglentot.le.0)then
		j_v(ivlenpot)=j_0
		j_v(ivlogpot)=j_0
 
	else
		j_v(ivlogpot)=vee(loglentot)
		j_v(ivlenpot)=loglentot
 
	endif !if(loglentot.le.0)  25965
 
	!	j_v(ivpulplen)=lepulp-loglentot
	nlogmax=loglentot/lmin
	j_v(ivnlogmax)=nlogmax
 
 
 
	nlogmin=loglentot/lmax
	if(p)write(6,*)'loglentot,nlogmin,lmax ', loglentot,nlogmin,lmax
 
	if(nlogmax.gt.0)nlogmin=max(1,nlogmin)
	if(loglentot-nlogmin*lmax.ge.lmin)nlogmin=nlogmin+1
	j_v(ivnlogmin)=nlogmin
 
	!	j_v(ivnlogmax)=nlogmax
 
	if(loglentot.gt.nlogmax*lmax)then
		loglenpot=nlogmax*lmax
 
	endif !if(loglentot.gt.nlogmax*lmax)  25990
 
	if(nlogmax.le.0)then
		j_v(ivnlog)=j_0
		j_v(ivpulp)=j_v(ivcom)
		do im=1,6
			j_o(j_o(iout)%i(im))%d=j_0
		enddo !im=1,6  25998
 
		if(p)write(6,*)'nlogmax0'
		return
 
 
	endif !if(nlogmax.le.0)  25995
 
 
	hbas=lmin
	!	vbas=j_stemcurveint(icalle,stump,hbas,iob,io,.false.)
	if(ivdtopfunc.eq.0)ivdtopfunc=j_object('Dtop')
	if(ivdtopfunc.le.0)then
		write(6,*)'func-> should be defined in terms of Dtop'
		j_err=.true.
	endif !if(ivdtopfunc.le.0)  26012
 
	do i=lmin,loglentot
		j_v(ivdtopfunc)=dee(i)
		aaf(i)=j_codevalue(iob,linkfunc)*aa(i)
		if(j_err)return
 
	enddo !i=lmin,loglentot  26017
	if(isweight)then
		do i=lmin,loglentot
			j_v(ivdtopfunc)=dee(i)
			uaaf(i)=j_codevalue(iob,linkweight)*aaf(i)
			if(j_err)return
		enddo !i=lmin,loglentot  26024
	else
		j_v(ivsawu)=j_0
		j_o(ivsawus)%d(1:maxlog)=j_0
	endif !if(isweight)  26023
	!write(6,*)'isweight,linkweight,loglentot',isweight,linkweight,loglentot
	!write(6,'(20f6.1)')uaaf(lmin:loglentot)
	!read(5,*)hoop
	if(p)write(6,*)'nlogmax ',nlogmax,' nlogmin ',nlogmin,' loglentot ',loglentot,' lentot ',lentot
 
 
	!case(7) !maximize utility ffor codeoption obj
	!	ouble precision,dimension(4) ::lam  !saw util,p-util,sawprice,p-price
 
	! if(p)then
	! call j_getname(iout,ivlens,j_o(ivlens)%i2(1))
	! write(6,*)'case7 ',j_oname(1:j_loname+1),j_oname2(1:j_loname2+1),j_oname3(1:j_loname3+1)
	! ! do il=1,nlog
	! ! loglencur0=loglencur0+loglen(il)
	! ! ivlogi=j_o(ivlogs)%i2(il)  !log volume variable
	! ! j_v(ivlogi)=vee(loglencur0)-j_dapu
	! endif !if(p)  25891
	optf=j_ninf
 
	nlogf=0
	is=0
	!	write(6,*)'is0'
	if(isany)then
		!		write(6,*)'loglentot ',loglentot,lmin,lmax
		nlog=0
		call compute()
		do ilog1=lmin,min(loglentot,lmax),3
			loglen(1)=ilog1   ! loglen is the length of the log in dm
			loglenc(1)=ilog1  ! loglenc is the cumulatice length
			nlog=1
 
			call compute()
			if(loglentot-loglenc(1).ge.lmin)then
				do ilog2=lmin,min(loglentot-loglenc(1),lmax),3
					loglen(2)=ilog2
					loglenc(2)=loglenc(1)+ilog2
					nlog=2
 
					call compute()
					if(loglentot-loglenc(2).ge.lmin)then
						do ilog3=lmin,min(loglentot-loglenc(2),lmax),3
							loglen(3)=ilog3
							loglenc(3)=loglenc(2)+ilog3
							nlog=3
 
							call compute()
							if(loglentot-loglenc(3).ge.lmin)then
								do ilog4=lmin,min(loglentot-loglenc(3),lmax),3
									loglen(4)=ilog4
									loglenc(4)=loglenc(3)+ilog4
									nlog=4
 
									call compute()
									!		if(iobs.eq.205)write(6,*)'4loglentot,loglenc(4),lmin',
									if(loglentot-loglenc(4).ge.lmin)then
										!	if(iobs.eq.205)write(6,*)'4l00'
										do ilog5=lmin,min(loglentot-loglenc(4),lmax),3
											loglen(5)=ilog5
											loglenc(5)=loglenc(4)+ilog5
											nlog=5
											call compute()
											if(loglentot-loglenc(5).ge.lmin)then
												do ilog6=lmin,min(loglentot-loglenc(5),lmax),3
													loglen(6)=ilog6
													loglenc(6)=loglenc(5)+ilog6
													nlog=6
													!loglencur
													call compute()
												end do !ilog6=lmin,min(loglentot-loglenc(5),lmax),3  26095
												! else
												! call compute()
											endif !if(loglentot-loglenc(5).ge.lmin)  26094
										enddo !ilog5=lmin,min(loglentot-loglenc(4),lmax),3  26089
										! else
										! call compute()
									endif !if(loglentot-loglenc(4).ge.lmin)  26087
								enddo !ilog4=lmin,min(loglentot-loglenc(3),lmax),3  26080
								! else
								! call compute()
							endif !if(loglentot-loglenc(3).ge.lmin)  26079
						enddo !ilog3=lmin,min(loglentot-loglenc(2),lmax),3  26073
						! else
						! call compute()
					endif !if(loglentot-loglenc(2).ge.lmin)  26072
				enddo !ilog2=lmin,min(loglentot-loglenc(1),lmax),3  26066
				! else
				! call compute()
			endif !if(loglentot-loglenc(1).ge.lmin)  26065
		enddo !ilog1=lmin,min(loglentot,lmax),3  26059
		!	write(6,*)'nlogf,optf,loglenf,saw ',nlogf,optf,loglenf,saw
 
		j_v(ivobj)=optf
		j_v(ivs)=isopt
		j_v(ivns)=is
 
		call addnew(nlogf,loglenf)
		!	write(6,*)'optf,isopt,is',optf,isopt,is,'nlogf',nlogf
		!	write(6,*)'loglenf',loglenf(1:nlogf)
		return
		!loglenpot
	endif !if(isany)  26055
 
 
 
	!if(ismax)then
	!		write(6,*)'loglentot ',loglentot,lmin,lmax
 
	do ilog1=lmin,min(loglentot,lmax),3 ! loglentot is the length up Dmin
		loglen(1)=ilog1  ! loglen is the length of the log
		loglenc(1)=ilog1 ! loglenc is the cumulative length of logs
		nlog=1           ! nlog is the number of logs
		if(isreject)then
			if(reject(ilog1,0))cycle
		endif !if(isreject)  26144
		if(loglentot-loglenc(1).ge.lmin)then ! lmin is the minimum log length Lmin
			do ilog2=lmin,min(loglentot-loglenc(1),lmax),3  !lmax = Lmax
				loglen(2)=ilog2
				loglenc(2)=loglenc(1)+ilog2
				nlog=2
				if(isreject)then
					if(reject(ilog2,1))cycle
				endif !if(isreject)  26152
 
				if(loglentot-loglenc(2).ge.lmin)then
					do ilog3=lmin,min(loglentot-loglenc(2),lmax),3
						loglen(3)=ilog3
						loglenc(3)=loglenc(2)+ilog3
						nlog=3
 
						if(isreject)then
							if(reject(ilog3,2))cycle
						endif !if(isreject)  26162
						if(loglentot-loglenc(3).ge.lmin)then
							do ilog4=lmin,min(loglentot-loglenc(3),lmax),3
								loglen(4)=ilog4
								loglenc(4)=loglenc(3)+ilog4
								nlog=4
								if(isreject)then
									if(reject(ilog4,3))cycle
								endif !if(isreject)  26170
								if(loglentot-loglenc(4).ge.lmin)then
									do ilog5=lmin,min(loglentot-loglenc(4),lmax),3
										loglen(5)=ilog5
										loglenc(5)=loglenc(4)+ilog5
										nlog=5
										if(isreject)then
											if(reject(ilog5,4))cycle
										endif !if(isreject)  26178
										if(loglentot-loglenc(5).ge.lmin)then
											do ilog6=lmin,min(loglentot-loglenc(5),lmax),3
												loglen(6)=ilog6
												loglenc(6)=loglenc(5)+ilog6
												nlog=6
												if(isreject)then
													if(reject(ilog6,5))cycle
												endif !if(isreject)  26186
											end do !ilog6=lmin,min(loglentot-loglenc(5),lmax),3  26182
										else
											call compute()
										endif !if(loglentot-loglenc(5).ge.lmin)  26181
									enddo !ilog5=lmin,min(loglentot-loglenc(4),lmax),3  26174
								else
									call compute()
								endif !if(loglentot-loglenc(4).ge.lmin)  26173
							enddo !ilog4=lmin,min(loglentot-loglenc(3),lmax),3  26166
						else
							call compute() ! three logs nlog=3
						endif !if(loglentot-loglenc(3).ge.lmin)  26165
					enddo !ilog3=lmin,min(loglentot-loglenc(2),lmax),3  26157
				else
					call compute()  ! two logs are possible nlog=2
				endif !if(loglentot-loglenc(2).ge.lmin)  26156
			enddo !ilog2=lmin,min(loglentot-loglenc(1),lmax),3  26148
		else
			call compute() ! only one log is possible nlog=1
		endif !if(loglentot-loglenc(1).ge.lmin)  26147
	enddo !ilog1=lmin,min(loglentot,lmax),3  26140
	!	write(6,*)'nlogf,optf,loglenf,saw ',nlogf,optf,loglenf,saw
 
	j_v(ivobj)=optf
	j_v(ivs)=isopt
	j_v(ivns)=is
	call addnew(nlogf,loglenf)
	return
 
	!if(ismax)then
 
 
	contains
	logical function reject(le,iba)
		j_v(ivdtop)=dee(loglenc(iba)+le)
		j_v(ivlenlog)=le
		reject=j_codevalue(iob,linkreject).ne.j_0
	end function
	subroutine compute()
 
		call addnew(nlog,loglen)
		!	if(isany)write(17,*)'nlog ,loglen ',nlog,loglen
		if(p)write(17,*)j_o(ivlens)%d,j_o(ivdtops)%d
 
		j_dapu=j_codevalue(iob,linkobj)
		!	if(isany)write(17,*)'dapun ',j_dapu
		!	write(6,*)'dapu',j_dapu,optf,j_v(ivsaw),j_o(ivsaws)%d(1:nlog)
		!	if(iobs.eq.205.or.iobs.eq.67)write(6,*)nlog,j_v(ivsaw),j_v(ivpulp),j_v(ivchip),loglen(1:nlog),loglenc(1:nlog)
		! write(6,*)'j_dapu,iob,linkobj,',j_dapu,iob,linkobj,'shaw',j_v(ivsaw)
		! write(6,*)j_dapu,nlog,j_v(ivsaw),j_v(ivpulp),j_v(ivchip)
		! write(6,*)loglen(1:nlog),loglenc(1:nlog)
		if(isfilter)then
			j_dapu2=j_codevalue(iob,linkfilter)
			if(j_dapu2.eq.j_0)return
		endif !if(isfilter)  26240
		is=is+1
		!	write(6,*)'isnow',is
		!		if(p)write(6,*)'is ',is,'j_dapu,op',j_dapu,optf
		if(j_dapu.gt.optf)then
			optf=j_dapu
			isopt=is
			!		write(6,*)'isopt',isopt
			nlogf=nlog
			loglenf(nlog+1:maxlog)=0
			if(nlog.gt.0)then
				loglenf(1:nlog)=loglen(1:nlog)
 
				loglencur=loglenc(nlog)
			else
				loglencur=0
			endif !if(nlog.gt.0)  26253
			!		if(p)write(6,*)'newop',nlog,loglenf(1:nlog),'optyf',optf
 
		endif !if(j_dapu.gt.optf)  26247
		!	if(iswrite)write(6,*)'nw',nwrite,writev
		if(iswrite)then
			j_v(ivobj)=j_dapu
			call j_write(nwrite,writev,nu,isbin,isbin8,lenwrite)
 
		endif !if(iswrite)  26264
	end subroutine
 
	! double precision function sawpot() !uses loglnen0 nlog
	! integer loglencur0
 
	! loglencur0=0
	! sawpot=j_0
	! do i=1,nlog
	! loglencur0=loglencur0+loglenc(i)
	! sawpot=sawpot+aaf(loglencur0)*loglenc(i)
	! enddo !i=1,nlog  25510
 
	! end function
 
 
 
 
 
 
 
 
	!		nobs=nobs+1
	!		call next()
	subroutine addnew(nlogf,loglenf)
		integer ::nlogf
		integer,dimension(maxlog)::loglenf
		! nlogf=nlog
		! loglenf(1:nlog)=loglen(1:nlog)
		! loglenf(nlog+1:maxlog)=0
		! loglencur=loglenc(nlog
		j_v(ivnlog)=nlogf
		if(nlogf.gt.0)then
 
 
			!			loglenc(1)=loglenf(1)
			j_o(ivlens)%d(1:nlogf)=loglenf(1:nlogf)
 
			!	call j_getname(ivlens)
			!	write(6,*)'nlogf',j_oname(1:j_loname),nlogf,loglenf(1:nlogf),j_o(ivlens)%d(1:nlogf)
			j_o(ivlencs)%d(1)=j_o(ivlens)%d(1)
			j_o(ivlogs)%d(1)=vee(loglenf(1))
			j_o(ivdtops)%d(1)=dee(loglenf(1))
			j_v(ivlog)=j_o(ivlogs)%d(1)
			j_v(ivcyl)=aa(loglenf(1))*loglenf(1)   !cylinder of current log
			j_o(ivcyls)%d(1)=j_v(ivcyl)
			j_o(ivsaws)%d(1)=aaf(loglenf(1))*loglenf(1)
 
 
 
			j_v(ivsaw)=j_o(ivsaws)%d(1)
 
			if(isweight)then
				j_o(ivsawus)%d(1)=uaaf(loglenf(1))*loglenf(1)
				j_v(ivsawu)=j_o(ivsawus)%d(1)
			endif !if(isweight)  26320
			j_v(ivinc)=j_v(ivcyl)-j_v(ivsaw)
			j_o(ivincs)%d(1)=j_v(ivinc)
 
			j_v(ivoutc)=j_o(ivlogs)%d(1)-j_v(ivcyl)
			j_o(ivoutcs)%d(1)=j_v(ivoutc)
 
 
			j_o(ivchips)%d(1)=	j_v(ivinc)+	j_v(ivoutc)
			j_v(ivchip)=j_o(ivchips)%d(1)
 
			lecu=loglenf(1)
			!	j_dapu=j_0
 
			do il=2,nlogf
				ilu=il-1
				lecu=lecu+loglenf(il)
				j_o(ivlencs)%d(il)=lecu
				j_o(ivdtops)%d(il)=dee(lecu)
				!		ivlogi=j_o(ivlogs)%i2(il)  !log volume variable
 
				j_o(ivlogs)%d(il)=vee(lecu)-j_v(ivlog)
				j_v(ivlog)=j_v(ivlog)+j_o(ivlogs)%d(il)
 
				!		j_o(ivlens)%d(il)=loglenf(il)
 
 
				j_o(ivcyls)%d(il)=aa(lecu)*loglenf(il)
				j_v(ivcyl)=j_v(ivcyl)+  j_o(ivcyls)%d(il)!cylinder of current log
 
 
				j_o(ivsaws)%d(il)=aaf(lecu)*loglenf(il)
				j_v(ivsaw)=j_v(ivsaw)+j_o(ivsaws)%d(il)
 
				if(isweight)then
					j_o(ivsawus)%d(il)=uaaf(lecu)*loglenf(il)
					j_v(ivsawu)=j_v(ivsawu)+j_o(ivsawus)%d(il)
 
				endif !if(isweight)  26357
				!	if(isany)write(17,*)'saw,sawu',j_v(ivsaw),j_v(ivsawu)
 
				j_o(ivincs)%d(il)=j_o(ivcyls)%d(il)-j_o(ivsaws)%d(il)
				j_v(ivinc)=j_v(ivinc)+j_o(ivincs)%d(il)
				j_o(ivoutcs)%d(il)=j_o(ivlogs)%d(il)-j_o(ivcyls)%d(il)
				j_v(ivoutc)=j_v(ivoutc)+j_o(ivoutcs)%d(il)
 
				j_o(ivchips)%d(il)=j_o(ivincs)%d(il)+j_o(ivoutcs)%d(il)
 
			enddo !il=2,nlogf  26337
 
			!		write(6,*)'saws ',j_v(ivsaw),j_o(ivsaws)%d(1:nlogf)
			j_v(ivchip)=j_v(ivinc)+j_v(ivoutc)
		else
 
			j_v(ivlog)=j_0
			j_v(ivcyl)=j_0!cylinder of current log
 
			j_v(ivsaw)=j_0
 
 
			j_v(ivsawu)=j_0
 
			j_v(ivinc)=j_0
 
 
			j_v(ivoutc)=j_0
			j_v(ivchip)=j_0
 
 
 
			j_v(ivchip)=j_0
			j_v(ivdtop)=j_0
			j_v(ivlog)=j_0
			j_v(ivpulp)=vee(lentot)
			j_v(ivpulplen)=lentot
			j_v(ivlen)=j_0
			j_v(ivtotc)=j_v(ivpulp)
 
		endif !if(nlogf.gt.0)  26300
		il=nlogf+1
		j_o(ivlogs)%d(il:maxlog)=j_0
		j_o(ivlens)%d(il:maxlog)=j_0
		j_o(ivlencs)%d(il:maxlog)=j_0
		j_o(ivcyls)%d(il:maxlog)=j_0
		j_o(ivdtops)%d(il:maxlog)=j_0
		j_o(ivincs)%d(il:maxlog)=j_0
		j_o(ivincs)%d(il:maxlog)=j_0
		j_o(ivoutcs)%d(il:maxlog)=j_0
		j_o(ivsaws)%d(il:maxlog)=j_0
		if(isweight)j_o(ivsawus)%d(il:maxlog)=j_0
		j_o(ivchips)%d(il:maxlog)=j_0
 
		if(nlogf.eq.0)return
 
 
		j_v(ivdtop)=dee(lecu)
		j_v(ivlog)=vee(lecu)
		j_v(ivpulp)=j_v(ivcom)-vee(lecu)
		j_v(ivpulplen)=lentot-lecu
		j_v(ivlen)=lecu
		j_v(ivtotc)=j_v(ivpulp)+j_v(ivchip)
 
 
 
	end subroutine
 
	! subroutine addall()
	! j_v(ivnlog)=nlog
	! j_v(ivdtop)=dee(loglencur)
	! j_v(ivlog)=vee(loglencur)
	! j_v(ivpulp)=vee(lentot)-vee(loglencur)
	! j_v(ivpulplen)=lentot-loglencur
	! j_v(ivlen)=loglencur
	! j_o(ivlens)%d(1)=loglen(1)
 
	! j_o(ivlogs)%d(1)=vee(loglen(1))
	! j_v(ivlog)=j_o(ivlogs)%d(1)
 
	! j_v(ivcyl)=aa(loglen(1))*loglen(1)   !cylinder of current log
	! j_o(ivcyls)%d(1)=j_v(ivcyl)
 
	! j_o(ivsaws)%d(1)=aaf(loglen(1))*loglen(1)
	! j_v(ivsaw)=j_o(ivsaws)%d(1)
 
	! j_v(ivinc)=j_v(ivcyl)-j_v(ivsaw)
	! j_o(ivincs)%d(1)=j_v(ivinc)
 
	! j_v(ivoutc)=j_o(ivlogs)%d(1)-j_v(ivcyl)
	! j_o(ivoutcs)%d(1)=j_v(ivoutc)
 
	! j_o(ivchips)%d(1)=	j_v(ivinc)+	j_v(ivoutc)
	! j_v(ivchip)=j_o(ivchips)%d(1)
 
	! !	j_o(ivchip)=j_o(ivchips)%d(1)
 
 
	! loglencur0=loglen(1)
 
	! do il=2,nlog
	! loglencur0=loglencur0+loglen(il)
	! !		ivlogi=j_o(ivlogs)%i2(il)  !log volume variable
	! j_o(ivlogs)%d(il)=vee(loglencur0)-j_v(ivlog)
	! j_o(ivlens)%d(il)=loglen(il)
	! j_v(ivlog)=j_v(ivlog)+j_o(ivlogs)%d(il)
	! j_o(ivcyls)%d(il)=aa(loglencur0)*loglen(il)
	! j_v(ivcyl)=j_v(ivcyl)+  j_o(ivcyls)%d(il)!cylinder of current log
 
 
	! j_o(ivsaws)%d(il)=aaf(loglencur0)*loglen(il)
	! j_v(ivsaw)=j_v(ivsaw)+j_o(ivsaws)%d(il)
 
	! j_o(ivincs)%d(il)=j_o(ivcyls)%d(il)-j_o(ivsaws)%d(il)
	! j_v(ivinc)=j_v(ivinc)+j_o(ivincs)%d(il)
	! j_o(ivoutcs)%d(il)=j_o(ivlogs)%d(il)-j_o(ivcyls)%d(il)
	! j_v(ivoutc)=j_v(ivinc)+j_o(ivoutcs)%d(il)
 
	! j_o(ivchips)%d(il)=j_o(ivincs)%d(il)+j_o(ivoutcs)%d(il)
 
	! enddo !il=2,nlog  25676
	! j_v(ivchip)=j_v(ivinc)+j_v(ivoutc)
	! if(nlog.lt.maxlog)then
	! do im=1,6
	! j_o(j_o(iout)%i(im))%d(nlog+1:maxlog)=j_0
	! enddo !im=1,6  25699
	! endif !if(nlog.lt.maxlog)  25698
	! !	j_v(ivs)=isopt
	! ! if(loglencur0.gt.0)then
	! ! j_v(ivlog)=vee(loglencur0)
	! ! j_v(ivdtop)=dee(loglencur0)
 
	! ! j_v(ivpulp)=j_v(ivvcom)-vee(loglencur0)
	! ! else
	! ! j_v(ivlog)=j_0
	! ! j_v(ivdtop)=j_0
 
	! ! j_v(ivpulp)=j_v(ivvcom)
 
	! ! endif !if(loglencur0.gt.0)  26066
 
 
	! end subroutine
 
 
end subroutine stemopt
 
subroutine cumvol(iob,io)
	!	double precision pi1200
	!	parameter(pi1200=   0.0026179939508438d0)  !Pi/1200
	double precision aa0,aa1
	integer, dimension(:), pointer :: arg !arguments of the function
	call j_startfunction(iob,io,0,narg,arg,ivout);if(j_err)return
	imat=arg(1)
	if(j_otype(imat).ne.j_ipmatrix)then
		call j_getname(imat)
		write(6,*)'cumvol ',j_oname(1:j_loname),' is not MATRIX'
		j_err=.true.;return
 
	endif !if(j_otype(imat).ne.j_ipmatrix)  26515
	nel=j_o(imat)%i(3)
	nrow=j_o(imat)%i(1)
	ncol=j_o(imat)%i(2)
	ivout=j_defmatrix(ivout,' ',int8(nrow),int8(ncol),j_matreg)
 
	j_dapu=2*j_o(imat)%d(1)-j_o(imat)%d(2)
	j_o(ivout)%d(1)=j_pi1200*(j_dapu*j_dapu+j_o(imat)%d(1)**2+j_dapu*j_o(imat)%d(1))
 
	i0=1
	aa0=j_o(imat)%d(1)**2
	do i=2,nel
		if(j_o(imat)%d(i).le.j_0)exit
		aa1=j_o(imat)%d(i)**2
		j_o(ivout)%d(i)=j_o(ivout)%d(i0)+j_pi1200*(aa1+aa0+j_o(imat)%d(i)*j_o(imat)%d(i0))  !cone
		i0=i
		aa0=aa1
	enddo !i=2,nel  26531
	return
 
 
 
end subroutine
 
 
subroutine tautspline(iob,io)   !
	!Section tautspline tautspline() Creates a more regular TAUTSPLINE
	! tautspline(x1,…,xn,y1,…,yn[,par->][,sort->][,print->])//
	! Output://
	! An interpolating cubic spline, which is more robust than an ordinary cubic spline. To prevent
	! oscillation (which can happen with splines) the function adds automatically additional knots
	! where needed.//
	! Arguments://
	! x1,…,xn the x values//
	! d1,…,dn the y values.//
	! There must be at least 3 knot point, i.e. 6 arguments.//
	! Options://
	! par Parameter determining the smoothness of the curve. The default is zero,
	! which produces ordinary cubic spline. A typical value may 2.5. Larger values mean
	! that the spline is more closely linear between knot points.//
	! sort the default is that the x’s are increasing, if not then sort-> option must be given//
	! print if print-> option is given, the knot points are printed (after possible sorting).
	! The resulting spline can be utilized using value() function.
	! The taut spline algorithm is published by de Boor (1978) on pages 310-314. The source code
	! was loaded from Netlib.
	! endsection
 
 
 
	real*8,dimension(:,:), allocatable::s
	real*8,dimension(:), allocatable::tau
	real*8,dimension(:), allocatable::gtau
	integer,dimension(:), allocatable::iperm
	logical sort
	character*1 tx
	logical p
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	p=j_linkoption(iob,io,j_mprint).ge.0
	sort=j_linkoption(iob,io,j_msort).ge.0
	npar=j_nargopt(iob,io,j_mpar)
	lipar=j_linkoption(iob,io,j_mpar)
	if(npar.gt.0)then
		gammap=j_v(j_o(iob)%i(lipar+1))
	else !if(npar.gt.0)then
		gammap=0
		if(p)write(6,*)'parameter given value 0'
	endif !if(npar.gt.0)  26586
 
	!!call j_clearoption(iob,io)  ! subroutine
 
	npo=narg/2
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_iptautspline.or.j_o(iout)%i(1).ne.npo)call j_del(iout) !pitäisko ne -> lt.
		! defstemspline
	endif !if(j_otype(iout).ne.j_ipreal)  26596
	if(allocated(s))then
		if(size(tau).lt.npo)then
			deallocate(tau,gtau,s)
		endif !if(size(tau).lt.npo)  26601
		allocate(tau(1:npo),gtau(1:npo),s(1:npo,1:6))
	else !if(allocated(s))then
		allocate(tau(1:npo),gtau(1:npo),s(1:npo,1:6))
	endif !if(allocated(s))  26600
	if(j_otype(iout).eq.j_ipreal)then
		allocate( j_o(iout)%i(1:2))
		npo2=3*npo !reserved number of break points
		allocate( j_o(iout)%d(1:5*npo2))
	end if !if(j_otype(iout).eq.j_ipreal)  26608
 
	do i=1,npo
		tau(i)=j_v(j_o(iob)%i(io+1+i) ) !h
		gtau(i)= j_v(j_o(iob)%i(io+1+npo+i))  !d
	enddo !i=1,npo  26614
	if(sort)then
		if(allocated(iperm))then
			if(size(iperm).lt.npo)then
				deallocate(iperm)
				allocate(iperm(1:npo+1)) !some reserve
			endif !if(size(iperm).lt.npo)  26620
		else !if(allocated(iperm))then
			allocate(iperm(1:npo+1))
		endif !if(allocated(iperm))  26619
 
		call j_quick_sort(tau(1:npo),iperm)
 
		do i=1,npo
			gtau(i)=gtau(iperm(i))
		enddo !i=1,npo  26630
 
	else !if(sort)then
		do i=1,npo-1
			if(tau(i).gt.tau(i+1))then
				write(6,*)'*tautspline: x:s not in order', &
					(tau(jj),jj=1,npo)
				j_err=.true.
				return
			endif !if(tau(i).gt.tau(i+1))  26636
		enddo !i=1,npo-1  26635
	endif !if(sort)  26618
 
	if(p)then
		write(6,*)'x ',tau(1:npo)
		write(6,*)'y ',gtau(1:npo)
 
	endif !if(p)  26645
 
	call tautsp(tau,gtau,npo,gammap,s,j_o(iout)%d,j_o(iout)%d(npo2+1:),&
		nbreak,k,iflag)
	if(iflag.gt.1)then
		if(iflag.eq.2)write(6,*)'*tautspline: input not correct'
		if(iflag.eq.3)write(6,*)'*tautspline: ask J. Lappi to increase npo2'
		j_err=.true.
		call j_del(iout)
	endif !if(iflag.gt.1)  26653
 
	j_otype(iout)=j_iptautspline
	j_o(iout)%i(1)=npo
	j_o(iout)%i(2)=npo2
	j_o(iout)%i(3)=nbreak ! l
 
	return
end subroutine !subroutine tautspline(iob,io)
 
subroutine eigen(iob,io)
	!Section eigen eigen() Eigenvector and eigenmatrix from MATRIX
	!Computes eigenvectors and eigenvalues of a square matrix. The eigenvectors are stored as
	!columns in matrix output%matrix and the eigenvalues are stored as a row vector
	! output%values. The eigenvalues and eigenvectors are sorted from smallest to larges eigenvalue.
	!Netlib subroutines DLASCL, DORGTR, DSCAL, DSTEQR, DSTERF, DSYTRD,
	!XERBLA, DLANSY and DLASCL are used.
	!endheader
	!Option
	!Args&1&MAT& A square MATRIX.
	!endoption
	!endsection
	!
	!
	!
 
	!*     .. Parameters ..
	DOUBLE PRECISION   ZERO, ONE
	PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
	!*     ..
	!*     .. Local Scalars ..
	INTEGER            IINFO, IMAX, INDE, INDTAU, INDWRK, ISCALE, &
		LLWORK, LWKOPT, NB
	DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,&
		SMLNUM
	! *     ..
	! *     .. External Functions ..
	INTEGER            ILAENV
	DOUBLE PRECISION   DLAMCH, DLANSY
	EXTERNAL           LSAME, ILAENV, DLAMCH, DLANSY
 
	! *     ..
	! *     .. External Subroutines ..
	EXTERNAL           DLASCL, DORGTR, DSCAL, DSTEQR, DSTERF, DSYTRD, &
		XERBLA
	! *     ..
	! *     .. Intrinsic Functions ..
	INTRINSIC          MAX, SQRT
	integer,dimension(:), pointer::arg=>null()
	double precision ,dimension(:,:), allocatable::a
	double precision ,dimension(:), allocatable::w,work
	CHARACTER          JOBZ, UPLO
 
	logical pr
	pr=.false.
 
	call j_startfunction(iob,io,j_ipmatrix,narg,arg,ivout)
	if(j_err)return
	if(ivout.eq.j_ivresult)then
		write(6,*)'*eigen must have explicit output'
		j_err=.true.
		return
 
	endif !if(ivout.eq.j_ivresult)  26716
	n=j_o(arg(1))%i(1)
	if(n.ne.j_o(arg(1))%i(2))then
		call j_printname('*eigen: argument ',arg(1),' not a square matrix')
		j_err=.true.
		return
	endif !if(n.ne.j_o(arg(1))%i(2))  26723
	allocate(a(1:n,1:n),w(1:n))
	LDA=n
	do i=1,n
		do j=i,n
			a(i,j)=j_o(arg(1))%d((i-1)*n+j)
		end do !j=i,n  26731
	end do !i=1,n  26730
	uplo='U'
	jobz='V'
	NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
	LWORK = MAX( 1, ( NB+2 )*N ) !lwkopt
 
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
	END IF !IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN )  26752
	IF( ISCALE.EQ.1 ) &
		CALL DLASCL( UPLO, 0, 0, ONE, SIGMA, N, N, A, LDA, INFO )
	! *
	! *     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
	! *
	INDE = 1
	INDTAU = INDE + N
	INDWRK = INDTAU + N
	LLWORK = LWORK - INDWRK + 1
	CALL DSYTRD( UPLO, N, A, LDA, W, WORK( INDE ), WORK( INDTAU ),&
		WORK( INDWRK ), LLWORK, IINFO )
	! *
	! *     For eigenvalues only, call DSTERF.  For eigenvectors, first call
	! *     DORGTR to generate the orthogonal matrix, then call DSTEQR.
	! *
	CALL DORGTR( UPLO, N, A, LDA, WORK( INDTAU ), WORK( INDWRK ),&
		LLWORK, IINFO )
	CALL DSTEQR( JOBZ, N, W, WORK( INDE ), A, LDA, WORK( INDTAU ),&
		INFO )
	! *
	! *     If matrix was scaled, then rescale eigenvalues appropriately.
	! *
	IF( ISCALE.EQ.1 ) THEN
		IF( INFO.EQ.0 ) THEN
			IMAX = N
		ELSE !IF( INFO.EQ.0 ) THEN
			IMAX = INFO - 1
		END IF !IF( INFO.EQ.0 )  26782
		CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
	END IF !IF( ISCALE.EQ.1 )  26781
	ivmat=j_defmatrix(ivout,'%matrix',int8(n),int8(n),j_matreg)
	do i=1,n
		do j=1,n
			j_o(ivmat)%d((i-1)*n+j)=a(i,j)
		enddo !j=1,n  26791
	enddo !i=1,n  26790
	if(pr)call j_printname('eigenvectors stored in matrix ',ivmat,' ')
	ivmat=j_defmatrix(ivout,'%values',j_18,int8(n),j_matreg)
	j_o(ivmat)%d(1:n)=w(1:n)
	if(pr)call j_printname('eigenvalues stored in matrix (row vector) ',ivmat,' ')
	if(info.gt.0)write(6,*)'*eigen did not converge!!!!!'
	return
 
end subroutine eigen !subroutine eigen(iob,io)
 
! recursive subroutine result(iob,io)  !result(
 
! real, dimension (:),allocatable:: vout
! call j_checkoutput(iob,io)
! if(j_err)return
! !	io=io_
! narg=j_o(iob)%i(io+1)
! !	io_=io_+narg+3
 
 
! ifunc=j_o(iob)%i(io+2)
! if(j_otype(ifunc).eq.j_iplist)then
! ivindex=j_igetopt(iob,io,j_mindex)
! if(ivindex.le.0)then
! write(6,*)'** result, first argument is list => there must be index->'
! j_err=.true.
! return
! endif !if(ivindex.le.0)then
! inde=j_v(ivindex)
! if(inde.le.0.or.inde.gt.j_o(ifunc)%i(0))then
! write(6,*)'**result, index-> has illegal value ',inde, 'max is ',j_o(ifunc)%i(0)
! j_err=.true.
! return
! endif !if(inde.le.0.or.inde.gt.j_o(ifunc)%i(0))then
! ifunc=j_o(ifunc)%i(inde)
 
! endif !if(j_otype(ifunc).eq.j_iplist)then
 
! if(j_otype(ifunc).ne.j_iptrans)then
! call j_printname('**result:',ifunc,' not a transformation')
! call j_printname('  result() in transforamtion set ',iob,' ')
! j_err=.true.
! return
! endif !if(j_otype(ifunc).ne.j_iptrans)then
! iout=j_o(iob)%i(io+2+narg)
! if(iout.gt.j_mxnamedv+1)then
! nsav=iout-j_mxnamedv-1
! allocate(vout(1:nsav)); vout=j_v(j_mxnamedv+1:iout-1)
! else !if(iout.gt.j_mxnamedv+1)then
! nsav=0
! endif !if(iout.gt.j_mxnamedv+1)then
! iresu=j_igetopt(iob,io,j_mresult)
! if(iresu.le.0)then
! iresu=j_o(ifunc)%i2(10)
 
! endif !if(iresu.le.0)then
! !!call j_clearoption(iob,io)  ! subroutine
! call dotrans(ifunc,1) !io=1
! if(j_otype(iout).ne.j_ipreal)call j_del(iout)
! if(j_err)return
! j_v(iout)=j_v(iresu)
! if(nsav.gt.0)then
! j_v(j_mxnamedv+1:iout-1)=vout ;deallocate(vout)
! endif !if(nsav.gt.0)then
 
! return
! end !recursive subroutine result(iob,io)
 
recursive subroutine call(iob,io)   !
 
	narg=j_o(iob)%i(io+1)
	ifunc=j_o(iob)%i(io+2)
	if(j_otype(ifunc).ne.j_iptrans)then
		call j_printname('**call:',ifunc,' not a transformation')
		j_err=.true.
		return !goto 90 !return
	endif !if(j_otype(ifunc).ne.j_iptrans)  26866
	!	call j_getname(iob,ifunc)
 
	!	write(6,*)'++',j_oname(1:j_loname),' ',j_oname2(1:j_loname2)
	!	write(6,*)'<55call ',j_o(ifunc)%i(1:j_o(ifunc)%i(0))
	call dotrans(ifunc,1) !io=1
	!	j_recursion=j_recursion-1
	!	write(6,*)'err',j_err,j_recursion
	!	90   io=io+narg+3
 
	return
end subroutine!recursive subroutine call(iob,io)
 
subroutine pause(iob,io)
	! Section pause pause() in a TRANS or in command input
	! Function pause() stops the execution of Jlp22 commands which are either in an
	! include file or in TRANS object. The user can give any commands during the
	! pause() except input programming commands.
	! If the user presses <return> the excution continues. If the user gives 'e', then
	! an error condition is generated, and Jlp22 comes to the sit> promt, except
	! if ]Continue[ has values 1, in which case the control returns one level above
	! the sit> promt.
	! If pause()  has a CHAR argument, then that character constant is used as
	! the prompt.
	! endheader
	! Note When reading commands from an include file, the a pause can be generated also
	! with ;pause, which works similarly as pause(), but during ;pause also input programming
	! commands can be given.
	! endnote
	! endsection
	! give any commands during the pause. Typing <return>, the exceution continues. Typing 'e' or 'end',
	! an error is generated, and the control returns to sit> promt.
 
 
	if(j_o(iob)%i(io+1).gt.0)then
		call j_getchar(j_o(iob)%i(io+2),j_pauseprompt,le);if(j_err)return
		if(j_err)return
		call j_pause(j_pauseprompt(1:le),do=.true.)
	else !if(j_o(iob)%i(io+1).gt.0)then
		call j_pause(do=.true.)
	endif !if(j_o(iob)%i(io+1).gt.0)  26904
	if(j_stop)stop
end subroutine !subroutine pause(iob,io)
 
subroutine noptions(iob,io)
	! Section noptions Number of options in the current function: noptions()
	! noptions() returns as a REAL value the number of options currently
	! active. This may be used for testing purposes.
	! endheader
	! Note When Jlp22 processes options in different funtions, it varies at which point
	! options are cleared and number of options decreases
	! endnote
	! endsection
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	j_v(iout)=j_nopt
end subroutine !subroutine noptions(iob,io)
 
subroutine R(iob,io)
	!Section R  R() Executes an R-script
	! An R script can be executed with R(script) where script is CHAR object
	! defining the script text file. The function is calling //
	! call execute_command_line('Rscript.lnk '//j_filename(1:le), wait=.false.)//
	! Thus a shortcut for the Rscript program needs to be available.
	! endheader
	! Ex Rex Example of Rscript
	! rscript=text()
	! # A simple R-script that generates a small data to file mydat.txt
	! wd<-"C:/jlp22/jmanual"
	! x<-runif(10,0,10)
	! y<-cbind(1,x)%*%c(1,2)+rnorm(10)
	! mydat<-data.frame(y,x)
	! write.table(mydat,file=paste(wd,"/mydat.txt",sep=""))
	! //
	! write('miniscript.r',rscript)
	! close('miniscript.r')
	! R('miniscript.r')
	! print('mydat.txt')
	!delete_f('mydat.txt','miniscript.r')
	! endex
	!endsection
 
 
 
	integer, dimension(:), pointer :: arg
	logical exis
	call j_startfunction(iob,io,j_ipchar,narg,arg,ivout,minarg=1)
	if(j_err)return
	if(j_otype(arg(1)).ne.j_ipchar)goto 99
 
	call j_getchar(arg(1),j_filename,le);if(j_err)return
	inquire(file='j.par',exist=exis)
	if(.not.exis)goto 99
	!call execute_command_line('Rscript.lnk '//j_filename(1:le), wait=.false.)
	call execute_command_line('Rscript '//j_filename(1:le), wait=.false.)
	return
	99 	call j_getname(arg(1))
	write(6,*)'**argument ',j_oname(1:j_loname), ' is not file name'
	j_err=.true.
	return
 
 
end subroutine
subroutine text(iob,io)
	logical ::clean  !
	! Section text text() Creates TEXT
	! Text objects are created as a side product by many Jlp22 functions. Text objects can be created
	! directly by the text() function which works in a nonstandard way. The syntax is:\\
	! output=text()\\
	! …\\
	! //
 
	! The input paragraph ends exceptionally with '//' and not with '/'. The lines within the input
	! paragraph of text are put literally into the text object (i.e. even if there would be input
	! programming functions or structures included)
	! endsection
	clean=j_isoption(iob,io,j_mclean)
	!!call j_clearoption(iob,io)
	narg=j_o(iob)%i(io+1)
 
	!how many domains are in one set, or in one row set
 
	iout=j_o(iob)%i(io+2+narg)
	if(iout.eq.j_ivresult)then
		write(*,*)'**text must have output';j_err=.true.;goto 99
	end if !if(iout.eq.j_ivresult)  26993
 
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	ndefrow=200
 
	ivrow_=j_deftext(iout,' ',ndefrow,80*ndefrow)
	!		write(6,*)'8484*,ivrow_
 
	1000      format(a,$)
	100      format(a)
	if(j_ninc.eq.1)then
		write(6,*)'give text, end://'
	1  write(6,1000)'text>'

		read(5,100,err=90,end=91)j_inp
 
		j_linp=j_lentrim(j_inp)
		if(j_inp(1:2).ne.'//')then
			if(clean)then
				call j_putcleantext(ivrow_,j_inp(1:j_linp))
			else
				call j_puttext(ivrow_,j_inp(1:j_linp))
			endif !if(clean)  27013
			goto 1
		end if !if(j_inp(1:2).ne.'//')  27012
	else !if(j_ninc.eq.1)then
		iiv=j_inciv(j_ninc)
		irow=j_o(iiv)%i(6)
195		if(j_o(iiv)%txt(irow+1)(1:2).ne.'//')then
	!		write(6,*)'<5454>',j_o(iiv)%txt(irow+1)(1:j_o(iiv)%i2(irow+1))
			if(clean)then
				call j_putcleantext(ivrow_,j_o(iiv)%txt(irow+1)(1:j_o(iiv)%i2(irow+1)))
 
			else
				call j_puttext(ivrow_,j_o(iiv)%txt(irow+1)(1:j_o(iiv)%i2(irow+1)))
			endif !if(clean)  27025
			irow=irow+1
			goto 195
		end if !195		if(j_o(iiv)%txt(irow+1)(1:2).ne.'//')  27023
		j_o(iiv)%i(6)=irow+1
 
	endif !if(j_ninc.eq.1)  27005
 
	99  continue ! io=io+narg+3

	return
	90 write( 6,*)'**error reading text'
	j_err=.true.
	goto 99
	91 write(6,*)'* end of file when reading in text(), missing //'
	j_err=.true.
	goto 99
end subroutine !subroutine text(iob,io)
 
subroutine txt(iob,io)   !
	! Section txt txt() Creates TXT
	! Works as text(). The new TXT object is used
	! to implement ;incl and gnuplot -figures.
	! endsection
 
	narg=j_o(iob)%i(io+1)
 
	!how many domains are in one set, or in one row set
 
	iout=j_o(iob)%i(io+2+narg)
	if(iout.eq.j_ivresult)then
		write(*,*)'**txt must have output';j_err=.true.;return
	end if !if(iout.eq.j_ivresult)  27060
	! if(j_linkoption(iob,io,j_min).eq.0)then
	! write(6,*)'txt() requires in->'
	! j_err=.true.;return
	! endif !if(j_linkoption(iob,io,j_min).eq.0)  22762
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	j_inpara2=.true.
 
	ndim1=0
	iiv=j_inciv( j_ninc)
	iline=j_o(iiv)%i(6)
 
	call j_getinput('txt> ',single=.true.)
	!	write(6,*)'dd ',j_inp(1:j_linpr)
	if(j_err)return
	do while(j_inpr(1:2).ne.'//')
		ndim1=ndim1+1
		call j_getinput('txt> ',single=.true.)
		!	write(6,*)'cc ',j_inp(1:j_linpr),j_err
		if(j_err)return
 
	enddo !while(j_inpr(1:2).ne.'//')  27078
 
	write(6,*)'found ',ndim1,' rows'
	iout=j_deftxt(iout,' ',ndim1)
	! allocate(j_o(iout)%txt(1:ndim1))
	! j_o(iout)%txt=' '
	! allocate(j_o(iout)%i2(1:ndim1))
	! allocate(j_o(iout)%i(1:8))
	! j_o(iout)%i(1:8)=0
	! j_o(iout)%i(5)=ndim1
	! j_o(iout)%i(8)=ndim1
 
	j_o(iiv)%i(6)=iline
 
	do i=1,ndim1
		call j_getinput('txt> ',0,single=.true.)
		!	write(6,*)'ee ',j_inp(1:j_linpr)
		j_o(iout)%txt(i)(1:j_linpr)=j_inpr(1:j_linpr)
	enddo !i=1,ndim1  27098
	call j_getinput('txt> ',0)
	!	write(6,*)'ss ',j_inp(1:j_linpr)
	j_otype(iout)=j_iptxt
	j_inpara2=.false.
	return
 
 
end subroutine !subroutine txt(iob,io)
 
subroutine char_(iob,io)
 
	call j_startfunction(iob,io,0,narg,j_arg,ivout); if(j_err)return
	if(j_otype(j_arg(1)).ne.j_ipreal)then
		write(6,*)'first argument in char must be REAL'
		j_err=.true.;return
	endif !if(j_otype(j_arg(1)).ne.j_ipreal)  27115
 
	if(narg.eq.1)then
		write(j_objname,*)j_v(j_arg(1))
 
		! ivin=j_defchar(0,j_chr10(j_v(j_arg(1))))
		! if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
 
	else
		call j_getchar(j_arg(2),p_apubuf,le);if(j_err)return
 
 
		write(j_objname,p_apubuf(1:le),err=90)j_v(j_arg(1))
 
		!ivin=j_defchar(0,p_apubuf(1:le))
 
 
	endif !if(narg.eq.1)  27120
	le=j_lentrimnum(j_objname)
	ib=j_nonblank(j_objname,1,le)
 
	!write(6,*)'ivout,ib,le',ivout,ib,le
	call j_asschar(ivout,j_objname(ib:le))
	return
90 write(6,*)'error in char when writing ',j_v(j_arg(1)),' with format ',p_apubuf(1:le)

	j_err=.true.;return
 
 
end subroutine
subroutine crossed(iob,io)  !
 
	integer ,dimension(:), pointer::inclass,inclass2
	logical sym
	integer*8 ::i
	call j_checkoutput(iob,io)
	if(j_err)return
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	sym=j_igetopt(iob,io,j_msym).ge.0
 
	ivclass=j_igetopt(iob,io,j_mclass)
	if(ivclass.le.0)then
		write(6,*)'**crossed: class-> missing'
		j_err=.true.
		return
	endif !if(ivclass.le.0)  27164
	ivdummy=j_igetopt(iob,io,j_mdummy)
	if(ivdummy.le.0)then
		write(6,*)'**crossed: dummy-> missing'
		j_err=.true.
		return
	endif !if(ivdummy.le.0)  27170
	call j_getdataobject(iob,io)
	if(j_err)return
	if(.not.j_transopt)then                              !call dotrans(ivtransopt,1)
		write(6,*)'**crossed: trans-> must be present'
		j_err=.true.
		return
	endif !if(.not.j_transopt)  27177
	likeep=j_linkoption(iob,io,j_mkeep)
	if(likeep.le.0)then
		write(6,*)'**crossed: keep-> missing'
		j_err=.true.
		return
	endif !if(likeep.le.0)  27183
	nvar=j_o(iob)%i(likeep)
	ivkeep=j_deflist(iout,'%keep',listold=j_o(iob)%i(likeep:));if(j_err)return
	!	call j_deflist2(iout,'%keep',j_o(iob)%i(likeep:),ivkeep)
	!call j_deflist2(iout,'%vars',j_o(iob)%i(likeep:),ivvars)
	ivstage=j_getobject(iout,'$Stage',j_ipreal)
	!	ivobs=j_igetopt(iob,io,j_mobs)  !; if(ivobs.le.0)ivobs=j_ivobs
	j_yes=j_isoption(iob,io,j_mobs)
	if(j_yes)then
		write(6,*)'obs-> is no more option in data(), obs variable is data%obs, often Obs can be used'
		j_err=.true.
		return
	endif !if(j_yes)  27195
	allocate(inclass(1:mxclass))
	!!call j_clearoption(iob,io)  ! subroutine
	nobc=0
 
	nclass=1
	!	do k=1,jndatasetss
	ic=j_inlistobject(ivclass,j_divkeep)
	if(ic.le.0)then
		!	call j_printname('**crossed: class variable ',ivclass, 'not in data ', j_datasets(k))
		j_err=.true.
		return
	endif !if(ic.le.0)  27207
	inclass(nclass)=1
	!	ivmat=j_o(j_datasets(k))%i(1)
	!	nobs=j_o(ivmat)%i(1)    !h(1)=ivmat
	!	nkeep=j_o(ivmat)%i(2)    !h(1)=ivmat
	classv=j_o(j_divmat)%d(ic)
	do i=2,j_dnobs
		class=j_o(j_divmat)%d((i-1)*j_dnkeep+ic)
		if(class.ne.classv)then
			if(sym)then
				nobc=nobc+inclass(nclass)*(inclass(nclass)+1)/2
			else !if(sym)then
				nobc=nobc+inclass(nclass)*inclass(nclass)
			endif !if(sym)  27220
			nclass=nclass+1
			if(nclass.gt.mxclass)then
				allocate(inclass2(1:2*mxclass))
				inclass2(1:mxclass)=inclass
				deallocate(inclass)
				mxclass=2*mxclass
				inclass=>inclass2
				nullify(inclass2)
 
			endif !if(nclass.gt.mxclass)  27226
			classv=class
			inclass(nclass)=0
 
		endif !if(class.ne.classv)  27219
		inclass(nclass)=inclass(nclass)+1
 
	enddo !i=2,j_dnobs  27217
	if(sym)then
		nobc=nobc+inclass(nclass)*(inclass(nclass)+1)/2
	else !if(sym)then
		nobc=nobc+inclass(nclass)*inclass(nclass)
	endif !if(sym)  27242
 
 
	!	enddo !do k=1,jndatasetss
	ivmat=j_defmatrix(iout,'%matrix',int8(nobc),int8(nvar),j_matreg)
	ibas=0
	iobs0=1
	ncl=1
	!	do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
	iclass=0
	do j_iobs=j_dfrom,j_duntil
		j_v(ivstage)=j_1  !1.
		call j_getobs(j_iobs); if(j_err)return
		if(j_rejected)then
			write(6,*)'**reject/filter does not work in crossed'
			j_err=.true.;return
		endif !if(j_rejected)  27260
		iclass=iclass+1
		if(sym)then
			iup=j_iobs
		else !if(sym)then
			iup=iobs0+inclass(ncl)-1
		endif !if(sym)  27265
		! do j=iobs0,iup
		! j_v(ivstage)=2.
		! !	call j_getob(j_datasets(k),j)   !getob does not call transopt
		! if(i.eq.j)then
		! j_v(ivdummy)=1.
		! else !if(i.eq.j)then
		! j_v(ivdummy)=0.
		! endif !if(i.eq.j)then
		! call dotrans(j_ivtransopt,1)
		! if(j_err)return
 
		! j_o(ivmat)%d(ibas+1:ibas+nvar)=j_v( j_o(iob)%i(likeep+1:likeep+nvar) )
		! ibas=ibas+nvar
 
		! enddo !do j=iobs0,iup
 
		if(iclass.eq.inclass(ncl))then
			iobs0=i+1
			iclass=0
			ncl=ncl+1
		endif !if(iclass.eq.inclass(ncl))  27286
 
	enddo !j_iobs=j_dfrom,j_duntil  27257
	!enddo !do k=1,jndatasetss
 
	call j_defdata(iout,ivmat,ivkeep)
	deallocate(inclass)
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
	return
end subroutine !subroutine crossed(iob,io)
 
subroutine abs(iob,io) ! abs(   absolute value
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dabs(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dabs(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'abs, illegal argument type ',j_otype(irg)
		j_err=.true.
		return
	endif !if(j_otype(irg).eq.j_ipreal)  27306
 
 
	!io=io+narg+3
	return
end subroutine !subroutine abs(iob,io)
 
subroutine bitmatrix(iob,io) ! matrix() %%matrix
 
	! Section bitmatrix bitmatrix() Creates BITMATRIX
	! To be reported later,  see old manual
	! endsection
 
	integer ,dimension(:), allocatable:: ivals
	character*8 loppu
	logical isinc
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	ndim1=j_v(j_o(iob)%i(io+2))
	if(narg.eq.1)then
		ndim2=1
	else !if(narg.eq.1)then
		ndim2=j_v(j_o(iob)%i(io+3))
	end if !if(narg.eq.1)  27340
	iout=j_o(iob)%i(io+2+narg)
	ivcolmin=j_igetopt(iob,io,j_mcolmin)
	if(ivcolmin.gt.0)then
		icolmin=j_v(ivcolmin)
		more=1-icolmin
	else !if(ivcolmin.gt.0)then
		icolmin=1
		more=0
	endif !if(ivcolmin.gt.0)  27347
	ivin=j_igetopt(iob,io,j_min)
	if(ivin.ge.0)then
		if(ivin.gt.0)then
			!call j_getchar(ivin,j_filename,j_lef_)
			!call j_openread(j_filename(1:j_lef_),'*',nu)
			call j_getfile(nu,'r',ivfile=ivin)
			if(j_err)return
 
		else !if(ivin.gt.0)then
			isinc=.true.
			nu=j_inciv(j_ninc)
 
		end if !if(ivin.gt.0)  27356
		iofunc=j_codelink(iob,io,j_mfunc); if(j_err)return
		! if(j_linkoption(iob,io,j_mfunc).gt.0)then
		! iofunc=j_linkopt2(j_mfunc)   ! start adress for
		! ivfunc=j_o(iob)%i(j_linkoption(iob,io,j_mfunc)+1)
		! else !if(j_linkoption(iob,io,j_mfunc).ge.0)then
		! iofunc=0
		! end if !if(j_linkoption(iob,io,j_mfunc).ge.0)then
		!!call j_clearoption(iob,io)  ! subroutine
 
		if(ndim2.lt.0)then
			n1000=1000
			allocate (ivals(1:n1000))
			ivalmin=1000000
			ivalmax=-100000
		else !if(ndim2.lt.0)then
			ndim22=ndim2+more
			allocate (ivals(1:ndim22))
		endif !if(ndim2.lt.0)  27376
 
		if(ndim1.lt.0.or.ndim2.lt.0)then
 
			iobs=0
	10		continue
			if(isinc)then
				if(j_o(nu)%txt(iobs+1)(1:1).eq.'/')goto 20
				read(j_o(nu)%txt(iobs+1),*,err=90)n,(ivals(k),k=1,n)
 
			else !if(isinc)then
				read(nu,*,err=90,end=20)n,(ivals(k),k=1,n)
			endif !if(isinc)  27390
			iobs=iobs+1
 
			if(ndim2.lt.0.and.n.gt.0)then  !need to c
				if(iofunc.gt.0)then
					do k=1,n
 
						j_v(j_ivdollar)=ivals(k)
 
						!	call dotrans(iob,iofunc)
						!	if(j_err)return
						ivals(k)=j_codevalue(iob,iofunc)  !j_v(ivfunc)
						if(j_err)return
					enddo !k=1,n  27401
 
				endif !if(iofunc.gt.0)  27400
				ivalmin2=minval(ivals(1:n))
				if((ivcolmin.gt.0.or.ndim2.gt.0).and.ivalmin2.lt.icolmin)then
					write(6,*)'*row:',iobs, 'smallest value ',ivalmin2, ' smaller than colmin->',icolmin
					goto 90
				endif !if((ivcolmin.gt.0.or.ndim2.gt.0).and.ivalmin2.lt.icolmin)  27413
 
				ivalmin=min(ivalmin,ivalmin2)
				ivalmax=max(ivalmax,maxval(ivals(1:n)))
			endif !if(ndim2.lt.0.and.n.gt.0)  27399
			if(ndim1.lt.0.or.iobs.lt.ndim1)goto 10
 
			20 continue
			if(ndim1.lt.0)then
				ndim1=iobs
			elseif(iobs.lt.ndim1)then !if(ndim1.lt.0)then
				write(6,*)'*wrn, bitmatrix: data had ',iobs, 'rows, but matrix has', ndim1
			endif !if(ndim1.lt.0)  27424
			if(ndim2.le.0)then
				icolmin=ivalmin
				more=1-icolmin
 
				ndim2=ivalmax
				ndim22=ndim2+more
			endif !if(ndim2.le.0)  27429
			call j_printname('bitmatrix ',iout,' got dimensions')
			write(6,*)ndim1,' x (',icolmin,':',ndim2,')'
			if(.not.isinc)rewind(nu)
 
		endif !if(ndim1.lt.0.or.ndim2.lt.0)  27386
 
	endif !if(ivin.ge.0)  27355
 
	nel=(ndim1*ndim22+31)/32
 
	call j_getobject2(iout,j_ipbitmatrix);if(j_err)return
	allocate(j_o(iout)%i(0:5),j_o(iout)%i2(1:nel))
	icolmin1=icolmin-1
	j_o(iout)%i=(/nel,ndim1,ndim2,icolmin,ndim22,icolmin1/)
	j_o(iout)%i2=0.
 
	if(ivin.ge.0)then
		do iobs=1,ndim1
			if(isinc)then
				read(j_o(nu)%txt(iobs),*,err=90)n,(ivals(k),k=1,n)
			else !if(isinc)then
				read(nu,*,err=90,end=89)n,(ivals(k),k=1,min(n,ndim2))
			endif !if(isinc)  27454
			if(n.gt.ndim22)then
				write(6,*)'*wrn* bitmatrix:  row ',iobs, 'has too many values, extra ignored :', n
			endif !if(n.gt.ndim22)  27459
			do k=1,n
				if(iofunc.gt.0)then
					j_v(j_ivdollar)=ivals(k)
					call dotrans(iob,iofunc)
					if(j_err)return
					ivals(k)=j_v(ivfunc)
				endif !if(iofunc.gt.0)  27463
				if(ivals(k).lt.icolmin.or.ivals(k).gt.ndim2)then
					write(6,*)'**bitmatrix, row ',iobs,' has illegal column ',ivals(k)
					j_err=.true.
					deallocate(ivals)
					if(ivin.gt.0)call j_closeunit(nu)
					return
				endif !if(ivals(k).lt.icolmin.or.ivals(k).gt.ndim2)  27469
				call j_bitset(iout,iobs,ivals(k),1.)
 
			enddo !k=1,n  27462
 
		enddo !iobs=1,ndim1  27453
		deallocate(ivals)
	endif !if(ivin.ge.0)  27452
	if(ivin.eq.0)then
		!read(j_nul(j_ninc),'(a)')loppu
 
		if(loppu(1:1).ne.'/')then
			write(6,*)'**not legal end for bitmatrix -values'//loppu
			j_err=.true.
		end if !if(loppu(1:1).ne.'/')  27486
 
	end if !if(ivin.eq.0)  27483
 
	70 continue ! io=io+o(iob)%i(io+1)+3
	if(ivin.gt.0)call j_closeunit(nu)
	return
	90 write(6,*)'**error in reading matrix values for row ',iobs
	deallocate(ivals)
	if(ivin.gt.0)call j_closeunit(nu)
	j_err=.true.
	return
	89 write(6,*)'**bitmatrix: premature eof when readin row ',iobs
	deallocate(ivals)
	if(ivin.gt.0.and..not.isinc)call j_closeunit(nu)
	j_err=.true.
	return
 
end subroutine !subroutine bitmatrix(iob,io)
 
subroutine setvalue(iob,io) !set value for a bitmatrix  %%bitmatrix
	! Section setvalue setvalue() Set value for a BITMATRIX
	! To be reported later,  see old manual
	! endsection
 
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	irg=j_o(iob)%i(io+2)
	inde=j_o(iob)%i(io+3)
	select case(j_otype(irg))
	case(j_ipbitmatrix) !select case(j_otype(irg))
 
	j=j_v(inde)
 
	if(j.gt.j_o(irg)%i(1).or.j.le.0)then
		call j_printname('setvalue(',irg,' ')
		write(6,*)'**illegal second argument',j_v(inde)
		j_err=.true. ;return
	endif !if(j.gt.j_o(irg)%i(1).or.j.le.0)  27525
 
	select case(narg)
	case(3) !select case(narg)
	if(j_o(irg)%i(2).ne.j_o(irg)%i(3))then
		call j_printname('**setvalue(',irg,' syntax: setvalue(bitmatrix,row,column,value) ')
		j_err=.true.;return
	endif !if(j_o(irg)%i(2).ne.j_o(irg)%i(3))  27533
	icol=j_o(irg)%i(2)
 
	case(4) !select case(narg)
	icol=j_v(j_o(iob)%i(io+4))
	if(icol.lt.j_o(irg)%i(3).or.icol.gt.j_o(irg)%i(2))then
		call j_printname('setvalue(',irg,' ')
		write(6,*)'**illegal second argument',j_v(j_o(iob)%i(io+4))
		j_err=.true. ;return
	endif !if(icol.lt.j_o(irg)%i(3).or.icol.gt.j_o(irg)%i(2))  27541
 
	case default !select case(narg)
	call j_printname('**setvalue(bitmatrix, ), illegal number of arguments:',irg,' ')
	j_err=.true. ;return
 
	end select !select case(narg)
	call j_bitset(irg,j,icol,real(j_v(j_o(iob)%i(io+1+narg))))
 
	case default !select case(j_otype(irg))
 
	call j_printname('**not legal obj for setvalue(obj, ):',irg,' ')
	j_err=.true.
 
	end select !select case(j_otype(irg))
 
	return
end subroutine !subroutine setvalue(iob,io)
 
subroutine closures(iob,io)             !
	! Section closures closures() Convex closure
	! To be desrribed later,  see old manual
	! endsection
 
	logical*1, dimension(:,:),allocatable::itemp
	integer, dimension(:,:),allocatable::nei
	integer, dimension(:),allocatable::nei2,nnn
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	select case(j_otype(irg))
	case(j_ipbitmatrix) !select case(j_otype(irg))
	if(j_o(irg)%i(3).ne.1.or.j_o(irg)%i(2).ne.j_o(irg)%i(1))then
		call j_printname('**closures, bitmatrix ',irg, 'should be square (1:n,1:n) matrix')
		write(6,*)'colmin =', j_o(irg)%i(3),' nrows=',j_o(irg)%i(1),' ncols=',j_o(irg)%i(2)
		j_err=.true.
		return
	endif !if(j_o(irg)%i(3).ne.1.or.j_o(irg)%i(2).ne.j_o(irg)%i(1))  27581
	ndim=j_o(irg)%i(1)
	allocate(itemp(1:ndim,1:ndim),nei(1:ndim+1,1:ndim),nei2(1:ndim),nnn(1:ndim))
	itemp=.false.
	nei=0 !  .false.
	do i=1,ndim   !row
		do j=1,ndim
			if(i.eq.j)then
				itemp(i,j)=.true.
			else !if(i.eq.j)then
				itemp(i,j)=j_ibittest(irg,i,j).ne.0
			endif !if(i.eq.j)  27593
		enddo !j=1,ndim  27592
	enddo !i=1,ndim  27591
	lkm=1
 
	do i=1,ndim
		nn=0
		nn2=0
		key2=0
		do j=1,ndim
			if(i.eq.j)then
				nn=nn+1
				nei(lkm,nn)=i
				nn2=nn2+1
				nei2(nn2)=j
 
			else !if(i.eq.j)then
 
				if(itemp(i,j))then
					nn2=nn2+1
					nei2(nn2)=j  ! all neighbours
					if(all(itemp(j,nei(lkm,1:nn)))) then
						nn=nn+1
						nei(lkm,nn)=j
					else !if(all(itemp(j,nei(lkm,1:nn)))) then
						if(key2.eq.0)key2=j  ! first which does not belong to same group
 
					endif !if(all(itemp(j,nei(lkm,1:nn))))  27618
				endif !if(itemp(i,j))  27615
			endif !if(i.eq.j)  27607
 
		enddo !j=1,ndim  27606
		! checkk if the same as previous
				15 continue !write(6,*)'i,key2,nn2',i,key2,nn2
		do k=1,lkm-1
			if(nn.ne.nnn(k))cycle
			if(all(nei(k,1:nn).eq.nei(lkm,1:nn)))goto 17
		enddo !k=1,lkm-1  27631
		nnn(lkm)=nn
		lkm=lkm+1
 
				17 continue
		if(key2.gt.0)then
			key=key2
			key2=0
			nn=0
 
			do jj=1,nn2
				j=nei2(jj)
				if(i.eq.j)then
					nn=nn+1
					nei(lkm,nn)=i
 
				else !if(i.eq.j)then
					if(itemp(key,j))then
						! test if thsi is neighbor to all previous
 
						if(all(itemp(j,nei(lkm,1:nn)))) then
							nn=nn+1
							nei(lkm,nn)=j
 
						else !if(all(itemp(j,nei(lkm,1:nn)))) then
							if(j.gt.key)key2=j
 
						endif !if(all(itemp(j,nei(lkm,1:nn))))  27654
 
					endif !if(itemp(key,j))  27651
				endif !if(i.eq.j)  27646
			enddo !jj=1,nn2  27644
			goto 15
 
		endif !if(key2.gt.0)  27639
 
	enddo !i=1,ndim  27602
 
	lkm=lkm-1
 
	call j_getobject2(iout,j_ipbitmatrix);if(j_err)return
	nel=(ndim*lkm+31)/32
 
	allocate(j_o(iout)%i(0:5),j_o(iout)%i2(1:nel))
 
	j_o(iout)%i=(/nel,lkm,ndim,1,ndim,0/)
	j_o(iout)%i2=0
	do i=1,lkm
		do j=1,nnn(i)
			call j_bitset(iout,i,nei(i,j),1.)
		enddo !j=1,nnn(i)  27682
	enddo !i=1,lkm  27681
 
	deallocate(itemp,nei,nei2,nnn)
 
	case default !select case(j_otype(irg))
	call j_printname('**closures: argument not a bitmatrix:',irg,' ')
	j_err=.true.
	return
 
	end select !select case(j_otype(irg))
	70 continue !io=io+narg+3
	return
end subroutine !subroutine closures(iob,io)
 
subroutine difference(iob,io)
	! Section difference difference() Difference of LISTs or removes an object from LIST
	! difference() removes elements from a LIST
	! endheader
	! Option
	! Output& 1&LIST & the generated LIST.
	! Args&2&LIST|OBJ & The first argument is the LIST from which the elements of of the
	! are removed  If second argument is LIST then all of its eleemts are remove, other wise
	! it is assumed that the second argument is an object which is removed from the lisrt.
	!endoption
	! Ex diffex
	! lis=list(x1...x3,z3..z5);
	! lis2=list(x1,z5);
	! liso=difference(lis,lis2);
	! liso2=difference(liso,z3);
	! Continue=1
	! lisoer=difference(lis,z6); ! error occurs
	! liser=difference(Lis,x3);  !error occurs
	! Continue=0
	! endex
	! endsection
 
 
 
 
	integer,dimension(:),allocatable :: output
	!	call j_checkoutput(iob,io) output can be input
	!	if(j_err)return
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	!	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_otype(irg1).ne.j_iplist)then
		call j_printname('**difference: 1.argument not a list ',irg1,' ')
		j_err=.true.
		return
	endif !if(j_otype(irg1).ne.j_iplist)  27732
	if(j_otype(irg2).ne.j_iplist)then
		ii2=j_inlistobject(irg2,irg1)
		if(ii2.le.0)then
			call j_getname(irg1,irg2)
			write(6,*)j_oname2(1:j_loname2),' is not LIST or object in ',j_oname(1:j_loname)
			j_err=.true.
			return
		endif !if(ii2.le.0)  27739
		if(ii2.lt.j_o(irg1)%i(1))then
			do ij=ii2,j_o(irg1)%i(1)-1
				j_o(irg1)%i2(ij)=j_o(irg1)%i2(ij+1) !matrix operation may fail in optimized code
			enddo !ij=ii2,j_o(irg1)%i(1)-1  27746
		endif !if(ii2.lt.j_o(irg1)%i(1))  27745
		j_o(irg1)%i(1)=j_o(irg1)%i(1)-1   ! there remains unused elements
		return
	endif !if(j_otype(irg2).ne.j_iplist)  27737
 
 
	nel= j_ndiffer(j_o(irg1)%i2,j_o(irg1)%i(1),j_o(irg2)%i2,j_o(irg2)%i(1))
 
 
	if(nel.gt.0)then
		allocate( output(0:nel ))
		!call j_differ(j_o(irg1)%i,j_o(irg1)%i(0),j_o(irg2)%i,j_o(irg2)%i(0),j_o(iout)%i,j_o(iout)%i(0))
		call j_differ(j_o(irg1)%i2,j_o(irg1)%i(1),j_o(irg2)%i2,j_o(irg2)%i(1),output,noutput)
 
	endif !if(nel.gt.0)  27758
 
	ivout=j_deflist(iout,' ',list0=nel,list=output)
	! if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	! j_otype(iout)=j_iplist
 
	! allocate( j_o(iout)%i(0:nel ))
	! j_o(iout)%i(0:0)=nel
	! if(nel.gt.0)then
	! j_o(iout)%i(1:noutput)=output
	! deallocate(output)
	! endif !if(nel.gt.0)then
 
	!900 !io=io+narg+3   !
	return
 
end subroutine !subroutine difference(iob,io)
 
subroutine save(iob,io) ! save(
 
 
end !subroutine save(iob,io)
 
subroutine unsave(iob,io)
 
 
end subroutine !subroutine unsave(iob,io)
 
subroutine interpolate(iob,io)
	! Section interpolate interpolate() Linear or quadratic interpolation
	! endheader
	!Option
	! Output & 1& REAL & Interpolated value
	!Args&& & Odd number of numeric values given with any combination of numbers, matrices and lists. If there is
	! Last value is the argument. Intial half of values give x-values and rest, omitting the last, give y-values.
	! q & N|1 & & q-> indicates quadratic interpolation
	!endoption
	! Note. The argument x need not be within the interval of given x values (thus the function also
	! extrapolates).
	! endnote
	! endsection
 
	!double precision, dimension(:), pointer :: argx,argy !arguments of the function
	double precision first,step
	logical isq,isdown !found,
	!	subroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	!call j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	!	io=io_
 
 
 
	call j_startfunction(iob,io,j_ipreal,narg0,j_arg,iout,minarg=1,argval=j_argval,nval=narg)
	if(j_err)return
 
	!ismatrix=.false.
	isq=j_isoption(iob,io,j_mq)
	isdown=j_isoption(iob,io,j_mdown)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	if(j_isoptionseq(iob,io,first,step))then
		if(step.le.j_0)then
			write(6,*)'step=',step,' should be >0'
			j_err=.true.;return
 
		endif !if(step.le.j_0)  27823
		np=narg-1
		!		write(6,*)'first.step',first,step
		i=(j_argval(narg)-first)/step
		i=max(i+1,1)
 
 
		if(isq)then
			if(narg.lt.4)goto 900
			i=min(nval-2,i)
			j_dapu=first+(i-1)*step
 
 
			if(j_argval(narg)-j_dapu.lt.0.5d0*step.and.i.gt.1)then
				j_dapu2=j_dapu
				j_dapu=j_dapu-step
 
				i=i-1
			else
				j_dapu=first+(i-1)*step
				j_dapu2=j_dapu+step
 
			endif !if(j_argval(narg)-j_dapu.lt.0.5d0*step.and.i.gt.1)  27840
			j_dapu3=j_dapu2+step
 
			j_v(iout)=j_quad(j_argval(narg),j_dapu,j_dapu2,j_dapu3,j_argval(i),j_argval(i+1),j_argval(i+2))
		else
			!	i=min(i,np-1)
			!	i=max(i,np-1)
			i=min(np-1,i)
			!		write(6,*)'dapu',j_dapu
			j_v(iout)=j_argval(i)+(j_argval(i+1)-j_argval(i))*(j_argval(narg)-first-(i-1)*step)/step
 
			!function j_quad(x,x0,x1,x2,y0,y1,y2)
 
		endif !if(isq)  27834
		return
	endif !if(j_isoptionseq(iob,io,first,step))  27822
	!	io_=io_+narg+3
	!	write(6,*)'narg,argva',narg,j_argval
	!call j_arg2val(j_arg,j_gpnarg,j_gpval,nval)
	np=narg/2
	if(np.lt.2)goto 900
	if(2*np.eq.narg)then
		write(6,*)'*must have odd number of arguments'
		j_err=.true.
		return
	endif !if(2*np.eq.narg)  27870
 
	!j_argval(narg)  !j_v(j_o(iob)%i(io+1+narg))
	!if(j_argval(narg).le.j_argval(1))then
	!	i=1
	!else
	!	write(6,*)'isdown np',isdown,np
	if(isdown)then
		if(np.lt.2)then
			i=1
		else
			do i=2,np-1
				if(j_argval(narg).ge.j_argval(i))exit
			enddo !i=2,np-1  27885
			i=i-1
		endif !if(np.lt.2)  27882
 
		!	write(6,*)'ii,',j_argval(narg),j_argval(i)
		!	100 continue
		!	endif !if(j_argval(narg).le.j_argval(1))  27864
		if(j_argval(i+1).gt.j_argval(i))then
 
			write(6,*)'nondecreasing x-values even if down->'
			j_err=.true.;return
		endif !if(j_argval(i+1).gt.j_argval(i))  27894
		if(isq)then
			!	y0+(y1-y0)*(x-x0)/(x1-x0)
			!	write(6,*)'isq',i
			if(i.gt.1)then
 
				if(j_argval(narg)-j_argval(i-1).lt.j_argval(i+2)-j_argval(narg))i=i-1
			endif !if(i.gt.1)  27902
			!	write(6,*)i,j_argval(narg),j_argval(i),j_argval(i+1),j_argval(i+2),j_argval(np+i),j_argval(np+i+1),j_argval(np+i+2)
			j_v(iout)=j_quad(j_argval(narg),j_argval(i),j_argval(i+1),j_argval(i+2),j_argval(np+i),j_argval(np+i+1),j_argval(np+i+2))
		else
			j_v(iout)=j_argval(np+i)+(j_argval(np+i+1)-j_argval(np+i))*(j_argval(narg)-j_argval(i))/(j_argval(i+1)-j_argval(i))
 
		endif !if(isq)  27899
		return
 
	else
 
		if(np.lt.2)then
			i=1
		else
			do i=2,np-1
				if(j_argval(narg).le.j_argval(i))exit
			enddo !i=2,np-1  27919
			i=i-1
		endif !if(np.lt.2)  27916
		!	100 continue
		!	endif !if(j_argval(narg).le.j_argval(1))  27864
		!	write(6,*)'i,',i,isq,'isq'
		if(j_argval(i+1).lt.j_argval(i))then
 
			write(6,*)'nonincreasing x-values'
			j_err=.true.;return
		endif !if(j_argval(i+1).lt.j_argval(i))  27927
		if(isq)then
			!	y0+(y1-y0)*(x-x0)/(x1-x0)
			!	write(6,*)'isq',i
			if(i.gt.1)then
 
				if(j_argval(narg)-j_argval(i-1).lt.j_argval(i+2)-j_argval(narg))i=i-1
			endif !if(i.gt.1)  27935
			!	write(6,*)i,j_argval(narg),j_argval(i),j_argval(i+1),j_argval(i+2),j_argval(np+i),j_argval(np+i+1),j_argval(np+i+2)
			j_v(iout)=j_quad(j_argval(narg),j_argval(i),j_argval(i+1),j_argval(i+2),j_argval(np+i),j_argval(np+i+1),j_argval(np+i+2))
		else
			!		write(6,*)'mmm',i,np,j_argval(np+i),j_argval(np+i+1),j_argval(np+i),j_argval(narg),j_argval(i),j_argval(i+1),j_argval(i)
			j_v(iout)=j_argval(np+i)+(j_argval(np+i+1)-j_argval(np+i))*(j_argval(narg)-j_argval(i))/(j_argval(i+1)-j_argval(i))
 
		endif !if(isq)  27932
	endif !if(isdown)  27881
 
	return
 900 write(6,*)'interpolation has too few arguments ',narg
	j_err=.true.
	return
 
 
end subroutine !subroutine interpolate(iob,io)
 
subroutine sign(iob,io)  !
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	irg=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_otype(irg).eq.j_ipreal.and.j_otype(irg2).eq.j_ipreal)then
 
		if(j_v(irg2).ge.j_0)then
			j_v(iout)=abs(j_v(irg))
		else !if(j_v(j_o(iob)%i(io+2)).ge.0)then
			j_v(iout)=-abs(j_v(irg))
		endif !if(j_v(irg2).ge.j_0)  27965
	elseif(j_otype(irg).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipreal)then
		call j_copy(irg,iout)
		nd=j_o(irg)%i(3)
		if(j_v(irg2).ge.j_0)then
			j_o(iout)%d(1:nd)=abs(j_o(irg)%d(1:nd))
		else !if(j_v(j_o(iob)%i(io+2)).ge.0)then
			j_o(iout)%d(1:nd)=-abs(j_o(irg)%d(1:nd))
		endif !if(j_v(irg2).ge.j_0)  27973
	else
		call j_getname(irg,irg2)
		write(6,*)'sign(), illegal arguments ', j_oname(1:j_loname),' ',j_oname2(1:j_loname2)
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal.and.j_otype(irg2).eq.j_ipreal)  27963
	!io=io+narg+3
	return
end subroutine !subroutine sign(iob,io)
 
subroutine getobs(iob,io)   !this is j_function
	!	integer*8::iobs
	! Section getobs getobs() Obsevarion from  DATA
	! Getting an observation from a data set: \\
	! getobs(dataset,obs[,trans->])\\
	! Get the values of all variables associated with observation obs in data object dataset. First all the
	! variables stored in row obs in the data matrix are put into the corresponding real variables. If
	! a transformation set is permanently associated with the data object, these transformations are
	! executed.
	! endheader
	! Option
	!Args&2&DATA,REAL& DATA and the number of observation
	! trans&-1|1 & TRANS & these transformations are also executed.
	! endoption
	! endsection
 
 
	logical listarg
	logical arg
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	ivdata=j_o(iob)%i(io+2)
	j_iobs=j_v(j_o(iob)%i(io+3))
 
	if(j_otype(ivdata).ne.j_ipdata)then
		call j_printname('**getobs ',ivdata, 'is not a data set')
		j_err=.true.
		return
	endif !if(j_otype(ivdata).ne.j_ipdata)  28012
	nobs=j_o( j_o(ivdata)%i(1) )%i(1)
	if(j_iobs.le.0.or.j_iobs.gt.nobs )then
		write(6,*)'**getobs: is asking observation ',j_iobs,' but maximum is ',nobs
		call j_printname('**data set was',ivdata, ' ')
		j_err=.true.
		return
	endif !if(j_iobs.le.0.or.j_iobs.gt.nobs )  28018
	ivtrans=j_igetopt(iob,io,j_mtrans)
	call j_getobs0(ivdata,j_iobs)
	if(j_err)return
	if(ivtrans.gt.0)then
		if(j_otype(ivtrans).ne.j_iptrans)then
			call j_printname('**getobs, trans->',ivtrans, ' is not transformation set ')
			j_err=.true.
			return
		endif !if(j_otype(ivtrans).ne.j_iptrans)  28028
		call dotrans(ivtrans,1)
		if(j_err)return
		!!call j_clearoption(iob,io)  ! subroutine
	endif !if(ivtrans.gt.0)  28027
 
	return
end subroutine !subroutine getobs(iob,io)
 
subroutine der(iob,io)
	! Section der  Derivatives der()
	! Derivates of a function with respect to any of its arguments can be
	! computed using the derivation rules by using der() function in the previous line. The funcion must be expressed with
	! one-line statement. The function can call other functions using the standard way
	! to obtain objects from transformations, but these functions cannot
	! contain variables for which derivatives are obtained.
	! Nonlinear regression needs the derivatives with respect to the parameters.\\
	!der() function does not have an explicit output, but
	!  der() accompanied with the function produces REAL variable for each of the
	! argument variables of der(). If the output of the next line is f and der() has argument a, then
	! f?/?a will get the value of the derivative of f with respect to a.
 
	! endheader
	!Note Derivative are now utlized in nonliear regression, but they will also be utilized in
	!nonlinear programming.
	!endnote
	! Ex derex Derivatives with der()
	! transa=trans()
	! der(x)
	! f=(1+x)*cos(x)
	! /
	! fi=draw(func->transa(f?/?x),x->x,xrange->(0,10),color->Blue,continue->)
	! fi=draw(func->transa(f),x->x,xrange->(0,10),color->Cyan,append->,continue->fcont)
	!endex
	!Ex derex2
	! X=matrix(do->(0,1000,10))
	! e=matrix(nrows(X))
	! e=rann(0,2);
	! A,Pmax,Res=0.1,20,2
	! A*Pmax*1000/(A*1000+Pmax);
	! Y=A*Pmax*X/.(A*X+Pmax)-Res+e  !rectangular hyperbola used often for photosynthesis
 
	! transa=trans()
	! der(A,Pmax,Res)
	! f=A*Pmax*I/(A*I+Pmax)-Res
	! /
 
	! fi=draw(func->(transa(f)),x->I,xrange->(0,1000),color->Orange,width->2,continue->,show->0)
	! da=newdata(X,Y,e,extra->(Regf,Resid),read->(I,P,er))
	! stat()
	! fi=plotyx(P,I,append->,show->0,continue->fcont)
 
	! A,Pmax,Res=0.07,17,3 !initial values
 
	! fi=draw(func->(transa(f)),x->I,xrange->(0,1000),color->Green,width->2,append->,show->0,continue->)
	! reg=nonlin(P,f,par->(A,Pmax,Res),var->,corr->,data->da,trans->transa)
	! reg%var;
	! reg%corr;
	! corrmatrix(reg%var);
	! fi=draw(func->(transa(f)),x->I,xrange->(0,1000),color->Violet,append->,continue->fcont)
	!endex
	!endsection
 
 
	double precision res,vee
	parameter (rlog10e=0.4342944819)
	integer,pointer,dimension(:)::arg
	integer,pointer,dimension(:)::deriv
	logical isvar,isvar2,isarg2,finito,isilo,isilo2
	double precision::u,v
	integer ::il
	integer ::il2
	nd=j_o(iob)%i(io+1) !number parameters and derivative
	ioutpos=io+2+nd  !standard output postion stores the location where the function ends
	iexitloc=j_o(iob)%i(ioutpos) !location where the function ends (is done)
 
	ioutfin=j_o(iob)%i(iexitloc)  !output of the function the output is put to temporal outputs initially
 
	!	write(6,*)'ioutpos',ioutpos,'iexitloc',iexitloc,'ioutfin',ioutfin
	arg=>j_o(iob)%i(io+2:io+1+nd)  !number of var
	deriv=>j_o(iob)%i(ioutpos+2:ioutpos+1+nd)
	!		write(6,*)'arg',arg
	!		write(6,*)'deriv',deriv
	!	write(6,*)j_o(iob)%i(1:20)
 
	io=ioutpos+nd+2
	!	write(6,*)'io ',io,j_o(iob)%i(io)
	ioutmax=j_mxnamedv+j_mxtemporalv0  !max temporal output
	finito=.false.
	!where function starts
	!	write(6,*)'nd,ioutpos,iexitloc,ioutfin,arg,der,io',nd,ioutpos,iexitloc,ioutfin,arg,deriv,io
	!write(6,*)j_o(iob)%i(io:io+20)
	do while(.true.)
		!this cycle works in the sawm way as dotrans cycle
		ifunc=j_o(iob)%i(io) !
		!	write(6,*)'io ',io
		!
		narg=j_o(iob)%i(io+1)
		io2=io+2+narg  !location of output
		!derivatives are stored for the temporal variables which have space in d-part.
		!In the output location is a named object whihc does not have the d-part.
		! Thus  the derivaties are first computed for the last function as if
		! the last out put would be a temporrary object, whose index is one larger than
		! the other temporal outputs.
		!finally the value of the function is put into the named output
		if(io2.eq.iexitloc)then
			iout=ioutmax+1   !last output is addtional temporal
			finito=.true.
			!	write(6,*)'iout0 ',iout
		else !if(io2.eq.iexitloc)then
			iout=j_o(iob)%i(io2)
			!		write(6,*)'ioutmax,iout',ioutmax,iout
			ioutmax=max(ioutmax,iout)
		endif !if(io2.eq.iexitloc)  28137
		! write(6,*)'<3738ifunc,narg,io2,iout,ioutmax,j_mxnamedv',ifunc,&
		! narg,io2,iout,ioutmax,j_mxnamedv
		! write(6,*)'io,io2,ifunc,narg,iout,ioutmax',io,io2,ifunc,narg,iout,ioutmax
		! write(6,'(30i5)')j_o(iob)%i(1:io+10)
		! write(6,*)'allo ',allocated(j_o(iout)%d)
 
		! write(6,*)'iout,nd ',iout,nd
		j_o(iout)%d(1:nd)=j_0
		irg=j_o(iob)%i(io+2)
		u=j_v(irg)  !
		isvar=irg.le.j_mxnamedv.or.irg.gt.j_nv
		!isvar =is first argument named variable or constant, if not it is temporary variable
		isilo=.false.
		if(isvar)then
			do il=1,nd
				if(irg.eq.arg(il))goto 135
			enddo !il=1,nd  28160
			il=0
			!			il=findloc(arg,irg,dim=1)
			! il=ilo(1)
135				isilo=il.gt.0  ! is first argument in der (...,possible only for named variable
		endif !if(isvar)  28159
		isarg2=narg.gt.1  !is there second argument
		if(isarg2)then
			irg2=j_o(iob)%i(io+3)
			v=j_v(irg2)
			isvar2=irg2.le.j_mxnamedv.or.irg2.gt.j_nv
			if(isvar2)then
				do il2=1,nd
					if(irg2.eq.arg(il2))goto 136
				enddo !il2=1,nd  28174
				il2=0
				!			il=findloc(arg,irg,dim=1)
				! il=ilo(1)
136					isilo2=il2.gt.0  !is second argument in der () ,il2 is then the position



			endif !if(isvar2)  28173
		endif !if(isarg2)  28169
		!	write(6,*)'isvar,isvar2,il,il2,isilo,isilo2,iout',isvar,isvar2,il,il2,isilo,isilo2,iout
 
		!		write(6,*)'ifunc,j_fplus,j_fmult,iout,io2',ifunc,j_fplus,j_fmult,iout,io2
		! d f(u) =f'(u) du
		! d f(u,v)= h(u,v) du + g(u,v) dv
		! if u is named variable then du=1 and for other named variables x dx=0
		! for intermediate results variables derivatives with respect to arguments are
		!stored in vector j_o(iout)%d
		!derivation rules taken from Standard mathematical tables which uses u and v notation
		select case (ifunc)
 
		case (j_fplus) !select case (ifunc)
		!d(u+v)=du+dv  ;
		j_v(iout)=u+v
		if(.not.isvar)then  !first argument is intermediate result
			do id=1,nd
				j_o(iout)%d(id)=j_o(irg)%d(id)
			enddo !id=1,nd  28201
		elseif(isilo)then !if(.not.isvar)then first argument il'th der var
			!		write(6,*)'<56554 ',iout,il
			j_o(iout)%d(il)=j_1   !du =1
 
		endif !if(.not.isvar)  28200
 
		if(.not.isvar2)then !for second variable it must be taken into account that the first argument has
			! already provided
			do id=1,nd
				j_o(iout)%d(id)=j_o(iout)%d(id)+j_o(irg2)%d(id)
			enddo !id=1,nd  28212
		elseif(isilo2)then !if(.not.isvar2)then
			j_o(iout)%d(il2)=j_o(iout)%d(il2)+j_1
		endif !if(.not.isvar2)  28210
 
		case (j_fminus) !select case (ifunc)
		!	write(6,*)'iobi',j_o(iob)%i(io:io+4)
		!	write(6,*)'narg,isarg2,isvar,isvar2,isilo,isilo2,il,il2',narg,isarg2,isvar,isvar2,isilo,isilo2,il,il2
		!d(u-v)=du-dv  ;
		j_v(iout)=u-v
		if(.not.isvar)then  !first argument intermediate result
			do id=1,nd
				j_o(iout)%d(id)=j_o(irg)%d(id)
			enddo !id=1,nd  28225
		elseif(isilo)then !if(.not.isvar)then
			!		write(6,*)'<56554 ',iout,il
			j_o(iout)%d(il)=j_1
 
		endif !if(.not.isvar)  28224
 
		if(.not.isvar2)then
			do id=1,nd
				j_o(iout)%d(id)=j_o(iout)%d(id)-j_o(irg2)%d(id)
			enddo !id=1,nd  28235
		elseif(isilo2)then !if(.not.isvar2)then
			j_o(iout)%d(il2)=j_o(iout)%d(il2)-j_1
		endif !if(.not.isvar2)  28234
 
 
 
		case (j_fmult) !select case (ifunc)
		!	d(uv)=v*du  +u*dv
		!write(6,*)'put ',iout,'u*v',u,v,u*v,isvar,isvar2,isilo,isilo2
		j_v(iout)=u*v
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=v*j_o(irg)%d(id)
			enddo !id=1,nd  28249
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=v
		endif !if(.not.isvar)  28248
 
		if(.not.isvar2)then  !u dv
			do id=1,nd
				j_o(iout)%d(id)=j_o(iout)%d(id)+u*j_o(irg2)%d(id)
			enddo !id=1,nd  28257
		elseif(isilo2)then !if(.not.isvar2)then
			j_o(iout)%d(il2)=j_o(iout)%d(il2)+u
		endif !if(.not.isvar2)  28256
 
		!write(6,*)'mult ',real(j_v(irg)),real(j_v(irg2)),isvar,isilo,ilo,isvar2,isilo2,ilo2,j_o(iout)%d(1:nd)
 
		case (j_fdiv) !select case (ifunc)
		! d u/v=  1/v * du -u/v**2 dv
 
		if(v.eq.j_0)then
			write(6,*)'division with zero'
			j_err=.true.
			return
		endif !if(v.eq.j_0)  28269
 
		j_v(iout)=u/v
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=j_o(irg)%d(id)/v
			enddo !id=1,nd  28277
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/v
		endif !if(.not.isvar)  28276
 
		if(.not.isvar2)then
			do id=1,nd
				j_o(iout)%d(id)=j_o(iout)%d(id)-j_o(irg2)%d(id)*u/v**2
			enddo !id=1,nd  28285
		elseif(isilo2)then !if(.not.isvar2)then
			j_o(iout)%d(il2)=j_o(iout)%d(il2)-u/v**2
		endif !if(.not.isvar2)  28284
 
		case (j_fipower) !select case (ifunc)
		!d u**n =(n-1)*u**(n-1) du
		n=v
		j_v(iout)=u**n
		if(.not.isvar)then
			do id=1,nd
				!j_o(iout)%d(id)=(v-j_1)*u**(n-1)*j_o(irg)%d(id)
				j_o(iout)%d(id)=(v-j_1)*j_v(iout)*j_o(irg)%d(id)/u
			enddo !id=1,nd  28297
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=(v-j_1)*j_v(iout)/u  !(v-j_1)*u**(n-1)
		endif !if(.not.isvar)  28296
 
		case (j_fpower) !select case (ifunc)
		! d u**v =v*u**(v-1) du + (log u)*u**v dv
		if(u.lt.j_0)then
			write(6,*)'in derivatives: ** has negative argument ',u
			j_err=.true.;return
		endif !if(u.lt.j_0)  28307
 
		j_v(iout)=u**v
 
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=v*u**(v-j_1)*j_o(irg)%d(id)
				!	j_o(iout)%d(id)=v*j_v(iout)*j_o(irg)%d(id)/u
			enddo !id=1,nd  28315
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=v*u**(v-j_1)  !j_v(iout)/u  !v*u**(v-j_1)
		endif !if(.not.isvar)  28314
 
		if(.not.isvar2)then
			do id=1,nd
				!	j_o(iout)%d(id)=j_o(iout)%d(id)+log(u)*u**v*j_o(irg2)%d(id)
				if(u.le.j_0)then
					write(6,*)'in derivates, nonpositive number ',u,' raised to power'
					j_err=.true.;return
				endif !if(u.le.j_0)  28326
				j_o(iout)%d(id)=j_o(iout)%d(id)+log(u)*j_v(iout)*j_o(irg2)%d(id)
			enddo !id=1,nd  28324
		elseif(isilo2)then !if(.not.isvar2)then
			!		write(6,*)'66iout,j_mxnamedv+j_mxtemporalv0',iout,j_mxnamedv+j_mxtemporalv0
			if(u.le.j_0)then
				write(6,*)'in derivates, nonpositive number ',u,' raised to power'
				j_err=.true.;return
			endif !if(u.le.j_0)  28334
			j_o(iout)%d(il2)=j_o(iout)%d(il2)+log(u)*j_v(iout)
		endif !if(.not.isvar2)  28323
 
		case (j_fexp) !select case (ifunc)
 
		j_v(iout)=exp(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=j_v(iout)*j_o(irg)%d(id)
			enddo !id=1,nd  28345
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_v(iout)
		endif !if(.not.isvar)  28344
 
		case (j_flogistic) !select case (ifunc)
 
		if(u.gt.j_0)then
			j_v(iout)=j_1/(j_1+exp(-u))
		else !if(u.gt.j_0)then
			j_v(iout)=exp(u)/(j_1+exp(u))
		endif !if(u.gt.j_0)  28354
 
		if(.not.isvar)then
			!		write(6,*)'ilo',isilo
			do id=1,nd
				!			write(6,*)'<44',j_o(irg)%d(id)
				j_o(iout)%d(id)=j_v(iout)*(j_1-j_v(iout))*j_o(irg)%d(id)
			enddo !id=1,nd  28362
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_v(iout)*(j_1-j_v(iout))
		endif !if(.not.isvar)  28360
 
		case (j_fsqrt) !select case (ifunc)
		if(u.lt.j_0)then
			write(6,*)'sqrt has nonpostive argument ',u
			j_err=.true.;return
		endif !if(u.lt.j_0)  28371
		j_v(iout)=sqrt(u)
		if(.not.isvar)then
			do id=1,nd
				!	j_o(iout)%d(id)=v*u**(v-j_1)*j_o(irg)%d(id)
				j_o(iout)%d(id)=j_o(irg)%d(id)/(2.d0*j_v(iout))
			enddo !id=1,nd  28377
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/(2.d0*j_v(iout))  !v*u**(v-j_1)
		endif !if(.not.isvar)  28376
		case (j_fsqrt2) !select case (ifunc)
 
		v=abs(u)
		j_v(iout)=sqrt(v)
		if(.not.isvar)then
			do id=1,nd
				!	j_o(iout)%d(id)=v*u**(v-j_1)*j_o(irg)%d(id)
				j_o(iout)%d(id)=j_o(irg)%d(id)/(2.d0*j_v(iout))
			enddo !id=1,nd  28389
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/(2.d0*j_v(iout))  !v*u**(v-j_1)
		endif !if(.not.isvar)  28388
		if(u.lt.j_0)j_v(iout)=-j_v(iout)
 
		case (j_fsin) !select case (ifunc)
		j_v(iout)=sin(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=cos(u)*j_o(irg)%d(id)
			enddo !id=1,nd  28401
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=cos(u)
		endif !if(.not.isvar)  28400
 
		case (j_fcos) !select case (ifunc)
		j_v(iout)=cos(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=-sin(u)*j_o(irg)%d(id)
			enddo !id=1,nd  28411
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=-sin(u)
		endif !if(.not.isvar)  28410
 
		case (j_flog) !select case (ifunc)
		if(u.le.j_0)then
			write(6,*)'*logarithm of nonpositive value ',u
			j_err=.true.;return
		endif !if(u.le.j_0)  28419
		j_v(iout)=log(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=j_o(irg)%d(id)/u
			enddo !id=1,nd  28425
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/u
		endif !if(.not.isvar)  28424
 
		case (j_flog10) !select case (ifunc)
		if(u.le.j_0)then
			write(6,*)'cannot take log10 from nonpositive value ',u
			j_err=.true.;return
		endif !if(u.le.j_0)  28433
		j_v(iout)=dlog10(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=dlog10(j_e)*j_o(irg)%d(id)/u
			enddo !id=1,nd  28439
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/u
		endif !if(.not.isvar)  28438
 
 
 
		case default !select case (ifunc)
		write(6,*)'derivative for function ',j_functions(ifunc),' not yet implemented, ask J. Lappi'
		!	write(6,*)j_flogistic,ifunc
		j_err=.true.
		return
		end select !select case (ifunc)
		if(finito)exit
		io=io+narg+3
	enddo !while(.true.)  28124
	!		write(6,*)'fin',ioutfin,iout
	j_v(ioutfin)=j_v(iout)
	do i=1,nd
		!	write(6,*)'73737deric',deriv(i)
		j_v(deriv(i))=j_o(iout)%d(i)
		!	write(6,*)'73737deric',deriv(i),j_v(deriv(i))
	enddo !i=1,nd  28459
 
end subroutine !subroutine der(iob,io)
 
subroutine itraceoff(iob,io)
 
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	do i=1,narg
		il=j_inlistobject(j_o(iob)%i(io+1+i),j_ivtracevars)
		if(il.le.0)then
			call j_printname('*  trying to ;traceoff variabe ',j_o(iob)%i(io+1+i),' which was not in ;trace')
			j_err=.true.
			return
		else !if(il.le.0)then
			call j_putod(j_ivtracestatus,il,j_0)
		endif !if(il.le.0)  28475
	enddo !i=1,narg  28473
 
	return
end subroutine !subroutine itraceoff(iob,io)
 
subroutine trace(iob,io)
	double precision tracelevel
	double precision ismin,ismax,valmin,valmax
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	! as in itrace
 
	nmin=-1
 
	ismin=0.
	iexit=1
	if(j_linkoption(iob,io,j_merrexit).ge.0)iexit=2
	imi=j_linkoption(iob,io,j_mmin,clear=.true.)
	if(imi.gt.0)then
 
		nmin=j_o(iob)%i(imi)
		if(nmin.eq.1)then
			valmin=j_v(j_o(iob)%i(imi+1))
 
		elseif(narg.ne.nmin.and.nmin.ne.0)then !if(nmin.eq.1)then
			write(6,*)';trace there must be one min or one for each var'
			j_err=.true.
			return
		endif !if(nmin.eq.1)  28505
		ismin=iexit
	endif !if(imi.gt.0)  28502
 
	nmax=-1
	ismax=0.
	imi=j_linkoption(iob,io,j_mmax,clear=.true.)
	if(imi.gt.0)then
		nmax=j_o(iob)%i(imi)
		if(nmax.eq.1)then
			valmax=j_v(j_o(iob)%i(imi+1))
 
		elseif(narg.ne.nmax.and.nmax.ne.0)then !if(nmax.eq.1)then
			write(6,*)'trace there must be one max or one for each var'
			j_err=.true.
			return
		endif !if(nmax.eq.1)  28521
		ismax=iexit
	endif !if(imi.gt.0)  28519
	if(nmax.lt.0.and.nmin.lt.0.and.iexit.gt.1)then
		write(6,*)'trace:, errexit must be with min-> or max->'
 
	endif !if(nmax.lt.0.and.nmin.lt.0.and.iexit.gt.1)  28531
 
	tracelevel=-1.
 
	if(nmin.gt.0.or.nmax.gt.0)tracelevel=1
	ivlevel=j_igetopt(iob,io,j_mlevel)
 
	if(ivlevel.gt.0)then
		tracelevel=j_v(ivlevel)
		if(tracelevel.lt.0.or.tracelevel.gt.2)then
			write(6,*)'trace: illegal level-> ' ,tracelevel
		endif !if(tracelevel.lt.0.or.tracelevel.gt.2)  28543
 
	endif !if(ivlevel.gt.0)  28541
 
	do i=1,narg
		il=j_inlistobject(j_o(iob)%i(io+1+i),j_ivtracevars)
		if(il.le.0)then
			call j_printname('*  trying to trace() variabe ',j_o(iob)%i(io+1+i),' which was not in ;trace')
			j_err=.true.
			return
		endif !if(il.le.0)  28551
 
		if(tracelevel.ge.0)call j_putod(j_ivtracelevel,il,tracelevel)
		call j_putod(j_ivtracecount,il,j_0)
		if(nmin.ge.0)then
			call j_putod(j_ivtraceminstatus,il,ismin)
			if(nmin.gt.1)valmin=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmin)+i))
			call j_putod(j_ivtracemin,il,valmin) !real value
		endif !if(nmin.ge.0)  28559
		if(nmax.ge.0)then
			call j_putod(j_ivtracemaxstatus,il,ismax)
			if(nmax.gt.1)valmax=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmax)+i))
			call j_putod(j_ivtracemax,il,valmax)
		endif !if(nmax.ge.0)  28564
	enddo !i=1,narg  28549
 
	!!call j_clearoption(iob,io)  ! subroutine
 
	return
end !subroutine trace(iob,io)
 
 
 
 
 
! subroutine tracenow(iob,io)   !io?
 
! character*24 name,name2
! logical bypass
! ii=j_o(iob)%i(io+1)
! j_level=j_o(j_ivtracelevel)%d(ii)
! if(j_level.le.0)goto 80
! j_o(j_ivtracecount)%d(ii)=j_o(j_ivtracecount)%d(ii)+1.
 
! iv=j_traceiv(ii)
! if(j_o(j_ivtraceminstatus)%d(ii).gt.0)then
! if(j_otype(iv).ne.j_ipreal)then
! write(6,*)'*changing variable with min checking into a general object'
! j_err=.true.
! goto 20
 
! endif !if(j_otype(iv).ne.j_ipreal)  23894
! if(j_v(iv).lt.j_o(j_ivtracemin)%d(ii))then
! j_err=j_o(j_ivtraceminstatus)%d(ii).ge.2
! goto 20
! endif !if(j_v(iv).lt.j_o(j_ivtracemin)%d(ii))  23900
! endif !if(j_o(j_ivtraceminstatus)%d(ii).gt.0)  23893
 
! if(j_o(j_ivtracemaxstatus)%d(ii).gt.0)then
! if(j_otype(iv).ne.j_ipreal)then
! write(6,*)'*changing variable with max checking into a general object'
! j_err=.true.
! goto 20
! endif !if(j_otype(iv).ne.j_ipreal)  23907
 
! if(j_v(iv).gt.j_o(j_ivtracemax)%d(ii))then
! j_err=j_o(j_ivtracemaxstatus)%d(ii).ge.2
! goto 20
! endif !if(j_v(iv).gt.j_o(j_ivtracemax)%d(ii))  23913
! endif !if(j_o(j_ivtracemaxstatus)%d(ii).gt.0)  23906
! if(j_level.le.1)goto 80
 
! 20 call j_getline(j_ivnames,iv,name,le)
! if(j_err)return
 
! lin=j_o(iob)%i(io+2)
 
! if(lin.gt.0)then
! call j_getline(j_ivnames,iob,name2,le2)
! if(j_err)return
! if(j_otype(iv).eq.j_ipreal)then
! write(6,*)name(1:le),' got value ',j_v(iv),' in ',name2(1:le2), ' at line ',lin,' :'
! else !if(j_otype(iv).eq.j_ipreal)then
! write(6,*)name(1:le),' new ',j_otypes(j_otype(iv)), ' in ',name2(1:le2), ' at line ',lin,' :'
! endif !if(j_otype(iv).eq.j_ipreal)  23928
! call j_printtext(j_o(iob)%i2(11),lin)
! else !if(lin.gt.0)then
! if(j_otype(iv).eq.j_ipreal)then
! write(6,*)name(1:le),' got value ',j_v(iv)
! else !if(j_otype(iv).eq.j_ipreal)then
! write(6,*)name(1:le),' new ',j_otypes(j_otype(iv))
 
! endif !if(j_otype(iv).eq.j_ipreal)  23935
! ! if(j_ninc.gt.1)then
! ! write(6,*)'**input line: ',j_inp(1:j_linp)
! ! endif !if(j_ninc.gt.1)then
! endif !if(lin.gt.0)  23925
 
! 80 continue !io=io+3
! return
! end subroutine !subroutine tracenow(iob,io)
 
! subroutine tracetest(iob,io)   !
 
! !	io=io_
! narg=j_o(iob)%i(io+1)
! !	io_=io_+narg+3
! do i=1,narg
! iset=j_o(iob)%i(io+i+1)
! if(j_otype(iset).ne.j_iptraceset)then
! call j_printname('*tracetest: ', iset,' is not a traceset')
! j_err=.true.
! return
! endif !if(j_otype(iset).ne.j_iptraceset)  23957
! do j=1,j_o(iset)%i(0)
! ii=j_o(iset)%i(j)
! if(j_o(j_ivtracecount)%d(ii).le.0)then
! call j_printname('*tracecount for ',j_traceiv(ii), ' is zero')
! j_err=.true.
! else !if(j_o(j_ivtracecount)%d(ii).le.0)then
! j_o(j_ivtracecount)%d(ii)=0.
! endif !if(j_o(j_ivtracecount)%d(ii).le.0)  23964
! enddo !j=1,j_o(iset)%i(0)  23962
! enddo !i=1,narg  23955
 
! return
! end subroutine !subroutine tracetest(iob,io)
 
subroutine existobject(iob,io)  !  vapaa
 
	logical ex
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	irg=j_o(iob)%i(io+2)
 
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	j_v(iout)=0
	if(j_otype(irg).ne.j_ipchar)then
		write(6,*)'exist requires text as argument'
		j_err=.true.
		return !goto 90 !return
	endif !if(j_otype(irg).ne.j_ipchar)  28683
	call j_getchar(irg,j_filename,le);if(j_err)return
	if(j_linkoption(iob,io,j_mobject).ge.0)then
		ex=j_object(j_filename(1:le)).gt.0
	else !if(j_linkoption(iob,io,j_mobject).ge.0)then
		inquire(file=j_filename(1:le),exist=ex)
	endif !if(j_linkoption(iob,io,j_mobject).ge.0)  28689
	!!call j_clearoption(iob,io)  ! subroutine
 
	if(ex)j_v(iout)=1
	!	90	io=io+narg+3
	return
end subroutine !subroutine existobject(iob,io)
 
subroutine exist(iob,io)  !  exist(
 
	logical ex
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	irg=j_o(iob)%i(io+2)
 
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	j_v(iout)=0.
	if(j_otype(irg).ne.j_ipchar)then
		write(6,*)'existobject requires text as argument'
		j_err=.true.
		return !goto 90 !return
	endif !if(j_otype(irg).ne.j_ipchar)  28711
	call j_getchar(irg,j_filename,le);if(j_err)return
	if(j_linkoption(iob,io,j_mobject).ge.0)then
		iobj=j_object(j_filename(1:le))
		if(iobj.gt.0)iobj=j_otype(iobj)
		j_v(iout)=iobj
		!!call j_clearoption(iob,io)  ! subroutin
	else !if(j_linkoption(iob,io,j_mobject).ge.0)then
		inquire(file=j_filename(1:le),exist=ex)
		if(ex)j_v(iout)=1.d0
	endif !if(j_linkoption(iob,io,j_mobject).ge.0)  28717
 
 
 
	!	90	io=io+narg+3
	return
end subroutine !subroutine exist(iob,io)
 
subroutine laasvol(iob,io)  !
	! Section laasvol laasvol() Volume equations of Laasasenaho
	! To be reported later.
	! endsection
 
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	ipl=j_v(j_o(iob)%i(io+2))
	dbh=j_v(j_o(iob)%i(io+3))
	select case (narg)
	case(2) !select case (narg)
	j_v(iout)=cv1k(ipl,dbh)
	case (3) !select case (narg)
	j_v(iout)=cv2k(ipl,dbh,j_v(j_o(iob)%i(io+4)))
	case(4) !select case (narg)
	j_v(iout)=cv3k(ipl,dbh,j_v(j_o(iob)%i(io+4)), j_v(j_o(iob)%i(io+5)))
	end select !select case (narg)
 
	!	io=io+narg+3
	return
 
	return
end subroutine !subroutine laasvol(iob,io)
 
subroutine laaspoly(iob,io)   !
	! Section laaspoly laaspoly() Polynomial stem curves of Laasasenaho
	! To be reported later.
	! endsection
 
	implicit double precision(a-h,o-z)
	call j_checkoutput(iob,io)
	if(j_err)return
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	ipl=j_v(j_o(iob)%i(io+2))
	dbh=j_v(j_o(iob)%i(io+3))
 
	if(j_otype(iout).gt.j_ipreal.and.j_otype(iout).ne.j_iplaaspoly)call j_del(iout)
	if(j_otype(iout).le.j_ipreal)then
		j_otype(iout)=j_iplaaspoly
		allocate(j_o(iout)%d(1:10))
	endif !if(j_otype(iout).le.j_ipreal)  28773
 
	select case (narg)
	case (3) !select case (narg)
	call crk(ipl,dbh,j_0,j_v(j_o(iob)%i(io+4)),j_0,j_o(iout)%d)
	case(4) !select case (narg)
	call crk(ipl,dbh,j_v(j_o(iob)%i(io+4)),j_v(j_o(iob)%i(io+5)),j_0,j_o(iout)%d)
	end select !select case (narg)
 
	!90	io=io+narg+3
	return
 
	return
end subroutine !subroutine laaspoly(iob,io)
 
subroutine loggamma(iob,io)  !
	! Section loggamma loggamma() Log of gamma function
	! Function gamma() produces the value of loggamma funtion for a positive argument.
	!
	! The function utilises gamma subroutine from
	! library dcdflib in Netlib. Loggamma is used in statistics in many cases where
	! gamma function gets a too large value to be presented in double precision.
	! endsection
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	arg=j_v(j_o(iob)%i(io+2))
	if(arg.lt.0)then
		write(6,*)'*gamma, argument should be >0, is ',arg
		j_err=.true.
		return
 
	endif !if(arg.lt.0)  28803
	write(6,*)'loggamma given if you need it, (J.L.)'
	j_err=.true.
	return
	!numerical recipes not availablegammln(v(o(iob)%i(io+2)))
	!	90 io=io+narg+3
	return
end subroutine !subroutine loggamma(iob,io)
 
subroutine gamma_(iob,io)  ! gamma(iob,io)
	! Section gamma gamma() Gamma function
	! Function gamma() produses the value of gamma funtion for a positive argument.
	! The function utilises gamma subroutine from
	! library dcdflib in Netlib. For computing gamma function for a product, loggamma() is often needed.
	! endsection
 
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	arg=j_v(j_o(iob)%i(io+2))
	if(arg.lt.0)then
		write(6,*)'*gamma, argument should be >0, is ',arg
		j_err=.true.
		return
 
	endif !if(arg.lt.0)  28828
	j_v(iout)=gamma(arg)   !
	!	90 io=io+narg+3
	return
end subroutine !subroutine gamma_(iob,io)
 
subroutine negbin(iob,io)  !
	! Section negbin negbin() Negative binomial
	! negbin(]k[,]myy[,]theta[)=
	! The probability that a negative binomial random variable
	! has value ]k[ when the variable
	! has mean ]myy[ and variance ]myy[+]theta[*]myy[**2.
	! endheader
	! Note  negbin(k,n*p,0)=
	! bin(k,n*p).
	! endnote
	! Note Sorry for the parameter inconsistency with rannegbin().
	! endnote
	! endsection
 
 
 
	double precision gamma,arg,pk,ak,amyy,kert,loge
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	arg=j_v(j_o(iob)%i(io+2))
	amyy=j_v(j_o(iob)%i(io+3))
	ak=j_v(j_o(iob)%i(io+4))
 
 
	if(arg.lt.j_0.or.amyy.lt.j_0.or.ak.lt.j_0)then
 
		write(6,*)'*negbin, arguments should be >=0, >0 and >=0  ',arg,amyy,ak
		j_err=.true.
		return
 
	endif !if(arg.lt.j_0.or.amyy.lt.j_0.or.ak.lt.j_0)  28863
	if(amyy.eq.j_0)then
		if(arg.eq.j_0)then
			j_v(iout)=j_1
		else
			j_v(iout)=j_0
		endif !if(arg.eq.j_0)  28871
	endif !if(amyy.eq.j_0)  28870
	if(ak.eq.j_0)then
		iarg=arg
		if(dble(iarg).ne.iarg)then
			write(6,*)'with Poisson distribution, i.e., theta=0 first argument should be integer ',arg
			j_err=.true.
			return
		endif !if(dble(iarg).ne.iarg)  28879
		! kert=j_1
		! do i=1,iarg
		! kert=kert+kert*i
		! enddo !do i=1,iarg
		! !	write(6,*)'<44',amyy**iarg*exp(-amyy)/kert
		kert=j_0
		do i=2,iarg
			kert=kert+log(dble(i))   !kert*i
		enddo !i=2,iarg  28890
		loge=iarg*log(amyy)-amyy-kert
		j_v(iout)=exp(loge) ! amyy**iarg*exp(-amyy)/kert
		!		write(6,*)'<55',j_v(iout)
 
	else !if(ak.eq.j_0)then
		pk=j_1/ak
		!	DLGAMA(X)
		!		loge=log(gamma(arg+pk))-log(gamma(pk))-log(gamma(arg+j_1))+pk*log(j_1/(j_1+ak*amyy))+&
		!		arg*log(ak*amyy/(j_1+ak*amyy))
		loge=dlgama(arg+pk)-dlgama(pk)-dlgama(arg+j_1)+pk*log(j_1/(j_1+ak*amyy))+&
			arg*log(ak*amyy/(j_1+ak*amyy))
		j_v(iout)=exp(loge)
 
 
		!		j_v(iout)=gamma(arg+pk)/(gamma(pk)*gamma(arg+1.d0))*(1.d0/(1.d0+ak*amyy))**pk*(ak*amyy/(1.d0+ak*amyy))**arg
		!		write(6,*)'<44 ',exp(loge),j_v(iout)
	endif !if(ak.eq.j_0)  28877
	!	90 io=io+narg+3
	return
end subroutine !subroutine negbin(iob,io)
 
subroutine density(iob,io)
	!Section density density() for any discrete or continues distribution
	! Make density for for random numbers either with a function
	!or histogram generated with classify.
	!	endheader
	!Option
	!Args&0-1&MATRIX & MATRIX generated with classify()
	!func& N|1& &codeoption defining the density. The x-varaible is $.
	!xrange&0|2& REAL& Range of x-values
	!discrete&-1|0& & Presence implies the the distribution is discrete
	!endoption
	!Note Actually the function generates a matrix having towo rows which
	!has values for the cumulative distribution function.
	!endnote
	!Note When defining the density function, the user need not care about
	!the scaling constant which makes the integral to integrate up to 1.
	!endnote
	!Ex densityex Example of distributions
	! ber=density(func->(1-p+(2*p-1)*$),xrange->(0,1),discrete->); Bernoully
	! bim=matrix(100)
	! bim=random(ber)
	! mean(bim);
	! p*(1-p);  !theoretical variance
	! var(bim);
	! pd=density(func->exp(-0.5*$*$),xrange->(-3,3))  !Normal distribution
 
	! ra=random(pd);
	! f=matrix(1000)
	! f=random(pd)
	! da=newdata(f,read->x)
	! stat(min->,max->)
	! cl=classify(x->x,xrange->);
	! fi=drawclass(cl,continue->fcont)
	! fi=drawclass(cl,area->,continue->fcont)
 
 
	! fi=draw(func->pdf(x),x->x,xrange->,append->,continue->fcont)
	! f=matrix(1000)
	! f=rann()
	! da=newdata(f,read->x)
	! stat(min->,max->)
	! cl=classify(x->x,xrange->,classes->20)
	! fi=drawclass(cl,histogram->,continue->fcont)
	! den=density(cl);
	! fi=drawline(den,continue->fcont)
	! endex
	! endsection
 
	double precision ::xx,scale,sum,dd,xmin,xmax,yvalue
	integer, dimension(:), pointer :: arg !arguments of the funct
	logical ::isdisc
	call  j_startfunction(iob,io,j_ipmatrix,narg,arg,iout)
	if(j_err)return
	linkfunc=j_codelink(iob,io,j_mfunc); if(j_err)return
	isdisc =j_linkoption(iob,io,j_mdiscrete,clear=.true.).ge.0
	ity=j_matreg
	if(isdisc)ity=j_matfreq
	if(linkfunc.gt.0)then
		call j_getoption(iob,io,j_mxrange,2,3,j_ipreal,.true.,noptarg,j_optarg0)
		if(j_err)return
		xmin=j_v(j_optarg0(1))
		xmax=j_v(j_optarg0(2))
		sum=j_0
		if(isdisc)then
			ncol=xmax-xmin+1
			iout=j_defmatrix(iout,' ',2_8,int8(ncol),ity)
			j_v(j_ivdollar)=xmin
			do i=1,ncol
				j_o(iout)%d(i)=j_v(j_ivdollar)
				sum=sum+j_codevalue(iob,linkfunc)
				if(j_err)return
				j_o(iout)%d(ncol+i)=sum
				j_v(j_ivdollar)=j_v(j_ivdollar)+j_1
			enddo !i=1,ncol  28981
			do i=1,ncol
				j_o(iout)%d(ncol+i)=j_o(iout)%d(ncol+i)/sum
			enddo !i=1,ncol  28988
 
		else
			ncol=101
			dd=(xmax-xmin)/ncol
			iout=j_defmatrix(iout,' ',2_8,int8(ncol),ity)
			j_o(iout)%d(1)=xmin
			j_o(iout)%d(2)=j_0
			j_v(j_ivdollar)=xmin
 
			do i=2,ncol
				j_o(iout)%d(i)=j_v(j_ivdollar)
				sum=sum+j_codevalue(iob,linkfunc)
				if(j_err)return
				j_o(iout)%d(ncol+i)=sum
				j_v(j_ivdollar)=j_v(j_ivdollar)+dd
			enddo !i=2,ncol  29000
			do i=1,ncol
				j_o(iout)%d(ncol+i)=j_o(iout)%d(ncol+i)/sum
			enddo !i=1,ncol  29007
		endif !if(isdisc)  28977
		return
 
	elseif(narg.eq.1)then
		if(j_o(arg(1))%i(4).ne.j_matclass)then
			call j_printname('**density: ',arg(1),' not a produced by classify')
			j_err=.true. ;return
			!goto 900
		endif !if(j_o(arg(1))%i(4).ne.j_matclass)  29014
		nrow_=j_o(arg(1))%i(1)
		ncol=j_o(arg(1))%i(2)
		xmin=j_o(arg(1))%d(nrow_*ncol+1)
		dd=j_o(arg(1))%d(nrow_*ncol+2)
		xx=xmin
		!			if(area)then
		!				scale=1./(j_o(iarg)%d(2*ncol)*dd)
		!			else !if(area)then
		!	scale=10j_0/j_o(iarg)%d(2*ncol)
		scale=j_1/j_o(arg(1))%d(2*ncol)
 
		iout=j_defmatrix(iout,' ',2_8,int8(ncol),ity)
		sum=j_0
		j_o(iout)%d(1)=xmin
		j_o(iout)%d(ncol+1)=j_0
		do i=1,ncol-1
			sum=sum+scale*j_getmatel(arg(1),2,i)
			!	if(i.eq.1)call j_gpputfigxy(ivfig,xx,yy)
			!call j_gpputfigxy(ivfig,xx,yy)
			xx=xx+dd
			j_o(iout)%d(i+1)=xx
			j_o(iout)%d(ncol+i+1)=sum
		enddo !i=1,ncol-1  29034
		return
		!		endif !if(area)then
	else
 
		write(6,*)'**density needs func-> or argument produced with classify()'
		j_err=.true.
		return
	endif !if(linkfunc.gt.0)  28971
 
end subroutine
 
subroutine bin(iob,io)  !binomial distribution
	! Section bin bin() Binomial probability
	! bin(]k[,]n[,]p[)=
	! The binomial probability that there will be ]k[ successes
	! in ]n[ independent trials when in a
	! single trial the probability of success is ]p[.
	! endsection
 
	double precision coef,p
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	k=j_v(j_o(iob)%i(io+2))
	n=j_v(j_o(iob)%i(io+3))
	p=j_v(j_o(iob)%i(io+4))
	k2=max(k,n-k)
	if(k2.ne.k)p=1.-p
	nk=n-k2
	if(k.lt.j_0)then
		write(6,*)'*bin, argument should be >=0, is ',arg
		j_err=.true.
		return
 
	endif !if(k.lt.j_0)  29070
	ios=1
	do i=k2+1,n
		ios=ios*i
	enddo !i=k2+1,n  29077
	in=1
	do i=1,nk
		in=in*i
	enddo !i=1,nk  29081
	coef=dble(ios)/dble(in)
	j_v(iout)=coef*p**k2*(1.d0-p)**nk!
	!	write(6,*)j_v(iout),coef*p**dble(k2)*(1.d0-p)**dble(nk)
	!	90 io=io+narg+3
	return
end subroutine !subroutine bin(iob,io)
 
 
!trigonometric and mathematica functions, same order as in modules
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
subroutine mod_(iob,io)
	integer,intent(in)::iob,io
 
	irg=j_o(iob)%i(io+2)  !narg is present
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+4)
	if(j_otype(irg).eq.j_ipreal)then
		j_v(iout)=mod(j_v(irg),j_v(irg2))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=mod(j_o(irg)%d,j_o(irg2)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)  29115
end subroutine !subroutine mod_(iob,io)
 
subroutine fraction_(iob,io)
	integer,intent(in)::iob,io
 
	irg=j_o(iob)%i(io+2)  !narg is present
	iout=j_o(iob)%i(io+3)
	if(j_otype(irg).eq.j_ipreal)then
		j_v(iout)=fraction(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=fraction(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)  29135
end subroutine !subroutine fraction_(iob,io)
 
 
 
 
 
 
 
 
 
