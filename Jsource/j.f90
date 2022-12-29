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
			call j_getchar(iv2,j_filename,le)
 
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
		write(6,1357)'jlp22 ',j_nfunctions_,j_noptions_,j_nobjecttypes_
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
			j_err=.false.
		endif !if(j_err)    143
 
		call o2_init()
		if(j_err)then
			write(6,*)'*o2* error initiliazing ',o2_title
			nerr=nerr+1
			j_err=.false.
		endif !if(j_err)    150
 
		call o3_init()
		if(j_err)then
			write(6,*)'*o3* error initiliazing ',o3_title
			nerr=nerr+1
			j_err=.false.
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
 
		do i_=1,j_nobjecttypes_
			j_lenobjecttypes(i_)=len_trim(j_objecttypes(i_))
		enddo !i_=1,j_nobjecttypes_    205
		it=j_nobjecttypes_
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
 
		do i_=1,j_nobjecttypes_
			j_lenobjecttypes(i_)=len_trim(j_objecttypes(i_))
		enddo !i_=1,j_nobjecttypes_    222
		it=j_nobjecttypes_
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
		!write(6,*)'j-namedv ',j_namedv
		!	write(6,*)'hep:',';incl('//jparf(1:len_trim(jparf))//')'
		call j_incl(';incl('//jparf(1:len_trim(jparf))//')' )   !j_command(";incl('j.par')")%%%
		!write(6,*)'<3633tas ',j_nul(0:1)
		if(j_err)then
			write(6,*)'*error in doing initilization commands in j.par, correct j.par and include it again'
			call j_errexit()
			j_err=.false.
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
 
	call j_isanyin(o1_objecttypes, o1_nobjecttypes, j_objecttypes, j_nobjecttypes_, 'o1_objecttypes', 'j_objecttypes')
	call j_isanyin(o2_objecttypes, o2_nobjecttypes, j_objecttypes, j_nobjecttypes_, 'o2_objecttypes', 'j_objecttypes' )
	call j_isanyin(o3_objecttypes, o3_nobjecttypes, j_objecttypes, j_nobjecttypes_, 'o3_objecttypes', 'j_objecttypes')
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
		endif !if(ifun.le.0)    315
	enddo !i=1,o1_nnamedfuncarg    312
 
	do i=1,o2_nnamedfuncarg
		le=len_trim(o2_namedfuncarg(i))
		ifun=j_isin(o2_namedfuncarg(i)(1:le),o2_functions,o2_nfunctions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o2mod element ', o2_namedfuncarg(i)(1:le), ' in namedfuncarg not a function'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedfuncarg(j_nfuncs1+ifun)=.true.
		endif !if(ifun.le.0)    326
	enddo !i=1,o2_nnamedfuncarg    323
	!write(6,*)'olo2'
	do i=1,o3_nnamedfuncarg
		le=len_trim(o3_namedfuncarg(i))
		ifun=j_isin(o3_namedfuncarg(i)(1:le),o3_functions,o3_nfunctions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o3mod element ', o3_namedfuncarg(i)(1:le), ' in namedfuncarg not a function'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedfuncarg(j_nfuncs2+ifun)=.true.
		endif !if(ifun.le.0)    337
	enddo !i=1,o3_nnamedfuncarg    334
	!write(6,*)'olo3'
	do i=1,j_nnamedoptarg
		le=len_trim(j_namedoptarg(i))
		ifun=j_isin(j_namedoptarg(i)(1:le),j_options,j_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module j _mod element ', j_namedoptarg(i)(1:le), ' in namedoptarg not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedoptarg(ifun)=.true.
		endif !if(ifun.le.0)    348
	enddo !i=1,j_nnamedoptarg    345
	!write(6,*)'olnam'
	do i=1,o1_nnamedoptarg
		le=len_trim(o1_namedoptarg(i))
		ifun=j_isin(o1_namedoptarg(i)(1:le),o1_options,o1_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o1 _mod element ', o1_namedoptarg(i)(1:le), ' in namedoptarg not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedoptarg(j_noptions_+ifun)=.true.
		endif !if(ifun.le.0)    359
	enddo !i=1,o1_nnamedoptarg    356
	!write(6,*)'olnamo1'
	do i=1,o2_nnamedoptarg
		le=len_trim(o2_namedoptarg(i))
		ifun=j_isin(o2_namedoptarg(i)(1:le),o2_options,o2_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o2_ mod element ', o2_namedoptarg(i)(1:le), ' in namedoptarg not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedoptarg(j_nopts1+ifun)=.true.
		endif !if(ifun.le.0)    370
	enddo !i=1,o2_nnamedoptarg    367
 
	do i=1,o3_nnamedoptarg
		le=len_trim(o3_namedoptarg(i))
		ifun=j_isin(o3_namedoptarg(i)(1:le),o3_options,o3_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o3_ mod element ', o3_namedoptarg(i)(1:le), ' in namedoptarg not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnamedoptarg(j_nopts2+ifun)=.true.
		endif !if(ifun.le.0)    381
	enddo !i=1,o3_nnamedoptarg    378
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
		endif !if(ifun.le.0)    393
	enddo !i=1,j_nnewvar    390
 
	do i=1,o1_nnewvar
		le=len_trim(o1_newvar(i))
		ifun=j_isin(o1_newvar(i)(1:le),o1_options,o1_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o1 _mod element ', o1_newvar(i)(1:le), ' in newvar not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnewvar(j_noptions_+ifun)=.true.
		endif !if(ifun.le.0)    404
	enddo !i=1,o1_nnewvar    401
 
	do i=1,o2_nnewvar
		le=len_trim(o2_newvar(i))
		ifun=j_isin(o2_newvar(i)(1:le),o2_options,o2_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o2 _ mod element ', o2_newvar(i)(1:le), ' in newvar not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnewvar(j_nopts1+ifun)=.true.
		endif !if(ifun.le.0)    415
	enddo !i=1,o2_nnewvar    412
 
	do i=1,o3_nnewvar
		le=len_trim(o3_newvar(i))
		ifun=j_isin(o3_newvar(i)(1:le),o3_options,o3_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o3_ mod element ', o3_newvar(i)(1:le), ' in newvar not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_isnewvar(j_nopts2+ifun)=.true.
		endif !if(ifun.le.0)    426
	enddo !i=1,o3_nnewvar    423
	!write(6,*)'olnew'
	!codeoptions
 
	do i=1,j_ncodeoptions_
 
		j_codeoption_(j_codeoptions(i))=.true.
 
	enddo !i=1,j_ncodeoptions_    436
 
	!write(6,*)'olcode'
	do i=1,o1_ncodeoptions,1
		le=len_trim(o1_codeoptions(i))
		ifun=j_isin(o1_codeoptions(i)(1:le),o1_options,o1_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o1 _mod element ', o1_codeoptions(i)(1:le), ' in codeoptions not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_codeoption_(j_noptions+ifun)=.true.
		endif !if(ifun.le.0)    446
	enddo !i=1,o1_ncodeoptions,1    443
 
	!write(6,*)'olcode1'
	do i=1,o2_ncodeoptions,1
		le=len_trim(o2_codeoptions(i))
		ifun=j_isin(o2_codeoptions(i)(1:le),o2_options,o2_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o2 _mod element ', o2_codeoptions(i)(1:le), ' in codeoptions not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_codeoption_(j_nopts1+ifun)=.true.
		endif !if(ifun.le.0)    458
	enddo !i=1,o2_ncodeoptions,1    455
 
	!write(6,*)'olcode2'
 
	do i=1,o3_ncodeoptions,1
		le=len_trim(o3_codeoptions(i))
		ifun=j_isin(o3_codeoptions(i)(1:le),o3_options,o3_noptions)
		if(ifun.le.0)then
			write(6,*)'*j* in module o3 _mod element ', o3_codeoptions(i)(1:le), ' in codeoptions not a option'
			j_err=.true.
		else !if(ifun.le.0)then
			j_codeoption_(j_nopts2+ifun)=.true.
		endif !if(ifun.le.0)    471
	enddo !i=1,o3_ncodeoptions,1    468
	!write(6,*)'olcode3'
 
	if(j_err) then
		write(6,*)'*j* transformations cannot be interpreted, close the program with end command'
		j_err = .false.
		!return
	else !if(j_err) then
		!	j_checkedo = .true.  ???
	endif !if(j_err)    480
 
	allocate(j_optarg2(1:300))
	!write(6,*)'allcate',allocated(j_optarg2)
	j_mxconstantv=500  ! %%constant max number of constants this can be increased
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
		endif !if(.not.jpar)    503
 
	else
 
		lep=5
		inquire(file='j.par',exist=jpar)
		if(jpar)jparf='j.par'
		!	write(6,*)'<333',jpar
	endif !if(lep.gt.0)    501
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
			endif !if(j_mxnamedv.lt.200)    525
		endif !if(le.gt.1.and.j_cline(1:1).eq.'*')    523
 
	end if !if(jpar)    517
	write(6,*)' '
	write(6,*)'max named objects  ',j_mxnamedv
	write(6,*)' '
	17 j_nv=j_mxnamedv+j_mxtemporalv
	j_mxv=j_nv+j_mxconstantv
 
 
	allocate(j_v(1:j_mxv))  !
	allocate(j_o(1:j_nv))
 
	do iv=j_mxnamedv+j_mxtemporalv0+1,j_mxnamedv+j_mxtemporalv !%dervative
		allocate(j_o(iv)%d(1:j_mxder0))
	enddo !iv=j_mxnamedv+j_mxtemporalv0+1,j_mxnamedv+j_mxtemporalv    543
	j_mxder=j_mxder0
 
	allocate(j_otype(0:j_mxv))
	!allocate(j_otitle(1:j_mxnamedv))
	allocate(j_iob(1:j_nv))   !possibly j_maxnamedv would be enough
	allocate(j_io(1:j_nv))   !possibly j_maxnamedv would be enough
	!	allocate(j_locked(1:j_nv))   !possibly j_maxnamedv would be enough
	!write(6,*)'otas'
	j_v=j_0 ;j_otype=j_ipreal
	j_otype(0)=0
	j_iob=0;j_io=0
 
	allocate( j_o(j_ivnames)%i(0:j_mxnamedv+1),j_o(j_ivnames)%ch(1:lename_*j_mxnamedv))
	!	j_temporalbas=j_mxnamedv+j_mxtemporalv0
	j_o(j_ivnames)%i(0)=0;j_o(j_ivnames)%i(1)=1
	! endif !if(istcmd.lt.lencmd) then
	j_nutf8=6; allocate(j_utf8(1:6));j_utf8=(/char(228), char(229),char(246),char(196),char(214),char(197)/)
	!   'ä','å','ö','Ä','Ö','Å'/)
	!	write(6,*)'>45/',j_utf8
	allocate(j_ocode(1:6));j_ocode=(/char(132),char(134),char(148),char(142),char(153),char(143)/)
 
 
	!j_ivstartedjlp=j_ivzero
	j_namedv=1
	j_otype(j_ivnames)=j_iptext
	j_err=.false.
	!the lines ended with !!! are not put tpo the Latex code
	!Section pref Predefined objects
	! The following objects are generated during the initilaization.
	!endheader
	!
	!Tabular 2 25mm 45mm 70mm
	call j_puttext(j_ivnames,'Names') !!!
	!write(6,*)'hep',j_err
	! Names& Text& Text object containg the names of named objects
 
	call j_getobject(0,'Pi',j_ipreal,ivout_)   !!!
	!	write(6,*)'<44	j_namedv=1',	j_namedv
 
	j_v(j_ivpi)=j_pi  !3.14159265358979323  !!!
 
	! Pi&REAL&The value of Pi (=3.1415926535897931)
 
	call j_getobject(0,'$0',j_ipreal,ivout_)  !!!
 
 
 
	call j_getobject(0,'$1',j_ipreal,ivout_) !!!
	j_v(j_ivone)=j_1!!!
 
	call j_getobject(0,'Inf',j_ipreal,ivout_)  !!!
 
	j_v(j_ivinf)=j_inf  !10.d300
 
	call j_getobject(0,'Tolast',j_ipreal,ivout_) ! ivtempdata)	 !!!
	j_v(ivout_)=j_ninf  !-10.d300
 
	call j_getobject(0,'Maxnamed',j_ipreal,ivout_) !!!
	j_v(ivout_)=j_mxnamedv !!!
 
 
 
 
	call j_getobject(0,'$Recursion',j_ipreal,ivout_ )! ivdebug) !!!
	j_v(j_ivrecursion)=0 !!!
	!	j_recursion=0 doen already
	!write(6,*)'hep2',hep2
 
	call j_deftrans(0,'$Cursor$',ivout_,1000,0,0,iii,iii,iii,0) ! ivcursor !!!
	! $Cursor$& TRANS & The transformation object used to run sit> prompt
 
	call j_deftrans(0,'$Cursor2$',ivout_,200,0,0,iii,iii,iii,0) !ivcursor2 !!!
	! $Cursor2$& TRANS & Another transformation object used to run >
	!sit> prompt
	!	$Val
	call j_deftrans(0,'$Val$',ivot_,200,0,0,iii,iii,iii,0) !ivval !!!
	! Val&TRANS& Transformation object used to extract values of mathematical statements, used,
	! e.g., in input programming.
	!	 call j_deflistobject(0,'$Data$',ivout_,nres=1) ! ivtempdata)
 
	call j_getobject(0,'Round',j_ipreal,ivout_) !!!
	!Round &REAL& jlp(): The current round through treatment units in jlp() function.
	! Can be used to define stopping criterion.
 
	!Change&REAL& jlp(): The change of objective in jlp()  in one round before finding feasible and thereafter
	!the change in 10 rounds.
	call j_getobject(0,'Change%',j_ipreal,ivout_) !; j_v(ivchangep)=0 !!!
 
	call j_getobject(0,'Imp',j_ipreal,ivout_)  !!!
	!Imp&REAL& jlp(): The number of improvements obtained from schedules outside the current active
	! set when updating the active set.
 
	! $Data$ & List& Default data set name for a new data set created by data()-function.
 
	!call j_deflistobject(0,'LastData',ivout_,nres=1) ! ivlastdata) !!
	call j_getobject(0,'LastData',j_ipreal,ivout_)
 
	call j_getobject(0,'Accepted',j_ipreal,ivout_) !!!
 
 
	call j_getobject(0,'Active%',j_ipreal,ivout_)  ! ivobsdef) !!!
	! Obs& REAL & The default name of variable obtaining the the number of
	! observation in a data set. Given in the data() function. Newdata ?
	!Maxnamed &REAL& The maximum number of named objects. Determined via j.par in
	! initilaization. default is 5000.
 
	call j_getobject(0,'Record',j_ipreal,ivout_) ! ivrecord) !!!
	! Record& REAL & The name of variable obtaining the the number of
	! record when reading data in data() function. Has the same value as Obs variabel
	! if no records are rejected.
 
	call j_getobject(0,'Subrecord',j_ipreal,ivout_) ! ivsubrecord) !!!
	! Subecord& REAL & The name of variable obtaining the the number of
	! record when reading subdata in data() function. Has the same value as obs variable
	! if no records are rejected.
 
	call j_getobject(0,'Duplicate',j_ipreal,ivout_) ! ivduplicate) !!½!
	!Duplicate& REAL& A special variable used in data() function when duplicating observations
 
	! LastaData &List&	A list object referring to the last data set made, used as default data set.
 
	! $Buffer&Char& A special character object used by the write() function.
	call j_deftext(0,'$Input0$',20,1500,ivout_) ! ivinput0) !!!
 
	! $Input$& Text & Text object used for original input line.
	call j_deftext(0,'$Input1$',20,1400,ivout_) ! ivinput1) !!!
	!write(6,*)'$Input1$',j_o(ivout_)%i(0),size(j_o(ivout_)%i),ubound(j_o(ivout_)%i,dim=1)
	!and endcommnets are removed
	!1$Input1$ &Text& Text object for input line after removing blanks and comments.
	call j_deftext(0,'$Input2$',20,1400,ivout_) ! !!!
	call j_defchar(0,'bis',ivout) !define a character constant binary
 
	!Data &List&  List object used to indicate current data setsDat
 
	call j_defchar(0,'b',ivout) !define a character constant binary j_ivb
 
	call j_defchar(0,'B',ivout) !define a character constant  j_ivb2 double prec
 
	call j_defchar(0,'di',ivout) !define a character constant  j_ivdi !intel
 
	call j_defchar(0,'DI',ivout) !define a character constant  j_ivdi2 !dbouble
 
	call j_defchar(0,'dg',ivout) !define a character constant   j_ivdg gfortran
 
	call j_defchar(0,'DG',ivout) !define a character constant j_ivdg2
 
	call j_defchar(0,'bn',ivout) !define a character constant
 
	call j_defchar(0,'BN',ivout) !define a character constant
 
 
	call j_getobject(0,'CHAR',j_ipreal,ivout_) !!!
	j_v(ivout_)=j_ipchar
 
	call j_getobject(0,'LIST2',j_ipreal,ivout_) !!!
	j_v(ivout_)=j_iplist2
 
	call j_getobject(0,'MATRIX',j_ipreal,ivout_) !!!
	j_v(ivout_)=j_ipmatrix
 
	call j_getobject(0,'TRANS',j_ipreal,ivout_) !!!
	j_v(ivout_)=j_iptrans
 
	call j_getobject(0,'LIST',j_ipreal,ivout_) !!!
	j_v(ivout_)=j_iplist
 
	call j_getobject(0,'TXT',j_ipreal,ivout_) !!!
	j_v(ivout_)=j_iptxt
 
	call j_getobject(0,'DATA',j_ipreal,ivout_) !!!
	j_v(ivout_)=j_ipdata
 
	call j_getobject(0,'FIG',j_ipreal,ivout_) !!
	j_v(ivout_)=j_ipfigure
 
	call j_getobject(0,'REGR',j_ipreal,ivout_) !!!
	j_v(ivout_)=j_ipregr
 
	call j_getobject(0,'BITMATRIX',j_ipreal,ivout_) !!
	j_v(ivout_)=j_ipbitmatrix
 
	call j_getobject(0,'REAL',j_ipreal,ivout_) !!
	j_v(ivout_)=j_ipreal
	! set style line 1 lt rgb "red" lw 3
	! set style line 2 lt rgb "orange" lw 2
	! set style line 3 lt rgb "yellow" lw 3
	! set style line 4 lt rgb "green" lw 2
	! set style line 5 lt rgb "cyan" lw 3
	! set style line 6 lt rgb "blue" lw 2
	! set style line 7 lt rgb "violet" lw 3
	!	write(6,*)'hep2',j_err
	ivout=j_defmatrix(0,'Fakematrix',1,1,j_matreg)
	!write(6,*)'hep3',j_err
	j_otype(j_ivfakematrix)=j_ipreal  !otherwise defmatrix faisl as fakematrix is llocked
 
 
	call j_getobject(0,'Black',j_ipreal,ivout_)
	j_v(j_ivblack)=j_1
 
	call j_getobject(0,'Red',j_ipreal,ivout_)
	j_v(j_ivred)=2.d0
 
	call j_getobject(0,'Green',j_ipreal,ivout_)
	j_v(j_ivgreen)=3.d0
 
	call j_getobject(0,'Blue',j_ipreal,ivout_)
	j_v(j_ivblue)=4.d0
 
	call j_getobject(0,'Cyan',j_ipreal,ivout_)
	j_v(j_ivcyan)=5.d0
 
	call j_getobject(0,'Violet',j_ipreal,ivout_)
	j_v(j_ivviolet)=6.d0
 
	call j_getobject(0,'Yellow',j_ipreal,ivout_)
	j_v(j_ivyellow)=7.d0
 
	call j_getobject(0,'Orange',j_ipreal,ivout_)
	j_v(j_ivorange)=8.d0
 
 
	! $&REAL& Object name used to indicate console and '*' format in reading and writing
	! commands.
	call j_getobject(0,'Maxlines',j_ipreal,ivout_)! ivxstar) !!!
	j_v(j_ivmaxlines)=20
 
	!x#&REAL& Variable used when drawing functions.
	call j_getobject(0,'Selected',j_ipreal,ivout_) ! ivselected) !!!
	!Selected& REAL &Variable used to indicate the simulator selected in simulations
	call j_getobject(0,'Printinput',j_ipreal,ivout_) ! ivprintinput) !!!
	!Printinput& REAL& Variable used to specify how input lines are printed. Not properly used.
	j_v(j_ivprintinput)=2.d0 !!!
	call j_getobject(0,'Printoutput',j_ipreal,ivout_) ! ivprintoutput) !!!
	!Prinoutpu& REAL& Variable used to indicate how much output is printed. Not properly used.
	j_v(j_ivprintoutput)=2.d0 !!!
 
	call j_getobject(0,'$Buffer',j_ipreal,ivout_) ! ivbuffer) !!!
 
	! $Input2$Text object for input line after interpreting “-sequencies.$
	call j_getobject(0,'Debug',j_ipreal,ivout_ )! ivdebug) !!!
 
	!write(6,*)'ivout_,j_ivdebug',ivout_,j_ivdebug
 
 
	! $Debug&REAL& Variable used to put debugging mode on.
 
 
	call j_getobject(0,'Regf',j_ipreal,ivout_ )! ivdebug) !!!free
 
	call j_getobject(0,'Resid',j_ipreal,ivout_)  !!!
 
	call j_getobject(0,'Debugconsole',j_ipreal,ivout_)  !!!  !unnecessar
	j_v(j_ivdebugconsole)=j_0 !!!
 
 
 
 
	! Accepted &REAL& The number of accepted observations in functions using data sets.
 
	call j_getobject(0,'Arg',j_ipreal,ivout_) !  ivarg) !!!
	! Arg& REAL & The default argument name when using transformation object as a function.
 
 
	call j_getobject(0,'Continue',j_ipreal,ivout_) !!!
	! Continue &REAL& If Continue has nonzero value then the control does not return to the
	! sit> prompt when an error occurs, but computation proceed from the first ;incl() file.
	! Used in the manual examples to demonstrate errors.
 
 
	call j_getobject(0,'Err',j_ipreal,ivout_) !!!
	! Err&REAL& If Continue prevents the control from returning to sit> prompt
	! this variable tells whether an error has occured.
 
 
	call j_getobject(0,'Result',j_ipreal,ivout_)  !!!
 
 
	call j_getobject(0,'Data',j_ipreal,ivout_)  ! ivcurrentdata) !!
 
	call j_getobject(0,'$',j_ipreal,ivout_) ! ivdollar) !!!
 
	call j_getobject(0,'Printresult',j_ipreal,ivout_) ! ivdollar) !!!
	j_v(j_ivprintresult)=3
	!Result& ? & The default name of output object.
	!   Type varies according to the function
	!endtabular
	!endsection
	call j_getobject(0,'$$',j_ipreal,ivout_) ! ivdollar) !!!
 
	call j_getobject(0,'Terminal',j_ipreal,ivout_) ! ivdollar) !!!
 
	call j_getobject(0,'Window',j_ipreal,ivout_) ! ivdollar) !!!
	call j_getobject(0,'All',j_ipreal,ivout_)
	j_v(ivout_)=10000.d0 !not zero in irnages of getelem
	call j_getobject(0,'Debugtrans',j_ipreal,ivout_)
	!write(6,*)'<666 ',ivout_
	j_v(ivout_)=j_0 !not zero in irnages of getelem
	call j_defchar(0,'bgaya',ivout_)
	!	write(6,*)'<77 ',ivout_
	call j_deftrans(0,'$Cursori$',ivout_,200,0,0,iii,iii,iii,0) !ivcursor2 !!!
 
	call j_getobject(0,'Fast%',j_ipreal,ivout_)
	if(j_ivfastp.ne.ivout_)then
		write(6,*)'predefined variables are mixed up,j_ivcfastp=',j_ivfastp,' but ivout_=',ivout_
		write(6,*)'j_namedv ',j_namedv
		j_err=.true.
		do i=1,j_namedv
			call j_getname(i)
			write(6,*)i,j_oname(1:j_loname)
 
		enddo !i=1,j_namedv    852
		j_stop=.true.
		return
	endif !if(j_ivfastp.ne.ivout_)    848
 
 
 
 
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
		11,12,13,14,15,16,17,18, & !special j_nfspec=8
		21,22,23,24, & !Objects j_nfobj=3
		31,32,33,34,35, & !transformations j_nftrans=3
		41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,&  !Loops and control j_nfloop=16
		61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79, & ! operations  j_nfoper=16
		81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100, & !arithmetic functions
		101,102,103,104,105,106,107,108,109,110,111,112,113, 114, &  ! arith continues j_nfarit=34
		121,122,123,124,125, & ! special artith j_nfarit2=5
		131,132,133,134,135,   &  ! probability distributions
		141,142,143,144,145,146,147, & ! random numbers
		151,152,153, &  !           interpolation
		161,162,163,164,165,166,167,168,169,  & !list functions
		171,172, &                   ! text object
		181,182,183,184,185,186,187,188, &  ! file handling
		191,192,193,194,195,196,197, &   ! io
		201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218, 219,&  ! matrices
		221,222,223,224,225,226,227,228,229,230,2301,2302, & !data functions
		231,232,233,234,235,236,237,238,239,240,241,242,243, & ! statistical functions
		251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271, & ! linear programming
		281,282,283,284,285,286, &    ! simulator
		291,292,293,294,295,296,& !    Figures
		301,302,303,304,305,306, &  !   splines , stem
		311,312,313,314,315,316,317, & ! bit functions
		321,322,323,324,325,326/
 
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
	endif !if(j_recursion.gt.j_mxrecursion)    928
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
		if(p4)then
			do j=1,j_nfunctions_
				write(6,*)j,j_functions(j),gotos(j),j_minarg_(j),j_maxarg_(j)
			enddo !j=1,j_nfunctions_    952
		endif !if(p4)    951
	endif !if(p)    947
 
	goto 2
 
	1 if(j_nopt.gt.0)call j_clearoption(iob,io)

	io=io+j_o(iob)%i(io+1)+3
 
 
 
 
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
		write(6,*)'recursion level set to ',j_recursion !j_v(j_ivrecursion)
		if(j_recursion.gt.j_1)write(6,*)' '
		!	write(6,*)'return from dotrans'
		return
		!		return
	endif !if(j_err)    970
 
 
	if(j_o(iob)%i(io).lt.0)then
 
		j_curline(j_recursion)=j_curline(j_recursion)-j_o(iob)%i(io)
		if(j_v(j_ivdebugtrans).ne.j_0)call j_debug(iob)
		io=io+1
	endif !if(j_o(iob)%i(io).lt.0)    990
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
		endif !if(j_o(iob)%i(io).ne.0)   1016
	endif !if(j_v(j_ivdollar2).eq.179.d0)   1011
	!	write(6,*)'io,',io,j_o(iob)%i(io:io+10)
	!	write(6,*)'io,',io,j_o(iob)%i(io)
	! write(6,*)'nollataa'
	! if(j_nopt.gt.0)j_linkoption(iob,io,j_curropt(1:j_nopt))=0
	! j_nopt=0
	!3001 continue
	!	write(6,*)'err before',j_err
 
12345	continue



	if(p)then
		write(6,*)'io',io,j_o(iob)%i(io)
		if(j_o(iob)%i(io).gt.0)write(6,*)j_functions(j_o(iob)%i(io)),gotos(j_o(iob)%i(io))
	endif !if(p)   1036
 
	goto (11,12,13,14,15,16,17,18, & !special j_nfspec=8
		21,22,23,24, & !Objects j_nfobj=3
		31,32,33,34,35, & !transformations j_nftrans=3
		41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,&  !Loops and control j_nfloop=16
		61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,& ! operations  j_nfoper=17
		81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100, & !arithmetic functions
		101,102,103,104,105,106,107,108,109,110,111,112,113, 114, &  ! arith continues j_nfarit=34
		121,122,123,124,125, & ! special artith j_nfarit2=5
		131,132,133,134,135,   &  ! probability distributions
		141,142,143,144,145,146,147, & ! random numbers
		151,152,153, &  !           interpolation
		161,162,163,164,165,166,167,168,169,  & !list functions
		171,172, &                   ! text object
		181,182,183,184,185,186,187,188, &  ! file handling
		191,192,193,194,195,196,197, &   ! io
		201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219, &  ! matrices 19
		221,222,223,224,225,226,227,228,229,230,2301,2302, & !data functions
		231,232,233,234,235,236,237,238,239,240,241,242,243, & ! statistical functions
		251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271, & ! linear programming
		281,282,283,284,285,286, &    ! simulator
		291,292,293,294,295,296,& !    Figures
		301,302,303,304,305,306, &  !   splines , stem
		311,312,313,314,315,316,317, & ! bit functions
		321,322,323,324,325,326)   j_o(iob)%i(io)
 
	j_recursion=j_recursion-1
 
	j_v(j_ivrecursion) = j_recursion
	!	write(6,*)'j1053 <recu>',j_v(j_ivrecursion)
	return
 
	!! The following 7 functions are obtained from j_interpret in nonstandard way
 
11	call setoption(iob,io)  ! %%option
	goto 2     !setoption updates io
 
12  call getelem(iob,io)  !get element or submatrix from matrix or list
	goto 1
 
13	call getelem(iob,io) !put element or submatrix to matrix or list this is actually setelem
	goto 1
 
14	call list2(iob,io) ! make list containg both named objects and numeric constants
	goto 1
 
15	io=io+1; call o1_funcs(iob,io);goto 1
16	io=io+1; call o2_funcs(iob,io);goto 1
17	io=io+1; call o3_funcs(iob,io);goto 1

18	call setcodeopt(iob,io)
	goto 2
 
	!! Objects
 
21	call type(iob,io)  !type of object
	goto 1
 
22	call delete_o(iob,io)   !delete object
	goto 1
 
23	call exist_o(iob,io)
	goto 1
 
24	call name(iob,io)
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
61	call BINOP(iob,io,14) ! HMULT(iob,io)
	goto 1
62 	call BINOP(iob,io,15) ! HDIV(iob,io)
	goto 1
 
63 call POWER(iob,io,1)  !IPOWER(iob,io)
	goto 1
 
64 call MULT(iob,io)
	goto 1
 
65	call DIV(iob,io)
	goto 1
 
66	call BINOP(iob,io,1)
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
 
161	call list(iob,io)
	goto 1
 
162	call merge(iob,io)
	goto 1
 
163	call difference(iob,io)
	goto 1
 
164	call index(iob,io)
	goto 1
 
165	call index_v(iob,io)
	goto 1
 
166	call len(iob,io)
	goto 1
 
167	call ilist(iob,io)
	goto 1
 
168	call putlist(iob,io)
	goto 1
169	call table(iob,io)
	goto 1
 
	!! Creating a text object
 
171	call text(iob,io)
	goto 1
172	call txt(iob,io)
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
 
213	call matrixstat(iob,io,2)  !sum(iob,io)
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
 
269	call xkf(iob,io)
	goto 1
 
270 call jlpz(iob,io)
	goto 1
 
271 call jlpcoef(iob,io)
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
	if(j_err)goto 1
	if(j_gpshow)call j_showfig(j_gpiout)
	goto 1
 
292	call draw(iob,io)
	if(j_err)goto 1
	if(j_gpshow)call j_showfig(j_gpiout)
	!if(p)write(6,*)'<555ioafterdraw ',io
	goto 1
 
293	call drawclass(iob,io)  ! plot results obtained with classify
	if(j_err)goto 1
	if(j_gpshow)call j_showfig(j_gpiout)
	goto 1
 
294	call drawline(iob,io)
	if(j_err)goto 1
	if(j_gpshow)call j_showfig(j_gpiout) !so drawline must not be recursive
 
	goto 1
 
295	call show(iob,io)
	goto 1
296	call plot3d(iob,io)
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
	!!! o function
 
 
 
 
 
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
 
	endif !if(j_nopt2.gt.j_maxopenopt2)   1836
	j_optionmoptio2(1,j_nopt2)=iopt
	j_optionmoptio2(2,j_nopt2)=j_o(iob)%i(io+4)
	j_optionlink2(j_nopt2)=io+5
	!	write(6,*)'<34,j-nopt2,j_optionmoptio2 ',j_nopt2,j_optionmoptio2(1,j_nopt2),j_optionmoptio2(2,j_nopt2)
	!	write(6,*)'code,io,iopt,io, ',io,iopt,j_o(iob)%i(io+4), 'link ',j_o(iob)%i(io+2)
 
	if(j_o(iob)%i(io+2).eq.0)then    !nothing to be computed
		io=io+5  !in this case j_o(iob)(j_linkopt2(option)-2).eq.0
	else !if(j_o(iob)%i(io+2).eq.0)then
		io=j_o(iob)%i(io+2)
	endif !if(j_o(iob)%i(io+2).eq.0)   1847
	j_optioniob2(j_nopt2)=iob
 
	!	write(6,*)'<38834',io
 
	!
	!setcode,option,jumptonow,output      io of func                                                      ioptjump,ioptjum
	!         +1      +2         +3      +4
	!                                  linkopt2
	! if linktowhencompuyting is zero dont jump anywhere
 
end subroutine !subroutine setcodeopt(iob,io)
 
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
	endif !if(j_nopt.ge.j_maxopenopt.or.(j_v(j_ivdollar).eq.455.D0.an   1871
	j_nopt=j_nopt+1
	j_optionmoptio(1,j_nopt)=iopt !option
	j_optionmoptio(2,j_nopt)=j_o(iob)%i(io+3+narg) ! io of the function
	j_optionlink(j_nopt)=io+2  !link to narg
	j_optioniob(j_nopt)=iob
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
	if(j_v(j_ivdollar).eq.456.D0)	write(6,*)'j_nopt,iob,io,iopt',j_nopt,iob,io,iopt,'iofunc',&
		j_optionmoptio(2,j_nopt),'link',j_optionlink(j_nopt)
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
	!It is currently not possible to mix ILIST range and contiguous range, so if ILIST
	! is needed for rows (columns), it must be used also for columns (rows). ILIST can be
	! specified using explicitly ilist() function or using {} construction.
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
	! Row and column ranges can be spcidie as follows.
	! \begin{itemize}
	! \item  ]M[(r,c) \tabto{5cm} Get or set single element.
	! \item  ]C[(r)  \tabto{5cm} Get or set single element in column vector.
	! \item  ]R[(c) \tabto{5cm} Get or set single element in column vector.
	! \item  ]M[(RANGES) \tabto{5cm} Get or set a submatrix, where RANGES
	!  can be. For a column vector, the column range need not to specified.
	! \begin{itemize}
	! \item  r1,-r2,c1,-c2
	! \item  r,c1,-c2  \tabto{5cm} part of row r
	! \item  r1,-r2,c	\tabto{5cm} part of column c
	!\item  r1,-r2,All  \tabto{5cm} All columns of the row range
	!\item  All,c1,-c2  \tabto{5cm} All rows of the column range
	!\item  {r1,....rm},{c1,...,cn}  \tabto{5cm} Given rows and columns
	!\item  {r1,....rm}  \tabto{5cm} Given rows for column vector
	!\item  il1,il2   \tabto{5cm} for matrix with several columns
	!\item  il1   \tabto{5cm} for column vector
	!\end{itemize}
	!\item  When r2= m, then -r2 can be replaced with ]Tolast[.
	!\item  When c2= n, then -c2 can be replaced with ]Tolast[.
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
	! endex
	! Note When giving range, the lower and upper limit can be equal.
	! endnote
	! endsection
 
	implicit none   !MUST if there are internal subroutines
	integer,intent(in)::iob,io
	integer, dimension(:), pointer :: arg !arguments of the funct
	integer, dimension(:), pointer :: ilist1,ilist2 !a
	integer,dimension(4)::inde,irange
	logical ::makenew
	logical ismatrix,islist,isvector,isrealfirst,isvectorout,diag,simple,isrow
	logical isregr
	logical ist,norange
	logical,dimension(2)::isili
	logical::p=.false.
	double precision::valu,sumcoef
	double precision,dimension(:),allocatable ::temp
	integer,dimension(:),allocatable ::list
	logical :: get,set,issum,useold,iscoef
	integer ::imat,ind,iout,narg1,narg
	integer ::iel
	integer*8 ::ncol,nrow,nel,nrow2,ncol2,nel2
	integer::iiv,is,ivkeep,ivlist
	integer*8::iba,ico,ie,ii,iro,iro0,i,i2,ibas,nre,ibasin
	integer:: ivmat,j,iar,nar2,iro2,mco,mro,nle,ionew,nargnew,ivcases,ice
	double precision::dapu
	logical :: isreal
	integer narg12,ir,nres,nar21,ind2
	p=j_v(j_ivdollar2).eq.56.d0.or.j_v(j_ivdollar2).eq.179.d0
 
 
	! if(p)write(6,*)' voimassaolevat optiot'
	! if(p)call j_printoptions()
	get=j_o(iob)%i(io).eq.2    ! if not get then set
	set=.not.get
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	narg1=narg-1  !number of index arguments
	norange=.true.
	if(p)then
		j=io+j_o(iob)%i(io+1)+3
		i=j_o(iob)%i(j)
		j_filename='return'
		if(i.gt.0)j_filename=j_functions(i)
 
		write(6,*)'*********************GETELEM,iob,io',iob,io,' new io ',&
			j,' new func ',j_filename(1:15),' funcindex ',i
 
		call j_where(iob,io)
		write(6,'(20i5)')j_o(iob)%i(1: j_o(iob)%i(0))
	endif !if(p)   2033
	if(narg1.gt.0)then
		arg=>j_o(iob)%i(io+3:io+1+narg)
		!	isrealfirst=j_otype(arg(1)).eq.j_ipreal
 
 
	endif !if(narg1.gt.0)   2045
	imat=j_o(iob)%i(io+2)
	ismatrix=j_otype(imat).eq.j_ipmatrix
	isreal=j_otype(imat).eq.j_ipreal
	isregr=j_otype(imat).eq.j_ipregr
 
	if(isregr)then
		if(set)then
			call j_getname(imat)
			write(6,*)j_oname(1:j_loname),' is REGR and it can not be as an output'
			j_err=.true. ;return
		endif !if(set)   2057
 
		nrow=j_o(imat)%i(0) !number of coefficients
		if(j_o(imat)%i(1).eq.j_ivone)then  !intercept
 
			j=1
			j_dapu=j_o(imat)%d(1)
			nrow=nrow-1
		else
			j_dapu=j_0
			j=0
 
		endif !if(j_o(imat)%i(1).eq.j_ivone)   2064
 
		if(narg1.gt.nrow)then
			call j_getname(imat)
			write(6,*)j_oname(1:j_loname),' had  ',nrow, ' regressors so you cannot give ',narg1, ' arguments'
			j_err=.true.;return
		endif !if(narg1.gt.nrow)   2075
		!write(6,*)'ncoef',nrow, 'reg ',j_o(imat)%i(1:nrow)
		!	write(6,*)j_o(imat)%d(1:nrow)
 
 
		do i=1,narg1
			j_dapu=j_dapu+j_o(imat)%d(i+j)*j_v(arg(i))
		enddo !i=1,narg1   2084
		do i=narg1+1,nrow
			j_dapu=j_dapu+j_o(imat)%d(i+j)*j_v(j_o(imat)%i(i+j))  !values they happaen to have
		enddo !i=narg1+1,nrow   2087
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_dapu
 
		return
		!	j_o(iout)%i(0)=ncoef
		!		j_o(iout)%i(1:ncoef)=regl(1:ncoef)
		!		j_o(iout)%d(1:ncoef)=rhs_(1:ncoef,ncoef1)
 
 
	endif !if(isregr)   2056
 
	call j_getoption(iob,io,j_msum,-1,1,j_ipreal,.false.,j,j_optarg0)
	if(j_err)return
	!	j_optionmoptio(1:2,1),'iob of option',j_optioniob(1), 'IIISSOPTIO',j
	issum=j.ge.0
	if(issum)then
		if(get)then
			write(6,*)'sum-> is not in the output'
			j_err=.true.
			return
 
		endif !if(get)   2106
		if(j.gt.0)then
			sumcoef=j_v(j_optarg0(1))
		else !if(j.gt.0)then
			sumcoef=j_1
		endif !if(j.gt.0)   2112
		if(p)write(6,*)'issum,sumcoef',issum,sumcoef,j_nopt,' io ',io
	endif !if(issum)   2105
 
 
	if(ismatrix.or.isreal)then
		if(ismatrix)then
			nrow=j_o(imat)%i(1)
			ncol=j_o(imat)%i(2)
			nel=j_o(imat)%i(3)
		else
			nrow=1
			ncol=1
			nel=1
		endif !if(ismatrix)   2122
 
		if(p)then
			call j_getname(imat,iout)
			write(6,*)'IS MATRIX,get',get,' imat ',imat,j_oname(1:j_loname),&
				' iout ',iout,' ',j_oname2(1:j_loname2),' ',j_objecttypes(j_otype(iout))
			write(6,*)'imat:',nrow,ncol
			call printmat(imat)
		endif !if(p)   2132
		if(j_otype(iout).eq.j_iplist)then
			call getsetlist()
			return
		endif !if(j_otype(iout).eq.j_iplist)   2139
 
		diag=j_linkoption(iob,io,j_mdiag,clear=.true.).ge.0
		if(diag.and.set.and.ismatrix)then
			if(narg1.eq.0.and.j_otype(iout).eq.j_ipreal)then
				iel=1
				do i=1,min(ncol,nrow)
					j_o(imat)%d(iel)=j_v(iout)
					iel=iel+ncol+1
 
				enddo !i=1,min(ncol,nrow)   2148
				return
			endif !if(narg1.eq.0.and.j_otype(iout).eq.j_ipreal)   2146
 
		endif !if(diag.and.set.and.ismatrix)   2145
 
		ist=j_linkoption(iob,io,j_mt,clear=.true.).ge.0
 
 
		if(narg1.eq.0)then
			! if(diag)then
			! call diags()
			! if(p)then
			! if(get)then
			! write(6,*)'GOT DIAG'
			! call printmat(iout)
			! else !if(get)then
			! write(6,*)'SET DIAG'
			! call printmat(imat)
 
			! endif !if(get)then
			! endif !if(p)then
			if(set)then
				irange(1)=1
				irange(2)=nrow
				irange(3)=1
				irange(4)=ncol
				norange=.false.
				isili=.false.
				simple=.false.
				goto 545
 
				return
			endif !if(set)   2174
 
			call j_getname(imat)
			write(6,*)'*',j_oname(1:j_loname), ' cannot produce anything'
			j_err=.true.
			return
		endif !if(narg1.eq.0)   2161
 
		isrealfirst=j_otype(arg(1)).eq.j_ipreal
		if(p)write(6,*)'<44,isreal',isrealfirst
		if(isrealfirst)then
			do i=2,narg1
				if(j_otype(arg(i)).ne.j_ipreal)goto 500
 
			enddo !i=2,narg1   2196
			if(ismatrix.or.isreal)then
				if(narg1.gt.4)then
					write(6,*)'illegal indices for matrix ',j_oname(1:j_loname)
					j_err=.true.;return
				endif !if(narg1.gt.4)   2201
				inde(1:narg1)=j_v(arg)
				if(p)write(6,*)'<888inde ',inde
			endif !if(ismatrix.or.isreal)   2200
		elseif(j_otype(arg(1)).eq.j_ipilist)then
			if(narg1.gt.2)then
				write(6,*)'if first argument is  ILIST then the second argument must be also'
				j_err=.true.
				return
			elseif(narg1.eq.2)then
				if(j_otype(arg(2)).ne.j_ipilist)goto 500
			else
				if(ncol.gt.1)then
					write(6,*) 'columns must be also specified for matrix ',nrow,ncol
					j_err=.true.
					return
				endif !if(ncol.gt.1)   2216
			endif !if(narg1.gt.2)   2209
 
 
		else
			call j_getname(imat)
			if(j_otype(imat).eq.j_ipreal)then
 
				write(6,*)j_oname(1:j_loname), ' is REAL and only legal indices shoud be 1 or 1,1'
				j_err=.true.
				return
 
			endif !if(j_otype(imat).eq.j_ipreal)   2226
			write(6,*)' all indeces of ',j_oname(1:j_loname),' must be either REAL or ILIST'
			j_err=.true.
			return
 
 
		endif !if(isrealfirst)   2195
		goto 545
500 		write(6,*)' all indeces must be either REAL or ILIST'
		j_err=.true.
		return
 
545		continue
		!	nrow=j_o(imat)%i(1)
		!	ncol=j_o(imat)%i(2)
		!	nel=j_o(imat)%i(3)
		isvector=nrow.eq.1.or.ncol.eq.1
		if(isvector)then
			isrow=nrow.eq.1
			!	write(6,*)'istranspose',ist
		else !if(isvector)then
			isrow=.false.
		endif !if(isvector)   2249
		if(norange)then
			irange=1
			call iranges()
		endif !if(norange)   2255
		!	write(6,*)'iranges',irange,diag
		nrow2=irange(2)-irange(1)+1
		ncol2=irange(4)-irange(3)+1
		nel2=nrow2*ncol2
		if(j_err)return
		if(p)write(6,*)'RANGES ',irange, ' nrow2 ',nrow2,'solve ncol2 ',ncol2
		if(get)then
			if(simple)then
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				if(isreal)then
					j_v(iout)=j_v(imat)
					return
				endif !if(isreal)   2268
				if(isvector)then
					j_v(iout)=j_o(imat)%d(irange(1))
				else !if(isvector)then
 
					j_v(iout)=j_o(imat)%d((irange(1)-1)*ncol+irange(3))
				endif !if(isvector)   2272
				return
			endif !if(simple)   2266
			call j_getoption(iob,io,j_mcoef,-1,1,j_ipreal,.true.,j,j_optarg0)
			if(j_err)return
			!	j_optionmoptio(1:2,1),'iob of option',j_optioniob(1), 'IIISSOPTIO',j
			iscoef=j.ge.0
			if(iscoef)then
 
				sumcoef=j_v(j_optarg0(1))
				if(sumcoef.eq.j_1)iscoef=.false.
				if(p)write(6,*)'iscoefsumcoef',iscoef,sumcoef
			endif !if(iscoef)   2284
			if(diag)then
				call diags()
				return
 
			endif !if(diag)   2290
 
			! if(diag)then
			! call diags()
			! goto 100
			! endif !if(diag)then
 
			isvectorout=nrow2.eq.1.or.ncol2.eq.1
			if(iout.eq.imat.and..not.isreal)then
				call getsame() !error if set
				goto 100
			endif !if(iout.eq.imat.and..not.isreal)   2302
			call getmatrix()
			if(p)then
 
				write(6,*)'got output iout '
				call printmat(iout)
			end if !if(p)   2307
			goto 100
		else !if(get)then
 
 
			!set
			if(j_otype(iout).eq.j_ipreal)then
				call setreal()
				if(p)then
					write(6,*)'set real into matrix, resulting matrix'
					call printmat(imat)
				endif !if(p)   2319
				return
			endif !if(j_otype(iout).eq.j_ipreal)   2317
 
 
			if(simple)then
				if(j_otype(iout).ne.j_ipreal)then
					call j_getname(iout)
					write(6,*)'only REAL can be put into MATRIX element, but ',j_oname(1:j_loname), &
						' is ',j_objecttypes(j_otype(iout))
					j_err=.true.;return
				endif !if(j_otype(iout).ne.j_ipreal)   2328
				if(issum)then
					if(isvector)then
						j_o(imat)%d(irange(1))=j_o(imat)%d(irange(1))+sumcoef*j_v(iout)
					else !if(isvector)then
						j_o(imat)%d((irange(1)-1)*ncol+irange(2))=&
							j_o(imat)%d((irange(1)-1)*ncol+irange(2))+sumcoef*j_v(iout)
					endif !if(isvector)   2335
 
 
				else !if(issum)then
					if(isvector)then
						j_o(imat)%d(irange(1))=j_v(iout)
					else !if(isvector)then
						j_o(imat)%d((irange(1)-1)*ncol+irange(2))=j_v(iout)
					endif !if(isvector)   2344
 
				endif !if(issum)   2334
				return
			endif !if(simple)   2327
			call setmatrix()
			if(p)then
				write(6,*)'set matrix into matrix, resulting matrix'
				call printmat(imat)
			endif !if(p)   2354
			return
		endif !if(get)   2265
	endif !if(ismatrix.or.isreal)   2121
 
 
 
	islist=j_otype(imat).eq.j_iplist
 
	if(narg1.eq.0)then
		!	arg=>j_o(iob)%i(io+3:io+1+narg)
 
 
		if(j_otype(imat).eq.j_ipregr)then
			call regr()
		else !if(j_otype(imat).eq.j_ipregr)then
 
			call j_printname(' ',imat,'() cannot provide anything')
			j_err=.true.
		endif !if(j_otype(imat).eq.j_ipregr)   2370
		return
	endif !if(narg1.eq.0)   2366
	if(islist)then
		ind=j_v(arg(1))
		if(p)write(6,*)'<3663>',ind,arg(1)
		if(ind.le.0.or.ind.gt.j_o(imat)%i(1))then
			call j_getname(imat)
			write(6,*)'illegal dimension ',ind,' for LIST ',j_oname(1:j_loname),' with ',j_o(imat)%i(1),' elements'
			j_err=.true.;return
		endif !if(ind.le.0.or.ind.gt.j_o(imat)%i(1))   2382
 
		if(narg1.ne.1)then
			ind2=-j_v(arg(2))
			if(ind2.le.0.or.ind2.gt.j_o(imat)%i(1).or.ind2.lt.ind)then
				call j_getname(imat)
				write(6,*)'illegal dimension ',ind2,' for LIST ',j_oname(1:j_loname),' with ',j_o(imat)%i(1),' elements'
				j_err=.true.;return
			endif !if(ind2.le.0.or.ind2.gt.j_o(imat)%i(1).or.ind2.lt.ind)   2390
			call  j_deflistobject(iout,' ',iout,list0=ind2-ind+1,list=j_o(imat)%i2(ind:ind2))
			return
		endif !if(narg1.ne.1)   2388
 
		iel=j_o(imat)%i2(ind)
		if(set)then
			call j_copy2(iout,iel)
		else !if(set)then
			if(j_otype(iel).eq.j_ipreal)then
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				j_v(iout)=j_v(iel)
			else !if(j_otype(iel).eq.j_ipreal)then
				ionew=io+narg+3
 
				nargnew=j_o(iob)%i(ionew+1)
				!iout-j_mxnamedv is the number of argument in the following function
				! starting from the end	thus it is
				if(p)write(6,*)'000',nargnew,'*',j_o(iob)%i(ionew:ionew+6)
				i=iout-j_mxnamedv
				!	ioutnewpos=j_o(iob)%i(ionew+2+nargnew)
				j_o(iob)%i(ionew+nargnew-i+2)=iel
				if(p)write(6,*)'111',nargnew,'*',j_o(iob)%i(ionew:ionew+6)
				!			call j_copy2(iel,iout)
			endif !if(j_otype(iel).eq.j_ipreal)   2403
		endif !if(set)   2400
		return
	endif !if(islist)   2379
 
 
 
	if(j_otype(imat).eq.j_iptrans)then
		!	write(6,*)'narg1',narg1
		! if(narg1.gt.1)then
 
 
		! if(mod(narg,2).ne.0)then
		! call j_getname(imat)
		! write(6,*)'transformation ',j_oname(1:j_loname),' has more argument values than arguments '
		! j_err=.true.
		! return
		! endif
		! narg12=narg/2-1
		! do ii=1,narg12
		! call j_copy2(arg(1+narg12+ii),arg(1+ii))
 
		! enddo
		! endif
 
		call trans()
		!	call j_copy2(arg(1),iout)
		return
	elseif(j_otype(imat).eq.j_ipdata)then !if(j_otype(imat).eq.j_iptrans)then
		if(.not.get.and.j_o(imat)%i(10).eq.0)then
			write(6,*)'dataobject() cannot be output without cases'
			j_err=.true.;return
		endif !if(.not.get.and.j_o(imat)%i(10).eq.0)   2447
		call data()
		return
	elseif(j_otype(imat).eq.j_ipregr)then !if(j_otype(imat).eq.j_iptrans)then
		call regr()
		return
	else !if(j_otype(imat).eq.j_iptrans)then
		if(get)then
			call j_printname('object ',imat,' cannot provide anything')
		else !if(get)then
			write(6,*)'illegal output'
		endif !if(get)   2457
		j_err=.true.
		return
 
	endif !if(j_otype(imat).eq.j_iptrans)   2425
 
 
	write(6,*)'input must be REAL, MATRIX or LIST'
 
 
	return
	99	write(6,*)'illegal dimensions'
	j_err=.true.
 
	return
	100 if(j_o(iout)%i(1).eq.1.and.j_o(iout)%i(2).eq.1)then
		dapu=j_o(iout)%d(1)
		call j_del(iout)
		j_v(iout)=dapu
 
	end if !100 if(j_o(iout)%i(1).eq.1.and.j_o(iout)%i(2).eq.1)   2476
 
 
 
	return
	contains
	subroutine printmat(imat)
		integer::imat
		call j_getname(imat)
		write(6,*)'matrix ',j_oname(1:j_loname)
		iba=0
		do j=1,j_o(imat)%i(1)
			write(6,'(10f4.0)')j_o(imat)%d(iba+1:iba+j_o(imat)%i(2))
			iba=iba+j_o(imat)%i(2)
		enddo !j=1,j_o(imat)%i(1)   2492
		return
	end subroutine !subroutine printmat(imat)
	subroutine iranges()
		irange(1:2)=1
		simple=.false.
		isili=.false.
		!		write(6,*)'irang ',inde
		!	if(any(inde(1:narg1).eq.0))goto 90
		select case (narg1)
		case(0) !select case (narg1)
		irange(2)=nrow
		case(1) !select case (narg1)
		if(j_otype(arg(1)).eq.j_ipilist)then
			isili(1)=.true.
			nle=j_o(arg(1))%i(1)
			irange(2)=nle
			ilist1=>j_o(arg(1))%i2(1:nle)
			if(p)write(6,*)ilist1,'&&',j_o(arg(1))%i2
			mro=maxval(ilist1)
			if(mro.gt.nel)goto 90
			return
		else !if(j_otype(arg(1)).eq.j_ipilist)then
			isili(1)=.false.
		endif !if(j_otype(arg(1)).eq.j_ipilist)   2508
		if(arg(1).eq.j_ivall)then
			irange(2)=nrow
			if(diag)return
 
		else
			irange(1)=inde(1)
			irange(2)=inde(1)
		endif !if(arg(1).eq.j_ivall)   2520
 
		if(inde(1).le.0.or.inde(1).gt.nel.or..not.isvector)goto 90
 
 
 
		!	if(ncol.gt.1)then
		!		irange(4)=ncol
		!	else !if(ncol.gt.1)then
		simple=.true.
		!	endif !if(ncol.gt.1)then
		case(2) !select case (narg1)
		if(j_otype(arg(1)).eq.j_ipilist)then
			isili(1)=.true.
			nle=j_o(arg(1))%i(1)
			irange(2)=nle
			ilist1=>j_o(arg(1))%i2(1:nle)
			if(p)write(6,*)ilist1,'&&',j_o(arg(1))%i2
			mro=maxval(ilist1)
			if(mro.gt.j_o(imat)%i(1))then
				write(6,*)'illegal rows ',ilist1
				goto 90
 
			endif !if(mro.gt.j_o(imat)%i(1))   2546
			if(j_otype(arg(2)).eq.j_ipilist)then
				isili(2)=.true.
				nle=j_o(arg(2))%i(1)
				irange(4)=nle
				ilist2=>j_o(arg(2))%i2(1:nle)
				if(p)write(6,*)ilist2,'&&',j_o(arg(2))%i2
				mco=maxval(ilist2)
 
			endif !if(j_otype(arg(2)).eq.j_ipilist)   2551
		else !if(j_otype(arg(1)).eq.j_ipilist)then
			if(p)write(6,*)'arg1',arg(1),j_ivall,' inde ',inde
			if(arg(1).eq.j_ivall)then
 
				irange(2)=nrow
				if(arg(2).eq.j_ivall)then
					irange(4)=ncol
					return
				endif !if(arg(2).eq.j_ivall)   2565
				if(p)write(6,*)'inde2',inde(2)
				if(inde(2).le.0.or.inde(2).gt.ncol)goto 90
				irange(3)=inde(2)
				irange(4)=inde(2)
				return
			endif !if(arg(1).eq.j_ivall)   2562
			if(p.and.(inde(1).le.0.or.inde(1).gt.nel))write(6,*)'inde1,nel',inde(1),nel
			if(inde(1).le.0.or.inde(1).gt.nel)goto 90
			if(p)write(6,*)'<6668>inde',inde,' irange ',irange
			irange(1)=inde(1)
			if(irange(1).gt.nrow)goto 90
			if(inde(2).lt.0)then
				irange(2)=-inde(2)
				if(arg(2).eq.j_ivtolast)irange(2)=nrow
				if(irange(2).gt.nrow.or.&
					irange(2).lt.irange(1))goto 90
 
				if(ncol.gt.1.and..not.diag)goto 90
 
				!irange(4)=ncol
			elseif(arg(2).eq.j_ivall)then
				irange(2)=irange(1)
				irange(4)=ncol
				return
			else !if(inde(2).lt.0)then
				irange(2)=irange(1)
				if(inde(2).gt.ncol)goto 90
				irange(3)=inde(2)
				irange(4)=inde(2)
				simple=.true.
			endif !if(inde(2).lt.0)   2580
			!endif
			! if(.not.isvector)then
			! irange(4)=ncol
			! endif !if(.not.isvector)then
		endif !if(j_otype(arg(1)).eq.j_ipilist)   2539
		case(3) !select case (narg1)
 
		if(arg(1).eq.j_ivall)then
			irange(2)=nrow
			if(inde(2).le.0.or.inde(2).gt.ncol)goto 90
			irange(3)=inde(2)
			irange(4)=-inde(3)
			if(irange(4).lt.irange(3).or.irange(4).gt.ncol)goto 90
			return
		endif !if(arg(1).eq.j_ivall)   2607
		irange(1)=inde(1)
		if(inde(2).lt.0)then
			irange(2)=-inde(2)
			if(arg(2).eq.j_ivtolast)irange(2)=nrow
			if(irange(2).gt.nel)goto 90
 
			if(arg(3).eq.j_ivall)then
				irange(4)=ncol
				return
			endif !if(arg(3).eq.j_ivall)   2621
			if(inde(3).lt.0.or.inde(3).gt.ncol)goto 90
			irange(3)=inde(3)
			irange(4)=inde(3)
		else !if(inde(2).lt.0)then
			if(inde(2).gt.ncol)goto 90
			irange(2)=inde(1)
			irange(3)=inde(2)
			if(inde(3).gt.0)goto 90
			irange(4)=-inde(3)
			if(arg(3).eq.j_ivtolast)irange(4)=ncol
			if(irange(4).gt.ncol)goto 90
		endif !if(inde(2).lt.0)   2616
		case(4) !select case (narg1)
		irange=abs(inde)
		if(arg(2).eq.j_ivtolast)irange(2)=nrow
		if(arg(4).eq.j_ivtolast)irange(4)=ncol
		if(irange(1).lt.0.or.irange(1).gt.nrow)goto 90
		if(irange(2).lt.0.or.irange(2).gt.nrow.or.irange(2).lt.irange(1))goto 90
		if(irange(3).lt.0.or.irange(3).gt.ncol)goto 90
		if(irange(4).lt.0.or.irange(4).gt.ncol.or.irange(4).lt.irange(3))goto 90
 
		end select !select case (narg1)
		return
	90	call j_getname(imat)
		if(ismatrix)then
			write(6,*)'illegal ranges ',inde(1:narg1),' for matrix ',&
				j_oname(1:j_loname),' with dimensions ',nrow,ncol
		else
			write(6,*)j_oname(1:j_loname),' is REAL, ranges ',inde(1:narg1),' are illegal, only (1) or (1,1) are allowed '
 
		endif !if(ismatrix)   2649
		j_err=.true.
	end subroutine !subroutine iranges()
 
	subroutine getmatrix()
		if(isreal)then
			if(irange(2).gt.1.or.irange(4).gt.1)then
				call j_getname(imat)
				write(6,*)j_oname(1:j_loname),' is REAL, ranges ',inde(1:narg1),' are illegal, only  only (1) or (1,1) allowed '
				j_err=.true.;return
			endif !if(irange(2).gt.1.or.irange(4).gt.1)   2661
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=j_v(imat)
			return
		endif !if(isreal)   2660
		useold=.false.
		if(j_otype(iout).eq.j_ipmatrix)then
			useold=j_o(iout)%i(3).eq.nel2
		endif !if(j_otype(iout).eq.j_ipmatrix)   2671
 
		if(isrow)then
			if(useold)then
				j_o(iout)%i(1)=1
				j_o(iout)%i(2)=max(nrow2,ncol2)
			else !if(useold)then
				j_i8=max(nrow2,ncol2)
				iout=j_defmatrix8(iout,' ',j_18,j_i8,j_matreg)
			endif !if(useold)   2676
		else !if(isrow)then
			if(useold)then
				j_o(iout)%i(1)=nrow2
				j_o(iout)%i(2)=ncol2
			else !if(useold)then
				if(p)write(6,*)'<555nrow2,ncol2',nrow2,ncol2
				iout=j_defmatrix8(iout,' ',nrow2,ncol2,j_matreg)
			endif !if(useold)   2684
		endif !if(isrow)   2675
		!	write(6,*)'getmatin',isvector,' p',p, 'irange ',irange,'ncol',ncol
		if(isvector)then
			if(isvectorout)then
				if(irange(2).gt.j_o(imat)%i(3).or.irange(4).gt.j_o(imat)%i(3))then
					call j_getname(imat)
					write(6,*)'cannot pick up to',nel2,' elements from vector ',&
						j_oname(1:j_loname),'	with length ',max(irange(2),irange(4))
					j_err=.true.;return
				endif !if(irange(2).gt.j_o(imat)%i(3).or.irange(4).gt.j_o(imat)%i   2695
 
				if(isili(1))then
					j_o(iout)%d=j_o(imat)%d(ilist1)
				else !if(isili(1))then
					j_o(iout)%d=j_o(imat)%d(max(irange(1),irange(3)):max(irange(2),irange(4)))
				endif !if(isili(1))   2702
			endif !if(isvectorout)   2694
			if(ist)then
				i2=j_o(iout)%i(2)
				j_o(iout)%i(2)=j_o(iout)%i(1)
				j_o(iout)%i(1)=i2
			endif !if(ist)   2708
		else !if(isvector)then
			ie=0
			!	write(6,*)'ira2',irange(1:2)
			if(irange(2).gt.j_o(imat)%i(1).or.irange(4).gt.j_o(imat)%i(2))then
				call j_getname(imat)
				write(6,*)'illegal ranges ',inde(1:narg1),' for matrix ',&
					j_oname(1:j_loname),' with dimensions ', j_o(imat)%i(1:2)
				j_err=.true.;return
			endif !if(irange(2).gt.j_o(imat)%i(1).or.irange(4).gt.j_o(imat)%i   2716
			do iro0=irange(1),irange(2)
				if(p)write(6,*)'<440',iro0
				iro=iro0
				if(isili(1))iro=ilist1(iro0)
				if(isili(2))then
					if(p)write(6,*)'<441',iro,ilist2,ncol2
					if(iscoef)then
						do ico=1,ncol2
							ie=ie+1
							j_o(iout)%d(ie)=sumcoef*j_o(imat)%d((iro-1)*ncol+ilist2(ico))
						enddo !ico=1,ncol2   2729
 
					else !if(iscoef)then
						do ico=1,ncol2
							ie=ie+1
							j_o(iout)%d(ie)=j_o(imat)%d((iro-1)*ncol+ilist2(ico))
						enddo !ico=1,ncol2   2735
					endif !if(iscoef)   2728
				else !if(isili(2))then
					if(iscoef)then
						do ico=irange(3),irange(4)
							if(p)write(6,*)'<44ico',ico,ie,ncol
							ie=ie+1
							j_o(iout)%d(ie)=sumcoef*j_o(imat)%d((iro-1)*ncol+ico)
						enddo !ico=irange(3),irange(4)   2742
 
					else !if(iscoef)then
						do ico=irange(3),irange(4)
							if(p)write(6,*)'<44ico',ico,ie,ncol
							!		if(ie.gt.j_o(iout)%i(3).or.(iro-1)*ncol+ico.gt.j_o(imat)%i(3))then
							!		write(6,*)'imat ',imat,' irange ',irange,', norow ',nrow,' ncol ',ncol,&
							!	' iro ',iro,' ico ',ico,' j_o(iout)%i(3) ',j_o(iout)%i(3),&
							!	 'j_o(imat)%i(3)',j_o(imat)%i(3)
							!	j_err=.true.
							!	return
							!	endif
							ie=ie+1
							j_o(iout)%d(ie)=j_o(imat)%d((iro-1)*ncol+ico)
						enddo !ico=irange(3),irange(4)   2749
					endif !if(iscoef)   2741
				endif !if(isili(2))   2726
			enddo !iro0=irange(1),irange(2)   2722
 
			if(p)write(6,*)'ist,isvectorout',ist,isvectorout
			if(ist.and.isvectorout)then
				i2=j_o(iout)%i(2)
				j_o(iout)%i(2)=j_o(iout)%i(1)
				j_o(iout)%i(1)=i2
			endif !if(ist.and.isvectorout)   2766
		endif !if(isvector)   2693
 
	end subroutine !subroutine getmatrix()
 
	subroutine setreal()
		!	write(6,*)'<setreal ',j_v(iout),issum,isvector,'isili ',isili,'ilist1 ',ilist1
		if(issum)then
			if(isvector)then
				if(isili(1))then
					j_o(imat)%d(ilist1)=j_o(imat)%d(ilist1)+sumcoef*j_v(iout)
				elseif(ismatrix)then !if(isili(1))then
					j_o(imat)%d(irange(1):irange(2))= &
						j_o(imat)%d(irange(1):irange(2))+sumcoef*j_v(iout)
				else
					j_v(imat)= j_v(imat)+sumcoef*j_v(iout)
 
				endif !if(isili(1))   2779
			else !if(isvector)then
				ie=0
				do iro0=irange(1),irange(2)
					iro=iro0
					if(isili(1))iro=ilist1(iro0)
					if(isili(2))then
						if(p)write(6,*)'<4477',iro,ilist2,ncol2
						do ico=1,ncol2
							ie=ie+1
							j_o(imat)%d((iro-1)*ncol+ilist2(ico))=&
								j_o(imat)%d((iro-1)*ncol+ilist2(ico))+sumcoef*j_v(iout)
						enddo !ico=1,ncol2   2795
					else !if(isili(2))then
						ibas=(iro-1)*ncol
						j_o(imat)%d(ibas+irange(3):ibas+irange(4))=&
							j_o(imat)%d(ibas+irange(3):ibas+irange(4))+sumcoef*j_v(iout)
					endif !if(isili(2))   2793
				enddo !iro0=irange(1),irange(2)   2790
			endif !if(isvector)   2778
		else !if(issum)then
			if(isvector)then
				if(isili(1))then
					j_o(imat)%d(ilist1)=j_v(iout)
				elseif(ismatrix)then !if(isili(1))then
					j_o(imat)%d(irange(1):irange(2))=j_v(iout)
				else
					j_v(imat)=j_v(iout)
				endif !if(isili(1))   2809
			else !if(isvector)then
				ie=0
				do iro0=irange(1),irange(2)
					iro=iro0
					if(isili(1))iro=ilist1(iro0)
					if(isili(2))then
						if(p)write(6,*)'<4477',iro,ilist2,ncol2
						do ico=1,ncol2
							ie=ie+1
							j_o(imat)%d((iro-1)*ncol+ilist2(ico))=j_v(iout)
						enddo !ico=1,ncol2   2823
					else !if(isili(2))then
						ibas=(iro-1)*ncol
						j_o(imat)%d(ibas+irange(3):ibas+irange(4))=j_v(iout)
					endif !if(isili(2))   2821
				enddo !iro0=irange(1),irange(2)   2818
			endif !if(isvector)   2808
		endif !if(issum)   2777
	end subroutine !subroutine setreal()
 
	subroutine setmatrix()
		if(nel2.ne.j_o(iout)%i(3))then
			write(6,*)'cannot put ',j_o(iout)%i(3),' elements into box of ',nel2,' elements'
			call j_getname(imat)
			write(6,*)'output matrix has dimensions ',j_o(imat)%i(1:2)
			write(6,*)'output ranges were in input ',inde(1:narg1),' translated into ',irange
			j_err=.true.;return
		endif !if(nel2.ne.j_o(iout)%i(3))   2837
		if(p)write(6,*)'issum ',issum,sumcoef,' isvector ',isvector
		if(issum)then
 
			if(isvector)then
				if(isili(1))then
					j_o(imat)%d(ilist1)=j_o(imat)%d(ilist1)+sumcoef*j_o(iout)%d
				else !if(isili(1))then
					j_o(imat)%d(irange(1):irange(2))=j_o(imat)%d(irange(1):irange(2))+sumcoef*j_o(iout)%d
				endif !if(isili(1))   2848
			else !if(isvector)then
				ie=0
				if(.not.isili(1))ibas=(irange(1)-1)*ncol
				do iro0=irange(1),irange(2)
					if(isili(1))ibas=(ilist1(iro0)-1)*ncol
 
					if(isili(2))then
						if(p)write(6,*)'<4488',iro,ilist2,ncol2
						do ico=1,ncol2
							ie=ie+1
							j_o(imat)%d(ibas+ilist2(ico))=j_o(imat)%d(ibas+ilist2(ico))+sumcoef*j_o(iout)%d(ie)
						enddo !ico=1,ncol2   2861
 
					else !if(isili(2))then
 
						j_o(imat)%d(ibas+irange(3):ibas+irange(4))=&
							j_o(imat)%d(ibas+irange(3):ibas+irange(4))+sumcoef*j_o(iout)%d(ie+1:ie+ncol2)
						ibas=ibas+ncol
						ie=ie+ncol2
					endif !if(isili(2))   2859
				enddo !iro0=irange(1),irange(2)   2856
			endif !if(isvector)   2847
 
		else !if(issum)then
			!	if(p)write(6,*)'isvector,isili(1)',isvector,isili(1)
			if(isvector)then
				if(isili(1))then
					j_o(imat)%d(ilist1)=j_o(iout)%d
				else !if(isili(1))then
					j_o(imat)%d(irange(1):irange(2))=j_o(iout)%d
				endif !if(isili(1))   2879
			else !if(isvector)then
				ie=0
				if(.not.isili(1))ibas=(irange(1)-1)*ncol
				if(p)write(6,*)'ncol,ncol2 ',ncol,ncol2
				if(diag)then
					ie=0
					do iro0=irange(1),irange(2)
						ie=ie+1
						if(isili(1))ibas=(ilist1(iro0)-1)*ncol
 
						if(isili(2))then
							if(p)write(6,*)'<4488',iro,ilist2,ncol2
							do ico=1,ncol2
								ie=ie+1
								j_o(imat)%d(ibas+ilist2(ico))=j_o(iout)%d(ie)
							enddo !ico=1,ncol2   2896
 
						else !if(isili(2))then
 
							j_o(imat)%d(ibas+iro0)=j_o(iout)%d(ie)
							ibas=ibas+ncol
							!	ie=ie+ncol2
						endif !if(isili(2))   2894
					enddo !iro0=irange(1),irange(2)   2890
				else
					do iro0=irange(1),irange(2)
						if(isili(1))ibas=(ilist1(iro0)-1)*ncol
 
						if(isili(2))then
							if(p)write(6,*)'<4488',iro,ilist2,ncol2
							do ico=1,ncol2
								ie=ie+1
								j_o(imat)%d(ibas+ilist2(ico))=j_o(iout)%d(ie)
							enddo !ico=1,ncol2   2914
 
						else !if(isili(2))then
 
							j_o(imat)%d(ibas+irange(3):ibas+irange(4))=j_o(iout)%d(ie+1:ie+ncol2)
							ibas=ibas+ncol
							ie=ie+ncol2
						endif !if(isili(2))   2912
					enddo !iro0=irange(1),irange(2)   2909
				endif !if(diag)   2888
			endif !if(isvector)   2878
		endif !if(issum)   2845
 
	end subroutine !subroutine setmatrix()
 
	subroutine trans()
		!	arg=>j_o(iob)%i(io+3:io+1+narg)
		!bypass tr(a
		nar2=narg/2.
		!		write(6,*)'<44',nar2,arg(1),iout,j_v(arg(1))
		if(2*nar2.ne.narg)then
			write(6,*)'illegal arguments, should be output,input, inputvalue,...'
			j_err=.true.;return
		endif !if(2*nar2.ne.narg)   2937
		nar21=nar2-1
		do iar=2,nar2
			call j_getname(arg(iar))
 
			call j_copy2(arg(nar2+iar-1),arg(iar ))
			!				write(6,*)'<774 ',arg(iar),j_oname(1:j_loname),j_v(arg(iar))
		enddo !iar=2,nar2   2942
 
		!	write(6,*)'<66',imat,iout,j_v(iout)
		nres=iout-j_mxnamedv-1
		!		write(6,*)'<66',imat,iout,j_v(iout),nres
		if(nres.gt.0)then
			do ir=1,nres
				if(j_otype(j_mxnamedv+ir).eq.j_ipreal)then
					j_temporals(ir)=j_v(j_mxnamedv+ir)
				elseif(j_otype(j_mxnamedv+ir).eq.j_ipmatrix)then
					write(6,*)'*ask J. Lsappi to add MATRIX here'
					j_err=.true.;return
				else
					write(6,*)'illegal type of temporal'
					j_err=.true.;return
 
				endif !if(j_otype(j_mxnamedv+ir).eq.j_ipreal)   2954
			enddo !ir=1,nres   2953
			call dotrans(imat,1)
			j_v(j_mxnamedv+1:j_mxnamedv+nres)=j_temporals(1:nres)
		else
			call dotrans(imat,1)
 
		endif !if(nres.gt.0)   2952
 
		if(j_err)return
		!		write(6,*)'<77',iout,j_v(iout),arg(1),j_v(arg(1))
		call j_copy2(arg(1),iout)
		!	write(6,*)'<88',j_v(iout)
		return
 
	end subroutine !subroutine trans()
 
	subroutine data()
 
		ivmat=j_o(imat)%i(1)
		ivkeep=j_o(imat)%i(2)
		ivcases=j_o(imat)%i(10)
		if(ivcases.ne.0)then
			i=j_inlistobject(arg(1),ivkeep)
			if(i.gt.0)goto 469
			if(narg1.ne.2)then
				write(6,*)'*if accessing element from data with cases, there must be two arguments'
				j_err=.true.
				return
			endif !if(narg1.ne.2)   2988
			ice=j_inlistobject(arg(1),ivcases)
			if(ice.le.0)then
				ice=j_v(arg(1))
				if(ice.le.0.or.ice.gt.j_o(ivmat)%i(1))then
					call j_getname(imat)
					write(6,*)'*row index ',ice,' of ',j_oname(1:j_loname),' is not in range 1,',j_o(ivmat)%i(1)
					j_err=.true.;return
				endif !if(ice.le.0.or.ice.gt.j_o(ivmat)%i(1))   2996
 
			endif !if(ice.le.0)   2994
			iro=j_inlistobject(arg(2),ivkeep)
			if(iro.le.0)then
				iro=j_v(arg(2))
				if(iro.le.0.or.iro.gt.j_o(ivmat)%i(2))then
					call j_getname(imat)
					write(6,*)'*column index ',iro,' of ',j_oname(1:j_loname),' is not in range 1,',j_o(ivmat)%i(2)
					j_err=.true.;return
				endif !if(iro.le.0.or.iro.gt.j_o(ivmat)%i(2))   3006
 
			endif !if(iro.le.0)   3004
 
			if(get)then
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				j_v(iout)=j_o(ivmat)%d((ice-1)*j_o(ivmat)%i(2)+iro)
				return
			else
				if(j_otype(iout).ne.j_ipreal)then
 
 
					call j_getname(imat,iout)
					write(6,*)'* ',j_oname2(1:j_loname2), ' is ',j_objecttypes(j_otype(iout))(1:j_lenobjecttypes(j_otype(iout))),&
						' only REAL can be put into matrix of DATA ',j_oname(1:j_loname)
					j_err=.true.
					return
				endif !if(j_otype(iout).ne.j_ipreal)   3019
				j_o(ivmat)%d((ice-1)*j_o(ivmat)%i(2)+iro)=j_v(iout)
				return
			endif !if(get)   3014
 
 
 
		endif !if(ivcases.ne.0)   2985
		! if(j_otype(arg(1)).eq.j_iplist)then
		! if(narg1.gt.1)then
		! write(6,*)'with a list argument, only one argument allowed'
		! j_err=.true.;return
		! endif !if(narg1.gt.1)then
		! ivlist=arg(1)
		! narg1=j_o(ivlist)%i(1)
 
		! arg=>j_o(ivlist)%i2(1:narg1)
 
		! endif !if(j_otype(arg(1)).eq.j_iplist)then
469			allocate(list(1:narg1))
			!	write(6,*)'keep',j_o(ivkeep)%i2,' arg ',arg
		do j=1,narg1
			list(j)=j_inlistobject(arg(j),ivkeep)
 
			if(list(j).le.0)then
				!	write(6,*)'imat ',imat
				call j_getname(arg(j),iv2=imat)
				!call j_printname('variable ',arg(j),' not in data')
				write(6,*)'variable ',j_oname(1:j_loname),' not in data ',j_oname2(1:j_loname2)
				j_err=.true.
			endif !if(list(j).le.0)   3051
		enddo !j=1,narg1   3048
		if(j_err)then
			deallocate(list);return
		endif !if(j_err)   3059
		j_i8=narg1
		iout=j_defmatrix8(iout,' ',j_nrows(ivmat),j_i8,j_matreg)
		ibas=0
		ibasin=0
		do j=1,j_o(ivmat)%i(1)
			j_o(iout)%d(ibas+1:ibas+narg1)=j_o(ivmat)%d(ibasin+list)
			ibasin=ibasin+j_o(ivkeep)%i(3)
			ibas=ibas+narg1
		enddo !j=1,j_o(ivmat)%i(1)   3066
		deallocate(list)
		return
 
	end subroutine !subroutine data()
 
	subroutine regr()
		nre=j_o(imat)%i(0)
		! if(j_o(ifunc)%i(nre).eq.j_ivone)then
		! dapu=j_o(ifunc)%d(nre)
		! if(isprint)write(6,*)'intcep ',dapu
		! nre=nre-1
		! else !if(j_o(ifunc)%i(nre).eq.j_ivone)then
		! dapu=0.
		! end if !if(j_o(ifunc)%i(nre).eq.j_ivone)then
 
		if(j_o(imat)%i(1).eq.j_ivone)then
			dapu=j_o(imat)%d(1)
			is=1
		else !if(j_o(imat)%i(1).eq.j_ivone)then
			dapu=j_0
			is=0
		endif !if(j_o(imat)%i(1).eq.j_ivone)   3086
		if(narg1.gt.nre-is)then
			write(6,*)'regression has only ',nre-is , ' arguments (in addtion to interc.) and you gave ',narg1
			j_err=.true.;return
		endif !if(narg1.gt.nre-is)   3093
 
		!	write(6,*)'<777narg,narg1,arg',narg,narg1,arg
		do i=1,narg1
			dapu=dapu+j_o(imat)%d(i+is)*j_v( arg(i))
			!	write(6,*)'+arg',j_o(imat)%d(i),j_v( arg(i))
		end do !i=1,narg1   3099
		do i=narg1+1,nre-is
			!		write(6,*)'+vi',i
			dapu=dapu+j_o(imat)%d(i+is)*j_v( j_o(imat)%i(i+is) )
			!		write(6,*)j_o(imat)%d(i),j_v( j_o(imat)%i(i))
		end do !i=narg1+1,nre-is   3103
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dapu
		return
 
	end subroutine !subroutine regr()
 
 
	subroutine getsame()
		if(.not.get)then
			write(6,*)'cannot put matrix into itself'
			j_err=.true.;return
 
		endif !if(.not.get)   3116
		if(allocated (temp))deallocate(temp)
		allocate(temp(1:nel2))
		if(isvector)then
			if(isili(1))then
				temp=j_o(imat)%d(ilist1)
			else !if(isili(1))then
				temp=j_o(imat)%d(irange(1):irange(2))
 
			endif !if(isili(1))   3124
		else !if(isvector)then
			ie=0
			do iro0=irange(1),irange(2)
				iro=iro0
				if(isili(1))iro=ilist1(iro0)
				do ico=irange(3),irange(4)
					ie=ie+1
					if(isili(2))then
 
						temp(ie)=j_o(imat)%d((iro-1)*ncol+ilist2(ico))
					else !if(isili(2))then
						temp(ie)=j_o(imat)%d((iro-1)*ncol+ico)
					endif !if(isili(2))   3137
				enddo !ico=irange(3),irange(4)   3135
 
			enddo !iro0=irange(1),irange(2)   3132
		endif !if(isvector)   3123
		iout=j_defmatrix8(iout,' ',nrow2,ncol2,j_matreg)
		j_o(iout)%d=temp
		deallocate(temp)
		return
	end subroutine !subroutine getsame()
 
 
	subroutine diags()
 
		if(nrow.ne.ncol)then
			write(6,*)'argument is ',nrow,ncol, ' diag-> requires square matrix'
			j_err=.true.;return
		endif !if(nrow.ne.ncol)   3156
		if(narg1.eq.0)then
			ibas=0
			if(get)then
				if(iout.eq.imat)then
					write(6,*)'output cannot be same as input (YET) '
					j_err=.true.;return
				endif !if(iout.eq.imat)   3163
				if(ist)then
					iout=j_defmatrix8(iout,' ',j_18,nrow,j_matreg)
				else !if(ist)then
					iout=j_defmatrix8(iout,' ',nrow,j_18,j_matreg)
				endif !if(ist)   3167
				do i=1,nrow
					j_o(iout)%d(i)=j_o(imat)%d(ibas+i)
					ibas=ibas+nrow
				enddo !i=1,nrow   3172
				return
			else !if(get)then
				if(j_otype(iout).eq.j_ipreal)then
 
					do i=1,nrow
						j_o(imat)%d(ibas+i)=j_v(iout)
						ibas=ibas+nrow
					enddo !i=1,nrow   3180
 
				elseif(j_otype(iout).eq.j_ipmatrix)then !if(j_otype(iout).eq.j_ipreal)then
					if(nrow.ne.j_o(iout)%i(3))then
						write(6,*)'cannot put to diagonal of size ',nrow,j_o(iout)%i(3),' elements'
						j_err=.true.;return
					endif !if(nrow.ne.j_o(iout)%i(3))   3186
 
					do i=1,nrow
						!				write(6,*)'imat,iout,ibas,ir1,ir2',imat,iout,ibas,ir1,ir2
						j_o(imat)%d(ibas+i)=j_o(iout)%d(i)
						ibas=ibas+nrow
					enddo !i=1,nrow   3191
				else !if(j_otype(iout).eq.j_ipreal)then
					write(6,*)'input is not MATRIX or REAL'
					j_err=.true.
				endif !if(j_otype(iout).eq.j_ipreal)   3178
				return
			endif !if(get)   3162
		endif !if(narg1.eq.0)   3160
		irange(1:2)=1
		irange(3:4)=0
		call iranges()
		write(6,*)'ira',irange
		if(irange(3).ne.0.and.(irange(3).ne.irange(1).or.irange(4).ne.irange(3)).or.&
				irange(2).gt.nrow)then
			write(6,*)'illegal dimensions ', inde(1:narg1)
			j_err=.true.
			return
 
		endif !if(irange(3).ne.0.and.(irange(3).ne.irange(1).or.irange(4)   3207
 
 
		!	nrow2=irange(2)-irange(1)+1
		ibas=(irange(1)-1)*nrow
		if(get)then
			if(iout.eq.imat)then
				write(6,*)'output cannot be same as input '
				j_err=.true.;return
			endif !if(iout.eq.imat)   3219
			if(ist)then
				iout=j_defmatrix8(iout,' ',j_18,nrow2,j_matreg)
			else !if(ist)then
				iout=j_defmatrix8(iout,' ',nrow2,j_18,j_matreg)
			endif !if(ist)   3223
			i2=0
			do i=irange(1),irange(2)
				i2=i2+1
				j_o(iout)%d(i2)=j_o(imat)%d(ibas+i)
				ibas=ibas+nrow
			enddo !i=irange(1),irange(2)   3229
			return
		else !if(get)then
			if(j_otype(iout).eq.j_ipreal)then
 
				do i=irange(1),irange(2)
					j_o(imat)%d(ibas+i)=j_v(iout)
					ibas=ibas+nrow
				enddo !i=irange(1),irange(2)   3238
				return
 
			elseif(j_otype(iout).eq.j_ipmatrix)then !if(j_otype(iout).eq.j_ipreal)then
				if(nrow2.ne.j_o(iout)%i(3))then
					write(6,*)'cannot put to diagonal of size ',nrow2,j_o(iout)%i(3),' elements'
					j_err=.true.;return
				endif !if(nrow2.ne.j_o(iout)%i(3))   3245
				i2=0
				do i=irange(1),irange(2)
					i2=i2+1
					!				write(6,*)'imat,iout,ibas,ir1,ir2',imat,iout,ibas,ir1,ir2
					j_o(imat)%d(ibas+i)=j_o(iout)%d(i2)
					ibas=ibas+nrow
				enddo !i=irange(1),irange(2)   3250
				return
			else !if(j_otype(iout).eq.j_ipreal)then
				write(6,*)'input is not MATRIX or REAL'
				j_err=.true.;return
			endif !if(j_otype(iout).eq.j_ipreal)   3236
 
		endif !if(get)   3218
	end subroutine !subroutine diags()
 
	subroutine getsetlist()
		if(nel2.eq.j_o(iout)%i(1))then
			if(isvector)then
				if(get)then
					iba=irange(1)
					do ii=1,nel2
						iiv=j_o(iout)%i2(ii)
						if(j_otype(iiv).ne.j_ipreal)call j_del(iiv)
						j_v( iiv)=j_o(imat)%d(iba)
						iba=iba+1
					enddo !ii=1,nel2   3270
				else !if(get)then
					j_o(imat)%d(irange(1):irange(2))=j_v( j_o(iout)%i2(1:nel2))
				endif !if(get)   3268
			else !if(isvector)then
				ie=0
				if(get)then
					do iro=irange(1),irange(2)
						do ico=irange(3),irange(4)
							ie=ie+1
							iiv=j_o(iout)%i2(ie)
							if(j_otype(iiv).ne.j_ipreal)call j_del(iiv)
							j_v( iiv)=j_o(imat)%d((iro-1)*ncol+ico)
						enddo !ico=irange(3),irange(4)   3283
 
					enddo !iro=irange(1),irange(2)   3282
				else !if(get)then
					do iro=irange(1),irange(2)
						do ico=irange(3),irange(4)
							ie=ie+1
							j_o(imat)%d((iro-1)*ncol+ico)=j_v( j_o(iout)%i2(ie))
 
						enddo !ico=irange(3),irange(4)   3293
					enddo !iro=irange(1),irange(2)   3292
 
				endif !if(get)   3281
			endif !if(isvector)   3267
			return
		else !if(nel2.eq.j_o(iout)%i(1))then
			write(6,*)'output list has wrong size ',j_o(iout)%i(1),' should be ',nel2
			j_err=.true.
		endif !if(nel2.eq.j_o(iout)%i(1))   3266
 
	end subroutine !subroutine getsetlist()
 
 
end subroutine getelem !recursive subroutine getelem(iob,io)
 
subroutine setelem(iob,io)  ! %%matrix put matrix element made from a(i,j)= -statements
	!	logical sing
	integer,intent(in)::iob
	integer,intent(in)::io
	integer, dimension(:), pointer :: arg
	real,dimension (:),allocatable :: mat2
	double precision,dimension (:),allocatable ::mat2d
 
	narg=j_o(iob)%i(io+1)
	write(6,*)'setl ',j_o(iob)%i(io:io+2+narg)
	return
	narg1=narg-1  !arguments for the matrix indices
	! arg(narg)= input
	!	io_=io_+narg+3
	imat=j_o(iob)%i(io+2)  !
	arg=>j_o(iob)%i(io+2+1:io+2+narg)
	ivin=arg(narg)
	!	write(6,*)'<99>',j_v(arg)
	!arg(narg) is the input
	!	call j_printname('setel  imat ',imat,' ')
	!write(6,*)'<33,',ima t,allocated(j_o(imat)%i)
	inde1=j_v(arg(1))
 
 
	if(j_otype(imat).eq.j_ipilist)then
		if(arg(1).eq.j_ivzero)then
			if(ivin.ne.j_ivzero)then
				write(6,*)'only legal value for element 0 of ILIST is zero'
				j_err=.true.;return
			endif !if(ivin.ne.j_ivzero)   3339
			j_o(imat)%i(1)=0
		else !if(arg(1).eq.j_ivzero)then
			if(inde1.gt.j_o(imat)%i(4))then
				write(6,*)'only ',j_o(imat)%i(4),' elements allocated, cannot use element ',inde1
				j_err=.true.
			endif !if(inde1.gt.j_o(imat)%i(4))   3345
			j_o(imat)%i2(inde1)=j_v(ivin)
			j_o(imat)%i(1)=max(j_o(imat)%i(1),inde1)
		endif !if(arg(1).eq.j_ivzero)   3338
		return
	endif !if(j_otype(imat).eq.j_ipilist)   3337
 
	if(j_otype(imat).ne.j_iplist)then !if(j_otype(imat).eq.j_iplist)then
		!	write(6,*)'<22setelem>',j_o(iob)%i(io)
		call j_printname('**not a legal matrix: ',imat,' ')
		j_err=.true. ;return
	end if !if(j_otype(imat).ne.j_iplist)   3355
 
	if(narg1.eq.1)then
 
		!	idim1=j_o(iob)%i(io+2)
 
		if(inde1.le.0.or.inde1.gt.j_o(imat)%i(3))then
			call j_printname('illegal index for ',imat,' ')
			write(6,*)'was ',inde1
			j_err=.true.
			return
		endif !if(inde1.le.0.or.inde1.gt.j_o(imat)%i(3))   3365
		if(j_otype(imat).eq.j_iplist)then
			iel=j_o(imat)%i2(inde1)
			call j_copy2(ivin,iel)
		else !if(j_otype(imat).eq.j_iplist)then
			j_o(imat)%d(inde1)=j_v(ivin)
 
		endif !if(j_otype(imat).eq.j_iplist)   3371
		return
 
	endif !if(narg1.eq.1)   3361
 
	if(narg1.ge.2)then
 
		inde2=j_v(arg(2))
		if(inde2.gt.0)then
			if(inde2.gt.j_o(imat)%i(2))then
				write(6,*)'illegal index ',inde2,' max is ',j_o(imat)%i(2)
				j_err=.true.;return
			endif !if(inde2.gt.j_o(imat)%i(2))   3386
			j_o(imat)%d((inde1-1)*j_o(imat)%i(2)+inde2)=j_v(ivin)
			return
		endif !if(inde2.gt.0)   3385
	endif !if(narg1.ge.2)   3382
 
	if(narg1.gt.2.or..not.(j_o(imat)%i(1).eq.1.or.j_o(imat)%i(2).eq.1))then
		write(6,*)'later'
		j_err=.true.;return
	endif !if(narg1.gt.2.or..not.(j_o(imat)%i(1).eq.1.or.j_o(imat)%i(   3395
	inde2=-inde2
	if(inde2.gt.j_o(imat)%i(3))then
		write(6,*)'illegal index ',inde2,' max is ',j_o(imat)%i(1)
		j_err=.true.;return
	endif !if(inde2.gt.j_o(imat)%i(3))   3400
	nel=inde2-inde1+1
	if(j_otype(ivin).eq.j_ipmatrix)then
		if(j_otype(imat).eq.j_ipmatrix)then
			j_o(imat)%d(inde1:inde2)=j_o(ivin)%d(1:min(nel,j_o(ivin)%i(3)))
		else !if(j_otype(imat).eq.j_ipmatrix)then
			j_v(j_o(imat)%i2(inde1:inde2))=j_o(ivin)%d(1:min(nel,j_o(ivin)%i(3)))
		endif !if(j_otype(imat).eq.j_ipmatrix)   3406
	else !if(j_otype(ivin).eq.j_ipmatrix)then
		if(j_otype(imat).eq.j_ipmatrix)then
			j_o(imat)%d(inde1:inde2)=j_v(ivin)
		else !if(j_otype(imat).eq.j_ipmatrix)then
			j_v(j_o(imat)%i2(inde1:inde2))=j_v(ivin)
		endif !if(j_otype(imat).eq.j_ipmatrix)   3412
	endif !if(j_otype(ivin).eq.j_ipmatrix)   3405
	return
 
 
end subroutine setelem !subroutine setelem(iob,io)
 
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
	call j_startfunction(iob,io,0,narg,arg,ivout)
	!subroutine j_deflist(iv,name,list0,ivout) !allocates list object with size list0,
	! but put it as empty
 
	call  j_deflistobject(ivout,' ',iout,list0=narg,list=arg)
	! j_o(iout)%i2(1:narg)=arg
	j_otype(iout)=j_iplist2
	call j_clearoption(iob,io)
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
	! A({1,5},{3}=
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
	call j_startfunction(iob,io,0,narg,arg,ivout)
	!j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
	call j_getoption(iob,io,j_mextra,-1,1,j_ipreal,.true.,noptarg,j_optarg0) ! %%option
 
	if(j_err)return
 
	if(noptarg.eq.1)nextra=j_v(j_optarg0(1))
 
	call j_getoption(iob,io,j_mdata,-1,1,j_ipdata,.true.,noptarg,j_optarg0)
	if(j_err)return
	if(noptarg.eq.1)then
		ivkeep=j_o(j_optarg0(1))%i(2)
		nkeep=j_o(ivkeep)%i(1)
		ntot=narg+nextra
		call  j_deflistobject(ivout,' ',iout,nres=ntot)
		do i=1,narg
			do iloc=1,nkeep
				if(arg(i).eq.j_o(ivkeep)%i2(iloc))goto 134
			enddo !iloc=1,nkeep   3512
			iloc=0
			!			iloc=findloc(j_o(ivkeep)%i2(1:nkeep),arg(i),dim=1)
	134			j_o(ivout)%i2(i)=iloc
		enddo !i=1,narg   3511
		j_o(ivout)%i(1)=narg
	else !if(noptarg.eq.1)then
		call j_getoption(iob,io,j_mvalues,-1,9999,j_ipdata,.true.,noptarg,j_optarg0)
		if(j_err)return
		if(narg.eq.0.and.noptarg.eq.0)then
			write(6,*)'without data-> and values-> there must be argument'
			j_err=.true.;return
		endif !if(narg.eq.0.and.noptarg.eq.0)   3523
		if(noptarg.gt.0)then
			nin=noptarg+nextra
			if(allocated(temp))deallocate(temp)
			allocate(temp(1:noptarg))
			temp=j_v(j_optarg0)
			call  j_deflistobject(ivout,' ',iout,list0=noptarg,list=temp,nres=nextra)
			deallocate(temp)
		else !if(noptarg.gt.0)then
			if(nextra.gt.0)then
				write(6,*)'if is size is given as argument there cannot be extra->'
				j_err=.true.;return
			endif !if(nextra.gt.0)   3535
			nin=narg+nextra
			call  j_deflistobject(ivout,' ',iout,nres=nin)
 
			j_o(iout)%i2(1:narg)=j_v(arg)
			j_o(iout)%i(1)=narg
		endif !if(noptarg.gt.0)   3527
 
	endif !if(noptarg.eq.1)   3506
	!subroutine j_deflist(iv,name,list0,ivout) !allocates list object with size list0,
 
	! j_o(iout)%i2(1:narg)=arg
	j_otype(ivout)=j_ipilist
	call j_clearoption(iob,io)
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
	call j_startfunction(iob,io,0,narg,arg,ivout)
	if(j_otype(arg(1)).ne.j_iplist.and.j_otype(arg(1)).ne.j_ipilist)then
		write(6,*)'first argument is not LIST or ILIST'
		j_err=.true.;return
	endif !if(j_otype(arg(1)).ne.j_iplist.and.j_otype(arg(1)).ne.j_ip   3567
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
	endif !if(nle2.ge.j_o(arg(1))%i(4))   3575
	if(j_otype(arg(1)).eq.j_iplist)then
 
		j_o(arg(1))%i2(nle+1:nle+narg1)=arg(2:narg)
	else !if(j_otype(arg(1)).eq.j_iplist)then
		j_o(arg(1))%i2(nle+1:nle+narg1)=j_v(arg(2:narg))
	endif !if(j_otype(arg(1)).eq.j_iplist)   3584
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
	call j_startfunction(iob,io,j_iplist,narg,j_arg,ivout)
	if(j_otype(ivout).ne.j_iptable.and.j_otype(ivout).ne.j_ipreal)call j_del(ivout)
	allocate(j_o(ivout)%i(1:5))
	j_o(ivout)%i(1)=j_o(j_arg(1))%i(1)
	j_o(ivout)%i(2)=j_o(j_arg(2))%i(1)
	j_o(ivout)%i(3)=j_o(j_arg(1))%i(1)*j_o(j_arg(2))%i(1)
	j_o(ivout)%i(4)=j_arg(1)
	j_o(ivout)%i(5)=j_arg(2)
	! endsection
end subroutine table
 
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
		write(6,*)'there are ',j_nobjecttypes_,' object types'
		write(6,*)'Note, there is a variable for each type, so if(type(data1).eq.DATA) works'
		do i=1,j_nobjecttypes_
			if(j_objecttypes(i)(1:4).ne.'free')write(6,*)j_objecttypes(i)
 
 
		enddo !i=1,j_nobjecttypes_   3640
		return
 
	endif !if(narg.eq.0)   3637
 
	if(j_linkoption(iob,io,j_mcontent).ge.0.and.j_otype(iarg)==j_ipchar) then
		call j_getchar2(iarg,j_tempchar,le)
		iv_ = j_object(j_tempchar(1:le))
		if(iv_>0) then
			j_v(iout) = j_otype(iv_)
		else !if(iv_>0) then
			j_v(iout) = -1
		endif !if(iv_>0)   3652
	else !if(j_linkoption(iob,io,j_mcontent).ge.0.and.j_otype(iarg)==j_ipchar) then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_otype(iarg)
	endif !if(j_linkoption(iob,io,j_mcontent).ge.0.and.j_otype(iarg)=   3649
 
	if(j_linkoption(iob,io,j_mcontent).ge.0) call j_clearoption(iob,io)  ! subroutine
 
	return
end subroutine !subroutine type(iob, io)
 
subroutine delete_o(iob,io)
	!Section delete_o delete_o() Deletes objects
	!The function delete_o() deletes all the argument objects, which means that the
	! associated allocated vectors are deallocated, and the object will be REAL.
	!endheader
	!Note Note that delete_o() is actually needed only for matrices, because objects
	!can be deleted with Object=0. When Object is a matrix, then Object=0 puts all elements
	!into zero.
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
	end do !i=1,narg   3686
end subroutine !subroutine delete_o(iob,io)
 
subroutine delete_f(iob,io)
	!Section delete_f delete_f() Deletes files
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
		endif !if(j_otype(iv).ne.j_ipchar)   3712
		nu_ = j_nunits(j_nused+1)	! väliaikainen kanava tiedostolle
		if (j_iounit(iv)>0)then !j_o(iv)%i(4)>0) then! aukioleva tiedosto
			nu_ = j_iounit(iv)  !j_o(iv)%i(4)
			call j_closeunit(nu_)
		endif !if (j_iounit(iv)>0)   3718
		call j_getchar(iv,j_filename,le_)
		inquire(file = j_filename(1:le_) , exist=exis)
		if(exis) then
			open(unit=nu_,err=99, file=j_filename(1:le_), status="old")
			goto 4
	99 		write(6,*)'errot opening ', j_filename(1:le_),' for deleting it'
			j_err=.true.;return
	4		close (unit=nu_, status="delete")
		else !if(exis) then
			j_err=.true.
			write(6,*)'*file ', j_filename(1:le_),' does not exist'
		endif !if(exis)   3724
	end do !i=1,narg   3710
 
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
	endif !if(j_otype(iv).ne.j_ipchar)   3746
	nu_ = j_nunits(j_nused+1)	! väliaikainen kanava tiedostolle
	if (j_iounit(iv)>0)then !j_o(iv)%i(4)>0) then! aukioleva tiedosto
		nu_ = j_iounit(iv)  !j_o(iv)%i(4)
		call j_closeunit(nu_)
	endif !if (j_iounit(iv)>0)   3751
	call j_getchar(iv,j_filename,le_)
	inquire(file = j_filename(1:le_) , exist=exis)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(exis) then
		j_v(iout)=j_1
	else !if(exis) then
		j_v(iout)=j_0
	endif !if(exis)   3758
	if(narg.ge.2)then
		iv2=j_o(iob)%i(io+2)
		if (j_iounit(iv)>0)then !j_o(iv)%i(4)>0) then! aukioleva tiedosto
			j_v(iv2)=j_iounit(iv)
		else !if (j_iounit(iv)>0)then
			j_v(iv2)=j_0
		endif !if (j_iounit(iv)>0)   3765
	endif !if(narg.ge.2)   3763
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
	endif !if(j_otype(iv).ne.j_ipchar)   3781
	call j_getchar(iv,j_filename,le)
	iv2=j_object(j_filename(1:le))
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(iv2.le.0)then
		j_v(iout)=0
	else !if(iv2.le.0)then
		j_v(iout)=j_otype(iout)
	endif !if(iv2.le.0)   3788
 
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
	endif !if(j_err)   3804
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
	endif !if(arg(8).eq.0)   3827
	niter=(arg(7)-arg(6))/arg(8)+1
 
	if(j_otype(arg(2)).ne.j_ipreal)call j_del(arg(2))
	j_v(arg(2))=arg(6)
	!write(6,*)'<74',arg,j_v(arg(2))
	if(niter.le.0)then
		io=arg(1)!jump after enddo
	else !if(niter.le.0)then
		io=io+9
	endif !if(niter.le.0)   3836
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
		endif !if(j_o(iob)%i(iod+6).gt.j_o(iob)%i(iod+7))   3855
	else !if(j_o(iob)%i(iod+8).gt.0)then
		if(j_o(iob)%i(iod+6).lt.j_o(iob)%i(iod+7))then
 
			io=io+2
			return
		endif !if(j_o(iob)%i(iod+6).lt.j_o(iob)%i(iod+7))   3861
	endif !if(j_o(iob)%i(iod+8).gt.0)   3853
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
	if(j_v(j_o(iob)%i(io+2)).eq.j_0)then
		io=j_o(iob)%i(io+1)  !condition not satisfied
	else !if(j_v(j_o(iob)%i(io+2)).eq.j_0)then
		io=j_o(iob)%i(io+3)   !condition satified
	endif !if(j_v(j_o(iob)%i(io+2)).eq.j_0)   3881
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
	! c=9
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
	!	write(6,*)'which narg,imod',narg,imod
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	do i=1,narg-imod,2
		if(j_v(j_o(iob)%i(io+1+i)).ne.0.)then
			j_v(iout)=j_v(j_o(iob)%i(io+2+i));return
		end if !if(j_v(j_o(iob)%i(io+1+i)).ne.0.)   3995
	end do !i=1,narg-imod,2   3994
	if(imod.ne.0)then
		j_v(iout)=j_v(j_o(iob)%i(io+1+narg))
	else
		write(6,*)'no condition was satisfied and defaul value was not given'
		j_err=.true.
	endif !if(imod.ne.0)   3999
 
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
			call j_getchar(iv,j_cline,le)
			write(6,*)j_cline(1:le)
		else !if(j_ipc(iv).ne.0)then
			write(6,*)j_v(iv)
		end if !if(j_ipc(iv).ne.0)   4020
	end do !j=1,narg   4018
	j_err=.true.
	return
end subroutine errexit !subroutine errexit(iob,io)
 
subroutine goto(iob,io)
 
	!	call j_getname(j_o(iob)%i(io+1))
	!	write(6,*)'var ',j_oname(1:j_loname)
	!write(6,*)'goto',j_o(iob)%i(io:io+15)
	narg=j_o(iob)%i(io+1)
	if(narg.eq.0)then
		io=j_o(iob)%i(io+2)  !simple
		!	write(6,*)'uusio',io
		return
	endif !if(narg.eq.0)   4037
 
 
	iad=j_o(iob)%i(0)+2					!
	nlabel=j_o(iob)%i(iad)
 
	!	write(6,*)'narg,iad,nlabel,labels ',narg,iad,nlabel,j_o(iob)%i(iad+1:iad+nlabel)
	!	write(6,*)io,'%',j_o(iob)%i(io+1:io+20)
	!select from all lables
	if(narg.eq.1)then
		igo=j_o(iob)%i(io+2)
 
		io=j_o(iob)%i(iad+igo)+1    !the basis is stored
		write(6,*)'uusio ',igo,io
		stop 'goto'
	endif !if(narg.eq.1)   4050
 
 
	igo=j_v(j_o(iob)%i(io+2)) !goto(igo,ad1,ad3,ad5
	!	write(6,*)'igo ',igo
 
	if(igo.eq.0)then
		io=io+narg+3
		!	write(6,*)'uusio0 ',io
		return
	endif !if(igo.eq.0)   4062
	if(j_v(j_ivdebugtrans).ne.0)then
		call j_getcurline(iob,io)
		write(6,*)'goto3curl ',j_curline(j_recursion)
	endif !if(j_v(j_ivdebugtrans).ne.0)   4067
 
 
	if(igo.lt.0.or.igo.ge.narg)then
		write(6,*)'cannot goto to label ',igo, 'there are only ',narg-1,' labels in goto()'
		j_err=.true.
		return
	endif !if(igo.lt.0.or.igo.ge.narg)   4073
 
	!	write(6,*)'igo,io+2,io+2+igo,igo2,iad+igo2',igo,io+2,io+2+igo,igo2,iad+igo2
	iad2=j_o(iob)%i(io+2+igo)
	!	write(6,*)'iad2 ',iad2
	io=j_o(iob)%i(iad+iad2)+1
 
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
	endif !if(igo.eq.0)   4102
	if(igo.lt.0.or.igo.gt.narg)then
		call j_getname(iout)
		write(6,*)j_oname(1:j_loname),' has illegal value ',igo,&
			' in goto(), should be between 1 and ',narg
		j_err=.true.
		return
	endif !if(igo.lt.0.or.igo.gt.narg)   4105
 
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
			if(j_v(irg).lt.0.d0)then
				write(6,*)'negative argument ',j_v(irg)
				j_err=.true.
			else !if(j_v(irg).lt.0.d0)then
				j_v(iout)=j_v(irg)**pow
			endif !if(j_v(irg).lt.0.d0)   4156
		endif !if(ipower)   4153
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
 
 
		!if(minva.lt.0.d0)then
		nrow=j_o(irg)%i(1)
		ncol=j_o(irg)%i(2)
		if(j_otype(iout).ne.j_ipmatrix)iout=j_defmatrix(iout,' ',nrow,ncol,j_matreg)
		if(j_o(iout)%i(3).ne.j_o(irg)%i(3))then
			ivout=j_defmatrix(iout,' ',nrow,ncol,j_matreg)
		else !if(j_o(iout)%i(3).ne.j_o(irg)%i(3))then
			j_o(iout)%i(1)=nrow
			j_o(iout)%i(2)=ncol
		endif !if(j_o(iout)%i(3).ne.j_o(irg)%i(3))   4170
		if(ipower)then
			j_o(iout)%d=	j_o(irg)%d**ipow
		else !if(ipower)then
			minva=minval(j_o(irg)%d)
			if(minva.lt.0.d0)then
				write(6,*)'negative elements,min = ',minva
				j_err=.true.;return
			endif !if(minva.lt.0.d0)   4180
			j_o(iout)%d=	j_o(irg)%d**pow
		endif !if(ipower)   4176
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   4151
 
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
	endif !if(j_otype(irg1).ne.j_ipmatrix.or.j_otype(irg2).ne.j_ipmat   4200
	nrow=j_o(irg1)%i(1)
	ncol=j_o(irg1)%i(2)
	nel=j_o(irg1)%i(3)
	if(nrow.ne.j_o(irg2)%i(1).or.ncol.ne.j_o(irg2)%i(2))then
		write(6,*)'arguments of *.  are not compatible'
		j_err=.true.;return
	endif !if(nrow.ne.j_o(irg2)%i(1).or.ncol.ne.j_o(irg2)%i(2))   4207
	if(irg1.ne.iout.and.irg2.ne.iout)ivout= j_defmatrix(iout,' ',nrow,ncol,j_matreg)
	do i=1,nel
		j_o(iout)%d(i)=j_o(irg1)%d(i)*j_o(irg2)%d(i)
	enddo !i=1,nel   4212
	return
 
end subroutine HMULT !subroutine HMULT(iob,io)
 
subroutine HDIV(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg1).ne.j_ipmatrix.or.j_otype(irg2).ne.j_ipmatrix)then
		write(6,*)' *.  requires matrix arguments'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix.or.j_otype(irg2).ne.j_ipmat   4224
	nrow=j_o(irg1)%i(1)
	ncol=j_o(irg1)%i(2)
	nel=j_o(irg1)%i(3)
	if(nrow.ne.j_o(irg2)%i(1).or.ncol.ne.j_o(irg2)%i(2))then
		write(6,*)'arguments of *.  are not compatible'
		j_err=.true.;return
	endif !if(nrow.ne.j_o(irg2)%i(1).or.ncol.ne.j_o(irg2)%i(2))   4231
	if(irg1.ne.iout.and.irg2.ne.iout)ivout=j_defmatrix(iout,' ',nrow,ncol,j_matreg)
	do i=1,nel
		if(j_o(irg2)%d(i).eq.j_0)then
			write(6,*)' /. :element ',i, 'is zero'
			j_err=.true.;return
 
		endif !if(j_o(irg2)%d(i).eq.j_0)   4237
		j_o(iout)%d(i)=j_o(irg1)%d(i)/j_o(irg2)%d(i)
	enddo !i=1,nel   4236
	return
 
end subroutine HDIV !subroutine HDIV(iob,io)
 
 
subroutine MULT(iob,io) ! mata*matb
	double precision,dimension(:),allocatable ::temp
	integer, dimension (:), pointer :: irg
	!	integer, dimension (:), pointer :: irg
	!	integer irg(2)
	logical ismatrix(2),matrixout
	integer*8 nrow(2),ncol(2),nel,i
	logical ::p=.false.
	!	write(6,*)'MULTio ',j_o(iob)%i(1: j_o(iob)%i(1))
	!write(6,*)'io ',io
	!	call  j_startfunction(iob,io,j_ipreal,narg,arg,ivout,iptype2=j_ipmatrix)
	call  j_startfunction(iob,io,0,narg,irg,iout)
	!	write(6,*)'irg',irg,j_otype(irg)
	!write(6,*)'<443irg1irg2iout',irg(1),irg(2),iout,j_otype(irg(1)),j_otype(irg(2))
 
	if(j_otype(irg(1)).eq.j_ipreal.and.j_otype(irg(2)).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_v(irg(1))*j_v(irg(2))  !two scalars as fast as possible
		return
	endif !if(j_otype(irg(1)).eq.j_ipreal.and.j_otype(irg(2)).eq.j_ip   4264
	matrixout=j_otype(iout).eq.j_ipmatrix
 
	do i=1,2
		select case(j_otype(irg(i)))
		case(j_ipreal) !select case(j_otype(irg(i)))
		ismatrix(i)=.false.
		ireal=irg(i)
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
 
		case default !select case(j_otype(irg(i)))
		call j_printname('argument ',irg(i),' of MULT is not MATRIX or REAL or LIST')
		j_err=.true.;return
		end select !select case(j_otype(irg(i)))
	enddo !i=1,2   4271
	if(p)write(6,*)'ismatrix ',ismatrix, ' nrow ',nrow,' ncol ',ncol
	if(.not.(ismatrix(1).and.ismatrix(2)))then
		nel=j_nelem(irg(imatrix))  !j_o(irg(imatrix))%i(3)
		if(j_otype(iout).eq.j_iplist)then
 
			if(j_o(iout)%i(1).ne.nel)goto 99
			do i=1,nel
				iv=j_o(iout)%i2(i)
				if(j_otype(iv).ne.j_ipreal)call j_del(iv)
				j_v(iv)=j_v(ireal)*j_o(irg(imatrix))%d(i)
				return
			enddo !i=1,nel   4299
		endif !if(j_otype(iout).eq.j_iplist)   4296
		if(j_otype(iout).eq.j_ipmatrix)then
			if(j_o(iout)%i(1).eq.nrow(imatrix).and.j_o(iout)%i(2).eq.ncol(imatrix))then
				j_o(iout)%d=j_v(ireal)*j_o(irg(imatrix))%d(1:nel)
				return
			endif !if(j_o(iout)%i(1).eq.nrow(imatrix).and.j_o(iout)%i(2).eq.n   4307
 
		endif !if(j_otype(iout).eq.j_ipmatrix)   4306
		ivout=j_defmatrix8(iout,' ',nrow(imatrix),ncol(imatrix),j_matreg)
		j_o(iout)%d=j_v(ireal)*j_o(irg(imatrix))%d(1:nel)
		return
 
	endif !if(.not.(ismatrix(1).and.ismatrix(2)))   4294
 
	!both are matrices
	if(ncol(1).ne.nrow(2))goto 99
	if(nrow(1).eq.1.and.ncol(2).eq.1)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dot_product(j_o(irg(1))%d(1:ncol(1)),j_o(irg(2))%d(1:ncol(1)))
		return
	endif !if(nrow(1).eq.1.and.ncol(2).eq.1)   4321
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
		ivout=j_defmatrix8(iout,' ',nrow(1),ncol(2),j_matreg)
		j_o(iout)%d=reshape(matmul(reshape(j_o(irg(2))%d,(/ncol(2),nrow(2)/)),&
			reshape(j_o(irg(1))%d,(/ncol(1),nrow(1)/) ) ), (/nel/) )
 
	endif !if(nrow(1).eq.ncol(1).and.iout.eq.irg(1).or.iout.eq.irg(2)   4329
	return
 
 
 
99		call j_getname(irg(1),irg(2))

	write(6,*)'dimensions do not agree in MULT ',j_oname(1:j_loname),' is ',&
		nrow(1),ncol(1),j_oname2(1:j_loname2),' is ',nrow(2),ncol(2)
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
	endif !if(j_otype(irg2).ne.j_ipreal)   4362
	if(j_v(irg2).eq.j_0)then
		write(6,*)'*division by zero'
		j_err=.true.
		return
	endif !if(j_v(irg2).eq.j_0)   4367
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
		endif !if(iout.ne.irg1)   4377
	else !if(j_otype(irg1).eq.j_ipreal)then
		write(6,*)'*numerator is ',j_objecttypes(j_otype(irg1)),' not REAL or MATRIX'
		j_err=.true.
 
	end if !if(j_otype(irg1).eq.j_ipreal)   4372
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
	logical::scalar(2),vector(2),matrix(2),list(2)
	character*6,dimension(19)::opname
	data opname/'+','?','.eq.','.ne.','.le.','.lt.','ge','.gt.','?','.and.',&
		'.or','.eqv.','.neqv','*.','/.','min','max','sign','mod'/
	!	call  j_startfunction(iob,io,j_ipreal,narg,arg,ivout,iptype2=j_ipmatrix)
	call  j_startfunction(iob,io,0,narg,irg,iout)
 
	!	write(6,*)'binop ',ioper,opname(ioper)
 
	if(narg.eq.1)then
		if(j_otype(irg(1)).eq.j_ipmatrix)then
 
			if(ioper.ne.16.and.ioper.ne.17)then
				!if(ioper.eq.16)then
				write(6,*)'illegal operation with one argument'
				j_err=.true.
				return
 
			endif !if(ioper.ne.16.and.ioper.ne.17)   4418
			nco=j_ncols(irg(1)) !j_o(irg(1))%i(2)
			nro=j_nrows(irg(1))  !j_o(irg(1))%i(1)
			if(nco.eq.j_18.or.j_linkoption(iob,io,j_many,clear=.true.).ge.0)then
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				!	write(6,*)'<65',iout,j_otype(iout)
				if(ioper.eq.16)then
 
					j_v(iout)=minval(j_o(irg(1))%d)
				elseif(ioper.eq.17)then !if(ioper.eq.16)then
					j_v(iout)=maxval(j_o(irg(1))%d)
 
				endif !if(ioper.eq.16)   4430
			else
				iout=j_defmatrix8(iout,' ',j_18,nco,j_matreg)
				ibas=0
				if(ioper.eq.16)then
					j_o(iout)%d(1:nco)=j_inf
					do i=1,nro
						do j=1,nco
							j_o(iout)%d(j)=min(j_o(iout)%d(j),j_o(irg(1))%d(ibas+j))
						enddo !j=1,nco   4443
						ibas=ibas+nco
					enddo !i=1,nro   4442
				else
 
					j_o(iout)%d(1:nco)=j_ninf
					do i=1,nro
						do j=1,nco
							j_o(iout)%d(j)=max(j_o(iout)%d(j),j_o(irg(1))%d(ibas+j))
						enddo !j=1,nco   4452
						ibas=ibas+nco
					enddo !i=1,nro   4451
				endif !if(ioper.eq.16)   4440
			endif !if(nco.eq.j_18.or.j_linkoption(iob,io,j_many,clear=.true.)   4427
 
 
		endif !if(j_otype(irg(1)).eq.j_ipmatrix)   4416
		return
	endif !if(narg.eq.1)   4415
	!	narg=j_o(iob)%i(io+1)
 
	!	irg=j_o(iob)%i(io+2:io+3)
	!	iout=j_o(iob)%i(io+narg+2)
	!	write(6,*)'binop ',ioper,irg,j_otype(irg(1)),j_otype(irg(2))
	!write(6,*)'<7373',narg,irg
	!ordinary summation as fast as possibleselect case
	if(j_otype(irg(1)).eq.j_ipreal.and.j_otype(irg(2)).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		select case (ioper)
		case( 1 ) !select case (ioper)
		!	write(6,*)'binoplus',j_v(irg(1)),j_v(irg(2))
		j_v(iout)=j_v(irg(1))+j_v(irg(2))
		case (2) !select case (ioper)
		j_v(iout)=j_v(irg(1))-j_v(irg(2))
		case (3) !select case (ioper)
		j_v(iout)=j_0
		if(j_v(irg(1)).eq.j_v(irg(2)))j_v(iout)=j_1
		case( 4) !select case (ioper)
		j_v(iout)=j_0
		if(j_v(irg(1)).ne.j_v(irg(2)))j_v(iout)=j_1
		case (5) !select case (ioper)
		j_v(iout)=j_0
		if(j_v(irg(1)).le.j_v(irg(2)))j_v(iout)=j_1
		case (6) !select case (ioper)
		j_v(iout)=j_0
		if(j_v(irg(1)).lt.j_v(irg(2)))j_v(iout)=j_1
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
 
		case( 11) !select case (ioper)
		!		write(6,*)'OR ',io,j_o(iob)%i(io-10:io),'*',j_o(iob)%i(io:io+10)
		j_v(iout)=j_0
		if(j_v(irg(1)).ne.j_0.or.j_v(irg(2)).ne.j_0)j_v(iout)=j_1
 
 
		case( 12) !select case (ioper)
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
		endif !if(j_v(irg(2)).eq.j_0)   4521
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
 
		return
	endif !if(j_otype(irg(1)).eq.j_ipreal.and.j_otype(irg(2)).eq.j_ip   4471
 
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
		select case(j_otype(irg(i)))
		case(j_ipreal) !select case(j_otype(irg(i)))
		!		ismatrix(i)=.false.
		!		ireal=irg(i)
		scalar(i)=.true.
 
		ivfake=irg(i)
		j_o(j_ivfakematrix)%d(1)=j_v(ivfake)
		iscalar=i
		irg(i)=j_ivfakematrix   !must be returned
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
		if(nrow(i).eq.j_18)then !j_o(irg(i))%i(2).eq.1)then
			rowstep(i)=-ncol(i)
 
		endif !if(nrow(i).eq.j_18)   4586
		if(ncol(i).gt.j_18)colstep(i)=j_18
 
 
		if(nrow(i).eq.j_18.or.ncol(i).eq.j_18)then
			vector(i)=.true.
			ivector=i
		else !if(j_o(irg(i))%i(1).eq.1.or.j_o(irg(i))%i(2).eq.1)then
			matrix(i)=.true.
			imatrix=i
		endif !if(nrow(i).eq.j_18.or.ncol(i).eq.j_18)   4593
 
		!	imatrix=i
 
		case default !select case(j_otype(irg(i)))
		call j_getname(irg(i))
		write(6,*)'argument ',j_oname(1:j_loname),' of ',opname(ioper),' is not MATRIX or REAL or LIST'
		j_err=.true.;if(iscalar.ne.0)irg(iscalar)=ivfake;return
		end select !select case(j_otype(irg(i)))
	enddo !i=1,2   4554
	!either 1 or 2 is matrix
	!	if(j_v(j_ivdollar).eq.5.d0)write(6,*)'binop',vector,j_o(irg(1))%i(1:3),j_o(irg(2))%i(1:3)
	!write(6,*)
	! if(vector(1).and.vector(2).and.j_o(irg(1))%i(3).ne.j_o(irg(2))%i(3).or. &
	! matrix(1).and.matrix(2).and.(j_o(irg(1))%i(1).ne.j_o(irg(2))%i(1).or. &
	! j_o(irg(1))%i(2).ne.j_o(irg(2))%i(2)))goto 99
	!write(6,*)'vec',vector,'matr',matrix,'scaalr',scalar,'nrow ',nrow,'ncol ',ncol
	if(vector(1).and.vector(2).and.nelem(1).ne.nelem(2).or. &
		matrix(1).and.matrix(2).and.(nrow(1).ne.nrow(2).or. &
		ncol(1).ne.ncol(2)))write(6,*)'vec',vector,nelem,matrix,nrow,ncol
	if(vector(1).and.vector(2).and.nelem(1).ne.nelem(2).or. &
		matrix(1).and.matrix(2).and.(nrow(1).ne.nrow(2).or. &
		ncol(1).ne.ncol(2)))goto 99
 
	if(vector(1).and.vector(2))then
		ncolout=max(ncol(1),ncol(2))
		nrowout=min(nrow(1),nrow(2))
	else !if(vector(1).and.vector(2))then
		nrowout=max(nrow(1),nrow(2)) !j_o(irg(1))%i(1),j_o(irg(2))%i(1))
		ncolout=max(ncol(1),ncol(2))  !j_o(irg(1))%i(2),j_o(irg(2))%i(2))
	endif !if(vector(1).and.vector(2))   4623
	nel=max(nelem(1),nelem(2))  !j_o(irg(1))%i(3),j_o(irg(2))%i(3))
	if(j_otype(iout).eq.j_iplist)then
		listout=.true.
		if(ncolout.ne.1.or.nrowout.ne.j_o(iout)%i(2))goto 99
	else !if(j_otype(iout).eq.j_iplist)then
		listout=.false.
	endif !if(j_otype(iout).eq.j_iplist)   4631
 
	if(j_otype(iout).ne.j_ipmatrix)ivout=j_defmatrix8(iout,' ',nrowout,ncolout,j_matreg)
 
	!	if(nel.ne.j_o(iout)%i(3))ivout=j_defmatrix8(iout,' ',nrowout,ncolout,j_matreg)
	if(nel.ne.j_nelem(iout))ivout=j_defmatrix8(iout,' ',nrowout,ncolout,j_matreg)
	!	write(6,*)'iope ',ioper,matrix,vector,scalar
 
 
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
			endif !if(j_o(irg(1))%d(iel).eq.j_o(irg(2))%d(iel))   4653
		enddo !iel=1,nel   4652
 
		case (4) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ne.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).ne.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ne.j_o(irg(2))%d(iel))   4662
		enddo !iel=1,nel   4661
 
		case ( 5) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).le.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).le.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).le.j_o(irg(2))%d(iel))   4671
		enddo !iel=1,nel   4670
 
		case (6) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).lt.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).lt.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).lt.j_o(irg(2))%d(iel))   4680
		enddo !iel=1,nel   4679
 
		case (7) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ge.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).ge.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ge.j_o(irg(2))%d(iel))   4689
		enddo !iel=1,nel   4688
 
		case (8) !select case (ioper)
		do iel=1,nel
			write(6,*)i,j_o(irg(1))%d(iel),'>',j_o(irg(2))%d(iel)
			if(j_o(irg(1))%d(iel).gt.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).gt.j_o(irg(2))%d(iel))then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).gt.j_o(irg(2))%d(iel))   4699
		enddo !iel=1,nel   4697
 
		case (10) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0)then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0)then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0   4708
		enddo !iel=1,nel   4707
 
 
		case (11) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ne.j_0.or.j_o(irg(2))%d(iel).ne.j_0)then
				j_o(iout)%d(iel)=j_1
			else !if(j_o(irg(1))%d(iel).ne.j_0.or.j_o(irg(2))%d(iel).ne.j_0)then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ne.j_0.or.j_o(irg(2))%d(iel).ne.j_0)   4718
		enddo !iel=1,nel   4717
 
 
 
		case (12) !select case (ioper)
		do iel=1,nel
			if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0.or.j_o(irg(1))%d(iel).eq.j_0.and.&
					j_o(irg(2))%d(iel).eq.j_0)then
				j_o(iout)%d(iel)=j_1
			else !j_o(irg(2))%d(iel).eq.j_0)then
				j_o(iout)%d(iel)=j_0
			endif !if(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0   4729
		enddo !iel=1,nel   4728
 
 
		case (13) !select case (ioper)
		do iel=1,nel
			if(.not.(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).ne.j_0.or.j_o(irg(1))%d(iel).eq.j_0.and.&
					j_o(irg(2))%d(iel).eq.j_0))then
				j_o(iout)%d(iel)=j_1
			else !j_o(irg(2))%d(iel).eq.j_0))then
				j_o(iout)%d(iel)=j_0
			endif !if(.not.(j_o(irg(1))%d(iel).ne.j_0.and.j_o(irg(2))%d(iel).   4740
		enddo !iel=1,nel   4739
 
		case (14 ) !select case (ioper)
		j_o(iout)%d=j_o(irg(1))%d*j_o(irg(2))%d
 
		case (15) !select case (ioper)
		if(any(j_o(irg(2))%d.eq.j_0))then
			write(6,*)'division with zero'
			j_err=.true.
		else !if(any(j_o(irg(2))%d.eq.j_0))then
			j_o(iout)%d=j_o(irg(1))%d/j_o(irg(2))%d
			!		diout=j_o(irg(1))%d(iel)/j_o(irg(2))%d(iel2)
		endif !if(any(j_o(irg(2))%d.eq.j_0))   4752
		case (16) !select case (ioper)
		do iel=1,nel
			j_o(iout)%d(iel)=min(j_o(irg(1))%d(iel),j_o(irg(2))%d(iel))
		enddo !iel=1,nel   4760
		case(17) !select case (ioper)
		do iel=1,nel
			j_o(iout)%d(iel)=max(j_o(irg(1))%d(iel),j_o(irg(2))%d(iel))
		enddo !iel=1,nel   4764
 
 
		case (18) !select case (ioper)
		do iel=1,nel
			j_o(iout)%d(iel)=dsign(j_o(irg(1))%d(iel),j_o(irg(2))%d(iel))
		enddo !iel=1,nel   4770
 
 
		end select !select case (ioper)
		! if(listout)then
		! if(j_otype(j_o(iout)%i2(ij)).ne.j_ipreal)call j_del(j_o(iout)%i2(ij))
		! j_v(j_o(iout)%i2(ij))=diout
		! else
		! j_o(iout)%d(ij)=diout
		! endif
		if(iscalar.ne.0)irg(iscalar)=ivfake
		return
 
 
 
 
 
	endif !if(matrix(1).and.matrix(2).or.vector(1).and.vector(2))   4645
	! write(16,*)'ioper,nrowout,ncolout,nel,irg(1),irg(2)',ioper,nrowout,ncolout,nel,irg(1),irg(2),&
	!j_o(irg(1))%i,j_o(irg(2))%i
 
	if(ivector.gt.0.and.imatrix.gt.0)then
		write(6,*)'ivector,imatrix,ncol(ivector),ncol(imatrix),nrow(ivector),nrow(imatrix)',&
			ivector,imatrix,ncol(ivector),ncol(imatrix),nrow(ivector),nrow(imatrix)
		if(ncol(ivector).ne.ncol(imatrix).and.nrow(ivector).ne.nrow(imatrix))goto 99
	endif !if(ivector.gt.0.and.imatrix.gt.0)   4793
	!	write(6,*)'rowstep,colstep',rowstep,colstep
	ij=1
	iel=1
	iel2=1
	!	write(6,*)'colstep ',colstep,'rowstep',rowstep,'nrowout,ncolout',nrowout,ncolout
	do i=1,nrowout
		do j=1,ncolout
			!		write(6,*)'i',i,'j',j,'iel ',iel,iel2
			select case (ioper)
			case (1 ) !select case (ioper)
			!	call j_getname(irg(1),irg(2))
			!	write(6,*)j_oname(1:j_loname),' d',j_o(irg(1))%d,j_oname2(1:j_loname2),j_o(irg(2))%d
			diout=j_o(irg(1))%d(iel)+j_o(irg(2))%d(iel2)
			case (2) !select case (ioper)
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
			case (15) !select case (ioper)
			if(j_o(irg(2))%d(iel2).eq.j_0)then
				write(6,*)'division with zero'
				j_err=.true.
			else !if(j_o(irg(2))%d(iel2).eq.j_0)then
				diout=j_o(irg(1))%d(iel)/j_o(irg(2))%d(iel2)
			endif !if(j_o(irg(2))%d(iel2).eq.j_0)   4854
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
			endif !if(listout)   4868
 
			ij=ij+1
			iel=iel+colstep(1)
			iel2=iel2+colstep(2)
			!	write(6,*)'i,j,ij,iel,iel2,',i,j,ij,iel,iel2,'colstep,rowstep',colstep,rowstep
 
 
		enddo !j=1,ncolout   4804
		iel=iel+rowstep(1)
 
		iel2=iel2+rowstep(2)
		!	write(6,*)'ROWi,j,ij,iel,iel2',i,j,ij,iel,iel2,',colstep,rowstep',colstep,rowstep
 
	enddo !i=1,nrowout   4803
	if(iscalar.ne.0)irg(iscalar)=ivfake
	!write(6,*)'scalr',iscalar
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
	call  j_startfunction(iob,io,0,narg,irg,iout)
 
	if(narg.ge.2)then
		call binop(iob,io,2)
		return
	endif !if(narg.ge.2)   4911
 
	!	if(narg.eq.1)then
 
	if(j_otype(irg(1)).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=-j_v(irg(1))
		return
	endif !if(j_otype(irg(1)).eq.j_ipreal)   4918
 
 
 
	select case(j_otype(irg(1)))
	case(j_ipmatrix) !select case(j_otype(irg(1)))
	nrow1=j_nrows(irg(1))  !j_o(irg(1))%i(1)
	ncol1=j_ncols(irg(1))  !j_o(irg(1))%i(2)
 
	if(irg(1).ne.iout)ivout=j_defmatrix8(iout,' ',nrow1,ncol1,j_matreg)
	j_o(iout)%d=-j_o(irg(1))%d(1:j_nelem(iout))   !j_o(iout)%i(3))
 
	case(j_iplist) !select case(j_otype(irg(1)))
	nrow=j_nrows(irg(1))  !j_o(irg(1))%i(1)
	if(j_otype(iout).eq.j_iplist)then
		write(6,*)'output cannot be list in output=-list'
		j_err=.true.
		return
	endif !if(j_otype(iout).eq.j_iplist)   4936
	ivout=j_defmatrix8(iout,' ',nrow1,j_18,j_matreg)
	j_o(iout)%d=j_v(j_o(irg(1))%i2(1:nrow1))
 
	case default !select case(j_otype(irg(1)))
	call j_printname('**object ',irg(1),' has illegal type in -matrix')
	write(6,*)'#type is:',j_objecttypes( j_otype(irg(1)))
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
				endif !if(j_o(irg1)%d(j).ne.val1)   4972
			enddo !j=2,j_o(irg1)%i(3)   4971
			j_v(iout)=j_1
			return
 
		elseif(j_otype(irg1).eq.j_iplist)then !if(j_otype(irg1).eq.j_ipmatrix)then
			val1=j_v( j_o(irg1)%i(1))
			do j=2,j_o(irg1)%i(1)
				if(j_v( j_o(irg1)%i2(j)).ne.val1)then
					j_v(iout)=j_0
					return
				endif !if(j_v( j_o(irg1)%i2(j)).ne.val1)   4984
			enddo !j=2,j_o(irg1)%i(1)   4983
			j_v(iout)=j_1
			return
		else !if(j_otype(irg1).eq.j_ipmatrix)then
			write(6,*)'If only one argument in EQ it must be matrix ot list'
			j_err=.true.
			return
		endif !if(j_otype(irg1).eq.j_ipmatrix)   4969
	endif !if(narg.eq.1)   4968
	irg2=j_o(iob)%i(io+3)
	if(j_v(irg1).eq.j_v(irg2))then
		j_v(iout)=j_1
	else !if(j_v(irg1).eq.j_v(irg2))then
		j_v(iout)=j_0
	endif !if(j_v(irg1).eq.j_v(irg2))   4998
 
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
	endif !if(j_v(irg1).ne.j_v(irg2))   5013
 
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
	endif !if(j_v(irg1).le.j_v(irg2))   5029
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
	endif !if(j_v(irg1).lt.j_v(irg2))   5043
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
	endif !if(j_v(irg1).ge.j_v(irg2))   5056
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
	endif !if(j_v(irg1).gt.j_v(irg2))   5069
end subroutine !subroutine GT(iob,io)
 
subroutine NOT(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	!	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_v(irg1).eq.0.d0)then
		j_v(iout)=j_1
	else !if(j_v(irg1).eq.0.d0)then
		j_v(iout)=j_0
	endif !if(j_v(irg1).eq.0.d0)   5082
end subroutine !subroutine NOT(iob,io)
 
subroutine AND(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_v(irg1).ne.0.d0.and.j_v(irg2).ne.0)then
		j_v(iout)=j_1
	else !if(j_v(irg1).ne.0.d0.and.j_v(irg2).ne.0)then
		j_v(iout)=j_0
	endif !if(j_v(irg1).ne.0.d0.and.j_v(irg2).ne.0)   5095
end subroutine !subroutine AND(iob,io)
 
subroutine OR(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(j_v(irg1).ne.0.d0.or.j_v(irg2).ne.0.d0)then
		j_v(iout)=j_1
	else !if(j_v(irg1).ne.0.d0.or.j_v(irg2).ne.0.d0)then
		j_v(iout)=0.d0
	endif !if(j_v(irg1).ne.0.d0.or.j_v(irg2).ne.0.d0)   5108
end subroutine !subroutine OR(iob,io)
 
subroutine EQV(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if((j_v(irg1).ne.0.d0.and.j_v(irg2).ne.0.d0).or.(j_v(irg1).eq.0.d0.and.j_v(irg2).eq.0.d0))then
		j_v(iout)=1.d0
	else !if((j_v(irg1).ne.0.d0.and.j_v(irg2).ne.0.d0).or.(j_v(irg1).eq.0.d0.and.j_v(irg2).eq.0.d0))then
		j_v(iout)=0.d0
	endif !if((j_v(irg1).ne.0.d0.and.j_v(irg2).ne.0.d0).or.(j_v(irg1)   5121
end subroutine !subroutine EQV(iob,io)
 
subroutine NEQV(iob,io)
	narg=j_o(iob)%i(io+1)
	irg1=j_o(iob)%i(io+2)
	irg2=j_o(iob)%i(io+3)
	iout=j_o(iob)%i(io+narg+2)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if((j_v(irg1).ne.0.d0.and.j_v(irg2).eq.0.d0).or.(j_v(irg1).eq.0.d0.and.j_v(irg2).ne.0.d0))then
		j_v(iout)=1.d0
	else !if((j_v(irg1).ne.0.d0.and.j_v(irg2).eq.0.d0).or.(j_v(irg1).eq.0.d0.and.j_v(irg2).ne.0.d0))then
		j_v(iout)=0.d0
	endif !if((j_v(irg1).ne.0.d0.and.j_v(irg2).eq.0.d0).or.(j_v(irg1)   5134
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
		endif !if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipreal   5148
		if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipmatrix)then
			if(j_o(irg1)%i(1).ne.j_o(irg2)%i(1).or.j_o(irg1)%i(2).ne.j_o(irg2)%i(2))then
				write(6,*)'incompatibe dimensions ',j_o(irg1)%i(1:2),j_o(irg2)%i(1:2)
				j_err=.true.;return
			endif !if(j_o(irg1)%i(1).ne.j_o(irg2)%i(1).or.j_o(irg1)%i(2).ne.j   5154
			if(irg1.ne.iout.and.irg2.ne.iout)ivout=j_defmatrix(iout,' ',j_o(irg2)%i(1),j_o(irg2)%i(2),j_matreg)
			do ii=1,j_o(irg2)%i(3)
				j_o(iout)%d(ii)=dmin1(j_o(irg1)%d(ii),j_o(irg2)%d(ii))
			enddo !ii=1,j_o(irg2)%i(3)   5159
			return
		endif !if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipma   5153
		if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipreal)then
			ir=irg1
			irg1=irg2
			irg2=ir
		endif !if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipre   5164
		!now irg2 is matrix
		!	write(6,*)'<66',irg1,irg2,j_otype(irg1),j_otype(irg2),j_ipmatrix
		if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipmatrix)then
 
			if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)then
				if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))then
					j_o(iout)%i(1)=j_o(irg2)%i(1)
					j_o(iout)%i(2)=j_o(irg2)%i(2)
				else !if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))then
					ivout=j_defmatrix(iout,' ',j_o(irg2)%i(1),j_o(irg2)%i(2),j_matreg)
				endif !if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))   5174
			else !if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)then
				ivout=j_defmatrix(iout,' ',j_o(irg2)%i(1),j_o(irg2)%i(2),j_matreg) !could do with one def
			endif !if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)   5173
			do ii=1,j_o(irg2)%i(3)
				j_o(iout)%d(ii)=dmin1(j_v(irg1),j_o(irg2)%d(ii))
			enddo !ii=1,j_o(irg2)%i(3)   5183
			return
 
		endif !if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipmatr   5171
 
		write(6,*)'illegal argument types ',j_objecttypes(j_otype(irg1)),j_objecttypes(j_otype(irg1))
		j_err=.true.;return
	endif !if(narg.eq.2)   5147
 
 
	if(j_otype(irg1).ne.j_ipmatrix)then
		write(6,*)'one argument must be MATRIX'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5195
	nrows=j_o(irg1)%i(1)
	ncols=j_o(irg1)%i(2)
	if(j_linkoption(iob,io,j_many).ge.0)then
		ncols=1
		call j_clearoption(iob,io)
	endif !if(j_linkoption(iob,io,j_many).ge.0)   5201
	if(nrows.eq.1.or.ncols.eq.1)then
 
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=minval(j_o(irg1)%d(1:j_o(irg1)%i(3)))
 
	else !if(nrows.eq.1.or.ncols.eq.1)then
		iout=j_defmatrix8(iout,' ',j_18,ncols,j_matreg)
		j_o(iout)%d=j_v(j_ivinf)
		ibas=0  !could use reshape
		do i=1,nrows
			do j=1,ncols
				j_o(iout)%d(j)=min(j_o(iout)%d(j),j_o(irg1)%d(ibas+j))
			enddo !j=1,ncols   5215
			ibas=ibas+ncols
		enddo !i=1,nrows   5214
 
	endif !if(nrows.eq.1.or.ncols.eq.1)   5205
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
		endif !if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipreal   5233
		if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipmatrix)then
			if(j_o(irg1)%i(1).ne.j_o(irg2)%i(1).or.j_o(irg1)%i(2).ne.j_o(irg2)%i(2))then
				write(6,*)'incompatibe dimensions ',j_o(irg1)%i(1:2),j_o(irg2)%i(1:2)
				j_err=.true.;return
			endif !if(j_o(irg1)%i(1).ne.j_o(irg2)%i(1).or.j_o(irg1)%i(2).ne.j   5239
			if(irg1.ne.iout.and.irg2.ne.iout)ivout=j_defmatrix(iout,' ',j_o(irg2)%i(1),j_o(irg2)%i(2),j_matreg)
			do ii=1,j_o(irg2)%i(3)
				j_o(iout)%d(ii)=dmax1(j_o(irg1)%d(ii),j_o(irg2)%d(ii))
			enddo !ii=1,j_o(irg2)%i(3)   5244
			return
		endif !if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipma   5238
		if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipreal)then
			ir=irg1
			irg1=irg2
			irg2=ir
		endif !if(j_otype(irg1).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipre   5249
		!now irg2 is matrix
		!	write(6,*)'<66',irg1,irg2,j_otype(irg1),j_otype(irg2),j_ipmatrix
		if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipmatrix)then
 
			if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)then
				if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))then
					j_o(iout)%i(1)=j_o(irg2)%i(1)
					j_o(iout)%i(2)=j_o(irg2)%i(2)
				else !if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))then
					ivout=j_defmatrix(iout,' ',j_o(irg2)%i(1),j_o(irg2)%i(2),j_matreg)
				endif !if(j_o(irg2)%i(3).eq.j_o(iout)%i(3))   5259
			else !if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)then
				ivout=j_defmatrix(iout,' ',j_o(irg2)%i(1),j_o(irg2)%i(2),j_matreg) !could do with one def
			endif !if(iout.ne.irg2.and.j_otype(iout).eq.j_ipmatrix)   5258
			do ii=1,j_o(irg2)%i(3)
				j_o(iout)%d(ii)=dmax1(j_v(irg1),j_o(irg2)%d(ii))
			enddo !ii=1,j_o(irg2)%i(3)   5268
			return
 
		endif !if(j_otype(irg1).eq.j_ipreal.and.j_otype(irg2).eq.j_ipmatr   5256
 
		write(6,*)'illegal argument types ',j_objecttypes(j_otype(irg1)),j_objecttypes(j_otype(irg1))
		j_err=.true.;return
	endif !if(narg.eq.2)   5232
 
	if(j_otype(irg1).ne.j_ipmatrix)then
		write(6,*)'one arguement must be MATRIX'
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5279
	nrows=j_o(irg1)%i(1)
	ncols=j_o(irg1)%i(2)
	if(j_linkoption(iob,io,j_many).ge.0)then
		ncols=j_18
		call j_clearoption(iob,io)
	endif !if(j_linkoption(iob,io,j_many).ge.0)   5285
	if(nrows.eq.1.or.ncols.eq.1)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=maxval(j_o(irg1)%d(1:j_o(irg1)%i(3)))
 
	else !if(nrows.eq.1.or.ncols.eq.1)then
		iout=j_defmatrix8(iout,' ',j_18,ncols,j_matreg)
		j_o(iout)%d=j_v(j_ivtolast)
		ibas=0  !could use reshape
		do i=1,nrows
			do j=1,ncols
				j_o(iout)%d(j)=dmax1(j_o(iout)%d(j),j_o(irg1)%d(ibas+j))
			enddo !j=1,ncols   5298
			ibas=ibas+ncols
		enddo !i=1,nrows   5297
 
	endif !if(nrows.eq.1.or.ncols.eq.1)   5289
	return
end subroutine !subroutine max_(iob,io)
 
subroutine minloc_(iob,io)
	! Section minloc minloc() Locations of the minimum values
	! minloc(MATRIX) generates a row vector containing the locations of the  minimum
	! values in each column. minloc(VECTOR) is the REAL scalar telling
	! the location of the minimum value. Thus the VECTOR can also be a row vector.
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
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5326
	nrows=j_nrows(irg1)  !j_o(irg1)%i(1)
	ncols=j_ncols(irg1) !j_o(irg1)%i(2)
	if(j_linkoption(iob,io,j_many,clear=.true.).ge.0)then
		ncols=1
		!	call j_clearoption(iob,io)
	endif !if(j_linkoption(iob,io,j_many,clear=.true.).ge.0)   5332
	if(nrows.eq.1.or.ncols.eq.1)then
 
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		loco=minloc(j_o(irg1)%d(1:j_o(irg1)%i(3)))
		j_v(iout)=loco(1)
	else !if(nrows.eq.1.or.ncols.eq.1)then
		iout=j_defmatrix8(iout,' ',j_18,ncols,j_matreg)
		minva=j_v(j_ivinf)
		loc=0
		!	write(6,*)'<888',nrows,ncols
		do i=1,ncols
			ibas=0  !could use reshape
			do j=1,nrows
 
				if(j_o(irg1)%d(ibas+i).lt.minva)then
					minva=j_o(irg1)%d(ibas+i)
					loc=j
				endif !if(j_o(irg1)%d(ibas+i).lt.minva)   5350
				ibas=ibas+ncols
			enddo !j=1,nrows   5348
			j_o(iout)%d(i)=loc
		enddo !i=1,ncols   5346
 
	endif !if(nrows.eq.1.or.ncols.eq.1)   5336
	return
 
end subroutine !subroutine minloc_(iob,io)
 
subroutine maxloc_(iob,io)
	! Section maxloc maxloc() Locations of the minimum values
	! maxloc(MATRIX) generates a row vector containing the locations of the  minimum
	! values in each column. maxloc(VECTOR) is the REAL scalar telling
	! the location of the maxim value whether VECTOR is a row vector or column vector.
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
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5383
	nrows=j_nrows(irg1)  !j_o(irg1)%i(1)
	ncols=j_ncols(irg1) !j_o(irg1)%i(2)
	if(j_linkoption(iob,io,j_many).ge.0)then
		ncols=j_18
		call j_clearoption(iob,io)
	endif !if(j_linkoption(iob,io,j_many).ge.0)   5389
	if(nrows.eq.1.or.ncols.eq.1)then
 
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		loco=maxloc(j_o(irg1)%d(1:j_o(irg1)%i(3)))
		j_v(iout)=loco(1)
 
	else !if(nrows.eq.1.or.ncols.eq.1)then
		iout=j_defmatrix8(iout,' ',j_18,ncols,j_matreg)
		minva=j_v(j_ivinf)
		loc=0
 
		do i=1,ncols
			ibas=0  !could use reshape
			do j=1,nrows
				if(j_o(irg1)%d(ibas+i).gt.maxva)then
					maxva=j_o(irg1)%d(ibas+i)
					loc=j
				endif !if(j_o(irg1)%d(ibas+i).gt.maxva)   5407
				ibas=ibas+ncols
			enddo !j=1,nrows   5406
			j_o(iout)%d(i)=loc
		enddo !i=1,ncols   5404
 
	endif !if(nrows.eq.1.or.ncols.eq.1)   5393
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
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5445
	nrows=j_o(irg1)%i(1)
	ncols=j_o(irg1)%i(2)
	nel=j_o(irg1)%i(3)
 
 
	iout=j_defmatrix8(iout,' ',nrows,ncols,j_matreg)
	cumsu=j_0
	if(nrows.eq.1.or.ncols.eq.1)then
 
		do i=1,nel
			cumsu=cumsu+j_o(irg1)%d(i)
			j_o(iout)%d(i)=cumsu
		enddo !i=1,nel   5458
 
	else !if(nrows.eq.1.or.ncols.eq.1)then
 
		j_o(iout)%d(1:ncols)=j_o(irg1)%d(1:ncols)
		ibas=nrows  !could use reshape
		ibasv=0
		do i=2,nrows
			j_o(iout)%d(ibas+1:ibas+ncols)=j_o(iout)%d(ibasv+1:ibasv+ncols)+&
				j_o(irg1)%d(ibas+1:ibas+ncols)
			ibasv=ibas
			ibas=ibas+ncols
 
		enddo !i=2,nrows   5468
	endif !if(nrows.eq.1.or.ncols.eq.1)   5456
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
		write(6,*)j_oname(1:j_loname),' is not MATRIX but ',j_objecttypes(j_otype(irg1))
		j_err=.true.;return
	endif !if(j_otype(irg1).ne.j_ipmatrix)   5498
	if(j_o(irg1)%i(1).ne.j_o(irg1)%i(2))then
		call j_getname(irg1)
		write(6,*)j_oname(1:j_loname),' is not square, it has dimensions ',j_o(irg1)%i(1),j_o(irg1)%i(2)
		j_err=.true.;return
	endif !if(j_o(irg1)%i(1).ne.j_o(irg1)%i(2))   5503
	ndim=j_o(irg1)%i(1)
	if(iout.ne.irg1)iout=j_defmatrix8(iout,' ',ndim,ndim,j_matreg)
	ibas=0
	do i=1,ndim
		if(j_o(irg1)%d(ibas+i).lt.j_0)then
			write(6,*)'diagonal ',i, 'has illegal value ',j_o(irg1)%d(ibas+i)
			j_err=.true.;return
 
		endif !if(j_o(irg1)%d(ibas+i).lt.j_0)   5512
		j_o(iout)%d(ibas+i)=sqrt(j_o(irg1)%d(ibas+i))
		ibas=ibas+ndim
	enddo !i=1,ndim   5511
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
 
				endif !if(j_dapu.ne.j_0)   5529
 
			endif !if(i.ne.j)   5527
			!		write(6,*)j_o(iout)%d(ibas+j)
		enddo !j=1,ndim   5525
 
		ibas=ibas+ndim
	enddo !i=1,ndim   5523
	if(j_linkoption(iob,io,j_msd,clear=.true.).lt.0)then
		ibas=0
		do i=1,ndim
			if(j_o(iout)%d(ibas+i).ne.j_0)j_o(iout)%d(ibas+i)=j_1
			ibas=ibas+ndim
 
		enddo !i=1,ndim   5545
	endif !if(j_linkoption(iob,io,j_msd,clear=.true.).lt.0)   5543
 
 
 
end subroutine
 
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
				endif !if(j_o(iout)%i(3).eq.j_o(irg)%i(3))   5579
			endif !if(j_otype(iout).eq.j_ipmatrix)   5578
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			ivout=j_defmatrix(iout,' ',j_o(irg)%i(1),j_o(irg)%i(2),j_matreg) !matreg?
		endif !if(iout.ne.irg)   5577
80		continue
		j_o(iout)%d=dabs(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   5572
 
 
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
	endif !if(j_otype(irg).eq.j_ipreal)   5614
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
	endif !if(j_otype(irg).eq.j_ipreal)   5635
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
	endif !if(j_otype(irg).eq.j_ipreal)   5656
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
	endif !if(j_otype(irg).eq.j_ipreal)   5677
end subroutine !subroutine floor_(iob,io)
 
subroutine UNIOP(iob,io,ioper)
	integer,intent(in)::iob,io
	double precision minva
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
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
			write(6,*)'negative argument ',j_v(irg)
			j_err=.true.
		else !if(j_v(irg).lt.j_0)then
			j_v(iout)=dsqrt(j_v(irg))
		endif !6 	if(j_v(irg).lt.j_0)   5722
		return
 
		!sqrt2
7 	if(j_v(irg).eq.j_0)then
			j_v(iout)=j_0
		elseif(j_v(irg).gt.j_0)Then !if(j_v(irg).eq.j_0)then
			j_v(iout)=sqrt(j_v(irg))
		else !if(j_v(irg).eq.j_0)then
			j_v(iout)=-sqrt(-j_v(irg))
		endif !7 	if(j_v(irg).eq.j_0)   5731
		return
		!
8 	if(j_v(irg).lt.0.d0)then
			write(6,*)'negative argument ',j_v(irg)
			j_err=.true.
		else !if(j_v(irg).lt.0.d0)then
			j_v(iout)=dlog(j_v(irg))
		endif !8 	if(j_v(irg).lt.0.d0)   5740
		return
		!abs
9 	if(j_v(irg).lt.0.d0)then
			write(6,*)'negative argument ',j_v(irg)
			j_err=.true.
		else !if(j_v(irg).lt.0.d0)then
			j_v(iout)=dlog10(j_v(irg))
		endif !9 	if(j_v(irg).lt.0.d0)   5748
		return
10	if(j_v(irg).gt.88.d0)then
			write(6,*)'too large argument ',j_v(irg)
			j_err=.true.
		else !if(j_v(irg).gt.88.d0)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=dexp(j_v(irg))
		endif !10	if(j_v(irg).gt.88.d0)   5755
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
		endif !17		if(dabs(j_v(irg)).lt.d-15)   5782
 
		j_v(iout)=dtan(1.d0/j_v(irg))
		return
18	if(dabs(j_v(irg)).lt.d-15)then
			write(6,*)'too close to zero ',j_v(irg)
			j_err=.true.
		endif !18	if(dabs(j_v(irg)).lt.d-15)   5789
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
		endif !25	if(abs(j_v(irg)).gt.0.d-15)   5811
 
26		if(abs(j_v(irg)).gt.0.d-15)then

			j_v(iout)=datan(1.d0/j_v(irg))*j_todeg  !cotand
 
		else !if(abs(j_v(irg)).gt.0.d-15)then
			write(6,*)'too small argument ',j_v(irg)
			j_err=.true.
		endif !26		if(abs(j_v(irg)).gt.0.d-15)   5820
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
 
 
 
 
		return
	endif !if(j_otype(irg).eq.j_ipreal)   5700
	if(j_otype(irg).ne.j_ipmatrix)then
		write(6,*)'argument is not MATRIX or REAL'
		j_err=.true.
 
	endif !if(j_otype(irg).ne.j_ipmatrix)   5847
	if(j_otype(iout).ne.j_ipmatrix)ivout= &
		j_defmatrix(iout,' ',j_o(irg)%i(1),j_o(irg)%i(2),j_matreg)
	if(j_o(iout)%i(3).ne.j_o(irg)%i(3))then
		ivout=j_defmatrix(iout,' ',j_o(irg)%i(1),j_o(irg)%i(2),j_matreg)
	else !if(j_o(iout)%i(3).ne.j_o(irg)%i(3))then
		j_o(iout)%i(1:2)=j_o(irg)%i(1:2)
	endif !if(j_o(iout)%i(3).ne.j_o(irg)%i(3))   5854
 
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
		write(6,*)'negative arguments, min= ',minva
		j_err=.true.
	else !if(minva.lt.j_0)then
		j_o(iout)%d=dsqrt(j_o(irg)%d)
	endif !if(minva.lt.j_0)   5879
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
		endif !if(j_o(irg)%d(i).eq.j_0)   5890
	end do !i=1,j_o(irg)%i(3)   5889
	return
	!
98 		minva=minval(j_o(irg)%d)
	if(minva.lt.0.d0)then
		write(6,*)'negative arguments, min= ',minva
		j_err=.true.
	else !if(minva.lt.0.d0)then
		j_o(iout)%d=dlog(j_o(irg)%d)
	endif !if(minva.lt.0.d0)   5901
	return
	!abs
99 		minva=minval(j_o(irg)%d)
	if(minva.lt.j_0)then
		write(6,*)'negative arguments, min= ',minva
		j_err=.true.
	else !if(minva.lt.j_0)then
		j_o(iout)%d=dlog10(j_o(irg)%d)
	endif !if(minva.lt.j_0)   5910
	return
910		minva=maxval(j_o(irg)%d)
	if(minva.gt.88.d0)then
		write(6,*)'too large arguments, max= ',minva
		j_err=.true.
	else !if(minva.gt.88.d0)then
		j_o(iout)%d=dexp(j_o(irg)%d)
	endif !if(minva.gt.88.d0)   5918
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
	endif !if(minva.lt.d-15)   5945
	j_o(iout)%d=dtan(1.d0/j_o(irg)%d)
	return
 
918		minva=minval(dabs(j_o(irg)%d))
	if(minva.lt.d-15)then
		write(6,*)'min absolute value too close to zero ',minva
		j_err=.true.
	else !if(minva.lt.d-15)then
		j_o(iout)%d=dtan(1.d0/(j_deg*j_o(irg)%d))
	endif !if(minva.lt.d-15)   5953
 
 
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
	endif !if(minva.gt.0.d-15)   5978
 
926			minva=minval(dabs(j_o(irg)%d))
	if(minva.gt.0.d-15)then
 
		j_o(iout)%d=datan(1.d0/j_o(irg)%d)*j_todeg  !cotand
 
	else !if(minva.gt.0.d-15)then
		write(6,*)'too small min absolute argument ',minva
		j_err=.true.
	endif !if(minva.gt.0.d-15)   5986
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
 
subroutine sqrt2(iob,io)
	! real,dimension(200)::rivi
	! real,dimension(40000)::blokki
	! real ::time1,time2
 
	! !		if(p)write(6,*)'<345irec2,nu',irec2,nu,lefi,j_v(j_ivdac)
	! !		if(p)write(6,*)j_filename(1:lefi)
	! open(unit=75,&
	! ACCESS='SEQUENTIAL',status='REPLACE',file='koeb.bin',form='UNFORMATTED',action='WRITE')
	! open(76,file='koed.bin',form='unformatted',access='direct',recl=4*200,&
	! status='REPLACE',iostat=ios,action='READWRITE')
 
 
	! do i=1,200*200
	! write(76,rec=i)rivi
	! enddo !do i=1,200*200
 
	! do i=1,200
	! write(75)blokki
	! enddo !do i=1,200
	! close(unit=75)
	! open(unit=75,ACCESS='SEQUENTIAL',status='OLD',file='koeb.bin',form='UNFORMATTED',action='READ')
	! !time1=time()
	! call cpu_time(time1)
	! do kie=1,500
	! n=0
	! do k=1,200
	! do i=1,200
	! n=n+1
	! read(76,rec=n)rivi
	! enddo !do i=1,200
	! enddo !do k=1,200
	! enddo !do kie=1,500
	! call cpu_time(time2)
	! !time2=time()
	! write(6,*)' da-time ',time2-time1
	! !time1=time()
	! call cpu_time(time1)
	! do kie=1,500
 
	! do k=1,200
 
	! read(75)blokki
	! enddo !do k=1,200
	! rewind(75)
	! enddo !do kie=1,500
	! !time2=time()
	! call cpu_time(time2)
	! write(6,*)' seqtime-time ',time2-time1
 
 
 
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	arg=j_v(j_o(iob)%i(io+2))
	if(arg.eq.0.)then
		j_v(iout)=0.
	elseif(arg.gt.0)Then !if(arg.eq.0.)then
		j_v(iout)=sqrt(arg)
	else !if(arg.eq.0.)then
		j_v(iout)=-sqrt(-arg)
	endif !if(arg.eq.0.)   6073
 
	return
end subroutine sqrt2 !subroutine sqrt2(iob,io)
 
subroutine log_(iob,io)
	integer,intent(in)::iob,io
	double precision minva
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		if(j_v(irg).lt.0.d0)then
			write(6,*)'negative argument ',j_v(irg)
			j_err=.true.
		else !if(j_v(irg).lt.0.d0)then
			j_v(iout)=dlog(j_v(irg))
		endif !if(j_v(irg).lt.0.d0)   6093
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		minva=minval(j_o(irg)%d)
		if(minva.lt.0.d0)then
			write(6,*)'negative elements,min = ',minva
			j_err=.true.
		else !if(minva.lt.0.d0)then
			call j_del(iout)
			j_otype(iout)=j_ipmatrix
			allocate( j_o(iout)%i(1:13))
			j_o(iout)%i=j_o(irg)%i
			allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
			j_o(iout)%d=dlog(j_o(irg)%d)
		endif !if(minva.lt.0.d0)   6101
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6091
end subroutine !subroutine log_(iob,io)
 
subroutine log10_(iob,io)
	integer,intent(in)::iob,io
	double precision minva
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		if(j_v(irg).lt.0.d0)then
			write(6,*)'negative argument ',j_v(irg)
			j_err=.true.
		else !if(j_v(irg).lt.0.d0)then
			j_v(iout)=dlog10(j_v(irg))
		endif !if(j_v(irg).lt.0.d0)   6127
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		minva=minval(j_o(irg)%d)
		if(minva.lt.0.d0)then
			write(6,*)'negative elements, min=',minva
			j_err=.true.
		else !if(minva.lt.0.d0)then
			call j_del(iout)
			j_otype(iout)=j_ipmatrix
			allocate( j_o(iout)%i(1:13))
			j_o(iout)%i=j_o(irg)%i
			allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
			j_o(iout)%d=dlog10(j_o(irg)%d)
		endif !if(minva.lt.0.d0)   6135
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6125
end subroutine !subroutine log10_(iob,io)
 
subroutine exp_(iob,io)
	integer,intent(in)::iob,io
	double precision minva
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_v(irg).gt.88.d0)then
			write(6,*)'too large argument ',j_v(irg)
			j_err=.true.
		else !if(j_v(irg).gt.88.d0)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=dexp(j_v(irg))
		endif !if(j_v(irg).gt.88.d0)   6160
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		minva=maxval(j_o(irg)%d)
		if(minva.lt.0.d0)then
			write(6,*)'too large elements,max = ',minva
			j_err=.true.
		else !if(minva.lt.0.d0)then
			call j_del(iout)
			j_otype(iout)=j_ipmatrix
			allocate( j_o(iout)%i(1:13))
			j_o(iout)%i=j_o(irg)%i
			allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
			j_o(iout)%d=dexp(j_o(irg)%d)
		endif !if(minva.lt.0.d0)   6169
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6159
end subroutine !subroutine exp_(iob,io)
 
subroutine sin_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	!	write(6,*)'irg,j_otype(ir)',j_otype(ir),j_ipreal
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dsin(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dsin(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6193
end subroutine !subroutine sin_(iob,io)
 
subroutine sind_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dsin(j_deg*j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dsin(j_deg*j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6215
end subroutine !subroutine sind_(iob,io)
 
subroutine cos_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dcos(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dcos(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6237
end subroutine !subroutine cos_(iob,io)
 
subroutine cosd_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dcos(j_v(irg)*j_deg)
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dcos(j_deg*j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6259
end subroutine !subroutine cosd_(iob,io)
 
subroutine tan_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dtan(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dtan(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6281
end subroutine !subroutine tan_(iob,io)
 
subroutine tand_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dtan(j_deg*j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dtan(j_deg*j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6303
end subroutine !subroutine tand_(iob,io)
 
subroutine cotan_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(dabs(j_v(irg)).lt.d-15)then
			write(6,*)'too close to zero ',j_v(irg)
			j_err=.true.
		endif !if(dabs(j_v(irg)).lt.d-15)   6326
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dtan(1.d0/j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dsin(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6325
end subroutine !subroutine cotan_(iob,io)
 
subroutine cotand_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(dabs(j_v(irg)).lt.d-15)then
			write(6,*)'too close to zero ',j_v(irg)
			j_err=.true.
		endif !if(dabs(j_v(irg)).lt.d-15)   6352
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dtan(1.d0/(j_deg*j_v(irg)))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dtan(1.d0/(j_deg*j_o(irg)%d))
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6351
end subroutine !subroutine cotand_(iob,io)
 
subroutine asin_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dasin(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dasin(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6377
end subroutine !subroutine asin_(iob,io)
 
subroutine asind_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dasin(j_v(irg))*j_todeg
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dasin(j_o(irg)%d)*j_todeg
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6399
end subroutine !subroutine asind_(iob,io)
 
subroutine acos_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dacos(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dacos(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6421
end subroutine !subroutine acos_(iob,io)
 
subroutine acosd_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dacos(j_v(irg))*j_todeg
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dacos(j_o(irg)%d)*j_todeg
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6443
end subroutine !subroutine acosd_(iob,io)
 
subroutine atan_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=datan(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=datan(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6465
end subroutine !subroutine atan_(iob,io)
 
subroutine atand_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=datan(j_v(irg))*j_todeg
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=datan(j_o(irg)%d)*j_todeg
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6487
end subroutine !subroutine atand_(iob,io)
 
subroutine acotan_(iob,io)
	integer,intent(in)::iob,io
	double precision minva
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(abs(j_v(irg)).gt.0.d-15)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=datan(1.d0/j_v(irg))
 
		else !if(abs(j_v(irg)).gt.0.d-15)then
			write(6,*)'too small argument ',j_v(irg)
			j_err=.true.
		endif !if(abs(j_v(irg)).gt.0.d-15)   6511
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
 
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=abs(j_o(irg)%d)
		minva=minval(j_o(iout)%d)
		if(minva.lt.1.d-15)then
			write(6,*)'too small absolute values, smallest ',minva
			j_err=.true.
			return
		endif !if(minva.lt.1.d-15)   6528
		j_o(iout)%d=datan(1.d0/j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6510
end subroutine !subroutine acotan_(iob,io)
 
subroutine acotand_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		if(abs(j_v(irg)).gt.0.d-15)then
 
			j_v(iout)=datan(1.d0/j_v(irg))*j_todeg
 
		else !if(abs(j_v(irg)).gt.0.d-15)then
			write(6,*)'too small argument ',j_v(irg)
			j_err=.true.
		endif !if(abs(j_v(irg)).gt.0.d-15)   6548
		j_v(iout)=dacos(j_v(irg))*j_todeg
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=abs(j_o(irg)%d)
		minva=minval(j_o(iout)%d)
		if(minva.lt.1.d-15)then
			write(6,*)'too small absolute values, smallest ',minva
			j_err=.true.
			return
		endif !if(minva.lt.1.d-15)   6565
		j_o(iout)%d=datan(1.d0/j_o(irg)%d)*j_todeg
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6546
end subroutine !subroutine acotand_(iob,io)
 
subroutine sinh_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dsinh(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dsinh(j_o(irg)%d)*j_todeg
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6583
end subroutine !subroutine sinh_(iob,io)
 
subroutine cosh_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dcosh(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dcosh(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6605
end subroutine !subroutine cosh_(iob,io)
 
subroutine tanh_(iob,io)
	integer,intent(in)::iob,io
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(irg).eq.j_ipreal)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=dcosh(j_v(irg))
	elseif(j_otype(irg).eq.j_ipmatrix)then !if(j_otype(irg).eq.j_ipreal)then
		call j_del(iout)
		j_otype(iout)=j_ipmatrix
		allocate( j_o(iout)%i(1:13))
		j_o(iout)%i=j_o(irg)%i
		allocate( j_o(iout)%d(1:j_o(iout)%i(3)))
		j_o(iout)%d=dcosh(j_o(irg)%d)
	else !if(j_otype(irg).eq.j_ipreal)then
		write(6,*)'illegal argument'
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal)   6627
end subroutine !subroutine tanh_(iob,io)
 
 
 
 
 
 
 
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
 
! subroutine matrixstat(iob,io)
! integer, dimension(:), pointer :: arg !arguments of the function
! integer, dimension(:), pointer :: rows !arguments of the function
! integer, dimension(:), pointer :: cols !arguments of the function
! double precision sum,sum2,dn,var,am
 
! ! j_optarg0
! call j_startfunction(iob,io,j_ipmatrix,.true.,narg,arg,ivout)
! if(j_err)return
! !j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
! call j_getoption(iob,io,j_mrow,-1,2,j_ipreal,.true.,noprows,rows)
! call j_getoption(iob,io,j_mcolumn,-1,2,j_ipreal,.true.,nopcols,cols)
! call j_getoption(iob,io,j_msum,-1,0,0,isum,j_optarg0)
! if(isum.ge.0)call j_getobject(ivout,'%sum',j_ipreal,ivsum)
! call j_getoption(iob,io,j_msd,-1,0,0,.false.,isd,j_optarg0)
! if(isd.ge.0)call j_getobject(ivout,'%sd',j_ipreal,ivsd)
! call j_getoption(iob,io,j_mvar,-1,0,0,.false.,ivar,j_optarg0)
! call j_clearoption(iob,io)  ! subroutine
 
! if(ivar.ge.0)call j_getobject(ivout,'%var',j_ipreal,ivvar)
! call j_getoption(iob,io,j_mmin,-1,0,0,.false.,imin,j_optarg0)
! call j_getobject(ivout,'%mean',j_ipreal,ivmean)
! if(imin.ge.0)then
! call j_getobject(ivout,'%min',j_ipreal,ivmin)
! j_v(ivmin)=1.7e37
! endif
! call j_getoption(iob,io,j_mmax,-1,0,0,.false.,imax,j_optarg0)
! if(imax.ge.0)then
! call j_getobject(ivout,'%max',j_ipreal,ivmax)
! j_v(ivmax)=-1.7d37
! endif
! nrows=j_o(arg(1))%i(1)
! ncols=j_o(arg(1))%i(2)
! !	j_o(ivout)%i(1)=ndim1;   j_o(ivout)%i(2)=ndim2
! ir1=1
! ir2=nrows
! if(noprows.gt.0)then
! ir1=j_v(rows(1))
! ir2=ir1
! if(noprows.gt.1)ir2=min(nrows,-nint(j_v(rows(2))))
! endif
! ic1=1
! ic2=ncols
! if(nopcols.gt.0)then
! ic1=j_v(cols(1))
! ic2=ic1
! if(nopcols.gt.1)ic2=min(ncols,-nint(j_v(cols(2))))
! endif
! if(ir1.le.0.or.ir2.le.0.or.ic1.le.0.or.ic2.le.0.or.ir2.lt.ir1.or.ic2.lt.ic1)then
!write(6,*)'*illegal row-> or column->'
! j_err=.true.
! return
! endif
 
! n=0
! sum=0.d0
! sum2=0.d0
! do ir=ir1,ir2
! do ic=ic1,ic2
! n=n+1
! if(j_o(arg(1))%i(13).ne.0)then
! am=j_o(arg(1))%r((ir-1)*ncols+ic)
! else
! am=j_o(arg(1))%d((ir-1)*ncols+ic)
! endif
! sum=sum+am
! sum2=sum2+am*am
! if(imin.ge.0)j_v(ivmin)=min(j_v(ivmin),am)
! if(imax.ge.0)j_v(ivmax)=max(j_v(ivmax),am)
 
! enddo
! enddo
! dn=n
! j_v(ivmean)=sum/dn
! if(ivar.ge.0.or.isd.ge.0)then
! var=(sum2-sum*sum/dn)/(dn-1.d0)
! if(ivar.ge.0)j_v(ivvar)=var
! if(isd.ge.0)j_v(ivsd)=sqrt(var)
! endif
! if(isum.ge.0)j_v(ivsum)=sum
! return
 
! end subroutine matrixstat
 
subroutine index_v(iob,io)
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
	endif !if(j_otype(arg(2)).ne.j_iplist)   6790
	inde=j_v(arg(1))
	if(inde.lt.1.or.inde.gt.j_o(arg(2))%i(1))then
		write(6,*)'*illegal index ',inde, 'for list with len=',j_o(arg(2))%i(1)
		j_err=.true.
	else !if(inde.lt.1.or.inde.gt.j_o(arg(2))%i(1))then
		!narg=j_o(iob)%i(io+1)  !note out put is not put into the output variable
		!ivout=j_o(iob)%i(io+2+narg)
		j_o(iob)%i(io+j_o(iob)%i(io+1)+2)=j_o(arg(2))%i2(inde)
	endif !if(inde.lt.1.or.inde.gt.j_o(arg(2))%i(1))   6796
	return
 
end subroutine !subroutine index_v(iob,io)
 
 
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
	! ** Example of find, illustrating also rann() (line   6426 file c:/j3/j.f90)
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
	endif !if(noprows.ge.0.or.nopcols.ge.0)   6869
	!write(6,*)'infind'
	link=j_codelink(iob,io,j_mfilter)
	! if(j_linkopt2(j_mfilter).gt.0)then
 
	! iofilter=j_linkopt2(j_mfilter)
	! dofilter=iofilter.ne.0
	! ivfilter=j_o(iob)%i(j_linkopt2(j_mfilter)-1)
	! write(6,*)'iofilter,ivfilter',iofilter,ivfilter
	if(link.le.0)then !if(j_linkoption(iob,io,j_mfilter).gt.0)then
		write(6,*)'* filter-> missing'
		j_err=.true. ;return
	end if !if(link.le.0)   6882
	!j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
	call j_getoption(iob,io,j_many,-1,0,0,.false.,nany,j_optarg0)
	if(j_err)return
	isany=nany.ge.0
	call j_getoption(iob,io,j_mexpand,-1,0,0,.false.,nexpand,j_optarg0)
	if(j_err)return
	isexpand=nexpand.ge.0
	call j_clearoption(iob,io)  ! subroutine
 
 
	nrows=j_o(arg(1))%i(1)
	ncols=j_o(arg(1))%i(2)
	nel=j_o(arg(1))%i(3)
	if(isexpand)then
		ive=j_defmatrix8(ivout,' ',nrows,ncols,j_matreg)
	elseif(isany)then !if(isexpand)then
		allocate(found(1:nel))
	else
		if(j_otype(ivout).ne.j_ipreal)call j_del(ivout)
	endif !if(isexpand)   6899
	nfound=0
	!	if(nrows.eq.1.or.ncols.eq.1)then
	do i=1,nel
		j_v(j_ivdollar)=j_o(arg(1))%d(i)
 
 
		filter=j_codevalue(iob,link)
		!	write(6,*)'<77 ',j_v(j_ivdollar),filter
		if (j_err)then
			if(allocated(found))deallocate(found)
			return
		endif !if (j_err)   6914
		if(filter.ne.j_0)then
			if(isexpand)then
				j_o(ivout)%d(i)=j_1
			elseif(isany)then !if(isexpand)then
				nfound=nfound+1
				found(nfound)=i
			else !if(isexpand)then
				j_v(ivout)=i
				return
			endif !if(isexpand)   6919
		endif !if(filter.ne.j_0)   6918
	enddo !i=1,nel   6908
	if(isexpand)return
	if(isany)then
		if(nfound.gt.0)then
			ive=j_defmatrix8(ivout,' ',nfound,j_18,j_matreg)
			j_o(ivout)%d=found(1:nfound)
		else !if(nfound.gt.0)then
			write(6,*)'*wrn* nothing found, output is REAL zero'
			j_v(ivout)=j_0
		endif !if(nfound.gt.0)   6932
		deallocate(found)
	else !if(isany)then
		j_v(ivout)=0.d0
	endif !if(isany)   6931
	return
 
end subroutine find !subroutine find(iob,io)
 
subroutine matrixstat(iob,io,ifunc)
 
	! Section matrixstat Statistical functions for matrices
	! Functions mean(), sd(), var(), sum(), min() and max()
	! can be used used to compute stastics from a matrix. Let mean() here present
	! any of thes functions. The following rules apply:
	! \begin{itemize}
	!   mean(VECTOR) computes the mean of the vector, output is REAL
	!   mean(MATRIX) computes the mean of the each column. Result is row vector.
	!   mean(VECTOR,weight->wvector) computes the
	! weigted mean of the vector,weights being in vector wvector.
	!   mean(MATRIX,weight->wvector) computes the
	! weighted mean of each column, weights being in vector wvector, result is row vector.
 
	! \end{itemize}
	! endsection
 
	! Section mean mean() Means or weighted means
	! See section matrixstat for details
	! endsection
 
	! Section sd sd() Sd's or weighted sd's
	! See section matrixstat for details
	! endsection
 
	! Section var var() Sample variances or weighted variances
	! See section matrixstat for details
	! endsection
 
	! Section sum sum() Sums or weighted sums
	! See section matrixstat for details
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
	!ifunc=1  =>mean
	!ifunc=2  =>sum
	!ifunc=3  =>var
	!ifunc=4  =>sd
	! j_optarg0
	call j_startfunction(iob,io,j_ipmatrix,narg,arg,ivout)
	if(j_err)return
	!j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
 
	need2=ifunc.ge.3
	imat=arg(1)
	nrows=j_nrows(imat)  !j_o(imat)%i(1)
	ncols=j_ncols(imat)  !j_o(imat)%i(2)
	nel=j_nelem(imat)  !j_o(imat)%i(3)
 
	!	return
	call j_getoption(iob,io,j_mfreq,-1,1,j_ipmatrix,.true.,noptarg,j_optarg0)
	if(j_err)return
	!write(6,*)'noptarg',noptarg,j_optarg0(1)
	if(j_linkoption(iob,io,j_many).ge.0)then
		ncols=j_18
 
 
	endif !if(j_linkoption(iob,io,j_many).ge.0)   7008
	call j_clearoption(iob,io)
 
	! j_getoption(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
 
 
	if(noptarg.gt.0)then
		iwe=j_optarg0(1)
 
		if(ncols.eq.1.or.nrows.eq.1)then
			if(j_o(iwe)%i(3).ne.nel)then
				call j_getname(imat,iwe)
				write(6,*)'weight ',j_oname2(1:j_loname2), 'does not have same size as ',&
					j_oname(1:j_loname)
				j_err=.true. ; return
			endif !if(j_o(iwe)%i(3).ne.nel)   7022
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
				endif !if(ifunc.ge.3)   7038
				return
			endif !if(need2)   7035
 
			if(ifunc.eq.1)then
				j_v(ivout)=mean
			else !if(ifunc.eq.1)then
				j_v(ivout)=suma
 
			endif !if(ifunc.eq.1)   7046
			return
 
		endif !if(ncols.eq.1.or.nrows.eq.1)   7021
		if(j_o(iwe)%i(1).ne.1.and.j_o(iwe)%i(2).ne.1)then
			call j_getname(iwe)
			write(6,*)'weight ',j_oname(1:j_loname),' is not vector but ',j_o(iwe)%i(1:2)
			j_err=.true.
			return
		endif !if(j_o(iwe)%i(1).ne.1.and.j_o(iwe)%i(2).ne.1)   7055
		ivout=j_defmatrix8(ivout,' ',j_18,ncols,j_matreg)
 
 
		ibas=0
		do i=1,nrows
 
			j_o(ivout)%d=j_o(ivout)%d+j_o(iwe)%d(i)*j_o(imat)%d(ibas+1:ibas+ncols)
			ibas=ibas+ncols
		end do !i=1,nrows   7065
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
		end do !i=1,nrows   7078
		j_o(ivout)%d=sum2/wsum
		deallocate(sum2)
		if(ifunc.eq.3)return
		j_o(ivout)%d=sqrt(j_o(ivout)%d)
		return
 
		!weighted
 
	endif !if(noptarg.gt.0)   7018
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
			enddo !i=1,nel   7102
			suma2=dcoef1*suma2
			if(ifunc.eq.3)then
				j_v(ivout)=suma2
			else !if(ifunc.eq.3)then
				j_v(ivout)=sqrt(suma2)
			endif !if(ifunc.eq.3)   7106
			return
		endif !if(need2)   7099
 
		if(ifunc.eq.1)then
			j_v(ivout)=mean
		else !if(ifunc.eq.1)then
			j_v(ivout)=suma
 
		endif !if(ifunc.eq.1)   7114
		return
 
	endif !if(ncols.eq.1.or.nrows.eq.1)   7093
 
	ivout=j_defmatrix8(ivout,' ',j_18,ncols,j_matreg)
	dcoef=j_1/nrows
	dcoef1=j_1/(nrows-j_1)
 
	ibas=0
	do i=1,nrows
 
		j_o(ivout)%d=j_o(ivout)%d+j_o(imat)%d(ibas+1:ibas+ncols)
		ibas=ibas+ncols
	end do !i=1,nrows   7129
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
	end do !i=1,nrows   7141
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
	endif !if(xmax.le.xmin)   7188
	if(j_v(arg(1)).lt.xmin.or.j_v(arg(1)).gt.xmax)then
		j_v(ivout)=0.d0
		return
	endif !if(j_v(arg(1)).lt.xmin.or.j_v(arg(1)).gt.xmax)   7193
 
	call j_getoption(iob,io,j_mdx,-1,1,j_ipreal,.true.,ndx,j_optarg0)
	if(j_err)return
	if(ndx.eq.1)then
		dx=j_v(j_optarg0(1))
		if(dx.le.0.d0)then
			write(6,*)'illegal dx->'
			j_err=.true.
			return
		endif !if(dx.le.0.d0)   7202
	endif !if(ndx.eq.1)   7200
 
	call j_getoption(iob,io,j_mclasses,-1,1,j_ipreal,.true.,nc,j_optarg0)
	if(j_err)return
	if(nc.eq.1)clasn=j_v(j_optarg0(1))
	if(nc.gt.0.and.ndx.gt.0)then
		write(6,*)'both dx-> and classes->, dx dominates'
		nc=0
	endif !if(nc.gt.0.and.ndx.gt.0)   7212
	call j_clearoption(iob,io) ! subroutine
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
	integer*8::nob,nkeep,ncols,iobs,ibas,ibas2,ibas1,iobs1,nel,ntot,nobstot
	call j_startfunction(iob,io,0,narg,arg,iout)
 
	if(j_err)return
	call j_getobject(iout,'%obs',j_ipreal,ivobs)
	ndata=0
	! isup=.false.
	! do i=1,narg
	! if(j_otype(arg(i)).eq.j_ipdata)then
	! ndata=ndata+1
	! if(j_o(arg(i))%i(5).ne.0)isup=.true.
	! endif !if(j_otype(arg(i)).eq.j_ipdata)   7176
	isdelete=j_isopt(iob,io,j_mdelete,.false.)
	if(isdelete.and.narg.le.2)then
		write(6,*)'with only ',narg, 'arguments delete-> is ignored'
		isdelete=.false.
 
	endif !if(isdelete.and.narg.le.2)   7302
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
	istime=j_isopt(iob,io,j_mtime)
	if(istime)then
		call cpu_time(cpu0)
		time0=secnds(0.)
	endif !if(istime)   7322
	call j_getoption_index(iob,io,j_mextra,-1,999,j_ipreal,&
		.true.,nextra,extra)
	if(nextra.lt.0)nextra=0
	call j_getoption_index(iob,io,j_mmaketrans,-1,1,j_iptrans,.true.,noptarg,j_optarg)
	!	write(6,*)'makenop',noptarg,j_optarg0(1)
	if(noptarg.gt.0)then
		maketran=.true.
		ivmaketrans=j_optarg(1)
		ivoul=j_trans_output(ivmaketrans)
		noutv=j_o(ivoul)%i(1)
		!	write(6,*)'<4554',noutv,ivoul,j_o(ivoul)%i2(1:noutv)
	else !if(noptarg.gt.0)then
		maketran=.false.
		noutv=0
	end if !if(noptarg.gt.0)   7331
	if(isdelete.and.maketran)then
		write(6,*)'with delete-> maketrans-> is not allowed'
		j_err=.true.; return
	endif !if(isdelete.and.maketran)   7341
	call	j_getoption_index(iob,io,j_mread,-1,999999,j_ipreal,.true.,nread,readv)
 
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
					!					call	j_getoption_index(iob,io,j_mread,ncols,ncols,j_ipreal,.true.,noptarg,j_optarg0)
					if(nread.ne.ncols)then
						call j_getname(arg(i))
						write(6,*)'*matrix ',j_oname(1:j_loname),' has ',ncols,' columns but read-> has ',nread,' arguments'
						j_err=.true.;return
					endif !if(nread.ne.ncols)   7369
					if(j_err)return
 
				elseif(j_otype(arg(1)).ne.j_ipmatrix)then
					write(6,*)'*the first argument is DATA ',&
						' but arg ', i,' is MATRIX, with append-> all arguments must be either DATA or MATRIX'
					j_err=.true.;return
				endif !if(i.eq.1)   7366
				ismatrix=.true.
				arg2(i)=ivmat		!arg2 is matrix
 
				if(ncols.ne.j_o(ivmat)%i(2))then
					write(6,*)'arg ',i,' has ', ncols, 'columns,but first matrix ',j_o(arg(1))%i(2),&
						' with append-> all matrices must have the same number of columns'
					j_err=.true.;return
				endif !if(ncols.ne.j_o(ivmat)%i(2))   7384
			elseif(j_otype(arg(i)).eq.j_ipdata)then !if(j_otype(arg(i)).eq.j_ipmatrix)then
 
				ivmat=j_o(arg(i))%i(1)
				arg2(i)=ivmat
				if(i.eq.1)then
					ncols=j_o(ivmat)%i(2)
					ivkeep1=j_o(arg(1))%i(2)
				else
					if(j_otype(arg(1)).ne.j_ipdata)then
						write(6,*)'*the first argument is MATRIX but argument ',i,&
							' is DATA with append-> all arguments must be either DATA or MATRIX'
						call j_getname(arg(i))
						write(6,*)'hint: take the matrix of data with ',j_oname(1:j_loname),'%matrix'
						j_err=.true.;return
					endif !if(j_otype(arg(1)).ne.j_ipdata)   7397
					if(j_o(ivmat)%i(2).ne.ncols)then
						write(6,*)'arg ',i,' has ',j_o(ivmat)%i(2),' vars but first matrix ',ncols, &
							' with append-> all matrices mus have the same number of colums'
						j_err=.true.;return
					endif !if(j_o(ivmat)%i(2).ne.ncols)   7404
					ivkeep=j_o(arg(1))%i(2)
					if(.not.all(j_o(ivkeep1)%i2(1:ncols).eq.j_o(ivkeep)%i2(1:ncols)))then
						write(6,*)'*arg ',i, ' does not have same keep-variables as the previous args'
						write(6,*)'hint: take matrices using ..%matrix and define names with read->'
						j_err=.true.;return
					endif !if(.not.all(j_o(ivkeep1)%i2(1:ncols).eq.j_o(ivkeep)%i2(1:n   7410
					arg2(i)=ivmat
				endif !if(i.eq.1)   7393
 
				!write(6,*)'77',arg2(i)
			else !if(j_otype(arg(i)).eq.j_ipmatrix)then
				call j_printname('argument ',arg(i), ' is not DATA or MATRIX')
				j_err=.true.
				return
			endif !if(j_otype(arg(i)).eq.j_ipmatrix)   7364
			nob=nob+j_nrows(ivmat)  !j_o(ivmat)%i(1)
 
			!	call j_getname(arg(i))
			!			write(6,*)'<666 ',i,nvar0(i),nvar,j_oname(1:j_loname),nob
		enddo !i=1,narg   7363
		!	write(6,*)'nob',nob
 
		j_filter=j_codelink(iob,io,j_mfilter).ne.0
 
		! if(j_linkoption(iob,io,j_mfilter).gt.0)then
		! !	write(6,*)'<1po34',j_linkoption(iob,io,j_mfilter)
		! j_filter=.true.
		! j_iofilter=j_linkopt2(j_mfilter)
		! j_ivfilter=j_o(iob)%i(j_linkoption(iob,io,j_mfilter)+1)
		! !write(6,*)'iofilter,ivfilter',iofilter,ivfilter
		! else !if(j_linkoption(iob,io,j_mfilter).gt.0)then
		! j_filter=.false.
		! endif !if(j_linkoption(iob,io,j_mfilter).gt.0)then
 
		j_reject=j_codelink(iob,io,j_mreject).ne.0
		if(j_reject.or.j_filter)then
			write(6,*)'*filter-> and reject-> do not work with append->'
			j_err=.true.;return
 
		endif !if(j_reject.or.j_filter)   7444
 
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
 
 
 
		call j_clearoption(iob,io)  ! subroutine
 
		nvartot=ncols+noutv+nextra
 
		!	allocate(vars(1:nvar))
		!write(6,*)'<4664,nvartot,nvar,noutv,nextra,ivkeep',nvartot,nvar,noutv,nextra
		call j_deflistobject(iout,'%keep',ivkeep,nres=nvartot)
		!		write(6,*)'<55775 ',ncols,noutv,nextra,nvartot,j_o(ivkeep)%i(1)
		nr=0
		nv=0
		if(ismatrix)then
			j_o(ivkeep)%i2(1:ncols)=j_optarg0(1:ncols)
		else
			j_o(ivkeep)%i2(1:ncols)=j_o( j_o(arg(1))%i(2))%i2(1:ncols)
		endif !if(ismatrix)   7473
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
				endif !if(iper.gt.0)   7485
			enddo !ij=1,nextra   7483
		endif !if(nextra.gt.0)   7482
		if(j_o(ivkeep)%i(1).lt.nvartot)then
			!write(6,*)'<555 ',ncols,noutv,nextra,nvartot,j_o(ivkeep)%i(1)
			write(6,*)'There are duplicated variable names'
			j_err=.true.; return
		endif !if(j_o(ivkeep)%i(1).lt.nvartot)   7492
 
		nkeep=j_o(ivkeep)%i(1)
		itemp=1  !stored in tempv
		!	write(6,*)'nkeep',nkeep
		!		write(6,*)'<new nkeep,nob,nvartot,nextra,noutv,nvar ',nkeep,nob,nvartot,nextra,noutv,nvar
		!		write(6,*)'ncols ',ncols
 
		!	ivmat=j_defmatrix8(iout,'%matrix',nob,nkeep,j_matreg)
 
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
				enddo !i=1,narg   7517
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
					endif !if(itemp.eq.1)   7541
					call j_getname(arg2(i))
					write(6,*)j_oname(1:j_loname),' appended, obs ',nobstot
				enddo !i=3,narg   7540
			endif !if(.not.isdelete)   7512
		else
 
			if(allocated(j_tempvector))deallocate(j_tempvector)
			allocate(j_tempvector(1:nob*nkeep))
 
			ibas=0
			!		nrejected=0
			!write(6,*)'hep0'
			iobs=0
			do i=1,narg
				!write(6,*)'hep0',i,arg2(i)
				ibas2=0
				!	if(iobs.eq.0.and.j_otype(arg(i)).eq.j_ipdata)
				do k=1,j_o(arg2(i))%i(1)
					if(maketran)then
						if(arg(i).eq.arg2(i))then
							j_v(j_o(ivkeep)%i2(1:ncols))=j_o(arg2(i))%d(ibas2+1:ibas2+ncols)
 
						else
							iobs1=iobs+j_18
							call j_getobs(iobs1)
						endif !if(arg(i).eq.arg2(i))   7587
 
						!			j_v(j_ivrecord)=j
						j_v(ivobs)=iobs+j_18
						!write(6,*)iobs+1,'in',j_o(arg2(i))%d(ibas2+1:ibas2+ncols)
						!	if(maketran)then
						call dotrans(ivmaketrans,1)
						if(j_err)then
							call j_getname(ivmaketrans)
							write(6,*)'error in doing maketrans->'//j_oname(1:j_loname)
							goto 900
							!	endif !if(j_err)then
						endif !if(j_err)   7600
 
						j_tempvector(ibas+1:ibas+nkeep)=j_v( j_o(ivkeep)%i2(1:nkeep))
						!		j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v( j_o(ivkeep)%i2(1:nkeep))
						!write(6,*)iobs+1,'out',j_o(ivmat)%d(ibas+1:ibas+nkeep)
						iobs=iobs+1
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
					endif !if(maketran)   7586
 
 
					ibas2=ibas2+ncols
					ibas=ibas+nkeep
				enddo !k=1,j_o(arg2(i))%i(1)   7585
 
			enddo !i=1,narg   7581
 
 
 
		endif !if(.not.maketran.and.nextra.le.0)   7511
		!write(6,*)'hellurei',nob,nkeep
		ivmat=j_defmatrix8(iout,'%matrix',nob,nkeep,j_matreg,nod=.true.)
		!	write(6,*)'i',j_o(ivmat)%i
		!	write(6,*)'itemp ',itemp
		if(itemp.eq.1)then
			call move_alloc(from=j_tempvector,to=j_o(ivmat)%d)
			!	write(6,*)'dny',j_o(ivmat)%d
			!	call j_getname(ivmat)
			!	write(6,*)'mat',j_oname(1:j_loname)
		else
			call move_alloc(from=j_tempvector2,to=j_o(ivmat)%d)
		endif !if(itemp.eq.1)   7637
		iobs=nob
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
 
			endif !if(j_otype(arg(i)).eq.j_ipmatrix.or.j_otype(arg(i)).eq.j_i   7654
			ncols=j_o(arg2(i))%i(2)
			if(i.eq.1)then
				nob=j_o(arg2(i))%i(1)
				!write(6,*)'nob ',nob
			elseif(j_o(arg2(i))%i(1).ne.nob)then
				call j_printname('argument ',arg(i), ' has different number of rows than first argument')
				j_err=.true.
 
			endif !if(i.eq.1)   7672
			nvar=nvar+ncols
 
 
			nrowstot=nrowstot+j_o(arg2(i))%i(2)
 
			nvar0(i)=j_o(arg2(i))%i(2)
			if(j_otype(arg(i)).eq.j_ipmatrix.or.j_otype(arg(i)).eq.j_ipmatrixs)nvarmatrix=nvarmatrix+nvar0(i)
			!		 nvar=nvar+nvar0(i)
 
 
			!	call j_getname(arg(i))
			!			write(6,*)'<666 ',i,nvar0(i),nvar,j_oname(1:j_loname)
		enddo !i=1,narg   7649
		if(j_err)return
		!	write(6,*)'nvar,nvarmatrix,ncols',nvar,nvarmatrix,ncols
 
		if(nmatrix.gt.0)then
 
			!	call j_getoption(iob,io,j_mread,nvarmatrix,nvarmatrix,j_ipreal,.true.,nread,j_optarg)
			if(nread.ne.nvarmatrix)then
				write(6,*)'read-> should refer to as many variables as there are columns'
				write(6,*)'read-> has ',nread ,' arguments and matrices ',nvarmatrix,' columns'
				return
			endif !if(nread.ne.nvarmatrix)   7699
 
		endif !if(nmatrix.gt.0)   7696
		j_filterlink=j_codelink(iob,io,j_mfilter)
		j_filter=j_filterlink.ne.0
 
		j_rejectlink=j_codelink(iob,io,j_mreject)
		j_reject=j_rejectlink.ne.0
		istrans=j_filter.or.j_reject.or.maketran
		!write(6,*)'istrans ',istrans
 
		call j_clearoption(iob,io)  ! subroutine
 
		nvartot=nvar+noutv+nextra
 
		!	allocate(vars(1:nvar))
		!write(6,*)'<4664,nvartot,nvar,noutv,nextra,ivkeep',nvartot,nvar,noutv,nextra
		call j_deflistobject(iout,'%keep',ivkeep,nres=nvartot)
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
 
			endif !if(arg(i).eq.arg2(i))   7727
		enddo !i=1,narg   7725
		if(j_o(ivkeep)%i(1).lt.nvar)then
			!	write(6,*)'<555 ,nvar,noutv,nextra,j_o(ivkeep)%i(1)',&
			!	nvar,noutv,nextra,j_o(ivkeep)%i(1)
			write(6,*)'There are duplicated variable names'
			j_err=.true.; return
		endif !if(j_o(ivkeep)%i(1).lt.nvar)   7738
 
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
				endif !if(iper.gt.0)   7751
			enddo !ij=1,nextra   7749
		endif !if(nextra.gt.0)   7748
 
 
		!nkeep takes duplications into account
		nkeep=j_o(ivkeep)%i(1)
		!	write(6,*)'<new ',nkeep,nob
		if(allocated(j_tempvector))deallocate(j_tempvector)
		allocate(j_tempvector(1:nob*nkeep))
		!	ivmat=j_defmatrix8(iout,'%matrix',nob,nkeep,j_matreg)
		!write(6,*)'ivmat',ivmat,allocated(j_o(ivmat)%d),size(j_o(ivmat)%d)
 
 
		ibas0=0
 
 
		nrejected=0
		ibas=0
		iobs=0
		if(.not.istrans)iobs=nob
		!write(6,*)'iobs ',iobs,nob,istrans
		nkeep0=nkeep
		!	write(6,*)'isup ',isup
		do j=1,nob
			ibas1=ibas
			!if(isup)then
			!	call j_getobswup(iobs+1,arg(1))
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
 
				endif !if(j_otype(arg2(i)).eq.j_ipmatrix)   7788
 
				ibas0(i)=ibas0(i)+nvar0(i)
				ibas1=ibas1+nvar0(i)
			enddo !i=1,narg   7786
			if(.not.istrans)then
				ibas=ibas+nkeep
				cycle  !nobloop
			endif !if(.not.istrans)   7803
			j_v(j_o(ivkeep)%i2(1:nvar))=j_tempvector(ibas+1:ibas+nvar) !j_o(ivmat)%d(ibas+1:ibas+nvar)
			!write(6,*)'<33>',j_o(ivmat)%d(ibas+1:ibas+nkeep)
 
			!	endif !if(isup)   7558
			j_v(ivobs)=iobs+1
			j_rejected=.false.
 
 
			if(maketran)then
				call dotrans(ivmaketrans,1)
				if(j_err)then
					call j_getname(ivmaketrans)
					write(6,*)'error in doing maketrans->'//j_oname(1:j_loname)
					goto 900
				endif !if(j_err)   7817
			endif !if(maketran)   7815
			if(j_filter)then
				if(j_codevalue(iob,j_filterlink).eq.j_0)j_rejected=.true.
				! call dotrans(iob,j_iofilter)
				! if (j_err) goto 900
				! if(j_v(j_ivfilter).eq.0.)then
				! j_rejected=.true.
				! end if !if(j_v(j_ivfilter).eq.0.)then
			end if !if(j_filter)   7823
			if(j_reject)then
				if(j_codevalue(iob,j_rejectlink).ne.j_0)j_rejected=.true.
 
			end if !if(j_reject)   7831
			if(j_rejected)then
				nrejected=nrejected+1
			else
				iobs=iobs+1
				j_tempvector(ibas+1:ibas+nkeep)=j_v(j_o(ivkeep)%i2(1:nkeep))
				!		j_o(ivmat)%d(ibas+1:ibas+nkeep)=j_v(j_o(ivkeep)%i2(1:nkeep))
				ibas=ibas+nkeep
			endif !if(j_rejected)   7835
 
		enddo !j=1,nob   7781
 
		ivmat=j_defmatrix8(iout,'%matrix',nob,nkeep,j_matreg,nod=.true.)
		!	write(6,*)'i',j_o(ivmat)%i
		!	write(6,*)'itemp ',itemp,' d ',j_o(ivmat)%d
		!	if(itemp.eq.1)then
		call move_alloc(from=j_tempvector,to=j_o(ivmat)%d)
		!	write(6,*)'dny',j_o(ivmat)%d
		call j_getname(ivmat)
		!	write(6,*)'mat',j_oname(1:j_loname)
		! else
		! call move_alloc(from=j_tempvector2,to=j_o(ivmat)%d)
		! endif !if(itemp.eq.1)   7606
		iobs=nob
 
 
		!write(6,*)'pooo'
 
	endif !if(isappend)   7361
	!write(6,*)'fkfk'
	call j_getname(iout)
	write(6,*)'Accepted ',iobs, ' observations and ',nkeep,&
		' variables, which can be seen with '//j_oname(1:j_loname)//'%keep;'
 
	j_v(j_ivaccepted)=iobs
 
 
 
	if(iobs.ne.nob)then
		write(6,*)'rejected ',nrejected
		!		allocate(temp(1:nob*nkeep))
		!		temp=j_o(ivmat)%d
		ivmat=j_defmatrix8(iout,'%matrix',iobs,nkeep,j_matreg)
		j_o(ivmat)%d=j_tempvector(1:iobs*nkeep)
		deallocate(j_tempvector)
 
 
 
		!	call j_defdata(ivout,ivmat,ivkeep,ivcases,ivprolog,ivmaketrans,ivtrans,&
		!		ivepilog,ivvars,iout2_,ivnobsw,0,ivobs,ivobs,ivnobswcum)
		!subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivmaketrans,& ! %%data
		!		ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum)
		!subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivmaketrans,& ! %%data
		!		ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum)
		! else
		! ivmat=j_defmatrix8(iout,'%matrix',iobs,nkeep,j_matreg,nod=.true.)
		! call move_alloc(from=j_tempvector,to=j_o(ivmat)%d)
 
	endif !if(iobs.ne.nob)   7872
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
				write(6,*)'object ',j_oname(1:j_loname),' is made REAL, it was ',j_objecttypes(itype)
				call j_del(readv(i))
			endif !if(itype.ne.j_ipreal)   7904
		enddo !i=1,nread   7902
	endif !if(nread.gt.0)   7901
	!	write(6,*)'ivmat',iv,'dati',j_o(iout)%i
 
	!	j_o(j_ivlastdata)%i(1)=1;j_o(j_ivlastdata)%i2(1)=iout
	if(istime)then
		call cpu_time(cpu1)
		time1=secnds(time0)
		write(6,*)'newdata() used cpu ',cpu1-cpu0,' s and total time ',time1,' s'
	endif !if(istime)   7914
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
	! If the column is read in but it is just used as an input variable fot
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
	integer, dimension(:),allocatable:: nobs,nu,readv,keepv
	logical, dimension(:),allocatable:: isread,ispe
	character*2 ,dimension(:),allocatable:: x,f
 
	call j_startfunction(iob,io,0,narg0,j_optarg0,ivout)
	if(j_err)return
	call j_getobject(ivout,'%obs',j_ipreal,ivobs)
	call j_getoption_index(iob,io,j_min,1,1,j_ipchar,.false.,nin_,in_)
	call j_getoption_index(iob,io,j_mmaketrans,-1,1,j_iptrans,.true.,noptarg,j_optarg0)
 
	if(noptarg.gt.0)then
		ivmaketrans=j_optarg0(1)
		maketran=.true.
		ivoul=j_trans_output(ivmaketrans)
		!		noutv=j_o(ivoul)%i(0)
	else !if(noptarg.gt.0)then
		maketran=.false.
		!		noutv=0
	end if !if(noptarg.gt.0)   7986
 
	allocate(nobs(1:nin_),nu(1:nin_))
	! if(nobsarg.gt.0)then
	! nobs=j_v(j_optarg0(1:nobsarg))
	! else
	nobs=0
	!	endif
	nobtot=0
	do ifi=1,nin_
		call j_getfile(nu(ifi),'r',ivfile=in_(ifi))
		if(j_err)return
		nhead=1
		!character*80 j_cline
		!		 if(nobs(ifi).eq.0)then
		if(ifi.eq.1)then
			read(nu(ifi),'(a)',end=93,err=99)j_cline
			lens=j_lentrim(j_cline)
			write(6,*)'file ',ifi, ' header:',j_cline
		else !if(ifi.eq.1)then
			read(nu(ifi),'(a)',end=93,err=99)j_inpr
			if(j_inpr(1:lens).ne.j_cline(1:lens))then
				write(6,*)'file ',ifi,' header does not agree with the header of first file'
				write(6,*)j_inpr(1:lens)
				return
			endif !if(j_inpr(1:lens).ne.j_cline(1:lens))   8015
		endif !if(ifi.eq.1)   8009
		read(nu(ifi),'(a)',end=93,err=99)j_inpr !second line
		write(6,*)'sec:',j_inpr(1:80)
		if(ifi.eq.1)then
			!	character*132 j_tempchar2, j_tempchar3
			!lin=j_lentrim(j_inpr)
			!			read(nu(1),'(a)',err=99)j_inpr
			if(j_inpr(1:2).ne.'##')then
				write(6,*)'second line in first file must start with ##'
				return
			endif !if(j_inpr(1:2).ne.'##')   8027
			lin=j_lentrim(j_inpr)
			call j_clean(j_inpr(1:lin),lin)
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
			do ic1=3,lin
				ic=ic1
				if(ic1.eq.lin)ic=lin+1
				if(j_inpr(ic:ic).eq.';'.or.ic1.eq.lin)then
					npp=npp+1
					if(ic.gt.ic0+1.and.j_inpr(ic0+1:ic0+1).ne.'!')then
						nread=nread+1
						if(j_inpr(ic-1:ic-1).eq.'?')then
							x(npp)=j_inpr(ic-5:ic-4)
							f(npp)=j_inpr(ic-3:ic-2)
							ispe(npp)=.true.
 
							call j_getobject(0,j_inpr(ic0+1:ic-6),j_ipreal,iread) ! ivsubrecord)
							write(6,*)'npp ',npp,' x ',x(npp),' f ',f(npp),'iread ',iread
						else !if(j_inpr(ic-1:ic-1).eq.'?')then
							call j_getobject(0,j_inpr(ic0+1:ic-1),j_ipreal,iread) ! ivsubrecord)
						endif !if(j_inpr(ic-1:ic-1).eq.'?')   8053
 
						!	write(6,*)j_inpr(ic0+1:ic-1),iread
						if(j_err)return
 
						readv(nread)=iread
						isread(npp)=.true.
						if(j_inpr(ic0+1:ic0+1).ne.'$')nkeep=nkeep+1
						keepv(nkeep)=iread
					endif !if(ic.gt.ic0+1.and.j_inpr(ic0+1:ic0+1).ne.'!')   8051
					ic0=ic
				endif !if(j_inpr(ic:ic).eq.';'.or.ic1.eq.lin)   8049
			enddo !ic1=3,lin   8046
			nkeep=nkeep+1
			keepv(nkeep)=j_ivrecord
			!	call j_getobject(0,'File',j_ipreal,ifile)
			! if(nin_.gt.1)then
			! nkeep=nkeep+1
			! keepv(nkeep)=j_ivin
			! endif !if(nin_.gt.1)then
			keepv(0)=nkeep
			if(maketran)then
				!function j_putlistobject(ivlist,single,list0,list,ivin)
				!deflistobject(iv,name,ivout,list0,list,nres)
				call j_deflistobject(ivout,'%keep',ivkeep,list0=nkeep,list=keepv(1:nkeep),nres=j_o(ivoul)%i(1))
				iperk= j_putlistobject(ivkeep,ivin=ivoul)
				!call j_defmergelist(ivout,'%keep',keepv(0:nkeep),j_o(ivoul)%i,0,ivkeep)
			else !if(maketran)then
				call j_deflistobject(ivout,'%keep',ivkeep,listold=keepv(0:nkeep+1))
				!call j_deflist2(ivout,'%keep',keepv(0:nkeep),ivkeep)
			endif !if(maketran)   8083
			nkeep=j_o(ivkeep)%i(1)
			nhead=nhead+1
			write(6,*)'nread,nkeep',nread,nkeep
			!	write(6,*)readv(1:nread)
		else !if(ifi.eq.1)then
			if(j_inpr(1:2).eq.'@#'.or.j_inpr(1:6).eq.'jcode:')then
				nhead=nhead+1
			else !if(j_inpr(1:2).eq.'@#'.or.j_inpr(1:6).eq.'jcode:')then
				nobs(ifi)=nobs(ifi)+1
			endif !if(j_inpr(1:2).eq.'@#'.or.j_inpr(1:6).eq.'jcode:')   8098
		endif !if(ifi.eq.1)   8023
 
 
		do i=1,10000000
			read(nu(ifi),'(a)',end=93,err=99)j_tempchar3
			if(j_tempchar3(1:lens).eq.j_cline(1:lens).or.j_tempchar3(1:6).eq.'jcode:')then
				nhead=nhead+1
			else !if(j_tempchar3(1:lens).eq.j_cline(1:lens).or.j_tempchar3(1:6).eq.'jcode:')then
				nobs(ifi)=nobs(ifi)+1
			endif !if(j_tempchar3(1:lens).eq.j_cline(1:lens).or.j_tempchar3(1   8108
		enddo !i=1,10000000   8106
 
93		nobtot=nobtot+nobs(ifi)
		rewind(nu(ifi))
		call j_printname('file ',in_(ifi),' contains ')
		write(6,*) nobs(ifi),' observations in addition to ',nhead,' header lines'
	enddo !ifi=1,nin_   8003
	iobs=0
	ivmat=j_defmatrix(ivout,'%matrix',nobtot,j_o(ivkeep)%i(1),j_matreg)
	!write(6,*)'total number of obs ',nobtot
	!	write(6,*)'tas'
	do ifi=1,nin_
		!	j_v(j_ivin)=ifi
		read(nu(ifi),'(a)')
		do i=2,1000000
			read(nu(ifi),'(a)',end=88,err=88)j_inpr;lin=j_lentrim(j_inpr)
			goto 66
88			call j_closeunit(nu(ifi))
			exit
			!for some reason err condition occurs at end of file
66			if(j_inpr(1:2).eq.'@#')cycle
			if(j_inpr(1:6).eq.'jcode:')then
				!lin=j_lentrim(j_inpr) !
				call j_clean(j_inpr(1:lin),le)
				if(j_err)return
				write(6,*)'jcode:',j_inpr(7:le)
				call j_command(j_inpr(7:le),.true.)
				if(j_stop)then
					write(6,*)'stopehere'
					call j_stopj()
					return
				endif !if(j_stop)   8140
				if(j_err)then
					call j_closeunit(nu(ifi))
					goto 99
				endif !if(j_err)   8145
				cycle
			endif !if(j_inpr(1:6).eq.'jcode:')   8134
			if(j_inpr(1:lens).eq.j_cline(1:lens))cycle
 
			npp0=0
			nrea=0
			ic0=0
			!	if(ifi.eq.2.and.i.eq.1089)write(6,*)'isrea',isread
			do ic1=2,lin
				ic=ic1
				if(ic1.eq.lin)ic=lin+1
				if(j_inpr(ic:ic).eq.';'.or.ic.gt.lin)then
					npp0=npp0+1
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
								endif !if(x(npp0).ne.'00')   8168
							else !if(ispe(npp0))then
								read(j_inpr(ic0+1:ic-1),*,err=991)j_v(readv(nrea))
								!	if(ifi.eq.2.and.i.eq.1089)write(6,*)'npp0,ic0,ic,nrea',npp0,ic0,ic,nrea,j_v(readv(nrea))
							endif !if(ispe(npp0))   8167
 
						else !if(ic.gt.ic0+1)then
							j_v(readv(nrea))=0.d0
						endif !if(ic.gt.ic0+1)   8166
						!				if(nrea.eq.nread-2.and.j_v(readv(nrea)).gt.0.d0)&
						!write(6,*)ifi,i,':',j_inpr(1:lin)
						!if(nrea.eq.nread-2.and.j_v(readv(nrea)).gt.0.d0)&
						!	write(6,*)j_v(readv(1:nread))
						! if(i.le.2)then
						! call j_printname(' var ',readv(nrea),' ')
						!write(6,*)nrea,j_v(readv(nrea))
						! endif
					endif !if(isread(npp0))   8164
 
					ic0=ic
				endif !if(j_inpr(ic:ic).eq.';'.or.ic.gt.lin)   8160
 
			enddo !ic1=2,lin   8157
			if(nrea.lt.nread)j_v(readv(nrea+1:nread))=0.d0
			j_v(j_ivrecord)=i
			if(maketran)then
				j_v(ivobs)=iobs
				call dotrans(ivmaketrans,1)
				if(j_err)return
			endif !if(maketran)   8197
			iobs=iobs+1
			j_o(ivmat)%d((iobs-1)*nkeep+1:iobs*nkeep)=j_v(j_o(ivkeep)%i2(1:nkeep))
 
		enddo !i=2,1000000   8127
	enddo !ifi=1,nin_   8124
900 	continue
	!call j_deflist2(ivout,'%vars',j_o(ivkeep)%i(0:nkeep),ivvars)
		!subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivprolog,ivmaketrans,ivtrans,& ! %%data
	!	ivepilog,ivvars,ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum) !%%data
	!subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivmaketrans,& ! %%data
	!	ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum)
	call j_defdata(ivout,ivmat,ivkeep)
	call j_printname('data ',ivout, ' created')
	write(6,*)'Accepted ',nobtot
	j_v(j_ivaccepted)=nobtot
	j_dlastdata=ivout
	j_v(j_ivlastdata)=ivout
	!j_o(j_ivlastdata)%i(1)=1;j_o(j_ivlastdata)%i2(1)=ivout
800	deallocate(nobs,nu)
	if(allocated(readv))deallocate(readv,keepv,x,f,isread,ispe)
	return
	!character*4096 j_inpr
 
99  write(6,*)'reading error, file ',ifi,' record',i
	j_err=.true.
	goto 800
998  write(6,*)'file ',ifi,' record ',i
	write(6,*)j_inpr(1:lin)
	write(6,*)'error reading number from ',ic0+1, 'to ',ic-1, 'i.e. from ',&
		j_inpr(ic0+1:ic-1),' with format'
	if(x(npp0).ne.'00')then
		write(6,*)'('//x(npp0)//'x,f'//f(npp0)//'.0)'
	else !if(x(npp0).ne.'00')then
		write(6,*)'(f'//f(npp0)//'.0)'
	endif !if(x(npp0).ne.'00')   8232
	j_err=.true.
	goto 800
991  write(6,*)'file ',ifi,' record ',i
	write(6,*)j_inpr(1:lin)
	write(6,*)'error reading variable from ',ic0+1, 'to ',ic-1, 'i.e. from ',&
		j_inpr(ic0+1:ic-1)
 
	j_err=.true.
	goto 800
end subroutine exceldata !subroutine exceldata(iob,io)
 
subroutine classvector(iob,io)
	!Section classvector classvector() Vectors from grouped DATA
	!Function classvector computes vectors from data which extract information from grouped
	!data. These vectors can be used to generate new data object using newdata() function or
	!new matrices from submatrices using matrix() function with matrix-> option or
	! they can be used in transformation objects to compute class related things.
	!There is no explicit output for the function, but several output vectors can
	! be generated depending on the arguments and first->, last-> and
	! expand-> options. The function prints the names of the output vectors generated.
	!endheader
	!Option
	!Args&0-&REAL& The variables whose class information is computed. Arguments
	! are not necessary if first-> and/or last-> are present.
	!Let §Arg be the generic name for arguments.
	!class&1&REAL& .
	!class&1&REAL&The variable indicating the class. The class variable which must be present in the data object or which is
	!an output variable of the trans-> transformations.
	!When the class-> variable, denoted as
	!as §Class changes,
	!the class changes.
	!data&0|1&Data&Data object used. Only one data object used; extra data-> objects just ignored. The default is the last
	! data object generated.
	!expand&-1|0& &If expand-> is present then the lengths output vectors are equal
	!to the number of observations in the data object and the values of the class variables
	! are repeated as many times as there are observations in each class. If
	!expand-> is not present, the lengths of the output vectors are.
	! equal to the number of classes.
	!first&0& &The the number of first observation in class is stored in vector
	! §Class%%first if expand-> is present and §Class%first if expand-> is not present.
	!last&0& &The the number of lastt observation in class is stored in vector
	! §Class%%last if expand-> is present and §Class%lastobject if expand-> is not present.
	!obsw&0& &If there axpnad-> option then vector Class%%obsw
	!ext&-1|1&Char&The extension to the names of vectors generated for arguments. Let
	! Ext be denote the extension.
	!mean&-1|0& &The class means are stored in the vectors \newline
	! §Arg#Class%%mean with expand-> and without ext->\newline
	! §Arg#Class%%meanExt with expand-> and with ext-> are \newline
	! §Arg#Class%mean without expand-> and without ext-> \newline
	! §Arg#Class%meanExt without expand-> and with ext->
	!sd&-1|0& & Class standard deviations are computed to sd vectors
	!var&-1|0& & Class variances are computed to var vectors
	!min&-1|0& & Class minimums are computed to min vectors
	!max&-1|0& & Class maximums are computed to max vectors.
	!endoption
	!Note Numbers of observations in each class can be obtained by \\
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
 
	!call j_defmatrix(ivclass,'%first',nclass,1,j_matreg,.false.,ive) nexpand
	!call j_defmatrix(0,j_varname1(1:le)//'%%first',nobs,1,j_matreg,.false.,ive) expand
	!nexpand
	!		if(next.le.0)&
	!call j_defmatrix(arg(ia),'#'//j_varname1(1:le)//'%mean',nclass,1,j_matreg,.false.,ive)
	!if(next.gt.0)&
	!call j_defmatrix(arg(ia),'#'//j_varname1(1:le)//'%mean'//j_cline(1:lext),nclass,1,j_matreg,.fals
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
	integer*8::iobs
	integer ::nclass
	!	logical maketran
	! call j_checkoutput(iob,io)
	! if(j_err)return
	! j_optarg0
	!	write(6,'(30i5/)')j_o(iob)%i(1: j_o(iob)%i(0) )
	call j_startfunction(iob,io,j_ipreal,narg,arg,ivout)
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
	!j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
	call j_getoption(iob,io,j_mclass,1,1,j_ipreal,.true.,nc,clvar)
	if(j_err)return
	ivclass=clvar(1)
	call j_getline(j_ivnames,ivclass,j_varname1,le)
	if(j_err)return
	! j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
	!	if(ivout.eq.j_ivresult)ivout=ivclass
	call j_getoption(iob,io,j_mext,-1,1,j_ipchar,.true.,next,j_optarg0)
	if(j_err)return
	if(next.gt.0)call j_getchar(j_optarg0(1),j_cline,lext)
 
 
	call j_getoption(iob,io,j_mfirst,-1,0,j_ipreal,.false.,ifirst,j_optarg0)
	if(j_err)return
	call j_getoption(iob,io,j_mobsw,-1,0,j_ipreal,.false.,iobsw,j_optarg0)
	if(j_err)return
	call j_getoption(iob,io,j_mlast,-1,0,j_ipreal,.false.,ilast,j_optarg0)
	if(j_err)return
	call j_getoption(iob,io,j_mw,-1,0,j_ipreal,.false.,iw,j_optarg0)
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
 
 
	call j_getoption(iob,io,j_mmax,-1,1,j_ipreal,.false.,imax0,j_optarg0)
	if(j_err)return
	if(imax0.ge.0)allocate(maxi0(1:j_dnobs))
	call j_getoption(iob,io,j_mexpand,-1,0,j_ipreal,.false.,nexpand,j_optarg0)
	if(j_err)return
 
 
 
 
	call j_clearoption(iob,io)  ! subroutine
	ifi=1
	i1=1
 
 
	n=0
	!	nkier=1
	!write(6,*)ivar0+isd0+isum0+imax0+imin0+imean0,ivar0,isd0,isum0,imax0,imin0,imean0
	if(narg.le.0.and.ilast.lt.0.and.ifirst.lt.0)then
		write(6,*)'*arguments missing'
		j_err=.true.
		return
	endif !if(narg.le.0.and.ilast.lt.0.and.ifirst.lt.0)   8453
	if(iw.ge.0.and.nexpand.lt.0)then
		write(6,*)'option ->w requires expand->'
		j_err=.true.;return
 
	endif !if(iw.ge.0.and.nexpand.lt.0)   8458
 
 
	allocate(last0(1:j_dnobs8))
 
	nlo=max(narg,1)
		vloop: do ia=1,nlo
		!call j_getdataset(j_datasets(1),nobs)
		if(narg.gt.0)then
 
 
			! endif
			sum=0.d0
			sum2=0.d0
			n=0
 
			cmin=1.d60
			cmax=-1.d60
			if(iw.ge.0)then
 
				if(next.le.0)&
					ivew=j_defmatrix8(arg(ia),'['//j_varname1(1:le)//']%%w',j_dnobs8,j_18,j_matreg)
				if(next.gt.0)&
					ivew=j_defmatrix8(arg(ia),'['//j_varname1(1:le)//']%%w'//j_cline(1:lext),j_dnobs8,j_18,j_matreg)
				call j_printname('   done ',ivew,' ')
			endif !if(iw.ge.0)   8480
		endif !if(narg.gt.0)   8470
		classv=-1.79d37
		ifi=1
		!write(6,*)'ivar,isd,isum,imin,imax,nobs',ivar,isd,isum,imin,imax
		nclass=1
		!	n
			ob:	do iobs=j_dfrom,j_duntil+1

!	write(6,*)'i',i

			if(iobs.le.j_dnobs)then
				!	j_v(j_ivobs)=i
				call j_getobs(iobs);
				!			call j_getob(j_datasets(1),i);if(j_err)return
				!call j_nextobs()
				if(j_err)return
				!			if(j_rejected)cycle
			endif !if(iobs.le.j_dnobs)   8498
 
			!	write(6,*)'j_v(clvar(1))',j_v(clvar(1)),ifi
			if((j_v(ivclass).ne.classv.and.iobs.gt.1).or.iobs.eq.j_dnobs+1)then
				!j_o(iv)%i(1)=ivmat
				!	write(6,*)'<44>'
				last0(nclass)=i-1
 
				if(narg.gt.0)then
					!		write(6,*)'<45>',ivmat,nkeep,indvar,'j_dnobs',nobs
					nd=n
					!	sum
					cmean=sum/nd
					cvar=0.d0
					if(nd.gt.1)cvar=(sum2-sum*sum/nd)/(nd-1.d0)
					csd=0.d0
					if(cvar.gt.0.d0)csd=sqrt(cvar)
					if(ivar0.ge.0)var0(nclass)=cvar
					if(isd0.ge.0)sd0(nclass)=csd
					if(isum0.ge.0) sum0(nclass)=sum
					if(imean0.ge.0.or.iw.ge.0) mean0(nclass)=cmean
					!write(6,*)'clasmmean var ',ia,'class ',nclass,' mean ',cmean
					if(imin0.ge.0) mini0(nclass)=cmin
					if(imax0.ge.0) maxi0(nclass)=cmax
					!				write(6,*)nclass,sum,sum2,cmean,cvar,csd
				endif !if(narg.gt.0)   8513
				if(iobs.eq.j_dnobs+1)exit ob
				nclass=nclass+1
				!		write(6,*)'<478>',nclass
				sum=0.d0
				sum2=0.d0
				n=0
				ifi=iobs
				cmin=1.d60
				cmax=-1.d60
			endif !if((j_v(ivclass).ne.classv.and.iobs.gt.1).or.iobs.eq.j_dno   8508
			classv=j_v(ivclass)
			if(j_rejected)cycle ob
 
 
			n=n+1
			if(narg.gt.0)then
				if(j_v(arg(ia)).ge.1.7d19)cycle
				sum=sum+j_v(arg(ia))
				sum2=sum2+j_v(arg(ia))*j_v(arg(ia))
				cmin=min(cmin,j_v(arg(ia)))
				cmax=max(cmax,j_v(arg(ia)))
				if(iw.ge.0)j_o(ivew)%d(i)=j_v(arg(ia))
				!if(iw.ge.0)!write(6,*) 'put in w matrix ',i,j_v(arg(ia))
				!	write(6,*)'csum',csum,sum2,cmin
			endif !if(narg.gt.0)   8546
		enddo ob !	do iobs=j_dfrom,j_duntil+1   8494
 
		if(ifirst.ge.0.and.ia.eq.1)then
			if(nexpand.lt.0)then
				ive=j_defmatrix(ivclass,'%first',nclass,1,j_matreg)
				!	write(6,*)'nclass',nclass,size(j
				!	ibas=0
				j_o(ive)%d(1)=j_1
				!	call j_putmatrix(ive,1,1,1.d0 )
				do i=2,nclass
					j_o(ive)%d(i)=last0(i-1)+j_1
					!	call j_putmatrix(ive,i,1,last0(i-1)+1.d0)
				enddo !i=2,nclass   8565
				call j_printname('   done ',ive,' ')
			else !if(nexpand.lt.0)then
				ifi=1
				!call j_defmatrix(arg(ia),'$'//j_varname1(1:le)//'%mean',j_dnobs,1,j_matreg,.false.,ive)
				!			call j_defmatrix(ivclass,'%first',j_dnobs,1,j_matreg,.false.,ive)
				ive=j_defmatrix(0,j_varname1(1:le)//'%%first',j_dnobs,1,j_matreg)
				!	write(6,*)'nclass',nclass,size(j
				j_o(ive)%d(1)=j_1  !call j_putmatrix(ive,1,1,1.d0 )
				ij=1
				do i=2,nclass
					do j=ifi,last0(ii)
						ij=ij+1
						j_o(ive)%d(ij)=last0(i-1)+j_1
						!		call j_putmatrix(ive,i,1,last0(i-1)+1.d0)
					enddo !j=ifi,last0(ii)   8579
					ifi=last0(i)+1
				enddo !i=2,nclass   8578
				call j_printname('   done ',ive,' ')
			endif !if(nexpand.lt.0)   8559
		endif !if(ifirst.ge.0.and.ia.eq.1)   8558
 
 
 
		if(ilast.ge.0.and.ia.eq.1)then
			if(nexpand.lt.0)then
				ive=j_defmatrix(ivclass,'%last',nclass,1,j_matreg)
				!		do i=1,nclass
				j_o(ive)%d(1:nclass)=last0(1:nclass)
				!		call j_putmatrix(ive,i,1,dble(last0(i)) )
				!	enddo !i=1,nclass   8430
				call j_printname('   done ',ive,' ')
			else !if(nexpand.lt.0)then
				ifi=1
				ive=j_defmatrix(0,j_varname1(1:le)//'%%last',j_dnobs,1,j_matreg)
				!	call j_defmatrix(ivclass,'%last',nobs,1,j_matreg,ive)
				ij=0
				do i=1,nclass
					do j=ifi,last0(ii)
						ij=ij+1
						j_o(ive)%d(ij)=last0(i)
						!		call j_putmatrix(ive,i,1,dble(last0(i)) )
					enddo !j=ifi,last0(ii)   8606
					ifi=last0(i)+1
				enddo !i=1,nclass   8605
 
				call j_printname('   done ',ive,' ')
			endif !if(nexpand.lt.0)   8593
		endif !if(ilast.ge.0.and.ia.eq.1)   8592
 
		if(iobsw.ge.0.and.ia.eq.1)then
			if(nexpand.lt.0)then
				write(6,*)'obsw-> requires expand->'
				j_err=.true.;return
			else !if(nexpand.lt.0)then
				ifi=1
				ive=j_defmatrix(0,j_varname1(1:le)//'%%obsw',j_dnobs,1,j_matreg)
				!	call j_defmatrix(ivclass,'%last',nobs,1,j_matreg,ive)
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=j-ifi+1
						!				call j_putmatrix(ive,i,1,dble(j-ifi+1) )
					enddo !j=ifi,last0(i)   8628
					ifi=last0(i)+1
				enddo !i=1,nclass   8627
 
				call j_printname('   done ',ive,' ')
			endif !if(nexpand.lt.0)   8619
 
 
		endif !if(iobsw.ge.0.and.ia.eq.1)   8618
		!	write(6,*)'<454545 imean0',imean0
		!	write(6,*)'ia,arg,narg',ia,arg,narg,'imean0',imean0
		call j_getname(arg(ia))
		!	write(6,*)'var0 ',j_oname(1:j_loname)
		if(imean0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				!			write(6,*)'<66 ',ia,arg
 
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%mean',nclass,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%mean'//j_cline(1:lext),nclass,1,j_matreg)
 
 
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=mean0(1:nclass)
				!	call j_putmatrix(ive,i,1,mean0(i) )
				!	enddo !i=1,nclass   8484
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%mean',j_dnobs,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%mean'//j_cline(1:lext),j_dnobs,1,j_matreg)
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						!write(6,*)'ijmean',ij,'mean',mean0(i)
						j_o(ive)%d(ij)=mean0(i)
						!		call j_putmatrix(ive,ij,1,mean0(i) )
					enddo !j=ifi,last0(i)   8667
					ifi=last0(i)+1
				enddo !i=1,nclass   8666
			endif !if(nexpand.lt.0)   8647
			call j_printname('   done ',ive,' ')
		endif !if(imean0.ge.0)   8645
 
		if(iw.ge.0)then
			ij=0
			ifi=1
			do i=1,nclass
				do j=ifi,last0(i)
					ij=ij+1
					!write(6,*)'ijol',ij,' invector ',j_o(ivew)%d(ij),'mean',mean0(i)
					j_o(ivew)%d(ij)=j_o(ivew)%d(ij)-mean0(i)
 
				enddo !j=ifi,last0(i)   8683
				ifi=last0(i)+1
			enddo !i=1,nclass   8682
			call j_printname('   done ',ivew,' ')
		endif !if(iw.ge.0)   8679
 
 
 
		if(ivar0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%var',nclass,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%var'//j_cline(1:lext),nclass,1,j_matreg)
 
				!			call j_printname('making ',ive,' ')
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=var0(1:nclass)
				!			call j_putmatrix(ive,i,1,var0(i) )
				!	enddo !i=1,nclass   8531
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),j_varname1(1:le)//']%%var',j_dnobs,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),j_varname1(1:le)//']%%var'//j_cline(1:lext),j_dnobs,1,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=var0(i)
						!		call j_putmatrix(ive,ij,1,var0(i) )
					enddo !j=ifi,last0(i)   8717
					ifi=last0(i)+1
				enddo !i=1,nclass   8716
			endif !if(nexpand.lt.0)   8698
			call j_printname('   done ',ive,' ')
		endif !if(ivar0.ge.0)   8696
 
		if(isd0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%sd',nclass,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%sd'//j_cline(1:lext),nclass,1,j_matreg)
 
				!			call j_printname('making ',ive,' ')
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=sd0(1:nclass)
				!			call j_putmatrix(ive,i,1,sd0(i) )
				!	enddo !i=1,nclass   8561
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%sd',j_dnobs,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%sd'//j_cline(1:lext),j_dnobs,1,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=sd0(i)
						!	call j_putmatrix(ive,ij,1,sd0(i) )
					enddo !j=ifi,last0(i)   8749
					ifi=last0(i)+1
				enddo !i=1,nclass   8748
			endif !if(nexpand.lt.0)   8730
			call j_printname('   done ',ive,' ')
		endif !if(isd0.ge.0)   8728
 
		if(isum0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%sum',nclass,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%sum'//j_cline(1:lext),nclass,1,j_matreg)
 
				!			call j_printname('making ',ive,' ')
				!		do i=1,nclass
				j_o(ive)%d(1:nclass)=sum0(1:nclass)
				!		call j_putmatrix(ive,i,1,sum0(i) )
				!	enddo !i=1,nclass   8591
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%sum',j_dnobs,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%sum'//j_cline(1:lext),j_dnobs,1,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=sum0(i)
						!		call j_putmatrix(ive,ij,1,sum0(i) )
					enddo !j=ifi,last0(i)   8781
					ifi=last0(i)+1
				enddo !i=1,nclass   8780
			endif !if(nexpand.lt.0)   8762
			call j_printname('   done ',ive,' ')
		endif !if(isum0.ge.0)   8760
 
		if(imin0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%min',nclass,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%min'//j_cline(1:lext),nclass,1,j_matreg)
 
				!			call j_printname('making ',ive,' ')
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=mini0(1:nclass)
				!	call j_putmatrix(ive,i,1,mini0(i) )
				!enddo !i=1,nclass   8621
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%min',j_dnobs,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%min'//j_cline(1:lext),j_dnobs,1,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=mini0(i)
						!	call j_putmatrix(ive,ij,1,mini0(i) )
					enddo !j=ifi,last0(i)   8813
					ifi=last0(i)+1
				enddo !i=1,nclass   8812
			endif !if(nexpand.lt.0)   8794
			call j_printname('   done ',ive,' ')
		endif !if(imin0.ge.0)   8792
 
		if(imax0.ge.0)then
			ifi=1
			if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%max',nclass,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%max'//j_cline(1:lext),nclass,1,j_matreg)
 
				!		call j_printname('making ',ive,' ')
				!	do i=1,nclass
				j_o(ive)%d(1:nclass)=maxi0(1:nclass)
				!		call j_putmatrix(ive,i,1,maxi0(i) )
				!	enddo !i=1,nclass   8651
			else !if(nexpand.lt.0)then
				if(next.le.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%max',j_dnobs,1,j_matreg)
				if(next.gt.0)&
					ive=j_defmatrix(arg(ia),'['//j_varname1(1:le)//']%%max'//j_cline(1:lext),j_dnobs,1,j_matreg)
 
				ij=0
				do i=1,nclass
					do j=ifi,last0(i)
						ij=ij+1
						j_o(ive)%d(ij)=maxi0(i)
						!			call j_putmatrix(ive,ij,1,maxi0(i) )
					enddo !j=ifi,last0(i)   8845
					ifi=last0(i)+1
				enddo !i=1,nclass   8844
			endif !if(nexpand.lt.0)   8826
			call j_printname('   done ',ive,' ')
		endif !if(imax0.ge.0)   8824
 
 
 
	enddo vloop !op: do ia=1,nlo   8468
	!	write(6,*)'tas'
 
	if(allocated(mean0))deallocate(mean0)
	if(allocated(var0))deallocate(var0)
	if(allocated(sd0))deallocate(sd0)
	if(allocated(sum0))deallocate(sum0)
	if(allocated(mini0))deallocate(mini0)
	if(allocated(maxi0))deallocate(maxi0)
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
 
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
	integer*8::ndim2,narg8,iobs,iobsw
	integer ::i,j,j2
	id(i)=i*(i+1)/2
 
	call j_startfunction(iob,io,j_ipreal,narg,arg,ivout)
	narg8=narg
	if(j_err)return
	if(narg.le.0)then
		write(6,*)'*varcomp: there must be arguments'
		j_err=.true.
	endif !if(narg.le.0)   8896
 
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
	endif !if(ivout.eq.j_ivresult.and.iscov)   8908
	call j_getdataobject(iob,io) ;if(j_err)return
	!call j_getdatasets(iob)
 
	call j_clearoption(iob,io)  ! subroutine
	if(j_err)return
 
	lkm=narg
	if(iscov)lkm=narg*(narg+1)/2
	allocate(s(1:lkm),xs(1:lkm),xss(1:lkm),sb(1:lkm),sumwt(1:lkm),xmi(1:lkm),xma(1:lkm),xs2(1:lkm))
	ntot=0
 
	call j_dbw(-2,s,lkm,iobsw,nmets,xs,xss,sb,sumwt,xmi,xma,xs2,s2)
	missing=0
	!do k=1,jndatasetss
 
	!call j_getdataset(j_datasets(k),j_dnobs) !initilize dtaset k, nobs will be the number of observations
 
		obloop:	do iobs=j_dfrom,j_duntil

		call j_getobs(iobs)   ! get next observation compute transformations
 
		if(j_err)return
 
		!there may be errors in computing transformations
 
		if(j_rejected)cycle ! j_rejected tells if the observation is rejected
		do j=1,narg
			if(abs(j_v(arg(j))).ge.1.7d19)then
				missing=missing+1
				cycle obloop
 
			endif !if(abs(j_v(arg(j))).ge.1.7d19)   8939
		enddo !j=1,narg   8938
		ntot=ntot+1
		!	 write(6,*)v(o(iob)%i(io+1+1)),v(o(iob)%i(io+1+2))
		mets=j_v(classv(1))
		jc=1
		do j=1,narg
			if(iscov)then
				do j2=1,j-1
					s(jc)=j_v(arg(j2))+j_v(arg(j))
					jc=jc+1
				end do !j2=1,j-1   8951
			end if !if(iscov)   8950
			s(jc)=j_v(arg(j))
			jc=jc+1
		end do !j=1,narg   8949
 
		call j_dbw(mets,s,lkm,iobsw,nmets,xs,xss,sb,sumwt,xmi,xma,xs2,s2)
 
	enddo obloop !oop:	do iobs=j_dfrom,j_duntil   8929
	!	end do !do k=1,jndatasetss
 
	call j_dbw(-1,s,lkm,iobsw,nmets,xs,xss,sb,sumwt,xmi,xma,xs2,s2)
 
	write(6,'(a,i5,a,i4)')'Accepted ',iobs,' classes:',nmets
	if(missing.gt.0)write(6,*)'missing ',missing
	j_v(j_ivaccepted)=iobs
	if(iobs.le.4)then
		write(6,*)'*not enough observations'
 
		return
	endif !if(iobs.le.4)   8970
 
	write(6,*)&
		'                average     GLS mean      sb         sw'// &
		'        RMSE'
 
	do j=1,narg
		j2=j
		if(iscov)	j2=j*(j+1)/2
		write(6,'(a15,8g12.5e1)')&
			j_object_name(arg(j),15),xs(j2),&
			xs2(j2), & !xss(j2)
			sb(j2),sumwt(j2),sqrt(sb(j2)**2+sumwt(j2)**2)  !,&
		!     100.*sb(j2)**2/(sumwt(j2)**2+sb(j2)**2)
	enddo !j=1,narg   8980
	if(ivout.eq.j_ivresult)return
	if(narg.eq.1)then
		call j_getobject(ivout,'%varb',j_ipreal,ivvarb)
		call j_getobject(ivout,'%varw',j_ipreal,ivvarw)
		call j_getobject(ivout,'%glsmean',j_ipreal,ivglsmean)
		call j_getobject(ivout,'%mean',j_ipreal,ivmean)
		j_v(ivvarb)=sb(j2)**2
		j_v(ivvarw)=sumwt(j2)**2
		j_v(ivglsmean)=xs2(j2)
		j_v(ivmean)=xs2(j2)
		return
	endif !if(narg.eq.1)   8990
 
	ndim2=1
	if(iscov)ndim2=narg
	if(icov.ge.0)then
		ivvarb=j_defmatrix8(ivout,'%varb',narg8,ndim2,j_matreg)
		call j_printname('matrix ',ivvarb,' generated')
		ivvarw=j_defmatrix8(ivout,'%varw',narg8,ndim2,j_matreg)
		call j_printname('matrix ',ivvarw,' generated')
	elseif(icorr.lt.0)then !if(icov.ge.0)then
		ivvarb=j_defmatrix8(ivout,'%varb',narg8,ndim2,j_matreg)
		call j_printname('vector ',ivvarb,' generated')
		ivvarw=j_defmatrix8(ivout,'%varw',narg8,ndim2,j_matreg)
		call j_printname('vector ',ivvarw,' generated')
 
	endif !if(icov.ge.0)   9004
	if(icorr.ge.0)then
		ivSb=j_defmatrix8(ivout,'%corrb',narg8,ndim2,j_matreg)
		call j_printname('matrix ',ivsb,' generated')
		ivSw=j_defmatrix8(ivout,'%corrw',narg8,ndim2,j_matreg)
		call j_printname('matrix ',ivsw,' generated')
	endif !if(icorr.ge.0)   9016
	ivglsmean=j_defmatrix8(ivout,'%glsmean',narg8,j_18,j_matreg)
	call j_printname('vector ',ivglsmean,' generated')
	ivmean=j_defmatrix8(ivout,'%mean',narg8,j_18,j_matreg)
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
				endif !if(icov.ge.0)   9030
				if(icorr.ge.0)then
					el=0.5*(sb(jc)**2-sb(id(j))**2-sb(id(j2))**2)/(sb(id(j))*sb(id(j2)))
					call j_putmatrix(ivsb,j,j2, el) !s(jc)=j_v(arg(j2))+j_v(arg(j))
					call j_putmatrix(ivsb,j2,j,el ) !s(jc)=j_v(arg(j2))+j_v(arg(j))
					el=0.5*(sumwt(jc)**2-sumwt(id(j))**2-sumwt(id(j2))**2)/(sumwt(id(j))*sumwt(id(j2)))
					call j_putmatrix(ivsw,j,j2,el)
					call j_putmatrix(ivsw,j2,j,el)
				endif !if(icorr.ge.0)   9038
				jc=jc+1
			end do !j2=1,j-1   9029
		end if !if(iscov)   9028
		if(icov.ge.0)then
			call j_putmatrix(ivvarb,j,j,sb(jc)**2)
			call j_putmatrix(ivvarw,j,j,sumwt(jc)**2)
		elseif(icorr.lt.0)then !if(icov.ge.0)then
			call j_putmatrix(ivvarb,j,1,sb(jc)**2)
			call j_putmatrix(ivvarw,j,1,sumwt(jc)**2)
 
		endif !if(icov.ge.0)   9049
		if(icorr.ge.0)then
			call j_putmatrix(ivsb,j,j,1.d0)
			call j_putmatrix(ivsw,j,j,1.d0)
		endif !if(icorr.ge.0)   9057
		call j_putmatrix(ivglsmean,1,j,xs2(jc))
		call j_putmatrix(ivmean,1,j,xs(jc))
 
		jc=jc+1
	end do !j=1,narg   9027
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
 
	call j_startfunction(iob,io,j_ipreal,narg,arg,ivout)
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
	call j_startfunction(iob,io,0,narg,arg,ivout_)
 
	if(j_ndo.ge.j_mxndo)then
		write(6,*)j_ndo,j_mxndo
		write(6,*)'**too many ;do -loops'
		j_err=.true.
		j_ndo=0
		return
	endif !if(j_ndo.ge.j_mxndo)   9227
	if(j_ndo.eq.j_ndoinc(j_ninc))then
		if(j_reacom(j_ninc))then
			ndotemp=1
			if(.not.j_savcom(j_ninc))then
				call j_deftext2(0,'Buffer'//char(47+j_ninc),500,10000,50,j_ivbuf(j_ninc))
				j_savcom(j_ninc)=.true.
				call j_puttext(j_ivbuf(j_ninc), j_inp(1:j_linp))
				istart=1
			else !if(.not.j_savcom(j_ninc))then
				istart=j_o(j_ivbuf(j_ninc))%i(0)  !cuuren line in text buffer
			endif !if(.not.j_savcom(j_ninc))   9237
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
			endif !if(j_inp(1:j_linp).eq.'endsimu')   9262
			call j_puttext(j_ivbuf(j_ninc), j_inp(1:j_linp))
 
			icu=icu+1
 
			if(j_inp(j_ialb:j_ialb+2).eq.';do'.and.j_inp(ialb2:ialb2).eq.'(')then
				ndotemp=ndotemp+1
			endif !if(j_inp(j_ialb:j_ialb+2).eq.';do'.and.j_inp(ialb2:ialb2).   9269
			if(j_inp(j_ialb:j_ialb+5).eq.';enddo'.or.j_inp(j_ialb:j_ialb+6).eq.';end do')then
				ndotemp=ndotemp-1
			endif !if(j_inp(j_ialb:j_ialb+5).eq.';enddo'.or.j_inp(j_ialb:j_ia   9272
 
			if(ndotemp.eq.0)goto 17
			goto 10
			90 		write(6,*)'unclosed ;do loop'
			j_err=.true.
 
			goto 900
		else !if(j_reacom(j_ninc))then
			istart=j_o(j_ivbuf(j_ninc))%i(0)
 
		endif !if(j_reacom(j_ninc))   9235
	else !if(j_ndo.eq.j_ndoinc(j_ninc))then
		istart=j_icurl(j_ninc)-1 ! this is set to next already
	endif !if(j_ndo.eq.j_ndoinc(j_ninc))   9234
 
	17 ilow=j_v(j_o(iob)%i(io+3)); iup=j_v(j_o(iob)%i(io+4))
	if(narg.le.3)then
		istep=1
	else !if(narg.le.3)then
		istep=j_v(j_o(iob)%i(io+5))
	endif !if(narg.le.3)   9291
 
	if(istep.eq.0)then
		write(6,*)'**illegal ;do loop with low,up,step:',ilow,iup,istep
		j_err=.true.
 
		goto 900
	endif !if(istep.eq.0)   9297
 
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
		call j_clearoption(iob,io)  ! subroutine
		return
	endif !if(nstep.le.0)   9305
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
	call j_clearoption(iob,io)  ! subroutine
 
	if(j_ndo.eq.1)then
		j_icurl(j_ninc)=istart+1
	endif !if(j_ndo.eq.1)   9333
	j_reacom(j_ninc)=.false.
	900 continue
	call j_clearoption(iob,io)  ! subroutine
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
	end if !if(j_ninc.lt.2.or.j_ipc(iva).eq.0)   9351
 
	if(j_ndo.gt.0)then
		write(6,*)'**open ;do loops closed'
		j_ndo=0
	endif !if(j_ndo.gt.0)   9356
 
	iva1=j_o(iva)%i(1);iva2=j_o(iva)%i(2)
	ivb=j_ivbuf(j_ninc);lena=iva2-iva1+1
	if(ivb.ne.0.and.j_otype(max(ivb,1)).eq.j_iptext)then !search for labels
			adrloop: do i=1,j_o(ivb)%i2(0)
			iil=j_o(ivb)%i2(i)
			j1=j_o(ivb)%i(iil)+1;j2=j_o(ivb)%i(iil+1)-2
			if(j2-j1.ne.iva2-iva1)cycle
			do j=0,iva2-iva1
				if(j_o(j_ivnames)%ch(iva1+j).ne.j_o(ivb)%ch(j1+j))cycle adrloop
			end do !j=0,iva2-iva1   9368
			j_icurl(j_ninc)=iil
			j_reacom(j_ninc)=.false.
			return !adrloop: do i=1,j_o(ivb)%i2(0)
 
		enddo adrloop !loop: do i=1,j_o(ivb)%i2(0)   9364
		sav=.true.
	else !if(ivb.ne.0.and.j_otype(max(ivb,1)).eq.j_iptext)then
		!not saving
		sav=.false.
	end if !if(ivb.ne.0.and.j_otype(max(ivb,1)).eq.j_iptext)   9363
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
			call j_deftext2(0,'Buffer'//char(47+j_ninc),500,10000,50,j_ivbuf(j_ninc))
			sav=.true.
			j_savcom(j_ninc)=.true.
		end if !if(.not.sav)   9392
		call j_puttext2(j_ivbuf(j_ninc), j_inp(ialgo:lin))
 
		if(lena.ne.lin-1-ialgo)goto 17
		do j=0,iva2-iva1
			if(j_o(j_ivnames)%ch(iva1+j).ne.j_inp(j+2:j+2))goto 17
		end do !j=0,iva2-iva1   9400
	else !if(j_inp(ialgo:ialgo).eq.';'.and.j_inp(lin:lin).eq.':')then
		! not adr
		if(sav)call j_puttext(j_ivbuf(j_ninc), j_inp(ialgo:lin))
		goto 17
	end if !if(j_inp(ialgo:ialgo).eq.';'.and.j_inp(lin:lin).eq.':')   9390
 
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
	call j_clearoption(iob,io)
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
 
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_namedv)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)   9458
	n_=j_v(j_o(iob)%i(io+2))
	if(n_.le.0)then
		write(6,*)'*rannegbin: n (=1st arg) must be greater than zero'
		j_err=.true.
 
	endif !if(n_.le.0)   9462
	pp_=j_v(j_o(iob)%i(io+3))
	if(pp_.lt.0.or.pp_.gt.1.)then
		write(6,*)'*rannegbin: p (=2nd arg) must be between 0 and 1, it was ',pp_
		j_err=.true.
	endif !if(pp_.lt.0.or.pp_.gt.1.)   9468
	if(j_err)return
 
	pp_=1.-pp_
 
	if(j_otype(iout).eq.j_ipreal)then
		if(pp_.eq.1.)then
 
			j_v(iout)=j_0
		elseif(pp_.eq.0.)then !if(p_.eq.1.)then
			j_v(iout)=1.7e37
		else !if(p_.eq.1.)then
			j_v(iout)=ignnbn(n_,pp_)
		endif !if(pp_.eq.1.)   9477
 
	elseif(pp_.eq.1.)then
		j_o(iout)%d(1: j_o(iout)%i(3))=j_0
	elseif(pp_.eq.0.)then
		j_o(iout)%d(1: j_o(iout)%i(3))=1.7e37
	else
		do i=1,j_o(iout)%i(3)
 
			j_o(iout)%d(i)=ignnbn(n_,pp_)
 
		enddo !i=1,j_o(iout)%i(3)   9491
	endif !if(j_otype(iout).eq.j_ipreal)   9476
 
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
	endif !if(j_otype(iarg).ne.j_ipmatrix)   9514
	ivrow_=j_igetopt(iob,io,j_mrow)
	if(ivrow_.gt.0)then
		irow=j_v(ivrow_)
		if(irow.le.0.or.irow.gt.j_o(iarg)%i(1))then
			call j_printname('**elementsum in trans-object',iob,' illegal row for matrix ', iarg)
			j_err=.true.  ;return
		endif !if(irow.le.0.or.irow.gt.j_o(iarg)%i(1))   9521
	endif !if(ivrow_.gt.0)   9519
	ivcolumn=j_igetopt(iob,io,j_mcolumn)
	if(ivcolumn.gt.0)then
		icolumn=j_v(ivcolumn)
		if(icolumn.le.0.or.icolumn.gt.j_o(iarg)%i(2))then
			call j_printname('**elementsum in trans-object',iob,' illegal column for matrix ', iarg)
			j_err=.true.  ;return
		endif !if(icolumn.le.0.or.icolumn.gt.j_o(iarg)%i(2))   9529
	endif !if(ivcolumn.gt.0)   9527
 
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
		enddo !i=1,j_o(iarg)%i(1)   9545
		j_v(iout)=su
	else !if(ivrow_.gt.0)then
		j_v(iout)=sum(j_o(iarg)%d(1:j_o(iarg)%i(3)))
	endif !if(ivrow_.gt.0)   9537
 
	case(2) !select case (narg)
	las=j_v( j_o(iob)%i(io+3))
	if(ivrow_.gt.0)then
		if(las.gt.j_o(iarg)%i(2).or.las.le.0)then
			call j_printname('**elementsum in trans-object',iob,' illegal second argument for matrix ', iarg)
			write(6,*)'there are ',j_o(iarg)%i(2), 'columns, you asked ', las
			j_err=.true.
			return
 
		endif !if(las.gt.j_o(iarg)%i(2).or.las.le.0)   9557
 
		ifi=(irow-1)*j_o(iarg)%i(2)
		j_v(iout)=sum(j_o(iarg)%d(ifi+1:ifi+las))
 
	elseif(ivcolumn.gt.0)then !if(ivrow_.gt.0)then
		if(las.le.0.or.las.gt.j_o(iarg)%i(1))then
			call j_printname('**elementsum in trans-object ',iob,' illegal upper bound for matrix:', iarg)
			write(6,*)'#you gave ',las,'maximum is',j_o(iarg)%i(1)
			j_err=.true. ;return
 
		end if !if(las.le.0.or.las.gt.j_o(iarg)%i(1))   9569
		su=0
		ii=icolumn
		do i=1,las
			su=su+j_o(iarg)%d(ii)
			ii=ii+j_o(iarg)%i(2)
		enddo !i=1,las   9577
		j_v(iout)=su
	else !if(ivrow_.gt.0)then
		if(las.le.0.or.las.gt.j_o(iarg)%i(3))then
			call j_printname('**elementsum in trans-object ',iob,' illegal upper bound for matrix:', iarg)
			write(6,*)'#you gave ',las,'maximum is',j_o(iarg)%i(3)
			j_err=.true. ;return
		end if !if(las.le.0.or.las.gt.j_o(iarg)%i(3))   9583
		j_v(iout)=sum(j_o(iarg)%d(1:las))
	endif !if(ivrow_.gt.0)   9556
 
	case(3) !select case (narg)
	ifi=j_v( j_o(iob)%i(io+3) );las=j_v( j_o(iob)%i(io+4) )
	if(ivrow_.gt.0)then
		if(las.gt.j_o(iarg)%i(2).or.ifi.gt.las.or.ifi.le.0)then
			call j_printname('**elementsum in trans-object',iob,' illegal second or third argument for matrix ', iarg)
			write(6,*)'there are ',j_o(iarg)%i(2), 'columns, you asked from ', ifi ,' to ',las
			j_err=.true.
			return
		endif !if(las.gt.j_o(iarg)%i(2).or.ifi.gt.las.or.ifi.le.0)   9594
		ifi=(irow-1)*j_o(iarg)%i(2)
		j_v(iout)=sum(j_o(iarg)%d(ifi+1:ifi+las))
 
 
	elseif(ivcolumn.gt.0)then !if(ivrow_.gt.0)then
		if(las.le.0.or.las.gt.j_o(iarg)%i(2))then
			call j_printname('**elementsum in trans-object ',iob,' illegal upper bound for matrix:', iarg)
			write(6,*)'#you gave ',las,'maximum is',j_o(iarg)%i(2)
			j_err=.true. ;return
		end if !if(las.le.0.or.las.gt.j_o(iarg)%i(2))   9605
		su=0
		ii=icolumn
		do i=1,las
			su=su+j_o(iarg)%d(ii)
			ii=ii+j_o(iarg)%i(2)
		enddo !i=1,las   9612
		j_v(iout)=su
	else !if(ivrow_.gt.0)then
		if(ifi.le.0.or.ifi.gt.j_o(iarg)%i(3).or.las.le.0.or.las.gt.j_o(iarg)%i(3))then
			call j_printname('**vecsum illegal bound for ',iarg,' in trans ', iob)
			write(6,*)'#you gave ',ifi,las,' maximum is',j_o(iarg)%i(3)
			j_err=.true. ;return
		end if !if(ifi.le.0.or.ifi.gt.j_o(iarg)%i(3).or.las.le.0.or.las.gt   9618
		j_v(iout)=sum(j_o(iarg)%d(ifi:las))
	endif !if(ivrow_.gt.0)   9593
	end select !select case (narg)
	call j_clearoption(iob,io)  ! subroutine
	return
end subroutine elementsum !subroutine elementsum(iob,io)
 
subroutine dotproduct(iob,io)  !%%matrix
	integer,intent(in)::iob
	integer,intent(in)::io
	call j_clearoption(iob,io)
 
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
	endif !if(j_otype(iarg).ne.j_ipmatrix)   9642
	if(j_otype(iarg2).ne.j_ipmatrix)then
		call j_printname('**dotproduct in trans-object',iob,' *Not a matrix:', iarg2)
		j_err=.true. ;return
	endif !if(j_otype(iarg2).ne.j_ipmatrix)   9646
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
	end if !if(las.le.0.or.las.gt.nele)   9656
	case(4) !select case (narg)
	ifi=j_v( j_o(iob)%i(io+4) );las=j_v( j_o(iob)%i(io+5) )
	if(ifi.le.0.or.ifi.gt.nele.or.las.le.0.or.las.gt.nele)then
		call j_printname('**dotproduct illegal bound for',iarg,' or for:', iarg2)
		write(6,*)'#you gave ',ifi,las,' maximum is',nele
		j_err=.true. ;return
	end if !if(ifi.le.0.or.ifi.gt.nele.or.las.le.0.or.las.gt.nele)   9663
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
		enddo !i=1,j_gettreevars(0)   9679
	enddo !itre=j_itree1,j_itree2   9677
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
		endif !if(j_otype(iout).eq.j_ipmatrix)   9694
		!	else !if(j_otype(iout).eq.j_ipmatrix)then
		!	call j_getname(iout)
		!	write(6,*)'del ',j_oname(1:j_loname)
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_v(irg)
		!	endif !if(j_otype(iout).eq.j_ipmatrix)then
	else !if(j_otype(j_o(iob)%i(io+2)).eq.j_ipreal)then
		!	write(6,*)'copy',j_o(iob)%i(io+2),j_o(iob)%i(io+3)
		call j_copy2(irg,iout)
 
	endif !if(j_otype(irg).eq.j_ipreal)   9692
 
end subroutine !subroutine assone(iob,io)
 
subroutine assign(iob,io)
	integer, dimension(:), pointer :: argin,argout !arguments of the funct
	double precision::valu
	narin=j_o(iob)%i(io+1)
	narout=j_o(iob)%i(io+2)
	argin=>j_o(iob)%i(io+3:io+2+narin)
	argout=>j_o(iob)%i(io+3+narin:io+2+narin+narout)
	ion=io+narin+narout+3
	if(j_v(j_ivdollar).eq.948)write(6,'(27i5)')narin,narout,argin,argout ,io,ion
	if(j_v(j_ivdollar).eq.948)write(6,'(27i5)')j_o(iob)%i(1:ion+20)
	if(j_v(j_ivdollar).eq.948)write(6,'(27i5)')(j,j=1,ion+20)
	io=io+narin+narout+3
 
	if(narin.eq.1.and.narout.gt.1)then
		!
		if(j_otype(argin(1)).ne.j_ipreal)then
			write(6,*)'only REAL objects can be duplicated'
			j_err=.true.;return
 
		endif !if(j_otype(argin(1)).ne.j_ipreal)   9727
		do i=1,narout
			!	iv=j_o(iob)%i(io+narin+2+i)
			if(j_otype(argout(i)).ne.j_ipreal)call j_del(argout(i))
			j_v(argout(i))=j_v(argin(1))
		enddo !i=1,narout   9732
		return
		!write(6,*)'22assas',io
	elseif(narout.eq.1)then !if(narin.eq.1.and.narout.gt.1)then
		if(j_otype(argout(1)).eq.j_ipmatrix.and.j_otype(argin(1)).eq.j_ipreal)then
 
			nrow=j_o(argout(1))%i(1)
			ncol=j_o(argout(1))%i(2)
			!		j_o(ivout)%i(4)=itype
			ibas=0
 
			if(j_o(argout(1))%i(4).eq.j_matdiag)then
 
 
				do i=1,nrow
					if(i.le.narin)valu=j_v(argin(i))
					j_o(argout(1))%d(ibas+i)=valu
					ibas=ibas+nrow
 
				enddo !i=1,nrow   9750
 
			else !if(j_o(argout(1))%i(4).eq.j_matdiag)then
				iel=0
				do i=1,nrow
					do j=1,ncol
						iel=iel+1
						if(iel.le.narin)valu=j_v(argin(i))
						j_o(argout(1))%d(iel)=valu
					enddo !j=1,ncol   9760
				enddo !i=1,nrow   9759
 
			endif !if(j_o(argout(1))%i(4).eq.j_matdiag)   9747
			return
		endif !if(j_otype(argout(1)).eq.j_ipmatrix.and.j_otype(argin(1)).   9740
	endif !if(narin.eq.1.and.narout.gt.1)   9725
 
	if(narin.eq.narout)	then
		do i=1,narin
			if(j_otype(argin(i)).eq.j_ipdata)then
				call j_printname('input ',argin(i),' is DATA and it cannot be copied')
				j_err=.true.;return
 
			endif !if(j_otype(argin(i)).eq.j_ipdata)   9774
			if(j_v(j_ivdebug).ge.2)write(6,*)'making copy of ',argin(i), ' to ',argout(i),j_otype(argin(i))
			call j_copy2(argin(i),argout(i))
		enddo !i=1,narin   9773
		!	io=io+2*narin+3
		!	write(6,*)'22tas  ',io
		return
	endif !if(narin.eq.narout)   9772
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
	endif !if(iprin.eq.0)   9813
	if(j_v(j_ivprintinput).ge.3)inprint=1
	call j_clearoption(iob,io)  ! subroutine
	j_inpara=.true.
	1 call j_getinput('prop>',inprint)
	if(j_err.or.j_stop)return
	if(j_inp(1:1).eq.'/')goto 90
	iip=index(j_inp(1:j_linp),',')
	iip2=j_lentrim(j_inp(1:iip-1))
	if(iip2.le.0)then
		write(6,*)'format of properties: name, .. (values)'
		goto 99
	end if !if(iip2.le.0)   9826
	do i=1,narg
		call j_getline(j_ivnames,j_o(iob)%i(io+1+i),name1,le)
		if(j_err)return
		call j_getobject(0,j_inp(1:iip2)//'%'//name1(1:le),j_ipreal,linevar(i))
		if(j_err) goto 99
	end do !i=1,narg   9830
	read(j_inp(iip+1:j_linp),*,err=99,end=99)(j_v(linevar(j)),j=1,narg)
	if(inprint.gt.0)then
		do j=1,narg
			call j_getname(linevar(j))
			write(6,*)j_oname(1:j_loname),j_v(linevar(j))
		enddo !j=1,narg   9838
		write(6,*)' '
		!	write(6,*)j_inp(1:iip),(j_v(linevar(j)),j=1,narg)
	endif !if(inprint.gt.0)   9837
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
	integer,intent(in)::iob
	integer,intent(in)::io
	logical maketran
	! Section datawcase datawcase() DATA with case names
	! This function will replace the properties function in factory optimizations.
	! The function creates subobjects Output%matrix,Output%keep, and Output%case, which is a list of case names.
	!
	!endheader
	!Option
	! Output &0|1&Data&
	! Data object to be created.
	! read &0|1-&REAL|List& Variables read from the following input paragraph.
	! maketrans &0|1&TRANS& Transformations made. Output variables whose names do not start with $ are put into the data.
	!endoption
	!Ex datawcaseex Example of datawcase
	!sawmills=datawcase(read->(capacity1...capacity4))
	!Kotka,110,120,130,140
	!Oulu,210,220,230,240
	!/
	!sawsub=;list(sawmills%?);
	!@sawsub;
	!sawmills%?;
	!stat()
	!endex
	! endsection
	double precision,dimension(:), allocatable::tempdata,tempdata2
	integer,dimension(:), allocatable::tempcases
	integer*8::nobs,nkeep
 
	!character*24 name1
	!		integer,dimension(:),allocatable::linevar
	call j_startfunction(iob,io,0,narg,j_optarg0,iout)
	if(j_err)return
	call j_getobject(iout,'%obs',j_ipreal,ivobs)
	call j_getname(iout)
	write(6,*)
	write(6,*)'Making data with cases: ',j_oname(1:j_loname)
	call j_getoption(iob,io,j_mread,1,99999,j_ipreal,.true.,nread,j_optarg);if(j_err)return
 
	call j_getoption(iob,io,j_mmaketrans,-1,1,j_iptrans,.true.,noptarg,j_optarg0);if(j_err)return
	!	write(6,*)'makenop',noptarg,j_optarg0(1)
	if(noptarg.gt.0)then
		maketran=.true.
		ivmaketrans=j_optarg0(1)
		ivoul=j_trans_output(ivmaketrans)
		noutv=j_o(ivoul)%i(1)
		call j_deflistobject(iout,'%keep',ivkeep,list0=nread,list=j_optarg,nres=noutv)
 
		iper=j_putlistobject(ivkeep,ivin=ivoul)
 
 
		!	write(6,*)'<4554',noutv,ivoul,j_o(ivoul)%i2(1:noutv)
	else !if(noptarg.gt.0)then
		maketran=.false.
		noutv=0
		call j_deflistobject(iout,'%keep',ivkeep,list0=nread,list=j_optarg)
		!	write(6,*)'xhshs ivkeep ',ivkeep
	end if !if(noptarg.gt.0)   9898
	call j_clearoption(iob,io)  ! subroutine
	nkeep=j_o(ivkeep)%i(1)
	!	io=
	iiv=j_inciv( j_ninc)
	maxl=j_o(iiv)%i(5)
	allocate(tempdata(maxl*nkeep))
	allocate(tempcases(maxl))
	nobs=0
	ibas=0
	1 call j_getinput('prop>',0,single=.true.)
	if(j_err.or.j_stop)return
	if(j_inp(1:1).eq.'/')goto 90
	iip=index(j_inp(1:j_linp),',')
	nobs=nobs+1
	iv=j_object(j_inp(1:iip-1))
	!		write(6,*)'nobsiv',nobs,iv
	if(iv.le.0)call j_getobject(0,j_inp(1:iip-1),j_ipreal,iv)
	ivr=j_inlistobject(iv,ivkeep)
	if(ivr.gt.0)then
		write(6,*)'*case ',j_inp(1:iip-1), 'for obs ',nobs,' is element ',ivr, &
			'among read-> variables '
		j_err=.true.
		return
 
	endif !if(ivr.gt.0)   9933
	!write(6,*)'nobsi3v',nobs,iv
	tempcases(nobs)=iv
	read(j_inp(iip+1:j_linp),*,end=99,err=99)tempdata(ibas+1:ibas+nread)
	if(maketran)then
		j_v(j_optarg)=tempdata(ibas+1:ibas+nread)
		call dotrans(ivmaketrans,1)
		tempdata(ibas+1:ibas+nkeep)	=j_v(j_o(ivkeep)%i2(1:nkeep))
 
	endif !if(maketran)   9943
	ibas=ibas+nkeep
 
 
 
	goto 1
 
 
	90 continue !io=io+narg+3
!	write(6,*)'nobs',nobs
	ivmat=j_defmatrix8(iout,'%matrix',nobs,nkeep,j_matreg) !
	j_o(ivmat)%d=tempdata(1:nobs*nkeep)
	nobsi=nobs
	call j_deflistobject(iout,'%case',ivcases,list0=nobsi,list=tempcases)
 
	do iv=1,nobs
		j_v(tempcases(iv))=iv
	enddo !iv=1,nobs   9963
	!	write(6,*)'hep'
	!	 subroutine j_defdata(iv,ivmat,ivkeep,ivsub,ivnobsw,&
	!	 ivup,ivobs,ivobsw,ivnobswcum) !%%data
	call j_defdata(iout,ivmat,ivkeep,ivcases=ivcases) !%%data
	j_dlastdata=iout
	j_v(j_ivlastdata)=iout
	! write(6,*)'hep2'
	goto 999
	99 write(6,*)'error in properties, reading line:'
	write(6,*)j_inp(1:j_linp)
	j_err=.true.
999		deallocate(tempcases,tempdata)
	return
end subroutine datawcase !subroutine properties(iob,io)
 
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
	! Args&1-&REAL&
	! Variables for which class means are computed.
 
	! @@data
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
	! @@trans
	! @@filter
	! @@reject
 
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
	!endsection
 
	double precision,dimension(:), allocatable::value9,value2
	integer, dimension(:),allocatable :: iperm  !for sorting
	integer, dimension (:,:),allocatable:: freqtot
	double precision ,dimension(:,:), allocatable::xs
	double precision ,dimension(:), allocatable::xsx,xssx,sumwtx  ! for class means of x var
	double precision ,dimension(:,:), allocatable::xss
	double precision ,dimension(:,:), allocatable::sumwt
	integer, dimension (:),allocatable:: prevc,nextc
	double precision dinobs,dapu,pros,cumpros
	double precision dx,dz,xmin,xmax,zmin,zmax,yvalue,xw
	integer :: ix_
	logical xrange,zrange,isfunc,isy,print0,istailtofirst,istailtolast
	logical::p
	integer*8::nval,nval2,i,j,nrow_
	integer*8::iobs
	p=j_v(j_ivdebug).gt.0
	call j_checkoutput(iob,io)
	if(j_err)return
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
 
	iy=0
	call j_getdataobject(iob,io)
	print0=j_dprint.gt.0
	linkfunc=j_codelink(iob,io,j_mfunc)
	isfunc=linkfunc.ne.0
	if(j_err)return
	jnobstot=j_dnobs
	isy=isfunc
	if(narg.gt.0)then
		iy=j_o(iob)%i(io+2)
		isy=.true.
		if(print0)call j_printname('y-variable: ',iy,' ')
		if(isfunc)then
			write(6,*)'cannot have y-variable and func-> simulataneously'
			j_err=.true.;return
 
		endif !if(isfunc)  10074
	endif !if(narg.gt.0)  10070
 
	ix_=j_igetopt(iob,io,j_mx)
	if(ix_.le.0)then
		write(6,*)'** x-> not given in classify';j_err=.true.;return
	end if !if(ix_.le.0)  10082
	iz=j_igetopt(iob,io,j_mz)
	iout=j_o(iob)%i(io+2+narg)
	!iprint=2
	!	write(6,*)'<634646',j_linkoption(iob,io,j_mxrange),j_linkoption(iob,io,j_many)
	if(j_linkoption(iob,io,j_mxrange).lt.0.and.j_linkoption(iob,io,j_many).lt.0)then
		write(6,*)'either any-> or xrange-> must be present'
		j_err=.true.;return
	endif !if(j_linkoption(iob,io,j_mxrange).lt.0.and.j_linkoption(io  10089
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
 
 
		endif !if(j_linkoption(iob,io,j_mdx).gt.0)  10100
		if(j_linkoption(iob,io,j_mclasses).ge.0)then
			write(6,*)'classes-> requires xrange->'
			j_err=.true.;return
 
		endif !if(j_linkoption(iob,io,j_mclasses).ge.0)  10106
		xrange=.false.
		if(allocated(value9))deallocate(value9)
		allocate(value9(1:jnobstot))
		if(allocated(iperm))deallocate(iperm)
		allocate(iperm(1:jnobstot))
	elseif(nr.eq.0)then !if(nr.lt.0)then
		ii1=j_object3(ix_,'%min')
		ii2=j_object3(ix_,'%max')
		!	write(6,*)'ii1,ii2',ii1,ii2
		if(ii1.le.0.or.ii2.le.0)then
			write(6,*)'...%min  or ...%max does not exist'
			j_err=.true. ;return
 
		endif !if(ii1.le.0.or.ii2.le.0)  10120
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
 
 
	endif !if(nr.lt.0)  10099
 
 
	if(xrange)then
		if(xmax.le.xmin)then
			write(6,*)'**classify: illegal xrange :',xmin,xmax
			j_err=.true. ;return
 
		endif !if(xmax.le.xmin)  10143
		li=j_linkoption(iob,io,j_mclasses)
		nargoc=0
		if(li.gt.0)nargoc=j_o(iob)%i(li)
		if(nargoc.gt.0)then
			clasn=j_v(j_o(iob)%i( li+1))
		else !if(nargoc.gt.0)then
			clasn=7.
		endif !if(nargoc.gt.0)  10151
		if(nargoc.ge.2)then
			clasz=j_v(j_o(iob)%i( li+2))
		else !if(nargoc.ge.2)then
			clasz=7.
		endif !if(nargoc.ge.2)  10156
 
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
		end if !if(nargopt.gt.0)  10167
	end if !if(xrange)  10142
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
				endif !if(size(iperm).lt.jnobstot)  10189
			else !if(allocated(iperm))then
				allocate(iperm(1:jnobstot))
			endif !if(allocated(iperm))  10188
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
			end if !if(nargopt.ge.1)  10201
 
		end if !if(nargopt.lt.2)  10185
	endif !if(iz.gt.0)  10184
	call	j_getoption(iob,io,j_mmean,-1,1,j_ipreal,.true.,igmean,j_optarg0);if(j_err)return
	if(igmean.gt.0)igmean=j_optarg0(1)
	!	igmean= j_igetopt(iob,io,j_mmean)
	minobs=j_igetopt(iob,io,j_mminobs)
	if(minobs.gt.0)then
		minobs=j_v(minobs)
		dinobs=minobs ! v(minobs)
		if(iz.gt.0)then
			write(6,*)'classify: minobs-> does not work with z-> (yet)'
		endif !if(iz.gt.0)  10219
	endif !if(minobs.gt.0)  10216
	! ivnobs=j_igetoptout(iob,j_mnobs)
 
	! if(ivnobs.eq.0) then
	! call j_getobject(0,'Nobs',j_ipreal,ivnobs)
	! !	if(j_err) return
	! endif !if(ivnobs.eq.0) then
	istailtofirst=j_isopt(iob,io,j_mtailtofirst)
	istailtolast=j_isopt(iob,io,j_mtailtolast)
	call j_clearoption(iob,io)  ! subroutine
	if(.not.xrange.and.iz.gt.0.and..not.zrange)then
		call j_getvalues(ix_,value9,nval,iz,value2,nval2)
	elseif(.not.xrange.and.(iz.le.0.or.zrange))then !if(.not.xrange.and.iz.gt.0.and..not.zrange)then
		call j_getvalues(ix_,value9,nval,0,value9,nval2)
	elseif(xrange.and.(iz.gt.0.and..not.zrange))then !if(.not.xrange.and.iz.gt.0.and..not.zrange)then
		call j_getvalues(iz,value2,nval2,0,value2,nval)
	endif !if(.not.xrange.and.iz.gt.0.and..not.zrange)  10232
	!	write(6,*)'values ',value9(1:nval)
	if(j_err)return
	if(.not.xrange)call j_quick_sort(value9(1:nval),iperm)
	!	write(6,*)'values ',value9(1:nval)
	if(iz.gt.0.and..not.zrange)call j_quick_sort(value2(1:nval2),iperm)
	j2=0
	allocate(freqtot(0:nval2,0:nval))
	if(isy)then
		allocate(xs(0:nval2,0:nval),xss(0:nval2,0:nval),sumwt(0:nval2,0:nval))
		xs=0.d0;xss=0.d0;sumwt=0.d0
	end if !if(isy)  10246
	xw=j_1
	allocate(xsx(0:nval),xssx(0:nval),sumwtx(0:nval))
	xsx=0.d0;xssx=0.d0;sumwtx=0.d0
	freqtot=0
	nobst=0
	missing=0
	!do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
	do iobs=j_dfrom,j_duntil
		call j_getobs(iobs)
		if(j_err)return
 
		if(j_rejected)cycle
		if(abs(j_v(ix_)).ge.1.7d19)then
			missing=missing+1
			cycle
		endif !if(abs(j_v(ix_)).ge.1.7d19)  10263
		if(iz.gt.0)then
			if(abs(j_v(iz)).ge.1.7d19)then
				missing=missing+1
				cycle
			endif !if(abs(j_v(iz)).ge.1.7d19)  10268
 
		endif !if(iz.gt.0)  10267
		if(iy.gt.0)then
			if(abs(j_v(iy)).ge.1.7d19)then
				missing=missing+1
				cycle
			endif !if(abs(j_v(iy)).ge.1.7d19)  10275
		endif !if(iy.gt.0)  10274
 
		j2=0
		if(xrange)then
 
			if(.not.istailtofirst.and.j_v(ix_).lt.xmin-0.000000001d0)cycle
			if(.not.istailtolast.and.j_v(ix_).gt.xmax+0.000000001d0)cycle
			j=(j_v(ix_)-xmin)/dx+1
			j=min(j,nval)
			j=max(j,1)
			!if(j.lt.0)cycle
 
 
		else !if(xrange)then
			do j=1,nval
				if(abs(value9(j)-j_v(ix_)).lt.1.d-15)exit
			enddo !j=1,nval  10293
 
		endif !if(xrange)  10282
		if(iz.gt.0)then
			if(zrange)then
				if(j_v(ix_).lt.zmin-0.000000001d0)cycle
				if(j_v(ix_).gt.zmax+0.000000001d0)cycle
				j2=(j_v(iz)-zmin)/dz+1
				j2=min(j2,nval2)
				j2=max(1,j2)
 
			else !if(zrange)then
				do j2=1,nval2
					if(abs(value2(j2)-j_v(iz)).lt.1.d-15)exit
				enddo !j2=1,nval2  10307
 
			end if !if(zrange)  10299
		end if !if(iz.gt.0)  10298
		call j_msd21(j_v(ix_),xsx(j),xssx(j),xw,sumwtx(j))
		call j_msd21(j_v(ix_),xsx(0),xssx(0),xw,sumwtx(0))
		nobst=nobst+1
		freqtot(j2,j)=freqtot(j2,j)+1
		if(isy)then
			if(isfunc)then
				yvalue=j_codevalue(iob,linkfunc)
			else !if(isfunc)then
				yvalue=j_v(iy)
			endif !if(isfunc)  10318
			call j_msd21(yvalue,xs(j2,j),xss(j2,j),xw,sumwt(j2,j))
			call j_msd21(yvalue,xs(0,0),xss(0,0),xw,sumwt(0,0))
 
		end if !if(isy)  10317
		if(j2.gt.0)then
			freqtot(0,j)=freqtot(0,j)+1
			freqtot(j2,0)=freqtot(j2,0)+1
 
			if(isy)then
				!	if(i.lt.20)write(6,*)j_v(ix_),j_v(iz),j,j2,j_v(iy)
 
				call j_msd21(yvalue,xs(j2,j),xss(j2,j),xw,sumwt(j2,j))
				call j_msd21(yvalue,xs(0,j),xss(0,j),xw,sumwt(0,j))
				call j_msd21(yvalue,xs(j2,0),xss(j2,0),xw,sumwt(j2,0))
			end if !if(isy)  10331
 
		endif !if(j2.gt.0)  10327
	enddo !iobs=j_dfrom,j_duntil  10258
	!	enddo !do k=1,jndatasetss
	!if(ivnobs.gt.0)j_v(ivnobs)=nobst
	write(6,*)'Accepted ',nobst, ' out of ',j_dnobs
	j_v(j_ivaccepted)=nobst
	if(nobst.le.0)then
		write(6,*)'nothing accepted'
		return
 
	endif !if(nobst.le.0)  10345
	if(minobs.gt.0)then
		! if(nobst.lt.minobs)then
		!write(6,*)'*classify: minobs',minobs,' > number of accpeted obs ',nobst
		! if(ivnobs.gt.0)return
		!write(6,*)'if nobs-> is given, then return without error'
		! j_err=.true.
 
		! return
		! endif !if(nobst.lt.minobs)then
 
		allocate(nextc(0:nval),prevc(0:nval))
		do i=0,nval
			nextc(i)=i+1
			prevc(i)=i-1
		enddo !i=0,nval  10361
		nextc(nval)=0
		prevc(0)=nval
		i0=0
		nmer=0
		156 i0=nextc(i0)
		155 if(sumwtx(i0).lt.dinobs.and.sumwtx(i0).gt.0)then
			nmer=nmer+1
			if((prevc(i0).ne.0.and.xsx(i0)-xsx(prevc(i0)).lt.xsx(nextc(i0))-xsx(i0)).or.&
					nextc(i0).eq.0)then
				i2=prevc(i0)
			else !nextc(i0).eq.0)then
				i2=nextc(i0)
			end if !if((prevc(i0).ne.0.and.xsx(i0)-xsx(prevc(i0)).lt.xsx(nextc  10372
			! (x-xm)**2=
			dapu=(sumwt(0,i0)*xs(0,i0)+sumwt(0,i2)*xs(0,i2))/(sumwt(0,i0)+sumwt(0,i2))
			! sumc=sum2-n*mean**2
			! totall sum2= sumc1+sumc2+n1*mean1**2+n2*mean2**2
			! toal mean= dapu
			xss(0,i0)=xss(0,i0)+xss(0,i2)+sumwt(0,i0)*xs(0,i0)**2+sumwt(0,i2)*xs(0,i2)**2 &
				-(sumwt(0,i0)+sumwt(0,i2))*dapu*dapu
			xs(0,i0)=dapu
			sumwt(0,i0)=sumwt(0,i0)+sumwt(0,i2)
			xsx(i0)=(sumwtx(i0)*xsx(i0)+sumwtx(i2)*xsx(i2))/(sumwtx(i0)+sumwtx(i2))
			sumwtx(i0)=sumwtx(i0)+sumwtx(i2)
			freqtot(0,i0)=sumwtx(i0)
			xsx(i2)=0.d0;xssx(i2)=0.d0;sumwtx(i2)=0.d0
			xs(0,i2)=0.d0;xss(0,i2)=0.d0;sumwt(0,i2)=0.d0
			freqtot(0,i2)=0.
			if(i2.lt.i0)then
				nextc(prevc(i2))=i0
				prevc(i0)=prevc(i2)
			else !if(i2.lt.i0)then
				nextc(i0)=nextc(i2)
				prevc(nextc(i0))=i0
			endif !if(i2.lt.i0)  10393
			goto 155
		else !if(sumwtx(i0).lt.dinobs.and.sumwtx(i0).gt.0)then
			if(nextc(i0).ne.0)goto 156
		end if !155 if(sumwtx(i0).lt.dinobs.and.sumwtx(i0).gt.0)  10370
		write(6,*)'number of classes merged ',nmer
		deallocate (nextc,prevc)
	endif !if(minobs.gt.0)  10350
	if(iout.ne.j_ivresult)then
		if(iz.gt.0)then
			ivmat=j_defmatrix8(iout,' ',nval2+2_8,nval+2_8,j_matclass)
			if(xrange)then
				j_o(ivmat)%d((nval2+2_8)*(nval+2_8)+j_18)=xmin
				j_o(ivmat)%d((nval2+2_8)*(nval+2)+2_8)=dx
				do i=1,nval
					j_o(ivmat)%d(i+1)=xmin+(i-0.5)*dx
					!	call j_putmatrix(ivmat,1,i+1,xmin+(i-0.5)*dx)
				end do !i=1,nval  10413
			else !if(xrange)then
				j_o(ivmat)%d(2:nval+1)=value9(1:nval)
			end if !if(xrange)  10410
			if(zrange)then
				j_o(ivmat)%d((nval2+2)*(nval+2)+3)=zmin
				j_o(ivmat)%d((nval2+2)*(nval+2)+4)=dz
				do i=1,nval2
					j_o(ivmat)%d(i+1)=zmin+(i-0.5)*dz
					!	call j_putmatrix(ivmat,i+1,1,zmin+(i-0.5)*dz)
				end do !i=1,nval2  10423
			else !if(zrange)then
				do i=1,nval2
					j_o(ivmat)%d(i+1)=value2(i)
					!	call j_putmatrix(ivmat,i+1,1,value2(i))
				end do !i=1,nval2  10428
			end if !if(zrange)  10420
			do i=1,nval
				do j=1,nval2
					j_dapu=freqtot(j,i)
					call j_putmatrix8(ivmat,j+j_18,i+j_18,j_dapu)
				end do !j=1,nval2  10434
				j_dapu=freqtot(0,i)
				call j_putmatrix8(ivmat,nval2+2_8,i+j_18,j_dapu)
			enddo !i=1,nval  10433
			do j=1,nval2
				j_dapu=freqtot(j,0)
				call j_putmatrix8(ivmat,j+j_18,nval+2_8,j_dapu)
			enddo !j=1,nval2  10441
			j_dapu=nobst
			call j_putmatrix8(ivmat,nval2+2_8,nval+2_8,j_dapu)
			if(igmean.gt.0)then
				ivmat=j_defmatrix8(igmean,' ',nval2+2_8,nval+2_8,j_matclass)
				if(xrange)then
					do i=1,nval
						j_o(ivmat)%d(i+1)=xmin+(i-0.5)*dx
						!			call j_putmatrix8(ivmat,1,i+1,xmin+(i-0.5)*dx)
					end do !i=1,nval  10450
				else !if(xrange)then
					j_o(ivmat)%d(2:nval+1)=value9(1:nval)
				end if !if(xrange)  10449
				if(zrange)then
					do i=1,nval2
						call j_putmatrix8(ivmat,i+j_18,j_18,zmin+(i-0.5)*dz)
					end do !i=1,nval2  10458
				else !if(zrange)then
					do i=1,nval2
						call j_putmatrix8(ivmat,i+j_18,j_18,value2(i))
					end do !i=1,nval2  10462
				end if !if(zrange)  10457
				do i=1,nval
					do j=1,nval2
						call j_putmatrix8(ivmat,j+j_18,i+j_18,xs(j,i))
					end do !j=1,nval2  10467
					call j_putmatrix8(ivmat,nval2+2_8,i+j_18,xs(0,i))
				enddo !i=1,nval  10466
				do j=1,nval2
					call j_putmatrix8(ivmat,j+j_18,nval+2_8,xs(j,0))
				enddo !j=1,nval2  10472
				call j_putmatrix8(ivmat,nval2+2_8,nval+2_8,xs(0,0))
			end if !if(igmean.gt.0)  10447
		else !if(iz.gt.0)then
			nrow_=2
			if(isy)nrow_=4
			ivmat=j_defmatrix8(iout,' ',nrow_,nval+j_18,j_matclass)
			if(p)write(6,*)'allocating matrix ',nrow_,nval+1
			if(xrange)then
				! extra positions
				j_o(ivmat)%d(nrow_*(nval+1)+1)=xmin
				j_o(ivmat)%d(nrow_*(nval+1)+2)=dx
				do i=1,nval
					j_o(ivmat)%d(i)=xsx(i)  ! xmin+(i-0.5)*dx
				end do !i=1,nval  10486
				j_o(ivmat)%d(nval+1)=xsx(0)
			else !if(xrange)then
				j_o(ivmat)%d(1:nval)=value9(1:nval)
			end if !if(xrange)  10482
			j_o(ivmat)%d(nval+2:2*nval+1)=freqtot(0,1:nval)
			j_o(ivmat)%d(2*nval+2)=nobst
			if(isy)then
				j_o(ivmat)%d(2*(nval+1)+1:3*(nval+1)-1)=xs(0,1:nval)
				j_o(ivmat)%d(3*(nval+1))=xs(0,0)
				do j=0,nval
					if(sumwt(0,j).gt.1.d0.and.xss(0,j).gt.0.d0)then
						haj=sqrt(xss(0,j)/(sumwt(0,j)-1.d0))
					else !if(sumwt(0,j).gt.1.d0.and.xss(0,j).gt.0.d0)then
						haj=0.d0
					end if !if(sumwt(0,j).gt.1.d0.and.xss(0,j).gt.0.d0)  10499
					j2=j
					if(j.eq.0)j2=nval+1
					j_o(ivmat)%d(3*(nval+1)+j2)=haj
				end do !j=0,nval  10498
			end if !if(isy)  10495
		end if !if(iz.gt.0)  10408
		if(isfunc)iy=j_ivresult
		j_o(ivmat)%i2=(/ix_,iz,iy/)
	end if !if(iout.ne.j_ivresult)  10407
 
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
 
		endif !if(zrange)  10522
		write(6,*)('_',ij=1,80)
		!write(6,'(8x,9f8.2/)')(xs(j2,j),j=1,nval),xs(j2,0)
		! end if !if(iy.gt.0)then
		! enddo !do j2=1,nval2
 
		if(isy)then
			do j=1,nval
				if(xrange)then
					write(6,'(15f9.3)')xmin+(j-1.)*dx,xmin+j*dx,(xs(j2,j),j2=1,nval2),xs(0,j)
				else !if(xrange)then
					write(6,'(15f9.3)')value9(j),(xs(j2,j),j2=1,nval2),xs(0,j)
				endif !if(xrange)  10537
				write(6,'(9x,15I9)')(freqtot(j2,j),j2=1,nval2),freqtot(0,j)
				write(6,*)' '
			enddo !j=1,nval  10536
 
		else !if(isy)then
			do j=1,nval
				if(xrange)then
					write(6,'(2f9.3,(15i9/)  )')xmin+(j-1)*dx,xmin+j*dx,(freqtot(j2,j),j2=1,nval2)
				else !if(xrange)then
					write(6,'(f9.3,(15i9/)  )')value9(j),(freqtot(j2,j),j2=1,nval2),freqtot(0,j)
				endif !if(xrange)  10548
				write(6,*)' '
			enddo !j=1,nval  10547
		endif !if(isy)  10535
 
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
			call j_printname(' ',ix_,'                      n      y-mean       sd y    se of mean')
		else !if(isy)then
			call j_printname(' ',ix_,'                       n      %      cum %')
			cumpros=j_0
		endif !if(isy)  10574
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
						!			write(6,456)xsx(j),freqtot(0,j),xs(0,j),sdy,sdy/sqrt(real(freqtot(0,j)))
						write(6,456)xmin+(j-1.)*dx,' -  ',xmin+j*dx,freqtot(0,j),xs(0,j),sdy,sdy/sqrt(real(freqtot(0,j)))
						! else
						!	write(6,456)xmin+(j+0.5)*dx,freqtot(0,j),xs(0,j),j_o(ivmat)%d(3*(nval+1)+j)
					endif !if(freqtot(0,j).gt.0)  10599
				enddo !j=1,nval  10597
				write(6,*)'---------------'
				write(6,4561)xsx(0),nobst,xs(0,0),j_o(ivmat)%d(3*(nval+1)+nval+1)
			else !if(j_dprint.ge.2)then
				do j=1,nval
					!		write(6,456)xsx(j),freqtot(0,j),xs(0,j),xs(0,0)
					write(6,456)xmin+(j-1.)*dx,' -  ',xmin+j*dx,freqtot(0,j),xs(0,j),xs(0,0)
				enddo !j=1,nval  10610
				write(6,*)'---------------'
				write(6,4561)xsx(0),nobst,xs(0,0)
			endif !if(j_dprint.ge.2)  10594
 
		elseif(xrange)then !if(xrange.and.isy)then
			do j=1,nval
				!		write(6,456)xsx(j),freqtot(0,j)
				pros=100.d0*freqtot(0,j)/nobst
				cumpros=cumpros+pros
				write(6,456)xmin+(j-1.)*dx,'  - ',xmin+j*dx,freqtot(0,j),pros,cumpros
			enddo !j=1,nval  10619
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
			do j=1,nval
				sdy=j_o(ivmat)%d(3*(nval+1)+j)
				write(6,4561)value9(j),freqtot(0,j),xs(0,j),sdy,sdy/sqrt(real(freqtot(0,j)))
			enddo !j=1,nval  10639
			write(6,*)'---------------'
			sdy=j_o(ivmat)%d(3*(nval+1)+nval+1)
			write(6,4561)xsx(0),nobst,xs(0,0),sdy,&
				sdy/sqrt(real(nobst))
			!endif
		else !if(xrange.and.isy)then
			do j=1,nval
				write(6,4561)value9(j),freqtot(0,j)
			enddo !j=1,nval  10649
			write(6,*)'-----------'
			write(6,*)'       ',nobst
 
		endif !if(xrange.and.isy)  10593
		!	endif
	end if !if(iz.gt.0)  10515
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
 
subroutine show(iob,io)
	! ! Section show show() Plots FIG
	! ! An figure stored in a figure object or in Gnuplot file can be plotted. If the
	! ! argument is FIGURE, the
	! ! parameters of the figure can be changed. If the argument is the name of
	! ! Gnuplot file, the file must be edited.
	! !endheader
	! ! Option
	! ! Args & 1& FIGURE | CHAR & The figure object or the name of the file containg Gnuplot commans,
	! ! @@figure
	! ! endoption
	! ! Note If the argument is the file name with .jfig extension, and you edit the file, its is safe to change the name,
	! ! becase if an figure with teh same name is generated, the edited fike is autimatically
	! ! deleted. If the file refers other files, it is wise to rename also these files and change
	! ! the names in the beginning of the .jfig file.
	! !endnote
	! ! Note You may wish to use show also if you cnange the window size
	! ! endnote
 
 
	!!endsection
 
 
	logical p
	call  j_startfunction(iob,io,0,narg,j_optarg0,ivout)
	ivfig=j_optarg0(1)
	if(j_err)return
	if(j_otype(ivfig).eq.j_ipchar)then
		call j_showfig(ivfig)
	elseif(j_otype(ivfig).eq.j_ipfigure)then !if(j_otype(ivfig).eq.j_ipchar)then
 
		call j_startfig(iob,io,update=.true.)
		if(j_err)return
		call j_showfig(ivfig)
	else !if(j_otype(ivfig).eq.j_ipchar)then
		call j_printname('argument ',ivfig,' not FIGURE or CHAR for file name')
		j_err=.true.
	endif !if(j_otype(ivfig).eq.j_ipchar)  10696
	call j_clearoption(iob,io)
	return
end subroutine show !subroutine show(iob,io)
 
subroutine plot3d(iob,io)
	! Section plot3d plot3d() 3d-figure.
	! Plot 3d-figure with indicater contours  with colours.
	! endheader
	! Option
	! Output & 1  & & ]fi[=plot3d() generates Gnuplot file ]fi[.jfig.
	! No figure object is produced.
	! Args& 1 & MATRIX & The argument is a matrix having 3 columns for x,y and z.
	! sorted & N | 1 & &plot3d() uses the Gnuplot function splot, which requires that the data
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
	! call(transa)
	! fi=plot3d(mat,sorted->,continue->fcont)
	!** This is commneted becaseu it takes some time
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
	j_gpcontinue=j_isopt(iob,io,j_mcontinue)
	if(j_otype(imat).ne.j_ipmatrix)then
		call j_getname(imat)
		write(6,*)'plot3d needs a matrix argument ',j_oname(1:j_loname), ' is ',&
			j_objecttypes(j_otype(imat))
		j_err=.true.;return
	endif !if(j_otype(imat).ne.j_ipmatrix)  10773
	nrow=j_o(imat)%i(1)
	if(j_o(imat)%i(2).ne.3)then
		write(6,*)'argument must have 3 columns'
		j_err=.true.
		return
 
	endif !if(j_o(imat)%i(2).ne.3)  10780
	sorted=j_linkoption(iob,io,j_msorted,clear=.true.).ge.0
 
	if(.not.sorted)then
		allocate(xi(1:nrow),p(1:nrow))
		ibas=1
		do i=1,nrow
			xi(i)=j_o(imat)%d(ibas)
 
			ibas=ibas+3
		end do !i=1,nrow  10791
		call SSORTP (xi, 1, nrow, p)
 
	endif !if(.not.sorted)  10788
	ibas=3
	do i=1,nrow
		zmin=min(zmin,j_o(imat)%d(ibas))
		zmax=max(zmax,j_o(imat)%d(ibas))
		ibas=ibas+3
	enddo !i=1,nrow  10800
	call j_getfile(nu,rw='w',ivout=iv,ivfile=iout,ext='.jfig',replace=.true.)
	if(j_err)return
	call j_getchar(iv,j_filename,le)
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
			endif !if(i.lt.nrow)  10822
			ibas=ibas+3
		enddo !i=1,nrow  10820
	else
		j=(p(1)-1)*3
		do i=1,nrow
			write(nu,'(3g12.6)')j_o(imat)%d(j+1:j+3)
			if(i.lt.nrow)then
				j2=(p(i+1)-1)*3
				if(j_o(imat)%d(j+1).ne.j_o(imat)%d(j2+1))write(nu,'(a)')' '
				j=j2
			endif !if(i.lt.nrow)  10831
		end do !i=1,nrow  10829
	endif !if(sorted)  10818
	if(allocated(xi))deallocate(xi,p)
	call j_closeunit(nu)
	call execute_command_line('gnuplot --persist '//j_filename(1:le), wait=.false.)
	!	 call execute_command_line('gnuplot --persist gnuclear.txt', wait=.false.)
	!	 old=.true.
	!	 	 call execute_command_line('gnuplot -- '//j_filename(1:le), wait=.false.)
	if(j_gpcontinue.or.j_ispause)return
	!	write(6,*)'pausetassa ',j_cline
	if(j_ninc.gt.1)call j_pause('<fig>',do=.true.)
end subroutine
 
subroutine clearbits(iob,io)
	! Section clearbits clearbits() Clears bits
	! To be reported later
	! endsection
 
	real rw
	integer iw
	equivalence(iw,rw)
	call j_clearoption(iob,io)
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
		end if !if(j.gt.32.or.j.le.0)  10870
		!   ii=(j-1)/32+1
		ibit=j-1  ! bit numbering starts from zero
		rw=j_v(ivb)
		iw= ibclr(iw,ibit)
		j_v(ivb)=rw
	end do !i=2,narg  10868
 
	case(j_iplist) !select case(j_otype(ivb))
	do i=2,narg
		j=j_v(j_o(iob)%i2(io+1+i))
		ii=(j-1)/64 +1  !32+1
		if(ii.gt.j_o(ivb)%i(0).or.j.le.0)then
			call j_printname('*setbits: list', ivb,' ')
			write(6,*)'**can store ',64*j_o(ivb)%i(0), 'bits and you try to set bit',j
			j_err=.true.
			return
		end if !if(ii.gt.j_o(ivb)%i(0).or.j.le.0)  10887
		ibit=j-(ii-1)*64-1   !(ii-1)*32-1  ! bit numbering starts from zero
		rw=j_v(j_o(ivb)%i(ii))
		iw= ibclr(iw,ibit)
		j_v(j_o(ivb)%i(ii))=rw
	end do !i=2,narg  10884
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
		end if !if(j.gt.64.or.j.le.0)  10922
		!   ii=(j-1)/32+1
		ibit=j-1  ! bit numbering starts from zero
		rw=j_v(ivb)
		iw= ibset(iw,ibit)
		j_v(ivb)=rw
	end do !i=2,narg  10920
	case(j_iplist) !select case(j_otype(ivb))
	do i=2,narg
		j=j_v(j_o(iob)%i2(io+1+i))
		if(j.gt.j_o(ivb)%i(1).or.j.le.0)then
			call j_printname('**bits object', ivb,' ')
			write(6,*)' has ',j_o(ivb)%i(1), 'bits and you try to set bit',j
			j_err=.true.
			return
		end if !if(j.gt.j_o(ivb)%i(1).or.j.le.0)  10937
		ii=(j-1)/64+1  !32+1
		if(ii.gt.j_o(ivb)%i(0).or.j.le.0)then
			call j_printname('**setbits: list', ivb,' ')
			write(6,*)'can store ',64*j_o(ivb)%i(1), 'bits and you try to set bit',j
			j_err=.true.
			return
		end if !if(ii.gt.j_o(ivb)%i(0).or.j.le.0)  10944
		ibit=j-(ii-1)*64-1  !32-1  ! bit numbering starts from zero
		rw=j_v(j_o(ivb)%i(ii))
		iw= ibset(iw,ibit)
		j_v(j_o(ivb)%i(ii))=rw
	end do !i=2,narg  10935
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
	end if !if(j.gt.64.or.j.le.0)  10982
	icurbit=j-1
	rw=j_v(ivb)
	if(btest(iw,icurbit))then
		j_v(iout)=1.
	else !if(btest(iw,icurbit))then
		j_v(iout)=0.
	end if !if(btest(iw,icurbit))  10990
	case(j_iplist) !select case(j_otype(ivb))
	ii=(j-1)/64+1
	if(ii.gt.j_o(ivb)%i(0).or.j.le.0)then
		call j_printname('**getbit: list', ivb,' ')
		write(6,*)'#can store ',64*j_o(ivb)%i(1), 'bits and you try to get bit',j
		j_err=.true.
		return
	end if !if(ii.gt.j_o(ivb)%i(0).or.j.le.0)  10997
	icurbit=j-(ii-1)*64-1
	rw=j_v(j_o(ivb)%i(ii))
	if(btest(iw,icurbit))then
		j_v(iout)=j_1
	else !if(btest(iw,icurbit))then
		j_v(iout)=j_0
	end if !if(btest(iw,icurbit))  11005
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
	call j_clearoption(iob,io)
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
	end if !if(narg.eq.2)  11030
	select case(j_otype(ivb))
	case default !select case(j_otype(ivb))
	call j_printname('**getbitch:', ivb,' is not legal object for bits')
	j_err=.true.
	return
	case (j_ipreal) !select case(j_otype(ivb))
	if(narg.le.1)i2=64
	i2=min(64,i2)
	call j_deftext(iout,' ',1,i2-i1+1,ivout_)
	rw=j_v(ivb)
	ie=0
	do i=i1,i2
		ie=ie+1
		if(btest(iw,i-1))then
			j_cline(ie:ie)='1'
		else !if(btest(iw,i-1))then
			j_cline(ie:ie)='0'
		end if !if(btest(iw,i-1))  11049
	end do !i=i1,i2  11047
	case(j_iplist) !select case(j_otype(ivb))
	if(narg.le.1)i2=j_o(ivb)%i(1)*64
	i2=min(64*j_o(ivb)%i(1),i2)
	call j_deftext(iout,' ',1,i2-i1+1,ivout_)
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
		end if !if(btest(iw,icurbit))  11065
	end do !i=i1,i2  11060
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
	end if !if(j_otype(imat).ne.j_ipmatrix)  11112
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
	end if !if(key.le.0.or.key.gt.ncol)  11125
 
	! if(j_o(iob)%i(j_linkoption(iob,io,j_mkey)).gt.2)then
	! write(6,*)'**max number of keys is two'
	! j_err=.true.
	! !	return
	! end if !if(j_o(iob)%i(j_linkoption(iob,io,j_mkey)).gt.2)then
	call j_clearoption(iob,io)  ! subroutine
	if(j_err)return
	key2=0
	if(noptarg.eq.2)then
		key2=j_v(j_optarg0(2) )
		if(key2.le.0.or.key2.gt.ncol)then
			write(6,*)'** 2. key in sort is ',key2, ' should be in range:',1,ncol
			j_err=.true.
			return
		end if !if(key2.le.0.or.key2.gt.ncol)  11141
		write(6,*)'*wrn* using two keys in sort assumes currently first key to be positive integer'
		smax=-huge(1.)
		smin=huge(1.)
		do i=1,nrow_
			s=j_o(imat)%d((i-1)*ncol+key2)
			smax=max(s,smax)
			smin=min(s,smin)
		end do !i=1,nrow_  11149
		range=smax-smin+1.
	end if !if(noptarg.eq.2)  11139
	allocate(xi(1:nrow_),p(1:nrow_))
	do i=1,nrow_
		xi(i)=j_o(imat)%d((i-1)*ncol+key)
		if(key2.ne.0)xi(i)=xi(i)+(j_o(imat)%d((i-1)*ncol+key2)-smin)/range
	end do !i=1,nrow_  11157
	if(key0.lt.0)xi=-xi
	!write(6,*)'xi',xi
	call SSORTP (xi, 1, nrow_, P)  !matsub
	if(key0.lt.0.and.key2.eq.0)xi=-xi
	if(iout.eq.j_ivresult)then
		iout=imat
	else !if(iout.eq.j_ivresult)then
		ivout_=j_defmatrix(iout,' ',nrow_,ncol,j_matreg)
	end if !if(iout.eq.j_ivresult)  11165
	!c     R is not disturbed.  P is set so that R(P(J)) is the J'th element
	!c     of the sorted sequence.
	if(key2.eq.0)then
		do i=1,nrow_
			j_o(iout)%d((i-1)*ncol+key)=xi(p(i))
		end do !i=1,nrow_  11173
	end if !if(key2.eq.0)  11172
	do j=1,ncol
		if(j.eq.key.and.key2.eq.0)cycle
		do i=1,nrow_
			xi(i)=j_o(imat)%d((i-1)*ncol+j)
		end do !i=1,nrow_  11179
		do i=1,nrow_
			j_o(iout)%d((i-1)*ncol+j)=xi(p(i))
		end do !i=1,nrow_  11182
	end do !j=1,ncol  11177
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
	!	call j_clearoption(iob,io)  ! subroutine
 
	if(j_ninc.le.1)then
		write(6,*)'*thisfile not allowed at command level'
		j_err=.true.
	else !if(j_ninc.le.1)then
		call j_getchar(j_inciv(j_ninc),j_filename,le)
		!inquire(j_nul(j_ninc),name=filename)
		ivchar=j_object("'"//j_filename(1:le)//"'") !const
 
		if(ivchar.le.0)then
			write(6,*)'*j* not found:'//"'"//j_filename(1:le)//"'"
			j_err=.true.
		endif !if(ivchar.le.0)  11252
		if(j_err) return
 
		call j_asschar2(ivchar,ivout)
	endif !if(j_ninc.le.1)  11244
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
	endif !if(j_otype(ifile).ne.j_ipchar)  11271
	isize=j_filesize(ifile=ifile,time=itime)
	if(isize.eq.0)then
		write(6,*)'cannot obtain infromation from file'
	else !if(isize.eq.0)then
		write(6,*)'size ',isize,' bytes, last accessed ',itime
	endif !if(isize.eq.0)  11276
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
	call j_startfunction(iob,io,j_ipchar,narg,j_arg,ivout)
	call j_getoption(iob,io,j_mmaxlines,-1,1,j_ipreal,.true.,nargo,j_optarg0)
	if(j_err)return
	if(nargo.gt.0)then
		maxlines=j_v(j_optarg0(1))
	else
		maxlines=j_v(j_ivmaxlines)
	endif !if(nargo.gt.0)  11327
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
			call j_getchar(j_arg(i),j_filename,le_)
			inquire(file = j_filename(1:le_) , exist=j_yes)
			if(j_yes)then
				call j_getfile(nuu,'r',ivfile=j_arg(i))
				if(j_err)return
				isopen=.false.
			else
				write(6,*)'file does not exist'
				j_err=.true.
				return
			endif !if(j_yes)  11345
		endif !if(nuu.ne.0)  11338
		nlin=0
500		read(nuu,'(a)',end=888)j_tempchar2
		nlin=nlin+1
		if(nlin.le.maxlines)then
 
			write(6,*)'  ',j_tempchar2(1:len_trim(j_tempchar2))
			goto 500
 
		endif !if(nlin.le.maxlines)  11358
		if(nlin.ge.maxlines)then
			write(6,*)' '
			write(6,*)'printing limited with Maxlines ',maxlines
			return
		endif !if(nlin.ge.maxlines)  11364
888	 continue
		write(6,*)'______________'
		call j_closeunit(nuu)
	enddo !i=1,narg  11332
	return
 
 
 
 
end subroutine
 
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
	if(any)call j_clearoption(iob,io)  ! subroutine
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
	endif !if(any)  11481
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
	endif !if(j_o(ivmat)%i(2).ne.2)  11545
	call j_getoption(iob,io,j_mnobs,-1,1,j_ipreal,.true.,narg,arg);if(j_err)return
	if(narg.ge.1)then
		n=j_v(arg(1))
	else !if(narg.ge.1)then
		n=j_o(ivmat)%i(1)
	endif !if(narg.ge.1)  11551
	IF (n < 2) then
		write(6,*)'*envelope: there must be at least two points, there was ',n
		j_err=.true.
		RETURN
	endif !IF (n < 2)  11556
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
	END IF !IF (x(1) > x(n))  11563
 
	DO i = 2, n-1
		temp = x(i)
		IF (temp < xmin) THEN
			vertex(1) = i
			xmin = temp
		ELSE IF (temp > xmax) THEN !IF (temp < xmin) THEN
			vertex(2) = i
			xmax = temp
		END IF !IF (temp < xmin)  11577
	END DO !i = 2, n-1  11575
 
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
		END IF !IF (y(1) > y(n))  11587
 
		DO i = 2, n-1
			temp = y(i)
			IF (temp < ymin) THEN
				vertex(1) = i
				ymin = temp
			ELSE IF (temp > ymax) THEN !IF (temp < ymin) THEN
				vertex(2) = i
				ymax = temp
			END IF !IF (temp < ymin)  11601
		END DO !i = 2, n-1  11599
 
		nvert = 2
		IF (ymax == ymin) nvert = 1
		write(6,*)'*envelope: ymax=ymin=',ymax
		j_err=.true.
		RETURN
	END IF !IF (xmax == xmin)  11586
 
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
			END IF !IF (dist > dmax)  11640
		ELSE IF (dist < zero) THEN !IF (dist > zero) THEN
			iwk(i2) = i
			i2 = i
			IF (dist < dmin) THEN
				next(2) = i
				dmin = dist
			END IF !IF (dist < dmin)  11647
		END IF !IF (dist > zero)  11637
	END DO !i = 1, n  11634
 
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
	END IF !40 IF (next(j) < 0)  11667
 
	jp1 = j + 1
	DO i = nvert, jp1, -1
		vertex(i+1) = vertex(i)
		next(i+1) = next(i)
	END DO !i = nvert, jp1, -1  11674
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
			END IF !IF (dist > DMAX1)  11714
		ELSE !IF (dist > zero) THEN
			dist = (y(i) - y2)*dx2 - (x(i) - x2)*dy2
			IF (dist > zero) THEN
				iwk(i2) = i
				i2 = i
				IF (dist > dmax2) THEN
					next(jp1) = i
					dmax2 = dist
				END IF !IF (dist > dmax2)  11723
			END IF !IF (dist > zero)  11720
		END IF !IF (dist > zero)  11711
		i = iwk(i)
	ELSE !IF (i /= i2save) THEN
		i = i2next
	END IF !60 IF (i /= i2save)  11709
 
	!  Get next point from old list at vertex j.
 
	IF (i > 0) GO TO 60
 
	!  End lists with -ve values.
 
	iwk(i1) = -1
	iwk(i2) = -1
 
	GO TO 40
99 iout=j_defmatrix(ivout,' ',nvert+1,2,j_matreg)
	do i=1,nvert
		j_o(ivout)%d((i-1)*2+1)=x(vertex(i))
		j_o(ivout)%d(i*2)=y(vertex(i))
	enddo !i=1,nvert  11745
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
	call j_clearoption(iob,io )
	allocate(value9(1:j_dnobs))
	!		write(6,*)'<33allocat',j_dnobs,size(value9),'j_dimat ',j_dimat
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
	endif !if(j_err)  11798
	if(j_rejected)then
		ial=ial+1
		if(ial.gt.j_duntil)then
			write(6,*)'*no observations accepted from Obs-range ',j_dfrom,j_duntil
			j_err=.true.
			return
		endif !if(ial.gt.j_duntil)  11804
		goto 100
	endif !if(j_rejected)  11802
	value9(1)=j_v(iv);nval=1;ial=ial+1
	nacc=1
	!	write(6,*)'<5757ial,j_duntil ',ial,j_duntil
	do i=ial,j_duntil
		call j_getobs(i)
		if(j_err)return  !j_err
 
		if(j_rejected)cycle
		if(abs(j_v(iv)).ge.1.7d19)then
			missining=missing+1
			cycle
		endif !if(abs(j_v(iv)).ge.1.7d19)  11819
		nacc=nacc+1
		if(.not.any(value9(1:nval).eq.j_v(iv)))then
			nval=nval+1;value9(nval)=j_v(iv)
		end if !if(.not.any(value9(1:nval).eq.j_v(iv)))  11824
	end do !i=ial,j_duntil  11814
	!	end do !do k=1,jndatasetss
	j_v(j_ivaccepted)=nacc
	write(6,*)'Accepted ',nacc,'  from ',j_dnobs
	allocate(iperm(1:nval))
	call j_quick_sort(value9(1:nval),iperm)
	ivout_=j_defmatrix8(iout,' ',nval,j_18,j_matreg)
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
		endif !if(j_v(arg(1)).eq.j_0)  11902
		if(j_otype(arg(2)).eq.j_ipreal)then
 
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=j_v(arg(2))/j_v(arg(1))
			return
		elseif(j_otype(arg(2)).eq.j_ipmatrix)then
			iout=j_defmatrix8(iout,' ',j_nrows(arg(2)),j_nrows(arg(2)),j_matreg)
			j_o(iout)%d=j_o(arg(2))%d/j_v(arg(1))
			return
		end if !if(j_otype(arg(2)).eq.j_ipreal)  11906
	endif !if(j_otype(arg(1)).eq.j_ipreal)  11901
	itarg1=j_otype(arg(1))
	itarg2=j_otype(arg(2))
	if(itarg1.ne.j_ipmatrix.or.itarg2.ne.j_ipmatrix)then
		write(6,*)'arguments have types ',j_objecttypes(itarg1),j_objecttypes(itagr2),' should be MATRIX'
		j_err=.true.;return
	endif !if(itarg1.ne.j_ipmatrix.or.itarg2.ne.j_ipmatrix)  11919
	ndim=j_nrows(arg(1))  !j_o(arg(1))%i8(1)
	ndim2=j_ncols(arg(2))  ! j_o(arg(2))%i8(2)
	!		if(ndim.ne.j_o(arg(1))%i8(2).or.j_o(arg(2))%i8(1).ne.ndim)then
	if(ndim.ne.j_ncols(arg(1)).or.j_nrows(arg(2)).ne.ndim)then   !j_o(arg(2))%i8(1).ne.ndim)then
		write(6,*)'illegal dimensions ',j_nrows(arg(1)),j_ncols(arg(1)),j_nrows(arg(2)),j_ncols(arg(2))
		j_err=.true.
		return
	endif !if(ndim.ne.j_ncols(arg(1)).or.j_nrows(arg(2)).ne.ndim)  11926
	allocate (mat(ndim,ndim),rhs_(1:ndim,1:ndim2))
	allocate (ipiv(ndim))
	iel=0
	iel2=0
	do i=1,ndim
		do j=1,ndim
			iel=iel+1
			mat(i,j)=j_o(arg(1))%d(iel)
		enddo !j=1,ndim  11936
 
		do j=1,ndim2
			iel2=iel2+1
			rhs_(i,j)=j_o(arg(2))%d(iel2)
		enddo !j=1,ndim2  11941
	enddo !i=1,ndim  11935
	!  SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
	call dgesv(ndim,ndim2,mat,ndim,ipiv,rhs_,ndim,info_)
	if(info_.ne.0)then
		write(6,*)'info ',info_
		call j_printname('matrix ',arg(1), ' is singular')
		j_err=.true.
 
	else !if(info_.ne.0)then
		ivout=j_defmatrix(iout,' ',ndim,ndim2,j_matreg)
		iel=0
		do i=1,ndim
			do j=1,ndim2
				iel=iel+1
				j_o(iout)%d(iel)=rhs_(i,j)
			enddo !j=1,ndim2  11957
		enddo !i=1,ndim  11956
	endif !if(info_.ne.0)  11948
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
	! @@data
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
 
	call j_startfunction(iob,io,0,narg,arg,iout) !the argumenst are used here as real
	singutot=.false. ! are there altogether droppings
	getpar=j_linkoption(iob,io,j_mpar).ge.0
	if(j_err)return
	iy=arg(1) !   o(iob)%i(io+2)
	intcep=1
	linkvariance=j_codelink(iob,io,j_mvariance)
	isvariance=linkvariance.gt.0
 
 
	!		if(isvariance)write(6,*)'hep ',j_o(iob)%i(linkvariance:linkvariance+4)
	call j_getoption_index(iob,io,j_mnoint,-1,0,0,&
		.false.,nnoint,j_optarg0);if(j_err)return
	if(nnoint.ge.0)intcep=0
	!j_getoption_name(iob,option,minarg,maxarg,iptype,expand,min0,noptarg,optarg)
 
	!	write(6,*)'iprint',iprint
	call j_getdataobject(iob,io);if(j_err)return
 
	! call j_getoption_index(iob,io,j_mprint,-1,1,j_ipreal,&
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
 
	endif !if(intcep.eq.1)  12065
	allocate(regl(1:ncoef))
	if(intcep.eq.1)then
		regl(1)=j_ivone
		regl(2:ncoef)=arg(2:narg)
	else !if(intcep.eq.1)then
		regl=arg(2:narg)
 
	endif !if(intcep.eq.1)  12074
 
	!	regl=>arg(2:narg) !    o(iob)%i(io+3:io+1+narg)
	!	ncoef=nind+intcep
	!x(iob,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
	call j_getoption_index(iob,io,j_mstep,-1,1,j_ipreal,&
		.true.,nstep,j_optarg0)
	if(j_err)return
	step=.false.
	if(nstep.ge.1)then
		step=.true.
		tmin=j_v(j_optarg0(1))
	endif !if(nstep.ge.1)  12089
 
	call j_getoption_index(iob,io,j_mvar,-1,0,0,&
		.false.,nvar,j_optarg0);if(j_err)return
	var=nvar.ge.0
	call j_getoption_index(iob,io,j_mcorr,-1,0,0,&
		.false.,nvar,j_optarg0);if(j_err)return
	corr=nvar.ge.0
	!call j_getdatasets(iob)
	!	write(6,*)'j_noptb',j_nopt,ncoef,ncoef1
	call j_clearoption(iob,io)  ! subroutine
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
		enddo !k=1,ncoef  12114
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
				if(abs(j_v(iy)).ge.1.7d19)then
					missing=missing+1
					cycle
				endif !if(abs(j_v(iy)).ge.1.7d19)  12132
 
				do j=intcep2,ncoef
					if(abs(j_v(regl(j))).ge.1.7d19)then
						missing=missing+1
						cycle obloopw
					endif !if(abs(j_v(regl(j))).ge.1.7d19)  12138
				enddo !j=intcep2,ncoef  12137
 
 
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
 
				!	if(i.le.3)write(6,*)'<66i',i,j_o(iob)%i(linkvariance:linkvariance+5),weight
				if(vari.le.j_0)then
					write(6,*)'for observation ',i,' variance was not positive'
					j_err=.true. ;return
				endif !if(vari.le.j_0)  12158
				weight=1.d0/sqrt(vari)
				do j=1,ncoef
					rhs_(j,ncoef1)=rhs_(j,ncoef1)+j_v(regl(j))*j_v(iy)/vari
 
					do j2=j,ncoef
 
						mat(j,j2)=mat(j,j2)+j_v(regl(j))*j_v(regl(j2))/vari
					enddo !j2=j,ncoef  12166
				enddo !j=1,ncoef  12163
				sumw=sumw+weight
				sumy=sumy+weight*j_v(iy)
				sumynw=sumynw+j_v(iy)
			enddo obloopw !oopw:do i=j_dfrom,j_duntil  12126
 
 
		else
				obloop1:do i=j_dfrom,j_duntil
					!call j_nextobs()
				call j_getobs(i)
				if(j_err)return  !j_err
 
				if(j_rejected)cycle
				if(abs(j_v(iy)).ge.1.7d19)then
					missing=missing+1
					cycle
				endif !if(abs(j_v(iy)).ge.1.7d19)  12184
 
				do j=intcep2,ncoef
					if(abs(j_v(regl(j))).ge.1.7d19)then
						missing=missing+1
						cycle obloop1
					endif !if(abs(j_v(regl(j))).ge.1.7d19)  12190
				enddo !j=intcep2,ncoef  12189
 
 
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
					enddo !j2=j,ncoef  12210
				enddo !j=1,ncoef  12207
 
				sumy=sumy+j_v(iy)
			enddo obloop1 !oop1:do i=j_dfrom,j_duntil  12178
		endif !if(isvariance)  12123
		if(ntot.lt.ncoef)then
			write(6,*)'*only ',ntot,' observations accepted, estimation not possible'
			j_err=.true.
			return
		endif !if(ntot.lt.ncoef)  12219
 
		!	end do !do k=1,jndatasetss
		singucur=.false.
		do j=1,ncoef
			if(mat(j,j).lt.1.d-18)then
				call j_printname('always zero: ',regl(j),',dropping it')
				iimin=j
				singucur=.true.
				singutot=.true. ! dropping bussiness working like in step
				goto 445
			endif !if(mat(j,j).lt.1.d-18)  12228
			do j2=1,j-1
				mat(j,j2)=mat(j2,j)
				if(abs(mat(j,j2)/sqrt(mat(j,j)*mat(j2,j2))).gt.0.99999d0)then
					if(j2.eq.1.and.intcep2.gt.1)then
						call j_printname('no variation in ',regl(j),',dropping it')
					else !if(j2.eq.1.and.intcep2.gt.1)then
						call j_printname('variable ',regl(j),' is dropped,&
							because it is linearly related to ',regl(j2))
					endif !if(j2.eq.1.and.intcep2.gt.1)  12238
					iimin=j
					singutot=.true.
					singucur=.true.
					goto 445
				endif !if(abs(mat(j,j2)/sqrt(mat(j,j)*mat(j2,j2))).gt.0.99999d0)  12237
			enddo !j2=1,j-1  12235
		enddo !j=1,ncoef  12227
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
		endif !if(isvariance)  12256
		ntot=0
		!	do k=1,jndatasetss
		!call j_getdataset(j_datasets(k),nobs)
		ibas=-j_dnkeep    !dtas
			obloop2:		do i=j_dfrom,j_duntil
			ibas=ibas+j_dnkeep
			call j_getobs(i)
			if(j_err)return  !j_err
 
 
 
			if(j_rejected.or.abs(j_v(iy)).ge.1.7d19)then
				if(iregf.gt.0)j_o(j_dimat)%d(ibas+iregf)=j_0
				if(iresid.gt.0)j_o(j_dimat)%d(ibas+iresid)=j_0
				cycle
			endif !if(j_rejected.or.abs(j_v(iy)).ge.1.7d19)  12277
 
			do j=intcep2,ncoef
				if(abs(j_v(regl(j))).ge.1.7d19)cycle obloop2
			enddo !j=intcep2,ncoef  12283
 
 
 
			ntot=ntot+1
			sum=0.d0
			!			if(intcep.gt.0)sum=rhs_(1,ncoef1)
 
			do j=1,ncoef
 
				sum=sum+rhs_(j,ncoef1)*j_v(regl(j))
			enddo !j=1,ncoef  12293
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
			endif !if(isvariance)  12299
			if(iregf.gt.0)j_o(j_dimat)%d(ibas+iregf)=sum
			if(iresid.gt.0)j_o(j_dimat)%d(ibas+iresid)=resid
 
 
		enddo obloop2 !oop2:		do i=j_dfrom,j_duntil  12270
		!		end do !do k=1,jndatasetss
		dfe=ntot-ncoef
		idfe=ntot-ncoef
		mse_=sse/dfe
		rmse=sqrt(mse_)
 
		do j=1,ncoef
			t(j)=real(rhs_(j, ncoef1))/(sqrt(rhs_(j,j))*rmse)
		enddo !j=1,ncoef  12323
		if(step)then
 
			tmin0=10000.
			do ii=intcep2,ncoef
				if(abs(t(ii)).lt.tmin0)then
					iimin=ii !REGL
					tmin0=abs(t(ii))
				endif !if(abs(t(ii)).lt.tmin0)  12330
			enddo !ii=intcep2,ncoef  12329
 
			if(j_dprint.gt.1.and.tmin0.lt.tmin)call j_printname('dropping ',regl(iimin),' ')
 
			if(j_dprint.lt.2.and.tmin0.lt.tmin)goto 445
		endif !if(step)  12326
 
 
		write(6,*)' '
		if(iout.ne.j_ivresult)call j_printname('output regression object ',iout,' ')
		write(6,*)'dependent variable ',j_object_name(iy,15)
		write(6,*)'Accepted ',ntot,' from ',j_dnobs
		if(missing.gt.0)write(6,*)'dependent was missing ',missing, ' times'
		j_v(j_ivaccepted)=ntot
		write(6,*)' '
		write(6,*)'     var           coef         se           t'
		if(intcep.gt.0)write(6,718)'Intercept       ',real(rhs_(1,ncoef1)),&
			sqrt(rhs_(1,1))*rmse,t(1)
	718 format(1x,a15,2g16.8e1,f11.2)
		do j=intcep2,ncoef
			!	t(j)=real(rhs_(j,ncoef1))/(sqrt(rhs_(j,j))*rmse)
			write(6,718)j_object_name(regl(j),15),real(rhs_(j,ncoef1)),&
				sqrt(rhs_(j,j))*rmse,t(j)
 
		end do !j=intcep2,ncoef  12353
		r2=1.d0-mse_*(ntot-1.)/sst
		write(6,*)' '
		write(6,'(a,g15.7,a,f8.5,a,i8)')'RMSE= ',rmse,' R2=',r2, ' df ',idfe
		if(isvariance)then
			write(6,*)'*Note the se of the residual error is RMSE*sqrt(variance), where variance is defined in variance->'
			!	mse_=sse/dfe
			r20=1.d0-ssenw*(ntot-1.)/(sstnw*dfe)
			write(6,*)'rmse of unweighted residuals=',sqrt(ssenw/dfe),' R2 in the original scale =',r20
		endif !if(isvariance)  12362
		if(intcep.gt.0)then
			write(6,*)'F reg (',ncoef-1,idfe,')=',(sst-sse)/(mse_*(ncoef-1.))
		end if !if(intcep.gt.0)  12368
445	if(step.and.tmin0.lt.tmin.or.singucur)then
			if(iimin.lt.ncoef)then
				do ii=iimin,ncoef-1
					regl(ii)=regl(ii+1)
				enddo !ii=iimin,ncoef-1  12373
			endif !if(iimin.lt.ncoef)  12372
 
 
			ncoef=ncoef-1
			ncoef1=ncoef1-1
			cycle steploop
		endif !445	if(step.and.tmin0.lt.tmin.or.singucur)  12371
		exit steploop
 
	enddo steploop !ploop:	do iste=1,ncoef  12110
 
	! needs list which transfers original regressors to ne one
	if(iout.ne.j_ivresult)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		!		nind2=nind+intcep
		!	write(6,*)'ncoef ',ncoef
		allocate(j_o(iout)%i(0:ncoef+4),j_o(iout)%d(1:2*ncoef+4)) !coef_,se,rmse,mse,r2,f !const last
		j_o(iout)%d=0
		ivvar=0
		if(var)then
			ivvar=j_defmatrix(iout,'%var',ncoef,ncoef,j_matreg)
			it=0
			do i=1,ncoef
				do j=1,ncoef
					it=it+1
 
					j_o(ivvar)%d(it)=rhs_(i,j)*mse_  !laske
 
 
				enddo !j=1,ncoef  12399
			enddo !i=1,ncoef  12398
		endif !if(var)  12395
		ivcor=0
		if(corr)then
			ivcor=j_defmatrix(iout,'%corr',ncoef,ncoef,j_matreg)
			it=0
			do i=1,ncoef
				do j=1,ncoef
					it=it+1
					!		if(ortonew(i).ne.0.and.ortonew(j).ne.0)then
					if(i.eq.j)then
						j_o(ivcor)%d(it)=1.d0 !=rmse*sqrt(rhs_(i,j))  !laske
					else !if(i.eq.j)then
						j_o(ivcor)%d(it)=rhs_(i,j)/sqrt(rhs_(i,i)*rhs_(j,j))
					endif !if(i.eq.j)  12416
					!		endif
				enddo !j=1,ncoef  12413
			enddo !i=1,ncoef  12412
		endif !if(corr)  12409
 
		j_o(iout)%i(0)=ncoef
		j_o(iout)%i(1:ncoef)=regl(1:ncoef)
		j_o(iout)%d(1:ncoef)=rhs_(1:ncoef,ncoef1)
		do j=1,ncoef
			j_o(iout)%d(ncoef+j)=sqrt(rhs_(j,j))*rmse
		enddo !j=1,ncoef  12429
 
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
			call j_deflistobject(iout,'%xvars',iv,listold=j_o(iout)%i(0:ncoef))
			!call j_deflist2(iout,'%xvars',j_o(iout)%i(0:ncoef),iv)
			iv=j_defmatrix(iout,'%coef',ncoef,1,j_matreg)
			j_o(iv)%d=j_o(iout)%d(1:ncoef)
			iv=j_defmatrix(iout,'%se',ncoef,1,j_matreg)
			j_o(iv)%d=j_o(iout)%d(ncoef+1:2*ncoef)
 
		endif !if(getpar)  12446
 
	endif !if(iout.ne.j_ivresult)  12388
 
	!		allocate(mat(1:ncoef,1:ncoef),rhs_(1:ncoef,ncoef1),ipiv(1:ncoef))
	!	allocate(t(1:ncoef))
	deallocate(mat,rhs_,ipiv,t,regl)
	write(6,*)' '
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
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
 
subroutine coefse(iob,io,ity)  !ity=0 coef, ity=1 se
 
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i(io+2)
	inde=j_o(iob)%i(io+3)  !second argument
	iout=j_o(iob)%i(io+2+narg)
	select case(j_otype(irg))
	case(j_ipregr) !select case(j_otype(irg))
	do i=1,j_o(irg)%i(0)
		if(j_o(irg)%i(i).eq.inde)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=j_o(irg)%d(i+ity*j_o(irg)%i(0))
			return
 
		endif !if(j_o(irg)%i(i).eq.inde)  12497
	end do !i=1,j_o(irg)%i(0)  12496
	call j_printname('**coef: variable ',inde,' was not a variable in ',irg)
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
	endif !if(.not.j_distrans)  12554
	iregf=j_inlistobject(j_ivregf,j_divkeep)
	iresid=j_inlistobject(j_ivresid,j_divkeep)
 
	linkvariance=j_codelink(iob,io,j_mvariance)
	isvariance=linkvariance.gt.0
 
	linkloglike=j_codelink(iob,io,j_mloglike)
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
		endif !if(ncoef.ne.ncoef1)  12593
 
		j_v(par(1:ncoef1))=j_v(initial(1:ncoef1))
	endif !if(ncoef1.gt.0)  12592
	write(6,*)'intial values:'
	if(j_printvar(6,ncoef,par(1:ncoef)))continue
 
 
	nnz=0
	do ii=1,ncoef
		if(j_v(par(ii)).ne.j_0)nnz=nnz+1
	enddo !ii=1,ncoef  12606
	if(nnz.eq.0)then
		write(6,*)'some initial values should be nonzero'
		j_err=.true.
		return
	endif !if(nnz.eq.0)  12609
 
 
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
		endif !if(narg.ne.ncoef)  12626
	endif !if(narg.gt.0.)  12625
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
		endif !if(narg.ne.ncoef)  12640
	endif !if(narg.gt.0)  12639
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
		endif !if(narg.ne.ncoef)  12654
	endif !if(narg.gt.0)  12653
	step=1
	call j_getoption(iob,io,j_mstep,-1,1,j_ipreal,.true.,narg,arg);if(j_err)return
	if(narg.gt.0)step=j_v(arg(1))
	ncoef1=ncoef+1
	! litr=j_linkoption(iob,io,j_mtrans)
 
	! ivtrans=j_o(iob)%i(litr+1)  !needed to be stored in the output object
	! endif !if(litr.eq.0)then
	!call j_getdatasets(iob)
	call j_clearoption(iob,io)  ! subroutine
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
		endif !if(allocated(variance))  12691
		if(.not.allocated(variance))allocate(variance(1:j_duntil))
	endif !if(isvariance)  12690
	100 continue
	if(iter.ge.maxiter)then
		if(j_dprint.gt.1)write(6,*)'maximum number of iterations ',iter
		goto 200
	endif !if(iter.ge.maxiter)  12697
	loop=loop+1
	mat=j_0
	rhs_=j_0
	do k=1,ncoef
		rhs_(k,k)=1.d0
	enddo !k=1,ncoef  12704
 
	ntot=0
	sse0=j_0
	sse0nw=j_0
	missing=0
	loglike=j_0
	!	do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
	ibas=-j_dnkeep
	do i=j_dfrom,j_duntil
		ibas=ibas+j_dnkeep
		call j_getobs(i)
		if(j_err)return  !j_err
 
		if(j_rejected)cycle
 
		if(abs(j_v(iy)).ge.1.7d19)then
			missing=missing+1
			cycle
		endif !if(abs(j_v(iy)).ge.1.7d19)  12723
		ntot=ntot+1
 
		resid=j_v(iy)-j_v(ivfunc)
 
		if(isvariance)then
			variance(i)=j_codevalue(iob,linkvariance)
 
			!	if(i.le.5)write(6,*)'<77',i,variance,j_v(ivfunc),j_v(iy),j_v(der(1:ncoef))
			if(variance(i).le.j_0)then
				write(6,*)'for observation ',i,' variance',variance(i),' was not positive'
				j_err=.true. ;return
			endif !if(variance(i).le.j_0)  12735
			weight=j_1/sqrt(variance(i))
			sumy=sumy+weight*j_v(iy)
			sumf=sumf+weight*j_v(ivfunc)
			sumfnw=sumfnw+j_v(ivfunc)
			sumy2=sumy2+j_v(iy)*j_v(iy)/variance(i)
 
			if(eka)then
				sumynw=sumynw+j_v(iy)
				sumy2nw=sumy2nw+j_v(iy)*j_v(iy)
			endif !if(eka)  12745
			do j=1,ncoef
				rhs_(j,ncoef1)=rhs_(j,ncoef1)+j_v(der(j))*resid/variance(i)
 
				do j2=j,ncoef
 
					mat(j,j2)=mat(j,j2)+j_v(der(j))*j_v(der(j2))/variance(i)
				enddo !j2=j,ncoef  12752
			enddo !j=1,ncoef  12749
			sse0nw=sse0+resid*resid
			sse0=sse0+resid*resid/variance(i)
		else
			if(eka)then
				sumy=sumy+j_v(iy)
				sumy2=sumy2+j_v(iy)*j_v(iy)
			endif !if(eka)  12760
			do j=1,ncoef
				rhs_(j,ncoef1)=rhs_(j,ncoef1)+j_v(der(j))*resid !(j_v(iy)-j_v(ivfunc))
 
				do j2=j,ncoef
 
					mat(j,j2)=mat(j,j2)+j_v(der(j))*j_v(der(j2))
				enddo !j2=j,ncoef  12767
			enddo !j=1,ncoef  12764
			sse0=sse0+resid*resid
			sumf=sumf+j_v(ivfunc)
			!	end do !do k=1,jndatasetss
		endif !if(isvariance)  12731
		!	resid=j_v(iy)-j_v(ivfunc)
		!	sse0nw=sse0+resid*resid
		if(isloglike)loglike=loglike+j_codevalue(iob,linkloglike)
		if(iregf.gt.0)j_o(j_dimat)%d(ibas+iregf)=j_v(ivfunc)
		if(iresid.gt.0)j_o(j_dimat)%d(ibas+iresid)=resid
	end do !i=j_dfrom,j_duntil  12716
	do j=1,ncoef
		do j2=1,j-1
			mat(j,j2)=mat(j2,j)
		enddo !j2=1,j-1  12783
	enddo !j=1,ncoef  12782
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
			endif !if(isvariance)  12798
			if(isloglike)write(6,*)'log likelihood ',loglike
		endif !if(j_dprint.gt.1)  12794
		eka=.false.
	endif !if(eka)  12793
	scale=step/0.6     ! j changed  6.10.2020   was 1/0.6
	iter2=0
	10 scale=scale*0.6d0
	iter=iter+1
	iter2=iter2+1
	if(iter.gt.maxiter)then
		j_v(par(1:ncoef))=current
		if(j_dprint.gt.1)write(6,*)'maximum number of iterations ',iter
		sse=sse0
		j_v(par(1:ncoef))=current
		goto 200
	endif !if(iter.gt.maxiter)  12814
	if(iter2.gt.8)then
		j_v(par(1:ncoef))=current
		if(j_dprint.gt.1)write(6,*)'maximum number of inner looop iterations ',iter2
		sse=sse0
		!		j_v(par(1:ncoef))=current
		goto 200
	endif !if(iter2.gt.8)  12821
	scalemin1=scale
	if(isparmin)then
		do i=1,ncoef
			if(j_v(par(i)).lt.parmin(i))then
				! current+scale*rhs=min ->
				scalemin1=	min((parmin(i) -current(i))/rhs_(i,ncoef1),scalemin1)
				if(j_dprint.gt.1) write(6,*)'parameter ',i,' constrained by minimum ',	parmin(i),' newscale=',scalemin1
			endif !if(j_v(par(i)).lt.parmin(i))  12831
		enddo !i=1,ncoef  12830
 
	endif !if(isparmin)  12829
	scalemin2=scale
	if(isparmax)then
		do i=1,ncoef
			if(j_v(par(i)).gt.parmax(i))then
				! current+scale*rhs=max ->
				scalemin2=	min((parmax(i) -current(i))/rhs_(i,ncoef1),scalemin1,scalemin2)
				if(j_dprint.gt.1) write(6,*)'parameter ',i,' constrained by minimum ',	parmax(i),'newscale=',scalemin2
			endif !if(j_v(par(i)).gt.parmax(i))  12842
		enddo !i=1,ncoef  12841
 
	endif !if(isparmax)  12840
	!	cscale=scale
	if(scalemin1.lt.scale.or.scalemin2.lt.scale)then
		scale=min(scalemin1,scalemin2)
		if(j_dprint.gt.1)write(6,*)'new scale =',scale
	endif !if(scalemin1.lt.scale.or.scalemin2.lt.scale)  12851
 
	do i=1,ncoef
		if(scale*abs(rhs_(i,ncoef1)).gt.tole*sqrt(rhs_(i,i))/100.)goto 19
 
	enddo !i=1,ncoef  12856
	if(j_dprint.gt.1) write(6,*)'potential changes in parameters are smaller than tolerance'
	sse=sse0
	j_v(par(1:ncoef))=current
	goto 200
	19 continue
	if(isdpar)then
		do i=1,ncoef
			if(abs(rhs_(i,ncoef1)).gt.dpar(i))scale=dpar(i)/abs(rhs_(i,ncoef1))
		enddo !i=1,ncoef  12866
 
	endif !if(isdpar)  12865
 
 
	j_v(par(1:ncoef))=current+scale*rhs_(1:ncoef,ncoef1)
	sse=j_0
	ntot=0
	ssenw=j_0
	sumf=j_0
	if(isvariance)then
		sumy=j_0
		sumy2=j_0
	endif !if(isvariance)  12878
	sumfnw=j_0
	!	do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
 
	do i=j_dfrom,j_duntil
 
		call j_getobs(i)
 
		if(j_err)return
		if(j_rejected)cycle
		if(abs(j_v(iy)).ge.1.7d19)cycle
		ntot=ntot+1
		resid=j_v(iy)-j_v(ivfunc)
		if(isvariance)then
			!if(i.le.15)write(6,*)'<88',i,variance,j_v(ivfunc),j_v(iy)
			!	variance=j_codevalue(iob,linkvariance)
			weight=j_1/sqrt(variance(i))
			! if(variance.le.0.d0)then
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
		endif !if(isvariance)  12895
	end do !i=j_dfrom,j_duntil  12886
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
		endif !if(isvariance)  12922
		if(isloglike)write(6,*)'loglikelihood ',loglike
		! if(isvariance)then
		! write(6,*)'unweighted: rmse',sqrt(sse0nw/dfe),' mean of y ',sumynw/ntot, 'mean of func ',sumfnw/ntot, &
		! ' Weighted: rmse0 ',rmse,' mean of w-y ',sumy/ntot,' mean of func ',sumf/ntot
		! else
		! write(6,*)'rmse',rmse,' mean of y ',sumy/ntot,' mean of func ',sumf/ntot
		! endif
		write(6,*)' step ',scale
		if(isvariance)write(6,*)' unweighted rmse ',	sqrt(ssenw/(ntot-ncoef))
	endif !if(j_dprint.gt.1)  12919
	if(slow.lt.0.)then
		perc=100.*(sse0-sse)/sse0
		if(perc.gt.abs(slow))goto 100
		if(j_dprint.gt.1)write(6,*)'improvement % in sse ',perc, ' < ',abs(slow)
	else !if(slow.lt.0.)then
		if(rmse0-rmse.gt.slow)goto 100
		if(j_dprint.gt.1)write(6,*)'improvement of rmse ',rmse0-rmse,' <  ',slow
	endif !if(slow.lt.0.)  12939
 
	200 dfe=ntot-ncoef
	idfe=dfe
	mse_=sse/dfe
	!		write(6,*)'ssehere ',sse,ssew
	rmse=sqrt(mse_)
	if(j_dprint.gt.0)then
		write(6,*)' '
		write(6,*)'dependent variable ',j_object_name(iy,15)
		write(6,*)'Accepted ',ntot
		j_v(j_ivaccepted)=ntot
		write(6,*)'     param          value         se           t'
 
		718 format(1x,a15,2g13.5e1,f8.2)
		do j=1,ncoef
			write(6,718)j_object_name(par(j),15),j_v(par(j)),&
				sqrt(rhs_(j,j))*rmse,j_v(par(j))/(sqrt(rhs_(j,j))*rmse)
 
		end do !j=1,ncoef  12961
	endif !if(j_dprint.gt.0)  12953
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
		endif !if(isvariance)  12972
	endif !if(j_dprint.gt.0)  12968
	if(isloglike)then
		write(6,*)'loglikehilihood ',loglike
		write(6,*)' '
	endif !if(isloglike)  12980
	if(iout.ne.j_ivresult)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
		allocate(j_o(iout)%i(0:ncoef1+3),j_o(iout)%d(1:2*ncoef+4)) !coef_,se,rmse,mse,r2,f const last
		j_o(iout)%i(0)=ncoef
		j_o(iout)%d(1:ncoef)=j_v(par(1:ncoef))
		j_o(iout)%i(1:ncoef)=par(1:ncoef)
		do i=1,ncoef
 
			j_o(iout)%d(i+ncoef)=sqrt(rhs_(i,i))*rmse
 
		end do !i=1,ncoef  12991
		ivvar=0
		if(var)then
			ivvar=j_defmatrix(iout,'%var',ncoef,ncoef,j_matreg)
			it=0
			do i=1,ncoef
				do j=1,ncoef
					it=it+1
					j_o(ivvar)%d(it)=rhs_(i,j)*mse_  !laske
				enddo !j=1,ncoef  13001
			enddo !i=1,ncoef  13000
		endif !if(var)  12997
		ivcor=0
		if(corr)then
			ivcor=j_defmatrix(iout,'%corr',ncoef,ncoef,j_matreg)
			it=0
			do i=1,ncoef
				do j=1,ncoef
					it=it+1
					if(i.eq.j)then
						j_o(ivcor)%d(it)=rmse*sqrt(rhs_(i,j))  !laske
					else !if(i.eq.j)then
						j_o(ivcor)%d(it)=rhs_(i,j)/sqrt(rhs_(i,i)*rhs_(j,j))
					endif !if(i.eq.j)  13014
				enddo !j=1,ncoef  13012
			enddo !i=1,ncoef  13011
		endif !if(corr)  13008
		j_o(iout)%i(ncoef1)=ntot
		j_o(iout)%i(ncoef1+1)=ivvar
		j_o(iout)%i(ncoef1+2)=ivcor
		j_o(iout)%i(ncoef1+3)=ivtrans
		j_o(iout)%d(2*ncoef+1)=SQRT(SSE/dfe)
		j_o(iout)%d(2*ncoef+2)=SSE/dfe
		j_o(iout)%d(2*ncoef+3)=r2
		j_o(iout)%d(2*ncoef+4)=9.9  ! p-value
		j_otype(iout)=j_ipregr
 
	endif !if(iout.ne.j_ivresult)  12984
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
	ifunc=j_o(iob)%i(io+2)
	if(j_otype(ifunc).eq.j_iplist)then
		ivindex=j_igetopt(iob,io,j_mindex)
		if(ivindex.le.0)then
			write(6,*)'** value, first argument is list => there must be index->'
			j_err=.true.
			!	call j_clearoption(iob,io)  ! subroutine
			return
		endif !if(ivindex.le.0)  13059
		inde=j_v(ivindex)
		if(inde.le.0.or.inde.gt.j_o(ifunc)%i(1))then
			write(6,*)'**value, index-> has illegal value ',inde, 'max is ',j_o(ifunc)%i(1)
			j_err=.true.
			return
		endif !if(inde.le.0.or.inde.gt.j_o(ifunc)%i(1))  13066
		ifunc=j_o(ifunc)%i2(inde)
	endif !if(j_otype(ifunc).eq.j_iplist)  13057
	iarg=j_o(iob)%i(io+3)
	select case(j_otype(ifunc) )
	case (j_ipsmooth) !select case(j_otype(ifunc) )
	! smooth
	!need wrk only for 2*m
	j_v(iout)=j_valuesspl(ifunc,real(j_v(iarg)))
	call j_clearoption(iob,io)  ! subroutine
	case(j_ipmatrix) !select case(j_otype(ifunc) )
	nval=j_o(ifunc)%i(2)
	arg=j_v(iarg)
	select case(j_o(ifunc)%i(1) )
	case(1) !select case(j_o(ifunc)%i(1) )
	goto 900
	case(2) !select case(j_o(ifunc)%i(1) )
	irow=2
	case default !select case(j_o(ifunc)%i(1) )
	irow=j_igetopt(iob,io,j_mrow)
	isz=j_linkoption(iob,io,j_mz).ge.0
	call j_clearoption(iob,io)  ! subroutine
	! type
	if(isz)then  !bilinear interpolation
		nrows=j_o(ifunc)%i(1)
		ncols=j_o(ifunc)%i(2)
		if(nrows.lt.3.or.ncols.lt.3.or.narg.lt.3)then
			write(6,*)'*value: illegal number of arguments or matrix for bilinear interpolation'
			j_err=.true.
			return
		endif !if(nrows.lt.3.or.ncols.lt.3.or.narg.lt.3)  13096
		arg2=j_v(j_o(iob)%i(io+4))
 
		do icol=3,ncols
			if(arg.le.j_getmatel(ifunc,1,icol))goto 17
 
		enddo !icol=3,ncols  13103
		icol=ncols
						17  continue

		do irow=3,nrows
			if(arg2.le.j_getmatel(ifunc,irow,1))goto 171
 
		enddo !irow=3,nrows  13110
		irow=nrows
						171 continue
		j_v(iout)=j_bilin(j_getmatel(ifunc,1,icol-1),j_getmatel(ifunc,1,icol),&
			j_getmatel(ifunc,irow-1,1),j_getmatel(ifunc,irow,1),&
			j_getmatel(ifunc,irow-1,icol-1),j_getmatel(ifunc,irow-1,icol), &
			j_getmatel(ifunc,irow,icol-1),j_getmatel(ifunc,irow,icol),arg,arg2)
		call j_clearoption(iob,io)  ! subroutine
		return
	endif !if(isz)  13093
	if(irow.le.0.and.j_o(ifunc)%i(4).eq.j_matreg)then
		! i(1) number of rows
		! i(2) number of columns
		if(iout.gt.j_namedv)then
			write(6,*)'*value can generate only named matrix'
			j_err=.true.
			return
		endif !if(iout.gt.j_namedv)  13126
		ivout_=j_defmatrix(iout,' ',j_o(ifunc)%i(1)-1,1,j_matreg)
		do i=2,j_o(ifunc)%i(2)
			if(j_o(ifunc)%d(i).ge.arg.or.i.eq.j_o(ifunc)%i(2))then
				!(irow-1)*o(ifunc)%i(2)  y0+(x-x0)*(y1-y0)/(x1-x0)
				do irow=2,j_o(ifunc)%i(1)
					j_o(iout)%d(irow-1)=j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i-1)+(arg-j_o(ifunc)%d(i-1))* &
						(j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i)-j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i-1))/ &
						(j_o(ifunc)%d(i)-j_o(ifunc)%d(i-1))
				end do !irow=2,j_o(ifunc)%i(1)  13135
				goto 900
			end if !if(j_o(ifunc)%d(i).ge.arg.or.i.eq.j_o(ifunc)%i(2))  13133
 
		end do !i=2,j_o(ifunc)%i(2)  13132
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
		end if !if(irow.le.0.or.irow.gt.j_o(ifunc)%i(1))  13149
	end if !if(irow.le.0.and.j_o(ifunc)%i(4).eq.j_matreg)  13123
	end select !select case(j_o(ifunc)%i(1) )
	do i=2,nval
		if(j_o(ifunc)%d(i).ge.arg.or.i.eq.nval)then
			!(irow-1)*o(ifunc)%i(2)  y0+(x-x0)*(y1-y0)/(x1-x0)
			j_v(iout)=j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i-1)+(arg-j_o(ifunc)%d(i-1))* &
				(j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i)-j_o(ifunc)%d((irow-1)*j_o(ifunc)%i(2)+i-1))/ &
				(j_o(ifunc)%d(i)-j_o(ifunc)%d(i-1))
			exit
		end if !if(j_o(ifunc)%d(i).ge.arg.or.i.eq.nval)  13158
	end do !i=2,nval  13157
	case(j_iptrans) !select case(j_otype(ifunc) )
	if(narg.ne.2)then
		call j_printname('**value(',ifunc, ', arg)  wrong number of arguments')
		j_err=.true.
		return
	endif !if(narg.ne.2)  13167
	iarg2=j_igetopt(iob,io,j_marg)
	if(iarg2.le.0)iarg2=j_o(ifunc)%i2(9)
	argv=j_v(iarg2)
	!iresu=j_igetopt(iob,io,j_mresult)
	if(iresu.le.0)iresu=j_o(ifunc)%i2(10)
	call j_clearoption(iob,io)  ! subroutine
	j_v(iarg2)=j_v(iarg)
	if(iout.gt.j_mxnamedv+1)then
		nsav=iout-j_mxnamedv-1
		allocate(vout(1:nsav)); vout=j_v(j_mxnamedv+1:iout-1)
	else !if(iout.gt.j_mxnamedv+1)then
		nsav=0
	endif !if(iout.gt.j_mxnamedv+1)  13179
	call dotrans(ifunc,1)
	if(j_err)return
	!write(6,*)'<54754'
	j_v(iarg2)=argv
	j_v(iout)=j_v(iresu)
	if(nsav.gt.0)then
		j_v(j_mxnamedv+1:iout-1)=vout ;deallocate(vout)
	endif !if(nsav.gt.0)  13190
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
	! dapu=0.d0
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
	!call j_clearoption(iob,io)  ! subroutine
	case(j_iplist) !select case(j_otype(ifunc) )
	if(inde.le.0.or.inde.gt.j_o(ifunc)%i(1))then
		call j_printname('**value-function, illegal index for list ',ifunc, ' :')
		write(6,*)inde
		j_err=.true.
		return
	endif !if(inde.le.0.or.inde.gt.j_o(ifunc)%i(1))  13218
	j_v(iout)=j_v(j_o(ifunc)%i(inde))
	case(j_ipstemspline) !select case(j_otype(ifunc) )
	!npo2= %i(2)
	arg=100.*j_v(iarg)
	npo=j_o(ifunc)%i(1)  !h(2)=npo2
	npo2=j_o(ifunc)%i(2)
	if(arg.ge.j_o(ifunc)%d(npo+npo2))then
		if(j_v(iarg).gt.60.)then
			write(6,*)'**value(stemspline,height), height must be in metres, was ',j_v(iarg)
			j_err=.true.
		endif !if(j_v(iarg).gt.60.)  13231
		if(arg.le.j_o(ifunc)%d(npo+npo2)+1.)then
			j_v(iout)=j_o(ifunc)%d(npo2)
		else !if(arg.le.j_o(ifunc)%d(npo+npo2)+1.)then
			j_v(iout)=0.
		endif !if(arg.le.j_o(ifunc)%d(npo+npo2)+1.)  13235
	else !if(arg.ge.j_o(ifunc)%d(npo+npo2))then
		j_v(iout)=spld(real(arg),npo2,j_o(ifunc)%d(npo+1:),  j_o(ifunc)%d(1+2*npo:))
	endif !if(arg.ge.j_o(ifunc)%d(npo+npo2))  13230
	call j_clearoption(iob,io)  ! subroutine
	case(j_iptautspline) !select case(j_otype(ifunc) )
	arg=j_v(iarg)
	j_v(iout)=ppvalu(j_o(ifunc)%d,j_o(ifunc)%d(j_o(ifunc)%i(2)+1:),j_o(ifunc)%i(3), 4,arg,0)
	case(j_ipbitmatrix) !select case(j_otype(ifunc) )
	j=j_v(iarg)
	if(j.gt.j_o(ifunc)%i(1).or.j.le.0)then
		if(j_linkoption(iob,io,j_many).ge.0)then
			j_v(iout)=0
			!	call j_clearoption(iob,io)  ! subroutine
			goto 900
		endif !if(j_linkoption(iob,io,j_many).ge.0)  13250
		call j_printname('**value: first index has illegal value for bitmatrix ',ifunc, ' ')
		j_err=.true.;return
	endif !if(j.gt.j_o(ifunc)%i(1).or.j.le.0)  13249
	if(narg.gt.2)then
		inde2=j_v(j_o(iob)%i(io+4))
		if(inde2.gt.j_o(ifunc)%i(2).or.inde2.lt.j_o(ifunc)%i(3))then
			if(j_linkoption(iob,io,j_many).ge.0)then
				j_v(iout)=0
				!	call j_clearoption(iob,io)  ! subroutine
				goto 900
			endif !if(j_linkoption(iob,io,j_many).ge.0)  13261
			call j_printname('**value: second argument has illegal value for bitmatrix ',ifunc, ' ')
			j_err=.true.;return
		endif !if(inde2.gt.j_o(ifunc)%i(2).or.inde2.lt.j_o(ifunc)%i(3))  13260
	endif !if(narg.gt.2)  13258
	j_v(iout)=j_ibittest(ifunc,j,inde2)
	if(j_linkoption(iob,io,j_many).ge.0)call j_clearoption(iob,io)  ! subroutine
	case (j_iplaaspoly) !select case(j_otype(ifunc) )
	j_v(iout)=crkd(real(j_v(iarg)),real(j_o(ifunc)%d))
	if(j_err)return
	case default !select case(j_otype(ifunc) )
	call j_printname('**value: first argument has not valid type ',ifunc, ' ')
	j_err=.true.
	return
	end select !select case(j_otype(ifunc) )
	900 call j_clearoption(iob,io)  ! subroutine !  io=io+narg+3
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
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_namedv)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  13316
	if(narg.gt.0)then
 
		write(6,*)'*ran: seed not yet '
		j_err=.true.
		return
 
	end if !if(narg.gt.0)  13319
	if(j_otype(iout).eq.j_ipmatrix)then
		do j=1,j_o(iout)%i(3)
			j_o(iout)%d(j)=ranf()
		enddo !j=1,j_o(iout)%i(3)  13327
	else !if(j_otype(iout).eq.j_ipmatrix)then
 
		j_v(iout)=ranf()
	endif !if(j_otype(iout).eq.j_ipmatrix)  13326
 
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
	endif !if(iout.eq.j_ivresult)  13362
 
	n=j_v(j_o(iob)%i(io+2))
	ntot=j_v(j_o(iob)%i(io+3))
	if(n.lt.0.or.n.gt.ntot.or.ntot.le.0)then
		WRITE(6,*)'*select: illegal arguments ',n,ntot
		j_err=.true.
	endif !if(n.lt.0.or.n.gt.ntot.or.ntot.le.0)  13369
 
	if(j_err)return
	ivout=j_defmatrix(iout,' ',ntot,1,j_matreg)
 
	if(n.eq.0)return
	if(n.eq.ntot)then
		j_o(ivout)%d(1:ntot)=1.
		return
	endif !if(n.eq.ntot)  13378
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
 
			endif !if(ir.eq.icount)  13390
 
		enddo !j=1,ntot  13388
	enddo !i=1,n  13383
 
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
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_namedv)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  13420
 
	if(narg.gt.1)then
 
		write(6,*)'*random: seed not yet '
		j_err=.true.
		return
 
	end if !if(narg.gt.1)  13424
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
					endif !if(j_o(iarg)%d(ncol+i).ge.ra)  13438
				enddo !i=1,ncol-1  13436
				j_o(iout)%d(j)=j_o(iarg)%d(ncol)
 
				!		j_o(iout)%d(j)=ranf()
			enddo jloop !op:		do j=1,j_o(iout)%i(3)  13433
 
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
 
					endif !if(j_o(iarg)%d(ncol+i).ge.ra)  13457
 
 
				enddo !i=2,ncol  13455
				!		j_o(iout)%d(j)=ranf()
			enddo !j=1,j_o(iout)%i(3)  13452
		endif !if(diskr)  13432
	elseif(diskr)then
		do i=1,ncol-1
			!y=y0+(y1-y0)*(x-x0)/(x1-x0)  here x is value of rnadom uniform and y is the xavalue
			if(j_o(iarg)%d(ncol+i).ge.ra)then
				!write(6,*)'i ',i,j_o(iarg)%d(i-1),j_o(iarg)%d(i),j_o(iarg)%d(i-1),j_o(iarg)%d(ncol+i-1)
 
 
				j_v(iout)=j_o(iarg)%d(i)
 
				return
			endif !if(j_o(iarg)%d(ncol+i).ge.ra)  13476
		enddo !i=1,ncol-1  13474
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
 
			endif !if(j_o(iarg)%d(ncol+i).ge.ra)  13493
		enddo !i=2,ncol  13491
		!	j_v(iout)=ranf()
	endif !if(j_otype(iout).eq.j_ipmatrix)  13431
 
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
	!	call j_clearoption(iob,io)
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	!	write(6,*)'iout,type',iout,j_otype(iout)
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_namedv)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  13555
	if(narg.ge.3)then
		write(6,*)'*rann: seed not yet '
		j_err=.true.
		return
	end if !if(narg.ge.3)  13558
 
	if(narg.ge.1)then
		av=j_v(j_o(iob)%i(io+2))
		if(narg.ge.2)then
			sd=j_v(j_o(iob)%i(io+3))
		else !if(narg.ge.2)then
			sd=1.
		endif !if(narg.ge.2)  13566
	else !if(narg.ge.1)then
		av=0.
		sd=1.
	endif !if(narg.ge.1)  13564
	if(j_otype(iout).eq.j_ipmatrix)then
		do j=1,j_o(iout)%i(3)
			j_o(iout)%d(j)=gennor(av,sd)
		enddo !j=1,j_o(iout)%i(3)  13576
	else !if(j_otype(iout).eq.j_ipmatrix)then
		j_v(iout)=gennor(av,sd)
	endif !if(j_otype(iout).eq.j_ipmatrix)  13575
 
	return
end subroutine rann !subroutine rann(iob,io)
 
subroutine ranpoi(iob,io)  !calls function ignpoi from netlib ranlib
	! Section ranpoi ranpoi() Poisson
	! ranpoi(]myy[//
	! returns a random Poisson variable with expected value and variance ]myy[
	! endsection
 
 
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_namedv)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  13597
	rmu=j_v(j_o(iob)%i(io+2))
	if(rmu.le.0)then
		write(6,*)'*ranpoi* mu is nonpositive ',rmu
		j_err=.true.
		return
 
	endif !if(rmu.le.0)  13601
	if(j_otype(iout).eq.j_ipmatrix)then
		do j=1,j_o(iout)%i(3)
			j_o(iout)%d(j)=ignpoi(rmu)
		enddo !j=1,j_o(iout)%i(3)  13608
	else !if(j_otype(iout).eq.j_ipmatrix)then
 
		j_v(iout)=ignpoi(rmu)
	endif !if(j_otype(iout).eq.j_ipmatrix)  13607
 
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
		if(j_otype(iout).ne.j_ipmatrix.or.iout.eq.j_ivresult.or.iout.gt.j_namedv)call j_del(iout)
	endif !if(j_otype(iout).ne.j_ipreal)  13662
	n_=j_v(j_o(iob)%i(io+2))
	if(n_.le.0)then
		write(6,*)'*ranbin: n (=1st arg) must be positive, it was ',n_
		j_err=.true.
 
	endif !if(n_.le.0)  13666
	pp_=j_v(j_o(iob)%i(io+3))
	if(pp_.lt.0.or.pp_.gt.1.)then
		write(6,*)'*ranbin: p (=2nd arg) must be between 0 and 1, it was ',pp_
		j_err=.true.
	endif !if(pp_.lt.0.or.pp_.gt.1.)  13672
	if(j_err)return
	if(j_otype(iout).eq.j_ipmatrix)then
		if(pp_.eq.0.)then
			j_o(iout)%d=0.
		elseif(pp_.eq.1.)then !if(pp_.eq.0.)then
			j_o(iout)%d=n_
		else !if(pp_.eq.0.)then
			do j=1,j_o(iout)%i(3)
				j_o(iout)%d(j)=ignbin(n_,pp_)
			enddo !j=1,j_o(iout)%i(3)  13683
		endif !if(pp_.eq.0.)  13678
 
	else !if(j_otype(iout).eq.j_ipmatrix)then
		if(pp_.eq.0.)then
			j_v(iout)=0.
		elseif(pp_.eq.1.)then !if(pp_.eq.0.)then
			j_v(iout)=n_
		else !if(pp_.eq.0.)then
			j_v(iout)=ignbin(n_,pp_)
		endif !if(pp_.eq.0.)  13689
	endif !if(j_otype(iout).eq.j_ipmatrix)  13677
 
	return
 
end subroutine !subroutine ranbin(iob,io)
 
 
 
 
subroutine cpu(iob,io)  !Jlp22-function cpu()
	! real,dimension(:),allocatable ::vc
	! double precision,dimension(:),allocatable ::vc2
 
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
	! allocate(vc(1:9),vc2(1:9))
	! 19	write(6,*)'anluku'
	! read(5,*)j_dapu
	! write(6,*)fraction(j_dapu),exponent(j_dapu)
	! goto 19
	! if(aluku.eq.0.0)return
	! vc=aluku
	! write(17,*)vc
	! goto 19
	! vc=2.d0/3.d0
	! vc2=2.d0/3.d0
	! do i=1,20
	! j_o(j_ivdollar)%i(i)=2*i
	! enddo !i=1,20  13476
	! j_o(j_ivdollar)%d=j_1/3.d0
	! j_o(j_ivdollar)%r=j_1/3.d0
	! j_dapu2=j_o(j_ivdollar)%d(1)*vc2(1)
	! j_dapu=j_o(j_ivdollar)%r(1)*vc(1)
	! j_dapu=9.d0*j_dapu
	! j_dapu2=9.d0*j_dapu2
	! write(6,*)'onko 2 ',j_dapu,j_dapu2
 
	! call cpu_time(time0)
	! do k=1,1000
	! ibas=0
	! j_dapu=j_0
	! do i=1,1000000
	! j_dapu=j_dapu+dot_product(vc2(1:9),j_o(j_ivdollar)%d(ibas+1:ibas+9))
 
	! ibas=ibas+9
	! enddo !i=1,1000000  13491
	! enddo !k=1,1000  13488
	! call cpu_time(time)
 
	! write(6,*)'d2 ',time-time0,j_dapu
 
	! ! call cpu_time(time0)
	! ! do k=1,1000
	! ! ibas=0
	! ! j_dapu=j_0
	! ! do i=1,1000000
	! ! j_dapu=j_dapu+dot(vc2(1:9),j_o(j_ivdollar)%d(ibas+1:ibas+9))
 
	! ! ibas=ibas+9
	! ! enddo !i=1,1000000  13490
	! ! enddo !k=1,1000  13487
	! call cpu_time(time)
 
	! write(6,*)'d2dot ',time-time0,j_dapu
 
 
	! call cpu_time(time0)
	! do k=1,1000
	! ibas=0
	! j_dapu=j_0
	! do i=1,1000000
	! j_dapu=j_dapu+dot_product(vc(1:9),j_o(j_ivdollar)%r(ibas+1:ibas+9))
 
	! ibas=ibas+9
	! enddo !i=1,1000000  13520
	! enddo !k=1,1000  13517
	! call cpu_time(time)
 
	! write(6,*)'sing ',time-time0,j_dapu
 
	! do k=1,1000
	! ibas=0
	! j_dapu=j_0
	! do i=1,1000000
	! j_dapu=j_dapu+dot_product(vc(1:9),j_o(j_ivdollar)%r(ibas+j_o(j_ivdollar)%i(1:9)))
 
	! ibas=ibas+9
	! enddo !i=1,1000000  13533
	! enddo !k=1,1000  13530
	! call cpu_time(time)
 
	! write(6,*)'d2ite ',time-time0,j_dapu
	! call cpu_time(time0)
	! do k=1,1000
	! ibas=0
	! j_dapu=j_0
	! do i=1,1000000
	! j_dapu=j_dapu+dot_product(vc(1:9),j_o(j_ivdollar)%r(ibas+j_o(j_ivdollar)%i(1:9)))
 
	! ibas=ibas+9
	! enddo !i=1,1000000  13546
	! enddo !k=1,1000  13543
	! call cpu_time(time)
 
	! write(6,*)'sinite',time-time0,j_dapu
 
	! do k2=1,1000
	! ibas=0
	! j_dapu=j_0
	! do i=1,1000000
	! do k=1,9
	! j_dapu=j_dapu+vc2(k)*j_o(j_ivdollar)%d(ibas+j_o(j_ivdollar)%i(k))
	! enddo !k=1,9  13560
	! ibas=ibas+9
	! enddo !i=1,1000000  13559
	! enddo !k2=1,1000  13556
	! call cpu_time(time)
 
	! write(6,*)'d2iteloop ',time-time0,j_dapu
	! call cpu_time(time0)
	! do k2=1,1000
	! ibas=0
	! j_dapu=j_0
	! do i=1,1000000
	! do k=1,9
	! j_dapu=j_dapu+vc(k)*j_o(j_ivdollar)%r(ibas+j_o(j_ivdollar)%i(k))
	! enddo !k=1,9  13574
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
	end if !if(narg.le.0)  13884
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
	end if !if(narg.le.0)  13913
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
 
subroutine jump(iob,io) ! Jlp22-function jump
 
	j_njump=j_njump+1
	if(j_njump.gt.j_mxjump)then
		write(6,*)'*j*jump:, jump limit ',j_mxjump ,' exceeded'
		j_err=.true.
		return ! error line perhaps not properly printed
	endif !if(j_njump.gt.j_mxjump)  13938
	j_jumpio(j_njump)=io+4
	j_jumpiob(j_njump)=iob
	io=j_o(iob)%i(io+2)
	return
end subroutine jump !subroutine jump(iob,io)
 
subroutine back(iob,io) !Jlp22-function back io?
 
	if(j_njump.le.0)then
		write(6,*)'**back without jump'
		j_err=.true.;return !error line not properly printed
	else if(iob.ne.j_jumpiob(j_njump))then !if(j_njump.le.0)then
		write(6,*)'**back in different trans than last jump'
		j_err=.true.;return
	end if !if(j_njump.le.0)  13951
	io=j_jumpio(j_njump)
	j_njump=j_njump-1
	return
end subroutine back !subroutine back(iob,io)
 
subroutine printresult(iob,io,level)  !
	! Section printresult printresult() and printresult2() for ending ; and ;;
	! The Jlp22 interpreter translates ';'  at the end of the
	! line to a call to printresult() function and ';;' to a call to printresult2()
	! The output of a function is printed by writing ';' or ';;' at the end of the line. The excution of implied print()
	! is dependent on the value of ]Printoutput[. If ]printoutput[ =0,
	! then the output is not printed, If ]printoutput[ =1, then ';' is
	! causing printing, if ]Printoutput[ =2 then only ';;'-outputs are
	! printed, and if ]Printoutput[ =3, then both ';' and ';;' outputs are printed.
	!endheader
	! Note printresult() and printresult2() are  simple functions
	!which just test the value of
	! ]Printoutput[ and then call the  printing subroutine, if needed.
	! endnote
	!endsection
 
 
	ilevel=j_v(j_ivprintresult)
 
	if(ilevel.eq.level.or.ilevel.eq.3)then
		!		write(6,*)j_o(iob)%i(io+1:io+4)
		if(j_o(iob)%i(io+1).eq.1)then
			irg=j_otype(j_o(iob)%i(io+2))
			!		write(6,*)'irg',irg,j_otype(irg)
			if(j_otype(irg).eq.j_ipchar)then
				call j_getchar(iv2,j_cline,le)
				write(6,'(a)')j_cline(1:le)
				return
			endif !if(j_otype(irg).eq.j_ipchar)  13987
		endif !if(j_o(iob)%i(io+1).eq.1)  13984
		call print(iob,io)
	endif !if(ilevel.eq.level.or.ilevel.eq.3)  13982
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
	end do !i=1,narg  14013
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
	end do !i=1,narg  14024
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
				end if !if(j_iper.le.j_keepper)  14049
				goto 500
			else !if(j_lastpoint(j_iper).ge.j_nextnodes(j_iper-1))then
				j_v(j_ivperiod)=j_iper
				j_lastpoint(j_iper)=j_lastpoint(j_iper)+1
				j_nextnodes(j_iper)=0
				io=j_istarts(j_iper,j_lastpoint(j_iper))
				iob=j_ivsimu    !!!!!!!!!!!!1
				return
			end if !if(j_lastpoint(j_iper).ge.j_nextnodes(j_iper-1))  14047
		else !if(j_keepper.lt.j_nper)then
			io=j_ioret
			return
		end if !if(j_keepper.lt.j_nper)  14045
	end if !if(j_iper.ge.j_nper)  14044
	if(j_nextnodes(j_iper).le.0)then   !nextnodes for period whee next is give
		if(j_iper.eq.j_keepper)then
			io=j_ioret;return
		end if !if(j_iper.eq.j_keepper)  14068
		5000  	continue
		iper1=j_iper-1
		if(iper1.le.0)then
			write(6,*)'*there is no next to start simulation'
			j_err=.true.
			return
		endif !if(iper1.le.0)  14073
		if(j_lastpoint(j_iper).ge.j_nextnodes(j_iper-1))then
			j_iper=j_iper-1
			if(j_iper.le.1)then
				j_iper=0
				!    write(6,*)'loppu tul endtrtpppssä'
				io=j_ioret;return
			else !if(j_iper.le.1)then
				goto 5000
			end if !if(j_iper.le.1)  14080
		end if !if(j_lastpoint(j_iper).ge.j_nextnodes(j_iper-1))  14078
	end if !if(j_nextnodes(j_iper).le.0)  14067
	j_iper=j_iper+1
	j_nextnodes(j_iper)=0  !index according to the next period
	io=j_istarts(j_iper,1)
	j_lastpoint(j_iper)=1
	j_v(j_ivperiod)=j_iper
	return
end subroutine endtrt !subroutine endtrt(iob,io)
 
 
 
subroutine draw(iob,io) ! draw()
	! Section draw draw() Draws a function
	! draw() draws a function.
	! endheader
	!Option
	! Output & 1& FIGURE & The FIGURE object created or updated.
	! func& N | 1 & Code &  Code option telling how the y-variable is computed.
	!@@draw
	! mark & N | 1 & REAL | CHAR & The mark used in the plot.
	!Numeric values refer to.
	! mark types of Gnuplot. The mark can be given also as CHAR varible or constant.
	!width & 0 | 1 & REAL & the width of the line
	!endoption
	! Ex drawex Example of draw()
	! fi=draw(func->sin(x),x->x,xrange->(0,2*Pi),color->Blue,continue->fcont)
	! fi=draw(func->cos(x),x->x,xrange->(0,2*Pi),color->Red,append->,continue->fcont)
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
	!Continue=1  !Errors
	!fi=draw(func->sin(x),x->x)
	!fi=draw(xrange->(1,100),func->Sin(x),x->x)
	!Continue=0
	! endex
	!Note ]fi[=draw() produces or updates file ]fi[.jfig] which contains
	! Gnuplot commands and file ]fi[.jfi0 containg data.
	!endnote
	!endsection
 
 
 
	real ::funcval
	character*3 ch
	character*2 ch0
	logical p
	p=j_v(j_ivdebug).gt.j_0
	linkfunc=j_codelink(iob,io,j_mfunc)
	if(p)write(6,*)'linkfunc',linkfunc,'teku:'
	if(p)write(6,'(20i5/)')j_o(iob)%i(0:j_o(iob)%i(0))
	if(linkfunc.eq.0)then
		write(6,*)'**draw: function not defined';goto 99
 
	endif !if(linkfunc.eq.0)  14152
 
	!	write(6,*)'draw0io',io
	!	write(6,'(20i5)')j_o(iob)%i(1:j_o(iob)%i(0))
	call j_startfig(iob,io)  !
	!		write(6,*)'draw0io,gpiout',io,j_gpiout,j_o(j_gpiout)%i
	!			write(6,*)'<88pp88AFTER',j_o(j_gpiout)%i(1:3)
	!		j_o(j_gpiout)%i(1)=0  !number of lines
	!	j_o(j_gpiout)%i(2)=j_gplines !number of allocated lines
	!		j_o(j_gpiout)%i(3)=0  !number plot commands
	! do i=1,j_o(j_gpiout)%i(1)
	! write(6,*)j_o(j_gpiout)%txt(i)(1:j_o(j_gpiout)%i2(i))
	! enddo
 
	call j_clearoption(iob,io)
	if(j_gpix.eq.0)then
		write(6,*)'**x-> missing'
		j_err=.true.
		return
	endif !if(j_gpix.eq.0)  14170
 
	if(j_o(j_gpiout)%i(5).eq.0)then
		call j_getfile(nu,rw='w',ivout=nuf,ivfile=j_gpiout,ext='.jfi0',replace=.true.)
	else !if(j_o(j_gpiout)%i(5).eq.0)then
		call j_getfile(nu,rw='a',ivout=nuf,ivfile=j_gpiout,ext='.jfi0')
		write(nu,*)' '
		write(nu,*)' '
	endif !if(j_o(j_gpiout)%i(5).eq.0)  14176
	if(j_err)return
	j_o(j_gpiout)%i(5)=j_o(j_gpiout)%i(5)+1
	!	j_o(j_gpiout)%i(3)=j_o(j_gpiout)%i(3)+1
	!number of functions
 
	if(j_gplexlabel.eq.0)then
		call j_getline(j_ivnames,j_gpix,j_varname1,le)
		if(j_err)return
		!		write(6,*)'<8888tas'
		call j_replacefig(2,'set xlabel "'//j_varname1(1:le)//'"')
		!		else
		!			call j_replacefig(2,'set xlabel "'//j_gpxlabel(1:j_gplexlabel)//'"')
	endif !if(j_gplexlabel.eq.0)  14188
	!	endif !if(j_gpix.eq.0)then
	if(j_gpappend)then
		ch0=', '
		! j_o(j_gpiout)%txt(6)(i2+1:i2+1)=','
		! i2=i2+1
	else !if(j_gpappend)then
		ch0='p '
 
	endif !if(j_gpappend)  14197
	if(j_err)return
 
	call j_getline(j_ivnames,nuf,j_filename,le) !note this takes ' ' into filename
	write(ch,'(i3)')j_o(j_gpiout)%i(5)-1
	!ch=char(j_o(j_gpiout)%i(5)+47)
	iaa=1
	if(ch(1:1).eq.' ')iaa=2
	if(ch(2:2).eq.' ')iaa=3
 
	call j_gpplot(ch0//j_filename(1:le)//' i '//ch(iaa:3)//' w l'//j_gplw)
	if(j_gpicolor.ge.1)call j_gpplot(j_gpcolors(j_gpicolor),add=.true.)
 
 
	!	write(6,*)'draw8io ',io
	!	p=.true.
 
 
 
	if(j_o(j_gpiout)%d(1).eq.j_inf.and.j_o(j_gpiout)%d(2).eq.j_inf)then
		write(6,*)'** xrange-> not given in draw';goto 99
	end if !if(j_o(j_gpiout)%d(1).eq.j_inf.and.j_o(j_gpiout)%d(2).eq.j  14223
 
	if(j_gpix.le.0)then
		write(6,*)'x-> not given in draw';goto 99
	end if !if(j_gpix.le.0)  14227
 
	if(j_o(j_gpiout)%d(1).ge.j_o(j_gpiout)%d(2))then
		write(6,*)'**draw: xrange (',j_o(j_gpiout)%d(1),',',j_o(j_gpiout)%d(2),') not properly defined';goto 99
	end if !if(j_o(j_gpiout)%d(1).ge.j_o(j_gpiout)%d(2))  14231
	if(j_err)goto 99
 
	dxx=(j_o(j_gpiout)%d(2)-j_o(j_gpiout)%d(1))/j_gppoints
 
	xx=j_o(j_gpiout)%d(1)
 
	do i=1,j_gppoints
		if(i.eq.j_gppoints)xx=j_o(j_gpiout)%d(2)
		j_v(j_gpix)=xx
		!call dotrans(iob,iofunc)
		if(j_err)goto 99
 
		yyy=j_codevalue(iob,linkfunc)
		write(nu,*)xx,yyy
		xx=xx+dxx
	end do !i=1,j_gppoints  14240
 
	!	if(p)write(6,*)'<000667io  ',io
	!write(6,*)'<747close ',nu,'nul',nul
	call j_closeunit(nu)
 
	!if(j_gpshow)call j_showfig(j_gpiout)
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
	!Args&1- &REAL | MATRIX & The points which are connected:
	! \begin{itemize}
	! \item  x1,...,xn,y1,...,yn The x-coordinates and y-coordinates,
	! $n \geq 1$
	! \item   If there is only one argument which is a
	! matrix object having two rows, then the first row is assumed to give the x values
	! and the second row the y values.
	! \item   If there are two matrix (vector) arguments, then
	! the first matrix gives the x-values and the second matrix gives the y-values.
	! It does not matter if arguments are row or column vectors.
	! \end{itemize}
	!@@figure
	! label & N | 1 & CHAR & Label written to the end of line. If arguments define only one point,
	! then with label-> option one can write text to any point.
	! mark & N | 1 & REAL | CHAR & The mark used in the plot.
	! break & N | 0 & & The line is broken when a x-value is smaller than the previous one.
	!set & N|1 & REAL<6 & Set to which lines are put. If the option is not present,
	! then a separate Gnuplot plot command with possible color and width information
	! is generated for each drawline() and data points are stored
	! in file ]fi[.jfi0, i.e. the same file used by plotyx().
	! If set is given e.g as set->3, then it is possible to plot a large number of lines
	! with the same width and color. The data points are stored into file ]fi[.jfi3. This is
	! useful e.g. when drawing figures showing transportation of timber to factories
	! for huge number of sample plots.
	!Numeric values refer to Gnuplot mar types.
	! The mark can be given also as CHAR varible or constant.
	!width & 0 | 1 & REAL & the width of the line. Default: width->1
	!label& N |1 &CHAR & Text plotted to the end of line.
	!endoption
	! Ex drawlineex Example of drawline()
	! ;drawlineex:
	! ** Example of drawline() (line  13761 file c:/j3/j.f90)
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
	! Continue=1  !Errors
	! fi=draw(func->sin(x),x->x)
	! fi=draw(xrange->(1,100),func->Sin(x),x->x)
	! Continue=0
	! ;if(wait);pause
	! ;return
	! endex
	!Note if a line is not visible, this may be caused by the fact that
	!the starting or ending point is outside the range specified by xrange-> or yrange->.
	!endnote
	!endsection
 
 
 
	logical ::onlylabel,p,makeplot,isbreak,exis
	character*2 ch
	call j_startfig(iob,io)
	!	write(6,*)'<8889',j_o(j_gpiout)%i(1:3)
	!		j_o(j_gpiout)%i(1)=0  !number of lines
	!	j_o(j_gpiout)%i(2)=j_gplines !number of allocated lines
	!		j_o(j_gpiout)%i(3)=0  !number plot commands
	!	do i=1,j_o(j_gpiout)%i(1)
	!		write(6,*)j_o(j_gpiout)%txt(i)(1:j_o(j_gpiout)%i2(i))
	!	enddo
	!	write(6,*)'<486',io
	!write(6,*)'j_nopt',j_nopt
	isbreak=j_linkoption(iob,io,j_mbreak,clear=.true.).ge.0
	!		call j_clearoption(iob,io)
	!write(6,*)'j_noptaft',j_nopt
	!write(6,*)'<486append',io,j_gpappend
	p=j_v(j_ivdebug).gt.j_0
	iarg=j_gparg(1)
	!	write(6,*)'<33>',j_gpiout,j_gpnarg,j_gparg,j_gpappend,
	! 177	write(6,1000)'minmax>'
	! read(5,*)xmin,xmax
	! if(xmin.eq.0..and.xmax.eq.0.)stop 'll'
	! call j_range(xmin,xmax,xmin2,xmax2)
	! write(6,*)xmin2,xmax2
	!goto 177
	makeplot=.true.
	exis=.false.
	if(j_gpset.gt.0)then
		if(j_o(j_gpiout)%i(4+j_gpset).eq.0)then
			call j_getfile(nu,rw='w',ivout=nuf,ivfile=j_gpiout,ext='.jfi'//char(48+j_gpset),replace=.true.)
 
		else !if(j_o(j_gpiout)%i(4+j_gpset).eq.0)then
			call j_getfile(nu,rw='a',ivout=nuf,ivfile=j_gpiout,ext='.jfi'//char(48+j_gpset))
			write(nu,*)' '
		endif !if(j_o(j_gpiout)%i(4+j_gpset).eq.0)  14376
		if(j_err)return
		call j_getline(j_ivnames,nuf,j_filename,le)
		if(j_err)return
		!	j_filename(le+1:le+8)=' i '//char(47+j_gpset)  !i = index
		!	le=le+4
		j_o(j_gpiout)%i(4+j_gpset)=j_o(j_gpiout)%i(4+j_gpset)+1
		if(j_o(j_gpiout)%i(4+j_gpset).gt.1)makeplot=.false.
 
	elseif(j_otype(iarg).eq.j_ipchar)then
		call j_getchar(iarg,j_filename(2:),le)
		inquire(file = j_filename(2:le+1) , exist=exis)
		if(.not.exis)then
			write(6,*)j_filename(2:le+1),' does not exist'
			j_err=.true.;return
		endif !if(.not.exis)  14394
		j_filename(1:1)='"'
		j_filename(le+2:le+2)='"'
		le=le+2
	else
		!if(j_gpset.gt.0)then
		j_filename(1:3)="'-'"
		le=3
	endif !if(j_gpset.gt.0)  14375
	! j_o(j_gpiout)%i(1)=0  !number of lines
	! j_o(j_gpiout)%i(2)=j_gplines !number of allocated lines
	! j_o(j_gpiout)%i(3)=0  !number plot commands
	! j_o(j_gpiout)%i(4)=j_gpplots  !number of availablr plotcommands
	! j_o(j_gpiout)%i(5:10
	!	if(makeplot)j_o(j_gpiout)%i(3)=j_o(j_gpiout)%i(3)+1 !number plot-commands gpplot does
	onlylabel=.false.
	if(j_gpnarg.eq.2)then
		if(j_otype(j_gparg(1)).eq.j_ipreal.and.j_otype(j_gparg(2)).eq.j_ipreal)then
			if(j_gplelabel.eq.0)then
				write(6,*)'with two real arguments there must be label->'
				j_err=.true.;return
			endif !if(j_gplelabel.eq.0)  14415
			onlylabel=.true.
		endif !if(j_otype(j_gparg(1)).eq.j_ipreal.and.j_otype(j_gparg(2))  14414
	endif !if(j_gpnarg.eq.2)  14413
	if(allocated(j_gpval))deallocate(j_gpval)
	! character*5 ::j_gppt=' pt 0'
	! character*5:: j_gplw=' lw 2'
	! character*5 ::j_gpps=' ps 2'
	! character*5 ::j_gplt=' lt 1'
	!allocate(j_o(j_gpiout)%d(1:10))  ! given xmin xmax xmin2 xmax2 ymin ymax obtained
 
 
	if(makeplot)then
 
		if(j_gpappend)then
			ch=' ,'
 
		else !if(j_gpappend)then
			ch='p '
		endif !if(j_gpappend)  14432
 
		! character*5 ::j_gppt=' pt 0'
		! character*5:: j_gplw=' lw 2'
		! character*5 ::j_gpps=' ps 2'
		! character*5 ::j_gplt=' lt 1'
		!		j_gppt(5:5)=char(48+j_o(j_gpiout)%i(3))
		!write(6,*)'<837drawline',j_o(j_gpiout)%i(1)
		! do iii=1,j_o(j_gpiout)%i(1)
		! write(6,*)iii,j_o(j_gpiout)%txt(iii)(1:j_o(j_gpiout)%i2(iii))
		! enddo
		!	write(6,*)'<777tasmark style ',j_gpmark,j_gpstyle
 
		if(j_gpstyle.gt.0.and.j_gpmark.ne.j_ivzero.and..not.onlylabel)then
			call j_gpplot(ch//j_filename(1:le)//' w lp'//j_gppt//j_gpps//j_gplw)
 
		elseif(j_gpstyle.gt.0.and..not.onlylabel)then !if(j_gpstyle.gt.0.and.j_gpmark.ne.j_ivzero)then
			call j_gpplot(ch//j_filename(1:le)//' w l'//j_gplw)
		else !if(j_gpstyle.gt.0.and.j_gpmark.ne.j_ivzero)then
			if(onlylabel)then
				call j_gpplot(ch//j_filename(1:le)//' w p'//j_gppt//' ps 0')
			else
				call j_gpplot(ch//j_filename(1:le)//' w p'//j_gppt//j_gpps)
			endif !if(onlylabel)  14456
		endif !if(j_gpstyle.gt.0.and.j_gpmark.ne.j_ivzero.and..not.onlyla  14450
 
 
		if(j_gpicolor.ge.1)call j_gpplot(j_gpcolors(j_gpicolor),add=.true.)
		!write(6,*)j_o(j_gpiout)%txt(6)
 
 
	endif !if(makeplot)  14430
	!	write(6,*)'<464664>',j_gpnarg,exis
	if(exis)return
	!	iarg=j_gparg(1)
	!write(6,*)'<464664>',j_gpnarg
 
	ili=j_o(j_gpiout)%i(1)
	if(j_gpnarg.eq.1)then
		if(j_otype(iarg).ne.j_ipmatrix)then
			write(6,*)'only argument must be matrix'
			j_err=.true.;return
		endif !if(j_otype(iarg).ne.j_ipmatrix)  14476
		!		if(j_o(iarg)%i(1).lt.2)then
		!			write(6,*)'argument matrix must have at least two rows'
		!			j_err=.true.;return
		!		endif !if(j_o(iarg)%i(1).lt.2)  14581
 
		npoints=j_o(iarg)%i(3)/2
		allocate(j_gpval(1:2*npoints))
		j_gpval=j_o(iarg)%d(1:2*npoints)
	elseif(j_gpnarg.eq.2.and.j_otype(iarg).eq.j_ipmatrix)then !if(j_gpnarg.eq.1)then
		iarg2=j_gparg(2)
		if(j_otype(iarg2).ne.j_ipmatrix)then
			write(6,*)'also second argument must be matrix'
			j_err=.true.;return
		endif !if(j_otype(iarg2).ne.j_ipmatrix)  14490
		if(j_o(iarg)%i(3).ne.j_o(iarg2)%i(3))then
			write(6,*)'argument sizes ',j_o(iarg)%i(3),j_o(iarg2)%i(3),' are not campatible'
			j_err=.true.;return
		endif !if(j_o(iarg)%i(3).ne.j_o(iarg2)%i(3))  14494
		npoints=j_o(iarg)%i(3)
		allocate(j_gpval(1:2*npoints))
		j_gpval(1:npoints)=j_o(iarg)%d(1:npoints)
		j_gpval(npoints+1:2*npoints)=j_o(iarg2)%d(1:npoints)
	else !if(j_gpnarg.eq.1)then
		npoints=j_gpnarg/2
		allocate(j_gpval(1:2*npoints))
		j_gpval=j_v(j_gparg(1:2*npoints))
	endif !if(j_gpnarg.eq.1)  14475
 
	!j_o(j_gpiout)%txt(ili)="plot '-'"
	!	if(.not.onlylabel)then
	if(j_gpset.eq.0)then
		do ip=1,npoints
			if(p)write(6,*)'<88ip',ip
			if(isbreak.and.ip.gt.1)then
				if(j_gpval(ip).lt.j_gpval(ip-1))call j_putfigbreak()
			endif !if(isbreak.and.ip.gt.1)  14513
			call j_putfigxy(j_gpval(ip),j_gpval(ip+npoints))
			!	write(j_o(j_gpiout)%txt(ili+ip),*)j_gpval(ip),j_gpval(ip+npoints)
			!	j_o(j_gpiout)%i2(ili+ip)=len_trim(j_o(j_gpiout)%txt(ili+ip))
 
 
		enddo !ip=1,npoints  14511
		call j_putfig('e')
	else !if(j_gpset.eq.0)then
		do ip=1,npoints
			if(p)write(6,*)'<889ip',ip
			if(isbreak.and.ip.gt.1)then
				if(j_gpval(ip).lt.j_gpval(ip-1))call j_putfigbreak(nu=nu)
			endif !if(isbreak.and.ip.gt.1)  14526
			call j_putfigxy(j_gpval(ip),j_gpval(ip+npoints),nu=nu)
			!	write(nuf,*)real(j_gpval(ip)),real(j_gpval(ip+npoints))
			!	write(j_o(j_gpiout)%txt(ili+ip),*)j_gpval(ip),j_gpval(ip+npoints)
			!	j_o(j_gpiout)%i2(ili+ip)=len_trim(j_o(j_gpiout)%txt(ili+ip))
 
 
		enddo !ip=1,npoints  14524
		call j_closeunit(nu)  ! nu is unit nuf is the char constant for the name
 
	endif !if(j_gpset.eq.0)  14510
 
	!	endif !if(.not.onlylabel)  14500
 
	if(j_gplelabel.gt.0)then
		ifo=j_font(j_gplabel,j_gplelabel)
		if(ifo.gt.0)then
			call j_putfig('set label "'//j_gplabel(1:ifo-1)//'" at '// &
				j_chr10(j_gpval(npoints))//','//j_chr10(j_gpval(2*npoints))//&
				' font "'//j_gplabel(ifo+1:j_gplelabel-1)//'"')
 
		else
			call j_putfig('set label "'//j_gplabel(1:j_gplelabel)//'" at '// &
				j_chr10(j_gpval(npoints))//','//j_chr10(j_gpval(2*npoints)))
		endif !if(ifo.gt.0)  14544
	endif !if(j_gplelabel.gt.0)  14542
	deallocate(j_gpval)
	!if(j_gpshow)call j_showfig(j_gpiout)
	!	io=j_gpnewio
	return
 
 
 
end subroutine !subroutine drawline(iob,io)
 
subroutine drawline3(iob,io)
	integer,dimension(:),pointer::arg
	logical::append,show,ismark,islabel
	character*40 mark
	character*1 ch
	call j_startfunction(iob,io,0,narg,arg,iout)
	!		write(6,*)'<66iout,append,j_otype(iout)',iout,append,j_otype(iout)
	! call j_getoption_index(iob,io,j_mshow,-1,1,j_ipreal,.false.,noptarg,j_optarg0)
	! if(noptarg.le.0)then
	! show=.true.
	! else !if(noptarg.le.0)then
	! show=j_v(j_optarg0(1)).gt.j_0
	! endif !if(noptarg.le.0)then
	show=j_isopt(iob,io,j_mshow,.true.)
	call j_getoption_index(iob,io,j_mmark,-1,1,j_ipchar,.true.,noptarg,j_optarg0);if(j_err)return
	if(noptarg.le.0)then
		ismark=.false.
	else !if(noptarg.le.0)then
		call j_getchar(j_optarg0(1),mark,lemark)
		ismark=.true.
	endif !if(noptarg.le.0)  14578
	!call j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	append=j_linkoption(iob,io,j_mappend).gt.0
 
	call j_clearoption(iob,io)
	!	write(6,*)'iout,append,j_otype(iout)',iout,append,j_otype(iout)
	if(j_otype(iout).ne.j_ipfigure)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		append=.false.
		!	call j_deffig2(iout)
	else !if(j_otype(iout).ne.j_ipfigure)then
		if(.not.append)j_o(iv)%i(1)=0  !nuber of lines i(2) allocated i(3) filechaarcter
	endif !if(j_otype(iout).ne.j_ipfigure)  14589
 
	!	write(6,*)'<55iout,append,j_otype(iout)',iout,append,j_otype(iout)
	islabel=.false.
	if(narg.eq.2)then
		if(j_otype(arg(1)).eq.j_ipreal.and.j_otype(arg(2)).eq.j_ipreal)&
			islabel=.true.
	endif !if(narg.eq.2)  14599
 
	if(append)then
		if(.not.islabel)then
			i2=j_o(iout)%i2(4)
			j_o(iout)%txt(4)(i2+1:i2+9)=",'-' w lp"
			j_o(iout)%i2(4)=i2+9
			! j_o(iout)%txt(ili)(1:1)='e'
			! j_o(iout)%i2(ili)=
		endif !if(.not.islabel)  14605
		ili=j_o(iout)%i(1)
	else !if(append)then
		j_buffer=j_vname(iout)
		j_o(iout)%txt(1)='set title "'//j_buffer(1:j_lentrim(j_buffer))//'"'
		j_o(iout)%i2(1)=j_lentrim(j_o(iout)%txt(1))
 
		j_o(iout)%txt(2)='set xlabel "x-var"'
		j_o(iout)%i2(2)=18
 
		j_o(iout)%txt(3)='set ylabel "y-var"'
		j_o(iout)%i2(3)=18
 
		if(.not.islabel)then
			j_o(iout)%i2(4)=13
			j_o(iout)%txt(4)="plot '-' w lp"
			j_o(iout)%i2(4)=13
			j_o(iout)%i(1)=4
			ili=4
		else !if(.not.islabel)then
			ili=3
		endif !if(.not.islabel)  14624
	endif !if(append)  14604
	if(narg.eq.1)then
		if(j_otype(arg(1)).ne.j_ipmatrix)then
			write(6,*)'only argument must be matrix'
			j_err=.true.;return
		endif !if(j_otype(arg(1)).ne.j_ipmatrix)  14635
		if(j_o(arg(1))%i(1).ne.2)then
			write(6,*)'argument matrix must have two rows'
			j_err=.true.;return
		endif !if(j_o(arg(1))%i(1).ne.2)  14639
 
		npoints=j_o(arg(1))%i(2)
 
 
		!j_o(iout)%txt(ili)="plot '-'"
 
		do ip=1,npoints
			write(j_o(iout)%txt(ili+ip),*)j_o(arg(1))%d(ip),j_o(arg(1))%d(ip+npoints)
			j_o(iout)%i2(ili+ip)=len_trim(j_o(iout)%txt(ili+ip))
		enddo !ip=1,npoints  14649
		! ili=j_o(iout)%i(1)+1
		! j_o(iout)%txt(ili)(1:1)='e'
		! j_o(iout)%i2(ili)=1
		j_o(iout)%i(1)=j_o(iout)%i(1)+npoints+1
		ili=j_o(iout)%i(1)
		j_o(iout)%txt(ili)(1:1)='e'
		j_o(iout)%i2(ili)=1
	elseif(narg.eq.2)then !if(narg.eq.1)then
		if(j_otype(arg(1)).eq.j_ipmatrix.and.j_otype(arg(2)).eq.j_ipmatrix)then
			if(j_o(arg(1))%i(3).ne.j_o(arg(2))%i(3))then
				write(6,*)'argument matrices must have equal size'
				j_err=.true.;return
			endif !if(j_o(arg(1))%i(3).ne.j_o(arg(2))%i(3))  14662
			npoints=j_o(arg(1))%i(3)
 
			do ip=1,npoints
				write(j_o(iout)%txt(ili+ip),*)j_o(arg(1))%d(ip),j_o(arg(2))%d(ip)
				j_o(iout)%i2(ili+ip)=len_trim(j_o(iout)%txt(ili+ip))
			enddo !ip=1,npoints  14668
			j_o(iout)%i(1)=j_o(iout)%i(1)+npoints+1
			ili=j_o(iout)%i(1)
			j_o(iout)%txt(ili)(1:1)='e'
			j_o(iout)%i2(ili)=1
 
		else !if(j_otype(arg(1)).eq.j_ipmatrix.and.j_otype(arg(2)).eq.j_ipmatrix)then
			if(.not.ismark)then
				write(6,*)'with one point there msut be mark->'
				j_err=.true.;return
			endif !if(.not.ismark)  14678
			j_o(iout)%i(1)=j_o(iout)%i(1)+1
			li=j_o(iout)%i(1)
			j_o(iout)%txt(li)='set label "'//mark(1:lemark)//'" at '// &
				j_chr10(j_v(arg(1)))//','//j_chr10(j_v(arg(2)))
			j_o(iout)%i2(li)=j_lentrim(j_o(iout)%txt(li))
 
		endif !if(j_otype(arg(1)).eq.j_ipmatrix.and.j_otype(arg(2)).eq.j_  14661
	endif !if(narg.eq.1)  14634
	if(show)then
		call j_getfile(nu,rw='w',ivfile=iout,ext='.jfig',replace=.true.)
		!	write(6,*)'<99iout,append,j_otype(iout)',iout,append,j_otype(iout)
		!	if(append)write(nu,*)'set multiplot'
		if(j_err)return
		do ili=1,j_o(iout)%i(1)
			write(nu,'(a)')j_o(iout)%txt(ili)(1:j_o(iout)%i2(ili))
		enddo !ili=1,j_o(iout)%i(1)  14695
		!	write(nu,'(a)')'e'
		write(nu,'(a)')'replot'
		write(nu,'(a)')'unset title'
		write(nu,'(a)')'unset label'
		write(nu,'(a)')'unset xlabel'
		write(nu,'(a)')'unset ylabel'
		!if(append)write(nu,*)'unset multiplot'
		call j_closeunit(nu)
1000	format(a,$)

78		write(6,1000)'<ret>'
		read(5,'(a)')ch
		if(ch.ne.' ')then
			lec=len_trim(ch)
			call j_clean(ch,lec)
			if(ch(1:lec).eq.'e')then
				write(6,*)'err return from pause'
				j_err=.true.;return
			endif !if(ch(1:lec).eq.'e')  14713
 
			call j_command(ch(1:lec))
			if(j_stop)then
				call j_stopj()
				return
			endif !if(j_stop)  14719
			if(j_err)then
				j_err=.false.
				write(6,*)'try again, <ret> to continue e => ;return'
 
			endif !if(j_err)  14723
			goto 78
		endif !if(ch.ne.' ')  14710
	endif !if(show)  14690
 
 
end subroutine !subroutine drawline3(iob,io)
 
 
 
 
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
	! of class means computed as sqrt(sample_within-class_variance)/number_of_obs)
	! sd & N | 0 & & Within-calss standard deviations are drawn.
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
	! fi=drawclass(cl,color->Blue,continue->fcont)
	! fi=drawclass(cl,se->,continue->fcont)
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
 
 
	double precision::xx,yy,cumu
	logical histo,area,freq,isse,issd,isvar,iscumu
	character*2 ch
 
	call j_startfig(iob,io)
	!j_getoption_index(iob,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) !
	call j_getoption(iob,io,j_mse,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	isse=noptarg.ge.0
	call j_getoption(iob,io,j_msd,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	issd=noptarg.ge.0
	call j_getoption(iob,io,j_mvar,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	isvar=noptarg.ge.0
	call j_getoption(iob,io,j_mfreq,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	freq=noptarg.ge.0
 
	call j_getoption(iob,io,j_mhistogram,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	histo=noptarg.ge.0
 
	call j_getoption(iob,io,j_marea,-1,0,0,.false.,noptarg,j_optarg0);if(j_err)return
	area=noptarg.ge.0
	iscumu=j_isopt(iob,io,j_mcumulative)
 
	! integer itype_
	! logical getyrange,freq,histo,area
	! call j_checkoutput(iob,io)
	if(j_err)return
	call j_clearoption(iob,io)
 
	iarg=j_gparg(1)
	if(j_otype(iarg).ne.j_ipmatrix)then
		call j_printname('**drawclass: ',iarg,' not a matrix')
		j_err=.true. ;return
		!goto 900
	end if !if(j_otype(iarg).ne.j_ipmatrix)  14815
	if(j_o(iarg)%i(4).ne.j_matclass)then
		call j_printname('**drawclass: ',iarg,' not a produced by classify')
		j_err=.true. ;return
		!goto 900
	endif !if(j_o(iarg)%i(4).ne.j_matclass)  14820
	if(j_o(iarg)%i(1).eq.2)histo=.true.
	ix=j_o(iarg)%i2(1)
	call j_getname(iarg)
	!write(6,*)'<555>',j_oname(1:j_loname),ix
	if(j_gplexlabel.eq.0)then
		call j_getline(j_ivnames,ix,j_varname1,le)
		if(j_err)return
		call j_replacefig(2,'set xlabel "'//j_varname1(1:le)//'"')
	else !if(j_gplexlabel.eq.0)then
		call j_replacefig(2,'set xlabel "'//j_gpxlabel(1:j_gplexlabel)//'"')
	endif !if(j_gplexlabel.eq.0)  14829
	! j_o(j_gpiout)%txt(2)='set xlabel "'//j_varname1(1:le)//'"'
	! j_o(j_gpiout)%i2(2)=j_lentrim(j_o(j_gpiout)%txt(2))
	iy=j_o(iarg)%i2(3)
	!	write(6,*)'<iy>',iy
	if(j_gpleylabel.eq.0.and.iy.ne.0)then
		call j_getline(j_ivnames,iy,j_varname1,le)
		call j_replacefig(3,'set ylabel "'//j_varname1(1:le)//'"')
	elseif(iy.eq.0)then
		call j_replacefig(3,'set ylabel "Frequency"')
	else !if(j_gpleylabel.eq.0)then
		call j_replacefig(3,'set ylabel "'//j_gpylabel(1:j_gpleylabel)//'"')
	endif !if(j_gpleylabel.eq.0.and.iy.ne.0)  14840
 
	nrow_=j_o(iarg)%i(1)
	ncol=j_o(iarg)%i(2)
 
 
	!	j_o(j_gpiout)%i(3)=j_o(j_gpiout)%i(3)+1  gpplot does
 
 
 
	!ili=j_o(j_gpiout)%i(1)
	!i2=j_o(j_gpiout)%i2(6)
	if(j_gpappend)then
		ch=', '
	else !if(j_gpappend)then
		ch='p '
 
	endif !if(j_gpappend)  14859
	if(isse)then
		call j_gpplot(ch//"'-' w errorl"//j_gplw)
	else !if(isse)then
		call j_gpplot(ch//"'-' w l"//j_gplw)
	endif !if(isse)  14865
	if(j_gpicolor.ge.1)call j_gpplot(j_gpcolors(j_gpicolor),add=.true.)
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
				endif !if(iscumu)  14883
			else !if(area)then
				scale=100.d0/j_o(iarg)%d(2*ncol)
 
			endif !if(area)  14882
		endif !if(freq)  14878
		vlast=0
		!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin)
		! 1.00000000       0.00000000
		! 1.00000000       37.5000000
		! 1.00000000       37.5000000
		! 1.66666722       37.5000000
 
		!	call j_putor(j_fig_ivout,j_fig_iba+2,0.)
		if(iscumu)then
			!	j_fig_iba=j_fig_iba+2
			call j_putfigxy(xx,j_0)
			cumu=j_0
 
			!	write(6,*)'5445',xx-0.5d0*dd,0,xx,0,xx,scale*j_getmatel(iarg,2,i)
			do i=1,ncol-1
				cumu=cumu+j_getmatel(iarg,2,i) !scale*j_getmatel(iarg,2,i)
				!	if(i.eq.1)call j_putfigxy(xx,yy)
				call j_putfigxy(xx,scale*cumu)
				xx=xx+dd
				!				call j_putfigxy(xx,yy)
 
				!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+(i-1)*dd)
				!	call j_putor(j_fig_ivout,j_fig_iba+2,scale*real(j_o(iarg)%d(ncol+i)))
				!	j_fig_iba=j_fig_iba+2
				!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+i*dd)
				!	call j_putor(j_fig_ivout,j_fig_iba+2,scale*real(j_o(iarg)%d(ncol+i)))
				!	j_fig_iba=j_fig_iba+2
 
			enddo !i=1,ncol-1  14907
			!		call j_putfigxy(xx,j_0)
			!		call j_putfigxy(xx+0.5*dd,j_0)
 
 
		else
			!	j_fig_iba=j_fig_iba+2
			call j_putfigxy(xx-0.5d0*dd,j_0)
			call j_putfigxy(xx,j_0)
			!	write(6,*)'5445',xx-0.5d0*dd,0,xx,0,xx,scale*j_getmatel(iarg,2,i)
			do i=1,ncol-1
				yy=scale*j_getmatel(iarg,2,i)
				!	if(i.eq.1)call j_putfigxy(xx,yy)
				call j_putfigxy(xx,yy)
				xx=xx+dd
				call j_putfigxy(xx,yy)
 
				!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+(i-1)*dd)
				!	call j_putor(j_fig_ivout,j_fig_iba+2,scale*real(j_o(iarg)%d(ncol+i)))
				!	j_fig_iba=j_fig_iba+2
				!	call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+i*dd)
				!	call j_putor(j_fig_ivout,j_fig_iba+2,scale*real(j_o(iarg)%d(ncol+i)))
				!	j_fig_iba=j_fig_iba+2
 
			enddo !i=1,ncol-1  14931
			call j_putfigxy(xx,j_0)
			call j_putfigxy(xx+0.5*dd,j_0)
			!call j_putor(j_fig_ivout,j_fig_iba+1,j_fig_xmin+(ncol-1)*dd)
			!call j_putor(j_fig_ivout,j_fig_iba+2,0.)
			!j_fig_iba=j_fig_iba+2
			!j_fig_np=2*ncol
			!call endfig(2)
			!	return
		endif !if(iscumu)  14901
	else !if(histo)then
		! if(getyrange)then
		! j_fig_ymin0=1.7e37;j_fig_ymax0=-1.7e37
		! end if !if(getyrange)then
		if(j_err)return
 
 
		! j_o(j_gpiout)%txt(6)(i2+1:)="'-' w l"//j_gplw
		! j_o(j_gpiout)%i2(6)=j_lentrim(j_o(j_gpiout)%txt(6))
		! ili=6
		!row 1 classmeans of x, total mean of x
		!row 2 frequencies
		!row 3 means of y
		!row 4 sd:s
		do ip=1,ncol-1
			!	write(6,*)'377373ip',ip
			fre=j_getmatel(iarg,2,ip)
			if(fre.le.0)cycle
			xx=    j_getmatel(iarg,1,ip)
			!		call j_putor(j_fig_ivout,j_fig_iba+1,xx)
			if(issd)then
				yy=j_getmatel(iarg,4,ip)
			elseif(isvar)then !if(issd)then
				yy=j_getmatel(iarg,4,ip)**2
			else !if(issd)then
				yy=j_getmatel(iarg,3,ip)
			endif !if(issd)  14975
			!		call j_putor(j_fig_ivout,j_fig_iba+2,yy)
			if(isse)then
				call j_putfigxy(xx,yy,se=j_getmatel(iarg,4,ip)/sqrt(j_getmatel(iarg,2,ip)))
			else !if(isse)then
				call j_putfigxy(xx,yy)
			endif !if(isse)  14983
		end do !ip=1,ncol-1  14969
	end if !if(histo)  14873
	!write(6,*)'377373'
	call j_putfig('e')
 
	!	if(j_gpshow)call j_showfig(j_gpiout)
 
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
	! @@figu
	! mark & N | 1 & REAL | CHAR & The mark used in the plot. Numeric values refer to
	! mark types of Gnuplot. The mark can be given also as CHAR varible or constant.
	! func& N | 1 & Code &  Code option telling how the y-variable is computed.
	! endoption
	! Ex plotyxex plotyx()
	! xmat=matrix(do->(0,10,0.001))
	! transa=trans()
	! y=2+3*x+0.4*x*x+4*rann()
	! /
	! datyx=newdata(xmat,read->x,maketrans->transa,extra->(Regf,Resid))
	! fi=plotyx(y,x,continue->fcont)
	! fi=plotyx(x,func->transa(y),mark->3,color->Orange,continue->fcont)
	! reg=regr(y,x)
	! figyx=plotyx(y,x,show->0)
	! fi=plotyx(Regf,x,append->,continue->fcont)
	! fir=plotyx(Resid,x,continue->fcont)
	! endex
	! Note With data with integer values, the default ranges of Gnuplot may be hide point at
	! borderlines.
	! endnote
	!Note ]fi[=plotyx() produces or updates file ]fi[.jfig] which contains
	! Gnuplot commands and file ]fi[.jfi0 containg data.
	!endnote
	! endsection
 
 
 
	logical::isfunc
	double precision::yvalue
	character*3 ch
	character*2 ch0
	real:: yvalue0
	integer*8 ::i
	call j_startfig(iob,io)
	!write(6,*)'<77',j_o(j_gpiout)%i(1)
	!	call j_checkoutput(iob,io)
	if(j_err)return
	linkfunc=j_codelink(iob,io,j_mfunc)
	isfunc=linkfunc.ne.0
	!narg=j_o(iob)%i(io+1)
	if(isfunc)then
		if(j_gpnarg.ne.1)then
			write(6,*)'with func-> there can be only x-variable'
			j_err=.true.;return
		endif !if(j_gpnarg.ne.1)  15053
		!	j_fig_ixx=j_o(iob)%i(io+2)
		!ixy=j_o(iob)%i(io+2)
		ix=j_gparg(1)
	else !if(isfunc)then
		if(j_gpnarg.ne.2)then
			write(6,*)'without func-> there must be two arguments, y and x'
			j_err=.true.;return
		endif !if(j_gpnarg.ne.2)  15061
		!		j_fig_ixx=j_o(iob)%i(io+3)
		!		ixy=j_o(iob)%i(io+2)
		ix=j_gparg(2)
		iy=j_gparg(1)
	endif !if(isfunc)  15052
	!	j_fig_istyle=0  !defaulst
	!	j_fig_tex='.';j_fig_lex=1   !default for
	!	call startfig(iob,io)
	if(j_err)return
	call j_getdataobject(iob,io)
	!call j_getdatasets(iob)  !using opt
	call j_clearoption(iob,io)  ! subroutine
 
	if(j_gplexlabel.eq.0)then
 
		call j_getline(j_ivnames,ix,j_varname1,le)
		call j_replacefig(2,'set xlabel "'//j_varname1(1:le)//'"')
		!else
		!	call j_replacefig(2,'set xlabel "'//j_gpxlabel(1:j_gplexlabel)//'"')
	endif !if(j_gplexlabel.eq.0)  15078
 
	if(j_gpleylabel.eq.0.and..not.isfunc)then
		call j_getline(j_ivnames,iy,j_varname1,le)
		call j_replacefig(3,'set ylabel "'//j_varname1(1:le)//'"')
	else !if(j_gpleylabel.eq.0)then
		call j_replacefig(3,'set ylabel "'//j_gpylabel(1:j_gpleylabel)//'"')
	endif !if(j_gpleylabel.eq.0.and..not.isfunc)  15086
	!	write(6,*)'<888',j_o(j_gpiout)%i(1)
	if(j_gpappend)then
		ch0=', '
		! j_o(j_gpiout)%txt(6)(i2+1:i2+1)=','
		! i2=i2+1
	else !if(j_gpappend)then
		ch0='p '
 
	endif !if(j_gpappend)  15093
	! j_o(j_gpiout)%i(1)=0  !number of lines
	! j_o(j_gpiout)%i(2)=j_gplines !number of allocated lines
	! j_o(j_gpiout)%i(3)=0  !number plot commands
	! j_o(j_gpiout)%i(4)=j_gpplots  !number of availablr plotcommands
	!! j_o(j_gpiout)%i(5)=number of sets !number of availablr plotcommands
	if(j_o(j_gpiout)%i(5).eq.0)then
		call j_getfile(nu,rw='w',ivout=nuf,ivfile=j_gpiout,ext='.jfi0',replace=.true.)
	else !if(j_o(j_gpiout)%i(5).eq.0)then
		call j_getfile(nu,rw='a',ivout=nuf,ivfile=j_gpiout,ext='.jfi0')
		write(nu,*)' '
		write(nu,*)' '
	endif !if(j_o(j_gpiout)%i(5).eq.0)  15106
	if(j_err)return
	j_o(j_gpiout)%i(5)=j_o(j_gpiout)%i(5)+1
 
	!ch=char(47+j_o(j_gpiout)%i(5))
	write(ch,'(i3)')j_o(j_gpiout)%i(5)-1
	iaa=1
	if(ch(1:1).eq.' ')iaa=2
	if(ch(2:2).eq.' ')iaa=3
	!	call j_getfile(nu,rw='w',ivout=nuf,ivfile=j_gpiout,ext='.jfi0',replace=.true.)
	!	write(6,*)'<8484',nuf
	call j_getline(j_ivnames,nuf,j_filename,le) !note this takes ' ' into filename
	if(j_err)return
	call j_gpplot(ch0//j_filename(1:le)//' i '//ch(iaa:3)//' w p'//j_gppt//j_gpps)  !i=index
	if(j_gpicolor.ge.1)call j_gpplot(j_gpcolors(j_gpicolor),add=.true.)
	ntot=0
	!	do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
	do i=j_dfrom,j_duntil
		call j_getobs(i); if(j_err)goto 900
		!	write(6,*)'i',i,j_v(j_gparg(1:2))
		if(j_rejected)cycle
 
		ntot=ntot+1
		! j_fig_xmin=min(j_fig_xmin,j_v(j_fig_ixx))
		! j_fig_xmax=max(j_fig_xmax,j_v(j_fig_ixx))
		if(isfunc)then
			yvalue=j_codevalue(iob,linkfunc)
 
		else !if(isfunc)then
			yvalue=j_v(j_gparg(1))
		endif !if(isfunc)  15138
		if(abs(j_v(ix)).ge.1.7d19.or.abs(yvalue).ge.1.7d19)cycle
		!	yvalue0=yvalue
		!j_fig_ymin=min(j_fig_ymin,yvalue0)
		!j_fig_ymax=max(j_fig_ymax,yvalue0)
		call j_putfigxy(j_v(ix),yvalue,nu=nu)
		! write(nu,*)real(j_v(j_gparg(2))),yvalue0
		! j_o(j_gpiout)%d(7)=min(j_o(j_gpiout)%d(7),j_v(j_gparg(2)))
		! j_o(j_gpiout)%d(8)=max(j_o(j_gpiout)%d(8),j_v(j_gparg(2)))
		! j_o(j_gpiout)%d(9)=min(j_o(j_gpiout)%d(9),yvalue)
		! j_o(j_gpiout)%d(10)=max(j_o(j_gpiout)%d(10),yvalue)
 
	end do !i=j_dfrom,j_duntil  15130
	call j_closeunit(nu)
	!	end do !do k=1,jndatasetss
	!	j_fig_np=ntot
	j_v(j_ivaccepted)=ntot
	!	if(j_gpshow)call j_showfig(j_gpiout)
	900 continue !  if(j_err)return
!	io=io+j_fig_narg+3
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
	write(6,*)' '
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
 
		ivmat=j_o(iv)%i(1)
		j_v(iout)=j_o(ivmat)%i(1)
 
 
	end if !if(j_otype(iv).eq.j_ipregr)  15180
	return
end subroutine nobs !subroutine nobs(iob,io)
 
 
 
 
 
subroutine joindata(iob,io) !
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
	!to the first argument DATA (cdata). There can be two options:\\
	!nrowtot-> gives the total number of observations when joindata is used several times to put several DATAs into
	! the same DATA.\\
	!append-> current data are appended to a DATA for which space has been reserved with nrowtot->
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
 
	! linkdata(data->cdata,subdata->xdata)
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
	! ** use nrowtot-> and append-> to put more data into the same DATA
	!** here the same DATA is put twice
	! ** the number of observations in the tree format data is the same as
	!**the number of observations in the data for the last period.
	! nobstot=2*nobs(xdataP#5);
	! Xdatanew=joindata(cdata,xdataP#1...xdataP#5,nrowtot->nobstot)
	! Xdatanew=joindata(cdata,xdataP#1...xdataP#5,append->)
	! stat()
	!** remove the created file
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
	isappend=j_isopt(iob,io,j_mappend,.false.)
	if(ndata.lt.2)then
		write(6,*)'joindata needs at least two DATA arguments'
		j_err=.true. ;return
	endif !if(ndata.lt.2)  15337
	call j_getoption(iob,io,j_mnrowtot,-1,1,j_ipreal,.true.,nn,j_optarg0)
	if(j_err)return
 
	if(nn.gt.0)then
		nrowtot=j_v(j_optarg0(1))
 
		isnrowtot=.true.
	else
		isnrowtot=.false.
	endif !if(nn.gt.0)  15344
 
	istwo=ndata.eq.2
	! call j_getoption(iob,io,j_mdata,2,999,j_ipdata,.true.,ndata,datat)
	! if(j_err)return
	allocate(nkeep(1:ndata),ivkeep(1:ndata),nobsw(1:ndata),obsw(1:ndata))
	allocate(imat(1:ndata),iba(1:ndata),rowba(1:ndata),nkeep2(1:ndata))
	iba=0
 
	rowba(1)=0
	ivmat=j_o(datat(ndata))%i(1)
	nobstot=j_o(ivmat)%i(1)
	if(isnrowtot.and.nrowtot.le.nobstot)then
		write(6,*)'illegal nrowtot->',nrowtot,' as current data has ',nobstot,' observations'
		j_err=.true.;return
		j_err=.true.;return
 
	endif !if(isnrowtot.and.nrowtot.le.nobstot)  15362
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
		endif !if(ipe.ne.1.and..not.istwo)  15379
		imat(i)=j_o(datat(i))%i(1)
		ivkeep(i)=j_o(datat(i))%i(2)
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
 
		endif !if(i.eq.2)  15386
 
 
		nkeep2(i)=nkeep(i)-1
		if(i.gt.1)then
			if(istwo)then
				nkeeptot=nkeeptot+nkeep(i)
			else
				nkeeptot=nkeeptot+nkeep2(i)
			endif !if(istwo)  15402
		endif !if(i.gt.1)  15401
		!	write(6,*)'i,nkeep,nkeep2,nkeeptot ',i,nkeep(i),nkeep2(i),nkeeptot
	enddo !i=1,ndata  15373
	!	write(6,*)'nkeep',nkeep
	!	write(6,*)'nkeep2 ',nkeep2
	!write(6,*)'nkeeptot',nkeeptot
 
	!stop
 
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
		enddo !i=2,ndata  15431
	endif !if(istwo)  15423
	!	write(6,*)'newkeep',newkeep
	!	write(6,*)'rowba ',rowba
	nkeepto=nkeeptot
	call  j_deflistobject(ivout,'%keep',ivkeeptot,list0=nkeepto,list=newkeep)
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
		endif !if(ivmatrix.le.0)  15446
		ibas2=j_nelem(ivmatrix)
	else
		if(isnrowtot)then
			ivmatrix=j_defmatrix8(ivout,'%matrix',nobstot,nkeeptot,j_matreg,rowtot=nrowtot)
		else
			ivmatrix=j_defmatrix8(ivout,'%matrix',nobstot,nkeeptot,j_matreg)
		endif !if(isnrowtot)  15455
		ibas2=0
	endif !if(isappend)  15443
 
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
			!	if(p)write(6,*)'iobs,level0 init',i,level0,obsw(level0),nobsw(level0)
 
700		if(obsw(level0).eq.nobsw(level0))then
		!		if(p)write(6,*)'level0',level0,obsw(level0),nobsw(level0)
				obsw(level0)=0
 
 
				nobsw(level0)=j_o(imat(level0-1))%d(iba(level0-1)+ 1)
				!	call j_getname(j_dimat2(level0-1))
				!	if(p)write(6,*)'level0',level0,nobsw(level0),imat(level0-1),j_oname(1:j_loname)
				!		write(6,*)'level0here dnobs',level0, j_dnobsw2(level0),' from ',j_diba2(level0-1)+ j_dnobswindex(level0-1)
				level0=level0-1
				!	if(p)write(6,*)'uus level0 ,nobsw(level0)',level0,nobsw(level0)
				!	if(level0.gt.0)goto 800
 
				!		write(6,*)'here ',level0,j_dnobsw2(level0)
				if(level0.gt.1)goto 700
			endif !700		if(obsw(level0).eq.nobsw(level0))  15483
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
				endif !if(ile.eq.2)  15512
				iba(ile)=iba(ile)+nkeep(ile)
			enddo !ile=level0,ndata  15511
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
		enddo !i=1,nobstot  15477
 
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
			enddo !j=1,nobo  15544
			ibas=ibas+nkeep(1)
		enddo !i=1,nobsup  15541
 
 
 
 
	endif !if(.not.istwo)  15475
 
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
	else
		call j_defdata(ivout,ivmatrix,ivkeeptot)
 
	endif !if(isappend)  15566
 
 
 
	!	call j_getname(imat(1))
	!	write(6,*)j_oname(1:j_loname),nkeep(1),j_o(imat(1))%i(1:3),nkeep(1)*j_o(imat(1))%i(1),&
	!		nobstot
	!write(6,*)'nobswup ',nobswup(1:10)
	if(.not.istwo)then
		do i=1,nobstop
			j_o(imat(1))%d((i-1)*nkeep(1)+1)=nobswup(i)
		enddo !i=1,nobstop  15595
 
		call j_linkdata(datat(1),ivout)
	endif !if(.not.istwo)  15594
 
	deallocate(newkeep,nkeep,ivkeep,nobsw,obsw,imat)
	deallocate(iba,rowba,nkeep2,nobswup,j_tempvector)
	! if(level0.lt.0)level0=0
	! !going down
	j_dlastdata=ivout
	call j_clearoption(iob,io)  ! subroutine
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
	ivkeep=j_o(j_arg(1))%i(2)
	ivmatrix=j_o(j_arg(1))%i(1)
	nobs=j_o(ivmatrix)%i(1)
	call j_getname(j_arg(1))
	ivup=j_o(j_arg(1))%i(5)
	call j_getname(j_arg(1))
	if(ivup.le.0)then
 
		write(6,*)j_oname(1:j_loname),' does not have upper level data'
		j_err=.true.;return
	endif !if(ivup.le.0)  15659
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
 
	endif !if(nobswlink.le.0)  15685
	ivmatrixup=j_o(ivup)%i(1)
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
			endif !if(ikeep(i,j).le.0)  15715
		enddo !j=1,nkeepper(i)  15712
		!write(6,*)'period,keep',i,ikeep(i,1:nkeepper(i))
		nvar=nvar+nkeepper(i)
	enddo !i=1,nperiod  15709
 
 
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
				enddo !j=1,nkeepper(i)  15734
 
			enddo !i=1,nperiod  15733
			if(lkm.ne.1)then
				call j_getname(iv)
				write(6,*)j_oname(1:j_loname),' appears ',lkm, ' times in periodvars'
 
			endif !if(lkm.ne.1)  15740
		enddo !k=1,nkeep  15730
		j_err=.true.
	endif !if(nvar.ne.nkeep)  15728
 
 
	if(j_err)then
		deallocate(ikeep)
		return
	endif !if(j_err)  15750
	allocate(iperiod(1:nobs),nobsp(1:nperiod),nobsp2(1:nperiod),ivmat(1:nperiod),ivdata(1:nperiod))
	!allocate(nobswp(1:nobsup,1:nperiod))
 
	nobsp=0
	ibas=0
	ibas0=0
	nobswcur=0
	ibasup=0
	iobs1=1
	iobs=0
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
		iobs=iobs+1
		!	write(6,*)'iunit,nobsw,iobs ',iunit,nobsw,iobs
		!	if(nobsw.eq.1)then
		nobsp=nobsp+1
		iperiod(iobs)=1
		!	nobsp(1:nperiod)=nobsp(1:nperiod)+1
		!	nobswp(iunit,1:nperiod)=nobswp(iunit,1:nperiod)+1
		ibas0=ibas
		ibas=ibas+nkeep
		ntot=ntot+1
		!	else
		!	write(6,*)'hep'
iobloop:		do iobsw=2,nobsw
			iobs=iobs+1
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
					!	if(iobs.le.200)
					!			if(iobs.le.200)write(6,*)iunit,iobsw,j,j_o(ivmatrix)%d(ibas+ikeep(i,j)),j_o(ivmatrix)%d(ibas0+ikeep(i,j))
					if(j_o(ivmatrix)%d(ibas+ikeep(i,j)).ne.j_o(ivmatrix)%d(ibas0+ikeep(i,j))) then
						!	iperiod(iobs)=i
						nobsp(i:nperiod)=nobsp(i:nperiod)+1
						!		nobswp(iunit,i)=nobswp(iunit,i)+1
						iperiod(iobs)=i
 
						!		write(6,*)'eka ',j_o(ivmatrix)%d(ibas+ikeep(nvar))
						!	write(6,*)'toka',j_o(ivmatrix)%d(ibas0+ikeep(nvar))
						goto 567
					endif !if(j_o(ivmatrix)%d(ibas+ikeep(i,j)).ne.j_o(ivmatrix)%d(iba  15808
				enddo !j=1,nkeepper(i)  15803
 
			enddo ploop !op:	do i=1,nperiod  15800
 
			ndupl=ndupl+1
			iperiod(iobs)=nperiod
			nobsp(nperiod)=nobsp(nperiod)+1
			!if(iobs.le.10)write(6,*)iunit,iobsw,iperiod(iobs)
			!	nobswp(iunit,i:nperiod)=nobswp(iunit,i:nperiod)+1
567			ibas0=ibas
			ibas=ibas+nkeep
			!		write(6,*)'ibas,nkeep ',ibas,nkeep
		enddo iobloop !loop:		do iobsw=2,nobsw  15791
		!	endif !if(nobsw.eq.1)  15415
	enddo !iunit=1,nobsup  15773
 
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
	!write(6,*)'nam ',j_oname(1:j_loname)
	ivdatav=ivup
	do i=1,nperiod
		call j_getobject(0,j_oname(1:j_loname)//'#'//char(48+i),j_ipdata,ivdata(i))
		call j_getname(-1,ivdata(i))
		!write(6,*)'nam2 ',j_oname2(1:j_loname2),nobsp(i),nkeepper(i)+1
		ivmat(i)=j_defmatrix8(0,j_oname2(1:j_loname2)//'%matrix',nobsp(i),nkeepper(i)+j_18,j_matreg)
		if(j_err)return
		!write(6,*)'ivmat(i)',ivmat(i)
		call j_deflistobject(0,j_oname2(1:j_loname2)//'%keep',ivkee,ivin=j_optarg(i),nres=1)
 
		call j_getobject(0,j_oname2(1:j_loname2)//'%nobsw',j_ipreal,ivnobsw)
		iper=j_putlistobject(ivkee,single=ivnobsw,append=.false.)
		!	call j_getobject(0,j_oname2(1:j_loname2)//'%obs',j_ipreal,ivnobs) defdata takes care of this
 
 
		call j_deflistobject(0,j_oname(1:j_loname)//'%vars',ivvars,ivin=ivkee)
		call j_deflistobject(0,j_oname(1:j_loname)//'%vars2',ivvars2,ivin=ivkee)
 
		call j_defdata(ivdata(i),ivmat(i),ivkee)
 
 
		if(j_err)return
		!	j_o(ivup)%i(3)=ivdata
		!	ivdatav=ivdata
	enddo !i=1,nperiod  15854
 
	allocate(ibasper(1:nperiod),nobswpos(1:nperiod))
 
 
 
	ibas=0
	ibasup=0
	ibasper=0
	iobs=0
	p=.false.
	do iunit=1,nobsup
		nobsw=j_o(ivmatrixup)%d(ibasup+nobswlink)
		j_o(ivmatrixup)%d(ibasup+nobswlink)=0
		!	j_o(ivmatrixup)%d(ibasup+nobswlink)=nobswp(iunit,1)
 
		!write(6,*)'iunit ', iunit,nobsw
 
 
 
		do j=1,nobsw
			iobs=iobs+1
 
			!	write(17,*)'iobs,p',iobs,p
			!	if(p)write(6,*)'iunit,j,iobs,iperiod(iobs)',iunit,j,iobs,iperiod(iobs)
			!write(17,*)'iunit,j,iobs,iperiod(iobs)',iunit,j,iobs,iperiod(iobs)
			do ip=iperiod(iobs),nperiod
				!	if(iobs.le.100)	write(6,*)iunit,j,ip,iperiod(iobs)ibasper(ip),ibas
				j_o(ivmat(ip))%d(ibasper(ip)+2:ibasper(ip)+1+nkeepper(ip))=&
					j_o(ivmatrix)%d(ibas+ikeep(ip,1:nkeepper(ip)))
				nobswpos(ip)=ibasper(ip)+1
				nobsp2(ip)=nobsp2(ip)+1
				!	write(17,*)'iunit,j,iobs,iperiod(iobs)',iunit,j,iobs,iperiod(iobs)
				!if(p)write(6,*)j_o(ivmat(ip))%d(ibasper(ip)+2:ibasper(ip)+1+nkeepper(ip))
				!	write(17,*)j_o(ivmat(ip))%d(ibasper(ip)+2:ibasper(ip)+1+nkeepper(ip))
				if(ip.gt.1)then
					j_o(ivmat(ip-1))%d(nobswpos(ip-1))=j_o(ivmat(ip-1))%d(nobswpos(ip-1))+1
				else
					j_o(ivmatrixup)%d(ibasup+nobswlink)=j_o(ivmatrixup)%d(ibasup+nobswlink)+1
				endif !if(ip.gt.1)  15912
				if(p)write(6,*)'ip,nobswpos(ip) ',ip,nobswpos(ip)
				!		if(ip.lt.nperiod)j_o(ivmat(ip))%d(ibasper(ip)+1)=nobswp(iunit,ip+1)
 
				!		if(iobs.le.10)	write(6,'(5i5,8f8.2)')iunit,j,ip,ibasper(ip),ibas,&
				!	j_o(ivmat(ip))%d(ibasper(ip)+2:ibasper(ip)+1+nkeepper(ip))
 
				ibasper(ip)=ibasper(ip)+1+nkeepper(ip)
			end do !ip=iperiod(iobs),nperiod  15903
			ibas=ibas+nkeep
		enddo !j=1,nobsw  15897
		ibasup=ibasup+nkeepup
	enddo !iunit=1,nobsup  15888
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
 
subroutine linkdata(iob,io) !linkdata()
 
	! Section linkdata linkdata() Links or combines hierarchical DATAs
	! linkdata(data->,subdata->)
	! links hierarchical data sets. Currently linkdata can create also one flat file which can be used in jlp()
	! endheader
	! Option
	! data&1&DATA& the upper level data set object
	! subdata &1&DATA& the lower data set object
	!endoption
	!Note In versions before 20.12.2022 nobsw-> obtion was needed to tell what is the number
	! of subdata-> -observations under the current upper level observation. When I made the data format for storing
	! schedules having the tree structure efficiently, everything became too complicated when allowing any
	! freedom with respect to the nobsw-variable. Thus the nobsw variable in DATA datab has
	!name datab%nosw and it must be the first variable among the keep-variables of the DATA.
	!endnote
	! Note  In most cases links between data sets can be either made using sub-options of data()
	! function or linkdata() function. If there is need to duplicate lower level observations, then
	! this can be currently made only in data() function. Also when the data for both the upper
	! level and lower level data are read from the same file, then data() function must be used.
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
	! linkdata(data->dataa,subdata->datab)
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
	! datab=linkdata(data->dataa,subdata->datab)
	! stat()
	! datab%matrix;
	! endsection
 
	!	call j_checkoutput(iob,io)
	!	call j_startfunction(iob,io,0,narg,j_arg,ivout)
 
	call j_getoption(iob,io,j_mdata,1,1,j_ipdata,.true.,noptarg,j_optarg0)
	if(j_err)return
	ivup=j_optarg0(1)
	call j_getoption(iob,io,j_msubdata,1,1,j_ipdata,.true.,noptarg,j_optarg0)
	ivsub=j_optarg0(1)
	if(j_err)return
	call j_linkdata(ivup,ivsub)
	return
	! if(j_nargopt(iob,io,j_mclasslink).gt.0)then
	! j_err=.true.
	! write(6,*)'**linkdata: classlink does not work yet'
	! goto 90
	! end if !if(j_nargopt(iob,io,j_mclasslink).gt.0)  14731
 
 
 
	!		ivvarsup=ivkeepup
	!	elsei(7
 
	! keepup=>j_o(ivvarsup)%i2(1:nkeepup)
	! nkeepup=j_o(ivvarsup)%i(1)
	! !	endif !if(j_o(ivup)%i(12).eq.0)  15469
 
 
	! !	call j_getname(ivsub,ivsubkeep)
	! !	write(6,*)j_oname(1:j_loname),' ',j_oname2(1:j_loname2)
	! !	write(6,*)'nvar,nvartot,nsubkeep ',nvar,nvartot,nsubkeep
	! nvar=0
	! do i=1,nkeepup
	! if(j_inlistobject(keepup(i),ivkeepsub).le.0)nvar=nvar+1
	! enddo !i=1,nkeepup  15483
	! nvartot=nkeepsub+nvar !Unit+obsw
	! allocate(newkeep(1:nvartot))
	! allocate(oldkeep(1:nvar),oldkeepiv(1:nvar))
	! nvar=0
	! do i=1,nkeepup
	! if(j_inlistobject(keepup(i),ivkeepsub).le.0)then
	! nvar=nvar+1
	! oldkeep(nvar)=i
	! oldkeepiv(nvar)=keepup(i)
	! endif !if(j_inlistobject(keepup(i),ivkeepsub).le.0)  15491
	! enddo !i=1,nkeepup  15490
	! newkeep(1:nkeepsub)=j_o(ivkeepsub)%i2(1:nkeepsub)
	! newkeep(nkeepsub+1:nkeepsub+nvar)=oldkeepiv
	! !newkeep(nkeepsub+nvar+1)=ivobsup
	! !newkeep(nkeepsub+nvar+2)=ivobsw
	! newk=nkeepsub+nvar
 
 
 
 
 
 
	! call j_getobject(ivobsup,'%obs',j_ipreal,ivobsup)
	! if(ivobsup.eq.j_ivobs)then
	! call j_getname(ivup)
	! write(6,*)'The obs-variable in ',j_oname(1:j_loname),' was Obs, it is changed into Unit'
	! call j_getobject(0,'Unit',j_ipreal,ivobsup)
 
	! call j_getobject(ivobsup,'%obsw',j_ipreal,ivobsw)
 
 
	! j_o(ivup)%i(6)=ivobsup
	! endif !if(ivobsup.eq.j_ivobs)  15119
 
 
 
 
	!deallocate(newkeep,oldkeep,oldkeepiv)
 
 
	!io=io+j_o(iob)%i(io+1)+3
	call j_clearoption(iob,io)  ! subroutine
	return
end subroutine !subroutine linkdata2iob,io)
 
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
	endif !if(narg.eq.1)  16133
	ivlist=j_o(iob)%i(io+3) !second arg
	select case(j_otype(ivlist))
	case(j_iplist) !select case(j_otype(ivlist))
	inde=j_inlistobject(j_o(iob)%i(io+2),ivlist)
 
	j_v(iout)=inde
	case (j_ipmatrix) !select case(j_otype(ivlist))
	valu=j_v(iarg)
	if(j_linkoption(iob,io,j_many).gt.0)then
		call j_clearoption(iob,io)  ! subroutine
		do j=1,j_o(ivlist)%i(3)
			if(j_o(ivlist)%d(j).ge.valu)then
				j_v(iout)=j
				return
			endif !if(j_o(ivlist)%d(j).ge.valu)  16148
		enddo !j=1,j_o(ivlist)%i(3)  16147
		j_v(iout)=j_0
	else !if(j_linkoption(iob,io,j_many).gt.0)then
		do j=1,j_o(ivlist)%i(3)
			if(valu.eq.j_o(ivlist)%d(j))then
				j_v(iout)=j
				return
			endif !if(valu.eq.j_o(ivlist)%d(j))  16156
		enddo !j=1,j_o(ivlist)%i(3)  16155
		j_v(iout)=j_0
	endif !if(j_linkoption(iob,io,j_many).gt.0)  16145
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
	endif !if(.not.j_remain)  16205
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	lindef=j_linkoption(iob,io,j_mdefault)
	ivexit=j_igetopt(iob,io,j_mexit)
	if(ivexit.gt.0)then
		if(j_otype(ivexit).ne.j_ipchar)then
			write(6,*)'**askc, end-> does not refer to character'
			j_err=.true.
			return
		endif !if(j_otype(ivexit).ne.j_ipchar)  16215
		call j_getchar(ivexit,exitc,lex)
	endif !if(ivexit.gt.0)  16214
 
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
 
				endif !if(j_o(iv)%i(3).eq.0)  16229
 
			endif !if(j_otype(iv).eq.j_ipchar)  16228
		end if !if(narg.le.0)  16224
		if(j_linkoption(iob,io,j_mq).le.0.or.j_ipc(j_o(iob)%i(j_linkoption(iob,io,j_mq)+1)).eq.0)then
			call j_getline(j_ivnames,iv,name(21:),le)
			if(j_err)return
			name(1:20)='give text value for ';le=le+20
		else !if(j_linkoption(iob,io,j_mq).le.0.or.j_ipc(j_o(iob)%i(j_linkoption(iob,io,j_mq)+1)).eq.0)then
			call j_getchar(j_o(iob)%i(j_linkoption(iob,io,j_mq)+1),name,le)
		end if !if(j_linkoption(iob,io,j_mq).le.0.or.j_ipc(j_o(iob)%i(j_li  16238
		lef=0
		if(lindef.gt.0)then
			ide=j_o(iob)%i(lindef+i)
			if(j_otype(ide).eq.j_ipchar)then
				call j_getchar(ide,def,lef)
			else !if(j_otype(ide).eq.j_ipchar)then
				write(6,*)'** illegal default in askc';j_err=.true.
			end if !if(j_otype(ide).eq.j_ipchar)  16248
		end if !if(lindef.gt.0)  16246
		if(lef.gt.0)then
			if(ivexit.gt.0)then
				write(6,'(1x,a,$)')name(1:le)//'  (default='//def(1:lef)//') >'
			else !if(ivexit.gt.0)then
				write(6,'(1x,a,$)')name(1:le)//'  (default='//def(1:lef)//',exit='//exitc(1:lex)//') >'
			endif !if(ivexit.gt.0)  16255
		else !if(lef.gt.0)then
			if(ivexit.le.0)then
				write(6,'(1x,a,$)')name(1:le)//' >'
			else !if(ivexit.le.0)then
				write(6,'(1x,a,$)')name(1:le)//'(exit='//exitc(1:lex)//') >'
			endif !if(ivexit.le.0)  16261
		end if !if(lef.gt.0)  16254
		read(5,'(a)')line_(2:) ; nc=j_lentrim(line_) !read(5,'(q,a)')nc,line_(2:)
		if(line_(2:).eq.' '.or.nc.le.0)then
			if(lef.gt.0)then
				ivchar=ide
			else !if(lef.gt.0)then
				call j_printname('*w* character variable',iv,' not created/modified');goto 900
			end if !if(lef.gt.0)  16269
		else !if(line_(2:).eq.' '.or.nc.le.0)then
			if(line_(2:2).eq."'")then
				nc1=2
			else !if(line_(2:2).eq."'")then
				line_(1:1)="'"
				nc1=1;nc=nc+1;line_(nc:nc)="'"
			end if !if(line_(2:2).eq."'")  16275
			if(ivexit.gt.0)then
				if(line_(nc1+1:nc-1).eq.exitc(1:lex))then
					write(6,*)'**exit from askc due to exit->'
					j_err=.true.
					return
				endif !if(line_(nc1+1:nc-1).eq.exitc(1:lex))  16282
			endif !if(ivexit.gt.0)  16281
			call j_defchar(0,line_(nc1:nc),ivchar)
			if(j_err) return
		end if !if(line_(2:).eq.' '.or.nc.le.0)  16268
		call j_asschar2(ivchar,iv)
	end do !i=1,max(narg,1)  16223
	900 if(j_err)return
	call j_clearoption(iob,io)  ! subroutine
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
	call j_startfunction(iob,io,0,narg0,arg,ivout)
	if(narg0.eq.0)then
		narg=1
		arg=>j_o(iob)%i(io+2:io+2)
	else
		narg=narg0
	endif !if(narg0.eq.0)  16334
	call j_getoption(iob,io,j_mdefault,-1,narg,j_ipreal,.true.,noptarg,j_optarg0);if(j_err)return
	if(noptarg.gt.0.and.noptarg.ne.narg)then
		write(6,*)'default-> should have as many arguments as the function has'
		j_err=.true. ;return
 
	endif !if(noptarg.gt.0.and.noptarg.ne.narg)  16341
 
 
	!	io=io_
	!	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	!j_getoption_index(iob,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
 
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
	endif !if(noptarg.gt.0)  16359
	if(nq.gt.0)then
		call j_getchar(argq(1),j_asktext,leb)
		write(6,100)j_asktext(1:leb)
	else !if(nq.gt.0)then
100 format(a,$)
		write(6,100)'give values for:'
		call j_printlist0(6,narg,arg)
	endif !if(nq.gt.0)  16364
 
 
	call j_clearoption(iob,io)  ! subroutine
	goto 179
178		write(6,100)'ask>'
179	read(5,'(a)')j_tempchar2
	le=j_lentrim(j_tempchar2)
	if(le.le.0)return
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
	call j_getoption_index(iob,io,j_mchi2,-1,0,j_ipreal,.false.,nchi2,j_optarg0)
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
			endif !if(idf.le.0)  16469
			if(arg.lt.0)then
				write(6,*)'*cdf(arg,df,chi2->): arg is negative',arg
				j_err=.true.
				return
 
			endif !if(arg.lt.0)  16474
			arg2=idf
		endif !if(narg.lt.2)  16463
 
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
 
 
	endif !if(nchi2.ge.0)  16461
	!	90	io=io+narg+3
	return
end subroutine cdf !subroutine cdf(iob,io)
 
subroutine close_(iob,io)  ! close()
	! Section close close() Closes a file
	! close(file) closes an open file where file is either a character constant
	! or character variable associated with a file.
	! endheader
	! Note No open(9)function is needed. An file is opened when it is first time in write() or
	!print(file->).
	! endnote
	! endsection
	logical ope
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	do i=1,narg
		iv=j_o(iob)%i(io+1+i)
		nu=j_iounit(iv)
		!	write(6,*)'<554 iv',iv
		if(j_err)return
		if(nu.le.0)then
			call j_printname('close: argument ',iv,' not open file')
			! j_err=.true.
			return
		endif !if(nu.le.0)  16521
		!		write(6,*)'<9'
		call j_closeunit(nu)
		if(j_err)return
	end do !i=1,narg  16516
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
	logical wait
	integer*1 ,dimension(:), allocatable::bytes
	save le,lef
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	iv2=j_o(iob)%i(io+2)
	ivform=j_o(iob)%i(io+3)
	ivarg3=j_o(iob)%i(io+4)
	nu=j_iounit(iv2)
	wait=j_linkoption(iob,io,j_mwait).ge.0
	call j_getoption(iob,io,j_meof,-1,1,j_ipreal,.true.,neof,eof);if(j_err)return
	call j_clearoption(iob,io)  ! subroutine
	if(j_err)return
	if(eof(1).gt.0)then
		j_v(eof(1))=0.
	endif !if(eof(1).gt.0)  16643
	if(nu.le.0.and.nu.ne.5)then
		call j_getchar2(ivform,j_form_,lef)
		if(j_err)return
		iv22=iv2
		if(wait)iv22=-iv2
		! pitäis katsoa
		! j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout,readit,deleted) !get file for writing, name is stored in character constant ivfile
		!222	call j_openreadiv(iv22,j_form_(1:lef),nu) !openfile
		222	call j_getfile(nu,'r',ivfile=iv22,forma=j_form_(1:lef)) !openfile

		if(j_err)return
 
	else !if(nu.le.0.and.nu.ne.5)then
		if(j_unitchar(nu).gt.0)then
			write(6,*)'*read: trying to read write file, file remains open'
			j_err=.true.
			return
		endif !if(j_unitchar(nu).gt.0)  16659
		if(ivform.ne.j_ivdollar)call j_getchar2(ivform,j_form_,lef)
		if(j_err)return
	end if !if(nu.le.0.and.nu.ne.5)  16646
	if(j_form_(1:1).eq.'b')then
		if(j_form_(lef:lef).eq.'n')then
			read(nu,err=990,end=880,iostat=ios)n,(j_v(j_o(iob)%i(io+1+j)),j=4,min(n+3,narg))
			j_v(j_o(iob)%i(io+4))=n
		else !if(j_form_(lef:lef).eq.'n')then
			if(narg.eq.2)then
				read(nu,err=990,end=880,iostat=ios)
			elseif(j_otype(ivarg3).eq.j_ipmatrix)then !if(narg.eq.2)then
				if(j_form_(1:lef).eq.'bis')then
					nel=0
					do j=3,narg
						nel=nel+j_o( j_o(iob)%i(io+1+j))%i(3)
					enddo !j=3,narg  16677
					allocate(bytes(1:nel))
					read(nu,err=990,end=880)bytes
					iel=0
					do j=3,narg
						nel=j_o(j_o(iob)%i(io+1+j))%i(3)
						j_o( j_o(iob)%i(io+1+j) )%d(1: nel)=bytes(iel+1:iel+nel)
						iel=iel+nel
					enddo !j=3,narg  16683
					deallocate(bytes)
				else !if(j_form_(1:lef).eq.'bis')then
					read(nu,err=990,end=880)(j_o( j_o(iob)%i(io+1+j) )%d(1: j_o( j_o(iob)%i(io+1+j))%i(3)),j=3,narg)
				endif !if(j_form_(1:lef).eq.'bis')  16675
			else !if(narg.eq.2)then
				nval=narg-2
				if(j_n_vector.lt.nval)then
					if(allocated(j_vector))deallocate(j_vector)
					allocate(j_vector(1:nval))
					j_n_vector=nval
				endif !if(j_n_vector.lt.nval)  16694
				!j_v(j_o(iob)%i(io+1+j)),j=3,narg)
				read(nu,err=990,end=880,iostat=ios)j_vector(1:nval)
				ii1=io+4
				ii2=io+1+narg
				j_v(j_o(iob)%i(ii1:ii2))=j_vector(1:nval)
			endif !if(narg.eq.2)  16672
		endif !if(j_form_(lef:lef).eq.'n')  16668
	elseif(j_form_(1:1).eq.'B')then !if(j_form_(1:1).eq.'b')then
		nval=narg-2
		if(j_n_dvector.lt.nval)then
			if(allocated(j_dvector))deallocate(j_dvector)
			allocate(j_dvector(1:nval))
			j_n_dvector=nval
		endif !if(j_n_dvector.lt.nval)  16708
		!j_v(j_o(iob)%i(io+1+j)),j=3,narg)
		read(nu,err=990,end=880,iostat=ios)j_dvector(1:nval)
		ii1=io+4
		ii2=io+1+narg
		j_v(j_o(iob)%i(ii1:ii2))=j_dvector(1:nval)
	else if(j_form_(1:1).eq.'(')then !if(j_form_(1:1).eq.'b')then
		if(j_form_(lef:lef).eq.'n')then
			read(nu,fmt=j_form_(1:lef-1),err=990,end=880,iostat=ios)n,(j_v(j_o(iob)%i(io+1+j)),j=4,min(n+3,narg))
			j_v(j_o(iob)%i(io+4))=n
			if(narg.lt.n+3)goto 770
		else !if(j_form_(lef:lef).eq.'n')then
			read(nu,fmt=j_form_(1:lef),err=990,end=880,iostat=ios)(j_v(j_o(iob)%i(io+1+j)),j=3,narg)
		endif !if(j_form_(lef:lef).eq.'n')  16719
	else !if(j_form_(1:1).eq.'b')then
		if(j_form_(lef:lef).eq.'n')then
			read(nu,*,err=990,end=880,iostat=ios)n,(j_v(j_o(iob)%i(io+1+j)),j=4,min(n+3,narg))
			j_v(j_o(iob)%i(io+4))=n
			if(narg.lt.n+3)goto 770
		elseif(ivform.eq.j_ivdollar)then !if(j_form_(lef:lef).eq.'n')then
 
			if(j_otype(ivarg3).eq.j_ipmatrix)then
				read(nu,*,err=990,end=880)(j_o( j_o(iob)%i(io+1+j) )%d(1: j_o( j_o(iob)%i(io+1+j))%i(3)),j=3,narg)
			else !if(j_otype(ivarg3).eq.j_ipmatrix)then
				read(nu,*,err=990,end=880,iostat=ios)(j_v(j_o(iob)%i(io+1+j)),j=3,narg)
			endif !if(j_otype(ivarg3).eq.j_ipmatrix)  16733
		else !if(j_form_(lef:lef).eq.'n')then
			write(6,*)'*read: illegal format ',j_form_(1:lef)
			if(j_ninc.eq.1)write(6,*)' file remains open'
			j_err=.true.
			return
		endif !if(j_form_(lef:lef).eq.'n')  16727
	end if !if(j_form_(1:1).eq.'b')  16667
	900	continue
	return
	990 write(6,*)'**read: error in reading from file, form=',j_form_(1:lef)
	write(6,*)'iostat=',ios
	write(6,*)'**closing file'
	call j_closeunit(nu)
	j_err=.true. !;return
	return
	880 continue
	if(eof(1).gt.0)then
		j_v(eof(1))=1
	else !if(eof(1).gt.0)then
		write(6,*)'**eof in file ',j_filename(1:le)
		write(6,*)'closing file'
		j_err=.true.
	endif !if(eof(1).gt.0)  16754
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
	! write(file,format,val1,…,valn[,tab->][,rows->])! case[1/6]
	! Writes real values to a file or to the console. If val1 is a matrix then this matrix (or usually
	! vector) is written or at most as many values as given in the values-> option.
	! Arguments:
	! file variable $ (indicating the console), or the name of the file as a character variable
	! or a character constant, or variable $Buffer
	! format
	! $ indicates the '*' format of Fortran, works only for numeric values.
	! A character expression, with the following possibilities:
	! A format starting with 'b' will indicate binary file. Now 'b' indicates ordinary
	! unformatted write, later there will be other binary formats
	! A Fortran format statement, e.g. (~the values were ~,4f6.0), with this
	! format pure text can be written by having no object to write (e.g.
	! write('out.txt','(~kukuu~)')).
	! For these formats, other arguments are supposed to be real variables or numeric
	! expressions or there is a matrix argument. If they are not, then just the real value
	! which is anyhow associated with each Jlp22 object is printed (usually it will be zero).
	! If the val1 argument is a matrix, then all values are printed.
	! val1,…,valn
	! real values
	! Options:
	! tab if format is a Fortran format then, tab-> option indicates that sequences of
	! spaces are replaced by tab character so that written text can be easily converted
	! to Ms Word tables. If there are no decimals after the decimal point also the
	! decimal point is dropped.
	! rows If val1 is a matrix or a vector and rows-> has one argument then at most as
	! many values are written as given in this option, if there are two arguments then
	! the option gives the range of written rows in the form rows->(row1,-row2). If the
	! upper limit is greater than the number of rows, no error is produced, all available
	! rows are just written.
	! write(file,'t',t1,val1,t2,val2,…,tn,valn[,tab->]) ! case[2/6]
	! Tabulation format. positive tab position values indicate that the value is written starting from
	! that position, negative tab positions indicate that the value is written up to that position. The
	! values can be either numeric expressions or character variables or character constants. Tab
	! positions can be in any order.
	! Arguments:
	! file variable $ (indicating the console), or the name of the file as a character variable
	! or a character constant, or variable $Buffer
	! 't' tabulation format
	! t1,val1,t2,val2,…,tn,valn
	! KUVAUS
	! Options:
	! tab option indicates that sequences of spaces are replaced by tab character so that
	! writ-ten text can be easily converted to Ms Word tables.
	!endheader
	! note Variable names from a variable list can be written using @-construction. E.g. if xl is a
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
	! or a character constant, or variable $Buffer
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
	!endsection
 
	logical bin,wform,bin8
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
	call j_startfunction(iob,io,0,narg0,j_optarg0,ivout) !args of the function
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
	nrow1=0
	nrow2=0
	if(nargval.eq.1)then
		nrow2=j_v(j_optarg0(1))
	elseif(nargval.eq.2)then !if(nargval.eq.1)then
		nrow1=j_v(j_optarg0(1))
		nrow2=-j_v(j_optarg0(2))
		if(nrow1.lt.1.or.nrow2.lt.nrow1)then
			write(6,*)'*write, illegal rows-> ',nrow1,-nrow2
			j_err=.true.
			return
		endif !if(nrow1.lt.1.or.nrow2.lt.nrow1)  16930
	endif !if(nargval.eq.1)  16925
 
	call j_clearoption(iob,io)  ! subroutine
 
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
			endif !if(j_otype(ivform).eq.j_iptext)  16958
			if(j_err)return
			!nu=j_iounit(ivfile) !j_o(ivfile)%i(4)
			!j_unitchar(nu)=ivfile
		else !if(nu.le.0)then
			if(j_unitchar(nu).lt.0)then
				write(6,*)'write: trying to write into read file'
				j_err=.true.
				return
			endif !if(j_unitchar(nu).lt.0)  16973
		end if !if(nu.le.0)  16956
 
	else if(ivfile.eq.j_ivbuffer)then !if(ivfile.eq.j_ivdollar)then
		if(j_v(j_ivbuffer).eq.0)j_buffer=' '
	else !if(ivfile.eq.j_ivdollar)then
		write(6,*)'**illegal file in write';j_err=.true.;return
	end if !if(ivfile.eq.j_ivdollar)  16951
 
 
 
	if(ivform.eq.j_ivdollar.or.ivform.eq.j_ivdollar2)then
		if(narg.eq.0)then
			write(nu,*)' '
			return
		endif !if(narg.eq.0)  16989
		if(narg.eq.1.and.j_otype(arg(1)).eq.j_iptext)then
			if(nu.eq.6)then
				call j_writetext(nu,arg(1),0)
			else
				call j_writetext(nu,arg(1),0,.true.)
 
			endif !if(nu.eq.6)  16994
			return
		endif !if(narg.eq.1.and.j_otype(arg(1)).eq.j_iptext)  16993
 
		if(j_writevar(nu,narg,arg,ivform))return
 
	endif !if(ivform.eq.j_ivdollar.or.ivform.eq.j_ivdollar2)  16988
	!	write(6,*)'ivform,ivbuffer',ivform,j_ivbuffer
 
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
						endif !if(j0.gt.0.and.j_buffer(i1:i1).eq.' ')  17019
 
						j0=j0+1
						line_(j0:j0)=j_buffer(i:i)
					endif !if(j_buffer(i:i).ne.' ')  17017
				enddo !i=1,lenbu  17016
				if(j0.gt.0)then
					if(line_(j0:j0).eq.'.')j0=j0-1
				endif !if(j0.gt.0)  17028
				!	if(nu.ne.6)call j_toutf8(line_(1:max(j0,1)))
				!write(6,*)'<12 ',line_(1:max(j0,1))
				write(nu,'(a)',err=990)line_(1:max(j0,1))
			else !if(tabform)then
				!if(nu.ne.6)call j_toutf8(j_buffer(1:lenbu))
				!write(6,*)'<13 ',j_buffer(1:lenbu)
				write(nu,'(a)',err=990)j_buffer(1:lenbu)
			endif !if(tabform)  17013
		end if !if(j_v(j_ivbuffer).eq.0.)  17009
		return
	elseif(j_otype(ivform).eq.j_iptext.and.narg.eq.0)then !if(ivform.eq.j_ivbuffer)then
		!	write(6,*)'>128'
		if(nu.eq.6)then
			call j_writetext(nu,ivform,0)
		else
			call j_writetext(nu,ivform,0,.true.)
 
		endif !if(nu.eq.6)  17043
		return
 
		! !		WRITE(6,*)'<J331NU,ITE,0,.TRUE',nu,ite
		! call j_writetext(nu,0,0,.true.)
		! return
	end if !800	if(ivform.eq.j_ivbuffer)  17008
	if(narg.le.0)then
		if(j_otype(ivform).eq.j_ipchar)then
			call j_getchar(ivform,j_form_,lef)
			!		if(nu.ne.6)call j_toutf8(j_form_(1:lef))
			!write(6,*)'>129',j_form_(1:lef)
			write(nu,'(a)')j_form_(1:lef)
 
			return
		else !if(ivform.ne.j_ivdollar.and.ivfile.ne.j_ivbuffer.and.j_otype(ivform).eq.j_ipchar)then
			goto 990
		endif !if(j_otype(ivform).eq.j_ipchar)  17056
	endif !if(narg.le.0)  17055
 
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
	if(j_otype(ivform).eq.j_ipchar)call j_getchar(ivform,j_form_,lef)
 
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
				enddo !ir=nrow1c,nrow2c  17116
				!endif !if(single)then
			endif !if(ncols.eq.1)  17108
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
				enddo !ir=nrow1c,nrow2c  17132
				!endif !if(single)then
			endif !if(ncols.eq.1)  17124
			return
		elseif(bin)then !if(ivform.eq.j_ivdollar)then
 
			if(ncols.eq.1)then
				write(nu,err=990)real(j_o(irg)%d(nrow1c:nrow2c))
			else !if(ncols.eq.1)then
				do ir=nrow1c,nrow2c
					write(nu,err=990)real(j_o(irg)%d((ir-1)*ncols+1:ir*ncols))
				enddo !ir=nrow1c,nrow2c  17143
			endif !if(ncols.eq.1)  17140
			!endif !if(single)then
			return
		elseif(bin8)then !if(ivform.eq.j_ivdollar)then
			if(ncols.eq.1)then
				write(nu,err=990)j_o(irg)%d(nrow1c:nrow2c)
			else !if(ncols.eq.1)then
				do ir=nrow1c,nrow2c
					write(nu,err=990)j_o(irg)%d((ir-1)*ncols+1:ir*ncols)
				enddo !ir=nrow1c,nrow2c  17153
				return
			endif !if(ncols.eq.1)  17150
 
		elseif(j_otype(ivform).eq.j_ipchar)then !if(ivform.eq.j_ivdollar)then
 
			if(ncols.eq.1)then
				write(nu,j_form_(1:lef),err=990)j_o(irg)%d(nrow1c:nrow2c)
			else !if(ncols.eq.1)then
				do ir=nrow1c,nrow2c
					write(nu,j_form_(1:lef),err=990)j_o(irg)%d((ir-1)*ncols+1:ir*ncols)
				enddo !ir=nrow1c,nrow2c  17164
			endif !if(ncols.eq.1)  17161
			return
			!endif !if(single)then
		endif !if(ivform.eq.j_ivdollar2)  17107
		ismat=.true.
	else !if(narg.eq.1.and.j_otype(irg).eq.j_ipmatrix)then
		ismat=.false.
		!	nval=narg-2
	endif !if(narg.eq.1.and.j_otype(irg).eq.j_ipmatrix)  17093
 
	!write(6,*)'tassa '
 
	if(bin)then
		if(j_n_vector.lt.narg)then
			if(allocated(j_vector))deallocate(j_vector)
			allocate(j_vector(1:narg))
			j_n_vector=narg
		endif !if(j_n_vector.lt.narg)  17180
		!	write(6,*)'<55>',j_v( j_o(iob)%i(io+4:io+3+nval) )
		j_vector(1:narg)=j_v( arg(1:narg) )
		!	write(6,*)'<388383',j_vector(1:nval)
		write(nu,err=990)j_vector(1:narg) !(v(o(iob)%i(io+1+j)),j=3,narg)
		return
	endif !if(bin)  17179
 
 
 
	if(narg.gt.0)then !if(ismat)then
		if(j_n_dvector.lt.narg)then
			if(allocated(j_dvector))deallocate(j_dvector)
			allocate(j_dvector(1:narg))
			j_n_dvector=narg
		endif !if(j_n_dvector.lt.narg)  17195
		!		write(6,*)'>3>',j_v( j_o(iob)%i(io+4:io+3+nval) )
		j_dvector(1:narg)=j_v( arg(1:narg) )
	endif !if(narg.gt.0)  17194
 
 
	if(bin8.and.narg.gt.0)then
		!	write(6,*)'nva',nval
		write(nu,err=990)j_dvector(1:narg)
		return
	endif !if(bin8.and.narg.gt.0)  17205
 
	if(ivform.eq.j_ivdollar)then !if(bin)then
 
		write(nu,*,err=990)real(j_vector(1:narg))
 
 
 
	elseif(ivform.eq.j_ivdollar2)then
		if(ivfile.ne.j_ivbuffer)then
			write(nu,*,err=990)j_dvector(1:narg) !(v(o(iob)%i(io+1+j)),j=3,narg)
		else !if(ivfile.ne.j_ivbuffer)then
			write(j_buffer,*,err=990)j_dvector(1:narg) !(v(o(iob)%i(io+1+j)),j=3,narg)
			j_v(j_ivbuffer)=len_trim(j_buffer)
		end if !if(ivfile.ne.j_ivbuffer)  17218
 
 
	else !if(ivform.eq.j_ivdollar)then
		call j_getchar(ivform,j_form_,lef)
		!	if(nu.ne.6)call j_toutf8(j_form_(1:lef))
		if(j_form_(1:1).eq.'(')then
			if(ivfile.ne.j_ivbuffer)then
				if(tabform)then
					write(line_,fmt=j_form_(1:lef),err=989)j_dvector(1:narg)
					lenbu=len_trim(line_)
					j0=0
					do i=1,lenbu
						if(line_(i:i).ne.' ')then
							i1=max(i-1,1)
							if(j0.gt.0.and.line_(i1:i1).eq.' ')then
								if(line_(j0:j0).ne.'.')j0=j0+1
								line_(j0:j0)=char(9)
							endif !if(j0.gt.0.and.line_(i1:i1).eq.' ')  17238
							j0=j0+1
							line_(j0:j0)=line_(i:i)
						endif !if(line_(i:i).ne.' ')  17236
					enddo !i=1,lenbu  17235
					if(j0.gt.0)then
						if(line_(j0:j0).eq.'.')j0=j0-1
					endif !if(j0.gt.0)  17246
					!	if(nu.ne.6)call j_toutf8(line_(1:max(j0,1)))
					!	write(6,*)'>12' ,line_(1:max(j0,1))
					write(nu,'(a)',err=990)line_(1:max(j0,1))
				else !if(tabform)then
					!	write(6,*)'>129' ,temp(1:nval)
 
					write(nu,fmt=j_form_(1:lef),err=989)j_dvector(1:narg)
				endif !if(tabform)  17231
			else !if(ivfile.ne.j_ivbuffer)then
				!	write(6,*)'>1288'
				write(j_buffer,fmt=j_form_(1:lef),err=989)j_dvector(1:narg) ! (v(o(iob)%i(io+1+j)),j=3,narg)
				j_v(j_ivbuffer)=len_trim(j_buffer)
			end if !if(ivfile.ne.j_ivbuffer)  17230
		else if(j_form_(1:lef).eq.'t'.or.j_form_(1:lef).eq.'w')then !if(j_form_(1:1).eq.'(')then
			if(ismat)then
				write(6,*)'write: matrix cannot be written with form->',j_form_(1:lef)
				j_err=.true.
				return
			endif !if(ismat)  17263
			if(mod(narg,2).ne.0)then; write(6,*)'**illegal nro of args in write';j_err=.true.;return;end if
			wform=.false.;if(j_form_(1:1).eq.'w')then ;wform=.true.;lew=0;end if
			line_=' '
			do j=1,narg,2
				tab=j_v(arg(j))
				itab=tab  !either tab or w
				if(abs(itab).gt.80)then
					write(6,*)'**illegal tab in writing with t-format:',itab;j_err=.true.;return
				end if !if(abs(itab).gt.80)  17274
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
					end if !if(wform.and.nint(100.*tab).ne.100*itab)  17280
				else if(j_otype(iv).eq.j_ipchar)then !if(j_otype(iv).eq.j_ipreal)then
					call j_getchar(iv,luku,le)
					!	write(6,*)'got:',luku(1:le)
				else !if(j_otype(iv).eq.j_ipreal)then
					call j_printname('**illegal variable type in write:',iv,' ')
					j_err=.true.;return
				end if !if(j_otype(iv).eq.j_ipreal)  17278
				if(wform.and.le.gt.itab)le=itab
				if(itab.lt.0)then
					itab=-itab
					if(wform)then   ! lew cumulative lentgth
						i1=lew+1;i2=i1+le-1;lew=lew+itab
					else !if(wform)then
						i1=max(itab-le+1,1);i2=itab
					end if !if(wform)  17299
				else !if(itab.lt.0)then
					if(wform)then
						i2=lew+itab;i1=i2-le+1;lew=lew+itab  !right justified
					else !if(wform)then
						i1=itab;i2=min(itab+le-1,80)
					end if !if(wform)  17305
				end if !if(itab.lt.0)  17297
				line_(i1:i2)=luku(1:le)
			end do !j=1,narg,2  17271
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
							endif !if(j0.gt.0.and.line_(i1:i1).eq.' ')  17321
							j0=j0+1
							line_(j0:j0)=line_(i:i)
						endif !if(line_(i:i).ne.' ')  17319
					enddo !i=1,lel  17318
					if(j0.gt.0)then
						if(line_(j0:j0).eq.'.')j0=j0-1
					endif !if(j0.gt.0)  17329
					write(nu,'(a)',err=990)line_(1:max(j0,1))
				else !if(tabform)then
					write(nu,'(a)',err=990)line_(1:lel)
				endif !if(tabform)  17315
			else !if(ivfile.ne.j_ivbuffer)then
				if(wform)then
					leb=j_v(j_ivbuffer)
					if(leb+lel.gt.256)then
						write(6,*)'**write: $Buffer is only 256 char long'
						j_err=.true.
						return
					endif !if(leb+lel.gt.256)  17339
					j_buffer(leb+1:leb+lel)=line_(1:lel)
					j_v(j_ivbuffer)=leb+lel
				else !if(wform)then
					do j=1,lel
						if(line_(j:j).ne.' ')j_buffer(j:j)=line_(j:j)
					enddo !j=1,lel  17347
					j_v(j_ivbuffer)=j_lentrim(j_buffer)
				endif !if(wform)  17337
			end if !if(ivfile.ne.j_ivbuffer)  17314
		else !if(j_form_(1:1).eq.'(')then
			write(6,*)'**illegal format in write';j_err=.true.
		end if !if(j_form_(1:1).eq.'(')  17229
	end if !if(ivform.eq.j_ivdollar)  17211
	900 continue !   if(j_err)return
	!write(6,*)'<776nu,iv ',j_iounit(ivfile),ivfile
	return
	989  write(6,*)'format:',j_form_(1:lef)
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
				write(6,*)'#type is:',j_objecttypes( j_otype(irg(i)))
				j_err=.true.
				return
			endif !if(j_otype( irg(i)).eq.j_ipreal)  17379
		end if !if(j_otype( irg(i)).eq.j_ipmatrix)  17375
	end do !i=1,2  17373
 
	iout=j_o(iob)%i(io+2+narg)
	if(ndim1(1).eq.0.and.ndim1(2).eq.0)then
		if(j_otype(iout).ne.j_ipreal)call j_del(iout)
		j_v(iout)=j_v(irg(1))*j_v(irg(2))
	else if(ndim1(1).ne.0.and.ndim1(2).ne.0)then !if(ndim1(1).eq.0.and.ndim1(2).eq.0)then
		nel=ndim1(1)*ndim2(1)
		if(irg(1).ne.iout.and.irg(2).ne.iout)ivout=j_defmatrix8(iout,' ',&
			ndim1(1),ndim2(1),j_matreg)
		j_o(iout)%d=j_o(irg(1))%d(1:nel)*j_o(irg(2))%d(1:nel)
	else !if(ndim1(1).eq.0.and.ndim1(2).eq.0)then
		write(6,*)'**arguments of elementprod must both be matrices or both real numbers'
		j_err=.true.
	end if !if(ndim1(1).eq.0.and.ndim1(2).eq.0)  17391
	900 if(irg(1).gt.j_namedv)call j_del(irg(1))
	if(irg(2).gt.j_namedv)call j_del(irg(2))
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
 
	integer*8::ndim1,ndim2,i
	narg=j_o(iob)%i(io+1)
	iout=j_o(iob)%i(io+2+narg)
	irg=j_o(iob)%i(io+2)
	if(j_otype(irg).ne.j_ipmatrix)then
		if(j_otype(irg).eq.j_ipreal)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=j_v(irg)
 
		elseif(j_otype(irg).eq.j_iplist)then !if(j_otype(irg).eq.j_ipreal)then
			ivout=j_defmatrix(iout,' ',1,j_o(irg)%i(0),j_matreg)
			j_o(iout)%d=j_v(j_o(irg)%i(1: j_o(irg)%i(0)))
		else !if(j_otype(irg).eq.j_ipreal)then
			call j_printname('illegal type of ',irg, ' for transpose')
			write(6,*)'type is: ',j_objecttypes( j_otype(irg))
			j_err=.true.
 
		endif !if(j_otype(irg).eq.j_ipreal)  17432
		return
	end if !if(j_otype(irg).ne.j_ipmatrix)  17431
	!ndim1=j_o(irg)%i8(1);ndim2=j_o(irg)%i8(2)
	ndim1=j_nrows(irg);ndim2=j_ncols(irg)
	if(irg.eq.iout)then
		allocate(copy(1:ndim1*ndim2))
		copy=j_o(irg)%d(1:ndim1*ndim2)
		do i=1,ndim1
			do j=1,ndim2
				j_o(iout)%d((j-1)*ndim1+i)   =copy((i-1)*ndim2+j)
 
			enddo !j=1,ndim2  17453
		enddo !i=1,ndim1  17452
		call j_i8i4(ndim2,j_o(ivout)%i(14:15))
		call j_i8i4(ndim1,j_o(ivout)%i(16:17))
		j_o(iout)%i(1)=ndim2
 
		j_o(iout)%i(2)=ndim1
 
	else !if(irg.eq.iout)then
		ivout=j_defmatrix8(iout,' ',ndim2,ndim1,j_matreg)
		j_o(ivout)%d=reshape(transpose(reshape(j_o(irg)%d,(/ndim2,ndim1/))),(/ndim1*ndim2/))
		if(irg.gt.j_namedv)call j_del(irg)
	endif !if(irg.eq.iout)  17449
	!	900 !io=io+j_o(iob)%i(io+1)+3
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
 
	call j_startfunction(iob,io,0,narg,arg,iout)
 
	irg=arg(1)  !o(iob)%i(io+2)
	if(j_otype(irg).ne.j_ipmatrix)then
		if(j_otype(irg).eq.j_ipreal)then
			if(j_otype(iout).ne.j_ipreal)call j_del(iout)
			j_v(iout)=1./j_v(irg)
			return
		else !if(j_otype(irg).eq.j_ipreal)then
			call j_printname('**argument of inverse ',irg,' not a matrix or real var')
			write(6,*)'#type is:',j_objecttypes( j_otype(irg))
			j_err=.true. ;return
		end if !if(j_otype(irg).eq.j_ipreal)  17520
	end if !if(j_otype(irg).ne.j_ipmatrix)  17519
	ndim1=j_nrows(irg)  !j_o(irg)%i8(1)
	ndim2=j_ncols(irg)   !j_o(irg)%i8(2)
	if(ndim1.ne.ndim2)then
		call j_printname('**argument ',irg,' of inverse is  not a square matrix')
		j_err=.true. ; return
	end if !if(ndim1.ne.ndim2)  17532
	if(j_o(irg)%i(4).eq.j_matdiag)then
		if(iout.ne.irg)ivout=j_defmatrix(iout,' ',ndim1,ndim2,j_matdiag)
		ibas=0
		do i=1,ndim1
			if(j_o(irg)%d(ibas+i).eq.j_0)then
				write(6,*)'diagonal element ',i,' is zero'
				j_err=.true.;return
			endif !if(j_o(irg)%d(ibas+i).eq.j_0)  17540
			j_o(iout)%d(ibas+i)=j_1/j_o(irg)%d(ibas+i)
			ibas=ibas+ndim1
		enddo !i=1,ndim1  17539
		return
	endif !if(j_o(irg)%i(4).eq.j_matdiag)  17536
	! call j_getoption(iob,io,j_mcondition,-1,1,j_ipreal,.true.,no,j_optarg0)
	! call j_clearoption(iob,io)
	! if(j_err)return
	!if(no.gt.0)ivnorm=j_optarg0(1)
 
 
	allocate(mat(1:ndim1,1:ndim1),rhs_(1:ndim1,1:ndim1),ipiv(1:ndim1))
	iel=0
	rhs_=0.d0  !
	do i=1,ndim1
		do j=1,ndim1
			iel=iel+1
			mat(i,j)=j_o(irg)%d(iel)
		enddo !j=1,ndim1  17559
		rhs_(i,i)=1.d0
	enddo !i=1,ndim1  17558
 
	call dgesv(ndim1,ndim1,mat,ndim1,ipiv,rhs_,ndim1,info_)
	if(info_.ne.0)then
		j_err=.true.
		write(6,*)'inverse: singular matrix'
		return
	endif !if(info_.ne.0)  17567
	ivout=j_defmatrix(iout,' ',ndim2,ndim1,j_matreg)
	iel=0
	do i=1,ndim1
		do j=1,ndim1
			iel=iel+1
			j_o(ivout)%d(iel)=rhs_(i,j)
		enddo !j=1,ndim1  17575
	enddo !i=1,ndim1  17574
 
	if(ivout.le.j_namedv)then
		call j_getobject(iout,'%condition',j_ipreal,ivnorm)
		j_dapu=j_0
		j_dapu2=j_0
		do i=1,ndim1*ndim1
			j_dapu=j_dapu+j_o(ivout)%d(i)**2
			j_dapu2=j_dapu2+j_o(irg)%d(i)**2
		enddo !i=1,ndim1*ndim1  17585
		j_v(ivnorm)=sqrt(j_dapu)*sqrt(j_dapu2)
	endif !if(ivout.le.j_namedv)  17581
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
	call j_startfunction(iob,io,0,narg,arg,iout)
	irg=arg(1)  !o(iob)%i(io+2)
	if(j_otype(irg).ne.j_ipmatrix)then
 
		call j_printname('**argument of qr ',irg,' not a matrix')
		write(6,*)'#type is:',j_objecttypes( j_otype(irg))
		j_err=.true. ;return
	end if !if(j_otype(irg).ne.j_ipmatrix)  17628
	ndim1=j_o(irg)%i(1);ndim2=j_o(irg)%i(2)
	allocate(a_(1:ndim1,1:ndim2),tau(1:min(ndim1,ndim2)),work(1:ndim2))
	iel=0
	do i=1,ndim1
		do j=1,ndim2
			iel=iel+1
			a_(i,j)=j_o(irg)%d(iel)
		enddo !j=1,ndim2  17638
	enddo !i=1,ndim1  17637
 
	call dgeqrf(ndim1,ndim2,a_,ndim1,tau,work,ndim2,info_)
 
	if(info_.ne.0)then
		write(6,*)'*j* qr failed, info ',info_
		j_err=.true.
		return
	endif !if(info_.ne.0)  17646
	if(iout.gt.0)iout=j_defmatrix(iout,' ',min(ndim1,ndim2),ndim2,j_matreg)
	write(6,*)'r -matrix' !, * indicates nonzero'
	do i=1,min(ndim1,ndim2)
 
		do j=1,ndim2
			j_tempchar(2*(j-1)+1:2*(j-1)+2)='0 '
		enddo !j=1,ndim2  17655
		do j=i,ndim2
			j_o(iout)%d((i-1)*ndim2+j)=a_(i,j)
			if(abs(a_(i,j)).gt.1.d-10)then
 
				j_tempchar(2*(j-1)+1:2*(j-1)+2)='* '
			endif !if(abs(a_(i,j)).gt.1.d-10)  17660
		enddo !j=i,ndim2  17658
		write(6,*)j_tempchar(1:2*ndim2)
		!write(6,'(9f8.5/)')a_(i,1:ndim2)
	enddo !i=1,min(ndim1,ndim2)  17653
 
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
	call  j_startfunction(iob,io,0,narg,j_arg,iout)
 
	isilist=j_isopt(iob,io,j_milist,.false.)
	nn=0
	do i=1,narg
		j_yes=.false. !is it already
		do j=1,i-1
			if(j_arg(i).eq.j_arg(j))then
				j_yes=.true.
				exit
			endif !if(j_arg(i).eq.j_arg(j))  17704
 
		enddo !j=1,i-1  17703
		if(.not.j_yes)nn=nn+1
 
	end do !i=1,narg  17701
 
	call j_deflistobject(iout,' ',ivout,nres=nn,ilist=isilist)
	nn=0
	do i=1,narg
		j_yes=.false. !is it already
		do j=1,i-1
			if(j_arg(i).eq.j_arg(j))then
				j_yes=.true.
				exit
			endif !if(j_arg(i).eq.j_arg(j))  17719
 
		enddo !j=1,i-1  17718
		if(.not.j_yes)then
			nn=nn+1
			j_o(iout)%i2(nn)=j_arg(i)
 
		endif !if(.not.j_yes)  17725
	end do !i=1,narg  17716
	j_o(iout)%i(1)=nn
	!90	io=io+j_o(iob)%i(io+1)+3
	return
end subroutine merge !subroutine merge(iob,io)
 
 
 
 
 
! subroutine dot(iob,io)  ! dot()
 
! double precision sum
! sum=0.d0
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
	pv=0.d0
	do  i=io+3,io+2+ni
		pv=pv+j_v( j_o(iob)%i(i)) * r1**j_v( j_o(iob)%i(i+ni) ) !do 11 i=io+3,io+2+ni
	enddo ! i=io+3,io+2+ni  17845
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
	! is dependent on the value of ]Printoutput[. If ]printoutput[ =0,
	! then the output is not printed, If ]printoutput[ =1, then ';' is
	! causing printing, if ]Printoutput[ =2 then only ';;'-outputs are
	! printed, and if ]Printoutput[ =3, then both ';' and ';;' outputs are printed.
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
	integer,dimension(:),pointer::arg,file
	!	io=io_
	call j_startfunction(iob,io,0,narg,arg,ivout)
 
	if(narg.eq.1)then
		if(j_otype(arg(1)).eq.j_iplist)then
 
 
			if(j_linkoption(iob,io,j_mexpand,clear=.true.).ge.0)then
				narg=j_o(arg(1))%i(1)
				arg=>j_o(arg(1))%i2(1:narg)
 
			endif !if(j_linkoption(iob,io,j_mexpand,clear=.true.).ge.0)  17914
 
		endif !if(j_otype(arg(1)).eq.j_iplist)  17911
	endif !if(narg.eq.1)  17910
 
 
	if(j_linkoption(iob,io,j_moptions,clear=.true.).ge.0)then
		call j_printoptions()
		return
 
	endif !if(j_linkoption(iob,io,j_moptions,clear=.true.).ge.0)  17924
 
 
	isappend=j_linkoption(iob,io,j_mappend).ge.0
	call j_getoption_index(iob,io,j_mform,-1,100,j_ipchar,.true.,noptargf,j_optarg0);if(j_err)return
 
 
	if(noptargf>0.and.j_optarg0(1).ne.j_ivdollar)then  !format
		call j_getchar(j_optarg0(1),j_form_,lef)
	endif !if(noptargf>0.and.j_optarg0(1).ne.j_ivdollar)  17935
	call j_getoption_index(iob,io,j_mfile,-1,100,j_ipchar,.true.,nfile,file);if(j_err)return
	!call j_getchar(ivform,j_f orm_,lef)
	isany=j_isopt(iob,io,j_many)
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
			endif !if(icur.eq.3.or.i.eq.j_nfunctions_.or.i.eq.j_nfuncs1.o  17958
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
		enddo !i=1,j_nfunctions  17952
		call j_clearoption(iob,io)  ! subroutine
		!goto 900
	endif !if(j_linkoption(iob,io,j_mfunctions).ge.0)  17944
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
					endif !if(isappend)  17997
					!call j_getwritefile(ivfile_,j_ivdollar,bin_)
					if(j_err)goto 900
					!	nu_=j_iounit(file(1)) !j_o(ivfile_)%i(4)
				endif !if(nu_.le.0)  17994
			else !if(j_otype(file(1)).eq.j_ipchar)then
				! virheellinen option arvo
				write(6,*)'**illegal file-> option';j_err=.true.;goto 900
			endif !if(j_otype(file(1)).eq.j_ipchar)  17991
		else !if (nfile==1) then
			write(6,*)'**illegal number of arguments in file-> option';j_err=.true.;goto 900
		endif !if (nfile==1)  17988
	endif !if(nfile>0)  17987
 
	if(j_linkoption(iob,io,j_mmaxlines).gt.0)then
		maxlines=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmaxlines)+1))
	else !if(j_linkoption(iob,io,j_mmaxlines).gt.0)then
		maxlines=j_v(j_ivmaxlines)
	end if !if(j_linkoption(iob,io,j_mmaxlines).gt.0)  18015
	lirow=j_linkoption(iob,io,j_mrow)
	linkoptmdata=j_linkoption(iob,io,j_mdata)
 
 
	call j_clearoption(iob,io)  ! subroutine
	!	write(6,*)'narg',narg,arg(1),nfile,j_otype(arg(1))
 
 
	if(.not.debug)then
		if(j_printvar(nu_,narg,arg))return
	endif !if(.not.debug)  18028
	!	write(6,*)'<73737narg',narg
	!	write(6,*)'debug',debug
	do j=1,narg  ! o(iob)%i(io+1)
		!iv=j_o(iob)%i(io+1+j)
		!	write(6,*)'<73737narg',iv,j_otype(iv)
		iv=arg(j)
		call j_getname(iv)
		!	write(6,*)'<555>',iv,j_otype(iv),j_ipilist,j_oname(1:j_loname)
		if(debug) then
 
			write(nu_,*)'object index ',iv,' variable part: ',j_v(iv)
			write(nu_,*)' object type:',j_otype(iv), j_objecttypes(j_otype(iv))
			if(allocated(j_o(iv)%i)) write(nu_,*)'i: size',size(j_o(iv)%i), ' elements (max 100) ',&
				j_o(iv)%i(1:min(100,size(j_o(iv)%i)))
			if(allocated(j_o(iv)%i2)) write(nu_,*)'i2: size',size(j_o(iv)%i2),' : ',&
				j_o(iv)%i2(1:min(100,size(j_o(iv)%i2)))
			if(allocated(j_o(iv)%r)) write(nu_,*)'r: size',size(j_o(iv)%r),' : ',&
				j_o(iv)%r(1:min(100,size(j_o(iv)%r)))
			if(allocated(j_o(iv)%d)) write(nu_,*)'d: size',size(j_o(iv)%d),' : ', &
				j_o(iv)%d(1:min(100,size(j_o(iv)%d)))
			cycle
		endif !if(debug)  18039
 
 
 
		!	if(iv.le.j_nv)then
		!	iotype=j_otype(iv)
 
		!	end if !if(iv.le.j_nv)  17250
		if(j_otype(iv).gt.j_ipreal)then  !vois muuttaa otypet
			if(j_otype(iv).eq.j_ipmatrix)then
				write(nu_,*)' '
				iba=0
 
				!	else !if(iotype.eq.j_ipmatrix0)then
				write(nu_,*)j_oname(1:j_loname),' is MATRIX (',j_o(iv)%i(1),',',j_o(iv)%i(2),' )'
 
				!	write(nu_,'(30i5/)')j_o(iob)%i(io:io+59)
				!	write(nu_,'(30i5/)')(jj,jj=io,io+59)
				do i=1,min(maxlines,j_o(iv)%i(1))
					if(noptargf.gt.0)then
						if(isrow)then
							write(nu_,j_form_(1:lef),err=950)i,(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
						else !if(isrow)then
							write(nu_,j_form_(1:lef),err=950)(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
 
						endif !if(isrow)  18072
					else !if(noptargf.gt.0)then
						if(isrow)then
							write(nu_,'(i5,1x,10(g14.7,1x))')i,(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
						else !if(isrow)then
							write(nu_,'(10(g14.7,1x))')(j_o(iv)%d(iba+k),k=1,j_o(iv)%i(2) )
						endif !if(isrow)  18079
					endif !if(noptargf.gt.0)  18071
					iba=iba+j_o(iv)%i(2)
 
				end do !i=1,min(maxlines,j_o(iv)%i(1))  18070
 
				if(maxlines.lt.j_o(iv)%i(1))write(nu_,*)'*printing limited by maxlines'
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
							write(nu_,j_form_(1:lef),err=950)i,(j_o(iv)%r(iba+k),k=1,j_o(iv)%i(2) )
						else !if(isrow)then
							write(nu_,j_form_(1:lef),err=950)(j_o(iv)%r(iba+k),k=1,j_o(iv)%i(2) )
 
						endif !if(isrow)  18101
					else !if(noptargf.gt.0)then
						if(isrow)then
							write(nu_,'(i5,1x,10(g14.7,1x))')i,(j_o(iv)%r(iba+k),k=1,j_o(iv)%i(2) )
						else !if(isrow)then
							write(nu_,'(10(g14.7,1x))')(j_o(iv)%r(iba+k),k=1,j_o(iv)%i(2) )
						endif !if(isrow)  18108
					endif !if(noptargf.gt.0)  18100
					iba=iba+j_o(iv)%i(2)
 
				end do !i=1,min(maxlines,j_o(iv)%i(1))  18099
 
				if(maxlines.lt.j_o(iv)%i(1))write(nu_,*)'*printing limited by maxlines'
			else if(j_otype(iv).eq.j_iptext)then !if(iotype.eq.j_ipmatrix)then
				if(nu_.eq.6)then
					write(6,*)' '
					write(nu_,*)j_oname(1:j_loname),' is text object:'
				endif !if(nu_.eq.6)  18120
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
					end do !i=1,j_namedv  18125
				else !if(iv.eq.j_ivnames)then
					if(maxlines.gt.j_o(iv)%i(0).and.lirow.le.0)then
						!				WRITE(6,*)'<J44NU,ITE,0,.TRUE',nu_,iv,0
						if(nu_.eq.6)then
							call j_writetext(nu_,iv,0)
							write(nu_,*)'///end of text object '
						else
							call j_writetext(nu_,iv,0,.true.)
						endif !if(nu_.eq.6)  18140
					else !if(maxlines.gt.j_o(iv)%i(0).and.lirow.le.0)then
						if(lirow.gt.0)then
							ifi=j_v(j_o(iob)%i(lirow+1))
							if(ifi.gt.j_o(iv)%i(0))then
								call j_printname('**text object ',iv, ' does not have that many rows')
								write(6,*)'it has ',j_o(iv)%i(0), ' rows ', 'you asked to print row ',ifi
								j_err=.true. ;return
							endif !if(ifi.gt.j_o(iv)%i(0))  18149
							if(j_o(iob)%i(lirow).ge.2)then
								las=-j_v(j_o(iob)%i(lirow+2))
								if(las.lt.ifi)then
									write(nu_,*)'**print: the range must be given in form row->(5,-7)'
									j_err=.true. ;return
								endif !if(las.lt.ifi)  18156
								las=min(las,j_o(iv)%i(0))
							else !if(j_o(iob)%i(lirow).ge.2)then
								las=ifi  ! min(o(iv)%i(0),ifi+maxlines)
							endif !if(j_o(iob)%i(lirow).ge.2)  18154
						else !if(lirow.gt.0)then
							ifi=1
							las=min(maxlines,j_o(iv)%i(0))
						endif !if(lirow.gt.0)  18147
						if(nu_.eq.6)then
							do i=ifi,las
								!		WRITE(6,*)'<J991NU,ITE,0,.TRUE',nu_,iv,i,ite
								call j_writetext(nu_,iv,i)
							enddo !i=ifi,las  18169
							write(nu_,*)'*lines ',ifi,'-',las,' printed out of ', j_o(iv)%i(0)
						else
							do i=ifi,las
								!		WRITE(6,*)'<J991NU,ITE,0,.TRUE',nu_,iv,i,ite
								call j_writetext(nu_,iv,i,.true.)
							enddo !i=ifi,las  18175
 
						endif !if(nu_.eq.6)  18168
					endif !if(maxlines.gt.j_o(iv)%i(0).and.lirow.le.0)  18138
				end if !if(iv.eq.j_ivnames)  18124
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
						end if !if(j_ibittest(iv,jj,kk).ne.0)  18199
					end do !kk=j_o(iv)%i(3),min(j_o(iv)%i(2),79+j_o(iv)%i(3))  18197
					write(nu_,'(a)')j_cline(1:ic)
				enddo !jj=1,min(j_o(iv)%i(1),maxlines)  18195
			else if(j_otype(iv).eq.j_iplist)then !if(iotype.eq.j_ipmatrix)then
				call j_printlist(nu_,iv)
 
			elseif(j_otype(iv).eq.j_ipilist)then !if(iotype.eq.j_ipmatrix)then
				write(6,*)' '
				write(6,*)j_oname(1:j_loname),' is ILIST with ', j_o(iv)%i(1),&
					' elements,and allocated size ', j_o(iv)%i(4)
				write(6,'(20i7/)')j_o(iv)%i2(1:j_o(iv)%i(1))
			else if(j_otype(iv).eq.j_ipfigure)then !if(iotype.eq.j_ipmatrix)then
				!	j_o(iv)%i(2)=j_gplines
				!j_o(j_gpiout)%i(4)=j_gpplots
				write(6,*)' '
				write(nu_,*)j_oname(1:j_loname),' is a figure object with ',j_o(iv)%i(2), ' lines and ',&
					j_o(iv)%i(4), 'subplots'
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
				ivmat=j_o(iv)%i(1)
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
				end if !if(j_o(iv)%i(3).ne.0)  18247
				if(j_o(iv)%i(5).ne.0)then
					write(nu_,*)'  up-data: ', j_vname(j_o(iv)%i(5))
					write(nu_,*)'obsw-variable: ', j_vname(j_o(iv)%i(7))
				end if !if(j_o(iv)%i(5).ne.0)  18251
			else if(j_otype(iv).eq.j_ipchar)then !if(iotype.eq.j_ipmatrix)then
				write(nu_,*)(j_o(j_ivnames)%ch(jj),jj=j_o(iv)%i(1)-1,j_o(iv)%i(2)+1)
			else if(j_ipc(iv).ne.0)then !if(iotype.eq.j_ipmatrix)then
				call j_getchar(iv,name(le+2:),le2)
				write(nu_,*)j_oname(1:j_loname),"='",name(le+2:le+le2+1),"'"
			end if !if(j_otype(iv).eq.j_ipmatrix)  18061
		else !if(iotype.gt.j_ipreal)then
			if(j_otype(iv).eq.j_ipreal)then
				call j_getname(iv)
				write(nu_,*)' '
				write(nu_,*)j_oname(1:j_loname),'=',j_v(arg(j))
			end if !if(j_otype(iv).eq.j_ipreal)  18262
		end if !if(j_otype(iv).gt.j_ipreal)  18060
	end do !j=1,narg  18033
	900 continue ! io=io+narg+3
	!write(6,*)'kui'
	call j_clearoption(iob,io)
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
 
! if(iout.le.j_namedv.and.iout.gt.0)then
! call j_copy2(j_o(imat)%i(inde1),iout)
! !	write(6,*)'out:',j_v(iout)
! else !if(iout.le.j_namedv.and.iout.gt.0)then
! loc=j_o(iob)%i(io+3)
! j_o(iob)%i(loc)=j_o(imat)%i(inde1)
! !		write(6,*)'loc,iut ',loc,j_o(imat)%i(inde1)
! !	write(6,*)'iolopus ',io,'i:',j_o(iob)%i(1:j_o(iob)%i(0))
! endif !if(iout.le.j_namedv.and.iout.gt.0)then
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
	endif !if(j_otype(iout).ne.j_ipchar)  18326
 
	j_o(iout)%i(1:2)=j_o(in)%i(1:2)
	j_o(iout)%i(3)=in !  ; o(in)%i(3)=iout
	j_o(iout)%i(4)=0
 
	!	io=io+3
	return
end subroutine asschar !subroutine asschar(iob,io)
 
 
subroutine data(iob,io)  !data(
 
	!Section datahead Working with DATAs
	! Data can be analyzed and processed either using matrix computations
	! or using DATAs. A DATA is compound object
	!linked to a data MATRIX and LIST containing variable (column) names,
	! some other information.
	! When data ere used via DATA in statistical or linear programming
	! functions, the data are processed observarion by observation.
	! It is possible to work using DATA or using directly the data matrix, wharever is more
	!convenient. It is posible make new data objects or new matrices
	!by to extracting  columns of data matrix, computing matrices with matrix computations.
	!It is possible to use data in hierarcchila way, This property is inherited fro JLP.
	! There are two Jlp22 functions which create DATAs from files, data() and
	! exceldta(). data() can create hierarchical data objects. Function newdata() creates a DATA from matrices, which temselfs can be
	! picked from data objects. Function linkdata() can link two data sets to make a hierarchical data.
	!endheader
	!Note If a data file contains columns which are referred with variable names and some vectors,
	! the it is practical to read data first into a matrix using matrix() function and then
	! use matrix operations and newdata() to make DATA with variable names and matrices.
	! See Simulator section for an example.
	!endnote
	!Note transdata() function goes through a DATA similarly as statitical functions, but
	! does not serve a specific purpose, just transformations defined in the TRANS object refreed with
	! trans-> option are computed. See again the simulator section.
	!endnote
	!Note In earlier versions it was possible to give several data sets as arguments for data-> option.
	! This festure is now deleted as it is possible to stack several data matrices and then use newdata() function to create a single data set.
	!endnote
	!endsection
 
 
	!Section data data() Making a DATA
	! Data objects are created with the data() function. Two linked data objects can be created with the
	! same function call (using option subdata-> and options thereafter in the following
	! description). It is recommended that two linked data objects are created with one data()
	! function call only in case the data is read from a single file where subdata observations are
	! stored immediately after the upper data observation.  Data objects can be linked also afterwards with the linkdata() function.
	! A data object can created by a data() function when data are read from files
	! or data are created using transformation objects. New data objects can
	!be created with newdata() function from previous data objects and/or matrices.
	!If data objects can created using transformation objects either with data() function
	!or by creating first data matrix by transformation and then using newdata() to
	! create data object.
	!endheader
	!Option
	! Output &1&Data&
	! Data object to be created.
	! It is recommended that this default is used only when only one data object
	! is used in the analysis.
	!
	! read &0|1-&REAL|List& Variables read from the input files. If the first variable is %nobsw its
	! name is changed into Output%nobsw which is assumed in hierarchical data. 	If no arguments are given and there is no readfirst-> option
	! then the variables to read in are stored in the first line of the data file separated
	! with commas.?? Also the … -shortcut can be used to define the varaible list. If no arguments are given and
	! there is readfirst-> option then the variable names are read from the second
	! line.
	! in &0-&Char&input file or list of input files. If no files are given, data is read from the following input
	! paragraph. If either of read-> or in-> option is given, then both options must
	! be present.
	! form &-1|1&Char& Format of the data as follows \newline
	! $  Fortran format '*', the default \newline
	! b    Single precison binary \newline
	!bs  Single precison binary opened with access='stream'
	! Needed for Pascal files in Windows. \newline
	! B  Double precison binary.\newline
	! Char giving a Fortran format, e.g. '(4f4.1,1x,f4.3)' \newline
	! d4 Single precison direct access for Gfortran files.\newline
	! d1 Single precison direct acces for Intel Fortran files.
	! maketrans&-1|1&TRANS &Transformations computed for each observation when reading the data
 
 
	! keep&-1|1-&REAL& variables kept in the data object, default: all read-> variables plus the output
	! variables of maketrans-> transformations.
 
 
 
	! filter&-1|1&Code& logical or arithmetic statement (nonzero value indicating True) describing which
	! observations will be accepted to the data object. maketrans->-transformations are
	! computed before using filter. Option filter-> can utilize automatically created
	! variable Record which tells which input record has been just read. If observations
	! are rejected, then the Obs-variable has as its value number of already accepted
	! observations+1.
 
	! reject &-1|1&Code& Logical or arithmetic statement (nonzero value indicating True) describing which
	! observations will be rejected from the data object. If filter-> option is given then
	! reject statement is checked for observations which have passed the filter. Option
	! reject-> can utilize automatically created variable Record which tells which
	! input record has been just read. If observations are rejected, then the Obsvariable has as its value number of already accepted observations+1.
	! subdata the name of the lower level data object to be created. This option is not allowed, if
	! there are multiple input files defined in option in->.
 
	! keepopen &-1:1&REAL& This option tells that if nobs-> option gives the number of observations
	!then after the current DATA is read in, the file remains open so that other DATAs can be read from the same file.
	!This option is useful when storing schdules having the tree structure and unpacking the structure
	! with joindata(). See also splitdata().
 
	! continue  &-1:1&REAL& This tells that the reading continues from
	! the file which was left open with the previous keepopen-> option.
 
 
	!Latex
	! subread,…,subobs sub data options similar as read->…obs-> for the upper level data.
	! (subform->'bgaya' is the format for the Gaya system). The following options
	! can be used only if subdata-> is present
	!endlatex
 
 
	! nobswcum&-1|1&REAL& A variable telling the cumulative number of subdata observations up to the
	! current upper data observation but not including it. This is useful when accessing
	! the data matrix one upper level unit by time, i.e., the observation numbers within
	! upper level observation are nobswcum+1,…,nobswcum+nobsw
 
 
	! duplicate&-1|2& TRANS &
	! The two transformation object arguments describe how observations in the subdata
	! will be duplicated. The first transformation object should have Duplicates as an
	! output variable so that the value of Duplicates tells how many duplicates ar
	! made (0= no duplication). The second transformation object defines how the values
	! of subdata variables are determined for each duplicate. The number of duplicate
	! is transmitted to the variable Duplicate. These transformations are called also
	! when Duplicate=0. This means that when there is the duplicate-> option,
	! then all transformations for the subdata can be defined in the duplicate
	! transformation object, and submaketrans-> is not necessary.
 
	! oldsubobs&-1|1& REAL& If there are duplications of sub-observations, then this option gives the variable
	! into which the original observation number is put. This can be stored in the
	! subdata by putting it into subkeep-> list, or, if subkeep-> option is not given
	! then this variable is automatically put into the keep-> list of the subdata.
 
	! oldobsw&-1|1&REAL& This works similarly with respect to the initial obsw variable as oldsubobs->
	! works for initial obs variable.
 
	! nobs&-1|1&Real& There are two uses of this option. First, a data object can be created without reading
	! from a file or from the following input paragraph by using nobs-> option and
	! maketrans-> transformation, which can use Obs variable as argument. Creation
	! of data object this way is indicated by the presence of nobs-> option and absence
	! of in-> and read-> options. Second, if read-> option is present nobs->
	! option can be used to indicate how many records are read from a file and what
	! will be the number of observations. Currently reject-> or filter-> can not
	! be used to reject records (consult authors if this would be needed). If there are
	! fewer records in file as given in nobs-> option, an error occurs. There are three
	! reasons for using nobs-> option this way. First, one can read a small sample
	! from a large file for testing purposes. Second, the reading is slightly faster as the
	! data can be read directly into proper memory area without using linked buffers.
	! Third, if the data file is so large that a virtual memory overflow occurs, then it may
	! be possible to read data in as linked buffers are not needed.
	! In case nobs-> option is present and read-> option is absent either
	! maketrans-> or keep-> option (or both) is required.
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
 
	! rfhead &-1|0& & When reading data from a text file, the first line can contain a header which is
	!printed but othewise ignored
 
	! rfcode &-1|0& &The data file can contain also Jlp22-code which is first executed. Note the code can
	! be like var1,var,x1...x5=1,2,3,4,5,6,7, which give the possibility to
	! define variables which describe the in-> file.
 
 
	! rfsubhead-> works for subdata similarly as rfhead-> for data.
	! rfsubcode works for subdata similarly as rfcode-> for data
 
	! If there are both rfhead-> and rfcode-> then rfhead-> is excuted first.
	! rfhead-> and rfcode-> replace readfirst-> option  of previous versions which was too
	! complicated.
	!time&-1|0&& If time-> is present, the cpu-time and total time in function are printed
 
	!endoption
 
	! Note  data() function will create a data object object, which is a compound object consisting
	! of links to data matrix, etc. see Data object object. If Data is the output
	! of the function, the function creates the list Data%keep telling the
	! variables in the data and
	! Data%matrix containg the data as a single precision matrix. The number of observations can be obtained by nobs(Data) or by
	! nrows(Data%matrix).
	!endnote
	! Note See common options section for how data objects used in other Jlp22 functions will be defined.
	!endnote
	! Note The in-> and subin-> can refer to the same file, or if both are without arguments
	! then data are in the following input paragraph. In this case data() function read first one
	! upper level record and then nobsw-> lower level records.
	!endnote
	!Note In ealier versions, the user could select teh name for the variable telling the number
	!	of the observation. After version Dec 20 2022, the name for DATA datab is always datab%obs.
	!endnote
	! Note  When reading the data the  can be used in maketrans-
	! > transformation and in reject-> option and filter-> option, and the variable dtab%obs refers to
	! the number of observation in resulting data object. The variable Record gets the number of
	! the read record in the input file, and can be used in maketrans-> transformations and in
	! reject-> and filter-> options. If subdata-> option is given, variable Subreject gets
	! the number of record in the sub file, and it can be used in submaketrans-> transformations
	! and in subreject-> option and in subfilter-> option.
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
	!Note After publishing splitdata() and joindata() functions I hope to get rid of the possibility to
	!read both the upper level and sublevel data from the same file.
	!endnote
 
	!Note Earlier versions contained trans-> and subtrans->options which associated
	! a permanent transformation object with the data object. This feature is now deleted because
	! it may confuse and is not really needed. If tranformations are needed in functions
	! they can always be included using trans-> .
	!endnote
	!Ex dataex data() generates a new data object by reading data.
	!data1=data(read->(x1...x3),in->)
	!1,2,3
	!4,5,6
	!7,8,9
	!/
	!endex
	!endsection
 
	!	parameter(iformstar = 1)  ! * or $ format
	!	parameter(iformb = 2)   ! 'b' = binary
	parameter(iformfortran = 43)  !fortran format
	!	parameter(iformin = 4)   !read directly from the paragraph
	!	parameter(iformo1 = 6)
	!	parameter(iformo2 = 7)
	!	parameter(iformo3 = 8)
	parameter(iformd = 49) !'d' =direct access singel
	parameter(iformd2 = 50) !'d' double
	!		parameter(iformgaya = 45)
	!integer, dimension(:),pointer::keepl=>null(),keepl2=>null()
	type(j_linkr),pointer ::head=>null(),tail=>null(),ptr=>null()
	type(j_linkr),pointer ::head2=>null(),tail2=>null(),ptr2=>null()   !subdata
	logical dupl
	integer :: lr_
 
	logical ::p
	!neede only in sub
 
	integer nnobswcum
	integer, dimension(:), pointer :: obswcum
	integer nobsw
	integer, dimension(:), pointer :: obsw
	integer noldsubobs
	integer, dimension(:), pointer :: oldsubobs
	integer noldobsw
	integer, dimension(:), pointer :: oldobsw
 
 
 
	!type(j_datatype),dimension(2)::j_d
	integer,dimension(2)::lread=(/j_mread,j_msubread/)
	!	integer,dimension(2)::lform=(/j_mform,j_msubform/)
	integer,dimension(2)::lmaketrans=(/j_mmaketrans,j_msubmaketrans/)
	!integer,dimension(2)::lreadfirst=(/j_mreadfirst,j_msubreadfirst/)
	integer,dimension(2)::ltrans=(/j_mtrans,j_msubtrans/)
	integer,dimension(2)::lkeep=(/j_mkeep,j_msubkeep/)
	integer,dimension(2)::lobs=(/j_mobs,j_msubobs/)
	integer,dimension(2)::lfilter=(/j_mfilter,j_msubfilter/)
	integer,dimension(2)::lreject=(/j_mreject,j_msubreject/)
	integer,dimension(2)::lrfhead=(/j_mrfhead,j_msubrfhead/)
	integer,dimension(2)::lrfcode=(/j_mrfcode,j_msubrfcode/)
	integer,dimension(2)::rfcodelink,rfheadlink,rfreadlink
	!	integer,dimension(2)::lin=(/j_min,j_msubin/)
	integer,dimension(2)::lnobs=(/j_mnobs,j_msubnobs/)
	integer,dimension(2)::lextra=(/j_mextra,j_msubextra/)
	character*100,dimension(2):: form
	integer,dimension(2)::ivkeep,ivmat,ivmaketrans,iout,ivobs
	integer*8,dimension(2)::iobs,iba,nobs,iobc,nkeep
	integer,dimension(2)::ivnobs,filterlink,rejectlink,nu,lenf,irecord,nbuf,nrejected,iform
	integer,dimension(2)::lenform
	logical,dimension(2)::ismaketrans,isnobs,isfilter,isreject,isincl,single,iskeep,vff !var from file
	!double precision,allocatable,dimension(:)::readvec
	logical ::cyclerobs,goto51,istime
	logical::sub,isgaya
	logical::keepopen,continue
	save
	!logical isinpu
	p=j_v(j_ivdollar).eq.278
	if(p)write(6,*)'<221,iob,io',iob,io,j_o(iob)%i(io:io+10)
	nullify(head)
	nullify(tail)
	nullify(ptr)
 
 
	iout=0
	single=.false.
	call j_startfunction(iob,io,0,narg,j_optarg0,iout(1))
 
	keepopen=j_isopt(iob,io,j_mkeepopen,.false.)
	continue=j_isopt(iob,io,j_mcontinue,.false.)
 
 
	if(j_err)goto 51
	istime=j_isopt(iob,io,j_mtime)
	if(istime)then
		call cpu_time(cpu0)
		time0=secnds(0.)
	endif !if(istime)  18648
	ibas=0
	iobs=0
	!	iobc=0
	!isinpu=j_isopt(iob,io,j_minpu)
	!	write(6,*)'isinpu0',isinpu
	irecord=0
	iobc=0  !number of obs in current buffer
	nbuf=0
	nrejected=0
	iba=0
	time=0.
	iobs=0
	isgaya=.false.
	isnobs=.false.
	iskeep=.false.
	ismaketrans=.false.
	isfilter=.false.
	isreject=.false.
	!write(6,*)'<777inli',j_linkoption(iob,io,j_min)
	ivnobsw=0
	ivnobswcum=0
	!	if(p)write(6,*)'<221999,iob,io',iob,io
	!	write(6,*)'<221uu,iob,io',iob,io
 
	!isgaya=j_linkoption(iob,io,j_mgaya).ge.0
	!write(6,*)'<88888 ',isgaya
	call initdata(1)
	if(j_err)return
	!*********************
	call j_getname(j_d(1)%readv(1))
 
	if(j_oname(1:j_loname).eq.'%nobsw')then
		call j_getobject(iout(1),'%nobsw',j_ipreal,ivnobsw)
		j_d(1)%readv(1)=ivnobsw
		call j_getname(-1,ivnobsw)
		write(6,*)'%nobsw changed into ',j_oname2(1:j_loname2)
		j_o(ivkeep(1))%i2(1)=ivnobsw
	endif !if(j_oname(1:j_loname).eq.'%nobsw')  18683
	if(j_err)return
	!	write(6,*)'<666initdta doen',isfilter,isreject
	lisu=j_linkoption(iob,io,j_msubdata)
	sub=lisu.gt.0
	if(sub)then
		iout(2)=j_o(iob)%i(lisu+1)
		!	write(6,*)'<4747lisu,iout(2) ',lisu,iout(2),j_o(iob)%i(lisu-2:lisu+10)
		call initdata(2)
		if(j_err)return
		call j_getname(j_d(2)%readv(1))
		if(j_oname(1:j_loname).eq.'%nobsw')call j_getobject(iout(2),'%nobsw',j_ipreal,j_d(2)%readv(1))
 
 
		isgaya=iform(2).eq.j_ivbgaya
		!	write(6,*)'<464664>',iform(2),isgaya,j_ivbgaya
		if(j_err)goto 51
	endif !if(sub)  18694
 
	! endif !if(jdataform_c(1:1).eq.'*')then
	call j_getoption_index(iob,io,j_mbuffersize,-1,1,j_ipreal,&
		.true.,noptarg,j_optarg0);if(j_err)return
 
	ivdefn=j_optarg0(1) !  igetopt(iob,io,mbuffersize)
	if(ivdefn.gt.0)then
		nobsb=j_v(ivdefn)
	else !if(ivdefn.gt.0)then
		nobsb=10000
	end if !if(ivdefn.gt.0)  18713
 
 
	!iobcur_=iob  !!used to compute filter and reject
 
 
 
	!!!!!!!!!!!!!!!!duplication
	if(.not.isnobs(1))then
		if(.not.associated(head))allocate (head)
		tail=>head ;nullify(tail%pnext)
		!write(6,*)'<87>',nobsb,nvar,nobsb*nvar
		allocate(tail%rbuf(1:nobsb*nkeep(1)))
		ivdefn=j_igetopt(iob,io,j_mbuffersize)
		if(ivdefn.gt.0)then
			nobsb=j_v(ivdefn)
		else !if(ivdefn.gt.0)then
			nobsb=10000
		end if !if(ivdefn.gt.0)  18731
	endif !if(.not.isnobs(1))  18725
 
	if(sub)then
		if(p)write(6,*)'>447 hep'
		nlev=2
		if(j_linkoption(iob,io,j_mduplicate).gt.0)then
			itduplicates=j_o(iob)%i(j_linkoption(iob,io,j_mduplicate)+1)
			itdupl=j_o(iob)%i(j_linkoption(iob,io,j_mduplicate)+2)
			if(j_o(iob)%i(j_linkoption(iob,io,j_mduplicate)).ne.2.or.&
				.not.j_istrans(itduplicates).or.&
					.not.j_istrans(itdupl ))then
				write(6,*)'** duplicate->(trans_#_of_duplicates,trans_for_treating)'
				j_err=.true.;return
			end if !if(j_o(iob)%i(j_linkoption(iob,io,j_mduplicate)).ne.2.  18744
			ivndupl=j_object('Duplicates') !tsekaksuonko output mutujissa
			if(ivndupl.le.0)then
				write(6,*)'**Duplicates -variable not defined'
				j_err=.true. ;return
			end if !if(ivndupl.le.0)  18751
			dupl=.true.
		else !if(j_linkoption(iob,io,j_mduplicate).gt.0)then
			dupl=.false.
		endif !if(j_linkoption(iob,io,j_mduplicate).gt.0)  18741
 
		call j_getoption_index(iob,io,j_mnobswcum,-1,1,j_ipreal,&
			.true.,noptarg,j_optarg0);if(j_err)return
		ivnobswcum=j_optarg0(1) !  igetopt(iob,io,mnobswcum)
		!needed even if no subdata
 
		!!!!!!!! subdata
		if(p)write(6,*)'<40 ',j_msubdata, j_linkoption(iob,io,j_msubdata)
 
 
 
		call j_getoption_index(iob,io,j_moldsubobs,-1,1,j_ipreal,&
			.true.,noptarg,j_optarg0);if(j_err)return
		ivoldsubobs=j_optarg0(1)  !  igetopt(iob,io,moldsubobs)
		call j_getoption_index(iob,io,j_moldobsw,-1,1,j_ipreal,&
			.true.,noptarg,j_optarg0);if(j_err)return
		ivoldobsw=j_optarg0(1)  ! igetopt(iob,io,moldobsw)
 
		call j_getname(iout(2))
 
		call j_getobject(0,j_oname(1:loname)//'%obs',j_ipreal,ivsubobs)! ivdebug) !!!
		ivsubo=j_igetopt(iob,io,j_msubobs)
		if(ivsubo.gt.0)then
			write(6,*)'*subobs-> is deleted option, obs variable will be ',j_oname(1:j_loname)
 
		endif !if(ivsubo.gt.0)  18781
		!	if(ivsubobs.le.0)ivsubobs=j_ivobs
 
 
 
		call j_getoption_index(iob,io,j_msubform,-1,100,j_ipchar,.true.,noptarg,j_optarg0)
		if(j_err)return
		ivnobsw=j_igetopt(iob,io,j_mnobsw)
		if(ivnobsw.le.0)then
			write(6,*)'**nobsw missing'
			j_err=.true.;return
		end if !if(ivnobsw.le.0)  18792
		!		maxnobsw=0 later during
		!	ivobsw=j_igetopt(iob,io,j_mobsw)
		!	if(ivobsw.le.0)then
		call j_getobject(iout(2),'%obsw',j_ipreal,ivobsw)
		!	end if !if(ivobsw.le.0)  18170
 
		if(.not.isnobs(2))then
			nullify(head2)
			nullify(tail2)
			nullify(ptr2)
			if(.not.associated(head2))allocate (head2)
			tail2=>head2 ;nullify(tail2%pnext)
			!write(6,*)'<87>',nobsb,nvar,nobsb*nvar
			allocate(tail2%rbuf(1:nobsb*nkeep(2)))
 
		endif !if(.not.isnobs(2))  18802
	else
		nlev=1
 
 
	endif !if(sub)  18738
 
 
	!	write(6,*)'<65656',rfcodelink,rfheadlink,rfreadlink
	do id=1,nlev
		!options are processed from right to left
		! rfcode and rfhead and read-$ar proceesed in the order of appearance
 
459		if(rfcodelink(id).gt.0.and.rfcodelink(id).gt.rfheadlink(id).and.rfcodelink(id).gt.rfreadlink(id))then
			if(iform(id).eq.iformd.or.iform(id).eq.iformb)then
				if(id.eq.1)write(6,*)'* rfcode does not work with binary files'
				if(id.eq.2)write(6,*)'* rfsubcode does not work with binary files'
				j_err=.true.;return
			endif !if(iform(id).eq.iformd.or.iform(id).eq.iformb)  18825
			read(nu(id),'(a)')j_tempchar2
			leco=len_trim(j_tempchar2)
			call j_clean(j_tempchar2,leco)
			!	write(6,*)'<33code',j_tempchar2(1:leco)
			call j_command(j_tempchar2(1:leco))
			if(j_err)then
				write(6,*)'error was in rfcode:',j_tempchar2(1:leco)
				return
			endif !if(j_err)  18835
			rfcodelink(id)=0
 
		endif !459		if(rfcodelink(id).gt.0.and.rfcodelink(id).gt.rfheadli  18824
 
		if(rfheadlink(id).gt.0.and.rfheadlink(id).gt.rfcodelink(id).and. &
				rfheadlink(id).gt.rfreadlink(id))then
			if(iform(id).eq.iformd.or.iform(id).eq.iformb)then
				if(id.eq.1)write(6,*)'* rfhead does not work with binary files'
				if(id.eq.2)write(6,*)'* rfsubhead does not work with binary files'
				j_err=.true.;return
			endif !if(iform(id).eq.iformd.or.iform(id).eq.iformb)  18845
			read(nu(id),'(a)')j_tempchar2
			leco=len_trim(j_tempchar2)
			if(id.eq.1)write(6,*)'header in file:',j_tempchar2(1:leco)
			if(id.eq.2)write(6,*)'header in subin file:',j_tempchar2(1:leco)
			rfheadlink(id)=0
			goto 459
		endif !if(rfheadlink(id).gt.0.and.rfheadlink(id).gt.rfcodelink(id  18843
 
 
 
		if(iform(id).eq.j_ivb.or.iform(id).eq.j_ivdi.or.iform(id).eq.j_ivdg.or.&
				iform(id).eq.j_ivbgaya)then
			if(allocated(j_d(id)%readvecsing))deallocate(j_d(id)%readvecsing)
			!		write(6,*)'<466464774474 ',j_nread(id)
			allocate(j_d(id)%readvecsing(1:j_nread(id)))
			!		write(6,*)'<4665555 ',j_nread(id)
		else !if(iform(id).eq.j_ivb.or.iform(id).eq.j_ivdi.or.iform(id).eq.j_ivdg)then
			if(allocated(j_d(id)%readvec))deallocate(j_d(id)%readvec)
			allocate(j_d(id)%readvec(1:j_nread(id)))
		endif !if(iform(id).eq.j_ivb.or.iform(id).eq.j_ivdi.or.iform(id).  18860
 
 
 
 
	enddo !id=1,nlev  18820
 
	if(isgaya)then!	ivarea=j_data_read_(2)
		!	write(6,*)'>&&&&&&&&&'
		ivarea=j_d(1)%readv(2)
		!		write(6,*)'<633336'
		call j_gayainit(iob,io) !uses par->
		!iform2=iform2bgaya
		call j_printname('gaya: area variable is ',ivarea,' ')
	endif !if(isgaya)  18876
	call j_clearoption(iob,io)  ! subroutine
 
	if(p)write(6,*)'<555rfcode nu',nu,' nobs ',nobs,'isnobs ',isnobs,' iout ',iout ,' nkeep ',nkeep
 
 
 
	if(isnobs(1))ivmat(1)=j_defmatrix8(iout(1),'%matrix',nobs(1),nkeep(1),j_matreg)
	if(isnobs(2))ivmat(2)=j_defmatrix8(iout(2),'%matrix',nobs(2),nkeep(2),j_matreg)
 
	if(.not.sub.and.isnobs(1).and..not.ismaketrans(1).and..not.iskeep(1))then
		!fast reading
		ibas=0
		!		write(6,*)'isnobs,nobs(1),iform(1),j_ivdollar',isnobs,nobs(1),iform(1),j_ivdollar
		if(iform(1).eq.j_ivb)then
 
			do i=1,nobs(1)
				read(nu(1),err=957,end=957)j_d(1)%readvecsing  !(1:nkeep(1))
				j_o(ivmat(1))%d(ibas+1:ibas+nkeep(1))=j_d(1)%readvecsing  !(1:nkeep(1))
				ibas=ibas+nkeep(1)
 
			enddo !i=1,nobs(1)  18899
		elseif(iform(1).eq.j_ivb2)then
 
			do i=1,nobs(1)
				read(nu(1),err=957,end=957)j_o(ivmat(1))%d(ibas+1:ibas+nkeep(1))
				ibas=ibas+nkeep(1)
 
			enddo !i=1,nobs(1)  18907
		else if(iform(1).eq.j_ivdollar)then
			! if(isinpu)then
			! do i=1,nobs(1)
			! call j_getinput('sit>',inprint)
			! read(j_inp(1:j_linp),*,err=956,end=956)j_o(ivmat(1))%d(ibas+1:ibas+nkeep(1))
 
 
			! enddo !i=1,nobs(1)  17621
 
			! else
			do i=1,nobs(1)
				read(nu(1),*,err=956,end=956)j_o(ivmat(1))%d(ibas+1:ibas+nkeep(1))
				ibas=ibas+nkeep(1)
 
			enddo !i=1,nobs(1)  18922
			!	endif !if(isinpu)  17620
		else !update later
			goto 1952
 
 
		endif !if(iform(1).eq.j_ivb)  18897
		if(.not.keepopen)call j_closeunit(nu(1))
		iobs(1)=nobs(1)
		write(6,*)'iobs(1)'
		goto 7000
	956 write(6,*)'*error/end reading text in-> file after reading ',i-1,' observations'
		goto 7000
	957		write(6,*)'*error reading binary in-> file after reading ',i-1,' observations'
		goto 7000
	endif !if(.not.sub.and.isnobs(1).and..not.ismaketrans(1).and..not  18893
 
	!if(idiskin.ne.0)iobs=nobdisk
 
 
 
	!	333 format(a)   ! format(Q,a)
 
	if(p)write(6,*)'<5555iform',iform,sub
1952		maxnobsw=0
	!!!main reading loop in one file
		robs: do while(iobs(1).lt.nobs(1))
		cyclerobs=.false.
		goto51=.false.
		if(p)write(6,*)'bef readobs(1)'
		call readobs(1)
		if(p)write(6,*)'aftt readobs(1) goto51,cyclerobs,iobc(1)',goto51,cyclerobs,j_err,iobc(1)
		if(goto51)goto 51
		if(j_err)goto 51
		if(cyclerobs)cycle robs
 
		iobc(1)=iobc(1)+1
		!write(6,*)'>221iobc',iobc
		if(iobc(1).gt.nobsb)then !store in chained list
			if(p)write(6,*)'>22iobc',iobc,associated(tail%pnext),associated(tail%pnext)
			if(associated(tail%pnext))then
				!		write(6,*)'allo:',allocated(tail%pnext)
				deallocate(tail%pnext)
			endif !if(associated(tail%pnext))  18966
			if(.not.associated(tail%pnext))allocate(tail%pnext)
			if(p)write(6,*)'>22iobsssc',nobsb,nkeep(1)
			tail=>tail%pnext ;nullify(tail%pnext)
			if(p)write(6,*)'>22iobsssc',nobsb,nkeep(1)
			allocate(tail%rbuf(1:nobsb*nkeep(1)))
			iobc(1)=1
			iba(1)=0
			nbuf(1)=nbuf(1)+1
			if(p)write(6,*)'>22iohere ',nbuf(1)
		end if !if(iobc(1).gt.nobsb)  18964
		100 continue

			! if(.not.associated(head))allocate (head)
			! tail=>head ;nullify(tail%pnext)

		!	allocate(tail%rbuf(1:nobsb*nkeep(1)))


		!!if(p)write(6,*)'subread ',subread
		if(sub)then
			nsub=j_v(ivnobsw)  !initial
			nsub2=nsub ! taking duplications into account
			j_v(ivobsw)=j_0
			iobsw=0  ! used if duplicates
			!	write(6,*)'<6363startsub nsub,',nsub,ivnobsw
			do ii=1,nsub           !nsub=v(ivnobsw)
				if(p)write(6,*)'*****ii ',ii,' duupl ',dupl
				call readobs(2)
				if(j_err)goto 51
 
				if(dupl)then
					j_v(ivobs)=iobs(1)
					j_v(ivndupl)=0.
					if(ivoldobsw.gt.0)then;iobsw=iobsw+1;j_v(ivoldobsw)=iobsw;end if
					if(p)write(6,*)'****itduplica ',itduplicates,ivndupl,j_ivduplicate,ivobsw,itdupl
					call dotrans(itduplicates,1)
					if(j_err)goto 51
					ndup=j_v(ivndupl)
					if(ndup.lt.0)then
						write(6,*)'*negative Duplicates',ndup
						j_err=.true. ;goto 51
					endif !if(ndup.lt.0)  19008
					j_v(j_ivduplicate)=j_0
					j_v(ivobsw)=iobc(2)
					j_v(ivsubobs)=iobs(2)
					call dotrans(itdupl,1)
					if(j_err)goto 51
				endif !if(dupl)  19000
				iobc(2)=iobc(2)+1
				if(iobc(2).gt.nobsb)then !store in chained list
					if(p)write(6,*)'>22iobc2',iobc,associated(tail%pnext),associated(tail%pnext)
					if(associated(tail2%pnext))then
						!		write(6,*)'allo:',allocated(tail%pnext)
						deallocate(tail2%pnext)
					endif !if(associated(tail2%pnext))  19021
					if(.not.associated(tail2%pnext))allocate(tail2%pnext)
					if(p)write(6,*)'>22iobsssc',nobsb,nkeep(2)
					tail2=>tail2%pnext ;nullify(tail2%pnext)
					if(p)write(6,*)'>22iobsssc',nobsb,nkeep(2)
					allocate(tail2%rbuf(1:nobsb*nkeep(2)))
					iobc(2)=1
					iba(2)=0
					nbuf(2)=nbuf(2)+1
					if(p)write(6,*)'>22iohere ',nbuf(1)
				end if !if(iobc(2).gt.nobsb)  19019
				!if(idisk2.eq.0)then
				if(p)write(6,*)'<8888iba,nkeep',iba(2),nkeep(2)
				tail2%rbuf(iba(2)+1:iba(2)+nkeep(2))=j_v(j_o(ivkeep(2))%i2(1:nkeep(2))) !keepl2(1:nvar2))
				!else
				!write(6,*)'>55',iobs2,nvar2,idisk2
				!if(associated(keepl2))write(6,*)'k2',keepl2
				!write(idisk2,rec=iobs2)j_v(keepl2(1:nvar2))
				!endif
 
				iba(2)=iba(2)+nkeep(2)
				if(dupl)then
					do idu=1,ndup
						j_v(j_ivduplicate)=idu
						call dotrans(itdupl,1)
						if(j_err)goto 51
						iobs(2)=iobs(2)+1
						iobc(2)=iobc(2)+1
						if(iobc(2).gt.nobsb)call more2()
						!if(idisk2.eq.0)then
						tail2%rbuf(iba(2)+1:iba(2)+nkeep(2))=j_v(j_o(ivkeep(2))%i2(1:nkeep(2))) !j_v(keepl2(1:nvar2))
						iba(2)=iba(2)+nkeep(2)
						!	else
 
						!	write(idisk2,rec=iobs2)j_v(keepl2(1:nvar2))
						!	endif
					enddo !idu=1,ndup  19046
 
					nsub2=nsub2+ndup
				endif !if(dupl)  19045
 
			end do !ii=1,nsub  18995
			if(dupl)j_v(ivnobsw)=nsub2
			maxnobsw=max(maxnobsw,nsub2)
			if(ivnobswcum.gt.0)j_v(ivnobswcum)=iobs2
		end if !if(sub)  18989
 
		if(.not.isnobs(1)) then
			!write(6,*)'<47 idisk',idisk
			!if(idisk.eq.0)then
			tail%rbuf(iba(1)+1:iba(1)+nkeep(1))=j_v(j_o(ivkeep(1))%i2(1:nkeep(1)))  !j_v(keepl(1:nvar))
			iba(1)=iba(1)+nkeep(1)
			! else
			!write(idisk,rec=iobs)j_v(keepl(1:nvar))
			! endif
		endif !if(.not.isnobs(1))  19071
		if(isnobs(1).and.irecord(1).ge.nobs(1)) exit robs  !
		if(p)write(6,*)'<34>goto50,isnobs,nobs,irecord',isnobs,nobs,irecord
 
	end do robs !s: do while(iobs(1).lt.nobs(1))  18952
 
 
	!		write(6,*)'<56nu',nu(1),nu(2)
	51  if(.not.isincl(1).and.j_ninc.gt.1.and..not.keepopen)	call j_closeunit(nu(1))  !end=51 in reading
	!	write(6,*)'<aau',nu(1),nu(2)
!	write(6,*)'25nu,iform,iformin',nu,iform,iformin
	if(sub.and.nu(2).ne.5.and.nu(2).ne.nu(1).and..not.isincl(2))call j_closeunit(nu(2))
	!	end do floop !floop:do iargf_=1,max(nin_,1)
 
	iobs(1)=iobs(1)-nrejected(1)
	if(p)write(6,*)'iobs',iobs(1)
7000		continue
	!600 	nob(1)=iobs(1)   !!!!! clean
	!	if(allocated(j_d(1)%readv))deallocate(j_d(1)%readv)
	if(allocated(j_d(1)%readvec))deallocate(j_d(1)%readvec)
	!		if(allocated(j_d(2)%readv))deallocate(j_d(2)%readv)
	if(allocated(j_d(2)%readvecsing))deallocate(j_d(2)%readvecsing)
	if(isreject(1).or.isfilter(1))write(6,*)'*number of rejected observations ',nrejected(1)
	if(nbuf(1).gt.0.and..not.isnobs(1))write(6,*)'number of data buffers ',nbuf(1)+1, ' (faster with nobs->)'
	if(iobs(1).le.0)then
		write(6,*)'*no observations, data not created'
		j_err=.true.
	endif !if(iobs(1).le.0)  19103
	if(j_err)call j_del(iout(1))
	!	if(iform.ne.4.and.iform.ne.6)call j_closeunit(nu) !###o###?
	if(sub)then
		write(6,*)'number of subobs: ',iobs(2),' keep% -vars:',nkeep(2)  !keepl2(0)
		if(.not.isnobs(2))write(6,*)'number of subdata buffers ',nbuf(2)+1
		if(j_err)call j_del(iout(2))
		!write(6,*)'<33clsing ',nu2
		!if(nu2.ne.nu.and.iform2.lt.iformo1_)call j_closeunit(nu2)
		!write(6,*)'<33closed ',nu2
	end if !if(sub)  19109
 
 
	if(.not.isnobs(1))then
		if(p)write(6,*)'iobs(1),nkeep(1) ',iobs(1),nkeep(1),iout(1)
		if(.not.j_err)ivmat(1)=j_defmatrix8(iout(1),'%matrix',iobs(1),nkeep(1),j_matreg)
		!if(idisk.gt.0)goto 800
		if(p)write(6,*)'ivmat(1) ',ivmat(1)
 
		ptr=>head
		ibaa=0
		do i=1,nbuf(1)
			!	write(6,*)'<77ibuf',i,nobsb,iobc
			if(.not.j_err)j_o(ivmat(1))%d(ibaa+1:ibaa+nobsb*nkeep(1))=ptr%rbuf
			!		write(6,*)'<772',i,nobsb,iobc
			deallocate(ptr%rbuf)
			!		write(6,*)'<772',i,nobsb,iobc
			nullify(ptr%rbuf)
			!		write(6,*)'<774f',i,nobsb,iobc
			ptr=>ptr%pnext
			!	write(6,*)associated(ptr),associated(head),associated(tail)
			!	write(6,*)'<775',i,nobsb,iobc
			ibaa=ibaa+nobsb*nkeep(1)
			!		write(6,*)'<776',i,nobsb,iobc
		end do !i=1,nbuf(1)  19127
		if(.not.j_err)j_o(ivmat(1))%d(ibaa+1:ibaa+iobc(1)*nkeep(1))=ptr%rbuf(1:iobc(1)*nkeep(1))
		!	write(6,*)'<77ahahha'
		deallocate(ptr%rbuf);nullify(ptr%rbuf)
		tail=>head
		do i=1,nbuf(1)
			!	write(6,*)'<77ahbba'
			ptr=>tail%pnext
			nullify(tail%pnext)
			tail=>ptr
		end do !i=1,nbuf(1)  19145
	endif !if(.not.isnobs(1))  19119
 
	!write(6,*)'<787e7iobs(1)',iobs(1),nrejected(1)
 
	if(isnobs(1).and.nobs(1)>iobs(1))then
		if(iobs(1).eq.0)then
			write(6,*)'*no observations, data not created'
			j_err=.true.
			call j_del(ivmat(1))
			if(sub)call j_del(ivmat(2))
			goto 950
		else
			write(6,*)'*number of observations ',iobs(1),' is less than given in nobs-> ',nobs(1)
			write(6,*)'the data matrix has now ',(nobs(1)-iobs(1)), ' extra elements'
			j_o(ivmat(1))%i(1)=iobs(1)
			j_o(ivmat(1))%i(3)=iobs(1)*nkeep(1)
		endif !if(iobs(1).eq.0)  19156
 
	endif !if(isnobs(1).and.nobs(1)>iobs(1))  19155
 
	if(.not.j_err)call j_defdata(iout(1),ivmat(1),ivkeep(1))   !ivout2 for mother
	!	write(6,*)'<466464lopus',ivmat,ivkeep,j_o(ivkeep(1))%i
	!	write(6,*)'*####',j_o(ivkeep(1))%i2,'%%%',j_o(ivmat(1))%i,'*&&&',j_o(ivmat(1))%d
	!	write(6,*)ivnobsw,ivobs(1)
	if(sub)j_o(iout(1))%i(9)=maxnobsw   !note it is stored here
	if(.not.j_err)then
		j_dlastdata=iout(1)
		j_v(j_ivlastdata)=iout(1)
	else
		if(j_dlastdata.eq.iout(1))then
			j_dlastdata=0
			j_dlastdata=0
		endif !if(j_dlastdata.eq.iout(1))  19180
	endif !if(.not.j_err)  19176
	!	j_o(j_ivlastdata)%i(1)=1;j_o(j_ivlastdata)%i2(1)=iout(1)
 
	if(isgaya)then
		write(6,*)'gayatime=',time
		call timegaya()
	end if !if(isgaya)  19187
	write(6,*)'number of observations ',iobs(1),' keep variables ', nkeep(1)
	!	write(6,*)'<1'
	if(sub)then
		nobs(2)=iobs(2)-nrejected(2)
		!if data is on disk mattype2 =-idiskin2 or -idiksk1
		if(.not.j_err)ivmat(2)=j_defmatrix8(iout(2),'%matrix',nobs(2),nkeep(2),j_matreg)
		!		write(6,*)'<373737 ',j_err,iout(2),ivmat(2)
		!	write(6,*)'<2',idisk2,idiskin2,nobs2,nobs2f
 
		!write(6,*)'<3'
		ptr2=>head2
		iba2=0
		do i=1,nbuf(2)
			j_o(ivmat(2))%d(iba2+1:iba2+nobsb*nkeep(2))=ptr2%rbuf
			deallocate(ptr2%rbuf);nullify(ptr2%rbuf)
			ptr2=>ptr2%pnext
			iba2=iba2+nobsb*nkeep(2)
		end do !i=1,nbuf(2)  19203
		j_o(ivmat(2))%d(iba2+1:iba2+iobc(2)*nkeep(2))=ptr2%rbuf(1:iobc(2)*nkeep(2))
		deallocate(ptr2%rbuf);nullify(ptr2%rbuf)
		tail2=>head2
		do i=1,nbuf(2)
			ptr2=>tail2%pnext
			nullify(tail2%pnext)
			tail2=>ptr2
		end do !i=1,nbuf(2)  19212
 
 
		if(.not.j_err)call j_defdata(iout(2),ivmat(2),ivkeep(2))
 
		!call j_getname(iout(2))
		!write(6,*)'<94949494',iout(2),j_oname(1:j_loname)
		!	 j_defdata(iv,ivmat,ivkeep,ivsub,ivnobsw,ivup,ivobs,ivobsw,ivnobswcum)
 
	end if !if(sub)  19193
	if(istime)then
		call cpu_time(cpu1)
		time1=secnds(time0)
		write(6,*)'data() used cpu ',cpu1-cpu0,' s and total time ',time1,' s'
	endif !if(istime)  19226
	write(6,*)' '
	return
 
 
 
	950 write(6,*)'**data error reading data at level ',id ,' after reading ',iobs(id), 'observations'

	j_err=.true.
 
	if(iform(1).ne.iformin.and.iform(1).le.iformo1)call j_closeunit(nu(1)) !###o###
	if(sub)then
		if(iform(2).ne.iformin.and.iform(2).le.iformo1)call j_closeunit(nu(2)) !###o###
 
	endif !if(sub)  19241
 
	return  ! j_err
 
	contains !subroutine more2
 
 
	subroutine more2()
		if(.not.associated(tail2%pnext))allocate(tail2%pnext)
		tail2=>tail2%pnext ;nullify(tail2%pnext)
		allocate(tail2%rbuf(1:nobsb*nvar2))
		iobc2=1
		iba2=0
		nbuf2=nbuf2+1
	end subroutine more2 !subroutine more2()
 
	subroutine initdata(id)
		integer ::id
		!j_getoption_index(iob,io,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg) ! %%option
		if(.not.continue)then
			if(id.eq.1)then
				!	write(6,*)'<8dbef',nu(id),iform(id)
				call j_getin(iob,io,nu(id),iform(id))
				!	write(6,*)'<8d88d',nu(id),iform(id)
				if(j_err)return
				if(nu(id).eq.0)then !.and..not.isinpu)then
					if(id.eq.1)then
						write(6,*)'in-> is missing'
					else !if(id.eq.1)then
						write(6,*)'subin-> is missing'
					endif !if(id.eq.1)  19270
					j_err=.true.;return
 
				endif !if(nu(id).eq.0)  19269
 
			else !if(id.eq.1)then
				!	write(6,*)'<4884 getsubin'
				call j_getsubin(iob,io,nu(id),iform(id))
 
 
			endif !if(id.eq.1)  19264
		endif !if(.not.continue)  19263
		single(id)=iform(id).eq.j_ivb.or.iform(id).eq.j_ivdi.or.iform(id).eq.j_ivdg.or.&
			iform(id).eq.j_ivbgaya
		if(p)write(6,*)'id ',id,'aft getin nu,iform', nu(id),iform(id)
		if(iform(id).gt.j_predefined)then
			call j_getchar(iform(id),form(id),lenform(id))
			iform(id)=iformfortran
		endif !if(iform(id).gt.j_predefined)  19289
		if(iform(id).eq.j_ivdi.or.iform(id).eq.j_ivdg)iform(id)=iformd
		if(iform(id).eq.j_ivdi2.or.iform(id).eq.j_ivdg2)iform(id)=iformd2
 
		isincl(id)=j_incin
 
		if(j_err)return
 
		!		write(6,*)'tas ',allocated(j_optarg2)
		rfreadlink(id)=j_linkoption(iob,io,lread(id),link=.true.)
		call j_getoption_index(iob,io,lread(id),0,9999,j_ipreal,.false.,&
			j_nread(id),j_d(id)%readv)
		!			write(6,*)'taseuue ',allocated(j_optarg2)
		!	write(6,*)'*note: if variables are read from the first line, use read->$'
		if(j_err)return
		!	ipe=0
		if(j_nread(id).eq.0)then
			vff(id)=.true.
		elseif(j_nread(id).eq.1.and.j_d(id)%readv(1).eq.j_ivdollar)then
			vff(id)=.true.
		else
			vff(id)=.false.
		endif !if(j_nread(id).eq.0)  19308
 
		if(.not.vff(id))then
			rfreadlink(id)=0
			!	if(allocated(j_d(id)%readv))deallocate(j_d(id)%readv)
			!	allocate(j_d(id)%readv(1:j_nread(id)))
			!	write(6,*)'<466464 ',j_nread(id)
		endif !if(.not.vff(id))  19316
		!	j_d(id)%readv=j_optarg0
		!	write(6,*)'<6336636 isgaya,id ',isgaya,id,isgaya.and.id.eq.2,id.eq.2
 
 
		!		if(ipe.gt.0)call j_del(j_ivdollar)
 
		!		call j_getoption_index(iob,io,lrfhead(id),-1,0,j_ipreal,.false.,noptarg,j_optarg0)
		!if(noptarg.eq.0)then
		rfheadlink(id)=j_linkoption(iob,io,lrfhead(id),clear=.true.,link=.true.)
 
		rfcodelink(id)=j_linkoption(iob,io,lrfcode(id),clear=.true.,link=.true.)
		if(rfcodelink(id).gt.0)then
			if(j_o(iob)%i(rfcodelink(id)).gt.0)then
				write(6,*)'rfcode-> cannot have arguments'
				j_err=.true.;return
			endif !if(j_o(iob)%i(rfcodelink(id)).gt.0)  19334
		endif !if(rfcodelink(id).gt.0)  19333
		if(rfheadlink(id).gt.0)then
			if(j_o(iob)%i(rfheadlink(id)).gt.0)then
				write(6,*)'rfcode-> cannot have arguments'
				j_err=.true.;return
			endif !if(j_o(iob)%i(rfheadlink(id)).gt.0)  19340
		endif !if(rfheadlink(id).gt.0)  19339
 
		!	call j_getoption_index(iob,io,lrfcode(id),-1,0,j_ipreal,.false.,noptarg,j_optarg0)
 
 
 
		call j_getoption_index(iob,io,lnobs(id),-1,1,j_ipreal,.true.,noptarg,j_optarg0)
		if(j_err)return
		isnobs(id)=noptarg.gt.0
		if(isnobs(id))then
 
			ivnobs(id) = j_optarg0(1)
			nobs(id) = nint(j_v(ivnobs(id)))
			!		write(6,*)'data(), nobs->',nobs(id)
			if(nobs(id).le.0)then
				write(6,*)'**nobs is illegal ',nobs(id)
				j_err=.true. ;return
			endif !if(nobs(id).le.0)  19358
		else !if(isnobs(id))then
			nobs(id)=10**9
		endif !if(isnobs(id))  19353
 
		call j_getoption_index(iob,io,lmaketrans(id),-1,1,j_iptrans,.true.,noptarg,j_optarg0)
		if(j_err)return
		!ivmaketrans=j_optarg0(1)
		! if(p)write(6,*)'linkopt', j_linkoption(iob,io,j_mmaketrans)
		! if(p)write(6,'(30i5/)')j_o(iob)%i(1: j_o(iob)%i(0))
		! if(p)write(6,*)'<5make',noptarg,ivmaketrans,j_o(iob)%i( j_linkoption(iob,io,j_mmaketrans):j_linkoption(iob,io,j_mmaketrans)+3)
 
		ismaketrans(id)=noptarg.gt.0
		noul=0
		if(ismaketrans(id))then
			if(j_otype(j_optarg0(1)).ne.j_iptrans)then
				call j_printname('argument of maketrans-> ,i.e. ',j_optarg0(1),' is not transformation')
				j_err=.true.; return
			endif !if(j_otype(j_optarg0(1)).ne.j_iptrans)  19376
			ivmaketrans(id)=j_optarg0(1)
			ivoul=j_trans_output(j_optarg0(1))
 
			noul=j_o(ivoul)%i(1)
		endif !if(ismaketrans(id))  19375
		!	write(6,*)'ivoul',ivoul,noul,ivmaketrans(id)
 
		!iskeep(id)=j_linkoption(iob,io,lkeep(id)).gt.0
		if(rfreadlink(id).gt.0)then
 
			read(nu(id),'(a)')j_tempchar2
			leco=len_trim(j_tempchar2)
			if(leco.le.0)then
				write(6,*)'read->$ or read-> but first line is empty'
				j_err=.true.
				return
			endif !if(leco.le.0)  19392
			call j_getname(iout(id))
			call j_command(j_oname(1:j_loname)//'%read=list('//j_tempchar2(1:leco)//')')
			if(j_err)then
				write(6,*)'illegal variable list:',j_tempchar2(1:leco)
				return
			endif !if(j_err)  19399
			ivread=j_object(j_oname(1:j_loname)//'%read')
			! if(ivr.le.0)j_getobject(0,j_oname(1:j_loname)//'%read',j_ipreal,ivr)
			!				call j_deflistobjectinp(iout(id),'%read',j_tempchar2(1:leco),ivread)
			!makes $ a list
 
			!		ipe=1
			j_nread(id)=j_o(ivread)%i(1)
			j_d(id)%readv=>j_o(ivread)%i2(1:j_nread(id))
			!			write(6,*)'j_nread(id),j_d(id)%readv',j_nread(id),j_d(id)%readv
			! nsi=size(j_d(id)%readv)
			! if(j_nread(id).gt.nsi)then
			! deallocate(j_d(id)%readv)
			! allocate(j_d(id)%readv(1:j_nread(id)))
			! endif
 
			! j_d(id)%readv(1:j_nread(id))=j_o(j_ivdollar)%i2(1:j_nread(id))
 
			! call j_del(j_ivdollar)
			rfreadlink(id)=0
			!			goto 459
		endif !if(rfreadlink(id).gt.0)  19388
		!	tassa
 
		if(j_linkoption(iob,io,lkeep(id)).gt.0)then
			iskeep(id)=.true.
			li=j_linkoption(iob,io,lkeep(id))
			call j_deflistobject(iout(id),'%keep',ivkeep(id),listold=j_o(iob)%i(li:))!   keep_)
			nkeep(id)=j_o(ivkeep(id))%i(1)
		else !if(j_linkoption(iob,io,lkeep(id)).gt.0)then
 
			nextra0=0
			if(id.eq.1.and.ivnobswcum.gt.0)nextra0=1
			call j_getoption_index(iob,io,lextra(id),-1,999,j_ipreal,&
				.true.,nextra,j_optarg0);if(j_err)return
			if(nextra.lt.0)nextra=0
			!	write(6,*)'<98)',iout(id),'%keep',ivkeep(id),'list0=',j_nread(id),&
			!		'list=',j_d(id)%readv,'nres=',noul+nextra0+nextra
			call j_deflistobject(iout(id),'%keep',ivkeep(id),list0=j_nread(id),&
				list=j_d(id)%readv,nres=noul+nextra0+nextra)!
			if(noul.gt.0)iper=j_putlistobject(ivkeep(id),ivin=ivoul)
 
			if(nextra0.gt.0)iper=j_putlistobject(ivkeep(id),single=ivnobswcum)
			if(nextra.gt.0)iper=j_putlistobject(ivkeep(id),list0=nextra,list=j_optarg0)
			nkeep(id)=j_o(ivkeep(id))%i(1)
			!	write(6,*)nkeep(id)
		end if !if(j_linkoption(iob,io,lkeep(id)).gt.0)  19426
 
		!write(6,*)'<48888464 ',j_nread(id)
 
		! call j_getoption_index(iob,io,lobs(id),-1,1,j_ipreal,&
		! .true.,noptarg,j_optarg0);if(j_err)return
		! if(noptarg.le.0)then
		! ivobs(id)=j_ivobs
		! else !if(noptarg.le.0)then
		! ivobs(id)=j_optarg0(1)
		! endif !if(noptarg.le.0)  18819
		call j_getobject(iout(id),'%obs',j_ipreal,ivobs(id))
 
 
		filterlink(id)=j_codelink(iob,io,lfilter(id))
		!	write(6,*)'<666  ',filterlink(id),lfilter(id),j_mfilter
		isfilter(id)=filterlink(id).ne.0
 
 
		rejectlink(id)=j_codelink(iob,io,lreject(id))
		isreject(id)=rejectlink(id).ne.0
 
 
 
		j_eof(id)=.false.
 
		!write(6,*)'<4888999 ',j_nread(id)
 
 
	end subroutine initdata !subroutine initdata(id)
 
	subroutine readobs(id)
	333 format(a)   ! format(Q,a)
	!	write(6,*)'inpu readobs',isinpu
		if(p)write(6,*)'<777 iform(id),nu(id),isincl(id),j_ivdollar',iform(id),nu(id),isincl(id),j_ivdollar
		! if(isinpu)then
		! write(6,*)'inpu',isinpu
		! call j_getinput('<',inprint)
		! !		write(6,*)j_inp(1:j_linp)
		! if(j_inp(1:j_linp).eq.'/')goto 51
 
		! read(j_inp(1:j_linp),*,err=26,end=26)j_d(id)%readvec
		! !	write(6,*)j_d(id)%readvec
		! return
		! endif !if(isinpu)  18185
 
		select case(iform(id))
 
		case(j_ivdollar) !select case(iform(id))
		if(p)write(6,*)'<227',nu(id)
		!	read(nu(id),*,end=51,err=950)j_v(j_d(id)%readv) !;j_v(j_d(id)%read)=readv
 
 
 
 
 
 
		if(isincl(id))then
			if(p)write(6,*)j_o( j_inciv(j_ninc))%i(6)+1,j_o(nu(id))%txt(iobc(id)+1)
			j_o( j_inciv(j_ninc))%i(6)=j_o( j_inciv(j_ninc))%i(6)+1
			if(j_o(nu(id))%txt(j_o( j_inciv(j_ninc))%i(6))(1:1).eq.'/')goto 51
			! write(6,*)'<617',j_o( j_inciv(j_ninc))%i(6)
			! write(6,*)'<618',j_inciv(j_ninc),nu(id)
			goto 444
			888	write(6,*)'too short record in incl file line ',j_o( j_inciv(j_ninc))%i(6)

			write(6,*)j_o(nu(id))%txt(j_o( j_inciv(j_ninc))%i(6))
			if(j_v(j_ivcontinue).ne.j_0)call j_bypassinc(nu(id))
 
			j_err=.true.
			goto 51
 
		666		write(6,*)'error in reading incl file line ',j_o( j_inciv(j_ninc))%i(6)

			write(6,*)j_o(nu(id))%txt(j_o( j_inciv(j_ninc))%i(6))
			if(j_v(j_ivcontinue).ne.j_0)call j_bypassinc(nu(id))
 
			j_err=.true.
			goto 51
 
 
			! write(6,*)'<618',j_o(nu(id))%i(6)
			if(p) write(6,*)'<618',j_o(nu(id))%txt(j_o( j_inciv(j_ninc))%i(6))
			444		read(j_o(nu(id))%txt(j_o( j_inciv(j_ninc))%i(6)),*,err=666,end=888)j_d(id)%readvec

		else !if(isincl(id))then
 
			if(nu(id).eq.5)then
				goto 27
			26	write(6,*)'reading error, but continue, end=/'
		1000 format(a,$)
					27	write(6,1000)'in>'
				read(5,'(a)')j_tempchar2
				if(index(j_tempchar2(1:10),'/').gt.0)goto 51
				read(j_tempchar2,*,err=26)j_d(id)%readvec
			else !if(nu(id).eq.5)then
				read(nu(id),*,end=51,err=950)j_d(id)%readvec
				!			if(id.eq.1)write(6,*)'<55id1 ',j_d(id)%readvec
			endif !if(nu(id).eq.5)  19535
		endif !if(isincl(id))  19505
 
		case(j_ivb2) !select case(iform(id))
		!if(p)write(6,*)'<33>',jdataread_
		!if(p)write(6,*)'<34>',j_v(jdataread_)
		read(nu(id),end=51,err=950)j_d(id)%readvec !j_v(j_d(id)%readv) !readv
		!j_v(jdataread_)=readv !(v(o(iob)%i(lre+j)),j=1,data_nread)
		case(j_ivb) !select case(iform(id))
		!if(p)write(6,*)'<33>',jdataread_
		!if(p)write(6,*)'<34>',j_v(jdataread_)
		read(nu(id),end=51,err=950)j_d(id)%readvecsing !j_v(j_d(id)%readv) !readv
		!j_v(jdataread_)=readv !(v(o(iob)%i(lre+j)),j=1,data_nread)
		case(iformd) !select case(iform(id))
		read(nu(id),rec=irecord(i)+1,iostat=ios)j_d(id)%readvecsing! j_v(j_d(id)%readv) !readv
		!j_v(jdataread_)=readv
		if(ios.ne.0)goto 51
 
		case(iformd2) !select case(iform(id))
		read(nu(id),rec=irecord(i)+1,iostat=ios)j_d(id)%readvec! j_v(j_d(id)%readv) !readv
		!j_v(jdataread_)=readv
		if(ios.ne.0)goto 51
 
		case(j_ivbgaya) !select case(iform(id))
		be=secnds(0.)
		call j_gayax(nu(2),ii) !!!!!,j_data_subread_,j_v)
		af=secnds(be)
		time=time+af
 
		case default !select case(iform(id))
		write(6,*)
		read(nu(id),fmt=form(id)(1:lenform(id)),end=51,err=950)j_d(id)%readvec !j_v(j_d(id)%readv)!j_v(readv
 
		end select !select case(iform(id))
600	continue
		if(p)write(6,*)'<22999',single(id),j_nread(id)
		if(p)write(6,*)'<22999BBid',id,single(id),j_nread(id),j_d(id)%readv(1:j_nread(id))
		if(isgaya.and.id.eq.2)then
			j_v(j_d(id)%readv(1:j_nread(id)))=j_g_var(1:j_nread(id))
		elseif(single(id))then
			j_v(j_d(id)%readv(1:j_nread(id)))=j_d(id)%readvecsing
		else !if(single(id))then
			!		write(6,*)'hu',j_nread(id),j_d(id)%readv
			j_v(j_d(id)%readv(1:j_nread(id)))=j_d(id)%readvec
			!	if(id.eq.1)write(6,*)'<8888 ',j_d(id)%readv(1:j_nread(id)),j_d(id)%readvec
		endif !if(isgaya.and.id.eq.2)  19583
 
		irecord(id)=irecord(id)+1
		if(ismaketrans(id))then
			j_v(j_ivrecord)=irecord(id)
			j_v(ivobs(id))=iobs(id)+1
			call dotrans(ivmaketrans(id),1)
			!	if(id.eq.1)write(6,*)'<9999 ',j_d(id)%readv(1:j_nread(id))
			if(j_err)goto 51
		end if !if(ismaketrans(id))  19594
		!	write(6,*)'<88',isfilter(id),isreject(id),filterlink(id),rejectlink(id)
		j_rejected=.false.
		if(isfilter(id))then
			j_v(j_ivrecord)=irecord(id)
			j_v(ivobs(id))=iobs(id)+1
			!	call dotrans(iobcur_,j_iofilter)
 
			if(j_codevalue(iob,filterlink(id)).eq.j_0)j_rejected=.true. !j_v(j_ivfilter).eq.0.)then
			!	write(6,*)'filte ',j_codevalue(iob,filterlink(id))
			if (j_err) goto 51
			!end if !if(j_v(j_ivfilter).eq.0.)then
		end if !if(isfilter(id))  19603
		if(isreject(id))then
			j_v(j_ivrecord)=irecord(id)
			j_v(ivobs(id))=iobs(id)+1
			!		write(6,*)'firejte ',j_codevalue(iob,rejectlink(id))
			if(j_codevalue(iob,rejectlink(id)).ne.j_0)j_rejected=.true.
			!	call dotrans(iobcur_,j_ioreject)
			if (j_err) goto 51
			!	if(j_v(j_ivreject).ne.0)then
			!		j_rejected=.true.
			!	end if !if(j_v(j_ivreject).ne.0)then
		end if !if(isreject(id))  19613
		if(j_rejected)then
			nrejected(id)=nrejected(id)+1
			if(isnobs(id).and.irecord(id).ge.nobs(id)) goto 51
			cyclerobs=.true.
		endif !if(j_rejected)  19624
		iobs(id)=iobs(id)+1
		if(p)write(6,*)'>645id,iobs',id,iobs(id),' isnobs ',isnobs(id),' ii ',ii
		if(isnobs(id)) then
			if(iobs(id)>nobs(id)) then
				write(6,*)'number of observations greater than given in nobs-> ',nobs(id)
				j_err=.true. ;goto 51
			endif !if(iobs(id)>nobs(id))  19632
			! if(idisk.gt.0)then
			!write(idisk,rec=iobs)real(j_v(keepl(1:nvar)))
			! else
			cyclerobs=.true.
			j_o(ivmat(id))%d(iba(id)+1:iba(id)+nkeep(id))=j_v(j_o(ivkeep(id))%i2(1:nkeep(id))) !j_v(keepl(1:nvar))
			iba(id)=iba(id)+nkeep(id)
			!endif
 
		endif !if(isnobs(id))  19631
 
		return
	950		if(id.eq.1)then
			write(6,*)'error reading in-> file'
		else !if(id.eq.1)then
			write(6,*)'error reading subin-> file'
		endif !950		if(id.eq.1)  19647
		j_err=.true.
	51		goto51=.true.
		return
 
 
 
	end subroutine !subroutine readobs(id)
 
 
 
end subroutine data !subroutine data(iob,io)
 
subroutine stat_(iob,io)  ! stat(iob,io)
	!Section stat stat() Basic statistics in DATA
	! Computes and prints basic statistics from data objects.
	!endheader
	!Option
	!Output&0-1&REAL& kokopo
	! Args & 0-99&REAL&variables for which the statistics are computed,
	! the default is all variables in the data (all variables in the data matrix plus the output variables of the associated transformation object) and all output
 
	!@@data
	! data & -1,99 & Data &	data objects , see section Common options for default! weight 	gives the weight of each observations if weighted means and variances ar
	! transformation or it can be a variable in the data object
	!@@seecom
	! min & -1,99&REAL&	defines to which variables the minima are stored.
	!   If the value is character constant or character variable,
	!   then the name is formed by concatenating the character with the name of the argument
	! variable. E.g. stat(x1,x2,min->'%pien') stores minimums into variables
	! x1%pien and x2%pien. The default value for min  is '%min'.
	!If the values of the min-> option are variables,
	! then the minima are stored into these variables.
	! max &-1,99&REAL& maxima are stored, works as min->
	! mean &-1,99&REAL & means are stored
	! var &-1,99&REAL& variances are stored
	! sd &-1,99&REAL& standard deviations are stored
	! sum &-1,99&REAL&	sums are stored, (note that sums are not printed automatically)
	! nobs &-1 | 1&REAL&	gives variable which will get the number of accepted observations, default is variable 'Nnobs'. If all observations are rejected due to fi
	! trans &-1 | 1&TRANS&	transformation object which is executed for each observation. If there is a transformation object associated with the data object, those
	! filter &-1 | 1&Code& logical or arithmetic statement (nonzero value indicating True) describing which observations will be accepted. trans-> transformations
	! reject &-1 | 1&Code&
	! transafter &-1 | 1& TRANS& transformation object which is executed for each observation which has passed the filter and is not rejected by the reject->-optio
	!endoption
	! Note 1: stat() function prints min, max, means, sd and sd of the mean computed
	! as sd/sqrt(number of observations)
	!endnote
	! Note 2: If the value of a variable is greater than or equal to 1.7e19,
	! then that observation is rejected when computing statistics for that variable.
	!endnote
	!Ex statex stat() computes minimums, maximums, means and std deviatons
	!;if(type(data1).ne.DATA)dataex
	! ;if(type(data1).ne.DATA)dataex
	! stat()
	! stat(data->data1,sum->x2,mean->,filter->(x3.le.18.5))
	! li=;list(x2%?);
	! @li;
	! stat(x1,data->data1,weight->x2)
	! stat(x1,weight->(x2**1.2))
	!endex
	!endsection
 
 
	double precision ,dimension(:), allocatable::xmin_
	double precision,dimension(:), allocatable::xmax_
	double precision ,dimension(:), allocatable::xs
	double precision ,dimension(:), allocatable::xss
	double precision ,dimension(:), allocatable::sumwt
	double precision ,dimension(:), allocatable::summa,x
	double precision haj,cov,cor,weisu
	double precision xws,xw,xh
	logical weig,weigt,rejo,missing,istime !filter, single
 
	integer,dimension(:),pointer::arg
 
	!double precision, dimension(2):: xh
 
	!	integer,dimension(:),pointer :: arg2  !all
	logical::isup,istrans
	integer*8 ::iobs,nobs
	!ubroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout) ! %%function
	call j_startfunction(iob,io,j_ipreal,narg,arg,iout)
 
 
 
	if(j_err)return
 
	call j_getdataobject(iob,io)
 
	if(j_v(j_ivprintinput).gt.0)then
		call j_getname(j_divdata)
		write(6,*)j_oname(1:j_loname)
	endif !if(j_v(j_ivprintinput).gt.0)  19740
	!	write(6,*)'j_divdata,keep,nkeep',j_divdata,j_o(j_divdata)%i(2),j_o( j_o(j_divdata)%i(2))%i(1)
	! write(6,*)'idata1',j_divdata,j_dnobs,j_dfilterlink,j_drejectlink,j_divtrans,j_divvars,j_dimat
	! write(6,*)j_dnkeep,j_divkeep,j_dimatup,j_divkeepup,j_dnkeepup,j_divnobsw
	! write(6,*)j_divobsup,j_dnextobs,j_diba,j_dibaup,j_divobsw,j_divobs,j_diob
	! write(6,*)j_disup,j_distrans,j_disreject,j_disfilter
 
	if(j_err)return
	missing=j_isopt(iob,io,j_mmissing)
	istime=j_isopt(iob,io,j_mtime)
	if(istime)then
		call cpu_time(cpu0)
		time0=secnds(0.)
	endif !if(istime)  19753
	nobs=j_dnobs8
	!write(6,*)'nobs ',nobs
	! iprint=1
	! if(j_igetopt(iob,io,j_mprint).gt.0)iprint=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mprint)+1) )
 
 
	! if(ivnobs.eq.0) then
	! call j_getobject(0,'Nobs',j_ipreal,ivnobs)
	! if(j_err) return
	! endif !if(ivnobs.eq.0) then
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
	linkweight=j_codelink(iob,io,j_mweight)
	weig=linkweight.ne.0
	! if(j_linkoption(iob,io,j_mweight).gt.0)then
	! weig=.true.
	! ioweight=j_linkopt2(j_mweight)
	! ivweight=j_o(iob)%i(j_linkoption(iob,io,j_mweight)+1)
	! weigt=j_o(iob)%i(ioweight).ne.0
	! else !if(j_linkoption(iob,io,j_mweight).gt.0)then
	! weig=.false.
	! end if !if(j_linkoption(iob,io,j_mweight).gt.0)then
	call j_clearoption(iob,io)  ! subroutine
	if(narg.eq.0)then
		narg=j_o(j_divvars)%i(1)
		arg=>j_o(j_divvars)%i2(1:narg)
	endif !if(narg.eq.0)  19788
	xw=1.d0  !weight
	xws=0.d0
	iobs=0
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
	xs=j_0;xss=j_0;xmin_=1.7e37;xmax_=-1.7e37;sumwt=j_0
	summa=j_0
	!	write(6,*)'<66666narg,from,until',narg,j_dfrom,j_duntil
	if(missing)then
		!		allocate(x(1:narg))
		do iobs=j_dfrom,j_duntil
 
			call j_getobs(iobs)
 
			!	!call j_nextobs()
			!	write(6,*)'<13',i,'*',j_v(arg)
			if(j_err)return
			if(j_rejected)cycle
			ntot=ntot+1
			if(weig)then
				!if(weigt) call dotrans(iob,ioweight)
				xw=j_codevalue(iob,linkweight) !j_v(ivweight)
			endif !if(weig)  19826
			xws=xws+xw
			!			write(6,*)'<334arg',j_v(arg)
			do j=1,narg
				xh=j_v(arg(j))  !j_v(j_o(iob)%i(io+1+j))
				if(abs(xh).lt.1.7e19)then
					call j_msd21(xh,xs(j),xss(j),xw,sumwt(j),summa(j))
					xmin_(j)=min(xmin_(j),xh)
					xmax_(j)=max(xmax_(j),xh)
				end if !if(abs(xh).lt.1.7e19)  19834
			end do !j=1,narg  19832
		end do !iobs=j_dfrom,j_duntil  19817
	else !missing
		!write(6,*)'j_dfrom,j_duntil ',j_dfrom,j_duntil
		do iobs=j_dfrom,j_duntil
 
			call j_getobs(iobs)
			!	write(6,*)'<66',iobs,j_rejected
			!	!call j_nextobs()
			!	write(6,*)'<13',i,'*',j_v(arg)
			if(j_err)return
			if(j_rejected)cycle
			ntot=ntot+1
			if(weig)then
				!if(weigt) call dotrans(iob,ioweight)
				xw=j_codevalue(iob,linkweight) !j_v(ivweight)
			endif !if(weig)  19852
			!		xws=xws+xw
			!			write(6,*)'<334arg',j_v(arg)
			x=j_v(arg)
			call j_msd2(narg,x,xs,xss,xw,xws,summa)
			xmin_=min(xmin_,x)
			xmax_=max(xmax_,x)
			! do j=1,narg
			! xh=j_v(arg(j))  !j_v(j_o(iob)%i(io+1+j))
			! if(abs(xh).lt.1.7e19)then
			! call j_msd21(xh,xs(j),xss(j),xw,sumwt(j),summa(j))
			! xmin_(j)=min(xmin_(j),xh)
			! xmax_(j)=max(xmax_(j),xh)
			! end if !if(abs(xh).lt.1.7e19)then
			!				end do !do j=1,narg
		end do !iobs=j_dfrom,j_duntil  19843
	endif !if(missing)  19815
 
 
 
 
	j_v(j_ivaccepted)=ntot
	if(ntot.le.0)then
		write(6,*)'**stat: all observations rejected'
		goto 750
	endif !if(ntot.le.0)  19877
	if(j_dprint.ne.0)then
		write(6,*)'Accepted ',ntot
		if(j_dnobs.ne.ntot)write(6,*)'from the total number of obs: ',nobs
		if(weig)write(6,*)'sum of weights ',xws !maxval(sumwt(1:narg))
		if(lirmse.lt.0)then
			write(*,8882)'var          ','min', 'max','mean','sd','sdmean'
		else
			write(*,88822)'var          ','min', 'max','mean','sd','sdmean','rmse'
		endif !if(lirmse.lt.0)  19885
	end if !if(j_dprint.ne.0)  19881
	rejo=.false.
	if(iout.ne.j_ivresult)then
		if(limean.ge.0)ivmean=j_defmatrix(iout,'%mean',narg,1,j_matreg)
		if(livar.ge.0)ivvar=j_defmatrix(iout,'%var',narg,1,j_matreg)
		if(lisd.ge.0)ivsd=j_defmatrix(iout,'%sd',narg,1,j_matreg)
		if(lisdmean.ge.0)ivsdmean=j_defmatrix(iout,'%sdmean',narg,1,j_matreg)
		if(limin.ge.0)ivmin=j_defmatrix(iout,'%min',narg,1,j_matreg)
		if(lirmse.ge.0)ivrmse=j_defmatrix(iout,'%rmse',narg,1,j_matreg)
		if(limax.ge.0)ivmax=j_defmatrix(iout,'%max',narg,1,j_matreg)
		if(lisum.ge.0)ivsum=j_defmatrix(iout,'%sum',narg,1,j_matreg)
		!		if(j_err)return
	endif !if(iout.ne.j_ivresult)  19892
	!write(6,*)'<7474got'.ligot
	weisu=xws
	do j=1,narg
 
		haj=j_0
		if(missing)	weisu=sumwt(j)
 
 
		if(weig)then
			haj=sqrt(xss(j)/weisu)
		else !if(weig)then
			if(xss(j).gt.j_0)then
				if(weisu.gt.j_1)	haj=sqrt(xss(j)/(weisu-j_1))
			endif !if(xss(j).gt.j_0)  19914
		endif !if(weig)  19911
		iv=arg(j)    !j_o(iob)%i(io+1+j)
		if(ligot.ne.0)call j_putoptv(iob,ligot,j,'%got',iv,weisu)
		!write(6,*)'<666 ',iv
		if(j_dprint.ne.0)then
			if(lirmse.lt.0)then
				write(*,8881)j_vname(iv),xmin_(j),xmax_(j),xs(j),haj,haj/sqrt(weisu)
			else
				write(*,8881)j_vname(iv),xmin_(j),xmax_(j),xs(j),haj,haj/sqrt(weisu),sqrt(xs(j)*xs(j)+haj*haj)
			endif !if(lirmse.lt.0)  19922
			if(j_err)then
				write(6,*)'*j* problem with variable ',iv
				return
			endif !if(j_err)  19927
			if(missing.and.weisu.lt.xws)then
				rejo=.true.
				if(weig)then
					write(*,*)' sum of weights of rejected observations ',xws-weisu
				else !if(weig)then
					write(*,'(85x,a,i6)')'got: ',int(weisu)  !xws-sumwt(j))
				endif !if(weig)  19933
			endif !if(missing.and.weisu.lt.xws)  19931
		endif !if(j_dprint.ne.0)  19921
		if(iout.eq.j_ivresult)then
			if(limean.ge.0)call j_putoptv(iob,limean,j,'%mean',iv,xs(j))
			if(livar.ge.0)call j_putoptv(iob,livar,j,'%var',iv,haj**2)
			if(lisd.ge.0)call j_putoptv(iob,lisd,j,'%sd',iv,haj)
			if(lirmse.ge.0)call j_putoptv(iob,lirmse,j,'%rmse',iv,sqrt(xs(j)*xs(j)+haj*haj))
			if(lisdmean.ge.0)call j_putoptv(iob,lisdmean,j,'%sdmean',iv,haj/sqrt(sumwt(j)))
 
			if(limin.ge.0)call j_putoptv(iob,limin,j,'%min',iv,xmin_(j))
			if(limax.ge.0)call j_putoptv(iob,limax,j,'%max',iv,xmax_(j))
			if(lisum.ge.0)call j_putoptv(iob,lisum,j,'%sum',iv,summa(j))
		else !if(iout.eq.j_ivresult)then
			if(limean.ge.0)call j_putmatrix(ivmean,j,1,xs(j))
			if(livar.ge.0)call j_putmatrix(ivvar,j,1,haj**2)
			if(lisd.ge.0)call j_putmatrix(ivsd,j,1,haj)
			if(lisdmean.ge.0)call j_putmatrix(ivsdmean,j,1,haj/sqrt(sumwt(j)))
			if(limin.ge.0)call j_putmatrix(ivmin,j,1,xmin_(j))
			if(limax.ge.0)call j_putmatrix(ivmax,j,1,xmax_(j))
			if(lisum.ge.0)call j_putmatrix(ivsum,j,1,summa(j))
 
		endif !if(iout.eq.j_ivresult)  19940
	end do !j=1,narg  19905
	if(rejo.and.j_dprint.ne.0)write(*,*)'*** reason for rejecting observations for variables: abs-value.ge.1.7e19'
	750 deallocate(xmin_,xmax_,xs,xss,sumwt,summa)
	if(.not.missing)deallocate(x)
	8881 format(1x,a16,1x,6g14.6)
	8882 format(1x,a16,a8,5a14,a7)
		88822 format(1x,a16,a8,6a14,a7)
	900 continue  ! io=io+narg+3
	if(istime)then
		call cpu_time(cpu1)
		time1=secnds(time0)
		write(6,*)' '
		write(6,*)'stat() used cpu ',cpu1-cpu0,' s and total time ',time1,' s'
	endif !if(istime)  19968
	write(6,*)' '
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
	! @@data
	! endoption
 
	!Listing
	!subroutine transdata(iob,io)
	! call j_getdataobject(iob,io)
	! if(j_err)return
	! call j_clearoption(iob,io)  ! subroutine
 
	! do iobs=j_dfrom,j_duntil
	! call j_getobs(iobs)
	! if(j_err)return
	! end do !do iobs=j_dfrom,j_duntil
 
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
 
	endif !if(.not.j_distrans)  20015
	call j_clearoption(iob,io)  ! subroutine
	!	write(6,*)'af',j_dfrom,j_duntil
	do iobs=j_dfrom,j_duntil
		!	write(6,*)'iobs ',iobs
		call j_getobs(iobs)
		if(j_err)return
	end do !iobs=j_dfrom,j_duntil  20022
	! else
	! call j_getdataobject(iob,io)
	! if(j_err)return
	! call j_clearoption(iob,io)  ! subroutine
 
	! do iobs=j_dfrom,j_duntil
	! call j_getobs(iobs)
	! if(j_err)return
	! end do !iobs=j_dfrom,j_duntil  19383
 
 
	! endif !if(is2)  19369
 
	if(j_depilog.gt.0)call dotrans(j_depilog,1)
 
	return
end subroutine transdata!subroutine stat_(iob,io)
 
 
 
 
subroutine corr(iob,io,itype)  !stat()
	! Section cov cov()  Covariance MATRIX
	! cov() computes the covariance matrix of variables in DATA.
	! endheader
	! Option
	! output&1&MATRIX& symmetric aoutput matrix.
	! arg& 1-N&LIST or REALV& variables for which covarianes are computed, listing
	! individually or given as a LIST.
	! @@data
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
	endif !if(j_otype(ivi).eq.j_iplist)  20106
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	if(allocated(cov))deallocate(cov,xc,xm)
	np=narg*(narg+1)/2
	allocate( cov(1:np),xc(1:narg),xm(1:narg))
	cov=0.d0;xm=0.d0
	call j_objargs(iob,io)
	if(j_err)return
	call j_getdataobject(iob,io)
	if(j_err)return
	! iprint=1
	! if(j_igetopt(iob,io,j_mprint).gt.0)iprint=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mprint)+1) )
	linkweight=j_codelink(iob,io,j_mweight)
	weig=linkweight.ne.0
	! if(j_linkoption(iob,io,j_mweight).gt.0)then
	! weig=.true.
	! ioweight=j_linkopt2(j_mweight)
	! ivweight=j_o(iob)%i(j_linkoption(iob,io,j_mweight)+1)
	! weigt=j_o(iob)%i(ioweight).ne.0
	! else !if(j_linkoption(iob,io,j_mweight).gt.0)then
	! weig=.false.
	!end if !if(j_linkoption(iob,io,j_mweight).gt.0)then
	call j_clearoption(iob,io)  ! subroutine
	iobs=0
	iout=j_defmatrix(iout,' ',narg,narg,j_matreg)
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
		endif !if(weig)  20148
		if(wt.ge.1.7d19)cycle obloop
		do i=1,narg
			if(abs(j_v(arg(i))).ge.1.7d19)cycle obloop
		enddo !i=1,narg  20154
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
				4	   				continue !4 j=1,i  20164
		enddo !i=1,narg  20161
	enddo obloop !oop:		do ii=j_dfrom,j_duntil  20142
	!	enddo !do kk=1,jndatasetss
	ih=sumwt
	if(ntot.le.1.)then
		write(6,*)'*all observations rejected'
		call j_del(iout)
		write(6,*)' '
		return
	endif !if(ntot.le.1.)  20172
 
 
	!write(6,*)'output matrix can be seen with print-function'
	write(6,*)'Accepted ',ntot
	j_v(j_ivaccepted)=ntot
	do  i=1,np
		cov(i)=cov(i)/(sumwt-1.) !do 7 i=1,np
	enddo ! i=1,np  20183
	if(.not.iscorr)then  !not correlation
		do i=1,narg
			j_o(iout)%d((i-1)*narg+i)=cov(i*(i+1)/2)
			do j=1,i-1
				j_o(iout)%d((i-1)*narg+j)=cov(i*(i-1)/2+j)
				j_o(iout)%d((j-1)*narg+i)=cov(i*(i-1)/2+j)
			enddo !j=1,i-1  20189
		enddo !i=1,narg  20187
	else !if(.not.iscorr)then
		do i=1,narg
			xc(i)=cov(i*(i+1)/2)
			if(xc(i).gt.0.d0)xc(i)=sqrt(xc(i))
			j_o(iout)%d((i-1)*narg+i)=1.
			do j=1,i-1
				if(xc(i).gt.0..and.xc(j).gt.0.)then
					cor=cov(i*(i-1)/2+j)/(xc(i)*xc(j))
				else !if(xc(i).gt.0..and.xc(j).gt.0.)then
					cor=0.
				endif !if(xc(i).gt.0..and.xc(j).gt.0.)  20200
				j_o(iout)%d((i-1)*narg+j)=cor
				j_o(iout)%d((j-1)*narg+i)= cor
			enddo !j=1,i-1  20199
		enddo !i=1,narg  20195
	endif !if(.not.iscorr)  20186
	if(j_dprint.ge.1)then
 
		if(j_o(iob)%i(io) .eq.212)then  !not correlation
			write(6,8886)(j_object_name(arg(j),7),j=1,narg)
			do i=1,narg
				write(6,8881)j_object_name(arg(i),15),j_o(iout)%d((i-1)*narg+1:i*narg)
			enddo !i=1,narg  20214
 
		else !if(j_o(iob)%i(io) .eq.212)then
			write(6,8885)(j_object_name(arg(j),12),j=1,narg)
			do i=1,narg
				write(6,8883)j_object_name(arg(i),15),j_o(iout)%d((i-1)*narg+1:i*narg)
			enddo !i=1,narg  20220
 
 
 
		endif !if(j_o(iob)%i(io) .eq.212)  20212
 
 
	endif !if(j_dprint.ge.1)  20210
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
	!Function matrix() creates a matrix and puts REAL values to the elements. Element values
	!can be read from the input paragraph, file, or the values can be generated
	! using values-> option, or sequential values can be generated
	! using do-> option. Function matrix() can generate a diagonal and block diagonal matrix.
	! A matrix can be generated from submatrices by using matrices as arguments
	! of the  values-> option. It should be noted that matrices are stored in row order.
	!endheader
	!Option
	!Output& 1& MATRIX | REAL& If a 1x1 matrix is defined, the output will be REAL.
	! The output can be a temporary matrix without name, if matrix() is an argument
	! of an arithmetic function  or matrix function. If no element values are
	! given in values-> or obtained from in-> input, all elemets get value zero.
	!Args&0-2&REAL& The dimension of the matrix. The first argument is the number of rows,
	! the second argument, if present, the number of columns.  If the matrix is generated from submatrices given in values->, then the dimensions
	!refer to the submatrix rows and submatrix columns. If there are no arguments, then the
	!it should be possible to infer the dimensions from values-> option. If the
	! first argument is ]Inf[, the the number of rows is determined by the number
	! number of lines in source determined by in->.
	! in&N|0|1&CHAR& The input for values. in-> means that values are read in from
	! the following input paragraps, in->]file[ means that the values are read from file.
	! in both cases a record must contain one row for the matrix.
	!  If there is reading error and values are read from the terminal, Jlp22 gives
	! possibility to continue with better luck, otherwise an error occurs.
	!values&N|1-& REAL &values or MATRIX objects put to the matrix. The argumenst of
	! values-> option go in the regular way
	! through the interpreter, so the values can be obtained by computations. If only one REAL value is
	! given then all diagonal elements are put equal to the value (ohers will be zero),
	! if diag-> option is present, otherwise all elements are put equal to this value. If matrix dimensions
	!are given, and there are fewer values than is the size the matrix, matrix is
	! filled row by row using all values given in
	! values->. If there are more values as is the size, an error occurs unless there is
	! any-> option present.
	! Thus matrix(N,N,values->1) generates the identity matrix.
	! If value-> refers to one MATRIX,and diag-> is present then a block diagonal
	! matrix is generated. Without diag->, a partitioned matrix is generated having all
	! submatrices equal
	!do &N|0-3&REAL  & A matrix of number sequences is generated, as followsws: \newline
	!do-> Values 1,2,...,]arg1[ x ]arg2[ are put into the matrix in the row order. \newline
	!do->5 Values 5,6,...,]arg1[ x ]arg2[+4 are put into the matrix \newline
	!do->
	!endoption
	!Ex matrixex Example of generating matrices
	!A=matrix(3,
 
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
	integer*8:: ndim,ibas,i,ndim1,ncol,nval,ndim2,nel,nrows,ncols
	integer::nva
	p=int(j_v(j_ivdollar2)).eq.j_o(iob)%i(io)
	p=j_v(j_ivdollar2).eq.345.d0
	! write(6,*)'22>',j_o(iob)%i(0),'**',j_o(iob)%i(1:13)
 
	! do jj=1,j_noptions_
	! write(6,*)jj,j_options(jj)
 
	! enddo
	! stop
	!	io=io_
	call j_getoption(iob,io,j_mvalues,-1,99999,0,.true.,nva,j_optarg0)
	if(j_err)return
	matmat=.false.
	if(nva.gt.0)then
		do iva=1,nva
			if(j_otype(j_optarg0(iva)).eq.j_ipmatrix)then
				matmat=.true.
			elseif(j_otype(j_optarg0(iva)).ne.j_ipreal)then
				call j_getname(j_optarg0(iva))
				write(6,*)'values-> must refer to REAL or MATRIX ',j_oname(1:j_loname),&
					' is ',j_objecttypes(j_otype(j_optarg0(iva)))
			endif !if(j_otype(j_optarg0(iva)).eq.j_ipmatrix)  20323
		enddo !iva=1,nva  20322
	endif !if(nva.gt.0)  20321
	narg=j_o(iob)%i(io+1)
	if(narg.eq.0)then
		arg=>j_o(iob)%i(io:io) !just to such small number which does not cause trouble in arg(1)
	elseif(narg.le.2)then !if(narg.eq.0)then
		arg=>j_o(iob)%i(io+1+1:io+1+narg) !i narg.eq.0 arg refers to nonsense
	else
		write(6,*)'**matrix, illegal number of arguments'
		j_err=.true.; return
	endif !if(narg.eq.0)  20333
	!write(6,*)'matrix diag',j_linkoption(iob,io,j_mdiag),j_linkoption(iob,io,j_mvalues)
	!	io_=io_+narg+3
	iout=j_o(iob)%i(io+2+narg)
	diag=j_linkoption(iob,io,j_mdiag,clear=.true.).ge.0
	!	matmat=j_linkoption(iob,io,j_mmatrix,clear=.true.).ge.0
	itype=j_matreg
	if(diag)itype=j_matdiag
	isinf=narg.ge.1.and.arg(1).eq.j_ivinf
 
	! if(matmat.and.narg.eq.0)then
	! ndim=0
	! ncol=0
 
	! do iva=1,nva
	! if(j_otype(j_optarg0(iva)).eq.j_ipmatrix)then
	! if(diag)then
	! ncol=max(ncol,ndim+j_optarg0(iva)%i(2))
	! else
	! ncol=max(ncol,ndim)
	! endif
	! ndim=ndim+j_o (j_optarg0(iva))%i(1)
	! else
	! if(diag)then
 
	! else
	! ndim=ndim+1
	! ncol=max(ncol,ndim)
	! endif
	! endif !if(j_otype(j_optarg0(iva)).eq.j_ipmatrix)  19466
	! enddo !iva=1,nva  19465
	! write(6,*)'ndim,ncol ',ndim,ncol
	! call j_defmatrix(iout,' ',ndim,ncol,itype,iout)
 
	! ndim=0
	! n	col=0
	! ibas=0
	! do iva=1,nva
	! ima=_optarg0(iva)
	! if(j_otype(j_optarg0(iva)).eq.j_ipmatrix)then
	! if(diag)then
	! ncol=max(ncol,ndim+j_optarg0(iva)%i(2))
	! else
	! ncol=max(ncol,ndim)
	! do i=1,j_o(ima)%i(1)
	! j_o(iout)%d(ibas+1:j_o(ima)%i(2)
	! ibas=ibas+ncol
	! enddo
	! endif
	! ndim=ndim+j_o (j_optarg0(iva))%i(1)
	! else
	! if(diag)then
 
	! else
	! ndim=ndim+1
	! ncol=max(ncol,ndim)
	! endif
	! endif !if(j_otype(j_optarg0(iva)).eq.j_ipmatrix)  19466
	! enddo !iva=1,nva  19465
	! write(6,*)'ndim,ncol ',ndim,ncol
	! call j_defmatrix(iout,' ',ndim,ncol,itype,iout)
 
	! endif
 
	if(isinf.and.j_linkoption(iob,io,j_min).lt.0)then
		write(6,*)'if number of rows is Inf, then there must be in->'
		j_err=.true.;goto 70
	endif !if(isinf.and.j_linkoption(iob,io,j_min).lt.0)  20404
	if(diag.and.matmat.and.narg.eq.0.and.nva.eq.1)then
		irg=j_optarg0(1)
		ndim=j_o(irg)%i(3)
 
		iout=j_defmatrix8(iout,' ',ndim,ndim,j_matreg)
		ibas=0
		do i=1,ndim
			j_o(iout)%d(ibas+i)=j_o(irg)%d(i)
			ibas=ibas+ndim
		enddo !i=1,ndim  20414
		return
 
	endif !if(diag.and.matmat.and.narg.eq.0.and.nva.eq.1)  20408
 
 
	!	iarg1=j_o(iob)%i(io+2)
	!****
	if(narg.eq.1.and.j_otype(arg(1)).eq.j_iplist)then
 
		ndim1=j_o(arg(1))%i(1)
 
		if(diag)then
 
			iout=j_defmatrix8(iout,' ',ndim1,ndim1,itype)
			ibas=0
			do i=1,ndim
				j_o(iout)%d(ibas+i)=j_v(j_o(arg(1))%i2(i))
				ibas=ibas+ndim1
			enddo !i=1,ndim  20433
		else !if(diag)then
			iout=j_defmatrix8(iout,' ',ndim1,j_18,j_matreg)
			j_o(iout)%d=j_v(j_o(arg(1))%i2(1:ndim1))
		endif !if(diag)  20429
		goto 70
	endif !if(narg.eq.1.and.j_otype(arg(1)).eq.j_iplist)  20425
	!******
 
	if(p)write(6,*)'<88  narg,arg,iout,nva,inout', narg,arg,j_v(arg),iout,nva,inout
	ndim1=0
	ndim2=0
	if(matmat.and.narg.eq.0)then
		ndim1=nva
		ndim2=1
	elseif(narg.eq.1)then
		ndim1=j_v(arg(1))
		ndim2=1
		if(diag)ndim2=ndim1
 
	elseif(narg.eq.2)then
		ndim1=j_v(arg(1))
		ndim2=j_v(arg(2))
		if(diag.and.ndim1.ne.ndim2)then
			write(6,*)'with diag-> the number of rows must be equal to number of columns, or drop secon argument'
			j_err=.true.;return
		endif !if(diag.and.ndim1.ne.ndim2)  20459
	endif !if(matmat.and.narg.eq.0)  20448
	isreal=ndim.eq.1.and.ndim2.eq.1
	isany=j_linkoption(iob,io,j_many).ge.0
	!	iarg2=j_o(iob)%i(io+3)
	call j_getoption(iob,io,j_mdo,-1,99999,j_ipreal,.false.,ndo,doval);if(j_err)return
 
	!		write(6,*)'ndoa:',ndo,doval,j_v(doval)
	!	call getoption(iob,io,option_name,minarg,maxarg,iptype,expand,min0,noptarg,optarg)
 
 
	!write(6,*)'nva ',nva,j_optarg0(1)
	if(nva.gt.0)then
		if(allocated(valu))deallocate(valu)
	endif !if(nva.gt.0)  20474
	if(nva.eq.1)then
		if(j_otype(j_optarg0(1)).eq.j_ipmatrix.and..not.matmat)then
			nva=j_o(j_optarg0(1))%i(3)
			allocate(valu(1:nva))
			valu=j_o(j_optarg0(1))%d(1:nva)
		endif !if(j_otype(j_optarg0(1)).eq.j_ipmatrix.and..not.matmat)  20478
 
	elseif(nva.gt.0.and..not.matmat)then !if(nva.eq.1)then
 
		allocate(valu(1:nva))
		valu=j_v(j_optarg0(1:nva))
	endif !if(nva.eq.1)  20477
	!write(6,*)'<77',nva,j_optarg0
	if(matmat.and.nva.lt.1)then
		write(6,*)'element matrices must be given in values->'
		j_err=.true. ;goto 70
	endif !if(matmat.and.nva.lt.1)  20490
 
	if(nva.gt.0.and.ndo.gt.0)then
		write(6,*)'values-> and do-> cannot be simultaeously'
		j_err=.true. ;goto 70
	endif !if(nva.gt.0.and.ndo.gt.0)  20495
 
	if(narg.eq.0.and..not.matmat)then
		if(nva.le.0.and.ndo.eq.0)then
			write(6,*)'without arguments, values must be given using values-> or do->'
			j_err=.true. ;goto 70
 
		elseif(nva.gt.0)then !if(nva.le.0.and.ndo.eq.0)then
			ncol=1
			if(diag)ncol=nva
			j_i8=nva
			iout=j_defmatrix8(iout,' ',j_i8,ncol,itype)
			!	write(6,*)'<77'
			if(diag)then
 
				do i=1,nva
					j_o(iout)%d((i-1)*nva+i)=valu(i) !j_v(j_optarg0(i))
				enddo !i=1,nva  20513
			else !if(diag)then
				!write(6,*)'<765 ',narg,nva,valu,size(j_o(iout)%d)
				j_o(iout)%d=valu  !j_v(j_optarg0)
				!write(6,*)'<766 ',narg,nva,valu,size(j_o(iout)%d)
			endif !if(diag)  20511
 
 
			goto 70
		elseif(ndo.ge.0)then !if(nva.le.0.and.ndo.eq.0)then
			if(ndo.lt.2)then
				write(6,*)'without dimensions do-> must have atl least two arguments'
				j_err=.true. ;goto 70
			endif !if(ndo.lt.2)  20525
			dostep=1.d0
			if(ndo.eq.3)dostep=j_v(doval(3))
			if(dostep.eq.0)then
				write(6,*)'do-step is zero'
				j_err=.true. ; goto 70
			endif !if(dostep.eq.0)  20531
 
			nval=nint((j_v(doval(2))-j_v(doval(1)))/dostep)+1
			!		write(6,*)'ndo',ndo,doval
 
			if(nval.le.0)then
				write(6,*)'illegal do->'
				j_err=.true. ; goto 70
			endif !if(nval.le.0)  20539
 
			if(diag)then
				iout=j_defmatrix8(iout,' ',nval,nval,itype)
				!write(6,*)'<78'
				docur=j_v(doval(1))
				do i=1,nval
					j_o(iout)%d((i-1)*nval+i)=docur
					docur=docur+dostep
 
				enddo !i=1,nval  20548
			else !if(diag)then
				iout=j_defmatrix8(iout,' ',nval,j_18,itype)
				!write(6,*)'<80'
				docur=j_v(doval(1))
				do i=1,nval
					j_o(iout)%d(i)=docur
					docur=docur+dostep
 
				enddo !i=1,nval  20557
			endif !if(diag)  20544
			goto 70
		endif !if(nva.le.0.and.ndo.eq.0)  20501
 
	endif !if(narg.eq.0.and..not.matmat)  20500
 
 
	if(narg.eq.2)then
		if(j_otype(arg(1)).eq.j_iplist)then
			inde=j_v(arg(2))
			!	if(inde.lt.1.or.inde.gt.j_o(arg(1))%i8(1))then
			if(inde.lt.1.or.inde.gt.j_nrows(arg(1)))then
				write(6,*)'*illegal index ',inde, 'for list with len=',j_o(arg(1))%i(1)
				j_err=.true.;goto 70
			endif !if(inde.lt.1.or.inde.gt.j_nrows(arg(1)))  20573
			irg=j_o(arg(1))%i2(inde)
			if(j_otype(irg).eq.j_ipreal)then
				if(j_otype(iout).ne.j_ipreal)call j_del(iout)
				j_v(iout)=j_v(irg)
				goto 70
			endif !if(j_otype(irg).eq.j_ipreal)  20578
			if(j_otype(irg).ne.j_ipmatrix)then
				call j_printname('*not a matrix:',irg,' ')
				j_err=.true. ;goto 70
			endif !if(j_otype(irg).ne.j_ipmatrix)  20583
			nel=ndim1*ndim2
 
			iout=j_defmatrix8(iout,' ',ndim1,ndim2,j_matreg)
			!write(6,*)'<89',ndim1
			j_o(iout)%d=j_o(irg)%d(1:nel)
			!write(6,*)' '
			goto 70
		end if !if(j_otype(arg(1)).eq.j_iplist)  20570
	endif !if(narg.eq.2)  20569
 
 
 
 
 
	if(.not.matmat.and..not.isinf)then
		if(diag.and.nva.gt.0.and.nva.ne.ndim1.and.nva.ne.1)then
			write(6,*)nva,' values given for diagonal ',ndim1,' x ', ndim,' matrix, not allowed without any->'
			j_err=.true.
		elseif(.not.diag.and.nva.gt.0.and.ndim1*ndim2.ne.nva.and..not.isany.and.nva.ne.1)then !if(diag.and.nva.gt.0.and.nva.ne.ndim1.and.nva.ne.1)then
			write(6,*)'nrows=',ndim1,' and ncols=',ndim2, 'and there are ',nva, &
				'values and no any->'
			j_err=.true.
		endif !if(diag.and.nva.gt.0.and.nva.ne.ndim1.and.nva.ne.1)  20602
		if(j_err)return
 
		iout=j_defmatrix8(iout,' ',ndim1,ndim2,itype)
 
	endif !if(.not.matmat.and..not.isinf)  20601
	!write(6,*)'<100',ndim1,ndim2
	if(j_err)goto 70
	!	endif !if(narg.le.0)then
 
	if(matmat)then
		!	write(6,*)'<7757>here'
		inout=.false.
		if(nva.ne.ndim1*ndim2.and..not.diag)then
			write(6,*)'there should be ', ndim1*ndim2 ,' arguments in values->'
			j_err=.true. ;goto 70
		endif !if(nva.ne.ndim1*ndim2.and..not.diag)  20622
		!***********************
		if(diag)then
			nrows=0
			ivastep=1
			if(nva.eq.1)ivastep=0
			write(6,*)'nva ',nva
			iva=1
 
			do iro=1,ndim1
				ii=j_optarg0(iva)
				ii=j_optarg0(iva)
				iva=iva+ivastep
				if(ii.eq.iout)then
					write(6,*)'output cannot be same as one ov values-> matrices'
					j_err=.true.;return
				endif !if(ii.eq.iout)  20638
				if(j_otype(ii).eq.j_ipreal)then
					niro=1
					ncols=1
				else !if(j_otype(ii).eq.j_ipreal)then
					if(j_otype(ii).ne.j_ipmatrix)then
						call j_getname(ii)
						write(6,*)'values-argument ',j_oname(1:j_loname), ' neither real nor matrix'
						call printvalues()
						j_err=.true. ;goto 70
					endif !if(j_otype(ii).ne.j_ipmatrix)  20646
					niro=j_o(ii)%i(1)
					ncols=j_o(ii)%i(2)
					if(ncols.ne.niro)then
						call j_getname(ii)
						write(6,*)'matrix ',j_oname(1:j_loname),' is not square'
						j_err=.true.
						return
 
					endif !if(ncols.ne.niro)  20654
				endif !if(j_otype(ii).eq.j_ipreal)  20642
				nrows=nrows+niro
			enddo !iro=1,ndim1  20634
			iout=j_defmatrix8(iout,' ',nrows,nrows,j_matreg)
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
						enddo !ir=1,niro  20681
						!		write(6,*)'iro, ndim1,nrows,ibas',iro,ndim1,nrows,ibas
						ibas=ibas+nrows
					enddo !ir0=1,niro  20680
					ibas=ibas+niro
				endif !if(j_otype(ii).eq.j_ipreal)  20673
 
			enddo !iro=1,ndim1  20669
 
			return
		endif !if(diag)  20627
		!*****************matmat and diag
 
		nrows=0
 
		do iro=1,ndim1
			ii=j_optarg0((iro-1)*ndim2+1)
			if(ii.eq.iout)inout=.true.
			if(j_otype(ii).eq.j_ipreal)then
				niro=1
				ncols=1
			else !if(j_otype(ii).eq.j_ipreal)then
				if(j_otype(ii).ne.j_ipmatrix)then
					call j_getname(ii)
					write(6,*)'values-argument ',j_oname(1:j_loname), ' neither real nor matrix'
					call printvalues()
					j_err=.true. ;goto 70
				endif !if(j_otype(ii).ne.j_ipmatrix)  20706
				niro=j_o(ii)%i(1)
				ncols=j_o(ii)%i(2)
			endif !if(j_otype(ii).eq.j_ipreal)  20702
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
						j_err=.true.;goto 70
					endif !if(j_otype(ii).ne.j_ipmatrix)  20723
					niro2=j_o(ii)%i(1)
					ncol=j_o(ii)%i(2)
				endif !if(j_otype(ii).eq.j_ipreal)  20719
				if(niro2.ne.niro)then
					call j_getname(iout)
 
					write(6,*)'values-argument ',j_oname(1:j_loname),' in row ',iro, &
						' does not have same number of rows as the first in the row'
					call printvalues()
					j_err=.true.;goto 70
				endif !if(niro2.ne.niro)  20732
				ncols=ncols+ncol
 
			enddo !j2=2,ndim2  20716
			if(iro.gt.1.and.ncols.ne.ncolsv)then
				write(6,*)'row ',iro,' does not have the same number of columns as previous '
				call printvalues()
				j_err=.true.;goto 70
			endif !if(iro.gt.1.and.ncols.ne.ncolsv)  20743
			ncolsv=ncols
 
		enddo !iro=1,ndim1  20699
		if(inout)then
			allocate(temp2(1:nrows*ncols))
			outd=>temp2
		else !if(inout)then
 
			iout=j_defmatrix8(iout,' ',nrows,ncols,itype)
			!write(6,*)'<890',nrows,ncols,size(j_o(iout)%d)
			outd=>j_o(iout)%d
		endif !if(inout)  20751
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
 
						enddo !jic=1,ncol0  20774
					enddo !iri=1,nrow0  20773
					ncols0=ncols0+ncol0
				endif !if(j_otype(ii).eq.j_ipreal)  20766
			enddo !j2=1,ndim2  20763
			nrows0=nrows0+nrow0
 
 
		enddo !iro=1,ndim1  20761
		if(inout)then
			!write(6,*)'n55',nrows,ncols
 
			iout=j_defmatrix8(iout,' ',nrows,ncols,itype)
			!write(6,*)'n55',nrows,ncols,size(j_o(iout)%d)
			j_o(iout)%d=temp2
			!write(6,*)'n57',nrows,ncols,size(j_o(iout)%d)
			deallocate(temp2)
		endif !if(inout)  20786
		!	write(6,*)j_object_name(iout,15),'is ', nrows,' X ',ncols,' matrix'
		!	write(6,*)' ';
		goto 70
 
	endif !if(matmat)  20619
 
	if(nva.ge.1)then
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
				enddo !i=1,min(ndim1,ndim2)  20810
			else !if(diag)then
				j_o(iout)%d(1:ndim1*ndim2)=val
			endif !if(diag)  20807
			!		write(6,*)'tas3'
		else !if(nva.eq.1)then
			if(diag)then
				j_o(iout)%d(1:ndim1*ndim2)=j_0
				iba=1
				do j=1,min(ndim1,ndim2,nva)
					j_o(iout)%d(iba)=valu(j)  !j_v(j_optarg0(j))
					iba=iba+ndim2+1
				enddo !j=1,min(ndim1,ndim2,nva)  20822
			else !if(diag)then
 
				j_o(iout)%d(1:nva)=valu  !j_v(j_optarg0)   !
 
				!do j=1,min(ndim1*ndim2,j_o(iob)%i(j_linkoption(iob,io,j_mvalues)) )
			endif !if(diag)  20819
		endif !if(nva.eq.1)  20804
	else if(j_linkoption(iob,io,j_min).ge.0)then !if(nva.ge.1)then
		call j_getin(iob,io,nu,ivform)
		!	write(
		! endif !if(iv2.gt.0)then
		if(j_incin)then
			ivi=j_inciv(j_ninc)
			if(isinf)then
				lin=j_o( j_inciv(j_ninc))%i(6)
				lin0=lin
				do while(j_o(ivi)%txt(lin+1)(1:1).ne.'/')
					lin=lin+1
					if(lin.gt.j_o(ivi)%i(5))then
						write(6,*)'no / in the include paragraph'
						j_err=.true.
						return
 
					endif !if(lin.gt.j_o(ivi)%i(5))  20844
				enddo !while(j_o(ivi)%txt(lin+1)(1:1).ne.'/')  20842
				ndim1=lin-lin0
				!		write(6,*)'found ',ndim1,' rows'
				if(diag)ndim2=ndim1
 
			endif !if(isinf)  20839
 
			iout=j_defmatrix8(iout,' ',ndim1,ndim2,itype)
 
			if(diag)then
				lin=j_o( j_inciv(j_ninc))%i(6)+1
				!	do i=1,ndim1
				!		write(6,*)i,ndim1,ndim2,j,j_o(ivi)%txt(j)
				read(j_o(ivi)%txt(lin),*,err=90)(j_o(iout)%d((j-1)*ndim2+j),j=1,ndim1)
				if(j_o(ivi)%txt(lin+1)(1:1).ne.'/')then
					write(6,*)'the next line should be /, but let us ignore it this time'
					lis=1
				else
					lis=2
 
				endif !if(j_o(ivi)%txt(lin+1)(1:1).ne.'/')  20864
				j_o( j_inciv(j_ninc))%i(6)=j_o( j_inciv(j_ninc))%i(6)+lis
				!		j=j+1
				!	enddo !do i=1,ndim1
			else !if(diag)then
				ibas=0
				do i=1,ndim1
					read(j_o(ivi)%txt(j_o( j_inciv(j_ninc))%i(6)+i),*,err=90)(j_o(iout)%d(ibas+j),j=1,ndim2)
					ibas=ibas+ndim2
				enddo !i=1,ndim1  20876
 
				j_o( j_inciv(j_ninc))%i(6)=j_o( j_inciv(j_ninc))%i(6)+ndim1+1
				write(6,*)j_o(ivi)%txt(j_o( j_inciv(j_ninc))%i(6))
			endif !if(diag)  20859
			goto 70
 
		elseif(nu.eq.5.and..not.isinf)then !if(j_incin)then
			ibas=0
1000 format(a,$)
			if(diag)then
				write(6,1000)'in>'
				read(j_o(ivi)%txt(lin),*,err=90)(j_o(iout)%d((j-1)*ndim2+j),j=1,ndim1)
				write(6,*)'got it'
			else
				do i=1,ndim1
					write(6,1000)'in>'
					read(5,*,err=90)(j_o(iout)%d(ibas+j),j=1,ndim2)
					ibas=ibas+ndim2
				enddo !i=1,ndim1  20894
				write(6,1000)'in>'
				read(5,'(a)')j_tempchar2
				if(j_tempchar2(1:1).ne.'/')&
					write(6,*)'Jlp22 was expecting /, but matrix anyhow is read in and you can continue'
 
			endif !if(diag)  20889
 
			GOTO 70
			! read(j_o(j_inciv(j_ninc))%txt(ndim1+1),'(a)')loppu
			! if(loppu(1:1).ne.'/')then
			! write(6,*)'**not legal end for values:'//loppu
			! j_err=.true. ;return
			! end if !if(loppu(1:1).ne.'/')then
		endif !if(j_incin)  20837
 
 
	else !if(nva.ge.1)then
		ibas=0
 
		!		j_o(iout)%d=0.d0
		if(ndo.eq.0)then !if(ndo.lt.0)then
			if(diag)then
				do i=1,ndim1
					j_o(iout)%d(ibas+i)=i
					ibas=ibas+ndim1
				enddo !i=1,ndim1  20921
 
			else !if(diag)then
				do i=1,ndim1*ndim2
					j_o(iout)%d(i)=i
				enddo !i=1,ndim1*ndim2  20927
 
			endif !if(diag)  20920
		elseif(ndo.eq.1)then !if(ndo.lt.0)then
			if(diag)then
				do i=1,ndim1
					j_o(iout)%d(ibas+i)=i+j_v(doval(1))-1
					ibas=ibas+ndim1
				enddo !i=1,ndim1  20934
			else !if(diag)then
				do i=1,ndim1*ndim2
					j_o(iout)%d(i)=i+j_v(doval(1))-1
				enddo !i=1,ndim1*ndim2  20939
			endif !if(diag)  20933
		elseif(ndo.eq.2)then !if(ndo.lt.0)then
			iup=j_v(doval(2))
			if(diag)then
 
				do i=1,min(ndim1,iup)
					j_o(iout)%d(ibas+i)=i+j_v(doval(1))-1
					ibas=ibas+ndim1
				enddo !i=1,min(ndim1,iup)  20947
 
			else !if(diag)then
				do i=1,min(ndim1*ndim2,iup)
					j_o(iout)%d(i)=i+j_v(doval(1))-1
				enddo !i=1,min(ndim1*ndim2,iup)  20953
			endif !if(diag)  20945
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
					endif !if(j_v(doval(3)).lt.j_0)  20963
					ibas=ibas+ndim1
				enddo !i=1,ndim1  20960
 
			else !if(diag)then
				do i=1,ndim1*ndim2
					j_o(iout)%d(i)=docur
					docur=docur+j_v(doval(3))
					if(j_v(doval(3)).lt.j_0)then
						if(docur.lt.j_v(doval(2))-1.0d-17)exit
					else
						if(docur.gt.j_v(doval(2))+1.0d-17)exit
					endif !if(j_v(doval(3)).lt.j_0)  20975
 
				enddo !i=1,ndim1*ndim2  20972
 
			endif !if(diag)  20959
 
		endif !if(ndo.eq.0)  20919
	end if !if(nva.ge.1)  20801
	70 	continue ! io=io+narg+3
	call j_clearoption(iob,io)  ! subroutine
	if(allocated(valu))deallocate(valu)
	if(j_err)return
	!write(26,*)linkopt(j_mdiag
	if(j_o(iout)%i(3).eq.1)then
		docur=j_o(iout)%d(1)
		call j_del(iout)
		j_v(iout)=docur
	endif !if(j_o(iout)%i(3).eq.1)  20992
	if(isreal)then
		docur=j_o(iout)%d(1)
		call j_del(iout)
		j_v(iout)=docur
	endif !if(isreal)  20997
 
 
 
 
 
 
 
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
				write(6,*)j_oname(1:j_loname), ' has wrong type ',j_objecttypes(j_otype(ii))
			endif !if(j_otype(ii).eq.j_ipmatrix)  21028
		enddo !j=1,nva  21025
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
	! printed if Printresult>1. If there is no output, but just list of objects, then these
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
	!Note The value of Printresult can be changed in other parts of the transformation, or
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
	call j_startfunction(iob,io,0,nm,j_optarg0,iout)
	!write(6,*)'<88trans type ',j_otype(iout)
	!write(6,*)'<88trans type ',allocated(j_o(iout)%i),allocated(j_o(iout)%i2),allocated(j_o(iout)%ch)
 
 
 
	if(j_err)return
	iscall=.false.
	!	subroutine j_getoption_name(iob,option,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
 
	linsource=200
	if(nm.gt.0)then
		call j_deflistobject(iout,'%arg',ivarg_,list0=nm,list=j_optarg0)
		call j_deftrans(iout,' ',ivtrans,4000,400,400,ivinputl,ivoutputl,ivlocal,linsource,ivarg=ivarg_,istrans=.true.)
	else !if(nm.gt.0)then
		call j_deftrans(iout,' ',ivtrans,4000,400,400,ivinputl,ivoutputl,ivlocal,linsource,istrans=.true.)
		!	write(6,*)'<33ivtrans',ivtrans,j_otype(ivtrans),j_iptrans
	endif !if(nm.gt.0)  21200
	if(j_err)return
	j_o(ivtrans)%i(0)=0
 
	inprint=0
	if(j_v(j_ivprintinput).ge.3.)inprint=1
	iprin=j_igetopt(iob,io,j_mprint)
	if(iprin.eq.0)then
		inprint=1
	elseif(iprin.gt.0)then !if(iprin.eq.0)then
		inprint=j_v(iprin)
	endif !if(iprin.eq.0)  21213
	ivargu=j_igetopt(iob,io,j_marg)
	if(ivargu.gt.0)j_o(iout)%i2(9)=ivargu
	!	ivres=j_igetopt(iob,io,j_mresult)
	if(ivres.gt.0)j_o(iout)%i2(10)=ivres
 
	call j_getoption(iob,io,j_min,-1,1,j_iptext,.true.,nin,j_optarg0)
	if(j_err)return
	call j_clearoption(iob,io)  ! subroutine
	!return
	!if(nin.gt.0)write(6,*)'<569nin',nin
	if(nin.gt.0)then
		ivtext=j_optarg0(1)
		if(j_otype(ivtext).ne.j_iptext)then
			call j_getname(ivtext)
			write(6,*)'*object ',j_oname(1:j_loname),' is not TEXT'
			j_err=.true.;return
		endif !if(j_otype(ivtext).ne.j_iptext)  21230
		call j_gettext(ivtext,j_o(ivtext)%i(0))
		if(j_gottext(1:j_lgottext).ne.'/')then
			!	call j_getname(ivtext)
			!				write(6,*)'the last line of ',j_oname(1:j_loname),' should be / but let it be this time'
			call j_puttext(ivtext, '/')
		endif !if(j_gottext(1:j_lgottext).ne.'/')  21236
		!		write(6,*)'<4664ivtext ',ivtext,ivtrans
		call j_getname(ivtrans)
		!	write(6,*)'<7778>',j_oname(1:j_loname)
		call j_parser('trans',ivtrans,ivtext)
	else
		call j_parser('trans',ivtrans)
	endif !if(nin.gt.0)  21228
 
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
! call j_clearoption(iob,io)
! !if(narg.eq.1.and.j_otype(ivarg_).eq.j_ipchar)then
! if(narg2.lt.0.and.(narg.gt.1.or.(narg.eq.1.and.j_otype(arg(1).ne.j_ipchar)))then
! do i=1,narg
! if(arg(i).gt.j_namedv)then
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
! call j_clearoption(iob,io)  ! subroutine
! return
! elseif(narg.eq.1.and.j_otype(ivarg_).eq.j_ipchar)then !if(j_linkoption(iob,io,j_mmask).ne.0)then
 
! call j_getobject(iout,' ',j_iplist,iv2) !call getv2(iplist,ivout(teku),ior
! ivarg_=arg(1)
! !	if(narg.eq.1.and.j_otype(ivarg_).eq.j_ipchar)then
 
! call j_getline(j_ivnames,ivarg_,j_tempchar2,le2)
! isq=index(j_tempchar2(1:le2),'?')   ! first and last character is '
! if(isq.eq.2.and.le2.eq.3) then  ! '?'  -> all named variables
! allocate( j_o(iv2)%i(0:j_namedv ))
! do i=1,j_namedv  ;  j_o(iv2)%i(i )=i ;enddo
! j_o(iv2)%i(0) =j_namedv
! goto 900
! endif !if(isq.eq.2.and.le2.eq.3) then
! different=j_tempchar2(2:2).eq.'-'.and.le2.gt.3
! ledif=le2-3
! isq2=le2+1
! if(isq.gt.0)isq2=j_nextlim(j_tempchar2,isq+1,le2,'?')
! ok=.false.
 
! if(isq.gt.0.or.different)then
! nfound=0
! 17                 do i_=1,j_namedv
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
 
! enddo !do i_=1,j_namedv
 
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
	!Args& 0- &  & named objects. If an argument is LIST it is ex+panded
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
	!Note The index of object in a LIST can be obtained using index().
	!endnote
	!Ex2
	! li=list(x1...x3);
	! index(x2,li);
	! Continue=1
	! index(x4,li);  ! error
	! Continue=0
 
	!endex2
 
	!Ex list2ex
	! all=list();  ! empty list
	! sub=list();
	! nper=3
	! ;do(i,1,nper)
	! period#"i"=list(ba#"i",vol#"i",age#"i",harv#"i")
	! sub#"i"=list(@period#"i",mask->(-2,1,-1))
	! all=list(@all,@period#"i") !note that all is on both sides
	! sub=list(@sub,@sub#"i")
	! ;end do
	!endex
	!endsection
 
 
	!defines from compiled transforamtion list
	logical ok,different,issub
 
	call j_checkoutput0(iob,io)  !list cannot refer to itself
	if(j_err)return
	!	io=io_
	narg=j_o(iob)%i(io+1)
	! io_=io_+narg+3
 
	iout=j_o(iob)%i(io+narg+2)
 
	if(narg.eq.0)then
		call j_deflistobject(iout,' ',iv2,nres=20)
		return
	endif !if(narg.eq.0)  21446
 
	issub=j_linkoption(iob,io,j_msub,clear=.true.).ge.0
 
	!	write(6,*)'LISTnarg,iout',narg,iout,issub
	if(j_linkoption(iob,io,j_mmask).ge.0)then
		nel=0
		nel2=0  ! how many elements specified
		narg2=j_o(iob)%i(j_linkoption(iob,io,j_mmask))
		do i=1,narg2
			nval=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmask)+i))
			if(nval.gt.0)nel=nel+nval
			if(nval.eq.0)nval=-1
			nel2=nel2+abs(nval)
		end do !i=1,narg2  21458
		if(nel.le.0)then
			write(6,*)'*wrn: all mask elements false, no list generated';goto 900
		end if !if(nel.le.0)  21464
		if(nel2.lt.narg)write(6,*)'*wrn* mask shorter, rest rejected'
		if(nel2.gt.narg)write(6,*)'*wrn* mask longer, tail ignored'
 
		!	call j_getobject(iout,' ',j_iplist,iv2) !call getv2(iplist,ivout(teku),ior
		!	allocate( j_o(iv2)%i(0:nel))
		call j_deflistobject(iout,' ',iv2,nres=nel)
		!if(p)write(6,*)'<aa8',j_o(ivkeep)%i,'*',j_o(ivkeep)%i2
		nel=0
		nel2=0
		do i=1,narg2
			nval=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmask)+i))
			if(nval.gt.0)then
				do j=1,nval
					nel=nel+1
					nel2=nel2+1
					if(nel.lt.narg)j_o(iv2)%i2(nel)=j_o(iob)%i(io+1+nel2)
				end do !j=1,nval  21479
			else if(nval.eq.0)then !if(nval.gt.0)then
				nel2=nel2+1
			else !if(nval.gt.0)then
				nel2=nel2-nval
			end if !if(nval.gt.0)  21478
		end do !i=1,narg2  21476
		j_o(iv2)%i(1)=nel
		call j_clearoption(iob,io)  ! subroutine
	else !if(j_linkoption(iob,io,j_mmask).ge.0)then
 
		!	call j_getobject(iout,' ',j_iplist,iv2) !call getv2(iplist,ivout(teku),ior
		ivarg_=j_o(iob)%i(io+2)
		if(narg.eq.1.and.issub)then
			call j_getname(ivarg_)
			write(6,*)ivarg,j_loname,j_oname(1:j_loname)
			nfound=0
			do i_=1,j_namedv
				call j_getline(j_ivnames,i_,j_tempchar3,le3)
				if(le3.le.j_loname)cycle
				if(j_tempchar3(1:j_loname+1).eq.j_oname(1:j_loname)//'%')nfound=nfound+1
			enddo !i_=1,j_namedv  21500
			write(6,*)'nfound',nfound
			if(nfound.le.0)then
				!allocate( j_o(iv2)%i(0:0 ))
				!j_o(iv2)%i(0)=0
				call j_deflistobject(iout,' ',iv2,nres=1)
				return
			endif !if(nfound.le.0)  21506
 
			call j_deflistobject(iout,' ',iv2,nres=nfound)
			nfound=0
			do i_=1,j_namedv
				call j_getline(j_ivnames,i_,j_tempchar3,le3)
				if(le3.le.j_loname)cycle
				if(j_tempchar3(1:j_loname+1).eq.j_oname(1:j_loname)//'%')then
					nfound=nfound+1
					j_o(iout)%i2(nfound)=i_
				endif !if(j_tempchar3(1:j_loname+1).eq.j_oname(1:j_loname)//'%')  21518
			enddo !i_=1,j_namedv  21515
			j_o(iout)%i(1)=nfound
			j_o(iout)%i(3)=nfound
			return
 
		elseif(narg.eq.1.and.j_otype(ivarg_).eq.j_ipchar)then
 
			call j_getline(j_ivnames,ivarg_,j_tempchar2,le2)
			if(j_err)return
			isq=index(j_tempchar2(1:le2),'?')   ! first and last character is ?
			if(isq.eq.2.and.le2.eq.3) then  ! '?'  -> all named variables
				call j_deflistobject(iout,' ',iv2,nres=j_namedv)  !,allocate( j_o(iv2)%i(0:j_namedv ))
				do i=1,j_namedv  ;  j_o(iv2)%i2(i )=i ;enddo
				!	j_o(iv2)%i(0) =j_namedv
				goto 900
			endif !if(isq.eq.2.and.le2.eq.3)  21532
			different=j_tempchar2(2:2).eq.'-'.and.le2.gt.3
			ledif=le2-3
			isq2=le2+1
			if(isq.gt.0)isq2=j_nextlim(j_tempchar2,isq+1,le2,'?')
			ok=.false.
 
			if(isq.gt.0.or.different)then
				nfound=0
				17                 do i_=1,j_namedv
					call j_getline(j_ivnames,i_,j_tempchar3,le3)
					if(j_err)return
					if(different.and.le3.ge.ledif)then  !'!x'
						if(j_tempchar2(3:le2-1).eq.j_tempchar3(1:ledif))cycle
						goto 11
					endif !if(different.and.le3.ge.ledif)  21549
					if(isq.eq.2.and.isq2.eq.le2-1)then    !   '?x?'
						if(index(j_tempchar3(1:le3),j_tempchar2(3:le2-2)).gt.0 )goto 11
						cycle
					endif !if(isq.eq.2.and.isq2.eq.le2-1)  21553
					if(isq.gt.2)then  !there is something before ?     'x?'  or 'x?y'   ~
						if(le3.lt.isq-2)cycle
						if(j_tempchar3(1:isq-2).ne.j_tempchar2(2:isq-1))cycle
 
					endif !if(isq.gt.2)  21557
					loppupit=le2-isq-1
					if(loppupit.gt.0)then  !there is something after ?  '?x'  or 'x?y'
						if(le3.lt.loppupit)cycle
						if(j_tempchar3(le3-loppupit+1:le3).ne.j_tempchar2(isq+1:le2-1))cycle
 
					endif !if(loppupit.gt.0)  21563
 
				11                  nfound=nfound+1
					if(ok)j_o(iv2)%i2(nfound)=i_
 
				enddo !                do i_=1,j_namedv  21546
 
				if(nfound.le.0)then
					!allocate( j_o(iv2)%i(0:0 ))
					!j_o(iv2)%i(0)=0
					call j_deflistobject(iout,' ',iv2,nres=1)
					goto 900
				endif !if(nfound.le.0)  21574
				if(ok)goto 900
				call j_deflistobject(iout,' ',iv2,nres=nfound)
				!	allocate( j_o(iv2)%i(0:nfound ))
				ok=.true.  !collect now'
				!j_o(iv2)%i(0)=nfound
				nfound=0
				goto 17
			endif !if(isq.gt.0.or.different)  21544
		endif !if(narg.eq.1.and.issub)  21496
		!normal case
		if(j_err) return
 
		call j_deflistobject(iout,' ',iv2,listold=j_o(iob)%i(io+1:io+1+narg))
		!	write(6,*)'LIST',j_o(iout)%i,'**#',j_o(iout)%i2
		!allocate( j_o(iv2)%i(0:narg ))
		!j_o(iv2)%i(0)=narg
		!	j_o(iv2)%i2(1:narg)=j_o(iob)%i(io+2:io+1+narg) !2=1+1
		!	do i=1,narg;j_o(iv2)%i(i)=j_o(iob)%i(io+1+i);end do
	end if !if(j_linkoption(iob,io,j_mmask).ge.0)  21454
 
	900 continue !  io=io+narg+3
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
	end if !if(j_otype(ispl).ne.j_ipstemspline)  21649
	if(j_linkoption(iob,io,j_morigo).gt.0)then
		if(j_o(iob)%i(j_linkoption(iob,io,j_morigo)).ne.1)then
			write(6,*)'**stempolar: origo->  must give height of the origo'
			j_err=.true.
			return
 
		endif !if(j_o(iob)%i(j_linkoption(iob,io,j_morigo)).ne.1)  21655
		origo=100.*j_v(j_o(iob)%i(j_linkoption(iob,io,j_morigo)+1) )
 
	else !if(j_linkoption(iob,io,j_morigo).gt.0)then
		origo=0.
	endif !if(j_linkoption(iob,io,j_morigo).gt.0)  21654
	iverr=j_igetopt(iob,io,j_merr)
	call j_clearoption(iob,io)  ! subroutine
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
		endif !if(abs(j_o(ispl)%d(lir+L)-dref).lt.0.001)  21682
 
		if(j_o(ispl)%d(lir+L).le.dref)goto 221
 
		441       continue !441 L=1,npo2-1  21677
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
	enddo ! i=1,4  21725
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
	end if !if(nroot.lt.1)  21737
	R = ROOT(1)
	if(L.eq.1.and.j_o(ispl)%d(1).le.cotang*(j_o(ispl)%d(lirh)-origo))then
		!* before first knot
 
		IF (NROOT.GT.1 .AND.( r.gt.0.01.or.-r.gt.(j_o(ispl)%d(lirh)-origo)))R = ROOT(2)
		IF (NROOT.EQ.3 .AND. (r.gt.0.01.or.-r.gt.(j_o(ispl)%d(lirh)-origo) ))R = ROOT(3)
		if(r.gt.0.01.or.-r.gt.j_o(ispl)%d(lirh)-origo)then
			write(6,*)'**stempolar, err=3',nroot,root,l,j_o(ispl)%d(lirh)
			goto 99
 
		end if !if(r.gt.0.01.or.-r.gt.j_o(ispl)%d(lirh)-origo)  21747
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
 
		end if !IF (R.LT.-0.01 .OR.r.gt.sero)  21759
	end if !if(L.eq.1.and.j_o(ispl)%d(1).le.cotang*(j_o(ispl)%d(lirh)-  21742
 
	!C height height(L)+root
	r=r+j_o(ispl)%d(lirh+L-1)
 
	j_v(iout)=spld(r,npo, j_o(ispl)%d( lirh:),  j_o(ispl)%d( lirsp:))
 
			90 continue  ! io=io+narg+3
	return
 
			99 	   if(iverr.gt.0)then
		if(j_otype(iverr).ne.j_iptrans)then
			call j_printname('err-> transforamtion ',iverr,' not a transforamtion set')
		else !if(j_otype(iverr).ne.j_iptrans)then
			call dotrans(iverr,1)
		endif !if(j_otype(iverr).ne.j_iptrans)  21783
 
	endif !99 	   if(iverr.gt.0)  21782
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
 
		endif !if(xl.ge.xu)  21812
		j_v(iout)=splv(100.*xl,100.*xu,j_o(irg)%i(2),j_o(irg)%d(1+j_o(irg)%i(1):),j_o(irg)%d(1+2*j_o(irg)%i(1):) )
 
		if(j_err)return
		case default !select case(j_otype(irg))
		call j_printname('**not yet legal argument for integrate:',irg,' ')
		j_err=.true.
		return
 
		end select !select case(j_otype(irg))
	else !if(narg.ge.3)then
		xl=j_v(irg)
		xu=j_v(j_o(iob)%i(io+3))
		write(6,*)'**integrate type not yet implemented'
		j_err=.true.
	end if !if(narg.ge.3)  21806
 
	return
end !subroutine integrate(iob,io)
 
subroutine valuex(iob,io)   !
 
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	irg=j_o(iob)%i(io+2)
	iout=j_o(iob)%i(io+2+narg)
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	select case(j_otype(irg))
	case(j_ipstemspline) !select case(j_otype(irg))
 
	arg=j_v(j_o(iob)%i(io+3))
	npo=j_o(irg)%i(1)
	npo2=j_o(irg)%i(2)
 
	if(arg.le.0.4)then
		j_v(iout)=0.01*j_o(irg)%d(npo+npo2)
	else !if(arg.le.0.4)then
		j_v(iout)=0.01*splh(arg,j_o(irg)%i(2),j_o(irg)%d, j_o(irg)%d(1+npo:),  j_o(irg)%d(1+2*npo:))
		if(j_err)then
			write(6,*)'there were', npo2,' points in the curve'
			write(6,*)'you asked height of diamter:',arg
			write(6,*)'heights of knots (m):', 0.01*j_o(irg)%d(1+npo:npo2+npo)
			write(6,*)'diams of knots (cm) :', j_o(irg)%d(1:npo2)
 
		endif !if(j_err)  21856
	endif !if(arg.le.0.4)  21852
 
	case default !select case(j_otype(irg))
	call j_printname('**not legal argument for valuex:',irg,' ')
	j_err=.true.
 
	end select !select case(j_otype(irg))
	!	if(j_err)return
 
	return
end !subroutine valuex(iob,io)
 
subroutine stemspline(iob,io)     !
	! Section stemspline stemspline() Creates STEMSPLINE
	!To be reported later.
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
	call j_clearoption(iob,io)  ! subroutine
 
	if(narg.eq.2)then
		ivh_=j_o(iob)%i(io+2)
		ivd_=j_o(iob)%i(io+3)
		if(j_otype(ivh_).ne.j_ipmatrix.or.j_otype(ivd_).ne.j_ipmatrix)then
			write(6,*)'*stemspline: if two arguments, they should be column vectors'
			goto 90
		endif !if(j_otype(ivh_).ne.j_ipmatrix.or.j_otype(ivd_).ne.j_ipmat  21908
		npo=j_o(ivh_)%i(1)
		if(j_o(ivd_)%i(1).ne.npo)then
			write(6,*)'*stemspline:arguments have different dimensions'
			goto 90
		endif !if(j_o(ivd_)%i(1).ne.npo)  21913
		vec=.true.
	else !if(narg.eq.2)then
		if(test)then
			npo=0
			npobas=narg/2
			do i=1,npobas
				if(j_v(j_o(iob)%i(io+1+i)).le.0.and.j_v(j_o(iob)%i(io+1+i+npobas)).le.0.)then
					npo=i-1
					goto 756
				endif !if(j_v(j_o(iob)%i(io+1+i)).le.0.and.j_v(j_o(iob)%i(io+1+i+  21923
			enddo !i=1,npobas  21922
			npo=narg/2
			756		continue
			!!write(6,*)'npo',npo,j_v(j_o(iob)%i(io+1+i:io+1+npo))
		else !if(test)then
			npo=narg/2
			npobas=npo
		endif !if(test)  21919
		vec=.false.
 
	endif !if(narg.eq.2)  21905
 
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_ipstemspline.or.j_o(iout)%i(1).ne.npo)call j_del(iout)
		! defstemspline
	endif !if(j_otype(iout).ne.j_ipreal)  21939
 
	if(j_otype(iout).eq.j_ipreal)then
		allocate( j_o(iout)%i(1:4));j_o(iout)%i(1)=npo;j_o(iout)%i(3:4)=0
		allocate( j_o(iout)%d(1:6*npo))
		j_otype(iout)=j_ipstemspline
	end if !if(j_otype(iout).eq.j_ipreal)  21944
	! in jakta spl(dt,ht) in j(ht,dt), but use interanlly the same order as in jakta
 
	if(sort)then
		ibd=2*npo
		ibh=ibd+npo   !use r to store
		if(allocated(iperm))then
			if(size(iperm).lt.npo)then
				deallocate(iperm)
				allocate(iperm(1:npo+1)) !some reserve
			endif !if(size(iperm).lt.npo)  21955
		else !if(allocated(iperm))then
			allocate(iperm(1:npo+1))
		endif !if(allocated(iperm))  21954
		ii=1
		do i=1,npo
			if(vec)then
				j_o(iout)%d(ibh+ii)=100.*j_o(ivh_)%d(i)
				j_o(iout)%d(ibd+ii)=j_o(ivd_)%d(i)
			else !if(vec)then
				j_o(iout)%d(ibh+ii)=100.*j_v(j_o(iob)%i(io+1+i) ) !h
				j_o(iout)%d(ibd+ii)=j_v(j_o(iob)%i(io+1+npo+i))
 
			endif !if(vec)  21964
			if(j_o(iout)%d(ibh+ii).lt.0..or.j_o(iout)%d(ibd+ii).le.0.)cycle
			iperm(ii)=ii
			ii=ii+1
		enddo !i=1,npo  21963
		npo2=ii-1
 
		j_o(iout)%d(npo+1:npo+npo2)=j_o(iout)%d(ibh+1:ibh+npo2)  !new, copy first to the output plcae
		call j_quick_sort(j_o(iout)%d(npo+1:npo+npo2),iperm)
 
		do i=1,npo2
 
			j_o(iout)%d(i)=j_o(iout)%d(ibd+iperm(i))
		enddo !i=1,npo2  21981
 
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
 
				endif !if(i.lt.npo2)  21989
				ii=ii-1
			elseif(ii.ne.i)then !if(j_o(iout)%d(npo+i)-j_o(iout)%d(npo+ii-1).lt.delh)then
				j_o(iout)%d(npo+ii)=j_o(iout)%d(npo+i)
				j_o(iout)%d(ii)=j_o(iout)%d(i)
			endif !if(j_o(iout)%d(npo+i)-j_o(iout)%d(npo+ii-1).lt.delh)  21988
			ii=ii+1
 
		enddo !i=2,npo2  21987
 
		npo2=ii-1
 
	else !if(sort)then
		ii=1
		do 31 i=1,npo
			! dt, ht , splmat
 
			if(vec)then
				j_o(iout)%d(ii)=j_o(ivd_)%d(i)   !dt
				j_o(iout)%d(ii+npo)=100.*j_o(ivh_)%d(i)  !ht
			else !if(vec)then
				j_o(iout)%d(ii)=j_v(j_o(iob)%i(io+1+npobas+i))  !dt
				j_o(iout)%d(ii+npo)=100.*j_v(j_o(iob)%i(io+1+i) ) !ht
			endif !if(vec)  22015
			if( j_o(iout)%d(ii+npo).lt.0..or.j_o(iout)%d(ii).lt.0.)then
				goto 311
			end if !if( j_o(iout)%d(ii+npo).lt.0..or.j_o(iout)%d(ii).lt.0.)  22022
			if(ii.gt.1)then
				if(j_o(iout)%d(npo+ii).lt.j_o(iout)%d(npo+ii-1))then
 
					write(6,*)'*stemspline: heights not in order'
					if(vec)then
						write(6,*)j_o(ivh_)%d
					else !if(vec)then
						write(6,*)j_v(j_o(iob)%i(io+1+1:io+1+npo))
					endif !if(vec)  22029
					goto 90
				endif !if(j_o(iout)%d(npo+ii).lt.j_o(iout)%d(npo+ii-1))  22026
				if(j_o(iout)%d(npo+ii)-j_o(iout)%d(npo+ii-1).lt.delh)then
					if(i.lt.npo)then
						j_o(iout)%d(npo+ii-1)=  &
							0.5*(j_o(iout)%d(npo+ii-1)+j_o(iout)%d(npo+ii))
						j_o(iout)%d(ii-1)= &
							0.5*(j_o(iout)%d(ii-1)+j_o(iout)%d(ii))
					else !if(i.lt.npo)then
						j_o(iout)%d(npo+ii-1)=j_o(iout)%d(npo+ii)
						j_o(iout)%d(ii-1)=j_o(iout)%d(ii)
					endif !if(i.lt.npo)  22037
					ii=ii-1
				end if !if(j_o(iout)%d(npo+ii)-j_o(iout)%d(npo+ii-1).lt.delh)  22036
			end if !if(ii.gt.1)  22025
			ii=ii+1
 
			31     continue !31 i=1,npo  22012

			311	npo2=ii-1
	endif !if(sort)  21951
 
	j_o(iout)%i(2)=npo2
 
	if(iprint.gt.2.or.p)then
		write(6,*)'ht',j_o(iout)%d(npo+1:npo+npo2)
		write(6,*)'dt',j_o(iout)%d(1:npo2)
 
	endif !if(iprint.gt.2.or.p)  22058
 
	call spltd(npo2,j_o(iout)%d,j_o(iout)%d(npo+1:),j_o(iout)%d(2*npo+1:))
	kie=0
	!check that spline works
	dif0=1.8   !dy
	dtop=0.6*j_o(iout)%d(npo2)
 
	700    do i=1,npo2-1

		arg1=0.67*j_o(iout)%d(npo+i)+0.33*j_o(iout)%d(npo+i+1)
		test1=spld(arg1,npo2,j_o(iout)%d(npo+1:),  j_o(iout)%d(1+2*npo:))
		arg2=0.33*j_o(iout)%d(npo+i)+0.67*j_o(iout)%d(npo+i+1)
		test2=spld(arg2,npo2,j_o(iout)%d(npo+1:),  j_o(iout)%d(1+2*npo:))
		dmi=min(j_o(iout)%d(i),j_o(iout)%d(i+1)) !greater end point
		dma=max(j_o(iout)%d(i),j_o(iout)%d(i+1)) !smaller end point
 
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
		endif !if(test1.gt.test2)  22079
 
		isok=1
		if(testma-dma.gt.dif0)then
			arg=argma
			isok=0
			f=0.7*dma+0.3*dmi
		else if(dmi-testmi.gt.dif0.or.testmi.le.dtop)then !if(testma-dma.gt.dif0)then
			arg=argmi
			f=0.7*dmi+0.3*dma
			isok=0
		endif !if(testma-dma.gt.dif0)  22093
		if(p)then
			write(6,*)i,j_o(iout)%d(i),j_o(iout)%d(i+1),test1,test2
		endif !if(p)  22102
 
		if(isok.le.0.and..false.)then
 
			if(p.or.iprint.ge.2) write(6,*)'illogical values in stemspline ',test1,test2, ' between values ',j_o(iout)%d(i),&
				j_o(iout)%d(i+1) !,' trying to fix' !, press <ret> to continue'
 
			if(npo2.ge.npo)then
				allocate(dapu(1:npo2+1),hapu(1:npo2+1))
				dapu(1:i)=j_o(iout)%d(1:i);dapu(i+2:npo2+1)=j_o(iout)%d(i+1:npo2)
				hapu(1:i)=j_o(iout)%d(npo+1:npo+i);hapu(i+2:npo2+1)=j_o(iout)%d(npo+i+1:npo+npo2)
				dapu(i+1:i+1)=f
				hapu(i+1:i+1)=arg
				deallocate(j_o(iout)%d)
 
 
				npo=npo2+1
				j_o(iout)%i(1)=npo;   !;o(iout)%i(3:4)=0
				allocate( j_o(iout)%d(1:6*npo))
				j_o(iout)%d(1:npo)=dapu
				j_o(iout)%d(npo+1:2*npo)=hapu
				deallocate(dapu,hapu)
			else !if(npo2.ge.npo)then
 
				do j=npo2,i+1,-1
					j_o(iout)%d(j+1)=j_o(iout)%d(j)
					j_o(iout)%d(npo+j+1)=j_o(iout)%d(npo+j)
				enddo !j=npo2,i+1,-1  22128
				j_o(iout)%d(i+1)=f
				j_o(iout)%d(npo+i+1)=arg
			endif !if(npo2.ge.npo)  22111
			npo2=npo2+1
			j_o(iout)%i(2)=npo2
			if(p.or.iprint.gt.2)then
				write(6,*)'new heights ',j_o(iout)%d(npo+1:npo+npo2)
				write(6,*)'new diams   ',j_o(iout)%d(1:npo2)
			endif !if(p.or.iprint.gt.2)  22137
			call spltd(npo2,j_o(iout)%d,j_o(iout)%d(npo+1:),j_o(iout)%d(2*npo+1:))
			kie=kie+1
 
			if(kie.le.7)goto 700
			write(6,*)'stemspline is mixed up'
			goto 90
		endif !if(isok.le.0.and..false.)  22106
 
	enddo !    do i=1,npo2-1  22070
 
	return
90 j_err=.true.

	call j_del(iout)
end !subroutine stemspline(iob,io)
 
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
	endif !if(npar.gt.0)  22198
 
	call j_clearoption(iob,io)  ! subroutine
 
	npo=narg/2
	if(j_otype(iout).ne.j_ipreal)then
		if(j_otype(iout).ne.j_iptautspline.or.j_o(iout)%i(1).ne.npo)call j_del(iout) !pitäisko ne -> lt.
		! defstemspline
	endif !if(j_otype(iout).ne.j_ipreal)  22208
	if(allocated(s))then
		if(size(tau).lt.npo)then
			deallocate(tau,gtau,s)
		endif !if(size(tau).lt.npo)  22213
		allocate(tau(1:npo),gtau(1:npo),s(1:npo,1:6))
	else !if(allocated(s))then
		allocate(tau(1:npo),gtau(1:npo),s(1:npo,1:6))
	endif !if(allocated(s))  22212
	if(j_otype(iout).eq.j_ipreal)then
		allocate( j_o(iout)%i(1:2))
		npo2=3*npo !reserved number of break points
		allocate( j_o(iout)%d(1:5*npo2))
	end if !if(j_otype(iout).eq.j_ipreal)  22220
 
	do i=1,npo
		tau(i)=j_v(j_o(iob)%i(io+1+i) ) !h
		gtau(i)= j_v(j_o(iob)%i(io+1+npo+i))  !d
	enddo !i=1,npo  22226
	if(sort)then
		if(allocated(iperm))then
			if(size(iperm).lt.npo)then
				deallocate(iperm)
				allocate(iperm(1:npo+1)) !some reserve
			endif !if(size(iperm).lt.npo)  22232
		else !if(allocated(iperm))then
			allocate(iperm(1:npo+1))
		endif !if(allocated(iperm))  22231
 
		call j_quick_sort(tau(1:npo),iperm)
 
		do i=1,npo
			gtau(i)=gtau(iperm(i))
		enddo !i=1,npo  22242
 
	else !if(sort)then
		do i=1,npo-1
			if(tau(i).gt.tau(i+1))then
				write(6,*)'*tautspline: x:s not in order', &
					(tau(jj),jj=1,npo)
				j_err=.true.
				return
			endif !if(tau(i).gt.tau(i+1))  22248
		enddo !i=1,npo-1  22247
	endif !if(sort)  22230
 
	if(p)then
		write(6,*)'x ',tau(1:npo)
		write(6,*)'y ',gtau(1:npo)
 
	endif !if(p)  22257
 
	call tautsp(tau,gtau,npo,gammap,s,j_o(iout)%d,j_o(iout)%d(npo2+1:),&
		nbreak,k,iflag)
	if(iflag.gt.1)then
		if(iflag.eq.2)write(6,*)'*tautspline: input not correct'
		if(iflag.eq.3)write(6,*)'*tautspline: ask J. Lappi to increase npo2'
		j_err=.true.
		call j_del(iout)
	endif !if(iflag.gt.1)  22265
 
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
 
	endif !if(ivout.eq.j_ivresult)  22328
	n=j_o(arg(1))%i(1)
	if(n.ne.j_o(arg(1))%i(2))then
		call j_printname('*eigen: argument ',arg(1),' not a square matrix')
		j_err=.true.
		return
	endif !if(n.ne.j_o(arg(1))%i(2))  22335
	allocate(a(1:n,1:n),w(1:n))
	LDA=n
	do i=1,n
		do j=i,n
			a(i,j)=j_o(arg(1))%d((i-1)*n+j)
		end do !j=i,n  22343
	end do !i=1,n  22342
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
	END IF !IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN )  22364
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
		END IF !IF( INFO.EQ.0 )  22394
		CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
	END IF !IF( ISCALE.EQ.1 )  22393
	ivmat=j_defmatrix(ivout,'%matrix',n,n,j_matreg)
	do i=1,n
		do j=1,n
			j_o(ivmat)%d((i-1)*n+j)=a(i,j)
		enddo !j=1,n  22403
	enddo !i=1,n  22402
	if(pr)call j_printname('eigenvectors stored in matrix ',ivmat,' ')
	ivmat=j_defmatrix(ivout,'%values',1,n,j_matreg)
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
! call j_clearoption(iob,io)  ! subroutine
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
	endif !if(j_otype(ifunc).ne.j_iptrans)  22478
	call j_getname(iob,ifunc)
 
	!	write(6,*)'++',j_oname(1:j_loname),' ',j_oname2(1:j_loname2)
	!	write(6,*)'<55call ',j_o(ifunc)%i(1:j_o(ifunc)%i(0))
	call dotrans(ifunc,1) !io=1
 
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
		call j_getchar(j_o(iob)%i(io+2),j_pauseprompt,le)
		if(j_err)return
		call j_pause(j_pauseprompt(1:le),do=.true.)
	else !if(j_o(iob)%i(io+1).gt.0)then
		call j_pause(do=.true.)
	endif !if(j_o(iob)%i(io+1).gt.0)  22515
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
 
	call j_getchar(arg(1),j_filename,le)
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
	! directly by the text() function which works in a nonstandard way. The syntax is:
	! output=text()//
	! …
	! //
 
	! The input paragraph ends exceptionally with '//' and not with '/'. The lines within the input
	! paragraph of text are put literally into the text object (i.e. even if there would be input
	! programming functions or structures included)
	! endsection
	clean=j_isopt(iob,io,j_mclean)
	call j_clearoption(iob,io)
	narg=j_o(iob)%i(io+1)
 
	!how many domains are in one set, or in one row set
 
	iout=j_o(iob)%i(io+2+narg)
	if(iout.eq.j_ivresult)then
		write(*,*)'**text must have output';j_err=.true.;goto 99
	end if !if(iout.eq.j_ivresult)  22603
 
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	ndefrow=200
 
	call j_deftext(iout,' ',ndefrow,80*ndefrow,ivrow_)
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
			endif !if(clean)  22623
			goto 1
		end if !if(j_inp(1:2).ne.'//')  22622
	else !if(j_ninc.eq.1)then
		iiv=j_inciv(j_ninc)
		irow=j_o(iiv)%i(6)
195		if(j_o(iiv)%txt(irow+1)(1:2).ne.'//')then
	!		write(6,*)'<5454>',j_o(iiv)%txt(irow+1)(1:j_o(iiv)%i2(irow+1))
			if(clean)then
				call j_putcleantext(ivrow_,j_o(iiv)%txt(irow+1)(1:j_o(iiv)%i2(irow+1)))
 
			else
				call j_puttext(ivrow_,j_o(iiv)%txt(irow+1)(1:j_o(iiv)%i2(irow+1)))
			endif !if(clean)  22635
			irow=irow+1
			goto 195
		end if !195		if(j_o(iiv)%txt(irow+1)(1:2).ne.'//')  22633
		j_o(iiv)%i(6)=irow+1
 
	endif !if(j_ninc.eq.1)  22615
 
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
	! Works as text(), to be documented later. The new TXT object is used
	! to implement ;incl and Gnuplot -figures.
	! endsection
 
	narg=j_o(iob)%i(io+1)
 
	!how many domains are in one set, or in one row set
 
	iout=j_o(iob)%i(io+2+narg)
	if(iout.eq.j_ivresult)then
		write(*,*)'**txt must have output';j_err=.true.;return
	end if !if(iout.eq.j_ivresult)  22670
	if(j_linkoption(iob,io,j_min).eq.0)then
		write(6,*)'txt() requires in->'
		j_err=.true.;return
	endif !if(j_linkoption(iob,io,j_min).eq.0)  22673
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
 
	call j_getin(iob,io,nu,ivform)
	!write(6,*)'<888nu,ivform ',nu,ivform,j_o(nu)%i
	if(j_err)return
	isize=j_filesize(ifile=nu)
	!write(6,*)'889isisze',isize
	if(nu.eq.5)isize=10000
	if(j_incin)then
		lin=j_o( nu)%i(6)
		!5 number of lines
		!6 lines used
		!7	last modified
		!8 lines allocated
		lin0=lin
		!	write(6,*)'<8880i',j_o(nu)%i
		do while(j_o(nu)%txt(lin+1)(1:2).ne.'//')
			lin=lin+1
			if(lin.ge.j_o(nu)%i(5))then
				write(6,*)'no // in the include paragraph'
				j_err=.true.
				return
 
			endif !if(lin.ge.j_o(nu)%i(5))  22695
		enddo !while(j_o(nu)%txt(lin+1)(1:2).ne.'//')  22693
		ndim1=lin-lin0
		write(6,*)'found ',ndim1,' rows'
		allocate(j_o(iout)%txt(1:ndim1))
		allocate(j_o(iout)%i2(1:ndim1))
		allocate(j_o(iout)%i(1:8))
		j_o(iout)%i(1:8)=0
		j_o(iout)%i(5)=ndim1
		j_o(iout)%i(8)=ndim1
		li=lin0
		do i=1,ndim1
			li=li+1
 
			j_o(iout)%txt(i)(1:j_o(nu)%i2(li))=j_o(nu)%txt(li)(1:j_o(nu)%i2(li))
			j_o(iout)%i2(i)=j_o(nu)%i2(li)
		enddo !i=1,ndim1  22711
		j_o(nu)%i(6)=li+1
		j_otype(iout)=j_iptxt
		return
	endif !if(j_incin)  22685
 
 
 
	nlin=isize/4
	if(allocated(j_temptxt))then
		if(size(j_temptxt).lt.nlin)then
			deallocate(j_temptxt)
			allocate(j_temptxt(1:nlin))
		endif !if(size(j_temptxt).lt.nlin)  22726
	else !if(allocated(j_temptxt))then
		allocate(j_temptxt(1:nlin))
	endif !if(allocated(j_temptxt))  22725
 
 
 
	irow=0
 
	1000      format(a,$)
	100      format(a)
	if(nu.eq.5)write(6,*)'give txt, end://'
 
	1  if(nu.eq.5)write(6,1000)'txt>'

	read(nu,100,end=91)j_temptxt(irow+1)
	if(j_temptxt(irow+1)(1:2).eq.'//')goto 91
	irow=irow+1
	goto 1
 
91		allocate(j_o(iout)%txt(1:irow))
	allocate(j_o(iout)%i2(1:irow))
	allocate(j_o(iout)%i(1:8))
	j_o(iout)%i(1:8)=0
	j_o(iout)%i(5)=irow
	j_o(iout)%i(8)=irow  !lines allocated
	do i=1,irow
		call j_cleanstart(j_temptxt(i)(1:j_lentrim(j_temptxt(i))),le)
		j_o(iout)%txt(i)=j_temptxt(i)(1:le)
		j_o(iout)%i2(i)=le
	enddo !i=1,irow  22755
	deallocate(j_temptxt)
	j_otype(iout)=j_iptxt
	return
 
end subroutine !subroutine txt(iob,io)
 
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
	endif !if(ivclass.le.0)  22782
	ivdummy=j_igetopt(iob,io,j_mdummy)
	if(ivdummy.le.0)then
		write(6,*)'**crossed: dummy-> missing'
		j_err=.true.
		return
	endif !if(ivdummy.le.0)  22788
	call j_getdataobject(iob,io)
	if(j_err)return
	if(.not.j_transopt)then                              !call dotrans(ivtransopt,1)
		write(6,*)'**crossed: trans-> must be present'
		j_err=.true.
		return
	endif !if(.not.j_transopt)  22795
	likeep=j_linkoption(iob,io,j_mkeep)
	if(likeep.le.0)then
		write(6,*)'**crossed: keep-> missing'
		j_err=.true.
		return
	endif !if(likeep.le.0)  22801
	nvar=j_o(iob)%i(likeep)
	call j_deflistobject(iout,'%keep',ivkeep,listold=j_o(iob)%i(likeep:))
	!	call j_deflist2(iout,'%keep',j_o(iob)%i(likeep:),ivkeep)
	!call j_deflist2(iout,'%vars',j_o(iob)%i(likeep:),ivvars)
	call j_getobject(0,'$Stage',j_ipreal,ivstage)
	!	ivobs=j_igetopt(iob,io,j_mobs)  !; if(ivobs.le.0)ivobs=j_ivobs
	mxclass=1000
	allocate(inclass(1:mxclass))
	call j_clearoption(iob,io)  ! subroutine
	nobc=0
 
	nclass=1
	!	do k=1,jndatasetss
	ic=j_inlistobject(ivclass,j_divkeep)
	if(ic.le.0)then
		!	call j_printname('**crossed: class variable ',ivclass, 'not in data ', j_datasets(k))
		j_err=.true.
		return
	endif !if(ic.le.0)  22820
	inclass(nclass)=1
	!	ivmat=j_o(j_datasets(k))%i(1)
	!	nobs=j_o(ivmat)%i(1)    !h(1)=ivmat
	!	nkeep=j_o(ivmat)%i(2)    !h(1)=ivmat
	classv=j_o(j_dimat)%d(ic)
	do i=2,j_dnobs
		class=j_o(j_dimat)%d((i-1)*j_dnkeep+ic)
		if(class.ne.classv)then
			if(sym)then
				nobc=nobc+inclass(nclass)*(inclass(nclass)+1)/2
			else !if(sym)then
				nobc=nobc+inclass(nclass)*inclass(nclass)
			endif !if(sym)  22833
			nclass=nclass+1
			if(nclass.gt.mxclass)then
				allocate(inclass2(1:2*mxclass))
				inclass2(1:mxclass)=inclass
				deallocate(inclass)
				mxclass=2*mxclass
				inclass=>inclass2
				nullify(inclass2)
 
			endif !if(nclass.gt.mxclass)  22839
			classv=class
			inclass(nclass)=0
 
		endif !if(class.ne.classv)  22832
		inclass(nclass)=inclass(nclass)+1
 
	enddo !i=2,j_dnobs  22830
	if(sym)then
		nobc=nobc+inclass(nclass)*(inclass(nclass)+1)/2
	else !if(sym)then
		nobc=nobc+inclass(nclass)*inclass(nclass)
	endif !if(sym)  22855
 
 
	!	enddo !do k=1,jndatasetss
	ivmat=j_defmatrix(iout,'%matrix',nobc,nvar,j_matreg)
	ibas=0
	iobs0=1
	ncl=1
	!	do k=1,jndatasetss
	!call j_getdataset(j_datasets(k),nobs)
	iclass=0
	do i=j_dfrom,j_duntil
		j_v(ivstage)=1.
		call j_getobs(i); if(j_err)return
		if(j_rejected)then
			write(6,*)'**reject/filter does not work in crossed'
			j_err=.true.;return
		endif !if(j_rejected)  22873
		iclass=iclass+1
		if(sym)then
			iup=i
		else !if(sym)then
			iup=iobs0+inclass(ncl)-1
		endif !if(sym)  22878
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
		endif !if(iclass.eq.inclass(ncl))  22899
 
	enddo !i=j_dfrom,j_duntil  22870
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
	endif !if(j_otype(irg).eq.j_ipreal)  22919
 
 
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
	end if !if(narg.eq.1)  22953
	iout=j_o(iob)%i(io+2+narg)
	ivcolmin=j_igetopt(iob,io,j_mcolmin)
	if(ivcolmin.gt.0)then
		icolmin=j_v(ivcolmin)
		more=1-icolmin
	else !if(ivcolmin.gt.0)then
		icolmin=1
		more=0
	endif !if(ivcolmin.gt.0)  22960
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
 
		end if !if(ivin.gt.0)  22969
		iofunc=j_codelink(iob,io,j_mfunc)
		! if(j_linkoption(iob,io,j_mfunc).gt.0)then
		! iofunc=j_linkopt2(j_mfunc)   ! start adress for
		! ivfunc=j_o(iob)%i(j_linkoption(iob,io,j_mfunc)+1)
		! else !if(j_linkoption(iob,io,j_mfunc).ge.0)then
		! iofunc=0
		! end if !if(j_linkoption(iob,io,j_mfunc).ge.0)then
		call j_clearoption(iob,io)  ! subroutine
 
		if(ndim2.lt.0)then
			n1000=1000
			allocate (ivals(1:n1000))
			ivalmin=1000000
			ivalmax=-100000
		else !if(ndim2.lt.0)then
			ndim22=ndim2+more
			allocate (ivals(1:ndim22))
		endif !if(ndim2.lt.0)  22989
 
		if(ndim1.lt.0.or.ndim2.lt.0)then
 
			iobs=0
	10		continue
			if(isinc)then
				if(j_o(nu)%txt(iobs+1)(1:1).eq.'/')goto 20
				read(j_o(nu)%txt(iobs+1),*,err=90)n,(ivals(k),k=1,n)
 
			else !if(isinc)then
				read(nu,*,err=90,end=20)n,(ivals(k),k=1,n)
			endif !if(isinc)  23003
			iobs=iobs+1
 
			if(ndim2.lt.0.and.n.gt.0)then  !need to c
				if(iofunc.gt.0)then
					do k=1,n
 
						j_v(j_ivdollar)=ivals(k)
 
						!	call dotrans(iob,iofunc)
						!	if(j_err)return
						ivals(k)=j_codevalue(iob,iofunc)  !j_v(ivfunc)
					enddo !k=1,n  23014
 
				endif !if(iofunc.gt.0)  23013
				ivalmin2=minval(ivals(1:n))
				if((ivcolmin.gt.0.or.ndim2.gt.0).and.ivalmin2.lt.icolmin)then
					write(6,*)'*row:',iobs, 'smallest value ',ivalmin2, ' smaller than colmin->',icolmin
					goto 90
				endif !if((ivcolmin.gt.0.or.ndim2.gt.0).and.ivalmin2.lt.icolmin)  23025
 
				ivalmin=min(ivalmin,ivalmin2)
				ivalmax=max(ivalmax,maxval(ivals(1:n)))
			endif !if(ndim2.lt.0.and.n.gt.0)  23012
			if(ndim1.lt.0.or.iobs.lt.ndim1)goto 10
 
			20 continue
			if(ndim1.lt.0)then
				ndim1=iobs
			elseif(iobs.lt.ndim1)then !if(ndim1.lt.0)then
				write(6,*)'*wrn, bitmatrix: data had ',iobs, 'rows, but matrix has', ndim1
			endif !if(ndim1.lt.0)  23036
			if(ndim2.le.0)then
				icolmin=ivalmin
				more=1-icolmin
 
				ndim2=ivalmax
				ndim22=ndim2+more
			endif !if(ndim2.le.0)  23041
			call j_printname('bitmatrix ',iout,' got dimensions')
			write(6,*)ndim1,' x (',icolmin,':',ndim2,')'
			if(.not.isinc)rewind(nu)
 
		endif !if(ndim1.lt.0.or.ndim2.lt.0)  22999
 
	endif !if(ivin.ge.0)  22968
 
	nel=(ndim1*ndim22+31)/32
 
	call j_getobject2(iout,j_ipbitmatrix)
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
			endif !if(isinc)  23066
			if(n.gt.ndim22)then
				write(6,*)'*wrn* bitmatrix:  row ',iobs, 'has too many values, extra ignored :', n
			endif !if(n.gt.ndim22)  23071
			do k=1,n
				if(iofunc.gt.0)then
					j_v(j_ivdollar)=ivals(k)
					call dotrans(iob,iofunc)
					if(j_err)return
					ivals(k)=j_v(ivfunc)
				endif !if(iofunc.gt.0)  23075
				if(ivals(k).lt.icolmin.or.ivals(k).gt.ndim2)then
					write(6,*)'**bitmatrix, row ',iobs,' has illegal column ',ivals(k)
					j_err=.true.
					deallocate(ivals)
					if(ivin.gt.0)call j_closeunit(nu)
					return
				endif !if(ivals(k).lt.icolmin.or.ivals(k).gt.ndim2)  23081
				call j_bitset(iout,iobs,ivals(k),1.)
 
			enddo !k=1,n  23074
 
		enddo !iobs=1,ndim1  23065
		deallocate(ivals)
	endif !if(ivin.ge.0)  23064
	if(ivin.eq.0)then
		!read(j_nul(j_ninc),'(a)')loppu
 
		if(loppu(1:1).ne.'/')then
			write(6,*)'**not legal end for bitmatrix -values'//loppu
			j_err=.true.
		end if !if(loppu(1:1).ne.'/')  23098
 
	end if !if(ivin.eq.0)  23095
 
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
	endif !if(j.gt.j_o(irg)%i(1).or.j.le.0)  23137
 
	select case(narg)
	case(3) !select case(narg)
	if(j_o(irg)%i(2).ne.j_o(irg)%i(3))then
		call j_printname('**setvalue(',irg,' syntax: setvalue(bitmatrix,row,column,value) ')
		j_err=.true.;return
	endif !if(j_o(irg)%i(2).ne.j_o(irg)%i(3))  23145
	icol=j_o(irg)%i(2)
 
	case(4) !select case(narg)
	icol=j_v(j_o(iob)%i(io+4))
	if(icol.lt.j_o(irg)%i(3).or.icol.gt.j_o(irg)%i(2))then
		call j_printname('setvalue(',irg,' ')
		write(6,*)'**illegal second argument',j_v(j_o(iob)%i(io+4))
		j_err=.true. ;return
	endif !if(icol.lt.j_o(irg)%i(3).or.icol.gt.j_o(irg)%i(2))  23153
 
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
	endif !if(j_o(irg)%i(3).ne.1.or.j_o(irg)%i(2).ne.j_o(irg)%i(1))  23193
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
			endif !if(i.eq.j)  23205
		enddo !j=1,ndim  23204
	enddo !i=1,ndim  23203
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
 
					endif !if(all(itemp(j,nei(lkm,1:nn))))  23230
				endif !if(itemp(i,j))  23227
			endif !if(i.eq.j)  23219
 
		enddo !j=1,ndim  23218
		! checkk if the same as previous
				15 continue !write(6,*)'i,key2,nn2',i,key2,nn2
		do k=1,lkm-1
			if(nn.ne.nnn(k))cycle
			if(all(nei(k,1:nn).eq.nei(lkm,1:nn)))goto 17
		enddo !k=1,lkm-1  23243
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
 
						endif !if(all(itemp(j,nei(lkm,1:nn))))  23266
 
					endif !if(itemp(key,j))  23263
				endif !if(i.eq.j)  23258
			enddo !jj=1,nn2  23256
			goto 15
 
		endif !if(key2.gt.0)  23251
 
	enddo !i=1,ndim  23214
 
	lkm=lkm-1
 
	call j_getobject2(iout,j_ipbitmatrix)
	nel=(ndim*lkm+31)/32
 
	allocate(j_o(iout)%i(0:5),j_o(iout)%i2(1:nel))
 
	j_o(iout)%i=(/nel,lkm,ndim,1,ndim,0/)
	j_o(iout)%i2=0
	do i=1,lkm
		do j=1,nnn(i)
			call j_bitset(iout,i,nei(i,j),1.)
		enddo !j=1,nnn(i)  23294
	enddo !i=1,lkm  23293
 
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
	! Section difference difference() Difference of LISTs
	! difference() removes elements from a LIST
	! endheader
	! Option
	! Output& 1&LIST & the generated LIST.
	! Args&2&LIST|OBJ & The first argument is the LIST from which the elements of of the
	! are removed  If second argument is LIST then all of its eleemts are remove, other wise
	! it is assumed that the second argument is an object which is remode from the lisrt.
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
	endif !if(j_otype(irg1).ne.j_iplist)  23344
	if(j_otype(irg2).ne.j_iplist)then
		ii2=j_inlistobject(irg2,irg1)
		if(ii2.le.0)then
			call j_printname('**difference: 2.argument ',irg2, ' not in  list:')
			call j_printname(' ',irg1,'  nothing done')
			return
		endif !if(ii2.le.0)  23351
		if(ii2.lt.j_o(irg1)%i(1))then
			do ij=ii2,j_o(irg1)%i(1)-1
				j_o(irg1)%i2(ij)=j_o(irg1)%i2(ij+1) !matrix operation may fail in optimized code
			enddo !ij=ii2,j_o(irg1)%i(1)-1  23357
		endif !if(ii2.lt.j_o(irg1)%i(1))  23356
		j_o(irg1)%i(1)=j_o(irg1)%i(1)-1   ! there remains unused elements
		return
	endif !if(j_otype(irg2).ne.j_iplist)  23349
 
 
	nel= j_ndiffer(j_o(irg1)%i2,j_o(irg1)%i(1),j_o(irg2)%i2,j_o(irg2)%i(1))
 
 
	if(nel.gt.0)then
		allocate( output(0:nel ))
		!call j_differ(j_o(irg1)%i,j_o(irg1)%i(0),j_o(irg2)%i,j_o(irg2)%i(0),j_o(iout)%i,j_o(iout)%i(0))
		call j_differ(j_o(irg1)%i2,j_o(irg1)%i(1),j_o(irg2)%i2,j_o(irg2)%i(1),output,noutput)
 
	endif !if(nel.gt.0)  23369
 
	call j_deflistobject(iout,' ',ivout,list0=nel,list=output)
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
	! Section interpolate interpolate() Linear interpolation
	! Usage://
	! interpolate(x0,x1[,x2],y0,y1[,y2],x]//
	! If arguments x2 and y2 are given then computes the value of the quadratic function at value
	! x going through the three points, otherwise computes the value of the linear function at value
	! x going through the two points.
	! endheader
	! Note. The argument x need not be within the interval of given x values (thus the function also
	! extrapolates).
	! endnote
	! endsection
 
	integer, dimension(:), pointer :: argx,argy !arguments of the function
	double precision denom,c1,c2,arg
	logical ismatrix,found
	!	subroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	!call j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	!	io=io_
	ismatrix=.false.
 
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	np=narg/2
	if(narg.gt.2.and.2*np.eq.narg)then
		write(6,*)'*wrong number of arguments)'
		j_err=.true.
		return
	endif !if(narg.gt.2.and.2*np.eq.narg)  23426
	iout=j_o(iob)%i(io+2+narg)
	if(narg.gt.3)then
		argx=>j_o(iob)%i(io+2:io+1+np)
		argy=>j_o(iob)%i(io+np+2:io+1+narg)
	else !if(narg.gt.3)then
		argx=>j_o(iob)%i(io+2:io+1+narg)
	endif !if(narg.gt.3)  23432
	if(j_otype(iout).ne.j_ipreal)call j_del(iout)
	! x0,x1, (x2),y0,y1,(y2),x
	arg=j_v(j_o(iob)%i(io+1+narg))
	if(narg.eq.5)then
		! x0, x1 ,y0,y1
		! +2,  3,  4, 5
		! y0+(y1-y0)/(x1-x0)* (x-x0)
		!	denom=  j_v(j_o(iob)%i(io+3))-j_v(j_o(iob)%i(io+2))
		denom=  j_v(argx(2))-j_v(argx(1))
		if(denom.eq.0.d0)then
			write(6,*)'**interpolate, x1 and x2 are equal'
			j_err=.true.
			return
		endif !if(denom.eq.0.d0)  23447
		!	j_v(iout)= j_v( j_o(iob)%i(io+4) )+(arg-j_v( j_o(iob)%i(io+2)))*( j_v(j_o(iob)%i(io+5))-j_v(j_o(iob)%i(io+4)))/denom
 
		j_v(iout)= j_v(argy(1))+(arg-j_v( argx(1)))*( j_v(argy(2))-j_v(argy(1)) )/denom
		return
	elseif(narg.eq.7.and.j_linkoption(iob,io,j_mq).ge.0)then !if(narg.eq.5)then
		! x0, x1 , x2 y0 ,y1  y2
		! +2,  3,  4, 5   6   7
 
		!      function quad(x,x0,x1,x2,y0,y1,y2)
		!*quadratic interpolation for point x, Newton forward-Form
		!      c1=(y1-y0)/(x1-x0)
		!	c2=(y2-y0-c1*(x2-x0))/((x2-x0)*(x2-x1))
		!	quad=y0+c1*(x-x0)+c2*(x-x0)*(x-x1)
		!	denom=  j_v(j_o(iob)%i(io+3))-j_v(j_o(iob)%i(io+2))
		denom=  j_v(argx(2))-j_v(argx(1))
		if(denom.eq.0.d0)then
			write(6,*)'**interpolate, x1 and x2 are equal'
			j_err=.true.
			return
		endif !if(denom.eq.0.d0)  23467
		!	c1=( j_v(j_o(iob)%i(io+6))-j_v(j_o(iob)%i(io+5)))/denom
		c1=( j_v(argy(2))-j_v(argy(1)))/denom
		!		denom=(j_v(j_o(iob)%i(io+4))-j_v(j_o(iob)%i(io+2)))*(j_v(j_o(iob)%i(io+4))-j_v(j_o(iob)%i(io+3)) )
		denom=(j_v(argx(3))-j_v(argx(1)))*(j_v(argx(1))-j_v(argx(2)) )
		if(denom.eq.0.d0)then
			write(6,*)'**interpolate, x1, x2,x3 are not different'
			j_err=.true.
			return
		endif !if(denom.eq.0.d0)  23476
		!	c2=( j_v(j_o(iob)%i(io+7)) - j_v(j_o(iob)%i(io+5)) -c1*(j_v(j_o(iob)%i(io+4))-j_v(j_o(iob)%i(io+2)) )) / denom
 
		c2=( j_v(argy(3)) - j_v(argy(1)) -c1*(j_v(argx(3))-j_v(argx(1)) )) / denom
		!j_v(iout)=j_v(j_o(iob)%i(io+5))+c1*(arg-j_v(j_o(iob)%i(io+2))) +c2*(arg-j_v(j_o(iob)%i(io+2)))*(arg-j_v(j_o(iob)%i(io+3)))
		j_v(iout)=j_v(argy(1))+c1*(arg-j_v(argx(1))) +c2*(arg-j_v(argx(1)))*(arg-j_v(argx(2)))
		call j_clearoption(iob,io)
		return
	elseif(narg.eq.2.and.j_otype(argx(1)).eq.j_ipmatrix)then !if(narg.eq.5)then
		if(j_o(argx(1))%i(1).lt.2.or.j_o(argx(1))%i(2).lt.2)then
			call j_printname('matrix ', argx(1),' cannot be interpolated')
			j_err=.true.
			return
		endif !if(j_o(argx(1))%i(1).lt.2.or.j_o(argx(1))%i(2).lt.2)  23489
		irow=j_igetopt(iob,io,j_mrow)
		if(irow.le.0)then
			ibasy=j_o(argx(1))%i(2)
		else !if(irow.le.0)then
			ibasy=(irow-1)*j_o(argx(1))%i(2)
		endif !if(irow.le.0)  23495
		ismatrix=.true.
		iy=argx(1)
		nval=j_o(argx(1))%i(2)
	elseif(narg.eq.3)then !if(narg.eq.5)then
		if(j_otype(argx(1)).ne.j_ipmatrix.or.j_otype(argx(2)).ne.j_ipmatrix)then
			write(6,*)'*illegal arguments'
			j_err=.true.
			return
		endif !if(j_otype(argx(1)).ne.j_ipmatrix.or.j_otype(argx(2)).ne.j  23504
		iy=argx(2)
		ibasy=0
 
		if(j_o(argx(1))%i(1).gt.1.and.j_o(argx(1))%i(2).gt.1.or. &
				j_o(argx(2))%i(1).gt.1.and.j_o(argx(2))%i(2).gt.1)then
			write(6,*)'first 2 arguments must be vectors'
			j_err=.true.
			return
 
		endif !if(j_o(argx(1))%i(1).gt.1.and.j_o(argx(1))%i(2).gt.1.o  23512
 
 
 
 
	endif !if(narg.eq.5)  23441
	found=.false.
	if(ismatrix)then
 
		do i=1,nval-1
			if(arg.ge.j_o(argx(1))%d(i).and.arg.le.j_o(argx(1))%d(i+1).or. &
					arg.le.j_o(argx(1))%d(i).and.arg.ge.j_o(argx(1))%d(i+1))then
				!	i1=min(i+1,nval)
				!	if(j_o(argx(1))%d(i).lt.j_o(argx(1))%d(max(i-1,1)))i1=i1-1
				i1=i
				found=.true.
				exit
			endif !if(arg.ge.j_o(argx(1))%d(i).and.arg.le.j_o(argx(1))%d(i+1)  23528
 
		enddo !i=1,nval-1  23527
 
		if(found)j_v(iout)=j_rlinter(j_o(argx(1))%d(i),j_o(argx(1))%d(i+1),&
			j_o(iy)%d(ibasy+i),j_o(iy)%d(ibasy+i+1),arg)
 
	else !if(ismatrix)then
 
		do i=1,np-1
			if(arg.ge.j_v(argx(i)).and.arg.le.j_v(argx(i+1)).or. &
					arg.le.j_v(argx(i)).and.arg.ge.j_v(argx(i+1)))then
				found=.true.
				!		if(j_v(argx(1)).lt.j_v(argx(max(i-1,1))))i1=i1-1
				exit
			endif !if(arg.ge.j_v(argx(i)).and.arg.le.j_v(argx(i+1)).o  23545
		enddo !i=1,np-1  23544
 
		if(found)j_v(iout)=j_rlinter(j_v(argx(i)),j_v(argx(i+1)),&
			j_v(argy(i)),j_v(argy(i+1)),arg)
 
	endif !if(ismatrix)  23525
	if(.not.found)then
		write(6,*)'no part found for ',arg
		j_err=.true.
 
	endif !if(.not.found)  23557
 
 
 
	call j_clearoption(iob,io)
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
		endif !if(j_v(irg2).ge.j_0)  23578
	elseif(j_otype(irg).eq.j_ipmatrix.and.j_otype(irg2).eq.j_ipreal)then
		call j_copy(irg,iout)
		nd=j_o(irg)%i(3)
		if(j_v(irg2).ge.j_0)then
			j_o(iout)%d(1:nd)=abs(j_o(irg)%d(1:nd))
		else !if(j_v(j_o(iob)%i(io+2)).ge.0)then
			j_o(iout)%d(1:nd)=-abs(j_o(irg)%d(1:nd))
		endif !if(j_v(irg2).ge.j_0)  23586
	else
		call j_getname(irg,irg2)
		write(6,*)'sign(), illegal arguments ', j_oname(1:j_loname),' ',j_oname2(1:j_loname2)
		j_err=.true.
	endif !if(j_otype(irg).eq.j_ipreal.and.j_otype(irg2).eq.j_ipreal)  23576
	!io=io+narg+3
	return
end subroutine !subroutine sign(iob,io)
 
subroutine getobs(iob,io)   !this is j_function
	integer*8::iobs
	! Section getobs getobs() Obsevarion from  DATA
	! Getting an observation from a data set: //
	! getobs(dataset,obs[,trans->])//
	! Get the values of all variables associated with observation obs in data object dataset. First all the
	! variables stored in row obs in the data matrix are put into the corresponding real variables. If
	! a transformation set is permanently associated with the data object, these transformations are
	! executed.
	! endheader
	! Option
	! dataset&1&DATA & the DATA
	! obs &1& REAL& row number in the data matrix of the dataset
	! trans&-1|1 & TRANS & these transformations are also executed.
	! endoption
	! endsection
 
 
	logical listarg
	logical arg
	!	io=io_
	narg=j_o(iob)%i(io+1)
	!	io_=io_+narg+3
	ivdata=j_o(iob)%i(io+2)
	iobs=j_v(j_o(iob)%i(io+3))
 
	if(j_otype(ivdata).ne.j_ipdata)then
		call j_printname('**getobs ',ivdata, 'is not a data set')
		j_err=.true.
		return
	endif !if(j_otype(ivdata).ne.j_ipdata)  23626
	nobs=j_o( j_o(ivdata)%i(1) )%i(1)
	if(iobs.le.0.or.iobs.gt.nobs )then
		write(6,*)'**getobs: is asking observation ',iobs,' but maximum is ',nobs
		call j_printname('**data set was',ivdata, ' ')
		j_err=.true.
		return
	endif !if(iobs.le.0.or.iobs.gt.nobs )  23632
	ivtrans=j_igetopt(iob,io,j_mtrans)
	call j_getobs0(ivdata,iobs)
	if(j_err)return
	if(ivtrans.gt.0)then
		if(j_otype(ivtrans).ne.j_iptrans)then
			call j_printname('**getobs, trans->',ivtrans, ' is not transformation set ')
			j_err=.true.
			return
		endif !if(j_otype(ivtrans).ne.j_iptrans)  23642
		call dotrans(ivtrans,1)
		if(j_err)return
		call j_clearoption(iob,io)  ! subroutine
	endif !if(ivtrans.gt.0)  23641
 
	return
end subroutine !subroutine getobs(iob,io)
 
subroutine der(iob,io)
	! Section der der() Derivatives
	! Derivates of a function with respect to any of its arguments can be
	! computed using the derivation rules by using der() function in the previous line. The funcion must be expressed with
	! one-line statement. The function can call other functions using the standard way
	! to obtain objects from transformations, but these functions cannot
	! contain variables for which derivatives are obtained.
	! Nonlinear regression needs the derivatives with respect to the parameters.
	! endheader
	! Option
	! Output &  &  & The der() function does not have an explicit output, but
	!  der() accompanied with the function produces REAL ]d[[] variable for each of the
	! argument variables.
	!Args &1- & REAL &  ]d[[Argi] variable will get the value of the derivative wiht
	! respect to the argument ]Argi[.
	!endoption
	! Ex derex Derivatives with der()
	! transa=trans()
	! der(x)
	! f=(1+x)*cos(x)
	! /
	! fi=draw(func->transa(f\x),x->x,xrange->(0,10),color->Blue,continue->)
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
		! the other te,mporal outputs.
		!finally the value of the function is put into the named output
		if(io2.eq.iexitloc)then
			iout=ioutmax+1   !last output is addtional temporal
			finito=.true.
			!	write(6,*)'iout0 ',iout
		else !if(io2.eq.iexitloc)then
			iout=j_o(iob)%i(io2)
			!		write(6,*)'ioutmax,iout',ioutmax,iout
			ioutmax=max(ioutmax,iout)
		endif !if(io2.eq.iexitloc)  23750
		! write(6,*)'<3738ifunc,narg,io2,iout,ioutmax,j_mxnamedv',ifunc,&
		! narg,io2,iout,ioutmax,j_mxnamedv
		! write(6,*)'io,io2,ifunc,narg,iout,ioutmax',io,io2,ifunc,narg,iout,ioutmax
		! write(6,'(30i5)')j_o(iob)%i(1:io+10)
		! write(6,*)'allo ',allocated(j_o(iout)%d)
 
		! write(6,*)'iout,nd ',iout,nd
		j_o(iout)%d(1:nd)=j_0
		irg=j_o(iob)%i(io+2)
		u=j_v(irg)  !
		isvar=irg.le.j_mxnamedv.or.irg.gt.j_nv  !is first argument named variable or constant
		isilo=.false.
		if(isvar)then
			do il=1,nd
				if(irg.eq.arg(il))goto 135
			enddo !il=1,nd  23772
			il=0
			!			il=findloc(arg,irg,dim=1)
			! il=ilo(1)
135				isilo=il.gt.0  ! is first argument in der (...,possible only for named variable
		endif !if(isvar)  23771
		isarg2=narg.gt.1  !is there second argumenr
		if(isarg2)then
			irg2=j_o(iob)%i(io+3)
			v=j_v(irg2)
			isvar2=irg2.le.j_mxnamedv.or.irg2.gt.j_nv
			if(isvar2)then
				do il2=1,nd
					if(irg2.eq.arg(il2))goto 136
				enddo !il2=1,nd  23786
				il2=0
				!			il=findloc(arg,irg,dim=1)
				! il=ilo(1)
136					isilo2=il2.gt.0  !is second argument in der () ,il2 is then the position



			endif !if(isvar2)  23785
		endif !if(isarg2)  23781
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
		if(.not.isvar)then  !first argument intermediate result
			do id=1,nd
				j_o(iout)%d(id)=j_o(irg)%d(id)
			enddo !id=1,nd  23813
		elseif(isilo)then !if(.not.isvar)then first argument il'th der var
			!		write(6,*)'<56554 ',iout,il
			j_o(iout)%d(il)=j_1   !du =1
 
		endif !if(.not.isvar)  23812
 
		if(.not.isvar2)then !for second variable it must be taken into account that the first argument has
			! already provided
			do id=1,nd
				j_o(iout)%d(id)=j_o(iout)%d(id)+j_o(irg2)%d(id)
			enddo !id=1,nd  23824
		elseif(isilo2)then !if(.not.isvar2)then
			j_o(iout)%d(il2)=j_o(iout)%d(il2)+j_1
		endif !if(.not.isvar2)  23822
 
		case (j_fminus) !select case (ifunc)
		!	write(6,*)'iobi',j_o(iob)%i(io:io+4)
		!	write(6,*)'narg,isarg2,isvar,isvar2,isilo,isilo2,il,il2',narg,isarg2,isvar,isvar2,isilo,isilo2,il,il2
		!d(u-v)=du-dv  ;
		j_v(iout)=u-v
		if(.not.isvar)then  !first argument intermediate result
			do id=1,nd
				j_o(iout)%d(id)=j_o(irg)%d(id)
			enddo !id=1,nd  23837
		elseif(isilo)then !if(.not.isvar)then
			!		write(6,*)'<56554 ',iout,il
			j_o(iout)%d(il)=j_1
 
		endif !if(.not.isvar)  23836
 
		if(.not.isvar2)then
			do id=1,nd
				j_o(iout)%d(id)=j_o(iout)%d(id)-j_o(irg2)%d(id)
			enddo !id=1,nd  23847
		elseif(isilo2)then !if(.not.isvar2)then
			j_o(iout)%d(il2)=j_o(iout)%d(il2)-j_1
		endif !if(.not.isvar2)  23846
 
 
 
		case (j_fmult) !select case (ifunc)
		!	d(uv)=v*du  +u*dv
		!write(6,*)'put ',iout,'u*v',u,v,u*v,isvar,isvar2,isilo,isilo2
		j_v(iout)=u*v
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=v*j_o(irg)%d(id)
			enddo !id=1,nd  23861
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=v
		endif !if(.not.isvar)  23860
 
		if(.not.isvar2)then  !u dv
			do id=1,nd
				j_o(iout)%d(id)=j_o(iout)%d(id)+u*j_o(irg2)%d(id)
			enddo !id=1,nd  23869
		elseif(isilo2)then !if(.not.isvar2)then
			j_o(iout)%d(il2)=j_o(iout)%d(il2)+u
		endif !if(.not.isvar2)  23868
 
		!write(6,*)'mult ',real(j_v(irg)),real(j_v(irg2)),isvar,isilo,ilo,isvar2,isilo2,ilo2,j_o(iout)%d(1:nd)
 
		case (j_fdiv) !select case (ifunc)
		! d u/v=  1/v * du -u/v**2 dv
 
		if(v.eq.j_0)then
			write(6,*)'division with zero'
			j_err=.true.
			return
		endif !if(v.eq.j_0)  23881
 
		j_v(iout)=u/v
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=j_o(irg)%d(id)/v
			enddo !id=1,nd  23889
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/v
		endif !if(.not.isvar)  23888
 
		if(.not.isvar2)then
			do id=1,nd
				j_o(iout)%d(id)=j_o(iout)%d(id)-j_o(irg2)%d(id)*u/v**2
			enddo !id=1,nd  23897
		elseif(isilo2)then !if(.not.isvar2)then
			j_o(iout)%d(il2)=j_o(iout)%d(il2)-u/v**2
		endif !if(.not.isvar2)  23896
 
		case (j_fipower) !select case (ifunc)
		!d u**n =(n-1)*u**(n-1) du
		n=v
		j_v(iout)=u**n
		if(.not.isvar)then
			do id=1,nd
				!j_o(iout)%d(id)=(v-j_1)*u**(n-1)*j_o(irg)%d(id)
				j_o(iout)%d(id)=(v-j_1)*j_v(iout)*j_o(irg)%d(id)/u
			enddo !id=1,nd  23909
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=(v-j_1)*j_v(iout)/u  !(v-j_1)*u**(n-1)
		endif !if(.not.isvar)  23908
 
		case (j_fpower) !select case (ifunc)
		! d u**v =v*u**(v-1) du + (log u)*u**v dv
		if(u.lt.j_0)then
			write(6,*)'power has negative argument ',u
			j_err=.true.;return
		endif !if(u.lt.j_0)  23919
 
		j_v(iout)=u**v
 
		if(.not.isvar)then
			do id=1,nd
				!	j_o(iout)%d(id)=v*u**(v-j_1)*j_o(irg)%d(id)
				j_o(iout)%d(id)=v*j_v(iout)*j_o(irg)%d(id)/u
			enddo !id=1,nd  23927
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=v*j_v(iout)/u  !v*u**(v-j_1)
		endif !if(.not.isvar)  23926
 
		if(.not.isvar2)then
			do id=1,nd
				!	j_o(iout)%d(id)=j_o(iout)%d(id)+log(u)*u**v*j_o(irg2)%d(id)
				j_o(iout)%d(id)=j_o(iout)%d(id)+log(u)*j_v(iout)*j_o(irg2)%d(id)
			enddo !id=1,nd  23936
		elseif(isilo2)then !if(.not.isvar2)then
			!		write(6,*)'66iout,j_mxnamedv+j_mxtemporalv0',iout,j_mxnamedv+j_mxtemporalv0
			j_o(iout)%d(il2)=j_o(iout)%d(il2)+log(u)*j_v(iout)
		endif !if(.not.isvar2)  23935
 
		case (j_fexp) !select case (ifunc)
 
		j_v(iout)=exp(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=j_v(iout)*j_o(irg)%d(id)
			enddo !id=1,nd  23949
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_v(iout)
		endif !if(.not.isvar)  23948
 
		case (j_flogistic) !select case (ifunc)
 
		if(u.gt.j_0)then
			j_v(iout)=j_1/(j_1+exp(-u))
		else !if(u.gt.j_0)then
			j_v(iout)=exp(u)/(j_1+exp(u))
		endif !if(u.gt.j_0)  23958
 
		if(.not.isvar)then
			!		write(6,*)'ilo',isilo
			do id=1,nd
				!			write(6,*)'<44',j_o(irg)%d(id)
				j_o(iout)%d(id)=j_v(iout)*(j_1-j_v(iout))*j_o(irg)%d(id)
			enddo !id=1,nd  23966
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_v(iout)*(j_1-j_v(iout))
		endif !if(.not.isvar)  23964
 
		case (j_fsqrt) !select case (ifunc)
		if(u.lt.j_0)then
			write(6,*)'sqrt has nonpostive argument ',u
			j_err=.true.;return
		endif !if(u.lt.j_0)  23975
		j_v(iout)=sqrt(u)
		if(.not.isvar)then
			do id=1,nd
				!	j_o(iout)%d(id)=v*u**(v-j_1)*j_o(irg)%d(id)
				j_o(iout)%d(id)=j_o(irg)%d(id)/(2.d0*j_v(iout))
			enddo !id=1,nd  23981
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/(2.d0*j_v(iout))  !v*u**(v-j_1)
		endif !if(.not.isvar)  23980
		case (j_fsqrt2) !select case (ifunc)
 
		v=abs(u)
		j_v(iout)=sqrt(v)
		if(.not.isvar)then
			do id=1,nd
				!	j_o(iout)%d(id)=v*u**(v-j_1)*j_o(irg)%d(id)
				j_o(iout)%d(id)=j_o(irg)%d(id)/(2.d0*j_v(iout))
			enddo !id=1,nd  23993
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/(2.d0*j_v(iout))  !v*u**(v-j_1)
		endif !if(.not.isvar)  23992
		if(u.lt.j_0)j_v(iout)=-j_v(iout)
 
		case (j_fsin) !select case (ifunc)
		j_v(iout)=sin(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=cos(u)*j_o(irg)%d(id)
			enddo !id=1,nd  24005
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=cos(u)
		endif !if(.not.isvar)  24004
 
		case (j_fcos) !select case (ifunc)
		j_v(iout)=cos(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=-sin(u)*j_o(irg)%d(id)
			enddo !id=1,nd  24015
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=-sin(u)
		endif !if(.not.isvar)  24014
 
		case (j_flog) !select case (ifunc)
		j_v(iout)=log(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=j_o(irg)%d(id)/u
			enddo !id=1,nd  24025
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/u
		endif !if(.not.isvar)  24024
 
		case (j_flog10) !select case (ifunc)
		j_v(iout)=dlog10(u)
		if(.not.isvar)then
			do id=1,nd
				j_o(iout)%d(id)=dlog10(j_e)*j_o(irg)%d(id)/u
			enddo !id=1,nd  24035
		elseif(isilo)then !if(.not.isvar)then
			j_o(iout)%d(il)=j_1/u
		endif !if(.not.isvar)  24034
 
 
 
		case default !select case (ifunc)
		write(6,*)'derivative for function ',j_functions(ifunc),' not yet implemented, ask J. Lappi'
		!	write(6,*)j_flogistic,ifunc
		j_err=.true.
		return
		end select !select case (ifunc)
		if(finito)exit
		io=io+narg+3
	enddo !while(.true.)  23737
	!		write(6,*)'fin',ioutfin,iout
	j_v(ioutfin)=j_v(iout)
	do i=1,nd
		!	write(6,*)'73737deric',deriv(i)
		j_v(deriv(i))=j_o(iout)%d(i)
		!	write(6,*)'73737deric',deriv(i),j_v(deriv(i))
	enddo !i=1,nd  24055
 
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
			call j_putod(j_ivtracestatus,il,0.d0)
		endif !if(il.le.0)  24071
	enddo !i=1,narg  24069
 
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
		endif !if(nmin.eq.1)  24101
		ismin=iexit
	endif !if(imi.gt.0)  24098
 
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
		endif !if(nmax.eq.1)  24117
		ismax=iexit
	endif !if(imi.gt.0)  24115
	if(nmax.lt.0.and.nmin.lt.0.and.iexit.gt.1)then
		write(6,*)'trace:, errexit must be with min-> or max->'
 
	endif !if(nmax.lt.0.and.nmin.lt.0.and.iexit.gt.1)  24127
 
	tracelevel=-1.
 
	if(nmin.gt.0.or.nmax.gt.0)tracelevel=1
	ivlevel=j_igetopt(iob,io,j_mlevel)
 
	if(ivlevel.gt.0)then
		tracelevel=j_v(ivlevel)
		if(tracelevel.lt.0.or.tracelevel.gt.2)then
			write(6,*)'trace: illegal level-> ' ,tracelevel
		endif !if(tracelevel.lt.0.or.tracelevel.gt.2)  24139
 
	endif !if(ivlevel.gt.0)  24137
 
	do i=1,narg
		il=j_inlistobject(j_o(iob)%i(io+1+i),j_ivtracevars)
		if(il.le.0)then
			call j_printname('*  trying to trace() variabe ',j_o(iob)%i(io+1+i),' which was not in ;trace')
			j_err=.true.
			return
		endif !if(il.le.0)  24147
 
		if(tracelevel.ge.0)call j_putod(j_ivtracelevel,il,tracelevel)
		call j_putod(j_ivtracecount,il,0.d0)
		if(nmin.ge.0)then
			call j_putod(j_ivtraceminstatus,il,ismin)
			if(nmin.gt.1)valmin=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmin)+i))
			call j_putod(j_ivtracemin,il,valmin) !real value
		endif !if(nmin.ge.0)  24155
		if(nmax.ge.0)then
			call j_putod(j_ivtracemaxstatus,il,ismax)
			if(nmax.gt.1)valmax=j_v(j_o(iob)%i(j_linkoption(iob,io,j_mmax)+i))
			call j_putod(j_ivtracemax,il,valmax)
		endif !if(nmax.ge.0)  24160
	enddo !i=1,narg  24145
 
	call j_clearoption(iob,io)  ! subroutine
 
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
! write(6,*)name(1:le),' new ',j_objecttypes(j_otype(iv)), ' in ',name2(1:le2), ' at line ',lin,' :'
! endif !if(j_otype(iv).eq.j_ipreal)  23928
! call j_printtext(j_o(iob)%i2(11),lin)
! else !if(lin.gt.0)then
! if(j_otype(iv).eq.j_ipreal)then
! write(6,*)name(1:le),' got value ',j_v(iv)
! else !if(j_otype(iv).eq.j_ipreal)then
! write(6,*)name(1:le),' new ',j_objecttypes(j_otype(iv))
 
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
	endif !if(j_otype(irg).ne.j_ipchar)  24279
	call j_getchar(irg,j_filename,le)
	if(j_linkoption(iob,io,j_mobject).ge.0)then
		ex=j_object(j_filename(1:le)).gt.0
	else !if(j_linkoption(iob,io,j_mobject).ge.0)then
		inquire(file=j_filename(1:le),exist=ex)
	endif !if(j_linkoption(iob,io,j_mobject).ge.0)  24285
	call j_clearoption(iob,io)  ! subroutine
 
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
	endif !if(j_otype(irg).ne.j_ipchar)  24307
	call j_getchar(irg,j_filename,le)
	if(j_linkoption(iob,io,j_mobject).ge.0)then
		iobj=j_object(j_filename(1:le))
		if(iobj.gt.0)iobj=j_otype(iobj)
		j_v(iout)=iobj
		call j_clearoption(iob,io)  ! subroutin
	else !if(j_linkoption(iob,io,j_mobject).ge.0)then
		inquire(file=j_filename(1:le),exist=ex)
		if(ex)j_v(iout)=1.d0
	endif !if(j_linkoption(iob,io,j_mobject).ge.0)  24313
 
 
 
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
	endif !if(j_otype(iout).le.j_ipreal)  24369
 
	select case (narg)
	case (3) !select case (narg)
	call crk(ipl,dbh,0.d0,j_v(j_o(iob)%i(io+4)),0.d0,j_o(iout)%d)
	case(4) !select case (narg)
	call crk(ipl,dbh,j_v(j_o(iob)%i(io+4)),j_v(j_o(iob)%i(io+5)),0.d0,j_o(iout)%d)
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
 
	endif !if(arg.lt.0)  24399
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
 
	endif !if(arg.lt.0)  24424
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
 
	endif !if(arg.lt.j_0.or.amyy.lt.j_0.or.ak.lt.j_0)  24459
	if(amyy.eq.j_0)then
		if(arg.eq.j_0)then
			j_v(iout)=j_1
		else
			j_v(iout)=j_0
		endif !if(arg.eq.j_0)  24467
	endif !if(amyy.eq.j_0)  24466
	if(ak.eq.j_0)then
		iarg=arg
		if(dble(iarg).ne.iarg)then
			write(6,*)'with Poisson distribution, i.e., theta=0 first argument should be integer ',arg
			j_err=.true.
			return
		endif !if(dble(iarg).ne.iarg)  24475
		! kert=j_1
		! do i=1,iarg
		! kert=kert+kert*i
		! enddo !do i=1,iarg
		! !	write(6,*)'<44',amyy**iarg*exp(-amyy)/kert
		kert=j_0
		do i=2,iarg
			kert=kert+log(dble(i))   !kert*i
		enddo !i=2,iarg  24486
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
	endif !if(ak.eq.j_0)  24473
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
	! cl=classify(x->x,xrange->)
	! fi=drawclass(cl,histogram->,classes->20,continue->fcont)
	! den=density(cl);
	! fi=drawline(den,continue->fcont)
	! endex
	! endsection
 
	double precision ::xx,scale,sum,dd,xmin,xmax,yvalue
	integer, dimension(:), pointer :: arg !arguments of the funct
	logical ::isdisc
	call  j_startfunction(iob,io,j_ipmatrix,narg,arg,iout)
	if(j_err)return
	linkfunc=j_codelink(iob,io,j_mfunc)
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
			iout=j_defmatrix(iout,' ',2,ncol,ity)
			j_v(j_ivdollar)=xmin
			do i=1,ncol
				j_o(iout)%d(i)=j_v(j_ivdollar)
				sum=sum+j_codevalue(iob,linkfunc)
				if(j_err)return
				j_o(iout)%d(ncol+i)=sum
				j_v(j_ivdollar)=j_v(j_ivdollar)+j_1
			enddo !i=1,ncol  24577
			do i=1,ncol
				j_o(iout)%d(ncol+i)=j_o(iout)%d(ncol+i)/sum
			enddo !i=1,ncol  24584
 
		else
			ncol=101
			dd=(xmax-xmin)/ncol
			iout=j_defmatrix(iout,' ',2,ncol,ity)
			j_o(iout)%d(1)=xmin
			j_o(iout)%d(2)=j_0
			j_v(j_ivdollar)=xmin
 
			do i=2,ncol
				j_o(iout)%d(i)=j_v(j_ivdollar)
				sum=sum+j_codevalue(iob,linkfunc)
				if(j_err)return
				j_o(iout)%d(ncol+i)=sum
				j_v(j_ivdollar)=j_v(j_ivdollar)+dd
			enddo !i=2,ncol  24596
			do i=1,ncol
				j_o(iout)%d(ncol+i)=j_o(iout)%d(ncol+i)/sum
			enddo !i=1,ncol  24603
		endif !if(isdisc)  24573
		return
 
	elseif(narg.eq.1)then
		if(j_o(arg(1))%i(4).ne.j_matclass)then
			call j_printname('**density: ',arg(1),' not a produced by classify')
			j_err=.true. ;return
			!goto 900
		endif !if(j_o(arg(1))%i(4).ne.j_matclass)  24610
		nrow_=j_o(arg(1))%i(1)
		ncol=j_o(arg(1))%i(2)
		xmin=j_o(arg(1))%d(nrow_*ncol+1)
		dd=j_o(arg(1))%d(nrow_*ncol+2)
		xx=xmin
		!			if(area)then
		!				scale=1./(j_o(iarg)%d(2*ncol)*dd)
		!			else !if(area)then
		!	scale=100.d0/j_o(iarg)%d(2*ncol)
		scale=j_1/j_o(arg(1))%d(2*ncol)
 
		iout=j_defmatrix(iout,' ',2,ncol,ity)
		sum=j_0
		j_o(iout)%d(1)=xmin
		j_o(iout)%d(ncol+1)=j_0
		do i=1,ncol-1
			sum=sum+scale*j_getmatel(arg(1),2,i)
			!	if(i.eq.1)call j_putfigxy(xx,yy)
			!call j_putfigxy(xx,yy)
			xx=xx+dd
			j_o(iout)%d(i+1)=xx
			j_o(iout)%d(ncol+i+1)=sum
		enddo !i=1,ncol-1  24630
		return
		!		endif !if(area)then
	else
 
		write(6,*)'**density needs func-> or argument produced with classify()'
		j_err=.true.
		return
	endif !if(linkfunc.gt.0)  24567
 
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
	if(k.lt.0.d0)then
		write(6,*)'*bin, argument should be >=0, is ',arg
		j_err=.true.
		return
 
	endif !if(k.lt.0.d0)  24666
	ios=1
	do i=k2+1,n
		ios=ios*i
	enddo !i=k2+1,n  24673
	in=1
	do i=1,nk
		in=in*i
	enddo !i=1,nk  24677
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
	endif !if(j_otype(irg).eq.j_ipreal)  24711
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
	endif !if(j_otype(irg).eq.j_ipreal)  24731
end subroutine !subroutine fraction_(iob,io)
 
 
 
 
 
 
 
 
 
