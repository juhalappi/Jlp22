! Copyright (C) 2022 Juha Lappi
! This is published using MIT licence
 
 
!+--! with gfortran jpre.exe can be made in foleder Jpre  using: make -f Makefile_debug
 
!The precompiler generates necessary 'use' statments for global variables or subroutines stored
! in modules, makes indentations, writes comments to endif and enddo lines,
! and gives better error  messages than Gfortran if ifthens or do's
! are mixed up.
! The prefixes and the files to be precompiled are in the command file whose default name is jpre.txt.
! the program generates the output files indicated in the command file
!The currrent jpre.txt is :
 
! j_ jlp_ o1_ o2_ o3_
! R:jmodules.f90
! R:own1mod.f90
! R:own2mod.f90
! R:own3mod.f90
! R:jownmod.f90
! jutilities.f90
! jsysdep_gfortran.f90
! j.f90
! jlp.f90
! own1.f90
! own2.f90
! own3.f90
 
!R indicates that the file must be read in even if it is not precompiled.
!The precompiler generatates _2 files (e.g. jlp_2.f90) containing use statements and
! _old files containing the original file (e.g. jlp_old.f90 (the new file 
! with the same name as the input file will contain
! indentations and added comments). The _old file is made for security reasons as
! it there can be unanticipated error situations where not proper files are generated.
! If an error occurs, _3  file (e.g. jlp_3.f90) will contain the new file with indentations
! and comments.
 
 
 
 
module jeqmod !case insensitive character comp
	interface operator(.jeq.)
		function jeq(ch1,ch2)
			character(len=*),intent(in) :: ch1
			character(len=*),intent(in) :: ch2
			logical ::jeq
		end function jeq
	end interface
 
	interface operator(.jne.)
		function jne(ch1,ch2)
			character(len=*),intent(in) :: ch1
			character(len=*),intent(in) :: ch2
			logical ::jne
		end function jne
	end interface
 
	logical ::cont=.false.
	integer ::icont=0
	logical ::contcur=.false.
	logical ::contif=.false.
	logical ::contfirst=.false.
	
	!integer ::letot,le,ico
	character*(20):: tabs=char(9)//char(9)//char(9)//char(9)//char(9)//char(9)//&
		char(9)//char(9)//char(9)//char(9)// &
		char(9)//char(9)//char(9)//char(9)//char(9)//char(9)//&
		char(9)//char(9)//char(9)//char(9)
	!integer ::istart
	integer ::indent
	integer ::istep
	logical::isad
end module jeqmod
 
logical function jeq(ch1,ch2)
	character(len=*),intent(in) :: ch1
	character(len=*),intent(in) :: ch2
	integer :: le1,le2,i
	le1=jlentrim(ch1)
	le2=jlentrim(ch2)
	jeq=.false.
	if(le1.ne.le2)return
	do i=1,le1
 
		if(ch1(i:i).ge.'a'.and.ch1(i:i).le.'z')then
			if(ch2(i:i).ne.ch1(i:i).and.ichar(ch2(i:i)).ne.ichar(ch1(i:i))-32)return
		elseif(ch1(i:i).ge.'A'.and.ch1(i:i).le.'Z')then
			if(ch2(i:i).ne.ch1(i:i).and.ichar(ch2(i:i)).ne.ichar(ch1(i:i))+32)return
		else
			if((ch1(i:i).eq.' '.or.ch1(i:i).eq.char(9)).and.&
				(ch2(i:i).eq.' '.or.ch2(i:i).eq.char(9)))cycle
			if(ch1(i:i).ne.ch2(i:i))return
		endif !if(ch1(i:i).ge.'a'.and.ch1(i:i).le.'z')     80
	enddo !i=1,le1     78
	jeq=.true.
	return
 
 
end function jeq
 
logical function jne(ch1,ch2)
	use jeqmod, only: jeq
	character(len=*),intent(in) :: ch1
	character(len=*),intent(in) :: ch2
 
	jne=.not.jeq(ch1,ch2)
 
end function jne
 
integer function jlentrim(inp)   !like len-trim but returns zero aslo when line consists of tabs
	character(len=*),intent(in):: inp
	integer ::le_
	le_=len(inp)
 
	if(le_.le.0)then
		jlentrim=0
		return
	endif !if(le_.le.0)    110
	do jlentrim=le_,1,-1
		if(ichar(inp(jlentrim:jlentrim)).gt.32)return
	enddo !jlentrim=le_,1,-1    114
	jlentrim=0
	return
end function jlentrim !integer function j_lentrim(inp)
 
 
 
module pref
	integer,parameter :: maxprefix=6
	integer,parameter :: lenprefix=4
	character(len=lenprefix),dimension(maxprefix):: prefix
	integer , dimension(maxprefix) ::lprefix
	integer:: nprefix
	! integer,parameter ::maxfiles=20
	! integer,dimension(maxfiles)::fileprefix
	!logical,dimension(maxfiles)::isprefix=.false.
	integer::nfiles
end module
 
module writeout
	logical::write4
	logical::write3
end module
 
module sub
	logical hipsu
	integer, parameter :: maxused=10000
	integer,dimension(maxused):: used
	integer, parameter :: maxmodules=50
	integer ::nmodules=0
	integer, parameter :: maxsublines=20000     !per subroutine
	integer, parameter :: maxwords=50 !per line
	integer,parameter ::maxmodulelines=50000
	character*(200) inp
 
	integer::le,letot,ico,istart,istart2
	!istart start of nonbl characters,istart2 start when interpreting
 
	character*(200),dimension(maxsublines) ::subline
	integer,dimension(maxsublines)::lensubline
	integer ::nsubline
	integer::modulewords=0
	integer ::modulewords0
	integer ::nused
	integer,dimension(maxmodulelines) ::modulemod
	character*(50),dimension(maxmodulelines) ::moduleword
	integer ,dimension(maxmodulelines) :: lenmoduleword
	character*(50), dimension(maxmodules) :: modulename
	character*(50)::modulename0
	integer , dimension(maxmodules) ::lenmodulename
	character*60 subname,subname0,subnamecont
	integer::lensubname
	logical ::nopremod=.false.
	logical :: p=.false. !.true.
	integer ::linein=0
	logical ::infunc,isfunc
	logical,dimension(10000)::subsit
	integer,dimension(10000)::subslines
 
end module
module main
	character(len=300)inp,inp2
	logical exis,exisout,exisout0
	logical::end3
 
	! logical reserved
 
 
	logical :: p=.false. !.true.
	logical ::p2=.false.  !if <> written
 
 
	character*60 outfile,outfile0
 
	character*60::inpf
	integer ::if1,lef
 
 
 
 
end module
 
subroutine letotle(inp,letot,le,icom,istart) !letot=total length, le=command length, icom=start of comment
	!use jeqmod
 
	character*(160),intent(in):: inp
	integer, intent(out):: letot
	integer, intent(out):: le
	integer, intent(out):: icom
	logical::ishipsu
 
	letot=jlentrim(inp)
 
	if(letot.le.0)then
		le=0
		icom=0
		istart=0
 
		return
	endif !if(letot.le.0)    209
	! if(f77)then
	! if((inp(1:1).jeq.'c').or.inp(1:1).eq.'*'.or.inp(1:1).eq.'!')then
	! icom=1
	! else
	! icom=index(inp(1:letot),'!')
	! endif
 
	! else
	istart=nonblank(inp,1,letot)
	icom=index(inp(1:letot),'!')
	!write(6,*)'icom',icom
	if(icom.le.0)then
		le=letot
		return
	elseif(inp(istart:istart).eq.'!')then
 
		le=0
		return
	endif !if(icom.le.0)    227
	!check if ! is within a character constant
	ishipsu=.false.
	do icom=1,letot
		if(inp(icom:icom).eq."'")ishipsu=.not.ishipsu
		if(inp(icom:icom).eq.'!'.and..not.ishipsu)goto 9
	enddo !icom=1,letot    237
	!	write(6,*)inp(1:25),letot,istart
9	 do le=icom-1,1,-1

		if(inp(le:le).gt.' ')return
 
	enddo !do le=icom-1,1,-1    242
 
	!if(le.eq.1)write(6,*)'<687>ichar ',ichar(inp(1:1))
 
 
 
end subroutine letotle
 
 
 
 
program jpre
	!use koemod,only: koe2
	!	use jeqmod
	use pref
	use writeout !write2 and write4
	use main
 
	integer::time,time2
	integer(4), dimension(13)::buff
	integer(4)::status
	logical ::rw,rwtot
	logical ::exisf
	logical ::perhaps  !it is not certain whether a new _2 file is needed. This happens when
	! the .f90 file has not been changed but R file has been changed
	logical ::exisf2
!	character(len=300)inp,inp2
	!logical :: write3  !is update of original file needed
 
	! goto 175
 
	nun=0
!	must=.false.
	rwtot=.false.
	nfiles2=0
	nfiles22=0
	nfiles22f=0
	nfiles3=0
	! read(5,*)in
	! moduleword(in)='9'
	! write(6,*)'log written to file fort.4 (or similar name depending on compiler)'
	goto 1
11 write(6,*)'error opening file ',inp(1:le), ',iostat =',iostatus, ' try again'
call system('dir')
	stop
 
1 write(6,'(a,$)')'Give command file, default jpre.txt>'
	read(5,'(a)')inp
	le=len_trim_(inp)
	if(le.le.0)then
		open(10,file='jpre.txt',err=11,status='old')
	else
		open(10,file=inp(1:len_trim_(inp)),err=11,status='old')
	endif !if(le.le.0)    283
	nfiles=0
	read(10,'(a)')inp
	le=len_trim_(inp)
	write(6,*)inp(1:le)
	ib=index(inp(1:le),' ')
	!	write(6,*)'ib ',ib
	ial=1
	nprefix=0
!	nwritten=0
	ib=nextlim(inp,ial,le,' ')
	do while(ib.lt.le)
		nprefix=nprefix+1
		prefix(nprefix)=inp(ial:ib-1)
		lprefix(nprefix)=ib-ial
		ial=ib+1
		ib=nextlim(inp,ial,le,' ')
	enddo !while(ib.lt.le)    297
	nprefix=nprefix+1
	prefix(nprefix)=inp(ial:le)
	lprefix(nprefix)=le-ial+1
	write(6,*)'prefixes ',prefix(1:nprefix)
	goto 2
		
99 write(6,*)' '
	write(6,*)'***all files processed, '
	write(6,*)'indentations made for      ',nfiles3 ,' files '
	write(6,*)'_2 files made directly for ',nfiles2 ,' files '
	if(nfiles22f.gt.0)write(6,*)'_2 files made from _22 files due to changes of R files  ',nfiles22f ,' times '
	stop
2 read(10,'(a)',end=99)inpf
	lef=len_trim_(inpf)
	if(lef.eq.0)goto 2
	write(6,*)' '

	write4=.false.
	write3=.false.
!	perhaps=.false.
141		nfiles=nfiles+1
	if(inpf(1:2).eq.'R:')then
		if1=3
		rw=.true.

		write(6,*)'processing ',nfiles,'  ',inpf(if1:lef),' ACTION=R'
		!			elseif(inpf(1:3).eq.'RW:')then
		!			if1=4
		!			rw=.true.
		!			write(6,*)'processing ',nfiles,'  ',inpf(if1:lef),' ACTION=RW'
	else
		if1=1
		write(6,*)'processing ',nfiles,'  ',inpf(if1:lef),' ACTION=W'
		rw=.false.
	endif !141		if(inpf(1:2).eq.'R:')    317
 
 
 
	goto 15
110		write(6,*)'error in opening '
	stop
 
15		CALL STAT(inpf(if1:lef),BUFF,STATUS)
	write(6,*)'STATUS '//inpf(if1:lef),'  ',STATUS,' TIME ',BUFF(10)
	if(STATUS.ne.0)then
		write(6,*)'cannot open file ',inpf(if1:lef),' STATUS=',STATUS
		stop 'file does not exit'
	endif !if(STATUS.ne.0)    340
	!	if(write4.or.rw)then
	ipi=index(inpf(1:lef),'.')
	time=buff(10)
	!		write(6,*)'time of ',inpf(if1:lef),time
	CALL STAT(inpf(if1:ipi-1)//'_2'//inpf(ipi:lef),BUFF,STATUS)
	write(6,*)'STATUS '//inpf(if1:ipi-1)//'_2'//inpf(ipi:lef),STATUS,' TIME ',BUFF(10)
	
	time2=0
	if(STATUS.eq.0)time2=buff(10)

	!		write(6,*)'time of ',inpf(if1:ipi-1)//'_2'//inpf(ipi:lef),time2
	if(time2.ge.time.and..not.rwtot.and..not.rw)then
	
			write(6,*)'_2 file NEWER, no previous changed R file, no action needed'
			
			goto 2
	!	else
	elseif(time2.ge.time.and.rwtot)then
		write4=.true.
!		inquire(file=inpf(if1:ipi-1)//'_2'//inpf(ipi:lef),exist=exisf2)
	
		write(6,*)'_2 file NEWER, reading only ',inpf(if1:lef)
		write(6,*)' 	but writing _22 file because a previous R file changed'
		
		nfiles22=nfiles22+1
			
	write(6,*)'opening ',inpf(if1:ipi-1)//'_22'//inpf(ipi:lef),' ********for POTENTIAL REWRITING'
			open(unit=2,  err=144,file=inpf(if1:ipi-1)//'_22'//inpf(ipi:lef),&
			ACCESS='SEQUENTIAL',status='REPLACE',form='FORMATTED',action='WRITE', &
			iostat=ier)
	elseif(time2.lt.time)then
			if(STATUS.eq.0)then
				write(6,*)'_2 file OLDER, writing both _2 and _3 file'
				
			else
				write(6,*)'_2 file does not exist, writing both _2 and _3 file'
			
			endif
			nfiles2=nfiles2+1
		!	write4=.true.
			write3=.true.
			write4=.true.
			if(rw)rwtot=.true.
			nfiles3=nfiles3+1

		write(6,*)'opening ',inpf(if1:ipi-1)//'_2'//inpf(ipi:lef),' ************for REWRITING'
			open(unit=2,  err=244,file=inpf(if1:ipi-1)//'_2'//inpf(ipi:lef),&
			ACCESS='SEQUENTIAL',status='REPLACE',form='FORMATTED',action='WRITE', &
			iostat=ier)

			
			write(6,*)'opening ',inpf(if1:ipi-1)//'_3'//inpf(ipi:lef)
			open(unit=3,  err=110,file=inpf(if1:ipi-1)//'_3'//inpf(ipi:lef),&
			ACCESS='SEQUENTIAL',status='REPLACE',form='FORMATTED',action='WRITE', &
			iostat=ier)
	else
		write(6,*)'rw ',rw
		write(6,*)'R file, reading only'
	endif !if(time2.ge.time.and..not.rw)    353

	write(6,*)'opening ',inpf(if1:lef)
	open(1,file=inpf(if1:lef),err=110,status='old')
 
!	nfiles=nfiles+1

 
!write(6,*)'write4,write3',write4,write3
 
 
	call readfile()
 
 
	!	write(6,*)'<4554tas'
 
	! close(3,status='delete')
	! close(4)
	! write(6,*)'rename ',inpf(1:ipi-1)//'_3'//inpf(ipi:lef),'  ',inpf(1:ipi-1)//'_2'//inpf(ipi:lef)
	! call rename(inpf(1:ipi-1)//'_3'//inpf(ipi:lef),inpf(1:ipi-1)//'_2'//inpf(ipi:lef),istatus)
	! write(6,*)'status ',istatus, ' 0 =success'
	close(2)
	if(write3)close(3)
	!close(1)
	if(write3)then
		inquire(file=inpf(if1:ipi-1)//'_old'//inpf(ipi:lef),exist=exisf)
		if(exisf)then
 			open(unit=1, file=inpf(if1:ipi-1)//'_old'//inpf(ipi:lef), status="old")
 			close (unit=1, status="delete")
 
		endif !if(exisf)    407
		write(6,*)'rename ',inpf(if1:lef),'  ',inpf(if1:ipi-1)//'_old'//inpf(ipi:lef)
		call rename(inpf(if1:lef),inpf(if1:ipi-1)//'_old'//inpf(ipi:lef),istatus)
		write(6,*)'status ',istatus, ' 0 =success'
 
		if(istatus.ne.0)then
			CALL STAT(inpf(if1:lef),BUFF,STATUS)
			write(6,*)inpf(if1:lef),'status=',status,'buff',buff
			stop 'rename error 1'
		endif !if(istatus.ne.0)    419
 
		write(6,*)'rename ',inpf(if1:ipi-1)//'_3'//inpf(ipi:lef),' ',inpf(if1:lef)
		call rename(inpf(if1:ipi-1)//'_3'//inpf(ipi:lef),inpf(if1:lef),istatus)
		write(6,*)'status ',istatus, ' 0 =success'
		if(istatus.ne.0)then
 
			CALL STAT(inpf(if1:ipi-1)//'_3'//inpf(ipi:lef),BUFF,STATUS)
			write(6,*)inpf(if1:ipi-1)//'_3'//inpf(ipi:lef),'status=',status,'buff',buff
 
			CALL STAT(inpf(if1:lef),BUFF,STATUS)
			write(6,*)inpf(if1:lef),'status=',status,'buff',buff
 
			stop 'rename error 2'
		endif !if(istatus.ne.0)    428
		!inpf(1:ipi-1)//'_3'//inpf(ipi:lef),inpf(if1:lef)
	elseif(write4)then
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
	!buff(13)	Number of blocks allocated (-1 if not availabl
	
	CALL STAT(inpf(if1:ipi-1)//'_22'//inpf(ipi:lef),BUFF,STATUS)
	write(6,*)'initbuff ',BUFF,'status' ,status
	write(6,*)'::',inpf(if1:ipi-1)//'_22'//inpf(ipi:lef)
			open(unit=2,err=144,file=inpf(if1:ipi-1)//'_22'//inpf(ipi:lef),status='old',form='formatted')
			CALL STAT(inpf(if1:ipi-1)//'_22'//inpf(ipi:lef),BUFF,STATUS)
			write(6,*)'aftbuff ',BUFF,'status', status
			write(6,*)'opening ',inpf(if1:ipi-1)//'_2'//inpf(ipi:lef),' for comparing _2 and _22 files'
			open(4,file=inpf(if1:ipi-1)//'_2'//inpf(ipi:lef),err=244,status='old',form='formatted')
		inp=' '
		inp2=' '
			do j=1,1000000
				read(2,'(a)',end=179)inp
			
				read(4,'(a)',end=777)inp2
				
				let=len_trim_(inp)
				let2=len_trim_(inp2)
	!		write(6,*)inp(1:let),'*',inp2(1:let)
				if(let.ne.let2)goto 777
				if(inp(1:let).ne.inp2(1:let)) goto 777
			enddo
179  read(4,*,end=181 )inp2 !end =>files are identical, delete _22 file
		
		
		
777		close (unit=4, status="delete") !files are different delete _2 file
		nfiles22f=nfiles22f+1
			close(2)
			write(6,*)'_2 and _22 files are different at line ',j,' replace _2 file with _22 file'
			write(6,*)'rename ',inpf(if1:ipi-1)//'_22'//inpf(ipi:lef),' ',inpf(if1:ipi-1)//'_2'//inpf(ipi:lef)
			call rename(inpf(if1:ipi-1)//'_22'//inpf(ipi:lef),inpf(if1:ipi-1)//'_22'//inpf(ipi:lef),istatus)
			write(6,*)'status ',istatus, ' 0 =success'
		!	nwritten=nwritten+1
			goto 2
			
181 	close(2,status="delete")
		write(6,*)'files are indentical, delete _22 file'
		endif
	!			nwritten=nwritten+1
	
		


 

	!		CALL STAT(inpf(if1:lef),BUFF,STATUS)
	!		time=BUFF(10)
	!		call stat(inpf(if1:ipi-1)//'_2'//inpf(ipi:lef),BUFF,STATUS)
	!		write(6,*)'time,time_2',time,BUFF(10)
	goto 2
	144			write(6,*)'error opening ',inpf(if1:ipi-1)//'_22'//inpf(ipi:lef)
		stop '_22'
							
344			write(6,*)'error opening ',inpf(if1:ipi-1)//'_3'//inpf(ipi:lef)
		stop '_3'
244			write(6,*)'error opening ',inpf(if1:ipi-1)//'_2'//inpf(ipi:lef)
		stop '_2'
 
 
end program jpre!program jprenew
 
! if(ifin_.gt.1.and.genuse)then
! call jindent(infileunit,outfileunit)
! goto 90
 
! endif !if(ifin_.gt.1)then
!	contains
 
subroutine readfile()
	use sub
	use writeout   !write4
	use pref
	use main, only:inpf,if1,lef
	use jeqmod
	!		use jeqmod, only:indent,istep,jeq
	!	use jeqmod, only: letot,le,istart
	!		use pref
	integer,dimension(8)::datevalues
	integer ::le1
 
	logical ::endmodule,issub,endsub,ok,inprogram,inmodule,insub
	logical incontains,incontsub
	!		logical ::isad=.false.
	logical ::canbeleft,ininterface
	!		integer::indent=0
	nsubs=0
	inprogram=.false.
	inmodule=.false.
	insub=.false.
	indent=0
	istep=0
	cont=.false.
	contcur=.false.
	contif=.false.
	contfirst=.false.
	call insubs(linein,subsit,subslines,nsubs,insub)
 
	incontains=.false.
	incontsub=.false.
	!		incontfunc=.false.
	linein=0
	nsubline=0
	nused=0
	!	p=nfiles.eq.10
	goto 30
90		close(1)
		!write(6,*)'<6alsulopuslinein,inmodule,insub,incontsub ',linein,inmodule,insub,incontsub
		! if(nfiles.eq.1)then
			! do ii=1,modulewords
			! write(6,*)ii,moduleword(ii),lenmoduleword(ii)
			! enddo
		! endif
	return
	30	read(1,'(a)',end=90)inp
	linein=linein+1
	call letotle(inp,letot,le,icom,istart)
	!		if(linein.eq.6572.or.linein.eq.6573)write(6,*)'linein,cont,indent,istep,contcur,contfirst'
	!		if(linein.eq.6572.or.linein.eq.6573)write(6,*)'<1>',linein,cont,indent,istep,contcur,contfirst,contif
	! if(linein.eq.1808)write(3,*)'hier ',letot,le,icom,istart,icom,inp
	! if(linein.eq.1808)write(6,*)'hier ',letot,le,icom,istart,icom,inp
	istart2=istart
 
	if(le.gt.0)&
		!	call jindent(inp,linein,indent,istep,isad,outfileunit,canbeleft,ininterface)
		call jindent(inp,linein,3,canbeleft,ininterface)
	!		if(linein.eq.6572.or.linein.eq.6573)write(6,*)'<af1>',linein,cont,indent,istep,contcur,contfirst,contif
	!if(linein.lt.100)write(6,*)'linein ',linein,le,letot,indent,istep
	!if(ilinn.eq.3509)write(6,*)'indent ',indent,istep,isad
	! if(indent.le.0.and..not.canbeleft.and.le.gt.0)then
	! write(6,*)'line ',ilinn,' crash with left wall'
	! write(outfileunit,*)'line ',ilinn,' crash with left wall'
	! stop 'rightcrash'
	!endif
	! if(indent.gt..0.and.canbeleft.and..not.ininterface)then
	! write(6,*)'line ',ilinn,' not ending properly'
	! write(outfileunit,*)'line ',ilinn,' not ending properly'
	! stop 'leftcrash'
	! endif
	!call wr(inp,outfileunit,indent,isad)
	! if(linein.eq.1808)write(3,*)'<678 le,letot bef wr',letot,le,' linein ',linein
	if(write3)call wr(inp,3)
	!if(linein.eq.6572.or.linein.eq.6573)write(6,*)'<afw>',linein,cont,indent,istep,contcur,contfirst,contif
	if(contfirst)then
		indent=indent+1
		contfirst=.false.
		cont=.true.
		! if(linein.eq.1808)write(3,*)'indent ',indent,' after contfirst, le ',le
	elseif(cont.and..not.contcur)then
 
		indent=indent-1
 
		istep=0
		cont=.false.
	endif !if(contfirst)    532
	!if(istep.ne.0)write(3,*)'istep,newi ',istep,indent+istep
	!if(linein.eq.6572.or.linein.eq.6573)write(6,*)'<abefst>',linein,cont,indent,istep,contcur,contfirst,contif
	indent=indent+istep
	istep=0
	!if(linein.eq.6572.or.linein.eq.6573)write(6,*)'<afin>',linein,cont,indent,istep,contcur,contfirst,contif
	if(indent.gt.19.or.indent.lt.0)then
		write(6,*)'**line ',linein,' illegal indent ',indent,' after istep ',istep
		call pstop('indent after istep')
	endif !if(indent.gt.19.or.indent.lt.0)    549
	! if(linein.eq.1808)write(6,*)'hieeyyeyr ',letot,le,icom,istart,icom,inp
	istep=0
	!	if(linein.lt.50)write(6,*)linein,letot,inp(1:letot)
	!		p=linein.eq.2464.or.linein.eq.2503
	! letot=len_trim_(inp)
	! icom=0
	! le=0
	! if(letot.gt.0)icom=index(inp(1:letot),'!')
	! if(icom.gt.1)then
	! le=len_trim_(inp(1:icom-1))
 
	! elseif(icom.eq.1)then
	! le=0
	! else
	! le=letot
	! !write(16,*)'<3 ',inp(1:letot)
	! endif
	if(inpf(if1:lef).eq.'j.f90')then
		if(inp(3:10).eq.'j_title=')then
			call date_and_time(values=datevalues)
			idst=index(inp(1:30),'22.')
			write(inp(idst:idst+1),'(i2)')datevalues(3)
			write(inp(idst+4:idst+5),'(i2)')datevalues(2)
			write(inp(idst+8:idst+11),'(i4)')datevalues(1)
		endif !if(inp(3:10).eq.'j_title=')    571
	endif !if(inpf(if1:lef).eq.'j.f90')    570
	le1=max(le,1)
 
	if(le.le.0)then
		if(.not.insub.and..not.inmodule)then
			if(write4)write(2,'(a)')inp(1:letot)
 
			goto 30
		endif !if(.not.insub.and..not.inmodule)    582
	endif !if(le.le.0)    581
	ihip=0
	if(le.gt.0)ihip=index(inp(1:le),"'")
	if(le.gt.0.and.ihip.eq.0)ihip=index(inp(1:le),'"')
	hipsu=ihip.ne.0
	!write(16+nfiles,*)'<23 le,le1',le,le1
	!		call letotle(inp,letot,le,icom)
 
	if(inp(1:le).jeq.'subroutine')then
		write(6,*)'line ',linein, 'wild subroutine'
		call pstop('wild subroutine')
	end if !if(inp(1:le).jeq.'subroutine')    595
	le1=max(le,1)
	!if(p)write(16+nfiles,*)'<44tasle',linein,le,le1inmodule,insub,inp(1:letot)
 
 
	if(endmodule())then
		if(.not.inmodule)then
			write(6,*)linein,' ',inp(1:letot),'  BUT WE ARE NOT IN MODULE'
			call pstop('end module but not in module')
		endif !if(.not.inmodule)    604
		inmodule=.false.
		if(p)write(6,*)'end module ',modulename(nmodules), 'used ',nused, ' got ',modulewords-modulewords0,&
			' nsubline ',nsubline
		do ius=1,nused
			in=used(ius)
			im=modulemod(in)
			if(write4)write(2,'(a)')&
				char(9)//'use '//modulename(im)(1:lenmodulename(im))// &
				', only: '//moduleword(in)(1:lenmoduleword(in))
		enddo !ius=1,nused    611
		nused=0
		do j=1,nsubline
			if(write4)write(2,'(a)')subline(j)(1:lensubline(j))
		enddo !j=1,nsubline    619
		nsubline=0
		if(write4)write(2,'(a)')inp(1:letot)
 
 
		!if(p.and.nmodules.gt.0)write(6,*)linein,' end module', modulename(nmodules)
		if(p.and.nmodules.gt.0)then
 
			do j=modulewords0+1,modulewords
				write(6,*)j,moduleword(j),lenmoduleword(j)
 
			enddo !j=modulewords0+1,modulewords    629
		endif !if(p.and.nmodules.gt.0)    627
 
		goto 30
 
	endif !if(endmodule())    603
 
	call getmodule(lemo)
	!	write(16+nfiles,*)'ismodule',lemo
	if(lemo.gt.0)then
		if(inmodule)then
			write(6,*)'line ',linein, 'new module without closing previous'
			call	pstop('new module without closing previous')
		endif !if(inmodule)    642
		if(write4)write(2,'(a)')inp(1:letot)
		!		write(6,*)'*module ', inp(1:letot)
		inmodule=.true.
		nopremod=.false.
		modulewords0=modulewords
		if(icom.gt.0)nopremod=inp(icom:icom+ 6).eq.'!nopre!'
		!	if(nopremod)write(6,*)'starting npremod ',inp(1:letot)
 
		if(.not.nopremod)then
			!		ifi=modulewords+1
 
 
			nmodules=nmodules+1
			modulename(nmodules)=modulename0(1:lemo)
			!	write(6,*)'<33module',nmodules,modulename(nmodules)
			lenmodulename(nmodules)=lemo
			!call isold() !tarvitaanko
			!	nsubline=0
 
			if(p)write(6,*)' start module ',nmodules,'  ',modulename0(1:lemo), ' words ',modulewords
 
		endif !if(.not.nopremod)    654
		goto 30
	endif !if(lemo.gt.0)    641
	! if(linein.eq.1808)write(6,*)'hie666yr ',letot,le,icom,istart,icom,inp
	! if(linein.eq.1808)write(3,*)'hie666yr ',letot,le,icom,istart,icom,inp
	if(inmodule)then
 
		!	if(write4)write(2,'(a)')inp(1:letot)
 
		if(nopremod)then
			if(write4)write(2,'(a)')inp(1:letot)
			goto 30
		endif !if(nopremod)    676
		!	nold=modulewords
		call modulew()
		call putsubline()
		!if(modulewords.gt.nold)write(6,*)'moduleword ',modulewords,moduleword(modulewords)
		goto 30
 
	endif !if(inmodule)    672
	ico=index(inp(1:le),'contains')
 
	!if(ico.gt.0)write(6,*)linein,'***contains',hipsu
 
	if(ico.gt.0.and..not.hipsu)then
		ib=nonblank(inp,1,ico)
		ib2=nonblank(inp,ico+8,le)
		!write(6,*)linein,ico,le,ib,ib2,inp(1:letot)
 
		if(ib.eq.ico.and.ib2.eq.le+1)then
			if(p)write(6,*)linein, ' contains'
			if(.not.insub)then
				write(6,*)'line ',linein,' contains but not in subroutine or function'
				write(6,*)inp(1:letot)
				write(6,*)'lines where insub changes'
				!inline,subsit,subslines,nsubs,insub
				do isu=max(nsubs-10,1),nsubs
					write(6,*)isu,subslines(isu),subsit(isu)
 
				enddo !isu=max(nsubs-10,1),nsubs    703
				call pstop('contains but not in subroutine ')
			endif !if(.not.insub)    698
			incontains=.true.
			if(p)write(6,*)'<555********',incontains,linein,inp(1:le)
			incontline=linein
			call putsubline()
			goto 30
		endif !if(ib.eq.ico.and.ib2.eq.le+1)    696
	endif !if(ico.gt.0.and..not.hipsu)    691
 
	if(issub())then
 
		if(p)write(6,*)'<34>',linein,le,insub,incontains,incontsub,inp(1:letot)
		if(insub.and. .not. (incontains.and..not.incontsub))then
			write(6,*)'line ',linein,' illegal ',inp(1:letot)
			write(6,*)'we are already in ',subname
			!	write(6,*)'<477474 laitettiin ',incontline
 
			call pstop('illegal:'//inp(1:letot))
		elseif(incontains)then
			incontsub=.true.
			subnamecont=subname0
			linecontsub=linein
			call putsubline()
			goto 30
		else
			if(write4)write(2,'(a)')inp(1:letot)
			if(write4.and.inp(le:le).eq.'&')then
				read(1,'(a)',end=90)inp
				linein=linein+1
				call letotle(inp,letot,le,icom,istart)
 
				istart2=istart
 
				if(le.gt.0)&
					!	call jindent(inp,linein,indent,istep,isad,outfileunit,canbeleft,ininterface)
					call jindent(inp,linein,3,canbeleft,ininterface)
 
			if(write3)call wr(inp,3)
				write(2,'(a)')inp(1:len_trim_(inp))
 
			endif !if(write4.and.inp(le:le).eq.'&')    734
			!if(p)write(6,*)'<29> insub',insub
			insub=.true.
			call insubs(linein,subsit,subslines,nsubs,insub)
			if(isfunc)infunc=.true.
			subname=subname0
			lensubname=len_trim(subname)
			linesub=linein
			nused=0
			goto 30
		endif !if(insub.and. .not. (incontains.and..not.incontsub))    720
 
 
	endif !if(issub())    717
	if(endsub())then
		if(p)write(6,*)'<19>endsub',linein,incontsub,insub,inp(1:letot)
		if(incontsub)then
			incontsub=.false.
			call putsubline()
			if(p)write(6,*)'<19>',linein,' endcontsub ',inp(1:letot)
			linecontend=linein
			goto 30
		elseif(.not.insub.and..not.isfunc)then
			write(6,*)'line ',linein,' *not in subroutine/function even if:',  inp(1:letot)
			write(6,*)' last subroutine started in ',linesub, 'lastcontsub in ', linecontsub
			write(6,*)' last contains started in ',incontline,' ended  in ',linecontend
			call pstop('illegal')
		endif !if(incontsub)    764
 
		if(p)write(6,*)'nused ',nused,' nsubline ',nsubline
		!write(6,*)'writing use',nused,'*'
 
		do ius=1,nused
			in=used(ius)
			im=modulemod(in)
			if(write4)write(2,'(a)')&
				char(9)//'use '//modulename(im)(1:lenmodulename(im))// &
				', only: '//moduleword(in)(1:lenmoduleword(in))
			if(p)write(6,'(a)')&
				char(9)//'use '//modulename(im)(1:lenmodulename(im))// &
				', only: '//moduleword(in)(1:lenmoduleword(in))
		enddo !ius=1,nused    780
		nused=0
		!			call getuse()
		!	write(6,*)'<52>',linein,' writing ',nsubline, 'lines'
		!if(p)if(write4)write(2,*)'<52>',linein,' writing ',nsubline, 'lines'
		do j=1,nsubline
			if(write4)write(2,'(a)')subline(j)(1:lensubline(j))
		enddo !j=1,nsubline    794
		if(write4)write(2,'(a)')inp(1:letot)
		insub=.false.
		call insubs(linein,subsit,subslines,nsubs,insub)
		infunc=.false.
		nsubline=0
		incontains=.false.
		linesubend=linein
		goto 30
 
 
 
	endif !if(endsub())    762
	if(insub)then
		call putsubline()
		call subwords()
		goto 30
	end if !if(insub)    809
	if(write4)write(2,'(a)')inp(1:letot)
	goto 30
	!	write(6,*)'<644linein,inmodule,insub,incontsub ',linein,inmodule,insub,incontsub
 
 
end subroutine
 
logical function endsub()
 
	use  sub, only: inp,hipsu,le
	!	use  sub, only: inp,le,hipsu
	endsub=.false.
	if(hipsu)return
	if(le.lt.3)return
 
	iend=max(index(inp(1:le),'end'),index(inp(1:le),'END'))
	if(nonblank(inp,1,le).lt.iend)return
	!	if(index(inp(1:le),'end').gt.0)write(6,*)'endsu ',iend,le,inp(1:le)
	if(iend.le.0)return
	if(iend+2.eq.le)goto 9
	ifu=max(index(inp(1:le),'function'),index(inp(1:le),'FUNCTION'))
	isu=max(index(inp(1:le),'subroutine'),index(inp(1:le),'SUBROUTINE'))
	ipu=max(index(inp(1:le),'program'),index(inp(1:le),'PROGRAM'))
	if(max(ifu,isu,ipu).eq.0)return
	if(max(ifu,isu,ipu).lt.iend)return
	!	integer::iop,isetopt,i,iopt,istart,lopo,ir,iv,ic1,ii5,ioptend
	9	endsub=.true.
end function
 
logical function endmodule()
	use  sub, only: inp,hipsu,le !inp,le,hipsu
	use jeqmod
	endmodule=.false.
	if(hipsu)return
	if(le.lt.9)return
	iend=max(index(inp(1:le),'end'),index(inp(1:le),'END'))
	if(nonblank(inp,1,le).lt.iend)return
	imod=max(index(inp(1:le),'module'),index(inp(1:le),'MODULE'))
	if(imod.le.0)return
	if(iend.gt.0.and.(imod.eq.le-5.or.(inp(imod+6:imod+6).jeq.' ')))endmodule=.true.
 
end function
 
logical function issub()
	use sub,only:inp,subname0,le,hipsu,isfunc,linein,le
	integer ::istart_  !local
	!		use jeqmod
	issub=.false.
 
	if(hipsu)return
	if(le.lt.7)return
	iend=max(index(inp(1:le),'end'),index(inp(1:le),'END'))
 
	ial=0
	ifu=max(index(inp(1:le),'function'),index(inp(1:le),'FUNCTION'))
	if(ifu.gt.0)then
 
		ial=ifu+8
		isfunc=.true.
 
		istart_=ifu
	endif !if(ifu.gt.0)    869
	isu=max(index(inp(1:le),'subroutine'),index(inp(1:le),'SUBROUTINE'))
	if(isu.gt.0)then
		ial=isu+10
		istart_=isu
	end if !if(isu.gt.0)    877
	ipu=max(index(inp(1:le),'program'),index(inp(1:le),'PROGRAM'))
	if(ipu.gt.0)then
 
		ial=ipu+7
		istart_=ipu
		if(iend.gt.0.and.iend.lt.ipu)return
		if(ipu.gt.1)then
			if(inp(ipu-1:ipu-1).ne.' '.and.inp(ipu-1:ipu-1).ne.char(9))return
			if(inp(ipu+7:ipu+7).ne.' '.and.inp(ipu+7:ipu+7).ne.char(9))return
		endif !if(ipu.gt.1)    887
		issub=.true.
		subname0=inp(ial:le)
		return
	endif !if(ipu.gt.0)    882
	if(ial.le.0)return
	if(iend.gt.0.and.iend.lt.ial)return
	if(inp(ial:ial).gt.' ')return
	if(istart_.gt.1)then
		if(inp(istart_-1:istart_-1).gt.' ')return
	endif !if(istart_.gt.1)    898
 
	isul=index(inp(ial:le),'(') !between subroutine and ( there are only blanks
	if(isul.eq.0)return
	isul=ial+isul-1
 
	if(inp(ial+1:isul).eq.' ')return
	if(index(inp(1:ial),"'").gt.0)return
	! do i=ial,isul-1
	! if(inp(i:i).ne.' ')exit
	! enddo
	do while(inp(ial:ial).eq.' ')
		ial=ial+1
	enddo !while(inp(ial:ial).eq.' ')    911
	subname0=inp(ial:isul-1)
 
	!	if(isfunc)write(6,*)'<34773',linein,subname0(1:len_trim(subname0)),'**',inp(1:le)
	!func=inp(i:j)
	!issub=j-i+1
	issub=.true.
 
 
end function
 
subroutine getmodule(imo)
	use sub
	imo=0
	if(hipsu)return
	if(le.le.6)return
	iend=max(index(inp(1:le),'end'),index(inp(1:le),'END'))
	if(iend.gt.0)return
	ial=0
	imo_=max(index(inp(1:le),'module'),index(inp(1:le),'MODULE'))
	if(imo_.le.0)return
	if(inp(imo_+6:imo_+6).ne.' '.and.inp(imo_+6:imo_+6).ne.char(9))return
 
	ib=nonblank(inp,1,imo_)
	if(ib.ne.imo_)return
 
 
	do j=le,imo_,-1
		if(inp(j:j).eq.' ')exit
	enddo !j=le,imo_,-1    940
	modulename0=inp(j+1:le)
	imo=le-j
	return
end subroutine
 
 
 
 
 
 
subroutine modulew()
	use sub
	use pref
	logical pm
	logical inhipsu
	pm=.false.
	!	if(modulewords.ge.13.and.modulewords.lt.20)pm=.true.
	if(pm)write(6,*)le,modulewords,inp(1:le),modulewords
	!		if(modulewords.gt.20)stop
	if(le.le.2)return
	!	write(6,*)'modulewords ',modulewords
 
ploop:		do ip=1,nprefix

		ipf=index(inp(1:le),prefix(ip)(1:lprefix(ip)))
		if(pm)write(6,*)'ip,ipf',ip,ipf
		!	istart=ipf
		ial=ipf+lprefix(ip)-1  !its
		if(pm)write(6,*)'ial',ial
		!	if(ipf.gt.0)write(6,*)'ial ',ial
		!	if(ipf.gt.0)write(6,*)'<3mw',inp(1:le)
		do while(ipf.gt.0)
			if(pm)write(6,*)'ipfuus',ipf, ' ial ',ial
			do k=ial,le
				if(.not.(inp(k:k).ge.'A'.and.inp(k:k).le.'Z'.or. &
					inp(k:k).ge.'a'.and.inp(k:k).le.'z'.or. &
					inp(k:k).eq.'_'.or. &
					inp(k:k).ge.'0'.and.inp(k:k).le.'9'))goto 70
			enddo !k=ial,le    976
			!cycle ploop
	70		continue
			if(pm)write(6,*)'k,chk',k,inp(k:k)
			if(ipf.gt.1)then
				ipf1=ipf-1
				if(inp(ipf1:ipf1).ge.'A'.and.inp(ipf1:ipf1).le.'Z'.or. &
					inp(ipf1:ipf1).ge.'a'.and.inp(ipf1:ipf1).le.'z'.or. &
					inp(ipf1:ipf1).eq.'_'.or. &
					inp(ipf1:ipf1).ge.'0'.and.inp(ipf1:ipf1).le.'9')goto 66
 
			endif !if(ipf.gt.1)    985
			if(inhipsu(ipf,k-1))goto 66
			if(inp(ipf:k-1).eq.prefix(ip)(1:lprefix(ip)))then
				write(6,*)'line ',linein, ' do not use prefix as a variable'
				write(6,*)inp(1:le)
				call pstop('prefix as a variable')
			endif !if(inp(ipf:k-1).eq.prefix(ip)(1:lprefix(ip)))    994
 
 
	loop2:		do j=1,modulewords0
	!if(k-1-ipf.gt.25.or.lenmoduleword(j).gt.29)write(6,*)modulewords0,ipf,j,le,lienein,inp(1:le)
				if(lenmoduleword(j).ne.k-ipf)cycle
				if(moduleword(j)(1:lenmoduleword(j)).eq.inp(ipf:k-1))then
					do ii=1,nused
						if(used(ii).eq.j)then
							k=k+1
							goto 66
 
						endif !if(used(ii).eq.j)   1006
					enddo !ii=1,nused   1005
					nused=nused+1
					used(nused)=j
					! write(6,*)linein,nused,j,istart,k,le,ial,inp(1:le)
 
					!if(nused.le.20)write(6,*)'nused,j',nused,j,moduleword(j),inp(1:le)
					k=k+1
					goto 66
 
				endif !if(moduleword(j)(1:lenmoduleword(j)).eq.inp(ipf:k-1))   1004
 
			enddo loop2 !p2:		do j=1,modulewords0   1001
 
			modulewords=modulewords+1
			!write(6,*)'<674674',modulewords,inp(istart:k-1)
			!		if(modulewords.gt.100)stop
			if(pm)write(6,*)'ipf,k-1',ipf,k-1,k-ipf
			moduleword(modulewords)=inp(ipf:k-1)
			lenmoduleword(modulewords)=k-ipf
			modulemod(modulewords)=nmodules
			!	if(p.and.modulewords.lt.50)write(6,*)'<55',modulewords,nmodules
66				ial=k+1
			if(pm)write(6,*)'ial653',ial,ial.ge.le-lprefix(ip)
			if(ial.ge.le-lprefix(ip))cycle ploop
			ipf=index(inp(ial:le),prefix(ip)(1:lprefix(ip)))
			if(pm)write(6,*)'ipf',ipf
			if(ipf.gt.0)ipf=ial+ipf-1
			ial=ipf+lprefix(ip)
			if(pm)write(6,*)'ipf2',ipf
			!				istart=ial+ipf-1
			!			 write(6,*)'ipf,istart,ial',ipf,istart,ial,le
		enddo !while(ipf.gt.0)    974
	enddo ploop !op:		do ip=1,nprefix    965
end subroutine
 
 
 
 
 
integer function len_trim_(inp)
	character*(*) inp
	integer le_
	le_=len(inp)
 
	if(le_.le.0)then
		len_trim_=0
		return
	endif !if(le_.le.0)   1055
	do len_trim_=le_,1,-1
		if(ichar(inp(len_trim_:len_trim_)).gt.32)return
	enddo !len_trim_=le_,1,-1   1059
	len_trim_=0
	return
end function len_trim_ !integer function len_trim_(inp)
 
subroutine putsubline()
	use sub,only:inp,letot,subline,nsubline,lensubline
 
	nsubline=nsubline+1
	subline(nsubline)=inp(1:max(letot,1))
	lensubline(nsubline)=max(letot,1)
	!		write(3,*)'nsub ',nsubline,'sub ' inp(1:max(letot,1))
	!		write(16,*)inp(1:max(letot,1))
end subroutine !subroutine putsubline()
 
subroutine subwords()
	use sub
	use pref
 
	integer ipf,ial,ip
	logical pm
	logical inhipsu
	pm=.false.
	!		write(3,*)'subw le ',le,letot
	!	if(pm)write(6,*)'<3mw',linein,inp(1:le)
	if(le.le.2)return
	!	write(6,*)'modulewords ',modulewords
	!	write(19,*)nfiles,linein,nused
ploop:		do ip=1,nprefix

		ipf=index(inp(1:le),prefix(ip)(1:lprefix(ip)))
		!		istart=ipf
		ial=ipf+lprefix(ip)-1  !+1  !
 
		do while(ipf.gt.0)
			do k=ial,le
				if(.not.(inp(k:k).ge.'A'.and.inp(k:k).le.'Z'.or. &
					inp(k:k).ge.'a'.and.inp(k:k).le.'z'.or. &
					inp(k:k).eq.'_'.or. &
					inp(k:k).ge.'0'.and.inp(k:k).le.'9'))goto 70
			enddo !k=ial,le   1096
 
			!	cycle ploop
			!modulewords=modulewords+1
			!write(6,*)'<674674',modulewords,inp(istart:k-1)
			!		if(modulewords.gt.100)stop
			!moduleword(modulewords)=inp(istart:k-1)
 
	70	continue
				! if(inp(ipf:k-1).eq.'j_)')then


				! write(6,*)ipf,ial,inp(ial:ial),k
				! stop
				! endif
			if(inhipsu(ipf,k-1))goto 19
			if(ipf.gt.1)then
				ipf1=ipf-1
				if(inp(ipf1:ipf1).ge.'A'.and.inp(ipf1:ipf1).le.'Z'.or. &
					inp(ipf1:ipf1).ge.'a'.and.inp(ipf1:ipf1).le.'z'.or. &
					inp(ipf1:ipf1).eq.'_'.or. &
					inp(ipf1:ipf1).ge.'0'.and.inp(ipf1:ipf1).le.'9')goto 19
 
			endif !if(ipf.gt.1)   1117
 
			if(inp(ipf:k-1).eq.prefix(ip)(1:lprefix(ip)))then
				write(6,*)'line ',linein, ' do not use prefix as a variable'
				write(6,*)inp(1:le)
				call pstop('prefix as a variable')
			endif !if(inp(ipf:k-1).eq.prefix(ip)(1:lprefix(ip)))   1126
			
				! if(linein.eq.18420)then
			! write(6,*)'k,ipf',k,ipf,'infunc',infunc,lensubname
			! write(6,*)'k',inp(1:k)
			! write(6,*)'ipf',inp(1:ipf)
			! endif
 
	loop2:		do j=1,modulewords
					!	if(pm)write(6,*)'<32>',ipf,k,inp(1:le),'*',inp(ipf:k-1)
			!		if(j.eq.14)write(6,*)'her',-ipf,lenmoduleword(j),'/',moduleword(j)(1:lenmoduleword(j)),'/',&
			!		inp(ipf:k-1)
				if(k-ipf.ne.lenmoduleword(j))cycle
				if(moduleword(j)(1:lenmoduleword(j)).eq.inp(ipf:k-1))then
			!				if(j.eq.14.and.linein.eq.18411-1)write(6,*)'<53 ',inp(ipf:k-1),nused,used(1:nused),' j ',j
					do ii=1,nused
						!	if(pm)write(6,*)'was used ',ii,inp(ipf:k-1)
						if(used(ii).eq.j)goto 19
					enddo !ii=1,nused   1137
			!		if(j.eq.14.and.linein.eq.18411-1)write(6,*)'inf',infunc,lensubname,k-ipf
					if(infunc.and.lensubname.eq.k-ipf)then
						! if(linein.gt.570.and.linein.lt.900)write(6,*)'<6623',inp(ipf:k-1),&
						! subname(1:len_trim(subname)),index(subname,inp(ipf:k-1))
						! if(index(subname,inp(ipf:k-1)).gt.0)write(6,*)'<554FU',subname
						!	if(pm)write(6,*)'/'//subname(1:len_trim(subname))//'/'
						if(inp(ipf:k-1).eq.subname(1:lensubname))goto 19
 
					endif !if(infunc.and.lensubname.eq.k-ipf)   1141
					!if(isfunc)	write(6,*)'3773',linein,isfunc,nused,inp(ipf:k-1),subname
					nused=nused+1
					used(nused)=j
 !if(j.eq.14.and.linein.eq.18420)write(6,*)'nused',nused,j
 
					!if(nused.le.20)write(6,*)'nused,j',nused,j,moduleword(j),inp(1:le)
					goto 19
 
				endif !if(moduleword(j)(1:lenmoduleword(j)).eq.inp(ipf:k-1))   1135
 
			enddo loop2 !p2:		do j=1,modulewords   1132
		
	
			
			write(6,*)'prefix word ',inp(ipf:k-1),' not in modules????????????',nused,' nused'
			write(6,*)'line ',linein,k-ipf.ne.lenmoduleword(j),k-ipf,lenmoduleword(j)
			write(6,*)inp(1:le)
			
			! write(6,*)'modulewords ',modulewords
			! do j=1,modulewords
			! write(6,*)j,moduleword(j)(1:lenmoduleword(j))
			! ! if(j.eq.952)write(6,'(a)')'/'//inp(ipf:k-1)//'/'//moduleword(j)(1:lenmoduleword(j))//'/'
			! ! if(j.eq.952)write(6,*)k-ipf.ne.lenmoduleword(j),k-ipf,lenmoduleword(j)
			! ! if(j.eq.952) write(6,*)used(1:nused)
			! ! if(j.eq.952) write(6,*)'infunc ',subname(1:lensubname)
			! enddo
			call pstop('not in modules')
			! if(k-ipf.eq.lenmoduleword(j))then
			! write(6,*)j,k-ipf.ne.lenmoduleword(j),moduleword(j)(1:lenmoduleword(j))
			! do ii=1,nused
			! if(used(ii).eq.j)write(6,*)'used ',ii,j,moduleword(j)(1:lenmoduleword(j)).eq.inp(ipf:k-1)
 
			! enddo
			! endif
			! enddo
			! stop
 
19				ial=k+1
!if(linein.eq.18420)p=.true.
!if(linein.eq.18420)write(6,*)'ial',ial,le,lprefix(ip),ial.ge.le-lprefix(ip)
			if(ial.gt.le-lprefix(ip))cycle ploop
			ipf=index(inp(ial:le),prefix(ip)(1:lprefix(ip)))
 
			if(ipf.gt.0)ipf=ipf+ial-1
			ial=ipf+lprefix(ip)
 
		enddo !while(ipf.gt.0)   1095
	enddo ploop !op:		do ip=1,nprefix   1089
 
end subroutine
 
 
 
 
function nonblank(inp,ial,lop)
	integer, intent(in):: ial,lop
	character*(*), intent(in):: inp
	!tabulators are counted as blank
	do i=ial,lop
		if(ichar(inp(i:i)).gt.32)then
			nonblank=i
			return
		endif !if(ichar(inp(i:i)).gt.32)   1201
	enddo !i=ial,lop   1200
	nonblank=lop+1
	return
end function nonblank !function nonblank(inp,ial,lop)
 
function nextlim(inp,ial,lop,limit)
	! Finds the next limiter.
	!*
	! inp   = string to be searched
	! ial   = first character of inp looked at
	! lop   = last character to consider
	! limit = string containing limiters
	! If no limiter character is found function returns lop+1
	! sections between [ and ] are ignored
	! use errmod
 
	character*(*), intent(in):: inp, limit
	integer, intent(in):: ial,lop
 
	le_=len(limit)
	do 1 i=ial,lop
 
		do 2 j=1,le_
			if(inp(i:i).eq.limit(j:j)) goto 3
2   continue !2 j=1,le_   1227
1 continue !1 i=ial,lop   1225
	i=lop+1
3 nextlim=i

	return
end function nextlim !function nextlim(inp,ial,lop,limit)
! logical function numlet(ch)
! character*(1) ch
! numlet=.true.
! if(ch.ge.'A'.and.ch.le.'Z')return
! if(ch.ge.'a'.and.ch.le.'z')return
! if(ch.ge.'0'.and.ch.le.'9')return
! numlet=.false.
! return
 
! end function numlet !logical function numlet(ch)
 
! integer function iword(word,inp,ialw,lopw,nwords,istart)
! !	use jeqmod
! character*(*),intent(in):: word
! character*(*),intent(in):: inp
! integer, dimension(*),intent(in):: ialw
! integer, dimension(*),intent(in):: lopw
! integer, intent(in)::nwords
! integer,intent(out) :: istart
! do iword=1,nwords
! if(inp(ialw(iword):lopw(iword)).jeq.word)then
! istart=lopw(iword)+1
! return
! endif !if(inp(ialw(iword):lopw(iword)).jeq.word)then
! enddo !do iword=1,nwords
! iword=0
! return
! end function iword !integer function iword(word,inp,ialw,lopw,nwords,istart)
 
logical function inhipsu(ial,lop)
	use sub, only: inp,le
	inhipsu=.false.
	ih=index(inp(1:le),"'")
	if(ih.eq.0.or.lop.lt.ih)return
10 ih2=nextlim(inp,ih+1,le,"'")
	if(ih2.gt.le)return
	if(ih.lt.ial.and.ih2.gt.lop)then
		inhipsu=.true.
		return
	endif !if(ih.lt.ial.and.ih2.gt.lop)   1272
	ih=nextlim(inp,ih2+1,le,"'")
	if(ih.le.le)goto 10
	return
end function
 
subroutine insubs(inline,subsit,subslines,nsubs,insub)
	logical,dimension(10000)::subsit
	integer,dimension(10000)::subslines
	logical ::insub
	nsubs=nsubs+1
	subsit(nsubs)=insub
	subslines(nsubs)=inline
end subroutine
 
!indent
 
subroutine jindent(inp,ilinn,outfileunit,canbeleft,ininterface)
	use jeqmod
	use sub,only: le,letot,ico,istart,istart2
	!character*(*)inp0
	character*(*) inp
	integer:: ilinn
	integer ::outfileunit
	!ger ::indent,istep
	!logical ::isad
	logical ::canbeleft,ininterface
	integer ::indentv=0
	character*6 ::linenum
	character*(60):: dolabel(12)=' '
	character*(60):: iflabel(12)=' '
	character*10::dolabel2(12)=' '
	integer,dimension(12)::doline,ifline
	integer ::ifline0=0
	character*(60)::label0=' '
	integer ::ndo=0
	integer ::nif=0
	character*20 endc
	!logical :: first=.true.
	logical::p=.false.
 
	save first,indentv,nif,ndo,dolabel,iflabel,label0,doline,ifline,ifline0
	! if(ilinn.gt.29.and.ilinn.lt.34)then
	! write(6,*)'<63463 ',ilinn,le,letot,ico,istep,indent,inp
	! p=.true.
	! else
	! p=.false.
	! endif
	istep=0
	isad=.false.
	!indent=0
	canbeleft=.false.
	! if(ilinn.eq.1)then
	! do i=1,40
	! tabs(i:i)=char(9)  !horizontal tab
	! enddo
	! endif
	!write(6,*)'here'
	!p=ilinn.eq.20032
	if(p)write(6,*)'<245inindent,cont,le,letot,',cont,le,letot,inp(1:letot)
	!write(6,*)ilinn,'>',inp
	isad=.false.
 
	!if(inp.lt.0)write(22,*)'!*****indent',indent
	!call letotle(inp,letot,le,icom,istart)
	if(p)write(6,*)'<',inp(istart:letot),'%',inp(istart:le),'/'
	if(p)write(6,*)ichar(inp(le:le)),inp(le:le).gt.' '
	if(p)write(6,*)'ilinn,letot,le,icom,nif,ndo',ilinn,letot,le,icom,nif,ndo,istart
	!write(outfileunit,'(a)')inp(1:max(1,letot))
	!if(index(inp(1:letot),'else').gt.0)write(outfileunit,*)'***el8se*',inp(istart:istart+3)
	if(le.le.0)return
 
 
	contcur=inp(le:le).eq.'&'
	!if(contcur)write(outfileunit,*)'contcur'
	if(.not.contcur.and.cont)then
		!write(outfileunit,*)'contindentpois'
		!	indent=indent-1
		!	cont=.false.
		!if(ilinn.eq.6573)write(6,*)'kdfkfkf',(inp(max(le-4,1):le).jne.' ').and.(inp(max(le-4,1):le).ne.')')
		if(le.gt.4)then
			if((inp(le-3:le).jne.'then').or.&
				((inp(le-4:le-4).jne.' ').and.(inp(le-4:le-4).ne.')')))contif=.false.
		endif !if(le.gt.4)   1355
	endif !if(.not.contcur.and.cont)   1350
	contfirst=contcur.and..not.cont
	!if(contfirst)istep=1
 
	!if(contfirst)write(outfileunit,*)'confirststep'
	!cont=contcur
 
	if(inp(istart:istart).ge.'0'.and.inp(istart:istart).le.'9'.and..not.cont)then
		ib=nextblank(inp,istart+1,le)
		istart2=nonblank(inp,ib+1,le)
		if(p)write(6,*)'ITART2',ib,istart2,istart
		!	write(outfileunit,*)'ilinn ',ilinn,' isad',' cont,ib,istart,istart2 ',cont,ib,istart,istart2
		isad=.true.
		!	write(3,*)'isad'
		do idi=1,ndo
			lee1=len_trim_(dolabel2(idi))
			!	write(3,*)lee1,dolabel2(idi)(1:lee1),'*',inp(istart:ib-1)
			if(dolabel2(idi)(1:lee1).eq.inp(istart:ib-1))then
				if(p)write(6,*)'here ndo',ndo
 
				ner=0
				do ifi=1,nif
					if(ifline(ifi).gt.doline(ndo))then
						write(6,*)'**if-then started at ',ifline(ifi),' is not closed at enddo at' ,ilinn
 
						write(outfileunit,*)'**if-then started at ',ifline(ifi),' is not closed at enddo at' ,ilinn
						ner=ner+1
					endif !if(ifline(ifi).gt.doline(ndo))   1381
				enddo !ifi=1,nif   1380
 
				indent=indent-1
				leli=jlentrim(dolabel(ndo))
				write(linenum,'(i6)')doline(ndo)
				inp(le+1:le+leli+7)=dolabel(ndo)(1:leli)//' '//linenum//'hep'
				letot=le+leli+7
				!			letot=le
 
				if(ner.gt.0)goto 99
 
				!	stop 'unclosed if-then at enddo'
 
 
				ndo=ndo-1
				return
			endif !if(dolabel2(idi)(1:lee1).eq.inp(istart:ib-1))   1376
 
		enddo !idi=1,ndo   1373
 
	endif !if(inp(istart:istart).ge.'0'.and.inp(istart:istart).le.'9'   1366
 
	if(p)write(6,*)'tassa le,inp(istart2:istart2+7) ',le,inp(istart2:istart2+7)
	if(inp(istart2:istart2+2).jeq.'end')then
		ine=nonblank(inp,istart2+3,le)
		if(ine.gt.le)then
			if(ndo.gt.0)then
				write(6,*)'***line ',ilinn,' end with open do starting at line ',doline(ndo)
				write(outfileunit,*) '***end with open do starting at line ',doline(ndo)
				goto 99
			endif !if(ndo.gt.0)   1413
			if(nif.gt.0)then
				write(6,*)'**line ',ilinn,' end with open ifthen started at line ',ifline(nif)
				write(outfileunit,*)' end with open ifthen started at line ',ifline(nif)
 
				goto 99
			endif !if(nif.gt.0)   1418
			write(6,*)'*line ',ilinn, ' for clarity tell what is ended (program/subroutine/function)'
			indent=indent-1
			canbeleft=.true.
			return
 
		endif !if(ine.gt.le)   1412
		ine2=nextblank(inp,ine,le)
		ine3=nextpp(inp,ine,le)
 
		if(ine.eq.istart2+3)then
 
			endc=inp(istart2+3:min(ine2,ine3)-1)
		else
			endc=inp(ine:min(ine2,ine3)-1)
		endif !if(ine.eq.istart2+3)   1433
		lendc=jlentrim(endc)
 
 
		if((endc(1:lendc).jeq.'module').or.&
			(endc(1:lendc).jeq.'function').or.&
			(endc(1:lendc).jeq.'subroutine').or.&
			(endc(1:lendc).jeq.'program').or.&
				(endc(1:lendc).jeq.'interface'))then
			canbeleft=.true.
			if((endc(1:lendc).jeq.'interface'))ininterface=.false.
			if(ndo.gt.0)then
				write(6,*)'**line ',ilinn,'end with open do started at line ',doline(ndo)
				write(outfileunit,*)'***end with open do started at line ',doline(ndo)
				goto 99
			endif !if(ndo.gt.0)   1449
			if(nif.gt.0)then
				write(6,*)'**line ',ilinn,' end with open if started at line ',ifline(nif)
				write(outfileunit,*)'*** end with open if started at line ',ifline(nif)
				goto 99
			endif !if(nif.gt.0)   1454
			indent=indent-1
			if(p)write(6,*)'<636346le',le
			return
 
			if(((endc(1:lendc).jeq.'function').or.&
				(endc(1:lendc).jeq.'subroutine').or.&
					(endc(1:lendc).jeq.'program')).and.indent.ne.indentv)then
				write(6,*)'line ',ilinn,' indent does not match start of subroutine/function/program'
				write(6,*)'indent ',indent, ' starting indent',indentv
				write(6,*)'/'//endc(1:lendc)//'/'
				call pstop('error')
 
			endif !if(((endc(1:lendc).jeq.'function').   1463
			return
		elseif(endc(1:lendc).jeq.'if')then
			indent=indent-1
			!	if(p)write(3,*)'<549 indent after endif ',indent,' le ',le
			if(p)write(6,*)'<549 indent after endif ',indent,' le ',le
			if(nif.le.0)then
 
				write(6,*)'line ',ilinn,' endif even if there are no open if then '
				goto 99
			endif !if(nif.le.0)   1477
			ner=0
			do ido=1,ndo
				if(doline(ido).gt.ifline(nif))then
					write(6,*)'**do started at ',doline(ido),' is not closed at endif at ' ,ilinn
 
					write(outfileunit,*)'**do started at ',doline(ido),' is not closed at endif at ' ,ilinn
					ner=ner+1
				endif !if(doline(ido).gt.ifline(nif))   1484
 
			enddo !ido=1,ndo   1483
 
 
 
			leli=jlentrim(iflabel(nif))
			write(linenum,'(i6)')ifline(nif)
			inp(le+1:le+leli+7)=iflabel(nif)(1:leli)//' '//linenum
			letot=le+leli+7
			!	letot=le
			if(ner.gt.0)goto 99
 
			nif=nif-1
			return
 
		elseif(endc(1:lendc).jeq.'do')then
			if(p)write(6,*)'here ndo',ndo
			if(ndo.le.0)then
 
				write(6,*)'line ',ilinn,' enddo even if there are no open do '
				goto 99
 
			endif !if(ndo.le.0)   1507
			ner=0
			do ifi=1,nif
				if(ifline(ifi).gt.doline(ndo))then
					write(6,*)'**if-then started at ',ifline(ifi),' is not closed at enddo at' ,ilinn
 
					write(outfileunit,*)'**if-then started at ',ifline(ifi),' is not closed at enddo at' ,ilinn
					ner=ner+1
				endif !if(ifline(ifi).gt.doline(ndo))   1515
 
			enddo !ifi=1,nif   1514
 
			indent=indent-1
			leli=jlentrim(dolabel(ndo))
			write(linenum,'(i6)')doline(ndo)
			inp(le+1:le+leli+7)=dolabel(ndo)(1:leli)//' '//linenum
			letot=le+leli+7
			!		letot=le
 
			if(ner.gt.0)goto 99
 
			!	stop 'unclosed if-then at enddo'
 
 
			ndo=ndo-1
			return
		endif !if((endc(1:lendc).jeq.'module').   1442
 
	endif !if(inp(istart2:istart2+2).jeq.'end')   1410
	!if(p)write(6,*)'TASSA'
	!if(index(inp(1:letot),'else').gt.0)write(outfileunit,*)'***el9se*',inp(istart:istart+3)
	!if(ilinn.eq.3509.or.p)write(6,*)'<577575',istart,le,'/'//inp(istart:le)//'/'
	if((inp(istart2:istart2+2).jeq.'if ').or.(inp(istart2:istart2+2).jeq.'if(').and.le.gt.4)then
		!if(ilinn.eq.3509.or.p)write(6,*)'<577575',istart,le,inp(1:le),'%',inp(le-3:le),'&'
		if((inp(le-3:le).jeq.'then').and.((inp(le-4:le-4).jeq.' ').or.&
				inp(le-4:le-4).eq.')'))then
			!	if(p)write(6,*)'TASSA'
			nif=nif+1
			iflabel(nif)=' !'//inp(istart:le-4)
			ifline(nif)=ilinn
			!write(6,*)'<4664 nif,ifline(nif) ',nif,ifline(nif)
			!	if(ilinn.gt.10)stop 'perk'
			istep=1
	!	if(ilinn.gt.5640.and.ilinn.le.5650)write(6,*)'<4995 tas ,nif ilinn',nif,ilinn,ilinn
			!	if(ilinn.eq.741)write(6,*)'heres'
			return
		elseif(contfirst)then
			contif=.true.
			!		indent=indent+1
			label0=' !'//inp(istart:le-4)
			ifline0=ilinn
		endif !if((inp(le-3:le).jeq.'then').and.((inp(le-4:le-4).jeq.' ')   1546
		if(p)write(6,*)'heresppp'
		return
	endif !if((inp(istart2:istart2+2).jeq.'if ').or.(inp(istart2:ista   1544
	!if(index(inp(1:letot),'else').gt.0)write(outfileunit,*)'***el4se*',inp(istart:istart+3),contif
	!if(ilinn.eq.6573)write(6,*)'<465,tas',contif,le,inp(max(le-3,1):le)
	if(contif.and.(inp(max(le-3,1):le).jeq.'then'))then
	
		nif=nif+1
		iflabel(nif)=label0(1:jlentrim(label0))
		ifline(nif)=ifline0
	!		if(ilinn.gt.5640.and.ilinn.le.5650)write(6,*)'<455 tas ,nif ilinn,ifline0',nif,ilinn,ifline0
		!	istep=-1
		indent=indent+1
		contif=.false.
		if(p)write(6,*)'heresuuu'
		return
	endif !if(contif.and.(inp(max(le-3,1):le).jeq.'then'))   1569
	!if(index(inp(1:letot),'else').gt.0)write(outfileunit,*)'***else*',inp(istart:istart+3)
	if(inp(istart2:istart2+3).jeq.'else')then
		indent=indent-1
		!		call wr(inp)
		istep=1
		!	if(ilinn.eq.741)write(6,*)'STEOPPP'
		!		nif=nif+1
		!		iflabel(nif)=' !'//inp(istart:le)
		if(p)write(6,*)'hereueruers'
		return
	endif !if(inp(istart2:istart2+3).jeq.'else')   1581
	!function nextlim(inp,ial,lop,limit)
	ikp=nextlim(inp(1:le),1,le,':,(')
	if(p)write(6,*)'STEhuiPP',isad,ikp,inp(ikp:ikp).eq.':'.and.inp(ikp+1:ikp+1).ne.':'
	if(inp(ikp:ikp).eq.':'.and.inp(ikp+1:ikp+1).ne.':')then
		isad=.true.
		istart2=nonblank(inp,ikp+1,le)
	endif !if(inp(ikp:ikp).eq.':'.and.inp(ikp+1:ikp+1).ne.':')   1594
 
	!write(outfileunit,*)ilinn,istart,'/',inp(istart:istart+2),'/'
	if(p)write(6,*)'<378testing do ',(inp(istart2:istart2+2).jeq.'do ')
	if((inp(istart2:istart2+2).jeq.'do ').and.(inp(max(le-5,1):le).jne.'end do').and.&
			(inp(max(le-4,1):le).jne.'enddo'))then
		!	if(ilinn.eq.741)write(6,*)'NYTT'
		istep=1
		!write(3,*)'istep after do'
		ndo=ndo+1
		dolabel(ndo)=' !'//inp(istart+3:le)
		doline(ndo)=ilinn
 
		iafdo=nonblank(inp,istart2+2,le)
		if(inp(iafdo:iafdo).ge.'0'.and.inp(iafdo:iafdo).le.'9')then
			nafdo=nextblank(inp,iafdo,le)
			dolabel2(ndo)=inp(iafdo:nafdo-1)
			!		write(3,*)'dolabel ',dolabel2(ndo)
		else
			dolabel2(ndo)=' '
 
		endif !if(inp(iafdo:iafdo).ge.'0'.and.inp(iafdo:iafdo).le.'9')   1611
 
		if(p)write(6,*)'here43664s'
		return
	endif !if((inp(istart2:istart2+2).jeq.'do ').and.(inp(max(le-5,1)   1601
	ifu=index(inp(1:le),'function ')
	ilo=nextlim(inp,1,le,"(,)'")
	!	character*8 function j_chr8_s(a)
	!write(3,*)inp(1:le),ifu,ilo,le
 
	if(ifu.gt.istart2.and.ilo.gt.ifu)istart2=ifu
	!if(index(inp(1:le),'subroutine').gt.0)write(6,*)'<66 ',ilinn,' sub',inp(1:le)
	!write(6,*)'<77',inp(istart:istart+11)
	!if(p)write(6,*)'her4436es'
	!if(index(inp,'module').gt.0.or.p)then
	!rite(6,*)'modu',ilinn,istart,inp(istart:istart+6),letot,ico,le,inp
	!stop 'mod'
	!endif
 
	!if(inp(istart:istart+8).jeq.'recursive')istart=nonblank(inp,istart+9,le)
 
 
 
	if((inp(istart2:istart2+8).jeq.'function ').or. &
		(inp(istart2:istart2+10).jeq.'subroutine ').or.&
		(inp(istart2:istart2+7).jeq.'program ').or.&
		(inp(istart2:istart2+9).jeq.'recursive ').or.&
		(inp(istart2:istart2+6).jeq.'module ').or.&
			(inp(istart2:istart2+9).jeq.'interface '))then
		!call wr(inp)
		!write(6,*)'subrouti/fun'
		if((inp(istart2:istart2+9).jeq.'interface '))ininterface=.true.
		indentv=indent
		istep=1
		!	if(ilinn.eq.741)write(6,*)'INTER',istart2,inp(istart2:istart2+8)
		!	write(outfileunit,*)'istep herer',istep
		!	if(contfirst)istep=2
		!	write(outfileunit,*)'istep nowr',istep
		!	write(outfileunit,*)'subr,istep,indent',istep,indent,contcur,contfirst,cont
		canbeleft=.true.
		return
 
 
	endif !if((inp(istart2:istart2+8).jeq.'function ').o   1641
 
 
 
	return
 
99	continue
	write(outfileunit,*)inp(1:letot)
	write(6,*)inp(1:letot)
	write(outfileunit,*)ndo," do's started at: ",doline(1:ndo)
	write(6,*)ndo," do's started at: ",doline(1:ndo)
	write(outfileunit,*)nif," if's started at: ",ifline(1:nif)
	write(6,*)nif," if's started at: ",ifline(1:nif)
	call pstop('confused')
 
end subroutine
 
subroutine wr(inp,outfileunit)
	use jeqmod, only: tabs,indent,isad  !,letot !,contcur
	use sub,only: istart,letot
	!use jeqmod, only: istart
	character*(160)inp
	!logical ::isad
	integer::outfileunit
	!write(22,'(a,i)')inp0,nt
	!write(6,*)'<45 istart,indent,letot,cont,isad',istart,indent,letot,cont,isad
	if(istart.gt.letot)then
		!write(outfileunit,'(a)')'<444>',inp(1:letot)
 
		! cont=.false.
		return
	endif !if(istart.gt.letot)   1687
	!icont=0
	!if(cont)icont=1
 
	!nt2=indent
	if(isad)then
		write(outfileunit,'(a)')inp(1:letot)
		return
 
	endif !if(isad)   1697
	!if(inp(istart:istart).ge.'0'.and.inp(ifi:ifi).le.'9'.and..not.cont)then
	!write(outfileunit,'(a)')inp(1:letot)
 
 
	!write(22,'(a,i)')tabs(1:nt2)//inp0(ifi:),nt2
	!write(6,*)'isart,letot,indent',istart,letot,indent
	if(letot.gt.0)then
		!write(outfileunit,*)'<tabs>',indent
		if(indent.gt.20)then
			write(6,*)'**indent ',indent
			call pstop('too large indent')
		endif !if(indent.gt.20)   1710
		write(outfileunit,'(a)')tabs(1:indent)//inp(istart:letot)
	else
		write(outfileunit,'(a)')' '
	endif !if(letot.gt.0)   1708
 
	!cont=contcur
	!if(.not.contcur)contif=.false.
end subroutine
 
! function nonblank(inp,ial,lop)
! integer, intent(in):: ial,lop
! character*(*), intent(in):: inp
 
! do i=ial,lop
! if(ichar(inp(i:i)).gt.32)then
! nonblank=i
! return
! endif
! enddo
! nonblank=lop+1
! return
! end function nonblank
 
function nextblank(inp,ial,lop)
	integer, intent(in):: ial,lop
	character*(*), intent(in):: inp
 
	do i=ial,lop
		if(ichar(inp(i:i)).le.32)then
			nextblank=i
			return
		endif !if(ichar(inp(i:i)).le.32)   1742
	enddo !i=ial,lop   1741
	nextblank=lop+1
	return
end function nextblank
 
function nextpp(inp,ial,lop)
	integer, intent(in):: ial,lop
	character*(*), intent(in):: inp
 
	do i=ial,lop
		if(inp(i:i).eq.';')then
			nextpp=i
			return
		endif !if(inp(i:i).eq.';')   1756
	enddo !i=ial,lop   1755
	nextpp=lop+1
	return
end function nextpp
 
subroutine pstop(message)
	character*(*) message
	!write(6,*)'**stop with message ',message
	close (unit=2, status="delete")
	write(6,*)' '
	write(6,*)'**ERROR***'
	stop message
end subroutine
