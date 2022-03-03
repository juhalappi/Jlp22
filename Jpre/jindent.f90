!+----------------------------------------! with gfortran J_precompiler.exe can be made using: gfortran j_precompiler.f90 -o j_precompiler


! j_precompiler can be used to do two things:
! 1. It can add prefixes to global variables or subroutines stored in modules
! Also the prefixes can be changed. The addition or changing of prefixes is done only once.
!2. The precompiler generates necessary 'use' statments for global variables or subroutines stored
! in modules.
! The prefixes and the files to be precompiled are in the command file whose default name is j_pre.txt.
! the program generates the output files indicated in the command file
! i the log is written to unit 4 to a file whose name will be fort.4 or a similar name
! depending on the environment.

! the program is poorly commented

module jeqmod !case insensitive character comparisons
logical f77
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
end module jeqmod

program jindent
!use koemod,only: koe2
use jeqmod 
integer :: letotsub2=0
integer, parameter ::iunit=1
integer, parameter ::infileunit=2
integer     ::outfileunit=3
integer     :: n6=4!4 !logfile   put log writing on: change !!write(n6  into write(n6
character*80 f77line
integer   ::moddelunit=7 ! unit used to delete modules
integer, parameter ::moddel=9  !unit used to delete mold module files
integer, parameter :: maxfiles=20
integer, parameter :: inunit=maxfiles+1
integer, parameter :: maxmodules=50
integer    :: nmodules=0
integer, parameter :: maxsublines=20000     !per subroutine
integer, parameter :: maxarguments=30
integer, parameter :: maxused=10000
integer, dimension(maxsublines) ::nsublevel
integer, parameter ::maxcontains=60
integer, dimension(0:maxcontains+1) :: firstcontains

integer :: nused
integer :: indent =0
integer, parameter :: maxwords=50 !per line
integer,dimension(maxwords) :: ialw,ialw2
integer,dimension(maxwords) :: lopw,lopw2


character*2,dimension(maxwords)::lim2w
integer :: nwords,nwords2
integer :: nerr=0
integer,parameter  :: lenword=20


integer :: narguments0=0
integer :: narguments=0
integer,parameter :: lenprefix=4
character(len=lenprefix), dimension(maxfiles):: newprefix
integer, dimension(maxfiles) :: len_newprefix
integer, dimension(maxfiles) :: len_oldprefix
character(len=lenprefix), dimension(maxfiles):: oldprefix
logical,dimension(maxfiles) :: isoldprefix
logical,dimension(maxfiles) :: isnewprefix
integer :: nfiles=0
character*(30), dimension(maxmodules) :: modulename
integer , dimension(maxmodules) ::len_modulename
!!integer :: modulewords
integer ,dimension(maxmodules) :: firstword
 integer,parameter ::maxmodulelines=50000
character*(30),dimension(maxmodulelines) ::moduleword 
integer,dimension(maxmodulelines) ::modulemod 
 integer ,dimension(maxmodulelines) :: len_moduleword
character*(120),dimension(maxmodulelines) ::moduleline =' ' !comment
integer ,dimension(maxmodulelines):: lenmoduleline=1
integer,dimension(maxmodulelines) :: wordfile

integer, parameter :: maxsubroutines=2000
character*(40),dimension(maxsubroutines) :: sub
integer, dimension(maxsubroutines)  :: len_sub
character*(40),dimension(maxsubroutines) :: sub2
integer, dimension(maxsubroutines)  :: len_sub2
integer, dimension(maxsubroutines) ::subfile
integer ::nsubr=0

character(len=50),dimension(maxfiles) :: inputfile
integer ,dimension(maxfiles) :: len_inputfile
integer,dimension(maxfiles) :: len_inputline
character*(30),dimension(maxarguments) ::argument
integer,dimension(maxarguments) ::len_argument
integer,dimension(maxused):: used
character(len=300)inp
logical exis
logical open_
!logical isoutput
character*(30) program_,subroutine_,function_,module_,interface_
integer iostatus
! logical reserved
logical :: nopreuse
logical,dimension(0:2) :: nopresub 
logical ::nopremod=.false.
logical :: p=.false. !.true.
logical ::p2=.false.  !if <> written
character ch
integer :: modulewords=0
logical :: issub
logical :: isprogram
logical :: isimplicit,isimplicit2
logical :: istype
logical ::endtype
logical :: endsub
logical :: endmod
 character*(280),dimension(maxsublines) ::subline
 integer ,dimension(maxsublines) ::itypesubline=0
 character*(380) ::subline2
 integer ::isubname
 logical ::yestoall=.true.
 logical isfunc,infunction
 character*(40) ::func
 integer ::len_func,ifuncname
 logical ::isf77
 logical:: cont
 character*20 cursub
 integer :: lecursub=1
 
 ! integer ::isubname
 isimplicit()=inp(ialw(1):lopw(1)).jeq.'implicit'
isf77()=(inp(lopw(1)-3:lopw(1)).jeq.'.for').or.(inp(lopw(1)-3:lopw(1)).jeq.'.f77').or.(inp(lopw(1)-1:lopw(1)).jeq.'.f')
 isubname()=max(iword('function',inp,ialw,lopw,nwords,istart)+1,&
 iword('subroutine',inp,ialw,lopw,nwords,istart)+1) !subroutine
 ifuncname()=iword('function',inp,ialw,lopw,nwords,istart)+1
 issub()=iword('subroutine',inp,ialw,lopw,nwords,istart).gt.0.or. &   !subroutine
      ( iword('function',inp,ialw,lopw,nwords,istart).gt.0.and.&
			(inp(ialw(1):lopw(1)).jne.'end'))
	isfunc()= iword('function',inp,ialw,lopw,nwords,istart).gt.0.and.&
			(inp(ialw(1):lopw(1)).jne.'end')		
	! endsub()=(nwords.eq.1.and.inp(ialw(1):lopw(1)).jeq.'end').and.&  !subroutine
	    ! (nwords.eq.1.or.(inp(ialw(2):lopw(2)).jeq.'subroutine').or.&
	! (inp(ialw(2):lopw(2)).jeq.'function')	.or.	(inp(ialw(2):lopw(2)).jeq.'program'))		
endsub()=(nwords.eq.1.and.(inp(ialw(1):lopw(1)).jeq.'end')).or.&  !subroutine
			((inp(ialw(1):lopw(1))//inp(ialw(2):lopw(2)).jeq.'endsubroutine').or.&
			(inp(ialw(1):lopw(1))//inp(ialw(2):lopw(2)).jeq.'endfunction').or.&
			(inp(ialw(1):lopw(1))//inp(ialw(2):lopw(2)).jeq.'endprogram'))
	
	isprogram()=inp(ialw(1):lopw(1)).jeq.'program'  !subroutine
	endmod()=(nwords.eq.1.and.(inp(ialw(1):lopw(1)).jeq.'end')).or.&  !
			(inp(ialw(1):lopw(1))//inp(ialw(2):lopw(2)).jeq.'endmodule')
	

	
	! endmod()=(inp(ialw(1):lopw(1)).jeq.'end').and.&   !subroutine
	      ! (nwords.eq.1.or.(inp(ialw(2):lopw(2)).jeq.'module'))
	istype()=nwords.ge.2.and.(inp(ialw(1):lopw(1)).jeq.'type').and.lim2w(1).ne.'( '
endtype()=	nwords.ge.2.and.(inp(ialw(1):lopw(1)).jeq.'end') &    !end type
			.and.( inp(ialw(2):lopw(2)).jeq.'type')


nun=0
goto 70
89	write(6,*)'error in opening'
90 stop
! read(5,*)in
 ! moduleword(in)='9'
! write(6,*)'log written to file fort.4 (or similar name depending on compiler)'

70 write(6,*)'opening indent.txt for reading'
	open(inunit,file='jindent.txt',err=89,status='old')


2 read(inunit,'(a)',end=90)inp
ipil=index(inp,',')
write(6,*)'opening ',inp(1:ipil-1),' for reading'
open(infileunit,file=inp(1:ipil-1),action='READ',err=89)
write(6,*)'opening ',inp(ipil+1:len_trim(inp)),' for writing'
open(outfileunit,file=inp(ipil+1:len_trim(inp)),action='WRITE',err=89)
	ntab=1  ! number of tabs
	ntabcur=1
!outfileunit=6
call j_indent(infileunit,outfileunit)
close(infileunit)
close(outfileunit)
goto 2




contains

subroutine putsubline()  !puts current inp to subline buffer
nsubline=nsubline+1
	  subline(nsubline)=inp(1:max(letot,1))
		itypesubline(nsubline)=0
end subroutine

subroutine putsubline1()  !puts current inp to subline buffer
nsubline=nsubline+1
	  subline(nsubline)=inp(1:max(letot,1))
		itypesubline(nsubline)=1
end subroutine

subroutine putuseaddmod()  !puts all words into module and use list if they are not there n
logical reserved
integer i_,iu
loop1: do i_=1,nwords
if(reserved(inp(ialw(i_):lopw(i_))))cycle
if(infunction.and.(inp(ialw(i_):lopw(i_)).jeq.func(1:len_func)))cycle
do iu=1,nused
m=used(iu)
if(inp(ialw(i_):lopw(i_)).jeq.moduleword(m)(1:len_moduleword(m)))cycle loop1
end do



loop2: do m=1,firstword(nmodules)-1
if(inp(ialw(i_):lopw(i_)).jeq.moduleword(m)(1:len_moduleword(m)))then

nused=nused+1
used(nused)=m
cycle loop1
 ! write(outfileunit,*)'<962>putuse,line, ',linein,inp(ialw(i_):lopw(i_)), ' inp:',inp(1:le)
endif

enddo loop2
do i_2=firstword(nmodules),modulewords
if(inp(ialw(i_):lopw(i_)).jeq.moduleword(i_2)(1:len_moduleword(i_2)))cycle loop1
end  do
			modulewords=modulewords+1
					
			moduleword(modulewords)=inp(ialw(i_):lopw(i_))
			modulemod(modulewords)=nmodules
				if(p)write(outfileunit,*)'<578> modulewords word',modulewords,moduleword(modulewords)

			len_moduleword(modulewords)=lopw(i_)-ialw(i_)+1
			wordfile(modulewords)=nfiles

			if(icom.gt.0)then
			
			do ic_=icom,letot-1
			if(inp(ic_:ic_+1).eq.'%%')inp(ic_:ic_+1)='!!'
			enddo
			moduleline(modulewords)=inp(icom:letot)
		   lenmoduleline(modulewords)=letot-icom+1
			endif
			!if(modulewords.le.10)write(outfileunit,*)'nfiles,wordfile',nfiles,wordfile(nfiles)
		!	write(19,*)moduleword(modulewords),moduleline(modulewords)
	enddo loop1



return

end subroutine
subroutine anainterface()  !anlyzes interface 
	!write(6,*)'<595> interface'
	if(nwords.ge.2)then
		!!write(n6,*)' putting  into module: ',inp(ialw(2):lopw(2))
		call putmod(2)
	!	write(outfileunit,*)(inp(ialw(j_):lopw(j_)),'/',j_=1,nwords)
		call addprefix2(letotsub2)
		if(p2)then
		write(outfileunit,'(a)')subline2(1:letotsub2)//' !<ai1> '
		else
		write(outfileunit,'(a)')subline2(1:letotsub2) !, ' ! ',indent
		! call writ(outfileunit,subline2(1:letotsub2))
		endif
	else
	if(p2)then
	write(outfileunit,'(a)')inp(1:letot) //' !<ai1+>'
	else
	write(outfileunit,'(a)')inp(1:letot) 
	! call writ(outfileunit,inp(1:letot))
	endif
	endif
	
	! call getuse(.true.)

!	if(isoutput)write(outfileunit,'(a)')inp(1:letot)

	131 read(infileunit,'(a)',end=90)inp
!	nsubline=nsubline+1
!	subline(nsubline)=inp(1:max(1,letot))
	
	
	linein=linein+1
	call letotle(inp,letot,le,icom)
	if(le.le.0)then
	write(outfileunit,'(a)')inp(1:max(1,letot))
	! call writ(outfileunit,inp(1:max(1,letot)))

	goto 131
	
	endif
	!if(p)write(outfileunit,*)'<605>:'
	!if(isoutput)write(outfileunit,*)'<ai>',inp(1:max(1,letot))
	!	 nsubline=nsubline+1
	!subline(nsubline)=inp(1:max(1,letot))
	!if(le.gt.0)	write(6,*)'<110:>, letot,le,icom,inp,',letot,le,icom,inp(1:letot)
	!write(6,*)'<485> letot,inp:',letot,inp(1:letot)
	if(le.le.0)goto 131
	call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
	! write(22,*)3
	! call getindent(indent,inp,ialw,lopw,nwords)
	if(inp(ialw(1):lopw(1)).jeq.'end')then   !end interface
		!!write(n6,*)' end interface',linein,inp(1:letot)
		 call addprefix2(letotsub2)
	   	if(p2)then
		write(outfileunit,'(a)')subline2(1:letotsub2)//' !<ai1> '
		else
			write(outfileunit,'(a)')subline2(1:letotsub2)
			! call writ(outfileunit,subline2(1:letotsub2))
		endif
	!	 nsubline=nsubline+1
		return
	endif
	if(issub())then
		!!write(n6,*)'<779sub> ',linein,inp(ialw(isubname()):lopw(isubname()))
		ipu=isubname()
		call putmod(ipu)
		!!write(n6,*)' putting subr into interface: ',inp(ialw(ipu):lopw(ipu))
					 call addprefix3(ipu,letotsub2)
			 if(p2)then
			write(outfileunit,'(a)')subline2(1:letotsub2)// '!<issubinterface>'
			else
					write(outfileunit,'(a)')subline2(1:letotsub2)     !, '!',indent
					! call writ(outfileunit,subline2(1:letotsub2))
			endif
         !!write(n6,*)'<775> uusgetuse,nused,nsublines',nused,nsublines
		 call getuse()  !uus
		call anasub(.false.)
	  if(.false.)then
		
			! if(p)write(6,*)'726> word file ',wordfile(modulewords),nfiles
		call addprefix3(ipu,letotsub2)
		if(p2)then
			write(outfileunit,'(a)')subline2(1:letotsub2)//' !<ai2> '
		else
			write(outfileunit,'(a)')subline2(1:letotsub2)
			! call writ(outfileunit,subline2(1:letotsub2))
		endif
			
	!		if(modulewords.le.10)write(outfileunit,*)'<719>nfiles,wordfile',nfiles,wordfile(nfiles)
!			! goto 134
133	  	read(infileunit,'(a)',end=90)inp
!	write(6,*)'<483> sivuutetaan',inp
		linein=linein+1
		call letotle(inp,letot,le,icom)
		call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
		! write(22,*)4
		! call getindent(indent,inp,ialw,lopw,nwords)
		if(inp(ialw(1):lopw(1)).jeq.'end')then
		   call addprefix2(letotsub2)
			if(p2)then
			write(outfileunit,'(a)')subline2(1:letotsub2)//' !<ai3> '
			else
				write(outfileunit,'(a)')subline2(1:letotsub2)
				! call writ(outfileunit,subline2(1:letotsub2))
			endif
	!	 nsubline=nsubline+1
	! subline(nsubline)=inp(1:max(1,letot))
	
		else
			if(p)write(outfileunit,'(a)')'<133>:'
			if(p2)then
			write(outfileunit,'(a)')inp(1:max(1,letot))//' !<ai2+>'
			else
			write(outfileunit,'(a)')inp(1:max(1,letot))
			! call writ(outfileunit,inp(1:max(1,letot)))
			endif
		goto 133
		endif !false
	 endif
	 !if(p)!!write(n6,*)' end interface subroutine/function ',linein,inp(1:letot)
	 endif
   goto 131
90 write(6,*)'unexpected end of file while reading interface'


end subroutine anainterface




subroutine writewords(text,nu)
character*(*) text
write(nu,*)text,'/',(inp(ialw(j):lopw(j)),'/',j=1,nwords)
return
end subroutine
function ismoduleword(i)

do i_=1,modulewords
if(p)write(outfileunit,*)'!<1033>',moduleword(i_)(1:len_moduleword(i_))
if(inp(ialw(i):lopw(i)).jeq.moduleword(i_)(1:len_moduleword(i_)))then
ismoduleword=i_
return
endif
end  do
ismoduleword=0

end function

subroutine putmod(i) !puts i:th word into module if it is not there
do i_=1,modulewords
if(p)write(outfileunit,*)'!<1033>',moduleword(i_)(1:len_moduleword(i_))
if(inp(ialw(i):lopw(i)).jeq.moduleword(i_)(1:len_moduleword(i_)))return
end  do
			modulewords=modulewords+1
					
			moduleword(modulewords)=inp(ialw(i):lopw(i))
			modulemod(modulewords)=nmodules
				if(p)write(outfileunit,*)'<578> modulewords word',modulewords,moduleword(modulewords)

			len_moduleword(modulewords)=lopw(i)-ialw(i)+1
			wordfile(modulewords)=nfiles
			if(p)write(6,*)'726> word file ',wordfile(modulewords),nfiles
		!	write(19,*)inp
		!	write(19,*)'*',moduleword(modulewords),icom,letot
						if(icom.gt.0)then
			
			do ic_=icom,letot-1
			if(inp(ic_:ic_+1).eq.'%%')inp(ic_:ic_+1)='!!'
			enddo
			moduleline(modulewords)=inp(icom:letot)
		   lenmoduleline(modulewords)=letot-icom+1
			endif
	!		if(modulewords.le.10)write(outfileunit,*)'nfiles,wordfile',nfiles,wordfile(nfiles)

return
end subroutine
subroutine putsubr(j__) !puts ith word into subroutines list and prefix version into
! makes the new line into subline2
nsubr=nsubr+1
sub(nsubr)=inp(ialw(j__):lopw(j__))
len_sub(nsubr)=lopw(j__)-ialw(j__)+1
lefn=len_newprefix(nfiles)
lefo=len_oldprefix(nfiles)
lefn1=lefn-1
lefo=len_oldprefix(nfiles)
lefo1=lefo-1
!!write(n6,*)'<subr>',nsubr,sub(nsubr)
if(isnewprefix(nfiles).and.inp(ialw(j__):ialw(j__)+lefn1).eq.newprefix(nfiles)(1:lefn))then !had the proper prefix
	!!write(n6,*)'<subr>2'
	sub2(nsubr)=sub(nsubr)
	len_sub2(nsubr)=len_sub(nsubr)
	
	elseif(isoldprefix(nfiles).and.inp(ialw(j__):ialw(j__)+lefo1).eq.oldprefix(nfiles)(1:lefo)      )then  !had the old prefix
	! if(eka.le.3)write(outfileunit,*)'<3>'
	sub2(nsubr)=newprefix(nfiles)(1:lefn)//sub(nsubr)(lefo+1:len_sub(nsubr))
	len_sub2(nsubr)=len_trim(sub2(nsubr))
      !!write(n6,*)'<subr>3'
	else !new prefix not old
	!if(eka.le.3)write(outfileunit,*)'<4>,jlen,ialw(j__),ialw(j__+1)',jlen,ialw(j__),ialw(j__+1)
	  sub2(nsubr)=newprefix(nfiles)(1:lefn)//sub(nsubr)(1:len_sub(nsubr))
	len_sub2(nsubr)=len_trim(sub2(nsubr))
	!!write(n6,*)'<subr>3'
	endif
	subline2=inp(1:ialw(j__)-1)//sub2(nsubr)(1:len_sub2(nsubr))//inp(lopw(j__)+1:letot)
	letotsub2=len_trim(subline2)
	!!write(n6,*)'<subr>subline2 ',subline2(1:letotsub2)
	return

end subroutine

subroutine getuse() !writes use statements and subline and if writeinp also inp

integer i,in,im,letotsub2
!if(p)write(outfileunit,*)'<971> getuse, nused',nused
!if(isoutput)write(outfileunit,'(a)')inp(1:letot)

do i=1,nused
in=used(i)
im=modulemod(in)
if(moduleword(in)(1:len_moduleword(in)).eq.cursub(1:lecursub))then
!subroutine cannot us itself, needed in recursive subroutines
if(f77)then
subline(nsubline+1)='C     use '//modulename(im)(1:len_modulename(im))// &
', only: '//moduleword(in)(1:len_moduleword(in))//'   '//moduleline(in)(1:lenmoduleline(in))
else
subline(nsubline+1)='! use '//modulename(im)(1:len_modulename(im))// &
', only: '//moduleword(in)(1:len_moduleword(in))//'   '//moduleline(in)(1:lenmoduleline(in))
endif
else
if(f77)then
subline(nsubline+1)='     use '//modulename(im)(1:len_modulename(im))// &
', only: '//moduleword(in)(1:len_moduleword(in))//'   '//moduleline(in)(1:lenmoduleline(in))
else
subline(nsubline+1)='use '//modulename(im)(1:len_modulename(im))// &
', only: '//moduleword(in)(1:len_moduleword(in))//'   '//moduleline(in)(1:lenmoduleline(in))
endif
endif
!write(n6,*)'bef:',subline(nsubline+1)
! ! call writewords('befaddpref2',n6)
!if(moduleword(in)(1:len_moduleword(in)).eq.'v')letotsub2=-1
call addprefix(nsubline+1,letotsub2)
if(p2)then
write(outfileunit,'(a)')subline2(1:letotsub2)//' !<use>'
else
write(outfileunit,'(a)')subline2(1:letotsub2) ! //' !'//cursub(1:lecursub)
! call writ(outfileunit,subline2(1:letotsub2))
!write(outfileunit,*)'!'//moduleword(in)(1:len_moduleword(in))//'/'//cursub(1:lecursub)//'/'
endif
! ! call writewords('aftaddpref2',n6)
!write(6,*)'outfileunit ,letotsub2',outfileunit,letotsub2
!write(6,*)'**',subline2(1:letotsub2)
! write(outfileunit,'(a)')'<ap2>'//subline2(1:letotsub2)

end do
nused=0
do i=1,nsubline
! if(isoutput)then
!write(outfileunit,'(a)')subline(i)(1:len_trim(subline(i)))

!write(outfileunit,*)'bef addpre ',subline(i)
! do ii=1,modulewords
! write(outfileunit,*)ii,moduleword(ii)
! enddo
call addprefix(i,letotsub2)

!write(6,*)'outfileunit ,letotsub2',outfileunit,letotsub2
!write(6,*)'**',subline2(1:letotsub2)
if(p2)then
write(outfileunit,'(a)')subline2(1:letotsub2)//' !<ap>'
else
write(outfileunit,'(a)')subline2(1:letotsub2)   !, ' ! ',indent
! call writ(outfileunit,subline2(1:letotsub2))

endif

! endif
enddo 
nsubline=0
subline2=' '
if(p)write(outfileunit,'(a)')'!<1286 getuse>'//inp(1:letot)

end subroutine getuse

subroutine addprefix(is,letotsub2) !adds prefix into subline(is) result subline2
integer is,letotsub2
integer letot2,le2,icom2,jlen,lefn,lefo,lefn1,lefo1,lesu2,ifil,nwords2,isu
! integer::eka=0
 !write(6,*)'*addprefix',is
! subline=subline(i)
call letotle(subline(is),letot2,le2,icom2)
call getwords(subline(is),1,le2,ialw2,lopw2,lim2w,nwords2)
!write(22,*)5
!call getindent(indent,inp,ialw,lopw,nwords)

! if(letotsub2.lt.0)then
 !write(n6,*)'got:',(subline(is)(ialw2(iii):lopw2(iii)),'/',iii=1,nwords2)
! nwords2=-1
! call getwords(subline(is),1,le2,ialw2,lopw2,lim2w,nwords2)

! endif

if(nwords2.le.0.or.(subline(is)(ialw2(1):lopw2(1)).jeq.'implicit').or.(subline(is)(ialw2(1):lopw2(1)).jeq.'format'))then
subline2=subline(is)
letotsub2=letot2
return
endif
!if(subline(is)(ialw2(4):lopw2(4)).jeq.'v')write(6,*)'***bingo'
! call getindent(indent,subline(is),ialw2,lopw2,nwords2)
! write(6,*)'subline',subline(is),'letot,le,icom',letot,le,icom,'nwords ',nwords
! write(6,*)'ialw ',(ialw(j),j=1,nwords+1)
! write(6,*)'ialw ',(lopw(j),j=1,nwords)
! write(6,*)'subline',(subline(is)(ialw(j):lopw(j)),j=1,nwords)
lesu2=0

if(ialw2(1).gt.1)then
subline2(1:ialw2(1)-1)=subline(is)(1:ialw2(1)-1)
lesu2=ialw2(1)-1
endif
! if(eka.eq.0)write(outfileunit,*)'nfiles ',nfiles,wordfile(1:modulewords),'oldp',isoldprefix(1:nfiles),'newp',newprefix(1:nfiles)
loop1: do j__=1,nwords2
jlen=ialw2(j__+1)-ialw2(j__) !sequence to be replaced
if(itypesubline(is).ne.0.and.j__.eq.nwords2)goto 789
if((subline(is)(ialw2(j__):ialw2(j__)+2).jeq.'own').and.&
 (subline(is)(ialw2(j__)+4:ialw2(j__)+4).eq.'_'))then
 subline2(lesu2+1:lesu2+jlen)=subline(is)(ialw2(j__):ialw2(j__+1)-1)
	lesu2=lesu2+jlen
 cycle loop1
 endif

do ia_=1,narguments
if(subline(is)(ialw2(j__):lopw2(j__)).jeq.argument(ia_)(1:len_argument(ia_)))then
subline2(lesu2+1:lesu2+jlen)=subline(is)(ialw2(j__):ialw2(j__+1)-1)
lesu2=lesu2+jlen
cycle loop1
end if
enddo
do isu=1,nsubr
if(subline(is)(ialw2(j__):lopw2(j__)).jeq.sub(isu)(1:len_sub(isu)))then

subline2(lesu2+1:lesu2+len_sub2(isu))=sub2(isu)(1:len_sub2(isu))
lesu2=lesu2+len_sub2(isu)
subline2(lesu2+1: )=subline(is)(lopw2(j__)+1:ialw2(j__+1)-1)
	lesu2=lesu2+ialw2(j__+1)-lopw2(j__)

cycle loop1
end if
end do

do i__=1,modulewords

ifil=wordfile(i__)
lefn=len_newprefix(ifil)
lefo=len_oldprefix(ifil)
lefn1=lefn-1
lefo=len_oldprefix(ifil)
lefo1=lefo-1

! if(eka.le.3)write(outfileunit,*)'j,i__',j__,i__,'   jlen'
! if(eka.le.3)write(outfileunit,*)ifil,lefn,lefo,newprefix(ifil),oldprefix(ifil)
! if(eka.le.3)write(outfileunit,*)subline(is)(ialw(j__):lopw(j__)),moduleword(i__)(1:len_moduleword(i__)),&
! subline(is)(ialw(j__):lopw(j__)).jeq.moduleword(i__)(1:len_moduleword(i__))
if(subline(is)(ialw2(j__):lopw2(j__)).jeq.moduleword(i__)(1:len_moduleword(i__)))goto 77


end do

do ifil=1,nfiles
lefo=len_oldprefix(ifil)
lefo1=lefo-1
if(isoldprefix(ifil))then
if(subline(is)(ialw2(j__):ialw2(j__)+lefo1).eq.oldprefix(ifil)(1:lefo).and.&
 subline(is)(lopw2(j__):lopw2(j__)).ne.'_'.and. &
 (subline(is)(ialw2(1):lopw2(1)).jne.'end').and.&
 .not.((subline(is)(ialw2(1):lopw2(1)).jeq.'use').and.j__.eq.2))then 
 
write(6,*)'***ERROR word ',subline(is)(ialw2(j__):lopw2(j__)),' has prefix of file ',ifil,&
 'but is NOT in modules'
 !write(6,*)'prefix:',oldprefix(ifil)(1:lefo),'isold',isoldprefix
write(6,*)subline(is)(1:letot2)
!!write(n6,*)'***wrn ',subline(is)(ialw2(j__):lopw2(j__)),' has prefix of file ',ifil, 'but is NOT in modules'
!nerr=nerr+1
endif
endif
enddo
789 continue
! write(6,*)'lesu2,jlen,is,ialw(j__),ialw(j__+1)-1',lesu2,jlen,is,ialw(j__),ialw(j__+1)-1
	subline2(lesu2+1:lesu2+jlen)=subline(is)(ialw2(j__):ialw2(j__+1)-1)
	lesu2=lesu2+jlen
	! write(6,*)'subline2',subline2(1:70)
	! if(eka.le.3)write(outfileunit,*)'<1>'
  cycle

77 continue
! if(p)write(outfileunit,*)isnewprefix(ifil)
! if(p)write(outfileunit,*)subline(is)(ialw(j__):ialw(j__)+lefn1)
! if(p)write(outfileunit,*)newprefix(ifil)(1:lefn)
! if(p)write(outfileunit,*)isoldprefix(ifil)
! if(p)write(outfileunit,*)subline(is)(ialw(j__):ialw(j__)+lefo1)
! if(p)write(outfileunit,*)lesu2

if(isnewprefix(ifil).and.subline(is)(ialw2(j__):ialw2(j__)+lefn1).eq.newprefix(ifil)(1:lefn))then !had the proper prefix
	! if(eka.le.3)write(outfileunit,*)'<2>'
	subline2(lesu2+1:lesu2+jlen)=subline(is)(ialw2(j__):ialw2(j__+1)-1)
	lesu2=lesu2+jlen
	
	elseif(isoldprefix(ifil).and.subline(is)(ialw2(j__):ialw2(j__)+lefo1).eq.oldprefix(ifil)(1:lefo)      )then  !had the old prefix
	! if(eka.le.3)write(outfileunit,*)'<3>'
	subline2(lesu2+1:lesu2+lefn)=newprefix(ifil)(1:lefn)
	lesu2=lesu2+lefn
	subline2(lesu2+1:lesu2+jlen-lefo+lefn)=subline(is)(ialw2(j__)+lefo:ialw2(j__+1)-1)
  lesu2=lesu2+jlen-lefo
	else !new prefix not old
	!if(eka.le.3)write(outfileunit,*)'<4>,jlen,ialw(j__),ialw(j__+1)',jlen,ialw(j__),ialw(j__+1)
	! if(j__.eq.4.and.(subline(is)(ialw2(4):lopw2(4)).jeq.'v'))write(6,*)'lesu2',subline2(1:lesu2),&
	! 'newp:',newprefix(ifil)(1:lefn)
		subline2(lesu2+1:lesu2+lefn)=newprefix(ifil)(1:lefn)
	lesu2=lesu2+lefn
	subline2(lesu2+1:lesu2+jlen)=subline(is)(ialw2(j__):ialw2(j__+1)-1)
	lesu2=lesu2+jlen
	! if(j__.eq.4.and.(subline(is)(ialw2(4):lopw2(4)).jeq.'v'))write(6,*)'lesu2',subline2(1:lesu2)
	endif
! write(6,*)'subline2b',subline2(1:70)



enddo loop1
letotsub2=lesu2+letot2-lopw2(nwords2)
! write(6,*)'letotsub2=lesu2+letot-le',letotsub2,lesu2,letot,le
if(letot2.gt.lopw2(nwords2))subline2(lesu2 +1:letotsub2)=subline(is)(lopw2(nwords2)+1:letot2)
!eka=eka+1
return
end subroutine


subroutine addprefix2(letotsub2) !adds prefix into inp line result subline2(1:letotsub2)

! integer::eka=0
 !write(6,*)'*addprefix',is
! subline=subline(i)
!call letotle(inp,letot,le,icom)
!call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
! write(6,*)'subline',inp,'letot,le,icom',letot,le,icom,'nwords ',nwords
! write(6,*)'ialw ',(ialw(j),j=1,nwords+1)
! write(6,*)'ialw ',(lopw(j),j=1,nwords)
! write(6,*)'subline',(inp(ialw(j):lopw(j)),j=1,nwords)
lesu2=0
if(ialw(1).gt.1)then
subline2(1:ialw(1)-1)=inp(1:ialw(1)-1)
lesu2=ialw(1)-1
endif
! if(eka.eq.0)write(outfileunit,*)'nfiles ',nfiles,wordfile(1:modulewords),'oldp',isoldprefix(1:nfiles),'newp',newprefix(1:nfiles)
loop1: do j__=1,nwords
jlen=ialw(j__+1)-ialw(j__)

if((inp(ialw(j__):ialw(j__)+2).jeq.'own').and.&
 inp(ialw(j__)+4:ialw(j__)+4).eq.'_')then
 subline2(lesu2+1:lesu2+jlen)=inp(ialw(j__):ialw(j__+1)-1)
	lesu2=lesu2+jlen
 cycle loop1
 endif

do i__=1,modulewords

ifil=wordfile(i__)
lefn=len_newprefix(ifil)
lefo=len_oldprefix(ifil)
lefn1=lefn-1
lefo=len_oldprefix(ifil)
lefo1=lefo-1

! if(eka.le.3)write(outfileunit,*)'j,i__',j__,i__,'   jlen'
! if(eka.le.3)write(outfileunit,*)ifil,lefn,lefo,newprefix(ifil),oldprefix(ifil)
! if(eka.le.3)write(outfileunit,*)inp(ialw(j__):lopw(j__)),moduleword(i__)(1:len_moduleword(i__)),&
! inp(ialw(j__):lopw(j__)).eq.moduleword(i__)(1:len_moduleword(i__))
if(inp(ialw(j__):lopw(j__)).jeq.moduleword(i__)(1:len_moduleword(i__)))goto 77


end do
do ifil=1,nfiles
lefo=len_oldprefix(ifil)
lefo1=lefo-1
if(isoldprefix(ifil).and.inp(ialw(j__):ialw(j__)+lefo1).eq.oldprefix(ifil)(1:lefo).and.&
 inp(lopw(j__):lopw(j__)).ne.'_')then 
write(6,*)'***wrn  ',subline(is)(ialw(j__):lopw(j__)),' has prefix of file ',ifil, 'but is NOT in modules'
write(6,*)inp(1:letot)
!!write(n6,*)'***wrn',subline(is)(ialw(j__):lopw(j__)),' has prefix of file ',ifil, 'but is NOT in modules'
nerr=nerr+1
endif
enddo 




! write(6,*)'lesu2,jlen,is,ialw(j__),ialw(j__+1)-1',lesu2,jlen,is,ialw(j__),ialw(j__+1)-1
	subline2(lesu2+1:lesu2+jlen)=inp(ialw(j__):ialw(j__+1)-1)
	lesu2=lesu2+jlen
	! write(6,*)'subline2',subline2(1:70)
	! if(eka.le.3)write(outfileunit,*)'<1>'
  cycle

77 continue
! if(p)write(outfileunit,*)isnewprefix(ifil)
! if(p)write(outfileunit,*)inp(ialw(j__):ialw(j__)+lefn1)
! if(p)write(outfileunit,*)newprefix(ifil)(1:lefn)
! if(p)write(outfileunit,*)isoldprefix(ifil)
! if(p)write(outfileunit,*)inp(ialw(j__):ialw(j__)+lefo1)
! if(p)write(outfileunit,*)lesu2

if(isnewprefix(ifil).and.inp(ialw(j__):ialw(j__)+lefn1).eq.newprefix(ifil)(1:lefn))then !had the proper prefix
	!if(eka.le.3)write(outfileunit,*)'<2>'
	subline2(lesu2+1:lesu2+jlen)=inp(ialw(j__):ialw(j__+1)-1)
	lesu2=lesu2+jlen
	
	elseif(isoldprefix(ifil).and.inp(ialw(j__):ialw(j__)+lefo1).eq.oldprefix(ifil)(1:lefo)      )then  !had the old prefix
	! if(eka.le.3)write(outfileunit,*)'<3>'
	subline2(lesu2+1:lesu2+lefn)=newprefix(ifil)(1:lefn)
	lesu2=lesu2+lefn
	subline2(lesu2+1:lesu2+jlen-lefo+lefn)=inp(ialw(j__)+lefo:ialw(j__+1)-1)
 lesu2=lesu2+jlen-lefo
	else !new prefix not old
	!if(eka.le.3)write(outfileunit,*)'<4>,jlen,ialw(j__),ialw(j__+1)',jlen,ialw(j__),ialw(j__+1)
		subline2(lesu2+1:lesu2+lefn)=newprefix(ifil)(1:lefn)
	lesu2=lesu2+lefn
	subline2(lesu2+1:lesu2+jlen)=inp(ialw(j__):ialw(j__+1)-1)
	lesu2=lesu2+jlen
	
	endif
! write(6,*)'subline2b',subline2(1:70)



enddo loop1
letotsub2=lesu2+letot-lopw(nwords)
! write(6,*)'letotsub2=lesu2+letot-le',letotsub2,lesu2,letot,le
if(letot.gt.lopw(nwords))subline2(lesu2 +1:letotsub2)=inp(lopw(nwords)+1:letot)
eka=eka+1
return
end subroutine

subroutine addprefix3(j_,letotsub2) !adds prefix into inp line result subline2(1:letotsub2)

! integer::eka=0
 !write(6,*)'*addprefix',is
! subline=subline(i)
!call letotle(inp,letot,le,icom)
!call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
! write(6,*)'subline',inp,'letot,le,icom',letot,le,icom,'nwords ',nwords
! write(6,*)'ialw ',(ialw(j),j=1,nwords+1)
! write(6,*)'ialw ',(lopw(j),j=1,nwords)
! write(6,*)'subline',(inp(ialw(j):lopw(j)),j=1,nwords)
lesu2=0
if(ialw(j_).gt.1)then
subline2(1:ialw(j_)-1)=inp(1:ialw(j_)-1)
lesu2=ialw(j_)-1
endif
! if(eka.eq.0)write(outfileunit,*)'nfiles ',nfiles,wordfile(1:modulewords),'oldp',isoldprefix(1:nfiles),'newp',newprefix(1:nfiles)
loop1: do j__=j_,j_
jlen=lopw(j_)-ialw(j_)+1

! if((inp(ialw(j__):ialw(j__)+2).jeq.'own').and.&
 ! inp(ialw(j__)+4:ialw(j__)+4).eq.'_')then
 ! subline2(lesu2+1:lesu2+jlen)=inp(ialw(j__):ialw(j__+1)-1)
	! lesu2=lesu2+jlen
 ! cycle loop1
 ! endif

do i__=1,modulewords

ifil=wordfile(i__)
lefn=len_newprefix(ifil)
lefo=len_oldprefix(ifil)
lefn1=lefn-1
lefo=len_oldprefix(ifil)
lefo1=lefo-1

! if(eka.le.3)write(outfileunit,*)'j,i__',j__,i__,'   jlen'
! if(eka.le.3)write(outfileunit,*)ifil,lefn,lefo,newprefix(ifil),oldprefix(ifil)
! if(eka.le.3)write(outfileunit,*)inp(ialw(j__):lopw(j__)),moduleword(i__)(1:len_moduleword(i__)),&
! inp(ialw(j__):lopw(j__)).eq.moduleword(i__)(1:len_moduleword(i__))
if(inp(ialw(j__):lopw(j__)).jeq.moduleword(i__)(1:len_moduleword(i__)))goto 77


end do
do ifil=1,nfiles
lefo=len_oldprefix(ifil)
lefo1=lefo-1
if(isoldprefix(ifil).and.inp(ialw(j__):ialw(j__)+lefo1).eq.oldprefix(ifil)(1:lefo).and.&
 inp(lopw(j__):lopw(j__)).ne.'_')then 
write(6,*)'***wrn  ',subline(is)(ialw(j__):lopw(j__)),' has prefix of file ',ifil, 'but is NOT in modules'
write(6,*)inp(1:letot)
!!write(n6,*)'***wrn',subline(is)(ialw(j__):lopw(j__)),' has prefix of file ',ifil, 'but is NOT in modules'
nerr=nerr+1
endif
enddo 




! write(6,*)'lesu2,jlen,is,ialw(j__),ialw(j__+1)-1',lesu2,jlen,is,ialw(j__),ialw(j__+1)-1
	subline2(lesu2+1:letot)=inp(lesu2:letot)
	letotsub2=letot
	! write(6,*)'subline2',subline2(1:70)
	! if(eka.le.3)write(outfileunit,*)'<1>'
  return

77 continue
! if(p)write(outfileunit,*)isnewprefix(ifil)
! if(p)write(outfileunit,*)inp(ialw(j__):ialw(j__)+lefn1)
! if(p)write(outfileunit,*)newprefix(ifil)(1:lefn)
! if(p)write(outfileunit,*)isoldprefix(ifil)
! if(p)write(outfileunit,*)inp(ialw(j__):ialw(j__)+lefo1)
! if(p)write(outfileunit,*)lesu2

if(isnewprefix(ifil).and.inp(ialw(j__):ialw(j__)+lefn1).eq.newprefix(ifil)(1:lefn))then !had the proper prefix
		subline2(lesu2:letot)=inp(lesu2:letot)
	letotsub2=letot
	! write(6,*)'subline2',subline2(1:70)
	! if(eka.le.3)write(outfileunit,*)'<1>'
  return
	
	elseif(isoldprefix(ifil).and.inp(ialw(j__):ialw(j__)+lefo1).eq.oldprefix(ifil)(1:lefo)      )then  !had the old prefix
	! if(eka.le.3)write(outfileunit,*)'<3>'
	subline2(lesu2+1:lesu2+lefn)=newprefix(ifil)(1:lefn)
	lesu2=lesu2+lefn
	subline2(lesu2+1:lesu2+jlen-lefo+lefn)=inp(ialw(j__):lopw(j__))
 letotsub2=letot+lefn-lefo
	else !new prefix not old
	!if(eka.le.3)write(outfileunit,*)'<4>,jlen,ialw(j__),ialw(j__+1)',jlen,ialw(j__),ialw(j__+1)
		subline2(lesu2+1:lesu2+lefn)=newprefix(ifil)(1:lefn)
		 letotsub2=letot+lefn
	subline2(lesu2+lefn+1:letotsub2)=inp(ialw(j_):letot)
	return
	
	
	endif
! write(6,*)'subline2b',subline2(1:70)



enddo loop1


end subroutine




subroutine anause()  !analyses use lines in the file if line contains !nopre! old use is contained
! otherwise it will be ignored
! if(inp(ialw(1):lopw(1)).jeq.'use')
	nopreuse=.false.
	if(icom.gt.0)then

	 nopreuse=inp(icom:icom+6).jeq.'!nopre!' !nopre!
	endif
	if(nopreuse)then
	!!write(n6,*)inp(1:letot)  ,' !******!nopreuse'
	 call putsubline()
	return
	endif
		212  continue
		!if(le.le.0)write(6,*)'?letot,inp:',letot,inp(1:letot)
		if(inp(max(le,1):max(le,1)).eq.'&'.or.le.le.0)then
		2100 read(infileunit,'(a)',end=90)inp
		!vois tehä alioghjelman


		linein=linein+1
		call letotle(inp,letot,le,icom)
	!	write(6,*)'<1100:> letot,le,icom,inp(le:le)  ,inp,',letot,le,icom,inp(le:le) ,inp(1:letot)
		!write(6,*)'<546> data, ':inp(1:letot)
	!	if(letot.gt.0)write(6,*)'>515>, letot,inp',letot,inp(1:letot)
	 goto 212
    endif
	return	
90 write(6,*)'unexpected end of file while reading use -section after reading ',linein,' lines'
	
	
end subroutine anause

subroutine writesub()
integer ialsub
ialsub=nsub
!!write(n6,*)'nsub'
1120	 read(infileunit,'(a)',end=90)inp 
	
		linein=linein+1
	call letotle(inp,letot,le,icom)
		call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
	!	call getindent(indent,inp,ialw,lopw,nwords)
		if(p2)then
		write(outfileunit,'(a)')inp(1:max(letot,1))//' !<nopre!sub>'
		else
		write(outfileunit,'(a)')inp(1:max(letot,1)) 
		! call writ(outfileunit,inp(1:max(letot,1)))
		endif
		if(issub())then
		nsub=nsub+1
		!!write(n6,*)'!nopre!-new level',linein,inp(1:letot)
		!!write(n6,*)'<1328sub> ',linein,inp(ialw(isubname()):lopw(isubname()))
		goto 1120
		endif
	!if(index(inp(1:letot),'end').gt.0)!!write(n6,*)'end,',inp(1:letot),endsub()
		if(endsub())then
			nsub=nsub-1
			if(nsub.lt.ialsub)then
				!!write(n6,*)'!nopre!-subroutine written ',linein
				return
				
			else
				goto 1120
			endif
	    endif
	90   write(6,*)'<1349> premature end ogf file'
	  stop


end subroutine

subroutine anasub(inmodule) !analyses subroutines and fucntions
logical inmodule

	 nsub=0    !tehty edellä kun tullaan subroutinesta, mutta miten kun tullaan modulista
	 nopresub(nsub)=.false.
	 if(icom.gt.0)nopresub(nsub)=inp(icom:icom+ 6).jeq.'!nopre!'
	 infunction=isfunc()
	 if(infunction)then
	 ifu=ifuncname()
		func=inp(ialw(ifu):lopw(ifu))
	len_func=lopw(ifu)-ialw(ifu)+1
!	write(outfileunit,*)'func',func
	 endif
	 !		 write(6,*)'hep0'
!		 write(outfileunit,*)'tassa <77>'
  !!write(n6,*)'getuse,anasubalussa,nused,nsublines',nused,nsublines
	 call getuse()
	!  write(outfileunit,*)'!tassa <88>,nsubline ',nsubline,' nused',nused
	 !		 write(6,*)'hep1'
	 116	continue
	 if(.not.f77)then

		if(inp(le:le).eq.'&')then
			read(infileunit,'(a)',end=90)inp(le:) !
			leold=le
			!!write(n6,*)'<anasub,contline>',inp(le:len_trim_(inp))
			call letotle(inp,letot,le,icom)
			call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
	!		call getindent(indent,inp,ialw,lopw,nwords)
			!!write(n6,*)'<contsub>',inp(leold:letot)
			linein=linein+1
			if(p2)then
				write(outfileunit,'(a)')inp(leold:letot) ,'  !<cont>'
			else
				write(outfileunit,'(a)')inp(leold:letot)
				! call writ(outfileunit,inp(leold:letot))
			endif
		goto 116
		endif !if(inp(le:le).eq.'&')then
	else !f77
	112	 continue
		if(inp(le:le).eq.',')then
		
			read(infileunit,'(a)',end=90)f77line
			call letotle(f77line,letotf,lef,icomf)
			ifi=nonblank(f77line,1,lef)
			inp(le+1:)=f77line(ifi+1:letotf)
			 call letotle(inp,letot,le,icom)
			 !!write(n6,*)'<contline>',f77line(1:letotf)
			 !!write(n6,*)'<contsub>',inp(le:letot)
			call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
			!call getindent(indent,inp,ialw,lopw,nwords)
			!!write(n6,*)'<contsub>',inp(le:letot)
			linein=linein+1
			if(p2)then
				write(outfileunit,'(a)')f77line(1:letotf) ,'  !<cont>'
			else
				write(outfileunit,'(a)')f77line(1:letotf)
				! call writ(outfileunit,f77line(1:letotf))
			endif
			goto 112
		endif
endif !F77
		call anaarg(nsub) !main arguments
		 ivar=0
		! write(6,*)'hep'
!		 write(outfileunit,*)'!tassa <99>,nsubline ',nsubline,' nused',nused
		 
	! do while (.not.endsub())
 117	 read(infileunit,'(a)',end=90)inp !we come h
		linein=linein+1
	call letotle(inp,letot,le,icom)
		!     write(outfileunit,*)'got:',inp(1:letot),le,letot,icom
		call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
	!	call getindent(indent,inp,ialw,lopw,nwords)
	if(nopresub(nsub))then
	  	 if(p2)then
		
			write(outfileunit,'(a)')inp(1:max(letot,1)) ! // ' !<nopresub>'
			 else
			write(outfileunit,'(a)')inp(1:max(letot,1)) 
			! call writ(outfileunit,inp(1:max(letot,1)))
		
		endif
		
		if(endsub())then
		  nopresub(nsub)=.false.
			if(nsub.gt.0)then
				!!write(n6,*)' contained !nopre! subroutine/function written <3>',linein,inp(1:letot)
				
				nsub=nsub-1
		!		 nopresub(nsub)=.false.
				goto 117
			else
				!!write(n6,*)' end writing !nopre! subroutine/function  ',linein  ,inp(1:letot)
				return
	
			endif
		endif
		if(issub()) then
		!!write(n6,*)'<1494sub> ',linein,inp(ialw(isubname()):lopw(isubname()))
			nsub=nsub+1
			nopresub(nsub)=nopresub(nsub-1)
			if(icom.gt.0)then
			if(inp(icom:icom+ 6).jeq.'!nopre!')nopresub(nsub)=.true.
			endif
			if(nsub.gt.1)then
				write(6,*)'nesting of subroutines/functions too deep <1339>'
				write(6,*)'line ',linein, inp(1:letot)
			stop
			endif
	  ! 	call anasub(nsub)
			!!write(n6,*)' contained subroutine/function <1> ',linein,inp(1:letot)
			!!write(n6,*)'<1479> getuse,nused,nsublines ',nused,nsublines
			call getuse()
			goto 117
		endif 
	endif
 if(nopresub(nsub))goto 117
	if(le.le.0)then
	if(ivar.le.0)then
	if(p2)then
	write(outfileunit,'(a)')inp(1:max(letot,1)) //' !<subcom+>'
	else
	write(outfileunit,'(a)')inp(1:max(letot,1)) 
	! call writ(outfileunit,inp(1:max(letot,1)))
	endif

	else
	call putsubline()
	endif
	goto 117
	endif
	!call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
	if(isimplicit())then
	call putsubline()
!	 write(6,*)'*linein IMPLICIT',linein
	 goto 117
	endif
!	if(ivar.le.0)
	ivar=ivar+1
		
		if(nwords.le.0)then
		call putsubline()
		goto 117
		endif
	if(inp(ialw(1):lopw(1)).jeq.'use')then
 !!write(n6,*)' analyzing use line <3>',linein,inp(1:letot)
  call anause()
 
  goto 117
  end if
    if(istype())then
	  !!write(n6,*)' analyzing type <2>',linein,inp(1:letot)
  call anatype(.false.)
  goto 117
  endif
  
	call putsubline()


	
	
!if(p)write(outfileunit,'(a)')'hep2'
if(endsub())then
!!write(n6,*)' contained subroutine/function ends <2>',linein,inp(1:letot)
!!write(n6,*)'<1531>calling getuse,nused,nsublines',nused,nsublines
call getuse()
if(nsub.gt.0)then
!!write(n6,*)' contained subroutine/function ended <2>',linein,inp(1:letot)
nsub=nsub-1
goto 117
else
 !!write(n6,*)' end subroutine/function  ',linein  ,inp(1:letot)
return
endif


endif


	if(issub()) then
	!!write(n6,*)'<1574sub> ',linein,inp(ialw(isubname()):lopw(isubname()))
	   nsub=nsub+1
	   if(nsub.gt.1)then
	   write(6,*)'nesting of subroutines/functions too deep'
	   write(6,*)'line ',linein, inp(1:letot)
	   stop
	   endif
	  ! 	call anasub(nsub)
	  
	   !!write(n6,*)' contained subroutine/function <2> ',linein,inp(1:letot)
	   !!write(n6,*)'<1556>getuse,nused,nsublines',nused,nsublines
	   call getuse()
   goto 116
 endif 



	if(inmodule)then
	call putuse(firstword(nmodules)-1)
	else
	call putuse(modulewords)
	endif
	goto 117
 !  write(n6,*)'hep2'
   !end do
	
90 write(6,*)'unexpected end of file while reading subroutine/function-section after reading ',&
 linein,' lines'	 
	   
	   
end subroutine anasub 


 
subroutine putuse(lastword) !puts words into use list if they are not there
integer i_,m,iu
loop1: do i_=1,nwords

do iu=1,nused
!write(n6,*)i_,iu,m
m=used(iu)
if(inp(ialw(i_):lopw(i_)).jeq.moduleword(m)(1:len_moduleword(m)))cycle loop1
end do

if(infunction.and.(inp(ialw(i_):lopw(i_)).jeq.func(1:len_func)))cycle loop1

do iu=1,narguments
! write(n6,*)'<599> argument ',argument(iu)(1:len_argument(iu))
!if(inp(ialw(i_):lopw(i_)).jeq.argument(iu)(1:len_argument(iu)))&
! write(n6,*)'<599> argument ',argument(iu)(1:len_argument(iu))
if(inp(ialw(i_):lopw(i_)).jeq.argument(iu)(1:len_argument(iu)))cycle loop1
end do

loop2: do m=1,lastword
!write(n6,*)i_,m
if(inp(ialw(i_):lopw(i_)).jeq.moduleword(m)(1:len_moduleword(m)))then
!write(6,*)'bingo ',moduleword(m),modulemod(m)
! do ia=1,narguments
! if(inp(ialw(i_):lopw(i_)).jeq.argument(ia)(1:len_argument(ia)))then
! write(6,*)'*wrn* ',inp(ialw(i_):lopw(i_)), 'appears both in module', modulename(m), &
! 'and as argument, no use generated'
! exit loop1
! endif

! enddo
nused=nused+1
used(nused)=m
 ! write(outfileunit,*)'<962>putuse,line, ',linein,inp(ialw(i_):lopw(i_)), ' inp:',inp(1:le)
endif

enddo loop2
end do loop1

end subroutine
subroutine anatype(inmodule)
logical inmodule
character*20 typename
integer letype
!	if(nwords.ge.2.and.(inp(ialw(1):lopw(1)).eq.'type'))then
		  ! write(outfileunit,*)'***kukuu'
		 ! close(outfileunit)
		 ! stop
		 typename=inp(ialw(2):lopw(2))
		 letype=lopw(2)-ialw(2)+1
		 if(inmodule)call putmod(2)
            call putsubline()
		
			 if(p)write(outfileunit,*)'566type,S?',inp(1:letot)
			117 read(infileunit,'(a)',end=90)inp
			!vois tehä alioghjelman

			linein=linein+1
			call letotle(inp,letot,le,icom)
			
		
	

				if(p)write(outfileunit,*)'<117S>',inp(1:letot)
		!	write(6,*)'<521> letot,inp:',letot,inp(1:letot)
		
		
			call getwords(inp,1,le,ialw,lopw,lim2w,nwords)
	!		call getindent(indent,inp,ialw,lopw,nwords)
					if(endtype())then
						call putsubline()
				!!write(n6,*)'<557> end type, at line ',linein
				return
			endif
			
			if(iword(typename(1:letype),inp,ialw,lopw,nwords,istart).le.0)then
	     call putsubline1()
		  nwords=nwords-1
			else
			call putsubline()
			endif
			if(nwords.le.0)goto 117
			
			
			
			
			if(inmodule)then
			call putuse(firstword(nmodules)-1)
			else
				call putuse(modulewords)
			endif
				! if(nwords.gt.0)write(6,*)'<117:> nwords,icom,le',nwords,icom,le,':', &
	! (inp(ialw(i):lopw(i)),'#',lim2w(i),i=1,nwords)

			! write(6,*)'<542> nwords,letot,le,firstw', nwords,letot,le, &
				! inp(ialw(1):lopw(1)),inp(ialw(2):lopw(2))
	
			goto 117

	!endif !type
	! write(6,*)'<548> ******'
	90 write(6,*)'unexpected end of file when reading type'
   stop

end subroutine


subroutine anaarg(level) !anal
integer :: i_
ner=0
if(level.eq.0)narguments=0
do i_=isubname()+1,nwords

    narguments=narguments+1   
 argument(narguments)=inp(ialw(i_):lopw(i_))
 len_argument(narguments)=lopw(i_)-ialw(i_)+1

end do
if(level.eq.0)narguments0=narguments
!!write(n6,*)'<1435> level,arguments',level,inp(1:letot)
!!write(n6,*)(argument(i_)(1:len_argument(i_)),'/',i_=1,narguments)
end subroutine

end program jindent

integer function len_trim_(inp)
character*(*) inp
le=len(inp)

if(le.le.0)then
len_trim_=0
return
endif
do len_trim_=le,1,-1
if(ichar(inp(len_trim_:len_trim_)).gt.32)return
enddo
len_trim_=0
return
end function len_trim_

subroutine nextword2(inp,ial,lopinp,lopw,ignore,lim1,lim2) ! gets next word
use jeqmod
logical islet
 ! use modulew
	! sections between [ and ] are ignored .false and .true are ignored
	! section between ' ' are ignored
	!	use errmod
	character*(*), intent(in):: inp
	integer, intent(inout):: ial
	integer, intent(in)::lopinp
	integer, intent(out):: lopw
	character*(*) ignore
	character*(1)lim1
	character*(2) lim2
	integer lenignore
	logical numlet
	logical ispist
	logical isit
	logical isnumber
	logical logic

	
	! logical haka
	!************************************************
  !haka=.false.
!  write(6,*)'alku ',ial
  if(ial.gt.lopinp)return
!	write(6,*)inp(ial:lopinp)
	lenignore=len_trim_(ignore)
!###TESTAUS###
!write(6,*)'nextword <526> ial,le,lop,limit ', ial,le,lop,'/',limit(1:le),'/'
lim1=' '
!write(6,*)'nextword2, ial,lopinp,inp ',ial,lopinp,inp(1:lopinp)
!if(modulewords.eq.40)write(6,*)'**inp',inp,' ial',ial
i=ial
loop1: do while(i.le.lopinp)
 !ial,lopinp
 ! if(inp(i:i+5).jeq.'.true.')then
 ! i=i+6
 ! goto 7
 ! endif
 ! if(inp(i:i+6).jeq.'.false.')then
 ! i=i+7
 ! !write(6,*)'.false.'
 ! goto 7
 ! endif
! write(6,*)'i',i
 if(inp(i:i).eq."'")then
  i=i+1
 !write(6,*)'got:',inp(i:i),i
	 do while(inp(i:i).ne."'".and.i.le.lopinp)
		i=i+1
	 enddo
	 if(i.gt.lopinp)then
		 ial=lopinp+1
		 return
	 endif
 endif
 
 if(inp(i:i).eq.'"')then
  i=i+1
 ! write(6,*)'got:',inp(i:i),i
	 do while(inp(i:i).ne.'"'.and.i.le.lopinp)
		i=i+1
	 enddo
	 if(i.gt.lopinp)then
	! write(6,*)'paluu tas'
		 ial=lopinp+1
		 return
	 endif
 endif
!  write(6,*)'jatketaan:',inp(i:lopinp)
 !sivuutetaan numerot ym
 
 !write(6,*)'tasi ',i
 isit=logic(inp,i,ilop) !ispist(inp,i,lopinp,ilop)
 !write(6,*)isit
 
 if(isit)then
  i=ilop+1
 ! write(6,*)'logic:',inp(i:ilop),'i,lopinp,ilop',i,lopinp,ilop
 ! ! write(6,*)'i,ilop',i,ilop
 
 cycle loop1

 endif
  isit=isnumber(inp,i,lopinp,ilop)
 !write(6,*)isit
 
 if(isit)then
 !write(6,*)'got number:',inp(i:ilop)
 i=ilop+1
  
 cycle loop1
 endif
  if(inp(i:i).eq.'%')then
   i=i+1
   do while( numlet(inp(i:i)))
   i=i+1
   enddo
   cycle loop1
  endif
 if(inp(i:i).ge.'A'.and.inp(i:i).le.'Z')goto 1
 if(inp(i:i).ge.'a'.and.inp(i:i).le.'z')goto 1
 if(inp(i:i).ne.' ')lim1=inp(i:i)
 i=i+1
  end do loop1
 ial=lopinp+1
 !if(modulewords.eq.40)write(6,*)'<686> nextword2 return'
 return
 1 ial=i
! write(6,*)'687> ial,lopinp ',ial,lopinp
loop_A: do i=ial+1,lopinp
	 if(inp(i:i).ge.'A'.and.inp(i:i).le.'Z')cycle
	 if(inp(i:i).ge.'a'.and.inp(i:i).le.'z')cycle
	 if(inp(i:i).ge.'0'.and.inp(i:i).le.'9')cycle
	 if(inp(i:i).eq.'_')cycle
	 if(ispist(inp,i,lopinp,ilop))then
	! write(6,*)'tul loppu',i
	 lopw=i-1
	 lim2='. '
	 else
	! write(6,*)'jatketaan ,i',i
	! cycle 
	 endif
	 
	 ! do j=1,lenignore
		! if(inp(i:i).eq. ignore(j:j))cycle loop_A
	 ! enddo 
	 ! ! if(inp(i:i).eq.'.')then
	   ! i2=i
	   ! do j=i,lopinp
	   ! i2=i2+1
	   ! if(inp(i2:i2).eq.' ')then
	   ! lopw=i2-1
	   ! lim2='  '
	   ! return
	   ! endif
	   ! enddo
	 ! endif
	 lopw=i-1
	 lim2=' '
	 inb= nonblank(inp,i,lopinp)
	 !if(modulewords.eq.40)write(6,*)'**',numlet(inp(inb:inb))
	 if(numlet(inp(inb:inb)))return
 	lim2=inp(inb:inb)
!	if(modulewords.eq.40)write(6,*)'*lim2*',lim2,inp(inb:inb+1)
		inb=inb+1

		if(numlet(inp(inb:inb)))return
		if(lim2(1:1).eq.'='.and.inp(inb:inb).eq.'.')return
		lim2(2:2)=inp(inb:inb)
		if(lim2.eq.'*(')lim2='('
		return
			

		
	
 end do loop_A
 lopw=lopinp
  lim2=' '

  return
end subroutine nextword2

logical function ispist(inp,i,lop,ilop)
logical numlet
character*(*) inp
ispist=.false.
! write(6,*)'ispist i,',i,lop,ilop
if(inp(i:i).eq.'.')then
do i2=i+1,lop
!write(6,*)'i2,inp
if(.not.numlet(inp(i2:i2)))then
if(inp(i2:i2).eq.'.')then
ispist=.true.
ilop=i2
return
else
ilop=i2
return
endif

endif
enddo
else
ilop=i


endif

end function

logical function isnumber(inp,i,lop,ilop)
use jeqmod
logical numlet
logical isnu,isplus,ispis,isoper,logic,ise
character*(*) inp
ise(i)=(inp(i2:i2).jeq.'e').or.(inp(i2:i2).jeq.'d')
isnu(i)=inp(i:i).ge.'0'.and.inp(i:i).le.'9'
isplus(i)=inp(i:i).eq.'+'.or.inp(i:i).eq.'-'
ispis(i)=inp(i:i).eq.'.'
isoper(i)=isplus(i).or.ispis(i).or.(inp(i:i).eq.'*').or.(inp(i:i).eq.'/')
isnumber=.false.
if(isnu(i).or.(isoper(i).and.isnu(i+1)).or.(ispis(i).and.isnu(i+1)))then
	isnumber=.true.
	i2=i+1
	do while((isnu(i2).or.ispis(i2)).and.i2.le.lop)
	  if(ispis(i2).and.logic(inp,i2,ilop2))then
	    ilop=i2-1
		return
	  endif
		i2=i2+1
	end do
	ilop=i2-1
	if(ise(i2).or.(isplus(i2).and.ise(i2)))then
		if(isplus(i2))then
		i2=i2+2
		else
		i2=i2+1
		endif
		do while(isnu(i2).and.i2.le.lop)
		i2=i2+1
		end do
		ilop=i2-1
	endif
else
isnumber=.false.
endif
return


end function

subroutine letotle(inp,letot,le,icom) !letot=total length, le=command length, icom=start of comment
use jeqmod

character*(*),intent(in):: inp	
integer, intent(out):: letot
integer, intent(out):: le
integer, intent(out):: icom
integer i_,nq,nq2
letot=len_trim_(inp)
if(letot.le.0)then
	le=0
	icom=0
return
endif
if(f77)then
	if((inp(1:1).jeq.'c').or.inp(1:1).eq.'*'.or.inp(1:1).eq.'!')then
	icom=1
	else
	icom=index(inp(1:letot),'!')
	endif

else
icom=index(inp(1:letot),'!')
endif
if(icom.le.0)then
	le=letot
elseif(icom.eq.1)then
	le=0
else
	!check if ! is within a character constant

	 ial=1
10 continue	
!write(6,*)'ia,icom ',ial,icom 
	 nc1=0
	 nc2=0
	do i_=ial,icom-1
	
		if(inp(i_:i_).eq."'")nc1=nc1+1
		    if(inp(i_:i_).eq.'"')nc2=nc2+1
			if(nc2.eq.2)then
			nc1=nc1-1
			nc2=0
			endif
	enddo
	i2=mod(nc1,2)
	! write(6,*)'nc1,i2',nc1,i2
!	nc1=0
	if(i2.eq.1)then
		i2=nextlim(inp,icom+1,letot,"'")
		! write(6,*)'i2 tas',i2
		if(i2.gt.letot)then
			write(6,*)'*not proper character constant '
			write(6,*)inp(1:letot)
		else
			i3=nextlim(inp,i2+1,letot,"!")
			if(i3.le.letot)then
				icom=i3
				ial=i2+1
			goto 10
			endif
			icom=0
			le=letot
			return
		endif
	endif
	le=len_trim_(inp(1:icom-1))
	!if(le.eq.1)write(6,*)'<687>ichar ',ichar(inp(1:1))
	
endif

end subroutine letotle

logical function islet(ch)
character*(1) ch
islet=.true.
if(ch.ge.'A'.and.ch.le.'Z')return
	 if(ch.ge.'a'.and.ch.le.'z')return
	 	 if(ch.eq.'_')return

end function

subroutine getwords(inp,ial0,le,ialw,lopw,lim2,nwords)
use jeqmod
	character*(*), intent(in):: inp
	integer, intent(in):: ial0
	integer, intent(in)::le
	integer, dimension(*),intent(out):: ialw
	integer, dimension(*),intent(out):: lopw
!	character*(*) ignore
	character*(1)lim1
	character*(2),dimension(*),intent(out):: lim2
	
	integer, intent(out) :: nwords
	! itest=0
    ! if(nwords.lt.0)itest=1
	nwords=0
  ial=ial0
!	write(6,*)'<655> getwords,ial,lopinp ',ial,le,inp(1:le)
10 call nextword2(inp,ial,le,lopw(nwords+1),' ',lim1,lim2(nwords+1))
!	write(6,*)'<657> aft getword2,ial,le ,lopw,',ial,le,lopw(nwords+1)

if(ial.le.le)then
 nwords=nwords+1
 ialw(nwords)=ial
! write(6,*)'<660 nwords+>',nwords
 ial=lopw(nwords)+1
 goto 10
endif
if(nwords.le.0)return


ialw(nwords+1)=lopw(nwords)+1 ! to get segments fro ial to ial
!write(6,*)'<664 got nwords>',nwords
!read(5,'(a)')chls
return


end subroutine
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

  logical haka
	!************************************************
	
	haka=.false.
	le=len(limit)
	do 1 i=ial,lop
		if(inp(i:i).eq.'[')then 
			haka=.true.
			goto 1
	  endif
		if(haka)then
			if(inp(i:i).eq.']')haka=.false.
		  goto 1
		endif
	  do 2 j=1,le
			if(inp(i:i).eq.limit(j:j)) goto 3
2   continue
1 continue
	i=lop+1
3 nextlim=i
	if(haka)then
		write(6,*)'*unclosed [ in ', inp(ial:lop)
!		err=.true.
	endif
	return
end function nextlim

subroutine getwords2(inp,ial0,le,ialw,lopw,lim2,nwords)

	character*(*), intent(in):: inp
	integer, intent(in):: ial0
	integer, intent(in)::le
	integer, dimension(*),intent(out):: ialw
	integer, dimension(*),intent(out):: lopw
!	character*(*) ignore
	character*(1)lim1
	character*(2),dimension(*),intent(out):: lim2
	
	integer, intent(out) :: nwords
	! itest=0
    ! if(nwords.lt.0)itest=1
	nwords=0
  ial=ial0
!	write(6,*)'<655> getwords,ial,lopinp ',ial,le,inp(1:le)
!10 call nextword2(inp,ial,le,lopw(nwords+1),' ',lim1,lim2(nwords+1))
!	write(6,*)'<657> aft getword2,ial,le ,lopw,',ial,le,lopw(nwords+1)
 10 ial=nonblank(inp,ial,le)


if(ial.le.le)then
lop=nextlim(inp,ial,le,' ')
 nwords=nwords+1
 ialw(nwords)=ial
 lopw(nwords)=lop-1
! write(6,*)'<660 nwords+>',nwords
 ial=lopw(nwords)+1
 goto 10
endif
if(nwords.le.0)return


ialw(nwords+1)=lopw(nwords)+1 ! to get segments fro ial to ial
!write(6,*)'<664 got nwords>',nwords
!read(5,'(a)')chls
return


end subroutine


function nonblank(inp,ial,lop)
	integer, intent(in):: ial,lop
	character*(*), intent(in):: inp
	
	do i=ial,lop
		if(ichar(inp(i:i)).gt.32)then
			nonblank=i
			return
		endif
	enddo
	nonblank=lop+1
	return
end function nonblank

logical function numlet(ch)
character*(1) ch
numlet=.true.
		  if(ch.ge.'A'.and.ch.le.'Z')return
		if(ch.ge.'a'.and.ch.le.'z')return
		if(ch.ge.'0'.and.ch.le.'9')return
numlet=.false.
return

end function numlet

integer function iword(word,inp,ialw,lopw,nwords,istart)
use jeqmod
	character*(*),intent(in):: word
	character*(*),intent(in):: inp	
	integer, dimension(*),intent(in):: ialw
	integer, dimension(*),intent(in):: lopw
	integer, intent(in)::nwords
	integer,intent(out) :: istart
	do iword=1,nwords
		if(inp(ialw(iword):lopw(iword)).jeq.word)then
			istart=lopw(iword)+1
			return
		endif
	enddo
	iword=0
	return
end function iword



logical function logic(inp,i,ilop)
use jeqmod
character*(*) inp
character*(7),dimension(13)::op
data op/'.true.','.false.','.and.','.or.','.ne.','.eq.','.eqv.','.neqv.','.gt.',&
'.ge.','.lt.','.le.','.not.'/
integer,dimension(13) ::let
data let/ 6,7,5,4,4,4,5,6,4,4,4,4,5/
logic=.true.
do i_=1,13
if(inp(i:i+let(i_)-1).jeq.op(i_)(1:let(i_)))then

ilop=i+let(i_)-1
!write(6,*)'yes',op(i_),'i,ilop',i,ilop
return
endif
enddo
logic=.false.
return
return


end function

logical function reserved(ch)
use jeqmod
character*(*) ch
reserved=.true.
if(ch.jeq.'parameter')return
if(ch.jeq.'integer')return
if(ch.jeq.'real')return
if(ch.jeq.'character')return
if(ch.jeq.'double')return
if(ch.jeq.'precision')return
if(ch.jeq.'null')return
if(ch.jeq.'intent')return
if(ch.jeq.'in')return
if(ch.jeq.'inout')return
if(ch.jeq.'out')return
if(ch.jeq.'len')return
if(ch.jeq.'logical')return
if(ch.jeq.'dimension')return
if(ch.jeq.'pointer')return
if(ch.jeq.'target')return
!if(ch.jeq.'.false.')return !not  needed?
!if(ch.jeq.'.true.')return  !not needed
if(ch.jeq.'end')return
if(ch.jeq.'err')return
if(ch.jeq.'lbound')return
if(ch.jeq.'ubound')return
if(ch.jeq.'optional')return
if(ch.jeq.'dim')return
if(ch.jeq.'nullify')return
if(ch.jeq.'max')return
if(ch.jeq.'min')return
if(ch.jeq.'module')return
if(ch.jeq.'contains')return
if(ch.jeq.'data')return
if(ch.jeq.'operator')return
if(ch.jeq.'implicit')return
if(ch.jeq.'none')return
if(ch.jeq.'type')return
if(ch.jeq.'allocatable')return
if(ch.jeq.'form')return
if(ch.jeq.'unit')return
if(ch.jeq.'recl')return
reserved=.false.
return
end function reserved

logical function jeq(ch1,ch2)
character(len=*),intent(in) :: ch1
character(len=*),intent(in) :: ch2
integer :: le1,le2,i
le1=len_trim_(ch1)
le2=len_trim_(ch2)
jeq=.false.
if(le1.ne.le2)return
do i=1,le1

if(ch1(i:i).ge.'a'.and.ch1(i:i).le.'z')then
if(ch2(i:i).ne.ch1(i:i).and.ichar(ch2(i:i)).ne.ichar(ch1(i:i))-32)return
elseif(ch1(i:i).ge.'A'.and.ch1(i:i).le.'Z')then
if(ch2(i:i).ne.ch1(i:i).and.ichar(ch2(i:i)).ne.ichar(ch1(i:i))+32)return
else
if(ch1(i:i).ne.ch2(i:i))return
endif
enddo
jeq=.true.
return


end function jeq

logical function jne(ch1,ch2)
use jeqmod, only: jeq
character(len=*),intent(in) :: ch1
character(len=*),intent(in) :: ch2

jne=.not.jeq(ch1,ch2)

end function jne

subroutine j_indent(infileunit,outfileunit)
use jeqmod
!character*(*)inp0
character*(180) inp
integer outfileunit
integer, parameter :: maxwords=50 !per line
integer,dimension(maxwords) :: ialw
integer,dimension(maxwords) :: lopw
character*2,dimension(maxwords)::lim2w
integer ::indent=0
integer ::ndo=0
character*(20) dolabel(10)
character*(120) endlabel(25)
integer lendlabel(25)
logical ::cont=.false.
integer ::icont
logical ::contif=.false.
logical ::contfirst=.false.
logical ::contcur
integer dolop(10)
character*(40) tabs
logical :: first=.true.
!save indent,ndo,dolabel,dolop,cont,contif,tabs,first
save first,tabs

indent=0

if(first)then
do i=1,40
tabs(i:i)=char(9)  !horizontal tab
 enddo
 first=.false.
endif
ilinn=0
10 read(infileunit,'(a)',end=90)inp
ilinn=ilinn+1
!write(6,*)ilinn,'>',inp


!if(indent.lt.0)write(22,*)'!*****indent',indent
call letotle(inp,letot,le,icom)
! write(outfileunit,'(a)')inp(1:max(1,letot))
! write(outfileunit,*)le
if(le.le.0)then
write(outfileunit,'(a)')inp(1:letot)
cont=.false.
goto 10
endif


contcur=inp(le:le).eq.'&'
contfirst=contcur.and..not.cont
!cont=inp(le:le).eq.'&'
call getwords(inp,1,le,ialw,lopw,lim2w,nwords) !getwords is accessing elements above letot
if(nwords.le.0)then
call wr(indent)
goto 10
endif

if(contfirst.and.inp(ialw(1):lopw(1)).eq.'if')contif=.true.

istart=1
ialku=0
if(ialw(1).gt.1.and.ndo.ge.1)then


call sword(inp,1,ialw(1)-1,ial,lop)

if(ial.eq.0)goto 100
!write(22,*)'?????/',inp(ial:lop),'/',dolabel(ndo)(1:dolop(ndo)),'/',ial,lop,dolop(ndo)
if(inp(ial:lop).eq.dolabel(ndo)(1:dolop(ndo)))then
letot=le+lendlabel(indent)
inp(le+1:letot)=endlabel(indent)
indent=indent-1

ndo=ndo-1
!write(22,'(a,i,i)')inp0//' ndo-1',indent,ndo
call wr(indent)
goto 10


endif
endif
100 continue
if( ndo.gt.0.and.(&
((inp(ialw(1):lopw(1)).jeq.'end').and.(inp(ialw(2):lopw(2)).jeq.'do')).or. &
(inp(ialw(1):lopw(1)).jeq.'enddo')&
 ))then
 letot=le+lendlabel(indent)
inp(le+1:letot)=endlabel(indent)
indent=indent-1

ndo=ndo-1
!write(22,'(a,i,i)')inp0,indent,ndo
call wr(indent)
elseif(&
(inp(ialw(1):lopw(1)).jeq.'end').or. &
(inp(ialw(1):lopw(1)).jeq.'endselect').or. &
(inp(ialw(1):lopw(1)).jeq.'endsubroutine').or. &
(inp(ialw(1):lopw(1)).jeq.'endmodule').or. &
(inp(ialw(1):lopw(1)).jeq.'endfunction').or. &
(inp(ialw(1):lopw(1)).jeq.'endif') &
)then
letot=le+lendlabel(indent)

inp(le+1:letot)=endlabel(indent)
!write(outfileunit,*)'indent=',indent,'enlabel2=/',endlabel(indent)(1:lendlabel(indent)),'/'
indent=indent-1
!write(22,'(a,i)')inp0,indent
call wr(indent)
elseif(&
((inp(ialw(1):lopw(1)).jeq.'do').or.(inp(ialw(2):lopw(2)).jeq.'do')).and.inp(ialw(nwords):lopw(nwords)).ne.'enddo'&
.and.inp(ialw(nwords):lopw(nwords)).ne.'do')then
ndo=ndo+1

dolabel(ndo)='?'
dolop(ndo)=1
if(inp(lopw(1)+1:ialw(2)-1).ne.' '.and.(inp(ialw(1):lopw(1)).jeq.'do'))then

call sword(inp,lopw(1)+1,ialw(2)-1,ial,lop)
dolabel(ndo)=inp(ial:lop)
dolop(ndo)=lop-ial+1
!write(22,*)'*dolabel',dolabel(ndo),dolop(ndo),ial,lop,lopw(1)+1,ialw(2)-1
endif
!write(22,'(a,i,i,a)')inp0,indent,ndo
call wr(indent)
 indent=indent+1
 endlabel(indent)=' !'//inp(ialw(1):le)
lendlabel(indent)=le-ialw(1)+3
! write(outfileunit,*)'indent=',indent,'enlabel=/',endlabel(indent)(1:lendlabel(indent)),'/'
 
elseif( &
(inp(ialw(1):lopw(1)).jeq.'subroutine') .or.&
(inp(ialw(1):lopw(1)).jeq.'recursive') .or.&
 iword('function',inp,ialw,lopw,nwords,istart).gt.0.or. &
 ((inp(ialw(nwords):lopw(nwords)).jeq.'then').and.(inp(ialw(1):lopw(1)).jeq.'if')).or.  &
 ((inp(ialw(nwords):lopw(nwords)).jeq.'then').and.&
    (inp(ialw(2):lopw(2)).jeq.'if').and.(inp(ialw(1):lopw(1)).jne.'else')).or.  &
 ((inp(ialw(nwords):lopw(nwords)).jeq.'then').and.contif).or.  &
(inp(ialw(1):lopw(1)).jeq.'module').or.&
(inp(ialw(1):lopw(1)).jeq.'program').or.&
  (inp(ialw(1):lopw(1)).jeq.'select').or.&
 ((inp(ialw(1):lopw(1)).jeq.'type').and.nwords.eq.2).or.&
 (inp(ialw(1):lopw(1)).jeq.'interface'))then
 !write(22,'(a,i)')inp0,indent
 !if((inp(ialw(nwords):lopw(nwords)).jeq.'then').and.contif)write(22,*)'hiphei'
 
 call wr(indent)
 indent=indent+1
 endlabel(indent)=' !'//inp(ialw(1):le)
lendlabel(indent)=le-ialw(1)+3
! write(outfileunit,*)'indent=',indent,'enlabel=/',endlabel(indent)(1:lendlabel(indent)),'/'
!contif=.false.

elseif(&
(inp(ialw(1):lopw(1)).jeq.'else').or. &
(inp(ialw(1):lopw(1)).jeq.'case').or. &
(inp(ialw(1):lopw(1)).jeq.'elseif')  )then
!write(22,'(a,i)')inp0//'ELSE*',indent-1
! contif=.false.
letot=le+lendlabel(indent)
inp(le+1:letot)=endlabel(indent)
call wr(indent-1)
else
call wr(indent)
!write(22,'(a,i)')inp0,indent
endif
goto 10


90 if(indent.gt.0)then
write(6,*)'***line ',ilinn, ' indent was ',indent,' should be zero'
indent=0
endif
return
contains

subroutine wr(nt)

do ifi=1,letot
if(ichar(inp(ifi:ifi)).gt.32)goto 19
enddo
!write(22,'(a,i)')inp0,nt
write(outfileunit,'(a)')inp(1:letot)

! cont=.false.
return
19 continue
icont=0
if(cont)icont=1

nt2=nt+icont
if(inp(ifi:ifi).ge.'0'.and.inp(ifi:ifi).le.'9'.and..not.cont)then
write(outfileunit,'(a)')inp(1:letot)

else if(nt2.le.0)then
!write(22,'(a,i)')inp0(ifi:),nt2
write(outfileunit,'(a)')inp(ifi:letot)
else
!write(22,'(a,i)')tabs(1:nt2)//inp0(ifi:),nt2
write(outfileunit,'(a)')tabs(1:nt2)//inp(ifi:letot)
endif
cont=contcur
if(.not.contcur)contif=.false.
end subroutine
end subroutine

subroutine sword(inp,i1,i2,ial,lop) !get a single word
character*(*) inp

do i=i1,i2
if(ichar(inp(i:i)).gt.32)then
ial=i
goto 2
endif
enddo
ial=0
lop=0
return
2 continue
do i=ial,i2
if(ichar(inp(i:i)).le.32)then
lop=i-1
return

endif

enddo
lop=i2
return
end subroutine

