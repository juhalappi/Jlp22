! Copyright (C) 2022 Juha Lappi
! This is published using MIT licence 


!+--! with gfortran J_precompiler.exe can be made using: gfortran j_precompiler2.f90 -o j_precompiler2

!The precompiler generates necessary 'use' statments for global variables or subroutines stored
! in modules.
! The prefixes and the files to be precompiled are in the command file whose default name is j_pre.txt.
! the program generates the output files indicated in the command file
! i the log is written to unit 4 to a file whose name will be fort.4 or a similar name
! depending on the environment.
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
	integer::le,letot
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




program jpre
!use koemod,only: koe2
!	use jeqmod
	use pref
	use writeout
	use main

 ! goto 175

	nun=0
! read(5,*)in
 ! moduleword(in)='9'
! write(6,*)'log written to file fort.4 (or similar name depending on compiler)'
	goto 1
11 write(6,*)'error opening file ',inp(1:le), ',iostat =',iostatus, ' try again'
	stop
	
1 write(6,'(a,$)')'Give command file, default jpre.txt>'
	read(5,'(a)')inp
	le=len_trim_(inp)
	if(le.le.0)then
		open(1,file='jpre.txt',err=11,status='old')
	else
		open(1,file=inp(1:len_trim_(inp)),err=11,status='old')
	endif
		nfiles=0
		read(1,'(a)')inp
		le=len_trim(inp)
		write(6,*)inp(1:le)
		ib=index(inp(1:le),' ')
	!	write(6,*)'ib ',ib
		ial=1
		nprefix=0
		ib=nextlim(inp,ial,le,' ')
		do while(ib.lt.le)
		nprefix=nprefix+1
		prefix(nprefix)=inp(ial:ib-1)
		lprefix(nprefix)=ib-ial
		ial=ib+1
		ib=nextlim(inp,ial,le,' ')
		enddo
		nprefix=nprefix+1
		prefix(nprefix)=inp(ial:le)
		lprefix(nprefix)=le-ial+1
		write(6,*)'prefixes ',prefix(1:nprefix)
		goto 2
99 write(6,*)'all files precessed'
	stop
2 read(1,'(a)',end=99)inpf
		lef=len_trim(inpf)
		
		
141		if(inpf(1:2).eq.'R:')then
			if1=3
			write4=.false.
		else
			if1=1
			write4=.true.
		endif
		
		if(write4)then
			write(6,*)' '
			write(6,*)'processing ',nfiles,'  ',inpf(if1:lef)
		else
			write(6,*)'processing ',nfiles,'  ',inpf(if1:lef),', reading only modules'
		endif
		
		goto 15
110		write(6,*)'error in opening '
			stop
		
15		open(2,file=inpf(if1:lef),err=110,status='old')
		nfiles=nfiles+1
		if(write4)then
			ipi=index(inpf(1:lef),'.')
			inquire(file=inpf(1:ipi-1)//'_2'//inpf(ipi:lef),exist=exisout) !old
			if(exisout)then
				inquire(file=inpf(1:ipi-1)//'_3'//inpf(ipi:lef),exist=exis) !old
				if(exis)then
					open(4,file=inpf(1:ipi-1)//'_3'//inpf(ipi:lef),status='old')
					close(4,status='delete')
				endif
				open(4,file=inpf(1:ipi-1)//'_3'//inpf(ipi:lef),status='new')
				write(6,*)char(9)//'opening new output ',inpf(1:ipi-1)//'_3'//inpf(ipi:lef)
				open(3,file=inpf(1:ipi-1)//'_2'//inpf(ipi:lef),err=11,status='old',action='READWRITE')
				write(6,*)char(9)//'opening also old output ' ,inpf(1:ipi-1)//'_2'//inpf(ipi:lef)
			else
				open(4,file=inpf(1:ipi-1)//'_2'//inpf(ipi:lef),err=11,status='new')
				write(6,*)char(9)//'opening ',inpf(1:ipi-1)//'_2'//inpf(ipi:lef)
			endif
		endif
		
		call readfile()
	!	write(6,*)'<4554tas'
	
		if(.not.write4) goto 2 !readfile closes 2
	!	goto 20
	!	call writeout()
		if(.not.exisout)then
			close(4)
		 goto 2 !directly output
		endif
		rewind(4)
		ili=0
		
		!write(6,*)'<45545tas'
		end3=.false.
		do while(.true.)
			read(3,'(a)',end=3)inp
			ili=ili+1
			goto 8
3			end3=.true.		
			
8			read(4,'(a)',end=7)inp2
			if(end3)goto 90 !file 3 ended but 4 did not 
			
			goto 70     !check difference
			
7			continue
			if(	end3)then !
				write(6,*)'           *** new output is the same as old'
				close(3)
				close(4)  !close(4,status='delete')
				goto 2  !goto new file
			endif
			goto 90  !file 4 ended but file 3 did not
70			continue			
			le=len_trim_(inp)
			le2=len_trim_(inp2)
			if(le.ne.le2)goto 90
			if(inp(1:le).ne.inp2(1:le))goto 90
		enddo !while
		
90		write(6,*)'************************************output changed  at line' ,ili

		close(3,status='delete')
		close(4)
		write(6,*)'rename ',inpf(1:ipi-1)//'_3'//inpf(ipi:lef),'  ',inpf(1:ipi-1)//'_2'//inpf(ipi:lef)
		call rename(inpf(1:ipi-1)//'_3'//inpf(ipi:lef),inpf(1:ipi-1)//'_2'//inpf(ipi:lef),istatus) 
		write(6,*)'status ',istatus, ' 0 =success'
		goto 2


	 end program jpre!program jprenew

	! if(ifin_.gt.1.and.genuse)then
		! call j_indent(infileunit,outfileunit)
		! goto 90

	! endif !if(ifin_.gt.1)then
!	contains
	
	subroutine readfile()
		use sub
		use writeout   !write4
		use pref
		use main, only:inpf,if1,lef
!		use pref
		integer,dimension(8)::datevalues
		integer ::le1

		logical ::endmodule,issub,endsub,ok,inprogram,inmodule,insub
		logical incontains,incontsub
		nsubs=0
		inprogram=.false.
		inmodule=.false.
		insub=.false.
		call insubs(linein,subsit,subslines,nsubs,insub)
	
		incontains=.false.
		incontsub=.false.
!		incontfunc=.false.
		linein=0
		nsubline=0
		nused=0
	!	p=nfiles.eq.10
		goto 30
90		close(2)
		!write(6,*)'<6alsulopuslinein,inmodule,insub,incontsub ',linein,inmodule,insub,incontsub
		! if(nfiles.eq.1)then
			! do ii=1,modulewords
			! write(6,*)ii,moduleword(ii),lenmoduleword(ii)
			! enddo
		! endif
		return
	30	read(2,'(a)',end=90)inp
		linein=linein+1
!		p=linein.eq.2464.or.linein.eq.2503
		letot=len_trim_(inp)
		icom=0
		le=0
		if(letot.gt.0)icom=index(inp(1:letot),'!')
		if(icom.gt.1)then
			le=len_trim_(inp(1:icom-1))
		
		elseif(icom.eq.1)then
		le=0
		else
			le=letot
			!write(16,*)'<3 ',inp(1:letot)
		endif
		if(inpf(if1:lef).eq.'j.f90')then
			if(inp(3:10).eq.'j_title=')then
			call date_and_time(values=datevalues)
				idst=index(inp(1:30),'22.')
				write(inp(idst:idst+1),'(i2)')datevalues(3)
				write(inp(idst+4:idst+5),'(i2)')datevalues(2)
				write(inp(idst+8:idst+11),'(i4)')datevalues(1)
			endif
		endif
		le1=max(le,1)
		
		if(le.le.0)then
		  if(.not.insub.and..not.inmodule)then
		  if(write4)write(4,'(a)')inp(1:letot)
		 
		  goto 30
		 endif
		 endif
		 ihip=0
		 if(le.gt.0)ihip=index(inp(1:le),"'")
		 if(le.gt.0.and.ihip.eq.0)ihip=index(inp(1:le),'"')
		 hipsu=ihip.ne.0
		!write(16+nfiles,*)'<23 le,le1',le,le1
!		call letotle(inp,letot,le,icom)
		
		if(inp(1:le).eq.'subroutine')then
			write(6,*)'line ',linein, 'wild subroutine'
			stop
		end if 
		le1=max(le,1)
		!if(p)write(16+nfiles,*)'<44tasle',linein,le,le1inmodule,insub,inp(1:letot)
		
		
		if(endmodule())then
			if(.not.inmodule)then
				write(6,*)linein,' ',inp(1:letot),'  BUT WE ARE NOT IN MODULE'
				stop
			endif
			inmodule=.false.
			if(p)write(6,*)'end module ',modulename(nmodules), 'used ',nused, ' got ',modulewords-modulewords0,&
			' nsubline ',nsubline
			do ius=1,nused
				in=used(ius)
				im=modulemod(in)
				if(write4)write(4,'(a)')& 
				char(9)//'use '//modulename(im)(1:lenmodulename(im))// &
						', only: '//moduleword(in)(1:lenmoduleword(in))
			enddo 
			nused=0
			do j=1,nsubline
				if(write4)write(4,'(a)')subline(j)(1:lensubline(j))
			enddo
			nsubline=0
			if(write4)write(4,'(a)')inp(1:letot)
			
			
			!if(p.and.nmodules.gt.0)write(6,*)linein,' end module', modulename(nmodules)
			if(p.and.nmodules.gt.0)then
				
				do j=modulewords0+1,modulewords
					write(6,*)j,moduleword(j),lenmoduleword(j)
				
				enddo
			endif
			
			goto 30
		
		endif
		
		call getmodule(lemo)
	!	write(16+nfiles,*)'ismodule',lemo
		if(lemo.gt.0)then
			if(inmodule)then
				write(6,*)'line ',linein, 'new module without closing previous'
				stop
			endif
			if(write4)write(4,'(a)')inp(1:letot)
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
		
			endif
			goto 30
		endif
		
		if(inmodule)then

		!	if(write4)write(4,'(a)')inp(1:letot)

			if(nopremod)then
				if(write4)write(4,'(a)')inp(1:letot)
				goto 30
			endif
		!	nold=modulewords
			call modulew()
			call putsubline()
			!if(modulewords.gt.nold)write(6,*)'moduleword ',modulewords,moduleword(modulewords)
			goto 30
		
		endif !inmodule
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
					
					enddo
					stop
				endif
			incontains=.true.
			if(p)write(6,*)'<555********',incontains,linein,inp(1:le)
			incontline=linein
			call putsubline()
			goto 30
			endif
		endif
		
		if(issub())then
			
			if(p)write(6,*)'<34>',linein,le,insub,incontains,incontsub,inp(1:letot)
			if(insub.and. .not. (incontains.and..not.incontsub))then
				write(6,*)'line ',linein,' illegal ',inp(1:letot)
				write(6,*)'we are already in ',subname
			!	write(6,*)'<477474 laitettiin ',incontline
	
				stop
			elseif(incontains)then
				incontsub=.true.
				subnamecont=subname0
				linecontsub=linein
				call putsubline()
				goto 30
			else
				if(write4)write(4,'(a)')inp(1:letot)
				if(write4.and.inp(le:le).eq.'&')then
				read(2,'(a)',end=90)inp
				linein=linein+1
				write(4,'(a)')inp(1:len_trim(inp))
				
				endif
				!if(p)write(6,*)'<29> insub',insub
				insub=.true.
				call insubs(linein,subsit,subslines,nsubs,insub)
				if(isfunc)infunc=.true.
				subname=subname0
				lensubname=len_trim(subname)
				linesub=linein
				nused=0
				goto 30
			endif
			
			
		endif
		if(endsub())then
			if(p)write(6,*)'<19>endsub',linein,incontsub,insub,inp(1:letot)
			if(incontsub)then
				incontsub=.false.
				call putsubline()
				if(p)write(6,*)'<19>',linein,' endcontsub ',inp(1:letot)
				linecontend=linein
				goto 30
			elseif(.not.insub)then
				write(6,*)'line ',linein,' *not in subroutine ',  inp(1:letot)
				write(6,*)' last subroutine started in ',linesub, 'lastcontsub in ', linecontsub
				write(6,*)' last contains started in ',incontline,' ended  in ',linecontend
				stop
			endif
		
			if(p)write(6,*)'nused ',nused,' nsubline ',nsubline
			!write(6,*)'writing use',nused,'*'
			
			do ius=1,nused
				in=used(ius)
				im=modulemod(in)
				if(write4)write(4,'(a)')& 
				char(9)//'use '//modulename(im)(1:lenmodulename(im))// &
						', only: '//moduleword(in)(1:lenmoduleword(in))
				if(p)write(6,'(a)')& 
				char(9)//'use '//modulename(im)(1:lenmodulename(im))// &
						', only: '//moduleword(in)(1:lenmoduleword(in))		
			enddo 
			nused=0
!			call getuse()
		!	write(6,*)'<52>',linein,' writing ',nsubline, 'lines'
			!if(p)if(write4)write(4,*)'<52>',linein,' writing ',nsubline, 'lines'
			do j=1,nsubline
				if(write4)write(4,'(a)')subline(j)(1:lensubline(j))
			enddo
			if(write4)write(4,'(a)')inp(1:letot)
			insub=.false.
			call insubs(linein,subsit,subslines,nsubs,insub)
			infunc=.false.
			nsubline=0
			incontains=.false.
			linesubend=linein
			goto 30
			
				
				
		endif
		if(insub)then
			call putsubline()
			call subwords()
			goto 30
		end if
		if(write4)write(4,'(a)')inp(1:letot)
		goto 30
	!	write(6,*)'<644linein,inmodule,insub,incontsub ',linein,inmodule,insub,incontsub
		

	end subroutine
	
	logical function endsub()
		
		use  sub, only: inp,le,hipsu
		
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
		use  sub, only: inp,le,hipsu
		endmodule=.false.
		if(hipsu)return
		if(le.lt.9)return
		iend=max(index(inp(1:le),'end'),index(inp(1:le),'END'))
		if(nonblank(inp,1,le).lt.iend)return
		imod=max(index(inp(1:le),'module'),index(inp(1:le),'MODULE'))
		if(iend.gt.0.and.imod.gt.0)then
		endmodule=.true.
		else
		endmodule=.false.
		endif
	end function
	
	logical function issub()
		use sub,only:inp,le,subname0,hipsu,isfunc,linein
		issub=.false.
	
		if(hipsu)return
		if(le.lt.7)return
		iend=max(index(inp(1:le),'end'),index(inp(1:le),'END'))
		
		ial=0
		ifu=max(index(inp(1:le),'function'),index(inp(1:le),'FUNCTION'))
		if(ifu.gt.0)then
			
			ial=ifu+8
			isfunc=.true.
			
			istart=ifu
		endif
		isu=max(index(inp(1:le),'subroutine'),index(inp(1:le),'SUBROUTINE'))
		if(isu.gt.0)then
			ial=isu+10
			istart=isu
		end if
		ipu=max(index(inp(1:le),'program'),index(inp(1:le),'PROGRAM'))
		if(ipu.gt.0)then
			
			ial=ipu+7
			istart=ipu
			if(iend.gt.0.and.iend.lt.ipu)return
			issub=.true.
			subname0=inp(ial:le)
			return
		endif
		if(ial.le.0)return
		if(iend.gt.0.and.iend.lt.ial)return
		if(inp(ial:ial).gt.' ')return 
		if(istart.gt.1)then
			if(inp(istart-1:istart-1).gt.' ')return  
		endif
		
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
		enddo
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
		imo=max(index(inp(1:le),'module'),index(inp(1:le),'MODULE'))
		if(imo.le.0)return
		if(imo.gt.0)then
			ib=nonblank(inp,1,imo)
			if(ib.ne.imo)then
			imo=0
			return
			endif
		endif
		do j=le,imo,-1
			if(inp(j:j).eq.' ')exit
		enddo
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
				enddo
				!cycle ploop
	70		continue			
		if(pm)write(6,*)'k,chk',k,inp(k:k)
		if(ipf.gt.1)then
			ipf1=ipf-1
			if(inp(ipf1:ipf1).ge.'A'.and.inp(ipf1:ipf1).le.'Z'.or. &
					inp(ipf1:ipf1).ge.'a'.and.inp(ipf1:ipf1).le.'z'.or. &
					inp(ipf1:ipf1).eq.'_'.or. &
					inp(ipf1:ipf1).ge.'0'.and.inp(ipf1:ipf1).le.'9')goto 66
		
		endif
		if(inhipsu(ipf,k-1))goto 66
			if(inp(ipf:k-1).eq.prefix(ip)(1:lprefix(ip)))then
					write(6,*)'line ',linein, ' do not use prefix as a variable'
					write(6,*)inp(1:le)
					stop
			endif
		
		
	loop2:		do j=1,modulewords0
	!if(k-1-ipf.gt.25.or.lenmoduleword(j).gt.29)write(6,*)modulewords0,ipf,j,le,lienein,inp(1:le)
					if(lenmoduleword(j).ne.k-ipf)cycle
					if(moduleword(j)(1:lenmoduleword(j)).eq.inp(ipf:k-1))then
						do ii=1,nused
							if(used(ii).eq.j)then
							k=k+1
							goto 66
							
							endif
						enddo
						nused=nused+1
						used(nused)=j
						! write(6,*)linein,nused,j,istart,k,le,ial,inp(1:le)
						
						!if(nused.le.20)write(6,*)'nused,j',nused,j,moduleword(j),inp(1:le)
						k=k+1
						goto 66
						
					endif
				
				enddo loop2
		
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
			enddo !while ipf gt.0
		enddo ploop
	end subroutine	
	




integer function len_trim_(inp)
	character*(*) inp
	le=len(inp)

	if(le.le.0)then
		len_trim_=0
		return
	endif !if(le.le.0)then
	do len_trim_=le,1,-1
		if(ichar(inp(len_trim_:len_trim_)).gt.32)return
	enddo !do len_trim_=le,1,-1
	len_trim_=0
	return
end function len_trim_ !integer function len_trim_(inp)

	subroutine putsubline()
	use sub,only:inp,letot,subline,nsubline,lensubline

		nsubline=nsubline+1
		subline(nsubline)=inp(1:max(letot,1))
		lensubline(nsubline)=max(letot,1)
!		write(16,*)inp(1:max(letot,1))
	end subroutine !subroutine putsubline()
	
	subroutine subwords()
		use sub
		use pref
		
		integer ipf,ial,ip
		logical pm
		logical inhipsu
		pm=.false.
!		pm=index(inp(1:le),'j_o(').gt.0.and.index(inp(1:le),'j_ivnames').gt.0
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
				enddo
				
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
		
				endif
				
				if(inp(ipf:k-1).eq.prefix(ip)(1:lprefix(ip)))then
					write(6,*)'line ',linein, ' do not use prefix as a variable'
					write(6,*)inp(1:le)
					stop
				endif
				
	loop2:		do j=1,modulewords
					!	if(pm)write(6,*)'<32>',ipf,k,inp(1:le),'*',inp(ipf:k-1)
					if(k-ipf.ne.lenmoduleword(j))cycle
					if(moduleword(j)(1:lenmoduleword(j)).eq.inp(ipf:k-1))then
			!		if(pm)write(6,*)'<53 ',inp(ipf:k-1),nused,used(1:nused),' j ',j
						do ii=1,nused
					!	if(pm)write(6,*)'was used ',ii,inp(ipf:k-1)
							if(used(ii).eq.j)goto 19
						enddo
						if(infunc.and.lensubname.eq.k-ipf)then
						! if(linein.gt.570.and.linein.lt.900)write(6,*)'<6623',inp(ipf:k-1),&
						! subname(1:len_trim(subname)),index(subname,inp(ipf:k-1))
						! if(index(subname,inp(ipf:k-1)).gt.0)write(6,*)'<554FU',subname
					!	if(pm)write(6,*)'/'//subname(1:len_trim(subname))//'/'
						if(inp(ipf:k-1).eq.subname(1:lensubname))goto 19
						
						endif
					!if(isfunc)	write(6,*)'3773',linein,isfunc,nused,inp(ipf:k-1),subname
						nused=nused+1
						used(nused)=j
						
						
						!if(nused.le.20)write(6,*)'nused,j',nused,j,moduleword(j),inp(1:le)
						goto 19
						
					endif
				
				enddo loop2
				 write(6,*)'prefix word ',inp(ipf:k-1),' not in modules????????????',nused,' nused'
				 write(6,*)'line ',linein,k-ipf.ne.lenmoduleword(j),k-ipf,lenmoduleword(j)
				 write(6,*)inp(1:le)
				 ! do j=1,modulewords
				  ! write(6,*)j,moduleword(j)(1:lenmoduleword(j))
				  ! ! if(j.eq.952)write(6,'(a)')'/'//inp(ipf:k-1)//'/'//moduleword(j)(1:lenmoduleword(j))//'/'
				  ! ! if(j.eq.952)write(6,*)k-ipf.ne.lenmoduleword(j),k-ipf,lenmoduleword(j)
				 ! ! if(j.eq.952) write(6,*)used(1:nused)
				  ! ! if(j.eq.952) write(6,*)'infunc ',subname(1:lensubname)
				  ! enddo
				  stop
			! if(k-ipf.eq.lenmoduleword(j))then
			! write(6,*)j,k-ipf.ne.lenmoduleword(j),moduleword(j)(1:lenmoduleword(j))
				! do ii=1,nused
				! if(used(ii).eq.j)write(6,*)'used ',ii,j,moduleword(j)(1:lenmoduleword(j)).eq.inp(ipf:k-1)
							
					! enddo
			! endif
				! enddo
				! stop
				
19				ial=k+1 
				if(ial.ge.le-lprefix(ip))cycle ploop
				ipf=index(inp(ial:le),prefix(ip)(1:lprefix(ip)))
				
				if(ipf.gt.0)ipf=ipf+ial-1
				ial=ipf+lprefix(ip)
				 
			enddo
		enddo ploop
			
	end subroutine	
	



function nonblank(inp,ial,lop)
	integer, intent(in):: ial,lop
	character*(*), intent(in):: inp
	!tabulators are counted as blank
	do i=ial,lop
		if(ichar(inp(i:i)).gt.32)then
			nonblank=i
			return
		endif !if(ichar(inp(i:i)).gt.32)then
	enddo !do i=ial,lop
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

	le=len(limit)
	do 1 i=ial,lop
	
		do 2 j=1,le
			if(inp(i:i).eq.limit(j:j)) goto 3
2   continue !do 2 j=1,le
1 continue !do 1 i=ial,lop
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
endif
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


