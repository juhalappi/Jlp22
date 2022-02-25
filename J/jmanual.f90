!+--! with gfortran J_precompiler.exe can be made using: gfortran j_precompiler.f90 -o j_precompiler




program j_manual
!use koemod,only: koe2

	use jmod, only: j_nfunctions_,j_noptions_,j_functions,j_options
	integer,parameter ::maxsections=2000
	integer,parameter :: maxmacros=200  !common options
	integer,parameter :: maxfuncs0=100 !per section
	integer,parameter :: maxfuncs=1000 !all together
	integer,parameter::maxhead=500 !lines per header
	integer,parameter::maxsec=1000 !lines per sectio
	integer,parameter::maxlfunc=10000 !lines per function
	integer,parameter::maxex=10000 !examples
	integer,parameter::maxlex=500 !lines per example
	integer,parameter::maxopt=1000 !maximum number of option sections

	character*180,dimension(maxlex,maxex)::ex
	integer,dimension(maxlex,maxex)::leex=0
	integer,dimension(maxex)::ninexample=0
	integer,dimension(maxex)::fexl,lexl !first and last line in example
	integer,dimension(maxsections)::fex,lex !first and last example in section
	logical,dimension(maxsections):: written=.false.
	character*80,dimension(maxex)::exlabel
	integer,dimension(maxex)::lexlabel
	integer::nexample=0
	integer,parameter::maxnote=10000 !examples
	integer,parameter::maxlnote=100 !lines per note
	character*500,dimension(maxnote,maxlnote)::note  !note 
	character*10,dimension(maxex)::notelabel
	integer,dimension(maxsections)::fnote,lnote=0
	integer,dimension(maxnote)::fnotel,lnotel=0
	character*40,dimension(maxsections)::section
	character*20,dimension(maxsections)::sectionlabel
	character*20,dimension(maxmacros)::macrolabel
	integer,dimension(maxsections)::fopt,lopt=0  !first and last option
	integer,dimension(maxopt)::foptl,loptl=0  !first and last line in option
!	logical,dimension(maxsections)::subsubsection
!	logical,dimension(maxsections)::subsection
	integer,dimension(maxsections)::nfuncs=0
	integer,dimension(maxsections)::lsection=0 !lenggth of section name
	integer,dimension(maxmacros)::lmacro=0  !number of examples per section
	
	character*1500 line,linec,line2
	character*500,dimension(maxhead,j_nfunctions_)::headers
	integer,dimension(j_nfunctions_)::lheaders=0
	
	character*400,dimension(maxsec,maxsections)::sheaders
	character*400,dimension(maxhead,j_noptions_)::macroheaders
	integer,dimension(j_noptions_)::lmacroheaders=0
	character*280,dimension(maxlfunc)::body
	logical, dimension(j_noptions_)::comoptions=.false.
	logical,dimension(j_noptions_)::curoptions=.false.
	logical, dimension(j_nfunctions_)::funcsinsec=.false.
	logical, dimension(j_nfunctions_)::funcsinfor=.false.
	integer,dimension(maxsections)::lsheaders=0
!	integer,dimension(maxfuncs0,maxsections)::funcs
!	integer,dimension(maxfuncs0,maxsections)::funcs
	character*20,dimension(maxfuncs)::funcstot
	integer,dimension(maxfuncs)::lfuncstot
	integer,dimension(j_noptions_)::options
	integer,dimension(1000)::ifirst,ilast
	logical isletter
	logical,dimension(j_noptions_):: iscuroption
	logical ::was=.false.
	logical:: isoption,isexample,isex2,isnote,isarguments,isoptions,intable,inoption=.false.
	logical::islatex,inlatex
	logical::islisting,inlisting
	logical ::inexample,inex2,innote,inheader,insection=.false.
	logical ::wasex2,wasoption,wastabular=.false.
	logical ::inmacro=.false.
!	logical ::inbox=.false.
	logical ::isargs,isoutput=.false.
	logical ::iss=.false.
	character*20 labe	
	integer p1
	character*280 optline
	character*1 ::ch
	logical test
	logical saa
	logical::frommacro=.false.
	integer::lfrommacro
	character*200 begintabular
	logical intabular
	logical ::isout,isin=.false.
	character*40 infile,outfile
	logical ::open20=.false.
	logical::p=.false.
	character*10,dimension(30)::inpuf
	integer::ninpuf=0
	write(6,*)j_nfunctions_
	write(6,*)j_functions
	! character*(len=*),parameter :: fucol='\textcolor{VioletRed}{'
	! lfucolp=len(fucol)+1
	! character*(len=*),parameter :: inpucol='\textcolor{Red}{'
	! linpucolp=len(fucol)+1
	! character*(len=*),parameter ::boxcol='\colorbox{GreenYellow}{'
	! lboxvolp=len(boxcol)+1
	! character*(len=*),parameter :: optcol='\textcolor{blue}{'
	! loptcolp=len(optcol)+1
	! character*(len=*),parameter :: jcol='\textcolor{blue}{'
	! ljcolp=len(jcol)+1
	! character*(len=*),parameter :: comcol='\textcolor{green}{'
	! lcomcol=len(comcol)
	
	!open(1,file='jsections.txt')  !read 
	!lfu=len('\textcolor{VioletRed}{')+1
	!lbox=len('\colorbox{GreenYellow}{')+1
	!'\textcolor{blue}{'
		! call type(iob,io)
	! call call(iob,io)
	! call result(iob,io)
	! call do(iob,io)
	! call if(iob,io)
	! call which(iob,io)
	! call goto(iob,io)
	! call min(iob,io)
	! call abs(iob,io)
	! call sign(iob,io)
	! call nint(iob,io)
	! call sqrt(iob,io)
	! call len(iob,io)
	! call read(iob,io)
	! call data(iob,io)
	! write(6,*)'hei'
	! stop
	
	line=' '
	line2=' '
	linec=' '
	headers=' '
	sheaders=' '
	intabular=.false.
	!subsubsection=.false.
	!subsection=.false.
	macroheaders=' '
	ex=' '
	njf=0  !number of !jf  in sections
	njf2=0  !number of !jf in source
	!optline='\begin{tabular}{| m{.10\textwidth} | m{.05\textwidth}|m{.05\textwidth}|p{.70\textwidth}|}'
	optline='\begin{tabular}{ m{.10\textwidth}  m{.05\textwidth}m{.05\textwidth}p{.70\textwidth}}'
	lop=j_len_trim(optline)
	ner=0
	nl=0
	nlsection=0
	nsection=0
	insection=.false.
	inexample=.false.;inex2=.false.
	inheader=.false.
	innote=.false.
	isoption=.false.
	inmacro=.false.
	isnote=.false.
	isexample=.false.
	ifi=0
	inlatex=.false.
	inmacro=.false.
	nsection0=0
	inlisting=.false.
	write(6,*)'opening jsections2.txt'
	open(10,file='jsections2.txt',action='READ')
100 read(10,'(a)',end=234)line
	write(6,*)'sections2: ',line(1:j_len_trim(line))
	if(line(1:1).eq.'!')goto 100
	
goto 235
234	write(6,*)nsection,nsections,' etc'
	write(6,*)' '
	write(6,*)'found altogether',nsections,' sections:'
	stop 'here'
235	le=j_len_trim(line)
	if(le.eq.0)goto 100
	write(6,*)'<',line(1:le)
	if(line(1:1).eq.'!')goto 100
	call words(line,1,le,nwords,ifirst,ilast,iss) 
!	write(6,*)'<42>',nwords,(line(ifirst(ii):ilast(ii)),' ',ii=1,nwords)
	if(line(ifirst(1):ilast(1)).eq.';goto')then
	101	read(10,'(a)',end=102)line2
		if(index(line2,line(ifirst(2):ilast(2))//':').le.0)then
			goto 101
		else
			goto 103
		endif
	102 write(6,*)'adress ',line(ifirst(2):ilast(2))//':  NOT FOUND'
		stop 'hyhu'
	endif
103 	continue
!write(6,*)'<54>',line(ifirst(2):ilast(2))
!	write(6,*)'<33>',line(1:le)
	if(index(line(1:le),';return').gt.0)stop ';return'
	
	if(index(line(1:le),'infile').gt.0)then
		call words(line,1,le,nwords,ifirst,ilast,iss) 
		open(2,file=line(ifirst(2):le),action='READ')
		nl=0
		nlsection=0
		ifi=ifi+1
	!	if(ifi.gt.1)call testblock() tested in readinput
		call readinput()
		
		write(6,*)'found ', nsection-nsection0,' sections'
		do isec=nsection0+1,nsection
		

		 lelabel=j_len_trim(sectionlabel(isec))
		 lesec=j_len_trim(section(isec))
		 write(6,*)isec,sectionlabel(isec)(1:lelabel),' ',section(isec)(1:lesec)
	
		enddo
			
		
		nsection0=nsection
		! write(6,*)'found ',count(funcsinsec),' functions out of ',j_nfunctions_ 
		! write(6,*)'found ',count(comoptions),' options out of ',j_noptions_
		if(nsection.eq.0)stop 'nsection=0'
		close(2)
!		isinput=.false.
		goto 100
		
	elseif(index(line(1:le),'outfile').gt.0)then
		 write(6,*)'deleting ',line(ifirst(2):le)
		! read(5,*)ii
		if(open20)close(20)
		open(20,file=line(ifirst(2):le),status='REPLACE',action='WRITE')
		open20=.true.
		if(line(le-3:le).eq.'.inc')then
			call writeincl()
		endif
		call writeoutput()
		stop 'tas#################'
	write(6,*)'goto100'
	!	goto 100
	! else
		! write(6,*)'<337writeoutput ',line(1:le)
		! write(6,*)'<429>',nwords,(line(ifirst(ii):ilast(ii))//'/',ii=1,nwords)
		! call writeoutput()
		! goto 100
	
	endif
	nsection=0
	nmacro=0


!	if(ner.gt.0)stop

!	open(20,file='jmanual.tex',status='REPLACE',action='WRITE')
!	open(30,file='jexamples.txt',status='REPLACE',action='WRITE')

	
	40	continue
	close(2)
	write(6,*)' '
	write(6,*)'found altogether',nsection ,' sections:'
	write(6,*)' '
	
	
	! open(2,file='jsections2.txt',action='READ')  !putting all together
	! open(20,file='jmanual.tex',status='REPLACE',action='WRITE')
	! nl2=0
	


	stop 'fini##################'
	
	
	
	99 write(6,*)'line ',nl, 'first character ',ch,' is not !'
	contains
	subroutine writeincl()
		write(6,*)'writing ',nexample, ' examples as include file'
		write(20,'(a)')'exfile=thisfile()'
		do ine=1,nexample
			write(20,'(a)')exlabel(ine)(1:lexlabel(ine))//"=';incl(exfile,from->~"// &
			exlabel(ine)(1:lexlabel(ine))//"~)'"
		enddo
		write(20,'(a)')"AGAIN=';incl(exfile)'"
		write(20,'(a)')"ALL=';incl(exfile,from->~ALL~)'"
	
		write(20,'(a)')';return'
		do ine=1,nexample
			write(20,'(a)')' '
			write(20,'(a)')';'//exlabel(ine)(1:lexlabel(ine))//':'
			write(20,'(a)')'!'//exlabel(ine)(lexlabel(ine)+1:j_len_trim(exlabel(ine)))
			do ine2=1,ninexample(ine)
				write(20,'(a)')ex(ine2,ine)(1:leex(ine2,ine))
			enddo	
			write(20,'(a)')';return'
		enddo
		
		write(20,*)' '
		write(20,'(a)')';ALL:'
		do ine=1,nexample
		!	write(20,'(a)')' '
		!	write(20,'(a)')';'//exlabel(ine)(1:lexlabel(ine))//':'
			write(20,'(a)')exlabel(ine)(1:lexlabel(ine))
		
		enddo
			write(20,'(a)')';return'
		
		
		
	end subroutine
	
	subroutine writeoutput()
	logical isend
	isend=.true.
700	 read(10,'(a)',end=999)line;nl=nl+1;le=j_len_trim(line) !read(2,'(a)',end=40)line;nl=nl+1;le=j_len_trim(line)
	if(line(1:1).eq.'!')goto 700
	write(6,*)line(1:le)
	if(le.le.0)goto 700
	if(index(line(1:le),';return').gt.0)then
	 goto 999
	 isend=.false.
	endif
	call words(line,1,le,nwords,ifirst,ilast,iss)  !
	
	
		do isec=1,nsection
		lelabel=j_len_trim(sectionlabel(isec))
			if(sectionlabel(isec)(1:lelabel).eq.line(ifirst(2):ilast(2)))then
				lesec=j_len_trim(section(isec))
				write(20,'(a)')'\'//line(ifirst(1):ilast(1))//'{'//section(isec)(1:lesec)//'}'
				write(20,'(a)')'\label{'//sectionlabel(isec)(1:lelabel)//'}'
				nl2=nl2+2
				write(6,*)'writing section ',sectionlabel(isec)(1:lelabel),'  ',lsection(isec)+2,' lines'
				do j=1,lsection(isec)
					lei=j_len_trim(sheaders(j,isec))
					write(20,'(a)')sheaders(j,isec)(1:lei)
				enddo
				written(isec)=.true.
				nl2=nl2+lsection(isec)
				goto 700
			endif
		
		enddo
		write(6,*)' '
		write(6,*)line(ifirst(2):ilast(2)), ' not found in sections'
		write(6,*)' '
		goto 700
999	 write(6,*)' ' 

	do isec=1,nsection
	lelabel=j_len_trim(sectionlabel(isec))
	if(.not.written(isec))write(6,*)'Section ',sectionlabel(isec)(1:lelabel),' not written'

	enddo
	if(isend)stop 'isend'
	return


	
	
	end subroutine
	
	subroutine readinput()
		! ifile=0
! 789 ifile=ifile+1
! if(ifile.gt.1)write(6,*)'found ',nsection-nsection0,'  sections'
	! if(ifile.eq.1)then
		! open(2,file='jsections.txt',action='READ')
		! write(6,*)'processing jsections.txt'
	! elseif(ifile.eq.2)then
		! write(6,*)'found ',nmacro,' macros'
		! open(2,file='c:/juusi/j_.f90',action='READ')
		! write(6,*)' '
		! write(6,*)'processing j_.f90'
	! elseif(ifile.eq.3)then
		! close(2)
		! open(2,file='c:/juusi/j_utilities.f90',action='READ')
		! write(6,*)' '
		! write(6,*)'processing j_utilities.f90'

	! elseif(ifile.eq.4)then
		! close(2)
		! open(2,file='c:/juusi/jlp.f90',action='READ')
		! write(6,*)' '
		! write(6,*)'processing jlp.f90'
	! else
		! goto 40
	! endif
	
	if(insection)then
		write(6,*)'*there was an unclosed section started at line'
		stop 'huono'
	endif
	nsection0=nsection
	insection=.false.
	inexample=.false.;inex2=.false.
	inheader=.false.
	innote=.false.
	isoption=.false.
	inmacro=.false.
	isnote=.false.
	isexample=.false.
	nl=0
	nlsection=0
1  continue
if(p)write(6,*)'<76',inoption,nl,line(1:40),frommacro
	if(frommacro)then
		lfrommacro=lfrommacro+1
		! write(6,*)'<44frommacro',lfrommacro,lmacro(imacro)
			! write(6,*)'inheader,inexample,inex2,innote'// &
		! 'inlisting,inmacro,inoption,inlatex,intabular',&
		! inheader,inexample,inex2,innote,&
		! inlisting,inmacro,inoption,inlatex,intabular
		if(lfrommacro.gt.lmacro(imacro) )then
			imacro=0
			frommacro=.false.
			goto 1
		endif
		line=macroheaders(lfrommacro,imacro)
		le=j_len_trim(line)
		nb=nonblank(line,1,le)
	else
17		read(2,'(a1,a)',end=789)ch,line;nl=nl+1;le=j_len_trim(line) !read(2,'(a)',end=40)line;nl=nl+1;le=j_len_trim(line)
goto 790
789 write(6,*)'inputfile ends'	
	close(2)
	call testblockend()
	return
790	continue
		if(p)write(6,*)'<34fromi2 ',line(1:le)
		!if(nl.eq.48)write(6,*)'inheader0,insection0',inheader,insection
		if(le.ge.3)then
		 if(line(le-2:le).eq.'!!!')goto 17
		endif
	!	if(nl.eq.11690)write(6,*)'<33',line(1:le)
	!	if(index(line(1:le),'endsection').gt.0)write(6,*)'<33enndsectioninline ',nl
		nb=nonblank(line,1,le)
		if(inmacro)then
			if(line(nb:le).eq.'endmacro')then
				inmacro=.false.
			else
				call putmacro(line(nb:le))				
			endif
			goto 1
		endif
	endif


	
	call words(line,1,le,nwords,ifirst,ilast,iss)  !tarvitaanko
!	if(nl.eq.11690)write(6,*)'<66',line(ifirst(1):ilast(1)),line(ifirst(1):ilast(1)).eq.'Latex'
	if(p)write(6,*)'<56nwords',nwords
	if(nwords.ge.1)then
		if(line(ifirst(1):ilast(1)).eq.'endlatex')then
			if(.not.inlatex)then
				write(6,*)'line ',nl, 'endlatex but we are not in latex'
				stop 'nixlatex'
			endif
			inlatex=.false.
			goto 1
		endif
	endif
	if(inlatex)then
		call putsec(line(1:max(le,1))) !should be colored
		goto 1
	endif	
!if(nl.eq.48)write(6,*)'<65inheader,insection',inheader,insection
	if(line(ifirst(1):ilast(1)).eq.'Macro')then
		call testblock()
		nmacro=nmacro+1
		macrolabel(nmacro)=line(ifirst(2):ilast(2))
		write(6,*)'line ',nl, 'Macro ',nmacro,macrolabel(nmacro)
		inmacro=.true.
		nlblock=nk
		goto 1
	endif
	
	if(nl.eq.11075)write(6,*)'<654>',line(ifirst(1):ilast(1)),line(ifirst(1):ilast(1)).eq.'Inpuf'
	if(line(ifirst(1):ilast(1)).eq.'Inpuf')then
		ninpuf=ninpuf+1
		inpuf(ninpuf)=line(ifirst(2):ilast(2))
		write(6,*)'line ',nl, 'Inpuf ',line(ifirst(2):ilast(2))
	endif
	
	if(line(ifirst(1):ilast(1)).eq.'Section')then
		call testblock()
		nlsection=nl
		nsection=nsection+1
		sectionlabel(nsection)=line(ifirst(2):ilast(2))
		write(6,*)'line ',nl,'Section ',sectionlabel(nsection)
		section(nsection)=line(ifirst(3):le)
	! section cominout Command input and output
	!	character*40,dimension(maxsections)::section
	!character*20,dimension(maxsections)::sectionlabel

!		write(6,*)'line,nsection',nl,nsection,line(ifirst(2):le),'label ',&
	!	sectionlabel(nsection)
		inheader=.true.
		insection=.true.
		goto 1
	endif
!if(nl.eq.11690)write(6,*)'<66',line(ifirst(1):ilast(1)),line(ifirst(1):ilast(1)).eq.'Latex',insection,&
!inmacro


	
	if(line(ifirst(1):ilast(1)).eq.'Latex')then
	write(6,*)'Latex ',nl
		nlblock=nl
		inlatex=.true.
		if(wasoption.and.inoption)then
			call endoption()  !call putsec('\end{table}')
			wasoption=.false.
	!		call putsec('\vspace{-1.51em}')
		endif
		goto 1
	endif

	if(line(ifirst(1):ilast(1)).eq.'endsection')then
		if(.not.insection)then
			write(6,*)'line ',nl,' endsection but we are not in section'
			stop 'nix sec'
		endif
		call testblock0()  !if section consists only header it is not necessary to have !endheader
		write(6,*)
		insection=.false.
		inheader=.false.
		goto 1
	endif
	
	if(line(ifirst(1):ilast(1)).eq.'endmacro')then
			if(.not.inmacro)then
				write(6,*)'line ',nl,' endmacro but we are not in macro'
				stop 'nixmacro'
		   endif
		inmacro=.false. 
		goto 1
	endif

	!if(nl.eq.48)write(6,*)'inheader,insection',inheader,insection
	if(line(ifirst(1):ilast(1)).eq.'endheader')then
	
		if(.not.inheader.and..not.insection)then
			write(6,*)'line ', nl,' !endheader even if we are not in header'
			stop 'nixsecmac#######################'
		endif
		inheader=.false.
		goto 1
	endif
	

	
	if(line(ifirst(1):ilast(1)).eq.'endlatex')then
		if(.not.inlatex)then
			write(6,*)'line ',nl,' endlatex even if we are not in latex'
			stop 'nix latex###############'
		endif
		inlatex=.false.
	goto 1
	endif
	
	
	if(line(ifirst(1):ilast(1)).eq.'Option')then
		call testblock()
		wasoption=.false.
		inoption=.true.
		nlblock=nl
		goto 1
	endif
	
	isoption=index(line(1:le),'&').gt.0.and.inoption
	
	
	if(isoption)then
	 nie=0
	 if(.not.inlatex)then
		 do ili=1,le
		  if(line(ili:ili).eq.'&')nie=nie+1
		 enddo
		 if(nie.ne.3)then
			write(6,*)'line ',nl, " should have 3 '&'-characters"
			stop 'nix3#######################'
		 endif
	 endif
		if(wasoption)then
			call endoption()  !call putsec('\end{table}')
			call putsec('\vspace{-1.51em}')
		endif
		
		call putsec('\begin{table}[H]')
		call putsec('\begin{tabular}{ m{.10\textwidth}  m{.05\textwidth}m{.10\textwidth}p{.60\textwidth}}')
		call colors(inpuf,ninpuf,line,line2,nwords,ifirst,ilast,.false.,isoption,le2)
		call putsec(line2(1:le2))
		icopt=j_isin(line(ifirst(1):ilast(1)),macrolabel,nmacro)
		wasoption=.true.
		goto 1
	endif
	
	if(line(ifirst(1):ilast(1)).eq.'endoption')then
		if(.not.inoption)then
			write(6,*)'line ',nl,' endoption even if we are not in option'
			stop 'enopt###################'
		endif
		call endoption()
		inoption=.false.
		wasoption=.false.
		goto 1
	endif
	
	
	
	if(line(ifirst(1):ilast(1)).eq.'Tabular')then
		call testblock()
		nmerk=ichar(line(ifirst(2):ilast(2)))-48
		begintabular=line(ilast(2)+1:le)
		intabular=.true.
		ntab=0
		nlblock=nl
	!	write(6,*)'<888>intabualr',intabular
		goto 1
	endif
	

	if(line(ifirst(1):ilast(1)).eq.'endtabular')then
		if(.not.intabular)then
			write(6,*)'line ',nl,' endtabular even if we are not in tabular'
			stop 'FALSE ENDTABULAR'
		endif
		intabular=.false.
		wastabular=.false.
		call endtabular()
		goto 1
	endif
		if(p)write(6,*)'<569nwords',nwords,'le',le
	if(intabular)then
		if(le.le.0)goto 1
		im1=index(line(1:le),'&')
		if(im1.gt.0.)then
			nme=1
			 do ii=im1+1,le
			 if(line(ii:ii).eq.'&')nme=nme+1
			 enddo
			 if(nme.ne.nmerk)then
				write(6,*)'line ',nl, ' does not have ',nmerk,' & '
				stop 'missing&'
			 endif
			
			if(wastabular)then
				call endtabular()
				call putsec('\vspace{-1.51em}')
			endif
			
			
			call putsec('\begin{table}[H]')
			call putsec(begintabular(1:j_len_trim(begintabular)))
		endif
		call colors(inpuf,ninpuf,line,line2,nwords,ifirst,ilast,.false.,.false.,le2)
		call putsec(line2(1:le2))
		wastabular=.true.
		goto 1
	endif
	

		if(p)write(6,*)'<587nwords',nwords
	isnote=line(ifirst(1):ilast(1)).eq.'Note'
	if(nl.eq.1406)write(6,*)'insection,innote,inexample,inex2,inoption,inlatex,isnote',&
		insection,innote,inexample,inex2,inoption,inlatex,isnote
	if(isnote)then	
		
		if(.not.insection)goto 1  !there can be other Note lines in the source.
		call testblock()
		call putsec('\begin{note}')
	!	call putsec('\label{'//line(ifirst(2):ilast(2))//'}')
		innote=.true.
		!write(6,*)'<888',line(1:le)
		call dropfirst()
	!	write(6,*)'<888',line(1:le)
	!	write(6,*)line(ifirst(1):ilast(1))
	!	stop
!		call words(line,ifirst(2),le,nwords,ifirst,ilast,iss)
		
		!call colors(inpuf,ninpuf,line,ifirst(1)-1,line2,nwords,ifirst,ilast,.false.,isoption,le2)
		call colors(inpuf,ninpuf,line,line2,nwords,ifirst,ilast,.false.,.false.,le2)
		call putsec(line2(1:le2))
			if(p)write(6,*)'<577le2',nwords
		nlblock=nl
		goto 1
	endif
	
	if(line(ifirst(1):ilast(1)).eq.'endnote')then
		if(.not.innote)then
			write(6,*)'line ', nl, 'endnote but we are not in note'
			stop 'nixnote'
		endif
		call putsec('\end{note}')
		innote=.false.
		goto 1
	endif
	
	
	
	
	islisting=line(ifirst(1):ilast(1)).eq.'Listing'
	if(islisting)then
		call testblock2()
		call putsec('\color{Green}')
		call putsec('\begin{verbatim}')
		inlisting=.true.
		nlblock=nl
		goto 1
	endif
	if(line(ifirst(1):ilast(1)).eq.'endlisting')then
		if(.not.inlisting)then
			write(6,*)'line ',nl, 'endlisting even if we are not in listing started with Listing'
			stop 'nixlistin'
		endif
		call putsec('\end{verbatim}')
		call putsec('\color{Black}')
		nexi=0
		inlisting=.false.
		goto 1
	endif
	if(inlisting)then
		call putsec(line(1:le))
		goto 1
	endif
	
	
	isexample=line(ifirst(1):ilast(1)).eq.'Ex'
	if(isexample)then
		call testblock()
		
		labe=line(ifirst(2):ilast(2))
		lelabe=ilast(2)-ifirst(2)+1
		nexample=nexample+1
		exlabel(nexample)=line(ifirst(2):le)
		lexlabel(nexample)=lelabe
		write(6,*)'                 ',exlabel(nexample)(1:lelabe),':',exlabel(nexample)(lelabe+1:le)
		line2='\begin{example}['//labe(1:lelabe)//']'//line(ifirst(3):le)//'\\'
		le=j_len_trim(line2)
		call words(line2,1,le,nwords,ifirst,ilast,iss)  !tarvitaanko
		
		call colors(inpuf,ninpuf,line2,line,nwords,ifirst,ilast,.false.,.false.,le2)
		call putsec(line(1:le2))
		!call putsec('\begin{example}['//line(ifirst(2):ilast(2))//']'//line(ifirst(3):le)//'\\')
	!	call putsec('\begin{example}['//line(ifirst(2):ilast(2))//']'//line(ifirst(3):le)//'\\')
	!	call putsec('\begin{example}['//line(ifirst(2):le)//']')
		call putsec('\label{'//labe(1:lelabe)//'}')
	!	call putsec('{\fontfamily{lmtt}\selectfont')		
		nexi=0
		inexample=.true.
		nlblock=nl		
		goto 1
	endif
	
	if(line(ifirst(1):ilast(1)).eq.'endex')then
		if(.not.inexample)then
			write(6,*)'line ',nl, ' endex but we are not in ex started with !Ex'
			stop 'endex ENDEX'
		endif
		if(inlisting)then
			write(6,*)'line ',nl, ' endex but listing is not ended'
			stop 'inlisrt ####'
		endif
		inexample=.false.
	!	call putsec('}')
		 call putsec('\end{example}')
		goto 1
	endif
	
	isex2=line(ifirst(1):ilast(1)).eq.'Ex2'
	if(isex2)then
		call testblock()
		if(wasex2)call putsec('\vspace{-1.51em}')
		call putsec('\begin{table}[H]') !%    '//line(ifirst(2):le)//']')
		call putsec('\label{'//line(ifirst(2):ilast(2))//'}')
		call putsec('\begin{tabularx}{\textwidth}{ c  X }')
		nlblock=nl
		inex2=.true.
		goto 1
	endif
	
	if(line(ifirst(1):ilast(1)).eq.'endex2')then
		if(.not.inex2)then
			write(6,*)'line ',nl, ' endex2 but we are not in ex2'
			stop 'jdjjd'
		endif
		inex2=.false.
		wasex2=.true.
		 call endex2()
		goto 1
	endif
	! if(inex2)then
		! if(wasex2)call putsec('\vspace{-1.51em}')
	
	! endif
	

		if(line(nb:nb).eq.'@')then
		imacro=j_isin(line(ifirst(1):ilast(1)),macrolabel,nmacro)
		if(imacro.le.0)then
			write(6,*)'line ',nl, line(ifirst(1):ilast(1)),'  is not among macros: '
			do ico=1,nmacro
				write(6,*)macrolabel(ico)
			enddo
			stop 'mcmcm'
		endif
		frommacro=.true.
		lfrommacro=0
		goto 1
	endif
!!!!!! ordinary line
	

	if(inlisting)then  !listing can be within example
		call putsec(line(1:max(1,le)))
		goto 1
	endif
	
	if(inexample)then
		saa=inexample.and.line(nb:nb).eq.'/'
	!		call putsec(line(nb:le))  ???+
	!if(saa)write(6,*)'<888sanlline',nl,line(1:le)
!	write(6,*)'<567line',inheader,nl,line(1:le)
		ninexample(nexample)=ninexample(nexample)+1
		ex(ninexample(nexample),nexample)=line(1:max(le,1))
		leex(ninexample(nexample),nexample)=max(le,1)
		if(le.le.0)goto 1
		if(nexi.gt.0)call appsec('\\')
		!	subroutine colors(inpuf,ninpuf,line,ial0,line2,nwords,ifirst,ilast,tabs,isopt,le2)
		call colors(inpuf,ninpuf,line,line2,nwords,ifirst,ilast,.false.,.false.,le2)
		call putsec(line2(1:le2))
		nexi=nexi+1
		goto 1
	endif
	
	if(inheader.or.inoption.or.inex2.or.innote)then
		call colors(inpuf,ninpuf,line,line2,nwords,ifirst,ilast,.false.,.false.,le2)
		call putsec(line2(1:le2))
		goto 1
	endif
	

	goto 1  !there can be empty lines
	write(6,*)'*line*',nl,':',ch,line(1:le)
	write(6,*)' this line does not belong to any known section part'
	! write(6,*)'inheader,inexample,inex2,innote'// &
		! 'inlisting,inmacro,inoption,inlatex,intabular',&
		! inheader,inexample,inex2,innote,&
		! inlisting,inmacro,inoption,inlatex,intabular	
			write(6,*) 'this far we obtained this much:'
			write(6,*)' '
	call printsec(nsection)
	stop 'hier'

	

4	close(1)	
	
	end subroutine
	
	subroutine dropfirst()
	
		i=0
		ialf=ifirst(2)-1
	!	ialf1=ialf-1 !how many chars dropped
		do j=ifirst(2),le
		i=i+1
		line(i:i)=line(j:j)
		enddo
		le=i
		do i=1,nwords-1
			ifirst(i)=ifirst(i+1)-ialf
			ilast(i)=ilast(i+1)-ialf
		enddo
		nwords=nwords-1
	
	end subroutine
	
	
	subroutine testblock()
		if(inheader.or.inexample.or.inex2.or.innote&
		.or.inmacro.or.inoption.or.inlatex.or.intabular)then
			write(6,*)'line ',nl,'cannot start new block before closing the previous block'
			write(6,*)'the current block started at line ',nlblock
			write(6,*)'the currentsection started at line ',nlsection
			stop 'testb'
		endif
		if(.not.insection.and.line(ifirst(1):ilast(1)).ne.'Section' &
		.and.line(ifirst(1):ilast(1)).ne.'Macro')then
			write(6,*)'line ',nl,' cannot start new block if not in section',line(ifirst(1):ilast(1))
			stop 'testbl2'
		endif
	
	end subroutine
	
	subroutine testblockend()
		if(inheader.or.inexample.or.inex2.or.innote&
		.or.inmacro.or.inoption.or.inlatex.or.intabular.or.insection)then
			write(6,*)'EOF when unclosed block'
			write(6,*)'the current block started at line ',nlblock
			write(6,*)'the current section started at line ',nlsection
			stop 'tesble'
		endif
	end subroutine
	

	
	subroutine testblock2() !when entering listing listing can be in exaple or in header
		if(inex2.or.innote&
		.or.inmacro.or.inoption.or.inlatex.or.intabular)then
			write(6,*)'line ',nl,'cannot start new block before closing the previous block'
			write(6,*)'the current block started at line ',nlblock
			write(6,*)'the current section started at line ',nlsection
			stop 'djjd'
		endif
	
	end subroutine
	
	subroutine testblock0()  !inheader missing
		if(inexample.or.inex2.or.innote.or.&
		inlisting.or.inmacro.or.inoption.or.inlatex.or.intabular)then
			write(6,*)'line ',nl,'cannot close section before closing the previous block'
			write(6,*)'the current block started at line ',nlblock
			write(6,*)'the current section started at line ',nlsection
			stop 'dkdk'
		endif
	
	end subroutine
	
	subroutine endoption()
	call putsec('\\ \cline{1-4}')
	call putsec('\end{tabular}')
	call putsec('\end{table}')
!	write(6,*)'<5444 ',line(1:le)
	end subroutine
	
	subroutine endtabular()
	!	call putsec('\\ \cline{1-4}')
	call putsec('\end{tabular}')
	call putsec('\end{table}')	
	end subroutine
	
	subroutine endex2()
!	call putsec('\\ \cline{1-4}')
	call putsec('\end{tabularx}')
	call putsec('\end{table}')
	inex2=.false.
	end subroutine
	
	subroutine putmacro(inpu)
		character*(*)inpu
	if(lmacro(nmacro).ge.500)then
	write(6,*)'<447>',nl,line
		write(6,*)'<660>',nmacro
		write(6,*)'<666>',macrolabel(nmacro)
		do isec=1,nmacro
			write(6,*)'<555>',lmacro(isec),macrolabel(isec)
		enddo
	
	endif
	lmacro(nmacro)=lmacro(nmacro)+1
	macroheaders(lmacro(nmacro),nmacro)=inpu
	!write(6,*)'<nsec,lsec,inpu',nsection,lsection(nsection),inpu
	end subroutine
	
	subroutine printsec(isec)
		write(6,*)sectionlabel(isec),' lines ',lsection(isec)
		do il=1,lsection(isec)
			write(6,*)il,':',sheaders(il,isec)(1:j_len_trim(sheaders(il,isec)))
		enddo
	
	end subroutine
	
	subroutine putsec(inpu)
	character*(*)inpu
	if(lsection(nsection).ge.500)then
	write(6,*)'there is too long section ',nsection,' which started at line ',nlsection
	
	write(6,*)'we are now at line',nl,':',line(1:le)
		write(6,*)'Sections and their lengths'
		do isec=1,nsection
			write(6,*)sectionlabel(isec),lsection(isec)
		enddo
		write(6,*)'current section lines'
		call printsec(nsection)
	
	endif
	lsection(nsection)=lsection(nsection)+1
	sheaders(lsection(nsection),nsection)=inpu
	
	!write(6,*)'<nsec,lsec,inpu',nsection,lsection(nsection),inpu
	end subroutine
	
	subroutine appsec(inpu)
	character*(*)inpu
	integer lei
	if(lsection(nsection).le.0)then
		write(6,*)'<44>nsection',nsection
		return
	endif
	lei=j_len_trim(sheaders(lsection(nsection),nsection))
	sheaders(lsection(nsection),nsection)(lei+1:)=inpu
	end subroutine
	end program
	
!! subroutines which are not contained	
	subroutine colors(inpuf,ninpuf,line,line2,nwords,ifirst,ilast,tabs,isopt,le2)
	use jmod, only: j_nfunctions_,j_noptions_,j_functions,j_options
	character(len=*),parameter :: fucol='\textcolor{VioletRed}{'
	integer,parameter:: lfucolp=len(fucol)+1
	character(len=*),parameter :: inpucol='\textcolor{Red}{'
	integer,parameter:: linpucolp=len(inpucol)+1
	character(len=*),parameter ::boxcol='\colorbox{GreenYellow}{'
	integer,parameter:: lboxvolp=len(boxcol)+1
	character(len=*),parameter :: optcol='\textcolor{blue}{'
	integer,parameter:: loptcolp=len(optcol)+1
	character(len=*),parameter :: jcol='\textbf{J}'
	integer,parameter:: ljcol=len(jcol)
	character(len=*),parameter :: comcol='\textcolor{green}{'
	integer,parameter:: lcomcol=len(comcol)

! tab=9
!initial secttion is dropped

	character*(*) line,line2	
	integer ::nwords,ifirst(1:*),ilast(1:*)
	logical tabs,got,isopt,wasblue,ischar
	character*4 ctabs
	real::left=1.2
	real::tab=0.5
	logical:: perk,p=.false.
	character*10,dimension(10)::inpuf
!	lfu=len('\textcolor{VioletRed}{')+1
!	lbox=len('\colorbox{GreenYellow}{')+1
!	lvar=len('\textcolor{
	!p=index(line,'(this)').gt.0
!	stop 
	le=j_len_trim(line)
	! perk=index(line(1:le),'func->').gt.0
	 if(p)then
	 write(6,*)line(1:le)
	 do iw=1,nwords
		 write(6,*)'<w>',iw,line(ifirst(iw):ilast(iw)),ifirst(iw)
	 enddo
	 endif
	! if(perk)write(6,*)'<<1>',line(1:le)
!	perk=inexample.and.line(1:1).eq.'/')  !index(line(1:le),'#').gt.0
!	if(perk)write(6,*)'<666nl,line',nl,le,'/',line(1:le),'/'
!	write(6,*)'<5>',line(1:le)
!	write(6,*)nwords,('/'//line(ifirst(iw):ilast(iw))//'/',iw=1,nwords)
	if(nwords.eq.0)then
	line2=line
	return
	endif
	line2=' '
	! if(ial0.gt.0)then
		! line2(1:ial0)=line(1:ial0)
		! le2=ial0
		! j=ial0
	! else
		le2=0

		j=0
!	endif

	ntab=0
	got=.false.
	
	nw=1
	icom=index(line(1:le),'!')
	icom1=max(icom-1,1)
	if(line(icom1:icom1).eq."'")icom=0  !ignore '!
	icom0=icom
	if(icom.le.0.or.icom.eq.le)then
		icom=le+1
		icom0=le+1
	else
		
			icom=len_trim(line(1:icom-1))+1
	
	!	write(6,*)'<<55uusicom',icom
	endif
	!p=icom.lt.le
	if(p)write(6,*)'<7773477icom',icom,le,line(1:le)
	ischar=.false.
!	perk=.false. !index(line(1:le),'$').gt.0
	if(p)then
		write(6,*)'<perk>',line(1:le)
		 do iw=1,nwords
		 write(6,*)'<w>',iw,line(ifirst(iw):ilast(iw)),ifirst(iw),ilast(iw)
		enddo
	endif
!	if(perk)write(6,*)'<5555>',line(1:le)
	! if(line(1:1).eq.'!')then
	! line2='\colorbox{GreenYellow}{'//line(2:le)//'}'
	! le2=len_trim(line2)
	! return
	! endif
	if(p)write(6,*)'le,icom',le,icom
100 j=j+1
if(p)write(6,*)'<5line2',le2,'/',line2(1:le2),'/'
	if(p)write(6,*)'<j,nw,ifirst(nw),ch',j,nw,ifirst(nw),'/',line(j:j),'/'
	
	if(nw.le.nwords.and.j.gt.ifirst(nw))then
		write(6,*)'<111>colors, j too big,j,nw,ifirst(nw) ',j,nw,ifirst(nw)
		write(6,*)'<77>ial0',ial0,line(1:ial0)
		write(6,*)'<line>',le,line(1:le)
		write(6,*)'<line2>',le2,line2(1:le2)
		 do iw=1,nwords
		 write(6,*)'<w>',iw,line(ifirst(iw):ilast(iw)),ifirst(iw),ilast(iw)
		enddo
	! endif
	! if(perk)write(6,*)'<<1>',line(1:le)
		
		stop 'colors3'
	endif
!if(perk)write(6,*)'<48>j,line,nw,ifirst,ilast',j,line(j:j),nw,ifirst(nw),ilast(nw)
!	if(perk)write(6,*)'<57>,j
	if(j.gt.le.and.p)write(6,*)'<perkout ',line2(1:le2),'icom ',icom
	if(j.gt.le.and.p)stop 'p'
	if(j.gt.le)then
		if(icom.lt.le)then
			le2=le2+1
			line2(le2:le2)='}'
		endif
		return
	
	endif
	! if(line(j:j+1).eq.'\\')then
		! line2(le2+1:le2+2)='\\'
		! le2=le2+2
		! j=j+1
		! goto 100
	! endif
	if(p)write(6,*)'<56000jai'
	 if(line(j:j).eq.'\')then
		 line2(le2+1:le2+1)='\'
		 le2=le2+1
		! j=j+1
		 goto 100
	 endif
	 if(line(j:j).eq.'J'.and.(j.eq.1.or. line(max(j-1,1):max(j-1,1)).eq.' ' &
		.and.line(j+1:j+1).eq.' '))then
		 line2(le2+1:le2+ljcol)=jcol
	!	 write(6,*)'<23>J',j
		 le2=le2+ljcol
		 nw=nw+1
		! j=j+1
		 goto 100
	 endif
	if(line(j:j).eq.'%')then
		
		line2(le2+1:le2+2)='\%'
		le2=le2+2
		goto 100
	endif
	if(line(j:j).eq.'$')then
		
		line2(le2+1:le2+2)='\$'
		le2=le2+2
		goto 100
	endif
	if(line(j:j).eq.'~')then
		!%le2=le2+1
		ischar=.not.ischar
		line2(le2+1:le2+6)='$\sim$'
		le2=le2+6
		goto 100
	endif
	if(line(j:j).eq.'/'.and.line(j+1:j+1).eq.'\')then
		!%le2=le2+1
		
		line2(le2+1:le2+12)='$\backslash$'
		le2=le2+12
		j=j+1
		goto 100
	endif
	if(line(j:j).eq.'#')then
		!%le2=le2+1
	!	write(6,*)'<44#in color>',nl,line(1:le)
		line2(le2+1:le2+2)='\#'
		le2=le2+2
		goto 100
	endif
	 if(line(j:j).eq.'_')then
		! !%le2=le2+1
	! !	write(6,*)'<44#in color>',nl,line(1:le)
		 line2(le2+1:le2+2)='\_'
		 le2=le2+2
		 goto 100
	 endif
	if(p)write(6,*)'<56jui'
	if(.not.got)then
		if(tabs.and.ichar(line(j:j)).eq.9)then
		 ntab=ntab+1
		endif
		if(ntab.gt.0.and.ichar(line(j:j)).ne.9)then
		write(ctabs,'(f4.2)')ntab*tab
		line2(1:12)='\hspace{'//ctabs//'}'
		le2=12
		got=.true.
		endif
	endif
!	if(j.lt.ifirst(1))goto 100
	! if(nw.eq.1.and.ifirst(nw).gt.1)then
		! if(le2+ifirst(1)-1.gt.len(line2))write(6,*)'*',nwords,j,ifirst(1:10),ilast(1:10),line
		! line2(le2+1:le2+ifirst(1)-1)=line(1:ifirst(1)-1)
		! le2=le2+ifirst(1)-1
	! endif
!		write(6,*)'<22>',j,nw,ifirst(nw)
	if(icom.lt.le.and.(j.ge.icom0.and.j.lt.icom.or.j.gt.icom.and.line(j:j).eq.' '))then
		line2(le2+1:le2+2)='\,'
		le2=le2+2
		goto 100
	endif
	 if(j.eq.icom0)then
		line2(le2+1:le2+lcomcol+1)=comcol//'!'
		le2=le2+lcomcol+1
		goto 100
	 endif
	if(line(j:j).eq.' '.and.(ischar.or.j.gt.icom))then
	line2(le2+1:le2+2)='\,'
	le2=le2+2
	goto 100
	endif
	if(p)write(6,*)'<j,nw,ifirst(nw)',j,nw,ifirst(nw)
	if(j.eq.ifirst(nw))then
		lenw=ilast(nw)-ifirst(nw)+1
		ifu=j_isin(line(ifirst(nw):ilast(nw)),j_functions,j_nfunctions_)
		iopt=j_isin(line(ifirst(nw):ilast(nw)),j_options,j_noptions_)
		inpufu=j_isin(line(ifirst(nw):ilast(nw)),inpuf,ninpuf)
		! if(index(line(1:le),';sum').gt.0)then
		! write(6,*)'<445ninpuf',ninpuf,inpuf(1:ninpuf)
		! write(6,*)inpufu
		! stop 'inpu'
		! endif
		
			if(p)write(6,*)'<44>',nw,ifu,iopt,line(ifirst(nw):ilast(nw)),&
			iopt.gt.0.and.(line(ilast(nw)+1:ilast(nw)+2).eq.'->'.or.(isopt.and.nw.eq.1))
			if(p)write(6,*)'353553,isopt',isopt
		if(ifu.gt.0.and.line(ilast(nw)+1:ilast(nw)+1).eq.'(')then
			! if(j_functions(ifu)(1:1).eq.';')then
			lis=17+lenw
			! lis=linpucolp+lenw
			line2(le2+1:le2+lis)='\textcolor{red}{'//line(ifirst(nw):ilast(nw))//'}'
			! line2(le2+1:le2+lis)=inpucol//line(ifirst(nw):ilast(nw))//'}'
			
			! else
			lis=lfucolp+lenw
		!	line2(le2+1:le2+lis)='\textcolor{VioletRed}{'//line(ifirst(nw):ilast(nw))//'}'
			line2(le2+1:le2+lis)=fucol//line(ifirst(nw):ilast(nw))//'}'
		
!			endif
			le2=le2+lis
			j=j+lenw-1
		elseif(inpufu.gt.0)then
			!	lis=17+lenw
				lis=linpucolp+lenw
			!	line2(le2+1:le2+lis)='\textcolor{red}{'//line(ifirst(nw):ilast(nw))//'}'
				line2(le2+1:le2+lis)=inpucol//line(ifirst(nw):ilast(nw))//'}'
				le2=le2+lis
				j=j+lenw-1
			
		elseif(iopt.gt.0.and.(line(ilast(nw)+1:ilast(nw)+2).eq.'->'.or.(isopt.and.nw.eq.1)))then
			if(p)write(6,*)'<654',iopt,lenw,line(ifirst(nw):ilast(nw))
			if(isopt)then
				lis=loptcolp+lenw
				
				!line2(le2+1:le2+lis)='\textcolor{blue}{'//line(ifirst(nw):ilast(nw))//'}'
				line2(le2+1:le2+lis)=optcol//line(ifirst(nw):ilast(nw))//'}'
				 le2=le2+lis
				j=j+lenw-1
	!			if(perk)write(6,*)'<222j',j
				
			else
				lis=loptcolp+lenw+2
				if(p)write(6,*)'<345bef',ilast(nw),ifirst(nw),lenw,line2(1:le2)
				line2(le2+1:le2+lis)=optcol//line(ifirst(nw):ilast(nw)+2)//'}'
				if(p)write(6,*)'<345bef',line2(1:le2)
				 le2=le2+lis
				j=j+lenw-1+2
				if(p)write(6,*)'<223j',j,line(j:le)
			 endif
	
		else
!			if(perk)write(6,*)'<666',line(ifirst(nw):ilast(nw))
			! if(line(ifirst(nw):ilast(nw)).eq.'$')then
				 ! line(le2+1:le2+2)='\$'
				 ! le2=le2+2
			! else
			! ia=index(line(ifirst(nw):ilast(nw)),'_')+ifirst(nw) !place of _
			! if(ia.le.ifirst(nw))then
				line2(le2+1:le2+lenw)=line(ifirst(nw):ilast(nw))
				le2=le2+lenw
				j=j+lenw-1
			! else
				! nfi=ia-ifirst(nw)
				! nla=ilast(nw)-ia
				! if(nfi.gt.0)line2(le2+1:le2+nfi)=line(ifirst(nw):ia-1)
				
				! line2(le2+nfi+1:le2+nfi+2)='\_'
				! if(nla.gt.0)line2(le2+nfi+3:le2+nfi+3+nla)=line(ia+1:ilast(nw))
			! endif
			!endif
		endif
		nw=nw+1
!		if(j.ge.ifirst(nw))nw=nw+1
!		 if(perk)write(6,*)'<39>',nw,j,ifirst(nw)

	else
		line2(le2+1:le2+1)=line(j:j)
	if(p)write(6,*)'<23>j,le2',j,le2,line(j:j)
		le2=le2+1
	endif
!		write(6,*)'<33>',j,le2
	
	goto 100
!\begin{adjustwidth}{<leftskip>}{<rightskip>}
!  ...
!\end{adjustwidth}	
!the changepage package	

! \section*{Abstract}
! \begin{adjustwidth}{1cm}{1cm}
  ! \lipsum[1-2]
! \end{adjustwidth}
! \section*{Keywords}
! \hspace{1cm} keyword1, keyword2

! \section{Introduction}
! \lipsum[3-5]
	
	end subroutine 

	function j_isin(text,vector,n) !location of text in character vector vector with size n
	integer, intent(in):: n
	character*(*), intent(in)::  text, vector(n)
! as in j but initial blanks are removed
	!20141217 oli: le1=len(text)
	le1=j_len_trim(text)
	do i=1,n
		le2=j_len_trim(vector(i))
		if(le1.eq.le2) then
			!20141217 oli: if(text.eq.vector(i)(1:le2))then
			if(text(1:le1).eq.vector(i)(1:le2))then
				j_isin=i
				return
			endif !if(text(1:le1).eq.vector(i)(1:le2))then
		endif !if(le1.eq.le2) then
	enddo !do i=1,n
	j_isin=0
	return
end function j_isin !function j_isin(text,vector,n)
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
	le=j_len_trim(limit)
!###TESTAUS###
!write(6,*)'nextword <526> ial,le,lop,limit ', ial,le,lop,'/',limit(1:le),'/'
	if(ial+le-1>lop) then
		j_nextword = lop+1
		return
	endif !if(ial+le-1>lop) then

	i = index(inp(ial:lop),limit(1:le))
!###TESTAUS###
!write(6,*)'nextword <534> i, inp(ial:lop),limit ',inp(ial:lop),' ',limit
	if(i>0) then
		j_nextword = ial+i-1
	else !if(i>0) then
		j_nextword = lop+1
	endif !if(i>0) then
	return
end function j_nextword !function j_nextword(inp,ial,lop,limit)	
subroutine j_clean(text,le) ! remove blanks, tabs etc , le is the length of the cleaned text
	integer, intent(out):: le
	character (len=*),intent(inout):: text

	jj=0
	le1=len(text)
	do j=1,le1
		if(text(j:j).le.' ')cycle
		jj=jj+1
		if(jj.ne.j)text(jj:jj)=text(j:j) !this can now be used for clean charcter constants also
	enddo !do j=1,le1
	le=jj
	if(le.lt.le1)then
		text(jj+1:le1)=' '
	endif !if(le.lt.le1)then
	return
end subroutine j_clean !subroutine j_clean(text,le)
logical function j_isletter(ch) ! is letter ?
	character*1, intent(in):: ch

	j_isletter=(ch.ge.'A'.and.ch.le.'Z').or.(ch.ge.'a'.and.ch.le.'z')
	return
	
end function j_isletter !logical function j_isletter(ch)
function nonblank(inp,ial,lop)
	integer, intent(in):: ial,lop
	character*(*), intent(in):: inp

	do i=ial,lop
		if(ichar(inp(i:i)).gt.32)then
			nonblank=i
			return
		endif !if(ichar(inp(i:i)).gt.32)then
	enddo !do i=ial,lop
	nonblank=lop+1
	return
end function nonblank !function nonblank(inp,ial,lop)

subroutine words(inp,ial0,le,nwords,ifirst,ilast,iss)
character*(*) inp
character*23 limit
integer,parameter::nl=23
logical islimit
logical iss
integer ::ifirst(1:*),ilast(1:*)
!write(6,*)'<1>',ial0,le,inp(ial0:le)
limit(1:22)=' ,():!&-+/#@"._<>=~\%$'
limit(23:23)="'"
nwords=0
islimit=.false.
ial=ial0
iss=.false.
do j=1,le
if(.not.(inp(j:j).eq.'!'.or.inp(j:j).eq.' '.or.ichar(inp(j:j)).eq.9))then
	if(j.eq.le)then
	iss=.true.
	return
	else
	goto 100
	endif
endif

enddo


100 continue
main: do j=ial,le
sub: do k=1,nl
if(inp(j:j).eq.limit(k:k).or.ichar(inp(j:j)).eq.9)then

cycle main
endif
enddo sub
nwords=nwords+1
ifirst(nwords)=j
!write(6,*)'ifirst ',j,nwords,ifirst(nwords)
if(ifirst(nwords).ge.le)then
ilast(nwords)=le
return

endif
!write(6,*)'goto 2',ifirst(nwords)+1,le
goto 2
enddo main
!write(6,*)'<2 return'
if(nwords.eq.0)then
ifirst(1)=le+1
ilast(1)=le+1
endif
return

2	continue
main2: do j=ifirst(nwords)+1,le
sub2: do k=1,nl
if(inp(j:j).eq.limit(k:k))then
ilast(nwords)=j-1
ial=j
goto 100
endif
enddo sub2

enddo main2

ilast(nwords)=le
! if(inp(1:4).eq.'from')then
! write(6,*)inp(1:10),'#1'
! write(6,*)ifirst(1),ilast(1)
! stop
! elseif(inp(2:5).eq.'from')then
! write(6,*)inp(1:10),'#2'
! write(6,*)ifirst(1),ilast(1)
! stop

! endif
return



end subroutine
integer function j_len_trim(inp)   !like len-trim but returns zero aslo when line consists of tabs
	character(len=*),intent(in):: inp
	le=len(inp)

	if(le.le.0)then
		j_len_trim=0
		return
	endif !if(le.le.0)then
	do j_len_trim=le,1,-1
		if(ichar(inp(j_len_trim:j_len_trim)).gt.32)return
	enddo !do j_len_trim_=le,1,-1
	j_len_trim=0
	return
end function j_len_trim !integer function j_len_trim_(inp)
 
